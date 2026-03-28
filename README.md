# Algebirc

> **An Isogeny-SPN Hybrid Cryptographic Obfuscation Engine with Additively Homomorphic Evaluation over Genus-2 Jacobian Varieties**

[![Haskell](https://img.shields.io/badge/language-Haskell%20%2B%20C%20%2B%20x86--64%20ASM-purple)](https://www.haskell.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.4+-green)](https://www.haskell.org/ghc/)

---

## Table of Contents

1. [Overview](#1-overview)
2. [Mathematical Foundations](#2-mathematical-foundations)
3. [System Architecture](#3-system-architecture)
4. [Core Cryptographic Pipeline](#4-core-cryptographic-pipeline)
5. [Native 256-bit Arithmetic Layer (cbits)](#5-native-256-bit-arithmetic-layer-cbits)
6. [Homomorphic Evaluation Engine](#6-homomorphic-evaluation-engine)
7. [Security Properties](#7-security-properties)
8. [Module Reference](#8-module-reference)
9. [Configuration Reference](#9-configuration-reference)
10. [Limitations and Honest Assessment](#10-limitations-and-honest-assessment)
11. [References](#11-references)

---

## 1. Overview

**Algebirc** is an experimental cryptographic obfuscation engine written in Haskell with performance-critical primitives implemented in C and hand-written x86-64 assembly. It integrates three distinct cryptographic paradigms into a unified pipeline:

| Paradigm | Role in Algebirc |
|---|---|
| **White-Box Cryptography** | Symmetric cipher architecture where key extraction is computationally intractable even with full source access |
| **Isogeny-Based Cryptography** | Genus-2 Jacobian walks as transform layers; believed post-quantum resistant |
| **Ring-LWE Homomorphic Encryption** | Additive homomorphism enabling `Enc(a) + Enc(b) = Enc(a+b)` without key exposure |

Algebirc is **not** a Fully Homomorphic Encryption (FHE) scheme — it does not support arbitrary depth computation on ciphertext. It supports *additive* homomorphism under RLWE, and *branchless conditional evaluation* via Richelot isogeny multiplexing, making it a novel hybrid between an obfuscator and a partially homomorphic engine.

---

## 2. Mathematical Foundations

### 2.1 Finite Field Arithmetic

All arithmetic is performed in the prime field $\mathbb{F}_p$ where $p = 257$ (default) or $p = 65537$ (hardened mode). The choice $p = 257$ is deliberate: it is the smallest prime strictly greater than 256, ensuring each byte value $b \in \{0, \ldots, 255\}$ embeds injectively as a field element without reduction.

**Multiplicative inverse** is computed via Fermat's Little Theorem:

$$a^{-1} \equiv a^{p-2} \pmod{p}$$

implemented in constant-time via the `ctPowMod` function using arithmetic masking — no branching on secret values:

```
newAcc = mulAcc * bit + acc * (1 - bit)   -- no branch, pure arithmetic
```

### 2.2 Polynomial Ring and Encoding

Plaintext data is encoded as a **degree-bounded polynomial** over $\mathbb{F}_p$:

$$f(x) = \sum_{i=0}^{D} b_i \cdot x^i \in \mathbb{F}_p[x], \quad \deg(f) \leq D$$

where each coefficient $b_i$ is a plaintext byte and $D = 64$ (default). This dense polynomial ring representation underpins every subsequent algebraic transform.

**Lagrange Interpolation** recovers the unique polynomial through $n$ points over $\mathbb{F}_p$:

$$f(x) = \sum_{i=0}^{n-1} y_i \prod_{j \neq i} \frac{x - x_j}{x_i - x_j}$$

**Polynomial composition** $f(g(x))$ satisfies $\deg(f \circ g) = \deg(f) \cdot \deg(g)$. The engine checks $\deg(f) \cdot \deg(g) \leq D$ before computation and returns `DegreeOverflow` if violated, preventing unbounded degree growth.

### 2.3 Elliptic Curves (Genus-1)

Elliptic curves in **short Weierstrass form** over $\mathbb{F}_p$:

$$E: y^2 = x^3 + ax + b, \quad \Delta = -16(4a^3 + 27b^2) \neq 0 \pmod{p}$$

The **j-invariant** classifies curves up to isomorphism over an algebraically closed field:

$$j(E) = 1728 \cdot \frac{(4a)^3}{4a^3 + 27b^2} \pmod{p}$$

**Point arithmetic** uses the chord-and-tangent method. For $P = (x_1, y_1)$, $Q = (x_2, y_2)$ with $P \neq Q$:

$$\lambda = \frac{y_2 - y_1}{x_2 - x_1}, \quad x_3 = \lambda^2 - x_1 - x_2, \quad y_3 = \lambda(x_1 - x_3) - y_1$$

A **constant-time Montgomery ladder** (`ecScalarMulCT`) processes exactly `bitWidth` iterations regardless of the scalar's Hamming weight.

### 2.4 Genus-2 Hyperelliptic Curves and Jacobian Varieties

A **genus-2 hyperelliptic curve** over $\mathbb{F}_p$:

$$C: y^2 = f(x), \quad \deg(f) \in \{5, 6\}$$

Its **Jacobian variety** $\text{Jac}(C)$ is a principally polarized abelian surface of dimension 2. Elements are represented in **Mumford form** $(u(x), v(x))$ satisfying:

$$u \mid \text{monic}, \quad \deg(u) \leq g = 2, \quad \deg(v) < \deg(u), \quad u \mid (v^2 - f)$$

**Cantor's Algorithm** computes the group law on $\text{Jac}(C)$. For divisors $D_1 = (u_1, v_1)$ and $D_2 = (u_2, v_2)$:

1. $(\gcd, e_1, e_2) \leftarrow \text{ExtGCD}(u_1, u_2)$
2. $(\gcd', c_1, c_2) \leftarrow \text{ExtGCD}(d, v_1 + v_2)$
3. $s_1 = c_1 e_1,\; s_2 = c_1 e_2,\; s_3 = c_2$
4. $u' = \frac{u_1 u_2}{d'^2}$, $\quad v' = \frac{s_1 u_1 v_2 + s_2 u_2 v_1 + s_3(v_1 v_2 + f)}{d'} \pmod{u'}$
5. Reduce until $\deg(u') \leq g$

### 2.5 Igusa Invariants

The **Igusa invariants** $(J_2, J_4, J_6, J_{10})$ classify genus-2 curves up to isomorphism over an algebraically closed field. For $f(x) = \sum_{i=0}^{6} f_i x^i$:

$$J_2 = f_0 f_6 - f_1 f_5 + f_2 f_4 - f_3^2$$

$$J_4 = f_0 f_4 f_6 + f_1 f_3 f_5 - f_0 f_5^2 - f_1^2 f_6 - f_2 f_3 f_4 + f_2^2 f_6$$

$$J_{10} = \text{disc}(C)$$

These serve as the definitive **geometric signature** of the final obfuscation state via `evaluateGeometricSignature`.

### 2.6 Richelot (2,2)-Isogenies

A **Richelot isogeny** is a $(2,2)$-isogeny between principally polarized abelian surfaces:

$$\varphi: \text{Jac}(C) \to \text{Jac}(C')$$

Given $C: y^2 = G_1(x) \cdot G_2(x) \cdot G_3(x)$ (sextic factored into three quadratics), define:

$$\delta_i = \det\begin{pmatrix} a_{j,0} & a_{j,2} \\ a_{k,0} & a_{k,2} \end{pmatrix}$$

$$H_i = \frac{1}{\delta_i}\bigl(a_{j,0}a_{k,1} - a_{j,1}a_{k,0},\; 2(a_{j,0}a_{k,2} - a_{j,2}a_{k,0}),\; a_{j,1}a_{k,2} - a_{j,2}a_{k,1}\bigr)$$

The codomain curve: $C': y^2 = \delta \cdot H_1 \cdot H_2 \cdot H_3$

The **Kunzweiler correspondence mapping** lifts a divisor $D \in \text{Jac}(C)$ to $\varphi(D) \in \text{Jac}(C')$:
- **Split case** ($u(x)$ factors over $\mathbb{F}_p$): pointwise lifting
- **Irreducible case**: symbolic ring arithmetic in $\mathbb{F}_p[r]/(u)$

### 2.7 Ring-LWE Encryption

Ciphertext lives in the cyclotomic ring $R_q = \mathbb{Z}_q[x]/(x^N + 1)$:

$$\text{Enc}(m) = (a, c) \quad \text{where} \quad c = a \cdot s + e + \Delta \cdot m$$

$$a \xleftarrow{\$} R_q, \quad s \in \{-1, 0, 1\}^N, \quad e \leftarrow \chi_\sigma, \quad \Delta = \left\lfloor \frac{q}{t} \right\rfloor$$

**Decryption** recovers $m$ when $\|e\|_\infty < q/2t$:

$$m = \left\lfloor \frac{t}{q} \cdot (c - a \cdot s) \right\rceil \bmod t = \left\lfloor \frac{t}{q} \cdot (e + \Delta m) \right\rceil \bmod t$$

**Additive Homomorphism** — no key required for addition:

$$\text{Enc}(a) + \text{Enc}(b) = (c_a + c_b,\; \text{pub}_{A,a} + \text{pub}_{A,b}) = \text{Enc}(a + b)$$

---

## 3. System Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         ALGEBIRC PIPELINE                                   │
│                                                                             │
│  Input: f ∈ {source AST, binary blob, byte stream}                         │
│       │                                                                     │
│       ▼                                                                     │
│  ┌─────────────────────────────────┐                                        │
│  │           ENCODER               │  bᵢ → coeff of xⁱ in 𝔽_p[x]           │
│  │  (Algebirc.Obfuscation.Encoder) │  Block size: D+1 bytes, deg ≤ 64      │
│  └─────────────────────────────────┘                                        │
│       │                                                                     │
│       ▼                                                                     │
│  ┌─────────────────────────────────────────────────────────┐               │
│  │              ISOGENY-CBC CHAINING                        │               │
│  │  masked_blockᵢ = plaintext_blockᵢ ⊕_field IVᵢ           │               │
│  │  IVᵢ₊₁ = (f(seed) · f(seed+1) + f(seed) + 1) mod p     │  quadratic    │
│  └─────────────────────────────────────────────────────────┘               │
│       │                                                                     │
│       ▼                                                                     │
│  ┌─────────────────────────────────────────────────────────┐               │
│  │           SPN TRANSFORM PIPELINE                         │               │
│  │                                                          │               │
│  │  Affine(x→ax+b) → PowerMap(x→xᵉ) → MDS → PowerMap → MDS│               │
│  │                                                          │               │
│  │  Each transform applied coefficient-wise over 𝔽_p       │               │
│  │  MDS = Cauchy matrix: M[i,j] = 1/(xᵢ+yⱼ) mod p          │               │
│  └─────────────────────────────────────────────────────────┘               │
│       │  (genus=2 only)                                                     │
│       ▼                                                                     │
│  ┌─────────────────────────────────────────────────────────┐               │
│  │            GEOMETRIC PIPELINE                            │               │
│  │                                                          │               │
│  │  Stage 1: Vélu Isogeny Walk on volcano graph            │               │
│  │     coeffᵢ ↦ x-coord of φ_ℓ(liftToPoint(coeffᵢ))       │               │
│  │  Stage 2: CM Group Action                               │               │
│  │     Permute via j-invariant orbits                       │               │
│  │  Stage 3: Siegel Modular Mixing                         │               │
│  │     cᵢ' = (cᵢ + jₙₑₓₜ) mod p, via Φ_ℓ(X, jcurr)       │               │
│  │  [Interleaved with MDS at each stage]                   │               │
│  └─────────────────────────────────────────────────────────┘               │
│       │                                                                     │
│  Output: Obfuscated polynomial blocks (coefficients over 𝔽_p)               │
└─────────────────────────────────────────────────────────────────────────────┘

Optional: Homomorphic Evaluation Layer (Algebirc.Evaluator.ObfEval)
┌─────────────────────────────────────────────────────────────────────────────┐
│  ObfPoly  → degree-tracking polynomial homomorphic ops                      │
│  ObfLWE   → RLWE: c = a·s + e + Δ·m in ℤ_q[x]/(xᴺ+1), Add/Sub keyless    │
│  ObfPoint → [x]G ∈ Jac(C), [x]G + [y]G = [x+y]G (additive homomorphic)    │
│  geometricIf → φ_T([b]D_T) + φ_F([(1-b)]D_F) — branchless conditional     │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 4. Core Cryptographic Pipeline

### 4.1 Isogeny-CBC Chaining

Files are processed as a sequence of polynomial blocks in **Isogeny-CBC** mode — an analogue of CBC block cipher chaining but operating in the polynomial coefficient domain with a nonlinear IV update:

$$\text{IV}_{i+1} = \bigl(f(\text{seed} \bmod p) \cdot f(\text{seed}+1 \bmod p) + f(\text{seed}) + 1\bigr) \bmod p$$

This is a **quadratic function** of the ciphertext polynomial coefficients, strictly stronger than the linear XOR-based CBC chaining of classical symmetric ciphers. The additive `+1` prevents the degenerate case $\text{IV} = 0$.

### 4.2 SPN Layer: PowerMap and MDS

**Power Map** $\pi_e: x \mapsto x^e \bmod p$ is bijective if and only if $\gcd(e, p-1) = 1$.  
Inverse: $\pi_e^{-1} = \pi_{e^{-1}}$ where $e^{-1} \equiv e^{-1} \pmod{p-1}$ via Euler's theorem.

**MDS Diffusion** uses a **Cauchy matrix**:

$$M_{i,j} = \frac{1}{x_i + y_j} \bmod p, \quad x_i = i, \quad y_j = n + j$$

A Cauchy matrix with distinct $x_i + y_j \neq 0$ is **Maximum Distance Separable** — every square submatrix is invertible. Any single coefficient change propagates to all output coefficients (strict avalanche). Inversion uses Gaussian elimination over $\mathbb{F}_p$.

**Pipeline Structure**:

```
[Affine] → [PowerMap] → [MDS] → [PowerMap] → [MDS]
   a = (seed·31 + 17) mod (p-1) + 1   (key-derived)
   e = 3 (nearest coprime to p-1)
```

### 4.3 Geometric Pipeline (Genus-2 Mode)

The full geometric encoding pipeline:

$$\text{coeffs} \xrightarrow{\text{Isogeny Walk}} \xrightarrow{\text{MDS}} \xrightarrow{\text{CM Action}} \xrightarrow{\text{MDS}} \xrightarrow{\text{Siegel Mix}} \xrightarrow{\text{MDS}} \text{output}$$

**Stage 1: Vélu Isogeny Walk**  
Each coefficient is lifted to an elliptic curve point $P \in E(\mathbb{F}_p)$ via `liftToPointDet`, transported through a volcano-structured $\ell$-isogeny chain $E_0 \to E_1 \to \cdots \to E_k$, then projected back via $x$-coordinate extraction.

**Stage 2: CM Group Action**  
Permutes the coefficient vector using an ordering derived from the j-invariant orbit under complex multiplication, inspired by CSIDH-style CM-based key exchange.

**Stage 3: Siegel Modular Mixing**  
The Siegel modular polynomials $\Phi_\ell(X, Y)$ encode $\ell$-isogeny relations between j-invariants. At each mixing step:

$$c_i' = (c_i + j_{\text{next}}) \bmod p, \quad j_{\text{next}} = \text{root of } \Phi_\ell(\cdot, j_{\text{curr}}) \neq j_{\text{prev}}$$

This creates a forward-secrecy-like dependency: each output coefficient depends on the entire j-invariant walk history.

---

## 5. Native 256-bit Arithmetic Layer (`cbits/`)

Algebirc includes a **hand-optimized native layer** for production-grade 256-bit modular arithmetic, targeting the large prime fields needed for cryptographically-sized elliptic curve operations (e.g., $p \approx 2^{256}$).

### 5.1 Architecture

```
cbits/
├── u256_ct.c         -- Portable C implementation (fallback, all platforms)
├── u256_x86_64.S     -- Hand-written x86-64 AT&T assembly (primary, Linux)
├── u256_ct.asm       -- ELF disassembly of compiled object (inspection)
└── u256_ct.o         -- Pre-compiled ELF64 object
```

The C and assembly are conditionally compiled:

```c
#ifndef __x86_64__
  // Portable C fallback
#endif
// x86_64: assembly overrides via .global declarations
```

### 5.2 256-bit Primitives

A 256-bit integer is represented as **4 × 64-bit limbs** (`uint64_t[4]`) in **little-endian** order: `limb[0]` is the least significant.

#### Addition with Carry Propagation

```c
// C: software carry chain
uint64_t u256_add_ct(u256_t r, const u256_t a, const u256_t b)
// carries via: carry = (sum < a[i]) | ((sum == a[i]) & carry)
```

```asm
// x86-64: hardware carry flag (CF) via ADC instruction
u256_add_ct:
    movq 0(%rsi), %r8    ; Load a[0]
    addq 0(%rdx), %r8    ; r8 = a[0] + b[0], sets CF
    adcq 8(%rdx), %r9    ; r9 = a[1] + b[1] + CF
    adcq 16(%rdx), %r10  ; ...
    adcq 24(%rdx), %r11  ; propagate through all 4 limbs
    setc %al             ; capture final carry into %al — no branch
```

The x86-64 `ADC` (Add with Carry) and `SBB` (Subtract with Borrow) instructions propagate carry/borrow in a single clock cycle, making the 256-bit operation exactly 4 instructions plus overhead, independent of operand values.

#### Branchless Conditional Selection

The `-cond` trick (two's complement):

```c
// C implementation
void u256_select_ct(u256_t r, uint64_t cond, const u256_t a, const u256_t b) {
    uint64_t mask  = -cond;  // 0xFFFFFFFFFFFFFFFF if cond=1, 0x0 if cond=0
    uint64_t nmask = ~mask;
    for (int i = 0; i < 4; i++)
        r[i] = (a[i] & mask) | (b[i] & nmask);
}
```

```asm
// x86-64 assembly — zero branches, zero memory-access patterns
u256_select_ct:           ; rdi=r, rsi=cond, rdx=a, rcx=b
    negq  %rsi            ; rsi = -cond (0xFF...FF or 0x00...00)
    movq  %rsi, %r8
    notq  %r8             ; r8 = ~mask
    ; Limb 0:
    movq  0(%rdx), %r9
    andq  %rsi, %r9       ; a[0] & mask
    movq  0(%rcx), %r10
    andq  %r8,  %r10      ; b[0] & ~mask
    orq   %r10, %r9       ; combine
    movq  %r9,  0(%rdi)
    ; ... repeat for limbs 1–3
```

#### Montgomery Multiplication (CIOS Method)

For large-prime modular multiplication, the **Coarsely Integrated Operand Scanning (CIOS)** Montgomery algorithm operates entirely in the Montgomery domain $\hat{a} = a \cdot R \bmod m$ where $R = 2^{256}$:

$$\text{MonMul}(a, b) = a \cdot b \cdot R^{-1} \bmod m$$

```c
// Uses __int128 for 128-bit product accumulation
void u256_mont_mul_ct(u256_t r, const u256_t a, const u256_t b,
                      const u256_t m, uint64_t m0_inv) {
    uint64_t t[5] = {0};
    for (int i = 0; i < 4; i++) {
        // Outer loop: accumulate a[i]*b
        for (int j = 0; j < 4; j++) {
            __int128 prod = (__int128)a[i] * b[j] + t[j] + carry;
            t[j] = (uint64_t)prod;
            carry = (uint64_t)(prod >> 64);
        }
        // Montgomery reduction step
        uint64_t q = t[0] * m0_inv;
        // ... subtract q*m from t
    }
    // Final conditional subtraction — branchless via u256_select_ct
    u256_select_ct(r, cond_keep_t, t, diff);
}
```

The final conditional subtraction (to reduce $t$ modulo $m$ if $t \geq m$) uses `u256_select_ct` — no branches.

#### Constant-Time Modular Exponentiation

```c
void u256_modpow_ct(u256_t r, const u256_t base_mont, const u256_t exp,
                    const u256_t m, uint64_t m0_inv, const u256_t mont_one) {
    // Fixed 256 iterations — always the same number of operations
    for (int i = 0; i < 256; i++) {
        uint64_t b = (exp[i/64] >> (i%64)) & 1;  // bit i of exponent
        u256_mont_mul_ct(res_mul, t_res, t_base, m, m0_inv);
        u256_select_ct(t_res, b, res_mul, t_res);  // branchless select
        u256_mont_mul_ct(t_base, t_base, t_base, m, m0_inv);  // always square
    }
}
```

Every iteration performs exactly 1 multiply + 1 select + 1 square — **constant operation count** regardless of the exponent's bit pattern.

#### Constant-Time Modular Inversion (Fermat)

$$a^{-1} \equiv a^{m-2} \pmod{m} \quad \text{(by Fermat's Little Theorem for prime } m\text{)}$$

```c
void u256_modinv_ct(u256_t r, const u256_t a_mont, const u256_t m,
                    uint64_t m0_inv, const u256_t mont_one) {
    u256_t exp;
    u256_sub_ct(exp, m, {2,0,0,0});  // exp = m - 2
    u256_modpow_ct(r, a_mont, exp, m, m0_inv, mont_one);
}
```

### 5.3 Side-Channel Resistance Guarantees

The cbits layer guarantees:

| Threat | Mitigation |
|--------|-----------|
| **Branch-based timing** | Zero secret-dependent branches in all primitives |
| **Cache-based timing** | Fixed memory access pattern: `limb[0..3]` always in order |
| **Power analysis (SPA)** | Fixed operation count per bit: 1 mul + 1 select + 1 square |
| **Compiler optimization** | Hand-written assembly bypasses GCC/Clang transformations |
| **Secret-indexed loads** | No table lookups; all operands from registers |

The disassembly in `u256_ct.asm` confirms the compiled object uses `SETB`/`SETC` flag captures (not conditional jumps) and `XMM` vector registers for SIMD-accelerated selections in `u256_modadd_ct` and `u256_modsub_ct`.

---

## 6. Homomorphic Evaluation Engine

**Module**: `Algebirc.Evaluator.ObfEval`

### 6.1 ObfuscatedValue Representation

The evaluator operates on three distinct representations:

```haskell
data ObfuscatedValue
  = ObfPoly BoundedPoly ObfuscationConfig ObfuscationPipeline Int
    -- Legacy polynomial mode

  | ObfLWE  { ovCipher :: BoundedPoly    -- c = a·s + e + Δ·m
             , ovPubA   :: BoundedPoly    -- random public a
             , ovLWEKey :: LWESecretKey   -- s ∈ {-1,0,1}^N
             , ... }

  | ObfPoint { ovDiv       :: MumfordDiv  -- D = [x]G ∈ Jac(C)
              , ovCurve     :: HyperCurve
              , ovGen       :: MumfordDiv  -- Generator G
              , ovBranchCtxs :: (RichelotCtx, RichelotCtx) }
```

### 6.2 Homomorphic Operations

| Operation | ObfPoly | ObfLWE | ObfPoint |
|-----------|---------|--------|----------|
| **Add** | `p1 + p2` (coefficient-wise) | `(c₁+c₂, a₁+a₂)` — **keyless** | `D₁ + D₂` Jacobian add — **keyless** |
| **Sub** | `p1 - p2` | `(c₁-c₂, a₁-a₂)` — **keyless** | `D₁ + (-D₂)` — **keyless** |
| **Mul** | `p1 * p2` (degree doubles) | decrypt → multiply → re-encrypt | Restricted to scalar: $k \cdot [x]G = [kx]G$ |
| **Bootstrap** | Cyclotomic fold mod $x^N+1$ | No-op (already in ring) | No-op |

### 6.3 Invisible Logic Branching

The most novel component is the **geometric if-then-else** primitive. Define the **Richelot multiplexer**:

$$D' = \varphi_{\text{true}}\bigl([b] \cdot D_{\text{true}}\bigr) + \varphi_{\text{false}}\bigl([(1-b)] \cdot D_{\text{false}}\bigr)$$

where $\varphi_{\text{true}}, \varphi_{\text{false}}$ are two distinct Richelot isogenies derived from independent factorizations of the same sextic.

**Analysis**:
- When $b = 1$: $[1]D_T$ contributes, $[0]D_F = \mathcal{O}$ (identity) vanishes
- When $b = 0$: $[0]D_T = \mathcal{O}$ vanishes, $[1]D_F$ contributes
- The Jacobian addition $\varphi_T(D_T') + \varphi_F(D_F')$ executes in both cases
- Scalar multiplication uses **constant-time Montgomery ladder** — same trace for $b=0$ and $b=1$

An observer monitoring memory access patterns, instruction traces, or power consumption **cannot distinguish which branch was taken**.

---

## 7. Security Properties

### 7.1 Hardness Assumptions

| Component | Hardness Assumption | Quantum Status |
|-----------|--------------------|----|
| SPN Core (Affine + PowerMap + MDS) | Algebraic-SPN indistinguishability | Grover's algorithm halves classical security |
| Isogeny-CBC chaining | Quadratic IV prediction hardness | Partially resistant |
| EC lifting + Vélu walk | Elliptic Curve DLP on $E(\mathbb{F}_p)$ | **Vulnerable** to Shor |
| Genus-2 Jacobian encoding | $\text{Jac}(C)$ discrete logarithm | Subexponential (Gaudry index calculus) |
| Richelot isogeny path | Path-finding in $(2,2)$-isogeny graph | **Conjectured post-quantum** |
| RLWE | Ring-LWE problem (lattice) | **Post-quantum** (Regev 2005) |

### 7.2 Constant-Time Implementation Summary

The following functions are implemented without secret-dependent branches or memory accesses:

| Module | Function | Method |
|--------|----------|--------|
| `FiniteField` | `ctInverse` | Fixed `bitlength(p-2)` Fermat squarings |
| `FiniteField` | `ctSelect` | Arithmetic masking: `a·cond + b·(1-cond)` |
| `Core.Types` | `getCoeffAtCT` | Always `maxDeg+1` scan iterations |
| `Polynomial` | `polyEvalCT` | Horner always `maxDeg+1` steps |
| `EllipticCurve` | `ecScalarMulCT` | Montgomery ladder, fixed `bitWidth` iterations |
| `EllipticCurve` | `branchlessSelect` | `(cond·a + (1-cond)·b) mod p` |
| `HyperellipticCurve` | `cantorReduceCT` | Always exactly 2 `oneStepReduce` calls |
| `HyperellipticCurve` | `jacobianScalarMulCT` | Montgomery ladder on Mumford divisors |
| `cbits/u256_ct.c` | All functions | Software bitmask, no branches |
| `cbits/u256_x86_64.S` | `u256_add_ct`, `u256_sub_ct`, `u256_select_ct` | Hardware `ADC`/`SBB`/`NEG`+`AND`, no `Jcc` |

### 7.3 CSPRNG

Key material is derived via **SHA-256 in counter mode**:

$$\text{csprng}(\text{seed}, \text{counter}) = \text{SHA-256}(\text{seed} \,\|\, \text{counter})$$

Pipeline geometry is derived via an **HKDF-like** SHA-256 construction at counters 1–14, tying all algebraic parameters to the master seed with 256-bit preimage security.

---

## 8. Module Reference

```
algebirc/
├── cbits/                               -- Native performance layer
│   ├── u256_ct.c                        -- Portable 256-bit CT arithmetic (C)
│   └── u256_x86_64.S                    -- Optimized x86-64 assembly
│
└── src/
    ├── Algebirc.hs                      -- Library entry point
    └── Algebirc/
        ├── Core/
        │   ├── Types.hs                 -- All algebraic data types
        │   ├── FiniteField.hs           -- GF(p) with Fermat CT inversion
        │   ├── Polynomial.hs            -- Ring ops, Lagrange, Karatsuba
        │   ├── Group.hs                 -- Permutation group S_n
        │   ├── Matrix.hs                -- Cauchy MDS over GF(p)
        │   └── U256Mod.hs               -- 256-bit modular arithmetic (Haskell FFI)
        ├── Geometry/
        │   ├── EllipticCurve.hs         -- E(𝔽_p), Weierstrass, CT scalar mul
        │   ├── HyperellipticCurve.hs    -- Jac(C), Cantor, Igusa invariants
        │   ├── RichelotIsogeny.hs       -- (2,2)-isogenies, Kunzweiler mapping
        │   ├── GeometricPipeline.hs     -- 4-stage geometric obfuscation
        │   ├── Isogeny.hs               -- Vélu isogenies, volcano walk
        │   ├── CMAction.hs              -- CM group action permutations
        │   └── SiegelModular.hs         -- Φ_ℓ(X,Y) evaluation and mixing
        ├── Obfuscation/
        │   ├── AST.hs                   -- Source-level Haskell-like AST
        │   ├── Encoder.hs               -- AST/bytes → BoundedPoly
        │   ├── Transform.hs             -- Affine, PolySub, Permutation
        │   ├── NonlinearTransform.hs    -- PowerMap, Cauchy MDS
        │   └── Pipeline.hs              -- Genus-dispatched orchestrator
        ├── Evaluator/
        │   ├── Eval.hs                  -- Turing-complete GF(p) evaluator
        │   └── ObfEval.hs               -- Homomorphic eval (ObfPoly/LWE/Point)
        ├── Integration/
        │   └── FileIO.hs                -- File obfuscation, Isogeny-CBC
        ├── Runtime/
        │   └── TransformFold.hs         -- Affine folding, identity elimination
        └── Analysis/
            ├── Adversarial.hs           -- KPA/CPA attack simulation
            └── StructuralProbe.hs       -- Degree, annihilator, Jacobian rank
```

---

## 9. Configuration Reference

```haskell
data ObfuscationConfig = ObfuscationConfig
  { cfgFieldPrime     :: Integer  -- p: GF(p) modulus
  , cfgMaxDegree      :: Int      -- D: polynomial degree cap
  , cfgMaxDepth       :: Int      -- evaluator recursion depth limit
  , cfgMaxSteps       :: Int      -- evaluator step budget
  , cfgSeed           :: Integer  -- master seed for all KDF operations
  , cfgEnableAnalysis :: Bool     -- if True, bypasses nonlinear layers (dev only)
  , cfgGenus          :: Int      -- 1 = RLWE/Vélu, 2 = Richelot+Siegel
  }
```

| Config | $p$ | $D$ | Genus | Use Case |
|--------|-----|-----|-------|----------|
| `defaultConfig` | 257 | 64 | 1 | Development / fast iteration |
| `secureConfig` | 65537 | 256 | 2 | Higher security margin |

> **Warning**: `cfgEnableAnalysis = True` disables `PowerMap` and `MDS` layers, reducing the system to a linear transform. Always set to `False` in production.

---

## 10. Limitations and Honest Assessment

Algebirc is an **academic research prototype**. The following critical limitations apply before any production consideration:

1. **Parameter sizes are not cryptographically sized**: GF(257) and RLWE ring dimension $N = 16$ provide negligible security by modern standards. Deployment-ready parameters require $p > 2^{128}$ and $N \geq 1024$.

2. **No formal security proofs**: The Richelot-based "Invisible Logic" branching primitive is a novel construction without a published security reduction. Its computational indistinguishability is conjectured, not formally proven.

3. **RLWE noise management**: The Gaussian noise $e$ is sampled from OS randomness without rigorous parameter analysis. No noise budget tracking is performed across multiplication chains, risking decryption failure after depth $> 1$ multiplications.

4. **Jacobian DLP is trivial for GF(257)**: The discrete logarithm on $\text{Jac}(C)$ over GF(257) is solvable by exhaustive search in $O(p^2)$ operations — computationally trivial. Security requires $p \gg 2^{64}$.

5. **Polynomial transform inversion is not implemented**: `PolynomialTransform` inversion returns `Left (GenericError "requires lookup table")`. The pipeline is currently restricted to affine, power map, permutation, and MDS transforms for full roundtrip correctness.

6. **Performance**: Jacobian group law and Richelot mapping are $O(p)$ for root-finding and $O(n^2)$ for polynomial multiplication. NTT-based polynomial arithmetic and projective coordinates are not implemented, making the system unsuitable for high-throughput applications.

---

## 11. References

1. **Cantor, D.** (1987). *Computing in the Jacobian of a hyperelliptic curve*. Mathematics of Computation, 48(177), 95–101.

2. **Richelot, F.J.** Historical; modern formulation: **Bröker, R., Howe, E., Lauter, K., Stevenhagen, P.** (2012). *Genus-2 curves and Jacobians with a given number of points*. LMS Journal of Computation and Mathematics.

3. **Kunzweiler, S.** (2022). *Efficient Computation of $(2^n, 2^n)$-Isogenies*. Advances in Mathematics of Communications. (Also: ASIACRYPT 2022.)

4. **Regev, O.** (2005). *On Lattices, Learning with Errors, Random Linear Codes, and Cryptography*. Proceedings of STOC '05, ACM. *(Ring-LWE hardness foundation.)*

5. **Fan, J., Vercauteren, F.** (2012). *Somewhat Practical Fully Homomorphic Encryption*. IACR ePrint 2012/144. *(BFV scheme — basis for the RLWE construction.)*

6. **Castryck, W., Decru, T.** (2022). *An Efficient Key Recovery Attack on SIDH*. EUROCRYPT 2023. *(Context: isogeny-based cryptography remains active research.)*

7. **Vélu, J.** (1971). *Isogénies entre courbes elliptiques*. C.R. Acad. Sci. Paris, 273, 238–241.

8. **Igusa, J-I.** (1960). *Arithmetic variety of moduli for genus two*. Annals of Mathematics, 72(3), 612–649.

9. **Montgomery, P.L.** (1985). *Modular multiplication without trial division*. Mathematics of Computation, 44(170), 519–521. *(Basis for `u256_mont_mul_ct`.)*

10. **Kocher, P., Jaffe, J., Jun, B.** (1999). *Differential power analysis*. CRYPTO '99. *(Motivation for constant-time design in cbits.)*

11. **Bernstein, D.J., Lange, T.** (2017). *Post-quantum cryptography*. Nature, 549, 188–194.

12. **Gaudry, P.** (2000). *An algorithm for solving the discrete log problem on hyperelliptic curves*. EUROCRYPT 2000. *(Context: genus-2 Jacobian DLP complexity.)*

---

*Algebirc is an experimental system for cryptographic research. Do not use in production without independent peer review by a qualified cryptographer.*
