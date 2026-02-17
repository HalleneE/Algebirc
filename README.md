![GHC](https://img.shields.io/badge/GHC-≥9.4-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![Status](https://img.shields.io/badge/status-experimental-orange)
![Language](https://img.shields.io/badge/language-Haskell-purple)
![FFI](https://img.shields.io/badge/FFI-none-lightgrey)

# Algebirc — Algebirc Obfuscation Engine
**A research-grade algebirc program transformation system written in Haskell.**
> Programs become polynomials.
> Polynomials become isogeny walks.
> Memory becomes secret shares.
Algebirc is an experimental algebraic obfuscation framework that transforms executable programs into mathematically equivalent representations over finite fields. By lifting computation into layered polynomial, nonlinear, and geometric domains, the system increases the structural complexity required for reverse engineering while preserving functional correctness.
## Overview
Algebirc is an experimental algebraic obfuscation engine that transforms executable programs into mathematically equivalent, structurally opaque representations defined over finite fields `GF(p)`. Unlike traditional binary-level obfuscators, Algebirc lifts computation into algebraic form across five distinct transformation domains:
* **Program logic** → bounded polynomials over `GF(p)`
* **Control flow** → nonlinear bijective transformations
* **Data mixing** → ARX-style diffusion networks
* **Structural embedding** → isogeny graph walks over elliptic and hyperelliptic curves
* **Runtime state** → additive secret-shared field elements
The resulting executable preserves functional correctness while embedding its computation inside layered algebraic structures that significantly complicate static and dynamic reverse engineering attempts. The system is implemented entirely in pure Haskell and designed as a modular, multi-phase transformation pipeline with configurable security depth.
## Design Philosophy
Algebirc is built on three foundational principles:

**Lift computation into algebraic structures** — All program semantics are expressed as polynomial arithmetic over finite fields, removing reliance on syntactic or binary-level representations.
**Increase algebraic degree and structural depth under composition** — Each transformation layer compounds degree growth, increasing the computational cost of algebraic simplification and coefficient extraction.
**Eliminate recoverable intermediate states via transform folding** — Compile-time composition collapses transformation chains, preventing inspection of intermediate algebraic states.

Rather than relying on syntactic obfuscation techniques, Algebirc operates exclusively on algebraic semantics, providing a fundamentally different threat model compared to conventional obfuscators.
## Architecture
| **Layer** | **Modules** | **Responsibility** |
|---|---|---|
| Core | `Types.hs` · `Polynomial` · `GFp.hs` · `Permutation` | Algebraic foundations and field arithmetic |
| Obfuscation | `Transform` · `NonlinearTransform` · `SBoxGen.hs` · `Feistel.hs` | Program transformation and structural obfuscation |
| Algebraic Geometry | `EllipticCurve` · `Isogeny` · `CMAction` · `HyperellipticCurve` · `RichelotIsogeny` · `SiegelModular` · `GeometricPipeline` | Advanced geometric embedding phase |
| Runtime Hardening | `SecretShare` · `TransformFold` · `InvariantGuard` | State protection and tamper detection |
| Analysis & Proofs | `Invertibility.hs` · `DegreeTracker.hs` · `LinearizationAttack` | Formal analysis and attack resistance modeling |
| Compiler | `Standalone.hs` · `Pipeline.hs` | Executable generation and pipeline orchestration |
## Pipeline Flow
### Formal Algebraic Model

Let

$$
x \in \mathbb{F}_p^n
$$

Define the composed algebraic core:

$$
\Psi =
\Sigma_{\ell}
\circ
\rho_{2^s}
\circ
[\mathfrak{a}]
\circ
\varphi_{\ell_i}
\circ
\mathcal{D}
\circ
\mathcal{F}^{(r)}
\circ
\mathcal{S}
\circ
\mathcal{P}
$$

The full runtime transformation is:

$$
\Omega(x) =
\mathcal{G}
\big(
\mathrm{Share}
(
\mathrm{Fold}(\Psi)(x)
)
\big)
$$

### Algebraic Degree Bound

Let

- $d$ polynomial degree  
- $r$ Feistel rounds  
- $\ell_i$ isogeny degrees  
- $s$ Richelot depth  
- $\ell$ Siegel level  

Then

$$
\deg(\Omega) = d (p-2)^{r+1}
\left( \prod_i \ell_i \right)
2^s \ell
$$

### Asymptotic Coefficient Recovery Cost

$$
\mathcal{O}(p^{\deg(\Omega)})
$$

Under naive coefficient interpolation and exhaustive field probing assumptions.

## Security Layers
Each layer is compositional and independently configurable. The following table enumerates all active security layers, their techniques, and the intended security barriers they establish:
| **Layer** | **Technique** | **Intended Security Barrier** |
|---|---|---|
| Base | Polynomial encoding in `GF(p)` | Coefficient recovery complexity |
| Nonlinear | S-Box bijections (Fermat inverse) | Algebraic degree growth barrier |
| Diffusion | ARX mixing (Add-Rotate-XOR) | Avalanche and mixing resistance |
| Feistel | 4-round balanced Feistel network | Round-function structural inversion |
| Isogeny | Elliptic curve isogeny walks (Vélu) | Structural recovery via graph traversal |
| CM Action | Class group action on j-invariants | Group action inversion |
| Genus-2 | Hyperelliptic Jacobian arithmetic | Igusa invariant reconstruction |
| Siegel | Modular polynomial graph walks | Modular graph traversal complexity |
| Sharing | 2-of-2 additive secret sharing | Information-theoretic secrecy per share |
| Folding | Compile-time transform composition | Intermediate state elimination |
| Guards | Geometric invariant tamper detection | Silent corruption on structural tamper |
## Minimal Example
### Secret Sharing over `GF(257)`
The following demonstrates encoding polynomial coefficients in `GF(257)` and applying additive secret sharing across four coefficients:
```haskell
import Algebirc.Core.GFp
import Algebirc.Obfuscation.Transform
import Algebirc.Runtime.SecretShare
let coeffs = [42, 137, 99, 200]
let p = 257
-- Split into additive shares
let shared = splitCoeffs (map fromIntegral coeffs) p
-- Example output (illustrative):
-- [1234, 5678, 9012, 3456]
-- Reconstruct at final output stage
let recovered = reconstructCoeffs shared
-- recovered == [42, 137, 99, 200]
```
### Before vs After Obfuscation
The following illustrates the structural transformation applied to a simple integer computation:
**Before (plaintext logic):**
```haskell
-- Original: simple linear computation
compute :: Int -> Int
compute x = 3 * x + 7
```
**After (Algebirc-obfuscated, GF(257) encoding with S-Box and Feistel):**
```haskell
-- Obfuscated: computation lifted into GF(257) with nonlinear mixing
compute :: Word64 -> Word64
compute x =
let x0    = fromIntegral x mod 257
-- Affine encoding: embed input into field element
enc   = (183 * x0 + 91) mod 257
-- S-Box bijection (Fermat inverse layer)
sbox  = fermatInverse enc 257
-- Feistel round application
(l,r) = feistelRound (sbox, enc) roundKeys
-- Secret share recombination at output
out   = reconstructCoeffs (splitCoeffs [l, r] nonces 257)
in fromIntegral (head out)
```
For valid inputs within the encoded field domain, the observable output remains functionally equivalent.
The obfuscated form embeds the computation inside nonlinear field arithmetic, making the original logic structurally unrecoverable without knowledge of the transform parameters.
## Module Overview
### Core — `Algebirc.Core.*`
* **Types** — Transform types and algebraic representations
* **Polynomial** — Bounded polynomial arithmetic over `GF(p)`
* **GFp** — Prime field arithmetic operations
* **Permutation** — Bijective coefficient permutation schemes
### Obfuscation — `Algebirc.Obfuscation.*`
* **Transform** — Affine, polynomial, and composite transform definitions
* **NonlinearTransform** — S-Box, Feistel, and ARX diffusion constructions
* **SBoxGen** — Deterministic S-Box generation via Fermat inversion
* **Feistel** — Balanced 4-round Feistel network construction
* **Pipeline** — Multi-pass transformation orchestration
### Geometry — `Algebirc.Geometry.*`
* **EllipticCurve** — Weierstrass curve arithmetic and j-invariant computation
* **Isogeny** — Vélu formulas and isogeny graph walk execution
* **CMAction** — Class group action constructions over CM fields
* **HyperellipticCurve** — Genus-2 Jacobian group arithmetic
* **RichelotIsogeny** — (2,2)-isogeny operations on genus-2 Jacobians
* **SiegelModular** — Modular polynomial graph construction and traversal
* **GeometricPipeline** — Full geometric transformation orchestration
### Runtime — `Algebirc.Runtime.*`
* **SecretShare** — 2-of-2 additive sharing over `Word64`
* **TransformFold** — Affine chain folding and polynomial composition
* **InvariantGuard** — Structural tamper detection via geometric invariants
### Analysis — `Algebirc.Analysis.*`
* **Invertibility** — Formal invertibility tracking across transform chains
* **DegreeTracker** — Algebraic degree bound computation per layer
* **LinearizationAttack** — Resistance modeling against XL and Gröbner-basis linearization
### Compiler — `Algebirc.Compiler.*`
* **Standalone** — Self-contained executable generation with folded transforms
* **Pipeline** — End-to-end multi-phase transformation pipeline
## Field & Performance
Default configuration parameters:
* **Field:** `GF(257)`
* **Word size:** `Word64` arithmetic throughout
* **Secret sharing:** Additive 2-of-2 scheme over field elements
* **Transform folding:** Enabled by default to reduce intermediate state exposure
Performance characteristics scale with transformation depth and geometric embedding level. Overhead is directly proportional to polynomial degree growth and the number of active geometric pipeline phases. Configurations without geometric phases impose significantly lower runtime cost.
## Security Model
### Designed To Increase Cost Of
* Static algebraic simplification and polynomial factoring
* Coefficient extraction via differential or interpolation attacks
* Structural recovery of control flow from algebraic representations
* Intermediate state inspection during transformation execution
* Runtime tampering of geometric invariants and Jacobian parameters
### Not Designed To Provide
* Provable indistinguishability obfuscation (iO) under standard cryptographic assumptions
* Side-channel resistance (timing, power analysis, electromagnetic)
* Protection against adversaries with full input/output oracle access
Algebirc provides **industrial-grade algebraic white-box style protection**, not theoretical iO. Its security guarantees are computational and heuristic, not information-theoretic, except at the secret-sharing layer. Security strength depends on parameter selection, transformation depth, and field size configuration.
* This system should not be relied upon for protecting high-value cryptographic keys without independent security review.
## Known Limitations
* **Field size constraints** — The default field `GF(257)` operates on 8-bit value ranges. Programs with wider native types require explicit domain decomposition before encoding.
* **Performance overhead** — Geometric pipeline phases (isogeny walks, Jacobian arithmetic) introduce significant computational overhead. These phases are optional and should be enabled only where maximum structural complexity is required.
* **No formal security proofs** — Algebirc's security properties are heuristic and based on computational hardness assumptions. No formal reduction to a standard hard problem is currently provided.
* **Output size growth** — Transformation depth increases output binary size. Deep pipelines with full geometric embedding may produce significantly larger executables than the original program.
* **Control flow complexity** — Highly branching programs with irregular control flow graphs may produce suboptimal polynomial encodings under the current pipeline.
## Research Context
Algebirc draws from and synthesizes techniques across the following research domains:
* **White-box cryptography** — Embedding sensitive constants within algebraic transformation layers
* **Algebraic circuit obfuscation** — Lifting computation to polynomial circuit representations
* **Polynomial degree growth techniques** — Compounding algebraic complexity under composition
* **Isogeny-based cryptographic constructions** — Vélu-formula graph walks for structural embedding
* **Algebraic geometry over finite fields** — Jacobian arithmetic, modular polynomials, CM theory
This project is experimental and intended for advanced research in algebraic program protection. It is not intended for production deployment without independent security evaluation.
## Maturity Status
Algebirc is an active research prototype. Interfaces and internal representations may evolve. Backward compatibility is not guaranteed between major revisions.
## Installation & Quickstart
### Prerequisites
* GHC ≥ 9.4
* Cabal ≥ 3.6
### Clone & Build
```bash
git clone https://github.com/your-org/algebirc.git
cd algebirc
cabal update
cabal build
```
### Run
```bash
cabal run algebirc
```
### Use as a Library
Add to your `cabal.project`:
```
packages:
./algebirc
```
Then import in your Haskell source:
```haskell
import Algebirc.Core.GFp
import Algebirc.Obfuscation.Transform
import Algebirc.Runtime.SecretShare
```
## Contributing
Contributions, issues, and discussion are welcome. This project is at an early research stage — feedback from the algebraic geometry, cryptography, and program analysis communities is particularly valued.
* Open an issue for bugs, questions, or design discussion
* Submit a pull request with a clear description of the change and its motivation
* For significant changes, open a discussion issue first
## Citation
If you use Algebirc in academic work, please cite as:
```bibtex
@software{algebirc2025,
title  = {Algebirc: An Algebraic Obfuscation Engine},
year   = {2025},
note   = {Experimental research prototype. \url{https://github.com/your-org/algebirc}}
}
```
## Project Metrics
| **Metric** | **Value** |
|---|---|
| Total modules | ~38 |
| Lines of code | ~10,000 |
| Implementation language | Pure Haskell |
| Unsafe operations | None |
| FFI dependencies | None |
---
*Spec Version: 1 (Extended)*
*Date: 2026-02-17*
*Author: Hallene*
