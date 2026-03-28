<div align="center">
  <h1>Algebirc: Pandora</h1>
  <p><strong>A Hyperelliptic Genus-2 Obfuscation Engine & Mathematical Tarpit</strong></p>

  [![GHC](https://img.shields.io/badge/GHC-≥9.4-blue)](#)
  [![License](https://img.shields.io/badge/license-MIT-green)](#)
  [![Status](https://img.shields.io/badge/status-experimental-orange)](#)
  [![Language](https://img.shields.io/badge/language-Haskell-purple)](#)
</div>

---

> *"A Program Is A Polynomial. A Polynomial Is A Path. A Path Has No Origin. When the Path Fails, the Dimension Closes."*

**Algebirc** is an experimental symmetric cryptographic obfuscation system based on an Isogeny-SPN construction. It takes raw programmatic logic (or any byte stream) and mathematically transforms it into equivalent operations over finite fields $GF(p)$, embedding the computation inside layered algebraic structures on **Genus-2 Hyperelliptic Curves** This work explores the use of algebraic geometry as a diffusion mechanism within symmetric cryptographic constructions, with a focus on obfuscation-oriented applications.

Unlike traditional obfuscators that rely on junk code, AST manipulation, or virtualization—which are easily dismantled by SMT Solvers (e.g., z3) and Symbolic Execution (e.g., angr)—Algebirc relies on **structured algebraic transformations and high algebraic complexity**. It shifts the security perimeter from syntactic heuristics to the high algebraic complexity and increased resistance to known classes of algebraic cryptanalysis, particularly those based on algebraic extraction such as **Gröbner basis methods**.

## Core Architecture

Algebirc introduces a paradigm shift in anti-reverse-engineering: **Dimensional Disorientation**. It does not merely hide code; it significantly increases the complexity of automated analysis by embedding computation within layered geometric transformations.

### 1. The Genus-2 Richelot Walk
Data is mapped to *Mumford Divisors* on the Jacobian variety $J_\mathcal{C}$ of a Genus-2 hyperelliptic curve. The program execution is then forced to "walk" across a sequence of $(2,2)$-Richelot Isogenies. Coefficient vectors are transported through the isogeny chain via **Igusa invariant mixing**: the endpoint invariants $(J_2, J_4, J_6, J_{10})$ of the Richelot walk are used to additively mask each coefficient position, coupling the algebraic geometry of the curve to the encoded data. All polynomial degrees are hard-capped via **Bounded Polynomial Arithmetic** (degree cap = 64): all operations are *coefficient-wise* over $GF(p)$, preventing degree blow-up while ensuring the engine executes in $O(n \log n)$ time via Karatsuba multiplication.

> **Research Note (prototype):** The current implementation uses $GF(257)$ (the smallest prime $> 256$) as a proof-of-concept. For production-strength security, upgrading to a 256-bit prime field is required — this is a *documented prototype limitation*, not a design flaw.

### 2. Scalable Karatsuba-based Polynomial Evaluator
Executing highly dense polynomial block multiplications natively incurs $O(n^2)$ computational cost. Algebirc fundamentally resolves this via a recursive $O(n^{1.58})$ Karatsuba Optimizer written natively over contiguous Unboxed/Boxed Vectors. This prevents invisible truncations when managing giant cryptographic primes (256-bit modulo) and allows the engine to handle massive polynomial degrees required by the Richelot steps efficiently.

### 3. Isogeny-CBC (Cipher Block Chaining)
Algebirc eliminates the 1:1 mapping of code to equations. Instead, it flattens the target program into a continuous byte stream and chunks it. The $x$-coordinate evaluated at the end of the Richelot Walk for Block $N$ is mathematically injected as the Initial Vector (IV) for Block $N+1$. Analyzing a single block out of context leads to immediate trajectory deviation on the curve.

### 4. Toxic Padding (Junk Injection)
The dimensions of your program are permanently obscured. If a data block is smaller than the maximum matrix degree ($deg=64$), Algebirc invokes True OS Entropy (`getRandomBytes`) to pad the remainder. To an external observer, a 4-byte string and a 400-byte function are mathematically indistinguishable—both manifest as massive, fully dense Genus-2 polynomials.

### 5. PBKDF2 Integration
Default seeds are dead. Algebirc derives its Isogeny pathing via PBKDF2 (10,000 rounds of SHA256) keyed by a user passphrase. Without the exact passphrase, the generated Initial Vector (IV) and Richelot parameters will misalign, causing the engine to reconstruct corrupted, pseudo-random byte streams that fail to parse or execute.

### 6. Adversarial Oracle & Red-Teaming (Analysis Suite)
Algebirc does not blindly trust its math. The engine ships with an internal `AlgebraicLeakage` and `AdversarialOracle` suite. During compilation, it autonomously attacks itself to measure Gaussian Rank Inference, Strict Avalanche Criterion (SAC), and Gröbner Basis complexity. It evaluates its own output matrices to to estimate algebraic complexity and diffusion properties.

---

## Installation & Quickstart

### Prerequisites
* **GHC** $\ge$ 9.4
* **Cabal** $\ge$ 3.6
* *Optional: `gcc` and `build-essential` for test suites.*

### Build
```bash
git clone https://github.com/HalleneE/Algebirc.git
cd algebirc
cabal update
cabal build
```

### Usage (CLI)

Obfuscate a target file. You will be prompted for a secure passphrase:
```bash
cabal run algebirc -- obfuscate target_file.hs output_obfuscated.hs --genus 2
# Enter Obfuscation Passphrase: ****
```

Recover the file (the execution environment must supply the exact KDF passphrase):
```bash
cabal run algebirc -- deobfuscate output_obfuscated.hs.meta recovered.hs --genus 2
```

---

## Security Model & Threat Landscape

**Designed to resist:**
- **SMT Solvers & Symbolic Execution:** Systems like `angr` or `z3` face a coupling of multiple algebraic layers (Feistel network ∘ Power Map ∘ S-box ∘ Igusa invariant mixing) operating over genus-2 geometry. The absence of a correctness oracle (no padding errors, no syntax failures) eliminates the feedback loop these tools depend on.
- **Traffic / Size Analysis:** Toxic Padding ensures all outputs look identical in structure and magnitude regardless of input size.

**Not Designed To Provide:**
- **Provable General VBB/iO:** Security guarantees remain computational and physical (thermodynamic limits of memory), not formal Virtual Black Box guarantees.
- **Side-Channel Resistance:** While Random Masking is present, execution on native hardware remains vulnerable to physical DPA/CPA EM analysis.

*This is an offensive/defensive research framework. It is designed to act as a computational tarpit against reverse-engineering tools.*

---

## Academic & Research Publications

The rigorous mathematical proofs defining the Genus-2 Richelot evaluator, the Avalanche Criterion analysis, and Gröbner Basis asymptotic complexities are available in the `publish/` directory:
- `publish/algebirc_paper.pdf` (Comprehensive Architectural Review)

## Contributing

This project operates at the intersection of **algebraic geometry**, **offensive cryptography**, and **compiler architecture**. Feedback from these communities is highly valued.

```bibtex
@software{algebirc_2026,
  title  = {Algebirc: A Hyperelliptic Obfuscation Engine},
  year   = {2026},
  note   = {Experimental research prototype validating Richelot Isogenies as Computational Tarpits. \url{https://github.com/HalleneE/algebirc}}
}
```