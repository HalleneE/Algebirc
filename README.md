<div align="center">
  <h1>Algebirc V2: Pandora</h1>
  <p><strong>A Hyperelliptic Genus-2 Obfuscation Engine & Mathematical Tarpit</strong></p>

  [![GHC](https://img.shields.io/badge/GHC-≥9.4-blue)](#)
  [![License](https://img.shields.io/badge/license-MIT-green)](#)
  [![Status](https://img.shields.io/badge/status-experimental-orange)](#)
  [![Language](https://img.shields.io/badge/language-Haskell-purple)](#)
</div>

---

> *"A Program Is A Polynomial. A Polynomial Is A Path. A Path Has No Origin. When the Path Fails, the Dimension Closes."*

**Algebirc V2** is an experimental, post-quantum algebraic obfuscation framework. It takes raw programmatic logic (or any byte stream) and mathematically transforms it into equivalent operations over finite fields $GF(p)$, embedding the computation inside layered algebraic structures on **Genus-2 Hyperelliptic Curves**.

Unlike traditional obfuscators that rely on junk code, AST manipulation, or virtualization—which are easily dismantled by SMT Solvers (e.g., z3) and Symbolic Execution (e.g., angr)—Algebirc relies on **pure geometry and combinatorial explosion**. It shifts the security perimeter from syntactic heuristics to the rigorous mathematical hardness of the **Group Action Inverse Problem (GAIP)** and **Gröbner Basis Deflation**.

## The V2 Architecture

Algebirc V2 introduces a paradigm shift in anti-reverse-engineering: **Dimensional Disorientation**. It does not merely hide code; it traps analysis tools in a recursive geometric labyrinth.

### 1. The Genus-2 Richelot Walk
Data is mapped to *Mumford Divisors* on the Jacobian variety $J_\mathcal{C}$ of a Genus-2 hyperelliptic curve. The program execution is then forced to "walk" across a sequence of $(2,2)$-Richelot Isogenies. Every single step multiplies the underlying polynomial degree by 4. After 20 steps, the resulting system possesses a polynomial degree of $2^{40}$, obliterating any prospect of static algorithmic reversal via Gröbner basis reduction.

### 2. Isogeny-CBC (Cipher Block Chaining)
Algebirc V2 eliminates the 1:1 mapping of code to equations. Instead, it flattens the target program into a continuous byte stream and chunks it. The $x$-coordinate evaluated at the end of the Richelot Walk for Block $N$ is mathematically injected as the Initial Vector (IV) for Block $N+1$. Analyzing a single block out of context leads to immediate trajectory deviation on the curve.

### 3. Toxic Padding (Junk Injection)
The dimensions of your program are permanently obscured. If a data block is smaller than the maximum matrix degree ($deg=64$), Algebirc invokes True OS Entropy (`getRandomBytes`) to pad the remainder. To an external observer, a 4-byte string and a 400-byte function are mathematically indistinguishable—both manifest as massive, fully dense Genus-2 polynomials.

### 4. PBKDF2 Integration
Default seeds are dead. Algebirc V2 derives its Isogeny pathing via PBKDF2 (10,000 rounds of SHA256) keyed by a user passphrase. Without the exact passphrase, the generated Initial Vector (IV) and Richelot parameters will misalign, causing the engine to reconstruct corrupted, pseudo-random byte streams that fail to parse or execute.

### 5. Adversarial Oracle & Red-Teaming (Analysis Suite)
Algebirc does not blindly trust its math. The engine ships with an internal `AlgebraicLeakage` and `AdversarialOracle` suite. During compilation, it autonomously attacks itself to measure Gaussian Rank Inference, Strict Avalanche Criterion (SAC), and Gröbner Basis complexity. It statically guarantees that its own output matrices are impervious to Subspace Attacks.

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

**Designed To Obliterate:**
- **SMT Solvers & Symbolic Execution:** Systems like `angr` or `z3` will crash via Out-Of-Memory (OOM) attempting to linearize the $2^{40}$ degree polynomials.
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
@software{algebircV2_2026,
  title  = {Algebirc V2: A Hyperelliptic Obfuscation Engine},
  year   = {2026},
  note   = {Experimental research prototype validating Richelot Isogenies as Computational Tarpits. \url{https://github.com/HalleneE/algebirc}}
}
```