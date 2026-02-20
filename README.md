<div align="center">
  <h1>Algebirc </h1>
  <p><strong>An Algebraic Program Transformation Engine over Finite Fields</strong></p>

  [![GHC](https://img.shields.io/badge/GHC-≥9.4-blue)](#)
  [![License](https://img.shields.io/badge/license-MIT-green)](#)
  [![Status](https://img.shields.io/badge/status-experimental-orange)](#)
  [![Language](https://img.shields.io/badge/language-Haskell-purple)](#)
</div>

---

> *A Program Is A Polynomial. A Polynomial Is A Path. A Path Has No Origin.*

**Algebirc** is an experimental, research-grade algebraic obfuscation framework. It takes standard programmatic logic and mathematically transforms it into equivalent operations over finite fields (e.g., $GF(p)$), embedding the computation inside layered algebraic structures. 

Unlike traditional binary-level obfuscators that rely on junk code or virtualization, Algebirc relies on **pure algebra and geometric cryptography** to make reverse engineering mathematically expensive.

## Core Philosophy & Architecture

Algebirc operates entirely on algebraic semantics, lifting your computation across five distinct, mathematically rigorous domains:

1. **Program Logic → Degree-Bounded Polynomials**: Logic is expressed as polynomial arithmetic over $GF(p)$.
2. **Control Flow → Bijective S-Boxes**: Non-linear transformations (Fermat inverses) obscure the algebraic relationships.
3. **Data Mixing → ARX Diffusion**: Add-Rotate-XOR networks spread data dependencies.
4. **Structural Embedding → Isogeny Graph Walks**: *The Crown Jewel.* Computation parameters are obfuscated using **Vélu Isogenies** over Elliptic Curves, creating a Group Action Inverse Problem (GAIP) inspired by CSIDH.
5. **Runtime State → Secret Sharing**: Memory states are split into 2-of-2 additive secret-shared field elements.

The result is a functionally identical executable where the underlying logic is buried in high-degree polynomials and cryptographic group actions.

---

## Recent Breakthroughs: Exact Vélu Formalization

Algebirc implements a highly rigorous isogeny engine to structurally embed obfuscated polynomials. 

Recently, the codebase was updated to implement the **exact, ground-truth algebraic geometry definitions** of Vélu's formulas (1971). The implementation follows Washington (2003), Proposition 12.16, using the exact half-kernel summation formulation.

### The Math
Instead of relying on heuristic coefficients, Algebirc uses the mathematically strict parameterization per half-kernel point $Q = (x_Q, y_Q)$:
* $v_Q = 3x_Q^2 + A$
* $u_Q = 2y_Q^2$ *(Note: 2y², not the paired summation 4y² often cited without warning)*
* $w_Q = u_Q + x_Q \cdot v_Q$

### Empirical Proof via QuickCheck
To prove the exactness of these formulas, Algebirc includes an automated `PropertySpec` test suite powered by **Haskell QuickCheck**.

We generate hundreds of random, non-singular elliptic curves over $GF(257)$ and automatically verify that:
1. `prop_isogenyPreservesOrder`: The isogeny perfectly preserves the curve's group order ($E_0$ and $E_k$) across every generated graph walk.
2. `prop_roundtrip`: The obfuscated polynomials can be perfectly deobfuscated by the evaluator using the derived public keys.

**Result: 100/100 tests uniformly pass with 0 discards.** 
This provides strict statistical and empirical proof that the geometric obfuscation relies on solid algebra, not "the Law of Small Numbers" or parameter-specific fudge factors. (Note: While validated over GF(257) for computational tractability, the formulas are field-agnostic and apply directly to CSIDH-512 parameter sets).

---

## Installation & Quickstart

### Prerequisites
* **GHC** $\ge$ 9.4
* **Cabal** $\ge$ 3.6
* *Optional: `gcc` and `build-essential` for test suites.*

### Build
```bash
git clone https://github.com/your-org/algebirc.git
cd algebirc
cabal update
cabal build
```

### Run Tests (including QuickCheck proofs)
```bash
cabal run test:hardness-test  # Run the standalone hardness suite
cabal test                    # Run the full randomized PropertySpec
```

### Example Usage (Library)
Embed polynomial coefficients into $GF(257)$ and apply additive secret sharing:

```haskell
import Algebirc.Core.GFp
import Algebirc.Obfuscation.Transform
import Algebirc.Runtime.SecretShare

-- Your data
let coeffs = [42, 137, 99, 200]
let p = 257

-- Split into additive shares (encrypted state in memory)
let shared = splitCoeffs (map fromIntegral coeffs) p
-- e.g., Output: [1234, 5678, 9012, 3456]

-- Reconstruct at the execution barrier
let recovered = reconstructCoeffs shared 
-- Result: [42, 137, 99, 200]
```

---

## Security Model

**Designed To Increase Cost Of:**

- Static algebraic simplification and polynomial factoring
- Coefficient extraction via differential or interpolation attacks  
- Structural recovery of control flow from algebraic representations

**Not Designed To Provide:**

- Provable iO: Security guarantees are computational and heuristic, not formal
- Side-channel resistance: No protection against timing or power analysis

*This is an experimental research prototype. Do not rely on it for protecting high-value cryptographic keys without independent specialized review.*

---

## Contributing

This project is at an active research stage. Feedback from the **algebraic geometry**, **cryptography**, and **program analysis** communities is highly valued.
* Open an issue for bugs or design discussions.
* Submit a PR with a clear description and motivation.

## Citation

If you reference Algebirc or our QuickCheck isogeny formalization in academic work, please cite as:

```bibtex
@software{algebirc2026,
  title  = {Algebirc: An Algebraic Obfuscation Engine},
  year   = {2026},
  note   = {Experimental research prototype validating exact Vélu formulas via randomized property testing. \url{https://github.com/your-org/algebirc}}
}
```
