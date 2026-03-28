# Algebirc: High-Genus Algebraic Obfuscation Engine

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Stability: Research Prototype](https://img.shields.io/badge/Stability-Research--Prototype-blue.svg)]()

**Algebirc** is a Turing-complete software obfuscation engine built on the formal foundations of high-genus algebraic geometry and lattice-based cryptography. It utilizes **Richelot Isogenies** over Genus-2 Hyperelliptic Jacobians to implement "Invisible Logic Gates"—branchless execution paths that are structurally indistinguishable under static and side-channel analysis.

## 🚀 Theoretical Foundations

The engine operates across three distinct algebraic layers to ensure mathematical irreducibility and computational hardness:

1.  **Isogeny-Based Topology**: Logic branching is mapped onto isogeny walks between genus-2 Jacobians. Using Richelot correspondences, the engine executes parallel "True" and "False" paths, merging them into a target codomain curve. This removes conditional jumps (`if/else`) and replaces them with continuous geometric mappings.
2.  **Lattice Hardness**: Plaintext scalars and polynomial coefficients are protected via Ring Learning With Errors (RLWE) compatible rings, providing a post-quantum hardness layer for state-transition protection.
3.  **Jacobian Arithmetic**: Efficient group operations are performed in the Jacobian variety of hyperelliptic curves using Mumford representation and Cantor’s algorithm, rigorously hardened against zero-divisor collapse.

## 🛡️ Hardened Security Architecture (Lapis-2)

Algebirc is engineered for **Hardware-Constant-Time (CT)** execution, specifically designed to resist Differential Power Analysis (DPA) and Microarchitectural Timing Attacks.

-   **Hand-written x86_64 Assembly**: Core arithmetic primitives (Addition, Subtraction, Selection) are implemented in pure assembly to bypass compiler-induced branching and stack canary spikes.
-   **Zero-Overhead FFI**: The Haskell orchestration layer interfaces with the C/ASM backend via `unsafe` FFI bindings, ensuring deterministic execution without Garbage Collector (GC) interference or lazy-evaluation thunks.
-   **Montgomery CIOS Backend**: Modular multiplication and inversion are performed using the Coarsely Integrated Operand Scanning (CIOS) method, providing fixed-schedule execution independent of Hamming weight or secret bit-patterns.
-   **Dense Vector Representation**: Polynomials are stored as dense, zero-padded unboxed vectors, ensuring $O(1)$ coefficient access and branchless scan patterns.

## 🛠️ Project Structure

-   `src/Algebirc/Core/`: Unboxed U256 primitives, Montgomery arithmetic, and Resultant calculators.
-   `src/Algebirc/Geometry/`: Elliptic/Hyperelliptic curve logic, Richelot isogeny mappings, and Siegel modular forms.
-   `src/Algebirc/Obfuscation/`: Pipeline architecture for metamorphic polynomial transformations.
-   `src/Algebirc/Evaluator/`: The "Holy Grail" evaluator capable of executing geometric branchless logic.
-   `cbits/`: The high-performance C/Assembly backend for hardware-level security.

## 🧪 Installation & Validation

### Prerequisites
-   GHC 9.4.7+
-   Cabal 3.0+
-   GCC (for x86_64 assembly support)

### Build
```bash
cabal build lib:algebirc
```

### Running Tests
The project includes an exhaustive suite of QuickCheck properties and side-channel analysis probes:
```bash
cabal test algebirc-test
```

## 📜 Academic Reference
This project is part of ongoing research into High-Genus Algebraic Obfuscation. For detailed architectural insights, refer to the internal documentation:
-   `RICHELOT_PIPELINE.md`: Deep dive into Kunzweiler correspondences.
-   `TESSERACT_ARCHITECTURE.md`: Formal specification of the constant-time execution model.

---
**Warning**: This is a research prototype. While it implements state-of-the-art constant-time techniques, it has not undergone a formal third-party security audit for production deployment.
