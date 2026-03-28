# Algebirc: High-Genus Algebraic Obfuscation Engine

Algebirc is a Turing-complete software obfuscation framework constructed upon the dual foundations of high-genus algebraic geometry and lattice-based cryptography. The engine sits at the intersection of Fully Homomorphic Encryption (FHE) and program obfuscation, implementing a hybrid architecture designed to protect both computational logic and sensitive data states.

## Mathematical Foundations

The system operates through a multi-layered algebraic pipeline to ensure mathematical irreducibility and computational hardness:

1.  Isogeny-Based Control Flow: Logical branching is mapped onto Richelot isogenies between Genus-2 hyperelliptic Jacobians. By utilizing geometric correspondences, the engine evaluates parallel "True" and "False" execution paths simultaneously. This construction, termed "Invisible Logic Gates," eliminates conditional jump instructions and replaces them with continuous isogeny walks that are indistinguishable under static analysis.
2.  Lattice-Based Data Protection (RLWE): Sensitive program data and polynomial coefficients are secured via Ring Learning With Errors (RLWE) constructions. This layer provides a homomorphic execution environment where mathematical operations are performed directly upon encrypted ciphertext ($c = a \cdot s + e + \Delta \cdot m$). This ensures that data remains opaque throughout the evaluation cycle while maintaining post-quantum hardness.
3.  Jacobian Arithmetic: Core group operations are executed within the Jacobian variety of hyperelliptic curves using Mumford representation. The system utilizes a hardened implementation of Cantor's algorithm, specifically engineered to resist zero-divisor collapse and identity leakage during isogeny mapping.

## Hardware-Level Security Architecture

Algebirc is engineered for Hardware-Constant-Time (CT) execution to mitigate Differential Power Analysis (DPA) and Microarchitectural Timing Attacks.

1.  Hand-written x86_64 Assembly: Critical arithmetic primitives, including 256-bit addition, subtraction, and constant-time selection (CT-MUX), are implemented in pure assembly. This bypasses compiler-induced branching and stack-based vulnerabilities.
2.  Deterministic Execution Environment: The Haskell orchestration layer interfaces with the C/Assembly backend via unsafe FFI bindings, ensuring deterministic execution without interference from the Garbage Collector (GC) or lazy evaluation thunks.
3.  Montgomery CIOS Backend: Modular multiplication and inversion are performed using the Coarsely Integrated Operand Scanning (CIOS) method, providing fixed-schedule execution independent of operand data patterns.
4.  Dense Vectorization: Polynomial structures are stored as dense, zero-padded unboxed vectors to ensure O(1) coefficient access and prevent data-dependent cache-timing leaks.

## Project Structure

The codebase is organized into modular components reflecting the hybrid nature of the system:

*   Algebirc.Core: Unboxed U256 primitives, Montgomery arithmetic backend, and RLWE-compatible ring structures.
*   Algebirc.Geometry: Hyperelliptic curve specifications, Richelot isogeny mappings, and Jacobian group law implementation.
*   Algebirc.Obfuscation: Metamorphic transformation pipelines for encoding programs into polynomial rings.
*   Algebirc.Evaluator: The hybrid execution engine capable of evaluating both geometric isogenies and RLWE-encrypted data.
*   cbits: The high-performance C and x86_64 assembly implementation of hardware-hardened primitives.

## Implementation and Validation

### Prerequisites
*   GHC 9.4.7 or higher
*   Cabal 3.0 or higher
*   GCC (with support for x86_64 inline assembly)

### Build Instructions
To compile the library and link the constant-time backend:
```bash
cabal build lib:algebirc
```

### Verification
The project includes a comprehensive suite of QuickCheck properties and side-channel cryptanalysis probes to verify mathematical correctness, homomorphic integrity, and timing invariance:
```bash
cabal test algebirc-test
```

## Documentation
Additional technical specifications are available in the following internal document:
*   RICHELOT_PIPELINE.md: Formal analysis of Kunzweiler correspondences and isogeny walk construction.

## Disclaimer
Algebirc is a research prototype intended for the study of advanced algebraic obfuscation and homomorphic evaluation techniques. While it incorporates state-of-the-art constant-time methodologies, it has not undergone a formal third-party security audit.
