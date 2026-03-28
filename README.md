# Algebirc: High-Genus Algebraic Obfuscation Engine

Algebirc is a Turing-complete software obfuscation framework constructed upon the formal principles of high-genus algebraic geometry and lattice-based cryptography. The engine implements a novel execution model termed "Invisible Logic Gates," wherein computational branching is mapped onto Richelot isogenies between Genus-2 hyperelliptic Jacobians. This architecture ensures that execution paths are structurally and temporally indistinguishable under both static analysis and microarchitectural side-channel observation.

## Mathematical Foundations

The obfuscation pipeline leverages three distinct algebraic layers to achieve computational hardness and mathematical irreducibility:

1.  Isogeny-Based Topology: Logical control flow is translated into isogeny walks within the isogeny graph of genus-2 curves. By utilizing Richelot correspondences, the engine evaluates parallel "True" and "False" branches simultaneously, merging the results into a unified codomain. This methodology eliminates conditional jump instructions and replaces them with continuous geometric mappings.
2.  Lattice-Based Hardness: Internal state transitions and plaintext scalars are protected via Ring Learning With Errors (RLWE) constructions. This provides a secondary layer of post-quantum security for sensitive coefficients within the polynomial rings.
3.  Jacobian Arithmetic: Group operations are executed within the Jacobian variety of hyperelliptic curves. The system utilizes Mumford representation and a hardened implementation of Cantor's algorithm, specifically engineered to resist zero-divisor collapse and identity leakage.

## Hardware-Level Security Architecture

Algebirc is specifically engineered for Hardware-Constant-Time (CT) execution to mitigate Differential Power Analysis (DPA) and Microarchitectural Timing Attacks.

1.  Hand-written x86_64 Assembly: Critical arithmetic primitives, including 256-bit addition, subtraction, and constant-time selection (CT-MUX), are implemented in pure assembly. This bypasses compiler-induced branching, stack canary insertion, and register spilling vulnerabilities associated with high-level languages.
2.  Deterministic Execution Environment: The Haskell orchestration layer interfaces with the C/Assembly backend via unsafe FFI bindings. This ensures that the cryptographic core operates without interference from the GHC Garbage Collector (GC) or the non-deterministic timing overhead of lazy evaluation thunks.
3.  Montgomery CIOS Backend: Modular multiplication and inversion are performed using the Coarsely Integrated Operand Scanning (CIOS) method. This guarantees a fixed-schedule execution pattern that is independent of operand Hamming weight or secret bit-patterns.
4.  Dense Vectorization: Polynomial structures are stored as dense, zero-padded unboxed vectors. This ensures O(1) coefficient access and prevents data-dependent memory scan patterns that could lead to cache-timing leaks.

## Project Structure

The codebase is organized into modular components reflecting the abstraction layers of the system:

*   Algebirc.Core: Unboxed U256 primitives, Montgomery arithmetic backend, and Sylvester matrix calculators.
*   Algebirc.Geometry: Hyperelliptic curve specifications, Richelot isogeny mappings, and Siegel modular form evaluations.
*   Algebirc.Obfuscation: Metamorphic transformation pipelines for polynomial-based program encoding.
*   Algebirc.Evaluator: The geometric execution engine capable of oblivious branch evaluation.
*   cbits: The high-performance C and x86_64 assembly implementation of the hardware-hardened primitives.

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
The project includes a comprehensive suite of QuickCheck properties and side-channel cryptanalysis probes to verify both mathematical correctness and timing invariance:
```bash
cabal test algebirc-test
```

## Documentation
Additional technical specifications are available in the following internal documents:
*   RICHELOT_PIPELINE.md: Formal analysis of Kunzweiler correspondences and isogeny walk construction.
*   TESSERACT_ARCHITECTURE.md: Specification of the constant-time memory model and FFI boundary.

## Disclaimer
Algebirc is a research prototype intended for the study of high-genus algebraic obfuscation techniques. While it incorporates advanced constant-time methodologies, it has not undergone a formal third-party security audit. Production deployment should be preceded by exhaustive independent verification.
