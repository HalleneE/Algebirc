// =====================================================================
// Hand-Written x86_64 Assembly for U256 Constant-Time Primitives
// 
// This bypasses GCC/Clang entirely. No stack canaries, no secret branch
// prediction, no register spilling. Just pure, deterministic silicon.
// =====================================================================

.text
.align 16

// uint64_t u256_add_ct(u256_t r, const u256_t a, const u256_t b);
// rdi = r, rsi = a, rdx = b
.global u256_add_ct
.type u256_add_ct, @function
u256_add_ct:
    // Load limbs of A
    movq 0(%rsi), %r8
    movq 8(%rsi), %r9
    movq 16(%rsi), %r10
    movq 24(%rsi), %r11

    // Add limbs of B with hardware carry flag (CF)
    addq 0(%rdx), %r8
    adcq 8(%rdx), %r9
    adcq 16(%rdx), %r10
    adcq 24(%rdx), %r11

    // Capture the final carry out into RAX without branching
    setc %al
    movzbq %al, %rax

    // Store result into R
    movq %r8, 0(%rdi)
    movq %r9, 8(%rdi)
    movq %r10, 16(%rdi)
    movq %r11, 24(%rdi)

    ret

// uint64_t u256_sub_ct(u256_t r, const u256_t a, const u256_t b);
// rdi = r, rsi = a, rdx = b
.global u256_sub_ct
.type u256_sub_ct, @function
u256_sub_ct:
    // Load limbs of A
    movq 0(%rsi), %r8
    movq 8(%rsi), %r9
    movq 16(%rsi), %r10
    movq 24(%rsi), %r11

    // Subtract limbs of B with hardware borrow flag (CF)
    subq 0(%rdx), %r8
    sbbq 8(%rdx), %r9
    sbbq 16(%rdx), %r10
    sbbq 24(%rdx), %r11

    // Capture the final borrow out into RAX without branching
    setc %al
    movzbq %al, %rax

    // Store result into R
    movq %r8, 0(%rdi)
    movq %r9, 8(%rdi)
    movq %r10, 16(%rdi)
    movq %r11, 24(%rdi)

    ret

// void u256_select_ct(u256_t r, uint64_t cond, const u256_t a, const u256_t b);
// rdi = r, rsi = cond (0 or 1), rdx = a, rcx = b
.global u256_select_ct
.type u256_select_ct, @function
u256_select_ct:
    // Create hardware masks
    negq %rsi         // If cond=1, rsi=0xFFFFFFFFFFFFFFFF. If cond=0, rsi=0x0
    movq %rsi, %r8
    notq %r8          // r8 is now ~mask

    // Limb 0
    movq 0(%rdx), %r9
    andq %rsi, %r9    // a[0] & mask
    movq 0(%rcx), %r10
    andq %r8, %r10    // b[0] & ~mask
    orq  %r10, %r9
    movq %r9, 0(%rdi)

    // Limb 1
    movq 8(%rdx), %r9
    andq %rsi, %r9    
    movq 8(%rcx), %r10
    andq %r8, %r10    
    orq  %r10, %r9
    movq %r9, 8(%rdi)

    // Limb 2
    movq 16(%rdx), %r9
    andq %rsi, %r9    
    movq 16(%rcx), %r10
    andq %r8, %r10    
    orq  %r10, %r9
    movq %r9, 16(%rdi)

    // Limb 3
    movq 24(%rdx), %r9
    andq %rsi, %r9    
    movq 24(%rcx), %r10
    andq %r8, %r10    
    orq  %r10, %r9
    movq %r9, 24(%rdi)

    ret
