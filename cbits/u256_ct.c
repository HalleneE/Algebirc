#include <stdint.h>
#include <string.h>

// 256-bit representation using 4 x 64-bit limbs
// LSB is limb[0], MSB is limb[3]
typedef uint64_t u256_t[4];

uint64_t u256_add_ct(u256_t r, const u256_t a, const u256_t b);
uint64_t u256_sub_ct(u256_t r, const u256_t a, const u256_t b);
void u256_select_ct(u256_t r, uint64_t cond, const u256_t a, const u256_t b);

#ifndef __x86_64__

// Constant-Time 256-bit addition
// Computes r = a + b, returns carry out (0 or 1)
uint64_t u256_add_ct(u256_t r, const u256_t a, const u256_t b) {
    uint64_t carry = 0;
    for (int i = 0; i < 4; i++) {
        uint64_t sum = a[i] + b[i] + carry;
        // Overflow logic:
        // if sum < a[i] (or sum < b[i] if carry=0) -> overflow
        carry = (sum < a[i]) | ((sum == a[i]) & carry);
        r[i] = sum;
    }
    return carry;
}

// Constant-Time 256-bit subtraction
// Computes r = a - b, returns borrow out (0 or 1)
uint64_t u256_sub_ct(u256_t r, const u256_t a, const u256_t b) {
    uint64_t borrow = 0;
    for (int i = 0; i < 4; i++) {
        uint64_t diff = a[i] - b[i] - borrow;
        // Borrow logic:
        // if a[i] < b[i] or (a[i] == b[i] and borrow == 1)
        borrow = (a[i] < b[i]) | ((a[i] == b[i]) & borrow);
        r[i] = diff;
    }
    return borrow;
}

// Constant-Time Selection
// cond must be strictly 0 or 1.
// If cond == 1, r = a; If cond == 0, r = b;
void u256_select_ct(u256_t r, uint64_t cond, const u256_t a, const u256_t b) {
    uint64_t mask = -cond; // 0xFFF...FFF if cond=1, 0x000...000 if cond=0
    uint64_t nmask = ~mask;
    for (int i = 0; i < 4; i++) {
        r[i] = (a[i] & mask) | (b[i] & nmask);
    }
}

#endif // __x86_64__

// Constant-Time Equality
// Returns 1 if a == b, else 0
uint64_t u256_eq_ct(const u256_t a, const u256_t b) {
    uint64_t diff = 0;
    for (int i = 0; i < 4; i++) {
        diff |= (a[i] ^ b[i]);
    }
    // IsZero trick: if diff is 0, -diff doesn't borrow, MSB stays 0.
    // If diff != 0, diff | -diff has MSB set to 1.
    return 1 - ((diff | (0 - diff)) >> 63);
}

// Constant-Time Modular Addition
// Computes r = (a + b) mod m
void u256_modadd_ct(u256_t r, const u256_t a, const u256_t b, const u256_t m) {
    u256_t sum, diff;
    uint64_t carry = u256_add_ct(sum, a, b);
    
    // We compute diff = sum - m.
    uint64_t borrow = u256_sub_ct(diff, sum, m);
    
    // If carry == 1, sum > 2^256 > m, so we MUST subtract m (take diff).
    // If carry == 0, if borrow == 1, sum < m, so we keep sum.
    // So we select sum if (carry == 0 && borrow == 1). Otherwise diff.
    uint64_t cond_keep_sum = (1 ^ carry) & borrow;
    
    u256_select_ct(r, cond_keep_sum, sum, diff);
}

// Constant-Time Modular Subtraction
// Computes r = (a - b) mod m
void u256_modsub_ct(u256_t r, const u256_t a, const u256_t b, const u256_t m) {
    u256_t diff, corrected;
    uint64_t borrow = u256_sub_ct(diff, a, b);
    
    // If borrow == 1, a < b, so we must add m to the negative result.
    u256_add_ct(corrected, diff, m);
    
    // Select corrected if borrow == 1, else diff.
    u256_select_ct(r, borrow, corrected, diff);
}

// Montgomery Multiplication: r = (a * b * R^-1) mod m
// Requires: m is odd, m0_inv = -m[0]^-1 mod 2^64
// This is CIOS (Coarsely Integrated Operand Scanning) method.
void u256_mont_mul_ct(u256_t r, const u256_t a, const u256_t b, const u256_t m, uint64_t m0_inv) {
    uint64_t t[5] = {0};
    
    for (int i = 0; i < 4; i++) {
        uint64_t carry = 0;
        for (int j = 0; j < 4; j++) {
            unsigned __int128 prod = (unsigned __int128)a[i] * b[j] + t[j] + carry;
            t[j] = (uint64_t)prod;
            carry = (uint64_t)(prod >> 64);
        }
        t[4] = carry;

        uint64_t q = t[0] * m0_inv;
        carry = 0;
        unsigned __int128 prod0 = (unsigned __int128)q * m[0] + t[0] + carry;
        carry = (uint64_t)(prod0 >> 64);
        
        for (int j = 1; j < 4; j++) {
            unsigned __int128 prod = (unsigned __int128)q * m[j] + t[j] + carry;
            t[j-1] = (uint64_t)prod;
            carry = (uint64_t)(prod >> 64);
        }
        
        unsigned __int128 prod4 = (unsigned __int128)t[4] + carry;
        t[3] = (uint64_t)prod4;
        t[4] = (uint64_t)(prod4 >> 64);
    }

    u256_t diff;
    uint64_t borrow = u256_sub_ct(diff, t, m);
    uint64_t cond_keep_t = (1 ^ t[4]) & borrow;
    u256_select_ct(r, cond_keep_t, t, diff);
}

// Constant-Time Exponentiation: r = (base^exp) mod m
// Operates in Montgomery domain. 
// base_mont must ALREADY be in Montgomery form (base * R mod m).
// mont_one is R mod m (which is 1 in Montgomery form).
void u256_modpow_ct(u256_t r, const u256_t base_mont, const u256_t exp, const u256_t m, uint64_t m0_inv, const u256_t mont_one) {
    u256_t t_res;
    memcpy(t_res, mont_one, sizeof(u256_t));
    u256_t t_base;
    memcpy(t_base, base_mont, sizeof(u256_t));

    for (int i = 0; i < 256; i++) {
        int limb = i / 64;
        int bit  = i % 64;
        uint64_t b = (exp[limb] >> bit) & 1;
        
        u256_t res_mul;
        u256_mont_mul_ct(res_mul, t_res, t_base, m, m0_inv);
        
        // If bit is 1, keep multiplied result. Otherwise keep previous result.
        u256_select_ct(t_res, b, res_mul, t_res);
        
        // Square the base for next bit
        u256_mont_mul_ct(t_base, t_base, t_base, m, m0_inv);
    }
    
    memcpy(r, t_res, sizeof(u256_t));
}

// Constant-Time Modular Inversion (Fermat's Little Theorem)
// Computes r = a^(m-2) mod m.
// m MUST be a prime.
void u256_modinv_ct(u256_t r, const u256_t a_mont, const u256_t m, uint64_t m0_inv, const u256_t mont_one) {
    u256_t exp;
    u256_t two = {2, 0, 0, 0};
    u256_sub_ct(exp, m, two); // exp = m - 2
    
    u256_modpow_ct(r, a_mont, exp, m, m0_inv, mont_one);
}
