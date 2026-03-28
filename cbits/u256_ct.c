#include <stdint.h>
#include <string.h>

typedef uint64_t u256_t[4];

// Fully unrolled 256-bit addition to prevent loop-induced branches
uint64_t u256_add_ct(u256_t r, const u256_t a, const u256_t b) {
    uint64_t sum, carry = 0;
    
    sum = a[0] + b[0] + carry;
    carry = (sum < a[0]) | ((sum == a[0]) & carry);
    r[0] = sum;

    sum = a[1] + b[1] + carry;
    carry = (sum < a[1]) | ((sum == a[1]) & carry);
    r[1] = sum;

    sum = a[2] + b[2] + carry;
    carry = (sum < a[2]) | ((sum == a[2]) & carry);
    r[2] = sum;

    sum = a[3] + b[3] + carry;
    carry = (sum < a[3]) | ((sum == a[3]) & carry);
    r[3] = sum;

    return carry;
}

// Fully unrolled 256-bit subtraction
uint64_t u256_sub_ct(u256_t r, const u256_t a, const u256_t b) {
    uint64_t diff, borrow = 0;

    diff = a[0] - b[0] - borrow;
    borrow = (a[0] < b[0]) | ((a[0] == b[0]) & borrow);
    r[0] = diff;

    diff = a[1] - b[1] - borrow;
    borrow = (a[1] < b[1]) | ((a[1] == b[1]) & borrow);
    r[1] = diff;

    diff = a[2] - b[2] - borrow;
    borrow = (a[2] < b[2]) | ((a[2] == b[2]) & borrow);
    r[2] = diff;

    diff = a[3] - b[3] - borrow;
    borrow = (a[3] < b[3]) | ((a[3] == b[3]) & borrow);
    r[3] = diff;

    return borrow;
}

// Fully unrolled Constant-Time Selection
void u256_select_ct(u256_t r, uint64_t cond, const u256_t a, const u256_t b) {
    uint64_t mask = -cond; // 0xFFF...FFF if cond=1, 0x000...000 if cond=0
    uint64_t nmask = ~mask;
    r[0] = (a[0] & mask) | (b[0] & nmask);
    r[1] = (a[1] & mask) | (b[1] & nmask);
    r[2] = (a[2] & mask) | (b[2] & nmask);
    r[3] = (a[3] & mask) | (b[3] & nmask);
}

// Fully unrolled Constant-Time Equality
uint64_t u256_eq_ct(const u256_t a, const u256_t b) {
    uint64_t diff = (a[0] ^ b[0]) | (a[1] ^ b[1]) | (a[2] ^ b[2]) | (a[3] ^ b[3]);
    return 1 - ((diff | (0 - diff)) >> 63);
}

void u256_modadd_ct(u256_t r, const u256_t a, const u256_t b, const u256_t m) {
    u256_t sum, diff;
    uint64_t carry = u256_add_ct(sum, a, b);
    uint64_t borrow = u256_sub_ct(diff, sum, m);
    uint64_t cond_keep_sum = (1 ^ carry) & borrow;
    u256_select_ct(r, cond_keep_sum, sum, diff);
}

void u256_modsub_ct(u256_t r, const u256_t a, const u256_t b, const u256_t m) {
    u256_t diff, corrected;
    uint64_t borrow = u256_sub_ct(diff, a, b);
    u256_add_ct(corrected, diff, m);
    u256_select_ct(r, borrow, corrected, diff);
}

// Unrolled outer loop of Montgomery Mul
void u256_mont_mul_ct(u256_t r, const u256_t a, const u256_t b, const u256_t m, uint64_t m0_inv) {
    uint64_t t[5] = {0};
    
    // Using GCC extensions to strictly inline unrolled blocks to avoid loop branches.
    #pragma GCC unroll 4
    for (int i = 0; i < 4; i++) {
        uint64_t carry = 0;
        
        #pragma GCC unroll 4
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
        
        #pragma GCC unroll 3
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

void u256_modpow_ct(u256_t r, const u256_t base_mont, const u256_t exp, const u256_t m, uint64_t m0_inv, const u256_t mont_one) {
    u256_t t_res;
    memcpy(t_res, mont_one, sizeof(u256_t));
    u256_t t_base;
    memcpy(t_base, base_mont, sizeof(u256_t));

    // GCC usually compiles a fixed 256 for loop efficiently, but we enforce it
    // because unrolling 256 completely is too large for icache.
    for (int i = 0; i < 256; i++) {
        int limb = i / 64;
        int bit  = i % 64;
        uint64_t b = (exp[limb] >> bit) & 1;
        
        u256_t res_mul;
        u256_mont_mul_ct(res_mul, t_res, t_base, m, m0_inv);
        
        u256_select_ct(t_res, b, res_mul, t_res);
        
        u256_mont_mul_ct(t_base, t_base, t_base, m, m0_inv);
    }
    
    memcpy(r, t_res, sizeof(u256_t));
}

void u256_modinv_ct(u256_t r, const u256_t a_mont, const u256_t m, uint64_t m0_inv, const u256_t mont_one) {
    u256_t exp;
    u256_t two = {2, 0, 0, 0};
    u256_sub_ct(exp, m, two);
    
    u256_modpow_ct(r, a_mont, exp, m, m0_inv, mont_one);
}
