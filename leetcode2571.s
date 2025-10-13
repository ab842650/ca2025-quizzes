    .data
PASS_MSG:    .asciz "PASS\n"
FAIL_MSG:    .asciz "FAIL\n"

# 10 test inputs
test_cases:
    .word 1, 2, 3, 5, 7, 15, 39, 100, 1234, 1023

# Expected minimal steps for each input
expected_steps:
    .word 1, 1, 2, 2, 2, 2, 3, 3, 5, 2

    .text
    .globl main

# =====================================================
# main: runs 10 test cases; prints PASS if all match.
# =====================================================
main:
    la   s0, test_cases      # s0 = ptr to inputs
    la   s1, expected_steps  # s1 = ptr to expected
    li   s2, 10              # s2 = number of test cases
    li   s4, 1               # s4 = pass flag (1 = true)

case_loop:
    beqz s2, all_done        # if all cases done → finish

    # Load n and expected steps
    lw   s5, 0(s0)           # s5 = n
    lw   s6, 0(s1)           # s6 = expected steps
    addi s0, s0, 4
    addi s1, s1, 4

    # ---- Run algorithm for this n ----
    mv   s2, s2              # keep counter intact
    mv   s7, s5              # s7 = n (working copy)
    li   s3, 0               # s3 = steps = 0

Loop:
    beqz s7, Loop_end        # if (n == 0) break

    # --- Compute e = 31 - clz(n) ---
    mv   a0, s7
    jal  ra, clz             # a0 = number of leading zeros
    li   t0, 31
    sub  t0, t0, a0          # t0 = e = 31 - clz(n)

    # --- Compute p = 2^e ---
    li   t1, 1
    sll  t1, t1, t0          # t1 = p = 1 << e

    # --- Compute two_n = 2 * n ---
    slli t3, s7, 1           # t3 = two_n

    # --- Compute three_p = 3 * p = p + 2p ---
    slli t4, t1, 1           # t4 = 2p
    add  t4, t4, t1          # t4 = 3p

    # --- Unsigned compare: if (2n <= 3p) go to p else go to 2p ---
    sltu t5, t4, t3          # t5 = (3p < 2n)
    beqz t5, to_p            # if (2n <= 3p) → to_p

    # Case: move toward 2p → n = 2p - n
    slli t6, t1, 1           # t6 = 2p
    sub  s7, t6, s7          # n = 2p - n
    j    step_done

to_p:
    # Case: move toward p → n = n - p
    sub  s7, s7, t1          # n = n - p

step_done:
    addi s3, s3, 1           # steps++
    j    Loop

Loop_end:
    # Compare computed steps (s3) with expected (s6)
    beq  s3, s6, case_ok
    li   s4, 0               # mark FAIL

case_ok:
    addi s2, s2, -1          # next case
    j    case_loop

all_done:
    # Print PASS/FAIL
    beqz s4, print_fail

print_pass:
    la   a0, PASS_MSG
    li   a7, 4               # print string
    ecall
    j    exit

print_fail:
    la   a0, FAIL_MSG
    li   a7, 4               # print string
    ecall

exit:
    li   a7, 10              # exit
    ecall


# =====================================================
# clz: count leading zeros (returns in a0)
# Input:  a0 = x (x > 0)
# Output: a0 = number of leading zeros in 32-bit x
# =====================================================
clz:
    li t0,32 #t0 = n
    # chceck high 16 bits
    # --- k = 16 ---
    srli  t1, a0, 16        # y = x >> 16
    sltu  t2, x0, t1        # cond = (y != 0) ? 1 : 0
    sub   t3, x0, t2        # mask = -cond (0x00000000 or 0xFFFFFFFF)
    slli  t4, t2, 4         # cond*16  
    sub   t0, t0, t4        # n -= 16 (if cond)
    xori  t5, t3, -1        # ~mask
    and   a0, a0, t5        # a0 = (a0 & ~mask) | (y & mask)
    and   t1, t1, t3
    or    a0, a0, t1
    
    # --- k = 8 ---
    # similar to k = 16
    srli  t1, a0, 8
    sltu  t2, x0, t1
    sub   t3, x0, t2
    slli   t4, t2, 3         # cond*8
    sub   t0, t0, t4
    xori  t5, t3, -1
    and   a0, a0, t5
    and   t1, t1, t3
    or    a0, a0, t1

    # --- k = 4 ---
    srli  t1, a0, 4
    sltu  t2, x0, t1
    sub   t3, x0, t2
    slli   t4, t2, 2         # cond*4
    sub   t0, t0, t4
    xori  t5, t3, -1
    and   a0, a0, t5
    and   t1, t1, t3
    or    a0, a0, t1

    # --- k = 2 ---
    srli  t1, a0, 2
    sltu  t2, x0, t1
    sub   t3, x0, t2
    slli   t4, t2, 1         # cond*2
    sub   t0, t0, t4
    xori  t5, t3, -1
    and   a0, a0, t5
    and   t1, t1, t3
    or    a0, a0, t1

    # --- k = 1 ---
    srli  t1, a0, 1
    sltu  t2, x0, t1
    sub   t3, x0, t2        # mask
    # cond*1 
    sub   t0, t0, t2        # n -= 1 if cond
    xori  t5, t3, -1
    and   a0, a0, t5
    and   t1, t1, t3
    or    a0, a0, t1

    # return n - x  
    sub   a0, t0, a0
    ret