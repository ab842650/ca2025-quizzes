.data
# op codes: 0=ADD, 1=SUB, 2=MUL, 3=DIV, 4=SQRT
# Layout per case (8 bytes):
# [0] .byte op, [1] pad=0, [2..3] .half a, [4..5] .half b, [6..7] .half expected
testcases:

    # --- ADD (6 cases) ---
    .byte 0, 0
    .half 0x3f40, 0x3EA0, 0x3f88      # 0.75 + 0.3125 = 1.062
    .byte 0, 0
    .half 0x3F80, 0xBFf0, 0xbf60      # 1.0 + (-1.875) = -0.875
    .byte 0, 0
    .half 0x3f80, 0xbf80, 0x0000      # 1.0 + (-1.0) = 0
    .byte 0, 0
    .half 0x7F80, 0x3F80, 0x7F80      # +Inf + 1.0 = +Inf
    .byte 0, 0
    .half 0x7F80, 0xFF80, 0x7FC0      # +Inf + (-Inf) = NaN
    .byte 0, 0
    .half 0x7f00, 0x7f00, 0x7F80      # NaN(payload) + 2.0 = propagate

    # --- SUB (4 cases) ---
    .byte 1, 0
    .half 0x3FC0, 0x3F40, 0x3F40      # 1.5 - 0.75 = 0.75
    .byte 1, 0
    .half 0x3F80, 0x4000, 0xBF80      # 1.0 - 2.0 = -1.0
    .byte 1, 0
    .half 0x3F80, 0x3F80, 0x0000      # 1.0 - 1.0 = 0
    .byte 1, 0
    .half 0x7F80, 0x7F80, 0x7FC0      # +Inf - +Inf = NaN
    

   # --- MUL (6 cases) ---
    .byte 2, 0
    .half 0x4040, 0x40c0, 0x4190     # 3.0 * 6.0 = 18.0
    .byte 2, 0
    .half 0x3F80, 0x3F40, 0x3F40      # 1.0 * 0.75 = 0.75
    .byte 2, 0
    .half 0x3FC8, 0x40d2, 0x4124      # 1.5625 * 6.5625 = 10.25390625
    .byte 2, 0
    .half 0x7F80, 0xBF80, 0xFF80      # +Inf * (-1.0) = -Inf
    .byte 2, 0
    .half 0x7F80, 0x0000, 0x7FC0      # +Inf * +0 = NaN (invalid)
    .byte 2, 0
    .half 0x0000, 0x0000, 0x0000      # 0 * 0 = 0 

    # --- DIV (7 cases) ---
    .byte 3, 0
    .half 0x3F80, 0x4000, 0x3F00      # 1.0 / 2.0 = 0.5
    .byte 3, 0
    .half 0x40b8, 0x4000, 0x4038      # 5.75 / 2 = 2.875
    .byte 3, 0
    .half 0xBF80, 0x4000, 0xBF00      # -1.0 / 2.0 = -0.5
    .byte 3, 0
    .half 0x7F80, 0x4000, 0x7F80      # +Inf / 2.0 = +Inf
    .byte 3, 0
    .half 0x3F80, 0x7F80, 0x0000      # 1.0 / +Inf = +0
    .byte 3, 0
    .half 0x0000, 0x0000, 0x7fc0      # 0 / 0 = NAN
    .byte 3, 0
    .half 0x3F80, 0x0000, 0x7f80      # 1.0 / 0 = +inf

    # --- SQRT (5 cases, b ignored) ---
    .byte 4, 0
    .half 0x4080, 0x0000, 0x4000      # sqrt(4.0) = 2.0
    .byte 4, 0
    .half 0x40c8, 0x0000, 0x4020      # sqrt(6.25) = 2.5
    .byte 4, 0
    .half 0xBF80, 0x0000, 0x7FC0      # sqrt(-1.0) = NaN
    .byte 4, 0
    .half 0x7F80, 0x0000, 0x7F80      # sqrt(+Inf) = +Inf
    .byte 4, 0
    .half 0x0000, 0x0000, 0x0000      # sqrt(+0) = +0

num_cases: .word 28


msg_add:     .asciz "[ADD] "
msg_sub:     .asciz "[SUB] "
msg_mul:     .asciz "[MUL] "
msg_div:     .asciz "[DIV] "
msg_sqrt:    .asciz "[SQRT] "
msg_unknown: .asciz "[?] "

msg_pass:    .asciz "PASS "
msg_fail:    .asciz "FAIL "
msg_result:  .asciz " -> result=0x"
newline:     .asciz "\n"
hexbuf:      .asciz "0000"   # 仍作為暫存區，但不以字串列印

.text
.globl main
main:
    la   s1, testcases
    lw   s2, num_cases
    li   s0, 0

test_loop:
    bge  s0, s2, test_done

    # addr = base + i * 8
    slli t0, s0, 3
    add  t0, s1, t0

    lbu  t2, 0(t0)          # op
    mv   s11, t2            # 保存 op（避免子程序破壞）
    lhu  a0, 2(t0)          # a
    lhu  a1, 4(t0)          # b (ignored for sqrt)
    lhu  s3, 6(t0)          # expected

    # dispatch
    beqz t2, do_add
    li   t1, 1
    beq  t2, t1, do_sub
    li   t1, 2
    beq  t2, t1, do_mul
    li   t1, 3
    beq  t2, t1, do_div
    li   t1, 4
    beq  t2, t1, do_sqrt
    j    do_unknown

do_add:   jal ra, bf16_add
  j after_call
do_sub:   jal ra, bf16_sub
  j after_call
do_mul:   jal ra, bf16_mul
  j after_call
do_div:   jal ra, bf16_div
  j after_call
do_sqrt:  jal ra, bf16_sqrt
 j after_call
do_unknown:
    # 未知 op：直接把 a0 當結果（或清零都可）
    addi a0, x0, 0

after_call:
    mv   s4, a0
    bne  a0, s3, test_fail

test_pass:
    jal  print_op_label   # 根據 s11 印 "[OP] "
    la   a0, msg_pass
    li   a7, 4
    ecall
    j    print_result

test_fail:
    jal  ra, print_op_label
    la   a0, msg_fail
    li   a7, 4
    ecall

print_result:
    la   a0, msg_result
    li   a7, 4
    ecall

    mv   a0, s4
    jal  ra, print_hex16_str   # 改為逐字元列印，避免 NUL 被顯示

    la   a0, newline
    li   a7, 4
    ecall

    addi s0, s0, 1
    j    test_loop

test_done:
    li   a7, 10
    ecall

# --- print_op_label ---
# 使用保存的 s11 (op) 印出對應標籤
print_op_label:
    addi sp, sp, -8
    sw   t0, 0(sp)
    sw   t1, 4(sp)

    mv   t0, s11
    beqz t0, .Lprint_add
    li   t1, 1
    beq  t0, t1, .Lprint_sub
    li   t1, 2
    beq  t0, t1, .Lprint_mul
    li   t1, 3
    beq  t0, t1, .Lprint_div
    li   t1, 4
    beq  t0, t1, .Lprint_sqrt
    la   a0, msg_unknown
    j    .Lprint_go

.Lprint_add:   la a0 msg_add
   j .Lprint_go
.Lprint_sub:   la a0, msg_sub
   j .Lprint_go
.Lprint_mul:   la a0, msg_mul
   j .Lprint_go
.Lprint_div:   la a0, msg_div
   j .Lprint_go
.Lprint_sqrt:  la a0, msg_sqrt

.Lprint_go:
    li   a7, 4
    ecall
    lw   t0, 0(sp)
    lw   t1, 4(sp)
    addi sp, sp, 8
    ret

# --- print_hex16_nostr ---
# input: a0 = 16-bit value to print in hex (4 digits)
# 逐字元用 ecall 11 輸出，不用 ecall 4（避免印出 NUL）
print_hex16_str:
    addi sp, sp, -20
    sw   t0, 0(sp)
    sw   t1, 4(sp)
    sw   t2, 8(sp)
    sw   t3, 12(sp)
    sw   t4, 16(sp)

    la   t0, hexbuf          # hexbuf must be at least 5 bytes ("0000\0")
    addi t1, t0, 3           # write from hexbuf[3] down to [0]
    li   t2, 4               # 4 nibbles
    li   t3, 10              # constant 10

1:  # loop over 4 nibbles
    andi t4, a0, 0xF         # low nibble
    srli a0, a0, 4
    blt  t4, t3, 2f
    addi t4, t4, 55          # 'A'(65) - 10 = 55
    j    3f
2:
    addi t4, t4, 48          # '0'..'9'
3:
    sb   t4, 0(t1)           # store char
    addi t1, t1, -1
    addi t2, t2, -1
    bnez t2, 1b

    # ensure NUL stays at hexbuf[4] (already there if hexbuf was .asciz "0000")
    la   a0, hexbuf
    li   a7, 4               # print string
    ecall

    lw   t0, 0(sp)
    lw   t1, 4(sp)
    lw   t2, 8(sp)
    lw   t3, 12(sp)
    lw   t4, 16(sp)
    addi sp, sp, 20
    ret
    
#----------------------------------------  
bf16_isnan:
    # check exponent
    li   t1, 0x7F80
    and  t2, a0, t1
    bne  t2, t1, isnan_end        
    # check mantissa 
    li   t1, 0x007F
    and  t2, a0, t1
    beqz t2, isnan_end     

    li   a0, 1              # return 1
    ret

    isnan_end:
    li   a0, 0              # return 0
    ret
#----------------------------------------   
bf16_isinf:
    # check exponent
    li   t1, 0x7F80
    and  t2, a0, t1
    bne  t2, t1, isinf_end  
    # check mantissa 
    li   t1, 0x007F
    and  t2, a0, t1
    bnez t2, isinf_end     

    li   a0, 1              # return 1
    ret

    isinf_end:
    li   a0, 0              # return 0
    ret
#----------------------------------------
bf16_iszero:
    li   t1, 0x7FFF          
    and  t2, a0, t1
    bnez t2, not_zero    
    li   a0, 1            
    ret
not_zero:
    li   a0, 0           
    ret
#----------------------------------------

bf32_to_bf16:
   lw t0,0(a0)
   srli t1,t0,23 #shift right t0 23bits
   li t2,0xff #load 0xff
   and t1,t1,t2
   beq t1,t2,return_bf16_inf
   srli t1,t0,16
   andi t1,t1,1
   li t2,0x7fff
   add t1,t1,t2
   add t0,t0,t1
   srli a0,t0,16
   
   ret

   
return_bf16_inf:
    srli t1,t0,16
    li t2,0xffff
    and a0,t1,t2
    ret
#----------------------------------------
bf16_to_bf32:
    slli a0,a0,16
    ret
#----------------------------------------    
bf16_add:
    # s2 = sign_a
    # s3 = sign_b
    # s4 = exp_a
    # s5 = exp_b
    # s6 = mant_a
    # s7 = mant_b
    
    addi sp, sp, -24
    sw   s2, 0(sp)
    sw   s3, 4(sp)
    sw   s4, 8(sp)
    sw   s5, 12(sp)
    sw   s6, 16(sp)
    sw   s7, 20(sp)
    
    #------------ extract a
    addi   t0, a0,0
    srli t0, t0, 15
    andi s2, t0, 1          # s2 = sign_a

    addi   t0, a0,0
    srli t0, t0, 7
    andi s4, t0, 0xFF       # s4 = exp_a

    addi   t0, a0,0
    andi s6, t0, 0x7F       # s6 = mant_a
    #------------ extract b
    
    addi t0, a1,0
    srli t0, t0, 15
    andi s3, t0, 1          # s3 = sign_b

    addi   t0, a1,0
    srli t0, t0, 7
    andi s5, t0, 0xFF       # s5 = exp_b

    addi   t0, a1,0
    andi s7, t0, 0x7F       # s7 = mant_b

    
    #handle inf
    
    li t0,0xff
    
    bne s4,t0,check_exponent_b
    bnez s6,return_a
    bne s5,t0,return_a
    
    xor t0,s2,s3 #if signa=signb t6=0 else t6=1    
    
    or t0,s7,t0
    
    beqz t0,return_b
    
    li a0,0x7FC0
    j restore_regs
    
    
    return_a:
        j restore_regs
        
    
    return_b:
        addi a0,a1,0
        j restore_regs
    
    check_exponent_b:
        li t0,0xff
        beq s5,t0,return_b
        
        seqz t1,s4 #s2=!exp_a
        seqz t2,s6 #s3=!mantissa_a
        
        and t1,t1,t2
        
        bnez t1,return_b
        
        seqz t1,s5 #s2=!exp_b
        seqz t2,s7 #s3=!mantissa_b
        
        and t1,t1,t2
        
        bnez t1,return_a
        
        beqz s4,skip_a
        ori s6,s6,0x80
        
        skip_a:
        beqz s5,skip_b
        ori s7,s7,0x80
        
        skip_b:
        
        sub t0,s4,s5 #t0=exp_diff=exp_a-exp_b 
        bgtz t0,exp_a_bigger
        bltz t0,exp_b_bigger
        exp_equal:
        addi t1,s4,0 #t1=result_exp = exp_a
        j sign_check
        
        exp_a_bigger:
        
        addi t1,s4,0 #t1=result_exp = exp_a
        addi t0,t0,-8 # exp_diff -= 8
        
        bgtz t0,return_a #exp_diff>0 return a
        addi t0,t0,8
        srl s7,s7,t0 #mantissa_b >> exp_diff
        j sign_check
            
            
        exp_b_bigger:
        addi t1,s5,0 #t1= result_exp =b
        addi t0,t0,8
        
        bltz t0,return_b
        
        addi t0,t0,-8
        neg t0,t0
        srl s6,s6,t0
        
        sign_check:
        
        bne s2,s3,sign_diff
        addi t2,s2,0 #t2 = result_sign = sign_a
        add t3,s6,s7 #t3 = result_mantissa = man_a + man_b
        
        
        
        andi t0,t3,0x100
        
        beqz t0,return_result
        srli t3,t3,1
        
        addi t1,t1,1
        
        addi t0,t1,-0xff
        
        bltz t0,return_result
        slli t2,t2,15
        li t0,0x7F80
        or t2,t2,t0
        addi a0,t2,0
        j restore_regs
        
        sign_diff:
        sub t0,s6,s7
        bltz t0,manb_greater
        addi t2,s2,0 #t2 = result_sign = sign_a
        sub t3,s6,s7 #t3 = result_mantissa = mana-manb
        j check_result
        
        manb_greater:
        
        addi t2,s3,0 #t2 = result_sign = sign_b
        sub t3,s7,s6 #t3 = result_man = manb-mana    
        
        check_result:
        
        beqz t3,bf16_zero
        
        normalize_loop:
            andi t0,t3,0x80
            bnez t0,return_result
            slli t3,t3,1
            addi t1,t1,-1
            
            blez t1,bf16_zero
            j normalize_loop
        
        bf16_zero:
            li a0,0x0000
            j restore_regs
        
        
        return_result:
        slli t2,t2,15
        andi t1,t1,0xFF
        slli t1,t1,7
        andi t3,t3,0x7F
        
        or a0,t1,t2
        or a0,a0,t3
        j restore_regs
        
        
        restore_regs:
        lw s2,0(sp)
        lw s3,4(sp)
        lw s4,8(sp)
        lw s5,12(sp)
        lw s6,16(sp)
        lw s7,20(sp)
        addi sp,sp,24
        ret
#----------------------------------------       
bf16_sub:

    li t0,0x8000
    
    xor a1, a1, t0 # b.bits ^= 0x8000

    # 呼叫 bf16_add(a, b)
    j bf16_add

#----------------------------------------
bf16_mul:
    
    # s2 = sign_a
    # s3 = sign_b
    # s4 = exp_a
    # s5 = exp_b
    # s6 = mant_a
    # s7 = mant_b
    
    addi sp, sp, -24
    sw   s2, 0(sp)
    sw   s3, 4(sp)
    sw   s4, 8(sp)
    sw   s5, 12(sp)
    sw   s6, 16(sp)
    sw   s7, 20(sp)
    
    #------------ extract a
    addi   t0, a0,0
    srli t0, t0, 15
    andi s2, t0, 1          # s2 = sign_a

    addi   t0, a0,0
    srli t0, t0, 7
    andi s4, t0, 0xFF       # s4 = exp_a

    addi   t0, a0,0
    andi s6, t0, 0x7F       # s6 = mant_a
    #------------ extract b
    
    addi t0, a1,0
    srli t0, t0, 15
    andi s3, t0, 1          # s3 = sign_b

    addi   t0, a1,0
    srli t0, t0, 7
    andi s5, t0, 0xFF       # s5 = exp_b

    addi   t0, a1,0
    andi s7, t0, 0x7F       # s7 = mant_b
    
    xor t1,s2,s3 #result_sign = signa xor signb
    
    li t0,0xff
    beq t0,s4,mul_a_inf
    beq t0,s5,mul_b_inf
    seqz t2,s4
    seqz t3,s6
    seqz t4,s5
    seqz t5,s7
    
    and t2,t2,t3
    and t3,t4,t5
    or t0,t2,t3
    
    beqz t0,mul_process
    
    slli t1,t1,15
    addi a0,t1,0
    j restore_regs
    
    
    
    mul_a_inf:
        
        beqz s6,check_expbmantb
        j restore_regs
        
        check_expbmantb:
        seqz t2,s5
        seqz t3,s7
        and t0,t2,t3
        bnez t0,mul_return_nan
        slli t1,t1,15
        li t0,0x7f80
        or a0,t1,t0
        j restore_regs
        
        
        mul_return_nan:
            li a0,0x7fc0
            j restore_regs
    
    
    mul_b_inf:
        beqz s7,check_expamanta
        addi a0,a1,0
        j restore_regs
        
        
        check_expamanta:
        seqz t2,s4
        seqz t3,s6
        and t0,t2,t3
        bnez t0,mul_return_nan
        slli t1,t1,15
        li t0,0x7f80
        or a0,t1,t0
        j restore_regs
        
    mul_process:
        li t2,0 #exp_adjust
        beqz s4,mul_normalized_a
        ori s6,s6,0x80
        j mul_a_done
        
    mul_normalized_a:
        andi t0,s6,0x80
        bnez t0,mul_norm_a_done
        slli s6,s6,1
        addi t2,t2,-1
        j mul_normalized_a
        
        
        mul_norm_a_done:
        li s4,1
        
    
    mul_a_done:
        beqz s5,mul_normalized_b
        ori s7,s7,0x80
        j mul_b_done
    mul_normalized_b:
        andi t0,s7,0x80
        bnez t0,mul_norm_b_done
        slli s7,s7,1
        addi t2,t2,-1
        j mul_normalized_b
    
        mul_norm_b_done:
        li s5,1
    
    mul_b_done:
    
    add  t3, s4, s5        # exp_a + exp_b
    add  t3, t3, t2        # + exp_adjust
    addi t2, t3, -127      # - bias #t2=result_exp t1=result_sign
    
    #mantissa multiplyer
    li   t3,0
    addi t4,s6,0
    addi t5,s7,0
    li   t6,8
    mul_loop:
      andi t0,t5,1      # t0 = (t5 & 1)
      neg  t0,t0        # t0 = 0 or -1 
      and  t0,t0,t4     # t0 = (t5&1)? t4 : 0
      add  t3,t3,t0     # t3 += t0
      slli t4,t4,1
      srli t5,t5,1
      addi t6,t6,-1
      bnez t6,mul_loop
    
    #check result mantissa
    
    li t0,0x8000
    and t0,t0,t3
    
    beqz t0,result_mant_zero
    
    srli t0,t3,8
    andi t3,t0,0x7F
    addi t2,t2,1
    j check_mantissa_done
    
    result_mant_zero:
    srli t0,t3,7
    andi t3,t0,0x7f
    
    check_mantissa_done:
    
    #check result exp
    
    # check overflow
    li   t0, 0xff
    bge  t2, t0, mul_overflow   # if result_exp >= 255 → INF

    # check underflow
    blez t2, mul_underflow      # if result_exp <= 0 → handle underflow
    j    mul_done               # else → normal

    mul_overflow:
    slli t1, t1, 15             # sign
    li   t0, 0x7f80             # INF exponent
    or   a0, t1, t0
    j    restore_regs

    mul_underflow:
    li   t0, -6
    blt  t2, t0, mul_to_zero    # if result_exp < -6 → return 0

    # else: shift mantissa >> (1 - exp)
    li   t0, 1
    sub  t0, t0, t2             # t0 = 1 - result_exp
    srl  t3, t3, t0
    li   t2, 0                  # exp = 0
    j    mul_done

    mul_to_zero:
    slli t1, t1, 15             # just sign
    addi a0, t1, 0              # return ±0
    j    restore_regs

    mul_done:
    slli t1, t1, 15             # sign
    andi t2, t2, 0xff           # exp
    slli t2, t2, 7
    andi t3, t3, 0x7f           # mant
    or   t1, t1, t2
    or   t1, t1, t3
    addi a0, t1, 0
    j    restore_regs

#-----------------------------
bf16_div:
    addi sp, sp, -24
    sw   s2, 0(sp)
    sw   s3, 4(sp)
    sw   s4, 8(sp)
    sw   s5, 12(sp)
    sw   s6, 16(sp)
    sw   s7, 20(sp)

    #--------------------------------
    # Extract fields
    #--------------------------------
    srli t0, a0, 15
    andi s2, t0, 1          # s2 = sign_a

    srli t0, a1, 15
    andi s3, t0, 1          # s3 = sign_b

    srli t0, a0, 7
    andi s4, t0, 0xFF       # s4 = exp_a

    srli t0, a1, 7
    andi s5, t0, 0xFF       # s5 = exp_b

    andi s6, a0, 0x7F       # mant_a
    andi s7, a1, 0x7F       # mant_b
    
    

    xor  t1, s2, s3         # result_sign = sign_a ^ sign_b

    #--------------------------------
    # Handle special cases
    #--------------------------------
    li t0, 0xFF
    beq s5, t0, div_b_inf_or_nan   # b = Inf or NaN
    beqz s5, div_b_zero_check      # b = subnormal or zero?
    j   div_check_a                # else continue


    div_b_inf_or_nan:
    bnez s7, div_return_nan       
    # mant_b == 0 → b 是 Inf
    li t0, 0xFF
    bne s4, t0, div_return_zero 
    bnez s6, div_return_nan     
    # a 是 Inf, b 是 Inf → NaN
    j div_return_nan


    div_b_zero_check:
    beqz s7, div_b_is_zero
    j   div_check_a

    div_b_is_zero:

    beqz s4, div_a_zero_check_for_bzero
    j   div_by_zero

    div_a_zero_check_for_bzero:
    beqz s6, div_return_nan        # 0 / 0 → NaN
    j   div_by_zero             

 
    div_by_zero:
    li   t0, 0x7F80                # Inf exponent
    slli t1, t1, 15
    or   a0, t0, t1
    j    restore_regs


    div_return_zero:
    slli t1, t1, 15
    addi a0, t1, 0
    j    restore_regs

    div_return_nan:
    li   a0, 0x7FC0
    j    restore_regs


    div_check_a:
    li t0, 0xFF
    beq s4, t0, div_a_inf_check
    beqz s4, div_a_zero_check
    j div_process

    div_a_inf_check:
    beqz s6, div_return_inf
    j div_return_nan

    div_a_zero_check:
    beqz s6, div_return_zero
    j div_process

    div_return_inf:
    li t0, 0x7F80
    slli t1, t1, 15
    or a0, t1, t0
    j restore_regs

#--------------------------------
# Main process (normal / subnormal)
#--------------------------------
    div_process:
    # add implicit 1
    beqz s4, div_norm_a_done
    ori s6, s6, 0x80
    div_norm_a_done:

    beqz s5, div_norm_b_done
    ori s7, s7, 0x80
    div_norm_b_done:

    # exp = exp_a - exp_b + bias
    sub t2, s4, s5
    addi t2, t2, 127          # t2 = result_exp
    #divider
    
    slli t4,s6,15 #dividend
    addi t5,s7,0  #divisor
    
    li t3,0 # quotient
    li t6,0 #loop_count
    div_loop:
        slli t3,t3,1
        li t0,15
        sub t0,t0,t6
        sll t0,t5,t0
        blt t4,t0,div_skip
        sub t4,t4,t0
        ori t3,t3,1
        
    div_skip:
        addi t6,t6,1
        li t0,16
        blt t6,t0,div_loop
        
    #div_end
    # exp_a == 0 → exp_adjust--
    beqz s4, div_exp_a_zero
    j div_exp_a_done
    div_exp_a_zero:
    addi t2, t2, -1
    div_exp_a_done:

    # exp_b == 0 → exp_adjust++
    beqz s5, div_exp_b_zero
    j div_exp_b_done
    div_exp_b_zero:
    addi t2, t2, 1
    div_exp_b_done:
    
    #mantissa normalization
    li t0, 0x8000
    and t0, t0, t3           
    bnez t0,div_msb_one

    div_shift_check:
    li   t0, 0x8000
    and  t0, t3, t0       # t0 = (t3 & 0x8000)
    bnez t0, div_norm_done      # MSB==1 done

    addi t0, t2, -1       # t0 = t2 - 1
    blez t0, div_norm_done      # t0<1 done

    slli t3, t3, 1        # quotient <<= 1
    addi t2, t2, -1       # result_exp--
    j    div_shift_check

    div_norm_done:

    srli t3, t3, 8        # quotient >>= 8
    j div_done
    
    div_msb_one:
    srli t3,t3,8
    j div_done
    
    div_done:   
         
    andi t3, t3, 0x7F           # mantissa & 0x7F

    # --- check overflow (result_exp >= 0xFF) ---
    li   t0, 0xFF
    bge  t2, t0, div_overflow   # if exp >= 255 → INF

    # --- check underflow (result_exp <= 0) ---
    blez t2, div_underflow      # if exp <= 0 → ZERO

    # --- normal case ---
    slli t1, t1, 15             # sign << 15
    andi t2, t2, 0xFF
    slli t2, t2, 7              # exp << 7
    or   t1, t1, t2
    or   t1, t1, t3
    addi a0, t1, 0
    j    restore_regs

    # --- overflow (INF) ---
    div_overflow:
    slli t1, t1, 15
    li   t0, 0x7F80
    or   a0, t1, t0
    j    restore_regs

    # --- underflow (ZERO) ---
    div_underflow:
    slli t1, t1, 15
    addi a0, t1, 0
    j restore_regs
#------------------------------
    
bf16_sqrt:
    # s2 = sign_a
    # s3 = exp_a
    # s4 = mant_a
    # s5 = result_sign
    # s6 = result_exp
    # s7 = result_mant

    addi sp, sp, -24
    sw   s2, 0(sp)
    sw   s3, 4(sp)
    sw   s4, 8(sp)
    sw   s5, 12(sp)
    sw   s6, 16(sp)
    sw   s7, 20(sp)

    #------------ extract a (bf16 in a0)
    srli t0, a0, 15
    andi s2, t0, 1          # s2 = sign_a

    srli t0, a0, 7
    andi s3, t0, 0xFF       # s3 = exp_a

    andi s4, a0, 0x7F       # s4 = mant_a

    # default sign=0
    li   s5, 0

    #=====================================
    # Special cases
    #=====================================

    # exp == 0xFF → Inf or NaN
    li   t0, 0xFF
    beq  s3, t0, sqrt_is_inf_or_nan

    # exp == 0 → zero or subnormal
    beqz s3, sqrt_is_zero_or_sub

    # negative (normal, non-zero) → NaN
    bnez s2, sqrt_neg_input

    # others go main process
    j    sqrt_process


# ---------- exp==0xFF ----------
    sqrt_is_inf_or_nan:
    # mant != 0  NaN
    bnez s4, sqrt_return_input_nan

    # mant == 0 → Inf
    # −Inf → NaN；+Inf → +Inf
    bnez s2, sqrt_return_qnan
    li   a0, 0x7F80           # +Inf
    j    restore_regs

    sqrt_return_input_nan:
    addi a0, a0, 0            
    j    restore_regs

    sqrt_return_qnan:
    li   a0, 0x7FC0           # quiet NaN
    j    restore_regs


    # ---------- exp==0 ----------
    sqrt_is_zero_or_sub:
    # mant == 0 → ±0
    beqz s4, sqrt_return_same_zero

    li   a0, 0x0000
    j    restore_regs

    sqrt_return_same_zero:
    li a0, 0x0000
    j    restore_regs


    # ---------- negative (normal, non-zero) ----------
    sqrt_neg_input:
    li   a0, 0x7FC0           # NaN
    j    restore_regs
    
    sqrt_process:
    addi t2,s3,-127 #t2= exp-bias
        
    ori t1,s4,0x80 # get full mantissa
    
    andi t0,t2,1
    
    beqz t0,even_exp
    slli t1,t1,1
    addi t0,t2,-1
    srai t0,t0,1
    addi t0,t0,127
    mv s6,t0 #s6 = new_exp
    j sqrt_adjust_done
    
    even_exp:
    srai t0,t2,1
    addi t0,t0,127
    mv s6,t0 #s6 = new_exp
    
    sqrt_adjust_done:

    li   t2, 127              # low
    li   t3, 256              # high
    li   t4, 128              # result 

    sqrt_binary_search_loop:
    # while (low <= high)
    blt  t3, t2, sqrt_binary_search_end

    # mid = (low + high) >> 1
    add  t0, t2, t3
    srli t0, t0, 1            # t0 = mid

    # sq = (mid * mid) >> 7  
    li   t5, 0             
    mv   t6, t0               # counter = mid
    sqrt_mul_loop:
    beqz t6, sqrt_mul_done
    add  t5, t5, t0           # t5 += mid
    addi t6, t6, -1
    j    sqrt_mul_loop
    sqrt_mul_done:
    srli t5, t5, 7            # sq = (mid*mid)/128

    # if (sq <= m) { result=mid; low=mid+1; } else { high=mid-1; }
    blt  t1, t5, sqrt_go_hi   # if m < sq → high = mid - 1
    mv   t4, t0               # result = mid
    addi t2, t0, 1            # low = mid + 1
    j    sqrt_binary_search_loop
    sqrt_go_hi:
    addi t3, t0, -1           # high = mid - 1
    j    sqrt_binary_search_loop

    sqrt_binary_search_end:

    li   t0, 256
    blt  t4, t0, sqrt_check_low
    # result >= 256 exp++
    srli t4, t4, 1
    addi s6, s6, 1

    sqrt_check_low:
    li   t0, 128
    bge  t4, t0, sqrt_norm_done
    sqrt_norm_loop:
    slli t4, t4, 1
    addi s6, s6, -1
    blt  t4, t0, sqrt_norm_loop

    sqrt_norm_done:
    andi s7, t4, 0x7F

    j    pack_result

    pack_result:
    # new_mant = result & 0x7F
    andi s7, t4, 0x7F


    # return (sign<<15) | (new_exp<<7) | new_mant;

    slli t1, s5, 15          # sign
    andi t2, s6, 0xFF
    slli t2, t2, 7           # !!! exp << 7（
    andi t3, s7, 0x7F        # mant
    or   t1, t1, t2
    or   a0, t1, t3
    j    restore_regs

    
    
