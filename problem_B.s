.data
PASS_MSG: .asciz "All tests PASSED!\n"
FAIL_MSG: .asciz "Some tests FAILED!\n"


.text
.globl main



main:
    jal  ra, test           
    mv   t0, a0         

    beqz t0, print_fail  

print_pass:
    la   a0, PASS_MSG
    li   a7, 4              # ecall 4 = print string
    ecall
    j    done

print_fail:
    la   a0, FAIL_MSG
    li   a7, 4
    ecall

done:
    li   a7, 10             # ecall 10 = exit
    ecall


clz:
    li t0,32 #t0 = n
    li t1,16 #t1 = c 
    
    clz_Loop:
        srl t2,a0,t1 #t2 = y
        sgtz t3,t2
        beqz t3,shift
        sub t0,t0,t1
        mv a0,t2
        shift:
        srli t1,t1,1
        sgtz t3,t1
        bnez t3,clz_Loop
        
        sub a0,t0,a0
        ret


uf8_decode:
    andi t1,a0,0x0f #t1 = mantissa
    srli t2,a0,4
    li t3,15
    sub t0,t3,t2
    li t3,0x7fff
    srl t3,t3,t0
    slli t0,t3,4 # t0 = offset
    sll t1,t1,t2
    add a0,t1,t0
    
    ret
uf8_encode:
    addi sp, sp, -24
    sw   s2, 0(sp)
    sw   s3, 4(sp)
    sw   s4, 8(sp)
    sw   s5, 12(sp)
    sw   s6, 16(sp)
    sw   s7, 20(sp)
    
    mv s7,a0 #s7 = value
    
    addi t0,s7,-16
    sltz t0,t0
    
    beqz t0,find_exp
    j restore_regs
    
    find_exp:
    addi sp,sp,-4
    sw ra,0(sp)
    jal ra,clz
    lw ra, 0(sp)
    addi sp,sp,4
    
    mv s2,a0 #s2=clz
    li s3,31
    sub s3,s3,s2 #s3 = msb
    
    li s4,0 #exp=0
    li s5,0 #overflow=0
    
    li t0,5
    blt s3,t0,find_extract_exp
    
    addi s4,s3,-4
    
    addi t0,s4,-15
    
    sgtz t0,t0
    
    beqz t0,end_check
    
    li s4,15
    
    end_check:
        
    li t0,0
        
    check_overflow:
    
    sub t1,t0,s4
    sltz t1,t1
    
    beqz t1,check_overflow_done
    
    slli s5,s5,1
    addi s5,s5,16
    addi t0,t0,1
    
    j check_overflow
    
    check_overflow_done:
    
    adjust:
        sgtz t0,s4
        sub t1,s7,s5
        sltz t1,t1
        and t0,t1,t0
        
        beqz t0,adjust_done
        addi s5,s5,-16
        srli s5,s5,1
        addi s4,s4,-1
        
        j adjust
        adjust_done:
    
    find_extract_exp:
        addi t0,s4,-15
        sltz t0,t0
        beqz t0,extract_done
        slli t0,s5,1
        addi s6,t0,16
        
        sub t0,s7,s6
        sltz t0,t0
        bnez t0,extract_done
        mv s5,s6
        addi s4,s4,1
        j find_extract_exp
        
    extract_done:
    
    sub t0,s7,s5
    srl t0,t0,s4
    
    slli t1,s4,4
    or a0,t1,t0
    j restore_regs
    
    restore_regs:
    lw   s2,  0(sp)
    lw   s3,  4(sp)
    lw   s4,  8(sp)
    lw   s5, 12(sp)
    lw   s6, 16(sp)
    lw   s7, 20(sp)
    addi sp, sp, 24
    ret

test:
    addi sp, sp, -32
    sw   s2, 0(sp)
    sw   s3, 4(sp)
    sw   s4, 8(sp)
    sw   s5, 12(sp)
    sw   s6, 16(sp)
    sw   s7, 20(sp)
    sw   s0, 24(sp)
    sw   ra, 28(sp)
    
    # s2 = i
    # s3 = previous_value
    # s4 = pass flag
    # s5 = decoded value
    # s6 = re-encoded value
    # s7 = pointer to FAIL_RECORD

    li   s2, 0              # i = 0
    li   s3, -1             # previous_value = -1
    li   s4, 1              # passed = true

test_loop:
    li   t0, 256
    beq  s2, t0, test_done

    mv   a0, s2             # a0 = fl = i
    jal  ra, uf8_decode
    mv   s5, a0             # s5 = value

    mv   a0, s5
    jal  ra, uf8_encode
    mv   s6, a0             # s6 = fl2

    # if (fl != fl2)
    bne  s2, s6, record_fail

    # if (value <= previous_value)
    sub  t1, s5, s3
    blez t1, record_fail

    
    
    # update
    mv   s3, s5
    addi s2, s2, 1
    j    test_loop

record_fail:
    sw   s2, 0(s7)          # fl
    sw   s5, 4(s7)          # value
    sw   s6, 8(s7)          # fl2
    li   s4, 0              # passed = false
    addi s7, s7, 12         # next record slot
    addi s2, s2, 1
    j    test_loop

test_done:
    mv   a0, s4             # return pass flag (1 = pass, 0 = fail)

    lw   s2, 0(sp)
    lw   s3, 4(sp)
    lw   s4, 8(sp)
    lw   s5, 12(sp)
    lw   s6, 16(sp)
    lw   s7, 20(sp)
    lw   s0, 24(sp)
    lw   ra, 28(sp)
    addi sp, sp, 32
    ret


