.intel_syntax noprefix
.data
.align 64
SCALAR:
.double 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0
.align 64
SSCALAR:
.single 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0
.align 64
ISCALAR:
.int 1, 1
.align 16
OMM:
.int 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
.align 16
IOMM:
.int 0,16,32,48,64,80,96,128,144,160,176,192,208,224,240,256
.align 16
TOMM:
.int 0,2,4,6,16,18,20,22,32,34,36,38,48,50,52,54
.text
.globl daxpy_sp
.type daxpy_sp, @function
daxpy_sp :
push rbp
mov rbp, rsp
push rbx
push r12
push r13
push r14
push r15
movss xmm8, [rip+SCALAR]
xor   rax, rax
.align 16
1:
movss xmm0, [rsi + rax*4]
movss xmm1, [rsi + rax*4 + 4]
movss xmm2, [rsi + rax*4 + 8]
movss xmm3, [rsi + rax*4 + 12]
mulss xmm0, xmm8
addss xmm0, [rdx + rax*4]
mulss xmm1, xmm8
addss xmm1, [rdx + rax*4 + 4]
mulss xmm2, xmm8
addss xmm2, [rdx + rax*4 + 8]
mulss xmm3, xmm8
addss xmm3, [rdx + rax*4 + 12]
movss xmm4, [rsi + rax*4 + 16]
movss xmm5, [rsi + rax*4 + 20]
movss xmm6, [rsi + rax*4 + 24]
movss xmm7, [rsi + rax*4 + 28]
mulss xmm4, xmm8
addss xmm4, [rdx + rax*4 + 16]
mulss xmm5, xmm8
addss xmm5, [rdx + rax*4 + 20]
mulss xmm6, xmm8
addss xmm6, [rdx + rax*4 + 24]
mulss xmm7, xmm8
addss xmm7, [rdx + rax*4 + 28]
movss [rdx + rax*4], xmm0
movss [rdx + rax*4 + 4], xmm1
movss [rdx + rax*4 + 8], xmm2
movss [rdx + rax*4 + 12], xmm3
movss [rdx + rax*4 + 16], xmm4
movss [rdx + rax*4 + 20], xmm5
movss [rdx + rax*4 + 24], xmm6
movss [rdx + rax*4 + 28], xmm7
addq rax, 8
cmpq rax, rdi
jl 1b

pop r15
pop r14
pop r13
pop r12
pop rbx
mov  rsp, rbp
pop rbp
ret
.size daxpy_sp, .-daxpy_sp

