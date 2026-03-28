
u256_ct.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <u256_add_ct>:
   0:	f3 0f 1e fa          	endbr64
   4:	31 c9                	xor    %ecx,%ecx
   6:	31 c0                	xor    %eax,%eax
   8:	4c 8b 0c 0e          	mov    (%rsi,%rcx,1),%r9
   c:	4c 8b 04 0a          	mov    (%rdx,%rcx,1),%r8
  10:	45 31 d2             	xor    %r10d,%r10d
  13:	4d 01 c8             	add    %r9,%r8
  16:	49 01 c0             	add    %rax,%r8
  19:	4d 39 c1             	cmp    %r8,%r9
  1c:	4c 89 04 0f          	mov    %r8,(%rdi,%rcx,1)
  20:	41 0f 94 c2          	sete   %r10b
  24:	4c 21 d0             	and    %r10,%rax
  27:	4d 39 c8             	cmp    %r9,%r8
  2a:	41 0f 92 c1          	setb   %r9b
  2e:	48 83 c1 08          	add    $0x8,%rcx
  32:	45 0f b6 c9          	movzbl %r9b,%r9d
  36:	4c 09 c8             	or     %r9,%rax
  39:	48 83 f9 20          	cmp    $0x20,%rcx
  3d:	75 c9                	jne    8 <u256_add_ct+0x8>
  3f:	c3                   	ret

0000000000000040 <u256_sub_ct>:
  40:	f3 0f 1e fa          	endbr64
  44:	49 89 fb             	mov    %rdi,%r11
  47:	31 c9                	xor    %ecx,%ecx
  49:	31 c0                	xor    %eax,%eax
  4b:	4c 8b 0c 0e          	mov    (%rsi,%rcx,1),%r9
  4f:	4c 8b 04 0a          	mov    (%rdx,%rcx,1),%r8
  53:	45 31 d2             	xor    %r10d,%r10d
  56:	4c 89 cf             	mov    %r9,%rdi
  59:	48 29 c7             	sub    %rax,%rdi
  5c:	4c 29 c7             	sub    %r8,%rdi
  5f:	4d 39 c1             	cmp    %r8,%r9
  62:	41 0f 94 c2          	sete   %r10b
  66:	49 89 3c 0b          	mov    %rdi,(%r11,%rcx,1)
  6a:	4c 21 d0             	and    %r10,%rax
  6d:	4d 39 c1             	cmp    %r8,%r9
  70:	41 0f 92 c0          	setb   %r8b
  74:	48 83 c1 08          	add    $0x8,%rcx
  78:	45 0f b6 c0          	movzbl %r8b,%r8d
  7c:	4c 09 c0             	or     %r8,%rax
  7f:	48 83 f9 20          	cmp    $0x20,%rcx
  83:	75 c6                	jne    4b <u256_sub_ct+0xb>
  85:	c3                   	ret
  86:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  8d:	00 00 00 

0000000000000090 <u256_select_ct>:
  90:	f3 0f 1e fa          	endbr64
  94:	49 89 f2             	mov    %rsi,%r10
  97:	49 89 f9             	mov    %rdi,%r9
  9a:	48 83 ee 01          	sub    $0x1,%rsi
  9e:	31 c0                	xor    %eax,%eax
  a0:	49 f7 da             	neg    %r10
  a3:	48 8b 3c 02          	mov    (%rdx,%rax,1),%rdi
  a7:	4c 8b 04 01          	mov    (%rcx,%rax,1),%r8
  ab:	4c 21 d7             	and    %r10,%rdi
  ae:	49 21 f0             	and    %rsi,%r8
  b1:	4c 09 c7             	or     %r8,%rdi
  b4:	49 89 3c 01          	mov    %rdi,(%r9,%rax,1)
  b8:	48 83 c0 08          	add    $0x8,%rax
  bc:	48 83 f8 20          	cmp    $0x20,%rax
  c0:	75 e1                	jne    a3 <u256_select_ct+0x13>
  c2:	c3                   	ret
  c3:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  ca:	00 00 00 00 
  ce:	66 90                	xchg   %ax,%ax

00000000000000d0 <u256_eq_ct>:
  d0:	f3 0f 1e fa          	endbr64
  d4:	f3 0f 6f 07          	movdqu (%rdi),%xmm0
  d8:	f3 0f 6f 1e          	movdqu (%rsi),%xmm3
  dc:	f3 0f 6f 4e 10       	movdqu 0x10(%rsi),%xmm1
  e1:	f3 0f 6f 57 10       	movdqu 0x10(%rdi),%xmm2
  e6:	66 0f ef c3          	pxor   %xmm3,%xmm0
  ea:	66 0f ef ca          	pxor   %xmm2,%xmm1
  ee:	66 0f eb c1          	por    %xmm1,%xmm0
  f2:	66 0f 6f c8          	movdqa %xmm0,%xmm1
  f6:	66 0f 73 d9 08       	psrldq $0x8,%xmm1
  fb:	66 0f eb c1          	por    %xmm1,%xmm0
  ff:	66 48 0f 7e c2       	movq   %xmm0,%rdx
 104:	48 89 d0             	mov    %rdx,%rax
 107:	48 f7 d8             	neg    %rax
 10a:	48 09 d0             	or     %rdx,%rax
 10d:	48 c1 f8 3f          	sar    $0x3f,%rax
 111:	48 83 c0 01          	add    $0x1,%rax
 115:	c3                   	ret
 116:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
 11d:	00 00 00 

0000000000000120 <u256_modadd_ct>:
 120:	f3 0f 1e fa          	endbr64
 124:	55                   	push   %rbp
 125:	49 89 d1             	mov    %rdx,%r9
 128:	49 89 cb             	mov    %rcx,%r11
 12b:	31 d2                	xor    %edx,%edx
 12d:	53                   	push   %rbx
 12e:	48 89 f3             	mov    %rsi,%rbx
 131:	48 83 ec 58          	sub    $0x58,%rsp
 135:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
 13c:	00 00 
 13e:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
 143:	31 c0                	xor    %eax,%eax
 145:	49 89 e2             	mov    %rsp,%r10
 148:	4c 8b 04 13          	mov    (%rbx,%rdx,1),%r8
 14c:	49 8b 0c 11          	mov    (%r9,%rdx,1),%rcx
 150:	31 f6                	xor    %esi,%esi
 152:	4c 01 c1             	add    %r8,%rcx
 155:	48 01 c1             	add    %rax,%rcx
 158:	49 39 c8             	cmp    %rcx,%r8
 15b:	49 89 0c 12          	mov    %rcx,(%r10,%rdx,1)
 15f:	40 0f 94 c6          	sete   %sil
 163:	48 21 c6             	and    %rax,%rsi
 166:	31 c0                	xor    %eax,%eax
 168:	4c 39 c1             	cmp    %r8,%rcx
 16b:	0f 92 c0             	setb   %al
 16e:	48 83 c2 08          	add    $0x8,%rdx
 172:	48 09 f0             	or     %rsi,%rax
 175:	48 83 fa 20          	cmp    $0x20,%rdx
 179:	75 cd                	jne    148 <u256_modadd_ct+0x28>
 17b:	31 d2                	xor    %edx,%edx
 17d:	31 c9                	xor    %ecx,%ecx
 17f:	48 8d 6c 24 20       	lea    0x20(%rsp),%rbp
 184:	4d 8b 0c 12          	mov    (%r10,%rdx,1),%r9
 188:	4d 8b 04 13          	mov    (%r11,%rdx,1),%r8
 18c:	31 db                	xor    %ebx,%ebx
 18e:	4c 89 ce             	mov    %r9,%rsi
 191:	48 29 ce             	sub    %rcx,%rsi
 194:	4c 29 c6             	sub    %r8,%rsi
 197:	4d 39 c1             	cmp    %r8,%r9
 19a:	0f 94 c3             	sete   %bl
 19d:	48 89 74 15 00       	mov    %rsi,0x0(%rbp,%rdx,1)
 1a2:	48 21 d9             	and    %rbx,%rcx
 1a5:	4d 39 c1             	cmp    %r8,%r9
 1a8:	41 0f 92 c0          	setb   %r8b
 1ac:	48 83 c2 08          	add    $0x8,%rdx
 1b0:	45 0f b6 c0          	movzbl %r8b,%r8d
 1b4:	4c 09 c1             	or     %r8,%rcx
 1b7:	48 83 fa 20          	cmp    $0x20,%rdx
 1bb:	75 c7                	jne    184 <u256_modadd_ct+0x64>
 1bd:	48 83 f0 01          	xor    $0x1,%rax
 1c1:	66 0f 6f 14 24       	movdqa (%rsp),%xmm2
 1c6:	66 0f 6f 5c 24 20    	movdqa 0x20(%rsp),%xmm3
 1cc:	48 21 c8             	and    %rcx,%rax
 1cf:	48 89 c3             	mov    %rax,%rbx
 1d2:	48 83 e8 01          	sub    $0x1,%rax
 1d6:	48 f7 db             	neg    %rbx
 1d9:	66 48 0f 6e c0       	movq   %rax,%xmm0
 1de:	66 48 0f 6e cb       	movq   %rbx,%xmm1
 1e3:	66 0f 6c c0          	punpcklqdq %xmm0,%xmm0
 1e7:	66 0f 6c c9          	punpcklqdq %xmm1,%xmm1
 1eb:	66 0f db d8          	pand   %xmm0,%xmm3
 1ef:	66 0f db 44 24 30    	pand   0x30(%rsp),%xmm0
 1f5:	66 0f db d1          	pand   %xmm1,%xmm2
 1f9:	66 0f db 4c 24 10    	pand   0x10(%rsp),%xmm1
 1ff:	66 0f eb d3          	por    %xmm3,%xmm2
 203:	66 0f eb c1          	por    %xmm1,%xmm0
 207:	0f 11 17             	movups %xmm2,(%rdi)
 20a:	0f 11 47 10          	movups %xmm0,0x10(%rdi)
 20e:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
 213:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
 21a:	00 00 
 21c:	75 07                	jne    225 <u256_modadd_ct+0x105>
 21e:	48 83 c4 58          	add    $0x58,%rsp
 222:	5b                   	pop    %rbx
 223:	5d                   	pop    %rbp
 224:	c3                   	ret
 225:	e8 00 00 00 00       	call   22a <u256_modadd_ct+0x10a>
 22a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000000230 <u256_modsub_ct>:
 230:	f3 0f 1e fa          	endbr64
 234:	55                   	push   %rbp
 235:	49 89 f8             	mov    %rdi,%r8
 238:	48 89 d5             	mov    %rdx,%rbp
 23b:	49 89 ca             	mov    %rcx,%r10
 23e:	53                   	push   %rbx
 23f:	31 d2                	xor    %edx,%edx
 241:	48 89 f3             	mov    %rsi,%rbx
 244:	48 83 ec 58          	sub    $0x58,%rsp
 248:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
 24f:	00 00 
 251:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
 256:	31 c0                	xor    %eax,%eax
 258:	49 89 e1             	mov    %rsp,%r9
 25b:	4c 8b 1c 13          	mov    (%rbx,%rdx,1),%r11
 25f:	48 8b 7c 15 00       	mov    0x0(%rbp,%rdx,1),%rdi
 264:	31 f6                	xor    %esi,%esi
 266:	4c 89 d9             	mov    %r11,%rcx
 269:	48 29 c1             	sub    %rax,%rcx
 26c:	48 29 f9             	sub    %rdi,%rcx
 26f:	49 39 fb             	cmp    %rdi,%r11
 272:	40 0f 94 c6          	sete   %sil
 276:	49 89 0c 11          	mov    %rcx,(%r9,%rdx,1)
 27a:	48 21 c6             	and    %rax,%rsi
 27d:	31 c0                	xor    %eax,%eax
 27f:	49 39 fb             	cmp    %rdi,%r11
 282:	0f 92 c0             	setb   %al
 285:	48 83 c2 08          	add    $0x8,%rdx
 289:	48 09 f0             	or     %rsi,%rax
 28c:	48 83 fa 20          	cmp    $0x20,%rdx
 290:	75 c9                	jne    25b <u256_modsub_ct+0x2b>
 292:	31 d2                	xor    %edx,%edx
 294:	31 f6                	xor    %esi,%esi
 296:	48 8d 5c 24 20       	lea    0x20(%rsp),%rbx
 29b:	49 8b 3c 11          	mov    (%r9,%rdx,1),%rdi
 29f:	49 8b 0c 12          	mov    (%r10,%rdx,1),%rcx
 2a3:	45 31 db             	xor    %r11d,%r11d
 2a6:	48 01 f9             	add    %rdi,%rcx
 2a9:	48 01 f1             	add    %rsi,%rcx
 2ac:	48 39 cf             	cmp    %rcx,%rdi
 2af:	48 89 0c 13          	mov    %rcx,(%rbx,%rdx,1)
 2b3:	41 0f 94 c3          	sete   %r11b
 2b7:	4c 21 de             	and    %r11,%rsi
 2ba:	48 39 f9             	cmp    %rdi,%rcx
 2bd:	40 0f 92 c7          	setb   %dil
 2c1:	48 83 c2 08          	add    $0x8,%rdx
 2c5:	40 0f b6 ff          	movzbl %dil,%edi
 2c9:	48 09 fe             	or     %rdi,%rsi
 2cc:	48 83 fa 20          	cmp    $0x20,%rdx
 2d0:	75 c9                	jne    29b <u256_modsub_ct+0x6b>
 2d2:	48 89 c3             	mov    %rax,%rbx
 2d5:	48 83 e8 01          	sub    $0x1,%rax
 2d9:	66 0f 6f 54 24 20    	movdqa 0x20(%rsp),%xmm2
 2df:	66 0f 6f 1c 24       	movdqa (%rsp),%xmm3
 2e4:	48 f7 db             	neg    %rbx
 2e7:	66 48 0f 6e c0       	movq   %rax,%xmm0
 2ec:	66 48 0f 6e cb       	movq   %rbx,%xmm1
 2f1:	66 0f 6c c0          	punpcklqdq %xmm0,%xmm0
 2f5:	66 0f 6c c9          	punpcklqdq %xmm1,%xmm1
 2f9:	66 0f db d8          	pand   %xmm0,%xmm3
 2fd:	66 0f db 44 24 10    	pand   0x10(%rsp),%xmm0
 303:	66 0f db d1          	pand   %xmm1,%xmm2
 307:	66 0f db 4c 24 30    	pand   0x30(%rsp),%xmm1
 30d:	66 0f eb d3          	por    %xmm3,%xmm2
 311:	66 0f eb c1          	por    %xmm1,%xmm0
 315:	41 0f 11 10          	movups %xmm2,(%r8)
 319:	41 0f 11 40 10       	movups %xmm0,0x10(%r8)
 31e:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
 323:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
 32a:	00 00 
 32c:	75 07                	jne    335 <u256_modsub_ct+0x105>
 32e:	48 83 c4 58          	add    $0x58,%rsp
 332:	5b                   	pop    %rbx
 333:	5d                   	pop    %rbp
 334:	c3                   	ret
 335:	e8 00 00 00 00       	call   33a <u256_modsub_ct+0x10a>
 33a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000000340 <u256_mont_mul_ct>:
 340:	f3 0f 1e fa          	endbr64
 344:	41 57                	push   %r15
 346:	66 0f ef c0          	pxor   %xmm0,%xmm0
 34a:	49 89 c9             	mov    %rcx,%r9
 34d:	49 89 f2             	mov    %rsi,%r10
 350:	41 56                	push   %r14
 352:	4d 89 c6             	mov    %r8,%r14
 355:	41 55                	push   %r13
 357:	41 54                	push   %r12
 359:	55                   	push   %rbp
 35a:	53                   	push   %rbx
 35b:	48 83 ec 78          	sub    $0x78,%rsp
 35f:	4c 8b 39             	mov    (%rcx),%r15
 362:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
 367:	4c 8d 44 24 40       	lea    0x40(%rsp),%r8
 36c:	48 8d 6c 24 38       	lea    0x38(%rsp),%rbp
 371:	48 8d 5c 24 60       	lea    0x60(%rsp),%rbx
 376:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
 37d:	00 00 
 37f:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
 384:	31 c0                	xor    %eax,%eax
 386:	48 8d 46 20          	lea    0x20(%rsi),%rax
 38a:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
 38f:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
 396:	00 00 
 398:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
 39d:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
 3a2:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
 3a7:	4d 8b 1a             	mov    (%r10),%r11
 3aa:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
 3af:	4c 89 c1             	mov    %r8,%rcx
 3b2:	31 f6                	xor    %esi,%esi
 3b4:	4c 89 d8             	mov    %r11,%rax
 3b7:	48 f7 27             	mulq   (%rdi)
 3ba:	49 89 c4             	mov    %rax,%r12
 3bd:	49 89 d5             	mov    %rdx,%r13
 3c0:	4c 03 21             	add    (%rcx),%r12
 3c3:	49 83 d5 00          	adc    $0x0,%r13
 3c7:	49 01 f4             	add    %rsi,%r12
 3ca:	49 83 d5 00          	adc    $0x0,%r13
 3ce:	4c 89 21             	mov    %r12,(%rcx)
 3d1:	48 83 c1 08          	add    $0x8,%rcx
 3d5:	48 83 c7 08          	add    $0x8,%rdi
 3d9:	4d 89 ec             	mov    %r13,%r12
 3dc:	45 31 ed             	xor    %r13d,%r13d
 3df:	4c 89 e6             	mov    %r12,%rsi
 3e2:	48 39 cb             	cmp    %rcx,%rbx
 3e5:	75 cd                	jne    3b4 <u256_mont_mul_ct+0x74>
 3e7:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
 3ec:	48 89 cf             	mov    %rcx,%rdi
 3ef:	49 0f af fe          	imul   %r14,%rdi
 3f3:	48 89 f8             	mov    %rdi,%rax
 3f6:	49 f7 e7             	mul    %r15
 3f9:	48 01 c8             	add    %rcx,%rax
 3fc:	b9 08 00 00 00       	mov    $0x8,%ecx
 401:	48 83 d2 00          	adc    $0x0,%rdx
 405:	49 89 d3             	mov    %rdx,%r11
 408:	48 89 f8             	mov    %rdi,%rax
 40b:	49 f7 24 09          	mulq   (%r9,%rcx,1)
 40f:	49 03 04 08          	add    (%r8,%rcx,1),%rax
 413:	48 83 d2 00          	adc    $0x0,%rdx
 417:	4c 01 d8             	add    %r11,%rax
 41a:	48 83 d2 00          	adc    $0x0,%rdx
 41e:	48 89 44 0d 00       	mov    %rax,0x0(%rbp,%rcx,1)
 423:	48 83 c1 08          	add    $0x8,%rcx
 427:	48 89 d0             	mov    %rdx,%rax
 42a:	31 d2                	xor    %edx,%edx
 42c:	49 89 c3             	mov    %rax,%r11
 42f:	48 83 f9 20          	cmp    $0x20,%rcx
 433:	75 d3                	jne    408 <u256_mont_mul_ct+0xc8>
 435:	49 01 f3             	add    %rsi,%r11
 438:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
 43d:	49 83 c2 08          	add    $0x8,%r10
 441:	4c 89 5c 24 58       	mov    %r11,0x58(%rsp)
 446:	49 39 f2             	cmp    %rsi,%r10
 449:	0f 85 58 ff ff ff    	jne    3a7 <u256_mont_mul_ct+0x67>
 44f:	49 01 c4             	add    %rax,%r12
 452:	4c 8b 5c 24 18       	mov    0x18(%rsp),%r11
 457:	48 8d 5c 24 20       	lea    0x20(%rsp),%rbx
 45c:	49 11 d5             	adc    %rdx,%r13
 45f:	31 c9                	xor    %ecx,%ecx
 461:	31 c0                	xor    %eax,%eax
 463:	4c 89 6c 24 60       	mov    %r13,0x60(%rsp)
 468:	4c 89 ea             	mov    %r13,%rdx
 46b:	4d 8b 14 08          	mov    (%r8,%rcx,1),%r10
 46f:	49 8b 3c 09          	mov    (%r9,%rcx,1),%rdi
 473:	31 ed                	xor    %ebp,%ebp
 475:	4c 89 d6             	mov    %r10,%rsi
 478:	48 29 c6             	sub    %rax,%rsi
 47b:	48 29 fe             	sub    %rdi,%rsi
 47e:	49 39 fa             	cmp    %rdi,%r10
 481:	40 0f 94 c5          	sete   %bpl
 485:	48 89 34 0b          	mov    %rsi,(%rbx,%rcx,1)
 489:	48 21 e8             	and    %rbp,%rax
 48c:	49 39 fa             	cmp    %rdi,%r10
 48f:	40 0f 92 c7          	setb   %dil
 493:	48 83 c1 08          	add    $0x8,%rcx
 497:	40 0f b6 ff          	movzbl %dil,%edi
 49b:	48 09 f8             	or     %rdi,%rax
 49e:	48 83 f9 20          	cmp    $0x20,%rcx
 4a2:	75 c7                	jne    46b <u256_mont_mul_ct+0x12b>
 4a4:	48 83 f2 01          	xor    $0x1,%rdx
 4a8:	66 0f 6f 54 24 40    	movdqa 0x40(%rsp),%xmm2
 4ae:	66 0f 6f 5c 24 20    	movdqa 0x20(%rsp),%xmm3
 4b4:	48 21 d0             	and    %rdx,%rax
 4b7:	48 89 c3             	mov    %rax,%rbx
 4ba:	48 83 e8 01          	sub    $0x1,%rax
 4be:	48 f7 db             	neg    %rbx
 4c1:	66 48 0f 6e c0       	movq   %rax,%xmm0
 4c6:	66 48 0f 6e cb       	movq   %rbx,%xmm1
 4cb:	66 0f 6c c0          	punpcklqdq %xmm0,%xmm0
 4cf:	66 0f 6c c9          	punpcklqdq %xmm1,%xmm1
 4d3:	66 0f db d8          	pand   %xmm0,%xmm3
 4d7:	66 0f db 44 24 30    	pand   0x30(%rsp),%xmm0
 4dd:	66 0f db d1          	pand   %xmm1,%xmm2
 4e1:	66 0f db 4c 24 50    	pand   0x50(%rsp),%xmm1
 4e7:	66 0f eb d3          	por    %xmm3,%xmm2
 4eb:	66 0f eb c1          	por    %xmm1,%xmm0
 4ef:	41 0f 11 13          	movups %xmm2,(%r11)
 4f3:	41 0f 11 43 10       	movups %xmm0,0x10(%r11)
 4f8:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
 4fd:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
 504:	00 00 
 506:	75 0f                	jne    517 <u256_mont_mul_ct+0x1d7>
 508:	48 83 c4 78          	add    $0x78,%rsp
 50c:	5b                   	pop    %rbx
 50d:	5d                   	pop    %rbp
 50e:	41 5c                	pop    %r12
 510:	41 5d                	pop    %r13
 512:	41 5e                	pop    %r14
 514:	41 5f                	pop    %r15
 516:	c3                   	ret
 517:	e8 00 00 00 00       	call   51c <u256_mont_mul_ct+0x1dc>
 51c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000000520 <u256_modpow_ct>:
 520:	f3 0f 1e fa          	endbr64
 524:	41 57                	push   %r15
 526:	41 56                	push   %r14
 528:	45 31 f6             	xor    %r14d,%r14d
 52b:	41 55                	push   %r13
 52d:	49 89 d5             	mov    %rdx,%r13
 530:	41 54                	push   %r12
 532:	49 89 cc             	mov    %rcx,%r12
 535:	55                   	push   %rbp
 536:	4c 89 c5             	mov    %r8,%rbp
 539:	53                   	push   %rbx
 53a:	48 81 ec 98 00 00 00 	sub    $0x98,%rsp
 541:	f3 41 0f 6f 21       	movdqu (%r9),%xmm4
 546:	f3 0f 6f 36          	movdqu (%rsi),%xmm6
 54a:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
 54f:	f3 41 0f 6f 69 10    	movdqu 0x10(%r9),%xmm5
 555:	48 8d 5c 24 40       	lea    0x40(%rsp),%rbx
 55a:	f3 0f 6f 7e 10       	movdqu 0x10(%rsi),%xmm7
 55f:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
 566:	00 00 
 568:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
 56f:	00 
 570:	31 c0                	xor    %eax,%eax
 572:	48 8d 44 24 20       	lea    0x20(%rsp),%rax
 577:	0f 29 64 24 20       	movaps %xmm4,0x20(%rsp)
 57c:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
 581:	48 8d 44 24 60       	lea    0x60(%rsp),%rax
 586:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
 58b:	0f 29 6c 24 30       	movaps %xmm5,0x30(%rsp)
 590:	0f 29 74 24 40       	movaps %xmm6,0x40(%rsp)
 595:	0f 29 7c 24 50       	movaps %xmm7,0x50(%rsp)
 59a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
 5a0:	44 89 f0             	mov    %r14d,%eax
 5a3:	44 89 f1             	mov    %r14d,%ecx
 5a6:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
 5ab:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
 5b0:	c1 f8 06             	sar    $0x6,%eax
 5b3:	49 89 e8             	mov    %rbp,%r8
 5b6:	48 89 da             	mov    %rbx,%rdx
 5b9:	41 83 c6 01          	add    $0x1,%r14d
 5bd:	48 98                	cltq
 5bf:	49 8b 44 c5 00       	mov    0x0(%r13,%rax,8),%rax
 5c4:	48 d3 e8             	shr    %cl,%rax
 5c7:	4c 89 e1             	mov    %r12,%rcx
 5ca:	83 e0 01             	and    $0x1,%eax
 5cd:	49 89 c7             	mov    %rax,%r15
 5d0:	e8 00 00 00 00       	call   5d5 <u256_modpow_ct+0xb5>
 5d5:	4c 89 f8             	mov    %r15,%rax
 5d8:	49 89 e8             	mov    %rbp,%r8
 5db:	4c 89 e1             	mov    %r12,%rcx
 5de:	48 f7 d8             	neg    %rax
 5e1:	66 0f 6f 5c 24 20    	movdqa 0x20(%rsp),%xmm3
 5e7:	48 89 da             	mov    %rbx,%rdx
 5ea:	48 89 de             	mov    %rbx,%rsi
 5ed:	66 48 0f 6e c8       	movq   %rax,%xmm1
 5f2:	49 8d 47 ff          	lea    -0x1(%r15),%rax
 5f6:	66 0f 6f 54 24 60    	movdqa 0x60(%rsp),%xmm2
 5fc:	48 89 df             	mov    %rbx,%rdi
 5ff:	66 48 0f 6e c0       	movq   %rax,%xmm0
 604:	66 0f 6c c9          	punpcklqdq %xmm1,%xmm1
 608:	66 0f 6c c0          	punpcklqdq %xmm0,%xmm0
 60c:	66 0f db d1          	pand   %xmm1,%xmm2
 610:	66 0f db 4c 24 70    	pand   0x70(%rsp),%xmm1
 616:	66 0f db d8          	pand   %xmm0,%xmm3
 61a:	66 0f db 44 24 30    	pand   0x30(%rsp),%xmm0
 620:	66 0f eb d3          	por    %xmm3,%xmm2
 624:	66 0f eb c1          	por    %xmm1,%xmm0
 628:	0f 29 54 24 20       	movaps %xmm2,0x20(%rsp)
 62d:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
 632:	e8 00 00 00 00       	call   637 <u256_modpow_ct+0x117>
 637:	41 81 fe 00 01 00 00 	cmp    $0x100,%r14d
 63e:	0f 85 5c ff ff ff    	jne    5a0 <u256_modpow_ct+0x80>
 644:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
 649:	66 0f 6f 64 24 20    	movdqa 0x20(%rsp),%xmm4
 64f:	66 0f 6f 6c 24 30    	movdqa 0x30(%rsp),%xmm5
 655:	0f 11 20             	movups %xmm4,(%rax)
 658:	0f 11 68 10          	movups %xmm5,0x10(%rax)
 65c:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
 663:	00 
 664:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
 66b:	00 00 
 66d:	75 12                	jne    681 <u256_modpow_ct+0x161>
 66f:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
 676:	5b                   	pop    %rbx
 677:	5d                   	pop    %rbp
 678:	41 5c                	pop    %r12
 67a:	41 5d                	pop    %r13
 67c:	41 5e                	pop    %r14
 67e:	41 5f                	pop    %r15
 680:	c3                   	ret
 681:	e8 00 00 00 00       	call   686 <u256_modpow_ct+0x166>
 686:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
 68d:	00 00 00 

0000000000000690 <u256_modinv_ct>:
 690:	f3 0f 1e fa          	endbr64
 694:	41 55                	push   %r13
 696:	49 89 d2             	mov    %rdx,%r10
 699:	49 89 fb             	mov    %rdi,%r11
 69c:	4d 89 c1             	mov    %r8,%r9
 69f:	41 54                	push   %r12
 6a1:	31 d2                	xor    %edx,%edx
 6a3:	49 89 cc             	mov    %rcx,%r12
 6a6:	55                   	push   %rbp
 6a7:	53                   	push   %rbx
 6a8:	48 89 f3             	mov    %rsi,%rbx
 6ab:	48 83 ec 58          	sub    $0x58,%rsp
 6af:	66 0f 6f 05 00 00 00 	movdqa 0x0(%rip),%xmm0        # 6b7 <u256_modinv_ct+0x27>
 6b6:	00 
 6b7:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
 6be:	00 00 
 6c0:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
 6c5:	31 c0                	xor    %eax,%eax
 6c7:	4c 8d 6c 24 20       	lea    0x20(%rsp),%r13
 6cc:	48 89 e5             	mov    %rsp,%rbp
 6cf:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
 6d4:	66 0f ef c0          	pxor   %xmm0,%xmm0
 6d8:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
 6dd:	49 8b 3c 02          	mov    (%r10,%rax,1),%rdi
 6e1:	49 8b 74 05 00       	mov    0x0(%r13,%rax,1),%rsi
 6e6:	45 31 c0             	xor    %r8d,%r8d
 6e9:	48 89 f9             	mov    %rdi,%rcx
 6ec:	48 29 d1             	sub    %rdx,%rcx
 6ef:	48 29 f1             	sub    %rsi,%rcx
 6f2:	48 39 f7             	cmp    %rsi,%rdi
 6f5:	41 0f 94 c0          	sete   %r8b
 6f9:	48 89 4c 05 00       	mov    %rcx,0x0(%rbp,%rax,1)
 6fe:	4c 21 c2             	and    %r8,%rdx
 701:	48 39 f7             	cmp    %rsi,%rdi
 704:	40 0f 92 c6          	setb   %sil
 708:	48 83 c0 08          	add    $0x8,%rax
 70c:	40 0f b6 f6          	movzbl %sil,%esi
 710:	48 09 f2             	or     %rsi,%rdx
 713:	48 83 f8 20          	cmp    $0x20,%rax
 717:	75 c4                	jne    6dd <u256_modinv_ct+0x4d>
 719:	4d 89 e0             	mov    %r12,%r8
 71c:	4c 89 d1             	mov    %r10,%rcx
 71f:	48 89 ea             	mov    %rbp,%rdx
 722:	48 89 de             	mov    %rbx,%rsi
 725:	4c 89 df             	mov    %r11,%rdi
 728:	e8 00 00 00 00       	call   72d <u256_modinv_ct+0x9d>
 72d:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
 732:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
 739:	00 00 
 73b:	75 0b                	jne    748 <u256_modinv_ct+0xb8>
 73d:	48 83 c4 58          	add    $0x58,%rsp
 741:	5b                   	pop    %rbx
 742:	5d                   	pop    %rbp
 743:	41 5c                	pop    %r12
 745:	41 5d                	pop    %r13
 747:	c3                   	ret
 748:	e8 00 00 00 00       	call   74d <u256_modinv_ct+0xbd>
