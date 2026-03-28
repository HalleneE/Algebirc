
u256_ct.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <u256_add_ct>:
   0:	f3 0f 1e fa          	endbr64
   4:	48 89 f1             	mov    %rsi,%rcx
   7:	48 89 d6             	mov    %rdx,%rsi
   a:	48 8b 12             	mov    (%rdx),%rdx
   d:	48 89 f8             	mov    %rdi,%rax
  10:	48 03 11             	add    (%rcx),%rdx
  13:	48 89 17             	mov    %rdx,(%rdi)
  16:	4c 8b 41 08          	mov    0x8(%rcx),%r8
  1a:	41 0f 92 c1          	setb   %r9b
  1e:	48 8b 56 08          	mov    0x8(%rsi),%rdx
  22:	41 0f b6 f9          	movzbl %r9b,%edi
  26:	4c 01 c2             	add    %r8,%rdx
  29:	48 01 fa             	add    %rdi,%rdx
  2c:	49 39 d0             	cmp    %rdx,%r8
  2f:	48 89 50 08          	mov    %rdx,0x8(%rax)
  33:	40 0f 94 c7          	sete   %dil
  37:	44 21 cf             	and    %r9d,%edi
  3a:	4c 39 c2             	cmp    %r8,%rdx
  3d:	4c 8b 49 10          	mov    0x10(%rcx),%r9
  41:	41 0f 92 c0          	setb   %r8b
  45:	44 09 c7             	or     %r8d,%edi
  48:	4c 8b 46 10          	mov    0x10(%rsi),%r8
  4c:	40 0f b6 d7          	movzbl %dil,%edx
  50:	4d 01 c8             	add    %r9,%r8
  53:	49 01 d0             	add    %rdx,%r8
  56:	4d 39 c1             	cmp    %r8,%r9
  59:	4c 89 40 10          	mov    %r8,0x10(%rax)
  5d:	0f 94 c2             	sete   %dl
  60:	21 fa                	and    %edi,%edx
  62:	4d 39 c8             	cmp    %r9,%r8
  65:	40 0f 92 c7          	setb   %dil
  69:	09 fa                	or     %edi,%edx
  6b:	48 8b 79 18          	mov    0x18(%rcx),%rdi
  6f:	48 8b 4e 18          	mov    0x18(%rsi),%rcx
  73:	0f b6 f2             	movzbl %dl,%esi
  76:	48 01 f9             	add    %rdi,%rcx
  79:	48 01 f1             	add    %rsi,%rcx
  7c:	48 39 cf             	cmp    %rcx,%rdi
  7f:	48 89 48 18          	mov    %rcx,0x18(%rax)
  83:	0f 94 c0             	sete   %al
  86:	21 d0                	and    %edx,%eax
  88:	48 39 f9             	cmp    %rdi,%rcx
  8b:	0f 92 c2             	setb   %dl
  8e:	09 d0                	or     %edx,%eax
  90:	0f b6 c0             	movzbl %al,%eax
  93:	c3                   	ret
  94:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  9b:	00 00 00 00 
  9f:	90                   	nop

00000000000000a0 <u256_sub_ct>:
  a0:	f3 0f 1e fa          	endbr64
  a4:	49 89 f0             	mov    %rsi,%r8
  a7:	48 89 d6             	mov    %rdx,%rsi
  aa:	48 8b 12             	mov    (%rdx),%rdx
  ad:	49 8b 00             	mov    (%r8),%rax
  b0:	48 39 d0             	cmp    %rdx,%rax
  b3:	0f 92 c1             	setb   %cl
  b6:	48 29 d0             	sub    %rdx,%rax
  b9:	48 89 07             	mov    %rax,(%rdi)
  bc:	4c 8b 4e 08          	mov    0x8(%rsi),%r9
  c0:	49 8b 40 08          	mov    0x8(%r8),%rax
  c4:	4c 39 c8             	cmp    %r9,%rax
  c7:	0f 94 c2             	sete   %dl
  ca:	21 ca                	and    %ecx,%edx
  cc:	4c 39 c8             	cmp    %r9,%rax
  cf:	0f b6 c9             	movzbl %cl,%ecx
  d2:	41 0f 92 c2          	setb   %r10b
  d6:	4c 29 c8             	sub    %r9,%rax
  d9:	48 29 c8             	sub    %rcx,%rax
  dc:	44 09 d2             	or     %r10d,%edx
  df:	48 89 47 08          	mov    %rax,0x8(%rdi)
  e3:	49 8b 40 10          	mov    0x10(%r8),%rax
  e7:	4c 8b 4e 10          	mov    0x10(%rsi),%r9
  eb:	4c 39 c8             	cmp    %r9,%rax
  ee:	0f 94 c1             	sete   %cl
  f1:	21 d1                	and    %edx,%ecx
  f3:	4c 39 c8             	cmp    %r9,%rax
  f6:	0f b6 d2             	movzbl %dl,%edx
  f9:	41 0f 92 c2          	setb   %r10b
  fd:	4c 29 c8             	sub    %r9,%rax
 100:	48 29 d0             	sub    %rdx,%rax
 103:	44 09 d1             	or     %r10d,%ecx
 106:	48 89 47 10          	mov    %rax,0x10(%rdi)
 10a:	4d 8b 40 18          	mov    0x18(%r8),%r8
 10e:	48 8b 56 18          	mov    0x18(%rsi),%rdx
 112:	0f b6 f1             	movzbl %cl,%esi
 115:	4c 89 c0             	mov    %r8,%rax
 118:	48 29 d0             	sub    %rdx,%rax
 11b:	48 29 f0             	sub    %rsi,%rax
 11e:	49 39 d0             	cmp    %rdx,%r8
 121:	48 89 47 18          	mov    %rax,0x18(%rdi)
 125:	0f 94 c0             	sete   %al
 128:	21 c8                	and    %ecx,%eax
 12a:	49 39 d0             	cmp    %rdx,%r8
 12d:	0f 92 c2             	setb   %dl
 130:	09 d0                	or     %edx,%eax
 132:	0f b6 c0             	movzbl %al,%eax
 135:	c3                   	ret
 136:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
 13d:	00 00 00 

0000000000000140 <u256_select_ct>:
 140:	f3 0f 1e fa          	endbr64
 144:	49 89 d0             	mov    %rdx,%r8
 147:	48 89 ca             	mov    %rcx,%rdx
 14a:	48 89 f0             	mov    %rsi,%rax
 14d:	48 83 ee 01          	sub    $0x1,%rsi
 151:	49 8b 08             	mov    (%r8),%rcx
 154:	4c 8b 0a             	mov    (%rdx),%r9
 157:	48 f7 d8             	neg    %rax
 15a:	49 21 f1             	and    %rsi,%r9
 15d:	48 21 c1             	and    %rax,%rcx
 160:	4c 09 c9             	or     %r9,%rcx
 163:	48 89 0f             	mov    %rcx,(%rdi)
 166:	49 8b 48 08          	mov    0x8(%r8),%rcx
 16a:	4c 8b 4a 08          	mov    0x8(%rdx),%r9
 16e:	48 21 c1             	and    %rax,%rcx
 171:	49 21 f1             	and    %rsi,%r9
 174:	4c 09 c9             	or     %r9,%rcx
 177:	48 89 4f 08          	mov    %rcx,0x8(%rdi)
 17b:	49 8b 48 10          	mov    0x10(%r8),%rcx
 17f:	4c 8b 4a 10          	mov    0x10(%rdx),%r9
 183:	48 21 c1             	and    %rax,%rcx
 186:	49 21 f1             	and    %rsi,%r9
 189:	4c 09 c9             	or     %r9,%rcx
 18c:	48 89 4f 10          	mov    %rcx,0x10(%rdi)
 190:	49 23 40 18          	and    0x18(%r8),%rax
 194:	48 23 72 18          	and    0x18(%rdx),%rsi
 198:	48 09 f0             	or     %rsi,%rax
 19b:	48 89 47 18          	mov    %rax,0x18(%rdi)
 19f:	c3                   	ret

00000000000001a0 <u256_eq_ct>:
 1a0:	f3 0f 1e fa          	endbr64
 1a4:	48 8b 17             	mov    (%rdi),%rdx
 1a7:	48 8b 47 08          	mov    0x8(%rdi),%rax
 1ab:	48 33 16             	xor    (%rsi),%rdx
 1ae:	48 33 46 08          	xor    0x8(%rsi),%rax
 1b2:	48 09 c2             	or     %rax,%rdx
 1b5:	48 8b 47 10          	mov    0x10(%rdi),%rax
 1b9:	48 33 46 10          	xor    0x10(%rsi),%rax
 1bd:	48 09 c2             	or     %rax,%rdx
 1c0:	48 8b 47 18          	mov    0x18(%rdi),%rax
 1c4:	48 33 46 18          	xor    0x18(%rsi),%rax
 1c8:	48 09 c2             	or     %rax,%rdx
 1cb:	48 89 d0             	mov    %rdx,%rax
 1ce:	48 f7 d8             	neg    %rax
 1d1:	48 09 d0             	or     %rdx,%rax
 1d4:	48 c1 f8 3f          	sar    $0x3f,%rax
 1d8:	48 83 c0 01          	add    $0x1,%rax
 1dc:	c3                   	ret
 1dd:	0f 1f 00             	nopl   (%rax)

00000000000001e0 <u256_modadd_ct>:
 1e0:	f3 0f 1e fa          	endbr64
 1e4:	55                   	push   %rbp
 1e5:	49 89 ca             	mov    %rcx,%r10
 1e8:	53                   	push   %rbx
 1e9:	48 89 fb             	mov    %rdi,%rbx
 1ec:	48 83 ec 58          	sub    $0x58,%rsp
 1f0:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
 1f7:	00 00 
 1f9:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
 1fe:	31 c0                	xor    %eax,%eax
 200:	48 89 e5             	mov    %rsp,%rbp
 203:	48 89 ef             	mov    %rbp,%rdi
 206:	e8 00 00 00 00       	call   20b <u256_modadd_ct+0x2b>
 20b:	4c 89 d2             	mov    %r10,%rdx
 20e:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
 213:	48 89 ee             	mov    %rbp,%rsi
 216:	49 89 c3             	mov    %rax,%r11
 219:	e8 00 00 00 00       	call   21e <u256_modadd_ct+0x3e>
 21e:	66 0f 6f 54 24 10    	movdqa 0x10(%rsp),%xmm2
 224:	66 0f 6f 5c 24 30    	movdqa 0x30(%rsp),%xmm3
 22a:	48 89 c2             	mov    %rax,%rdx
 22d:	4c 89 d8             	mov    %r11,%rax
 230:	48 83 f0 01          	xor    $0x1,%rax
 234:	48 21 d0             	and    %rdx,%rax
 237:	48 89 c1             	mov    %rax,%rcx
 23a:	48 83 e8 01          	sub    $0x1,%rax
 23e:	48 f7 d9             	neg    %rcx
 241:	66 48 0f 6e c0       	movq   %rax,%xmm0
 246:	66 48 0f 6e c9       	movq   %rcx,%xmm1
 24b:	66 0f 6c c0          	punpcklqdq %xmm0,%xmm0
 24f:	66 0f 6c c9          	punpcklqdq %xmm1,%xmm1
 253:	66 0f db d8          	pand   %xmm0,%xmm3
 257:	66 0f db 44 24 20    	pand   0x20(%rsp),%xmm0
 25d:	66 0f db d1          	pand   %xmm1,%xmm2
 261:	66 0f db 0c 24       	pand   (%rsp),%xmm1
 266:	66 0f eb d3          	por    %xmm3,%xmm2
 26a:	66 0f eb c8          	por    %xmm0,%xmm1
 26e:	0f 11 53 10          	movups %xmm2,0x10(%rbx)
 272:	0f 11 0b             	movups %xmm1,(%rbx)
 275:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
 27a:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
 281:	00 00 
 283:	75 07                	jne    28c <u256_modadd_ct+0xac>
 285:	48 83 c4 58          	add    $0x58,%rsp
 289:	5b                   	pop    %rbx
 28a:	5d                   	pop    %rbp
 28b:	c3                   	ret
 28c:	e8 00 00 00 00       	call   291 <u256_modadd_ct+0xb1>
 291:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
 298:	00 00 00 00 
 29c:	0f 1f 40 00          	nopl   0x0(%rax)

00000000000002a0 <u256_modsub_ct>:
 2a0:	f3 0f 1e fa          	endbr64
 2a4:	53                   	push   %rbx
 2a5:	49 89 fb             	mov    %rdi,%r11
 2a8:	48 89 cb             	mov    %rcx,%rbx
 2ab:	48 83 ec 50          	sub    $0x50,%rsp
 2af:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
 2b6:	00 00 
 2b8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
 2bd:	31 c0                	xor    %eax,%eax
 2bf:	48 89 e7             	mov    %rsp,%rdi
 2c2:	e8 00 00 00 00       	call   2c7 <u256_modsub_ct+0x27>
 2c7:	48 89 fe             	mov    %rdi,%rsi
 2ca:	48 89 da             	mov    %rbx,%rdx
 2cd:	49 89 c2             	mov    %rax,%r10
 2d0:	48 8d 44 24 20       	lea    0x20(%rsp),%rax
 2d5:	48 89 c7             	mov    %rax,%rdi
 2d8:	e8 00 00 00 00       	call   2dd <u256_modsub_ct+0x3d>
 2dd:	4c 89 d0             	mov    %r10,%rax
 2e0:	66 0f 6f 54 24 30    	movdqa 0x30(%rsp),%xmm2
 2e6:	66 0f 6f 5c 24 10    	movdqa 0x10(%rsp),%xmm3
 2ec:	48 f7 d8             	neg    %rax
 2ef:	66 48 0f 6e c8       	movq   %rax,%xmm1
 2f4:	49 8d 42 ff          	lea    -0x1(%r10),%rax
 2f8:	66 48 0f 6e c0       	movq   %rax,%xmm0
 2fd:	66 0f 6c c9          	punpcklqdq %xmm1,%xmm1
 301:	66 0f 6c c0          	punpcklqdq %xmm0,%xmm0
 305:	66 0f db d1          	pand   %xmm1,%xmm2
 309:	66 0f db 4c 24 20    	pand   0x20(%rsp),%xmm1
 30f:	66 0f db d8          	pand   %xmm0,%xmm3
 313:	66 0f db 04 24       	pand   (%rsp),%xmm0
 318:	66 0f eb d3          	por    %xmm3,%xmm2
 31c:	66 0f eb c8          	por    %xmm0,%xmm1
 320:	41 0f 11 53 10       	movups %xmm2,0x10(%r11)
 325:	41 0f 11 0b          	movups %xmm1,(%r11)
 329:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
 32e:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
 335:	00 00 
 337:	75 06                	jne    33f <u256_modsub_ct+0x9f>
 339:	48 83 c4 50          	add    $0x50,%rsp
 33d:	5b                   	pop    %rbx
 33e:	c3                   	ret
 33f:	e8 00 00 00 00       	call   344 <u256_modsub_ct+0xa4>
 344:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
 34b:	00 00 00 00 
 34f:	90                   	nop

0000000000000350 <u256_mont_mul_ct>:
 350:	f3 0f 1e fa          	endbr64
 354:	41 57                	push   %r15
 356:	49 89 d1             	mov    %rdx,%r9
 359:	45 31 db             	xor    %r11d,%r11d
 35c:	41 56                	push   %r14
 35e:	41 55                	push   %r13
 360:	41 54                	push   %r12
 362:	55                   	push   %rbp
 363:	48 89 f5             	mov    %rsi,%rbp
 366:	53                   	push   %rbx
 367:	48 89 cb             	mov    %rcx,%rbx
 36a:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
 371:	48 8b 0e             	mov    (%rsi),%rcx
 374:	4d 8b 69 08          	mov    0x8(%r9),%r13
 378:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
 37d:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
 384:	00 00 
 386:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
 38d:	00 
 38e:	31 c0                	xor    %eax,%eax
 390:	48 8b 02             	mov    (%rdx),%rax
 393:	4c 89 6c 24 18       	mov    %r13,0x18(%rsp)
 398:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
 39d:	48 89 04 24          	mov    %rax,(%rsp)
 3a1:	48 f7 e1             	mul    %rcx
 3a4:	49 89 c6             	mov    %rax,%r14
 3a7:	4c 89 e8             	mov    %r13,%rax
 3aa:	49 89 d2             	mov    %rdx,%r10
 3ad:	48 f7 e1             	mul    %rcx
 3b0:	49 01 c2             	add    %rax,%r10
 3b3:	49 11 d3             	adc    %rdx,%r11
 3b6:	49 8b 51 10          	mov    0x10(%r9),%rdx
 3ba:	4d 8b 49 18          	mov    0x18(%r9),%r9
 3be:	45 31 ed             	xor    %r13d,%r13d
 3c1:	4d 89 dc             	mov    %r11,%r12
 3c4:	48 89 d0             	mov    %rdx,%rax
 3c7:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
 3cc:	48 f7 e1             	mul    %rcx
 3cf:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
 3d4:	49 01 c4             	add    %rax,%r12
 3d7:	48 89 c8             	mov    %rcx,%rax
 3da:	4c 89 c1             	mov    %r8,%rcx
 3dd:	49 11 d5             	adc    %rdx,%r13
 3e0:	49 f7 e1             	mul    %r9
 3e3:	31 ff                	xor    %edi,%edi
 3e5:	4c 01 e8             	add    %r13,%rax
 3e8:	48 11 fa             	adc    %rdi,%rdx
 3eb:	49 0f af ce          	imul   %r14,%rcx
 3ef:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
 3f4:	45 31 ff             	xor    %r15d,%r15d
 3f7:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
 3fc:	48 89 c8             	mov    %rcx,%rax
 3ff:	48 f7 23             	mulq   (%rbx)
 402:	48 89 c6             	mov    %rax,%rsi
 405:	48 89 d7             	mov    %rdx,%rdi
 408:	48 89 c8             	mov    %rcx,%rax
 40b:	4c 01 f6             	add    %r14,%rsi
 40e:	4c 11 ff             	adc    %r15,%rdi
 411:	48 f7 63 08          	mulq   0x8(%rbx)
 415:	45 31 db             	xor    %r11d,%r11d
 418:	48 89 fe             	mov    %rdi,%rsi
 41b:	31 ff                	xor    %edi,%edi
 41d:	4c 01 d0             	add    %r10,%rax
 420:	4c 11 da             	adc    %r11,%rdx
 423:	48 01 c6             	add    %rax,%rsi
 426:	48 89 c8             	mov    %rcx,%rax
 429:	48 11 d7             	adc    %rdx,%rdi
 42c:	48 f7 63 10          	mulq   0x10(%rbx)
 430:	45 31 ff             	xor    %r15d,%r15d
 433:	45 31 ed             	xor    %r13d,%r13d
 436:	49 89 c2             	mov    %rax,%r10
 439:	49 89 d3             	mov    %rdx,%r11
 43c:	48 89 c8             	mov    %rcx,%rax
 43f:	4d 01 e2             	add    %r12,%r10
 442:	4d 11 eb             	adc    %r13,%r11
 445:	49 01 fa             	add    %rdi,%r10
 448:	4d 11 fb             	adc    %r15,%r11
 44b:	48 f7 63 18          	mulq   0x18(%rbx)
 44f:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
 454:	45 31 ed             	xor    %r13d,%r13d
 457:	48 8b 4d 08          	mov    0x8(%rbp),%rcx
 45b:	45 31 c9             	xor    %r9d,%r9d
 45e:	49 01 c0             	add    %rax,%r8
 461:	48 8b 04 24          	mov    (%rsp),%rax
 465:	49 11 d1             	adc    %rdx,%r9
 468:	4d 01 d8             	add    %r11,%r8
 46b:	4d 11 e9             	adc    %r13,%r9
 46e:	48 f7 e1             	mul    %rcx
 471:	31 ff                	xor    %edi,%edi
 473:	48 01 c6             	add    %rax,%rsi
 476:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
 47b:	48 11 d7             	adc    %rdx,%rdi
 47e:	31 d2                	xor    %edx,%edx
 480:	45 31 db             	xor    %r11d,%r11d
 483:	49 01 fa             	add    %rdi,%r10
 486:	49 11 d3             	adc    %rdx,%r11
 489:	48 f7 e1             	mul    %rcx
 48c:	4d 89 d6             	mov    %r10,%r14
 48f:	4d 89 c2             	mov    %r8,%r10
 492:	4d 89 df             	mov    %r11,%r15
 495:	49 01 c6             	add    %rax,%r14
 498:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
 49d:	49 11 d7             	adc    %rdx,%r15
 4a0:	45 31 ed             	xor    %r13d,%r13d
 4a3:	45 31 db             	xor    %r11d,%r11d
 4a6:	48 f7 e1             	mul    %rcx
 4a9:	4d 89 fc             	mov    %r15,%r12
 4ac:	49 01 c2             	add    %rax,%r10
 4af:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
 4b4:	49 11 d3             	adc    %rdx,%r11
 4b7:	4d 01 d4             	add    %r10,%r12
 4ba:	4d 11 dd             	adc    %r11,%r13
 4bd:	4c 01 c8             	add    %r9,%rax
 4c0:	45 31 c9             	xor    %r9d,%r9d
 4c3:	4d 89 e8             	mov    %r13,%r8
 4c6:	49 01 c0             	add    %rax,%r8
 4c9:	48 89 c8             	mov    %rcx,%rax
 4cc:	48 8b 4d 10          	mov    0x10(%rbp),%rcx
 4d0:	49 83 d1 00          	adc    $0x0,%r9
 4d4:	48 f7 64 24 28       	mulq   0x28(%rsp)
 4d9:	4c 01 c0             	add    %r8,%rax
 4dc:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
 4e1:	4c 11 ca             	adc    %r9,%rdx
 4e4:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
 4e9:	31 ff                	xor    %edi,%edi
 4eb:	4c 0f af c6          	imul   %rsi,%r8
 4ef:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
 4f4:	4c 89 c0             	mov    %r8,%rax
 4f7:	48 f7 23             	mulq   (%rbx)
 4fa:	48 01 c6             	add    %rax,%rsi
 4fd:	4c 89 c0             	mov    %r8,%rax
 500:	48 11 d7             	adc    %rdx,%rdi
 503:	48 f7 63 08          	mulq   0x8(%rbx)
 507:	45 31 ff             	xor    %r15d,%r15d
 50a:	48 89 fe             	mov    %rdi,%rsi
 50d:	31 ff                	xor    %edi,%edi
 50f:	4c 01 f0             	add    %r14,%rax
 512:	4c 11 fa             	adc    %r15,%rdx
 515:	48 01 c6             	add    %rax,%rsi
 518:	4c 89 c0             	mov    %r8,%rax
 51b:	48 11 d7             	adc    %rdx,%rdi
 51e:	48 f7 63 10          	mulq   0x10(%rbx)
 522:	45 31 ed             	xor    %r13d,%r13d
 525:	45 31 db             	xor    %r11d,%r11d
 528:	49 89 fa             	mov    %rdi,%r10
 52b:	4c 01 e0             	add    %r12,%rax
 52e:	4c 11 ea             	adc    %r13,%rdx
 531:	49 01 c2             	add    %rax,%r10
 534:	4c 89 c0             	mov    %r8,%rax
 537:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
 53c:	49 11 d3             	adc    %rdx,%r11
 53f:	48 f7 63 18          	mulq   0x18(%rbx)
 543:	45 31 ed             	xor    %r13d,%r13d
 546:	45 31 c9             	xor    %r9d,%r9d
 549:	49 01 c0             	add    %rax,%r8
 54c:	48 8b 04 24          	mov    (%rsp),%rax
 550:	49 11 d1             	adc    %rdx,%r9
 553:	4d 01 d8             	add    %r11,%r8
 556:	4d 11 e9             	adc    %r13,%r9
 559:	48 f7 e1             	mul    %rcx
 55c:	31 ff                	xor    %edi,%edi
 55e:	48 01 c6             	add    %rax,%rsi
 561:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
 566:	48 11 d7             	adc    %rdx,%rdi
 569:	31 d2                	xor    %edx,%edx
 56b:	45 31 db             	xor    %r11d,%r11d
 56e:	49 01 fa             	add    %rdi,%r10
 571:	49 11 d3             	adc    %rdx,%r11
 574:	48 f7 e1             	mul    %rcx
 577:	4d 89 d6             	mov    %r10,%r14
 57a:	4d 89 c2             	mov    %r8,%r10
 57d:	4d 89 df             	mov    %r11,%r15
 580:	49 01 c6             	add    %rax,%r14
 583:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
 588:	49 11 d7             	adc    %rdx,%r15
 58b:	45 31 ed             	xor    %r13d,%r13d
 58e:	45 31 db             	xor    %r11d,%r11d
 591:	48 f7 e1             	mul    %rcx
 594:	4d 89 fc             	mov    %r15,%r12
 597:	49 01 c2             	add    %rax,%r10
 59a:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
 59f:	49 11 d3             	adc    %rdx,%r11
 5a2:	4d 01 d4             	add    %r10,%r12
 5a5:	4c 8b 54 24 40       	mov    0x40(%rsp),%r10
 5aa:	4d 11 dd             	adc    %r11,%r13
 5ad:	4c 01 c8             	add    %r9,%rax
 5b0:	45 31 c9             	xor    %r9d,%r9d
 5b3:	4d 89 e8             	mov    %r13,%r8
 5b6:	49 01 c0             	add    %rax,%r8
 5b9:	48 89 c8             	mov    %rcx,%rax
 5bc:	49 83 d1 00          	adc    $0x0,%r9
 5c0:	48 f7 64 24 28       	mulq   0x28(%rsp)
 5c5:	4c 01 c0             	add    %r8,%rax
 5c8:	4c 8b 45 18          	mov    0x18(%rbp),%r8
 5cc:	4c 11 ca             	adc    %r9,%rdx
 5cf:	4c 0f af d6          	imul   %rsi,%r10
 5d3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
 5d8:	31 ff                	xor    %edi,%edi
 5da:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
 5df:	4c 89 d0             	mov    %r10,%rax
 5e2:	48 f7 23             	mulq   (%rbx)
 5e5:	48 01 c6             	add    %rax,%rsi
 5e8:	4c 89 d0             	mov    %r10,%rax
 5eb:	48 11 d7             	adc    %rdx,%rdi
 5ee:	48 f7 63 08          	mulq   0x8(%rbx)
 5f2:	45 31 ff             	xor    %r15d,%r15d
 5f5:	48 89 fe             	mov    %rdi,%rsi
 5f8:	31 ff                	xor    %edi,%edi
 5fa:	4c 01 f0             	add    %r14,%rax
 5fd:	4c 11 fa             	adc    %r15,%rdx
 600:	48 01 c6             	add    %rax,%rsi
 603:	4c 89 d0             	mov    %r10,%rax
 606:	48 11 d7             	adc    %rdx,%rdi
 609:	48 f7 63 10          	mulq   0x10(%rbx)
 60d:	45 31 ff             	xor    %r15d,%r15d
 610:	45 31 ed             	xor    %r13d,%r13d
 613:	4c 01 e0             	add    %r12,%rax
 616:	4c 11 ea             	adc    %r13,%rdx
 619:	49 89 c4             	mov    %rax,%r12
 61c:	4c 89 d0             	mov    %r10,%rax
 61f:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
 624:	49 01 fc             	add    %rdi,%r12
 627:	49 89 d5             	mov    %rdx,%r13
 62a:	4d 11 fd             	adc    %r15,%r13
 62d:	48 f7 63 18          	mulq   0x18(%rbx)
 631:	45 31 c9             	xor    %r9d,%r9d
 634:	45 31 db             	xor    %r11d,%r11d
 637:	49 01 c2             	add    %rax,%r10
 63a:	48 8b 04 24          	mov    (%rsp),%rax
 63e:	49 11 d3             	adc    %rdx,%r11
 641:	4d 01 ea             	add    %r13,%r10
 644:	4d 11 cb             	adc    %r9,%r11
 647:	49 f7 e0             	mul    %r8
 64a:	31 ff                	xor    %edi,%edi
 64c:	48 01 c6             	add    %rax,%rsi
 64f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
 654:	48 11 d7             	adc    %rdx,%rdi
 657:	31 d2                	xor    %edx,%edx
 659:	45 31 ed             	xor    %r13d,%r13d
 65c:	49 01 fc             	add    %rdi,%r12
 65f:	49 11 d5             	adc    %rdx,%r13
 662:	49 f7 e0             	mul    %r8
 665:	49 01 c4             	add    %rax,%r12
 668:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
 66d:	49 11 d5             	adc    %rdx,%r13
 670:	4c 89 24 24          	mov    %r12,(%rsp)
 674:	4d 89 d4             	mov    %r10,%r12
 677:	45 31 ff             	xor    %r15d,%r15d
 67a:	49 f7 e0             	mul    %r8
 67d:	4c 89 6c 24 08       	mov    %r13,0x8(%rsp)
 682:	4d 89 ee             	mov    %r13,%r14
 685:	45 31 ed             	xor    %r13d,%r13d
 688:	49 01 c4             	add    %rax,%r12
 68b:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
 690:	49 11 d5             	adc    %rdx,%r13
 693:	4d 01 e6             	add    %r12,%r14
 696:	4d 11 ef             	adc    %r13,%r15
 699:	4c 01 d8             	add    %r11,%rax
 69c:	31 c9                	xor    %ecx,%ecx
 69e:	4d 89 fa             	mov    %r15,%r10
 6a1:	49 89 cb             	mov    %rcx,%r11
 6a4:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
 6a9:	49 01 c2             	add    %rax,%r10
 6ac:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
 6b1:	49 83 d3 00          	adc    $0x0,%r11
 6b5:	49 f7 e0             	mul    %r8
 6b8:	49 89 c0             	mov    %rax,%r8
 6bb:	49 89 d1             	mov    %rdx,%r9
 6be:	4d 01 d0             	add    %r10,%r8
 6c1:	4c 8b 14 24          	mov    (%rsp),%r10
 6c5:	4d 11 d9             	adc    %r11,%r9
 6c8:	48 0f af ce          	imul   %rsi,%rcx
 6cc:	45 31 ed             	xor    %r13d,%r13d
 6cf:	31 ff                	xor    %edi,%edi
 6d1:	4d 89 cc             	mov    %r9,%r12
 6d4:	48 89 c8             	mov    %rcx,%rax
 6d7:	48 f7 23             	mulq   (%rbx)
 6da:	48 01 c6             	add    %rax,%rsi
 6dd:	48 89 c8             	mov    %rcx,%rax
 6e0:	48 11 d7             	adc    %rdx,%rdi
 6e3:	48 f7 63 08          	mulq   0x8(%rbx)
 6e7:	45 31 db             	xor    %r11d,%r11d
 6ea:	48 89 fe             	mov    %rdi,%rsi
 6ed:	31 ff                	xor    %edi,%edi
 6ef:	4c 01 d0             	add    %r10,%rax
 6f2:	4c 11 da             	adc    %r11,%rdx
 6f5:	48 01 c6             	add    %rax,%rsi
 6f8:	48 89 c8             	mov    %rcx,%rax
 6fb:	48 11 d7             	adc    %rdx,%rdi
 6fe:	48 f7 63 10          	mulq   0x10(%rbx)
 702:	45 31 ff             	xor    %r15d,%r15d
 705:	48 89 74 24 70       	mov    %rsi,0x70(%rsp)
 70a:	48 89 fe             	mov    %rdi,%rsi
 70d:	31 ff                	xor    %edi,%edi
 70f:	4c 01 f0             	add    %r14,%rax
 712:	4c 11 fa             	adc    %r15,%rdx
 715:	48 01 c6             	add    %rax,%rsi
 718:	48 89 c8             	mov    %rcx,%rax
 71b:	48 11 d7             	adc    %rdx,%rdi
 71e:	48 f7 63 18          	mulq   0x18(%rbx)
 722:	45 31 db             	xor    %r11d,%r11d
 725:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
 72a:	48 89 fe             	mov    %rdi,%rsi
 72d:	31 ff                	xor    %edi,%edi
 72f:	4c 01 c0             	add    %r8,%rax
 732:	4c 11 da             	adc    %r11,%rdx
 735:	48 01 f0             	add    %rsi,%rax
 738:	4c 8d 5c 24 70       	lea    0x70(%rsp),%r11
 73d:	48 11 fa             	adc    %rdi,%rdx
 740:	4c 89 de             	mov    %r11,%rsi
 743:	48 8d 7c 24 50       	lea    0x50(%rsp),%rdi
 748:	45 31 ff             	xor    %r15d,%r15d
 74b:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
 752:	00 
 753:	49 89 d6             	mov    %rdx,%r14
 756:	49 8d 04 11          	lea    (%r9,%rdx,1),%rax
 75a:	48 89 da             	mov    %rbx,%rdx
 75d:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
 764:	00 
 765:	e8 00 00 00 00       	call   76a <u256_mont_mul_ct+0x41a>
 76a:	4d 01 f4             	add    %r14,%r12
 76d:	48 89 f9             	mov    %rdi,%rcx
 770:	48 8b 7c 24 48       	mov    0x48(%rsp),%rdi
 775:	4d 11 fd             	adc    %r15,%r13
 778:	4c 89 da             	mov    %r11,%rdx
 77b:	4c 89 ee             	mov    %r13,%rsi
 77e:	48 83 f6 01          	xor    $0x1,%rsi
 782:	48 21 c6             	and    %rax,%rsi
 785:	e8 00 00 00 00       	call   78a <u256_mont_mul_ct+0x43a>
 78a:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
 791:	00 
 792:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
 799:	00 00 
 79b:	75 12                	jne    7af <u256_mont_mul_ct+0x45f>
 79d:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
 7a4:	5b                   	pop    %rbx
 7a5:	5d                   	pop    %rbp
 7a6:	41 5c                	pop    %r12
 7a8:	41 5d                	pop    %r13
 7aa:	41 5e                	pop    %r14
 7ac:	41 5f                	pop    %r15
 7ae:	c3                   	ret
 7af:	e8 00 00 00 00       	call   7b4 <u256_mont_mul_ct+0x464>
 7b4:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
 7bb:	00 00 00 00 
 7bf:	90                   	nop

00000000000007c0 <u256_modpow_ct>:
 7c0:	f3 0f 1e fa          	endbr64
 7c4:	41 57                	push   %r15
 7c6:	41 56                	push   %r14
 7c8:	49 89 ce             	mov    %rcx,%r14
 7cb:	41 55                	push   %r13
 7cd:	45 31 ed             	xor    %r13d,%r13d
 7d0:	41 54                	push   %r12
 7d2:	55                   	push   %rbp
 7d3:	53                   	push   %rbx
 7d4:	48 81 ec 98 00 00 00 	sub    $0x98,%rsp
 7db:	f3 41 0f 6f 01       	movdqu (%r9),%xmm0
 7e0:	f3 0f 6f 16          	movdqu (%rsi),%xmm2
 7e4:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
 7e9:	f3 0f 6f 5e 10       	movdqu 0x10(%rsi),%xmm3
 7ee:	48 8d 6c 24 40       	lea    0x40(%rsp),%rbp
 7f3:	4c 8d 64 24 20       	lea    0x20(%rsp),%r12
 7f8:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
 7fd:	f3 41 0f 6f 49 10    	movdqu 0x10(%r9),%xmm1
 803:	4c 8d 7c 24 60       	lea    0x60(%rsp),%r15
 808:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
 80d:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
 814:	00 00 
 816:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
 81d:	00 
 81e:	31 c0                	xor    %eax,%eax
 820:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
 825:	0f 29 4c 24 30       	movaps %xmm1,0x30(%rsp)
 82a:	0f 29 54 24 40       	movaps %xmm2,0x40(%rsp)
 82f:	0f 29 5c 24 50       	movaps %xmm3,0x50(%rsp)
 834:	0f 1f 40 00          	nopl   0x0(%rax)
 838:	44 89 ea             	mov    %r13d,%edx
 83b:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
 840:	44 89 e9             	mov    %r13d,%ecx
 843:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
 848:	c1 fa 06             	sar    $0x6,%edx
 84b:	4c 89 e6             	mov    %r12,%rsi
 84e:	4c 89 ff             	mov    %r15,%rdi
 851:	41 83 c5 01          	add    $0x1,%r13d
 855:	48 63 d2             	movslq %edx,%rdx
 858:	48 8b 1c d0          	mov    (%rax,%rdx,8),%rbx
 85c:	48 89 ea             	mov    %rbp,%rdx
 85f:	48 d3 eb             	shr    %cl,%rbx
 862:	4c 89 f1             	mov    %r14,%rcx
 865:	e8 00 00 00 00       	call   86a <u256_modpow_ct+0xaa>
 86a:	83 e3 01             	and    $0x1,%ebx
 86d:	4c 89 e1             	mov    %r12,%rcx
 870:	4c 89 fa             	mov    %r15,%rdx
 873:	48 89 de             	mov    %rbx,%rsi
 876:	4c 89 e7             	mov    %r12,%rdi
 879:	e8 00 00 00 00       	call   87e <u256_modpow_ct+0xbe>
 87e:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
 883:	4c 89 f1             	mov    %r14,%rcx
 886:	48 89 ea             	mov    %rbp,%rdx
 889:	48 89 ee             	mov    %rbp,%rsi
 88c:	48 89 ef             	mov    %rbp,%rdi
 88f:	e8 00 00 00 00       	call   894 <u256_modpow_ct+0xd4>
 894:	41 81 fd 00 01 00 00 	cmp    $0x100,%r13d
 89b:	75 9b                	jne    838 <u256_modpow_ct+0x78>
 89d:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
 8a2:	66 0f 6f 64 24 20    	movdqa 0x20(%rsp),%xmm4
 8a8:	66 0f 6f 6c 24 30    	movdqa 0x30(%rsp),%xmm5
 8ae:	0f 11 20             	movups %xmm4,(%rax)
 8b1:	0f 11 68 10          	movups %xmm5,0x10(%rax)
 8b5:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
 8bc:	00 
 8bd:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
 8c4:	00 00 
 8c6:	75 12                	jne    8da <u256_modpow_ct+0x11a>
 8c8:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
 8cf:	5b                   	pop    %rbx
 8d0:	5d                   	pop    %rbp
 8d1:	41 5c                	pop    %r12
 8d3:	41 5d                	pop    %r13
 8d5:	41 5e                	pop    %r14
 8d7:	41 5f                	pop    %r15
 8d9:	c3                   	ret
 8da:	e8 00 00 00 00       	call   8df <u256_modpow_ct+0x11f>
 8df:	90                   	nop

00000000000008e0 <u256_modinv_ct>:
 8e0:	f3 0f 1e fa          	endbr64
 8e4:	48 83 ec 38          	sub    $0x38,%rsp
 8e8:	48 89 d0             	mov    %rdx,%rax
 8eb:	49 89 f3             	mov    %rsi,%r11
 8ee:	49 89 fa             	mov    %rdi,%r10
 8f1:	64 48 8b 14 25 28 00 	mov    %fs:0x28,%rdx
 8f8:	00 00 
 8fa:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
 8ff:	48 8b 10             	mov    (%rax),%rdx
 902:	48 8b 70 08          	mov    0x8(%rax),%rsi
 906:	48 83 fa 01          	cmp    $0x1,%rdx
 90a:	40 0f 96 c7          	setbe  %dil
 90e:	48 83 ea 02          	sub    $0x2,%rdx
 912:	48 85 f6             	test   %rsi,%rsi
 915:	48 89 14 24          	mov    %rdx,(%rsp)
 919:	0f 94 c2             	sete   %dl
 91c:	21 fa                	and    %edi,%edx
 91e:	40 0f b6 ff          	movzbl %dil,%edi
 922:	48 29 fe             	sub    %rdi,%rsi
 925:	44 0f b6 ca          	movzbl %dl,%r9d
 929:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
 92e:	48 8b 70 10          	mov    0x10(%rax),%rsi
 932:	48 89 f7             	mov    %rsi,%rdi
 935:	4c 29 cf             	sub    %r9,%rdi
 938:	48 85 f6             	test   %rsi,%rsi
 93b:	4d 89 c1             	mov    %r8,%r9
 93e:	49 89 c8             	mov    %rcx,%r8
 941:	40 0f 94 c6          	sete   %sil
 945:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
 94a:	48 89 c1             	mov    %rax,%rcx
 94d:	4c 89 d7             	mov    %r10,%rdi
 950:	40 0f b6 f6          	movzbl %sil,%esi
 954:	48 21 d6             	and    %rdx,%rsi
 957:	48 8b 50 18          	mov    0x18(%rax),%rdx
 95b:	48 29 f2             	sub    %rsi,%rdx
 95e:	4c 89 de             	mov    %r11,%rsi
 961:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
 966:	48 89 e2             	mov    %rsp,%rdx
 969:	e8 00 00 00 00       	call   96e <u256_modinv_ct+0x8e>
 96e:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
 973:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
 97a:	00 00 
 97c:	75 05                	jne    983 <u256_modinv_ct+0xa3>
 97e:	48 83 c4 38          	add    $0x38,%rsp
 982:	c3                   	ret
 983:	e8 00 00 00 00       	call   988 <u256_modinv_ct+0xa8>
