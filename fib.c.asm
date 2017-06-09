
./a.out:     ファイル形式 elf64-x86-64


セクション .init の逆アセンブル:

00000000004003c8 <.init>:
  4003c8:	48 83 ec 08          	sub    $0x8,%rsp
  4003cc:	48 8b 05 25 0c 20 00 	mov    0x200c25(%rip),%rax        # 600ff8 <__libc_start_main@plt+0x200be8>
  4003d3:	48 85 c0             	test   %rax,%rax
  4003d6:	74 05                	je     4003dd <printf@plt-0x23>
  4003d8:	e8 43 00 00 00       	callq  400420 <__libc_start_main@plt+0x10>
  4003dd:	48 83 c4 08          	add    $0x8,%rsp
  4003e1:	c3                   	retq   

セクション .plt の逆アセンブル:

00000000004003f0 <printf@plt-0x10>:
  4003f0:	ff 35 12 0c 20 00    	pushq  0x200c12(%rip)        # 601008 <__libc_start_main@plt+0x200bf8>
  4003f6:	ff 25 14 0c 20 00    	jmpq   *0x200c14(%rip)        # 601010 <__libc_start_main@plt+0x200c00>
  4003fc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400400 <printf@plt>:
  400400:	ff 25 12 0c 20 00    	jmpq   *0x200c12(%rip)        # 601018 <__libc_start_main@plt+0x200c08>
  400406:	68 00 00 00 00       	pushq  $0x0
  40040b:	e9 e0 ff ff ff       	jmpq   4003f0 <printf@plt-0x10>

0000000000400410 <__libc_start_main@plt>:
  400410:	ff 25 0a 0c 20 00    	jmpq   *0x200c0a(%rip)        # 601020 <__libc_start_main@plt+0x200c10>
  400416:	68 01 00 00 00       	pushq  $0x1
  40041b:	e9 d0 ff ff ff       	jmpq   4003f0 <printf@plt-0x10>

セクション .plt.got の逆アセンブル:

0000000000400420 <.plt.got>:
  400420:	ff 25 d2 0b 20 00    	jmpq   *0x200bd2(%rip)        # 600ff8 <__libc_start_main@plt+0x200be8>
  400426:	66 90                	xchg   %ax,%ax

セクション .text の逆アセンブル:

0000000000400430 <.text>:
  400430:	31 ed                	xor    %ebp,%ebp
  400432:	49 89 d1             	mov    %rdx,%r9
  400435:	5e                   	pop    %rsi
  400436:	48 89 e2             	mov    %rsp,%rdx
  400439:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  40043d:	50                   	push   %rax
  40043e:	54                   	push   %rsp
  40043f:	49 c7 c0 00 06 40 00 	mov    $0x400600,%r8
  400446:	48 c7 c1 90 05 40 00 	mov    $0x400590,%rcx
  40044d:	48 c7 c7 6a 05 40 00 	mov    $0x40056a,%rdi
  400454:	e8 b7 ff ff ff       	callq  400410 <__libc_start_main@plt>
  400459:	f4                   	hlt    
  40045a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  400460:	b8 3f 10 60 00       	mov    $0x60103f,%eax
  400465:	55                   	push   %rbp
  400466:	48 2d 38 10 60 00    	sub    $0x601038,%rax
  40046c:	48 83 f8 0e          	cmp    $0xe,%rax
  400470:	48 89 e5             	mov    %rsp,%rbp
  400473:	76 1b                	jbe    400490 <__libc_start_main@plt+0x80>
  400475:	b8 00 00 00 00       	mov    $0x0,%eax
  40047a:	48 85 c0             	test   %rax,%rax
  40047d:	74 11                	je     400490 <__libc_start_main@plt+0x80>
  40047f:	5d                   	pop    %rbp
  400480:	bf 38 10 60 00       	mov    $0x601038,%edi
  400485:	ff e0                	jmpq   *%rax
  400487:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40048e:	00 00 
  400490:	5d                   	pop    %rbp
  400491:	c3                   	retq   
  400492:	0f 1f 40 00          	nopl   0x0(%rax)
  400496:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  40049d:	00 00 00 
  4004a0:	be 38 10 60 00       	mov    $0x601038,%esi
  4004a5:	55                   	push   %rbp
  4004a6:	48 81 ee 38 10 60 00 	sub    $0x601038,%rsi
  4004ad:	48 c1 fe 03          	sar    $0x3,%rsi
  4004b1:	48 89 e5             	mov    %rsp,%rbp
  4004b4:	48 89 f0             	mov    %rsi,%rax
  4004b7:	48 c1 e8 3f          	shr    $0x3f,%rax
  4004bb:	48 01 c6             	add    %rax,%rsi
  4004be:	48 d1 fe             	sar    %rsi
  4004c1:	74 15                	je     4004d8 <__libc_start_main@plt+0xc8>
  4004c3:	b8 00 00 00 00       	mov    $0x0,%eax
  4004c8:	48 85 c0             	test   %rax,%rax
  4004cb:	74 0b                	je     4004d8 <__libc_start_main@plt+0xc8>
  4004cd:	5d                   	pop    %rbp
  4004ce:	bf 38 10 60 00       	mov    $0x601038,%edi
  4004d3:	ff e0                	jmpq   *%rax
  4004d5:	0f 1f 00             	nopl   (%rax)
  4004d8:	5d                   	pop    %rbp
  4004d9:	c3                   	retq   
  4004da:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  4004e0:	80 3d 51 0b 20 00 00 	cmpb   $0x0,0x200b51(%rip)        # 601038 <__libc_start_main@plt+0x200c28>
  4004e7:	75 11                	jne    4004fa <__libc_start_main@plt+0xea>
  4004e9:	55                   	push   %rbp
  4004ea:	48 89 e5             	mov    %rsp,%rbp
  4004ed:	e8 6e ff ff ff       	callq  400460 <__libc_start_main@plt+0x50>
  4004f2:	5d                   	pop    %rbp
  4004f3:	c6 05 3e 0b 20 00 01 	movb   $0x1,0x200b3e(%rip)        # 601038 <__libc_start_main@plt+0x200c28>
  4004fa:	f3 c3                	repz retq 
  4004fc:	0f 1f 40 00          	nopl   0x0(%rax)
  400500:	bf 20 0e 60 00       	mov    $0x600e20,%edi
  400505:	48 83 3f 00          	cmpq   $0x0,(%rdi)
  400509:	75 05                	jne    400510 <__libc_start_main@plt+0x100>
  40050b:	eb 93                	jmp    4004a0 <__libc_start_main@plt+0x90>
  40050d:	0f 1f 00             	nopl   (%rax)
  400510:	b8 00 00 00 00       	mov    $0x0,%eax
  400515:	48 85 c0             	test   %rax,%rax
  400518:	74 f1                	je     40050b <__libc_start_main@plt+0xfb>
  40051a:	55                   	push   %rbp
  40051b:	48 89 e5             	mov    %rsp,%rbp
  40051e:	ff d0                	callq  *%rax
  400520:	5d                   	pop    %rbp
  400521:	e9 7a ff ff ff       	jmpq   4004a0 <__libc_start_main@plt+0x90>
  400526:	55                   	push   %rbp
  400527:	48 89 e5             	mov    %rsp,%rbp
  40052a:	53                   	push   %rbx
  40052b:	48 83 ec 18          	sub    $0x18,%rsp
  40052f:	89 7d ec             	mov    %edi,-0x14(%rbp)
  400532:	83 7d ec 00          	cmpl   $0x0,-0x14(%rbp)
  400536:	74 06                	je     40053e <__libc_start_main@plt+0x12e>
  400538:	83 7d ec 01          	cmpl   $0x1,-0x14(%rbp)
  40053c:	75 07                	jne    400545 <__libc_start_main@plt+0x135>
  40053e:	b8 01 00 00 00       	mov    $0x1,%eax
  400543:	eb 1e                	jmp    400563 <__libc_start_main@plt+0x153>
  400545:	8b 45 ec             	mov    -0x14(%rbp),%eax
  400548:	83 e8 01             	sub    $0x1,%eax
  40054b:	89 c7                	mov    %eax,%edi
  40054d:	e8 d4 ff ff ff       	callq  400526 <__libc_start_main@plt+0x116>
  400552:	89 c3                	mov    %eax,%ebx
  400554:	8b 45 ec             	mov    -0x14(%rbp),%eax
  400557:	83 e8 02             	sub    $0x2,%eax
  40055a:	89 c7                	mov    %eax,%edi
  40055c:	e8 c5 ff ff ff       	callq  400526 <__libc_start_main@plt+0x116>
  400561:	01 d8                	add    %ebx,%eax
  400563:	48 83 c4 18          	add    $0x18,%rsp
  400567:	5b                   	pop    %rbx
  400568:	5d                   	pop    %rbp
  400569:	c3                   	retq   
  40056a:	55                   	push   %rbp
  40056b:	48 89 e5             	mov    %rsp,%rbp
  40056e:	bf 0a 00 00 00       	mov    $0xa,%edi
  400573:	e8 ae ff ff ff       	callq  400526 <__libc_start_main@plt+0x116>
  400578:	89 c6                	mov    %eax,%esi
  40057a:	bf 14 06 40 00       	mov    $0x400614,%edi
  40057f:	b8 00 00 00 00       	mov    $0x0,%eax
  400584:	e8 77 fe ff ff       	callq  400400 <printf@plt>
  400589:	b8 00 00 00 00       	mov    $0x0,%eax
  40058e:	5d                   	pop    %rbp
  40058f:	c3                   	retq   
  400590:	41 57                	push   %r15
  400592:	41 56                	push   %r14
  400594:	41 89 ff             	mov    %edi,%r15d
  400597:	41 55                	push   %r13
  400599:	41 54                	push   %r12
  40059b:	4c 8d 25 6e 08 20 00 	lea    0x20086e(%rip),%r12        # 600e10 <__libc_start_main@plt+0x200a00>
  4005a2:	55                   	push   %rbp
  4005a3:	48 8d 2d 6e 08 20 00 	lea    0x20086e(%rip),%rbp        # 600e18 <__libc_start_main@plt+0x200a08>
  4005aa:	53                   	push   %rbx
  4005ab:	49 89 f6             	mov    %rsi,%r14
  4005ae:	49 89 d5             	mov    %rdx,%r13
  4005b1:	4c 29 e5             	sub    %r12,%rbp
  4005b4:	48 83 ec 08          	sub    $0x8,%rsp
  4005b8:	48 c1 fd 03          	sar    $0x3,%rbp
  4005bc:	e8 07 fe ff ff       	callq  4003c8 <printf@plt-0x38>
  4005c1:	48 85 ed             	test   %rbp,%rbp
  4005c4:	74 20                	je     4005e6 <__libc_start_main@plt+0x1d6>
  4005c6:	31 db                	xor    %ebx,%ebx
  4005c8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4005cf:	00 
  4005d0:	4c 89 ea             	mov    %r13,%rdx
  4005d3:	4c 89 f6             	mov    %r14,%rsi
  4005d6:	44 89 ff             	mov    %r15d,%edi
  4005d9:	41 ff 14 dc          	callq  *(%r12,%rbx,8)
  4005dd:	48 83 c3 01          	add    $0x1,%rbx
  4005e1:	48 39 eb             	cmp    %rbp,%rbx
  4005e4:	75 ea                	jne    4005d0 <__libc_start_main@plt+0x1c0>
  4005e6:	48 83 c4 08          	add    $0x8,%rsp
  4005ea:	5b                   	pop    %rbx
  4005eb:	5d                   	pop    %rbp
  4005ec:	41 5c                	pop    %r12
  4005ee:	41 5d                	pop    %r13
  4005f0:	41 5e                	pop    %r14
  4005f2:	41 5f                	pop    %r15
  4005f4:	c3                   	retq   
  4005f5:	90                   	nop
  4005f6:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  4005fd:	00 00 00 
  400600:	f3 c3                	repz retq 

セクション .fini の逆アセンブル:

0000000000400604 <.fini>:
  400604:	48 83 ec 08          	sub    $0x8,%rsp
  400608:	48 83 c4 08          	add    $0x8,%rsp
  40060c:	c3                   	retq   
