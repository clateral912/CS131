1. x86Lite是x86的精简子集[[lec03.pdf#page=7&selection=61,1,66,20|lec03, 页面 7]]：
	- 只有64bit的带符号整数
	- 只有大约20条指令
	- 认定所有操作数都是64bit（quadword）的，因此一个word为16bit，因此所有内存地址必须是64位对其的，即不能访问0x04这种地址，处理器以qword为单位读写内存
2. 我们使用AT&T notation，即如图所示，立即数使用dollar符号$开头，寄存器使用百分号%开头：
3. 注意符号：DEST表示通用操作数，是内存地址或寄存器别名，SRC也是通用操作数，表示内存地址，寄存器或立即数
	. 
		![[Pasted image 20250617001706.png]]
4. 一些你没接触过的指令：
	 - neg DEST  取DEST的2的补码
	 - imul SRC, Reg  乘法，Reg = Reg * SRC，只保留乘法结果的低64位，字母i表示signed有符号乘法
	 - sarq Amt, DEST 算数右移Amt位 sar = shift arithmetic right
	 - shlq Amt, DEST 逻辑（按位）左移Amt位 shl = shift left
	 - shrq Amt, DEST 逻辑（按位）右移Amt位
	 - leaq Ind, DEST 将Ind指向的内存地址放入DEST中，即DEST = addr(ind)
	 - cmpq SRC1, SRC2 注意比较顺序，是SRC2 ？ SRC1, 这对大于还是小于而言很重要
	 - setbCC DEST setb中的b代表一个byte，即只会设置DEST的低8bit，CC表示条件码后缀，比如e或z，是上一次运算得到的condition code，e表示是否equal，z表示是否为0, le表示是否小于等于等等。比如setbge DEST表示如果上次运算的结果是大于等于，那么将DEST的低八位设置为0。
	 - jCC SRC CC同样为条件码后缀，如果CC那么跳转到SRC，否则往下执行
	 - pushq SRC 将rsp设置为rsp - 8, 栈向下增长4words，之后令Mem\[rsp] = SRC，先增长栈再压栈
	 - popq DEST 令DEST = Mem\[rsp], 随后将rsp设置为rsp + 8, 先弹栈再释放栈指针
	 - callq SRC 将rip现在的值压入栈中，随后jump到SRC（将rip设置为SRC）
	 - retq 将栈顶的值弹出，作为rip的新值，即无条件跳转到栈顶存储的地址
5. x86的间接寻址方式：
	Ind := \[base:Reg]\[index:Reg,scale:int32]\[disp]
	addr(ind) = Base + \[Index × scale] + Disp
	（注意： index寄存器不能为rip）
	使用addr表示Ind表达式指向的实际内存地址，当我们希望表示内存地址中存储的值的时候，我们使用Mem\[addr]记号，比如Mem\[addr(ind)]表示ind指向的值本身
	一般disp偏移量写在括号外，括号内的三个参数分别为(base, index, scale)
	- ind = 12(%rax, %rcx, 4)表示的地址为addr(ind) = rax + 4\*rcx +12
6. x86Lite的计算是有副作用的：设置flag与code，详见图片：
	![[Pasted image 20250617004052.png]]
7. 任何x86程序都以main label为入口点！label的具体翻译由linker和loader完成
8. 调用约定
	- 对于寄存器，调用者和被调用者都可能要使用同样的一些寄存器，比如可能都要使用到rax，那么如果不做任何的保护，调用者和被调用者可能会覆盖掉对方rax中的内容
		- 调用者保存：调用者在调用函数前手动保存自己的寄存器状态，即使被调用函数覆写了这些寄存器，最后也能恢复原样。被调用者可以自由使用这些寄存器，除了%rbp, %rbx, %r12-%r15以外，其他的寄存器都由调用者保存。
		- 被调用者保存：由被调用者保存调用者的寄存器，好处是调用者完全不需要care寄存器而数据安全，以下寄存器由被调用者保存：
			%rbp, %rbx, %r12-%r15
	- 对于栈清理Stack Cleanup，函数调用时，多余的参数（六个以上的参数，参数若小于六个，则依靠寄存器传递参数）传递是靠压栈实现的（注意：压栈顺序为右手侧的参数先压入栈中），当函数结束后，栈中的多余参数必须被清理（这里的“清理”其实很简单，就是回退栈指针即可）。
		- 调用者清理：调用者在函数返回后将栈指针调整回去，简单，调用者可以很清楚的知道自己push了多少参数，缺点是每个调用点都需要执行清理，增加了代码大小
		- 被调用者清理：被调用函数在返回前，清理栈上的空间，使得调用者代码更紧凑，因为清理代码只要在被调用函数中出现一次即可，缺点是对于可变参数的函数很不友好，因为被调用函数无法知道调用者在栈上push了多少参数
9. IMPORTANT：函数调用的整个流程，假设f为调用者，g为被调用者
	1. 设置参数：假设我们要传递八个参数，前六个参数放置在寄存器中，后两个参数被push到栈上，第八个参数在高地址（先push），第七个在低地址，参数设置后rsp在第七个参数的位置
	2. 执行callq：在函数f内，将callq的下一条语句的地址设置为Iret，并将其push到stack上，现在rsp指向Iret，此时rip指向函数g的code首地址，此时rbp还是旧的f的栈底
	3. 开始执行函数g
	4. 保存rbp：函数g的第一件事就是将f的栈底保存起来，具体上来说就是将其push到stack上，用于之后恢复f的栈帧，此时rsp指向栈顶的rbp的值（不是rbp！）
	5. 移动rbp到g的栈底（此时的rsp）：因为我们已经保存了f的栈底，现在可以安全的调整栈底了（令rbp=rsp），注意此时rsp就指向旧的rbp的值，此时rbp=rsp
	6. g为自己分配scratch栈空间：g不由分说的将rsp向下调整128字节，这128字节是用于存储可能的局部变量，保存寄存器值，传递大于6个的参数等等要求的。分配scratch空间是C语言编译器的约定，可以被优化掉，不是必须的。
	7. g保存调用约定中指定的callee save寄存器：即%rbp, %rbx, %r12-%r15，将它们压入scratch栈空间
	8. g执行完毕
	9. g返回值：g将结果ANS存放进rax寄存器中
	10. g清理栈空间：将rsp指针向下调整128个字节，现在，rsp和rbp又指向同一个地址了
	11. g设置f的栈底：将栈中存储的旧rbp弹出，并重设（popq %rbp）,执行完这条语句后，rsp向下移动一格，恰好指向Iret的位置
	12. g返回：执行ret，即popq %rip，将此时rsp指向的位置中的值Iret弹出到rip中，程序跳转到Iret label处继续执行
	13. 至此，f完成了函数调用的整个过程，栈帧恢复如初，并拿到了返回值（注意！此时stack中的arg7和arg8还尚未被清理！）
	![[Pasted image 20250617014433.png]]
	