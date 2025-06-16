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
8. 