# input , output

def doTest(input, output):
    bin = Assembler(input)
    #print(bin)
    print(bin == output)
    
doTest("ret 24", 'C21800')


def Test0or1arg():
    # no arg commands
    doTest("clc", 'F8')

    # one arg commands

    # no reg or memory 
    doTest("ret 24", 'C21800')
    doTest("ret 0x24", 'C22400')

    doTest("call 01160002", 'E800000000')
    doTest("jmp 0x00123ABC", 'E900000000')
    doTest("jne 12", '0F8500000000')
    doTest("jne 0x12", '0F8500000000')

    doTest("push 1", '6A01')
    doTest("push 12", '6A0C') 
    doTest("push 0x12", '6A12') 
    doTest("push 2424", '6878090000') 
    doTest("push 24242424", '68F8E87101') 

    # only reg 
    doTest("inc rax", '48FFC0')
    doTest("inc eax", 'FFC0') 
    doTest("inc ax", '66FFC0') 

    doTest("call rax", 'FFD0') 
    doTest("jmp ax", '66FFE0') 

    doTest("idiv rcx", '48F7F9') 

    doTest("not al", 'F6D0')
    
    # only memory 

    # indirect one reg no scale -> no SIB
    #no disp
    
    doTest("not DWORD PTR [ebx]",'67F713')
    doTest("not BYTE PTR [rax]",'F610')
    doTest("not QWORD PTR [rax]",'48F710')

    # indicret base=ebp(or rbp) -> mod=01, base=ebp, disp8=0
    doTest("not QWORD PTR [ebp]",'6748F75500')
    #with disp
    doTest("not DWORD PTR [ebp+0x32]",'67F75532')
    doTest("not DWORD PTR [ebp+0x3234]",'67F79534320000')
    doTest("not DWORD PTR [ebp+0x32345467]",'67F79567543432')
    doTest("not DWORD PTR [rbx+0x32345467]",'F79367543432')

    # direct must use SIB -> mod=00, base=ebp, disp32
    doTest("not QWORD PTR [0x5555551E]",'48F714251E555555')
    doTest("not QWORD PTR [0x5555]",'48F7142555550000')
    doTest("not QWORD PTR [0x55]",'48F7142555000000')

    # indirect no base only scale -> mod=00, base=ebp, disp32=0 
    doTest("not QWORD PTR [ecx*8]",'6748F714CD00000000')
    # indirect no base only scale and disp8 -> mod=00, base=ebp, disp8->disp32 
    doTest("not QWORD PTR [ecx*8+0x06]",'6748F714CD06000000')

    # indicret base=ebp(or rbp) and no disp -> mod=01, base=ebp, disp8=8
    doTest("not QWORD PTR [ebp+ecx*1]",'6748F7540D00') 
    doTest("not QWORD PTR [rbp+rcx*8]",'48F754CD00')
    doTest("not DWORD PTR [edx+ecx*2]",'67F7144A')

    # indirect has base, index
    doTest("not DWORD PTR [rbx+rcx*1]",'F7140B')

    # indirect has base, index and disp
    doTest("not DWORD PTR [ebp+ecx*2+0x32345467]",'67F7944D67543432')
    doTest("not DWORD PTR [rbx+rcx*1+0x32345467]",'F7940B67543432')

    doTest("not QWORD PTR [ebp+ecx]",'6748F7540D00')
    doTest("not QWORD PTR [rbx+rcx]",'48F7140B')


    #using new registers
    doTest("not r8",'49F7D0')

    doTest("not QWORD PTR [r8]",'49F710')
    doTest("not QWORD PTR [r8+rax]",'49F71400')

    doTest("not QWORD PTR [r8+r9]",'4BF71408')
    doTest("not QWORD PTR [r8d+r9d]",'674BF71408')

    doTest("imul rbx",'48F7EB')
    doTest("imul QWORD PTR [rbx]",'48F72B')

    doTest("shl rax", '48D1E0')
    doTest("shl QWORD PTR [rax]", '48D120')

    doTest("mov edx,DWORD PTR [ebx+ecx*1+0x32345467]", '678B940B67543432')
    doTest("mov DWORD PTR [ebx+ecx*1+0x32345467],edx", '6789940B67543432')
    doTest("mov rax,QWORD PTR [r8+r12*4+0x16]", '4B8B44A016')
    doTest("mov DWORD PTR [r8+r12*4+0x16],eax",  '438944A016')
    doTest("mov DWORD PTR [r8+r12*4+0x16],eax",  '438944A016')
    doTest("mov DWORD PTR [r8+r12*4+0x1],eax",  '438944A001')

    return

def Test2args():

    '''
    #WRONG -> disp shoulb be extended to match ?WORD
    doTest("add DWORD PTR [ebx+ecx*1],852",   '6781040B54030000')
    doTest("adc DWORD PTR [rbx+rcx*1+0x32345467],0x852", '81940B6754343252080000') #no rex   
    doTest("add DWORD PTR [r10+rcx*1+0x32345467],0x852", '4181840A6754343252080000')
    '''

    # reg to reg with rex
    doTest("mov rax,rbx", '4889D8')
    doTest("mov r9,r8", '4D89C1')

    # reg to reg with prefix
    doTest("mov ax,bx", '6689D8')
    doTest("mov r9w,ax", '664189C1')
    
    # memory_to_reg or reg_to_memory 
    doTest("mov edx,DWORD PTR [ebx+ecx*1+0x32345467]", '678B940B67543432')
    doTest("mov DWORD PTR [ebx+ecx*1+0x32345467],edx", '6789940B67543432')
    doTest("mov rax,QWORD PTR [r8+r12*4+0x16]", '4B8B44A016')
    doTest("mov DWORD PTR [r8+r12*4+0x16],eax",  '438944A016')
    doTest("xadd BYTE PTR [r8+0x1100],al", '410FC08000110000')

    # immediate_to_memory
    doTest("mov DWORD PTR [ebx+ecx*1+0x32345467],8",   '67C7840B6754343208000000')
    doTest("mov DWORD PTR [ebx+ecx*1+0x32345467],0x8", '67C7840B6754343208000000')
    doTest("add DWORD PTR [ebx+ecx*1],852",   '6781040B54030000')
    doTest("adc DWORD PTR [rbx+rcx*1+0x32345467],0x852", '81940B6754343252080000') #no rex                     
    doTest("add DWORD PTR [r10+rcx*1+0x32345467],0x852", '4181840A6754343252080000')
    
    # immediate_to_reg 
    doTest("add rbx,8", '4883C308')
    doTest("adc ecx,0x3", '83D103')
    doTest("adc r10,0x3523",  '4981D223350000')
    doTest("mov rbx,8", '48C7C308000000')

    doTest("add QWORD PTR [r12],rax", '49010424') # -> r12 alone in memory needs SIB

    doTest("imul rax,rbx", '480FAFC3')
    doTest("mov ax,bx", '6689D8')
    doTest("and al,rbx", '4921D9')
    doTest("cmp r8d,ebx", '4139D8')
    doTest("bsf r12,r11", '4D0FBCE3')
    doTest("add al,bl", '00D8')
    doTest("or rcx,rbx", '4809D9')
    doTest("test ecx,edx", '85D1')
    doTest("bsr bx,cx", '660FBDD9')
    doTest("adc rax,rbx", '4811D8')
    doTest("xor rcx,r12", '4C31E1')
    doTest("xchg rax,rbx", '4893')
    doTest("sub r9w,dx", '664129D1')
    doTest("sbb rax,rbx", '4819D8')
    doTest('xor bh,cl', '30CF')

 

    # immediate_to_AL_AX_EAX
    doTest("adc eax,32346321", '15D190ED01')
    doTest("adc eax,0x3333", '1533330000')  # less than 16 uses the normal r|imd
    doTest("adc rax,0x3523", '481523350000')
    doTest("add eax,0x33333333", '0533333333')

    doTest("shl rax,3", '48C1E003')
    doTest("shr ax,3", '66C1E803')
    doTest("shr bl,32",   'C0EB20')
    doTest("shr bl,0x32", 'C0EB32')
    doTest("shl QWORD PTR [rax],4", '48C12004')

    doTest("xadd rax,r10", '4C0FC1D0')
    doTest("xadd BYTE PTR [edx+0x1100],al", '670FC08200110000')
    doTest("xadd QWORD PTR [edx+0x1100],r10", '674C0FC19200110000')


    # exceptions: 

    # bsr/bsf
    doTest("bsr rax,rbx",  '480FBDC3') 
    doTest("bsf rax,rax",  '480FBCC0')
    doTest("bsf ax,bx",    '660FBCC3')
    doTest("bsf r10w,bx",  '66440FBCD3')    
    doTest("bsf r11,QWORD PTR [r8+r12*4+0x16]", '4F0FBC5CA016') 

    # xchg (r2a)
    doTest("xchg bx,ax", '6693')
    doTest("xchg eax,ebx", '93')
    doTest("xchg rbx,rax", '4893')
    doTest("xchg rax,rbx", '4893')

    # imul
    doTest("imul rax,rbx", '480FAFC3')
    doTest("imul bx,r10w", '66410FAFDA')
    doTest("imul rax,QWORD PTR [rbx]", '480FAF03')
    return
    
#Test0or1arg()
#Test2args()


'''
#print(DisAssembler('4D0FBCE3')) #"bsf r12,r11"


    doTest("imul rbx",'48F7EB')
    doTest("imul QWORD PTR [rbx]",'48F72B')


#one operand imul with register
#doTest("imul rbx",'48F7EB')
#print(DisAssembler('48F7EB'))

#one operand imul with memory
#doTest("imul QWORD PTR [rbx]",'48F72B')
#print(DisAssembler('48F72B'))

#two operand imul with register, register
#print(DisAssembler('480FAFC3')) #->"imul rax,rbx", '480FAFC3'
#print(DisAssembler('66440FAFD3')) #->"imul r10w,bx", '66440FAFD3'

#two operand imul with memory, register
#print(DisAssembler('460FAF542500')) #imul r10,QWORD PTR [rbx]


#print(DisAssembler('480FAFC3')) #->"imul rax,rbx", '480FAFC3'

# 11111100000010000000
#print(DisAssembler('410FC08000110000')) #->"xadd BYTE PTR [r8+0x1100],al"
#print(DisAssembler('418600')) #-> xchg BYTE PTR [r8],al'


#print(DisAssembler('4883C005')) #-> 'add rax,0x5'

#doTest("call rax", 'FFD0') -> call eax
# 6748F75500 "not QWORD PTR [ebp+0x0]"
# doTest("not DWORD PTR [ebp+0x32]",'67F75532')



    doTest("not QWORD PTR [rax]",'48F710')
    doTest("not DWORD PTR [ebx]",'67F713')
    doTest("not BYTE PTR [rax]",'F610')
    # indicret base=ebp(or rbp) -> mod=01, base=ebp, disp8=0
    doTest("not QWORD PTR [ebp]",'6748F75500')
    #with disp
    doTest("not DWORD PTR [ebp+0x32]",'67F75532')
    doTest("not DWORD PTR [ebp+0x3234]",'67F79534320000')
    doTest("not DWORD PTR [ebp+0x32345467]",'67F79567543432')
    doTest("not DWORD PTR [rbx+0x32345467]",'F79367543432')
    
#print(DisAssembler('4e0fbd5c2500')) #bsr r11,QWORD PTR [rbp+r12*1+0x0]
#print(DisAssembler('66ba5213')) #mov dx,0x1352

def doTest(input, output):
    bin = DisAssembler(input)
    print(bin)
    print(bin == output)

#"add eax,0x33333333"


#doTest('4889D8',"mov rax,rbx")
#doTest('48830001','add QWORD PTR [rax],0x1')
#doTest('4883C001','add rax,0x1')
# '4187CC'
# xchg r12d, ecx



doTest('FE840B67543432', 'inc BYTE PTR [rbx+rcx*1+0x32345467]')
doTest('48FF840B67543432', 'inc QWORD PTR [rbx+rcx*1+0x32345467]')
doTest('66FF840B67543432', 'inc WORD PTR [rbx+rcx*1+0x32345467]')
doTest('FF840B67543432', 'inc DWORD PTR [rbx+rcx*1+0x32345467]')
doTest('48FF840B67543432', 'inc QWORD PTR [rbx+rcx*1+0x32345467]')




# one arg imd
#doTest('C21800',"ret 0x18")
#doTest("6878090000",'push 0x978')

# one arg register
#doTest('48FFC0',"inc rax")
#doTest('49FFC2',"inc r10")
#doTest('41FEC2',"inc r10b")
#doTest('FEC0',"inc al")

# one arg memory 
#doTest('67F7944D67543432',"not DWORD PTR [ebp+ecx*2+0x32345467]")
#doTest('48F710',"not QWORD PTR [rax]")


# two arg r|r
doTest('4889D8',"mov rax,rbx")
# two arg m|r
doTest('410FC08000110000',"xadd BYTE PTR [r8+0x1100],al")
# two arg m|imd
doTest('81940B6754343252080000',"adc DWORD PTR [rbx+rcx*1+0x32345467],0x852") #no rex 
         -> no rex but knows that the registers are 64
            because in 32bit we would have addr_prefix=67,
            and we don't have 8/16 registers in memory!

doTest('6781940B6754343252080000',"adc DWORD PTR [ebx+ecx*1+0x32345467],0x852")
doTest('6781940B6754343252080000',"adc DWORD PTR [bx+cx*1+0x32345467],0x852")


# two arg r|imd   
doTest('4981D223350000',"adc r10,0x3523")
# two arg a|imd   
doTest('0533333333',"add eax,0x33333333")


#doTest('FEC0',"inc al")

#doTest('66FFC0',"inc ax")
#doTest('FFC0',"inc eax")
#doTest('48FFC0',"inc rax")
#doTest('49FFC2',"inc r10")     


# F728              -> imul DWORD PTR [rax]
# 42F62CA500000000  ->  imul BYTE PTR [r12*4+0x0]

#460FAF0CA500000000 -> imul r9d,DWORD PTR [r12*4+0x0]
#


# 0000111110101111 00 001 100 10 100 101 00000000000000000000000000000000
# 0000111110101111
#print(DisAssembler('4e0fbd5c2500')) #-> imul r9d,DWORD PTR [r12*4+0x0]

# 4e0fbd5c2500

'''