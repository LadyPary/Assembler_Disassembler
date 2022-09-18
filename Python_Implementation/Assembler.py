# Define dicts for fast and easy look ups
# We aim to fill the necessary fields for each assembly code!

# Here we keep the binary codes
sizes ={
    'BYTE':8,
    'WORD':16,
    'DWORD':32,
    'QWORD':64
}
# Registers
# 16/32/64 bit
registers = { 
    # old, if used with 64, set Rex.W or Rex.B fields as 0.
    "al": "000",
    "ax": "000",
    "ah": "100",
    "eax": "000",
    "rax": "000",

    "cl": "001",
    "cx": "001",
    "ch": "101",
    "ecx": "001",
    "rcx": "001",

    "dl": "010",
    "dx": "010",
    "dh": "110",
    "edx": "010",
    "rdx": "010",

    "bl": "011",
    "bx": "011",
    "bh": "111",
    "ebx": "011",
    "rbx": "011",

    "sp": "100",
    "esp": "100",
    "rsp": "100",

    "bp": "101",
    "ebp": "101",
    "rbp": "101",

    "si": "110",
    "esi": "110",
    "rsi": "110",

    "di": "111",
    "edi": "111",
    "rdi": "111",

    #new, set Rex fields as 1.
    "r8": "000",
    "r8d": "000",
    "r8w": "000",
    "r8b": "000",

    "r9": "001",
    "r9d": "001",
    "r9w": "001",
    "r9b": "001",

    "r10": "010",
    "r10d": "010",
    "r10w": "010",
    "r10b": "010",

    "r11": "011",
    "r11d": "011",
    "r11w": "011",
    "r11b": "011",

    "r12": "100",
    "r12d": "100",
    "r12w": "100",
    "r12b": "100",

    "r13": "101",
    "r13d": "101",
    "r13w": "101",
    "r13b": "101",

    "r14": "110",
    "r14d": "110",
    "r14w": "110",
    "r14b": "110",

    "r15": "111",
    "r15d": "111",
    "r15w": "111",
    "r15b": "111"
}

# Conditionals i.e. tnn
conditionals = {
    "o": "0000",
    "no":'0001',
    "b":'0010', "NAE":'0010',
    "nb":'0011', "AE":'0011',
    "e":'0100', "Z":'0100',
    "ne":'0101', "NZ":'0101',
    "be":'0110', 'NA':'0110',
    'nbe':'0111', 'A':'0111',
    's':'1000',
    'ns':'1001',
    'p':'1010', 'PE':'1010',
    'np':'1011', 'PO':'1011',
    'l':'1100', 'NGE':'1100',
    'nl':'1101', 'GE':'1101',
    'le':'1110', 'NG':'1110',
    'nle':'1111', 'G':'1111'
}

# Commands
noArgCommands = {
    "stc": "11111001",
    "std": "11111101",
    "clc": "11111000",
    "cld": "11111100",
    "syscall": "0000111100000101",
    "ret": "11000011"
}

oneArgCommands = {
    # in commands with just one argument,
    # the reg field is an opcode! 
    # That's why we use comm_arg.

    # for AL, AX, or EAX with register or
    #     AL, AX, or EAX with memory
    "imul": "1111011",
    "imul_reg": '101',

    "dec": "1111111",
    "dec_reg": "001",

    "inc": "1111111", 
    "inc_reg": "000",

    "push":{"register": "1111111",
            "memory":   "1111111",
            "imd": "011010"},
    "push_reg": "110", # for register and memory

    "pop": "10001111",
    "pop_reg": "000",

    "idiv": "1111011",
    "idiv_reg": "111",

    "jmp":{"direct": "11101001", 
           "indirect": "1111111"},
    "jmp_reg": "100", #only for indirect

    "jcc":{"disp8":"0111",
           "dispFull":"000011111000"}, #+tnn+disp

    "neg": "1111011",
    "neg_reg": "011",

    "not": "1111011",
    "not_reg": "010",

    #direct+dsip 
    #indirect+mod+call_reg+reg/rm
    "call":{"direct":   "11101000",
            "indirect": "1111111"},
    "call_reg":"010", #only for indirect

    "ret": "11000010", #+disp16 

    # 1 bit
    'shl':'1101000',
    'shl_reg':'100',

    'shr':'1101000',
    'shr_reg':'101'
}

twoArgCommands = {
    # commands with more than 1 operand

    'mov':{'r2r_m2r':'100010',
           'imd2r_imd2m':'110001'},
    'mov_reg':'000',

    'add':{'r2r_m2r':'000000',
           'imd2r_imd2m':'100000',
           'imd2a':'000001'},
    'add_reg':'000',

    'adc':{'r2r_m2r':'000100',
           'imd2r_imd2m':'100000',
           'imd2a':'000101'},
    'adc_reg':'010',

    'sub':{'r2r_m2r':'001010',
           'imd2r_imd2m':'100000',
           'imd2a':'001011'},
    'sub_reg':'101',

    'sbb':{'r2r_m2r':'000110',
           'imd2r_imd2m':'100000',
           'imd2a':'000111'},
    'sbb_reg':'011',

    'and':{'r2r_m2r':'001000',
           'imd2r_imd2m':'100000',
           'imd2a':'001001'},
    'and_reg':'100',

    'or':{'r2r_m2r':'000010',
           'imd2r_imd2m':'100000',
           'imd2a':'000011'},
    'or_reg':'001',

    'xor':{'r2r_m2r':'001100',
           'imd2r_imd2m':'100000',
           'imd2a':'001101'}, 
    'xor_reg':'110',

    'cmp':{'r2r_m2r':'001110',
           'imd2r_imd2m':'100000',
           'imd2a':'001111'},
    'cmp_reg':'111',

    'test':{'r2r_m2r':'100001',
           'imd2r_imd2m':'111101',
           'imd2a':'101010'},
    'test_reg':'000',

    'xchg':{'r2r_m2r':'1000011',
            'r2a':'10010' #+reg 
            },

    'xadd':{'r2r':'000011111100000', #+w+11+reg2+reg1
            'm2r':'000011111100000'  #+w+mod+reg+r/m
            },

    #has 2 operands!
    'imul':{'r2r':'000011111010111111', #reg1+reg2
            'm2r':'0000111110101111'   #mod reg r/m
        },

    # i bit
    'shl':'110000',
    'shl_reg':'100',
    'shr':'110000',
    'shr_reg':'101',

    # exceptions
                     
    'bsf':{'r2r':'000011111011110011', #+reg1+reg2
           'm2r':'0000111110111100'   #+mod+reg+r/m
           },
    'bsr':{'r2r':'000011111011110111', #+reg1+reg2
           'm2r':'0000111110111101'   #+mod+reg+r/m
           }
}

# helper functions
def bin2hex(bin):
    z_num = 0
    temp = bin 
    # convert binary code to hex
    while temp.startswith('0000'):
        temp = temp[4:]
        z_num+=1
    
    hex_ = '0'*z_num+(f'{int(bin, 2):X}')
    return hex_

def hex2dec(hex_):
    # convert hex code to decimal
    return (hex(int(hex_,base=16))[2:])

def dec2bin(num, n_bit):
    '''
    takes a decimal number, converts it to hex and
    extends it to the number of bits.
    Returns the number word by word from low to high byte in binary.

    example: 5682, 32 -> 82 56 00 00 -> 1000 0010 0101 0110 
               0 , 8  -> 00 
    '''
    retHex = ''

    if len(num)%2 == 1:
        num = '0'+num

    lenOrg = len(num)

    # reverse the num word by word
    while num!='':
        retHex = num[:2] + retHex
        num = num[2:]

    # hex to binary 
    retHex = bin(int(retHex, base=16))[2:]

    # this is used to add the left out 0's to the beginning
    # e.g.: 100 -> 0100
    retHex = '0'*(lenOrg*4-len(retHex))+retHex

    # extend to the desired bits
    zeros = (n_bit - len(retHex))
    retHex+= zeros*'0'

    return retHex

def getArgNum(rest):
    # returns the number of arguments
    if ' ' not in rest:
        # no args
        return 0
    elif ',' in rest:
        # two args
        return 2
    else: 
        # one arg
        return 1

def is64bit(oper, memory=''):
    if memory=='':
        if oper[0]=='r':
            return True
        else:
            return False
    else:
        new_regs=['r{}'.format(i) for i in range(8,16)]
        for reg in new_regs:
            if reg in memory:
                return True
        return False

def isNewReg(oper):
    if oper[0]=='r' and oper[1].isnumeric():
       return True
    else:
        return False

def getRegSize(reg):
    size = 0
    #for old registers
    if not reg[1].isnumeric():
        if reg[-1]=='l' or reg[-1]=='h':
            size = 8
        elif reg[0]=='e':
            size = 32
        elif reg[0]=='r':
            size = 64
        else:
            size = 16
    #for new registers
    else:
        if reg[-1]=='b':
            size = 8
        elif reg[-1]=='d':
            size = 32
        elif reg[-1]=='w':
            size = 16
        else:
            size = 64
    return size

def getDispSize(disp):
    # 0x0 to 0xFD -> 8 bit
    # 0xFF to ... -> 32 bit
    dec = int(hex2dec(disp[2:]))
    if dec<128:
        return 8
    return 32

def handleDisp(disp, bits):
    # convert a displacement to appropriate 
    # representation in memory then extends
    # it to the number of bits
    retDisp = ''

    # remove 0x
    disp = disp[2:]

    if len(disp)%2 == 1:
        disp = '0'+disp

    lenOrg = len(disp)

    # reverse the num word by word
    while disp!='':
        retDisp = disp[:2] + retDisp
        disp = disp[2:]

    # hex to binary 
    retDisp = bin(int(retDisp, base=16))[2:]

    # this is used to add the left out 0's to the beginning
    # e.g.: 100 -> 0100
    retDisp = '0'*(lenOrg*4-len(retDisp))+retDisp

    # extend to the desired bits
    zeros = (bits - len(retDisp))
    retDisp+= zeros*'0'

    return retDisp

def setW(arg, is64):
    if is64 and getRegSize(arg)!=8:
        return '1'
    elif getRegSize(arg)!=8:
        return '1'
    else:
        return '0'

def setSData(imd, noMemory=False):
    if imd.startswith('0x'):
        # 0x24
        imd = imd[2:]
    else: 
        # 24
        imd = hex2dec(imd)

    # is 8bit? #S
    if len(imd)<=2:
        s = '1'
    else: 
        s = '0'

    # 8 bit register 
    if not noMemory:
        data = dec2bin(imd, 32)
    else:
        # 16 bit imd -> 32 bit imd
        if len(imd)==4:
            data = dec2bin(imd, 32)
        else:
            data = dec2bin(imd, len(imd)*4)

    return s, data

def handleMemory(memory_opr, reg):
    '''
    takes a memory portion : QWORD PTR [rax]
    fills the binary code for "mod r/m", 
    "SIP" (if applicable), "disp",
    an address used for 67 prefix.
    And the name of all registers so that
    rex and prefix can use them later.
    '''
    res = {
        # mod r/m
        'w':'1',
        'mod':'',
        'rm':'',

        #SIB 
        'scale':'',
        'index':'',
        'base':'',

        #registers:
        'base_reg':'',
        'index_reg':'',

        #displacement
        'disp':'',

        # prefix helpers
        'oper_size':'',
        #address -> used for prefix 67
        #can be direct(rax) or indirect(0x5555551E)!
        'addr':''
    }

    def setScale(scale):
        if scale=='1':
            return '00'
        if scale=='2':
            return '01'
        if scale=='4':
            return '10'
        if scale=='8':
            return '11'

    oper_size, PTR, memory = memory_opr.split(" ")
    memory = memory.replace('[', "").replace(']','')
    #QWORD PTR rax

    # set operation size
    res['oper_size']=sizes[oper_size]

    # set code.w
    if res['oper_size']==8:
        res['w']='0'

    if ('*' not in memory) and (not memory[0].isnumeric()):
        #memory = [ebx] or [ebx+disp] or [ebx+ecx]

        # indirect no scale(one reg) -> no SIB
        if memory in registers:
            # [ebx]
            if memory=='r12':
                res['mod']= '00'
                res['rm']= '100'
                res['scale']= '00'
                res['index']= registers['r12']
                res['base']= registers['r12']
                res['addr']= 'r12'
                res['base_reg']='r12'
            
            elif memory=='rbp' or memory=='ebp':
                # indicret base=ebp(or rbp) -> mod=01, base=ebp, disp8=0
                res['mod']= '01'
                res['disp']=handleDisp('0x0',8)
                if memory=='rbp':
                    res['mod']= '10'
                    res['disp']=handleDisp('0x0',32)
            else:
                res['mod']= '00'

            res['rm']= registers[memory]
            res['base_reg']= memory
            res['addr']= memory
        elif (memory[memory.find('+')+1].isnumeric()):
            # [ebx+disp], [r8+disp]
            base, disp = memory.split('+')

            res['mod']= '10'

            if getDispSize(disp)==8:
                # 8bit disp
                res['mod']= '01'
                res['disp']=handleDisp(disp,8)
            else:
                res['mod']= '10'
                res['disp']=handleDisp(disp,32)
            
            ##
            #if getRegSize(base)==64 and len(disp[2:])<2:
            #    res['mod']= '10'
            #    res['disp']=handleDisp(disp,32)
            ##
            res['rm']= registers[base]
            res['base_reg']= base
            res['addr']= base

        else:
            # [ebx+ecx], [r8+r9]
            base, index = memory.split('+')

            if base == 'ebp' or base == 'rbp':
                if base == 'rbp':
                    res['mod']= '10'
                    res['disp']= handleDisp('0x0', 32)
                else:
                    res['mod']= '01'
                    res['disp']= handleDisp('0x0', 8)
            else:
                res['mod']= '00'

            res['rm']= '100'
            res['scale']= '00'
            res['index']= registers[index]
            res['base']= registers[base]
            res['addr']= base
            res['base_reg']=base
            res['index_reg']=index

    else:
        if memory[0].isnumeric():
            # direct must use SIB
            res['mod']= '00'
            res['rm']= '100'

            res['scale']= '00'
            res['index']= '100' #illegal
            res['base']= '101' #ebp 
            
            res['disp']= handleDisp(memory, 32)

            if res['oper_size']==64:
                res['base_reg']= 'rbp'
            else:
                res['base_reg']= 'ebp'

        # indirect no base only scale
        elif (memory[3]=='*' or memory[2]=='*'):
            res['mod']= '00'
            res['rm']= '100'

            index, rest = memory.split("*")
            res['index']= registers[index]
            res['index_reg']=index

            res['base']= '101'
            if res['oper_size']==64:
                res['base_reg']= 'rbp' #rbp
                res['addr']= index
            else:
                res['base_reg']= 'ebp' #ebp
                res['addr']= index

            if '+' in rest:
                # only scale and disp8/16/32 -> mod=00, base=ebp, disp8,disp16->disp32  -> e.g. ecx*8+0x06
                scale, disp = rest.split("+")
                res['scale']= setScale(scale)            
                res['disp']= handleDisp(disp, 32)

            else:
                # only scale -> mod=00, base=ebp, disp32=0 -> e.g. ecx*8
                res['scale']= setScale(rest)            
                res['disp']= handleDisp('0x0', 32)

        # indirect has base, index, scale
        else:
            base, rest = memory.split("+",1)
            index, rest = rest.split('*')       
            # base, index, rest = s or s+disp

            if '+' in rest:
                # rest = s+disp
                scale, disp = rest.split('+')
                # indirect has base, index, disp -> e.g. [ebp+ecx*2+0x32345467] 

                res['mod']= '10'
                
                if getDispSize(disp)==8:
                    # 8bit disp
                    res['mod']= '01'
                    res['disp']=handleDisp(disp,8)
                else:
                    res['mod']= '10'
                    res['disp']=handleDisp(disp,32)
                
                ##
                #if getRegSize(base)==64 and len(disp[2:])<2:
                #    res['mod']= '10'
                #    res['disp']=handleDisp(disp,32)
                ##

            else:
                # rest = s
                # indirect has base, index (check for base=rbp/ebp) -> e.g. [ebp+ecx*1]
                scale = rest
                
                if base=='rbp' or base=='ebp':
                    res['disp']=handleDisp('0x0',8)
                    res['mod']= '01'

                else:
                    res['mod']= '00'

            res['rm']= '100' #have SIB
            res['scale']=setScale(scale)
            res['index']= registers[index]
            res['base']= registers[base]
            res['base_reg']= base
            res['index_reg']= index
            res['addr']= base

    res_f = (res['w']+res['mod']+reg+res['rm']+\
             res['scale']+res['index']+res['base']+\
             res['disp'])

    return (res_f, res)

def setRexPrefix(oper1='', oper2='', oper3='', addr='', is64=False, oper_size='', mod=''):
    # r extends oper1 (reg/rm)
    # x extends oper2 (index) (if applicable)
    # b exentds oper3 (r/m or base)
    oper_prefix = ''
    addr_prefix = ''
    
    if oper_size==64:
        is64=True

    if is64:
        rex, w, r, x, b = '0100', '0',  '0', '0', '0'

        # set w
        # only 1 if we have 64 bit registers
        if mod=='m|imd':
            if (oper_size==64):
                w = '1'

        elif (oper_size==64):
              w = '1'

        elif oper_size=='' and\
             ((oper1!='' and getRegSize(oper1)==64) or \
             (oper2!='' and getRegSize(oper2)==64) or \
             (oper3!='' and getRegSize(oper3)==64)):
                w = '1'

        # set r,x,b for new operands
        if oper1!='' and  isNewReg(oper1):
            r='1'
        if oper2!='' and isNewReg(oper2):
            x='1'
        if oper3!='' and isNewReg(oper3):
            b='1'

    # set prefix , oper_size = ?WORD
    if (not is64 or not int(w)):
        if (oper3!='') and (getRegSize(oper3)==16):
            oper_prefix = '66'
        elif (oper_size==16):
            oper_prefix = '66'

    # can be indirect(rax) or direct(0x5555551E)!
    # direct doesn't have prefix! 
    if (addr != '') and (getRegSize(addr)==32):
        addr_prefix='67'

    if is64:
        res = addr_prefix+oper_prefix+bin2hex(rex+w+r+x+b)
    else:
        res = addr_prefix+oper_prefix
    return res

def getOperType(arg1, arg2):
    '''
    types-> r|r:   register  | register
            r|m:   register  | memory
            m|r:   memory    | register

            r|imd: register | immediate
            m|imd: memory | immediate
            a|imd: al/ax/eax | immediate
    '''

    if (arg1 in registers) and ('PTR' in arg2):
        type_, opcodeKey, D = 'r|m', 'r2r_m2r', '1'

    elif ('PTR' in arg1) and  (arg2 in registers):
        type_, opcodeKey, D = 'm|r', 'r2r_m2r', '0'

    elif (arg2[0].isnumeric()) and ('PTR' in arg1):
        type_, opcodeKey, D = 'm|imd', 'imd2r_imd2m', '1'

    elif (arg2[0].isnumeric()) and ('a' in arg1):
        type_, opcodeKey, D = 'a|imd', 'imd2a', '0'

    elif (arg1 in registers) and (arg2 in registers):
        type_, opcodeKey, D = 'r|r', 'r2r_m2r', '0'

    elif (arg2[0].isnumeric()) and (arg1 in registers):
        type_, opcodeKey, D = 'r|imd', 'imd2r_imd2m', '0'


    return (type_, opcodeKey, D)

# functions to handle the commands
def handleNoArg(assemCommand):
    return bin2hex(noArgCommands[assemCommand])

def handleOneArg(assemCommand):
    returnCode = ""
    commandName, arg = assemCommand.split(" ", 1)

    if ("PTR" not in arg):
        if arg[0].isnumeric(): 
        # arg is an immediate address or data
            if commandName=="ret": 
                # e.g. ret 24 
                returnCode = oneArgCommands["ret"]
                if arg.startswith('0x'):
                    # ret 0x24
                    arg = arg[2:]
                else: 
                    # ret 24
                    arg = hex2dec(arg)
                returnCode+= dec2bin(arg, 16)
                returnCode = bin2hex(returnCode)

            elif (commandName=="call" or commandName=="jmp"):
                if commandName=="call":
                    #direct call e.g. call disp32
                    returnCode = oneArgCommands["call"]["direct"]
                else:
                    #direct jmp e.g. jmp disp32
                    returnCode = oneArgCommands["jmp"]["direct"]
                returnCode+= dec2bin("0", 32)
                returnCode = bin2hex(returnCode)

            elif commandName[0]=="j":
                # jcc 
                cond = commandName[1:]
                # full displacement
                returnCode = oneArgCommands["jcc"]["dispFull"]
                returnCode+= conditionals[cond] + dec2bin("0", 32)
                returnCode = bin2hex(returnCode)

            elif (commandName=="push"):
                #immediate push e.g. push 12 
                #0110 10s0 : immediate data
                returnCode = oneArgCommands["push"]["imd"]
                #0110 1010 0000 1100                                 s=1 8bit
                #0110 1000 0111 1000 0000 1001 '0000 0000 0000 0000' s=0 16bit
                #0110 1000 1111 1000 1110 1000  0111 0001 0000 0001  s=0 32bit

                if arg.startswith('0x'):
                    # push 0x24
                    arg = arg[2:]
                else: 
                    # push 24
                    arg = hex2dec(arg)

                # is 8bit? 
                if len(arg)<=2:
                    s = '1'
                    temp = dec2bin(arg, 8)
                else: 
                    s = '0'
                    temp = dec2bin(arg, 32)

                returnCode+= s + '0' + temp
                returnCode = bin2hex(returnCode)
        
        else:
            # arg is a register

            # Indirect call - Indirect jmp -  
            # NOT/NEG - IDIV - 
            # POP/PUSH(REG) - inc/dec
            # SHL/SHR
            
            # here we should fill all the fields!
            opcode, w, mod, reg, rm = '','','','',''

            # fill opcode
            if commandName=='push':
                opcode = oneArgCommands['push']['register']
            elif commandName=='call':
                opcode = oneArgCommands['call']['indirect']
            elif commandName=='jmp':
                opcode = oneArgCommands['jmp']['indirect']
            else:
            # other commands
                opcode = oneArgCommands[commandName]

            # fill w
            size_ = getRegSize(arg)
            if size_ == 8:
                w='0'
            else:
                w='1'

            # fill mod -> 11 because we have register
            mod = '11'

            # fill reg -> opcode
            reg = oneArgCommands['{}_reg'.format(commandName)]

            # fill r/m -> our register
            rm = registers[arg]

            returnCode=bin2hex(opcode+w+mod+reg+rm)

            # fill rex or prefix
            # we don't have jmp or call for 64bit!
            # so NO Rex! only prefix.

            # are we in the 64 bit mode?
            is64 = is64bit(arg)

            if (commandName=='jmp') or (commandName=='call'):
                if (not is64) and (getRegSize(arg)==16):
                    returnCode = '66'+returnCode
            else:
                returnCode = setRexPrefix(oper3=arg, is64=is64) + returnCode

    else:
        # we have memory!

        # Indirect call - Indirect jmp -  
        # NOT/NEG - IDIV - 
        # POP/PUSH(REG) - inc/dec

        # here we should fill all the fields!

            # fill opcode
            if commandName=='push':
                opcode = oneArgCommands['push']['memory']
            elif commandName=='call':
                opcode = oneArgCommands['call']['indirect']
            elif commandName=='jmp':
                opcode = oneArgCommands['jmp']['indirect']
            else:
            # other commands
                opcode = oneArgCommands[commandName]
            # fill reg -> opcode
            reg = oneArgCommands['{}_reg'.format(commandName)]

            rest, dict_ = handleMemory(arg, reg)

            returnCode = bin2hex(opcode + rest)
            # fill rex or prefix
            # we don't have jmp or call for 64bit!
            # so NO Rex! only prefix.

            # are we in the 64 bit mode?
            is64 = is64bit(arg, arg)
            returnCode = setRexPrefix(oper2=dict_['index_reg'], oper3=dict_['base_reg'], \
                                      addr=dict_['addr'], oper_size=dict_['oper_size'], \
                                      is64=is64) + returnCode

    return returnCode

def handleTwoArgsExcep(commandName, arg1, arg2):
    returnCode = ""
    # exceptions:
    #   bsr/bsf - imul
    #   xchg (r2a)

    type_, opcodeKey, D =getOperType(arg1, arg2)
    
    if commandName=='bsr' or commandName=='bsf' or commandName=='imul':
        if type_=='r|r':
            #r|r -> bsf[r2r]+reg1+reg2
            is64=(is64bit(arg1) or is64bit(arg2))
            returnCode+= twoArgCommands[commandName]['r2r']+registers[arg1]+registers[arg2]
            rex_prefix = setRexPrefix(oper1=arg1, oper3=arg2, is64=is64)
            returnCode = rex_prefix+bin2hex(returnCode)
        else:
            #r|m -> bsf[m2r]+mod+reg+r/m
            opcode= twoArgCommands[commandName]['m2r']
            is64=is64bit(arg1, arg2)
            reg= registers[arg1]
            rest, dict_ = handleMemory(arg2, reg)
            
            rex_prefix = setRexPrefix(oper1=arg1, oper2=dict_['index_reg'],\
                                        oper3=dict_['base_reg'], addr=dict_['addr'], \
                                        oper_size=dict_['oper_size'], is64=is64)        
            returnCode+= rex_prefix+bin2hex(opcode+rest[1:])

    elif commandName=='xchg':
        # xchg['r2a']+reg -> r|a
        if 'a' in arg1:
            arg1, arg2 = arg2, arg1

        is64=(is64bit(arg1) or is64bit(arg2))
        returnCode+= twoArgCommands[commandName]['r2a']+registers[arg1]
        rex_prefix = setRexPrefix(oper1=arg1, oper3=arg2,is64=is64)
        returnCode = rex_prefix+bin2hex(returnCode)

    return returnCode

def handleTwoArgs(assemCommand):
    returnCode = ""
    commandName, args = assemCommand.split(" ", 1)
    arg1, arg2 = args.split(",")

    if commandName in {'bsr','bsf','imul'} or\
       (commandName=='xchg' and (('a' in arg1) or ('a' in arg2))):
        returnCode = handleTwoArgsExcep(commandName, arg1, arg2)

    else:
        #general case
        type_, opcodeKey, D =getOperType(arg1, arg2)
        # type_: r|r , r|m, m|r, r|imd, m|imd, a|imd

        # opcode
        if commandName=='shl' or commandName=='shr':
            opcode = twoArgCommands[commandName]
        elif commandName=='xadd':
            if type_=='r|r':
                opcode = twoArgCommands[commandName]['r2r']
            else:
                opcode = twoArgCommands[commandName]['m2r']
        else:
         opcode = twoArgCommands[commandName][opcodeKey]

        if 'imd' in type_:
            # Have immediate -> r|imd, m|imd, a|imd
            if type_[0]=='m':
                # memory m|imd
                if commandName=='shl' or commandName=='shr':
                    if arg2=='0x1' or arg2=='1':
                        return handleOneArg(assemCommand[:assemCommand.find(',')])
                    else:
                        s = '0'
                        data = setSData(arg2, noMemory=True)[1]
                elif commandName=='mov':
                    s, data = setSData(arg2)
                else:
                    s, data = setSData(arg2, noMemory=True)

                # reg <- opcode, w mod rm sib disp
                reg= twoArgCommands['{}_reg'.format(commandName)]
                rest, dict_ = handleMemory(arg1, reg)

                #w
                is64=(arg1[arg1.find(' ')+1]=='Q' or\
                      isNewReg(dict_['base_reg']) or\
                      (dict_['index_reg']!= '' and isNewReg(dict_['index_reg'])))
                w=setW(dict_['base_reg'], is64)

                rex_prefix = setRexPrefix(oper2=dict_['index_reg'],\
                                          oper3=dict_['base_reg'], addr=dict_['addr'], \
                                          oper_size=dict_['oper_size'], is64=is64, mod='m|imd') 
                returnCode+= rex_prefix+bin2hex(opcode+s+rest+data)

            elif type_[0]=='r' or\
                ((commandName=='shl' or commandName=='shr') and \
                  type_[0]=='a'):
                # register -> r|imd

                # s
                if commandName=='shl' or commandName=='shr':
                    if arg2=='0x1' or arg2=='1':
                        return handleOneArg(assemCommand[:assemCommand.find(',')])
                    else:
                        s = '0'
                        data = setSData(arg2, noMemory=True)[1]

                elif commandName=='mov':
                    s = '1'
                    data = setSData(arg2)[1]

                else:
                    s, data = setSData(arg2, noMemory=True)

                #w
                is64=is64bit(arg1)
                w=setW(arg1, is64)

                mod= '11'
                reg= twoArgCommands['{}_reg'.format(commandName)]
                rm= registers[arg1]

                rex_prefix = setRexPrefix(oper3=arg1, is64=is64)
                returnCode+= rex_prefix+bin2hex(opcode+s+w+mod+reg+rm+data)
                
                # imd data
            else: 
                # a|imd -> al/ax/eax -> opcode+0+w+imd

                # s
                s, data = setSData(arg2, noMemory=True)

                #w
                is64=is64bit(arg1)
                w=setW(arg1, is64)
                

                rex_prefix = setRexPrefix(oper3=arg1, is64=is64)

                returnCode+= rex_prefix+bin2hex(opcode+'0'+w+data)

        else:
            # No immediate -> r|r , r|m, m|r
            if type_=='r|r':
                #w
                is64=(is64bit(arg1) or is64bit(arg2))
                w=setW(arg1, is64)

                # reg <- arg2, mod=11, rm <- arg1
                mod='11'
                reg= registers[arg2]
                rm= registers[arg1]

                if commandName=='xadd' or commandName=='xchg':
                    D=''

                rex_prefix = setRexPrefix(oper1=arg2, oper3=arg1, is64=is64)
                returnCode+= rex_prefix+bin2hex(opcode+D+w+mod+reg+rm)
                
            elif type_=='r|m' or type_=='m|r':
                if type_=='m|r':
                    arg1, arg2 = arg2, arg1
                #w
                is64=is64bit(arg1, arg2)
                w=setW(arg1, is64)
                # reg <- arg1, w mod rm sib disp
                reg= registers[arg1]
                rest, dict_ = handleMemory(arg2, reg)

                rex_prefix = setRexPrefix(oper1=arg1, oper2=dict_['index_reg'],\
                                          oper3=dict_['base_reg'], addr=dict_['addr'], \
                                          oper_size=dict_['oper_size'], is64=is64)
                
                if commandName=='xadd':
                    D=''

                returnCode+= rex_prefix+bin2hex(opcode+D+rest)

    return returnCode

def Assembler(assemCommand):
    # storing the final binary code here 
    commandBin = ""

    # how many arguments does it have?
    n_arg = getArgNum(assemCommand)

    if n_arg == 0:
        commandBin = handleNoArg(assemCommand)
    elif n_arg == 1:
        commandBin = handleOneArg(assemCommand)
    else:
        commandBin = handleTwoArgs(assemCommand)

    return commandBin.lower()

print(Assembler(input()))
