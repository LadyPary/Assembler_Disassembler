# Define dicts for fast and easy look ups
import pprint

# Here we keep the binary codes
sizes ={
    8:'BYTE',
    16:'WORD',
    32:'DWORD',
    64:'QWORD'
}
# Registers
# 16/32/64 bit
registers = { 
    # old
    # 3 bit -> [8bit, 16bit, 32bit, 64bit]
    '000': ['al', 'ax', 'eax', 'rax'],
    '001': ['cl', 'cx', 'ecx', 'rcx'],
    '010': ['dl', 'dx', 'edx', 'rdx'],
    '011': ['bl', 'bx', 'ebx', 'rbx'],

    '100': ['ah', 'sp', 'esp', 'rsp'],
    '101': ['ch', 'bp', 'ebp', 'rbp'],
    '110': ['dh', 'si', 'esi', 'rsi'],
    '111': ['bh', 'di', 'edi', 'rdi'],

    # new
    # 4bit -> hax rex -> [8bit, 16bit, 32bit, 46bit]

    '1000': ['r8b',   'r8w',  'r8d',  'r8'],
    '1001': ['r9b',   'r9w',  'r9d',  'r9'],
    '1010': ['r10b', 'r10w', 'r10d', 'r10'],
    '1011': ['r11b', 'r11w', 'r11d', 'r11'],

    '1100': ['r12b', 'r12w', 'r12d', 'r12'],
    '1101': ['r13b', 'r13w', 'r13d', 'r13'],
    '1110': ['r14b', 'r14w', 'r14d', 'r14'],
    '1111': ['r15b', 'r15w', 'r15d', 'r15'],
}

# Conditionals i.e. tnn
conditionals = {
    "0000":"o",
    '0001':"no",
    '0010':"b",   #"nae"
    '0011':"nb",  #"ae"
    '0100':"e",   #"z"
    '0101':"ne",  #"nz"
    '0110':"be",  #'na'
    '0111':'nbe', #'z'
    '1000':'s',
    '1001':'ns',
    '1010':'p',   #'pe'
    '1011':'np',  #'po'
    '1100':'l',   #'nge'
    '1101':'nl',  #'ge'
    '1110':'le',  #'ng'
    '1111':'nle' #'g'
}

# Commands
noArgCommands = {
    "11111001":"stc",
    "11111101":"std",
    "11111000":"clc",
    "11111100":"cld",
    "11000011":"ret",
    "0000111100000101":"syscall"
}

oneArgCommands = {
    # in commands with just one argument,
    # the reg field is an opcode! 
    # That's why we use comm_arg.

    # for AL, AX, or EAX with register or
    #     AL, AX, or EAX with memory

    # 7 bits
    "1111011":{
        '101':"imul_reg",
        "111":"idiv_reg",
        "011":"neg_reg",
        "010":"not_reg" 
    },

    "1111111":{
        "001":"dec_reg",
        "000":"inc_reg",
        "011":"neg_reg",
        "010":"not_reg",
        "110":"push_reg", # for register and memory
        "100":"jmp_reg",  # only for indirect
        "010":"call_reg"  # only for indirect # +mod+call_reg+reg/rm
    },

    '1101000':{
        '100':'shl_reg',
        '101':'shr_reg'
        },

    '1000111':'pop_', # memory / reg
    "000":'pop_reg',

    ###############################################
    # 4 bit and 12 bit
    "0111":'jcc_disp8', #+tnn+disp8
    "000011111000":'jcc_dispFull', #+tnn+disp32

    # 6 bit
    "011010":"push_imd", #+imd

    # 8 bit
    "11000010":"ret_", #+disp16 

    # 8 bit 
    "11101001":"jmp_direct",   #+disp
    "11101000":"call_direct",  #+disp 
}

twoArgCommands = {
    # commands with more than 1 operand

    # 6 bits
    '100010': 'mov_r:r|m:r',
    '110001': 'mov_r:imd|m:imd', # 000 is the reg/op field in this case!

    #'mov_reg':'000',

    '000000': 'add_r:r|m:r',
    #'000001': 'add_a:imd',
    #'add_reg':'000',
    
    # dealing with immediates
            # reg/op
    '100000':{'000':'add_r:imd|m:imd', 
              '010':'adc_r:imd|m:imd',
              '101':'sub_r:imd|m:imd',
              '011':'sbb_r:imd|m:imd',
              '100':'and_r:imd|m:imd',
              '001': 'or_r:imd|m:imd',
              '110':'xor_r:imd|m:imd',
              '111':'cmp_r:imd|m:imd'},

    '000100': 'adc_r:r|m:r',
    #'000101': 'adc_a:imd',
    #'adc_reg':'010',

    '001010': 'sub_r:r|m:r',
    #'001011': 'sub_a:imd',
    #'sub_reg':'101',

    '000110': 'sbb_r:r|m:r',
    #'000111': 'sbb_a:imd',
    #'sbb_reg':'011',

    '001000': 'and_r:r|m:r',
    #'001001': 'and_a:imd',
    #'and_reg':'100',

    '000010': 'or_r:r|m:r',
    #'000011': 'or_a:imd', #imul and xadd and bsf/bsr starts with this!
    #'or_reg':'001',

    '001100': 'xor_r:r|m:r',
    #'001101': 'xor_a:imd',
    #'xor_reg':'110',

    '001110': 'cmp_r:r|m:r',
    #'001111': 'cmp_a:imd',
    #'cmp_reg':'111',

    '100001': {
        '0':'test_r:r|m:r', # D=0
        '1':'xchg_r:r|m:r', # D=1
        },

    #'101010': 'test_a:imd',
    '111101': 'test_r:imd|m:imd',
    #'test_reg':'000',

    '110000':{
        '100':'shl_r:imd', #this is the reg field!
        '101':'shr_r:imd',
    },
    ########################################
    # exceptions

    # 4 bit
    '1011': 'mov_r:imd', # +w+reg+imd

    # 5 bit 
    '10010':  'xchg_r:a', #+reg

    # 15 bit
    '000011111100000':"xadd_r:r|m:r",  
                                    #w+mod=11+reg1+reg2
                                    #w+mod+reg+r/m

    # 16 bit
    '0000111110101111':"imul_r:r|m:r",  #has 2 operands!
                                     #mod=11+reg1+reg2
                                     #+mod+reg+r/m

    # 16 bit
    '0000111110111100':"bsf_r:r|m:r", 
                                    #mod=11+reg1+reg2
                                    #+mod+reg+r/m
    '0000111110111101':"bsr_r:r|m:r" 
                                    #mod=11+reg1+reg2
                                    #+mod+reg+r/m
}

'''
    PART 1 : PARSING THE MACHINE CODE
             We aim to parse the machine code to fill each field.
'''

code = {

    'n_args':'',
    'specialCommand_0arg':'',
    'specialCommand_1arg':'',
    'specialCommand_2arg':'',

    # prefix -> 67 66
    'addr_prefix':'',
    'oper_prefix':'',

    # Rex -> 1 byte
    'rex':'',
    'rex.w':'',
    'rex.r':'',
    'rex.x':'',
    'rex.b':'',

    #OpCode -> 1 byte (MUST HAVE) 
    # could be: opcode(4bit 0111(jcc))+tnn(4bit) |or| opcode(12bit 000011111000(jcc))+tnn(4bit)
    'opcode':'', # 6 bit
    'tnn':'',    # none or 4bits
    'd/s':'',    # 1 bit
    'code.w':'', # 1 bit

    #mod r/m -> 1 byte
    'mod':'', # 2 bit
    'reg':'', # 3 bit
    'rm':'',  # 3 bit

    # SIB -> 1 byte
    'scale':'', # 2 bit
    'index':'', # 3 bit
    'base':'',  # 3 bit

    # displacement -> 0-4 byte
    'disp':'',

    # data -> 0-8 byte
    'data':''
}

# helper functions
def hex2bin(hex_):
    h_size = len(hex_) * 4
    return ((bin(int(hex_, 16))[2:]).zfill(h_size))

# functions that parse the hex
def hasPrefix(command):

    if command.startswith('67'):
        code['addr_prefix']='67'
        command = command[2:]
        if command.startswith('66'):
            code['oper_prefix']='66'
            command = command[2:]

    elif command.startswith('66'):
        code['oper_prefix']='66'
        command = command[2:]

    return command

def hasRex(command):
    # 48 
    if command.startswith('4'):
        # 0100 1000 
        hex_ = hex2bin(command)
        
        code['rex']= '4'
        code['rex.w']=hex_[4]
        code['rex.r']=hex_[5]
        code['rex.x']=hex_[6]
        code['rex.b']=hex_[7]
        command= command[2:]
    return command

def setOpcode(command):
    # in general cases it's 1 Byte
    commandBin=hex2bin(command)
    
    # special one arg commands  -> jcc/push_imd/ret/jmp_direct/call_direct
    #                               4, 6, 8, 12 bit
    for len in [4, 6, 8, 12]:
        if (commandBin[:len] in oneArgCommands):
            code['specialCommand_1arg']=oneArgCommands[commandBin[:len]]
            commandBin = commandBin[len:]
            code['n_args']=1

            commandBin = specialOneArgParse(commandBin)
            return commandBin

    # special two arg commands  -> xchg_r:a 
    #                              xadd_r:r|m:r , imul/bsf/bsr_r:r|m:r
    #                              5, 6, 7, 15, 16 bit
    for len in [4, 5, 15, 16]:
        if (commandBin[:len] in twoArgCommands):
            code['specialCommand_2arg']=twoArgCommands[commandBin[:len]]
            commandBin = commandBin[len:]
            code['n_args']=2

            commandBin = specialTwoArgParse(commandBin)
            return commandBin

    # normal zero arg command
    if commandBin in noArgCommands:
        #  zero arg commands -> all (no other opcode starts with them!)
        code['specialCommand_0arg']=noArgCommands[commandBin]
        code['n_args']=0
        return ''

    # normal one arg commands: reg <- command_reg 
    if (commandBin[:7] in oneArgCommands):
        code['opcode']=oneArgCommands[commandBin[:7]] # 7 bit
        code['d/s']=''    # 0 bit
        code['code.w']=commandBin[7] # 1 bit
        commandBin = commandBin[8:]
        code['n_args']=1
        return commandBin

    # normal two arg commands:  
    if (commandBin[:6] in twoArgCommands):
        code['opcode']=twoArgCommands[commandBin[:6]] # 6 bit
        code['d/s']=commandBin[6]    # 1 bit
        code['code.w']=commandBin[7] # 1 bit

        commandBin = commandBin[8:]
        code['n_args']=2
        return commandBin


def specialOneArgParse(commandBin):
    # commands that are in the form of opCode+Disp or opCode+Imd
    opcode = code['specialCommand_1arg']

    if opcode=='push_imd':
        #push_imd,  s0+immediate data
        code['d/s']=commandBin[0]
        code['code.w']=commandBin[1]
        code['data']=commandBin[2:]
        commandBin =''
        return commandBin

    elif opcode=="jcc_disp8" or opcode=="jcc_dispFull":
        #'jcc_disp8', #+tnn+disp8
        #'jcc_dispFull', #+tnn+disp32

        code['tnn']=commandBin[:4]
        commandBin = commandBin[4:]

        code['disp']=commandBin
        commandBin = ''

        return commandBin
    
    else:
        #ret / jmp|call_direct, #+disp
        code['disp']=commandBin
        commandBin = ''
        return commandBin

def specialTwoArgParse(commandBin):
    opcode = code['specialCommand_2arg']

    
    if opcode=='xchg_r:a':
        # 'xchg_r:a', #+reg
        code['reg']=commandBin
        return ''

    if opcode=='xadd_r:r|m:r':
    #  "xadd_r:r|m:r" 
    #       w+mod=11+reg1+reg2
    #       w+mod+reg+r/m
        code['code.w']= commandBin[0]
        ret = hasMod_RM(commandBin[1:])
        return ret   

    if opcode=='imul_r:r|m:r' or \
       opcode=='bsf_r:r|m:r' or \
       opcode=='bsr_r:r|m:r':
        # "imul_r:r|m:r" - bsf - bsr
        #       mod=11+reg1+reg2
        #       mod+reg+r/m
        if opcode=='bsf_r:r|m:r':
            code['code.w']='1'
            code['rex.w']='1'
            code['d/s']='0'
        elif opcode=='bsr_r:r|m:r':
            code['code.w']='1'
            code['rex.w']='1'
            code['d/s']='0'
        elif opcode=='imul_r:r|m:r':
            code['code.w']='1'
        ret = hasMod_RM(commandBin)
        return ret 
    
    if opcode=='mov_r:imd':
        #'1011': 'mov_r:imd', # +w+reg+imd
        code['code.w']=commandBin[0]
        code['reg']=commandBin[1:4]
        code['data']=commandBin[4:]
        return ''

def hasMod_RM(command):
    # also handels SIB and disp because
    # if we have SIB/disp, we must have Mod R/M

    # special cases where opcode+disp are handled elsewhere
    hasDisp=False

    code['mod']=command[:2]
    code['reg']=command[2:5]
    code['rm']=command[5:8]
    command = command[8:]

    if command=='' or code['rm']!='100' : # no SIB, could be "no memory" or "[reg]" or "[reg+disp]"
        if code['mod']=='10' or code['mod']=='01':
            # "[reg+disp]"
            code['disp']=command
            command=''
        return command

    elif code['mod']=='11':
        # no memory! reg is a register
        return command


    elif code['mod']=='00':
        if code['rm']=='100':
            # SIB with no displacement
            hasDisp=False 
        elif code['rm']=='101':
            # SIB for direct memory access
            hasDisp=True
            disp_size = 32
        else: 
            # no disp
            hasDisp=False
            return command

    elif code['mod']=='01':
        # 8 bit disp
        hasDisp=True
        disp_size = 8
    elif code['mod']=='10':
        hasDisp=True
        # 32 bit disp
        disp_size = 32

    # SIB
    code['scale']=command[:2]
    code['index']=command[2:5]
    code['base']=command[5:8]
    command = command[8:]

    if code['mod']=='00' and code['base']=='101' and command!='':
        hasDisp=True
        disp_size=len(command)
    if hasDisp:
        # Displacement
        code['disp']=command[:disp_size]
        command=command[disp_size:]

    return command

def hasData(command):
    code['data']=command
    command = command[len(code['data']):]
    return command

def Parse(hexString):
    # sets each field in code dictionary.
    hexString = hasPrefix(hexString)
    hexString = hasRex(hexString)

    hexString = setOpcode(hexString)

    if hexString=='':
        #zero arg command or special 1,2 arg and done.
        return
    
    # handels mod/rm, SIB, and disp
    hexString = hasMod_RM(hexString)

    if hexString == '':
        # no imd data
        return
    
    hexString=hasData(hexString)

    if hexString!='':
        raise Exception("WARNING: not parsed completely!")

    return

'''
    PART 2 : INTERPRETING THE PARSED CODE INTO ASSEMBLY CODE.
             We aim to use the parsed code to figure out the assembly commands and registers
'''

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

def reverseBits(num):
    r_num=''
    num = bin2hex(num)

    # reverse the num word by word
    while num!='':
        r_num = num[:2] + r_num
        num = num[2:]
    
    while r_num!='' and r_num[0]=='0':
        r_num = r_num[1:]

    if r_num=='':
        r_num='0'

    return '0x'+r_num

def getRegNameBySize(reg_loc, inMemory=False):
    # reg_loc = in which field is the register?  reg_loc=reg/rm/index/base
    size = 0

    def getIndex(size):
        if size==8:
            return 0 
        if size==16:
            return 1 
        if size==32:
            return 2
        if size==64:
            return 3

    if code['rex']!= '':
        # has rex -> 64 bit regs
        if code['code.w']=='0' and code['rex.w']=='0' and code['oper_prefix']=='':
            # 8 bit 
            size= 8 
        elif code['code.w']=='1' and code['rex.w']=='0' and code['oper_prefix']!='':
            # 16 bit 
            size= 16
        elif code['code.w']=='1' and code['rex.w']=='0' and code['oper_prefix']=='':
            # 32 bit 
            size= 32
        elif code['code.w']=='1' and code['rex.w']=='1' and code['oper_prefix']=='':
            # 64 bit 
            size= 64

        if inMemory:
                # in memory-> no  67: 64bit regs
                #             has 67: 32bit regs
            if code['addr_prefix']=='':
                size= 64
            else:
                size= 32

        # detect new registers
        if reg_loc=='reg' and code['rex.r']=='1': # extends reg
            reg_name= registers['1'+code['reg']][getIndex(size)]
        elif reg_loc=='index' and code['rex.x']=='1': # extends index
            reg_name= registers['1'+code['index']][getIndex(size)]
        elif (reg_loc=='base' or reg_loc=='rm') and code['rex.b']=='1':
            if reg_loc=='base' and code['base']!='': # extends base
                reg_name= registers['1'+code['base']][getIndex(size)]
            elif reg_loc=='rm': # extends rm
                reg_name= registers['1'+code['rm']][getIndex(size)]
        # old register
        else:
            reg_name= registers[code[reg_loc]][getIndex(size)]

    else:
        # no rex -> 8/16/32 bit regs
        if code['code.w']=='0' and code['oper_prefix']=='':
            # 8 bit 
            size= 8 
        elif code['code.w']=='1' and code['oper_prefix']!='':
            # 16 bit 
            size= 16
        elif code['code.w']=='1' and code['oper_prefix']=='':
            # 32 bit 
            size= 32

        if inMemory:
            # in memory-> no  67: 64bit regs
            #             has 67: 32bit regs
            if code['addr_prefix']=='':
                size= 64
            else:
                size= 32
        
        reg_name= registers[code[reg_loc]][getIndex(size)]

    return reg_name

def getOperSize():
    if code['rex']!='':
        if code['rex.w']=='0' and code['code.w']=='0' and code['oper_prefix']=='': 
            return 'BYTE'
        elif code['rex.w']=='0' and code['code.w']=='1' and code['oper_prefix']!='': 
            return 'WORD'
        elif code['rex.w']=='0' and code['code.w']=='1' and code['oper_prefix']=='': 
            return 'DWORD'
        elif code['rex.w']=='1' and code['code.w']=='1' and code['oper_prefix']=='': 
            return 'QWORD'
    else:
        if code['code.w']=='0' and code['oper_prefix']=='': 
            return 'BYTE'
        elif code['code.w']=='1' and code['oper_prefix']!='': 
            return 'WORD'
        elif code['code.w']=='1' and code['oper_prefix']=='': 
            return 'DWORD'

def handleMemory():

    def getScale():
        if code['scale']=='00':
            return '1'
        if code['scale']=='01':
            return '2'   
        if code['scale']=='10':
            return '4'
        if code['scale']=='11':
            return '8'  

    if code['base']=='':
        # NO SIB: indirect addr with one reg (except r12)
        if code['disp']=='0x' or code['disp']=='':
            # ONE REG, NO DISP  -> [rax]
            if code['mod']!='00':
                #rbp or ebp are base
                return ("{reg}+0x0").format(reg=getRegNameBySize('rm',inMemory=True))
            else:   
                return getRegNameBySize('rm', inMemory=True)
        else:
            # ONE REG, HAS DISP -> [ebp+0x32]
            return ("{reg}+{disp}").format(reg=getRegNameBySize('rm',inMemory=True),\
                                           disp=code['disp'])           
    else:
        hasBase=False
        hasIndex=True
        hasDisp=False

        # HAS SIB
        if code['base']!='101':
            # HAS BASE other than rbp\ebp
            hasBase=True
            if code['rex.b']=='1' and code['base']=='100' and code['scale']=='00' and code['index']=='100':
                #we have [r12]
                return "r12"
            if code['index']=='100' and code['scale']=='00' and (code['rex.x']=='0' or code['rex.x']==''):
                # dosen't have Index 
                hasIndex=False
            if code['mod']!='00':
                hasDisp=True

        else:
            # base = 101 -> mod=00->  NO BASE
            #            -> mod!=00-> rbp IS BASE
            if code['mod']=='00':
                #NO BASE
                hasBase=False
                hasDisp=True
            else:
                #rbp IS BASE
                hasBase=True
                hasDisp=True

            if code['index']=='100' and code['scale']=='00' and (code['rex.x']=='0' or code['rex.x']==''):
                # dosen't have Index 
                hasIndex=False

        if (not hasBase) and (not hasIndex):
            # Direct addressing             -> [0x5555551E]  #mod=00, rm=100, base=101, scale=00, index=100
            return "{disp}".format(disp=code['disp'])
        if (not hasBase) and (hasIndex):
            if not hasDisp:
                # NO BASE,  HAS INDEX, NO DISP  -> [ecx*1]       #mod=00, base=101 ,rm=101, disp32=0
                return "{index}*{scale}".format(index=getRegNameBySize('index',inMemory=True),\
                                                scale=getScale())
            else:
                 # NO BASE,  HAX INDEX, HAS DISP -> [ecx*1+0x12]  #mod=00, base=101 ,rm=101, disp8->disp32
                return "{index}*{scale}+{disp}".format(index=getRegNameBySize('index',inMemory=True),\
                                                scale=getScale(),\
                                                disp=code['disp'])

        if (hasBase) and (hasIndex) and (not hasDisp):
            # HAS BASE, HAS INDEX, NO DISP  -> [rbx+rcx*1]
            return "{base}+{index}*{scale}".format(base=getRegNameBySize('base',inMemory=True),\
                                                   index=getRegNameBySize('index',inMemory=True),\
                                                   scale=getScale())
        else:
            # HAS BASE, HAS INDEX, HAS DISP -> [ebp+ecx*2+0x32345467]
            return "{base}+{index}*{scale}+{disp}".format(base=getRegNameBySize('base',inMemory=True),\
                                                          index=getRegNameBySize('index',inMemory=True),\
                                                          scale=getScale(),\
                                                          disp=code['disp'])
    
# functions to interprete the dictionary 
def handleOneArg():
    machineCode = ''

    #special case
    if code['specialCommand_1arg']!='':
        commandName, inst = code['specialCommand_1arg'].split('_')
        if commandName=='jcc':
            machineCode = "j{tnn} {disp}".format(tnn= conditionals[code['tnn']],\
                                                 disp= code['disp']) 
        elif commandName=='jmp' or commandName=='call' or commandName=='ret':
            machineCode = "{commandName} {disp}".format(commandName=commandName,\
                                                        disp= code['disp']) 
        elif commandName=='push':
            machineCode = "{commandName} {imd}".format(commandName=commandName,\
                                                        imd= code['data']) 

    else:
        # in commands with just one argument, the reg field is an opcode! 
        # use code[reg]=command_reg to identify the command

        # arg can't be imd

        commandName = code['opcode']
        if type(commandName)==dict:
            commandName = commandName[code['reg']].split('_')[0]

        if code['mod']=='11':
            # arg is register -> reg is in rm

            if commandName=='shl' or commandName=='shr':
                return "{commandName} {reg},1".format(commandName=commandName, \
                                                    reg= getRegNameBySize('rm'))
            else:
                machineCode = "{commandName} {reg}".format(commandName=commandName, \
                                                        reg= getRegNameBySize('rm'))

        else:
            # arg is memory ->
            if commandName=='shl' or commandName=='shr':
                machineCode = "{commandName} {oper_size} PTR [{memory}],1".format(commandName=commandName, \
                                                                                oper_size= getOperSize(),\
                                                                                memory= handleMemory())
            else:
                machineCode = "{commandName} {oper_size} PTR [{memory}]".format(commandName=commandName, \
                                                                                oper_size= getOperSize(),\
                                                                                memory= handleMemory())
    return machineCode

def handleTwoArg():
    machineCode=''

    #special case
    if code['specialCommand_2arg']!='':
        commandName, inst = code['specialCommand_2arg'].split('_')
        if commandName=='mov':
            # w reg : immediate data
            return "mov {reg},{imd}".format(reg=getRegNameBySize('reg'),\
                                            imd=code['data'])

        if commandName=='xchg':
            #'xchg_r:a', #+reg
            return "xchg {reg}".format(reg=getRegNameBySize('reg'))
        else:
            # xadd/imul/bsf/bsr  _  r:r|m:r
            #       -> #mod=11+reg1+reg2
            #       -> #mod+reg+r/m
            if code['mod']=='11':
                #r:r 
                if commandName=='imul' or commandName=='bsf' or commandName=='bsr':
                    return "{commandName} {reg},{rm}".format(commandName=commandName, \
                                                            rm= getRegNameBySize('rm'),\
                                                            reg= getRegNameBySize('reg'))
                else:
                    return "{commandName} {rm},{reg}".format(commandName=commandName, \
                                                            rm= getRegNameBySize('rm'),\
                                                            reg= getRegNameBySize('reg'))
            elif commandName=='xadd':
                # m:r
                return "{commandName} {oper_size} PTR [{memory}],{reg}".format(commandName=commandName, \
                                                                                oper_size= getOperSize(),\
                                                                                memory= handleMemory(),\
                                                                                reg=getRegNameBySize('reg'))
            else:
                # r:m
                return "{commandName} {reg},{oper_size} PTR [{memory}]".format(commandName=commandName, \
                                                                                oper_size= getOperSize(),\
                                                                                memory= handleMemory(),\
                                                                                reg=getRegNameBySize('reg'))

    #general case
    #   use opcode[reg]=command_reg to identify the command
    commandName = code['opcode']
    if type(commandName)==dict:
        if '0'in code['opcode'].keys():
            # differ test D=0 and xchg D=1
            commandName, inst = commandName[code['d/s']].split('_')

        else:
            commandName, inst = commandName[code['reg']].split('_')
    else:
        commandName, inst = commandName.split('_')
#   r:r|m:r
    if inst=='r:r|m:r':
        if code['mod']=='11':
            #r:r
            return "{commandName} {rm},{reg}".format(commandName=commandName, \
                                                     rm= getRegNameBySize('rm'),\
                                                     reg= getRegNameBySize('reg'))                               
        else:
            if commandName=='xadd' or commandName=='xchg':
                code['d/s']='0'
            if code['d/s']=='0':
                # m:r
                return "{commandName} {oper_size} PTR [{memory}],{reg}".format(commandName=commandName, \
                                                                                oper_size= getOperSize(),\
                                                                                memory= handleMemory(),\
                                                                                reg=getRegNameBySize('reg'))
            else:
                # r:m
                return "{commandName} {reg},{oper_size} PTR [{memory}]".format(commandName=commandName, \
                                                                                oper_size= getOperSize(),\
                                                                                memory= handleMemory(),\
                                                                                reg=getRegNameBySize('reg'))
            #m:r

    if 'imd' in inst:
    # HAS IMD
        if code['mod']=='11':
            if inst=='a:imd':
                #a:imd
                return "{commandName} {rm},{imd}".format(commandName=commandName, \
                                            rm= getRegNameBySize('rm'),\
                                            imd= code['data'])
            else:
                # r:imd
                return "{commandName} {rm},{imd}".format(commandName=commandName, \
                                                            rm= getRegNameBySize('rm'),\
                                                            imd= code['data'])
        else:
            # m:imd
            return "{commandName} {oper_size} PTR [{memory}],{imd}".format(commandName=commandName, \
                                                                            oper_size= getOperSize(),\
                                                                            memory= handleMemory(),\
                                                                            imd= code['data'])
    return machineCode


def DisAssembler(binary):
    Parse(binary)
    #now figure it out! 

    # exceptions:

    # zero arg commands 
    if code['specialCommand_0arg']!='':
        return(code['specialCommand_0arg'])

    if code['disp']!='':
        # binary disp to reversed hex disp
        code['disp']=reverseBits(code['disp']).lower()
    if code['data']!='':
        # binary imd to reversed hex imd
        code['data']=reverseBits(code['data']).lower()

    # one arg commands 
    if code['n_args']==1:
        return handleOneArg()
    
    # two arg commands 
    if code['n_args']==2:
        return handleTwoArg()

    return

print(DisAssembler(input()))

# 67835B0A0C fix this! does not print the imd
