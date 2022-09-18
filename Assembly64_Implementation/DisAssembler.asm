%include "in_out.asm"

section .data
    ;binary  db '01100111011001101000111100000000',0

    msg  db  "Please input the absolute address to the file! like: /home/ladypary/Documents/Assembly/test/", NL,0
    msg2 db  "Please input the file's name! like: DisAssembler_Testcases", NL,0

    dir      db '                                                                                                                                                      ',0 
    dir_size dq 0
    fileName db '                                                             ',0 

    ;space    db ' ',0
    newLine_ db 0x0a 

    file_name_output db 'output.txt',0

    test_fdesc   dq 0     ; file descriptor of test case file
    output_fdesc    dq 0  ; file descriptor of output file

    ; syscalls
    sys_open           equ     2
    sys_read           equ     0
    sys_write          equ     1

    ; access mode
    O_RDWR          equ     0q000002
    O_CREAT         equ     0q000100
    O_APPEND        equ     0q002000

    ; create permission mode
    sys_IRUSR     equ     0q400      ; user read permission
    sys_IWUSR     equ     0q200      ; user write permission

    ; msgs
    error_close     db     "error in closing file        ", NL, 0
    error_write     db     "error in writing file        ", NL, 0
    error_open      db     "error in opening file        ", NL, 0
    error_read      db     "error in reading file        ", NL, 0

    suces_close    db      "file closed                     ", NL, 0
    suces_write    db      "written to file                 ", NL, 0
    suces_open     db      "file opened for R/W             ", NL, 0
    suces_read     db      "reading file                    ", NL, 0


    cantHandle db "can't handle this command.",0x0a,0  
    successMSG db "CORRECT.",0x0a,0  
    failMSG    db "WRONG.",0x0a,0  

    binary db "",0,0x0a
    binary_space db "                                                                          ",0
    binary_space2 db "                                                                          ",0

    command db '',0
    space_comm db '                                                                          ',0
;---------------------------------------
;----------CODE-------------------------------
    n_args dq 0 ; 0 or 1 or 2
    specialCommand_0arg db '               ',0
    specialCommand_1arg db '               ',0
    specialCommand_2arg db '               ',0

    ; prefix -> 67 66
    addr_prefixF dq 0 ; 0 means no, 1 means has
    oper_prefixF dq 0 ; 0 means no, 1 means has

    ; Rex -> 1 byte
    rexF  dq  0 ; 0 means no, 1 means has
    rex   db '0100',0
    rex_w db ' ',0
    rex_r db ' ',0
    rex_x db ' ',0
    rex_b db ' ',0

    ;OpCode -> 1 byte (MUST HAVE) 
    opcode db '              ',0; is command's name
    d_s    db ' ',0    ; 1 bit
    code_w db ' ',0    ; 1 bit

    ;mod r/m -> 1 byte
    mod db '  ',0  ; 2 bit
    mod11 db '11',0  ; 2 bit
    mod10 db '10',0  ; 2 bit
    mod01 db '01',0  ; 2 bit
    mod00 db '00',0  ; 2 bit

    reg    db '   ',0 ; 3 bit
    rm     db '   ',0 ; 3 bit
    rm100  db '100',0 ; 3 bit
    rm101  db '101',0 ; 3 bit

    ; SIB -> 1 byte
    scale    db '  ',0  ; 2 bit
    scaleNum db ' ',0   ; 1 bit
    scale00  db '00',0   ; 2 bit
    scale01  db '01',0   ; 2 bit
    scale10  db '10',0   ; 2 bit
    scale11  db '11',0   ; 2 bit

    index    db '   ',0  ; 3 bit
    index100 db '100',0  ; 3 bit

    base     db '   ',0  ; 3 bit
    base100  db '100',0  ; 3 bit
    base101  db '101',0  ; 3 bit

    ; displacement -> 0-4 byte
    disp db '                                                                                ',0

    ; imd -> 0-8 byte
    imd db '                                                                                 ',0
    ;-------------------------------------------------
;----------Messages-------------------------------
    msgWarning db "WARNING: not parsed completely!",0
    ;-------------------------------------------------
;----------prefixes-------------------------------
    addr_prefix db "01100111",0 // 67
    oper_prefix db "01100110",0 // 66
    ;-------------------------------------------------
    commandTypeF dq 0
    ; 1: zero arg command
    ; 2: normal one arg command
    ; normal two arg command: r2r, m2r-> 31, imd2r, imd2m-> 32
    ; 11: push_Imd, 12: call_direct, ret
    ; 21: xadd, 22: imul, 23: bsf, 24: bsr
    
    hasDispF dq 0 ; 0 is false, 1 is true
    
    commandFoundF dq 0 ; 0 is false, 1 is true

;----------CHARACTERS-------------------------------
    space  db ' ',0
    comma  db ',',0
    one    db '1',0
    ptr_   db 'PTR',0
    openB  db '[',0
    closeB db ']',0 
    plus   db '+',0
    mult   db '*',0
    hexIndicator        db "0x",0  

;---------------------------------------------------
    regNameTemp db '      ',0
    operSize db  '      ',0
    operSizeNum dq 0
    memoryMunch db '',0
    space_memory db '                                            ',0

    reversedTemp db '                                                         ',0
    reversedTempHex db '                                                         ',0
    inMemoryF dq 0 
;---------------------------------------------------
;+++++ Used As Dictionary ++++++++++++++++++++++++++++++++++++++++++++++++
    ;----- Sizes -------------------------------------------------------------
    PTR_        db  "PTR",0

    BYTE_        db  "BYTE",0
    WORD_        db  "WORD",0
    DWORD_       db  "DWORD",0
    QWORD_       db  "QWORD",0

    BYTE_n       dq  8
    WORD_n       dq  16
    DWORD_n      dq  32
    QWORD_n      dq  64

    ;----- Registers ---------------------------------------------------------
    
    ;old, if used with 64, set Rex.W or Rex.B fields as 0.
    al_    db  "al ",0
    ax_    db  "ax ",0
    ah_    db  "ah ",0
    eax_   db  "eax",0
    rax_   db  "rax",0

    cl_    db  "cl ",0
    cx_    db  "cx ",0
    ch_    db  "ch ",0
    ecx_   db  "ecx",0
    rcx_   db  "rcx",0

    dl_    db  "dl ",0
    dx_    db  "dx ",0
    dh_    db  "dh ",0
    edx_   db  "edx",0
    rdx_   db  "rdx",0

    bl_    db  "bl ",0
    bx_    db  "bx ",0
    bh_    db  "bh ",0
    ebx_   db  "ebx",0
    rbx_   db  "rbx",0

    sp_    db  "sp ",0
    esp_   db  "esp",0
    rsp_   db  "rsp",0 

    bp_    db  "bp ",0
    ebp_   db  "ebp",0
    rbp_   db  "rbp",0 

    si_    db  "si ",0
    esi_   db  "esi",0
    rsi_   db  "rsi",0 

    di_    db  "di ",0
    edi_   db  "edi",0
    rdi_   db  "rdi",0 

    ;new, set Rex fields as 1.
    r8_    db  "r8 ",0
    r8d_   db  "r8d",0
    r8w_   db  "r8w",0
    r8b_   db  "r8b",0

    r9_    db  "r9 ",0
    r9d_   db  "r9d",0
    r9w_   db  "r9w",0
    r9b_   db  "r9b",0

    r10_    db  "r10 ",0
    r10d_   db  "r10d",0
    r10w_   db  "r10w",0
    r10b_   db  "r10b",0

    r11_    db  "r11 ",0
    r11d_   db  "r11d",0
    r11w_   db  "r11w",0
    r11b_   db  "r11b",0

    r12_    db  "r12 ",0
    r12d_   db  "r12d",0
    r12w_   db  "r12w",0
    r12b_   db  "r12b",0

    r13_    db  "r13 ",0
    r13d_   db  "r13d",0
    r13w_   db  "r13w",0
    r13b_   db  "r13b",0

    r14_    db  "r14 ",0
    r14d_   db  "r14d",0
    r14w_   db  "r14w",0
    r14b_   db  "r14b",0

    r15_    db  "r15",0
    r15d_   db  "r15d",0
    r15w_   db  "r15w",0
    r15b_   db  "r15b",0

    ; al,  ax, eax, rax
    ; r8, r8d, r8w, r8b
    a_r8_bin    db  "000",0 ; excluding ah
    
    ; cl,  cx, ecx, rcx
    ; r9, r9d, r9w, r9b
    c_r9_bin    db  "001",0 ; excluding ch

    ; dl,  dx, edx, rdx
    ; r10, r10d, r10w, r10b
    d_r10_bin   db  "010",0 ; excluding dh

    ; bl,  bx, ebx, rbx
    ; r11, r11d, r11w, r11b
    b_r11_bin   db  "011",0 ; excluding bh
    
    ; ah
    ; sp, esp, rsp
    ; r12, r12d, r12w, r12b
    ah_sp_r12_bin  db  "100",0

    ; ch
    ; bp, ebp, rbp
    ; r13, r13d, r13w, r13b
    ch_bp_r13_bin  db  "101",0

    ; dh
    ; si, esi, rsi
    ; r14, r14d, r14w, r14b
    dh_si_r14_bin  db  "110",0

    ; bh
    ; di, edi, rdi
    ; r15, r15d, r15w, r15b
    bh_di_r15_bin  db  "111",0

    ;----- Commands ----------------------------------------------------------
    ; noArgCommands
    stc_        db  "stc",0
    std_        db  "std",0
    clc_        db  "clc",0
    cld_        db  "cld",0
    sys_        db  "syscall",0 ;syscall
    ret_        db  "ret",0
    ;...............................................
    stc_bin     db  "11111001",0
    std_bin     db  "11111101",0
    clc_bin     db  "11111000",0
    cld_bin     db  "11111100",0
    sys_bin     db  "0000111100000101",0 ;syscall
    ret_bin     db  "11000011",0
    ;--------------------------------------------------------
    ; oneArgCommands

    ;for AL, AX, or EAX with register/memory
    imul_       db  "imul",0 
    idiv_       db  "idiv",0

    dec_        db  "dec",0
    inc_        db  "inc",0

    push_       db  "push",0
    pop_        db  "pop",0 

    neg_        db  "neg",0
    not_        db  "not",0

    call_       db  "call",0

    shr_        db  "shr",0
    shl_        db  "shl",0
    ;...............................................
    imul_bin       db  "1111011",0 
    imul_reg       db  "101",0 

    idiv_bin       db  "1111011",0
    idiv_reg       db  "111",0 

    dec_bin        db  "1111111",0
    dec_reg        db  "001",0

    inc_bin        db  "1111111",0
    inc_reg        db  "000",0

    push_bin_reg   db  "01010",0 ; + reg code
    push_bin_mem   db  "1111111",0
    push_bin_imd   db  "011010",0 
    ;                dw
    ; imd 8:  011010 10
    ; imd 32: 011010 00
    push_reg       db  "110",0 ;for memory

    pop_bin_reg    db  "01011",0 ; + reg code
    pop_bin_mem    db  "1000111",0
    pop_reg        db  "000",0

    neg_bin        db  "1111011",0
    neg_reg        db  "011",0

    not_bin        db  "1111011",0
    not_reg        db  "010",0

    call_bin_direct    db  "11101000",0 ;+dsip 
    call_bin_indirect  db  "1111111",0  ;+mod+call_reg+reg/rm
    call_reg           db  "010",0 ;only for indirect
    
    ret_bin1       db  "11000010",0 ;+imd16(4 bytes fix)

    shr_bin        db  "1101000",0
    shr_reg        db  "101",0

    shl_bin        db  "1101000",0
    shl_reg        db  "100",0
    ;-----------------------------------------------
    ; twoArgCommands

    mov_           db  "mov",0 

    add_           db  "add",0
    adc_           db  "adc",0

    sub_           db  "sub",0
    sbb_           db  "sbb",0

    and_           db  "and",0 
    or_            db  "or",0
    xor_           db  "xor",0

    cmp_           db  "cmp",0
    test_          db  "test",0

    xchg_          db  "xchg",0
    xadd_          db  "xadd",0

    bsf_           db  "bsf",0
    bsr_           db  "bsr",0

    ;...............................................
    dummy_reg            db "   ",0
    
    mov_imd2r            db  '1011',0 ; +w+reg+imd
    mov_r2r_m2r          db  "100010",0 
    mov_imd2r_imd2m      db  "110001",0 
    mov_reg              db  "000",0 

    add_r2r_m2r          db  "000000",0
    add_imd2r_imd2m      db  "100000",0
    add_reg              db  "000",0

    adc_r2r_m2r          db  "000100",0
    adc_imd2r_imd2m      db  "100000",0
    adc_reg              db  "010",0

    sub_r2r_m2r          db  "001010",0
    sub_imd2r_imd2m      db  "100000",0
    sub_reg              db  "101",0

    sbb_r2r_m2r          db  "000110",0
    sbb_imd2r_imd2m      db  "100000",0
    sbb_reg              db  "011",0

    and_r2r_m2r          db  "001000",0 
    and_imd2r_imd2m      db  "100000",0 
    and_reg              db  "100",0 

    or_r2r_m2r           db  "000010",0
    or_imd2r_imd2m       db  "100000",0
    or_reg               db  "001",0

    xor_r2r_m2r          db  "001100",0
    xor_imd2r_imd2m      db  "100000",0
    xor_reg              db  "110",0

    cmp_r2r_m2r          db  "001110",0
    cmp_imd2r_imd2m      db  "100000",0
    cmp_reg              db  "111",0

    test_r2r_m2r         db  "100001",0
    test_imd2r_imd2m     db  "111101",0
    test_reg             db  "000",0

    xchg_r2r_m2r         db  "1000011",0 ;r2r_m2r

    xadd_r2r_m2r         db  "000011111100000",0
    ; r2r: +w+11+reg2+reg1
    ; m2r: +w+mod+reg+r/m

    ;has 2 operands!          
    imul_r2r_m2r         db  "0000111110101111",0 
    ; r2r: +11+reg1+reg2
    ; m2r: +mod+reg+r/m

    ; i bit shift, has 2 operands
    shr_bin2             db  "110000",0
    ;shr_reg             db  "101",0 same

    shl_bin2             db  "110000",0
    ;shl_reg             db  "100",0 same

    bsf_r2r_m2r               db  "0000111110111100",0   
    ;m:r-> opcode+mod+reg+r/m
    ;r:r-> opcode+11+reg1+reg2

    bsr_r2r_m2r               db  "0000111110111101",0 
    ;m:r-> opcode+mod+reg+r/m
    ;r:r-> opcode+11+reg1+reg2

    ;-----------------------------------------------
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    hexTemp    db  "                                                                                   ", 0
;----- bin to hex conversion --------------------------
    zero_hex db '0',0
    zero_bin db '0000',0

    one_hex db '1',0
    one_bin db '0001',0

    two_hex db '2',0
    two_bin db '0010',0

    three_hex db '3',0
    three_bin db '0011',0

    four_hex db '4',0
    four_bin db '0100',0

    five_hex db '5',0
    five_bin db '0101',0

    six_hex db '6',0
    six_bin db '0110',0

    seven_hex db '7',0
    seven_bin db '0111',0

    eight_hex db '8',0
    eight_bin db '1000',0

    nine_hex db '9',0
    nine_bin db '1001',0

    A_hex db 'a',0
    A_bin db '1010',0

    B_hex db 'b',0
    B_bin db '1011',0

    C_hex db 'c',0
    C_bin db '1100',0

    D_hex db 'd',0
    D_bin db '1101',0

    E_hex db 'e',0
    E_bin db '1110',0

    F_hex db 'f',0
    F_bin db '1111',0
    ;------------------------------------------------------
section .bss
    binary_buffer resb 1

section .text
    global _start

%macro appendString 2
    ; returns arg1 = arg1+arg2
    mov rdi, %1
    call GetStrlen ; len in rdx
    mov r15, rdx ; len of arg1

    mov rdi, %2
    call GetStrlen
    mov r14, rdx ; len of arg2

    lea rdi, [%1+r15]
    mov rsi, %2
    mov rcx, r14
    rep movsb

    mov al, 0 ; make it zero terminated
    stosb
%endmacro

%macro copyString 3
    ; copies lentgh arg3 of arg2 in arg1 
    lea rsi, %2
    lea rdi, %1
    mov rcx, %3
    rep movsb
    mov al, 0 ; make it zero terminated
    stosb
%endmacro
;---------------Start-------------------------;
;%if 0
_start:
    mov rsi, msg 
    call printString

    mov r14, dir
    call readString ; dir

    call newLine

    mov rsi, msg2 
    call printString

    mov r14, fileName
    call readString ; file name

    mov rdi, dir 
    call GetStrlen
    mov [dir_size], rdx
    mov r11, rdx
    lea r12, [fileName]
    mov r13, dir
    call pathPlusName
     
    ; open source file
    mov rdi, r14 
    call openFile
    mov [test_fdesc], rax
    

    ; create output file
    ; Open new file in samePath/output.txt
    ; r11: path size, r12: file name, r13: dir, r14: path+name
    mov r11, [dir_size]
    lea r12, [file_name_output]
    mov r13, dir
    call pathPlusName

    ;r14: path/name
    mov rdi, r14
    call openFile ; creates it!
    mov [output_fdesc], rax ; keep the file descriptor

    mov rcx, 1
    readBinary:
        ; read from source file
        ; rdi : file descriptor ; rsi : buffer ; rdx : length
        mov rdi, [test_fdesc]
        mov rsi, binary_buffer
        mov rdx, 1
        call readFile

        cmp rax, 0 ; bytes actually read
        je fullBinaryFound

        cmp BYTE [binary_buffer], 0x0a ; new line
        je fullBinaryFound

        appendString binary, binary_buffer
        jmp readBinary

        fullBinaryFound:
            push rax

            mov rsi, binary
            ;call printString
            ;call newLine

            call DoDisAssembler

            mov rsi, command
            ;call printString
            ;call newLine

            call writeToOutput

            ; reset binary 
            mov BYTE [binary], 0

            pop rax
            cmp rax, 0 ; bytes actually read
            je doneReadingBinarys

            jmp readBinary

    doneReadingBinarys:
        ; close files and exit 
        mov rdi, [test_fdesc]
        call closeFile
        mov rdi, [output_fdesc]
        call closeFile

exit:
    mov rax, sys_exit
    xor rdi, rdi
    syscall
;nasm -felf64 Assembler.asm && ld Assembler.o && ./a.out
;%endif

writeToOutput:
    ; write the machine code

    ; get it's len
    mov rdi, command
    call GetStrlen
    push rdx
    ; rdi : file descriptor ; rsi : buffer ; rdx : length
    mov rdi, [output_fdesc]
    mov rsi, command
    pop rdx
    call writeFile

    mov rdi, [output_fdesc]
    mov rsi, newLine_
    mov rdx, 1
    call writeFile

    ret
;------SYSCALLS----------------------------------------------
 readString:
    xor rax, rax
    xor rdi, rdi
    mov rsi, r14             ; string
    mov rdx, 1000000         ; number of bytes to read (upperbound)
    syscall 
    
    mov byte [rsi+rax-1], 0 ; add a zero ; number of bytes read in rax

    ret
 ;----------------------------------------------------
 ;----------------------------------------------------
 ; rdi: path/name ; rsi: file access mode 
 ; rdx: file permission, need in case of creating
 openFile:
    mov     rax, sys_open
    mov     rsi, O_RDWR | O_CREAT | O_APPEND
    mov     rdx, sys_IRUSR | sys_IWUSR
    syscall
    cmp     rax, -1    ; file descriptor in rax
    jle     openError
    mov     rsi, suces_open
    call    printString
    ret
 openError:
    mov     rsi, error_open
    call    printString
    ret
 ;----------------------------------------------------
 ;----------------------------------------------------
 ; r11: path size, r12: file name, r13: dir, r14: path+name
 pathPlusName:
    mov rdi, r12
    call GetStrlen ; len name in rdx
    mov rcx, rdx

    mov r14, r13

    mov rdx, r11
    lea rdi, [r14+rdx]
    mov rsi, r12

    rep movsb 

    mov byte [rdi], 0 ; add a zero

    ret
 ;----------------------------------------------------
 ;----------------------------------------------------
 ; rdi : file descriptor ; rsi : buffer ; rdx : length
 readFile:
    mov     rax, sys_read
    syscall
    cmp     rax, -1           ; number of read byte
    jle     readerror
    mov     byte [rsi+rax], 0 ; add a zero
    mov     rsi, suces_read
    ;call    printString
    ret
 readerror:
    mov     rsi, error_read
    call    printString
    ret
 ;----------------------------------------------------
 ;----------------------------------------------------
 ; rdi : file descriptor ; rsi : buffer ; rdx : length
 writeFile:
    mov     rax, sys_write
    syscall
    cmp     rax, -1         ; number of written byte
    jle     writeerror
    mov     rsi, suces_write
    ;call    printString
    ret
 writeerror:
    mov     rsi, error_write
    call    printString
    ret
 ;----------------------------------------------------
 ;----------------------------------------------------
 ; rdi : file descriptor
 closeFile:
    mov     rax, sys_close
    syscall
    cmp     rax, -1      ; 0 successful
    jle     closeerror
    mov     rsi, suces_close
    call    printString
    ret
 closeerror:
    call writeNum
    mov     rsi, error_close
    call    printString
    ret

 ;----------------------------------------------------

;++++++++++NEED MACROS+++++++++++++++++++++++++++++++++++++++++++

 %macro compStrings 4
    ; e.g. compStrings arg1, arg2, 3, isEqual
    mov rsi, %1
    mov rdi, %2
    mov rcx, %3
    repe cmpsb
    je %4
 %endmacro

 %macro removeBits 2
    ; e.g. removeBits arg1, n
    ; removes first n bits of arg 1
    lea rdi, [%1]
    call GetStrlen
    sub rdx, %2

    lea rsi, [%1+%2]
    lea rdi, [%1]
    mov rcx, rdx
    rep movsb
    mov al, 0 ; make it zero terminated
    stosb
 %endmacro

 %macro findCommand 7
    ; command_bin, number of bits, command_name, 
    ; number of args, where to keep name,
    ; command type, command reg

    ; command_bin start in r15,
    ; number of bits in r14, 
    ; command_name start in r13
    ; number of args in r12
    ; start of where to keep the command's name in r11
    ; command type flag in r10
    ; commnad reg in r9

    mov r15, %1
    mov r14, %2
    mov r13, %3
    mov r12, %4
    mov r11, %5
    mov r10, %6
    mov r9, %7

    call find_setOpcode

    cmp BYTE [commandFoundF], 1
    je contSetOpcode

 %endmacro


;++++++++++CONVERT+++++++++++++++++++++++++++++++++++++++++++++++
 %macro getHexDigit 3
    ; e.g. getHexDigit d_bin, isE ,d_hex
    lea rsi, [rbx + r8] ;r8(index on binary)
    mov rdi, %1
    mov rcx, 4
    repe cmpsb
    jne %2
    ; it was d
    add r8, 4
    mov rsi, %3
    lea rdi, [hexTemp + r9] ;r9(index on hex)
    inc r9
    movsb
 %endmacro
 bin2hex:
    ; start address of binary in rbx
    ; Converts a binary string to hex
    mov rdi, rbx   ; binary
    call GetStrlen ; len in rdx

    mov r15, rdx ; save len

    xor rax, rax
    mov rax, rdx
    xor rdx, rdx
    xor r14, r14
    mov r14, 4
    idiv r14 ; len/4
    cmp rdx, 0
    je preDoConversion

    ; len is not a multiple of 4
    ; do the first char with added zeros
    xor r8, r8 ; iterator on binary
    xor r9, r9 ; iterator on hex
    mov r14, 4
    sub r14, rdx 
    sub r8, r14
    jmp doConversion

    preDoConversion:
        xor r8, r8 ; iterator on binary
        xor r9, r9 ; iterator on hex
        doConversion:
            cmp r8, r15 ; len
            je exitBin2hex

            is0:
                getHexDigit zero_bin, is1 ,zero_hex
            is1:
                getHexDigit one_bin, is2 ,one_hex
            is2:
                getHexDigit two_bin, is3 ,two_hex
            is3:
                getHexDigit three_bin, is4 ,three_hex
            is4:
                getHexDigit four_bin, is5 ,four_hex
            is5:
                getHexDigit five_bin, is6 ,five_hex
            is6:
                getHexDigit six_bin, is7 ,six_hex

            is7:
                getHexDigit seven_bin, is8 ,seven_hex
            is8:
                getHexDigit eight_bin, is9 ,eight_hex
            is9:
                getHexDigit nine_bin, isA ,nine_hex
            isA:
                getHexDigit A_bin, isB ,A_hex
            isB:
                getHexDigit B_bin, isC ,B_hex
            isC:
                getHexDigit C_bin, isD ,C_hex
            isD:
                getHexDigit D_bin, isE ,D_hex
            isE:
                getHexDigit E_bin, isF ,E_hex
            isF:
                getHexDigit F_bin, doConversion ,F_hex

            jmp doConversion
    exitBin2hex:
        mov BYTE [hexTemp+r9], 0 ; make it zero terminated
        ret
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
reverseBits:
    ; start address in rsi, converts binary to hex and reverse
    ; in reversedTemp

    ;   bin2hex->       Start address of binary in rbx
    ;                   Converts a binary string to hex in reversedTempHex
    mov rbx, rsi 
    call bin2hex

    ;appendString  reversedTemp ,  hexIndicator
    ; check for one digit!
    mov r15, hexTemp

    checkOneDigit:
        ; if one digit like 9 -> return 9
        mov rdi, r15
        call GetStrlen
        cmp rdx, 1
        jne reverseCont
        copyString [reversedTempHex], [hexIndicator], 2
        copyString [reversedTempHex+2], [hexTemp], 1
        jmp exitReverseBits
    reverseCont:
    mov rdi, r15
    call GetStrlen ; len in rdx
    mov r13, rdx ; save len

    shr rdx,1 ; len/2

    reverse:
        lea rsi, [r15+r13-2] ; end of string
        mov rdi, reversedTemp
        mov rcx, rdx ; half the len
        cld
        reverseLoop:
            mov r8w, [rsi]
            mov [rdi], r8w
            sub rsi, 2
            add rdi, 2
            loop reverseLoop


    mov byte [rdi], 0 ; make null terminated
    removeLeading0s:
        cmp BYTE [reversedTemp], '0'
        jne add0x
        mov rdi, reversedTemp
        call GetStrlen
        cmp rdx, 1
        je add0x
        dec rdx
        copyString [reversedTemp], [reversedTemp+1], rdx
        jmp removeLeading0s
    add0x:
        copyString [reversedTempHex], [hexIndicator], 2
        mov rdi, reversedTemp
        call GetStrlen
        copyString [reversedTempHex+2], [reversedTemp], rdx
    exitReverseBits:
    ret

getRegNameBySize:
    ; reg in rm or reg or index or base
    ; reg binary start address in r15
    ; reg extend bit of rex in r14
    ; returns the reg name in regNameTemp
    call getOperSize
    cmp QWORD [inMemoryF], 0
    je cont__
    
    ; we are in memory, 67->32 else 64
    cmp qword [addr_prefixF], 1
    je is32_
    jne is64_
    is32_:
        mov qword [operSizeNum], 32
        jmp cont__
    is64_:
        mov qword [operSizeNum], 64
        jmp cont__
    cont__:
    compStrings r15, a_r8_bin,      3, is_a_r8_bin
    compStrings r15, c_r9_bin,      3, is_c_r9_bin
    compStrings r15, d_r10_bin,     3, is_d_r10_bin
    compStrings r15, b_r11_bin,     3, is_b_r11_bin
    compStrings r15, ah_sp_r12_bin, 3, is_ah_sp_r12_bin
    compStrings r15, ch_bp_r13_bin, 3, is_ch_bp_r13_bin
    compStrings r15, dh_si_r14_bin, 3, is_dh_si_r14_bin
    compStrings r15, bh_di_r15_bin, 3, is_bh_di_r15_bin

    is_a_r8_bin:
        ; al,  ax, eax, rax
        ; r8, r8d, r8w, r8b
        cmp byte [r14], '1'
        je is_r8_
        jne is_a_

        is_r8_:
            cmp qword [operSizeNum], 8
            je is_r8b
            cmp qword [operSizeNum], 16
            je is_r8w
            cmp qword [operSizeNum], 32
            je is_r8d
            cmp qword [operSizeNum], 64
            je is_r8

            is_r8b:
                copyString [regNameTemp], [r8b_], 3
                jmp exitGetRegNameBySize
            is_r8w:
                copyString [regNameTemp], [r8w_], 3
                jmp exitGetRegNameBySize
            is_r8d:
                copyString [regNameTemp], [r8d_], 3
                jmp exitGetRegNameBySize
            is_r8:
                copyString [regNameTemp], [r8_], 2
                jmp exitGetRegNameBySize

        is_a_:
            cmp qword [operSizeNum], 8
            je is_al
            cmp qword [operSizeNum], 16
            je is_ax
            cmp qword [operSizeNum], 32
            je is_eax
            cmp qword [operSizeNum], 64
            je is_rax
            is_al:
                copyString [regNameTemp], [al_], 2
                jmp exitGetRegNameBySize
            is_ax:
                copyString [regNameTemp], [ax_], 2
                jmp exitGetRegNameBySize
            is_eax:
                copyString [regNameTemp], [eax_], 3
                jmp exitGetRegNameBySize
            is_rax:
                copyString [regNameTemp], [rax_], 3
                jmp exitGetRegNameBySize

    is_c_r9_bin:
        ; cl,  cx, ecx, rcx
        ; r9, r9d, r9w, r9b
        cmp byte [r14], '1'
        je is_r9_
        jne is_c_

        is_r9_:
            cmp qword [operSizeNum], 8
            je is_r9b
            cmp qword [operSizeNum], 16
            je is_r9w
            cmp qword [operSizeNum], 32
            je is_r9d
            cmp qword [operSizeNum], 64
            je is_r9

            is_r9b:
                copyString [regNameTemp], [r9b_], 3
                jmp exitGetRegNameBySize
            is_r9w:
                copyString [regNameTemp], [r9w_], 3
                jmp exitGetRegNameBySize
            is_r9d:
                copyString [regNameTemp], [r9d_], 3
                jmp exitGetRegNameBySize
            is_r9:
                copyString [regNameTemp], [r9_], 2
                jmp exitGetRegNameBySize

        is_c_:
            cmp qword [operSizeNum], 8
            je is_cl
            cmp qword [operSizeNum], 16
            je is_cx
            cmp qword [operSizeNum], 32
            je is_ecx
            cmp qword [operSizeNum], 64
            je is_rcx
            is_cl:
                copyString [regNameTemp], [cl_], 2
                jmp exitGetRegNameBySize
            is_cx:
                copyString [regNameTemp], [cx_], 2
                jmp exitGetRegNameBySize
            is_ecx:
                copyString [regNameTemp], [ecx_], 3
                jmp exitGetRegNameBySize
            is_rcx:
                copyString [regNameTemp], [rcx_], 3
                jmp exitGetRegNameBySize
    is_d_r10_bin:
        ; dl,  dx, edx, rdx
        ; r10, r10d, r10w, r10b
        cmp byte [r14], '1'
        je is_r10_
        jne is_d_

        is_r10_:
            cmp qword [operSizeNum], 8
            je is_r10b
            cmp qword [operSizeNum], 16
            je is_r10w
            cmp qword [operSizeNum], 32
            je is_r10d
            cmp qword [operSizeNum], 64
            je is_r10

            is_r10b:
                copyString [regNameTemp], [r10b_], 4
                jmp exitGetRegNameBySize
            is_r10w:
                copyString [regNameTemp], [r10w_], 4
                jmp exitGetRegNameBySize
            is_r10d:
                copyString [regNameTemp], [r10d_], 4
                jmp exitGetRegNameBySize
            is_r10:
                copyString [regNameTemp], [r10_], 3
                jmp exitGetRegNameBySize

        is_d_:
            cmp qword [operSizeNum], 8
            je is_dl
            cmp qword [operSizeNum], 16
            je is_dx
            cmp qword [operSizeNum], 32
            je is_edx
            cmp qword [operSizeNum], 64
            je is_rdx
            is_dl:
                copyString [regNameTemp], [dl_], 2
                jmp exitGetRegNameBySize
            is_dx:
                copyString [regNameTemp], [dx_], 2
                jmp exitGetRegNameBySize
            is_edx:
                copyString [regNameTemp], [edx_], 3
                jmp exitGetRegNameBySize
            is_rdx:
                copyString [regNameTemp], [rdx_], 3
                jmp exitGetRegNameBySize
    is_b_r11_bin:
        ; bl,  bx, ebx, rbx
        ; r11, r11d, r11w, r11b
        cmp byte [r14], '1'
        je is_r11_
        jne is_b_

        is_r11_:
            cmp qword [operSizeNum], 8
            je is_r11b
            cmp qword [operSizeNum], 16
            je is_r11w
            cmp qword [operSizeNum], 32
            je is_r11d
            cmp qword [operSizeNum], 64
            je is_r11

            is_r11b:
                copyString [regNameTemp], [r11b_], 4
                jmp exitGetRegNameBySize
            is_r11w:
                copyString [regNameTemp], [r11w_], 4
                jmp exitGetRegNameBySize
            is_r11d:
                copyString [regNameTemp], [r11d_], 4
                jmp exitGetRegNameBySize
            is_r11:
                copyString [regNameTemp], [r11_], 3
                jmp exitGetRegNameBySize

        is_b_:
            cmp qword [operSizeNum], 8
            je is_bl
            cmp qword [operSizeNum], 16
            je is_bx
            cmp qword [operSizeNum], 32
            je is_ebx
            cmp qword [operSizeNum], 64
            je is_rbx
            is_bl:
                copyString [regNameTemp], [bl_], 2
                jmp exitGetRegNameBySize
            is_bx:
                copyString [regNameTemp], [bx_], 2
                jmp exitGetRegNameBySize
            is_ebx:
                copyString [regNameTemp], [ebx_], 3
                jmp exitGetRegNameBySize
            is_rbx:
                copyString [regNameTemp], [rbx_], 3
                jmp exitGetRegNameBySize

    is_ah_sp_r12_bin:
        ; ah
        ; sp, esp, rsp
        ; r12, r12d, r12w, r12b
        cmp byte [r14], '1'
        je is_r12_
        jne is_ah_sp_

        is_r12_:
            cmp qword [operSizeNum], 8
            je is_r12b
            cmp qword [operSizeNum], 16
            je is_r12w
            cmp qword [operSizeNum], 32
            je is_r12d
            cmp qword [operSizeNum], 64
            je is_r12

            is_r12b:
                copyString [regNameTemp], [r12b_], 4
                jmp exitGetRegNameBySize
            is_r12w:
                copyString [regNameTemp], [r12w_], 4
                jmp exitGetRegNameBySize
            is_r12d:
                copyString [regNameTemp], [r12d_], 4
                jmp exitGetRegNameBySize
            is_r12:
                copyString [regNameTemp], [r12_], 3
                jmp exitGetRegNameBySize

        is_ah_sp_:
            cmp qword [operSizeNum], 8
            je is_ah
            cmp qword [operSizeNum], 16
            je is_sp
            cmp qword [operSizeNum], 32
            je is_esp
            cmp qword [operSizeNum], 64
            je is_rsp
            is_ah:
                copyString [regNameTemp], [ah_], 2
                jmp exitGetRegNameBySize
            is_sp:
                copyString [regNameTemp], [sp_], 2
                jmp exitGetRegNameBySize
            is_esp:
                copyString [regNameTemp], [esp_], 3
                jmp exitGetRegNameBySize
            is_rsp:
                copyString [regNameTemp], [rsp_], 3
                jmp exitGetRegNameBySize
    is_ch_bp_r13_bin:
        ; ch
        ; bp, ebp, rbp
        ; r13, r13d, r13w, r13b
        cmp byte [r14], '1'
        je is_r13_
        jne is_ch_bp_

        is_r13_:
            cmp qword [operSizeNum], 8
            je is_r13b
            cmp qword [operSizeNum], 16
            je is_r13w
            cmp qword [operSizeNum], 32
            je is_r13d
            cmp qword [operSizeNum], 64
            je is_r13

            is_r13b:
                copyString [regNameTemp], [r13b_], 4
                jmp exitGetRegNameBySize
            is_r13w:
                copyString [regNameTemp], [r13w_], 4
                jmp exitGetRegNameBySize
            is_r13d:
                copyString [regNameTemp], [r13d_], 4
                jmp exitGetRegNameBySize
            is_r13:
                copyString [regNameTemp], [r13_], 3
                jmp exitGetRegNameBySize

        is_ch_bp_:
            cmp qword [operSizeNum], 8
            je is_ch
            cmp qword [operSizeNum], 16
            je is_bp
            cmp qword [operSizeNum], 32
            je is_ebp
            cmp qword [operSizeNum], 64
            je is_rbp
            is_ch:
                copyString [regNameTemp], [ch_], 2
                jmp exitGetRegNameBySize
            is_bp:
                copyString [regNameTemp], [bp_], 2
                jmp exitGetRegNameBySize
            is_ebp:
                copyString [regNameTemp], [ebp_], 3
                jmp exitGetRegNameBySize
            is_rbp:
                copyString [regNameTemp], [rbp_], 3
                jmp exitGetRegNameBySize
    is_dh_si_r14_bin:
        ; dh
        ; si, esi, rsi
        ; r14, r14d, r14w, r14b
        cmp byte [r14], '1'
        je is_r14_
        jne is_dh_si_

        is_r14_:
            cmp qword [operSizeNum], 8
            je is_r14b
            cmp qword [operSizeNum], 16
            je is_r14w
            cmp qword [operSizeNum], 32
            je is_r14d
            cmp qword [operSizeNum], 64
            je is_r14

            is_r14b:
                copyString [regNameTemp], [r14b_], 4
                jmp exitGetRegNameBySize
            is_r14w:
                copyString [regNameTemp], [r14w_], 4
                jmp exitGetRegNameBySize
            is_r14d:
                copyString [regNameTemp], [r14d_], 4
                jmp exitGetRegNameBySize
            is_r14:
                copyString [regNameTemp], [r14_], 3
                jmp exitGetRegNameBySize

        is_dh_si_:
            cmp qword [operSizeNum], 8
            je is_dh
            cmp qword [operSizeNum], 16
            je is_si
            cmp qword [operSizeNum], 32
            je is_esi
            cmp qword [operSizeNum], 64
            je is_rsi
            is_dh:
                copyString [regNameTemp], [dh_], 2
                jmp exitGetRegNameBySize
            is_si:
                copyString [regNameTemp], [si_], 2
                jmp exitGetRegNameBySize
            is_esi:
                copyString [regNameTemp], [esi_], 3
                jmp exitGetRegNameBySize
            is_rsi:
                copyString [regNameTemp], [rsi_], 3
                jmp exitGetRegNameBySize
    is_bh_di_r15_bin:
        ; bh
        ; di, edi, rdi
        ; r15, r15d, r15w, r15b
        cmp byte [r14], '1'
        je is_r15_
        jne is_bh_di_

        is_r15_:
            cmp qword [operSizeNum], 8
            je is_r15b
            cmp qword [operSizeNum], 16
            je is_r15w
            cmp qword [operSizeNum], 32
            je is_r15d
            cmp qword [operSizeNum], 64
            je is_r15

            is_r15b:
                copyString [regNameTemp], [r15b_], 4
                jmp exitGetRegNameBySize
            is_r15w:
                copyString [regNameTemp], [r15w_], 4
                jmp exitGetRegNameBySize
            is_r15d:
                copyString [regNameTemp], [r15d_], 4
                jmp exitGetRegNameBySize
            is_r15:
                copyString [regNameTemp], [r15_], 3
                jmp exitGetRegNameBySize

        is_bh_di_:
            cmp qword [operSizeNum], 8
            je is_bh
            cmp qword [operSizeNum], 16
            je is_di
            cmp qword [operSizeNum], 32
            je is_edi
            cmp qword [operSizeNum], 64
            je is_rdi
            is_bh:
                copyString [regNameTemp], [bh_], 2
                jmp exitGetRegNameBySize
            is_di:
                copyString [regNameTemp], [di_], 2
                jmp exitGetRegNameBySize
            is_edi:
                copyString [regNameTemp], [edi_], 3
                jmp exitGetRegNameBySize
            is_rdi:
                copyString [regNameTemp], [rdi_], 3
                jmp exitGetRegNameBySize
    exitGetRegNameBySize:  
        ret

getOperSize: 
    ; sets operation size in operSize
    cmp QWORD [rexF], 1
    je hasRex__
    jne noRex

    hasRex__:
        cmp byte [rex_w], '1'
        je setQWORD
    noRex:
        cmp byte [code_w], '1'
        jne setBYTE
        ;size16_32
        cmp qword [oper_prefixF],1 
        je setWORD
        jne setDWORD

    setBYTE:
        copyString [operSize], [BYTE_], 4
        mov QWORD [operSizeNum], 8
        jmp exitGetOperSize
    setWORD:
        copyString [operSize], [WORD_], 4
        mov QWORD [operSizeNum], 16
        jmp exitGetOperSize
    setDWORD:
        copyString [operSize], [DWORD_], 5
        mov QWORD [operSizeNum], 32
        jmp exitGetOperSize
    setQWORD:
        copyString [operSize], [QWORD_], 5
        mov QWORD [operSizeNum], 64
        jmp exitGetOperSize

    exitGetOperSize:
    ret

getScale:
    compStrings scale, scale00, 2, is_scale00
    compStrings scale, scale01, 2, is_scale01
    compStrings scale, scale10, 2, is_scale10
    compStrings scale, scale11, 2, is_scale11

    is_scale00:
        mov byte [scaleNum], '1'
        jmp exitGetScale
    is_scale01:
        mov byte [scaleNum], '2'
        jmp exitGetScale
    is_scale10:
        mov byte [scaleNum], '4'
        jmp exitGetScale
    is_scale11:
        mov byte [scaleNum], '8'
        jmp exitGetScale
    exitGetScale:
        ret
handleMemory:
    ; returns memory in memoryMunch!
    appendString  memoryMunch ,  openB
    
    cmp BYTE [base], ' '
    je noBase 
    jne hasBase
    noBase:
        ; NO SIB: indirect addr with one reg (except r12)
        cmp byte [disp], 0
        je noBase_noDisp
        jne noBase_hasDisp
        noBase_noDisp:
            ; ONE REG, NO DISP  -> [rax]
            mov QWORD [inMemoryF], 1
            mov r15, rm
            mov r14, rex_b
            call getRegNameBySize
            appendString  memoryMunch ,  regNameTemp 
            jmp exitHandleMemory
        noBase_hasDisp:
            ; ONE REG, HAS DISP -> [ebp+0x32]
            mov QWORD [inMemoryF], 1
            mov r15, rm
            mov r14, rex_b
            call getRegNameBySize
            appendString  memoryMunch ,  regNameTemp 
            appendString  memoryMunch ,  plus 
            appendString  memoryMunch ,  disp 
            jmp exitHandleMemory

    hasBase:
        xor r8, r8 ; has base flag
        mov r9, 1  ; has index flag
        
        compStrings base, base101, 3, baseIsRBP_EBP
        jmp baseIsNotRBP_EBP
        baseIsRBP_EBP:
        ;  base = 101 -> mod=00->  NO BASE
        ;             -> mod!=00-> rbp IS BASE
        compStrings mod, mod00, 2, no_base
        jmp has_base
        no_base:
            mov r8, 0
            jmp cont_baseIsRBP_EBP
        has_base:
            mov r8, 1
            jmp cont_baseIsRBP_EBP
        cont_baseIsRBP_EBP:
            compStrings index, index100, 3, check_index__
            jmp setMemory
            check_index__:
                compStrings scale, scale00, 2, check_index___
                jmp setMemory
                check_index___:
                    cmp byte [rex_w], ' '
                    je no_index_
                    cmp byte [rex_x], '0'
                    je no_index_
                    jne setMemory
                    no_index_:
                        mov r9, 0
                        jmp setMemory
        baseIsNotRBP_EBP:
            ; HAS BASE other than rbp\ebp
            mov r8, 1
            cmp byte [rex_b], '1'
            jne cont_baseIsNotRBP_EBP
            compStrings base, base100, 3, check_r12
            jmp cont_baseIsNotRBP_EBP
            check_r12:
                compStrings scale, scale00, 2, check_r12_
                jmp cont_baseIsNotRBP_EBP
                check_r12_:
                    compStrings index, index100, 3, check_r12__
                    jmp cont_baseIsNotRBP_EBP
                    check_r12__:
                        ; we have [r12]
                        appendString  memoryMunch ,  r12_
                        jmp exitHandleMemory

            cont_baseIsNotRBP_EBP:
                compStrings index, index100, 3, check_index
                jmp cont_baseIsNotRBP_EBP_
                check_index:
                    compStrings scale, scale00, 2, check_index_
                    jmp cont_baseIsNotRBP_EBP_
                    check_index_:
                        cmp byte [rex_w], '0'
                        je no_index
                        cmp byte [rex_x], ' '
                        je no_index
                        jne cont_baseIsNotRBP_EBP_
                        no_index:
                            mov r9, 0
                            jmp setMemory

            cont_baseIsNotRBP_EBP_:
                compStrings mod, mod00, 2, setMemory
                has_index:
                    mov r9, 1
                    jmp setMemory

    setMemory:
    ;    r8 base flag
    ;    r9 index flag
        cmp r9, 1
        jne has_no_index
        cmp r8, 1
        je has_index_has_base
        jne has_index_no_base

        has_no_index:
            ; no base and no index -> Direct addressing
            ; [0x5555551E]  #mod=00, rm=100, base=101, scale=00, index=100
            ; {disp}
            appendString  memoryMunch ,  disp 
            jmp exitHandleMemory

        has_index_has_base:
            ; has base and has index -> 
            ; [rbx_rcx*1] or [rbx+rcx*1+0x12]  #mod=00, base=101 ,rm=101
            ; {base}+{index}*{scale}
            mov QWORD [inMemoryF], 1
            mov r15, base
            mov r14, rex_b
            call getRegNameBySize
            appendString  memoryMunch ,  regNameTemp 
            appendString  memoryMunch ,  plus
            mov QWORD [inMemoryF], 1
            mov r15, index
            mov r14, rex_x
            call getRegNameBySize
            appendString  memoryMunch ,  regNameTemp 
            appendString  memoryMunch ,  mult
            call getScale
            appendString  memoryMunch ,  scaleNum 
            jmp checkForDisp
        has_index_no_base:
            ; no base and has index -> 
            ; [ecx*1] or [ecx*1+0x12]  #mod=00, base=101 ,rm=101
            ; {index}*{scale}
            mov QWORD [inMemoryF], 1
            mov r15, index
            mov r14, rex_x
            call getRegNameBySize
            appendString  memoryMunch ,  regNameTemp 
            appendString  memoryMunch ,  mult
            call getScale
            appendString  memoryMunch ,  scaleNum 
            jmp checkForDisp

    checkForDisp:
        cmp byte [disp], 0
        je exitHandleMemory
        ; has disp
        appendString  memoryMunch ,  plus
        appendString  memoryMunch ,  disp
    exitHandleMemory:
        appendString  memoryMunch ,  closeB
        ret
handleOneArg:

    ; one arg could be:
    ;   {commandName} {imd}
    ;   {commandName} {reg}
    ;   {commandName} {oper_size} PTR [{memory}]

    ; for shr and shl:
    ;    {commandName} {reg},1
    ;    {commandName} {oper_size} PTR [{memory}],1

    cmp BYTE [specialCommand_1arg], ' '
    jne handleSpecialOneArg
    je handleNormalOneArg
    handleSpecialOneArg:
        ; check for push/pop reg
        cmp QWORD [commandTypeF], 13
        je handle_PUSH_POP_REG

        ; push_bin_imd, call_bin_direct, ret_bin1
        ; {commandName} {imd}
        appendString  command ,  specialCommand_1arg 
        appendString  command ,  space 
        appendString  command ,  disp 
        jmp exitHandleOneArg
        
        handle_PUSH_POP_REG:
            ;{commandName} {reg}
            appendString  command ,  specialCommand_1arg 
            appendString  command ,  space 
            mov QWORD [inMemoryF], 0
            mov r15, reg
            mov r14, rex_r
            call getRegNameBySize
            appendString  command ,  regNameTemp 
            jmp exitHandleOneArg

    handleNormalOneArg:
        compStrings mod, mod11, 2, argIsReg
        argIsMemory:
            ;   {commandName} {oper_size} PTR [{memory}]
            compStrings opcode, shr_, 3, handleShiftOne_
            compStrings opcode, shl_, 3, handleShiftOne_
        
            ; check for push/pop mem
            cmp QWORD [commandTypeF], 14
            je is_PUSH_POP_Mem
            
            jmp cont_argIsMemory

            is_PUSH_POP_Mem:
            ; only 16 or 64 regs
            copyString [operSize], [QWORD_], 5
            cmp QWORD [oper_prefixF], 1
            jne cont_is_PUSH_POP_Mem
            copyString [operSize], [WORD_], 4
            cont_is_PUSH_POP_Mem:
            appendString  command ,  opcode 
            appendString  command ,  space 

            appendString  command ,  operSize 
            appendString  command ,  space 

            appendString  command ,  ptr_ 
            appendString  command ,  space 

            call handleMemory

            appendString  command ,  memoryMunch 

            jmp exitHandleOneArg
            cont_argIsMemory:
            ; normal case
            appendString  command ,  opcode 
            appendString  command ,  space 
            call getOperSize
            appendString  command ,  operSize 
            appendString  command ,  space 

            appendString  command ,  ptr_ 
            appendString  command ,  space 

            call handleMemory

            appendString  command ,  memoryMunch 

            jmp exitHandleOneArg

            handleShiftOne_:
                ;  {commandName} {oper_size} PTR [{memory}],1
                appendString  command ,  opcode 
                appendString  command ,  space 
                call getOperSize
                appendString  command ,  operSize 
                appendString  command ,  space 

                appendString  command ,  ptr_ 
                appendString  command ,  space 

                call handleMemory

                appendString  command ,  memoryMunch 

                appendString  command ,  comma 
                appendString  command ,  one 
                jmp exitHandleOneArg

        argIsReg:
            ;   {commandName} {reg}
            ;   reg name is in rm 
            compStrings opcode, shr_, 3, handleShiftOne
            compStrings opcode, shl_, 3, handleShiftOne

            ; normal case 
            compStrings opcode, call_, 3, is_call
            jmp contargIsReg
            is_call:
                ; only 16 or 64 regs
                mov BYTE [code_w], '1'
                cmp QWORD [oper_prefixF], 1
                je contargIsReg
                mov QWORD [rexF], 1
                mov BYTE [rex_w], '1'
 
            contargIsReg:
            ;    {commandName} {reg}
            appendString  command ,  opcode 
            appendString  command ,  space 
            mov QWORD [inMemoryF], 0
            mov r15, rm
            mov r14, rex_b
            call getRegNameBySize
            appendString  command ,  regNameTemp 
            jmp exitHandleOneArg


            handleShiftOne:
                ;    {commandName} {reg},1
                appendString  command ,  opcode 
                appendString  command ,  space 
                mov QWORD [inMemoryF], 0
                mov r15, rm
                mov r14, rex_b
                call getRegNameBySize
                appendString  command ,  regNameTemp 
                appendString  command ,  comma 
                appendString  command ,  one 
                jmp exitHandleOneArg


    exitHandleOneArg:
        ret
handleTwoArg:
    ; two arg could be:
    ;   r2r->
    ;       {commandName} {reg},{rm}
    ;       {commandName} {rm},{reg}
    ;   r2m or m2r->
    ;       {commandName} {oper_size} PTR [{memory}],{reg}
    ;       {commandName} {reg},{oper_size} PTR [{memory}]
    ;   imd2r or imd2m ->
    ;       {commandName} {rm},{imd}
    ;       {commandName} {oper_size} PTR [{memory}],{imd}

    ; for mov:
    ;   mov {reg},{imd}

    cmp BYTE [specialCommand_2arg], ' '
    jne handleSpecialTwoArg
    je handleNormalTwoArg
    handleSpecialTwoArg:
        compStrings specialCommand_2arg, mov_, 3, handleMov__

        compStrings mod, mod11, 2, do_special_rr
        jmp do_special_rm_mr

        compStrings specialCommand_2arg, mov_, 3, handleMov__
        handleMov__:
            ; mov {reg},{imd}
            appendString  command ,  specialCommand_2arg 
            appendString  command ,  space
            mov QWORD [inMemoryF], 0 
            mov r15, reg
            mov r14, rex_r
            call getRegNameBySize
            appendString  command ,  regNameTemp 
            appendString  command ,  comma 
            appendString  command ,  imd 
            jmp exitHandleTwoArg

        do_special_rr:
            compStrings specialCommand_2arg, imul_, 3, is_reg_rm
            compStrings specialCommand_2arg, bsf_, 3, is_reg_rm
            compStrings specialCommand_2arg, bsr_, 3, is_reg_rm
            jmp is_rm_reg
            is_reg_rm:
                ; {commandName} {reg},{rm}
                appendString  command ,  specialCommand_2arg 
                appendString  command ,  space 
                mov QWORD [inMemoryF], 0
                mov r15, reg
                mov r14, rex_r
                call getRegNameBySize
                appendString  command ,  regNameTemp 
                appendString  command ,  comma 
                mov QWORD [inMemoryF], 0
                mov r15, rm
                mov r14, rex_b
                call getRegNameBySize
                appendString  command ,  regNameTemp 
                jmp exitHandleTwoArg

            is_rm_reg:
                ; {commandName} {rm},{reg}
                appendString  command ,  specialCommand_2arg 
                appendString  command ,  space 
                mov QWORD [inMemoryF], 0
                mov r15, rm
                mov r14, rex_b
                call getRegNameBySize
                appendString  command ,  regNameTemp 
                appendString  command ,  comma 
                mov QWORD [inMemoryF], 0
                mov r15, reg
                mov r14, rex_r
                call getRegNameBySize
                appendString  command ,  regNameTemp 
                jmp exitHandleTwoArg

        do_special_rm_mr:
            compStrings specialCommand_2arg, xadd_, 3, do_special_mr
            jmp do_special_rm
            do_special_mr:
                ; {commandName} {oper_size} PTR [{memory}],{reg}
                appendString  command ,  specialCommand_2arg 
                appendString  command ,  space 
                call getOperSize
                appendString  command ,  operSize 
                appendString  command ,  space 
                appendString  command ,  ptr_ 
                appendString  command ,  space 
                call handleMemory
                appendString  command ,  memoryMunch 
                appendString  command ,  comma 
                mov QWORD [inMemoryF], 0
                mov r15, reg
                mov r14, rex_r
                call getRegNameBySize
                appendString  command ,  regNameTemp 
                jmp exitHandleTwoArg

            do_special_rm:
                ; {commandName} {reg},{oper_size} PTR [{memory}]
                appendString  command ,  specialCommand_2arg 
                appendString  command ,  space 
                mov QWORD [inMemoryF], 0
                mov r15, reg
                mov r14, rex_r
                call getRegNameBySize
                appendString  command ,  regNameTemp 
                appendString  command ,  comma 
                call getOperSize
                appendString  command ,  operSize 
                appendString  command ,  space 
                appendString  command ,  ptr_ 
                appendString  command ,  space 
                call handleMemory
                appendString  command ,  memoryMunch 

                jmp exitHandleTwoArg
    handleNormalTwoArg:
        ; normal two arg command: 
        ; r2r, m2r-> 31, 
        ; imd2r, imd2m-> 32

        cmp QWORD [commandTypeF], 31   
        je handleTwoArgs_rr_mr 
        jne handleTwoArgs_mimd_rimd

        handleTwoArgs_rr_mr:
            compStrings mod, mod11, 2, twoArgs_rr
            jmp twoArgs_mr_rm
            twoArgs_rr:
                ; {commandName} {rm},{reg}
                appendString  command ,  opcode 
                appendString  command ,  space 
                mov QWORD [inMemoryF], 0
                mov r15, rm
                mov r14, rex_b
                call getRegNameBySize
                appendString  command ,  regNameTemp 
                appendString  command ,  comma 
                mov QWORD [inMemoryF], 0
                mov r15, reg
                mov r14, rex_r
                call getRegNameBySize
                appendString  command ,  regNameTemp 
                jmp exitHandleTwoArg

            twoArgs_mr_rm:
                compStrings opcode, xadd_, 3, twoArgs_mr
                compStrings opcode, xchg_, 3, twoArgs_mr
                cmp BYTE [d_s], '0'
                je twoArgs_mr
                jmp twoArgs_rm

                twoArgs_mr:
                    ; {commandName} {oper_size} PTR [{memory}],{reg}
                    appendString  command ,  opcode 
                    appendString  command ,  space 
                    call getOperSize
                    appendString  command ,  operSize 
                    appendString  command ,  space 
                    appendString  command ,  ptr_ 
                    appendString  command ,  space 
                    call handleMemory
                    appendString  command ,  memoryMunch 
                    appendString  command ,  comma 
                    mov QWORD [inMemoryF], 0
                    mov r15, reg
                    mov r14, rex_r
                    call getRegNameBySize
                    appendString  command ,  regNameTemp 
                    jmp exitHandleTwoArg

                twoArgs_rm:
                    ; {commandName} {reg},{oper_size} PTR [{memory}]
                    appendString  command ,  opcode 
                    appendString  command ,  space 
                    mov QWORD [inMemoryF], 0
                    mov r15, reg
                    mov r14, rex_r
                    call getRegNameBySize
                    appendString  command ,  regNameTemp 
                    appendString  command ,  comma 
                    call getOperSize
                    appendString  command ,  operSize 
                    appendString  command ,  space 
                    appendString  command ,  ptr_ 
                    appendString  command ,  space 
                    call handleMemory
                    appendString  command ,  memoryMunch  
                    jmp exitHandleTwoArg

        handleTwoArgs_mimd_rimd:
            compStrings mod, mod11, 2, twoArgs_rimd
            jmp twoArgs_mimd 

            twoArgs_rimd:
                ; {commandName} {rm},{imd}
                appendString  command ,  opcode 
                appendString  command ,  space 
                mov QWORD [inMemoryF], 0
                mov r15, rm
                mov r14, rex_b
                call getRegNameBySize
                appendString  command ,  regNameTemp 
                appendString  command ,  comma 
                appendString  command ,  imd 

                jmp exitHandleTwoArg

            twoArgs_mimd:
                ; {commandName} {oper_size} PTR [{memory}],{imd}
                appendString  command ,  opcode 
                appendString  command ,  space 
                call getOperSize
                appendString  command ,  operSize 
                appendString  command ,  space 
                appendString  command ,  ptr_ 
                appendString  command ,  space 
                call handleMemory
                appendString  command ,  memoryMunch 
                appendString  command ,  comma 
                appendString  command ,  imd 

                jmp exitHandleTwoArg

    exitHandleTwoArg:
        ret


setDefaults:

    mov qword [n_args],0

    ; Rex -> 1 byte
    mov byte [rex_w], ' ' 
    mov byte [rex_r], ' ' 
    mov byte [rex_x], ' ' 
    mov byte [rex_b], ' ' 


    ;OpCode -> 1 byte (MUST HAVE) 
    mov byte [opcode], 0; is command's name


    mov byte [d_s], ' ' 
    mov byte [code_w], ' ' 

    mov byte [mod], ' '
    mov byte [reg], ' '
    mov byte [rm], ' '
    mov byte [code_w], ' '
    mov byte [scale], ' '
    mov byte [scaleNum], ' '
    mov byte [index], ' '
    mov byte [base], ' '

    ; reset some flags
    mov QWORD [addr_prefixF], 0
    mov QWORD [oper_prefixF], 0
    mov QWORD [rexF], 0
    mov QWORD [commandTypeF], 0
    mov QWORD [commandFoundF], 0
    mov QWORD [inMemoryF], 0

    mov BYTE [command], 0
    mov BYTE [reversedTempHex], 0
    mov BYTE [disp], 0
    mov BYTE [imd], 0

    mov BYTE [specialCommand_0arg], ' '
    mov BYTE [specialCommand_0arg+1], 0
    mov BYTE [specialCommand_1arg], ' '
    mov BYTE [specialCommand_1arg+1], 0
    mov BYTE [specialCommand_2arg], ' '
    mov BYTE [specialCommand_2arg+1], 0
    mov BYTE [memoryMunch], 0

    ret
;---------------Main Function----------------;
DoDisAssembler:
    
    call setDefaults


    ; fills the sections in code data
    call parseBinary

    ; check if it's zero arg
    cmp BYTE [specialCommand_0arg], ' '
    je cont1
    ; is zero arg 
    mov rdi, specialCommand_0arg
    call GetStrlen
    copyString [command], [specialCommand_0arg], rdx
    jmp exitDoDisAssembler

    cont1:
    cmp BYTE [disp], 0 
    jne hasDisp
    je checkImd

    hasDisp:
        mov rsi, disp 
        call reverseBits
        mov rdi, reversedTempHex
        call GetStrlen
        copyString [disp], [reversedTempHex], rdx

    checkImd:
        cmp BYTE [imd], 0 
        jne hasImd
        je cont

    hasImd:
        mov rsi, imd 
        call reverseBits
        mov rdi, reversedTempHex
        call GetStrlen
        copyString [imd], [reversedTempHex], rdx

    cont:
        cmp QWORD [n_args], 1
        jne isTwoArg
        ; has only one arg
        call handleOneArg
        jmp exitDoDisAssembler
    
    isTwoArg:
        ; has two args
        call handleTwoArg
        jmp exitDoDisAssembler
    
    exitDoDisAssembler:
        ret
;---------------------------------------------;

;------------PARSER--------------------
hasPrefix:
    compStrings binary, addr_prefix, 8, hasAddrPrefix
    jmp checkOperPrefix
    hasAddrPrefix:
        mov QWORD [addr_prefixF], 1
        removeBits binary, 8

    checkOperPrefix:
        compStrings binary, oper_prefix, 8, hasOperPrefix
        jmp exitHasPrefix
    hasOperPrefix:
        mov QWORD [oper_prefixF], 1
        removeBits binary, 8

    exitHasPrefix:
    ret

hasRex:
    compStrings binary, rex, 4, hasRex_
    jmp exitHasRex
    hasRex_:
        mov QWORD [rexF], 1
        removeBits binary, 4

        mov al, [binary]
        mov BYTE [rex_w], al
        removeBits binary, 1

        mov al, [binary]
        mov BYTE [rex_r], al
        removeBits binary, 1

        mov al, [binary]
        mov BYTE [rex_x], al
        removeBits binary, 1

        mov al, [binary]
        mov BYTE [rex_b], al
        removeBits binary, 1

    exitHasRex:
    ret

find_setOpcode:
    ; command_bin start in r15,
    ; number of bits in r14, 
    ; command_name start in r13
    ; number of args in r12
    ; start of where to keep the command's name in r11
    ; command type flag in r10
    ; commnad reg in r9

    ; sets command's name in where we want it
    ; and number of args in n_args
    ; also sets d_s and code_w for normal one/two args
    ; sets command type flag for special handeling
    
    compStrings binary, r15, r14, ispreFound
    jmp exitFind_setOpcode

    ispreFound:
        cmp BYTE [r9], ' '
        je isFound

        ; check reg field!
        cmp r14, 7
        je checkNoD_S
        cmp r14, 6
        je checkWithD_S
        
        compStrings binary, r9, 3, isFound
        checkNoD_S:
            mov rbx, binary
            add rbx, r14
            add rbx, 3
            compStrings rbx, r9, 3, isFound
            jmp exitFind_setOpcode
        checkWithD_S:
            mov rbx, binary
            add rbx, r14
            add rbx, 4
            compStrings rbx, r9, 3, isFound
            jmp exitFind_setOpcode

    isFound:
        mov QWORD [commandFoundF], 1
        mov QWORD [commandTypeF], r10
        push r11
        push r12 
        push r13 
        push r14 

        mov rdi, r13 
        call GetStrlen

        pop r14
        pop r13
        pop r12
        pop r11

        copyString [r11], [r13], rdx
    
        removeBits binary, r14

        mov QWORD [n_args], r12

        cmp r12, 1 
        je checkNormalOne
        cmp r12, 2
        je checkNormalTwo

        jmp exitFind_setOpcode

        checkNormalOne:
            cmp BYTE [specialCommand_1arg], ' '
            jne exitFind_setOpcode
            ; is normal one arg command
            ; d_s does not matter

            mov al, [binary]
            mov [code_w], al

            removeBits binary, 1

            jmp exitFind_setOpcode

        checkNormalTwo:
            cmp BYTE [specialCommand_2arg], ' '
            jne exitFind_setOpcode
            ; is normal two arg command
            
            ; is push_bin_reg, pop_bin_reg, no d_s or w
            cmp QWORD [commandTypeF], 14
            je exitFind_setOpcode
            ; is pop_bin_mem, no d_s or w
            cmp QWORD [commandTypeF], 13
            je exitFind_setOpcode

            compStrings opcode, xchg_, 4, addW

            mov al, [binary]
            mov [d_s], al
            removeBits binary, 1

            addW:
            mov al, [binary]
            mov [code_w], al

            removeBits binary, 1

            jmp exitFind_setOpcode

    exitFind_setOpcode:
    ret
setOpcode:
    ; zero arg commands(1): 8 bits OR 16 bits(SYSCALL)

    ; one  arg commands(2): 7 bits OR  4 bits(push_reg) -> 13
    ;                              OR  6 bits(push_imd) -> 11

    ;                              OR  5 bits(pop_reg) -> 13
    ;                              OR  8 bits(pop_mem) -> 14

    ;                              OR  8 bits(call_direct) -> 12
    ;                              OR  8 bits(ret) -> 12

    ; two  arg commands(3): 6 bits OR  7 bits(xchg) 
    ;                              OR  15 bits(xadd) -> 21
    ;                              OR  16 bits(imul) -> 22
    ;                              OR  16 bits(bsf) -> 23
    ;                              OR  16 bits(bsr) -> 24
    ;                              OR  4  bits(mov_imd2r) -> 25
    ; r2r, m2r-> 31, imd2r, imd2m-> 32


    ; check if it's special one arg command
    findCommand push_bin_imd,    6, push_, 1, specialCommand_1arg, 11, dummy_reg
    findCommand call_bin_direct, 8, call_, 1, specialCommand_1arg, 12, dummy_reg
    findCommand ret_bin1,        8, ret_,  1, specialCommand_1arg, 12, dummy_reg

    ; check if it's special two arg command
    findCommand mov_imd2r,     4, mov_,  2, specialCommand_2arg, 25, dummy_reg
    findCommand xadd_r2r_m2r, 15, xadd_, 2, specialCommand_2arg, 21, dummy_reg
    findCommand imul_r2r_m2r, 16, imul_, 2, specialCommand_2arg, 22, dummy_reg
    findCommand bsf_r2r_m2r,  16, bsf_,  2, specialCommand_2arg, 23, dummy_reg
    findCommand bsr_r2r_m2r,  16, bsr_,  2, specialCommand_2arg, 24, dummy_reg

    ; check if it's normal zero arg command
    findCommand stc_bin,  8, stc_,  0, specialCommand_0arg, 1, dummy_reg
    findCommand std_bin,  8, std_,  0, specialCommand_0arg, 1, dummy_reg
    findCommand clc_bin,  8, clc_,  0, specialCommand_0arg, 1, dummy_reg
    findCommand cld_bin,  8, cld_,  0, specialCommand_0arg, 1, dummy_reg
    findCommand sys_bin, 16, sys_,  0, specialCommand_0arg, 1, dummy_reg
    findCommand ret_bin,  8, ret_,  0, specialCommand_0arg, 1, dummy_reg

    ; check if it's normal one arg command
    findCommand imul_bin, 7, imul_,  1, opcode, 2, imul_reg
    findCommand idiv_bin, 7, idiv_,  1, opcode, 2, idiv_reg
    findCommand dec_bin,  7, dec_,   1, opcode, 2, dec_reg
    findCommand inc_bin,  7, inc_,   1, opcode, 2, inc_reg

    findCommand push_bin_reg, 5, push_, 1, specialCommand_1arg, 13, dummy_reg
    findCommand push_bin_mem, 7, push_, 1, opcode, 14, push_reg
    findCommand pop_bin_reg,  5,  pop_, 1, specialCommand_1arg, 13, dummy_reg
    findCommand pop_bin_mem,  7,  pop_, 1, opcode, 14, pop_reg

    findCommand neg_bin, 7, neg_,  1, opcode, 2, neg_reg
    findCommand not_bin, 7, not_,  1, opcode, 2, not_reg
    findCommand shr_bin, 7, shr_,  1, opcode, 2, shr_reg
    findCommand shl_bin, 7, shl_,  1, opcode, 2, shl_reg

    findCommand call_bin_indirect, 7, call_,  1, opcode, 2, call_reg

    ; check if it's normal two arg command
    findCommand mov_r2r_m2r,      6, mov_,  2, opcode, 31, dummy_reg
    findCommand mov_imd2r_imd2m,  6, mov_,  2, opcode, 32, mov_reg

    findCommand add_r2r_m2r,      6, add_,  2, opcode, 31, dummy_reg
    findCommand add_imd2r_imd2m,  6, add_,  2, opcode, 32, add_reg

    findCommand adc_r2r_m2r,      6, adc_,  2, opcode, 31, dummy_reg
    findCommand adc_imd2r_imd2m,  6, adc_,  2, opcode, 32, adc_reg

    findCommand sub_r2r_m2r,      6, sub_,  2, opcode, 31, dummy_reg
    findCommand sub_imd2r_imd2m,  6, sub_,  2, opcode, 32, sub_reg

    findCommand sbb_r2r_m2r,      6, sbb_,  2, opcode, 31, dummy_reg
    findCommand sbb_imd2r_imd2m,  6, sbb_,  2, opcode, 32, sbb_reg
    
    findCommand and_r2r_m2r,      6, and_,  2, opcode, 31, dummy_reg
    findCommand and_imd2r_imd2m,  6, and_,  2, opcode, 32, and_reg

    findCommand or_r2r_m2r,       6, or_,   2, opcode, 31, dummy_reg
    findCommand or_imd2r_imd2m,   6, or_,   2, opcode, 32, or_reg

    findCommand xor_r2r_m2r,      6, xor_,  2, opcode, 31, dummy_reg
    findCommand xor_imd2r_imd2m,  6, xor_,  2, opcode, 32, xor_reg

    findCommand cmp_r2r_m2r,      6, cmp_,  2, opcode, 31, dummy_reg
    findCommand cmp_imd2r_imd2m,  6, cmp_,  2, opcode, 32, cmp_reg

    findCommand xchg_r2r_m2r,     7, xchg_, 2, opcode, 31, dummy_reg

    findCommand test_r2r_m2r,     6, test_, 2, opcode, 31, dummy_reg
    findCommand test_imd2r_imd2m, 6, test_, 2, opcode, 32, test_reg

    findCommand shr_bin2,         6, shr_,  2, opcode, 32, shr_reg

    findCommand shl_bin2,         6, shl_,  2, opcode, 32, shl_reg
    

    contSetOpcode:
    cmp BYTE [specialCommand_1arg], ' '
    jne specialOneArgParse
    cmp BYTE [specialCommand_2arg], ' '
    jne specialTwoArgParse
    jmp exitSetOpcode
    specialOneArgParse:
        ; commands that are in the form of opCode+Disp or opCode+Imd
        compStrings specialCommand_1arg, push_, 4, isPush
        ; check if it's push reg
        cmp QWORD [commandTypeF], 13
        je is_PUSH_POP_REG
        jmp ret_or_call
        isPush:
            ; check if it's push reg
            cmp QWORD [commandTypeF], 13
            je is_PUSH_POP_REG

            mov al, [binary]
            mov [d_s], al
            removeBits binary, 1

            mov al, [binary]
            mov [code_w], al
            removeBits binary, 1

            mov rdi, binary
            call GetStrlen
            copyString [disp], [binary], rdx
    
            removeBits binary, rdx

            jmp exitSetOpcode
        is_PUSH_POP_REG:
            mov rdi, binary
            call GetStrlen
            copyString [reg], [binary], rdx
            removeBits binary, rdx

            ; only 16 or 64 regs
            mov BYTE [code_w], '1'
            cmp QWORD [oper_prefixF], 1
            je exitSetOpcode
            mov QWORD [rexF], 1
            mov BYTE [rex_w], '1'

            jmp exitSetOpcode

        ret_or_call:
            ; ret / call_direct, #+disp
            mov rdi, binary
            call GetStrlen
            copyString [disp], [binary], rdx
    
            removeBits binary, rdx

            jmp exitSetOpcode
    specialTwoArgParse:
        ; xadd
        cmp QWORD [commandTypeF], 21
        jne next1
        ;  "xadd_r:r|m:r" 
        ;       w+mod=11+reg1+reg2
        ;       w+mod+reg+r/m
        mov al, [binary]
        mov [code_w], al
        removeBits binary, 1
        call hasMod_RM

        jmp exitSetOpcode
        next1:
        ; bsf
        cmp QWORD [commandTypeF], 23
        jne next2

        mov BYTE [code_w], '1'
        ;mov BYTE [rex_w], '1'
        mov BYTE [d_s], '0'
        call hasMod_RM
        jmp exitSetOpcode
        next2:
        ; bsr
        cmp QWORD [commandTypeF], 24
        jne next3
        
        mov BYTE [code_w], '1'
        ;mov BYTE [rex_w], '1'
        mov BYTE [d_s], '0'
        call hasMod_RM
        jmp exitSetOpcode

        next3:
        ; imul 
        cmp QWORD [commandTypeF], 22
        jne next4

        mov BYTE [code_w], '1'
        call hasMod_RM
        jmp exitSetOpcode

        next4:
        ; mov r:imd
        ; '1011': 'mov_r:imd', # +w+reg+imd
        cmp QWORD [commandTypeF], 25
        jne exitSetOpcode

        mov al, [binary]
        mov [code_w], al
        removeBits binary, 1

        copyString [reg], [binary], 3
        removeBits binary, 3

        mov rdi, binary
        call GetStrlen
        copyString [imd], [binary], rdx
        removeBits binary, rdx

        jmp exitSetOpcode

    exitSetOpcode:
    ret
hasMod_RM:
    ; also handels SIB and disp because
    ; if we have SIB/disp, we must have Mod R/M
    
    
    ; special cases where opcode+disp are handled elsewhere
    mov QWORD [hasDispF], 0

    ; mod
    copyString [mod], [binary], 2
    removeBits binary, 2
    ; reg
    copyString [reg], [binary], 3
    removeBits binary, 3
    ; rm
    copyString [rm], [binary], 3
    removeBits binary, 3

    cmp BYTE [binary], 0
    je exitHasMod_RM

    ; mod 11 -> rm is reg 
    compStrings mod, mod11, 2, registerAddressing
    ; mod 01 -> 8 bit disp
    compStrings mod, mod01, 2, disp8
    ; mod 10 -> 32 bit disp
    compStrings mod, mod10, 2, disp32

    ; mod 00
    ; check for SIB
    compStrings rm, rm100, 3, hasSIB_NoDisp
    compStrings rm, rm101, 3, hasSIB_directAddressing
    jmp exitHasMod_RM

    hasSIB_NoDisp:
        ; mod 00, rm 100 -> SIB no DISP 

        ; take SIB out
        ; scale
        copyString [scale], [binary], 2
        removeBits binary, 2
        ; index
        copyString [index], [binary], 3
        removeBits binary, 3
        ; base
        copyString [base], [binary], 3
        removeBits binary, 3
        
        ; take disp out (if any!)
        compStrings base, base101, 3, cont___
        ; base is not epb/rpb so no disp, if anything left it's imd
        jmp cont_imd

        cont___:
        cmp byte [binary], 0
        je exitHasMod_RM
        copyString [disp], [binary], 32
        removeBits binary, 32
        cont_imd:
        ; if anything left, is imd
        cmp BYTE [binary], 0
        je exitHasMod_RM

        mov rdi, binary
        call GetStrlen
        copyString [imd], [binary], rdx
        removeBits binary, rdx
        
        jmp exitHasMod_RM

    hasSIB_directAddressing:
        ; mod 00, rm 101 -> Memory direct access

        ; take SIB out
        ; scale
        copyString [scale], [binary], 2
        removeBits binary, 2
        ; index
        copyString [index], [binary], 3
        removeBits binary, 3
        ; base
        copyString [base], [binary], 3
        removeBits binary, 3

        ; take disp out
        copyString [disp], [binary], 32
        removeBits binary, 32

        ; if anything left, is imd
        cmp BYTE [binary], 0
        je exitHasMod_RM

        mov rdi, binary
        call GetStrlen
        copyString [imd], [binary], rdx
        removeBits binary, rdx

        jmp exitHasMod_RM
    registerAddressing:
        ; no memory! reg is a register
        jmp exitHasMod_RM
    disp8:
        ; check for SIB
        compStrings rm, rm100, 3, hasSIB_disp8
        jmp noSIB_disp8
        hasSIB_disp8:
            ; take SIB out
            ; scale
            copyString [scale], [binary], 2
            removeBits binary, 2
            ; index
            copyString [index], [binary], 3
            removeBits binary, 3
            ; base
            copyString [base], [binary], 3
            removeBits binary, 3

            ; take disp out
            copyString [disp], [binary], 8
            removeBits binary, 8

            ; if anything left, is imd
            cmp BYTE [binary], 0
            je exitHasMod_RM

            mov rdi, binary
            call GetStrlen
            copyString [imd], [binary], rdx
            removeBits binary, rdx
            
            jmp exitHasMod_RM

        noSIB_disp8:
            ; take disp out
            copyString [disp], [binary], 8
            removeBits binary, 8

            ; if anything left, is imd
            cmp BYTE [binary], 0
            je exitHasMod_RM

            mov rdi, binary
            call GetStrlen
            copyString [imd], [binary], rdx
            removeBits binary, rdx

            jmp exitHasMod_RM
 
    disp32:
        ; check for SIB
        compStrings rm, rm100, 3, hasSIB_disp32
        jmp noSIB_disp32
        hasSIB_disp32:
            ; take SIB out
            ; scale
            copyString [scale], [binary], 2
            removeBits binary, 2
            ; index
            copyString [index], [binary], 3
            removeBits binary, 3
            ; base
            copyString [base], [binary], 3
            removeBits binary, 3

            ; take disp out
            copyString [disp], [binary], 32
            removeBits binary, 32

            ; if anything left, is imd
            cmp BYTE [binary], 0
            je exitHasMod_RM

            mov rdi, binary
            call GetStrlen
            copyString [imd], [binary], rdx
            removeBits binary, rdx

            jmp exitHasMod_RM
        noSIB_disp32:
            ; take disp out
            copyString [disp], [binary], 32
            removeBits binary, 32

            ; if anything left, is imd
            cmp BYTE [binary], 0
            je exitHasMod_RM

            mov rdi, binary
            call GetStrlen
            copyString [imd], [binary], rdx
            removeBits binary, rdx  

            jmp exitHasMod_RM     

    exitHasMod_RM:
    ret
hasData:
    mov rdi, binary
    call GetStrlen
    copyString [imd], [binary], rdx
    removeBits binary, rdx

    ret

parseBinary:
    call hasPrefix
    call hasRex
    call setOpcode

    ;zero arg command or special 1,2 arg and done. 
    cmp BYTE [binary], 0
    je exitParseBinary

    ; handels mod/rm, SIB, and disp
    call hasMod_RM

    ; no imd data
    cmp BYTE [binary], 0
    je exitParseBinary 

    call hasData

    cmp BYTE [binary], 0
    je exitParseBinary 
    mov rsi, msgWarning
    call printString

    exitParseBinary:
    ret

;----------------------------------------------------

%if 0
_start:

    call DoDisAssembler

    mov rsi, command
    call printString
    call newLine

exit:
    mov rax, sys_exit
    xor rdi, rdi
    syscall

%endif ; 0 