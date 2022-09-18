%include "in_out.asm"

section .data
    msg  db  "Please input the absolute address to the file! like: /home/ladypary/Documents/Assembly/test/", NL,0
    msg2 db  "Please input the file's name! like: Assembler_Testcases", NL,0

    dir      db '                                                                                                                                                      ',0 
    dir_size dq 0
    fileName db '                                                             ',0 

    space    db ' ',0
    newLine_ db 0x0a 

    file_name_binary db 'binary_output.txt',0
    file_name_hex    db 'hex_output.txt',0

    test_fdesc   dq 0     ; file descriptor of test case file
    bin_fdesc    dq 0     ; file descriptor of binary file
    hex_fdesc    dq 0     ; file descriptor of hex file

    
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

    ; the command string is stored here
    command db "",0,0x0a
    command_space db "                                                                          ",0
    
    ; only for shift rax,1 -> shift rax
    command_untouched db "",0,0x0a
    command_untouched_space db "                                                                          ",0

    ans     db "4181840a6754343252080000",0

    extendedFirstByte db  "000"
    ; the final binary code is stored here.
    binary db  "", 0
    space_ db  "                                                                                                                                              ",0
    ; the final hex code is stored here.
    hex    db  "                                                   ", 0

;+++ Store Binary Bits +++++++++++++++++++++++++++++++++++++++++++++++++++
    ; prefixes
    addr_prefix db "01100111",0 // 67
    oper_prefix db "01100110",0 // 66

    ; rex
    rex          db "0100",0

    rex_w        db " ",0
    rex_r        db " ",0
    rex_x        db " ",0
    rex_b        db " ",0

    ;OpCode -> 1 byte (MUST HAVE)  
    opcode db "                                           ",0  ; 6 bit

    code_w db "0",0  ; 1 bit
    ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ;+++ Flags +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    isHandeledF       dq 0 ;0: no, 1: yes
    hasMemoryF        dq 0 ;0: no, 1: yes
    endOnSuccessF     dq 0 ;0: no, 1: yes
    isNewRegF         dq 0 ;0: no, 1: yes
    isBSR_BSF_IMUL_F  dq 0 ;0: no, 1: yes
    ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ;+++ Numbers +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    zero   db '0',0
    one    db '1',0
    hexOne db '0x1',0
    ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ;+++ Temporary Storages ++++++++++++++++++++++++++++++++++++++++++++++++++
    register        db '    ',0 
    registerBinary  db '   ',0 
    registerSize    dq  0 
    regOpcode       db '   ',0 

    ; for regReg
    reg1            db '    ',0 
    reg2            db '    ',0 

    d_s             db " ",0 

    mod             db '',0
    space1          db '            '
    
    reg             db '',0 
    spacee          db '      ',0

    rm              db '',0
    space2          db '            '

    memoryMunch     db '                                                ',0
    
    base            db '',0  ; register binary code
    space3          db '            '
    
    scale           db '',0
    space4          db '            '

    baseReg         db '',0 ; register name
    space5          db '            '

    index           db '',0   ; register binary code
    space6          db '            '

    indexReg        db '',0 ; register name
    space7          db '            '

    disp            db '                                                ',0

    imd             db '                                                ',0

    between         db '                                                ',0
    
    regSizeTemp     dq 0
    regTempBinary   db '   ',0 

    type            dq 0
    ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ;+++++ Used As Dictionary ++++++++++++++++++++++++++++++++++++++++++++++++
    ;----- Sizes -------------------------------------------------------------
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
    sys_        db  "sys",0 ;syscall
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
    push_bin_imd   db  "01101010",0 
    ; imd 8:  01101010
    ; imd 32: 01101000
    push_reg       db  "110",0 ;for register and memory

    pop_bin_reg    db  "01011",0 ; + reg code
    pop_bin_mem    db  "10001111",0
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
    or_            db  "or ",0
    xor_           db  "xor",0

    cmp_           db  "cmp",0
    test_          db  "test",0

    xchg_          db  "xchg",0
    xadd_          db  "xadd",0

    bsf_           db  "bsf",0
    bsr_           db  "bsr",0

    ;...............................................
    dummy_reg            db "   ",0

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
    hexIndicator_       db "0x",0  
    numbers db '0','1','2','3','4','5','6','7','8','9'
    dataORdisp          db "                                                   ", 0
    hexIndicator        db "0x",0  
    tempReversedHex     db "                                                   ", 0
    isOddFlag           dq 0
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        binaryTemp db  "                                                                                   ", 0
    hexTemp    db  "                                                                                   ", 0

    ;decimal db  "0", 0
    ;hex      db  "1E555555", 0

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
    ;-----------------------------------------------------
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
section .bss
    reminder resq 1000
    command_buffer resb 1

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
    

    ; create binary file
    ; Open new file in samePath/binary_output.txt
    ; r11: path size, r12: file name, r13: dir, r14: path+name
    mov r11, [dir_size]
    lea r12, [file_name_binary]
    mov r13, dir
    call pathPlusName

    ;r14: path/name
    mov rdi, r14
    call openFile ; creates it!
    mov [bin_fdesc], rax ; keep the file descriptor

    ; create hex file
    ; Open new file in samePath/hex_output.txt
    ; r11: path size, r12: file name, r13: dir, r14: path+name
    mov r11, [dir_size]
    lea r12, [file_name_hex]
    mov r13, dir
    call pathPlusName

    ;r14: path/name
    mov rdi, r14
    call openFile ; creates it!
    mov [hex_fdesc], rax ; keep the file descriptor

    mov rcx, 1
    readCommand:
        ; read from source file
        ; rdi : file descriptor ; rsi : buffer ; rdx : length
        mov rdi, [test_fdesc]
        mov rsi, command_buffer
        mov rdx, 1
        call readFile

        cmp rax, 0 ; bytes actually read
        je fullCommandFound

        cmp BYTE [command_buffer], 0x0a ; new line
        je fullCommandFound

        appendString command, command_buffer
        jmp readCommand

        fullCommandFound:
            push rax

            mov rsi, command
            call printString
            call newLine

            mov rdi, command
            call GetStrlen
            copyString [command_untouched], [command], rdx



            call DoAssembler

            mov rsi, binary
            call printString
            call newLine

            ; returns the hex code in hexTemp
            mov rbx, binary
            call bin2hex

            mov rdi, hexTemp
            call GetStrlen
            copyString [hex], [hexTemp], rdx

            mov rsi, hex
            call printString

            call newLine

            call writeToBin
            call writeToHex

            ; reset command 
            mov BYTE [command], 0
            mov BYTE [command_untouched], 0

            pop rax
            cmp rax, 0 ; bytes actually read
            je doneReadingCommands

            jmp readCommand

    doneReadingCommands:
        ; close files and exit 
        mov rdi, [test_fdesc]
        call closeFile
        mov rdi, [hex_fdesc]
        call closeFile
        mov rdi, [bin_fdesc]
        call closeFile

exit:
    mov rax, sys_exit
    xor rdi, rdi
    syscall
;nasm -felf64 Assembler.asm && ld Assembler.o && ./a.out

writeToBin:
    ; write the binary

    ; get it's len
    mov rdi, binary
    call GetStrlen
    push rdx
    ; rdi : file descriptor ; rsi : buffer ; rdx : length
    mov rdi, [bin_fdesc]
    mov rsi, binary
    pop rdx
    call writeFile

    ret
writeToHex:
    ; write the hex+' '+command

    ; get it's len
    mov rdi, hex
    call GetStrlen
    push rdx
    ; rdi : file descriptor ; rsi : buffer ; rdx : length
    mov rdi, [hex_fdesc]
    mov rsi, hex
    pop rdx
    call writeFile

    mov rdi, [hex_fdesc]
    mov rsi, space
    mov rdx, 1
    call writeFile

    ; get it's len
    mov rdi, command_untouched
    call GetStrlen
    push rdx
    ; rdi : file descriptor ; rsi : buffer ; rdx : length
    mov rdi, [hex_fdesc]
    mov rsi, command_untouched
    pop rdx
    call writeFile

    mov rdi, [hex_fdesc]
    mov rsi, newLine_
    mov rdx, 1
    call writeFile

    ret
;----------------------------------------------------
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


;-----------------------CONVERT---------------------------;
; Main functions in this file:
;   bin2hex->       Start address of binary in rbx
;                   Converts a binary string to hex in hexTemp
;
;   string2int->    String start address in rsi
;                   Returns num in rax
;
;   dec2hex->       Start  address of dec string in rsi
;                   Converts a dec string to hex in hexTemp
;
;   hex2bin->       Start address of hex in rbx
;                   Converts a hex string to binary in hexBinary

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
getChar:
    ; string start address in rsi
    ; returns a character in al
    mov al, [rsi+r8]
    inc r8

    ret
string2int:
    ; string start address in rsi
    ; returns num in rax
    push   rcx
    push   rbx
    push   rdx

    mov    bl,0
    mov    rdx, 0
    xor    r8, r8 ; index on dec

    rAgain_:
    xor    rax, rax
    call   getChar
    cmp    al, '-'
    jne    sAgain_
    mov    bl,1  
    jmp    rAgain_
    sAgain_:
    cmp    al, NL
    je     rEnd_
    cmp    al, 0 ;null terminated
    je     rEnd_
    sub    rax, 0x30
    imul   rdx, 10
    add    rdx,  rax
    xor    rax, rax
    call   getChar
    jmp    sAgain_
    rEnd_:
    mov    rax, rdx 
    cmp    bl, 0
    je     sEnd_
    neg    rax 
    sEnd_:  
    pop    rdx
    pop    rbx
    pop    rcx
    ret

%macro getHexDigit_1 3
    ; e.g. getHexDigit_1 0, isOne_1 , zero_hex
    mov al, %1
    cmp [reminder + r8], al
    jne %2
    ; it was 0
    dec r8
    mov rsi, %3
    lea rdi, [hexTemp + r9] ;r9(index on hex)
    inc r9
    movsb
    cmp r8, 0
    jl exitDec2hex
%endmacro

dec2hex:
    ; start address of dec string in rsi
    ; Converts a dec string to hex

    ; step1: convert string rep to int
    call string2int ; int in rax

    ; step2: div by 16, keep reminders in the 'reminder' resb
    ;                   keep index on last element in r8
    mov r9, 16
    xor r8, r8
    xor rdx, rdx
    divLoop:
        cmp rax, 16
        jb preDoConversion_1
        xor rdx, rdx
        idiv r9 ; div rax/16
        mov [reminder + r8], rdx
        inc r8
        jmp divLoop
        
    ; step3: starting from the end of 'reminder', convert digits to hex rep, 
    ;        store in hex
    preDoConversion_1:
        ; get the last reminder
        mov [reminder + r8], rax

        ; r8 iterator on reminder
        xor r9, r9 ; iterator on hex
        doConversion_1:
            cmp r8, 0 ; len
            jb exitDec2hex

            is0_1:
                getHexDigit_1  0, is1_1 ,zero_hex
            is1_1:
                getHexDigit_1  1, is2_1 ,one_hex
            is2_1:
                getHexDigit_1  2, is3_1 ,two_hex
            is3_1:
                getHexDigit_1  3, is4_1 ,three_hex
            is4_1:
                getHexDigit_1  4, is5_1 ,four_hex
            is5_1:
                getHexDigit_1  5, is6_1 ,five_hex
            is6_1:
                getHexDigit_1  6, is7_1 ,six_hex
            is7_1:
                getHexDigit_1  7, is8_1 ,seven_hex
            is8_1:
                getHexDigit_1  8, is9_1 ,eight_hex
            is9_1:
                getHexDigit_1  9, isA_1 ,nine_hex
            isA_1:
                getHexDigit_1  10, isB_1 ,A_hex
            isB_1:
                getHexDigit_1  11, isC_1 ,B_hex
            isC_1:
                getHexDigit_1  12, isD_1 ,C_hex
            isD_1:
                getHexDigit_1  13, isE_1 ,D_hex
            isE_1:
                getHexDigit_1  14, isF_1 ,E_hex
            isF_1:
                getHexDigit_1  15, doConversion_1 ,F_hex

            jmp doConversion_1
    exitDec2hex:
        mov BYTE [hexTemp+r9], 0 ; make it zero terminated
        ret
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%macro getBinDigit 3
    ; e.g. getBinDigit d_bin, isE ,d_hex
    lea rsi, [rbx + r8] ;r8(index on hex)
    mov rdi, %3
    mov rcx, 1
    repe cmpsb
    jne %2
    ; it was d
    inc r8
    mov rsi, %1
    lea rdi, [binaryTemp + r9] ;r9(index on bin)
    mov rcx, 4
    rep movsb
    add r9, 4
%endmacro
hex2bin:
    ; start address of hex in rbx
    ; Converts a hex string to binary
    ; reverse of bin2hex
    mov rdi, rbx
    call GetStrlen ; len in rdx

    mov r15, rdx ; save len
    preDoConversion_2:
        xor r8, r8 ; iterator on hex
        xor r9, r9 ; iterator on binary
        doConversion_2:
            cmp r8, r15 ; len
            je exitHex2bin

            is0_2:
                getBinDigit zero_bin, is1_2 ,zero_hex
            is1_2:
                getBinDigit one_bin, is2_2 ,one_hex
            is2_2:
                getBinDigit two_bin, is3_2 ,two_hex
            is3_2:
                getBinDigit three_bin, is4_2 ,three_hex
            is4_2:
                getBinDigit four_bin, is5_2 ,four_hex
            is5_2:
                getBinDigit five_bin, is6_2 ,five_hex
            is6_2:
                getBinDigit six_bin, is7_2 ,six_hex

            is7_2:
                getBinDigit seven_bin, is8_2 ,seven_hex
            is8_2:
                getBinDigit eight_bin, is9_2 ,eight_hex
            is9_2:
                getBinDigit nine_bin, isA_2 ,nine_hex
            isA_2:
                getBinDigit A_bin, isB_2 ,A_hex
            isB_2:
                getBinDigit B_bin, isC_2 ,B_hex
            isC_2:
                getBinDigit C_bin, isD_2 ,C_hex
            isD_2:
                getBinDigit D_bin, isE_2 ,D_hex
            isE_2:
                getBinDigit E_bin, isF_2 ,E_hex
            isF_2:
                getBinDigit F_bin, doConversion_2 ,F_hex

            jmp doConversion_2
    exitHex2bin:
        mov BYTE [binaryTemp+r9], 0 ; make it zero terminated
        ret
;--------------------------------------------------------;
;---------------Data OR Disp TO Memory-------------------;

; Main functions in this file:
;   dataORdisp_to_memory_extend-> 
;            Start address of imd or disp in rsi and
;            Number of bits to extend in rax.
;            Converts a decimal or hex imd or disp
;            to binary rep in memory and saved in dataORdisp.
dataORdisp_to_memory_extend:
    ; data beginning address in rsi, 
    ; number of extended bits in rax
    ; returns the binary in dataORdisp

    ; ex: 
    ;      24(0x18)  , 16 -> 18 00 -> 0001 1000 0000 0000 
    ;         0x24   , 16 -> 24 00 -> 0010 0100 0000 0000 

    ;    2425(0x979) , 16 -> 79 09 -> 0111 1001 0000 1001 
    ;         0x2425 , 16 -> 25 24 -> 0010 0101 0010 0100 

    ; step1: if it's not hex (i.e. start with 0x)
    ;        convert dec to hex
        ; e.g. noArgMacro stc_, isSTD, stc_bin, 8
    
    mov r15, rsi ; save address
    mov r14, rax ; save extend bits

    mov qword [isOddFlag], 0 ; reset it 

    mov rdi, hexIndicator
    mov rcx, 2
    repe cmpsb
    je preReverse

    isDecimal:
        ; it is decimal

        ; start address of dec string in rsi
        mov rsi, r15
        ; Converts a dec string to hex
        call dec2hex ; hex in hexTemp
        ;mov rsi, hexTemp
        ;call printString

        mov r15, hexTemp ; update address
        jmp reverse

    preReverse:
        ; remove 0x from the beginning
        add r15, 2
    reverse:
        checkOneDigit:
            ; if one digit like 9 -> make it 09
            mov rdi, r15
            call GetStrlen
            cmp rdx, 1
            jne reverseCont

            mov bl, [r15]
            mov BYTE [r15], '0'
            inc r15
            mov [r15], bl
            inc r15
            mov BYTE [r15], 0 ; make it zero terminated
            sub r15, 2
        reverseCont:
        ; step2: reverse word by word
        mov rdi, r15
        call GetStrlen ; len in rdx
        mov r13, rdx ; save len

        shr rdx,1 ; len/2
        jnc reverseNext; carry=0 -> is even
        ; odd needs a leading zero!
        mov qword [isOddFlag], 1 

        reverseNext:
            lea rsi, [r15+r13-2] ; end of string
            mov rdi, tempReversedHex
            mov rcx, rdx ; half the len
            cld
            reverseLoop:
                mov r8w, [rsi]
                mov [rdi], r8w
                sub rsi, 2
                add rdi, 2
                loop reverseLoop
        
        cmp qword [isOddFlag], 1
        jne reverseExit

        mov al, '0'
        mov [rdi], al
        inc rdi
        inc rsi
        mov al, [rsi]
        mov [rdi], al 
        inc rdi
    reverseExit:
        mov BYTE [rdi], 0 ; make it zero terminated

    extend:
        ; step3: extend
        mov rdi, tempReversedHex
        call GetStrlen ; len in rdx
        mov r10, rdx ; save len
        ; r14: number of bits needed
        xor rdx, rdx
        mov rax, r14
        mov r8, 4
        idiv r8 ; bits needed/4 = number of hex chars

        cmp r10, rax
        ja convertH2B

        ; needs extention
        sub rax, r10
        mov rcx, rax
        mov rdx, rax ; save for later
        mov al, '0'
        lea rdi, [tempReversedHex+r10]
        rep stosb

        add r10, rdx
        mov BYTE [tempReversedHex+r10], 0 ; make it zero terminated

    convertH2B:
        ; step4: convert hex to binary
        mov rbx, tempReversedHex
        call hex2bin ; res in binaryTemp

        mov rdi, binaryTemp
        call GetStrlen ; len in rdx
        mov rcx, rdx
        mov rsi, binaryTemp
        mov rdi, dataORdisp
        rep movsb

        mov al, 0 ; make it zero terminated
        stosb 

    ret

;--------------------------------------------------;

;---------------Python Functions-------------------;
; Main functions in this file:
;   isNumeric->     Takes a char in al, checks if it's a number
;                   Sets carry flag if it's a number
isNumeric:
    push rdi
    clc
    ; char in al
    mov rdi, numbers
    mov rcx, 11
    repne scasb
    cmp rcx, 0
    je noIsNumeric
    jne yesIsNumeric
    noIsNumeric:
        clc
        jmp exitIsNumeric
    yesIsNumeric:
        stc
        jmp exitIsNumeric
    exitIsNumeric:
        pop rdi
        ret
;--------------------------------------------------;

;---------------Utility Functions-------------------;
; Main functions in this file:
;   getImdOrDispSize-> Start address of hex in rsi
;                      Returns size in rax
getImdOrDispSize:
    ; Start address in rsi
    ; Returns size in rax  
    ; sets the d_s field

    mov r15, rsi 
    call isNumDecimal
    jnc contGetImdOrDispSize
 
    mov rsi, r15
    call dec2hex ;hexTemp
    mov r15, hexTemp
    
    sub r15, 2
    contGetImdOrDispSize:
        add r15, 2
        mov rdi, r15

        call GetStrlen
        cmp rdx, 2
        je checkIs8_size
        jg is32_size
        jb is8_size

        checkIs8_size:
        ; len=2
        mov al, 'f'
        inc r15
        cmp [r15], al
        jne is8_size

        ; 0x7f or 0xff?
        dec r15
        mov al, 'f'
        cmp [r15], al
        jne is8_size
        je is32_size
        is8_size:
            ; 0x0 to 0x7f -> 8 bit
            mov rax, 8
            cmp BYTE [d_s], ' ' 
            jne exitGetImdOrDispSize
            mov BYTE [d_s], '1' 
            jmp exitGetImdOrDispSize
        ;is16:
        ;    ; 0xff to ... -> 32 bit
        ;    mov rax, 16
        ;    jmp exitGetImdOrDispSize
        is32_size:
            ; 0xff to ... -> 32 bit
            mov rax, 32
            cmp BYTE [d_s], ' ' 
            jne exitGetImdOrDispSize
            mov BYTE [d_s], '0' 
            jmp exitGetImdOrDispSize
        exitGetImdOrDispSize:
            ret
isNumDecimal:
    ; start address in rsi
    ; sets carry flag if it's decimal
    mov rdi, hexIndicator_
    mov rcx, 2
    repe cmpsb
    je NoIsNumDecimal
    jne YesIsNumDecimal

    NoIsNumDecimal:
        clc
        jmp exitIsNumDecimal
    YesIsNumDecimal:
        stc
        jmp exitIsNumDecimal
    exitIsNumDecimal:
        ret

;---------------------------------------------;

;---------------OTHER STUFF-------------------;

%macro oneArgMacroReg 5
    ; e.g. oneArgMacro push_, push_reg, push_bin, isPOP, postProcessPUSH
    mov rsi, command
    mov rdi, %1
    mov rcx, 3
    repe cmpsb
    jne %4
    ; it was PUSH
    ; save reg opcode
    mov rsi, %2
    mov rdi, regOpcode
    mov rcx, 3
    rep movsb

    ; Store binary Opcode
    appendString binary, %3
            
    jmp %5
%endmacro

setDefaults:
    ; reset some flags
    mov QWORD [isHandeledF],      0
    mov QWORD [hasMemoryF],       0
    mov QWORD [isNewRegF],        0
    mov QWORD [endOnSuccessF],    0
    mov QWORD [isBSR_BSF_IMUL_F], 0

    mov BYTE [binary], 0
    mov BYTE [hex], 0
    mov BYTE [hexTemp], 0
    mov BYTE [mod], 0
    mov BYTE [reg], 0
    mov BYTE [rm], 0
    mov BYTE [base], 0
    mov BYTE [scale], 0
    mov BYTE [baseReg], 0
    mov BYTE [index], 0
    mov BYTE [indexReg], 0
    mov BYTE [disp], 0
    mov BYTE [imd], 0
    mov BYTE [dataORdisp], 0

    ret
hasMemory:
    mov rdi, command
    call GetStrlen ; len in rdx
    mov byte al, '['
    mov rcx, rdx
    mov rdi, command
    repne scasb
    cmp rcx, 0
    je exitHasMemory

    ; has memory 
    xor rax, rax
    mov rax, 1
    mov [hasMemoryF], rax
    exitHasMemory:
        ret

%macro oneArgMacro 3
    ; e.g. oneArgMacro push_, isPOP, postProcessPUSH
    mov rsi, command
    mov rdi, %1
    mov rcx, 3
    repe cmpsb
    jne %2
    ; it was PUSH
    jmp %3
%endmacro
%macro compReg 4
    ; e.g. compReg al_, isAH, a_r8_bin, 2
    mov rsi, rbx
    mov rdi, %1
    mov rcx, %4
    repe cmpsb
    jne %2
    ; it was al
    mov rsi, %3
    lea rdi, [regTempBinary]
    mov rcx, 3
    rep movsb
    mov al, 0 ; make it zero terminated
    stosb
    jmp exitGetRegBin
%endmacro

%macro compRegNoModify 4
    ; reg should be in rbx
    ; e.g. compRegNoModify al_, isAH, 2, postProcessAL
    mov rsi, rbx
    mov rdi, %1
    mov rcx, %3
    repe cmpsb
    jne %2
    ; it was al
    jmp %4
%endmacro

%macro compSize 3
    ; e.g. compSize BYTE_, isWORD, BYTE_n
    mov rsi, memoryMunch
    mov rdi, %1
    mov rcx, 1
    repe cmpsb
    jne %2
    ; it was BYTE
    mov rax, [%3]
    mov [registerSize], rax
    jmp exitSetOperationSize
%endmacro


%macro getBetweenChars 3
    ; example 'BYTE PTR [rbx+rcx], '[', '+' -> returns rbx in between
    mov r14, %1
    mov r13b, %2
    call isCharInString ; rdi is the address
    mov r8, rdi ; start address
    call GetStrlen
    mov r11, rdx ; len of start

    ; ending point
    mov r14, %1
    mov r13b, %3
    call isCharInString ; rdi is the address
    dec rdi
    call GetStrlen
    mov r10, rdx ; len of end

    sub r11, r10

    copyString [between], [r8], r11
%endmacro

%macro compStrings 4
    ; e.g. compStrings command, push_, 3, isPush
    mov rsi, %1
    mov rdi, %2
    mov rcx, %3
    repe cmpsb
    je %4
%endmacro

isCharInString: 
    ; arg1 in r14, arg2 in r13b

    ; sets r15 to 1 if arg2 is in arg1
    ; if found, rdi would be the address
    ;           len-rcx is the index

    xor r15, r15 

    mov rdi, r14
    call GetStrlen ; len in rdx
    inc rdx 

    mov byte al, r13b
    mov rcx, rdx
    mov rdi, r14
    repne scasb
    cmp rcx, 0
    je notFound

    ; is found!
    mov r15, 1 
    jmp exitIsCharInString

    notFound:
        xor r15, r15

    exitIsCharInString:
        ret

getReg:
    ; start address of command in rdi
    ; returns the reg name in 'register' data

    mov r14, rdi ; save start addr
    call GetStrlen ; len in rdx
    mov r13, rdx ; save len 

    mov rdi, r14
    mov al, ' '
    mov rcx, rdx
    repne scasb  ; find space
    
    ;len of reg name in rcx
    ;start of reg in    rdi
    mov rsi, rdi
    mov rdi, register
    rep movsb
    mov al, 0 ; make it zero terminated
    stosb 

    ret

getRegSize:
    ; reset flag
    mov QWORD [isNewRegF], 0

    ; returns the size of register with name address start in rsi data
    ; in 'regSizeTemp' data
    mov al, [rsi+1]
    call isNumeric
    jc isNewRegSize
    ; is old register
    cmp BYTE [rsi+1], 'h'
    je isReg8
    cmp BYTE [rsi+1], 'l'
    je isReg8
    cmp BYTE [rsi], 'e'
    je isReg32
    cmp BYTE [rsi], 'r'
    je isReg64
    jne isReg16
    isNewRegSize:
        ; is new register
        mov QWORD [isNewRegF], 1

        cmp BYTE [rsi+2], 'b' ; r8b
        je isReg8
        cmp BYTE [rsi+3], 'b' ; r12b
        je isReg8

        cmp BYTE [rsi+2], 'd' ; r8d
        je isReg32
        cmp BYTE [rsi+3], 'd' ; r12d
        je isReg32

        cmp BYTE [rsi+2], 'w' ; r8w
        je isReg16
        cmp BYTE [rsi+3], 'w' ; r12w
        je isReg16

        jne isReg64

    isReg8:
        mov rax, 8
        mov [regSizeTemp], rax
        jmp exitGetRegSize
    isReg16:
        mov rax, 16
        mov [regSizeTemp], rax    
        jmp exitGetRegSize
    isReg32:
        mov rax, 32
        mov [regSizeTemp], rax
        jmp exitGetRegSize
    isReg64:
        mov rax, 64
        mov [regSizeTemp], rax
        jmp exitGetRegSize
    exitGetRegSize:
        ret
getRegBin:
    ; takes reg name in rbx, reg size addr in rdx,
    ; returns binary in regTempBinary
    cmp rdx, 8
    je getRegBin8
    cmp rdx, 16
    je getRegBin16
    cmp rdx, 32
    je getRegBin32
    cmp rdx, 64
    je getRegBin64
    getRegBin8:
        isAL:
            compReg al_, isAH, a_r8_bin, 2
        isAH:
            compReg ah_, isCL, ah_sp_r12_bin, 2
        isCL:
            compReg cl_, isCH, c_r9_bin, 2
        isCH:
            compReg ch_, isDL, ch_bp_r13_bin, 2
        isDL:
            compReg dl_, isDH, d_r10_bin, 2
        isDH:
            compReg dh_, isBL, dh_si_r14_bin, 2
        isBL:
            compReg bl_, isBH, b_r11_bin, 2
        isBH:
            compReg bh_, isR8B, bh_di_r15_bin, 2
        isR8B:
            compReg r8b_, isR9B, a_r8_bin, 2
        isR9B:
            compReg r9b_, isR10B, c_r9_bin, 2
        isR10B:
            compReg r10b_, isR11B, d_r10_bin, 3
        isR11B:
            compReg r11b_, isR12B, b_r11_bin, 3
        isR12B:
            compReg r12b_, isR13B, ah_sp_r12_bin, 3
        isR13B:
            compReg r13b_, isR14B, ch_bp_r13_bin, 3
        isR14B:
            compReg r14b_, isR15B, dh_si_r14_bin, 3
        isR15B:
            compReg r15b_, exitGetRegBin, bh_di_r15_bin, 3
    getRegBin16:
        isAX:
            compReg ax_, isCX, a_r8_bin, 2
        isCX:
            compReg cx_, isDX, c_r9_bin, 2
        isDX:
            compReg dx_, isBX, d_r10_bin, 2
        isBX:
            compReg bx_, isSP, b_r11_bin, 2

        isSP:
            compReg sp_, isBP, ah_sp_r12_bin, 2
        isBP:
            compReg bp_, isSI, ch_bp_r13_bin, 2
        isSI:
            compReg si_, isDI, dh_si_r14_bin, 2
        isDI:
            compReg di_, isR8W, bh_di_r15_bin, 2
        isR8W:
            compReg r8w_, isR9W, a_r8_bin, 3
        isR9W:
            compReg r9w_, isR10W, c_r9_bin, 3
        isR10W:
            compReg r10w_, isR11W, d_r10_bin, 3
        isR11W:
            compReg r11w_, isR12W, b_r11_bin, 3
        isR12W:
            compReg r12w_, isR13W, ah_sp_r12_bin, 3
        isR13W:
            compReg r13w_, isR14W, ch_bp_r13_bin, 3
        isR14W:
            compReg r14w_, isR15W, dh_si_r14_bin, 3
        isR15W:
            compReg r15w_, exitGetRegBin, bh_di_r15_bin, 3

    getRegBin32:
        isEAX:
            compReg eax_, isECX, a_r8_bin, 3
        isECX:
            compReg ecx_, isEDX, c_r9_bin, 3
        isEDX:
            compReg edx_, isEBX, d_r10_bin, 3
        isEBX:
            compReg ebx_, isESP, b_r11_bin, 3
        isESP:
            compReg esp_, isEBP, ah_sp_r12_bin, 3
        isEBP:
            compReg ebp_, isESI, ch_bp_r13_bin, 3
        isESI:
            compReg esi_, isEDI, dh_si_r14_bin, 3
        isEDI:
            compReg edi_, isR8D, bh_di_r15_bin, 3

        isR8D:
            compReg r8d_, isR9D, a_r8_bin, 3
        isR9D:
            compReg r9d_, isR10D, c_r9_bin, 3
        isR10D:
            compReg r10d_, isR11D, d_r10_bin, 3
        isR11D:
            compReg r11d_, isR12D, b_r11_bin, 3
        isR12D:
            compReg r12d_, isR13D, ah_sp_r12_bin, 3
        isR13D:
            compReg r13d_, isR14D, ch_bp_r13_bin, 3
        isR14D:
            compReg r14d_, isR15D, dh_si_r14_bin, 3
        isR15D:
            compReg r15d_, exitGetRegBin, bh_di_r15_bin, 3

    getRegBin64:
        isRAX:
            compReg rax_, isRCX, a_r8_bin, 3
        isRCX:
            compReg rcx_, isRDX, c_r9_bin, 3
        isRDX:
            compReg rdx_, isRBX, d_r10_bin, 3
        isRBX:
            compReg rbx_, isRSP, b_r11_bin, 3

        isRSP:
            compReg rsp_, isRBP, ah_sp_r12_bin, 3
        isRBP:
            compReg rbp_, isRSI, ch_bp_r13_bin, 3
        isRSI:
            compReg rsi_, isRDI, dh_si_r14_bin, 3
        isRDI:
            compReg rdi_, isR8, bh_di_r15_bin, 3

        isR8:
            compReg r8_, isR9, a_r8_bin, 2
        isR9:
            compReg r9_, isR10, c_r9_bin, 2
        isR10:
            compReg r10_, isR11, d_r10_bin, 3
        isR11:
            compReg r11_, isR12, b_r11_bin, 3
        isR12:
            compReg r12_, isR13, ah_sp_r12_bin, 3
        isR13:
            compReg r13_, isR14, ch_bp_r13_bin, 3
        isR14:
            compReg r14_, isR15, dh_si_r14_bin, 3
        isR15:
            compReg r15_, exitGetRegBin, bh_di_r15_bin, 3

    exitGetRegBin:
        ret
getRegBinNoSize:
    ; takes reg name in rbx
    ; returns binary in regTempBinary
    mov rsi, rbx
    call getRegSize
    mov rdx, [regSizeTemp]
    call getRegBin
    ret
addAddrPrefix:
    ; check if we even had memory!
    cmp BYTE [memoryMunch], ' '
    je exitAddAddrPrefix

    ; add address prefix 67 if we have indirect addressing and 
    ; base in memory is 32 bits

    ; check for direct address
    mov r14, command
    mov r13b, '['
    call isCharInString ; rdi is the address
    mov al, [rdi]
    call isNumeric
    jc exitAddAddrPrefix
    jnc checkFor32

    checkFor32:
        ; takes reg name in rbx
        ; returns binary in regTempBinary
        mov rsi, baseReg
        call getRegSize
        cmp QWORD [regSizeTemp], 32
        jne exitAddAddrPrefix
        
        ; is 32, add 67
        ; add oper_prefix "01100110" to binary
        appendString binary, addr_prefix

    exitAddAddrPrefix:
        ret
addOperPrefix:
    cmp QWORD [registerSize], 16
    jne exitAddOperPrefix

    ; add oper_prefix "01100110" to binary
    appendString binary, oper_prefix

    exitAddOperPrefix:
        ret

addRexNoMemory:
    ; 1: r|r or 4: r|imd
    ; check for newRegs in mod and reg 
    ; check for 64 bit registers

    ; r extends oper1 (reg)
    ; x extends oper2 - no SIB no USE -> 0
    ; b exentds oper3 (r/m)

    cmp QWORD [type], 1
    je twoRegs
    jne oneReg__

    oneReg__:
        ; one reg in register 
        mov rsi, register
        call getRegSize
        cmp QWORD [isNewRegF], 1
        je oneReg_extend_r
        jne oneReg_extend_w
        oneReg_extend_r:
            cmp QWORD [regSizeTemp], 64
            je w1_r0_b1
            jne exitAddRexNoMemory
        oneReg_extend_w:
            cmp QWORD [regSizeTemp], 64
            je w1_r0_b0
            jne exitAddRexNoMemory
    twoRegs:
       ; reg name in reg1, extends r
       ; rm  name in reg2, extends b
        mov rsi, reg1
        call getRegSize
        cmp QWORD [isNewRegF], 1
        je reg1_new
        jne reg1_old
        reg1_new:
            cmp QWORD [regSizeTemp], 64
            je w1_r1
            jne w0_r1
        reg1_old:
            cmp QWORD [regSizeTemp], 64
            je w1_r0
            jne w0_r0
            
            ; check second reg
            w1_r1:
                mov rsi, reg2
                call getRegSize
                cmp QWORD [isNewRegF], 1
                je w1_r1_b1
                jne w1_r1_b0

            w1_r0:
                mov rsi, reg2
                call getRegSize
                cmp QWORD [isNewRegF], 1
                je w1_r0_b1
                jne w1_r0_b0

            w0_r1:
                mov rsi, reg2
                call getRegSize
                cmp QWORD [isNewRegF], 1
                je  w_r1_b1
                jne w_r1_b0

            w0_r0:
                mov rsi, reg2
                call getRegSize
                cmp QWORD [isNewRegF], 1
                je  w_r0_b1
                jne w_r0_b0

            w_r1_b1:
                cmp QWORD [regSizeTemp], 64
                je  w1_r1_b1
                jne w0_r1_b1
            w_r1_b0:
                cmp QWORD [regSizeTemp], 64
                je  w1_r1_b0
                jne w0_r1_b0
            w_r0_b1:
                cmp QWORD [regSizeTemp], 64
                je  w1_r0_b1
                jne w0_r0_b1
            w_r0_b0:
                cmp QWORD [regSizeTemp], 64
                je  w1_r0_b0
                jne exitAddRexNoMemory

    w1_r0_b0:
        mov BYTE [rex_w], '1'
        mov BYTE [rex_r], '0'
        mov BYTE [rex_b], '0'
        jmp contAddRexNoMemory
    w1_r1_b0:
        mov BYTE [rex_w], '1'
        mov BYTE [rex_r], '1'
        mov BYTE [rex_b], '0'
        jmp contAddRexNoMemory
    w1_r0_b1:
        mov BYTE [rex_w], '1'
        mov BYTE [rex_r], '0'
        mov BYTE [rex_b], '1'
        jmp contAddRexNoMemory
    w1_r1_b1:
        mov BYTE [rex_w], '1'
        mov BYTE [rex_r], '1'
        mov BYTE [rex_b], '1'
        jmp contAddRexNoMemory
    w0_r1_b0:
        mov BYTE [rex_w], '0'
        mov BYTE [rex_r], '1'
        mov BYTE [rex_b], '0'
        jmp contAddRexNoMemory
    w0_r0_b1:
        mov BYTE [rex_w], '0'
        mov BYTE [rex_r], '0'
        mov BYTE [rex_b], '1'
        jmp contAddRexNoMemory
    w0_r1_b1:
        mov BYTE [rex_w], '0'
        mov BYTE [rex_r], '1'
        mov BYTE [rex_b], '1'
        jmp contAddRexNoMemory

    contAddRexNoMemory:
        mov BYTE [rex_x], '0'
        ; REX: 0100+w+r+x+b

        appendString binary, rex
        appendString binary, rex_w
        appendString binary, rex_r
        appendString binary, rex_x
        appendString binary, rex_b

    exitAddRexNoMemory:
        ret

checkNewReg:
    cmp Byte [register], ' '
    je exitCheckNewReg

    ; check if reg is new to extend r as well!
    mov rsi, register
    call getRegSize
    cmp QWORD [isNewRegF], 1
    jne notNew
    ; is new reg 
    mov BYTE [rex_r], '1'
    jmp exitCheckNewReg
    notNew:
        mov BYTE [rex_r], '0'

    exitCheckNewReg:
        ret
addRexForMemory:
    ; add rex only if we have new reg in memory (could be base, or index!)
    ; or reg could be new 
    ; or registerSize is QWORD, then rex and 

    cmp Byte [indexReg], 0
    je noIndex
    mov rsi, indexReg
    call getRegSize
    cmp QWORD [isNewRegF], 1
    je newIndex

    noIndex:
        mov rsi, baseReg
        call getRegSize
        cmp QWORD [isNewRegF], 1
        je newBaseOnly
        jne checkIfRegIsNew

    newIndex:
        mov rsi, baseReg
        call getRegSize
        cmp QWORD [isNewRegF], 1
        je newIndexBase
        jne newIndexOnly

    newBaseOnly:
        mov BYTE [rex_r], '0'
        mov BYTE [rex_x], '0'
        mov BYTE [rex_b], '1'
        call checkNewReg
        jmp contAddRexForMemory
    newIndexOnly:
        mov BYTE [rex_r], '0'
        mov BYTE [rex_x], '1'
        mov BYTE [rex_b], '0'
        call checkNewReg
        jmp contAddRexForMemory
    newIndexBase:
        mov BYTE [rex_r], '0'
        mov BYTE [rex_x], '1'
        mov BYTE [rex_b], '1'
        call checkNewReg
        jmp contAddRexForMemory
    
    checkIfRegIsNew:
        call checkNewReg
        cmp byte [rex_r], '1'
        jne checkIfOperis64
        mov BYTE [rex_x], '0'
        mov BYTE [rex_b], '0'
    checkIfOperis64:
        cmp QWORD [registerSize], 64
        jne exitAddRexForMemory
        mov byte [rex_r], '0'
        mov BYTE [rex_x], '0'
        mov BYTE [rex_b], '0'
    contAddRexForMemory:
        appendString binary, rex

        ; add w
        cmp QWORD [registerSize], 64
        jne rex_w0_
        ; w is one
            appendString binary, one
            jmp contAddRexForMemory__
        rex_w0_:
            appendString binary, zero

    contAddRexForMemory__:
        appendString binary, rex_r
        appendString binary, rex_x
        appendString binary, rex_b

    exitAddRexForMemory:
        ret

addCodeW:
    ; they have no w! w already in opcode
    cmp QWORD [isBSR_BSF_IMUL_F], 1
    je exitAddCodeW

    cmp QWORD [registerSize], 8
    je codeW0
    jne codeW1
    codeW1:
        appendString binary, one
        jmp exitAddCodeW
    codeW0:
        appendString binary, zero
        jmp exitAddCodeW
    exitAddCodeW:
        ret

setOperationSize:
    isBYTE:
        compSize BYTE_, isWORD, BYTE_n
    isWORD:
        compSize WORD_, isDWORD, WORD_n
    isDWORD:
        compSize DWORD_, isQWORD, DWORD_n
    isQWORD:
        compSize QWORD_, exitSetOperationSize, QWORD_n 
    exitSetOperationSize:
        ret

setScale:
    ; start address of scale in rsi 
    cmp BYTE [rsi], '1'
    je is00
    cmp BYTE [rsi], '2'
    je is01
    cmp BYTE [rsi], '4'
    je is10
    cmp BYTE [rsi], '8'
    je is11

    is00:
        appendString scale, zero
        appendString scale, zero
        jmp esitSetScale
    is01:
        appendString scale, zero
        appendString scale, one
        jmp esitSetScale
    is10:
        appendString scale, one
        appendString scale, zero
        jmp esitSetScale
    is11:
        appendString scale, one
        appendString scale, one
        jmp esitSetScale
    esitSetScale:
        ret

extractMemory:
    ; returns the memory munch of command in memoryMunch.
    ; adds operation size to registerSize
    ; fills scale, index, base, disp if there are any.
    
    ; inc BYTE PTR [rbx+rcx*8+0x32]
    ; add BYTE PTR [rbx+rcx*8+0x32],al
    ; add al,BYTE PTR [rbx+rcx*8+0x32]

    mov r14, command
    mov r13b, ','
    call isCharInString
    cmp r15, 1
    je isTwoArgMemory
    jne isOneArgMemory

    isOneArgMemory:
        mov r14, command
        mov r13b, ' '
        call isCharInString ; rdi is the address
        mov r15, rdi ; save start address
        call GetStrlen
        
        copyString [memoryMunch], [r15], rdx

        jmp setSize
    isTwoArgMemory:
        ; also saves the register or imd (second arg)
        mov r14, command
        mov r13b, ' '
        call isCharInString ; rdi is the address

        cmp BYTE [rdi], 'B'
        je firstArgMemory
        cmp BYTE [rdi], 'W'
        je firstArgMemory
        cmp BYTE [rdi], 'D'
        je firstArgMemory
        cmp BYTE [rdi], 'Q'
        je firstArgMemory        
        jne secondArgMemory

        firstArgMemory:
            mov r11, rdi ; save start address
            call GetStrlen
            mov r12, rdx ; save
            mov r14, command
            mov r13b, ','
            call isCharInString ; rdi is the address
            call GetStrlen
            sub r12, rdx
            dec r12
            copyString [memoryMunch], [r11], r12

            ; save the second arg
            mov r14, command
            mov r13b, ','
            call isCharInString ; rdi is the address
            mov al, [rdi]
            call isNumeric
            jc secondArgImd
            jnc secondArgReg
            secondArgImd:
                mov r15, rdi 
                call GetStrlen
                copyString [imd], [r15], rdx

                ; 5: m|imd: memory    | immediate
                mov BYTE [d_s], '1'
                mov QWORD [type], 5

                jmp setSize
            secondArgReg:
                mov r15, rdi 
                call GetStrlen
                copyString [register], [r15], rdx

                ; 3: m|r:   memory    | register
                mov BYTE [d_s], '0'
                mov QWORD [type], 3

                jmp setSize
        secondArgMemory:
            ; save the first arg
            getBetweenChars command, ' ', ',' ; get reg
            mov rdi, between 
            call GetStrlen
            copyString [register], [between], rdx

            ; 2: r|m:   register  | memory
            mov BYTE [d_s], '1'
            mov QWORD [type], 2

            jmp saveMemory

            saveMemory:
            ; save memory
            mov r14, command
            mov r13b, ','
            call isCharInString ; rdi is the address
     
            mov r15, rdi ; save start address
            call GetStrlen
        
            copyString [memoryMunch], [r15], rdx
            jmp setSize

    setSize:
        ; now memory is in memoryMunch
        call setOperationSize

    checkDirect:
        mov r14, command
        mov r13b, '['
        call isCharInString ; rdi is the address
        mov al, [rdi]
        call isNumeric
        jc isDirect
        jnc isIndirect

    isDirect:
        ; BYTE PTR [0x32]
        ; must use SIB
        ; mod: 00, rm: 100, scale:00, index:100
        ; base: 101, disp: extend to 32
        appendString mod, zero
        appendString mod, zero

        appendString rm, one
        appendString rm, zero
        appendString rm, zero

        appendString scale, zero
        appendString scale, zero

        appendString index, one
        appendString index, zero
        appendString index, zero

        appendString base, one
        appendString base, zero
        appendString base, one

        getBetweenChars memoryMunch, '[', ']'
                
        lea rsi, [between] ; start of disp 
        mov rax, 32
        call dataORdisp_to_memory_extend ; binary disp in dataORdisp

        mov rdi, dataORdisp
        call GetStrlen
        copyString [disp], [dataORdisp], rdx

        cmp QWORD [registerSize], 64
        je set_rbp_asBase
        jne set_ebp_asBase
        
        set_rbp_asBase:
            copyString [baseReg], [rbp_], 3
            jmp exitExtractMemory1
        set_ebp_asBase:
            copyString [baseReg], [ebp_], 3
            jmp exitExtractMemory1

    isIndirect:
        ; BYTE PTR [rbx+rcx*8+0x32]
        mov r14, memoryMunch
        mov r13b, '*'
        call isCharInString ; rdi is the address
        cmp r15, 1 
        jne noScale
        je hasScale

        noScale:
            ; memory = [ebx] or 
            ;          [ebx+disp] or 
            ;          [ebx+ecx]
            mov r14, memoryMunch
            mov r13b, '+'
            call isCharInString ; rdi is the address
            cmp r15, 1 
            je twoReg
            jne oneReg
                oneReg:
                    ; indirect no scale(one reg) -> no SIB
                    ; e.g. [ebx]

                    getBetweenChars memoryMunch, '[', ']'

                    mov rdi, between 
                    call GetStrlen
                    ; get the base 
                    copyString [baseReg], [between], rdx

                    mov rbx, baseReg

                    compRegNoModify r12_, isEBP_, 3, postProcessR12
                    isEBP_:
                        compRegNoModify ebp_, isRBP_, 3, postProcessEBP
                    isRBP_:
                        compRegNoModify rbp_, oneReg_noException, 3, postProcessRBP

                    postProcessR12:
                    ; mod: 00, rm: 100, scale:00, index:100
                    ; base:100 , baseReg:r12
                    appendString mod, zero
                    appendString mod, zero

                    appendString rm, one
                    appendString rm, zero
                    appendString rm, zero

                    appendString scale, zero
                    appendString scale, zero

                    appendString index, one
                    appendString index, zero
                    appendString index, zero

                    appendString base, one
                    appendString base, zero
                    appendString base, zero
                    
                    jmp oneReg_sharedAll
                    postProcessEBP:
                    ; mod: 01,
                    ; base:101 , baseReg:ebp
                    ; disp 0x0, 8 bits
                    appendString mod, zero
                    appendString mod, one
                    
                    lea rsi, [zero] ; zero disp
                    mov rax, 8
                    call dataORdisp_to_memory_extend ; binary disp in dataORdisp
                    
                    mov rdi, dataORdisp
                    call GetStrlen
                    copyString [disp], [dataORdisp], rdx

                    jmp oneReg_sharedAll
                    postProcessRBP:
                    ; mod: 10,
                    ; base:101 , baseReg:rbp
                    ; disp 0x0, 32 bits
                    appendString mod, one
                    appendString mod, zero
                    
                    lea rsi, [zero] ; zero disp
                    mov rax, 32
                    call dataORdisp_to_memory_extend ; binary disp in dataORdisp
                    
                    mov rdi, dataORdisp
                    call GetStrlen
                    copyString [disp], [dataORdisp], rdx

                    jmp oneReg_sharedAll

                    oneReg_noException:
                        ; mod: 00
                        appendString mod, zero
                        appendString mod, zero
                    oneReg_sharedAll:
                        ; get rm

                        mov rbx, baseReg
                        call getRegBinNoSize ; bin in regTempBinary
                        
                        copyString [rm], [regTempBinary], 3

                        jmp exitExtractMemory1
                        
                twoReg:
                    getBetweenChars memoryMunch, '[', '+'

                    mov rdi, between 
                    call GetStrlen
                    ; get the base 
                    copyString [baseReg], [between], rdx

                    mov r14, memoryMunch
                    mov r13b, '+'
                    call isCharInString ; rdi is the address
                    mov al, [rdi]
                    call isNumeric
                    jc regPlusDisp
                    jnc regPlusReg

                    regPlusDisp:
                        ; mod: 10, 
                        ; [ebx+disp], [r8+disp]

                        ; get base

                        getBetweenChars memoryMunch, '[', '+'
                        mov rdi, between 
                        call GetStrlen
                        ; get the index 
                        copyString [baseReg], [between], rdx


                        getBetweenChars memoryMunch, '+', ']' ; get disp
                        mov rsi, between      ; disp
                        call getImdOrDispSize ; size in rax

                        cmp rax, 8
                        je disp8bit
                        jne disp32bit

                        disp8bit:
                            ; mod: 01
                            appendString mod, zero
                            appendString mod, one
                            lea rsi, [between] ; start address of disp
                            mov rax, 8
                            call dataORdisp_to_memory_extend ; binary disp in dataORdisp
                                    
                            mov rdi, dataORdisp
                            call GetStrlen
                            copyString [disp], [dataORdisp], rdx

                            mov rdi, baseReg
                            call GetStrlen
                            mov rbx, baseReg
                            call getRegBinNoSize ; bin in regTempBinary

                            copyString [rm], [regTempBinary], 3
                            jmp exitExtractMemory1

                        disp32bit:
                            ; mod: 10
                            appendString mod, one
                            appendString mod, zero
                            lea rsi, [between] ; start address of disp
                            mov rax, 32
                            call dataORdisp_to_memory_extend ; binary disp in dataORdisp

                            mov rdi, dataORdisp
                            call GetStrlen
                            copyString [disp], [dataORdisp], rdx

                            mov rdi, baseReg
                            call GetStrlen
                            mov rbx, baseReg
                            call getRegBinNoSize ; bin in regTempBinary

                            copyString [rm], [regTempBinary], 3
                            jmp exitExtractMemory1

                    regPlusReg:
                        ; [ebx+ecx], [r8+r9]
                        getBetweenChars memoryMunch, '+', ']'

                        mov rdi, between 
                        call GetStrlen
                        ; get the index 
                        copyString [indexReg], [between], rdx

                        mov rbx, baseReg

                        isEBP1_:
                            compRegNoModify ebp_, isRBP1_, 3, postProcessEBP1
                        isRBP1_:
                            compRegNoModify rbp_, twoReg_noException, 3, postProcessRBP1
                        postProcessEBP1:
                            ; mod: 01,
                            ; base:101 , baseReg:ebp
                            ; disp 0x0, 8 bits
                            appendString mod, zero
                            appendString mod, one
                            
                            lea rsi, [zero] ; zero disp
                            mov rax, 8
                            call dataORdisp_to_memory_extend ; binary disp in dataORdisp
                            
                            mov rdi, dataORdisp
                            call GetStrlen
                            copyString [disp], [dataORdisp], rdx

                            jmp twoReg_sharedAll
                        postProcessRBP1:
                            ; mod: 10,
                            ; base:101 , baseReg:ebp
                            ; disp 0x0, 32 bits
                            appendString mod, zero
                            appendString mod, one
                            
                            lea rsi, [zero] ; zero disp
                            mov rax, 32
                            call dataORdisp_to_memory_extend ; binary disp in dataORdisp

                            mov rdi, dataORdisp
                            call GetStrlen
                            copyString [disp], [dataORdisp], rdx

                            jmp twoReg_sharedAll
                        twoReg_noException:
                            appendString mod, zero
                            appendString mod, zero
                        twoReg_sharedAll:
                        ; rm: 100, scale: 00
                        appendString rm, one
                        appendString rm, zero
                        appendString rm, zero

                        appendString scale, zero
                        appendString scale, zero

                        ; set base binary 
                        mov rbx, baseReg
                        call getRegBinNoSize ; bin in regTempBinary
                        
                        copyString [base], [regTempBinary], 3

                        ; set index binary 
                        mov rbx, indexReg
                        call getRegBinNoSize ; bin in regTempBinary
                        
                        copyString [index], [regTempBinary], 3

                        jmp exitExtractMemory1
        hasScale:
            mov r14, memoryMunch
            mov r13b, '['
            call isCharInString ; rdi is the address
            cmp BYTE [rdi+3], '*'
            je hasScaleNoBase
            cmp BYTE [rdi+2], '*'
            je hasScaleNoBase
            jne hasScaleHasBase  
            
            hasScaleNoBase:
                ; indirect no base only scale
                ; mod:00, rm:100, base:101 

                appendString mod, zero
                appendString mod, zero

                appendString rm, one
                appendString rm, zero
                appendString rm, zero

                getBetweenChars memoryMunch, '[', '*'

                mov rdi, between 
                call GetStrlen
                ; get the index 
                copyString [indexReg], [between], rdx

                ; set index binary 
                mov rbx, indexReg
                call getRegBinNoSize ; bin in regTempBinary
                    
                copyString [index], [regTempBinary], 3
                
                appendString base, one
                appendString base, zero
                appendString base, one

                ; get reg size
                mov rsi, indexReg
                call getRegSize ; size in [regSizeTemp]

                cmp QWORD [regSizeTemp], 64
                je addBaseRBP
                jne addBaseEBP
                addBaseRBP:
                    copyString [baseReg], [rbp_], 3 
                    jmp checkForDisp
                addBaseEBP:
                    copyString [baseReg], [ebp_], 3 
                    jmp checkForDisp

                checkForDisp:
                    mov r14, memoryMunch
                    mov r13b, '+'
                    call isCharInString ; rdi is the address
                    cmp r15, 1
                    je hasScaleNoBaseHasDisp
                    jne hasScaleNoBaseNoDisp  

                hasScaleNoBaseHasDisp:
                    ; only scale and disp8/16/32 -> 
                    ; mod=00, base=ebp, disp8,disp16->disp32  -> 
                    ; e.g. [ecx*8+0x06]

                    ; set scale 
                    getBetweenChars memoryMunch, '*', '+'
                    mov rsi, between 
                    call setScale

                    ; set disp 
                    getBetweenChars memoryMunch, '+', ']'
                    lea rsi, [between] ;disp
                    mov rax, 32
                    call dataORdisp_to_memory_extend ; binary disp in dataORdisp

                    mov rdi, dataORdisp
                    call GetStrlen
                    copyString [disp], [dataORdisp], rdx

                    jmp exitExtractMemory1

                hasScaleNoBaseNoDisp:
                    ; only scale -> 
                    ; mod=00, base=ebp, disp32=0 -> 
                    ; e.g. [ecx*8]
                    
                    ; set scale 
                    getBetweenChars memoryMunch, '*', ']'
                    mov rsi, between 
                    call setScale

                    ; set disp 
                    lea rsi, [zero] ;disp
                    mov rax, 32
                    call dataORdisp_to_memory_extend ; binary disp in dataORdisp

                    mov rdi, dataORdisp
                    call GetStrlen
                    copyString [disp], [dataORdisp], rdx

                    jmp exitExtractMemory1

            hasScaleHasBase:
                ; indirect has base, index, scale
                ; e.g. [eax+ecx*8]

                appendString rm, one
                appendString rm, zero
                appendString rm, zero

                getBetweenChars memoryMunch, '[', '+'
                mov rdi, between 
                call GetStrlen
                ; get the Base 
                copyString [baseReg], [between], rdx

                getBetweenChars memoryMunch, '+', '*'
                mov rdi, between 
                call GetStrlen
                ; get the index 
                copyString [indexReg], [between], rdx
                
                mov r14, memoryMunch
                mov r13b, '*'
                call isCharInString ; rdi is the address
                cmp BYTE [rdi+1], '+'
                je hasScaleHasBaseHasDisp
                jne hasScaleHasBaseNoDisp

                hasScaleHasBaseHasDisp:
                    ; indirect has base, index, disp -> 
                    ; e.g. [ebp+ecx*2+0x32345467] 

                    mov r14, memoryMunch
                    mov r13b, '+'
                    call isCharInString ; rdi is the address: ecx*2+0x32345467] 
                    mov r9, rdi

                    getBetweenChars r9, '*', '+'
                    mov rsi, between 
                    call setScale

                    mov r14, memoryMunch
                    mov r13b, '+'
                    call isCharInString ; rdi is the address: ecx*2+0x32345467] 
                    mov r9, rdi
                    getBetweenChars r9, '+', ']' ; get disp 
                    mov rsi, between      ; disp
                    call getImdOrDispSize ; size in rax

                    cmp rax, 8
                    je disp8bit_
                    jne disp32bit_

                    disp8bit_:
                        ; mod: 01
                        appendString mod, zero
                        appendString mod, one
                        lea rsi, [between] ; start address of disp
                        mov rax, 8
                        call dataORdisp_to_memory_extend ; binary disp in dataORdisp
                        
                        mov rdi, dataORdisp
                        call GetStrlen
                        copyString [disp], [dataORdisp], rdx

                        jmp exitExtractMemory1

                    disp32bit_:
                        ; mod: 10
                        appendString mod, one
                        appendString mod, zero
                        lea rsi, [between] ; start address of disp
                        mov rax, 32
                        call dataORdisp_to_memory_extend ; binary disp in dataORdisp
                        
                        mov rdi, dataORdisp
                        call GetStrlen
                        copyString [disp], [dataORdisp], rdx

                        jmp exitExtractMemory1

                hasScaleHasBaseNoDisp:
                    ; indirect has base, index (check for base=rbp/ebp) -> 
                    ; e.g. [ebp+ecx*1]
                    getBetweenChars memoryMunch, '*', ']'
                    mov rsi, between 
                    call setScale

                    mov rbx, baseReg

                    isEBP2_:
                        compRegNoModify ebp_, isRBP2_, 3, postProcessEBP_RBP
                    isRBP2_:
                        compRegNoModify rbp_, full_noException, 3, postProcessEBP_RBP
                    postProcessEBP_RBP:
                        ; mod: 01,
                        ; base:101 , baseReg:ebp
                        ; disp 0x0, 8 bits
                        appendString mod, zero
                        appendString mod, one
                        
                        lea rsi, [zero] ; zero disp
                        mov rax, 8
                        call dataORdisp_to_memory_extend ; binary disp in dataORdisp

                        mov rdi, dataORdisp
                        call GetStrlen
                        copyString [disp], [dataORdisp], rdx

                        jmp full_sharedAll
                    full_noException:
                        appendString mod, zero
                        appendString mod, zero
                    full_sharedAll:
                    ; rm: 100, scale: set before

                    ; set base binary 
                    mov rbx, baseReg
                    call getRegBinNoSize ; bin in regTempBinary
                    
                    copyString [base], [regTempBinary], 3

                    ; set index binary 
                    mov rbx, indexReg
                    call getRegBinNoSize ; bin in regTempBinary
                    
                    copyString [index], [regTempBinary], 3

                    jmp exitExtractMemory1                    
    exitExtractMemory1:
        ; convert any reg found to it's binary rep 
        cmp BYTE [index], 0
        je checkIndex
        jne checkBase

        checkIndex:
            cmp BYTE [indexReg], 0
            je checkBase

            mov rbx, indexReg
            call getRegBinNoSize ; bin in regTempBinary
            copyString [index], [regTempBinary], 3
        
        checkBase:
            cmp BYTE [base], 0
            jne exitExtractMemory

            mov rbx, baseReg
            call getRegBinNoSize ; bin in regTempBinary
            copyString [base], [regTempBinary], 3

    exitExtractMemory:
        ret



addSIB:
    cmp BYTE [index], 0
    je exitAddSIB

    appendString binary, scale
    appendString binary, index
    appendString binary, base

    exitAddSIB:
        ret
addDisp:
    cmp BYTE [disp], ' '
    je exitAddDisp

    appendString binary, disp
    exitAddDisp:
        ret 

getOperType:
    ;types-> 1: r|r:   register  | register
    ;        2: r|m:   register  | memory
    ;        3: m|r:   memory    | register
    ;        4: r|imd: register  | immediate
    ;        5: m|imd: memory    | immediate

    call hasMemory
    cmp QWORD [hasMemoryF], 1
    je memoryType
    jne noMemoryType
    memoryType:
        ; 2: r|m:   register  | memory
        ; 3: m|r:   memory    | register
        ; 5: m|imd: memory    | immediate

        ; would also set the type and d_s
        call extractMemory 
        jmp exitGetOperType
    noMemoryType:
        ; 1: r|r:   register  | register
        ; 4: r|imd: register  | immediate
        mov r14, command
        mov r13b, ','
        call isCharInString ; rdi is the address
        mov al, [rdi]
        call isNumeric
        jc secondArgImd_
        jnc secondArgReg_
        secondArgImd_:
            ; 4: r|imd: register  | immediate
            mov r15, rdi 
            call GetStrlen
            copyString [imd], [r15], rdx
            mov BYTE [d_s], '0'
            mov QWORD [type], 4
            jmp exitGetOperType
        secondArgReg_:
            ; 1: r|r:   register  | register
            mov r15, rdi 
            call GetStrlen
            copyString [register], [r15], rdx
            mov BYTE [d_s], '0'
            mov QWORD [type], 1
            jmp exitGetOperType

    exitGetOperType:
        ret 
;---------------------------------------------;

;---------------NO ARG HANDLE-----------------;
%macro noArgMacro 4
    ; e.g. noArgMacro stc_, isSTD, stc_bin, 8
    mov rsi, command
    mov rdi, %1
    mov rcx, 3
    repe cmpsb
    jne %2
    ; it was STC
    mov rsi, %3
    lea rdi, [binary]
    mov rcx, %4
    rep movsb
    mov al, 0 ; make it zero terminated
    stosb
    jmp exitSucessHandleNoArg
%endmacro
%macro noArgMacroRet 1
    ; e.g. noArgMacroRet 0
    mov rsi, command
    mov rdi, ret_
    mov rcx, 3
    repe cmpsb
    jne isNotNoArg
    ; it was RET
    ; check if it has a an imd after it
    ; if so, get out and let oneArg handle this.
    xor r14, r14
    add r14, 3
    mov al, [command+r14] 
    cmp al, 0x0a ; new line
    jne isNotNoArg

    mov rsi, ret_bin
    lea rdi, [binary]
    mov rcx, 8
    rep movsb
    mov al, 0 ; make it zero terminated
    stosb
    jmp exitSucessHandleNoArg
%endmacro
handleNoArg:
    xor rsi, rsi
    xor rdi, rdi
    xor rcx, rcx

    xor r8, r8 ;iterator on strings

    ; compare 3 first chars of command with noArgCommands
    isSTC:
        noArgMacro stc_, isSTD, stc_bin, 8
    isSTD:
        noArgMacro std_, isCLC, std_bin, 8
    isCLC:
        noArgMacro clc_, isCLD, clc_bin, 8
    isCLD:
        noArgMacro cld_, isSYS, cld_bin, 8
    isSYS:
        noArgMacro sys_, isRET, sys_bin, 16
    isRET:
        noArgMacroRet 0
    isNotNoArg:
        ; set isHandeledF flag to 0 -> the command was not handled here
        mov QWORD rax, 0
        mov [isHandeledF], rax
        jmp exitHandleNoArg
    exitSucessHandleNoArg:
        ; set isHandeledF flag to 1 -> the command was handled here
        mov QWORD rax, 1
        mov [isHandeledF], rax
        jmp exitHandleNoArg
    exitHandleNoArg:
        ret
;---------------------------------------------;

;---------------ONE ARG HANDLE---------------;

%macro oneArgStoreOpcode 2
    ; e.g. oneArgMacro push_bin, 8
    mov rsi, %1
    lea rdi, [binary]
    mov rcx, %2
    rep movsb
    mov al, 0 ; make it zero terminated
    stosb
%endmacro

%macro oneArgStoreImd 1
    mov rdi, dataORdisp
    call GetStrlen
    mov rcx, rdx ;len dataORdisp

    mov rdi, binary
    call GetStrlen
    lea rdi, [binary+rdx] ; binary+len binary

    mov rsi, dataORdisp

    rep movsb

    mov al, 0 ; make it zero terminated
    stosb 
%endmacro

; handels commands with one argument
addRexOneReg:
    ; either we have a newReg (e.g. r8b)
    ; or we have a 64 bit register!
    ; REX: 0100+w+r+x+b
    mov rdi, binary
    call GetStrlen ; len in rdx

    mov r15, rdx ; keep current len 

    ; add rex 4 "0100" to binary
    mov rsi, rex
    lea rdi, [binary+r15] ; end of binary 
    mov rcx, 4
    rep movsb
    add r15, 4

    ; add w
    cmp QWORD [registerSize], 64
    jne rex_w0
    ; w is one
        lea rdi, [binary+r15]
        mov al, '1'
        stosb
        jmp contAddRexOneReg
    rex_w0:
        lea rdi, [binary+r15]
        mov al, '0'
        stosb
    contAddRexOneReg:
    inc r15

    ; add r ; NO SIB -> 0
    lea rdi, [binary+r15]
    mov al, '0'
    stosb
    inc r15   

    ; add x ; NO SIB -> 0
    lea rdi, [binary+r15]
    mov al, '0'
    stosb
    inc r15

    ; add b ; extends reg
    cmp QWORD [isNewRegF], 1
    jne rex_b0
    lea rdi, [binary+r15]
    mov al, '1'
    stosb
    jmp contAddRexOneReg2
    rex_b0:
    lea rdi, [binary+r15]
    mov al, '0'
    stosb
    
    contAddRexOneReg2:
    inc r15
    lea rdi, [binary+r15]
    mov al, 0 ; make it zero terminated
    stosb

    ret
addOneArgRegOpcode:
    ; opcode could be:
    ; not/neg, idiv/imul, inc/dec
    ; shl/shr, call, pop/push   
    isNOT:
        oneArgMacroReg not_, not_reg, not_bin, isNEG, postProcessSharedOneArg
    isNEG:
        oneArgMacroReg neg_, neg_reg, neg_bin, isIMUL, postProcessSharedOneArg
    isIMUL:
        oneArgMacroReg imul_, imul_reg, imul_bin, isIDIV, postProcessSharedOneArg
    isIDIV:
        oneArgMacroReg idiv_, idiv_reg, idiv_bin, isINC, postProcessSharedOneArg
    isINC:
        oneArgMacroReg inc_, inc_reg, inc_bin, isDEC, postProcessSharedOneArg

    isDEC:
        oneArgMacroReg dec_, dec_reg, dec_bin, isSHL, postProcessSharedOneArg

    isSHL:
        oneArgMacroReg shl_, shl_reg, shl_bin, isSHR, postProcessSharedOneArg

    isSHR:
        oneArgMacroReg shr_, shr_reg, shr_bin, isCALL_, postProcessSharedOneArg

    isCALL_:
        oneArgMacroReg call_, call_reg, call_bin_indirect, isPUSH_, postProcessCALL_

    isPUSH_:
        oneArgMacroReg push_, push_reg, push_bin_reg, isPOP, postProcessPush_POP

    isPOP:
        oneArgMacroReg pop_, pop_reg, pop_bin_reg, isNotOneArg, postProcessPush_POP

    postProcessSharedOneArg:
        ; we are done leave
        jmp exitAddOneArgRegOpcode

    postProcessPush_POP:
        ; discard rex if reg is not new
        cmp QWORD [registerSize], 64
        jne postProcessPush_POP1 

        cmp QWORD [isNewRegF], 1
        je  postProcessPush_POP1
        jne removeRex1

        ; be careful if reg is 16, keep 66
        cmp QWORD [registerSize], 16
        jne removeRex1 
        
        ; preserve 66 and remove rex
        mov rdi, binary
        call GetStrlen
        sub rdx, 16

        lea rsi, [binary+16]
        lea rdi, [binary+8]
        mov rcx, rdx
        rep movsb
        mov al, 0 ; make it zero terminated
        stosb
        jmp postProcessPush_POP1

        removeRex1:
        mov rdi, binary
        call GetStrlen
        sub rdx, 8

        lea rsi, [binary+8]
        mov rdi, binary
        mov rcx, rdx
        rep movsb
        mov al, 0 ; make it zero terminated
        stosb

        postProcessPush_POP1:
        ; remove 1 from rex.w because no 64 bit pop really!
        cmp QWORD [registerSize], 64
        jne postProcessPush_POP2

        cmp QWORD [isNewRegF], 1
        jne  postProcessPush_POP2

        mov BYTE [binary+4], '0'

        postProcessPush_POP2:
        mov rbx, register
        call getRegBinNoSize

        appendString binary, regTempBinary
        mov QWORD [endOnSuccessF], 1
        jmp exitAddOneArgRegOpcode

    postProcessCALL_:
        ; discard rex if reg is not new
        cmp QWORD [registerSize], 64
        jne postProcessCALL_1 

        cmp QWORD [isNewRegF], 1
        je  postProcessCALL_1
        jne removeRex1_

        ; be careful if reg is 16, keep 66
        cmp QWORD [registerSize], 16
        jne removeRex1_ 
        
        ; preserve 66 and remove rex
        mov rdi, binary
        call GetStrlen
        sub rdx, 16

        lea rsi, [binary+16]
        lea rdi, [binary+8]
        mov rcx, rdx
        rep movsb
        mov al, 0 ; make it zero terminated
        stosb
        jmp postProcessCALL_1

        removeRex1_:
        mov rdi, binary
        call GetStrlen
        sub rdx, 7

        lea rsi, [binary+8]
        mov rdi, binary
        mov rcx, rdx
        rep movsb
        mov al, 0 ; make it zero terminated
        stosb

        postProcessCALL_1:
        ; remove 1 from rex.w because no 64 bit pop really!
        cmp QWORD [registerSize], 64
        jne exitAddOneArgRegOpcode

        cmp QWORD [isNewRegF], 1
        jne  exitAddOneArgRegOpcode

        mov BYTE [binary+4], '0'
    exitAddOneArgRegOpcode:
        ret
addOneArgMemOpcode:
    ; opcode could be:
    ; not/neg, idiv/imul, inc/dec
    ; shl/shr, call, pop/push   
    isNOT_:
        oneArgMacroReg not_, not_reg, not_bin, isNEG_, postProcessSharedOneArgMem
    isNEG_:
        oneArgMacroReg neg_, neg_reg, neg_bin, isIMUL_, postProcessSharedOneArgMem
    isIMUL_:
        oneArgMacroReg imul_, imul_reg, imul_bin, isIDIV_, postProcessSharedOneArgMem
    isIDIV_:
        oneArgMacroReg idiv_, idiv_reg, idiv_bin, isINC_, postProcessSharedOneArgMem
    isINC_:
        oneArgMacroReg inc_, inc_reg, inc_bin, isDEC_, postProcessSharedOneArgMem

    isDEC_:
        oneArgMacroReg dec_, dec_reg, dec_bin, isSHL_, postProcessSharedOneArgMem

    isSHL_:
        oneArgMacroReg shl_, shl_reg, shl_bin, isSHR_, postProcessSharedOneArgMem

    isSHR_:
        oneArgMacroReg shr_, shr_reg, shr_bin, isCALL__, postProcessSharedOneArgMem

    isCALL__:
        oneArgMacroReg call_, call_reg, call_bin_indirect, isPUSH__, postProcessSharedOneArgMem

    isPUSH__:
        oneArgMacroReg push_, push_reg, push_bin_mem, isPOP_, postProcessSharedOneArgMem

    isPOP_:
        oneArgMacroReg pop_, pop_reg, pop_bin_mem, isNotOneArg, postProcessSharedOneArgMem

    postProcessSharedOneArgMem:
    exitAddOneArgMemOpcode:
        ret
handleOneArg:
    call hasMemory
    mov rax, 0
    cmp [hasMemoryF], rax
    je oneArgNoMemory
    jne oneArgMemory
    oneArgNoMemory:
        oneArgImd:
            ; Formula: OPCODE + DATA
            ;   ret 24 or ret 0x24 (16 bits)
            ;   direct call e.g. call 23456 (any number) -> E800000000 (8 32bit 0)
            ;   push 24 or push 0x24 (7 bits(0x7f max) or extend to 32 bits)
                
            ; compare 3 first chars of command with noArgCommands
            isRET_1:
                oneArgMacro ret_, isCALL, postProcessRET
            isCALL: 
                oneArgMacro call_, isPUSH, postProcessCALL
            isPUSH: 
                oneArgMacro push_, oneArgRegister, postProcessPUSH

            postProcessRET:
                ; Store Opcode
                oneArgStoreOpcode ret_bin1, 8

                lea rsi, [command + 4] ; start of disp 
                mov rax, 16
                call dataORdisp_to_memory_extend ; binary disp in dataORdisp
                
                ; Store Imd
                oneArgStoreImd 0

                jmp exitSucessHandleOneArg

            postProcessCALL:
                ; if arg is not a reg
                mov al, [command + 5] ; load first char of arg
                call isNumeric
                jnc oneArgRegister ; arg is a reg

                ; Store Opcode
                oneArgStoreOpcode call_bin_direct, 8 

                mov rsi, zero ; start of disp 
                mov rax, 32
                call dataORdisp_to_memory_extend ; binary disp in dataORdisp
                
                ; Store Imd
                oneArgStoreImd 0

                jmp exitSucessHandleOneArg

            postProcessPUSH: 
                ; if arg is not a reg
                mov al, [command + 5] ; load first char of arg
                call isNumeric
                jnc oneArgRegister ; arg is a reg

                ; Store Opcode
                oneArgStoreOpcode push_bin_imd, 8  

                ;7 bits(0x7f max) or extend to 32 bits
                lea rsi, [command + 5] ; start of disp 
                call isNumDecimal
                jnc cont1PostProcessPUSH
                ; convert to hex
                lea rsi, [command + 5] ; start of disp 
                call dec2hex ;hex in hexTemp
                mov rsi, hexTemp
                jmp contPostProcessPUSH
                cont1PostProcessPUSH:
                ; is hex, discard 0x
                lea rsi, [command + 5]
                add rsi, 2
                contPostProcessPUSH:
                call getImdOrDispSize ; size in rax
                cmp rax, 32
                jne cont2PostProcessPUSH
                ; opcode is different in second from last byte 
                ; for imd8 and imd32
                lea rsi, [binary + 6]
                mov bl, '0'
                mov [rsi], bl

                cont2PostProcessPUSH:
                lea rsi, [command + 5] ; start of disp 
                call dataORdisp_to_memory_extend ; binary disp in dataORdisp
                
                ; Store Imd
                oneArgStoreImd 0
                
                jmp exitSucessHandleOneArg
        ; ----------------------------------------------------------------
        oneArgRegister:
            ; Formula: oper_prefix(66 - 01100110): only if regSize=16
            ;          + REX + OPCODE + MOD + REG(opcode) + R/M (reg) 
            ;   not/neg rax
            ;   idiv rax
            ;   inc/dec rax
            ;   shl/shr rax

            ;   call rax
            ;   pop/push rax   


            ; step1: get reg name
            mov rdi, command
            call getReg ; reg name in register
            ; get reg size
            mov rsi, register
            call getRegSize ; size in [regSizeTemp]
            mov rax, [regSizeTemp]
            mov [registerSize], rax
            ; step2: add prefix -if needed-
            call addOperPrefix
            ; step3: add rex    -if needed-
            cmp QWORD [registerSize], 64
            je needRex
            cmp QWORD [isNewRegF], 1
            je needRex
            jne contOneArgReg
            needRex:
                call addRexOneReg
            contOneArgReg:
            ; step4: add opcode
            ; find the name of command and add the opcode or 
            ; other necessary stuff at this point.
            call addOneArgRegOpcode

            cmp QWORD [endOnSuccessF], 1
            je exitSucessHandleOneArg

            ; step5: add w
            call addCodeW

            ; step6: add mod=11
            appendString binary, one
            appendString binary, one

            ; step7: add reg(opcode)
            appendString binary, regOpcode

            ; step8: add r/m(reg code)
            mov rbx, register
            call getRegBinNoSize
            appendString binary, regTempBinary

            jmp exitSucessHandleOneArg
    ; -------------------------------------------------------------------------
    oneArgMemory:
        ; Formula: addr_prefix(67 - 01100111) + oper_prefix(66 - 01100110) + 
        ;          REX + OPCODE + w + MOD + REG(opcode) + RM + 
        ;          SIB(SCALE+INDEX+BASE) + DISP

        ; not/neg, idiv, inc/dec, shl/shr, call, pop/push 
        
        ; Memory Formula: BYTE PTR [rbx+rcx*8+0x32] ; indirect
        ;                 BYTE PTR [0x32]           ; direct

        ; Step1: Extract the memory munch(e.g. BYTE PTR [rbx+rcx*8+0x32]) 
        ;        to memoryMunch.
        ;        updates mod, r/m, SIB, Disp, baseReg, indexReg
        call extractMemory 

        ; Step2: add address prefix 67 if we have indirect addressing and 
        ;                                 base in memory is 32 bits
        call addAddrPrefix

        ; Step3: add operand prefix 66 if registerSize is 16 (filled in step1)
        call addOperPrefix

        ; Step4: add rex only if we have new reg in memory (could be base, or index!)
        call addRexForMemory

        ; Step5: add opcode
        call addOneArgMemOpcode

        ; step6: add w
        call addCodeW

        ; step7: add MOD
        appendString binary, mod

        ; step8: add REG(opcode)
        appendString binary, regOpcode

        ; step9: add rm
        appendString binary, rm

        ; step10: add SIB(SCALE+INDEX+BASE)
        call addSIB

        ; step11: add DISP
        call addDisp

        exitSucessHandleOneArg:
            ; set isHandeledF flag to 1 -> the command was handled here
            mov QWORD rax, 1
            mov [isHandeledF], rax
            jmp exitHandleOneArg
        isNotOneArg:
            ; set isHandeledF flag to 0 -> the command was not handled here
            mov QWORD rax, 0
            mov [isHandeledF], rax
            jmp exitHandleOneArg

    exitHandleOneArg:
        ret
;---------------------------------------------;
;---------------TWO ARG HANDLE----------------;
addTwoArgsOpcode:
    ; opcode could be:
    ; mov, add/adc/sub/sbb, and/or/xor, cmp/test, xchg/xadd, imul, 
    ; shr/shl, bsf/bsf 
    cmp QWORD [type], 4
    je hasIMD 
    cmp QWORD [type], 5
    je hasIMD 

    ; no imd opcode
    isMOV:
        oneArgMacroReg mov_, mov_reg, mov_r2r_m2r, isADD, exitAddTwoArgsOpcode
    isADD:
        oneArgMacroReg add_, add_reg, add_r2r_m2r, isADC, exitAddTwoArgsOpcode
    isADC:
        oneArgMacroReg adc_, adc_reg, adc_r2r_m2r, isSUB, exitAddTwoArgsOpcode
    isSUB:
        oneArgMacroReg sub_, sub_reg, sub_r2r_m2r, isSBB, exitAddTwoArgsOpcode
    isSBB:
        oneArgMacroReg sbb_, sbb_reg, sbb_r2r_m2r, isAND, exitAddTwoArgsOpcode
    isAND:
        oneArgMacroReg and_, and_reg, and_r2r_m2r, isOR, exitAddTwoArgsOpcode
    isOR:
        oneArgMacroReg or_, or_reg, or_r2r_m2r, isXOR, exitAddTwoArgsOpcode
    isXOR:
        oneArgMacroReg xor_, xor_reg, xor_r2r_m2r, isCMP, exitAddTwoArgsOpcode
    isCMP:
        oneArgMacroReg cmp_, cmp_reg, cmp_r2r_m2r, isTEST, exitAddTwoArgsOpcode
    isTEST:
        oneArgMacroReg test_, test_reg, test_r2r_m2r, isXCHG, exitAddTwoArgsOpcode
    isXCHG:
        oneArgMacroReg xchg_, dummy_reg, xchg_r2r_m2r, isXADD, exitAddTwoArgsOpcode
   
    isXADD:
        oneArgMacroReg xadd_, dummy_reg, xadd_r2r_m2r, isIMUL__, exitAddTwoArgsOpcode
    isIMUL__:
        oneArgMacroReg imul_, dummy_reg, imul_r2r_m2r, isSHR__, exitAddTwoArgsOpcode
    isSHR__:
        oneArgMacroReg shr_, shr_reg, shr_bin2, isSHL__, exitAddTwoArgsOpcode
    isSHL__:
        oneArgMacroReg shl_, shl_reg, shl_bin2, isBSF, exitAddTwoArgsOpcode
    isBSF:
        oneArgMacroReg bsf_, dummy_reg, bsf_r2r_m2r, isBSR, exitAddTwoArgsOpcode
    isBSR:
        oneArgMacroReg bsr_, dummy_reg, bsr_r2r_m2r, exitAddTwoArgsOpcode, exitAddTwoArgsOpcode

    hasIMD:
    ; has imd opcode
        isMOV_imd:
            oneArgMacroReg mov_, mov_reg, mov_imd2r_imd2m, isADD_imd, exitAddTwoArgsOpcode
        isADD_imd:
            oneArgMacroReg add_, add_reg, add_imd2r_imd2m, isADC_imd, exitAddTwoArgsOpcode
        isADC_imd:
            oneArgMacroReg adc_, adc_reg, adc_imd2r_imd2m, isSUB_imd, exitAddTwoArgsOpcode
        isSUB_imd:
            oneArgMacroReg sub_, sub_reg, sub_imd2r_imd2m, isSBB_imd, exitAddTwoArgsOpcode
        isSBB_imd:
            oneArgMacroReg sbb_, sbb_reg, sbb_imd2r_imd2m, isAND_imd, exitAddTwoArgsOpcode
        isAND_imd:
            oneArgMacroReg and_, and_reg, and_imd2r_imd2m, isOR_imd, exitAddTwoArgsOpcode
        isOR_imd:
            oneArgMacroReg or_, or_reg, or_imd2r_imd2m, isXOR_imd, exitAddTwoArgsOpcode
        isXOR_imd:
            oneArgMacroReg xor_, xor_reg, xor_imd2r_imd2m, isCMP_imd, exitAddTwoArgsOpcode
        isCMP_imd:
            oneArgMacroReg cmp_, cmp_reg, cmp_imd2r_imd2m, isTEST_imd, exitAddTwoArgsOpcode
        isTEST_imd:
            oneArgMacroReg test_, test_reg, test_imd2r_imd2m, isSHR___imd, exitAddTwoArgsOpcode
        isSHR___imd:
            oneArgMacroReg shr_, shr_reg, shr_bin2, isSHL___imd, exitAddTwoArgsOpcode
        isSHL___imd:
            oneArgMacroReg shl_, shl_reg, shl_bin2, exitAddTwoArgsOpcode, exitAddTwoArgsOpcode
    exitAddTwoArgsOpcode:
        ret

setSdata:
    ; imd is in imd and rbx is set to 1 if has memory!
    ; sets d_s, and handels imds

    mov rsi, imd 
    call getImdOrDispSize

    ; Returns size in rax  
    ; sets the d_s field
    cmp rax, 32
    je addData16_32
    jne addData8
    addData16_32:
        mov BYTE [d_s], '0'

        lea rsi, [imd] ; start address of imd
        mov rax, 32
        call dataORdisp_to_memory_extend ; binary disp in dataORdisp
        copyString [imd], [dataORdisp], 32
        jmp exitSetSdata

    addData8:
        mov BYTE [d_s], '1'

        cmp rbx, 0
        je addData16_32

        mov rdi, imd
        call GetStrlen
        mov r13, rdx ; save

        mov rsi, imd 
        call isNumDecimal
        jc contAddData8

        sub r13, 2

        contAddData8:
            imul r13, 4

        lea rsi, [imd] ; start address of imd
        mov rax, r13

        push r13
        call dataORdisp_to_memory_extend ; binary disp in dataORdisp
        pop r13

        mov rdi, dataORdisp
        call GetStrlen
        
        copyString [imd], [dataORdisp], rdx
        jmp exitSetSdata
    exitSetSdata:
        ret
addImd:
    cmp BYTE [imd], ' '
    je exitAddImd

    appendString binary, imd
    exitAddImd:
        ret
addD_S:
    cmp QWORD [isBSR_BSF_IMUL_F], 1
    je exitAddD_S

    appendString binary, d_s

    exitAddD_S:
        ret
setRegAsOpcode:
    copyString [reg], [regOpcode], 3
    ret
getReg_:
    ; start addr in rdi
    ; returns the first reg in register 

    getBetweenChars command, ' ', ','
    mov rdi, between
    call GetStrlen
    copyString [register], [between], rdx

    ret
handleTwoArgs:
    ; Formula: addr_prefix(67 - 01100111) + oper_prefix(66 - 01100110) + 
    ;          REX + OPCODE + d_s + w + MOD + REG + RM + 
    ;          SIB(SCALE+INDEX+BASE) + DISP + IMD

    ; also sets d_s
    
    call getOperType ; also extracts memory if any

    ; these have no w or d_s
    compStrings command, bsr_, 3, handleBSR_BSF_IMUL
    compStrings command, bsf_, 3, handleBSR_BSF_IMUL
    compStrings command, imul_,3, handleBSR_BSF_IMUL
    
    jmp contHandleTwoArgs_

    handleBSR_BSF_IMUL:
        mov QWORD [isBSR_BSF_IMUL_F], 1

    
    contHandleTwoArgs_:

    cmp QWORD [type], 1
    je regReg
    cmp QWORD [type], 2
    je regMem
    cmp QWORD [type], 3
    je memReg
    cmp QWORD [type], 4
    je regImd
    cmp QWORD [type], 5
    je memImd
    regReg:
        ; reg <- arg2, 
        ; mod=11, 
        ; rm <- arg1
        appendString mod, one
        appendString mod, one

        cmp QWORD [isBSR_BSF_IMUL_F],1
        jne normalRegReg

        ; in bsr, bsf and imul:
        ;reg is the reg1
        mov rdi, command
        call getReg_ ;reg name in 'register' data
        
        mov rdi, register 
        call GetStrlen
        copyString [reg1], [register], rdx

        mov rbx, register
        call getRegBinNoSize ; returns binary in regTempBinary
        copyString [reg], [regTempBinary], 3

        mov rax, [regSizeTemp]
        mov [registerSize], rax

        ;rm is the reg2
        mov r14, command
        mov r13b, ','
        call isCharInString ; rdi is the address
        mov r15, rdi ; save

        call GetStrlen
        copyString [reg2], [r15], rdx

        mov rbx, r15
        call getRegBinNoSize ; returns binary in regTempBinary
        copyString [rm], [regTempBinary], 3

        jmp contRegReg
        normalRegReg:
            ;rm is the reg1
            mov rdi, command
            call getReg_ ;reg name in 'register' data

            mov rdi, register 
            call GetStrlen
            copyString [reg2], [register], rdx

            mov rbx, register
            call getRegBinNoSize ; returns binary in regTempBinary
            copyString [rm], [regTempBinary], 3

            mov rax, [regSizeTemp]
            mov [registerSize], rax

            ;reg is the reg2
            mov r14, command
            mov r13b, ','
            call isCharInString ; rdi is the address
            mov r15, rdi ; save

            call GetStrlen
            copyString [reg1], [r15], rdx

            mov rbx, r15

            call getRegBinNoSize ; returns binary in regTempBinary
            copyString [reg], [regTempBinary], 3

            ; d_s for xadd and xchg is non
            compStrings command, xadd_, 3, handleXADD_XCHG
            compStrings command, xchg_, 3, handleXADD_XCHG
            jmp contRegReg
            handleXADD_XCHG:
                mov BYTE [d_s], 0 
        contRegReg:
            jmp contHandleTwoArgs

    regMem:
        ; reg <- arg1, w mod rm sib disp

        ;reg is the reg1
        mov rdi, command
        call getReg_ ;reg name in 'register' data
        mov rbx, register
        call getRegBinNoSize ; returns binary in regTempBinary
        copyString [reg], [regTempBinary], 3

        ; d_s for xadd non
        compStrings command, xadd_, 3, handleXADD
        jmp contHandleTwoArgs
        handleXADD:
            mov BYTE [d_s], 0 

        jmp contHandleTwoArgs

    memReg:
        ; reg <- arg2, w mod rm sib disp

        ;reg is the reg2
        mov r14, command
        mov r13b, ','
        call isCharInString ; rdi is the address

        mov rbx, rdi
        call getRegBinNoSize ; returns binary in regTempBinary
        copyString [reg], [regTempBinary], 3

        ; d_s for xadd non
        compStrings command, xadd_, 3, handleXADD_
        jmp contHandleTwoArgs
        handleXADD_:
            mov BYTE [d_s], 0 

        jmp contHandleTwoArgs

    regImd:
        ; get the imd out 
        mov r14, command
        mov r13b, ','
        call isCharInString ; rdi is the address
        mov r15, rdi ; save
        call GetStrlen
        mov r14, rdx ; save
        mov rdi, command
        call GetStrlen
        sub rdx, r14
        copyString [imd], [r15], rdx

        compStrings command, shr_, 3, handleSHL_SHR_
        compStrings command, shl_, 3, handleSHL_SHR_
        compStrings command, mov_, 3, handleMOV_
        jmp normalRegImd
        
        handleSHL_SHR_:
            mov rdi, imd 
            call GetStrlen
 
            cmp rdx, 1
            je len1_

            compStrings imd, hexOne, 3, handleShift1_
            jmp handleShift_

            len1_:
                mov al, [imd]
                cmp BYTE [imd], '1'
                je handleShift1_
                jne handleShift_
        handleShift1_:
            ; remove 1 and do like normal shift!
            mov r14, command
            mov r13b, ','
            call isCharInString ; rdi is the address
            dec rdi
            mov BYTE [rdi], 0
            call handleOneArg
            jmp exitSucessHandleTwoArgs

        handleShift_:
            mov rbx, 1 ;rbx is set to 1 if no memory!
            call setSdata
            mov BYTE [d_s], '0'
            jmp contRegImd

        handleMOV_:
            mov rbx, 0 ;rbx is set to 1 if no memory!
            call setSdata

            mov BYTE [d_s], '1'
            jmp contRegImd
        
        normalRegImd:
            mov rbx, 1 ;rbx is set to 1 if no memory!
            call setSdata
            
        contRegImd:
            ; mod:11
            appendString mod, one
            appendString mod, one

            ;rm is the reg
            mov rdi, command
            call getReg_ ;reg name in 'register' data
            mov rbx, register
            call getRegBinNoSize ; returns binary in regTempBinary
            copyString [rm], [regTempBinary], 3

            mov rax, [regSizeTemp]
            mov [registerSize], rax

            ; reg is opcode
            jmp contHandleTwoArgs

    memImd:
        ; get the imd out 
        mov r14, command
        mov r13b, ','
        call isCharInString ; rdi is the address
        mov r15, rdi ; save
        call GetStrlen
        mov r14, rdx ; save
        mov rdi, command
        call GetStrlen
        sub rdx, r14
        copyString [imd], [r15], rdx

        compStrings command, shr_, 3, handleSHL_SHR
        compStrings command, shl_, 3, handleSHL_SHR
        compStrings command, mov_, 3, handleMOV
        jmp contMemImd
        handleSHL_SHR:

            mov rdi, imd 
            call GetStrlen
 
            cmp rdx, 1
            je len1

            compStrings imd, hexOne, 3, handleShift1
            jmp handleShift

            len1:
                mov al, [imd]
                cmp BYTE [imd], '1'
                je handleShift1
                jne handleShift
        handleShift1:
            ; remove 1 and do like normal shift!
            mov r14, memoryMunch
            mov r13b, ','
            call isCharInString ; rdi is the address
            dec rdi
            mov BYTE [rdi], 0
            call handleOneArg
            jmp exitSucessHandleTwoArgs

        handleShift:
            mov rbx, 1 ;rbx is set to 1 if has memory!
            call setSdata
            mov BYTE [d_s], '0'
            jmp contHandleTwoArgs

        handleMOV:
            ;mov BYTE [d_s], '1'
            mov rbx, 0 ;rbx is set to 1 if has memory!
            call setSdata

            mov BYTE [d_s], '1'

            jmp contHandleTwoArgs

        contMemImd:
            mov rbx, 1 ;rbx is set to 1 if has memory!
            call setSdata
            jmp contHandleTwoArgs

            ; reg is opcode

            jmp contHandleTwoArgs


    contHandleTwoArgs:
        ; Step2: add address prefix 67 if we have indirect addressing and 
        ;                                 base in memory is 32 bits
        call addAddrPrefix

        ; Step3: add operand prefix 66 if registerSize is 16 (filled in step1)
        call addOperPrefix

        ; Step4: add rex
        cmp QWORD [hasMemoryF], 1
        je addRexForMemory_
        jne addRexNoMemory_
        addRexForMemory_:
            call addRexForMemory 
            jmp contHandleTwoArgs__
        
        addRexNoMemory_:
            call addRexNoMemory 
            jmp contHandleTwoArgs__
        
        contHandleTwoArgs__:

        ; Step5: add opcode
        call addTwoArgsOpcode

        ; step6: add d_s
        call addD_S

        ; step6: add w
        call addCodeW

        ; step7: add MOD
        appendString binary, mod

        ; regImd and memImd -> reg is Opcode
        cmp QWORD [type], 4
        je setRegAsOpcode_
        cmp QWORD [type], 5
        je setRegAsOpcode_
        jmp cont_

        setRegAsOpcode_:
            call setRegAsOpcode

        cont_:
        ; step8: add REG
        appendString binary, reg

        ; step9: add rm
        appendString binary, rm

        ; step10: add SIB(SCALE+INDEX+BASE)
        call addSIB

        ; step11: add DISP
        call addDisp

        ; step11: add IMD
        call addImd


    exitSucessHandleTwoArgs:
        ; set isHandeledF flag to 1 -> the command was handled here
        mov QWORD rax, 1
        mov [isHandeledF], rax
        jmp exitHandleTwoArgs
    isNotTwoArg:
        ; set isHandeledF flag to 0 -> the command was not handled here
        mov QWORD rax, 0
        mov [isHandeledF], rax
        jmp exitHandleTwoArgs

    exitHandleTwoArgs:
        ret
;---------------------------------------------;
;---------------Main Function----------------;
DoAssembler:
    ; We call each of handleNoArg, handleOneArg, handleTwoArgs
    ; on the command, if it is handeled, we set the isHandeledF flag.

    call setDefaults

    mov r14, command
    mov r13b, ' '
    call isCharInString
    cmp r15, 1
    je notNoArg

    ; returns the binary command in binary
    call handleNoArg
    ; if isHandeledF=1 then the command was a no argumant command
    ; and is handeled.
    mov rax, [isHandeledF]
    cmp QWORD rax, 1
    je done

    notNoArg:
    mov r14, command
    mov r13b, ','
    call isCharInString
    cmp r15, 1
    je isTwoArg
    jne isOneArg

    isOneArg:
        ; returns the binary command in binary
        call handleOneArg
        mov rax, [isHandeledF]
        cmp QWORD rax, 1
        je done
    isTwoArg:
        ; returns the binary command in binary
        call handleTwoArgs
        mov rax, [isHandeledF]
        cmp QWORD rax, 1
        je done

    ; could not handle 
    mov rsi, cantHandle
    call printString

    done:
        ret
;---------------------------------------------;


%if 0

_start:

    ;call the assembler 
    ; returns the bin code in binary

    call DoAssembler

    mov rsi, binary
    call printString
    call newLine

    ; returns the hex code in hexTemp
    mov rbx, binary
    call bin2hex

    mov rdi, hexTemp
    call GetStrlen
    copyString [hex], [hexTemp], rdx

    mov rsi, hex
    call printString

    ;call testCommand

exit:
    mov rax, sys_exit
    xor rdi, rdi
    syscall
%endif ; 0 
;nasm -felf64 Assembler.asm && ld Assembler.o && ./a.out
