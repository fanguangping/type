;; ============================================================================
;; ELF 文件格式规则 (ELF file layout rules)
;; ============================================================================
;; 本文件是纯数据(S-expression),由 wrapper/elf-file-wrapper.rkt 用基础 read
;; 读入并解释。它描述 ELF32/ELF64 的字段布局、常量取值、枚举映射以及
;; 可执行文件/可重定位目标文件所使用的节模板。
;;
;; 字段格式:
;;   (name width annotation ...)
;;   width:      整数(字节数) | (word) -> 4/8 字节(由 class 决定) | (half)
;;   annotation: (= value)              常量值
;;               ((raw decoded) ...)    枚举(raw 写入文件,decoded 为可读名)
;;               (kind ...)             用途说明(addr/offset/size/perm/...)
;;
;; 结构中的 (class-fields ((32 (...)) (64 (...)))) 用于描述 32/64 位字段
;; 顺序不同的结构(程序头、节头、符号表项),由包装器按 class 选择。

((fileinfo
  (format  0 (= "ELF"))
  (class   1 ((32 1) (64 2)))                ; 字长 -> ELFCLASS32/64
  (data    1 ((little 1) (big 2)))           ; 字节序 -> ELFDATA2LSB/MSB
  (version 1 (= 1)))                         ; EV_CURRENT

 ;; ---------------------------------------------------------------------------
 ;; ELF 文件头 (Elf32_Ehdr / Elf64_Ehdr)
 ;; ---------------------------------------------------------------------------
 (header
  (magic      4 (= (127 69 76 70)))          ; 0x7f 'E' 'L' 'F'
  (class      1 (from fileinfo class))
  (data       1 (from fileinfo data))
  (version    1 (= 1))
  (osabi      1 (= 0))                       ; ELFOSABI_SYSV
  (abiversion 1 (= 0))
  (pad        7 (= 0))
  (type       2 ((0 none) (1 rel) (2 exec) (3 dyn) (4 core)))
  (machine    2 ((0 none) (3 x86) (8 mips) (40 arm)
                 (62 x86-64) (243 riscv)))
  (version2   4 (= 1))
  (entry      (word) (addr))
  (phoff      (word) (offset))
  (shoff      (word) (offset))
  (flags      4 (= 0))
  (ehsize     2 (sizeof header))
  (phentsize  2 (sizeof program-header))
  (phnum      2 (count program-headers))
  (shentsize  2 (sizeof section-header))
  (shnum      2 (count sections))
  (shstrndx   2 (index-of shstrtab sections)))

 ;; ---------------------------------------------------------------------------
 ;; 程序头 (Elf32_Phdr / Elf64_Phdr) —— 32/64 位字段顺序不同
 ;; ---------------------------------------------------------------------------
 (program-header
  (type 4 ((0 null) (1 load) (2 dynamic) (3 interp) (4 note) (6 phdr)))
  (class-fields
   ((32 ((offset (word) (offset))
         (vaddr  (word) (addr))
         (paddr  (word) (addr))
         (filesz (word) (size))
         (memsz  (word) (size))
         (flags  4 (perm))
         (align  (word) (align))))
    (64 ((flags  4 (perm))
         (offset (word) (offset))
         (vaddr  (word) (addr))
         (paddr  (word) (addr))
         (filesz (word) (size))
         (memsz  (word) (size))
         (align  (word) (align)))))))

 ;; ---------------------------------------------------------------------------
 ;; 节头 (Elf32_Shdr / Elf64_Shdr)
 ;; ---------------------------------------------------------------------------
 (section-header
  (name 4 (str-offset shstrtab))
  (type 4 ((0 null) (1 progbits) (2 symtab) (3 strtab)
           (4 rela) (8 nobits) (9 rel)))
  (class-fields
   ((32 ((flags    4 (perm))
         (addr     (word) (addr))
         (offset   (word) (offset))
         (size     (word) (size))
         (link     4 (section-index))
         (info     4)
         (addralign 4 (align))
         (entsize  4 (size))))
    (64 ((flags    (word) (perm))
         (addr     (word) (addr))
         (offset   (word) (offset))
         (size     (word) (size))
         (link     4 (section-index))
         (info     4)
         (addralign (word) (align))
         (entsize  (word) (size)))))))

 ;; ---------------------------------------------------------------------------
 ;; 符号表项 (Elf32_Sym / Elf64_Sym)
 ;; ---------------------------------------------------------------------------
 (symbol
  (class-fields
   ((32 ((name  4 (str-offset strtab))
         (value (word) (addr))
         (size  (word) (size))
         (info  1 (sym-info))
         (other 1 (= 0))
         (shndx 2 (section-index))))
    (64 ((name  4 (str-offset strtab))
         (info  1 (sym-info))
         (other 1 (= 0))
         (shndx 2 (section-index))
         (value (word) (addr))
         (size  (word) (size)))))))

 ;; ---------------------------------------------------------------------------
 ;; 重定位项 (Elf32_Rela / Elf64_Rela)
 ;; ---------------------------------------------------------------------------
 (relocation
  (class-fields
   ((32 ((offset (word) (offset))
         (info   (word) (rel-info))
         (addend (word) (addend))))
    (64 ((offset (word) (offset))
         (info   (word) (rel-info))
         (addend (word) (addend)))))))

 ;; ---------------------------------------------------------------------------
 ;; 字符串表
 ;; ---------------------------------------------------------------------------
 (string-table
  (bytes 0 (string)))

 ;; ---------------------------------------------------------------------------
 ;; 段布局模板 (可执行文件)
 ;; ---------------------------------------------------------------------------
 (segments
  (text (type load) (flags r x))
  (data (type load) (flags r w)))

 ;; ---------------------------------------------------------------------------
 ;; 节模板:节名 -> 类型/标志/内容来源
 ;; ---------------------------------------------------------------------------
 (sections
  (null     (type null))
  (text     (type progbits) (flags alloc execinstr) (content code))
  (data     (type progbits) (flags alloc write)     (content data))
  (bss      (type nobits)   (flags alloc write)     (content zero))
  (shstrtab (type strtab)   (content section-names))
  (strtab   (type strtab)   (content symbol-names))
  (symtab   (type symtab)   (link strtab) (info 1)  (content symbols))
  (rela.text (type rela)    (link symtab) (info text) (content relocations)))

 ;; ---------------------------------------------------------------------------
 ;; 内存布局(派生信息,不直接写入文件)
 ;; ---------------------------------------------------------------------------
 (memory-layout
  (entry    (header entry))
  (phdrs    (program-header ...))
  (shdrs    (section-header ...))
  (segments (sections with (flags alloc)))))
