(build
 (source "example.scm")
 (dump-dir "build")

 ;; 工序清单:每项是「工序名 + 规则/选项」。
 ;; 前段沿用 sum100_rv32 的多阶段翻译;link 工序产出符号与重定位;
 ;; 末段编码为字节并包装成 ELF 目标文件与可执行文件。
 (stages
  (translate (rules "rules-0.scm"))
  (translate (rules "rules-1.scm"))
  (translate (rules "rules-2.scm"))
  (link      (rules "rules-link.scm") (emit symbols relocs))
  (translate (rules "rules-bin.scm"))
  (encode    (from source) (to code))
  (wrap-elf  (kind object)     (output "sum100.o")   (class 32))
  (wrap-elf  (kind executable) (output "sum100.elf") (class 32)
             (entry _start) (load-addr #x80000000))))
