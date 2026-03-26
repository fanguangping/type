(rules
 (processor
  (post
   (upgrade (lambda (x) x) :<=)))
 (macro
  (li           (rd imm)
                (: high (arithmetic-shift (+ imm #x800) -12))
                (: low  (- imm (arithmetic-shift high 12)))
                (:<= ((lui rd imm) (addi rd rd low))))
  (let          (x imm)       (li x imm))
  (:-quit    ()    (:<= ((li t0 #x100000)
                         (li t1 #x5555)
                         (sw t1 t0 0)
                         (label spin)
                         (jump spin))))
  (:-putchar ()    (:<= ((li t0 #x10000000)
                         (sb a0 t0 0))))))
