;;; Core Arithmetic

(defun llvm-riscv64:ADDI (dst src off)
  (set$ dst (+ src off)))

(defun llvm-riscv64:C_ADDI (dst src off)
  (set$ dst (+ src off)))

(defun llvm-riscv64:ADDIW (dst src off)
  (set$ dst (cast-signed (word-width)
                         (cast-low (/ (word-width) 2)
                                   (+ src off)))))

(defun llvm-riscv64:C_ADDIW (dst src off)
  (set$ dst (cast-signed (word-width)
                         (cast-low (/ (word-width) 2)
                                   (+ src off)))))

(defun llvm-riscv64:ADDW (rd r1 r2)
  (set$ rd (cast-signed (word-width)
                         (cast-low (/ (word-width) 2)
                                   (+ r1 r2)))))

(defun llvm-riscv64:C_ADDW (rd r1 r2)
  (set$ rd (cast-signed (word-width)
                         (cast-low (/ (word-width) 2)
                                   (+ r1 r2)))))

(defun llvm-riscv64:C_ADD (rd r1 r2)
  (set$ rd (+ r1 r2)))

(defun llvm-riscv64:C_ADDI4SPN (dst src off)
  (set$ dst (+ src off)))

(defun llvm-riscv64:C_ADDI16SP (dst src off)
  (set$ dst (+ src off)))

(defun llvm-riscv64:SUB (rd r1 r2)
  (set$ rd (- r1 r2)))

(defun llvm-riscv64:C_SUB (rd r1 r2)
  (set$ rd (- r1 r2)))


;;; Moves

(defun llvm-riscv64:C_MV (dst src)
  (set$ dst src))

(defun llvm-riscv64:C_LUI (dst imm)
  (set$ dst (lshift (cast-signed
                     (- (word-width) 12)
                     (cast-low 6 imm))
                    12)))

(defun llvm-riscv64:AUIPC (dst off)
  (set$ dst (+ (get-program-counter) off)))

(defun llvm-riscv64:LUI (dst imm)
  (set$ dst (lshift imm 12)))

(defun llvm-riscv64:LI (dst imm)
  (set$ dst (cast-signed (word-width) imm)))

(defun llvm-riscv64:C_LI (dst imm)
  (set$ dst (cast-signed (word-width) imm)))



;;; Memory operations
(defun llvm-riscv64:LD (dst reg off)
  (set$ dst (load-word (+ reg off))))

(defun llvm-riscv64:C_LD (dst reg off)
  (set$ dst (load-word (+ reg off))))

(defmacro llvm-riscv64:load-word (cast part dst reg off)
  (set$ dst (cast (word-width)
                  (load-bits (/ (word-width) part) (+ reg off)))))

(defun llvm-riscv64:LW (dst reg off)
  (llvm-riscv64:load-word cast-signed 2 dst reg off))

(defun llvm-riscv64:LH (dst reg off)
  (llvm-riscv64:load-word cast-signed 4 dst reg off))

(defun llvm-riscv64:LWU (dst reg off)
  (llvm-riscv64:load-word cast-unsigned 2 dst reg off))

(defun llvm-riscv64:LB (dst reg off)
  (set$ dst (cast-signed (word-width) (load-byte (+ reg off)))))

(defun llvm-riscv64:LBU (dst reg off)
  (set$ dst (cast-unsigned (word-width) (load-byte (+ reg off)))))

(defun llvm-riscv64:LHU (dst reg off)
  (llvm-riscv64:load-word cast-unsigned 4 dst reg off))

(defun llvm-riscv64:C_LDSP (dst reg off)
  (set$ dst (load-word (+ reg off))))


(defun llvm-riscv64:C_SDSP (val sp imm)
  (store-word (+ sp imm) val))

(defun llvm-riscv64:SDSP (val sp imm)
  (store-word (+ sp imm) val))

(defun llvm-riscv64:SD (val reg imm)
  (store-word (+ reg imm) val))

(defun llvm-riscv64:C_SD (val reg imm)
  (store-word (+ reg imm) val))

(defun llvm-riscv64:SW (val reg imm)
  (store-word (+ reg imm) (cast-low (/ (word-width) 2) val)))

(defun llvm-riscv64:SH (val reg imm)
  (store-word (+ reg imm) (cast-low (/ (word-width) 4) val)))


(defun llvm-riscv64:SB (val reg imm)
  (store-byte (+ reg imm) val))

;;; Bitwise Operations

(defun llvm-riscv64:ANDI (dst src off)
  (set$ dst (logand src off)))

(defun llvm-riscv64:ORI (dst src off)
  (set$ dst (logor src off)))

(defun llvm-riscv64:XORI (dst src off)
  (set$ dst (logxor src off)))

(defun llvm-riscv64:SRLI (dst reg off)
  (set$ dst (rshift reg off)))

(defun llvm-riscv64:C_SRLI (dst reg off)
  (set$ dst (rshift reg off)))

(defun llvm-riscv64:C_SRAI (dst src imm)
  (set$ dst (arshift src imm)))

(defun llvm-riscv64:SRAI (dst src imm)
  (set$ dst (arshift src imm)))


(defun llvm-riscv64:SLLI (dst reg off)
  (set$ dst (lshift reg off)))

(defun llvm-riscv64:C_SLLI (dst reg off)
  (set$ dst (lshift reg off)))

;;; Comparison
(defun llvm-riscv64:SLTI (dst src off)
  (set$ dst (< dst src off)))


;;; Jumps
(defun llvm-riscv64:JAL (lr off)
  (let ((pc (get-program-counter)))
    (set$ lr (+ pc 4))
    (exec-addr (+ pc off))))

(defun llvm-riscv64:JALR (lr rs off)
  (let ((pc (get-program-counter)))
    (set$ lr (+ pc 4))
    (exec-addr (+ pc rs off))))

(defun llvm-riscv64:C_JR (dst)
  (exec-addr dst))

(defun llvm-riscv64:C_J (dst)
  (exec-addr (+ (get-program-counter) dst)))

(defun llvm-riscv64:C_JALR (dst)
  (set X1 (+ (get-program-counter) 2))
  (exec-addr dst))


(defmacro conditional-jump (cmp off)
  (let ((pc (get-program-counter)))
    (when cmp
      (exec-addr (+ pc off)))))

(defun llvm-riscv64:BEQ (rs1 rs2 off)
  (conditional-jump (= rs1 rs2) off))

(defun llvm-riscv64:BLT (rs1 rs2 off)
  (conditional-jump (< rs1 rs2) off))

(defun llvm-riscv64:BNE (rs1 rs2 off)
  (conditional-jump (/= rs1 rs2) off))

(defun llvm-riscv64:C_BEQ (rs1 rs2 off)
  (conditional-jump (= rs1 rs2) off))

(defun llvm-riscv64:C_BLT (rs1 rs2 off)
  (conditional-jump (< rs1 rs2) off))

(defun llvm-riscv64:C_BNE (rs1 rs2 off)
  (conditional-jump (/= rs1 rs2) off))

(defun llvm-riscv64:BEQZ (rs1 off)
  (conditional-jump (is-zero rs1) off))

(defun llvm-riscv64:C_BEQZ (rs1 off)
  (conditional-jump (is-zero rs1) off))


(defun llvm-riscv64:BNEZ (rs1 off)
  (conditional-jump (not (is-zero rs1)) off))

(defun llvm-riscv64:C_BNEZ (rs1 off)
  (conditional-jump (not (is-zero rs1)) off))
