(defun llvm-riscv:ADDI (dst src off)
  (set$ dst (+ src off)))

(defun llvm-riscv:ANDI (dst src off)
  (set$ dst (logand src off)))

(defun llvm-riscv:ORI (dst src off)
  (set$ dst (logor src off)))

(defun llvm-riscv:XORI (dst src off)
  (set$ dst (logxor src off)))

(defun llvm-riscv:SLTI (dst src off)
  (set$ dst (< dst src off)))

(defun llvm-riscv:AUIPC (dst off)
  (set$ dst (+ (get-program-counter) off)))

(defun llvm-riscv:AUIPC (dst reg off)
  (set$ dst (+ (get-program-counter) off)))

(defun llvm-riscv64:JAL (lr off)
  (let ((pc (get-program-counter)))
    (set$ lr (+ pc 4))
    (exec-addr (+ pc off))))

(defun llvm-riscv64:JALR (lr rs off)
  (let ((pc (get-program-counter)))
    (set$ lr (+ pc 4))
    (exec-addr (+ pc rs off))))


(defmacro conditional-jump (cmp rs1 rs2 off)
  (let ((pc (get-program-counter)))
    (when (cmp rs1 rs2)
      (exec-addr (+ pc off)))))

(defun llvm-riscv64:BEQ (rs1 rs2 off)
  (conditional-jump = rs1 rs2 off))

(defun llvm-riscv64:BLT (rs1 rs2 off)
  (conditional-jump < rs1 rs2 off))

(defun llvm-riscv64:BNE (rs1 rs2 off)
  (conditional-jump /= rs1 rs2 off))
