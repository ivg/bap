(require bits)

(defpackage pcode (:use core target))
(in-package pcode)

(defmacro cast-word (x) (cast word-width x))
(defmacro coerce (type x)
  (if (/= (word-width x) type) (extract (-1 type) 0 x) x))

(defmacro set# (typ dst src)
  (if (is-symbol typ)
    (store-word (cast-word dst) src)
    (set$ dst src)))

(defmacro get# (typ src)
  (if (is-symbol typ) (load-word (cast-word src))
    (coerce typ src)))

(defmacro get# (op typ src)
  (op (get# typ src)))

(defmacro get# (op tx x ty y)
  (op (get# tx x) (get# ty y)))

(defun COPY (td d ts s)
  (set# td d (get# ts s)))

(defun STORE (_ _ _ ptr typ val)
  (store-word ptr (get# typ val)))

(defun LOAD (td dst _ _ _ ptr)
  (set# td dst (load-bits td ptr)))

(defun branch (typ dst)
  (if (is-symbol typ)
      (exec-addr dst)
    (goto-subinstruction dst)))

(defun BRANCH (typ dst)
  (branch typ dst))

(defun CBRANCH (typ dst tc cnd)
  (when (get# tc cnd) (branch typ dst)))

(defun BRANCHIND (_ dst)
  (exec-addr dst))

(defun CALL (typ dst)
  (exec-addr dst))

(defun CALLIND (typ dst)
  (exec-addr dst))

(defun RETURN (typ dst)
  (exec-addr dst))

(defun PIECE (tr r tx x ty y)
  (set# tr r (get# concat tx x ty y)))

(defun SUBPIECE (tr r tx x ts s)
  (set# tr r (rshift (get# tx x) (* 8 (get# ts s)))))

(defun INT_EQUAL (tr r tx x ty y)
  (set# tr r (get# = tx x ty y)))

(defun INT_NOTEQUAL (tr r tx x ty y)
  (set# tr r (get# /= tx x ty y)))

(defun INT_LESS (tr r tx x ty y)
  (set# tr r (get# < tx x ty y)))

(defun INT_SLESS (tr r tx x ty y)
  (set# tr r (get# s< tx x ty y)))

(defun INT_LESSEQUAL (tr r tx x ty y)
  (set# tr r (get# <= tx x ty y)))

(defun INT_SLESSEQUAL (tr r tx x ty y)
  (set# tr r (get# s<= tx x ty y)))

(defun INT_ZEXT (tr r tx x)
  (set# tr r (cast-unsigned tr (get# tx x))))

(defun INT_SEXT (tr r tx x)
  (set# tr r (cast-signed tr (get# tx x))))

(defun INT_ADD (tr r tx x ty y)
  (set# tr r (get# + tx x ty y)))

(defun INT_SUB (tr r tx x ty y)
  (set# tr r (get# - tx x ty y)))

(defun INT_CARRY (tr r tx x ty y)
  (let ((x (get# tx x))
        (y (get# ty y))
        (z (+ x y)))
    (set# tr r (coerce tr (carry z x y)))))

(defun INT_SCARRY (tr r tx x ty y)
  (let ((x (get# tx x))
        (y (get# ty y))
        (z (+ x y)))
    (set# tr r (coerce tr (overflow z x y)))))

(defun INT_SBORROW (tr r tx x ty y)
  (let ((x (get# tx x))
        (y (get# ty y))
        (z (- x y)))
    (set# tr r (coerce tr (overflow z x (- y))))))

(defun INT_2COMP (tr r tx x)
  (set# tr r (get# - tx x)))

(defun INT_NEGATE (tr r tx x)
  (set# tr r (get# lnot tx x)))

(defun INT_XOR (tr r tx x ty y)
  (set# tr r (get# logxor tx x ty y)))

(defun INT_AND (tr r tx x ty y)
  (set# tr r (get# logand tx x ty y)))

(defun INT_OR (tr r tx x ty y)
  (set# tr r (get# logor tx x ty y)))

(defun INT_LEFT (tr r tx x ty y)
  (set# tr r (get# lshift tx x ty y)))

(defun INT_RIGHT (tr r tx x ty y)
  (set# tr r (get# rshift tx x ty y)))

(defun INT_SRIGHT (tr r tx x ty y)
  (set# tr r (get# arshift tx x ty y)))

(defun INT_MULT (tr r tx x ty y)
  (set# tr r (get# * tx x ty y)))

(defun INT_DIV (tr r tx x ty y)
  (set# tr r (get# / tx x ty y)))

(defun INT_SDIV (tr r tx x ty y)
  (set# tr r (get# s/ tx x ty y)))

(defun INT_MOD (tr r tx x ty y)
  (set# tr r (get# mod tx x ty y)))

(defun INT_SMOD (tr r tx x ty y)
  (set# tr r (get# signed-mod tx x ty y)))

(defun BOOL_NEGATE (tr r tx x)
  (set# tr r (get# not tx x)))

(defun BOOL_AND (tr r tx x ty y)
  (set# tr r (get# logand tx x ty y)))

(defun BOOL_OR (tr r tx x ty y)
  (set# tr r (get# logor tx x ty y)))

(defun BOOL_XOR (tr r tx x ty y)
  (set# tr r (get# logxor tx x ty y)))


(defun FLOAT_ADD (tr r tx x ty y)
  (set# tr r (intrinsic '__fadd_rne_ieee754_binary64
                        (get# tx x) (get# ty y)
                        :result 64)))

;; the idea is to declare primitives and then depending on
;; the context use them.
;; The context should specify the set of logics that we would like
;; to use and each primitive operation belongs to some logic.

;; (declare (context empty-float-theory))

;; (in-package core)
;; (defun ieee754-fadd.rne (x y)
;;   (intrinsic __fadd_rne_ieee754_bin64
;;              :src1  x y
;;              :oreg x))

;; (defun normalize-typ (typ)
;;   (if (is-symbol typ) (word-size)))

;; (defun FLOAT_ADD (tr r tx x ty y)
;;   (set# tr r (intrinsic '__fadd_rne_ieee754_bin64
;;                         (get# tx x)
;;                         (get# ty y)
;;                         :writes 'RAX 64
;;                         :return tr)))
;; intrinsic NAME ARGS... OUTPUTS...
;; calls the intrinsic function passing to it arguments
;; in the specified order. Then N-th argument is passed
;; via the variable intrinsic:xN.
;; The optional OUTPUTS defines how the intrinisc returns
;; the information back (if it does). If no OUTPUTS are
;; specified, then the intrinsic doesn't produce any observable
;; effects and is reified to the call that falls through.
;; The following keyworded arguments are allowed, :return,
;; :aborts, :writes, and :stores.
;;
;; :RESULT SIZE
;;
;; The intrinsic is evaluated to a bitvector with SIZE bits (which has
;; to be a statically known integer), therefore the whole intrinisc
;; form could be assigned to a value. The result is returned from the
;; intrinsic via the intrinsic:result variable.
;;
;; :WRITES REG SIZE?
;;
;; The intrinsic writes the specified variable REG. If REG is the target
;; register then SIZE is optional. This parameter can be specified several
;; times. The value of REG is returned from the intrinsic vi
;;

;; (defun CPUID ()
;;   (set EAX (intrinsic '__example EAX EBX (load-word EDX)
;;                       :result 32
;;                       :writes 'EBX
;;                       :writes 'ECX
;;                       :writes 'EDX
;;                       :stores (+ EAX 12) 32)))

;; reified to
;;
;; intrinsic:x0 := EAX
;; intrinsic:x1 := EBX
;; intrinsic:x2 := mem[EDX]:32
;; call __example with return @next
;;
;; next:
;; EBX := intrinsic:y0
;; ECX := intrinsic:y1
;; EDX := intrinsic:y2
;; mem := mem with [EAX+12]:32 <- intrinsic:y3
;;


;; (defun FLOAT_ADD (tr r tx x ty y)
;;   (set# tr r (apply tr :__ieee754_add_rne (get# tx x) (get# ty y))))


;; we can implement an intrinsic reification for each unhandled LLVM
;; instruction, e.g.,
;; (defgeneric (llvm- (arch :word)))
