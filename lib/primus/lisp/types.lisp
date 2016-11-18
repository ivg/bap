(defun int () (abi-c-int-width))
(defun long () (abi-c-long-width))
(defun long-long () (abi-c-long-long-width))
(defun char () 8)
(defun int32_t () 32)
(defun int64_t () 64)
(defun ptr_t () (word-size))

(defmacro cast (type x) (coerce x 0 (type)))
(defmacro sizeof (type) (type))
