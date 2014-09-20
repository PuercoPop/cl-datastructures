(fiasco:define-test-package :ordered-tests
  (:use :cl :ordered))
(in-package :ordered-tests)

(deftest ord-eql-sanity-check ()
  (let ((a 0)
        (b 1)
        (c 0)) 
    (is (ord-eql a c))
    (is (not (ord-eql a b)))))

(deftest ord-neql-sanity-check ()
  (let ((a 0)
        (b 1)
        (c 0)) 
    (is (not (ord-neql a c)))
    (is (ord-neql a b))))

(deftest ord-lt-sanity-check ()
  (let ((a 0)
        (b 1)) 
    (is (ord-lt a b))
    (is (not (ord-lt b a)))))

(deftest ord-leq-sanity-check ()
  (let ((a 0)
        (b 1)
        (c 0)) 
    (is (ord-leq a c))
    (is (ord-leq a b))
    (is (not (ord-leq b a)))))

(deftest ord-gt-sanity-check ()
  (let ((a 0)
        (b 1)) 
    (is (ord-gt b a))
    (is (not (ord-gt a b)))))
