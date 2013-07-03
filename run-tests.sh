#!/bin/sh
sbcl --eval "(ql:quickload :cl-pfds-tests)" \
     --eval "(pfds-tests:runner)" \
     --eval "(progn (terpri) (sb-ext:quit))"
