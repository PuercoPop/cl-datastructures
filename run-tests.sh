#!/bin/sh
sbcl --eval "(ql:quickload :cl-pfds-tests)" \
     --eval "(cl-pfds:run! ch02)" \
     --eval "(progn (terpri) (sb-ext:quit))"
