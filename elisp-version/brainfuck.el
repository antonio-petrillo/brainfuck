;;; brainfuck.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Antonio Petrillo
;;
;; Author: Antonio Petrillo <https://github.com/anto>
;; Maintainer: Antonio Petrillo <antonio.petrillo4@studenti.unina.it>
;; Created: December 21, 2021
;; Modified: December 21, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/anto/brainfuck
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(defun brainfuck-execute-program (program)
  "Interpret the brainfuck PROGRAM given in input."
  (let ((program-len (1- (length program)))
        (pointer 0)
        (memory (make-vector 30000 0))
        (ic 0)
        (bf-out (get-buffer-create "brainfuck-output"))
        (bf-message (get-buffer-create "brainfuck-message")))

    (with-current-buffer bf-message
      (delete-region (point-min) (point-max)))

    (with-current-buffer bf-out
      (delete-region (point-min) (point-max)))

    (with-current-buffer bf-message
      (insert "--------------------\nExecuting:\n\n" program "\n\n--------------------\n"))

    (while (<= ic program-len)
      (let ((current-instr (aref program ic)))

        (with-current-buffer bf-message
          (insert "ic: " (number-to-string ic) "\t instr: " current-instr "\n"))

        (cond ((eq current-instr ?+)
               (aset memory pointer (mod (1+ (aref memory pointer)) 30000)))

              ((eq current-instr ?-)
               (aset memory pointer (mod (1- (aref memory pointer)) 30000)))

              ((eq current-instr ?.)
               (let ((byte-out (aref memory pointer)))
                 (with-current-buffer bf-out
                   (insert byte-out))))

              ((eq current-instr ?,)
               (aset memory pointer (read-char)))

              ((eq current-instr ?>)
               (setq pointer (1+ pointer)))

              ((eq current-instr ?<)
               (setq pointer (1- pointer)))

              ((eq current-instr ?\[)
               (if (eq (aref memory pointer) 0)
                   (let ((num-of-bracket 0)
                         (continue t))
                     (while (eq continue t)
                       (setq ic (1+ ic))
                       (cond ((eq (aref program ic) ?\[)
                              (setq num-of-bracket (1+ num-of-bracket)))
                             ((eq (aref program ic) ?\])
                              (if (eq num-of-bracket 0)
                                  (setq continue nil)
                                (setq num-of-bracket (1- num-of-bracket)))))))))

              ((eq current-instr ?\])
               (if (not (eq (aref memory pointer) 0))
                   (let ((num-of-bracket 0)
                         (continue t))
                     (while (eq continue t)
                       (setq ic (1- ic))
                       (cond ((eq (aref program ic) ?\])
                              (setq num-of-bracket (1+ num-of-bracket)))
                             ((eq (aref program ic) ?\[)
                              (if (eq num-of-bracket 0)
                                  (setq continue nil)
                                (setq num-of-bracket (1- num-of-bracket)))))))))

              (t (with-current-buffer bf-message
                   (insert current-instr)))))
      (setq ic (1+ ic)))
    (message "\n\nExecution ended\n--------------------\n")))

(let ((hello-world-program "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."))
  (brainfuck-execute-program hello-world-program))

(let ((factorial "+++++++++++++++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++>+++++++>>+<<[>++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------<<<<.-.>.<.+>>>>>>>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>[<+>-]>[-]>>>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]<[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]<<<++++++++++++++++++++++++++++++++++++++++++++++++.[-]<<<<<<.>>+>[>>+<<-]>>[<<<[>+>+<<-]>>[<<+>>-]>-]<<<<-]"))
  (brainfuck-execute-program factorial))

(let ((fibonacci ">++++++++++>+>+[[+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>[[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>]<<<]"))
  (brainfuck-execute-program fibonacci))

(provide 'brainfuck)
;;; brainfuck.el ends here
