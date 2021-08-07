(in-package :gravedigger)

(setq *random-state* (make-random-state t))

(defun main ()
  (print-dungeon (generate-dungeon 'bsp-rooms)))
