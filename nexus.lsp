
; File name               :    nexus.lsp
;
; Created at              :    6/27/1990 16:54:47 by CL(NX) Indexer
;
; Indexed (source) file   :    nexus.lsp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; load_nexus.......................    2
;
;                                    Common Lisp Indexer, v1.1, by Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

(setq *load-verbose* nil
      *verbose* nil)

;                                  -----
;                                    2
;                                  -----

(defun load_nexus ()
  "This function loads all the Nexus modules into memory. It returns t
   if all exist in the current directory and error otherwise. The order of
   loading is important!" 
  (format t "~2%Loading NEXUS ... ~%") 
  (load 'memory.lsp) 
  (load 'utility.lsp) 
  (init_memory) 
  (load 'parser.lsp) 
  (load 'lexicon.lsp) 
  (load 'pgd.lsp) 
  (load 'match.lsp) 
  (load 'cause.lsp) 
  (format t "~%Welcome to NEXUS, a program for learning causal relations.~
~%Design and development:~24TH. Cem Bozsahin   1989-1990~
~%~24TArtificial Intelligence Laboratory~
~%~24TComputer Science Department~%~24TArizona State University, Tempe AZ.~
~%Usage: (nexus <sentence list>)") 
  (load_memory) 
  (load_dynamic_variables) 
  (format t "~%Ready."))

;                                  -----
;                                    3
;                                  -----

(load_nexus)
