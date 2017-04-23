
; File name               :    parser.lsp
;
; Created at              :    7/12/1990 19:56:06 by CL(NX) Indexer
;
; Indexed (source) file   :    parser.lsp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; add_packets......................    8
; add_stack........................   11
; check_top........................    4
; def_word.........................   15
; do_assigns.......................    6
; is_triggered.....................    5
; load_def.........................   12
; look_ahead.......................   17
; np_type..........................   16
; process_sentence.................    2
; process_text.....................    1
; reassign.........................    7
; remove_variables.................    9
; req_clause.......................   13
; run_stack........................    3
; set_discourse....................   18
; top_of...........................   10
;
;                                    Common Lisp Indexer, v1.1, by Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

(defun process_text (text)
  (setq *discourse_he* '(human (male (unknown)))
        *discourse_she* '(human (female (unknown)))
        *discourse2_he* nil
        *discourse2_she* nil
        *discourse_it* nil
        *discourse_they* nil
        *discourse2_it* nil
        *discourse2_they* nil) 
  (mapcar #'(lambda (sentence) (process_sentence sentence)) text))

;                                  -----
;                                    2
;                                  -----

(defun process_sentence (sentence)
  "note: time attribute is found in either one of prepositional phrases or
   implied by the tense of the verb. In latter case, a dummy is created for
   symbolic time value. If time is explicitly mentioned, it overrides that
   value--cf. the 'or' function as argument to 'append'." 
  (setq *concept* nil
        *stack* nil
        *word* nil
        *sentence* (cons '*start* sentence)) 
  (do ()
      ((not (setq *word* (pop *sentence*)))
       (remove_variables (append *concept*
                                 (list (list 'time
                                             (list
                                              *temprel*
                                              (or
                                               (remove_variables *time*)
                                               (gentemp "time-"))))))))
    (load_def)
    (run_stack)))

;                                  -----
;                                    3
;                                  -----

(defun run_stack ()
  (do ((request nil) 
       (triggered nil))
      ((not (setq request (check_top *stack*))) (add_packets triggered))
    (pop *stack*)
    (do_assigns request)
    (push request triggered)))

;                                  -----
;                                    4
;                                  -----

(defun check_top (stack)
  (cond (stack
         (do ((request nil) 
              (packet (top_of stack)))
             ((not (setq request (pop packet))) request)
           (cond ((is_triggered request) (return request)))))))

;                                  -----
;                                    5
;                                  -----

(defun is_triggered (request)
  (let ((test (req_clause 'test request))) (or (null test) (eval (car test)))))

;                                  -----
;                                    6
;                                  -----

(defun do_assigns (request)
  (do ((assignments (req_clause 'assign request)))
      ((not assignments))
    (reassign (pop assignments) (pop assignments))))

;                                  -----
;                                    7
;                                  -----

(defun reassign (var val) (set var (eval val)))

;                                  -----
;                                    8
;                                  -----

(defun add_packets (requests)
  (mapc #'(lambda (request) (add_stack (req_clause 'next_packet request)))
        requests))

;                                  -----
;                                    9
;                                  -----

(defun remove_variables (cd_form)
  (cond ((atom cd_form) cd_form)
        ((is_var cd_form) (remove_variables (eval (name_var cd_form))))
        ((not (cd_event_type cd_form)) cd_form)
        (t
         (let ((val nil))
           (cons (header_cd cd_form)
                 (remove-if #'null
                            (mapcar #'(lambda (pair)
                                        (cond
                                         ((setq
                                           val
                                           (remove_variables
                                            (filler_pair pair)))
                                          (list (role_pair pair) val))))
                                    (roles_cd cd_form))))))))

;                                  -----
;                                    10
;                                  -----

(defun top_of (stack) (car stack))

;                                  -----
;                                    11
;                                  -----

(defun add_stack (packet)
  (and packet (push packet *stack*)) 
  packet)

;                                  -----
;                                    12
;                                  -----

(defun load_def ()
  (let ((packet (get *word* 'definition)))
    (cond (packet (add_stack packet)) (t))))

;                                  -----
;                                    13
;                                  -----

(defun req_clause (key l) (let ((x (assoc key l))) (and x (cdr x))))

;                                  -----
;                                    14
;                                  -----

(proclaim '(special *part_of_speech* *cd_form* *subject* *predicates* *concept*
            *stack* *word* *sentence* *noun* *act* *tense* *discourse_he*
            *discourse_she* *discourse_it* *discourse_they* *discourse2_he*
            *discourse2_she* *discourse2_it* *discourse2_they* *time* *temprel*
            *co_agent*))

;                                  -----
;                                    15
;                                  -----

(defmacro def_word (word &rest def)
  (setf (get word 'definition) def) 
  `',word)

;                                  -----
;                                    16
;                                  -----

(defun np_type (slot)
  "This function determines the type of noun phrase in the slot. It returns
    the case for which the slot could be used." 
  (cond ((member (car slot) '(physobj artifact inanimate)) 'location)
        ((member (car slot) '(human group animal animate plant)) 'object)
        ((member (car slot) '(action plan goal theme)) 'time)
        (t (car slot))))

;                                  -----
;                                    17
;                                  -----

(defun look_ahead ()
  "This function removes the first element from *sentence* and 
   returns it. It effectively eliminates the loading of that word's packet
   onto the *stack*." 
  (and *sentence* (pop *sentence*)))

;                                  -----
;                                    18
;                                  -----

(defun set_discourse (subject_np)
  "In Nexus, default pronoun resolution is based on recency. The exception is
   when there is a complement to a verb phrase. In this case, the subject
   of the sentence tells which discourse variable to update and use later
   in the complement which is a subordinate sentence." 
  (case (caadr subject_np)
    (male (setq *discourse_he* subject_np)) 
    (female (setq *discourse_she* subject_np)) 
    (t
     (cond ((equal (car subject_np) 'group) (setq *discourse_they* subject_np))
           (t (setq *discourse_it* subject_np))))))
