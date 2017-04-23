
; File name               :    utility.lsp
;
; Created at              :    6/27/1990 16:56:11 by CL(NX) Indexer
;
; Indexed (source) file   :    utility.lsp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; add_progn........................   33
; affirm...........................   57
; and_function.....................   66
; atrans...........................   39
; basic_cd.........................    4
; cdpath...........................   64
; cd_event_type....................    3
; check_input......................    1
; cons_end.........................   35
; cons_l...........................   70
; do_clause........................   27
; ended............................   74
; filler_pair......................   18
; filler_role......................   19
; filter...........................   76
; find_cd_causal_link..............    8
; find_cd_state_changes............    9
; find_max.........................   12
; for1.............................   29
; foritem..........................   28
; for_lambda.......................   31
; for_mapfn........................   30
; for_when.........................   32
; future...........................   63
; gencd............................   10
; get_keyword......................   34
; goal_cd..........................    6
; grasp............................   40
; has..............................   49
; header_cd........................   15
; high_level_cd....................    5
; ingest...........................   42
; intended_function................   75
; is_a.............................   14
; is_at............................   50
; is_var...........................   21
; loop1............................   26
; make_cd..........................   77
; make_var.........................   11
; maybe............................   59
; mbuild...........................   43
; mloc.............................   51
; mode.............................   56
; monitor..........................   67
; mtrans...........................   44
; name_var.........................   22
; negate...........................   58
; neg_val..........................   72
; or_function......................   65
; plan.............................   45
; plan_cd..........................    7
; pos_val..........................   71
; ppf..............................   36
; ppm..............................   37
; propel...........................   46
; ptrans...........................   47
; question.........................   60
; relation.........................   53
; repeat...........................   38
; rloff............................   68
; roles_cd.........................   16
; role_pair........................   17
; setrole..........................   20
; shorthand_variable...............   23
; state............................   52
; subsume..........................   13
; tf...............................   62
; un_grasp.........................   41
; un_question......................   61
; var_list.........................   25
; verify_input_2_p.................    2
; wants............................   48
; where_is.........................   54
; who_has..........................   55
; wlotf............................   69
; zero_val.........................   73
;
;                                    Common Lisp Indexer, v1.1, by Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

(defun check_input (text)
  "This function verifies natural language input format. It should be in
   the form of ( (<sentence>) ... (<sentence>) ) . Returns t if it is o.k.
   and nil otherwise. " 
  (cond ((and (listp text) (verify_input_2_p text)) text) (t '(nil))))

;                                  -----
;                                    2
;                                  -----

(defun verify_input_2_p (text)
  "This is a companion function to verify_input_p. Returns t if every
   car of text is a list, and nil otherwise." 
  (cond ((endp text) t)
        ((listp (car text)) (verify_input_2_p (cdr text)))
        (t nil)))

;                                  -----
;                                    3
;                                  -----

(defun cd_event_type (cd)
  "This function returns the type of major ACT in the cd. It could be either
   'mental, 'physical, 'theme, plan, goal, or nil." 
  (cond ((atom cd) nil)
        ((member (header_cd cd) '(atrans mtrans mbuild)) 'mental)
        ((member (header_cd cd)
                 '(is move attend speak grasp propel ptrans ingest expel))
         'physical)
        ((member (header_cd cd) '(plan goal theme prox do)) 'theme)
        (t nil)))

;                                  -----
;                                    4
;                                  -----

(defun basic_cd (cd) (member (cd_event_type cd) '(mental physical)))

;                                  -----
;                                    5
;                                  -----

(defun high_level_cd (cd) (equal (cd_event_type cd) 'theme))

;                                  -----
;                                    6
;                                  -----

(defun goal_cd (cd) (member (header_cd cd) '(plan goal)))

;                                  -----
;                                    7
;                                  -----

(defun plan_cd (cd) (member (header_cd cd) '(plan goal)))

;                                  -----
;                                    8
;                                  -----

(defun find_cd_causal_link (cd)
  "This function returns the sub-cd structure that the major
   cd is causally related to. The value returned is an event structure, or
   nil if no causal connection exists." 
  (filler_role 'cause cd))

;                                  -----
;                                    9
;                                  -----

(defun find_cd_state_changes (cd)
  "This function returns a list of state changes mentioned in the CD. Each
   state change is represented as (state (new_value))." 
  (do* ((cd_states '(mloc health fear anger mental_state physical_state
                          consciousness hunger disgust surprise control part
                          poss ownership contain proximity)
                   (cdr cd_states)) 
        (state_change (assoc (car cd_states) (roles_cd cd))) 
        (state_change_list (cond (state_change (list state_change)) (t 'nil))
                           (cond (state_change
                                  (cons state_change state_change_list))
                                 (t state_change_list))))
       ((null cd_states) state_change_list)))

;                                  -----
;                                    10
;                                  -----

(defun gencd (cd_constant)
  "Generalize CD: This function turns the role pairs in the cd_constant
   into variables (patterns). It uses internally generated symbols for
   this purpose. It returns the generalized CD form. The name attribute is
    not generalized since it has to match as a constant (i.e., two goals
    can match only if they have the same name plus other slots match). The cd
    can consist of only a single slot" 
  (cond ((cd_event_type cd_constant)
         (do* ((role_list cd_constant (roles_cd role_list)) 
               (role nil (caar role_list)) 
               (g_list nil
                       (cond ((equal role 'name)
                              (cons (role_pair role_list) g_list))
                             (role
                              (cons (list role (make_var (gentemp))) g_list))
                             (t g_list))))
              ((null role_list)
               (cons (header_cd cd_constant) (reverse g_list)))))
        (t (list (header_cd cd_constant) (make_var (gentemp))))))

;                                  -----
;                                    11
;                                  -----

(defun make_var (var_name)
  "This function creates and returns a (*var* 'var_name) structure from 
   var_name. It is cumbersome but it works. " 
  (list '*var* (eval (list 'quote (list 'quote var_name)))))

;                                  -----
;                                    12
;                                  -----

(defun find_max (pair_list)
  "This function finds the maximum number in the list of pairs. It returns
   the pair if the maximum number is greater than zero. Returns nil otherwise." 
  (car (sort (remove-if #'zerop pair_list :key #'second) #'> :key #'second)))

;                                  -----
;                                    13
;                                  -----

(defun subsume (slot1 slot2)
  "This function returns t if slot1 subsumes slot2 and nil otherwise. In 
   Nexus, slot values start with the object type, e.g.,
   (actor (human (name (modigliani)))). Human is the slot type. Slot
   value can also be an embedded CD structure." 
  (cond ((equal slot1 slot2) t)
        ((and (atom slot1) (atom slot2) nil))
        ((and (cd_event_type slot1) (cd_event_type slot2)) t)
        ((and slot1 slot2 (is_a (car slot2) (car slot1))))
        (t nil)))

;                                  -----
;                                    14
;                                  -----

(defun is_a (class1 class2)
  "determines if class1 is_a class2. Classes are assumed to have a property
   named is_a." 
  (cond ((equal class1 class2))
        ((equal (get class1 'is_a) 'root) nil)
        ((or (null class1) (null class2)) nil)
        (t (is_a (get class1 'is_a) class2))))

;                                  -----
;                                    15
;                                  -----

(defun header_cd (x) (car x))

;                                  -----
;                                    16
;                                  -----

(defun roles_cd (x) (cdr x))

;                                  -----
;                                    17
;                                  -----

(defun role_pair (x) (car x))

;                                  -----
;                                    18
;                                  -----

(defun filler_pair (x) (cadr x))

;                                  -----
;                                    19
;                                  -----

(defun filler_role (role cd)
  (let ((pair (assoc role (roles_cd cd)))) (and pair (filler_pair pair))))

;                                  -----
;                                    20
;                                  -----

(defun setrole (role filler cd)
  (cons (header_cd cd)
        (append (mapcan #'(lambda (pair)
                            (cond ((not (equal (role_pair pair) role))
                                   (list pair))))
                        (roles_cd cd))
                (list (list role filler)))))

;                                  -----
;                                    21
;                                  -----

(defun is_var (x) (and (consp x) (equal (car x) '*var*)))

;                                  -----
;                                    22
;                                  -----

(defun name_var (x) (and (consp x) (consp (cdr x)) (eval (cadr x))))

;                                  -----
;                                    23
;                                  -----

(defun shorthand_variable (stream char)
  (declare (ignore char)) 
  (list '*var* (list 'quote (read stream t nil t))))

;                                  -----
;                                    24
;                                  -----

(set-macro-character #\? #'shorthand_variable)

;                                  -----
;                                    25
;                                  -----

(defun var_list (l)
  (cond ((null l) nil) (t (cons (list (car l) (cadr l)) (var_list (cddr l))))))

;                                  -----
;                                    26
;                                  -----

(defun loop1 (clauses i_body r_body)
  (append (list 'prog (var_list i_body))
          (cons 'loop (apply 'append (mapcar 'do_clause clauses)))
          (list '(go loop) 'exit (cons 'return r_body))))

;                                  -----
;                                    27
;                                  -----

(defun do_clause (clause)
  (case (car clause)
    (initial nil) 
    (result nil) 
    (while (list (list 'or (cadr clause) '(go exit)))) 
    (do (cdr 
         clause)) 
    (until (list (list 'and (cadr clause) '(go exit)))) 
    (t (terpri) (princ "unknown keyword") (print clause))))

;                                  -----
;                                    28
;                                  -----

(defmacro foritem (paramarg &body l)
  (for1 paramarg (get_keyword 'when l) (get_keyword 'do l)
        (get_keyword 'save l) (get_keyword 'exists l)))

;                                  -----
;                                    29
;                                  -----

(defun for1 (in when do save exists)
  (cons (for_mapfn when do save exists)
        (cons (for_lambda (car in) when do save exists) (cddr in))))

;                                  -----
;                                    30
;                                  -----

(defun for_mapfn
       (when do
         save 
         exists)
  (cond (do 'mapc) (exists 'some) (when 'mapcan) (t 'mapcar)))

;                                  -----
;                                    31
;                                  -----

(defun for_lambda (var when do save exists)
  (list 'function
        (cons 'lambda
              (cons (list var)
                    (cond (when (for_when when do save))
                          (t (or do save exists)))))))

;                                  -----
;                                    32
;                                  -----

(defun for_when (when do save)
  (list (list 'cond
              (append (add_progn when)
                      (or do (list (cons 'list (add_progn save))))))))

;                                  -----
;                                    33
;                                  -----

(defun add_progn (l) (cond ((cdr l) (list (cons 'progn l))) (t l)))

;                                  -----
;                                    34
;                                  -----

(defun get_keyword (key l) (cdr (assoc key l)))

;                                  -----
;                                    35
;                                  -----

(defun cons_end (l x) (append l (list x)))

;                                  -----
;                                    36
;                                  -----

(defmacro ppf (func &body rest) (list 'pprint (list 'symbol-function func)))

;                                  -----
;                                    37
;                                  -----

(defmacro ppm (macro &body rest) (list 'pprint (list 'macro-function macro)))

;                                  -----
;                                    38
;                                  -----

(defmacro repeat (&body rest)
  (loop1 rest (get_keyword 'initial rest) (get_keyword 'result rest)))

;                                  -----
;                                    39
;                                  -----

(defun atrans (actor object to from)
  (list 'atrans (list 'actor actor) (list 'object object) (list 'to to)
        (list 'from from)))

;                                  -----
;                                    40
;                                  -----

(defun grasp (actor object)
  (list 'grasp (list 'actor actor) (list 'object object)))

;                                  -----
;                                    41
;                                  -----

(defun un_grasp (actor object) (tf (grasp actor object)))

;                                  -----
;                                    42
;                                  -----

(defun ingest (actor object)
  (list 'ingest (list 'actor actor) (list 'object object)))

;                                  -----
;                                    43
;                                  -----

(defun mbuild (actor object)
  (list 'mbuild (list 'actor actor) (list 'object object)))

;                                  -----
;                                    44
;                                  -----

(defun mtrans (actor object to from)
  (list 'mtrans (list 'actor actor) (list 'object object)
        (list 'to (list 'cp (list 'part to))) (list 'from from)))

;                                  -----
;                                    45
;                                  -----

(defun plan (actor object)
  (list 'plan (list 'actor actor) (list 'object object)))

;                                  -----
;                                    46
;                                  -----

(defun propel (actor object to)
  (list 'propel (list 'actor actor) (list 'object object) (list 'to to)))

;                                  -----
;                                    47
;                                  -----

(defun ptrans (actor object to from)
  (list 'ptrans (list 'actor actor) (list 'object object) (list 'to to)
        (list 'from from)))

;                                  -----
;                                    48
;                                  -----

(defun wants (actor goal) (list 'want (list 'actor actor) (list 'object goal)))

;                                  -----
;                                    49
;                                  -----

(defun has (actor object) (list 'cont (list 'actor object) (list 'val actor)))

;                                  -----
;                                    50
;                                  -----

(defun is_at (actor lok) (list 'loc (list 'actor actor) (list 'val lok)))

;                                  -----
;                                    51
;                                  -----

(defun mloc (actor con)
  (list 'mloc (list 'con con) (list 'val (list 'cp (list 'part actor)))))

;                                  -----
;                                    52
;                                  -----

(defun state (actor st mode)
  (list st (list 'actor actor) (list 'mode (list mode))))

;                                  -----
;                                    53
;                                  -----

(defun relation (actor object rel mode)
  (list rel (list 'actor actor) (list 'to object) (list 'mode (list mode))))

;                                  -----
;                                    54
;                                  -----

(defun where_is (x) (list 'loc (list 'actor x) (list 'val '(*var* unspec))))

;                                  -----
;                                    55
;                                  -----

(defun who_has (x) (list 'cont (list 'actor x) (list 'val '(*var* unspec))))

;                                  -----
;                                    56
;                                  -----

(defun mode (cd) (cdpath '(mode) cd))

;                                  -----
;                                    57
;                                  -----

(defun affirm (cd)
  (cond ((member 'pos (mode cd)) cd)
        (t (setrole 'mode (cons 'pos (remove 'neg (mode cd))) cd))))

;                                  -----
;                                    58
;                                  -----

(defun negate (cd)
  (cond ((member 'neg (mode cd)) (affirm cd))
        (t (setrole 'mode (cons 'neg (remove 'pos (mode cd))) cd))))

;                                  -----
;                                    59
;                                  -----

(defun maybe (cd)
  (cond ((member 'maybe (mode cd)) cd)
        (t (setrole 'mode (cons 'maybe (mode cd)) cd))))

;                                  -----
;                                    60
;                                  -----

(defun question (cd)
  (cond ((member 'ques (mode cd)) cd)
        (t (setrole 'mode (cons 'ques (mode cd)) cd))))

;                                  -----
;                                    61
;                                  -----

(defun un_question (cd) (setrole 'mode (remove 'ques (mode cd)) cd))

;                                  -----
;                                    62
;                                  -----

(defun tf (cd)
  (cond ((member 'tf (mode cd)) cd)
        (t (setrole 'mode (cons 'tf (mode cd)) cd))))

;                                  -----
;                                    63
;                                  -----

(defun future (cd) (setrole 'time 'future cd))

;                                  -----
;                                    64
;                                  -----

(defun cdpath (rolelist cd)
  (foritem (role in rolelist)
    (do (setq 
         cd 
         (filler_role role cd)))) 
  cd)

;                                  -----
;                                    65
;                                  -----

(defun or_function (clauses)
  "This function is needed because 'or' is a macro hence cannot
   be used with 'apply'. Does the same thing as 'or'." 
  (cond ((null clauses) nil) ((car clauses)) (t (or_function (rest clauses)))))

;                                  -----
;                                    66
;                                  -----

(defun and_function (clauses)
  "This function is needed because 'and' is a macro hence cannot
   be used with 'apply'. Does the same thing as 'and'." 
  (and (consp clauses) (not (or_function (mapcar #'not clauses)))))

;                                  -----
;                                    67
;                                  -----

(defun monitor (message &rest pars)
  "This function prints the message if *verbose* is set to non-nil. Used
   for debugging and annotation purposes." 
  (cond (*verbose* (pprint message) (mapc #'pprint pars) t) (t t)))

;                                  -----
;                                    68
;                                  -----

(defun rloff (fn)
  "read lisp object from file" 
  (with-open-file (ifile fn :direction :input :if-does-not-exist :error)
    (read ifile)))

;                                  -----
;                                    69
;                                  -----

(defun wlotf (obj fn)
  "write lisp object to file" 
  (with-open-file (ifile fn :direction :output :if-exists :new-version)
    (pprint obj ifile)))

;                                  -----
;                                    70
;                                  -----

(defun cons_l (ob1 ob2)
  "this creates a list of the form (ob1 (ob2)) and returns it." 
  (cons ob1 (list ob2)))

;                                  -----
;                                    71
;                                  -----

(defun pos_val (cd)
  (cond ((consp cd) (setq cd (car cd)))) 
  (and (numberp cd) (> cd 0)))

;                                  -----
;                                    72
;                                  -----

(defun neg_val (cd)
  (cond ((consp cd) (setq cd (car cd)))) 
  (and (numberp cd) (< cd 0)))

;                                  -----
;                                    73
;                                  -----

(defun zero_val (cd)
  (cond ((consp cd) (setq cd (car cd)))) 
  (or (null cd) (and (numberp cd) (= cd 0))))

;                                  -----
;                                    74
;                                  -----

(defun ended (tense) (and tense (equal tense '(perfect (past)))))

;                                  -----
;                                    75
;                                  -----

(defun intended_function (slot_val)
  "this function returns the intended function of the entity in the slot_val.
   Slot values in Nexus are of the form (entity_type (intended_function(value)))
   " 
  (or (numberp slot_val) (atom slot_val) (caadr slot_val)))

;                                  -----
;                                    76
;                                  -----

(defun filter (cd_list)
  "This function filters out any CD s in the cd_list which have nil slot values" 
  (remove-if #'null
             (mapcar #'(lambda (cd)
                         (cond ((and_function (mapcar #'cadr (roles_cd cd)))
                                cd)
                               (t nil)))
                     cd_list)))

;                                  -----
;                                    77
;                                  -----

(defun make_cd (type &rest roles) (cons type roles))
