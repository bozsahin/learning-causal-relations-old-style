
; File name               :    lexicon.lsp
;
; Created at              :    7/12/1990 19:54:48 by CL(NX) Indexer
;
; Indexed (source) file   :    lexicon.lsp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; *start*..........................    1
; a................................   35
; after............................   57
; alex.............................   15
; ambulance........................   87
; and..............................   54
; ann..............................   21
; are..............................   28
; arrested.........................   61
; at...............................   59
; august-1979......................   46
; away.............................   69
; been.............................   29
; before...........................   90
; call.............................   84
; called...........................   83
; came.............................   32
; car..............................   47
; children.........................   41
; daniel...........................   17
; david............................   14
; divorced.........................   63
; door.............................   70
; drove............................   68
; escape...........................   72
; forced...........................   79
; get..............................   24
; go...............................   33
; got..............................   23
; had..............................    4
; has..............................    3
; have.............................    2
; he...............................    5
; heard............................   94
; her..............................    8
; herself..........................    7
; him..............................    9
; himself..........................    6
; his..............................   37
; home.............................   50
; house............................   51
; in...............................   56
; into.............................   49
; is...............................   27
; it...............................   53
; itself...........................   10
; jack.............................   18
; john.............................   16
; kicked...........................   80
; kill.............................   74
; kite.............................   38
; knocked..........................   67
; left.............................   66
; let..............................   93
; lisa.............................   20
; lived............................   55
; lost.............................   88
; married..........................   62
; mary.............................   22
; neighbors........................   39
; opened...........................   77
; parents..........................   40
; parking..........................   52
; police...........................   86
; refused..........................   81
; return...........................   34
; saw..............................   76
; scream...........................   85
; screamed.........................   82
; she..............................   12
; since............................   89
; stay.............................   43
; store............................   44
; sue..............................   19
; that.............................   91
; the..............................   36
; themselves.......................   11
; they.............................   13
; threatened.......................   73
; threw............................   78
; to...............................   48
; told.............................   92
; took.............................   31
; townville........................   45
; tried............................   75
; until............................   65
; waited...........................   64
; wanted...........................   71
; was..............................   25
; went.............................   30
; were.............................   26
; will.............................   42
; with.............................   60
; work.............................   58
;
;                                    Common Lisp Indexer, v1.1, by Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

(def_word *start*
          ((assign *part_of_speech* nil *cd_form* nil *subject* nil
            *predicates* nil *noun* nil *act* nil *tense* nil *co_agent* nil
            *time* nil *temprel* 'at)
           (next_packet
            ((test (equal *part_of_speech* 'np)) (assign *subject* *cd_form*)
             (next_packet
              ((test (equal *part_of_speech* 'vp))
               (assign *concept*
                (append *act* (list (list 'tense *tense*))
                        (list (list 'co_agent *co_agent*))))
               (next_packet
                ((test (equal *part_of_speech* 'pp))
                 (assign *concept* (cons_end *concept* *cd_form*))
                 (next_packet
                  ((test (equal *part_of_speech* 'pp))
                   (assign *concept* (cons_end *concept* *cd_form*))
                   (next_packet
                    ((test (equal *part_of_speech* 'pp))
                     (assign *concept*
                      (cons_end *concept* *cd_form*))))))))))))))

;                                  -----
;                                    2
;                                  -----

(def_word have
          ((assign *part_of_speech* 'aux)
           (next_packet
            ((test
              (cond ((equal *part_of_speech* 'v)
                     (setq *tense* '(perfect (present))))
                    ((equal *part_of_speech* 'aux)
                     (setq *tense* '(perfect (present))))
                    ((equal *part_of_speech* 'np)
                     (setq *act* '(is (actor ?hv_1) (state ?hv_2))
                           *tense* '(simple (present))
                           hv_1 *subject*
                           hv_2 *cd_form*
                           *part_of_speech* 'vp))))))))

;                                  -----
;                                    3
;                                  -----

(def_word has
          ((assign *part_of_speech* 'aux)
           (next_packet
            ((test
              (cond ((equal *part_of_speech* 'v)
                     (setq *tense* '(perfect (present))))
                    ((equal *part_of_speech* 'aux)
                     (setq *tense* '(perfect (present))))
                    ((equal *part_of_speech* 'np)
                     (setq *act* '(is (actor ?hv_1) (state ?hv_2))
                           *tense* '(simple (present))
                           hv_1 *subject*
                           hv_2 *cd_form*
                           *part_of_speech* 'vp))))))))

;                                  -----
;                                    4
;                                  -----

(def_word had
          ((assign *part_of_speech* 'aux)
           (next_packet
            ((test
              (cond ((equal *part_of_speech* 'v)
                     (setq *tense* '(perfect (past))))
                    ((equal *part_of_speech* 'aux)
                     (setq *tense* '(perfect (past))))
                    ((equal *part_of_speech* 'np)
                     (setq *act* '(is (actor ?hv_1) (state ?hv_2))
                           *tense* '(simple (past))
                           hv_1 *subject*
                           hv_2 *cd_form*
                           *part_of_speech* 'vp))))))))

;                                  -----
;                                    5
;                                  -----

(def_word he ((assign *part_of_speech* 'np *cd_form* *discourse_he*)))

;                                  -----
;                                    6
;                                  -----

(def_word himself
          ((test (equal *subject* *discourse_he*))
           (assign *part_of_speech* 'np *cd_form* *discourse_he*)))

;                                  -----
;                                    7
;                                  -----

(def_word herself
          ((test (equal *subject* *discourse_she*))
           (assign *part_of_speech* 'np *cd_form* *discourse_she*)))

;                                  -----
;                                    8
;                                  -----

(def_word her ((assign *part_of_speech* 'np *cd_form* *discourse_she*)))

;                                  -----
;                                    9
;                                  -----

(def_word him
          ((assign *part_of_speech* 'np *cd_form*
            (progn
              (psetq *discourse2_he* *discourse_he*
                     *discourse_he* (or *discourse2_he* *discourse_he*)) 
              *discourse_he*))))

;                                  -----
;                                    10
;                                  -----

(def_word itself
          ((test (equal *subject* *discourse_it*))
           (assign *part_of_speech* 'np *cd_form* *discourse_it*)))

;                                  -----
;                                    11
;                                  -----

(def_word themselves
          ((test (equal *subject* *discourse_they*))
           (assign *part_of_speech* 'np *cd_form* *discourse_they*)))

;                                  -----
;                                    12
;                                  -----

(def_word she ((assign *part_of_speech* 'np *cd_form* *discourse_she*)))

;                                  -----
;                                    13
;                                  -----

(def_word they
          ((assign *part_of_speech* 'np *cd_form* *discourse_they* *co_agent*
            (or *co_agent* *discourse_co_agent*))))

;                                  -----
;                                    14
;                                  -----

(def_word david
          ((assign *cd_form* '(human (male (david))) *part_of_speech* 'np
            *discourse_he* *cd_form*)))

;                                  -----
;                                    15
;                                  -----

(def_word alex
          ((assign *cd_form* '(human (male (alex))) *part_of_speech* 'np
            *discourse_he* *cd_form*)))

;                                  -----
;                                    16
;                                  -----

(def_word john
          ((assign *cd_form* '(human (male (john))) *part_of_speech* 'np
            *discourse_he*
            (progn
              (cond ((and (not (equal *cd_form* *discourse_he*))
                          (not (equal *discourse_he*
                                      '(human (male (unknown))))))
                     (setq *discourse2_he* *discourse_he*))) 
              *cd_form*))))

;                                  -----
;                                    17
;                                  -----

(def_word daniel
          ((assign *cd_form* '(human (male (daniel))) *part_of_speech* 'np
            *discourse_he*
            (progn
              (cond ((and (not (equal *cd_form* *discourse_he*))
                          (not (equal *discourse_he*
                                      '(human (male (unknown))))))
                     (setq *discourse2_he* *discourse_he*))) 
              *cd_form*))))

;                                  -----
;                                    18
;                                  -----

(def_word jack
          ((assign *cd_form* '(human (male (jack))) *part_of_speech* 'np
            *discourse_he*
            (progn
              (cond ((and (not (equal *cd_form* *discourse_he*))
                          (not (equal *discourse_he*
                                      '(human (male (unknown))))))
                     (setq *discourse2_he* *discourse_he*))) 
              *cd_form*))))

;                                  -----
;                                    19
;                                  -----

(def_word sue
          ((assign *cd_form* '(human (female (sue))) *part_of_speech* 'np
            *discourse_she* *cd_form*)))

;                                  -----
;                                    20
;                                  -----

(def_word lisa
          ((assign *cd_form* '(human (female (lisa))) *part_of_speech* 'np
            *discourse_she* *cd_form*)))

;                                  -----
;                                    21
;                                  -----

(def_word ann
          ((assign *cd_form* '(human (female (ann))) *part_of_speech* 'np
            *discourse_she* *cd_form*)))

;                                  -----
;                                    22
;                                  -----

(def_word mary
          ((assign *cd_form* '(human (female (mary))) *part_of_speech* 'np
            *discourse_she* *cd_form*)))

;                                  -----
;                                    23
;                                  -----

(def_word got
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *act* '(atrans (actor ?get_var1) (object ?get_var2) (to ?get_var3))
            get_var1 *subject* get_var2 nil get_var3 *subject*)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign get_var2 *cd_form* *part_of_speech* 'vp)
             (next_packet
              ((test
                (cond ((equal *part_of_speech* 'np)
                       (setq get_var3 get_var2
                             get_var2 *cd_form*))
                      (t (setq *part_of_speech* 'vp))))))))))

;                                  -----
;                                    24
;                                  -----

(def_word get
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (present)))
            *act* '(atrans (actor ?get_var1) (object ?get_var2) (to ?get_var3))
            get_var1 *subject* get_var2 nil get_var3 *subject*)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign get_var2 *cd_form* *part_of_speech* 'vp)
             (next_packet
              ((test
                (cond ((equal *part_of_speech* 'np)
                       (setq get_var3 get_var2
                             get_var2 *cd_form*))
                      (t (setq *part_of_speech* 'vp))))))))))

;                                  -----
;                                    25
;                                  -----

(def_word was
          ((assign *tense* '(simple (past)) *part_of_speech* 'vp *act*
            '(is (actor ?was_var) (object ?was_var) (state ?was_var2)) was_var
            *subject* was_var2 nil)
           (next_packet
            ((test
              (cond ((equal *part_of_speech* 'adj)
                     (setq *part_of_speech* 'vp
                           was_var2 *cd_form*))
                    (t (setq *part_of_speech* 'vp))))))))

;                                  -----
;                                    26
;                                  -----

(def_word were
          ((assign *tense* '(simple (past)) *part_of_speech* 'vp *act*
            '(is (actor ?was_var) (object ?was_var) (state ?was_var2)) was_var
            *subject* was_var2 nil)
           (next_packet
            ((test
              (cond ((equal *part_of_speech* 'adj)
                     (setq *part_of_speech* 'vp
                           was_var2 *cd_form*))
                    (t (setq *part_of_speech* 'vp))))))))

;                                  -----
;                                    27
;                                  -----

(def_word is
          ((assign *tense* '(simple (present)) *part_of_speech* 'vp *act*
            '(is (actor ?was_var) (object ?was_var) (state ?was_var2)) was_var
            *subject* was_var2 nil)
           (next_packet
            ((test
              (cond ((equal *part_of_speech* 'adj)
                     (setq *part_of_speech* 'vp
                           was_var2 *cd_form*))
                    (t (setq *part_of_speech* 'vp))))))))

;                                  -----
;                                    28
;                                  -----

(def_word are
          ((assign *tense* '(simple (present)) *part_of_speech* 'vp *act*
            '(is (actor ?was_var) (state ?was_var2)) was_var *subject* was_var2
            nil)
           (next_packet
            ((test
              (cond ((equal *part_of_speech* 'adj)
                     (setq *part_of_speech* 'vp
                           was_var2 *cd_form*))
                    (t (setq *part_of_speech* 'vp))))))))

;                                  -----
;                                    29
;                                  -----

(def_word been
          ((assign *part_of_speech* 'aux *act*
            '(is (actor ?was_var) (object ?was_var) (state ?was_var2)) was_var
            *subject* was_var2 nil)
           (next_packet
            ((test (equal *part_of_speech* 'adj))
             (assign *part_of_speech* 'vp was_var2 *cd_form*)))))

;                                  -----
;                                    30
;                                  -----

(def_word went
          ((assign *part_of_speech* 'vp *tense* (or *tense* '(simple (past)))
            *act* '(ptrans (actor ?go_var1) (cause ?go_var1) (to ?go_var2))
            go_var1 *subject*)
           (next_packet
            ((test (equal *word* 'home))
             (assign go_var2 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    31
;                                  -----

(def_word took
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *act* '(grasp (actor ?go_var1) (object ?go_var2)) go_var1
            *subject*)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign go_var2 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    32
;                                  -----

(def_word came
          ((assign *part_of_speech* 'vp *tense* (or *tense* '(simple (past)))
            *act* '(ptrans (actor ?c_var1) (cause ?c_var1)) c_var1 *subject*)))

;                                  -----
;                                    33
;                                  -----

(def_word go
          ((assign *part_of_speech* 'vp *tense*
            (or *tense* '(simple (present))) *act*
            '(ptrans (actor ?go_var1) (cause ?go_var1) (to ?go_var2)) go_var1
            *subject* go_var2 nil)
           (next_packet
            ((test (equal *word* 'home))
             (assign go_var2 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    34
;                                  -----

(def_word return
          ((assign *part_of_speech* 'vp *tense*
            (or *tense* '(simple (present))) *act*
            '(ptrans (actor ?go_var1) (object ?go_var1)) go_var1 *subject*)))

;                                  -----
;                                    35
;                                  -----

(def_word a
          ((assign *part_of_speech* 'det)
           (next_packet
            ((test (equal *part_of_speech* 'noun))
             (assign *part_of_speech* 'np *noun* *cd_form*)))))

;                                  -----
;                                    36
;                                  -----

(def_word the
          ((assign *part_of_speech* 'det)
           (next_packet
            ((test (equal *part_of_speech* 'noun))
             (assign *part_of_speech* 'np *noun* *cd_form*)))))

;                                  -----
;                                    37
;                                  -----

(def_word his
          ((assign *part_of_speech* 'det)
           (next_packet
            ((test (equal *part_of_speech* 'noun))
             (assign *part_of_speech* 'np *noun* *cd_form* *predicates*
              '(poss))))))

;                                  -----
;                                    38
;                                  -----

(def_word kite
          ((assign *part_of_speech* 'noun *cd_form*
            '(artifact (entertain (kite))) *discourse_it* *cd_form*)))

;                                  -----
;                                    39
;                                  -----

(def_word neighbors
          ((assign *part_of_speech* 'noun *cd_form* '(group (name (neighbors)))
            *discourse_they* *cd_form* *discourse_co_agent* nil)))

;                                  -----
;                                    40
;                                  -----

(def_word parents
          ((assign *part_of_speech* 'noun *cd_form* '(group (family (parents)))
            *discourse_they* *cd_form* *discourse_co_agent* nil)))

;                                  -----
;                                    41
;                                  -----

(def_word children
          ((assign *part_of_speech* 'noun *cd_form* '(group (family (child)))
            *discourse_they* *cd_form* *discourse_co_agent* nil)))

;                                  -----
;                                    42
;                                  -----

(def_word will ((assign *tense* '(simple (future)))))

;                                  -----
;                                    43
;                                  -----

(def_word stay
          ((assign *part_of_speech* 'vp *act*
            '(ptrans (actor ?st_1) (state (action (val (-10))))) st_1
            *subject*)
           (next_packet
            ((test (equal *part_of_speech* 'pp))
             (assign *act*
              (cons_end *act* (cons_l (np_type *cd_form*) *cd_form*)))))))

;                                  -----
;                                    44
;                                  -----

(def_word store
          ((assign *part_of_speech* 'noun *cd_form*
            '(artifact (shopping (store))) *discourse_it* *cd_form*)))

;                                  -----
;                                    45
;                                  -----

(def_word townville
          ((assign *part_of_speech* 'np *cd_form*
            '(artifact (dwelling (townville))))))

;                                  -----
;                                    46
;                                  -----

(def_word august-1979
          ((assign *part_of_speech* 'np *cd_form*
            '(time (date (august-1979))))))

;                                  -----
;                                    47
;                                  -----

(def_word car
          ((assign *part_of_speech* 'noun *cd_form*
            '(artifact (transportation (car))) *discourse_it* *cd_form*)))

;                                  -----
;                                    48
;                                  -----

(def_word to
          ((assign *part_of_speech* 'prep)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *cd_form* (cons_l 'to *cd_form*) *part_of_speech* 'pp)))))

;                                  -----
;                                    49
;                                  -----

(def_word into
          ((assign *part_of_speech* 'prep)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *cd_form* (cons_l 'to *cd_form*) *part_of_speech* 'pp)))))

;                                  -----
;                                    50
;                                  -----

(def_word home
          ((assign *cd_form* '(artifact (dwelling (house))) *part_of_speech*
            'np)))

;                                  -----
;                                    51
;                                  -----

(def_word house
          ((assign *cd_form* '(artifact (dwelling (house))) *part_of_speech*
            'noun)))

;                                  -----
;                                    52
;                                  -----

(def_word parking
          ((next_packet
            ((test (member *word* '(lot space structure)))
             (assign *noun* '(artifact (storage (parking))) *part_of_speech*
              'np)))))

;                                  -----
;                                    53
;                                  -----

(def_word it ((assign *part_of_speech* 'np *cd_form* *discourse_it*)))

;                                  -----
;                                    54
;                                  -----

(def_word and
          ((test (equal *part_of_speech* 'np))
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *co_agent* *cd_form* *discourse_they* *subject*
              *discourse_co_agent* *co_agent*)))))

;                                  -----
;                                    55
;                                  -----

(def_word lived
          ((assign *part_of_speech* 'vp *tense* (or *tense* '(simple (past)))
            *act* '(is (actor ?live_1)) live_1 *subject*)))

;                                  -----
;                                    56
;                                  -----

(def_word in
          ((assign *part_of_speech* 'prep)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *cd_form*
              (cond ((equal (np_type *cd_form*) 'time)
                     (setq *time* *cd_form*
                           *cd_form* nil))
                    (t (cons_l (np_type *cd_form*) *cd_form*)))
              *part_of_speech* 'pp)))))

;                                  -----
;                                    57
;                                  -----

(def_word after
          ((assign *part_of_speech* 'prep *temprel* 'after)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *cd_form*
              (cond ((equal (np_type *cd_form*) 'time)
                     (setq *time* *cd_form*
                           *cd_form* nil))
                    (t (cons_l (np_type *cd_form*) *cd_form*)))
              *part_of_speech* 'pp)))))

;                                  -----
;                                    58
;                                  -----

(def_word work
          ((assign *part_of_speech* 'np *cd_form* '(action (earning (work))))))

;                                  -----
;                                    59
;                                  -----

(def_word at
          ((assign *part_of_speech* 'prep)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *cd_form* (cons_l (np_type *cd_form*) *cd_form*)
              *part_of_speech* 'pp)))))

;                                  -----
;                                    60
;                                  -----

(def_word with
          ((assign *part_of_speech* 'prep)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *part_of_speech* 'pp)))))

;                                  -----
;                                    61
;                                  -----

(def_word arrested
          ((assign *part_of_speech* 'v *act*
            '(ptrans (actor ?ar_1) (object ?ar_2)
                     (to (artifact (law_enforcement (jail)))))
            ar_1 *subject*)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign ar_2 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    62
;                                  -----

(def_word married
          ((test (equal *part_of_speech* 'np))
           (assign *act*
            '(is (actor ?mar_1) (state (marital (val (10)))) (object ?mar_2)
              (to ?mar_2))
            mar_1 *subject* *tense* (or *tense* '(simple (past))))
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign mar_2 *cd_form* *part_of_speech* 'vp))))
          ((test (equal *part_of_speech* 'aux))
           (assign *part_of_speech* 'adj *cd_form* '(marital (val (10))))))

;                                  -----
;                                    63
;                                  -----

(def_word divorced
          ((test (equal *part_of_speech* 'np))
           (assign *act*
            '(is (actor ?mar_1) (state (marital (val (-10)))) (object ?mar_2)
              (to ?mar_2))
            mar_1 *subject* *tense* (or *tense* '(simple (past))))
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign mar_2 *cd_form* *part_of_speech* 'vp))))
          ((test (equal *part_of_speech* 'aux))
           (assign *part_of_speech* 'adj *cd_form* '(marital (val (-10))))))

;                                  -----
;                                    64
;                                  -----

(def_word waited
          ((assign *part_of_speech* 'vp *tense* (or *tense* '(simple (past)))
            *act* '(attend (actor ?wait_1) (object ?wait_1)) wait_1 *subject*)))

;                                  -----
;                                    65
;                                  -----

(def_word until
          ((assign *part_of_speech* 'prep)
           (next_packet
            ((test (equal *part_of_speech* 'np)) (assign *subject* *cd_form*)
             (next_packet
              ((test (equal *part_of_speech* 'vp))
               (assign *time* *act* *temprel* 'before)))))))

;                                  -----
;                                    66
;                                  -----

(def_word left
          ((assign *part_of_speech* 'vp *tense* (or *tense* '(simple (past)))
            *act* '(ptrans (actor ?lv_1)) lv_1 *subject*)))

;                                  -----
;                                    67
;                                  -----

(def_word knocked
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *act* '(propel (actor ?bk_1) (cause ?bk_1) (object ?bk_2)) bk_1
            *subject*)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *part_of_speech* 'vp bk_2 *cd_form*)))))

;                                  -----
;                                    68
;                                  -----

(def_word drove
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *act*
            '(ptrans (actor ?bk_1) (instrument ?bk_2) (cause ?bk_1)
                     (object ?bk_3))
            bk_1 *subject* bk_2 nil bk_3 nil)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *part_of_speech* 'vp bk_3 *cd_form*))
            ((test (equal *part_of_speech* 'pp))
             (assign bk_2 '(artifact (transportation (car))) *part_of_speech*
              'vp *act* (cons_end *act* *cd_form*))))))

;                                  -----
;                                    69
;                                  -----

(def_word away
          ((assign *part_of_speech* 'pp *cd_form* '(to (state (val (-10)))))))

;                                  -----
;                                    70
;                                  -----

(def_word door
          ((assign *part_of_speech* 'noun *cd_form*
            '(artifact (opening (door))) *discourse_it* *cd_form*)))

;                                  -----
;                                    71
;                                  -----

(def_word wanted
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *concept* '(plan (name ?gw_1) (planner ?gw_2) (objective ?gw_3))
            gw_2 *subject* gw_3 nil *word* (look_ahead) *word*
            (and (equal *word* 'to) (look_ahead)) gw_1 *word* *word*
            (look_ahead) *word*
            (cond ((member *word* '(if whether)) *word*)
                  (t (push *word* *sentence*) *word*)))
           (next_packet
            ((test (equal *part_of_speech* 'np)) (assign *subject* *cd_form*)
             (next_packet
              ((test (equal *part_of_speech* 'vp))
               (assign gw_3 *act* *act* *concept*)
               (next_packet
                ((test (equal *part_of_speech* 'pp))
                 (assign gw_3 (cons_end gw_3 *cd_form*) *part_of_speech*
                  'vp)))))))))

;                                  -----
;                                    72
;                                  -----

(def_word escape
          ((assign *part_of_speech* 'np *cd_form*
            '(action (be_free (escape))))))

;                                  -----
;                                    73
;                                  -----

(def_word threatened
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *predicates*
            '(plan (name threat) (planner ?gw_2) (object ?gw_1) (state ?gw_4)
                   (objective ?gw_3))
            gw_2 *subject* gw_1 nil gw_4 nil gw_3 nil *word* (look_ahead))
           (next_packet
            ((test (equal *part_of_speech* 'vp))
             (assign gw_3 *act* gw_1 *cd_form*)
             (next_packet
              ((assign *word*
                (cond ((member *word* '(if whether)) *word*)
                      (t (push *word* *sentence*) *word*)))
               (next_packet
                ((test (equal *part_of_speech* 'np))
                 (assign *subject* *cd_form*)
                 (next_packet
                  ((test (equal *part_of_speech* 'vp)) (assign gw_4 *act*)
                   (next_packet
                    ((test (equal *part_of_speech* 'pp))
                     (assign gw_4 (cons_end gw_4 *cd_form*) *act* *predicates*
                      *concept* *act* *part_of_speech* 'vp)))))))))))))

;                                  -----
;                                    74
;                                  -----

(def_word kill
          ((assign *part_of_speech* 'v *act*
            '(propel (object ?o_1) (state (health (val (-10))))) o_1 nil)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign o_1 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    75
;                                  -----

(def_word tried
          ((assign *part_of_speech* 'vp *act*
            '(do (actor 
                  ?x))
            x *subject*)))

;                                  -----
;                                    76
;                                  -----

(def_word saw
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *act* '(attend (actor ?see_1) (object ?see_1) (to ?see_2)) see_1
            *subject* see_2 nil see_3 nil)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign see_2 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    77
;                                  -----

(def_word opened
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *act*
            '(propel (actor ?op_1) (cause ?op_1) (object ?op_2)
                     (state (object (open))))
            op_1 *subject* op_2 nil)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign op_2 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    78
;                                  -----

(def_word threw
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *act* '(propel (actor ?th_1) (cause ?th_1) (object ?th_2)) th_1
            *subject* th_2 nil)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign th_2 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    79
;                                  -----

(def_word forced
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *act* '(propel (actor ?th_1) (cause ?th_1) (object ?th_2)) th_1
            *subject* th_2 nil)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign th_2 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    80
;                                  -----

(def_word kicked
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *act*
            '(propel (actor ?th_1) (cause ?th_1) (object ?th_2)
                     (state (intensity (val (8)))))
            th_1 *subject* th_2 nil)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign th_2 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    81
;                                  -----

(def_word refused
          ((assign *part_of_speech* 'v *act*
            '(mtrans (actor ?ref_1) (object ?ref_2) (state (mood (val (-10)))))
            ref_1 *subject* ref_2 nil)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign ref_2 *cd_form* *part_of_speech* 'vp)))))

;                                  -----
;                                    82
;                                  -----

(def_word screamed
          ((assign *part_of_speech* 'vp *tense* (or *tense* '(simple (past)))
            *act* '(speak (actor ?sc_1) (state (intensity (val (10))))) sc_1
            *subject*)))

;                                  -----
;                                    83
;                                  -----

(def_word called
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (past)))
            *act* '(attend (from ?hr_1) (actor ?hr_2)) hr_1 *subject* hr_2 nil)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *part_of_speech* 'vp hr_2 *cd_form*)))))

;                                  -----
;                                    84
;                                  -----

(def_word call
          ((assign *part_of_speech* 'v *tense* (or *tense* '(simple (present)))
            *act* '(attend (from ?hr_1) (actor ?hr_2)) hr_1 *subject* hr_2 nil)
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign *part_of_speech* 'vp hr_2 *cd_form*)))))

;                                  -----
;                                    85
;                                  -----

(def_word scream
          ((assign *part_of_speech* 'noun *cd_form*
            '(speak (state (intensity (val (10))))))))

;                                  -----
;                                    86
;                                  -----

(def_word police
          ((assign *part_of_speech* 'noun *cd_form* '(group (law (police))))))

;                                  -----
;                                    87
;                                  -----

(def_word ambulance
          ((assign *part_of_speech* 'noun *cd_form*
            '(group (health (medicine))))))

;                                  -----
;                                    88
;                                  -----

(def_word lost
          ((assign *part_of_speech* 'adj *cd_form* '(location (val (-10))))))

;                                  -----
;                                    89
;                                  -----

(def_word since
          ((assign *part_of_speech* 'prep)
           (next_packet
            ((test
              (cond ((null *sentence*)
                     (setq *part_of_speech* 'pp
                           *cd_form* nil))))))))

;                                  -----
;                                    90
;                                  -----

(def_word before
          ((assign *part_of_speech* 'prep)
           (next_packet
            ((test
              (cond ((null *sentence*)
                     (setq *part_of_speech* 'pp
                           *cd_form* nil))))))))

;                                  -----
;                                    91
;                                  -----

(def_word that
          ((assign *part_of_speech* 'comp *cd_form* (set_discourse *subject*))))

;                                  -----
;                                    92
;                                  -----

(def_word told
          ((assign *part_of_speech* 'v *predicates*
            '(mtrans (actor ?tl_1) (object ?tl_2) (to ?tl_3)) tl_1 *subject*
            tl_2 nil tl_3 nil *tense* (or *tense* '(simple (past))))
           (next_packet
            ((test (equal *part_of_speech* 'np)) (assign tl_3 *cd_form*)
             (next_packet
              ((test (equal *part_of_speech* 'comp))
               (next_packet
                ((test (equal *part_of_speech* 'np))
                 (assign *subject* *cd_form*)
                 (next_packet
                  ((test (equal *part_of_speech* 'vp))
                   (assign tl_2 *act* *act* *predicates*)))))))))))

;                                  -----
;                                    93
;                                  -----

(def_word let
          ((assign *part_of_speech* 'v *predicates*
            '(plan (name allow) (planner ?tl_1) (object ?tl_2)
                   (objective ?tl_3))
            tl_1 *subject* tl_2 nil tl_3 nil *tense*
            (or *tense* '(simple (past))))
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign tl_2 *cd_form* *subject* *cd_form*)
             (next_packet
              ((test (equal *part_of_speech* 'vp))
               (assign tl_3 *act* *act* *predicates*)))))))

;                                  -----
;                                    94
;                                  -----

(def_word heard
          ((assign *part_of_speech* 'v *act*
            '(attend (actor ?hr_1) (object ?hr_2)) hr_1 *subject* hr_2 nil
            *tense* (or *tense* '(simple (past))))
           (next_packet
            ((test (equal *part_of_speech* 'np))
             (assign hr_2 *cd_form* *part_of_speech* 'vp)))))
