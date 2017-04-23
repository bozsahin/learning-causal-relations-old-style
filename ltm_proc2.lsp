
; File name               :    ltm_proc2.lsp
;
; Created at              :    5/27/1990 18:02:03 by CL(NX) Indexer
;
; Indexed (source) file   :    ltm_proc2.lsp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; agency...........................    7
; constant-conjunction.............    4
; goal-directed-causality..........    5
; irreflexivity....................    1
; plan-based-causality.............    6
; spatial_causality................    3
; temporal-precedence..............    2
;
;                                    Common Lisp Indexer, v1.1, by Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

(define_heuristic "irreflexivity" '(equal (header_cd x) (header_cd y))
                  '(and (basic_cd x) (basic_cd y))
                  '(and (equal (filler_role 'actor x) (filler_role 'actor y))
                        (equal (filler_role 'object x) (filler_role 'object y))
                        (equal (filler_role 'to x) (filler_role 'to y))
                        (equal (filler_role 'from x) (filler_role 'from y))
                        (assert_c (negative_cause x y))))

;                                  -----
;                                    2
;                                  -----

(define_heuristic "temporal-precedence" '(and (basic_cd x) (basic_cd y))
                  '(and (filler_role 'time x) (filler_role 'time y))
                  '(and (earlier (cadr (filler_role 'time y))
                                 (cadr (filler_role 'time x))
                                 (scratch_pad-temporal_relations *working_memory*))
                        (assert_c (negative_cause x y))))

;                                  -----
;                                    3
;                                  -----

(define_heuristic "spatial_causality"
                  '(and (basic_cd x)
                        (basic_cd y)
                        (equal (cd_event_type x) (cd_event_type y)))
                  '(and (filler_role 'location x) (filler_role 'location y))
                  '(and (not_accessible (filler_role 'location y)
                                        (filler_role 'location x))
                        (assert_c (negative_cause x y))))

;                                  -----
;                                    4
;                                  -----

(define_heuristic "constant-conjunction"
                  '(and *e*
                        (basic_cd x)
                        (basic_cd y)
                        (not (equal (header_cd x) 'is)))
                  '(and *current_hypo*
                        (earlier (cadr (filler_role 'time x))
                                 (cadr (filler_role 'time y))
                                 (scratch_pad-temporal_relations *working_memory*)))
                  '(and (constant_conjunct x y *e*)
                        (assert_c (positive_cause x y 'similarity))
                        (assert_c (negative_cause y x))))

;                                  -----
;                                    5
;                                  -----

(define_heuristic "goal-directed-causality" '(and (goal_cd x) (plan_cd y))
                  '(and (filler_role 'name x)
                        (filler_role 'name y)
                        (common_roles_p x y))
                  '(let ((means (leads_to (filler_role 'name y)
                                          (filler_role 'name x))))
                     (and means
                          (assert_c (positive_cause x y means))
                          (assert_c (negative_cause y x)))))

;                                  -----
;                                    6
;                                  -----

(define_heuristic "plan-based-causality" '(and (plan_cd x) (basic_cd y))
                  '(and (subsume '(human) (filler_role 'actor y))
                        (filler_role 'name x)
                        (common_roles_p x y))
                  '(let ((relation (has_plan (filler_role 'name x) y)))
                     (cond ((equal relation 'activate)
                            (assert_c (positive_cause y x relation))
                            (assert_c (negative_cause x y)))
                           ((equal relation 'consequence)
                            (assert_c (positive_cause x y relation))
                            (assert_c (negative_cause y x))))))

;                                  -----
;                                    7
;                                  -----

(defun agency (event)
  "This function finds out if agent 'cause slot is filled. If so, that
   slot is carried over to causal node and removed from event (effect)." 
  (let ((cause_slot (assoc 'cause event)))
    (and (cadr cause_slot)
         (assert_c (positive_cause (list 'do
                                         (list 'actor (cadr cause_slot)))
                                   (remove
                                            cause_slot
                                            event
                                            :test
                                            #'equal)
                                   'agency))
         t)))

(define_heuristic "domain 1"
                  '(and (requal (filler_role 'object x)
                                (filler_role 'actor y))
                        (or
                            (equal (filler_role 'state y)
                                   '(location(val(-10))))
                            (equal (filler_role 'location y)
                                   '(state(val(-10))))))
                  '(and (equal (cd_event_type x) 'theme)
                        (equal (header_cd y) 'is)
                        (filler_role 'time y)
                        (equal (filler_role 'name x) 'abduct))
                  '(and (nothing_after (filler_role 'time y)
                                       (scratch_pad-temporal_relations
                                           *working_memory*))
                        (assert_c (positive_cause x y 'domain))
                        (assert_c (negative_cause y x))))

(define_heuristic "domain 2"
                  '(and (requal (filler_role 'actor x)
                                (filler_role 'planner y))
                        (requal (filler_role 'actor
                                             (filler_role 'object x))
                                (filler_role 'object y)))
                  '(and (equal (header_cd x) 'mtrans)
                        (equal (cd_event_type y) 'theme)
                        (equal (filler_role 'name y) 'abduct)
                        (cd_event_type (filler_role 'object x)))
                  '(assert_c
                       (positive_cause
                           (make_cd 'is
                                    (list 'actor (filler_role 'actor x))
                                    (list 'state
                                          (list 'belief
                                                (filler_role 'object x))))
                           y
                           'domain)))

(define_heuristic "domain 3"
                  '(and (requal (filler_role 'planner x)
                               (filler_role 'planner y))
                       (requal (filler_role 'object x)
                               (filler_role 'object y)))
                  '(and (equal (cd_event_type x) 'theme)
                        (equal (cd_event_type y) 'theme)
                        (equal (filler_role 'name x) 'kidnap_for_ransom)
                        (equal (filler_role 'name y) 'abduct))
                  '(and (not (ask_money (filler_role 'planner x)
                                        (scratch_pad-event_list 
                                            *working_memory*)))
                        (assert_c (negative_cause x y))))

(defun ask_money (actor cd_list)
  "This function is used by one of domain heuristics. It returns t if the
   'actor asked for money' cd is in the cd_list"
  (find-if #'(lambda(ev)
               (and
                   (equal (filler_role 'name ev) 'ask)
                   (requal actor (filler_role 'actor ev))
                   (equal (filler_role 'object ev)
                          '(artifact(valuable(money))))))
           cd_list))

(defun nothing_after (time_slot temporal_relations)
  "This function is used by one of domain heuristics. It finds out if the
   time value in time-slot succeeds all values in the temporal_relations
   list. That list is just like a before list in episode structure. Returns
   t if the test is true, nil otherwise."
  (not (find (filler_pair time_slot)
             (cdr temporal_relations)
             :test #'equal
             :key #'car)))
