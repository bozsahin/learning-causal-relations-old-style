; File name               :    ltm_decl.lsp
;
; Created at              :    4/22/1990 14:59:12 by CL(NX) Indexer
;
; Indexed (source) file   :    ltm_decl.lsp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; not_accessible...................   46
;
;                                    Common Lisp Indexer, v1.1, by Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

nil

;                                  -----
;                                    2
;                                  -----

(setq *base_plans* nil
      *base_goals* nil
      *sub_plans* nil
      *sub_goals* nil)

;                                  -----
;                                    3
;                                  -----

(setf (get 'group 'is_a) 'human
      (get 'human 'is_a) 'animate
      (get 'animate 'is_a) 'root
      (get 'artifact 'is_a) 'inanimate
      (get 'plant 'is_a) 'inanimate
      (get 'physobj 'is_a) 'inanimate
      (get 'animal 'is_a) 'animate
      (get 'inanimate 'is_a) 'root)

;                                  -----
;                                    4
;                                  -----

(base_plan '(((plan (name go_in_group) (planner ?x) (object ?z)
                    (location ?y)))
             ((ptrans (actor ?x) (to ?y) (co_agent ?z)) (and ?z ?y ?x))))

;                                  -----
;                                    5
;                                  -----
(subplan '(((plan (name accomplice)(planner ?x) (object ?y)))
           ((plan (name go_in_group) (planner ?x) (object ?y))
            (and ?x ?y))
           ((plan (name ask_help) (planner ?x) (object ?y))
            (and ?x ?y))))

(base_plan '(((plan (name go_in_group) (planner ?x)
                   (location ?y)))
             ((ptrans (actor ?x)
                      (object ?z) (to ?y))
              (and ?x ?y ?z 
                   (equal ?x ?z)
                   (equal (car ?x) 'group)))))

;                                  -----
;                                    6
;                                  -----

(base_plan '(((plan (name divorced) (planner ?x) (object ?y)))
             ((is (actor ?x) (to ?y) (tense ?te) (state (marital (val ?n))))
              (and (ended ?te) ?x ?y (pos_val ?n)))))
(base_plan '(((plan (name divorced) (planner ?x) (object ?y)))
             ((is (actor ?x) (to ?y) (tense ?te) (state (marital (val ?n))))
              (and ?x ?y (neg_val ?n)))))

;                                  -----
;                                    7
;                                  -----

(base_plan '(((plan (name let_know) (planner ?x) (to ?y) (object ?z)))
             ((mtrans (actor ?x) (to ?y)
                      (objective ?act) (object ?z))
              (and ?x ?y ?z ?act
                   (member (header_cd ?act)
                          '(plan goal attend mtrans mbuild))))))

(base_plan '(((plan (name let_know) (planner ?x) (to ?y) (object ?z)))
             ((mtrans (actor ?x) (to ?y)
                      (object ?z))
              (and ?x ?y ?z 
                   (member (header_cd ?z)
                  '(plan goal attend mtrans mbuild))))))

;                                  -----
;                                    8
;                                  -----

(base_plan '(((plan (name attention) (planner ?x)
                    (objective (speak (state (intensity (val ?n)))))))
             ((speak (actor ?x) (state (intensity (val ?n))))
              (and ?x (pos_val ?n)))))

;                                  -----
;                                    9
;                                  -----

(base_plan '(((plan (name surprised) (planner ?x)
                    (object (speak (state (intensity (val ?n)))))))
             ((speak (actor ?x) (state (intensity (val ?n))))
              (and ?x (pos_val ?n)))))

;                                  -----
;                                    10
;                                  -----

(base_plan '(((plan (name call_help) (planner ?x) (object ?y)))
             ((attend (actor ?y) (from ?x))
              (and ?x ?y (equal (intended_function ?y) 'law)))))

;                                  -----
;                                    11
;                                  -----

(base_plan '(((plan (name do_after_event) (planner ?x) (object ?y)))
             ((attend (actor ?x) (object ?x) (time ?y))
              (and ?x
                   (equal (car ?y) 'before)
                   (member (cd_event_type (cadr ?y))
                           '(mental physical theme))))))

;                                  -----
;                                    12
;                                  -----

(base_plan '(((plan (name know_event) (planner ?x) (objective ?y)))
             ((attend (actor ?x) (object ?y))
              (and ?x
                   ?y
                   (not (equal ?x ?y))
                   (member (cd_event_type ?y) '(mental physical theme))))))

(subplan '(((plan (name know_event) (planner ?x) 
                  (objective (attend (from ?y)(actor ?x)))))
           ((plan (name allow) (planner ?z)
                  (object ?y)
                  (objective (attend (from ?y)
                                     (actor ?x))))
            (and ?x ?y ?z))))

                                       
;                                  -----
;                                    13
;                                  -----

(base_plan '(((plan (name force_into) (planner ?x) (object ?y) (to ?z)))
             ((propel (actor ?x) (object ?y) (to ?z))
              (and ?z
                   (member (car ?x) '(group human))
                   (member (car ?y) '(group human animate))
                   (not (equal ?x ?y))))))

;                                  -----
;                                    14
;                                  -----

(base_plan '(((plan (name see_otherside) (planner ?x)
                    (object (artifact (dwelling (house))))))
             ((propel (actor ?x) (object ?z) (state ?s))
              (and (equal (intended_function ?s) 'open)
                   (member (car ?x) '(human animate group))
                   (equal (intended_function ?z) 'opening)))))

;                                  -----
;                                    15
;                                  -----

(base_plan '(((plan (name look_in) (planner ?x)
                    (object (artifact (dwelling (house))))))
             ((propel (actor ?x) (object ?z) (state ?s))
              (and (null ?s)
                   (member (car ?x) '(human animate group))
                   (equal (intended_function ?z) 'opening)))))

;                                  -----
;                                    16
;                                  -----

(subgoal '(((plan (name check_house) (planner ?z) (objective (prox (actor ?x)))
             (object ?y)))
           ((plan (name look_in) (planner ?z) (object ?y)))
           ((plan (name see_otherside) (planner ?x) (object ?y)))))

;                                  -----
;                                    17
;                                  -----

(base_plan '(((plan (name provide_help) (planner ?z)
                    (objective (know (actor ?x)))))
             ((is (actor ?x) (state (location (val ?n)))) (neg_val ?n))))

;                                  -----
;                                    18
;                                  -----

(base_plan '(((plan (name take_plan) (planner ?x) (object ?y)))
             ((grasp (actor ?x) (object ?y)))))

;                                  -----
;                                    19
;                                  -----

(subplan '(((plan (name snatch) (planner ?x) (object ?y) (instrument ?z)))
           ((plan (name force_into) (planner ?x) (object ?y) (to ?z))
            (and ?x ?y ?z))))

;                                  -----
;                                    20
;                                  -----

(subplan '(((plan (name drive_away) (planner ?x) (object ?z))
            (and ?x  ?z (equal (intended_function ?z) 'transportation)))
           ((plan (name force_into) (planner ?x) (object ?y))
            (and ?x ?y))))

;                                  -----
;                                    21
;                                  -----

(base_plan '(((plan (name drive_away) (planner ?x) (object ?y)
                    (to ?z)))
             ((ptrans (actor ?x) (object ?y) (to ?z))
              (and ?z 
                   ?y ?x
                   (equal (intended_function ?y) 'transportation)))))

;                                  -----
;                                    22
;                                  -----
(base_plan '(((plan (name find_someone) (planner ?x) (object ?y)))
             ((attend (actor ?x)  (to ?y))
              (and ?x ?y
                   (member (car ?x) '(human group animate))
                   (member (car ?y) '(human group animate))))))

(subgoal  '(((plan (name snatch) (planner ?y) (object ?z)))
            ((ptrans (actor ?x) (object ?y) (to ?w))
             (and (equal (intended_function ?x) 'law)
                  (equal (intended_function ?w) 'law_enforcement)))))
           
            
         
;(subgoal '(((plan (name find_opport) (planner ?x) (object ?z)))
;           ((plan (name allow) (planner ?z) (object ?x))
;            (and ?x ?z))))
                                                      
(subgoal '(((plan (name find_someone) (planner ?x) (object ?y)))
           ((plan (name check_house) (planner ?x) (object ?z)
                  (objective (prox (actor ?y))))
            (and ?x
                 ?y
                 (not (equal ?x ?y))
                 (member (car ?x) '(group human))
                 (member (car ?y) '(group human))))))

;                                  -----
;                                    23
;                                  -----

(subplan '(((plan (name where_is) (planner ?x) (object ?y)))
           ((plan (name see_otherside) (planner ?x)
                  (objective (prox (actor ?y)))))))

;                                  -----
;                                    24
;                                  -----

(subplan '(((plan (name wait_for_event) (planner ?x) (act ?y)))
           ((plan (name do_after_event) (planner ?x) (object ?y))
            (and ?x ?y (not (equal ?x ?y))))))

;                                  -----
;                                    25
;                                  -----

(subplan '(((plan (name find_opport) (planner ?x) (act ?y)))
           ((plan (name wait_for_event) (planner ?x) (act ?y))
            (and ?x ?y (not (equal ?x ?y))))))

;                                  -----
;                                    26
;                                  -----

(subgoal '(((plan (name find_opport) (planner ?x) (object ?y)))
           ((plan (name find_someone) (planner ?x) (object ?y))
            (not (equal ?x ?y)))))

;                                  -----
;                                    27
;                                  -----

(subgoal '(((plan (name find_opport) (planner ?x) (co_planner ?z) (object ?y)))
           ((plan (name find_someone) (planner ?z) (object ?y))
            (and ?z ?y (not (equal ?z ?y))))
           ((plan (name accomplice) (planner ?x) (object ?z))
            (and ?x ?z (not (equal ?x ?z))))))

;                                  -----
;                                    28
;                                  -----

(base_plan '(((plan (name is_at) (planner ?x) (object ?x) (location ?z)))
             ((is (actor ?x) (location ?z)) (and ?x ?z))))

;                                  -----
;                                    29
;                                  -----

(base_plan '(((plan (name is_at) (planner ?x) (object ?x) (location ?z)))
             ((is (co_agent ?x) (location ?z)) (and ?x ?z))))

;                                  -----
;                                    30
;                                  -----

(subplan '(((plan (name go_where_is) (planner ?x) (object ?y) (location ?z)))
           ((plan (name is_at) (planner ?y) (location ?z)) (and ?y ?z))))

;                                  -----
;                                    31
;                                  -----

(subgoal '(((goal (name abduct) (planner ?x) (object ?y)))
           ((plan (name find_opport) (planner ?x) (object ?y))
            (not (equal ?x ?y)))
           ((plan (name find_someone) (planner ?x) (object ?y))
            (not (equal ?x ?y)))
           ((plan (name snatch) (planner ?x) (object ?y))
            (and ?x
                 ?y
                 (not (equal ?x ?y))
                 (member (car ?x) '(human group animate))
                 (member (car ?y) '(human group animate))))
           ((goal (name help) (planner ?z) (object ?y))
            (equal (intended_function ?z) 'law))))

(subgoal '(((goal (name abduct) (planner ?x) (object ?y) (co_planner ?z)))
           ((plan (name snatch) (planner ?x) (object ?y))
            (and ?x
                 ?y
                 (not (equal ?x ?y))
                 (member (car ?x) '(human group animate))
                 (member (car ?y) '(human group animate))))
           ((plan (name accomplice) (planner ?x) (object ?z)))))
(subgoal '(((goal (name kidnap_for_ransom) (planner ?x) (object ?y)))
          ((goal (name abduct) (planner ?x) (object ?y))
           (and ?x ?y (not (equal ?x ?y))))))
;                                  -----
;                                    32
;                                  -----

(subplan '(((goal (name grab) (planner ?x)
             (objective (poss (actor ?x) (object ?y)))))
           ((plan (name take_plan) (planner ?x) (object ?y)))))

;                                  -----
;                                    33
;                                  -----

(subplan '(((plan (name ask_help) (planner ?x) (object ?y) (objective ?z)))
           ((plan (name let_know) (planner ?x) (to ?y) (object ?z))
            (and ?x ?y ?z (cd_event_type ?z)))))

;                                  -----
;                                    34
;                                  -----

(subplan '(((plan (name accomplice) (planner ?x) (object ?y) (objective ?z)))
           ((plan (name ask_help) (planner ?x) (object ?y) (objective ?z))
            (and (member (car ?x) '(human group animate))
                 (member (car ?y) '(human group animate))))))

;                                  -----
;                                    35
;                                  -----

(base_plan '(((plan (name not_agree) (planner ?x) (object ?y)))
             ((propel (actor ?y) (object ?x))
              (and (member (car ?x) '(group human))
                   (member (car ?y) '(group human))
                   (not (equal ?x ?y))))))
(base_plan '(((plan (name not_agree) (planner ?x) (object ?y)))
             ((mtrans (actor ?x) (object ?y)(state(mood(val ?n))))
              (and (member (car ?x) '(group human))
                   (member (car ?y) '(group human))
                   (neg_val ?n)
                   (not (equal ?x ?y))))))

(base_plan '(((plan (name arrest) (planner ?x) (object ?y)))
             ((ptrans (actor ?x) (object ?y) (to ?z))
              (and ?x ?y ?z (equal (intended_function ?z) 'law_enforcement)))))
;                                  -----
;                                    36
;                                  -----

(subplan '(((plan (name not_agree) (planner ?x) (object ?y)))
           ((plan (name snatch) (planner ?y) (object ?x)) (not (equal ?x ?y)))))
(subplan '(((plan (name not_agree) (planner ?x) (object ?y)))
           ((plan (name threat) (planner ?y) (object ?x))
            (not (equal ?x ?y))
                 (member (car ?x) '(human group animate)))))

;                                  -----
;                                    37
;                                  -----

(subplan '(((plan (name not_agree) (planner ?x) (object ?y)))
           ((plan (name force_into) (planner ?y) (object ?x)))))

;                                  -----
;                                    38
;                                  -----

(subgoal '(((plan (name not_agree) (planner ?x) (object ?y)))
           ((plan (name attention) (planner ?x)))))
(subgoal '(((plan (name not_agree) (planner ?x) (object ?y)))
           ((plan (name surprised) (planner ?x)(object ?y))
            (and ?x ?y (member (car ?y) '(human group animate))))))

;                                  -----
;                                    39
;                                  -----

(subgoal '(((plan (name attention) (planner ?x) (objective ?y)))
           ((plan (name know_event) (planner ?z) (objective ?y)))
           ((plan (name surprised) (planner ?x) (object ?y)))))

;                                  -----
;                                    40
;                                  -----

(subplan '(((plan (name call_help) (planner ?x) (object ?y)))
           ((plan (name 
                      know_event) (planner ?x) (objective ?y)) 
            (and ?x ?y
                 (member (cd_event_type (cadr ?y))
                         '(mental physical theme))))))

;                                  -----
;                                    41
;                                  -----

(subplan '(((goal (name provide_help) (planner ?x) (object ?y)))
           ((plan (name call_help) (planner ?y) (object ?x))
            (and ?x
                 ?y
                 (member (car ?x) '(human group animate))
                 (member (car ?y) '(human group animate))))))

(subplan '(((goal (name help) (planner ?x) (object ?y)))
           ((plan (name threat) (planner ?z) (object ?y))
            (and ?y ?z))
           ((plan (name arrest) (planner ?x) (object ?z)))))
;                                  -----
;                                    42
;                                  -----

(subplan '(((goal (name help) (planner ?x) (object ?y)))
           ((plan (name snatch) (planner ?z) (object ?y)))
           ((plan (name call_help) (planner ?w) (object ?x)))))

;                                  -----
;                                    43
;                                  -----

(base_goal '(((plan (name provide_help) (planner ?x) (object ?y)))
             ((is (actor ?y) (state (location (val ?n)))) (neg_val ?n))))

;                                  -----
;                                    44
;                                  -----

(subgoal '(
           ((goal (name help) (planner ?x) (object ?y)))
           ((plan (name provide_help) (planner ?x) (object ?y))
            (and ?x
                 ?y
                 (not (equal ?x ?y))
                 (member (car ?x) '(human animate group))))))

;                                  -----
;                                    45
;                                  -----

(subplan '(((plan (name provide_help) (planner ?x) (object ?y)))
           ((plan (name ask_help) (planner ?y) (object ?x))
            (not (equal ?x ?y)))))

;                                  -----
;                                    46
;                                  -----


(defun not_accessible (loc1 loc2)
  "This function finds out whether loc1 is accessible from loc2" 
  (and nil))
