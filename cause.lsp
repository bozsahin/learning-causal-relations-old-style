
; File name               :    cause.lsp
;
; Created at              :    7/12/1990 19:55:29 by CL(NX) Indexer
;
; Indexed (source) file   :    cause.lsp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; activate_variables...............   10
; analyze_tense....................   42
; antec............................   31
; assert_c.........................   43
; assimilate.......................    6
; causal_node......................   32
; cause............................   15
; chain............................   26
; circumscribe.....................    7
; clausify.........................   58
; combine_antecedents..............   19
; combine_cause....................   52
; combine_consequents..............   18
; combine_effect...................   53
; combine_hypotheses...............   51
; common_roles.....................   46
; common_roles_p...................   49
; conjunct.........................   33
; conseq...........................   30
; constant_conjunct................   20
; define_heuristic.................   16
; derive...........................   14
; disjunct.........................   34
; earlier..........................   21
; explain..........................    3
; extract_causal_relations.........   17
; find_related_hypos...............   11
; find_roles.......................   47
; get_agent........................   57
; get_d_var........................   35
; get_effect.......................   56
; get_roles........................   48
; get_roles_index..................   13
; get_single_event.................   54
; get_slots........................   41
; has_plan.........................   22
; leads_to.........................   23
; learn............................    4
; learn_from_scratch...............    5
; load_dynamic_variables...........    9
; means............................   29
; negative_cause...................   27
; nexus............................    1
; order_ltm_matches................   61
; parse............................    8
; populate_graph...................   12
; positive_cause...................   28
; pp_conj..........................   39
; pp_ev............................   40
; pp_ev_list.......................   38
; pp_node..........................   37
; print_hypothesis.................   36
; reg_cd...........................   44
; relate...........................    2
; requal...........................   50
; resolve..........................   25
; rhs_equal........................   24
; same_type........................   55
; save_role........................   45
; sethyp...........................   60
; subclause........................   59
; trans_closure....................   62
; trans_closure2...................   63
;
;                                    Common Lisp Indexer, v1.1, by Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

(defun nexus (nl_text)
  "This is the top-level function to the Nexus system. " 
  (format t "~2%Processing new episode ...~%") 
  (init_nx_globals) 
  (assimilate nl_text) 
  (relate *working_memory*) 
  (explain *short_term_memory*) 
  (record) 
  (format t "~2%NEXUS run completed ...~%"))

;                                  -----
;                                    2
;                                  -----

(defun relate (current_episode)
  "This function returns a list of sub-lists and assigns it to short
   term memory. Each sublist contains an episode name and a match count
   showing how much of events in episode are related with the episodes 
   (in long term memory). The list is sorted by descending match value." 
  (format t "~%  Relating new episode to LTM ...~%") 
  (setf (stm-episode_list *short_term_memory*) (order_ltm_matches
                                                (mapcar
                                                 #'(lambda
                                                    (ltm_episode)
                                                    (partial_ltm_match
                                                     (match_episodes
                                                      (scratch_pad-event_list
                                                       current_episode)
                                                      ltm_episode)
                                                     (list-length
                                                      (scratch_pad-event_list
                                                       current_episode))
                                                     (list-length
                                                      (episode-event_list
                                                       ltm_episode))))
                                                 *episodes*))) 
  (format t "~%    Related to ~S episodes.~%"
          (cond ((stm-episode_list *short_term_memory*)
                 (mapcar #'(lambda (x) (car x))
                         (stm-episode_list *short_term_memory*)))
                (t 'no))))

;                                  -----
;                                    3
;                                  -----

(defun explain (related_episodes)
  "This function tries to find a causal explanation for the current episode 
   in working memory, in light of related_episodes from long term memory.
   If a relevant hypothesis is found, it is removed from list and augmented
   version is pushed back into the *hypos* list. If none found, then it will
   generate a hypothesis from a single example (i.e., from scratch)." 
  (monitor "entering explain") 
  (format t "~%  Trying to find a relevant hypothesis ...~%") 
  (setq *current_hypo* (find_hypothesis (find_related_hypos
                                         related_episodes
                                         *hypos*
                                         nil))) 
  (cond (*current_hypo*
         (setq *hypos* (remove *current_hypo* *hypos*))
         (format t "~%    This episode can be explained by ~A hypothesis.~
~%    Augmenting ...~%"
                 (causal_hypothesis-name *current_hypo*))
         (learn (scratch_pad-event_list *working_memory*) related_episodes))
        (t
         (format t "~%    None found. Generating one from new episode ...~%")
         (learn_from_scratch (scratch_pad-event_list *working_memory*)
                             related_episodes))))

;                                  -----
;                                    4
;                                  -----

(defun learn (new_events related_episodes)
  "This function creates a new hypothesis for new_events and combines it with
   *current_hypo*. Combined hypothesis is returned as *current_hypo*." 
  (monitor "entering learn") 
  (setq *past_hypo* *current_hypo*
        *new_hypo* (learn_from_scratch new_events related_episodes)
        *current_hypo* (combine_hypotheses)))

;                                  -----
;                                    5
;                                  -----

(defun learn_from_scratch (new_events related_episodes)
  "This function derives and returns a new hypothesis from scratch." 
  (monitor "entering learn_from_scratch") 
  (extract_causal_relations new_events related_episodes) 
  (mapc #'(lambda (ev) (agency ev)) new_events) 
  (setq *current_hypo* (derive *causal_inferences*)) 
  (format t "~2%    Hypothesis generation completed ...~%") 
  *current_hypo*)

;                                  -----
;                                    6
;                                  -----

(defun assimilate (text)
  "This function converts NL input to CD forms by calling the NL analyzer
   (process_text). The result is then circumscribed by plans/goals and is
   transformed into Nexus structures (i.e., events and other STM/WM structures)
   Its side effect is to save that list in working memory to be processed 
   later" 
  (monitor "entering assimilate") 
  (setf (episode-event_list *working_memory*) (circumscribe
                                               (parse (check_input text)))) 
  (setf (episode-text *working_memory*) text) 
  (setf (episode-temporal_relations *working_memory*) (analyze_tense
                                                       (episode-event_list
                                                        *working_memory*))) 
  (format t "~%  Semantic Analysis of the episode completed ...~%"))

;                                  -----
;                                    7
;                                  -----

(defun circumscribe (cd_list)
  "This function is the hook to the Nexus plan and goal detector." 
  (format t "~%    Goal/plan detection in progress ...~%") 
  (find_goals_and_plans cd_list))

;                                  -----
;                                    8
;                                  -----

(defun parse (nl_text)
  "This function is the hook to the natural language analyzer. It calls the
   top level function of NL analyzer. It returns whatever the top level
   function returns, which is expected to be a list of CD forms." 
  (format t "~%    Parsing into semantic representation ...~%") 
  (process_text nl_text))

;                                  -----
;                                    9
;                                  -----

(defun load_dynamic_variables ()
  "THis function binds all dynamic variables in all causal graphs to their
    respective values. This eliminates any redundant reactivation later 
    needed. It also prevents any shadowing of a newly created dnamic 
    variable created by gentemp function. Called only in loading phase." 
  (dolist (hyp *hypos*) (activate_variables hyp)))

;                                  -----
;                                    10
;                                  -----

(defun activate_variables (hypo)
  "This function activates each dynamic variable/value pair in the hypo
   to their values. Event variables are of the form ('x value) and
   role variables are of the form (x value). They are in event_list and 
   roles_list of the hypo, respectively." 
  (monitor "entering activate_variables") 
  (mapc #'(lambda (var) (set (eval (car var)) (cadr var)))
        (causal_hypothesis-event_list hypo)) 
  (dolist (roles (causal_hypothesis-roles_list hypo))
    (mapc #'(lambda (var) (set (car var) (cadr var))) roles)) 
  t)

;                                  -----
;                                    11
;                                  -----

(defun find_related_hypos (related_episodes hyps result)
  "this function finds out the hypothesis available for related-episodes. It 
   does so by intersecting the source episodes of hypotheses with the 
   related-episodes. The intersection with highest cardinality gives the 
   most likely hypothesis." 
  (monitor "entering find_related_hypos") 
  (cond ((null hyps) (car result))
        (t
         (cond ((> (list-length (intersection (episode_names related_episodes)
                                              (causal_hypothesis-sources
                                               (car hyps))
                                              :test #'equal))
                   (list-length (cadr result)))
                (find_related_hypos related_episodes (cdr hyps)
                                    (cons (causal_hypothesis-name (car hyps))
                                          (intersection
                                           (episode_names related_episodes)
                                           (causal_hypothesis-sources
                                            (car hyps))
                                           :test
                                           #'equal))))
               (t (find_related_hypos related_episodes (cdr hyps) result))))))

;                                  -----
;                                    12
;                                  -----

(defun populate_graph (hyp)
  "This function populates the graph by replacing its event patterns with the
   events in the event_list and role patterns with the roles in the 
   first element of roles_list.
   It returns t" 
  (monitor "entering populate_graph") 
  (activate_variables hyp 1) 
  t)

;                                  -----
;                                    13
;                                  -----

(defun get_roles_index (source_list)
  "This function returns the index number of the roles_list to be
   instantiated later on. Event_list and roles_list have 1-1 correspondence." 
  (format t
          "~2%      Source episodes: ~:A ~2%      Enter the index # of the episode : "
          source_list) 
  (let ((index (read)))
    (cond ((or (< index 0) (> index (list-length source_list)))
           (get_roles_index source_list))
          (t index))))

;                                  -----
;                                    14
;                                  -----

(defun derive (causal_inferences)
  "This function creates and returns a new hypothesis. Its graph and event_list
   are filled in by causal inferences. It returns the causal hypothesis." 
  (monitor "entering derive") 
  (setq *ev_list* nil) 
  (setf (scratch_pad-name *working_memory*) (read_episode_name)) 
  (let ((h (make-causal_hypothesis)))
    (format t "~2%      Constructing a hypothesis for the new episode ...~%")
    (setf (causal_hypothesis-progressive h) (combine_consequents
                                             causal_inferences))
    (setf (causal_hypothesis-regressive h) (combine_antecedents
                                            causal_inferences))
    (setf (causal_hypothesis-event_list h) *ev_list*)
    (setf (causal_hypothesis-name h) (read_hypothesis_name))
    (setf (causal_hypothesis-sources h) (list
                                         (scratch_pad-name *working_memory*)))
    (setf (causal_hypothesis-roles_list h) (list *roles*))
    h))

;                                  -----
;                                    15
;                                  -----

(defun cause (ante conseq)
  "This function finds out if ante event is causally related to conseq 
   by a heuristic. When applicability and precondition of the
   heuristic are satisfied, its body returns the causal relationship.
   The globals variables used by heuristics are; two events (x,y), the
   current hypothesis (*current_hypo*), and related episodes. The names 
   of related episodes reside in short term memeory (STM)." 
  (monitor "entering cause") 
  (setq x ante
        y conseq) 
  (mapc #'(lambda (heuristic)
            (and (monitor "entering heur: " (causal_heuristic-name heuristic))
                 (eval (causal_heuristic-precondition heuristic))
                 (eval (causal_heuristic-applicability heuristic))
                 (eval (causal_heuristic-body heuristic))))
        *heuristics*) 
  t)

;                                  -----
;                                    16
;                                  -----

(defun define_heuristic (name applic precond body)
  "This function defines a 'causal_heuristic structure and loads its
   attributes. Its side effect is to save the heuristic in procedural
   long term memory." 
  (let ((h (make-causal_heuristic)))
    (setf (causal_heuristic-name h) name)
    (setf (causal_heuristic-applicability h) applic)
    (setf (causal_heuristic-precondition h) precond)
    (setf (causal_heuristic-body h) body)
    (update_memory h)))

;                                  -----
;                                    17
;                                  -----

(defun extract_causal_relations (new_episode related_episodes)
  "This function calls the causal heuristics for each pair of events in
   working memory to extract causal connections. The causal graph of
   related_episodes is utilized in finding constant conjunctions and
   similarities. Result is a list of (cause x y m) saved in
   *causal_inferences* after cause/not_cause conflicts are resolved." 
  (monitor "entering extract_causal_relations") 
  (setq *causal_inferences* nil
        *neg_causal_inferences* nil
        *roles* nil
        *e* (stm-episode_list related_episodes)
        *s* new_episode) 
  (mapc #'(lambda (e1)
            (mapc #'(lambda (e2) (and (not (eq e1 e2)) (cause e1 e2)))
                  (scratch_pad-event_list *working_memory*)))
        (scratch_pad-event_list *working_memory*)) 
  (setq *causal_inferences* (resolve *causal_inferences*
                                     (trans_closure *neg_causal_inferences*))))

;                                  -----
;                                    18
;                                  -----

(defun combine_consequents (causal_inferences)
  "This function brings all the consequents with common antecedents together.
   Initially, they form a single conjunction, later on it will change into
   an and/or graph. It returns the resultant list. Its side effect is to
   build templates of events (via reg_cd) mentioned in causal inferences. It is
   later used in graph construction. The dynamic variables created and their
   values are saved in the *ev_list* variable by the 'reg' function." 
  (monitor "entering combine_consequents") 
  (let ((consequents nil) 
        (a_list nil) 
        (c_list nil))
    (mapc #'(lambda (c1)
              (setq c_list nil
                    antvar (reg_cd (antec c1))
                    consvar (reg_cd (conseq c1))) 
              (setq ac1 (get_d_var antvar)
                    cc1 (get_d_var consvar)) 
              (set ac1 antvar) 
              (set cc1 consvar) 
              (mapc #'(lambda (c2)
                        (setq consvar2 (reg_cd (conseq c2))) 
                        (setq cc2 (get_d_var consvar2)) 
                        (set cc2 consvar2) 
                        (and (not (eq c1 c2))
                             (equal (antec c1) (antec c2))
                             (not (find (antec c1) a_list :test 'equal))
                             (push (list cc2 (means c2)) c_list)))
                    causal_inferences) 
              (cond (c_list
                     (push (list cc1 (means c1)) c_list)
                     (push (causal_node (disjunct (conjunct (list ac1)))
                                        (disjunct (conjunct c_list)))
                           consequents))
                    (t
                     (and (not (find (antec c1) a_list :test 'equal))
                          (push (causal_node (disjunct (conjunct (list ac1)))
                                             (disjunct
                                              (conjunct
                                               (list (list cc1 (means c1))))))
                                consequents)))) 
              (push (antec c1) a_list))
          causal_inferences)
    consequents))

;                                  -----
;                                    19
;                                  -----

(defun combine_antecedents (causal_inferences)
  "This function brings all the antecedents with common consequents together.
   Initially, they form a single conjunction, later on it will change into
   an and/or graph. It returns the resultant list. The variable patterns are
   created as in combine_consequents." 
  (monitor "entering combine_antecedents") 
  (let ((antecedents nil) 
        (c_list nil) 
        (a_list nil))
    (mapc #'(lambda (c1)
              (setq a_list nil
                    antvar (reg_cd (antec c1))
                    consvar (reg_cd (conseq c1))) 
              (setq ac1 (get_d_var antvar)
                    cc1 (get_d_var consvar)) 
              (set ac1 antvar) 
              (set cc1 consvar) 
              (mapc #'(lambda (c2)
                        (setq antvar2 (reg_cd (antec c2))) 
                        (setq ac2 (get_d_var antvar2)) 
                        (set ac2 antvar2) 
                        (and (not (eq c1 c2))
                             (equal (conseq c1) (conseq c2))
                             (not (find (conseq c1) c_list :test 'equal))
                             (push (list ac2 (means c2)) a_list)))
                    causal_inferences) 
              (cond (a_list
                     (push (list ac1 (means c1)) a_list)
                     (push (causal_node (disjunct (conjunct a_list))
                                        (disjunct (conjunct (list cc1))))
                           antecedents))
                    (t
                     (and (not (find (conseq c1) c_list :test 'equal))
                          (push (causal_node (disjunct
                                              (conjunct
                                               (list (list ac1 (means c1)))))
                                             (disjunct (conjunct (list cc1))))
                                antecedents)))) 
              (push (conseq c1) c_list))
          causal_inferences)
    antecedents))

;                                  -----
;                                    20
;                                  -----

(defun constant_conjunct (c e episode_names)
  "this function finds out whether c and e events both appear in all episodes
   names of which are in episode_names. It also checks to see if they violate
   time precedence. It returns t or nil. The threshold for partial match is 50%
   mc and me are events similar to c and e in other episodes, respectively." 
  (monitor "entering constant_conjunct") 
  (do* ((e_name episode_names (cdr e_name)) 
        (ep (find_episode (caar e_name)) (find_episode (caar e_name))) 
        (mc nil) 
        (me nil))
       ((endp e_name) t)
    (setq mc (partial_match c (episode-event_list ep))
          me (partial_match e (episode-event_list ep)))
    ''(format t "~%mc ~A ~%me ~A" mc me)
    (cond ((or (null mc)
               (null me)
               (earlier (cadr (filler_role 'time me))
                        (cadr (filler_role 'time mc))
                        (episode-temporal_relations ep)))
           (return nil)))))

;                                  -----
;                                    21
;                                  -----

(defun earlier (e1 e2 temp_rels)
  "This function finds out whether e1 is before e2 (e1, e2 being two time
   points) given the list of temporal relations in temp_rels. Each 
   element in before_list is expected to be (ti tj) which means ti is before tj.
   Note that (e1 e2) relation may not be explicit in the before list, but
   may be deduced via recursion.It returns t (succ.) or nil (unsucc.)." 
  (cond ((or (null e1) (null e2)) nil)
        ((null (remove-if-not #'(lambda (x) (equal (second x) e2))
                              (cdr temp_rels)))
         nil)
        ((member (list e1 e2) temp_rels :test #'equal) t)
        (t
         (or_function (mapcar #'(lambda (rel)
                                  (cond ((equal (second rel) e2)
                                         (earlier e1 (first rel) temp_rels))))
                              (cdr temp_rels))))))

;                                  -----
;                                    22
;                                  -----

(defun has_plan (plan_name cd)
  "This function finds out if cd event is a consequence of the plan.
   Returns t or nil. If cd type is 'is, only goal-based causality is looked
   for. Result cause is for primitive cds activating goal states, reason
   cause is for primitive cds being part of plans." 
  (monitor "entering has_plan") 
  (let ((found (do ((plist *base_goals* (cdr plist)))
                   ((endp plist) nil)
                 (and plist
                      (equal plan_name (filler_role 'name (lhs (caar plist))))
                      (match_side (car (rhs (car plist))) cd nil)
                      (return 'activate)))))
    (cond (found)
          ((equal (header_cd cd) 'is) nil)
          (t
           (do ((plist *base_plans* (cdr plist)))
               ((endp plist) nil)
             (and plist
                  (equal plan_name (filler_role 'name (lhs (caar plist))))
                  (match_side (car (rhs (car plist))) cd nil)
                  (return 'consequence)))))))

;                                  -----
;                                    23
;                                  -----

(defun leads_to (plan_name goal_name)
  "This function determines if the plan leads to the goal. Returns t or nil.
   Causal relationship can come from sub-plan or sub-goal relationships." 
  (monitor "entering leads_to") 
  (let ((found (do ((glist *sub_plans* (cdr glist)))
                   ((endp glist) nil)
                 (and (rhs_equal goal_name (rhs (car glist)))
                      (equal plan_name (filler_role 'name (lhs (caar glist))))
                      (return 'plan-part)))))
    (cond (found)
          (t
           (do ((plist *sub_goals* (cdr plist)))
               ((endp plist) nil)
             (and (rhs_equal plan_name (rhs (car plist)))
                  (equal goal_name (filler_role 'name (lhs (caar plist))))
                  (return 'sub-goal)))))))

;                                  -----
;                                    24
;                                  -----

(defun rhs_equal (p_name rhs_list)
  "This function returns t if any member of rhs_list has a name equal to
   p_name." 
  (find p_name rhs_list :key #'(lambda (x) (filler_role 'name (car x))) :test
        #'equal))

;                                  -----
;                                    25
;                                  -----

(defun resolve (inf_list neg_inf_list)
  "This function resolves the cause/not_cause conflicts in the inf_list and
   neg_inf_list. The latter is the transitive closure of the original
   negative inferences.
   when a not_cause is found, anything with same agent and effect is
   removed from the inf_list. It returns the modified inf_list which
   contains only resolved positive causes." 
  (monitor "entering resolve") 
  (do* ((ilist (append inf_list neg_inf_list)) 
        (n_clause (find 'not_cause ilist :test #'equal :key #'car)
                  (find 'not_cause ilist :test #'equal :key #'car)))
       ((null n_clause) ilist)
    (setq ilist (remove n_clause ilist :test #'equal))
    (setq ilist (remove-if #'(lambda (x)
                               (and (equal (assoc 'agent n_clause)
                                           (assoc 'agent x))
                                    (equal (assoc 'effect n_clause)
                                           (assoc 'effect x))))
                           ilist))))

;                                  -----
;                                    26
;                                  -----

(defun chain (event_list c_chain)
  "This function put the events in the event_list in a causal chain-- a
   list of (cause ..) lists. It returns the causal chain. Event_list is 
   a list of CDs (lists)." 
  (monitor "entering chain") 
  (cond ((null event_list) c_chain)
        ((< (list-length event_list) 2) c_chain)
        (t
         (chain (cdr event_list)
                (cond ((and (basic_cd (car event_list))
                            (basic_cd (cadr event_list))
                            (filler_role 'time (car event_list))
                            (filler_role 'time (cadr event_list))
                            (earlier (cadr (filler_role
                                            'time
                                            (car event_list)))
                                     (cadr (filler_role
                                            'time
                                            (cadr event_list)))
                                     (scratch_pad-temporal_relations *working_memory*)))
                       (cons_end c_chain
                                 (positive_cause (car event_list)
                                                 (cadr event_list) 'result)))
                      (t c_chain))))))

;                                  -----
;                                    27
;                                  -----

(defun negative_cause (cause effect)
  (list 'not_cause (list 'agent cause) (list 'effect effect)))

;                                  -----
;                                    28
;                                  -----

(defun positive_cause (cause effect means)
  (list 'cause (list 'agent cause) (list 'effect effect) (list 'means means)))

;                                  -----
;                                    29
;                                  -----

(defun means (causal_inference) (assoc 'means (cdr causal_inference)))

;                                  -----
;                                    30
;                                  -----

(defun conseq (causal_inference)
  (second (assoc 'effect (cdr causal_inference))))

;                                  -----
;                                    31
;                                  -----

(defun antec (causal_inference) (second (assoc 'agent (cdr causal_inference))))

;                                  -----
;                                    32
;                                  -----

(defun causal_node (cause effect)
  (list 'cause (list 'agent cause) (list 'effect effect)))

;                                  -----
;                                    33
;                                  -----

(defun conjunct (literal_list)
  (cond ((listp (car literal_list)) (cons 'and literal_list))
        (t (list 'and literal_list))))

;                                  -----
;                                    34
;                                  -----

(defun disjunct (literal_list) (list 'or literal_list))

;                                  -----
;                                    35
;                                  -----

(defun get_d_var (cdvar)
  "This function registers the name and value of a dynamic variable dvar
   in the *ev_list*. A dynamic va will be generated if cdvar is not 
   already in the list. The form of that list is (('x value) ..). dvar 
   contains the name of the dynamically bound variable. It essentially
   does the same thing as save_role, but for whole CD's not roles." 
  (let ((in_cd (find cdvar *ev_list* :test #'equal :key #'second)))
    (cond (in_cd (setq *dynvar* (eval (car in_cd))))
          (t
           (setq *dynvar* (gentemp))
           (push (list (eval (list 'quote (list 'quote *dynvar*))) cdvar)
                 *ev_list*)
           *dynvar*))))

;                                  -----
;                                    36
;                                  -----

(defun print_hypothesis (hyp)
  "This function displays the causal hypothesis in a somewhat readable
   form. Refer to the grammar for causal hypos about the data structures.
   The variables are re-activated to prevent any shadowing due to off-line
   printing (i.e., printing a hypo directly from file before nexus is loaded).
   If the hypothesis has more than one source, variable patterns will be 
   printed in causes and effects of progressive and regressive chains, 
   respectively." 
  (format t
          "~3%~33TCausal Hypothesis~3%~25TName : ~A~2%~25TDerived from : ~A~2%"
          (causal_hypothesis-name hyp) (causal_hypothesis-sources hyp)) 
  (activate_variables hyp) 
  (let ((cnt (list-length (causal_hypothesis-sources hyp))))
    (and (> cnt 1)
         (format t "~%~5TThis hypothesis explains more than one episode.~
~%~5TCauses and effects are printed as patterns in progressive and~
~%~5Tregressive chains, respectively.~%"))
    (format t "~2%Progressive causation : ~%-----------------------~%")
    (mapc #'(lambda (node) (pp_node node 'f cnt))
          (causal_hypothesis-progressive hyp))
    (format t "~2%Regressive causation  : ~%-----------------------~%")
    (mapc #'(lambda (node) (pp_node node 'b cnt))
          (causal_hypothesis-regressive hyp))
    t))

;                                  -----
;                                    37
;                                  -----

(defun pp_node (n chain cnt)
  "This function pretty prints the nodes of a causal graph." 
  (cond ((equal chain 'f)
         (format t "~2%causes : ")
         (pp_ev_list (cadr (assoc 'agent n)) 'single cnt)
         (format t "~%effects: ")
         (pp_ev_list (cadr (assoc 'effect n)) 'multi cnt)
         (terpri))
        (t
         (format t "~2%causes : ")
         (pp_ev_list (cadr (assoc 'agent n)) 'multi cnt)
         (format t "~%effects: ")
         (pp_ev_list (cadr (assoc 'effect n)) 'single cnt)
         (terpri))))

;                                  -----
;                                    38
;                                  -----

(defun pp_ev_list (ev_list map cnt)
  "This function pretty prints the cause/effect clauses in causal hypos." 
  (format t "~%  ~:A" (car ev_list)) 
  (mapc #'(lambda (c) (pp_conj c map cnt)) (cdr ev_list)) 
  t)

;                                  -----
;                                    39
;                                  -----

(defun pp_conj (c map cnt)
  "This function pretty prints the conjunction list." 
  (format t "~%    ~:A" (car c)) 
  (mapc #'(lambda (ev) (pp_ev ev map cnt)) (cdr c)) 
  t)

;                                  -----
;                                    40
;                                  -----

(defun pp_ev (ev map cnt)
  "This function prints the event and associated means (if any). Note that
   argument is evaluated first to get the dynamically bound value." 
  (cond ((or (= cnt 1) (and (> cnt 1) (equal map 'multi)))
         (cond ((equal (list-length ev) 2)
                (format t "~%      ~:A~%      ~:A" (get_slots (eval (car ev)))
                        (cadr ev)))
               (t (format t "~%      ~A" (get_slots (eval (car ev)))))))
        (t
         (cond ((equal (list-length ev) 2)
                (format t "~%      ~:A~%      ~:A" (eval (car ev)) (cadr ev)))
               (t (format t "~%      ~A" (eval (car ev))))))))

;                                  -----
;                                    41
;                                  -----

(defun get_slots (cd)
  "This function returns the cd in instantiated form. Instantiations are
   saved in the dynamic variables." 
  (cons (header_cd cd)
        (mapcar #'(lambda (slot)
                    (cond ((is_var (filler_pair slot))
                           (list (car slot)
                                 (eval (name_var (filler_pair slot)))))
                          (t slot)))
                (roles_cd cd))))

;                                  -----
;                                    42
;                                  -----

(defun analyze_tense (cd_list)
  "This function takes the cd_list (an episode in CD form), and finds out
   the temporal relations within the episode by looking at the tenses. It
   a list of the form (before (t1 t2) (t2 t3) ...) where ts are event times
   and (before (ti tj) ) means ti is before tj in time. Time attribute of CDs
   take values of the form (relation_type value) where relation_type is
   at, before, .. The check in 2nd clause is needed to prevent cyclic temporal
   relations (in case first cd's tense is past perfect)" 
  (monitor "entering analyze_tense") 
  (do* ((cds cd_list (cdr cds)) 
        (cd (car cds) (car cds)) 
        (origin (gentemp "time0-")) 
        (now_past origin) 
        (now_past_perfect origin) 
        (tense (filler_role 'tense cd) (filler_role 'tense cd)) 
        (e_time (cadr (filler_role 'time cd)) (cadr (filler_role 'time cd))) 
        (result '(before)))
       ((null cd) (reverse result))
    (cond ((equal tense '(simple (past)))
           (push (list now_past e_time) result)
           (setq now_past e_time))
          ((equal tense '(perfect (past)))
           (push (list now_past_perfect e_time) result)
           (and (not (equal now_past_perfect now_past))
                (push (list e_time now_past) result))
           (setq now_past_perfect e_time)))))

;                                  -----
;                                    43
;                                  -----

(defun assert_c (clause)
  "This function asserts a causal clause (funny!) of the type 'type. The clause
    is saved in *causal_inferences* or *neg_causal_inferences*
    . It returns t(saved) or nil(duplicate)." 
  (cond ((equal (car clause) 'cause)
         (and (not (find clause *causal_inferences* :test #'equal))
              (push clause *causal_inferences*)))
        (t
         (and (not (find clause *neg_causal_inferences* :test #'equal))
              (push clause *neg_causal_inferences*)))))

;                                  -----
;                                    44
;                                  -----

(defun reg_cd (cd_constant)
  "This function is same as gencd except the side effect: It saves the role
   values in the *roles* variable after they are generalized. used for causal
    graph construction. It returns the value of (gencd cd). The value of global
    variable *dynvar* is updated by the save_role function." 
  (cond ((cd_event_type cd_constant)
         (do* ((role_list cd_constant (roles_cd role_list)) 
               (role nil (caar role_list)) 
               (g_list nil
                       (cond ((equal role 'name)
                              (cons (role_pair role_list) g_list))
                             (role
                              (save_role (filler_pair (role_pair role_list)))
                              (cons (list role (make_var *dynvar*)) g_list))
                             (t g_list))))
              ((null role_list)
               (cons (header_cd cd_constant) (reverse g_list)))))
        (t
         (save_role (filler_pair cd_constant))
         (list (header_cd cd_constant) (make_var *dynvar*)))))

;                                  -----
;                                    45
;                                  -----

(defun save_role (filler)
  "This function saves the filler of a role in the *roles* list if it's not
   already in there. returns t. If it's in the list *dynvar* is set to
   the existing variable, otherwise, a new variable is put into *dynvar*." 
  (let ((in_role (find filler *roles* :test #'equal :key #'second)))
    (cond (in_role (setq *dynvar* (car in_role)))
          (t (setq *dynvar* (gentemp)) (push (list *dynvar* filler) *roles*)))))

;                                  -----
;                                    46
;                                  -----

(defun common_roles (a_cd b_cd)
  "This function finds out if two cds have any common slot fillers at all.
   Note that slot fillers are found recursively since a slot may have a CD
   as its value." 
  (let ((roles_a (find_roles (roles_cd a_cd))) 
        (roles_b (find_roles (roles_cd b_cd))))
    (and (> (list-length roles_a)
            (list-length (set-difference roles_a roles_b :test #'equal))))))

;                                  -----
;                                    47
;                                  -----

(defun find_roles (cd_roles)
  "This function finds the set of slot values in the cd." 
  (mapcar #'(lambda (role) (get_roles role)) cd_roles))

;                                  -----
;                                    48
;                                  -----

(defun get_roles (role)
  "This function gets the roles in the role recursively" 
  (cond ((null role) nil)
        ((cd_event_type (filler_pair role))
         (find_roles (roles_cd (filler_pair role))))
        (t (filler_pair role))))

;                                  -----
;                                    49
;                                  -----

(defun common_roles_p (cd1 cd2)
  "This function finds out if key cases are common in 2 CDs" 
  (let ((p1 (filler_role 'planner cd1)) 
        (p2 (filler_role 'planner cd2)) 
        (o1 (filler_role 'object cd1)) 
        (o2 (filler_role 'object cd2)) 
        (a1 (filler_role 'actor cd1)) 
        (a2 (filler_role 'actor cd2)) 
        (ob1 (filler_role 'objective cd1)) 
        (ob2 (filler_role 'objective cd2)) 
        (cp1 (filler_role 'co_planner cd1)) 
        (cp2 (filler_role 'co_planner cd2)))
    (or (requal p1 p2)
        (requal p1 o2)
        (requal o1 p2)
        (requal o1 o2)
        (requal p1 a2)
        (requal a1 p2)
        (requal ob1 ob2)
        (requal cp1 p2)
        (requal p1 cp2)
        (requal o1 cp2)
        (requal cp1 o2))))

;                                  -----
;                                    50
;                                  -----

(defun requal (c1 c2)
  "This function returns t if arguments are non nill and equalp to each other" 
  (and c1 c2 (equalp c1 c2)))

;                                  -----
;                                    51
;                                  -----

(defun combine_hypotheses ()
  "This function combines the new hypothesis *new_hypo* with the old
   hypothesis *old_hypo* based on the algorithm given in chapter 4. It returns
   the combined hypothesis." 
  (format t "~%  Combining hypotheses ~S and ~S ...~%"
          (causal_hypothesis-name *past_hypo*)
          (causal_hypothesis-name *new_hypo*)) 
  (let ((comb_hypo (make-causal_hypothesis :sources
                                           (append
                                            (causal_hypothesis-sources
                                             *past_hypo*)
                                            (causal_hypothesis-sources
                                             *new_hypo*))
                                           :event_list
                                           (append
                                            (causal_hypothesis-event_list
                                             *past_hypo*)
                                            (causal_hypothesis-event_list
                                             *new_hypo*))
                                           :roles_list
                                           (append
                                            (causal_hypothesis-roles_list
                                             *past_hypo*)
                                            (causal_hypothesis-roles_list
                                             *new_hypo*)))))
    (dolist (clause (causal_hypothesis-progressive *past_hypo*))
      (combine_cause clause))
    (dolist (clause (causal_hypothesis-regressive *past_hypo*))
      (combine_effect clause))
    (setf (causal_hypothesis-progressive comb_hypo) (append
                                                     (causal_hypothesis-progressive
                                                      *new_hypo*)
                                                     *new_f_chain*))
    (setf (causal_hypothesis-regressive comb_hypo) (append
                                                    (causal_hypothesis-regressive
                                                     *new_hypo*)
                                                    *new_b_chain*))
    (setf (causal_hypothesis-name comb_hypo) (read_hypothesis_name))
    comb_hypo))

;                                  -----
;                                    52
;                                  -----

(defun combine_cause (clause)
  "this function checks the clause ( a causal node) against the progressive 
   chain of *new_hypo*. *new_hypo* may be modified as a result. It accumulates
   the result in *new_f_chain*. Agency causes are not combined(but included)
   since they are not event-event relations but entity-event." 
  (let* ((agent (assoc 'agent clause)) 
         (effect (assoc 'effect clause)) 
         (corresponding_clause (find (get_single_event agent)
                                     (causal_hypothesis-progressive *new_hypo*)
                                     :test #'same_type :key #'get_agent)))
    (cond ((equal (header_cd (get_single_event agent)) 'do)
           (pushnew clause *new_f_chain* :test #'equal)
           (and corresponding_clause
                (pushnew corresponding_clause *new_f_chain* :test #'equal)))
          (t
           (and corresponding_clause
                (setf (causal_hypothesis-progressive *new_hypo*) (remove
                                                                  corresponding_clause
                                                                  (causal_hypothesis-progressive
                                                                   *new_hypo*))))
           (pushnew (list 'cause agent
                          (clausify 'cause effect
                                    (assoc 'effect corresponding_clause)))
                    *new_f_chain* :test #'equal)))) 
  t)

;                                  -----
;                                    53
;                                  -----

(defun combine_effect (clause)
  "this function checks the clause ( a causal node) against the progressive 
   chain of *new_hypo*. *new_hypo* may be modified as a result. It accumulates
   the result in *new_b_chain*." 
  (let* ((agent (assoc 'agent clause)) 
         (effect (assoc 'effect clause)) 
         (corresponding_clause (find (get_single_event effect)
                                     (causal_hypothesis-regressive *new_hypo*)
                                     :test #'same_type :key #'get_effect)))
    (cond ((equal (header_cd (get_single_event effect)) 'do)
           (pushnew clause *new_b_chain* :test #'equal)
           (and corresponding_clause
                (pushnew corresponding_clause *new_b_chain* :test #'equal)))
          (t
           (and corresponding_clause
                (setf (causal_hypothesis-regressive *new_hypo*) (remove
                                                                 corresponding_clause
                                                                 (causal_hypothesis-regressive
                                                                  *new_hypo*))))
           (pushnew (list 'cause
                          (clausify 'effect agent
                                    (assoc 'agent corresponding_clause))
                          effect)
                    *new_b_chain* :test #'equal)))) 
  t)

;                                  -----
;                                    54
;                                  -----

(defun get_single_event (clause)
  "This function returns the only event in the event_list of clause." 
  (eval (caadr (cadadr clause))))

;                                  -----
;                                    55
;                                  -----

(defun same_type (ev1 ev2)
  "Two events are same type if their name slots or headers match." 
  (and (cd_event_type ev2)
       (cond ((equal (cd_event_type ev1) 'theme)
              (requal (filler_role 'name ev1) (filler_role 'name ev2)))
             (t (requal (header_cd ev1) (header_cd ev2))))))

;                                  -----
;                                    56
;                                  -----

(defun get_effect (clause)
  "gets the singular agent of the singular event." 
  (get_single_event (assoc 'effect clause)))

;                                  -----
;                                    57
;                                  -----

(defun get_agent (clause)
  "gets the singular agent of the singular event." 
  (get_single_event (assoc 'agent clause)))

;                                  -----
;                                    58
;                                  -----

(defun clausify (type first_part second_part)
  "This nasty function finds out common parts of first and second parts and
   makes a conjunction. The rest is lumped into disjunctions. Events
   which are in first part but not in second part are also taken off the
   first part and form a disjunction. If either 
   part is nil, the other one is returned. Otherwise, the combined clause
   (of type) is returned." 
  (cond ((null second_part) first_part)
        ((null first_part) second_part)
        (t
         (let ((disj_list (cdadr first_part)) 
               (ev_list (cdr (cadadr second_part))) 
               (common_list nil) 
               (new_disj_list) 
               (new_diff_list nil) 
               (flag nil) 
               (diff_list nil))
           (dolist (conj disj_list)
             (setq common_list nil
                   diff_list nil) 
             (dolist (event2 (cdr conj))
               (setq flag nil) 
               (dolist (event ev_list)
                 (cond ((same_type (eval (car event)) (eval (car event2)))
                        (setq flag t)
                        (push event common_list)
                        (setq ev_list (remove event ev_list))))) 
               (cond (flag (push event2 common_list))
                     (t (push event2 diff_list)))) 
             (and common_list (push common_list new_disj_list)) 
             (and diff_list (push diff_list new_diff_list)))
           (cond ((equal type 'cause)
                  (subclause 'effect ev_list new_diff_list new_disj_list))
                 (t (subclause 'agent ev_list new_diff_list new_disj_list)))))))

;                                  -----
;                                    59
;                                  -----

(defun subclause (type conj difference disj_list)
  "This function makes up a sub clause of type type. Conj is the remaining
   events and forms a conjunction. disj_list is the modified disjunctions
   with common events inserted. difference is the remaining events which
   were in first_part but not in second part (see clausify)." 
  (let* ((conj_diff (cons (and conj (cons 'and conj))
                          (append (and difference
                                       (mapcar #'(lambda
                                                  (diff)
                                                  (cons 'and diff))
                                               difference))
                                  (and disj_list
                                       (mapcar #'(lambda
                                                  (disj)
                                                  (cons 'and disj))
                                               disj_list))))) 
         (comb_list (remove-if #'null conj_diff)))
    (list type (cons 'or comb_list))))

;                                  -----
;                                    60
;                                  -----

(defun sethyp (type hyp)
  "This is a function used for debugging purposes." 
  (cond ((equal type 'old) (setq *past_hypo* hyp) t)
        (t (setq *new_hypo* hyp) t)) 
  (activate_variables hyp) 
  (setq *new_f_chain* nil
        *new_b_chain* nil))

;                                  -----
;                                    61
;                                  -----

(defun order_ltm_matches (matched_episode_list)
  "This function removes null values (no matches) and sorts the remaining
   matches by descending match value (chapter 5)." 
  (sort (remove-if #'null matched_episode_list) #'> :key #'second))

;                                  -----
;                                    62
;                                  -----

(defun trans_closure (relation)
  "This function finds the transitive closure of the relation. Relation
   is in the form ((rel elm1 elm2) (rel elm3 elm4) ..)." 
  (let ((closure relation) 
        (y nil) 
        (flag nil))
    (dolist (tuple relation)
      (setq y (trans_closure2 tuple closure)) 
      (and y
           (setq flag t)
           (mapc #'(lambda (sy)
                     (push (list (first tuple) (second tuple) (third sy))
                           closure))
                 y)))
    (cond (flag (trans_closure closure)) (t closure))))

;                                  -----
;                                    63
;                                  -----

(defun trans_closure2 (tuple rel)
  "This function returns every tuple in rel which has same first element
   as the second of tuple." 
  (let ((result nil) 
        (firstel (second tuple)) 
        (secel (third tuple)))
    (dolist (tp rel result)
      (and (equal secel (second tp))
           (not (member (list (first tuple) firstel (third tp)) rel :test
                        #'equal))
           (push tp result)))))
