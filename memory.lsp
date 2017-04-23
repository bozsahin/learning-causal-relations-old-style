
; File name               :    memory.lsp
;
; Created at              :    7/12/1990 19:56:35 by CL(NX) Indexer
;
; Indexed (source) file   :    memory.lsp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; causal_heuristic.................    1
; causal_hypothesis................    2
; coerce_wm........................   12
; create_a_new_episode.............   13
; episode..........................    3
; episode_names....................   20
; find_episode.....................   14
; find_hypothesis..................   15
; init_memory......................    6
; init_nx_globals..................    7
; load_memory......................    8
; read_episode_name................   16
; read_hypothesis_name.............   17
; record...........................    9
; save_memory......................   10
; scratch_pad......................    5
; stm..............................    4
; unique_episode_name..............   18
; unique_hypo_name.................   19
; update_memory....................   11
;
;                                    Common Lisp Indexer, v1.1, by Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

(defstruct causal_heuristic
  "this defines a causal heuristic as having three parts. Precondition, 
   appilicability, and procedure body. If precond and applic are satisfied,
   then the heuristic (i.e, its body) can be applied. All three are lambda
   expressions." 
  (name nil) 
  (precondition nil) 
  (applicability nil) 
  (body nil) 
  (utility 0))

;                                  -----
;                                    2
;                                  -----

(defstruct causal_hypothesis
  "This structure defines an AND/OR graph for representing causal graphs. 
   'Sources' identify the episodes in LTM from which the hypo was derived.
   'Event_list' contains a generalized list of CD events which take part in
   the causal graph. Roles_list is a local semantic memory which contains
    the list of roles in the events of the causal graph. Causal AND/OR graph
     is split into progressive and regressive chains to facilitate 
    question answering. It is indexed by
    the event_list,i.e, for each element of event_list, there is a corresponding
    element of roles_list. That helps instantiate a causal graph very fast and
    correctly. 'graph' is the and/or graph itself. 
    Its syntax is given in chapter 4." 
  (name nil) 
  (type nil) 
  (sources nil) 
  (event_list nil) 
  (roles_list nil) 
  (progressive nil) 
  (regressive nil))

;                                  -----
;                                    3
;                                  -----

(defstruct episode
  " An episode is a semantic representation of a coherent set of
    sentences (events). Events are stored in cd form. It also has a small 
    local semantic memory for event-object and object-object relations.'text'
    is saved for debugging purposes." 
  (name nil) 
  (event_list nil) 
  (temporal_relations nil) 
  (text nil))

;                                  -----
;                                    4
;                                  -----

(defstruct stm
  "Short term memory. It can only remember a list of episode names found
   relevant during processing. Names are stored in a list. The list consists
   of (name match-count) where match-count shows how relevant the episode is,
   in terms of how many events in it are matched. Episode names should be
   unique, otherwise there will be name shadowing." 
  (episode_list nil))

;                                  -----
;                                    5
;                                  -----

(defstruct (scratch_pad (:include episode))
  "working memory. It's different than STM. Primarily used for
    matching purposes, not recall. Also limited in size and life 
    span.")

;                                  -----
;                                    6
;                                  -----

(defun init_memory ()
  "Nexus's memory consists of long-term, short-term and working memories. 
   Short term memory is used for recalling the names of relevant episodes.
   Working memory is used  as a scratchpad during matching. Long term 
   memory has episodic, declarative, and procedural components. ELTM is
   for storing episodes and each episode could have a small local 
   declarative memory component. DLTM is for keeping object-object
   relations. Procedural component includes plan-base, inference rules, causal
   heuristics, and causal hypotheses. All these are initialized by this
   routine." 
  (setq *episodes* nil
        *hypos* nil
        *heuristics* nil
        *inference_rules* nil
        *domain_knowledge* nil
        *short_term_memory* (make-stm)
        *working_memory* (make-scratch_pad :name 'wm)) 
  (init_nx_globals))

;                                  -----
;                                    7
;                                  -----

(defun init_nx_globals ()
  "This function initalizes the variables that change in each iteration only." 
  (setq *roles* nil
        *new_f_chain* nil
        *new_b_chain* nil
        *current_hypo* nil
        *causal_inferences* nil
        *neg_causal_inferences* nil
        *ev_list* nil))

;                                  -----
;                                    8
;                                  -----

(defun load_memory ()
  " This function loads the top level memory structures and all their instances
    from files. *heuristics* memory is set when cause.lsp is loaded." 
  (setf *episodes* (with-open-file (in "ltm_epi.lsp" :direction :input
                                    :if-does-not-exist :error)
                     (read in))) 
  (setf *hypos* (with-open-file (in "ltm_proc.lsp" :direction :input
                                 :if-does-not-exist :error)
                  (read in))) 
  (load 'ltm_proc2.lsp) 
  (load 'ltm_decl.lsp))

;                                  -----
;                                    9
;                                  -----

(defun record ()
  "This function updates the LTM" 
  (cond ((yes-or-no-p "~%  Save session ? ")
         (update_memory *current_hypo*)
         (update_memory *working_memory*)
         (save_memory *episodes* "ltm_epi.lsp")
         (save_memory *hypos* "ltm_proc.lsp")
         (format t "~%  Episodic and Procedural LTM updated.~%"))
        (t)))

;                                  -----
;                                    10
;                                  -----

(defun save_memory (memory filename)
  " This function saves the memory structure to a file. It returns
    no values (blame it on CL). " 
  (with-open-file (ofile filename :direction :output :if-exists :new-version)
    (pprint memory ofile)))

;                                  -----
;                                    11
;                                  -----

(defun update_memory (token)
  "This function tacks on the token to the beginning of relevant memory
   structure. Token type is determined from the structure type of token." 
  (typecase token
    (scratch_pad (push (coerce_wm token) *episodes*)) 
    (episode (push token *episodes*)) 
    (causal_heuristic (push token *heuristics*)) 
    (causal_hypothesis (push token *hypos*)) 
    (t nil)))

;                                  -----
;                                    12
;                                  -----

(defun coerce_wm (wm)
  "This function converts scratch_pad into episode structure and returns the
   result." 
  (let ((ep (make-episode)))
    (setf (episode-name ep) (scratch_pad-name wm))
    (setf (episode-event_list ep) (scratch_pad-event_list wm))
    (setf (episode-temporal_relations ep) (scratch_pad-temporal_relations wm))
    (setf (episode-text ep) (scratch_pad-text wm))
    ep))

;                                  -----
;                                    13
;                                  -----

(defun create_a_new_episode ()
  "This function creates and returns an instance of an episode structure.
   Slots of episode are filled in from working memory." 
  (let ((new_episode (make-episode)))
    (setf (episode-name new_episode) (read_episode_name))
    (setf (episode-event_list new_episode) (episode-event_list
                                            *working_memory*))
    (setf (episode-temporal_relations new_episode) (analyze_tense
                                                    (episode-event_list
                                                     new_episode)))))

;                                  -----
;                                    14
;                                  -----

(defun find_episode (name)
  "This function returns the episode from LTM with the required name." 
  (find name *episodes* :test #'equal :key #'episode-name))

;                                  -----
;                                    15
;                                  -----

(defun find_hypothesis (name)
  "This function returns a causal hypothesis in the procedural LTM with the
   given name." 
  (find name *hypos* :test #'equal :key #'causal_hypothesis-name))

;                                  -----
;                                    16
;                                  -----

(defun read_episode_name ()
  (format t "~%      Enter episode name : ") 
  (cond ((unique_episode_name *episodes* (read))) (t (read_episode_name))))

;                                  -----
;                                    17
;                                  -----

(defun read_hypothesis_name ()
  (format t "~%      Enter hypothesis name : ") 
  (cond ((unique_hypo_name *hypos* (read))) (t (read_hypothesis_name))))

;                                  -----
;                                    18
;                                  -----

(defun unique_episode_name (e_list name)
  (cond ((null e_list) name)
        ((equal name (episode-name (car e_list))) nil)
        (t (unique_episode_name (cdr e_list) name))))

;                                  -----
;                                    19
;                                  -----

(defun unique_hypo_name (h_list name)
  (cond ((null h_list) name)
        ((equal name (causal_hypothesis-name (car h_list))) nil)
        (t (unique_hypo_name (cdr h_list) name))))

;                                  -----
;                                    20
;                                  -----

(defun episode_names (stm_struct)
  "This function returns the names of episodes in stm_struct." 
  (mapcar #'(lambda (ep) (car ep)) (stm-episode_list stm_struct)))
