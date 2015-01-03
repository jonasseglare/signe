(ns signe.core
  (:require [clojure.data :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Funcall
(defn- remove-arg [args index]
  (assert (number? index))
  (let [argv (vec args)]
    (concat (subvec argv 0 index) (subvec argv (+ 1 index)))))

(defrecord Funcall [fn args])

(defn Funcall? [x]
  (instance? Funcall x))

(defn make-root []
  (Funcall. nil nil))

(def obj (make-root))

(defn is-root? [x]
  (if (Funcall? x)
    (nil? (:fn x))))

(defn- find-funcall-index [args]
  (loop [remaining args
         counter 0]
    (if (empty? remaining) nil
        (if (Funcall? (first remaining))
          counter
          (recur (rest remaining)
                 (+ 1 counter))))))

(defn call [fn & args]
  (Funcall. fn args))

;; (defn is-root-obj? [expr]
;;   (println (str "EXPR = " expr))
;;   (= obj expr))

(defn make-calls [expr]
  (cond
   (seq? expr) (let [fun (first expr)
                     args (rest expr)
                     evaled-args (map make-calls args)]
                 (if (some (fn [x] (not (nil? x))) evaled-args) 
                   `(call ~fun ~@(map (fn [a b] (if a a b))
                                        evaled-args
                                        args))))
   (keyword? expr) (if (= :obj expr) obj)))
   ;(is-root-obj? expr) expr))

; Facility macro, that simplifies the call chain:
; Instead for for instance writing (call + 1 (call / obj 2)),
; write (call-chain (+ 1 (/ obj 2)))
;
; EXAMPLES
;; signe.core> (call-chain (+ 1 obj (* 1 2)))
;; #signe.core.Funcall{:fn #<core$_PLUS_ clojure.core$_PLUS_@790cf1cd>, :args (1 #signe.core.Funcall{:fn nil, :args nil} 2)}
;; signe.core> (macroexpand '(call-chain (+ 1 obj (* 1 2))))
;; (signe.core/call + 1 obj (* 1 2))
;; signe.core> (call-chain obj)
;; #signe.core.Funcall{:fn nil, :args nil}
;; signe.core> (macroexpand '(call-chain obj))
;; obj
;; signe.core> (resolve (macroexpand '(call-chain obj)))
;; #'signe.core/obj
;; signe.core> 
(defmacro call-chain [expr]
  (make-calls expr))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Accessor
(defrecord Accessor [fn args main-arg-pos])

(defn Accessor? [x] (instance? Accessor x))


; Takes a Funcall and turns it into a list of Accessor.
(defn make-accessor-list [funcall]
  (assert (Funcall? funcall))
  (loop [func funcall
         result []]
    (let [args (:args func)]
      (if (or (is-root? func) (empty? args))
        result
        (let [index (find-funcall-index args)]
          (assert (not (= -1 index)))
          (recur (nth args index)
                 (cons (Accessor. (:fn func)
                                  (remove-arg args index)
                                  index)
                       result)))))))

(defn apply-accessor [accessor x]
  (assert (Accessor? accessor))
  (if (is-root? accessor) x
      (let [argv (vec (:args accessor))
            all-args (concat (subvec argv 0 (:main-arg-pos accessor))
                               [x]
                               (subvec argv (:main-arg-pos accessor)))]
          (apply (:fn accessor) all-args))))
      
(defn apply-accessors [all-accessors x]
  (loop [accessors all-accessors
         result [x]
         y x]
    (if (empty? accessors)
      result
      (let [next-value (apply-accessor (first accessors) y)]
        (recur (rest accessors)
               (conj result next-value)
               next-value)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AccessorMap
(defrecord AccessorMap [map update-functions])

(defn AccessorMap? [x]
  (instance? AccessorMap x))

(defn make-empty-accessor-map []
  (AccessorMap. {} []))

(defn insert [accessor-map accessor-vec update-function]
  (assert (or (AccessorMap? accessor-map) (nil? accessor-map)))
  (assert (or (seq? accessor-vec) (vector? accessor-vec)))
  (assert (fn? update-function))
  (let [dst (if (nil? accessor-map)
              (make-empty-accessor-map)
              accessor-map)]
    (if (empty? accessor-vec)
      (assoc dst
        :update-functions
        (conj (:update-functions dst) update-function))
      (assoc dst
        :map
        (let [dst-map (:map dst)
              key (first accessor-vec)]
          (assoc dst-map
            key
            (insert (get dst-map key)
                    (rest accessor-vec)
                    update-function)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Monitor

; Monitors a model, wrapped in an atom.
(defrecord Monitor [model accessor-map])

(defn Monitor? [x]
  (instance? Monitor x))

(defn get-model [monitor]
  (assert (Monitor? monitor))
  (:model monitor))

(defn get-accessor-map [monitor]
  (assert (Monitor? monitor))
  (:accessor-map monitor))



(defrecord AmapChanges [map remove updaters])

(defn AmapChanges? [x]
  (instance? AmapChanges x))

(defn make-node-to-remove []
  (AmapChanges. {} true []))

(defn make-updaters-to-remove [updaters]
  (AmapChanges. {} false updaters))

(defn nothing-to-remove? [am]
  (if (not (nil? am))
    (do (assert (AmapChanges? am))
        (= (AmapChanges. {} false []) am))))
      
(defn insert-amap-changes [map key am]
  (assert (map? map))
  (if (nothing-to-remove? am) map
      (assoc map key am)))


(declare call-updaters)

; Returns a map with stuff to remove
(defn- call-updaters-sub [accessor-map old-model new-model
                          old-value-list new-value-list]
                                        ; Recursive calls on all keys
  (assert (AccessorMap? accessor-map))
  (let [map (:map accessor-map)]
    (loop [k (keys map)
           result-map {}]
      (if (empty? k)
        result-map
        (let [key (first k)]
          (assert (Accessor? key))
          (recur (rest k)
                 (insert-amap-changes result-map key
                                      (call-updaters (get map key)
                                                    (apply-accessor key old-model)
                                                    (apply-accessor key new-model)
                                                    (conj old-value-list old-model)
                                                    (conj new-value-list new-model)))))))))

; Returns a vector with functions to remove
(defn- call-update-functions [accessor-map old-value-list new-value-list]
  (loop [functions (:update-functions accessor-map)
         result []]
    (if (empty? functions)
      result
      (let [f (first functions)
            out (f old-value-list new-value-list)]
        (recur (rest functions)
               (if (= :detach out)
                 (conj result f)
                 result))))))



(defn empty-accessor-map? [x]
  (if (AccessorMap? x)
    (and (empty? (:map x)) (empty? (:update-functions x)))))

; Returns nil if x is empty. Otherwise it returns x.
(defn clean-accessor-map [x]
  (if (not (empty-accessor-map? x)) x))

(defn call-updaters
  ([accessor-map old-model new-model] (call-updaters accessor-map old-model new-model [] []))
  ([accessor-map old-model new-model old-value-list new-value-list]
   (if (nil? accessor-map) nil
       (do ; The accessor-map is not nil.
         (assert (AccessorMap? accessor-map))
         (if (nil? new-model)
           (make-node-to-remove) ; <-- This node should be removed
           (if (= old-model new-model)
             nil ; <-- Nothing will be removed here.
             (let [functions-to-remove (call-update-functions accessor-map
                                                              (conj old-value-list old-model)
                                                              (conj new-value-list new-model))
                   new-sub-map (call-updaters-sub
                                accessor-map
                                old-model new-model old-value-list new-value-list)]
               (AmapChanges. new-sub-map false functions-to-remove))))))))


(defn assoc-or-remove [map key value]
  (if (nil? value)
    (dissoc map key)
    (assoc map key value)))

(defn remove-items [to-remove all-items]
  (filter (fn [x]
            (= -1 (.indexOf to-remove x)))
          all-items))
; (remove-items [3 9] [1 2 3 4 9 9 9 119])

  


(defn apply-amap-changes [amap-changes amap]
  (clean-accessor-map
   (if (nil? amap-changes) amap
       (do
        (assert (AmapChanges? (or false amap-changes)))
        (assert (AccessorMap? amap))
         (if (:remove amap-changes)
           nil ; Remove this node completely!
           (let [remaining-updaters (remove-items (:updaters amap-changes)
                                                  (:update-functions amap))
                 new-map (loop [keys (keys (:map amap-changes))
                                new-map (:map amap)]
                           (if (empty? keys) new-map
                               (let [key (first keys)
                                     result-sub (apply-amap-changes
                                                 (get (:map amap-changes) key)
                                                 (get (:map amap) key))]
                                 (recur (rest keys)
                                        (assoc-or-remove new-map key result-sub)))))]
             (AccessorMap. new-map remaining-updaters)))))))

(defn call-updaters-old [accessor-map old-model new-model]
  (apply-amap-changes
   (call-updaters accessor-map old-model new-model)
   accessor-map))


(defn perform-updates [monitor old-state new-state]

  ; Call all the updaters and obtain a list of operations that need to
  ; be performed on the accessor-map tree. These operations are delayed
  ; until after all updaters have been called, because the updaters themselves
  ; might change the accessor-map, for instance by calling make-monitor.
  (let [amap-changes (call-updaters (deref (get-accessor-map monitor))
                                    old-state
                                    new-state)]
    
    ; Quickly apply the changes to the tree.
    (swap! (get-accessor-map monitor)
           (fn [x] (apply-amap-changes amap-changes x)))))

          
(defn make-monitor [model]
  (assert (instance? clojure.lang.Atom model))
  (let [monitor (Monitor. model (atom (make-empty-accessor-map)))]
    (add-watch
     (get-model monitor)
     (gensym)
     (fn [key model-ref old-state new-state]
       (perform-updates monitor old-state new-state)))
    monitor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ModelChange. The object passed to an updater.
(defrecord ModelChange [old-values
                        new-values
                        state])

(defn ModelChange? [x]
  (instance? ModelChange x))

(defn get-old-values [model-change]
  (assert (ModelChange? model-change))
  (:old-values model-change))

(defn get-old-value [x]
  (last (get-old-values x)))

(defn get-new-values [model-change]
  (assert (ModelChange? model-change))
  (:new-values model-change))

(defn get-new-value [x]
  (last (get-new-values x)))

(defn get-state [x]
  (assert (ModelChange? x))
  (:state x))


(defn internal-state-from-args [args]
  (first args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main user function to attach
(defn only-register [monitor          ; Of type Monitor
                     funcall          ; The function calls to be applied to the model object.
                     updater          ; A function that takes a ModelChange. Returns
                                        ;   new internal state. If nil is returned, this
                                        ;   listener will be detached. SHOULD NOT TWEAK THE ATOM
                                        ;   THAT HOLDS THE MODEL!!! (that could in the worst case
                                        ;    lead to an infinite loop if done carelessly.)
                     & args]          ; Optional: Initial internal state. nil by default.
                                        ; RETURNS: A function that calls the updater on the
                                        ;          model.
  (let [initial-internal-state (internal-state-from-args args)]
    (assert (Monitor? monitor))
    (assert (Funcall? funcall))
    (assert (fn? updater))
    (let [internal-state (atom initial-internal-state)
          internal-updater (fn [old-values new-values]
                             (swap!
                              internal-state
                              (fn [s]
                                (updater (ModelChange. old-values new-values s))))
                             (deref internal-state)) ; <-- Return the new internal state.
          accessor-list (make-accessor-list funcall)]

      ; Add a path for this updater to the model
      (swap!
       (:accessor-map monitor)
       (fn [accessor-map]
         (insert accessor-map
                 accessor-list
                 internal-updater)))

      (fn [] (internal-updater
             nil
             (apply-accessors accessor-list (deref (:model monitor))))))))

(defn register-and-update [monitor funcall updater & args]
  ((only-register monitor
                  funcall
                  updater
                  (internal-state-from-args args))))





(defn remove-keys [all-keys full-map]
  (assert (or (map? full-map) (nil? full-map)))
  (loop [keys all-keys
         result full-map]
    (if (empty? keys)
      result
      (recur (rest keys)
             (dissoc result (first keys))))))
; (remove-keys [:rulle :signe] {:rulle 1, :signe 2, :malte 3})


(defn add-new-keys [all-keys key-to-value init-map]
  (loop [keys all-keys
         result init-map]
    (if (empty? keys) result
        (let [key (first keys)]
          (recur (rest keys)
                 (assoc result key (key-to-value key)))))))

; When working with maps
(defn register-map         [monitor    ; A monitor
                            funcall    ; Funcalls performed
                            child-instantiator  ; A function that takes as arguments:
                                        ;  - monitor
                                        ;  - funcall to access this object.
                                        ;  - key
                                        ;  - old values (only up to parent)
                                        ;  - new model values
                                        ; Returns the first state
                            updater    ; A function that performs updates. Takes
                                        ; as arguments a ModelChange object, with additional fields
                                        ; :state, :widget-map, :removed, :added. Should
                                        ; return :detach when it should be detached.
                                        ; The second argument
                                        ; is a list of all widgets that were removed and the third
                                        ; argument is a list with all widgets that were added.
                            & args]     ; Optional arg: initial-state. defaults to nil
  (let [initial-state (first args)
        updater-sub (fn [model-change]
                      (assert (ModelChange? model-change))
                       (let [new-value (get-new-value model-change)]
                         (if (nil? new-value) :detach
                             (do (assert (map? new-value))
                                 (let [[user-state state] (get-state model-change)
                                       [removed added unchanged]
                                       (diff (set (keys (get-old-value model-change)))
                                             (set (keys new-value)))
                                       new-map (add-new-keys
                                                  added
                                                  (fn [key]
                                                    (child-instantiator
                                                     monitor
                                                     (call (fn [x] (get x key)) funcall)
                                                     key
                                                     (get-old-values model-change)
                                                     (conj (get-new-values model-change)
                                                           (get new-value key))))
                                                  (remove-keys removed state))
                                       removed-widgets (map (fn [key] (get state key)) removed)
                                       added-widgets (map (fn [key] (get new-map key)) added)
                                       new-model-change (merge
                                                         model-change
                                                         {:state user-state,
                                                          :widget-map new-map,
                                                          :removed removed-widgets,
                                                          :added added-widgets})]
                                        ; Finally, call the user function
                                   (let [new-user-state (updater new-model-change)]
                                     [new-user-state new-map]))))))
        f2 (fn [mc] (updater-sub mc))]
    (register-and-update monitor funcall f2 [initial-state {}])))
