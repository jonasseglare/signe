(ns signe.examples
  (:require [signe.core :as mon])
  (:import [javax.swing JPanel JButton JFrame JLabel JTextField]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; EXAMPLE I : A counter example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; THE MODEL AND PRIMITIVES
(defrecord TestModel [counter])

(defn TestModel? [x]
  (instance? TestModel x))

(defn get-counter [x]
  (assert (TestModel? x))
  (:counter x))

(defn set-counter [x new-value]
  (assert (TestModel? x))
  (assoc x :counter new-value))

(defn increase-counter [x]
  (assert (TestModel? x))
  (set-counter x (+ 1 (get-counter x))))

(defn make-test-model []
  (TestModel. 0))


; For debugging
(defn show-message [message]
  (javax.swing.JOptionPane/showMessageDialog nil message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; A GUI
; A button attached to a counter.
; All logic to update the underlying model and to update the counter when the underlying model
; is changed is contained in this function.
(defn make-counter-button [monitor]
  (let [
        ; This is an object of type 'Funcall'.
        ; It tells the system
        ; what function calls that need to be applied to the domain model
        ; to get the data used by this widget. In our case, we are going to call
        ; the 'get-counter' function on the model. 'obj' denotes the model itself
        ; and is bound to a an empty Funcall object.
        funcall (mon/call-chain (get-counter :obj))

        ; This is the actual button instance that we create once, here.
        ; We don't fill it with any information, this is done later
        ; by the updater function.
        widget (JButton.)] 

    ; Attach a handler that is called when the button is clicked.
    (.addActionListener
     widget
     (proxy [java.awt.event.ActionListener] []
       (actionPerformed [e]
         ; Replace the domain model with a new version of the
         ; model where the counter has been increased.
         (swap!

          ; The atom pointing to the model
          (mon/get-model monitor)

          ; The operation performed on the model.
          (fn [x] (increase-counter x))))))
     

    ; Return a widget that is bound to updates in the model
    (mon/register-and-update
     monitor
     funcall

                                        ; This function will be called automatically whenever
                                        ; changes are detected to the underlying model
     (fn [model-change]
       (.setText widget
                 (str "Counter = "
                      (mon/get-new-value model-change))))
     
     nil) ; <-- Initial state. Not used.
    widget)) ; <-- This is the widget that this function is supposed to update
                 

(defn run-counter-gui-example []

  ; First create all the data
  ; that will make up the gui.
  (let [; This is an atom to wrap the domain model.
        state-atom (atom (make-test-model))

        ; Used to create widgets that will be updated. see the signe.core/make-widget
        ; function that uses a monitor.
        monitor (mon/make-monitor state-atom)

        ; The window that will be shown on the screen
        frame (JFrame.)

        ; A dumb panel where the button lives.
        panel (JPanel.)

        ; We need this monitor to make the button.
        ; The monitor also holds the model that we will modify
        ; by clicking the button
        button (make-counter-button monitor)]

    ; Put together the gui and show it.
    (.setTitle frame "Counter Application")
    (.setContentPane frame panel)
    (.add panel button)
    (.pack frame)
    (.show frame)))
    















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; EXAMPLE II : Second order polynomial root solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;; THE MODEL

(defrecord Polynomial [coefs])

; (Empty set) -> Polynomial
(defn make-second-order-polynomial
  ([] (Polynomial. [0 0 0]))
  ([a b c] (Polynomial. [a b c]))
  ([root-a root-b] (Polynomial. [1.0 (- (+ root-a root-b)) (* root-a root-b)])))

; Test if x is of type Polynomial
; ? -> boolean
(defn Polynomial? [x]
  (instance? Polynomial x))

; Get the polynomial coefficients
; Polynomial -> vector
(defn get-coefs [x]
  (assert (Polynomial? x))
  (:coefs x))

(defn get-coef [x index]
  (nth (get-coefs x) index))

(defn to-number-or-nil [x]
  (if (number? x) x
      (try
        (let [v (read-string x)]
          (if (number? v) v
              nil))
        (catch Exception e nil))))
; (to-number-or-nil nil)
; (to-number-or-nil "119")
; (to-number-or-nil "rulle")

(defn get-numeric-coef [x index]
  (to-number-or-nil (get-coef x index)))

(defn has-numeric-coefs? [x]
  (every? to-number-or-nil (get-coefs x)))

; Set the polynomial coefficients
; Polynomial x vector -> Polynomial
(defn set-coefs [x coefs]
  (assert (Polynomial? x))
  (assoc x :coefs coefs))

; Test if it is a second order polynomial
; ? -> boolean
(defn second-order-polynomial? [x]
  (if (Polynomial? x)
    (= 3 (count (get-coefs x)))))

; Set a coefficient
; Polynomial x number x number -> Polynomial
(defn set-coef [x index value]
  (assert (Polynomial? x))
  ;  (assert (number? index)) <-- Don't assert this, because it is in the gui
  ;  (assert (number? value))
  (set-coefs
   x
   (assoc (get-coefs x) index value)))

; Returns a vector of roots, or nil if no real roots.
; Polynomial -> vector
;
; OBS! If this accessor returns nil, then listeners will be detached. Therefore, return
; an empty vector in place of nil, since we don't want to detach listeners.
(defn calc-roots [poly]
  (assert (Polynomial? poly))
  (let [val (if (second-order-polynomial? poly)
              (if (has-numeric-coefs? poly)
                (let [a (get-numeric-coef poly 0)
                      b (get-numeric-coef poly 1)
                      c (get-numeric-coef poly 2)
                      R (- (* b b) (* 4 a c))]
                  (if (and (> a 0) (>= R 0)) ; There are two real roots
                    (let [a2 (* 2 a)
                          p (/ (- b) a2)
                          q (/ (Math/sqrt R) a2)]
                      [(- p q) (+ p q)])))))]
    (if (nil? val) []
        val)))
; (= [3.0 4.0] (calc-roots (make-second-order-polynomial 3.0 4.0)))  ; <-- Should evaluate to true



;;;;;;;;;;;;;;;;;;;;;;;;;;;; THE GUI

; Return an empty vector if no roots. Returning
; nil will detach the listeners.
(defn get-root [roots index]
  (if (empty? roots) []
      (roots index)))

; Responsible for editing coefficients in the polynomial
(defn make-coef-editor [monitor index]
  (let [coefs ["a" "b" "c"]
        panel (JPanel.)
        label (JLabel.)
        textfield (JTextField. "" 20)]
    (.add panel label)
    (.add panel textfield)
    (mon/register-and-update
     monitor
     (mon/call-chain (get-coef :obj index))
     (fn [model-change]
       (let [new-value (str (mon/get-new-value model-change))]
         (.setText textfield new-value)
         (.setText label (str (coefs index) " = " new-value)))))
    [textfield panel]))

; Displays a root of the polynomial
(defn make-root-label [monitor index]
  (let [labels ["r_0" "r_1"]
        label (JLabel.)]
    (mon/register-and-update
     monitor (mon/call-chain (get-root (calc-roots :obj) index))
     (fn [model-change]
       (let [new-value (mon/get-new-value model-change)]
         (.setText label (str (labels index) " = "
                              (if (number? new-value)
                                new-value
                                "(none)")
                              ". Updated " (mon/get-state model-change) " times."))
         (+ 1 (mon/get-state model-change))))
     0)
    label))

; Update the model with the new coefficients fetched from the text boxes
(defn make-set-button [monitor & args]
  (doto (JButton. "Set")
    (.addActionListener 
     (proxy [java.awt.event.ActionListener] []
       (actionPerformed [e]
         (swap!
          (mon/get-model monitor)
          (fn [model]
            (set-coefs
             model
             (vec
              (map
               (fn [widget]
                 (to-number-or-nil
                  (.getText widget)))
               args))))))))))
                      

; Open a gui window and return an atom to the model
(defn run-poly-gui []
  (let [model (atom (make-second-order-polynomial))
        monitor (mon/make-monitor model)
        frame (JFrame. "Polynomial Roots")
        panel (JPanel.)
        [t1 e1] (make-coef-editor monitor 0)
        [t2 e2] (make-coef-editor monitor 1)
        [t3 e3] (make-coef-editor monitor 2)]
    (.setContentPane frame panel)
    
    (doto panel
      ; Fields for the coefficients.
      (.add e1)
      (.add e2)
      (.add e3)

      ; A button to update the underlying model
      (.add (make-set-button monitor t1 t2 t3))

      ; Two labels to show the polynomial roots
      (.add (make-root-label monitor 0))
      (.add (make-root-label monitor 1)))
    
    (.pack frame)
    (.show frame)
    
    model)) ; <-- A model that can be seen from the repl


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; EXAMPLE III : Phone directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-directory []
  {})

(defn update-person [directory name phone]
  (assoc directory name phone))

(defn has-person? [directory name]
  (contains? directory name))

(defn get-phone [directory name]
  (get directory name))

(defn remove-person [directory name]
  (dissoc directory name))

(defn make-update-person-widget [monitor]
  (let [panel (JPanel.)
        name (JTextField. "" 20)
        phone (JTextField. "" 20)
        add-button (JButton. "Update")]
    (doto panel
      (.add (JLabel. "Name:"))
      (.add name)
      (.add (JLabel. "Phone:"))
      (.add phone)
      (.add add-button))
    (.addActionListener
     add-button
     (proxy [java.awt.event.ActionListener] []
       (actionPerformed [e]
         (swap! (mon/get-model monitor)
                (fn [dir] (update-person dir
                                         (.getText name)
                                         (.getText phone)))))))
    panel))

(defn make-person-widget [mon funcall key old-value new-values]
  (let [remove-button (JButton. "Remove")
        label (JLabel.)]
    (.addActionListener
     remove-button
     (proxy [java.awt.event.ActionListener] []
       (actionPerformed [e]
         (swap! (mon/get-model mon)
                (fn [model] (remove-person model key))))))
    (mon/register-and-update
     mon
     funcall
     (fn [model-change]
       (.setText
        label
        (str "Person(name = " key ", phone = "
             (mon/get-new-value model-change) ")"))))
    (doto (JPanel.)
      (.add label)
      (.add remove-button))))


(defn make-directory-view [monitor]
  (let [panel (JPanel.)]
    (mon/register-map
     monitor
     mon/obj

     ; Create a new widget when a person is added
     make-person-widget
     
     ; Update the parent widget
     (fn [model-change]
       
       ; When added or removed
       (when (not (and (= 0 (:added model-change))
                       (= 0 (:removed model-change))))

         ; Completely refresh the widget
         (.removeAll panel)
         (doseq [x (:widget-map model-change)]
           (.add panel (second x)))
         (.revalidate panel))))

    ; Return the panel containing the map.
    panel))

(defn run-phone-directory-example []
  (let [model (atom (make-directory))
        monitor (mon/make-monitor model)
        frame (JFrame. "Phone Directory")
        panel (JPanel.)]
    (mon/register-and-update
     monitor
     mon/obj
     (fn [mc] (.pack frame)))
    (.setContentPane frame panel)
    (doto panel
      (.add (make-update-person-widget monitor))
      (.add (make-directory-view monitor)))
    (.pack frame)
    (.show frame)
    model))
