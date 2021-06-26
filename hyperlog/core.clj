(ns interpreter.core
  (:require [clojure.edn :as edn]
            [clojure.walk :as walk]
            [interpreter.tools :as tools])
  (:import java.io.StringWriter))

;; Users are only allowed to perform operations listed in this set.
;(def allowed-operations '#{+ - let letfn foo * do fn})

;; Users are also allowed to use lists and numbers
(defn allowed? [x allowed-operations]
  (and (or (not (list? x))  (contains? allowed-operations (first x)))

   (or (list? x) ;; we mist allow lists since Clojure code is made of lists
       (number? x)
       (vector? x)
       (string? x)
       (keyword? x)
       (contains? allowed-operations x)))
 )

(defn validate! [parsed-code allowed-operations]
  (walk/postwalk (fn [x] (if (allowed? x allowed-operations)
                          x
                          (throw (ex-info "Unknown identifier" {:value x}))))
                 parsed-code))

;; This is safe as long as `allowed-operations` do not list anything sensitive
(defn eval-script [code allowed-operations]
  (let [out (new java.io.StringWriter)]
    (binding [*out* out]
      {:result (-> (edn/read-string code) ;; read safely
                   (validate! allowed-operations)            ;; stop on forbidden operations or literals
                   (eval)                 ;; run
                   )
       :out    (str out)})))


(defn load-tools! []
  (require '[interpreter.tools :as tools]))

;; (eval-script "(+ 1 (tools/cos 1))")
