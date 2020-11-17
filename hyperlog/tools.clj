(ns interpreter.tools)

(defn cos [x]
  (java.lang.Math/cos x))

(defn version []
  "Interpreter v0.1")

(defn print-version[]
  (println (version)))
