(ns code-signal.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(require '[clojure.string :as str])


(let [a [0 1]
      l 0
      r 0]
  (concat (take l a ) (drop (inc r) a))
  )

(defn mdPoint
  [x y]
  (seq (* 3 x) (* 3 y))
  )



(<= (/ 100 40) 2)

(let [n 160
      tens 100]
  (cond (zero? n) 0
        (and (not= 0 (mod n tens)) (zero? (quot n tens))) -1
        :else 1)

   )


;(def candles 5)
;(def nubsToCandles (quot candles 2))
;(def remNubs (rem candles 2))
;(+ candles nubsToCandles )