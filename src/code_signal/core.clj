(ns code-signal.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(require '[clojure.string :as str])

(defn digits [x]
  (if (= x 0)
    ()
    (cons (mod x 10) (digits (quot x 10))))
  )

(defn sumOfSquareDigits
  [x]
  (reduce + (map #(* % %) (digits x))))

(defn squareDigitsSequence [a0]
  (loop [x a0
         xs (set ())]
    (if (contains? xs x)
      (inc (count xs))
      (recur (sumOfSquareDigits x) (conj xs x)))))


(let [a 7]
  (take 5 (iterate #(mod % 2) a)))

(let [a 4
      p 2]
  (cond (= p (/ a p)) true
        :else false
        ))



(let [a [1 3 5]]
  (reduce + (map (fn [[a b]] (dec (- b a))) (partition 2 1 a)))
  )


(let [a [1 2 3 5 6]
      mid (quot (count a) 2)
      [f s] (split-at mid a)]
  (if (= (count f) (count s))
    (concat (drop-last f) [(+ (last f) (first s))] (drop 1 s))
    a)
  )


(let [a [2 1 1 2]]
  (let [c (count a)]
    (take (if (odd? c) 1 2) (drop (int (/ (dec c) 2)) a))))

(if (odd? (count [2 1 1 2])) 1 2)

(int (/ (dec 5) 2))


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