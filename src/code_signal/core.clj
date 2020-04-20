(ns code-signal.core)

(require '[clojure.string :as string])



(defn digits [x]
  (if (= x 0)
    ()
    (cons (mod x 10) (digits (quot x 10))))
  )

(defn sum-digits
  [x]
  (reduce + (digits x)))

(def memo-sum-digits
  (memoize sum-digits))

(defn b-in-segment?
  [a b]
  (let [x (memo-sum-digits a)
        lower (- a x)
        upper (+ a x)]
    (<= lower b upper))
  )

(defn comfortable-with-each-other?
  [[a b]]
  (if (and (not (= a b)) (b-in-segment? a b) (b-in-segment? b a))
    1
    0)
  )


(defn combinations
  [l r]
  (if (= l r)
    []
    (let [list-of-l (repeat l)
          up-to-r (range (inc l) (inc r))]
      (concat (map vector list-of-l up-to-r) (combinations (inc l) r))
      ))
  )

(defn comfortableNumbers [l r]
  (let [pairs (combinations l r)]
    (reduce + (map comfortable-with-each-other? pairs)))
  )




(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(string/split "/home//foo/" #"/+")


(string/replace "/home//foo/" #"^/" "")

(string/split (string/replace "/home//foo/" #"^/" "") #"/+")

(defn get-k-and-string
  [x]
  (let [split-on-open (string/split x #"\[")]
    [(read-string (first split-on-open))
     (string/join (drop-last (second split-on-open)))]
    ))

(defn replicateString [x]
  (let [k (get-k-and-string x)]
    (string/join (take (first k) (repeat (second k))))
    ))

(defn replicateAndReplaces
  [s match]
  (string/replace s match (replicateString match)))


(defn decodeString [s]
  (let [matches (re-find #"\d\[.*?\]" s)]
    ;(prn s "-matchs-" matches)
    (if matches
      (replicateAndReplaces s matches)
      s
      ))

  )



(let [code "1[x]1[x]"
      matcher (re-matcher #"\d\[.*?\]" code)]
  (re-find matcher)
  )

(defn safe-pop
  [xs]
  (if (empty? xs)
    []
    (pop xs)))

(defn processDir
  [[next & _]
   processedDirs]
  (case next
    ".." (safe-pop processedDirs)
    "." processedDirs
    (conj processedDirs next)
    )
  )


(defn split-path
  [path]
  (string/split (string/replace path #"^/+" "") #"/+")
  )

(defn simplifyPath
  ([path]
   (loop [splitPath (split-path path)
          processed []]
     (if (empty? splitPath)
       (str "/" (string/join "/" processed))
       (recur (rest splitPath) (processDir splitPath processed)))
     )))




(defn kthLargestElement [nums k]
  (nth (sort > nums) (dec k)))




(defn pagesNumberingWithInk [c n]
  (loop [current c
         numberOfDigits n
         lastPrinted -1]
    (let [digitsInCurrent (count (digits current))]
      (if (< numberOfDigits digitsInCurrent)
        lastPrinted
        (recur (inc current) (- numberOfDigits digitsInCurrent) current)
        ))
    ))



(defn subSequenceAddsToTotal
  [max sequenceStart]
  (reduce (fn f [a b]
            (let [sum (+ a b)]
              (if (>= sum max)
                (reduced (if (= sum max) 1 0))
                sum)))
          (range sequenceStart max)))


(defn isSumOfConsecutive2
  [n]
  (reduce + (map #(subSequenceAddsToTotal n %) (range 1 (dec n))))
  )


(reduce + (map #(subSequenceAddsToTotal 10 %) (range 1 (dec 10))))
(reduce + (map #(subSequenceAddsToTotal 9 %) (range 1 (dec 9))))



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