(ns code-signal.core-test
  (:require [clojure.test :refer :all]
            [code-signal.core :refer :all]))



(defn calcApples
  [f k]
  (reduce + (map #(* % %) (filter f (range (inc k)))))
  )

(defn appleBoxes
  [k]
  (- (calcApples even? k) (calcApples odd? k))
  )

(deftest appleBoxesTest
  (testing appleBoxes)
  (is (= 0 (appleBoxes 0)))
  (is (= -1 (appleBoxes 1)))
  (is (= 3 (appleBoxes 2)))
  )


(defn addNoCarryTimesPowerOF10
  [x y tenPower]
  (* (mod (+ x y) 10) (int (Math/pow 10 tenPower))
  ))

(defn divBy10
  [x]
  (quot x 10))


(defn additionWithoutCarrying
  ([param1 param2] (additionWithoutCarrying param1 param2 0))
  ([param1 param2 tenPower]
   (if (and (zero? param1) (zero? param2))
     0
    (+ (addNoCarryTimesPowerOF10 param1 param2 tenPower)
      (additionWithoutCarrying (divBy10 param1) (divBy10 param2) (inc tenPower)))
   )
  ))


(deftest additionWithoutCarryingTest
  (testing additionWithoutCarrying)
  (is (= 0 (additionWithoutCarrying 0 0)))
  (is (= 1 (additionWithoutCarrying 0 1)))
  (is (= 11 (additionWithoutCarrying 10 1)))
  (is (= 8 (additionWithoutCarrying 9 9)))
  (is (= 40 (additionWithoutCarrying 20 20)))
  )



(deftest addNoCarryTimesPowerOF10Test
  (testing addNoCarryTimesPowerOF10)
  (is (= 0 (addNoCarryTimesPowerOF10 0 0 0)))
  (is (= 2 (addNoCarryTimesPowerOF10 1 1 0)))
  (is (= 0 (addNoCarryTimesPowerOF10 5 5 2)))
  (is (= 8000 (addNoCarryTimesPowerOF10 9 9 3)))
  )


;;;;;;;;;;;;;;


(def ^:const halfPi (/ Math/PI 2))
(def ^:const negHalfPi (- halfPi))


(defn turn
  [direction students]
  (let [
        goodTurn {\L negHalfPi \R halfPi \A 0}
        badTurn {\L halfPi \R negHalfPi \A 0}
        ]
    [(+ (first students) (goodTurn direction)) (+ (second students) (badTurn direction)) ]
    ))


(defn closeEnough
  [x y]
  (<= (Math/abs (- x y)) 0.00001))


(defn directionSame
  [directions]
  (let [cosi (map #(Math/sin %) directions)]
    (if (closeEnough (first cosi) (second cosi))
      1
      0)
    )
  )

(defn lineUp
  ([commands] (lineUp (seq commands) [0 0] ))
  ([commands students]
   (if (or (nil? commands) (empty? commands))
     0
      (let [turnedStudents (turn (first commands) students)]
        (+ (directionSame turnedStudents) (lineUp (rest commands) turnedStudents) )
        )
     )
   )
  )


(deftest lineUpTest
  (testing lineUp)
  (is (zero? (lineUp "")))
  (is (= 1 (lineUp "A")))
  (is (= 0 (lineUp "L")))
  (is (= 0 (lineUp "R")))
  (is (= 2 (lineUp "AA")))
  (is (= 1 (lineUp "LL")))
  (is (= 3 (lineUp "LLARL")))
  )


(deftest turnTest
  (testing turn)
  (is (= [0 0] (turn \A [0 0])))
  (is (= [(- halfPi) halfPi] (turn \L [0 0])))
  (is (= [halfPi (- halfPi)] (turn \R [0 0])))
  (is (= [halfPi (- halfPi)] (turn \R [0 0])))
  )


(deftest directionSameTest
  (testing directionSame)
  (is (= 1 (directionSame [ halfPi halfPi ])))
  (is (= 1 (directionSame [ 0 (* 4 halfPi) ])))
  (is (= 1 (directionSame [ halfPi (* 5 halfPi)])))
  (is (= 0 (directionSame [ halfPi (- halfPi)])))
  )




(defn magicalWell
  [a b n]
  (let [fromA (drop a (range))
        fromb (drop b (range))]
    (reduce + (take n (map * fromA fromb)))
    )
  )

(deftest magicalWellTest
  (testing magicalWell)
  (is (= 0 (magicalWell 0 0 0)))
  (is (= 1 (magicalWell 1 1 1)))
  (is (= 8 (magicalWell 1 2 2)))

  )


(defn countSumOfTwoRepresentations2 [n l r]
  (max 0 (inc (- (quot n 2) (max (- n r) l)))))

(deftest countSumOfTwoRepresentations2Test
  (testing countSumOfTwoRepresentations2)
  (is (= 0 (countSumOfTwoRepresentations2 10 9 11)))
  (is (= 1 (countSumOfTwoRepresentations2 2 0 1)))
  (is (= 1 (countSumOfTwoRepresentations2 6 3 3)))
  ;(is (= 2 (countSumOfTwoRepresentations2 6 2 4)))
  )


(defn leastFactorial
  ([n] (leastFactorial n 1 1))
  ([n factorial factor]
   (if (>= factorial n)
     factorial
     (leastFactorial n (* factorial factor) (inc factor))
   )
  ))

(deftest leastFactorialTest
  (testing leastFactorial)
  (is (= 1 (leastFactorial 1)))
  (is (= 6 (leastFactorial 5)))
  (is (= 120 (leastFactorial 25)))
  )



(defn mirrorBits
  ([a] (mirrorBits a 0))
  ([a b]
   (if (zero? a)
     b
     (mirrorBits (bit-shift-right a 1) (bit-or (bit-shift-left b 1) (bit-and 1 a ))))
   )
  )


(deftest mirrorBitsTest
  (testing mirrorBits)
  (is (= 0 (mirrorBits 0)))
  (is (= 1 (mirrorBits 2)))
  (is (= 1 (mirrorBits 8)))
  (is (= 67 (mirrorBits 97)))
  )

(defn intToCountOfOneBits
  [a]
  (if (zero? a)
    0
    (+ (bit-and 1 a) (intToCountOfOneBits (bit-shift-right a 1)))
    )
  )

(defn rangeBitCount
  [a b]
  (reduce + (map intToCountOfOneBits (range a (inc b))))
  )


(deftest rangeBitCountTest
  (testing rangeBitCount)
  (is (= 0 (rangeBitCount 0 0)))
  (is (= 1 (rangeBitCount 0 1)))
  (is (= 2 (rangeBitCount 0 2)))
  (is (= 4 (rangeBitCount 7 8)))
  )



(deftest intToCountOfOneBitsTest
  (testing intToCountOfOneBits)
  (is (= 0 (intToCountOfOneBits 0)))
  (is (= 1 (intToCountOfOneBits 1)))
  (is (= 2 (intToCountOfOneBits 3)))
  (is (= 1 (intToCountOfOneBits 4)))
  )





(defn safetyShift
  [i s]
  (if (nil? i)
    0
    (bit-shift-left i s))
  )

(defn arrayPacking
  [[a b c d & rest]]
  (bit-or (safetyShift d 24) (safetyShift c 16) (safetyShift b 8) a)
  )

(deftest arrayPackingTest
  (testing arrayPacking)
  (is (= 0 (arrayPacking [0])))
  (is (= 1 (arrayPacking [1])))
  (is (= 256 (arrayPacking [0 1])))
  )



(defn metroCard [lastNumberOfDays]
  (if (= 31 lastNumberOfDays)
    [28, 30, 31]
    [31])
  )

(deftest metroCardTest
  (testing metroCard)
  (is (= [31] (metroCard 30)))
  (is (= [28, 30, 31] (metroCard 31)))
  (is (= [31] (metroCard 28)))

  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn willYou
  [young beautiful loved]
  (or (and young beautiful (not loved))
      (and loved (or (not young) (not beautiful))))
  )


(deftest willYouTest
  (testing willYou)
  (is (not (willYou true true true)))
  (is (willYou true false true))
  (is (not (willYou false false false)))
  (is (willYou true true false))
  (is (willYou true false true))
  )



(defn tennisSet [score1 score2]
  (let [maxScore (max score1 score2)
        minScore (min score1 score2)
        diff (- maxScore minScore)]
    (or (and (= maxScore 6) (> 5 minScore))
        (and (= maxScore 7) (<= 1 diff 2))
        )
    ))


(deftest tennisSetTest
  (testing tennisSet)
  (is (not (tennisSet 0 0)))
  (is (tennisSet 3 6))
  (is (not (tennisSet 8 5)))
  (is (not (tennisSet 6 5)))
  (is (not (tennisSet 7 7)))
  (is (tennisSet 7 5))
  )


(defn addStars
  [inputString]
  (apply str "*" inputString "*")
  )

(defn addBorder [picture]
  (let [ allStar (list (apply str (repeat (+ 2 (count (first picture))) "*")))
        addedStars (flatten (map addStars picture))]
    (flatten (list allStar addedStars allStar)))
  )



(deftest addBorderTest
  (testing addBorder)
  (is (= ["***" "*a*" "***"] (addBorder ["a"])))
  (is (= ["***" "*a*" "*b*" "***"] (addBorder ["a" "b"])))
  (is (= ["****" "*ab*" "*bc*" "****"] (addBorder ["ab" "bc"])))
  )



(defn alternatingSums
  [lst]
  (list (reduce + (take-nth 2 lst)) (reduce + (take-nth 2 (rest lst))))
  )





(deftest alternatingSumsTest
  (testing alternatingSums)
  (is (= [1 2] (alternatingSums [1 2])))
  (is (= [1 0] (alternatingSums [1])))
  (is (= [4 2] (alternatingSums [1 2 3])))
  (is (= [4 6] (alternatingSums [1 2 3 4])))
  (is (= [9 6] (alternatingSums [1 2 3 4 5])))
  )




(defn substringAfterFirstCloseParen
  [inputString]
  (apply str (rest (drop-while #(not= \) %) inputString))))


(defn subStringUpToFirstCloseParen
  [inputString]
  (apply str (take-while #(not= \) %) inputString)))

(defn reverseStringAfterMatchingOpenParen
  [inputString]
  (let [indexOfLastOpen (clojure.string/last-index-of inputString \()]
    (if (nil? indexOfLastOpen)
      nil
      (apply str (subs inputString 0 indexOfLastOpen)
           (reverse (subs inputString (inc indexOfLastOpen)))
           )
    )
  ))


(defn reverseContentsOfFirstClosedParenPair
  [inputString]
  (let [afterCloseParen (substringAfterFirstCloseParen inputString)
        uptoFirstCloseParen (subStringUpToFirstCloseParen inputString)]
    (if (not (clojure.string/includes? uptoFirstCloseParen "("))
      inputString
      (str (reverseStringAfterMatchingOpenParen uptoFirstCloseParen) afterCloseParen))
    ))


(defn reverseInParentheses
  [inputString]
  (let [swappedString (reverseContentsOfFirstClosedParenPair inputString)]
    (if (= inputString swappedString)
      inputString
      (reverseInParentheses swappedString)
    )
  ))

(deftest substringAfterFirstCloseParenTest
  (testing substringAfterFirstCloseParen)
  (is (= "" (substringAfterFirstCloseParen "")))
  (is (= "" (substringAfterFirstCloseParen "ab)")))
  (is (= "c" (substringAfterFirstCloseParen "ab)c")))
  (is (= "c)d" (substringAfterFirstCloseParen "ab)c)d")))
  )

(deftest subStringUpToFirstCloseParenTest
  (testing subStringUpToFirstCloseParen)
  (is (= "abc" (subStringUpToFirstCloseParen "abc")))
  (is (= "" (subStringUpToFirstCloseParen ")")))
  (is (= "a" (subStringUpToFirstCloseParen "a)b)")))
  )

(deftest reverseStringAfterMatchingOpenParenTest
  (testing reverseStringAfterMatchingOpenParen)
  (is (= nil (reverseStringAfterMatchingOpenParen "")))
  (is (= "a" (reverseStringAfterMatchingOpenParen "(a")))
  (is (= "ab" (reverseStringAfterMatchingOpenParen "a(b")))
  (is (= "abc" (reverseStringAfterMatchingOpenParen "a(cb")))
  (is (= "a(xybc" (reverseStringAfterMatchingOpenParen "a(xy(cb")))
  )

(deftest reverseInParenthesesTest
  (testing reverseInParentheses)
  (is (= "a" (reverseInParentheses "a")))
  (is (= "a" (reverseInParentheses "(a)")))
  (is (= "ab" (reverseInParentheses "(ba)")))
  (is (= "ab" (reverseInParentheses "(a)(b)")))
  (is (= "abcd" (reverseInParentheses "(ba)(dc)")))
  (is (= "abcd" (reverseInParentheses "(d(bc)a)")))
  (is (= "abcdef" (reverseInParentheses "(d(bc)a)(fe)")))
  )

(deftest reverseContentsOfFirstClosedParenPairTest
  (testing reverseContentsOfFirstClosedParenPair)
  (is (= "a" (reverseContentsOfFirstClosedParenPair "a")))
  (is (= "a" (reverseContentsOfFirstClosedParenPair "(a)")))
  (is (= "ca" (reverseContentsOfFirstClosedParenPair "c(a)")))
  (is (= "cad" (reverseContentsOfFirstClosedParenPair "c(a)d")))
  (is (= "cad(ef)" (reverseContentsOfFirstClosedParenPair "c(a)d(ef)")))
  (is (= "c(abd(ef))" (reverseContentsOfFirstClosedParenPair "c((ba)d(ef))")))
  )



;(defn reverseString
;  [inputString]
;  (apply str (reverse inputString)))
;
;
;(defn getUntilNextCloseParen
;  [inputString]
;  (take-while #(not= % \)) inputString)
;  )
;
;(deftest getUntilNextCloseParenTest
;  (testing getUntilNextCloseParen)
;  (is (= [\a] (getUntilNextCloseParen "a")))
;  (is (= [\( \a] (getUntilNextCloseParen "(a)")))
;  (is (= [\( \( \a \b ] (getUntilNextCloseParen "((ab)c)")))
;  )
;
;(defn getReversedCharsInsideOfParens
;  [inputString upToLastCloseParen]
;  (reverseString (drop (inc (clojure.string/last-index-of inputString \()) upToLastCloseParen)))
;;
;;(testdef getReversedCharsInsideOfParensTest
;;         )
;
;
;(defn getStringUpToLastOpenParen
;  [upToLastCloseParen]
;  (apply str (take (clojure.string/last-index-of (apply str upToLastCloseParen) \()  upToLastCloseParen)))
;
;
;(deftest getStringUpToLastOpenParenTest
;  (testing getStringUpToLastOpenParen)
;  (is (= "a" (getStringUpToLastOpenParen (getUntilNextCloseParen "a(cb"))))
;  (is (= "a(d" (getStringUpToLastOpenParen (getUntilNextCloseParen "a(d(cb"))))
;  )
;
;
;(defn reverseStringInsideParens
;  [inputString]
;  (let [upToNextCloseParen (getUntilNextCloseParen inputString)
;        afterNextCloseParen (rest (drop-while #(not= % \)) inputString))
;        ]
;    (prn upToNextCloseParen "|" afterNextCloseParen)
;    (if (= (apply str upToNextCloseParen) inputString)
;      inputString
;       (apply str
;              (getStringUpToLastOpenParen upToNextCloseParen)
;              (getReversedCharsInsideOfParens upToNextCloseParen)
;              afterNextCloseParen)
;      )
;  ))
;
;(deftest getStringInsideParensTest
;  (testing reverseStringInsideParens)
;  (is (= "a" (reverseStringInsideParens "a")))
;  (is (= "a" (reverseStringInsideParens "(a)")))
;  (is (= "abc" (reverseStringInsideParens "(ba)c")))
;  (is (= "abc" (reverseStringInsideParens "a(cb)")))
;  (is (= "abc(de)" (reverseStringInsideParens "a(cb)(ed)")))
;  ;(is (= "(bac)" (reverseStringInsideParens "((ab)c)")))
;  )


;(let [split (str/split inputString #"\(" 2)
;      secondSplit (second split)
;      lastParenIndex (str/last-index-of secondSplit \))
;      inParens (take lastParenIndex secondSplit)
;      afterLastClose (drop (inc lastParenIndex) secondSplit)]
;  (if (= 1 (count split))
;    inputString
;    (apply str (first split)
;           (reverseString (reverseInParentheses inParens))
;           afterLastClose)
;    ))


;
;(deftest getStringAfterLastCloseParenTest
;  (testing getStringAfterLastCloseParen)
;  (is (= "b" (getStringAfterLastCloseParen "a()b")))
;  (is (= "bc" (getStringAfterLastCloseParen "a()a)bc")))
;  )
;
;
;
;(deftest splitStringOnFirstParenTest
;  (testing splitStringOnFirstParen)
;  (is (= ["a"] (splitStringOnFirstParen "a")))
;  (is (= ["" "a"] (splitStringOnFirstParen "(a")))
;  (is (= ["b" "a"] (splitStringOnFirstParen "b(a")))
;  (is (= ["b" "a(c"] (splitStringOnFirstParen "b(a(c")))
;  )
;
;(defn getStringAfterLastCloseParen
;  [inputString]
;  (apply str (nthrest inputString (inc (clojure.string/last-index-of inputString \)))))
;  )
;
;(defn getStringUpToLastCloseParen
;  [inputString]
;  (apply str (take (clojure.string/last-index-of inputString \)) inputString))
;  )
;
;(deftest getStringUpToLastCloseParenTest
;  (testing getStringUpToLastCloseParen)
;  (is (= "a" (getStringUpToLastCloseParen "a)")))
;  (is (= "a)" (getStringUpToLastCloseParen "a))")))
;  (is (= "a)" (getStringUpToLastCloseParen "a))a")))
;  (is (= "a)b" (getStringUpToLastCloseParen "a)b)a")))
;  )


;(defn splitOnLastOpenParen
;  [inputString upToLastCloseParen]
;  (let [lastOpenParenIndex (clojure.string/last-index-of inputString \()
;        splitString (split-at lastOpenParenIndex upToLastCloseParen)]
;     [(first splitString) (reverse (rest (second splitString)))]
;    )
;  )
;
;(deftest splitOnLastOpenParenTest
;  (testing splitOnLastOpenParen)
;  (is (= [[\a] [\c \b]] (splitOnLastOpenParen "a(bc)" (getUntilNextCloseParen "a(bc)"))))
;  (is (= [[\a] [\c \b]] (splitOnLastOpenParen "a(bc)d" (getUntilNextCloseParen "a(bc)d"))))
;  (is (= [[] [\c \b]] (splitOnLastOpenParen "(bc)" (getUntilNextCloseParen "(bc)"))))
;  )