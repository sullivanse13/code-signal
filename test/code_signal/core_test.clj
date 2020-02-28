(ns code-signal.core-test
  (:require [clojure.test :refer :all]
            [code-signal.core :refer :all]))

(deftest processDirTest
  (testing processDir)
  (is (= [""] (processDir [""] [])))
  (is (= ["" "a"] (processDir ["a"] [""])))
  (is (= [] (processDir [".."] [""])))
  (is (= [""] (processDir [".."] ["" "a"])))
  (is (= [""] (processDir ["" "home"] [])))
  (is (= ["" "home" "abc"] (processDir ["abc"] ["" "home"])))
  (is (= ["" "home"] (processDir ["."] ["" "home"])))
  )


(deftest simplifyPathTest
  (testing simplifyPath)
  (is (= "/" (simplifyPath "/")))
  (is (= "/a" (simplifyPath "/a")))
  (is (= "/a/b" (simplifyPath "/a/b")))
  (is (= "/a/b/c" (simplifyPath "/a/b/./c")))
  (is (= "/a" (simplifyPath "/a/b/..")))
  (is (= "/home/foo" (simplifyPath "/home//foo/")))
  (is (= "/home/foo" (simplifyPath "home//foo/")))
  (is (= "/a" (simplifyPath "///a///")))
  (is (= "/a" (simplifyPath "/a/./././././.")))
  (is (= "/" (simplifyPath "/../")))
  (is (= "/b" (simplifyPath "a/../../b/")))
  (is (= "/K/BruP/RMplU" (simplifyPath "/////..///K/BruP/RMplU/././")))

  )



(deftest kthLargestElementTest
  (testing kthLargestElement)
  (is (= 1 (kthLargestElement [1] 1)))
  (is (= 0 (kthLargestElement [0 1] 2)))
  (is (= 1 (kthLargestElement [0 1] 1)))
  (is (= 6 (kthLargestElement [7, 6, 5, 4, 3, 2, 1] 2)))
  (is (= 0 (kthLargestElement [-1, 2, 0] 2)))
  )



(deftest pagesNumberingWithInkTest
  (testing pagesNumberingWithInk)
  (is (= 1 (pagesNumberingWithInk 1 1)))
  (is (= 22 (pagesNumberingWithInk 22 2)))
  (is (= 5 (pagesNumberingWithInk 1 5)))
  (is (= 22 (pagesNumberingWithInk 21 5)))
  )




(deftest isSumOfConsecutive2Test
  (testing isSumOfConsecutive2)
  (is (= 2 (isSumOfConsecutive2 9)))
  )





(deftest subSequenceAddsToTotalTest
  (testing subSequenceAddsToTotal)
  (is (= 1 (subSequenceAddsToTotal 3 1)))
  (is (= 1 (subSequenceAddsToTotal 10 1)))
  (is (= 1 (subSequenceAddsToTotal 9 2)))
  (is (= 1 (subSequenceAddsToTotal 9 4)))
  (is (= 0 (subSequenceAddsToTotal 10 2)))
  (is (= 0 (subSequenceAddsToTotal 9 1)))
  )



(deftest squareDigitsSequenceTest
  (testing squareDigitsSequence)
  (is (= 9 (squareDigitsSequence 16)))
  (is (= 4 (squareDigitsSequence 103)))
  (is (= 2 (squareDigitsSequence 1)))
  )



(defn isPower
  [n]
  (loop [x n
         p 2]
    (cond (= 1 n) true
          (= p (/ x p)) true
          (> p (/ n 2)) false
          (zero? (mod x p)) (recur (/ x p) p)
          :else (recur n (inc p))
          )))


(deftest isPowerTest
  (testing isPower)
  (is (isPower 1))
  (is (not (isPower 3)))
  (is (not (isPower 2)))
  (is (isPower 4))
  (is (not (isPower 6)))
  (is (isPower 9))
  (is (isPower 8))
  (is (not (isPower 72)))
  (is (isPower 100))
  )

(defn makeArrayConsecutive2
  [statues]
  (reduce + (map (fn [[a b]] (dec (- b a))) (partition 2 1 (sort statues)))))

(deftest makeArrayConsecutive2Test
  (testing makeArrayConsecutive2)
  (is (= 0 (makeArrayConsecutive2 [1 2])))
  (is (= 1 (makeArrayConsecutive2 [1 3])))
  )

(defn replaceMiddle
  [a]
  (let [mid (quot (count a) 2)
        [f s] (split-at mid a)]
    (if (= (count f) (count s))
      (concat (drop-last f) [(+ (last f) (first s))] (drop 1 s))
      a)
    )
  )

(deftest replaceMiddleTest
  (testing replaceMiddle)
  (is (= [1 2 3] (replaceMiddle [1 2 3])))
  (is (= [1 4 3] (replaceMiddle [1 2 2 3])))
  )

(defn sumMiddle
  [a]
  (let [c (count a)]
    (reduce + (take (if (odd? c) 1 2) (drop (int (/ (dec c) 2)) a))))
  )

(defn isSmooth
  [xs]
  (= (first xs) (last xs) (sumMiddle xs))
  )

(deftest isSmoothTest
  (testing isSmooth)
  (is (not (isSmooth [1 2])))
  (is (isSmooth [1 1 1]))
  (is (isSmooth [2 1 1 2]))
  )




(defn removeArrayPart
  [inputArray l r]
  (concat (take l inputArray) (drop (inc r) inputArray))
  )


(deftest removeArrayPartTest
  (testing removeArrayPart)
  (is (= [1] (removeArrayPart [0 1] 0 0)))
  (is (= [0] (removeArrayPart [0 1] 1 1)))
  )


(defn concatenateArrays [a b]
  (concat a b))

(defn firstReverseTry
  [arr]
  (if (<= (count arr) 1)
      arr
      (flatten (list (last arr) (drop-last (rest arr)) (first arr)))
  )
  )

(deftest firstReverseTryTest
  (testing firstReverseTry)
  (is (= [] (firstReverseTry [])))
  (is (= [2] (firstReverseTry [2])))
  (is (= [0 0] (firstReverseTry [0 0])))
  (is (= [1 0] (firstReverseTry [0 1])))
  (is (= [1 2 0] (firstReverseTry [0 2 1])))
  )


(defn myReplace
  [elemToReplace substitutionElem x]
  (if (= x elemToReplace)
    substitutionElem
    x
    )
  )

(defn arrayReplace [inputArray elemToReplace substitutionElem]
  (map #(myReplace elemToReplace substitutionElem %) inputArray))


(deftest arrayReplaceTest
  (testing arrayReplace)
  (is (= [0] (arrayReplace [0] 0 0)))
  (is (= [1] (arrayReplace [0] 0 1)))
  (is (= [3 2 3] (arrayReplace [1 2 1] 1 3)))
  )



(defn createArray [size] (repeat size 1))




(defn countBlackCells [m n]
  (- (+ m n (.gcd (biginteger m) (biginteger n))) 2)
  )

(deftest countBlackCellsTest
  (testing countBlackCells)
  (is (= 1 (countBlackCells 1 1)))
  (is (= 4 (countBlackCells 2 2)))
  (is (= 25 (countBlackCells 1 25)))
  (is (= 7 (countBlackCells 3 3)))
  )




(defn candles
  ([candlesNumber makeNew] (candles candlesNumber makeNew 0))
  ([candlesNumber makeNew leftovers]
   (let [candleNubs (+ candlesNumber leftovers)
         candlesFromBurnedAndLeftovers (quot candleNubs makeNew)
         remainingLeftovers (rem candleNubs makeNew)]
     (if (and (zero? candlesFromBurnedAndLeftovers) (< remainingLeftovers makeNew))
       candlesNumber
      (+ candlesNumber (candles candlesFromBurnedAndLeftovers makeNew remainingLeftovers))
     ))
  ))

(deftest candlesTest
  (testing candles)
  (is (zero? (candles 0 2)))
  (is (= 1 (candles 1 2)))
  (is (= 3 (candles 2 2)))
  (is (= 9 (candles 5 2)))
  )


   ;(let [newCandles (quot candlesNumber makeNew)]
     ;  (prn candlesNumber newCandles)
     ;(if (zero? newCandles)
     ;  candlesNumber
     ;  (+ candlesNumber (candles newCandles makeNew))
     ;  ))




(defn calcRoundingValue
  [n tens]
  (cond (zero? (mod n tens)) 0
        (<= (/ tens (mod n tens)) 2) tens
        :else 0)
  )

(defn dropValueToBeRounded
  [n tens]
  (- n (mod n tens))
  )

(defn round
  [n tens]
  (+ (dropValueToBeRounded n tens) (calcRoundingValue n tens))
  )

(defn rounders
  ([n] (rounders n 10))
  ([n tens]
   (cond (< n 5) n
         (= n tens) n
         (zero? (mod n (* tens 10))) n
         (and (not= 0 (mod n tens)) (<= (quot n tens) 10)) (round n tens)
         :else (rounders (round n tens) (* 10 tens))))
  )




(deftest roundersTest
  (testing rounders)
  (is (zero? (rounders 0)))
  (is (= 1 (rounders 1)))
  (is (= 10 (rounders 5)))
  (is (= 10 (rounders 10)))
  (is (= 20 (rounders 15)))
  (is (= 10 (rounders 14)))
  (is (= 30 (rounders 25)))
  (is (= 30 (rounders 25)))
  (is (= 100 (rounders 95)))
  (is (= 100 (rounders 101)))
  (is (= 110 (rounders 105)))
  (is (= 200 (rounders 190)))
  (is (= 7000 (rounders 7001)))

  )





(deftest roundTest
  (test round)
  (is (= 0 (round 0 10)))
  (is (= 0 (round 4 10)))
  (is (= 10 (round 5 10)))
  (is (= 10 (round 10 10)))
  (is (= 20 (round 15 10)))
  (is (= 150 (round 154 10)))
  (is (= 160 (round 155 10)))
  (is (= 200 (round 150 100)))
  (is (= 100 (round 140 100)))
  )




(deftest dropValueToBeRoundedTest
  (testing dropValueToBeRounded)
  (is (= 10 (dropValueToBeRounded 10 10)))
  (is (= 0 (dropValueToBeRounded 0 10)))
  (is (= 0 (dropValueToBeRounded 1 10)))
  (is (= 10 (dropValueToBeRounded 10 10)))
  (is (= 10 (dropValueToBeRounded 12 10)))
  (is (= 100 (dropValueToBeRounded 100 100)))
  (is (= 100 (dropValueToBeRounded 120 100)))
  )

(deftest calcRoundingValueTest
  (testing calcRoundingValue)
  (is (= 0 ( calcRoundingValue 10 10)))
  (is (= 0 ( calcRoundingValue 4 10)))
  (is (= 10 ( calcRoundingValue 5 10)))
  (is (= 10 ( calcRoundingValue 15 10)))
  (is (= 0 ( calcRoundingValue 14 10)))
  (is (= 0 ( calcRoundingValue 140 100)))
  (is (= 100 ( calcRoundingValue 150 100)))
  (is (= 1000 ( calcRoundingValue 1500 1000)))
  )




(defn increaseNumberRoundness
  [n]
  (not (empty? (drop-while #(not= 0 %) (drop-while zero? (digits n)))))
  )



(deftest increaseNumberRoundnessTest
  (testing increaseNumberRoundness)
  (is (not (increaseNumberRoundness 0)))
  (is (not (increaseNumberRoundness 10)))
  (is (increaseNumberRoundness 1010))

  )


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



(defn stop
  [n tens]
  (> 0 (- n tens)))

(deftest stopTest
  (testing stop)
  (is (stop 50 100))
  (is (not (stop 190 100)))
  (is (not (stop 15 10)))
  )

