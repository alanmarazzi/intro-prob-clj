;; gorilla-repl.fileformat = 1

;; **
;;; # Probability
;;; 
;;; An intro to **probability** in Clojure
;;; 
;;; ## `P` function
;;; 
;;; `P` is the name for the *Probability* function
;; **

;; @@
(ns probability
  (:require [gorilla-plot.core :as plot]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn P
  "Probability of an event occurring"
  [event space]
  (/ (count (filter (set event) space))
     (count space)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/P</span>","value":"#'probability/P"}
;; <=

;; @@
(def D [1 2 3 4 5 6])
(def even [2 4 6])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/even</span>","value":"#'probability/even"}
;; <=

;; @@
(P even D)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>1/2</span>","value":"1/2"}
;; <=

;; **
;;; We might have done `(/ (count event) (count space))`, but in this way we would have counted stuff not present in the event space.
;; **

;; @@
(def even [2 4 6 8 10 12])

(P even D)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>1/2</span>","value":"1/2"}
;; <=

;; **
;;; ## Urn problems
;;; 
;;; Usually urns are used to explain probability. Let's take this problem as an example:
;;; 
;;; > An urn contains 23 balls: 8 white, 6 blue, and 9 red. We select six balls at random (each possible selection is equally likely). What is the probability of each of these possible outcomes:
;;; 1. all balls are red
;;; 2. 3 are blue, 2 are white, and 1 is red
;;; 3. exactly 4 balls are white
;;; 
;;; Here an *outcome* is a set of 6 balls and the *sample space* is the set of all possible 6 ball combinations. We can solve this problem with our `P` function and *counting*.
;;; 
;;; Note that an outcome is a *set* of balls, not a *sequence*, so the order doesn't matter. To account for the fact that we have more than one ball of the same color, we will represent them with the initial followed by a number, such as: `W1`
;; **

;; @@
(defn cross
  "Create a set of ways of concatenating
  items from A with items from B"
  [A B]
  (set (map #(str A %) B)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/cross</span>","value":"#'probability/cross"}
;; <=

;; @@
(def balls [{:A "W" :B (range 1 9)}
            {:A "B" :B (range 1 7)}
            {:A "R" :B (range 1 10)}])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/balls</span>","value":"#'probability/balls"}
;; <=

;; @@
(def urn (mapcat #(cross (:A %) (:B %)) balls))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/urn</span>","value":"#'probability/urn"}
;; <=

;; @@
(count urn)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>23</span>","value":"23"}
;; <=

;; **
;;; By using `clojure.math.combinatorics` we can create the sample space as the set of all 6 balls combinations.
;; **

;; @@
(require '[clojure.math.combinatorics :as combinatorics])

(defn combos
  "All combinations of n items, each one concatenated
  with str"
  [items n]
  (let [combi (combinatorics/combinations items n)]
    (map #(clojure.string/join " " %) combi)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/combos</span>","value":"#'probability/combos"}
;; <=

;; @@
(def U6 (combos urn 6))

(count U6)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>100947</span>","value":"100947"}
;; <=

;; @@
(clojure.pprint/pprint (take 10 (random-sample 0.5 U6)))
;; @@
;; ->
;;; (&quot;W3 W7 W6 W1 W8 W5&quot;
;;;  &quot;W3 W7 W6 W1 W8 W2&quot;
;;;  &quot;W3 W7 W6 W1 W8 B6&quot;
;;;  &quot;W3 W7 W6 W1 W8 B1&quot;
;;;  &quot;W3 W7 W6 W1 W8 B3&quot;
;;;  &quot;W3 W7 W6 W1 W8 B4&quot;
;;;  &quot;W3 W7 W6 W1 W8 B2&quot;
;;;  &quot;W3 W7 W6 W1 W8 R8&quot;
;;;  &quot;W3 W7 W6 W1 W8 R2&quot;
;;;  &quot;W3 W7 W6 W1 W8 R1&quot;)
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; To check if it is true that 100,947 is the right number of possible combinations we can build a function that calculates it
;; **

;; @@
(defn ! [n]
  (loop [cur n acc 1]
    (if (zero? cur) acc
      (recur (dec cur) (*' cur acc)))))

(defn choose
  "Number of ways to choose c items from
  a list of n items"
  [n c]
  (int (/ (! n) 
          (* (! (- n c)) (! c)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/choose</span>","value":"#'probability/choose"}
;; <=

;; @@
(choose 23 6)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>100947</span>","value":"100947"}
;; <=

;; **
;;; ### Problem 1: what's the probability of selecting 6 red balls?
;; **

;; @@
(defn count-color
  [string color]
  (->> string
       (re-seq (re-pattern color))
       count))

(defn draw
  [urn color n]
  (filter 
    #(= (count-color % color) n) 
    urn))

(def red6 (draw U6 "R" 6))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/red6</span>","value":"#'probability/red6"}
;; <=

;; @@
(P red6 U6)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>4/4807</span>","value":"4/4807"}
;; <=

;; @@
(count red6)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>84</span>","value":"84"}
;; <=

;; @@
(choose 9 6)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>84</span>","value":"84"}
;; <=

;; **
;;; As we saw there are **84** different ways of drawing 6 red balls. Since there are 9 red balls in the urn, we are asking `(choose 9 6)` basically. This means that **P** of 6 red balls is 9 choose 6 divided by the size of the sample space.
;; **

;; @@
(= (P red6 U6)
   (/ (choose 9 6)
      (count U6)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; **
;;; ### Problem 2: what is the probability of 3 blue, 2 white and 1 red?
;; **

;; @@
(defn draw
  [urn colors n]
  (let [count-colors (fn [s c]
                       (map #(count-color s %) c))
        right-draw? (fn [s c n]
                      (= (count-colors s c)
                         n))]
    (filter #(right-draw? % colors n) urn)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/draw</span>","value":"#'probability/draw"}
;; <=

;; @@
(def b3w2r1 (draw U6 ["B" "W" "R"] [3 2 1]))

(P b3w2r1 U6)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>240/4807</span>","value":"240/4807"}
;; <=

;; @@
(= (P b3w2r1 U6)
   (/ (* (choose 6 3)
         (choose 8 2)
         (choose 9 1))
      (count U6)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; **
;;; ### Problem 3: what is the probability of drawing 4 white balls?
;; **

;; @@
(def w4 (draw U6 ["W"] [4]))

(P w4 U6)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>350/4807</span>","value":"350/4807"}
;; <=

;; **
;;; ## Generalizing `P`
;; **

;; @@
(defn P
  [event space]
  (let [e (if (fn? event)
            (filter event space)
            (filter (set event) space))]
    (/ (count e)
       (count space))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/P</span>","value":"#'probability/P"}
;; <=

;; @@
(P even? D)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>1/2</span>","value":"1/2"}
;; <=

;; @@
(def D12 (range 1 13))

(P even? D12)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>1/2</span>","value":"1/2"}
;; <=

;; @@
(defn divisible? 
  [a b]
  (zero? (mod a b)))

(defn prime? 
  [n]
  (and (> n 1) (not-any? (partial divisible? n) (range 2 n))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/prime?</span>","value":"#'probability/prime?"}
;; <=

;; @@
(def D3 
  (->> (for [a D
             b D
             c D]
         [a b c])
       (map #(reduce + %))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/D3</span>","value":"#'probability/D3"}
;; <=

;; @@
(P prime? D3)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>73/216</span>","value":"73/216"}
;; <=

;; **
;;; ## Card Problems
;;; 
;;; Let's play some Poker! We define a `deck` as a set of 52 cards, and `hands` as the sample space of all combinations of 5 cards:
;; **

;; @@
(def ranks (clojure.string/split "A23456789TJQK" #""))

(def suits&ranks
  [{:A "S" :B ranks}
   {:A "H" :B ranks}
   {:A "D" :B ranks}
   {:A "C" :B ranks}])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/suits&amp;ranks</span>","value":"#'probability/suits&ranks"}
;; <=

;; @@
(def deck (atom (mapcat #(cross (:A %) (:B %)) suits&ranks)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/deck</span>","value":"#'probability/deck"}
;; <=

;; @@
(count @deck)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>52</span>","value":"52"}
;; <=

;; @@
(def hands (atom (combos @deck 5)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/hands</span>","value":"#'probability/hands"}
;; <=

;; @@
(= (count @hands)
   (choose 52 5))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@
(count @hands)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>2598960</span>","value":"2598960"}
;; <=

;; @@
(clojure.pprint/pprint (take 10 (random-sample 0.5 @hands)))
;; @@
;; ->
;;; (&quot;S9 S6 S2 S5 SA&quot;
;;;  &quot;S9 S6 S2 S5 SQ&quot;
;;;  &quot;S9 S6 S2 S5 ST&quot;
;;;  &quot;S9 S6 S2 S5 SK&quot;
;;;  &quot;S9 S6 S2 S5 S4&quot;
;;;  &quot;S9 S6 S2 S5 H3&quot;
;;;  &quot;S9 S6 S2 S5 HQ&quot;
;;;  &quot;S9 S6 S2 S5 HJ&quot;
;;;  &quot;S9 S6 S2 S5 H8&quot;
;;;  &quot;S9 S6 S2 S5 H4&quot;)
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn flush?
  [hand]
  (let [suits (map :A suits&ranks)]
    (some true?
          (map #(= (count-color hand %) 5) suits))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/flush?</span>","value":"#'probability/flush?"}
;; <=

;; @@
(P flush? @hands)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>33/16660</span>","value":"33/16660"}
;; <=

;; @@
(defn four-kind?
  [hand]
  (some true?
        (map #(= (count-color hand %) 4) ranks)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;probability/four-kind?</span>","value":"#'probability/four-kind?"}
;; <=

;; @@
(P four-kind? @hands)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>1/4165</span>","value":"1/4165"}
;; <=

;; @@

;; @@
