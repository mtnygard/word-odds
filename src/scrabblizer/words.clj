(ns scrabblizer.words
  (:require (clojure [string :as str])))

(def us-tile-frequencies [9 2 2 4 12 2 3 2 9 1 1 4 2 6 8 2  1 6 4 6 4 2 2 1 2  1])
(def us-tile-scores      [1 3 3 2  1 4 2 4 1 8 5 1 3 1 1 3 10 1 1 1 1 4 4 8 4 10])

(comment
  (use 'clojure.test)

  (is (= 26 (count us-tile-frequencies) (count us-tile-scores)))
  (is (= 98 (reduce + us-tile-frequencies)))
  )

(defn letter-odds
  "Compute the odds of drawing a specific letter from the bag of letters available.
   Returns a ratio in [0, 1]"
  [letter bag]
  (/ (nth bag letter) (reduce + bag)))

(comment
  (is (= 12/98 (letter-odds 4 us-tile-frequencies)))
  (def depleted-bag (assoc us-tile-frequencies 4 0))
  (is (= 0 (letter-odds 4 depleted-bag)))
  )

(defn remove-tile
  "Draw a single letter from the bag, returning a bag with one fewer of the letter.
   If there are zero of the letter remaining, just return the bag unchanged."
  [bag letter]
  (let [remaining (nth bag letter)]
    (if (> remaining 0)
      (assoc bag letter (dec remaining))
      bag)))

(defn successive-bags
  "Return a sequence of letter bags. Each member of the sequence has
  one more tile drawn from it, corresponding to the letters in the
  sequence. If there are zero of a letter, then the bag is unchanged."
  [letters bag]
  (reductions remove-tile bag letters))

(defn word-odds
  "Compute the odds of a sequence of letters being drawn from the initial bag."
  [letters bag]
  (reduce * (map letter-odds letters (successive-bags letters bag))))

(comment
  (successive-bags [0 0 0 0] us-tile-frequencies)
  (successive-bags '(0 15 15 11 4 15 8 4) us-tile-frequencies)
  )

(defn lowercase    [c]     (Character/toLowerCase c))
(defn char->number [c]     (- (int c) 97))
(defn letter?      [c]     (<= 0 c 25))

(defn phrase->letters
  "Convert an input string into a sequence of letter indices in [0, 25].
   Blanks are ignored. Non-alphabetic characters are ignored."
  [words]
  (filter letter? (map (comp char->number lowercase) words)))

(defn phrase-odds
  "Compute the odds of drawing a phrase in your rack of 7 letters
   using the given bag of tiles. If the phrase is longer than 7
   letters, this is zero, by definition."
   [phrase bag]
  (let [letters (phrase->letters phrase)]
    (if (> (count letters) 7)
      0
      (word-odds letters bag))))

(defn us-phrase-odds
  "Compute the odds of drawing a phrase in your rack of 7 letters,
  drawing from a full bag of tiles using the U.S. edition of
  Scrabble (tm)"
  [phrase]
  (phrase-odds phrase us-tile-frequencies))

(defn phrase-score
  "Compute the base score for a phrase using the tile scores supplied.
   Unlike the odds of drawing a phrase, this is not limited by length, because
   a player could be extending a previously played word."
  [phrase scores]
  (reduce + (map scores (phrase->letters phrase))))

(defn us-phrase-score
  "Compute the base score for a phrase in the U.S. edition of Scrabble (tm).
   Unlike the odds of drawing a phrase, this is not limited by length, because
   a player could be extending a previously played word."
  [phrase]
  (phrase-score phrase us-tile-scores))

(comment

  (is (= (us-phrase-odds "apple") (us-phrase-odds "AppLE") (us-phrase-odds "ppale")))
  (is (> (us-phrase-odds "a") (us-phrase-odds "app") (us-phrase-odds "apple")))
  (is (= 0N (us-phrase-odds "apple pie")))
  (is (= 0N (us-phrase-odds "abracadabra")))
  (is (= (us-phrase-odds "apple") (us-phrase-odds "a p p le!")))
  )
