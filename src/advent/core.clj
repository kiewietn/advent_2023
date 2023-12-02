(ns advent.core)

(defn split-file [file]
  (clojure.string/split-lines (slurp file)))

(defn day1 [input]
  (reduce + (map (fn [line]
                   (Integer/parseInt (str (first line) (last line))))
                 (map (fn [line]
                        (filter #(Character/isDigit %) line)) input))))

(defn day1_2 [input]
  (reduce + (map (fn [line]
                   (let [calibration-sequence (list (coerce-to-int (first line)) (coerce-to-int (last line)))]
                      (Integer/parseInt (apply str calibration-sequence))))
                  (map (fn [line]
                         (split-numbers line)) input))))

(defn coerce-to-int [input]
  (cond (Character/isDigit (first input)) input
        (= "one" input) "1"
        (= "two" input) "2"
        (= "three" input) "3"
        (= "four" input) "4"
        (= "five" input) "5"
        (= "six" input) "6"
        (= "seven" input) "7"
        (= "eight" input) "8"
        (= "nine" input) "9"))

(defn split-numbers [input]
  (let [matcher (re-matcher #"one|two|three|four|five|six|seven|eight|nine|\d" input)]
    (loop [match (re-find matcher)
           acc []]
      (if (nil? match)
        acc
        (recur (re-find matcher) (conj acc match))))))
