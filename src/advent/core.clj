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
                         (map last (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))" line))) input))))

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


(defn parse-games-input [input]
  (map (fn [game]
         (let [rounds (clojure.string/split game #";")]
           (map (fn [round]
                  (let [colors (map #(clojure.string/trim %) (clojure.string/split round #","))]
                    (reduce (fn [acc color]
                              (let [[count col] (clojure.string/split color #" ")]
                                (assoc acc col (Integer/parseInt count)))) {} colors))) rounds))) (into [] (map (fn [line]
                                                                                                                  (last (clojure.string/split line #":"))) input))))

(def capacity-map {"blue" 14 "red" 12 "green" 13})

(defn day2 [input]
  (reduce +
          (map #(inc (get % 0))
               (filter (fn [[idx bool]]
                         bool) (map-indexed vector (map game-possible? (parse-games-input input)))))))

(defn game-possible? [game]
  (every? (fn [round]
            (let [green-valid? (<= (get round "green" 0) (get capacity-map "green"))
                  red-valid? (<= (get round "red" 0) (get capacity-map "red"))
                  blue-valid? (<= (get round "blue" 0) (get capacity-map "blue"))]
              (and green-valid? red-valid? blue-valid?))) game))xo
