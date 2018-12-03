(ns advent-of-code-2018.core
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.java.io :as io]))

;;; https://adventofcode.com/2018

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn read-file-lines [daynumber]
  (let [file-name (str "inputs/" daynumber ".txt")]
    (with-open [rdr (io/reader file-name)]
      (into [] (line-seq rdr)))))

;;https://adventofcode.com/2018/day/1
(defn day1-p1 [freq]
  (reduce + freq))

(def day1-input (with-open [rdr (io/reader "inputs/day1.txt")]
                  (into [] (doall (map parse-int (line-seq rdr))))))

(day1-p1 day1-input)                                        ;416

(defn day1-p2 [freq]
  (let [size (count freq)]
    (loop [pos 0 seen #{} cur 0]
      (if (contains? seen cur)
        cur
        (recur (mod (+ pos 1) size) (conj seen cur) (+ cur (get freq pos)))))))

(day1-p2 day1-input)                                        ;56752

;Day 2
;;https://adventofcode.com/2018/day/2

(def day2-input (read-file-lines "day2"))

(def day2-test-input ["abcdef"
                      "bababc"
                      "abbcde"
                      "abcccd"
                      "aabcdd"
                      "abcdee"
                      "ababab"])

(defn into-or-inc [m c]
  (update-in m [c] (fnil inc 0)))

(defn to-map-count [s]
  (let [arr (char-array s)]
    (reduce into-or-inc {} arr)))

(defn has-count [m c]
  (some #(= c %) (vals m)))

(defn has-two-count [m]
  (has-count m 2))

(defn has-three-count [m]
  (has-count m 3))

(defn day2-p1 [input]
  (let [m (map to-map-count input)
        two-counts (filter has-two-count m)
        three-counts (filter has-three-count m)]
    (* (count two-counts) (count three-counts))))

(day2-p1 day2-input)                                        ;7134

(def day2-test-input-2 ["abcde"
                        "fghij"
                        "klmno"
                        "pqrst"
                        "fguij"
                        "axcye"
                        "wvxyz"])

(defn diff-by-one [s1 s2]
  (loop [seq1 s1
         seq2 s2
         pos 0
         diff -1]
    (if (or (empty? seq1) (empty? seq2))
      diff
      (if (not= (first seq1) (first seq2))
        (if (not= -1 diff)
          nil
          (recur (rest seq1) (rest seq2) (+ pos 1) pos))
        (recur (rest seq1) (rest seq2) (+ pos 1) diff)))))

(defn diff-by-ones [word words]
  (loop [other (first words)
         words (rest words)]
    (let [diff (diff-by-one word other)]
      (if (or (not (nil? diff)) (empty? words))
        diff
        (recur (first words) (rest words))))))

(defn day2-p2 [seqs]
    (loop [word (first seqs)
           words (rest seqs)]
      (let [diff (diff-by-ones word words)]
        (if (not (nil? diff))
          (str (subs word 0 diff) (subs word (+ 1 diff)))
          (recur (first words) (rest words))))))

(day2-p2 day2-input)                                        ;kbqwtcvzhmhpoelrnaxydifyb

;;;day3 https://adventofcode.com/2018/day/3

(def day3-test-input ["#1 @ 1,3: 4x4"
                      "#2 @ 3,1: 4x4"
                      "#3 @ 5,5: 2x2"])

(def day3-input (read-file-lines "day3"))

(defn str-to-m [input]
  (let [spaces (s/split input #" ")
        id (first spaces)
        skip-2 (rest (rest spaces))
        coord (s/split (first skip-2) #",")
        lft (parse-int (first coord))
        top (parse-int (subs (last coord) 0 (dec (count (last coord)))))
        dim (s/split (last skip-2) #"x")
        x (parse-int (first dim))
        y (parse-int (last dim))
        claimed (combo/cartesian-product (range lft (+ lft x)) (range top (+ top y)))]
    {:id id
     :pos-lft lft
     :pos-top top
     :pos-rgt (dec (+ lft x))
     :pos-bot (dec (+ top y))
     :by-x x
     :by-y y
     :claimed (into #{} claimed)
     :claimed-total (count claimed)}))


(defn day3-p1 [input]
  (let [ms (map str-to-m input)]
    (count (filter (fn [[k v]] (> v 1)) (frequencies (mapcat :claimed ms))))))

(defn day3-p2 [input]
  (let [ms (map str-to-m input)
        pts (into #{} (keys (filter (fn [[k v]] (> v 1)) (frequencies (mapcat :claimed ms)))))]
    (filter #(empty? (set/intersection (:claimed %) pts)) ms)))


(day3-p1 day3-input)                                        ;110389
(:id (first (day3-p2 day3-input)))                          ;552
