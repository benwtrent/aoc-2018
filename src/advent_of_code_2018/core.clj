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


;;;Day 4

(def day4-test-input ["[1518-11-01 00:00] Guard #10 begins shift"
                      "[1518-11-01 00:05] falls asleep"
                      "[1518-11-01 00:25] wakes up"
                      "[1518-11-01 00:30] falls asleep"
                      "[1518-11-01 00:55] wakes up"
                      "[1518-11-03 00:05] Guard #10 begins shift"
                      "[1518-11-02 00:40] falls asleep"
                      "[1518-11-02 00:50] wakes up"
                      "[1518-11-03 00:24] falls asleep"
                      "[1518-11-01 23:58] Guard #99 begins shift"
                      "[1518-11-04 00:02] Guard #99 begins shift"
                      "[1518-11-03 00:29] wakes up"
                      "[1518-11-04 00:36] falls asleep"
                      "[1518-11-05 00:03] Guard #99 begins shift"
                      "[1518-11-04 00:46] wakes up"
                      "[1518-11-05 00:45] falls asleep"
                      "[1518-11-05 00:55] wakes up"])

(defn get-guard [str-input]
  (keyword (second (re-find #"Guard #(\d+)" str-input))))

(defn get-action [str-input]
  (cond
        (re-find #"falls asleep" str-input) :sleep
        (re-find #"wakes up" str-input) :wake
        (re-find #"Guard " str-input) :guard-change
        :else :unk))

(defn get-minute-seq [str1 str2]
  (let [min-start (parse-int (second (re-find #"\:(\d+)\]" str1)))
        min-end (parse-int (second (re-find #"\:(\d+)\]" str2)))]
    (range min-start min-end)))

(defn update-guard-sleep [sleep-act wake-act guard data]
  (let [guard-data (guard data)
        guard-data (if (nil? guard-data) {} guard-data)
        min-seq (get-minute-seq sleep-act wake-act)
        min-keys (into [] (map #(into [] [(keyword (str %))]) min-seq))
        updated-guard (reduce (fn [a k] (update-in a k (fnil inc 0))) guard-data min-keys)]
    (assoc data guard updated-guard)))

(defn day4-to-m [input]
  (let [sort-input (sort input)]
    (loop [curr-action (first sort-input)
           last-action nil
           rest-action (rest sort-input)
           guard nil
           data {}]
      (if (nil? curr-action)
        data
        (let [action (get-action curr-action)
              new-action (first rest-action)
              left (rest rest-action)]
          (case action
            :sleep (recur new-action curr-action left guard data)
            :wake (recur new-action curr-action left guard (update-guard-sleep last-action curr-action guard data))
            :guard-change (recur new-action curr-action left (get-guard curr-action) data)
            (recur new-action curr-action left guard data)))))))

(defn maxkeyval [maxkv [k v]]
  (if (> v (first (vals maxkv))) {k v} maxkv))

(defn day4-p1 [input]
  (let [m (day4-to-m input)
        total-sleeps (reduce-kv #(assoc %1 %2 (reduce + (vals %3))) {} m)
        max-guard (first (keys (reduce maxkeyval {:val 0} total-sleeps)))
        max-sleep (first (keys (reduce maxkeyval {:val 0} (max-guard m))))]
    (* (parse-int (name max-guard)) (parse-int (name max-sleep)))))

(def day4-input (read-file-lines "day4"))

(day4-p1 day4-input)                                        ;19025

(defn common-sleep [guard m]
  (reduce maxkeyval {:val 0} (guard m)))

(defn to-common-sleep [m]
  (reduce (fn [acc [k v]] (assoc acc k (common-sleep k m))) {} m))

(defn max-common-sleep [commons]
  (reduce (fn [mv [k v]] (if (> (first (vals v)) (first (vals (first (vals mv))))) {k v} mv)) {:guard {:val 0}} commons))

(defn day4-p2 [input]
  (let [m (day4-to-m input)
        commons (to-common-sleep m)
        max-c (max-common-sleep commons)
        guard-num (parse-int (name (first (keys max-c))))
        sleep-num (parse-int (name (first (keys (first (vals max-c))))))]
    (* guard-num sleep-num)))

(day4-p2 day4-input)                                        ;23776

