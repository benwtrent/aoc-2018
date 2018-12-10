(ns advent-of-code-2018.core
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]))

;;; https://adventofcode.com/2018

(defn sum [coll]
  (reduce + coll))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn read-file-lines [daynumber]
  (let [file-name (str "inputs/" daynumber ".txt")]
    (with-open [rdr (io/reader file-name)]
      (into [] (line-seq rdr)))))

;;https://adventofcode.com/2018/day/1
(defn day1-p1 [freq]
  (sum freq))

(def day1-input (with-open [rdr (io/reader "inputs/day1.txt")]
                  (into [] (doall (map parse-int (line-seq rdr))))))

;(day1-p1 day1-input)                                        ;416

(defn day1-p2 [freq]
  (let [size (count freq)]
    (loop [pos 0 seen #{} cur 0]
      (if (contains? seen cur)
        cur
        (recur (mod (+ pos 1) size) (conj seen cur) (+ cur (get freq pos)))))))

;(day1-p2 day1-input)                                        ;56752

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

;(day2-p1 day2-input)                                        ;7134

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
      (if (or (some? diff) (empty? words))
        diff
        (recur (first words) (rest words))))))

(defn day2-p2 [seqs]
  (loop [word (first seqs)
         words (rest seqs)]
    (let [diff (diff-by-ones word words)]
      (if (some? diff)
        (str (subs word 0 diff) (subs word (+ 1 diff)))
        (recur (first words) (rest words))))))

;(day2-p2 day2-input)                                        ;kbqwtcvzhmhpoelrnaxydifyb

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
    {:id            id
     :pos-lft       lft
     :pos-top       top
     :pos-rgt       (dec (+ lft x))
     :pos-bot       (dec (+ top y))
     :by-x          x
     :by-y          y
     :claimed       (into #{} claimed)
     :claimed-total (count claimed)}))


(defn day3-p1 [input]
  (let [ms (map str-to-m input)]
    (count (filter (fn [[k v]] (> v 1)) (frequencies (mapcat :claimed ms))))))

(defn day3-p2 [input]
  (let [ms (map str-to-m input)
        pts (into #{} (keys (filter (fn [[k v]] (> v 1)) (frequencies (mapcat :claimed ms)))))]
    (filter #(empty? (set/intersection (:claimed %) pts)) ms)))


;(day3-p1 day3-input)                                        ;110389
;(:id (first (day3-p2 day3-input)))                          ;552


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
        total-sleeps (reduce-kv #(assoc %1 %2 (sum (vals %3))) {} m)
        max-guard (first (keys (reduce maxkeyval {:val 0} total-sleeps)))
        max-sleep (first (keys (reduce maxkeyval {:val 0} (max-guard m))))]
    (* (parse-int (name max-guard)) (parse-int (name max-sleep)))))

(def day4-input (read-file-lines "day4"))

;(day4-p1 day4-input)                                        ;19025

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

;(day4-p2 day4-input)                                        ;23776

(def day5-test-input "dabAcCaCBAcCcaDA")

(def day5-input (first (read-file-lines "day5")))

(defn same-but-diff-case? [c1 c2]
  (and
    (some? c1)
    (some? c2)
    (not= c1 c2)
    (= (s/upper-case c1) (s/upper-case c2))))

(defn remove-dissimilar-case [str-letters]
  (let [cs (seq (char-array str-letters))
        cleaned-cs (reduce
                     (fn [res c]
                       (if (same-but-diff-case? (first res) c)
                         (rest res)
                         (conj res c))) (conj '() (first cs)) (rest cs))
        cs-fixed (reverse cleaned-cs)]
    (s/join cs-fixed)))

(defn day5-p1 [str-input]
  (loop [cs str-input
         fixed (remove-dissimilar-case str-input)]
    (if (= cs fixed)
      cs
      (recur fixed (remove-dissimilar-case fixed)))))

;(count (day5-p1 day5-input))                                ;11590
(def alphas (map char (range (int \a) (inc (int \z)))))

(defn remove-char [str-input c]
  (let [pattern (re-pattern (#(str "(?i)" c)))]
    (s/replace str-input pattern "")))

(defn day5-p2 [str-input]
  (let [sizes (map #(count (day5-p1 (remove-char str-input %))) alphas)]
    (apply min sizes)))

;(day5-p2 day5-input)                                        ;4504

;;day6

(def day6-test-input [[1, 1]
                      [1, 6]
                      [8, 3]
                      [3, 4]
                      [5, 5]
                      [8, 9]])

(def day6-input [[249, 60]
                 [150, 332]
                 [174, 83]
                 [287, 329]
                 [102, 338]
                 [111, 201]
                 [259, 96]
                 [277, 161]
                 [143, 288]
                 [202, 311]
                 [335, 55]
                 [239, 148]
                 [137, 224]
                 [48, 214]
                 [186, 87]
                 [282, 334]
                 [147, 157]
                 [246, 191]
                 [241, 181]
                 [286, 129]
                 [270, 287]
                 [79, 119]
                 [189, 263]
                 [324, 280]
                 [316, 279]
                 [221, 236]
                 [327, 174]
                 [141, 82]
                 [238, 317]
                 [64, 264]
                 [226, 151]
                 [110, 110]
                 [336, 194]
                 [235, 333]
                 [237, 55]
                 [230, 137]
                 [267, 44]
                 [258, 134]
                 [223, 42]
                 [202, 76]
                 [159, 135]
                 [229, 238]
                 [197, 83]
                 [173, 286]
                 [123, 90]
                 [314, 165]
                 [140, 338]
                 [347, 60]
                 [108, 76]
                 [268, 329]])

(defn manhatten-distance [p1 p2]
  (let [diffs (map (comp math/abs -) p1 p2)]
    (sum diffs)))

(defn build-points [centroids]
  (let [maxy (inc (apply max (map last centroids)))
        maxx (inc (apply max (map first centroids)))
        pts (combo/cartesian-product (range 0 (inc maxx)) (range 0 (inc maxy)))]
    pts))

(defn centroid-dists [centroids pt]
  (loop [centroid (first centroids)
         centroids (rest centroids)
         distances []]
    (if (nil? centroid)
      distances
      (let [dist (manhatten-distance centroid pt)]
        (recur (first centroids) (rest centroids) (conj distances dist))))))

(defn cluster [centroids pts]
  (loop [clusters (into [] (map #(empty [%]) centroids))
         pt (first pts)
         pts (rest pts)]
    (if (nil? pt)
      clusters
      (let [c-dists (centroid-dists centroids pt)
            c-centroid (first (apply min-key second (map-indexed vector c-dists)))
            c-dist (apply min c-dists)
            shared? (> (get (frequencies c-dists) c-dist) 1)
            n-cluster (conj (nth clusters c-centroid) pt)
            new-clusters (if shared? clusters (assoc clusters c-centroid n-cluster))]
        (recur new-clusters (first pts) (rest pts))))))

(defn day6-p1 [centroids]
  (let [pts (build-points centroids)
        clusters (cluster centroids pts)
        max-y (inc (apply max (map last centroids)))
        max-x (inc (apply max (map first centroids)))
        edge? #(or (= max-y (last %)) (= 0 (first %)) (= 0 (last %)) (= max-x (first %)))]
    (loop [c (first clusters)
           clusters (rest clusters)
           max-pts 0]
      (if (nil? c)
        max-pts
        (let [size (if (some edge? c) -1 (count c))]
          (recur (first clusters) (rest clusters) (max size max-pts)))))))

;(day6-p1 day6-input)                                        ;3420

(defn in-region? [centroids pt sum-dist]
  (let [c-dists (centroid-dists centroids pt)]
    (< (sum c-dists) sum-dist)))

(defn region-seed [centroids pts sum-dist]
  (first (filter #(in-region? centroids % sum-dist) pts)))

(defn day6-p2 [centroids max-sum-dist]
  (let [pts (build-points centroids)]
    (count (filter #(in-region? centroids % max-sum-dist) pts))))

;; Very slow, might be better to simply get the FIRST point in the region
;; and then build out from there using it as a "region seed"
;; However, this method goes through every point instead
;(day6-p2 day6-input 10000)                                  ;46667

;;;Day 7

(def day7-test-input ["Step C must be finished before step A can begin."
                      "Step C must be finished before step F can begin."
                      "Step A must be finished before step D can begin."
                      "Step A must be finished before step B can begin."
                      "Step B must be finished before step E can begin."
                      "Step D must be finished before step E can begin."
                      "Step F must be finished before step E can begin."])

(def day7-input (read-file-lines "day7"))

(defn into-map [m str-input]
  (let [f-str (second (re-find #"Step (.)" str-input))
        s-str (second (re-find #"step (.)" str-input))
        v (keyword f-str)
        k (keyword s-str)]
    (update-in m [k] (fnil #(into [] (sort (conj % v))) []))))

(defn r-into-map [m str-input]
  (let [f-str (second (re-find #"Step (.)" str-input))
        s-str (second (re-find #"step (.)" str-input))
        k (keyword f-str)
        v (keyword s-str)]
    (update-in m [k] (fnil #(into [] (sort (conj % v))) []))))

(defn input-to-dag [input]
  (reduce into-map {} input))

(defn r-input-to-dag [input]
  (reduce r-into-map {} input))

(defn update-reqs [reqs k]
  (into [] (filter #(not= % k) reqs)))

(defn update-all-reqs [m ks req]
  (loop [k (first ks)
         ks (rest ks)
         m m]
    (if (nil? k)
      m
      (recur (first ks) (rest ks) (update-in m [k] #(update-reqs % req))))))

(defn not-quiet-bfs [dag r-dag starting-keys]
  (loop [seen #{}
         path '()
         sats r-dag
         stack (into [] starting-keys)]
    (if (empty? stack)
      path
      (let [current (peek stack)
            sub-nodes (into [] (reverse (current dag)))
            new-sats (update-all-reqs sats sub-nodes current)
            satisfied? (empty? (current sats))
            not-visited? (into [] (filter #(not (contains? seen %)) sub-nodes))
            new-path (if (contains? seen current) path (conj path current))
            new-queue (pop stack)
            potential-new-queue (into [] (reverse (sort (into #{} (concat new-queue not-visited?)))))
            new-queue (if satisfied? potential-new-queue (into [] (cons current new-queue)))
            new-seen (into seen new-path)
            ]
        (recur (if satisfied? new-seen seen) (if satisfied? new-path path) (if satisfied? new-sats sats) new-queue)))))


(not-quiet-bfs (r-input-to-dag day7-test-input) (input-to-dag day7-test-input) [:K :C])

(defn day7-p1 [input]
  (let [r-dag (r-input-to-dag input)
        dag (input-to-dag input)
        sinks (reverse (sort (set/difference (into #{} (keys r-dag)) (into #{} (keys dag)))))
        re-keys (not-quiet-bfs r-dag dag sinks)]
    (s/join (map name (reverse re-keys)))))

;(day7-p1 day7-input)                                        ;ABGKCMVWYDEHFOPQUILSTNZRJX

(defn tick-workers [workers]
  (zipmap (keys workers) (map (fn [[f s]] [f (dec s)]) (vals workers))))

(defn any-assignable? [workers]
  (some #(<= % 0) (map second (vals workers))))

(defn all-assignable? [workers]
  (every? #(<= % 0) (map second (vals workers))))

(defn get-assignable [workers]
  (first (first (filter (fn [[k v]] (<= (second v) 0)) workers))))

(defn get-completed [workers]
  (filter some? (map (fn [[k v]] (first v)) (filter (fn [[k v]] (<= (second v) 0)) workers))))

(defn assign-if-possible [workers tasks]
  (loop [workers workers
         task (first tasks)
         tasks (rest tasks)
         assigned '()]
    (if (or (not (any-assignable? workers)) (nil? task))
      {:assigned assigned :workers workers}
      (let [assignable (get-assignable workers)
            task-val (+ 60 (- (int (first (char-array (name task)))) 64))
            already-assigned? (contains? (into #{} (map first (vals workers))) task)]
        (if already-assigned?
          (recur workers (first tasks) (rest tasks) (conj assigned task))
          (recur (assoc workers assignable [task task-val]) (first tasks) (rest tasks) (conj assigned task)))))))


(defn update-alls-reqs [dag reqs ts]
  (loop [ts ts
         reqs reqs]
    (if (empty? ts)
      reqs
      (let [t (first ts)
            sub-nodes (t dag)]
        (recur (rest ts) (update-all-reqs reqs sub-nodes t))))))

(defn not-quiet-bfs-ticks [dag r-dag starting-keys workers]
  (loop [seen #{}
         sats r-dag
         workers (:workers (assign-if-possible workers starting-keys))
         stack (into [] starting-keys)
         tick 0]
    (if (and (all-assignable? workers) (empty? stack))
      tick
      (if (any-assignable? workers)
        (let [completed-tasks (get-completed workers)
              new-sats (update-alls-reqs dag sats completed-tasks)
              assignable-tasks (sort (filter #(empty? (% new-sats)) stack))
              assign (assign-if-possible workers assignable-tasks)
              assigned-tasks (:assigned assign)
              workers (:workers assign)
              new-seen (into seen assigned-tasks)
              adj-stack (set/difference (into #{} stack) (into #{} assigned-tasks))
              new-stack (concat adj-stack (filter #(not (contains? seen %)) (mapcat #(% dag) assigned-tasks)))]
          (recur new-seen new-sats (tick-workers workers) new-stack (inc tick)))
        (recur seen sats (tick-workers workers) stack (inc tick))))))


(assign-if-possible {:1 [nil 2] :2 [nil 0]} '(:C))
(not-quiet-bfs-ticks (r-input-to-dag day7-test-input) (input-to-dag day7-test-input) '(:C) {:1 [nil 0] :2 [nil 0]})

(defn day7-p2 [input workers]
  (let [r-dag (r-input-to-dag input)
        dag (input-to-dag input)
        sinks (reverse (sort (set/difference (into #{} (keys r-dag)) (into #{} (keys dag)))))
        res (not-quiet-bfs-ticks r-dag dag sinks workers)]
    res))

;(day7-p2 day7-test-input {:1 [nil 0] :2 [nil 0]})
;(day7-p2 day7-input {:1 [nil 0] :2 [nil 0] :3 [nil 0] :4 [nil 0] :5 [nil 0]}) ;898

;;;;Day 8

(defn create-id-generator
  "Returns an impure function to generate ids"
  []
  (let [next-id (atom 0)]
    (fn []
      (swap! next-id inc)
      @next-id)))

;; here we create our "object"
(def generate-id! (create-id-generator))

(def day8-test-input [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(def day8-input (map parse-int (s/split (first (read-file-lines "day8")) #" ")))

(defn nth-or-nil [n coll]
  (if (<= 0 n (dec (count coll))) (nth coll n) nil))

;probably should use an ordered set library here
(defn conj-if-uniq [xs x]
  (if (some (fn [c] (= c x)) xs) xs (conj xs x)))

(defn generate-node [parent-id nums]
  (let [num-children (first nums)
        num-metadata (second nums)
        nums (drop 2 nums)
        meta-data (if (< 0 num-children) [] (into [] (take num-metadata nums)))
        nums (drop (count meta-data) nums)]
    {:node {:id           (keyword (str (generate-id!)))
            :parent       parent-id
            :num-children num-children
            :children     []
            :num-metadata num-metadata
            :value        (sum meta-data)
            :meta-data    meta-data}
     :nums nums}))

(defn update-parent [node node-map]
  (let [parent-id (:parent node)
        id (:id node)]
    (if (nil? parent-id)
      node-map
      (update-in node-map [parent-id] #(update-in % [:children] (fn [cs] (conj-if-uniq cs id)))))))

(defn children-completed? [node]
  (= (:num-children node) (count (:children node))))

(defn node-completed? [node]
  (and (children-completed? node)
       (= (:num-metadata node) (count (:meta-data node)))))

(defn calc-value [metadata children node-map]
  (if (or (empty? children) (empty? metadata))
    0
    (let [child-keys (filter some? (map #(nth-or-nil (dec %) children) metadata))
          child-values (map #(:value (% node-map)) child-keys)]
      (sum child-values))))

(defn update-metadata [node node-map nums]
  (let [shoud-update? (and (= (count (:children node)) (:num-children node)) (not (node-completed? node)))
        metadata (take (:num-metadata node) nums)
        new-nums (drop (:num-metadata node) nums)
        new-node (assoc node :meta-data metadata :value (calc-value metadata (:children node) node-map))
        new-node-map (assoc node-map (:id node) new-node)]
    {:nums (if shoud-update? new-nums nums) :nodes (if shoud-update? new-node-map node-map)}))


(defn push-generations [parent nums node-stack nodes]
  (if (children-completed? parent)
    {:stack node-stack :nodes nodes :nums nums}
    (loop [parent-id (:id parent)
           nums nums
           node-stack node-stack
           nodes nodes]
      (let [h (generate-node parent-id nums)
            nums (:nums h)
            node (:node h)]
        (if (= 0 (:num-children node))
          {:stack (conj node-stack (:id node)) :nodes (assoc nodes (:id node) node) :nums nums}
          (recur (:id node) nums (conj node-stack (:id node)) (assoc nodes (:id node) node)))))))

(defn pop-nodes [nums node-stack node-map]
  (loop [node-stack node-stack
         nums nums
         node-map node-map]
    (if (empty? node-stack)
      node-map
      (let [node-id (peek node-stack)
            node (node-id node-map)
            node-map (update-parent node node-map)
            updated-metadata (update-metadata node node-map nums)
            nums (:nums updated-metadata)
            node-map (:nodes updated-metadata)
            generations (push-generations node nums node-stack node-map)
            new-node-map (:nodes generations)
            new-nums (:nums generations)
            new-stack (if (children-completed? node) (pop node-stack) (:stack generations))]
        (recur new-stack new-nums new-node-map)))))


(defn build-node-map [input]
  (let [generate (generate-node nil input)
        nums (:nums generate)
        root (:node generate)
        node-map {(:id root) root}
        stack (conj '() (:id root))
        gen1 (push-generations root nums stack node-map)
        result-map (pop-nodes (:nums gen1) (:stack gen1) (:nodes gen1))]
    {:root-id (:id root) :nodes result-map}))

(build-node-map day8-input)

(defn day8-p1 [input]
  (let [result-map (:nodes (build-node-map input))]
    (sum (mapcat #(:meta-data %) (vals result-map)))))

(defn day8-p2 [input]
  (let [result-map (build-node-map input)
        root-id (:root-id result-map)
        nodes (:nodes result-map)]
    (:value (root-id nodes))))


;(day8-p1 day8-input)                                        ;45868

;(day8-p2 day8-input)                                        ;19724

;;; day 9
;;; This could probably be done with just an array but I started with a map
;;; So just kept it that way. The memory overhead of just keeping the marbles that we
;;; are supposed to remove would be probably less than the overhead of using a map
;;; vs a vector


(defn next-marble [m pos]
  (get m (:next pos)))

(defn prev-marble [m pos]
  (get m (:prev pos)))

(defn walk-back [pos m times]
  (loop [t times
         pos pos]
    (if (<= t 0)
      pos
      (recur (dec t) (prev-marble m pos)))))

(defn place-marble [current-position marble marbles]
  (if (= 0 (mod marble 23))
    (let [score-init marble
          marble-to-take (walk-back current-position marbles 7)
          fixed-next (next-marble marbles marble-to-take)
          fixed-prev (prev-marble marbles marble-to-take)
          fixed-next (assoc fixed-next :prev (:val fixed-prev))
          fixed-prev (assoc fixed-prev :next (:val fixed-next))
          new-marbles (assoc (dissoc marbles (:val marble-to-take)) (:val fixed-prev) fixed-prev (:val fixed-next) fixed-next)]
      {:pos fixed-next :score (+ score-init (:val marble-to-take)) :marbles new-marbles})
    (let [new-prev (next-marble marbles current-position)
          new-next (next-marble marbles (next-marble marbles current-position))
          new-marble {:val marble :prev (:val new-prev) :next (:val new-next) }
          new-prev (assoc new-prev :next (:val new-marble))
          new-next (assoc new-next :prev (:val new-marble))
          new-marbles (assoc marbles marble new-marble (:val new-prev) new-prev (:val new-next) new-next)]
      {:pos new-marble :score 0 :marbles new-marbles})))

(defn day9-p1 [num-players num-marbles]
  (loop [marbles {0 {:val 0 :prev 1 :next 2} 2 {:val 2 :prev 0 :next 1} 1 {:val 1 :prev 2 :next 0}}
         curr-player 2                                      ;player numbers start at 0
         scores (into [] (replicate num-players 0))
         marble 3
         pos (get marbles 2)]
    (if (> marble num-marbles)
      scores
      (let [placed (place-marble pos marble marbles)
            score (:score placed)
            new-scores (update-in scores [curr-player] #(+ score %))]
        (recur (:marbles placed) (mod (inc curr-player) num-players) new-scores (inc marble) (:pos placed))))))

;(apply max (day9-p1 448 71628))                             ;394486
;(apply max (day9-p1 448 (* 100 71628)))                     ;3276488008


;;;Day 10

(def day10-test-input ["position=< 9,  1> velocity=< 0,  2>"
                       "position=< 7,  0> velocity=<-1,  0>"
                       "position=< 3, -2> velocity=<-1,  1>"
                       "position=< 6, 10> velocity=<-2, -1>"
                       "position=< 2, -4> velocity=< 2,  2>"
                       "position=<-6, 10> velocity=< 2, -2>"
                       "position=< 1,  8> velocity=< 1, -1>"
                       "position=< 1,  7> velocity=< 1,  0>"
                       "position=<-3, 11> velocity=< 1, -2>"
                       "position=< 7,  6> velocity=<-1, -1>"
                       "position=<-2,  3> velocity=< 1,  0>"
                       "position=<-4,  3> velocity=< 2,  0>"
                       "position=<10, -3> velocity=<-1,  1>"
                       "position=< 5, 11> velocity=< 1, -2>"
                       "position=< 4,  7> velocity=< 0, -1>"
                       "position=< 8, -2> velocity=< 0,  1>"
                       "position=<15,  0> velocity=<-2,  0>"
                       "position=< 1,  6> velocity=< 1,  0>"
                       "position=< 8,  9> velocity=< 0, -1>"
                       "position=< 3,  3> velocity=<-1,  1>"
                       "position=< 0,  5> velocity=< 0, -1>"
                       "position=<-2,  2> velocity=< 2,  0>"
                       "position=< 5, -2> velocity=< 1,  2>"
                       "position=< 1,  4> velocity=< 2,  1>"
                       "position=<-2,  7> velocity=< 2, -2>"
                       "position=< 3,  6> velocity=<-1, -1>"
                       "position=< 5,  0> velocity=< 1,  0>"
                       "position=<-6,  0> velocity=< 2,  0>"
                       "position=< 5,  9> velocity=< 1, -2>"
                       "position=<14,  7> velocity=<-2,  0>"
                       "position=<-3,  6> velocity=< 2, -1>"])

(def day10-input (read-file-lines "day10"))

(defn input-to-pts [inputs]
  (map (fn [input-str]
         (let [pos-and-velocity (re-matches #"position=<(.*)> velocity=<(.*)>" input-str)
               pos-str (second pos-and-velocity)
               vel-str (last pos-and-velocity)
               pos (map (comp parse-int s/trim ) (s/split pos-str #","))
               vel (map (comp parse-int s/trim ) (s/split vel-str #","))]
           {:pos (into [] pos) :delta (into [] vel)})) inputs))

(defn move-pt [pt]
  (let [init-pos (:pos pt)
        delta (:delta pt)]
    {:pos (into [] (map + init-pos delta)) :delta delta}))

(sort (map :pos (input-to-pts day10-test-input)))

(defn move-pts [pts]
  (map move-pt pts))

(defn print-pts [pts]
  (let [positions (sort (map :pos pts))
        min-x (dec (apply min (map first positions)))
        max-x (inc (apply max (map first positions)))
        max-y (inc (apply max (map last positions)))
        min-y (dec (apply min (map last positions)))
        lines (into [] (repeat (+ (math/abs max-y) (math/abs min-y)) (into [] (repeat (+ (math/abs max-x) (math/abs min-x)) "."))))]
    (loop [pt (first positions)
           pts (rest positions)
           lines lines]
      (if (nil? pt)
        (let [joined (map str lines)]
          (for [x joined] (println x)))
        (let [adj-pos (map - pt [min-x min-y])
              new-lines (update-in lines [(second adj-pos)] #(assoc % (first adj-pos) "#"))]
          (recur (first pts) (rest pts) new-lines))))))

(defn potential-word [pts]
  (let [positions (sort (map :pos pts))]
    (loop [cur-pos (first positions)
           next-pos (second positions)
           positions (rest positions)
           line-size 0]
      (if (= 4 line-size)                                   ;just a wild guess, assuming font size, etc. it worked :)
        true
        (if (nil? next-pos)
          false
          (if (and (= (first cur-pos) (first next-pos)) (= 1 (- (second next-pos) (second cur-pos))))
            (recur (first positions) (second positions) (rest positions) (inc line-size))
            (recur (first positions) (second positions) (rest positions) 0)))))))

(defn day10-p1 [input]
  (let [pts (input-to-pts input)]
    (loop [pts pts]
      (if (potential-word pts)
        (print-pts pts)
        (recur (move-pts pts))))))

(defn day10-p2 [input]
  (let [pts (input-to-pts input)]
    (loop [pts pts wait 0]
      (if (potential-word pts)
        wait
        (recur (move-pts pts) (inc wait))))))

;(day10-p1 day10-input)                                      ;RRANZLAC
;(day10-p2 day10-input)                                      ;10942
