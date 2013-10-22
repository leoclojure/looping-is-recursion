(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [seq]
                 (if (empty? (rest seq))
                   (first seq)
                   (recur (rest seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (loop [seq1 seq1
         seq2 seq2]
    (cond
     (and  (empty? seq1) (empty? seq2)) true
     (or   (empty? seq1) (empty? seq2)) false
     (not= (first seq1) (first seq2))   false
     :else                              (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         pred pred
         seq a-seq]
    (cond
     (empty? seq)       nil
     (pred (first seq)) index
     :else              (recur (inc index) pred (rest seq)))))

(defn avg [a-seq]
  (loop [acc 0
         seq a-seq]
    (cond
     (empty? seq) (/ acc (count a-seq))
     :else        (recur (+ acc (first seq)) (rest seq)))))

(defn parity [a-seq]
  (loop [input a-seq
         set #{}]
    (cond
     (empty? input)                set
     (contains? set (first input)) (recur (rest input) (disj set (first input)))
     :else                         (recur (rest input) (conj set (first input))))))

(defn fast-fibo [n]
  (loop [round 2
         fn1 1
         fn2 1]
    (cond
     (= n 0)     0
     (= n 1)     1
     (= n 2)     1
     (< round n) (recur (inc round) fn2 (+ fn1 fn2))
     :else       fn2)))

(defn cut-at-repetition [a-seq]
  (loop[output '[]
        input  a-seq]
    (cond
     (empty? input)                     output
     (some #(= (first input) %) output) output
     :else                              (recur (conj output (first input)) (rest input)))))

