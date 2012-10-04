(ns exercises.exercises)
(use 'clojure.set)

(defn rangef [start end]
  "Write a function which creates a list of all integers in a given range."
  (loop [i (dec end) coll '()]
    (if (< i start)
      coll
      (recur (dec i) (list* i coll)))))

(defn get-max [& args]
  "Write a function which takes a variable number of parameters and returns the maximum value."
  (first (sort > args)))

(defn countdown-recur [x]
  "A recursive function that counts down from integer x to zero"
  (loop [i x coll '()]
    (if (zero? i)
      (into () coll)
      (recur (dec i) (cons i coll)))))

(defn flip [f]
  "Write a higher-order function which flips the order of the arguments of an input function."
  (fn [a b] (f b a)))

(defn sp-at [n coll]
  "Write a function which will split a sequence into two parts."
  [(take n coll) (drop n coll)])

(defn half-truth [& args]
  "Returns true if some of the parameters are true but if not all are true"
  (let [counts (group-by true? args)
        true-count (count (get counts true))]
    (if (= true-count (count args))
      false
      (boolean (some true? args)))))

(defn gcd [x y]
  "Calculates the greatest common divisor of two numbers"
  (let [rm (rem x y)]
    (if (= rm 0)
      y
      (gcd y rm))))

(defn cartesian [coll1 coll2]
  "Return the cartesian product of two collections"
  (into #{} (for [x coll1 y coll2]
              [x y])))

(defn mygroupby [f coll]
  "An implementation of group-by"
  (->> (partition-by f coll) (map #(apply vector %)) (reduce #(assoc % (f (first %2)) %2) {})))

(defn pascal [size]
  "Return the nth row [size] of Pascal's Triangle"
  (loop [x 1 row [1]]
    (if (= x size)
      (vec row)
      (recur (inc x) (cons 1 (conj (into [] (map #(apply + %) (partition 2 1 row))) 1))))))

(defn symmetric-diff [set1 set2]
  "Returns the symmetric difference between two sets"
  (into #{} (concat (difference set1 set2) (difference set2 set1))))

(defn read-binary-number [n]
  "Convert a string representation of a binary number to its base 10 form"
  (let [integer-seq (map #(Character/digit % 10) n)
        powers (reverse (range 0 (count integer-seq)))]
    (reduce + (map #(if-not (zero? %) (int (Math/pow 2 %2)) 0) integer-seq powers))))

(defn infix [arg & more]
  (loop [rem more
         result arg]
    (println rem)
    (if (nil? (second rem))
      result
      (recur (rest rem) ((first rem) result (second rem))))))
