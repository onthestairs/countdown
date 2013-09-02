(ns countdown-simon.core)

(def operators
  ['+ '- '* '/])

(defn find-solutions [target ns]
  (let [evaled-solutions (eval-solutions target ns)
        new-solutions (if (> (count ns) 1)
                        (reduce-pairs target ns)
                        `())
        all-solutions (concat evaled-solutions new-solutions)]
    all-solutions))

(defn eval-solutions [target ns]
  (filter (fn [n]
            (= target (eval n))) ns))

(defn eval-solutions [target ns]
  ;(println "<" ns ">")
  ns)

(defn reduce-pairs [target ns]
  (let [pair-and-rests (pairs ns)]
    (mapcat (fn [pair-and-rest]
              (let [pair (:pair pair-and-rest)
                    rest (:rest pair-and-rest)
                    a (first pair)
                    b (last pair)
                    solutionss (mapcat (fn [operator]
                                         (let [c (list operator a b)
                                               new-ns (cons c rest)
                                               found-solutions (if (should-compute? operator a b)
                                                                 (find-solutions target new-ns)
                                                                 '())]
                                           found-solutions)) operators)]
                solutionss)) pair-and-rests)))

(defn should-compute? [operator a b]
  (cond
   (= operator '+) (> (eval a) (eval b))
   (= operator '*) (> (eval a) (eval b))
   (= operator '/) (not= (eval b) 0)
   :else true))

(defn coll-rotations [coll]
  (let [count (count coll)]
    (map (fn [n]
           (concat
            (take-last (- count n) coll)
            (take n coll))) (range count))))

(defn pairs [ns]
  (let [rotations (coll-rotations ns)]
    (flatten (concat (map (fn [rotation]
                            (let [a (first rotation)
                                  remaining (rest rotation)
                                  rotations-2 (coll-rotations remaining)]
                              (map (fn [rotation-2]
                                     (let [b (first rotation-2)
                                           rest-2 (rest rotation-2)]
                                       {:pair [a b]
                                        :rest rest-2})) rotations-2))) rotations)))))

