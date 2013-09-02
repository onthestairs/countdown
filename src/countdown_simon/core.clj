(ns countdown-simon.core)

(def operators
  ['+ '- '* '/])

(defn locate-solutions [target ns]
  (let [solutions (find-solutions target (map (fn [n]
                                                 {:value n :exp n}) ns))]
    (map (fn [solution]
           (:exp solution)) solutions)))

(defn print [coll]
  (map (fn [item]
         (println item)) coll))

(defn find-solutions [target ns]
  (let [evaled-solutions (eval-solutions target ns)
        new-solutions (if (> (count ns) 1)
                        (reduce-pairs target ns)
                        `())
        all-solutions (concat evaled-solutions new-solutions)]
    all-solutions))

(defn eval-solutions [target ns]
  (filter (fn [n]
            (= target (:value n))) ns))

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
                                         (if (should-compute? operator (:value a) (:value b))
                                           (let [exp (list operator (:exp a) (:exp b))
                                                 value ((eval operator) (:value a) (:value b))
                                                 n {:exp exp :value value}
                                                 new-ns (cons n rest)]
                                             (find-solutions target new-ns))
                                           '())) operators)]
                solutionss)) pair-and-rests)))

(defn should-compute? [operator a b]
  (cond
   (= operator '+) (> a b)
   (= operator '*) (> a b)
   (= operator '/) (not= b 0)
   :else true))

(defn coll-rotations [coll]
  (let [count (count coll)]
    (map (fn [n]
           (concat
            (take-last (- count n) coll)
            (take n coll))) (range count)))))

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

