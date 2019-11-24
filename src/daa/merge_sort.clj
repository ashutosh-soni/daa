(ns daa.merge-sort)

(defn merge-seqs
  "Merges two sorted sequences into a single sorted sequence"
  [left right]
  (loop [l left, r right, result []]
    (let [lfirst (first l), rfirst (first r)]
      (cond
        (nil? lfirst) (concat result r)
        (nil? rfirst) (concat result l)
        (<= lfirst rfirst) (recur (rest l) r (conj result lfirst))
        :else (recur l (rest r) (conj result rfirst))))))

(defn half [coll]
  (let [middle (bit-shift-right (count coll) 1)] ; fast division by 2
    (split-at middle coll)))

(defn mergesort
 [coll]
  (if (< (count coll) 2)
    coll
    (let [[left right] (half coll)]
      (merge-seqs (mergesort left) (mergesort right)))))


(comment
  (def num-list (shuffle (range 28 34)))

  (println num-list)

  (mergesort num-list)
  )
