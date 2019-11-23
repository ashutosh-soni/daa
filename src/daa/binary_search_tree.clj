(ns daa.binary-search-tree)

(defrecord Node [data left right])


(defn insert [{:keys [data left right] :as tree} value]
  (if-not value
    tree
    (cond
      (nil? tree) (Node. value nil nil)
      (< value data) (Node. data (insert left value) right)
      (> value data) (Node. data left (insert right value))
      :else tree)))

(defn to-list [{:keys [data left right] :as tree}]
  (when tree
    `(~@(to-list left) ~data ~@(to-list right))))

(defn min-value [{:keys [data left right] :as tree}]
  (if left
    (recur left)
    data))

(defn max-value [{:keys [data left right] :as tree}]
  (if right
    (recur right)
    data))

(defn count-node [{:keys [left right] :as tree}]
  (if tree
    (+ 1 (count-node left) (count-node right))
    0))

(defn height
  ([tree] (height tree 0))
  ([tree level-count]
   (if tree
     (max (height (:left tree) (inc level-count))
          (height (:right tree) (inc level-count)))
     level-count)))


(defn remove-value [{:keys [data left right] :as tree} value]
  (cond
    (nil? tree) nil
    (< value data) (Node. data (remove-value left value) right)
    (> value data) (Node. data left (remove-value right value))
    (nil? left) right
    (nil? right) left
    :else (let [min (min-value right)]
            (Node. min left (remove-value right min )))))


(defn contain-value? [{:keys [data left right] :as tree} value]
  (cond
    (nil? tree) false
    (< value data) (recur left value)
    (> value data) (recur right value)
    :else true))

(defn bst?
  ([tree] (bst? tree Integer/MIN_VALUE Integer/MAX_VALUE))
  ([{:keys [data left right] :as tree} min-value max-value]
   (cond
     (nil? tree) true
     (or (< data min-value) (> data max-value)) false
     :else (and (bst? left min-value (dec data))
                (bst? right (dec data) max-value)))))


(def to-tree #(reduce insert nil %))


(comment
  (def tree (to-tree '(5 8 2 3 4 1)))

  (bst? tree) ; true


  (count-node tree) ; 6
  (height tree) ; 4
  (max-value tree) ; 8
  (min-value tree) ; 1
  (to-list (remove-value tree 3)) ; (1 2 4 5 8)
  (contain-value? tree 2)
  ) ; true
