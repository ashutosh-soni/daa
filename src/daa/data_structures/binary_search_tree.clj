(ns daa.binary-search-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binary Search Tree is a node-based binary tree data structure                 ;;
;; which has the following properties:                                           ;;
;;                                                                               ;;
;; 1. The left subtree of a node contains only nodes with keys lesser            ;;
;;    than the node’s key.                                                       ;;
;; 2. The right subtree of a node contains only nodes with keys greater          ;;
;;    than the node’s key.                                                       ;;
;; 3. The left and right subtree each must also be a binary search tree.         ;;
;;                                                                               ;;
;; * Insertion of a key                                                          ;;
;; A new key is always inserted at leaf. We start searching a key from root      ;;
;; till we hit a leaf node. Once a leaf node is found,                           ;;
;; the new node is added as a child of the leaf node.                            ;;
;;                                                                               ;;
;; * Searching a key                                                             ;;
;; To search a given key in Binary Search Tree, we first compare it with root,   ;;
;; if the key is present at root, we return root. If key is greater than         ;;
;; root’s key, we recur for right subtree of root node.                          ;;
;; Otherwise we recur for left subtree.                                          ;;
;;                                                                               ;;
;; * Deletion                                                                    ;;
;; When we delete a node, three possibilities arise.                             ;;
;; 1. Node to be deleted is leaf: Simply remove from the tree.                   ;;
;; 2. Node to be deleted has only one child: Copy the child to the node          ;;
;;    and delete the child.                                                      ;;
;; 3. Node to be deleted has two children: Find inorder successor of the node.   ;;
;;    Copy contents of the inorder successor to the node and                     ;;
;; delete the inorder successor. Note that inorder predecessor can also be used. ;;
;;                                                                               ;;
;; NOTE: The important thing to note is, inorder successor is needed only        ;;
;; when right child is not empty. In this particular case, inorder successor     ;;
;; can be obtained by finding the minimum value in right child of the node.      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
