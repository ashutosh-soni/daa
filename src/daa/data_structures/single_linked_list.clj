(ns daa.data-structures.single-linked-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linked list can be visualized as a chain of nodes,                ;;
;; where every node points to the next node.                         ;;
;; Simple Linked List −> Item navigation is forward only             ;;
;;                                                                   ;;
;; * Linked List contains a link element called first.               ;;
;;                                                                   ;;
;; * Each link carries a data field(s) and a link field called next. ;;
;;                                                                   ;;
;; * Each link is linked with its next link using its next link.     ;;
;;                                                                   ;;
;; * Last link carries a link as null to mark the end of the list.   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrecord Node [data next])

(defn insertion
  "Adds an element at the beginning of the list"
  [{:keys [data next] :as linked-list} value]
  (if-not value
    linked-list
    (cond
      (nil? linked-list) (Node. value nil)
      :else (Node. value linked-list))))


(defn deletion
  "Deletion an element at the beginning of the list"
  [{:keys [data next] :as linked-list}]
  (cond
    (nil? linked-list) nil
    :else next))


(defn traverse [{:keys [data next] :as linked-list}]
  (if (nil? linked-list)
    `()
    `(~data ~@(traverse next))))

(defn delete
  "Deletes an element using the given key"
  [{:keys [data next] :as linked-list} key]
  (if (nil? key)
    linked-list
    (cond
      (nil? linked-list) nil
      (= data key) next
      :else (Node. data (delete next key)))))

(defn search
  "This function return true if key exist in linked list otherwise false."
  [{:keys [data next] :as linked-list} key]
  (if (nil? key)
    false
    (cond
      (nil? linked-list) false
      (= data key) true
      :else (search next key))))


(def make-list #(reduce insertion nil %))

(comment

  (def my-list (make-list '(5 8 2 3 4 1)))

  (traverse my-list) ;; (1 4 3 2 8 5)

  (traverse (deletion my-list)) ;; (4 3 2 8 5)

  (traverse (delete my-list 2)) ;; (1 4 3 8 5)

  (search my-list 2) ;; true

  (search my-list 19) ;; false

  )
