(ns daa.exercise.palindrome-in-singly-linked-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given a singly linked list of characters,                ;;
;; write a function that returns true                       ;;
;; if the given list is a palindrome, else false.           ;;
;;                                                          ;;
;; for example:                                             ;;
;; 1. R->A->D->A->R                                         ;;
;;   return true                                            ;;
;;                                                          ;;
;; 2. R->B->D->A->R                                         ;;
;;  return false                                            ;;
;;                                                          ;;
;; APPROACH (USE OF STACK)                                  ;;
;; 1. A simple solution is to use a stack of list nodes.    ;;
;;    This mainly involves three steps.                     ;;
;; 2. Traverse the given list from head to tail             ;;
;;    and push every visited node to stack.                 ;;
;; 3. Traverse the list again. For every visited node,      ;;
;;    pop a node from stack and compare data of popped node ;;
;;    with currently visited node.                          ;;
;; 4. If all nodes matched, then return true, else false.   ;;
;;                                                          ;;
;; The time complexity of the above method is O(n).         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Node [data next])

(defn insertion [{:keys [data next] :as linked-list} value]
  (if (nil? value)
    linked-list
    (cond
      (nil? linked-list) (Node. value nil)
      :else (Node. value linked-list))))

(defn traverse [{:keys [data next] :as linked-list}]
  (if (nil? data)
    `()
    `(~data ~@(traverse next))))

(defn visited-stack [linked-list]
  (loop [stack [] {:keys [data next] :as linked-list} linked-list]
    (if (nil? data)
      stack
      (let [new-stack (conj stack data)]
        (recur new-stack next))))
  )

(defn is-palindrome [{:keys [data next] :as linked-list}]
  (let [stack (visited-stack linked-list)]
    (loop [status true stack stack {:keys [data next] :as linked-list} linked-list]
      (if status
        (let [item (peek stack)]
          (cond
            (nil? item) status
            (nil? data) status
            (= item data) (recur true (pop stack) next)
            :else (recur false (pop stack) next))
          )
        false)

      ))
  )


(def make-list #(reduce insertion nil %))


(comment
  (def my-tree (make-list '("R" "A" "D" "A" "R")))

  (def not-palin (make-list '("R" "A")))

  (is-palindrome not-palin) ;; false

  (is-palindrome my-tree) ;; true

  )
