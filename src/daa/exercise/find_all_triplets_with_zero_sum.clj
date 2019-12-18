(ns daa.exercise.find-all-triplets-with-zero-sum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find all triplets with zero sum                          ;;
;; Given an array of distinct elements.                     ;;
;; The task is to find triplets in array whose sum is zero. ;;
;;                                                          ;;
;; Examples :                                               ;;
;;                                                          ;;
;; Input : arr[] = {0, -1, 2, -3, 1}                        ;;
;; Output : 0 -1 1                                          ;;
;; 2 -3 1                                                   ;;
;;                                                          ;;
;; Input : arr[] = {1, -2, 1, 0, 5}                         ;;
;; Output : 1 -2  1                                         ;;
;;                                                          ;;
;; Solution (Sorting : O(n2))                               ;;
;;                                                          ;;
;; 1. Sort all element of array                             ;;
;; 2. Run loop from i=0 to n-2.                             ;;
;; Initialize two index variables l=i+1 and r=n-1           ;;
;; 4. while (l < r)                                         ;;
;; Check sum of arr[i], arr[l], arr[r] is                   ;;
;; zero or not if sum is zero then print the                ;;
;; triplet and do l++ and r--.                              ;;
;; 5. If sum is less than zero then l++                     ;;
;; 6. If sum is greater than zero then r--                  ;;
;; 7. If not exist in array then print not found.           ;;
;;                                                          ;;
;; Time Complexity : O(n2)                                  ;;
;; Auxiliary Space : O(1)                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-triplets-with-zero-sum [arr]
  (let [sorted-arr (vec (sort arr))
        n (count arr)]
    (dotimes [i (- n 2)]
      (loop [result [] l (inc i) r (dec n)]
        (if (< l r)
          (let [x (get sorted-arr i)
                y (get sorted-arr l)
                z (get sorted-arr r)
                sum (+ x y z)]
            (cond
              (= sum 0)
              (do
                (println (vector x y z))
                (recur (conj result (vector x y z)) (inc l) (dec r)))
              (< sum 0) (recur result (inc l) r)
              :else (recur result l (dec r))))
          result)
        )
      )
    )
  )


(comment

  (find-triplets-with-zero-sum [0 -1 2 -3 1]) ;; [-3 1 2] , [-1 0 1]

  (find-triplets-with-zero-sum [1 -2 1 0 5]) ;; [-2 1 1]

  )
