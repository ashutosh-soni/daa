(ns daa.exercise.find-maximum-product-of-a-triplet-in-array)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximum product of a triplet (subsequnece of size 3) in array                ;;
;;                                                                              ;;
;; Given an integer array, find a maximum product of a triplet in array.        ;;
;;                                                                              ;;
;; Examples:                                                                    ;;
;;                                                                              ;;
;; Input:  [10, 3, 5, 6, 20]                                                    ;;
;; Output: 1200                                                                 ;;
;; Multiplication of 10, 6 and 20                                               ;;
;;                                                                              ;;
;; Input:  [-10, -3, -5, -6, -20]                                               ;;
;; Output: -90                                                                  ;;
;;                                                                              ;;
;; Input:  [1, -4, 3, -6, 7, 0]                                                 ;;
;; Output: 168                                                                  ;;
;;                                                                              ;;
;;                                                                              ;;
;; Approach : O(n) Time, O(1) Space                                             ;;
;;                                                                              ;;
;; 1. Scan the array and compute Maximum, second maximum and                    ;;
;;    third maximum element present in the array.                               ;;
;; 2. Scan the array and compute Minimum and second minimum                     ;;
;;    element present in the array.                                             ;;
;; 3. Return the maximum of product of Maximum, second maximum and              ;;
;;    third maximum and product of Minimum, second minimum and Maximum element. ;;
;; Note – Step 1 and Step 2 can be done in single traversal of the array.       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn max-in-array
  "This function helps to find max value in given array"
  ([arr] (max-in-array arr Integer/MAX_VALUE))
  ([arr smaller-then]
   (loop [arr arr r Integer/MIN_VALUE]
     (if (empty? arr)
       r
       (let [x (first arr)
             rarr (rest arr)]
         (cond
           (and (> x r) (< x smaller-then)) (recur rarr x)
           :else (recur rarr r))
         ))
     ))
  )

(defn min-in-array
  "This function helps to find min value in given array"
  ([arr] (min-in-array arr Integer/MIN_VALUE))
  ([arr greater-then]
   (loop [arr arr r Integer/MAX_VALUE]
     (if (empty? arr)
       r
       (let [x (first arr)
             rarr (rest arr)]
         (cond
           (and (< x r) (> x greater-then)) (recur rarr x)
           :else (recur rarr r))
         ))
     )
   ))


(defn find-max-product-of-triplet [arr]
  (let [fmax (max-in-array arr)
        smax (max-in-array arr fmax)
        tmax (max-in-array arr smax)
        fmin (min-in-array arr)
        smin (min-in-array arr fmin)]
    (max (* fmax smax tmax) (* fmin smin fmax)))

  )


(comment

  (find-max-product-of-triplet [10 3 5 6 20]) ;; 1200

  (find-max-product-of-triplet [-10 -3 -5 -6 -20]) ;; -90

  (find-max-product-of-triplet [1 -4 3 -6 7 0]) ;; 168

  )
