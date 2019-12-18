(ns daa.exercise.find-second-largest-number-in-an-array)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find Second largest element in an array                                     ;;
;; Given an array of integers, our task is to write a program that efficiently ;;
;; finds the second largest element present in the array.                      ;;
;;                                                                             ;;
;; Example:                                                                    ;;
;;                                                                             ;;
;; Input : arr[] = {12, 35, 1, 10, 34, 1}                                      ;;
;; Output : The second largest element is 34.                                  ;;
;;                                                                             ;;
;; Input : arr[] = {10, 5, 10}                                                 ;;
;; Output : The second largest element is 5.                                   ;;
;;                                                                             ;;
;; Input : arr[] = {10, 10, 10}                                                ;;
;; Output : The second largest does not exist.                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn find-second-largest [arr]
  (if (> (count arr) 1)
    (loop [arr arr largest Integer/MIN_VALUE second-largest Integer/MIN_VALUE]
      (if (empty? arr)
        (if (= second-largest Integer/MIN_VALUE)
          "The Second largest does not exist"
          second-largest)
        (let [item (first arr)]
          (cond
            (> item largest) (recur (rest arr) item largest)
            (and (> item second-largest) (not (= item largest)))
            (recur (rest arr) largest item)
            :else (recur (rest arr) largest second-largest)))
        ))
    "The Second largest does not exist"
    ))




(comment

  (find-second-largest [12 35 1 10 34 1]) ;; 34

  (find-second-largest [10 5 10])  ;; 5

  (find-second-largest [10 10 10]) ;; "The Second largest does not exist"

)
