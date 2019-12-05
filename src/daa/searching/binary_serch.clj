(ns daa.searching.binary-serch)


(defn binary-search
  "Binary Search is the most popular search Algorithm.
  Prerequisite:It works on sorted array.
  OUTPUT:
  -1 : if not able to find the target item into array otherwise
  retrun index i.
  NOTE: Thi function was designed to work with vector array.
  "
  [arr target]
  (let [high-index (dec (count arr))]
    (loop [low 0, high high-index]
      (if (> low high) -1
          (let [mid (bit-shift-right (+ low high) 1)
                mid-value (get arr mid)]
            (cond
              (> target mid-value) (recur (inc mid) high)
              (< target mid-value) (recur low (dec mid))
              (= target mid-value) mid
              :else -1
              ))))))



(comment

  (def arr (vec (take 10 (range 0 11))))


  (binary-search arr 9)

  )
