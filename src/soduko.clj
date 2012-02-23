(ns soduko 
  (:use clojure.test)
)

(def board
[
0 0	0	0	0	0	6	4 0
0	0	0	0	8	9	0	0	0
0	8	0	4	7	0	9	0	0
0	5	0	7	3	4	1	0	0
0	4	7	8	0	2	5	9	0
0	0	3	9	5	1	0	6	0
9	0	8	0	2	3	0	7	0
5	0	0	6	4	0	0	0	0
0 0 0 0 0 0 0 0 0
])
      
(with-test
(defn horizontal [board size n]
  "Finds the horizontal line of the cell"
  (let [first-line (* size (int (/ n size)))]
    (map #(board %) (range first-line (+ first-line size)))
    )
)

(is (= [3 4] (horizontal [1 2 3 4] 2 3)))
(is (= [9 0 8 0 2 3 0 7 0] (horizontal board 9 57)))
)


(with-test 
  (defn vertical [board size n]
  "Finds the vertival column of the cell"
  (map #(board (+ (rem n size) (* % size))) (range 0 size)))
  
  (is (= [2 4] (vertical [1 2 3 4] 2 1)))
  (is (= [0 0 8 5 4 0 0 0 0] (vertical board 9 10)))    
)


(run-tests)