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

(def default-problem
  [3 0 0 0 0 5 0 1 0
   0 7 0 0 0 6 0 3 0
   1 0 0 0 9 0 0 0 0
   7 0 8 0 0 0 0 9 0
   9 0 0 4 0 8 0 0 2
   0 6 0 0 0 0 5 0 1
   0 0 0 0 4 0 0 0 6
   0 4 0 7 0 0 0 2 0
   0 2 0 6 0 0 0 0 3])
      
(with-test
(defn horizontal [board size n]
  "Finds the horizontal line of the cell"
  (let [first-line (* size (int (/ n size)))]
    (map #((vec board) %) (range first-line (+ first-line size)))
    )
)

(is (= [3 4] (horizontal [1 2 3 4] 2 3)))
(is (= [9 0 8 0 2 3 0 7 0] (horizontal board 9 57)))
)


(with-test 
  (defn vertical [board size n]
  "Finds the vertival column of the cell"
  (map #((vec board) (+ (rem n size) (* % size))) (range 0 size)))
  
  (is (= [2 4] (vertical [1 2 3 4] 2 1)))
  (is (= [0 0 8 5 4 0 0 0 0] (vertical board 9 10)))    
)

(with-test
  (defn illegal-values [board size n]
    "Finds the illegal values for the cell"
    (distinct (into (horizontal board size n) (vertical board size n)))
    )
  (is (= [0 4 5 6 9] (sort (illegal-values board 9 0))))
)

(with-test
  (defn legal-values [board size n]
    "Finds the legal values for a cell"
    (let [illegal (set (illegal-values board size n))]    
    (filter #(not (contains? illegal %)) (range 1 (+ size 1)))
    )
    )
  
  (is (= [1 2 3 7 8] (sort (legal-values board 9 0))))
  )

(with-test 
  (defn solve [board size]
    "Solves it"
    (let [solve-for-space (fn ! [board values size solution]
                            (if (or (not (empty? solution)) (empty? values)) 
                              solution
                              (! board (rest values) size (solve (assoc board (.indexOf board 0) (first values)) size))
                            )
                          )
          ]
    (if     
      (not (.contains board 0)) board      
      (solve-for-space board (legal-values board size (.indexOf board 0)) size [])       
    )
   ))
  (is (= [1 2 2 1] (solve [0 2 2 0] 2)))
)

(solve default-problem 9)