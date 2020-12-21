;;LIGN 165 Homework 1

;;Problem 1
(defn abs [a] (Math/sqrt (* a a)))

;;Problem 2
;;(defn take-square (* x x)) 
;;There is no parameter declaration for x, which should  be in brackets. Corrected version shown below
(defn take-square [x] (* x x))
;;(defn sum-of-squares [(take-square x) (take-square y)] (+ (take-square x) (take-square y)))
;;The parameters brackets define what the inputs are called in the function, they should not be calling the take-square function.
(defn sum-of-squares [x y] (+ (take-square x) (take-square y)))

;;Problem 3
(def exp-13-1 (- 13 0))
(def exp-13-2 (* 13 1))
(def exp-13-3(+ 10 1 2))
(def exp-13-4(+ 69 -56 ))

;;Problem 4
(defn third [l]
  (first (rest (rest l)))
)

;;Problem 5
;;is it just the composure of the two lists?
(defn compose  [f g ] (fn [x]
              (f (g  x ) )
))

;;(defn sqrt [x] (Math/sqrt x))
;;(defn abs [x] (Math/abs x))
;;((compose sqrt abs) -16)


;;Problem 6
(defn first-two [l]
  (list (first l) (second l))
)

;;Problem 7
(defn remove-second [l]
  (cons (first l) (rest (rest l)))
)

;;Problem 8
(defn add-to-end [l x]
  (concat l [x])
)

;;Problem 9
(defn reverse [l]
              (if (empty? l)
                '()
                (
                 concat (reverse (rest l))  (cons (first l) '())
                 )  
               
                )
 )

;;Problem 10
(defn count-to-1 [n]
              (if (= 1 n)
                    (list 1)
               ( cons n (count-to-1 (- n 1))  ) 
              )
 )

;;Problem 11
(defn count-to-n [n]
  (reverse (count-to-1 n))
  
  )

;;Problem 12
(defn get-max [l]
  (  if (= (count l) 1)
    (first l)
   ;;else
    (if (> (first l) (get-max (rest l)) ) 
      (first l)
      ;;else
      ( get-max ( rest l))
      )
  )
 )

;;Problem 13
(defn greater-than-five? [l]
              (if (empty? l)
                   '()
               (if (> (first l) 5 ) 
                 (cons  true (greater-than-five? (rest l)) )
                 (cons  false (greater-than-five? (rest l)) )
                )
                )
 )

;;Problem 14
(defn concat-three [l li lis]
  (concat l li lis)
  
  )















