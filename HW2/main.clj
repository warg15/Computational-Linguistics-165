;;William Argus
;;A12802324

;;Problem 1
(defn sequence-to-power [x n]
  (if (> n 0)
     (concat (sequence-to-power x (- n 1)) x  )
    )
  )
;;(sequence-to-power (list 'abz 'b 2) 4)

;;Problem 2
;;note that this function is derived in lecture and is used here, 
;;credit to Leon Bergen
(defn prefix? [pr str] 
  (if (> (count pr) (count str))
    false
    (if (empty? pr)
      true
      (if (= (first pr) (first str))
        (prefix? (rest pr) (rest str))
        false))))
(defn in-L? [x]
  (if (empty? x)
    true
    (if (prefix? '(a) x)
      (in-L? (rest x))
      false)))
;;(in-L? (list 'a 'a 'a))

;;Problem 3
(defn sequence-to-power [x n]
  (if (> n 0)
     (concat (sequence-to-power x (- n 1)) x  )
    )
  )
(defn generate-an-bn [k]
  (concat (sequence-to-power (list 'a) k) (sequence-to-power (list 'b) k)  ) 
)
;;(generate-an-bn 5)

;;Problem 4
(defn reverse [l]
              (if (empty? l)
                '()
                (
                 concat (reverse (rest l))  (cons (first l) '())
                 )  
               
                )
 )

(defn remove-last-element [l]
  (reverse (rest (reverse l) ) )
  
  )

;;Problem 5
(defn remove-last-element [l]
  (reverse (rest (reverse l) ) )
  )

(defn recognize-an-bn [str]
  (if (empty? str) 
    true
    (if (not= (first str) 'a)
      false
      (if (not= (last str) 'b) 
        false
        (if (odd? (count str))
        false
           (if (even? (count str))
              (recognize-an-bn (rest   (remove-last-element str)     )) 
        )
      )
     )
    )
  )
  )
;;(recognize-an-bn (list 'a 'a 'a 'b 'b 'b))

;;Problem 6
(defn concat-L-A [l a]
  (if (= (count l) 1)
    (list (concat  (first l) a)    )
  (cons (concat (first l) a ) (concat-L-A(rest l) a) )
  ) 
)
;;(concat-L-A (list (list 'a 'a) (list 3 45 6 7 878 7 7 66)(list 'b 'b)
;; (list 2 4)(list 3 4)) (list 'a) )

;;Problem 7
;;Language A: (list (list ‘a ‘a) )
;;Language B: (list (list ‘a) )
;;concat(A,B) = concat(B,A) = (list (list ‘a ‘a ‘a) )

;;Problem 8
;;Language A: (list (list ‘a ‘b ‘c))
;;Language B: (list (list ‘d) (list  ‘k) )
;;concat(A,B) = (list (list ‘a ‘b ‘c ‘d) (list ‘a ‘b ‘c ‘k) )
;;concat(B,A) = (list (list ‘d ‘a ‘b ‘c) (list ‘k ‘a ‘b ‘c) )
;;Clearly, concat(A,B) not equal concat(B,A) 

;;Problem 9
;;Language: (list ’() )
;;ie the empty language is the only one that meets this property, as 
;; if the language had any strings, the lengths would be changed,
;; making there no way that the language would be preserved if concatenated with 
;; itself






















