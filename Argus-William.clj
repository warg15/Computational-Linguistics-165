;;William Argus
;;A12802324
;;Collaborated with Colman Dekker via a phone call in which we discussed the homework
;;questions to ensure mutual understanding and gave guidance to each other in debugging
;;The code was mainly done individually with us just sort of checking up with each other after
;;each problem to make sure we agreed on directions and general approach 

;;Problem 1
(def vocabulary '(call me ishmael))
(def theta1 (list (/ 1 2 ) (/ 1 4 ) (/ 1 4 )))
(def theta2 (list (/ 1 4 ) (/ 1 2 ) (/ 1 4 )))
(def thetas (list theta1 theta2))
(def theta-prior (list (/ 1 2) (/ 1 2)))

(defn score-categorical [outcome outcomes params]
   (if (empty? params)
      (/ 1 0)
         (if (= outcome (first outcomes))
            (first params)
            (score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
   1
   (if (empty? lst)
     base
      (f (first lst)
         (list-foldr f base (rest lst)))))

(defn log2 [n]
   (/ (Math/log n) (Math/log 2)))

(defn score-BOW-sentence [sen probabilities]
   (list-foldr
      (fn [word rest-score]
         (+ (log2 (score-categorical word vocabulary probabilities))
            rest-score))
               0
               sen))

(defn score-corpus [corpus probabilities]
   (list-foldr
     (fn [sen rst]
       (+ (score-BOW-sentence sen probabilities) rst))
        0
        corpus))

(defn logsumexp [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
    (log2
     (apply +
      (map (fn [z] (Math/pow 2 z))
       (map (fn [x] (- x mx)) log-vals)))))))


(def my-corpus '((call me) (call ishmael)))

(defn theta-corpus-joint [theta corpus theta-probs]
  (+ (score-corpus corpus theta) (log2 (first theta-probs)))
  )
;(theta-corpus-joint theta1 my-corpus theta-prior)
;;Problem 2

(defn compute-marginal [corpus theta-probs]
 (logsumexp (map + (map (fn [theta] (score-corpus corpus theta)) thetas)                (map (fn [y] (log2 y) ) theta-probs)  )))
;(compute-marginal my-corpus theta-prior)
;;Problem 3
(defn compute-conditional-prob [theta corpus theta-probs]
  (- (theta-corpus-joint theta corpus theta-probs) (compute-marginal my-corpus theta-probs))
  )
;;Problem 4
(defn compute-conditional-dist [corpus theta-probs]
  (map (fn [x] (compute-conditional-prob x corpus theta-probs)) thetas)       
  )
;;Problem 5
;(compute-conditional-dist my-corpus theta-prior)

;(map (fn [smthg] (Math/pow 2 smthg) ) (compute-conditional-dist my-corpus theta-prior))
;;Problem 6
(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
   (let [conditional-dist (map (fn [smthg] (Math/pow 2 smthg) ) (compute-conditional-dist observed-corpus theta-probs))]
     
     (compute-marginal new-corpus conditional-dist)))

;(compute-posterior-predictive my-corpus my-corpus theta-prior)

;;This quantity represents the likelihood of getting this corpus calculated using the updated prior
;;distribution for thetas, (which was found based on the liklihood of each theta in the observed 
;;corpus). This updated prior favors theta 1 over theta 2, which makes sense as theta 1
;;gives higher probability (½) to “call”, which is used twice in this corpus, while theta 2
;;gives higher probability to “me”, which is used only once. 
;;
;;This likelihood is greater than the original liklihood (after exponentiation, this likelihood
;;is 0.0130 while the likelihood from Problem 2 after exponentiation is 0.01172), which makes
;;sense as the prior for the thetas used for this value is an updated prior updated with information
;;from the observed-corpus, weighting theta 1 more heavily than theta 2, and since theta 1 has a 
;;higher probability for a word that occurs twice “call”, it means the overall likelihood of the 
;;new-corpus will be higher with these updated priors.
;;Problem 7
(defn normalize [params]
    (let [sum (apply + params)]
       (map (fn [x] (/ x sum)) params)))

(defn flip [weight]
   (if (< (rand 1) weight)
       true
      false))

(defn sample-categorical [outcomes params]
    (if (flip (first params))
       (first outcomes)
        (sample-categorical (rest outcomes)
        (normalize (rest params)))))

(defn repeat [f n]
   (if (= n 0)
     '()
     (cons (f) (repeat f (- n 1)))))

(defn sample-BOW-sentence [len probabilities]
    (if (= len 0)
      '()
     (cons (sample-categorical vocabulary probabilities)
      (sample-BOW-sentence (- len 1) probabilities))))


(defn sample-BOW-corpus [theta sent-len corpus-len]
  (repeat  (fn [] (sample-BOW-sentence sent-len theta)) corpus-len)  
)

;(sample-BOW-corpus theta1 3 3)

;;Problem 8
(defn sample-theta-corpus [sent-len corpus-len theta-probs]
  (let [theta (sample-categorical thetas theta-probs) ]
     (list theta (sample-BOW-corpus theta sent-len corpus-len))))

;(sample-theta-corpus 1 4 theta-prior)

;;Problem 9
(defn get-theta [theta-corpus]
   (first theta-corpus))

(defn get-corpus [theta-corpus]
    (first (rest theta-corpus)))

(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
    (repeat (fn [] (sample-theta-corpus sent-len corpus-len theta-probs))             sample-size))

(defn repeat [f n]
   (if (= n 0)
     '()
     (cons (f) (repeat f (- n 1)))))

(defn estimate-corpus-marginal [corpus sample-size sent-len corpus-len theta-probs]
  (let [clst (map (fn [xz] (get-corpus xz)) (sample-thetas-corpora sample-size sent-len corpus-len theta-probs) )]  
  (let [ numTrue (count (remove false? (map (fn [y] (= y corpus)) clst)))]
    (/ numTrue (count clst))    
    ))
)
     
;(estimate-corpus-marginal my-corpus 1000 2 2 theta-prior)

;;Problem 10
;(estimate-corpus-marginal my-corpus 50 2 2 theta-prior)
;;It is noticed that every probability calculated here is divisible by
;;2, which makes sense as the total number of samples is only 50, and it varies usually between 
;;0 and 0.2, with the occasional 0.4. This makes sense as the average for Problem 2 after
;;exponentiation is around 0.01172, so these results are expected to average very close to 
;;that number.
;(estimate-corpus-marginal my-corpus 9000 2 2 theta-prior)
;;It is noticed that every probability calculated here has a much larger (more accurate) number of
;;decimal places which makes sense as the total number of samples is 9000. The results from this
;;call are much closer together which makes sense because this is done over a lot more samples 
;;than the previous (50 samples). The results usually average around 0.0117; this makes
;;sense as the average for Problem 2 after exponentiation is around 0.01172, so these results are
;;very close to that number, as expected. In summary, this is a very accurate way to
;;estimate the marginal likelihood.


;;Problem 11
(defn get-count [obs observation-list count]
    (if (empty? observation-list)
       count
       (if (= obs (first observation-list))
         (get-count obs (rest observation-list) (+ 1 count))
         (get-count obs (rest observation-list) count))))

(defn get-counts [outcomes observation-list]
    (let [count-obs (fn [obs] (get-count obs observation-list 0))]
      (map count-obs outcomes)))

(defn rmv [my-crpus theta rlst]
  (if (= my-crpus (first (rest rlst))   )    
    rlst
    '()
      )
  )

(defn rmvtheta [my-crpus theta slst]
  (if (= theta (first slst)   )    
    slst
    '()
      )
  )

(defn cnt-empty [tlst]
  (if (empty? tlst)
    0
    (if (empty? (first tlst))
     (+ 0 (cnt-empty (rest tlst) ))
    (+ 1 (cnt-empty (rest tlst) ) )
    )
    )
  )


(defn rejection-sampler [theta observed-corpus sample-size sent-len corpus-len theta-probs]
      (let [clst (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)]
      (let [dlst (map (fn [y] (rmv observed-corpus theta y)) clst )  ]
      (let [elst (map (fn [y] (rmvtheta observed-corpus theta y)) dlst )  ]
      ( / (cnt-empty elst) (cnt-empty dlst)  )
  ))))


;(rejection-sampler theta1 my-corpus 100 2 2 theta-prior)


;;Problem 12
;(rejection-sampler theta1 my-corpus 100 2 2 theta-prior)
;;It is observed that this function call results in sporadic, unstable outputs, going between
;;0, 1, ##NaN, and occasionally decimals.
;;Calling it 1000 times gets an output that’s always a number instead of occasionally getting
;;#NaN, but the numbers very wildly. This can be stable in that there is always some 
;;numerical output, but it is not stable in that the numerical output has no consistency
;;In order to get a consistent numerical output, the sample-size is increased to the maximum
;;the compiler allows: 8000. At 8000 samples the estimate of the conditional probability
;;of theta1 is centered around an average of 0.6666; though outputs are seen to stray as 
;;far as 0.53 or 0.76 it is still possible to estimate the  conditional probability
;;of theta1 as close to its true value of 0.6666 (found in previous problems).
;;It takes such a large number to get an output because many of the samples are not used.
;;Since it is seen in previous problems that the probability of generating a random corpus given the
;; constraints (2 and 2 and the same vocabulary) that is the same as my-corpus is only a little
;;more than 1%, so over 98% of the samples are thrown out, and less than 2% are used in the
;;calculation of the estimate, which explains the unstable output at only 100 samples. So even at 
;;8000 samples, only 104 are actually used in this calculation of the estimate, which explains
;;the fact that the results at 8000 samples do not form a tight distribution, though they’re
;;centered around the true value of 0.66666.
