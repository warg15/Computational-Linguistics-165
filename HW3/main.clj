;;William Argus
;;A12802324
;;Collaborated with Colman Dekker via a phone call in which we discussed the homework
;;questions to ensure mutual understanding and gave guidance to each other in debugging
;;The code was mainly done individually with us just sort of checking up with each other after
;;each problem to make sure we agreed on directions and general approach 

;;Problem 1
(def moby-word-tokens '(CALL me Ishmael . Some years ago never mind
how long precisely having little or no money in my purse , and
nothing particular to interest me on shore , I thought I would
sail about a little and see the watery part of the world . It is
a way I have of driving off the spleen , and regulating the
circulation . Whenever I find myself growing grim about the mouth
whenever it is a damp , drizzly November in my soul whenever I
find myself involuntarily pausing before coffin warehouses , and
bringing up the rear of every funeral I meet and especially
whenever my hypos get such an upper hand of me , that it requires
a strong moral principle to prevent me from deliberately stepping
into the street , and methodically knocking people's hats off
then , I account it high time to get to sea as soon as I can .
This is my substitute for pistol and ball . With a philosophical
flourish Cato throws himself upon his sword I quietly take to the
ship . There is nothing surprising in this . If they but knew it
, almost all men in their degree , some time or other , cherish
very nearly the same feelings toward the ocean with me .))

(defn member-of-list? [w l]
   1
   (if (empty? l)
       false
       (if (= w (first l))
         true
     (member-of-list? w (rest l)))))

(defn get-vocabulary [word-tokens vocab]
   (if (empty? word-tokens)
       vocab
      (if (member-of-list? (first word-tokens) vocab )
      (get-vocabulary (rest word-tokens) vocab)  
      (get-vocabulary (rest word-tokens) (cons (first word-tokens) vocab)))))


(def moby-vocab   (get-vocabulary moby-word-tokens (list)  ) )


;;Problem 2
(defn get-count-of-word [w word-tokens]
  (if (empty? word-tokens)
    0
    (if (= w (first word-tokens))
      (+ 1 (get-count-of-word w (rest word-tokens) ))
      (get-count-of-word w (rest word-tokens))
      )))
;;(get-count-of-word 'CALL (list 'CALL 'CALL 'CALL) )

;;Problem 3
(def moby-word-tokens '(CALL me Ishmael . Some years ago never mind
how long precisely having little or no money in my purse , and
nothing particular to interest me on shore , I thought I would
sail about a little and see the watery part of the world . It is
a way I have of driving off the spleen , and regulating the
circulation . Whenever I find myself growing grim about the mouth
whenever it is a damp , drizzly November in my soul whenever I
find myself involuntarily pausing before coffin warehouses , and
bringing up the rear of every funeral I meet and especially
whenever my hypos get such an upper hand of me , that it requires
a strong moral principle to prevent me from deliberately stepping
into the street , and methodically knocking people's hats off
then , I account it high time to get to sea as soon as I can .
This is my substitute for pistol and ball . With a philosophical
flourish Cato throws himself upon his sword I quietly take to the
ship . There is nothing surprising in this . If they but knew it
, almost all men in their degree , some time or other , cherish
very nearly the same feelings toward the ocean with me .))

(defn member-of-list? [w l]
   1
   (if (empty? l)
       false
       (if (= w (first l))
         true
     (member-of-list? w (rest l)))))

(defn get-vocabulary [word-tokens vocab]
   (if (empty? word-tokens)
       vocab
      (if (member-of-list? (first word-tokens) vocab )
      (get-vocabulary (rest word-tokens) vocab)  
      (get-vocabulary (rest word-tokens) (cons (first word-tokens) vocab)))))

(defn get-count-of-word [w word-tokens]
  (if (empty? word-tokens)
    0
    (if (= w (first word-tokens))
      (+ 1 (get-count-of-word w (rest word-tokens) ))
      (get-count-of-word w (rest word-tokens))
      )))

(defn get-word-counts [vocab word-tokens]
  (let [count-word (fn [w]
    (get-count-of-word w word-tokens))]
    (map count-word vocab)))

(def moby-vocab  (get-vocabulary moby-word-tokens (list) ))

(def moby-word-frequencies (get-word-counts moby-vocab moby-word-tokens))

;;(print moby-word-frequencies)


;;Problem 4
(defn flip [p]
  (if (< (rand 1) p)
    true
    false))

(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))

(defn sample-categorical [outcomes params]
  (if (flip (first params))
    (first outcomes)
    (sample-categorical (rest outcomes)
                        (normalize (rest params)))))


(defn create-uniform-distribution [outcomes]
  (let [num-outcomes (count outcomes)]
    (map (fn [x] (/ 1 num-outcomes))
         outcomes)))

(defn sample-uniformBOW-sentence [n vocab] 
  (if (= 0 n)
    '()
    (cons (sample-categorical vocab (create-uniform-distribution vocab)) (sample-uniformBOW-sentence (- n 1) vocab)  )
    )
 )

;;(sample-uniformBOW-sentence 12 (list 'bye 'fuck ' duck 'ye 'hi))

;;Problem 5
(defn compute-uniform-BOW-prob [vocab sentence]
  (if (= '() sentence)
    1
    (* (/ 1 (count vocab)) (compute-uniform-BOW-prob  vocab (rest sentence)))
    )
)

;;Problem 6
;;Though the list of words sampled changes, the probability from compute-uniform-BOW-prob
;;remains the same at about 0.00714. This makes sense for two reasons. First, the probability
;;is very small that those 3 specific words will be chosen from the relatively large list that is 
;;moby-vocab, so the probability being a small number makes sense. Secondly, the fact that is
;;stays constant also makes sense, since all words have the same probability so this value is 
;;simply the probability of a specific word (the same for all words), cubed, since there are 3
;;words. It will remain the same regardless of what the three specific words are.



;;Problem 7
(def moby-word-tokens '(CALL me Ishmael . Some years ago never mind
how long precisely having little or no money in my purse , and
nothing particular to interest me on shore , I thought I would
sail about a little and see the watery part of the world . It is
a way I have of driving off the spleen , and regulating the
circulation . Whenever I find myself growing grim about the mouth
whenever it is a damp , drizzly November in my soul whenever I
find myself involuntarily pausing before coffin warehouses , and
bringing up the rear of every funeral I meet and especially
whenever my hypos get such an upper hand of me , that it requires
a strong moral principle to prevent me from deliberately stepping
into the street , and methodically knocking people's hats off
then , I account it high time to get to sea as soon as I can .
This is my substitute for pistol and ball . With a philosophical
flourish Cato throws himself upon his sword I quietly take to the
ship . There is nothing surprising in this . If they but knew it
, almost all men in their degree , some time or other , cherish
very nearly the same feelings toward the ocean with me .))

(defn member-of-list? [w l]
   1
   (if (empty? l)
       false
       (if (= w (first l))
         true
     (member-of-list? w (rest l)))))

(defn get-vocabulary [word-tokens vocab]
   (if (empty? word-tokens)
       vocab
      (if (member-of-list? (first word-tokens) vocab )
      (get-vocabulary (rest word-tokens) vocab)  
      (get-vocabulary (rest word-tokens) (cons (first word-tokens) vocab)))))

(defn get-count-of-word [w word-tokens]
  (if (empty? word-tokens)
    0
    (if (= w (first word-tokens))
      (+ 1 (get-count-of-word w (rest word-tokens) ))
      (get-count-of-word w (rest word-tokens))
      )))

(defn get-word-counts [vocab word-tokens]
  (let [count-word (fn [w]
    (get-count-of-word w word-tokens))]
    (map count-word vocab)))

(def moby-vocab  (get-vocabulary moby-word-tokens (list) ))

(def moby-word-frequencies (get-word-counts moby-vocab moby-word-tokens))

(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))

(def moby-word-probabilities (normalize moby-word-frequencies) )

;;(print moby-word-probabilities)



;;Problem 8
;;Process will be repeated 10 times
;; growing . of
;;the off involuntarily
;;I time my
;;such toward ago
;;throws pistol the
;get upon men
;;is toward moral
;;is before pausing
;;as on mind
;;to whenever me
;;The results are as expected, with some words occurring more than
;;others because the probabilities are dependent on frequency now,
;;as opposed to being uniform







;;Problem 9
(defn lookup-probability [w outcomes probs]
  (if (empty? outcomes)
    0
  (if (= w (first outcomes))
    (first probs)
    (lookup-probability w (rest outcomes) (rest probs))
    )
  ))
;;(lookup-probability 'the (list 'the 'a 'every) (list 0.2 0.5 0.3) )



;;Problem 10
(defn lookup-probability [w outcomes probs]
  (if (empty? outcomes)
    0
  (if (= w (first outcomes))
    (first probs)
    (lookup-probability w (rest outcomes) (rest probs))
    )
  ))

(defn product [l]
(apply * l))

(defn compute-BOW-prob [sentence vocabulary probabilities] 
  (if (empty? sentence)
    1
      (product (concat (list (lookup-probability (first sentence) vocabulary probabilities)) (list (compute-BOW-prob (rest sentence) vocabulary probabilities))))))
      

;;(def vocab (list 'their 'as 'me))
;;(def probs (list .1 .1 .8))
;;(compute-BOW-prob (list 'me 'me 'me) vocab probs)


;;Problem 11
;;Process will be repeated 10 times
;; growing . of = 0.0000035
;;the off involuntarily = 0.00000219
;;I time my = 0.00000789
;;such toward ago = 0.0000001095
;;throws pistol the = 0.000001095
;get upon men = 0.000000219
;;is toward moral = 0.000000438
;;is before pausing = 0.000000438
;;as on mind = 0.000000219
;;to whenever me = 0.000008215

;;This answer is different because each sentence has a different probability. This is expected as 
;;the probabilities used here are not the uniform probabilities of problem 6 but rather the 
;;probabilities from frequency of occupancy - giving words different probabilities.

;; Choosing: ;;the off involuntarily
;; the sentence ;get upon men has the same probability as it (note that both were found above)
;;The only thing that matters is the three words, order doesn’t matter, as the probability is 
;;simply the probability of each word (based on frequency), multiplied together












