(ns my-project.core
  (:require [clojure.string :as str]
             [clojure.java.io :as io]
             [iota :as iota]))

(def path "data/SentiWordNet_3.0.0_20130122.txt")

(def temp (atom{}))
(def dict(atom{}))

(defn String->Number [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))

(defn message [score]     
        (if (>= score 0.75) "strong_positive" 
          (if (and(> score 0.25 ) (<= score 0.5)) "positive" 
          (if (and (> score 0 ) (<= score 0.25)) "weak_positive" 
            (if (and (< score 0 ) (>= score -0.25)) "weak_negative"
              (if (and (< score -0.25 ) (>= score -0.5)) "negative"
                (if (< score -0.75 ) "strong_negative")))))))

(defn set-maps []
  (do
    (def temp-set(keys @temp))
    (loop [i 0]
      (when (<= i (- (count(keys temp-set)) 1))
        (def word (nth (keys @temp) i))
        (def v (get @temp word))
        (def score (atom 0.0))
        (def sum (atom 0.0))
        (loop [j 0]
          (when (< j (count v))
            (swap! score (fn[sc]
                           (double(+ sc (* (/ 1 (+ j 1)) (get v j))))))
            (recur (inc j))))
        (loop [j 1]
          (when (<= j (count v))
            (swap! sum (fn[s]
                           (+ (double s) (double(/ 1 j)))))
            (recur (inc j))))
        (swap! score (fn[s]
                       (double (/ s @sum))))
     
         (swap! dict (fn[d]
                       (assoc d word (message @score))))
        (recur (inc i))))
    )
  )
(defn set-maps2 []
  (do
    (doseq [t (keys @temp)]
        (def v (get @temp t))
        (def score (atom 0.0))
        (def sum (atom 0.0))
        (loop [j 0]
          (when (< j (count v))
            (swap! score (fn[sc]
                           (double(+ sc (* (/ 1 (+ j 1)) (get v j))))))
            (recur (inc j))))
        (loop [j 1]
          (when (<= j (count v))
            (swap! sum (fn[s]
                           (+ (double s) (double(/ 1 j)))))
            (recur (inc j))))
        (swap! score (fn[s]
                       (double (/ s @sum))))
     
         (swap! dict (fn[d]
                       (assoc d t (message @score)))))))


(defn read-file[]
  (do
  (def file-seqe (iota/seq path))
  (doseq [line file-seqe]
    (def  data (str/split line #"\t"))
    (def score (- (String->Number(get data 2))(String->Number(get data 3))))
    (def words (str/split (get data 4) #" "))
    (doseq [x words]
      (def w-n (atom (str/split x #"#")))
      (swap! w-n (fn [wn]
                  (assoc wn 0 (str (get wn 0) "#" (get data 0)))))
     
      (def index (- (String->Number(get @w-n 1)) 1))
 
      (if ( find @temp (get @w-n 1))
       (do 
         (def v(atom(vector (get @temp (get @w-n 0)))))
         (if (> index (count v))
           (loop [i (count v)]
             (when (< i index)
              (swap! v (fn[vec]
                         (conj vec 0.0))))
               (recur (inc i))))
          (swap! v (fn[vect]
                     (assoc vect index score)))
          (swap! temp (fn[t]
                        (assoc t (keyword (str(get @w-n 0))) @v))))
          (do
              (def v(atom(vector )))
            (loop [i 0]
              (when (<= i index)
                  (swap! v (fn [vec]
                         (conj vec 0.0)))
                (recur (inc i))))
            (swap! v (fn [vect]
                       (assoc vect index score)))
            (swap! temp (fn[te]
                            (assoc te (keyword (str(get @w-n 0))) @v))))))) 
   (set-maps2)))



(defn extract [token word]
  (get @dict (keyword(str token "#" word))))

(def text (slurp "data/test.txt"))
(def stop-words (slurp "data/stop_words2.txt"))

(def type-words ["a" "n" "v" "r"])

(defn classify-all []
  (do
    (def countB (atom 0))
    (def tokens (str/split text #" "))
    (def stop false)
    (loop [i 0]
      (when (and (< i (count tokens))
            (nil?(loop[j 0] 
                   (when (< j (count stop-words))
                     (if (= (get stop-words j)(get tokens i)) false
                       (recur (inc j)))))))
        (if (not(empty? (get tokens i)))
          (do
            (loop [w 0]
              (when (<= w (count type-words))
               (def feeling (extract (get tokens i) (get type-words w)))
               (if (and (not(empty? feeling))(not (nil? feeling)))
                 (case feeling
                   "strong_positive" (swap! countB + 2)
                   "positive"	(swap! countB + 1)
                   "weak_positive" (swap! countB + 0.5)
                   "weak_negative"	(swap! countB - 0.5)
                   "negative"	(swap! countB - 1)
                   "strong_negative"	(swap! countB - 2))) 
               (recur(inc w))))))
        (recur (inc i)))))
  (if (>= @countB 0) 
  (println "positive" @countB)
  (println "negative" @countB)))

; function with more arguments working!
(defn classify-all2 [& args]
  (do
   ; (println (into [] args))
    (def type-words (into [] args))
    (def countB (atom 0))
    (def tokens (str/split text #" "))
    (def stop false)
    (loop [i 0]
      (when (and (< i (count tokens))
            (nil?(loop[j 0] 
                   (when (< j (count stop-words))
                     (if (= (get stop-words j)(get tokens i)) false
                       (recur (inc j)))))))
        (if (not(empty? (get tokens i)))
          (do
            (loop [w 0]
              (when (<= w (count type-words))
               (def feeling (extract (get tokens i) (get type-words w)))
               (if (and (not(empty? feeling))(not (nil? feeling)))
                 (case feeling
                   "strong_positive" (swap! countB + 2)
                   "positive"	(swap! countB + 1)
                   "weak_positive" (swap! countB + 0.5)
                   "weak_negative"	(swap! countB - 0.5)
                   "negative"	(swap! countB - 1)
                   "strong_negative"	(swap! countB - 2))) 
               (recur(inc w))))))
        (recur (inc i)))))
  (if (>= @countB 0) 
  (println "positive" @countB)
  (println "negative" @countB)))



