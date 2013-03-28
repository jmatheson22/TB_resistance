;Tuberculosis bacteria infect over one in three people worldwide. When the bacteria ;become active there are a number of drugs available to treat patients. However, TB is ;becoming increasingly resistant to these drugs. What I have attempted to model is a set ;of strategies using various drugs to minimize the problem of resistance. 

(ns TB_resistance.core)

;variables per use for each drug with 
;name, cost, initial resistance, 
;% new tb cases who become resistant, and % other failure of drug

(def a [200 0 0.05 0.1])
(def b [100 0.2 0.1 0.15])
(def c [150 0.05 0.08 0.15])
(def d [250 0 0.03 0.1])
(def e [125 0.15 0.15 0.15])
(def f [100 0 0.25 0.1])
(def budget 10000000) ;the budget to spend on drugs per year
(def min-outcomes 30000) ;the minimum positve outcomes per year

(defn rand-strat
  "Returns a percentage for use of each drug determined by cost. 
   ex .2 means 20% of the budget is used on drug a. These add to 100%
   ex.1 a .2 b .3 c 0 d .2 e 0 f 0 g .15 h .15"
  []
  (let [a (rand 1) b (rand 1) c (rand 1) d (rand 1) e (rand 1) f (rand 1) 
        tot (apply + [a b c d e f])]
    (list 'a (/ a tot) 'b (/ b tot) 'c (/ c tot)
          'd (/ d tot) 'e (/ e tot) 'f (/ f tot))))

(defn rand-strat
  "Same as above but without names."
  []
  (let [a (rand 1) b (rand 1) c (rand 1) d (rand 1) e (rand 1) f (rand 1) 
        tot (apply + [a b c d e f])]
    (list (/ a tot) (/ b tot) (/ c tot) (/ d tot) (/ e tot) (/ f tot))))

(defn length-strat
  "Takes a rand-strat and tests it until it violates one of the criteria.
   Needs to iterate through the years recording resistance level and measuring outcomes. 
   If either of these reaches critical levels it returns the year."
  [strat]
  (let [doses (map #(/ (* budget %) %2) 
                   (map second (partition 2 strat)) 
                   (map first [a b c d e f]))
        tot-doses (apply + doses)]
    (loop [year 0
           resistances (map second [a b c d e f])]
      (let [cured (apply + (map #(* % (- 1 %2 %3))
                                doses
                                resistances
                                (map #(nth % 3) [a b c d e f])))]
        (if (or (some #(> % 0.35) resistances)
                (< cured min-outcomes))
          year
          (recur (inc year)
                 (map #(+ (/ (* (nth a 2) %) tot-doses) %2)
                      doses
                      resistances)))))))

(let [s (rand-strat)]
  (println s)
  (length-strat s))

;;;Here is how length-strat works:
;;;ex. strat is a .5 b .5 all others 0. outcomes must total 400000 cured per year
;;;y0: a 50 mill/200 is 250000 doses. 
;;;for every use 1/20 of new cases have resistance to a
;;;resistance increase is (.05*250000)/750000=.016 + 0 is initial resist for y2
;;;total*(1-resistance)=total cured.
;;;250000*(1-(.1+0))=225000 cured. 
;;;y0: b 50 mill/100 is 500000 doses.
;;;for every use 1/10 of new cases have resistance to b
;;;resistance increase is (.1*500000)/750000=.067 + .2 is r for y2
;;;500000*(1-.15-.2)=325000 cured.
;;;y1: a
;;;r increase is (.05*250000)/750000=.016 + .016 is r for y3
;;;250000*(1-(.1+.016))=221000 cured. 
;;;y1: b
;;;r increase is (.1*500000)/750000=.067 + .267 is r for y3
;;;500000*(1-.15-.267)=291500 cured.
;;;y2: a
;;;r=.016*3=.048
;;;cured=250000*(1-r-failure)=213000
;;;y2: b
;;;r=.067*3+.2=.4
;;;cured=500000*(1-r-failure)=225000
;;;let's say that initial resistance over 35% is too high to use the drug 
;;;so here we stop using drug b. However we continue to use the other drugs 
;;;until we see successful outcomes at too low a rate (in this case the next year), 
;;;so we end as a 3 year strategy and report outcomes.

(def mystrats
  (vector (rand-strat) (rand-strat) (rand-strat) (rand-strat) (rand-strat)))

(defn sort-length
  "Sort-length sorts the field of strats by which strat lasts longest."
  [field]
  ;(map length-strat (sort #(> (length-strat %) (length-strat %2)) field)))
  (sort #(> (length-strat %) (length-strat %2)) field))

(defn mod-strat
  "Returns the strat list changing one variable then 
   adjusting to ensure all values still sum to 100%.
   ex. a .2 b .5 c .3 reset a to .5 -> divide all values by change 
   i.e. .7/1.5 .5/1.5 .3/1.5-> a .467 b .33 c .2"
  [strat]
  (let [n (+ 1 (* (rand-int 6) 2)) ;odds 1-9
        change (rand 1) 
        init (nth strat n)
        item (concat (take n strat) (list change) (drop (inc n) strat))]
    (println strat)
    (println item)
    (map-indexed (fn [index item] 
                   (if (odd? index) (/ item (- (inc change) init)) item)) item)))

(defn crossover
 [strat]
 "Switches drug percentage within a strategy. Since everything is based
  on percent of the cost we don't need to worry about things not adding
  up properly."
 (let [n (+ 1 (* (rand-int 6) 2)) m (+ 1 (* (rand-int 6) 2))]
   (if (= n m)
     strat
     (let [lower (min n m) higher (max n m)]
       (concat (take lower strat)
               (list (nth strat higher))
               (take (- higher lower 1) (drop (inc lower) strat))
               (list (nth strat lower))
               (drop (inc higher) strat))))))

(defn selector
  [field reduction-size]
  "A utility tool for randomly selecting a smaller number of strategies from a field."
  (let [size (count field)] ;;counts number of strats in play
    (nth field
         (apply min (repeatedly reduction-size #(rand-int size))))))

(defn evolve
  [fieldsize] ;I'd suggest a number around 1000
  (println "Starting evolution with successful outcomes per year at least" min-outcomes "and a budget of" budget)
  (loop [generation 0
         field (sort-length (repeatedly fieldsize #(rand-strat)))]
    (let [best (first field)
          best-year (length-strat best)]
      (println "Best Strategy Length:" (length-strat best) "years.")
      (println "Best strategy spends this percentage on each drug: ")
      (println best))
    (if (> generation 100)
      (recur
        (inc generation)
        (sort-length
          (concat
            (repeatedly (* 1/2 fieldsize) #(mod-strat (selector field 10)))
            (repeatedly (* 1/4 fieldsize) #(crossover (selector field 10)))
            (repeatedly (* 1/4 fieldsize) #(selector field 10))))))))
