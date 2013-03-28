;Tuberculosis bacteria infect over one in three people worldwide. When the bacteria ;become active there are a number of drugs available to treat patients. However, TB is ;becoming increasingly resistant to these drugs. What I have attempted to model is a set ;of strategies using various drugs to minimize the problem of resistance. 

(ns TB_resistance.core)

;variables per use for each drug with 
;name, cost, initial resistance, 
;% new tb cases who become resistant, and % other failure of drug
(def a '[200 0 .05 .1])
(def b '[100 .2 .1 .15])
(def c '[150 .05 .08 .15])
(def d '[250 0 .03 .1])
(def e '[125 .15 .15 .15])

;budget per year set to 100 million for now
;successful outcomes per year set at 400000 for now
(def budget 100000000)
(def min-outcomes 400000)

(defn rand-strat
  "Returns a percentage for use of each drug determined by cost. 
   ex .2 means 20% of the budget is used on drug a. These add to 100%
   ex.1 a .2 b .3 c 0 d .2 e 0 f 0 g .15 h .15"
  []
  (let [a (rand 1) b (rand 1) c (rand 1) d (rand 1) e (rand 1)]
    (list 'a (/ a (+ a b c d e)) 'b (/ b (+ a b c d e)) 'c (/ c (+ a b c d e)) 
          'd (/ d (+ a b c d e)) 'e (/ e (+ a b c d e)))))

(defn length-strat
  "Takes a rand-strat and tests it until it violates one of the criteria.
   Needs to iterate through the years recording resistance level and measuring outcomes. 
   If either of these reaches critical levels it returns the year."
  [strat]
  (def year-counter 0)
  (def cured 500000)
  (let
    [a-doses (/ (* budget (nth strat 1)) (nth a 0))
     b-doses (/ (* budget (nth strat 3)) (nth b 0))
     c-doses (/ (* budget (nth strat 5)) (nth c 0))
     d-doses (/ (* budget (nth strat 7)) (nth d 0))
     e-doses (/ (* budget (nth strat 9)) (nth e 0))      
     tot-doses (+ a-doses b-doses c-doses d-doses e-doses)
     a-r (double (nth a 1))
     b-r (double (nth b 1))
     c-r (double (nth c 1))
     d-r (double (nth d 1))
     e-r (double (nth e 1))]
    (while (cured > min-outcomes)
      (while ((every? #(< % 0.35) [a-r b-r c-r d-r e-r])
                      (let [cured 0]) ;reset cured
                      (let [cured (+ (* a-doses (- 1 a-r (double (nth a 3))))
                                     (* b-doses (- 1 b-r (double (nth b 3))))
                                     (* c-doses (- 1 c-r (double (nth c 3))))
                                     (* d-doses (- 1 d-r (double (nth d 3))))
                                     (* e-doses (- 1 e-r (double (nth e 3)))))
                            a-r (+ (/ (* (nth a 2) a-doses) tot-doses) a-r)
                            b-r (+ (/ (* (nth b 2) b-doses) tot-doses) b-r)
                            c-r (+ (/ (* (nth c 2) c-doses) tot-doses) c-r)
                            d-r (+ (/ (* (nth d 2) d-doses) tot-doses) d-r)
                            e-r (+ (/ (* (nth e 2) e-doses) tot-doses) e-r)]))
        (inc year-counter))))
  year-counter)

;;q for lee: why can't I cast double on a value in a list which is a number?

;;Here is how length-strat works:
;;ex. strat is a .5 b .5 all others 0. outcomes must total 400000 cured per year
;;y0: a 50 mill/200 is 250000 doses. 
;;for every use 1/20 of new cases have resistance to a
;;resistance increase is (.05*250000)/750000=.016 + 0 is initial resist for y2
;;total*(1-resistance)=total cured.
;;250000*(1-(.1+0))=225000 cured. 
;;y0: b 50 mill/100 is 500000 doses.
;;for every use 1/10 of new cases have resistance to b
;;resistance increase is (.1*500000)/750000=.067 + .2 is r for y2
;;500000*(1-.15-.2)=325000 cured.

;;y1: a
;;r increase is (.05*250000)/750000=.016 + .016 is r for y3
;;250000*(1-(.1+.016))=221000 cured. 
;;y1: b
;;r increase is (.1*500000)/750000=.067 + .267 is r for y3
;;500000*(1-.15-.267)=291500 cured.

;;y2: a
;;r=.016*3=.048
;;cured=250000*(1-r-failure)=213000
;;y2: b
;;r=.067*3+.2=.4
;;cured=500000*(1-r-failure)=225000

;;let's say that initial resistance over 35% is too high to use the drug 
;;so here we stop using drug b. However we continue to use the other drugs 
;;until we see successful outcomes at too low a rate (in this case the next year), 
;;so we end as a 3 year strategy and report outcomes.

(defn sort-length
  "Sort-length sorts the field of strats by which strat lasts longest."
  [field]
  ;sudo code--given a set of lists which hold a strat only
  ;calculate length-strat for each and somehow link this to the strat
  ;then use sort by value of length-strat
  (loop [field index 0]
    (if (>= index (count field))
      field ;we need to test each and order by the result
      (recur (sort (length-strat index))
             (inc index)))))
  ;;q for lee: without storing our results can we compare the entire field?
  ;;need help on this one

(defn mod-strat
  "Returns the strat list changing one variable then 
   adjusting to ensure all values still sum to 100%.
   ex. a .2 b .5 c .3 reset a to .5 -> divide all values by change 
   i.e. .7/1.5 .5/1.5 .3/1.5-> a .467 b .33 c .2"
  [strat]
  (let [n (+ 1 (* (rand-int 5) 2)) change (rand 1) init (nth strat n)]
    (println strat)
    ;;(println (concat (take n strat) (list change) (drop (inc n) strat)))
    (println n change)
    (println 
      (map-indexed 
        (fn [index item] 
          (if (n) (concat (take n strat) (list change) (drop (inc n) strat)))
          (if (odd? index) (/ item (- (inc change) init)) item)) strat))))
;;problem: I have two processes--the swap and the divide. Right now they are each 
;;generating seperate lists; I tried to merge them without success.
;;know new-tot=1+change-init and need to divide all values by new-tot

(defn crossover
 [strat]
 "Switches drug percentage within a strategy. Since everything is based
  on percent of the cost we don't need to worry about things not adding
  up properly."
 (let [n (+ 1 (* (rand-int 5) 2)) m (+ 1 (* (rand-int 5) 2))]
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
  [fieldsize]
  (println "Starting evolution...")
  (loop [generation 0
         field (sort-length (repeatedly fieldsize #(rand-strat)))]
    (let [best (first field)
          best-year (length-strat best)]
      (println "Best Strategy Length:" length-strat)
      (println "Best Strategy:" best)
      (if (> generation 100) ;;run enough, end returning best result
        (println "100 year strat?:" best)
        (recur
          (inc generation)
          (sort-length
            (concat
              (repeatedly (* 1/2 fieldsize) #(mod-strat (selector field 10)))
              (repeatedly (* 1/4 fieldsize) #(crossover (selector field 10)))
              (repeatedly (* 1/4 fieldsize) #(selector field 10)))))))))
