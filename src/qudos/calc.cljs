(ns ^:figwheel-always qudos.calc
  (:require [rum.core :as rum]))

(def num-pats 100)

(def centiles (range 1 21))
(def percentiles (map #(* 0.05 %) centiles))


(comment ;;unused
         (defn intervals-on [monotonic]
           (mapv (fn [& args] (vec args)) (cons 0 monotonic) monotonic))

         (def centile-boundaries (intervals-on percentiles))
         (def centile-risks (intervals-on ub-risks)))

(def ub-risks [0.0005456
               0.0009988
               0.0015798
               0.0023618
               0.0033583
               0.0044388
               0.0056236
               0.0068915
               0.0084389
               0.0102027
               0.0123412
               0.0151487
               0.0184223
               0.0226536
               0.0278246
               0.0348422
               0.0463867
               0.0640427
               0.1017762
               0.6340773])

(defn random-centile [risks]
  (let [index (rand-nth (range 0 20))
        z-risks (vec (cons 0 risks))]
    [(z-risks index) (z-risks (inc index))]))

(defn uniform-sample-on [[lower-bound upper-bound]]
  (+ lower-bound (* (rand) (- upper-bound lower-bound))))

(defn sim-risk [] (uniform-sample-on (random-centile ub-risks)))

(defn to-survival-% [risk] (* 100 (- 1 risk)))

(defn sample-% [n] (take 100 (repeatedly (comp to-survival-% sim-risk))))

(defn formatted-% [n] (map #(.toFixed (js/Number. %) 0) (sample-% n)))

;Sub GenRiskDist()
  ;
  ;Dim NumPats As Integer 'number of patient risks to simulate
  ;Dim ThisRandNum As Double
  ;Dim LBRisk As Double
  ;Dim UBRisk As Double
  ;Dim SimRisk As Double
  ;Dim CentileRisks(1 To 20, 1 To 2)
  ;Dim CentileBoundaries(1 To 20, 1 To 2)
  ;
  ;Application.ScreenUpdating = False
  ;
  ;'read in centile ranges
  ;CentileRisks(1, 1) = 0
  ;CentileRisks(1, 2) = Sheets("Setup").Range("C3").Value
  ;CentileBoundaries(1, 1) = 0
  ;CentileBoundaries(1, 2) = Sheets("Setup").Range("B3").Value
  ;
  ;For j = 2 To 20
  ;CentileRisks(j, 1) = Sheets("Setup").Range("C2").Offset(j - 1, 0).Value
  ;CentileRisks(j, 2) = Sheets("Setup").Range("C2").Offset(j, 0).Value
  ;CentileBoundaries(j, 1) = Sheets("Setup").Range("B2").Offset(j - 1, 0).Value
  ;CentileBoundaries(j, 2) = Sheets("Setup").Range("B2").Offset(j, 0).Value
  ;Next j
  ;
  ;NumPats = Sheets("Setup").Range("F4").Value 'read from sheet
  ;
  ;Randomize 'reset seed for random number generator
  ;
  ;'start simulation
  ;For j = 1 To NumPats
  ;ThisRandNum = Rnd()
  ;For k = 1 To 20 'check which centile
  ;If ThisRandNum > CentileBoundaries(k, 1) And ThisRandNum <= CentileBoundaries(k, 2) Then
  ;LBRisk = CentileRisks(k, 1)
  ;UBRisk = CentileRisks(k, 2)
  ;Exit For
  ;End If
  ;Next k
  ;
  ;'generate risk within this boundary
  ;ThisRandNum = Rnd()
  ;SimRisk = (UBRisk - LBRisk) * ThisRandNum + LBRisk
  ;
  ;'write out
  ;Sheets("SimulDist").Range("A1").Offset(j, 0).Value = j
  ;Sheets("SimulDist").Range("B1").Offset(j, 0).Value = SimRisk
  ;Next j
  ;
  ;Sheets("SimulDist").Activate
  ;
  ;End Sub