(ns get-taxi.locations
  (:require [clojure.set]
            [clojure.data.priority-map :as pm]))

(def epsilon 0.00000000001)
(defrecord Taxi [id x y])
(defrecord TaxiProjection [id projection])

(defn x-projection [taxi]
  (->TaxiProjection (:id taxi) (:x taxi)))

(defn y-projection [taxi]
  (->TaxiProjection (:id taxi) (:y taxi)))

(defn -epsilon [n]
  (- n epsilon))

(defn projections->Taxi [x-projection y-projection]
  (assert (= (:id x-projection) (:id y-projection)))
  (->Taxi (:id x-projection)
          (:projection x-projection)
          (:projection y-projection)))     
          

(defn upsert [locations taxi-projection]
  (assoc locations (:id taxi-projection) taxi-projection))

(defprotocol Locator
  (k-nearest [_ x y k sort-fn])
  (upsert-location [_ taxi]))

(defn projections-comparator [t1 t2] 
  (let [c (compare (:projection t1) (:projection t2))]
    (if-not (zero? c)
      c
      (compare (:id t1) (:id t2)))))

(defn get-nearest 
  "sorted-coll is a sorted collection
  taxi-projection is the one we are looking for (does not have to be a member)
  pred is a boolean function that returns true if the element is in distance (pred elem)"
  [sorted-coll taxi-projection pred]
  (let [zero-id (dissoc taxi-projection :id)
        pred (partial pred zero-id)]
    ;; search right and left on the projections until we got too far
    (concat (take-while pred (map val (pm/rsubseq sorted-coll < zero-id)))
            (take-while pred (map val (pm/subseq sorted-coll > (update zero-id :projection -epsilon)))))))

(defn distance-2d [t1 t2]
  (Math/sqrt (+ (Math/pow (- (:x t1) (:x t2)) 2)
                (Math/pow (- (:y t1) (:y t2)) 2))))

(defn distance-2d-comparator [x y other-taxi]
  (let [pos (->Taxi nil x y)]
    (distance-2d pos other-taxi)))
                                
(defn less-than-1k-2d [t1 t2]
  (<= (distance-2d t1 t2) 1000))

(defn less-than-1k [projection1 prjection2]
  (<= (Math/abs (- (:projection projection1) (:projection prjection2))) 
      1000))

(defn intersect-kxs-kys 
  "take 2 vectors (taxiX's and taxiY's)
  - computes a set of intersecting ids
  - joins the 2 vectors by matching ids
  - returns a sequence of coresponding Taxis"
  [k-nearest-xs k-nearest-ys]
  (let [k-nearest-ids (clojure.set/intersection (set (map :id k-nearest-xs))
                                                (set (map :id k-nearest-ys)))]
      (map projections->Taxi (sort-by :id (filter #(k-nearest-ids (:id %)) k-nearest-xs))
                             (sort-by :id (filter #(k-nearest-ids (:id %)) k-nearest-ys)))))

(defrecord Locations [x-locations y-locations]
  Locator
  (k-nearest [_ x y k sort-fn]
    ;; search left, search right, join results, filter them, sort and only take what you need
    (let [position (->Taxi nil x y)
          k-nearest-xs (get-nearest @x-locations (x-projection position) less-than-1k)
          k-nearest-ys (get-nearest @y-locations (y-projection position) less-than-1k)
          k-nearest-taxis (intersect-kxs-kys k-nearest-xs k-nearest-ys)]
      (->> k-nearest-taxis
           ;; all computations below are lazy and performed by demand of the last `take`
           (filter (partial less-than-1k-2d position))
           (sort-by sort-fn)
           (take k))))

  (upsert-location [_ new-taxi]
    ;; deliberately do an async write here since it is more performant
    ;; and read logic is robust to partial state
    (send x-locations upsert (x-projection new-taxi))
    (send y-locations upsert (y-projection new-taxi))))

;; using a priority map enables:
;; fast access by id O(1)
;; maintain sorting after update by popping and pushing in O(log(n))
;; iterating in a sorted manner (see subseq/rsubseq above)
;; (def locations (->Locations (agent (pm/priority-map-by projections-comparator)) 
;;                             (agent (pm/priority-map-by projections-comparator))))
;; ;; testing
;; (upsert-location locations (->Taxi :a 0    0))
;; (upsert-location locations (->Taxi :b 400  0))
;; (upsert-location locations (->Taxi :c 500  600))
;; (upsert-location locations (->Taxi :d 400  700))
;; (upsert-location locations (->Taxi :e 300  900))
;; (upsert-location locations (->Taxi :f 1000 800))
;; (upsert-location locations (->Taxi :g 1000 0))
;; (upsert-location locations (->Taxi :h 1100 0))
;;
;;
;; ;; below var is computed to: 
;; ;;(#get_taxi.locations.Taxi {:id :d, :x 400, :y 700}
;; ;; #get_taxi.locations.Taxi {:id :e, :x 300, :y 900}
;; ;; #get_taxi.locations.Taxi {:id :c, :x 500, :y 600})
;;
;; (def three-nearest (k-nearest locations 
;;                               400 800 
;;                               3 (partial distance-2d-comparator 400 800)))
