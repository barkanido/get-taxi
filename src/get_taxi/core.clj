(ns get-taxi.core
  (:require [get-taxi.locations :as locs]
            [clojure.data.priority-map :as pm]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(defn -main 
  "some sleep here because of evenual consistency of writes"
  [& args]
  (let [locations (locs/->Locations (agent (pm/priority-map-by locs/projections-comparator)) 
                                    (agent (pm/priority-map-by locs/projections-comparator)))]
    (locs/upsert-location locations (locs/->Taxi :a 0    0))
    (locs/upsert-location locations (locs/->Taxi :b 400  0))
    (locs/upsert-location locations (locs/->Taxi :c 500  600))
    (locs/upsert-location locations (locs/->Taxi :d 400  700))
    (locs/upsert-location locations (locs/->Taxi :e 300  900))
    (locs/upsert-location locations (locs/->Taxi :f 1000 800))
    (locs/upsert-location locations (locs/->Taxi :g 1000 0))
    (locs/upsert-location locations (locs/->Taxi :h 1100 0))
    (Thread/sleep 100)
    (prn "x-locations:")
    (pprint @(:x-locations locations))
    (prn "y-locations:")
    (pprint @(:y-locations locations))

    (prn "result:")
    (pprint (locs/k-nearest locations 
                 400 800 
                 3
                 (partial locs/distance-2d-comparator 400 800)))
    (shutdown-agents)))
