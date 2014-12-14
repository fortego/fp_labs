(ns fpl1.reader
	(require [clojure-csv.core :as csv])
	)
 

(defn readFromFile
	[fpath]
	(map (fn [numb items] {:values (drop-last 1.0 (:values items)) :potential 0.0 :position numb})
		   (range)  
		    (map (fn [string] {:values string :potential 0.0}) (csv/parse-csv (slurp fpath)))))