(ns fpl1.dist)

(defn evklid
	[p1 p2]
 	(Math/sqrt (apply + (map (fn [valuep1 valuep2] (Math/pow (- (read-string valuep1) (read-string  valuep2)) 2))  p1  p2))))
(defn heming
	[p1 p2]
 	(apply + (map (fn [valuep1 valuep2] (if (= valuep1 valuep2) 0.0 1.0))  p1  p2)))