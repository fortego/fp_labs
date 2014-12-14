(ns fpl1.core
	(:gen-class)
	(:use fpl1.reader)
	(:use fpl1.dist))


(defn Distances
	[p1 p2 dist]
	(case dist
		"heming" (heming (:values p1) (:values p2))
		"evklid" (evklid (:values p1) (:values p2))
	)
)

(defn FirstPotentials
	[point points dist rad]
	(apply + (map (fn [point2] ( Math/exp(- (* (Distances point point2 dist) (/ 4 (* rad rad)))))) points)))


(defn MaxPotential
	[points]
	(apply max-key (fn [point] (:potential point)) points))


(defn Stop
	[points centers newCenter firstCenter dt rad]
	 (cond
     (> (:potential (first centers)) (* 0.5 (:potential firstCenter)))
    	 {:centers (cons newCenter centers) :points points}
     (< (:potential (first centers)) (* 0.15 (:potential firstCenter) ))
  		 {:centers centers}
     (>= (+ (/ (apply min (map (fn [center] (Distances newCenter center dt)) centers)) rad) (/ (:potential newCenter) (:potential firstCenter))) 1.0)
   		 {:centers (cons newCenter centers) :points points}
     :else (let [newpoints (map (fn [center] {:values (:values center) :potential (if(= (:position newCenter) (:position center)) 0 (:potential center)) :position (:position center)}) points )]
		{:centers (cons (MaxPotential newpoints) centers) :cpoints newpoints})))

(defn -main [& args]
  (def disttype (second args))
  (def radius (read-string(nth args 2)))
   (doseq [centers (
   			let [potentials (let [points (readFromFile (first args))]
									(map (fn [point] {:values (:values point) :potential (FirstPotentials point points disttype radius) :position (:position point)}) points))]
			(loop [array potentials centers [(MaxPotential potentials)]]
				(let [points (map (fn [point] {:values (:values point) :potential (let [center (first centers)](- (:potential point) (* (:potential center)
				 (Math/exp (- (* (Distances point center disttype) (/ 4 (* (* radius 1.5) (* radius 1.5))))))))) :position (:position point)}) array)]
					(let [Array (Stop points centers (MaxPotential points) (last centers) disttype radius)]
								(if (nil? (:array Array))
									(:centers Array)
									(recur (:array Array) (:centers Array)))))))]
   (println (:position centers))))
