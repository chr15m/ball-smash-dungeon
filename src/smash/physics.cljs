(ns smash.physics
  (:require
    [sitefox.ui :refer [log]]
    ["matter-js/build/matter" :as Matter]))

(print "physics")

(def threshold 0.001)

(defn all-bodies-at-rest [bodies]
  (empty? (remove #(aget % "isSleeping") bodies)))

(defn get-bodies [engine]
  (.allBodies Matter/Composite (.-world engine)))

(defn simulate [adjacent-tiles entities]
  (let [sim (atom [])
        e (.create Matter/Engine (clj->js {:gravity {:x 0 :y 0}
                                           :enableSleeping true}))
        static-bodies (map #(.rectangle Matter/Bodies (:x %) (:y %) (:width %) (:height %) #js {:isStatic true}) adjacent-tiles)
        entity-bodies (map (fn [e]
                             (let [body (.circle Matter/Bodies (:x e) (:y e) (:radius e) (clj->js {:frictionAir 0.5}))
                                   [x y] (:velocity e)
                                   velocity (.create Matter/Vector x y)]
                               (.setVelocity Matter/Body body velocity)
                               ;(.applyForce Matter/Body body (aget body "position") velocity)
                               body)) entities)]
    (log (first entity-bodies))
    (.add Matter/Composite (.-world e) (clj->js (concat static-bodies entity-bodies)))
    (doall
      (loop [c 0]
        (js/console.log "update:" c)
        (.update Matter/Engine e (/ 1000 60))
        ;(swap! sim conj (map (fn [b] ) entity-bodies))
        ; store positions etc.
        (when (or (not (all-bodies-at-rest (.allBodies Matter/Composite (.-world e))))
                  (< c 3))
          (recur (inc c)))))
    (let [bodies (get-bodies e)]
      ; bodies[i].vertices
      (js/console.log "bodies" bodies)
      bodies)))
