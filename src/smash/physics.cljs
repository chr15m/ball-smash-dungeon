(ns smash.physics
  {:clj-kondo/config '{:lint-as {applied-science.js-interop/let clojure.core/let}}}
  (:require
    [sitefox.ui :refer [log]]
    [applied-science.js-interop :as j]
    ["matter-js/build/matter" :as Matter]))

(print "physics")

(def threshold 0.001)

(defn all-bodies-at-rest [bodies]
  (empty? (remove #(aget % "isSleeping") bodies)))

(defn get-bodies [engine]
  (.allBodies Matter/Composite (.-world engine)))

(defn serialize-body [b]
  (js->clj (j/let [^:js {:keys [velocity position uuid vertices entity]} b
                   vertices (map #(j/let [^:js {:keys [x y]} %] {:x x :y y}) vertices)]
             {:velocity velocity :position position :uuid uuid :vertices vertices :entity entity})
           :keywordize-keys true))

(defn simulate [adjacent-tiles entities]
  (let [sim (atom [])
        e (.create Matter/Engine (clj->js {:gravity {:x 0 :y 0}
                                           :enableSleeping true}))
        static-bodies (map #(.rectangle Matter/Bodies
                                        (+ (:x %) (/ (:width %) 2))
                                        (+ (:y %) (/ (:height %) 2))
                                        (:width %)
                                        (:height %)
                                        #js {:isStatic true
                                             :friction 0
                                             :restitution 1.0})
                           adjacent-tiles)
        entity-bodies (map (fn [e]
                             (let [body (.circle Matter/Bodies (:x e) (:y e) (:radius e) (clj->js {:frictionAir 0.01
                                                                                                   :friction 0
                                                                                                   :restitution 1.0}))
                                   [x y] (:velocity e)
                                   velocity (.create Matter/Vector x y)]
                               (.setVelocity Matter/Body body velocity)
                               ;(.setAngularVelocity Matter/Body body 0.01)
                               ;(.setFriction Matter/Body body 0 0 0)
                               (aset body "uuid" (:uuid e))
                               (aset body "entity" e)
                               ;(.applyForce Matter/Body body (aget body "position") velocity)
                               body)) entities)]
    (log (first entity-bodies))
    (.add Matter/Composite (.-world e) (clj->js (concat static-bodies entity-bodies)))
    (js/console.log "updates:"
      (loop [c 0]
        (.update Matter/Engine e (/ 1000 60))
        (swap! sim conj (doall (map serialize-body entity-bodies)))
        ; store positions etc.
        (if (or (not (all-bodies-at-rest (.allBodies Matter/Composite (.-world e))))
                  (< c 3))
          (recur (inc c))
          c)))
    (log "sim" @sim)
    (log "bodies" static-bodies)
    {:sim @sim :bodies (map serialize-body static-bodies)}))
