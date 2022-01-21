(ns smash.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [applied-science.js-interop :as j]
            [sitefox.ui :refer [log]]
            ["rot-js" :as ROT]))

(log "HI")

(defn initial-state []
  {:screen :menu})

(defonce state (r/atom (initial-state)))

(defn get-map []
  (js/console.log "hi")
  (let [digger (ROT/Map.Digger.)
        positions (atom {})]
    (js/console.log digger)
    (.create digger
             (fn [x y v]
               ; (js/console.log (clj->js args))
               (swap! positions assoc [x y] v)))
    (js/console.log "digger:" digger)
    (js/console.log (clj->js positions))
    (js/console.log (aget digger "_rooms"))
    (js/console.log (aget digger "_corridors"))
    {:tiles @positions
     :rooms (-> digger (aget "_rooms") js/JSON.stringify js/JSON.parse (js->clj :keywordize-keys true))
     :corridors (-> digger (aget "_corridors") js/JSON.stringify js/JSON.parse (js->clj :keywordize-keys true))
     :size [(aget digger "_width") (aget digger "_height")]
     :digger digger}))

(defn expand-pos [[x y]]
  [(- x 0.5) (+ y 0.5)])

(defn start-game [state]
  (swap! state
         #(-> %
              (assoc :map (get-map))
              (assoc :screen :game))))

(defn process-game-key [state ev]
  (js/console.log (aget ev "keyCode"))
  (case (aget ev "keyCode")
    27 (swap! state assoc :screen :menu)
    nil))

(defn component-menu [state]
  [:div#menu
   [:h1 "ball smash" [:br] "dungeon"]
   [:p "a video game about smashing balls"]
   (when (:map @state)
     [:p [:button {:on-click #(swap! state assoc :screen :game)} "resume game"]])
   [:p [:button {:on-click #(start-game state)} "new game"]]])

(defn component-game [state]
  (js/console.log "state" (clj->js @state))
  (let [scale 10
        game-map (:map @state)
        {:keys [size rooms corridors tiles]} game-map
        tile-positions (remove nil?
                               (for [[[x y] v] tiles]
                                 (when (= v 0)
                                   {:key (str "tile" x y)
                                    :x (* (- x 0.5) scale)
                                    :y (* (- y 0.5) scale)
                                    :width scale
                                    :height scale})))
        room-positions (for [r rooms]
                         (let [{:keys [_x1 _x2 _y1 _y2 _doors]} r
                               xs (expand-pos (sort [_x1 _x2]))
                               ys (expand-pos (sort [_y1 _y2]))
                               w (- (second xs) (first xs))
                               h (- (second ys) (first ys))]
                           (log "doors" _doors)
                           {:key (str "room" xs ys)
                            :x (* (first xs) scale)
                            :y (* (first ys) scale)
                            :width (* w scale)
                            :height (* h scale)}))
        corridor-positions (for [c corridors]
                             (let [{:keys [_startX _startY _endX _endY]} c
                                   [_x1 _x2 _y1 _y2] [_startX _endX _startY _endY]
                                   xs (expand-pos (sort [_x1 _x2]))
                                   ys (expand-pos (sort [_y1 _y2]))
                                   w (- (second xs) (first xs))
                                   h (- (second ys) (first ys))]
                               {:x (* (first xs) scale)
                                :y (* (first ys) scale)
                                :width (* w scale)
                                :height (* h scale)}))]
    [:svg {:on-key-down #(process-game-key state %)
           :tabIndex 0
           :ref #(when % (.focus %))
           :width (* (first size) scale)
           :height (* (second size) scale)
           ;:style {:transform "rotate(-25deg)"}
           }
     ; draw hatching
     ; draw outlines
     ; draw interiors
     (for [pos-map tile-positions]
       [:rect (merge pos-map {:fill "#333"})])

     (for [pos-map room-positions]
       [:g
        #_ [:rect (merge pos-map
                         {:stroke "#cdcdcd"
                          :stroke-width 5
                          :stroke-linejoin "round"})]
        [:rect (merge pos-map
                      {:fill "#777"})]])

     (for [pos-map corridor-positions]
       [:rect (merge pos-map {:fill "#555"})])]))

(defn component-main [state]
  [:main
   (case (:screen @state)
     :menu [component-menu state]
     :game [component-game state])])

(defn start {:dev/after-load true} []
  (rdom/render [component-main state]
               (js/document.getElementById "app")))

(defn init []
  (start))
