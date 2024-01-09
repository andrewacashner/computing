;from seesaw github seesaw/test/examples/xyz_panel.clj
(defn moveable
  [widget]
  (let [start-point (java.awt.Point.)]
    (sb/when-mouse-dragged 
      widget
      :start (fn [e] (ss/move! e :to-front)
               (.setLocation start-point (.getPoint e)))
      :drag (fn [e _]
              (let [p (.getPoint e)]
                (ss/move! e :by [(- (.x p) (.x start-point))
                                 (- (.y p) (.y start-point))])))
      :finish (fn [e] 
                (let [p (.getPoint e)]
                  (ss/alert e (format "Dragged to (%d, %d)" (.x p) (.y p)))
                  ; check if mouse release spot is inside correct box
                  ; or send click or other message to box at this spot
                  ; TODO use sd/default-transfer-handler from drag-and-drop
                  ; interface to transfer info from one object to another
                  )
                )))
      ; TODO move timeline cards to allow space for new card
      ; check if correct place
  widget)

(defn clue-card
  [fact]
  (moveable
  (ss/text :text (:description fact)
           card-options)))

(defn fact-card
  [state fact]
  (ss/text :text (show-fact fact)
           :listen [:mouse-clicked 
                    (partial check-and-advance state fact)
                    :mouse-moved
                    (fn [e] (ss/alert e "Dragged to here!"))]
           card-options))


