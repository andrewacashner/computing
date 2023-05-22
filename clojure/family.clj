(defrecord Date [year month day])
(defrecord Name [firstName middleName lastName])
(defrecord Person [name sex birth death relationship])
(declare Andrew, Ann, Ben, Joy)

(def Andrew (map->Person
              {:id :Andrew
               :name (->Name "Andrew" "Aaron" "Cashner")
               :sex :male
               :birth (->Date 1981 04 11)
               :death nil
               :relationship {:spouse #{:Ann},
                              :child #{:Ben, :Joy}}}))

(def Ann (map->Person
          {:id :Ann
           :name (->Name "Ann" "Charlotte" "Cashner")
           :sex :female
           :birth (->Date 1983 03 24)
           :death nil
           :relationship {:spouse #{:Andrew},
                          :child #{:Ben, :Joy}}}))

(def Ben (map->Person
          {:id :Ben
           :name (->Name "Benjamin" "Robert" "Cashner")
           :sex :male
           :birth (->Date 2011 01 16)
           :death nil
           :relationship {:parent #{:Ann, :Andrew}, 
                          :sibling #{:Joy}}}))

(def Joy (map->Person
          {:id :Joy
           :name (->Name "Joy" "Elizabeth Laura" "Cashner")
           :sex :female
           :birth (->Date 2014 03 10)
           :death nil
           :relationship {:parent #{:Ann, :Andrew},
                          :sibling #{:Ben}}}))

(def BachJS (map->Person
            {:id :Bach
             :name (->Name "Johann" "Sebastian" "Bach")
             :sex :male
             :birth (map->Date {:year 1685})
             :death (map->Date {:year 1750})
             :relationship {:child #{:BachWF, :BachCPE, :BachJC}}}))


(def Cashners #{ Andrew Ann Ben Joy })


(defn alive? [person] (nil? (:death person)))

(defn relations [relation person] (-> person :relationship relation))
(def child   (partial relations :child))
(def sibling (partial relations :sibling))
(def spouse  (partial relations :spouse))
(def parent  (partial relations :parent))

(defn hasRelationship?
  [relationFn person1 person2]
  (contains? (relationFn person1) (:id person2)))

(def hasParent?  (partial hasRelationship? parent))
(def hasSpouse?  (partial hasRelationship? spouse))
(def hasSibling? (partial hasRelationship? sibling))
(def hasChild?   (partial hasRelationship? child))

