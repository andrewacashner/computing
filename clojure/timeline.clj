; 2023/09/20

(defrecord Event [date place description])

(def NineEleven 
  (map->Event 
    {:date "2001-09-11" 
     :place "New York, NY, US"
     :description "Terrorist attack on the World Trade Center"}))

(def COVIDPandemic
  (map->Event
    {:date "2020-03-01"
     :place "Global"
     :description "First major surge of COVID-19 Pandemic"}))

(def timeline
  (list COVIDPandemic NineEleven))

(def timeline-order
  (sort-by :date timeline))

(println timeline-order)
