(defrecord XML-element [name attributes contents])

(defn attr-str
  [attr]
  (str/join " "
  (for [a attr]
    (str (key a) "=\"" (val a) "\""))))


(def tree
  (list (->XML-element 
          "html" {}
          (->XML-element 
            "body" {}
            (list 
              (->XML-element 
                "header" {}
                (list 
                  (->XML-element "h1" {} "Website title")
                  (->XML-element "h2" { :class "author" } "Andrew Cashner")))
              (->XML-element 
                "main" {}
                (list 
                  (->XML-element 
                    "section" { :id "sec:intro" }
                    (list 
                      (->XML-element "h1" {} "Introduction")
                      (->XML-element "p" {} "This is the intro.")))
                  (->XML-element 
                    "section" { :id "sec:conclusion" }
                    (list 
                      (->XML-element "h1" {} "Conclusion")
                      (->XML-element "p" {} "This is the end."))))))))))

(defn XML-string
  [node]
  (str "<" (:name node) (attr-str (:attributes node)) ">" 
       (XML-string (:contents node))
       "</" (:name node) ">"))

(str/join "" (map XML-string tree))

