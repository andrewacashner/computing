; First Clojure try, 2023/05/12
(require '[clojure.string :as str])

(defn join-with-ending
  "Insert `delim` between each item, and `ending` before last"
  [delim ending items]
  (let [delimited (interpose delim items)
        final     (last delimited)
        series    (butlast delimited)]
    (str (str/join "" series) ending final)))

(defn oxford-name-list
  "Separate list of names with Oxford comma"
  [names]
  (join-with-ending ", " "and " names))

(defn oxford-greeting
  "Return a string with a greeting message to a list of names with Oxford comma"
  [salutation names]
  (str salutation ", " (oxford-name-list names) "!"))

; Use `& arg` to package remaining args as list
(defn greeting
  "Greet one or more people by name
    Input:  (1) String message, (2) List of string names
    Returns: String"
  [msg & people] 
  (let [people-count (count people)]
  (cond
    (= people-count 0) (str msg "!")
    (= people-count 1) (str msg ", " (first people))
    (< people-count 3) (str msg ", " (str/join " and " people) "!")
    (>= people-count 3) (oxford-greeting msg people)))) 

(def hello-str "Hello")

; Use def and partial to create partial function; avoid extra processing of
; args
(def hello (partial greeting hello-str))


