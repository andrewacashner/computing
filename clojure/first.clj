; First Clojure try, 2023/05/12
(require '[clojure.string :as str])

(def hello "Hello")

(defn greeting
  "Return string with hello message to `name`"
  [name]
  (str hello ", " name "!"))

(defn name-list
  "Insert commas between list of names"
  [ls]
  (str/join ", " ls))

(defn group-greeting
  "Return a string with a greeting message to a list of names without Oxford comma"
  [names]
  (greeting (name-list names)))

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


