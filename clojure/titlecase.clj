; 2023/05/17

(require '[clojure.string :as string])

(defn split-words
  [s]
  (str/split s #"\b"))

(def caps-blacklist
  #{ "a" "and" "as" "for" "in" "of" "on" "the" "to" })

(defn title-caps
  [s]
  (let [lower-s (str/lower-case s)]
    (if (contains? caps-blacklist lower-s)
      lower-s
      (capitalize s))))

(defn capitalize-words
  [s f]
  (->> (split-words (str s))
       (map f)
       (str/join)))

(defn emphatic-case
  [s]
  (capitalize-words s capitalize))

(defn title-case
  [s]
  (capitalize (capitalize-words s title-caps)))

(def fox "the quick, brown fOX named Clarence---it jumped over the lazy dog ON a bike!")
(def nasa "NASA directory Of program services")

(do
  (println (emphatic-case fox))
  (println (title-case fox))
  (println (title-case nasa)))
