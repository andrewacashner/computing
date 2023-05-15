; 2023/05/15
(require '[clojure.string :as str])

(defrecord Book [author title location publisher year keywords])

(defrecord PersonName [lastname firstname])

(def Cashner:HearingFaith 
  (map->Book 
    { :author (map->PersonName { :lastname "Cashner",
                                :firstname "Andrew" }),
      :title "Hearing Faith: Music as Theology in the Spanish Empire",
      :location "Leiden",
      :publisher "Brill",
      :year 2020,
      :keywords #{ "Spain -- Music, 17th century",
                   "Mexico -- Music, 17th century",
                   "Villancicos -- History and criticsm",
                   "Christianity -- Roman Catholicism -- Theology" }}))

(def Kircher:Musurgia
  (map->Book
    { :author (map->PersonName { :lastname "Kircher", :firstname "Athanasius" }),
      :title "Musurgia universalis",
      :location "Rome",
      :publisher "Unknown",
      :year 1650,
      :keywords #{ "Music -- History and theory",
                   "Jesuits" }}))

(def bibliography (list Cashner:HearingFaith Kircher:Musurgia))

(defn fmt-PersonName
  [^PersonName name]
  (str (:lastname name) ", " (:firstname name)))

(defn fmt-author
  [^Book book]
  (fmt-PersonName (:author book)))

(defn fmt-imprint
  [^Book book]
  (str (:location book) ": " (:publisher book)))



(defn attr-str
  [attr]
  (str/join " "
  (for [a attr]
    (str (key a) "=\"" (val a) "\""))))

(defn html-element
  ([name contents]
  (str "<" name ">" contents "</" name ">"))
  ([name attr contents]
  (str "<" name " " (attr-str attr) ">" contents "</" name ">")))

(defn html-booktitle
  [s]
  (html-element "cite" s))


(defn latex-cmd
  [csname & args]
  (str "\\" csname "{" (str/join "}{" args) "}"))

(defn latex-env
  [name contents]
  (str "\\begin{" name "}\n" contents "\n\\end{" name "}\n"))

(defn latex-booktitle
  [s]
  (latex-cmd "worktitle" s))

(defn fmt-book
  [bib-style markup-lang ^Book book]
  (let [title-markup-fn 
                (case markup-lang 
                  :html  html-booktitle
                  :latex latex-booktitle),
        author  (fmt-author book),
        year    (:year book),
        title   (title-markup-fn (:title book)),
        imprint (fmt-imprint book)]
    (case bib-style
      :authordate (str author ". " year ". " title ". " imprint ".")
      :notes      (str author ". " title ". (" imprint "), " year "."))))

(defn fmt-bibentry
  [markup-lang entry]
  (case markup-lang
    :html   (html-element "li" entry)
    :latex  (latex-cmd "bibEntry" entry)))

(defn bibframe
  [markup-lang entry-ls]
  (let [bib-str (str/join "\n" entry-ls)]
  (case markup-lang
    :html  (html-element "ul" {"class" "biblio"} bib-str)
    :latex (latex-env "biblio" bib-str))))

(defn fmt-bibliography
  [bib-style markup-lang book-ls]
  (bibframe markup-lang 
    (for [book book-ls] 
      (fmt-bibentry markup-lang 
        (fmt-book bib-style markup-lang book)))))

(println (fmt-bibliography :authordate :html bibliography))
(println (fmt-bibliography :authordate :latex bibliography))

