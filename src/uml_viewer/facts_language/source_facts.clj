(ns uml-viewer.facts-language.source-facts
  "Facts LanguageSource: open the file the facts file records for a member
  and jump to its recorded line. Without a record, search the class file
  for the member's source name."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.facts-language.facts :as facts]
            [uml-viewer.source :as source]))

(def ^:private pattern-fmts
  ["\\b%s\\s*(?:<[^>\\n]*>)?\\s*\\("
   "\\b%s\\b"])

(defn source-name
  "Name to search for when a member has no recorded line. CLR metadata
  names map back to source: `get_Total` is `Total`, `.ctor` is the class
  name, `op_Addition` is `operator`."
  [ns-name member]
  (let [member (str member)]
    (cond
      (#{".ctor" ".cctor"} member) (last (str/split (str ns-name) #"\."))
      (re-find #"^(?:get|set|init|add|remove)_." member) (subs member (inc (str/index-of member "_")))
      (str/starts-with? member "op_") "operator"
      :else member)))

(defn recorded-op
  "The op the nearest facts file records for `ident`, with its file
  resolved, when that file exists. Nil otherwise."
  [ident]
  (when-let [file (facts/find-facts-file (:file ident))]
    (let [doc (try (facts/read-facts file) (catch Exception _ nil))
          base (facts/base-dir file)
          c (some #(when (= (str (:ns ident)) (str (:id %))) %) (:classes doc))
          op (some #(when (= (str (:name ident)) (str (:name %))) %) (:ops c))
          path (facts/resolve-path base (or (:file op) (:file c)))]
      (when (and op (:line op) path (.isFile (io/file path)))
        {:file (str/replace (.getPath (io/file path)) #"\\" "/")
         :line (long (:line op))}))))

(defn- line-count [text]
  (inc (count (re-seq #"\n" text))))

(defn- recorded-line [text ident]
  (when-let [{:keys [line]} (recorded-op ident)]
    (when (<= 1 line (line-count text))
      line)))

(defn- searched-line [text ident]
  (source/member-line-of text (source-name (:ns ident) (:name ident)) pattern-fmts))

(defn- line-text [text line]
  (nth (str/split-lines text) (dec line) nil))

(defn member-line
  "1-based line of `ident` in `text`: the recorded line, else a search."
  [text ident]
  (or (recorded-line text ident) (searched-line text ident)))

(def impl
  (reify source/LanguageSource
    (locate [_ ident]
      (or (when (seq (str (:name ident)))
            (:file (recorded-op ident)))
          (source/existing-file ident)))
    (extract [_ text ident]
      (when-let [line (member-line text ident)]
        (some-> (line-text text line) str/trimr)))
    (start-line [_ text ident]
      (member-line text ident))
    (title [_ ident]
      (source/file-title ident))))

(source/register! :facts impl)
