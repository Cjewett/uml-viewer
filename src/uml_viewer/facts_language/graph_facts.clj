(ns uml-viewer.facts-language.graph-facts
  "Facts LanguageGraph: reads a facts file another tool wrote from its
  compiler instead of parsing source. `ub graph` writes one for C#."
  (:require [clojure.string :as str]
            [uml-viewer.facts-language.facts :as facts]
            [uml-viewer.graph :as graph]))

(def ^:private stereotypes
  "Facts stereotypes in the IR's spelling. Others pass through."
  {:enum :enumeration})

(defn- stereotype-of [c]
  (when-let [st (:stereotype c)]
    (let [k (keyword (name st))]
      (get stereotypes k k))))

(defn- file-of [base rel]
  (some-> (facts/resolve-path base rel) graph/relative-path))

(defn- op-of [base op]
  (cond-> {:name (str (:name op)) :text (str (:name op))}
    (:line op) (assoc :line (long (:line op)))
    (:file op) (assoc :file (file-of base (:file op)))))

(defn- ops-of
  "One op per name, the first declaration of an overload set."
  [base ops]
  (->> ops
       (reduce (fn [[seen acc] op]
                 (if (contains? seen (:name op))
                   [seen acc]
                   [(conj seen (:name op)) (conj acc (op-of base op))]))
               [#{} []])
       second))

(defn- project-class
  "A scanned class. `:ns` is the full name the CRAP and mutation snapshots
  use, so the overlay joins each class to its own snapshot."
  [base prefix c]
  (let [full (str (:id c))
        ops (ops-of base (:ops c))
        files (keep #(file-of base %) (:files c))
        st (stereotype-of c)]
    (cond-> {:id (graph/id-of full prefix)
             :name (or (:name c) (graph/module-name (graph/id-of full prefix)))
             :ns full
             :lang :facts
             :file (file-of base (:file c))}
      (seq files) (assoc :files (vec files))
      (seq ops) (assoc :ops ops)
      st (assoc :stereotype st))))

(defn- id-map
  "Facts id to graph id: project ids lose the prefix, foreign ids stay whole."
  [prefix classes]
  (into {}
        (map (fn [c]
               (let [full (str (:id c))]
                 [full (if (:foreign c)
                         (keyword full)
                         (graph/id-of full prefix))])))
        classes))

(defn- edge-of [ids e]
  (let [from (get ids (str (:from e)))
        to (get ids (str (:to e)))]
    (when (and from to (not= from to))
      {:from from :to to :kind (keyword (name (or (:kind e) :dependency)))})))

(defn scan-facts
  "Graph of the facts document `doc`. Relative paths resolve against `base`.
  Foreign classes keep their own ids; the policy `:foreign` list collapses
  or drops them, as for every other scanner."
  [doc base prefix]
  (let [classes (vec (:classes doc))
        ids (id-map prefix classes)]
    {:classes (into (mapv #(project-class base prefix %) (remove :foreign classes))
                    (->> classes
                         (filter :foreign)
                         (map #(keyword (str (:id %))))
                         distinct
                         (mapv graph/foreign-class)))
     :edges (->> (:edges doc)
                 (keep #(edge-of ids %))
                 distinct
                 vec)}))

(defrecord FactsGraph []
  graph/LanguageGraph
  (scan [_ root opts]
    (let [file (facts/facts-file root)
          doc (facts/read-facts file)
          prefix (when-not (str/blank? (str (:prefix opts))) (str (:prefix opts)))]
      (scan-facts doc (facts/base-dir file) prefix))))

(def impl (->FactsGraph))

(graph/register! :facts impl)
