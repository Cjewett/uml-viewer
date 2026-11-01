(ns uml-viewer.facts-language.graph-facts-spec
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.application.ir-generator :as ir-generator]
            [uml-viewer.application.overlay :as overlay]
            [uml-viewer.facts-language.facts :as facts]
            [uml-viewer.facts-language.graph-facts]
            [uml-viewer.graph :as graph]))

;; spec/resources/facts/.metrics/graph.edn is what `ub graph` wrote for the
;; C# files under spec/resources/facts/src.
(def fixture-dir "spec/resources/facts")
(def fixture-file "spec/resources/facts/.metrics/graph.edn")

(defn- temp-text [text]
  (let [dir (io/file (System/getProperty "java.io.tmpdir")
                     (str "uml-facts-" (System/nanoTime)))
        f (io/file dir ".metrics" "graph.edn")]
    (io/make-parents f)
    (spit f text)
    dir))

(defn- temp-facts [doc]
  (temp-text (pr-str doc)))

(defn- failure
  "The message `read-facts` throws for `src`, or nil."
  [src]
  (try
    (facts/read-facts src)
    nil
    (catch Exception e (ex-message e))))

(defn- scan [src prefix]
  (graph/scan (graph/lookup :facts) src {:prefix prefix}))

(defn- by-id [g]
  (into {} (map (juxt :id identity) (:classes g))))

(defn- edge-set [edges]
  (set (map (juxt :from :to :kind) edges)))

(describe "facts files"
  (it "finds .metrics/graph.edn under a directory, or takes the file itself"
    (should= (.getCanonicalFile (io/file fixture-file))
             (.getCanonicalFile (facts/facts-file fixture-dir)))
    (should= (.getCanonicalFile (io/file fixture-file))
             (.getCanonicalFile (facts/facts-file fixture-file))))

  (it "resolves paths against the parent of .metrics"
    (should= (.getCanonicalFile (io/file fixture-dir))
             (.getCanonicalFile (facts/base-dir (io/file fixture-file)))))

  (it "finds the nearest facts file above a source file"
    (should= (.getCanonicalFile (io/file fixture-file))
             (.getCanonicalFile (facts/find-facts-file "spec/resources/facts/src/Shop/Order.cs"))))

  (it "refuses a missing file and an unknown version"
    (should (str/starts-with? (failure "spec/resources/no-such-dir") "facts file not found: "))
    (should (str/starts-with? (failure (temp-facts {:version 2 :classes [] :edges []}))
                              "unsupported facts version 2 in ")))

  (it "tells a file that is not a facts map from one it cannot parse"
    (let [empty-dir (temp-text "")
          vector-dir (temp-text "[1 2 3]")
          broken-dir (temp-text "{:version 1 :classes [")]
      (should (str/starts-with? (failure empty-dir) "not a facts file: "))
      (should (str/starts-with? (failure vector-dir) "not a facts file: "))
      (should (str/starts-with? (failure broken-dir) "cannot read facts file "))
      (should (str/includes? (failure broken-dir) "graph.edn")))))

(describe "facts graph"
  (it "is registered as :facts"
    (should (satisfies? graph/LanguageGraph (graph/lookup :facts))))

  (it "strips the prefix and keeps the full type name as :ns"
    (let [g (scan fixture-dir "Shop")
          classes (by-id g)
          project (set (keep (fn [[id c]] (when-not (:foreign c) id)) classes))]
      (should= #{:IPriced :Money :Order :Order.Line :Tier} project)
      (should= "Shop.Order" (:ns (classes :Order)))
      (should= "Shop.Order.Line" (:ns (classes :Order.Line)))
      (should= "Order" (:name (classes :Order)))
      (should= "Line" (:name (classes :Order.Line)))
      (should= :facts (:lang (classes :Order)))
      (should (str/ends-with? (:file (classes :Order)) "spec/resources/facts/src/Shop/Order.cs"))
      (should= 2 (count (:files (classes :Order))))
      (should (str/ends-with? (second (:files (classes :Order))) "src/Shop/Order.Pricing.cs"))))

  (it "keeps a class whole when no prefix is given"
    (should (contains? (by-id (scan fixture-file nil)) :Shop.Order)))

  (it "maps stereotypes to the IR's spelling"
    (let [classes (by-id (scan fixture-dir "Shop"))]
      (should= :interface (:stereotype (classes :IPriced)))
      (should= :enumeration (:stereotype (classes :Tier)))
      (should-be-nil (:stereotype (classes :Order)))
      (should= :abstract (:stereotype (get (by-id (scan (temp-facts {:version 1
                                                                       :classes [{:id "A.Shape" :name "Shape" :ns "A"
                                                                                  :stereotype :abstract :file "Shape.cs"}]
                                                                       :edges []})
                                                          "A"))
                                            :Shape)))))

  (it "lists one op per name with its recorded line and file"
    (let [order ((by-id (scan fixture-dir "Shop")) :Order)
          price (some #(when (= "Price" (:name %)) %) (:ops order))]
      (should= ["get_Total" "Count" "Dispose" "Price"] (map :name (:ops order)))
      (should= ["get_Total" "Count" "Dispose" "Price"] (map :text (:ops order)))
      (should= 10 (:line (first (:ops order))))
      (should= 5 (:line price))
      (should (str/ends-with? (:file price) "src/Shop/Order.Pricing.cs"))
      (should-be-nil (:ops ((by-id (scan fixture-dir "Shop")) :Tier)))))

  (it "emits foreign classes whole and edges between graph ids"
    (let [g (scan fixture-dir "Shop")
          classes (by-id g)
          edges (edge-set (:edges g))]
      (should= {:id :System.IDisposable :name "System.IDisposable"
                :ns "System.IDisposable" :foreign true}
               (classes :System.IDisposable))
      (should (:foreign (classes :System.Collections)))
      (should (contains? edges [:Order :IPriced :implements]))
      (should (contains? edges [:Order :System.IDisposable :implements]))
      (should (contains? edges [:Order :Money :dependency]))
      (should (contains? edges [:Order :Order.Line :dependency]))
      (should (contains? edges [:Order.Line :Money :dependency]))
      (should (contains? edges [:IPriced :Money :dependency]))
      (should= 8 (count (:edges g)))))

  (it "drops edges to unknown ids and onto the class itself"
    (let [g (scan (temp-facts {:version 1
                               :classes [{:id "A.B" :name "B" :ns "A" :file "B.cs"}]
                               :edges [{:from "A.B" :to "A.Ghost" :kind :dependency}
                                       {:from "A.B" :to "A.B" :kind :dependency}]})
                  "A")]
      (should= [] (:edges g))))

  (it "collapses foreign classes through the policy and keeps implements over a dependency"
    (let [doc (ir-generator/document
                (graph/lookup :facts)
                {:title "Shop"
                 :prefix "Shop"
                 :lang :facts
                 :src fixture-file
                 :hierarchical true
                 :foreign [:System]})
          ids (set (map :id (:classes doc)))
          edges (edge-set (:edges doc))]
      (should (contains? ids :System))
      (should-not (contains? ids :System.IDisposable))
      (should-not (contains? ids :System.Collections))
      (should (contains? edges [:Order :System :implements]))
      (should-not (contains? edges [:Order :System :dependency]))
      (should (contains? edges [:Order :IPriced :implements]))
      (should= :facts (:lang (some #(when (= :Order (:id %)) %) (:classes doc))))))

  (it "drops unlisted foreign classes"
    (let [doc (ir-generator/document
                (graph/lookup :facts)
                {:prefix "Shop" :lang :facts :src fixture-dir :hierarchical true})]
      (should= [] (filter :foreign (:classes doc)))))

  (it "lets the overlay join each class to its own snapshot"
    (let [doc (ir-generator/document
                (graph/lookup :facts)
                {:prefix "Shop" :lang :facts :src fixture-dir :hierarchical true})
          crap {"Shop.Order" [{:name "get_Total" :namespace "Shop.Order"
                               :complexity 2 :coverage 100.0 :crap 2.0}]}
          painted (overlay/apply-metrics doc {:crap crap :mutate {}})
          classes (by-id painted)
          total (some #(when (= "get_Total" (:name %)) %) (:ops (classes :Order)))]
      (should= 2 (:cc total))
      (should= 1.0 (:coverage total))
      (should-be-nil (:cc (some #(when (= "get_Zero" (:name %)) %) (:ops (classes :Money)))))
      (should-be-nil (:crap (classes :Money))))))
