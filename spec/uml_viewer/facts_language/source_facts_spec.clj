(ns uml-viewer.facts-language.source-facts-spec
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.facts-language.source-facts :as source-facts]
            [uml-viewer.source :as source]))

;; The class card passes the class's :ns, :lang and :file and the op's name.
(def order-file "spec/resources/facts/src/Shop/Order.cs")

(defn- ident [ns-name member]
  (cond-> {:lang :facts :ns ns-name :file order-file}
    member (assoc :name member)))

(describe "facts extractor"
  (it "is registered as :facts"
    (should (satisfies? source/LanguageSource (source/lookup :facts))))

  (it "opens the recorded line, not the first mention of the name"
    (let [found (source/member-source (ident "Shop.Order" "get_Total"))]
      (should= :facts (:lang found))
      (should= 10 (:line found))
      (should (str/ends-with? (:file found) "spec/resources/facts/src/Shop/Order.cs"))
      (should (re-find #"Order\.cs:10$" (:title found)))
      (should (str/includes? (:body found) "public Money Total"))))

  (it "opens the file the facts record for a member of a partial class"
    (let [found (source/member-source (ident "Shop.Order" "Price"))]
      (should (str/ends-with? (:file found) "src/Shop/Order.Pricing.cs"))
      (should= 5 (:line found))
      (should (str/includes? (:body found) "public decimal Price(Tier tier)"))))

  (it "searches the class file for the source name when nothing is recorded"
    (let [count-line (source/member-source (ident "Shop.Unrecorded" "Count"))
          total-line (source/member-source (ident "Shop.Unrecorded" "get_Total"))]
      (should= 16 (:line count-line))
      (should (str/ends-with? (:file count-line) "src/Shop/Order.cs"))
      (should= 3 (:line total-line))))

  (it "returns nil when the member is neither recorded nor in the file"
    (should-be-nil (source/member-source (ident "Shop.Order" "Missing"))))

  (it "opens a class at the top when no member is named"
    (let [found (source/member-source (ident "Shop.Order" nil))]
      (should= order-file (:file found))
      (should-be-nil (:line found))))

  (it "searches a file that has no facts file above it"
    (let [dir (io/file (System/getProperty "java.io.tmpdir")
                       (str "uml-facts-src-" (System/nanoTime)))
          file (io/file dir "Shape.cs")]
      (io/make-parents file)
      (spit file "public abstract class Shape\n{\n    public Shape() { }\n    public abstract double Area();\n}\n")
      (let [found (source/member-source {:lang :facts :ns "Shape" :file (.getPath file) :name "Area"})
            ctor (source/member-source {:lang :facts :ns "Shape" :file (.getPath file) :name ".ctor"})]
        (should= 4 (:line found))
        (should= 3 (:line ctor)))))

  (it "maps CLR metadata names back to source names"
    (should= "Total" (source-facts/source-name "Shop.Order" "get_Total"))
    (should= "Total" (source-facts/source-name "Shop.Order" "set_Total"))
    (should= "Shipped" (source-facts/source-name "Shop.Order" "add_Shipped"))
    (should= "Order" (source-facts/source-name "Shop.Order" ".ctor"))
    (should= "operator" (source-facts/source-name "Shop.Money" "op_Addition"))
    (should= "Parse" (source-facts/source-name "Shop.Money" "Parse"))))
