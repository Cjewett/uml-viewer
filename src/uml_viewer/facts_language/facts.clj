(ns uml-viewer.facts-language.facts
  "Facts files: a graph another tool wrote from its own compiler (`ub graph`
  for C# writes `.metrics/graph.edn`). Shared by the facts scanner and the
  facts extractor."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def metrics-dir ".metrics")
(def facts-name "graph.edn")
(def supported-version 1)

(defn facts-file
  "The facts file `src` names: `src` itself, or `.metrics/graph.edn` under
  `src` when it is a directory."
  [src]
  (let [f (io/file (str src))]
    (if (.isDirectory f)
      (io/file f metrics-dir facts-name)
      f)))

(defn base-dir
  "Directory the paths inside `file` are relative to: the parent of the
  `.metrics` directory that holds it, else the working directory."
  [file]
  (let [dir (.getParentFile (.getCanonicalFile (io/file file)))]
    (if (and dir (= metrics-dir (.getName dir)))
      (.getParentFile dir)
      (.getCanonicalFile (io/file (or (System/getProperty "user.dir") "."))))))

(defn read-facts
  "The facts document at `src`. Throws when the file is missing or its
  `:version` is not one this reader knows."
  [src]
  (let [f (facts-file src)]
    (when-not (.isFile f)
      (throw (ex-info (str "facts file not found: " (.getPath f)) {:src src})))
    (let [doc (edn/read-string (slurp f))]
      (when-not (= supported-version (:version doc))
        (throw (ex-info (str "unsupported facts version " (pr-str (:version doc))
                             " in " (.getPath f))
                        {:src src :version (:version doc)})))
      doc)))

(defn resolve-path
  "Absolute file for `rel` under `base`, or nil when `rel` is blank."
  [base rel]
  (when-not (str/blank? (str rel))
    (let [f (io/file (str rel))]
      (if (.isAbsolute f) f (io/file base (str rel))))))

(defn find-facts-file
  "The nearest `.metrics/graph.edn` at or above the directory of `path`."
  [path]
  (when-not (str/blank? (str path))
    (loop [dir (.getParentFile (.getCanonicalFile (io/file (str path))))]
      (when dir
        (let [candidate (io/file dir metrics-dir facts-name)]
          (if (.isFile candidate)
            candidate
            (recur (.getParentFile dir))))))))
