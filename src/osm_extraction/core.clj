(ns osm-extraction.core
  (:require [clojure.data.xml :as xml])
  (:gen-class))

(def name-key? #{"name" "name:fi"})

(defn- detect [pred coll]
  (some #(when (pred %) %) coll))

(defn- find-name [el]
  (when-let [name-tag (detect #(name-key? (:k (:attrs %))) (:content el))]
    (:v (:attrs name-tag))))

(defn- add-way [mp el]
  (if-let [name (find-name el)]
    (assoc mp name "")
    mp))

(defn- add-node [mp el]
  (if-let [name (find-name el)]
    (assoc mp name (str (:lon (:attrs el)) "," (:lat (:attrs el))))
    mp))

(defn -main
  "I don't do a whole lot."
  [& args]
  (let [all (reduce
              (fn [res el]
                (condp = (:tag el)
                  :way  (add-way res el)
                  :node (add-node res el)
                  res))
              {}
              (:content (xml/parse *in*)))]
    (doseq [[k v] all]
      (println (str k "|" v)))))
