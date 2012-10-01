(ns osm-extraction.core
  (:require [clojure.data.xml :as xml])
  (:gen-class))

(def name-key? #{"loc_name" "name" "name:fi"})

(defn- find-names [el]
  (let [name-tags (filter #(name-key? (:k (:attrs %))) (:content el))]
    (map #(:v (:attrs %)) name-tags)))

(defn- add-way [mp el]
  (reduce
    #(assoc %1 %2 "")
    mp
    (find-names el)))
    
(defn- add-node [mp el]
  (reduce
    #(assoc %1 %2 (str (:lon (:attrs el)) "," (:lat (:attrs el))))
    mp
    (find-names el)))

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
