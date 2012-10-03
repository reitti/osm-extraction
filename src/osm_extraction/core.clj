(ns osm-extraction.core
  (:require [clojure.data.xml :as xml])
  (:gen-class))

(def name-key? #{"loc_name" "name" "name:fi"})

(defmacro kv-match [[k v] & body]
  `(or ~@(map
           (fn [[k-opt v-opts]]
             (if (= [:*] v-opts)
               `(= ~k ~(name k-opt))
               `(and (= ~k ~(name k-opt))
                     (~(apply hash-set (map name v-opts)) ~v))))
           (partition 2 body))))

(defn- interesting-tag? [{{:keys [k v]} :attrs}]
  (kv-match [k v]
    :aeroway  [:aerodrome]
    :amenity  [:bar :biergarten :cafe :fast_food :food_court :ice_cream :pub :restaurant :college :kindergarten
               :library :school :university :car_rental :car_sharing :ferry_terminal :fuel :bureau_de_change
               :baby_hatch :clinic :dentist :doctors :hospital :nursing_home :pharmacy :social_facility :veterinary
               :arts_centre :cinema :community_centre :fountain :nightclub :social_centre :stripclub :studio :swingerclub
               :theatre :brothel :courthouse :crematorium :embassy :fire_station :grave_yard :marketplace :place_of_worship
               :police :post_office :prison :public_building :sauna :townhall]
    :historic [:monument :memorial]
    :leisure  [:*]
    :office   [:*]
    :sport    [:*]
    :shop     [:*]
    :tourism  [:camp_site :caravan_site :chalet :guest_house :hostel :hotel :information :motel :museum :picnic_site :theme_park :zoo]
    :railway  [:station :tram_stop]
    :place    [:locality]))

(defn- interesting-node? [el]
  (some interesting-tag? (filter #(= :tag (:tag %)) (:content el))))

(defn- find-names [el]
  (let [name-tags (filter #(name-key? (:k (:attrs %))) (:content el))]
    (into #{} (map #(:v (:attrs %)) name-tags))))

(defn- add-way [mp el]
  (assoc
    mp
    (:id (:attrs el))
    {:names (find-names el), :loc ""}))

(defn- add-node [mp el]
  (if (interesting-node? el)
      (assoc 
        mp
        (:id (:attrs el))
        {:names (find-names el)
         :loc (str (:lon (:attrs el)) "," (:lat (:attrs el)))})
      mp))

(defn -main [& args]
  (let [all (reduce
              (fn [res el]
                (condp = (:tag el)
                  :way  (add-way res el)
                  :node (add-node res el)
                  res))
              {}
              (:content (xml/parse *in*)))
        by-name (reduce
                  (fn [mp {:keys [names loc]}]
                    (reduce #(assoc %1 %2 loc) mp names))
                  {}
                  (vals all))]
    (doseq [[name loc] by-name]
      (println (str name "|" loc)))))
