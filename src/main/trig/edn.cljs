(ns trig.edn
  (:require [cljs.pprint :as pprint]))

(defonce ^:dynamic *key-counter* (atom 0))

(defn get-key []
  (swap! *key-counter* inc)
  (str "k-" @*key-counter*))

(declare html)

(defn literal? [x]
  (and
   (not (map-entry? x))
   (not (seq? x))
   (not (coll? x))))

(defn separator* [s]
  [:span.seperator ^{:key (get-key)} s])

(defn clearfix-separator* [s]
  [:span ^{:key (get-key)} (separator* s) [:span.clearfix]])

(defn separate-fn [coll]
  (try
    (if (not (every? literal? coll)) clearfix-separator* separator*)
    (catch js/Error e
      clearfix-separator*)))

(defn interpose-separator [rct-coll s sep-fn]
  (->> (rest rct-coll)
       (interleave (repeatedly #(sep-fn s)))
       (cons (first rct-coll))
       to-array))

(defn pprint-str [obj]
  ;; this is currently only to handle the
  ;; problem of printing JavaScript Symbols
  (try
    (with-out-str (pprint/pprint obj))
    (catch js/Error e1
      (try
        (.toString obj)
        (catch js/Error e2
          (str "<<Un-printable Type>>"))))))

(defn literal [class x]
  [:span ^{:className class :key (get-key)} (pprint-str x)])

(defn html-val [index v]
  [:span ^{:key index} (html v)])

(defn join-html [separator coll]
  (interpose-separator (into [] (map-indexed html-val coll))
                       separator
                       (separate-fn coll)))

(defn html-keyval [[k v]]
  [:span.keyval ^{:key (prn-str k)} (html k) (html v)])

(defn html-keyvals [coll]
  (js->clj (interpose-separator (mapv html-keyval coll)
                                " "
                                (separate-fn (vals coll)))))

(defn open-close [class-str opener closer rct-coll]
  [:span ^{:className class-str :key (str (hash rct-coll))}
    [:span.opener ^{:key 1} opener]
    [:span.contents ^{:key 2} rct-coll]
    [:span.closer ^{:key 3} closer]])

(defn html-collection [class opener closer coll]
  (open-close (str "collection " class) opener closer (join-html " " coll)))

(defn html-map [coll]
  (open-close "collection map" "{" "}" (html-keyvals coll)))

(defn html-string [s]
  (open-close "string" "\"" "\"" s))

(defn html [x]
  (cond
    (number? x)  (literal "number" x)
    (keyword? x) (literal "keyword" x)
    (symbol? x)  (literal "symbol" x)
    (string? x)  (html-string x)
    (map? x)     (html-map x)
    (set? x)     (html-collection "set"    "#{" "}" x)
    (vector? x)  (html-collection "vector" "[" "]" x)
    (seq? x)     (html-collection "seq"    "(" ")" x)
    :else        (literal "literal" x)))

(defn html-edn [e]
  (binding [*key-counter* (atom 0)]
    [:div.edn-block
               ^{:key "edn-block"} (html e)]))

(html-edn {:label2 "B", :label1 "A", :angle3 55.3033976437348, :line2 10, :line3 13, :angle1 48.368620460647016, :label3 "C", :angle2 76.3279818956182, :line1 11})