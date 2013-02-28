(ns flapjax-spike.main
  (:require [flapjax :as fj]
            [goog.dom.classes :as classes]
            [goog.dom :as dom]
            [cljs.reader :as reader]))

(defn elm
  "If e is a string it is looked as an id in the dom tree.
  Otherwise it is returned."
  [e]
  (if (string? e)
    (dom/getElement e)
    e))

(defn elm-id [e]
  "If e is a string return it directly.
  Otherwise regard it as a dom element and return its id"
  (.-id (elm e)))

(defn e=
  "Compares two elements, represented as dom elements or
  their ids."
  [e1 e2]
  (= (elm e1) (elm e2)))

(defn toElementE [eventE]
  (fj/mapE (fn [event] (.-toElement event)) eventE))

(defn activeClassB [B e]
  (fj/liftB (fn [target-elm]
              (if (e= target-elm e) "active" "")) B))

(defn condB [& pairs]
  (apply fj/condB (map clj->js (partition 2 pairs))))

(def page-map
  {"counting-link" :counting
   "frontpage-link" :frontpage})

(defn isActiveB? [B k]
  (fj/liftB
   (fn [e]
     (let [id (elm-id e)
           result (= (id page-map) k)]
       (.log js/console result)
       result)) B))

(def namesB
  (fj/constantB ["olga" "otto"]))

(defn activitiesB []
  (let [request (clj->js {:url "/rest/activities"
                          :request "get"
                          :response "plain"})]
    (fj/startsWith (fj/getWebServiceObjectE request) ["No activities"])))

(defn frontPageB [])

(defn countingB [])

(defn mainContentB [activeB]
  (condB (isActiveB? activeB :frontpage) :frontpage
         (isActiveB? activeB :counting) :counting))

(defn menuB [B]
  (fj/liftB
   (fn [items]
     (let [menu (dom/createDom "div")]
       (doseq [item items]
         (let [e (dom/createDom "span" (clj->js {:class "menu-item"}) item)]
           (dom/appendChild menu e)))
       menu)) B))

;; rest

(defn getRestE [urlE]
  (fj/mapE #(reader/read-string %)
           (fj/getWebServiceObjectE
            (fj/mapE #(clj->js {:url %
                                :request "get"
                                :response "plain"})
                     urlE))))

(defn breastFeedGetRestE [childE]
  (getRestE (fj/mapE
             (fn [child]
               (str "/rest/" child "/breast-feed")) childE)))

(defn breastFeedDomFromDataE [dataE]
  (fj/mapE
   (fn [{:keys [meta count timestamp]}]
     (let [value (str "Breast-fed " count " times. Last one was at " timestamp " on the " (:side meta) " side.")]
      (dom/createDom "span" nil value))) dataE))

(defn dynamicDomB [domE]
  (fj/startsWith domE (dom/createDom "span" nil "Loading ...")))

;; init

(defn ^:export init []
  (let [frontpage-clicksE (fj/clicksE "frontpage-link")
        counting-clicksE (fj/clicksE "counting-link")
        currentActiveB (-> (fj/mergeE frontpage-clicksE counting-clicksE)
                           toElementE
                           (fj/startsWith "frontpage-link"))]

    (let [nameE (fj/oneE "olga")]
     (fj/insertDomB
      (dynamicDomB
       (breastFeedDomFromDataE (breastFeedGetRestE nameE)))
      "content-holder"))


    #_    (doseq [e ["frontpage-link" "counting-link"]]
      (fj/insertValueB (activeClassB currentActiveB e)
                       e
                       "className"))
    #_    (fj/insertDomB (menuB namesB) "name-menu")

    #_    (fj/insertValueB (contentB currentActiveB) "content-holder" "innerHTML")))
