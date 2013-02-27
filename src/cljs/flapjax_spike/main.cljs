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

#_(defn myCondB [& pairs]
  (reduce (fn [test result])))

(defn myCondB [& pairs]
  (let [[test result] (first pairs)]
    (fj/ifB test
            result
            (if (= (count (rest pairs)) 0)
              (fj/constantB "Bom")
              (myCondB (rest pairs))))))

(defn restB [url]
  (fj/liftB
   (fn [s]
     (reader/read-string s))
   (fj/startsWith
    (fj/getWebServiceObjectE
     (fj/oneE (clj->js {:url url
                        :request "get"
                        :response "plain"})))
    :loading)))

(defn contentB [activeB]
  (fj/ifB (isActiveB? activeB :frontpage)
          (fj/constantB "Frontpage")
          (restB "/rest/activities")))

#_(defn contentB [activeB]
  (fj/condB [(isActiveB? activeB :frontpage) (fj/constantB "Frontpage")]
            [(isActiveB? activeB :counting) (restB "/rest/activities")]))

(defn menuB [B]
  (fj/liftB
   (fn [items]
     (let [menu (dom/createDom "div")]
       (doseq [item items]
         (let [e (dom/createDom "span" (clj->js {:class "menu-item"}) item)]
           (dom/appendChild menu e)))
       menu)) B))

(defn ^:export init []
  (let [frontpage-clicksE (fj/clicksE "frontpage-link")
        counting-clicksE (fj/clicksE "counting-link")
        currentActiveB (-> (fj/mergeE frontpage-clicksE counting-clicksE)
                           toElementE
                           (fj/startsWith "frontpage-link"))]
    (doseq [e ["frontpage-link" "counting-link"]]
      (fj/insertValueB (activeClassB currentActiveB e)
                       e
                       "className"))
    (fj/insertDomB (menuB namesB) "name-menu")

    (fj/insertValueB (contentB currentActiveB) "content-holder" "innerHTML")

))
