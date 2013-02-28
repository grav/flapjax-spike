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

(defn isEqualB [B v]
  (fj/liftB
   (fn [v2]
     (= v v2 )) B))

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
  (condB (isActiveB? activeB :frontpage) (fj/constantB :frontpage)
         (isActiveB? activeB :counting) (fj/constantB :counting)))

(defn menuB [B]
  (fj/liftB
   (fn [items]
     (let [menu (dom/createDom "div")]
       (doseq [item items]
         (let [e (dom/createDom "span" (clj->js {:class "menu-item"}) item)]
           (dom/appendChild menu e)))
       menu)) B))

;; rest

(defn rest-request [url]
  (clj->js {:url url
            :request "get"
            :response "plain"}))


(defn switch-activity [main activity breast-feed nappy-change]
  (when (= :counting main)
    (case activity
      :breast-feed breast-feed
      :nappy-change nappy-change)))

(defn breast-feed-request [child]
  (rest-request (str "/rest/" child "/breast-feed"))  )

(defn nappy-change-request [child]
  (rest-request (str "/rest/" child "/nappy-change"))  )

(defn restE [requestE]
  (fj/mapE
   reader/read-string
   (fj/getWebServiceObjectE requestE)))

(defn getSwitchE
  "Invokes switch-fn on switchB value to get E and sends inputB value as event to E."
  [switchB inputB switch-fn]
  (fj/liftB
   (fn [switch input]
     (let [E (switch-fn switch)]
       (when E
         (fj/sendEvent E input))))
   switchB
   inputB))

(defn liftVectorB
  [& Bs]
  (apply fj/liftB
         (fn [& args] args)
         Bs))

;; dom

(defn frontPageDomB []
  (fj/oneE (dom/createDom "h3" nil "Welcome!")))

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
        mainB (fj/constantB :counting)
        activityB (-> (fj/mergeE frontpage-clicksE counting-clicksE)
                      toElementE
                      (fj/startsWith :breast-feed))
        childB (fj/constantB "olga")

        switchB (liftVectorB mainB activityB)
        breastFeedE (fj/receiverE)
        nappyChangeE (fj/receiverE)
        switch-fn (fn [[main activity]] (switch-activity main activity breastFeedE nappyChangeE))]

    (->> breastFeedE
         (fj/mapE breast-feed-request)
         restE
         (fj/mapE (fn [s] (.log js/console "(.)(.)" (pr-str s)))))

    (->> nappyChangeE
         (fj/mapE nappy-change-request)
         restE
         (fj/mapE (fn [s] (.log js/console "(_|_)" (pr-str s)))))

    (getSwitchE switchB childB switch-fn)))
