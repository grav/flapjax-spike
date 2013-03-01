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

(defn mainContentB [activeB]
  (condB (isActiveB? activeB :frontpage) (fj/constantB :frontpage)
         (isActiveB? activeB :counting) (fj/constantB :counting)))

(defn menu [items]
  (let [menu (dom/createDom "div")]
   (doseq [item items]
     (let [span (dom/createDom "span")
           a (dom/createDom "a" (clj->js {"id" (str item)
                                          "href" "#"
                                          "class" "menu-item"}) item)]
       (dom/appendChild span a)
       (dom/appendChild menu span)))
   menu))

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

(defn children-request []
  (rest-request "/rest/children"))

(defn restE [requestE]
  (fj/mapE
   reader/read-string
   (fj/getWebServiceObjectE requestE)))

(defn switch [breastFeedE nappyChangeE]
  (fn [main activity]
    (switch-activity main activity [breastFeedE breast-feed-request] [nappyChangeE nappy-change-request])))

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

(def activity-map
  {"nappy-link" :nappy-change
   "breast-link" :breast-feed})

(defn frontPageDomB []
  (fj/oneE (dom/createDom "h3" nil "Welcome!")))

(defn breastFeedDomFromDataE [dataE]
  (fj/mapE
   (fn [{:keys [meta count timestamp]}]
     (let [value (str "Breast-fed " count " times. Last one was at " timestamp " on the " (:side meta) " side.")]
      (dom/createDom "span" nil value))) dataE))

(defn dynamicDomB [domE]
  (fj/startsWith domE (dom/createDom "span" nil "Loading ...")))

(defn logE [E]
  (fj/mapE #(.log js/console (pr-str %)) E))

;; init

(defn addChildE [id domB]
  (.log js/console id)
  (fj/insertValueE domB id "innerHTML"))

(defn ^:export init []
  (let [activityE (->>
                   (fj/mergeE
                    (fj/clicksE "nappy-link")
                    (fj/clicksE "breast-link"))
                   (fj/mapE (fn [event]
                              (let [elm (.-toElement event)
                                    id (.-id elm)]
                                (id activity-map)))))

        childrenE (restE (fj/mapE children-request))

        mainB (fj/constantB :counting)
        activityB (fj/startsWith activityE :breast-feed)

        childB (fj/constantB "Olga")


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

    (getSwitchE switchB childB switch-fn)

    (fj/insertDomE
     (fj/mapE menu childrenE)
     "child-menu")

    (fj/mapE (fn [e] (.log js/console "foo")) (fj/clicksE "test-link"))

    (->>
     childrenE
     logE))





#_  (dom/appendChild (elm "child-menu") ))
