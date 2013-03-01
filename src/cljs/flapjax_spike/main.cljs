(ns flapjax-spike.main
  (:require [flapjax :as fj]
            [goog.dom.classes :as classes]
            [goog.dom :as dom]
            [cljs.reader :as reader]
            [flapjax-spike.util :as util]))

(defn toElementE [eventE]
  (fj/mapE (fn [event] (.-toElement event)) eventE))

(defn activeClassB [B e]
  (fj/liftB (fn [target-elm]
              (if (util/e= target-elm e) "active" "")) B))

(defn condB [& pairs]
  (apply fj/condB (map clj->js (partition 2 pairs))))

(def page-map
  {"counting-link" :counting
   "frontpage-link" :frontpage})

(defn isActiveB? [B k]
  (fj/liftB
   (fn [e]
     (let [id (util/elm-id e)
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

(defn active-class [activity elm-id]
  (if (= activity (get activity-map elm-id))
    "active" ""))


(defn frontPageDomB []
  (fj/oneE (dom/createDom "h3" nil "Welcome!")))

(defn breast-feed-dom [{:keys [meta count timestamp] :as data}]
  (let [value (if data
                (str "(.)(.) Breast-fed "
                     count " times. Last one was at "
                     timestamp " on the "
                     (:side meta) " side.")
                "No breastfeeding done. Kid might be hungry.")]
    (dom/createDom "span" nil value)))

(defn nappy-change-dom [{:keys [meta count timestamp] :as data}]
  (let [value (if data
                (str "(_|_) Nappy-changed "
                     count "times. Changed at"
                     timestamp " by  "
                     {:who meta} ".")
                "No nappy-changing done. Kid might be smelly.")]
    (dom/createDom "span" nil value)))

(defn dynamicDomB [domE]
  (fj/startsWith domE (dom/createDom "span" nil "Loading ...")))

(defn logE [E]
  (fj/mapE #(.log js/console (clj->js %)) E))

;; init

(defn ^:export init []

  (let [activityE (->>
                   (apply fj/mergeE
                          (map fj/clicksE
                               (fj/getElementsByClass
                                "menu-item"
                                (util/elm "activity-menu"))))
                   (fj/mapE (fn [event]
                              (let [elm (.-toElement event)
                                    id (.-id elm)]
                                (id activity-map)))))

        childrenE (restE (fj/mapE children-request))
        childMenuE (fj/mapE menu childrenE)


        mainB (fj/constantB :counting)
        activityB (fj/startsWith activityE :breast-feed)

        childB (fj/constantB "olga")

        switchB (liftVectorB mainB activityB)
        breastFeedE (fj/receiverE)
        nappyChangeE (fj/receiverE)
        switch-fn (fn [[main activity]] (switch-activity main activity breastFeedE nappyChangeE))]


    (doseq [elm (fj/getElementsByClass
                 "menu-item"
                 (util/elm "activity-menu"))]
      (fj/insertValueB
       (fj/liftB
        #(active-class % (util/elm-id elm))
        activityB)
       elm "className"))

    (fj/insertDomE
     (fj/mergeE
      (->> nappyChangeE
           (fj/mapE nappy-change-request)
           restE
           (fj/mapE nappy-change-dom))
      (->> breastFeedE
          (fj/mapE breast-feed-request)
          restE
          (fj/mapE breast-feed-dom)))
     "content")

    (->> childrenE
         (fj/mapE fj/clicksE)
         logE)

    (getSwitchE switchB childB switch-fn)

   (fj/insertDomE
     childMenuE
     "child-menu-content")))
