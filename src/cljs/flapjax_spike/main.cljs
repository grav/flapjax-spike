(ns flapjax-spike.main
  (:require [flapjax :as fj]
            [goog.dom.classes :as classes]
            [goog.dom :as dom]
            [cljs.reader :as reader]
            [flapjax-spike.util :as util]))

(defn toElementE [eventE]
  (fj/mapE (fn [event] (.-toElement event)) eventE))

(def page-map
  {"counting-link" :counting
   "frontpage-link" :frontpage})

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

(defn post-request [url data]
  (clj->js {:url url
            :request "post"
            :response "plain"
            :body (pr-str data)}))

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
    (switch-activity main
                     activity
                     [breastFeedE breast-feed-request]
                     [nappyChangeE nappy-change-request])))

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
                     (:who meta) ".")
                "No nappy-changing done. Kid might be smelly.")]
    (dom/createDom "span" nil value)))

(defn dynamicDomB [domE]
  (fj/startsWith domE (dom/createDom "span" nil "Loading ...")))

(defn logE
  "Logs events from E to the console with the prefix s. Returns
  an event stream which lets events from E through."
  [s E]
  (fj/mapE
   (fn [e]
     (.log js/console s (pr-str e))
     e)
   E))

(defn logB
  "Logs changes to B to the console with the prefix s."
  [s B]
  (logE s (fj/changes B)))


(defn removeNilE
  "Removes nil events from E."
  [E]
  (fj/filterE E (comp not nil?)))

(defn serverModelFromB
  "Retrieves the current server model based on the value of B.
  request-fn is assumed to take values of B and returns a request
  as used by restE. The initial value of the behaviour is nil.
  Ignores nil values of B."
  [B request-fn]
  (fj/startsWith
   (->> B
        fj/changes
        removeNilE
        (fj/mapE request-fn)
        restE)
   nil))

(defn children-menu-items
  "Creates the children menu items and binds the names of clicked
  children to E. Returns the menu items as a seq."
  [children E]
  (for [child children]
    (let [elm (dom/createDom "a" (clj->js {:href "#"}) child)]
      (fj/mapE
       #(fj/sendEvent E child)
       (fj/clicksE elm))
      elm)))

(defn set-children-menu
  [children E]
  (let [menu (dom/getElement "child-menu-content")]
    (doseq [elm (children-menu-items children E)]
      (dom/appendChild menu elm))))


;; transforming state

(defn feed
  "Returns the next feed item based on the current data. Returns a fresh
  set of data when data is nil."
  [{count :count :as data} side]
  (if data
    (-> data
        (assoc :count (inc count))
        (assoc :timestamp 123)
        (assoc-in [:meta :side] side))
    {:count 1
     :timestamp 456
     :meta {:side side}}))

(defn change
  "Returns the next nappy change item based on the currenct data. Returns
  a fresh set of data when data is nil."
  [{count :count :as data} who]
  (if data
    (-> data
        (assoc :count (inc count))
        (assoc :timestamp 123)
        (assoc-in [:meta :who] who))
    {:count 1
     :timestamp 456
     :meta {:who who}}))

;; init

(defn ^:export init
  []
  (let [childrenE (restE (fj/mapE children-request))
        switchChildE (fj/receiverE)
        currentChildB (fj/startsWith switchChildE nil)

        breastFeedB (serverModelFromB currentChildB breast-feed-request)
        nappyChangeB (serverModelFromB currentChildB nappy-change-request)

        nextBreastFeedB (fj/liftB feed breastFeedB "left")
        nextNappyChangeB (fj/liftB change nappyChangeB "Mikkel")

        breastFeedDomB (fj/liftB breast-feed-dom breastFeedB)
        nappyChageDomB (fj/liftB nappy-change-dom nappyChangeB)

        activity-elms (fj/getElementsByClass "menu-item" (util/elm "activity-menu"))
        activityE (->> activity-elms
                       (map fj/clicksE)
                       (apply fj/mergeE)
                       (fj/mapE (fn [event]
                                  (let [elm (.-toElement event)
                                        id (.-id elm)]
                                    (id activity-map)))))


        mainB (fj/constantB :counting)
        activityB (fj/startsWith activityE :breast-feed)

        childB (fj/constantB "olga")

        switchB (liftVectorB mainB activityB)
        breastFeedE (fj/receiverE)
        nappyChangeE (fj/receiverE)
        switch-fn (fn [[main activity]] (switch-activity
                                        main
                                        activity
                                        breastFeedE
                                        nappyChangeE))

        ]
    ;; setting up children menu
    (fj/mapE #(set-children-menu % switchChildE) childrenE)

    ;; insert content
    (fj/insertDomB breastFeedDomB "content" "beginning")
    (fj/insertDomB nappyChageDomB "content" "beginning")

    ;; update events



    ;; logging
    #_(logB "currentChildB changes:" currentChildB)
    #_(logB "breastFeedB:" breastFeedB)
    #_(logB "nappyChangeB:" nappyChangeB)
    #_(logB "activityB" activityB)

    (logB "nextBreastFeedB" nextBreastFeedB)
    (logB "nextNappyChangeB" nextNappyChangeB)

    ;; old

    (doseq [elm (fj/getElementsByClass
                 "menu-item"
                 (util/elm "activity-menu"))]
      (fj/insertValueB
       (fj/liftB
        #(active-class % (util/elm-id elm))
        activityB)
       elm "className"))

    #_(fj/insertDomE
     (fj/mergeE
      (->> nappyChangeE
           (fj/mapE nappy-change-request)
           restE
           (fj/mapE nappy-change-dom))
      (->> breastFeedE
          (fj/mapE breast-feed-request)
          restE
          (fj/mapE breast-feed-dom)))
     "content")))
