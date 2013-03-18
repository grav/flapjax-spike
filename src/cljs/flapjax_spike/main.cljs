(ns flapjax-spike.main
  (:require [flapjax :as fj]
            [goog.dom.classes :as classes]
            [goog.dom :as dom]
            [cljs.reader :as reader]
            [flapjax-spike.util :as util]
            [flapjax-spike.edn :as edn]))

;; rest

(defn breast-feed-url [child]
  (str "/rest/" child "/breast-feed"))

(defn nappy-change-url [child]
  (str "/rest/" child "/nappy-change"))

(defn breast-feed-request [child]
  (util/rest-request (breast-feed-url child)))

(defn nappy-change-request [child]
  (util/rest-request (nappy-change-url child)))

(defn breast-feed-post-request [child data]
  (util/post-request (breast-feed-url child) data))

(defn nappy-change-post-request [child data]
  (util/post-request (nappy-change-url child) data))

(defn children-request []
  (util/rest-request "/rest/children"))

;; switching

(defn switch-activity [main activity breast-feed nappy-change]
  (when (= :counting main)
    (case activity
      :breast-feed breast-feed
      :nappy-change nappy-change)))

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
                     (get meta :side) " side.")
                "No breastfeeding done. Kid might be hungry.")]
    (dom/createDom "span" nil value)))

(defn nappy-change-dom [{:keys [meta count timestamp] :as data}]
  (let [value (if data
                (str "(_|_) Nappy-changed "
                     count " times. Changed at"
                     timestamp " by  "
                     (get meta :who) ".")
                "No nappy-changing done. Kid might be smelly.")]
    (dom/createDom "span" nil value)))

(defn dynamicDomB [domE]
  (fj/startsWith domE (dom/createDom "span" nil "Loading ...")))

(defn serverModelFromE
  "Retrieves the current server model based on the events from E.
  request-fn is assumed to take values of E and returns a request
  as used by restE. Ignores nil values of E."
  [E request-fn]
  (->> E
       util/removeNilE
       (fj/mapE request-fn)
       util/restE))

(defn serverModelFromB
  "Retrieves the current server model based on the value of B.
  request-fn is assumed to take values of B and returns a request
  as used by restE. The initial value of the behaviour is nil.
  Ignores nil values of B."
  [B request-fn]
  (fj/startsWith
   (-> B
       fj/changes
       (serverModelFromE request-fn))
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


;; init

(defn ^:export init
  []
  (let [childrenE (util/restE (fj/oneE (children-request)))
        switchChildE (fj/receiverE)
        currentChildB (fj/startsWith switchChildE nil)

        serverModelUpdatedE (fj/receiverE)

        breastFeedB (fj/startsWith
                     (serverModelFromE
                      (util/changesBOrFireE currentChildB serverModelUpdatedE)
                      breast-feed-request)
                     nil)
        nappyChangeB (fj/startsWith
                     (serverModelFromE
                      (util/changesBOrFireE currentChildB serverModelUpdatedE)
                      nappy-change-request)
                     nil)

        nextBreastFeedRequestB (fj/liftB breast-feed-post-request currentChildB {:side "left"})
        nextNappyChangeRequestB (fj/liftB nappy-change-post-request currentChildB {:who "Mikkel"})

        breastFeedDomB (fj/liftB breast-feed-dom breastFeedB)
        nappyChangeDomB (fj/liftB nappy-change-dom nappyChangeB)

        activity-elms (fj/getElementsByClass "menu-item" (util/elm "activity-menu"))
        activityE (->> activity-elms
                       (map fj/clicksE)
                       (apply fj/mergeE)
                       (fj/mapE (fn [event]
                                  (let [elm (.-toElement event)
                                        id (.-id elm)]
                                    (id activity-map)))))
        activityB (fj/startsWith activityE :breast-feed)

        contentDom (fj/liftB switch-activity :counting activityB breastFeedDomB nappyChangeDomB)

        breastFeedClickE (fj/clicksE "breast-feed-action")
        nappyChangeClickE (fj/clicksE "nappy-change-action")

        ;; stream for feeding events as restE maps
        responseFeedE (-> (util/BonE nextBreastFeedRequestB breastFeedClickE)
                          util/restE)
        responseChangeE (-> (util/BonE nextNappyChangeRequestB nappyChangeClickE)
                            util/restE)]

    ;; setting up children menu
    (fj/mapE #(set-children-menu % switchChildE) childrenE)

    ;; insert content
    (fj/insertDomB contentDom "content" "beginning")
    (fj/insertDomB
     (fj/liftB #(if % % "No child selected") currentChildB)
     "content" "beginning")

    ;; wiring up the response event streams to serverModelUpdatedE
    (util/forwardEvents responseFeedE serverModelUpdatedE)
    (util/forwardEvents responseChangeE serverModelUpdatedE)

    ;; Setting active class

    (doseq [elm (fj/getElementsByClass
                 "menu-item"
                 (util/elm "activity-menu"))]
      (fj/insertValueB
       (fj/liftB
        #(active-class % (util/elm-id elm))
        activityB)
       elm "className"))))
