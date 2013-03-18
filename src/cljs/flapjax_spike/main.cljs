(ns flapjax-spike.main
  (:require [flapjax :as fj]
            [goog.dom.classes :as classes]
            [goog.dom :as dom]
            [cljs.reader :as reader]
            [flapjax-spike.util :as util]
            [flapjax-spike.edn :as edn]))

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
  {:url url
   :request "get"
   :response "plain"})

(defn post-request [url data]
  {:url url
   :request "post"
   :response "plain"
   :body data})

(defn switch-activity [main activity breast-feed nappy-change]
  (when (= :counting main)
    (case activity
      :breast-feed breast-feed
      :nappy-change nappy-change)))

(defn breast-feed-url [child]
  (str "/rest/" child "/breast-feed"))

(defn nappy-change-url [child]
  (str "/rest/" child "/nappy-change"))

(defn breast-feed-request [child]
  (rest-request (breast-feed-url child)))

(defn nappy-change-request [child]
  (rest-request (nappy-change-url child)))

(defn breast-feed-post-request [child data]
  (post-request (breast-feed-url child) data))

(defn nappy-change-post-request [child data]
  (post-request (nappy-change-url child) data))

(defn children-request []
  (rest-request "/rest/children"))

(defn restE
  "Takes an event stream of maps as defined in the Flapjax api, but body is assumed
  to be edn data."
  [requestE]
  (let [responseE (fj/receiverE)
        callback #(fj/sendEvent responseE %)]
    (fj/mapE
     (fn [req]
       (case (:request req)
         "get" (edn/get (:url req) callback)
         "post" (edn/post (:url req) callback (:body req))))
     requestE)
    responseE))

(defn switch [breastFeedE nappyChangeE]
  (fn [main activity]
    (switch-activity main
                     activity
                     [breastFeedE breast-feed-request]
                     [nappyChangeE nappy-change-request])))

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
                     count "times. Changed at"
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
       utils/removeNilE
       (fj/mapE request-fn)
       restE))

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


;; transforming state

(defn feed
  "Returns the next feed item based on the current data. Returns a fresh
  set of data when data is nil."
  [side]
  {:side side})

(defn change
  "Returns the next nappy change item based on the currenct data. Returns
  a fresh set of data when data is nil."
  [who]
  {:who who})

(defn BonE
  "Returns a new stream which fires the current value of B on events on E."
  [B E]
  (let [newE (fj/receiverE)]
    (fj/mapE (fn [_] (fj/sendEvent newE (fj/valueNow B))) E)
    newE))

(defn changesBOrFireE
  [B E]
  (fj/mergeE (fj/changes B) (BonE B E)))

(defn forwardEvents
  "Sends all events received on sourceE to sinkE. sinkE is assumed to be
  able to receive events via sendEvent."
  [sourceE sinkE]
  (fj/mapE
   (fn [e] (fj/sendEvent sinkE))
   sourceE))

;; init

(defn ^:export init
  []
  (let [childrenE (restE (fj/mapE children-request))
        switchChildE (fj/receiverE)
        currentChildB (fj/startsWith switchChildE nil)

        serverModelUpdatedE (fj/receiverE)

        breastFeedB (fj/startsWith
                     (serverModelFromE
                      (changesBOrFireE currentChildB serverModelUpdatedE)
                      breast-feed-request)
                     nil)
        nappyChangeB (fj/startsWith
                     (serverModelFromE
                      (changesBOrFireE currentChildB serverModelUpdatedE)
                      nappy-change-request)
                     nil)

        nextBreastFeedB (fj/liftB feed "left")
        nextNappyChangeB (fj/liftB change "Mikkel")

        nextBreastFeedRequestB (fj/liftB breast-feed-post-request currentChildB nextBreastFeedB)
        nextNappyChangeRequestB (fj/liftB nappy-change-post-request currentChildB nextNappyChangeB)

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

        breastFeedClickE (fj/clicksE "breast-feed-action")
        nappyChangeClickE (fj/clicksE "nappy-change-action")

        ;; stream for feeding events as restE maps
        responseFeedE (-> (BonE nextBreastFeedRequestB breastFeedClickE)
                          restE)
        responseChangeE (-> (BonE nextNappyChangeRequestB nappyChangeClickE)
                            restE)]

    ;; setting up children menu
    (fj/mapE #(set-children-menu % switchChildE) childrenE)

    ;; insert content
    (fj/insertDomB nappyChageDomB "content" "beginning")
    (fj/insertDomB breastFeedDomB "content" "beginning")
    (fj/insertDomB
     (fj/liftB #(if % % "No child selected") currentChildB)
     "content" "beginning")

    ;; wiring up the response event streams to serverModelUpdatedE
    (forwardEvents responseFeedE serverModelUpdatedE)
    (forwardEvents responseChangeE serverModelUpdatedE)

    ;; Setting active class

    (doseq [elm (fj/getElementsByClass
                 "menu-item"
                 (util/elm "activity-menu"))]
      (fj/insertValueB
       (fj/liftB
        #(active-class % (util/elm-id elm))
        activityB)
       elm "className"))))
