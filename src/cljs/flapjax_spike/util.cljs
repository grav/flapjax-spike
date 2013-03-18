(ns flapjax-spike.util
  (:require [goog.dom :as dom]
            [flapjax :as fj]
            [flapjax-spike.edn :as edn]))

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



;; Flapjax utils

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

(defn toElementE [eventE]
  (fj/mapE (fn [event] (.-toElement event)) eventE))

(defn logE
  "Logs events from E to the console with the prefix s. Returns
  an event stream which lets events from E through."
  [s E]
  (fj/mapE
   (fn [e]
     (.log js/console s (pr-str e) (clj->js e))
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

;; REST

(defn rest-request [url]
  {:url url
   :request "get"
   :response "plain"})

(defn post-request [url data]
  {:url url
   :request "post"
   :response "plain"
   :body data})

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
