(ns flapjax-spike.util
  (:require [goog.dom :as dom]
            [flapjax :as fj]))

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
