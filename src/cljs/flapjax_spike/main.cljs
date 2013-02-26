(ns flapjax-spike.main
  (:require [flapjax :as fj]
            [goog.dom.classes :as classes]
            [goog.dom :as dom]))

(defn update-menu [E elm]
  (fj/mapE (fn [event]
             (classes/remove elm "active")
             (when (= elm (.-toElement event))
               (classes/add elm "active"))) E))

(defn ^:export init []
  (let [frontpage-clicksE (fj/clicksE "frontpage-link")
        counting-clicksE (fj/clicksE "counting-link")
        activateE (fj/mergeE frontpage-clicksE counting-clicksE)]
    (update-menu activateE (dom/getElement "frontpage-link"))
    (update-menu activateE (dom/getElement "counting-link"))))
