(ns flapjax-spike.util
  (:require [goog.dom :as dom]))

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
