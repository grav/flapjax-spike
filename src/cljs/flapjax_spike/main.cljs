(ns flapjax-spike.main
  (:require [flapjax :as fj]
            [goog.dom.classes :as classes]
            [goog.dom :as dom]))

(defn elm
  "If e is a string it is looked as an id in the dom tree.
  Otherwise it is returned."
  [e]
  (if (string? e)
    (dom/getElement e)
    e))

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

(defn pageB [B]
  (fj/liftB
   (fn [e]
     (let [value (case (.-id e)
                   "counting-link" "Counting Page"
                   "frontpage-link" "Front Page"
                   "No idea?")]
       (dom/createDom "span" nil value))) B))

(def namesB
  (fj/constantB ["olga" "otto"]))

(defn namesMenuB [B]
  (fj/liftB
   (fn [names]
     (let [menu (dom/createDom "div")]
       (doseq [name names]
         (let [item (dom/createDom "span" (clj->js {:class "menu-item"}) name)]
           (dom/appendChild menu item)))
       menu)) B))

(defn ^:export init []
  (let [frontpage-clicksE (fj/clicksE "frontpage-link")
        counting-clicksE (fj/clicksE "counting-link")
        currentActiveB (-> (fj/mergeE frontpage-clicksE counting-clicksE)
                           toElementE
                           (fj/startsWith "frontpage-link"))]
    (doseq [e ["frontpage-link" "counting-link"]]
      (fj/insertValueB (activeClassB currentActiveB e)
                       e
                       "className"))
    (fj/insertDomB (pageB currentActiveB) "content-holder" "beginning")

    (fj/insertDomB (namesMenuB namesB) "name-menu")))
