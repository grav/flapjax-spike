(ns flapjax-spike.routes
  (:use compojure.core
        ring.middleware.edn
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]))

(def state (atom {}))

(defn update-state [state ks meta]
  (assoc-in
   state ks
   {:meta meta
    :count (inc (get-in state (concat ks [:count]) 0))
    :timestamp (System/currentTimeMillis)}))

(defn generate-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/edn"}
   :body (pr-str data)})

(defroutes main-routes

  ;; default activities
  (GET "/rest/activities" []
       (generate-response ["breast-feed" "nappy-change"]))

  (GET "/rest/:child/:activity" [child activity]
       (generate-response
        (get-in @state [child activity])))

  (POST "/rest/:child/:activity" {{:keys [child activity]} :params
                             edn-params :edn-params}
        (do
          (swap! state update-state [child activity]
                 edn-params)
          {:status 200}))

  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> (handler/site main-routes)
      wrap-base-url
      wrap-edn-params))
