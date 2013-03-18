(ns flapjax-spike.edn
  (:refer-clojure :exclude [get])
  (:require
   [cljs.reader :as reader]
   [goog.net.XhrIo :as xhr]))

;; edn communication layer

(defn- extract-response [message]
  (reader/read-string
   (. message/target (getResponseText))))

(defn get
  [path callback]
  (xhr/send path
            (fn [response] (callback (extract-response response)))))

(defn post
  [path callback data]
  (xhr/send path
            (fn [response] (callback))
            "POST"
            (pr-str data)
            (clj->js {"Content-Type" "application/edn"})))

(defn put
  [path callback data]
  (xhr/send path
            (fn [_] (callback))
            "PUT"
            (pr-str data)
            (clj->js {"Content-Type" "application/edn"})))

(defn delete
  [path callback]
  (xhr/send path
            (fn [response] (callback))
            "DELETE"))
