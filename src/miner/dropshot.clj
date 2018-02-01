(ns miner.dropshot
  (:require [etaoin.api :as e]
            [etaoin.keys :as k]))


(def signup-url "http://www.SignUpGenius.com/go/60B0E49A4A628A5F49-edgefield")


;; (def driver (e/chrome))

;;; UNTESTED
(defn qtext
  ([txt] (qtext :* txt))
  ([tag txt]
   {:xpath (str ".//" (name tag) "[contains(text(),'" txt "']")}))

(defn smoke []
  (e/with-chrome {} driver
    (e/go driver signup-url)
    (e/wait-visible driver {:tag :input :type :submit})
    (when (e/has-text? driver "02/15/2018 (Thu.)")
      (let [bingo (e/query driver {:xpath ".//tr[td[span[contains(text(),'02/15/2018 (Thu.)')]]]"})]

        (e/get-element-text-el driver bingo)
        ))))
        

;; (e/query driver {:xpath ".//span[contains(text(),'02/15/2018 (Thu.)')]"})]
;;    (e/query-all driver {:tag :input :type :checkbox})))


