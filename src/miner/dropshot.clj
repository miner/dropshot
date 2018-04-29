(ns miner.dropshot
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [etaoin.api :as e]
            [etaoin.keys :as k]))

;;; Keys for launch-dropshot input
;;; required -- :email, :first, :last, :url
;;         :requests [vector of maps]
;;; optional -- :wait secs, :launch military-time
;;; if :wait is missing or nil, then adaptive-wait is used
;;; if :launch is missing or nil, then lauches immediately.  Also if launch time is recent
;;; (2 hours), then lauch immediately.
;;;
;;; Request map format, required keys:
;;; :date "02/28/2018"
;;; :start 1600
;;; :players ["Names A B" "More C D"]
;;; -- players are grouped into one string per court

;;; Should use spec for input format.

;;; Military time is encoded as an integer.  Don't use leading 0 with military time, because
;;; that's octal notation in Java/Clojure.


;;; UNIMPLEMENTED:
;;; Another possible idea is to wait 5 minutes after there are available courts before you
;;; even try to take one.  Or wait until someone else goes first.  You would have to keep
;;; track of the new dates.  The originals are typically before any requests so take the
;;; first :available snapshot to get a baseline (don't let :launch-time delay first
;;; available check.  Then notice when the new stuff is available.  Then decide to wait for
;;; another delay or wait from someone else to reserve before you grab courts.  Sounds
;;; complicated.


;; This task manager would be good for testing with "virtual" time.  The test can act like a
;; lot of clock time has passed but runs much faster in virtual time.
;; https://github.com/aphyr/tea-time


;;; 02/09/18  09:45 by miner -- need to clean up old and WAS
;; consider :taken  probably want to invert map to {NAME courts} since we only lookup name


(defn third [coll]
  (second (rest coll)))

(defn fourth [coll]
  (nth coll 3 nil))

;; collection of keys can have duplicates, corresponding vals are conj-ed on to a vector
;; sort of a variation on zipmap
(defn zipconj
  ([keys vals] (zipconj keys vals []))
  ([keys vals empty-val]
   (reduce (fn [res kv] (update res (first kv) (fnil conj empty-val) (second kv)))
           {}
           (map list keys vals))))


;; SEM TODO: variable throttling based on time of day.  Every 15 sec from 4am to 10am on Thursday.
;; Every 10 minutes otherwise.  Or give it a hot start option in the data.
;; (.getDayOfWeek (java.time.LocalDateTime/now))
;; #object[java.time.DayOfWeek 0x2133c523 "SUNDAY"]
;; (str (.getDayOfWeek (java.time.LocalDateTime/now)))
;; "SUNDAY"
;; (.getHour (java.time.LocalDateTime/now))
;; (.getMinute (java.time.LocalDateTime/now))





;; timestamp string
;; requires Java 8
(defn now
  ([] (let [nowstr (str (java.time.LocalDateTime/now))]
        (str (subs nowstr 5 10) " " (subs nowstr 11 19))))
  ([msg] (str msg " " (now))))



(def signup-url "http://www.SignUpGenius.com/go/60B0E49A4A628A5F49-edgefield")

(def aiken-url "http://www.signupgenius.com/go/20f0a4aada929a7fa7-court")

(def sample-input
  {:first "Banger" :last "Smash" :email "miner@velisco.com"
   :url signup-url
   :wait-time 10
   :requests  [{:date "02/28/2018" :start 1600 :players [(now "Aaa")]}
               {:date "04/04/2018" :start 1600 :players [(now "Bbb") (now "Ccc")]}]
   })

(def sample-input2
  {:first "Indigo" :last "Pickle" :email "indigo@indigopickleball.com"
   :url signup-url
   :wait-time 15
   :requests  [{:date "02/28/2018" :start 2000 :players [(now "Indigo A")]}
               {:date "02/06/2018" :start 1300 :players [(now "Indigo B") (now "Indigo C")]}]
   })


(def impossible-input
  {:first "Banger" :last "Smash" :email "miner@velisco.com"
   :url signup-url
   :wait-time 10
   :requests  [{:date "02/28/2018" :start 2000 :players [(now "Aaa")]}
               {:date "04/28/2018" :start 2000 :players [(now "Unavailable")]}
               {:date "02/06/2018" :start 1300 :players [(now "Bbb") (now "Ccc")]}]
   })


(def lisa-input
  {:first "Lisa" :last "Miner" :email "lj@lisaminer.com"
   :url aiken-url
   :launch 800
   :requests  [{:date "05/02/2018" :start 1300
                :players ["KShaver BShaver MTewkesbury, MRead"
                          "WMarinaccio, MBeckner, DLilly, RBromley"
                          ]}]
   })




(defn adaptive-wait-secs []
  (if (let [now (java.time.LocalDateTime/now)]
        (and (= (str (.getDayOfWeek now)) "THURSDAY") (<= 6 (long (.getHour now)) 15)))
    (+ 200 (rand-int 100))
    (+ 7000 (rand-int 100))))

(defn adaptive-wait
  ([] (e/wait (adaptive-wait-secs)))
  ([msg] (adaptive-wait msg (adaptive-wait-secs)))
  ([msg wait-secs]
   (let [wait-secs (or wait-secs (adaptive-wait-secs))]
     (println (now) msg "- waiting" wait-secs "secs")
     (flush)
     (e/wait wait-secs))))

;; slice-by like partition-by but expects singleton items satisfying pred, then conjoins
;; following items after it.  If coll doesn't start with a pred item, nil is placed in the
;; first place to mark potential junk.

;; faster and simpler to reduce, but not lazy
(defn slice-by [pred coll]
  (reduce (fn [res x]
            (if (pred x)
              (conj res [x])
              (if (empty? res)
                [[nil x]]
                (conj (pop res) (conj (peek res) x)))))
          []
          coll))

;; nil if not available
(defn button-id [available date start court]
  (get-in available [date start :buttons court]))

  
(defn click-court [driver available date start court]
  (when-let [bid (button-id available date start court)]
    (e/scroll-query driver {:id bid})
    (e/click-visible driver {:id bid})
    (let [checked (e/get-element-attr driver {:id bid} :checked)]
      (cond (= checked "true") true
            (= checked "false") false
            :else false))))

(defn click-submit-and-sign-up [driver]
  (when (e/click-visible driver {:tag :input :value "Submit and Sign Up"})
    true))

(defn signup-button-ids [driver]
  (mapv #(e/get-element-attr-el driver % :id)
        (e/query-all driver {:tag :input :type :checkbox})))

;; (e/query driver {:xpath ".//span[contains(text(),'02/15/2018 (Thu.)')]"})
;;    (e/query-all driver {:tag :input :type :checkbox})

;; Looks like all their layout is in tables, several nested.  First TD is basically all the
;; interesting text -- that would be parsible.  Need to associate check boxes with actual
;; date and time slots.  Decode the top-level TD to assign to order of "Sign Up"
;; checkboxes.  Then fetch all the sign-up buttons elements, depending on natural order to
;; align.  Pick the matching date/time/court to guess the ordinal of the button, get its ID
;; from the element and click it.

(comment

  (doseq [b (e/query-all ddd {:tag :input :name "siid" :type :checkbox})]
    (println b "ID=" (e/get-element-attr-el ddd b :id) "#"))

  (doseq [b (e/query-all ddd {:tag :input :name "siid" :type :checkbox})]
    (let [id (e/get-element-attr-el ddd b :id)] (e/click ddd {:id id})))
  

  (e/get-element-text ddd {:tag :td})
  
"Indigo Pickleball\nEdgefield Pickleball\nLocation: Edgefield Town Gym\nCreated by: \nSM\n Steve Miner\n  Already signed up? You can change your sign up.\n\nDate (mm/dd/yyyy) Time (EST)\nCalendar View\nAvailable Slot\n02/05/2018 (Mon.) 10:00am - 12:00pm  \nCourt 1  \nSign Up \nCourt 2  \nSign Up \n1:00pm - 4:00pm  \nCourt 1  \nES\nElliott Steele\nabc def hij klm\nCourt 2  \nSign Up \n02/06/2018 (Tue.) 10:00am - 12:00pm  \nCourt 1  \nSign Up \nCourt 2  \nSign Up \n1:00pm - 4:00pm  \nCourt 1  \nSign Up \nCourt 2  \nSign Up \n02/08/2018 (Thu.) 1:00pm - 4:00pm  \nCourt 1  \nSign Up \nCourt 2  \nSign Up \n02/13/2018 (Tue.) 10:00am - 12:00pm  \nCourt 1  \nSign Up \nCourt 2  \nSign Up "

)

(defn date-line? [txt]
  (re-matches #"\d{1,2}/\d{1,2}/20\d\d .*" txt))

(defn time-line? [txt]
  (re-matches #"\d{1,2}:\d\d[ap]m.*" txt))

(defn court? [txt]
  (str/starts-with? txt "Court "))

(defn sign-up? [txt]
  (= (str/trim txt) "Sign Up"))

(defn parse-time1 [txt]
  (let [colon (str/index-of txt ":")
        hr (Long/parseLong (subs txt 0 colon))
        min (Long/parseLong (subs txt (inc colon) (+ colon 3)))]
    (if (and (not= hr 12) (str/ends-with? txt "pm"))
      (+ (* (+ hr 12) 100) min)
      (+ (* hr 100) min))))

;; More conventional.  Allows 10:30am, 1pm, 1030, etc.  Two digit time without am/pm is just
;; hr. Assume hrs less than 8 are pm if no am/pm.  Use explicit am/pm to avoid
;; ambiguity. Colon means not military time. Leading "0" is always military time. 4-digit
;; military time (without colon) works.  Result is always long in military time.

(defn parse-time [txt]
  (let [colon (str/index-of txt ":")
        am (str/index-of txt "am")
        pm (str/index-of txt "pm")]
    (cond colon (let [hr (Long/parseLong (subs txt 0 colon))
                      min (Long/parseLong (subs txt (inc colon) (+ colon 3)))]
                  (cond (and am pm) (throw (ex-info "Bad time format" {:bad txt}))
                        (not (or am pm)) (+ min (* 100 (if (< hr 8) (+ hr 12) hr)))
                        pm (+ min (* 100 (if (= hr 12) 12 (+ hr 12))))
                        am (+ min (* 100 (if (= hr 12) 0 hr)))
                        :else (throw (ex-info "Bad time format" {:bad txt}))))
          pm (let [hr (Long/parseLong (str/trim (subs txt 0 pm)))]
               (if (= hr 12) 1200 (* 100 (+ hr 12))))
          am (* 100 (Long/parseLong (str/trim (subs txt 0 am))))
          (str/starts-with? txt "0")      (Long/parseLong (str/trim txt))
          :else (let [hr (Long/parseLong (str/trim txt))]
                  (cond (>= hr 100) hr
                        (< hr 8) (* 100 (+ hr 12))
                        :else (* hr 100))))))

(defn split-date-timestr [dt]
  (let [sp (str/index-of dt " ")
        date (subs dt 0 sp)
        time-sep (str/index-of dt " - ")
        timestr (str/trim (subs dt (- time-sep 7)))]
    [date timestr]))

(defn parse-time-line [txt]
  (let [[start end] (str/split txt #" [-] ")]
    [(parse-time start) (parse-time (str/trim end))]))

(defn parse-court [txt]
  (Long/parseLong (str/trim (subs txt (.length "Court ")))))


(defn make-time-slot [date raw-slot]
  (let [[start end] (parse-time-line (first raw-slot))
        court-parts (slice-by court? (rest raw-slot))
        available (filter #(sign-up? (second %)) court-parts)
        taken (remove #(sign-up? (second %)) court-parts)
        courts (mapv parse-court (map first available))
        taken-courts (map parse-court (map first taken))]
    {:date date
     :start start
     :end end
     :courts courts
     :rosters (zipmap taken-courts (map fourth taken))
     :taken (zipconj (map third taken)
                     taken-courts)}))
    

(defn make-day-slots [raw-day]
  (let [[date timestr] (split-date-timestr (first raw-day))
        raw-slots (slice-by time-line? (cons timestr (rest raw-day)))]
    (map #(make-time-slot date %) raw-slots)))

;; desired slot format
;; {:date date :start start :end end :courts [1 2 3] :assigned {4 "somebody" 5 "other guy"}
;;   :buttons {1 "check23" 2 "check33" 3 "check45"}





(comment
  ;; temporary day structure, gets converted to "available" structure later
  {:date date
   :slots [{:start start
            :end end
            :courts [1 2 3]}
           {:start st2
            :end end2
            :courts [4 5]}]}
  )

;; vbid is a vector of signup-button-ids
(defn patch-buttons [driver vbid available]
  (let [b (volatile! 0)]
    (w/postwalk (fn [form]
                  (if (and (map? form) (contains? form :courts))
                    (let [courts (:courts form)
                          bstart @b]
                      ;; could (dissoc form :courts) but don't bother
                      (assoc form :buttons
                             (zipmap courts (subvec vbid bstart (vswap! b + (count courts))))))
                    form))
                available)))

(defn WAS-available-by-date [available]
  (reduce (fn [res day] (let [d (:date day) slots (:slots day)]
                          (assoc res d (zipmap (map :start slots)
                                               (map #(assoc % :date d) slots)))))
          {}
          available))

;; There are some funny transformations done to convert from HTML order of elements into
;; something that's easier to find the right button.

;; SEM:  could drop day/time with no courts, but that's not really necessary.  And it was
;; hard when I tried.


(defn nest-by-date-time [day-group]
  (reduce-kv (fn [res date slots]
               (assoc res date (zipmap (map :start slots) slots)))
             {}
             day-group))



;; used to get {:tag :td} but sometimes the ad goes first and screws that up.  Looks like I
;; can just get the table text

(defn parse-available-courts [driver sign-up-ids]
  (e/wait-visible driver {:tag :td})
  (let [lines (str/split-lines (e/get-element-text driver {:tag :table :class "SUGtableouter"}))
        raw-days (rest (slice-by date-line? lines))]
    (nest-by-date-time (group-by :date (patch-buttons driver sign-up-ids
                                                      (mapcat make-day-slots raw-days))))))
  

;; Just for testing.  Real code can use the p-a-courts version
(defn parse-available [driver]
  {:time (now)
   :timestamp (System/currentTimeMillis)
   :url (e/get-url driver)
   :available  (parse-available-courts driver   (signup-button-ids driver))} )







(defn preferred-courts [courts ^long cnt]
  (if (<= (count courts) cnt)
    courts
    (let [avail-set (set courts)]
      (or (case cnt
            0 []
            1 (vector (first (filter avail-set [6 4 3 5 2 1])))
            2 (first (filter #(set/subset? % avail-set) [#{6 5} #{4 5} #{6 4} #{4 3} #{6 1}]))
            3 (first (filter #(set/subset? % avail-set) [#{6 5 4} #{5 4 3} #{4 3 2}]))
            4 (first (filter #(set/subset? % avail-set) [#{6 5 4 3} #{6 5 4 1} #{6 5 1 2}
                                                         #{4 5 3 2}]))
            5 (when (set/subset? #{6 5 4 3 2} avail-set) [6 5 4 3 2])
            nil)
          (take cnt (rseq courts))))))
    
;; SEM FIXME:  we're assuming no overlap in requests.  We might be assigning the same
;; court twice if the requests overlap!!!

(defn assign-courts [available requester-name req]
  (if (get-in available [(:date req) (:start req) :taken requester-name])
    (assoc (dissoc req :courts) :assigned requester-name)
    (let [available-courts (get-in available [(:date req) (:start req) :courts])
          cnt (count (:players req))]
      (if (empty? available-courts)
        req
        (assoc req :courts (preferred-courts available-courts cnt))))))


;; SEM TODO: handle split assignments where some courts were available but not others
;; SEM FIXME: doesn't notice that you've already signed up for another court with the same
;; people.  Might happen if you restart with the old request.  It will take another court
;; when it finds an opening.

;; SEM FIXME: should look at actual assignments and try not to sign up twice.
;; SEM FIXME: should confirm actual assignment after it thinks it signed up.  Should recycle
;; if it fails.  That way you don't have to anticipate failure modes.  Just look at the page
;; again and quit only if you see the signups you wanted.



;; SEM: try with-chrome-headless  (for headless Chrome)
;; Didn't work for clicks

;; SEM DONE: combine url requests and request-input into one map


;; SEM FIXME:  not sure about the final remove.  Should instead check that requests are
;; filled by checking reserver name at slots.  Then quit when all requests have matching
;; actual or the slots are taken by someone else.

(defn attempt-signup [request-input]
  (let [request-input (assoc request-input :timestamp (now))]
    (try
      (e/with-chrome {} driver
        (e/go driver (:url request-input))
        (e/wait-visible driver {:tag :input :type :submit})
        (let [sign-up-ids (signup-button-ids driver)]
          (if (empty? sign-up-ids)
            ;; wait if nothing available
            (assoc request-input :wait "empty sign ups")
            (let [available (parse-available-courts driver sign-up-ids)
                  requester-name (str (:first request-input) " " (:last request-input))
                  assignments (mapv (fn [r] (assign-courts available requester-name r))
                                    (:requests request-input))
                  request-output (assoc request-input :requests assignments :available available)]
              (if (every? :assigned assignments)
                request-output
                (if-not (some :courts assignments)
                  (assoc request-output :wait "no assignments")
                  (do
                    (println (now (str (:email request-input) " Attempting assignments")))
                    (doseq [r assignments]
                      (doseq [court (:courts r)]
                        ;; need to double check if court was taken???
                        ;; can't tell until submission
                        (click-court driver available (:date r) (:start r) court)))
                    ;; ready, submit
                    (click-submit-and-sign-up driver)

                    (let [players (mapcat (fn [r] (take (count (:courts r)) (:players r)))
                                          assignments)]
                      (e/wait-visible driver {:name "btnSignUp"})
                      (let [comm-els (e/query-all driver {:tag :input :data-ng-model "i.mycomment"})]
                        (doseq [[el pls] (map vector comm-els players)]
                          (e/fill-el driver el pls))))
                    (e/fill driver {:id :firstname} (:first request-input))
                    (e/fill driver {:id :lastname} (:last request-input))
                    (e/fill driver {:id :email} (:email request-input))
                    (e/click driver {:name "btnSignUp"})
                    request-output)))))))
      (catch Throwable e (assoc  request-input
                                 :wait (ex-info "Exception" {:stacktrace (.getStackTrace e)}
                                                e))))))

(defn dropshot [request-input]
  (pprint request-input)
  (loop [request-input request-input]
    (if (every? :assigned (:requests request-input))
      request-input
      (let [again (attempt-signup request-input)
            wait-msg (:wait again)]
        (when-not (= (:available request-input) (:available again))
          (println (now))
          (println "** available **")
          (pprint (:available again))
          (println)
          (println "requests")
          (pprint (:requests again))
          (println)
          (flush))
        (when wait-msg
          (when-let [exd (ex-data wait-msg)]
            (println "Ex-data")
            (pprint exd))
          (adaptive-wait wait-msg (:wait-time request-input)))
        (recur (dissoc again :wait))))))


;; SEM FIXME -- make this an agent so you can run multiple competing ones and control from
;; the repl
;; create (agent ...) and (send-off ag ....)
;; look at options for error handling and validation

(defn droptest []
  (dropshot sample-input))


(defn one-dropshot [request-input]
  (let [request-input (dissoc request-input :wait :finished)
        result (attempt-signup request-input)]
    (if (and (not (:wait result)) (every? :assigned (:requests result)))
      (assoc result :finished true)
      (update result :requests (fn [rs] (map #(dissoc % :courts) rs))))))


(defn report-status [key ag old-state new-state]
  (when-not (= (:available old-state) (:available new-state))
    (println key (now))
    (println "** available **")
    (pprint (:available new-state))
    (println)
    (println "requests")
    (pprint (:requests new-state))
    (println)
    (flush))
  (when-let [wait-msg (:wait new-state)]
    (when-let [exd (ex-data wait-msg)]
      (println)
      (println "*** Error with ex-data")
      (pprint new-state)
      (flush))))

(defn military-minutes [military]
  (+ (* 60 (quot military 100)) (rem military 100)))

(defn seconds-from-now [military]
  (let [now (java.time.LocalDateTime/now)
        nmin (+ (* (.getHour now) 60) (.getMinute now))
        m (military-minutes military)
        diff (- m nmin)
        adjusted (cond (<= -60 diff 0) 0
                       (neg? diff) (- (+ (* 24 60) m) nmin)
                       :else diff)]
    (if (pos? adjusted)
      (* 60 adjusted)
      0)))
    

(defn wait-until [hrmin]
  (e/wait (seconds-from-now hrmin)))


(defn droploop [ag]
  (when-let [launch-time (:launch @ag)]
    (println (now)  (:email @ag))
    (println "** Waiting to launch until" launch-time)
    (wait-until launch-time))
  (while (and @ag (not (:finished @ag)))
    (send-off ag one-dropshot)
    (await ag)
    (when-let [wait-msg (:wait @ag)]
      (let [msg (str (:email @ag) " - " (if (string? wait-msg) wait-msg "ERROR"))]
        (adaptive-wait msg (:wait-time @ag)))))
  (println "** Finished" (:email @ag) (now))
  (dissoc @ag :available))
  

;; SEM FIXME -- could be a bug with non-chrono order of input requests.  I think sign up
;; comments for player names is base on chrono order, not necessarily the same as request
;; order.  But naturally, real input is in chrono order.  Although, my test requests aren't
;; always.
;; Cheap fix is to make a sort ordering on requests when initializing.

(defn launch-dropshot [request-input]
  (let [request-input (update request-input :requests
                              (fn [rs] (sort-by (juxt :date :start) rs)))]
  (pprint request-input)
  (let [ag (agent request-input :error-mode :continue)]
    (add-watch ag (:email request-input) report-status)
    (future (droploop ag))
    ag)))

(defn hack-request [ag req-input]
  (send ag (fn [_] req-input)))

(defn show [ag]
  (pprint (dissoc @ag :available)))


(defn smoke []
  (pprint (dropshot sample-input)))

                    

;; NOT REALLY NEEDED
(defn extract-date [txt]
  (when-let [sp (str/index-of txt " (")]
    (subs txt 0 sp)))

(defn just-dates [driver]
  (let [lines (str/split-lines (e/get-element-text driver {:tag :td}))]
    (map extract-date (filter date-line? lines))))


  

(comment
  ;; final data structure looks like this.  Key nav ["date" start :buttons court] to get
  ;; button id
  
{"02/05/2018"
 {1000
  {:start 1000,
   :end 1200,
   :courts [1 2],
   :buttons {1 "checkbox432874363", 2 "checkbox432874399"},
   :date "02/05/2018"},
  1300
  {:start 1300,
   :end 1600,
   :courts [2],
   :buttons {2 "checkbox432880143"},
   :date "02/05/2018"}},
 "02/06/2018"
 {1000
  {:start 1000,
   :end 1200,
   :courts [1 2],
   :buttons {1 "checkbox432874365", 2 "checkbox432874401"},
   :date "02/06/2018"},
  1300
  {:start 1300,
   :end 1600,
   :courts [1 2],
   :buttons {1 "checkbox432880144", 2 "checkbox432880145"},
   :date "02/06/2018"}}}

)
  
      
