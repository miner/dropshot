(ns miner.dropshot
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [clojure.set :as set]
            [etaoin.api :as e]
            [etaoin.keys :as k]))


(def signup-url "http://www.SignUpGenius.com/go/60B0E49A4A628A5F49-edgefield")

(def aiken-url "http://www.signupgenius.com/go/20f0a4aada929a7fa7-court")
    
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
      (let [q {:xpath ".//tr[td[span[contains(text(),'02/15/2018 (Thu.)')]]]"}

            rows (if-let [rowspan (e/get-element-attr driver q :rowspan)]
                     (Long/parseLong rowspan)
                     1)
            
            tr (e/query driver q)
            ;; tr2 (e/query driver {:xpath (str "following-sibling:://" tr)})

            tds (e/query-all driver
                         {:xpath ".//tr[td[span[contains(text(),'02/15/2018 (Thu.)')]]]/td"})]

        (println "Rows = " rows)

        ;;        (println (e/get-element-text-el driver tr))
        ;; (mapv #(e/get-element-text-el driver %) [tr tr2])
        (e/get-element-text-el driver tr)
        ))))


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
  (= txt "Sign Up "))

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

      


(defn parse-date-time [dt]
  (let [sp (str/index-of dt " ")
        day-end (+ (str/index-of dt ".) ") 3)
        time-sep (str/index-of dt " - ")
        date (subs dt 0 sp)
        start (parse-time (str/trim (subs dt day-end time-sep)))
        end (parse-time (str/trim (subs dt (+ time-sep 3))))]
    [date start end]))

(defn parse-time-line [txt]
  (let [[start end] (str/split txt #" [-] ")]
    [(parse-time start) (parse-time (str/trim end))]))

(defn parse-court [txt]
  (Long/parseLong (str/trim (subs txt (.length "Court ")))))

(defn make-day [[dtstrs txts]]
  (assert (not (next dtstrs)))
  (let [[date start end] (parse-date-time (first dtstrs))]
    (loop [txts txts start start end end courts [] ct 0 slots []]
      (if-let [txt (first txts)]
        (cond (court? txt) (recur (rest txts) start end courts (long (parse-court txt)) slots)
              (sign-up? txt) (recur (rest txts) start end (conj courts ct) 0 slots)
              (time-line? txt) (let [[start1 end1] (parse-time-line txt)]
                                 (recur (rest txts) start1 end1 [] 0
                                        (conj slots {:start start :end end :courts
                                                     courts})))
              :else (recur (rest txts) start end courts ct slots))

        ;; finished
        {:date date
         :slots (conj slots {:start start :end end :courts courts})}))))

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

(defn available-by-date [available]
  (reduce (fn [res day] (let [d (:date day) slots (:slots day)]
                          (assoc res d (zipmap (map :start slots)
                                               (map #(assoc % :date d) slots)))))
          {}
          available))

;; There are some funny transformations done to convert from HTML order of elements into
;; something that's easier to find the right button.

;; SEM:  could drop day/time with no courts, but that's not really necessary.  And it was
;; hard when I tried.

;; Just for testing.  Real code can use the p-a-courts version
(defn parse-available [driver]
  (let [lines (str/split-lines (e/get-element-text driver {:tag :td}))
        parts (partition 2 (rest (partition-by (complement date-line?) lines)))]
    (available-by-date (patch-buttons driver (signup-button-ids driver) (map make-day parts)))))


(defn parse-available-courts [driver sign-up-ids]
  (let [lines (str/split-lines (e/get-element-text driver {:tag :td}))
        parts (partition 2 (rest (partition-by (complement date-line?) lines)))]
    (available-by-date (patch-buttons driver sign-up-ids (map make-day parts)))))




(def sample-input
  {:first "Steve" :last "Miner" :email "steve@indigopickleball.com"
   :requests  [["02/05/2018" 1000 "Aaa Bbb Ccc Ddd"]
               ["02/06/2018" 1300 "Aaa Bbb Ccc Ddd" "Eee Fff Ggg Hhh"]]
   })

;; :courts to be added when assigned
;; TODO: partial assignments will have to split players accordingly and report failure.
(def sample-expanded-requests
  [{:date "02/05/2018" :start 1000 :players ["Aaa Bbb Ccc Ddd"] :courts [4]}
   {:date "02/06/2018" :start 1300 :players ["Aaa Bbb Ccc Ddd" "Eee Fff Ggg Hhh"]
    :courts [1 2]}]
   )


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

;; For now, we only assign
(defn assign-courts [available req]
  (if (:courts req)
    req
    (let [available-courts (get-in available [(:date req) (:start req) :courts])
          cnt (count (:players req))]
      (if (empty? available-courts)
        req
        (assoc req :courts (preferred-courts available-courts cnt))))))

        


(defn expand-request [input-request]
  (let [[date start & players] input-request]
    {:date date :start start :players players}))
     
;; emergency
(def ^:dynamic *continue* true)


;; SEM TODO: handle split assignments where some courts were available but not others

(defn dropshot [url request-input timeout-hrs]
  (let [timeout (+ (System/currentTimeMillis) (* 60 60 1000 timeout-hrs))]
    (e/with-chrome {} driver
      (loop [reqs (mapv expand-request (:requests request-input))]
        (when (and *continue* (seq reqs) (< (System/currentTimeMillis) timeout))
          (e/go driver url)
          (e/wait-visible driver {:tag :input :type :submit})        
          (let [sign-up-ids (signup-button-ids driver)]
            (if (empty? sign-up-ids)
              ;; wait if nothing available
              (do (Thread/sleep 150000)
                  (recur reqs))
              (let [available (parse-available-courts driver sign-up-ids)
                    assignments (mapv (fn [r] (assign-courts available r)) reqs)]
                (doseq [r assignments]
                  (doseq [court (:courts r)]
                    ;; need to double check if court was taken???
                    ;; can't tell until submission
                    (click-court driver available (:date r) (:start r) court)))
                ;; ready, submit
                (click-submit-and-sign-up driver)

                (let [players (mapcat (fn [r] (take (count (:courts r)) (:players r))) assignments)]
                  (e/wait-visible driver {:name "btnSignUp"})
                  (let [comm-els (e/query-all driver {:tag :input :data-ng-model "i.mycomment"})]
                    (doseq [[el pls] (map vector comm-els players)]
                      (e/fill-el driver el pls))))
                (e/fill driver {:id :firstname} (:first request-input))
                (e/fill driver {:id :lastname} (:last request-input))
                (e/fill driver {:id :email} (:email request-input))
                (e/click driver {:name "btnSignUp"})
                (recur (remove :courts assignments))))))))))

                    

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

;; nil if not available
(defn button-id [available date start court]
  (get-in available [date start :buttons court]))

  
(defn click-court [driver available date start court]
  (when-let [bid (button-id available date start court)]
    (e/click-visible driver {:id bid})
    (let [checked (e/get-element-attr driver {:id bid} :checked)]
      (cond (= checked "true") true
            (= checked "false") false
            :else false))))

(defn click-submit-and-sign-up [driver]
  (when (e/click-visible driver {:tag :input :value "Submit and Sign Up"})
    true))

  
      
      
;; first page is the court available
;; then submit and go to sign up page
;;
;; comment fields for players (in order of date/time/court slots) from previous checkboxes,
;; no useful ideas, but order seems to work
;;
;;   (e/query-all ddd {:tag :input :data-ng-model "i.mycomment"})
;; (e/fill-el ddd EL "my text for players")
;;
;; other three input fiels by id: firstname, lastname,  email


;; click on name = btnSignUp

(defn smoke-sign [driver]
  (doseq [el (e/query-all driver {:tag :input :data-ng-model "i.mycomment"})]
    (e/fill-el driver el (str "aaa bbb ccc" (last el))))
  (e/fill driver {:id :firstname} "Steve")
  (e/fill driver {:id :lastname} "Miner")
  (e/fill driver {:id :email} "steveminer@gmail.com")
  (e/click driver {:name "btnSignUp"}))

;; players should be vector of strings, each appropriate for a comment field
;; "Steve Tucker Jeeves Lexi"
;; The order is the same as the sign up buttons from the first page

(defn sign-up [driver vplayers]
  (e/wait-visible driver {:name "btnSignUp"})
  (let [comm-els (e/query-all driver {:tag :input :data-ng-model "i.mycomment"})]
    (doseq [[el pls] (map vector comm-els vplayers)]
      (e/fill-el driver el pls)))
  (e/fill driver {:id :firstname} "Steve")
  (e/fill driver {:id :lastname} "Miner")
  (e/fill driver {:id :email} "steveminer@gmail.com")
  (e/click driver {:name "btnSignUp"}))
