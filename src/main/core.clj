(ns main.core
  (:require
    [clara.rules :refer :all]))

;The bartender's question
(defrecord Question [number descr])

;Pub drinks
(defrecord Drink [name conditions])

;User input
(defrecord Answer [question choice])

(defquery get-condition-query [?number]
  [Question (= ?number number) (= ?descr descr)])

(defquery get-drink-query []
  [Drink (= ?name name) (= ?cond conditions)])

(defquery find-answer-query [?condition]
  [Answer (= ?condition question) (= ?iscorrect choice)])

(defquery get-all-answers []
  [Answer (= ?condition question) (= ?iscorrect choice)])

(defn insert-menu-base [session]
  (-> (insert session (->Question 1 "Are you 18 years old"))
      (insert (->Question 2 "Sweet drink"))
      (insert (->Question 3 "Large portion"))
      (insert (->Question 4 "Some ice"))

      ;looks like rules should be in ascending order
      (insert (->Drink "Cola with ice" '(2 3 4)))
      (insert (->Drink "Sweet beer 'Corona Extra' with ice" '(1 2 3 4)))))
      ;TODO add more variants

(defn ask-user [condition session]
  (-> (query session get-condition-query :?number condition)
      (first)
      (get :?descr)
      (str "? [y/n]")
      (println))

  (loop [answer (read-line)]
    (if (and (not= answer "y")
             (not= answer "n"))
      (do (println "Could you please provide correct answer [y/n]")
          (ask-user condition session))
      (= "y" answer))))

(defn save-answer-in-session [session condition is-true]
 ; (println condition)
  (-> session
      (insert (->Answer condition is-true))
      (fire-rules)))

(defn check-condition [condition session]
  (let [query-result
        (query session find-answer-query :?condition condition)]
    (if (empty? query-result)
      (let [user-answer (ask-user condition session)]
        {:session (save-answer-in-session session condition user-answer)
         :result  user-answer})
      {:session session :result (first query-result)})))

(defn check-conditions [condition-list session]
  (let [condition-result (check-condition (first condition-list) session)]
    (if (true? (get condition-result :result))
      (if (empty? (rest condition-list))
        {:session (get condition-result :session) :result true}
        (check-conditions (rest condition-list) (get condition-result :session)))
      {:session (get condition-result :session) :result false})))

(defn check-item-with-conditions [item-list session]
  (let [conditions-result (check-conditions (get (first item-list) :?cond) session)]
    ;(println (get conditions-result :result))
    ;(println item-list)
    (if (true? (get conditions-result :result))

      (first item-list)

      (if (empty? (rest item-list))
        :not-found
        (check-item-with-conditions (rest item-list) (get conditions-result :session))))))

(defn examine [session]
  (let [exam-result
        (-> (query session get-drink-query)
            (check-item-with-conditions session))]
    (if (= :not-found exam-result)
      (println "Such a drink has run out. Come in tomorrow")
      (println "Here's a drink for you: " (get exam-result :?name)))))

(defn pub []
  (-> (mk-session [get-drink-query
                   get-condition-query
                   find-answer-query
                   get-all-answers] :cache false)
      (insert-menu-base)
      (fire-rules)
      (examine))
  nil)

