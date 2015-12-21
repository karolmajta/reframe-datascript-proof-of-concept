(ns uber.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r]
            [datascript.core :as d]
            [cljs-uuid-utils :as uuid]))

(defn nil-safe-set-add
  [set-or-nil x]
  (let [s (or set-or-nil #{})]
    (conj s x)))

(defn- retain*
  [cache query args state]
  {:pre [(not (.-__key state))]}
  (let [k (vector query args)
        watch-key (gensym)
        listener (get-in cache [k :listener] (atom nil))]
    (set! (.-__key state) watch-key)
    (set! (.-__query state) query)
    (set! (.-__args state) args)
    (reset! state @listener)
    (add-watch listener watch-key (fn [_ _ _ new-state] (reset! state new-state)))
    (-> cache
        (assoc-in [k :listener] listener)
        (update-in [k :ratoms] #(nil-safe-set-add % state)))))

(defn- release*
  [cache query args state]
  ;{:pre [(.-__key state)]}
  (let [k (vector query args)
        watch-key (.-__key state)
        ratom-released (update-in cache [k :ratoms] #(disj % state))
        listener (get-in cache [k :listener])]
    (set! (.-__key state) nil)
    (println "remove watch...")
    (remove-watch listener watch-key)
    (if (empty? (get-in ratom-released [k :ratoms]))
      (dissoc ratom-released k)
      ratom-released)))

(defonce ^:private query-cache (atom {}))

(defn retain
  [query args state]
  (let [k (vector query args)
        no-listener? (nil? (get-in @query-cache [k :listener])) ]
    (swap! query-cache retain* query args state)
    (if no-listener?
      (get-in @query-cache [k :listener])
      nil)))

(defn release
  [state]
  (let [query (.-__query state)
        args (.-__args state)
        k (vector query args)
        listener (get-in @query-cache [k :listener])]
    (swap! query-cache release* query args state)
    (if (empty? (get-in @query-cache [k :ratoms]))
      listener
      nil)))

(defn bind
  ([conn q state & args]
   (let [k (gensym)
         qargs (concat [q @conn] args)
         listen-atom (retain q args state)]
     (when-not (nil? listen-atom)
       (reset! listen-atom (apply d/q qargs))
       (set! (.-__key listen-atom) k)
       (d/listen! conn k (fn [tx-report]
                           (let [recompute-qargs (concat [q (:db-after tx-report)] args)]
                             (println "recomputing..." recompute-qargs)
                             (reset! listen-atom (apply d/q recompute-qargs))))))
     state)))

(defn unbind
  [conn state]
  (let [unlisten-atom (release state)]
    (when-not (nil? unlisten-atom)
      (d/unlisten! conn (.-__key unlisten-atom)))))

;;; Creates a DataScript "connection" (really an atom with the current DB value)
(defonce conn (d/create-conn))c

;;; Add some data
(d/transact! conn [{:db/id -1 :name "Bob" :age 30}
                   {:db/id -2 :name "Sally" :age 25}])

;;; Query to get name and age of peeps in the DB
(def q-peeps '[:find ?n ?a
               :where
               [?e :name ?n]
               [?e :age ?a]])

;; Simple reagent component. Returns a function that performs render
(defn peeps-view
  []
  (let [peeps (reaction @(bind conn q-peeps (r/atom nil)))
        temp (r/atom {:name "" :age ""})]
    (fn []
      [:div
        [:h2 "Peeps!"]
        [:ul
          (map-indexed (fn [i [n a]] ^{:key i} [:li [:span (str "Name: " n " Age: " a)]]) @peeps)]
        [:div
          [:span "Name"][:input {:type "text"
                                 :value (:name @temp)
                                 :on-change #(swap! temp assoc-in [:name] (.. % -target -value))}]]
      [:div
        [:span "Age"][:input {:type "text"
                              :value (:age @temp)
                              :on-change #(swap! temp assoc-in [:age] (.. % -target -value))}]]
        [:button {
          :onClick (fn []
                     (d/transact! conn [{:db/id -1 :name (:name @temp) :age (js/parseInt (:age @temp))}])
                     (reset! temp {:name "" :age ""}))}
          "Add Peep"]])))

;;; Query to find peeps whose age is less than 18
(def q-young
  '[:find ?n
    :in $ ?age
    :where [?e :name ?n]
           [?e :age ?a]
           [(< ?a ?age)]])

(defn younguns-view
  []
  (let [age (r/atom 30)
        users (reaction @(bind conn q-young (r/atom nil) @age))]
    (fn [_]
       [:div
         [:h2 "Young people (under " @age ")"]
         [:div
           [:input {:type :text :value @age :on-change #(reset! age (-> % (.-target) (.-value)))}]]
           [:ul
             (map-indexed (fn [i [n]] ^{:key i} [:li [:span n]]) @users)]])))

;;; Some non-DB state
(defonce state (r/atom {:show-younguns false}))

;;; Uber component, contains/controls stuff and younguns.
(defn uber
  []
  [:div
    [:div [peeps-view]]
    [:div [peeps-view]]
    [:div {:style {:margin-top "20px"}}
      [:input {:type "checkbox"
               :name "younguns"
               :onChange #(swap! state assoc-in [:show-younguns] (.. % -target -checked))}
       "Show Young people"]]
     (when (:show-younguns @state)
       [:div [younguns-view]])
  ])

;;; Initial render
(defn mount-root
  []
  (r/render-component [uber] (.-body js/document))
)

(defn on-reload
  []
  (println "Figwheel reloaded")
  (mount-root))



(enable-console-print!)
(mount-root)