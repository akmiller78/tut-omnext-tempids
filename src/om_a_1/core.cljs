(ns om-a-1.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

(def init-data
  {:users [{:db/id 1 :user/first-name "Adam" :user/last-name "Miller"}
           {:db/id 2 :user/first-name "Joe" :user/last-name "Schmoe"}]})

;;============================================
;; Components
;;============================================

(defui AddPerson
  Object
  (render [this]
    (let [{:keys [add-fn]} (om/get-computed this)
          clear-fn #(om/update-state! this assoc :first-name "" :last-name "")]
      (dom/div nil
        (dom/input #js {:type "text"
                        :placeholder "Firstname"
                        :value       (om/get-state this :first-name)
                        :onChange    #(om/update-state! this assoc :first-name (.. % -target -value))})
        (dom/input #js {:type "text"
                        :placeholder "Lastname"
                        :value       (om/get-state this :last-name)
                        :onChange    #(om/update-state! this assoc :last-name (.. % -target -value))})
        (dom/button #js {:onClick #(let [{:keys [first-name last-name]} (om/get-state this)]
                                    (add-fn first-name last-name)
                                    (clear-fn))} "Add")))))

(def add-person (om/factory AddPerson))

(defui Person
  static om/Ident
  (ident [this props]
    [:person/by-id (:db/id props)])

  static om/IQuery
  (query [this]
    [:db/id :user/first-name :user/last-name])

  Object
  (render [this]
    (let [{:keys [:user/first-name :user/last-name]} (om/props this)]
      (dom/li nil (str first-name " " last-name)))))

(def person (om/factory Person {:keyfn :db/id}))

(defui RootView
  static om/IQuery
  (query [this]
    (let [subquery (om/get-query Person)]
      `[{:users ~subquery}]))

  Object
  (render [this]
    (let [{:keys [users]} (om/props this)]
      (dom/div nil
        (dom/h1 nil "Users")
        (apply dom/ul nil
          (map person users))
        (add-person (om/computed {}
                      {:add-fn (fn [first-name last-name]
                                 (om/transact! this `[(users/create {:db/id ~(om/tempid)
                                                                     :user/first-name ~first-name
                                                                     :user/last-name ~last-name})]))}))))))

;;============================================
;; Reads
;;============================================

(defmulti readf om/dispatch)

(defn- get-people [state key]
  (let [st @state]
    (into [] (map #(get-in st %)) (get st key))))

(defmethod readf :users
  [{:keys [state]} key _]
  {:value (get-people state key)})

;;============================================
;; Mutations
;;============================================
(defmulti mutatef om/dispatch)

(defn- create-user [st u]
  (let [ref [:person/by-id (:db/id u)]]
    (-> st
      (assoc-in ref u)
      (update :users conj ref))))

(defmethod mutatef 'users/create
  [{:keys [state]} _ {:keys [:db/id :user/first-name :user/last-name]}]
  {:remote true
   :action
   (fn []
     (let [user {:db/id id :user/first-name first-name :user/last-name last-name}]
       (swap! state create-user user)))})

;;============================================
;; Remote moq
;;============================================

;; next-id and get-next-id are just here to help generate new "permanent" ids...not needed
;; once remote service that actually persists and generates id is implemeneted.

;; maintain a local state variable for ids (
(def next-id (atom 100))

;; function to generate new id
(defn get-next-id []
  (let [id @next-id]
    (swap! next-id inc)
    id))

(defn remote-fn []
  (fn [{:keys [remote]} callback]
    (let [{[children] :children} (om.next/query->ast remote)
          temp-id (get-in children [:params :db/id])]
      ;; execute callback function with a representation of a data structure that might be returned
      ;; by a remote service that has the mapping of the temp-id to the actual id
      (callback [['user/created {:tempids {[:person/by-id temp-id] [:person/by-id (get-next-id)]}}]]))))

;;============================================
;; Parser/Reconciler
;;============================================
(def parser
  (om/parser {:read readf
              :mutate mutatef}))

(def reconciler
  (om/reconciler
    {:state init-data
     :parser parser
     :send (remote-fn)
     :id-key :db/id}))


;;============================================
;; Initialize root
;;============================================
(om/add-root! reconciler RootView (gdom/getElement "app"))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
