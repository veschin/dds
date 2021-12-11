(ns core.main
  (:require config.core
            clojure.core.async
            clj-http.client
            clojure.string
            discljord.connections
            discljord.events
            discljord.messaging))

(def config (config.core/read-config-file "config.edn"))

(defn modify [opts-map]
  (fn [vals]
    (let [opts-keys (-> vals keys set)
          map-to-merge
          (for [[key func-or-val] (filter (comp opts-keys first) opts-map)
                :let [meta? (-> func-or-val meta seq)]]
            (cond
              meta?
              {key ((modify func-or-val) (key vals))}

              (fn? func-or-val)
              {key (-> vals key func-or-val)}

              :value
              {key func-or-val}))]
      (merge
       vals
       (into {} map-to-merge)))))

(defn flatten-key [map key] (merge (dissoc map key) (key map)))

(defn req []
  (clj-http.client/request
   {:url     (str "https://api.stratz.com/"
                  "api/v1/Player/170793485/matches")
    :headers {"Authorization:" (str "Bearer " (:bearer-token config))}
    :method  :get
    :as      :json
    :coerce  :always}))

(defn ->stat [result]
  (let [->min       #(float (/ % 60))
        match-keys  [:firstBloodTime
                     :durationSeconds :id
                     :players]
        player-keys [:numLastHits :numAssists :experiencePerMinute :numDeaths :imp :isVictory :towerDamage :numKills :level :numDenies :heroDamage :networth :goldPerMinute]
        ->player    (comp #(select-keys % player-keys) first)]
    (-> result
        :body
        first
        (select-keys match-keys)
        ((modify {:durationSeconds ->min
                  :firstBloodTime  ->min
                  :players         ->player}))
        (flatten-key :players))))

(defn ->message [{:keys [numLastHits numAssists experiencePerMinute
                         firstBloodTime numDeaths durationSeconds
                         imp isVictory towerDamage numKills
                         level id numDenies
                         heroDamage networth goldPerMinute]}]
  (let [kda-r (float (/ (+ numKills numAssists) numDeaths))
        clj->msg (fn [vec-]
                   (clojure.string/join "\n"
                    (cons
                     "\n"
                     (for [[key val] (partition-all 2 vec-)]
                       (str (name key)
                            " "
                            (if (vector? val)
                              (apply str val)
                              (str val)))))))]
    (clj->msg
     [:KDA         [numKills "/" numDeaths "/" numAssists " -> " kda-r]
      :LD          [numLastHits "/" numDenies]
      :Level       level
      :Networth    networth
      :Match       id
      :FirstBlood  firstBloodTime
      :Duration    durationSeconds
      :Win?        isVictory
      :Impact      imp
      :Nothingness (neg? imp)
      :TowerDamage towerDamage
      :HeroDamage  heroDamage
      "GPM/XPM"    [goldPerMinute "/" experiencePerMinute]])))

(def state (atom nil))
(def intents #{:guilds :guild-messages})

(defmulti handle-event
  (fn [event-type event-data]
    event-type))

(defmethod handle-event :default
  [event-type event-data])

(defmethod handle-event :message-create
  [event-type {{bot :bot} :author :keys [channel-id content]}]
  (if (= content "!disconnect")
    (discljord.connections/disconnect-bot! (:connection @state))
    (when-not bot
      (discljord.messaging/create-message!
       (:messaging @state) 
       channel-id :content
       (-> (req)
           ->stat
           ->message
           (#(str "```" % "```")))))))

(let [event-ch (clojure.core.async/chan 100)
      token         (:token config)
      connection-ch (discljord.connections/connect-bot! token event-ch :intents intents)
      messaging-ch (discljord.messaging/start-connection! token)
      init-state {:connection connection-ch
                  :event event-ch
                  :messaging messaging-ch}]
  (reset! state init-state)
  (try (discljord.events/message-pump! event-ch handle-event)
       (finally
         (discljord.messaging/stop-connection! messaging-ch)
         (clojure.core.async/close!           event-ch))))


(comment
  (def res (clj-http.client/request
            {:url     (str "https://api.stratz.com/"
                           "api/v1/Player/170793485/matches")
             :headers {"Authorization:" (str "Bearer " (:bearer-token config))}
             :method  :get
             :as      :json
             :coerce  :always}))
  (prn (->message (->stat res)))

  


  ;
  )