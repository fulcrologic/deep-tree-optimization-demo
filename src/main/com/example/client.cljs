(ns com.example.client
  (:require
    [com.fulcrologic.fulcro.algorithms.timbre-support :refer [console-appender prefix-output-fn]]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.fulcro.mutations :as m]
    [taoensso.timbre :as log]
    [com.fulcrologic.fulcro.react.hooks :as hooks]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.dom.events :as evt]))

(defonce app (app/fulcro-app {}))

(defonce node-id (atom 1))
(defn next-node-id [] (swap! node-id inc))
(defn gen-tree [depth branching]
  (if (zero? depth)
    []
    (mapv (fn [n]
            {:node/id       (next-node-id)
             ;; If you open them ALL then the first frame is, of course, horrible, but the interactive is still tolerable
             ;; with 15k nodes on-screen.
             ;:ui/open?      true
             :node/content  (str "node" n "-" depth)
             :node/children (gen-tree (dec depth) branching)}) (range branching))))


(declare ui-deep-node ui-early-node)

(defsc DeepNode [this {:ui/keys   [open?]
                       :node/keys [id content children]} {:keys [depth]}]
  {:query (fn [] '[:ui/open? :node/id :node/content {:node/children ...}])
   :ident :node/id}
  (dom/li {}
    (dom/a {:onClick #(m/toggle!! this :ui/open?)}
      (dom/input {:value    (or content "")
                  :onClick  (fn [evt] (evt/stop-propagation! evt))
                  :onChange (fn [evt] (m/set-string!! this :node/content :event evt))})
      (cond
        open? (str " collapse")
        (seq children) (str " expand")))
    (when (and open? (seq children))
      (dom/ul {}
        (map #(ui-deep-node % {:depth (inc (or depth 4))}) children)))))

(def ui-deep-node (comp/computed-factory DeepNode {:keyfn :node/id}))

(defsc DeepTree [this {:keys [parent-id] :as props}]
  {:use-hooks? true}
  (let [props (hooks/use-component app DeepNode {:initialize? false
                                                 :ident       [:node/id parent-id]})]
    (ui-deep-node props {:depth 3})))

(def ui-deep-tree (comp/computed-factory DeepTree {:keyfn :node/id}))

(defsc EarlyNode [this {:ui/keys   [open?]
                        :node/keys [id content children]} {:keys [depth]}]
  {:query [:ui/open?
           :node/id
           :node/content
           {:node/children 3}]
   :ident :node/id}
  (if (> depth 2)
    (ui-deep-tree {:parent-id id} {:depth (inc depth)})
    (dom/li {}
      (dom/a {:onClick #(m/toggle!! this :ui/open?)}
        (dom/input {:value    (or content "")
                    :onClick  (fn [evt] (evt/stop-propagation! evt))
                    :onChange (fn [evt] (m/set-string!! this :node/content :event evt))})
        (if open?
          (str " collapse")
          (str " expand")))
      (when (and open? (seq children))
        (dom/ul {}
          (map #(ui-early-node % {:depth (inc depth)})
            children))))))

(def ui-early-node (comp/computed-factory EarlyNode {:keyfn :node/id}))

(defsc Root [this {:keys [tree]}]
  {:query         (fn [] [{:tree (comp/get-query EarlyNode)}])
   :initial-state (fn [_]
                    {:tree {:node/id       1
                            :node/content  "ROOT"
                            :node/children (gen-tree 6 5)}})}
  (dom/div
    (dom/h2 {} "Tree")
    (dom/ol {}
      (ui-early-node tree {:depth 1}))))

(defn refresh []
  ;; hot code reload of installed controls
  (log/info "Reinstalling controls")
  (comp/refresh-dynamic-queries! app)
  (app/mount! app Root "app"))

(comment
  (reset! node-id 1)
  (app/schedule-render! app)
  (merge/merge-component! app DeepNode
    {:node/id       1
     :node/content  "ROOT"
     :node/children (gen-tree 7 5)}
    :replace [:tree]))

(defn init []
  (log/merge-config! {:output-fn prefix-output-fn
                      :appenders {:console (console-appender)}})
  (log/info "Starting App")
  (app/mount! app Root "app"))
