(ns memento.core)

(defn make-fn-name
  [prefix registry-name]
  (-> prefix
      name
      (str "-" registry-name "-handler!")
      symbol))

(defmacro defregistry
  [registry-name & [{:keys [register-fn-alias trigger-fn-alias] :as opts}]]
  (let [registry-atom-name       (-> registry-name
                                     (str "-registry")
                                     symbol)
        make-defregistry-fn-name (fn [prefix alias]
                                   (or alias (make-fn-name prefix registry-name)))
        register-fn-name         (make-defregistry-fn-name :register register-fn-alias)
        trigger-fn-name          (make-defregistry-fn-name :trigger trigger-fn-alias)]
    `(do
       ;; define registry
       (def ~registry-atom-name (atom {}))

       ;; define register!-fn
       (defn ~register-fn-name
         [handler-name# impl#]
         (swap! ~registry-atom-name assoc handler-name# impl#))
       
       ;; define trigger!-fn
       (defn ~trigger-fn-name
         [handler-name# & [data#]]
         (when-let [handler# (get (deref ~registry-atom-name) handler-name#)]
           (if data#
             (handler# data#)
             (handler#)))))))
