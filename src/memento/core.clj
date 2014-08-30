(ns memento.core)

(defmacro defregistry
  [registry-name]
  (let [registry-atom-name (-> registry-name (str "-registry") symbol)
        register-fn-name   (symbol (str "register-" (str registry-name) "-handler!"))
        trigger-fn-name    (symbol (str "trigger-" (str registry-name) "-handler!"))]
    `(do
       ;; define registry:    
       (def ~registry-atom-name (atom {}))

       ;; define register!-fn
       (defn ~register-fn-name
         [handler-name# impl#]
         (swap! ~registry-atom-name assoc handler-name# impl#))
       
       ;; define trigger!-fn
       (defn ~trigger-fn-name
         [handler-name# data#]
         (when-let [handler# (get (deref ~registry-atom-name) handler-name#)]
           (handler# data#))))))
