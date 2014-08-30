(ns memento.core)

(defn make-fn-name
  [prefix registry-name]
  (-> prefix
      name
      (str "-")
      (str registry-name)
      (str "-handler!")
      symbol))

(defmacro defregistry
  [registry-name]
  (let [registry-atom-name (-> registry-name (str "-registry") symbol)
        register-fn-name   (make-fn-name :register registry-name)
        trigger-fn-name    (make-fn-name :trigger  registry-name)]
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
