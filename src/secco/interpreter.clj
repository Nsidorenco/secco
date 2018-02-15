(ns secco.interpreter
  )

(def venv
  {})

(defn interpret [node]
  (let [guard (.exp node)]
    (if guard (interpret (get-t node) (interpret (get-f node))))))
