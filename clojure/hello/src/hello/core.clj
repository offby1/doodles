(ns hello.core)

(def visitors (ref #{}))

(defn hello
  "Returns a hello message, calling you by username.
          Knows if you've been naughty or nice."
  [username]
  (dosync
   (let [past-visitor (@visitors username)]
     (if past-visitor
       (str "Welcome back, wanker " username)
       (do
         (alter visitors conj username)
         (str "Hello, " username))))))
