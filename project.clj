(defproject secco "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/Nsidorenco/secco"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"],
                 [instaparse "1.4.8"],
                 [org.clojure/core.match "0.3.0-alpha5"]]
  :profiles {:dev {:dependencies
                   [[rhizome "0.2.9"]]}}
  :scm {:name "git"
        :url "https://github.com/Nsidorenco/secco"}
  :main secco.core)


