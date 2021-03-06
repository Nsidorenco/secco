(defproject secco "0.1.0-SNAPSHOT"
  :description "Secco - Minimal language for Symbolic Execution testing"
  :url "https://github.com/Nsidorenco/secco"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"],
                 [instaparse "1.4.8"],
                 [org.clojure/core.match "0.3.0-alpha5"]]
                 
  :profiles {:dev {:dependencies
                   [[rhizome "0.2.9"]]}}
  :scm {:name "git"
        :url "https://github.com/Nsidorenco/secco"}
  :main secco.core)


