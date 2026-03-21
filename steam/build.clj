(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'org.clojars.kovan/steam-mcp)
(def version "0.1.0")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                 :lib lib
                 :version version
                 :basis @basis
                 :src-dirs ["src"]
                 :scm {:url "https://github.com/kovan/steam-mcp"
                        :connection "scm:git:git://github.com/kovan/steam-mcp.git"
                        :developerConnection "scm:git:ssh://git@github.com/kovan/steam-mcp.git"
                        :tag (str "v" version)}
                 :pom-data [[:licenses
                             [:license
                              [:name "MIT"]
                              [:url "https://opensource.org/licenses/MIT"]]]]})
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
           :jar-file jar-file}))

(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact (b/resolve-path jar-file)
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
