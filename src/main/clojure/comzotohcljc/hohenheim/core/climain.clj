;;
;; COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
;;
;; THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
;; MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE
;; VERSION 2.0 (THE "LICENSE").
;;
;; THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL
;; BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
;;
;; SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
;; AND LIMITATIONS UNDER THE LICENSE.
;;
;; You should have received a copy of the Apache License
;; along with this distribution; if not you may obtain a copy of the
;; License at
;; http://www.apache.org/licenses/LICENSE-2.0
;;

(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.hohenheim.core.climain )

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.i18n.resources :as LN])
(require '[comzotohcljc.util.process :as PU])
(require '[comzotohcljc.util.meta :as MU])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.files :as FU])
(require '[comzotohcljc.util.ini :as WI])

(use '[comzotohcljc.hohenheim.impl.execvisor :only (make-execvisor) ])
(use '[comzotohcljc.hohenheim.core.constants])
(use '[comzotohcljc.hohenheim.impl.defaults])

(import '(com.zotoh.hohenheim.core
  Startable ConfigError
  AppClassLoader RootClassLoader ExecClassLoader))
(import '(com.zotoh.hohenheim.etc CmdHelpError))
(import '(java.util Locale))
(import '(java.io File))
(import '(org.apache.commons.io FileUtils))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- inizContext [baseDir]
  (let [ cfg (File. baseDir DN_CFG)
         f (File. cfg (str "app/" (name K_PROPS)))
         home (.getParentFile cfg) ]
    (precondDir home)
    (precondDir cfg)
    (precondFile f)
    (doto (make-context)
      (.setf! K_BASEDIR home)
      (.setf! K_CFGDIR cfg))))

(defn- setupClassLoader [ctx]
  (let [ root (.getf ctx K_ROOT_CZLR)
         cl (ExecClassLoader. root) ]
    (MU/set-cldr cl)
    (.setf! ctx K_EXEC_CZLR cl)
    ctx))

(defn- setupClassLoaderAsRoot [ctx]
  (let [ root (RootClassLoader. (MU/get-cldr)) ]
    (.setf! ctx K_ROOT_CZLR root)
    ctx))

(defn- maybeInizLoaders [ctx]
  (let [ cz (MU/get-cldr) ]
    (if (instance? ExecClassLoader cz)
      (-> ctx
        (.setf! K_ROOT_CZLR (.getParent cz))
        (.setf! K_EXEC_CZLR cz))
      (setupClassLoader (setupClassLoaderAsRoot ctx)))
    ctx))

(defn- loadConf [ctx]
  (let [ home (.getf ctx K_BASEDIR)
         cf (File. home  (str DN_CFG "/app/" (name K_PROPS) ))
         w (WI/parse-inifile cf)
         lg (.toLowerCase (.optString w K_LOCALE K_LANG "en"))
         cn (.toUpperCase (.optString w K_LOCALE K_COUNTRY ""))
         loc (if (SU/hgl? cn) (Locale. lg cn) (Locale. lg)) ]
    (doto ctx
      (.setf! K_PROPS w)
      (.setf! K_L10N loc))) )

(defn- setupResources [ctx]
  (let [ rc (LN/get-resource "comzotohcljc.hohenheim.etc.Resources"
                             (.getf ctx K_LOCALE)) ]
    (.setf! ctx K_RCBUNDLE rc)
    ctx))

(defn- pre-parse [cli args]
  (let [ bh (File. (first args))
         ctx (inizContext bh) ]
    (precondDir (File. bh DN_PATCH))
    (precondDir (File. bh DN_CORE))
    (precondDir (File. bh DN_LIB))
    (.setf ctx K_CLISH cli)
    (.setCtx! cli ctx)
    ctx))

(defn- start-exec [ctx]
  (do
    (info "About to start Hohenheim...")
    (-> (.getf ctx K_EXECV) (.start))
    (info "Hohenheim started.")
    ctx))

(defn- primodial [ctx]
  (let [ cl (.getf ctx K_EXEC_CZLR)
         cli (.getf ctx K_CLISH)
         wc (.getf ctx K_PROPS)
         cz (.optString wc K_COMPS K_EXECV "") ]
    (CU/test-cond "conf file:exec-visor"
                  (= cz "comzotohcljc.hohenheim.impl.Execvisor"))
    (let [ execv (make-execvisor cli) ]
      (.setf! ctx K_EXECV execv)
      (synthesize-component execv { :ctx ctx } )
      ctx)))

(defn- enableRemoteShutdown []
  (let [ port (CU/conv-long (System/getProperty "hohenheim.kill.port") 4444) ]
    nil))

(defn- stop-cli [ctx trigger]
  (let [ pid (.getf ctx K_PIDFILE)
         execv (.getf ctx K_EXECV) ]
    (when-not (nil? pid) (FileUtils/deleteQuietly pid))
    (info "About to stop Hohenheim...")
    (when-not (nil? execv) (.stop execv))
    (info "Hohenheim stopped.")
    (deliver trigger 911)))

(defn- hookShutdown [ctx]
  (let [ cli (.getf ctx K_CLISH)
         trigger (promise) ]
    (.addShutdownHook (Runtime/getRuntime)
          (Thread. (reify Runnable
                      (run [_] (CU/TryC (stop-cli ctx trigger))))))
    (enableRemoteShutdown)
    (debug "Added shutdown hook.")
    [ctx trigger] ))

(defn- writePID [ctx]
  (let [ fp (File. (.getf ctx K_BASEDIR) "hohenheim.pid") ]
    (FileUtils/writeStringToFile fp (PU/pid) "utf-8")
    (.setf! ctx K_PIDFILE fp)))

(defn- pause-cli [[ctx trigger]]
  (do
    (print-mutableObj ctx)
    (info "Applications are now running...")
    (deref trigger) ;; pause here
    (info "Applications are shutting down...")
    (PU/safe-wait 5000) ;; give some time for stuff to wind-down.
    (info "Bye.")
    (System/exit 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defn- make-climain [ args ]
  (let [ impl (CU/make-mmap) ]
    (reify

      Component

        (setCtx! [_ x] (.mm-s impl :ctx x))
        (getCtx [_] (.mm-g impl :ctx))
        (parent [_] nil)
        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )
        (version [_] "1.0")
        (id [_] K_CLISH)

      Startable

        (start [this]
          (-> (pre-parse this args)
            (maybeInizLoaders)
            (loadConf)
            (setupResources )
            (primodial)
            (start-exec)
            (writePID)
            (hookShutdown)
            (pause-cli)) )

        (stop [_] (.stop (.mm-g impl K_EXECV) )))) )


(defn start-main "" [ & args ]
  (do
    (when (< (count args) 1)
      (throw (CmdHelpError. "Hohenheim Home not defined.")))
    (info "set hohenheim-home= " (first args))
    (-> (make-climain args) (.start)) ))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private climain-eof nil)

