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

  comzotohcljc.hhh.core.climain )

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.i18n.resources :as LN])
(require '[comzotohcljc.util.process :as PU])
(require '[comzotohcljc.util.meta :as MU])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.files :as FU])
(require '[comzotohcljc.util.ini :as WI])

(use '[comzotohcljc.hhh.impl.exec :only (make-execvisor) ])
(use '[comzotohcljc.hhh.core.constants])
(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.impl.defaults])

(import '(com.zotoh.hohenheim.core
  Versioned Identifiable Hierarchial
  Startable ConfigError))
(import '(com.zotoh.hohenheim.loaders
  AppClassLoader RootClassLoader ExecClassLoader))
(import '(com.zotoh.hohenheim.etc CmdHelpError))
(import '(java.util Locale))
(import '(java.io File))
(import '(org.apache.commons.io FileUtils))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- inizContext ^comzotohcljc.util.core.MuObj [^File baseDir]
  (let [ cfg (File. baseDir ^String DN_CFG)
         f (File. cfg (str "app/" (name K_PROPS)))
         home (.getParentFile cfg) ]
    (precondDir home)
    (precondDir cfg)
    (precondFile f)
    (doto (make-context)
      (.setf! K_BASEDIR home)
      (.setf! K_CFGDIR cfg))))

(defn- setupClassLoader [^comzotohcljc.util.core.MuObj ctx]
  (let [ root (.getf ctx K_ROOT_CZLR)
         cl (ExecClassLoader. root) ]
    (MU/set-cldr cl)
    (.setf! ctx K_EXEC_CZLR cl)
    ctx))

(defn- setupClassLoaderAsRoot [^comzotohcljc.util.core.MuObj ctx]
  (let [ root (RootClassLoader. (MU/get-cldr)) ]
    (.setf! ctx K_ROOT_CZLR root)
    ctx))

(defn- maybeInizLoaders [^comzotohcljc.util.core.MuObj ctx]
  (let [ cz (MU/get-cldr) ]
    (if (instance? ExecClassLoader cz)
      (do
        (.setf! ctx K_ROOT_CZLR (.getParent cz))
        (.setf! ctx K_EXEC_CZLR cz) )
      (setupClassLoader (setupClassLoaderAsRoot ctx)))
    ctx))

(defn- loadConf [^comzotohcljc.util.core.MuObj ctx]
  (let [ ^File home (.getf ctx K_BASEDIR)
         cf (File. home  (str DN_CFG "/app/" (name K_PROPS) ))
        ^comzotohcljc.util.ini.IWin32Conf
         w (WI/parse-inifile cf)
         lg (.toLowerCase ^String (.optString w K_LOCALE K_LANG "en"))
         cn (.toUpperCase ^String (.optString w K_LOCALE K_COUNTRY ""))
         loc (if (SU/hgl? cn) (Locale. lg cn) (Locale. lg)) ]
    (doto ctx
      (.setf! K_PROPS w)
      (.setf! K_L10N loc))) )

(defn- setupResources [^comzotohcljc.util.core.MuObj ctx]
  (let [ rc (LN/get-resource "comzotohcljc.hhh.etc.Resources"
                             (.getf ctx K_LOCALE)) ]
    (.setf! ctx K_RCBUNDLE rc)
    ctx))

(defn- pre-parse [^comzotohcljc.hhh.core.sys.Thingy cli args]
  (let [ bh (File. ^String (first args))
         ctx (inizContext bh) ]
    (info "inside pre-parse()")
    (precondDir (File. bh ^String DN_PATCH))
    (precondDir (File. bh ^String DN_CORE))
    (precondDir (File. bh ^String DN_LIB))
    (.setf! ctx K_CLISH cli)
    (.setCtx! cli ctx)
    ctx))

(defn- start-exec [^comzotohcljc.util.core.MuObj ctx]
  (do
    (info "About to start Hohenheim...")
    (let [ ^Startable exec (.getf ctx K_EXECV) ]
      (.start exec))
    (info "Hohenheim started.")
    ctx))

(defn- primodial [^comzotohcljc.util.core.MuObj ctx]
  (let [ cl (.getf ctx K_EXEC_CZLR)
         cli (.getf ctx K_CLISH)
        ^comzotohcljc.util.ini.IWin32Conf
         wc (.getf ctx K_PROPS)
         cz (.optString wc K_COMPS K_EXECV "") ]
    (CU/test-cond "conf file:exec-visor"
                  (= cz "comzotohcljc.hhh.impl.Execvisor"))
    (info "inside primodial()")
    (let [ ^comzotohcljc.util.core.MuObj execv (make-execvisor cli) ]
      (.setf! ctx K_EXECV execv)
      (synthesize-component execv { :ctx ctx } )
      ctx)))

(defn- enableRemoteShutdown []
  (let [ port (CU/conv-long (System/getProperty "hohenheim.kill.port") 4444) ]
    (info "Enabling remote shutdown...")
    nil))

(defn- stop-cli [^comzotohcljc.util.core.MuObj ctx trigger]
  (let [ ^File pid (.getf ctx K_PIDFILE)
         execv (.getf ctx K_EXECV) ]
    (when-not (nil? pid) (FileUtils/deleteQuietly pid))
    (info "About to stop Hohenheim...")
    (when-not (nil? execv)
      (.stop ^Startable execv))
    (info "Hohenheim stopped.")
    (deliver trigger 911)))

(defn- hookShutdown [^comzotohcljc.util.core.MuObj ctx]
  (let [ cli (.getf ctx K_CLISH)
         trigger (promise) ]
    (.addShutdownHook (Runtime/getRuntime)
          (Thread. (reify Runnable
                      (run [_] (CU/Try! (stop-cli ctx trigger))))))
    (enableRemoteShutdown)
    (debug "Added shutdown hook.")
    [ctx trigger] ))

(defn- writePID [^comzotohcljc.util.core.MuObj ctx]
  (let [ fp (File. ^File (.getf ctx K_BASEDIR) "hohenheim.pid") ]
    (FileUtils/writeStringToFile fp (PU/pid) "utf-8")
    (.setf! ctx K_PIDFILE fp)
    ctx))

(defn- pause-cli [[ ^comzotohcljc.util.core.MuObj ctx trigger]]
  (do
    (print-mutableObj ctx)
    (info "Applications are now running...")
    (deref trigger) ;; pause here
    (info "Applications are shutting down...")
    (PU/safe-wait 5000) ;; give some time for stuff to wind-down.
    (info "Bye.")
    (System/exit 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- make-climain [ & args ]
  (let [ impl (CU/make-mmap) ]
    (reify

      Thingy

        (setCtx! [_ x] (.mm-s impl :ctx x))
        (getCtx [_] (.mm-g impl :ctx))
        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )

      Hierarchial
        (parent [_] nil)

      Versioned
        (version [_] "1.0")

      Identifiable
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

        (stop [_] (.stop ^Startable (.mm-g impl K_EXECV) )))) )


(defn start-main "" [ & args ]
  (do
    (when (< (count args) 1)
      (throw (CmdHelpError. "Hohenheim Home not defined.")))
    (info "set hohenheim-home= " (first args))
    (let [ ^Startable cm  (apply make-climain args) ]
      (.start cm))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private climain-eof nil)

