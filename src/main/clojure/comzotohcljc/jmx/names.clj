(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.jmx.names )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(org.apache.commons.lang3 StringUtils))
(import '(javax.management ObjectName))

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def make-objectName ""
  ([rc ^String domain ^String beanName paths]
    (let [ sb (StringBuilder.)
           p1 (seq paths)
           p2 (seq (.paths rc)) ]
      (with-local-vars [ comma false nnum 0 ]
        (.append sb (if (SU/hgl? domain) domain (.domain rc)))
        (.append sb ":")
        (doseq [ fns (if (> (count p1) 0) p1 p2) ]
          (when @comma (.append sb ","))
          (when (< (.indexOf fns \=) 0)
            (do
              (.append sb (String/format "%1$02d" asJObj(@nnum)))
              (.append sb "=")
              (var-set nnum (inc @nnum))))
          (.append sb fns)
          (var-set comma true))
      (when @comma (.append sb ","))
      (.append sb "name=")
      (.append sb
          (if (SU/hgl? beanName) beanName (.beanName rc)))
      (ObjectName. (.toString sb)))))

  ([rc] (make-objectName rc "" "" []))

  ([domain beanName]
    (ObjectName. (str domain ":name=" beanName))) )

(defn inferObjectNameEx [obj ^String domain ^String beanName paths]
  (let [ rc (:jms-resource (meta obj)) ]
    (CU/test-nonil "jms-resource meta-data" rc)
    (make-objectName rc domain beanName paths)))

(defn inferObjectName [obj]
  (let [ rc (:jmx-resource (meta obj)) ]
    (CU/test-nonil "jms-resource meta-data" rc)
    (make-objectName rc)))


(def ^:private names-eof nil)

