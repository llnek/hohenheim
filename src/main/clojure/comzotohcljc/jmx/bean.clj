(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.jmx.bean )


(use '[clojure.tools.logging :only (info warn error debug)])


(import '(org.apache.commons.lang3 StringUtils))
(import '(java.lang.reflect Field Method))
(import '(java.util Arrays))
(import '(javax.management
  Attribute
  AttributeList
  AttributeNotFoundException
  DynamicMBean
  MBeanAttributeInfo
  MBeanException
  MBeanInfo
  MBeanOperationInfo
  MBeanParameterInfo
  ReflectionException))

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.meta :as MU])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype NameParams [_name _pms]
  Object
  (hashCode [_]
    (let [ hv (* 31  (+ 31 (.hashCode _name))) ]
      (if (empty? _pms)
        hv
        (+ hv (Arrays/hashCode (into-array Object _pms))) )))

  (equals [this obj]
    (if (or (nil? obj)
            (not= (.getClass obj) (.getClass this)))
      false
      (let [ ^comzotohcljc.jmx.bean.NameParams other obj]
        (if (not= _name (._name other))
          false
          (Arrays/equals (into-array Object _pms)
                         (into-array Object (._pms other))))))))

(defprotocol ^:private BFieldInfo
  ""
  (isGetter [_] )
  (isSetter [_] )
  (field [_] ))

(defn- mkBFieldInfo [^Field fld getr setr]
  (reify BFieldInfo
    (isGetter [_] getr)
    (isSetter [_] setr)
    (field [_] fld)))


(defprotocol ^:private BPropInfo
  ""
  (getType [_] )
  (desc [_] )
  (getter [_] )
  (getName [_] )
  (setter [_] )
  (setSetter [_ m] )
  (setGetter [_ m] )
  (isQuery [_] ))

(defn- mkBPropInfo [^String prop ^String descn ^Method getr ^Method setr]
  (let [ impl (CU/make-mmap) ]
    (.mm-s impl :type nil)
    (.mm-s impl :getr getr)
    (.mm-s impl :setr setr)
    (reify BPropInfo
      (getType [this]
        (let [ ^Method g (getter this) ^Method s (setter this) ]
          (if (CU/notnil? g)
            (.getReturnType g)
            (if (nil? s)
              nil
              (let [ ps (.getParameterTypes s) ]
                (if (== 1 (count ps)) (first ps) nil))))))
      (desc [_] descn)
      (getter [_] (.mm-g impl :getr))
      (getName [_] prop)
      (setter [_] (.mm-g impl :setr))
      (setSetter [_ m] (.mm-s impl :setr m))
      (setGetter [_ m] (.mm-s impl :getr m))
      (isQuery [this]
        (let [ ^Method g (getter this) ]
          (if (nil? g)
            false
            (and (-> g (.getName)(.startsWith "is"))
                 (MU/is-boolean? (getType this)))))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- unknownError [attr]
  (AttributeNotFoundException. (str "Unknown property " attr)))

(defn- beanError [msg]
  (MBeanException. (Exception. msg)))

(defn- badArg [msg]
  (IllegalArgumentException. msg))

(defn- assertArgs [mtd ptypes n]
  (when (not (== n (count ptypes)))
    (throw badArg (str "\"" mtd "\" needs " n "args.") )))

(defn- maybeGetPropName ^String [^String mn]
  (let [ pos (cond
               (or (.startsWith mn "get")
                   (.startsWith mn "set"))
               3
               (.startsWith mn "is")
               2
               :else
               -1) ]
    (if (< pos 0)
      ""
      (str (Character/toLowerCase (.charAt mn pos)) (.substring mn (+ pos 1))))))

(defn- mkParameterInfo [^Method mtd]
  (with-local-vars [ ptypes (.getParameterTypes mtd)
                     ctr 1
                     rc (transient []) ]
    (doseq [ t (seq @ptypes) ]
      (conj! @rc (MBeanParameterInfo. (str "p" @ctr) (.getName t) ""))
      (var-set ctr (inc @ctr)))
    (persistent! rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handleProps [mtds]
  (with-local-vars [ props (transient {}) ]
    (doseq [ ^Method mtd (seq mtds) ]
      (let [ mn (.getName mtd)
             ptypes (.getParameterTypes mtd)
             rtype (.getReturnType mtd)
             pname (maybeGetPropName mtd)
            ^comzotohcljc.jmx.bean.BPropInfo
             methodInfo (get props pname) ]
        (cond
          (or (.startsWith mn "is")
              (.startsWith mn "get"))
          (do
            (assertArgs mn ptypes 0)
            (if (nil? methodInfo)
              (assoc! props pname
                    (mkBPropInfo pname "" mtd nil))
              (.setGetter methodInfo mtd)))

          (.startsWith mn "set")
          (do
            (assertArgs mn ptypes 1)
            (if (nil? methodInfo)
              (assoc! props pname
                    (mkBPropInfo pname "" nil mtd))
              (.setSetter methodInfo mtd)))
          :else nil)))
    (let [ rc (persistent! props)
           ba (transient []) ]
      (doseq [ [k v] (seq rc) ]
        (conj! ba
               (MBeanAttributeInfo.
                 (.getName v)
                 ;; could NPE here if type is nil!
                 (-> v (.getType)(.getName))
                 (.desc v)
                 (CU/notnil? (.getter v))
                 (CU/notnil? (.setter v))
                 (.isQuery v))))
      [ (persistent! ba) rc ] )) )

(defn- handleFlds [^Class cz]
  (with-local-vars [ flds (transient {})
                     rc (transient []) ]
    (doseq [ ^Field field (seq (.getDeclaredFields cz)) ]
      (let [ fnm (.getName field) ]
        (when (.isAccessible field)
          (assoc! flds fnm (mkBFieldInfo field true true))
          (conj! rc
                 (MBeanAttributeInfo.
                   fnm
                   (-> field (.getType)(.getName) )
                   (str fnm " attribute")
                   true
                   true
                   (and (.startsWith fnm "is")
                        (MU/is-boolean? (.getType field))))))))
    [ (persistent! rc)(persistent! flds) ] ))

(defn- handleMethods [mtds]
  (with-local-vars [ metds (transient {}) rc (transient []) ]
    (doseq [ ^Method m (seq mtds) ]
      (let [ mn (.getName m) ]
        (when-not (or (.startsWith mn "is")
                      (.startsWith mn "get")
                      (.startsWith mn "set"))
          (let [ ptypes (.getParameterTypes m)
                 pns (map (fn [^Class c] (.getName c)) (seq ptypes))
                 nameParams (NameParams. mn pns)
                 pmInfos (mkParameterInfo m) ]
            (assoc! metds nameParams m)
            (conj! rc (MBeanOperationInfo.
                        mn
                        (str mn " attribute")
                        (into-array MBeanParameterInfo pmInfos)
                        (-> m (.getReturnType)(.getName))
                        MBeanOperationInfo/ACTION_INFO ))))))
    [ (persistent! rc) (persistent! metds)] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-jmxBean [^Object obj]
  (let [ impl (CU/make-mmap)
         cz (.getClass obj)
         ms (.getMethods cz)
         [ps propsMap] (handleProps ms)
         [fs fldsMap] (handleFlds cz)
         [ms mtdsMap] (handleMethods ms)
         bi (MBeanInfo. (.getName cz)
                        (str "Information about " cz)
                        (into-array MBeanAttributeInfo (conj ps fs))
                        nil
                        (into-array MBeanOperationInfo ms)
                        nil) ]
    (reify DynamicMBean
      (getMBeanInfo [_] bi)
      (getAttribute [_ attrName]
        (let [ ^comzotohcljc.jmx.bean.BPropInfo
               prop (get propsMap attrName)
               ^comzotohcljc.jmx.bean.BFieldInfo
               fld (get fldsMap attrName) ]
          (cond
            (nil? prop)
            (do
              (when (or (nil? fld)
                      (not (.isGetter fld)))
                (throw (unknownError attrName)))
              (let [ ^Field f (.field fld) ]
                (.get f obj)))

            (nil? (.getter prop))
            (throw (unknownError attrName))

            :else
            (let [ ^Field f (.getter prop) ]
              (.invoke f obj)))))

      (getAttributes [this attrNames]
        (let [ rcl (AttributeList.) ]
          (doseq [ ^String nm (seq attrNames) ]
            (try
              (.add rcl (Attribute. nm (.getAttribute this nm)))
              (catch Throwable e#
                (error e# "")
                (.add rcl (Attribute. nm (.getMessage e#))))))
          rcl))

      (setAttribute [_ attr]
        (let [ v (.getValue ^Attribute attr)
               an (.getName ^Attribute attr)
               ^comzotohcljc.jmx.bean.BPropInfo
               prop (get propsMap an)
               ^comzotohcljc.jmx.bean.BFieldInfo
               fld (get fldsMap an) ]
          (cond
            (nil? prop)
            (do
              (when (or (nil? fld)
                        (not (.isSetter fld)))
                (throw (unknownError an)))
              (let [ ^Field f (.field fld) ]
                (.set f obj v)))

            (nil? (.setter prop))
            (throw unknownError an)

            :else
            (let [ ^Field f (.setter prop) ]
              (.invoke f obj v)))))

      (setAttributes [this attrs]
        (let [ rcl (AttributeList. (count attrs)) ]
          (doseq [ ^Attribute a (seq attrs) ]
            (let [ nn (.getName a) ]
              (try
                (.setAttribute this a)
                (.add rcl (Attribute. nn (.getAttribute this nn)))
                (catch Throwable e#
                  (error e# "")
                  (.add rcl (Attribute. nn (.getMessage e#)))))) )
          rcl))

      (invoke [_ opName params sig]
        (let [ mtd (get mtdsMap (NameParams. opName sig)) ]
          (when (nil? mtd)
            (throw (beanError (str "Unknown operation \"" opName "\""))))
          (if (empty? params)
            (.invoke mtd obj)
            (.invoke mtd obj params))))

      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private bean-eof nil)

