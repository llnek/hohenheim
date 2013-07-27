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

(ns ^{  :doc "Utility functions for class related or reflection related operations."
        :author "kenl" }
  comzotohcljc.util.meta)


(import '(java.lang.reflect Field Method Modifier))
(require '[ comzotohcljc.util.core :as CU ] )
(require '[ comzotohcljc.util.str :as SU ] )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmulti is-child "Returns true if clazz is subclass of this base class."
  (fn [a b]
    (cond
      (instance? Class b)
      :class

      :else
      :object)))

(defmethod is-child :class
  [^Class basz ^Class cz]
  (if (or (nil? basz) (nil? cz)) false (.isAssignableFrom basz cz)))

(defmethod is-child :object
  [^Class basz obj]
  (if (or (nil? basz) (nil? obj)) false (is-child basz (.getClass obj))))

(defn bytes-class "Return the java class for byte[]."
  []
  (Class/forName "[B"))

(defn chars-class "Return the java class for char[]."
  []
  (Class/forName "[C"))

(defn is-boolean? "True if class is Boolean."
  [^Class classObj]
  (SU/eq-any? (.getName classObj) ["boolean" "Boolean" "java.lang.Boolean"] ))

(defn is-char? "True if class is Char."
  [^Class classObj]
  (SU/eq-any? (.getName classObj) [ "char" "Char" "java.lang.Character" ] ))

(defn is-int? "True if class is Int."
  [^Class classObj]
  (SU/eq-any? (.getName classObj) [ "int" "Int" "java.lang.Integer" ] ))

(defn is-long? "True if class is Long."
  [^Class classObj]
  (SU/eq-any? (.getName classObj) [ "long" "Long" "java.lang.Long" ] ))

(defn is-float? "True if class is Float."
  [^Class classObj]
  (SU/eq-any? (.getName classObj) [ "float" "Float" "java.lang.Float" ]))

(defn is-double? "True if class is Double."
  [^Class classObj]
  (SU/eq-any? (.getName classObj) [ "double" "Double" "java.lang.Double" ]))

(defn is-byte? "True if class is Byte."
  [^Class classObj]
  (SU/eq-any? (.getName classObj) [ "byte" "Byte" "java.lang.Byte" ]))

(defn is-short? "True if class is Short."
  [^Class classObj]
  (SU/eq-any? (.getName classObj) [ "short" "Short" "java.lang.Short" ]))

(defn is-string? "True if class is String."
  [^Class classObj]
  (SU/eq-any? (.getName classObj) [ "String" "java.lang.String" ]))

(defn is-bytes? "True if class is byte[]."
  [^Class classObj]
  (= classObj (bytes-class)) )

(defn for-name "Load a java class by name."
  ( [^String z] (for-name z nil))
  ( [^String z ^ClassLoader cl]
    (if (nil? cl) (java.lang.Class/forName z) (java.lang.Class/forName z true cl))) )

(defn get-cldr "Get the current classloader."
  ([] (get-cldr nil))
  ([^ClassLoader cl] (if (nil? cl) (.getContextClassLoader (Thread/currentThread)) cl)))

(defn set-cldr "Set current classloader."
  [^ClassLoader cl]
  (let []
    (CU/test-nonil "class-loader" cl)
    (.setContextClassLoader (Thread/currentThread) cl)))

(defn load-class "Load this class by name."
  ( [^String clazzName] (load-class clazzName nil))
  ( [^String clazzName ^ClassLoader cl]
    (if (not (SU/hgl? clazzName)) nil (.loadClass (get-cldr cl) clazzName))))

(defn- ctorObj [cz]
  (do
    (CU/test-nonil "java-class" cz)
    (.newInstance (.getDeclaredConstructor cz (make-array Class 0))  (make-array Object 0)  )))

(defn make-obj "Make an object of this class by calling the default constructor."
  ([^String clazzName] (make-obj clazzName nil))
  ([^String clazzName ^ClassLoader cl]
   (if (not (SU/hgl? clazzName)) nil (ctorObj (load-class clazzName cl)))) )

(defn list-parents "List all parent classes."
  [^Class javaClass]
  (let [ rc (loop [ sum (transient []) par javaClass ]
              (if (nil? par)
                (persistent! sum)
                (recur (conj! sum par) (.getSuperclass par))))  ]
    ;; since we always add the original class, we need to ignore it on return
    (if (> (.size rc) 1) (rest rc) [] )))


(defn- iterXXX [ ^Class cz level getDeclXXX bin ]
  (let [ props (getDeclXXX cz) ]
    (reduce (fn [sum m]
              (let [ x (.getModifiers m) ]
                (if (and (> level 0)
                         (or (Modifier/isStatic x) (Modifier/isPrivate x)) )
                  sum
                  (assoc! sum (.getName m) m)))) bin props) ))

(defn- listMtds [ ^Class cz level ]
  (let [ par (.getSuperclass cz) ]
    (iterXXX cz level #(.getDeclaredMethods %)
             (if (nil? par) (transient {}) (listMtds par (inc level))))))

(defn- listFlds [ ^Class cz level ]
  (let [ par (.getSuperclass cz) ]
    (iterXXX cz level #(.getDeclaredFields %)
             (if (nil? par) (transient {}) (listFlds par (inc level))))))

(defn list-methods "List all methods belonging to this class, including inherited ones."
  [^Class javaClass]
  (vals (if (nil? javaClass)
          {}
          (persistent! (listMtds javaClass 0 ))   )) )

(defn list-fields "List all fields belonging to this class, including inherited ones."
  [^Class javaClass]
  (vals (if (nil? javaClass)
          {}
          (persistent! (listFlds javaClass 0 ))     )) )





(def ^:private meta-eof nil)

