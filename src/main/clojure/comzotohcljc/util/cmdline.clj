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

(ns ^{  :doc "Functions to enable console questions."
        :author "kenl" }

  comzotohcljc.util.cmdline )

(import '(java.io BufferedOutputStream InputStreamReader OutputStreamWriter))
(import '(java.io Reader Writer))
(import '(java.util Properties))
(import '(org.apache.commons.lang3 StringUtils))
(require '[ comzotohcljc.util.core :as CU])
(require '[ comzotohcljc.util.str :as SU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


;;(defrecord CmdSeqQ [qid qline choices dft must onok] )

(defn make-CmdSeqQ ""
  [qid qline choices dft must onok]
  {
    :choices choices
    :qline qline
    :qid qid
    :dft dft
    :must must
    :onok onok } )

(defn- readData
  ^String [^Writer cout ^Reader cin]
  (let [ buf (StringBuilder.)
         ms (loop [ c (.read cin) ]
                  ;; windows has '\r\n' linux has '\n'
                  (let [ m (cond
                              (or (= c -1)(= c 4))
                              #{ :quit :break }

                              (= c (int \newline))
                              #{ :break }

                              (or (= c (int \return))
                                  (= c (int \backspace)) (= c 27))
                              #{}

                              :else
                              (do (.append buf (char c)) #{})) ]
                    (if (contains? m :break)
                      m
                      (recur (.read cin))))) ]
    (if (contains? ms :quit) nil (.trim (.toString buf)))) )


(defn- popQQ [^Writer cout ^Reader cin cmdQ ^Properties props]
  (let [ must (:must cmdQ)
         dft (SU/nsb (:dft cmdQ))
         onResp (:onok cmdQ)
         q (:qline cmdQ)
         chs (SU/nsb (:choices cmdQ)) ]
    (.write cout (str q (if must "*" "" ) " ? "))
    (when-not (StringUtils/isEmpty chs)
      (if (SU/has? chs \n)
        (do (.write cout (str
              (if (.startsWith chs "\n") "[" "[\n")  chs
              (if (.endsWith chs "\n") "]" "\n]" ) )))
        (do (.write cout (str "[" chs "]")))))
    (when-not (StringUtils/isEmpty dft)
      (.write cout (str "(" dft ")")) )
    (.write cout " ")
    (.flush cout)
    ;; get the input from user
    ;; point to next question, blank ends it
    (let [ rc (readData cout cin)]
      (if (nil? rc)
        (do (.write cout "\n") nil )
        (do (onResp (if (StringUtils/isEmpty rc) dft rc) props))))) )

(defn- popQ [^Writer cout ^Reader cin cmdQ ^Properties props]
  (if (nil? cmdQ)
    ""
    (popQQ cout cin cmdQ props)) )

(defn- cycleQ [^Writer cout ^Reader cin cmdQNs ^String start ^Properties props]
  (do
    (loop [ rc (popQ cout cin (get cmdQNs start) props) ]
      (cond
        (StringUtils/isEmpty rc)
        (CU/into-map props)

        (nil? rc)
        nil

        :else
        (recur (popQ cout cin (get cmdQNs rc) props))))))

(defn cli-converse [cmdQs ^String q1]
  (let [ cout (OutputStreamWriter. (BufferedOutputStream. (System/out)))
         kp (if (CU/is-windows?) "<Ctrl-C>" "<Ctrl-D>")
         cin (InputStreamReader. (System/in))
         props (Properties.) ]
    (.write cout (str ">>> Press " kp "<Enter> to cancel...\n"))
    (cycleQ cout cin cmdQs q1 props)))

(comment
(def q1 (make-CmdSeqQ "q1" "hello ken" "q|b|c" "c" true
           (fn [a ps]
             (do (.put ps "a1" a) "q2")) ) )
(def q2 (make-CmdSeqQ "q2" "hello paul" "" "" false
           (fn [a ps]
             (do (.put ps "a2" a) "q3"))) )
(def q3 (make-CmdSeqQ "q3" "hello joe" "z" "" false
           (fn [a ps]
             (do (.put ps "a3" a) "" ))) )
(def QM { "q1" q1 "q2" q2 "q3" q3 })
)


(def ^:private cmdline-eof nil)

