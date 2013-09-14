;;
;; Copyright (c) 2013 Cherimoia, LLC. All rights reserved.
;;
;; This library is distributed in the hope that it will be useful
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.hhh.auth.shiro

  (:gen-class
    :extends org.apache.shiro.realm.AuthorizingRealm
    :name comzotohcljc.hhh.auth.shiro.JdbcRealm
    :init myInit
    :constructors {[] []}
    :exposes-methods { init superInit  getServletName myName}
    :state myState
  ))

(import '(org.apache.shiro.subject PrincipalCollection))
(import '(org.apache.shiro.authz AuthorizationInfo))
(import '(org.apache.shiro.realm AuthorizingRealm))
(import '(org.apache.shiro.authc SimpleAccount))

(defn -doGetAuthenticationInfo
  [^AuthenticationToken token]
    (let [ pwd (.getCredentials token)
           user (.getPrincipal token)
           db (dbio-connect jdbc mcache)
           sql (.newSimpleSQLr db) ]
      (try
        (let[ acc (get-loginAccount sql user (CE/pwdify pwd)) ]
          (SimpleAuthenticationInfo.
            (:acctid acc)
            (:passwd acc)
            "realm-name")
          )
        (catch Throwable e#
          (throw (AuthenticationException. e#))))
      ))

(defn -doGetAuthorizationInfo [^PrincipalCollection principals]
        String username = (String) getAvailablePrincipal(principals);

        //call the underlying EIS for the account data:
        return getAccount(username);
    }
}

