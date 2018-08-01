;;; wt-config.lisp -- WebTA configuration definitions for use
;;; by XARF WebTA reports in 'webta.lisp'.
;;; D. Racine 20140528

(in-package :xarf)

;; User with read access to webTA logs on app-tier hosts:
(defparameter *wt-ssh-user* "wtrptusr")

(defstruct wt-instance id client webfarm type)

(defstruct wt-node id hostname webfarm)

(defparameter *wt-webfarms*
  '((test . "https://test_vip_dns_name.domain/<ID>/Login")
    (prod . "https://prod_vip_dns_name.domain/<ID>/Login")))

(defparameter *wt-instance-types*
  '((trn-dmo . "4")
    (test-nr-auto . "5")
    (test-nr-func . "6")
    (test-em-auto . "12")
    (test-em-func . "14")
    (prod . "16")
    (para . "17")))

(defparameter *wt-nodes*
  (mapcar
   (lambda (x) (apply #'make-wt-node x))
   '((:id a01 :hostname "hostname1" :webfarm test)
     (:id a02 :hostname "hostname2" :webfarm test)
     (:id a03 :hostname "hostname3" :webfarm prod)
     (:id a04 :hostname "hostname4" :webfarm prod)
     (:id a05 :hostname "hostname5" :webfarm prod))))

(defparameter *wt-instances*
  (mapcar
   (lambda (x) (apply #'make-wt-instance x))
   '((:id |489|  :client nil  :webfarm prod :type trn-dmo)
     (:id |490|  :client nil  :webfarm prod :type trn-dmo)
     (:id |491|  :client nil  :webfarm prod :type trn-dmo)
     (:id |492|  :client nil  :webfarm prod :type trn-dmo)
     (:id |493|  :client nil  :webfarm prod :type trn-dmo)
     (:id |610|  :client ibwc :webfarm test :type test-nr-func)
     (:id |611|  :client iaf  :webfarm test :type test-nr-func)
     (:id |612|  :client ntsb :webfarm test :type test-nr-func)
     (:id |613|  :client opic :webfarm test :type test-nr-func)
     (:id |614|  :client ssa  :webfarm test :type test-nr-func)
     (:id |615|  :client nsf  :webfarm test :type test-nr-func)
     (:id |616|  :client edu  :webfarm test :type test-nr-func)
     (:id |617|  :client flra :webfarm test :type test-nr-func)
     (:id |618|  :client ftc  :webfarm test :type test-nr-func)     
     (:id |619|  :client ferc :webfarm test :type test-nr-func)
     (:id |620|  :client ussc :webfarm test :type test-nr-func)
     (:id |621|  :client cpsc :webfarm test :type test-nr-func)
     (:id |622|  :client nlrb :webfarm test :type test-nr-func)
     (:id |623|  :client fhfa :webfarm test :type test-nr-func)
     (:id |624|  :client sec  :webfarm test :type test-nr-func)
     (:id |625|  :client dcc  :webfarm test :type test-nr-func)
     (:id |627|  :client pbgc :webfarm test :type test-nr-func)
     (:id |689|  :client nil  :webfarm test :type test-nr-func)
     (:id |1214| :client ssa  :webfarm prod :type test-em-auto)
     (:id |1410| :client ibwc :webfarm test :type test-em-func)
     (:id |1411| :client iaf  :webfarm test :type test-em-func)
     (:id |1412| :client ntsb :webfarm test :type test-em-func)
     (:id |1413| :client opic :webfarm test :type test-em-func)
     (:id |1414| :client ssa  :webfarm test :type test-em-func)
     (:id |1415| :client nsf  :webfarm test :type test-em-func)
     (:id |1416| :client edu  :webfarm test :type test-em-func)
     (:id |1417| :client flra :webfarm test :type test-em-func)
     (:id |1418| :client ftc  :webfarm test :type test-em-func)     
     (:id |1419| :client ferc :webfarm test :type test-em-func)
     (:id |1420| :client ussc :webfarm test :type test-em-func)
     (:id |1421| :client cpsc :webfarm test :type test-em-func)
     (:id |1422| :client nlrb :webfarm test :type test-em-func)
     (:id |1423| :client fhfa :webfarm test :type test-em-func)
     (:id |1424| :client sec  :webfarm test :type test-em-func)
     (:id |1425| :client dcc  :webfarm test :type test-em-func)
     (:id |1427| :client pbgc :webfarm test :type test-em-func)
     (:id |1610| :client ibwc :webfarm prod :type prod)
     (:id |1611| :client iaf  :webfarm prod :type prod)
     (:id |1612| :client ntsb :webfarm prod :type prod)
     (:id |1613| :client opic :webfarm prod :type prod)
     (:id |1614| :client ssa  :webfarm prod :type prod)
     (:id |1615| :client nsf  :webfarm prod :type prod)
     (:id |1616| :client edu  :webfarm prod :type prod)
     (:id |1617| :client flra :webfarm prod :type prod)
     (:id |1618| :client ftc  :webfarm prod :type prod)     
     (:id |1619| :client ferc :webfarm prod :type prod)
     (:id |1620| :client ussc :webfarm prod :type prod)
     (:id |1621| :client cpsc :webfarm prod :type prod)
     (:id |1622| :client nlrb :webfarm prod :type prod)
     (:id |1623| :client fhfa :webfarm prod :type prod)
     (:id |1624| :client sec  :webfarm prod :type prod)
     (:id |1625| :client dcc  :webfarm prod :type prod)
     (:id |1627| :client pbgc :webfarm prod :type prod)
     (:id |1714| :client ssa  :webfarm prod :type para))))

; Scripts in app-tier-hosts[1-5]:/path/to/groovy/ whose basenames are
; listed here will be accessible via /bulkgroovy:
(defparameter *groovy*
  '("FetchPaycodeValues" "RolesAndPermissionWithTargets" "MaxProcessedUserCountPayPeriod"))

; Timesheet states in processing order (also in host[1-5]:~/bin/xarf-ts-status):
(defparameter *wt-ts-states*
  '("Saved" "Pending Attestation" "Validated by Employee" "Validated by Timekeeper"
    "Validation Overridden" "Certified" "Pre-processed" "Processed" "Corrected"))
