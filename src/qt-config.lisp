;;; qt-config.lisp -- QT configuration definitions for use by
;;; the XARF Quicktime reports in 'quicktime.lisp'.
;;; D. Racine, 20080326

(in-package :xarf)

;; Host on which RLD's Qstatus program is deployed & running:
(defparameter *qstatus-host* "hostname2")

;; User with read access to QT logs on app-tier hosts:
(defparameter *qt-ssh-user* "qtrptusr")

(defstruct qt-instance id client webfarm type)

(defstruct qt-node id hostname webfarm)

(defparameter *qt-webfarms*
  '((test . "https://test_vip_dns_name.domain/proweb/qt<ID>/login")
    (prod . "https://prod_vip_dns_name.domain/proweb/qt<ID>/login")))

(defparameter *qt-instance-types*
  '(dev
    trn-dmo
    test-nr-auto
    test-nr-func
    test-em-auto
    test-em-func
    prod
    para))

(defparameter *qt-hostname-regex* "^hostname[0-9]$")

(defparameter *qt-nodes*
  (mapcar
   (lambda (x) (apply #'make-qt-node x))
   '((:id a01 :hostname "hostname1" :webfarm test)
     (:id a02 :hostname "hostname2" :webfarm test)
     (:id a03 :hostname "hostname3" :webfarm prod)
     (:id a04 :hostname "hostname4" :webfarm prod)
     (:id a05 :hostname "hostname5" :webfarm prod))))

(defparameter *qt-instances*
  (mapcar
   (lambda (x) (apply #'make-qt-instance x))
   '((:id |0002| :client nil  :webfarm test :type test-nr-auto)
     (:id |504|  :client nil  :webfarm test :type test-nr-func)
     (:id |505|  :client nil  :webfarm test :type test-nr-func)
     (:id |506|  :client nil  :webfarm test :type test-nr-func)
     (:id |507|  :client nil  :webfarm test :type test-nr-func)
     (:id |608|  :client nil  :webfarm prod :type trn-dmo)    
     (:id |1034| :client sos  :webfarm test :type test-em-func)
     (:id |1036| :client dnfs :webfarm test :type test-em-func)
     (:id |1038| :client usgs :webfarm test :type test-em-func)
     (:id |1040| :client bse  :webfarm test :type test-em-func)
     (:id |1042| :client tib  :webfarm test :type test-em-func)
     (:id |1044| :client osm  :webfarm test :type test-em-func)
     (:id |1046| :client pds  :webfarm test :type test-em-func)
     (:id |1048| :client bia  :webfarm test :type test-em-func)
     (:id |1050| :client itc  :webfarm test :type test-em-func)
     (:id |1052| :client fws  :webfarm test :type test-em-func)
     (:id |1054| :client oig  :webfarm test :type test-em-func)
     (:id |1056| :client nps  :webfarm test :type test-em-func)
     (:id |1058| :client nil  :webfarm test :type test-em-func)
     (:id |1060| :client blm  :webfarm test :type test-em-func)
     (:id |1062| :client mcc  :webfarm test :type test-em-func)
     (:id |1064| :client pbgc :webfarm test :type test-em-func)
     (:id |1066| :client ost  :webfarm test :type test-em-func)
     (:id |1068| :client ptr  :webfarm test :type test-em-func)
     (:id |1070| :client nil  :webfarm test :type test-em-func)
     (:id |1072| :client ustc :webfarm test :type test-em-func)
     (:id |1074| :client eeoc :webfarm test :type test-em-func)
     (:id |1076| :client onr  :webfarm test :type test-em-func)
     (:id |1078| :client bem  :webfarm test :type test-em-func)
     (:id |1080| :client nara :webfarm test :type test-em-func)
     (:id |1082| :client sss  :webfarm test :type test-em-func)
     (:id |1084| :client imls :webfarm test :type test-em-func)
     (:id |1338| :client usgs :webfarm test :type test-nr-auto)
     (:id |1534| :client sos  :webfarm prod :type prod)
     (:id |1536| :client dnfs :webfarm prod :type prod)
     (:id |1538| :client usgs :webfarm prod :type prod)
     (:id |1540| :client bse  :webfarm prod :type prod)
     (:id |1542| :client tib  :webfarm prod :type prod)
     (:id |1544| :client osm  :webfarm prod :type prod)
     (:id |1546| :client pds  :webfarm prod :type prod)
     (:id |1548| :client bia  :webfarm prod :type prod)
     (:id |1550| :client itc  :webfarm prod :type prod)
     (:id |1552| :client fws  :webfarm prod :type prod)
     (:id |1554| :client oig  :webfarm prod :type prod)
     (:id |1556| :client nps  :webfarm prod :type prod)
     (:id |1558| :client nil  :webfarm prod :type prod)
     (:id |1560| :client blm  :webfarm prod :type prod)
     (:id |1562| :client mcc  :webfarm prod :type prod)
     (:id |1564| :client pbgc :webfarm prod :type prod)
     (:id |1566| :client ost  :webfarm prod :type prod)
     (:id |1568| :client ptr  :webfarm prod :type prod)
     (:id |1570| :client nil  :webfarm prod :type prod)
     (:id |1572| :client ustc :webfarm prod :type prod)
     (:id |1574| :client eeoc :webfarm prod :type prod)
     (:id |1576| :client onr  :webfarm prod :type prod)
     (:id |1578| :client bem  :webfarm prod :type prod)
     (:id |1580| :client nara :webfarm prod :type prod)
     (:id |1582| :client sss  :webfarm prod :type prod)
     (:id |1584| :client imls :webfarm prod :type prod)
     (:id |2044| :client nil  :webfarm prod :type para)
     (:id |2048| :client bia  :webfarm prod :type para)
     (:id |5555| :client nil  :webfarm test :type test-em-auto))))

