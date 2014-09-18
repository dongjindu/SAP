*----------------------------------------------------------------------*
*   INCLUDE ZRMMGM99R_LOGVIEWTOP
*----------------------------------------------------------------------*

TABLES: ztlog.   "Table for Application Log

*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-bl1.
SELECT-OPTIONS: s_log_h   FOR ztlog-logno_h.
SELECT-OPTIONS: s_log_d   FOR ztlog-logno_d.
SELECT-OPTIONS: s_zdocno  FOR ztlog-zdocno.
SELECT-OPTIONS: s_ztcode  FOR ztlog-ztcode.
SELECT-OPTIONS: s_zpgm    FOR ztlog-zprogramm.
SELECT-OPTIONS: s_zuname  FOR ztlog-zuname.
SELECT-OPTIONS: s_zdatum  FOR ztlog-zdatum
                          OBLIGATORY.
SELECT-OPTIONS: s_zuzeit  FOR ztlog-zuzeit.
SELECT-OPTIONS: s_msgtyp  FOR ztlog-bb_msgtyp.
SELECTION-SCREEN END OF BLOCK bl1.
*----------------------------------------------------------------------*

DATA: it_ztlog    LIKE TABLE OF ztlog.  " Itab for App Log

* Log Date
data: fr_logdate  like sy-datum.
data: to_logdate  like sy-datum.
