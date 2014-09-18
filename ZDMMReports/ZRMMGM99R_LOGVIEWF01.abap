*----------------------------------------------------------------------*
*   INCLUDE ZRMMGM99R_LOGVIEWF01
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_it_ztlog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_ztlog.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_ztlog
    FROM ztlog
    WHERE logno_h    IN s_log_h  AND
          logno_d    IN s_log_d  AND
          zdocno     IN s_zdocno AND
          ztcode     IN s_ztcode AND
          zprogramm  IN s_zpgm   AND
          zuname     IN s_zuname AND
          zdatum     IN s_zdatum AND
          zuzeit     IN s_zuzeit AND
          bb_msgtyp  IN s_msgtyp.
ENDFORM.                    " get_it_ztlog
