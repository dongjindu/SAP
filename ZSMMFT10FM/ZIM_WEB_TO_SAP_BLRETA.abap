FUNCTION ZIM_WEB_TO_SAP_BLRETA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_ZSBLDT STRUCTURE  ZSBLDT_INF
*"----------------------------------------------------------------------
DATA: LV_ZRESULT LIKE ZSCA_IF_TIME_STAMP_OUT-ZRESULT.
DATA: LV_MESSAGE TYPE BAPI_MSG. "Message text (220)
DATA: W_SUCCESS  TYPE I,
      W_FAIL     TYPE I.

  REFRESH : IT_ZSBLDT_LOG.
  CLEAR : IT_ZSBLDT.

  DESCRIBE TABLE IT_ZSBLDT LINES W_LINE.
  IF W_LINE GT 0.
     LV_ZRESULT  =  'S'.
  ELSE.
     LV_ZRESULT  =  'E'.
  ENDIF.

* HEADER DATA LOOP -> MULTI BL UPDATE.
  LOOP AT IT_ZSBLDT.

    CLEAR : W_ERROR_FLAG.

    SELECT SINGLE * FROM ZTBL
    WHERE  ZFHBLNO  EQ   IT_ZSBLDT-ZFHBLNO .

    IF SY-SUBRC EQ 0.
       UPDATE  ZTBL
       SET     ZFRETA  =  IT_ZSBLDT-ZFRETA
       WHERE   ZFBLNO  =  ZTBL-ZFBLNO.
** Furong on 06/23/14 (
*       IF SY-SUBRC NE 0.
       IF SY-SUBRC eq 0.
** )
          W_SUCCESS  =  W_SUCCESS  +  1.
          LV_ZRESULT =  'S'.
       ELSE.
          W_FAIL     =  W_FAIL     +  1.
          LV_ZRESULT =  'E'.
       ENDIF.
    ELSE.
       W_FAIL  =  W_FAIL  +  1.
       LV_ZRESULT =  'E'.
    ENDIF.
    ">> Log Write.
    MOVE-CORRESPONDING IT_ZSBLDT  TO  IT_ZSBLDT_LOG.
    MOVE : SY-UNAME               TO  IT_ZSBLDT_LOG-ZUSER,
           SY-DATUM               TO  IT_ZSBLDT_LOG-ZEDAT,
           SY-UZEIT               TO  IT_ZSBLDT_LOG-ZETIM,
           'U'                    TO  IT_ZSBLDT_LOG-ZMODE,
           LV_ZRESULT             TO  IT_ZSBLDT_LOG-ZRESULT,
           LV_MESSAGE             TO  IT_ZSBLDT_LOG-ZMSG .

    PERFORM   GET_NUMBER_NEXT  USING  'TB'  IT_ZSBLDT_LOG-ZFBLNO.
    APPEND  IT_ZSBLDT_LOG.

  ENDLOOP.
  INSERT ZTBLDT_INF FROM TABLE IT_ZSBLDT_LOG.

*>> EAI Interface Log Write.
  PERFORM  Z_FCA_EAI_INTERFACE_LOG USING 'ZIM22'    W_LINE
                                         W_SUCCESS  W_FAIL.

ENDFUNCTION.
