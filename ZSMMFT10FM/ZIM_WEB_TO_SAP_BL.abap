FUNCTION ZIM_WEB_TO_SAP_BL .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_ZSBLHD STRUCTURE  ZSBLHD_INF
*"      IT_ZSBLIT STRUCTURE  ZSBLIT_INF
*  Date        Developer    Request        Description
*  02/19/2007  Manju        UD1K930445     Check for BOL
*                                          before creating ASN
*"----------------------------------------------------------------------
DATA: LV_ZRESULT LIKE ZSCA_IF_TIME_STAMP_OUT-ZRESULT.
DATA: LV_MESSAGE TYPE BAPI_MSG. "Message text (220)
DATA: W_SUCCESS  TYPE I,
      W_FAIL     TYPE I.

  W_MODE  =  'N'.

  REFRESH : IT_TEMP_CIV, IT_TEMP_BLIT.
  CLEAR : IT_ZSBLHD, IT_ZSBLIT.

  DESCRIBE TABLE IT_ZSBLHD LINES W_LINE.
  IF W_LINE GT 0.
     LV_ZRESULT  =  'S'.
  ELSE.
     LV_ZRESULT  =  'E'.
     W_MESSAGE   =  'No Data Uplaod!'.
  ENDIF.

* HEADER DATA LOOP => Multi B/L Create.
  LOOP AT IT_ZSBLHD.

    W_TABIX  =  SY-TABIX.

    CLEAR : W_ERROR_FLAG, W_MESSAGE, W_SUBRC.

*   Making BDC Data.
    PERFORM P2000_MAKE_BDC_DATA TABLES IT_ZSBLHD IT_ZSBLIT.
    IF W_ERROR_FLAG IS INITIAL.
*>> BDC CALL.
       REFRESH : MESSTAB. CLEAR : MESSTAB.

       SET PARAMETER ID 'ZPBLNO' FIELD ''.
       SET PARAMETER ID 'ZPHBLNO' FIELD ''.

       CALL TRANSACTION 'ZIM21'  USING       BDCDATA
                                 MODE        W_MODE
                                 UPDATE      'V'
                                 MESSAGES    INTO   MESSTAB.

       W_SUBRC = SY-SUBRC.

    ENDIF.

    IF W_SUBRC EQ 0 AND W_ERROR_FLAG IS INITIAL.

       PERFORM  P3000_CONTAINER_INSERT  USING  W_ZPBLNO.

       W_SUCCESS  =  W_SUCCESS  +  1.
       LV_ZRESULT =  'S'.
    ENDIF.

    IF W_SUBRC NE 0 OR W_ERROR_FLAG EQ 'X'.
       W_FAIL     =  W_FAIL     +  1.
       LV_ZRESULT =  'E'.
       IF W_MESSAGE IS INITIAL.
          W_MESSAGE = 'BDC(B/L) Error!'.
       ENDIF.
    ENDIF.

    ">> Header Table Error Log Write.
     MOVE-CORRESPONDING IT_ZSBLHD TO  IT_ZSBLHD_LOG.
     MOVE : SY-UNAME              TO  IT_ZSBLHD_LOG-ZUSER,
            SY-DATUM              TO  IT_ZSBLHD_LOG-ZEDAT,
            SY-UZEIT              TO  IT_ZSBLHD_LOG-ZETIM,
            'C'                   TO  IT_ZSBLHD_LOG-ZMODE,
            LV_ZRESULT            TO  IT_ZSBLHD_LOG-ZRESULT,
            W_MESSAGE             TO  IT_ZSBLHD_LOG-ZMSG .
     PERFORM   GET_NUMBER_NEXT  USING  'TB'  IT_ZSBLHD_LOG-ZFBLNO.
     APPEND IT_ZSBLHD_LOG.

     ">> Item Table Error Log Write.
     LOOP AT IT_ZSBLIT  WHERE  ZFHBLNO  EQ  IT_ZSBLHD-ZFHBLNO.

        MOVE-CORRESPONDING IT_ZSBLIT TO  IT_ZSBLIT_LOG.
        MOVE : IT_ZSBLHD_LOG-ZFBLNO  TO  IT_ZSBLIT_LOG-ZFBLNO,
               SY-TABIX              TO  IT_ZSBLIT_LOG-ZFSEQ,
               SY-UNAME              TO  IT_ZSBLIT_LOG-ZUSER,
               SY-DATUM              TO  IT_ZSBLIT_LOG-ZEDAT,
               SY-UZEIT              TO  IT_ZSBLIT_LOG-ZETIM,
               'C'                   TO  IT_ZSBLIT_LOG-ZMODE,
               LV_ZRESULT            TO  IT_ZSBLIT_LOG-ZRESULT,
               W_MESSAGE             TO  IT_ZSBLIT_LOG-ZMSG .
        APPEND IT_ZSBLIT_LOG.
     ENDLOOP.
  ENDLOOP.

  INSERT ZTBLHD_INF FROM TABLE IT_ZSBLHD_LOG.
  INSERT ZTBLIT_INF FROM TABLE IT_ZSBLIT_LOG.

*>> EAI Interface Log Write.
  PERFORM  Z_FCA_EAI_INTERFACE_LOG USING 'ZIM21'    W_LINE
                                         W_SUCCESS  W_FAIL.


  WAIT UP TO 5 SECONDS.

*>> Commercial Invoice BDC Data Making.
  PERFORM P2000_MAKE_CIV_DATA TABLES IT_ZSBLIT.

  " BDC Data Make & Call
  PERFORM P2000_CIV_BDC_DATA.

  COMMIT WORK.

ENDFUNCTION.
