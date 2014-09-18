FUNCTION ZIM_FTZ_TO_SAP.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_ZSFTZHD STRUCTURE  ZSFTZHD
*"      IT_ZSFTZHS STRUCTURE  ZSFTZHS
*"      IT_ZSFTZMT STRUCTURE  ZSFTZMT
*"----------------------------------------------------------------------
DATA: LV_ZRESULT LIKE ZSCA_IF_TIME_STAMP_OUT-ZRESULT.
DATA: LV_MESSAGE TYPE BAPI_MSG.
DATA: W_SUCCESS  TYPE I,
      W_FAIL     TYPE I.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFBDCYN EQ 'X'.
     W_MODE  =  'A'.
  ELSE.
     W_MODE  =  'N'.
  ENDIF.

  CLEAR : IT_ZSFTZHD, IT_ZSFTZMT, IT_ZSFTZHS.
  REFRESH : IT_ZSFTZHD_LOG, IT_ZSFTZHS_LOG, IT_ZSFTZMT_LOG.
  REFRESH : IT_ENTRY.

* B/L, Material Grouping
  PERFORM P2000_MAKE_GROUPING_MT TABLES IT_ZSFTZMT.

* HEADER DATA LOOP => Multi Entry Summary Create.
  LOOP AT IT_ZSFTZHD.

    W_TABIX  =  SY-TABIX.

    CLEAR : W_ERROR_FLAG.

*   Making BDC Data.
    PERFORM P2000_MAKE_CC_BDC_DATA TABLES IT_ZSFTZHD IT_ZSFTZMT.

*>> BDC CALL.
    REFRESH : MESSTAB. CLEAR : MESSTAB.

    SET PARAMETER ID 'ZPIVNO'  FIELD ''.

    IF W_ERROR_FLAG IS INITIAL.
       CALL TRANSACTION 'ZIM31'  USING       BDCDATA
                                 MODE        W_MODE
                                 UPDATE      'V'
                                 MESSAGES    INTO   MESSTAB.

       W_SUBRC = SY-SUBRC.
    ELSE.
       W_SUBRC = 4.
    ENDIF.

    IF W_SUBRC EQ 0.

       GET PARAMETER ID 'ZPIVNO'  FIELD W_ZPIVNO.

       MOVE : W_ZPIVNO            TO  IT_ENTRY-ZFIVNO,
              IT_ZSFTZHD-ZFENTNO  TO  IT_ENTRY-ZFENTNO.
       APPEND  IT_ENTRY.

       W_SUCCESS  =  W_SUCCESS  +  1.
       LV_ZRESULT =  'S'.
    ELSEIF W_SUBRC NE 0 AND W_ERROR_FLAG IS INITIAL.
       W_FAIL     =  W_FAIL     +  1.
       LV_ZRESULT =  'E'.
       IF W_MESSAGE IS INITIAL.
          W_MESSAGE  =  'BDC Data Creation Failed!'.
       ENDIF.
    ELSE.
       W_FAIL     =  W_FAIL     +  1.
       LV_ZRESULT =  'E'.
    ENDIF.

    ">> Header Table Error Log Write.
     MOVE-CORRESPONDING IT_ZSFTZHD TO  IT_ZSFTZHD_LOG.
     MOVE : SY-UNAME               TO  IT_ZSFTZHD_LOG-ZUSER,
            SY-DATUM               TO  IT_ZSFTZHD_LOG-ZEDAT,
            SY-UZEIT               TO  IT_ZSFTZHD_LOG-ZETIM,
            'C'                    TO  IT_ZSFTZHD_LOG-ZMODE,
            LV_ZRESULT             TO  IT_ZSFTZHD_LOG-ZRESULT,
            W_MESSAGE              TO  IT_ZSFTZHD_LOG-ZMSG .
     PERFORM   GET_NUMBER_NEXT  USING  'TB'  IT_ZSFTZHD_LOG-ZFDOCNO.
     APPEND IT_ZSFTZHD_LOG.

     ">> HS Table Error Log Write.
     LOOP AT IT_ZSFTZHS  WHERE  ZFENTNO  EQ  IT_ZSFTZHD-ZFENTNO.

        MOVE-CORRESPONDING IT_ZSFTZHS TO  IT_ZSFTZHS_LOG.
        MOVE : IT_ZSFTZHD_LOG-ZFDOCNO TO  IT_ZSFTZHS_LOG-ZFDOCNO,
               SY-TABIX               TO  IT_ZSFTZHS_LOG-ZFSEQ,
               SY-UNAME               TO  IT_ZSFTZHS_LOG-ZUSER,
               SY-DATUM               TO  IT_ZSFTZHS_LOG-ZEDAT,
               SY-UZEIT               TO  IT_ZSFTZHS_LOG-ZETIM,
               'C'                    TO  IT_ZSFTZHS_LOG-ZMODE,
               LV_ZRESULT             TO  IT_ZSFTZHS_LOG-ZRESULT,
               W_MESSAGE              TO  IT_ZSFTZHS_LOG-ZMSG .
        APPEND IT_ZSFTZHS_LOG.
     ENDLOOP.

     ">> Material Table Error Log Write.
     LOOP AT IT_ZSFTZMT  WHERE  ZFENTNO  EQ  IT_ZSFTZHD-ZFENTNO.

        MOVE-CORRESPONDING IT_ZSFTZMT TO  IT_ZSFTZMT_LOG.
        MOVE : IT_ZSFTZHD_LOG-ZFDOCNO TO  IT_ZSFTZMT_LOG-ZFDOCNO,
               SY-TABIX               TO  IT_ZSFTZMT_LOG-ZFSEQ,
               SY-UNAME               TO  IT_ZSFTZMT_LOG-ZUSER,
               SY-DATUM               TO  IT_ZSFTZMT_LOG-ZEDAT,
               SY-UZEIT               TO  IT_ZSFTZMT_LOG-ZETIM,
               'C'                    TO  IT_ZSFTZMT_LOG-ZMODE,
               LV_ZRESULT             TO  IT_ZSFTZMT_LOG-ZRESULT,
               W_MESSAGE              TO  IT_ZSFTZMT_LOG-ZMSG .
        APPEND IT_ZSFTZMT_LOG.
     ENDLOOP.

  ENDLOOP.

*>> EAI Interface Log Write.
  PERFORM  Z_FCA_EAI_INTERFACE_LOG USING 'ZIM31'    W_LINE
                                         W_SUCCESS  W_FAIL.

  WAIT UP TO 10 SECONDS.

*>> Entry Summary Insert.
  PERFORM P2000_MAKE_ENTRY_SUMMARY TABLES IT_ZSFTZHD IT_ZSFTZHS
                                          IT_ZSFTZMT.

  INSERT ZTFTZHD_LOG FROM TABLE IT_ZSFTZHD_LOG.
  INSERT ZTFTZHS_LOG FROM TABLE IT_ZSFTZHS_LOG.
  INSERT ZTFTZMT_LOG FROM TABLE IT_ZSFTZMT_LOG.

  COMMIT WORK.

ENDFUNCTION.
