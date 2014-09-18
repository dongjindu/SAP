FUNCTION ZIM_FTZ_TO_SAP_NEW.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_ZSFTZHD STRUCTURE  ZSFTZHD
*"      IT_ZSFTZMT STRUCTURE  ZSFTZMT
*"----------------------------------------------------------------------
  DATA: LV_ZRESULT LIKE ZSCA_IF_TIME_STAMP_OUT-ZRESULT.
  DATA: LV_MESSAGE TYPE BAPI_MSG.
  DATA: W_SUCCESS  TYPE I,
        W_FAIL     TYPE I,
        W_TEMP_MATNR LIKE ZTIVIT-MATNR.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFBDCYN EQ 'X'.
    W_MODE  =  'A'.
  ELSE.
    W_MODE  =  'N'.
  ENDIF.

  CLEAR : IT_ZSFTZHD, IT_ZSFTZMT.
  REFRESH : IT_ZSFTZHD_LOG, IT_ZSFTZHS_LOG, IT_ZSFTZMT_LOG.
  REFRESH : IT_ENTRY.

* B/L, Material Grouping
  PERFORM P2000_MAKE_GROUPING_MT TABLES IT_ZSFTZMT.

* HEADER DATA LOOP => Multi Entry Summary Create.
  LOOP AT IT_ZSFTZHD.

    W_TABIX  =  SY-TABIX.

    CLEAR : W_ERROR_FLAG.


* Begin of changes - UD1K940719
*   Making BDC Data.
* PERFORM P2000_MAKE_CC_BDC_DATA TABLES IT_ZSFTZHD IT_ZSFTZMT.

*    PERFORM P2000_MAKE_CC_BDC_DATA_REV TABLES IT_ZSFTZHD IT_ZSFTZMT.





**>> BDC CALL.
*    REFRESH : MESSTAB. CLEAR : MESSTAB.
*
*    SET PARAMETER ID 'ZPIVNO'  FIELD ''.

*    IF W_ERROR_FLAG IS INITIAL.
*      CALL TRANSACTION 'ZIM31'  USING       BDCDATA
*                                MODE        W_MODE
*                                UPDATE      'V'
*                                MESSAGES    INTO   MESSTAB.
*
*      W_SUBRC = SY-SUBRC.
*    ELSE.
*      W_SUBRC = 4.
*    ENDIF.

*    IF W_SUBRC EQ 0.
*
*      GET PARAMETER ID 'ZPIVNO'  FIELD W_ZPIVNO.
*
*      MOVE : W_ZPIVNO            TO  IT_ENTRY-ZFIVNO,
*             IT_ZSFTZHD-ZFENTNO  TO  IT_ENTRY-ZFENTNO.
*      APPEND  IT_ENTRY.
*
*      W_SUCCESS  =  W_SUCCESS  +  1.
*      LV_ZRESULT =  'S'.
*    ELSEIF W_SUBRC NE 0 AND W_ERROR_FLAG IS INITIAL.
*      W_FAIL     =  W_FAIL     +  1.
*      LV_ZRESULT =  'E'.
*      IF W_MESSAGE IS INITIAL.
*        W_MESSAGE  =  'BDC Data Creation Failed!'.
*      ENDIF.
*    ELSE.
*      W_FAIL     =  W_FAIL     +  1.
*      LV_ZRESULT =  'E'.
*    ENDIF.

* End of changes -   UD1K940719

    ">> Header Table Error Log Write.
    MOVE-CORRESPONDING IT_ZSFTZHD TO  IT_ZSFTZHD_LOG.
    select single * from  ZTFTZHD_LOG where
            ZFENTNO eq IT_ZSFTZHD_LOG-ZFENTNO and
            ZFENTP  eq IT_ZSFTZHD_LOG-ZFENTP.
    if sy-subrc eq 0.
        move 'E' to LV_ZRESULT.
        Move 'Entry Alreay exists for the same Key' to    W_MESSAGE.
    else.
         LV_ZRESULT = 'S'.
         clear W_MESSAGE.
    endif.
    MOVE : SY-UNAME               TO  IT_ZSFTZHD_LOG-ZUSER,
           SY-DATUM               TO  IT_ZSFTZHD_LOG-ZEDAT,
           SY-UZEIT               TO  IT_ZSFTZHD_LOG-ZETIM,
           'C'                    TO  IT_ZSFTZHD_LOG-ZMODE,
           LV_ZRESULT             TO  IT_ZSFTZHD_LOG-ZRESULT,
           W_MESSAGE              TO  IT_ZSFTZHD_LOG-ZMSG .
    PERFORM   GET_NUMBER_NEXT  USING  'TB'  IT_ZSFTZHD_LOG-ZFDOCNO.
    APPEND IT_ZSFTZHD_LOG.

    ">> HS Table Error Log Write.
*     LOOP AT IT_ZSFTZHS  WHERE  ZFENTNO  EQ  IT_ZSFTZHD-ZFENTNO.
*
*        MOVE-CORRESPONDING IT_ZSFTZHS TO  IT_ZSFTZHS_LOG.
*        MOVE : IT_ZSFTZHD_LOG-ZFDOCNO TO  IT_ZSFTZHS_LOG-ZFDOCNO,
*               SY-TABIX               TO  IT_ZSFTZHS_LOG-ZFSEQ,
*               SY-UNAME               TO  IT_ZSFTZHS_LOG-ZUSER,
*               SY-DATUM               TO  IT_ZSFTZHS_LOG-ZEDAT,
*               SY-UZEIT               TO  IT_ZSFTZHS_LOG-ZETIM,
*               'C'                    TO  IT_ZSFTZHS_LOG-ZMODE,
*               LV_ZRESULT             TO  IT_ZSFTZHS_LOG-ZRESULT,
*               W_MESSAGE              TO  IT_ZSFTZHS_LOG-ZMSG .
*        APPEND IT_ZSFTZHS_LOG.
*     ENDLOOP.

.
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
*  PERFORM P2000_MAKE_ENTRY_SUMMARY TABLES IT_ZSFTZHD IT_ZSFTZHS
*                                          IT_ZSFTZMT.

*  PERFORM P2000_MAKE_ENTRY_SUMMARY_NEW TABLES IT_ZSFTZHD
*                                              IT_ZSFTZMT.

  INSERT ZTFTZHD_LOG FROM TABLE IT_ZSFTZHD_LOG.
*  INSERT ZTFTZHS_LOG FROM TABLE IT_ZSFTZHS_LOG.
  INSERT ZTFTZMT_LOG FROM TABLE IT_ZSFTZMT_LOG.

         LV_ZRESULT = 'S'.
         clear W_MESSAGE.

* Insert Records to ZTMM_DUTY_HD Table & ZTMM_DUTY_IT Tables
  loop at IT_ZSFTZHD_LOG.
    ZTMM_DUTY_HD-ENTNO = IT_ZSFTZHD_LOG-ZFENTNO.
    ZTMM_DUTY_HD-FENTP = IT_ZSFTZHD_LOG-ZFENTP.
    ZTMM_DUTY_HD-ENTDATE = IT_ZSFTZHD_LOG-ZFEDT.
    ZTMM_DUTY_HD-ENTAMOUNT = IT_ZSFTZHD_LOG-ZFIVAMK.
    ZTMM_DUTY_HD-ENTCURR = 'USD'. "IT_ZSFTZHD_LOG-ZFUSD.
    ZTMM_DUTY_HD-DUTY_AMT = IT_ZSFTZHD_LOG-ZFDUTY.
    ZTMM_DUTY_HD-MPF_AMT = IT_ZSFTZHD_LOG-ZFMPF.
    ZTMM_DUTY_HD-Zuser = IT_ZSFTZHD_LOG-ZUSER.
    ZTMM_DUTY_HD-CR_DATE = IT_ZSFTZHD_LOG-ZEDAT.
    ZTMM_DUTY_HD-CR_TIME = IT_ZSFTZHD_LOG-ZETIM.
*   ZTMM_DUTY_HD-ZMSG = IT_ZSFTZHD_LOG-ZMSG.
*   ZTMM_DUTY_HD-ZMSG = LV_ZRESULT.
    insert ZTMM_DUTY_HD.
  endloop.

* UD1K940997 - by IG.MOON
* {
*  loop at IT_ZSFTZMT_LOG.
*    ZTMM_DUTY_IT-ENTNO = IT_ZSFTZMT_LOG-ZFENTNO.
*    ZTMM_DUTY_IT-SEQ = IT_ZSFTZMT_LOG-ZFSEQ.
** Color code logic / Check whether PO EXISTS in EKKO
*
*    select single * from EKKO where ebeln = IT_ZSFTZMT_LOG-EBELN(10).
*    if sy-subrc eq 0.
*      W_STRLEN     = STRLEN( IT_ZSFTZMT-EBELN ).
*      W_COLOR_LEN  = W_STRLEN - 10.
*      IF W_COLOR_LEN GT 0.
*        CONCATENATE  IT_ZSFTZMT-MATNR(10)
*                     IT_ZSFTZMT-EBELN+10(W_COLOR_LEN)
*                     INTO W_TEMP_MATNR.
*        ZTMM_DUTY_IT-MATNR = W_TEMP_MATNR.
*        ZTMM_DUTY_IT-EBELN =  IT_ZSFTZMT_LOG-EBELN(10).
*      ELSE.
*        ZTMM_DUTY_IT-MATNR = IT_ZSFTZMT_LOG-matnr.
*        ZTMM_DUTY_IT-EBELN =  IT_ZSFTZMT_LOG-EBELN.
*      ENDIF.
*    else.
*      ZTMM_DUTY_IT-MATNR = IT_ZSFTZMT_LOG-matnr.
*      ZTMM_DUTY_IT-EBELN =  IT_ZSFTZMT_LOG-EBELN.
*   endif.
*    ZTMM_DUTY_IT-MENGE = IT_ZSFTZMT_LOG-MENGE.
*    ZTMM_DUTY_IT-UOM = IT_ZSFTZMT_LOG-ZFQNTM.
*    ZTMM_DUTY_IT-ENTAMOUNT = IT_ZSFTZMT_LOG-ZFAMT.
*    ZTMM_DUTY_IT-ENTCURR  = 'USD'.
*    ZTMM_DUTY_IT-DUTY_AMT = IT_ZSFTZMT_LOG-ZFDUTY.
*    ZTMM_DUTY_IT-MPF_AMT = IT_ZSFTZMT_LOG-ZFMPAMT.
*    insert ZTMM_DUTY_IT.
*  endloop.

  loop at IT_ZSFTZMT_LOG.
    ZTMM_DUTY_IT-ENTNO = IT_ZSFTZMT_LOG-ZFENTNO.
    ZTMM_DUTY_IT-SEQ = IT_ZSFTZMT_LOG-ZFSEQ.
* Color code logic / Check whether PO EXISTS in EKKO
    select single * from EKKO where ebeln = IT_ZSFTZMT_LOG-EBELN(10).
    if sy-subrc eq 0.
      W_STRLEN     = STRLEN( IT_ZSFTZMT_LOG-EBELN ).
      W_COLOR_LEN  = W_STRLEN - 10.
      IF W_COLOR_LEN GT 0.
        CONCATENATE  IT_ZSFTZMT_LOG-MATNR(10)
                     IT_ZSFTZMT_LOG-EBELN+10(W_COLOR_LEN)
                     INTO W_TEMP_MATNR.
        ZTMM_DUTY_IT-MATNR = W_TEMP_MATNR.
        ZTMM_DUTY_IT-EBELN =  IT_ZSFTZMT_LOG-EBELN(10).
      ELSE.
        ZTMM_DUTY_IT-MATNR = IT_ZSFTZMT_LOG-matnr.
        ZTMM_DUTY_IT-EBELN =  IT_ZSFTZMT_LOG-EBELN.
      ENDIF.
    else.
      ZTMM_DUTY_IT-MATNR = IT_ZSFTZMT_LOG-matnr.
      ZTMM_DUTY_IT-EBELN =  IT_ZSFTZMT_LOG-EBELN.
   endif.
    ZTMM_DUTY_IT-MENGE = IT_ZSFTZMT_LOG-MENGE.
    ZTMM_DUTY_IT-UOM = IT_ZSFTZMT_LOG-ZFQNTM.
    ZTMM_DUTY_IT-ENTAMOUNT = IT_ZSFTZMT_LOG-ZFAMT.
    ZTMM_DUTY_IT-ENTCURR  = 'USD'.
    ZTMM_DUTY_IT-DUTY_AMT = IT_ZSFTZMT_LOG-ZFDUTY.
    ZTMM_DUTY_IT-MPF_AMT = IT_ZSFTZMT_LOG-ZFMPAMT.
    insert ZTMM_DUTY_IT.
  endloop.

* } UD1K940997

  COMMIT WORK.

ENDFUNCTION.
