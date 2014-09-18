*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM01F03                                                  *
*&---------------------------------------------------------------------*
*&  ÇÁ·Î±×·¥¸í : ¼öÀÔ B/L °ü·Ã Sub MODULE Include                      *
*&      ÀÛ¼ºÀÚ : °­¼®ºÀ INFOLINK Ltd.                                  *
*&      ÀÛ¼ºÀÏ : 2001.02.28                                            *
*&  Àû¿ëÈ¸»çPJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_SELECT_CIVHST_ITEM
*&---------------------------------------------------------------------*
FORM P2000_LINE_SELECT_CIVHST_ITEM.

  CLEAR : ZSCIVHST.
  W_COUNT = 0.
  LOOP AT IT_ZSCIVHST WHERE ZFMARK = 'X'.
    W_COUNT = W_COUNT + 1.
    MOVE-CORRESPONDING  IT_ZSCIVHST  TO ZSCIVHST.
  ENDLOOP.

ENDFORM.                    " P2000_LINE_SELECT_CIVHST_ITEM
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIV_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_ZTIV_MODIFY.
  IF W_OK_CODE NE 'DELE'.
    CLEAR : W_COUNT, W_MONETARY, W_IV_LINE.
    LOOP AT IT_ZSIVIT WHERE NOT ( CCMENGE LE 0
                      AND         GRMENGE LE 0 ).
      ADD   1   TO  W_COUNT.
      IF NOT IT_ZSIVIT-ZFPOTY IS INITIAL.
         W_MONETARY  =  W_MONETARY + 1.
      ENDIF.
    ENDLOOP.
    IF W_COUNT EQ 0.
       MESSAGE E565.  EXIT.
    ENDIF.
    DESCRIBE TABLE IT_ZSIVIT LINES W_IV_LINE.
    IF W_IV_LINE EQ W_MONETARY.
       MOVE  'N'   TO  ZTIV-ZFPOYN.
    ELSEIF W_MONETARY EQ 0.
       MOVE  'Y'   TO  ZTIV-ZFPOYN.
    ELSE.
       MOVE  'M'   TO  ZTIV-ZFPOYN.
    ENDIF.
  ENDIF.

* Give a Number in case of creation.
  IF W_STATUS EQ C_REQ_C.
     PERFORM   P2000_GET_NUMBER_NEXT  USING  'IV'  ZTIV-ZFIVNO.
  ENDIF.

  CALL FUNCTION 'ZIM_CUSTOMS_CLEARANCE_MODIFY'
       EXPORTING
            W_OK_CODE     = W_OK_CODE
            ZFIVNO        = ZTIV-ZFIVNO
            ZFSTATUS      = W_STATUS
            W_ZTIV        = ZTIV
            W_ZTIV_OLD    = *ZTIV
       TABLES
            IT_ZSIVIT     = IT_ZSIVIT
            IT_ZSIVIT_OLD = IT_ZSIVIT_ORG
       EXCEPTIONS
            ERROR_UPDATE  = 4.

  W_SUBRC  =  SY-SUBRC.

  IF W_SUBRC  NE  0.
    CLEAR : ZTIV-ZFIVNO.
    MESSAGE  E952.  EXIT.
  ELSE.
*------------------------------------------------------------------
* 2002/03/03 KSB MODIFY  B/L Data UPDATE.
* ¡æ Manage Import Trade Classification in B/L.
* PERFORM ZTBL_ZTIV_POYN.
*------------------------------------------------------------------
    SET PARAMETER ID 'ZPIVNO'   FIELD ZTIV-ZFIVNO.

    IF W_OK_CODE EQ 'IMPREQ'.
       MESSAGE  S410  WITH  ZTIV-ZFIVNO.
       LEAVE TO TRANSACTION 'ZIM62' AND SKIP FIRST SCREEN.
    ELSEIF W_OK_CODE EQ 'DELE'.
      MESSAGE  S756.   "  WITH  ZTIV-ZFIVNO.
    ELSE.
      MESSAGE  S410  WITH  ZTIV-ZFIVNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_ZTIV_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_CC_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
FORM P2000_CC_DOC_ITEM_SELECT.

  DATA : WL_HBLNO LIKE ZTBL-ZFHBLNO.

  W_ZFIVNO    = ZSIV-ZFIVNO.

  REFRESH : IT_ZSIV, IT_ZSIV_TMP.
  CLEAR : WL_HBLNO.

* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIV_TMP
  FROM   ( ZTIV  AS  A INNER  JOIN  ZTIVIT  AS B
  ON       A~ZFIVNO    EQ     B~ZFIVNO )
  INNER    JOIN  ZTBL  AS     C
  ON       B~ZFBLNO    EQ     C~ZFBLNO
  WHERE    C~ZFHBLNO   EQ     ZSIV-ZFHBLNO.

  LOOP AT IT_ZSIV_TMP.
     IF WL_HBLNO NE IT_ZSIV_TMP-ZFHBLNO AND SY-TABIX NE 1.
        APPEND IT_ZSIV.
     ENDIF.

     MOVE-CORRESPONDING IT_ZSIV_TMP  TO  IT_ZSIV.
     MOVE IT_ZSIV_TMP-ZFHBLNO  TO  WL_HBLNO.

     AT LAST.
        APPEND IT_ZSIV.
     ENDAT.
  ENDLOOP.

  DESCRIBE TABLE IT_ZSIV LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.  EXIT.
  ENDIF.

  W_STATUS_CHK = 'C'.
  INCLUDE = 'CCHBL'.                 ">Customs Clearance Request.

  CALL SCREEN 0014 STARTING AT  07 3
                   ENDING   AT  87 15.

ENDFORM.                    " P2000_CC_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIV_DUP_LIST_1
*&---------------------------------------------------------------------*
FORM P2000_ZTIV_DUP_LIST_1.

  WRITE : / SY-VLINE, IT_ZSIV-ZFIVNO COLOR COL_KEY INTENSIFIED,
            SY-VLINE.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE : IT_ZSIV-ZFCCDT,   SY-VLINE.

*>> Customs Type
  CASE IT_ZSIV-ZFCLCD.
    WHEN 'A'.
      WRITE : 'Bonded',   SY-VLINE.
    WHEN 'C'.
      WRITE : 'Taxiable', SY-VLINE.
    WHEN 'X'.
      WRITE : 'N.Clear.', SY-VLINE.
    WHEN OTHERS.
      WRITE : '        ', SY-VLINE.
  ENDCASE.
*>> Clearance Status.
  CASE IT_ZSIV-ZFCUST.
    WHEN '1'.
      WRITE : (17) 'Declaration Creat', SY-VLINE.
    WHEN '2'.
      WRITE : (17) 'object to declare', SY-VLINE.
    WHEN '3'.
      WRITE : (17) 'in declaring',      SY-VLINE.
    WHEN 'Y'.
      WRITE : (17) 'Clearance',         SY-VLINE.
    WHEN 'N'.
      WRITE : (17) 'Not object clear',  SY-VLINE.
  ENDCASE.
*>> G/R Status.
  CASE IT_ZSIV-ZFGRST.
    WHEN 'Y'.
      WRITE : (09) 'to G/R',    SY-VLINE.
    WHEN 'N'.
      WRITE : (09) 'Part. G/R', SY-VLINE.
    WHEN 'P'.
      WRITE : (09) 'Completed', SY-VLINE.
    WHEN 'X'.
      WRITE : (09) 'Not obj.',  SY-VLINE.
  ENDCASE.

ENDFORM.                    " P2000_ZTIV_DUP_LIST_1
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IV_DOC
*&---------------------------------------------------------------------*
FORM P1000_READ_IV_DOC.

  CALL FUNCTION 'ZIM_GET_CC_DOCUMENT'
       EXPORTING
            ZFIVNO        = ZTIV-ZFIVNO
       IMPORTING
            W_ZTIV        = ZTIV
       TABLES
            IT_ZSIVIT     = IT_ZSIVIT
            IT_ZSIVIT_ORG = IT_ZSIVIT_ORG
            IT_ZSIVCD     = IT_ZSIVCD
            IT_ZSIVHST    = IT_ZSIVHST
            IT_ZSIVHST1   = IT_ZSIVHST1
       EXCEPTIONS
            NOT_FOUND     = 4
            NOT_INPUT     = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E413 WITH ZTIV-ZFIVNO.
    WHEN 8.
      MESSAGE E412.
  ENDCASE.

*>> ºñ¿ëDOCUMENT SELECT.
  W_ZFIMDNO = ZTIV-ZFIVNO.
  CALL FUNCTION 'ZIM_GET_COST_DOCUMENT'
       EXPORTING
            ZFCSTGRP    = '006'
            ZFIMDNO     = W_ZFIMDNO
       TABLES
            IT_ZSIMCOST = IT_ZSIMCOST.
   *ZTIV   =   ZTIV.

*>> Ref. Document Select
  IF NOT ZTIV-ZFBLNO IS INITIAL.
    SELECT SINGLE * FROM  ZTBL
                    WHERE ZFBLNO EQ ZTIV-ZFBLNO.
    MOVE : ZTBL-ZFHBLNO  TO  ZSIV-ZFHBLNO.

    IF NOT ZTBL-ZFMSNO IS INITIAL.
      SELECT SINGLE * FROM  ZTMSHD
                     WHERE ZFMSNO  EQ  ZTBL-ZFMSNO.
    ENDIF.
  ENDIF.

  IF NOT ZTIV-BUKRS IS INITIAL.
    SELECT SINGLE * FROM  T001
                    WHERE BUKRS  EQ  ZTIV-BUKRS.
  ENDIF.
*>> °Å·¡Ã³..
  IF NOT ZTIV-LIFNR IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTIV-LIFNR
                                           CHANGING  W_LIFNR_NM.
  ELSE.
    CLEAR : W_LIFNR_NM.
  ENDIF.

*>> °¡º¥´õ.
  IF NOT ZTIV-ZFPHVN IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTIV-ZFPHVN
                                           CHANGING  W_ZFOPBN_NM.
  ELSE.
    CLEAR : W_ZFOPBN_NM.
  ENDIF.
  CLEAR : ZSIMIMG08.
  IF NOT ZTIV-ZFPONC IS INITIAL.
    SELECT SINGLE ZFCDNM INTO ZSIMIMG08-ZFCDNM
                    FROM ZTIMIMG08
                    WHERE ZFCDTY   EQ   '001'
                    AND   ZFCD     EQ   ZTIV-ZFPONC.
  ENDIF.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE E963.
  ELSE.
    IF ZTIMIMG00-ZFCSTMD IS INITIAL.
      MESSAGE E986.
    ENDIF.
  ENDIF.

  IF SY-TCODE(5) EQ 'ZIM32'.
*    IF NOT ( ZTIV-ZFCUST EQ '1' OR ZTIV-ZFCUST EQ 'Y' ).  "> Åë°ü»óÅÂ.
    IF ZTIV-ZFCUST NE '1'.  "> Åë°ü»óÅÂ.
      CASE ZTIV-ZFCUST.
        WHEN '2'.
          MESSAGE E419 WITH ZTIV-ZFIVNO 'request object' 'Change'.
        WHEN '3'.
          MESSAGE E419 WITH ZTIV-ZFIVNO 'Request in progress' 'Change'.
        WHEN 'Y'.
          MESSAGE E419 WITH ZTIV-ZFIVNO 'clearance completion' 'Change'.
      ENDCASE.
    ENDIF.
    IF ZTIV-ZFCDST EQ 'Y'.        "> ¹èºÎ»óÅÂ.
      MESSAGE E420 WITH ZTIV-ZFIVNO 'Distribution completioná' 'change'.
    ENDIF.
    IF ZTIV-ZFGRST EQ 'Y'.        "> ÀÔ°í»óÅÂ.
      MESSAGE E422 WITH ZTIV-ZFIVNO 'G/R completion' 'change'.
    ENDIF.
    IF ZTIV-ZFGRST EQ 'P'.        "> ÀÔ°í»óÅÂ.
      MESSAGE E422 WITH ZTIV-ZFIVNO 'Partial G/R in progress' 'change'.
    ENDIF.

    IF ZTIV-ZFCIVST EQ 'Y'.        "> Á¦ºñ¿ë»óÅÂ.
      MESSAGE E423 WITH ZTIV-ZFIVNO 'Transaction completion' 'change'.
    ENDIF.
  ENDIF.

  IF SY-TCODE EQ 'ZIM31L' OR SY-TCODE EQ 'ZIM32L' OR
     SY-TCODE EQ 'ZIM33L' OR SY-TCODE EQ 'ZIM34L'.
    IF NOT ( ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' ).
      MESSAGE E633 WITH ZSIV-ZFIVNO ZTIV-ZFREQTY.
    ENDIF.
  ENDIF.

  IF SY-TCODE(5) EQ 'ZIM32' OR SY-TCODE(5) EQ 'ZIM34'.
    PERFORM P2000_SET_LOCK .
  ENDIF.

ENDFORM.                    " P1000_READ_IV_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_CC_DOC_ITEM_SELECT_1
*&---------------------------------------------------------------------*
FORM P2000_CC_DOC_ITEM_SELECT_1.
  W_ZFIVNO    = ZSIV-ZFIVNO.

  REFRESH IT_ZSIV.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIV
           FROM   ZVBL_IV
           WHERE  ZFBLNO EQ ZSIV-ZFBLNO.

  DESCRIBE TABLE IT_ZSIV LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'CCBL'.                 ">Åë°ü¿äÃ» Á¶È¸.

  CALL SCREEN 0014 STARTING AT  07 3
                   ENDING   AT  87 15.

ENDFORM.                    " P2000_CC_DOC_ITEM_SELECT_1
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE TABLES   IT_ERR_LIST STRUCTURE IT_ERR_LIST
                          USING    DOC_NO.

  LOOP AT  RETURN.

    MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
           RETURN-ID           TO     IT_ERR_LIST-MSGID,
           RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
           RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
           RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
           RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
           RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
           RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT,
           DOC_NO              TO     IT_ERR_LIST-ZFIVNO,
           IT_ZSBKPF-ZFSEQ     TO     IT_ERR_LIST-ZFSEQ.

    CASE IT_ERR_LIST-MSGTYP.
      WHEN 'E'.
        MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
      WHEN 'I'.
        MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'S'.
        MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'W'.
        MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
    ENDCASE.
    APPEND  IT_ERR_LIST.
  ENDLOOP.

ENDFORM.                    " P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_EXEC_INV_DOC_CALCEL
*&---------------------------------------------------------------------*
FORM P2000_EXEC_INV_DOC_CALCEL.
  W_OK_CODE = OK-CODE.

  PERFORM   P2000_LINE_SELECT_CIVHST_ITEM.
  CASE W_COUNT.
    WHEN 0.        MESSAGE S962.   EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965.   EXIT.
  ENDCASE.

*>> Ãë¼Ò¹®¼­ ¿©ºÎ Ã¼Å©.
  IF NOT ZSCIVHST-CBELNR IS INITIAL.
    MESSAGE E426.
  ENDIF.
  MOVE-CORRESPONDING  ZSCIVHST   TO   ZTCIVHST.

*-----------------------------------------------------------------------
* AUTHORITY CHECK
  PERFORM P2000_AUTHORITY_CHECK USING W_OK_CODE.
*-----------------------------------------------------------------------
* Lock Object Set
  IF W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D.
    PERFORM P2000_SET_LOCK.
  ENDIF.

* MESSAGE BOX
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      IF W_STATUS NE C_REQ_D.
        PERFORM  P3000_DB_MODIFY_SCRCOM.
      ENDIF.
      PERFORM  P3000_CALL_INVOICE_CANCEL
               USING ZTCIVHD-ZFCIVRN.

      PERFORM  P2000_SET_UNLOCK.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
* MKIM --¿Ï·á ¸Þ½ÃÁö »Ñ¸®?
      IF W_SUBRC EQ 0.
        MESSAGE  S918.
      ENDIF.
      CLEAR OK-CODE.
      LEAVE SCREEN.
    WHEN 'N'.              " No...
      MESSAGE  S957.
      CLEAR OK-CODE.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_EXEC_INV_DOC_CALCEL
*&---------------------------------------------------------------------*
*&      Form  P2000_MIR2_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_MIR2_MESSAGE.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-CSREALYN EQ 'X'.
    ANTWORT = 'Y'.
  ELSE.
    ZTCIVHST-CBUDAT = SY-DATUM.   "> TO DAY.

    CASE SY-LANGU.
      WHEN '3'.
        PERFORM P2000_MESSAGE_BOX USING 'ÀüÇ¥ Ãë¼Ò È®ÀÎ'
                       'ÇØ´ç ÀüÇ¥¸¦ Ãë¼ÒÇÕ´Ï´Ù.'
                       '°è¼Ó ÁøÇà ÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                       'N'             " Ãë¼Ò ¹öÆ° À¯.
                       '1'.            " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
               'Slip cancel confirm'
               'Now cancel the slip.'
               'Do you want to continue?'
               'N'
               '1'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_MIR2_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_INVOICE_CANCEL
*&---------------------------------------------------------------------*
FORM P3000_CALL_INVOICE_CANCEL USING    P_ZFCIVRN.

  CALL FUNCTION 'ZIM_BAPI_INVOICE_CANCEL'
       EXPORTING
            P_ZFCIVRN                 = P_ZFCIVRN
            INVOICEDOCNUMBER          = ZTCIVHST-BELNR
            FISCALYEAR                = ZTCIVHST-GJAHR
            REASONREVERSAL            = ZTCIVHST-STGRD
            POSTINGDATE               = ZTCIVHST-CBUDAT
       IMPORTING
            INVOICEDOCNUMBER_REVERSAL = ZTCIVHST-CBELNR
            FISCALYEAR_REVERSAL       = ZTCIVHST-CGJAHR
       TABLES
            RETURN                    = RETURN
       EXCEPTIONS
            OTHERS                    = 4.

  W_SUBRC = SY-SUBRC.

  IF SY-SUBRC NE 0.
    READ TABLE RETURN INDEX 1.
    IF SY-SUBRC NE 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY  NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      MESSAGE ID RETURN-ID TYPE RETURN-TYPE NUMBER RETURN-NUMBER
                 WITH   RETURN-MESSAGE_V1  RETURN-MESSAGE_V2
                        RETURN-MESSAGE_V3  RETURN-MESSAGE_V4.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_CALL_INVOICE_CANCEL
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_SELECT_IV_ITEM
*&---------------------------------------------------------------------*
FORM P2000_LINE_SELECT_IV_ITEM.

  CLEAR : ZSIVIT.
  W_COUNT = 0.
  LOOP AT IT_ZSIVIT WHERE ZFMARK = 'X'.
    W_COUNT = W_COUNT + 1.
    MOVE-CORRESPONDING  IT_ZSIVIT  TO ZSIVIT.
  ENDLOOP.

ENDFORM.                    " P2000_LINE_SELECT_IV_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_CALL_TCODE_IV_ITEM
*&---------------------------------------------------------------------*
FORM P2000_LINE_CALL_TCODE_IV_ITEM.
  DATA : L_AWKEY   LIKE   BKPF-AWKEY.

  CASE W_COUNT.
    WHEN 0.        MESSAGE S962.   EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965.   EXIT.
  ENDCASE.

  CASE  OK-CODE.
    WHEN 'PODP'.          " P/O DISPLAY
      PERFORM  P2000_PO_DOC_DISPLAY USING ZSIVIT-EBELN
                                          ZSIVIT-EBELP.
    WHEN 'IMDP'.          " IMPORT L/C DISPLAY
      PERFORM  P2000_LC_DOC_DISPLAY USING ZSIVIT-ZFREQNO ''.
    WHEN 'BLDP'.          " B/L DISPLAY
      PERFORM  P2000_BL_DOC_DISPLAY USING ZSIVIT-ZFBLNO.
*      WHEN 'CIDP'.          " COMMERCIAL DISPLAY
*         PERFORM  P2000_CIV_DOC_DISPLAY USING ZSIVIT-ZFCIVRN.
    WHEN 'CGDP'.          ">ÇÏ¿ªÁ¶È¸.
      PERFORM  P2000_CG_DOC_DISPLAY USING ZSIVIT-ZFCGNO
                                          ZSIVIT-ZFBLNO.
*      WHEN 'DIMI'.          "> ÀÚÀç¹®¼­.
*         SET  PARAMETER ID  'RBN'   FIELD   ZSIVHST-MBLNR.
*         SET  PARAMETER ID  'GJR'   FIELD   ZSIVHST-MJAHR.
*
*         CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
*      WHEN 'DIFB'.          "> ÈÄ¼Ó¹®¼­.
*         CLEAR : L_AWKEY.
*         MOVE : ZSIVHST-MBLNR  TO   L_AWKEY(10),
*                ZTIV-BUKRS     TO   L_AWKEY+10(4),
*                ZSIVHST-MJAHR  TO   L_AWKEY+14(4).

*        CLEAR : BKPF.
*         SELECT * FROM BKPF UP TO 1 ROWS
*                  WHERE AWKEY  EQ  L_AWKEY.
*         ENDSELECT.
*         IF SY-SUBRC EQ 0.
*            SET  PARAMETER ID  'BUK'   FIELD   BKPF-BUKRS.
*            SET  PARAMETER ID  'BLN'   FIELD   BKPF-BELNR.
*            SET  PARAMETER ID  'GJR'   FIELD   BKPF-GJAHR.
*            CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
*         ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_LINE_CALL_TCODE_IV_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_CG_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_CG_DOC_DISPLAY USING    P_ZFCGNO P_ZFBLNO.

  IF P_ZFCGNO IS INITIAL.
    MESSAGE E391.
  ENDIF.

  SELECT SINGLE * FROM ZTCGHD WHERE ZFCGNO EQ P_ZFCGNO.

  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPCGNO'  FIELD P_ZFCGNO.
  SET PARAMETER ID 'ZPCGPT'  FIELD ZTCGHD-ZFCGPT.

  CALL TRANSACTION 'ZIM83' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_CG_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSIVIT_UPDATE
*&---------------------------------------------------------------------*
FORM P2000_IT_ZSIVIT_UPDATE    USING  PA_MODE..
* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  CHECK : ZTIV-ZFREQTY NE 'LO' AND ZTIV-ZFREQTY NE 'PU'.

  DESCRIBE TABLE IT_ZSIVIT   LINES W_LINE.
  IF W_LINE < 1.
    IF PA_MODE EQ 'E'.
      MESSAGE ID 'ZIM' TYPE PA_MODE NUMBER '901'.
    ENDIF.
  ENDIF.

  W_TOT_AMOUNT  = 0.    W_COUNT      = 0.    W_TOT_ITEM   = 0.
  W_TOT_AMOUNT1 = 0.

  CLEAR : ZSCIVIT.
  LOOP AT IT_ZSIVIT.
    W_TABIX = SY-TABIX.
    W_COUNT = W_COUNT + 1.

    ADD  IT_ZSIVIT-ZFIVAMT TO  W_TOT_AMOUNT.
    ADD  IT_ZSIVIT-ZFIVAMK TO  W_TOT_AMOUNT1.

    W_TOT_ITEM   = W_TOT_ITEM   +   IT_ZSIVIT-CCMENGE.

  ENDLOOP.

** Packing Chg, Handing CHG. Addition
  W_TOT_AMOUNT = W_TOT_AMOUNT + ZTIV-ZFPKCHG + ZTIV-ZFHDCHG.

  WRITE :  ZTIV-ZFIVAMT TO W_AMTTXT1 CURRENCY  ZTIV-ZFIVAMC.
  W_AMTLEN1 = STRLEN( W_AMTTXT1 ).

  WRITE : W_TOT_AMOUNT TO W_AMTTXT2 CURRENCY ZTIV-ZFIVAMC.
  W_AMTLEN2 = STRLEN( W_AMTTXT2 ).

  ZTIV-ZFIVAMK = W_TOT_AMOUNT1.
  IF W_OK_CODE EQ 'CALC'.
    ZTIV-ZFIVAMT = W_TOT_AMOUNT.
  ELSE.
    IF W_TOT_AMOUNT NE ZTIV-ZFIVAMT.
      MESSAGE W437 WITH  W_AMTTXT1(W_AMTLEN1)  W_AMTTXT2(W_AMTLEN2).
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_IT_ZSIVIT_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSIVIT_RECALC
*&---------------------------------------------------------------------*
FORM P2000_IT_ZSIVIT_RECALC.
  LOOP AT IT_ZSIVIT.
    W_TABIX = SY-TABIX.

*>> Material Invoice Amount Compute
    IF IT_ZSIVIT-BPUMN IS INITIAL OR IT_ZSIVIT-BPUMZ IS INITIAL.
      IT_ZSIVIT-ZFIVAMT =   IT_ZSIVIT-CCMENGE *
                          ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ).
    ELSE.
      IT_ZSIVIT-ZFIVAMT = ( IT_ZSIVIT-CCMENGE *
                          ( IT_ZSIVIT-BPUMZ / IT_ZSIVIT-BPUMN ) *
                          ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ) ).
    ENDIF.

*>> Local Currency Amount Compute
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIVIT-ZFIVAMT
                                            IT_ZSIVIT-ZFIVAMC
                                            IT_ZSIVIT-ZFIVAMK.
     *BAPICURR-BAPICURR = ( ZTIV-ZFEXRT / ZTIV-FFACT )
                          * IT_ZSIVIT-ZFIVAMK.

    PERFORM SET_CURR_CONV_TO_INTERNAL USING
                                     *BAPICURR-BAPICURR  ZTIV-ZFKRW.
    IF *BAPICURR-BAPICURR GT 9999999999999.
      MESSAGE W923 WITH *BAPICURR-BAPICURR.
      IT_ZSIVIT-ZFIVAMK = 0.
    ELSE.
      IT_ZSIVIT-ZFIVAMK = *BAPICURR-BAPICURR.
    ENDIF.

    MODIFY  IT_ZSIVIT  INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P2000_IT_ZSIVIT_RECALC
*&---------------------------------------------------------------------*
*&      Form  P2000_NO_INPUT
*&---------------------------------------------------------------------*
FORM P2000_NO_INPUT USING NOI_TAB NOI_FIELD
                          DFIES-SCRTEXT_M RC.

  DATA: BEGIN OF FELD,
          TABLE(10),
          STRICH,
          NAME(10),
        END OF FELD.

  FELD-TABLE = NOI_TAB.
  FELD-STRICH = '-'.
  CONDENSE FELD.
  SEARCH FELD FOR '-'.
  SY-FDPOS = SY-FDPOS - 1.
  WRITE '-' TO FELD+SY-FDPOS.
  SY-FDPOS = SY-FDPOS + 1.
  WRITE NOI_FIELD TO FELD+SY-FDPOS.
  IF SY-STEPL EQ 0.
    SET CURSOR FIELD FELD.
  ELSE.
    SET CURSOR FIELD FELD LINE SY-STEPL.
  ENDIF.

  PERFORM GET_FTEXT(RDDFIE00) USING NOI_TAB NOI_FIELD SY-LANGU
                              CHANGING DFIES RC.
*  IF rc EQ 0.
*    MESSAGE e083 WITH dfies-scrtext_m.
*  ELSE.
*    MESSAGE e083 WITH noi_tab noi_field.
*  ENDIF.

ENDFORM.                    " P2000_NO_INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_REQ_TO_CC_MOVE
*&---------------------------------------------------------------------*
FORM P2000_REQ_TO_CC_MOVE.

  IF ZSIV-ZFTRIPLE EQ 'X'.
    IF ZTREQHD-ZFREQTY EQ 'PU' OR ZTREQHD-ZFREQTY EQ 'LO'.
      MESSAGE E442 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
    ENDIF.
  ELSE.
    IF NOT ( ZTREQHD-ZFREQTY EQ 'PU' OR ZTREQHD-ZFREQTY EQ 'LO' ).
      MESSAGE E442 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
    ENDIF.
  ENDIF.

  IF ZSIV-ZFTRIPLE EQ 'X'.
    IF ZTREQHD-ZFTRIPLE NE 'X'.
      MESSAGE E441 WITH  ZTREQHD-ZFREQNO.
    ENDIF.
  ENDIF.

*  IF SY-DYNNR EQ '3500'.
  MOVE : ZTREQHD-WAERS       TO     ZTIV-ZFIVAMC,
         'KRW'               TO     ZTIV-ZFKRW,
         ZTREQHD-ZTERM       TO     ZTIV-ZTERM,
         ZTREQHD-LIFNR       TO     ZTIV-LIFNR,  " ¼öÀÍÀÚ.
         ZTREQHD-BUKRS       TO     ZTIV-BUKRS,
         ZTREQHD-ZFREQTY     TO     ZTIV-ZFREQTY,
         ZTREQHD-KURSF       TO     ZTIV-ZFEXRT,
         ZTREQHD-FFACT       TO     ZTIV-FFACT,
         SY-UNAME            TO     ZTIV-ERNAM,
         SY-DATUM            TO     ZTIV-CDAT,
         SY-UNAME            TO     ZTIV-UNAM,
         SY-DATUM            TO     ZTIV-UDAT.
*  ENDIF.

  REFRESH : IT_ZSIVIT.
*  SELECT * INTO TABLE IT_ZSREQIT
  SELECT * INTO CORRESPONDING FIELDS OF TABLE  IT_ZSREQIT
           FROM ZTREQIT
           WHERE ZFREQNO  EQ  ZTREQHD-ZFREQNO.

  LOOP AT IT_ZSREQIT.
    CLEAR : IT_ZSIVIT.
    MOVE-CORRESPONDING   IT_ZSREQIT   TO   IT_ZSIVIT.
*++++> P/O DATA Á¶È¸.
    SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                  WERKS LGORT BPUMN BPUMZ REPOS
           INTO (IT_ZSIVIT-MENGE_PO, IT_ZSIVIT-UEBTO,
                 IT_ZSIVIT-UEBTK,    IT_ZSIVIT-WEPOS,
                 IT_ZSIVIT-ELIKZ,    IT_ZSIVIT-LOEKZ,
                 IT_ZSIVIT-UNTTO,    IT_ZSIVIT-WERKS,
                 IT_ZSIVIT-LGORT,
                 IT_ZSIVIT-BPUMN,    IT_ZSIVIT-BPUMZ,
                 IT_ZSIVIT-REPOS)
           FROM   EKPO
           WHERE  EBELN   EQ   ZTREQHD-EBELN
           AND    EBELP   EQ   IT_ZSREQIT-EBELP.

    IF IT_ZSIVIT-LOEKZ NE SPACE.
      CLEAR : ZSIVIT.
      MESSAGE W069 WITH ZTREQHD-EBELN IT_ZSIVIT-EBELP.
      CONTINUE.
    ENDIF.

*    IF IT_ZSIVIT-ELIKZ EQ 'X'.
*       CLEAR : ZSIVIT.
*       MESSAGE W359 WITH ZTREQHD-EBELN IT_ZSIVIT-EBELP.
*       CONTINUE.
*    ENDIF.

*+++++> ¼öÀÔÀÇ·Ú ¼ö·®.
    SELECT SINGLE MENGE INTO IT_ZSIVIT-MENGE
           FROM  ZTREQIT
           WHERE ZFREQNO EQ IT_ZSREQIT-ZFREQNO
           AND   ZFITMNO EQ IT_ZSREQIT-ZFITMNO.

    MOVE : ZTREQHD-WAERS             TO   IT_ZSIVIT-ZFIVAMC,
           ZTREQHD-EBELN             TO   IT_ZSIVIT-EBELN,
           IT_ZSREQIT-EBELP          TO   IT_ZSIVIT-EBELP,
           IT_ZSREQIT-ZFREQNO        TO   IT_ZSIVIT-ZFREQNO,
           IT_ZSREQIT-ZFITMNO        TO   IT_ZSIVIT-ZFITMNO,
           IT_ZSREQIT-ZFITMNO        TO   IT_ZSIVIT-ZFIVDNO,
           'KRW'                     TO   IT_ZSIVIT-ZFKRW.

*>> ±âÃ³¸® ¼ö·®.
    SELECT SUM( CCMENGE ) SUM( GRMENGE )
           INTO (IT_ZSIVIT-ZFCCTOT, IT_ZSIVIT-ZFGRTOT)
           FROM ZTIVIT
           WHERE ZFREQNO  EQ IT_ZSIVIT-ZFREQNO
           AND   ZFITMNO  EQ IT_ZSIVIT-ZFITMNO.
*            AND   ZFPRPYN NE 'Y'.            " ¼±±Þ±ÝÀÌ ¾Æ´Ñ °Í.

    IT_ZSIVIT-CCMENGE = IT_ZSIVIT-MENGE - IT_ZSIVIT-ZFCCTOT.
    IF IT_ZSIVIT-CCMENGE LT 0.
      IT_ZSIVIT-CCMENGE = 0.
    ENDIF.

    IT_ZSIVIT-GRMENGE = IT_ZSIVIT-MENGE - IT_ZSIVIT-ZFGRTOT.
    IF IT_ZSIVIT-GRMENGE LT 0.
      IT_ZSIVIT-GRMENGE = 0.
    ENDIF.
    IF IT_ZSIVIT-GRMENGE GT 0.
      IT_ZSIVIT-UMSON = 'X'.
    ENDIF.

*>> INVOICE ÀÚÀçº° ±Ý¾×.
    IT_ZSIVIT-ZFIVAMT = ( IT_ZSIVIT-CCMENGE *
                        ( IT_ZSIVIT-BPUMZ / IT_ZSIVIT-BPUMN ) *
                        ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ) ).

*>> Ã³¸® ¿øÈ­ ±Ý¾× °è»ê...
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIVIT-ZFIVAMT
                                            IT_ZSIVIT-ZFIVAMC
                                            IT_ZSIVIT-ZFIVAMK.
    IF ZTIV-FFACT IS INITIAL.
      ZTIV-FFACT = 1.
    ENDIF.
    IT_ZSIVIT-ZFIVAMK = ( ZTIV-ZFEXRT / ZTIV-FFACT )
                                      * IT_ZSIVIT-ZFIVAMK.
    PERFORM SET_CURR_CONV_TO_INTERNAL USING
                                      IT_ZSIVIT-ZFIVAMK 'KRW'.
    APPEND   IT_ZSIVIT.
  ENDLOOP.

  DESCRIBE   TABLE IT_ZSIVIT LINES W_LINE.
  IF W_LINE EQ 0.
    MESSAGE E026 WITH ZTREQHD-ZFREQNO.
  ENDIF.

*  REFRESH : IT_ZFREQNO, IT_LOCKED.
*  PERFORM P2000_SET_CIVIT_LOCK USING 'L'.

ENDFORM.                    " P2000_REQ_TO_CC_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_CG_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_CG_DEL_STATUS_CHECK.
* INVOICE Á¸Àç¿©ºÎ Ã¼Å©.
  SELECT COUNT( * ) INTO W_COUNT
                    FROM ZTIVIT
                    WHERE ZFCGNO  EQ   ZTCGHD-ZFCGNO.
  IF W_COUNT > 0.
    MESSAGE E517 WITH 'Loading/Unloading' ZTCGHD-ZFCGNO
                      W_COUNT 'Clearance request Doc'.
  ENDIF.
* MKIM Ãß°¡ FROM ÇÏ¿ªºñ¿ë Ã¼?
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFPSMS = '2'.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTBSEG
     WHERE ZFIMDNO  = ZTCGHD-ZFCGNO
       AND ZFCSTGRP = '007'.
    IF W_COUNT > 0.
      MESSAGE E469.
    ENDIF.
  ENDIF.
* MKIM Ãß°¡ TO
ENDFORM.                    " P2000_CG_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_CC_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_CC_DEL_STATUS_CHECK.

  IF NOT ( ZTIV-ZFCUST EQ '1' OR ZTIV-ZFCUST EQ 'N' ).
    MESSAGE E518 WITH 'Clearance request'
                       ZTIV-ZFIVNO 'Clearance status' ZTIV-ZFCUST.
  ENDIF.

  IF ZTIV-ZFCDST EQ 'Y'.
    MESSAGE E518 WITH 'Clearance request'
               ZTIV-ZFIVNO 'Distribution status' 'Distributed status'.
  ENDIF.

  IF ZTIV-ZFGRST EQ 'Y'.
    MESSAGE E518 WITH 'Clearance request'
                 ZTIV-ZFIVNO 'G/R status' 'G/R status'.
  ENDIF.

  IF ZTIV-ZFCIVST EQ 'Y'.
    MESSAGE E518 WITH 'Clearance request'
                 ZTIV-ZFIVNO 'Expenses status' 'Transaction status'.
  ENDIF.

* INVOICE Á¸Àç¿©ºÎ Ã¼Å©.
  SELECT COUNT( * ) INTO W_COUNT
                    FROM ZTCUCLIV
                    WHERE ZFIVNO  EQ   ZTCGHD-ZFCGNO.
  IF W_COUNT > 0.
    MESSAGE E517 WITH 'Clearance request' ZTIV-ZFIVNO W_COUNT
                      'Import declaration'.
  ENDIF.

*> ÀâÀÌÀÍ Ã³¸® ¿©ºÎ.
  LOOP AT IT_ZSIVIT WHERE NDFTX EQ 'X'.
    ADD 1 TO W_COUNT.
  ENDLOOP.
  IF W_COUNT GT 0.
    MESSAGE E418(ZIM1).
  ENDIF.

* Åë°ü¿äÃ» ºñ¿ë Ã¼?
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFPSMS = '2'.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTBSEG
     WHERE ZFIMDNO  EQ ZTCGHD-ZFCGNO
       AND ZFCSTGRP EQ '006'.
    IF W_COUNT > 0. MESSAGE E469. ENDIF.
  ENDIF.

ENDFORM.                    " P2000_CC_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PAYMENT_INT_SCREEN
*&---------------------------------------------------------------------*
FORM P2000_SET_PAYMENT_INT_SCREEN.

  MOVE 'TRTR' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " AP ¹ÝÁ¦.
  MOVE 'TRTI' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀÌÀÚ¹Ý¿µ.
  MOVE 'TRCC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀÌÀÚÃë¼Ò.
  MOVE 'TRCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¹ÝÁ¦Ãë¼Ò.
  MOVE 'CHIN' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Save & Conf.
  MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Save & Conf.
  MOVE 'SVCO' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Save & Conf.
  MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Save.
  MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »ý?
  MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
  MOVE 'DSRQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¼öÀÔÀÇ?
  MOVE 'DSPO' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " P/O
  MOVE 'DSLC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " L/C
  MOVE 'DSBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " B/L
  MOVE 'DSRE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ÀÎ¼ö?
  MOVE 'DSFI' TO IT_EXCL-FCODE.    APPEND IT_EXCL. "¹ÝÁ¦ÀüÇ¥.
  MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. "¹ÝÁ¦ÀüÇ¥.
  MOVE 'PSHT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. "¹ÝÁ¦ÀüÇ¥.
  MOVE 'PYMT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
  MOVE 'PYCN' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
  MOVE 'DSPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL.

ENDFORM.                    " P2000_SET_PAYMENT_INT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  P2000_TRTR_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_TRTR_MESSAGE.

  IF W_STATUS EQ C_REQ_C OR W_STATUS EQ C_REQ_U
                         OR W_STATUS EQ C_OPEN_U.
    CASE SY-LANGU.
      WHEN '3'.
        PERFORM P2000_MESSAGE_BOX USING 'ÀÚ±Ý ¹Ý¿µ È®ÀÎ'
                       '¸ÕÀú ÀúÀå ÈÄ ¹ÝÁ¦Ã³¸® µË´Ï´Ù.'
                       'ÀúÀå ÈÄ ¹Ý¿µÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                       'Y'             " Ãë¼Ò ¹öÆ° À¯.
                       '1'.            " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
               'Fund update confirm'
               'Do clear after save.'
               'Do you want to clear after save?'
               'Y'
               '1'.
    ENDCASE.

  ELSE.
    CASE SY-LANGU.
      WHEN '3'.
        PERFORM P2000_MESSAGE_BOX USING 'ÀÚ±Ý ¹Ý¿µ È®ÀÎ'
                       '¹ÝÁ¦Ã³¸®µË´Ï´Ù..'
                       '¹Ý¿µÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                       'Y'             " Ãë¼Ò ¹öÆ° À¯.
                       '1'.            " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
               'Fund update confirm'
               'Now do clearing.'
               'Do you want to continue?'
               'Y'
               '1'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_TRTR_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_TRTI_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_TRTI_MESSAGE.

  IF W_STATUS EQ C_REQ_C OR W_STATUS EQ C_REQ_U
                         OR W_STATUS EQ C_OPEN_U.
    CASE SY-LANGU.
      WHEN '3'.
        PERFORM P2000_MESSAGE_BOX USING 'ÀÌÀÚ ÀüÇ¥ »ý¼º È®ÀÎ'
                       '¸ÕÀú ÀúÀå ÈÄ ÀüÇ¥»ý¼º ÇÕ´Ï´Ù.'
                       'ÀúÀå ÈÄ »ý¼ºÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                       'Y'             " Ãë¼Ò ¹öÆ° À¯.
                       '1'.            " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
               'Intr.Acc.Doc. Posting confirm'
               'Post Intr.Acc.Doc after save.'
               'Do you want to post after save?'
               'Y'
               '1'.
    ENDCASE.

  ELSE.
    CASE SY-LANGU.
      WHEN '3'.
        PERFORM P2000_MESSAGE_BOX USING
                'ÀÌÀÚ ÀüÇ¥ »ý¼º È®ÀÎ'
                'ÀÌÀÚÀüÇ¥¸¦ »ý¼ºÇÕ´Ï´Ù.'
                '»ý¼ºÇÏ½Ã°Ú½À´Ï±î?'
                'Y'
                '1'.
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
                'Intr.Acc.Doc. Posting confirm'
                'Post Intr.Acc.Doc.'
                'Do you want to post?'
                'Y'
                '1'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_TRTI_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_EXEC_TR_DOCUMENT
*&---------------------------------------------------------------------*
FORM P2000_EXEC_TR_DOCUMENT.
  W_OK_CODE = OK-CODE.

*>> 2001.04.12 ³ªÇöÁÖ Ãß°¡. ÀÚ±Ý FUNCTION CALL ÀÌÀü CHECK.
  IF ZTPMTHD-ZFPNAMC IS INITIAL.                  " Â÷ÀÔ±Ý¾×.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'ZFPNAMC'.
  ENDIF.
  IF ZTPMTHD-BUKRS  IS INITIAL.                   " È¸»çÄÚµå.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'BUKRS'.
  ENDIF.
*   IF ZTPMTHD-RPORTB IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'RPORTB'.
*   ENDIF.
*   IF ZTPMTHD-ZFSTDT IS INITIAL.                  " ½ÃÀÛÀÏ.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'ZFSTDT'.
*   ENDIF.
*   IF ZTPMTHD-ZFPWDT IS INITIAL.                  " ¸¸±âÀÏ.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTPMTHD' 'ZFPWDT'.
*   ENDIF.
*   IF NOT ( ZTPMTHD-ZFLCKN EQ '2' OR ZTPMTHD-ZFLCKN EQ '3' OR
*            ZTPMTHD-ZFLCKN EQ 'G' ).
*      MESSAGE E557.
*   ENDIF.
  WRITE :  ZTPMTHD-ZFTIVAM TO W_AMTTXT1 CURRENCY ZTPMTHD-ZFKRW.
  W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
  WRITE : ZTPMTHD-ZFPNAM TO W_AMTTXT2 CURRENCY ZTPMTHD-ZFPNAMC.
  W_AMTLEN2 = STRLEN( W_AMTTXT2 ).

  IF ZTPMTHD-ZFTIVAM NE ZTPMTHD-ZFPNAM.
    MESSAGE E558 WITH W_AMTTXT1(W_AMTLEN1) W_AMTTXT2(W_AMTLEN2).
*      MESSAGE E558 WITH ZTPMTHD-ZFTIVAM ZTPMTHD-ZFPNAM.
  ENDIF.

*>> °³¼³ÀºÇà.
  SELECT SINGLE * FROM LFA1
          WHERE  LIFNR  EQ   ZTPMTHD-ZFOPBN.
  IF LFA1-KUNNR IS INITIAL.
    MESSAGE E556 WITH ZTPMTHD-ZFOPBN.
  ENDIF.

*-----------------------------------------------------------------------
* AUTHORITY CHECK
  PERFORM P2000_AUTHORITY_CHECK USING W_OK_CODE.
*-----------------------------------------------------------------------
* Lock Object Set
  IF W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D.
    PERFORM P2000_SET_LOCK.
  ENDIF.

* MESSAGE BOX
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      PERFORM  P3000_CALL_TR_UPDATE.        ">TR ¹Ý¿µ ÇÔ¼ö.
      IF  W_SUBRC EQ 0.
        PERFORM  P3000_DB_MODIFY_SCRCOM.      ">DB MODIFY
        PERFORM  P2000_SET_UNLOCK.            ">UNLOCK.
        PERFORM  P2000_SET_SCREEN_SCRCOM.     ">ÃÊ±âÈ­¸é.
        CLEAR OK-CODE.
        LEAVE SCREEN.
      ELSE.
*            MESSAGE E571.
      ENDIF.
    WHEN 'N'.              " No...
      MESSAGE  S957.
      CLEAR OK-CODE.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_EXEC_TR_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_TR_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_CALL_TR_UPDATE.

  DATA : L_AWKEY   LIKE   BKPF-AWKEY.
  DATA : L_TYPE         TYPE    C,
         L_CHAR10(10)   TYPE    C,
         L_CHAR13(13)   TYPE    C,
         L_CHAR13_1(13) TYPE    C,
         L_AMOUNT       TYPE    VTBFHAPO-BZBETR,
         L_AMOUNT1      TYPE    VTBFHAPO-BZBETR.

  DATA : BEGIN OF IT_ZSBELNR  OCCURS 0,
         BUKRS    LIKE  ZTPMTHD-BUKRS,
         BELNR    LIKE  ZTPMTHD-BELNR,
         GJAHR    LIKE  BKPF-GJAHR,
         BUDAT    LIKE  BKPF-BUDAT,
         END   OF IT_ZSBELNR.

  W_SUBRC = 0.
*>> USANCE or GSM ±¸ºÐ.
  CASE ZTPMTHD-ZFLCKN.
    WHEN '2' OR '3'.      "> USANCE.
      L_TYPE  =  'U'.
    WHEN 'G'.             "> GSM.
      L_TYPE  =  'G'.
    WHEN OTHERS.
      W_SUBRC = 4.        EXIT.
  ENDCASE.

*>> Â÷ÀÔ±Ý¾×.
  L_AMOUNT = ZTPMTHD-ZFPNAM.
*   WRITE : ZTPMTHD-ZFPNAM  CURRENCY  ZTPMTHD-ZFPNAMC  TO  W_TEXT18.
*   PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT18.
*   L_CHAR13 = W_TEXT18.

*>> ÀÌÀÚ±Ý¾×.
  L_AMOUNT1 = ZTPMTHD-ZFUSIT.
*   WRITE : ZTPMTHD-ZFUSIT  CURRENCY  ZTPMTHD-ZFUSITC  TO  W_TEXT18.
*   PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT18.
*   L_CHAR13_1 = W_TEXT18.

*>> ÀÌÀÚÀ².
  WRITE : ZTPMTHD-ZFUSITR          TO  L_CHAR10.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  L_CHAR10.

*>> °³¼³ÀºÇà.
  SELECT SINGLE * FROM LFA1
         WHERE  LIFNR  EQ   ZTPMTHD-ZFOPBN.
  CLEAR : W_AMOUNT.
  LOOP AT IT_ZSPMTIV WHERE ZFGFDNO NE SPACE.
    CLEAR : L_AWKEY.
    MOVE : IT_ZSPMTIV-ZFGFDNO  TO   L_AWKEY(10),
*             ZTPMTHD-BUKRS       TO   L_AWKEY+10(4),
           IT_ZSPMTIV-ZFGFDYR  TO   L_AWKEY+10(4).
    CLEAR : BKPF.
    SELECT * FROM BKPF UP TO 1 ROWS
             WHERE AWKEY  EQ  L_AWKEY.
    ENDSELECT.

    IF SY-SUBRC EQ 0.
      MOVE : BKPF-BUKRS TO    IT_ZSBELNR-BUKRS,
             BKPF-GJAHR TO    IT_ZSBELNR-GJAHR,
             BKPF-BELNR TO    IT_ZSBELNR-BELNR,
             BKPF-BUDAT TO    IT_ZSBELNR-BUDAT.
      ADD  IT_ZSPMTIV-ZFPNAM    TO    W_AMOUNT.
*          APPEND  IT_ZSBELNR.
      COLLECT  IT_ZSBELNR.
    ENDIF.
  ENDLOOP.

  IF W_AMOUNT NE ZTPMTHD-ZFPNAM.
    WRITE : W_AMOUNT TO W_AMTTXT1 CURRENCY ZTPMTHD-ZFPNAMC.
    W_AMTLEN1 = STRLEN( W_AMTTXT1 ).

    WRITE : ZTPMTHD-ZFPNAM TO W_AMTTXT2 CURRENCY ZTPMTHD-ZFPNAMC.
    W_AMTLEN2 = STRLEN( W_AMTTXT2 ).

    MESSAGE E558 WITH W_AMTTXT1(W_AMTLEN1) W_AMTTXT2(W_AMTLEN2).
*       MESSAGE E558 WITH W_AMOUNT ZTPMTHD-ZFPNAM.
  ENDIF.

*>> CALL FUNCTION.
*    CALL FUNCTION 'Z_TR_MM_TRANSFER'
*         EXPORTING
*             BUKRS     =   ZTPMTHD-BUKRS
*             PTYPE     =   L_TYPE
*             WGSCHFT   =   ZTPMTHD-ZFPNAMC
*             RPORTB    =   ZTPMTHD-RPORTB
*             DBLFZ     =   ZTPMTHD-ZFSTDT
*             DELFZ     =   ZTPMTHD-ZFPWDT
*             BZBETR    =   L_AMOUNT
*             BZBETR2   =   L_AMOUNT1
*             PKOND     =   L_CHAR10
*             SRHYTHM   =   ZTPMTHD-SRHYTHM
*             ARHYTM    =   ZTPMTHD-ARHYTM
**             DOWNPAY   =
*             KUNNR     =   LFA1-KUNNR
*             LIFNR     =   LFA1-LIFNR
**             PARTREP   =
**             DELAY     =
**             REPAMT    =
*    IMPORTING
*             RFHA      =   ZTPMTHD-RFHA
*             BELNR_RET =   ZTPMTHD-BELNR
*    TABLES
*             ITBEL     =   IT_ZSBELNR
*    EXCEPTIONS
*             BUKRS_ERR   =  1
*             PTYPE_ERR   =  2
*             WGSCHFT     =  3
*             RPORTB      =  4
*             DBLFZ_ERR   =  5
*             DELFZ_ERR   =  6
*             BZBETR_ERR  =  7
*             PKOND_ERR   =  8
*             SRHYTHM_ERR =  9
*             ARHYTM_ERR  = 10
*             TRXN_ERR    = 11
*             POST_ERR    = 12
*             LIFNR_ERR   = 13
*             GJAHR_ERR   = 14
*             BELNR_ERR   = 15
*             KUNNR_ERR   = 16.
*
*   W_SUBRC = SY-SUBRC.
  W_SUBRC = 4.
  IF W_SUBRC EQ 0.
*>> Á¤»óÀûÀÎ °æ¿ì.
    IF NOT ZTPMTHD-BELNR IS INITIAL OR NOT ZTPMTHD-RFHA IS INITIAL.
      ZTPMTHD-ZFBKCHT = 'Y'.
      ZTPMTHD-UNAM    = SY-UNAME.
      ZTPMTHD-UDAT    = SY-DATUM.
*>> ºñÁ¤»óÀûÀÎ °æ¿ì.
    ELSE.
      W_SUBRC = 99.
    ENDIF.
  ELSE.
*>> ºñÁ¤»óÀûÀÎ °æ¿ì.
    ZTPMTHD-ZFBKCHT = 'Y'.
    ZTPMTHD-UDAT    = SY-DATUM.
    ZTPMTHD-UNAM    = SY-UNAME.
  ENDIF.

ENDFORM.                    " P3000_CALL_TR_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_ITEM_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P2000_ITEM_CHANGE_DOC.

  CASE SY-DYNNR.
    WHEN '0101'.
      CLEAR : W_COUNT.
      LOOP AT IT_ZSBLIT WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSBLIT  TO   ZTBLIT.
        MOVE : ZTBL-ZFBLNO            TO   ZTBLIT-ZFBLNO.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.        MESSAGE S951. EXIT.
        WHEN 1.
        WHEN OTHERS.   MESSAGE S965. EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZIBLIT'.
      OBJEKTID      =   ZTBLIT+3(15).
    WHEN '0811'.     "> ÇÏ¿ª...
      CLEAR : W_COUNT.
      LOOP AT IT_ZSCGIT WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSCGIT  TO   ZTCGIT.
        MOVE : ZTCGHD-ZFCGNO          TO   ZTCGIT-ZFCGNO.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.        MESSAGE S951. EXIT.
        WHEN 1.
        WHEN OTHERS.   MESSAGE S965. EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZICGIT'.
      OBJEKTID      =   ZTCGIT+3(15).
    WHEN '3110'.       "> Åë°ü¿äÃ».
      CLEAR : W_COUNT.
      LOOP AT IT_ZSIVIT WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSIVIT  TO   ZTIVIT.
        MOVE : ZTIV-ZFIVNO            TO   ZTIVIT-ZFIVNO.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.        MESSAGE S951. EXIT.
        WHEN 1.
        WHEN OTHERS.   MESSAGE S965. EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZICCIT'.
      OBJEKTID      =   ZTIVIT+3(15).
    WHEN '3510'.
      CLEAR : W_COUNT.
      LOOP AT IT_ZSCIVIT WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSCIVIT  TO   ZTCIVIT.
        MOVE : ZTCIVHD-ZFCIVRN         TO   ZTCIVIT-ZFCIVRN.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.        MESSAGE S951. EXIT.
        WHEN 1.
        WHEN OTHERS.   MESSAGE S965. EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZICIVIT'.
      OBJEKTID      =   ZTCIVIT+3(15).
    WHEN '7410'.
      W_COUNT = 0.
      LOOP AT IT_ZSIDSHS WHERE ZFMARK NE SPACE.
        MOVE-CORRESPONDING IT_ZSIDSHS TO ZTIDSHS.
        W_COUNT = W_COUNT + 1.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.          MESSAGE  S962.   EXIT.
        WHEN 1.
        WHEN OTHERS.     MESSAGE  S965.   EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZTIDSHS'.
      OBJEKTID      =   ZTIDSHS+3(18).
    WHEN OTHERS.   EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_ITEM_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_COST_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P2000_COST_CHANGE_DOC.
  CASE SY-DYNNR.
    WHEN '0101'.
      CLEAR : W_COUNT.
      LOOP AT IT_ZSBLCST  WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSBLCST   TO   ZTBLCST.
        MOVE : ZTBL-ZFBLNO              TO   ZTBLCST-ZFBLNO.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.        MESSAGE S951. EXIT.
        WHEN 1.
        WHEN OTHERS.   MESSAGE S965. EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZIBLCST'.
      OBJEKTID      =   ZTBLCST+3(15).
    WHEN '0811'.     "> ÇÏ¿ª...
      CLEAR : W_COUNT.
      LOOP AT IT_ZSCGCST WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSCGCST  TO   ZTCGCST.
        MOVE : ZTCGHD-ZFCGNO           TO   ZTCGCST-ZFCGNO.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.        MESSAGE S951. EXIT.
        WHEN 1.
        WHEN OTHERS.   MESSAGE S965. EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZICGCST'.
      OBJEKTID      =   ZTCGCST+3(15).
    WHEN OTHERS.   EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_COST_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_COST_CHANGE_DOC_1
*&---------------------------------------------------------------------*
FORM P2000_COST_CHANGE_DOC_1.
  CASE SY-DYNNR.
    WHEN '0101'.
      CLEAR : W_COUNT.
      LOOP AT IT_ZSBLCST1 WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSBLCST1  TO   ZTBLCST.
        MOVE : ZTBL-ZFBLNO              TO   ZTBLCST-ZFBLNO.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.        MESSAGE S951. EXIT.
        WHEN 1.
        WHEN OTHERS.   MESSAGE S965. EXIT.
      ENDCASE.
      OBJECTCLASS   =   'ZIBLCST'.
      OBJEKTID      =   ZTBLCST+3(15).
    WHEN OTHERS.   EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_COST_CHANGE_DOC_1
*&---------------------------------------------------------------------*
*&      Form  P2000_SELECT_MATERIAL
*&---------------------------------------------------------------------*
FORM P2000_SELECT_MATERIAL.
  CASE SY-DYNNR.
    WHEN '0101'.
      CLEAR : W_COUNT.
      LOOP AT IT_ZSBLIT WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSBLIT  TO   ZTBLIT.
        MOVE : ZTBL-ZFBLNO            TO   ZTBLIT-ZFBLNO.
        MOVE-CORRESPONDING IT_ZSBLIT  TO   EKPO.
        MOVE : ZTBL-BUKRS             TO   EKPO-BUKRS.
      ENDLOOP.
      MOVE : ZTBL-LIFNR    TO    EKKO-LIFNR.
    WHEN '0811'.     "> ÇÏ¿ª...
      CLEAR : W_COUNT.
      LOOP AT IT_ZSCGIT WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSCGIT  TO   ZSCGIT.
        MOVE : ZTCGHD-ZFCGNO          TO   ZSCGIT-ZFCGNO.
        MOVE-CORRESPONDING IT_ZSCGIT  TO   EKPO.
        MOVE : ZTCGHD-BUKRS           TO   EKPO-BUKRS.
      ENDLOOP.
    WHEN '3110'.       "> Åë°ü¿äÃ».
      CLEAR : W_COUNT.
      LOOP AT IT_ZSIVIT WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSIVIT  TO   ZTIVIT.
        MOVE : ZTIV-ZFIVNO            TO   ZTIVIT-ZFIVNO.
        MOVE-CORRESPONDING IT_ZSIVIT  TO   EKPO.
        MOVE : ZTIV-BUKRS             TO   EKPO-BUKRS.
      ENDLOOP.
    WHEN '3510'.             "CIV.
      CLEAR : W_COUNT.
      LOOP AT IT_ZSCIVIT WHERE ZFMARK = 'X'.
        ADD 1   TO   W_COUNT.
        MOVE-CORRESPONDING IT_ZSCIVIT  TO   ZSCIVIT.
        MOVE : ZTCIVHD-ZFCIVRN         TO   ZSCIVIT-ZFCIVRN.
        MOVE-CORRESPONDING IT_ZSCIVIT  TO   EKPO.
        MOVE : ZTCIVHD-BUKRS           TO   EKPO-BUKRS.
      ENDLOOP.
      MOVE : ZTBL-LIFNR    TO    EKKO-LIFNR.
    WHEN OTHERS.
      CLEAR : W_COUNT.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_SELECT_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_MATERIAL_DISPLAY.

  CASE W_COUNT.
    WHEN 0.        MESSAGE S951. EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965. EXIT.
  ENDCASE.

  SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
  SET PARAMETER ID 'BUK' FIELD EKPO-BUKRS.
  SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
  SET PARAMETER ID 'LAG' FIELD ''.
  SET PARAMETER ID 'MXX' FIELD MM03_START_SICHT.
  CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.



ENDFORM.                    " P2000_MATERIAL_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_MD04
*&---------------------------------------------------------------------*
FORM P2000_MATERIAL_MD04.

  CASE W_COUNT.
    WHEN 0.        MESSAGE S951. EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965. EXIT.
  ENDCASE.

  SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
  SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
  SET PARAMETER ID 'BERID' FIELD ''.
  CALL TRANSACTION 'MD04' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_MATERIAL_MD04
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_MMBE
*&---------------------------------------------------------------------*
FORM P2000_MATERIAL_MMBE.

  CASE W_COUNT.
    WHEN 0.        MESSAGE S951. EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965. EXIT.
  ENDCASE.

  SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
  SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
  SET PARAMETER ID 'LAG' FIELD ''.          " ÀúÀåÀ§?
  SET PARAMETER ID 'CHA' FIELD ''.          " Batch ¹ø?
  CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_MATERIAL_MMBE
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_MB51
*&---------------------------------------------------------------------*
FORM P2000_MATERIAL_MB51.

  CASE W_COUNT.
    WHEN 0.        MESSAGE S951. EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965. EXIT.
  ENDCASE.

  SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
  SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
  SET PARAMETER ID 'BWA' FIELD ''.               " ÀÚÀçÀÌµ¿ À¯Çü.
  SET PARAMETER ID 'LIF' FIELD EKKO-LIFNR.       " °Å·¡Ã³ÄÚµå.
  SET PARAMETER ID 'LAG' FIELD ''.               " ÀúÀåÀ§Ä¡.
  SET PARAMETER ID 'CHA' FIELD ''.               " Batch ¹øÈ£.
  CALL TRANSACTION 'MB51' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_MATERIAL_MB51
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_ME2M
*&---------------------------------------------------------------------*
FORM P2000_MATERIAL_ME2M.

  CASE W_COUNT.
    WHEN 0.        MESSAGE S951. EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965. EXIT.
  ENDCASE.

  SUBMIT RM06EM00
        WITH EM_MATNR EQ EKPO-MATNR
        WITH EM_WERKS EQ EKPO-WERKS
        WITH LISTU    EQ 'BEST'
        WITH SELPA    EQ 'WE101'         " ¸Å°³º¯?
        AND RETURN.

ENDFORM.                    " P2000_MATERIAL_ME2M
*&---------------------------------------------------------------------*
*&      Form  P2000_MATERIAL_ME03
*&---------------------------------------------------------------------*
FORM P2000_MATERIAL_ME03.

  CASE W_COUNT.
    WHEN 0.        MESSAGE S951. EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965. EXIT.
  ENDCASE.

  SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
  SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
  CALL TRANSACTION 'ME03' AND SKIP FIRST SCREEN.


ENDFORM.                    " P2000_MATERIAL_ME03
*&---------------------------------------------------------------------*
*&      Form  P2000_LG_PRINT
*&---------------------------------------------------------------------*
FORM P2000_LG_PRINT.

  DATA: SELTAB     TYPE TABLE OF RSPARAMS,
        SELTAB_WA  LIKE LINE OF SELTAB.

  MOVE: 'S_ZFBLNO'     TO SELTAB_WA-SELNAME,
        'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
        'I'            TO SELTAB_WA-SIGN,
        'EQ'           TO SELTAB_WA-OPTION,
        ZTLG-ZFBLNO    TO SELTAB_WA-LOW,
        SPACE          TO SELTAB_WA-HIGH.
  APPEND SELTAB_WA  TO SELTAB.
  MOVE: 'S_LGSEQ'      TO SELTAB_WA-SELNAME,
        'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
        'I'            TO SELTAB_WA-SIGN,
        'EQ'           TO SELTAB_WA-OPTION,
        ZTLG-ZFLGSEQ   TO SELTAB_WA-LOW,
        SPACE          TO SELTAB_WA-HIGH.
  APPEND SELTAB_WA TO SELTAB.

  SUBMIT ZRIMLGPRT WITH P_ZFBLNO EQ ZTLG-ZFBLNO
                   WITH P_LGSEQ  EQ ZTLG-ZFLGSEQ
                   AND RETURN. " ´Ù½Ã µ¹¾Æ ¿Ã¼ö ÀÖ°Ô.

ENDFORM.                    " P2000_LG_PRINT
*&---------------------------------------------------------------------*
*&      Form  GET_MATHER_SHIP_NO
*&---------------------------------------------------------------------*
FORM GET_MATHER_SHIP_NO.
  SELECT SINGLE *
         FROM   ZTMSHD
         WHERE  ZFMSNO EQ ZTBL-ZFMSNO.
  IF ZTMSHD-BUKRS NE ZTBL-BUKRS.
    MESSAGE E540 WITH ZTBL-BUKRS ZTMSHD-BUKRS.
  ENDIF.
  IF ZTBL-ZFCARNM IS INITIAL.
    MOVE ZTMSHD-ZFMSNM   TO   ZTBL-ZFCARNM.
  ENDIF.
  IF ZTBL-ZFETD IS INITIAL.
    MOVE ZTMSHD-ZFSHSDT  TO   ZTBL-ZFETD.
  ENDIF.
  IF ZTBL-ZFETA IS INITIAL.
    MOVE ZTMSHD-ZFETA    TO   ZTMSHD-ZFETA.
  ENDIF.
  IF ZTBL-ZFCARC  IS INITIAL.
    SELECT MAX( LAND1 ) INTO ZTBL-ZFCARC
           FROM  ZTIEPORT
           WHERE PORT EQ ZTMSHD-ZFSPRTC.
*        SELECT ZFCD2 INTO ZTBL-ZFCARC
*                 FROM ZTIMIMG08
*                 WHERE ZFCDTY  EQ  '002'
*                 AND   ZFCD    EQ  ZTMSHD-ZFSPRTC.
*        ENDSELECT.
  ENDIF.
  IF ZTBL-ZFSPRTC IS INITIAL.
    MOVE ZTMSHD-ZFSPRTC  TO   ZTBL-ZFSPRTC.
  ENDIF.
  IF ZTBL-ZFAPRTC IS INITIAL.
    MOVE ZTMSHD-ZFAPRTC  TO   ZTBL-ZFAPRTC.
  ENDIF.

ENDFORM.                    " GET_MATHER_SHIP_NO
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_CUT
*&---------------------------------------------------------------------*
FORM P1000_GET_CUT USING    P_ZFCUT
                   CHANGING ZSIMIMG10_NAME1.

  SELECT SINGLE * FROM ZTIMIMG10
         WHERE    ZFCUT  EQ  P_ZFCUT.
  IF SY-SUBRC NE 0.
    MESSAGE E768 WITH P_ZFCUT.
    EXIT.
  ENDIF.

  CLEAR : ZSIMIMG10_NAME1.
  SELECT SINGLE NAME1 INTO ZSIMIMG10_NAME1
         FROM   LFA1
         WHERE  LIFNR   EQ   ZTIMIMG10-ZFVEN.

ENDFORM.                    " P1000_GET_CUT
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_BLCST_COST_SEQ
*&---------------------------------------------------------------------*
FORM P1000_GET_BLCST_COST_SEQ.

ENDFORM.                    " P1000_GET_BLCST_COST_SEQ
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
FORM P2000_ALV_COMMAND USING R_UCOMM      LIKE SY-UCOMM
                              RS_SELFIELD TYPE SLIS_SELFIELD.
*
  CASE R_UCOMM.
    WHEN  'WAHL'.             " ºñ¿ë¹®¼­ & doubleclick
      IF SY-TCODE(4) EQ 'ZIMP'.
        READ TABLE IT_ZSPMTHST INDEX RS_SELFIELD-TABINDEX. "
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_FI_DOCUMENT_DISPLAY
                                 USING   IT_ZSPMTHST-BUKRS
                                         IT_ZSPMTHST-GJAHR
                                         IT_ZSPMTHST-BELNR.
        ELSE.
          MESSAGE S962.
        ENDIF.

      ELSE.
        READ TABLE IT_ZSIMCOST INDEX RS_SELFIELD-TABINDEX. "
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_FI_DOCUMENT_DISPLAY
                                 USING   IT_ZSIMCOST-BUKRS
                                         IT_ZSIMCOST-ZFFIYR
                                         IT_ZSIMCOST-ZFACDO.
        ELSE.
          MESSAGE S962.
        ENDIF.
      ENDIF.
      CLEAR R_UCOMM.
    WHEN  'DSPY'.             " ºñ¿ë¹®¼­ & doubleclick
      IF SY-TCODE(4) EQ 'ZIMP'.
        READ TABLE IT_ZSPMTHST INDEX RS_SELFIELD-TABINDEX. "
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_FI_DOCUMENT_DISPLAY
                                 USING   IT_ZSPMTHST-BUKRS
                                         IT_ZSPMTHST-ZFPMYR
                                         IT_ZSPMTHST-ZFPMNO.
        ELSE.
          MESSAGE S962.
        ENDIF.
      ENDIF.
      CLEAR R_UCOMM.
    WHEN 'WAH2' OR '&IC1'.
      IF SY-TCODE(4) EQ 'ZIMP'.
        READ TABLE IT_ZSPMTHST INDEX RS_SELFIELD-TABINDEX. "
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_FI_DOCUMENT_DISPLAY
                                 USING   IT_ZSPMTHST-BUKRS
                                         IT_ZSPMTHST-GJAHR
                                         IT_ZSPMTHST-BELNR.
        ELSE.
          MESSAGE S962.
        ENDIF.
      ELSE.
        READ TABLE IT_ZSIMCOST INDEX RS_SELFIELD-TABINDEX.
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_COST_DOCUMENT_DISPLAY
                                 USING   IT_ZSIMCOST-BUKRS
                                         IT_ZSIMCOST-GJAHR
                                         IT_ZSIMCOST-BELNR.
        ELSE.
          MESSAGE S962.
        ENDIF.
      ENDIF.
      CLEAR R_UCOMM.
  ENDCASE.

ENDFORM.                    " P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_ALV_PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  IF SY-TCODE(4) EQ 'ZIMP'.
    SET PF-STATUS 'STANDA01' EXCLUDING EXTAB.
  ELSE.
    SET PF-STATUS 'STANDA02' EXCLUDING EXTAB.
  ENDIF.

ENDFORM.                    " P2000_ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  P2000_COST_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_COST_DOCUMENT_DISPLAY USING    P_BUKRS
                                          P_GJAHR
                                          P_BELNR.
  IF P_BELNR IS INITIAL.
    MESSAGE S167 WITH 'Import expense Doc'.   EXIT.
  ELSE.
    SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
    SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
    SET PARAMETER ID 'ZPBENR' FIELD P_BELNR.
    CALL TRANSACTION 'ZIMY3' AND SKIP  FIRST SCREEN.

*>> ºñ¿ëDOCUMENT SELECT.
    IF SY-TCODE EQ 'ZIM21' OR SY-TCODE EQ 'ZIM22' OR
       SY-TCODE EQ 'ZIM23' OR SY-TCODE EQ 'ZIM221' OR
       SY-TCODE EQ 'ZIM22' OR SY-TCODE EQ 'ZIM223'.

      W_ZFIMDNO = ZTBL-ZFBLNO.
      CALL FUNCTION 'ZIM_GET_COST_DOCUMENT'
           EXPORTING
                ZFCSTGRP    = '004'
                ZFCSTGRP1   = '005'
                ZFIMDNO     = W_ZFIMDNO
           TABLES
                IT_ZSIMCOST = IT_ZSIMCOST.

* B/L ºñ¿ë( ÇØ¿Ü¿î¼Ûºñ )
      REFRESH : IT_ZSBLCST.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLCST
               FROM ZTBLCST
               WHERE ZFBLNO EQ ZTBL-ZFBLNO
               AND   ZFCSQ  >=  '10000'.

      LOOP AT IT_ZSBLCST.
        W_TABIX = SY-TABIX.
        SELECT SINGLE ZFCDNM INTO IT_ZSBLCST-ZFCDNM FROM ZTIMIMG08
                      WHERE ZFCDTY EQ '004'
                      AND   ZFCD   EQ IT_ZSBLCST-ZFCSCD.
        MODIFY IT_ZSBLCST INDEX W_TABIX.
      ENDLOOP.
      IT_ZSBLCST_ORG[] = IT_ZSBLCST[].

* B/L ºñ¿ë ( º¸¼¼¿î¼Ûºñ )
      REFRESH : IT_ZSBLCST1.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLCST1
               FROM  ZTBLCST
               WHERE ZFBLNO EQ ZTBL-ZFBLNO
               AND   ZFCSQ  <   '10000'.

      LOOP AT IT_ZSBLCST1.
        W_TABIX = SY-TABIX.
        SELECT SINGLE ZFCDNM INTO IT_ZSBLCST1-ZFCDNM FROM ZTIMIMG08
                             WHERE ZFCDTY EQ '005'
                             AND   ZFCD   EQ IT_ZSBLCST1-ZFCSCD.
        MODIFY IT_ZSBLCST1 INDEX W_TABIX.
      ENDLOOP.
      IT_ZSBLCST1_ORG[] = IT_ZSBLCST1[].
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_COST_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_FI_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_FI_DOCUMENT_DISPLAY USING    P_BUKRS
                                        P_GJAHR
                                        P_BELNR.
  IF P_BELNR IS INITIAL.
    MESSAGE S588.   EXIT.
  ELSE.
*>>> LIV ÀüÇ¥¹øÈ£ÀÎÁö, È¸°èÀüÇ¥ÀÎÁö¸¦ ±¸ºÐ.
    SELECT * FROM EKBZ UP TO 1 ROWS
             WHERE BELNR EQ P_BELNR
             AND   GJAHR EQ P_GJAHR.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      SELECT * FROM EKBE UP TO 1 ROWS
               WHERE BELNR EQ P_BELNR
               AND   GJAHR EQ P_GJAHR.
      ENDSELECT.
    ENDIF.
    IF SY-SUBRC EQ 0.
      SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
      SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
      SET PARAMETER ID 'RBN'    FIELD P_BELNR.
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
    ELSE.
      SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
      SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
      SET PARAMETER ID 'BLN'    FIELD P_BELNR.
      CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_FI_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_COST_SCREEN
*&---------------------------------------------------------------------*
FORM P2000_CALL_COST_SCREEN.

  DESCRIBE  TABLE  IT_ZSIMCOST  LINES  W_LINE.
  IF W_LINE GT 0.
    IF SY-TCODE EQ 'ZIM21'  OR SY-TCODE EQ 'ZIM22' OR
       SY-TCODE EQ 'ZIM23'  OR
       SY-TCODE EQ 'ZIM221' OR SY-TCODE EQ 'ZIM222' OR
       SY-TCODE EQ 'ZIM223'.
      PERFORM P2000_MODIFY_CHECK USING W_MODIF_BIT.
      IF W_MODIF_BIT EQ 'Y'.
        PERFORM P2000_SET_MESSAGE USING  'ANZG'.
      ELSE.
        ANTWORT = 'Y'.
      ENDIF.

      CASE ANTWORT.
        WHEN 'Y'.
          PERFORM  P3000_DB_MODIFY_SCRCOM.
      ENDCASE.
    ENDIF.

    W_DYNNR  =  SY-DYNNR.
    SET SCREEN 0050.   LEAVE TO SCREEN 0050.
  ELSE.
    MESSAGE S608.
  ENDIF.

ENDFORM.                    " P2000_CALL_COST_SCREEN
*&---------------------------------------------------------------------*
*&      Form  P2000_EXEC_GOODS_RECEIPT
*&---------------------------------------------------------------------*
FORM P2000_EXEC_GOODS_RECEIPT.

  W_OK_CODE = OK-CODE.
*-----------------------------------------------------------------------
* AUTHORITY CHECK
  PERFORM P2000_AUTHORITY_CHECK USING W_OK_CODE.
*-----------------------------------------------------------------------
* Lock Object Set
  IF W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D.
    PERFORM P2000_SET_LOCK.
  ENDIF.

* MESSAGE BOX
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      IF W_STATUS NE C_REQ_D.
        PERFORM  P3000_DB_MODIFY_SCRCOM.
      ENDIF.
      PERFORM  P3000_CALL_GOOD_RECEIPT
               USING ZTIV-ZFIVNO.

      PERFORM  P2000_SET_UNLOCK.
*         PERFORM  P2000_SET_SCREEN_SCRCOM.
      CLEAR OK-CODE.
      IF W_ERR_CHK EQ 'N'.
        MESSAGE S012(MIGO) WITH MATERIALDOCUMENT.
      ENDIF.

      DESCRIBE  TABLE IT_ERR_LIST   LINES  W_LINE.
      IF W_LINE GT 0.
        INCLUDE = 'POPU'.
        CALL SCREEN 0015 STARTING AT  01   3
                         ENDING   AT  107 12.
        CLEAR : INCLUDE.
      ENDIF.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
    WHEN 'N'.              " No...
      MESSAGE  S957.
      CLEAR OK-CODE.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_EXEC_GOODS_RECEIPT
*&---------------------------------------------------------------------*
*&      Form  P2000_MIGO_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_MIGO_MESSAGE.
  DATA : L_MVTP LIKE ZTIMIMG00-ZFMVTY1.

  IF ZTIV-ZFGRST EQ 'Y'.
    MESSAGE E500 WITH ZTIV-ZFIVNO.
  ENDIF.

  IF ZTIV-ZFGRST EQ 'X'.
    MESSAGE E501 WITH ZTIV-ZFIVNO.
  ENDIF.

  SELECT MAX( ZFIVHST ) INTO ZTIVHST-ZFIVHST
         FROM ZTIVHST
         WHERE ZFIVNO   EQ   ZTIV-ZFIVNO.
  ADD  1  TO  ZTIVHST-ZFIVHST.

  MOVE: ZTIV-ZFIVNO TO ZTIVHST-ZFIVNO.

*> ÀÚÀç³»¿ª SELECT..
  IF W_STATUS EQ C_REQ_C.
    REFRESH : IT_ZSIVHSTIT.
    LOOP AT IT_ZSIVIT WHERE UMSON  EQ 'X'.
      CLEAR : IT_ZSIVHSTIT.
      MOVE-CORRESPONDING IT_ZSIVIT TO IT_ZSIVHSTIT.
      APPEND IT_ZSIVHSTIT.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      MESSAGE E549.
    ENDIF.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVHSTIT
             FROM  ZTIVIT
             WHERE ZFIVNO EQ ZTIV-ZFIVNO
             AND   UMSON  EQ 'X'.
    IF SY-SUBRC NE 0.
      MESSAGE E549.
    ENDIF.
  ENDIF.
*>
  LOOP AT IT_ZSIVHSTIT.
    W_TABIX = SY-TABIX.
    MOVE:  IT_ZSIVHSTIT-GRMENGE   TO   IT_ZSIVHSTIT-CCMENGE,
           ZTIVHST-ZFIVHST        TO   IT_ZSIVHSTIT-ZFIVHST.

    IT_ZSIVHSTIT-GRMENGE = IT_ZSIVHSTIT-CCMENGE
                         - IT_ZSIVHSTIT-GRTOTMN.

    IF IT_ZSIVHSTIT-GRMENGE GT 0.
      IT_ZSIVHSTIT-UMSON = 'X'.
    ELSE.
      CLEAR : IT_ZSIVHSTIT-UMSON.
    ENDIF.
    IT_ZSIVHSTIT-ZFGRST = 'Y'.

    MODIFY IT_ZSIVHSTIT INDEX W_TABIX.
  ENDLOOP.

*MODIFIED BY SEUNGYEON(2002.08.30)
  IF ZTIV-ZFCUST EQ 'Y'.
    SELECT  MAX( ZFIDSDT )  INTO  W_ZFIDSDT
    FROM    ZTIDS
*                AS  H  INNER  JOIN  ZTCUCLIV  AS  I
*     ON      H~ZFBLNO   EQ  I~ZFBLNO
*     AND     H~ZFCLSEQ  EQ  I~ZFCLSEQ
    WHERE   ZFIVNO   EQ  ZTIV-ZFIVNO.
  ENDIF.

  CASE SY-LANGU.
    WHEN '3'.
      MOVE 'Àü±âµ¥ÀÌÅ¸ ÀÔ·Â¿ë' TO SPOP-TITEL.
    WHEN OTHERS.
      MOVE 'Posting data'      TO SPOP-TITEL.
  ENDCASE.

  MOVE : ZTIV-ZFCCDT  TO ZTIVHST-BUDAT.  "ÀüÇ¥Àü±â?

  IF W_ZFIDSDT IS INITIAL.
    MOVE SY-DATUM    TO ZTIVHST-BLDAT.  "ÁõºùÀÏ.
  ELSE.
    MOVE W_ZFIDSDT   TO ZTIVHST-BLDAT.  "ÁõºùÀÏ.
  ENDIF.

  IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'Y'. "¹«È¯¼öÃâÀÔ.
    MOVE ZTIMIMG00-ZFMVTY2  TO ZTIVHST-BWART.
  ENDIF.
*> ¹«È¯.
  IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'N'. "¹«È¯.
    MOVE ZTIMIMG00-ZFMVTY3  TO ZTIVHST-BWART.
  ENDIF.
*> À¯È¯.
  IF ZTIV-ZFPOYN = 'Y'.
    MOVE ZTIMIMG00-ZFMVTY1  TO ZTIVHST-BWART.
  ENDIF.

  IF W_STATUS EQ C_REQ_C OR W_STATUS EQ C_REQ_U
                          OR W_STATUS EQ C_OPEN_U.
    CASE SY-LANGU.
      WHEN '3'.
        PERFORM P2000_MESSAGE_BOX USING 'ÀÔ°í È®ÀÎ'
                       '¸ÕÀú ÀúÀå ÈÄ ÀÔ°í Àü±âÇØ¾ß ÇÕ´Ï´Ù.'
                       'ÀúÀå ÈÄ ÀÔ°íÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                       'Y'             " Ãë¼Ò ¹öÆ° À¯.
                       '1'.            " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
           'G/R confirm'
           'You should do G/R posting after save.'
           'Do you want to G/R after save?'
           'Y'
           '1'.
    ENDCASE.

  ELSE.
    CASE SY-LANGU.
      WHEN '3'.
        PERFORM P2000_MESSAGE_BOX USING 'ÀÔ°í È®ÀÎ'
                       'ÀÔ°í Àü±âµ¥ÀÌÅ¸¸¦ ÀÔ·ÂÇÏ¼¼¿ä.'
                       'ÀÔ°í Àü±âÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                       'Y'             " Ãë¼Ò ¹öÆ° À¯.
                       '1'.            " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
           'G/R confirm'
           'Enter data for G/R posting.'
           'Do you want to do G/R posting?'
           'Y'
           '1'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_MIGO_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_GOOD_RECEIPT
*&---------------------------------------------------------------------*
FORM P3000_CALL_GOOD_RECEIPT USING    P_ZFIVNO.

  W_ERR_CHK = 'N'.
  REFRESH : IT_ERR_LIST.  CLEAR : IT_ERR_LIST.
**> ÀÚÀç³»¿ª SELECT..
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVHSTIT
*           FROM  ZTIVIT
*           WHERE ZFIVNO EQ P_ZFIVNO
*           AND   UMSON  EQ 'X'.
*
*  IF SY-SUBRC NE 0.
*     MESSAGE E549 WITH ZTIV-ZFIVNO.
*  ENDIF.
**>
*  LOOP AT IT_ZSIVHSTIT.
*     W_TABIX = SY-TABIX.
*     MOVE:  IT_ZSIVHSTIT-GRMENGE   TO   IT_ZSIVHSTIT-CCMENGE,
*            ZTIVHST-ZFIVHST        TO   IT_ZSIVHSTIT-ZFIVHST.
*
*     IT_ZSIVHSTIT-GRMENGE = IT_ZSIVHSTIT-CCMENGE
*                          - IT_ZSIVHSTIT-GRTOTMN.
*
*     IF IT_ZSIVHSTIT-GRMENGE GT 0.
*        IT_ZSIVHSTIT-UMSON = 'X'.
*     ELSE.
*        CLEAR : IT_ZSIVHSTIT-UMSON.
*     ENDIF.
*     MODIFY IT_ZSIVHSTIT INDEX W_TABIX.
*  ENDLOOP.

*>> ÀÔ°í BAPIs FUCNTION
  CALL FUNCTION 'ZIM_BAPI_GOODSMVT_CREATE'
       EXPORTING
            P_ZFIVNO         = P_ZFIVNO
            P_CHG_MODE       = 'X'
            P_MVT_TYPE       = ZTIVHST-BWART
            P_BLDAT          = ZTIVHST-BLDAT
            P_BUDAT          = ZTIVHST-BUDAT
       IMPORTING
            MATERIALDOCUMENT = MATERIALDOCUMENT
            MATDOCUMENTYEAR  = MATDOCUMENTYEAR
       TABLES
            IT_ZSIVHSTIT     = IT_ZSIVHSTIT
            RETURN           = RETURN
       EXCEPTIONS
            MVT_ERROR        = 4.

  IF SY-SUBRC NE 0.           ">> ¿À·ù ¹ß»ý½Ã...
    ROLLBACK WORK.
    IF RETURN[] IS INITIAL.
      PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
    ELSE.
      PERFORM  P2000_MULTI_MSG_MAKE1 TABLES  IT_ERR_LIST.
    ENDIF.
    W_ERR_CHK = 'Y'.
  ELSE.
    COMMIT WORK.
    MESSAGE S012(MIGO) WITH MATERIALDOCUMENT.
    PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
  ENDIF.

ENDFORM.                    " P3000_CALL_GOOD_RECEIPT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST.

  MOVE : SY-MSGTY            TO     IT_ERR_LIST-MSGTYP,
         SY-MSGID            TO     IT_ERR_LIST-MSGID,
         SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
         SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
         SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
         SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
         SY-MSGV4            TO     IT_ERR_LIST-MSGV4,
         ZTIV-ZFIVNO         TO     IT_ERR_LIST-ZFIVNO,
         IT_ZSBKPF-ZFSEQ     TO     IT_ERR_LIST-ZFSEQ.


  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = IT_ERR_LIST-MSGID
            MSGNR               = IT_ERR_LIST-MSGNR
            MSGV1               = IT_ERR_LIST-MSGV1
            MSGV2               = IT_ERR_LIST-MSGV2
            MSGV3               = IT_ERR_LIST-MSGV3
            MSGV4               = IT_ERR_LIST-MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = IT_ERR_LIST-MESSTXT.

  CASE IT_ERR_LIST-MSGTYP.
    WHEN 'E' OR 'A'.
      MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
    WHEN 'I'.
      MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
    WHEN 'S'.
      MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
    WHEN 'W'.
      MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
  ENDCASE.

  APPEND  IT_ERR_LIST.

ENDFORM.                    " P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE1
*&---------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE1 TABLES   IT_ERR_LIST STRUCTURE IT_ERR_LIST.

  LOOP AT  RETURN.

    MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
           RETURN-ID           TO     IT_ERR_LIST-MSGID,
           RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
           RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
           RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
           RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
           RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
           RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT,
           ZTIV-ZFIVNO         TO     IT_ERR_LIST-ZFIVNO.

    CASE IT_ERR_LIST-MSGTYP.
      WHEN 'E' OR 'A'.
        MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
      WHEN 'I'.
        MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'S'.
        MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'W'.
        MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
    ENDCASE.

    APPEND  IT_ERR_LIST.

  ENDLOOP.

ENDFORM.                    " P2000_MULTI_MSG_MAKE1
*&---------------------------------------------------------------------*
*&      Form  P2000_MIGO_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_MIGO_CANCEL_MESSAGE.

  DATA : WL_TEXT1(20), WL_TEXT2(20), WL_TEXT3(40).
  CASE SY-LANGU.
    WHEN '3'.
      WL_TEXT1 = 'ÀÔ°í´ë»ó'.
      WL_TEXT2 = '¹ÌÀÔ°í´ë»ó'.
      WL_TEXT3 = 'ÀÔ°íÃë¼Ò Àü±âÀÏÀÚ'.
    WHEN OTHERS.
      WL_TEXT1 = 'Object to G/R'.
      WL_TEXT2 = 'Not Object to G/R'.
      WL_TEXT3 = 'Posting date of G/R cancelation'.
  ENDCASE.

  CASE ZTIV-ZFGRST.
    WHEN 'N'.   MESSAGE E648 WITH ZTIV-ZFIVNO WL_TEXT1.
    WHEN 'X'.   MESSAGE E648 WITH ZTIV-ZFIVNO WL_TEXT2.
    WHEN OTHERS.
  ENDCASE.

  MOVE : SY-DATUM  TO *ZTIVHST-BUDAT.  "ÀüÇ¥Àü±â?
  CLEAR : ZTIVHST.
  MOVE WL_TEXT3 TO SPOP-TITEL.
*  MOVE 'X'            TO RADIO_NONE.
  IF *ZTIVHST-BUDAT IS INITIAL.
    MOVE SY-DATUM    TO *ZTIVHST-BUDAT.
  ENDIF.

  IF ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' OR
     ZTIMIMG00-GRPARTX NE 'X'.
    CLEAR : ZTIVHST.
    SELECT SINGLE * FROM ZTIVHST
           WHERE ZFIVNO  EQ  ZTIV-ZFIVNO
           AND   ZFIVHST EQ  ( SELECT MAX( ZFIVHST )
                                   FROM  ZTIVHST
                                   WHERE ZFIVNO   EQ  ZTIV-ZFIVNO
                                   AND ( CMBLNR   IS  NULL
                                   OR    CMBLNR   EQ  SPACE ) ).
    IF SY-SUBRC NE 0.
      MESSAGE E647.
    ENDIF.
  ELSE.
    SELECT COUNT( * ) INTO W_COUNT
                      FROM ZTIVHST
                      WHERE ZFIVNO  EQ  ZTIV-ZFIVNO
                      AND ( CMBLNR  EQ  SPACE
                      OR    CMBLNR  IS  NULL ).

    CASE W_COUNT.
      WHEN 1.
        SELECT SINGLE * FROM  ZTIVHST
                        WHERE ZFIVNO  EQ  ZTIV-ZFIVNO
                        AND ( CMBLNR  EQ  SPACE
                        OR    CMBLNR  IS  NULL ).
        IF SY-SUBRC NE 0.
          MESSAGE E647.
        ENDIF.
      WHEN 0.
        MESSAGE E647.
      WHEN OTHERS.
        REFRESH : IT_ZSIVHST.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVHST
                 FROM ZTIVHST
                 WHERE ZFIVNO  EQ  ZTIV-ZFIVNO
                 AND ( CMBLNR  EQ  SPACE
                 OR    CMBLNR  IS  NULL ).
    ENDCASE.
  ENDIF.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING 'ÀÔ°íÃë¼Ò È®ÀÎ'
                         'ÀÔ°íÃë¼Ò Àü±â ÇÕ´Ï´Ù.'
                         'Ãë¼ÒÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                         'N'             " Ãë¼Ò ¹öÆ° À¯.
                         '1'.            " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'G/R cancel confirm'
             'Now do reverse posting.'
             'Do you want to do reverse posting?'
             'N'
             '1'.
  ENDCASE.
ENDFORM.                    " P2000_MIGO_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_EXEC_GR_CANCLE
*&---------------------------------------------------------------------*
FORM P2000_EXEC_GR_CANCLE.
  W_OK_CODE = OK-CODE.

*>> Ãë¼Ò¹®¼­ ¿©ºÎ Ã¼Å©.
  IF NOT ( ZTIV-ZFGRST EQ 'Y' OR ZTIV-ZFGRST EQ 'P' ).
    MESSAGE E422 WITH ZTIV-ZFIVNO ZTIV-ZFGRST 'G/R cancel'.
  ENDIF.

*-----------------------------------------------------------------------
* AUTHORITY CHECK
  PERFORM P2000_AUTHORITY_CHECK USING W_OK_CODE.
*-----------------------------------------------------------------------
* Lock Object Set
  IF W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D.
    PERFORM P2000_SET_LOCK.
  ENDIF.

* MESSAGE BOX
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      PERFORM  P3000_CALL_GR_DOC_CANCEL
               USING ZTIV-ZFIVNO.

      PERFORM  P2000_SET_UNLOCK.

      DESCRIBE  TABLE IT_ERR_LIST   LINES  W_LINE.
      IF W_LINE GT 0.
        INCLUDE = 'POPU'.
        CALL SCREEN 0015 STARTING AT  01   3
                         ENDING   AT  107 12.
        CLEAR : INCLUDE.
      ENDIF.
      IF W_SUBRC EQ 0.
        MESSAGE  S922.
      ENDIF.
      CLEAR OK-CODE.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
    WHEN 'N'.              " No...
      MESSAGE  S957.
      CLEAR OK-CODE.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_EXEC_GR_CANCLE
*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_GR_DOC_CANCEL
*&---------------------------------------------------------------------*
FORM P3000_CALL_GR_DOC_CANCEL USING    P_ZFIVNO.

  DATA : L_CBUDAT   LIKE   ZTIVHST-CBUDAT.

  W_ERR_CHK = 'N'.
  L_CBUDAT    = *ZTIVHST-BUDAT.
  REFRESH : IT_ERR_LIST.  CLEAR : IT_ERR_LIST.

*   CLEAR : ZTIVHST.
*   SELECT SINGLE * FROM ZTIVHST
*          WHERE ZFIVNO  EQ  P_ZFIVNO
*          AND   ZFIVHST EQ  ( SELECT MAX( ZFIVHST )
*                                  FROM  ZTIVHST
*                                  WHERE ZFIVNO   EQ  P_ZFIVNO
*                                  AND ( CMBLNR   IS  NULL
*                                  OR    CMBLNR   EQ  SPACE ) ).
*>> ÀÔ°í BAPIs FUCNTION
  CALL FUNCTION 'ZIM_BAPI_GOODSMVT_CANCEL'
       EXPORTING
            P_ZFIVNO            = P_ZFIVNO
            P_ZFIVHST           = ZTIVHST-ZFIVHST
            MATERIALDOCUMENT    = ZTIVHST-MBLNR
            MATDOCUMENTYEAR     = ZTIVHST-MJAHR
            GOODSMVT_PSTNG_DATE = L_CBUDAT
            GOODSMVT_PR_UNAME   = SY-UNAME
       IMPORTING
            GOODSMVT_HEADRET    = GR_DOC
       TABLES
            RETURN              = RETURN
       EXCEPTIONS
            MVT_ERROR           = 4.

  W_SUBRC = SY-SUBRC.

  IF W_SUBRC NE 0.           ">> ¿À·ù ¹ß»ý½Ã...
    IF RETURN[] IS INITIAL.
      PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
    ELSE.
      PERFORM  P2000_MULTI_MSG_MAKE1 TABLES  IT_ERR_LIST.
    ENDIF.
  ELSE.
    W_ERR_CHK = 'Y'.
    MESSAGE S060(M7) WITH GR_DOC-MAT_DOC.
    PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
  ENDIF.

ENDFORM.                    " P3000_CALL_GR_DOC_CANCEL
*&---------------------------------------------------------------------*
*&      Form  P2000_TAX_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_TAX_DEL_STATUS_CHECK.

  IF ZTVT-ZFVTRYN NE 'N'.
    MESSAGE  E428.
  ENDIF.

ENDFORM.                    " P2000_TAX_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_EKPO_SELECT_ITEM
*&---------------------------------------------------------------------*
FORM P1000_EKPO_SELECT_ITEM.

  SELECT SINGLE * FROM EKPO WHERE EBELN EQ ZTRED-EBELN
                            AND   EBELP EQ IT_ZSREDSG1-EBELP.

  IF SY-SUBRC EQ 0.
    IF EKPO-MATNR IS INITIAL.
      MESSAGE E336(ME) WITH IT_ZSREDSG1-ZFITMNO.
    ENDIF.
  ELSE.
    MESSAGE E071 WITH ZTRED-EBELN IT_ZSREDSG1-EBELP.
  ENDIF.

ENDFORM.                    " P1000_EKPO_SELECT_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
FORM P2000_DOC_ITEM_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
* INPUT VALUE º¸Á¸À» À§ÇØ...
  W_ZSREQHD = ZSREQHD.

  REFRESH IT_ZSIV.
* Table Multi-Select

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIV
           FROM ZTIV
           WHERE ZFIVNO IN ( SELECT DISTINCT ZFIVNO
                                    FROM   ZTIVIT
                                    WHERE  EBELN EQ ZSREQHD-EBELN ).
  IF SY-SUBRC NE 0.
    MESSAGE E406.
  ENDIF.

*----------------------------------------------------------------------
* position
*----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSIV LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LOGR'.                 " ÀÔ°í¿äÃ» List.
  CALL SCREEN 0014 STARTING AT  08 3
                   ENDING   AT  90 15.
  CLEAR : INCLUDE.                  " ÀÔ°í¿äÃ» List.
ENDFORM.                    " P2000_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIV_LOCAL_LIST
*&---------------------------------------------------------------------*
FORM P2000_ZTIV_LOCAL_LIST.

  FORMAT RESET.

  WRITE : / SY-VLINE, IT_ZSIV-ZFIVNO COLOR COL_KEY INTENSIFIED,
            SY-VLINE.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE : IT_ZSIV-ZFCCDT,   SY-VLINE,
          IT_ZSIV-LIFNR,    SY-VLINE,
          IT_ZSIV-ZFREQTY,  SY-VLINE,
          IT_ZSIV-ERNAM,    SY-VLINE.

*>> ÀÔ°í À¯Çü.
  CASE IT_ZSIV-ZFGRST.
    WHEN 'Y'.
      WRITE : 'ÀÔ°í  ¿Ï·á', SY-VLINE.
    WHEN 'N'.
      WRITE : 'ÀÔ°í  ´ë»ó', SY-VLINE.
    WHEN 'X'.
      WRITE : '¹ÌÀÔ°í´ë»ó', SY-VLINE.
    WHEN OTHERS.
      WRITE : '          ', SY-VLINE.
  ENDCASE.

ENDFORM.                    " P2000_ZTIV_LOCAL_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_ITEM_SELECT1
*&---------------------------------------------------------------------*
FORM P2000_DOC_ITEM_SELECT1.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
* INPUT VALUE º¸Á¸À» À§ÇØ...
  W_ZSREQHD = ZSREQHD.

  REFRESH IT_ZSIV.
* Table Multi-Select

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIV
           FROM ZTIV
           WHERE ZFIVNO IN ( SELECT DISTINCT ZFIVNO
                                    FROM   ZTIVIT
                                    WHERE  ZFREQNO EQ ZSREQHD-ZFREQNO ).
  IF SY-SUBRC NE 0.
    MESSAGE E406.
  ENDIF.

*----------------------------------------------------------------------
* position
*----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSIV LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LOGR1'.                 " ÀÔ°í¿äÃ» List.
  CALL SCREEN 0014 STARTING AT  08 3
                   ENDING   AT  90 15.
  CLEAR : INCLUDE.                  " ÀÔ°í¿äÃ» List.

ENDFORM.                    " P2000_DOC_ITEM_SELECT1
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IMPORT_DATA_REFRESH
*&---------------------------------------------------------------------*
FORM P1000_GET_IMPORT_DATA_REFRESH.

  LOOP AT IT_ZSBLIT.
    W_TABIX = SY-TABIX.
    SELECT  SINGLE *  FROM  ZTREQIT
            WHERE     ZFREQNO     EQ    IT_ZSBLIT-ZFREQNO
            AND       ZFITMNO     EQ    IT_ZSBLIT-ZFITMNO.

    MOVE :  ZTREQIT-NETPR   TO   IT_ZSBLIT-NETPR,
            ZTREQIT-MENGE   TO   IT_ZSBLIT-MENGE,
            ZTREQIT-MEINS   TO   IT_ZSBLIT-MEINS,
            ZTREQIT-PEINH   TO   IT_ZSBLIT-PEINH,
            ZTREQIT-BPRME   TO   IT_ZSBLIT-BPRME.

    MODIFY IT_ZSBLIT INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_GET_IMPORT_DATA_REFRESH
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_CONTAINER_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_CONTAINER_DATA.
  CLEAR  :  IT_CONTLST, IT_CIVRN.
  REFRESH : IT_CONTLST, IT_CIVRN.

  SELECT A~ZFCIVNO INTO CORRESPONDING FIELDS OF TABLE IT_CIVRN
  FROM   ZTCIVHD AS A INNER JOIN ZTCIVIT AS B
  ON     A~ZFCIVRN    EQ    B~ZFCIVRN
  WHERE  B~ZFBLNO     EQ    ZTBL-ZFBLNO
  GROUP BY A~ZFCIVNO.

  LOOP  AT  IT_CIVRN.
     SELECT *
       FROM LIKP
       WHERE BOLNR = IT_CIVRN-ZFCIVNO.
       IF SY-SUBRC EQ 0.
*         SELECT SINGLE *
*                  FROM EIKP
*                 WHERE EXNUM = LIKP-EXNUM.
         SELECT *
           FROM LIPS
          WHERE VBELN = LIKP-VBELN
            AND VGBEL = IT_ZSBLIT-EBELN
            AND VGPOS = IT_ZSBLIT-EBELP.
           ADD LIPS-LFIMG TO IT_CONTLST-LFIMG.
         ENDSELECT.
         IF SY-SUBRC EQ 0.
           MOVE LIKP-BORGR_GRP   TO IT_CONTLST-BORGR_GRP.
           MOVE ZTBL-ZFBLNO      TO IT_CONTLST-ZFBLNO.
           MOVE IT_ZSBLIT-ZFBLIT TO IT_CONTLST-ZFBLIT.
           MOVE IT_ZSBLIT-EBELN  TO IT_CONTLST-EBELN.
           MOVE IT_ZSBLIT-EBELP  TO IT_CONTLST-EBELP.
           MOVE IT_ZSBLIT-MATNR  TO IT_CONTLST-MATNR.
           MOVE IT_ZSBLIT-TXZ01  TO IT_CONTLST-TXZ01.
           MOVE LIPS-VRKME       TO IT_CONTLST-VRKME.
           MOVE LIKP-TRAID       TO IT_CONTLST-TRAID.
           MOVE LIPS-KDMAT       TO IT_CONTLST-KDMAT.
           MOVE LIPS-VBELN       TO IT_CONTLST-VBELN.
           APPEND IT_CONTLST.
           CLEAR IT_CONTLST-LFIMG.
         ENDIF.
       ENDIF.
     ENDSELECT.
  ENDLOOP.

  DESCRIBE  TABLE IT_CONTLST  LINES  W_LINE.
  IF W_LINE GT 0.
    INCLUDE = 'CONTDISP'.
    CALL SCREEN 0015 STARTING AT  10   3
                     ENDING   AT  155 12.
    CLEAR : INCLUDE.
  ENDIF.

ENDFORM.                    " P1000_GET_CONTAINER_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_CONTAINER_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_CONTAINER_DATA_CC.
  CLEAR   IT_CONTLST.
  REFRESH IT_CONTLST.

  SELECT *
    FROM LIKP
   WHERE BOLNR = ZTBL-ZFHBLNO.
*     AND VGBEL = IT_ZSIVIT-EBELN
*     AND VGPOS = IT_ZSIVIT-EBELP.
    IF SY-SUBRC EQ 0.
      SELECT *
        FROM LIPS
       WHERE VBELN = LIKP-VBELN
         AND VGBEL = IT_ZSIVIT-EBELN
         AND VGPOS = IT_ZSIVIT-EBELP.
        ADD LIPS-LFIMG TO IT_CONTLST-LFIMG.
      ENDSELECT.
      IF SY-SUBRC EQ 0.
        MOVE ZTBL-ZFBLNO      TO IT_CONTLST-ZFBLNO.
        MOVE IT_ZSIVIT-ZFBLIT TO IT_CONTLST-ZFBLIT.
        MOVE IT_ZSIVIT-EBELN  TO IT_CONTLST-EBELN.
        MOVE IT_ZSIVIT-EBELP  TO IT_CONTLST-EBELP.
        MOVE IT_ZSIVIT-MATNR  TO IT_CONTLST-MATNR.
        MOVE IT_ZSIVIT-TXZ01  TO IT_CONTLST-TXZ01.
*        MOVE LIPS-LFIMG       TO IT_CONTLST-LFIMG.
        MOVE LIPS-VRKME       TO IT_CONTLST-VRKME.
        MOVE LIKP-TRAID       TO IT_CONTLST-TRAID.
        MOVE LIPS-KDMAT       TO IT_CONTLST-KDMAT.
        MOVE LIPS-VBELN       TO IT_CONTLST-VBELN.
        APPEND IT_CONTLST.
        CLEAR IT_CONTLST-LFIMG.
      ENDIF.
    ENDIF.
  ENDSELECT.
  DESCRIBE  TABLE IT_CONTLST  LINES  W_LINE.
  IF W_LINE GT 0.
    INCLUDE = 'CONTDISP'.
    CALL SCREEN 0015 STARTING AT  10   3
                     ENDING   AT  135 12.
    CLEAR : INCLUDE.
  ENDIF.

ENDFORM.                    " P1000_GET_CONTAINER_DATA_CC
*&---------------------------------------------------------------------*
*&      Form  P2000_PO_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
FORM P2000_PO_DOC_ITEM_SELECT.

  W_ZFOPNNO = ZSCIVHD-ZFOPNNO.
  W_ZFREQNO = ZSCIVHD-ZFREQNO.
  W_EBELN   = ZSCIVHD-EBELN.
  W_ZFCIVRN = ZSCIVHD-ZFCIVRN.

  REFRESH IT_ZSCIVHD.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSCIVHD
           FROM ZTCIVHD
           WHERE ZFCIVRN IN ( SELECT DISTINCT ZFCIVRN
                                    FROM   ZTCIVIT
                                    WHERE  EBELN EQ ZSCIVHD-EBELN ).
  IF SY-SUBRC NE 0.
    MESSAGE E406.
  ENDIF.

  DESCRIBE TABLE IT_ZSCIVHD LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'CIVPO'.                 " LOCAL ¹°´ë List.
  CALL SCREEN 0014 STARTING AT  08 3
                   ENDING   AT  90 15.
  CLEAR : INCLUDE.                  "

ENDFORM.                    " P2000_PO_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTCIVHD_LOCAL_LIST
*&---------------------------------------------------------------------*
FORM P2000_ZTCIVHD_LOCAL_LIST.

  FORMAT RESET.

  WRITE : / SY-VLINE, IT_ZSCIVHD-ZFCIVRN COLOR COL_KEY INTENSIFIED,
            SY-VLINE.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE : IT_ZSCIVHD-BUKRS,                                SY-VLINE,
          IT_ZSCIVHD-ZFREQTY,                              SY-VLINE,
          IT_ZSCIVHD-ZFIVAMP CURRENCY IT_ZSCIVHD-ZFIVAMC,  SY-VLINE,
          IT_ZSCIVHD-ZFIVAMK CURRENCY IT_ZSCIVHD-ZFIVAMC,  SY-VLINE.

ENDFORM.                    " P2000_ZTCIVHD_LOCAL_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_IV_DOC_ITEM_SELECT1
*&---------------------------------------------------------------------*
FORM P2000_IV_DOC_ITEM_SELECT1.

  W_ZFOPNNO = ZSCIVHD-ZFOPNNO.
  W_ZFREQNO = ZSCIVHD-ZFREQNO.
  W_EBELN   = ZSCIVHD-EBELN.
  W_ZFCIVRN = ZSCIVHD-ZFCIVRN.

  REFRESH IT_ZSCIVHD.
* Table Multi-Select

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSCIVHD
           FROM ZTCIVHD
           WHERE ZFCIVRN IN ( SELECT DISTINCT ZFCIVRN
                                    FROM   ZTCIVIT
                                    WHERE  ZFREQNO EQ ZSCIVHD-ZFREQNO ).
  IF SY-SUBRC NE 0.
    MESSAGE E406.
  ENDIF.

  DESCRIBE TABLE IT_ZSCIVHD LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'CIVRQ'.                 " LOCAL ¹°´ë List.
  CALL SCREEN 0014 STARTING AT  08 3
                   ENDING   AT  90 15.
  CLEAR : INCLUDE.                  "

ENDFORM.                    " P2000_IV_DOC_ITEM_SELECT1
*&---------------------------------------------------------------------*
*&      Form  P2000_IMPRES_PRINT
*&---------------------------------------------------------------------*
FORM P2000_IMPRES_PRINT.

  SUBMIT ZRIMIMPRES  WITH P_BLNO EQ ZTIDS-ZFBLNO
                   WITH P_CLSEQ  EQ ZTIDS-ZFCLSEQ
                   AND RETURN. " ´Ù½Ã µ¹¾Æ ¿Ã¼ö ÀÖ°Ô.

ENDFORM.                    " P2000_IMPRES_PRINT
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_SD_PRINT
*&---------------------------------------------------------------------*
FORM P2000_BL_SD_PRINT USING    P_ZFBLNO.

  SUBMIT ZRIMBLSD  WITH P_BLNO EQ P_ZFBLNO
                     AND RETURN. " ´Ù½Ã µ¹¾Æ ¿Ã¼ö ÀÖ°Ô.

ENDFORM.                    " P2000_BL_SD_PRINT
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_SEND_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DOC_SEND_MESSAGE.

  IF W_STATUS EQ C_REQ_D.
    CASE SY-LANGU.
      WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING '¼±Àû¼­·ù ¼ÛºÎÃë¼Ò È®ÀÎ' " Å¸ÀÌÆ²..
                         '¼±Àû¼­·ù ¼ÛºÎ Ãë¼ÒÇÕ´Ï´Ù.'
                         '¼ÛºÎÃë¼Ò ÇÏ½Ã°Ú½À´Ï±î?'           " MSG2
                         'N'                      " Ãë¼Ò ¹öÆ° À¯/?
                         '1'.                     " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
               'Shipping document send cancel confirm'
               'Now cancel the shipping document send.'
               'Do you want to cancel?'
               'N'
               '1'.
    ENDCASE.

  ELSE.
    CASE SY-LANGU.
      WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING '¼±Àû¼­·ù ¼ÛºÎÃë¼Ò È®ÀÎ' " Å¸ÀÌÆ²..
                         'ÇöÀç¹®¼­¸¦ ÀúÀå ÈÄ ¼±Àû¼­·ù ¼ÛºÎ Ãë¼ÒÇÕ´Ï´Ù.'
                         '¼ÛºÎÃë¼Ò ÇÏ½Ã°Ú½À´Ï±î?'           " MSG2
                         'N'                      " Ãë¼Ò ¹öÆ° À¯/?
                         '1'.                     " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
               'Shipping document send cancel confirm'
               'Now cancel the shipping document send after save.'
               'Do you want to cancel?'
               'N'
               '1'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_DOC_SEND_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_REAL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DOC_REAL_MESSAGE.

  IF W_STATUS EQ C_REQ_D.
    CASE SY-LANGU.
      WHEN '3'.
       PERFORM P2000_MESSAGE_BOX USING '½ÇÀÔÇ×ÀÔ·Â Ãë¼Ò È®ÀÎ' " Å¸ÀÌÆ²..
                       '½ÇÀÔÇ×ÀÔ·ÂÀ» Ãë¼ÒÇÕ´Ï´Ù.'
                       'Ãë¼Ò ÇÏ½Ã°Ú½À´Ï±î?'                 " MSG2
                       'N'                      " Ãë¼Ò ¹öÆ° À¯/?
                       '1'.                     " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
               'Cancel input of actual arrival date confirm'
               'Now cancel input of actual arrival date.'
               'Do you want to cancel?'
               'N'
               '1'.
    ENDCASE.

  ELSE.
    CASE SY-LANGU.
      WHEN '3'.
       PERFORM P2000_MESSAGE_BOX USING '½ÇÀÔÇ×ÀÔ·Â Ãë¼Ò È®ÀÎ' " Å¸ÀÌÆ²..
                       'ÇöÀç¹®¼­¸¦ ÀúÀå ÈÄ ½ÇÀÔÇ× ÀÔ·Â Ãë¼ÒÇÕ´Ï´Ù.'
                       'Ãë¼Ò ÇÏ½Ã°Ú½À´Ï±î?'                 " MSG2
                       'N'                      " Ãë¼Ò ¹öÆ° À¯/?
                       '1'.                     " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
               'Cancel input of actual arrival date confirm'
               'Now cancel input of actual arrival date after save.'
               'Do you want to cancel?'
               'N'
               '1'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_DOC_REAL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_IMPREQ_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_IMPREQ_MESSAGE.
  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING 'ÀÌµ¿ È®ÀÎ'             " Å¸ÀÌÆ²...
                                   'ÇöÀç ÀÔ·Â³»¿ªÀ» ÀúÀåÇÏÁö ¾Ê½À´Ï´Ù.'
                                       'ÀúÀå ÈÄ ÀÌµ¿ÇÏ½Ã°Ú½À´Ï±î?'" MSG2
                                    'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '1'.                 " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'End confirm'
             'Do not save the entered item.'
             'Do you want to end after save?'
             'Y'
             '1'.
  ENDCASE.
ENDFORM.                    " P2000_IMPREQ_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_FI_POST
*&---------------------------------------------------------------------*
FORM P3000_FI_POST_MESSAGE.

  PERFORM P2000_POST_MESSAGE.

  IF ANTWORT EQ 'Y'.
    REFRESH : IT_ERR_LIST.
    SET UPDATE TASK LOCAL.
    CLEAR : *ZTBKPF.
    CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
         EXPORTING
              W_OK_CODE    = 'SAVE'
              BUKRS        = ZTBKPF-BUKRS
              GJAHR        = ZTBKPF-GJAHR
              ZFSTATUS     = 'C'
              W_ZTBKPF_OLD = *ZTBKPF
              W_ZTBKPF     = ZTBKPF
         TABLES
              IT_ZSBSEG    = IT_ZSBSEG
         CHANGING
              BELNR        = ZTBKPF-BELNR
         EXCEPTIONS
              ERROR_UPDATE = 4.

    W_SUBRC = SY-SUBRC.
    IF SY-SUBRC NE 0.
      MESSAGE S494.
      ROLLBACK WORK.
      PERFORM  P2000_EXIT_ROUTINE_COM.
      EXIT.
    ENDIF.

    CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST'
         EXPORTING
              BUKRS            = ZTBKPF-BUKRS
              BELNR            = ZTBKPF-BELNR
              GJAHR            = ZTBKPF-GJAHR
         IMPORTING
              INVOICEDOCNUMBER = INVOICEDOCNUMBER
              FISCALYEAR       = FISCALYEAR
         TABLES
              RETURN           = RETURN
         EXCEPTIONS
              POST_ERROR       = 4.

    ZTIV-ZFIVNO = ZTBL-ZFBLNO.

    IF SY-SUBRC EQ 0.
      LOOP AT IT_COST_SELECTED.
        W_TABIX = SY-TABIX.
        IT_COST_SELECTED-ZFFIYR = FISCALYEAR.
        IT_COST_SELECTED-ZFACDO = INVOICEDOCNUMBER.
        IT_COST_SELECTED-ZFPSDT = SY-DATUM.
        MODIFY IT_COST_SELECTED INDEX W_TABIX.
      ENDLOOP.
      MODIFY ZTBLCST FROM TABLE IT_COST_SELECTED.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST.

      COMMIT WORK.
    ELSE.
      ZTIV-ZFIVNO = ZTBL-ZFBLNO.
      IF RETURN[] IS INITIAL.
        PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST.
      ELSE.
        PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST
                                       USING   ZTBL-ZFBLNO.
      ENDIF.

      CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
           EXPORTING
                W_OK_CODE    = 'DELE'
                BUKRS        = ZTBKPF-BUKRS
                GJAHR        = ZTBKPF-GJAHR
                ZFSTATUS     = C_REQ_U
                W_ZTBKPF_OLD = *ZTBKPF
                W_ZTBKPF     = ZTBKPF
           TABLES
                IT_ZSBSEG    = IT_ZSBSEG
           CHANGING
                BELNR        = ZTBKPF-BELNR
           EXCEPTIONS
                ERROR_UPDATE = 4.

      MESSAGE S494.
    ENDIF.

    DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
    IF W_LINE GT 0.
      INCLUDE = 'POPU'.
      CALL SCREEN 0014 STARTING AT  04   3
                       ENDING   AT  100 12.
      CLEAR : INCLUDE.
    ENDIF.

    CLEAR OK-CODE.
    PERFORM  P2000_SET_UNLOCK.
    LEAVE TO TRANSACTION SY-TCODE AND SKIP FIRST SCREEN.
  ELSE.
    PERFORM  P2000_SET_UNLOCK.
    PERFORM  P2000_SET_SCREEN_SCRCOM.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.                    " P3000_FI_POST_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_POST_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POST_MESSAGE.

  CALL SCREEN 0060 STARTING AT 001 01
                   ENDING   AT 110 25.

ENDFORM.                    " P2000_POST_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_BELEGART_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_BELEGART_AUTHORITY_CHECK.

  CHECK T003-BRGRU NE SPACE.

  AUTHORITY-CHECK OBJECT F_BKPF_BLA
    ID 'ACTVT' FIELD ACT_HINZ
    ID 'BRGRU' FIELD T003-BRGRU.

  CHECK SY-SUBRC NE 0.
  MESSAGE E087(F5) WITH ZTBKPF-BLART.

ENDFORM.                    " P2000_BELEGART_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_POST_DATA_MOVE
*&---------------------------------------------------------------------*
FORM P2000_POST_DATA_MOVE.

  REFRESH : IT_ZSBLCST_POST, IT_ZSBSEG, IT_BLCSTHD.
*">Freight .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLCST_POST
           FROM ZTBLCST
           WHERE ZFBLNO  EQ ZTBL-ZFBLNO
           AND   ZFACDO  EQ SPACE
           AND   ZFCSQ   GT  '10000'
           AND   ZFCKAMT GT 0.

*>Bonded Transportation Fee.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLCST_POST1
           FROM ZTBLCST
           WHERE ZFBLNO  EQ ZTBL-ZFBLNO
           AND   ZFACDO  EQ SPACE
           AND   ZFCSQ   LE '10000'
           AND   ZFCKAMT GT 0.
*">
  LOOP AT IT_ZSBLCST_POST.
    W_TABIX = SY-TABIX.
    SELECT SINGLE * FROM ZTIMIMG08
           WHERE ZFCDTY EQ '004'
           AND   ZFCD   EQ IT_ZSBLCST_POST-ZFCSCD.

    IT_ZSBLCST_POST-COND_TYPE = ZTIMIMG08-COND_TYPE.
    IT_ZSBLCST_POST-ZFCDNM    = ZTIMIMG08-ZFCDNM.
    IT_ZSBLCST_POST-ZFCD1     = ZTIMIMG08-ZFCD1.
    IT_ZSBLCST_POST-BLART     = ZTIMIMG08-BLART.
    IT_ZSBLCST_POST-ZFCDTY    = '004'.
    IF IT_ZSBLCST_POST-MWSKZ IS INITIAL.
      IT_ZSBLCST_POST-MWSKZ     = ZTIMIMG08-ZFCD5.
    ENDIF.
    MODIFY IT_ZSBLCST_POST INDEX W_TABIX.

    MOVE-CORRESPONDING IT_ZSBLCST_POST TO IT_BLCSTHD.
    MOVE T001-WAERS TO IT_BLCSTHD-WAERS.

    COLLECT IT_BLCSTHD.
  ENDLOOP.

*"> Others Cost
  LOOP AT IT_ZSBLCST_POST1.
    W_TABIX = SY-TABIX.
    SELECT SINGLE * FROM ZTIMIMG08
           WHERE ZFCDTY EQ '005'
           AND   ZFCD   EQ IT_ZSBLCST_POST1-ZFCSCD.

    IT_ZSBLCST_POST1-COND_TYPE = ZTIMIMG08-COND_TYPE.
    IT_ZSBLCST_POST1-ZFCDNM    = ZTIMIMG08-ZFCDNM.
    IT_ZSBLCST_POST1-ZFCD1     = ZTIMIMG08-ZFCD1.
    IT_ZSBLCST_POST1-BLART     = ZTIMIMG08-BLART.
    IT_ZSBLCST_POST1-ZFCDTY    = '005'.
    IT_ZSBLCST_POST1-ZFCAMT    = IT_ZSBLCST_POST1-ZFCKAMT.
    IT_ZSBLCST_POST1-WAERS     = IT_ZSBLCST_POST1-KRW.
    IF IT_ZSBLCST_POST1-MWSKZ IS INITIAL.
      IT_ZSBLCST_POST1-MWSKZ   = ZTIMIMG08-ZFCD5.
    ENDIF.
    MODIFY IT_ZSBLCST_POST1 INDEX W_TABIX.

    MOVE-CORRESPONDING IT_ZSBLCST_POST1 TO IT_ZSBLCST_POST.
    APPEND IT_ZSBLCST_POST.

    MOVE-CORRESPONDING IT_ZSBLCST_POST1 TO IT_BLCSTHD.
    COLLECT IT_BLCSTHD.
  ENDLOOP.

  DESCRIBE TABLE  IT_BLCSTHD LINES W_LINE.
  IF W_LINE LE 0.
    MESSAGE S931.
    EXIT.
  ENDIF.

*> POSTING¿ë ¼±ÅÃ µ¥ÀÌÅ¸ BULID..
  REFRESH : IT_ZSBKPF, IT_ZSBSEG, IT_ZSBSEG_TMP.
  CLEAR : IT_ZSBKPF, IT_ZSBSEG, IT_ZSBSEG_TMP.
  CLEAR : ZTBKPF, ZSBKPF, ZSBSEG.

*> °øÅë»ç¿ë¹ø¼ö Á¤ÀÇ...
  SELECT SINGLE * FROM  T001
                  WHERE BUKRS EQ ZTBL-BUKRS.

  SELECT SINGLE * FROM  T005
                  WHERE LAND1 EQ T001-LAND1.

  SELECT SINGLE * FROM ZTIMIMG11
                  WHERE BUKRS EQ ZTBL-BUKRS.

  MOVE : SY-MANDT        TO   ZTBKPF-MANDT,
         T001-BUKRS      TO   ZTBKPF-BUKRS,
         ZTBL-ZFBLNO     TO   ZTBKPF-ZFIMDNO,
         SY-UNAME        TO   ZTBKPF-ZFCNAME,
         'X'             TO   ZTBKPF-ZFAUTO,
         'X'             TO   ZTBKPF-ZFATPT,
         T001-WAERS      TO   ZTBKPF-HWAER,
         'N'             TO   ZTBKPF-ZFPOSYN,
*         'A'             TO   ZTBKPF-ZLSPR,
         ZTBL-ZFPOYN     TO   ZTBKPF-ZFPOYN,
         'B/L Cost'      TO   ZTBKPF-BKTXT,
         SY-DATUM        TO   ZTBKPF-WWERT,
         SY-DATUM        TO   ZTBKPF-BUDAT,
         SY-DATUM        TO   ZTBKPF-BLDAT,
         SY-DATUM(4)     TO   ZTBKPF-GJAHR,
         SY-DATUM+4(2)   TO   ZTBKPF-MONAT.

   *ZTBL = ZTBL.

  LOOP AT IT_BLCSTHD.
    W_TABIX = SY-TABIX.
    SELECT SINGLE NAME1 INTO IT_BLCSTHD-NAME1
           FROM   LFA1
           WHERE  LIFNR EQ IT_BLCSTHD-ZFPAY.
    IF IT_BLCSTHD-ZFCDTY EQ '004'.
      IF IT_BLCSTHD-ZFCD1 EQ 'Y'.
        IT_BLCSTHD-TEXT = 'Planned Cost(Freight)'.
      ELSE.
        IT_BLCSTHD-TEXT = 'Extra Charge(Freight)'.
      ENDIF.
    ELSE.
      IF IT_BLCSTHD-ZFCD1 EQ 'Y'.
        IT_BLCSTHD-TEXT = 'Planned Cost(Freight)'.
      ELSE.
        IT_BLCSTHD-TEXT = 'Extra Charge(Freight)'.
      ENDIF.
    ENDIF.

    SELECT SINGLE J_1BBRANCH INTO IT_BLCSTHD-BUPLA
           FROM T001W
           WHERE WERKS EQ IT_BLCSTHD-ZFWERKS.
    IT_BLCSTHD-ZFSEQ = W_TABIX.
    MODIFY IT_BLCSTHD INDEX W_TABIX.

    CLEAR : IT_ZSBKPF.
    MOVE-CORRESPONDING ZTBKPF  TO  IT_ZSBKPF.
    MOVE : IT_BLCSTHD-ZFCDTY   TO  IT_ZSBKPF-ZFCSTGRP,
           IT_BLCSTHD-ZFSEQ    TO  IT_ZSBKPF-ZFSEQ,
           IT_BLCSTHD-BLART    TO  IT_ZSBKPF-BLART,
           IT_BLCSTHD-ZFPAY    TO  IT_ZSBKPF-LIFNR,
           'N'                 TO  IT_ZSBKPF-ZFPOSYN,
           IT_BLCSTHD-BUPLA    TO  IT_ZSBKPF-BUPLA,
           IT_BLCSTHD-WAERS    TO  IT_ZSBKPF-WAERS,
           IT_BLCSTHD-ZTERM    TO  IT_ZSBKPF-ZTERM,
           IT_BLCSTHD-MWSKZ    TO  IT_ZSBKPF-MWSKZ.

    IF T001-WAERS EQ IT_ZSBKPF-WAERS.
      MOVE 'X'             TO   IT_ZSBKPF-ZFPCUR.
    ENDIF.
    IF IT_BLCSTHD-ZFCD1 EQ 'Y'.
      MOVE 'X'             TO   IT_ZSBKPF-ZFDCSTX.
    ENDIF.

    CALL FUNCTION 'FI_VENDOR_DATA'
         EXPORTING
              I_BUKRS = IT_ZSBKPF-BUKRS
              I_LIFNR = IT_ZSBKPF-LIFNR
         IMPORTING
              E_KRED  = VF_KRED.

    IT_ZSBKPF-AKONT = VF_KRED-AKONT.
    IT_ZSBKPF-NAME1 = VF_KRED-NAME1.

*> ITEM LOOPING...
    LOOP AT IT_ZSBLCST_POST WHERE ZFPAY     = IT_BLCSTHD-ZFPAY
                            AND   COND_TYPE = IT_BLCSTHD-COND_TYPE
                            AND   ZFCD1     = IT_BLCSTHD-ZFCD1
                            AND   MWSKZ     = IT_BLCSTHD-MWSKZ
                            AND   BLART     = IT_BLCSTHD-BLART
                            AND   KRW       = IT_BLCSTHD-WAERS
                            AND   ZFCDTY    = IT_BLCSTHD-ZFCDTY
                            AND   ZFWERKS   = IT_BLCSTHD-ZFWERKS.
      W_TABIX = SY-TABIX.
      IT_ZSBLCST_POST-ZFSEQ = IT_BLCSTHD-ZFSEQ.
      MODIFY IT_ZSBLCST_POST INDEX W_TABIX.

*> ºñ¿ë¾ÆÀÌÅÛ Å×ÀÌºí BUILD.
      MOVE-CORRESPONDING IT_ZSBLCST_POST TO IT_ZSBSEG.

      MOVE : IT_BLCSTHD-COND_TYPE       TO IT_ZSBSEG-COND_TYPE,
             IT_BLCSTHD-ZFCDTY          TO IT_ZSBSEG-ZFCSTGRP,
             IT_ZSBKPF-ZFDCSTX          TO IT_ZSBSEG-ZFDCSTX,
             IT_ZSBLCST_POST-ZFVEN      TO IT_ZSBKPF-ZFVEN,
             IT_ZSBLCST_POST-ZFCSCD     TO IT_ZSBSEG-ZFCD,
             '40'                       TO IT_ZSBSEG-NEWBS,
             'S'                        TO IT_ZSBSEG-SHKZG,
             IT_ZSBLCST_POST-ZFCKAMT    TO IT_ZSBSEG-WRBTR,
             IT_ZSBLCST_POST-ZFVAT      TO IT_ZSBSEG-WMWST,
             IT_ZSBLCST_POST-ZFCKAMT    TO IT_ZSBSEG-DMBTR,
             IT_ZSBLCST_POST-ZFEXRT     TO IT_ZSBSEG-KURSF,
             IT_ZSBKPF-ZFPOYN           TO IT_ZSBSEG-ZFPOYN,
             SY-DATUM                   TO IT_ZSBSEG-WWERT,
             *ZTBL-ZFBLNO               TO IT_ZSBSEG-ZFIMDNO,
             *ZTBL-ZFHBLNO              TO IT_ZSBSEG-ZUONR,
             *ZTBL-KOSTL                TO IT_ZSBSEG-KOSTL,
             *ZTBL-PS_POSID             TO IT_ZSBSEG-PS_POSID,
             IT_ZSBLCST_POST-ZFCDNM     TO IT_ZSBSEG-SGTXT.

      PERFORM P1000_IMPORT_DOC_CHEKC USING IT_ZSBSEG-ZFIMDNO
                                           IT_ZSBSEG-ZFDCNM
                                           IT_ZSBSEG-ZFPOYN
                                           'I'
                                           IT_ZSBSEG-KOSTL.

      PERFORM  P2000_SET_NEWKO       USING IT_ZSBSEG-ZFCSTGRP
                                           IT_ZSBSEG-NEWKO
                                           IT_ZSBSEG-ZFCD
                                           IT_ZSBSEG-ZFIMDNO.

      SELECT SINGLE TXT20 INTO IT_ZSBSEG-KONTO_TXT
             FROM  SKAT
             WHERE SPRAS  EQ  SY-LANGU
             AND   KTOPL  EQ  T001-KTOPL
             AND   SAKNR  EQ  IT_ZSBSEG-NEWKO.

      APPEND IT_ZSBSEG.

      ADD IT_ZSBSEG-WRBTR TO IT_ZSBKPF-WRBTR.
      ADD IT_ZSBSEG-WMWST TO IT_ZSBKPF-WRBTR.

      ADD IT_ZSBSEG-DMBTR TO IT_ZSBKPF-DMBTR.
      ADD IT_ZSBSEG-WMWST TO IT_ZSBKPF-DMBTR.

      ADD IT_ZSBSEG-WMWST TO IT_ZSBKPF-WMWST.
    ENDLOOP.

    IF T001-WAERS EQ IT_ZSBKPF-WAERS.
      IT_ZSBKPF-WRBTR = IT_ZSBKPF-DMBTR.
    ENDIF.

    IF IT_ZSBKPF-WRBTR EQ 0.
      IT_ZSBKPF-WRBTR = IT_ZSBKPF-DMBTR.
      IT_ZSBKPF-WAERS = T001-WAERS.
    ELSE.
      IT_ZSBKPF-WAERS = IT_BLCSTHD-WAERS.
    ENDIF.

*> Cost Posting Screen
    APPEND IT_ZSBKPF.
  ENDLOOP.

  CLEAR: ANTWORT, IT_BLCSTHD.
  INCLUDE = 'BLCST'.
  SPOP-TITEL = 'B/L Cost Posting Screen'.

  SELECT SINGLE * FROM ZTIMIMG00.

  CALL SCREEN 0070 STARTING AT  01 1
                   ENDING   AT  105 28.

  CLEAR : INCLUDE.
  IF ANTWORT EQ 'Y'.
    DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
    IF W_LINE GT 0.
      INCLUDE = 'POPU'.
      CALL SCREEN 0071 STARTING AT  01   1
                       ENDING   AT  102 16.
      CLEAR : INCLUDE.
    ENDIF.
    PERFORM  P2000_SET_UNLOCK.
    PERFORM  P2000_SET_SCREEN_SCRCOM.
    LEAVE SCREEN.
  ELSE.
    PERFORM  P2000_SET_UNLOCK.
    PERFORM  P2000_SET_SCREEN_SCRCOM.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.                    " P2000_POST_DATA_MOVE
*&---------------------------------------------------------------------*
*&      Form  P3000_SCR0061_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM P3000_SCR0061_HEADER_WRITE.

  SELECT SINGLE * FROM  LFA1
                  WHERE LIFNR EQ IT_ZSBLCST_POST-ZFPAY.
  WRITE : / SY-ULINE(76).
  FORMAT COLOR COL_KEY INTENSIFIED OFF.
  WRITE : / SY-VLINE,
            MARKFIELD  AS CHECKBOX,
            SY-VLINE NO-GAP,
            IT_ZSBLCST_POST-ZFPAY NO-GAP,
            '-',
            LFA1-NAME1,
            SY-VLINE,
            IT_ZSBLCST_POST-COND_TYPE,
            SY-VLINE NO-GAP,
            IT_ZSBLCST_POST-MWSKZ NO-GAP,
            SY-VLINE,
            IT_ZSBLCST_POST-ZFWERKS,
            SY-VLINE NO-GAP,
            IT_ZSBLCST_POST-WAERS(4) NO-GAP,
            SY-VLINE.

  HIDE IT_ZSBLCST_POST.

  WRITE : / SY-ULINE(76).
  W_CHG_CHK = 'Y'.

ENDFORM.                    " P3000_SCR0061_HEADER_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_ROUTINE_COM
*&---------------------------------------------------------------------*
FORM P2000_EXIT_ROUTINE_COM.

  CLEAR OK-CODE.
  PERFORM  P2000_SET_UNLOCK.
  PERFORM  P2000_SET_SCREEN_SCRCOM.
  LEAVE SCREEN.

ENDFORM.                    " P2000_EXIT_ROUTINE_COM
*&---------------------------------------------------------------------*
*&      Form  P2000_CHECK_COST_GROUP
*&---------------------------------------------------------------------*
FORM P2000_CHECK_COST_GROUP USING    W_ERR_MODE.

  DATA : W_ZFCSTGRP LIKE ZTBKPF-ZFCSTGRP.

  W_ERR_MODE = 'Y'.

  IF ZTBKPF-ZFCSTGRP IS INITIAL.
    PERFORM P2000_NO_INPUT(SAPMZIM02) USING 'ZTBKPF' 'ZFCSTGRP'.
    EXIT.
  ELSE.
    CLEAR : DD07T.
    SELECT * FROM DD07T WHERE DOMNAME     EQ 'ZDCSTGRP'
                        AND   DDLANGUAGE  EQ SY-LANGU
                        AND   AS4LOCAL    EQ 'A'
                        AND   DOMVALUE_L  EQ ZTBKPF-ZFCSTGRP
                        ORDER BY AS4VERS DESCENDING.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      MESSAGE S593 WITH 'Expense group'.
      EXIT.
    ENDIF.

    IF W_ZFCSTGRP IS INITIAL.
      W_ZFCSTGRP = ZTBKPF-ZFCSTGRP.
      PERFORM  P1000_GET_COST_ITEM.
    ELSE.
      IF W_ZFCSTGRP NE ZTBKPF-ZFCSTGRP.
*>> LINE COUNTER.
        DESCRIBE TABLE IT_ZSBSEG LINES W_LINE.
        IF W_LINE GT 0.
          PERFORM  P2000_ITEM_REFRESH_MSG.
          IF ANTWORT EQ 'Y'.
            W_ZFCSTGRP = ZTBKPF-ZFCSTGRP.
            PERFORM  P1000_GET_COST_ITEM.
          ENDIF.
        ELSE.
          SET CURSOR FIELD 'ZTBKPF-ZFCSTGRP'.
          MESSAGE W590 WITH W_ZFCSTGRP ZTBKPF-ZFCSTGRP.
          W_ZFCSTGRP = ZTBKPF-ZFCSTGRP.
          PERFORM  P1000_GET_COST_ITEM.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  W_ERR_MODE = 'N'.

ENDFORM.                    " P2000_CHECK_COST_GROUP
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_COST_ITEM
*&---------------------------------------------------------------------*
FORM P1000_GET_COST_ITEM.

  REFRESH : IT_ZSBSEG.
  CLEAR : IT_ZSBSEG.

  PERFORM   GET_DD07T_SELECT(SAPMZIM01)
            USING 'ZDCSTGRP'  ZTBKPF-ZFCSTGRP
            CHANGING          W_COST_TYPE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG08
                FROM ZTIMIMG08
                WHERE ZFCDTY EQ ZTBKPF-ZFCSTGRP.
  IF SY-SUBRC NE 0.
    MESSAGE E591 WITH ZTBKPF-ZFCSTGRP.
  ENDIF.

*   LOOP AT IT_ZSIMIMG08.
*      CLEAR : IT_ZSBSEG.
*      MOVE-CORRESPONDING IT_ZSIMIMG08  TO  IT_ZSBSEG.
*      MOVE: ZTBKPF-KURSF               TO  IT_ZSBSEG-KURSF,
*            ZTBKPF-WWERT               TO  IT_ZSBSEG-WWERT,
*            ZTBKPF-ZFCSTGRP            TO  IT_ZSBSEG-ZFCSTGRP,
*            '40'                       TO  IT_ZSBSEG-NEWBS,
*            'S'                        TO  IT_ZSBSEG-SHKZG.
*
*      CASE ZTBKPF-ZFCSTGRP.
*         WHEN '003'.
*            MOVE: ZTIMIMG11-ZFIOCAC1  TO IT_ZSBSEG-NEWKO.
*         WHEN '004'.
*            MOVE: ZTIMIMG11-ZFIOCAC2  TO IT_ZSBSEG-NEWKO.
*         WHEN '005'.
*            MOVE: ZTIMIMG11-ZFIOCAC2  TO IT_ZSBSEG-NEWKO.
*         WHEN '006'.
*            MOVE: ZTIMIMG11-ZFIOCAC4  TO IT_ZSBSEG-NEWKO.
*         WHEN '007'.
*            MOVE: ZTIMIMG11-ZFIOCAC12 TO IT_ZSBSEG-NEWKO.
*         WHEN OTHERS.
*      ENDCASE.
*
*      APPEND IT_ZSBSEG.
*   ENDLOOP.

ENDFORM.                    " P1000_GET_COST_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_ITEM_REFRESH_MSG
*&---------------------------------------------------------------------*
FORM P2000_ITEM_REFRESH_MSG.

  PERFORM P2000_MESSAGE_BOX USING '¾ÆÀÌÅÛ °»½Å È®ÀÎ'
                          'ÇöÀç ¹®¼­ÀÇ ºñ¿ëÇ×¸ñÀ» Àç°»½ÅÇÕ´Ï´Ù.'
                          '°è¼Ó ÁøÇàÇÏ½Ã°Ú½À´Ï±î?'
                          'N'                 " Ãë¼Ò ¹öÆ° À¯/¹«.
                          '1'.                " default button

ENDFORM.                    " P2000_ITEM_REFRESH_MSG
*&---------------------------------------------------------------------*
*&      Form  P2000_SO_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_SO_DOC_DISPLAY USING    P_VBELN.

  IF P_VBELN IS INITIAL.
    MESSAGE E937.
  ENDIF.

  SET PARAMETER ID 'AUN' FIELD P_VBELN.

  CALL TRANSACTION 'VA03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SO_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_TRCL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_TRCL_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING
              '¹ÝÁ¦ Ãë¼Ò È®ÀÎ'
              'ÇØ´ç ¹ÝÁ¦ÀüÇ¥¸¦ Ãë¼ÒÇÕ´Ï´Ù.'
              '°è¼Ó ÁøÇà ÇÏ½Ã°Ú½À´Ï±î?'
              'N'
              '1'.
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Cancel clearing confirm'
             'Do cancel the clearing slip.'
             'Do you want to continue?'
             'N'
             '1'.
  ENDCASE.
ENDFORM.                    " P2000_TRCL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_TRCC_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_TRCC_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING
              'ÀÌÀÚ¹Ý¿µ Ãë¼Ò È®ÀÎ'
              'ÇØ´ç ÀÌÀÚ¹Ý¿µÀüÇ¥¸¦ Ãë¼ÒÇÕ´Ï´Ù.'
              '°è¼Ó ÁøÇà ÇÏ½Ã°Ú½À´Ï±î?'
              'N'
              '1'.
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Cancel Interest Acc.Doc. confirm'
             'Do cancel the Interest Acc.Doc.'
             'Do you want to continue?'
             'N'
             '1'.
  ENDCASE.
ENDFORM.                    " P2000_TRCC_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_CUSTOMER_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_CUSTOMER_DATA USING    P_KUNNR
                                      P_KUNNR_NM.
  CLEAR : P_KUNNR_NM.
  IF NOT P_KUNNR IS INITIAL.
    SELECT SINGLE NAME1 INTO P_KUNNR_NM FROM KNA1
           WHERE    KUNNR EQ P_KUNNR.
  ENDIF.

ENDFORM.                    " P1000_GET_CUSTOMER_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDR_EDI_SEND
*&---------------------------------------------------------------------*
FORM P3000_ZTIDR_EDI_SEND.
*-----------------------------------------------------------------------
* »óÅÂ °Ë?
*-----------------------------------------------------------------------
* MODIFIED BY SEUNGYEON (2002.08.30)
*  CLEAR  ZTCUCL.
*  SELECT SINGLE * FROM ZTCUCL
*  WHERE  ZFBLNO  = ZTIDR-ZFBLNO
*  AND    ZFCLSEQ = ZTIDR-ZFCLSEQ.
  CLEAR ZTIV.
  SELECT SINGLE * FROM ZTIV
    WHERE ZFIVNO = ZTIDR-ZFIVNO.

*  IF ZTCUCL-ZFCUST EQ '4'. MESSAGE E754. ENDIF.
  IF ZTIV-ZFCUST EQ '4'. MESSAGE E754. ENDIF.
*  IF ZTCUCL-ZFCUST EQ '3'. MESSAGE W754. ENDIF.

  DESCRIBE TABLE IT_ZSIDRHS LINES LINE.
  IF LINE = 0. MESSAGE E762. ENDIF.

  DESCRIBE TABLE IT_ZSIDRHSD LINES LINE.
  IF LINE = 0. MESSAGE E763. ENDIF.

  IF ZTIDR-ZFCUT IS INITIAL.
    MESSAGE  E476.
  ENDIF.
  IF ZTIDR-ZFIDWDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDR' 'ZFIDWDT'.
  ENDIF.

*>> EDI »óÅÂ Ã¼Å©.
  CALL FUNCTION 'ZIM_CUDATA_EDI_CHK'
       EXPORTING
            MODE        = 'Y'
       TABLES
            IT_ZTIDRHS  = IT_ZSIDRHS
            IT_ZTIDRHSD = IT_ZSIDRHSD
            IT_ZTIDRHSL = IT_ZSIDRHSL
       CHANGING
            ZTIDR       = ZTIDR.

  IF ZTIDR-ZFEDICK EQ 'X'.
    MESSAGE E119 WITH ZTIDR-ZFBLNO  ZTIDR-ZFCLSEQ.
  ENDIF.
  IF ZTIDR-ZFEDIST NE 'N'.
    MESSAGE E105 WITH  ZTIDR-ZFBLNO  ZTIDR-ZFCLSEQ  ZTIDR-ZFEDIST.
  ENDIF.
*>> °ü¼¼»çÀÇ °Å·¡Ã³ ¹øÈ£ SELECT!
  CLEAR : ZTIMIMG10, LFA1.
  SELECT SINGLE * FROM ZTIMIMG10 WHERE ZFCUT EQ ZTIDR-ZFCUT.

*>> °ü¼¼»çÀÇ ±¸¸ÅÃ³ Á¤º¸ SELECT!
  SELECT SINGLE * FROM LFA1  WHERE  LIFNR  EQ  ZTIMIMG10-ZFVEN.

  IF LFA1-BAHNS IS INITIAL.
    MESSAGE E606(ZIM1) WITH ZTIMIMG10-ZFVEN.
  ENDIF.

* EDI CREATE
  PERFORM   P3000_MASTER_IDR_FLAT_CREATE.
* ¹®¼­ STATUS º¯?
  ZTIDR-ZFDOCST = 'R'.
  ZTIDR-ZFEDIST = 'S'.
  ZTIDR-ZFDOCNO = W_ZFDHENO.
  ZTIDR-UDAT    = SY-DATUM.
  ZTIDR-UNAM    = SY-UNAME.

ENDFORM.                    " P3000_ZTIDR_EDI_SEND
*&---------------------------------------------------------------------*
*&      Form  P3000_MASTER_IDR_FLAT_CREATE
*&---------------------------------------------------------------------*
FORM P3000_MASTER_IDR_FLAT_CREATE.

  REFRESH : IT_EDIFILE.

  W_ZFCDDOC = 'IMPREQ'.

  W_ZFDHSRO = LFA1-BAHNS.          " ½Äº°ÀÚ.
  W_ZFDHREF = ZTIDR-ZFBLNO.        " ÂüÁ¶¹øÈ£.
  MOVE '-'             TO W_ZFDHREF+10(1).
  MOVE ZTIDR-ZFCLSEQ   TO W_ZFDHREF+11(5).
*   W_ZFDHDDB = ZTIDR-UNAM.          " ºÎ¼­.
  W_ZFDHENO = ZTIDR-ZFDOCNO.       " ¹®¼­¹øÈ£.

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
            W_ZFCDDOC = W_ZFCDDOC
            W_ZFDHSRO = W_ZFDHSRO
            W_ZFDHREF = W_ZFDHREF
            W_BUKRS   = ZTIDR-BUKRS
       CHANGING
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4
            NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*>> EDI FLAT FILE CREATE FUNCTION CALL
  CALL FUNCTION 'ZIM_LG_IMPREQ_EDI_DOC'
       EXPORTING
            W_ZFBLNO     = ZTIDR-ZFBLNO
            W_ZFCLSEQ    = ZTIDR-ZFCLSEQ
            W_ZFDHENO    = W_ZFDHENO
            W_BAHNS      = LFA1-BAHNS
       IMPORTING
            W_EDI_RECORD = W_EDI_RECORD
       EXCEPTIONS
            CREATE_ERROR = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*>>> INTERNAL TABLE WRITE....
  IT_EDIFILE-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE.

*>>> LG-EDS VAN. SAM-FILE WRITE FUNCTION
  CALL FUNCTION 'ZIM_EDI_SAMFILE_WRITE'
       EXPORTING
            ZFCDDOC = W_ZFCDDOC
            BUKRS   = ZTIDR-BUKRS
       TABLES
            EDIFILE = IT_EDIFILE.

ENDFORM.                    " P3000_MASTER_IDR_FLAT_CREATE
*&---------------------------------------------------------------------*
*&      Form  P2000_CALC_ZTIDS_COST
*&---------------------------------------------------------------------*
FORM P2000_CALC_ZTIDS_COST.
  DATA : L_ZTIMIMG06_USD  LIKE ZTIMIMG06.
  DATA : L_ZFSTAMT        LIKE ZTIDS-ZFSTAMT.
  DATA : L_ZFSTAMT_WON    LIKE ZTIDS-ZFSTAMT.

* °ü¼¼,Åë°ü¼ö¼ö·á,¼±±ÞºÎ°¡?
  CLEAR : ZTBL, W_ZFCAMT, W_TOT_VAT, W_ZFCAMTU, W_AMOUNT_CNF,
          W_ZFCAMTU, W_ZFCAMTK, W_TOT_VAT, W_TOT_TAX, W_TOT_GAM,
          W_ZFINAMT, W_ZFTFA.

  SELECT SINGLE * FROM   ZTBL
  WHERE  ZFBLNO = ZTIDS-ZFBLNO.

*>È¯À².
  SELECT SINGLE * FROM ZTIMIMG06
         WHERE WAERS    EQ  ZTIDS-ZFSTAMC
         AND   ZFAPLDT  EQ ( SELECT MAX( ZFAPLDT )
                             FROM ZTIMIMG06
                             WHERE WAERS    EQ  ZTIDS-ZFSTAMC
                             AND   ZFAPLDT  LE  ZTIDS-ZFIDWDT
                             AND   ZFEXPDT  GE  ZTIDS-ZFIDWDT ).
  IF SY-SUBRC EQ 0.
    MOVE : ZTIMIMG06-ZFEXRT   TO   ZTIDS-ZFEXRT,
           ZTIMIMG06-FFACT    TO   ZTIDS-FFACT.
  ELSE.
    MESSAGE W503(ZIM1) WITH ZTIDS-ZFIDWDT.   EXIT.
  ENDIF.

  IF ZTIDS-FFACT IS INITIAL.
    ZTIDS-FFACT = 1.
  ENDIF.

  IF ZTIDS-ZFSTAMC NE 'USD'.
    SELECT SINGLE * INTO L_ZTIMIMG06_USD FROM ZTIMIMG06
           WHERE WAERS    EQ  'USD'
           AND   ZFAPLDT  EQ ( SELECT MAX( ZFAPLDT )
                               FROM ZTIMIMG06
                               WHERE WAERS    EQ  'USD'
                               AND   ZFAPLDT  LE  ZTIDS-ZFIDWDT
                               AND   ZFEXPDT  GE  ZTIDS-ZFIDWDT ).
    IF SY-SUBRC NE 0.
      MESSAGE W504(ZIM1) WITH ZTIDS-ZFIDWDT.   EXIT.
    ENDIF.
  ELSE.
    L_ZTIMIMG06_USD = ZTIMIMG06.
  ENDIF.

  IF L_ZTIMIMG06_USD-ZFEXRT IS INITIAL.
    MESSAGE W504(ZIM1) WITH ZTIDS-ZFIDWDT. EXIT.
  ENDIF.
  IF L_ZTIMIMG06_USD-FFACT IS INITIAL.
    MESSAGE W504(ZIM1) WITH ZTIDS-ZFIDWDT. EXIT.
  ENDIF.

  CLEAR : ZTIDS-ZFCUAMTS, ZTIDS-FWBAS, ZTIDS-ZFVAAMTS,
          W_ZFCAMTU, W_ZFCAMTK.

*>º¸Çè·á Ãâ·Â¿ëÀ¸·Î ÀüÈ¯.
  PERFORM SET_CURR_CONV_TO_EXTERNAL USING ZTIDS-ZFINAMT
                                         'KRW'
                                          W_ZFINAMT.
*>¿îÀÓ Ãâ·Â¿ëÀ¸·Î ÀüÈ¯.
  PERFORM SET_CURR_CONV_TO_EXTERNAL USING ZTIDS-ZFTFA
                                         'USD'
                                          W_ZFTFA.
*>USD ¿îÀÓ--->¿øÈ­.
  ZTIDS-ZFTFBC = 'KRW'.
  W_ZFTFA = W_ZFTFA * ( L_ZTIMIMG06_USD-ZFEXRT /
                        L_ZTIMIMG06_USD-FFACT ).
  ZTIDS-ZFTFB = W_ZFTFA.
  PERFORM SET_CURR_CONV_TO_INTERNAL USING ZTIDS-ZFTFB 'KRW'.

*> °áÁ¦±Ý¾× ---> ¿ÜºÎÇüÀ¸·Î º¯È¯.
  PERFORM SET_CURR_CONV_TO_EXTERNAL USING ZTIDS-ZFSTAMT
                                          ZTIDS-ZFSTAMC
                                          L_ZFSTAMT.
*> ¿øÈ­±Ý¾×À¸·Î È¯»ê.(°áÁ¦±Ý¾× ¿øÈ­) --> ÇØ?
  L_ZFSTAMT_WON = L_ZFSTAMT * ( ZTIMIMG06-ZFEXRT /
                                ZTIMIMG06-FFACT ).

*> ¶õº°·Î °ú¼¼±Ý¾× (¿øÈ­.¿ÜÈ­) °è»ê.
  LOOP AT IT_ZSIDSHS.
    W_TABIX = SY-TABIX.
*> °ú¼¼°¡°Ý ¿øÈ­.
    CLEAR : IT_ZSIDSHS-ZFTBAK.
    LOOP AT IT_ZSIDSHSD WHERE ZFCONO EQ IT_ZSIDSHS-ZFCONO.
      PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDSHSD-ZFAMT
                                              ZTIDS-ZFSTAMC
                                              BAPICURR-BAPICURR.
*> ¶õº° °ú¼¼°¡°Ý - ¿øÈ­.[
*  (°áÁ¦¿ÜÈ­±Ý¾× * È¯À²) + ¿îÀÓ¿øÈ­ + º¸Çè·á ¿ø?
      W_ZFCAMTK = BAPICURR-BAPICURR * ( ZTIDS-ZFEXRT / ZTIDS-FFACT )
             + W_ZFTFA   * ( BAPICURR-BAPICURR / L_ZFSTAMT )
*                    * ( ZTIDS-ZFEXRT / ZTIDS-FFACT )
             + W_ZFINAMT * ( BAPICURR-BAPICURR / L_ZFSTAMT ).

*        W_ZFCAMTU = BAPICURR-BAPICURR.
      PERFORM SET_CURR_CONV_TO_INTERNAL USING
               W_ZFCAMTK 'KRW'.

      ADD  W_ZFCAMTK    TO   IT_ZSIDSHS-ZFTBAK.
    ENDLOOP.
*>¿øÈ­±Ý?
*     PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDSHS-ZFTBAK
*                                             'KRW'
*                                             BAPICURR-BAPICURR.
*
*     IT_ZSIDSHS-ZFTBAK = TRUNC( BAPICURR-BAPICURR ).
*     PERFORM SET_CURR_CONV_TO_INTERNAL USING
*                                       IT_ZSIDSHS-ZFTBAK 'KRW'.

    ADD IT_ZSIDSHS-ZFTBAK TO W_ZFCAMT.

*-------------------------------------------------------------
*> °ú¼¼°¡°Ý - ¹ÌÈ­.
*  ( °ú¼¼°¡°Ý(¿øÈ­) / ¹ÌÈ­ °í½ÃÈ¯À² ) --> ¼Ò¼öÁ¡ ÀÌÇÏ Àý»ç.
*-------------------------------------------------------------
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDSHS-ZFTBAK
                                            'KRW'
                                            IT_ZSIDSHS-ZFTBAU.

    IT_ZSIDSHS-ZFTBAU = IT_ZSIDSHS-ZFTBAU /
                    ( L_ZTIMIMG06_USD-ZFEXRT / L_ZTIMIMG06_USD-FFACT ).
    IT_ZSIDSHS-ZFTBAU = TRUNC( IT_ZSIDSHS-ZFTBAU ).

    PERFORM SET_CURR_CONV_TO_INTERNAL USING
            IT_ZSIDSHS-ZFTBAU 'USD'.

    ADD   IT_ZSIDSHS-ZFTBAU   TO   W_ZFCAMTU.

*-------------------------------------------------------------
*> °ü¼¼.  --> ¿ø´ÜÀ§ ÀÌÇÏ Àý?
*  ( °ú¼¼°¡°Ý(¿øÈ­) * °ü¼¼À² ) .
*> ºÎ°¡¼¼.--> ¼Ò¼öÁ¡ ÀÌÇÏ Àý»ç.
*  ( °ú¼¼°¡°Ý(¿øÈ­) + °ü¼¼(¼Ò¼öÁ¡ ÀÌÇÏ Àý»ç) ) -
*-------------------------------------------------------------
*     PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDSHS-ZFCUAMT
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDSHS-ZFTBAK
                                            'KRW'
                                            IT_ZSIDSHS-ZFCUAMT.
*     IT_ZSIDSHS-ZFCUAMT = IT_ZSIDSHS-ZFCURT * IT_ZSIDSHS-ZFTBAK.
    IT_ZSIDSHS-ZFCUAMT = IT_ZSIDSHS-ZFCUAMT *
                       ( IT_ZSIDSHS-ZFCURT / 100 ).

*---> °ü¼¼°¨¸éÀ² Àû¿ë 2001.12.05 KSB MODIFY.
    IF NOT IT_ZSIDSHS-ZFRDRT IS INITIAL.
      IT_ZSIDSHS-ZFCUAMT = IT_ZSIDSHS-ZFCUAMT *
                       ( 1 - ( IT_ZSIDSHS-ZFRDRT / 100 ) ).
    ENDIF.

*>ºÎ°¡¼¼ °è»ê¿ë(¿ø´ÜÀ§ ÀÌÇÏ Àý»ç) / °ü¼¼(10¿ø´ÜÀ§ ÀÌÇÏ Àý»ç).
    W_TOT_GAM = TRUNC( IT_ZSIDSHS-ZFCUAMT ).           ">ºÎ°¡¼¼ °è»ê¿ë.
    IT_ZSIDSHS-ZFCUAMT = TRUNC( W_TOT_GAM / 10 ) * 10. ">°ü¼¼.
*    W_TOT_GAM = IT_ZSIDSHS-ZFCUAMT.

    PERFORM SET_CURR_CONV_TO_INTERNAL USING
                                      IT_ZSIDSHS-ZFCUAMT 'KRW'.

    ADD   IT_ZSIDSHS-ZFCUAMT   TO   ZTIDS-ZFCUAMTS.
*-------------------------------------------------------------
*> ºÎ°¡¼¼(°ú¼¼Ç¥ÁØ¾×).
*-------------------------------------------------------------
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDSHS-ZFTBAK
                                            'KRW'
                                            IT_ZSIDSHS-FWBAS.
    IT_ZSIDSHS-FWBAS = W_TOT_GAM + IT_ZSIDSHS-FWBAS.
    PERFORM SET_CURR_CONV_TO_INTERNAL USING
                                      IT_ZSIDSHS-FWBAS 'KRW'.
    ADD   IT_ZSIDSHS-FWBAS   TO  ZTIDS-FWBAS.

*-------------------------------------------------------------
*> ºÎ°¡¼¼.  ¿ø´ÜÀ§ ÀÌÇÏ Àý»ç..
*-------------------------------------------------------------
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIDSHS-FWBAS
                                            'KRW'
                                            IT_ZSIDSHS-ZFVAAMT.

    IT_ZSIDSHS-ZFVAAMT = TRUNC( IT_ZSIDSHS-ZFVAAMT / 100  ) * 10.
    PERFORM SET_CURR_CONV_TO_INTERNAL USING
                                      IT_ZSIDSHS-ZFVAAMT 'KRW'.

    ADD   IT_ZSIDSHS-ZFVAAMT TO ZTIDS-ZFVAAMTS.

    MODIFY IT_ZSIDSHS INDEX W_TABIX.
  ENDLOOP.

*>°ú¼¼°¡°Ý ¿øÈ­...
  ZTIDS-ZFTBAK = ZTIDS-ZFSTAMT * ( ZTIDS-ZFEXRT / ZTIDS-FFACT )
               + ZTIDS-ZFTFA
               * ( L_ZTIMIMG06_USD-ZFEXRT / L_ZTIMIMG06_USD-FFACT )
               + ZTIDS-ZFINAMT.

  IF W_ZFCAMT NE ZTIDS-ZFTBAK.
    IF ZTIDS-ZFTBAK GT W_ZFCAMT.
      ZTIDS-ZFTBAK = ZTIDS-ZFTBAK - ( ZTIDS-ZFTBAK - W_ZFCAMT ).
    ELSE.
      ZTIDS-ZFTBAK = ZTIDS-ZFTBAK + ( W_ZFCAMT - ZTIDS-ZFTBAK ).
    ENDIF.
    MODIFY IT_ZSIDSHS INDEX W_TABIX.
  ENDIF.

*>°ú¼¼°¡°Ý ¿ÜÈ­.
  ZTIDS-ZFTBAU = ZTIDS-ZFTBAK /
               ( L_ZTIMIMG06_USD-ZFEXRT * L_ZTIMIMG06_USD-FFACT ).

  IF W_ZFCAMTU NE ZTIDS-ZFTBAU.
    IF ZTIDS-ZFTBAU GT W_ZFCAMTU.
      ZTIDS-ZFTBAU = ZTIDS-ZFTBAU - ( ZTIDS-ZFTBAU - W_ZFCAMTU ).
    ELSE.
      ZTIDS-ZFTBAU = ZTIDS-ZFTBAU + ( W_ZFCAMTU - ZTIDS-ZFTBAU ).
    ENDIF.
    MODIFY IT_ZSIDSHS INDEX W_TABIX.
  ENDIF.

*>ÃÑ¼¼¾×.
  ZTIDS-ZFTXAMTS = ZTIDS-ZFCUAMTS + ZTIDS-ZFVAAMTS.

*> Åë°ü¼ö¼ö·á °è»ê.
  CALL FUNCTION 'ZIM_CC_TAX_CALCULATE'
       CHANGING
            ZTIDS = ZTIDS.

ENDFORM.                    " P2000_CALC_ZTIDS_COST
*&---------------------------------------------------------------------*
*&      Form  P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
FORM P1000_IMPORT_DOC_CHEKC USING    P_ZFIMDNO
                                     P_ZFDCNM
                                     P_ZFPOYN
                                     P_GUBUN
                                     P_KOSTL.

  CLEAR : P_ZFDCNM, ZTREQHD, ZTBL, ZTIV, ZTCGHD, ZTMSHD.

  IF P_ZFIMDNO IS INITIAL.
    EXIT.
  ENDIF.

  CASE ZTBKPF-ZFCSTGRP.
    WHEN '003'.           ">¼öÀÔÀÇ·Ú.
      SELECT SINGLE * FROM ZTREQHD
                      WHERE ZFREQNO EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      MOVE: ZTREQHD-ZFOPNNO TO P_ZFDCNM.
      P_ZFPOYN = 'Y'.
* CORECESS ÁÖ¼®Ã³¸®.
*         IF W_SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTREQHD-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.
    WHEN '004' OR '005'.  ">B/L °ü¸®¹øÈ£.
      SELECT SINGLE * FROM ZTBL
                      WHERE ZFBLNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      IF W_SUBRC EQ 0.
        P_ZFPOYN = ZTBL-ZFPOYN.
        P_KOSTL  = ZTBL-KOSTL.
      ELSE.
        P_ZFPOYN = 'Y'.
      ENDIF.
*> 1. È­¹°°ü¸®¹øÈ£.
      IF NOT ZTBL-ZFGMNO IS INITIAL.
        MOVE: ZTBL-ZFGMNO TO P_ZFDCNM.
        IF NOT ZTBL-ZFMSN IS INITIAL.
          CONCATENATE P_ZFDCNM '-' ZTBL-ZFMSN INTO P_ZFDCNM.
        ENDIF.
        IF NOT ZTBL-ZFHSN IS INITIAL.
          CONCATENATE P_ZFDCNM '-' ZTBL-ZFHSN INTO P_ZFDCNM.
        ENDIF.
      ELSE.
*> 2. HOUSE B/L No.
        IF NOT ZTBL-ZFHBLNO IS INITIAL.
          MOVE: ZTBL-ZFHBLNO TO P_ZFDCNM.
        ELSE.
*> 3. MASTER B/L No.
          IF NOT ZTBL-ZFMBLNO IS INITIAL.
            MOVE: ZTBL-ZFMBLNO TO P_ZFDCNM.
          ELSE.
*> 4. ¼±»ç B/L No.
            IF NOT ZTBL-ZFCGHNO IS INITIAL.
              MOVE: ZTBL-ZFCGHNO TO P_ZFDCNM.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
* CORECESS ÁÖ¼®Ã³¸®.
*         IF W_SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTBL-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.

    WHEN '006'.           ">Åë°ü°ü¸®¹øÈ£.
      SELECT SINGLE * FROM ZTIV
                      WHERE ZFIVNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.

      IF W_SUBRC EQ 0.
        P_ZFPOYN = ZTIV-ZFPOYN.
      ELSE.
        P_ZFPOYN = 'Y'.
      ENDIF.

      IF W_SUBRC EQ 0.
        IF ZTIV-ZFCUST EQ '3' OR ZTIV-ZFCUST EQ 'Y'.
*               SELECT SINGLE * FROM ZTCUCLIV
*                               WHERE ZFIVNO EQ P_ZFIMDNO.
*               IF SY-SUBRC EQ 0.
          SELECT SINGLE * FROM ZTIDR
                      WHERE ZFIVNO EQ P_ZFIMDNO.
*                                  WHERE ZFBLNO  EQ ZTCUCLIV-ZFBLNO
*                                  AND   ZFCLSEQ EQ ZTCUCLIV-ZFCLSEQ.
          IF SY-SUBRC EQ 0.
            IF ZTIDR-ZFIDRNO IS INITIAL.
              MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
            ELSE.
              MOVE: ZTIDR-ZFIDRNO TO P_ZFDCNM.
            ENDIF.
          ELSE.
            MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
          ENDIF.
*               ELSE.
*                  MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
*               ENDIF.
        ELSE.
          MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
        ENDIF.
      ENDIF.
      SELECT SINGLE * FROM ZTBL
             WHERE ZFBLNO EQ ZTIV-ZFBLNO.

* CORECESS ÁÖ¼®Ã³¸®.
*         IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*            P_KOSTL  = ZTBL-KOSTL.
*            P_ZFPOYN = ZTBL-ZFPOYN.
*
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTBL-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.

    WHEN '007'.           ">ÇÏ¿ª°ü¸®¹øÈ£.
      SELECT SINGLE * FROM ZTCGHD
                      WHERE ZFCGNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      P_ZFPOYN = 'Y'.
      IF SY-SUBRC EQ 0.
        IF NOT ZTCGHD-ZFMSNO IS INITIAL.
          SELECT SINGLE * FROM  ZTMSHD
                          WHERE ZFMSNO  EQ  ZTCGHD-ZFMSNO.
          IF SY-SUBRC EQ 0.
            MOVE ZTMSHD-ZFMSNM  TO  P_ZFDCNM.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '008'.           ">±â³³Áõ¸®¹øÈ£.
      SELECT SINGLE * FROM ZTTAXBKHD
                      WHERE ZFTBNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      IF SY-SUBRC EQ 0.
        IF ZTTAXBKHD-BASISNO IS INITIAL.
          MOVE ZTTAXBKHD-EBELN    TO  P_ZFDCNM.
        ELSE.
          MOVE ZTTAXBKHD-BASISNO  TO  P_ZFDCNM.
        ENDIF.
      ELSE.
        CLEAR : P_ZFDCNM.
      ENDIF.
      SELECT SINGLE * FROM ZTREQHD
             WHERE ZFREQNO EQ ZTTAXBKHD-ZFREQNO.
*          IF SY-SUBRC EQ 0.
* CORECESS ÁÖ¼®Ã³¸®.
*         IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTREQHD-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

*>>¿À·ù°¡ ¹ß»ýÇßÀ» °æ¿ì.
  IF W_SUBRC NE 0.
    IF P_GUBUN EQ 'H'.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'ZFIMDNO'.
    ELSE.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBSEG' 'ZFIMDNO'.
    ENDIF.

    CASE ZTBKPF-ZFCSTGRP.
      WHEN '003'.
        MESSAGE E585 WITH 'Import request No' P_ZFIMDNO.
      WHEN '004' OR '005'.
        MESSAGE E585 WITH 'B/L No' P_ZFIMDNO.
      WHEN '006'.
        MESSAGE E585 WITH 'Clearance No' P_ZFIMDNO.
      WHEN '007'.
        MESSAGE E585 WITH 'Loading/Unloading No' P_ZFIMDNO.
      WHEN '008'.
        MESSAGE E585 WITH 'CTM No' P_ZFIMDNO.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_NEWKO
*&---------------------------------------------------------------------*
FORM P2000_SET_NEWKO   USING ZSBSEG-ZFCSTGRT
                             ZSBSEG-NEWKO
                             ZSBSEG-ZFCD
                             ZSBSEG-ZFIMDNO.
*> °èÁ¤°áÁ¤ ÇÔ¼ö.
  CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
       EXPORTING
            ZFCSTGRP = ZSBSEG-ZFCSTGRT
            ZFCD     = ZSBSEG-ZFCD
            ZFIMDNO  = ZSBSEG-ZFIMDNO
       IMPORTING
            NEWKO    = ZSBSEG-NEWKO.

ENDFORM.                    " P2000_SET_NEWKO
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIDR_CALL
*&---------------------------------------------------------------------*
FORM P2000_ZTIDR_CALL USING    P_ZFBLNO
                               P_ZFCLSEQ.

  IF P_ZFBLNO IS INITIAL OR  P_ZFCLSEQ IS INITIAL.
    MESSAGE E781.
  ENDIF.

  SET PARAMETER ID 'ZPBLNO'     FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPCLSEQ'    FIELD P_ZFCLSEQ.
  SET PARAMETER ID 'ZPHBLNO'    FIELD ''.
  SET PARAMETER ID 'ZPIDRNO'    FIELD ''.

  CALL TRANSACTION 'ZIM63' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_ZTIDR_CALL
*&---------------------------------------------------------------------*
*&      Form  P2000_CREATE_BLCOST_TREE
*&---------------------------------------------------------------------*
FORM P2000_CREATE_BLCOST_TREE.

  DATA: NODE_TABLE TYPE TREEV_NTAB,
        ITEM_TABLE TYPE ITEM_TABLE_TYPE,
        EVENT TYPE CNTL_SIMPLE_EVENT,
        EVENTS TYPE CNTL_SIMPLE_EVENTS,
        HIERARCHY_HEADER TYPE TREEV_HHDR.

  CREATE OBJECT G_APPLICATION.

* create a container for the tree control
  CREATE OBJECT G_CUSTOM_CONTAINER
    EXPORTING
      " the container is linked to the custom control with the
      " name 'TREE_CONTAINER' on the dynpro
      CONTAINER_NAME = 'TREE_CONTAINER'
    EXCEPTIONS
      CNTL_ERROR = 1
      CNTL_SYSTEM_ERROR = 2
      CREATE_ERROR = 3
      LIFETIME_ERROR = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* setup the hierarchy header
  HIERARCHY_HEADER-HEADING = 'Posting Yes/No'.
  " heading
  HIERARCHY_HEADER-WIDTH = 14.         " width: 30 characters

* create a tree control

* After construction, the control contains one column in the
* hierarchy area. The name of this column
* is defined via the constructor parameter HIERACHY_COLUMN_NAME.
  CREATE OBJECT G_TREE
    EXPORTING
      PARENT              = G_CUSTOM_CONTAINER
      NODE_SELECTION_MODE = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION = 'X'
      HIERARCHY_COLUMN_NAME = 'Column1'
      HIERARCHY_HEADER = HIERARCHY_HEADER
    EXCEPTIONS
      CNTL_SYSTEM_ERROR           = 1
      CREATE_ERROR                = 2
      FAILED                      = 3
      ILLEGAL_NODE_SELECTION_MODE = 4
      ILLEGAL_COLUMN_NAME         = 5
      LIFETIME_ERROR              = 6.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* define the events which will be passed to the backend
  " node double click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'. " process PAI if event occurs
  APPEND EVENT TO EVENTS.

  " item double click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " expand no children
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_EXPAND_NO_CHILDREN.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " link click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_LINK_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " button click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_BUTTON_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " checkbox change
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_CHECKBOX_CHANGE.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " header click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_HEADER_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  CALL METHOD G_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS                    = EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* assign event handlers in the application class to each desired event
  SET HANDLER G_APPLICATION->HANDLE_NODE_DOUBLE_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_ITEM_DOUBLE_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_EXPAND_NO_CHILDREN FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_LINK_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_BUTTON_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_CHECKBOX_CHANGE FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_HEADER_CLICK FOR G_TREE.

* insert two additional columns

* Column2
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME                         = 'Column2'
      WIDTH                        = 20
      HEADER_TEXT                  = 'Payee'
    EXCEPTIONS
      COLUMN_EXISTS                = 1
      ILLEGAL_COLUMN_NAME          = 2
      TOO_MANY_COLUMNS             = 3
      ILLEGAL_ALIGNMENT            = 4
      DIFFERENT_COLUMN_TYPES       = 5
      CNTL_SYSTEM_ERROR            = 6
      FAILED                       = 7
      PREDECESSOR_COLUMN_NOT_FOUND = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.
* Column4
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME = 'Column3'
      WIDTH = 3
*      ALIGNMENT = CL_GUI_COLUMN_TREE=>ALIGN_RIGHT
      HEADER_TEXT = 'Tax'
    EXCEPTIONS
      COLUMN_EXISTS                 = 1
      ILLEGAL_COLUMN_NAME           = 2
      TOO_MANY_COLUMNS              = 3
      ILLEGAL_ALIGNMENT             = 4
      DIFFERENT_COLUMN_TYPES        = 5
      CNTL_SYSTEM_ERROR             = 6
      FAILED                        = 7
      PREDECESSOR_COLUMN_NOT_FOUND  = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* Column4
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME                         = 'Column4'
      WIDTH                        = 2
      HEADER_TEXT                  = 'Document Type'
    EXCEPTIONS
      COLUMN_EXISTS                = 1
      ILLEGAL_COLUMN_NAME          = 2
      TOO_MANY_COLUMNS             = 3
      ILLEGAL_ALIGNMENT            = 4
      DIFFERENT_COLUMN_TYPES       = 5
      CNTL_SYSTEM_ERROR            = 6
      FAILED                       = 7
      PREDECESSOR_COLUMN_NOT_FOUND = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* Column5
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME                         = 'Column5'
      WIDTH                        = 3
      HEADER_TEXT                  = 'Currency'
    EXCEPTIONS
      COLUMN_EXISTS                = 1
      ILLEGAL_COLUMN_NAME          = 2
      TOO_MANY_COLUMNS             = 3
      ILLEGAL_ALIGNMENT            = 4
      DIFFERENT_COLUMN_TYPES       = 5
      CNTL_SYSTEM_ERROR            = 6
      FAILED                       = 7
      PREDECESSOR_COLUMN_NOT_FOUND = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* Column6
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME                         = 'Column6'
      WIDTH                        = 4
      HEADER_TEXT                  = 'Payment Term'
    EXCEPTIONS
      COLUMN_EXISTS                = 1
      ILLEGAL_COLUMN_NAME          = 2
      TOO_MANY_COLUMNS             = 3
      ILLEGAL_ALIGNMENT            = 4
      DIFFERENT_COLUMN_TYPES       = 5
      CNTL_SYSTEM_ERROR            = 6
      FAILED                       = 7
      PREDECESSOR_COLUMN_NOT_FOUND = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* add some nodes to the tree control
* NOTE: the tree control does not store data at the backend. If an
* application wants to access tree data later, it must store the
* tree data itself.

  PERFORM P2000_BUILD_COST_NODE_TABLE USING NODE_TABLE ITEM_TABLE.

  CALL METHOD G_TREE->ADD_NODES_AND_ITEMS
    EXPORTING
      NODE_TABLE                     = NODE_TABLE
      ITEM_TABLE                     = ITEM_TABLE
      ITEM_TABLE_STRUCTURE_NAME      = 'MTREEITM'
    EXCEPTIONS
      FAILED                         = 1
      CNTL_SYSTEM_ERROR              = 3
      ERROR_IN_TABLES                = 4
      DP_ERROR                       = 5
      TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* expand the node with key 'Root'
  LOOP AT IT_BLCSTHD.
    G_NODE_KEY = IT_BLCSTHD-ZFSEQ.
    CALL METHOD G_TREE->EXPAND_NODE
      EXPORTING
        NODE_KEY            = G_NODE_KEY                    "#EC NOTEXT
      EXCEPTIONS
        FAILED              = 1
        ILLEGAL_LEVEL_COUNT = 2
        CNTL_SYSTEM_ERROR   = 3
        NODE_NOT_FOUND      = 4
        CANNOT_EXPAND_LEAF  = 5.
    IF SY-SUBRC <> 0.
      MESSAGE A000.
    ENDIF.
    CLEAR : G_NODE_KEY.
  ENDLOOP.

ENDFORM.                    " P2000_CREATE_BLCOST_TREE
*&---------------------------------------------------------------------*
*&      Form  P2000_BUILD_COST_NODE_TABLE
*&---------------------------------------------------------------------*
FORM P2000_BUILD_COST_NODE_TABLE
  USING
    NODE_TABLE TYPE TREEV_NTAB
    ITEM_TABLE TYPE ITEM_TABLE_TYPE.

  DATA: NODE TYPE TREEV_NODE,
        ITEM TYPE MTREEITM,
        SEQ  TYPE I.

* Build the node table.

* Caution: The nodes are inserted into the tree according to the order
* in which they occur in the table. In consequence, a node must not
* must not occur in the node table before its parent node.

* Node with key 'Root'
*  NODE-NODE_KEY = 'Root'.   "#EC NOTEXT
*                            " Key of the node
*  CLEAR NODE-RELATKEY.      " Special case: A root node has no parent
*  CLEAR NODE-RELATSHIP.     " node.
*
*  NODE-HIDDEN = ' '.        " The node is visible,
*  NODE-DISABLED = ' '.      " selectable,
*  NODE-ISFOLDER = 'X'.      " a folder.
*  CLEAR NODE-N_IMAGE.       " Folder-/ Leaf-Symbol in state "closed":
*                            " use default.
*  CLEAR NODE-EXP_IMAGE.     " Folder-/ Leaf-Symbol in state "open":
*                            " use default
*  CLEAR NODE-EXPANDER.      " see below.
*  APPEND NODE TO NODE_TABLE.

* Node with key 'Child1'
  LOOP AT IT_BLCSTHD.
    NODE-NODE_KEY = IT_BLCSTHD-ZFSEQ.                       "#EC NOTEXT
    " Key of the node
* Node is inserted as child of the node with key 'Root'.
*     NODE-RELATKEY = 'Root'.
    CLEAR : NODE-RELATKEY.
*     NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
    CLEAR : NODE-RELATSHIP.

    NODE-HIDDEN = ' '.
    NODE-DISABLED = ' '.
    NODE-ISFOLDER = 'X'.
    CLEAR NODE-N_IMAGE.
    CLEAR NODE-EXP_IMAGE.
*    NODE-T_IMAGE = '@0B@'.
    NODE-EXPANDER = 'X'. " The node is marked with a '+', although
*                      " it has no children. When the user clicks on the
*                      " + to open the node, the event expand_nc is
*                      " fired. The programmerr can
*                      " add the children of the
*                      " node within the event handler of the expand_nc
*                      " event  (see callback handle_expand_nc).
    APPEND NODE TO NODE_TABLE.
  ENDLOOP.

  CLEAR : SEQ.
  LOOP AT IT_ZSBSEG.
    ADD 1 TO SEQ.
    NODE-NODE_KEY = ( IT_ZSBSEG-ZFSEQ * 10000 ) + SEQ.
    " Key of the node
* Node is inserted as child of the node with key 'Root'.
    NODE-RELATKEY = IT_ZSBSEG-ZFSEQ.
*     CLEAR : NODE-RELATKEY.
    NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
*     CLEAR : NODE-RELATSHIP.

    NODE-HIDDEN = ' '.
    NODE-DISABLED = ' '.
    NODE-ISFOLDER = ' '.
    CLEAR NODE-N_IMAGE.
    CLEAR NODE-EXP_IMAGE.
    NODE-EXPANDER = ' '. " The node is marked with a '+', although
*                      " it has no children. When the user clicks on the
*                      " + to open the node, the event expand_nc is
*                      " fired. The programmerr can
*                      " add the children of the
*                      " node within the event handler of the expand_nc
*                      " event  (see callback handle_expand_nc).
    APPEND NODE TO NODE_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = NODE-NODE_KEY.
    ITEM-ITEM_NAME = 'Column1'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    ITEM-TEXT = IT_ZSBSEG-ZFCDNM.                           "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    READ TABLE IT_ZSBKPF WITH KEY ZFSEQ = IT_ZSBSEG-ZFSEQ.
    WRITE IT_ZSBSEG-DMBTR CURRENCY IT_ZSBKPF-HWAER TO
          W_AMTTXT1.
    ITEM-NODE_KEY = NODE-NODE_KEY.
    ITEM-ITEM_NAME = 'Column2'.     "
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = W_AMTTXT1.                                  "#EC NOTEXT
*     ITEM-T_IMAGE = '@0B@'.
    APPEND ITEM TO ITEM_TABLE.
  ENDLOOP.

* The items of the nodes:

* Node with key 'Root'
*  CLEAR ITEM.
*  ITEM-NODE_KEY = 'Root'.
*  ITEM-ITEM_NAME = 'Column1'.     " Item of Column 'Column1'
*  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
*  ITEM-TEXT = 'ºñ¿ëÀü±â ±×·ì'. "#EC NOTEXT
*  APPEND ITEM TO ITEM_TABLE.

*  CLEAR ITEM.
*  ITEM-NODE_KEY = 'Root'.
*  ITEM-ITEM_NAME = 'Column2'.     " Item of Column 'Column2'
*  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
**  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_CHECKBOX.
*  ITEM-TEXT = 'Root Col. 2'. "#EC NOTEXT
*  APPEND ITEM TO ITEM_TABLE.

*  CLEAR ITEM.
*  ITEM-NODE_KEY = 'Root'.
*  ITEM-ITEM_NAME = 'Column3'.     " Item of Column 'Column3'
*  " Item is a link (click on link fires event LINK_CLICK)
*  ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
*  ITEM-TEXT = 'Root Col. 3'. "#EC NOTEXT "
*  APPEND ITEM TO ITEM_TABLE.

* Node with key 'Child1'
  LOOP AT IT_BLCSTHD.
    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column1'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_CHECKBOX.
    ITEM-TEXT = IT_BLCSTHD-TEXT.                            "#EC NOTEXT
    ITEM-EDITABLE = 'X'.
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column2'.     "
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
* ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_BUTTON. " Item is a button
    ITEM-TEXT = IT_BLCSTHD-NAME1.                           "#EC NOTEXT
*     ITEM-T_IMAGE = '@0B@'.
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column3'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_BLCSTHD-BUPLA.                           "#EC NOTEXT
*  ITEM-T_IMAGE = '@0C@'.
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column4'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_BLCSTHD-MWSKZ.                           "#EC NOTEXT
*  ITEM-T_IMAGE = '@0C@'.
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column5'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_BLCSTHD-BLART.                           "#EC NOTEXT
*  ITEM-T_IMAGE = '@0C@'.
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column6'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_BLCSTHD-WAERS.                           "#EC NOTEXT
*  ITEM-T_IMAGE = '@0C@'.
    APPEND ITEM TO ITEM_TABLE.

  ENDLOOP.

ENDFORM.                    " P2000_BUILD_COST_NODE_TABLE
*&---------------------------------------------------------------------*
*&      Form  P2000_BLCST_TREE_EVENT_HANDLER
*&---------------------------------------------------------------------*
FORM P2000_BLCST_TREE_EVENT_HANDLER.
  DATA : L_NODE_KEY   TYPE   I.

  CASE G_EVENT.
    WHEN 'ITEM_DOUBLE_CLICK' OR 'NODE_DOUBLE_CLICK'.
      IF G_NODE_KEY EQ 'Root' OR G_NODE_KEY IS INITIAL.
        MESSAGE S903(ZIM1).
        EXIT.
      ELSE.
        L_NODE_KEY = G_NODE_KEY.
        IF L_NODE_KEY GT 10000.
          MESSAGE S904(ZIM1).
          EXIT.
        ELSE.
*             G_ZFSEQ = G_NODE_KEY.
          IF NOT G_NODE_KEY_OLD IS INITIAL AND
             G_NODE_KEY_OLD  NE G_NODE_KEY.
            G_ZFSEQ = G_NODE_KEY_OLD.
            MOVE-CORRESPONDING: ZSBKPF  TO  IT_ZSBKPF.
            MODIFY IT_ZSBKPF INDEX G_ZFSEQ.
            DELETE IT_ZSBSEG WHERE ZFSEQ EQ G_ZFSEQ.
            LOOP AT IT_ZSBSEG_TMP.
              MOVE-CORRESPONDING IT_ZSBSEG_TMP TO IT_ZSBSEG.
              APPEND IT_ZSBSEG.
            ENDLOOP.
          ENDIF.

          G_ZFSEQ = G_NODE_KEY.
          READ TABLE IT_ZSBKPF WITH KEY ZFSEQ = G_ZFSEQ.
          IF SY-SUBRC EQ 0.
            READ TABLE IT_BLCSTHD WITH KEY ZFSEQ = G_ZFSEQ.
            MOVE-CORRESPONDING IT_ZSBKPF TO ZSBKPF.
            REFRESH : IT_ZSBSEG_TMP.
            LOOP AT IT_ZSBSEG WHERE ZFSEQ EQ G_ZFSEQ.
              MOVE-CORRESPONDING IT_ZSBSEG TO IT_ZSBSEG_TMP.
              APPEND IT_ZSBSEG_TMP.
            ENDLOOP.
          ENDIF.
          G_NODE_KEY_OLD = G_NODE_KEY.
        ENDIF.
      ENDIF.
    WHEN 'CHECKBOX_CHANGE'.
      IF G_NODE_KEY EQ 'Root' OR G_NODE_KEY IS INITIAL.
        MESSAGE S903(ZIM1).  EXIT.
      ELSE.
        L_NODE_KEY = G_NODE_KEY.
        IF L_NODE_KEY GT 10000.
          MESSAGE S904(ZIM1).
          EXIT.
        ELSE.
*             G_ZFSEQ = G_NODE_KEY.
          IF NOT G_NODE_KEY_OLD IS INITIAL AND
             G_NODE_KEY_OLD  NE G_NODE_KEY.
            G_ZFSEQ = G_NODE_KEY_OLD.
            MOVE-CORRESPONDING: ZSBKPF  TO  IT_ZSBKPF.
            MODIFY IT_ZSBKPF INDEX G_ZFSEQ.
            DELETE IT_ZSBSEG WHERE ZFSEQ EQ G_ZFSEQ.
            LOOP AT IT_ZSBSEG_TMP.
              MOVE-CORRESPONDING IT_ZSBSEG_TMP TO IT_ZSBSEG.
              APPEND IT_ZSBSEG.
            ENDLOOP.
          ENDIF.

          G_ZFSEQ = G_NODE_KEY.
          READ TABLE IT_ZSBKPF WITH KEY ZFSEQ = G_ZFSEQ.
          IF SY-SUBRC EQ 0.
            IF IT_ZSBKPF-ZFMARK EQ 'X'.
              CLEAR:IT_ZSBKPF-ZFMARK.
            ELSE.
              IT_ZSBKPF-ZFMARK = 'X'.
            ENDIF.
            MODIFY IT_ZSBKPF INDEX SY-TABIX.
          ENDIF.
          READ TABLE IT_ZSBKPF WITH KEY ZFSEQ = G_ZFSEQ.
          IF SY-SUBRC EQ 0.
            READ TABLE IT_BLCSTHD WITH KEY ZFSEQ = G_ZFSEQ.
            MOVE-CORRESPONDING IT_ZSBKPF TO ZSBKPF.
            REFRESH : IT_ZSBSEG_TMP.
            LOOP AT IT_ZSBSEG WHERE ZFSEQ EQ G_ZFSEQ.
              MOVE-CORRESPONDING IT_ZSBSEG TO IT_ZSBSEG_TMP.
              APPEND IT_ZSBSEG_TMP.
            ENDLOOP.
          ENDIF.
          G_NODE_KEY_OLD = G_NODE_KEY.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_BLCST_TREE_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&      Form  P3000_COST_POSTING
*&---------------------------------------------------------------------*
FORM P3000_COST_POSTING USING P_ZFSEQ.

  SET UPDATE TASK LOCAL.
  CLEAR : *ZTBKPF.

  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
       EXPORTING
            W_OK_CODE    = 'SAVE'
            BUKRS        = ZTBKPF-BUKRS
            GJAHR        = ZTBKPF-GJAHR
            ZFSTATUS     = 'C'
            W_ZTBKPF_OLD = *ZTBKPF
            W_ZTBKPF     = ZTBKPF
       TABLES
            IT_ZSBSEG    = IT_ZSBSEG
       CHANGING
            BELNR        = ZTBKPF-BELNR
       EXCEPTIONS
            ERROR_UPDATE = 4.

  W_SUBRC = SY-SUBRC.
  ZTIV-ZFIVNO = ZTBL-ZFBLNO.

  IF W_SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE S494.
    PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM ZTBKPF
  WHERE  BUKRS    EQ   ZTBKPF-BUKRS
  AND    BELNR    EQ   ZTBKPF-BELNR
  AND    GJAHR    EQ   ZTBKPF-GJAHR.

  IF ZTBKPF-ZFOVROW NE 'X'.
     CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST'
          EXPORTING
              BUKRS            = ZTBKPF-BUKRS
              BELNR            = ZTBKPF-BELNR
              GJAHR            = ZTBKPF-GJAHR
          IMPORTING
              INVOICEDOCNUMBER = INVOICEDOCNUMBER
              FISCALYEAR       = FISCALYEAR
          TABLES
              RETURN           = RETURN
          EXCEPTIONS
            POST_ERROR       = 4.
  ELSE.
     CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST_CR'
          EXPORTING
              BUKRS            = ZTBKPF-BUKRS
              BELNR            = ZTBKPF-BELNR
              GJAHR            = ZTBKPF-GJAHR
          IMPORTING
              INVOICEDOCNUMBER = INVOICEDOCNUMBER
              FISCALYEAR       = FISCALYEAR
          TABLES
              RETURN           = RETURN
          EXCEPTIONS
            POST_ERROR       = 4.
  ENDIF.

  W_SUBRC = SY-SUBRC.
  ZTIV-ZFIVNO = ZTBL-ZFBLNO.

  IF W_SUBRC EQ 0.
    LOOP AT IT_ZSBLCST_POST WHERE ZFSEQ EQ P_ZFSEQ.
      W_TABIX = SY-TABIX.
      READ TABLE IT_BLCSTHD WITH KEY ZFSEQ = P_ZFSEQ.

      IT_ZSBLCST_POST-ZFFIYR = FISCALYEAR.
      IT_ZSBLCST_POST-ZFACDO = INVOICEDOCNUMBER.

      READ TABLE IT_ZSBKPF WITH KEY ZFSEQ = P_ZFSEQ.
*         IF IT_ZSBLCST_POST-ZFPSDT IS INITIAL.
      IT_ZSBLCST_POST-ZFPSDT = IT_ZSBKPF-BLDAT.
*         ENDIF.
      IT_ZSBLCST_POST-ZFOCDT = IT_ZSBKPF-BUDAT.

      IT_ZSBLCST_POST-ZTERM = IT_ZSBKPF-ZTERM.
      IT_ZSBLCST_POST-BLART = IT_ZSBKPF-BLART.
*         IT_ZSBLCST_POST-MWSKZ = IT_ZSBKPF-MWSKZ.
      IT_ZSBLCST_POST-UNAM  = SY-UNAME.
      IT_ZSBLCST_POST-UDAT  = SY-DATUM.

      MODIFY IT_ZSBLCST_POST INDEX W_TABIX.
    ENDLOOP.
    MODIFY ZTBLCST FROM TABLE IT_ZSBLCST_POST.
*      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST.
    PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST
                                   USING   ZTBL-ZFBLNO.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.

    IF RETURN[] IS INITIAL.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST.
    ELSE.
      PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST
                                     USING   ZTBL-ZFBLNO.
    ENDIF.

    CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
         EXPORTING
              W_OK_CODE    = 'DELE'
              BUKRS        = ZTBKPF-BUKRS
              GJAHR        = ZTBKPF-GJAHR
              ZFSTATUS     = C_REQ_U
              W_ZTBKPF_OLD = *ZTBKPF
              W_ZTBKPF     = ZTBKPF
         TABLES
              IT_ZSBSEG    = IT_ZSBSEG
         CHANGING
              BELNR        = ZTBKPF-BELNR
         EXCEPTIONS
              ERROR_UPDATE = 4.

*      W_SUBRC = SY-SUBRC.
*      COMMIT WORK.

*      IF W_SUBRC EQ 0.
*         MESSAGE S494.
*      ELSE.
*         MESSAGE S313.
*      ENDIF.
*      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST.
*         ROLLBACK WORK.
  ENDIF.

*      DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
*      IF W_LINE GT 0.
*         INCLUDE = 'POPU'.
*         CALL SCREEN 0014 STARTING AT  04   3
*                          ENDING   AT  100 12.
*         CLEAR : INCLUDE.
*      ENDIF.
*
*      CLEAR OK-CODE.
*      PERFORM  P2000_SET_UNLOCK.
*      LEAVE TO TRANSACTION SY-TCODE AND SKIP FIRST SCREEN.
**     PERFORM  P2000_SET_SCREEN_SCRCOM.
**     LEAVE SCREEN.
*   ELSE.
*      PERFORM  P2000_SET_UNLOCK.
*      PERFORM  P2000_SET_SCREEN_SCRCOM.
*      LEAVE SCREEN.
*   ENDIF.
*

ENDFORM.                    " P3000_COST_POSTING
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DATA_MODIFY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_BL_DATA_MODIFY_CHECK.

*> Áß·®,¿ëÀû ÀÔ·Â¿©ºÎÃ¼Å©.(ÇÑ¼ö¿ø 20021016 JSY)
*  PERFORM P3000_WIGHT_INPUT_CHECK.  " NHJ COMMENT(TEMP)

*> B/L HEADER ³»¿ë ºñ±³.
  IF ZTBL NE *ZTBL.
    W_MODIF_BIT = 'Y'.
    EXIT.
  ENDIF.

*> B/L ÇØ¿Ü¿îÀÓ°ü·Ã ³»¿ë ºñ±³.
  LOOP AT IT_ZSBLCST_ORG.
    READ TABLE IT_ZSBLCST WITH KEY ZFCSQ = IT_ZSBLCST_ORG-ZFCSQ.
    IF SY-SUBRC NE 0.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
    IF IT_ZSBLCST_ORG-ZFCSCD  NE IT_ZSBLCST-ZFCSCD  OR
       IT_ZSBLCST_ORG-ZFCAMT  NE IT_ZSBLCST-ZFCAMT  OR
       IT_ZSBLCST_ORG-WAERS   NE IT_ZSBLCST-WAERS   OR
       IT_ZSBLCST_ORG-ZFCKAMT NE IT_ZSBLCST-ZFCKAMT OR
       IT_ZSBLCST_ORG-ZFEXRT  NE IT_ZSBLCST-ZFEXRT  OR
       IT_ZSBLCST_ORG-ZFVAT   NE IT_ZSBLCST-ZFVAT   OR
       IT_ZSBLCST_ORG-ZFOCDT  NE IT_ZSBLCST-ZFOCDT  OR
       IT_ZSBLCST_ORG-ZFPSDT  NE IT_ZSBLCST-ZFPSDT  OR
       IT_ZSBLCST_ORG-BUKRS   NE IT_ZSBLCST-BUKRS   OR
       IT_ZSBLCST_ORG-ZFFIYR  NE IT_ZSBLCST-ZFFIYR  OR
       IT_ZSBLCST_ORG-ZFACDO  NE IT_ZSBLCST-ZFACDO  OR
       IT_ZSBLCST_ORG-ZFVEN   NE IT_ZSBLCST-ZFVEN   OR
       IT_ZSBLCST_ORG-ZFPAY   NE IT_ZSBLCST-ZFPAY   OR
       IT_ZSBLCST_ORG-ZFCSTYN NE IT_ZSBLCST-ZFCSTYN OR
       IT_ZSBLCST_ORG-ZFUPCST NE IT_ZSBLCST-ZFUPCST OR
       IT_ZSBLCST_ORG-ZFPCST  NE IT_ZSBLCST-ZFPCST  OR
       IT_ZSBLCST_ORG-ZTERM   NE IT_ZSBLCST-ZTERM   OR
       IT_ZSBLCST_ORG-ZFWERKS NE IT_ZSBLCST-ZFWERKS OR
       IT_ZSBLCST_ORG-MWSKZ   NE IT_ZSBLCST-MWSKZ   OR
       IT_ZSBLCST_ORG-KBETR   NE IT_ZSBLCST-KBETR   OR
       IT_ZSBLCST_ORG-KONWA   NE IT_ZSBLCST-KONWA.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF W_MODIF_BIT EQ 'Y'.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSBLCST.
    READ TABLE IT_ZSBLCST_ORG WITH KEY ZFCSQ = IT_ZSBLCST-ZFCSQ.
    IF SY-SUBRC NE 0.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF W_MODIF_BIT EQ 'Y'.
    EXIT.
  ENDIF.

*> B/L º¸¼¼¿îÀÓ°ü·Ã ³»¿ë ºñ±³.
  LOOP AT IT_ZSBLCST1_ORG.
    READ TABLE IT_ZSBLCST1 WITH KEY ZFCSQ = IT_ZSBLCST1_ORG-ZFCSQ.
    IF SY-SUBRC NE 0.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
    IF IT_ZSBLCST1_ORG-ZFCSCD  NE IT_ZSBLCST1-ZFCSCD  OR
       IT_ZSBLCST1_ORG-ZFCAMT  NE IT_ZSBLCST1-ZFCAMT  OR
       IT_ZSBLCST1_ORG-WAERS   NE IT_ZSBLCST1-WAERS   OR
       IT_ZSBLCST1_ORG-ZFCKAMT NE IT_ZSBLCST1-ZFCKAMT OR
       IT_ZSBLCST1_ORG-ZFEXRT  NE IT_ZSBLCST1-ZFEXRT  OR
       IT_ZSBLCST1_ORG-ZFVAT   NE IT_ZSBLCST1-ZFVAT   OR
       IT_ZSBLCST1_ORG-ZFOCDT  NE IT_ZSBLCST1-ZFOCDT  OR
       IT_ZSBLCST1_ORG-ZFPSDT  NE IT_ZSBLCST1-ZFPSDT  OR
       IT_ZSBLCST1_ORG-BUKRS   NE IT_ZSBLCST1-BUKRS   OR
       IT_ZSBLCST1_ORG-ZFFIYR  NE IT_ZSBLCST1-ZFFIYR  OR
       IT_ZSBLCST1_ORG-ZFACDO  NE IT_ZSBLCST1-ZFACDO  OR
       IT_ZSBLCST1_ORG-ZFVEN   NE IT_ZSBLCST1-ZFVEN   OR
       IT_ZSBLCST1_ORG-ZFPAY   NE IT_ZSBLCST1-ZFPAY   OR
       IT_ZSBLCST1_ORG-ZFCSTYN NE IT_ZSBLCST1-ZFCSTYN OR
       IT_ZSBLCST1_ORG-ZFUPCST NE IT_ZSBLCST1-ZFUPCST OR
       IT_ZSBLCST1_ORG-ZFPCST  NE IT_ZSBLCST1-ZFPCST  OR
       IT_ZSBLCST1_ORG-ZTERM   NE IT_ZSBLCST1-ZTERM   OR
       IT_ZSBLCST1_ORG-ZFWERKS NE IT_ZSBLCST1-ZFWERKS OR
       IT_ZSBLCST1_ORG-MWSKZ   NE IT_ZSBLCST1-MWSKZ   OR
       IT_ZSBLCST1_ORG-KBETR   NE IT_ZSBLCST1-KBETR   OR
       IT_ZSBLCST1_ORG-KONWA   NE IT_ZSBLCST1-KONWA.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF W_MODIF_BIT EQ 'Y'.
    EXIT.
  ENDIF.
  LOOP AT IT_ZSBLCST1.
    READ TABLE IT_ZSBLCST1_ORG WITH KEY ZFCSQ = IT_ZSBLCST1-ZFCSQ.
    IF SY-SUBRC NE 0.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF W_MODIF_BIT EQ 'Y'.
    EXIT.
  ENDIF.

*> ÄÁÅ×ÀÌ³Ê ³»¿ë ºñ±³.
  LOOP AT IT_ZSBLCON_ORG.
    READ TABLE IT_ZSBLCON WITH KEY ZFCONSEQ = IT_ZSBLCON_ORG-ZFCONSEQ.
    IF SY-SUBRC NE 0.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
    IF IT_ZSBLCON_ORG-ZFCONNO  NE IT_ZSBLCON-ZFCONNO  OR
       IT_ZSBLCON_ORG-ZFTRCO   NE IT_ZSBLCON-ZFTRCO   OR
       IT_ZSBLCON_ORG-ZFCOTP   NE IT_ZSBLCON-ZFCOTP   OR
       IT_ZSBLCON_ORG-ZFCOSZ   NE IT_ZSBLCON-ZFCOSZ   OR
       IT_ZSBLCON_ORG-ZFWHCYT  NE IT_ZSBLCON-ZFWHCYT  OR
       IT_ZSBLCON_ORG-ZFCOTR   NE IT_ZSBLCON-ZFCOTR   OR
       IT_ZSBLCON_ORG-ZFPIDT   NE IT_ZSBLCON-ZFPIDT   OR
       IT_ZSBLCON_ORG-ZFULDT   NE IT_ZSBLCON-ZFULDT   OR
       IT_ZSBLCON_ORG-ZFPODT   NE IT_ZSBLCON-ZFPODT   OR
       IT_ZSBLCON_ORG-ZFRCVER  NE IT_ZSBLCON-ZFRCVER.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF W_MODIF_BIT EQ 'Y'.
    EXIT.
  ENDIF.
  LOOP AT IT_ZSBLCON.
    READ TABLE IT_ZSBLCON_ORG WITH KEY ZFCONSEQ = IT_ZSBLCON-ZFCONSEQ.
    IF SY-SUBRC NE 0.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
  ENDLOOP.

*> B/L ITEM Á¤º¸ ºñ±³< 2002.08.19 NHJ Ãß°¡ >.
  LOOP AT IT_ZSBLIT_ORG.
    READ TABLE IT_ZSBLIT WITH KEY ZFBLIT = IT_ZSBLIT_ORG-ZFBLIT.
    IF SY-SUBRC NE 0.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
    IF IT_ZSBLIT_ORG NE IT_ZSBLIT.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF W_MODIF_BIT EQ 'Y'.
    EXIT.
  ENDIF.
  LOOP AT IT_ZSBLIT.
    READ TABLE IT_ZSBLIT_ORG WITH KEY ZFBLIT = IT_ZSBLIT-ZFBLIT.
    IF SY-SUBRC NE 0.
      W_MODIF_BIT = 'Y'.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_BL_DATA_MODIFY_CHECK
*&---------------------------------------------------------------------*
*&      Form  p2000_show_bar
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BAR USING TEXT PERC.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PERC
            TEXT       = TEXT.

ENDFORM.                    " p2000_show_bar
*&---------------------------------------------------------------------*
*&      Form  P2000_CREATE_POST_MESSAGE_TREE
*&---------------------------------------------------------------------*
FORM P2000_CREATE_POST_MESSAGE_TREE.

  DATA: NODE_TABLE TYPE TREEV_NTAB,
        ITEM_TABLE TYPE ITEM_TABLE_TYPE,
        EVENT TYPE CNTL_SIMPLE_EVENT,
        EVENTS TYPE CNTL_SIMPLE_EVENTS,
        HIERARCHY_HEADER TYPE TREEV_HHDR.

  CREATE OBJECT G_APPLICATION.

* create a container for the tree control
  CREATE OBJECT G_CUSTOM_CONTAINER
    EXPORTING
      " the container is linked to the custom control with the
      " name 'TREE_CONTAINER' on the dynpro
      CONTAINER_NAME = 'TREE_CONTAINER'
    EXCEPTIONS
      CNTL_ERROR = 1
      CNTL_SYSTEM_ERROR = 2
      CREATE_ERROR = 3
      LIFETIME_ERROR = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.
* setup the hierarchy header
  HIERARCHY_HEADER-HEADING = 'Expense ype'.
  " heading
  HIERARCHY_HEADER-WIDTH = 11.         " width: 30 characters

* create a tree control

* After construction, the control contains one column in the
* hierarchy area. The name of this column
* is defined via the constructor parameter HIERACHY_COLUMN_NAME.
  CREATE OBJECT G_TREE
    EXPORTING
      PARENT              = G_CUSTOM_CONTAINER
      NODE_SELECTION_MODE = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION = 'X'
      HIERARCHY_COLUMN_NAME = 'Column1'
      HIERARCHY_HEADER = HIERARCHY_HEADER
    EXCEPTIONS
      CNTL_SYSTEM_ERROR           = 1
      CREATE_ERROR                = 2
      FAILED                      = 3
      ILLEGAL_NODE_SELECTION_MODE = 4
      ILLEGAL_COLUMN_NAME         = 5
      LIFETIME_ERROR              = 6.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* define the events which will be passed to the backend
  " node double click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'. " process PAI if event occurs
  APPEND EVENT TO EVENTS.

  " item double click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " expand no children
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_EXPAND_NO_CHILDREN.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " link click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_LINK_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " button click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_BUTTON_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " checkbox change
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_CHECKBOX_CHANGE.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " header click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_HEADER_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  CALL METHOD G_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS                    = EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* assign event handlers in the application class to each desired event
  SET HANDLER G_APPLICATION->HANDLE_NODE_DOUBLE_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_ITEM_DOUBLE_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_EXPAND_NO_CHILDREN FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_LINK_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_BUTTON_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_CHECKBOX_CHANGE FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_HEADER_CLICK FOR G_TREE.

* insert two additional columns

* Column2
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME                         = 'Column2'
      WIDTH                        = 30
      HEADER_TEXT                  = 'Payee'
    EXCEPTIONS
      COLUMN_EXISTS                = 1
      ILLEGAL_COLUMN_NAME          = 2
      TOO_MANY_COLUMNS             = 3
      ILLEGAL_ALIGNMENT            = 4
      DIFFERENT_COLUMN_TYPES       = 5
      CNTL_SYSTEM_ERROR            = 6
      FAILED                       = 7
      PREDECESSOR_COLUMN_NOT_FOUND = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.
* Column3
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME = 'Column3'
      WIDTH = 8
*      ALIGNMENT = CL_GUI_COLUMN_TREE=>ALIGN_RIGHT
      HEADER_TEXT = 'Tax Code'
    EXCEPTIONS
      COLUMN_EXISTS                 = 1
      ILLEGAL_COLUMN_NAME           = 2
      TOO_MANY_COLUMNS              = 3
      ILLEGAL_ALIGNMENT             = 4
      DIFFERENT_COLUMN_TYPES        = 5
      CNTL_SYSTEM_ERROR             = 6
      FAILED                        = 7
      PREDECESSOR_COLUMN_NOT_FOUND  = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* Column4
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME                         = 'Column4'
      WIDTH                        = 8
      HEADER_TEXT                  = 'Document Type'
    EXCEPTIONS
      COLUMN_EXISTS                = 1
      ILLEGAL_COLUMN_NAME          = 2
      TOO_MANY_COLUMNS             = 3
      ILLEGAL_ALIGNMENT            = 4
      DIFFERENT_COLUMN_TYPES       = 5
      CNTL_SYSTEM_ERROR            = 6
      FAILED                       = 7
      PREDECESSOR_COLUMN_NOT_FOUND = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* Column5
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME                         = 'Column5'
      WIDTH                        = 4
      HEADER_TEXT                  = 'Currency'
    EXCEPTIONS
      COLUMN_EXISTS                = 1
      ILLEGAL_COLUMN_NAME          = 2
      TOO_MANY_COLUMNS             = 3
      ILLEGAL_ALIGNMENT            = 4
      DIFFERENT_COLUMN_TYPES       = 5
      CNTL_SYSTEM_ERROR            = 6
      FAILED                       = 7
      PREDECESSOR_COLUMN_NOT_FOUND = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* Column6
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME                         = 'Column6'
      WIDTH                        = 8
      HEADER_TEXT                  = 'Payment Term'
    EXCEPTIONS
      COLUMN_EXISTS                = 1
      ILLEGAL_COLUMN_NAME          = 2
      TOO_MANY_COLUMNS             = 3
      ILLEGAL_ALIGNMENT            = 4
      DIFFERENT_COLUMN_TYPES       = 5
      CNTL_SYSTEM_ERROR            = 6
      FAILED                       = 7
      PREDECESSOR_COLUMN_NOT_FOUND = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* add some nodes to the tree control
* NOTE: the tree control does not store data at the backend. If an
* application wants to access tree data later, it must store the
* tree data itself.
  REFRESH : NODE_TABLE, ITEM_TABLE.
  PERFORM P2000_BUILD_MSG_NODE_TABLE USING NODE_TABLE ITEM_TABLE.

  CALL METHOD G_TREE->ADD_NODES_AND_ITEMS
    EXPORTING
      NODE_TABLE                     = NODE_TABLE
      ITEM_TABLE                     = ITEM_TABLE
      ITEM_TABLE_STRUCTURE_NAME      = 'MTREEITM'
    EXCEPTIONS
      FAILED                         = 1
      CNTL_SYSTEM_ERROR              = 3
      ERROR_IN_TABLES                = 4
      DP_ERROR                       = 5
      TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* expand the node with key 'Root'
  LOOP AT IT_ERR_GRP.
    G_NODE_KEY = IT_ERR_GRP-ZFSEQ.
    CALL METHOD G_TREE->EXPAND_NODE
      EXPORTING
        NODE_KEY            = G_NODE_KEY                    "#EC NOTEXT
      EXCEPTIONS
        FAILED              = 1
        ILLEGAL_LEVEL_COUNT = 2
        CNTL_SYSTEM_ERROR   = 3
        NODE_NOT_FOUND      = 4
        CANNOT_EXPAND_LEAF  = 5.
    IF SY-SUBRC <> 0.
      MESSAGE A000.
    ENDIF.
    CLEAR : G_NODE_KEY.
  ENDLOOP.

ENDFORM.                    " P2000_CREATE_POST_MESSAGE_TREE
*&---------------------------------------------------------------------*
*&      Form  P2000_BUILD_MSG_NODE_TABLE
*&---------------------------------------------------------------------*
FORM P2000_BUILD_MSG_NODE_TABLE
  USING
    NODE_TABLE TYPE TREEV_NTAB
    ITEM_TABLE TYPE ITEM_TABLE_TYPE.

  DATA: NODE TYPE TREEV_NODE,
        ITEM TYPE MTREEITM,
        SEQ  TYPE I.

  REFRESH : IT_ERR_GRP.
  SORT IT_ERR_LIST BY ZFSEQ.
  CLEAR : SEQ.
  LOOP AT IT_ERR_LIST.
    W_TABIX = SY-TABIX.
    ADD 1 TO SEQ.
    IT_ERR_LIST-ZFSUBSEQ = ( IT_ERR_LIST-ZFSEQ * 10000 ) + SEQ.
    MODIFY IT_ERR_LIST INDEX W_TABIX.

    READ TABLE IT_ERR_GRP WITH KEY ZFSEQ = IT_ERR_LIST-ZFSEQ.
    IF SY-SUBRC NE 0.
      IT_ERR_GRP-ZFSEQ = IT_ERR_LIST-ZFSEQ.
      APPEND IT_ERR_GRP.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ERR_GRP.
    READ TABLE IT_BLCSTHD WITH KEY ZFSEQ = IT_ERR_GRP-ZFSEQ.
    NODE-NODE_KEY = IT_BLCSTHD-ZFSEQ.                       "#EC NOTEXT
    " Key of the node
    CLEAR : NODE-RELATKEY.
*     NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
    CLEAR : NODE-RELATSHIP.

    NODE-HIDDEN = ' '.
    NODE-DISABLED = ' '.
    NODE-ISFOLDER = 'X'.
    CLEAR NODE-N_IMAGE.
    CLEAR NODE-EXP_IMAGE.
*    NODE-T_IMAGE = '@0B@'.
    NODE-EXPANDER = 'X'. " The node is marked with a '+', although
*                      " it has no children. When the user clicks on the
*                      " + to open the node, the event expand_nc is
*                      " fired. The programmerr can
*                      " add the children of the
*                      " node within the event handler of the expand_nc
*                      " event  (see callback handle_expand_nc).
    APPEND NODE TO NODE_TABLE.
  ENDLOOP.

  LOOP AT IT_ERR_LIST.
    NODE-NODE_KEY = IT_ERR_LIST-ZFSUBSEQ.
* Node is inserted as child of the node with key 'Root'.
    NODE-RELATKEY = IT_ERR_LIST-ZFSEQ.
    NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.

    NODE-HIDDEN = ' '.
    NODE-DISABLED = ' '.
    NODE-ISFOLDER = ' '.
    CLEAR NODE-N_IMAGE.
    CLEAR NODE-EXP_IMAGE.
    NODE-EXPANDER = ' '. " The node is marked with a '+', although
    APPEND NODE TO NODE_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = NODE-NODE_KEY.
    ITEM-ITEM_NAME = 'Column1'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    CASE IT_ERR_LIST-MSGTYP.
      WHEN 'E'.
        ITEM-TEXT = 'Error Message'.                        "#EC NOTEXT
      WHEN 'A'.
        ITEM-TEXT = 'Exit Message'.                         "#EC NOTEXT
      WHEN 'S'.
        ITEM-TEXT = 'Posting Message'.                      "#EC NOTEXT
      WHEN 'I'.
        ITEM-TEXT = 'Information Message'.                  "#EC NOTEXT
      WHEN 'W'.
        ITEM-TEXT = 'Warning Message'.                      "#EC NOTEXT
    ENDCASE.
    ITEM-T_IMAGE = IT_ERR_LIST-ICON.
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = NODE-NODE_KEY.
    ITEM-ITEM_NAME = 'Column2'.     "
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_ERR_LIST-MESSTXT.                        "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.

  ENDLOOP.

* The items of the nodes:

* Node with key 'Child1'
  LOOP AT IT_ERR_GRP.
    READ TABLE IT_BLCSTHD WITH KEY ZFSEQ = IT_ERR_GRP-ZFSEQ.
    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column1'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_BLCSTHD-TEXT.                            "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column2'.     "
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_BLCSTHD-NAME1.                           "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column3'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_BLCSTHD-BUPLA.                           "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column4'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_BLCSTHD-MWSKZ.                           "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column5'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_BLCSTHD-BLART.                           "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = IT_BLCSTHD-ZFSEQ.                      "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column6'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_BLCSTHD-WAERS.                           "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.

  ENDLOOP.

ENDFORM.                    " P2000_BUILD_MSG_NODE_TABLE
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_TREE_HANDLER
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_TREE_HANDLER.
  DATA : L_NODE_KEY   TYPE   I.

  CASE G_EVENT.
    WHEN 'ITEM_DOUBLE_CLICK' OR 'NODE_DOUBLE_CLICK'.
      IF G_NODE_KEY EQ 'Root' OR G_NODE_KEY IS INITIAL.
        MESSAGE S903(ZIM1).
        EXIT.
      ELSE.
        L_NODE_KEY = G_NODE_KEY.
        IF L_NODE_KEY LE 10000.
          MESSAGE S905(ZIM1).
          EXIT.
        ELSE.

*              G_ZFSEQ = G_NODE_KEY.
          READ TABLE IT_ERR_LIST WITH KEY ZFSUBSEQ = L_NODE_KEY.
          IF SY-SUBRC EQ 0.
            CALL FUNCTION 'MASS_MESSAGE_SHOW_LONGTEXT'
                 EXPORTING
                    SPRSL     = SY-LANGU
                    ARBGB     = IT_ERR_LIST-MSGID
                    MSGNR     = IT_ERR_LIST-MSGNR
                    MSGV1     = IT_ERR_LIST-MSGV1
                    MSGV2     = IT_ERR_LIST-MSGV2
                    MSGV3     = IT_ERR_LIST-MSGV3
                    MSGV4     = IT_ERR_LIST-MSGV4
*                    extravars = 'F'
                 EXCEPTIONS
                     NOT_FOUND = 1
                     OTHERS    = 2.
            IF SY-SUBRC NE 0.
            ENDIF.
          ENDIF.
          G_NODE_KEY_OLD = G_NODE_KEY.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_MESSAGE_TREE_EVENT_HANDL
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ITEM_GSBER
*&---------------------------------------------------------------------*
FORM P1000_GET_ITEM_GSBER TABLES   IT_GSBER STRUCTURE IT_GSBER
                          USING    P_MATNR
                                   P_WERKS.

  IF P_MATNR IS INITIAL.
    SELECT MAX( GSBER ) INTO IT_GSBER-GSBER
           FROM T134G
           WHERE WERKS EQ P_WERKS.
  ELSE.
    SELECT SINGLE * FROM MARA
           WHERE MATNR EQ P_MATNR.

    IF SY-SUBRC EQ 0 AND
       NOT MARA-SPART IS INITIAL.
      SELECT SINGLE GSBER INTO  IT_GSBER-GSBER
             FROM  T134G
             WHERE WERKS EQ P_WERKS
             AND   SPART EQ MARA-SPART.
    ELSE.
      SELECT MAX( GSBER ) INTO IT_GSBER-GSBER
             FROM T134G
             WHERE WERKS EQ P_WERKS.
    ENDIF.
  ENDIF.

  IF NOT IT_GSBER-GSBER IS INITIAL.
    COLLECT IT_GSBER.
  ENDIF.

ENDFORM.                    " P1000_GET_ITEM_GSBER
*&---------------------------------------------------------------------*
*&      Form  P2000_CALC_COST_AMOUNT
*&---------------------------------------------------------------------*
FORM P2000_CALC_COST_AMOUNT.

*>°ü¼¼/ºÎ°¡¼¼ °è»ê ·ÎÁ÷ Ãß°¡...
  IF ZTIMIMG00-ZFPSMS EQ '1'.
    PERFORM CALC_ZTCUCLCST_TO_IT.
  ELSE.
*    PERFORM CALC_ZTCUCLCST_TO_IT.
    PERFORM  P2000_CALC_ZTIDS_COST.
  ENDIF.

ENDFORM.                    " P2000_CALC_COST_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  P2000_PYMT_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_PYMT_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING 'Áö±Þ Ã³¸® È®ÀÎ'
                     'Áö±Þ ¹ÝÁ¦ Ã³¸®µË´Ï´Ù..'
                     '¹Ý¿µÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                     'Y'             " Ãë¼Ò ¹öÆ° À¯.
                     '1'.            " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Outgoing payment confirm'
             'Now do payment clearing.'
             'Do you want to continue?'
             'Y'
             '1'.
  ENDCASE.
ENDFORM.                    " P2000_PYMT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_PYCN_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_PYCN_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING 'Áö±Þ ¹ÝÁ¦ Ãë¼Ò È®ÀÎ'
                     'ÇØ´ç Áö±Þ ¹ÝÁ¦ÀüÇ¥¸¦ Ãë¼ÒÇÕ´Ï´Ù.'
                     '°è¼Ó ÁøÇà ÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                     'N'             " Ãë¼Ò ¹öÆ° À¯.
                     '1'.            " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Payment clearing cancel confirm'
             'Now cancel the clearing document.'
             'Do you want to continue?'
             'N'
             '1'.

  ENDCASE.
ENDFORM.                    " P2000_PYCN_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_WIGHT_INPUT_CHECK
*&---------------------------------------------------------------------*
FORM P3000_WIGHT_INPUT_CHECK.
*> GROSS WEIGHT CHECK. (ÇÊ¼ö)
  IF ZTBL-ZFNEWT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFNEWT'.
  ENDIF.
  IF ZTBL-ZFNEWTM IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFNEWTM'.
  ENDIF.

*> ¿ëÀû CHECK. (ÇÊ¼ö)
*  IF ZTBL-ZFTOVL IS INITIAL.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFTOVL'.
*  ENDIF.
  IF ZTBL-ZFTOVLM IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFTOVLM'.
  ENDIF.
*ENDIF.

ENDFORM.                    " P3000_WIGHT_INPUT_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_HS_DATA_CHECK
*&---------------------------------------------------------------------*
FORM P2000_HS_DATA_CHECK.

  " HS CODE CHECK
  IF ZTIDRHS-STAWN IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDRHS' 'STAWN'.
  ENDIF.

  " Ç°¸í.
  IF ZTIDRHS-ZFGDNM  IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDRHS' 'ZFGDNM'.
  ENDIF.

  " °Å·¡Ç°¸í.
  IF ZTIDRHS-ZFTGDNM  IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDRHS' 'ZFTGDNM'.
  ENDIF.

  " ¿ø»êÁö.
  IF ZTIDRHS-ZFORIG   IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDRHS' 'ZFORIG'.
  ENDIF.

  " ¿ø»êÁö Ç¥½Ã À¯¹«.
  IF ZTIDRHS-ZFORYN   IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDRHS' 'ZFORYN'.
  ENDIF.

*  " °ü¼¼ °è»ê ¹æ¹ý.
*  IF ZTIDRHS-ZFTXMT   IS INITIAL.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDRHS' 'ZFTXMT'.
*  ENDIF.

*  " °¨¸é±¸ºÐ.
*  IF ZTIDRHS-ZFCDPCD  IS INITIAL.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDRHS' 'ZFCDPCD'.
*  ENDIF.

  " HS CODE Áßº¹ CHECK.
  CLEAR : W_CNT.
  LOOP  AT  IT_ZSIDRHS WHERE STAWN  EQ  ZTIDRHS-STAWN.
    W_CNT  =  W_CNT  + 1.
  ENDLOOP.

  " ¶õ¹øÈ£ CHECK.
  IF ZTIDRHS-ZFCONO IS INITIAL.

    SELECT MAX( ZFCONO )  INTO  W_CONO
    FROM   ZTIDRHS
    WHERE  ZFBLNO         EQ    ZTIDRHS-ZFBLNO
    AND    ZFCLSEQ        EQ    ZTIDRHS-ZFCLSEQ.

    LOOP  AT  IT_ZSIDRHS.
      IF  W_CONO  LE  IT_ZSIDRHS-ZFCONO.
        MOVE  IT_ZSIDRHS-ZFCONO   TO  W_CONO.
      ENDIF.
    ENDLOOP.

    ZTIDRHS-ZFCONO  =  W_CONO  +  10.

    IF W_CNT GE 1.
      MESSAGE  E235(ZIM1) WITH ZTIDRHS-STAWN.
    ENDIF.
  ELSE.
    IF W_CNT GT 1.
      MESSAGE  E235(ZIM1) WITH ZTIDRHS-STAWN.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_HS_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_CHECK.

   CLEAR : W_DOWN_PAY.
   LOOP AT IT_ZSCIVIT.

      IF IT_ZSCIVIT-EBELN IS INITIAL.  EXIT.  ENDIF.

      SELECT * FROM EKBE
      WHERE  EBELN  EQ   IT_ZSCIVIT-EBELN
      AND    EBELP  EQ   IT_ZSCIVIT-EBELP
      AND    VGABE  EQ   '4'
      AND    BEWTP  EQ   'A'.
         IF EKBE-SHKZG EQ 'S'.
            W_DOWN_PAY  =  W_DOWN_PAY + EKBE-DMBTR.
         ELSE.
            W_DOWN_PAY  =  W_DOWN_PAY - EKBE-DMBTR.
         ENDIF.
      ENDSELECT.

   ENDLOOP.

ENDFORM.                    " P2000_MESSAGE_CHECK
