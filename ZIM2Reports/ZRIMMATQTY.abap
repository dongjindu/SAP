*&---------------------------------------------------------------------*
*& Report  ZRIMMATQTY                                                  *
*&---------------------------------------------------------------------*
*&  Program Name : Progress by material quantity                       *
*&  Created by   : Na Hyun Ju INFOLINK Ltd.                            *
*&  Created on   : 2001.04.04                                          *
*&---------------------------------------------------------------------*
*&   DESC. :
*&---------------------------------------------------------------------*
*& [Change Log]
*&---------------------------------------------------------------------*
REPORT  ZRIMMATAQTY  MESSAGE-ID ZIM
                     LINE-SIZE 212
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE : <ICON>,
          <SYMBOL>,
          ZRIMMATQTYTOP.

*-----------------------------------------------------------------------
* Search Condition Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_EBELN   FOR EKKO-EBELN,       " P/O No.
               S_MATNR   FOR EKPO-MATNR,       " Material No.
               S_OPNDT   FOR ZTREQST-ZFOPNDT,  " Open Date.
               S_OPBN    FOR ZTREQHD-ZFOPBN,   " Open Bank.
               S_REQNO   FOR ZTREQHD-ZFREQNO,  " Import Request No.
               S_LIFNR   FOR ZTREQHD-LIFNR,    " Vendor.
               S_MATGB   FOR ZTREQHD-ZFMATGB,  " Material Type
               S_REQTY   FOR ZTREQHD-ZFREQTY,  " Import Payment Type
               S_EKORG   FOR ZTREQST-EKORG,    " Purch. Org.
               S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
               S_HBLNO   FOR ZTBL-ZFHBLNO,     " House B/L No
               S_CONT    FOR LIKP-TRAID,       " Container No
               S_WERKS   FOR EKPO-WERKS.       " Plant
PARAMETERS :   P_NAME    LIKE USR02-BNAME.     " Charge in

SELECT-OPTIONS:
               S_DOCST   FOR ZTREQST-ZFDOCST.  " Document Status

SELECTION-SCREEN END OF BLOCK B2.

* Title Text Write
TOP-OF-PAGE.
  CLEAR  WRITE_CHK.
  IF INCLUDE NE 'POPU'.
    PERFORM   P3000_TITLE_WRITE.
  ENDIF.
*-----------------------------------------------------------------------
* INITIALIZATION
*-----------------------------------------------------------------------
INITIALIZATION.
  SET TITLEBAR 'TIT1'.

*-----------------------------------------------------------------------
* START OF SELECT.
*-----------------------------------------------------------------------
START-OF-SELECTION.
  SELECT SINGLE * FROM ZTIMIMG00.
* Import Request No.
  PERFORM P1000_READ_RN_DATA USING W_ERR_CHK.
  CHECK W_ERR_CHK NE 'Y'.

* P/O Table Select..
  PERFORM P1000_READ_PO_TABLE.

* B/L Table Select..
  PERFORM P1000_READ_BL_DATA.

* Commercial Invoice Quantity.
  PERFORM P1000_READ_CIV_DATA.

* Unloading Data Selection
  IF ZTIMIMG00-ZFCGYN EQ 'X'.
    PERFORM P1000_READ_CG_DATA.
  ENDIF.

* Customs Clearance Request Quantity Selection.
  PERFORM P1000_READ_IV_DATA.

* Customs Clearance Table Select..
  PERFORM P1000_READ_ZTIDS_DATA.

*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
END-OF-SELECTION.

  CHECK W_ERR_CHK NE 'Y'.

* Title Text Write.
  SET TITLEBAR 'TIT1'.
  SET PF-STATUS 'ZIM95'.

* DATA WRITE
  PERFORM P2000_SORT_DATA.
  PERFORM P2000_WRITE_IT.
  PERFORM P2000_WRITE_DATA.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
*------- Abbrechen (CNCL) ----------------------------------------------
    WHEN 'CNCL'.
      SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
    WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
    WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
    WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
    WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
    WHEN 'WESU'.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
* LINE 선택시 진행현황 DISPLAY
*-----------------------------------------------------------------------
AT LINE-SELECTION.
*>> 자재별 진행관리 DISPLAY 화면 OPEN.
  IF INCLUDE NE 'POPU'.
    MOVE IT_TAB-EBELN  TO  W_EBELN.
    MOVE IT_TAB-EBELP  TO  W_EBELP.
    INCLUDE = 'POPU'.
    CALL SCREEN 0100 STARTING AT  10   3
                     ENDING   AT  95   20.
    CLEAR : INCLUDE.
*>> 각 문서의 상세화면 DISPLAY.
  ELSE.
    DATA : L_TEXT(30).

    GET CURSOR FIELD L_TEXT.
    CASE  L_TEXT.
      WHEN 'IT_PO-EBELN'   OR  'IT_PO-EBELP'.
        SET PARAMETER ID 'BES'  FIELD IT_PO-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      WHEN 'IT_RN-ZFREQNO' OR  'IT_RN-ZFITMNO'.
        SET PARAMETER ID 'ZPREQNO'  FIELD  IT_RN-ZFREQNO.
        CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
      WHEN 'IT_BL-ZFBLNO'  OR  'IT_BL-ZFBLIT'.
        SET PARAMETER ID 'ZPBLNO'   FIELD  IT_BL-ZFBLNO.
        SET PARAMETER ID 'ZPHBLNO'  FIELD  ' '.
        CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
      WHEN 'IT_CG-ZFCGNO'  OR  'IT_CG-ZFCGIT'.
        SET PARAMETER ID 'ZPBLNO'   FIELD  IT_CG-ZFBLNO.
        SET PARAMETER ID 'ZPCGPT'   FIELD  IT_CG-ZFCGPT.
        CALL TRANSACTION 'ZIM83' AND SKIP FIRST SCREEN.
      WHEN 'IT_IV-ZFIVNO'  OR  'IT_IV-ZFIVDNO'.
        SET PARAMETER ID 'ZPIVNO'   FIELD  IT_IV-ZFIVNO.
        CALL TRANSACTION 'ZIM33' AND SKIP FIRST SCREEN.
      WHEN 'IT_IDS-ZFENTNO'.
        SET PARAMETER ID 'ZPBLNO'   FIELD  IT_IDS-ZFBLNO.
        SET PARAMETER ID 'ZPCLSEQ'  FIELD  IT_IDS-ZFCLSEQ.
        CALL TRANSACTION 'ZIMCC3' AND SKIP FIRST SCREEN.
      WHEN 'IT_IV-MJAHR'    OR   'IT_IV-MBLNR'.
        SET  PARAMETER ID  'MBN'   FIELD   IT_IV-MBLNR.
        SET  PARAMETER ID  'MJA'   FIELD   IT_IV-MJAHR.
        CALL TRANSACTION 'MB03' AND SKIP  FIRST SCREEN.
    ENDCASE.

  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_PO
  FROM   EKPO
  FOR    ALL  ENTRIES IN IT_RN
  WHERE  EBELN  EQ  IT_RN-EBELN
  AND    EBELP  EQ  IT_RN-EBELP.

ENDFORM.                    " P1000_READ_PO_DATA
*&------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&------------------------------------------------------------------*
FORM P1000_READ_RN_DATA USING W_ERR_CHK.

  DATA: L_LINE_COUNT TYPE I.

  W_ERR_CHK = 'N'.

  SELECT P~WERKS   H~ZFREQNO  H~ZFREQTY I~EBELN    I~EBELP
         H~LIFNR   H~LLIEF    H~ZFBENI  H~ZFLASTAM H~WAERS
         H~ZFUSDAM H~ZFUSD    I~ZFITMNO I~MATNR    I~STAWN
         I~MENGE   I~MEINS
  INTO CORRESPONDING FIELDS OF TABLE IT_RN
  FROM      ( ( ZTREQHD AS H INNER JOIN ZTREQIT AS I
  ON            H~ZFREQNO    EQ   I~ZFREQNO         )
  INNER JOIN EKPO     AS  P
  ON            I~EBELN      EQ   P~EBELN
  AND           I~EBELP      EQ   P~EBELP           )
  INNER JOIN EKKO     AS  K
  ON            P~EBELN      EQ   K~EBELN
  WHERE    H~ZFREQNO  IN  S_REQNO
  AND      H~EBELN    IN  S_EBELN
  AND      I~MATNR    IN  S_MATNR
  AND      H~LIFNR    IN  S_LIFNR
  AND      H~ZFMATGB  IN  S_MATGB
  AND      H~ZFREQTY  IN  S_REQTY
  AND      K~EKORG    IN  S_EKORG
  AND      K~EKGRP    IN  S_EKGRP
  AND      H~ZFOPBN   IN  S_OPBN
  AND      K~BSTYP    EQ  'F'
  AND      P~WERKS    IN  S_WERKS.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S738.
    EXIT.
  ENDIF.

  CONCATENATE P_NAME '%' INTO P_NAME.

  LOOP AT IT_RN.
    W_TABIX = SY-TABIX.
    SELECT SINGLE EKORG   EKGRP   ZFOPNNO
    INTO    (IT_RN-EKORG,   IT_RN-EKGRP,   IT_RN-ZFOPNNO)
    FROM   ZTREQST
    WHERE  ZFREQNO EQ   IT_RN-ZFREQNO
    AND    EKORG   IN   S_EKORG
    AND    EKGRP   IN   S_EKGRP
    AND    ZFDOCST IN   S_DOCST
    AND    ZFOPNDT IN   S_OPNDT
    AND    ERNAM   LIKE P_NAME
    AND    ZFAMDNO EQ ( SELECT MAX( ZFAMDNO ) FROM ZTREQST
                        WHERE  ZFREQNO EQ IT_RN-ZFREQNO ).
    IF SY-SUBRC = 0.
      MODIFY IT_RN INDEX W_TABIX.
    ELSE.
      DELETE IT_RN INDEX W_TABIX.
    ENDIF.

  ENDLOOP.

  DESCRIBE TABLE IT_RN LINES L_LINE_COUNT.
  IF L_LINE_COUNT = 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S738.
    EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IV_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_IV_DATA.

  SELECT  *
  INTO    CORRESPONDING FIELDS OF TABLE IT_IV
  FROM    ZTIV AS H INNER  JOIN ZTIVIT AS I
  ON      H~ZFIVNO     EQ  I~ZFIVNO
  FOR ALL ENTRIES      IN  IT_RN
  WHERE   I~ZFREQNO    EQ  IT_RN-ZFREQNO
  AND     I~ZFITMNO    EQ  IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_IV_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_TABLE
*&---------------------------------------------------------------------*
FORM P1000_READ_PO_TABLE.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_PO
  FROM   EKPO
  FOR    ALL  ENTRIES IN IT_RN
  WHERE  EBELN  EQ  IT_RN-EBELN
  AND    EBELP  EQ  IT_RN-EBELP.

  LOOP AT IT_RN.
     W_TABIX  =  SY-TABIX.

     IF NOT S_HBLNO[] IS INITIAL.

        CLEAR : W_BL_CNT.
        SELECT COUNT( * )   INTO  W_BL_CNT
        FROM   ZTBL  AS   A INNER JOIN ZTBLIT AS B
        ON     A~ZFBLNO     EQ    B~ZFBLNO
        WHERE  A~ZFHBLNO    IN    S_HBLNO
        AND    B~EBELN      EQ    IT_RN-EBELN
        AND    B~EBELP      EQ    IT_RN-EBELP.

        IF W_BL_CNT LE 0.
           DELETE  IT_RN  INDEX  W_TABIX.
           DELETE  IT_PO  WHERE  EBELN  =  IT_RN-EBELN
                          AND    EBELP  =  IT_RN-EBELP.
           CONTINUE.
        ENDIF.
     ENDIF.

     ">> Container No
     IF NOT S_CONT[] IS INITIAL.

        CLEAR : W_TR_CNT.
        SELECT COUNT( * )   INTO  W_TR_CNT
        FROM   LIKP  AS  A  INNER JOIN  LIPS AS B
        ON     A~VBELN      EQ    B~VBELN
        WHERE  A~TRAID      IN    S_CONT
        AND    B~VGBEL      EQ    IT_RN-EBELN
        AND    B~VGPOS      EQ    IT_RN-EBELP.

        IF W_TR_CNT LE 0.
           DELETE  IT_RN  INDEX  W_TABIX.
           DELETE  IT_PO  WHERE  EBELN = IT_RN-EBELN
                          AND    EBELP = IT_RN-EBELP.
           CONTINUE.
        ENDIF.
     ENDIF.

  ENDLOOP.

ENDFORM.                    " P1000_READ_PO_TABLE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_BL
   FROM  ZTBL AS H INNER  JOIN ZTBLIT AS I
   ON    H~ZFBLNO     EQ  I~ZFBLNO
   FOR   ALL ENTRIES  IN  IT_RN
   WHERE I~ZFREQNO    EQ  IT_RN-ZFREQNO
   AND   I~ZFITMNO    EQ  IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDR_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIDR_DATA.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_IDR
  FROM   ZTIDRUSD  AS  H  INNER JOIN ZTIDRUS AS I
  ON     H~ZFBLNO      EQ  I~ZFBLNO
  AND    H~ZFCLSEQ     EQ  I~ZFCLSEQ
  FOR    ALL  ENTRIES  IN  IT_IV
  WHERE  H~ZFIVNO      EQ  IT_IV-ZFIVNO
  AND    H~ZFIVDNO     EQ  IT_IV-ZFIVDNO .

  LOOP  AT  IT_IDR.
    W_TABIX  =  SY-TABIX.
    READ TABLE IT_IV WITH KEY ZFIVNO  = IT_IDR-ZFIVNO
                              ZFIVDNO = IT_IDR-ZFIVDNO.
    IF SY-SUBRC EQ 0.
      MOVE  IT_IV-EBELN   TO  IT_IDR-EBELN.
      MOVE  IT_IV-EBELP   TO  IT_IDR-EBELP.
      MOVE  IT_IV-ZFREQNO TO  IT_IDR-ZFREQNO.
      MOVE  IT_IV-ZFITMNO TO  IT_IDR-ZFITMNO.
      MOVE  IT_IV-ZFBLIT  TO  IT_IDR-ZFBLIT.
      MOVE  IT_IV-ZFCGNO  TO  IT_IDR-ZFCGNO.
      MOVE  IT_IV-ZFCGIT  TO  IT_IDR-ZFCGIT.
      MODIFY  IT_IDR  INDEX  W_TABIX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P1000_READ_ZFIDR_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDS_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIDS_DATA.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_IDR
  FROM   ZTIDSUSD  AS  H  INNER JOIN ZTIDSUS AS I
  ON     H~ZFIVNO      EQ  I~ZFIVNO
  AND    H~ZFCLSEQ     EQ  I~ZFCLSEQ
  FOR    ALL  ENTRIES  IN  IT_IV
  WHERE  H~ZFIVNO      EQ  IT_IV-ZFIVNO
  AND    H~ZFIVDNO     EQ  IT_IV-ZFIVDNO .

  LOOP  AT  IT_IDS.
    W_TABIX  =  SY-TABIX.
    READ TABLE IT_IV WITH KEY ZFIVNO  = IT_IDS-ZFIVNO
                              ZFIVDNO = IT_IDS-ZFIVDNO.
    IF SY-SUBRC EQ 0.
      MOVE  IT_IV-EBELN   TO  IT_IDS-EBELN.
      MOVE  IT_IV-EBELP   TO  IT_IDS-EBELP.
      MOVE  IT_IV-ZFREQNO TO  IT_IDS-ZFREQNO.
      MOVE  IT_IV-ZFITMNO TO  IT_IDS-ZFITMNO.
      MOVE  IT_IV-ZFBLIT  TO  IT_IDS-ZFBLIT.
      MOVE  IT_IV-ZFCGNO  TO  IT_IDS-ZFCGNO.
      MOVE  IT_IV-ZFCGIT  TO  IT_IDS-ZFCGIT.
      MOVE  IT_IV-WERKS   TO  IT_IDS-WERKS.
      MOVE  IT_IV-LIFNR   TO  IT_IDS-LIFNR.
      MODIFY  IT_IDS  INDEX  W_TABIX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P1000_READ_ZFIDS_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*       COMMERCIAL I/V DATA READ.
*----------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_CIV
  FROM   ZTCIVHD AS H INNER JOIN ZTCIVIT AS I
  ON     H~ZFCIVRN    EQ  I~ZFCIVRN
  FOR    ALL ENTRIES  IN  IT_RN
  WHERE  I~ZFREQNO    EQ  IT_RN-ZFREQNO
  AND    I~ZFITMNO    EQ  IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_CG_DATA.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_CG
  FROM   ZTCGHD AS H INNER JOIN ZTCGIT AS I
  ON     H~ZFCGNO     EQ  I~ZFCGNO
  FOR    ALL ENTRIES  IN  IT_RN
  WHERE  I~ZFREQNO    EQ  IT_RN-ZFREQNO
  AND    I~ZFITMNO    EQ  IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_DATA
*&---------------------------------------------------------------------*
FORM P2000_WRITE_DATA.
  CLEAR : SUM_PO_MENGE,  SUM_RE_MENGE,  SUM_BL_MENGE,
          SUM_IV_MENGE,  SUM_CG_MENGE,  SUM_IDR_MENGE,
          SUM_IDS_MENGE, SUM_CIV_MENGE, SUM_IN_MENGE,
          SUM_BLCG_MENGE,
          TOT_PO_MENGE,  TOT_RE_MENGE,  TOT_BL_MENGE,
          TOT_IV_MENGE,  TOT_CG_MENGE,  TOT_IDR_MENGE,
          TOT_IDS_MENGE, TOT_CIV_MENGE, TOT_IN_MENGE.

  LOOP  AT  IT_TAB.

    W_COUNT = W_COUNT + 1.
    W_MOD   = W_COUNT MOD 2.

    IF SY-TABIX = 1.
      WRITE_CHK = 'X'.
      MOVE  IT_TAB-MATNR  TO  SV_MATNR.
      MOVE  IT_TAB-MEINS  TO  SV_MEINS.
      MOVE  IT_TAB-MATNM  TO  SV_MATNM.
      PERFORM  P3000_TITLE_WRITE.
    ENDIF.
    IF IT_TAB-MATNR  NE  SV_MATNR.
      WRITE_CHK = 'X'.
      PERFORM  P3000_SUM_WRITE.

      MOVE  IT_TAB-MATNR  TO  SV_MATNR.
      MOVE  IT_TAB-MEINS  TO  SV_MEINS.
      MOVE  IT_TAB-MATNM  TO  SV_MATNM.

      PERFORM  P3000_TITLE_WRITE.
      CLEAR : SUM_PO_MENGE,  SUM_RE_MENGE,  SUM_BL_MENGE,
              SUM_IV_MENGE,  SUM_CG_MENGE,  SUM_IDR_MENGE,
              SUM_IDS_MENGE, SUM_CIV_MENGE, SUM_IN_MENGE,
              SUM_NO_MENGE,  SUM_NO_MENGE,  SUM_BLCG_MENGE.
    ENDIF.
* Amount SUM.
    PERFORM  P3000_LINE_WRITE.
    ADD : IT_TAB-MENGE      TO  SUM_PO_MENGE,
          IT_TAB-RE_MENGE   TO  SUM_RE_MENGE,
          IT_TAB-BL_MENGE   TO  SUM_BL_MENGE,
          IT_TAB-CG_MENGE   TO  SUM_CG_MENGE,
          IT_TAB-IV_MENGE   TO  SUM_IV_MENGE,
          IT_TAB-CIV_MENGE  TO  SUM_CIV_MENGE,
          IT_TAB-IDR_MENGE  TO  SUM_IDR_MENGE,
          IT_TAB-IDS_MENGE  TO  SUM_IDS_MENGE,
          IT_TAB-NO_MENGE   TO  SUM_NO_MENGE,
          IT_TAB-BLCG_MENGE TO  SUM_BLCG_MENGE,
          IT_TAB-IN_MENGE   TO  SUM_IN_MENGE.
    ADD : IT_TAB-MENGE      TO  TOT_PO_MENGE,
          IT_TAB-RE_MENGE   TO  TOT_RE_MENGE,
          IT_TAB-BL_MENGE   TO  TOT_BL_MENGE,
          IT_TAB-CG_MENGE   TO  TOT_CG_MENGE,
          IT_TAB-IV_MENGE   TO  TOT_IV_MENGE,
          IT_TAB-CIV_MENGE  TO  TOT_CIV_MENGE,
          IT_TAB-IDR_MENGE  TO  TOT_IDR_MENGE,
          IT_TAB-IDS_MENGE  TO  TOT_IDS_MENGE,
          IT_TAB-BLCG_MENGE TO  TOT_BLCG_MENGE,
          IT_TAB-NO_MENGE   TO  TOT_NO_MENGE,
          IT_TAB-IN_MENGE   TO  TOT_IN_MENGE.
    AT LAST.
      PERFORM P3000_SUM_WRITE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " P2000_WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_RE_QTY
*&---------------------------------------------------------------------*
FORM P4000_COMPUTE_RE_QTY.

  LOOP  AT  IT_RN  WHERE  EBELN  =  IT_PO-EBELN
                   AND    EBELP  =  IT_PO-EBELP.
    ADD  IT_RN-MENGE   TO  SUM_RE_MENGE.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_RE_QTY
*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_BL_QTY
*&---------------------------------------------------------------------*
FORM P4000_COMPUTE_BL_QTY.

  CLEAR  IN_CHK_YN.
  LOOP  AT  IT_BL  WHERE  EBELN  =  IT_PO-EBELN
                   AND    EBELP  =  IT_PO-EBELP.
    ADD  IT_BL-BLMENGE   TO  SUM_BL_MENGE.
    IF IT_BL-ZFELIKZ NE 'X'.
      MOVE  'N'  TO  IN_CHK_YN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_BL_QTY
*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_CG_QTY
*&---------------------------------------------------------------------*
FORM P4000_COMPUTE_CG_QTY.

  LOOP  AT  IT_CG  WHERE  EBELN  =  IT_PO-EBELN
                   AND    EBELP  =  IT_PO-EBELP.
    ADD  IT_CG-CGMENGE   TO  SUM_CG_MENGE.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_CG_QTY
*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_IV_QTY
*&---------------------------------------------------------------------*
FORM P4000_COMPUTE_IV_QTY.

  LOOP  AT  IT_IV  WHERE  EBELN  =  IT_PO-EBELN
                   AND    EBELP  =  IT_PO-EBELP.

    ADD  IT_IV-CCMENGE   TO  SUM_IV_MENGE.
    IF IT_IV-ZFGRST = 'Y'.
      ADD  IT_IV-GRMENGE  TO  SUM_IN_MENGE.
    ENDIF.
    IF IT_IV-ZFCUST = 'Y'.
      ADD  IT_IV-CCMENGE  TO  SUM_IDS_MENGE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_IV_QTY
*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_IDR_QTY
*&---------------------------------------------------------------------*
FORM P4000_COMPUTE_IDR_QTY.

  LOOP  AT  IT_IDR   WHERE  EBELN  =  IT_PO-EBELN
                     AND    EBELP  =  IT_PO-EBELP.
    ADD  IT_IDR-ZFQNT  TO  SUM_IDR_MENGE.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_IDR_QTY
*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_IDS_QTY
*&---------------------------------------------------------------------*
FORM P4000_COMPUTE_IDS_QTY.
  LOOP  AT  IT_IDS   WHERE  EBELN  =  IT_PO-EBELN
                     AND    EBELP  =  IT_PO-EBELP.
    ADD  IT_IDS-ZFQNT  TO  SUM_IDS_MENGE.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_IDS_QTY
*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_DATA
*&---------------------------------------------------------------------*
*       INTERNAL TABLE SORT.
*----------------------------------------------------------------------*
FORM P2000_SORT_DATA.

  SORT  IT_PO  BY  MATNR  EBELN  EBELP.
  SORT  IT_RN  BY  MATNR  EBELN  EBELP.
  SORT  IT_BL  BY  EBELN  EBELP.
  SORT  IT_CG  BY  EBELN  EBELP.
  SORT  IT_IV  BY  EBELN  EBELP.
  SORT  IT_IDS BY  EBELN  EBELP.

ENDFORM.                    " P2000_SORT_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_IT
*&---------------------------------------------------------------------*
FORM P2000_WRITE_IT.

  CLEAR  : SUM_RE_MENGE,  SUM_BL_MENGE,  SUM_CG_MENGE,  SUM_IV_MENGE,
           SUM_IDR_MENGE, SUM_IDS_MENGE, SUM_CIV_MENGE, SUM_IN_MENGE,
           SUM_BLCG_MENGE.

  LOOP  AT  IT_PO.
    CLEAR : SUM_RE_MENGE,  SUM_BL_MENGE,  SUM_CG_MENGE,  SUM_IV_MENGE,
            SUM_IDR_MENGE, SUM_IDS_MENGE, SUM_CIV_MENGE, SUM_IN_MENGE,
            SUM_NO_MENGE,  SUM_BLCG_MENGE.
    PERFORM  P4000_COMPUTE_RE_QTY.
    PERFORM  P4000_COMPUTE_BL_QTY.
    PERFORM  P4000_COMPUTE_CG_QTY.
    PERFORM  P4000_COMPUTE_IV_QTY.
    PERFORM  P4000_COMPUTE_IDS_QTY.
    PERFORM  P4000_COMPUTE_CIV_QTY.
    SUM_NO_MENGE   =  SUM_BL_MENGE  -  SUM_IDS_MENGE.
    SUM_BLCG_MENGE =  SUM_BL_MENGE  -  SUM_CG_MENGE.

    MOVE  :  IT_PO-MATNR    TO  IT_TAB-MATNR,
             IT_PO-TXZ01    TO  IT_TAB-MATNM,
             IT_PO-EBELN    TO  IT_TAB-EBELN,
             IT_PO-EBELP    TO  IT_TAB-EBELP,
             IT_PO-MENGE    TO  IT_TAB-MENGE,
             IT_PO-MEINS    TO  IT_TAB-MEINS,
             SUM_RE_MENGE   TO  IT_TAB-RE_MENGE,
             SUM_BL_MENGE   TO  IT_TAB-BL_MENGE,
             SUM_CG_MENGE   TO  IT_TAB-CG_MENGE,
             SUM_IV_MENGE   TO  IT_TAB-IV_MENGE,
             SUM_CIV_MENGE  TO  IT_TAB-CIV_MENGE,
             SUM_IN_MENGE   TO  IT_TAB-IN_MENGE,
             SUM_IDR_MENGE  TO  IT_TAB-IDR_MENGE,
             SUM_IDS_MENGE  TO  IT_TAB-IDS_MENGE,
             SUM_NO_MENGE   TO  IT_TAB-NO_MENGE,
             SUM_BLCG_MENGE TO  IT_TAB-BLCG_MENGE.

*>> G/R Quantity CHECK
    IF SUM_IN_MENGE  GT 0.
      IF SUM_CG_MENGE GT 0 AND
         SUM_CG_MENGE EQ SUM_IN_MENGE.
        MOVE  'Complete G/R'  TO  IT_TAB-IN_CHK.
      ELSEIF SUM_BL_MENGE GT 0 AND
         SUM_BL_MENGE EQ SUM_IN_MENGE.
        MOVE  'Complete G/R'  TO  IT_TAB-IN_CHK.
      ELSEIF SUM_CG_MENGE EQ 0 AND SUM_BL_MENGE EQ 0 AND
             SUM_RE_MENGE EQ SUM_IN_MENGE.
        MOVE  'Complete G/R'  TO  IT_TAB-IN_CHK.
      ELSE.
        MOVE 'Not yet G/R'     TO  IT_TAB-IN_CHK.
      ENDIF.
    ELSE.
      MOVE  'Not yet G/R'    TO  IT_TAB-IN_CHK.
    ENDIF.
    APPEND  IT_TAB.
  ENDLOOP.
  SORT  IT_TAB  BY  MATNR  EBELN  EBELP.

ENDFORM.                    " P2000_WRITE_IT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFCGYN EQ 'X'.
  ELSE.
  ENDIF.
  IF WRITE_CHK NE 'X'.
    SKIP 2.
    FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
    WRITE : /70  '[ Progression by material quantity ]'
                 COLOR COL_HEADING INTENSIFIED OFF.
    WRITE : / 'Date : ', SY-DATUM.
    IF ZTIMIMG00-ZFCGYN EQ 'X'.
      WRITE : / SY-ULINE.
    ELSE.
      WRITE : / SY-ULINE(175).
    ENDIF.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE : / SY-VLINE NO-GAP,
              'Purchase Order  '     NO-GAP, SY-VLINE NO-GAP,
              'Purchase Ord. Qty'    NO-GAP, SY-VLINE NO-GAP,
              ' import req. Qty.'    NO-GAP, SY-VLINE NO-GAP,
              '         B/L Qty.'    NO-GAP, SY-VLINE NO-GAP.
    IF ZTIMIMG00-ZFCGYN EQ 'X'.
      WRITE : '     Qty of cargo'    NO-GAP, SY-VLINE NO-GAP.
    ENDIF.
    WRITE :   ' Cleared req. Qty'    NO-GAP, SY-VLINE NO-GAP,
              '      Cleared Qty'    NO-GAP, SY-VLINE NO-GAP,
              '  Not cleared Qty'    NO-GAP, SY-VLINE NO-GAP,
              '          G/R Qty'    NO-GAP, SY-VLINE NO-GAP,
              '     Invoiced Qty'    NO-GAP, SY-VLINE NO-GAP.
    IF ZTIMIMG00-ZFCGYN EQ 'X'.
      WRITE : ' B/L Cargo Differ.'    NO-GAP, SY-VLINE NO-GAP.
    ENDIF.
    WRITE : 'Delivery com'         NO-GAP, SY-VLINE NO-GAP.
    IF ZTIMIMG00-ZFCGYN EQ 'X'.
      WRITE : / SY-ULINE(212) NO-GAP.
    ELSE.
      WRITE : / SY-ULINE(175) NO-GAP.
    ENDIF.
  ENDIF.

  IF WRITE_CHK = 'X'.
    FORMAT COLOR COL_BACKGROUND.
    WRITE :/  SY-VLINE NO-GAP,
              'Material :'                    NO-GAP,
              SV_MATNR                        NO-GAP,
              ' '                             NO-GAP,
              SV_MATNM                        NO-GAP,
              'Unit :  '                      NO-GAP,
              SV_MEINS                        NO-GAP,
              '                    '          NO-GAP,
              '                    '          NO-GAP,
              '                    '          NO-GAP,
              '          '                    NO-GAP.

    IF ZTIMIMG00-ZFCGYN EQ 'X'.
      WRITE : 212 SY-VLINE NO-GAP, / SY-ULINE(212) NO-GAP.
    ELSE.
      WRITE : 175 SY-VLINE NO-GAP, / SY-ULINE(175) NO-GAP.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_WRITE
*&---------------------------------------------------------------------*
FORM P3000_SUM_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.

  IF ZTIMIMG00-ZFCGYN EQ 'X'.
    WRITE : / SY-ULINE(212).
  ELSE.
    WRITE : / SY-ULINE(175).
  ENDIF.
  WRITE:/ SY-VLINE NO-GAP,
       'Sum(Material)'          NO-GAP,
       '   '                    NO-GAP, SY-VLINE NO-GAP,
       SUM_PO_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       SUM_RE_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       SUM_BL_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP.
  IF ZTIMIMG00-ZFCGYN EQ 'X'.
    WRITE: SUM_CG_MENGE UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP.
  ENDIF.
  WRITE:
       SUM_IV_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       SUM_IDS_MENGE    UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       SUM_NO_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       SUM_IN_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       SUM_CIV_MENGE    UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP.
  IF ZTIMIMG00-ZFCGYN EQ 'X'.
    WRITE: SUM_BLCG_MENGE UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP.
  ENDIF.
    WRITE:
         ' '                             NO-GAP,
         '           '                   NO-GAP, SY-VLINE NO-GAP.
  IF ZTIMIMG00-ZFCGYN EQ 'X'.
    WRITE : / SY-ULINE(212).
  ELSE.
    WRITE : / SY-ULINE(175).
  ENDIF.

ENDFORM.                    " P3000_SUM_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
  FORMAT RESET.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  WRITE:/ SY-VLINE NO-GAP,
       IT_TAB-EBELN         NO-GAP,
       ' '                     NO-GAP,
       IT_TAB-EBELP        UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-MENGE        UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-RE_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-BL_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP.
  IF ZTIMIMG00-ZFCGYN EQ 'X'.
    WRITE: IT_TAB-CG_MENGE UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP.
  ENDIF.
   WRITE:
       IT_TAB-IV_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-IDS_MENGE    UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-NO_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-IN_MENGE     UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-CIV_MENGE    UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP.
  IF ZTIMIMG00-ZFCGYN EQ 'X'.
    WRITE: IT_TAB-BLCG_MENGE UNIT SV_MEINS  NO-GAP, SY-VLINE NO-GAP.
  ENDIF.
    WRITE: IT_TAB-IN_CHK NO-GAP, SY-VLINE NO-GAP.
* Stored value...
  HIDE IT_TAB.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

*  WRITE:/ SY-VLINE NO-GAP,
*       '자재별 합계 '          NO-GAP,
*       '    '                  NO-GAP, SY-VLINE NO-GAP,
*       TOT_PO_MENGE            NO-GAP, SY-VLINE NO-GAP,
*       TOT_RE_MENGE            NO-GAP, SY-VLINE NO-GAP,
*       TOT_BL_MENGE            NO-GAP, SY-VLINE NO-GAP,
*       TOT_CG_MENGE            NO-GAP, SY-VLINE NO-GAP,
*       TOT_IV_MENGE            NO-GAP, SY-VLINE NO-GAP,
*       TOT_IDS_MENGE           NO-GAP, SY-VLINE NO-GAP,
*       TOT_IN_MENGE            NO-GAP, SY-VLINE NO-GAP,
*       TOT_CIV_MENGE           NO-GAP, SY-VLINE NO-GAP,
*       ' '                     NO-GAP,
*       '           '           NO-GAP, SY-VLINE NO-GAP.
*  WRITE : / SY-ULINE.

  WRITE:/ SY-VLINE NO-GAP,
       '자재별 합계 '          NO-GAP,
       '    '                  NO-GAP, SY-VLINE NO-GAP,
       TOT_PO_MENGE  UNIT SV_MEINS         NO-GAP, SY-VLINE NO-GAP,
       TOT_RE_MENGE  UNIT SV_MEINS        NO-GAP, SY-VLINE NO-GAP,
       TOT_BL_MENGE  UNIT SV_MEINS        NO-GAP, SY-VLINE NO-GAP,
       TOT_CG_MENGE  UNIT SV_MEINS        NO-GAP, SY-VLINE NO-GAP,
       TOT_IV_MENGE  UNIT SV_MEINS          NO-GAP, SY-VLINE NO-GAP,
       TOT_IDS_MENGE UNIT SV_MEINS        NO-GAP, SY-VLINE NO-GAP,
       TOT_NO_MENGE  UNIT SV_MEINS NO-GAP, SY-VLINE NO-GAP,  "미통관 TOT
       TOT_IN_MENGE  UNIT SV_MEINS NO-GAP, SY-VLINE NO-GAP,  "입고   TOT
       TOT_CIV_MENGE UNIT SV_MEINS NO-GAP, SY-VLINE NO-GAP,  "송장   TOT
       TOT_BLCG_MENGE UNIT SV_MEINS NO-GAP, SY-VLINE NO-GAP,
       '            '          NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_CIV_QTY
*&---------------------------------------------------------------------*
FORM P4000_COMPUTE_CIV_QTY.
  LOOP  AT  IT_CIV WHERE  EBELN  =  IT_PO-EBELN
                   AND    EBELP  =  IT_PO-EBELP.

    ADD  IT_CIV-CMENGE   TO  SUM_CIV_MENGE.
    ADD  IT_CIV-CMENGE   TO  TOT_CIV_MENGE.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_CIV_QTY
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.
  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Status List'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.
  LEAVE TO LIST-PROCESSING.
  CASE INCLUDE.
    WHEN 'POPU'.
      READ  TABLE  IT_PO  WITH  KEY  EBELN  =  W_EBELN
                                     EBELP  =  W_EBELP.
      IF SY-SUBRC EQ 0.
        PERFORM  P1000_WRITE_PO_SCR0100.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_PO_SCR0100
*&---------------------------------------------------------------------*
FORM P1000_WRITE_PO_SCR0100.
*>> PO DATA WRITE.
  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.

  WRITE: /  'P/O       ' NO-GAP,
         15 IT_PO-EBELN  NO-GAP.
  WRITE: '-'             NO-GAP,
            IT_PO-EBELP  NO-GAP,
         68 IT_PO-MENGE  UNIT IT_PO-MEINS NO-GAP,
         85 IT_PO-MEINS  NO-GAP.
  HIDE IT_PO.
*>> 수입의뢰 DATA WRITE.
  LOOP  AT  IT_RN  WHERE  EBELN  =  W_EBELN  AND  EBELP = W_EBELP.
    PERFORM  P2000_WRITE_RN_SCR0100.
  ENDLOOP.

ENDFORM.                    " P1000_WRITE_PO_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_RN_SCR0100
*&---------------------------------------------------------------------*
FORM P2000_WRITE_RN_SCR0100.
  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.

  WRITE: / 'Import Req '    NO-GAP,
          17  IT_RN-ZFREQNO NO-GAP,
         '-'               NO-GAP,
             IT_RN-ZFITMNO NO-GAP,
         45  IT_RN-ZFOPNNO(20) NO-GAP,
         68 IT_RN-MENGE UNIT IT_RN-MEINS NO-GAP,
         85 IT_RN-MEINS NO-GAP.
  HIDE IT_RN.

  IF IT_RN-ZFREQTY EQ 'LO' OR IT_RN-ZFREQTY EQ 'PU'.
    LOOP  AT  IT_IV  WHERE  ZFREQNO EQ IT_RN-ZFREQNO
                     AND    ZFITMNO EQ IT_RN-ZFITMNO.
      PERFORM  P5000_WRITE_IV_SCR0100.
    ENDLOOP.
  ELSE.
    LOOP  AT  IT_BL  WHERE  ZFREQNO  =  IT_RN-ZFREQNO
                  AND    ZFITMNO  =  IT_RN-ZFITMNO.
      PERFORM  P3000_WRITE_BL_SCR0100.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P2000_WRITE_RN_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_BL_SCR0100
*&---------------------------------------------------------------------*
FORM P3000_WRITE_BL_SCR0100.

  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.
  WRITE: /'B/L       '      NO-GAP,
         19  IT_BL-ZFBLNO   NO-GAP,
         '-'                NO-GAP,
            IT_BL-ZFITMNO  NO-GAP,
         45 IT_BL-ZFFORD NO-GAP,
         68 IT_BL-BLMENGE UNIT IT_BL-MEINS NO-GAP,
         85 IT_BL-MEINS NO-GAP.
  HIDE  IT_BL.

  LOOP  AT  IT_CG  WHERE  ZFBLNO = IT_BL-ZFBLNO
                   AND    ZFBLIT = IT_BL-ZFBLIT.
    W_LOOP_CNT = W_LOOP_CNT + 1.
    PERFORM P4000_WRITE_CG_SCR0100.
  ENDLOOP.

  IF W_LOOP_CNT < 1.
    LOOP  AT  IT_IV WHERE  ZFBLNO = IT_BL-ZFBLNO
                    AND    ZFBLIT = IT_BL-ZFBLIT.
      W_TABIX  =  SY-TABIX.
      PERFORM P5000_WRITE_IV_SCR0100.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P3000_WRITE_BL_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_CG_SCR0100
*&---------------------------------------------------------------------*
FORM P4000_WRITE_CG_SCR0100.
  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.
  WRITE: /'Cargo Work   '    NO-GAP,
          21   IT_CG-ZFCGNO NO-GAP,
         '-'               NO-GAP,
            IT_CG-ZFCGIT   NO-GAP,
         45 IT_CG-ZFCGPT   NO-GAP,
         68 IT_CG-CGMENGE UNIT IT_CG-MEINS NO-GAP,
         85 IT_CG-MEINS NO-GAP.
  HIDE IT_CG.

  LOOP  AT  IT_IV WHERE  ZFCGNO  =  IT_CG-ZFCGNO
                  AND    ZFCGIT  =  IT_CG-ZFCGIT.
    W_TABIX  =  SY-TABIX.
    PERFORM  P5000_WRITE_IV_SCR0100.
  ENDLOOP.
ENDFORM.                    " P4000_WRITE_CG_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P5000_WRITE_IV_SCR0100
*&---------------------------------------------------------------------*
FORM P5000_WRITE_IV_SCR0100.
  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.
*>> 자재문서 DISPLAY
  SELECT SINGLE *     FROM ZTIVHST
  WHERE  ZFIVNO       EQ   IT_IV-ZFIVNO
  AND    ZFIVHST      EQ   ( SELECT MAX( ZFIVHST )
                             FROM   ZTIVHST
                             WHERE  ZFIVNO  EQ  IT_IV-ZFIVNO ).

  MOVE  ZTIVHST-MBLNR       TO   IT_IV-MBLNR.
  MOVE  ZTIVHST-MJAHR       TO   IT_IV-MJAHR.
  MODIFY  IT_IV  INDEX      W_TABIX.

  WRITE: /'Clearance Req'      NO-GAP,
          23  IT_IV-ZFIVNO  NO-GAP,
          '-'              NO-GAP,
             IT_IV-ZFIVDNO NO-GAP,
          68 IT_IV-CCMENGE UNIT IT_IV-MEINS NO-GAP,
          85 IT_IV-MEINS NO-GAP.
  HIDE IT_IV.

  IF IT_IV-ZFBLNO IS INITIAL AND IT_IV-ZFCGNO IS INITIAL.
    IF IT_IV-ZFGRST = 'Y'.
      PERFORM P7000_WRITE_IN_SCRO100.
    ENDIF.
  ELSE.
    LOOP  AT  IT_IDS  WHERE  ZFIVNO  =  IT_IV-ZFIVNO
                      AND    ZFIVDNO =  IT_IV-ZFIVDNO.
      PERFORM  P6000_WRITE_IDR_SCR0100.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P5000_WRITE_IV_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P6000_WRITE_IDR_SCR0100
*&---------------------------------------------------------------------*
FORM P6000_WRITE_IDR_SCR0100.
  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.

  WRITE: /' Cust.Clrnc '    NO-GAP,
          23 IT_IDS-ZFENTNO NO-GAP,
          45 IT_IDS-ZFCUT   NO-GAP,
          68 IT_IDS-ZFQNT   UNIT  IT_IDS-ZFQNTM NO-GAP,
          85 IT_IDS-ZFQNTM  NO-GAP.
  HIDE  IT_IDS.

  IF IT_IV-ZFGRST = 'Y'.
    PERFORM P7000_WRITE_IN_SCRO100.
  ENDIF.

ENDFORM.                    " P6000_WRITE_IDR_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P7000_WRITE_IN_SCRO100
*&---------------------------------------------------------------------*
FORM P7000_WRITE_IN_SCRO100.

  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.

*>> PLANT 명 DISPLAY
  SELECT SINGLE NAME1 INTO W_NAME1
  FROM   T001W
  WHERE  WERKS  EQ  IT_IV-WERKS.
*>> 저장위치 명 DISPLAY.
  SELECT SINGLE LGOBE  INTO W_LGOBE
  FROM   T001L
  WHERE  WERKS  EQ  IT_IV-WERKS
  AND    LGORT  EQ  IT_IV-LGORT.

  WRITE: /'Good Receipt'    NO-GAP,
          25 IT_IV-MJAHR    NO-GAP,
             '-'            NO-GAP,
             IT_IV-MBLNR    NO-GAP,
          45 IT_IV-WERKS    NO-GAP,
             ' '            NO-GAP,
             W_NAME1        NO-GAP,
             ' '            NO-GAP,
             IT_IV-LGORT    NO-GAP,
             W_LGOBE        NO-GAP,
          68 IT_IV-GRMENGE  UNIT IT_IV-MEINS NO-GAP,
          85 IT_IV-MEINS    NO-GAP.

ENDFORM.                    " P7000_WRITE_IN_SCRO100
