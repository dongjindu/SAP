*&---------------------------------------------------------------------*
*& Report  ZRIMSTCVLM                                                  *
*&---------------------------------------------------------------------*
*&  Program Name : Inventory Quantity by Import progress steps         *
*&  Created by   : Jung Seung Yeon INFOLINK Ltd.                       *
*&  Created on   : 2003.01.20                                          *
*&---------------------------------------------------------------------*
*&   DESC. : 1.
*&
*&---------------------------------------------------------------------*
*& [Change Log]
*&
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMSTCVLM   MESSAGE-ID ZIM
                     LINE-SIZE 255
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE : <ICON>,
          <SYMBOL>.
INCLUDE ZRIMSTCVLMTOP.

*-----------------------------------------------------------------------
* Serach Condition Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_WERKS   FOR EKPO-WERKS,       " Plant
               S_EKGRP   FOR EKKO-EKGRP,       " Purch. Grp.
               S_MATNR   FOR EKPO-EMATN,       " Material No.
               S_MATKL   FOR EKPO-MATKL,       " Material Group.
               S_LIFNR   FOR EKKO-LIFNR,       " Vendor.
               S_EBELN   FOR EKKO-EBELN,       " P/O No.
               S_HBLNO   FOR ZTBL-ZFHBLNO,     " House B/L
               S_CONT    FOR LIKP-TRAID,       " Container No
               S_ZTERM   FOR EKKO-ZTERM.       " Payment Term
PARAMETERS :   P_DATE    LIKE SY-DATUM
                              DEFAULT SY-DATUM. "Basic Date.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.

SELECTION-SCREEN: BEGIN OF LINE, POSITION 3.
PARAMETERS: R_WERKS  RADIOBUTTON GROUP B2.
SELECTION-SCREEN: COMMENT   7(20)  TEXT-011, POSITION  30.
PARAMETERS: R_EKGRP  RADIOBUTTON GROUP B2.
SELECTION-SCREEN: COMMENT  34(20)  TEXT-012.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE, POSITION 3.
PARAMETERS: R_MATKL  RADIOBUTTON GROUP B2.
SELECTION-SCREEN: COMMENT   7(20)  TEXT-014, POSITION  30.
PARAMETERS: R_LIFNR  RADIOBUTTON GROUP B2.
SELECTION-SCREEN: COMMENT  34(20)  TEXT-015, POSITION 56.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN END OF BLOCK B3.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  SET TITLEBAR 'TIT1'.
*----------------------------------------------------------------------*
* EVENT TOP-OF-PAGE.
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU'.
    PERFORM   P3000_TITLE_WRITE.       " MAIN LIST TITLE.
  ELSE.
    PERFORM   P3000_DETAIL_TITLE.      " DETAIL LIST TITLE  .
  ENDIF.
*----------------------------------------------------------------------*
* START OF SELECT.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM P2000_GROUP_SETTING..

*-> Payment Term Configuration Select.
  PERFORM P1000_READ_ZTERM.
  CHECK W_ERR_CHK NE 'Y'.

*-> P/O Selection.
  PERFORM P1000_READ_PO_DATA.
  CHECK W_ERR_CHK NE 'Y'.

  REFRESH : IT_TAB.
  CLEAR   : IT_TAB.
*<< P/O ~ L/C open >>===============================>
  PERFORM P1000_READ_RN_DATA USING W_ERR_CHK.

*<< L/C open ~ B/L >>===============================>
  PERFORM P1000_READ_BL_DATA.

*<< B/L ~ Arrival >>================================>
  PERFORM P1000_READ_IB_DATA.

*<< Arrival ~ Warehouse in >>=======================>
  PERFORM P1000_READ_TR_DATA.

*<< Warehouse-in ~ Customs Clearance >>=============>
  PERFORM P1000_READ_ST_DATA.

*<< Customs Clearance ~ G/R.>>======================>
  PERFORM P1000_READ_GR_DATA.

*-----------------------------------------------------------------------
* EVENT END-OF-SELECTION.
*-----------------------------------------------------------------------
END-OF-SELECTION.

  CHECK W_ERR_CHK NE 'Y'.

* Title Text Write.
  SET TITLEBAR 'TIT1'.
  SET PF-STATUS 'ZIM95'.

* DATA WRITE
  PERFORM   P2000_WRITE_DATA.
  CLEAR : IT_TAB.
*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
*------- Abbrechen (CNCL) ----------------------------------------------
    WHEN 'CNCL'.
      SET SCREEN 0.    LEAVE SCREEN.
*------- Download (DOWN) -------------------------------
    WHEN 'DOWN'.
      PERFORM P3000_TO_PC_DOWNLOAD.
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
  DATA : L_TEXT(30).

*>> 상세 LIST DISPLAY 화면 OPEN.
  IF INCLUDE NE 'POPU'.
    CHECK NOT IT_TAB IS INITIAL.
    GET CURSOR FIELD L_TEXT.
    W_SELECT = L_TEXT+7(2).
    IF W_SELECT NE 'LC' AND W_SELECT NE 'BL'  AND
       W_SELECT NE 'IB' AND W_SELECT NE 'TR'  AND
       W_SELECT NE 'ST' AND W_SELECT NE 'GR'.
      EXIT.
    ENDIF.
    INCLUDE = 'POPU'.
    CALL SCREEN 0100 STARTING AT  1     6
                     ENDING   AT  178   34.
    CLEAR : INCLUDE, L_TEXT, W_SELECT.
*>> 각 문서의 상세화면 DISPLAY.
  ELSE.
    CLEAR : L_TEXT.
    GET CURSOR FIELD L_TEXT.
    CASE  L_TEXT(9).
      WHEN 'IT_LCTAB2'.
        SET PARAMETER ID 'BES'  FIELD IT_LCTAB2-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      WHEN 'IT_OPTAB2'.
        SET PARAMETER ID 'ZPOPNNO'  FIELD  ' '.
        SET PARAMETER ID 'BES'      FIELD  ' '.
        SET PARAMETER ID 'ZPREQNO'  FIELD  IT_OPTAB2-ZFREQNO.
        CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
      WHEN 'IT_BLTAB2'.
        SET PARAMETER ID 'ZPHBLNO'  FIELD  ' '.
        SET PARAMETER ID 'ZPBLNO'   FIELD  IT_BLTAB2-ZFBLNO.
        CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
      WHEN 'IT_TRTAB2'.
        SET PARAMETER ID 'ZPBLNO'   FIELD  IT_TRTAB2-ZFBLNO.
        CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
      WHEN 'IT_STTAB2'.
        SET PARAMETER ID 'BES'      FIELD  ' '.
        SET PARAMETER ID 'ZPBLNO'   FIELD  IT_STTAB2-ZFBLNO.
        SET PARAMETER ID 'ZPHBLNO'  FIELD  ' '.
        SET PARAMETER ID 'ZPBTSEQ'  FIELD  ' '.
        CALL TRANSACTION 'ZIMI8' AND SKIP FIRST SCREEN.
      WHEN 'IT_GRTAB2'.
        SET PARAMETER ID 'ZPHBLNO'  FIELD  ' '.
        SET PARAMETER ID 'ZPBLNO'   FIELD  IT_GRTAB2-ZFBLNO.
        SET PARAMETER ID 'ZPCLSEQ'  FIELD  ' '.
        SET PARAMETER ID 'ZPIDRNO'  FIELD  ' '.
        CALL TRANSACTION 'ZIM76' AND SKIP FIRST SCREEN.
    ENDCASE.

  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTERM
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTERM.

  CLEAR : ZTIMIMG00.
  SELECT SINGLE * FROM ZTIMIMG00.

  SELECT BSART BSTYP ZTERM  MAX( ZFAPLDT ) AS ZFAPLDT
    INTO   CORRESPONDING FIELDS OF TABLE IT_IMG01
    FROM   ZTIMIMG01
   WHERE   ZFAPLDT   LE   P_DATE
   GROUP BY BSART BSTYP ZTERM.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S353.
    EXIT.
  ENDIF.

ENDFORM.                               " P1000_READ_ZTERM
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

  W_ERR_CHK = 'N'.
  REFRESH : IT_PO.
  LOOP AT IT_IMG01.
    SELECT  *
         APPENDING CORRESPONDING FIELDS OF TABLE IT_PO
         FROM EKKO AS  A INNER JOIN  EKPO AS B
         ON    A~EBELN          EQ   B~EBELN
         WHERE A~ZTERM          EQ   IT_IMG01-ZTERM
         AND   A~BSTYP          EQ   IT_IMG01-BSTYP
         AND   A~BSART          EQ   IT_IMG01-BSART
*            AND   A~AUTLF          EQ   SPACE
         AND   A~LOEKZ          EQ   SPACE
         AND   B~LOEKZ          EQ   SPACE
*            AND   B~ELIKZ          EQ   SPACE        " 납품완료지시자.
         AND   B~WERKS          IN   S_WERKS         " 플랜트.
         AND   A~EKGRP          IN   S_EKGRP         " 구매그룹.
         AND   B~MATNR          IN   S_MATNR         " 자재.
         AND   B~MATKL          IN   S_MATKL         " 자재그룹.
         AND   A~LIFNR          IN   S_LIFNR         " Vendor.
         AND   A~EBELN          IN   S_EBELN         " P/O번호.
         AND   A~ZTERM          IN   S_ZTERM         " 지급조건.
         AND   A~AEDAT          LE   P_DATE.         " P/O생성일.

  ENDLOOP.

  DATA : W_MENGE LIKE EKPO-MENGE.
  DATA : W_MENGE_OLD LIKE EKPO-MENGE.

  LOOP AT IT_PO.
    W_TABIX = SY-TABIX.
    W_MENGE_OLD = IT_PO-MENGE.

    SELECT GRMENGE INTO W_MENGE
      FROM  ZTIVHST AS H INNER  JOIN ZTIVHSTIT AS I
        ON   H~ZFIVNO     EQ  I~ZFIVNO
       AND   H~ZFIVHST    EQ  I~ZFIVHST
     WHERE   I~EBELN      EQ  IT_PO-EBELN
       AND   I~EBELP      EQ  IT_PO-EBELP
       AND  ( H~ZFGRST    EQ  'P'
                      OR  H~ZFGRST   EQ  'Y' )
       AND   H~BUDAT      LE  P_DATE.

      IT_PO-MENGE = IT_PO-MENGE - W_MENGE.

    ENDSELECT.

    IF IT_PO-MENGE LE 0.
      DELETE IT_PO INDEX W_TABIX.
    ELSEIF W_MENGE_OLD NE IT_PO-MENGE.
      MODIFY IT_PO INDEX W_TABIX.
    ENDIF.

    IF NOT S_HBLNO[] IS INITIAL.

       CLEAR : W_BL_CNT.
       SELECT COUNT( * )   INTO  W_BL_CNT
       FROM   ZTBL  AS   A INNER JOIN ZTBLIT AS B
       ON     A~ZFBLNO     EQ    B~ZFBLNO
       WHERE  A~ZFHBLNO    IN    S_HBLNO
       AND    B~EBELN      EQ    IT_PO-EBELN
       AND    B~EBELP      EQ    IT_PO-EBELP.

       IF W_BL_CNT LE 0.
          DELETE  IT_PO  INDEX  W_TABIX.
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
       AND    B~VGBEL      EQ    IT_PO-EBELN
       AND    B~VGPOS      EQ    IT_PO-EBELP.

       IF W_TR_CNT LE 0.
          DELETE  IT_PO  INDEX  W_TABIX.
          CONTINUE.
       ENDIF.
     ENDIF.

  ENDLOOP.

  DESCRIBE TABLE IT_PO LINES W_COUNT.

  IF W_COUNT LE 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S966.
    EXIT.
  ENDIF.

ENDFORM.                               " P1000_READ_PO_DATA

*&------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&------------------------------------------------------------------*
FORM P1000_READ_RN_DATA USING W_ERR_CHK.

  REFRESH : IT_LCTAB1, IT_LCTAB2.
  CLEAR   : IT_LCTAB1, IT_LCTAB2.

  LOOP AT IT_PO .
    SELECT *
       APPENDING CORRESPONDING FIELDS OF TABLE IT_RN
       FROM   ( ZTREQHD AS H INNER JOIN ZTREQIT AS I
         ON     H~ZFREQNO     EQ  I~ZFREQNO             )
               INNER JOIN  ZTREQST AS  P
               ON     I~ZFREQNO     EQ  P~ZFREQNO
       WHERE   I~EBELN     EQ   IT_PO-EBELN
         AND   I~EBELP     EQ   IT_PO-EBELP
         AND   P~ZFDOCST   NE   'A'
         AND   P~ZFOPNDT   NE   '00000000'
         AND   P~ZFOPNDT   LE    P_DATE.

    IF SY-SUBRC EQ 0.
      LOOP AT IT_RN WHERE  EBELN     EQ   IT_PO-EBELN
                      AND  EBELP     EQ   IT_PO-EBELP.
        IT_PO-MENGE = IT_PO-MENGE - IT_RN-MENGE.
      ENDLOOP.
      IF IT_PO-MENGE LE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

*> Main List Table Insert
    MOVE-CORRESPONDING IT_PO TO IT_LCTAB1.
    IF IT_PO-PEINH NE 0.
      IT_LCTAB1-ZFAMT =
           ( IT_LCTAB1-MENGE * ( IT_PO-BPUMZ / IT_PO-BPUMN )
         * ( IT_PO-NETPR / IT_PO-PEINH ) ).
    ENDIF.
    APPEND IT_LCTAB1.
    PERFORM P2000_MOVE_TAB USING IT_PO-MEINS
                                 IT_PO-WERKS
                                 IT_PO-EKGRP
                                 IT_PO-MATKL
                                 IT_PO-MATNR
                                 IT_PO-LIFNR
                                 IT_LCTAB1-MENGE
                                 'LC'.

*> Detail List Table Insert
    MOVE-CORRESPONDING IT_PO TO IT_LCTAB2.
    MOVE IT_LCTAB1-ZFAMT TO IT_LCTAB2-ZFAMT.
    APPEND IT_LCTAB2.

    CLEAR : IT_LCTAB1, IT_LCTAB2.
  ENDLOOP.

ENDFORM.                               " P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

  REFRESH : IT_OPTAB1, IT_OPTAB2, IT_BL.
  CLEAR   : IT_OPTAB1, IT_OPTAB2, IT_BL, IT_PO, IT_RN.
  LOOP AT IT_RN .
    SELECT *
     APPENDING  CORRESPONDING FIELDS OF TABLE IT_BL
     FROM  ZTBL AS H INNER  JOIN ZTBLIT AS I
     ON    H~ZFBLNO     EQ  I~ZFBLNO
     WHERE I~ZFREQNO    EQ  IT_RN-ZFREQNO
     AND   I~ZFITMNO    EQ  IT_RN-ZFITMNO
     AND   H~CDAT       LE  P_DATE.

    IF SY-SUBRC EQ 0.
      LOOP AT IT_BL WHERE  ZFREQNO     EQ   IT_RN-ZFREQNO
                      AND  ZFITMNO     EQ   IT_RN-ZFITMNO.
        IT_RN-MENGE = IT_RN-MENGE - IT_BL-BLMENGE.
      ENDLOOP.
      IF IT_RN-MENGE LE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE IT_PO WITH KEY EBELN = IT_RN-EBELN
                              EBELP = IT_RN-EBELP.
    IF SY-SUBRC EQ 0.
*> Main List Table Insert
      MOVE-CORRESPONDING IT_PO TO IT_OPTAB1.
      MOVE IT_RN-MENGE TO IT_OPTAB1-MENGE.
      IF  IT_PO-PEINH NE 0.
        IT_OPTAB1-ZFAMT =
           ( IT_OPTAB1-MENGE * ( IT_PO-BPUMZ / IT_PO-BPUMN )
         * ( IT_PO-NETPR / IT_PO-PEINH ) ).
      ENDIF.
      COLLECT IT_OPTAB1.
      PERFORM P2000_MOVE_TAB USING IT_PO-MEINS
                                   IT_PO-WERKS
                                   IT_PO-EKGRP
                                   IT_PO-MATKL
                                   IT_PO-MATNR
                                   IT_PO-LIFNR
                                   IT_OPTAB1-MENGE
                                   'OP'.
*> Detail List Table Insert
      MOVE-CORRESPONDING IT_RN TO IT_OPTAB2.
      MOVE : IT_PO-WERKS     TO IT_OPTAB2-WERKS,
             IT_PO-MATKL     TO IT_OPTAB2-MATKL,
             IT_OPTAB1-ZFAMT TO IT_OPTAB2-ZFAMT.
      APPEND IT_OPTAB2.

    ENDIF.

    CLEAR   : IT_OPTAB1, IT_OPTAB2, IT_BL, IT_PO.
  ENDLOOP.

ENDFORM.                               " P1000_READ_BL_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IB_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_IB_DATA.
  DATA : WL_BDATE LIKE SY-DATUM.
  REFRESH : IT_BLTAB1, IT_BLTAB2, IT_IB.
  CLEAR   : IT_BLTAB1, IT_BLTAB2, IT_IB, IT_PO, IT_BL.
  LOOP AT IT_BL .

    IF ZTIMIMG00-ZFIBYN = 'X'.
      IF ZTIMIMG00-ZFIBBD = 'ETA'.
        WL_BDATE = IT_BL-ZFETA.
      ELSE.
        WL_BDATE = IT_BL-ZFRETA.
      ENDIF.
    ELSE.
      IF ZTIMIMG00-BLSTYN = 'X'.
        WL_BDATE = IT_BL-ZFRETA.
      ELSE.
        IF NOT IT_BL-ZFRETA IS INITIAL.
           WL_BDATE = IT_BL-ZFRETA.
        ELSE.
           WL_BDATE = IT_BL-ZFETA.
        ENDIF.
      ENDIF.
    ENDIF.

    IF WL_BDATE  GT '00000000'
       AND WL_BDATE  LE P_DATE.
      MOVE-CORRESPONDING IT_BL TO IT_IB.
      APPEND IT_IB.

    ELSE.

      READ TABLE IT_PO WITH KEY  EBELN   =  IT_BL-EBELN
                                 EBELP   =  IT_BL-EBELP.
      IF SY-SUBRC EQ 0 .
*> Main List Table Insert
        MOVE-CORRESPONDING IT_PO TO IT_BLTAB1.
        MOVE IT_BL-BLMENGE TO IT_BLTAB1-MENGE.
        IF IT_PO-PEINH NE 0.
          IT_BLTAB1-ZFAMT =
             ( IT_BLTAB1-MENGE * ( IT_PO-BPUMZ / IT_PO-BPUMN )
           * ( IT_PO-NETPR / IT_PO-PEINH ) ).
        ENDIF.
        COLLECT IT_BLTAB1.
        PERFORM P2000_MOVE_TAB USING IT_PO-MEINS
                                     IT_PO-WERKS
                                     IT_PO-EKGRP
                                     IT_PO-MATKL
                                     IT_PO-MATNR
                                     IT_PO-LIFNR
                                     IT_BLTAB1-MENGE
                                     'BL'.
*> Detail List Table Insert
        MOVE-CORRESPONDING IT_BL TO IT_BLTAB2.
        MOVE : IT_PO-WAERS     TO IT_BLTAB2-WAERS,
               IT_PO-EKGRP     TO IT_BLTAB2-EKGRP,
               IT_BLTAB1-ZFAMT TO IT_BLTAB2-ZFAMT.
        APPEND IT_BLTAB2.

      ENDIF.
    ENDIF.
    CLEAR   : IT_OPTAB1, IT_OPTAB2, IT_BL, IT_PO.
  ENDLOOP.

ENDFORM.                               " P1000_READ_IB_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TR_DATA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM P1000_READ_TR_DATA.

  REFRESH : IT_TRTAB1, IT_TRTAB2, IT_TR, IT_IDS.
  CLEAR   : IT_TRTAB1, IT_TRTAB2, IT_TR, IT_PO, IT_IB, IT_IDS.
  LOOP AT IT_IB.
    SELECT *
     APPENDING  CORRESPONDING FIELDS OF TABLE IT_TR
     FROM  ZTBLINR AS H INNER  JOIN ZTBLIT AS I
       ON    H~ZFBLNO     EQ  I~ZFBLNO
     WHERE I~ZFBLNO     EQ  IT_IB-ZFBLNO
     AND   I~ZFBLIT     EQ  IT_IB-ZFBLIT
     AND   H~CDAT       LE   P_DATE.

    IF SY-SUBRC EQ 0.
      CONTINUE.
    ELSE.
      SELECT *
       APPENDING  CORRESPONDING FIELDS OF TABLE IT_IDS
       FROM (  ZTIV AS H INNER  JOIN ZTIVIT AS I
         ON    H~ZFIVNO     EQ  I~ZFIVNO )
             INNER JOIN ZTIDSUS AS D
             ON  H~ZFIVNO   EQ  D~ZFIVNO
       WHERE I~ZFBLNO    EQ  IT_IB-ZFBLNO
       AND   I~ZFBLIT    EQ  IT_IB-ZFBLIT
       AND   H~ZFCUST    EQ  'Y'
       AND   D~ZFEDT     LE  P_DATE.

      IF SY-SUBRC EQ 0.
        CLEAR : IT_TR.
        MOVE-CORRESPONDING IT_IB TO IT_TR.
        CLEAR : IT_TR-BLMENGE.
        LOOP AT IT_IDS  WHERE  ZFBLNO    EQ  IT_IB-ZFBLNO
                          AND  ZFBLIT    EQ  IT_IB-ZFBLIT.
          IT_TR-BLMENGE = IT_TR-BLMENGE + IT_IDS-CCMENGE.
        ENDLOOP.
        APPEND IT_TR.
        IT_IB-BLMENGE = IT_IB-BLMENGE - IT_TR-BLMENGE.
        IF IT_IB-BLMENGE LE 0.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE IT_PO   WITH KEY   EBELN = IT_IB-EBELN
                                  EBELP = IT_IB-EBELP.
    IF SY-SUBRC EQ 0 .
*> Main List Table Insert
      MOVE-CORRESPONDING IT_PO TO IT_TRTAB1.
      MOVE IT_IB-BLMENGE  TO IT_TRTAB1-MENGE.
      IF  IT_PO-PEINH NE 0.
        IT_TRTAB1-ZFAMT =
           ( IT_TRTAB1-MENGE * ( IT_PO-BPUMZ / IT_PO-BPUMN )
         * ( IT_PO-NETPR / IT_PO-PEINH ) ).
      ENDIF.
      COLLECT IT_TRTAB1.
      PERFORM P2000_MOVE_TAB USING IT_PO-MEINS
                                   IT_PO-WERKS
                                   IT_PO-EKGRP
                                   IT_PO-MATKL
                                   IT_PO-MATNR
                                   IT_PO-LIFNR
                                   IT_TRTAB1-MENGE
                                   'TR'.
*> Detail List Table Insert
      MOVE-CORRESPONDING IT_IB TO IT_TRTAB2.
      MOVE : IT_PO-WAERS        TO   IT_TRTAB2-WAERS,
             IT_PO-EKGRP        TO   IT_TRTAB2-EKGRP,
             IT_TRTAB1-ZFAMT    TO   IT_TRTAB2-ZFAMT.
      APPEND IT_TRTAB2.

    ENDIF.
    CLEAR   : IT_TRTAB1, IT_TRTAB2, IT_TR, IT_PO.
  ENDLOOP.

ENDFORM.                               " P1000_READ_TR_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ST_DATA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM P1000_READ_ST_DATA.

  REFRESH : IT_STTAB1, IT_STTAB2, IT_IDS.
  CLEAR   : IT_STTAB1, IT_STTAB2, IT_IDS, IT_PO, IT_TR.
  LOOP AT IT_TR.
    SELECT *
     APPENDING  CORRESPONDING FIELDS OF TABLE IT_IDS
     FROM (  ZTIV AS H INNER  JOIN ZTIVIT AS I
       ON    H~ZFIVNO     EQ  I~ZFIVNO )
           INNER JOIN ZTIDSUS AS D
           ON  H~ZFIVNO   EQ  D~ZFIVNO
     WHERE I~ZFBLNO    EQ  IT_TR-ZFBLNO
     AND   I~ZFBLIT    EQ  IT_TR-ZFBLIT
     AND   H~ZFCUST    EQ  'Y'
     AND   D~ZFEDT     LE  P_DATE.

    IF SY-SUBRC EQ 0.
      LOOP AT IT_IDS  WHERE  ZFBLNO    EQ  IT_TR-ZFBLNO
                        AND  ZFBLIT    EQ  IT_TR-ZFBLIT.
        IT_TR-BLMENGE = IT_TR-BLMENGE - IT_IDS-CCMENGE.
      ENDLOOP.
      IF IT_TR-BLMENGE LE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE IT_PO  WITH KEY  EBELN = IT_TR-EBELN
                                EBELP = IT_TR-EBELP.
    IF SY-SUBRC EQ 0 .
*> Main List Table Insert
      MOVE-CORRESPONDING IT_PO TO IT_STTAB1.
      MOVE IT_TR-BLMENGE TO IT_STTAB1-MENGE.
      IF IT_PO-PEINH NE 0.
        IT_STTAB1-ZFAMT =
           ( IT_STTAB1-MENGE * ( IT_PO-BPUMZ / IT_PO-BPUMN )
         * ( IT_PO-NETPR / IT_PO-PEINH ) ).
      ENDIF.
      COLLECT IT_STTAB1.
      PERFORM P2000_MOVE_TAB USING IT_PO-MEINS
                                   IT_PO-WERKS
                                   IT_PO-EKGRP
                                   IT_PO-MATKL
                                   IT_PO-MATNR
                                   IT_PO-LIFNR
                                   IT_STTAB1-MENGE
                                   'ST'.
*> Detail List Table Insert
      MOVE-CORRESPONDING IT_TR TO IT_STTAB2.
      MOVE : IT_PO-WAERS       TO   IT_STTAB2-WAERS,
             IT_PO-EKGRP       TO   IT_STTAB2-EKGRP,
             IT_PO-LIFNR       TO   IT_STTAB2-LIFNR,
             IT_STTAB1-ZFAMT   TO   IT_STTAB2-ZFAMT.
      CLEAR IT_IB.
      READ TABLE IT_IB WITH KEY ZFBLNO = IT_STTAB2-ZFBLNO.
      MOVE : IT_IB-ZFHBLNO     TO   IT_STTAB2-ZFHBLNO,
             IT_IB-ZF20FT      TO   IT_STTAB2-ZF20FT,
             IT_IB-ZF40FT      TO   IT_STTAB2-ZF40FT.
*>  Bonded Area Name
      SELECT SINGLE ZFBNARM INTO IT_STTAB2-ZFBNARM
               FROM ZTIMIMG03
              WHERE ZFBNARCD  EQ  IT_TR-ZFBNARCD.

      APPEND IT_STTAB2.
    ENDIF.
    CLEAR   : IT_STTAB1, IT_STTAB2, IT_IDS, IT_PO, IT_IB.
  ENDLOOP.

ENDFORM.                               " P1000_READ_ST_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_GR_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_GR_DATA.

  REFRESH : IT_GRTAB1, IT_GRTAB2, IT_GR.
  CLEAR   : IT_GRTAB1, IT_GRTAB2, IT_GR, IT_PO, IT_IDS.
  LOOP AT IT_IDS.
    SELECT *
     APPENDING  CORRESPONDING FIELDS OF TABLE IT_GR
     FROM  ZTIVHST AS H INNER  JOIN ZTIVHSTIT AS I
       ON    H~ZFIVNO     EQ  I~ZFIVNO
       AND   H~ZFIVHST    EQ  I~ZFIVHST
     WHERE I~ZFIVNO     EQ  IT_IDS-ZFIVNO
     AND   I~ZFIVDNO    EQ  IT_IDS-ZFIVDNO
     AND  ( H~ZFGRST    EQ  'P'
         OR  H~ZFGRST   EQ  'Y' )
     AND   H~BUDAT      LE  P_DATE.

    IF SY-SUBRC EQ 0.
      LOOP AT IT_GR   WHERE  ZFIVNO     EQ  IT_IDS-ZFIVNO
                        AND  ZFIVDNO    EQ  IT_IDS-ZFIVDNO.
        IT_IDS-CCMENGE = IT_IDS-CCMENGE - IT_GR-GRMENGE.
      ENDLOOP.
      IF IT_IDS-CCMENGE LE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE IT_PO  WITH KEY  EBELN  = IT_IDS-EBELN
                                EBELP  = IT_IDS-EBELP.
    IF SY-SUBRC EQ 0 .
*> Main List Table Insert
      MOVE-CORRESPONDING IT_PO TO IT_GRTAB1.
      MOVE IT_IDS-CCMENGE TO IT_GRTAB1-MENGE.
      IF IT_PO-PEINH NE 0.
        IT_GRTAB1-ZFAMT =
           ( IT_GRTAB1-MENGE * ( IT_PO-BPUMZ / IT_PO-BPUMN )
         * ( IT_PO-NETPR / IT_PO-PEINH ) ).
      ENDIF.
      COLLECT IT_GRTAB1.
      PERFORM P2000_MOVE_TAB USING IT_PO-MEINS
                                   IT_PO-WERKS
                                   IT_PO-EKGRP
                                   IT_PO-MATKL
                                   IT_PO-MATNR
                                   IT_PO-LIFNR
                                   IT_GRTAB1-MENGE
                                   'GR'.
*> Detail List Table Insert
      MOVE-CORRESPONDING IT_IDS TO IT_GRTAB2.
      MOVE : IT_PO-WAERS      TO  IT_GRTAB2-WAERS,
             IT_PO-EKGRP      TO  IT_GRTAB2-EKGRP,
             IT_PO-MATKL      TO  IT_GRTAB2-MATKL,
             IT_GRTAB1-ZFAMT  TO  IT_GRTAB2-ZFAMT.
      CLEAR IT_IB.
      READ TABLE IT_IB WITH KEY ZFBLNO = IT_GRTAB2-ZFBLNO.
      MOVE : IT_IB-ZFHBLNO    TO  IT_GRTAB2-ZFHBLNO.
      APPEND IT_GRTAB2.

    ENDIF.
    CLEAR   : IT_GRTAB1, IT_GRTAB2, IT_GR, IT_PO, IT_IB.
  ENDLOOP.

ENDFORM.                               " P1000_READ_GR_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_DATA
*&---------------------------------------------------------------------*
*       DATA WRITE
*----------------------------------------------------------------------*
FORM P2000_WRITE_DATA.
*  DATA  : WL_WAERS LIKE IT_TAB-WAERS.
*  CLEAR : WL_WAERS.

  SORT IT_TAB BY (W_GROUP) MEINS.
  LOOP  AT  IT_TAB.

    W_COUNT = W_COUNT + 1.
    W_MOD   = W_COUNT MOD 2.

*    IF WL_WAERS NE IT_TAB-WAERS AND SY-TABIX NE 1.
*      PERFORM P3000_SUM_WRITE USING WL_WAERS.
*    ENDIF.

    FORMAT RESET.
    IF W_MOD = 1.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ENDIF.

*    IF WL_WAERS NE IT_TAB-WAERS.
*    WRITE : / SY-VLINE NO-GAP,
*           (05) IT_TAB-WAERS   NO-GAP, SY-VLINE NO-GAP.
*    ELSE.
*      WRITE : / SY-VLINE NO-GAP,
*             (05) ' '            NO-GAP, SY-VLINE NO-GAP.
*    ENDIF.

    PERFORM P3000_LINE_WRITE.

*    WL_WAERS = IT_TAB-WAERS.
*    AT LAST.
*      PERFORM P3000_SUM_WRITE USING WL_WAERS.
*      PERFORM P3000_LAST_WRITE.
*    ENDAT.
  ENDLOOP.

ENDFORM.                               " P2000_WRITE_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /70  '[ Inventory Quantity by Import progress steps ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Standard date   : ', P_DATE.

  NEW-LINE.
  WRITE AT 1(MAX_LINE) SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP.
  WRITE AT 2(TITLE_LINE) W_GR_TITLE  CENTERED NO-GAP.
  WRITE : SY-VLINE NO-GAP,
         (50) 'Materials'              CENTERED NO-GAP, SY-VLINE NO-GAP,
         (04) 'Unit'                   CENTERED NO-GAP, SY-VLINE NO-GAP.
  SET LEFT SCROLL-BOUNDARY.
  WRITE : (20) 'Ordering'              CENTERED NO-GAP, SY-VLINE NO-GAP,
          (20) 'L/C Open inventory'    CENTERED NO-GAP, SY-VLINE NO-GAP,
          (20) 'Shipment inventory'    CENTERED NO-GAP, SY-VLINE NO-GAP,
          (20) 'Arrival inventory'     CENTERED NO-GAP, SY-VLINE NO-GAP,
          (20) 'Bonded warehose inventory'
                                       CENTERED NO-GAP, SY-VLINE NO-GAP,
          (20) 'Not yet G/R inventory after clearance'
                                       CENTERED NO-GAP, SY-VLINE NO-GAP.
  NEW-LINE.
  WRITE AT 1(MAX_LINE) SY-ULINE.
ENDFORM.                               " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LAST_WRITE.


ENDFORM.                               " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_GROUP_SETTING.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_GROUP_SETTING.
  CASE 'X'.
    WHEN R_WERKS.
      W_GROUP = 'WERKS'. CODE_LINE = 4.  TEXT_LINE = 20.
      W_GR_TITLE = 'Plant'.
    WHEN R_EKGRP.
      W_GROUP = 'EKGRP'. CODE_LINE = 3.  TEXT_LINE = 18.
      W_GR_TITLE = 'Purchasing group'.
    WHEN R_MATKL.
      W_GROUP = 'MATKL'. CODE_LINE = 9.  TEXT_LINE = 20.
      W_GR_TITLE = 'Material group'.
    WHEN R_LIFNR.
      W_GROUP = 'LIFNR'. CODE_LINE = 10. TEXT_LINE = 30.
      W_GR_TITLE = 'Supplier'.
  ENDCASE.

  TITLE_LINE = CODE_LINE + TEXT_LINE + 1.
  MAX_LINE = 184 + TITLE_LINE.

ENDFORM.                               " P2000_GROUP_SETTING.
*&---------------------------------------------------------------------*
*&      Form  P2000_MOVE_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MOVE_TAB USING    P_MEINS
                             P_WERKS
                             P_EKGRP
                             P_MATKL
                             P_MATNR
                             P_LIFNR
                             P_MENGE
                             P_STEP.
  CLEAR : IT_TAB.
  IT_TAB-MEINS = P_MEINS.
  IT_TAB-MATNR = P_MATNR.
  CASE 'X'.
    WHEN R_WERKS.
      IT_TAB-WERKS = P_WERKS.
    WHEN R_EKGRP.
      IT_TAB-EKGRP = P_EKGRP.
    WHEN R_MATKL.
      IT_TAB-MATKL = P_MATKL.
    WHEN R_LIFNR.
      IT_TAB-LIFNR = P_LIFNR.
  ENDCASE.

  CASE P_STEP.
    WHEN 'LC'.
      IT_TAB-LC_MENGE = P_MENGE.
    WHEN 'OP'.
      IT_TAB-BL_MENGE = P_MENGE.
    WHEN 'BL'.
      IT_TAB-IB_MENGE = P_MENGE.
    WHEN 'TR'.
      IT_TAB-TR_MENGE = P_MENGE.
    WHEN 'ST'.
      IT_TAB-ST_MENGE = P_MENGE.
    WHEN 'GR'.
      IT_TAB-GR_MENGE = P_MENGE.
  ENDCASE.

  COLLECT : IT_TAB.
  CLEAR   : IT_TAB.

ENDFORM.                               " P2000_MOVE_TAB
*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_WRITE
*&---------------------------------------------------------------------*
*FORM P3000_SUM_WRITE USING    P_WAERS.
*
*  CLEAR   IT_TOT.
*  READ TABLE IT_TOT WITH KEY WAERS = P_WAERS.
*
*  FORMAT RESET.
*  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
*
*  WRITE : / SY-VLINE NO-GAP,
*       (05) P_WAERS   NO-GAP, SY-VLINE NO-GAP.
*
*  WRITE AT 8(TITLE_LINE) '소     계' CENTERED NO-GAP.
*
*  WRITE : SY-VLINE NO-GAP,
*         (20) IT_TOT-LC_ZFAMT CURRENCY P_WAERS
*                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP,
*         (20) IT_TOT-BL_ZFAMT CURRENCY P_WAERS
*                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP,
*         (20) IT_TOT-IB_ZFAMT CURRENCY P_WAERS
*                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP,
*         (20) IT_TOT-TR_ZFAMT CURRENCY P_WAERS
*                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP,
*         (20) IT_TOT-ST_ZFAMT CURRENCY P_WAERS
*                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP,
*         (20) IT_TOT-GR_ZFAMT CURRENCY P_WAERS
*                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP.
*  NEW-LINE.
*  WRITE AT 1(MAX_LINE) SY-ULINE.
*
*ENDFORM.                    " P3000_SUM_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
  DATA : WL_FNAME(40).
  DATA : WL_NLINE TYPE I.
  WL_NLINE = 2 + CODE_LINE + 1.

  PERFORM P2000_GET_GR_NAME CHANGING WL_FNAME.

  SELECT SINGLE MAKTX INTO IT_TAB-TXZ01
        FROM MAKT
       WHERE MATNR EQ IT_TAB-MATNR
         AND SPRAS EQ SY-LANGU.

  CLEAR : W_FIELD.
  CONCATENATE 'IT_TAB-' W_GROUP INTO W_FIELD.
  ASSIGN (W_FIELD) TO <FS>.

  WRITE : / SY-VLINE NO-GAP.
  WRITE AT 2(CODE_LINE) <FS> NO-GAP.
  WRITE : (01) '' NO-GAP.
  WRITE AT WL_NLINE(TEXT_LINE) WL_FNAME NO-GAP.

  WRITE : SY-VLINE NO-GAP,
         (19) IT_TAB-MATNR          NO-GAP,
         (31) IT_TAB-TXZ01          NO-GAP,  SY-VLINE  NO-GAP,
         (04) IT_TAB-MEINS          NO-GAP,  SY-VLINE  NO-GAP,
         (20) IT_TAB-LC_MENGE  UNIT    IT_TAB-MEINS
                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP,
         (20) IT_TAB-BL_MENGE  UNIT    IT_TAB-MEINS
                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP,
         (20) IT_TAB-IB_MENGE  UNIT    IT_TAB-MEINS
                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP,
         (20) IT_TAB-TR_MENGE  UNIT    IT_TAB-MEINS
                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP,
         (20) IT_TAB-ST_MENGE  UNIT    IT_TAB-MEINS
                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP,
         (20) IT_TAB-GR_MENGE  UNIT    IT_TAB-MEINS
                    RIGHT-JUSTIFIED  NO-GAP, SY-VLINE NO-GAP.
  HIDE IT_TAB.
  NEW-LINE.
  WRITE AT 1(MAX_LINE) SY-ULINE.

  CLEAR : W_FIELD, WL_FNAME, WL_NLINE.
ENDFORM.                               " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_GR_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WL_FNAME  text
*----------------------------------------------------------------------*
FORM P2000_GET_GR_NAME CHANGING P_FNAME.

  CASE 'X'.
    WHEN R_WERKS.
      SELECT SINGLE NAME1 INTO P_FNAME
             FROM T001W
            WHERE WERKS EQ IT_TAB-WERKS.

    WHEN R_EKGRP.
      SELECT SINGLE EKNAM INTO P_FNAME
             FROM T024
            WHERE EKGRP EQ IT_TAB-EKGRP.

    WHEN R_MATKL.
      SELECT SINGLE WGBEZ INTO P_FNAME
             FROM T023T
            WHERE MATKL EQ IT_TAB-MATKL
              AND SPRAS EQ SY-LANGU.

    WHEN R_LIFNR.
      SELECT SINGLE NAME1 INTO P_FNAME
             FROM LFA1
            WHERE LIFNR EQ IT_TAB-LIFNR.

  ENDCASE.

ENDFORM.                               " P2000_GET_GR_NAME
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Detail LIST'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                             " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.
  LEAVE TO LIST-PROCESSING.
  DATA : WL_CNT TYPE I.
  CLEAR : W_FIELD, WL_CNT.
  CONCATENATE 'IT_TAB-' W_GROUP INTO W_FIELD.
  ASSIGN (W_FIELD) TO <FS>.

  CASE W_SELECT.
    WHEN 'LC'.
      W_DT_TITLE = ' Ordering inventory '.
      DETAIL_LINE = 175.
      SORT IT_LCTAB2 BY (W_GROUP) MATNR MEINS EBELN EBELP.
      CLEAR : W_FIELD.
      CONCATENATE 'IT_LCTAB2-' W_GROUP INTO W_FIELD.
      ASSIGN (W_FIELD) TO <FS2>.
      LOOP AT IT_LCTAB2 WHERE MATNR EQ IT_TAB-MATNR
                          AND MEINS EQ IT_TAB-MEINS.
        IF <FS> EQ <FS2>.
          PERFORM P3000_WRITE_LC_DATA.
          ADD 1 TO WL_CNT.
        ENDIF.
      ENDLOOP.

    WHEN 'BL'.
      W_DT_TITLE = '  L/C Open inventory  '.
      DETAIL_LINE = 222.
      SORT IT_OPTAB2 BY (W_GROUP) MATNR MEINS EBELN EBELP.
      CLEAR : W_FIELD.
      CONCATENATE 'IT_OPTAB2-' W_GROUP INTO W_FIELD.
      ASSIGN (W_FIELD) TO <FS2>.
      LOOP AT IT_OPTAB2 WHERE MATNR EQ IT_TAB-MATNR
                          AND MEINS EQ IT_TAB-MEINS.
        IF <FS> EQ <FS2>.
          PERFORM P3000_WRITE_BL_DATA.
          ADD 1 TO WL_CNT.
        ENDIF.
      ENDLOOP.

    WHEN 'IB'.
      W_DT_TITLE = 'Shipment inventory'.
      DETAIL_LINE = 214.
      SORT IT_BLTAB2 BY (W_GROUP) MATNR MEINS EBELN EBELP.
      CLEAR : W_FIELD.
      CONCATENATE 'IT_BLTAB2-' W_GROUP INTO W_FIELD.
      ASSIGN (W_FIELD) TO <FS2>.
      LOOP AT IT_BLTAB2 WHERE MATNR EQ IT_TAB-MATNR
                          AND MEINS EQ IT_TAB-MEINS.
        IF <FS> EQ <FS2>.
          PERFORM P3000_WRITE_IB_DATA.
          ADD 1 TO WL_CNT.
        ENDIF.
      ENDLOOP.

    WHEN 'TR'.
      W_DT_TITLE = ' Bonded transportation inventory '.
      DETAIL_LINE = 183 .
      SORT IT_TRTAB2 BY (W_GROUP) MATNR MEINS EBELN EBELP.
      CLEAR : W_FIELD.
      CONCATENATE 'IT_TRTAB2-' W_GROUP INTO W_FIELD.
      ASSIGN (W_FIELD) TO <FS2>.
      LOOP AT IT_TRTAB2 WHERE MATNR EQ IT_TAB-MATNR
                          AND MEINS EQ IT_TAB-MEINS.
        IF <FS> EQ <FS2>.
          PERFORM P3000_WRITE_TR_DATA.
          ADD 1 TO WL_CNT.
        ENDIF.
      ENDLOOP.

    WHEN 'ST'.
      W_DT_TITLE = ' Warehouse inventory '.
      DETAIL_LINE = 193.
      SORT IT_STTAB2 BY (W_GROUP) MATNR MEINS EBELN EBELP.
      CLEAR : W_FIELD.
      CONCATENATE 'IT_STTAB2-' W_GROUP INTO W_FIELD.
      ASSIGN (W_FIELD) TO <FS2>.
      LOOP AT IT_STTAB2 WHERE MATNR EQ IT_TAB-MATNR
                          AND MEINS EQ IT_TAB-MEINS.
        IF <FS> EQ <FS2>.
          PERFORM P3000_WRITE_ST_DATA.
          ADD 1 TO WL_CNT.
        ENDIF.
      ENDLOOP.

    WHEN 'GR'.
      W_DT_TITLE = 'Not yet G/I inventory after clearance'.
      DETAIL_LINE = 165.
      SORT IT_GRTAB2 BY (W_GROUP) MATNR MEINS EBELN EBELP.
      CLEAR : W_FIELD.
      CONCATENATE 'IT_GRTAB2-' W_GROUP INTO W_FIELD.
      ASSIGN (W_FIELD) TO <FS2>.
      LOOP AT IT_GRTAB2 WHERE MATNR EQ IT_TAB-MATNR
                          AND MEINS EQ IT_TAB-MEINS.
        IF <FS> EQ <FS2>.
          PERFORM P3000_WRITE_GR_DATA.
          ADD 1 TO WL_CNT.
        ENDIF.
      ENDLOOP.
  ENDCASE.

  IF WL_CNT LE 0.
    CLEAR IT_TAB.
    LEAVE TO SCREEN 0.
  ENDIF.

  CLEAR IT_TAB.

ENDMODULE.                             " D0100_LIST_CHECK_SCR0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  P3000_DETAIL_TITLE
*&---------------------------------------------------------------------*
FORM P3000_DETAIL_TITLE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /70  '[ ' NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
               W_DT_TITLE NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
               ' ]' NO-GAP COLOR COL_HEADING INTENSIFIED OFF.

  WRITE : / 'Standard date   : ', P_DATE.
  NEW-LINE.
  WRITE AT 1(DETAIL_LINE) SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,
         (16) 'Purchase Doc-Item'      CENTERED NO-GAP, SY-VLINE NO-GAP,
         (49) 'Materials'              CENTERED NO-GAP, SY-VLINE NO-GAP.
  SET LEFT SCROLL-BOUNDARY.
  WRITE : (20) 'Quantity'              CENTERED NO-GAP, SY-VLINE NO-GAP,
         (24) 'Amount'                 CENTERED NO-GAP, SY-VLINE NO-GAP.

  CASE W_SELECT.
    WHEN 'LC'.
      WRITE : (10) 'P/O Date'          CENTERED NO-GAP, SY-VLINE NO-GAP,
              (40) 'Vendor'            CENTERED NO-GAP, SY-VLINE NO-GAP,
              (08) 'Payment'           CENTERED NO-GAP, SY-VLINE NO-GAP.

    WHEN 'BL'.
      WRITE : (30) 'Vendor'            CENTERED NO-GAP, SY-VLINE NO-GAP,
              (25) 'L/C No'            CENTERED NO-GAP, SY-VLINE NO-GAP,
              (10) 'Opening date'      CENTERED NO-GAP, SY-VLINE NO-GAP,
              (30) 'Opening bank'      CENTERED NO-GAP, SY-VLINE NO-GAP,
              (08) 'Payment'           CENTERED NO-GAP, SY-VLINE NO-GAP.

    WHEN 'IB'.
      WRITE : (24) 'House B/L No.'     CENTERED NO-GAP, SY-VLINE NO-GAP,
              (10) 'B/L created on'    CENTERED NO-GAP, SY-VLINE NO-GAP,
              (10) 'Shipment'          CENTERED NO-GAP, SY-VLINE NO-GAP,
              (10) 'Arrival'           CENTERED NO-GAP, SY-VLINE NO-GAP,
              (20) 'Shipment port'     CENTERED NO-GAP, SY-VLINE NO-GAP,
              (20) 'Destination port'  CENTERED NO-GAP, SY-VLINE NO-GAP.

    WHEN 'TR'.
      WRITE : (24) 'House B/L No.'     CENTERED NO-GAP, SY-VLINE NO-GAP,
              (10) 'Actual entry date' CENTERED NO-GAP, SY-VLINE NO-GAP,
              (20) 'Entry'             CENTERED NO-GAP, SY-VLINE NO-GAP,
              (11) 'Container No'      CENTERED NO-GAP, SY-VLINE NO-GAP.
      WRITE : / SY-VLINE NO-GAP,
              (16) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (49) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (20) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (24) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (24) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (10) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (20) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (05) '20fit'             CENTERED NO-GAP, SY-VLINE NO-GAP,
              (05) '40fit'             CENTERED NO-GAP, SY-VLINE NO-GAP.

    WHEN 'ST'.
      WRITE : (24) 'House B/L No.'     CENTERED NO-GAP, SY-VLINE NO-GAP,
              (10) 'Carry-in'          CENTERED NO-GAP, SY-VLINE NO-GAP,
              (30) 'Bonded area name'  CENTERED NO-GAP, SY-VLINE NO-GAP,
              (11) 'Container No'      CENTERED NO-GAP, SY-VLINE NO-GAP.
      WRITE : / SY-VLINE NO-GAP,
              (16) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (49) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (20) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (24) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (24) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (10) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (30) ' '                 CENTERED NO-GAP, SY-VLINE NO-GAP,
              (05) '20fit'             CENTERED NO-GAP, SY-VLINE NO-GAP,
              (05) '40fit'             CENTERED NO-GAP, SY-VLINE NO-GAP.

    WHEN 'GR'.
      WRITE : (24) 'House B/L No.'     CENTERED NO-GAP, SY-VLINE NO-GAP,
              (10) 'Clearance date'    CENTERED NO-GAP, SY-VLINE NO-GAP,
              (14) 'Imp license No'    CENTERED NO-GAP, SY-VLINE NO-GAP.
  ENDCASE.
  NEW-LINE.
  WRITE AT 1(DETAIL_LINE) SY-ULINE.
  FORMAT RESET.

ENDFORM.                               " P3000_DETAIL_TITLE

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_LC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_LC_DATA.
  WRITE : / SY-VLINE NO-GAP,
         (10) IT_LCTAB2-EBELN  NO-GAP,  '-' NO-GAP,
         (05) IT_LCTAB2-EBELP  NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_LCTAB2-MATNR  NO-GAP,
         (30) IT_LCTAB2-TXZ01  NO-GAP,  SY-VLINE NO-GAP,
         (17) IT_LCTAB2-MENGE  UNIT IT_LCTAB2-MEINS
                    RIGHT-JUSTIFIED NO-GAP,
         (03) IT_LCTAB2-MEINS  NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_LCTAB2-ZFAMT  CURRENCY IT_LCTAB2-WAERS
                     RIGHT-JUSTIFIED NO-GAP,
         (05) IT_LCTAB2-WAERS  NO-GAP,  SY-VLINE NO-GAP,
         (10) IT_LCTAB2-AEDAT  NO-GAP,  SY-VLINE NO-GAP.

*> Vendor Name.
  CLEAR V_VENDNM.
  SELECT SINGLE NAME1 INTO V_VENDNM
                FROM  LFA1
               WHERE  LIFNR   EQ  IT_LCTAB2-LIFNR.

  WRITE : (11) IT_LCTAB2-LIFNR  NO-GAP,
          (29) V_VENDNM         NO-GAP,  SY-VLINE NO-GAP,
          (08) IT_LCTAB2-ZTERM  CENTERED NO-GAP,  SY-VLINE NO-GAP.

  HIDE IT_LCTAB2.
  NEW-LINE.
  WRITE AT 1(DETAIL_LINE) SY-ULINE.

ENDFORM.                               " P3000_WRITE_LC_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_BL_DATA.
  WRITE : / SY-VLINE NO-GAP,
         (10) IT_OPTAB2-EBELN  NO-GAP,  '-' NO-GAP,
         (05) IT_OPTAB2-EBELP  NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_OPTAB2-MATNR  NO-GAP,
         (30) IT_OPTAB2-TXZ01  NO-GAP,  SY-VLINE NO-GAP,
         (17) IT_OPTAB2-MENGE  UNIT IT_OPTAB2-MEINS
                    RIGHT-JUSTIFIED NO-GAP,
         (03) IT_OPTAB2-MEINS  NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_OPTAB2-ZFAMT  CURRENCY IT_OPTAB2-WAERS
                     RIGHT-JUSTIFIED NO-GAP,
         (05) IT_OPTAB2-WAERS  NO-GAP,  SY-VLINE NO-GAP.

*> Vendor Name.
  CLEAR V_VENDNM.
  SELECT SINGLE NAME1 INTO V_VENDNM
                FROM  LFA1
               WHERE  LIFNR   EQ  IT_OPTAB2-LIFNR.

  WRITE : (11) IT_OPTAB2-LIFNR  NO-GAP,
          (19) V_VENDNM         NO-GAP,  SY-VLINE NO-GAP,
          (25) IT_OPTAB2-ZFOPNNO NO-GAP, SY-VLINE NO-GAP,
          (10) IT_OPTAB2-ZFOPNDT NO-GAP, SY-VLINE NO-GAP.
*> Open Bank Name.
  CLEAR V_VENDNM.
  SELECT SINGLE NAME1 INTO V_VENDNM
                FROM  LFA1
               WHERE  LIFNR   EQ  IT_OPTAB2-ZFOPBN.

  WRITE : (11) IT_OPTAB2-ZFOPBN NO-GAP,
          (19) V_VENDNM         NO-GAP,  SY-VLINE NO-GAP,
          (08) IT_OPTAB2-ZTERM  CENTERED NO-GAP,  SY-VLINE NO-GAP.

  HIDE IT_OPTAB2.
  NEW-LINE.
  WRITE AT 1(DETAIL_LINE) SY-ULINE.

ENDFORM.                               " P3000_WRITE_BL_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IB_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_IB_DATA.
  WRITE : / SY-VLINE NO-GAP,
         (10) IT_BLTAB2-EBELN   NO-GAP,  '-' NO-GAP,
         (05) IT_BLTAB2-EBELP   NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_BLTAB2-MATNR   NO-GAP,
         (30) IT_BLTAB2-TXZ01   NO-GAP,  SY-VLINE NO-GAP,
         (17) IT_BLTAB2-BLMENGE  UNIT IT_BLTAB2-MEINS
                    RIGHT-JUSTIFIED NO-GAP,
         (03) IT_BLTAB2-MEINS   NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_BLTAB2-ZFAMT  CURRENCY IT_BLTAB2-WAERS
                     RIGHT-JUSTIFIED NO-GAP,
         (05) IT_BLTAB2-WAERS   NO-GAP,  SY-VLINE NO-GAP,
         (24) IT_BLTAB2-ZFHBLNO NO-GAP,  SY-VLINE NO-GAP,
         (10) IT_BLTAB2-CDAT    NO-GAP,  SY-VLINE NO-GAP,
         (10) IT_BLTAB2-ZFETD   NO-GAP,  SY-VLINE NO-GAP,
         (10) IT_BLTAB2-ZFETA   NO-GAP,  SY-VLINE NO-GAP,
         (20) IT_BLTAB2-ZFSPRT  NO-GAP,  SY-VLINE NO-GAP,
         (20) IT_BLTAB2-ZFAPRT  NO-GAP,  SY-VLINE NO-GAP.

  HIDE IT_BLTAB2.
  NEW-LINE.
  WRITE AT 1(DETAIL_LINE) SY-ULINE.


ENDFORM.                               " P3000_WRITE_IB_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_TR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_TR_DATA.
  WRITE : / SY-VLINE NO-GAP,
         (10) IT_TRTAB2-EBELN    NO-GAP,  '-' NO-GAP,
         (05) IT_TRTAB2-EBELP    NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_TRTAB2-MATNR    NO-GAP,
         (30) IT_TRTAB2-TXZ01    NO-GAP,  SY-VLINE NO-GAP,
         (17) IT_TRTAB2-BLMENGE  UNIT IT_TRTAB2-MEINS
                    RIGHT-JUSTIFIED NO-GAP,
         (03) IT_TRTAB2-MEINS    NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_TRTAB2-ZFAMT  CURRENCY IT_TRTAB2-WAERS
                     RIGHT-JUSTIFIED NO-GAP,
         (05) IT_TRTAB2-WAERS    NO-GAP,  SY-VLINE NO-GAP,
         (24) IT_TRTAB2-ZFHBLNO  NO-GAP,  SY-VLINE NO-GAP,
         (10) IT_TRTAB2-ZFRETA   NO-GAP,  SY-VLINE NO-GAP,
         (20) IT_TRTAB2-ZFAPRT   NO-GAP,  SY-VLINE NO-GAP,
         (05) IT_TRTAB2-ZF20FT   RIGHT-JUSTIFIED NO-GAP NO-ZERO,
                                          SY-VLINE NO-GAP,
         (05) IT_TRTAB2-ZF40FT   RIGHT-JUSTIFIED NO-GAP NO-ZERO,
                                          SY-VLINE NO-GAP.
  HIDE IT_TRTAB2.
  NEW-LINE.
  WRITE AT 1(DETAIL_LINE) SY-ULINE.

ENDFORM.                               " P3000_WRITE_TR_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_ST_DATA.
  WRITE : / SY-VLINE NO-GAP,
         (10) IT_STTAB2-EBELN    NO-GAP,  '-' NO-GAP,
         (05) IT_STTAB2-EBELP    NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_STTAB2-MATNR    NO-GAP,
         (30) IT_STTAB2-TXZ01    NO-GAP,  SY-VLINE NO-GAP,
         (17) IT_STTAB2-BLMENGE  UNIT IT_STTAB2-MEINS
                    RIGHT-JUSTIFIED NO-GAP,
         (03) IT_STTAB2-MEINS    NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_STTAB2-ZFAMT  CURRENCY IT_STTAB2-WAERS
                     RIGHT-JUSTIFIED NO-GAP,
         (05) IT_STTAB2-WAERS    NO-GAP,  SY-VLINE NO-GAP,
         (24) IT_STTAB2-ZFHBLNO  NO-GAP,  SY-VLINE NO-GAP,
         (10) IT_STTAB2-ZFINDT   NO-GAP,  SY-VLINE NO-GAP,
         (30) IT_STTAB2-ZFBNARM  NO-GAP,  SY-VLINE NO-GAP,
         (05) IT_STTAB2-ZF20FT   RIGHT-JUSTIFIED NO-GAP NO-ZERO,
                                          SY-VLINE NO-GAP,
         (05) IT_STTAB2-ZF40FT   RIGHT-JUSTIFIED NO-GAP NO-ZERO,
                                          SY-VLINE NO-GAP.

  HIDE IT_STTAB2.
  NEW-LINE.
  WRITE AT 1(DETAIL_LINE) SY-ULINE.

ENDFORM.                               " P3000_WRITE_ST_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_GR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_GR_DATA.
  WRITE : / SY-VLINE NO-GAP,
         (10) IT_GRTAB2-EBELN    NO-GAP,  '-' NO-GAP,
         (05) IT_GRTAB2-EBELP    NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_GRTAB2-MATNR    NO-GAP,
         (30) IT_GRTAB2-TXZ01    NO-GAP,  SY-VLINE NO-GAP,
         (17) IT_GRTAB2-CCMENGE  UNIT IT_GRTAB2-MEINS
                    RIGHT-JUSTIFIED NO-GAP,
         (03) IT_GRTAB2-MEINS    NO-GAP,  SY-VLINE NO-GAP,
         (19) IT_GRTAB2-ZFAMT  CURRENCY IT_GRTAB2-WAERS
                     RIGHT-JUSTIFIED NO-GAP,
         (05) IT_GRTAB2-WAERS    NO-GAP,  SY-VLINE NO-GAP,
         (24) IT_GRTAB2-ZFHBLNO  NO-GAP,  SY-VLINE NO-GAP,
         (10) IT_GRTAB2-ZFEDT    NO-GAP,  SY-VLINE NO-GAP,
         (14) IT_GRTAB2-ZFENTNO  NO-GAP,  SY-VLINE NO-GAP.

  HIDE IT_GRTAB2.
  NEW-LINE.
  WRITE AT 1(DETAIL_LINE) SY-ULINE.

ENDFORM.                               " P3000_WRITE_GR_DATA
*&---------------------------------------------------------------------
*&      Form  P3000_TO_PC_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P3000_TO_PC_DOWNLOAD.

  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      FILENAME = 'C:\TEMP.xls'
      FILETYPE = 'WK1'
    TABLES
      DATA_TAB = IT_TAB.

ENDFORM.                               " P3000_TO_PC_DOWNLOAD
