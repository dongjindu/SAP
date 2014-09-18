*&---------------------------------------------------------------------*
*& Report  ZRIMCSTLST                                                  *
*&---------------------------------------------------------------------*
*&  Program : Material-in-transit List                                 *
*&     Name : Na Hyun-Ju INFOLINK Ltd.                                 *
*&     Date : 2003.11.24                                               *
*&---------------------------------------------------------------------*
*&   DESC.  : Plan Amount, Actual Amount Comparison                    *
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMCSTLST    MESSAGE-ID ZIM
                      LINE-SIZE 211
                      NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE : <ICON>,
          ZRIMCSTLSTTOP.

*-----------------------------------------------------------------------
* Write Internal Table Declare
*-----------------------------------------------------------------------
DATA : BEGIN OF   IT_TAB OCCURS 0,
       EBELN      LIKE  EKBZ-EBELN,          " Purchasing Document.
       ZFOPNNO    LIKE  ZTREQHD-ZFOPNNO,     " L/C No
       WAERS      LIKE  ZTREQHD-WAERS,       " Currency
       PLAN_AMT   LIKE  EKBZ-DMBTR,          " Planned Amount
       ACT_AMT    LIKE  EKBZ-DMBTR,          " Atcuatl Amount
       BLAN_AMT   LIKE  EKBZ-DMBTR.          " Balance Amount
DATA : END OF IT_TAB.

DATA : BEGIN OF   IT_TEMP OCCURS 0,
       EBELN      LIKE  EKBZ-EBELN,          " Purchasing Document.
       EBELP      LIKE  EKBZ-EBELP,          " Purchasing Document Item
       MATNR      LIKE  EKPO-MATNR,          " Material No.
       TXZ01      LIKE  EKPO-TXZ01,          " Material Text.
       PLAC_GB    TYPE  C,                   " Plan/Actual Sort
       GP_FO      LIKE  EKBE-WRBTR,          " Foreign Amount
       WAERS      LIKE  EKBE-WAERS,          " Foreign Currency
       GP_LO      LIKE  EKBE-DMBTR,          " Local Goods Price Amount
       HSWAE      LIKE  EKBE-HSWAE,          " Foreign Currency
       TARIF      LIKE  EKBE-DMBTR,          " Tariff
       FREIGHT    LIKE  EKBE-DMBTR,          " Freight
       LC_AMT     LIKE  EKBE-DMBTR,          " L/C Expense
       BL_AMT     LIKE  EKBE-DMBTR,          " B/L Expense
       GR_QTY     LIKE  EKBE-MENGE,          " Quantity
       MEINS      LIKE  EKPO-MEINS,          " Unit of Measure
       PO_QTY     LIKE  EKBE-MENGE.          " P/O Quantity
DATA : END OF IT_TEMP.

*-----------------------------------------------------------------------
* Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS    FOR ZTREQHD-BUKRS NO-EXTENSION
                                             NO INTERVALS,
                S_EBELN    FOR ZTREQIT-EBELN,   " P/O
                S_EBELP    FOR ZTREQIT-EBELP,   " P/O-ITEM
                S_AEDAT    FOR EKKO-AEDAT,      " Created on
                S_MATNR    FOR ZTREQIT-MATNR,   " Material
                S_REQTY    FOR ZTREQHD-ZFREQTY, " Import Type
                S_LIFNR    FOR ZTREQHD-LIFNR,   " VENDOR
                S_EKORG    FOR EKKO-EKORG,      " Purchasing Organizatio
                S_EKGRP    FOR EKKO-EKGRP,      " Purchasing Group
                S_WERKS    FOR ZTREQHD-ZFWERKS. " Plant
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM  P1000_SET_BUKRS.
  SET  TITLEBAR  'ZIMY13'.

* Title Text Write
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU'.
    PERFORM P1000_TITLE_WRITE.
  ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

*>> Import CONFIGURATION CHECK
  PERFORM P1000_CONFIG_CHECK   USING  W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.   EXIT. ENDIF.

*>> DATA SELECT!
  PERFORM P2000_READ_TEXT      USING  W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.  EXIT.  ENDIF.

*-----------------------------------------------------------------------
* END OF SELECTION
*-----------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM P3000_DATA_WRITE     USING  W_ERR_CHK.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'DIPO'.
      SET PARAMETER ID 'BES' FIELD IT_TAB-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.

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
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.

  IF IT_TAB-EBELN IS INITIAL.
     EXIT.
  ENDIF.
  INCLUDE = 'POPU'.
  CALL SCREEN 0100 STARTING AT  5     3
                   ENDING   AT  210   20.
  CLEAR : INCLUDE.

*&---------------------------------------------------------------------*
*&      Form  P1000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P1000_CONFIG_CHECK USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.

* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.

* Not Found
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  IF ZTIMIMG00-ZFCSTMD NE 'S' AND ZTIMIMG00-ZFCSTMD NE 'P'.
     W_ERR_CHK = 'Y'.   MESSAGE S573.   EXIT.
  ENDIF.

ENDFORM.                    " P1000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P2000_READ_TEXT USING    W_ERR_CHK.

  REFRESH : IT_TAB, IT_PO, IT_REQ, IT_DIV, IT_EKBZ.
  MOVE  'N'      TO  W_ERR_CHK.

*>> PO Number Get!
  PERFORM P3000_READ_REDATA.
  IF W_ERR_CHK EQ 'Y'.
    MESSAGE  S738.
    EXIT.
  ENDIF.

*>> Related cost data GET!
  PERFORM P3000_READ_CSTDATA.

*>> P/O No Group.
  PERFORM P3000_PO_GROUP.

*>> LIST UP INTERNAL TEMP TABLE INSERT
  PERFORM P3000_WRITE_TAB.

*>> WRITE INTERNAL TABLE INSERT.
  PERFORM P3000_WRITE_IT_TAB.

ENDFORM.                    " P2000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_READ_REDATA
*&---------------------------------------------------------------------*
FORM P3000_READ_REDATA.

*>> Import request PO data SELECT!
  SELECT  B~EBELN  B~EBELP
  INTO    CORRESPONDING FIELDS OF TABLE IT_REQ
  FROM    ZTREQHD  AS  A  INNER  JOIN  ZTREQIT  AS  B
  ON      A~ZFREQNO    EQ   B~ZFREQNO
  WHERE   B~EBELN      IN   S_EBELN
  AND     A~BUKRS      IN   S_BUKRS
  AND     B~EBELP      IN   S_EBELP
  AND     B~MATNR      IN   S_MATNR
  AND     A~ZFREQTY    IN   S_REQTY
  AND     A~LIFNR      IN   S_LIFNR.

  CLEAR  W_LINE.
  DESCRIBE TABLE IT_REQ LINES W_LINE.
  IF W_LINE EQ 0.
    MOVE  'Y'   TO  W_ERR_CHK.
    EXIT.
  ENDIF.

*>>  Accurate DATA SELECT using import request PO
  SELECT  B~EBELN   B~EBELP  B~MEINS  A~BUKRS  B~WERKS  B~MENGE
          B~MATNR   B~TXZ01
  INTO    CORRESPONDING FIELDS OF TABLE IT_PO
  FROM    EKKO   AS  A  INNER  JOIN  EKPO  AS  B
  ON      A~EBELN       EQ     B~EBELN
  FOR     ALL  ENTRIES  IN     IT_REQ
  WHERE   B~EBELN       EQ     IT_REQ-EBELN
  AND     B~EBELP       EQ     IT_REQ-EBELP
  AND     A~EKORG       IN     S_EKORG
  AND     A~EKGRP       IN     S_EKGRP
  AND     B~WERKS       IN     S_WERKS
  AND     A~AEDAT       IN     S_AEDAT.

ENDFORM.                    " P3000_READ_REDATA
*&---------------------------------------------------------------------*
*&      Form  P1000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_TITLE_WRITE.

  IF SY-LANGU EQ '3'.
  ELSE.
    SKIP 2.
    FORMAT RESET.
    FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
    WRITE : /35  '[ Import Expense Balance Sheet ]'
                 COLOR COL_HEADING INTENSIFIED OFF.
    WRITE : / 'Date : ', SY-DATUM.
    WRITE : /(102) SY-ULINE.
    FORMAT RESET.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE : /                                          SY-VLINE NO-GAP,
           (14) 'Purchase Order'               NO-GAP, SY-VLINE NO-GAP,
           (25) 'L/C Approve No'               NO-GAP, SY-VLINE NO-GAP,
           (08) 'Currency'                     NO-GAP, SY-VLINE NO-GAP,
           (16) 'Actual Amount'                NO-GAP, SY-VLINE NO-GAP,
           (16) 'Planned Amount'               NO-GAP, SY-VLINE NO-GAP,
           (16) 'Balance'                      NO-GAP, SY-VLINE NO-GAP.
    WRITE : /(102) SY-ULINE NO-GAP.
  ENDIF.

ENDFORM.                    " P1000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_READ_CSTDATA
*&---------------------------------------------------------------------*
FORM P3000_READ_CSTDATA.

*>> Import Expense (Actual Amount)
  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_DIV
  FROM    ZTBKPF       AS  A  INNER  JOIN  ZTBDIV  AS  B
  ON      A~BUKRS      EQ  B~BUKRS
  AND     A~GJAHR      EQ  B~GJAHR
  AND     A~BELNR      EQ  B~BELNR
  FOR     ALL ENTRIES  IN  IT_PO
  WHERE   B~EBELN      EQ  IT_PO-EBELN
  AND     B~EBELP      EQ  IT_PO-EBELP
  AND     B~COND_TYPE  NE  'ZOA1'
  AND     B~COND_TYPE  NE  'FRA1'
  AND     A~ZFPOSYN    EQ  'Y'.

*>> Import Request Expense POSTING( LIV ) Data SELECT!
  DESCRIBE TABLE IT_DIV LINES W_LINE.
  IF W_LINE GT  0.
    SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_EKBZ
    FROM    EKBZ
    FOR     ALL ENTRIES  IN  IT_DIV
    WHERE   EBELN        EQ  IT_DIV-EBELN
    AND     EBELP        EQ  IT_DIV-EBELP.
  ELSE.
    SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_EKBZ
    FROM    EKBZ
    FOR     ALL ENTRIES  IN  IT_PO
    WHERE   EBELN        EQ  IT_PO-EBELN
    AND     EBELP        EQ  IT_PO-EBELP.
  ENDIF.

*>> Goods Price POSTING( LIV ) Data SELECT!
  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_EKBE
  FROM    EKBE
  FOR     ALL ENTRIES  IN  IT_PO
  WHERE   EBELN        EQ  IT_PO-EBELN
  AND     EBELP        EQ  IT_PO-EBELP
  AND   ( BEWTP        EQ  'E'
  OR      BEWTP        EQ  'Q'
  OR      BEWTP        EQ  'R'
  OR      BEWTP        EQ  'N' ).

ENDFORM.                    " P3000_READ_CSTDATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_TAB
*&---------------------------------------------------------------------*
FORM P3000_WRITE_TAB.

  REFRESH : IT_TEMP.

  LOOP AT IT_PO.

     CLEAR : EKKO, T001, W_GR_QTY, IT_TEMP.
     SELECT SINGLE * FROM EKKO WHERE EBELN EQ IT_PO-EBELN.
     IF SY-TABIX EQ 1.
        SELECT SINGLE * FROM T001 WHERE BUKRS EQ IT_PO-BUKRS.
        MOVE  T001-WAERS  TO W_CURRENCY.
     ENDIF.

     "---------------------------------------------
     " Plan Amount Set.
     "---------------------------------------------
     MOVE : W_CURRENCY     TO  IT_TEMP-HSWAE,
            EKKO-WAERS     TO  IT_TEMP-WAERS,
            IT_PO-MATNR    TO  IT_TEMP-MATNR,
            IT_PO-TXZ01    TO  IT_TEMP-TXZ01,
            IT_PO-EBELN    TO  IT_TEMP-EBELN,
            IT_PO-EBELP    TO  IT_TEMP-EBELP,
            IT_PO-MEINS    TO  IT_TEMP-MEINS,
            IT_PO-MENGE    TO  IT_TEMP-PO_QTY,
            '1'            TO  IT_TEMP-PLAC_GB.

     " Goods Price( Local, Foreign ) Set.
     LOOP AT IT_EKBE  WHERE EBELN EQ IT_PO-EBELN
                      AND   EBELP EQ IT_PO-EBELP
                      AND   BEWTP EQ 'E'
                      AND   VGABE EQ '1'.
        IF IT_EKBE-SHKZG EQ 'H'.
           IT_EKBE-MENGE  =  IT_EKBE-MENGE * -1.
           IT_EKBE-DMBTR  =  IT_EKBE-DMBTR * -1.
           IT_EKBE-WRBTR  =  IT_EKBE-WRBTR * -1.
        ENDIF.

        IT_TEMP-GR_QTY  =  IT_TEMP-GR_QTY + IT_EKBE-MENGE.
        IT_TEMP-GP_FO   =  IT_TEMP-GP_FO  + IT_EKBE-WRBTR.
        IT_TEMP-GP_LO   =  IT_TEMP-GP_LO  + IT_EKBE-DMBTR.
        W_GR_QTY        =  IT_TEMP-GR_QTY.

     ENDLOOP.

     " Tariff Set.
     LOOP AT IT_EKBZ  WHERE EBELN EQ IT_PO-EBELN
                      AND   EBELP EQ IT_PO-EBELP
                      AND   VGABE EQ '1'.
        IF IT_EKBZ-SHKZG NE 'H'.
           IT_EKBZ-DMBTR   =  IT_EKBZ-DMBTR * -1.
        ENDIF.

        CASE IT_EKBZ-KSCHL.
           WHEN 'ZOA1'.
              IT_TEMP-TARIF  =  IT_TEMP-TARIF   + IT_EKBZ-DMBTR.
           WHEN 'FRA1'.
              IT_TEMP-FREIGHT = IT_TEMP-FREIGHT + IT_EKBZ-DMBTR.
           WHEN 'ZOTH'.
              IT_TEMP-LC_AMT  = IT_TEMP-LC_AMT  + IT_EKBZ-DMBTR.
           WHEN 'ZOTI'.
              IT_TEMP-BL_AMT  = IT_TEMP-BL_AMT  + IT_EKBZ-DMBTR.
        ENDCASE.

     ENDLOOP.
     APPEND  IT_TEMP.

     "---------------------------------------------
     " Actual Amount Set.
     "---------------------------------------------
     CLEAR : IT_TEMP.
     MOVE : W_CURRENCY     TO  IT_TEMP-HSWAE,
            EKKO-WAERS     TO  IT_TEMP-WAERS,
            IT_PO-EBELN    TO  IT_TEMP-EBELN,
            IT_PO-EBELP    TO  IT_TEMP-EBELP,
            IT_PO-MATNR    TO  IT_TEMP-MATNR,
            IT_PO-TXZ01    TO  IT_TEMP-TXZ01,
            IT_PO-MEINS    TO  IT_TEMP-MEINS,
            IT_PO-MENGE    TO  IT_TEMP-PO_QTY,
            '2'            TO  IT_TEMP-PLAC_GB,
            W_GR_QTY       TO  IT_TEMP-GR_QTY.

     " Goods Price( Local, Foreign ) Set.
     LOOP AT IT_EKBE  WHERE EBELN EQ IT_PO-EBELN
                      AND   EBELP EQ IT_PO-EBELP
                      AND   VGABE NE '1'.
        IF IT_EKBE-SHKZG EQ 'H'.
           IT_EKBE-DMBTR  =  IT_EKBE-DMBTR * -1.
           IT_EKBE-WRBTR  =  IT_EKBE-WRBTR * -1.
           IT_EKBE-MENGE  =  IT_EKBE-MENGE * -1.
        ENDIF.

        IT_TEMP-GP_FO   =  IT_TEMP-GP_FO  + IT_EKBE-WRBTR.
        IT_TEMP-GP_LO   =  IT_TEMP-GP_LO  + IT_EKBE-DMBTR.

     ENDLOOP.

     " Delivery Cost Amount Set
     LOOP AT IT_EKBZ  WHERE EBELN EQ IT_PO-EBELN
                      AND   EBELP EQ IT_PO-EBELP
                      AND   VGABE NE '1'.
        IF IT_EKBZ-SHKZG EQ 'H'.
           IT_EKBZ-DMBTR   =  IT_EKBZ-DMBTR + -1.
        ENDIF.

        CASE IT_EKBZ-KSCHL.
           WHEN 'ZOA1'.
              IT_TEMP-TARIF  =  IT_TEMP-TARIF   + IT_EKBZ-DMBTR.
           WHEN 'FRA1'.
              IT_TEMP-FREIGHT = IT_TEMP-FREIGHT + IT_EKBZ-DMBTR.
        ENDCASE.

     ENDLOOP.

     " Non-Delivery Cost Amount Set
     LOOP AT IT_DIV  WHERE  EBELN  EQ  IT_PO-EBELN
                     AND    EBELP  EQ  IT_PO-EBELP.

       IF IT_DIV-ZFRVSX EQ 'X'.
          IT_DIV-DMBTR = IT_DIV-DMBTR * -1.
       ENDIF.

       IF IT_DIV-ZFCSTGRP EQ '003'.
          IT_TEMP-LC_AMT  =  IT_TEMP-LC_AMT  +  IT_DIV-DMBTR.
       ELSE.
          IT_TEMP-BL_AMT  =  IT_TEMP-BL_AMT  +  IT_DIV-DMBTR.
       ENDIF.

     ENDLOOP.

     APPEND  IT_TEMP.

  ENDLOOP.

  SORT  IT_TEMP  BY  EBELN  EBELN.

  DESCRIBE  TABLE  IT_TEMP  LINES  W_COUNT.
  IF W_COUNT  LE  0.
     MESSAGE  S966.
     W_ERR_CHK  =  'N'.
  ENDIF.

ENDFORM.                    " P3000_WRITE_TAB
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.

  MOVE  'N'    TO  W_ERR_CHK.
  SET  PF-STATUS 'ZIMY13'.         " GUI STATUS SETTING
  SET  TITLEBAR  'ZIMY13'.         " GUI TITLE SETTING..

  LOOP  AT  IT_TAB.

    W_LINE  =  W_LINE + 1.
    W_MOD   =  W_LINE MOD 2.

    " Line Write.
    PERFORM   P3000_LINE_WRITE.

    " Total Amount.
    W_TOT_PLAN  =  W_TOT_PLAN +  IT_TAB-PLAN_AMT.
    W_TOT_ACT   =  W_TOT_ACT  +  IT_TAB-ACT_AMT.
    W_TOT_BALA  =  W_TOT_BALA +  IT_TAB-BLAN_AMT.

    AT LAST.
      PERFORM P3000_SUM_WRITE.
      WRITE : /(102) SY-ULINE NO-GAP.
    ENDAT.

  ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  IF W_MOD EQ 0.
     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE : /                                           SY-VLINE NO-GAP,
         (14) IT_TAB-EBELN    CENTERED        NO-GAP, SY-VLINE NO-GAP,
         (25) IT_TAB-ZFOPNNO                  NO-GAP, SY-VLINE NO-GAP,
         (08) IT_TAB-WAERS    CENTERED        NO-GAP, SY-VLINE NO-GAP,
         (16) IT_TAB-ACT_AMT  RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
         (16) IT_TAB-PLAN_AMT RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
         (16) IT_TAB-BLAN_AMT RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP.

  HIDE  IT_TAB.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
    WHEN 'POPU'.
       SET TITLEBAR 'POPU' WITH 'Import Expense Balance Detail Sheet'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.

  LEAVE TO LIST-PROCESSING.
  CASE INCLUDE.
    WHEN 'POPU'.
      PERFORM  P1000_WRITE_CST_SCR0100.
  ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_CST_SCR0100
*&---------------------------------------------------------------------*
FORM P1000_WRITE_CST_SCR0100.

  PERFORM  P1000_WRITE_TITLE_SCR0100.

  SORT IT_TEMP BY EBELN EBELP PLAC_GB.

  LOOP  AT  IT_TEMP  WHERE  EBELN EQ IT_TAB-EBELN.

     IF W_EBELP NE IT_TEMP-EBELP.
        W_CHECK = 'X'.
        WRITE : / SY-ULINE NO-GAP.
     ENDIF.

     " Line Write.
     PERFORM P2000_LINE_WRITE_DATA_SCR0100.

     MOVE IT_TEMP-EBELP  TO  W_EBELP.
     CLEAR : W_CHECK.

  ENDLOOP.

  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P1000_WRITE_CST_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_TITLE_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_WRITE_TITLE_SCR0100.

  WRITE: / 'L/C Approve No : '   NO-GAP,
           IT_TAB-ZFOPNNO        NO-GAP.

  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : /                                          SY-VLINE NO-GAP,
         (16) 'Purchase Order'               NO-GAP, SY-VLINE NO-GAP,
         (40) 'Material'                     NO-GAP, SY-VLINE NO-GAP,
         (06) 'R. Type'                      NO-GAP, SY-VLINE NO-GAP,
         (21) 'Goods Price(Foreign)'         NO-GAP, SY-VLINE NO-GAP,
         (21) 'Goods Price(Local)'           NO-GAP, SY-VLINE NO-GAP,
         (16) 'Tariff'                       NO-GAP, SY-VLINE NO-GAP,
         (16) 'Freight'                      NO-GAP, SY-VLINE NO-GAP,
         (16) 'L/C Ohter'                    NO-GAP, SY-VLINE NO-GAP,
         (16) 'B/L Other'                    NO-GAP, SY-VLINE NO-GAP,
         (18) 'P/O Quantity'                 NO-GAP, SY-VLINE NO-GAP,
         (13) 'G/R Quantity'                 NO-GAP, SY-VLINE NO-GAP.

ENDFORM.                    " P1000_WRITE_TITLE_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : ZTIMIMG00, P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

*>> Company Code SET.
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IT_TAB
*&---------------------------------------------------------------------*
FORM P3000_WRITE_IT_TAB.

   REFRESH : IT_TAB.
   SORT  IT_PO_TMP  BY  EBELN.

   LOOP AT IT_PO_TMP.

      CLEAR : ZTREQHD, T001, IT_TAB.
      SELECT SINGLE * FROM ZTREQHD WHERE EBELN EQ IT_PO_TMP-EBELN.

      MOVE : IT_PO_TMP-EBELN  TO  IT_TAB-EBELN,
             ZTREQHD-ZFOPNNO  TO  IT_TAB-ZFOPNNO,
             W_CURRENCY       TO  IT_TAB-WAERS.

      " Planned Amount Sum.
      LOOP AT IT_TEMP  WHERE  EBELN    EQ IT_PO_TMP-EBELN
                       AND    PLAC_GB  EQ '1'.
         IT_TAB-PLAN_AMT  =  IT_TAB-PLAN_AMT  +  IT_TEMP-GP_LO    +
                             IT_TEMP-TARIF    +  IT_TEMP-FREIGHT  +
                             IT_TEMP-LC_AMT   +  IT_TEMP-BL_AMT.
      ENDLOOP.

      " Actual Amount Sum.
      LOOP AT IT_TEMP  WHERE  EBELN    EQ IT_PO_TMP-EBELN
                       AND    PLAC_GB  EQ '2'.
         IT_TAB-ACT_AMT   =  IT_TAB-ACT_AMT   +  IT_TEMP-GP_LO    +
                             IT_TEMP-TARIF    +  IT_TEMP-FREIGHT  +
                             IT_TEMP-LC_AMT   +  IT_TEMP-BL_AMT.
      ENDLOOP.

      " Balance Amount.
      IT_TAB-BLAN_AMT  =  IT_TAB-ACT_AMT  -  IT_TAB-PLAN_AMT.

      IF IT_TAB-ACT_AMT EQ 0 AND IT_TAB-PLAN_AMT EQ 0.
         CONTINUE.
      ENDIF.

      APPEND  IT_TAB.
   ENDLOOP.

ENDFORM.                    " P3000_WRITE_IT_TAB
*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_WRITE
*&---------------------------------------------------------------------*
FORM P3000_SUM_WRITE.

  WRITE : /(102) SY-ULINE NO-GAP.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED ON.

  WRITE : /                                           SY-VLINE NO-GAP,
         (40) 'Total'         CENTERED        NO-GAP, SY-VLINE NO-GAP,
         (08) W_CURRENCY      CENTERED        NO-GAP, SY-VLINE NO-GAP,
         (16) W_TOT_ACT       RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
         (16) W_TOT_PLAN      RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
         (16) W_TOT_BALA      RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP.

ENDFORM.                    " P3000_SUM_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_PO_GROUP
*&---------------------------------------------------------------------*
FORM P3000_PO_GROUP.

   CLEAR : W_PO.
   SORT IT_PO BY EBELN.

   LOOP AT IT_PO.
      IF W_PO NE IT_PO-EBELN.
         MOVE  IT_PO-EBELN  TO  IT_PO_TMP-EBELN.
         APPEND  IT_PO_TMP.
      ENDIF.
      MOVE  IT_PO-EBELN  TO  W_PO.
   ENDLOOP.

ENDFORM.                    " P3000_PO_GROUP
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_WRITE_DATA_SCR0100
*&---------------------------------------------------------------------*
FORM P2000_LINE_WRITE_DATA_SCR0100.

  IF IT_TEMP-PLAC_GB EQ '1'.
     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
     W_GUBUN  =  'Plan'.
  ELSE.
     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      W_GUBUN  =  'Actual'.
  ENDIF.

  IF W_CHECK EQ 'X'.
     WRITE : /                                          SY-VLINE NO-GAP,
            (10) IT_TEMP-EBELN                  NO-GAP,
            (01) '-'                            NO-GAP,
            (05) IT_TEMP-EBELP                  NO-GAP, SY-VLINE NO-GAP,
            (17) IT_TEMP-MATNR                  NO-GAP,
            (23) IT_TEMP-TXZ01                  NO-GAP, SY-VLINE NO-GAP.
  ELSE.
     WRITE : /                                          SY-VLINE NO-GAP,
            (16) ' '                            NO-GAP, SY-VLINE NO-GAP,
            (40) ' '                            NO-GAP, SY-VLINE NO-GAP.
  ENDIF.

  WRITE :(06) W_GUBUN                        NO-GAP, SY-VLINE NO-GAP,
         (16) IT_TEMP-GP_FO CURRENCY IT_TEMP-WAERS   NO-GAP,
         (05) IT_TEMP-WAERS                  NO-GAP, SY-VLINE NO-GAP,
         (16) IT_TEMP-GP_LO CURRENCY IT_TEMP-HSWAE   NO-GAP,
         (05) IT_TEMP-HSWAE                  NO-GAP, SY-VLINE NO-GAP,
         (16) IT_TEMP-TARIF CURRENCY IT_TEMP-HSWAE   NO-GAP,
                                                     SY-VLINE NO-GAP,
         (16) IT_TEMP-FREIGHT CURRENCY IT_TEMP-HSWAE NO-GAP,
                                                     SY-VLINE NO-GAP,
         (16) IT_TEMP-LC_AMT CURRENCY IT_TEMP-HSWAE  NO-GAP,
                                                     SY-VLINE NO-GAP,
         (16) IT_TEMP-BL_AMT CURRENCY IT_TEMP-HSWAE  NO-GAP,
                                                     SY-VLINE NO-GAP,
         (13) IT_TEMP-PO_QTY UNIT     IT_TEMP-MEINS  NO-GAP,
         (05) IT_TEMP-MEINS                  NO-GAP, SY-VLINE NO-GAP,
         (13) IT_TEMP-GR_QTY UNIT     IT_TEMP-MEINS  NO-GAP,
                                                     SY-VLINE NO-GAP.

ENDFORM.                    " P2000_LINE_WRITE_DATA_SCR0100
