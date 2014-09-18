*&---------------------------------------------------------------------*
*& Report  ZRIMPOCRT                                                   *
*&---------------------------------------------------------------------*
*& Report ZRIMPOCRT                                                    *
*&---------------------------------------------------------------------*
*&  Program : P/O Due List                                          *
*&     Name : Na Shin-Ho INFOLINK Ltd.                                *
*&     Date : 2001.01.09                                            *
*&---------------------------------------------------------------------*
*&   DESC. : 1. Display not created import request P/O Doc.
*&---------------------------------------------------------------------*
*& [Change]
*&---------------------------------------------------------------------*
REPORT  ZRIMPOCRT NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                  LINE-SIZE 126.

TABLES: EKKO,                " Purchasing Document Header Table
        EKPO,                " Purchasing Document Item Table
        ZTREQHD,             " Import request Header Table..
        ZTREQIT,             " Import request Item Table..
        ZTIMIMG01,           " Payment Term Configuration.
        ZTIMIMG00.           " Basic Config.

DATA: BEGIN OF IT_IMG00 OCCURS 1000,
      BSART     LIKE   ZTIMIMG01-BSART,
      BSTYP     LIKE   ZTIMIMG01-BSTYP,
      ZTERM     LIKE   ZTIMIMG01-ZTERM,
      ZFAPLDT   LIKE   ZTIMIMG01-ZFAPLDT,
      END OF IT_IMG00.

DATA: BEGIN OF IT_IMG01 OCCURS 1000,
      BSART     LIKE   ZTIMIMG01-BSART,
      BSTYP     LIKE   ZTIMIMG01-BSTYP,
      ZTERM     LIKE   ZTIMIMG01-ZTERM,
      ZFREQTY   LIKE   ZTIMIMG01-ZFREQTY,
      ZFAPLDT   LIKE   ZTIMIMG01-ZFAPLDT,
      END OF IT_IMG01.

DATA: BEGIN OF IT_TAB OCCURS 1000.
      INCLUDE STRUCTURE EKKO.
DATA: EBELP  LIKE  EKPO-EBELP,
      MENGE  LIKE  EKPO-MENGE.
DATA  END OF IT_TAB.

DATA: BEGIN OF IT_TEMP OCCURS 1000.
      INCLUDE STRUCTURE EKKO.
DATA: EBELP  LIKE  EKPO-EBELP,
      MENGE  LIKE  EKPO-MENGE.
DATA  END OF IT_TEMP.

DATA : BEGIN OF IT_REQIT  OCCURS 0,
       EBELN       LIKE   ZTREQIT-EBELN,
       EBELP       LIKE   ZTREQIT-EBELP,
       MENGE       LIKE   ZTREQIT-MENGE.
DATA : END   OF IT_REQIT.

DATA: TAB_INDEX    TYPE   I,
      W_TABIX      LIKE   SY-TABIX,
      W_EBELN      LIKE   EKKO-EBELN,
      W_FIELD_NM   LIKE   DD03D-FIELDNAME,                 " Field name
      TEMP         TYPE   F,
      W_ERR_CHK    TYPE   C   VALUE 'N',
      P_BUKRS      LIKE   ZTIMIMG00-ZFBUKRS.
TAB_INDEX = 0.

INCLUDE   ZRIMSORTCOM.    " Include for import request report sort
*-----------------------------------------------------------------------
* Search condition Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_ZTERM   FOR ZTIMIMG01-ZTERM,     " Payment term key.
                S_REQTY   FOR ZTIMIMG01-ZFREQTY.   " Import request type
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                                NO INTERVALS,
               S_EBELN   FOR EKKO-EBELN,            " P/O No.
               S_EKORG   FOR EKKO-EKORG,            " Purch. Org.
               S_EKGRP   FOR EKKO-EKGRP,            " Purch. Grp.
               S_LIFNR   FOR ZTREQHD-LIFNR.         " Vendor.
SELECTION-SCREEN END OF BLOCK B2.
*-----------------------------------------------------------------------
* Initialization.
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM  P1000_SET_BUKRS.
  SET TITLEBAR 'TIT1'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-LOW.
  PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
  PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-HIGH.

*----------------------------------------------------------------------*
* Top of Page.
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  IF SY-LANGU EQ '3'.
     PERFORM P3000_TITLE_WRITE.             "Header write...
  ELSE.
     PERFORM P3000_TITLE_WRITE_EN.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection.
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Payment Term Configuration Select.
  PERFORM P1000_READ_ZTERM.
  CHECK W_ERR_CHK EQ 'N'.

* Read PO data
  PERFORM P1000_READ_PO_DATA.
  IF W_ERR_CHK EQ 'Y'.
    MESSAGE S966. EXIT.
  ENDIF.

  PERFORM P3000_WRITE_PO_DATA.

  PERFORM P3000_BOTTOM_WRITE.

*-----------------------------------------------------------------------
* END-OF-SELECTION.
*-----------------------------------------------------------------------
END-OF-SELECTION.

*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
*>> PO DATA DISPLAY
    WHEN 'PODP'.
      IF NOT IT_TAB-EBELN IS INITIAL.
        SELECT SINGLE * FROM EKKO
               WHERE  EBELN  EQ  IT_TAB-EBELN.

        CASE EKKO-BSTYP.
          WHEN 'K'.
            SET PARAMETER ID 'CTR' FIELD IT_TAB-EBELN.
            CALL TRANSACTION 'ME33K' AND SKIP FIRST SCREEN.
          WHEN 'L'.
            SET PARAMETER ID 'SAG' FIELD IT_TAB-EBELN.
            CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.
          WHEN OTHERS.
            SET PARAMETER ID 'BES' FIELD IT_TAB-EBELN.
            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        ENDCASE.
      ENDIF.
*>> Import request create
    WHEN 'LCCR'.
      IF NOT IT_TAB-EBELN IS INITIAL.
        SET PARAMETER ID 'BES' FIELD IT_TAB-EBELN.
        CALL TRANSACTION 'ZIM01' AND SKIP FIRST SCREEN.

        PERFORM P1000_READ_PO_DATA.
        IF W_ERR_CHK EQ 'Y'.
          MESSAGE  S325.
          LEAVE TO SCREEN 0.
        ENDIF.
        MOVE 0 TO SY-LSIND.

        IF SY-LANGU EQ '3'.
           PERFORM P3000_TITLE_WRITE.             "Header write...
        ELSE.
           PERFORM P3000_TITLE_WRITE_EN.
        ENDIF.
        PERFORM P3000_WRITE_PO_DATA.
        PERFORM P3000_BOTTOM_WRITE.
      ENDIF.
*>> PAYMENT TERMS CONFIG.
    WHEN 'PAYT'.
      SET PARAMETER ID 'DFD' FIELD IT_TAB-ZTERM.
      CALL TRANSACTION 'ZIMG01' AND SKIP FIRST SCREEN.

    WHEN 'STUP' OR 'STDN'.         " SORT Selection
      W_FIELD_NM = 'EBELN'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.

  ENDCASE.
  CLEAR: IT_TAB.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTERM
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTERM.

  SELECT BSART BSTYP ZTERM  MAX( ZFAPLDT ) AS ZFAPLDT
  INTO   CORRESPONDING FIELDS OF TABLE IT_IMG00
  FROM   ZTIMIMG01
  WHERE  ZTERM     IN   S_ZTERM
  AND    ZFREQTY   IN   S_REQTY
  AND    ZFAPLDT   LE   SY-DATUM
  GROUP BY BSART BSTYP ZTERM.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_IMG01
  FROM   ZTIMIMG01
  FOR ALL ENTRIES  IN   IT_IMG00
  WHERE  BSART     EQ   IT_IMG00-BSART
  AND    BSTYP     EQ   IT_IMG00-BSTYP
  AND    ZTERM     EQ   IT_IMG00-ZTERM
  AND    ZFAPLDT   EQ   IT_IMG00-ZFAPLDT.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S353.
    EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_ZTERM

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

  DATA : W_TOT_MENGE  LIKE ZTREQIT-MENGE,
         W_MENGE_CHK,
         W_COUNT     TYPE  I.

  RANGES : R_ZTERM   FOR   ZTIMIMG01-ZTERM OCCURS 0.

  REFRESH : IT_TAB, IT_TEMP, IT_REQIT.
  CLEAR   : W_EBELN.

  W_ERR_CHK = 'N'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
           FROM EKKO AS  A INNER JOIN  EKPO AS B
           ON   A~EBELN          EQ   B~EBELN
           FOR ALL ENTRIES IN IT_IMG01
           WHERE A~ZTERM          EQ   IT_IMG01-ZTERM
           AND   A~BSTYP          EQ   IT_IMG01-BSTYP
           AND   A~BSART          EQ   IT_IMG01-BSART
           AND   A~AUTLF          EQ   ''
           AND   A~LOEKZ          EQ   ''
           AND   B~ELIKZ          EQ   ''
           AND   B~LOEKZ          EQ   ''
           AND   A~BUKRS          IN   S_BUKRS           " Company Code
           AND   A~ZTERM          IN   S_ZTERM           " Payment Term
           AND   A~EBELN          IN   S_EBELN           " P/O No.
           AND   A~EKORG          IN   S_EKORG           " Purch. Org.
           AND   A~EKGRP          IN   S_EKGRP           " Purch. Grp.
           AND   A~LIFNR          IN   S_LIFNR.          " Vendor.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REQIT
           FROM ZTREQIT
           FOR ALL ENTRIES IN IT_TEMP
           WHERE EBELN   EQ   IT_TEMP-EBELN
           AND   EBELP   EQ   IT_TEMP-EBELP.

  SORT IT_TEMP   BY EBELN EBELP.
  SORT IT_REQIT  BY EBELN EBELP.

*>> Group by P/O-ITEM
  CLEAR W_EBELN.
  LOOP AT IT_TEMP.
    W_TABIX  =  SY-TABIX.
    W_MENGE_CHK = 'N'.
    READ TABLE IT_REQIT WITH KEY  EBELN  =  IT_TEMP-EBELN
                                  EBELP  =  IT_TEMP-EBELP.

    IF SY-SUBRC EQ 0.

      SELECT SUM( MENGE )  INTO  W_TOT_MENGE
      FROM   ZTREQIT
      WHERE  EBELN         EQ    IT_TEMP-EBELN
      AND    EBELP         EQ    IT_TEMP-EBELP.

      IF IT_TEMP-MENGE GT W_TOT_MENGE.
        W_MENGE_CHK = 'Y'.
      ENDIF.
    ELSE.
      W_MENGE_CHK = 'Y'.
    ENDIF.

    IF  W_EBELN  NE IT_TEMP-EBELN AND W_MENGE_CHK = 'Y'.
      MOVE : IT_TEMP-EBELN  TO  IT_TAB-EBELN,
             IT_TEMP-LIFNR  TO  IT_TAB-LIFNR,
             IT_TEMP-WAERS  TO  IT_TAB-WAERS,
             IT_TEMP-ZTERM  TO  IT_TAB-ZTERM,
             IT_TEMP-BEDAT  TO  IT_TAB-BEDAT,
             IT_TEMP-EKORG  TO  IT_TAB-EKORG,
             IT_TEMP-EKGRP  TO  IT_TAB-EKGRP,
             IT_TEMP-ERNAM  TO  IT_TAB-ERNAM,
             IT_TEMP-BSART  TO  IT_TAB-BSART,
             IT_TEMP-BSTYP  TO  IT_TAB-BSTYP.
      APPEND  IT_TAB.
      W_EBELN  =  IT_TEMP-EBELN.
    ENDIF.

  ENDLOOP.
  W_MENGE_CHK = 'N'.
  DESCRIBE TABLE IT_TAB LINES W_COUNT.

  IF W_COUNT LE 0.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE: /55 '[ Import request creation List ]'
             COLOR COL_HEADING INTENSIFIED OFF.

  WRITE: / 'Date: ' COLOR COL_NORMAL INTENSIFIED ON,
            SY-DATUM COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE: / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE: / SY-VLINE, 'Purchase Doc',   SY-VLINE, 'Type',
           SY-VLINE, 'Vendor    ',   SY-VLINE, 'Currency unit',
           SY-VLINE, 'Payment term    ', SY-VLINE, ' P/O Date ',
           SY-VLINE, 'Purchasing Org', SY-VLINE, 'Purchasing GRP',
           SY-VLINE, 'Person in charge', SY-VLINE, 'Pur Doc category',
           SY-VLINE, SY-ULINE.
  FORMAT RESET.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_BOTTOM_WRITE
*&---------------------------------------------------------------------*
FORM P3000_BOTTOM_WRITE.
  ULINE.
     WRITE: 100 'Total:', TAB_INDEX, 'Case'.

ENDFORM.                    " P3000_BOTTOM_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_PO_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_PO_DATA.
  CLEAR  TAB_INDEX.

  SET TITLEBAR 'TIT1'.
  SET PF-STATUS 'ZIM01N'.

  LOOP AT IT_TAB.
    READ TABLE IT_IMG01 WITH KEY   BSART = IT_TAB-BSART
                                   BSTYP = IT_TAB-BSTYP
                                   ZTERM = IT_TAB-ZTERM.
    TEMP = TAB_INDEX MOD 2.
    IF TEMP EQ 0.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDIF.
    WRITE: / SY-VLINE, IT_TAB-EBELN, SY-VLINE, IT_IMG01-ZFREQTY,
          22 SY-VLINE, IT_TAB-LIFNR, 35 SY-VLINE, IT_TAB-WAERS,
          46 SY-VLINE, IT_TAB-ZTERM, 61 SY-VLINE, IT_TAB-BEDAT,
             SY-VLINE, IT_TAB-EKORG, 85 SY-VLINE, IT_TAB-EKGRP,
          96 SY-VLINE, IT_TAB-ERNAM, SY-VLINE, IT_TAB-BSART,
         126 SY-VLINE.

    FORMAT RESET.
    HIDE: IT_TAB.
    CLEAR: IT_TAB.
    TAB_INDEX = TAB_INDEX + 1.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_PO_DATA
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RESET_LIST.
  MOVE 0 TO SY-LSIND.

  IF SY-LANGU EQ '3'.
     PERFORM   P3000_TITLE_WRITE.                  " Header write...
  ELSE.
     PERFORM   P3000_TITLE_WRITE_EN.
  ENDIF.
* Report Write
  PERFORM   P3000_WRITE_PO_DATA.
  PERFORM P3000_BOTTOM_WRITE.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_ZTERM_LOW  text
*----------------------------------------------------------------------*
FORM P1000_PAY_TERM_HELP USING    P_ZTERM.

  TABLES : T052.

  CALL FUNCTION 'FI_F4_ZTERM'
       EXPORTING
            I_KOART       = 'K'
            I_ZTERM       = P_ZTERM
            I_XSHOW       = ' '
       IMPORTING
            E_ZTERM       = T052-ZTERM
       EXCEPTIONS
            NOTHING_FOUND = 01.

  IF SY-SUBRC NE 0.
*   message e177 with ekko-zterm.
    MESSAGE S177(06) WITH P_ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    P_ZTERM = T052-ZTERM.
  ENDIF.


ENDFORM.                    " P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE_EN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE_EN.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE: /55 '[ P/O Due List ]'
             COLOR COL_HEADING INTENSIFIED OFF.

  WRITE: / 'Date: ' COLOR COL_NORMAL INTENSIFIED ON,
            SY-DATUM COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE: / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE: / SY-VLINE, '    P/O   ',   SY-VLINE NO-GAP, 'Type  ',
           SY-VLINE, 'Vendor    ',   SY-VLINE NO-GAP, ' Currency',
           SY-VLINE, 'Payment Term', SY-VLINE NO-GAP, ' P/O Date  ',
           SY-VLINE, 'Pur.Org.',     SY-VLINE NO-GAP, ' Pur.Grp.',
           SY-VLINE, 'Create by   ', SY-VLINE NO-GAP, 'Doc. Category',
           SY-VLINE, SY-ULINE.
  FORMAT RESET.

ENDFORM.                    " P3000_TITLE_WRITE_EN
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> Company code SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
