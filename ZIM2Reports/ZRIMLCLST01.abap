*&---------------------------------------------------------------------*
*& Report  ZRIMLCLST01                                                 *
*&---------------------------------------------------------------------*
*&  Program : Import actual results LIST                               *
*&     Name : Lee Chae-Kyung INFOLINK Ltd.                             *
*&     Date : 2001.01.26                                               *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [Change]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLCLST01  MESSAGE-ID ZIM
                     LINE-SIZE 181
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Import request for release INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       EBELN      LIKE ZTREQHD-EBELN,            " Purchasing document
       ZFREQNO    LIKE ZTREQHD-ZFREQNO,          " Import request No
       PAMDNO(5)  TYPE C,                        " ALL AMEND No
       WAERS      LIKE ZTREQHD-WAERS,            " Currency
       ZFMATGB    LIKE ZTREQHD-ZFMATGB,          " Material type
       MATNR      LIKE ZTREQHD-MATNR,            " Material No
       INCO1      LIKE ZTREQHD-INCO1,            " Delivery Condition
       ZFREQSD    LIKE ZTREQHD-ZFREQSD,          " S/D
       ZFREQED    LIKE ZTREQHD-ZFREQED,          " E/D
       ZTERM      LIKE ZTREQHD-ZTERM,            " Terms of Payment
       MAKTX      LIKE ZTREQHD-MAKTX,            " Material description
       ZFWERKS    LIKE ZTREQHD-ZFWERKS,          " Rep Plant
       WERNM(18)  TYPE C,
       LIFNR      LIKE ZTREQHD-LIFNR,            " Vendor Code
       NAME1(25),                                " Name 1
       ZFOPBN     LIKE ZTREQHD-ZFOPBN,           " Open Bank
       NAME2(25),                                " Name 1
       ZFDOCST    LIKE ZTREQST-ZFDOCST,          " Doc status
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,          " AMEND No
       ZFREQTY    LIKE ZTREQST-ZFREQTY,          " Payment type
       ZFOPNNO    LIKE ZTREQST-ZFOPNNO,          " L/C NO
       CDAT       LIKE ZTREQST-CDAT,             " Requested date
       ZFOPNDT    LIKE ZTREQST-ZFOPNDT,          " Issuing date
       ZFOPAMT    LIKE ZTREQST-ZFOPAMT,          " Opening amount
       ZFUSDAM    LIKE ZTREQST-ZFUSDAM,          " USD exchange amount
       ZFEDIST    LIKE ZTREQST-ZFEDIST,          " EDI CHECK
       MATNM(20)  TYPE C,                        " Material type name
       EKGRP      LIKE ZTREQST-EKGRP,            " Pur Group
       GRPNM(18)  TYPE C,
       MEINS      LIKE ZTREQIT-MEINS,            " Quantity unit
       ZFRECNO    LIKE ZTREQIL-ZFRECNO.          " Import recommendation
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
TABLES : ZTREQHD,         " Import request Header
         ZTREQIT,         " Import request Item
         ZTREQST,         " Import request Status
         ZTREQIL,         " Import recommendation.
         LFA1,            " Vendor Master
         ZTIMIMG00,       " BASIC CONFIG.
         ZVREQHD_ST,      " Imp Req Header + Status View
         ZVEKKO_REQHD_ST. " EKKO + Imp Req Header + Status View
*-----------------------------------------------------------------------
* SELECT RECORD
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
       GUBUN      TYPE C,                        " First classification
       ZFREQNO    LIKE ZTREQST-ZFREQNO,          " Imp Req No
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,          " Amend Seq.
       ZFRLST1    LIKE ZTREQST-ZFRLST1,
       ZFRLST2    LIKE ZTREQST-ZFRLST2,
       ZFDOCST    LIKE ZTREQST-ZFDOCST,          " Doc status
       ZFEDIST    LIKE ZTREQST-ZFEDIST,          " EDI status
       ZFEDICK    LIKE ZTREQST-ZFEDICK,          " EDI CHECK
       ZFCLOSE    LIKE ZTREQHD-ZFCLOSE,          " Close status
       ZFREQTY    LIKE ZTREQHD-ZFREQTY,          " DOC. TYPE.
       ZFFILE(100),
END OF IT_SELECTED.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,                 " Selection LINE COUNT
       W_PAGE            TYPE I,                 " Page Counter
       W_LINE            TYPE I,                 " LINE COUNT per page
       LINE(3)           TYPE N,                 " LINE COUNT per page
       W_COUNT           TYPE I,                 " Total COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " Field name.
       W_TABIX           LIKE SY-TABIX,          " TABLE INDEX
       W_UPDATE_CNT      TYPE I,
       W_BUTTON_ANSWER   TYPE C,
       W_ITEM_CNT        LIKE SY-TABIX,          " Item count
       W_AMOUNT          LIKE ZTIV-ZFIVAMT,      " Imp Req Amount
       W_TOT_AMOUNT      LIKE ZTIV-ZFIVAMT,      " Imp Req Amount
       W_LOCAL_AMT       LIKE ZTIV-ZFIVAMT,      " USD Exchange Amount
       W_ZFPWDT          LIKE ZTPMTHD-ZFPWDT,
       W_LFA1            LIKE LFA1,
       W_MENGE           LIKE ZTREQIT-MENGE,
       W_ZSREQIT         LIKE ZSREQIT,
       W_MAX_ZFAMDNO_OLD LIKE ZTREQST-ZFAMDNO,
       W_MAX_ZFAMDNO     LIKE ZTREQST-ZFAMDNO,
       P_BUKRS           LIKE ZTIMIMG00-ZFBUKRS.

INCLUDE   ZRIMSORTCOM.    "  Include for import request Report Sort
INCLUDE   ZRIMUTIL01.     " Utility function

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                               NO INTERVALS,
                   S_RLDT    FOR ZTREQST-ZFOPNDT , " Opening date.
                   S_OPBN    FOR ZTREQHD-ZFOPBN,   " Opening bank.
                   S_MATGB   FOR ZTREQHD-ZFMATGB,  " Material type.
                   S_MATNR   FOR ZTREQHD-MATNR,    " Material No
                   S_REQTY   FOR ZTREQHD-ZFREQTY,  " Imp Req Type
                   S_WERKS   FOR ZTREQHD-ZFWERKS,  " Rep plant
                   S_EKORG   FOR ZTREQST-EKORG,    " Purch. Org.
                   S_EBELN   FOR ZTREQHD-EBELN,    " P/O Number
                   S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                   S_ZFBENI  FOR ZTREQHD-ZFBENI,   " Beneficiary
                   S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
                   S_REQNO   FOR ZTREQHD-ZFREQNO,  " Imp Req No.
                   S_REQDT   FOR ZTREQST-ZFREQDT,
                   S_TERM    FOR ZTREQHD-ZTERM,    " Payment term
                   S_INCO    FOR ZTREQHD-INCO1,    " Delivery condition
                   S_RVDT    FOR ZTREQST-ZFRVDT,   " Registration date
                   S_OPNM    FOR ZTREQST-ZFOPNNM.  " Person(Opening)
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-003.
   PARAMETER : R_NO    RADIOBUTTON GROUP GP1,
               R_YES   RADIOBUTTON GROUP GP1,
               R_ALL   RADIOBUTTON GROUP GP1.    "ALL.
SELECTION-SCREEN END OF BLOCK B2.

* PARAMETER Initial value Setting
INITIALIZATION.                          " Initial value SETTING
   PERFORM   P1000_SET_BUKRS.
   PERFORM   P2000_SET_PARAMETER.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_TERM-LOW.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_TERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_TERM-HIGH.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_TERM-HIGH.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " Header write...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Purchase reqeust Table SELECT
   PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.

* Report Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
   CASE SY-UCOMM.
* SORT Selection
      WHEN 'STUP' OR 'STDN'.         " SORT Selection.
         W_FIELD_NM = 'ZFOPBN'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
         PERFORM RESET_LIST.
* Selection & Deselection
      WHEN 'MKAL' OR 'MKLO'.           " Selection & Deselection
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISP' OR 'PUPRT'.          " L/C Display.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P2000_SHOW_LC USING  IT_SELECTED-ZFREQNO
                                         IT_SELECTED-ZFAMDNO.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
* 구매의뢰 테이블 SELECT
           PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
           IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
           PERFORM RESET_LIST.
      WHEN OTHERS.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIM85'.          " TITLE BAR
  MOVE 'X' TO R_YES.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /90  '[ Transaction opening list ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /3 'Date : ', SY-DATUM, 160 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,  ' ',               SY-VLINE,
                 (10)  'Req date'     NO-GAP, SY-VLINE NO-GAP,
                 (35)  'Supplier'     NO-GAP, SY-VLINE NO-GAP,
                 (35)  'Representative Material'   NO-GAP, SY-VLINE
NO-GAP,
                 (20)  'Purchase Document'   NO-GAP, SY-VLINE NO-GAP,
                 (4)   'Payment'       NO-GAP, SY-VLINE NO-GAP,
                 (10)  'S/D'        NO-GAP, SY-VLINE NO-GAP,
                 (22)  'Plant'     NO-GAP, SY-VLINE NO-GAP,
                 (20)  'Material type'   NO-GAP, SY-VLINE NO-GAP,
                 (10)  'Import Req'   NO-GAP, SY-VLINE NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE,  ' ',               SY-VLINE,
                 (10)  'Opening '       NO-GAP, SY-VLINE NO-GAP,
                 (35)  'Opening bank'     NO-GAP, SY-VLINE NO-GAP,
                 (35)  'L/C No.'     NO-GAP, SY-VLINE NO-GAP,
                 (20)  'Transaction opening amounts' NO-GAP, SY-VLINE
NO-GAP,
                 (4)   'Delivery'         NO-GAP, SY-VLINE NO-GAP,
                 (10)  'E/D'          NO-GAP, SY-VLINE NO-GAP,
                 (22)  'Purchasing group'     NO-GAP, SY-VLINE NO-GAP,
                 (20)  'Recommendation No.'     NO-GAP, SY-VLINE NO-GAP,
                 (10)  'End amend'   NO-GAP, SY-VLINE NO-GAP.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  Screen AUTHORITY CHECK
*-----------------------------------------------------------------------
*  AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
*           ID 'ACTVT' FIELD '*'.
*
*  IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'Request Release Transaction'.
*      W_ERR_CHK = 'Y'.   EXIT.
*  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
FORM P1000_GET_ZVREQHD_ST   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  REFRESH IT_TAB.
  SELECT *  FROM ZTREQHD     WHERE  ZFOPBN     IN     S_OPBN
                             AND    ZFMATGB    IN     S_MATGB
                             AND    MATNR      IN     S_MATNR
                             AND    ZFREQTY    IN     S_REQTY
                             AND    ZFWERKS    IN     S_WERKS
                             AND    EBELN      IN     S_EBELN
                             AND    LIFNR      IN     S_LIFNR
                             AND    ZFBENI     IN     S_ZFBENI
                             AND    ZFREQNO    IN     S_REQNO
                             AND    BUKRS      IN     S_BUKRS
                             AND    ZFCLOSE    EQ     SPACE
                             AND    ZTERM      IN     S_TERM
                             AND    INCO1      IN     S_INCO.

     MOVE-CORRESPONDING ZTREQHD TO IT_TAB.
*>> AMEND No.
     SELECT SINGLE * FROM ZTREQST
            WHERE    ZFREQNO EQ ZTREQHD-ZFREQNO
            AND      ZFAMDNO EQ ( SELECT MAX( ZFAMDNO )
                                  FROM   ZTREQST
                                  WHERE  ZFREQNO  EQ  ZTREQHD-ZFREQNO ).

*>> FILTER
     IF R_NO EQ 'X'.
        IF ZTREQST-ZFDOCST EQ 'O'.
           CONTINUE.
        ENDIF.
     ELSEIF R_YES EQ 'X'.
        IF ZTREQST-ZFDOCST NE 'O'.
           CONTINUE.
        ENDIF.
     ENDIF.
*>> DATA SELECT.
     SELECT SINGLE *  FROM ZTREQST
            WHERE ZFREQNO    EQ     ZTREQHD-ZFREQNO
            AND   ZFAMDNO    EQ     ZTREQST-ZFAMDNO
            AND   ZFOPNDT    IN     S_RLDT
            AND   EKGRP      IN     S_EKGRP
            AND   ZFREQDT    IN     S_REQDT
            AND   ZFRVDT     IN     S_RVDT
            AND   ZFOPNNM    IN     S_OPNM.
     IF SY-SUBRC NE 0 .
        CONTINUE.
     ENDIF.
     MOVE:   ZTREQST-ZFDOCST TO IT_TAB-ZFDOCST,
             ZTREQST-ZFAMDNO TO IT_TAB-ZFAMDNO,
             ZTREQST-ZFREQTY TO IT_TAB-ZFREQTY,
             ZTREQST-ZFOPNNO TO IT_TAB-ZFOPNNO,
             ZTREQST-ZFOPNDT TO IT_TAB-ZFOPNDT,
             ZTREQST-ZFOPAMT TO IT_TAB-ZFOPAMT,
             ZTREQST-ZFUSDAM TO IT_TAB-ZFUSDAM,
             ZTREQST-ZFAMDNO TO IT_TAB-ZFAMDNO,
             ZTREQST-EKGRP   TO IT_TAB-EKGRP,
             ZTREQST-CDAT    TO IT_TAB-CDAT.
*>> SELECT IL.
      SELECT SINGLE * FROM ZTREQIL
                      WHERE ZFREQNO = IT_TAB-ZFREQNO.
      MOVE ZTREQIL-ZFRECNO TO  IT_TAB-ZFRECNO.
*>> Vendor name.
      SELECT SINGLE NAME1 INTO IT_TAB-NAME1 FROM LFA1
             WHERE  LIFNR EQ   ZTREQHD-LIFNR.
*>> OPEN BANK name.
      SELECT SINGLE NAME1 INTO IT_TAB-NAME2  FROM LFA1
             WHERE  LIFNR EQ IT_TAB-ZFOPBN.
*>> PLAT name.
      SELECT SINGLE NAME1 INTO IT_TAB-WERNM  FROM T001W
             WHERE  WERKS EQ   IT_TAB-ZFWERKS.
*>> Purchasign group name.
     SELECT SINGLE EKNAM  INTO IT_TAB-GRPNM  FROM T024
            WHERE  EKGRP  EQ   IT_TAB-EKGRP.
*>> Material type name
     SELECT SINGLE DDTEXT INTO IT_TAB-MATNM
     FROM   DD07T
     WHERE  DOMNAME       EQ   'ZDMATGB'
     AND    DDLANGUAGE    EQ   SY-LANGU
     AND    DOMVALUE_L    EQ   IT_TAB-ZFMATGB.

     APPEND IT_TAB.
 ENDSELECT.
 DESCRIBE TABLE IT_TAB LINES W_LINE.
 IF W_LINE = 0. MESSAGE S009. EXIT.ENDIF.

ENDFORM.                    " P1000_GET_ZVREQST
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIM85'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM85'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P2000_PAGE_CHECK.
      PERFORM P3000_LINE_WRITE.

      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.
   ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

   MOVE 0 TO SY-LSIND.

   W_PAGE = 1.
   W_LINE = 1.
   W_COUNT = 0.
   PERFORM   P3000_TITLE_WRITE.                  " Header write...
* Report Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST

*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFREQNO LIKE ZTREQST-ZFREQNO,
        ZFAMDNO LIKE ZTREQST-ZFAMDNO,
        ZFRLST1 LIKE ZTREQST-ZFRLST1,
        ZFRLST2 LIKE ZTREQST-ZFRLST2,
        ZFDOCST LIKE ZTREQST-ZFDOCST,
        ZFEDIST LIKE ZTREQST-ZFEDIST,
        ZFEDICK LIKE ZTREQST-ZFEDICK,
        ZFREQTY LIKE ZTREQHD-ZFREQTY.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFREQNO  TO ZFREQNO,
         IT_TAB-ZFREQTY  TO ZFREQTY,
         IT_TAB-ZFAMDNO  TO ZFAMDNO.
  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFREQNO  TO IT_SELECTED-ZFREQNO,
             IT_TAB-ZFREQTY  TO IT_SELECTED-ZFREQTY,
             IT_TAB-ZFAMDNO  TO IT_SELECTED-ZFAMDNO.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO,
             ZFREQTY TO IT_SELECTED-ZFREQTY.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ELSE.
      MESSAGE S962.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

   IF W_LINE >= 53.
      WRITE : / SY-ULINE.
      W_PAGE = W_PAGE + 1.    W_LINE = 0.
      NEW-PAGE.
   ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

   FORMAT RESET.
   FORMAT COLOR  6.
   SUM.
*   WRITE:/ SY-VLINE,100 'TOTAL:',
*         109 IT_TAB-ZFUSDAM CURRENCY IT_TAB-ZFUSD,
*         147 SY-VLINE.
   FORMAT COLOR OFF.
   IF W_COUNT GT 0.
      WRITE : / 'Total', W_COUNT, 'records'.
   ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.

   WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX, SY-VLINE ,
          (10) IT_TAB-CDAT  NO-GAP,   SY-VLINE NO-GAP, "Request date.
          (10) IT_TAB-LIFNR NO-GAP,                    "VENDOR
          (25) IT_TAB-NAME1 NO-GAP,   SY-VLINE NO-GAP, "VENDOR name
          (35) IT_TAB-MAKTX NO-GAP,   SY-VLINE NO-GAP, "Material
          (10) IT_TAB-EBELN NO-GAP,                    "PO No
          (10) '          ' NO-GAP,   SY-VLINE NO-GAP, "
          (4)  IT_TAB-INCO1 NO-GAP,   SY-VLINE NO-GAP, "Delivery conditi
          (10) IT_TAB-ZFREQSD NO-GAP, SY-VLINE NO-GAP, "S/D
          (4)  IT_TAB-ZFWERKS NO-GAP,                  "PLANT
          (18) IT_TAB-WERNM   NO-GAP, SY-VLINE NO-GAP, "PLANT name
          (20) IT_TAB-MATNM   NO-GAP, SY-VLINE NO-GAP, "Material type.
          (10) IT_TAB-ZFREQNO NO-GAP, SY-VLINE NO-GAP. "Imp Req No.
* hide
   MOVE SY-TABIX  TO W_LIST_INDEX.
   HIDE: W_LIST_INDEX, IT_TAB.
   MODIFY IT_TAB INDEX SY-TABIX.

   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   WRITE:/ SY-VLINE, ' ',             SY-VLINE ,
          (10) IT_TAB-ZFOPNDT  NO-GAP,   SY-VLINE NO-GAP,   "Issuing dat
          (10) IT_TAB-ZFOPBN   NO-GAP,                      "BANK
          (25) IT_TAB-NAME2    NO-GAP,   SY-VLINE NO-GAP,   "BANK Name
          (35) IT_TAB-ZFOPNNO  NO-GAP,   SY-VLINE NO-GAP,   "L/C No
          (15) IT_TAB-ZFOPAMT  CURRENCY IT_TAB-WAERS  NO-GAP,
          (5)  IT_TAB-WAERS    NO-GAP,   SY-VLINE NO-GAP,   "Opening Amt
          (4)  IT_TAB-ZTERM    NO-GAP,   SY-VLINE NO-GAP,   "Payment ter
          (10) IT_TAB-ZFREQED  NO-GAP,   SY-VLINE NO-GAP,   "E/D
          (4)  IT_TAB-EKGRP    NO-GAP,                      "Pur group
          (18) IT_TAB-GRPNM    NO-GAP,   SY-VLINE NO-GAP,   "Grp name
          (20) IT_TAB-ZFRECNO  NO-GAP,   SY-VLINE NO-GAP,   "Recommen No
          (5)  IT_TAB-ZFAMDNO  NO-GAP,
          (5)  '     '         NO-GAP,   SY-VLINE NO-GAP.   "AMEND No

   WRITE : / SY-ULINE.
* hide
   MOVE SY-TABIX  TO W_LIST_INDEX.
   HIDE: W_LIST_INDEX, IT_TAB.
   MODIFY IT_TAB INDEX SY-TABIX.
   W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO P_ZFAMDNO.

   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
   EXPORT 'BES'           TO MEMORY ID 'BES'.
   EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
   EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
   EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.

   IF P_ZFAMDNO = '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_LC
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
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
