*&---------------------------------------------------------------------*
*& Report  ZRIMAPCNC                                                   *
*&---------------------------------------------------------------------*
*&  Program Name : Import Expense posting/reverse                      *
*&  Created by   : Na Hyun Ju INFOLINK Ltd.                            *
*&  Created on   : 2001.05.14                                          *
*&---------------------------------------------------------------------*
*&  DESC.        : Import Expense posting/reverse
*&
*&---------------------------------------------------------------------*
*& [Change Log]
*&    1. 2001/10/12  Kang suk bong Modify
*&       - Expense Code Description Display
*&       - Insuarance Expense -> Posting Check
*&       - B/L Expense -> B/L Expense Table Update
*&---------------------------------------------------------------------*
REPORT  ZRIMAPCNC    MESSAGE-ID ZIM
                     LINE-SIZE 123
                     NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE:  <ICON>.

*>> TABLE DEFINE.
TABLES : ZTBKPF,
         ZTBSEG,
         ZTREQHD,
         ZTREQIT,
         ZTBL,
         ZTBLIT,
         ZTIV,
         ZTIVIT,
         ZTIMIMG00,
         ZTIMIMG08,
         EKKO,
         MKPF,
         SPOP,
         UF05A,
         BSIS,
         BAL_S_DMSG,
         BDCMSGCOLL,
         BAPIRET2.

*>>> ERROR
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
INCLUDE  STRUCTURE  BDCMSGCOLL.
DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
       MESSTXT(255) TYPE C,
       ZFCSTGRP      LIKE ZTBKPF-ZFCSTGRP,
       BELNR         LIKE ZTBKPF-BELNR,
       GJAHR         LIKE ZTBKPF-GJAHR.
DATA : END OF IT_ERR_LIST.

DATA: BEGIN OF    IT_TAB     OCCURS 0,
      BUKRS           LIKE   ZTBKPF-BUKRS,
      BELNR           LIKE   ZTBKPF-BELNR,
      GJAHR           LIKE   ZTBKPF-GJAHR,
      ZFCSTGRP        LIKE   ZTBKPF-ZFCSTGRP,
      CSTNM(20)       TYPE   C,
      WRBTR           LIKE   ZTBKPF-WRBTR,
      WAERS           LIKE   ZTBKPF-WAERS,
      HWAER           LIKE   ZTBKPF-HWAER,
      DMBTR           LIKE   ZTBKPF-DMBTR,
      WMWST           LIKE   ZTBKPF-WMWST,
      BUDAT           LIKE   ZTBKPF-BUDAT,
      LIFNR           LIKE   ZTBKPF-LIFNR,
      LIFNR_NM(20)    TYPE   C,
      ZFVEN           LIKE   ZTBKPF-ZFVEN,
      ZFVEN_NM(20)    TYPE   C,
      MWSKZ           LIKE   ZTBKPF-MWSKZ,
      BUPLA           LIKE   ZTBKPF-BUPLA,
      ZTERM           LIKE   ZTBKPF-ZTERM,
      ZFBDT           LIKE   ZTBKPF-ZFBDT,
      KURSF           LIKE   ZTBKPF-KURSF,
      WWERT           LIKE   ZTBKPF-WWERT,
      ZFIMDNO         LIKE   ZTBKPF-ZFIMDNO,
      ZFPOSYN         LIKE   ZTBKPF-ZFPOSYN,
      ZFACDO          LIKE   ZTBKPF-ZFACDO,
      ZFFIYR          LIKE   ZTBKPF-ZFFIYR,
      ZFRVSX          LIKE   ZTBKPF-ZFRVSX,
      YN(8)           TYPE   C,
      GRP_MARK(10)    TYPE   C,
END OF IT_TAB.

DATA : IT_ZSBSEG      LIKE  ZSBSEG  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSBSEG_OLD  LIKE  ZSBSEG  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSBDIV      LIKE  ZSBDIV  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSBHIS      LIKE  ZSBHIS  OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF    IT_SELECTED     OCCURS 0,
      BUKRS           LIKE   ZTBKPF-BUKRS,
      BELNR           LIKE   ZTBKPF-BELNR,
      GJAHR           LIKE   ZTBKPF-GJAHR,
END OF IT_SELECTED.

DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA:   W_ERR_CNT         TYPE I,
        W_ERR_STATUS      TYPE C,
        W_COUNT           TYPE I,
        W_PO_COUNT        TYPE I,
        W_DIV_CNT         TYPE I,
        W_SELECTED_LINES  TYPE I,
        W_LINE            TYPE I,
        W_PAGE            TYPE I,
        W_PROC_CNT        TYPE I,
        W_ERR_CHK         TYPE C,
        INCLUDE(8)        TYPE C,
        W_FIELD_NM(20)    TYPE C,
        W_YN(10)          TYPE C,
        ZFACDO            LIKE ZTBKPF-ZFACDO,
        ZFFIYR            LIKE ZTBKPF-ZFFIYR,
        W_TABIX           LIKE SY-TABIX,
        W_LIST_INDEX      LIKE SY-TABIX,
        OK-CODE           LIKE SY-UCOMM,
        W_MOD             TYPE I,
        P_BUKRS           LIKE ZTBKPF-BUKRS.

INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.

*-----------------------------------------------------------------------
* Selection Screen.
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS  FOR  ZTBKPF-BUKRS NO-EXTENSION NO INTERVALS,
                S_GROUP  FOR  ZTBKPF-ZFCSTGRP,   " Expense Group
                S_ZFCD   FOR  ZTBSEG-ZFCD,       " Expense Code
                S_EBELN  FOR  EKKO-EBELN,        " P/O NO.
                S_CNAME  FOR  ZTBKPF-ZFCNAME,    " Charge
                S_VEN    FOR  ZTBKPF-LIFNR,      " Vendor(Payee).
                S_ZFVEN  FOR  ZTBKPF-ZFVEN,      " Vendor
                S_ZTERM  FOR  ZTBKPF-ZTERM,      " Terms Of Payment
                S_MWSKZ  FOR  ZTBKPF-MWSKZ,      " Tax Code
                S_WERKS  FOR  ZTBKPF-BUPLA,      " Plant
                S_OCDT   FOR  ZTBKPF-ZFBDT,      " baseline date
                S_PSDT   FOR  ZTBKPF-BUDAT.      " posting date
PARAMETERS : P_ITEM AS CHECKBOX DEFAULT 'X'. ">Item Display Yes/No
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
* Follow-up progress status
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN : COMMENT 17(12) TEXT-003, POSITION 30.
PARAMETERS : P_YES  AS CHECKBOX.     " None
SELECTION-SCREEN : COMMENT 48(12) TEXT-004, POSITION 60.
PARAMETERS : P_NO  AS CHECKBOX.     " None
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

*> Payment term HELP.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-LOW.
  PERFORM   P2000_PAYMENT_TERM_HELP  USING  S_ZTERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
  PERFORM   P2000_PAYMENT_TERM_HELP  USING  S_ZTERM-HIGH.

*> Cost group HELP.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZFCD-LOW.
  PERFORM   P1000_COST_CODE_HELP  USING  S_ZFCD-LOW 'S_ZFCD-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZFCD-HIGH.
  PERFORM   P1000_COST_CODE_HELP  USING  S_ZFCD-HIGH 'S_ZFCD-HIGH'.

INITIALIZATION.                          " Initial value SETTING
  PERFORM   P2000_INIT.

* Title Text Write
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU'.
    PERFORM   P3000_TITLE_WRITE.                  " Header write...
  ENDIF.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM   P1000_SET_BUKRS.

*-----------------------------------------------------------------------
* START OF SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Import System Configuration Check
  PERFORM   P2000_CONFIG_CHECK     USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Read data..
  PERFORM   P1000_READ_DATA        USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Report Write
  PERFORM   P3000_DATA_WRITE      USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.   ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.         " SORT selection
      W_FIELD_NM = 'ZFREQDT'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
    WHEN 'MKAL' OR 'MKLO'.         " Selection & Deselection
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
    WHEN 'DPDO'.                   " Expense Document Display
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ENDIF.
      IF W_SELECTED_LINES GT 1.
        MESSAGE S965.
        EXIT.
      ENDIF.
      PERFORM  P4000_DISPLAY_DOCUMENT.
    WHEN 'POST'.                  " Posting
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ENDIF.
      PERFORM P4000_COST_POST      USING W_ERR_CHK.
      DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
      IF W_LINE GT 0.
        INCLUDE = 'POPU'.
        CALL SCREEN 0100 STARTING AT  05   3
                         ENDING   AT  100 12.
        CLEAR : INCLUDE.
      ENDIF.
      PERFORM   P1000_READ_DATA  USING W_ERR_CHK.
      MESSAGE S826 WITH W_PROC_CNT.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM RESET_LIST.
    WHEN 'POCN'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ENDIF.
      PERFORM P4000_COST_CANC         USING W_ERR_CHK.
      DESCRIBE  TABLE IT_ERR_LIST     LINES  W_LINE.
      IF W_LINE GT 0.
        INCLUDE = 'POPU'.
        CALL SCREEN 0100 STARTING AT  05   3
                         ENDING   AT  100 12.
        CLEAR : INCLUDE.
      ENDIF.
      PERFORM   P1000_READ_DATA  USING W_ERR_CHK.
      MESSAGE S826 WITH W_PROC_CNT.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM RESET_LIST.
    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'REFR'.
      PERFORM   P1000_READ_DATA   USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
      PERFORM RESET_LIST.
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
  ENDCASE.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
  CASE INCLUDE.
    WHEN 'POPU'.
      IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
        MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
                NUMBER IT_ERR_LIST-MSGNR
                WITH   IT_ERR_LIST-MSGV1
                       IT_ERR_LIST-MSGV2
                       IT_ERR_LIST-MSGV3
                       IT_ERR_LIST-MSGV4.
      ENDIF.
      CLEAR : IT_ERR_LIST.
    WHEN OTHERS.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

  MOVE  'X'   TO  P_YES.
  SET  TITLEBAR 'ZIMY4'.           " GUI TITLE SETTING..

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P3000_KOREAN_TITLE.
    WHEN OTHERS.
      PERFORM P3000_ENGLISH_TITLE.
  ENDCASE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING     W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  SET PF-STATUS 'ZIMY4'.           " GUI STATUS SETTING.
  SET  TITLEBAR 'ZIMY4'.           " GUI TITLE SETTING.

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    PERFORM P3000_LINE_WRITE.
    AT LAST.
      PERFORM P3000_LAST_WRITE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE
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
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE                NO-GAP,
       IT_TAB-CSTNM            NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-BUKRS            NO-GAP,
       '           '           NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-DMBTR  CURRENCY IT_TAB-HWAER NO-GAP,
       IT_TAB-HWAER            NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-KURSF            NO-GAP,
       SY-VLINE                NO-GAP,
       '  '                    NO-GAP,
       IT_TAB-BUDAT            NO-GAP,
       '  '                    NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-LIFNR            NO-GAP,
       IT_TAB-LIFNR_NM         NO-GAP,
       SY-VLINE                NO-GAP.
* Hide
  HIDE  IT_TAB.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
  '                    '  NO-GAP,
  SY-VLINE                NO-GAP,
  IT_TAB-GJAHR            NO-GAP,
  '-'                     NO-GAP,
  IT_TAB-BELNR            NO-GAP,
  SY-VLINE                NO-GAP,
  IT_TAB-WMWST    CURRENCY IT_TAB-HWAER NO-GAP,
  IT_TAB-HWAER            NO-GAP,
  SY-VLINE                NO-GAP,
  ' '                     NO-GAP,
  IT_TAB-WWERT            NO-GAP,
  ' '                     NO-GAP,
  SY-VLINE                NO-GAP,
  IT_TAB-ZTERM            NO-GAP,              " Terms Of Payment
  ' /'                    NO-GAP,
  IT_TAB-MWSKZ            NO-GAP,              " Tax Code
  ' /'                    NO-GAP,
  IT_TAB-BUPLA            NO-GAP,              " Plant
  SY-VLINE                NO-GAP,
  '  '                    NO-GAP,
  IT_TAB-ZFBDT            NO-GAP,
  '   '                   NO-GAP,
  SY-VLINE                NO-GAP,
  '   '                   NO-GAP,
  IT_TAB-YN               NO-GAP,
  '   '                   NO-GAP,
  SY-VLINE                NO-GAP.

* Stored value...
  HIDE  IT_TAB.
  ADD 1 TO W_COUNT.

  IF P_ITEM EQ 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBSEG
             FROM  ZTBSEG
             WHERE BUKRS EQ IT_TAB-BUKRS
             AND   BELNR EQ IT_TAB-BELNR
             AND   GJAHR EQ IT_TAB-GJAHR
             AND   ZFCD  IN S_ZFCD.
*>
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

    LOOP AT IT_ZSBSEG.
      W_TABIX = SY-TABIX.
      CLEAR : ZTIMIMG08.
      SELECT SINGLE * FROM ZTIMIMG08
             WHERE ZFCDTY EQ IT_ZSBSEG-ZFCSTGRP
             AND   ZFCD   EQ IT_ZSBSEG-ZFCD.

*> 역기표 전표 ...
      IF IT_TAB-ZFRVSX EQ 'X'.
        IT_ZSBSEG-DMBTR = IT_ZSBSEG-DMBTR * -1.
        IT_ZSBSEG-WRBTR = IT_ZSBSEG-WRBTR * -1.
        IT_ZSBSEG-WMWST = IT_ZSBSEG-WMWST * -1.
      ENDIF.

      WRITE : /  SY-VLINE, ' ',                            SY-VLINE,
              (18) ZTIMIMG08-ZFCDNM,                       SY-VLINE,
              (13) IT_ZSBSEG-ZUONR,                        SY-VLINE,
              (15) IT_ZSBSEG-DMBTR CURRENCY IT_TAB-HWAER NO-GAP,
              (04) IT_TAB-HWAER,                           SY-VLINE,
              (10) IT_ZSBSEG-ZFIMDNO,                      SY-VLINE,
              (12) IT_ZSBSEG-WMWST CURRENCY IT_TAB-HWAER,  SY-VLINE,
              (28) IT_ZSBSEG-SGTXT,                        SY-VLINE.

    ENDLOOP.
  ENDIF.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE : / 'Total', W_COUNT, 'cases'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
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

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR   W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-BUKRS       TO IT_SELECTED-BUKRS,
             IT_TAB-GJAHR       TO IT_SELECTED-GJAHR,
             IT_TAB-BELNR       TO IT_SELECTED-BELNR.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P4000_COST_POST
*&---------------------------------------------------------------------*
FORM P4000_COST_POST USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR: W_PROC_CNT, W_ERR_CNT.

  REFRESH : IT_ERR_LIST.

  LOOP AT IT_SELECTED.

    SELECT SINGLE *
             FROM ZTBKPF
            WHERE BUKRS = IT_SELECTED-BUKRS
              AND GJAHR = IT_SELECTED-GJAHR
              AND BELNR = IT_SELECTED-BELNR.
    IF ZTBKPF-ZFPOSYN EQ 'Y'.
      MESSAGE  S578.
      PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                   USING   'E'.
      CONTINUE.
    ENDIF.

    PERFORM  P2000_SET_LOCK_MODE   USING IT_SELECTED-BUKRS
                                         IT_SELECTED-GJAHR
                                         IT_SELECTED-BELNR
                                         'L'   W_SY_SUBRC.

   SELECT COUNT( * ) INTO W_DIV_CNT
   FROM   ZTBDIV
   WHERE  BUKRS      EQ   IT_SELECTED-BUKRS
   AND    GJAHR      EQ   IT_SELECTED-GJAHR
   AND    BELNR      EQ   IT_SELECTED-BELNR
   AND    EBELN      NE   SPACE.
   IF W_DIV_CNT EQ 0.

      PERFORM  P3000_CHARGE_DB_UPDATE  USING IT_SELECTED-BUKRS
                                             IT_SELECTED-GJAHR
                                             IT_SELECTED-BELNR.
   ENDIF.

*>> AP FUNCTION CALL!
    CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST'
         EXPORTING
              BUKRS      = IT_SELECTED-BUKRS
              BELNR      = IT_SELECTED-BELNR
              GJAHR      = IT_SELECTED-GJAHR
         TABLES
              RETURN     = RETURN
         EXCEPTIONS
              POST_ERROR = 4.

    IF SY-SUBRC NE 0.           ">> When occuring error
      ROLLBACK WORK.
      IF RETURN[] IS INITIAL.
        PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'E'.
      ELSE.
        PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
      ENDIF.
      ADD    1    TO    W_ERR_CNT.
    ELSE.
      COMMIT WORK.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'S'.
      ADD 1       TO W_PROC_CNT.
    ENDIF.

    PERFORM  P2000_SET_LOCK_MODE   USING IT_SELECTED-BUKRS
                                         IT_SELECTED-GJAHR
                                         IT_SELECTED-BELNR
                                         'U'   W_SY_SUBRC.

  ENDLOOP.

ENDFORM.                    " P4000_COST_POST
*&---------------------------------------------------------------------*
*&      Form  P4000_GET_INIVAL
*&---------------------------------------------------------------------*
FORM P4000_GET_INIVAL.

  SELECT SINGLE * FROM ZTIMIMG00.

  IF ZTIMIMG00-CSREALYN EQ 'X'.
    OK-CODE  =  'YES'.
  ELSE.
    MOVE 'Initial Value' TO SPOP-TITEL.
    IF BSIS-BUDAT IS INITIAL.
      MOVE : IT_TAB-BUDAT    TO BSIS-BUDAT,
             '03'            TO UF05A-STGRD.
    ENDIF.

    CALL SCREEN 0010 STARTING AT 30 4
                     ENDING   AT 68 10.
  ENDIF.

ENDFORM.                    " P4000_GET_INIVAL
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0010 OUTPUT.

  SET TITLEBAR  'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0010 INPUT.

  IF OK-CODE NE 'YES' AND OK-CODE NE 'ENTR'.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDIF.

  OK-CODE = 'YES'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF OK-CODE NE 'YES' AND OK-CODE NE 'ENTR'.
    EXIT.
  ENDIF.

  IF UF05A-STGRD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST
                         USING   P_MSGTY.

  MOVE : P_MSGTY             TO     IT_ERR_LIST-MSGTYP,
         SY-MSGID            TO     IT_ERR_LIST-MSGID,
         SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
         SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
         SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
         SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
         SY-MSGV4            TO     IT_ERR_LIST-MSGV4,
         IT_SELECTED-BELNR   TO     IT_ERR_LIST-BELNR,
         IT_SELECTED-GJAHR   TO     IT_ERR_LIST-GJAHR.

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
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE TABLES   IT_ERR_LIST STRUCTURE IT_ERR_LIST.

  LOOP AT  RETURN.

    MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
           RETURN-ID           TO     IT_ERR_LIST-MSGID,
           RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
           RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
           RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
           RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
           RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
           RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT,
           IT_SELECTED-GJAHR   TO     IT_ERR_LIST-GJAHR,
           IT_SELECTED-BELNR   TO     IT_ERR_LIST-BELNR.

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

ENDFORM.                    " P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Message LIST'.
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
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE(105), / SY-VLINE NO-GAP,
                'Type'   NO-GAP, SY-VLINE NO-GAP,
                'Cost doc. ' NO-GAP, SY-VLINE NO-GAP,
                'message text ', 103 SY-VLINE NO-GAP,
                'T'      NO-GAP, SY-VLINE,
              / SY-ULINE(105).
      LOOP AT IT_ERR_LIST.
        W_MOD  =  SY-TABIX MOD 2.
        FORMAT RESET.
        IF W_MOD EQ 0.
          FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
        ELSE.
          FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
        ENDIF.
        WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)   NO-GAP,
                  SY-VLINE NO-GAP, IT_ERR_LIST-BELNR       NO-GAP,
                  SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(85) NO-GAP,
                  SY-VLINE NO-GAP.

        CASE IT_ERR_LIST-MSGTYP.
          WHEN 'E'.
            FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
          WHEN 'W'.
            FORMAT COLOR COL_KEY      INTENSIFIED OFF.
          WHEN 'I'.
            FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
          WHEN 'S'.
            FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
        ENDCASE.

        WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
        HIDE:IT_ERR_LIST.
      ENDLOOP.
      WRITE : / SY-ULINE(105).
      CLEAR : IT_ERR_LIST.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
FORM P2000_CONFIG_CHECK USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  IF ZTIMIMG00-ZFPSMS NE 2.
    W_ERR_CHK = 'Y'.   MESSAGE S573.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_PAYMENT_TERM_HELP
*&---------------------------------------------------------------------*
FORM P2000_PAYMENT_TERM_HELP USING    P_ZTERM.

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
    MESSAGE S177(06) WITH P_ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    P_ZTERM = T052-ZTERM.
  ENDIF.

ENDFORM.                    " P2000_PAYMENT_TERM_HELP
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA USING    W_ERR_CHK.
  RANGES : R_POST   FOR    ZTBKPF-ZFPOSYN  OCCURS 0.

  MOVE 'N' TO W_ERR_CHK.
  REFRESH IT_TAB.

  CLEAR : R_POST.

*>> For Posting.
  IF P_YES EQ 'X' AND P_NO IS INITIAL.
    MOVE : 'I'       TO    R_POST-SIGN,
           'EQ'      TO    R_POST-OPTION,
           'N'       TO    R_POST-LOW.
    APPEND R_POST.
    MOVE : 'I'       TO    R_POST-SIGN,
           'EQ'      TO    R_POST-OPTION,
           'P'       TO    R_POST-LOW.
    APPEND R_POST.
*>> For Cancel.
  ELSEIF P_NO EQ 'X' AND P_YES IS INITIAL.
    MOVE : 'I'       TO    R_POST-SIGN,
           'EQ'      TO    R_POST-OPTION,
           'Y'       TO    R_POST-LOW.
    APPEND R_POST.
  ELSEIF P_YES  EQ  'X'  AND  P_NO EQ 'X'.
  ELSE.
    MESSAGE  S576. EXIT.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
           FROM  ZTBKPF
           WHERE ZFCSTGRP  IN    S_GROUP
           AND   BUKRS     IN    S_BUKRS
           AND   ZFCNAME   IN    S_CNAME
           AND   LIFNR     IN    S_VEN
           AND   ZFVEN     IN    S_ZFVEN
           AND   ZTERM     IN    S_ZTERM
           AND   MWSKZ     IN    S_MWSKZ
           AND   BUPLA     IN    S_WERKS
           AND   BUDAT     IN    S_PSDT
           AND   ZFBDT     IN    S_OCDT
           AND   ZFPOSYN   IN    R_POST.

  LOOP  AT  IT_TAB.
    W_TABIX  =  SY-TABIX.
    IF NOT S_ZFCD[] IS INITIAL.
      SELECT COUNT( * ) INTO W_COUNT
               FROM  ZTBSEG
               WHERE BUKRS EQ IT_TAB-BUKRS
               AND   BELNR EQ IT_TAB-BELNR
               AND   GJAHR EQ IT_TAB-GJAHR
               AND   ZFCD  IN S_ZFCD.
      IF W_COUNT EQ 0.
        DELETE IT_TAB INDEX W_TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.

    SELECT SINGLE NAME1  INTO  IT_TAB-LIFNR_NM
    FROM   LFA1          WHERE LIFNR  EQ  IT_TAB-LIFNR.

    CASE  IT_TAB-ZFCSTGRP.
      WHEN '003'.   MOVE 'Import Request'   TO IT_TAB-CSTNM.
      WHEN '004'.   MOVE 'Oversea trans'    TO IT_TAB-CSTNM.
      WHEN '005'.   MOVE 'bonded transport' TO IT_TAB-CSTNM.
      WHEN '006'.   MOVE 'Clearance'        TO IT_TAB-CSTNM.
      WHEN '009'.   MOVE 'Inland trans'     TO IT_TAB-CSTNM.
    ENDCASE.

    IF  IT_TAB-ZFPOSYN  =  'Y'.
      MOVE  'Complete'    TO  IT_TAB-YN.
    ELSEIF  IT_TAB-ZFPOSYN  =  'N'.
      MOVE  'Not yet '     TO  IT_TAB-YN.
    ELSEIF  IT_TAB-ZFPOSYN  =  'P'.
      MOVE  'Adv Pmnt'     TO  IT_TAB-YN.
    ENDIF.

    IF IT_TAB-ZFRVSX EQ 'X'.
      IT_TAB-WMWST = IT_TAB-WMWST * -1.
      IT_TAB-WRBTR = IT_TAB-WRBTR * -1.
      IT_TAB-DMBTR = IT_TAB-DMBTR * -1.
    ENDIF.

    MODIFY  IT_TAB  INDEX  W_TABIX.
  ENDLOOP.

  LOOP AT IT_TAB.

    W_TABIX  =  SY-TABIX.

    SELECT COUNT( * )
    INTO   W_PO_COUNT
    FROM   ZTBDIV
    WHERE  BUKRS   EQ  IT_TAB-BUKRS
    AND    GJAHR   EQ  IT_TAB-GJAHR
    AND    BELNR   EQ  IT_TAB-BELNR
    AND    EBELN   IN  S_EBELN.

    IF W_PO_COUNT EQ 0.
       DELETE  IT_TAB  INDEX  W_TABIX.
       CONTINUE.
    ENDIF.
 ENDLOOP.

  SORT  IT_TAB  BY ZFCSTGRP  BUKRS  GJAHR BELNR.

  DESCRIBE TABLE  IT_TAB  LINES  W_COUNT.
  IF W_COUNT EQ 0.
    MESSAGE  S738.   W_ERR_CHK  =  'Y'.
  ENDIF.

ENDFORM.                    " P1000_READ_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_COST_CANC
*&---------------------------------------------------------------------*
FORM P4000_COST_CANC USING    W_ERR_CHK.
  REFRESH : IT_ERR_LIST.
  MOVE 'N' TO W_ERR_CHK.
  CLEAR: W_PROC_CNT, W_ERR_CNT.
  PERFORM P4000_GET_INIVAL.
  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
    EXIT.
  ENDIF.

  REFRESH : IT_ERR_LIST.

  LOOP AT IT_SELECTED.
    SELECT  SINGLE * FROM ZTBKPF
    WHERE   BUKRS    EQ   IT_SELECTED-BUKRS
    AND     GJAHR    EQ   IT_SELECTED-GJAHR
    AND     BELNR    EQ   IT_SELECTED-BELNR.

    IF ZTBKPF-ZFPOSYN EQ 'N'.
      MESSAGE  S577.
      PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                   USING   'E'.
      CONTINUE.
    ENDIF.

*>> AP POSTING FUNCTION CALL
    CALL FUNCTION 'ZIM_BAPI_COST_INVOICES_CANCEL'
         EXPORTING
              P_ZFIMDTY                 = 'CS'
              P_BUKRS                   = ZTBKPF-BUKRS
              INVOICEDOCNUMBER          = ZTBKPF-ZFACDO
              FISCALYEAR                = ZTBKPF-ZFFIYR
              REASONREVERSAL            = UF05A-STGRD
              POSTINGDATE               = BSIS-BUDAT
         IMPORTING
              INVOICEDOCNUMBER_REVERSAL = ZFACDO
              FISCALYEAR_REVERSAL       = ZFFIYR
         TABLES
              RETURN                    = RETURN
         EXCEPTIONS
              LIV_ERROR                 = 4.

    IF SY-SUBRC NE 0.           ">> When occuring error...
      IF RETURN[] IS INITIAL.
        PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'E'.
      ELSE.
        PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
      ENDIF.
      ADD    1    TO    W_ERR_CNT.
    ELSE.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'S'.
      ADD 1       TO W_PROC_CNT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " P4000_COST_CANC
*&---------------------------------------------------------------------*
*&      Form  P4000_DISPLAY_DOCUMENT
*&---------------------------------------------------------------------*
FORM P4000_DISPLAY_DOCUMENT.

  LOOP  AT  IT_SELECTED.

    SET  PARAMETER ID  'BUK'       FIELD   IT_SELECTED-BUKRS.
    SET  PARAMETER ID  'GJR'       FIELD   IT_SELECTED-GJAHR.
    SET  PARAMETER ID  'ZPBENR'    FIELD   IT_SELECTED-BELNR.

    CLEAR ZTBKPF.
    SELECT SINGLE *
           FROM  ZTBKPF
           WHERE BUKRS  = IT_SELECTED-BUKRS
             AND GJAHR  = IT_SELECTED-GJAHR
             AND BELNR  = IT_SELECTED-BELNR.

    IF ZTBKPF-ZFPOSYN = 'N'.
      CALL TRANSACTION 'ZIMY2'.
    ELSE.
      CALL TRANSACTION 'ZIMY3'.
    ENDIF.

    PERFORM   P1000_READ_DATA  USING W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
    PERFORM RESET_LIST.

  ENDLOOP.

ENDFORM.                    " P4000_DISPLAY_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
FORM P2000_SET_LOCK_MODE USING    P_BUKRS P_GJAHR P_BELNR
                                  PA_MODE
                                  W_SUBRC.
  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBKPF'
         EXPORTING
              MANDT  = SY-MANDT
              BUKRS  = P_BUKRS
              BELNR  = P_BELNR
              GJAHR  = P_GJAHR
         EXCEPTIONS
              OTHERS = 1.

    W_SUBRC = SY-SUBRC.

    IF W_SUBRC <> 0.
      MESSAGE S510 WITH SY-MSGV1 'Expense Document'
                        P_BELNR ''
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBKPF'
         EXPORTING
              MANDT  = SY-MANDT
              BUKRS  = P_BUKRS
              BELNR  = P_BELNR
              GJAHR  = P_GJAHR
         EXCEPTIONS
              OTHERS = 1.

    W_SUBRC = SY-SUBRC.
  ENDIF.

ENDFORM.                    " P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
*&      Form  P1000_COST_CODE_HELP
*&---------------------------------------------------------------------*
FORM P1000_COST_CODE_HELP USING    P_ZFCD P_FIELDNAME.

  DATA : L_DISPLAY.

  DATA: DYNPROG            LIKE SY-REPID,
        DYNNR              LIKE SY-DYNNR,
        WINDOW_TITLE(30)   TYPE C.
*>> Expense code HELP.
  DATA : BEGIN OF IT_COST_HELP OCCURS 0,
         ZFCD      LIKE ZTIMIMG08-ZFCD,
         ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
         ZFCD1     LIKE ZTIMIMG08-ZFCD1,
         ZFCD5     LIKE ZTIMIMG08-ZFCD5,
         COND_TYPE LIKE ZTIMIMG08-COND_TYPE,
         END OF IT_COST_HELP.

  IF S_GROUP-LOW IS INITIAL.
    SELECT *
           INTO CORRESPONDING FIELDS OF TABLE IT_COST_HELP
           FROM   ZTIMIMG08
           WHERE  ZFCDTY   IN   ('003', '004', '005', '006', '007',
                                 '008').
  ELSE.
    SELECT *
           INTO CORRESPONDING FIELDS OF TABLE IT_COST_HELP
           FROM   ZTIMIMG08
           WHERE  ZFCDTY   EQ   S_GROUP-LOW.
  ENDIF.

  IF SY-SUBRC NE 0.
    MESSAGE S406.
    EXIT.
  ENDIF.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.
  WINDOW_TITLE = '비용코드 Help'.
  CLEAR: L_DISPLAY.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'ZFCD'
            DYNPPROG        = DYNPROG
            DYNPNR          = DYNNR
            DYNPROFIELD     = P_FIELDNAME
            WINDOW_TITLE    = WINDOW_TITLE
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = IT_COST_HELP
       EXCEPTIONS
            PARAMETER_ERROR = 1
            NO_VALUES_FOUND = 2
            OTHERS          = 3.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDFORM.                    " P1000_COST_CODE_HELP
*&---------------------------------------------------------------------*
*&      Form  P3000_KOREAN_TITLE
*&---------------------------------------------------------------------*
FORM P3000_KOREAN_TITLE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /40  '[ Doc journalizing & Reverse of related expense ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / ' Date : ', SY-DATUM, 105 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',                          SY-VLINE NO-GAP,
            'Expense Group     '            NO-GAP, SY-VLINE NO-GAP,
            'Company           '            NO-GAP, SY-VLINE NO-GAP,
            'Expense Amount    '            NO-GAP, SY-VLINE NO-GAP,
            'Exchange rate     '            NO-GAP, SY-VLINE NO-GAP,
            'Posting Date      '            NO-GAP, SY-VLINE NO-GAP,
            'Payee             '            NO-GAP,
            '                  '            NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',                          SY-VLINE NO-GAP,
            '                  '            NO-GAP, SY-VLINE NO-GAP,
            'Doc No            '            NO-GAP, SY-VLINE NO-GAP,
            'Tax amount        '            NO-GAP, SY-VLINE NO-GAP,
            'Exchage rate date '            NO-GAP, SY-VLINE NO-GAP,
            'Term/Tax/Plant    '            NO-GAP, SY-VLINE NO-GAP,
            'Payment date      '            NO-GAP, SY-VLINE NO-GAP,
            'Posting Yes/No    '            NO-GAP, SY-VLINE NO-GAP.

  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
    WRITE : /  SY-VLINE, ' ',  SY-VLINE,
            (18) ' 비용항목',  SY-VLINE,
            (13) ' 지정',      SY-VLINE,
            (19) ' 세부금액',  SY-VLINE,
            (10) ' 관련문서',  SY-VLINE,
            (12) ' 세액',      SY-VLINE,
            (28) ' 텍스트',    SY-VLINE.
  ENDIF.

  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_KOREAN_TITLE
*&---------------------------------------------------------------------*
*&      Form  P3000_ENGLISH_TITLE
*&---------------------------------------------------------------------*
FORM P3000_ENGLISH_TITLE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /38  '[ Expense Posting/Reverse Posting ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / ' Date : ', SY-DATUM, 105 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'Cost Group          '           NO-GAP, SY-VLINE NO-GAP,
            'Company        '                NO-GAP, SY-VLINE NO-GAP,
            '        Cost Amount  '          NO-GAP, SY-VLINE NO-GAP,
            'Exchange Rte'                   NO-GAP, SY-VLINE NO-GAP,
            ' Posting Date '                 NO-GAP, SY-VLINE NO-GAP,
            'Payee               '           NO-GAP,
            '          '                     NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',                           SY-VLINE NO-GAP,
            '                    '           NO-GAP, SY-VLINE NO-GAP,
            'Document Number'                NO-GAP, SY-VLINE NO-GAP,
            '                Tax  '          NO-GAP, SY-VLINE NO-GAP,
            'Excha rte dt'                   NO-GAP, SY-VLINE NO-GAP,
            'Term/Tax/Plant'                 NO-GAP, SY-VLINE NO-GAP,
            'Payment date   '                NO-GAP, SY-VLINE NO-GAP,
            'Posting Yes/No'                 NO-GAP, SY-VLINE NO-GAP.

  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
    WRITE : /  SY-VLINE, ' ',  SY-VLINE,
           (18) ' Cost Item',      SY-VLINE,
           (13) ' Assignment',     SY-VLINE,
           (19) ' Detail amount',  SY-VLINE,
           (10) ' Rel Docum',      SY-VLINE,
           (12) ' Tax Amount',     SY-VLINE,
           (28) ' Text',           SY-VLINE.
  ENDIF.

  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_ENGLISH_TITLE
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : ZTIMIMG00, P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

*>> Company Code Set.
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P3000_CHARGE_DB_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_CHARGE_DB_UPDATE USING    P_BUKRS
                                     P_GJAHR
                                     P_BELNR.

  REFRESH : IT_ZSBSEG, IT_ZSBSEG_OLD.

  SELECT SINGLE * FROM ZTBKPF
  WHERE  BUKRS    EQ   P_BUKRS
  AND    GJAHR    EQ   P_GJAHR
  AND    BELNR    EQ   P_BELNR.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBSEG
  FROM   ZTBSEG
  WHERE  BUKRS    EQ   P_BUKRS
  AND    GJAHR    EQ   P_GJAHR
  AND    BELNR    EQ   P_BELNR.

  IT_ZSBSEG_OLD[]  =  IT_ZSBSEG[].

  " Modify Function Call.
  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
       EXPORTING
            W_OK_CODE     = 'SAVE'
            BUKRS         = ZTBKPF-BUKRS
            GJAHR         = ZTBKPF-GJAHR
            ZFSTATUS      = 'U'
            W_ZTBKPF_OLD  = ZTBKPF
            W_ZTBKPF      = ZTBKPF
       TABLES
            IT_ZSBSEG_OLD = IT_ZSBSEG_OLD
            IT_ZSBSEG     = IT_ZSBSEG
            IT_ZSBDIV     = IT_ZSBDIV
            IT_ZSBHIS     = IT_ZSBHIS
       CHANGING
            BELNR         = ZTBKPF-BELNR
       EXCEPTIONS
            ERROR_UPDATE.

ENDFORM.                    " P3000_CHARGE_DB_UPDATE
