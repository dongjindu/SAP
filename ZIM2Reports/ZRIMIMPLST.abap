*&---------------------------------------------------------------------*
*& Report  ZRIMIMPLST                                                  *
*&---------------------------------------------------------------------*
*&  Program Name : Import Result List                                  *
*&    Created by : SY, JUNG INFOLINK Ltd.                              *
*&    Created on : 2003.04.28                                          *
*&---------------------------------------------------------------------*
REPORT  ZRIMIMPLST       MESSAGE-ID ZIM
                         LINE-SIZE 110
                         NO STANDARD PAGE HEADING .

TABLES : LFA1,  T000, ZTREQHD, ZTREQST, BSIK, BKPF,
         ZTCIVIT, ZTCIVHD, ZTCIVHST, ZTBL, EKKO, ZTIMIMG00.

DATA : W_ERR_CHK         TYPE C,
       W_SUBRC           LIKE SY-SUBRC,
       W_PAGE            TYPE I,
       W_LINE            TYPE I,                " LINE COUNT
       W_COUNT           TYPE I,                " TOTAL COUNT
       P_BUKRS           LIKE ZTREQHD-BUKRS.

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFREQNO  LIKE   ZTCIVIT-ZFREQNO,
       ZFOPNNO  LIKE   ZTREQST-ZFOPNNO,
       ZFOPNDT  LIKE   ZTREQST-ZFOPNDT,
       ZFBLNO   LIKE   ZTCIVIT-ZFBLNO,
       ZFHBLNO  LIKE   ZTBL-ZFHBLNO,
       ZFMBLNO  LIKE   ZTBL-ZFMBLNO,
       ZFBLDT   LIKE   ZTBL-ZFBLDT,
       EBELN    LIKE   ZTCIVIT-EBELN,
       ZFCIVRN  LIKE   ZTCIVHD-ZFCIVRN,
       ZFCIVNO  LIKE   ZTCIVHD-ZFCIVNO,
       ZFCIDT   LIKE   ZTCIVHD-ZFCIDT,
       ZFIVAMC  LIKE   ZTCIVHD-ZFIVAMC,
       ZFIVAMT  LIKE   ZTCIVHD-ZFIVAMT,
       ZFOPBN   LIKE   ZTCIVHD-ZFOPBN,
       BKNAME   LIKE   LFA1-NAME1,
       ZTERM    LIKE   ZTCIVHD-ZTERM,
       TERM_NM(50),
       ZFREQTY  LIKE   ZTREQST-ZFREQTY,
       WRBTR    LIKE   BSIK-WRBTR,         "지급액.
       BALANCE  LIKE   BSIK-WRBTR,         "미지급액.
       ZFDUEDT  LIKE   BSIK-ZFBDT,         " dueDT.
       ZFSTATUS(10),
       ZFPDDT   TYPE   D,
       ZFMAVN   LIKE   ZTCIVHD-ZFMAVN,
       BNNAME   LIKE   LFA1-NAME1,
       BELNR    LIKE   ZTCIVHST-BELNR,   "Invoice doc. no.
       GJAHR    LIKE   ZTCIVHST-GJAHR,
       F_BELNR  LIKE   BSIK-BELNR,       "Account doc. no.
       F_GJAHR  LIKE   BSIK-GJAHR.
DATA : END OF IT_TAB.

DATA : BEGIN OF IT_REQ OCCURS 0.
        INCLUDE STRUCTURE ZTCIVIT.
DATA : END   OF IT_REQ.

DATA : W_AWKEY  LIKE   BKPF-AWKEY,
       W_BELNR  LIKE   BSIK-BELNR,
       W_GJAHR  LIKE   BSIK-GJAHR.

DATA : BEGIN OF IT_FIDOC OCCURS 0,
       BELNR    LIKE   BSIK-BELNR,
       GJAHR    LIKE   BSIK-GJAHR,
       BUZEI    LIKE   BSIK-BUZEI,
       ZFBDT    LIKE   BSIK-ZFBDT,
       ZBD1T    LIKE   BSIK-ZBD1T,
       ZBD2T    LIKE   BSIK-ZBD2T,
       ZBD3T    LIKE   BSIK-ZBD3T,
       WRBTR    LIKE   BSIK-WRBTR,
       BUDAT    LIKE   BSIK-BUDAT.
DATA : END   OF IT_FIDOC.

DATA : C_COL TYPE I.

TYPE-POOLS : SLIS.

DATA : W_PROG       LIKE  SY-REPID,
       GT_FIELDCAT  TYPE  SLIS_T_FIELDCAT_ALV,
       GT_SORT      TYPE  SLIS_T_SORTINFO_ALV,
       G_REPID      LIKE  SY-REPID,
       G_LAYOUT     TYPE  SLIS_LAYOUT_ALV,
       G_TITLE      TYPE  LVC_TITLE,
       LS_FIELDCAT  TYPE  SLIS_FIELDCAT_ALV,
       LS_SORTINFO  TYPE  SLIS_SORTINFO_ALV.

* User-command.
DATA  G_USER_COMMAND   TYPE SLIS_FORMNAME VALUE 'P2000_USER_COMMAND'.

DATA : V_COLUMN(30)      TYPE C,
       V_COUNT(2)        TYPE C,
       V_PRODS(1)        TYPE C,
       POS               TYPE I,
       V_ITAB(10)        TYPE C,
       V_LINE            TYPE I,
       V_DIV             TYPE I,
       V_PCNT            TYPE I,
       V_OLD_PCNT        TYPE I,
       V_CPCNT(3)        TYPE C.

DATA : W_VPRSV           LIKE MBEW-VPRSV,
       W_STPRS           LIKE MBEW-STPRS,
       W_VERPR           LIKE MBEW-VERPR,
       W_PEINH           LIKE MBEW-PEINH,
       W_WAERS           LIKE T001-WAERS.

RANGES : R_DUEDT FOR BSIK-ZFBDT OCCURS 0.

*-----------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_BUKRS  FOR    ZTCIVHD-BUKRS NO-EXTENSION
                                               NO INTERVALS,
                 S_REQNO  FOR    ZTCIVIT-ZFREQNO,
                 S_OPNNO  FOR    ZTREQST-ZFOPNNO,
                 S_OPNDT  FOR    ZTREQST-ZFOPNDT,
                 S_BLNO   FOR    ZTCIVIT-ZFBLNO,
                 S_HBLNO  FOR    ZTBL-ZFHBLNO,
                 S_BLDT   FOR    ZTBL-ZFBLDT,
                 S_EBELN  FOR    ZTCIVIT-EBELN,
                 S_CIVRN  FOR    ZTCIVHD-ZFCIVRN,
                 S_CIVNO  FOR    ZTCIVHD-ZFCIVNO,
                 S_CIDT   FOR    ZTCIVHD-ZFCIDT,     "INVOICE DATE
                 S_OPBN   FOR    ZTCIVHD-ZFOPBN,
                 S_ZTERM  FOR    EKKO-ZTERM,
                 S_REQTY  FOR    ZTCIVHD-ZFREQTY,
                 S_DUEDT  FOR    BSIK-ZFBDT  NO-EXTENSION,
                 S_MAVN   FOR    ZTCIVHD-ZFMAVN.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* EVENT  AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-LOW.
  PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
  PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-HIGH.

*-----------------------------------------------------------------------
* EVENT  INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.
   PERFORM  P1000_SET_BUKRS.
   PERFORM  P1000_TITLE.

*----------------------------------------------------------------------*
* EVENT  START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM   P1000_READ_CIV         USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.
    MESSAGE S738.  EXIT.
    EXIT.
  ENDIF.

  PERFORM   P1000_READ_DATA        USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.
    MESSAGE S738.  EXIT.
    EXIT.
  ENDIF.

  PERFORM P3000_WRITE_TEXT.

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CIV
*&---------------------------------------------------------------------*
FORM P1000_READ_CIV  USING    W_ERR_CHK.
  CLEAR : IT_TAB.
  SELECT  DISTINCT I~ZFREQNO   I~ZFBLNO    I~EBELN
            H~ZFCIVRN   H~ZFCIVNO   H~ZFIVAMC
            H~ZFIVAMT   H~ZFOPBN    H~ZTERM
                                    H~ZFMAVN
            H~ZFCIDT
         INTO  (IT_TAB-ZFREQNO, IT_TAB-ZFBLNO,   IT_TAB-EBELN,
                IT_TAB-ZFCIVRN, IT_TAB-ZFCIVNO,  IT_TAB-ZFIVAMC,
                IT_TAB-ZFIVAMT, IT_TAB-ZFOPBN,   IT_TAB-ZTERM,
                                                 IT_TAB-ZFMAVN,
                IT_TAB-ZFCIDT)
         FROM  ZTCIVHD AS H INNER JOIN ZTCIVIT AS I
           ON  H~ZFCIVRN    EQ    I~ZFCIVRN
        WHERE  H~ZFCIVRN    IN    S_CIVRN
          AND  H~ZFCIVNO    IN    S_CIVNO
          AND  H~ZFCIDT     IN    S_CIDT
          AND  H~ZFOPBN     IN    S_OPBN
          AND  H~ZTERM      IN    S_ZTERM
          AND  H~ZFMAVN     IN    S_MAVN
          AND  I~ZFREQNO    IN    S_REQNO
          AND  I~ZFBLNO     IN    S_BLNO
          AND  I~EBELN      IN    S_EBELN.

    COLLECT IT_TAB.
    CLEAR IT_TAB.
  ENDSELECT.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.
  ENDIF.
ENDFORM.                    " P1000_READ_CIV
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA USING    W_ERR_CHK.

  LOOP AT IT_TAB.

*>> Own Explanations for Terms of Payment Table Select..
    SELECT SINGLE TEXT1 INTO IT_TAB-TERM_NM
             FROM T052U
            WHERE SPRAS  EQ  SY-LANGU
              AND ZTERM  EQ  IT_TAB-ZTERM.

    MOVE : 'CIV' TO IT_TAB-ZFSTATUS.

*>> LC info.
    SELECT SINGLE ZFOPNNO ZFOPNDT ZFREQTY
      INTO (IT_TAB-ZFOPNNO, IT_TAB-ZFOPNDT, IT_TAB-ZFREQTY)
      FROM ZTREQST
     WHERE ZFREQNO  EQ   IT_TAB-ZFREQNO
       AND ZFOPNNO  IN   S_OPNNO
       AND ZFOPNDT  IN   S_OPNDT
       AND ZFREQTY  IN   S_REQTY
       AND ZFDOCST  EQ   'O'.

    IF SY-SUBRC  NE  0.
      DELETE IT_TAB INDEX SY-TABIX.
      CONTINUE.
    ENDIF.

*>> BL info.
    IF IT_TAB-ZFBLNO IS INITIAL.
      IF NOT S_HBLNO[] IS INITIAL OR
         NOT S_BLDT[]  IS INITIAL.
        DELETE IT_TAB INDEX SY-TABIX.
      ENDIF.
    ELSE.
      SELECT  SINGLE  ZFHBLNO   ZFBLDT  ZFMBLNO
        INTO  (IT_TAB-ZFHBLNO, IT_TAB-ZFBLDT, IT_TAB-ZFMBLNO)
        FROM  ZTBL
       WHERE  ZFBLNO    EQ  IT_TAB-ZFBLNO
         AND  ZFHBLNO   IN  S_HBLNO
         AND  ZFBLDT    IN  S_BLDT.

      IF SY-SUBRC  NE 0.
        DELETE IT_TAB INDEX SY-TABIX.
      ENDIF.
    ENDIF.

*>> Posted Invoice doc. info.
    SELECT SINGLE BELNR  GJAHR
      INTO (IT_TAB-BELNR, IT_TAB-GJAHR)
      FROM ZTCIVHST
     WHERE ZFCIVRN    EQ    IT_TAB-ZFCIVRN
       AND CGJAHR     EQ    SPACE
       AND CBELNR     EQ    SPACE.

    IF SY-SUBRC NE 0.
      IT_TAB-WRBTR   = 0.
    ELSE.
      MOVE : 'AP' TO IT_TAB-ZFSTATUS.

      CLEAR : IT_FIDOC.
      CONCATENATE IT_TAB-BELNR IT_TAB-GJAHR
                            INTO W_AWKEY.
*>> Read Account doc. no.
      SELECT BELNR GJAHR
        FROM BKPF
        INTO (W_BELNR, W_GJAHR)
       WHERE  AWTYP EQ 'RMRP'
         AND  AWKEY EQ  W_AWKEY.

       CLEAR : IT_FIDOC.
       MOVE  : W_BELNR   TO   IT_FIDOC-BELNR,
               W_GJAHR   TO   IT_FIDOC-GJAHR.
*>> read paid amount.
      SELECT SINGLE BUZEI ZFBDT ZBD1T ZBD2T ZBD3T
        INTO (IT_FIDOC-BUZEI, IT_FIDOC-ZFBDT,
              IT_FIDOC-ZBD1T, IT_FIDOC-ZBD2T, IT_FIDOC-ZBD3T)
        FROM BSIK
       WHERE BELNR    EQ    IT_FIDOC-BELNR
         AND GJAHR    EQ    IT_FIDOC-GJAHR
         AND BSCHL    EQ    '31'
         AND REBZG    EQ     SPACE.

*>> Document dosen't exist in BSIK is payment completed case.
      IF SY-SUBRC NE 0.
        SELECT SINGLE WRBTR AUGDT ZFBDT ZBD1T ZBD2T ZBD3T
          INTO (IT_FIDOC-WRBTR,  IT_FIDOC-BUDAT,
                IT_FIDOC-ZFBDT,  IT_FIDOC-ZBD1T,
                IT_FIDOC-ZBD2T,  IT_FIDOC-ZBD3T)
          FROM BSAK
         WHERE BELNR    EQ    IT_FIDOC-BELNR
           AND GJAHR    EQ    IT_FIDOC-GJAHR
           AND BSCHL    EQ    '31'
           AND REBZG    EQ     SPACE.
        IF SY-SUBRC EQ 0.
          MOVE : 'Cleared' TO IT_TAB-ZFSTATUS.
          EXIT.
        ENDIF.
      ELSE.

*>> Select "Accounting: Secondary Index for Vendors" Tables..
        SELECT SUM( WRBTR ) MAX( BUDAT )
          INTO (IT_FIDOC-WRBTR,  IT_FIDOC-BUDAT)
          FROM BSIK
         WHERE REBZG    EQ   IT_FIDOC-BELNR
           AND REBZJ    EQ   IT_FIDOC-GJAHR
           AND REBZZ    EQ   IT_FIDOC-BUZEI.
        IF IT_FIDOC-WRBTR GT 0.
          MOVE : 'Partial' TO IT_TAB-ZFSTATUS.
          EXIT.
        ENDIF.
      ENDIF.
      CLEAR : W_BELNR, W_GJAHR.
    ENDSELECT.
      MOVE : IT_FIDOC-BELNR TO IT_TAB-F_BELNR,
             IT_FIDOC-GJAHR TO IT_TAB-F_GJAHR,
             IT_FIDOC-WRBTR TO IT_TAB-WRBTR,
             IT_FIDOC-BUDAT TO IT_TAB-ZFPDDT.

*>>  Due Date 계산.
      CLEAR : IT_TAB-ZFDUEDT.
      IF NOT IT_FIDOC-ZBD3T IS INITIAL.
        IT_TAB-ZFDUEDT = IT_FIDOC-ZFBDT + IT_FIDOC-ZBD3T.
      ELSEIF NOT IT_FIDOC-ZBD2T IS INITIAL.
        IT_TAB-ZFDUEDT = IT_FIDOC-ZFBDT + IT_FIDOC-ZBD1T
                                        + IT_FIDOC-ZBD2T.
      ELSEIF NOT IT_FIDOC-ZBD1T IS INITIAL.
        IT_TAB-ZFDUEDT = IT_FIDOC-ZFBDT + IT_FIDOC-ZBD1T.
      ELSE.
        IT_TAB-ZFDUEDT = IT_FIDOC-ZFBDT.
      ENDIF.
    ENDIF.

    IT_TAB-BALANCE = IT_TAB-ZFIVAMT - IT_TAB-WRBTR.

*>> DUE DATE CHECK.
    IF NOT S_DUEDT[] IS INITIAL.
       R_DUEDT[] = S_DUEDT[].
       READ TABLE R_DUEDT.
       IF IT_TAB-BALANCE LT R_DUEDT-LOW.
          DELETE IT_TAB INDEX SY-TABIX.
          CONTINUE.
       ENDIF.
       IF NOT R_DUEDT-HIGH IS INITIAL
          AND IT_TAB-BALANCE GT R_DUEDT-HIGH.
          DELETE IT_TAB INDEX SY-TABIX.
          CONTINUE.
       ENDIF.
    ENDIF.

*>> Open Bank info.
    CLEAR : LFA1, IT_TAB-BKNAME.
    IF NOT IT_TAB-ZFOPBN IS INITIAL.
      CALL FUNCTION 'READ_LFA1'
           EXPORTING
                XLIFNR = IT_TAB-ZFOPBN
           IMPORTING
                XLFA1  = LFA1.

      MOVE LFA1-NAME1 TO IT_TAB-BKNAME.
    ENDIF.
*>> Beneficiary info.
    CLEAR : LFA1, IT_TAB-BNNAME.
    IF NOT IT_TAB-ZFMAVN IS INITIAL.
      CALL FUNCTION 'READ_LFA1'
           EXPORTING
                XLIFNR = IT_TAB-ZFMAVN
           IMPORTING
                XLFA1  = LFA1.

      MOVE LFA1-NAME1 TO IT_TAB-BNNAME.
    ENDIF.

    MODIFY IT_TAB INDEX SY-TABIX.

    CLEAR   : IT_TAB, IT_FIDOC.
    REFRESH : IT_FIDOC.

  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT LE 0.
    W_ERR_CHK = 'Y'.
  ENDIF.
ENDFORM.                    " P1000_READ_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_TEXT
*&---------------------------------------------------------------------*
FORM P3000_WRITE_TEXT.

  MOVE 'N' TO W_ERR_CHK.

  PERFORM P3000_APPEND_FIELDCAT.      " ALV Report TiTle.

  G_REPID = SY-REPID.

  PERFORM P3000_TITLE_WRITE CHANGING G_TITLE.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM      = G_REPID
            I_CALLBACK_USER_COMMAND = G_USER_COMMAND
            IS_LAYOUT               = G_LAYOUT
            I_GRID_TITLE            = G_TITLE
            I_SAVE                  = 'A'
            IT_SORT                 = GT_SORT[]
            IT_FIELDCAT             = GT_FIELDCAT[]
       TABLES
            T_OUTTAB                = IT_TAB
       EXCEPTIONS
            PROGRAM_ERROR           = 1
            OTHERS                  = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E977 WITH 'Error occured on displaying grid.'.
  ENDIF.

ENDFORM.                    " P3000_WRITE_TEXT
*&-------------------------------------------------------------------*
*&      Form  P3000_APPEND_FIELDCAT
*&-------------------------------------------------------------------*
FORM P3000_APPEND_FIELDCAT.

  CLEAR: GT_FIELDCAT, POS.
  PERFORM P2000_GT_FIELDCAT USING 'ZFCIVNO'
                                  'Invoice Number'
                                  ' '  ' '
                                   25.
  PERFORM P2000_GT_FIELDCAT USING 'ZFCIVRN'
                                  'CIV doc.no'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'ZFBLNO'
                                  'B/L doc.no'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'ZFHBLNO'
                                  'House B/L'
                                  ' '  ' '
                                   24.
  PERFORM P2000_GT_FIELDCAT USING 'ZFMBLNO'
                                  'Master B/L'
                                  ' '  ' '
                                   24.
  PERFORM P2000_GT_FIELDCAT USING 'ZFBLDT'
                                  'B/L date'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'ZFCIDT'
                                  'CIV date'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'EBELN'
                                  'P/O No.'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'ZFOPNNO'
                                  'Credit No'
                                  ' '  ' '
                                   25.
  PERFORM P2000_GT_FIELDCAT USING 'ZFREQNO'
                                  'Req.doc.no'
                                   ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'ZFOPNDT'
                                  'Open date'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'ZFIVAMC'
                                  'Curr.'
                                  ' '  ' '
                                   5.
  PERFORM P2000_GT_FIELDCAT USING 'ZFIVAMT'
                                  'Invoice Amount'
                                  'C'  'ZFIVAMC'
                                   19.
  PERFORM P2000_GT_FIELDCAT USING 'ZFOPBN'
                                  'Bank code'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'BKNAME'
                                  'Opening Bank Name'
                                  ' '  ' '
                                   30.
  PERFORM P2000_GT_FIELDCAT USING 'ZTERM'
                                  'Pytm'
                                  ' '  ' '
                                   4.
  PERFORM P2000_GT_FIELDCAT USING 'TERM_NM'
                                  'Payment terms discription'
                                  ' ' ' '
                                   50.
  PERFORM P2000_GT_FIELDCAT USING 'ZFREQTY'
                                  'TY'
                                  ' '  ' '
                                   2.
  PERFORM P2000_GT_FIELDCAT USING 'WRBTR'
                                  'Paid Amount'
                                  'C'  'ZFIVAMC'
                                   16.
  PERFORM P2000_GT_FIELDCAT USING 'BALANCE'
                                  'Balacne'
                                  'C'  'ZFIVAMC'
                                   16.
  PERFORM P2000_GT_FIELDCAT USING 'ZFDUEDT'
                                  'Due date'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'ZFSTATUS'
                                  'Status'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'ZFPDDT'
                                  'Paid date'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'ZFMAVN'
                                  'Benef.code'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'BNNAME'
                                  'Beneficiary Name'
                                  ' '  ' '
                                   30.
  PERFORM P2000_GT_FIELDCAT USING 'BELNR'
                                  'IV doc.'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'GJAHR'
                                  'year'
                                  ' '  ' '
                                   4.
  PERFORM P2000_GT_FIELDCAT USING 'F_BELNR'
                                  'FI doc.'
                                  ' '  ' '
                                   10.
  PERFORM P2000_GT_FIELDCAT USING 'F_GJAHR'
                                  'year'
                                  ' '  ' '
                                   4.
* Sub Total.
  CLEAR GT_SORT.

ENDFORM.                    " P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  P2000_GT_FIELDCAT
*&---------------------------------------------------------------------*
FORM P2000_GT_FIELDCAT USING    P_FIELDNAME   P_TITLE
                                P_GUBUN       P_GBFIELDNAME
                                P_LENGTH.

  CLEAR LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = P_FIELDNAME.
  LS_FIELDCAT-SELTEXT_M      = P_TITLE.
  LS_FIELDCAT-OUTPUTLEN      = P_LENGTH.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  IF POS EQ 1.
    LS_FIELDCAT-KEY         = 'X'.
  ENDIF.
  IF P_GUBUN EQ 'Q'.
    LS_FIELDCAT-QFIELDNAME  = P_GBFIELDNAME.
    LS_FIELDCAT-DO_SUM      = 'X'.
  ELSEIF P_GUBUN EQ 'C'.
    LS_FIELDCAT-CFIELDNAME  = P_GBFIELDNAME.
    LS_FIELDCAT-DO_SUM      = 'X'.
  ENDIF.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " P2000_GT_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  P2000_GT_SORT
*&---------------------------------------------------------------------*
FORM P2000_GT_SORT USING    P_GUBUN  P_SPOS  P_FIELDNAME.

  CLEAR LS_SORTINFO.
  LS_SORTINFO-SPOS      = P_SPOS.
  LS_SORTINFO-FIELDNAME = P_FIELDNAME.
  LS_SORTINFO-TABNAME   = 'IT_TAB'.
  LS_SORTINFO-UP        = 'X'.
  IF NOT P_GUBUN IS INITIAL.
    LS_SORTINFO-SUBTOT    = 'X'.
    LS_SORTINFO-GROUP     = '*'.
  ENDIF.
  APPEND LS_SORTINFO TO GT_SORT.

ENDFORM.                    " P2000_GT_SORT
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM P2000_USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM
                              RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN '&IC1'.                       "doubleclick
      READ TABLE IT_TAB INDEX RS_SELFIELD-TABINDEX. "cursorposit.
      IF SY-SUBRC EQ 0.
        CASE RS_SELFIELD-SEL_TAB_FIELD.
*>>     Import request.
          WHEN '1-ZFREQNO' OR '1-ZFOPNNO'.
            IF IT_TAB-ZFREQNO IS INITIAL.
              MESSAGE E018.
            ENDIF.
            SET PARAMETER ID 'ZPOPNNO'  FIELD ''.
            SET PARAMETER ID 'BES'      FIELD ''.
            SET PARAMETER ID 'ZPREQNO'  FIELD IT_TAB-ZFREQNO.
            CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
*>>     Bill of loading
          WHEN '1-ZFHBLNO' OR '1-ZFBLNO'.
            IF IT_TAB-ZFBLNO IS INITIAL.
              MESSAGE E038.
            ENDIF.
            SET PARAMETER ID 'ZPHBLNO'  FIELD ''.
            SET PARAMETER ID 'ZPBLNO'   FIELD IT_TAB-ZFBLNO.
            CALL TRANSACTION 'ZIM23'   AND SKIP FIRST SCREEN.
*>>     Purchase Order.
          WHEN '1-EBELN'.
            IF IT_TAB-EBELN IS INITIAL.
              MESSAGE E003.
            ENDIF.

            SELECT SINGLE * FROM EKKO
                   WHERE    EBELN  EQ   IT_TAB-EBELN.

            CASE EKKO-BSTYP.
              WHEN  'L'.   ">Delivery
                SET PARAMETER ID 'SAG' FIELD IT_TAB-EBELN.
                CALL TRANSACTION 'ME33L' AND SKIP  FIRST SCREEN.
              WHEN  'K'.   ">Contract
                SET PARAMETER ID 'CTR' FIELD IT_TAB-EBELN.
                CALL TRANSACTION 'ME33K' AND SKIP  FIRST SCREEN.
              WHEN  OTHERS.
                SET PARAMETER ID 'BSP' FIELD ''.
                SET PARAMETER ID 'BES' FIELD IT_TAB-EBELN.
                CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
            ENDCASE.
*>>     Commercial invoice.
          WHEN '1-ZFCIVRN' OR '1-ZFCIVNO'.
            IF IT_TAB-ZFCIVRN IS INITIAL.
              MESSAGE E374.
            ENDIF.
            SET  PARAMETER ID 'ZPCIVRN'   FIELD IT_TAB-ZFCIVRN.
            SET  PARAMETER ID 'ZPCIVNO'   FIELD ''.
            CALL TRANSACTION  'ZIM37'   AND   SKIP FIRST SCREEN.
*>>     Open Bank.
          WHEN '1-ZFOPBN' OR '1-BKNAME'.
            IF IT_TAB-ZFOPBN IS INITIAL.
              MESSAGE E136.
            ENDIF.
            SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
            SET PARAMETER ID 'LIF' FIELD IT_TAB-ZFOPBN.
            SET PARAMETER ID 'EKO' FIELD ''.
            CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.
*>>     Beneficiary
          WHEN '1-ZFMAVN' OR '1-BNNAME'.
            IF IT_TAB-ZFMAVN IS INITIAL.
              MESSAGE E136.
            ENDIF.
            SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
            SET PARAMETER ID 'LIF' FIELD IT_TAB-ZFMAVN.
            SET PARAMETER ID 'EKO' FIELD ''.

            CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.
*>>     Invoice document.
          WHEN '1-BELNR' OR '1-GJAHR'.
            IF IT_TAB-BELNR IS INITIAL.
              MESSAGE E977 WITH 'Invoice doc. does not exist.'.
            ENDIF.
            SET  PARAMETER ID  'RBN'   FIELD   IT_TAB-BELNR .
            SET  PARAMETER ID  'GJR'   FIELD   IT_TAB-GJAHR.
            CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
*>>      Following doc. (Accounting doc.)
          WHEN '1-F_BELNR' OR '1-F_GJAHR'.
            IF IT_TAB-F_BELNR IS INITIAL.
              MESSAGE E977 WITH 'Accounting doc. does not exist.'.
            ENDIF.
            SET  PARAMETER ID  'BUK'   FIELD   'H201'.
            SET  PARAMETER ID  'BLN'   FIELD   IT_TAB-F_BELNR.
            SET  PARAMETER ID  'GJR'   FIELD   IT_TAB-F_GJAHR.
            CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
          WHEN OTHERS.
        ENDCASE.
      ELSE.
        MESSAGE S962.
      ENDIF.
      CLEAR R_UCOMM.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE CHANGING P_TITLE.

  MOVE ' [  Import result List  ]  '   TO P_TITLE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
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
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> 회사코드 SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P1000_TITLE
*&---------------------------------------------------------------------*
FORM P1000_TITLE.

  SET TITLEBAR 'TITLE' WITH 'Import result List(C/I base)'.

ENDFORM.                    " P1000_TITLE
