*&---------------------------------------------------------------------*
*& Report  ZRIMINDECL                                                  *
*&---------------------------------------------------------------------*
*&  Program Name : Object list to Send Customs Broker                  *
*&  Created On   : Na Hyun Joo                                         *
*&  Created by   : 10.28.2003                                          *
*&---------------------------------------------------------------------*
*& [Change History]
*&---------------------------------------------------------------------*
REPORT  ZRIMNINDECL  MESSAGE-ID ZIM
                     LINE-SIZE 115
                     NO STANDARD PAGE HEADING.

TABLES : ZTBL,                 " B/L Header Table
         ZTBLIT,               " B/L Item Table
         ZTIV,                 " Customs Clearance Request Header Table
         ZTIVIT,               " Customs Clearance Request Item Table
         ZTIMIMG00,            " Basic Configuration.
         ZTIMIMG08,            " Other Code Management Table.
         ZTIDRUS,              " Customs Declaration.
         ZTIDRUSH,             " Customs Declaration HS Data
         ZTIDRUSD,             " Customs Declaration HS Detail.
         LFA1.                 " Vendor Master

*-----------------------------------------------------------------------
* Internal Table ( List )
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFIVNO      LIKE  ZTIDRUS-ZFIVNO,          " I/V No
       ZFBLNO      LIKE  ZTIDRUS-ZFBLNO,          " B/L Document No
       ZFCLSEQ     LIKE  ZTIDRUS-ZFCLSEQ,         " Clearance Sequence
       ZFEEDT      LIKE  ZTIDRUS-ZFEEDT,          " Estimated Entry Date
       ZFHBLNO     LIKE  ZTIDRUS-ZFHBLNO,         " House B/L
       ZFORIG      LIKE  ZTIDRUS-ZFORIG,          " Country of origin
       ZFCAC       LIKE  ZTIDRUS-ZFCAC,           " Country of export
       ZFENDT      LIKE  ZTIDRUS-ZFENDT,          " Import Date
       ZFCTW       LIKE  ZTIDRUS-ZFCTW,           " Customs Broker
       ZFENPT      LIKE  ZTIDRUS-ZFENPT,          " Entry Port
       ZFTRMET     LIKE  ZTIDRUS-ZFTRMET,         " Transportation
       ZFVIA(6)    TYPE  C,                       " Transportation
       ZFIVAMT     LIKE  ZTIDRUS-ZFIVAMT,         " Total Amount
       ZFIVAMC     LIKE  ZTIDRUS-ZFIVAMC,         " Currency
       STAWN       LIKE  ZTIDRUSH-STAWN,          " HS CODE
       ZFGDNM      LIKE  ZTIDRUSH-ZFGDNM,         " HS Code Name
       ZFCURT      LIKE  ZTIDRUSH-ZFCURT,         " Tariff rate
       ZFMPRT      LIKE  ZTIDRUSH-ZFMPRT,         " M.P.F. Rate
       ZFHMRT      LIKE  ZTIDRUSH-ZFHMRT,         " H.M.F. Rate
       ZFHSAM      LIKE  ZTIDRUSH-ZFHSAM,         " HS Amount
       WAERS       LIKE  ZTIDRUSH-WAERS.          " Currency.
DATA : END OF IT_TAB.
*-----------------------------------------------------------------------
* Include & Variance Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.

DATA :  W_ERR_CHK TYPE  C,
        W_TABIX   LIKE  SY-TABIX,
        W_COUNT   TYPE  I,
        W_ZFIVNO  LIKE  ZTIV-ZFIVNO,
        W_PAGE    TYPE  I,
        W_LINE    TYPE  I,
        W_NAME    LIKE  LFA1-NAME1,
        P_BUKRS   LIKE  ZTBL-BUKRS,
        W_ZFCTW   LIKE  ZTIDRUS-ZFCTW.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR  ZTBL-BUKRS  NO-EXTENSION
                                              NO INTERVALS,
                   S_EKGRP   FOR  ZTBL-EKGRP,
                   S_EKORG   FOR  ZTBL-EKORG,
                   S_VIA     FOR  ZTBL-ZFVIA,
                   S_CTW     FOR  ZTIDRUS-ZFCTW,
                   S_EMDT    FOR  ZTIDRUS-ZFEEDT,
                   S_IMDT    FOR  ZTIDRUS-ZFENDT,
                   S_HBLNO   FOR  ZTIDRUS-ZFHBLNO.

SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER 초기값 Setting
INITIALIZATION.                                 " 초기값 SETTING
   PERFORM   P1000_SET_BUKRS.
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Data retrieval
   PERFORM   P1000_GET_IT_TAB     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.   MESSAGE S738.   EXIT.   ENDIF.

* Report Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.   MESSAGE S738.   EXIT.   ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
   CASE SY-UCOMM.
     WHEN 'REFR'.
        PERFORM   P1000_GET_IT_TAB    USING   W_ERR_CHK.
        IF W_ERR_CHK EQ 'Y'.  EXIT.   ENDIF.
        PERFORM RESET_LIST.
     WHEN 'DISP'.                       " Display B/L
        IF IT_TAB-ZFBLNO IS INITIAL.
           MESSAGE S951. EXIT.
        ENDIF.
        PERFORM P2000_DISP_ZTBL USING IT_TAB-ZFBLNO.
     WHEN 'DIIV'.                       " Display I/V.
        IF IT_TAB-ZFIVNO IS INITIAL.
           MESSAGE S951. EXIT.
        ENDIF.
        PERFORM P2000_DISP_ZTIV USING IT_TAB-ZFIVNO.
     WHEN 'DICD'.                       " Customs Declaration
        IF IT_TAB-ZFIVNO IS INITIAL.
           MESSAGE S951. EXIT.
        ENDIF.
        PERFORM P2000_DISP_ZTIDR USING IT_TAB-ZFBLNO
                                       IT_TAB-ZFCLSEQ.
     WHEN 'DICC'.                       " Customs Clearance
        IF IT_TAB-ZFIVNO IS INITIAL.
           MESSAGE S951. EXIT.
        ENDIF.
        PERFORM P2000_DISP_ZTIDS USING IT_TAB-ZFBLNO
                                       IT_TAB-ZFCLSEQ.
        PERFORM P1000_GET_IT_TAB USING W_ERR_CHK.
        IF W_ERR_CHK EQ 'Y'.
           LEAVE TO SCREEN 0.
        ELSE.
           PERFORM RESET_LIST.
        ENDIF.
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

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR70' .

ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

     " Customs Broker Name Get.
     SELECT SINGLE B~NAME1 INTO W_NAME
     FROM   ZTIMIMG10 AS A INNER JOIN LFA1 AS B
     ON     A~ZFVEN        EQ    B~LIFNR
     WHERE  A~ZFCUT        EQ    IT_TAB-ZFCTW.

     SKIP 2.
     FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
     WRITE : /40  ' [ Object List to Send Customs Broker  ] '
                  COLOR COL_HEADING INTENSIFIED OFF.
     SKIP 2.
     WRITE : /'Broker : ', IT_TAB-ZFCTW, W_NAME,
            90 'DATE : ',
            99 SY-DATUM.

     WRITE : / SY-ULINE.
     FORMAT COLOR COL_HEADING INTENSIFIED ON.
     WRITE:/ SY-VLINE,(10) 'E.Entry.Dt' ,
             SY-VLINE,(20) 'House B/L',
             SY-VLINE,(10) 'Origin',
             SY-VLINE,(10) 'Export',
             SY-VLINE,(10) 'Import Dt',
             SY-VLINE,(10) 'Ves/Voy',
             SY-VLINE,(23) 'Total Value',
             SY-VLINE.
     FORMAT COLOR COL_HEADING INTENSIFIED OFF.
     WRITE:/ SY-VLINE,(10) 'TSUSA No',
             SY-VLINE,(20) 'General Description',
             SY-VLINE,(14) 'Tariff Rate',
             SY-VLINE,(14) 'M.P.F. Rate',
             SY-VLINE,(15) 'H.M.F. Rate',
             SY-VLINE,(23) 'Invoice Value',
             SY-VLINE.
     FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

   W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'B/L 관리 트랜잭션'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET TITLEBAR  'ZIMR70'.
   SET PF-STATUS 'ZIMR70'.

   W_COUNT = 0.

   SORT IT_TAB BY ZFIVNO ZFEEDT.

   CLEAR : W_ZFIVNO, W_ZFCTW.

   LOOP AT IT_TAB.

      IF W_ZFCTW  NE IT_TAB-ZFCTW AND SY-TABIX NE 1.
         PERFORM P3000_LAST_WRITE.
         NEW-PAGE.
         CLEAR : W_ZFIVNO.
      ENDIF.
      IF W_ZFIVNO NE IT_TAB-ZFIVNO.
         PERFORM  P3000_HEADER_WRITE.
      ENDIF.
      PERFORM P3000_LINE_WRITE.
      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.
      MOVE : IT_TAB-ZFIVNO  TO  W_ZFIVNO,
             IT_TAB-ZFCTW   TO  W_ZFCTW.
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
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST

*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*FORM P2000_MULTI_SELECTION.
*
*  DATA: INDEX   TYPE P,
*        ZFREQNO LIKE ZTREQST-ZFREQNO,
*        ZFAMDNO LIKE ZTREQST-ZFAMDNO,
*        ZFRLST1 LIKE ZTREQST-ZFRLST1,
*        ZFRLST2 LIKE ZTREQST-ZFRLST2.
*
*  REFRESH IT_SELECTED.
*  CLEAR W_SELECTED_LINES.
*
*  MOVE : W_LIST_INDEX    TO INDEX,
*         IT_TAB-ZFBLNO   TO ZFREQNO.
*
*  DO.
*    CLEAR MARKFIELD.
*    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
*    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
*    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
*       MOVE : IT_TAB-ZFBLNO  TO IT_SELECTED-ZFREQNO.
**
*      APPEND IT_SELECTED.
*      ADD 1 TO W_SELECTED_LINES.
*    ENDIF.
*  ENDDO.
*
*  IF W_SELECTED_LINES EQ 0.
*    IF INDEX GT 0.
*      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO.
*
*      APPEND IT_SELECTED.
*      ADD 1 TO W_SELECTED_LINES.
*    ELSE.
*      MESSAGE S962.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  WRITE : / SY-ULINE.
  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / 'Total', W_COUNT, 'Case'.
  ENDIF.
  CLEAR : W_COUNT.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE,(10)IT_TAB-STAWN,         " HS Code
          SY-VLINE,(20)IT_TAB-ZFGDNM,        " HS Name
          SY-VLINE,(14)IT_TAB-ZFCURT,        " Tariff Rate
          SY-VLINE,(14)IT_TAB-ZFMPRT,        " M.P.F. Rate
          SY-VLINE,(15)IT_TAB-ZFHMRT,        " H.M.F. Rate
          SY-VLINE,(18)IT_TAB-ZFHSAM CURRENCY
                       IT_TAB-WAERS  NO-GAP,
                   (05)IT_TAB-WAERS,
          SY-VLINE.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_TAB USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting
  REFRESH : IT_TAB.

  SELECT A~ZFBLNO  A~ZFCLSEQ  A~ZFHBLNO  A~ZFORIG  A~ZFCAC
         A~ZFENDT  A~ZFENPT   A~ZFEEDT   A~ZFTRMET A~ZFIVAMT
         A~ZFIVAMC B~STAWN    B~ZFGDNM   B~ZFCURT  B~ZFMPRT
         B~ZFHMRT  B~ZFHSAM   B~WAERS    A~ZFIVNO  A~ZFCTW
  INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM  ( ZTIDRUS AS A INNER JOIN ZTIDRUSH AS B
  ON     A~ZFBLNO      EQ    B~ZFBLNO
  AND    A~ZFCLSEQ     EQ    B~ZFCLSEQ   )
  INNER  JOIN  ZTIV    AS    D
  ON     A~ZFIVNO      EQ    D~ZFIVNO
  WHERE  D~ZFCUST      EQ    '3'
  AND    A~BUKRS       IN    S_BUKRS
  AND    A~ZFCTW       IN    S_CTW
  AND    A~ZFEEDT      IN    S_EMDT
  AND    A~ZFENDT      IN    S_IMDT
  AND    A~ZFHBLNO     IN    S_HBLNO .

  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'.  ENDIF.

  CLEAR : IT_TAB.
  LOOP  AT  IT_TAB.

     W_TABIX  =  SY-TABIX.

     CLEAR : ZTBL.
     SELECT SINGLE * FROM ZTBL
     WHERE  EKGRP    IN   S_EKGRP
     AND    EKORG    IN   S_EKORG
     AND    ZFVIA    IN   S_VIA
     AND    ZFBLNO   EQ   IT_TAB-ZFBLNO.

     IF SY-SUBRC NE 0.
        DELETE  IT_TAB  INDEX  W_TABIX.
        CONTINUE.
     ENDIF.
     IF IT_TAB-ZFTRMET(1) EQ '1'.
        MOVE  'Vessel'  TO  IT_TAB-ZFVIA.
     ELSEIF IT_TAB-ZFTRMET(1) EQ '4'.
        MOVE  'Voyage'  TO  IT_TAB-ZFVIA.
     ENDIF.

     MODIFY IT_TAB  INDEX  W_TABIX.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE EQ 0. W_ERR_CHK = 'Y'.  ENDIF.

ENDFORM.                    " P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE  USING VALUE(P_TITLE)
                                VALUE(P_QUESTION)
                                VALUE(P_BUTTON1)
                                VALUE(P_BUTTON2)
                                VALUE(P_DEFAULT)
                          CHANGING    P_ANSWER.

   CLEAR : P_ANSWER.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = P_TITLE
             DIAGNOSE_OBJECT = ''
             TEXT_QUESTION   = P_QUESTION
             TEXT_BUTTON_1   = P_BUTTON1
             TEXT_BUTTON_2   = P_BUTTON2
             DEFAULT_BUTTON  = P_DEFAULT
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  P_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE
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
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTBL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SELECTED_ZFBLNO  text
*----------------------------------------------------------------------*
FORM P2000_DISP_ZTBL USING    P_ZFBLNO.

   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
   SET PARAMETER ID 'BES'     FIELD ' '.

   CALL TRANSACTION 'ZIM23'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTBL
*&---------------------------------------------------------------------*
*&      Form  P3000_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM P3000_HEADER_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE:/ SY-ULINE.
  WRITE:/ SY-VLINE,(10) IT_TAB-ZFEEDT,     " Estimated Entry Date
          SY-VLINE,(20) IT_TAB-ZFHBLNO,    " House B/L
          SY-VLINE,(10) IT_TAB-ZFORIG,     " Country of Origin
          SY-VLINE,(10) IT_TAB-ZFCAC,      " Country of Export
          SY-VLINE,(10) IT_TAB-ZFENDT,     " Entry Date
          SY-VLINE,(10) IT_TAB-ZFVIA,      " Transportation
          SY-VLINE,(18) IT_TAB-ZFIVAMT CURRENCY
                        IT_TAB-ZFIVAMC NO-GAP,
                   (05) IT_TAB-ZFIVAMC,    " Total Amount
          SY-VLINE.
  HIDE: IT_TAB.
  W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_HEADER_WRITE

*&---------------------------------------------------------------------*
*&      Form  P4000_DD_TEXT
*&---------------------------------------------------------------------*
FORM P4000_DD_TEXT USING    P_DOMAIN
                            P_VALUE
                   CHANGING P_TEXT.

   SELECT SINGLE DDTEXT INTO P_TEXT
   FROM   DD07T
   WHERE  DOMNAME       EQ   P_DOMAIN
   AND    DDLANGUAGE    EQ   SY-LANGU
   AND    DOMVALUE_L    EQ   P_VALUE.

ENDFORM.                    " P4000_DD_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIV
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTIV USING    P_ZFIVNO.

   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   SET PARAMETER ID 'ZPBLNO'  FIELD ''.
   SET PARAMETER ID 'ZPIVNO'  FIELD P_ZFIVNO.

   CALL TRANSACTION 'ZIM33'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIV
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIDR
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTIDR USING    P_ZFBLNO  P_ZFCLSEQ.

   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
   SET PARAMETER ID 'ZPCLSEQ' FIELD P_ZFCLSEQ.

   CALL TRANSACTION 'ZIMCD3'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIDR
*&---------------------------------------------------------------------*
*&      Form  P4000_PONC_TEXT
*&---------------------------------------------------------------------*
FORM P4000_PONC_TEXT USING    P_ZFPONC
                     CHANGING P_NM.

   CLEAR : ZTIMIMG08.
   SELECT SINGLE ZFCDNM  INTO P_NM
   FROM   ZTIMIMG08
   WHERE  ZFCDTY   EQ   '001'
   AND    ZFCD     EQ   P_ZFPONC.

ENDFORM.                    " P4000_PONC_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIDS
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTIDS USING    P_ZFBLNO  P_ZFCLSEQ.

   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   SET PARAMETER ID 'ZPBLNO'  FIELD IT_TAB-ZFBLNO.
   SET PARAMETER ID 'ZPCLSEQ' FIELD IT_TAB-ZFCLSEQ.

   CALL TRANSACTION 'ZIMCC1'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIDS
