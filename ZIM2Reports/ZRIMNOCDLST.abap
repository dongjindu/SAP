*&---------------------------------------------------------------------*
*& Report  ZRIMNOCCLST                                                 *
*&---------------------------------------------------------------------*
*&  Program Name : Object list to Declare Customs                      *
*&  Created On   : Na Hyun Joo                                         *
*&  Created by   : 10.24.2003                                          *
*&---------------------------------------------------------------------*
*& [Change History]
*&---------------------------------------------------------------------*
REPORT  ZRIMNOCDLST  MESSAGE-ID ZIM
                     LINE-SIZE 137
                     NO STANDARD PAGE HEADING.

TABLES : ZTBL,                 " B/L Header Table
         ZTBLIT,               " B/L Item Table
         ZTIV,                 " Customs Clearance Request Header Table
         ZTIVIT,               " Customs Clearance Request Item Table
         ZTIMIMG00,            " Basic Configuration.
         ZTIMIMG08,            " Other Code Management Table.
         ZTIDRUS.              " Customs Declaration.

*-----------------------------------------------------------------------
* Internal Table ( List )
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFBLNO      LIKE  ZTBL-ZFBLNO,             " B/L Document No
       ZFBLIT      LIKE  ZTBLIT-ZFBLIT,           " B/L Docuemnt Item
       ZFHBLNO     LIKE  ZTBL-ZFHBLNO,            " House B/L Number
       ZFAPRT      LIKE  ZTBL-ZFAPRT,             " Arrival Port
       ZFETD       LIKE  ZTBL-ZFETD,              " Expected time of De
       ZFRPTTY     LIKE  ZTIV-ZFRPTTY,            " Declaration Type
       RPTY_NM(15) TYPE  C,                       " Declaration Type Txt
       ZFPONC      LIKE  ZTIV-ZFPONC,             " Import Transaction
       PONC_NM(15) TYPE  C,                       " Transaction Text
       ZFCCDT      LIKE  ZTIV-ZFCCDT,             " Clearance Request
       ZFIVNO      LIKE  ZTIV-ZFIVNO,             " Invoice No
       ZFIVDNO     LIKE  ZTIVIT-ZFIVDNO,          " Invoice Item No
       MATNR       LIKE  ZTIVIT-MATNR,            " Material Code
       TXZ01       LIKE  ZTIVIT-TXZ01,            " Material Text
       EBELN       LIKE  ZTIVIT-EBELN,            " P/O
       EBELP       LIKE  ZTIVIT-EBELP,            " P/O Item
       BLMENGE     LIKE  ZTBLIT-BLMENGE,          " B/L Quantity
       CCMENGE     LIKE  ZTIVIT-CCMENGE,          " Clearance Quantity
       MEINS       LIKE  ZTIVIT-MEINS,            " Unit
       ERNAM       LIKE  ZTIV-ERNAM.              " In charge of Clea.
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
        P_BUKRS   LIKE  ZTBL-BUKRS.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR  ZTBL-BUKRS  NO-EXTENSION
                                              NO INTERVALS,
                   S_MATNR   FOR  ZTBLIT-MATNR,
                   S_EKORG   FOR  ZTBL-EKORG,
                   S_EKGRP   FOR  ZTBL-EKGRP,
                   S_WERKS   FOR  ZTBL-ZFWERKS,
                   S_ETD     FOR  ZTBL-ZFETD,
                   S_ETA     FOR  ZTBL-ZFETA,
                   S_ERNAM   FOR  ZTIV-ERNAM,
                   S_CCDT    FOR  ZTIV-ZFCCDT,
                   S_CDAT    FOR  ZTBL-CDAT,
                   S_EBELN   FOR  ZTBLIT-EBELN.

SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
   PERFORM   P1000_SET_BUKRS.
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.

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
        PERFORM P2000_DISP_ZTIDR USING IT_TAB-ZFIVNO.
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

     SKIP 2.
     FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
     WRITE : /40  ' [ Object List to Declare  ] '
                  COLOR COL_HEADING INTENSIFIED OFF.
     WRITE : /'Date : ', SY-DATUM.
     WRITE : / SY-ULINE.
     FORMAT COLOR COL_HEADING INTENSIFIED ON.
     WRITE:/ SY-VLINE,(20) 'House B/L No' ,
             SY-VLINE,(25) 'Port of Arriving',
             SY-VLINE,(10) 'E.T.D.',
             SY-VLINE,(15) 'Clearance Type',
             SY-VLINE,(15) 'Entry Type',
             SY-VLINE,(15) 'Cl.Req.date',
             SY-VLINE,(15) 'In Charge',
             SY-VLINE.
     FORMAT COLOR COL_HEADING INTENSIFIED OFF.
     WRITE:/ SY-VLINE,(20) 'Purchase Order No',
             SY-VLINE,(38) 'Material',
             SY-VLINE,(33) 'B/L Quantity',
             SY-VLINE,(33) 'Clearance Request Quantity',
             SY-VLINE.
     FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

   W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET TITLEBAR  'ZIMR70'.
   SET PF-STATUS 'ZIMR70'.

   W_COUNT = 0.

   SORT IT_TAB BY ZFBLNO ZFIVNO ZFCCDT.

   CLEAR : W_ZFIVNO.

   LOOP AT IT_TAB.
      IF W_ZFIVNO NE IT_TAB-ZFIVNO.
         PERFORM  P3000_HEADER_WRITE.
      ENDIF.
      PERFORM P3000_LINE_WRITE.
      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.
      MOVE  IT_TAB-ZFIVNO  TO  W_ZFIVNO.
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
  PERFORM   P3000_TITLE_WRITE.

* Report Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  WRITE : / SY-ULINE.
  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / 'Total', W_COUNT, 'Case'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE,(10)IT_TAB-EBELN,         " Purchase Order
                   (01)'-'          ,
                   (07)IT_TAB-EBELP,
          SY-VLINE,(16)IT_TAB-MATNR,
                   (21)IT_TAB-TXZ01,
          SY-VLINE,(27)IT_TAB-BLMENGE UNIT IT_TAB-MEINS,
                   (05)IT_TAB-MEINS,
          SY-VLINE,(27)IT_TAB-CCMENGE UNIT IT_TAB-MEINS,
                   (05)IT_TAB-MEINS,
          SY-VLINE.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_TAB USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting
  REFRESH : IT_TAB.

  SELECT A~ZFIVNO  B~ZFIVDNO B~MATNR  B~CCMENGE A~ZFRPTTY
         A~ZFPONC  B~TXZ01   B~MEINS  A~ERNAM   A~ZFCCDT
         B~ZFBLNO  B~ZFBLIT  B~EBELN  B~EBELP
  INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM   ZTIV  AS  A INNER JOIN ZTIVIT AS B
  ON     A~ZFIVNO    EQ    B~ZFIVNO
  WHERE  A~ZFCUST    EQ    '2'
  AND    A~BUKRS     IN    S_BUKRS
  AND    A~ZFCCDT    IN    S_CCDT
  AND    A~ERNAM     IN    S_ERNAM
  AND    B~EBELN     IN    S_EBELN
  AND    B~MATNR     IN    S_MATNR
  AND    B~WERKS     IN    S_WERKS.

  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'.  ENDIF.

  CLEAR : IT_TAB.
  LOOP  AT  IT_TAB.

     W_TABIX  =  SY-TABIX.

     CLEAR : ZTBL, ZTBLIT, IT_TAB-BLMENGE.
     SELECT SINGLE * FROM ZTBL
     WHERE  ZFETA    IN   S_ETA
     AND    ZFETD    IN   S_ETD
     AND    CDAT     IN   S_CDAT
     AND    EKGRP    IN   S_EKGRP
     AND    EKORG    IN   S_EKORG
     AND    ZFBLNO   EQ   IT_TAB-ZFBLNO.

     IF SY-SUBRC NE 0.
        DELETE  IT_TAB  INDEX  W_TABIX.
        CONTINUE.
     ENDIF.

     MOVE : ZTBL-ZFHBLNO    TO  IT_TAB-ZFHBLNO,
            ZTBL-ZFAPRT     TO  IT_TAB-ZFAPRT,
            ZTBL-ZFETD      TO  IT_TAB-ZFETD.

     SELECT SINGLE BLMENGE  INTO IT_TAB-BLMENGE
     FROM   ZTBLIT
     WHERE  ZFBLNO   EQ  IT_TAB-ZFBLNO
     AND    ZFBLIT   EQ  IT_TAB-ZFBLIT.

     " Clearance Type Text Get.
     PERFORM  P4000_DD_TEXT    USING 'ZDRPTTY'
                                     IT_TAB-ZFRPTTY
                            CHANGING IT_TAB-RPTY_NM.

     " Etry Type Text Get.
     PERFORM  P4000_PONC_TEXT  USING IT_TAB-ZFPONC
                            CHANGING IT_TAB-PONC_NM.

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
  WRITE:/ SY-VLINE,(20) IT_TAB-ZFHBLNO,    " House B/L No.
          SY-VLINE,(25) IT_TAB-ZFAPRT,     " Arriving Port
          SY-VLINE,(10) IT_TAB-ZFETD,      " ETD
          SY-VLINE,(15) IT_TAB-RPTY_NM,    " Clearance Type Text
          SY-VLINE,(15) IT_TAB-PONC_NM,    " Entry Type Text
          SY-VLINE,(15) IT_TAB-ZFCCDT,     " Clearance Request Date
          SY-VLINE,(15) IT_TAB-ERNAM,      " In Charge of Clearance
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
FORM P2000_DISP_ZTIDR USING    P_ZFIVNO.

  SELECT SINGLE * FROM ZTIDRUS
  WHERE  ZFIVNO   EQ   P_ZFIVNO.

   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   SET PARAMETER ID 'ZPBLNO'  FIELD ZTIDRUS-ZFBLNO.
   SET PARAMETER ID 'ZPCLSEQ' FIELD ZTIDRUS-ZFCLSEQ.

   CALL TRANSACTION 'ZIMCD2'  AND SKIP  FIRST SCREEN.

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
