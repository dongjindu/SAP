*&---------------------------------------------------------------------*
*& Report  ZRIMCIVLST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : Shipping doc. list for Commercial Invoice.            *
*&      작성자 : CH, Lee INFOLINK Ltd.
*&      작성일 : 2001.11.11                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : Display Commercial Invoice and Create and I/V.
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
REPORT  ZRIMCIVLST  MESSAGE-ID ZIM
                    LINE-SIZE 141
                    NO STANDARD PAGE HEADING.

TABLES: EKKO,EKPO,ZTBL,ZTBLIT,ZTCIVHD,ZTCIVIT,T001W,LFA1,SPOP,ZSCIVHD,
        ZTIMIMG00.
*-----------------------------------------------------------------------
* B/L 입수내역 리스트용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 300,
       BUKRS      LIKE ZTBL-BUKRS,               " 회사코드.
       ZFWERKS    LIKE ZTBL-ZFWERKS,             " 플랜트.
       NAME       LIKE T001W-NAME1,              " 플랜트.
       ZFCIVRN    LIKE ZTCIVHD-ZFCIVRN,          " 송장관리번호.
       EBELN      LIKE ZTBLIT-EBELN,             " P/O.
       EBELP      LIKE ZTBLIT-EBELP,             " P/O ITEM.
       ZFREQNO    LIKE ZTBLIT-ZFREQNO,
       ZFITMNO    LIKE ZTBLIT-ZFITMNO,
       TXZ01      LIKE ZTBLIT-TXZ01,
       ZFBLIT     LIKE ZTBLIT-ZFBLIT,            " B/L ITEM.
       ZFIVNO     LIKE ZTIV-ZFIVNO,              " CIV No.
       MATNR      LIKE ZTBLIT-MATNR,             " Material Code..
       ZFRPTTY    LIKE ZTBL-ZFRPTTY,             " delivery type.
       RPTTY      LIKE DD07T-DDTEXT,             " delivery type.
       ZFCARC     LIKE ZTBL-ZFCARC,              " Loading County Code.
       ZFSPRTC    LIKE ZTBL-ZFSPRTC,             " Loading Port..
       UNTTO      LIKE EKPO-UNTTO,               "
       UEBTO      LIKE EKPO-UEBTO,
       ZFBENI     LIKE ZTBL-ZFBENI,              " Vendor.
       NAME1      LIKE LFA1-NAME1,
       EKORG      LIKE EKKO-EKORG,
       EKGRP      LIKE EKKO-EKGRP,
       ZFBLNO     LIKE ZTBL-ZFBLNO,              " B/L Document No.
       ZFHBLNO    LIKE ZTBL-ZFHBLNO,             " House B/L No.
       ZFETD      LIKE ZTBL-ZFETD,               " ETD
       ZFETA      LIKE ZTBL-ZFETA,               " ETA
       ZTERM      LIKE EKKO-ZTERM,               " Payment Term.
       BLMENGE    LIKE ZTBLIT-BLMENGE,           " B/L Qty.
       MEINS      LIKE ZTBLIT-MEINS,             " Qty Unit,
       ZFPRQN     LIKE ZTCIVIT-ZFPRQN,           " G/P Created Qty.
       CIVTOT     LIKE ZTCIVIT-ZFPRQN,           " Not Created Qty.
       GUBUN(12)  TYPE C.                        " .
DATA : END OF IT_TAB.

DATA:    BEGIN OF ZBDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA     END OF ZBDCDATA.

DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFBLNO    LIKE ZTBL-ZFBLNO,          " B/L 관리번호.
      ZFBLIT    LIKE ZTBLIT-ZFBLIT,
      GUBUN(06) TYPE C,
      ZFHBLNO   LIKE ZTBL-ZFHBLNO,         " H B/L No.
END OF IT_SELECTED.

INCLUDE : <ICON>,
           ZRIMBDCCOM.

DATA :  W_ERR_CHK         TYPE C,
        W_SELECTED_LINES  TYPE P,
        W_PAGE            TYPE I,
        W_CNT             TYPE I,
        EGRKZ             LIKE T007A-EGRKZ,
        W_TITLE(24)       TYPE C,
        W_TITLE1(50)      TYPE C,
        W_DOM_TEX1        LIKE DD07T-DDTEXT,
        W_FNAME           LIKE ZTIMIMG08-ZFCDNM,
        W_CHK_TITLE       TYPE C,
        DISPMODE(1)       TYPE C,
        W_LINE            TYPE I,
        W_COUNT1          TYPE I,
        ANTWORT(1)        TYPE C,
        CANCEL_OPTION     TYPE C,
        OPTION(1)         TYPE C,
        TEXTLEN           TYPE I,
        W_GUBUN(50)       TYPE C,
        W_KRWAMT(18)      TYPE C,
        W_COUNT           TYPE I,
        W_TABIX           LIKE SY-TABIX,
        W_FIELD_NM        LIKE DD03D-FIELDNAME,
        W_LIST_INDEX      LIKE SY-TABIX,
        TEMP_ZFBLNO       LIKE ZTBL-ZFBLNO,
        W_MOD             TYPE I,
        W_PAGE_CHECK(1)   TYPE C,
        W_ZFBTSEQ         LIKE ZTBLINR-ZFBTSEQ,
        P_BUKRS           LIKE ZTBL-BUKRS.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen Clause.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR ZTBL-BUKRS NO INTERVALS  " 회사코드.
                                         NO-EXTENSION,
                S_WERKS   FOR ZTBL-ZFWERKS NO INTERVALS  ".PLANT.
                          NO-EXTENSION,
                S_ZTERM   FOR EKKO-ZTERM,       " 지급조건.
                S_EBELN   FOR ZTBLIT-EBELN,     " P/O No
                S_MATNR   FOR ZTBLIT-MATNR,     " 자재코드.
                S_EKGRP   FOR EKKO-EKGRP,       " 구매그룹.
                S_EKORG   FOR EKKO-EKORG,       " 구매조직.
                S_HBLNO   FOR ZTBL-ZFHBLNO,
                S_BLNO    FOR ZTBL-ZFBLNO.
SELECTION-SCREEN END OF BLOCK B1.
*>> 선택구분.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
SELECTION-SCREEN : COMMENT 4(18) TEXT-021, POSITION 1.
PARAMETERS : P_NO  RADIOBUTTON GROUP RDG.     " 미생성현황.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
SELECTION-SCREEN : COMMENT 4(18) TEXT-022, POSITION 1.
PARAMETERS : P_YES      RADIOBUTTON GROUP RDG. " 기생성현황.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN : BEGIN OF LINE,  POSITION 1.
SELECTION-SCREEN : COMMENT 4(18) TEXT-023, POSITION 1.
PARAMETERS : P_ALL   RADIOBUTTON GROUP RDG.   "전체.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN : BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-024.
PARAMETERS: P_ITEM AS CHECKBOX.
SELECTION-SCREEN : END OF BLOCK B3.

* PARAMETER Initial Value Setting
INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
  IF P_ITEM EQ 'X'.
    PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ELSE.
    PERFORM   P3000_TITLE_WRITE_HEADER.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-LOW.
  PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
  PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-HIGH.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* B/L 테이블 SELECT
  PERFORM   P1000_READ_DATA          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.MESSAGE S738.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.         " SORT 선택?
      W_FIELD_NM = 'ZFBLHNO'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
    WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
    WHEN 'EXCU'.           " 물대생성.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S951.
        EXIT.
      ENDIF.
      IF W_SELECTED_LINES GT 1.
        MESSAGE S965.
        EXIT.
      ENDIF.
      PERFORM P2000_MESSAGE_POSTING.
      IF ANTWORT EQ 'Y'.
        PERFORM P4000_BDC_CALL.
      ENDIF.
      PERFORM   P1000_READ_DATA USING   W_ERR_CHK.
      PERFORM RESET_LIST.
    WHEN 'DISP' OR 'DISP2' .
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S951.
        EXIT.
      ENDIF.
      IF W_SELECTED_LINES GT 1.
        MESSAGE S965.
        EXIT.
      ENDIF.
      READ TABLE IT_SELECTED INDEX 1.
      PERFORM P2000_DISP_CIVHD USING IT_SELECTED-ZFBLNO
                                     IT_SELECTED-ZFBLIT.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIM35N'.          " TITLE BAR
  P_NO = 'X'.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  IF P_YES = 'X'.
    MOVE '[Created Invoice List ]' TO W_TITLE.
  ENDIF.
  IF P_NO = 'X'.
    MOVE '[Uncreated Invoice List]' TO W_TITLE.
  ENDIF.
  IF P_ALL = 'X'.
    MOVE '[Invoice Document List ]' TO W_TITLE.
  ENDIF.
  WRITE : /53  W_TITLE
                  COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 2.
  WRITE : /'Date:', SY-DATUM.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-ULINE.
  WRITE:/ SY-VLINE NO-GAP,(20) 'Vendor'              NO-GAP,
          SY-VLINE NO-GAP,(15) 'B/L No'              NO-GAP,
          SY-VLINE NO-GAP,(25) 'Plant'               NO-GAP,
          SY-VLINE NO-GAP,(15) 'Purch. Group'        NO-GAP,
          SY-VLINE NO-GAP,(15) 'Purch. Organizion'   NO-GAP,
          SY-VLINE NO-GAP,(14) 'Payment Term'        NO-GAP,
          SY-VLINE NO-GAP,(14) 'ETD'                 NO-GAP,
          SY-VLINE NO-GAP,(14) 'ETA'                 NO-GAP,
          SY-VLINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/ SY-VLINE NO-GAP,(20) 'Material No.'         NO-GAP,
          SY-VLINE NO-GAP,(15) 'Repr P/O No.'         NO-GAP,
          SY-VLINE NO-GAP,(25) 'Description of Goods' NO-GAP,
          SY-VLINE NO-GAP,(15) 'B/L Quantity'         NO-GAP,
          SY-VLINE NO-GAP,(15) 'Uncreated Qty'        NO-GAP,
          SY-VLINE NO-GAP,(14) 'Tolerance(Under)'     NO-GAP,
          SY-VLINE NO-GAP,(14) 'Tolerance(Over)'      NO-GAP,
          SY-VLINE NO-GAP,(14) 'I/V Status'           NO-GAP,
          SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE_HEADER
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE_HEADER.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  IF P_YES = 'X'.
    MOVE '[Created Invoice List ]' TO W_TITLE.
  ENDIF.
  IF P_NO = 'X'.
    MOVE '[Uncreated Invoice List]' TO W_TITLE.
  ENDIF.
  IF P_ALL = 'X'.
    MOVE '[Invoice Document List ]' TO W_TITLE.
  ENDIF.
  WRITE : /53  W_TITLE
                  COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 2.
  WRITE : /'Date:', SY-DATUM.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-ULINE(129).
  WRITE:/ SY-VLINE NO-GAP,(20) 'Vendor'              NO-GAP,
          SY-VLINE NO-GAP,(15) 'B/L No'              NO-GAP,
          SY-VLINE NO-GAP,(25) 'Plant'               NO-GAP,
          SY-VLINE NO-GAP,(10) 'Purch.Grp.'          NO-GAP,
          SY-VLINE NO-GAP,(10) 'Purch.Org.'          NO-GAP,
          SY-VLINE NO-GAP,(09) 'Pymt.Term'           NO-GAP,
          SY-VLINE NO-GAP,(10) 'ETD'                 NO-GAP,
          SY-VLINE NO-GAP,(10) 'ETA'                 NO-GAP,
          SY-VLINE NO-GAP,(10) 'Loading Pt'          NO-GAP,
          SY-VLINE.
  WRITE:/ SY-ULINE(129).

ENDFORM.                    " P3000_TITLE_WRITE_HEADER.
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.
  CLEAR:  W_COUNT, TEMP_ZFBLNO.
  SET PF-STATUS 'ZIM35N'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIM35N'.           " GUI TITLE SETTING..

  SORT IT_TAB BY EBELN ZFWERKS ZFBLNO ZFBLIT.

  IF P_ITEM EQ 'X'.
    LOOP AT IT_TAB.
      PERFORM P3000_LINE_WRITE.
      AT LAST.
        PERFORM P3000_LAST_WRITE.
      ENDAT.
    ENDLOOP.
  ELSE.
    LOOP AT IT_TAB.
      PERFORM P3000_LINE_WRITE_HEADER.
      AT LAST.
        PERFORM P3000_LAST_WRITE.
      ENDAT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.

  IF P_ITEM EQ 'X'.
    PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ELSE.
    PERFORM   P3000_TITLE_WRITE_HEADER.
  ENDIF.

  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF P_ITEM EQ 'X'.
    WRITE : / SY-ULINE.
  ELSE.
    WRITE : / SY-ULINE(129).
  ENDIF.
  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE : /'Total:', W_COUNT.
  ENDIF.
ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  IF TEMP_ZFBLNO NE IT_TAB-ZFBLNO .
    IF SY-TABIX NE 1.
      WRITE:/ SY-ULINE.
    ENDIF.
    WRITE:/ SY-VLINE,MARKFIELD  AS CHECKBOX,
                            (17) IT_TAB-NAME1 NO-GAP,   " Vendor'
            SY-VLINE NO-GAP,(15) IT_TAB-ZFHBLNO NO-GAP, " B/L No'
            SY-VLINE NO-GAP,(25) IT_TAB-NAME    NO-GAP, " Plant'
            SY-VLINE NO-GAP,(15) IT_TAB-EKORG   NO-GAP, " 구매조직.
            SY-VLINE NO-GAP,(15) IT_TAB-EKGRP   NO-GAP, " 구매그룹'
            SY-VLINE NO-GAP,(14) IT_TAB-ZTERM CENTERED NO-GAP,
            SY-VLINE NO-GAP,(14) IT_TAB-ZFETD   NO-GAP, " ETD.
            SY-VLINE NO-GAP,(14) IT_TAB-ZFETA   NO-GAP, " ETA.
            SY-VLINE.
    HIDE IT_TAB.
    W_COUNT = W_COUNT + 1.
  ENDIF.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE NO-GAP ,(20)IT_TAB-MATNR NO-GAP,
         SY-VLINE NO-GAP,(15) IT_TAB-EBELN NO-GAP,  " P/O NO
         SY-VLINE NO-GAP,(25) IT_TAB-TXZ01 NO-GAP,
         SY-VLINE NO-GAP,(11) IT_TAB-BLMENGE UNIT
                              IT_TAB-MEINS  NO-GAP, " B/L 수?
                         (03) IT_TAB-MEINS,
         SY-VLINE NO-GAP,(11) IT_TAB-CIVTOT UNIT
                              IT_TAB-MEINS  NO-GAP, " 미생성수?
                         (03) IT_TAB-MEINS,
         SY-VLINE NO-GAP,(14) IT_TAB-UNTTO  NO-GAP, " 미달납품허용치.
         SY-VLINE NO-GAP,(14) IT_TAB-UEBTO  NO-GAP,
         SY-VLINE NO-GAP,(14) IT_TAB-GUBUN  NO-GAP,  " IV상태'
         SY-VLINE.

* stored value...
  HIDE: IT_TAB, W_TABIX.
* WRITE:/ SY-ULINE.
  TEMP_ZFBLNO = IT_TAB-ZFBLNO.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE_HEADER.
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE_HEADER.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  IF TEMP_ZFBLNO NE IT_TAB-ZFBLNO .
    IF SY-TABIX NE 1.
      WRITE:/ SY-ULINE(129).
    ENDIF.
    WRITE:/ SY-VLINE,MARKFIELD  AS CHECKBOX,
                             (17)IT_TAB-NAME1 NO-GAP,
            SY-VLINE NO-GAP, (15)IT_TAB-ZFHBLNO NO-GAP,
            SY-VLINE NO-GAP, (25)IT_TAB-NAME    NO-GAP,
            SY-VLINE NO-GAP, (10)IT_TAB-EKORG   NO-GAP,
            SY-VLINE NO-GAP, (10)IT_TAB-EKGRP   NO-GAP,
            SY-VLINE NO-GAP, (09)IT_TAB-ZTERM CENTERED NO-GAP,
            SY-VLINE NO-GAP, (10)IT_TAB-ZFETD   NO-GAP,
            SY-VLINE NO-GAP, (10)IT_TAB-ZFETA   NO-GAP,
            SY-VLINE NO-GAP, (03)IT_TAB-ZFCARC  NO-GAP,
            '/ ' NO-GAP,     (05) IT_TAB-ZFSPRTC NO-GAP,
            SY-VLINE.
    HIDE IT_TAB.
    W_COUNT = W_COUNT + 1.
  ENDIF.

* stored value...
  HIDE: IT_TAB, W_TABIX.
* WRITE:/ SY-ULINE.
  TEMP_ZFBLNO = IT_TAB-ZFBLNO.

ENDFORM.                    " P3000_LINE_WRITE_HEADER.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA USING  W_ERR_CHK.

  W_ERR_CHK = 'N'.
  CLEAR IT_TAB.
  REFRESH IT_TAB.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
            FROM ZTBL AS H INNER JOIN ZTBLIT AS I
             ON H~ZFBLNO = I~ZFBLNO
            WHERE I~EBELN   IN S_EBELN
              AND I~MATNR   IN S_MATNR
              AND H~BUKRS   IN S_BUKRS
              AND H~EKORG   IN S_EKORG
              AND H~EKGRP   IN S_EKGRP
              AND H~ZFHBLNO IN S_HBLNO
              AND H~ZFBLNO  IN S_BLNO
              AND H~ZFWERKS IN S_WERKS
              AND I~ZFPOTY  EQ SPACE        " 유환.
              AND H~ZFPOYN  NE 'N'.
  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'. EXIT.  ENDIF.
*>> 송장 ITEM 단위.
  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.

    " 주기기 선적은 비용처리에서 제외.
    SELECT COUNT( * ) INTO W_CNT
    FROM   ZTREQHD
    WHERE  EBELN      EQ   IT_TAB-EBELN.
    IF W_CNT EQ 0 OR W_CNT IS INITIAL.
      DELETE  IT_TAB  INDEX  W_TABIX.
      CONTINUE.
    ENDIF.

    " 지급조건 GET!
    CLEAR EKKO.
    SELECT SINGLE *  FROM  EKKO
    WHERE  EBELN     EQ    IT_TAB-EBELN
    AND    ZTERM     IN    S_ZTERM.
    IF NOT S_ZTERM[] IS INITIAL.
      IF SY-SUBRC NE 0.
        DELETE  IT_TAB  INDEX  W_TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.
    MOVE  EKKO-ZTERM  TO  IT_TAB-ZTERM.

    CLEAR EKPO.
    SELECT SINGLE *
           FROM EKPO
          WHERE EBELN = IT_TAB-EBELN
            AND EBELP = IT_TAB-EBELP.
    IF SY-SUBRC EQ 0.
      MOVE: EKPO-UEBTO TO IT_TAB-UEBTO,               "
            EKPO-UNTTO TO IT_TAB-UNTTO.
    ENDIF.
    CLEAR: ZTCIVIT,W_COUNT.
    SELECT COUNT( * ) INTO W_COUNT
           FROM ZTCIVIT
          WHERE ZFBLNO = IT_TAB-ZFBLNO
            AND ZFBLIT = IT_TAB-ZFBLIT.

    CLEAR : W_COUNT1.
    SELECT COUNT( * )   INTO W_COUNT1
           FROM ZTCIVIT
          WHERE ZFREQNO EQ   IT_TAB-ZFREQNO
            AND ZFITMNO EQ   IT_TAB-ZFITMNO.

    W_COUNT = W_COUNT + W_COUNT1.

    IF W_COUNT EQ 0.
      IF P_YES EQ 'X'.   " 생성.
        DELETE IT_TAB INDEX W_TABIX.
        CONTINUE.
      ENDIF.
    ELSE.
      IF P_NO EQ 'X'.
        DELETE IT_TAB INDEX W_TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.
*>> 기생성 수량.
    SELECT SUM( ZFPRQN ) INTO IT_TAB-ZFPRQN
        FROM ZTCIVIT
       WHERE ZFBLNO = IT_TAB-ZFBLNO
         AND ZFBLIT = IT_TAB-ZFBLIT.
    IF IT_TAB-ZFPRQN IS INITIAL.
      SELECT SUM( ZFPRQN ) INTO IT_TAB-ZFPRQN
      FROM   ZTCIVIT
      WHERE  ZFREQNO   EQ  IT_TAB-ZFREQNO
      AND    ZFITMNO   EQ  IT_TAB-ZFITMNO.
    ENDIF.
*>> 미생성 수량.
    IT_TAB-CIVTOT = IT_TAB-BLMENGE - IT_TAB-ZFPRQN.
*>> 송장처리구분.
    IF NOT IT_TAB-ZFPRQN IS INITIAL.
      IF IT_TAB-CIVTOT EQ 0.
        IT_TAB-GUBUN = 'Created'.
      ENDIF.
    ENDIF.
    IF NOT IT_TAB-ZFPRQN IS INITIAL.
      IF IT_TAB-CIVTOT > 0.
        IT_TAB-GUBUN = 'Partially'.
      ENDIF.
    ENDIF.
    IF IT_TAB-ZFPRQN EQ 0 AND IT_TAB-CIVTOT NE 0.
      IT_TAB-GUBUN = 'Not created'.
    ENDIF.

    CLEAR T001W.
    SELECT SINGLE *
           FROM T001W
          WHERE WERKS = IT_TAB-ZFWERKS.
    CLEAR LFA1.
    SELECT SINGLE *
           FROM LFA1
          WHERE LIFNR = IT_TAB-ZFBENI.

    MOVE LFA1-NAME1  TO  IT_TAB-NAME1.
    MOVE T001W-NAME1 TO  IT_TAB-NAME.

    MODIFY IT_TAB INDEX W_TABIX.

  ENDLOOP.
  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE EQ 0.
    W_ERR_CHK = 'Y'.
  ENDIF.

ENDFORM.                    " P1000_READ_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR: W_SELECTED_LINES.
  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFBLNO  TO IT_SELECTED-ZFBLNO,
             IT_TAB-ZFHBLNO TO IT_SELECTED-ZFHBLNO,
             IT_TAB-ZFBLIT  TO IT_SELECTED-ZFBLIT.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_POSTING
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_POSTING.

  IF SY-LANGU EQ '3'.
  PERFORM P2000_MESSAGE_BOX USING '화면이동 '  " 타이틀...
                          '선택된 자료에 대해 물대전표생성/저장 합니다.'
                          '생성/저장화면으로 이동 하시겠습니까?'
                          'N'                 " 취소 버튼 유/?
                          '1'.                " default button
  ELSE.
     PERFORM P2000_MESSAGE_BOX USING
             'Move confirm'
             'You should create or save goods price of selected record'
             'Do you want to move?'
             'N'
             '1'.
  ENDIF.

ENDFORM.                    " P2000_MESSAGE_POSTING
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_BOX USING    TITLE  LIKE SPOP-TITEL
                                TEXT1  LIKE SPOP-TEXTLINE1
                                TEXT2  LIKE SPOP-TEXTLINE2
                                CANCEL LIKE CANCEL_OPTION
                                DEFAULT LIKE OPTION.
  IF CANCEL EQ 'Y'.
    CANCEL_OPTION = 'Y'.
  ELSE.
    CLEAR : CANCEL_OPTION.
  ENDIF.
  OPTION = DEFAULT.
  TEXTLEN = 40.

  CALL SCREEN 0001 STARTING    AT 10 2
                      ENDING   AT 90 04.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'SPOP-OPTION_CAN'.
      IF CANCEL_OPTION = SPACE.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE'.                   "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  ANTWORT = 'NO'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFCIVNO_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE ZFCIVNO_CHECK_SCR0001 INPUT.

  IF SY-UCOMM = 'YES' OR SY-UCOMM = 'ENTR'.
    IF ZSCIVHD-ZFCIVNO IS INITIAL.
      MESSAGE W977 WITH 'Input invoice No!'.
    ENDIF.
    CLEAR ZTCIVHD.
    SELECT SINGLE *
      FROM ZTCIVHD
     WHERE ZFCIVNO = ZSCIVHD-ZFCIVNO.
    IF SY-SUBRC EQ 0.
      MESSAGE W977 WITH 'Already exist invoice No.'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " PERIOD_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'ENTR'.   ANTWORT = 'Y'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.    ANTWORT = 'Y'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.     ANTWORT = 'N'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_CIVHD
*&---------------------------------------------------------------------*
FORM P2000_DISP_CIVHD USING    P_ZFBLNO
                               P_ZFBLIT.

  IF SY-UCOMM  EQ 'DISP2'.
    SET  PARAMETER ID  'ZPBLNO' FIELD  P_ZFBLNO.
    SET  PARAMETER ID  'ZPHBLNO' FIELD ''.
    CALL TRANSACTION 'ZIM23'  AND SKIP FIRST SCREEN.
    EXIT.
  ENDIF.
  SELECT SINGLE *
       FROM ZTCIVIT
      WHERE ZFBLNO = P_ZFBLNO
        AND ZFBLIT = P_ZFBLIT.
  IF SY-SUBRC NE 0.
    MESSAGE S681.
    EXIT.
  ENDIF.

  SET  PARAMETER ID  'ZPCIVNO' FIELD ''.
  SET  PARAMETER ID  'ZPCIVRN' FIELD  ZTCIVIT-ZFCIVRN.
  CALL TRANSACTION 'ZIM37'  AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_DISP_CIVHD
*&---------------------------------------------------------------------*
*&      Form  P4000_BDC_CALL
*&---------------------------------------------------------------------*
FORM P4000_BDC_CALL.

  PERFORM ZTCIVHD_BDC_INSERT.
  DISPMODE = 'E'.
  CALL TRANSACTION 'ZIM35'
                USING       ZBDCDATA
                MODE        DISPMODE
                UPDATE      UMODE.
  IF SY-SUBRC <> 0.
    MESSAGE I952.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

ENDFORM.                    " P4000_BDC_CALL
*&---------------------------------------------------------------------*
*&      Form  ZTCIVHD_BDC_INSERT
*&---------------------------------------------------------------------*
FORM ZTCIVHD_BDC_INSERT.

  REFRESH ZBDCDATA.
  PERFORM A_ZBDCDATA USING: 'X' 'SAPMZIM01' '3500',
  ' ' 'BDC_CURSOR' 'ZSCIVHD-ZFCIVNO',
  ' ' 'BDC_OKCODE' '=ENTR',
  ' ' 'ZSCIVHD-ZFCIVNO'   ZSCIVHD-ZFCIVNO.
  PERFORM A_ZBDCDATA USING: 'X' 'SAPMZIM01' '3512',
     ' ' 'BDC_OKCODE' 'yes',
     ' ' 'ZSREQHD-ZFHBLNO'  IT_SELECTED-ZFHBLNO,  " B/L NUMBER.
     ' ' 'ZSREQHD-ZFBLNO'   IT_SELECTED-ZFBLNO.  " B/L NUMBER.

ENDFORM.                    " ZTCIVHD_BDC_INSERT
*&---------------------------------------------------------------------*
*&      Form  A_ZBDCDATA
*&---------------------------------------------------------------------*
FORM A_ZBDCDATA USING   BEGIN_CHECK OBJNAM VALUE.

  CLEAR ZBDCDATA.
  IF BEGIN_CHECK = 'X'.
    MOVE : OBJNAM TO ZBDCDATA-PROGRAM,
           VALUE  TO ZBDCDATA-DYNPRO,
           BEGIN_CHECK TO ZBDCDATA-DYNBEGIN.
  ELSE.
    MOVE : OBJNAM TO ZBDCDATA-FNAM,
           VALUE  TO ZBDCDATA-FVAL.
  ENDIF.
  APPEND ZBDCDATA.

ENDFORM.                    " A_ZBDCDATA
*&---------------------------------------------------------------------*
*&      Form  P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
FORM P1000_PAY_TERM_HELP  USING    P_ZTERM.

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
