*&---------------------------------------------------------------------*
*& Report  ZRIMBLPRCSLST                                               *
*&---------------------------------------------------------------------*
*&  Program Name : Progress Status By P/O Seq(B/L)                     *
*&  Created by   : Lee Chae Kyung INFOLINK Ltd.                        *
*&  Created on    : 2001.10.11                                         *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [Change Log]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMBLPRCSLST  MESSAGE-ID ZIM
                     LINE-SIZE 104
                     NO STANDARD PAGE HEADING.

TABLES: EKKO,ZTBL,ZTBLIT,ZTCUCLIV,ZTIV,ZTIVIT,ZTIVHST,ZTIDR,
        ZTIDS,ZTBLINR,ZTBLOUR, ZTIDRUS,
        ZTBWIT,ZTBWHD,T001W, ZTBLINR_TMP, ZTTRHD, ZTTRIT, ZTIMIMG00.

*-----------------------------------------------------------------------
* B/L List INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 300,
       PONO(13),
       ZFWERKS    LIKE ZTBL-ZFWERKS,             " Plant.
       EBELN      LIKE ZTBLIT-EBELN,             " P/O.
       ZFBLIT     LIKE ZTBLIT-ZFBLIT,            " BL ITEM
       ZFIVNO     LIKE ZTIV-ZFIVNO,              " CC(GR) Request No.
       ZFCLSEQ    LIKE ZTIDS-ZFCLSEQ,            " Clearance Seq.
       ZFSHNO     LIKE ZTBL-ZFSHNO,              " Shipping Seq.
       MATNR      LIKE ZTBLIT-MATNR,             " Material Code.
       ZFRPTTY    LIKE ZTBL-ZFRPTTY,             " Import dec.
       RPTTY      LIKE DD07T-DDTEXT,             " Import dec.
       ZFRGDSR    LIKE ZTBL-ZFRGDSR,             " Representive Goods.
       ZFBLNO     LIKE ZTBL-ZFBLNO,              " B/L DOCUMENT NO.
       ZFHBLNO    LIKE ZTBL-ZFHBLNO,             " HOUSE
       ZFINDT     LIKE ZTBLINR-ZFINDT,           " Bonded-in Date.
       ZFGIDT     LIKE ZTBWHD-ZFGIDT,            " G/I Date.
       ZFETD      LIKE ZTBL-ZFETD,               " ETD
       ZFETA      LIKE ZTBL-ZFETA,               " ETA
       ZFBLSDT    LIKE ZTBL-ZFBLSDT,             " Sending Date
       ZFRETA     LIKE ZTBL-ZFRETA,              " Real Arriving Date.
       BUDAT      LIKE ZTIVHST-BUDAT,            " G/R Date
       BLMENGE    LIKE ZTBLIT-BLMENGE,           " B/L Quantity.
       MEINS      LIKE ZTBLIT-MEINS,             " Unit.
       CCMENGE    LIKE ZTIVIT-CCMENGE,           " Clearance Quantity
       MEINS2     LIKE ZTIVIT-MEINS,             " Basic Unit
       GRMENGE    LIKE ZTIVIT-GRMENGE,           " G/R Quantity
       GRTOTMN    LIKE ZTIVIT-GRTOTMN,           " Real G/R Quantity
       GIMENGE    LIKE ZTBWIT-GIMENGE,           " G/I Quantity
       MEINS3     LIKE ZTBWIT-MEINS,             " Unit
       ZFIDSDT    LIKE ZTIDS-ZFIDSDT.            " Entry Date
DATA : END OF IT_TAB.

DATA :  W_ERR_CHK         TYPE C,
        W_SUBRC           LIKE SY-SUBRC,
        W_SELECTED_LINES  TYPE P,
        W_PAGE            TYPE I,
        EGRKZ             LIKE     T007A-EGRKZ,
        W_TITLE(32)       TYPE C,
        W_TITLE1(50)      TYPE C,
        W_DOM_TEX1        LIKE DD07T-DDTEXT,
        W_FNAME           LIKE ZTIMIMG08-ZFCDNM,
        W_CHK_TITLE       TYPE C,
        W_LINE            TYPE I,
        W_GUBUN(50)       TYPE C,
        W_KRWAMT(18)      TYPE C,
        W_COUNT           TYPE I,
        W_TABIX           LIKE SY-TABIX,
        W_FIELD_NM        LIKE DD03D-FIELDNAME,
        W_LIST_INDEX      LIKE SY-TABIX,
        W_MOD             TYPE I,
        W_PAGE_CHECK(1)   TYPE C,
        W_ZFBTSEQ         LIKE ZTBLINR-ZFBTSEQ,
        W_CBUDAT_INIT     LIKE ZTIVHST-CBUDAT,
        P_BUKRS           LIKE ZTBL-BUKRS.

INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.

*-----------------------------------------------------------------------
* Selection Screen.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTBL-BUKRS NO INTERVALS
                                            NO-EXTENSION,
                   S_EBELN   FOR ZTBLIT-EBELN,
                   S_EKGRP   FOR EKKO-EKGRP,
                   S_WERKS   FOR ZTBL-ZFWERKS NO INTERVALS
                             NO-EXTENSION,
                   S_MATNR   FOR ZTBLIT-MATNR.
SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER Initial Setting
INITIALIZATION.
   PERFORM   P1000_SET_BUKRS.
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
W_PAGE_CHECK = 'N'.

TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.

*-----------------------------------------------------------------------
* START OF SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

   " B/L Data Select
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
      WHEN 'REFR'.
           PERFORM   P1000_READ_DATA USING   W_ERR_CHK.
           PERFORM RESET_LIST.
      WHEN 'DISP1'.                       " B/L 조회.
          IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
           ENDIF.
           PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_TAB-ZFBLNO.
     WHEN 'DISP2'.      " 통관요청.
          IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
          ENDIF.
          PERFORM P2000_DISP_ZTIV USING IT_TAB-ZFBLNO.
     WHEN 'DISP3'. " 수입신고.
           IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
           ENDIF.
           SELECT SINGLE *
              FROM ZTIDRUS
              WHERE ZFBLNO  = IT_TAB-ZFBLNO
                AND ZFCLSEQ = IT_TAB-ZFCLSEQ.
           IF SY-SUBRC NE 0.
              MESSAGE S753. EXIT.
           ENDIF.
           PERFORM P2000_DISP_ZTIDR(SAPMZIM09) USING IT_TAB-ZFBLNO
                                                     IT_TAB-ZFCLSEQ.
     WHEN 'DISP4'.                       " 수입면허.
           IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
           ENDIF.
           IF IT_TAB-ZFIDSDT IS INITIAL.
              MESSAGE S639.EXIT.
           ENDIF.
           PERFORM P2000_DISP_ZTIDS(SAPMZIM09) USING IT_TAB-ZFBLNO
                                                     IT_TAB-ZFCLSEQ.
      WHEN 'DISP5'.                     " 보세창고출고.
         IF W_TABIX EQ 0.
            MESSAGE E962. EXIT.
         ENDIF.
         PERFORM  P2000_DISP_ZTBWHD USING IT_TAB-ZFBLNO.
     WHEN OTHERS.
   ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR69'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  IF W_PAGE_CHECK = 'N'.
      WRITE : /33  ' [ B/L Progress Status by P/O Seq ] '
                  COLOR COL_HEADING INTENSIFIED OFF.
     W_PAGE_CHECK = 'Y'.
  ENDIF.
  CLEAR T001W.
  SELECT SINGLE *
         FROM T001W
        WHERE WERKS = IT_TAB-ZFWERKS.
  WRITE : / IT_TAB-ZFWERKS,T001W-NAME1,85 'Date : ', SY-DATUM.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-ULINE.
    WRITE:/ SY-VLINE NO-GAP,(18) 'Purchase Order' NO-GAP,
          SY-VLINE NO-GAP,(25) 'House B/L No'     NO-GAP,
          SY-VLINE NO-GAP,(10) 'Ship.date'        NO-GAP,
          SY-VLINE NO-GAP,(17) 'Shimpment Qty'    NO-GAP,
          SY-VLINE NO-GAP,(10) 'Entry Date'       NO-GAP,
          SY-VLINE NO-GAP,(17) ' '                NO-GAP,
          SY-VLINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/ SY-VLINE NO-GAP,(18) 'Material'         NO-GAP,
          SY-VLINE NO-GAP,(25) 'Description'      NO-GAP,
          SY-VLINE NO-GAP,(10) 'Import Date'      NO-GAP,
          SY-VLINE NO-GAP,(17) 'Clearance Qty'    NO-GAP,
          SY-VLINE NO-GAP,(10) 'G/R date'         NO-GAP,
          SY-VLINE NO-GAP,(17) '      G/R Qty'    NO-GAP,
          SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIMR69'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMR69'.           " GUI TITLE SETTING..

   W_COUNT = 0.
   SORT IT_TAB BY ZFWERKS ZFHBLNO ZFBLIT.
   LOOP AT IT_TAB.
      ON CHANGE OF IT_TAB-ZFWERKS.
          NEW-PAGE.
      ENDON.
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
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : /'Total', W_COUNT, 'Case'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE:/ SY-VLINE NO-GAP,(18) IT_TAB-PONO NO-GAP,
          SY-VLINE NO-GAP,(19) IT_TAB-ZFHBLNO NO-GAP,    " B/L No'
                          (06) IT_TAB-ZFBLIT NO-GAP,
           SY-VLINE NO-GAP,(10) IT_TAB-ZFETD NO-GAP,  " 선적일.

          SY-VLINE NO-GAP,(14) IT_TAB-BLMENGE
                          UNIT IT_TAB-MEINS NO-GAP,    " 선적수량.
                          (03) IT_TAB-MEINS NO-GAP,
          SY-VLINE NO-GAP,(10) IT_TAB-ZFIDSDT NO-GAP,    " 면허일'
          SY-VLINE NO-GAP,(14) ' '            NO-GAP,
                          (03) ' '            NO-GAP,
          SY-VLINE.
* stored value...
  HIDE: IT_TAB, W_TABIX.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE NO-GAP,(18) IT_TAB-MATNR      NO-GAP,
          SY-VLINE NO-GAP,(25) IT_TAB-ZFRGDSR    NO-GAP,
          SY-VLINE NO-GAP,(10) IT_TAB-ZFRETA     NO-GAP,
          SY-VLINE NO-GAP,(14) IT_TAB-CCMENGE UNIT IT_TAB-MEINS NO-GAP,
                          (03) IT_TAB-MEINS      NO-GAP,
          SY-VLINE NO-GAP,(10) IT_TAB-BUDAT      NO-GAP,
          SY-VLINE NO-GAP,(14) IT_TAB-GRTOTMN UNIT IT_TAB-MEINS NO-GAP,
                          (03) IT_TAB-MEINS      NO-GAP,
          SY-VLINE.
* stored value...
  HIDE: IT_TAB, W_TABIX.

  W_COUNT = W_COUNT + 1.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA USING  W_ERR_CHK.

  W_ERR_CHK = 'N'.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
            FROM ZTBL AS H INNER JOIN ZTBLIT AS I
             ON H~ZFBLNO = I~ZFBLNO
            WHERE I~EBELN   IN S_EBELN
              AND I~MATNR   IN S_MATNR
              AND H~BUKRS   IN S_BUKRS
              AND H~ZFWERKS IN S_WERKS.
  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'. EXIT.  ENDIF.

  LOOP AT IT_TAB.

    W_TABIX = SY-TABIX.

    " Clearance, G/R.
    SELECT SINGLE *
      FROM EKKO
     WHERE EBELN = IT_TAB-EBELN
       AND EKGRP IN S_EKGRP.

    IF SY-SUBRC NE 0.
       DELETE IT_TAB INDEX W_TABIX.
       CONTINUE.
    ENDIF.

    CLEAR ZTIVIT.
    SELECT SINGLE *
      FROM ZTIVIT
     WHERE ZFBLNO = IT_TAB-ZFBLNO
       AND ZFBLIT = IT_TAB-ZFBLIT.

    IF ZTIVIT-UMSON EQ 'X'.
       SELECT SUM( GRTOTMN )
         INTO IT_TAB-GRTOTMN
         FROM ZTIVIT
        WHERE ZFBLNO = IT_TAB-ZFBLNO
          AND ZFBLIT = IT_TAB-ZFBLIT.

       IT_TAB-MEINS2  = ZTIVIT-MEINS.
    ENDIF.

    SELECT SUM( B~CCMENGE )   INTO  IT_TAB-CCMENGE
    FROM   ZTIV  AS  A  INNER JOIN ZTIVIT AS B
    ON     A~ZFIVNO     EQ    B~ZFIVNO
    WHERE  B~ZFBLNO     EQ    IT_TAB-ZFBLNO
    AND    B~ZFBLIT     EQ    IT_TAB-ZFBLIT
    AND    A~ZFCUST     EQ    'Y'.

*>> 입고일.
    CLEAR  ZTIVHST.
    SELECT MIN( ZFIVHST ) INTO ZTIVHST-ZFIVHST
      FROM ZTIVHST
     WHERE ZFIVNO  = ZTIVIT-ZFIVNO
       AND CBUDAT = W_CBUDAT_INIT.

     SELECT  SINGLE *
       FROM ZTIVHST
      WHERE ZFIVNO  = ZTIVIT-ZFIVNO
        AND ZFIVHST = ZTIVHST-ZFIVHST.

     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDRPTTY'
                                            IT_TAB-ZFRPTTY
                                          CHANGING IT_TAB-RPTTY.
    IF NOT IT_TAB-ZFSHNO IS INITIAL.
       CONCATENATE IT_TAB-EBELN '-' IT_TAB-ZFSHNO INTO IT_TAB-PONO.
    ELSE.
       MOVE IT_TAB-EBELN  TO IT_TAB-PONO.
    ENDIF.

    MOVE  ZTIVHST-BUDAT TO IT_TAB-BUDAT.
    MODIFY IT_TAB INDEX W_TABIX.

 ENDLOOP.

ENDFORM.                    " P1000_READ_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLAY_ZTBL
*&---------------------------------------------------------------------*
FORM P2000_DISPLAY_ZTBL USING    P_ZFBLNO.

ENDFORM.                    " P2000_DISPLAY_ZTBL
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLAY_ZTIDS
*&---------------------------------------------------------------------*
FORM P2000_DISPLAY_ZTIDS USING    P_ZFBLNO
                                  P_ZFCLSEQ.

ENDFORM.                    " P2000_DISPLAY_ZTIDS
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLAY_ZTIIV
*&---------------------------------------------------------------------*
FORM P2000_DISPLAY_ZTIIV USING    P_ZFIVNO.

ENDFORM.                    " P2000_DISPLAY_ZTIIV
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIV
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTIV USING    P_ZFBLNO.

 SELECT COUNT( * ) INTO W_COUNT
      FROM ZTIV
     WHERE ZFBLNO = P_ZFBLNO.

   IF W_COUNT = 0.
      MESSAGE S679 WITH  P_ZFBLNO.
      EXIT.
   ENDIF.

   SET PARAMETER ID 'ZPIVNO'  FIELD ''.
   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.

   CALL TRANSACTION 'ZIM33'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIV
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTBWHD
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTBWHD USING  P_ZFBLNO.

  SELECT COUNT( * ) INTO W_COUNT
      FROM ZTBWHD
     WHERE ZFBLNO = P_ZFBLNO.
  IF W_COUNT EQ 0.
     MESSAGE S977 WITH 'Not status of bonded warehouse G/I!!'.
     EXIT.
  ENDIF.
  SET PARAMETER ID 'ZPIVNO'  FIELD ''.
  SET PARAMETER ID 'ZPGISEQ' FIELD ''.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.

  CALL TRANSACTION 'ZIMBG3'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTBWHD
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

*>> Company Code Set.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
