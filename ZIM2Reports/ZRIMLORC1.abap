*&---------------------------------------------------------------------*
*& Report  ZRIMLORC1                                                   *
*&---------------------------------------------------------------------*
*& ABAP Name : ZRIMLORC1                                               *
*& Created by: INFOLINK.Ltd                                            *
*& Created on: 07/19/2000                                              *
*& Version   : 1.0                                                     *
*&---------------------------------------------------------------------*
* Import Result of Country/Bank Display .
* Import Result of Funds => Report ZRIMLORF SUBMIT Call
*&---------------------------------------------------------------------*

REPORT  ZRIMLORC1      NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 194
                       LINE-COUNT 65.

FIELD-SYMBOLS : <FS_F>, <FS_F2>, <FS_F3>, <FS_F4>.

TABLES : ZTREQHD,
         ZTREQST,
         EKKO,
         ZTIMIMG00,
         DD07T.

DATA : BEGIN OF IT_TAB1 OCCURS 0,
               CODE(10)    TYPE  C,
               ZFMATGB     LIKE ZTREQHD-ZFMATGB,
               ZFREQNO     LIKE ZTREQHD-ZFREQNO,
               WAERS       LIKE ZTREQHD-ZFWAERS,
               ZFOPAMT     LIKE ZTREQHD-ZFOPAMT.
DATA : END   OF IT_TAB1.

DATA : BEGIN OF IT_TEMP OCCURS 0,
       ZFREQNO   LIKE   ZTREQST-ZFREQNO,
       ZFAMDNO   LIKE   ZTREQST-ZFAMDNO.
DATA : END   OF IT_TEMP.

DATA : BEGIN OF IT_TAB2 OCCURS 0,
               CODE(10)    TYPE  C,
               WAERS       LIKE ZTREQHD-ZFWAERS,
               COUNT1(5)   TYPE  I,
               OPAMT1      LIKE ZTREQST-ZFOPAMT,
               COUNT2(5)   TYPE  I,
               OPAMT2      LIKE ZTREQST-ZFOPAMT,
               COUNT3(5)   TYPE  I,
               OPAMT3      LIKE ZTREQST-ZFOPAMT,
               COUNT4(5)   TYPE  I,
               OPAMT4      LIKE ZTREQST-ZFOPAMT,
               COUNT5(5)   TYPE  I,
               OPAMT5      LIKE ZTREQST-ZFOPAMT,
               COUNT6(5)   TYPE  I,
               OPAMT6      LIKE ZTREQST-ZFOPAMT,
               COUNT7(5)   TYPE  I,
               OPAMT7      LIKE ZTREQST-ZFOPAMT,
               COUNT8(5)   TYPE  I,
               OPAMT8      LIKE ZTREQST-ZFOPAMT,
               COUNT9(5)   TYPE  I,
               OPAMT9      LIKE ZTREQST-ZFOPAMT,
               COUNT10(5)  TYPE  I,
               OPAMT10     LIKE ZTREQST-ZFOPAMT,
               COUNTS(5)   TYPE  I,
               OPAMTS      LIKE ZTREQST-ZFOPAMT.
DATA : END   OF IT_TAB2.

DATA : BEGIN OF IT_TAB3 OCCURS 0,
               CODE(10)    TYPE  C,
               WAERS       LIKE ZTREQHD-ZFWAERS,
               COUNT1(5)   TYPE  I,
               OPAMT1      LIKE ZTREQST-ZFOPAMT,
               COUNT2(5)   TYPE  I,
               OPAMT2      LIKE ZTREQST-ZFOPAMT,
               COUNT3(5)   TYPE  I,
               OPAMT3      LIKE ZTREQST-ZFOPAMT,
               COUNT4(5)   TYPE  I,
               OPAMT4      LIKE ZTREQST-ZFOPAMT,
               COUNT5(5)   TYPE  I,
               OPAMT5      LIKE ZTREQST-ZFOPAMT,
               COUNT6(5)   TYPE  I,
               OPAMT6      LIKE ZTREQST-ZFOPAMT,
               COUNT7(5)   TYPE  I,
               OPAMT7      LIKE ZTREQST-ZFOPAMT,
               COUNT8(5)   TYPE  I,
               OPAMT8      LIKE ZTREQST-ZFOPAMT,
               COUNT9(5)   TYPE  I,
               OPAMT9      LIKE ZTREQST-ZFOPAMT,
               COUNT10(5)  TYPE  I,
               OPAMT10     LIKE ZTREQST-ZFOPAMT,
               COUNTS(5)   TYPE  I,
               OPAMTS      LIKE ZTREQST-ZFOPAMT.
DATA : END   OF IT_TAB3.

DATA : BEGIN OF IT_LAND1 OCCURS 0,
               LAND1       LIKE  T005T-LAND1,
               LANDX       LIKE  T005T-LANDX.
DATA : END   OF IT_LAND1.

DATA : BEGIN OF IT_LIFNR OCCURS 0,
               LIFNR       LIKE  LFA1-LIFNR,
               NAME1       LIKE  LFA1-NAME1.
DATA : END   OF IT_LIFNR.

DATA : W_SUBRC  LIKE  SY-SUBRC,
       W_LINE   TYPE  I,
       W_AMDNO  LIKE  ZTREQST-ZFAMDNO,
       W_OPAMT  LIKE  ZTREQST-ZFOPAMT,
       W_CNT(1),
       SV_CODE(10) TYPE  C,
       SV_WAERS    LIKE  ZTREQHD-ZFWAERS,
       W_TEXT(23)  TYPE  C,
       W_CHAR(1)   TYPE  N,
       P_BUKRS     LIKE  ZTREQHD-BUKRS,
       W_DD_CNT    TYPE  I,
       W_TMP(20)   TYPE  C,
       W_LINE_SIZE TYPE  I.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
   SELECT-OPTIONS: S_BUKRS    FOR ZTREQHD-BUKRS   NO-EXTENSION
                                                  NO INTERVALS,
                   S_EKGRP    FOR ZTREQST-EKGRP,     "Purchasing Group
                   S_WERKS    FOR ZTREQHD-ZFWERKS,   "Plant
                   S_OPNDT    FOR ZTREQST-ZFOPNDT OBLIGATORY.
   SELECTION-SCREEN SKIP.

   PARAMETER : R_COUN   RADIOBUTTON GROUP GP1,     "Country.
               R_BANK   RADIOBUTTON GROUP GP1,     "Bank.
               R_JEWG   RADIOBUTTON GROUP GP1,     "Funds.
               R_VEN    RADIOBUTTON GROUP GP1,     "Vendor.
               R_INCO   RADIOBUTTON GROUP GP1,     "Incoterms.
               R_TERM   RADIOBUTTON GROUP GP1.     "Payment Term.
SELECTION-SCREEN END   OF BLOCK B1.

INITIALIZATION.
  PERFORM P1000_SET_BUKRS.
  PERFORM P1000_INITIALIZATION.

START-OF-SELECTION.
     PERFORM P1000_READ_DATA.
     IF W_SUBRC = 4.
        MESSAGE S191 WITH 'Import Reques Doc.'.  EXIT.
     ENDIF.

     PERFORM P1000_CHECK_DATA.

     PERFORM P1000_WRITE_DATA.

TOP-OF-PAGE.
  PERFORM P1000_TOP_PAGE.

*&---------------------------------------------------------------------*
*&      Form  P1000_INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_INITIALIZATION.
  SET TITLEBAR 'ZIMR05' .
  CONCATENATE SY-DATUM(6) '01' INTO S_OPNDT-LOW.
  S_OPNDT-HIGH = SY-DATUM.
  APPEND S_OPNDT.
ENDFORM.                    " P1000_INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_DATA.

* L/C MAX AMEND Seq. SELECT.
   SELECT ZFREQNO  MAX( ZFAMDNO ) AS ZFAMDNO
   INTO   CORRESPONDING FIELDS OF TABLE IT_TEMP
   FROM   ZTREQST
   WHERE  ZFDOCST     EQ   'O'
   GROUP BY ZFREQNO.

* Import Result of Country.
  IF  R_COUN = 'X'.

      SELECT B~ZFREQNO  C~LAND1    AS CODE A~ZFMATGB
             A~WAERS    A~ZFLASTAM AS ZFOPAMT
      INTO   CORRESPONDING FIELDS OF TABLE IT_TAB1
      FROM ( ZTREQST  AS  B  INNER JOIN  ZTREQHD AS A
      ON     B~ZFREQNO       EQ    A~ZFREQNO )
      INNER  JOIN LFA1 AS C
      ON     A~LIFNR         EQ    C~LIFNR
      FOR    ALL  ENTRIES    IN    IT_TEMP
      WHERE  B~ZFREQNO       EQ    IT_TEMP-ZFREQNO
      AND    B~ZFAMDNO       EQ    IT_TEMP-ZFAMDNO
      AND    B~ZFOPNDT       IN    S_OPNDT
      AND    B~EKGRP         IN    S_EKGRP
      AND    A~ZFWERKS       IN    S_WERKS.

      DESCRIBE TABLE IT_TAB1 LINES W_LINE.
      IF W_LINE EQ 0. W_SUBRC = 4. EXIT. ENDIF.

      SELECT LAND1 LANDX INTO CORRESPONDING FIELDS OF TABLE IT_LAND1
      FROM   T005T FOR ALL ENTRIES IN IT_TAB1
      WHERE  SPRAS EQ  SY-LANGU
      AND    LAND1 EQ  IT_TAB1-CODE(3).

* Import Result of Bank.
  ELSEIF R_BANK = 'X'.

      SELECT B~ZFREQNO  A~ZFOPBN AS CODE A~ZFMATGB
             A~WAERS    A~ZFLASTAM AS ZFOPAMT
      INTO   CORRESPONDING FIELDS OF TABLE IT_TAB1
      FROM ( ZTREQST  AS  B  INNER JOIN  ZTREQHD AS A
      ON     B~ZFREQNO       EQ    A~ZFREQNO )
      FOR    ALL  ENTRIES    IN    IT_TEMP
      WHERE  B~ZFREQNO       EQ    IT_TEMP-ZFREQNO
      AND    B~ZFAMDNO       EQ    IT_TEMP-ZFAMDNO
      AND    B~ZFOPNDT       IN    S_OPNDT
      AND    B~EKGRP         IN    S_EKGRP
      AND    A~ZFWERKS       IN    S_WERKS.

      DESCRIBE TABLE IT_TAB1 LINES W_LINE.
      IF W_LINE EQ 0. W_SUBRC = 4. EXIT. ENDIF.

      SELECT LIFNR NAME1 INTO CORRESPONDING FIELDS OF TABLE IT_LIFNR
      FROM   LFA1  FOR ALL ENTRIES IN IT_TAB1
      WHERE  LIFNR EQ IT_TAB1-CODE.

* Import Result of Vendor.
  ELSEIF R_VEN = 'X'.

      SELECT B~ZFREQNO  A~LIFNR  AS CODE A~ZFMATGB
             A~WAERS    A~ZFLASTAM AS ZFOPAMT
      INTO   CORRESPONDING FIELDS OF TABLE IT_TAB1
      FROM ( ZTREQST  AS  B  INNER JOIN  ZTREQHD AS A
      ON     B~ZFREQNO       EQ    A~ZFREQNO )
      FOR    ALL  ENTRIES    IN    IT_TEMP
      WHERE  B~ZFREQNO       EQ    IT_TEMP-ZFREQNO
      AND    B~ZFAMDNO       EQ    IT_TEMP-ZFAMDNO
      AND    B~ZFOPNDT       IN    S_OPNDT
      AND    B~EKGRP         IN    S_EKGRP
      AND    A~ZFWERKS       IN    S_WERKS.

      DESCRIBE TABLE IT_TAB1 LINES W_LINE.
      IF W_LINE EQ 0. W_SUBRC = 4. EXIT. ENDIF.

      SELECT LIFNR NAME1 INTO CORRESPONDING FIELDS OF TABLE IT_LIFNR
      FROM   LFA1  FOR ALL ENTRIES IN IT_TAB1
      WHERE  LIFNR EQ  IT_TAB1-CODE.

* Import Result of Funds..
  ELSEIF R_JEWG = 'X'.

      SELECT B~ZFREQNO  A~ZFJEWGB AS CODE A~ZFMATGB
             A~WAERS    A~ZFLASTAM AS ZFOPAMT
      INTO   CORRESPONDING FIELDS OF TABLE IT_TAB1
      FROM ( ZTREQST  AS  B  INNER JOIN  ZTREQHD AS A
      ON     B~ZFREQNO       EQ    A~ZFREQNO )
      FOR    ALL  ENTRIES    IN    IT_TEMP
      WHERE  B~ZFREQNO       EQ    IT_TEMP-ZFREQNO
      AND    B~ZFAMDNO       EQ    IT_TEMP-ZFAMDNO
      AND    B~ZFOPNDT       IN    S_OPNDT
      AND    B~EKGRP         IN    S_EKGRP
      AND    A~ZFWERKS       IN    S_WERKS.

      DESCRIBE TABLE IT_TAB1 LINES W_LINE.
      IF W_LINE EQ 0. W_SUBRC = 4. EXIT. ENDIF.

* Import Result of Incoterms.
  ELSEIF R_INCO = 'X'.

      SELECT B~ZFREQNO  A~INCO1 AS CODE A~ZFMATGB
             A~WAERS    A~ZFLASTAM AS ZFOPAMT
      INTO   CORRESPONDING FIELDS OF TABLE IT_TAB1
      FROM ( ZTREQST  AS  B  INNER JOIN  ZTREQHD AS A
      ON     B~ZFREQNO       EQ    A~ZFREQNO )
      FOR    ALL  ENTRIES    IN    IT_TEMP
      WHERE  B~ZFREQNO       EQ    IT_TEMP-ZFREQNO
      AND    B~ZFAMDNO       EQ    IT_TEMP-ZFAMDNO
      AND    B~ZFOPNDT       IN    S_OPNDT
      AND    B~EKGRP         IN    S_EKGRP
      AND    A~ZFWERKS       IN    S_WERKS.

      DESCRIBE TABLE IT_TAB1 LINES W_LINE.
      IF W_LINE EQ 0. W_SUBRC = 4. EXIT. ENDIF.

* Import Result of Payment Terms.
  ELSEIF R_TERM = 'X'.

      SELECT B~ZFREQNO  A~ZTERM AS CODE A~ZFMATGB
             A~WAERS    A~ZFLASTAM AS ZFOPAMT
      INTO   CORRESPONDING FIELDS OF TABLE IT_TAB1
      FROM ( ZTREQST  AS  B  INNER JOIN  ZTREQHD AS A
      ON     B~ZFREQNO       EQ    A~ZFREQNO )
      FOR    ALL  ENTRIES    IN    IT_TEMP
      WHERE  B~ZFREQNO       EQ    IT_TEMP-ZFREQNO
      AND    B~ZFAMDNO       EQ    IT_TEMP-ZFAMDNO
      AND    B~ZFOPNDT       IN    S_OPNDT
      AND    B~EKGRP         IN    S_EKGRP
      AND    A~ZFWERKS       IN    S_WERKS.

      DESCRIBE TABLE IT_TAB1 LINES W_LINE.
      IF W_LINE EQ 0. W_SUBRC = 4. EXIT. ENDIF.

  ENDIF.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_CHECK_DATA.

  CLEAR IT_TAB2.
  SORT IT_TAB1 BY CODE WAERS.

  LOOP AT IT_TAB1.

    IF SY-TABIX EQ 1.
       MOVE : IT_TAB1-WAERS TO  SV_WAERS,
              IT_TAB1-CODE  TO  SV_CODE.
    ENDIF.

* 각각의 구분별, 개설통화별로 SUM 및 COUNT.
    IF SV_CODE NE IT_TAB1-CODE.
       MOVE : SV_CODE       TO  IT_TAB2-CODE,
              SV_WAERS      TO  IT_TAB2-WAERS.
       APPEND  IT_TAB2.
       CLEAR   IT_TAB2.
       MOVE : IT_TAB1-CODE  TO  SV_CODE,
              IT_TAB1-WAERS TO  SV_WAERS.
    ELSE.
      IF SV_WAERS NE IT_TAB1-WAERS.
         MOVE : SV_CODE       TO IT_TAB2-CODE,
                SV_WAERS      TO IT_TAB2-WAERS.
         APPEND  IT_TAB2.
         CLEAR   IT_TAB2.
         MOVE : IT_TAB1-CODE  TO SV_CODE,
                IT_TAB1-WAERS TO SV_WAERS.
      ENDIF.
    ENDIF.

    IF IT_TAB1-ZFMATGB = '1'.
       ADD : 1                 TO  IT_TAB2-COUNT1,
             IT_TAB1-ZFOPAMT   TO  IT_TAB2-OPAMT1.
    ELSEIF IT_TAB1-ZFMATGB = '2'.
       ADD : 1                 TO  IT_TAB2-COUNT2,
             IT_TAB1-ZFOPAMT   TO  IT_TAB2-OPAMT2.
    ELSEIF IT_TAB1-ZFMATGB = '3'.
       ADD : 1                 TO  IT_TAB2-COUNT3,
             IT_TAB1-ZFOPAMT   TO  IT_TAB2-OPAMT3.
    ELSEIF IT_TAB1-ZFMATGB = '4'.
       ADD : 1                 TO  IT_TAB2-COUNT4,
             IT_TAB1-ZFOPAMT   TO  IT_TAB2-OPAMT4.
    ELSEIF IT_TAB1-ZFMATGB = '5'.
       ADD : 1                 TO  IT_TAB2-COUNT5,
             IT_TAB1-ZFOPAMT   TO  IT_TAB2-OPAMT5.
    ELSEIF IT_TAB1-ZFMATGB = '6'.
       ADD : 1                 TO  IT_TAB2-COUNT6,
             IT_TAB1-ZFOPAMT   TO  IT_TAB2-OPAMT6.
    ELSEIF IT_TAB1-ZFMATGB = '7'.
       ADD : 1                 TO  IT_TAB2-COUNT7,
             IT_TAB1-ZFOPAMT   TO  IT_TAB2-OPAMT7.
    ELSEIF IT_TAB1-ZFMATGB = '8'.
       ADD : 1                 TO  IT_TAB2-COUNT8,
             IT_TAB1-ZFOPAMT   TO  IT_TAB2-OPAMT8.
    ELSEIF IT_TAB1-ZFMATGB = '9'.
       ADD : 1                 TO  IT_TAB2-COUNT9,
             IT_TAB1-ZFOPAMT   TO  IT_TAB2-OPAMT9.
    ELSEIF IT_TAB1-ZFMATGB = '10'.
       ADD : 1                 TO  IT_TAB2-COUNT10,
             IT_TAB1-ZFOPAMT   TO  IT_TAB2-OPAMT10.
    ENDIF.

    ADD : 1               TO  IT_TAB2-COUNTS,
          IT_TAB1-ZFOPAMT TO  IT_TAB2-OPAMTS.

  ENDLOOP.
  MOVE : SV_CODE       TO IT_TAB2-CODE,
         SV_WAERS      TO IT_TAB2-WAERS.
  APPEND IT_TAB2.

ENDFORM.                    " P1000_CHECK_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_WRITE_DATA.

  SET TITLEBAR  'ZIMR05'.
  SORT IT_TAB2 BY CODE WAERS.

  SELECT COUNT( * )  INTO  W_DD_CNT
  FROM   DD07T
  WHERE  DOMNAME     EQ    'ZDMATGB'
  AND    DDLANGUAGE  EQ    'E'.

  W_LINE_SIZE  = 60  + ( 27 * W_DD_CNT ).
  NEW-PAGE LINE-SIZE W_LINE_SIZE NO-HEADING .

  LOOP AT IT_TAB2.

    IF W_CNT = 1.
       W_CNT = 2.   FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
       W_CNT = 1.   FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.

    CLEAR : W_TEXT, IT_LAND1, IT_LIFNR.
    IF R_COUN = 'X'.
       READ TABLE IT_LAND1 WITH KEY LAND1 = IT_TAB2-CODE.
       W_TEXT = IT_LAND1-LANDX.
    ELSEIF  R_BANK = 'X'.
       READ TABLE IT_LIFNR WITH KEY LIFNR = IT_TAB2-CODE.
       W_TEXT = IT_LIFNR-NAME1.
    ELSEIF  R_VEN = 'X'.
       READ TABLE IT_LIFNR WITH KEY LIFNR = IT_TAB2-CODE.
       W_TEXT = IT_LIFNR-NAME1.
    ELSEIF  R_JEWG EQ 'X'.
       SELECT SINGLE DDTEXT INTO  W_TEXT
       FROM   DD07T
       WHERE  DOMNAME       EQ    'ZDJEWGB'
       AND    DDLANGUAGE    EQ    'E'
       AND    VALPOS        EQ    IT_TAB2-CODE.
    ELSEIF R_TERM EQ 'X'.
       SELECT SINGLE ZFTRTX1  INTO W_TEXT
       FROM   ZTIMIMG01
       WHERE  ZTERM      EQ  IT_TAB2-CODE
       AND    ZFAPLDT    EQ  ( SELECT MAX( ZFAPLDT )
                               FROM   ZTIMIMG01
                               WHERE  ZTERM   EQ  IT_TAB2-CODE
                               AND    ZFAPLDT LT  SY-DATUM ).
    ELSE.
       MOVE IT_TAB2-CODE TO W_TEXT.
    ENDIF.

    WRITE:/'|' NO-GAP,
           (23) W_TEXT NO-GAP, '|',
           (05) IT_TAB2-WAERS  NO-GAP, '|'.

    CLEAR : W_CHAR.
    DO W_DD_CNT TIMES.

       W_CHAR  =  W_CHAR  + 1.

       CONCATENATE  'IT_TAB2-COUNT'  W_CHAR  INTO  W_TMP.
       ASSIGN   (W_TMP)  TO  <FS_F>.

       CONCATENATE  'IT_TAB2-OPAMT'  W_CHAR  INTO  W_TMP.
       ASSIGN   (W_TMP)  TO  <FS_F2>.

       WRITE: (05) <FS_F> NO-GAP, '|',
              (18) <FS_F2> CURRENCY IT_TAB2-WAERS NO-GAP, '|'.
    ENDDO.
    WRITE: (05) IT_TAB2-COUNTS NO-GAP, '|',
           (18) IT_TAB2-OPAMTS CURRENCY IT_TAB2-WAERS NO-GAP, '|'.

  ENDLOOP.

* 통화별로 소계 SUM.
  SORT IT_TAB2 BY WAERS.
  LOOP AT IT_TAB2.

    IF SY-TABIX EQ 1.
       MOVE : IT_TAB2-WAERS TO  SV_WAERS.
    ENDIF.

* 각각의 구분별, 개설통화별로 SUM 및 COUNT.
    IF SV_WAERS NE IT_TAB2-WAERS.
       MOVE : SV_WAERS      TO IT_TAB3-WAERS.
       APPEND  IT_TAB3.
       CLEAR   IT_TAB3.
       MOVE : IT_TAB2-WAERS TO SV_WAERS.
    ENDIF.

    CLEAR : W_CHAR.
    DO W_DD_CNT TIMES.

       W_CHAR  =  W_CHAR  + 1.

       CONCATENATE  'IT_TAB2-COUNT'  W_CHAR  INTO  W_TMP.
       ASSIGN   (W_TMP)  TO  <FS_F>.

       CONCATENATE  'IT_TAB2-OPAMT'  W_CHAR  INTO  W_TMP.
       ASSIGN   (W_TMP)  TO  <FS_F2>.

       CONCATENATE  'IT_TAB3-COUNT'  W_CHAR  INTO  W_TMP.
       ASSIGN   (W_TMP)  TO  <FS_F3>.

       CONCATENATE  'IT_TAB3-OPAMT'  W_CHAR  INTO  W_TMP.
       ASSIGN   (W_TMP)  TO  <FS_F4>.

       ADD  : <FS_F>     TO   <FS_F3>,
              <FS_F2>    TO   <FS_F4>.
    ENDDO.

    ADD  : IT_TAB2-COUNTS     TO   IT_TAB3-COUNTS,
           IT_TAB2-OPAMTS     TO   IT_TAB3-OPAMTS.

  ENDLOOP.

  MOVE : SV_WAERS      TO IT_TAB3-WAERS.
  APPEND IT_TAB3.

  PERFORM P3000_TOT_LINE_WRITE.

ENDFORM.                    " P1000_WRITE_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_TOP_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_TOP_PAGE.
  SKIP 1.
  IF  R_COUN = 'X'.
    WRITE:/65 '   Open Result List by County   '  COLOR 1.
  ELSEIF  R_BANK = 'X'.
    WRITE:/65 '   Open Result List by Bank     '  COLOR 1.
  ELSEIF  R_JEWG = 'X'.
    WRITE:/65 '   Open Result List by Fund     '  COLOR 1.
  ELSEIF  R_INCO = 'X'.
    WRITE:/65 '  Open Result List by Incoterms '  COLOR 1.
  ELSEIF  R_TERM = 'X'.
    WRITE:/65 'Open Result List by payment term ' COLOR 1.
  ENDIF.

  WRITE:/6 'Open Period :', S_OPNDT-LOW, '-', S_OPNDT-HIGH.

  ULINE.
  FORMAT COLOR 1 INTENSIFIED OFF.
  WRITE:/'|' NO-GAP,
         (23) '      Section  ' NO-GAP, '|',
         (05) 'Cur.'            NO-GAP, '|'.

  SET LEFT SCROLL-BOUNDARY.

  SELECT  *  FROM DD07T
  WHERE   DOMNAME     EQ   'ZDMATGB'
  AND     DDLANGUAGE  EQ   'E'.
     WRITE: (05) ' Case' NO-GAP, '|',
            (18) DD07T-DDTEXT  RIGHT-JUSTIFIED NO-GAP, '|'.
  ENDSELECT.

  WRITE: (05) ' Case' NO-GAP, '|',
         (18) 'Sub Total'   RIGHT-JUSTIFIED NO-GAP, '|'.
  ULINE.

ENDFORM.                    " P1000_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_TOT_LINE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TOT_LINE_WRITE.

  SORT IT_TAB3 BY WAERS.

  LOOP AT IT_TAB3.

    IF W_CNT = 1.
       W_CNT = 2.   FORMAT COLOR 3 INTENSIFIED OFF.
    ELSE.
       W_CNT = 1.   FORMAT COLOR 3 INTENSIFIED ON.
    ENDIF.

    CLEAR : W_TEXT, IT_LAND1, IT_LIFNR.
    IF SY-TABIX EQ 1.
       WRITE : / SY-ULINE.
       MOVE '      Total      '    TO  W_TEXT.
    ELSE.
       CLEAR : W_TEXT.
    ENDIF.
    WRITE:/'|' NO-GAP,
           (23) W_TEXT NO-GAP, '|',
           (05) IT_TAB3-WAERS  NO-GAP, '|'.

    CLEAR : W_CHAR.
    DO W_DD_CNT TIMES.

       W_CHAR  =  W_CHAR  + 1.

       CONCATENATE  'IT_TAB3-COUNT'  W_CHAR  INTO  W_TMP.
       ASSIGN   (W_TMP)  TO  <FS_F>.

       CONCATENATE  'IT_TAB3-OPAMT'  W_CHAR  INTO  W_TMP.
       ASSIGN   (W_TMP)  TO  <FS_F2>.

       WRITE: (05) <FS_F> NO-GAP, '|',
              (18) <FS_F2> CURRENCY IT_TAB3-WAERS NO-GAP, '|'.
    ENDDO.

    WRITE: (05) IT_TAB3-COUNTS NO-GAP, '|',
           (18) IT_TAB3-OPAMTS CURRENCY IT_TAB3-WAERS NO-GAP, '|'.

  ENDLOOP.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TOT_LINE_WRITE
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

*>> 회사코드 SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
