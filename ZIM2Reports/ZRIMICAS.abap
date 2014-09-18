*&---------------------------------------------------------------------*
*& Report  ZRIMICAS                                                    *
*&---------------------------------------------------------------------*
*&ABAP Name : ZRIMICAS                                                 *
*&Created by: 나신호 INFOLINK.Ltd                                      *
*&Created on: 07/28/2000                                               *
*&Version   : 1.0                                                      *
*&---------------------------------------------------------------------*
* 수입부대비 발생 현?
*&---------------------------------------------------------------------*

REPORT  ZRIMICAS       NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 140
                       LINE-COUNT 65.

TABLES : ZTRECST,                      " 수입의뢰 비용.
         ZTBLCST,                      " B/L 비용.
         ZTCGCST,                      " 하역비용.
         ZTCUCLCST,                    " 통관 비용.
         WMTO_S.

DATA : BEGIN OF IT_TAB1 OCCURS 0,
               CODE(1)     TYPE C,
               CDTEXT(12)  TYPE C,
               DOCNO(10)   TYPE C,
               ZFOCDT      LIKE ZTBLCST-ZFOCDT,
               ZFCSCD      LIKE ZTBLCST-ZFCSCD,
               TEXT(30)    TYPE C,
               ZFPAY       LIKE ZTBLCST-ZFPAY,
               ZFCKAMT     LIKE ZTBLCST-ZFCKAMT,
               ZFVAT       LIKE ZTBLCST-ZFVAT,
               MWSKZ       LIKE ZTBLCST-MWSKZ,
               ZTERM       LIKE ZTBLCST-ZTERM,
               ZFACDO      LIKE ZTBLCST-ZFACDO,
               BUKRS       LIKE ZTBLCST-BUKRS,
               ZFPSDT      LIKE ZTBLCST-ZFPSDT.
DATA : END   OF IT_TAB1.

DATA : BEGIN OF IT_RE OCCURS 0.
                INCLUDE STRUCTURE ZTRECST.
DATA : END   OF IT_RE.

DATA : BEGIN OF IT_BL OCCURS 0.
                INCLUDE STRUCTURE ZTBLCST.
DATA : END   OF IT_BL.

DATA : BEGIN OF IT_CG OCCURS 0.
                INCLUDE STRUCTURE ZTCGCST.
DATA : END   OF IT_CG.

DATA : BEGIN OF IT_CU OCCURS 0.
                INCLUDE STRUCTURE ZTCUCLCST.
DATA : END   OF IT_CU.

DATA : BEGIN OF IT_CODE OCCURS 0,
               ZFCDTY    LIKE  ZTIMIMG08-ZFCDTY,
               ZFCD      LIKE  ZTIMIMG08-ZFCD,
               ZFCDNM    LIKE  ZTIMIMG08-ZFCDNM.
DATA : END   OF IT_CODE.

DATA : W_SUBRC   LIKE  SY-SUBRC,
       W_TABIX(4) TYPE C.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
   SELECT-OPTIONS: S_BUKRS    FOR ZTRECST-BUKRS,        " 회사코드.
                   S_OCDT     FOR SY-DATUM OBLIGATORY,  " 발생기?
                   S_WERKS    FOR ZTRECST-ZFWERKS.      " 플랜?
   PARAMETERS    : P_RE       AS CHECKBOX,              " 수입의뢰비?
                   P_BL       AS CHECKBOX,              " B/L 비?
                   P_CG       AS CHECKBOX,              " 하역비용.
                   P_CU       AS CHECKBOX.              " 통관비?
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM P1000_INITIALIZATION.

START-OF-SELECTION.
  PERFORM P1000_READ_DATA.

  PERFORM P1000_CHECK_DATA.
  IF IT_TAB1[] IS INITIAL.
     MESSAGE S191 WITH '비용 DATA'.  EXIT.
  ENDIF.

  SET PF-STATUS 'PF1000'.
  SET TITLEBAR  'TI1000'.
  PERFORM P1000_WRITE_DATA.

TOP-OF-PAGE.

AT USER-COMMAND.
  CHECK SY-UCOMM = 'DOWN'.
  PERFORM P1000_DOWNLOAD.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_DATA.
  IF P_RE = 'X'.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_RE FROM ZTRECST
      WHERE ZFOCDT IN S_OCDT AND ZFWERKS IN S_WERKS.
  ENDIF.
  IF  P_BL = 'X'.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BL FROM ZTBLCST
      WHERE ZFOCDT IN S_OCDT AND ZFWERKS IN S_WERKS.
  ENDIF.
  IF  P_CG = 'X'.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_CG FROM ZTCGCST
      WHERE ZFOCDT IN S_OCDT AND WERKS  IN S_WERKS.
  ENDIF.
  IF  P_CU = 'X'.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_CU FROM ZTCUCLCST
      WHERE ZFOCDT IN S_OCDT AND ZFWERKS IN S_WERKS.
  ENDIF.

  SELECT ZFCDTY ZFCD ZFCDNM INTO CORRESPONDING FIELDS OF TABLE IT_CODE
    FROM ZTIMIMG08  WHERE ZFCDTY IN ('003', '004', '005', '006', '007').

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

IF P_RE = 'X'.
  LOOP AT IT_RE.
    CLEAR : IT_CODE, IT_TAB1.
    READ TABLE IT_CODE WITH KEY ZFCDTY = '003' ZFCD = IT_RE-ZFCSCD.
    IF IT_RE-ZFCKAMT LE 0. CONTINUE. ENDIF.

    MOVE : '1'                 TO  IT_TAB1-CODE,
           '수입의뢰비용'      TO  IT_TAB1-CDTEXT,
            IT_RE-ZFREQNO      TO  IT_TAB1-DOCNO,
            IT_RE-ZFOCDT       TO  IT_TAB1-ZFOCDT,
            IT_RE-ZFCSCD       TO  IT_TAB1-ZFCSCD,
            IT_CODE-ZFCDNM(30) TO  IT_TAB1-TEXT,
            IT_RE-ZFPAY        TO  IT_TAB1-ZFPAY,
            IT_RE-ZFCKAMT      TO  IT_TAB1-ZFCKAMT,
            IT_RE-ZFVAT        TO  IT_TAB1-ZFVAT,
            IT_RE-MWSKZ        TO  IT_TAB1-MWSKZ,
            IT_RE-ZTERM        TO  IT_TAB1-ZTERM,
            IT_RE-ZFACDO       TO  IT_TAB1-ZFACDO,
            IT_RE-ZFPSDT       TO  IT_TAB1-ZFPSDT.
     APPEND IT_TAB1.  CLEAR : IT_TAB1.
  ENDLOOP.
ENDIF.
IF P_BL = 'X'.
   LOOP AT IT_BL.
    CLEAR : IT_CODE, IT_TAB1.
    IF IT_BL-ZFCSQ < '10000'.
       READ TABLE IT_CODE WITH KEY ZFCDTY = '004' ZFCD = IT_BL-ZFCSCD.
    ELSE.
       READ TABLE IT_CODE WITH KEY ZFCDTY = '005' ZFCD = IT_BL-ZFCSCD.
    ENDIF.
    IF IT_BL-ZFCKAMT LE 0. CONTINUE. ENDIF.
    MOVE : '2'                 TO  IT_TAB1-CODE,
           'B/L 비용'           TO  IT_TAB1-CDTEXT,
            IT_BL-ZFBLNO       TO  IT_TAB1-DOCNO,
            IT_BL-ZFOCDT       TO  IT_TAB1-ZFOCDT,
            IT_BL-ZFCSCD       TO  IT_TAB1-ZFCSCD,
            IT_CODE-ZFCDNM(30) TO  IT_TAB1-TEXT,
            IT_BL-ZFPAY        TO  IT_TAB1-ZFPAY,
            IT_BL-ZFCKAMT      TO  IT_TAB1-ZFCKAMT,
            IT_BL-ZFVAT        TO  IT_TAB1-ZFVAT,
            IT_BL-MWSKZ        TO  IT_TAB1-MWSKZ,
            IT_BL-ZTERM        TO  IT_TAB1-ZTERM,
            IT_BL-ZFACDO       TO  IT_TAB1-ZFACDO,
            IT_BL-ZFPSDT       TO  IT_TAB1-ZFPSDT.
     APPEND IT_TAB1.  CLEAR : IT_TAB1.
  ENDLOOP.
ENDIF.
IF P_CG = 'X'.
   LOOP AT IT_CG.
    CLEAR : IT_CODE, IT_TAB1.
    READ TABLE IT_CODE WITH KEY ZFCDTY = '007' ZFCD = IT_CG-ZFCSCD.
    IF IT_CG-ZFCKAMT LE 0. CONTINUE. ENDIF.
    MOVE : '3'                 TO  IT_TAB1-CODE,
           '하역 비용'         TO  IT_TAB1-CDTEXT,
            IT_CG-ZFCGNO       TO  IT_TAB1-DOCNO,
            IT_CG-ZFOCDT       TO  IT_TAB1-ZFOCDT,
            IT_CG-ZFCSCD       TO  IT_TAB1-ZFCSCD,
            IT_CODE-ZFCDNM(30) TO  IT_TAB1-TEXT,
            IT_CG-ZFPAY        TO  IT_TAB1-ZFPAY,
            IT_CG-ZFCKAMT      TO  IT_TAB1-ZFCKAMT,
            IT_CG-ZFVAT        TO  IT_TAB1-ZFVAT,
            IT_CG-MWSKZ        TO  IT_TAB1-MWSKZ,
            IT_CG-ZTERM        TO  IT_TAB1-ZTERM,
            IT_CG-BELNR        TO  IT_TAB1-ZFACDO,
            IT_CG-ZFPSDT       TO  IT_TAB1-ZFPSDT.
     APPEND IT_TAB1.  CLEAR : IT_TAB1.
  ENDLOOP.
ENDIF.
IF P_CU = 'X'.
   LOOP AT IT_CU.
    CLEAR : IT_CODE, IT_TAB1.
    READ TABLE IT_CODE WITH KEY ZFCDTY = '006' ZFCD = IT_CU-ZFCSCD.
    IF IT_CU-ZFCAMT LE 0. CONTINUE. ENDIF.

    MOVE : '4'                 TO  IT_TAB1-CODE,
           '통관비용'          TO  IT_TAB1-CDTEXT,
            IT_CU-ZFBLNO       TO  IT_TAB1-DOCNO,
            IT_CU-ZFOCDT       TO  IT_TAB1-ZFOCDT,
            IT_CU-ZFCSCD       TO  IT_TAB1-ZFCSCD,
            IT_CODE-ZFCDNM(30) TO  IT_TAB1-TEXT,
            IT_CU-ZFPAY        TO  IT_TAB1-ZFPAY,
            IT_CU-ZFCAMT       TO  IT_TAB1-ZFCKAMT,
            IT_CU-ZFVAT        TO  IT_TAB1-ZFVAT,
            IT_CU-MWSKZ        TO  IT_TAB1-MWSKZ,
            IT_CU-ZTERM        TO  IT_TAB1-ZTERM,
            IT_CU-ZFACDO       TO  IT_TAB1-ZFACDO,
            IT_CU-ZFPSDT       TO  IT_TAB1-ZFPSDT.
     APPEND IT_TAB1.  CLEAR : IT_TAB1.
  ENDLOOP.
ENDIF.
ENDFORM.                    " P1000_CHECK_DATA

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
  SET LEFT SCROLL-BOUNDARY COLUMN 31.
  FORMAT COLOR OFF.
  WRITE:/50 '   수 입 부 대 비 발 생 현 황   ' COLOR 1.
  FORMAT COLOR OFF.
  WRITE :/6  '*' NO-GAP, IT_TAB1-CDTEXT, 120 'DATE :', SY-DATUM,
         /6  '발생기간 :', S_OCDT-LOW, '-', S_OCDT-HIGH,
         120 'PAGE :', SY-PAGNO.

  FORMAT COLOR 1 INTENSIFIED OFF.
  ULINE.
  WRITE:/'|' NO-GAP,
         (04) 'Seq.',
         (10) '문서 번호',
         (10) '발생일',
         (37) '비용구분 / Description',
*         (04) 'CODE',
*         (29) 'IT_TAB1-TEXT,
         (10) '지불처',
         (16) '비용금액(\)  '  RIGHT-JUSTIFIED,
         (15) '세  금   ' RIGHT-JUSTIFIED,
         (02) 'VA',
         (04) 'TERM',
         (10) '회계 Doc.',
         (10) '회계처리일' NO-GAP, '|' NO-GAP.
  ULINE.
ENDFORM.                    " P1000_TOP_PAGE

*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_WRITE_DATA.
  CLEAR : W_SUBRC, W_TABIX.
  SORT IT_TAB1 BY CODE.

  LOOP AT IT_TAB1.

    W_TABIX = SY-TABIX.
    AT NEW CDTEXT.
       NEW-PAGE.
       PERFORM  P1000_TOP_PAGE.
    ENDAT.

    IF W_SUBRC = 1.
       W_SUBRC = 2.    FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
       W_SUBRC = 1.    FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.

    WRITE:/'|' NO-GAP,
           (04) W_TABIX RIGHT-JUSTIFIED,
           (10) IT_TAB1-DOCNO,
           (10) IT_TAB1-ZFOCDT,
           (03) IT_TAB1-ZFCSCD,
           (33) IT_TAB1-TEXT,
           (10) IT_TAB1-ZFPAY,
           (16) IT_TAB1-ZFCKAMT CURRENCY 'KRW',
           (15) IT_TAB1-ZFVAT CURRENCY 'KRW',
           (02) IT_TAB1-MWSKZ,
           (04) IT_TAB1-ZTERM,
           (10) IT_TAB1-ZFACDO,
           (10) IT_TAB1-ZFPSDT NO-GAP, '|' NO-GAP.
    ULINE.

    AT END OF CDTEXT.
       SUM.
       FORMAT COLOR 3 INTENSIFIED OFF.
       WRITE:/      '|' NO-GAP,
              6     IT_TAB1-CDTEXT, '합계',
             78(16) IT_TAB1-ZFCKAMT CURRENCY 'KRW',
               (15) IT_TAB1-ZFVAT CURRENCY 'KRW',
            140     '|' NO-GAP.
       ULINE.
    ENDAT.
    AT LAST.
       SUM.
       FORMAT COLOR 3 INTENSIFIED OFF.
       WRITE:/      '|' NO-GAP,
              3     '총  합  계',
             78(16) IT_TAB1-ZFCKAMT CURRENCY 'KRW',
               (15) IT_TAB1-ZFVAT CURRENCY 'KRW',
            140     '|' NO-GAP.
         ULINE.
    ENDAT.

  ENDLOOP.
ENDFORM.                    " P1000_WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_INITIALIZATION.
  CONCATENATE SY-DATUM(6) '01' INTO S_OCDT-LOW.
  S_OCDT-HIGH = SY-DATUM.
  APPEND S_OCDT.
  P_RE = 'X'.
  P_BL = 'X'.
  P_CG = 'X'.
  P_CU = 'X'.
ENDFORM.                    " P1000_INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  P1000_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_DOWNLOAD.
  LOOP AT IT_TAB1.
       WMTO_S-AMOUNT =  IT_TAB1-ZFCKAMT.
       CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
           EXPORTING
               CURRENCY  =  'KRW'
               AMOUNT_INTERNAL = WMTO_S-AMOUNT
           IMPORTING
               AMOUNT_DISPLAY  = WMTO_S-AMOUNT
           EXCEPTIONS
               INTERNAL_ERROR = 1.
       IT_TAB1-ZFCKAMT =  WMTO_S-AMOUNT.

       WMTO_S-AMOUNT   =  IT_TAB1-ZFVAT.
       CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
           EXPORTING
               CURRENCY  =  'KRW'
               AMOUNT_INTERNAL = WMTO_S-AMOUNT
           IMPORTING
               AMOUNT_DISPLAY  = WMTO_S-AMOUNT
           EXCEPTIONS
               INTERNAL_ERROR = 1.
     IT_TAB1-ZFVAT =  WMTO_S-AMOUNT .

     MODIFY IT_TAB1.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
        EXPORTING
        FILENAME = 'C:\TEMP\TEMP.TXT'
        FILETYPE = 'DAT'
   TABLES
       DATA_TAB = IT_TAB1.

ENDFORM.                    " P1000_DOWNLOAD
