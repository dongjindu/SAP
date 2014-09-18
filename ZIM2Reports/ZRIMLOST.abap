*&---------------------------------------------------------------------*
*& Report  ZRIMLOST                                                    *
*&---------------------------------------------------------------------*
*&ABAP Name : ZRIMLOST                                                 *
*&Created on: 07/18/2000                                               *
*&Version   : 1.0                                                      *
*&---------------------------------------------------------------------*
* 수입의뢰 관리 번호 / AMAND 번호 별로 진행 현황을 보여준다.
*&---------------------------------------------------------------------*

REPORT  ZRIMLOST       NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 170.
*                       LINE-COUNT 65.

TABLES : ZTREQHD,                      " 수입의뢰 Header
         ZTREQST,                      " 수입의뢰 상태(Status)
         ZTREQHD_TMP,                  " 수입의뢰 Header Amend용 Temp
         LFA1,
         WMTO_S.

DATA : BEGIN OF IT_TAB OCCURS 0,
               ZFWERKS     LIKE  ZTREQHD-ZFWERKS,
               EBELN       LIKE  ZTREQHD-EBELN,
               ZFREQNO     LIKE  ZTREQHD-ZFREQNO,
               ZFAMDNO     LIKE  ZTREQST-ZFAMDNO,
               ZFRLDT1     LIKE  ZTREQST-ZFRLDT1,
               ZFRVDT      LIKE  ZTREQST-ZFRVDT,
               ZFREQDT     LIKE  ZTREQST-ZFREQDT,
               ZFAPPDT     LIKE  ZTREQST-ZFAPPDT,
               ZFOPNDT     LIKE  ZTREQST-ZFOPNDT,
               WAERS       LIKE  ZTREQST-WAERS,
               ZFOPAMT     LIKE  ZTREQST-ZFOPAMT,
               ZFMATGB     LIKE  ZTREQHD-ZFMATGB,
               ZFREQTY     LIKE  ZTREQST-ZFREQTY,
               ZFBACD      LIKE  ZTREQHD-ZFBACD,
               INCO1       LIKE  ZTREQHD-INCO1,
               ZFTRANS     LIKE  ZTREQHD-ZFTRANS,
               ZFBENI      LIKE  ZTREQHD-ZFBENI,
*               LIFNR       LIKE  ZTREQHD-LIFNR,
               ZFSHCU      LIKE  ZTREQHD-ZFSHCU,
               MAKTX       LIKE  ZTREQHD-MAKTX,
               ZFOPBN      LIKE  ZTREQHD-ZFOPBN,
               ZFJEWGB     LIKE  ZTREQHD-ZFJEWGB,
               ZFLEPD      LIKE  ZTREQHD-ZFLEPD,
               ZFPREPAY    LIKE  ZTREQHD-ZFPREPAY,
               ZFOPNNO     LIKE  ZTREQHD-ZFOPNNO,
               ZFLASTED    LIKE  ZTREQHD-ZFLASTED,
               ZFLASTSD    LIKE  ZTREQHD-ZFLASTSD.
DATA : END   OF IT_TAB.

DATA : BEGIN OF IT_TMP1  OCCURS 0,
               ZFREQNO   LIKE  ZTREQHD_TMP-ZFREQNO,
               ZFAMDNO   LIKE  ZTREQHD_TMP-ZFAMDNO,
               INCO1     LIKE  ZTREQHD_TMP-INCO1,
               ZFTRANS   LIKE  ZTREQHD_TMP-ZFTRANS,
               ZFSHCU    LIKE  ZTREQHD_TMP-ZFSHCU,
               MAKTX     LIKE  ZTREQHD_TMP-MAKTX.
DATA : END   OF IT_TMP1.

DATA : BEGIN OF IT_LIFNR  OCCURS 0,
              LIFNR    LIKE  LFA1-LIFNR,
              NAME1    LIKE  LFA1-NAME1,
              LAND1    LIKE  LFA1-LAND1.
DATA : END   OF IT_LIFNR.

DATA : W_ZFAMDNO   LIKE  ZTREQST-ZFAMDNO.

DATA : W_SUBRC   LIKE  SY-SUBRC,
       W_TABIX(4),
       W_TABCNT(5),
       W_FIELD_NM        LIKE DD03D-FIELDNAME.   " 필드?

INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
   SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                                NO INTERVALS,
                   S_WERKS    FOR ZTREQHD-ZFWERKS,   "Plant
*                   S_RLDT1    FOR ZTREQST-ZFRLDT1,   "의뢰Release ?
*                   S_RLDT2    FOR ZTREQST-ZFRLDT2,   "개설Release ?
                   S_OPNDT    FOR ZTREQST-ZFOPNDT,   "개설일.
                   S_EBELN    FOR ZTREQHD-EBELN,     " Po No.
                   S_REQNO    FOR ZTREQHD-ZFREQNO,   " 수입의뢰번?
                   S_REQTY    FOR ZTREQST-ZFREQTY,   " 결재구?
                   S_MATGB    FOR ZTREQHD-ZFMATGB,   " 자재구?
*                  S_EBELN    FOR ZTREQHD-EBELN,     " 용도구?
                   S_JEWGB    FOR ZTREQHD-ZFJEWGB,   " 재원구?
                   S_BACD     FOR ZTREQHD-ZFBACD,    " 송금구?
*                  S_MATGB    FOR ZTREQHD-ZFMATGB,   " 구매담당?
                   S_SHCU     FOR ZTREQHD-ZFSHCU,    " 선적?
                   S_TRANS    FOR ZTREQHD-ZFTRANS,   " 운송방?
                   S_OPBN     FOR ZTREQHD-ZFOPBN,    " 개설은?
                   S_LEVN     FOR ZTREQHD-ZFLEVN,    " 차입기?
                   S_BENI     FOR ZTREQHD-ZFBENI.    " Beneficiary
   SELECTION-SCREEN SKIP.
   PARAMETERS : P_OPNDT   AS CHECKBOX.               " 개설일 누락?

SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM P1000_INITIALIZATION.

START-OF-SELECTION.
  PERFORM P1000_READ_DATA.
  IF W_SUBRC = 4.
     MESSAGE S191 WITH '수입의뢰문서'.  EXIT.
  ENDIF.

  PERFORM P1000_CHECK_DATA.

  SET PF-STATUS 'PF1000'.
  SET TITLEBAR  'TI1000'.
  PERFORM P1000_TOP_PAGE.
  SORT IT_TAB.
  PERFORM P1000_WRITE_DATA.

*TOP-OF-PAGE.
*  PERFORM P1000_TOP_PAGE.

AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'DOWN'.
          PERFORM P1000_DOWNLOAD.
    WHEN 'STUP' OR 'STDN'.         " SORT 선택?
         W_FIELD_NM = 'EBELN'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
*         GET CURSOR FIELD W_FIELD_NM.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
         CLEAR : IT_TAB.

  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_DATA.
  SELECT A~ZFWERKS A~EBELN   A~ZFREQNO A~ZFMATGB
         A~ZFBACD  A~INCO1   A~ZFTRANS A~ZFBENI
         A~ZFSHCU  A~MAKTX   A~ZFOPBN  A~ZFJEWGB
         A~ZFLEPD  A~ZFPREPAY A~ZFOPNNO A~ZFLASTED A~ZFLASTSD
         B~ZFAMDNO B~ZFRLDT1 B~ZFRVDT  B~ZFREQDT
         B~ZFAPPDT B~WAERS   B~ZFOPAMT B~ZFREQTY
         B~ZFOPNDT
   INTO CORRESPONDING FIELDS OF TABLE IT_TAB
    FROM ( ZTREQHD AS A INNER JOIN ZTREQST AS B
      ON A~ZFREQNO = B~ZFREQNO )
   WHERE A~ZFWERKS  IN  S_WERKS AND
         A~BUKRS    IN  S_BUKRS AND
         A~EBELN    IN  S_EBELN AND
         A~ZFREQNO  IN  S_REQNO AND   " 수입의뢰번?
         A~ZFMATGB  IN  S_MATGB AND   " 자재구?
         A~ZFJEWGB  IN  S_JEWGB AND   " 재원구?
         A~ZFSHCU   IN  S_SHCU  AND   " 선적?
         A~ZFTRANS  IN  S_TRANS AND   " 운송방?
         A~ZFOPBN   IN  S_OPBN  AND   " 개설은?
         A~ZFLEVN   IN  S_LEVN  AND   " 차입기?
         A~ZFBENI   IN  S_BENI  AND   " Beneficiary
         A~ZFBACD   IN  S_BACD  AND   " 송금구분 (사전사후구분 )
*         B~ZFRLDT1  IN  S_RLDT1 AND   " 의뢰 Release 상?
*         B~ZFRLDT2  IN  S_RLDT2 AND   " 개설 Release 상?
         B~ZFOPNDT  IN  S_OPNDT AND   " 개설?
         B~ZFREQTY  IN  S_REQTY.      " 결재구?

*                  S_EBELN    FOR ZTREQHD-EBELN,     " 용도구?
*                  S_MATGB    FOR ZTREQHD-ZFMATGB,   " 구매담당?

  IF SY-SUBRC <> 0.   W_SUBRC = 4.  EXIT.   ENDIF.

  IF P_OPNDT = 'X'.
     DELETE IT_TAB WHERE NOT ZFOPNDT IS INITIAL.
  ENDIF.

  IF IT_TAB[] IS INITIAL.   W_SUBRC = 4.  EXIT.  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TMP1
    FROM ZTREQHD_TMP FOR ALL ENTRIES IN IT_TAB
   WHERE ZFREQNO = IT_TAB-ZFREQNO AND ZFAMDNO = IT_TAB-ZFAMDNO.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_LIFNR
    FROM LFA1 FOR ALL ENTRIES IN IT_TAB
   WHERE LIFNR = IT_TAB-ZFBENI OR LIFNR = IT_TAB-ZFOPBN.

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
  LOOP AT IT_TAB.
    CLEAR : IT_TMP1, W_ZFAMDNO.

    SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO FROM ZTREQST
     WHERE ZFREQNO = IT_TAB-ZFREQNO.

    CHECK W_ZFAMDNO <> '00000'.

    IF IT_TAB-ZFAMDNO <> W_ZFAMDNO.
       READ TABLE IT_TMP1 WITH KEY ZFREQNO = IT_TAB-ZFREQNO
                                   ZFAMDNO = IT_TAB-ZFAMDNO.
       MOVE : IT_TMP1-INCO1   TO  IT_TAB-INCO1,
              IT_TMP1-ZFTRANS TO  IT_TAB-ZFTRANS,
              IT_TMP1-ZFSHCU  TO  IT_TAB-ZFSHCU,
              IT_TMP1-MAKTX   TO  IT_TAB-MAKTX.
       MODIFY IT_TAB.
    ENDIF.

  ENDLOOP.
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
  WRITE:/70 '   수 입 실 적  L I S T   ' COLOR 1.
  WRITE:/150 'DATE :', SY-DATUM.
*  SKIP 1.
  DESCRIBE TABLE IT_TAB LINES W_TABCNT.
  WRITE :/100 'M:자재구분  Re:결제구분  G:사전사후구분',
              'Inc:Incoterms  T:운송방법',
         /6   'TOTAL :', W_TABCNT RIGHT-JUSTIFIED, '건',
          100 'Cou:Country  SPT:선적국가',
              'B:재원구분  Dat:차입기간  Rat:선급금비율'.

  FORMAT COLOR 1 INTENSIFIED OFF.
  ULINE.
  WRITE:/(04) 'Seq.',
         (04) 'Plant',
         (10) 'P/O No.',
         (10) '수입의뢰No',
         (05) 'AmdNo'.
  SET LEFT SCROLL-BOUNDARY.
  WRITE: (10) '의뢰일자',
         (10) '접수일자',
         (10) '요개설일',
         (10) '예정일자',
         (10) '개설일자',
         (05) 'CURR',
         (14) '의뢰금액',
         (01) 'M',
         (02) 'Re',
         (01) 'G',
         (03) 'Inc',
         (01) 'T',
         (11) 'Beneficiary',
         (27) 'Vendor Name',
         (03) 'Cou'.

  WRITE:/39(03) 'SPT',
           (35) 'Description',
           (10) '개설은행',
           (24) 'Bank Name',
           (01) 'B',
           (03) 'Dat',
           (03) 'Rat',
           (24) '신용장/승인번호',
           (10) '유효일',
           (10) '선적일'.
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
  LOOP AT IT_TAB.
    IF W_SUBRC = 1.
       W_SUBRC = 2.    FORMAT COLOR 2 INTENSIFIED ON.
    ELSE.
       W_SUBRC = 1.    FORMAT COLOR 2 INTENSIFIED OFF.
    ENDIF.

    NEW-LINE.

    AT NEW EBELN.
       W_TABIX = W_TABIX + 1.
       WRITE: (04) W_TABIX RIGHT-JUSTIFIED,
              (04) IT_TAB-ZFWERKS,
              (10) IT_TAB-EBELN.
    ENDAT.

    AT NEW ZFREQNO.
       WRITE:22(10) IT_TAB-ZFREQNO.
    ENDAT.

    CLEAR : IT_LIFNR.
    READ TABLE IT_LIFNR WITH KEY LIFNR = IT_TAB-ZFBENI.

    WRITE:33(05) IT_TAB-ZFAMDNO,
            (10) IT_TAB-ZFRLDT1,
            (10) IT_TAB-ZFRVDT,
            (10) IT_TAB-ZFREQDT,
            (10) IT_TAB-ZFAPPDT,
            (10) IT_TAB-ZFOPNDT,
            (05) IT_TAB-WAERS,
            (14) IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS RIGHT-JUSTIFIED,
            (01) IT_TAB-ZFMATGB,
            (02) IT_TAB-ZFREQTY,
            (01) IT_TAB-ZFBACD,
            (03) IT_TAB-INCO1,
            (01) IT_TAB-ZFTRANS,
            (10) IT_TAB-ZFBENI,
            (28) IT_LIFNR-NAME1,
            (03) IT_LIFNR-LAND1.

    CLEAR : IT_LIFNR.
    READ TABLE IT_LIFNR WITH KEY LIFNR = IT_TAB-ZFOPBN.

    WRITE:/39(03) IT_TAB-ZFSHCU,
             (35) IT_TAB-MAKTX,
             (10) IT_TAB-ZFOPBN,
             (24) IT_LIFNR-NAME1,
             (01) IT_TAB-ZFJEWGB,
             (03) IT_TAB-ZFLEPD,
             (03) IT_TAB-ZFPREPAY,
             (24) IT_TAB-ZFOPNNO,
             (10) IT_TAB-ZFLASTED,
             (10) IT_TAB-ZFLASTSD.

    AT END OF EBELN.
       ULINE.
    ENDAT.

    AT LAST.
       FORMAT RESET.
       WRITE:/20 '*********** End of Page ***********'.
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
*  CONCATENATE SY-DATUM(6) '01' INTO S_RLDT1-LOW.
*  S_RLDT1-HIGH = SY-DATUM.
*  APPEND S_RLDT1.
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
  LOOP AT IT_TAB.

     WMTO_S-AMOUNT = IT_TAB-ZFOPAMT.
     CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
           EXPORTING
               CURRENCY  = IT_TAB-WAERS
               AMOUNT_INTERNAL = WMTO_S-AMOUNT
           IMPORTING
               AMOUNT_DISPLAY  = WMTO_S-AMOUNT
           EXCEPTIONS
               INTERNAL_ERROR = 1.
     IT_TAB-ZFOPAMT  =  WMTO_S-AMOUNT.
     MODIFY IT_TAB.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
        EXPORTING
        FILENAME = 'C:\TEMP\TEMP.TXT'
        FILETYPE = 'DAT'
   TABLES
       DATA_TAB = IT_TAB.

ENDFORM.                    " P1000_DOWNLOAD

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
  SET PF-STATUS 'PF1000'.
  SET TITLEBAR  'TI1000'.
  PERFORM P1000_TOP_PAGE.
  PERFORM P1000_WRITE_DATA.
ENDFORM.                    " RESET_LIST
