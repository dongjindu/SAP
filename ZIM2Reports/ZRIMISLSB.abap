*&---------------------------------------------------------------------*
*& Report  ZRIMISLSB                                                   *
*&---------------------------------------------------------------------*
*&ABAP Name : ZRIMISLSB                                                *
*&Created by: 정승연 INFOLINK.Ltd                                      *
*&Created on: 09/04/2002                                               *
*&Version   : 1.0                                                      *
*&---------------------------------------------------------------------*
* 적하보험 부보 명세.
* BL기준으로 보험부보한 건에 대한 부보 명세.
* ZTINSB : BL기준의 보험부보 테이블
*&---------------------------------------------------------------------*

REPORT  ZRIMISLSB      NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 142
                       LINE-COUNT 65.

TABLES :  WMTO_S.                       "

DATA : BEGIN OF IT_TAB OCCURS 0,
               ZFBLNO      LIKE  ZTINSB-ZFBLNO,      " B/L Document No.
               ZFINSEQ     LIKE  ZTINSB-ZFINSEQ,     " Insuarance Seq.
               INCO1       LIKE  ZTBL-INCO1,         " Incoterms.
               ZFINSDT     LIKE  ZTINSB-ZFINSDT,     " Issuing Date
               ZFEXRT(12)  TYPE  C,                  " Exchange Rate
               ZFTAMI      TYPE  P DECIMALS 0,       " Ins. Amount
               ZFIVAMT     LIKE  ZTINSB-ZFIVAMT,     " Invoice Amount.
               WAERS       LIKE  ZTINSB-WAERS,       " Currency.
               ZFKRWAMT    LIKE  ZTINSB-ZFKRWAMT,    " Local Insuarance.
               ZFKRW       LIKE  ZTINSB-ZFKRW,       " Local Currency.
               ZFINNO      LIKE  ZTINSB-ZFINNO,      " Policy No
               ZFHBLNO     LIKE  ZTBL-ZFHBLNO,       " HBL
               ZFTRANS     LIKE  ZTINSB-ZFTRANS,     " VIA
               ZFRSTAW     LIKE  ZTINSB-ZFRSTAW,     " HS.
               ZFINRT(9)   TYPE  C,                  " Rate
               ZFINAMTC    LIKE  ZTINSB-ZFINAMTC,    " CURR
               ZFINAMT     LIKE  ZTINSB-ZFINAMT,     " Premium
               ZFRGDSR     LIKE  ZTBL-ZFRGDSR.       " Goods Descriptio.
DATA : END   OF IT_TAB.

DATA : BEGIN OF IT_TMP1  OCCURS 0,
               ZFBLNO      LIKE  ZTINSB-ZFBLNO,      " B/L Document No.
               ZFINSEQ     LIKE  ZTINSB-ZFINSEQ,     " Insuarance Seq.
               INCO1       LIKE  ZTBL-INCO1,         " Incoterms.
               ZFINSDT     LIKE  ZTINSB-ZFINSDT,     " Issuing Date.
               ZFIVAMT     LIKE  ZTINSB-ZFIVAMT,     " Invoice Amount.
               WAERS       LIKE  ZTINSB-WAERS,       " Currency.
               ZFKRWAMT    LIKE  ZTINSB-ZFKRWAMT,    " Local Ins Amount.
               ZFKRW       LIKE  ZTINSB-ZFKRW,       " Local Currency
               ZFINNO      LIKE  ZTINSB-ZFINNO,      " Policy No.
               ZFHBLNO     LIKE  ZTBL-ZFHBLNO,       " HBL
               ZFTRANS     LIKE  ZTINSB-ZFTRANS,     " VIA
               ZFRSTAW     LIKE  ZTINSB-ZFRSTAW,     " HS.
               ZFINRT      LIKE  ZTINSB-ZFINRT,      " Insuarance Rate.
               ZFINAMTC    LIKE  ZTINSB-ZFINAMTC,    " CURR
               ZFINAMT     LIKE  ZTINSB-ZFINAMT,     " Insuarancy Amt.
               ZFRGDSR     LIKE  ZTBL-ZFRGDSR.       " Goods Desc.
DATA : END   OF IT_TMP1.

DATA : W_ZFEXRT    LIKE  ZTINSBRSP-ZFEXRT,
       W_ZFTAMI    TYPE  P DECIMALS 0,
       W_ZFKRWAMT  LIKE  ZTINSB-ZFKRWAMT,
       W_SUBRC     LIKE  SY-SUBRC.

INCLUDE   ZRIMISLSBTOP.
INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.

*---------------------------------------------------------------------*
* SELECTION-SCREEN
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS: S_BUKRS    FOR ZTBL-BUKRS NO-EXTENSION
                                          NO INTERVALS,
                S_WERKS    FOR ZTBL-ZFWERKS,   " Plant
                S_MATGB    FOR ZTBL-ZFMATGB,   " Material Type.
                S_WAERS    FOR ZTBL-ZFBLAMC,   " B/L 통화.
                S_DOCST    FOR ZTINSB-ZFDOCST
                              OBLIGATORY,
                S_INSDT    FOR ZTINSB-ZFINSDT  " Issuing date
                              OBLIGATORY,
                S_TRANS    FOR ZTINSB-ZFTRANS. " VIA.
SELECTION-SCREEN END OF BLOCK B1.
*---------------------------------------------------------------------*
* EVENT INITIALIZATION
*---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM P1000_INITIALIZATION.
  PERFORM P1000_SET_BUKRS.
  SET  TITLEBAR  'TI1000'.               " GUI TITLE  SETTING
*---------------------------------------------------------------------*
* EVENT START-OF-SELECTION.
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM P1000_READ_DATA.
  IF W_SUBRC = 4.
    MESSAGE S191 WITH 'Insuarance Policy List'.  EXIT.
  ENDIF.

  PERFORM P1000_CHECK_DATA.
  IF IT_TAB[] IS INITIAL.
    MESSAGE S191 WITH 'Insuarance Policy List'.  EXIT.
  ENDIF.

  SET PF-STATUS 'PF1000'.
  SET TITLEBAR  'TI1000'.
  PERFORM P1000_WRITE_DATA.
  CLEAR IT_TAB.

*---------------------------------------------------------------------*
* EVENT TOP-OF-PAGE.
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM P1000_TOP_PAGE.
*---------------------------------------------------------------------*
* EVENT AT USER-COMMAND.
*---------------------------------------------------------------------*
AT USER-COMMAND.

  IF IT_TAB-ZFBLNO IS INITIAL.
    MESSAGE S962.
  ELSE.

    CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택.
        W_FIELD_NM = 'ZFBLNO'.
        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
        PERFORM HANDLE_SORT TABLES  IT_TAB
                            USING   SY-UCOMM.
      WHEN 'DISP'.                   " 부보조회.
        PERFORM P2000_SHOW_INS USING  IT_TAB-ZFBLNO
                                      IT_TAB-ZFINSEQ.
      WHEN 'DSBL'.                  " B/L 조회.
        PERFORM P2000_SHOW_BL USING  IT_TAB-ZFBLNO.

      WHEN 'DOWN'.          " FILE DOWNLOAD....
        PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
*  테이블 SELECT
        PERFORM P1000_READ_DATA.
        PERFORM P1000_CHECK_DATA.
        PERFORM RESET_LIST.
      WHEN OTHERS.
    ENDCASE.
    CLEAR IT_TAB.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_DATA.

  REFRESH : IT_TMP1.
  CLEAR : W_SUBRC.

  CLEAR IT_TMP1.
  REFRESH IT_TMP1.
  SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_TMP1
       FROM ZTINSB AS I INNER JOIN ZTBL AS B
         ON I~ZFBLNO = B~ZFBLNO
      WHERE B~BUKRS    IN   S_BUKRS
        AND B~ZFWERKS  IN   S_WERKS
        AND B~ZFMATGB  IN   S_MATGB
        AND B~ZFBLAMC  IN   S_WAERS
        AND I~ZFDOCST  IN   S_DOCST
        AND I~ZFINSDT  IN   S_INSDT
        AND I~ZFTRANS  IN   S_TRANS.

  IF SY-SUBRC <> 0.   W_SUBRC = 4.  EXIT.   ENDIF.

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

  CLEAR IT_TAB.
  REFRESH IT_TAB.
  LOOP AT IT_TMP1.
    CLEAR : W_ZFEXRT, W_ZFTAMI, IT_TAB.

    MOVE-CORRESPONDING IT_TMP1  TO  IT_TAB.

   SELECT SINGLE ZFEXRT ZFTAMI INTO (W_ZFEXRT, W_ZFTAMI) FROM ZTINSBRSP
       WHERE ZFBLNO = IT_TMP1-ZFBLNO.
    IF SY-SUBRC = 0 AND W_ZFEXRT <> 0.
      IT_TAB-ZFEXRT = W_ZFEXRT.
      IT_TAB-ZFTAMI = W_ZFTAMI * W_ZFEXRT.
    ELSE.
      IT_TAB-ZFEXRT = W_ZFEXRT.
      IT_TAB-ZFTAMI = 0.
    ENDIF.

    APPEND IT_TAB.   CLEAR IT_TAB.
  ENDLOOP.
ENDFORM.                    " P1000_CHECK_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_TOP_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_TOP_PAGE.
  SKIP 1.
  WRITE:/50 '   Insurance Policy List   ' COLOR 1.
  SKIP 2.
  WRITE:/ 'DATE :', SY-DATUM,
          75 'INC:Incoterms   VIA:Transpotation method'.

  ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : SY-VLINE NO-GAP,
         (04) 'Seq.'              CENTERED NO-GAP, SY-VLINE NO-GAP,
         (24) 'B/L Manage No'     CENTERED NO-GAP, SY-VLINE NO-GAP,
         (03) 'INC'               CENTERED NO-GAP, SY-VLINE NO-GAP,
         (10) 'Insuring Date'     CENTERED NO-GAP, SY-VLINE NO-GAP,
         (12) 'Exchange Rate'     CENTERED NO-GAP, SY-VLINE NO-GAP,
         (20) 'Invoice amount'    CENTERED NO-GAP, SY-VLINE NO-GAP,
         (20) 'Premium(Local)'    CENTERED NO-GAP, SY-VLINE NO-GAP,
         (40) 'Policy No'         CENTERED NO-GAP, SY-VLINE NO-GAP.
  NEW-LINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : SY-VLINE NO-GAP,
         (04) ' '              CENTERED NO-GAP, SY-VLINE NO-GAP,
         (24) 'House B/L No'   CENTERED NO-GAP, SY-VLINE NO-GAP,
         (03) 'VIA'            CENTERED NO-GAP, SY-VLINE NO-GAP,
         (10) 'Rep. H/S'       CENTERED NO-GAP, SY-VLINE NO-GAP,
         (12) 'Ins. Rate'      CENTERED NO-GAP, SY-VLINE NO-GAP,
         (20) ' '              CENTERED NO-GAP, SY-VLINE NO-GAP,
         (20) 'Premium'        CENTERED NO-GAP, SY-VLINE NO-GAP,
         (40) 'Item name'      CENTERED NO-GAP, SY-VLINE NO-GAP.
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
  CLEAR : W_SUBRC, W_TABIX, W_ZFTAMI, W_ZFKRWAMT.
  SORT IT_TAB BY ZFBLNO.
  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.

    FORMAT RESET.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    NEW-LINE.
    WRITE : SY-VLINE NO-GAP,
           (04) W_TABIX RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
           (10) IT_TAB-ZFBLNO NO-GAP, '-'  NO-GAP,
           (13) IT_TAB-ZFINSEQ          NO-GAP, SY-VLINE NO-GAP,
           (03) IT_TAB-INCO1            NO-GAP, SY-VLINE NO-GAP,
           (10) IT_TAB-ZFINSDT          NO-GAP, SY-VLINE NO-GAP,
           (12) IT_TAB-ZFEXRT           NO-GAP, SY-VLINE NO-GAP,
           (05) IT_TAB-WAERS            NO-GAP,
           (15) IT_TAB-ZFIVAMT CURRENCY IT_TAB-WAERS
                        RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
           (20) IT_TAB-ZFKRWAMT CURRENCY IT_TAB-ZFKRW
                        RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
           (40) IT_TAB-ZFINNO           NO-GAP, SY-VLINE NO-GAP.
    HIDE: IT_TAB.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    NEW-LINE.
    WRITE : SY-VLINE NO-GAP,
           (04) ' '                      NO-GAP, SY-VLINE NO-GAP,
           (24) IT_TAB-ZFHBLNO           NO-GAP, SY-VLINE NO-GAP,
           (03) IT_TAB-ZFTRANS  CENTERED NO-GAP, SY-VLINE NO-GAP,
           (10) IT_TAB-ZFRSTAW           NO-GAP, SY-VLINE NO-GAP,
           (12) IT_TAB-ZFINRT            NO-GAP, SY-VLINE NO-GAP,
           (20) ' '                      NO-GAP, SY-VLINE NO-GAP,
           (05) IT_TAB-ZFINAMTC          NO-GAP,
           (15) IT_TAB-ZFINAMT CURRENCY IT_TAB-ZFINAMTC
                         RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
           (40) IT_TAB-ZFRGDSR           NO-GAP, SY-VLINE NO-GAP.
    HIDE: IT_TAB.
    ULINE.
    AT LAST.
      SUM.
      FORMAT COLOR 3 INTENSIFIED OFF.
      WRITE:/ SY-VLINE NO-GAP,
             (20) '   Total sum',
           81(01)'('               NO-GAP,
             (05) 'USD'            NO-GAP,
             (01) ')'              NO-GAP,
             (15) IT_TAB-ZFKRWAMT  CURRENCY IT_TAB-ZFKRW
                                 RIGHT-JUSTIFIED NO-GAP,
           142 SY-VLINE NO-GAP.
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
  CONCATENATE SY-DATUM(6) '01' INTO S_INSDT-LOW.
  S_INSDT-HIGH = SY-DATUM.
  APPEND S_INSDT.

  S_DOCST-SIGN   = 'I'.
  S_DOCST-OPTION = 'EQ'.
  S_DOCST-LOW    = 'O'.
  S_DOCST-HIGH   = SPACE.
  APPEND S_DOCST.

*  AMEND 없음.
*  S_DOCST-SIGN   = 'I'.
*  S_DOCST-OPTION = 'EQ'.
*  S_DOCST-LOW    = 'A'.
*  S_DOCST-HIGH   = SPACE.
*  APPEND S_DOCST.

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
*    WMTO_S-AMOUNT = IT_TAB-ZFOPAMT.
*    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
*         EXPORTING
*              CURRENCY        = IT_TAB-WAERS
*              AMOUNT_INTERNAL = WMTO_S-AMOUNT
*         IMPORTING
*              AMOUNT_DISPLAY  = WMTO_S-AMOUNT
*         EXCEPTIONS
*              INTERNAL_ERROR  = 1.
*    IT_TAB-ZFOPAMT = WMTO_S-AMOUNT.

    WMTO_S-AMOUNT = IT_TAB-ZFKRWAMT.
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
         EXPORTING
              CURRENCY        = 'KRW'
              AMOUNT_INTERNAL = WMTO_S-AMOUNT
         IMPORTING
              AMOUNT_DISPLAY  = WMTO_S-AMOUNT
         EXCEPTIONS
              INTERNAL_ERROR  = 1.
    IT_TAB-ZFKRWAMT = WMTO_S-AMOUNT.

    WMTO_S-AMOUNT = IT_TAB-ZFINAMT.
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
         EXPORTING
              CURRENCY        = IT_TAB-ZFINAMTC
              AMOUNT_INTERNAL = WMTO_S-AMOUNT
         IMPORTING
              AMOUNT_DISPLAY  = WMTO_S-AMOUNT
         EXCEPTIONS
              INTERNAL_ERROR  = 1.
    IT_TAB-ZFINAMT = WMTO_S-AMOUNT.

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
*&      Form  P2000_SHOW_INS
*&---------------------------------------------------------------------*
FORM P2000_SHOW_INS USING    P_ZFBLNO
                             P_ZFINSEQ.

  SET PARAMETER ID 'ZPOPNNO'   FIELD ' '.
  SET PARAMETER ID 'BES'       FIELD ' '.
  SET PARAMETER ID 'ZPBLNO'    FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPINSEQ'   FIELD P_ZFINSEQ.

  EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
  EXPORT 'BES'           TO MEMORY ID 'BES'.
  EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
  EXPORT 'ZPINSEQ'       TO MEMORY ID 'ZPINSEQ'.

  CALL TRANSACTION 'ZIMB3' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_INS
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFBLNO.

  SET PARAMETER ID 'ZPHBLNO'   FIELD ' '.
  SET PARAMETER ID 'ZPBLNO'    FIELD P_ZFBLNO.

  EXPORT 'ZPHBLNO'        TO MEMORY ID 'ZPHBLNO'.
  EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_BL
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P1000_TOP_PAGE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P1000_WRITE_DATA.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   TABLES : ZTIMIMG00.

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
