*&---------------------------------------------------------------------*
*& Report  ZRIMBTAS                                                    *
*&---------------------------------------------------------------------*
*&  프로그램명 : 보세창고 반입현황 조회                                *
*&      작성자 : 나 신 호                                              *
*&      작성일 : 2001.11.06                                            *
*&    적용회사 : L/G Chem                                              *
*&---------------------------------------------------------------------*
*&   DESC.     :                                                       *
*&---------------------------------------------------------------------*
*& [변경내용]  :                                                       *
*&---------------------------------------------------------------------*
REPORT  ZRIMBTAS       NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 133
                       LINE-COUNT 90.

TABLES : ZTBL,                       " Bill of Lading Header
         ZTBLINR_TMP,                " B/L 반입신고 TEMP
         ZTIMIMG03,                  " 보세구역 코드.
         ZTIMIMG00,                  " IMG
         DD07T,
         ZSBLINR_TMP.
DATA : BEGIN OF IT_TAB OCCURS 0.
       INCLUDE STRUCTURE ZSBLINR_TMP.
DATA : END OF IT_TAB.

DATA : BEGIN OF IT_TAB_WH OCCURS 0.
       INCLUDE STRUCTURE ZSBLINR_TMP.
DATA : END OF IT_TAB_WH.

DATA: BEGIN OF IT_FIELD_NAMES OCCURS 0,
      FIELD_NAME(50).
DATA: END OF IT_FIELD_NAMES.

DATA: W_RGDSR LIKE ZTBL-ZFRGDSR.

DATA : W_SUBRC         LIKE  SY-SUBRC,
       W_TABIX         LIKE  SY-TABIX,
       W_DOM_TEX1(10)  TYPE C,
       W_TMP_TEXT(18)  TYPE C,
       W_CNT(4),
       L_FNAME(30),
       P_BUKRS         LIKE  ZTBL-BUKRS.

DATA: W_VERT(1) TYPE C VALUE '|',
      W_FIELD_NM        LIKE DD03D-FIELDNAME.   " 필드?

INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMOLECOM.     " OLE 공통모듈.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.

SELECT-OPTIONS: S_BUKRS     FOR  ZTBL-BUKRS    NO-EXTENSION
                                               NO INTERVALS,
                S_PO        FOR  ZTBL-ZFREBELN NO-EXTENSION,
                S_INDT      FOR  ZTBLINR_TMP-ZFINDT
                                               NO-EXTENSION OBLIGATORY.

PARAMETERS    : P_ABNAR     LIKE ZTBLINR_TMP-ZFABNAR  OBLIGATORY.

SELECT-OPTIONS: S_LOC       FOR  ZTBLINR_TMP-ZFLOC NO-EXTENSION,
                S_INRNO     FOR  ZTBLINR_TMP-ZFINRNO NO-EXTENSION,
                S_HBL       FOR  ZTBL-ZFHBLNO NO-EXTENSION,
                S_ETA       FOR  ZTBLINR_TMP-ZFETA NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ABNAR.
  PERFORM   P1000_CODE_HELP(ZRIMBWINVLST)
                           USING  P_ABNAR  'P_ABNAR'.

INITIALIZATION.
  PERFORM P1000_SET_BUKRS.
  PERFORM P1000_INITIALIZATION.

START-OF-SELECTION.
  SET TITLEBAR 'TI1000'.
  SET PF-STATUS 'PF1000'.

  PERFORM P1000_READ_DATA.
  PERFORM P1000_WRITE_DATA.

END-OF-SELECTION.

TOP-OF-PAGE.
  PERFORM P1000_TOP_PAGE.

AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'DISP_BL'.
      PERFORM P2000_DISP_BL.

    WHEN 'DISP_IN'.
      PERFORM P2000_DISP_IN.

    WHEN 'DISP_WH'.
*            SELECT SINGLE FUNCNAME INTO L_FNAME
*                                  FROM TFTIT
*                                 WHERE SPRAS = '3'
*                                   AND STEXT LIKE '%ZIM_BLIN_WHACHAL'.
*
*           PERFORM P2000_DISP_WH USING L_FNAME.

    WHEN 'DISP_BN'.
*            SELECT SINGLE FUNCNAME INTO L_FNAME
*                                  FROM TFTIT
*                                 WHERE SPRAS = '3'
*                                   AND STEXT LIKE '%ZIM_BLIN_BANIB'.
*
*           PERFORM P2000_DISP_BN USING L_FNAME.

    WHEN 'STUP' OR 'STDN'.
*            W_FIELD_NM = 'ZFETA'.
      GET CURSOR FIELD W_FIELD_NM.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.

      PERFORM HANDLE_SORT TABLES  IT_TAB
                           USING   SY-UCOMM.
      CLEAR : IT_TAB.
    WHEN 'EXCEL'.
      PERFORM P3000_EXCEL_DOWNLOAD.

    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------
FORM P1000_READ_DATA.
  REFRESH IT_TAB.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_TAB
    FROM ZTBLINR_TMP
   WHERE ZFREBELN IN S_PO
     AND ZFINDT IN S_INDT
     AND ZFABNAR = P_ABNAR
     AND ZFLOC IN S_LOC
     AND ZFINRNO IN S_INRNO
     AND ZFHBLNO IN S_HBL
     AND ZFETA IN S_ETA.

  IF SY-SUBRC <> 0.
    MESSAGE I738.
    EXIT.
  ENDIF.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    CLEAR: W_RGDSR,
           W_DOM_TEX1.

    IF IT_TAB-ZFMCYN = 'X'.
      SELECT SINGLE ZFRGDSR
        INTO W_RGDSR
        FROM ZTBL
       WHERE ZFBLNO =  IT_TAB-ZFBLNO
       AND   BUKRS  IN S_BUKRS.

      IF SY-SUBRC = 0.
        IT_TAB-ZFRGDSR = W_RGDSR.
      ENDIF.
    ENDIF.

    IF NOT IT_TAB-ZFFORD IS INITIAL.
      SELECT SINGLE NAME1
        INTO IT_TAB-NAME1
        FROM LFA1
       WHERE LIFNR EQ IT_TAB-ZFFORD.
    ENDIF.

    SELECT SINGLE ZFWERKS
      INTO IT_TAB-ZFWERKS
      FROM ZTBL
     WHERE ZFBLNO EQ IT_TAB-ZFBLNO.

    IF SY-SUBRC NE 0.
      MOVE SPACE TO IT_TAB-ZFWERKS.
    ENDIF.

    MODIFY IT_TAB INDEX W_TABIX.
    MOVE IT_TAB TO IT_TAB_WH.

    WRITE IT_TAB-ZFPKCN NO-ZERO TO IT_TAB_WH-ZFPKCN.

    APPEND IT_TAB_WH.
  ENDLOOP.

ENDFORM.                    " P1000_READ_DATA
*&---------------------------------------------------------------------
*&      Form  P1000_TOP_PAGE
*&---------------------------------------------------------------------
FORM P1000_TOP_PAGE.
  SKIP 1.
  WRITE:/60 '[ Carry-in status ]' COLOR 1.
  SKIP 1.
  WRITE:/ 'Carry-in date :', (09) S_INDT-LOW.

  IF NOT S_INDT-HIGH IS INITIAL.
    WRITE: (2) '~', (09) S_INDT-HIGH.
  ENDIF.

  WRITE: (70) 'Bonded warehouse :' RIGHT-JUSTIFIED, (10) P_ABNAR,
         120(5) 'PAGE:' NO-GAP, SY-PAGNO.

  FORMAT RESET.
  ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ W_VERT NO-GAP,
         (13) 'P/O Doc'     CENTERED, W_VERT,
          (8) 'Arrival in port'       CENTERED, W_VERT,
         (18) 'House B/L No' CENTERED, W_VERT,
         (20) 'Rep goods name'     CENTERED, W_VERT,
         (15) 'Weight'         CENTERED, W_VERT,
         (15) 'Shipping company'         CENTERED, W_VERT,
         (08) 'Carry-in No'     CENTERED, W_VERT,
         (13) 'Forwarder'       CENTERED NO-GAP, W_VERT NO-GAP.
  WRITE:/ W_VERT NO-GAP,
         (13) 'Plant' CENTERED, W_VERT,
         (29) 'Ship name' CENTERED,     W_VERT,
         (20) 'Packing No' CENTERED, W_VERT,
         (15) 'Measurement' CENTERED, W_VERT,
         (15) 'loading/unloading company' CENTERED,   W_VERT,
         (08) 'Storage location ' CENTERED, W_VERT,
         (13) 'Shipment type' CENTERED NO-GAP, W_VERT NO-GAP.
  ULINE.

ENDFORM.                    " P1000_TOP_PAGE
*&---------------------------------------------------------------------
*&      Form  P1000_WRITE_DATA
*&---------------------------------------------------------------------
FORM P1000_WRITE_DATA.
  DATA: W_EBELN(13).
  CLEAR : W_SUBRC.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    CLEAR: W_DOM_TEX1, W_TMP_TEXT, W_EBELN.

    PERFORM  GET_DD07T_SELECT USING 'ZDSHTY' IT_TAB-ZFSHTY
                              CHANGING   W_DOM_TEX1.

    CONCATENATE IT_TAB-ZFGMNO IT_TAB-ZFMSN IT_TAB-ZFHSN
                INTO W_TMP_TEXT.
    CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO
                INTO W_EBELN.

    IF W_SUBRC EQ 1.
      W_SUBRC = 2.    FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      W_SUBRC = 1.    FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.

    W_CNT = W_CNT + 1.

    WRITE:/ W_VERT NO-GAP,
           (13) W_EBELN         CENTERED,       W_VERT,
            (8) IT_TAB-ZFETA    CENTERED,       W_VERT,
           (18) IT_TAB-ZFHBLNO ,                W_VERT,
           (20) IT_TAB-ZFRGDSR,                 W_VERT,
           (12) IT_TAB-ZFTOWT  UNIT IT_TAB-ZFTOWTM
                               RIGHT-JUSTIFIED   NO-GAP,
            (3) IT_TAB-ZFTOWTM CENTERED,        W_VERT,
           (15) IT_TAB-NAME1,                   W_VERT NO-GAP,
           (10) IT_TAB-ZFINRNO NO-GAP,          W_VERT,
           (13) IT_TAB-ZFGSNM NO-GAP, W_VERT    NO-GAP.
    HIDE: IT_TAB.

    WRITE:/ W_VERT NO-GAP,
          (13) IT_TAB-ZFWERKS CENTERED,        W_VERT,
           (29) IT_TAB-ZFCARNM ,                W_VERT,
          (18) IT_TAB-ZFPKCN NO-ZERO RIGHT-JUSTIFIED NO-GAP,
           (2) IT_TAB-ZFPKCNM,                 W_VERT,
          (12) IT_TAB-ZFTOVL UNIT IT_TAB-ZFTOVLM
                             RIGHT-JUSTIFIED   NO-GAP,
           (3) IT_TAB-ZFTOVLM CENTERED,        W_VERT,
          (15) IT_TAB-ZFTRCK,                  W_VERT,
          (08) IT_TAB-ZFLOC ,                  W_VERT,
          (13) W_DOM_TEX1    NO-GAP, W_VERT    NO-GAP.
    HIDE: IT_TAB.
    ULINE.

    IF W_CNT >= 26.
      NEW-PAGE.
      W_CNT = 0.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " P1000_WRITE_DATA
*&---------------------------------------------------------------------
*&      Form  P1000_INITIALIZATION
*&---------------------------------------------------------------------
FORM P1000_INITIALIZATION.
  S_INDT-LOW = SY-DATUM.
  APPEND S_INDT.
ENDFORM.                    " P1000_INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_BL
*&---------------------------------------------------------------------*
FORM P2000_DISP_BL.
  IF IT_TAB-ZFBLNO IS INITIAL.
    MESSAGE S951.
    STOP.
  ENDIF.

  SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
  SET PARAMETER ID 'ZPBLNO'    FIELD IT_TAB-ZFBLNO.

  SELECT SINGLE * FROM ZTIMIMG00.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_BL
*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
FORM GET_DD07T_SELECT USING    P_DOMNAME
                               P_FIELD
                      CHANGING P_W_NAME.
  CLEAR : DD07T, P_W_NAME.

  IF P_FIELD IS INITIAL.   EXIT.   ENDIF.

  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ P_FIELD
                      ORDER BY AS4VERS DESCENDING.
    EXIT.
  ENDSELECT.

  P_W_NAME   = DD07T-DDTEXT.
  TRANSLATE P_W_NAME TO UPPER CASE.
ENDFORM.                    " GET_DD07T_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_IN
*&---------------------------------------------------------------------*
FORM P2000_DISP_IN.
  IF IT_TAB-ZFHBLNO IS INITIAL.
    MESSAGE S951.
    STOP.
  ENDIF.

  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPHBLNO'   FIELD IT_TAB-ZFHBLNO.
  SET PARAMETER ID 'ZPBLNO'    FIELD ''.
  SET PARAMETER ID 'ZPBTSEQ'   FIELD ''.

  EXPORT 'BES'           TO MEMORY ID 'BES'.
  EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
  EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.
  EXPORT 'ZPBTSEQ'       TO MEMORY ID 'ZPBTSEQ'.

  CALL TRANSACTION 'ZIMI8' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_DISP_IN
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_WH
*&---------------------------------------------------------------------*
FORM P2000_DISP_WH USING L_FNAME.

ENDFORM.                    " P2000_DISP_WH
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_BN
*&---------------------------------------------------------------------*
FORM P2000_DISP_BN USING L_FNAME.

ENDFORM.                    " P2000_DISP_BN
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.
  MOVE 0 TO SY-LSIND.
  SET PF-STATUS 'PF1000'.
  SET TITLEBAR  'TI1000'.
  PERFORM P1000_TOP_PAGE.
  PERFORM P1000_WRITE_DATA.
ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P3000_EXCEL_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P3000_EXCEL_DOWNLOAD.

  DATA : L_COL        TYPE I,
         L_ROW        TYPE I,
         L_WRBTR(20)  TYPE C,
         L_EBELN(12)  TYPE C.

  PERFORM P2000_EXCEL_INITIAL  USING  '굴림체'
                                       10.

  PERFORM P2000_FIT_CELL    USING 1 6 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 1 '플랜트'.
  PERFORM P2000_FIT_CELL    USING 2 12 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 2 'P/O No.'.
  PERFORM P2000_FIT_CELL    USING 3 10 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 3 '입항일'.
  PERFORM P2000_FIT_CELL    USING 4 20 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 4 'B/L No.'.
  PERFORM P2000_FIT_CELL    USING 5 15 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 5 '선 명'.
  PERFORM P2000_FIT_CELL    USING 6 25 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 6 '대표품명'.
  PERFORM P2000_FIT_CELL    USING 7 8 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 7 '포장갯수'.
  PERFORM P2000_FIT_CELL    USING 8 8 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 8 '포장단위'.
  PERFORM P2000_FIT_CELL    USING 9 13 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 9 '중량'.
  PERFORM P2000_FIT_CELL    USING 10 8 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 10 '중량단위'.
  PERFORM P2000_FIT_CELL    USING 11 13 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 11 '용적'.
  PERFORM P2000_FIT_CELL    USING 12 8 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 12 '용적단위'.
  PERFORM P2000_FIT_CELL    USING 13 20 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 13 '선사'.
  PERFORM P2000_FIT_CELL    USING 14 20 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 14 '하역사'.
  PERFORM P2000_FIT_CELL    USING 15 10 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 15 '반입번호'.
  PERFORM P2000_FIT_CELL    USING 16 8 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 16 '저장위치'.
  PERFORM P2000_FIT_CELL    USING 17 20 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 17 '운송사'.
  PERFORM P2000_FIT_CELL    USING 18 8 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 18 '선적유형'.

  LOOP AT IT_TAB.
    L_ROW = SY-TABIX + 1.
    CLEAR: W_DOM_TEX1, L_EBELN.

    PERFORM  GET_DD07T_SELECT USING 'ZDSHTY' IT_TAB-ZFSHTY
                               CHANGING   W_DOM_TEX1.
    CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO
                 INTO L_EBELN.

    PERFORM P2000_FILL_CELL   USING L_ROW 1 IT_TAB-ZFWERKS.
    PERFORM P2000_FILL_CELL   USING L_ROW 2 L_EBELN.
    PERFORM P2000_FILL_CELL   USING L_ROW 3 IT_TAB-ZFETA.
    PERFORM P2000_FILL_CELL   USING L_ROW 4 IT_TAB-ZFHBLNO.
    PERFORM P2000_FILL_CELL   USING L_ROW 5 IT_TAB-ZFCARNM.
    PERFORM P2000_FILL_CELL   USING L_ROW 6 IT_TAB-ZFRGDSR.
    PERFORM P2000_FILL_CELL   USING L_ROW 7 IT_TAB-ZFPKCN.
    PERFORM P2000_FILL_CELL   USING L_ROW 8 IT_TAB-ZFPKCNM.
    PERFORM P2000_FILL_CELL   USING L_ROW 9 IT_TAB-ZFTOWT.
    PERFORM P2000_FILL_CELL   USING L_ROW 10 IT_TAB-ZFTOWTM.
    PERFORM P2000_FILL_CELL   USING L_ROW 11 IT_TAB-ZFTOVL.
    PERFORM P2000_FILL_CELL   USING L_ROW 12 IT_TAB-ZFTOVLM.
    PERFORM P2000_FILL_CELL   USING L_ROW 13 IT_TAB-NAME1.
    PERFORM P2000_FILL_CELL   USING L_ROW 14 IT_TAB-ZFTRCK.
    PERFORM P2000_FILL_CELL   USING L_ROW 15 IT_TAB-ZFSEQ.
    PERFORM P2000_FILL_CELL   USING L_ROW 16 IT_TAB-ZFLOC.
    PERFORM P2000_FILL_CELL   USING L_ROW 17 IT_TAB-ZFGSNM.
    PERFORM P2000_FILL_CELL   USING L_ROW 18 W_DOM_TEX1.
  ENDLOOP.

  SET PROPERTY OF EXCEL 'VISIBLE' = 1.

ENDFORM.                    " P3000_EXCEL_DOWNLOAD
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
