*&---------------------------------------------------------------------*
*& Report  ZRIMSDRS                                                    *
*&---------------------------------------------------------------------*
*&ABAP Name : ZRIMSDRS                                                 *
*&Created by: 나신호 INFOLINK.Ltd                                      *
*&Created on: 07/31/2000                                               *
*&Version   : 1.0                                                      *
*&---------------------------------------------------------------------*
* 선적서류 입수 내역
* Charge 부분은 보류...
*&---------------------------------------------------------------------*
REPORT  ZRIMSDRS       NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 140.
*                       LINE-COUNT 65.

TABLES : ZTBL,                       " Bill of Lading Header
         ZTBLCST,                    " B/L 비?
         ZTIMIMG08,                  " 관리코드 관리(선적항, 도착항)
         WMTO_S.

DATA : BEGIN OF IT_TAB OCCURS 0,
               ZFWERKS     LIKE  ZTBL-ZFWERKS,
               ZFBLNO      LIKE  ZTBL-ZFBLNO,
               ZFETD       LIKE  ZTBL-ZFETD,
               LIFNR       LIKE  ZTBL-LIFNR,
               ZFSPRTC     LIKE  ZTBL-ZFSPRTC,
               SPRTCNM(14) TYPE  C,
               ZFTOVL      LIKE  ZTBL-ZFTOVL,
               ZFTOVLM     LIKE  ZTBL-ZFTOVLM,
               BASIC       LIKE  ZTBL-ZFBLAMT,
               BASICC      LIKE  ZTBL-ZFBLAMC,
               ZFBLAMT     LIKE  ZTBL-ZFBLAMT,
               ZFBLAMC     LIKE  ZTBL-ZFBLAMC,
               ZFCARNM     LIKE  ZTBL-ZFCARNM,
               ZFETA       LIKE  ZTBL-ZFETA,
               ZFFORD      LIKE  ZTBL-ZFFORD,
               ZFAPRTC     LIKE  ZTBL-ZFAPRTC,
               APRTCNM(14) TYPE  C,
               ZFPKCN      LIKE  ZTBL-ZFPKCN,
               OTHER       LIKE  ZTBL-ZFBLAMT,
               OTHERC      LIKE  ZTBL-ZFBLAMC,
               ZFTRTE      LIKE  ZTBL-ZFTRTE,
               ZFTRTEC     LIKE  ZTBL-ZFTRTEC,
               ZFMBLNO     LIKE  ZTBL-ZFMBLNO,
               ZFBNDT      LIKE  ZTBL-ZFBNDT,
               ZFTRCK      LIKE  ZTBL-ZFTRCK,
               ZFMATGB     LIKE  ZTBL-ZFMATGB,
               ZFVIA       LIKE  ZTBL-ZFVIA,
               INCO1       LIKE  ZTBL-INCO1,
               ZFTRTPM     LIKE  ZTBL-ZFTRTPM,
               ZFOTHPM     LIKE  ZTBL-ZFOTHPM,
               ZFPOYN      LIKE  ZTBL-ZFPOYN,
               ZFTOWT      LIKE  ZTBL-ZFTOWT,
               ZFTOWTM     LIKE  ZTBL-ZFTOWTM,
               TOTAL       LIKE  ZTBL-ZFBLAMT,
               TOTALC      LIKE  ZTBL-ZFBLAMC,
               ZFSHTY      LIKE  ZTBL-ZFSHTY,
               SHTYNM(4)   TYPE  C,
               ZFRGDSR     LIKE  ZTBL-ZFRGDSR.
DATA : END   OF IT_TAB.

DATA : BEGIN OF IT_BL OCCURS 0.
              INCLUDE STRUCTURE  ZTBL.
DATA : END   OF IT_BL.

DATA : BEGIN OF IT_CD OCCURS 0.
       INCLUDE STRUCTURE ZTIEPORT.
*               ZFCD     LIKE ZTIMIMG08-ZFCD,
*               ZFCDNM   LIKE ZTIMIMG08-ZFCDNM.
DATA : END   OF IT_CD.

DATA : BEGIN OF IT_CST OCCURS 0,
               ZFBLNO      LIKE  ZTBLCST-ZFBLNO,
               ZFCSCD      LIKE  ZTBLCST-ZFCSCD,
               ZFCAMT      LIKE  ZTBLCST-ZFCAMT,
               WAERS       LIKE  ZTBLCST-WAERS.
DATA : END   OF IT_CST.

DATA : W_SUBRC   LIKE  SY-SUBRC,
       W_TABIX(4),
       W_FIELD_NM        LIKE DD03D-FIELDNAME.   " 필드?

INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
    SELECT-OPTIONS: S_BUKRS     FOR ZTBL-BUKRS NO-EXTENSION
                                               NO INTERVALS,
                    S_EBELN     FOR ZTBL-ZFREBELN,      " 대표 구매문서.
                    S_WERKS     FOR ZTBL-ZFWERKS,       " 플랜?
                    S_FORD      FOR ZTBL-ZFFORD,        " Forwarder
                    S_LIFNR     FOR ZTBL-LIFNR,         " Vendor
                    S_TRCK      FOR ZTBL-ZFTRCK,        " Trucker
                    S_MATGB     FOR ZTBL-ZFMATGB,       " 자재구?
                    S_VIA       FOR ZTBL-ZFVIA,         " VIA
                    S_SPRTC     FOR ZTBL-ZFSPRTC,       " 선적?
                    S_SHTY      FOR ZTBL-ZFSHTY,        " 선적유?
                    S_ETD       FOR ZTBL-ZFETD,         " 출항?
                    S_ETA       FOR ZTBL-ZFETA,         " 도착?
                    S_CARNM     FOR ZTBL-ZFCARNM.       " 선기?
SELECTION-SCREEN END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM P1000_READ_DATA.
  IF W_SUBRC = 4.
     MESSAGE S191 WITH 'B/L'.  EXIT.
  ENDIF.

  PERFORM P1000_CHECK_DATA.

  SET PF-STATUS 'PF1000'.
  SET TITLEBAR  'TI1000'.
  PERFORM P1000_WRITE_DATA.
END-OF-SELECTION.

INITIALIZATION.
  SET  TITLEBAR  'TI1000'.               " GUI TITLE  SETTING

TOP-OF-PAGE.
  PERFORM P1000_TOP_PAGE.

AT USER-COMMAND.
  CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
         W_FIELD_NM = 'ZFBLHNO'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
    WHEN 'DOWN'.     PERFORM P1000_DOWNLOAD.
    WHEN 'DISP'.          " B/L 조회.
         IF NOT IT_TAB-ZFBLNO IS INITIAL.
            PERFORM P2000_SHOW_LC USING IT_TAB-ZFBLNO.
         ELSE.
            MESSAGE E962.
         ENDIF.
         CLEAR IT_TAB.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.
   SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
   SET PARAMETER ID 'ZPBLNO'    FIELD IT_TAB-ZFBLNO.
   EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
   EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.

      CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

* 구매의뢰 테이블 SELECT
   PERFORM   P1000_READ_DATA."        USING   W_SUBRC.
   IF W_SUBRC EQ 4.    EXIT.    ENDIF.
   PERFORM RESET_LIST.
ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

*  W_PAGE = 1.
   SY-PAGNO = 1.
*  W_LINE = 1.
*  W_COUNT = 0.
  PERFORM   P1000_TOP_PAGE.
  PERFORM   P1000_WRITE_DATA.                  " 해더 출력...
ENDFORM.                    " RESET_LIST

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA.
** B/L
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BL FROM ZTBL
   WHERE BUKRS   IN  S_BUKRS
         AND ZFWERKS IN  S_WERKS   AND  ZFFORD  IN  S_FORD   AND
         LIFNR   IN  S_LIFNR   AND  ZFTRCK   IN  S_TRCK   AND
         ZFMATGB IN  S_MATGB   AND  ZFVIA    IN  S_VIA    AND
         ZFSPRTC IN  S_SPRTC   AND  ZFSHTY   IN  S_SHTY   AND
         ZFETD   IN  S_ETD     AND  ZFETA    IN  S_ETA    AND
         ZFCARNM IN  S_CARNM   AND  ZFREBELN IN S_EBELN.

  IF SY-SUBRC <> 0.   W_SUBRC = 4.  EXIT.   ENDIF.

** CODE
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_CD
            FROM ZTIEPORT
            FOR ALL ENTRIES IN IT_BL
            WHERE ( LAND1 EQ IT_BL-ZFAPPC
            AND     PORT  EQ IT_BL-ZFAPRTC )
            OR    ( LAND1 EQ IT_BL-ZFCARC
            AND     PORT  EQ IT_BL-ZFSPRTC ).
*          ( ZFCD = IT_BL-ZFSPRTC OR ZFCD = IT_BL-ZFAPRTC ).

** B/L 비?
   SELECT ZFBLNO ZFCSCD ZFCAMT WAERS
     INTO CORRESPONDING FIELDS OF TABLE IT_CST
     FROM ZTBLCST FOR ALL ENTRIES IN IT_BL
    WHERE ZFBLNO = IT_BL-ZFBLNO AND ZFCAMT <> 0.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_CHECK_DATA
*&---------------------------------------------------------------------*
FORM P1000_CHECK_DATA.
  LOOP AT IT_BL.

    MOVE-CORRESPONDING  IT_BL  TO IT_TAB.

    CLEAR : IT_CD.
*    READ TABLE IT_CD WITH KEY ZFCD = IT_BL-ZFSPRTC.
    READ TABLE IT_CD WITH KEY PORT  = IT_BL-ZFSPRTC
                              LAND1 = IT_BL-ZFCARC.
    MOVE IT_CD-PORTT  TO  IT_TAB-SPRTCNM.

    CLEAR : IT_CD.
*    READ TABLE IT_CD WITH KEY ZFCD = IT_BL-ZFAPRTC.
    READ TABLE IT_CD WITH KEY PORT  = IT_BL-ZFAPRTC
                              LAND1 = IT_BL-ZFAPPC.
    MOVE IT_CD-PORTT  TO  IT_TAB-APRTCNM.

    CASE IT_BL-ZFSHTY.
      WHEN 'L'.     IT_TAB-SHTYNM = 'LCL'.
      WHEN 'F'.     IT_TAB-SHTYNM = 'FCL'.
      WHEN 'B'.     IT_TAB-SHTYNM = 'Bulk'.
      WHEN ' '.     IT_TAB-SHTYNM = ''.
      WHEN OTHERS.  IT_TAB-SHTYNM = '***'.
    ENDCASE.

    LOOP AT IT_CST WHERE ZFBLNO = IT_BL-ZFBLNO.
       IF IT_CST-ZFCSCD = 'ABC'.
          IT_TAB-BASIC = IT_TAB-BASIC + IT_CST-ZFCAMT.
          IT_TAB-TOTAL = IT_TAB-TOTAL + IT_CST-ZFCAMT.
          IT_TAB-BASICC = IT_CST-WAERS.
          IT_TAB-TOTALC = IT_CST-WAERS.
       ELSEIF IT_CST-ZFCSCD = 'AHC'.
       ELSE.
          IT_TAB-OTHER = IT_TAB-OTHER + IT_CST-ZFCAMT.
          IT_TAB-TOTAL = IT_TAB-TOTAL + IT_CST-ZFCAMT.
          IT_TAB-OTHERC = IT_CST-WAERS.
          IT_TAB-TOTALC = IT_CST-WAERS.
       ENDIF.
    ENDLOOP.

    APPEND IT_TAB.  CLEAR IT_TAB.

  ENDLOOP.
  SORT IT_TAB BY ZFWERKS ZFFORD ZFETA ZFBLNO.
ENDFORM.                    " P1000_CHECK_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_TOP_PAGE
*&---------------------------------------------------------------------*
FORM P1000_TOP_PAGE.
  SKIP 1.
  WRITE:/70 '   선 적 서 류 입 수 내 역   ' COLOR 1.
  SKIP 1.
  WRITE:/5 'M:자재구분  Inc:Incoterms  B:Basic Charge 지불방법 ',
           'O:Other Charge 지불방법  Y:유한'.
*        120 'PGAE :', SY-PAGNO.
  WRITE:120 'DATE :', SY-DATUM.
  FORMAT COLOR 1 INTENSIFIED OFF.
  ULINE.
  WRITE:/'|' NO-GAP,
         (04) 'Seq.',
         (05) 'Plant',
         (09) 'B/L 번호'.
  SET LEFT SCROLL-BOUNDARY.
  WRITE: (10) '출항일',
         (10) 'Vendor',
         (17) '선적항',
         (15) '총 중량' RIGHT-JUSTIFIED,
         (14) 'Freight Charge' RIGHT-JUSTIFIED,
         (04) 'CURR',
         (15) 'B/L 금액' RIGHT-JUSTIFIED,
         (04) 'CURR',
         (20) '선기명' NO-GAP, '|' NO-GAP.
  WRITE:/'|' NO-GAP,
       23(10) '도착일',
         (10) 'Forwarder',
         (17) '도착항',
         (15) '반입개수' RIGHT-JUSTIFIED,
         (14) 'Other Charge' RIGHT-JUSTIFIED,
         (04) 'CURR',
         (15) '운임율' RIGHT-JUSTIFIED,
         (04) 'CURR',
         (20) 'Master B/L No.' NO-GAP, '|' NO-GAP.
  WRITE:/'|' NO-GAP,
       23(10) '보세운송일',
         (10) 'Trucker',
         (01) 'M',                        " 자재구?
         (03) 'Via',                      " Via
         (03) 'Inc',                      " Incoterms
         (02) 'B',                         " Basic Charge 지불방?
         (02) 'O',                         " Other Charge 지불방?
         (01) 'Y',                         " 유한구?
         (15) '총 용적' RIGHT-JUSTIFIED,
         (14) 'Total Charge' RIGHT-JUSTIFIED,
         (04) 'CURR',
         (08) '선적유형',
         (32) '대표품명' NO-GAP, '|' NO-GAP.
*  WRITE: (10) '출항일',
*         (10) '도착일',
*         (10) '보세운송일',
*         (18) '선적항',
*         (15) '총 중량' RIGHT-JUSTIFIED,
*         (15) '반입개수' RIGHT-JUSTIFIED,
*         (16) 'B/L 금액' RIGHT-JUSTIFIED,
*         (04) 'CURR',
*         (01) 'M',                        " 자재구?
*         (03) 'VIA',                      " Via
*         (03) 'Inc',                      " Incoterms
*         (01) 'B',                         " Basic Charge 지불방?
*         (01) 'O',                         " Other Charge 지불방?
*         (01) 'Y',                         " 유한구?
*         (20) '선기명' NO-GAP, '|' NO-GAP.
*  WRITE:/'|' NO-GAP,
*       23(10) 'Vendor',
*         (10) 'Forwarder',
*         (10) 'Trucker',
*         (18) '도착항',
*         (15) '총 용적' RIGHT-JUSTIFIED,
*         (10) '운임율' RIGHT-JUSTIFIED,
*         (04) 'CURR',
*         (06) '선적',
*         (30) '대표품명',
*         (20) 'Master B/L No.' NO-GAP, '|' NO-GAP.

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
    W_TABIX = SY-TABIX.

    IF W_SUBRC = 1.
       W_SUBRC = 2.    FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
       W_SUBRC = 1.    FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.
    WRITE:/'|' NO-GAP,
           (04) W_TABIX RIGHT-JUSTIFIED,
           (04) IT_TAB-ZFWERKS,
           (10) IT_TAB-ZFBLNO,
           (10) IT_TAB-ZFETD,
           (10) IT_TAB-LIFNR,
           (03) IT_TAB-ZFSPRTC,
           (13) IT_TAB-SPRTCNM,
           (15) IT_TAB-ZFTOVL UNIT IT_TAB-ZFTOVLM,
           (15) IT_TAB-BASIC  CURRENCY IT_TAB-BASICC NO-GAP,
           (04) IT_TAB-BASICC,
           (16) IT_TAB-ZFBLAMT CURRENCY IT_TAB-ZFBLAMC NO-GAP,
           (04) IT_TAB-ZFBLAMC,
           (20) IT_TAB-ZFCARNM NO-GAP, '|' NO-GAP.
    HIDE IT_TAB.
    WRITE:/'|' NO-GAP,
         23(10) IT_TAB-ZFETA,
           (10) IT_TAB-ZFFORD,
           (03) IT_TAB-ZFAPRTC,
           (13) IT_TAB-APRTCNM,
           (15) IT_TAB-ZFPKCN,
           (15) IT_TAB-OTHER  CURRENCY IT_TAB-OTHERC NO-GAP,
           (04) IT_TAB-OTHERC,
           (16) IT_TAB-ZFTRTE CURRENCY IT_TAB-ZFTRTEC NO-GAP,
           (04) IT_TAB-ZFTRTEC,
           (20) IT_TAB-ZFMBLNO NO-GAP, '|' NO-GAP.
    HIDE  IT_TAB.
    WRITE:/'|' NO-GAP,
         23(10) IT_TAB-ZFBNDT,
           (10) IT_TAB-ZFTRCK,
           (01) IT_TAB-ZFMATGB,
           (03) IT_TAB-ZFVIA,
           (03) IT_TAB-INCO1,
           (02) IT_TAB-ZFTRTPM,
           (02) IT_TAB-ZFOTHPM,
           (01) IT_TAB-ZFPOYN,
           (15) IT_TAB-ZFTOWT UNIT  IT_TAB-ZFTOWTM,
           (15) IT_TAB-TOTAL  CURRENCY IT_TAB-TOTALC NO-GAP,
           (04) IT_TAB-TOTALC,
           (01) IT_TAB-ZFSHTY,
           (05) IT_TAB-SHTYNM,
           (33) IT_TAB-ZFRGDSR NO-GAP, '|' NO-GAP.
    HIDE IT_TAB.
*    WRITE:/'|' NO-GAP,
*           (04) W_TABIX RIGHT-JUSTIFIED,
*           (04) IT_TAB-ZFWERKS,
*           (10) IT_TAB-ZFBLNO,
*           (10) IT_TAB-ZFETD,
*           (10) IT_TAB-ZFETA,
*           (10) IT_TAB-ZFBNDT,
*           (03) IT_TAB-ZFSPRTC,
*           (14) IT_TAB-SPRTCNM,
*           (15) IT_TAB-ZFTOVL,
*           (15) IT_TAB-ZFPKCN,
*           (16) IT_TAB-ZFBLAMT CURRENCY IT_TAB-ZFBLAMC,
*           (04) IT_TAB-ZFBLAMC,
*           (01) IT_TAB-ZFMATGB,
*           (03) IT_TAB-ZFVIA,
*           (03) IT_TAB-INCO1,
*           (01) IT_TAB-ZFTRTPM,
*           (01) IT_TAB-ZFOTHPM,
*           (01) IT_TAB-ZFPOYN,
*           (20) IT_TAB-ZFCARNM NO-GAP, '|' NO-GAP.
*    WRITE:/'|' NO-GAP,
*         23(10) IT_TAB-LIFNR,
*           (10) IT_TAB-ZFFORD,
*           (10) IT_TAB-ZFTRCK,
*           (03) IT_TAB-ZFAPRTC,
*           (14) IT_TAB-APRTCNM,
*           (15) IT_TAB-ZFTOWT,
*           (10) IT_TAB-ZFTRTE CURRENCY IT_TAB-ZFTRTEC,
*           (04) IT_TAB-ZFTRTEC,
*           (01) IT_TAB-ZFSHTY,
*           (04) IT_TAB-SHTYNM,
*           (30) IT_TAB-ZFRGDSR,
*           (20) IT_TAB-ZFMBLNO NO-GAP, '|' NO-GAP.
    ULINE.

    AT LAST.
       SUM.
       FORMAT COLOR 3 INTENSIFIED OFF.
       WRITE:/'|' NO-GAP,
              4(20) '합    계',
             59     '중량 ', (15) IT_TAB-ZFTOVL UNIT IT_TAB-ZFTOVLM
                                                LEFT-JUSTIFIED,
             85     '반입개수 ', (15) IT_TAB-ZFPKCN LEFT-JUSTIFIED,
*               (15) IT_TAB-ZFTOVL,
*               (15) IT_TAB-ZFPKCN,
            166 '|' NO-GAP.
      ULINE.
    ENDAT.
  ENDLOOP.
  CLEAR IT_TAB.
ENDFORM.                    " P1000_WRITE_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P1000_DOWNLOAD.
  LOOP AT IT_TAB.

     WMTO_S-AMOUNT  =  IT_TAB-ZFTRTE.
     CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
           EXPORTING
               CURRENCY  = IT_TAB-ZFTRTEC
               AMOUNT_INTERNAL = WMTO_S-AMOUNT
           IMPORTING
               AMOUNT_DISPLAY  = WMTO_S-AMOUNT
           EXCEPTIONS
               INTERNAL_ERROR = 1.
     IT_TAB-ZFTRTE  =  WMTO_S-AMOUNT.

     WMTO_S-AMOUNT  =  IT_TAB-ZFBLAMT.
     CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
           EXPORTING
               CURRENCY  = IT_TAB-ZFBLAMC
               AMOUNT_INTERNAL = WMTO_S-AMOUNT
           IMPORTING
               AMOUNT_DISPLAY  = WMTO_S-AMOUNT
           EXCEPTIONS
               INTERNAL_ERROR = 1.
     IT_TAB-ZFBLAMT  =  WMTO_S-AMOUNT.
     MODIFY IT_TAB.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
        EXPORTING
        FILENAME = 'C:\TEMP\TEMP.TXT'
        FILETYPE = 'DAT'
   TABLES
       DATA_TAB = IT_TAB.

ENDFORM.                    " P1000_DOWNLOAD
