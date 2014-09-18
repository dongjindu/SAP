*&---------------------------------------------------------------------*
*& Report  ZRIMEDIS04                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 지급지시서 SEND                                       *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2002.07.30                                            *
*&    적용회사 : 매일유업                                              *
*&---------------------------------------------------------------------*
*&   DESC.     : 지급지시 내용(CIV) 을 출력한후 EDI SEND 한다.         *
*&---------------------------------------------------------------------*
*& [변경내용]  :                                                       *
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMEDIS04   MESSAGE-ID ZIM NO STANDARD PAGE HEADING
                     LINE-SIZE 115.
INCLUDE : <ICON>,
           ZRIMBDCCOM.

TABLES : ZTCIVHD,    " Commercial Invoice Header Table..
         ZTCIVIT,    " Commercial Invoice Items Table..
         ZTCIVHST,
         LFA1,       " 구매처마스터 (일반섹션) Table..
         SPOP,
         ZTIMIMG00,  " 수입 IMG Config.
         ZTIMIMG11,  " G/R, I/V, 비용처리 Configuration.
         ZTIMIMGTX.

DATA : W_TABIX  LIKE   SY-TABIX,
       W_LFA1   LIKE   LFA1,
       W_LFA1_1 LIKE   LFA1,
       W_BUTTON_ANSWER  TYPE C,
       LINE(3)       TYPE   N,
       OUT_TEXT(70)  TYPE     C.

*>> FUNCTION CALL 한후 RETURN 되는 내역.
DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA:   BEGIN OF IT_ZSCIVIT OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   ZSCIVIT.
DATA:   END   OF IT_ZSCIVIT.

*>> SELECTED 된 FILE COPY
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFCIVRN     LIKE ZTCIVHD-ZFCIVRN,
      ZFCIVNO     LIKE ZTCIVHD-ZFCIVNO,
      ZFEDICK     LIKE ZTCIVHD-ZFEDICK,
      ZFEDIST     LIKE ZTCIVHD-ZFEDIST,
      ZFDOCST     LIKE ZTCIVHD-ZFDOCST,
END OF IT_SELECTED.

*>>> Commercial Invoice 관련자료를 읽기위한 Internal Table 선언..
DATA: BEGIN OF IT_CIV OCCURS 1000,
        ZFCIVRN    LIKE  ZTCIVHD-ZFCIVRN, " Commercial Invoice 관리번호.
        ZFCIVNO    LIKE  ZTCIVHD-ZFCIVNO, " Commercial Invoice Number..
        ZFPRPYN    LIKE  ZTCIVHD-ZFPRPYN, " 선급금여부..
        ZFPRTE     LIKE  ZTCIVHD-ZFPRTE,  " 선급금비율..
        ZFPOYN     LIKE  ZTCIVHD-ZFPOYN,  " 유환여부..
        ZFIVST     LIKE  ZTCIVHD-ZFIVST,  " Invoice Verify 상태..
        ZFMAVN     LIKE  ZTCIVHD-ZFMAVN,  " 물대 거래처코드..
        ZFOPBN     LIKE  ZTCIVHD-ZFOPBN,  " 개설은행 거래처코드..
        ZFIVAMP    LIKE  ZTCIVHD-ZFIVAMP, " Invoice 처리금액..
        ZFIVAMC    LIKE  ZTCIVHD-ZFIVAMC, " Invoice 처리통화..
        ZFIVAMK    LIKE  ZTCIVHD-ZFIVAMK, " Invoice 금액(원화)..
        ZFEXRT     LIKE  ZTCIVHD-ZFEXRT,  " 환율..
        ZFREQNO    LIKE  ZTCIVIT-ZFREQNO, " 수입의뢰 관리번호..
        ZFBLNO     LIKE  ZTCIVIT-ZFBLNO,  " B/L 관리번호..
        EBELN      LIKE  ZTCIVIT-EBELN,   " 구매문서번호..
        ZFDOCST    LIKE  ZTCIVHD-ZFDOCST, " 문서상태.
        ZFEDIST    LIKE  ZTCIVHD-ZFEDIST, " EDI 상태.
        ZFEDICK    LIKE  ZTCIVHD-ZFEDICK, " EDI CHECK.
      END OF IT_CIV.

*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C,
              ZFCIVRN       LIKE ZTCIVHD-ZFCIVRN.
DATA : END OF IT_ERR_LIST.

*------ EDI
DATA : W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO,
       W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
       W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF.

DATA  W_ERR_MSG(100)   TYPE C.

DATA : W_EDI_RECORD(65535),
       W_FILENAME   LIKE  ZTDHF1-FILENAME,
       MI_HANDLE    TYPE  I.
DATA: BEGIN OF IT_EDIFILE OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
      END OF IT_EDIFILE.
DATA: BEGIN OF MTAB_DATA OCCURS 0,
      LINE(132)   TYPE C,
END OF MTAB_DATA.
*-----------------------------------------------------------------------
* 지급지시서 릴리즈 상태 SELECT 조건 PARAMETER
*-----------------------------------------------------------------------
SELECT-OPTIONS : S_EDIST  FOR ZTCIVHD-ZFEDIST NO INTERVALS NO-DISPLAY.
SELECT-OPTIONS : S_EDICK  FOR ZTCIVHD-ZFEDICK NO INTERVALS NO-DISPLAY.

*>>> HIDE를 위한 구조체 선언..
DATA: BEGIN OF DOCU,
        TYPE   TYPE  C,
        CODE   LIKE  ZTCIVHD-ZFCIVRN,
        CIVN   LIKE  ZTCIVHD-ZFCIVNO,
        EDIST  LIKE  ZTCIVHD-ZFEDIST,
        EDICK  LIKE  ZTCIVHD-ZFEDICK,
        DOCST  LIKE  ZTCIVHD-ZFDOCST,
      END OF DOCU.

DATA : HEADERDATA        LIKE    BAPI_INCINV_CREATE_HEADER,
       I_INVOICE         LIKE    RBKP-XRECH,
       I_CREDITMEMO      LIKE    RBKP-XRECH,
       INVOICEDOCNUMBER  LIKE    BAPI_INCINV_FLD-INV_DOC_NO,
       FISCALYEAR        LIKE    BAPI_INCINV_FLD-FISC_YEAR.

DATA: W_ERR_CHK         TYPE C,
      MARKFIELD         TYPE C,
      PRP1              TYPE C,
      PRP2              TYPE C,
      IVST1             TYPE C,
      IVST2             TYPE C,
      W_COUNT           TYPE I,
      W_SELECTED_LINES  TYPE I,
      W_LINE            TYPE I,
      W_PROC_CNT        TYPE I,
      W_MOD             TYPE I,
      W_ERR_CNT         TYPE I,
      INCLUDE(8)        TYPE C,
      W_LIST_INDEX      LIKE SY-TABIX,
      OK-CODE           LIKE SY-UCOMM,
      W_OK_CODE         LIKE SY-UCOMM,
      W_STGRD           LIKE ZTCIVHST-STGRD,
      OPTION(1)         TYPE C,
      ANTWORT(1)        TYPE C,
      CANCEL_OPTION     TYPE C,
      TEXTLEN           TYPE I,
      RADIO_NONE(1)     TYPE C,
      RADIO_ALL(1)      TYPE C,
      RADIO_ERROR(1)    TYPE C,
      DISPMODE(1)       TYPE C.

*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_EBELN   FOR ZTCIVIT-EBELN,
               S_REQNO   FOR ZTCIVIT-ZFREQNO,
               S_BLNO    FOR ZTCIVIT-ZFBLNO,
               S_CIVNO   FOR ZTCIVHD-ZFCIVNO,
               S_CIVRN   FOR ZTCIVHD-ZFCIVRN.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
*>>> EDI 상태.
      PARAMETERS : P_NOOPEN   AS CHECKBOX DEFAULT 'X'. " Yes.
      PARAMETERS : P_OPEN     AS CHECKBOX .            " No.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
*>>> EDI 필수항목 입력여부.
      PARAMETERS : P_OK      AS CHECKBOX DEFAULT 'X'.  " Yes.
      PARAMETERS : P_NOTOK   AS CHECKBOX.              " No.
SELECTION-SCREEN END OF BLOCK B3.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.

   SET TITLEBAR 'ZIM55'.

*-----------------------------------------------------------------------
* TOP-OF-PAGE.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
   IF  INCLUDE  NE 'POPU'.
       PERFORM P3000_TITLE_WRITE.
   ENDIF.

*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 파라메타 설정.
  PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

   PERFORM P1000_READ_CIV_DATA.
   CHECK W_ERR_CHK EQ 'N'.

*-----------------------------------------------------------------------
* END-OF-SELECTION.
*-----------------------------------------------------------------------
END-OF-SELECTION.
   CHECK W_ERR_CHK EQ 'N'.
   SET TITLEBAR 'ZIM55'.
   SET PF-STATUS 'ZIM55'.

   PERFORM P3000_WRITE_CIV_DATA.
*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
 AT USER-COMMAND.

  W_OK_CODE = SY-UCOMM.
  CASE SY-UCOMM.
* 전체 선택 및 선택해제.
    WHEN 'MKAL' OR 'MKLO'.             " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
    WHEN 'DISP'.                       " L/C 조?
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_DOC USING IT_SELECTED-ZFCIVRN
                                    IT_SELECTED-ZFCIVNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'FRGS' OR 'FRGR'.     " EDI FILE CREATE / EDI FILE 취소.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES NE 0.
        PERFORM P2000_POPUP_MESSAGE.   " 메세지 박스.
        IF W_BUTTON_ANSWER EQ '1'.     " 확인일 경우.
          PERFORM P3000_DATA_UPDATE USING W_OK_CODE. " 데이타 반영.
          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.
*    WHEN 'DOWN'.                       " FILE DOWNLOAD....
*      PERFORM P3000_DOWNLOAD_EDI_FILE.
**           PERFORM P3000_TO_PC_DOWNLOAD.
*    WHEN 'REFR'.
** 구매의뢰 테이블 SELECT
*      PERFORM   P1000_READ_CIV_DATA      USING   W_ERR_CHK.
*      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
    WHEN OTHERS.
  ENDCASE.
  CLEAR: DOCU.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

   W_ERR_CHK = 'N'.
   SELECT H~ZFCIVRN AS ZFCIVRN          MAX( H~ZFCIVNO ) AS ZFCIVNO
          MAX( H~ZFPRPYN ) AS ZFPRPYN   MAX( H~ZFPRTE )  AS ZFPRTE
          MAX( H~ZFPOYN )  AS ZFPOYN    MAX( H~ZFIVST )  AS ZFIVST
          MAX( H~ZFMAVN )  AS ZFMAVN    MAX( H~ZFOPBN )  AS ZFOPBN
          MAX( H~ZFIVAMP ) AS ZFIVAMP   MAX( H~ZFIVAMC ) AS ZFIVAMC
          MAX( H~ZFIVAMK ) AS ZFIVAMK   MAX( H~ZFEXRT )  AS ZFEXRT
          MAX( I~ZFREQNO ) AS ZFREQNO   MAX( I~ZFBLNO )  AS ZFBLNO
          MAX( I~EBELN )   AS EBLN      MAX( H~ZFEDIST ) AS ZFEDIST
          MAX( H~ZFEDICK ) AS ZFEDICK   MAX( H~ZFDOCST ) AS ZFDOCST
     INTO CORRESPONDING FIELDS OF TABLE IT_CIV
          FROM   ZTCIVHD AS H INNER JOIN ZTCIVIT AS I
          ON  H~ZFCIVRN   EQ  I~ZFCIVRN
     WHERE  H~ZFCIVRN  IN  S_CIVRN
     AND    H~ZFCIVNO  IN  S_CIVNO
     AND    I~EBELN    IN  S_EBELN
     AND    I~ZFBLNO   IN  S_REQNO
     AND    I~ZFREQNO  IN  S_BLNO
     AND    H~ZFEDIST  IN  S_EDIST
     AND    H~ZFEDICK  IN  S_EDICK
     GROUP BY
            H~ZFCIVRN.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_CIV_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_CIV_DATA.
   LOOP AT IT_CIV.

      ON CHANGE OF IT_CIV-ZFMAVN.
         SELECT SINGLE * INTO W_LFA1
                FROM LFA1 WHERE LIFNR = IT_CIV-ZFMAVN.
      ENDON.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
      WRITE: / SY-VLINE NO-GAP, '  ' NO-GAP,
               MARKFIELD  AS CHECKBOX, ' ' NO-GAP,
             7 SY-VLINE NO-GAP, IT_CIV-ZFCIVRN NO-GAP,
            35 SY-VLINE NO-GAP, W_LFA1-NAME1 NO-GAP.

      ON CHANGE OF IT_CIV-ZFMAVN.
         SELECT SINGLE * INTO W_LFA1_1
                FROM LFA1 WHERE LIFNR = IT_CIV-ZFOPBN.
      ENDON.

      WRITE:   SY-VLINE NO-GAP,
               IT_CIV-ZFIVAMP CURRENCY IT_CIV-ZFIVAMC NO-GAP,
               SY-VLINE NO-GAP, IT_CIV-ZFEXRT NO-GAP,
               SY-VLINE NO-GAP.

      CASE IT_CIV-ZFEDIST.
         WHEN 'N'.
            FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
         WHEN 'R'.
            FORMAT COLOR COL_TOTAL    INTENSIFIED OFF.
         WHEN OTHERS.
            FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
      ENDCASE.
      WRITE : '     '        NO-GAP,
              IT_CIV-ZFEDIST NO-GAP,
              '    '         NO-GAP, SY-VLINE NO-GAP.

      PERFORM P2000_HIDE_VAR_MOVE
              USING 'CIV' IT_CIV-ZFCIVRN IT_CIV-ZFCIVNO IT_CIV-ZFEDIST
                          IT_CIV-ZFEDICK IT_CIV-ZFDOCST.

      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: / SY-VLINE NO-GAP,
             7 SY-VLINE NO-GAP, IT_CIV-ZFCIVNO NO-GAP,
            35 SY-VLINE NO-GAP, W_LFA1_1-NAME1 NO-GAP,
            71 SY-VLINE NO-GAP, 'KRW  ' NO-GAP,
            IT_CIV-ZFIVAMK CURRENCY 'KRW' NO-GAP,
           104 SY-VLINE NO-GAP.

*>>> EDI Check.
      CASE IT_CIV-ZFEDICK.
         WHEN 'N'.
            FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
      ENDCASE.
      WRITE : '     '        NO-GAP,
              IT_CIV-ZFEDICK NO-GAP,
              '    '         NO-GAP, SY-VLINE NO-GAP.

      PERFORM P2000_HIDE_VAR_MOVE
              USING 'CIV' IT_CIV-ZFCIVRN IT_CIV-ZFCIVNO IT_CIV-ZFEDIST
                          IT_CIV-ZFEDICK IT_CIV-ZFDOCST.
      WRITE : SY-ULINE.
*      CLEAR: IT_CIV.
   ENDLOOP.

ENDFORM.                    " P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

   SKIP.
   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
   WRITE 50 '[지급지시서 EDI Send]' NO-GAP
             COLOR COL_HEADING INTENSIFIED ON.

   SKIP.
   WRITE: 99 'DATE : ' NO-GAP,
               SY-DATUM(4) NO-GAP, '/' NO-GAP,
               SY-DATUM+4(2) NO-GAP, '/' NO-GAP,
               SY-DATUM+6(2) NO-GAP.

   ULINE.
   FORMAT COLOR COL_HEADING INTENSIFIED ON.
   WRITE: / SY-VLINE NO-GAP, 'Check' NO-GAP,
            SY-VLINE NO-GAP, 'Commercial Invoice 관리번호' NO-GAP,
            SY-VLINE NO-GAP, 'Benificiary' NO-GAP,
         71 SY-VLINE NO-GAP, 'Invoice 처리금액' NO-GAP,
         91 SY-VLINE NO-GAP, '환률' NO-GAP,
        104 SY-VLINE NO-GAP, ' EDI 상태 ' NO-GAP,
        115 SY-VLINE.

   FORMAT COLOR COL_HEADING INTENSIFIED OFF.
   WRITE: / SY-VLINE NO-GAP,
          7 SY-VLINE NO-GAP, 'Commercial Invoice No.' NO-GAP,
         35 SY-VLINE NO-GAP, '개설은행' NO-GAP,
         71 SY-VLINE NO-GAP, '원화금액(KRW)' NO-GAP,
        104 SY-VLINE NO-GAP, 'EDI CHECK ' NO-GAP,
        115 SY-VLINE NO-GAP,
            SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_HIDE_VAR_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_HIDE_VAR_MOVE USING   P_TYPE  P_CODE P_CIVN P_EDIST
                                 P_EDICK P_DOCST.

CLEAR :  DOCU.

MOVE: P_TYPE    TO   DOCU-TYPE,
      P_CODE    TO   DOCU-CODE,
      P_CIVN    TO   DOCU-CIVN,
      P_EDIST   TO   DOCU-EDIST,
      P_EDICK   TO   DOCU-EDICK,
      P_DOCST   TO   DOCU-DOCST.

HIDE  :  DOCU.
CLEAR :  DOCU.

ENDFORM.                    " P2000_HIDE_VAR_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX    TYPE P,
        ZFCIVRN  LIKE ZTCIVHD-ZFCIVRN.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR   W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_CIV-ZFCIVRN  TO ZFCIVRN.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX    TO INDEX,
                DOCU-CODE  TO IT_SELECTED-ZFCIVRN,
                DOCU-CIVN  TO IT_SELECTED-ZFCIVNO,
                DOCU-EDIST TO IT_SELECTED-ZFEDIST,
                DOCU-EDICK TO IT_SELECTED-ZFEDICK,
                DOCU-DOCST TO IT_SELECTED-ZFDOCST.

         APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P5000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1522   text
*      -->P_1523   text
*      -->P_1524   text
*      -->P_1525   text
*      -->P_1526   text
*----------------------------------------------------------------------*
FORM P5000_MESSAGE_BOX USING    TITLE  LIKE SPOP-TITEL
                                TEXT1  LIKE SPOP-TEXTLINE1
                                TEXT2  LIKE SPOP-TEXTLINE2
                                CANCEL LIKE CANCEL_OPTION
                                DEFAULT LIKE OPTION.

   SPOP-TITEL = TITLE.
   SPOP-TEXTLINE1 = TEXT1.
   SPOP-TEXTLINE2 = TEXT2.
   IF CANCEL EQ 'Y'.
      CANCEL_OPTION = 'Y'.
   ELSE.
      CLEAR : CANCEL_OPTION.
   ENDIF.
   OPTION = DEFAULT.
   TEXTLEN = 40.

   SELECT SINGLE * FROM ZTIMIMG00.
   CLEAR : RADIO_ALL, RADIO_ERROR.
   RADIO_NONE = 'X'.

   IF W_OK_CODE EQ 'IV' OR W_OK_CODE EQ 'CM'.

      AUTHORITY-CHECK OBJECT 'ZM_BDC_MGT'
                      ID 'ACTVT' FIELD '*'.

      IF SY-SUBRC EQ 0.
         IF ZTIMIMG00-ZFIVTY EQ 'L'.
            CALL SCREEN 3515 STARTING AT 12 3
                             ENDING   AT 86 15.
         ELSE.
            CALL SCREEN 3515 STARTING AT 12 3
                             ENDING   AT 86 15.
         ENDIF.
      ELSE.
         RADIO_NONE = 'X'.
         CALL SCREEN 3515 STARTING AT 12 3
                          ENDING   AT 86 15.
      ENDIF.
      IF RADIO_NONE = 'X'.
         DISPMODE = 'N'.
      ENDIF.
      IF RADIO_ALL = 'X'.
         DISPMODE = 'A'.
      ENDIF.
      IF RADIO_ERROR = 'X'.
         DISPMODE = 'E'.
      ENDIF.
   ELSEIF  W_OK_CODE EQ 'MIR2'.

      CALL SCREEN 3516 STARTING AT 34 3
                       ENDING   AT 72 12.
   ELSE.
      CALL SCREEN 0001 STARTING AT 30 6
                       ENDING   AT 78 10.
   ENDIF.

*   IF ANTWORT = 'C'.       " Cancel
*      SET SCREEN SY-DYNNR.
*   ENDIF.

ENDFORM.                    " P5000_MESSAGE_BOX

*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE TABLES   P_IT_ERR_LIST STRUCTURE IT_ERR_LIST.

   MOVE : SY-MSGTY            TO     IT_ERR_LIST-MSGTYP,
          SY-MSGID            TO     IT_ERR_LIST-MSGID,
          SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
          SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
          SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
          SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
          SY-MSGV4            TO     IT_ERR_LIST-MSGV4,
          IT_SELECTED-ZFCIVRN TO     IT_ERR_LIST-ZFCIVRN.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
                MSGID               = IT_ERR_LIST-MSGID
                MSGNR               = IT_ERR_LIST-MSGNR
                MSGV1               = IT_ERR_LIST-MSGV1
                MSGV2               = IT_ERR_LIST-MSGV2
                MSGV3               = IT_ERR_LIST-MSGV3
                MSGV4               = IT_ERR_LIST-MSGV4
         IMPORTING
                MESSAGE_TEXT_OUTPUT = IT_ERR_LIST-MESSTXT.

   CASE IT_ERR_LIST-MSGTYP.
      WHEN 'E'.
         MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
      WHEN 'I'.
         MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'S'.
         MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'W'.
         MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
   ENDCASE.

   APPEND  IT_ERR_LIST.

ENDFORM.                    " P2000_MESSAGE_MAKE

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

  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_WRITE_CIV_DATA.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE TABLES  IT_ERR_LIST STRUCTURE IT_ERR_LIST.

   LOOP AT  RETURN.

      MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
             RETURN-ID           TO     IT_ERR_LIST-MSGID,
             RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
             RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
             RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
             RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
             RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
             RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT,
             IT_SELECTED-ZFCIVRN TO     IT_ERR_LIST-ZFCIVRN.

      CASE IT_ERR_LIST-MSGTYP.
         WHEN 'E'.
            MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
         WHEN 'I'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'S'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'W'.
            MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
      ENDCASE.

      APPEND  IT_ERR_LIST.

   ENDLOOP.

ENDFORM.                    " P2000_MULTI_MSG_MAKE

*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_INVOICE_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTCIVHD_ZFCIVRN  text
*----------------------------------------------------------------------*
FORM P3000_CALL_INVOICE_CANCEL USING    P_ZFCIVRN.

   W_STGRD  =  ZTCIVHST-STGRD.

   SELECT SINGLE * FROM ZTCIVHST
   WHERE  ZFCIVRN   =  P_ZFCIVRN
   AND    ZFCIVHST  =  ( SELECT  MAX( ZFCIVHST )  FROM  ZTCIVHST
                         WHERE   ZFCIVRN  =  P_ZFCIVRN ).
   MOVE  W_STGRD  TO  ZTCIVHST-STGRD.

   CALL FUNCTION 'ZIM_BAPI_INVOICE_CANCEL'
        EXPORTING
            P_ZFCIVRN           =   P_ZFCIVRN
            INVOICEDOCNUMBER    =   ZTCIVHST-BELNR
            FISCALYEAR          =   ZTCIVHST-GJAHR
            REASONREVERSAL      =   ZTCIVHST-STGRD
            POSTINGDATE         =   ZTCIVHST-CBUDAT
        IMPORTING
            INVOICEDOCNUMBER_REVERSAL    =   ZTCIVHST-CBELNR
            FISCALYEAR_REVERSAL          =   ZTCIVHST-CGJAHR
        TABLES
            RETURN              =   RETURN
        EXCEPTIONS
            OTHERS              =   4.

    IF SY-SUBRC NE 0.           ">> 오류 발생시...
       IF RETURN[] IS INITIAL.
          PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
       ELSE.
          PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
       ENDIF.
       ADD    1    TO    W_ERR_CNT.
    ELSE.
       MESSAGE S282(M8) WITH ZTCIVHST-CBELNR.
       PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
       ADD    1    TO    W_PROC_CNT.
       W_SUBRC = 0.
    ENDIF.
*>>> UNLOCK.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCIVHD'
         EXPORTING
         ZFCIVRN      =     IT_SELECTED-ZFCIVRN.

*  IF W_SUBRC NE 0.
*     PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
*     ADD    1    TO    W_ERR_CNT.
*  ENDIF.

ENDFORM.                    " P3000_CALL_INVOICE_CANCEL
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION USING    P_W_ERR_CHK.

  W_ERR_CHK = 'N'.

  IF P_NOOPEN IS INITIAL AND P_OPEN IS INITIAL.
    W_ERR_CHK = 'Y'.   MESSAGE S351.   EXIT.
  ENDIF.

  IF P_OK IS INITIAL AND P_NOTOK IS INITIAL.
    W_ERR_CHK = 'Y'.   MESSAGE S352.   EXIT.
  ENDIF.

*-----------------------------------------------------------------------
* EDI CREATE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_NOOPEN EQ 'X'.
    MOVE: 'I'      TO S_EDIST-SIGN,
          'EQ'     TO S_EDIST-OPTION,
          'N'      TO S_EDIST-LOW.
    APPEND S_EDIST.
  ENDIF.

*-----------------------------------------------------------------------
* EDI CALCLE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_OPEN EQ 'X'.
    MOVE: 'I'      TO S_EDIST-SIGN,
          'EQ'     TO S_EDIST-OPTION,
          'S'      TO S_EDIST-LOW.
    APPEND S_EDIST.
  ENDIF.

*-----------------------------------------------------------------------
* EDI CHECK BIT  SETTING
*-----------------------------------------------------------------------
  IF P_OK EQ 'X'.
    MOVE: 'I'      TO S_EDICK-SIGN,
          'EQ'     TO S_EDICK-OPTION,
          'O'      TO S_EDICK-LOW.
    APPEND S_EDICK.
  ENDIF.

*-----------------------------------------------------------------------
* EDI CALCLE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_NOTOK EQ 'X'.
    MOVE: 'I'      TO S_EDICK-SIGN,
          'EQ'     TO S_EDICK-OPTION,
          'X'      TO S_EDICK-LOW.
    APPEND S_EDICK.
  ENDIF.

ENDFORM.                    " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SELECT_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UCOMM  text
*----------------------------------------------------------------------*
FORM P2000_SELECT_RECORD USING    P_SY_UCOMM.

   DATA : WL_MARK.

   IF P_SY_UCOMM EQ 'MKAL'.
      WL_MARK = 'X'.
   ELSEIF P_SY_UCOMM EQ 'MKLO'.
      CLEAR : WL_MARK.
   ENDIF.
   DO.
      CLEAR MARKFIELD.
      READ LINE SY-INDEX FIELD VALUE MARKFIELD.
      IF SY-SUBRC NE 0.    EXIT.   ENDIF.
      MODIFY CURRENT LINE FIELD VALUE MARKFIELD FROM WL_MARK.
   ENDDO.

ENDFORM.                    " P2000_SELECT_RECORD
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SHOW_DOC USING    P_ZFCIVRN
                             P_ZFCIVNO.

  SET PARAMETER ID 'ZPCIVRN' FIELD P_ZFCIVRN.
  SET PARAMETER ID 'ZPCIVNO' FIELD ''.
  CALL TRANSACTION 'ZIM37' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.

  DATA : TEXT100(100) TYPE  C.
  IF W_OK_CODE EQ 'FRGS'.
    TEXT100 = 'EDI FILE CREATE 작업을 계속 진행하시겠습니까?'.
  ELSEIF W_OK_CODE EQ 'FRGR'.
    TEXT100 = 'EDI FILE CANCLE 작업을 계속 진행하시겠습니까?'.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            TITLEBAR              = 'EDI FILE CREATE/CANCLE 확인'
            DIAGNOSE_OBJECT       = ''
            TEXT_QUESTION         = TEXT100
            TEXT_BUTTON_1         = '확    인'
            TEXT_BUTTON_2         = '아 니 오'
            DEFAULT_BUTTON        = '1'
            DISPLAY_CANCEL_BUTTON = 'X'
            START_COLUMN          = 30
            START_ROW             = 8
       IMPORTING
            ANSWER                = W_BUTTON_ANSWER.


ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_OK_CODE  text
*----------------------------------------------------------------------*
FORM P3000_DATA_UPDATE USING    W_GUBUN.

  DATA : L_RETURN  LIKE   SY-SUBRC,
         L_COUNT   TYPE   I.

  REFRESH : IT_EDIFILE.
  CLEAR : IT_EDIFILE, L_COUNT.
  SORT IT_SELECTED BY ZFCIVRN ZFCIVNO.

  LOOP AT IT_SELECTED.
    W_TABIX = SY-TABIX.
*>>> 진행상태바..
    LINE = ( SY-TABIX / W_SELECTED_LINES ) * 100.
    OUT_TEXT = 'JOB PROGRESS %99999%%'.
    REPLACE '%99999%' WITH LINE INTO OUT_TEXT.
    PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE.
*>> EDI BIT CHECK
    IF W_GUBUN EQ 'FRGS'.              " EDI CREATE
      IF IT_SELECTED-ZFEDICK EQ 'X'.
        MESSAGE I119 WITH IT_SELECTED-ZFCIVRN ' '.
        CONTINUE.
      ENDIF.
      IF IT_SELECTED-ZFDOCST NE 'N'.
        MESSAGE I104 WITH IT_SELECTED-ZFCIVRN
                          ' '          IT_SELECTED-ZFEDIST.
        CONTINUE.
      ENDIF.
    ELSEIF W_GUBUN EQ 'FRGR'.          " EDI CANCLE
      IF IT_SELECTED-ZFDOCST NE 'R'.
        MESSAGE I104 WITH IT_SELECTED-ZFCIVRN
                          ' '    IT_SELECTED-ZFEDIST.
        CONTINUE.
      ENDIF.
    ENDIF.

*>>> 수입의뢰 헤더, 상태 테이블 조회...
    SELECT SINGLE * FROM ZTCIVHD
                    WHERE ZFCIVRN EQ IT_SELECTED-ZFCIVRN.

*>>  개설은행 조회.
    SELECT SINGLE * FROM LFA1
                    WHERE LIFNR   EQ ZTCIVHD-ZFOPBN.
*>>>  EDI 식별자 조회.
    IF LFA1-BAHNS IS INITIAL.
       MESSAGE I274 WITH ZTCIVHD-ZFOPBN.
       CONTINUE.
    ENDIF.

* LOCK CHECK
    PERFORM   P2000_LOCK_MODE_SET  USING    'L'
                                            IT_SELECTED-ZFCIVRN
                                            L_RETURN.
    CHECK L_RETURN EQ 0.

*>>> EDI용 FIELD CREATE.
    IF W_GUBUN EQ 'FRGS'.              " EDI CREATE
      PERFORM   P3000_FILE_CREATE.
*>>> READY KOREA LTD. SAM-FILE WRITE FUNCTION
      CALL FUNCTION 'ZIM_EDI_SAMFILE_WRITE'
           EXPORTING
                ZFCDDOC     = W_ZFCDDOC
                BUKRS       = ZTCIVHD-BUKRS
           IMPORTING
                W_FILENAME  =  W_FILENAME
           TABLES
                EDIFILE = IT_EDIFILE.
      REFRESH : IT_EDIFILE.

      PERFORM  P3000_FILE_TRANSFER.
      IF W_ERR_CHK EQ 'Y'.  CONTINUE.  ENDIF.

    ELSE.
      CALL FUNCTION 'ZIM_EDI_SAMFILE_DELETE'
           EXPORTING
                ZFDHENO = ZTCIVHD-ZFDOCNO.
    ENDIF.

    ADD 1   TO    L_COUNT.             "---> 마지막을 알?

* 상태 변?
*>>>>> 문서취소일 경우, 이전 EDI문서관리번호를 가지고 있기 위해.....
    IF W_GUBUN EQ 'FRGR'.
      MOVE ZTCIVHD-ZFDOCNO  TO  W_ZFDHENO.
    ENDIF.

    MOVE : SY-UNAME    TO    ZTCIVHD-UNAM,
           SY-DATUM    TO    ZTCIVHD-UDAT,
           W_ZFDHENO   TO    ZTCIVHD-ZFDOCNO.
    IF W_GUBUN EQ 'FRGS'.              " EDI CREATE
      MOVE : 'R'     TO    ZTCIVHD-ZFDOCST,
             'S'     TO    ZTCIVHD-ZFEDIST.
    ELSEIF W_GUBUN EQ 'FRGR'.          " EDI CANCLE
      MOVE : 'N'     TO    ZTCIVHD-ZFDOCST,
             'N'     TO    ZTCIVHD-ZFEDIST.
    ENDIF.
*>>> 변경...
    UPDATE ZTCIVHD.

*>>> UNLOCK SETTTING.
    PERFORM   P2000_LOCK_MODE_SET  USING    'U'
                                             IT_SELECTED-ZFCIVRN
                                             L_RETURN.

  ENDLOOP.


ENDFORM.                    " P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SHOW_BAR USING TEXT PERC.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING   PERCENTAGE = PERC
                   TEXT       = TEXT.

ENDFORM.                    " P2000_SHOW_BAR
*&---------------------------------------------------------------------*
*&      Form  P2000_LOCK_MODE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_LOCK_MODE_SET USING    VALUE(P_MODE)
                                  VALUE(P_CIVRN)
                                  P_RETURN.
* LOCK CHECK
  IF P_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTCIVHD'
         EXPORTING
              ZFCIVRN = P_CIVRN
         EXCEPTIONS
              OTHERS  = 1.

    MOVE SY-SUBRC     TO     P_RETURN.
    IF SY-SUBRC NE 0.
      MESSAGE I510 WITH SY-MSGV1 'Import Document' P_CIVRN ' '
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF P_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCIVHD'
         EXPORTING
              ZFCIVRN = P_CIVRN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P3000_FILE_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FILE_CREATE.

*>> DOCUMENT TYPE DETERMINE
  W_ZFCDDOC = 'PAYORD'.

*>> 대표 P/O GET.
  SELECT * FROM ZTCIVIT UP TO 1 ROWS
  WHERE ZFCIVRN    EQ   ZTCIVHD-ZFCIVRN.
  ENDSELECT.

*>>> FIELD MOVE
  W_ZFDHSRO = LFA1-BAHNS.              " 식별자.
  W_ZFDHREF = ZTCIVIT-EBELN.           " 참조번호.
  W_ZFDHENO = ZTCIVHD-ZFDOCNO.         " 문서관리번호.

*>> EDI 환경 GET.
  CLEAR : ZTIMIMGTX.
  SELECT SINGLE * FROM ZTIMIMGTX
  WHERE  BUKRS    EQ   ZTCIVHD-BUKRS.

*>>> EDI 관리번호 SETTING
  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
      EXPORTING
           W_ZFCDDOC = W_ZFCDDOC
           W_ZFDHSRO = W_ZFDHSRO
           W_ZFDHREF = W_ZFDHREF
           W_BUKRS   = ZTCIVHD-BUKRS
      CHANGING
           W_ZFDHENO = W_ZFDHENO
      EXCEPTIONS
           DB_ERROR  = 4
           NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

  CLEAR : W_EDI_RECORD.
*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_MAT_PAYORD_EDI_DOC'
       EXPORTING
            W_ZFCIVRN    = ZTCIVHD-ZFCIVRN
            W_ZFDHENO    = W_ZFDHENO
            W_BAHNS      = LFA1-BAHNS
       IMPORTING
            W_EDI_RECORD = W_EDI_RECORD
       EXCEPTIONS
            CREATE_ERROR = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*>>> INTERNAL TABLE WRITE....
  IT_EDIFILE-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE.

ENDFORM.                    " P3000_FILE_CREATE
*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_CREATE_DATA.

  DATA:  W_SUBRC     LIKE    SY-SUBRC.
  W_ZFDHREF = IT_SELECTED-ZFCIVRN.

  SET UPDATE TASK LOCAL.
  CALL FUNCTION 'ZIM_MAT_PAYORD_EDI_DOC'
       EXPORTING
            W_ZFCIVRN    = IT_SELECTED-ZFCIVRN
            W_ZFDHENO    = W_ZFDHENO
            W_BAHNS      = W_LFA1-BAHNS
       IMPORTING
            W_EDI_RECORD = W_EDI_RECORD
       EXCEPTIONS
            CREATE_ERROR = 4.
  MOVE SY-SUBRC TO W_SUBRC.

  IF W_SUBRC NE 0.                     ">> 오류 발생시...
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
  ENDIF.

  CASE W_SUBRC.
    WHEN  4.    MESSAGE E020 WITH   ''." W_ZFDHENO.
  ENDCASE.

ENDFORM.                    " P4000_CREATE_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_FILE_TRANSFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FILE_TRANSFER .

*>> EDI SERVER로 FTP CONNECT.
   PERFORM  P2000_FTP_CONNECT.
   IF W_ERR_CHK  EQ  'Y'.  EXIT.  ENDIF.

*>> FILE TRANSFER.
   PERFORM  P2000_FTP_COMMAND.

*>> EDI SERVER FTP DISCONNECT.
   PERFORM  P2000_FTP_DISCONNECT.

ENDFORM.                    " P3000_FILE_TRANSFER

*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_CONNECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_CONNECT .

   PERFORM P2000_SHOW_BAR USING 'File Server에 로그인 중입니다...' 0.

DATA: mc_password(20) TYPE c ,
      mc_userid(20)   TYPE c ,
      mi_key          TYPE i VALUE 26101957,
      mi_pwd_len      TYPE i.

   DESCRIBE FIELD MC_PASSWORD LENGTH MI_PWD_LEN.

**-- FTP_CONNECT requires an encrypted password to work
   MC_PASSWORD = 'edi_int'.
   CALL 'AB_RFC_X_SCRAMBLE_STRING'
      ID 'SOURCE' FIELD MC_PASSWORD ID 'KEY'         FIELD MI_KEY
      ID 'SCR'    FIELD 'X'         ID 'DESTINATION' FIELD MC_PASSWORD
      ID 'DSTLEN' FIELD MI_PWD_LEN.

  CALL FUNCTION 'FTP_CONNECT'
       EXPORTING
           USER            = 'edi_int'
           PASSWORD        = MC_PASSWORD
           HOST            = '10.135.9.34'
           RFC_DESTINATION = 'SAPFTP'
      IMPORTING
           HANDLE          = MI_HANDLE
      EXCEPTIONS
           NOT_CONNECTED   = 1
           OTHERS          = 2.

   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S201(04) WITH ZTIMIMGTX-HOST.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S202(04) WITH ZTIMIMGTX-UNAME ZTIMIMGTX-HOST.
   ENDCASE.

ENDFORM.                    " P2000_FTP_CONNECT

*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_COMMAND .

DATA: L_STRLEN           TYPE I,
      L_START            TYPE I,
      L_END              TYPE I,
      L_LEN              TYPE I,
      L_POSITION1        TYPE I,
      L_POSITION2        TYPE I,
      L_FIRST_CHK        TYPE C VALUE 'N',
      L_COMMAND(90)      TYPE C.

   CONCATENATE 'cd' 'edi_send' INTO L_COMMAND SEPARATED BY SPACE.

*> INBOUND DIRECTORY로 이동.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = L_COMMAND
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

   CLEAR : L_COMMAND.
   CONCATENATE ZTIMIMGTX-ZFPATH '/' W_FILENAME INTO L_COMMAND.
   CONCATENATE 'put' L_COMMAND W_FILENAME
                     INTO L_COMMAND SEPARATED BY SPACE.

*> INBOUND DIRECTORY로 이동.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = L_COMMAND
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 0.
         DELETE DATASET  W_FILENAME.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

ENDFORM.                    " P2000_FTP_COMMAND
*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_DISCONNECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_DISCONNECT .

*> FTP DISCONNET.
   CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
            HANDLE   =   MI_HANDLE
      EXCEPTIONS
            OTHERS   =   1.

ENDFORM.                    " P2000_FTP_DISCONNECT
