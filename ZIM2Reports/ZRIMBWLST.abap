*&---------------------------------------------------------------------*
*& Report  ZRIMBWLST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 보세창고출고조회                                      *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.25                                            *
*$     적용회사: LG 화학
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용] 2001.01.31 김영광
*&            : 내용조정 , SORT 및 EXCEL 화면출력 추가
*&---------------------------------------------------------------------*
REPORT  ZRIMBWLST   MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------
INCLUDE   ZRIMBWLSTTOP.
INCLUDE   ZRIMUTIL01.     " Utility function 모음.
INCLUDE   ZRIMOLECOM.     " OLE 공통모듈.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: S_BUKRS     FOR ZTBWHD-BUKRS NO-EXTENSION
                                            NO INTERVALS,
                S_EBELN FOR ZTBWHD-ZFREBELN,
                S_BNARCD FOR ZTBWHD-ZFBNARCD, " 보세구역.
                S_HBLNO FOR ZTBL-ZFHBLNO,
                S_BLNO  FOR ZTBWHD-ZFBLNO,
                S_IVNO  FOR ZTBWHD-ZFIVNO,
                S_GISEQ FOR ZTBWHD-ZFGISEQ,
                S_SHNO  FOR ZTBWHD-ZFSHNO,    " 선적차수.
                S_TRCO  FOR ZTBWHD-ZFTRCO,    " 운송업체.
                S_SEND  FOR ZTBWHD-ZFSENDER,  " 발송자명.
                S_GIDT  FOR ZTBWHD-ZFGIDT,    " 출고일자.
                S_IDSDT FOR ZTBWHD-ZFIDSDT.   " 신고일자.

SELECTION-SCREEN END OF BLOCK B1.
* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.
* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 권한 검증 함수.
*   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
*   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*  테이블 SELECT
  PERFORM   P1000_GET_ZTBW      USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'. MESSAGE S738.   EXIT.    ENDIF.
* 레포트 Write
  PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.  EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'MKAL' OR 'MKLO'.          " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD USING   SY-UCOMM.

    WHEN 'DISP'.                     " 보세창고출고.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM  P2000_DISP_ZTBWHD USING IT_SELECTED-ZFIVNO
                                         IT_SELECTED-ZFGISEQ.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.

    WHEN 'DISP1'.                       " B/L 조회.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_SELECTED-ZFBLNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.

    WHEN 'DISP2'.                     " 통관요청.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_DISP_ZTIV(SAPMZIM09) USING IT_SELECTED-ZFIVNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'DISP3'.                     " 수입신고.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_DISP_ZTIDR(SAPMZIM09) USING IT_SELECTED-ZFBLNO
                                             IT_SELECTED-ZFCLSEQ.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'DISP4'.                     " 수입면허.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_DISP_ZTIDS(SAPMZIM09) USING IT_SELECTED-ZFBLNO
                                            IT_SELECTED-ZFCLSEQ.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'BWPRT'.                     " 외자적송명세서.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_PRINT_ZTBW(SAPMZIM09) USING IT_SELECTED-ZFIVNO
                                            IT_SELECTED-ZFGISEQ.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.

    WHEN 'STUP' OR 'STDN'.
*            W_FIELD_NM = 'ZFETA'.
      GET CURSOR FIELD W_FIELD_NM.
*           ASSIGN W_FIELD_NM   TO <SORT_FIELD>.

      PERFORM SORT_DATA  USING   SY-UCOMM.

    WHEN 'EXCEL'.
      PERFORM P3000_EXCEL_DOWNLOAD.

    WHEN 'REFR'.
*          테이블 SELECT
      PERFORM   P1000_GET_ZTBW       USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM RESET_LIST.
    WHEN OTHERS.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR61'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

  IF SY-LANGU EQ '3'.
    WRITE : /50 '[ 보세창고출고현황 ]'
                 COLOR COL_HEADING INTENSIFIED OFF.
    WRITE : /3 'Date : ', SY-DATUM.
    WRITE : / SY-ULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE : / SY-VLINE, ' ' ,
              SY-VLINE, (13) '플랜트' NO-GAP,
              SY-VLINE,(15) 'P/O No' NO-GAP,
*            SY-VLINE,(10)  '통관요청 No' NO-GAP,
*            SY-VLINE,(06) 'Seq' NO-GAP,
              SY-VLINE,(20) '운송업체' NO-GAP,
              SY-VLINE,(13) '발송자명' NO-GAP,
              SY-VLINE,(16) '운전사' NO-GAP,
              SY-VLINE,(12) '통관일' NO-GAP,
              SY-VLINE,(12) '출고일' NO-GAP,
              SY-VLINE.

    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE :/  SY-VLINE, ' ' ,
              SY-VLINE,(30) 'House B/L No' NO-GAP,
*            SY-VLINE,(06) 'No' NO-GAP,
              SY-VLINE,(20) '출고수량' NO-GAP,
              SY-VLINE,(12) '단가',
              SY-VLINE,(16) '자재번호' NO-GAP,
              SY-VLINE,(26) '내역' NO-GAP,
              SY-VLINE.
  ELSEIF SY-LANGU EQ 'E'.
    WRITE : /50 '[ Bonded warehouse G/I status ]'
                  COLOR COL_HEADING INTENSIFIED OFF.
    WRITE : /3 'Date : ', SY-DATUM.
    WRITE : / SY-ULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE : / SY-VLINE, ' ' ,
              SY-VLINE, (13) 'Plant' NO-GAP,
              SY-VLINE,(15) 'P/O No' NO-GAP,
*            SY-VLINE,(10)  '통관요청 No' NO-GAP,
*            SY-VLINE,(06) 'Seq' NO-GAP,
              SY-VLINE,(20) 'Transportation agent' NO-GAP,
              SY-VLINE,(13) 'Sender' NO-GAP,
              SY-VLINE,(16) 'Driver' NO-GAP,
              SY-VLINE,(12) 'Clearance date' NO-GAP,
              SY-VLINE,(12) 'G/I date' NO-GAP,
              SY-VLINE.

    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE :/  SY-VLINE, ' ' ,
              SY-VLINE,(30) 'House B/L No' NO-GAP,
*            SY-VLINE,(06) 'No' NO-GAP,
              SY-VLINE,(20) 'G/I quantity' NO-GAP,
              SY-VLINE,(12) 'Price unit',
              SY-VLINE,(16) 'Material No' NO-GAP,
              SY-VLINE,(26) 'Particulars' NO-GAP,
              SY-VLINE.
  ENDIF.

  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
  AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
           ID 'ACTVT' FIELD '*'.

  IF SY-SUBRC NE 0.
    MESSAGE S960 WITH SY-UNAME 'Request Release Transaction'.
    W_ERR_CHK = 'Y'.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTBW
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTBW   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
  REFRESH IT_BWHD.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BWHD
     FROM ZTBWHD   AS R INNER JOIN ZTBL AS I
               ON  R~ZFBLNO = I~ZFBLNO
     WHERE R~ZFREBELN IN S_EBELN
       AND R~BUKRS    IN S_BUKRS
       AND R~ZFBNARCD IN S_BNARCD
       AND R~ZFBLNO   IN S_BLNO
       AND I~ZFHBLNO  IN S_HBLNO
       AND R~ZFIVNO   IN S_IVNO
       AND R~ZFGISEQ  IN S_GISEQ
       AND R~ZFSHNO   IN S_SHNO   " 선적차수.
       AND R~ZFTRCO   IN S_TRCO   " 운송업체.
       AND R~ZFSENDER IN S_SEND   " 발송자명.
       AND R~ZFGIDT   IN S_GIDT   " 출고일자.
       AND R~ZFIDSDT  IN S_IDSDT. " 신고일자.
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'. EXIT.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BWIT
           FROM ZTBWIT
           FOR ALL ENTRIES IN IT_BWHD
           WHERE ZFIVNO  = IT_BWHD-ZFIVNO
             AND ZFGISEQ = IT_BWHD-ZFGISEQ.
*>> MODIFY.
  PERFORM   P1000_GET_TEXT.

ENDFORM.                    " P1000_GET_ZTBW.
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'ZIMR61'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIMR61'.           " GUI TITLE SETTING..

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_BWHD.
    W_LINE = W_LINE + 1.
*     PERFORM P2000_PAGE_CHECK.
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
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

  IF W_LINE >= 53.
    WRITE : / SY-ULINE.
    W_PAGE = W_PAGE + 1.    W_LINE = 0.
    NEW-PAGE.
  ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF SY-LANGU EQ '3'.
    IF W_COUNT GT 0.
      WRITE : / '총', W_COUNT, '건'.
    ENDIF.
  ELSEIF SY-LANGU EQ 'E'.
    IF W_COUNT GT 0.
      WRITE : / 'Total', W_COUNT, 'Case'.
    ENDIF.
  ENDIF.

  ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE : / SY-VLINE, MARKFIELD  AS CHECKBOX,
            SY-VLINE,(13) IT_BWHD-WERKS    NO-GAP,
            SY-VLINE,(15) IT_BWHD-W_EBELN  NO-GAP,
*             SY-VLINE,(10) IT_BWHD-ZFIVNO   NO-GAP,
*             SY-VLINE,(06) IT_BWHD-ZFGISEQ  NO-GAP,
            SY-VLINE,(20) IT_BWHD-NAME1    NO-GAP,
            SY-VLINE,(13) IT_BWHD-ZFSENDER NO-GAP,
            SY-VLINE,(16) IT_BWHD-ZFDRVNM  NO-GAP,
            SY-VLINE,(12) IT_BWHD-ZFIDSDT  NO-GAP,
            SY-VLINE,(12) IT_BWHD-ZFGIDT   NO-GAP,
            SY-VLINE.
* hide
  HIDE: IT_BWHD.
  W_COUNT = W_COUNT + 1.
  PERFORM P2000_IT_LINE_WRITE.
  WRITE :/  SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IDS
*&---------------------------------------------------------------------*
FORM P2000_SHOW_IDS USING    P_ZFBLNO P_ZFCLSEQ.

  SET PARAMETER ID 'ZPBLNO'    FIELD  P_ZFBLNO.
  SET PARAMETER ID 'ZPCLSEQ'   FIELD  P_ZFCLSEQ.
  SET PARAMETER ID 'ZPHBLNO'    FIELD ''.
  SET PARAMETER ID 'ZPIDRNO'   FIELD ''.

  CALL TRANSACTION 'ZIM76' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_IDS

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_TEXT
*&---------------------------------------------------------------------*
FORM P1000_GET_TEXT.

  LOOP AT IT_BWHD.
    W_TABIX = SY-TABIX.
    CLEAR LFA1.
    SELECT SINGLE *
           FROM LFA1
          WHERE LIFNR = IT_BWHD-ZFTRCO.
    MOVE LFA1-NAME1 TO IT_BWHD-NAME1.
    IF NOT IT_BWHD-ZFSHNO IS INITIAL.
      CONCATENATE IT_BWHD-ZFREBELN '-' IT_BWHD-ZFSHNO
                  INTO IT_BWHD-W_EBELN.
    ELSE.
      MOVE  IT_BWHD-ZFREBELN  TO  IT_BWHD-W_EBELN.
    ENDIF.

    MODIFY  IT_BWHD INDEX W_TABIX.

  ENDLOOP.

ENDFORM.                    " P1000_GET_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P2000_IT_LINE_WRITE.
  W_FIRST_CHECK = 0.
  LOOP AT IT_BWIT WHERE ZFIVNO  = IT_BWHD-ZFIVNO
                   AND ZFGISEQ  = IT_BWHD-ZFGISEQ.
    ADD 1 TO W_FIRST_CHECK.
    W_TABIX = SY-TABIX.
    PERFORM  P3000_IT_LINE_WRITE.
  ENDLOOP.

ENDFORM.                    " P2000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_IT_LINE_WRITE.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  IF W_FIRST_CHECK = 1.
    WRITE :/ SY-VLINE,'' ,
             SY-VLINE,(30) IT_BWHD-ZFHBLNO  NO-GAP.
  ELSE.
    WRITE :/ SY-VLINE,'' ,
              SY-VLINE,(30) ''  NO-GAP.
  ENDIF.

  WRITE:    " SY-VLINE,(06)IT_BWIT-ZFIVDNO NO-GAP,
            SY-VLINE,(03) IT_BWIT-MEINS,
                     (16) IT_BWIT-GIMENGE
                          UNIT IT_BWIT-MEINS NO-GAP, " 출고수량
*            SY-VLINE,(13) IT_BWIT-PEINH
*                          UNIT IT_BWIT-BPRME NO-GAP, " 가격단위
            SY-VLINE, (13) IT_BWIT-NETPR
                           CURRENCY IT_BWHD-WAERS NO-GAP,
            SY-VLINE,(16) IT_BWIT-MATNR NO-GAP,      " 자재번호'
            SY-VLINE,(26) IT_BWIT-TXZ01 NO-GAP,      " 내역,
                                       SY-VLINE.

ENDFORM.                    " P3000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_BWHD-ZFIVNO  TO IT_SELECTED-ZFIVNO,
             IT_BWHD-ZFGISEQ TO IT_SELECTED-ZFGISEQ,
             IT_BWHD-ZFBLNO  TO IT_SELECTED-ZFBLNO,
             IT_BWHD-ZFCLSEQ  TO IT_SELECTED-ZFCLSEQ.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    MESSAGE S951.
  ENDIF.


ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTBWHD
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTBWHD USING    P_ZFIVNO P_ZFGISEQ.

  SET PARAMETER ID 'ZPIVNO'  FIELD P_ZFIVNO.
  SET PARAMETER ID 'ZPGISEQ' FIELD P_ZFGISEQ.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD ''.

  CALL TRANSACTION 'ZIMBG3'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTBWHD
*&---------------------------------------------------------------------*
*&      Form  P2000_SELECT_RECORD
*&---------------------------------------------------------------------*
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
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM RESET_LIST.
*   MOVE 0 TO SY-LSIND.
*   PERFORM P3000_TITLE_WRITE.
*   PERFORM P3000_DATA_WRITE.
*ENDFORM.                    " RESET_LIST

*&---------------------------------------------------------------------*
*&      Form  P3000_EXCEL_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
  PERFORM P2000_FIT_CELL    USING 3 25 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 3 'B/L No.'.
  PERFORM P2000_FIT_CELL    USING 4 20 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 4 '운송업체'.
  PERFORM P2000_FIT_CELL    USING 5 10 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 5 '발송자명'.
  PERFORM P2000_FIT_CELL    USING 6 10 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 6 '운전사'.
  PERFORM P2000_FIT_CELL    USING 7 10 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 7 '통관일'.
  PERFORM P2000_FIT_CELL    USING 8 10 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 8 '출고일'.
  PERFORM P2000_FIT_CELL    USING 9 13 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 9 '총중량'.
  PERFORM P2000_FIT_CELL    USING 10 8 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 10 '중량단위'.
  PERFORM P2000_FIT_CELL    USING 11 13 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 11 '총용적'.
  PERFORM P2000_FIT_CELL    USING 12 8 'C'.  "COLUMN조정.
  PERFORM P2000_FILL_CELL   USING 1 12 '용적단위'.

  LOOP AT IT_BWHD.
    CLEAR: L_EBELN.
    L_ROW = SY-TABIX + 1.

    CONCATENATE IT_BWHD-ZFREBELN '-' IT_BWHD-ZFSHNO
                 INTO L_EBELN.

    PERFORM P2000_FILL_CELL   USING L_ROW 1 IT_BWHD-WERKS.
    PERFORM P2000_FILL_CELL   USING L_ROW 2 L_EBELN.
    PERFORM P2000_FILL_CELL   USING L_ROW 3 IT_BWHD-ZFHBLNO.
    PERFORM P2000_FILL_CELL   USING L_ROW 4 IT_BWHD-NAME1.
    PERFORM P2000_FILL_CELL   USING L_ROW 5 IT_BWHD-ZFSENDER.
    PERFORM P2000_FILL_CELL   USING L_ROW 6 IT_BWHD-ZFDRVNM.
    PERFORM P2000_FILL_CELL   USING L_ROW 7 IT_BWHD-ZFIDSDT.
    PERFORM P2000_FILL_CELL   USING L_ROW 8 IT_BWHD-ZFGIDT.
    PERFORM P2000_FILL_CELL   USING L_ROW 9 IT_BWHD-ZFTOWT.
    PERFORM P2000_FILL_CELL   USING L_ROW 10 IT_BWHD-ZFTOWTM.
    PERFORM P2000_FILL_CELL   USING L_ROW 11 IT_BWHD-ZFTOVL.
    PERFORM P2000_FILL_CELL   USING L_ROW 12 IT_BWHD-ZFTOVLM.
  ENDLOOP.

  SET PROPERTY OF EXCEL 'VISIBLE' = 1.
ENDFORM.                    " P3000_EXCEL_DOWNLOAD

*&---------------------------------------------------------------------*
*&      Form  SORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UCOMM  text
*----------------------------------------------------------------------*
FORM SORT_DATA USING    P_SY_UCOMM.
  CASE P_SY_UCOMM.
    WHEN 'STUP'.
      IF W_FIELD_NM EQ 'IT_BWHD-WERKS'.
        SORT IT_BWHD BY WERKS  ASCENDING.
      ENDIF.

      IF W_FIELD_NM EQ 'IT_BWHD-W_EBELN'.
        SORT IT_BWHD BY W_EBELN  ASCENDING.
      ENDIF.

      IF W_FIELD_NM EQ 'IT_BWHD-ZFHBLNO'.
        SORT IT_BWHD BY ZFHBLNO  ASCENDING.
      ENDIF.

    WHEN 'STDN'.
      IF W_FIELD_NM EQ 'IT_BWHD-WERKS'.
        SORT IT_BWHD BY WERKS  DESCENDING.
      ENDIF.

      IF W_FIELD_NM EQ 'IT_BWHD-W_EBELN'.
        SORT IT_BWHD BY W_EBELN  DESCENDING.
      ENDIF.

      IF W_FIELD_NM EQ 'IT_BWHD-ZFHBLNO'.
        SORT IT_BWHD BY ZFHBLNO  DESCENDING.
      ENDIF.
  ENDCASE.

  MOVE 0 TO SY-LSIND.
  PERFORM P3000_TITLE_WRITE.
  PERFORM P3000_DATA_WRITE USING W_ERR_CHK.

ENDFORM.                    " SORT_DATA
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
