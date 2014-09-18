*&---------------------------------------------------------------------*
*& INCLUDE ZRIM08I01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 기납증  Main PAI MODULE Include                       *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

* W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
       ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

   IF SY-DYNNR EQ '0020'.
      CASE OK-CODE.
         WHEN 'CANC'.
           ANTWORT = 'C'.
           LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
         WHEN OTHERS.
      ENDCASE.
      EXIT.
   ENDIF.

   IF W_STATUS EQ 'D'.
      SET SCREEN 0.   LEAVE SCREEN.
   ELSE.
      IF SY-DYNNR EQ '0100' OR SY-DYNNR EQ '0200'.
         SET SCREEN 0.   LEAVE SCREEN.
      ELSE.
         PERFORM P2000_SET_MESSAGE USING  SY-UCOMM.
      ENDIF.
   ENDIF.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_DOC_SCR0100 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분기.
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-EBELN   IS INITIAL AND   " P/O NO를 입력하지 않을 경?
     ZSREQHD-ZFREQNO IS INITIAL AND   " 관리번호가 입력하지 않을 경?
     ZSREQHD-ZFOPNNO IS INITIAL.      " 문서번호가 입력하지 않을 경?
     MESSAGE E066.
  ENDIF.

  W_COUNT = 1.

*>> P/O 번호 체크.
  IF NOT ZSREQHD-EBELN IS INITIAL.      " P/O 번호가 입력되었을 경우.
* P/O NO에 Count
     SELECT COUNT( * ) INTO  W_COUNT
                       FROM  ZTREQHD
                       WHERE EBELN EQ ZSREQHD-EBELN.
     CASE W_COUNT.
        WHEN 0.     MESSAGE E064 WITH ZSREQHD-EBELN.
        WHEN 1.
           SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                          FROM ZTREQHD
                          WHERE EBELN EQ ZSREQHD-EBELN.
                EXIT.
           ENDSELECT.
        WHEN OTHERS.
           PERFORM P2000_DOC_ITEM_SELECT1.
           IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
           PERFORM P2000_SEARCH_FIELD_MOVE.
     ENDCASE.
* 수입의뢰 문서 READ PERFORM ?
     PERFORM   P1000_REQ_DOC_READ.
     PERFORM   P3000_MOVE_REQ_TO_TAXBK.
     EXIT.
  ENDIF.

*
  IF NOT ZSREQHD-ZFOPNNO IS INITIAL.  " 문서번호가 입력된 경?
*    IF ZSREQHD-ZFREQNO IS INITIAL.
* 문서 승인번?
        SELECT COUNT( * ) INTO  W_COUNT
                          FROM  ZTREQST
                          WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                          AND   ZFAMDNO EQ '00000'.
        CASE W_COUNT.
           WHEN 0.     MESSAGE E067 WITH ZSREQHD-ZFOPNNO.
           WHEN 1.
              SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                             FROM ZTREQST
                             WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                             AND   ZFAMDNO EQ '00000'.
                   EXIT.
              ENDSELECT.
           WHEN OTHERS.
              PERFORM P2000_OPEN_DOC_ITEM_SELECT.
              IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
              PERFORM P2000_SEARCH_FIELD_MOVE.
        ENDCASE.
*    ENDIF.
* 수입의뢰 문서 READ PERFORM ?
     PERFORM   P1000_REQ_DOC_READ.
     PERFORM   P3000_MOVE_REQ_TO_TAXBK.
     EXIT.
  ENDIF.

*  IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
* 수입의뢰 문서 READ PERFORM ?
  PERFORM  P1000_REQ_DOC_READ.
  PERFORM   P3000_MOVE_REQ_TO_TAXBK.

ENDMODULE.                 " READ_DOC_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0014_LIST_CHECK_SCR0014 INPUT.

  LEAVE TO LIST-PROCESSING.
* LIST WRITE
  PERFORM P2000_DATA_LISTING.


ENDMODULE.                 " D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0100 INPUT.

   IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
      PERFORM  P2000_SET_REQ_SCR.
   ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  ITEM_GET_LINE_SCR0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ITEM_GET_LINE_SCR0110 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0110-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " ITEM_GET_LINE_SCR0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0110 INPUT.

   CHECK : W_EXIT_MODE EQ 'N'.

   READ TABLE IT_ZSTAXBKIT  INDEX TC_0110-CURRENT_LINE.
   W_SY_SUBRC = SY-SUBRC.
   W_TABIX    = SY-TABIX.

   IF W_SY_SUBRC = 0.
      IF NOT ( W_ROW_MARK IS INITIAL ).
         IT_ZSTAXBKIT-ZFMARK = 'X'.
      ELSE.
         CLEAR : IT_ZSTAXBKIT-ZFMARK.
      ENDIF.
      MODIFY IT_ZSTAXBKIT INDEX W_TABIX.
   ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0110 INPUT.

   CASE OK-CODE.
      WHEN 'OTDC' OR 'CRDC' OR 'CHDC' OR 'DISP'.
         PERFORM  P2000_SET_INIT_SCREEN.
      WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
         PERFORM  P2000_EXIT_PROCESS.
      WHEN 'POST'.     ">전기.
         PERFORM  P2000_SET_INDICATE.
      WHEN 'DCDE'.     ">전기취소.
         PERFORM  P2000_SET_INDICATE.
      WHEN 'DELE'.     ">삭제.
         PERFORM  P2000_SET_INDICATE.
      WHEN 'DEL1'.     ">아이템 삭제.
         DELETE IT_ZSTAXBKIT WHERE ZFMARK NE SPACE.
      WHEN 'PORE'.            ">자동계산.
         PERFORM  P2000_AUTO_CALC.
      WHEN 'ZIMG'.
         CALL TRANSACTION OK-CODE.
      WHEN 'FB03'.       ">FI DOC.
         PERFORM  P2000_FI_DOCUMENT_DISPLAY(SAPMZIM03)
                           USING   ZTTAXBKHD-BUKRS
                                   ZTTAXBKHD-GJAHR
                                   ZTTAXBKHD-BELNR.
      WHEN 'ZIMY3'.
         PERFORM P2000_DISPLAY_DOCUMEMT(SAPMZIM03)
                           USING ZTTAXBKHD-ZFACDO
                                 ZTTAXBKHD-ZFFIYR
                                 ZTTAXBKHD-BUKRS.
*      WHEN 'OPDC'.      "전기.
*         PERFORM  P2000_SET_INDICATE.
*      WHEN 'CCDC'.      "전기취소.
*         PERFORM  P2000_SET_INDICATE.
*      WHEN 'FIHI'.      ">문서이력.
*         PERFORM  P2000_POSTING_HISTORY.
      WHEN 'ME23'.      "> P/O 조회.
*         PERFORM  P2000_PO_DOCUMENT_DISPLAY.
          PERFORM  P2000_PO_DOC_DISPLAY(SAPMZIM01)
                   USING ZTTAXBKHD-EBELN ''.
      WHEN 'ZIM03'.
         PERFORM  P2000_LC_DOC_DISPLAY
                  USING  ZTTAXBKHD-ZFREQNO ''.
      WHEN 'MK03'.
         PERFORM  P2000_VENDOR_DISPLAY.
      WHEN 'HIST'.      ">헤더변경사항.
         PERFORM  P2000_HEADER_CHANGE_DOC.
      WHEN 'MM03'.           "자재 기준정보.
         PERFORM  P2000_SELECT_ITEM.     " 선택된 아이템.
         PERFORM  P2000_MATERIAL_DISPLAY.
      WHEN 'MD04'.           "재고/소요량 리스?
           PERFORM P2000_SELECT_ITEM.     " 선택된 자재.
           PERFORM P2000_MATERIAL_MD04.
      WHEN 'MMBE'.           "재고개?
           PERFORM P2000_SELECT_ITEM.     " 선택된 자재.
           PERFORM P2000_MATERIAL_MMBE.
      WHEN 'MB51'.           "자재별 자재문?
           PERFORM P2000_SELECT_ITEM.     " 선택된 자재.
           PERFORM P2000_MATERIAL_MB51.
      WHEN 'ME2M'.           "미결 구매 오?
           PERFORM P2000_SELECT_ITEM.     " 선택된 자재.
           PERFORM P2000_MATERIAL_ME2M.
      WHEN 'ME03'.           "소스 리스?
           PERFORM P2000_SELECT_ITEM.     " 선택된 자재.
           PERFORM P2000_MATERIAL_ME03.
      WHEN 'HIIT'.      ">아이템변경사항.
         PERFORM  P2000_SELECT_ITEM.     " 선택된 아이템.
         PERFORM  P2000_ITEM_CHANGE_DOC.
      WHEN OTHERS.
   ENDCASE.



ENDMODULE.                 " USER_COMMAND_SCR0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_FIELD_CHECK_SCR0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_FIELD_CHECK_SCR0110 INPUT.

   CHECK : W_STATUS NE C_REQ_D.
   CHECK : W_EXIT_MODE EQ 'N'.

   IF ZTTAXBKHD-ZFDCGB IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTAXBKHD' 'ZFDCGB'.
   ENDIF.

*>제출번호.
   IF ZTTAXBKHD-ZFTBPNO IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTAXBKHD' 'ZFTBPNO'.
   ELSE.

   ENDIF.

*>H/S CODE..
   IF ZTTAXBKHD-STAWN IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTAXBKHD' 'STAWN'.
   ENDIF.

*>매입일자..
   IF ZTTAXBKHD-ZFBUYDT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTAXBKHD' 'ZFBUYDT'.
   ENDIF.

*   IF ZTTAXBKHD-ZFBUYMN IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTAXBKHD' 'ZFBUYMN'.
*   ENDIF.

*   IF ZTTAXBKHD-ZFTBAK IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTAXBKHD' 'ZFTBAK'.
*   ENDIF.

*   IF ZTTAXBKHD-ZFCUAMT IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTTAXBKHD' 'ZFCUAMT'.
*   ENDIF.

*   ZTTAXBKHD-ZFTXAMTS = ZTTAXBKHD-ZFCUAMT + ZTTAXBKHD-ZFHMAMT
*                      + ZTTAXBKHD-ZFEDAMT + ZTTAXBKHD-ZFAGAMT.

*     FIELD : ZTTAXBKHD-ZFTBPNO, ">제출번호.
*             ZTTAXBKHD-STAWN,   ">H/S CODE.
*             ZTTAXBKHD-ZFBUYDT, ">매입일자.
*             ZTTAXBKHD-ZFBUYMN, ">매입수량.
*             ZTTAXBKHD-MEINS,   ">매입수량단위.
*             ZTTAXBKHD-ZFTBAK,  ">공급가격.
*             ZTTAXBKHD-ZFKRW,   ">원화통화.
*             ZTTAXBKHD-ZFCUAMT. ">관세


ENDMODULE.                 " INPUT_FIELD_CHECK_SCR0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONT_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONT_UPDATE INPUT.

   CHECK : W_STATUS NE C_REQ_D.
   CHECK : W_EXIT_MODE EQ 'N'.

   READ TABLE IT_ZSTAXBKIT  INDEX TC_0110-CURRENT_LINE.
   W_SUBRC = SY-SUBRC.
   W_TABIX = SY-TABIX.

   IF ZSTAXBKIT-BKMENGE GT 0.
      IF ZSTAXBKIT-TXZ01 IS INITIAL.
         PERFORM NO_INPUT(SAPFMMEX) USING 'ZSTAXBKIT' 'TXZ01'.
      ENDIF.
      IF ZSTAXBKIT-ZFCUAMT IS INITIAL.
         PERFORM NO_INPUT(SAPFMMEX) USING 'ZSTAXBKIT' 'ZFCUAMT'.
      ENDIF.
      IF ZSTAXBKIT-ZFTBAK IS INITIAL.
         PERFORM NO_INPUT(SAPFMMEX) USING 'ZSTAXBKIT' 'ZFTBAK'.
      ENDIF.
      IF ZSTAXBKIT-MEINS  IS INITIAL.
         PERFORM NO_INPUT(SAPFMMEX) USING 'ZSTAXBKIT' 'MEINS'.
      ENDIF.
*> 입력수량 체크..
      W_MENGE = ZSTAXBKIT-MENGE - ZSTAXBKIT-RMENGE.
      IF ZSTAXBKIT-BKMENGE GT W_MENGE.
         PERFORM  P2000_NO_INPUT(SAPMZIM01)
                               USING 'ZSTAXBKIT' 'BKMENGE'
                                     dfies-scrtext_m W_SUBRC.
         MESSAGE E683.
      ENDIF.
**> 입고수량 체크.
      W_MENGE = ZSTAXBKIT-GRMENGE - ZSTAXBKIT-RMENGE.
      IF ZSTAXBKIT-BKMENGE GT W_MENGE.
         PERFORM  P2000_NO_INPUT(SAPMZIM01)
                               USING 'ZSTAXBKIT' 'BKMENGE'
                                     dfies-scrtext_m W_SUBRC.
         MESSAGE E214(ZIM1).
      ENDIF.
  ENDIF.

*> 세액합계.
   ZSTAXBKIT-ZFTXAMTS = ZSTAXBKIT-ZFCUAMT + ZSTAXBKIT-ZFHMAMT
                      + ZSTAXBKIT-ZFEDAMT + ZSTAXBKIT-ZFAGAMT.
   MOVE-CORRESPONDING  ZSTAXBKIT TO IT_ZSTAXBKIT.

   IF W_SUBRC EQ 0.
      MODIFY IT_ZSTAXBKIT INDEX  W_TABIX.
   ELSE.
      APPEND IT_ZSTAXBKIT.
   ENDIF.

ENDMODULE.                 " TABLE_CONT_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  COMMAND_BEFORE_PROCESS_SCR0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE COMMAND_BEFORE_PROCESS_SCR0110 INPUT.

   PERFORM  SET_MODIFY_FIELD_CHECK.

   IF OK-CODE EQ 'MKA1'.
      W_MARK = 'X'.
   ELSE.
      CLEAR : W_MARK.
   ENDIF.

   W_EXIT_MODE = 'Y'.

   CASE OK-CODE.
      WHEN 'MKA1' OR 'MKL1'.  ">전체선택/선택해제..
         LOOP AT IT_ZSTAXBKIT.
           IT_ZSTAXBKIT-ZFMARK = W_MARK.   MODIFY IT_ZSTAXBKIT.
         ENDLOOP.
      WHEN 'REF1'.            ">최신표시.
         SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQIT
                  FROM  ZTREQIT
                  WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.

         PERFORM   P3000_REQIT_MENGE_MOVE.
*      WHEN 'PORE'.            ">자동계산.
*         PERFORM  P2000_AUTO_CALC.
      WHEN OTHERS.
         W_EXIT_MODE = 'N'.
   ENDCASE.

ENDMODULE.                 " COMMAND_BEFORE_PROCESS_SCR0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_DOC_SCR0200 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분기.
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-EBELN   IS INITIAL AND   " P/O NO를 입력하지 않을 경?
     ZSREQHD-ZFREQNO IS INITIAL AND   " 관리번호가 입력하지 않을 경?
     ZSREQHD-ZFOPNNO IS INITIAL AND   " 문서번호가 입력하지 않을 경?
     ZSREQHD-ZFTBNO  IS INITIAL.
     MESSAGE E066.
  ENDIF.

  W_COUNT = 1.

*>> P/O 번호 체크.
  IF NOT ZSREQHD-EBELN IS INITIAL.      " P/O 번호가 입력되었을 경우.
* P/O NO에 Count
     SELECT COUNT( * ) INTO  W_COUNT
                       FROM  ZTTAXBKHD
                       WHERE EBELN EQ ZSREQHD-EBELN.
     CASE W_COUNT.
        WHEN 0.     MESSAGE E684 WITH ZSREQHD-EBELN.
        WHEN 1.
           SELECT ZFTBNO  INTO ZSREQHD-ZFTBNO UP TO 1 ROWS
                          FROM ZTTAXBKHD
                          WHERE EBELN EQ ZSREQHD-EBELN.
                EXIT.
           ENDSELECT.
        WHEN OTHERS.
           REFRESH IT_ZSTAXBKHD.
* Table Multi-Select
           SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSTAXBKHD
                    FROM   ZTTAXBKHD
                    WHERE  EBELN   EQ ZSREQHD-EBELN.

           PERFORM P2000_DOC_ITEM_SELECT.
           IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
           PERFORM P2000_SEARCH_FIELD_MOVE.
     ENDCASE.
* READ PERFORM ?
     PERFORM    P1000_TAXBOOK_READ.
     EXIT.
  ENDIF.

*
  IF NOT ZSREQHD-ZFOPNNO IS INITIAL.  " 문서번호가 입력된 경?
* 문서 승인번?
        SELECT COUNT( * ) INTO  W_COUNT
                          FROM  ZTTAXBKHD
                          WHERE BASISNO EQ ZSREQHD-ZFOPNNO.
        CASE W_COUNT.
           WHEN 0.     MESSAGE E689 WITH ZSREQHD-ZFOPNNO.
           WHEN 1.
              SELECT ZFTBNO  INTO  ZSREQHD-ZFTBNO UP TO 1 ROWS
                             FROM  ZTTAXBKHD
                             WHERE BASISNO EQ ZSREQHD-ZFOPNNO.
                   EXIT.
              ENDSELECT.
           WHEN OTHERS.
              REFRESH IT_ZSTAXBKHD.
* Table Multi-Select
              SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSTAXBKHD
                       FROM   ZTTAXBKHD
                       WHERE  BASISNO EQ ZSREQHD-ZFOPNNO.

              PERFORM P2000_DOC_ITEM_SELECT.
              IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
              PERFORM P2000_SEARCH_FIELD_MOVE.
        ENDCASE.
*    ENDIF.
* READ PERFORM ?
     PERFORM    P1000_TAXBOOK_READ.
     EXIT.
  ENDIF.

*>> 수입의뢰 번호 체크.
  IF NOT ZSREQHD-ZFREQNO IS INITIAL.      "수입의뢰번호..
* P/O NO에 Count
     SELECT COUNT( * ) INTO  W_COUNT
                       FROM  ZTTAXBKHD
                       WHERE ZFREQNO EQ ZSREQHD-ZFREQNO.
     CASE W_COUNT.
        WHEN 0.     MESSAGE E690 WITH ZSREQHD-ZFREQNO.
        WHEN 1.
           SELECT ZFTBNO  INTO ZSREQHD-ZFTBNO UP TO 1 ROWS
                          FROM ZTTAXBKHD
                          WHERE ZFREQNO EQ ZSREQHD-ZFREQNO.
                EXIT.
           ENDSELECT.
        WHEN OTHERS.
           REFRESH IT_ZSTAXBKHD.
* Table Multi-Select
           SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSTAXBKHD
                    FROM   ZTTAXBKHD
                    WHERE  ZFREQNO  EQ ZSREQHD-ZFREQNO.

           PERFORM P2000_DOC_ITEM_SELECT.
           IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
           PERFORM P2000_SEARCH_FIELD_MOVE.
     ENDCASE.
* READ PERFORM ?
     PERFORM    P1000_TAXBOOK_READ.
     EXIT.
  ENDIF.

* 수입의뢰 문서 READ PERFORM ?
  PERFORM    P1000_TAXBOOK_READ.

ENDMODULE.                 " READ_DOC_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  COMPANYCODE_CHECK_SCR00200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE COMPANYCODE_CHECK_SCR0020 INPUT.

   IF ZTBKPF-BUKRS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUKRS'.
   ELSE.
      PERFORM  P1000_GET_COMPANY_CODE(SAPMZIM02) USING ZTBKPF-BUKRS.
*>> IMG 비용계정코드.
      CLEAR: ZTIMIMG11.
      SELECT SINGLE * FROM ZTIMIMG11
             WHERE    BUKRS  EQ   ZTBKPF-BUKRS.
      IF SY-SUBRC NE 0.
         MESSAGE S987 WITH ZTBKPF-BUKRS.
         LEAVE TO SCREEN 0.
      ENDIF.
   ENDIF.

ENDMODULE.                 " COMPANYCODE_CHECK_SCR00200  INPUT
*&---------------------------------------------------------------------*
*&      Module  PERIOD_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERIOD_CHECK_SCR0020 INPUT.

  DATA: S_MONAT LIKE BKPF-MONAT.      "Save field for input period

*> 증빙일.
  IF ZTBKPF-BLDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BLDAT'.
  ENDIF.
  IF ZTBKPF-BUDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUDAT'.
  ENDIF.

  IF ZTBKPF-ZFBDT IS INITIAL.
     ZTBKPF-ZFBDT = ZTBKPF-BUDAT.
  ENDIF.
  CLEAR ZTBKPF-GJAHR.
  S_MONAT = ZTBKPF-MONAT.

  PERFORM PERIODE_ERMITTELN(SAPMZIM02) USING ZTBKPF-BUDAT
                                             ZTBKPF-GJAHR
                                             ZTBKPF-MONAT.

  IF NOT S_MONAT IS INITIAL
  AND    S_MONAT NE ZTBKPF-MONAT
  AND ( SY-BINPT = SPACE AND SY-CALLD = SPACE ).
    MESSAGE W000(F5) WITH S_MONAT ZTBKPF-BUDAT ZTBKPF-MONAT.
  ENDIF.

ENDMODULE.                 " PERIOD_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  BELEGART_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BELEGART_CHECK_SCR0020 INPUT.

   IF ZTBKPF-BLART IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BLART'.
   ENDIF.
*> 문서 종류 체크.
   PERFORM BELEGART_PRUEFEN(SAPFF001)
           USING ZTBKPF-BLART ZTBKPF-GJAHR.
*> 전기년도 체크.
   BKPF-BUKRS = ZTBKPF-BUKRS.
   PERFORM NUMMERNKREIS_LESEN(SAPFF001)
           USING ZTBKPF-GJAHR.
*> 권한 검증.
   PERFORM P2000_BELEGART_AUTHORITY_CHECK(SAPMZIM02).

ENDMODULE.                 " BELEGART_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  VENDOR_ACCOUNT_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VENDOR_ACCOUNT_CHECK_SCR0020 INPUT.

   IF ZTBKPF-BUPLA IS INITIAL.    ">
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
   ENDIF.

   IF ZTBKPF-GSBER IS INITIAL.    ">
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'GSBER'.
   ELSE.
      READ TABLE IT_GSBER WITH KEY ZTBKPF-GSBER.
      IF SY-SUBRC NE 0.
         MESSAGE E407(ZIM1) WITH  ZTBKPF-GSBER.
      ENDIF.
   ENDIF.

   IF ZTBKPF-LIFNR IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'LIFNR'.
   ENDIF.

   CALL FUNCTION 'FI_POSTING_KEY_DATA'
         exporting
              i_bschl       = '31'
              i_umskz       = SPACE       ">bseg-umskz
         importing
              e_t074u       = t074u
              e_tbsl        = tbsl
              e_tbslt       = tbslt
         exceptions
              error_message = 1.

* 1. PBO: no message if bschl request umskz
    if sy-subrc = 1.
* (del) if not ( firstcall = 'X'                           "Note 352492
      if tbsl-xsonu ne space.
        message id sy-msgid type sy-msgty number sy-msgno with
                sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.
*
*>> VENDOR MASTER DEFINE.
    CLEAR : *LFA1.
    SELECT SINGLE * INTO *LFA1 FROM LFA1
                    WHERE LIFNR EQ ZTBKPF-LIFNR.
    IF SY-SUBRC NE 0.
       MESSAGE E023 WITH ZTBKPF-LIFNR.
    ENDIF.

    CALL FUNCTION 'FI_VENDOR_DATA'
         EXPORTING
            i_bukrs = ZTBKPF-BUKRS
            i_lifnr = ZTBKPF-LIFNR
         IMPORTING
            e_kred  = vf_kred.
*
*    IF ZTBKPF-ZTERM NE VF_KRED-ZTERM.
*       MESSAGE W574 WITH  ZTBKPF-LIFNR VF_KRED-ZTERM ZTBKPF-ZTERM.
*    ENDIF.
    ZTBKPF-ZTERM = VF_KRED-ZTERM.

*    if lfb1-bukrs is initial.
*       move-corresponding vf_kred to lfa1.
*    else.
*       move-corresponding vf_kred to lfa1.
*       move-corresponding vf_kred to lfb1.
*       lfb1-sperr = vf_kred-sperr_b.
*       lfb1-loevm = vf_kred-loevm_b.
*       lfb1-begru = vf_kred-begru_b.
*   endif.

   IF ZTBKPF-MWSKZ IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'MWSKZ'.
   ENDIF.

   IF ZTBKPF-AKONT IS INITIAL.
      ZTBKPF-AKONT = vf_kred-AKONT.
   ENDIF.
   IF ZTBKPF-AKONT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'AKONT'.
   ENDIF.
*>> TAX CODE CHECK.
   call function 'FI_TAX_INDICATOR_CHECK'
        exporting
            i_bukrs  = ZTBKPF-BUKRS
            i_hkont  = vf_kred-AKONT
            i_koart  = 'K'
            i_mwskz  = ZTBKPF-MWSKZ
            i_stbuk  = SPACE
            x_dialog = 'X'
       importing
            e_egrkz  = egrkz.
*> ??????.
   IF ZTBKPF-WRBTR IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
   ENDIF.
*> ??.
   IF ZTBKPF-WAERS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
   ENDIF.

*>> 세금자동계산.
   IF ZTBKPF-XMWST EQ 'X'.
      SELECT SINGLE * FROM T007A
             WHERE KALSM EQ 'TAXKR'
             AND   MWSKZ EQ  ZTBKPF-MWSKZ.
      IF SY-SUBRC NE 0.
         MESSAGE E495 WITH 'TAXKR' ZTBKPF-MWSKZ.
      ENDIF.
*>> 조건품목.
      SELECT * FROM  KONP
               WHERE KAPPL EQ 'TX'       ">??.
               AND   KSCHL EQ 'KRIT'     ">?????.
               AND   MWSK1 EQ ZTBKPF-MWSKZ.

         MOVE: KONP-KBETR   TO   W_KBETR,         ">??.
               KONP-KONWA   TO   W_KONWA.         ">??.
         IF NOT W_KBETR IS INITIAL.
            W_KBETR = W_KBETR / 10.
         ENDIF.
      ENDSELECT.

      IF SY-SUBRC EQ 0.  " 세액계산.
         IF NOT W_KBETR IS INITIAL.
            PERFORM SET_CURR_CONV_TO_EXTERNAL(SAPMZIM01)
                    USING ZTBKPF-WRBTR ZTBKPF-WAERS ZTBKPF-WMWST.
*>>>> ?? : (100 + %) =  X : % ======>
            W_WMWST = ZTBKPF-WMWST.
            BAPICURR-BAPICURR = ZTBKPF-WMWST * W_KBETR * 1000.
            W_KBETR1 = W_KBETR.
            W_KBETR = ( W_KBETR + 100 ).
            BAPICURR-BAPICURR = BAPICURR-BAPICURR / W_KBETR.

*           ZTBKPF-WMWST = W_WMWST - ( BAPICURR-BAPICURR * 100 / 1000 ).
*           ZTBKPF-WMWST = ZTBKPF-WMWST / 10.
            BAPICURR-BAPICURR = BAPICURR-BAPICURR / 1000.
            ZTBKPF-WMWST = BAPICURR-BAPICURR.
*            COMPUTE ZTBKPF-WMWST = TRUNC( BAPICURR-BAPICURR ).
*            ZTBKPF-WMWST = ZTBKPF-WMWST * 10.

            PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM01)
                    USING ZTBKPF-WMWST ZTBKPF-WAERS.
         ELSE.
            CLEAR : ZTBKPF-WMWST.
         ENDIF.
      ELSE.
         CLEAR : ZTBKPF-WMWST.
      ENDIF.
   ENDIF.

ENDMODULE.                 " VENDOR_ACCOUNT_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_HEADER_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_HEADER_CHECK_SCR0020 INPUT.
*> 사업장.
  IF ZTBKPF-BUPLA IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
  ENDIF.
*> 전표통화금액.
  IF ZTBKPF-WRBTR IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
  ENDIF.
*> 통화.
  IF ZTBKPF-WAERS IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
  ENDIF.

*> 유환여부..
  IF ZTBKPF-ZFPOYN IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'ZFPOYN'.
  ENDIF.

ENDMODULE.                 " INPUT_HEADER_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  TBTKZ_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TBTKZ_CHECK_SCR0020 INPUT.

  IF ZTBKPF-TBTKZ IS INITIAL.    "> 후속 차변/대변.
     MESSAGE W616.
  ELSE.
     SELECT SINGLE * FROM T163C
                     WHERE SPRAS EQ SY-LANGU
                     AND BEWTP   EQ ZTBKPF-TBTKZ.
     IF SY-SUBRC EQ 0.
        MESSAGE W617 WITH T163C-BEWTL.
     ELSE.
        MESSAGE W617 WITH ZTBKPF-TBTKZ.
     ENDIF.
  ENDIF.

ENDMODULE.                 " TBTKZ_CHECK_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0020 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
       SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.    ANTWORT = 'Y'.
       SET SCREEN 0.   LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0020 INPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZTBKPF-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZTBKPF-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZTBKPF-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE S177(06) WITH ZTBKPF-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZTBKPF-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                 " HELP_ZTERM_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_TBTKZ_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_TBTKZ_SCR0020 INPUT.

  PERFORM   P2000_TBTKZ_HELP(SAPMZIMG)  USING   ZTBKPF-TBTKZ.
  SET CURSOR FIELD 'ZTBKPF-TBTKZ'.

ENDMODULE.                 " HELP_TBTKZ_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF NOT ( SY-UCOMM EQ 'ENTR' OR
           SY-UCOMM EQ 'YES' ).
     EXIT.
  ENDIF.

  IF UF05A-STGRD IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZFTBPNO_DUPLICATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_ZFTBPNO_DUPLICATE INPUT.

   CHECK : W_STATUS NE C_REQ_D.
   CHECK : W_EXIT_MODE EQ 'N'.

*>제출번호.
   IF NOT ZTTAXBKHD-ZFTBPNO IS INITIAL.
      IF W_STATUS EQ C_REQ_C.
         SELECT COUNT( * ) INTO W_COUNT1
                           FROM ZTTAXBKHD
                           WHERE ZFTBPNO  EQ   ZTTAXBKHD-ZFTBPNO.
      ELSEIF W_STATUS EQ C_REQ_U.
         SELECT COUNT( * ) INTO W_COUNT1
                           FROM ZTTAXBKHD
                           WHERE ZFTBPNO  EQ   ZTTAXBKHD-ZFTBPNO
                           AND   ZFTBNO   EQ   ZTTAXBKHD-ZFTBNO.
      ENDIF.
      IF W_COUNT1 GT 0.
         MESSAGE W694 WITH ZTTAXBKHD-ZFTBPNO.
      ENDIF.
   ENDIF.

ENDMODULE.                 " CHECK_ZFTBPNO_DUPLICATE  INPUT
