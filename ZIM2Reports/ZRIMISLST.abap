*&---------------------------------------------------------------------*
*& Report  ZRIMISLST                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 :  보험료 일괄전기                                      *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.02.15                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :보험료 일괄전기 및 소계 조회 리스트.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&   1. 2001/10/12 강석봉 추가/보완 작?
*&      - 전기기능 추?
*&      - 리스트 보험사별/사업장별 GROUP
*&      - 전기취소 기능 추?
*&      - 출력 양식 조?
*&---------------------------------------------------------------------*
REPORT  ZRIMISLST   MESSAGE-ID ZIM
                    LINE-SIZE 127
                    NO STANDARD PAGE HEADING.

TABLES :UF05A,
        BSIS,
        ZVREQHD_INS,
        ZTBKPF,
        *ZTBKPF,
        ZTBL,
        ZTCGHD,
        ZTIV,
        ZTTAXBKHD,
        ZTMSHD,
        BKPF,
        T134G,
        ZTCUCLIV,
        ZTIDR,
        T001,
        T005,
        ZTIMIMG11,
        ZTIMIMG00,
        ZTIMIMG08,
        J_1BBRANCH,
        T001W,
        BAPICURR,
        KONP,
        T007A,
        VF_KRED,
        ZTINSSG3,
        T074U,
       *LFA1,
        TBSL,
        TBSLT.

TYPES : BEGIN OF IT_TAB_TYPE,
               ZFREQNO     LIKE  ZTINS-ZFREQNO,
               ZFINSEQ     LIKE  ZTINS-ZFINSEQ,
               INSAMDNO    LIKE  ZTINS-ZFAMDNO,
               ZFAMDNO     LIKE  ZTREQST-ZFAMDNO,
               BUKRS       LIKE  ZTINS-BUKRS,
               BELNR       LIKE  ZTINS-BELNR,
               GJAHR       LIKE  ZTINS-GJAHR,
               ZFIVAMT     LIKE  ZTINS-ZFINAMT,
               WAERS       LIKE  ZTINS-WAERS,
               ZFWERKS     LIKE  ZTREQHD-ZFWERKS,
               BUPLA       LIKE  ZTBKPF-BUPLA,
               PAMDNO(18),
               ZFINNO      LIKE  ZTINS-ZFINNO,
               ZFINAMT     LIKE  ZTINS-ZFINAMT,
               ZFINAMTC    LIKE  ZTINS-ZFINAMTC,
               ZFKRWAMT    LIKE  ZTINS-ZFKRWAMT,
               ZFKRW       LIKE  ZTINS-ZFKRW,
               ZFSHCUNM    LIKE  ZTINSSG3-ZFSHCUNM,
               ZFARCUNM    LIKE  ZTINSSG3-ZFARCUNM,
               ZFINSDT     LIKE  ZTINS-ZFINSDT,
               ZFOPCD      LIKE  ZTINS-ZFOPCD,
               ZFINSU1     LIKE  ZTINS-ZFINSU1,
               ZFREDON2    LIKE  ZTINS-ZFREDON2,
               EBELN       LIKE  EKKO-EBELN,
               ZFDSOG1     LIKE  ZTINSSG2-ZFDSOG1,
               ZFDOCST     LIKE  ZTINS-ZFDOCST,
               ZFCNCD      LIKE  ZTINSAGR-ZFCNCD,
               ZFCNCDNM    LIKE  ZTINSAGR-ZFCNCDNM,
               ZFISDT      LIKE  ZTINSRSP-ZFISDT,
               ZFACDO      LIKE  ZTRECST-ZFACDO.
TYPES: END   OF IT_TAB_TYPE.

DATA: IT_TAB TYPE STANDARD TABLE OF IT_TAB_TYPE WITH HEADER LINE
                   WITH NON-UNIQUE DEFAULT KEY INITIAL SIZE 100.

DATA : BEGIN OF IT_TAB_DOWN  OCCURS 0,
               ZFREQNO     LIKE  ZTINS-ZFREQNO,
               ZFAMDNO     LIKE  ZTINS-ZFAMDNO,
               PAMDNO(18),
               ZFINNO      LIKE  ZTINS-ZFINNO,
               ZFINAMT(14),
               ZFINAMTC    LIKE  ZTINS-ZFINAMTC,
               ZFKRWAMT(14),
               ZFKRW       LIKE  ZTINS-ZFKRW,
               ZFINSDT     LIKE  ZTINS-ZFINSDT,
               ZFOPCD      LIKE  ZTINS-ZFOPCD,
               ZFINSU1     LIKE  ZTINS-ZFINSU1,
               ZFREDON2    LIKE  ZTINS-ZFREDON2,
               ZFDSOG1     LIKE  ZTINSSG2-ZFDSOG1,
               ZFDOCST     LIKE  ZTINS-ZFDOCST,
               ZFCNCD      LIKE  ZTINSAGR-ZFCNCD,
               ZFCNCDNM    LIKE  ZTINSAGR-ZFCNCDNM,
               ZFISDT      LIKE  ZTINSRSP-ZFISDT,
               ZFACDO      LIKE  ZTRECST-ZFACDO.
DATA : END   OF IT_TAB_DOWN.

DATA : IT_ZSBSEG      LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.
DATA : IT_ZTINS       LIKE ZTINS  OCCURS 100 WITH HEADER LINE.

DATA:   BEGIN OF RETURN OCCURS 0.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

*>>> ERROR.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
INCLUDE  STRUCTURE  BDCMSGCOLL.
DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
       MESSTXT(255) TYPE C.
DATA : END OF IT_ERR_LIST.

TABLES:  SPOP,ZSREQHD.
DATA : OPTION(1)       TYPE C,
       ANTWORT(1)      TYPE C,
       CANCEL_OPTION   TYPE C,
       TEXTLEN         TYPE I,
       INCLUDE(8)      TYPE C,
       W_MOD           TYPE I,
       W_SUBRC             LIKE    SY-SUBRC,
       W_KBETR             LIKE     KONP-KBETR,
       W_KBETR1            LIKE     KONP-KBETR,
       W_KONWA             LIKE     KONP-KONWA,
       W_WMWST             LIKE     ZTBKPF-WMWST,
       W_WMWST1            LIKE     ZTBKPF-WMWST,
       EGRKZ               LIKE     T007A-EGRKZ,
       INVOICEDOCNUMBER    LIKE    BAPI_INCINV_FLD-INV_DOC_NO,
       FISCALYEAR          LIKE    BAPI_INCINV_FLD-FISC_YEAR.

INCLUDE   ZRIMISLSTTOP.
INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.
INCLUDE   <ICON>.

*-----------------------------------------------------------------------
* Selection Screen 용.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: S_BUKRS    FOR ZTREQHD-BUKRS NO-EXTENSION
                                             NO INTERVALS,
                S_EKORG    FOR ZTREQST-EKORG,
                S_EKGRP    FOR ZTREQST-EKGRP,
                S_EBELN    FOR ZTREQHD-EBELN,
                S_INCOM    FOR ZTINS-ZFOPCD,
                S_INSDT    FOR ZTINS-ZFINSDT,
                S_ISDT     FOR ZTINSRSP-ZFISDT,
                S_OPNO     FOR ZTINS-ZFOPNO,
                S_OPNNO    FOR ZTREQHD-ZFOPNNO,
                S_OPBN     FOR ZTREQHD-ZFOPBN,
                S_BENI     FOR ZTREQHD-ZFBENI,     " Beneficiary.
                S_INCO1    FOR ZTREQHD-INCO1,      " Incoterms.
                S_REQTY    FOR ZTREQHD-ZFREQTY,    " 결제구분.
                S_REQNO    FOR ZTREQHD-ZFREQNO.    " 수입의뢰 번호.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS    : P_BLART   LIKE ZTBKPF-BLART OBLIGATORY
                               DEFAULT  'RE',
                P_MWSKZ   LIKE ZTBKPF-MWSKZ OBLIGATORY
                               DEFAULT  'U0'.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

* SORT 구?
SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
SELECTION-SCREEN : COMMENT 4(18) TEXT-021, POSITION 1.
PARAMETERS : P_NO  RADIOBUTTON GROUP RDG.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
SELECTION-SCREEN : COMMENT 4(18) TEXT-022, POSITION 1.
PARAMETERS : P_YES      RADIOBUTTON GROUP RDG.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B2.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

TOP-OF-PAGE.
  IF INCLUDE NE 'POPU'.
     IF SY-LANGU EQ '3'.
        PERFORM   P3000_TITLE_WRITE.
     ELSE.
        PERFORM   P3000_TITLE_WRITE_EN.
     ENDIF.
  ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 테이블 SELECT
  PERFORM   P1000_GET_ZTINS.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
* SORT 선택.
    WHEN 'STUP' OR 'STDN'.         " SORT 선택.
      W_FIELD_NM = 'ZFREQNO'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
* 전체 선택 및 선택해제.
    WHEN 'MKAL' OR 'MKLO'.          " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.

    WHEN 'DISP'.                   " 부보조회.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_INS USING  IT_SELECTED-ZFREQNO
                                      IT_SELECTED-INSAMDNO
                                      IT_SELECTED-ZFINSEQ.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'ZIMY3'.                   " 비용문서조회.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_COST_DOCUMENT USING  IT_SELECTED-BUKRS
                                                IT_SELECTED-GJAHR
                                                IT_SELECTED-BELNR.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'FB03'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_ERR_CHK NE 'Y'.
        ZSREQHD-ZFINSEQ = W_SELECTED_LINES.

        PERFORM P2000_HEADER_DATA_MAKE.
        PERFORM P2000_MESSAGE_POSTING.

        IF ANTWORT EQ 'Y'.
           *ZTBKPF = ZTBKPF.
          REFRESH : IT_ZTINS.
          PERFORM P2000_POSTING_ZTINS.
          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.
    WHEN 'DELE'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_ERR_CHK NE 'Y'.
        ZSREQHD-ZFINSEQ = W_SELECTED_LINES.

*>> POPUP WINDOWS-->전기취소용.
        MOVE 'Premium Accounting Document Cancel' TO SPOP-TITEL.
        IF BSIS-BUDAT IS INITIAL.
          MOVE SY-DATUM    TO BSIS-BUDAT.
        ENDIF.

        CALL SCREEN 0010 STARTING AT 15 1
                         ENDING   AT 52 9.

        IF ANTWORT EQ 'Y'.
           *ZTBKPF = ZTBKPF.
          PERFORM P2000_POSTING_CANCLE.
          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.
    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM P3000_CREATE_DOWNLOAD_FILE.
      PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'REFR'.
      PERFORM   P1000_GET_ZTINS.
      PERFORM RESET_LIST.
*------- Abbrechen (CNCL) ----------------------------------------------
    WHEN 'CNCL'.
      SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
    WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
    WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
    WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
    WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
    WHEN 'WESU'.
    WHEN OTHERS.
  ENDCASE.


*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
  CASE INCLUDE.
    WHEN 'POPU'.
      IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
*            MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
*                    NUMBER IT_ERR_LIST-MSGNR
*                    WITH   IT_ERR_LIST-MSGV1
*                           IT_ERR_LIST-MSGV2
*                           IT_ERR_LIST-MSGV3
*                           IT_ERR_LIST-MSGV4.
        CALL FUNCTION 'MASS_MESSAGE_SHOW_LONGTEXT'
             EXPORTING
                  SPRSL     = SY-LANGU
                  ARBGB     = IT_ERR_LIST-MSGID
                  MSGNR     = IT_ERR_LIST-MSGNR
                  MSGV1     = IT_ERR_LIST-MSGV1
                  MSGV2     = IT_ERR_LIST-MSGV2
                  MSGV3     = IT_ERR_LIST-MSGV3
                  MSGV4     = IT_ERR_LIST-MSGV4
             EXCEPTIONS
                  NOT_FOUND = 1
                  OTHERS    = 2.

      ENDIF.
      CLEAR : IT_ERR_LIST.
    WHEN OTHERS.
  ENDCASE.



*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR20'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /51  '[ 보험료 일괄전기 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 2.
  IF W_PAGE EQ 1.
    WRITE : /91 SY-ULINE,
            /91 SY-VLINE, (9) '기   안' CENTERED,
                SY-VLINE, (9) '심   의' CENTERED,
                SY-VLINE, (9) '확   정' CENTERED,
                SY-VLINE,
            /91 SY-ULINE,
            /91 SY-VLINE,
            103 SY-VLINE,
            115 SY-VLINE,
            127 SY-VLINE,
            /91 SY-VLINE,
            103 SY-VLINE,
            115 SY-VLINE,
            127 SY-VLINE,
            /91 SY-VLINE,
            103 SY-VLINE,
            115 SY-VLINE,
            127 SY-VLINE,
            /91 SY-ULINE.
    SKIP 2.
  ENDIF.

  SELECT SINGLE * FROM J_1BBRANCH
         WHERE BUKRS   EQ IT_TAB-BUKRS
         AND   BRANCH  EQ IT_TAB-BUPLA.

*  WRITE : /3 'Date : ', SY-DATUM.
  WRITE : / '보험회사:', IT_TAB-ZFOPCD, IT_TAB-ZFINSU1,
            '사업장  :', IT_TAB-BUPLA,  J_1BBRANCH-NAME.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, '', SY-VLINE ,
                 (10) '구매오더',
                 (01) '-',
                 (05) '회 차',
                 (01) '-',
                 (05) 'Amend' NO-GAP,   SY-VLINE,
                 (20) '부보금액',       SY-VLINE,
                 (30) '부보조건',       SY-VLINE,
                 (12) '부보일자',       SY-VLINE,
                 (20) '보험료(외화)',   SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE : / SY-VLINE, '', SY-VLINE ,
                 (25) '증권 NO',        SY-VLINE,
                 (20) '선적항',         SY-VLINE,
                 (30) '품명',           SY-VLINE,
                 (12) '발급일자',       SY-VLINE,
                 (20) '보험료(원화)',   SY-VLINE,
          / SY-ULINE.

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
    MESSAGE S960 WITH SY-UNAME '의뢰 Release 트랜잭션'.
    W_ERR_CHK = 'Y'.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTINS.

  REFRESH IT_TAB.

*>> 보험부보 주DATA SELECT.
  CLEAR ZTINS.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
           FROM ZVREQHD_INS1
           WHERE BUKRS    IN  S_BUKRS
           AND   EBELN    IN  S_EBELN
           AND   ZFOPBN   IN  S_OPBN
           AND   INCO1    IN  S_INCO1
           AND   ZFINSDT  IN  S_INSDT
           AND   ZFOPCD   IN  S_INCOM
           AND   ZFREQNO  IN  S_REQNO
           AND   ZFBENI   IN  S_BENI
           AND   ZFOPNO   IN  S_OPNO
           AND   ZFREQTY  IN  S_REQTY
           AND   ZFDOCST  IN ('O', 'A').
  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    SELECT SINGLE * FROM ZTREQST
           WHERE ZFREQNO  EQ  IT_TAB-ZFREQNO
           AND   EKORG    IN  S_EKORG
           AND   EKGRP    IN  S_EKGRP
           AND   ZFOPNNO  IN  S_OPNNO
           AND   ZFAMDNO  EQ
          ( SELECT MAX( ZFAMDNO ) FROM ZTREQST
                   WHERE ZFREQNO  EQ  IT_TAB-ZFREQNO
                   AND   EKORG    IN  S_EKORG
                   AND   EKGRP    IN  S_EKGRP
                   AND   ZFOPNNO  IN  S_OPNNO ).
    IF SY-SUBRC NE 0.
      DELETE IT_TAB INDEX W_TABIX.
      CONTINUE.
    ENDIF.
*>> 전기현황.
    IF P_YES = 'X'.
      IF IT_TAB-BELNR IS INITIAL.
        DELETE IT_TAB INDEX W_TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.
*>>  미전기현황.
    IF P_NO = 'X'.
      IF NOT IT_TAB-BELNR IS INITIAL.
        DELETE IT_TAB INDEX W_TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.

*>> 보험부보 RESPONSE.
    CLEAR ZTINSRSP.
    SELECT SINGLE * FROM ZTINSRSP
           WHERE ZFREQNO = IT_TAB-ZFREQNO
           AND ZFAMDNO   = IT_TAB-INSAMDNO
           AND ZFINSEQ   = IT_TAB-ZFINSEQ
           AND ZFISDT    IN S_ISDT.
    IF SY-SUBRC NE 0.
      DELETE IT_TAB INDEX W_TABIX.
      CONTINUE.
    ENDIF.

    MOVE:  ZTINSRSP-ZFISDT  TO IT_TAB-ZFISDT.

    SELECT SINGLE * FROM ZTINSSG3
           WHERE ZFREQNO = IT_TAB-ZFREQNO
           AND ZFAMDNO   = IT_TAB-INSAMDNO
           AND ZFINSEQ   = IT_TAB-ZFINSEQ.

    MOVE : ZTINSSG3-ZFSHCUNM TO IT_TAB-ZFSHCUNM,
           ZTINSSG3-ZFARCUNM TO IT_TAB-ZFARCUNM.

*>>  SEG2  첫번째 항목 품명 DATA SELECT.
    CLEAR ZTINSSG2.
    SELECT  MIN( ZFLSG2 ) INTO W_MIN_LSG2
            FROM ZTINSSG2
            WHERE ZFREQNO = IT_TAB-ZFREQNO
            AND   ZFINSEQ = IT_TAB-ZFINSEQ
            AND   ZFAMDNO = IT_TAB-INSAMDNO.

    SELECT SINGLE * FROM ZTINSSG2
           WHERE ZFREQNO = IT_TAB-ZFREQNO
           AND ZFINSEQ   = IT_TAB-ZFINSEQ
           AND ZFAMDNO   = IT_TAB-INSAMDNO
           AND ZFLSG2    = W_MIN_LSG2.
    MOVE: ZTINSSG2-ZFDSOG1 TO  IT_TAB-ZFDSOG1.

*>> 보험 조건 SELECT.
    CLEAR ZTINSAGR.
    SELECT  MIN( ZFLAGR ) INTO W_MIN_LAGR
            FROM  ZTINSAGR
            WHERE ZFREQNO = IT_TAB-ZFREQNO
            AND ZFINSEQ   = IT_TAB-ZFINSEQ
            AND ZFAMDNO   = IT_TAB-INSAMDNO.

    SELECT SINGLE * FROM ZTINSAGR
           WHERE ZFREQNO = IT_TAB-ZFREQNO
           AND ZFAMDNO   = IT_TAB-INSAMDNO
           AND ZFINSEQ   = IT_TAB-ZFINSEQ
           AND ZFLAGR    = W_MIN_LAGR.

    MOVE: ZTINSAGR-ZFCNCDNM   TO   IT_TAB-ZFCNCDNM,
          ZTINSAGR-ZFCNCD     TO   IT_TAB-ZFCNCD.
*> 사업장.
    SELECT SINGLE * FROM T001W
           WHERE    WERKS   EQ   IT_TAB-ZFWERKS.
    IF SY-SUBRC EQ 0.
      MOVE T001W-J_1BBRANCH  TO IT_TAB-BUPLA.
    ELSE.
      DELETE IT_TAB INDEX W_TABIX.
      CONTINUE.
    ENDIF.

    MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0. MESSAGE S738. EXIT.ENDIF.

ENDFORM.                    " P1000_GET_ZVREQST

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE .

  SET PF-STATUS 'ZIMR20'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIMR20'.           " GUI TITLE SETTING..

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.    W_LCOUNT = 0.

  SORT IT_TAB BY ZFOPCD BUPLA ZFREQNO ZFINSEQ INSAMDNO .
*  SORT IT_TAB BY ZFREQNO ZFAMDNO.
  LOOP AT IT_TAB.
    W_LINE = W_LINE + 1.
*      PERFORM P2000_PAGE_CHECK.
*>>> FIELD CHANGE.
    ON CHANGE OF IT_TAB-ZFOPCD OR IT_TAB-BUPLA.
      IF SY-TABIX NE 1.
        PERFORM P3000_SUB_TOTAL.
        NEW-PAGE.
        ADD 1 TO W_PAGE.
      ENDIF.
    ENDON.
    PERFORM P3000_LINE_WRITE.
    AT LAST.
      PERFORM P3000_SUB_TOTAL.
      PERFORM P3000_LAST_WRITE.
    ENDAT.
*>> OLD FIELD VALUE MOVE
    CLEAR: OLD_ZFOPCD,OLD_ZFKRW.
    MOVE : IT_TAB-ZFOPCD TO  OLD_ZFOPCD,
           IT_TAB-ZFKRW   TO  OLD_ZFKRW.

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
  IF SY-LANGU EQ '3'.
     PERFORM   P3000_TITLE_WRITE.
  ELSE.
     PERFORM   P3000_TITLE_WRITE_EN.
  ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE .

ENDFORM.                    " RESET_LIST

*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

*  IF W_LINE >= 15.
*      W_PAGE = W_PAGE + 1.    W_LINE = 0.
*      NEW-PAGE.
*  ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL ON.
  SUM.
  WRITE:/ SY-VLINE,82 'Total                   :',
          107 IT_TAB-ZFKRWAMT CURRENCY IT_TAB-ZFKRW,
         127 SY-VLINE.
  WRITE:/ SY-ULINE.
  FORMAT COLOR OFF.

  IF W_COUNT GT 0.
    WRITE : / 'Total', W_COUNT, 'Case'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE : / SY-VLINE, MARKFIELD  AS CHECKBOX, SY-VLINE ,
                     (10)IT_TAB-EBELN,
                     (01) '-',
                     (05)IT_TAB-ZFINSEQ,
                     (01) '-',
                     (05)IT_TAB-INSAMDNO NO-GAP,
                     SY-VLINE,
                     (03)IT_TAB-WAERS,
                     (16)IT_TAB-ZFIVAMT CURRENCY IT_TAB-WAERS, SY-VLINE,
                     (30)IT_TAB-ZFCNCDNM,SY-VLINE,
                     (12)IT_TAB-ZFINSDT ,SY-VLINE,
                      (3)IT_TAB-ZFINAMTC,
                   (16)IT_TAB-ZFINAMT CURRENCY IT_TAB-ZFINAMTC,SY-VLINE.

*hide.
  HIDE: IT_TAB.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE : /  SY-VLINE, '', SY-VLINE ,
                 (25)  IT_TAB-ZFINNO,  SY-VLINE,
*                 (25)  IT_TAB-ZFREDON2, SY-VLINE,
                 (20)  IT_TAB-ZFSHCUNM, SY-VLINE,
                 (30)  IT_TAB-ZFDSOG1,  SY-VLINE,
                 (12)  IT_TAB-ZFISDT,   SY-VLINE,
                 (03)  IT_TAB-ZFKRW,
                 (16)  IT_TAB-ZFKRWAMT CURRENCY IT_TAB-ZFKRW,SY-VLINE,
          / SY-ULINE.
* hide.
  HIDE: IT_TAB.

*>> 소계 보험료.
  ADD   IT_TAB-ZFKRWAMT TO SUB_TOTALK.
  W_COUNT  = W_COUNT  + 1. " 전체 COUNT.
  W_LCOUNT = W_LCOUNT + 1. " 보험사별 COUNT.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_INS  부보조회.
*&---------------------------------------------------------------------*
FORM P2000_SHOW_INS USING    P_ZFREQNO P_ZFAMDNO P_ZFINSEQ.

  SET PARAMETER ID 'ZPOPNNO'   FIELD ' '.
  SET PARAMETER ID 'BES'       FIELD ' '.
  SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
  SET PARAMETER ID 'ZPINSEQ'   FIELD P_ZFINSEQ.

  EXPORT 'BES'           TO MEMORY ID 'BES'.
  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
  EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
  EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.
  EXPORT 'ZPINSEQ'       TO MEMORY ID 'ZPINSEQ'.

  IF P_ZFAMDNO > '00000'.
    CALL TRANSACTION 'ZIM47' AND SKIP  FIRST SCREEN.
  ELSE.
    CALL TRANSACTION 'ZIM43' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                    " P2000_SHOW_INS

*&---------------------------------------------------------------------*
*&      Form P3000_SUB_TOTAL
*&---------------------------------------------------------------------*
FORM P3000_SUB_TOTAL.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL OFF.
  IF IT_TAB-ZFOPCD NE  OLD_ZFOPCD. "마지막 데이타 WRITE 시.
    CLEAR: IT_TAB-ZFKRW.
    MOVE OLD_ZFKRW TO IT_TAB-ZFKRW.
  ENDIF.

  WRITE:/ SY-VLINE,  127 SY-VLINE,
        / SY-VLINE,82 'Insurance Company Sum   :',
         107 SUB_TOTALK CURRENCY  IT_TAB-ZFKRW,
                     127 SY-VLINE,
        / SY-ULINE.
  CLEAR: SUB_TOTALK,W_LCOUNT.

ENDFORM.                    " SUB_TOTAL

*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM P3000_CREATE_DOWNLOAD_FILE.

  REFRESH IT_TAB_DOWN.
  LOOP AT IT_TAB.
    CLEAR IT_TAB_DOWN.
    MOVE-CORRESPONDING IT_TAB TO IT_TAB_DOWN.

    WRITE : IT_TAB-ZFINAMT CURRENCY IT_TAB-ZFINAMTC TO
                                                  IT_TAB_DOWN-ZFINAMT,
            IT_TAB-ZFKRWAMT CURRENCY IT_TAB-ZFKRW TO
                                                  IT_TAB_DOWN-ZFKRWAMT.
    APPEND IT_TAB_DOWN.
  ENDLOOP.

ENDFORM.                    " P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR : W_SELECTED_LINES, ZSREQHD-ZFKRWAMT.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFREQNO  TO IT_SELECTED-ZFREQNO,
             IT_TAB-ZFINSEQ  TO IT_SELECTED-ZFINSEQ,
             IT_TAB-ZFAMDNO  TO IT_SELECTED-ZFAMDNO,
             IT_TAB-INSAMDNO TO IT_SELECTED-INSAMDNO,
             IT_TAB-ZFKRWAMT TO IT_SELECTED-ZFKRWAMT,
             IT_TAB-BUPLA    TO IT_SELECTED-BUPLA,
             IT_TAB-ZFWERKS  TO IT_SELECTED-WERKS,
             IT_TAB-BUKRS    TO IT_SELECTED-BUKRS,
             IT_TAB-ZFOPCD   TO IT_SELECTED-ZFOPCD,
             IT_TAB-BUKRS    TO IT_SELECTED-BUKRS,
             IT_TAB-BELNR    TO IT_SELECTED-BELNR,
             IT_TAB-GJAHR    TO IT_SELECTED-GJAHR.
      APPEND IT_SELECTED.

      MOVE : IT_TAB-ZFOPCD   TO IT_LIFNR-ZFOPCD.
      COLLECT IT_LIFNR.
      ADD IT_SELECTED-ZFKRWAMT TO ZSREQHD-ZFKRWAMT.
      ADD 1 TO W_SELECTED_LINES.

      IF NOT IT_TAB-BELNR IS INITIAL AND SY-UCOMM EQ 'FB03'.
        MESSAGE E401(ZIM1) WITH IT_TAB-ZFINNO.
      ENDIF.

*      IF IT_TAB-ZFKRWAMT LE 0.
*         MESSAGE E404(ZIM1).
*      ENDIF.

      IF IT_TAB-BELNR IS INITIAL AND SY-UCOMM EQ 'DELE'.
        MESSAGE E402(ZIM1) WITH IT_TAB-ZFINNO.
      ENDIF.

    ENDIF.
  ENDDO.

  W_ERR_CHK = 'N'.
  IF W_SELECTED_LINES EQ 0.
    MESSAGE S951.
    W_ERR_CHK = 'Y'.
  ENDIF.

*  DESCRIBE TABLE IT_LIFNR LINES W_LINE.
*  IF W_LINE GT 1.
*     MESSAGE S601(ZIM1).
*     W_ERR_CHK = 'Y'.
*  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_COST_DOCUMENT
*&---------------------------------------------------------------------*
FORM P2000_SHOW_COST_DOCUMENT USING    P_BUKRS
                                       P_GJAHR
                                       P_BELNR.
  IF P_BELNR IS INITIAL.
    MESSAGE S588.
    EXIT.
  ENDIF.
  SET  PARAMETER ID  'BUK'       FIELD   P_BUKRS.
  SET  PARAMETER ID  'GJR'       FIELD   P_GJAHR.
  SET  PARAMETER ID  'ZPBENR'    FIELD   P_BELNR.
  CALL TRANSACTION 'ZIMY3'.

ENDFORM.                    " P2000_SHOW_COST_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P2000_POSTING_ZTINS
*&---------------------------------------------------------------------*
FORM P2000_POSTING_ZTINS.

  REFRESH : IT_ERR_LIST.
  CLEAR : IT_ERR_LIST.

  W_COUNT = 0.
  LOOP AT IT_SELECTED.

*>>> 진행상태바..
    LINE = ( SY-TABIX / W_SELECTED_LINES ) * 100.
    OUT_TEXT = 'JOB PROGRESS %99999%%'.
    REPLACE '%99999%' WITH LINE INTO OUT_TEXT.
    PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE.

    REFRESH : IT_ZSBSEG.
    CLEAR : IT_ZSBSEG, ZTBKPF.

*> 헤더 내역..
    MOVE-CORRESPONDING *ZTBKPF TO ZTBKPF.

    SELECT SINGLE * FROM ZTREQHD
           WHERE    ZFREQNO  EQ IT_SELECTED-ZFREQNO.

    SELECT SINGLE * FROM ZTREQST
           WHERE    ZFREQNO  EQ IT_SELECTED-ZFREQNO
           AND      ZFAMDNO  EQ
                ( SELECT MAX( ZFAMDNO ) FROM ZTREQST
                         WHERE ZFREQNO  EQ IT_SELECTED-ZFREQNO ).

    MOVE : IT_SELECTED-ZFREQNO     TO IT_ZSBSEG-ZFIMDNO,
           IT_SELECTED-ZFINSEQ     TO IT_ZSBSEG-ZFINSEQ,
           IT_SELECTED-INSAMDNO    TO IT_ZSBSEG-ZFAMDNO,
           IT_SELECTED-ZFINNO      TO IT_ZSBSEG-ZUONR,
           SPACE                   TO IT_ZSBSEG-KOSTL,
           '003'                   TO IT_ZSBSEG-ZFCSTGRP.

    SELECT SINGLE * FROM ZTIMIMG08
           WHERE    ZFCDTY   EQ   '003'
           AND      ZFCD     EQ   '1AB'.
    IF SY-SUBRC NE 0.
      MESSAGE S601(ZIM1).
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                  USING  'E'.
      CONTINUE.
    ENDIF.

    MOVE : ZTIMIMG08-COND_TYPE        TO IT_ZSBSEG-COND_TYPE,
           ZTIMIMG08-ZFCD             TO IT_ZSBSEG-ZFCD,
           P_MWSKZ                    TO IT_ZSBSEG-MWSKZ,
           '40'                       TO IT_ZSBSEG-NEWBS,
           'S'                        TO IT_ZSBSEG-SHKZG,
           IT_SELECTED-ZFKRWAMT       TO IT_ZSBSEG-WRBTR,
           0                          TO IT_ZSBSEG-WMWST,
           IT_SELECTED-ZFKRWAMT       TO IT_ZSBSEG-DMBTR,
           1                          TO IT_ZSBSEG-KURSF,
           SY-DATUM                   TO IT_ZSBSEG-WWERT,
           'Y'                        TO IT_ZSBSEG-ZFPOYN,
           ZTIMIMG08-ZFCDNM           TO IT_ZSBSEG-SGTXT.

    IF ZTIMIMG08-ZFCD1 EQ 'Y'.
      MOVE  : 'X'  TO  IT_ZSBSEG-ZFDCSTX,
              'X'  TO  ZTBKPF-ZFDCSTX.
    ELSE.
      CLEAR : IT_ZSBSEG-ZFDCSTX, ZTBKPF-ZFDCSTX.
    ENDIF.

    MOVE:ZTIMIMG11-ZFIOCAC1   TO IT_ZSBSEG-NEWKO.
    MOVE:'Y'                  TO IT_ZSBSEG-ZFPOYN.

    IF IT_ZSBSEG-WRBTR IS INITIAL.
      IT_ZSBSEG-WRBTR = IT_ZSBSEG-DMBTR.
    ENDIF.

*> 음수일 경우... --> 전기키 변경 작업.
    IF IT_SELECTED-ZFKRWAMT  LT 0.
      IT_ZSBSEG-WRBTR = IT_ZSBSEG-WRBTR * -1.
      IT_ZSBSEG-DMBTR = IT_ZSBSEG-DMBTR * -1.
      IT_ZSBSEG-SHKZG = 'H'.
      IT_ZSBSEG-NEWBS = '50'.
    ENDIF.

    PERFORM P1000_IMPORT_DOC_CHEKC
                                     USING IT_ZSBSEG-ZFIMDNO
                                           IT_ZSBSEG-ZFDCNM
                                           IT_ZSBSEG-ZFPOYN
                                           'I'
                                           IT_ZSBSEG-KOSTL.

    PERFORM  P2000_SET_NEWKO
                              USING IT_ZSBSEG-NEWKO
                                    IT_ZSBSEG-ZFCD
                                    IT_ZSBSEG-ZFIMDNO.

    APPEND IT_ZSBSEG.

    MOVE :  IT_SELECTED-BUKRS           TO ZTBKPF-BUKRS.

    CALL FUNCTION 'FI_VENDOR_DATA'
         EXPORTING
              I_BUKRS = IT_SELECTED-BUKRS
              I_LIFNR = IT_SELECTED-ZFOPCD
         IMPORTING
              E_KRED  = VF_KRED
         EXCEPTIONS
              OTHERS  = 4.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
    ZTBKPF-AKONT = VF_KRED-AKONT.
    *LFA1-NAME1  = VF_KRED-NAME1.

*> 전기년도 체크.
    BKPF-BUKRS = ZTBKPF-BUKRS.
    PERFORM NUMMERNKREIS_LESEN(SAPFF001)
            USING ZTBKPF-GJAHR.

    MOVE: SY-MANDT             TO ZTBKPF-MANDT,
          'X'                  TO ZTBKPF-ZFPCUR,
          'Y'                  TO ZTBKPF-ZFPOYN,
          SY-DATUM(4)          TO ZTBKPF-GJAHR,
         *ZTBKPF-BUDAT         TO ZTBKPF-BUDAT,
         *ZTBKPF-BLDAT         TO ZTBKPF-BLDAT,
         *ZTBKPF-ZFBDT         TO ZTBKPF-ZFBDT,
         *ZTBKPF-ZTERM         TO ZTBKPF-ZTERM,
          P_BLART              TO ZTBKPF-BLART,
          '003'                TO ZTBKPF-ZFCSTGRP,
          SY-DATUM+4(2)        TO ZTBKPF-MONAT,
          P_MWSKZ              TO ZTBKPF-MWSKZ,
          1                    TO ZTBKPF-KURSF,
          SY-DATUM             TO ZTBKPF-WWERT,
          T001-WAERS           TO ZSREQHD-ZFKRW,
          T001-WAERS           TO ZTBKPF-HWAER,
          IT_SELECTED-ZFKRWAMT TO ZTBKPF-WRBTR,
          IT_SELECTED-ZFKRWAMT TO ZTBKPF-DMBTR,
          T001-WAERS           TO ZTBKPF-WAERS,
          'N'                  TO ZTBKPF-ZFPOSYN,
          IT_SELECTED-ZFOPCD   TO ZTBKPF-LIFNR,
          IT_SELECTED-ZFOPCD   TO ZTBKPF-ZFVEN,
          'X'                  TO ZTBKPF-ZFAUTO,
          'X'                  TO ZTBKPF-ZFATPT.

*> 음수일 경우... --> 전기키 변경 작업.
    IF IT_SELECTED-ZFKRWAMT  LT 0.
      ZTBKPF-WRBTR = ZTBKPF-WRBTR * -1.
      ZTBKPF-DMBTR = ZTBKPF-DMBTR * -1.
      ZTBKPF-ZFRVSX = 'X'.
    ENDIF.

    ADD IT_SELECTED-ZFKRWAMT TO ZSREQHD-ZFKRWAMT.

*> 회계전기.
    PERFORM P3000_FI_POSTING.

    W_COUNT = W_COUNT + 1.
  ENDLOOP.

  DESCRIBE TABLE IT_ZTINS LINES W_LINE.
  IF W_LINE GT 0.
    MODIFY ZTINS FROM TABLE IT_ZTINS.
    IF SY-SUBRC NE 0.
      MESSAGE E429 WITH 'Insurance Data'.
    ENDIF.
  ENDIF.

  DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
  IF W_LINE GT 0.
    INCLUDE = 'POPU'.
    CALL SCREEN 0100 STARTING AT  05   3
                     ENDING   AT  100 12.
    CLEAR : INCLUDE.
  ENDIF.

ENDFORM.                    " P2000_POSTING_ZTINS
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

  SET TITLEBAR 'POPU'.
  SET PF-STATUS 'POPU'.

  IF OPTION = '1'.
    SET CURSOR FIELD 'SPOP-OPTION1'.
  ELSE.
    SET CURSOR FIELD 'SPOP-OPTION2'.
  ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'SPOP-OPTION_CAN'.
      IF CANCEL_OPTION = SPACE.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE'.                   "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'ENTR'.   EXIT.
    WHEN 'YES'.    ANTWORT = 'Y'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.     ANTWORT = 'N'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_BOX USING    TITLE  LIKE SPOP-TITEL
                                TEXT1  LIKE SPOP-TEXTLINE1
                                TEXT2  LIKE SPOP-TEXTLINE2
                                CANCEL LIKE CANCEL_OPTION
                                DEFAULT LIKE OPTION.

  SPOP-TITEL     = TITLE.
  SPOP-TEXTLINE1 = TEXT1.
  SPOP-TEXTLINE2 = TEXT2.
  IF CANCEL EQ 'Y'.
    CANCEL_OPTION = 'Y'.
  ELSE.
    CLEAR : CANCEL_OPTION.
  ENDIF.
  OPTION = DEFAULT.
  TEXTLEN = 40.

  CALL SCREEN 0001 STARTING    AT 10 2
                      ENDING   AT 90 16.

*  IF ANTWORT = 'C'.                                         " Cancel
*    SET SCREEN SY-DYNNR.
*  ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_FOSTING
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_FOSTING.

  PERFORM P2000_MESSAGE_BOX USING '전기 확인'             " 타이틀...
                          '선택된자료를 전기합니다 '
                          '전기하시겠습니까?'               " MSG2
                          'N'                 " 취소 버튼 유/?
                          '1'.                " default button


ENDFORM.                    " P2000_MESSAGE_FOSTING
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  ANTWORT = 'C'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  PERIOD_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERIOD_CHECK_SCR0001 INPUT.
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

ENDMODULE.                 " PERIOD_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  BELEGART_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BELEGART_CHECK_SCR0001 INPUT.

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


ENDMODULE.                 " BELEGART_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  VENDOR_ACCOUNT_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VENDOR_ACCOUNT_CHECK_SCR0001 INPUT.

  IF ZTBKPF-LIFNR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'LIFNR'.
  ENDIF.

  CALL FUNCTION 'FI_POSTING_KEY_DATA'
       EXPORTING
            I_BSCHL       = '31'
            I_UMSKZ       = SPACE  ">bseg-umskz
       IMPORTING
            E_T074U       = T074U
            E_TBSL        = TBSL
            E_TBSLT       = TBSLT
       EXCEPTIONS
            ERROR_MESSAGE = 1.

  IF SY-SUBRC = 1.
    IF TBSL-XSONU NE SPACE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH
              SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*>> VENDOR MASTER DEFINE.
  CLEAR : *LFA1.
  SELECT SINGLE * INTO *LFA1 FROM LFA1
                  WHERE LIFNR EQ ZTBKPF-LIFNR.
  IF SY-SUBRC NE 0.
    MESSAGE E023 WITH ZTBKPF-LIFNR.
  ENDIF.

  CALL FUNCTION 'FI_VENDOR_DATA'
       EXPORTING
            I_BUKRS = ZTBKPF-BUKRS
            I_LIFNR = ZTBKPF-LIFNR
       IMPORTING
            E_KRED  = VF_KRED.
  IF ZTBKPF-ZTERM NE VF_KRED-ZTERM.
    IF ZTBKPF-ZTERM IS INITIAL.
      ZTBKPF-ZTERM = VF_KRED-ZTERM.
    ELSE.
      MESSAGE W574 WITH  ZTBKPF-LIFNR VF_KRED-ZTERM ZTBKPF-ZTERM.
    ENDIF.
  ENDIF.

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
    ZTBKPF-AKONT = VF_KRED-AKONT.
  ENDIF.
  IF ZTBKPF-AKONT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'AKONT'.
  ENDIF.
*>> TAX CODE CHECK.
  CALL FUNCTION 'FI_TAX_INDICATOR_CHECK'
       EXPORTING
            I_BUKRS  = ZTBKPF-BUKRS
            I_HKONT  = VF_KRED-AKONT
            I_KOART  = 'K'
            I_MWSKZ  = ZTBKPF-MWSKZ
            I_STBUK  = SPACE
            X_DIALOG = 'X'
       IMPORTING
            E_EGRKZ  = EGRKZ.
*> ??????.
  IF ZTBKPF-WRBTR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
  ENDIF.
*> ??.
  IF ZTBKPF-WAERS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
  ENDIF.

*>> ??? ??.
  IF ZTBKPF-XMWST EQ 'X'.
    SELECT SINGLE * FROM T007A
           WHERE KALSM EQ 'TAXUS'
           AND   MWSKZ EQ  ZTBKPF-MWSKZ.
    IF SY-SUBRC NE 0.
      MESSAGE E495 WITH 'TAXUS' ZTBKPF-MWSKZ.
    ENDIF.

    SELECT * FROM  KONP
             WHERE KAPPL EQ 'TX'       ">??.
             AND   KSCHL EQ 'MWVS'     ">?????.
             AND   MWSK1 EQ ZTBKPF-MWSKZ.

      MOVE: KONP-KBETR   TO   W_KBETR,         ">??.
            KONP-KONWA   TO   W_KONWA.         ">??.
      IF NOT W_KBETR IS INITIAL.
        W_KBETR = W_KBETR / 10.
      ENDIF.
    ENDSELECT.

    IF SY-SUBRC EQ 0.
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


ENDMODULE.                 " VENDOR_ACCOUNT_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_HEADER_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_HEADER_CHECK_SCR0001 INPUT.
*> 전표통화금액.
  IF ZTBKPF-WRBTR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WRBTR'.
  ENDIF.
*> 통화.
  IF ZTBKPF-WAERS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'WAERS'.
  ENDIF.

ENDMODULE.                 " INPUT_HEADER_CHECK_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Message LIST'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.

  LEAVE TO LIST-PROCESSING.
  CASE INCLUDE.
    WHEN 'POPU'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE(105), / SY-VLINE NO-GAP,
                'Type'   NO-GAP, SY-VLINE NO-GAP,
                'Message Text', 103 SY-VLINE NO-GAP,
                'T'      NO-GAP, SY-VLINE,
              / SY-ULINE(105).

      LOOP AT IT_ERR_LIST.
        W_MOD  =  SY-TABIX MOD 2.
        FORMAT RESET.
        IF W_MOD EQ 0.
          FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
        ELSE.
          FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
        ENDIF.
        WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)   NO-GAP,
                  SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(96) NO-GAP,
                  SY-VLINE NO-GAP.

        CASE IT_ERR_LIST-MSGTYP.
          WHEN 'E' OR 'A'.
            FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
          WHEN 'W'.
            FORMAT COLOR COL_KEY      INTENSIFIED OFF.
          WHEN 'I'.
            FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
          WHEN 'S'.
            FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
        ENDCASE.

        WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
*                   / SY-ULINE(96).
        HIDE:IT_ERR_LIST.
      ENDLOOP.
      WRITE : / SY-ULINE(105).
      CLEAR : IT_ERR_LIST.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT



*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST
                         USING   P_MSGTY.

  MOVE : P_MSGTY             TO     IT_ERR_LIST-MSGTYP,
         SY-MSGID            TO     IT_ERR_LIST-MSGID,
         SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
         SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
         SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
         SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
         SY-MSGV4            TO     IT_ERR_LIST-MSGV4.

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
    WHEN 'E' OR 'A'.
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
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE TABLES   IT_ERR_LIST STRUCTURE IT_ERR_LIST.

  LOOP AT  RETURN.

    MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
           RETURN-ID           TO     IT_ERR_LIST-MSGID,
           RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
           RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
           RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
           RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
           RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
           RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT.

    CASE IT_ERR_LIST-MSGTYP.
      WHEN 'E' OR 'A'.
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
*&      Form  P2000_MESSAGE_POSTING
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_POSTING.

  PERFORM P2000_MESSAGE_BOX USING 'Posting confirm'
                          'Post selected data.'
                          'Do you want to post?'
                          'N'
                          '1'.


ENDFORM.                    " P2000_MESSAGE_POSTING

*&---------------------------------------------------------------------*
*&      Form  P3000_FI_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FI_POSTING.
  DATA : L_ZTBKPF LIKE ZTBKPF.

  SET UPDATE TASK LOCAL.
  CLEAR : L_ZTBKPF.
  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
       EXPORTING
            W_OK_CODE    = 'SAVE'
            BUKRS        = ZTBKPF-BUKRS
            GJAHR        = ZTBKPF-GJAHR
            ZFSTATUS     = 'C'
            W_ZTBKPF_OLD = L_ZTBKPF
            W_ZTBKPF     = ZTBKPF
       TABLES
            IT_ZSBSEG    = IT_ZSBSEG
       CHANGING
            BELNR        = ZTBKPF-BELNR
       EXCEPTIONS
            ERROR_UPDATE = 4.

  W_SUBRC = SY-SUBRC.
  IF SY-SUBRC NE 0.
    MESSAGE S494.
    PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                USING  'E'.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

  REFRESH : RETURN.
  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST'
       EXPORTING
          BUKRS             =     ZTBKPF-BUKRS
          BELNR             =     ZTBKPF-BELNR
          GJAHR             =     ZTBKPF-GJAHR
       IMPORTING
          INVOICEDOCNUMBER  =     INVOICEDOCNUMBER
          FISCALYEAR        =     FISCALYEAR
       TABLES
          RETURN            =     RETURN
       EXCEPTIONS
          POST_ERROR        =     4.

  IF SY-SUBRC EQ 0.
    PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                  USING   'S'.
    SELECT SINGLE * FROM ZTINS
           WHERE    ZFREQNO  EQ  IT_SELECTED-ZFREQNO
           AND      ZFAMDNO  EQ  IT_SELECTED-INSAMDNO
           AND      ZFINSEQ  EQ  IT_SELECTED-ZFINSEQ.

    ZTINS-BELNR = ZTBKPF-BELNR.
    ZTINS-GJAHR = ZTBKPF-GJAHR.

    MOVE-CORRESPONDING ZTINS TO IT_ZTINS.
    APPEND IT_ZTINS.

    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.

    CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
         EXPORTING
              W_OK_CODE    = 'DELE'
              BUKRS        = ZTBKPF-BUKRS
              GJAHR        = ZTBKPF-GJAHR
              ZFSTATUS     = 'U'
              W_ZTBKPF_OLD = L_ZTBKPF
              W_ZTBKPF     = ZTBKPF
         TABLES
              IT_ZSBSEG    = IT_ZSBSEG
         CHANGING
              BELNR        = ZTBKPF-BELNR
         EXCEPTIONS
              ERROR_UPDATE = 4.

    IF RETURN[] IS INITIAL.
      MESSAGE S494.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                  USING  'E'.
    ELSE.
      PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_FI_POSTING
*&---------------------------------------------------------------------*
*&      Form  P2000_HEADER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_HEADER_DATA_MAKE.

  CLEAR : ZTBKPF, *ZTBKPF.

*> POSTING DATA MOVE.
  SELECT SINGLE * FROM  T001
                  WHERE BUKRS EQ IT_SELECTED-BUKRS.

  SELECT SINGLE * FROM  T005
                  WHERE LAND1 EQ T001-LAND1.

  SELECT SINGLE * FROM ZTIMIMG11
                  WHERE BUKRS EQ IT_SELECTED-BUKRS.

  CALL FUNCTION 'FI_VENDOR_DATA'
       EXPORTING
            I_BUKRS = IT_SELECTED-BUKRS
            I_LIFNR = IT_SELECTED-ZFOPCD
       IMPORTING
            E_KRED  = VF_KRED
       EXCEPTIONS
            OTHERS  = 4.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  ZTBKPF-ZTERM = VF_KRED-ZTERM.
  ZTBKPF-AKONT = VF_KRED-AKONT.
   *LFA1-NAME1  = VF_KRED-NAME1.

  MOVE: SY-MANDT            TO ZTBKPF-MANDT,
        'X'                 TO ZTBKPF-ZFPCUR,
        'Y'                 TO ZTBKPF-ZFPOYN,
        IT_SELECTED-BUKRS   TO ZTBKPF-BUKRS,
        SY-DATUM            TO ZTBKPF-BUDAT,
        SY-DATUM            TO ZTBKPF-BLDAT,
        SY-DATUM            TO ZTBKPF-ZFBDT,
        P_BLART             TO ZTBKPF-BLART,
        '003'               TO ZTBKPF-ZFCSTGRP,
        SY-DATUM+4(2)       TO ZTBKPF-MONAT,
        P_MWSKZ             TO ZTBKPF-MWSKZ,
        1                   TO ZTBKPF-KURSF,
        SY-DATUM            TO ZTBKPF-WWERT,
        ' '                 TO ZTBKPF-ZLSPR,
        T001-WAERS          TO ZSREQHD-ZFKRW,
        T001-WAERS          TO ZTBKPF-HWAER,
        ZSREQHD-ZFKRWAMT    TO ZTBKPF-WRBTR,
        ZSREQHD-ZFKRWAMT    TO *ZTBKPF-WRBTR,
        T001-WAERS          TO *ZTBKPF-WAERS,
        ZSREQHD-ZFKRWAMT    TO ZTBKPF-DMBTR,
        T001-WAERS          TO ZTBKPF-WAERS,
        'X'                 TO ZTBKPF-ZFAUTO,
        'X'                 TO ZTBKPF-ZFATPT,
        'N'                 TO ZTBKPF-ZFPOSYN,
        IT_SELECTED-ZFOPCD  TO ZTBKPF-LIFNR,
        IT_SELECTED-ZFOPCD  TO ZTBKPF-ZFVEN.
  CLEAR : ZTREQHD.
  SELECT SINGLE * FROM ZTREQHD
         WHERE  ZFREQNO  EQ  IT_SELECTED-ZFREQNO.

  WRITE ZTREQHD-ZFLASTAM TO ZTBKPF-BKTXT CURRENCY ZTREQHD-WAERS
                            LEFT-JUSTIFIED.
  CONCATENATE  ZTBKPF-BKTXT  ZTREQHD-WAERS  INTO  ZTBKPF-BKTXT
                             SEPARATED BY SPACE.

  IF ZSREQHD-ZFKRWAMT LT 0.
     ZTBKPF-DMBTR     = ZTBKPF-DMBTR * -1.
     ZTBKPF-WRBTR     = ZTBKPF-WRBTR * -1.
     *ZTBKPF-WRBTR    = *ZTBKPF-WRBTR * -1.
     ZSREQHD-ZFKRWAMT = ZSREQHD-ZFKRWAMT * -1.
     ZTBKPF-ZFRVSX    = 'X'.
  ENDIF.

ENDFORM.                    " P2000_HEADER_DATA
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0010 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF UF05A-STGRD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_POSTING_CANCLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_POSTING_CANCLE.

  REFRESH : IT_ERR_LIST.

  LOOP AT IT_SELECTED.

*>>> 진행상태바..
    LINE = ( SY-TABIX / W_SELECTED_LINES ) * 100.
    OUT_TEXT = 'JOB PROGRESS %99999%%'.
    REPLACE '%99999%' WITH LINE INTO OUT_TEXT.
    PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE.


    SELECT  SINGLE * FROM ZTBKPF
    WHERE   BUKRS    EQ   IT_SELECTED-BUKRS
    AND     GJAHR    EQ   IT_SELECTED-GJAHR
    AND     BELNR    EQ   IT_SELECTED-BELNR.

    IF ZTBKPF-ZFPOSYN EQ 'N'.
      MESSAGE  S577.
      PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                   USING   'E'.
      CONTINUE.
    ENDIF.

*>> AP POSTING FUNCTION CALL
    CALL FUNCTION 'ZIM_BAPI_COST_INVOICES_CANCEL'
         EXPORTING
              P_ZFIMDTY                 = 'CS'
              P_BUKRS                   = ZTBKPF-BUKRS
              INVOICEDOCNUMBER          = ZTBKPF-ZFACDO
              FISCALYEAR                = ZTBKPF-ZFFIYR
              REASONREVERSAL            = UF05A-STGRD
              POSTINGDATE               = BSIS-BUDAT
         IMPORTING
              INVOICEDOCNUMBER_REVERSAL = INVOICEDOCNUMBER
              FISCALYEAR_REVERSAL       = FISCALYEAR
         TABLES
              RETURN                    = RETURN
         EXCEPTIONS
              LIV_ERROR                 = 4.

    IF SY-SUBRC NE 0.           ">> 오류 발생시...
      IF RETURN[] IS INITIAL.
        MESSAGE  S691.
        PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'E'.
      ELSE.
        PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
      ENDIF.
*        ADD    1    TO    W_ERR_CNT.
    ELSE.
      PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST USING 'S'.

*        SELECT SINGLE * FROM ZTINS
*               WHERE    ZFREQNO  EQ  IT_SELECTED-ZFREQNO
*               AND      ZFAMDNO  EQ  IT_SELECTED-ZFAMDNO
*               AND      ZFINSEQ  EQ  IT_SELECTED-ZFINSEQ.
*
*        CLEAR : ZTINS-BELNR, ZTINS-GJAHR.
*
*        MOVE-CORRESPONDING ZTINS TO IT_ZTINS.
*        APPEND IT_ZTINS.

*        ADD 1       TO W_PROC_CNT.
    ENDIF.

  ENDLOOP.

  DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
  IF W_LINE GT 0.
    INCLUDE = 'POPU'.
    CALL SCREEN 0100 STARTING AT  05   3
                     ENDING   AT  100 12.
    CLEAR : INCLUDE.
  ENDIF.




*  DESCRIBE TABLE IT_ZTINS LINES W_LINE.
*  IF W_LINE GT 0.
*     MODIFY ZTINS FROM TABLE IT_ZTINS.
*     IF SY-SUBRC NE 0.
*        MESSAGE E000.
*     ENDIF.
*  ENDIF.

ENDFORM.                    " P2000_POSTING_CANCLE
*&---------------------------------------------------------------------*
*&      Form  P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_IMPORT_DOC_CHEKC USING    P_ZFIMDNO
                                     P_ZFDCNM
                                     P_ZFPOYN
                                     P_GUBUN
                                     P_KOSTL.

  CLEAR : P_ZFDCNM, ZTREQHD, ZTBL, ZTIV, ZTCGHD, ZTMSHD.

  IF P_ZFIMDNO IS INITIAL.
    EXIT.
  ENDIF.

  CASE ZTBKPF-ZFCSTGRP.
    WHEN '003'.           ">수입의뢰.
      SELECT SINGLE * FROM ZTREQHD
                      WHERE ZFREQNO EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      MOVE: ZTREQHD-ZFOPNNO TO P_ZFDCNM.
      P_ZFPOYN = 'Y'.
* CORECESS 주석처리.
*         IF W_SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTREQHD-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.
    WHEN '004' OR '005'.  ">B/L 관리번호.
      SELECT SINGLE * FROM ZTBL
                      WHERE ZFBLNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      IF W_SUBRC EQ 0.
        P_ZFPOYN = ZTBL-ZFPOYN.
        P_KOSTL  = ZTBL-KOSTL.
      ELSE.
        P_ZFPOYN = 'Y'.
      ENDIF.
*> 1. 화물관리번호.
      IF NOT ZTBL-ZFGMNO IS INITIAL.
        MOVE: ZTBL-ZFGMNO TO P_ZFDCNM.
        IF NOT ZTBL-ZFMSN IS INITIAL.
          CONCATENATE P_ZFDCNM '-' ZTBL-ZFMSN INTO P_ZFDCNM.
        ENDIF.
        IF NOT ZTBL-ZFHSN IS INITIAL.
          CONCATENATE P_ZFDCNM '-' ZTBL-ZFHSN INTO P_ZFDCNM.
        ENDIF.
      ELSE.
*> 2. HOUSE B/L No.
        IF NOT ZTBL-ZFHBLNO IS INITIAL.
          MOVE: ZTBL-ZFHBLNO TO P_ZFDCNM.
        ELSE.
*> 3. MASTER B/L No.
          IF NOT ZTBL-ZFMBLNO IS INITIAL.
            MOVE: ZTBL-ZFMBLNO TO P_ZFDCNM.
          ELSE.
*> 4. 선사 B/L No.
            IF NOT ZTBL-ZFCGHNO IS INITIAL.
              MOVE: ZTBL-ZFCGHNO TO P_ZFDCNM.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
* CORECESS 주석처리.
*         IF W_SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTBL-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.
*
    WHEN '006'.           ">통관관리번호.
      SELECT SINGLE * FROM ZTIV
                      WHERE ZFIVNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.

      IF W_SUBRC EQ 0.
        P_ZFPOYN = ZTIV-ZFPOYN.
      ELSE.
        P_ZFPOYN = 'Y'.
      ENDIF.

      IF W_SUBRC EQ 0.
        IF ZTIV-ZFCUST EQ '3' OR ZTIV-ZFCUST EQ 'Y'.
*         SELECT SINGLE * FROM ZTCUCLIV
*                        WHERE ZFIVNO EQ P_ZFIMDNO.
*       IF SY-SUBRC EQ 0.
          SELECT SINGLE * FROM ZTIDR
                          WHERE ZFIVNO EQ P_ZFIMDNO.
*                         WHERE ZFBLNO  EQ ZTCUCLIV-ZFBLNO
*                           AND   ZFCLSEQ EQ ZTCUCLIV-ZFCLSEQ.
          IF SY-SUBRC EQ 0.
            IF ZTIDR-ZFIDRNO IS INITIAL.
              MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
            ELSE.
              MOVE: ZTIDR-ZFIDRNO TO P_ZFDCNM.
            ENDIF.
          ELSE.
            MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
          ENDIF.
*       ELSE.
*          MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
*       ENDIF.
        ELSE.
          MOVE: ZTIV-ZFIVNO TO P_ZFDCNM.
        ENDIF.
      ENDIF.
      SELECT SINGLE * FROM ZTBL
             WHERE ZFBLNO EQ ZTIV-ZFBLNO.

* CORECESS 주석처리.
*         IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*            P_KOSTL  = ZTBL-KOSTL.
*            P_ZFPOYN = ZTBL-ZFPOYN.
*
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTBL-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.

    WHEN '007'.           ">하역관리번호.
      SELECT SINGLE * FROM ZTCGHD
                      WHERE ZFCGNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      P_ZFPOYN = 'Y'.
      IF SY-SUBRC EQ 0.
        IF NOT ZTCGHD-ZFMSNO IS INITIAL.
          SELECT SINGLE * FROM  ZTMSHD
                          WHERE ZFMSNO  EQ  ZTCGHD-ZFMSNO.
          IF SY-SUBRC EQ 0.
            MOVE ZTMSHD-ZFMSNM  TO  P_ZFDCNM.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '008'.           ">기납증리번호.
      SELECT SINGLE * FROM ZTTAXBKHD
                      WHERE ZFTBNO  EQ P_ZFIMDNO.
      W_SUBRC = SY-SUBRC.
      IF SY-SUBRC EQ 0.
        IF ZTTAXBKHD-BASISNO IS INITIAL.
          MOVE ZTTAXBKHD-EBELN    TO  P_ZFDCNM.
        ELSE.
          MOVE ZTTAXBKHD-BASISNO  TO  P_ZFDCNM.
        ENDIF.
      ELSE.
        CLEAR : P_ZFDCNM.
      ENDIF.
      SELECT SINGLE * FROM ZTREQHD
             WHERE ZFREQNO EQ ZTTAXBKHD-ZFREQNO.
*          IF SY-SUBRC EQ 0.
* CORECESS 주석처리.
*         IF SY-SUBRC EQ 0 AND SY-TCODE NE 'ZIMY3'.
*
*            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
*                 EXPORTING
*                    UNAME   =    SY-UNAME
*                    WERKS   =    ZTREQHD-ZFWERKS
*                 IMPORTING
*                    GSBER   =    ZTBKPF-GSBER
*                    BUPLA   =    ZTBKPF-BUPLA.
*         ENDIF.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

*>>오류가 발생했을 경우.
  IF SY-SUBRC NE 0.
*      IF P_GUBUN EQ 'H'.
*         PERFORM P2000_NO_INPUT USING 'ZTBKPF' 'ZFIMDNO'.
*      ELSE.
*         PERFORM P2000_NO_INPUT USING 'ZSBSEG' 'ZFIMDNO'.
*      ENDIF.

    CASE ZTBKPF-ZFCSTGRP.
      WHEN '003'.
        MESSAGE E585 WITH '수입의뢰번호' P_ZFIMDNO.
      WHEN '004' OR '005'.
        MESSAGE E585 WITH 'B/L 관리번호' P_ZFIMDNO.
      WHEN '006'.
        MESSAGE E585 WITH '통관관리번호' P_ZFIMDNO.
      WHEN '007'.
        MESSAGE E585 WITH '하역관리번호' P_ZFIMDNO.
      WHEN '008'.
        MESSAGE E585 WITH '기납증관리번호' P_ZFIMDNO.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_NEWKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSBSEG_NEWKO  text
*      -->P_IT_ZSBSEG_ZFCD  text
*      -->P_IT_ZSBSEG_ZFIMDNO  text
*----------------------------------------------------------------------*
FORM P2000_SET_NEWKO   USING ZSBSEG-NEWKO
                             ZSBSEG-ZFCD
                             ZSBSEG-ZFIMDNO.
*> 계정결정 함수.
  CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
       EXPORTING
            ZFCSTGRP = '003'
            ZFCD     = ZSBSEG-ZFCD
            ZFIMDNO  = ZSBSEG-ZFIMDNO
       IMPORTING
            NEWKO    = ZSBSEG-NEWKO.

ENDFORM.                    " P2000_SET_NEWKO
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

*>> Company code SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.


ENDFORM.                    " P1000_SET_BUKRS

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE_EN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE_EN.

  IF W_PAGE EQ 1.

    SKIP 2.
    FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
    WRITE : /51  '[ Blanket posting of premium ]'
                 COLOR COL_HEADING INTENSIFIED OFF.
    SKIP 2.
  ENDIF.
  WRITE : / 'Insurance company:', IT_TAB-ZFOPCD, IT_TAB-ZFINSU1.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, '',                     SY-VLINE,
          (10) 'P/O Doc',
          (01) '-'                            ,
          (05) 'Count',
          (01) '-',
          (05) 'Amend'                NO-GAP, SY-VLINE,
          (20) 'Insuring amount',             SY-VLINE,
          (30) 'Insuring condition',          SY-VLINE,
          (12) 'Insuring date',               SY-VLINE,
          (20) 'Premium(foreign)',            SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE : / SY-VLINE, '',                     SY-VLINE,
           (25) 'Policy No',                  SY-VLINE,
           (20) 'Shipping port',              SY-VLINE,
           (30) 'Item name',                  SY-VLINE,
           (12) 'Issuing date',               SY-VLINE,
           (20) 'Premium(Local)',             SY-VLINE,
          / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE_EN
