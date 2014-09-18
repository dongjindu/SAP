*&---------------------------------------------------------------------*
*& Report  ZRIMREQDISP                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 Additional List                              *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.03.06                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMREQDISP  MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 수입의뢰 리스트용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK       TYPE C,                        " MARK
       W_GB01(1)  TYPE C VALUE ';',
       UPDATE_CHK TYPE C,                        " DB 반영 여부...
       W_GB02(1)  TYPE C VALUE ';',
       ZFREQDT    LIKE ZTREQST-ZFREQDT,          " 요개설일?
       W_GB03(1)  TYPE C VALUE ';',
       ZFAPPDT    LIKE ZTREQST-ZFAPPDT,          " 개설예정(신청)
*      ZFMAUD     LIKE ZTREQHD-ZFMAUD,           " 자재납기?
       W_GB04(1)  TYPE C VALUE ';',
       EBELN      LIKE ZTREQHD-EBELN,            " Purchasing document
       W_GB05(1)  TYPE C VALUE ';',
       ZFREQNO    LIKE ZTREQHD-ZFREQNO,          " 수입의뢰 번?
       W_GB06(1)  TYPE C VALUE ';',
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,          " Amend Seq.
       W_GB07(1)  TYPE C VALUE ';',
       ZFOPAMT1(18) TYPE C,                      " 개설금액 TEXT
       W_GB08(1)  TYPE C VALUE ';',
       WAERS      LIKE ZTREQST-WAERS,            " Currency
       W_GB09(1)  TYPE C VALUE ';',
       ZFUSDAM1(18) TYPE C,                      " USD 환산금액 TEXT
       W_GB10(1)  TYPE C VALUE ';',
       ZFUSD      LIKE ZTREQST-ZFUSD,            " USD Currency
       W_GB11(1)  TYPE C VALUE ';',
       ZFREQTY    LIKE ZTREQST-ZFREQTY,          " 결제구?
       W_GB12(1)  TYPE C VALUE ';',
       ZFMATGB    LIKE ZTREQHD-ZFMATGB,          " 자재구?
       W_GB99(1)  TYPE C VALUE ';',
       ZFBACD     LIKE ZTREQHD-ZFBACD,           " 사전/사후 구?
       W_GB13(1)  TYPE C VALUE ';',
       EKORG      LIKE ZTREQST-EKORG,            " Purchasing organizati
       W_GB14(1)  TYPE C VALUE ';',
       EKGRP      LIKE ZTREQST-EKGRP,            " Purchasing group
       W_GB15(1)  TYPE C VALUE ';',
*      ZFSPRT     LIKE ZTREQHD-ZFSPRT,           " 선적?
       ZFSPRT(18) TYPE C,                        " 선적?
       W_GB16(1)  TYPE C VALUE ';',
*      ZFAPRT     LIKE ZTREQHD-ZFAPRT,           " 도착?
       ZFAPRT(18) TYPE C,                        " 도착?
       W_GB17(1)  TYPE C VALUE ';',
       INCO1      LIKE ZTREQHD-INCO1,            " Incoterms
       W_GB18(1)  TYPE C VALUE ';',
       ZFTRANS    LIKE ZTREQHD-ZFTRANS,          " VIA
       W_GB19(1)  TYPE C VALUE ';',
       ZFWERKS    LIKE ZTREQHD-ZFWERKS,          " 대표 Plant
       W_GB20(1)  TYPE C VALUE ';',
       ERNAM      LIKE ZTREQST-ERNAM,            " 구매담?
       W_GB21(1)  TYPE C VALUE ';',
       LIFNR      LIKE ZTREQHD-LIFNR,            " Vendor Code
       W_GB22(1)  TYPE C VALUE ';',
*      NAME1      LIKE LFA1-NAME1,               " Name 1
       NAME1(20)  TYPE C,                        " Name 1
       W_GB23(1)  TYPE C VALUE ';',
       ZFBENI     LIKE ZTREQHD-ZFBENI,           " Beneficairy
       W_GB24(1)  TYPE C VALUE ';',
       BUKRS      LIKE ZTREQHD-BUKRS,
*      NAME2      LIKE LFA1-NAME1,               " Name 1
       NAME2(20)  TYPE C,                        " Name 1
       W_GB25(1)  TYPE C VALUE ';',
       ZFRLST1    LIKE ZTREQST-ZFRLST1,          " 의뢰 Release 상?
       W_GB26(1)  TYPE C VALUE ';',
       ZFRLDT1    LIKE ZTREQST-ZFRLDT1,          " 의뢰 Release 일?
       W_GB27(1)  TYPE C VALUE ';',
       ZFRLNM1    LIKE ZTREQST-ZFRLNM1,          " 의뢰 Release 담당?
       W_GB28(1)  TYPE C VALUE ';',
       ZFCLOSE    LIKE ZTREQHD-ZFCLOSE,          " 수입의뢰 종료여?
       W_GB29(1)  TYPE C VALUE ';',
       ZFRLST2    LIKE ZTREQST-ZFRLST2,          " 개설 Release 상?
       W_GB50(1)  TYPE C VALUE ';',
       ZFOPAMT    LIKE ZTREQST-ZFOPAMT,         " 개설금?
       W_GB51(1)  TYPE C VALUE ';',
       ZFUSDAM    LIKE ZTREQST-ZFUSDAM,          " USD 환산금?
       W_GB30(1)  TYPE C VALUE ';'.
DATA : END OF IT_TAB.
* PF-STATUS 용 INTERNAL TABLE.
DATA: BEGIN OF IT_EXCL OCCURS 20,
      FCODE    LIKE RSMPE-FUNC.
DATA: END   OF IT_EXCL.
* EDI Send 용 변수.
DATA : W_OK_CODE    LIKE   SY-UCOMM,
       W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO,
       W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
       W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF,
       W_EDI_RECORD(65535).
* EDI INTERNAL TABLE.
DATA: BEGIN OF IT_EDIFILE OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
      END OF IT_EDIFILE.


*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMPRELTOP.    " 구매 Released  Report Data Define용 Include

INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?


*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS,    " 회사코드.
                   S_REQDT   FOR ZTREQST-ZFREQDT,  " 요개설일?
                   S_MATGB   FOR ZTREQHD-ZFMATGB,  " 자재구?
                   S_REQTY   FOR ZTREQHD-ZFREQTY,  " 수입의뢰 Type
                   S_WERKS   FOR ZTREQHD-ZFWERKS,  " 대표 plant
                   S_EKORG   FOR ZTREQST-EKORG.    " Purch. Org.
   SELECT-OPTIONS: S_EBELN   FOR ZTREQHD-EBELN,    " P/O Number
                   S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                   S_ZFBENI  FOR ZTREQHD-ZFBENI,   " Beneficiary
                   S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
                   S_REQNO   FOR ZTREQHD-ZFREQNO.  " 수입의뢰 관리번?
   PARAMETERS :    P_NAME    LIKE USR02-BNAME.      " 담당?
   SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
   PARAMETERS : P_OPEN     AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
* L/C 릴리즈 상태 SELECT 조건 PARAMETER
*-----------------------------------------------------------------------
SELECT-OPTIONS : S_STATUS FOR ZTREQST-ZFRLST1 NO INTERVALS NO-DISPLAY.
SELECT-OPTIONS : S_STATU2 FOR ZTREQST-ZFRLST2 NO INTERVALS NO-DISPLAY.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 파라메타 설?
   PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 구매의뢰 테이블 SELECT
   PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 Text Table SELECT
   PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.


*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
         W_FIELD_NM = 'ZFREQDT'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'DISP'.          " L/C 조건.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P2000_SHOW_LC USING IT_SELECTED-ZFREQNO.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
* 구매의뢰 테이블 SELECT
           PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
           IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
           PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
           IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
           PERFORM RESET_LIST.
      WHEN 'HOPEN'.
           IF NOT ( IT_TAB-ZFREQNO IS INITIAL
                AND IT_TAB-ZFAMDNO IS INITIAL ).
              SET PARAMETER ID 'BES'       FIELD ''.
              SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
              SET PARAMETER ID 'ZPREQNO'   FIELD IT_TAB-ZFREQNO.

              CALL TRANSACTION 'ZIM07' AND SKIP FIRST SCREEN.

              PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
              IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
              PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
              IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
              PERFORM RESET_LIST.
           ELSE.
              MESSAGE E962.
           ENDIF.

      WHEN 'ESEND'.           " EDI FILE CREATE.
           IF NOT ( IT_TAB-ZFREQNO IS INITIAL
                AND IT_TAB-ZFAMDNO IS INITIAL ).

               W_OK_CODE = SY-UCOMM.

               PERFORM P2000_POPUP_MESSAGE.     " 메세지 박스.
               IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경우.
                  PERFORM P3000_DATA_UPDATE USING W_OK_CODE.
*                  LEAVE TO SCREEN 0.
               ENDIF.
* REFRESH.
              PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
              IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
              PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
              IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
              PERFORM RESET_LIST.
           ELSE.
              MESSAGE E962.
           ENDIF.
      WHEN 'ADDIN'.
           IF NOT ( IT_TAB-ZFREQNO IS INITIAL
                AND IT_TAB-ZFAMDNO IS INITIAL ).
              SET PARAMETER ID 'BES'       FIELD ''.
              SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
              SET PARAMETER ID 'ZPREQNO'   FIELD IT_TAB-ZFREQNO.

              CALL TRANSACTION 'ZIM05' AND SKIP FIRST SCREEN.

              PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
              IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
              PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
              IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
              PERFORM RESET_LIST.
           ELSE.
              MESSAGE E962.
           ENDIF.
      WHEN OTHERS.
   ENDCASE.
   CLEAR IT_TAB.
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_DATA_UPDATE   USING   W_GUBUN.
DATA : L_REQTY   LIKE   ZTREQHD-ZFREQTY,
       L_RETURN  LIKE   SY-SUBRC,
       O_ZTREQST LIKE   ZTREQST,  "취소시 이전 정보저장용.
       L_COUNT   TYPE   I.

   REFRESH : IT_EDIFILE.
   CLEAR : L_REQTY, IT_EDIFILE, L_COUNT.

      W_TABIX = SY-TABIX.

*>>> 수입의뢰 헤더, 상태 테이블 조회...
      SELECT SINGLE * FROM ZTREQHD
                      WHERE ZFREQNO EQ IT_TAB-ZFREQNO.

      SELECT SINGLE * FROM ZTREQST
                      WHERE ZFREQNO EQ IT_TAB-ZFREQNO
                      AND   ZFAMDNO EQ IT_TAB-ZFAMDNO.

         IF ZTREQST-ZFEDICK NE 'O'.
            MESSAGE E119 WITH IT_TAB-ZFREQNO IT_TAB-ZFAMDNO.
            EXIT.
         ENDIF.
         IF ZTREQST-ZFDOCST NE 'N'.
            MESSAGE E104 WITH IT_TAB-ZFREQNO
                              IT_TAB-ZFAMDNO ZTREQST-ZFDOCST.
            EXIT.
         ENDIF.

*>> 변경이력..
      O_ZTREQST = ZTREQST.
*>>  개설은행 조회.
      SELECT SINGLE * FROM LFA1
                      WHERE LIFNR   EQ ZTREQHD-ZFOPBN.
*>>>  EDI 식별자 조회.
      IF LFA1-BAHNS IS INITIAL.
         MESSAGE E274 WITH ZTREQHD-ZFOPBN.
      ENDIF.

* LOCK CHECK  "다른 사람이 수정하지 못하도록 잠금.
      PERFORM   P2000_LOCK_MODE_SET  USING    'L'
                                              IT_TAB-ZFREQNO
                                              IT_TAB-ZFAMDNO
                                              L_RETURN.
      CHECK L_RETURN EQ 0.

*>>> EDI용 FIELD CREATE.
         PERFORM   P3000_FILE_CREATE.
*>>> READY KOREA LTD. SAM-FILE WRITE FUNCTION
         CALL  FUNCTION 'ZIM_EDI_SAMFILE_WRITE'
               EXPORTING
                      ZFCDDOC       = W_ZFCDDOC
                      BUKRS         = ZTREQHD-BUKRS
               TABLES
                      EDIFILE       = IT_EDIFILE.
         REFRESH : IT_EDIFILE.

     ADD 1   TO    L_COUNT.      "---> 마지막을 알기

* 상태 변경

     MOVE : SY-UNAME    TO    ZTREQST-UNAM,
            SY-DATUM    TO    ZTREQST-UDAT,
            W_ZFDHENO   TO    ZTREQST-ZFDOCNO,
            'R'         TO    ZTREQST-ZFDOCST,
            'S'         TO    ZTREQST-ZFEDIST.
*>>> 변경...
     UPDATE ZTREQST.

* CHANGE DOCUMENT
     CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_STATUS'
     EXPORTING
        W_ZFREQNO      =     ZTREQST-ZFREQNO
        W_ZFAMDNO      =     ZTREQST-ZFAMDNO
        N_ZTREQST      =     ZTREQST
        O_ZTREQST      =     O_ZTREQST.

*>>> UNLOCK SETTTING.
     PERFORM   P2000_LOCK_MODE_SET  USING    'U'
                                              IT_TAB-ZFREQNO
                                              IT_TAB-ZFAMDNO
                                              L_RETURN.

     L_REQTY = ZTREQHD-ZFREQTY.

ENDFORM.                    " P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P3000_FILE_CREATE
*&---------------------------------------------------------------------*
FORM P3000_FILE_CREATE.
*>> DOCUMENT TYPE DETERMINE
      SELECT SINGLE * FROM ZTIMIMGTX
                      WHERE BUKRS = ZTREQHD-BUKRS.

      CASE ZTREQHD-ZFREQTY.
         WHEN 'LC'.
            IF ZTREQST-ZFAMDNO = '00000'.
               W_ZFCDDOC = 'APP700'.
            ELSEIF ZTREQST-ZFAMDNO GT '00000'.
               W_ZFCDDOC = 'APP707'.
            ENDIF.
         WHEN 'LO'.
            IF ZTREQST-ZFAMDNO = '00000'.
               W_ZFCDDOC = 'LOCAPP'.
            ELSEIF ZTREQST-ZFAMDNO GT '00000'.
               W_ZFCDDOC = 'LOCAMR'.
            ENDIF.
         WHEN 'TT'.
            W_ZFCDDOC = 'PAYORD'.
         WHEN OTHERS.  EXIT.
      ENDCASE.

*>>> FIELD MOVE
      W_ZFDHSRO = LFA1-BAHNS.          " 식별자.
      W_ZFDHREF = ZTREQHD-ZFREQNO.     " 참조번호.
      MOVE '-'             TO W_ZFDHREF+10(1).
      MOVE ZTREQST-ZFAMDNO TO W_ZFDHREF+11(5).
*      W_ZFDHDDB = ZTREQST-EKORG.       " 부서.
      W_ZFDHENO = ZTREQST-ZFDOCNO.     " 문서번호.

*>>> EDI 관리번호 SETTING
      CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
          EXPORTING
               W_ZFCDDOC = W_ZFCDDOC
               W_ZFDHSRO = W_ZFDHSRO
               W_ZFDHREF = W_ZFDHREF
*               W_ZFDHDDB = W_ZFDHDDB
               W_BUKRS   = ZTREQHD-BUKRS
*               W_ZFEDIID = SPACE
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
     CASE ZTREQHD-ZFREQTY.
        WHEN 'LC'.
          IF ZTREQST-ZFAMDNO = '00000' AND ZTIMIMGTX-APP700 = 'X'.
             CALL  FUNCTION 'ZIM_LG_APP700_EDI_DOC'
                   EXPORTING
                      W_ZFREQNO     = ZTREQHD-ZFREQNO
                      W_ZFDHENO     = W_ZFDHENO
                      W_BAHNS       = W_LFA1-BAHNS
                   IMPORTING
                      W_EDI_RECORD  = W_EDI_RECORD
                   EXCEPTIONS
                      CREATE_ERROR      = 4.

          ELSEIF ZTREQST-ZFAMDNO GT '00000' AND ZTIMIMGTX-APP707 = 'X'.
             CALL  FUNCTION 'ZIM_LG_APP707_EDI_DOC'
                   EXPORTING
                      W_ZFREQNO     = ZTREQHD-ZFREQNO
                      W_ZFAMDNO     = ZTREQST-ZFAMDNO
                      W_ZFDHENO     = W_ZFDHENO
                      W_BAHNS       = W_LFA1-BAHNS
                   IMPORTING
                      W_EDI_RECORD  = W_EDI_RECORD
                   EXCEPTIONS
                      CREATE_ERROR      = 4.
          ELSE.
             EXIT.
          ENDIF.
        WHEN 'LO'.
          IF ZTREQST-ZFAMDNO = '00000' AND ZTIMIMGTX-LOCAPP = 'X'.
             CALL  FUNCTION 'ZIM_LG_LOCAPP_EDI_DOC'
                   EXPORTING
                        W_ZFREQNO     = ZTREQHD-ZFREQNO
                        W_ZFDHENO     = W_ZFDHENO
                        W_BAHNS       = W_LFA1-BAHNS
                   IMPORTING
                        W_EDI_RECORD  = W_EDI_RECORD
                   EXCEPTIONS
                        CREATE_ERROR      = 4.
          ELSEIF ZTREQST-ZFAMDNO GT '00000' AND ZTIMIMGTX-LOCAMR = 'X'.
             CALL  FUNCTION 'ZIM_LG_LOCAMR_EDI_DOC'
                   EXPORTING
                        W_ZFREQNO     = ZTREQHD-ZFREQNO
                        W_ZFAMDNO     = ZTREQST-ZFAMDNO
                        W_ZFDHENO     = W_ZFDHENO
                        W_BAHNS       = W_LFA1-BAHNS
                   IMPORTING
                        W_EDI_RECORD  = W_EDI_RECORD
                   EXCEPTIONS
                        CREATE_ERROR      = 4.
          ELSE.
             EXIT.
          ENDIF.

        WHEN 'TT'.
           CALL  FUNCTION 'ZIM_LG_PAYORD_EDI_DOC'
                 EXPORTING
                      W_ZFREQNO     = ZTREQHD-ZFREQNO
                      W_ZFDHENO     = W_ZFDHENO
                      W_BAHNS       = W_LFA1-BAHNS
                 IMPORTING
                      W_EDI_RECORD  = W_EDI_RECORD
                 EXCEPTIONS
                      CREATE_ERROR      = 4.
        WHEN OTHERS.
             EXIT.
     ENDCASE.

     CASE SY-SUBRC.
        WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
        WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
     ENDCASE.

*>>> INTERNAL TABLE WRITE....
     IT_EDIFILE-W_RECORD = W_EDI_RECORD.
     APPEND IT_EDIFILE.

ENDFORM.                    " P3000_FILE_CREATE

*&---------------------------------------------------------------------*
*&      Form  P2000_LOCK_MODE_SET
*&---------------------------------------------------------------------*
FORM P2000_LOCK_MODE_SET USING    VALUE(P_MODE)
                                  VALUE(P_REQNO)
                                  VALUE(P_AMDNO)
                                  P_RETURN.
* LOCK CHECK
   IF P_MODE EQ 'L'.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
           EXPORTING
                ZFREQNO                =     P_REQNO
                ZFAMDNO                =     P_AMDNO
           EXCEPTIONS
                OTHERS        = 1.

      MOVE SY-SUBRC     TO     P_RETURN.
      IF SY-SUBRC NE 0.
         MESSAGE I510 WITH SY-MSGV1 'Import Document' P_REQNO P_AMDNO
                      RAISING DOCUMENT_LOCKED.
      ENDIF.
   ELSEIF P_MODE EQ 'U'.
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
           EXPORTING
             ZFREQNO                =     P_REQNO
             ZFAMDNO                =     P_AMDNO.
   ENDIF.
ENDFORM.                    " P2000_LOCK_MODE_SET
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.
DATA : TEXT100(100) TYPE  C.

   TEXT100 = 'EDI FILE CREATE 작업을 계속 진행하시겠습니까?'.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = 'EDI FILE CREATE'
             DIAGNOSE_OBJECT = ''
             TEXT_QUESTION   = TEXT100
             TEXT_BUTTON_1   = '확    인'
             TEXT_BUTTON_2   = '아 니 오'
             DEFAULT_BUTTON  = '1'
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.   MESSAGE S961.
     LEAVE TO SCREEN 0.
  ENDIF.

  SET  TITLEBAR 'ZIM09'.          " TITLE BAR
  IF ZTIMIMG00-ZFBKYN EQ 'X'.
     P_OPEN = 'X'.
  ELSE.
     CLEAR : P_OPEN.                 " 기접수분 포?
  ENDIF.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /55  '[ 수입의뢰 요청 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 101 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, '요개설일'    ,  SY-VLINE NO-GAP,
            'P/O Number'    NO-GAP,  SY-VLINE NO-GAP,
            'CUR. '         NO-GAP,  SY-VLINE NO-GAP,
 '    개설 금액     '       NO-GAP,  SY-VLINE NO-GAP,
            'Ty'            NO-GAP,  SY-VLINE NO-GAP,
            'Mat'           NO-GAP,  SY-VLINE NO-GAP,
            '  구매담당  '  NO-GAP,  SY-VLINE NO-GAP,
        '     선적지     '  NO-GAP,  SY-VLINE NO-GAP,
            'Inc'           NO-GAP,  SY-VLINE NO-GAP,
            'Vendor    '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              120 SY-VLINE NO-GAP.

*            'Release Date'   NO-GAP,  SY-VLINE NO-GAP,
*            'Release 담당자' NO-GAP,  SY-VLINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, '개설신청'    ,  SY-VLINE NO-GAP,
            '수입의뢰No'    NO-GAP,  SY-VLINE NO-GAP,
            '     '         NO-GAP,  SY-VLINE NO-GAP,
 '   USD 환산금액   '       NO-GAP,  SY-VLINE NO-GAP,
            'TT'            NO-GAP,  SY-VLINE NO-GAP,
            'PGp'           NO-GAP,  SY-VLINE NO-GAP,
            '   Plant    '  NO-GAP,  SY-VLINE NO-GAP,
        '     도착지     '  NO-GAP,  SY-VLINE NO-GAP,
            'VIA'           NO-GAP,  SY-VLINE NO-GAP,
            'Bene.     '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              120 SY-VLINE NO-GAP.

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

   W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
*  AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
*          ID 'ACTVT' FIELD '*'.
*
*  IF SY-SUBRC NE 0.
*     MESSAGE S960 WITH SY-UNAME '의뢰 Release 트랜잭션'.
*     W_ERR_CHK = 'Y'.   EXIT.
*  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.
*
  W_ERR_CHK = 'N'.
* 구매 릴리즈 사용 여?
   IF ZTIMIMG00-ZFRELYN1 EQ 'X'.
      MOVE: 'I'      TO S_STATUS-SIGN,
            'EQ'     TO S_STATUS-OPTION,
            'R'      TO S_STATUS-LOW.
      APPEND S_STATUS.
   ELSE.
     MOVE: 'I'      TO S_STATUS-SIGN,
           'EQ'     TO S_STATUS-OPTION,
           'N'      TO S_STATUS-LOW.
     APPEND S_STATUS.
*
*     MOVE: 'I'      TO S_STATUS-SIGN,
*           'EQ'     TO S_STATUS-OPTION,
*           'N'      TO S_STATUS-LOW.
*     APPEND S_STATUS.
  ENDIF.

* 개설 릴리즈 사용 여?
  IF  ZTIMIMG00-ZFRELYN2 EQ 'X'.
     MOVE: 'I'      TO S_STATU2-SIGN,
           'EQ'     TO S_STATU2-OPTION,
           'C'      TO S_STATU2-LOW.
     APPEND S_STATU2.
     MOVE: 'I'      TO S_STATU2-SIGN,
           'EQ'     TO S_STATU2-OPTION,
           'N'      TO S_STATU2-LOW.
     APPEND S_STATU2.
  ELSE.
     MOVE: 'I'      TO S_STATU2-SIGN,
           'EQ'     TO S_STATU2-OPTION,
           'N'      TO S_STATU2-LOW.
     APPEND S_STATU2.
  ENDIF.

  IF P_NAME IS INITIAL.       P_NAME  =  '%'.      ENDIF.

ENDFORM.                    " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
FORM P1000_GET_ZVREQHD_ST   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  SELECT * INTO TABLE IT_ZVREQ FROM ZVREQHD_ST
                               WHERE ZFREQDT    IN     S_REQDT
                               AND   ZFMATGB    IN     S_MATGB
                               AND   ZFREQTY    IN     S_REQTY
                               AND   ZFWERKS    IN     S_WERKS
                               AND   EKORG      IN     S_EKORG
                               AND   ERNAM      LIKE   P_NAME
                               AND   ZFRLST1    IN     S_STATUS
                               AND   ZFRLST2    IN     S_STATU2
                               AND   EBELN      IN     S_EBELN
                               AND   LIFNR      IN     S_LIFNR
                               AND   ZFBENI     IN     S_ZFBENI
                               AND   EKGRP      IN     S_EKGRP
                               AND   ZFREQNO    IN     S_REQNO
                               AND   ZFDOCST    EQ     'N'
                               AND   ZFAMDNO    EQ     '00000'
                               AND   ZFCLOSE    EQ     SPACE.
*                              AND   ZFRTNYN    NE     SPACE
*                              AND   LOEKZ      EQ     SPACE.

  IF SY-SUBRC NE 0.               " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S009.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.
   REFRESH : IT_TAB.

   LOOP AT IT_ZVREQ.
      W_TABIX = SY-TABIX.

      IF  P_OPEN NE 'X'.
          IF IT_ZVREQ-ZFRVDT > '00000000'.
*             OR NOT IT_ZVREQ-ZFRVDT IS INITIAL.
             CONTINUE.
          ENDIF.
      ENDIF.

      MOVE-CORRESPONDING IT_ZVREQ  TO  IT_TAB.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
      CALL FUNCTION 'READ_LFA1'
           EXPORTING
                 XLIFNR          = IT_TAB-LIFNR
           IMPORTING
                 XLFA1           = LFA1
           EXCEPTIONS
                 KEY_INCOMPLETE  = 01
                 NOT_AUTHORIZED  = 02
                 NOT_FOUND       = 03.

      CASE SY-SUBRC.
         WHEN 01.     MESSAGE E022.
         WHEN 02.     MESSAGE E950.
         WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
      ENDCASE.
      MOVE: LFA1-NAME1   TO   IT_TAB-NAME1.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
      CALL FUNCTION 'READ_LFA1'
           EXPORTING
                 XLIFNR          = IT_TAB-ZFBENI
           IMPORTING
                 XLFA1           = LFA1
           EXCEPTIONS
                 KEY_INCOMPLETE  = 01
                 NOT_AUTHORIZED  = 02
                 NOT_FOUND       = 03.

      CASE SY-SUBRC.
         WHEN 01.     MESSAGE E022.
         WHEN 02.     MESSAGE E950.
         WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
      ENDCASE.
      MOVE: LFA1-NAME1   TO   IT_TAB-NAME2.

      APPEND  IT_TAB.
   ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  IF ZTIMIMG00-ZFRELYN2 EQ 'X'.
     MOVE 'HOPEN' TO IT_EXCL-FCODE. APPEND IT_EXCL.
  ENDIF.

   SET PF-STATUS 'ZIM09' EXCLUDING IT_EXCL.    " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM09'.                      " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P2000_PAGE_CHECK.
      PERFORM P3000_LINE_WRITE.

      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.

   ENDLOOP.
   CLEAR IT_TAB.
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
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFREQNO LIKE ZTREQST-ZFREQNO,
        ZFAMDNO LIKE ZTREQST-ZFAMDNO,
        ZFRLST1 LIKE ZTREQST-ZFRLST1,
        ZFRLST2 LIKE ZTREQST-ZFRLST2.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFREQNO  TO ZFREQNO,
         IT_TAB-ZFAMDNO  TO ZFAMDNO,
         IT_TAB-ZFRLST1  TO ZFRLST1,
         IT_TAB-ZFRLST2  TO ZFRLST2.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFREQNO  TO IT_SELECTED-ZFREQNO,
             IT_TAB-ZFAMDNO  TO IT_SELECTED-ZFAMDNO,
             IT_TAB-ZFRLST1  TO IT_SELECTED-ZFRLST1,
             IT_TAB-ZFRLST2  TO IT_SELECTED-ZFRLST2.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF NOT ZFREQNO IS INITIAL.
      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO,
             ZFAMDNO TO IT_SELECTED-ZFAMDNO,
             ZFRLST1 TO IT_SELECTED-ZFRLST1,
             ZFRLST2 TO IT_SELECTED-ZFRLST2.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ELSE.
      MESSAGE S962.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION
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

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / '총', W_COUNT, '건'.
*    WRITE : / SY-ULINE.    WRITE : / '총', W_COUNT, '건'.
  ENDIF.


ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE  NO-GAP,
       IT_TAB-ZFREQDT NO-GAP,               " 요개설?
       SY-VLINE NO-GAP,
       IT_TAB-EBELN   NO-GAP,               " p/o
       SY-VLINE NO-GAP,
       IT_TAB-WAERS NO-GAP,                 " curr
       SY-VLINE NO-GAP,
       IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFREQTY NO-GAP,               " 결제 구?
       SY-VLINE,
       IT_TAB-ZFMATGB,                      " 자재 구?
       SY-VLINE NO-GAP,
       IT_TAB-ERNAM   NO-GAP,               " 구매 담?
       SY-VLINE NO-GAP,
       IT_TAB-ZFSPRT  NO-GAP,               " 선적?
    85 SY-VLINE NO-GAP,
       IT_TAB-INCO1   NO-GAP,               " Incoterms
       SY-VLINE NO-GAP,
       IT_TAB-LIFNR   NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-NAME1   NO-GAP,
   120 SY-VLINE NO-GAP.

* hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE NO-GAP,
       IT_TAB-ZFAPPDT NO-GAP,             " 개설신?
       SY-VLINE NO-GAP,
       IT_TAB-ZFREQNO NO-GAP,             " 관리번?
       SY-VLINE NO-GAP,
       IT_TAB-ZFUSD NO-GAP,               " Currency
       SY-VLINE NO-GAP,
       IT_TAB-ZFUSDAM  CURRENCY IT_TAB-ZFUSD NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFBACD,                     " 송금구?
       SY-VLINE NO-GAP,
       IT_TAB-EKGRP NO-GAP,               " 구매 그?
       SY-VLINE NO-GAP,
       IT_TAB-ZFWERKS NO-GAP,             " 대표 Plant
    68 SY-VLINE NO-GAP,
       IT_TAB-ZFAPRT  NO-GAP,             " 도착?
    85 SY-VLINE,
       IT_TAB-ZFTRANS,                    " VIA
       SY-VLINE NO-GAP,
       IT_TAB-ZFBENI  NO-GAP,             " Beneficiary
       SY-VLINE NO-GAP,
       IT_TAB-NAME2   NO-GAP,
   120 SY-VLINE NO-GAP.

* stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.
ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.
   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
   EXPORT 'BES'           TO MEMORY ID 'BES'.
   EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
   EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.

   CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.

* 구매의뢰 테이블 SELECT
   PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
   PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
   PERFORM RESET_LIST.
ENDFORM.                    " P2000_SHOW_LC
