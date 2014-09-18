*& Report  ZRIMEDIR01                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : EDI RECEIPT(수입면 용)                                *
*&      작성자 : 나현주                                                *
*&      작성일 : 2003.01.16                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 1. Local Directory 의 FILE 을 UPLOAD.
*&               2. FILE 내역 DB 저장.
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
REPORT  ZRIMEDIR02  MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* REPORT 조회 및 DATA UPDATE 되기 위한 TABLE 정의.
*-----------------------------------------------------------------------
TABLES : ZTBL,
         ZTIDS,
        *ZTIDS,
         ZTIDSHS,
         ZTIDSHSD,
         ZTIDR,
         ZTIDRHS,
         ZTIDRHSD.

*-----------------------------------------------------------------------
* EDI 수신하기 위한 구조체 선언.
*-----------------------------------------------------------------------
TABLES : ZSRECHS,
         ZSRECHSD,
         BAPICURR.

INCLUDE : <ICON>.

DATA : W_OK_CODE         LIKE   SY-UCOMM,
       W_READ_CNT        TYPE   I,
       W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO,
       W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
       W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF,
       W_FILENAME        LIKE   RLGRAP-FILENAME,
       W_SUBRC           LIKE   SY-SUBRC,
       W_ERR_CHK         TYPE   C,
       W_PAGE            TYPE   I,
       W_LINE            TYPE   I,
       W_COUNT           TYPE   I,
       DIGITS            TYPE   I,
       W_KRW             LIKE   ZTBL-ZFBLAMC  VALUE 'KRW',
       W_USD             LIKE   ZTBL-ZFBLAMC  VALUE 'USD'.

SELECT-OPTIONS : S_ZFBLNO FOR ZTBL-ZFBLNO NO INTERVALS NO-DISPLAY.

*-----------------------------------------------------------------------
* REPORT WRITE 내역 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA:   BEGIN OF IT_TAB OCCURS 0,   ">> 내역.
        ZFBLNO    LIKE ZTBL-ZFBLNO,
        ZFSHNO    LIKE ZTBL-ZFSHNO,
        W_EBELN(15),
        ZFHBLNO   LIKE ZTBL-ZFHBLNO,
        ZFRGDSR   LIKE ZTBL-ZFRGDSR,   " 대표품명.
        ZFIDRNO   LIKE ZTIDS-ZFIDRNO,  "
        ZFCLSEQ   LIKE ZTIDS-ZFCLSEQ,
        ZFOPNNO   LIKE ZTIDS-ZFOPNNO,  " L/C NO.
        ZFREBELN  LIKE ZTIDS-ZFREBELN, " FILE NO.
        ZFIDSDT   LIKE ZTIDS-ZFIDSDT,  " 통관일.
        ZFCHGDT   LIKE ZTIDS-ZFCHGDT,  " 정정일.
        ZFIDWDT   LIKE ZTIDS-ZFIDWDT,  " 신고희망일.
        ZFINRC    LIKE ZTIDS-ZFINRC,   " 세관
        DOMTEXT   LIKE DD07T-DDTEXT,   " 세관명.
        ZFCUT     LIKE ZTIDS-ZFCUT,    " 관세사,
        NAME1     LIKE LFA1-NAME1,     " 관세사명.
        ZFSTAMT   LIKE ZTIDS-ZFSTAMT,  " 결제금액.
        ZFSTAMC   LIKE ZTIDS-ZFSTAMC,  " 통화.
        ZFINAMT  LIKE ZTIDS-ZFINAMT,   " 보험료.
        ZFINAMTC  LIKE ZTIDS-ZFINAMTC, " 통화.
        INCO1     LIKE ZTIDS-INCO1,    " INCO.
        ZFTFA     LIKE ZTIDS-ZFTFA,    " 운임.
        ZFTFAC    LIKE ZTIDS-ZFTFAC,   " 통화.
        ZFPRNAM   LIKE ZTIDS-ZFPRNAM.  " 청구자.
DATA:   END   OF IT_TAB.

*-----------------------------------------------------------------------
* EDI 수신하기 위한 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA  W_EDI_RECORD(9000).

" 수입신고 MAIN.
DATA: BEGIN OF IT_EDI OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
END OF IT_EDI.

" 수입신고 HS MAIN.
DATA: BEGIN OF IT_EDIHS OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
END OF IT_EDIHS.

" 수입신고 HS MAIN.
DATA: BEGIN OF IT_EDIHSD OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
END OF IT_EDIHSD.

" 수입면장 HEADER DATA.
DATA : BEGIN OF IT_HEADER  OCCURS  0,
       REC_001(015)   TYPE     C,          " 신고번호.
       REC_002(001)   TYPE     C,          " 신고구분.
       REC_003(008)   TYPE     C,          " 신고일자.
       REC_004(028)   TYPE     C,          " 수입상호.
       REC_005(008)   TYPE     C,          " 수입부호.
       REC_006(028)   TYPE     C,          " 납세상호.
       REC_007(012)   TYPE     C,          " 납세대표.
       REC_008(015)   TYPE     C,          " 납세통관.
       REC_009(040)   TYPE     C,          " 납세주소.
       REC_010(010)   TYPE     C,          " 납세사업자등록.
       REC_011(026)   TYPE     C,          " 공급자상호.
       REC_012(010)   TYPE     C,          " 공급자 CD.
       REC_013(003)   TYPE     C,          " 세관.
       REC_014(002)   TYPE     C,          " 과.
       REC_015(002)   TYPE     C,          " 거래구분.
       REC_016(002)   TYPE     C,          " 수입종류.
       REC_017(008)   TYPE     C,          " 입항일자.
       REC_018(003)   TYPE     C,          " 도착항CD.
       REC_019(008)   TYPE     C,          " 반입장 CD.
       REC_020(006)   TYPE     C,          " 장치위치.
       REC_021(008)   TYPE     C,          " 입고일자.
       REC_022(002)   TYPE     C,          " 징수형태.
       REC_023(020)   TYPE     C,          " 선기명.
       REC_024(002)   TYPE     C,          " 운송형태.
       REC_025(003)   TYPE     C,          " 운송용기.
       REC_026(020)   TYPE     C,          " HOUSE B/L.
       REC_027(020)   TYPE     C,          " 화물번호.
       REC_028(020)   TYPE     C,          " KEY FIELD.
       REC_029(003)   TYPE     C,          " 인도조건.
       REC_030(003)   TYPE     C,          " 결제통화.
       REC_031(014)   TYPE     C,          " 결제금액.
       REC_032(003)   TYPE     C,          " 운임통화.
       REC_033(013)   TYPE     C,          " 운임금액.
       REC_034(003)   TYPE     C,          " 보험통화.
       REC_035(013)   TYPE     C,          " 보험금액.
       REC_036(002)   TYPE     C,          " 포장단위.
       REC_037(008)   TYPE     C,          " 포장갯수.
       REC_038(014)   TYPE     C,          " 중량.
       REC_039(003)   TYPE     C,          " 총란수.
       REC_040(012)   TYPE     C,          " 감정가격.
       REC_041(010)   TYPE     C,          " 신고가 $.
       REC_042(015)   TYPE     C,          " 납부번호.
       REC_043(008)   TYPE     C,          " 수리일자.
       REC_044(020)   TYPE     C,          " LC NO.
       REC_045(020)   TYPE     C,          " MASTER B/L.
       REC_046(028)   TYPE     C,          " 무역상호.
       REC_047(007)   TYPE     C,          " 무역CD.
       REC_048(004)   TYPE     C,          " 수입자 CD.
       REC_049(001)   TYPE     C,          " 수입자 KD.
       REC_050(004)   TYPE     C,          " 납세자 CD.
       REC_051(003)   TYPE     C,          " 납세자 우편.
       REC_052(001)   TYPE     C,          " 통관계획.
       REC_053(030)   TYPE     C,          " 장치장명.
       REC_054(002)   TYPE     C,          " 결재방법.
       REC_055(002)   TYPE     C,          " 공급자국.
       REC_056(013)   TYPE     C,          " 도착항명.
       REC_057(002)   TYPE     C,          " 선기국 CD.
       REC_058(012)   TYPE     C,          " 선기국명.
       REC_059(002)   TYPE     C,          " 적출국 CD.
       REC_060(010)   TYPE     C,          " 적출국명.
       REC_061(001)   TYPE     C,          " 원산지 YN.
       REC_062(004)   TYPE     C,          " 운수 CD.
       REC_063(020)   TYPE     C,          " 운수명.
       REC_064(002)   TYPE     C,          " 특송 CD.
       REC_065(020)   TYPE     C,          " 특송명.
       REC_066(009)   TYPE     C,          " 달러환율.
       REC_067(009)   TYPE     C,          " 적용환율.
       REC_068(001)   TYPE     C,          " 가산구분.
       REC_069(006)   TYPE     C,          " 가산율.
       REC_070(003)   TYPE     C,          " 가산통화.
       REC_071(009)   TYPE     C,          " 가산환율.
       REC_072(015)   TYPE     C,          " 가산금액.
       REC_073(015)   TYPE     C,          " 가산원화.
       REC_074(001)   TYPE     C,          " 공제구분.
       REC_075(006)   TYPE     C,          " 공제율.
       REC_076(003)   TYPE     C,          " 공제통화.
       REC_077(009)   TYPE     C,          " 공제환율.
       REC_078(015)   TYPE     C,          " 공제금액.
       REC_079(015)   TYPE     C,          " 공제원화.
       REC_080(012)   TYPE     C,          " 신고액 원화.
       REC_081(011)   TYPE     C,          " 관세.
       REC_082(011)   TYPE     C,          " 특소세.
       REC_083(011)   TYPE     C,          " 교육세.
       REC_084(011)   TYPE     C,          " 부가세.
       REC_085(011)   TYPE     C,          " 농특세.
       REC_086(011)   TYPE     C,          " 주세.
       REC_087(012)   TYPE     C,          " 담당자.
       REC_088(012)   TYPE     C,          " 접수일자.
       REC_089(008)   TYPE     C.          " 납기일자.
DATA : END   OF IT_HEADER.

" 수입신고 란 사항.
DATA: BEGIN OF IT_HS OCCURS 0.
      INCLUDE STRUCTURE  ZSRECHS.
DATA : END OF IT_HS.

" 수입신고 행 사항.
DATA: BEGIN OF IT_HSD OCCURS 0.
      INCLUDE STRUCTURE  ZSRECHSD.
DATA : END OF IT_HSD.

*-----------------------------------------------------------------------
* DB UPDATE 하기 위한 INTERNAL TABLE
*-----------------------------------------------------------------------
" 수입신고 란 사항.
DATA: BEGIN OF IT_ZTIDSHS OCCURS 0.
      INCLUDE STRUCTURE  ZSIDSHS.
DATA : END OF IT_ZTIDSHS.

" 수입신고 행 사항.
DATA: BEGIN OF IT_ZTIDSHSD OCCURS 0.
      INCLUDE STRUCTURE  ZSIDSHSD.
DATA : END OF IT_ZTIDSHSD.

" 수입신고 란 사항.
DATA: BEGIN OF IT_ZSIDSHS OCCURS 0.
      INCLUDE STRUCTURE  ZSIDSHS.
DATA : END OF IT_ZSIDSHS.

" 수입신고 행 사항.
DATA: BEGIN OF IT_ZSIDSHSD OCCURS 0.
      INCLUDE STRUCTURE  ZSIDSHSD.
DATA : END OF IT_ZSIDSHSD.

" 수입신고 란 사항.
DATA: BEGIN OF IT_ZSIDSHS_ORG OCCURS 0.
      INCLUDE STRUCTURE  ZSIDSHS.
DATA : END OF IT_ZSIDSHS_ORG.

" 수입신고 행 사항.
DATA: BEGIN OF IT_ZSIDSHSD_ORG OCCURS 0.
      INCLUDE STRUCTURE  ZSIDSHSD.
DATA : END OF IT_ZSIDSHSD_ORG.

*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(266) TYPE C.
DATA : END OF IT_ERR_LIST.

DATA: MI_HANDLE       TYPE I,
      ANTWORT(1)      TYPE C,             " 공통 popup Screen에서 사?
      L_LEN           TYPE I,
      L_STRLEN        TYPE I,
      L_SIZE          TYPE I,
      W_MOD           TYPE I,
      L_DATE          TYPE SY-DATUM,
      INCLUDE(8)      TYPE C,             "
      L_TIME          TYPE SY-UZEIT,
      W_BUKRS         LIKE ZTIMIMGTX-BUKRS.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
    PARAMETERS: P_BUKRS LIKE ZTIMIMGTX-BUKRS OBLIGATORY
                        DEFAULT 'KHNP'.
SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION 절.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Import System Config Check
   W_BUKRS = P_BUKRS.

* LOCAL FILE DATA UPLOAD.
   PERFORM   P1000_GET_UPLOAD_FILE  USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* DB Write
   PERFORM   P3000_DB_DATA_WRITE.

* REPORT WRITE.
   PERFORM   P3000_DATA_WRITE       USING    W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'. MESSAGE S738.   EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
   W_OK_CODE = SY-UCOMM.
   CASE SY-UCOMM.
*------- Abbrechen (CNCL) ----------------------------------------------
      WHEN 'CNCL'.
         ANTWORT = 'C'.
         SET SCREEN 0.    LEAVE SCREEN.
      WHEN 'DISP'.
         PERFORM P2000_SHOW_IDS USING  IT_TAB-ZFBLNO
                                       IT_TAB-ZFCLSEQ.

      WHEN OTHERS.
   ENDCASE.
   CLEAR : IT_TAB.

*&---------------------------------------------------------------------*
*&   Event AT LINE-SELECTION
*&---------------------------------------------------------------------*
AT LINE-SELECTION.
  CASE INCLUDE.
    WHEN 'POPU'.
       IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
          MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
                  NUMBER IT_ERR_LIST-MSGNR
                  WITH   IT_ERR_LIST-MSGV1
                         IT_ERR_LIST-MSGV2
                         IT_ERR_LIST-MSGV3
                         IT_ERR_LIST-MSGV4.
       ENDIF.
       CLEAR : IT_ERR_LIST.
  ENDCASE.

  SET SCREEN 0.    LEAVE SCREEN.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIME10'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50 '[ 수입신고필정보 조회 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /3 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,(15) '구매문서' NO-GAP,  SY-VLINE,
            (21) 'House B/L No'      NO-GAP,  SY-VLINE,
            (20) '결제금액'          NO-GAP,  SY-VLINE,
            (20) '보험료'            NO-GAP , SY-VLINE,
            (20) '세관'              NO-GAP,  SY-VLINE,
            (11) '통관일'            NO-GAP,  SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE :/ SY-VLINE,(15)  '면허번호' NO-GAP,  SY-VLINE,
            (21)  '대표 L/C 개설번호' NO-GAP, SY-VLINE,
            (20)  '대표품명'         NO-GAP,  SY-VLINE,
            (20)  '운임'             NO-GAP,  SY-VLINE,
            (20)  '관세사'           NO-GAP,  SY-VLINE,
            (11)  '신고희망일'       NO-GAP,  SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   PERFORM   P4000_SET_SELETE_OPTION.
   IF S_ZFBLNO[] IS INITIAL.
      W_ERR_CHK  =  'Y'.
      EXIT.
   ENDIF.

   PERFORM   P4000_GET_ZTIDS      USING   W_ERR_CHK.
   IF  W_ERR_CHK  EQ  'Y'. EXIT.  ENDIF.

   PERFORM   P4000_ZTIDS_WRITE    USING   W_ERR_CHK.

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

   IF W_COUNT GT 0.
      WRITE : / '총', W_COUNT, '건'.
   ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.

   SELECT SINGLE * FROM ZTBL WHERE ZFBLNO EQ IT_TAB-ZFBLNO.
   CONCATENATE IT_TAB-ZFREBELN ZTBL-ZFSHNO INTO IT_TAB-W_EBELN
               SEPARATED BY '-'.

   WRITE :/ SY-VLINE,
           (15) IT_TAB-W_EBELN NO-GAP,           SY-VLINE, "PO No.
           (21) IT_TAB-ZFHBLNO NO-GAP,           SY-VLINE, "House B/L No
           (03) IT_TAB-ZFSTAMC,                            "결제통화.
           (16) IT_TAB-ZFSTAMT CURRENCY IT_TAB-ZFSTAMC NO-GAP,
                                                 SY-VLINE, "결제금액.
           (03) IT_TAB-ZFINAMTC,                           "보험료통화.
           (16) IT_TAB-ZFINAMT CURRENCY IT_TAB-ZFINAMTC NO-GAP,
                                                 SY-VLINE, "보험료
           (20) IT_TAB-DOMTEXT NO-GAP,           SY-VLINE, "세관.
           (11) IT_TAB-ZFIDSDT NO-GAP,           SY-VLINE. "통관일.
* HIDE.
   HIDE: IT_TAB.

   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   WRITE :/ SY-VLINE,
           (15)  IT_TAB-ZFIDRNO NO-GAP,        SY-VLINE,  "면허번호.
           (21)  IT_TAB-ZFOPNNO NO-GAP,        SY-VLINE,  "대표 L/C No
           (20)  IT_TAB-ZFRGDSR NO-GAP,        SY-VLINE,  "대표품명.
           (03)  IT_TAB-ZFTFAC,                           "운임통화.
           (16)  IT_TAB-ZFTFA CURRENCY IT_TAB-ZFTFAC NO-GAP,
                                               SY-VLINE,  "운 임.
           (20)  IT_TAB-NAME1   NO-GAP,        SY-VLINE,  "관세사.
           (11)  IT_TAB-ZFIDWDT NO-GAP,        SY-VLINE.  "신고희망일.
   WRITE:/ SY-ULINE.
* HIDE
   HIDE: IT_TAB.
   W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_UNRELEASE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_UNRELEASE_CHECK USING    P_ZFREQNO.
* Amend 존재여부 체?

* Invoice 체?

ENDFORM.                    " P2000_UNRELEASE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_UPLOAD_FILE
*&---------------------------------------------------------------------*
*       UPLOAD용 FILENAME을 발췌하는 서브루틴.
*----------------------------------------------------------------------*
FORM P1000_GET_UPLOAD_FILE USING    W_ERR_CHK.

  " 수입면허 HEADER DATA UPLOAD.
  MOVE  'C:\한전Data\import_h.TXT'  TO  W_FILENAME.

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME = W_FILENAME
      FILETYPE = 'DAT'
    TABLES
      DATA_TAB = IT_EDI.

  IF SY-SUBRC NE 0.
     MESSAGE E977 WITH 'File 을 Upload 하는 도중 Error가 발생했습니다.'.
  ENDIF.

  " 수입면허 란 사항 DATA UPLOAD.
  MOVE  'C:\한전Data\import_d.TXT' TO W_FILENAME.
  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME = W_FILENAME
      FILETYPE = 'DAT'
    TABLES
      DATA_TAB = IT_EDIHS.

  IF SY-SUBRC NE 0.
     MESSAGE E977 WITH 'File 을 Upload 하는 도중 Error가 발생했습니다.'.
  ENDIF.

  " 수입면허 행 사항 DATA UPLOAD.
  MOVE  'C:\한전Data\import_m.TXT' TO W_FILENAME.
  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME = W_FILENAME
      FILETYPE = 'DAT'
    TABLES
      DATA_TAB = IT_EDIHSD.

  IF SY-SUBRC NE 0.
     MESSAGE E977 WITH 'File 을 Upload 하는 도중 Error가 발생했습니다.'.
  ENDIF.

ENDFORM.                    " P1000_GET_UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ZTIDS_ZFTBAK  text
*      <--P_ZTIDS_ZFSTAMC  text
*----------------------------------------------------------------------*
FORM SET_CURR_CONV_TO_INTERNAL  CHANGING P_AMOUNT
                                         P_WAERS.
   BAPICURR-BAPICURR = P_AMOUNT.

   CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
        EXPORTING
           CURRENCY              =   P_WAERS
           AMOUNT_EXTERNAL       =   BAPICURR-BAPICURR
           MAX_NUMBER_OF_DIGITS  =   DIGITS
        IMPORTING
           AMOUNT_INTERNAL       =   P_AMOUNT
        EXCEPTIONS
           OTHERS                =   1.

ENDFORM.                    " SET_CURR_CONV_TO_INTERNAL

*&---------------------------------------------------------------------*
*&      Form  P4000_HEADER_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_HEADER_INSERT .

   REFRESH : IT_ZSIDSHS, IT_ZSIDSHSD.

   " INTERNAL TABLE 값을 DB로 저장.
   LOOP  AT  IT_EDI.

      CLEAR : IT_HEADER.
      MOVE  IT_EDI  TO  IT_HEADER.

      CLEAR : ZTIDS.
      " KEY FIELD CHECK.
      MOVE : IT_HEADER-REC_028(10)    TO  ZTIDS-ZFBLNO,
             IT_HEADER-REC_028+10(5)  TO  ZTIDS-ZFCLSEQ.

      CLEAR : ZTIDR.
      SELECT SINGLE * FROM ZTIDR
             WHERE    ZFBLNO  EQ  ZTIDS-ZFBLNO
             AND      ZFCLSEQ EQ  ZTIDS-ZFCLSEQ.
      IF SY-SUBRC NE 0.
         CONTINUE.
      ENDIF.

      " 기존 자료 있으면 SKIP.
      SELECT SINGLE * FROM ZTIDS
             WHERE    ZFBLNO   EQ  ZTIDS-ZFBLNO
             AND      ZFCLSEQ  EQ  ZTIDS-ZFCLSEQ.

      " 수입신고의뢰 하지 않은 DATA SKIP.
      IF SY-SUBRC EQ 0.
         CONTINUE.
      ENDIF.

      " 수입 DATA 와 연계 KEY 값 SET.
      MOVE-CORRESPONDING  ZTIDR  TO  ZTIDS.

      MOVE : IT_HEADER-REC_001   TO  ZTIDS-ZFIDRNO,     "수입신고번호.
             IT_HEADER-REC_002   TO  ZTIDS-ZFIDRCD,     "신고구분.
             IT_HEADER-REC_004   TO  ZTIDS-ZFIAPNM,     "수입자상호.
             IT_HEADER-REC_005   TO  ZTIDS-ZFIAPCD,     "수입자부호.
             IT_HEADER-REC_006   TO  ZTIDS-ZFTDNM1,     "납세자상호.
             IT_HEADER-REC_007   TO  ZTIDS-ZFTDNM2,     "납세자대표자.
             IT_HEADER-REC_008   TO  ZTIDS-ZFTDNO,      "납세자통관고유.
             IT_HEADER-REC_009   TO  ZTIDS-ZFTDAD1,     "납세자주소.
             IT_HEADER-REC_010   TO  ZTIDS-ZFTDTC,      "납세자사업자.
             IT_HEADER-REC_011   TO  ZTIDS-ZFSUPNM,     "공급자상호.
             IT_HEADER-REC_012   TO  ZTIDS-ZFSUPNO,     "공급자부호.
             IT_HEADER-REC_013   TO  ZTIDS-ZFINRC,      "신고지세관.
             IT_HEADER-REC_014   TO  ZTIDS-ZFINRCD,     "신고지세관과.
             IT_HEADER-REC_015   TO  ZTIDS-ZFPONC,      "수입거래구분.
             IT_HEADER-REC_016   TO  ZTIDS-ZFITKD,      "수입신고종류.
             IT_HEADER-REC_018   TO  ZTIDS-ZFAPRTC,     "도착항CD.
             IT_HEADER-REC_020   TO  ZTIDS-ZFLOCA,      "장치위치.
             IT_HEADER-REC_022   TO  ZTIDS-ZFCOCD,      "징수형태.
             IT_HEADER-REC_023   TO  ZTIDS-ZFCARNM,     "선기명.
             IT_HEADER-REC_024   TO  ZTIDS-ZFTRMET,     "운송형태.
             IT_HEADER-REC_025   TO  ZTIDS-ZFTRCN,      "운송용기.
             IT_HEADER-REC_026   TO  ZTIDS-ZFHBLNO,     "HOUSE B/L NO.
             IT_HEADER-REC_027   TO  ZTIDS-ZFIMCR,      "화물관리번호.
             IT_HEADER-REC_029   TO  ZTIDS-INCO1,       "인도조건.
             IT_HEADER-REC_030   TO  ZTIDS-ZFSTAMC,     "결제통화.
             IT_HEADER-REC_032   TO  ZTIDS-ZFTFAC,      "운임통화.
             IT_HEADER-REC_034   TO  ZTIDS-ZFINAMTC,    "보험료통화.
             IT_HEADER-REC_036   TO  ZTIDS-ZFPKNM,      "포장종류.
             IT_HEADER-REC_037   TO  ZTIDS-ZFPKCNT,     "포장수.
             IT_HEADER-REC_038   TO  ZTIDS-ZFTOWT,      "총중량.
             IT_HEADER-REC_042   TO  ZTIDS-ZFRFFNO,     "납부번호.
             IT_HEADER-REC_044   TO  ZTIDS-ZFOPNNO,     "L/C 번호.
             IT_HEADER-REC_045   TO  ZTIDS-ZFMBLNO,     "MASTER B/L
             IT_HEADER-REC_046   TO  ZTIDS-ZFTRDNM,     "무역상호.
             IT_HEADER-REC_047   TO  ZTIDS-ZFTRDNO,     "무역부호.
             IT_HEADER-REC_048   TO  ZTIDS-ZFIAPCD,     "수입자부호.
             IT_HEADER-REC_049   TO  ZTIDS-ZFIMCD,      "수입자종류.
             IT_HEADER-REC_050   TO  ZTIDS-ZFTDCD,      "납세자부호.
             IT_HEADER-REC_052   TO  ZTIDS-ZFCUPR,      "통관계획부호.
             IT_HEADER-REC_053   TO  ZTIDS-ZFPLNM,      "장치장명.
             IT_HEADER-REC_054   TO  ZTIDS-ZFAMCD,      "결재방법.
             IT_HEADER-REC_055   TO  ZTIDS-ZFSUPC,      "공급자국.
             IT_HEADER-REC_057   TO  ZTIDS-ZFCAC,       "선기국.
             IT_HEADER-REC_059   TO  ZTIDS-ZFSCON,      "적출국.
             IT_HEADER-REC_061   TO  ZTIDS-ZFORGYN,     "원산지유무.
             IT_HEADER-REC_064   TO  ZTIDS-ZFSTRCD,     "특송업체CD.
             IT_HEADER-REC_066   TO  ZTIDS-ZFEXUS,      "달러환율.
             IT_HEADER-REC_068   TO  ZTIDS-ZFADAMC,     "가산금구분.
             IT_HEADER-REC_069   TO  ZTIDS-ZFADRT,      "가산율.
             IT_HEADER-REC_070   TO  ZTIDS-ZFADAMCU,    "가산금통화.
             IT_HEADER-REC_071   TO  ZTIDS-ZFADAMX,     "가산환율.
             IT_HEADER-REC_074   TO  ZTIDS-ZFDUAMC,     "공제금구분.
             IT_HEADER-REC_075   TO  ZTIDS-ZFDURT,      "공제율.
             IT_HEADER-REC_076   TO  ZTIDS-ZFDUAMCU,    "공제금통화.
             IT_HEADER-REC_077   TO  ZTIDS-ZFEXDU,      "공제금환율.
             IT_HEADER-REC_087   TO  ZTIDS-ZFCHNAM.     "담당자.

      " DATE CONVERT.
      "신고일.
      IF NOT IT_HEADER-REC_003  IS  INITIAL.
         CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                  DATE_EXTERNAL = IT_HEADER-REC_003
              IMPORTING
                  DATE_INTERNAL = ZTIDS-ZFIDWDT.
      ENDIF.

      "입항일.
      IF NOT IT_HEADER-REC_017 IS INITIAL.
         CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                  DATE_EXTERNAL = IT_HEADER-REC_017
              IMPORTING
                  DATE_INTERNAL = ZTIDS-ZFENDT.
      ENDIF.

      "입고일.
      IF NOT IT_HEADER-REC_021 IS INITIAL.
         CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                  DATE_EXTERNAL = IT_HEADER-REC_021
              IMPORTING
                  DATE_INTERNAL = ZTIDS-ZFINDT.
      ENDIF.

      "신고일.
      IF NOT IT_HEADER-REC_043 IS INITIAL.
         CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                  DATE_EXTERNAL = IT_HEADER-REC_043
              IMPORTING
                  DATE_INTERNAL = ZTIDS-ZFIDSDT.
      ENDIF.

*      "접수일.
*      IF NOT IT_HEADER-REC_088 IS INITIAL.
*         CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
*              EXPORTING
*                  DATE_EXTERNAL = IT_HEADER-REC_088
*              IMPORTING
*                  DATE_INTERNAL = ZTIDS-ZFCTDT.
*      ENDIF.
*
*      "납기일.
*      IF NOT IT_HEADER-REC_089 IS INITIAL.
*         CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
*              EXPORTING
*                  DATE_EXTERNAL = IT_HEADER-REC_089
*              IMPORTING
*                  DATE_INTERNAL = ZTIDS-ZFTXDT.
*      ENDIF.

      " 금액 FILED CONVERT.
      "결제금액.
      MOVE IT_HEADER-REC_031 TO ZTIDS-ZFTBAK.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFTBAK ZTIDS-ZFSTAMC.

      "운임금액.
      MOVE IT_HEADER-REC_033 TO ZTIDS-ZFTFA.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFTFA ZTIDS-ZFTFAC.

      "보험금액.
      MOVE IT_HEADER-REC_035 TO ZTIDS-ZFINAMT.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFINAMT ZTIDS-ZFINAMTC.

      "감정가격.
      MOVE IT_HEADER-REC_040 TO ZTIDS-ZFTBAK.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFTBAK  W_KRW .

      "신고가 $.
      MOVE IT_HEADER-REC_041 TO ZTIDS-ZFTBAU.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFTBAU W_USD.

      "가산금액.
      MOVE IT_HEADER-REC_072 TO ZTIDS-ZFADAM.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFADAM ZTIDS-ZFADAMCU.

      "가산금액원화.
      MOVE IT_HEADER-REC_073 TO ZTIDS-ZFADAMK.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFADAMK W_KRW.

      "공제금액.
      MOVE IT_HEADER-REC_078 TO ZTIDS-ZFDUAM.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFDUAM ZTIDS-ZFDUAMCU.

      "공제금액원화.
      MOVE IT_HEADER-REC_079 TO ZTIDS-ZFDUAMK.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFDUAMK W_KRW.

      "관세.
      MOVE IT_HEADER-REC_081 TO ZTIDS-ZFCUAMTS.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFCUAMTS W_KRW.

      "특소세.
      MOVE IT_HEADER-REC_082 TO ZTIDS-ZFSCAMTS.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFSCAMTS W_KRW.

      "교육세.
      MOVE IT_HEADER-REC_083 TO ZTIDS-ZFEDAMTS.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING  ZTIDS-ZFEDAMTS W_KRW.

      "부가세.
      MOVE IT_HEADER-REC_084 TO ZTIDS-ZFVAAMTS.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFVAAMTS W_KRW.

      "농특세.
      MOVE IT_HEADER-REC_085 TO ZTIDS-ZFAGAMTS.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFAGAMTS W_KRW .

      "주세.
      MOVE IT_HEADER-REC_086 TO ZTIDS-ZFDRAMTS.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING ZTIDS-ZFDRAMTS W_KRW .

      " 란사항 DATA 걸르기.
      LOOP  AT IT_ZTIDSHS WHERE ZFBLNO  EQ  ZTIDS-ZFBLNO
                          AND   ZFCLSEQ EQ  ZTIDS-ZFCLSEQ.
         MOVE-CORRESPONDING  IT_ZTIDSHS TO  IT_ZSIDSHS.
         APPEND  IT_ZSIDSHS.
      ENDLOOP.

      " 행사항 DATA 걸르기.
      LOOP  AT IT_ZTIDSHSD WHERE ZFBLNO  EQ  ZTIDS-ZFBLNO
                           AND   ZFCLSEQ EQ  ZTIDS-ZFCLSEQ.
         MOVE-CORRESPONDING  IT_ZTIDSHSD TO  IT_ZSIDSHSD.
         APPEND  IT_ZSIDSHSD.
      ENDLOOP.

      " DB UPDATE.
      CALL FUNCTION 'ZIM_ZTIDS_DOC_MODIFY'
           EXPORTING
              W_OK_CODE        = W_OK_CODE
              ZFBLNO           = ZTIDS-ZFBLNO
              ZFCLSEQ          = ZTIDS-ZFCLSEQ
              ZFSTATUS         = 'C'
              W_ZTIDS_OLD      = *ZTIDS
              W_ZTIDS          = ZTIDS
           TABLES
             IT_ZSIDSHS_OLD   = IT_ZSIDSHS_ORG
             IT_ZSIDSHS       = IT_ZSIDSHS
             IT_ZSIDSHSD_OLD  = IT_ZSIDSHSD_ORG
             IT_ZSIDSHSD      = IT_ZSIDSHSD
           EXCEPTIONS
             ERROR_UPDATE     = 1
             ERROR_DELETE     = 2
             ERROR_INSERT     = 3.
      APPEND IT_HEADER.
   ENDLOOP.

ENDFORM.                    " P4000_HEADER_INSERT
*&---------------------------------------------------------------------*
*&      Form  P4000_HS_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_HS_INSERT .

   REFRESH : IT_ZTIDSHS.
   LOOP  AT  IT_EDIHS.

      CLEAR : IT_HS.
      MOVE  IT_EDIHS   TO  IT_HS.

      CLEAR : IT_ZTIDSHS.
      " KEY FIELD CHECK.
      MOVE : IT_HS-REC_024(10)    TO  IT_ZTIDSHS-ZFBLNO,
             IT_HS-REC_024+10(5)  TO  IT_ZTIDSHS-ZFCLSEQ.

      SELECT SINGLE * FROM ZTIDS
             WHERE    ZFBLNO   EQ  IT_ZTIDSHS-ZFBLNO
             AND      ZFCLSEQ  EQ  IT_ZTIDSHS-ZFCLSEQ.
      W_SUBRC = SY-SUBRC.
      " 수입신고의뢰 하지 않은 DATA SKIP.
      CLEAR : ZTIDR.
      SELECT SINGLE * FROM ZTIDR
             WHERE    ZFBLNO  EQ  IT_ZTIDSHS-ZFBLNO
             AND      ZFCLSEQ EQ  IT_ZTIDSHS-ZFCLSEQ.
      IF SY-SUBRC NE 0.
         CONTINUE.
      ENDIF.

      MOVE : IT_HS-REC_002   TO  IT_ZTIDSHS-ZFCONO,     "란사항.
             IT_HS-REC_003   TO  IT_ZTIDSHS-STAWN,      "세번부호.
             IT_HS-REC_004   TO  IT_ZTIDSHS-ZFWETM,     "중량단위.
             IT_HS-REC_005   TO  IT_ZTIDSHS-ZFWET,      "순중량.
             IT_HS-REC_006   TO  IT_ZTIDSHS-ZFQNTM,     "수량단위.
             IT_HS-REC_007   TO  IT_ZTIDSHS-ZFQNT,      "수량.
             IT_HS-REC_012   TO  IT_ZTIDSHS-ZFGDNM,     "표준품명.
             IT_HS-REC_020   TO  IT_ZTIDSHS-ZFTXGB,     "세율구분.
             IT_HS-REC_021   TO  IT_ZTIDSHS-ZFCURT1,    "관세종가.
             IT_HS-REC_023   TO  IT_ZTIDSHS-ZFINRT1,    "내국세종가.
             IT_HS-REC_027   TO  IT_ZTIDSHS-ZFTGDNM,    "거래품명.
             IT_HS-REC_028   TO  IT_ZTIDSHS-ZFGCCD,     "상표코드.
             IT_HS-REC_029   TO  IT_ZTIDSHS-ZFGCNM,     "상표품명.
             IT_HS-REC_031   TO  IT_ZTIDSHS-ZFATTYN,    "첨부서류여부.
             IT_HS-REC_032   TO  IT_ZTIDSHS-ZFREQNM,    "환급단위.
             IT_HS-REC_033   TO  IT_ZTIDSHS-ZFREQN,     "환급수량.
             IT_HS-REC_034   TO  IT_ZTIDSHS-ZFTXAMCD,   "관세방법.
             IT_HS-REC_035   TO  IT_ZTIDSHS-ZFCDPCD,    "관세감면구분.
             IT_HS-REC_037   TO  IT_ZTIDSHS-ZFCUDIV,    "관세감면부호.
             IT_HS-REC_038   TO  IT_ZTIDSHS-ZFRDRT,     "관세감면율.
             IT_HS-REC_039   TO  IT_ZTIDSHS-ZFHMTCD,    "내국세구분.
             IT_HS-REC_040   TO  IT_ZTIDSHS-ZFSCCD,     "특소세부호.
             IT_HS-REC_041   TO  IT_ZTIDSHS-ZFHMTTY,    "내종세부호.
             IT_HS-REC_042   TO  IT_ZTIDSHS-ZFVTXCD,    "부가세구분.
             IT_HS-REC_043   TO  IT_ZTIDSHS-ZFVTXTY,    "부가세감면부호.
             IT_HS-REC_044   TO  IT_ZTIDSHS-ZFVTRT,     "부가감면율.
             IT_HS-REC_045   TO  IT_ZTIDSHS-ZFETXCD,    "교육세구분.
             IT_HS-REC_046   TO  IT_ZTIDSHS-ZFATXCD,    "농특세구분.
             IT_HS-REC_047   TO  IT_ZTIDSHS-ZFADGB,     "가산금구분.
             IT_HS-REC_048   TO  IT_ZTIDSHS-ZFADRT,     "가산율.
             IT_HS-REC_049   TO  IT_ZTIDSHS-ZFADC,      "가산통화.
             IT_HS-REC_050   TO  IT_ZTIDSHS-ZFEXAD,     "가산환율.
             IT_HS-REC_053   TO  IT_ZTIDSHS-ZFDUAMC,    "공제구분.
             IT_HS-REC_054   TO  IT_ZTIDSHS-ZFDURT,     "공제율.
             IT_HS-REC_055   TO  IT_ZTIDSHS-ZFDUAMCU,   "공제통화.
             IT_HS-REC_056   TO  IT_ZTIDSHS-ZFEXDU,     "공제환율.
             IT_HS-REC_058   TO  IT_ZTIDSHS-ZFORYN,     "원산유무.
             IT_HS-REC_059   TO  IT_ZTIDSHS-ZFORME,     "원산방법.
             IT_HS-REC_060   TO  IT_ZTIDSHS-ZFORTY,     "원산형태.
             IT_HS-REC_061   TO  IT_ZTIDSHS-ZFTRRL,     "관세거래관계.
             IT_HS-REC_062   TO  IT_ZTIDSHS-ZFGDAL,     "품목세번.
             IT_HS-REC_063   TO  IT_ZTIDSHS-ZFEXOP,     "관세검사.
             IT_HS-REC_064   TO  IT_ZTIDSHS-ZFCTW1,     "관세사기재1.
             IT_HS-REC_065   TO  IT_ZTIDSHS-ZFCTW2,     "관세사기재2.
             IT_HS-REC_066   TO  IT_ZTIDSHS-ZFCTW3,     "관세사기재3.
             IT_HS-REC_067   TO  IT_ZTIDSHS-ZFCTW4,     "관세사기재4.
             IT_HS-REC_068   TO  IT_ZTIDSHS-ZFSTCS,     "특송 C/S.
             IT_HS-REC_069   TO  IT_ZTIDSHS-ZFCSGB,     "C/S 구분.
             IT_HS-REC_070   TO  IT_ZTIDSHS-ZFCSCH.     "검사변경.

      "금액 FILED CONVERT.
      "신고액 $.
      MOVE IT_HS-REC_010 TO IT_ZTIDSHS-ZFTBAU.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFTBAU W_USD.

      "신고액 원화.
      MOVE IT_HS-REC_011 TO IT_ZTIDSHS-ZFTBAK.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFTBAK W_KRW.

      "관세액.
      MOVE IT_HS-REC_013 TO IT_ZTIDSHS-ZFCUAMT.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFCUAMT W_KRW.

      "내국세액.
      MOVE IT_HS-REC_014 TO IT_ZTIDSHS-ZFHMAMT.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFHMAMT W_KRW.

      "교육액.
      MOVE IT_HS-REC_015 TO IT_ZTIDSHS-ZFEDAMT.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFEDAMT W_KRW.

      "부가세.
      MOVE IT_HS-REC_017 TO IT_ZTIDSHS-ZFVAAMT.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFVAAMT W_KRW.

      "보험료.
      MOVE IT_HS-REC_018 TO IT_ZTIDSHS-ZFCUAMT.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFCUAMT W_KRW.

      "결제금액.
      MOVE IT_HS-REC_022 TO IT_ZTIDSHS-ZFSTAMT.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFSTAMT W_KRW.

      "농특세.
      MOVE IT_HS-REC_023 TO IT_ZTIDSHS-ZFAGAMT.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFAGAMT W_KRW.

      "가산금액
      MOVE IT_HS-REC_051 TO IT_ZTIDSHS-ZFADAM.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFADAM IT_ZTIDSHS-ZFADC.

      "가산금액원화
      MOVE IT_HS-REC_052 TO IT_ZTIDSHS-ZFADAMK.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFADAMK W_KRW.

      "공제금액.
      MOVE IT_HS-REC_057 TO IT_ZTIDSHS-ZFDUAM.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFDUAM IT_ZTIDSHS-ZFDUAMCU.

      "관세분담금.
      MOVE IT_HS-REC_079 TO IT_ZTIDSHS-ZFCDIVAM.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFCDIVAM W_KRW.

      "관세총분수.
      MOVE IT_HS-REC_080 TO IT_ZTIDSHS-ZFTDIVAM.
      PERFORM SET_CURR_CONV_TO_INTERNAL
                       CHANGING IT_ZTIDSHS-ZFTDIVAM IT_ZTIDSHS-ZFDUAMCU.

      APPEND  IT_ZTIDSHS.

   ENDLOOP.

ENDFORM.                    " P4000_HS_INSERT

*&---------------------------------------------------------------------*
*&      Form  P4000_HSD_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_HSD_INSERT .

   REFRESH : IT_ZSIDSHSD.
   LOOP  AT  IT_EDIHSD.

      CLEAR : IT_HSD.
      MOVE  IT_EDIHSD   TO  IT_HSD.

      CLEAR : ZTIDSHSD.
      " KEY FIELD CHECK.
      MOVE : IT_HSD-REC_009(10)    TO  IT_ZTIDSHSD-ZFBLNO,
             IT_HSD-REC_009+10(5)  TO  IT_ZTIDSHSD-ZFCLSEQ.

      SELECT SINGLE * FROM ZTIDS
             WHERE    ZFBLNO   EQ  IT_ZTIDSHSD-ZFBLNO
             AND      ZFCLSEQ  EQ  IT_ZTIDSHSD-ZFCLSEQ.
      W_SUBRC = SY-SUBRC.
      " 수입신고의뢰 하지 않은 DATA SKIP.
      CLEAR : ZTIDR.
      SELECT SINGLE * FROM ZTIDR
             WHERE    ZFBLNO  EQ  IT_ZTIDSHSD-ZFBLNO
             AND      ZFCLSEQ EQ  IT_ZTIDSHSD-ZFCLSEQ.
      IF SY-SUBRC NE 0.
         CONTINUE.
      ENDIF.

      MOVE : IT_HSD-REC_002   TO  IT_ZTIDSHSD-ZFCONO,    "란번호.
             IT_HSD-REC_003   TO  IT_ZTIDSHSD-ZFRONO,    "행번호.
             IT_HSD-REC_006   TO  IT_ZTIDSHSD-ZFQNT,     "수량.
             IT_HSD-REC_007   TO  IT_ZTIDSHSD-ZFQNTM,    "수량단위.
             IT_HSD-REC_023   TO  IT_ZTIDSHSD-STAWN,     "HSCODE.
             IT_HSD-REC_025   TO  IT_ZTIDSHSD-ZFGDDS1,   "규격1.
             IT_HSD-REC_026   TO  IT_ZTIDSHSD-ZFGDDS2,   "규격2.
             IT_HSD-REC_027   TO  IT_ZTIDSHSD-ZFGDDS3,   "규격3.
             IT_HSD-REC_028   TO  IT_ZTIDSHSD-ZFGDIN1,   "성분1.
             IT_HSD-REC_029   TO  IT_ZTIDSHSD-ZFGDIN2.   "성분2.

      " 수입신고시 행 존재유무 CHECK.
      SELECT SINGLE * FROM ZTIDRHSD
             WHERE  ZFBLNO  EQ  IT_ZTIDSHSD-ZFBLNO
             AND    ZFCLSEQ EQ  IT_ZTIDSHSD-ZFCLSEQ
             AND    ZFCONO  EQ  IT_ZTIDSHSD-ZFCONO
             AND    ZFRONO  EQ  IT_ZTIDSHSD-ZFRONO.
      IF SY-SUBRC EQ 0.
         MOVE  : ZTIDRHSD-ZFIVNO  TO  IT_ZTIDSHSD-ZFIVNO,
                 ZTIDRHSD-ZFIVDNO TO  IT_ZTIDSHSD-ZFIVDNO.
      ENDIF.

      " 행 TABLE 존재유무 CHECK.
      SELECT SINGLE * FROM ZTIDSHSD
             WHERE  ZFBLNO  EQ  IT_ZTIDSHSD-ZFBLNO
             AND    ZFCLSEQ EQ  IT_ZTIDSHSD-ZFCLSEQ
             AND    ZFCONO  EQ  IT_ZTIDSHSD-ZFCONO
             AND    ZFRONO  EQ  IT_ZTIDSHSD-ZFRONO.
      APPEND  IT_ZTIDSHSD.

   ENDLOOP.

ENDFORM.                    " P4000_HSD_INSERT
*&---------------------------------------------------------------------*
*&      Form  P3000_DB_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_DB_DATA_WRITE .

   " HS 란 사항 TABLE INSERT.
   PERFORM  P4000_HS_INSERT.

   " HS 행 사항 TABLE INSERT.
   PERFORM  P4000_HSD_INSERT.

   " MAIN HEADER TABLE INSERT.
   PERFORM  P4000_HEADER_INSERT.

ENDFORM.                    " P3000_DB_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P4000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_SET_SELETE_OPTION .

   REFRESH : S_ZFBLNO.
   LOOP  AT  IT_HEADER.

      MOVE : IT_HEADER-REC_028(10)    TO  S_ZFBLNO-LOW,
             'I'                      TO  S_ZFBLNO-SIGN,
             'EQ'                     TO  S_ZFBLNO-OPTION.
      APPEND  S_ZFBLNO.
   ENDLOOP.

ENDFORM.                    " P4000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTIDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P4000_GET_ZTIDS  USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.
  REFRESH IT_TAB.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM  ZTIDS
  WHERE ZFBLNO    IN  S_ZFBLNO.
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'. EXIT.
  ENDIF.

ENDFORM.                    " P4000_GET_ZTIDS
*&---------------------------------------------------------------------*
*&      Form  P4000_ZTIDS_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P4000_ZTIDS_WRITE  USING    W_ERR_CHK.

   SET PF-STATUS 'ZIMR51'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMR51'.           " GUI TITLE SETTING..

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P3000_LINE_WRITE.

      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.
   ENDLOOP.

ENDFORM.                    " P4000_ZTIDS_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SHOW_IDS  USING    P_ZFBLNO
                              P_ZFCLSEQ.

   SET PARAMETER ID 'ZPBLNO'    FIELD  P_ZFBLNO.
   SET PARAMETER ID 'ZPCLSEQ'   FIELD  P_ZFCLSEQ.
   SET PARAMETER ID 'ZPHBLNO'    FIELD ''.
   SET PARAMETER ID 'ZPIDRNO'   FIELD ''.

   CALL TRANSACTION 'ZIM76' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_IDS
