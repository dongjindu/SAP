*&---------------------------------------------------------------------*
*& Report  ZRIMAMDREQ                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : LC Amend 신청서.                                      *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2001.02.23                                            *
*&    적용회사 : 제일제당                                              *
*&---------------------------------------------------------------------*
*&   DESC.     : L/C Amend 신청서를 출력하기 위한 레포트.              *
*                Nashinho's 8th Report Program ssi-ik ^^;              *
*&---------------------------------------------------------------------*
*& [변경내용]  :                                                       *
*&
*&---------------------------------------------------------------------*

REPORT  ZRIMAMDREQ   MESSAGE-ID ZIM NO STANDARD PAGE HEADING
                     LINE-SIZE 120.

TABLES : ZTREQHD_TMP,                  " 수입의뢰 Header Amend용 Temp..
         ZTMLCAMNARR,                  " MLC Amend 기타조건변경사항..
         ZTMLCAMHD,                    " Master L/C Amend Header..
         ZTREQHD,                      " 수입의뢰 Header Table..
*         ZTREQIT,                      " 수입의뢰 Item   Table..
         ZTREQST,                      " 수입의뢰 상태   Table..
         ZTMLCHD,                      " Master L/C Header Table..
         ZTMLCSG2.                     " Master L/C Seg 2..

*>>> 수입의뢰 Header Amend용 Temp Table Declaration.
DATA: BEGIN OF IT_REQ_TMP OCCURS 1000,
        ZFREQNO    LIKE  ZTREQHD_TMP-ZFREQNO,  " 수입의뢰관리번호..
        ZFAMDNO    LIKE  ZTREQHD_TMP-ZFAMDNO,  " Amend Seq..
        ZFREQED    LIKE  ZTREQHD_TMP-ZFREQED,  " 의뢰 유효일..
        ZFLASTAM   LIKE  ZTREQHD_TMP-ZFLASTAM, " 최종개설금액..
        WAERS      LIKE  ZTREQHD_TMP-WAERS,    " 통화키..
      END OF IT_REQ_TMP.

*>>> Master L/C Amend Header Table Declaration.
DATA: BEGIN OF IT_MLCAMD OCCURS 1000,
        ZFREQNO    LIKE  ZTMLCAMHD-ZFREQNO,    " 수입의뢰관리번호..
        ZFAMDNO    LIKE  ZTMLCAMHD-ZFAMDNO,    " Amend Seq..
        ZFNEXDT    LIKE  ZTMLCAMHD-ZFNEXDT,    " 유효기일변경..
        ZFIDCD     LIKE  ZTMLCAMHD-ZFIDCD,     " 금액변동 구분..
        ZFIDAM     LIKE  ZTMLCAMHD-ZFIDAM,     " 금액변동분.
        WAERS      LIKE  ZTMLCAMHD-WAERS,
        ZFNDAMT    LIKE  ZTMLCAMHD-ZFNDAMT,    " 변경후 최종금액..
        ZFALCQ     LIKE  ZTMLCAMHD-ZFALCQ,     " 과부족허용율 사용여부..
        ZFALCP     LIKE  ZTMLCAMHD-ZFALCP,     " 과부족허용율..
        ZFNSPRT    LIKE  ZTMLCAMHD-ZFNSPRT,    " 선적항변경..
        ZFNAPRT    LIKE  ZTMLCAMHD-ZFNAPRT,    " 도착항변경..
        ZFNLTSD    LIKE  ZTMLCAMHD-ZFNLTSD,    " 최종선적일 변경..
        ZFBENI1    LIKE  ZTMLCAMHD-ZFBENI1,    " 수익자 상호/주소 1..
        ZFBENI2    LIKE  ZTMLCAMHD-ZFBENI2,    " 수익자 상호/주소 2..
        ZFBENI3    LIKE  ZTMLCAMHD-ZFBENI3,    " 수익자 상호/주소 3..
        ZFBENI4    LIKE  ZTMLCAMHD-ZFBENI4,    " 수익자 상호/주소 4..
*>>> DF=Full Cable, DG=Short Cable, DD=Air Mail..
        ZFOPME     LIKE  ZTMLCAMHD-ZFOPME,     " 개설방법..
      END OF IT_MLCAMD.

*>>> 수입의뢰 Header Table Declaration.
DATA: BEGIN OF IT_REQHD OCCURS 1000,
        ZFREQNO    LIKE  ZTREQHD-ZFREQNO,      " 수입의뢰관리번호..
        ZFREQED    LIKE  ZTREQHD-ZFREQED,      " 의뢰유효일..
      END OF IT_REQHD.

*>>> Master L/C Header Table Declaration.
DATA: BEGIN OF IT_MLCHD OCCURS 1000,
        ZFREQNO    LIKE  ZTMLCHD-ZFREQNO,      " 수입의뢰관리번호..
        ZFOBNM     LIKE  ZTMLCHD-ZFOBNM,       " 개설은행명..
      END OF IT_MLCHD.

*DATA: BEGIN OF IT_REQIT OCCURS 1000,


*>>> 수입의뢰 상태(Status) Table Declaration.
DATA: BEGIN OF IT_REQST OCCURS 1000,
        ZFREQNO    LIKE  ZTREQST-ZFREQNO,      " 수입의뢰관리번호..
        ZFAMDNO    LIKE  ZTREQST-ZFAMDNO,      " Amend 회차..
        ZFOPNNO    LIKE  ZTREQST-ZFOPNNO,      " 신용장-승인번호..
        ZFAPPDT    LIKE  ZTREQST-ZFAPPDT,      " 변경예정일..
        ZFOPNDT    LIKE  ZTREQST-ZFOPNDT,      " 개설일..
        ZFDOCST    LIKE  ZTREQST-ZFDOCST,      " 문서상태..
      END OF IT_REQST.

*>>> Master L/C Amend 기타조건 변경사항 Table Declaration.
DATA: BEGIN OF IT_MLCAMNARR OCCURS 1000,
        ZFREQNO     LIKE  ZTMLCAMNARR-ZFREQNO, " 수입의뢰관리번호..
        ZFAMDNO     LIKE  ZTMLCAMNARR-ZFAMDNO, " Amend 회차..
        ZFLNARR     LIKE  ZTMLCAMNARR-ZFLNARR, " Seq 기타조건변경사항..
        ZFNARR      LIKE  ZTMLCAMNARR-ZFNARR,  " 기타조건변경사항..
*        ZFFIELD     LIKE  ZTMLCAMNARR-ZFFIELD, " 필드이름..
      END OF IT_MLCAMNARR.

DATA: BEGIN OF IT_MLCSG2 OCCURS 1000,
        ZFREQNO     LIKE  ZTMLCSG2-ZFREQNO,    " 수입의뢰관리번호..
*        ZFELEAD1    LIKE  ZTMLCSG2-ZFELEAD1,   " App 전자서명 주소1..
*        ZFELEAD2    LIKE  ZTMLCSG2-ZFELEAD2,   " App 전자서명 주소2..
        ZFAPPNM     LIKE  ZTMLCSG2-ZFAPPNM,    " App 상호/성명..
        ZFTELNO     LIKE  ZTMLCSG2-ZFTELNO,    " Applicant 전화번호..
        ZFAPPAD1    LIKE  ZTMLCSG2-ZFAPPAD1,   " Applicant 주소1..
        ZFAPPAD2    LIKE  ZTMLCSG2-ZFAPPAD2,   " Applicant 주소2..
      END OF IT_MLCSG2.

DATA: TEMP    LIKE  ZTREQST-ZFAMDNO,
      LINE    TYPE  I.
*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.

*   SELECT-OPTIONS:
*      S_REQNO  FOR  ZTREQHD-ZFREQNO NO INTERVALS NO-EXTENSION,
*      S_AMDNO  FOR  ZTREQST-ZFAMDNO NO INTERVALS NO-EXTENSION.
*      NO INTERVALS NO-EXTENSION OBLIGATORY
   PARAMETERS: P_REQNO  LIKE  ZTREQHD-ZFREQNO,
               P_AMDNO  LIKE  ZTREQST-ZFAMDNO.
SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* INITIALIZATION: 검색화면 직전에 발생하는 이벤트..
*-----------------------------------------------------------------------
INITIALIZATION.
   SET TITLEBAR 'TIT1'.          " Title: '수입신용장조건변경신청서'.

*-----------------------------------------------------------------------
* START-OF-SELECTION: 검색화면을 구성하기위한 이벤트..
*-----------------------------------------------------------------------
START-OF-SELECTION.

   PERFORM P1000_READ_MLCHD_DATA.     " Master L/C Header..
   PERFORM P1000_READ_MLCAMD_DATA.    " Master L/C Amend Header..
   PERFORM P1000_READ_MLCAMNARR_DATA. " Master L/C Amend 기타조건변경사.
   PERFORM P1000_READ_REQHD_DATA.     " 수입의뢰 Header..
   PERFORM P1000_READ_REQST_DATA.     " 수입의뢰 상태(Status)..
   PERFORM P1000_READ_REQ_TEMP_DATA.  " 수입의뢰 Header Amend용 Temp..
   PERFORM P1000_READ_MLCSG2_DATA.

*-----------------------------------------------------------------------
* END-OF-SELECTION.
*-----------------------------------------------------------------------
END-OF-SELECTION.

   SET TITLEBAR 'TIT1'.
   PERFORM P3000_WRITE_MLCAMD_DATA.
*   PERFORM P3000_WRITE_MLCAMNARR_DATA.
*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
AT USER-COMMAND.


*-----------------------------------------------------------------------
* From Now On Only Perform Statement Will Be Appeared.
*-----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MLCHD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_MLCHD_DATA.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MLCHD
            FROM ZTMLCHD
           WHERE ZFREQNO EQ P_REQNO.

ENDFORM.                    " P1000_READ_MLCHD_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MLCAMD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_MLCAMD_DATA.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MLCAMD
            FROM ZTMLCAMHD
           WHERE ZFREQNO EQ P_REQNO
             AND ZFAMDNO EQ P_AMDNO.
ENDFORM.                    " P1000_READ_MLCAMD_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MLCAMNARR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_MLCAMNARR_DATA.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MLCAMNARR
            FROM ZTMLCAMNARR
           WHERE ZFREQNO EQ P_REQNO
             AND ZFAMDNO EQ P_AMDNO.
ENDFORM.                    " P1000_READ_MLCAMNARR_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_REQHD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_REQHD_DATA.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REQHD
            FROM ZTREQHD
           WHERE ZFREQNO EQ P_REQNO.

ENDFORM.                    " P1000_READ_REQHD_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_REQST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_REQST_DATA.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REQST
            FROM ZTREQST
           WHERE ZFREQNO EQ P_REQNO.

ENDFORM.                    " P1000_READ_REQST_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_REQ_TEMP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_REQ_TEMP_DATA.

   TEMP = P_AMDNO - 1.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REQ_TMP
            FROM ZTREQHD_TMP
           WHERE ZFREQNO EQ P_REQNO
             AND ZFAMDNO EQ TEMP.

ENDFORM.                    " P1000_READ_REQ_TEMP_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MLCSG2_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_MLCSG2_DATA.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MLCSG2
            FROM ZTMLCSG2
           WHERE ZFREQNO EQ P_REQNO.

ENDFORM.                    " P1000_READ_MLCSG2_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_MLCAMD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_MLCAMD_DATA.

LOOP AT IT_MLCAMD.
   READ TABLE IT_REQST      WITH KEY ZFREQNO = IT_MLCAMD-ZFREQNO
                                     ZFAMDNO = IT_MLCAMD-ZFAMDNO.
   READ TABLE IT_REQHD      WITH KEY ZFREQNO = IT_MLCAMD-ZFREQNO.
   READ TABLE IT_REQ_TMP    WITH KEY ZFREQNO = IT_MLCAMD-ZFREQNO
                                     ZFAMDNO = TEMP.
   READ TABLE IT_MLCHD      WITH KEY ZFREQNO = IT_MLCAMD-ZFREQNO.
   READ TABLE IT_MLCSG2     WITH KEY ZFREQNO = IT_MLCAMD-ZFREQNO.
   READ TABLE IT_MLCAMD     WITH KEY ZFREQNO = IT_MLCAMD-ZFREQNO
                                     ZFAMDNO = TEMP.
   READ TABLE IT_MLCAMNARR  WITH KEY ZFREQNO = IT_MLCAMD-ZFREQNO
                                     ZFAMDNO = IT_MLCAMD-ZFAMDNO.
   SKIP 2.
   WRITE: /50 '수입신용장조건변경신청서' NO-GAP,
          /30 'APPLICATION FOR AMENDMENT TO ' NO-GAP,
              'IREEVOCABLE DOCUMENTARY CREDIT' NO-GAP.

   SKIP 2.
   WRITE: / 'TO : ' NO-GAP, IT_MLCHD-ZFOBNM,
         91 'DATE : ' NO-GAP, 110 IT_REQST-ZFAPPDT NO-GAP.

   READ TABLE IT_REQST      WITH KEY ZFREQNO = IT_MLCAMD-ZFREQNO
                                     ZFAMDNO = TEMP.

   SKIP 2.
   WRITE: / 'CREDIT NO.  : ' NO-GAP, IT_REQST-ZFOPNNO NO-GAP,
         82 'DATE OF ISSUE : ' NO-GAP, 110 IT_REQST-ZFOPNDT NO-GAP.
   SKIP.
   WRITE: / 'EXPIRY DATE : ' NO-GAP, IT_REQ_TMP-ZFREQED NO-GAP,
         89 'AMOUNT : ' NO-GAP, IT_REQ_TMP-WAERS NO-GAP,
             IT_REQ_TMP-ZFLASTAM CURRENCY IT_REQ_TMP-WAERS.
   SKIP.
   WRITE: / 'BENEFICIARY : ' NO-GAP, IT_MLCAMD-ZFBENI1 NO-GAP.

   READ TABLE IT_MLCAMD     WITH KEY ZFREQNO = IT_MLCAMD-ZFREQNO
                                     ZFAMDNO = IT_MLCAMD-ZFAMDNO.

   SKIP 2.
   WRITE: / 'We request you to amend by ' NO-GAP.
      IF IT_MLCAMD-ZFOPME EQ 'DF'.
         WRITE: 'Full Cable' NO-GAP.
      ELSEIF IT_MLCAMD-ZFOPME EQ 'DG'.
         WRITE: 'Short Cable' NO-GAP.
      ELSE.
         WRITE: 'Airmail' NO-GAP.
      ENDIF.
   WRITE: ' the Captioned documentary credit' NO-GAP.

   SKIP 2.

*>>> 유효기일 관련항목을 표시하기 위한 구문..
   IF NOT IT_MLCAMD-ZFNEXDT IS INITIAL.
      WRITE: / '■ Expiry date of credit extended to : ' NO-GAP,
               IT_MLCAMD-ZFNEXDT NO-GAP.
   ELSE.
      WRITE: / '□ Expiry date of credit extended to : ' NO-GAP.
   ENDIF.
   SKIP.
*>>>개설금액 관련항목을 표시하기 위한 구문..
   IF IT_MLCAMD-ZFIDCD EQ '+'.
      WRITE: / '■ Increase of L/C amount : ' NO-GAP,
               IT_MLCAMD-ZFIDAM CURRENCY IT_MLCAMD-WAERS NO-GAP.
      SKIP.
      WRITE: / '□ Decrease of L/C amount : ' NO-GAP.
      SKIP.
      WRITE: / '■ New credit amount after amendment : ' NO-GAP,
               IT_MLCAMD-ZFNDAMT CURRENCY IT_MLCAMD-WAERS NO-GAP.
   ELSEIF IT_MLCAMD-ZFIDCD EQ '-'.
      WRITE: / '□ Increase of L/C amount : ' NO-GAP.
      SKIP.
      WRITE: / '■ Decrease of L/C amount : ' NO-GAP,
               IT_MLCAMD-ZFIDAM  CURRENCY IT_MLCAMD-WAERS NO-GAP.
      SKIP.
      WRITE: / '■ New credit amount after amendment : ' NO-GAP,
               IT_MLCAMD-ZFNDAMT  CURRENCY IT_MLCAMD-WAERS NO-GAP.
   ELSE.
      WRITE: / '□ Increase of L/C amount : ' NO-GAP.
      SKIP.
      WRITE: / '□ Decrease of L/C amount : ' NO-GAP.
      SKIP.
      WRITE: / '□ New credit amount after amendment : ' NO-GAP.

   ENDIF.
   SKIP.
*>>> 개설금액 관련항목(과부족허용률)을 표시하기 위한 구문..
   IF IT_MLCAMD-ZFALCQ EQ 'T'.
      WRITE: / '■ New percentage credit after amendment : ' NO-GAP,
               '(' NO-GAP, IT_MLCAMD-ZFALCP NO-GAP, '% ' NO-GAP,
               'Plus/Minus' NO-GAP, ')' NO-GAP.
   ELSEIF IT_MLCAMD-ZFALCQ EQ 'X'.
      WRITE: / '■ New percentage credit after amendment : ' NO-GAP,
               '(' NO-GAP, IT_MLCAMD-ZFALCP NO-GAP, '% ' NO-GAP,
               'Maximum' NO-GAP, ')' NO-GAP.
   ELSEIF IT_MLCAMD-ZFALCQ EQ '2AA'.
      WRITE: / '■ New percentage credit after amendment : ' NO-GAP,
               '(' NO-GAP, IT_MLCAMD-ZFALCP NO-GAP, '% ' NO-GAP,
               'Up To' NO-GAP, ')' NO-GAP.
   ELSEIF IT_MLCAMD-ZFALCQ EQ '2AB'.
      WRITE: / '■ New percentage credit after amendment : ' NO-GAP,
               '(' NO-GAP, IT_MLCAMD-ZFALCP NO-GAP, '% ' NO-GAP,
               'Not Exceeding' NO-GAP, ')' NO-GAP.
   ELSE.
      WRITE: / '□ New percentage credit after amendment : ' NO-GAP.

   ENDIF.
   SKIP.
*>>> 선적항, 도착항 변경여부를 표시하기 위한 구문..
   IF NOT IT_MLCAMD-ZFNSPRT IS INITIAL.
      WRITE: / '■ New shipment from : ' NO-GAP,
               IT_MLCAMD-ZFNSPRT NO-GAP.
   ELSE.
      WRITE: / '□ New shipment from : ' NO-GAP.
   ENDIF.
   SKIP.
   IF NOT IT_MLCAMD-ZFNAPRT IS INITIAL.
      WRITE: / '■ New shipment to : ' NO-GAP,
               IT_MLCAMD-ZFNAPRT NO-GAP.
   ELSE.
      WRITE: / '□ New shipment to : ' NO-GAP.
   ENDIF.
   SKIP.
*>>> 문서상태가 취소인지 여부를 표시하기 위한 구문..
   IF IT_REQST-ZFDOCST EQ 'C'.
      WRITE: / '■ Credit is cancelled subjected ' NO-GAP,
               'to beneficiary`s consent : ' NO-GAP.

   ELSE.
      WRITE: / '□ Credit is cancelled subjected ' NO-GAP,
               'to beneficiary`s consent : ' NO-GAP.
   ENDIF.
   SKIP.
*>>> 기타 변동사항을 표시하기 위한 구문..
   IF NOT IT_MLCAMNARR-ZFNARR IS INITIAL.
      WRITE: / '■ Other amendments' NO-GAP.
   ELSE.
      WRITE: / '□ Other amendments' NO-GAP.
   ENDIF.

   LOOP AT IT_MLCAMNARR.
      SORT IT_MLCAMNARR BY ZFNARR.
      LINE = 0.
      ULINE.
      WRITE: / SY-VLINE NO-GAP, '  ' NO-GAP,IT_MLCAMNARR-ZFNARR NO-GAP,
           120 SY-VLINE NO-GAP.
      LINE = LINE + 1.
   ENDLOOP.

   WHILE LINE LT 15.
      WRITE: / SY-VLINE NO-GAP, 120 SY-VLINE NO-GAP.
      LINE = LINE + 1.
   ENDWHILE.
   ULINE.

   SKIP 5.
   WRITE: /50 '주    소 : ' NO-GAP, IT_MLCSG2-ZFAPPAD1 NO-GAP.
   IF NOT IT_MLCSG2-ZFAPPAD2 IS INITIAL.
      WRITE: /61 IT_MLCSG2-ZFAPPAD2 NO-GAP, 105 SY-ULINE(15).
   ELSE.
      WRITE /105 SY-ULINE(15).
   ENDIF.


   WRITE: /50 '신 청 인 : ' NO-GAP, IT_MLCSG2-ZFAPPNM NO-GAP,
          105 SY-VLINE NO-GAP, '  인감대조  ' NO-GAP,
          119 SY-VLINE NO-GAP,
         /105 SY-ULINE(15).
   WRITE: /50 '전화번호 : ' NO-GAP, IT_MLCSG2-ZFTELNO NO-GAP,
          105 SY-VLINE NO-GAP, 119 SY-VLINE,
         /105 SY-VLINE NO-GAP, 119 SY-VLINE,
         /105 SY-VLINE NO-GAP, 119 SY-VLINE,
         /105 SY-ULINE(15).
ENDLOOP.

ENDFORM.                    " P3000_WRITE_MLCAMD_DATA
