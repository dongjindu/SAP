*&---------------------------------------------------------------------*
*& Report  ZRIMFLAT_DISP                                               *
*&---------------------------------------------------------------------*
*&  프로그램명 : FLAT TABLE 조회용 Report Program                      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.03.15                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMFLAT_DISP MESSAGE-ID ZIM
                      LINE-SIZE 83
                      NO STANDARD PAGE HEADING.

TABLES : ZTDDF1,     " 표준 EDI FLAT DETAIL
         ZTDHF1.     " 표준 EDI FLAT HEAD

*-----------------------------------------------------------------------
* INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 500,
       ZFDDSEQ    LIKE ZTDDF1-ZFDDSEQ,           " 연?
       W_GB01(1)  TYPE C VALUE ';',
       ZFDDFDA    LIKE ZTDDF1-ZFDDFDA,           " DATA
       W_GB02(1)  TYPE C VALUE ';'.
DATA : END OF IT_TAB.



*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include

*-----------------------------------------------------------------------
* DATA DEFINE
*-----------------------------------------------------------------------
DATA : W_ERR_CHK         TYPE C,
       W_LINE            TYPE I,
       W_MOD             TYPE I.
*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   PARAMETERS:     P_DDENO   LIKE ZTDDF1-ZFDDENO.  " Purchasing document
SELECTION-SCREEN END OF BLOCK B1.

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
* 테이블 SELECT
   PERFORM   P1000_GET_ZTDDF1          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN OTHERS.
   ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM04'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /35  '[ FLAT DATA 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,
            ' Seq. ',      SY-VLINE NO-GAP,
            'data' CENTERED,  83 SY-VLINE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTDDF1
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTDDF1   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB FROM ZTDDF1
                             WHERE ZFDDENO    EQ     P_DDENO.

  IF SY-SUBRC NE 0.               " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S993.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_ZTDDF1
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIM04'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM04'.           " GUI TITLE SETTING..

   W_LINE = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      W_MOD = SY-TABIX MOD 2.
      IF W_MOD EQ 1.
         FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      ELSE.
         FORMAT COLOR COL_NORMAL INTENSIFIED ON.
      ENDIF.
      WRITE : / SY-VLINE, IT_TAB-ZFDDSEQ,
                SY-VLINE, IT_TAB-ZFDDFDA, SY-VLINE.

      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.

   ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_LINE GT 0.
     WRITE : / SY-ULINE.
     FORMAT RESET.
     WRITE : / '총', W_LINE, '건'.
  ENDIF.


ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

ENDFORM.                    " RESET_LIST
