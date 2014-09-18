*&---------------------------------------------------------------------*
*& Report  ZRIMTCCRT
*&---------------------------------------------------------------------*
*&  프로그램명 : 세금계산서 조회                                       *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2000.11.13                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*


REPORT  ZRIMTCCRT    MESSAGE-ID ZIM
                     LINE-SIZE 116
                     NO STANDARD PAGE HEADING.
TABLES: ZTVT, ZTVTSG1,ZTVTSG3.
*-----------------------------------------------------------------------
* 세금계산서  INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFVTNO  LIKE ZTVT-ZFVTNO ,   " 세금계산서 관리번호"
       ZFTXAMT LIKE ZTVTSG3-ZFTXAMT," 세액
*       ZFVERC  LIKE ZTVTSG3-ZFVERC, " 공란수.
       ZFSUDT  LIKE ZTVTSG3-ZFSUDT, " 공급일"
*       ZFVCDT  LIKE ZTVTSG3-ZFVCDT, " 작성일.
       ZFGONM  LIKE ZTVTSG3-ZFGONM, " 품목명"
       ZFKRW   LIKE ZTVTSG3-ZFKRW,
       ZFGOSD1 LIKE ZTVTSG3-ZFGOSD1," 규격1
       ZFGOSD2 LIKE ZTVTSG3-ZFGOSD2," 규격2
       ZFGOSD3 LIKE ZTVTSG3-ZFGOSD3,
       ZFGOSD4 LIKE ZTVTSG3-ZFGOSD4,
       ZFREMK1 LIKE ZTVTSG3-ZFREMK1," 참조사항"
       ZFREMK2 LIKE ZTVTSG3-ZFREMK2,
       ZFREMK3 LIKE ZTVTSG3-ZFREMK3,
       ZFREMK4 LIKE ZTVTSG3-ZFREMK4,
       ZFREMK5 LIKE ZTVTSG3-ZFREMK5,
       ZFQUN   LIKE ZTVTSG3-ZFQUN,  " 수량"
       ZFQUNM  LIKE ZTVTSG3-ZFQUNM, " 수량단위"
       ZFQUNSM  LIKE ZTVTSG3-ZFQUNSM," 단위"
       NETPR   LIKE ZTVTSG3-NETPR,  " 단가"
       PEINH   LIKE ZTVTSG3-PEINH,  " 단가 단위"
       BPRME   LIKE ZTVTSG3-BPRME,  " Order price unit
       ZFSAMK  LIKE ZTVTSG3-ZFSAMK, " 공급가액 원화"
       ZFSAMF  LIKE ZTVTSG3-ZFSAMF, " 공급가액 외화"
       ZFEXRT  LIKE ZTVTSG3-ZFEXRT. "  환율"
DATA : END OF IT_TAB.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,               " 선택 LINE COUNT
       W_PAGE            TYPE I,               " Page Counter
       W_LINE            TYPE I,               " 페이지당 LINE COUNT
       LINE(3)           TYPE N,               " 페이지당 LINE COUNT
       W_COUNT           TYPE I,               " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME, " 필드명.
       W_SUBRC           LIKE SY-UCOMM,
       W_DOM_TEX1        LIKE DD07T-DDTEXT,
       W_TABIX           LIKE SY-TABIX.    " TABLE INDEX

*-----------------------------------------------------------------------
* Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 2.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
 PARAMETERS : P_ZFVTNO  LIKE ZTVT-ZFVTNO      "관리번호 "
              MEMORY ID ZPVTNO.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.                    " 초기값 SETTING
   SET  TITLEBAR 'ZIMA9'.          " TITLE BAR
*title Text Write
TOP-OF-PAGE.
 PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

*  테이블 SELECT
   PERFORM   P1000_READ_DATA.
  IF SY-SUBRC NE 0.               " Not Found?
   MESSAGE S738.    EXIT.
  ENDIF.
* 레포트 Write
    PERFORM   P3000_DATA_WRITE.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.

     WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.


ENDFORM.                    " P3000_TITLE_WRITE


*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA.
** ZTVT

  SELECT  SINGLE *
     FROM ZTVT
    WHERE ZFVTNO = P_ZFVTNO.

 IF SY-SUBRC NE 0.               " Not Found?
         W_ERR_CHK = 'Y'.  MESSAGE S966.    EXIT.
  ENDIF.
  SELECT  SINGLE *
     FROM ZTVTSG1
    WHERE ZFVTNO = P_ZFVTNO.

** ZTVTSG3
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
     FROM ZTVTSG3
    WHERE ZFVTNO = P_ZFVTNO.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE .

  SET PF-STATUS 'ZIMA9'.
  SET  TITLEBAR 'ZIMA9'.          " TITLE BAR

  SORT IT_TAB BY ZFVTNO.
  PERFORM P3000_LINE_WRITE.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  SKIP 2.
  WRITE :/40  ' [ 세 금 계 산 서  ] '.
  WRITE :/45  ' (공급자용) '.
  WRITE :/85 'Date : ', SY-DATUM.
  SKIP 3.
  WRITE :/  '전자문서번호     :', ZTVT-ZDREINO.
  WRITE :/  '책번호           :',  ZTVT-ZFVTKW.
  WRITE :/  '호번호           :',  ZTVT-ZFVTHO.
  WRITE :/  '일련번호         :',  ZTVT-ZFVTSEQ.
  WRITE :/  '관련참조번호     :',  ZTVT-ZFVTRNO.
  WRITE :/  '(-)세금계산서번호:',  ZTVTSG1-ZFVTNO.
  SKIP 2.
  WRITE : / '공급자           :',20  '<등록번호>',ZTVTSG1-ZFTXN1.
  WRITE :                 /20  '<상 호>', ZTVTSG1-ZFCONM1.
  WRITE :                 /20  '<대표자명>',ZTVTSG1-ZFCHNM1.
  IF NOT ZTVTSG1-ZFADD11 IS INITIAL OR
     NOT ZTVTSG1-ZFADD21 IS INITIAL OR
     NOT ZTVTSG1-ZFADD31 IS INITIAL.
     WRITE : /20  '<주   소>'.
     IF NOT ZTVTSG1-ZFADD11 IS INITIAL.
        WRITE : 32  ZTVTSG1-ZFADD11.
     ENDIF.
     IF NOT ZTVTSG1-ZFADD21 IS INITIAL.
        WRITE : /32 ZTVTSG1-ZFADD21.
     ENDIF.
     IF NOT ZTVTSG1-ZFADD31 IS INITIAL.
        WRITE :/32  ZTVTSG1-ZFADD31.
     ENDIF.
  ENDIF.
  IF NOT ZTVTSG1-ZFLEID1 IS INITIAL.
     WRITE : /20  '<전자서명>',ZTVTSG1-ZFLEID1.
  ENDIF.
  SKIP 2.
  WRITE : / '공급받는자       :',20  '<등록번호>',ZTVTSG1-ZFTXN2,
                          /20  '<상    호>',ZTVTSG1-ZFCONM2,
                          /20  '<대표자명>',ZTVTSG1-ZFCHNM2.
  IF NOT ZTVTSG1-ZFADD12 IS INITIAL OR
     NOT ZTVTSG1-ZFADD22 IS INITIAL OR
     NOT ZTVTSG1-ZFADD32 IS INITIAL.
     WRITE:/20  '<주    소>'.
     IF NOT ZTVTSG1-ZFADD12 IS INITIAL.
        WRITE: 32 ZTVTSG1-ZFADD12.
     ENDIF.
     IF NOT ZTVTSG1-ZFADD22 IS INITIAL.
        WRITE:/32  ZTVTSG1-ZFADD22.
     ENDIF.
     IF NOT ZTVTSG1-ZFADD32 IS INITIAL.
        WRITE:/32  ZTVTSG1-ZFADD32.
     ENDIF.
     IF NOT ZTVTSG1-ZFTXN2 IS INITIAL.
        WRITE:/20 '<전자서명>',ZTVTSG1-ZFTXN2.
     ENDIF.
  ENDIF.
   SKIP 1.
  IF NOT ZTVTSG1-ZFTXN3 IS INITIAL.
         WRITE : / '수 탁 자       :',20  '<등록번호>',ZTVTSG1-ZFTXN3,
                               /20  '<상    호>',ZTVTSG1-ZFCONM3,
                               /20  '<대표자명>',ZTVTSG1-ZFCHNM3.
  ENDIF.
  IF NOT ZTVTSG1-ZFADD13 IS INITIAL OR
     NOT ZTVTSG1-ZFADD23 IS INITIAL OR
     NOT ZTVTSG1-ZFADD33 IS INITIAL.
     WRITE:/20  '<주    소>'.
     IF NOT ZTVTSG1-ZFADD13 IS INITIAL.
        WRITE: 32 ZTVTSG1-ZFADD13.
     ENDIF.
     IF NOT ZTVTSG1-ZFADD22 IS INITIAL.
        WRITE:/32  ZTVTSG1-ZFADD23.
     ENDIF.
     IF NOT ZTVTSG1-ZFADD32 IS INITIAL.
        WRITE:/32  ZTVTSG1-ZFADD33.
     ENDIF.
     IF NOT ZTVTSG1-ZFTXN3 IS INITIAL.
        WRITE:/20 '<전자서명>',ZTVTSG1-ZFTXN3.
     ENDIF.
  ENDIF.

  WRITE: / SY-ULINE ,50 '< 품     목 >'.
  SKIP 1.
  WRITE :/(20) '품     명',
          (15) '참조사항',
          (19) '수       량' RIGHT-JUSTIFIED,
          (19) '원화공급가액' RIGHT-JUSTIFIED,
          (15) '세       액' RIGHT-JUSTIFIED .
  WRITE: /(20) '규     격',
          (15) '공 급 일',
          (19)'(단가/기준)'RIGHT-JUSTIFIED,
          (19) '외화공급가액'RIGHT-JUSTIFIED,
          (15) '환       율'RIGHT-JUSTIFIED.

  LOOP AT IT_TAB.
       WRITE :/(20) IT_TAB-ZFGONM, " 품목명.
               (15) IT_TAB-ZFREMK1," 참조사항.
               (15) IT_TAB-ZFQUN
                    UNIT IT_TAB-ZFQUNM,      " 수량.
                    IT_TAB-ZFQUNM,
               (15) IT_TAB-ZFSAMK
                    CURRENCY IT_TAB-ZFKRW,   " 공급가액.
               (03) IT_TAB-ZFKRW,
               (15) IT_TAB-ZFTXAMT
                   CURRENCY IT_TAB-ZFKRW.   " 세액.

       WRITE :/(20) IT_TAB-ZFGOSD1," 규격1.
               (15) IT_TAB-ZFSUDT, " 공급일.
               (15) IT_TAB-NETPR
                        CURRENCY ZTVT-ZFTSAMFC,
                             IT_TAB-BPRME,
               (15) IT_TAB-ZFSAMF
                    CURRENCY ZTVT-ZFTSAMFC,        " 공급가액외.
                     (03) ZTVT-ZFTSAMFC,
                80  IT_TAB-ZFEXRT RIGHT-JUSTIFIED. " 환율.

  ENDLOOP.

  SKIP 2.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDVPTB' ZTVT-ZFVPTB
                                        CHANGING   W_DOM_TEX1.

  WRITE :/ '총공급가액  :',ZTVT-ZFVTAMT CURRENCY ZTVT-ZFKRW
                            LEFT-JUSTIFIED.

  WRITE :/ '총수량      :',ZTVT-ZFTQUN UNIT ZTVT-ZFTQUNM
                           LEFT-JUSTIFIED.
  WRITE :/ '총금액      :',ZTVT-ZFTOTAM CURRENCY ZTVT-ZFKRW
                           LEFT-JUSTIFIED,
         / '결제방법    :', (08)W_DOM_TEX1.
  IF NOT ZTVT-ZFVPAMK IS INITIAL.
     WRITE:/'결제금액    :', ZTVT-ZFKRW,ZTVT-ZFVPAMK
                             CURRENCY ZTVT-ZFKRW
                             LEFT-JUSTIFIED.
  ENDIF.
  IF NOT ZTVT-ZFVPAMF IS INITIAL.
     WRITE:/'            :', ZTVT-ZFTSAMFC,
                             ZTVT-ZFVPAMF CURRENCY ZTVT-ZFTSAMFC
                             LEFT-JUSTIFIED.
  ENDIF.
  SKIP 1.
  WRITE: / SY-ULINE .
  SKIP 1.
  WRITE:/ '이 금액을 청구함.',
        / '*본 계산서는 부가가치세법 시행령 제53 조 3항및 4항의 규정에',
      '의하여 교부한 세금계산서의 내역을 출력한 것입니다.'.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_TO_PC_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P3000_TO_PC_DOWNLOAD.

   CALL FUNCTION 'DOWNLOAD'
        EXPORTING
        FILENAME = 'C:\TEMP\TEMP.XLS'
        FILETYPE = 'ASC'
   TABLES
       DATA_TAB = IT_TAB.

ENDFORM.                    " P3000_TO_PC_DOWNLOAD
