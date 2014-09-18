*&---------------------------------------------------------------------*
*& Report  ZRIMFINLST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 :계산서(FINBIL) EDI 수신현황                            *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.03                                            *
*$     적용회사: LG 화학
*&---------------------------------------------------------------------*
*&   DESC.     :
*& EDI 수신내역을 조회 한후(수입의뢰)제비용처리를 한다.
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMFINLST   MESSAGE-ID ZIM
                     LINE-SIZE 153
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------
 INCLUDE   <ICON>.
 INCLUDE   ZRIMFINTOP.
*INCLUDE   ZRIMSORTCOM.    " Report Sort를 위한 Include
*INCLUDE   ZRIMUTIL01.     " Utility function 모음.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

   SELECT-OPTIONS:  S_BUKRS  FOR  ZTBKPF-BUKRS
                                  NO-EXTENSION
                                  NO INTERVALS
                                  OBLIGATORY ,
                    S_RCDT	  FOR  ZTFINHD-ZFRCDT,	  " 수신일.
                    S_BKCD	  FOR  ZTFINHD-ZFBKCD,	  " 은행코드.
                    S_TERM	  FOR  ZTFINHD-ZFTERM,        " 결제방법.
                    S_NOTE	  FOR  ZTFINHD-ZFNOTE.        " 어음조건.
   SELECTION-SCREEN SKIP 1.
   PARAMETERS : P_NOT AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
   PARAMETER P_HKONT     LIKE ZTBKPF-HKONT
                              OBLIGATORY MATCHCODE OBJECT KRED.
SELECTION-SCREEN END OF BLOCK B3.


SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-003.
* SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(14) TEXT-003.",POSITION 1.
     SELECTION-SCREEN : BEGIN OF LINE,
                        COMMENT 17(15) TEXT-021, POSITION 33.
       PARAMETERS : P_NON    AS CHECKBOX.              " 미반영자료포함.
     SELECTION-SCREEN : COMMENT 43(14) TEXT-022, POSITION 58.
        PARAMETERS : P_YES   AS CHECKBOX.               " 반영자료포함.
     SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.


* Title Text Write
TOP-OF-PAGE.
   IF INCLUDE NE 'POPU'.
      PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
   ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 권한 검증 함수.
*   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
*   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*  사원벤더 검증  2002.02.21 김영광 추가
   PERFORM   P2000_VERIFY_HKONT.

*  테이블 SELECT
   PERFORM   P1000_GET_ZTFINBILL     USING   W_ERR_CHK.
   IF W_ERR_CHK = 'Y'. MESSAGE S738.   EXIT.  ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
   CASE SY-UCOMM.
** SORT 선택.
*      WHEN 'STUP' OR 'STDN'.         " SORT 선택.
*         W_FIELD_NM = 'ZFOPBN'.
*         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
*         PERFORM HANDLE_SORT TABLES  IT_TAB
*                             USING   SY-UCOMM.
** 전체 선택 및 선택해제.
      WHEN 'MKAL' OR 'MKLO'.          " 전체 선택 및 선택해제.
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISP'.                    " L/C 조회.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P4000_VALID_CHECK USING  W_ERR_CHK.
            IF W_ERR_CHK = 'Y'.   EXIT.  ENDIF.
            PERFORM P2000_SHOW_LC USING  IT_SELECTED-ZFREQNO
                                         IT_SELECTED-ZFAMDNO.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
       WHEN 'ZIMY3'.                    " 비용문서조회.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P4000_VALID_CHECK USING  W_ERR_CHK.
            IF W_ERR_CHK = 'Y'.   EXIT.  ENDIF.
            PERFORM P2000_SHOW_COST USING  IT_SELECTED-ZFDHENO.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
       WHEN 'DELE'.                    " 비용문서조회.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
            PERFORM P2000_DELETE_ZTFINHD USING W_ERR_CHK.
         ENDIF.
         PERFORM   P1000_GET_ZTFINBILL    USING   W_ERR_CHK.
         IF W_ERR_CHK EQ 'Y'.
            LEAVE TO SCREEN 0.
         ELSE.
            PERFORM RESET_LIST.
         ENDIF.

       WHEN 'FB03'.                    " 전기.
          PERFORM P2000_MULTI_SELECTION.

          IF W_SELECTED_LINES NE 0.
            PERFORM   P4000_GET_INIVAL  USING   W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'. EXIT.  ENDIF.

            PERFORM   P4000_CALL_BAPPI_PROCESS. " 저장/전기.
            PERFORM   P1000_GET_ZTFINBILL    USING   W_ERR_CHK.

            IF W_ERR_CHK EQ 'Y'.
               LEAVE TO SCREEN 0.
            ELSE.
               PERFORM RESET_LIST.
            ENDIF.
         ENDIF.

*      WHEN 'DOWN'.          " FILE DOWNLOAD....
*           PERFORM P3000_CREATE_DOWNLOAD_FILE.
*           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
*  테이블 SELECT
         PERFORM   P1000_GET_ZTFINBILL     USING   W_ERR_CHK.
         IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
         PERFORM RESET_LIST.
*------- Abbrechen (CNCL) ----------------------------------------------
      WHEN 'CNCL' OR 'PICK' OR 'CANC'.
         SET SCREEN 0.    LEAVE SCREEN.
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
            MESSAGE ID IT_ERR_LIST-MSGID TYPE 'I'
                    NUMBER IT_ERR_LIST-MSGNR
                    WITH   IT_ERR_LIST-MSGV1
                           IT_ERR_LIST-MSGV2
                           IT_ERR_LIST-MSGV3
                           IT_ERR_LIST-MSGV4.
         ENDIF.
         CLEAR : IT_ERR_LIST.
     WHEN OTHERS.
  ENDCASE.



*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMY9'.          " TITLE BAR
  P_NON = 'X'.                     " 미반영자료포함.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT RESET.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /65  '[ 계산서(FINBIL) EDI 수신현황 ]'.
  WRITE : /3 'Date : ', SY-DATUM, 135 'Page :',W_PAGE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-ULINE.
  WRITE:/ SY-VLINE, ' ' ,
                   (25) '발급은행',
          SY-VLINE,(10) '수신일자' ,
          SY-VLINE,(15) '참조문서번호' NO-GAP,
          SY-VLINE,(05) 'Im-Ex',
          SY-VLINE,(10) '어음조건',
          SY-VLINE,(08) '결제방법',
          SY-VLINE,(18) '계산서용도',
          SY-VLINE,(10) '거래일자',
          SY-VLINE,(10) '반영일자',
          SY-VLINE,(10) '반영시간',
          SY-VLINE.

  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/ SY-VLINE,' ',
                   (06) 'Seq',
                   (18) '관리코드',
          SY-VLINE,(10) '수수료율',
          SY-VLINE,(22) '산출금액(수수료,이자) ',
          SY-VLINE,(21) '외화금액',
          SY-VLINE,(18) '환율',
          SY-VLINE,(10) '적용일수',
          SY-VLINE,(10) '기간 From',

          SY-VLINE,(10) '기간 To',

          SY-VLINE.
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
      MESSAGE S960 WITH SY-UNAME '의뢰 Release 트랜잭션'.
      W_ERR_CHK = 'Y'.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTFINBILL
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTFINBILL   USING   W_ERR_CHK.

  RANGES : R_ZFDBYN FOR  ZTFINHD-ZFDBYN  OCCURS 5.

  W_ERR_CHK = 'N'.
  IF P_YES = SPACE AND P_NON = SPACE.
      W_ERR_CHK = 'Y'. EXIT.
  ENDIF.
  IF P_YES = 'X'.
     MOVE : 'I'       TO   R_ZFDBYN-SIGN,
            'EQ'      TO   R_ZFDBYN-OPTION,
            'Y'       TO   R_ZFDBYN-LOW,
            SPACE     TO   R_ZFDBYN-HIGH.
     APPEND R_ZFDBYN.
     MOVE : 'I'       TO   R_ZFDBYN-SIGN,
            'EQ'      TO   R_ZFDBYN-OPTION,
            'M'       TO   R_ZFDBYN-LOW,
            SPACE     TO   R_ZFDBYN-HIGH.
     APPEND R_ZFDBYN.
  ENDIF.

  IF P_NON = 'X'.
     MOVE : 'I'       TO   R_ZFDBYN-SIGN,
            'EQ'      TO   R_ZFDBYN-OPTION,
            'N'       TO   R_ZFDBYN-LOW,
            SPACE     TO   R_ZFDBYN-HIGH.
     APPEND R_ZFDBYN.
     MOVE : 'I'       TO   R_ZFDBYN-SIGN,
            'EQ'      TO   R_ZFDBYN-OPTION,
            SPACE     TO   R_ZFDBYN-LOW,
            SPACE     TO   R_ZFDBYN-HIGH.
     APPEND R_ZFDBYN.
  ENDIF.


  REFRESH: IT_FINHD, IT_FINIT.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_FINHD
    FROM ZTFINHD
   WHERE ZFRCDT   IN S_RCDT      " 수신일.
     AND ZFBKCD   IN S_BKCD      " 은행코드.
       AND ZFTERM IN S_TERM      " 결제방법.
       AND ZFNOTE IN S_NOTE      " 어음조건.
       AND ZFDBYN IN R_ZFDBYN.

  DESCRIBE TABLE IT_FINHD LINES W_LINE.
  IF W_LINE = 0.  W_ERR_CHK = 'Y'. EXIT.   ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_FINIT
    FROM ZTFINIT
           FOR ALL ENTRIES IN IT_FINHD
           WHERE ZFDHENO = IT_FINHD-ZFDHENO.  	  " 수신일.
*>> MODIFY.
  PERFORM   P1000_GET_TEXT.

ENDFORM.                    " P1000_GET_ZTFINBIL
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIMY9'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMY9'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_FINHD.
      W_LINE = W_LINE + 1.
      PERFORM P3000_HD_LINE_WRITE.
      AT LAST.
         WRITE : / '총', W_COUNT, '건'.
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
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFDHENO    LIKE  ZTFINIT-ZFDHENO,  " 문서관리번호.
        ZFREQNO    LIKE ZTREQST-ZFREQNO,   " 수입의뢰 관리번호.
        ZFAMDNO    LIKE ZTREQST-ZFAMDNO,   " Amend Seq.
        ZFBKCD     LIKE  ZTFINHD-ZFBKCD,   " 은행코드.
        ZFTFEE     LIKE  ZTFINHD-ZFTFEE.   " 수수료(이자) 합계.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : IT_FINHD-ZFREQNO  TO ZFREQNO,
         IT_FINHD-ZFAMDNO  TO ZFAMDNO,
         IT_FINHD-ZFDHENO  TO ZFDHENO,
         IT_FINHD-ZFBKCD   TO ZFBKCD,
         IT_FINHD-ZFTFEE   TO ZFTFEE.
  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_FINHD-ZFREQNO  TO IT_SELECTED-ZFREQNO,
             IT_FINHD-ZFAMDNO  TO IT_SELECTED-ZFAMDNO,
             IT_FINHD-ZFDHENO  TO IT_SELECTED-ZFDHENO,
             IT_FINHD-ZFBKCD   TO IT_SELECTED-ZFBKCD,
             IT_FINHD-BUKRS    TO IT_SELECTED-BUKRS,
*             IT_FINHD-BLART    TO IT_SELECTED-BLART,
             IT_FINHD-ZFTFEE   TO IT_SELECTED-ZFTFEE.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

*  IF W_SELECTED_LINES EQ 0.
*    IF INDEX GT 0.
*      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO,
*             ZFAMDNO TO IT_SELECTED-ZFAMDNO,
*             ZFDHENO TO IT_SELECTED-ZFDHENO,
*             BUKRS   TO IT_SELECTED-BUKRS,
*             ZFBKCD  TO IT_SELECTED-ZFBKCD,
*             ZFTFEE  TO IT_SELECTED-ZFTFEE.
*
*      APPEND IT_SELECTED.
*      ADD 1 TO W_SELECTED_LINES.
*    ELSE.
*      MESSAGE S951.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P3000_HD_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_HD_LINE_WRITE.

   PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDNOTE' IT_FINHD-ZFNOTE
                             CHANGING   W_DOM_TEX1.
   PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDTERM'IT_FINHD-ZFTERM
                             CHANGING   W_DOM_TEX2.
   PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDBILCD'IT_FINHD-ZFBILCD
                             CHANGING   W_DOM_TEX3.
   FORMAT RESET.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.
   WRITE:/  SY-VLINE, MARKFIELD  AS CHECKBOX,
                     (10) IT_FINHD-ZFBKNM NO-GAP, " 은행명.
                     (15) IT_FINHD-ZFBRNM,
            SY-VLINE,(10) IT_FINHD-ZFRCDT.        " 수신일.
    WRITE: SY-VLINE,(15) IT_FINHD-ZFREFNO NO-GAP. " 참조문서번호.
   IF IT_FINHD-OK EQ 'N'.
       FORMAT RESET.
       FORMAT COLOR COL_NEGATIVE  INTENSIFIED OFF.
   ENDIF.
   WRITE:  SY-VLINE,(06) IT_FINHD-MAT_OK NO-GAP.
   FORMAT RESET.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.
   WRITE: SY-VLINE NO-GAP,(11)  W_DOM_TEX1,        " 어음조건.
          SY-VLINE NO-GAP,(09)  W_DOM_TEX2,        " 결제방법.
          SY-VLINE NO-GAP,(20)  W_DOM_TEX3 NO-GAP. " 계산서용도.

   WRITE:   SY-VLINE,(10)  IT_FINHD-ZFTRDT.      " 거래일자.
   IF NOT IT_FINHD-ZFDBDT IS INITIAL.
      WRITE:   SY-VLINE, (10) IT_FINHD-ZFDBDT.   " 수수료 반영 일자.
   ELSE.
     WRITE:   SY-VLINE, (10) ' '.                " 수수료 반영 일자.
   ENDIF.
   IF NOT IT_FINHD-ZFDBTM IS INITIAL.
      WRITE:  SY-VLINE,(10)  IT_FINHD-ZFDBTM.    " 수수료 반영 시간.
   ELSE.
      WRITE:  SY-VLINE,(10)  ' '.    " 수수료 반영 시간.
   ENDIF.
   WRITE: SY-VLINE.

   W_COUNT = W_COUNT + 1.
   HIDE: IT_FINHD.
   PERFORM P2000_IT_LINE_WRITE.
   WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_HD_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO P_ZFAMDNO.

   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
   EXPORT 'BES'           TO MEMORY ID 'BES'.
   EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
   EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
   EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.

   IF P_ZFAMDNO = '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_LC

*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM P3000_CREATE_DOWNLOAD_FILE.
*
*  REFRESH IT_TAB_DOWN.
*  LOOP AT IT_TAB.
*    CLEAR IT_TAB_DOWN.
*    MOVE-CORRESPONDING IT_TAB TO IT_TAB_DOWN.
*    WRITE : IT_TAB-MENGE   UNIT     IT_TAB-MEINS TO IT_TAB_DOWN-MENGE,
*            IT_TAB-ZFUSDAM CURRENCY IT_TAB-ZFUSD TO IT_TAB_DOWN-ZFUSDAM
*            IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS TO IT_TAB_DOWN-ZFOPAMT
*    APPEND IT_TAB_DOWN.
*  ENDLOOP.

ENDFORM.                    " P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P2000_IT_LINE_WRITE.

   LOOP AT IT_FINIT WHERE ZFDHENO = IT_FINHD-ZFDHENO .
       W_TABIX = SY-TABIX.
       PERFORM  P3000_IT_LINE_WRITE.
   ENDLOOP.

ENDFORM.                    " P2000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_IT_LINE_WRITE.

  DATA: W_MAT_OK(10) TYPE C VALUE '수입'.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE,' ',
                   (06) IT_FINIT-ZFSEQ,   " 일련번호.
                   (18) IT_FINIT-ZFCDNM, " 관리코드.
          SY-VLINE NO-GAP,
                   (12) IT_FINIT-ZFRATE NO-GAP, " 수수료(이자) 적용율.
          SY-VLINE,(18) IT_FINIT-ZFFEE
                        CURRENCY IT_FINIT-ZFKRW," 산출금액(수수료,이자).
                   (03) IT_FINIT-ZFKRW,	" 원화통화.
          SY-VLINE,(17) IT_FINIT-ZFAMT
                        CURRENCY IT_FINIT-WAERS, " 금액.
                   (03) IT_FINIT-WAERS,	
          SY-VLINE,(18) IT_FINIT-ZFEXRT,  " 환율.
          SY-VLINE,(10) IT_FINIT-ZFDAY,	" 적용일수.
          SY-VLINE,(10) IT_FINIT-ZFFROM,	" 적용기간(FROM).
          SY-VLINE,(10) IT_FINIT-ZFEND,	" 적용기간(TO).
          SY-VLINE.

ENDFORM.                    " P3000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_TEXT
*&---------------------------------------------------------------------*
FORM P1000_GET_TEXT.

 LOOP AT IT_FINHD.
      W_TABIX = SY-TABIX.
      CLEAR ZTREQST.
      SELECT SINGLE * FROM  ZTREQST
             WHERE ZFOPNNO EQ IT_FINHD-ZFREFNO.
      W_SUBRC = SY-SUBRC.

      IF W_SUBRC NE 0.
         IT_FINHD-OK = 'N'.
         IT_FINHD-MAT_OK = 'None'.
      ELSE.
         IT_FINHD-OK = 'Y'.
         IT_FINHD-MAT_OK = '수입'.
      ENDIF.

      IF W_SUBRC EQ 0.
         MOVE ZTREQST-ZFREQNO  TO IT_FINHD-ZFREQNO.

         SELECT MAX( ZFAMDNO ) INTO IT_FINHD-ZFAMDNO
                FROM ZTREQST
                WHERE ZFREQNO = IT_FINHD-ZFREQNO.

         SELECT SINGLE * FROM ZTREQHD
                WHERE ZFREQNO EQ IT_FINHD-ZFREQNO
                AND   BUKRS   IN S_BUKRS.

         W_SUBRC = SY-SUBRC.

         IF W_SUBRC EQ 0.
            MOVE ZTREQHD-BUKRS TO IT_FINHD-BUKRS.
            MOVE ZTREQHD-ZFOPBN TO IT_FINHD-ZFOPBN.
         ENDIF.
      ENDIF.

      IF P_NOT NE 'X'.
         IF W_SUBRC NE 0.
            DELETE IT_FINIT WHERE ZFDHENO = IT_FINHD-ZFDHENO.
            DELETE IT_FINHD INDEX W_TABIX.
            CONTINUE.
         ENDIF.
      ENDIF.
*>회사코드.
      MODIFY  IT_FINHD INDEX W_TABIX.
*>> IT_FINIT MODIFY.
      PERFORM P1000_MODIFY_FINIT.
 ENDLOOP.

 DESCRIBE TABLE IT_FINHD LINES W_LINE.
 IF W_LINE EQ 0.
    W_ERR_CHK = 'Y'.
 ENDIF.

ENDFORM.                    " P1000_GET_TEXT
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
*&      Form  P4000_GET_INIVAL
*&---------------------------------------------------------------------*
FORM P4000_GET_INIVAL USING  W_ERR_CHK.

  CLEAR: W_PROC_CNT,W_COUNT,W_OLD_ZFBKCD,W_AMOUNT.


  W_ERR_CHK = 'N'.
  READ TABLE IT_FINIT WITH KEY ZFDHENO = IT_SELECTED-ZFDHENO.
  IF SY-SUBRC NE 0.
     MESSAGE I664.
     W_ERR_CHK = 'Y'.
     EXIT.
  ENDIF.

  LOOP AT IT_SELECTED.
       IF SY-TABIX NE 1.
          IF IT_SELECTED-ZFBKCD NE W_OLD_ZFBKCD. " 은행코드 처리시.
             MESSAGE E660. " 전기시 처리은행이 다름.
             W_ERR_CHK = 'Y'.
             EXIT.
          ENDIF.
          IF IT_SELECTED-BUKRS NE W_OLD_BUKRS. " 회사코드 처리시.
             MESSAGE E660. " 전기시 처리은행이 다름.
             W_ERR_CHK = 'Y'.
             EXIT.
          ENDIF.
       ENDIF.

       MOVE: IT_SELECTED-ZFBKCD TO W_OLD_ZFBKCD,
             IT_SELECTED-BUKRS  TO W_OLD_BUKRS.

       PERFORM P4000_VALID_CHECK USING  W_ERR_CHK.
       IF W_ERR_CHK = 'Y'.   EXIT.  ENDIF.

       ADD IT_SELECTED-ZFTFEE TO W_AMOUNT.       " TOTAL 금액.
  ENDLOOP.

ENDFORM.                    " P4000_GET_INIVAL
*&---------------------------------------------------------------------*
*&      Form  P4000_VALID_CHECK
*&---------------------------------------------------------------------*
FORM P4000_VALID_CHECK USING  W_ERR_CHK.

  W_ERR_CHK = 'N'.
*>> 데이타 삭제시.
  READ TABLE IT_FINHD WITH KEY  ZFDHENO  = IT_SELECTED-ZFDHENO .
  IF SY-UCOMM EQ 'DELE'.
     IF  IT_FINHD-ZFDBYN EQ 'Y'.
         MESSAGE I894.
         W_ERR_CHK = 'Y'.
         EXIT.
     ENDIF.
  ENDIF.
*>> 전기시.
  IF SY-UCOMM EQ 'FB03'.
     IF  IT_FINHD-ZFDBYN EQ 'Y'.
         MESSAGE I578.
         W_ERR_CHK = 'Y'.
         EXIT.
     ENDIF.
  ENDIF.

*>> 비용문서조회시.
  IF SY-UCOMM EQ 'ZIMY3'.
     IF  IT_FINHD-ZFDBYN EQ 'N'.
         MESSAGE I665.
         W_ERR_CHK = 'Y'.
         EXIT.
     ENDIF.
  ENDIF.

*>> 지우는 것만제외 .수입의뢰내용에 없을경우.
  IF SY-UCOMM NE 'DELE'.
     IF  IT_FINHD-OK EQ 'N'.
         MESSAGE I661.
         W_ERR_CHK = 'Y'.
         EXIT.
     ENDIF.
  ENDIF.

ENDFORM.                    " P4000_VALID_CHECK
*&---------------------------------------------------------------------*
*&      Form  P4000_CALL_SCREEN
*&---------------------------------------------------------------------*
FORM P4000_CALL_SCREEN.

  CALL SCREEN 0100   STARTING AT 5   5
                      ENDING  AT 110 25.

ENDFORM.                    " P4000_CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0100 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU1'.

   IF OPTION = '1'.
      SET CURSOR FIELD 'SPOP-OPTION1'.
   ELSE.
      SET CURSOR FIELD 'SPOP-OPTION2'.
   ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_INIT_FIELD_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_INIT_FIELD_SCR0100 OUTPUT.

ENDMODULE.                 " SET_INIT_FIELD_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
       SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'YES'.    ANTWORT = 'Y'.
       SET SCREEN 0.   LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

  MOVE OK-CODE TO W_OK_CODE.
  CLEAR : OK-CODE.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0100_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0100_SCR0100 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0100-CURRENT_LINE GT TC_0100-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBSEG INDEX TC_0100-CURRENT_LINE.
  IF SY-SUBRC = 0.                                  " READ SUCCESS..
     MOVE-CORRESPONDING IT_ZSBSEG   TO ZSBSEG.      " DATA MOVE
     MOVE: IT_ZSBSEG-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0100_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0100 OUTPUT.

  DESCRIBE TABLE IT_ZSBSEG   LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0100-LINES = G_PARAM_LINE.                    " LINE 수 정의.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0100 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0100-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBSEG_UPDATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSBSEG_UPDATE_SCR0100 INPUT.

  CLEAR IT_ZSBSEG.
*>> 2001.08.13 KSB MODIFY.
*  MOVE-CORRESPONDING  ZSBSEG  TO  IT_ZSBSEG.
  READ TABLE IT_ZSBSEG INDEX  TC_0100-CURRENT_LINE.
  MOVE : ZSBSEG-ZUONR  TO IT_ZSBSEG-ZUONR,
         ZSBSEG-SGTXT  TO IT_ZSBSEG-SGTXT,
         ZSBSEG-NEWKO  TO IT_ZSBSEG-NEWKO,
         ZSBSEG-PRCTR  TO IT_ZSBSEG-PRCTR.

  IF IT_ZSBSEG-NEWKO IS INITIAL.
     SELECT SINGLE * FROM ZTIMIMG11
            WHERE    BUKRS  EQ   ZTBKPF-BUKRS.
     IF SY-SUBRC EQ 0.
        MOVE ZTIMIMG11-ZFIOCAC1 TO IT_ZSBSEG-NEWKO.
     ENDIF.
  ENDIF.

  MODIFY IT_ZSBSEG INDEX TC_0100-CURRENT_LINE.

ENDMODULE.                 " IT_ZSBSEG_UPDATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P4000_MOVE_INIT_DATA
*&---------------------------------------------------------------------*
FORM P4000_MOVE_INIT_DATA.

DATA : L_ZFFEE LIKE IT_FINHD-ZFTFEE.


  CLEAR IT_FINHD.
  READ TABLE IT_FINHD
             WITH KEY ZFDHENO = IT_SELECTED-ZFDHENO.

  CLEAR : W_AMOUNT.

  IF SY-SUBRC EQ 0.
       IF W_COUNT EQ 1.
          PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCDTY'
                                                     '003'
                                       CHANGING   W_COST_TYPE.
*>>
          CLEAR:ZTIMIMG08-ZFCD.
          READ TABLE IT_FINIT WITH KEY ZFDHENO = IT_SELECTED-ZFDHENO.
          MOVE: IT_FINIT-ZFCD   TO ZTIMIMG08-ZFCD,
                IT_FINIT-ZFCDNM TO W_CODE_TYPE.

*>> 지급조건 가져오기.
*          GET PARAMETER ID 'BUK'    FIELD ZTBKPF-BUKRS.
          MOVE : IT_FINHD-BUKRS    TO ZTBKPF-BUKRS.

          CALL FUNCTION 'FI_VENDOR_DATA'
             EXPORTING
               i_bukrs = ZTBKPF-BUKRS
               i_lifnr = IT_FINHD-ZFOPBN
             IMPORTING
               e_kred  = vf_kred.

          ZTBKPF-ZTERM = VF_KRED-ZTERM.
          ZTBKPF-AKONT = VF_KRED-AKONT.

          MOVE:SY-DATUM        TO  ZTBKPF-BLDAT,
               SY-DATUM        TO  ZTBKPF-BUDAT,
               SY-DATUM        TO  ZTBKPF-ZFBDT,
               IT_FINHD-BUKRS  TO  ZTBKPF-BUKRS,
               IT_FINHD-ZFBKNM TO  ZTFINHD-ZFBKNM," 은행명.
               IT_FINHD-ZFOPBN TO  ZTBKPF-LIFNR,
               IT_FINHD-ZFOPBN TO  ZTBKPF-ZFVEN,
*               W_AMOUNT        TO  ZTBKPF-WRBTR,  " 수수료합계.
*               W_AMOUNT        TO  ZTBKPF-DMBTR,  " 현지통화금액.
               W_DV_CT         TO  ZTBKPF-ZFDCSTX, " Delivery Cost 여부.
               'X'             TO  ZTBKPF-ZFPCUR,
               IT_FINHD-WAERS  TO  ZTBKPF-HWAER,
               IT_FINHD-WAERS  TO  ZTBKPF-WAERS,
               SPACE           TO  ZTBKPF-WMWST,
               'Y'             TO  ZTBKPF-ZFPOYN,
               SPACE           TO  ZTBKPF-MWSKZ,
               '003'           TO  ZTBKPF-ZFCSTGRP,
               'X'             TO  ZTBKPF-ZFATPT,
               'X'             TO  ZTBKPF-ZFAUTO.
*> 업무가불...
          IF NOT P_HKONT IS INITIAL.
             MOVE : P_HKONT TO ZTBKPF-HKONT,
                    'X'     TO ZTBKPF-ZFADVPT.
          ENDIF.
       ENDIF.

    REFRESH : IT_ZSBSEG, IT_COSTGB.
    CLEAR   : IT_ZSBSEG, IT_COSTGB, ZTBKPF-WRBTR, ZTBKPF-DMBTR.

    LOOP AT IT_FINIT WHERE ZFDHENO = IT_SELECTED-ZFDHENO.
       MOVE: IT_FINHD-BELNR   TO IT_ZSBSEG-BELNR,     " 수입비용문서.
             IT_FINHD-ZFREQNO TO IT_ZSBSEG-ZFIMDNO,   " 수입의뢰번호.
             IT_FINHD-ZFREFNO TO IT_ZSBSEG-ZFDCNM,    " 관련문서.
             IT_FINHD-ZFREFNO TO IT_ZSBSEG-ZUONR,     " 지정.
             '003'            TO IT_ZSBSEG-ZFCSTGRP.  " 비용그룹.

       IF IT_FINIT-ZFFEE EQ 0.
          L_ZFFEE =  IT_FINHD-ZFTFEE.
       ELSE.
          L_ZFFEE =  IT_FINIT-ZFFEE.
       ENDIF.

       ADD L_ZFFEE TO W_AMOUNT.

       MOVE: IT_FINIT-ZFCD    TO   IT_ZSBSEG-ZFCD,   " 비용코드.
             L_ZFFEE          TO   IT_ZSBSEG-WRBTR,  " 금액.
             L_ZFFEE          TO   IT_ZSBSEG-DMBTR,  " 수수료.
             IT_FINIT-ZFDHENO TO   IT_ZSBSEG-ZFDOCNO," 전자문서번호.
              '1'             TO   IT_ZSBSEG-KURSF,
              '40'            TO   IT_ZSBSEG-NEWBS,
             IT_FINIT-DV_CT   TO   IT_ZSBSEG-ZFDCSTX,    " DC  여부.
              'S'             TO   IT_ZSBSEG-SHKZG,      " 전기키.
             IT_FINIT-COND_TYPE TO IT_ZSBSEG-COND_TYPE,  " 조건.
             IT_FINIT-ZFCDNM  TO   IT_ZSBSEG-SGTXT,      " TEXT.
             IT_FINIT-BLART   TO   ZTBKPF-BLART,         "문서종류.
             L_ZFFEE          TO   IT_ZSBSEG-DMBTR.      " 수수료.

       PERFORM P1000_IMPORT_DOC_CHEKC    USING IT_ZSBSEG-ZFIMDNO
                                               IT_ZSBSEG-ZFDCNM
                                               IT_ZSBSEG-ZFPOYN
                                               IT_ZSBSEG-KOSTL
                                               ZTBKPF-GSBER
                                               ZTBKPF-BUPLA.


*> 계정결정 함수.
       CALL FUNCTION 'ZIM_GET_NODRAFT_ACCOUNT'
            EXPORTING
                ZFCSTGRP   =     IT_ZSBSEG-ZFCSTGRP
                ZFCD       =     IT_ZSBSEG-ZFCD
                ZFIMDNO    =     IT_ZSBSEG-ZFIMDNO
            IMPORTING
                 NEWKO     =     IT_ZSBSEG-NEWKO.

*> 2001.08.13 KSB INSERT.
       SELECT SINGLE * FROM ZTIMIMG11
              WHERE    BUKRS  EQ   ZTBKPF-BUKRS.
       IF SY-SUBRC EQ 0.
          MOVE ZTIMIMG11-ZFIOCAC1 TO IT_ZSBSEG-NEWKO.
       ENDIF.

*       IF IT_ZSBSEG-DMBTR IS INITIAL.
*
*       ENDIF.
*       ADD IT_ZSBSEG-DMBTR TO ZTBKPF-DMBTR.

       APPEND IT_ZSBSEG.
       IT_COSTGB-DV_CT = IT_FINIT-DV_CT.
       COLLECT IT_COSTGB.
    ENDLOOP.
  ENDIF.

  ZTBKPF-DMBTR = W_AMOUNT.
  ZTBKPF-WRBTR = ZTBKPF-DMBTR.

  DESCRIBE  TABLE IT_COSTGB LINES W_LINE.
  IF W_LINE EQ 1.
     READ TABLE IT_COSTGB INDEX 1.
     ZTBKPF-ZFDCSTX = IT_COSTGB-DV_CT.
  ELSE.
     CLEAR : ZTBKPF-ZFDCSTX.
  ENDIF.

 ENDFORM.                    " P4000_MOVE_INIT_DATA
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  CASE OK-CODE.
    WHEN 'CANC'.                                                " Yes...
       LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_BAPPI
*&---------------------------------------------------------------------*
FORM P2000_CALL_BAPPI.

 PERFORM P2000_DB_MODIFY_ZTBKPF.
 IF W_ERR_CHK EQ 'Y'. EXIT.ENDIF.
 PERFORM P2000_CHAGE_DOCUMENT_POST.

 ENDFORM.                    " P2000_CALL_BAPPI
*&---------------------------------------------------------------------*
*&      Form  P4000_CALL_BAPPI_PROCESS
*&---------------------------------------------------------------------*
FORM P4000_CALL_BAPPI_PROCESS.

  REFRESH IT_ZSBSEG.
  LOOP AT IT_SELECTED.
       W_COUNT = W_COUNT + 1.
       PERFORM P4000_MOVE_INIT_DATA.
  ENDLOOP.

  PERFORM P4000_CALL_SCREEN.

  CASE  OK-CODE.
     WHEN 'YES'.
        PERFORM P2000_CALL_BAPPI.
     WHEN 'CANC'.
        MESSAGE S957.    EXIT.
     WHEN OTHERS.

  ENDCASE.
*  IF W_SUBRC EQ 0.       " 안에 집어 넣어 났슴.
*     PERFORM P2000_DBMODIFY_ZTFINHD.
*  ENDIF.
  IF W_ERR_CHK = 'Y'. EXIT. ENDIF.
  MESSAGE S992 WITH W_PROC_CNT.
  EXIT.

ENDFORM.                    " P4000_CALL_BAPPI_PROCESS
*&---------------------------------------------------------------------*
*&      Module  COMPANYCODE_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE COMPANYCODE_CHECK_SCR0100 INPUT.

   IF OK-CODE EQ 'CANC'.
      SET SCREEN 0.  LEAVE SCREEN.
   ENDIF.

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

ENDMODULE.                 " COMPANYCODE_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  PERIOD_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE PERIOD_CHECK_SCR0100 INPUT.

   IF OK-CODE EQ 'CANC'.
      SET SCREEN 0.  LEAVE SCREEN.
   ENDIF.


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


ENDMODULE.                 " PERIOD_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_DB_MODIFY_ZTBKPF
*&---------------------------------------------------------------------*
FORM P2000_DB_MODIFY_ZTBKPF.

  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
     EXPORTING
             W_OK_CODE           =   W_OK_CODE
             BUKRS               =   ZTBKPF-BUKRS
             GJAHR               =   ZTBKPF-GJAHR
             ZFSTATUS            =   'C'
             W_ZTBKPF_OLD        =   ZTBKPF
             W_ZTBKPF            =   ZTBKPF
    TABLES
*            IT_ZSBSEG_OLD       =
             IT_ZSBSEG           =  IT_ZSBSEG
*            IT_ZSBDIV           =
*            IT_ZSBHIS           =
    CHANGING
             BELNR               =   ZTBKPF-BELNR
    EXCEPTIONS
             ERROR_UPDATE.
  IF SY-SUBRC EQ 0.
*     COMMIT WORK.
  ELSE.
     W_ERR_CHK = 'Y'.
     ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " P2000_DB_MODIFY_ZTBKPF
*&---------------------------------------------------------------------*
*&      Form  P2000_CHAGE_DOCUMENT_POST
*&---------------------------------------------------------------------*
FORM P2000_CHAGE_DOCUMENT_POST.
DATA : L_MSGTY,
       L_ZTBKPF LIKE ZTBKPF.

  REFRESH : IT_ERR_LIST.

  IF ZTBKPF-BELNR IS INITIAL.
     MESSAGE  S587.
     L_MSGTY = 'E'.
     PERFORM  P2000_MESSAGE_MAKE(SAPMZIM02)
                                 TABLES  IT_ERR_LIST
                                  USING  L_MSGTY.
     EXIT.
  ENDIF.

  IF ZTBKPF-ZFPOSYN EQ 'Y'.
     MESSAGE  S578.
     L_MSGTY = 'E'.
     PERFORM  P2000_MESSAGE_MAKE(SAPMZIM02)
                              TABLES  IT_ERR_LIST
                              USING   L_MSGTY.
     EXIT.
  ENDIF.
*>> AP 발생 FUNCTION CALL!
  CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_POST'
          EXPORTING
            BUKRS            =     ZTBKPF-BUKRS
            BELNR            =     ZTBKPF-BELNR
            GJAHR            =     ZTBKPF-GJAHR
         TABLES
            RETURN           =     RETURN
         EXCEPTIONS
            POST_ERROR       =     4.

   W_SUBRC = SY-SUBRC.

   IF SY-SUBRC NE 0.           ">> 오류 발생시...
      ROLLBACK WORK.
      IF RETURN[] IS INITIAL.
         L_MSGTY = 'E'.
         PERFORM  P2000_MESSAGE_MAKE(SAPMZIM02)
                        TABLES IT_ERR_LIST USING L_MSGTY.
      ELSE.
         PERFORM  P2000_MULTI_MSG_MAKE
                        TABLES  IT_ERR_LIST.
      ENDIF.
      CLEAR : L_ZTBKPF.
      CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
           EXPORTING
                     W_OK_CODE           =   'DELE'
                     BUKRS               =   ZTBKPF-BUKRS
                     GJAHR               =   ZTBKPF-GJAHR
                     ZFSTATUS            =   'U'
                     W_ZTBKPF_OLD        =   L_ZTBKPF
                     W_ZTBKPF            =   ZTBKPF
           TABLES
                     IT_ZSBSEG           =   IT_ZSBSEG
           CHANGING
                     BELNR               =   ZTBKPF-BELNR
           EXCEPTIONS
                     ERROR_UPDATE         =   4.

   ELSE.
      COMMIT WORK.
      PERFORM P2000_DBMODIFY_ZTFINHD.
      L_MSGTY = 'S'.
      PERFORM  P2000_MESSAGE_MAKE(SAPMZIM02)
                         TABLES IT_ERR_LIST USING L_MSGTY.
   ENDIF.

   DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
   IF W_LINE GT 0.
      INCLUDE = 'POPU'.
      CALL SCREEN 0014 STARTING AT  05   3
                       ENDING   AT  100 12.
      CLEAR : INCLUDE.
   ENDIF.

*   IF W_SUBRC EQ 0.
*      PERFORM  P2000_SET_LOCK_MODE(SAPMZIM02)  USING  'U'.
*   ELSE.
*      EXIT.
*   ENDIF.

ENDFORM.                    " P2000_CHAGE_DOCUMENT_POST
*&---------------------------------------------------------------------*
*&      Module  D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
MODULE D0014_STATUS_SCR0014 OUTPUT.

  SET PF-STATUS 'POPU'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH '메시지 현황'.
     WHEN OTHERS.
  ENDCASE.
  SUPPRESS DIALOG.

ENDMODULE.                 " D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0014_LIST_CHECK_SCR0014 INPUT.

   LEAVE TO LIST-PROCESSING.
   CASE INCLUDE.
      WHEN 'POPU'.
         FORMAT COLOR COL_HEADING INTENSIFIED OFF.
         WRITE : / SY-ULINE(96), / SY-VLINE NO-GAP,
                   '유형'   NO-GAP, SY-VLINE NO-GAP,
*                   ' 문서번호 ' NO-GAP, SY-VLINE NO-GAP,
                   '메세지 텍스트', 94 SY-VLINE NO-GAP,
                   'T'      NO-GAP, SY-VLINE,
                 / SY-ULINE(96).
*         MESSAGE
         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)     NO-GAP,
*                      SY-VLINE NO-GAP, IT_ERR_LIST-BELNR       NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(87) NO-GAP,
                      SY-VLINE NO-GAP.

            CASE IT_ERR_LIST-MSGTYP.
               WHEN 'E'.
                  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
               WHEN 'W'.
                  FORMAT COLOR COL_KEY      INTENSIFIED OFF.
               WHEN 'I'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
               WHEN 'S'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
            ENDCASE.

            WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP,
                   / SY-ULINE(96).
            HIDE:IT_ERR_LIST.
         ENDLOOP.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_DBMODIFY_ZTFINHD
*&---------------------------------------------------------------------*
FORM P2000_DBMODIFY_ZTFINHD.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTFINHD
           FROM ZTFINHD
           FOR ALL ENTRIES IN IT_SELECTED
           WHERE ZFDHENO = IT_SELECTED-ZFDHENO.

  CLEAR W_PROC_CNT .
  LOOP AT IT_ZTFINHD.
     W_TABIX = SY-TABIX.
     MOVE:  'Y'           TO   IT_ZTFINHD-ZFDBYN,
            ZTBKPF-BUKRS  TO   IT_ZTFINHD-BUKRS,
            ZTBKPF-GJAHR  TO   IT_ZTFINHD-GJAHR,
            ZTBKPF-BELNR  TO   IT_ZTFINHD-BELNR,
            SY-DATUM      TO   IT_ZTFINHD-ZFDBDT,
            SY-UZEIT      TO   IT_ZTFINHD-ZFDBTM,
            SY-UNAME      TO   IT_ZTFINHD-ZFDBID.
     MODIFY IT_ZTFINHD INDEX W_TABIX.

     IF SY-SUBRC EQ 0.
       W_PROC_CNT = W_PROC_CNT + 1.
     ENDIF.
  ENDLOOP.
**>> DB MODIFY.
  MODIFY ZTFINHD FROM TABLE IT_ZTFINHD.

*>> 수신현황
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTDHF1
           FROM ZTDHF1
           FOR ALL ENTRIES IN IT_SELECTED
           WHERE ZFDHENO = IT_SELECTED-ZFDHENO.
  LOOP AT IT_ZTDHF1.
     W_TABIX = SY-TABIX.
     MOVE : 'Y'           TO   IT_ZTDHF1-ZFDHAPP,
            SY-DATUM      TO   IT_ZTDHF1-ZFDHSSD,
            SY-UZEIT      TO   IT_ZTDHF1-ZFDHSST.
     MODIFY IT_ZTDHF1     INDEX   W_TABIX.
  ENDLOOP.
**>> DB MODIFY.
  MODIFY ZTDHF1 FROM TABLE IT_ZTDHF1.

ENDFORM.                    " P2000_DBMODIFY_ZTFINHD
*&---------------------------------------------------------------------*
*&      Module  VENDOR_ACCOUNT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE VENDOR_ACCOUNT_CHECK_SCR0100 INPUT.

   IF OK-CODE EQ 'CANC'.
      SET SCREEN 0.  LEAVE SCREEN.
   ENDIF.

   IF ZTBKPF-BUPLA IS INITIAL.    ">
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUPLA'.
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

ENDMODULE.                 " VENDOR_ACCOUNT_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  BELEGART_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE BELEGART_CHECK_SCR0100 INPUT.

   IF OK-CODE EQ 'CANC'.
      SET SCREEN 0.  LEAVE SCREEN.
   ENDIF.

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

ENDMODULE.                 " BELEGART_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_HEADER_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE INPUT_HEADER_CHECK_SCR0100 INPUT.

   IF OK-CODE EQ 'CANC'.
      SET SCREEN 0.  LEAVE SCREEN.
   ENDIF.

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

ENDMODULE.                 " INPUT_HEADER_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TBTKZ_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE TBTKZ_CHECK_SCR0100 INPUT.

  IF OK-CODE EQ 'CANC'.
     SET SCREEN 0.  LEAVE SCREEN.
  ENDIF.

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

ENDMODULE.                 " TBTKZ_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0100 INPUT.

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

ENDMODULE.                 " HELP_ZTERM_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_TBTKZ_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_TBTKZ_SCR0100 INPUT.

  PERFORM   P2000_TBTKZ_HELP(SAPMZIMG)  USING   ZTBKPF-TBTKZ.
  SET CURSOR FIELD 'ZTBKPF-TBTKZ'.

ENDMODULE.                 " HELP_TBTKZ_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_COST_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_COST_CODE_SCR0100 INPUT.

   DATA : L_DISPLAY.

   PERFORM   P2000_CHECK_COST_GROUP(SAPMZIM02)  USING   W_ERR_MODE.

   IF W_ERR_MODE EQ 'N'.
      SELECT *
             INTO CORRESPONDING FIELDS OF TABLE IT_COST_HELP
             FROM   ZTIMIMG08
             WHERE  ZFCDTY   EQ   ZTBKPF-ZFCSTGRP.
      IF SY-SUBRC NE 0.
         MESSAGE S406.
         EXIT.
      ENDIF.

      DYNPROG = SY-REPID.
      DYNNR   = SY-DYNNR.

      WINDOW_TITLE = W_COST_TYPE.
      CONCATENATE W_COST_TYPE '코드 HELP' INTO WINDOW_TITLE
                  SEPARATED BY SPACE.

      CLEAR: L_DISPLAY.


      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
           EXPORTING
*                RETFIELD        = 'OTYPE'
                RETFIELD        = 'ZFCD'
                DYNPPROG        = DYNPROG
                DYNPNR          = DYNNR
                DYNPROFIELD     = 'ZSBSEG-ZFCD'
                WINDOW_TITLE    = WINDOW_TITLE
                VALUE_ORG       = 'S'
                DISPLAY         = L_DISPLAY
           TABLES
                VALUE_TAB       = IT_COST_HELP
           EXCEPTIONS
                PARAMETER_ERROR = 1
                NO_VALUES_FOUND = 2
                OTHERS          = 3.
       IF SY-SUBRC <> 0.
          EXIT.
       ENDIF.
   ENDIF.

ENDMODULE.                 " HELP_COST_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_MODIFY_FINIT
*&---------------------------------------------------------------------*
FORM P1000_MODIFY_FINIT.

  LOOP AT IT_FINIT WHERE ZFDHENO = IT_FINHD-ZFDHENO.
      CLEAR ZTIMIMG08.
      SELECT SINGLE * FROM ZTIMIMG08
             WHERE ZFCDTY EQ '003'
             AND   ZFCD   EQ  IT_FINIT-ZFCD.

      IF SY-SUBRC EQ 0.
         MOVE: ZTIMIMG08-ZFCDNM    TO IT_FINIT-ZFCDNM,
               ZTIMIMG08-COND_TYPE TO IT_FINIT-COND_TYPE,
               ZTIMIMG08-BLART     TO IT_FINIT-BLART.

*>> DELEVERY COST CHECK 섞여있을경우 무조건 'N'으로 처리.
         IF  ZTIMIMG08-ZFCD1 EQ 'Y'.
             IT_FINIT-DV_CT = 'X'.     " DELEVERY COST CHECK.
         ELSE.
             IT_FINIT-DV_CT = SPACE.
         ENDIF.

         MODIFY IT_FINIT INDEX SY-TABIX.
      ENDIF.
  ENDLOOP.

ENDFORM.                    " P1000_MODIFY_FINIT
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
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
*             IT_SELECTED-GJAHR   TO     IT_ERR_LIST-GJAHR,
*             IT_SELECTED-BELNR   TO     IT_ERR_LIST-BELNR.

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
*&      Form  P2000_SHOW_COST
*&---------------------------------------------------------------------*
FORM P2000_SHOW_COST USING    P_ZFDHENO.


   SET PARAMETER ID 'BUK'    FIELD IT_FINHD-BUKRS.
   SET PARAMETER ID 'GJR'    FIELD IT_FINHD-GJAHR.
   SET PARAMETER ID 'ZPBENR' FIELD IT_FINHD-BELNR.

   CALL TRANSACTION 'ZIMY3' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_COST
*&---------------------------------------------------------------------*
*&      Form  P2000_DELETE_ZTFINHD
*&---------------------------------------------------------------------*
FORM P2000_DELETE_ZTFINHD USING  W_ERR_CHK.

  W_ERR_CHK = 'N'.
   LOOP AT IT_SELECTED.
       PERFORM P4000_VALID_CHECK USING  W_ERR_CHK.
       IF W_ERR_CHK EQ 'Y'. EXIT.  ENDIF.
  ENDLOOP.
  IF W_ERR_CHK EQ 'Y'. EXIT.  ENDIF.
  PERFORM  P2000_DELETE_MESSAGE.

  IF  ANTWORT = 'Y'.
*>> 수행속도 관계로 Internal 테이블사용.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTFINIT
        FROM ZTFINIT
         FOR ALL ENTRIES IN IT_SELECTED
        WHERE ZFDHENO = IT_SELECTED-ZFDHENO.
      DELETE ZTFINIT FROM TABLE IT_ZTFINIT.
      IF SY-SUBRC NE 0.
         MESSAGE S313.EXIT.
      ENDIF.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTFINHD
        FROM ZTFINHD
         FOR ALL ENTRIES IN IT_SELECTED
        WHERE ZFDHENO = IT_SELECTED-ZFDHENO.

      DELETE ZTFINHD FROM TABLE IT_ZTFINHD.
      IF SY-SUBRC EQ 0.
         MESSAGE S339.   EXIT.
      ELSE.
         W_ERR_CHK = 'Y'.
         MESSAGE S313.  EXIT.
      ENDIF.
  ENDIF.

ENDFORM.                    " P2000_DELETE_ZTFINHD
*&---------------------------------------------------------------------*
*&      Form  P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DELETE_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING '삭제 확인'  " 타이틀...
                           '선택한 데이를 삭제합니다.'
                           '삭제하시겠습니까?' " MSG2
                           'N'                 " 취소 버튼 유/?
                           '1'.                " default button

ENDFORM.                    " P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_BOX USING    TITLE  LIKE SPOP-TITEL
                                TEXT1  LIKE SPOP-TEXTLINE1
                                TEXT2  LIKE SPOP-TEXTLINE2
                                CANCEL LIKE CANCEL_OPTION
                                DEFAULT LIKE OPTION.

   SPOP-TITEL = TITLE.
   SPOP-TEXTLINE1 = TEXT1.
   SPOP-TEXTLINE2 = TEXT2.
   CANCEL_OPTION  = CANCEL.
   OPTION = DEFAULT.
   TEXTLEN = 40.
   CALL SCREEN 0001 STARTING AT 30 6
                    ENDING   AT 78 10.

ENDFORM.                    " P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU1'.

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
*&      Form  P1000_IMPORT_DOC_CHEKC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSBSEG_ZFIMDNO  text
*      -->P_IT_ZSBSEG_ZFDCNM  text
*      -->P_IT_ZSBSEG_ZFPOYN  text
*      -->P_2006   text
*      -->P_IT_ZSBSEG_KOSTL  text
*----------------------------------------------------------------------*
FORM P1000_IMPORT_DOC_CHEKC USING    P_ZFIMDNO
                                     P_ZFDCNM
                                     P_ZFPOYN
                                     P_KOSTL
                                     ZTBKPF-GSBER
                                     ZTBKPF-BUPLA.

   CLEAR : P_ZFDCNM, ZTREQHD.

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

         IF W_SUBRC EQ 0.
            CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
                 EXPORTING
                    UNAME   =    SY-UNAME
                    WERKS   =    ZTREQHD-ZFWERKS
                 IMPORTING
                    GSBER   =    ZTBKPF-GSBER
                    BUPLA   =    ZTBKPF-BUPLA.
         ENDIF.

      WHEN OTHERS.
   ENDCASE.
ENDFORM.                    " P1000_IMPORT_DOC_CHEKC

*&---------------------------------------------------------------------*
*&      Form  P2000_VERIFY_HKONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_VERIFY_HKONT.

    SELECT SINGLE * FROM ZTIMIMG00.

    SELECT SINGLE * FROM LFA1
                   WHERE LIFNR EQ P_HKONT.
    IF SY-SUBRC NE 0.
        MESSAGE S585 WITH '사원벤더' P_HKONT.
        STOP.
    ELSE.
       IF LFA1-KTOKK NE ZTIMIMG00-KTOKK.
          MESSAGE E408(ZIM1) WITH LFA1-KTOKK ZTIMIMG00-KTOKK.
       ENDIF.
    ENDIF.
ENDFORM.                    " P2000_VERIFY_HKONT
