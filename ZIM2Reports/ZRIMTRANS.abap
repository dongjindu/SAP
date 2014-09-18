*&-----------------------------------------------------------------
*& Report  ZRIMTRANS
*&-----------------------------------------------------------------
*&  프로그램명 : Import System Basic Data Client Transferring
*&      작성자 : 백진영 INFOLINK Ltd.
*&      작성일 : 2001.05.18
*&  적용회사PJT:
*&-----------------------------------------------------------------
*& [변경내용]
*&
*&-----------------------------------------------------------------
REPORT  ZRIETRANS  MESSAGE-ID ZIES.
TABLES: ZTIEPORT,  " 선적항, 도착항.
        ZTIMIMG00, " 수입시스템 Basic Configuration.
        ZTIMIMG01, " Payment Term Configuration.
        ZTIMIMG02, " 세관 코드.
        ZTIMIMG03, " 보세구역 코드.
        ZTIMIMG04, " Planned Cost Rate 관리.
        ZTIMIMG05, " 보세운송 운임단가 관리.
        ZTIMIMG06, " 관세청 고시환율 관리.
        ZTIMIMG07, " 통관수수료율 관리.
        ZTIMIMG08, " 관리코드 관리.
        ZTIMIMG09, " HS코드별 관세율 관리.
        ZTIMIMG10, " 관세사 관리.
        ZTIMIMG11, " G/R, I/V, 비용처리 Configuration.
        ZTIMIMG12, " 운송수단 MATCH CODE.
        ZTIMIMG17, " 항공화물해외운송 요율 관리.
        ZTIMIMG18, " 수입시스템 자재관리.
        ZTIMIMG19, " 사용자별 사업영역 정의.
        ZTIMIMGTX. " EDI BASIC CONFIG

CONSTANTS MARK VALUE 'X'.

SELECTION-SCREEN BEGIN OF BLOCK OUT WITH FRAME.
   SELECTION-SCREEN BEGIN OF BLOCK ORDER WITH FRAME TITLE TEXT-TL1.
      PARAMETERS : VAL_FROM  LIKE ZTIMIMG00-MANDT  OBLIGATORY,
                   VAL_TO    LIKE ZTIMIMG00-MANDT  OBLIGATORY.
   SELECTION-SCREEN END OF BLOCK ORDER.

   SELECTION-SCREEN BEGIN OF BLOCK TABLE WITH FRAME TITLE TEXT-TL2.
*      SELECTION-SCREEN PUSHBUTTON 71(2) PUBU
*                 USER-COMMAND UCOM_ALL.
*
      SELECTION-SCREEN BEGIN OF LINE.
         SELECTION-SCREEN COMMENT 1(21) TEXT-TT0.
         PARAMETERS   ZT0 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 30(20) TEXT-TT1.
         PARAMETERS   ZT1 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 57(15) TEXT-TT2.
         PARAMETERS   ZT2 AS CHECKBOX.
      SELECTION-SCREEN END OF LINE .

      SELECTION-SCREEN BEGIN OF LINE.
         SELECTION-SCREEN COMMENT 1(21) TEXT-TT5.
         PARAMETERS   ZT5 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 30(20) TEXT-TT4.
         PARAMETERS   ZT4 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 57(15) TEXT-TT3.
         PARAMETERS   ZT3 AS CHECKBOX.
      SELECTION-SCREEN END OF LINE .

      SELECTION-SCREEN BEGIN OF LINE.
         SELECTION-SCREEN COMMENT 1(21) TEXT-TT6.
         PARAMETERS   ZT6 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 30(20) TEXT-TT7.
         PARAMETERS   ZT7 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 57(15) TEXT-TT8.
         PARAMETERS   ZT8 AS CHECKBOX.
      SELECTION-SCREEN END OF LINE .

      SELECTION-SCREEN BEGIN OF LINE.
         SELECTION-SCREEN COMMENT 1(21) TEXT-TT9.
         PARAMETERS   ZT9 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 30(20) TEXT-T11.
         PARAMETERS   ZT11 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 57(15) TEXT-T10.
         PARAMETERS   ZT10 AS CHECKBOX.
      SELECTION-SCREEN END OF LINE .

      SELECTION-SCREEN BEGIN OF LINE.
         SELECTION-SCREEN COMMENT 1(21) TEXT-T17.
         PARAMETERS   ZT17 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 30(20) TEXT-T12.
         PARAMETERS   ZT12 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 57(15) TEXT-T13.
         PARAMETERS   ZT13 AS CHECKBOX.
      SELECTION-SCREEN END OF LINE .

      SELECTION-SCREEN BEGIN OF LINE.
         SELECTION-SCREEN COMMENT 1(21) TEXT-T18.
         PARAMETERS   ZT18 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 30(20) TEXT-T19.
         PARAMETERS   ZT19 AS CHECKBOX.
         SELECTION-SCREEN COMMENT 57(15) TEXT-TTX.
         PARAMETERS   ZTTX AS CHECKBOX.
      SELECTION-SCREEN END OF LINE .

   SELECTION-SCREEN END OF BLOCK TABLE.

SELECTION-SCREEN END OF BLOCK OUT.

START-OF-SELECTION.

*  Basic Configuration
   IF MARK EQ ZT0.
      SELECT * FROM ZTIMIMG00.
         MOVE VAL_TO TO ZTIMIMG00-MANDT.
         MODIFY ZTIMIMG00 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH 'Basic Configuration'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  Payment Term Configuration
   IF MARK EQ ZT1.
      SELECT * FROM ZTIMIMG01.
         MOVE VAL_TO TO ZTIMIMG01-MANDT.
         MODIFY ZTIMIMG01 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH 'Payment Term Configuration'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  세관 코드.
   IF MARK EQ  ZT2.
      SELECT * FROM ZTIMIMG02.
         MOVE VAL_TO TO ZTIMIMG02-MANDT.
         MODIFY ZTIMIMG02 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH '세관 코드'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  보세구역 코드.
   IF MARK EQ  ZT3.
      SELECT * FROM ZTIMIMG03.
         MOVE VAL_TO TO ZTIMIMG03-MANDT.
         MODIFY ZTIMIMG03 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH '보세구역 코드'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  Planned Cost Rate.
   IF MARK EQ ZT4.
      SELECT * FROM ZTIMIMG04.
         MOVE VAL_TO TO ZTIMIMG04-MANDT.
         MODIFY ZTIMIMG04 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH 'Planned Cost Rate'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  보세운송 운임단가.
   IF MARK EQ ZT5.
      SELECT * FROM ZTIMIMG05.
         MOVE VAL_TO TO ZTIMIMG05-MANDT.
         MODIFY ZTIMIMG05 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH '보세운송 운임단가'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  관세청 고시환율.
   IF MARK EQ ZT6.
      SELECT * FROM ZTIMIMG06.
         MOVE VAL_TO TO ZTIMIMG06-MANDT.
         MODIFY ZTIMIMG06 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH '관세청 고시환율'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  통관수수료율.
   IF MARK EQ ZT7.
      SELECT * FROM ZTIMIMG07.
         MOVE VAL_TO TO ZTIMIMG07-MANDT.
         MODIFY ZTIMIMG07 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH '통관수수료율'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  관리코드.
   IF MARK EQ ZT8.
      SELECT * FROM ZTIMIMG08.
         MOVE VAL_TO TO ZTIMIMG08-MANDT.
         MODIFY ZTIMIMG08 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH '관리코드'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  HS코드별 관세율.
   IF MARK EQ ZT9.
      SELECT * FROM ZTIMIMG09.
         MOVE VAL_TO TO ZTIMIMG09-MANDT.
         MODIFY ZTIMIMG09 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH 'HS코드별 관세율'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  관세사.
   IF MARK EQ ZT10.
      SELECT * FROM ZTIMIMG10.
         MOVE VAL_TO TO ZTIMIMG10-MANDT.
         MODIFY ZTIMIMG10 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH '관세사'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  G/R, I/V, 비용처리 Configuration.
   IF MARK EQ ZT11.
      SELECT * FROM ZTIMIMG11.
         MOVE VAL_TO TO ZTIMIMG11-MANDT.
         MODIFY ZTIMIMG11 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000.
         ELSE.
            MESSAGE S001 WITH 'G/R, I/V, 비용처리 Configuration'.
         ENDIF.
      ENDIF.
   ENDIF.

*  운송수단 MATCH CODE 관리.
   IF MARK EQ ZT12.
      SELECT * FROM ZTIMIMG12.
         MOVE VAL_TO TO ZTIMIMG12-MANDT.
         MODIFY ZTIMIMG12 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000.
         ELSE.
            MESSAGE S001 WITH '운송수단 MATCH CODE'.
         ENDIF.
      ENDIF.
   ENDIF.

*  선적항, 도착항관리.
   IF MARK EQ ZT13.
      SELECT * FROM ZTIEPORT.
         MOVE VAL_TO TO ZTIEPORT-MANDT.
         MODIFY ZTIEPORT CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000.
         ELSE.
            MESSAGE S001 WITH '선적항, 도착항 관리'.
         ENDIF.
      ENDIF.
   ENDIF.

*  항공화물해외운송 요율 관리.
   IF MARK EQ ZT17.
      SELECT * FROM ZTIMIMG17.
         MOVE VAL_TO TO ZTIMIMG17-MANDT.
         MODIFY ZTIMIMG17 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH '항공화물해외운송 요율 관리'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  수입시스템 자재관리.
   IF MARK EQ ZT18.
      SELECT * FROM ZTIMIMG18.
         MOVE VAL_TO TO ZTIMIMG18-MANDT.
         MODIFY ZTIMIMG18 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH '수입시스템 자재관리'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  사용자별 사업영역 지정.
   IF MARK EQ ZT19.
      SELECT * FROM ZTIMIMG19.
         MOVE VAL_TO TO ZTIMIMG19-MANDT.
         MODIFY ZTIMIMG19 CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH '사용자별 사업영역 지정'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.

*  수입시스템 EDI Basic Config.
   IF MARK EQ ZT19.
      SELECT * FROM ZTIMIMGTX.
         MOVE VAL_TO TO ZTIMIMGTX-MANDT.
         MODIFY ZTIMIMGTX CLIENT SPECIFIED.
      ENDSELECT.
      IF SY-DBCNT > 0.
         IF SY-SUBRC NE 0.
            MESSAGE A000 WITH 'EDI Basic Config'.
         ELSE.
            MESSAGE S001.
         ENDIF.
      ENDIF.
   ENDIF.
