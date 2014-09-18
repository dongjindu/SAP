*&---------------------------------------------------------------------*
*& Report  ZRIMINSLST                                                  *
*&---------------------------------------------------------------------*
*&  Program    : Insurance Covering Request List                       *
*&  Created by : Lee  INFOLINK Ltd.                                    *
*&  Created on : 2001.09.20                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&               Display Insurance Covering Request List
*&---------------------------------------------------------------------*
*& [Change Contents]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMINSLST MESSAGE-ID ZIM
                   LINE-SIZE 104
                   NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFREQNO      LIKE ZTINS-ZFREQNO,     " Import Request No
       ZFINSEQ      LIKE ZTINS-ZFINSEQ,     " Insurance sequence
       ZFAMDNO      LIKE ZTINS-ZFAMDNO,     " AMENDNO.
       EBELN        LIKE ZTREQHD-EBELN,     " P/O No.
       ZFETC1       LIKE ZTINS-ZFETC1,      " other condition
       W_AMDNO      LIKE ZTREQST-ZFAMDNO,   " L/C AMDNO.
       ZFREQTY      LIKE ZTREQHD-ZFREQTY,   " Import Type
       REQTY        LIKE DD07T-DDTEXT,      " Text
       ZFINSDT      LIKE ZTINS-ZFINSDT,     " Insurance start date
       ZFSHCUNM     LIKE ZTINSSG3-ZFSHCUNM, " Loading Area
       INCO1        LIKE ZTREQHD-INCO1,     " Incoterms.
       ZFCNCDNM     LIKE ZTINSAGR-ZFCNCDNM, " Insurance condition
       ZFLASTAM     LIKE ZTREQHD-ZFLASTAM,  " Open amount
       ZFARCUNM     LIKE ZTINSSG3-ZFARCUNM, " Arriving area
       ZFTRANS      LIKE ZTINS-ZFTRANS,     " Transportation method
       TRANS        LIKE DD07T-DDTEXT,      " Transportation text
        ZFIVAMT     LIKE ZTINS-ZFIVAMT,     " Invoice Amount
        WAERS       LIKE ZTINS-WAERS.       " Invoice Amount currency
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------
INCLUDE   ZRIMISNSTOP.
INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.
*-----------------------------------------------------------------------
* Selection Screen.
*-----------------------------------------------------------------------

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS  FOR ZTREQHD-BUKRS NO-EXTENSION NO INTERVALS,
                   S_EBELN  FOR ZTREQHD-EBELN,   " P/O No.
                   S_OPNNO  FOR ZTREQHD-ZFOPNNO, " L/C No.
                   S_OPBN   FOR ZTREQHD-ZFOPBN,  " L/C Open bank
                   S_BENI   FOR ZTREQHD-ZFBENI,  " Beneficiary.
                   S_INCO1  FOR ZTREQHD-INCO1,   " Incoterms.
                   S_REQTY  FOR ZTREQHD-ZFREQTY, " Import Type
                   S_REQNO  FOR ZTINS-ZFREQNO,   " Import Request No
                   S_INSDT  FOR ZTINS-ZFINSDT.   " Insurance start date
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
   PERFORM   P1000_SET_BUKRS.
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
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
      WHEN 'STUP' OR 'STDN'.              " SORT 선택?
         W_FIELD_NM = 'ZFOPBN'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.

      WHEN 'DSIN'.                       " Insurance 조회.
            PERFORM P2000_SHOW_INS USING IT_TAB-ZFREQNO
                                         IT_TAB-ZFINSEQ
                                         IT_TAB-ZFAMDNO.
      WHEN 'DSRQ'.                       " L/C 조회.
            PERFORM P2000_SHOW_LC USING IT_TAB-ZFREQNO
                                        IT_TAB-W_AMDNO.
      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT USING W_ERR_CHK.
            PERFORM RESET_LIST.
      WHEN 'EXCU'.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S962.
            ELSE.
               PERFORM P2000_REQ_INS.
           ENDIF.

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
   CLEAR : IT_ERR_LIST.


*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
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
     WHEN OTHERS.
  ENDCASE.




*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET TITLEBAR 'ZIMR14'.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  WRITE : /40  '[ 적하보험의뢰현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,'',
            SY-VLINE, (10) 'P/O NO.',
            SY-VLINE, (20) '결제구분',
            SY-VLINE, (30) '부보조건',
            SY-VLINE, (14) '선적지',
            SY-VLINE, (10) 'Incoterms',
            SY-VLINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /  SY-VLINE,'',
             SY-VLINE, (10) '개시일',
             SY-VLINE, (20) '보험가액',
             SY-VLINE, (30) '기타조건',
             SY-VLINE, (14) '도착지',
             SY-VLINE, (10) '운송구분',SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
          FROM ZTREQHD AS R INNER JOIN ZTINS AS I
          ON R~ZFREQNO = I~ZFREQNO
         WHERE I~ZFREQNO   IN  S_REQNO
           AND R~BUKRS     IN  S_BUKRS
           AND I~ZFINSDT   IN  S_INSDT
*           AND I~ZFDOCST   EQ  'R'
           AND I~ZFDOCST   EQ  'N'
           AND R~ZFOPNNO   IN S_OPNNO
           AND R~ZFOPBN    IN S_OPBN
           AND R~ZFBENI    IN S_BENI
           AND R~EBELN     IN S_EBELN
           AND R~INCO1     IN S_INCO1
           AND R~ZFREQTY   IN S_REQTY
           AND R~ZFINSYN   EQ 'A' .           " 보험등급.

*    SELECT-OPTIONS:  S_EBELN     FOR ZTREQHD-EBELN,   " P/O No.
*                    S_OPNNO     FOR ZTREQHD-ZFOPNNO, " L/C No.
*                    S_OPBN      FOR ZTREQHD-ZFOPBN,  " L/C 개설은행.
*                    S_BENI      FOR ZTREQHD-ZFBENI,  " Beneficiary.
*                    S_INCO1     FOR ZTREQHD-INCO1,   " Incoterms.
*                    S_REQTY     FOR ZTREQHD-ZFREQTY, " 결제구분.
*                    S_REQNO     FOR ZTINS-ZFREQNO,   " 수입의뢰 번호.
*                    S_INSDT     FOR ZTINS-ZFINSDT.   " 보험개시일.

  LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      SELECT MAX( ZFSHCUNM ) MAX( ZFARCUNM )
           INTO (IT_TAB-ZFSHCUNM, IT_TAB-ZFARCUNM)    "선적지,도착지.
           FROM ZTINSSG3
          WHERE ZFREQNO = IT_TAB-ZFREQNO
            AND ZFINSEQ = IT_TAB-ZFINSEQ
            AND ZFAMDNO = IT_TAB-ZFAMDNO.
      SELECT MAX( ZFCNCDNM ) INTO IT_TAB-ZFCNCDNM  "부보 조건.
          FROM ZTINSAGR
         WHERE ZFREQNO = IT_TAB-ZFREQNO
           AND ZFINSEQ = IT_TAB-ZFINSEQ
           AND ZFAMDNO = IT_TAB-ZFAMDNO.
*        SELECT SINGLE NAME1 INTO IT_TAB-ZFINCOM_NM   "보험회사명.
*          FROM LFA1
*         WHERE LIFNR = IT_TAB-ZFOPCD.
      SELECT MAX( ZFAMDNO ) INTO IT_TAB-W_AMDNO
          FROM ZTREQST
         WHERE ZFREQNO = IT_TAB-ZFREQNO
           AND ZFDOCST IN ('O', 'A').
      SELECT SINGLE *
          FROM ZTREQST
         WHERE ZFREQNO = IT_TAB-ZFREQNO
           AND ZFAMDNO =  IT_TAB-W_AMDNO.
*        ZTREQST-ZFOPNDT  TO IT_TAB-ZFOPNDT.     "개설일.

*        SELECT SINGLE NAME1 INTO IT_TAB-ZFOPBN_NM    "개설은행명.
*                    FROM LFA1
*              WHERE LIFNR = IT_TAB-ZFOPBN.
*
*        SELECT SINGLE NAME1 INTO IT_TAB-ZFBENI_NM    "Beneficiary명.
*          FROM LFA1
*         WHERE LIFNR = IT_TAB-ZFBENI.

*         SELECT SINGLE LANDX INTO IT_TAB-LANDX        "원산지.
*           FROM T005T
*          WHERE SPRAS EQ SY-LANGU
*            AND LAND1 EQ W_ZFORIG.
*
*        SELECT MAX( ZFACDO ) INTO IT_TAB-ZFACDO      "기표.
*           FROM ZTRECST
*          WHERE ZFREQNO = ZTREQHD-ZFREQNO
*            AND ZFCSCD = '1AB'.
      PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDREQTY '
                                             IT_TAB-ZFREQTY
                                   CHANGING  IT_TAB-REQTY .
      PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDTRANS'
                                             IT_TAB-ZFTRANS
                                   CHANGING  IT_TAB-TRANS.

      MODIFY IT_TAB INDEX W_TABIX.

  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0.
     W_ERR_CHK = 'Y'.  MESSAGE S738.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'ZIMR14'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIMR14'.           " GUI TITLE SETTING..

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  SORT IT_TAB BY EBELN.

  LOOP AT IT_TAB.
     W_LINE = W_LINE + 1.
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
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / '총', W_COUNT, '건'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE : /  SY-VLINE, MARKFIELD AS CHECKBOX,
             SY-VLINE, (10) IT_TAB-EBELN,
             SY-VLINE, (20) IT_TAB-REQTY, " 결제구분.
             SY-VLINE, (30) IT_TAB-ZFCNCDNM,      " 부보조건.
             SY-VLINE, (14) IT_TAB-ZFSHCUNM, " 선적지.
             SY-VLINE, (10) IT_TAB-INCO1,
             SY-VLINE.
  HIDE : IT_TAB.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE : /  SY-VLINE,'',
             SY-VLINE, (10) IT_TAB-ZFINSDT, " 개시일',
             SY-VLINE, (03) IT_TAB-WAERS,
                       (16) IT_TAB-ZFIVAMT CURRENCY IT_TAB-WAERS,
             SY-VLINE, (30) IT_TAB-ZFETC1,  " 기타조건.
             SY-VLINE, (14) IT_TAB-ZFARCUNM, "도착지',
             SY-VLINE, (10) IT_TAB-TRANS,    "운송구분.
             SY-VLINE.
* Hide
  HIDE : IT_TAB.
  WRITE : / SY-ULINE.
  W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING   P_ZFREQNO  P_ZFAMDNO.

   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.

   IF P_ZFAMDNO = '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_INS
*&---------------------------------------------------------------------*
FORM P2000_SHOW_INS USING    P_ZFREQNO
                             P_ZFINSEQ
                             P_ZFAMDNO.

   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   SET PARAMETER ID 'ZPINSEQ'   FIELD P_ZFINSEQ.
   SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
   EXPORT 'BES'       TO MEMORY ID 'BES'.
   EXPORT 'ZPOPNNO'   TO MEMORY ID 'ZPOPNNO'.
   EXPORT 'ZPREQNO'   TO MEMORY ID 'ZPREQNO'.
   EXPORT 'ZPINSEQ'   TO MEMORY ID 'ZPINSEQ'.
   EXPORT 'ZPAMDNO'   TO MEMORY ID 'ZPAMDNO'.

   IF P_ZFAMDNO > '00000'.
      CALL TRANSACTION 'ZIM47' AND SKIP  FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM43' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_INS
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
      MOVE : IT_TAB-ZFREQNO   TO IT_SELECTED-ZFREQNO,
             IT_TAB-ZFAMDNO  TO IT_SELECTED-ZFAMDNO,
             IT_TAB-ZFINSEQ  TO IT_SELECTED-ZFINSEQ.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_REQ_INS
*&---------------------------------------------------------------------*
FORM P2000_REQ_INS.

   REFRESH : IT_ERR_LIST.

   LOOP AT IT_SELECTED.

      W_TABIX = SY-TABIX.
*>>> 진행상태바..

      line = ( sy-tabix / w_selected_lines ) * 100.
      out_text = 'JOB PROGRESS %99999%%'.
      replace '%99999%' with line into out_text.
      perform p2000_show_bar using out_text line.

      W_SUBRC = 0.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTINS'
           EXPORTING
                 ZFREQNO = IT_SELECTED-ZFREQNO
                 ZFINSEQ = IT_SELECTED-ZFINSEQ
                 ZFAMDNO = IT_SELECTED-ZFAMDNO
           EXCEPTIONS
                 OTHERS  = 1.

      IF SY-SUBRC NE 0.
         PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
         CONTINUE.
      ENDIF.

      CALL FUNCTION 'ZIM_INSURANCE_LGI_TO_LG'
           EXPORTING
              ZFREQNO   = IT_SELECTED-ZFREQNO
              ZFINSEQ   = IT_SELECTED-ZFINSEQ
              ZFAMDNO   = IT_SELECTED-ZFAMDNO
           EXCEPTIONS
              NOT_FOUND   =  4
              NOT_SELECT  =  8
              RCV_ERROR   =  10.
      W_SUBRC  =  SY-SUBRC.

      CASE W_SUBRC.
         WHEN 0.
           MESSAGE S928 WITH  IT_SELECTED-ZFREQNO IT_SELECTED-ZFINSEQ
                              IT_SELECTED-ZFAMDNO.
         WHEN 4.
           MESSAGE S169 WITH  IT_SELECTED-ZFREQNO.
         WHEN 8.
           MESSAGE S929 WITH  IT_SELECTED-ZFREQNO IT_SELECTED-ZFINSEQ
                              IT_SELECTED-ZFAMDNO.
         WHEN 10.
           MESSAGE S930 WITH  IT_SELECTED-ZFREQNO IT_SELECTED-ZFINSEQ
                              IT_SELECTED-ZFAMDNO.
      ENDCASE.

      PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.

      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTINS'
           EXPORTING
                 ZFREQNO = IT_SELECTED-ZFREQNO
                 ZFINSEQ = IT_SELECTED-ZFINSEQ
                 ZFAMDNO = IT_SELECTED-ZFAMDNO
           EXCEPTIONS
                 OTHERS  = 1.

   ENDLOOP.

   INCLUDE = 'POPU'.
   CALL SCREEN 0100 STARTING AT  04   3
                    ENDING   AT 100  12.
   CLEAR : INCLUDE.

   LEAVE TO SCREEN 0.

ENDFORM.                    " P2000_REQ_INS
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.
*  IF W_STATUS_CHK = 'D'.
*     SET PF-STATUS 'STDLISA'.
*  ELSE.
     SET PF-STATUS 'STDLISW'.
*  ENDIF.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH '메시지 LIST'.
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
         WRITE : / SY-ULINE(96), / SY-VLINE NO-GAP,
                   '유형'   NO-GAP, SY-VLINE NO-GAP,
*                   ' 통관요청 ' NO-GAP, SY-VLINE NO-GAP,
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
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4) NO-GAP,
*                      SY-VLINE NO-GAP, IT_ERR_LIST-ZFIVNO  NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(87) NO-GAP,
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
         WRITE : / SY-ULINE(96).
         CLEAR : IT_ERR_LIST.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST.

   MOVE : SY-MSGTY            TO     IT_ERR_LIST-MSGTYP,
          SY-MSGID            TO     IT_ERR_LIST-MSGID,
          SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
          SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
          SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
          SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
          SY-MSGV4            TO     IT_ERR_LIST-MSGV4.

   IF W_SUBRC NE 0.
      IT_ERR_LIST-MSGTYP = 'E'.
   ENDIF.
*          IT_SELECTED-ZFIVNO  TO     IT_ERR_LIST-ZFIVNO.

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

  SKIP 2.
  WRITE : /40  '[ 적하보험의뢰현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,'',
            SY-VLINE, (10) 'P/O NO.',
            SY-VLINE, (20) '결제구분',
            SY-VLINE, (30) '부보조건',
            SY-VLINE, (14) '선적지',
            SY-VLINE, (10) 'Incoterms',
            SY-VLINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /  SY-VLINE,'',
             SY-VLINE, (10) '개시일',
             SY-VLINE, (20) '보험가액',
             SY-VLINE, (30) '기타조건',
             SY-VLINE, (14) '도착지',
             SY-VLINE, (10) '운송구분',SY-VLINE.
  WRITE : / SY-ULINE.


ENDFORM.                    " P3000_TITLE_WRITE_EN
