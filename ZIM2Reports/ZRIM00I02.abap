*&---------------------------------------------------------------------*
*& INCLUDE ZRIM00I02 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 Amend Main PAI MODULE Include                *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.11                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  REQ_READ_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE REQ_READ_DOC_SCRCOM INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

   IF NOT ZSREQHD-ZFREQNO IS INITIAL.
* 수입의뢰 문서 조?
      PERFORM   P1000_READ_REQ_DOC.
      IF SY-TCODE NE 'ZIMC1'.   " L/C COST가 아닐 경?
         CASE ZTREQHD-ZFREQTY.
            WHEN 'LC'.          " Import Master L/C 문서 조?
               PERFORM   P1000_READ_MASTER_LC.
               PERFORM   P2000_MOVE_LC_AMAND.
            WHEN 'LO'.          " Local L/C 문서 조?
               PERFORM   P1000_READ_LOCAL_LC.
*              PERFORM   P2000_MOVE_LC_AMAND.
            WHEN 'PU'.          " 구매승인?
               PERFORM   P1000_READ_PURCH_DOC.
*              PERFORM   P2000_MOVE_LC_AMAND.
            WHEN 'DA' OR 'DP' OR 'TT'.
*              PERFORM   P2000_MOVE_LC_AMAND.
         ENDCASE.
      ENDIF.
   ENDIF.

ENDMODULE.                 " REQ_READ_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  REQIT_GET_LINE_SCR1106  INPUT
*&---------------------------------------------------------------------*
MODULE REQIT_GET_LINE_SCR1106 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1106-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " REQIT_GET_LINE_SCR1106  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4500 INPUT.
  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
* 손보사 상?
     IF ZTINS-ZFCONN1 IS INITIAL AND ZTINS-ZFCONN2 IS INITIAL.
        SELECT SINGLE * FROM ZTIMIMGTX
               WHERE    BUKRS  EQ   ZTREQHD-BUKRS.
        IF SY-SUBRC NE 0.   MESSAGE E949 WITH ZTREQHD-BUKRS.   ENDIF.
      ENDIF.

      MOVE : 'N'          TO    ZTINS-ZFDOCST,
             'N'          TO    ZTINS-ZFEDIST,
             'X'          TO    ZTINS-ZFEDICK,
             'X'          TO    ZTINS-ZFEDICK,
             '43'         TO    ZTINS-ZFEDFU,
             ''           TO    ZTINS-ZFIVYN,
             ''           TO    ZTINS-ZFDUYN,
             ''           TO    ZTINS-ZFINAYN,
             ''           TO    ZTINS-ZFTAMYN,
             ''           TO    ZTINS-ZFPEYN,
             ''           TO    ZTINS-ZFPDYN,
             ''           TO    ZTINS-ZFDTYN,
             ''           TO    ZTINS-ZFPRYN,
             ''           TO    ZTINS-ZFDOYN,
             ''           TO    ZTINS-ZFTMYN,
             ''           TO    ZTINS-ZFHSYN,
             ''           TO    ZTINS-ZFGDYN,
             ''           TO    ZTINS-ZFPAYN,
             ''           TO    ZTINS-ZFEIYN,
             ''           TO    ZTINS-ZFCDYN,
             ''           TO    ZTINS-ZFETYN,
             ''           TO    ZTINS-ZFADYN,
             ''           TO    ZTINS-ZFDOCNO,   " 전자문서번?
             SY-DATUM     TO    ZTINS-ZFINSDT,   " 보험 개시?
             SY-UNAME     TO    ZTINS-ERNAM,
             SY-DATUM     TO    ZTINS-CDAT,
             SY-UNAME     TO    ZTINS-UNAM,
             SY-DATUM     TO    ZTINS-UDAT.

*
*      IF ZTREQHD-ZFUSDAM GE 10000.
         MOVE : ZTREQHD-ZFLASTAM   TO    ZTINS-ZFIVAMT.
*      ELSE.
*         MOVE : 0                  TO    ZTINS-ZFIVAMT.
*      ENDIF.
      IF ZTINS-ZFALCP NE 0.
          ZTINS-ZFIVAMT =  ZTINS-ZFIVAMT +
                           ZTINS-ZFIVAMT * ( ZTINS-ZFALCP / 100 ).
      ENDIF.

      MOVE : ''                 TO    ZTINSRSP-ZFISDT,
             0                  TO    ZTINS-ZFINAMT,
             0                  TO    ZTINS-ZFKRWAMT,
             ''                 TO    ZTINS-ZFINNO,
             0                  TO    ZTINSRSP-ZFTAMI,
             0                  TO    ZTINSRSP-ZFCAMI,
             0                  TO    ZTINSRSP-ZFDAMI,
             0                  TO    ZTINSRSP-ZFTPR,
             0                  TO    ZTINSRSP-ZFCPR,
             0                  TO    ZTINSRSP-ZFDPR,
             0                  TO    ZTINSRSP-ZFVPR,
             0                  TO    ZTINSRSP-ZFIPR.

      W_STATUS = C_REQ_C.
      SET SCREEN 4501.  LEAVE TO SCREEN 4501.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_SCR4500  INPUT
*&---------------------------------------------------------------------*
*&      Module  AMEND_DOC_DISPLAY_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE AMEND_DOC_DISPLAY_SCRCOM INPUT.

   IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
      DATA : WL_ERR_FLAG.
      PERFORM  P2000_SET_INIT_DATA_MOVE  USING WL_ERR_FLAG.
      IF WL_ERR_FLAG = 'Y'.
        MESSAGE S977 WITH
        'Payment type is not correct with amend function.'.
        EXIT.
      ENDIF.
      PERFORM  P2000_SET_REQ_SCR.
   ENDIF.

ENDMODULE.                 " AMEND_DOC_DISPLAY_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSMLCAMNARR_SET_UPDATE  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSMLCAMNARR_SET_UPDATE INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSMLCAMNARR WITH KEY ZFLNARR = ZSMLCAMNARR-ZFLNARR.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSMLCAMNARR TO IT_ZSMLCAMNARR.

  IF W_SY_SUBRC EQ 0.
     MODIFY IT_ZSMLCAMNARR INDEX W_TABIX.
  ELSE.
     IT_ZSMLCAMNARR-ZFLNARR = TC_1106-CURRENT_LINE * 10.
     APPEND IT_ZSMLCAMNARR.
  ENDIF.

ENDMODULE.                 " IT_ZSMLCAMNARR_SET_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR1106_MARK_TC_1106  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR1106_MARK_TC_1106 INPUT.

   READ TABLE IT_ZSMLCAMNARR WITH KEY ZFLNARR = ZSMLCAMNARR-ZFLNARR
                                      BINARY SEARCH.

   IF SY-SUBRC = 0.
*       MOVE-CORRESPONDING   ZSMLCAMNARR TO IT_ZSMLCAMNARR.
        IF NOT ( W_ROW_MARK IS INITIAL ).
           IT_ZSMLCAMNARR-ZFMARK = 'X'.
        ELSE.
           CLEAR : IT_ZSMLCAMNARR-ZFMARK.
        ENDIF.
        MODIFY IT_ZSMLCAMNARR INDEX SY-TABIX.
     ENDIF.

ENDMODULE.                 " SET_SCR1106_MARK_TC_1106  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR1106  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR1106 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
     W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
     CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.       " 삭제 및 취?

      LOOP AT IT_ZSMLCAMNARR WHERE ZFMARK NE SPACE.
        DELETE IT_ZSMLCAMNARR INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSMLCAMNARR.
        IT_ZSMLCAMNARR-ZFMARK = W_MARK.   MODIFY IT_ZSMLCAMNARR.
      ENDLOOP.

    WHEN 'REF1'.           " Refresh
      REFRESH : IT_ZSMLCAMNARR.
      LOOP AT IT_ZSMLCAMNARR_ORG.
         MOVE-CORRESPONDING   IT_ZSMLCAMNARR_ORG   TO   IT_ZSMLCAMNARR.
         APPEND IT_ZSMLCAMNARR.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

  PERFORM   P2000_IT_ZSMLCAMNARR_UPDATE.


ENDMODULE.                 " USER_COMMAND_SCR1106  INPUT
*&---------------------------------------------------------------------*
*&      Module  SHIP_PORT_SCR1102  INPUT
*&---------------------------------------------------------------------*
MODULE SHIP_PORT_SCR1102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.

  CASE ZTREQHD-ZFREQTY.
     WHEN 'LC'.
       ASSIGN ZTREQHD-ZFSPRT       TO <FS_F>.
       PERFORM P2000_SPACE_CUT USING <FS_F>.
       PERFORM SPECIAL_CHAR_SEARCH USING <FS_F> 'E'.
*      CHECK : ZTREQST-ZFEDICK NE 'X'.
       IF ZTREQHD-ZFSPRT NE ZTREQHD_TMP-ZFSPRT.
          MOVE : ZTREQHD-ZFSPRT   TO ZTMLCAMHD-ZFNSPRT.   " 선적?
       ELSE.
          CLEAR : ZTMLCAMHD-ZFNSPRT.
       ENDIF.
     WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " SHIP_PORT_SCR1102  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRANS_PORT_SCR1102  INPUT
*&---------------------------------------------------------------------*
MODULE TRANS_PORT_SCR1102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.

  CASE ZTREQHD-ZFREQTY.
     WHEN 'LC'.
       ASSIGN ZTREQHD-ZFAPRT       TO <FS_F>.
       PERFORM P2000_SPACE_CUT USING <FS_F>.
       PERFORM SPECIAL_CHAR_SEARCH USING <FS_F> 'E'.
*      CHECK : ZTREQST-ZFEDICK NE 'X'.
       IF ZTREQHD-ZFAPRT NE ZTREQHD_TMP-ZFAPRT.
          MOVE : ZTREQHD-ZFAPRT     TO ZTMLCAMHD-ZFNAPRT.   " 선적?
       ELSE.
          CLEAR : ZTMLCAMHD-ZFNAPRT.
       ENDIF.
     WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " TRANS_PORT_SCR1102  INPUT
*&---------------------------------------------------------------------*
*&      Module  APP_DATE_SCR1102  INPUT
*&---------------------------------------------------------------------*
MODULE APP_DATE_SCR1102 INPUT.
* if display mode, exit.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D OR
     W_STATUS = C_REQ_C.
     EXIT.
  ENDIF.

  CASE ZTREQHD-ZFREQTY.
     WHEN 'LC' OR 'LO' OR 'PU' OR 'TT'.
        IF W_STATUS = C_ADD_U OR W_STATUS = C_OPEN_C.
           IF ZTREQST-ZFAPPDT IS INITIAL.
              MESSAGE E167 WITH 'Opening expected date'.
           ENDIF.
        ENDIF.
     WHEN OTHERS.
  ENDCASE.

*  IF ZTREQHD-ZFREQTY EQ 'LO'.
*    IF W_STATUS = C_ADD_U OR W_STATUS = C_OPEN_C.
**>>>2000/12/27 KSB 막음
*        EXIT.
**----------------------------------------------------------------------
**LOCAL일 경우, 2개월 후의 일자를 GET하여 ==> 유효일자?
**----------------------------------------------------------------------
*        CALL FUNCTION 'ZIM_BEFORE_N_MONTH_DATE'
*             EXPORTING
*                  DATE                      = ZTREQST-ZFAPPDT
*                  W_MONTH                   = 2
*             IMPORTING
*                  W_OUT_DATE                = W_DATE
*             EXCEPTIONS
*                  PLAUSIBILITY_CHECK_FAILED = 4.
*
*        IF SY-SUBRC NE 0.
*           MESSAGE E133 WITH ZTREQST-ZFAPPDT.
*        ELSE.
*           IF ZTREQHD-ZFREQED IS INITIAL.
*              ZTREQHD-ZFREQED = W_DATE.
*           ENDIF.
*        ENDIF.
*     ENDIF.
*   ENDIF.

ENDMODULE.                 " APP_DATE_SCR1102  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXPIRY_DATE_SCR1102  INPUT
*&---------------------------------------------------------------------*
MODULE EXPIRY_DATE_SCR1102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.

  CASE ZTREQHD-ZFREQTY.
     WHEN 'LC' OR 'LO'.
        IF ZTREQHD-ZFREQED IS INITIAL.
           MESSAGE E167 WITH 'Effective date(E/D)'.
        ENDIF.
     WHEN OTHERS.
  ENDCASE.

  PERFORM   P2000_EXPIRY_DATE_CHECK.

ENDMODULE.                 " EXPIRY_DATE_SCR1102  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR1300  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCR1300 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-EBELN IS INITIAL AND     " P/O NO를 입력하지 않을 경?
     ZSREQHD-ZFREQNO IS INITIAL AND   " 관리번호가 입력하지 않을 경?
     ZSREQHD-ZFOPNNO IS INITIAL.      " 문서번호가 입력하지 않을 경?
     MESSAGE E066.
  ENDIF.
*
  IF NOT ZSREQHD-ZFOPNNO IS INITIAL.  " 문서번호가 입력된 경?
* 문서 승인번?
     IF ZSREQHD-ZFAMDNO IS INITIAL.
        SELECT COUNT( * ) INTO  W_COUNT
                          FROM  ZTREQST
                          WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                          AND   ZFAMDNO GT '00000'.
     ELSE.
        SELECT COUNT( * ) INTO  W_COUNT
                          FROM  ZTREQST
                          WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                          AND   ZFAMDNO EQ ZSREQHD-ZFAMDNO.
     ENDIF.
     CASE W_COUNT.
        WHEN 0.     MESSAGE E067 WITH ZSREQHD-ZFOPNNO.
        WHEN 1.
           IF ZSREQHD-ZFAMDNO IS INITIAL.
              SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                             FROM ZTREQST
                             WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                             AND   ZFAMDNO GT '00000'.
                   EXIT.
              ENDSELECT.
           ELSE.
              SELECT ZFREQNO INTO ZSREQHD-ZFREQNO UP TO 1 ROWS
                             FROM ZTREQST
                             WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
                             AND   ZFAMDNO EQ ZSREQHD-ZFAMDNO.
                   EXIT.
              ENDSELECT.
           ENDIF.
        WHEN OTHERS.
           PERFORM P2000_AMEND_DOC_SELECT.
           IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
           PERFORM P2000_SEARCH_FIELD_MOVE.
     ENDCASE.

  ENDIF.

  IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
     IF ZSREQHD-ZFAMDNO IS INITIAL.
        SELECT COUNT( * ) INTO  W_COUNT
                       FROM  ZVREQHD_ST
                       WHERE EBELN   EQ ZSREQHD-EBELN
                       AND   ZFAMDNO GT '00000'.
     ELSE.
        SELECT COUNT( * ) INTO  W_COUNT
                       FROM  ZVREQHD_ST
                       WHERE EBELN   EQ ZSREQHD-EBELN
                       AND   ZFAMDNO EQ ZSREQHD-ZFAMDNO.
     ENDIF.
     CASE W_COUNT.
        WHEN 0.     MESSAGE E064 WITH ZSREQHD-EBELN.
        WHEN 1.
           IF ZSREQHD-ZFAMDNO IS INITIAL.
              SELECT * UP TO 1 ROWS
                          FROM ZVREQHD_ST
                          WHERE EBELN   EQ ZSREQHD-EBELN
                          AND   ZFAMDNO GT '00000'.
                   EXIT.
              ENDSELECT.
           ELSE.
              SELECT * UP TO 1 ROWS
                          FROM ZVREQHD_ST
                          WHERE EBELN   EQ ZSREQHD-EBELN
                          AND   ZFAMDNO EQ ZSREQHD-ZFAMDNO.
                   EXIT.
              ENDSELECT.
           ENDIF.
           MOVE : ZVREQHD_ST-ZFREQNO   TO     ZSREQHD-ZFREQNO,
                  ZVREQHD_ST-ZFAMDNO   TO     ZSREQHD-ZFAMDNO.
        WHEN OTHERS.
           PERFORM P2000_AMEND_DOC_SELECT_1.
           IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
           PERFORM P2000_SEARCH_FIELD_MOVE.
     ENDCASE.
  ENDIF.
* Amend No.
  IF ZSREQHD-ZFAMDNO IS INITIAL.
     SELECT COUNT( * ) INTO  W_COUNT
                    FROM  ZVREQHD_ST
                    WHERE ZFREQNO EQ ZSREQHD-ZFREQNO
                    AND   ZFAMDNO GT '00000'.
  ELSE.
     SELECT COUNT( * ) INTO  W_COUNT
                    FROM  ZVREQHD_ST
                    WHERE ZFREQNO EQ ZSREQHD-ZFREQNO
                    AND   ZFAMDNO EQ ZSREQHD-ZFAMDNO.
  ENDIF.
* COUNTER
  CASE W_COUNT.
     WHEN 0.
        IF ZSREQHD-ZFAMDNO IS INITIAL.
           MESSAGE E196 WITH ZSREQHD-ZFREQNO.
        ELSE.
           MESSAGE E197 WITH ZSREQHD-ZFREQNO ZSREQHD-ZFAMDNO.
        ENDIF.
     WHEN 1.
        IF ZSREQHD-ZFAMDNO IS INITIAL.
           SELECT * UP TO 1 ROWS
                       FROM ZVREQHD_ST
                       WHERE ZFREQNO EQ ZSREQHD-ZFREQNO
                       AND   ZFAMDNO GT '00000'.
                EXIT.
           ENDSELECT.
        ELSE.
           SELECT * UP TO 1 ROWS
                       FROM ZVREQHD_ST
                       WHERE ZFREQNO EQ ZSREQHD-ZFREQNO
                       AND   ZFAMDNO EQ ZSREQHD-ZFAMDNO.
               EXIT.
           ENDSELECT.
        ENDIF.
        MOVE : ZVREQHD_ST-ZFREQNO   TO     ZSREQHD-ZFREQNO,
               ZVREQHD_ST-ZFAMDNO   TO     ZSREQHD-ZFAMDNO.
     WHEN OTHERS.
        PERFORM P2000_AMEND_DOC_SELECT_2.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
  ENDCASE.

* 수입의뢰 문서 READ PERFORM ?
  PERFORM   P1000_REQ_DOC_READ.

ENDMODULE.                 " READ_DOC_SCR1300  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZSLLCAMSGOF_UPDATE_SCR1114  INPUT
*&---------------------------------------------------------------------*
MODULE ZSLLCAMSGOF_UPDATE_SCR1114 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSLLCAMSGOF  WITH KEY ZFLSGOF = ZSLLCOF-ZFLSGOF.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSLLCOF  TO IT_ZSLLCAMSGOF.
  MOVE : ZSLLCOF-ZFOFFER      TO IT_ZSLLCAMSGOF-ZFSGOF.

  IF W_SY_SUBRC EQ 0.
     MODIFY IT_ZSLLCAMSGOF   INDEX W_TABIX.
  ELSE.
     IT_ZSLLCAMSGOF-ZFLSGOF   = TC_0114-CURRENT_LINE * 10.
     APPEND IT_ZSLLCAMSGOF.
  ENDIF.
ENDMODULE.                 " ZSLLCAMSGOF_UPDATE_SCR1114  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_SCR1114_MARK_TC_1114  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR1114_MARK_TC_1114 INPUT.

   READ TABLE IT_ZSLLCAMSGOF   WITH KEY ZFLSGOF = ZSLLCOF-ZFLSGOF
                               BINARY SEARCH.

   IF SY-SUBRC = 0.
        MOVE-CORRESPONDING   ZSLLCOF   TO IT_ZSLLCAMSGOF.
        MOVE : ZSLLCOF-ZFOFFER      TO IT_ZSLLCAMSGOF-ZFSGOF.
        IF NOT ( W_ROW_MARK  IS INITIAL ).
           IT_ZSLLCAMSGOF-ZFMARK   = 'X'.
        ELSE.
           CLEAR : IT_ZSLLCAMSGOF-ZFMARK.
        ENDIF.
        MODIFY IT_ZSLLCAMSGOF   INDEX SY-TABIX.
     ENDIF.

ENDMODULE.                 " SET_SCR1114_MARK_TC_1114  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR1114  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR1114 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
     W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
     CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.       " 원산지 삭?

      LOOP AT IT_ZSLLCAMSGOF   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSLLCAMSGOF INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 원산지 전체선택 or 원산지 선택해?
      LOOP AT IT_ZSLLCAMSGOF.
        IT_ZSLLCAMSGOF-ZFMARK = W_MARK.   MODIFY IT_ZSLLCAMSGOF.
      ENDLOOP.

    WHEN 'REF1'.           " 원산지 Refresh
      REFRESH : IT_ZSLLCAMSGOF.
      LOOP AT IT_ZSLLCAMSGOF_ORG.
         MOVE-CORRESPONDING   IT_ZSLLCAMSGOF_ORG    TO IT_ZSLLCAMSGOF.
         APPEND IT_ZSLLCAMSGOF.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
*
*-----------------------------------------------------------------------
* ORIJIN CODE
*-----------------------------------------------------------------------
  PERFORM   P2000_ZSLLCAMSGOF_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR1114  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_IV_AMT_BIT_SCR4502  INPUT
*&---------------------------------------------------------------------*
MODULE SET_IV_AMT_BIT_SCR4502 INPUT.

   IF ZTINS-ZFIVAMT NE ZTINS_OLD-ZFIVAMT.
      ZTINS-ZFIVYN = 'X'.
   ELSE.
      CLEAR : ZTINS-ZFIVYN.
   ENDIF.

*   IF ZTINS-ZFINCD IS INITIAL.
*      ZTINS-ZFADYN = 'X'.
*   ELSE.
*      IF ZTREQHD-ZFUSDAM GT 10000.
*         CLEAR : ZTINS-ZFADYN.
*      ELSE.
*         ZTINS-ZFADYN = 'X'.
*      ENDIF.
*   ENDIF.

ENDMODULE.                 " SET_IV_AMT_BIT_SCR4502  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DOC_TY_BIT_SCR4502  INPUT
*&---------------------------------------------------------------------*
MODULE SET_DOC_TY_BIT_SCR4502 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.
* 전자문서기?
  IF ZTINS-ZFEDFU EQ '14'.
     ZTINS-ZFPAYN = 'X'.
  ELSE.
     CLEAR : ZTINS-ZFPAYN.
  ENDIF.

ENDMODULE.                 " SET_DOC_TY_BIT_SCR4502  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_CANCEL_BIT_SCR4502  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CANCEL_BIT_SCR4502 INPUT.
   IF ZTINS-ZFINCD IS INITIAL.
      ZTINS-ZFADYN = 'X'.
   ELSE.
      IF ZTREQHD-ZFUSDAM GT 10000.
         CLEAR : ZTINS-ZFADYN.
      ELSE.
         ZTINS-ZFADYN = 'X'.
      ENDIF.
   ENDIF.
ENDMODULE.                 " SET_CANCEL_BIT_SCR4502  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_HS_CODE_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_HS_CODE_BIT_SCR4104 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.
  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47'
                         OR SY-TCODE EQ 'ZIM48'.
     IF ZTINS-ZFRSTAW  NE  ZTINS_OLD-ZFRSTAW.
        ZTINS-ZFHSYN = 'X'.
     ELSE.
        CLEAR : ZTINS-ZFHSYN.
     ENDIF.
  ENDIF.

ENDMODULE.                 " SET_HS_CODE_BIT_SCR4104  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_TRANS_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
MODULE SET_TRANS_BIT_SCR4104 INPUT.

  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47'
                         OR SY-TCODE EQ 'ZIM48'.
     IF ZTINSSG3-ZFCARNU EQ ZTINSSG3_OLD-ZFCARNU AND
        ZTINSSG3-ZFCARNM EQ ZTINSSG3_OLD-ZFCARNM.
        CLEAR : ZTINS-ZFTMYN.
     ELSE.
        ZTINS-ZFTMYN = 'X'.
     ENDIF.
  ENDIF.

ENDMODULE.                 " SET_TRANS_BIT_SCR4104  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DOC_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
MODULE SET_DOC_BIT_SCR4104 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.
* 관련 서류 1
  IF ( ZTINS-ZFREDOC1 IS INITIAL AND NOT ZTINS-ZFREDON1 IS INITIAL ).
     MESSAGE E167 WITH 'Related Doc code1'.
  ENDIF.
  IF ( NOT ZTINS-ZFREDOC1 IS INITIAL AND ZTINS-ZFREDON1 IS INITIAL ).
     MESSAGE E167 WITH 'Related Doc No1'.
  ENDIF.
* 관련 서류 2
  IF ( ZTINS-ZFREDOC2 IS INITIAL AND NOT ZTINS-ZFREDON2 IS INITIAL ).
     MESSAGE E167 WITH 'Related Doc code2'.
  ENDIF.
  IF ( NOT ZTINS-ZFREDOC2 IS INITIAL AND ZTINS-ZFREDON2 IS INITIAL ).
     MESSAGE E167 WITH 'Related Doc No2'.
  ENDIF.
* 관련 서류 3
  IF ( ZTINS-ZFREDOC3 IS INITIAL AND NOT ZTINS-ZFREDON3 IS INITIAL ).
     MESSAGE E167 WITH 'Related Doc code3'.
  ENDIF.
  IF ( NOT ZTINS-ZFREDOC3 IS INITIAL AND ZTINS-ZFREDON3 IS INITIAL ).
     MESSAGE E167 WITH 'Related Doc No3'.
  ENDIF.

  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47'
                         OR SY-TCODE EQ 'ZIM48'.
     IF ( ZTINS-ZFREDOC1 EQ ZTINS_OLD-ZFREDOC1 ) AND
        ( ZTINS-ZFREDOC2 EQ ZTINS_OLD-ZFREDOC2 ) AND
        ( ZTINS-ZFREDOC3 EQ ZTINS_OLD-ZFREDOC3 ) AND
        ( ZTINS-ZFREDON1 EQ ZTINS_OLD-ZFREDON1 ) AND
        ( ZTINS-ZFREDON2 EQ ZTINS_OLD-ZFREDON2 ) AND
        ( ZTINS-ZFREDON3 EQ ZTINS_OLD-ZFREDON3 ).
        CLEAR : ZTINS-ZFDOYN.
     ELSE.
        ZTINS-ZFDOYN = 'X'.
     ENDIF.
  ENDIF.

ENDMODULE.                 " SET_DOC_BIT_SCR4104  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_PORT_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_PORT_BIT_SCR4104 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.
* 선적?
  IF ( ZTINSSG3-ZFSHCU IS INITIAL AND
       NOT ZTINSSG3-ZFSHCUNM IS INITIAL ).
     MESSAGE E167 WITH 'Shipping area code'.
  ENDIF.
  IF ( NOT ZTINSSG3-ZFSHCU IS INITIAL AND
           ZTINSSG3-ZFSHCUNM IS INITIAL ).
      PERFORM GET_ORIJIN_NAME   USING    ZTINSSG3-ZFSHCU
                                CHANGING ZTINSSG3-ZFSHCUNM.
  ENDIF.
* 도착?
  IF ( ZTINSSG3-ZFARCU IS INITIAL AND
       NOT ZTINSSG3-ZFARCUNM IS INITIAL ).
     MESSAGE E167 WITH 'Arrival area code'.
  ENDIF.
  IF ( NOT ZTINSSG3-ZFARCU IS INITIAL AND
           ZTINSSG3-ZFARCUNM IS INITIAL ).
      PERFORM GET_ORIJIN_NAME   USING    ZTINSSG3-ZFARCU
                                CHANGING ZTINSSG3-ZFARCUNM.
  ENDIF.
* 최종도착?
  IF ( ZTINSSG3-ZFLACU IS INITIAL AND
       NOT ZTINSSG3-ZFLACUNM IS INITIAL ).
     MESSAGE E167 WITH 'Final arrival area code'.
  ENDIF.
  IF ( NOT ZTINSSG3-ZFLACU IS INITIAL AND
           ZTINSSG3-ZFLACUNM IS INITIAL ).
      PERFORM GET_ORIJIN_NAME   USING    ZTINSSG3-ZFLACU
                                CHANGING ZTINSSG3-ZFLACUNM.
  ENDIF.
* 환적?
  IF ( ZTINSSG3-ZFTRCU IS INITIAL AND
       NOT ZTINSSG3-ZFTRCUNM IS INITIAL ).
     MESSAGE E167 WITH 'Partial shipment area code'.
  ENDIF.
  IF ( NOT ZTINSSG3-ZFTRCU IS INITIAL AND
           ZTINSSG3-ZFTRCUNM IS INITIAL ).
      PERFORM GET_ORIJIN_NAME   USING    ZTINSSG3-ZFTRCU
                                CHANGING ZTINSSG3-ZFTRCUNM.
  ENDIF.

  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47'
                         OR SY-TCODE EQ 'ZIM48'.
     IF ( ZTINSSG3-ZFSHCU   EQ ZTINSSG3_OLD-ZFSHCU   ) AND
        ( ZTINSSG3-ZFSHCUNM EQ ZTINSSG3_OLD-ZFSHCUNM ) AND
        ( ZTINSSG3-ZFARCU   EQ ZTINSSG3_OLD-ZFARCU   ) AND
        ( ZTINSSG3-ZFARCUNM EQ ZTINSSG3_OLD-ZFARCUNM ) AND
        ( ZTINSSG3-ZFLACU   EQ ZTINSSG3_OLD-ZFLACU   ) AND
        ( ZTINSSG3-ZFLACUNM EQ ZTINSSG3_OLD-ZFLACUNM ) AND
        ( ZTINSSG3-ZFTRCU   EQ ZTINSSG3_OLD-ZFTRCU   ) AND
        ( ZTINSSG3-ZFTRCUNM EQ ZTINSSG3_OLD-ZFTRCUNM ).
        CLEAR : ZTINS-ZFPRYN.
     ELSE.
        ZTINS-ZFPRYN = 'X'.
     ENDIF.
  ENDIF.

ENDMODULE.                 " SET_PORT_BIT_SCR4104  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_START_DT_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_START_DT_BIT_SCR4104 INPUT.
  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47'
                         OR SY-TCODE EQ 'ZIM48'.
     IF ZTINSSG3-ZFDPDT NE ZTINSSG3_OLD-ZFDPDT.
        ZTINS-ZFDTYN = 'X'.
     ELSE.
        CLEAR : ZTINS-ZFDTYN.
     ENDIF.
  ENDIF.
ENDMODULE.                 " SET_START_DT_BIT_SCR4104  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4505  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4505 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1' OR
     SY-UCOMM EQ 'MKA2' OR SY-UCOMM EQ 'DEL2'.
     W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1' OR
         SY-UCOMM EQ 'MKL2' OR SY-UCOMM EQ 'UND2'.  " 선택 해?
     CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'. " 삭제 / 취?
      LOOP AT IT_ZSINSAGR  WHERE ZFMARK NE SPACE.
        DELETE IT_ZSINSAGR  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSINSAGR.
        IT_ZSINSAGR-ZFMARK = W_MARK.   MODIFY IT_ZSINSAGR.
      ENDLOOP.
    WHEN 'REF1'.           " Refresh
      REFRESH : IT_ZSINSAGR.
      LOOP AT IT_ZSINSAGR_ORG.
         CLEAR : IT_ZSINSAGR.
         MOVE-CORRESPONDING   IT_ZSINSAGR_ORG   TO   IT_ZSINSAGR.
         APPEND IT_ZSINSAGR.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

* INDEX 수정 작?
  PERFORM   P2000_IT_ZSINSAGR_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR4505  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_PEIV_BIT_SCR4505  INPUT
*&---------------------------------------------------------------------*
MODULE SET_PEIV_BIT_SCR4505 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.

  IF ZTINS-ZFPEIV NE ZTINS_OLD-ZFPEIV.
     ZTINS-ZFPEYN = 'X'.
  ELSE.
     CLEAR :ZTINS-ZFPEYN.
  ENDIF.

ENDMODULE.                 " SET_PEIV_BIT_SCR4505  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_PEDU_BIT_SCR4505  INPUT
*&---------------------------------------------------------------------*
MODULE SET_PEDU_BIT_SCR4505 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.

  IF ZTINS-ZFPEDU NE ZTINS_OLD-ZFPEDU.
     ZTINS-ZFPDYN = 'X'.
  ELSE.
     CLEAR : ZTINS-ZFPDYN.
  ENDIF.

ENDMODULE.                 " SET_PEDU_BIT_SCR4505  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR4502  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR4502 INPUT.
   CHECK  W_STATUS NE C_REQ_D.

   IF ZTINS-ZFINSDT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINS' 'ZFINSDT'.
   ENDIF.
   IF ZTINS-ZFTRANS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINS' 'ZFTRANS'.
   ENDIF.
   IF ZTINS-ZFIVAMT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINS' 'ZFIVAMT'.
   ENDIF.

   IF ZTREQHD-ZFREQTY EQ 'LC'.
      SELECT SINGLE * FROM ZTMLCHD
                      WHERE ZFREQNO  EQ   ZTREQHD-ZFREQNO.
      IF ZTMLCHD-ZFALCQ EQ 'T'.
         W_AMOUNT  = ( ZTREQHD-ZFLASTAM * ZTMLCHD-ZFALCP ) / 100.
         W_AMOUNT1 = ZTREQHD-ZFLASTAM + W_AMOUNT.
         W_AMOUNT2 = ZTREQHD-ZFLASTAM - W_AMOUNT.
         IF NOT ( ZTINS-ZFIVAMT GE W_AMOUNT2 AND
                  ZTINS-ZFIVAMT LE W_AMOUNT1 ).
            PERFORM P2000_NO_INPUT(SAPMZIM01) USING  'ZTINS' 'ZFIVAMT'
                                              dfies-scrtext_m  W_SUBRC.
            WRITE: ZTINS-ZFIVAMT TO W_AMTTXT1 CURRENCY ZTINS-WAERS.
            W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
            WRITE: W_AMOUNT1 TO W_AMTTXT2 CURRENCY ZTINS-WAERS.
            W_AMTLEN2 = STRLEN( W_AMTTXT2 ).
            WRITE: W_AMOUNT2 TO W_AMTTXT3 CURRENCY ZTINS-WAERS.
            W_AMTLEN1 = STRLEN( W_AMTTXT3 ).

            MESSAGE W525 WITH
            W_AMTTXT1(W_AMTLEN1) W_AMTTXT3(W_AMTLEN3)
                               W_AMTTXT2(W_AMTLEN2).
*           MESSAGE W525 WITH ZTINS-ZFIVAMT W_AMOUNT2 W_AMOUNT1.

         ENDIF.
      ELSE.
         IF ZTREQHD-ZFLASTAM NE ZTINS-ZFIVAMT.
            PERFORM P2000_NO_INPUT(SAPMZIM01) USING  'ZTINS' 'ZFIVAMT'
                                              DFIES-SCRTEXT_M  W_SUBRC.
            WRITE: ZTINS-ZFIVAMT TO W_AMTTXT1 CURRENCY ZTINS-WAERS.
            W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
            WRITE: ZTREQHD-ZFLASTAM TO W_AMTTXT2
                                     CURRENCY ZTREQHD-WAERS.
            W_AMTLEN2 = STRLEN( W_AMTTXT2 ).
            MESSAGE W526 WITH  W_AMTTXT1(W_AMTLEN1)
                               W_AMTTXT2(W_AMTLEN2).

*            MESSAGE W526 WITH ZTINS-ZFIVAMT ZTREQHD-ZFLASTAM.

         ENDIF.
      ENDIF.
   ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR4502  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0105 INPUT.

*   CASE ZTMLCHD-ZFALCQ.
*      WHEN 'T'.       ">Plus/Minus (Percentage)
*      WHEN 'X'.       ">Maximum
*      WHEN '2AA'.     ">Up To
*      WHEN '2AB'.     ">Not Exceeding
*      WHEN OTHERS.
*   ENDCASE.

   IF ZTMLCHD-ZFALCQ EQ 'T'.
      IF ZTMLCHD-ZFALCP IS INITIAL AND
         ZTMLCHD-ZFALCM IS INITIAL.
         PERFORM NO_INPUT(SAPFMMEX) USING 'ZTMLCHD' 'ZFALCP'.
      ENDIF.
   ELSE.
      IF NOT ZTMLCHD-ZFALCP IS INITIAL OR
         NOT ZTMLCHD-ZFALCM IS INITIAL.
         PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZTMLCHD' 'ZFALCP'
                                           dfies-scrtext_m W_SUBRC.
         MESSAGE E493.
      ENDIF.
   ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0105  INPUT
