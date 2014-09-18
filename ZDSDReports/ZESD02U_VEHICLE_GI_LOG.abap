************************************************************************
* Program Name      : ZESD02U_VEHICLE_GI_LOG
* Author            : jun ho choi
* Creation Date     : 2004.04.15.
* Specifications By : jun ho choi
* Pattern           : 3-1
* Development Request No : UD1K909527
* Addl Documentation:
* Description       : Vehicle G/I Log
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZESD02U_VEHICLE_GI_LOG NO STANDARD PAGE HEADING
                              MESSAGE-ID ZMSD
                              LINE-SIZE 107.


*
TABLES : ZTSD_VEH_LOG,
         CABN,
         AUSP,
         USR01.


CONSTANTS: C_CG_DATE TYPE D VALUE '20100731'.
*
DATA : BEGIN OF IT_LOG OCCURS 0.
        INCLUDE STRUCTURE ZTSD_VEH_LOG.
DATA : END OF IT_LOG.

DATA : BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.

DATA : BEGIN OF MESS_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESS_TAB.

DATA : W_CNT TYPE I,
       W_OBJEK LIKE AUSP-OBJEK,
       W_VBELN LIKE VBAK-VBELN,
       W_N_8(8) TYPE N,
       W_C_8(8),
       W_C_8_GI(8),
       W_DEALER(5),
       W_CNT_S TYPE I,
       W_CNT_E TYPE I,
       W_INDEX LIKE SY-TABIX,
       W_MESSAGE LIKE ZTSD_VEH_LOG-ZMESSAGE,
       WWW(1) VALUE 'N'.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_ZDATE FOR ZTSD_VEH_LOG-ZDATE,
                 S_ZKEY  FOR ZTSD_VEH_LOG-ZKEY.
SELECTION-SCREEN SKIP 1.
PARAMETERS : P_ALL RADIOBUTTON GROUP RADI,
             P_S   RADIOBUTTON GROUP RADI,
             P_E   RADIOBUTTON GROUP RADI.
SELECTION-SCREEN END OF BLOCK B1.


*
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.


*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_OF_PAGE.


*
START-OF-SELECTION.
  SET PF-STATUS 'ESD02U'.
  PERFORM GET_DATA.
  PERFORM DISPLAY_MESSAGE.
  PERFORM DISPLAY_RESULT.


*
END-OF-SELECTION.


*
AT USER-COMMAND.
  PERFORM USER_COMMAND.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  REFRESH : IT_LOG.
  CLEAR   : IT_LOG.

  CASE 'X'.
    WHEN P_ALL.
      SELECT *
             INTO TABLE IT_LOG
             FROM ZTSD_VEH_LOG
            WHERE ZGUBUN EQ 'B'
            AND   ZDATE IN S_ZDATE
            AND   ZKEY  IN S_ZKEY.
    WHEN P_S.
      SELECT *
             INTO TABLE IT_LOG
             FROM ZTSD_VEH_LOG
            WHERE ZGUBUN EQ 'B'
            AND   ZDATE IN S_ZDATE
            AND   ZKEY  IN S_ZKEY
            AND   ZRESULT2 EQ 'S'.
    WHEN P_E.
      SELECT *
             INTO TABLE IT_LOG
             FROM ZTSD_VEH_LOG
            WHERE ZGUBUN EQ 'B'
            AND   ZDATE IN S_ZDATE
            AND   ZKEY  IN S_ZKEY
            AND   ( ZRESULT2 EQ 'E' OR ZRESULT2 EQ '' ).
  ENDCASE.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM DISPLAY_RESULT.
  WRITE:/ ''.
  LOOP AT IT_LOG.
    WRITE:/ SY-VLINE, (10) IT_LOG-ZDATE,
            SY-VLINE, (08) IT_LOG-ZTIME,
            SY-VLINE, (18) IT_LOG-ZKEY,
            SY-VLINE.

    CASE IT_LOG-ZRESULT.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (01) IT_LOG-ZRESULT, SY-VLINE.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (01) IT_LOG-ZRESULT, SY-VLINE.
        FORMAT COLOR COL_NEGATIVE OFF.
    ENDCASE.

    CASE IT_LOG-ZRESULT2.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (01) IT_LOG-ZRESULT2, SY-VLINE.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (01) IT_LOG-ZRESULT2, SY-VLINE.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (01) '',              SY-VLINE.
    ENDCASE.

    WRITE:            (50) IT_LOG-ZMESSAGE,
            SY-VLINE.

    W_INDEX = SY-TABIX.
    HIDE : W_INDEX.

    WRITE:/(107) SY-ULINE.
  ENDLOOP.
ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  DESCRIBE TABLE IT_LOG LINES W_CNT.
  WRITE:/ 'Total   records :', W_CNT.

  W_CNT_S = 0. W_CNT_E = 0.
  LOOP AT IT_LOG.
    IF IT_LOG-ZRESULT2 = 'S'.
      W_CNT_S = W_CNT_S + 1.
    ENDIF.
    IF ( IT_LOG-ZRESULT2 = 'E' OR IT_LOG-ZRESULT2 = '' ).
      W_CNT_E = W_CNT_E + 1.
    ENDIF.
  ENDLOOP.
  WRITE:/ 'Success records :', W_CNT_S.
  WRITE:/ 'Error   records :', W_CNT_E.

  FORMAT COLOR COL_HEADING.
  WRITE:/(107) SY-ULINE.
  WRITE:/ SY-VLINE, (10) '   Date   ',
          SY-VLINE, (08) '  Time  ',
          SY-VLINE, (18) '       Key        ',
          SY-VLINE, (01) 'D',
          SY-VLINE, (01) 'B',
          SY-VLINE, (50) 'Message',
          SY-VLINE.
  WRITE:/(107) SY-ULINE.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND.
  DATA : OK_CODE(4).
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REST'.
      PERFORM RESTARTING.
      SY-LSIND = SY-LSIND - 1.
      PERFORM DISPLAY_RESULT.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  RESTARTING
*&---------------------------------------------------------------------*
FORM RESTARTING.
  IF SY-LISEL+26(1) EQ ' '.
    MESSAGE I000 WITH TEXT-M03.
    EXIT.
  ENDIF.

  IF SY-LISEL+15(1) NE ' '.
    READ TABLE IT_LOG INDEX W_INDEX.
    IF SY-SUBRC = 0.
      IF IT_LOG-ZRESULT2 EQ 'E'.
        PERFORM UPDATE_BFST.
      ELSEIF IT_LOG-ZRESULT2 EQ ''.
        PERFORM GI.
      ENDIF.
      PERFORM GET_DATA.
    ENDIF.
  ELSE.
    MESSAGE I000 WITH TEXT-M02.
  ENDIF.
ENDFORM.                    " RESTARTING
*&---------------------------------------------------------------------*
*&      Form  UPDATE_BFST
*&---------------------------------------------------------------------*
FORM UPDATE_BFST.
  UPDATE ZTPP_BFST SET : SD_DELI_FLG = 'Y'
                         SD_DELI_DAT = SY-DATUM
                         SD_DELI_TIM = SY-UZEIT
                   WHERE VIN_NUM = IT_LOG-ZKEY.
  IF SY-SUBRC = 0.
    UPDATE ZTSD_VEH_LOG SET : ZRESULT2 = 'S'
                              ZMESSAGE = ''
                   WHERE ZGUBUN = 'B'
                   AND   ZDATE = IT_LOG-ZDATE
                   AND   ZTIME = IT_LOG-ZTIME
                   AND   ZKEY = IT_LOG-ZKEY.
    IT_LOG-ZRESULT2 = 'S'.
    IT_LOG-ZMESSAGE = ''.
    MODIFY IT_LOG INDEX W_INDEX.
    MESSAGE S000 WITH TEXT-M04.
  ENDIF.
ENDFORM.                    " UPDATE_BFST
*&---------------------------------------------------------------------*
*&      Form  GI
*&---------------------------------------------------------------------*
FORM GI.
  REFRESH : BDC_TAB, MESS_TAB.
  CLEAR   : BDC_TAB, MESS_TAB.

  PERFORM GET_RP19_S_VM.
  PERFORM BDC_FILL USING :
          'X' 'SAPMV50A'             '4004',
          ' ' 'LIKP-VBELN'           IT_LOG-ZKEY,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV50A'             '1000',
          ' ' 'LIKP-WADAT_IST'       W_C_8_GI,
          ' ' 'BDC_OKCODE'           '=WABU_T'.

  CALL TRANSACTION 'VL02N' USING BDC_TAB MODE WWW
                                 UPDATE 'S'
                                 MESSAGES INTO MESS_TAB.
  READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = MESS_TAB-MSGID
              MSGNR               = MESS_TAB-MSGNR
              MSGV1               = MESS_TAB-MSGV1
              MSGV2               = MESS_TAB-MSGV2
              MSGV3               = MESS_TAB-MSGV3
              MSGV4               = MESS_TAB-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = W_MESSAGE.
    UPDATE ZTSD_VEH_LOG SET : ZRESULT = 'E'
                              ZMESSAGE = W_MESSAGE
                   WHERE ZGUBUN = 'B'
                   AND   ZDATE = IT_LOG-ZDATE
                   AND   ZTIME = IT_LOG-ZTIME
                   AND   ZKEY = IT_LOG-ZKEY.
    IT_LOG-ZRESULT = 'E'.
    IT_LOG-ZMESSAGE = W_MESSAGE.
    MODIFY IT_LOG INDEX W_INDEX.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                                 MSGID  = 'VL'
                                 MSGNR  = '311'.
    IF SY-SUBRC = 0.
      UPDATE ZTSD_VEH_LOG SET : ZRESULT = 'S'
                                ZMESSAGE = ''
                     WHERE ZGUBUN = 'B'
                     AND   ZDATE = IT_LOG-ZDATE
                     AND   ZTIME = IT_LOG-ZTIME
                     AND   ZKEY = IT_LOG-ZKEY.
      IT_LOG-ZRESULT = 'S'.
      IT_LOG-ZMESSAGE = ''.
      MODIFY IT_LOG INDEX W_INDEX.

      PERFORM UPDATE_BFST.
    ELSE.
      READ TABLE MESS_TAB INDEX 1.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = MESS_TAB-MSGID
                MSGNR               = MESS_TAB-MSGNR
                MSGV1               = MESS_TAB-MSGV1
                MSGV2               = MESS_TAB-MSGV2
                MSGV3               = MESS_TAB-MSGV3
                MSGV4               = MESS_TAB-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = W_MESSAGE.
      UPDATE ZTSD_VEH_LOG SET : ZRESULT = 'E'
                                ZMESSAGE = W_MESSAGE
                     WHERE ZGUBUN = 'B'
                     AND   ZDATE = IT_LOG-ZDATE
                     AND   ZTIME = IT_LOG-ZTIME
                     AND   ZKEY = IT_LOG-ZKEY.
      IT_LOG-ZRESULT = 'E'.
      IT_LOG-ZMESSAGE = W_MESSAGE.
      MODIFY IT_LOG INDEX W_INDEX.
    ENDIF.
  ENDIF.
ENDFORM.                    " GI
*&---------------------------------------------------------------------*
*&      Form  GET_RP19_S_VM
*&---------------------------------------------------------------------*
FORM GET_RP19_S_VM.

  DATA: L_DATE LIKE SY-DATUM,
        L_ATWRT LIKE AUSP-ATWRT.

  SELECT SINGLE *
         FROM CABN
        WHERE ATNAM = 'P_RP19_SHOP_DATE'.
  SELECT SINGLE *
         FROM AUSP
        WHERE OBJEK EQ IT_LOG-ZKEY
        AND   ATINN EQ CABN-ATINN.

** Change by Furong on 07/20/10

  L_DATE = W_N_8 = AUSP-ATFLV.
  IF L_DATE > C_CG_DATE.

** Changed by Furong on 06/10/11
    SELECT SINGLE ATWRT INTO L_ATWRT
       FROM AUSP AS A
       INNER JOIN CABN AS B
       ON A~ADZHL = B~ADZHL
         AND A~ATINN = B~ATINN
       WHERE OBJEK = IT_LOG-ZKEY
         AND KLART = '002'
       AND ATNAM = 'P_NATN_CODE'.

    IF L_ATWRT = 'B28'.
      CLEAR: CABN, AUSP.
      SELECT SINGLE *
              FROM CABN
             WHERE ATNAM = 'P_RP23_SHOP_DATE'.

      SELECT SINGLE *
             FROM AUSP
            WHERE OBJEK EQ IT_LOG-ZKEY
            AND   ATINN EQ CABN-ATINN.
    ELSE.
      CLEAR: CABN, AUSP.
      SELECT SINGLE *
              FROM CABN
             WHERE ATNAM = 'P_RP25_SHOP_DATE'.

      SELECT SINGLE *
             FROM AUSP
            WHERE OBJEK EQ IT_LOG-ZKEY
              AND   ATINN EQ CABN-ATINN.

      IF AUSP-ATFLV IS INITIAL.
        CLEAR: CABN, AUSP.
        SELECT SINGLE *
                FROM CABN
               WHERE ATNAM = 'P_RP27_SHOP_DATE'.

        SELECT SINGLE *
               FROM AUSP
              WHERE OBJEK EQ IT_LOG-ZKEY
              AND   ATINN EQ CABN-ATINN.
      ENDIF.

    ENDIF.
  ENDIF.

*     CLEAR: CABN, AUSP.
*    SELECT SINGLE *
*            FROM CABN
*           WHERE ATNAM = 'P_RP23_SHOP_DATE'.
*
*    SELECT SINGLE *
*           FROM AUSP
*          WHERE OBJEK EQ IT_LOG-ZKEY
*          AND   ATINN EQ CABN-ATINN.

** End on 06/10/11

** End of change

  W_N_8 = AUSP-ATFLV.
  IF W_N_8 NE '00000000'.
    SELECT SINGLE *
           FROM USR01
          WHERE BNAME = SY-UNAME.
    CASE USR01-DATFM.
      WHEN '1'. "DD.MM.YYYY
        W_C_8_GI+4(4) = W_N_8+0(4).
        W_C_8_GI+2(2) = W_N_8+4(2).
        W_C_8_GI+0(2) = W_N_8+6(2).
      WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
        W_C_8_GI+4(4) = W_N_8+0(4).
        W_C_8_GI+0(2) = W_N_8+4(2).
        W_C_8_GI+2(2) = W_N_8+6(2).
    ENDCASE.
  ELSE.
    SELECT SINGLE *
           FROM USR01
          WHERE BNAME = SY-UNAME.
    CASE USR01-DATFM.
      WHEN '1'. "DD.MM.YYYY
        W_C_8_GI+4(4) = SY-DATUM+0(4).
        W_C_8_GI+2(2) = SY-DATUM+4(2).
        W_C_8_GI+0(2) = SY-DATUM+6(2).
      WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
        W_C_8_GI+4(4) = SY-DATUM+0(4).
        W_C_8_GI+0(2) = SY-DATUM+4(2).
        W_C_8_GI+2(2) = SY-DATUM+6(2).
    ENDCASE.
  ENDIF.
ENDFORM.                    " GET_RP19_S_VM
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
FORM BDC_FILL USING    P1 P2 P3.
  CLEAR BDC_TAB.
  IF P1 = 'X'.
    BDC_TAB-DYNBEGIN = P1.
    BDC_TAB-PROGRAM  = P2.
    BDC_TAB-DYNPRO   = P3.
  ELSE.
    BDC_TAB-DYNBEGIN = P1.
    BDC_TAB-FNAM     = P2.
    BDC_TAB-FVAL     = P3.
  ENDIF.
  APPEND BDC_TAB.
ENDFORM.                    " BDC_FILL
*&---------------------------------------------------------------------*
*&      Form  display_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_MESSAGE.
  DESCRIBE TABLE IT_LOG LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
    EXIT.
  ENDIF.
ENDFORM.                    " display_message
