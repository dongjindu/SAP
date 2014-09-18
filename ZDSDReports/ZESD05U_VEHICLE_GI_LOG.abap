************************************************************************
* Program Name      : ZESD05U_VEHICLE_GI_LOG
* Author            : jun ho choi
* Creation Date     : 2004.03.19.
* Specifications By : jun ho choi
* Pattern           : 3-1
* Development Request No : UD1K908261
* Addl Documentation:
* Description       : Vehicle G/I Log
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZESD05U_VEHICLE_GI_LOG NO STANDARD PAGE HEADING
                              MESSAGE-ID ZMSD
                              LINE-SIZE 95.


*
TABLES : ZTSD_VEH_LOG.


*
DATA : BEGIN OF IT_LOG OCCURS 0.
        INCLUDE STRUCTURE ZTSD_VEH_LOG.
DATA : END OF IT_LOG.

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
       WWW(1) VALUE 'N'.
*
TABLES : CABN, AUSP, USR01.

DATA : BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.

DATA : BEGIN OF MESS_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESS_TAB.

DATA : W_VESL_N(20), ""
       W_VESL_D(20). ""


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_ZDATE FOR ZTSD_VEH_LOG-ZDATE,
                 S_ZKEY  FOR ZTSD_VEH_LOG-ZKEY.
SELECTION-SCREEN SKIP 1.
PARAMETERS : P_ALL RADIOBUTTON GROUP RADI,
             P_S   RADIOBUTTON GROUP RADI,
             P_E   RADIOBUTTON GROUP RADI.

SELECTION-SCREEN SKIP 1.
PARAMETERS : P_BATCH(1).

SELECTION-SCREEN END OF BLOCK B1.

*
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.

*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_OF_PAGE.

*
START-OF-SELECTION.
  SET PF-STATUS 'ESD05U'.
  PERFORM GET_DATA.
  PERFORM DISPLAY_RESULT.

  IF NOT P_BATCH IS INITIAL.
    PERFORM BATCH_PROCESSING.
  ENDIF.
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
            WHERE ZGUBUN EQ 'G'
            AND   ZDATE IN S_ZDATE
            AND   ZKEY  IN S_ZKEY.
    WHEN P_S.
      SELECT *
             INTO TABLE IT_LOG
             FROM ZTSD_VEH_LOG
            WHERE ZGUBUN EQ 'G'
            AND   ZDATE IN S_ZDATE
            AND   ZKEY  IN S_ZKEY
            AND   ZRESULT EQ 'S'.
    WHEN P_E.
      SELECT *
             INTO TABLE IT_LOG
             FROM ZTSD_VEH_LOG
            WHERE ZGUBUN EQ 'G'
            AND   ZDATE IN S_ZDATE
            AND   ZKEY  IN S_ZKEY
            AND   ZRESULT EQ 'E'.
  ENDCASE.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM DISPLAY_RESULT.
  DESCRIBE TABLE IT_LOG LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
    EXIT.
  ENDIF.

  WRITE:/ ''.
  LOOP AT IT_LOG.
    WRITE:/ SY-VLINE, (10) IT_LOG-ZDATE,
            SY-VLINE, (08) IT_LOG-ZTIME,
            SY-VLINE, (10) IT_LOG-ZKEY,
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

    WRITE:            (50) IT_LOG-ZMESSAGE,
            SY-VLINE.

    W_INDEX = SY-TABIX.
    HIDE : W_INDEX.

    WRITE:/(95) SY-ULINE.
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
    IF IT_LOG-ZRESULT = 'S'.
      W_CNT_S = W_CNT_S + 1.
    ENDIF.
    IF IT_LOG-ZRESULT = 'E'.
      W_CNT_E = W_CNT_E + 1.
    ENDIF.
  ENDLOOP.
  WRITE:/ 'Success records :', W_CNT_S.
  WRITE:/ 'Error   records :', W_CNT_E.

  FORMAT COLOR COL_HEADING.
  WRITE:/(95) SY-ULINE.
  WRITE:/ SY-VLINE, (10) '   Date   ',
          SY-VLINE, (08) '  Time  ',
          SY-VLINE, (10) '   Key    ',
          SY-VLINE, (01) 'G',
          SY-VLINE, (50) 'Message',
          SY-VLINE.
  WRITE:/(95) SY-ULINE.
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
      IF IT_LOG-ZRESULT EQ 'E'.
        PERFORM CALL_FUNC.
        PERFORM UPDATE_LOG.
        PERFORM GET_DATA.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE I000 WITH TEXT-M02.
  ENDIF.
ENDFORM.                    " RESTARTING
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNC
*&---------------------------------------------------------------------*
FORM CALL_FUNC.
**  CALL FUNCTION 'Z_FSD_VEHICLE_GOODISSUE'
**    EXPORTING
**      EQUNO         = IT_LOG-ZKEY
***   IMPORTING
***     RETURN1       =
***     RETURN2       =
**          .
  PERFORM BDC_VL02N_GI.
ENDFORM.                    " CALL_FUNC
*&---------------------------------------------------------------------*
*&      Form  BDC_VL02N_GI
*&---------------------------------------------------------------------*
FORM BDC_VL02N_GI.
  SELECT SINGLE *
         FROM CABN
        WHERE ATNAM = 'P_VESL_NO'.
  SELECT SINGLE *
         FROM AUSP
        WHERE OBJEK EQ IT_LOG-ZKEY
        AND   ATINN EQ CABN-ATINN.
  IF SY-SUBRC = 0.
    W_VESL_N = AUSP-ATWRT.
  ELSE.
    W_VESL_N = ''.
  ENDIF.

  SELECT SINGLE *
         FROM CABN
        WHERE ATNAM = 'P_VESL_DEST'.
  SELECT SINGLE *
         FROM AUSP
        WHERE OBJEK EQ IT_LOG-ZKEY
        AND   ATINN EQ CABN-ATINN.
  IF SY-SUBRC = 0.
    W_VESL_D = AUSP-ATWRT.
  ELSE.
    W_VESL_D = ''.
  ENDIF.

  SELECT SINGLE *
         FROM CABN
        WHERE ATNAM = 'P_RP25_SHOP_DATE'.
  SELECT SINGLE *
         FROM AUSP
        WHERE OBJEK EQ IT_LOG-ZKEY
        AND   ATINN EQ CABN-ATINN.
  W_N_8 = AUSP-ATFLV.
  IF W_N_8 EQ '00000000'.
    SELECT SINGLE *
           FROM CABN
          WHERE ATNAM = 'P_RP27_SHOP_DATE'.
    SELECT SINGLE *
           FROM AUSP
          WHERE OBJEK EQ IT_LOG-ZKEY
          AND   ATINN EQ CABN-ATINN.
    W_N_8 = AUSP-ATFLV.
  ENDIF.
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

  REFRESH : BDC_TAB, MESS_TAB.
  CLEAR   : BDC_TAB, MESS_TAB.

  PERFORM BDC_FILL USING :
          'X' 'SAPMV50A'             '4004',
          ' ' 'LIKP-VBELN'           IT_LOG-ZKEY,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV50A'             '1000',
          ' ' 'BDC_OKCODE'           '=HLOA_T', "LOADING
          'X' 'SAPMV50A'             '2000',
          ' ' 'LIKP-LDDAT'           W_C_8_GI,  "LOADING DATE.
          ' ' 'BDC_OKCODE'           '=HTRA_T', "SHIPMENT
          'X' 'SAPMV50A'             '2000',
          ' ' 'LIKP-ROUTE'           W_VESL_D,  "DEST.
          ' ' 'BDC_OKCODE'           '=HADM_T', "ADMINISTRATION
          'X' 'SAPMV50A'             '2000',
          ' ' 'LIKP-LIFEX'           W_VESL_N,  "RC#
          ' ' 'BDC_OKCODE'           '=SICH_T'. "SAVE ONLY

  CALL TRANSACTION 'VL02N' USING BDC_TAB MODE WWW
                                 UPDATE 'S'
                                 MESSAGES INTO MESS_TAB.
  READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC = 0.
    IT_LOG-ZRESULT = 'E'.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = MESS_TAB-MSGID
              MSGNR               = MESS_TAB-MSGNR
              MSGV1               = MESS_TAB-MSGV1
              MSGV2               = MESS_TAB-MSGV2
              MSGV3               = MESS_TAB-MSGV3
              MSGV4               = MESS_TAB-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = IT_LOG-ZMESSAGE.
    MODIFY IT_LOG INDEX W_INDEX.
    MESSAGE S000 WITH IT_LOG-ZMESSAGE.
    EXIT.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                                 MSGID  = 'VL'
                                 MSGNR  = '311'.
    IF SY-SUBRC = 0.
      IT_LOG-ZRESULT = 'S'.
      IT_LOG-ZMESSAGE = ''.
      MODIFY IT_LOG INDEX W_INDEX.
      MESSAGE S000 WITH 'Successfully re-processed'.
      EXIT.
    ELSE.
      READ TABLE MESS_TAB INDEX 1.
      IT_LOG-ZRESULT = 'E'.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = MESS_TAB-MSGID
                MSGNR               = MESS_TAB-MSGNR
                MSGV1               = MESS_TAB-MSGV1
                MSGV2               = MESS_TAB-MSGV2
                MSGV3               = MESS_TAB-MSGV3
                MSGV4               = MESS_TAB-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = IT_LOG-ZMESSAGE.
      MODIFY IT_LOG INDEX W_INDEX.
      MESSAGE S000 WITH IT_LOG-ZMESSAGE.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " BDC_VL02N_GI
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
*&      Form  update_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_LOG.
  UPDATE ZTSD_VEH_LOG SET ZRESULT = IT_LOG-ZRESULT
                          ZMESSAGE  = IT_LOG-ZMESSAGE
          WHERE ZDATE = IT_LOG-ZDATE
          AND   ZKEY  = IT_LOG-ZKEY.

ENDFORM.                    " update_log
*&---------------------------------------------------------------------*
*&      Form  batch_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BATCH_PROCESSING.
  DATA: L_ATINN_VESL_NO LIKE CABN-ATINN,
        L_ATINN_VESL_DEST LIKE CABN-ATINN,
        L_ATINN_RP25_SHOP_DATE LIKE CABN-ATINN,
        L_ATINN_RP27_SHOP_DATE LIKE CABN-ATINN.
  DATA: L_INDEX LIKE SY-TABIX,
        L_MESS LIKE IT_LOG-ZMESSAGE.

  SELECT SINGLE ATINN INTO L_ATINN_VESL_NO
        FROM CABN
       WHERE ATNAM = 'P_VESL_NO'.

  SELECT SINGLE ATINN INTO L_ATINN_VESL_DEST
        FROM CABN
       WHERE ATNAM = 'P_VESL_DEST'.

  SELECT SINGLE ATINN INTO L_ATINN_RP25_SHOP_DATE
        FROM CABN
       WHERE ATNAM = 'P_RP25_SHOP_DATE'.

  SELECT SINGLE ATINN INTO L_ATINN_RP27_SHOP_DATE
          FROM CABN
         WHERE ATNAM = 'P_RP27_SHOP_DATE'.

  SELECT SINGLE *
       FROM USR01
      WHERE BNAME = SY-UNAME.

  LOOP AT IT_LOG WHERE ZRESULT EQ 'E' .
    L_INDEX  = SY-TABIX.
    SELECT SINGLE *
         FROM AUSP
        WHERE OBJEK EQ IT_LOG-ZKEY
        AND   ATINN EQ L_ATINN_VESL_NO.
    IF SY-SUBRC = 0.
      W_VESL_N = AUSP-ATWRT.
    ELSE.
      W_VESL_N = ''.
    ENDIF.

    SELECT SINGLE *
           FROM AUSP
          WHERE OBJEK EQ IT_LOG-ZKEY
          AND   ATINN EQ L_ATINN_VESL_DEST.
    IF SY-SUBRC = 0.
      W_VESL_D = AUSP-ATWRT.
    ELSE.
      W_VESL_D = ''.
    ENDIF.

    SELECT SINGLE *
           FROM AUSP
          WHERE OBJEK EQ IT_LOG-ZKEY
          AND   ATINN EQ L_ATINN_RP25_SHOP_DATE.

    W_N_8 = AUSP-ATFLV.
    IF W_N_8 EQ '00000000'.

      SELECT SINGLE *
             FROM AUSP
            WHERE OBJEK EQ IT_LOG-ZKEY
            AND   ATINN EQ L_ATINN_RP27_SHOP_DATE.
      W_N_8 = AUSP-ATFLV.
    ENDIF.

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

    REFRESH : BDC_TAB, MESS_TAB.
    CLEAR   : BDC_TAB, MESS_TAB, L_MESS.

    PERFORM BDC_FILL USING :
            'X' 'SAPMV50A'             '4004',
            ' ' 'LIKP-VBELN'           IT_LOG-ZKEY,
            ' ' 'BDC_OKCODE'           '/00',
            'X' 'SAPMV50A'             '1000',
            ' ' 'BDC_OKCODE'           '=HLOA_T', "LOADING
            'X' 'SAPMV50A'             '2000',
            ' ' 'LIKP-LDDAT'           W_C_8_GI,  "LOADING DATE.
            ' ' 'BDC_OKCODE'           '=HTRA_T', "SHIPMENT
            'X' 'SAPMV50A'             '2000',
            ' ' 'LIKP-ROUTE'           W_VESL_D,  "DEST.
            ' ' 'BDC_OKCODE'           '=HADM_T', "ADMINISTRATION
            'X' 'SAPMV50A'             '2000',
            ' ' 'LIKP-LIFEX'           W_VESL_N,  "RC#
            ' ' 'BDC_OKCODE'           '=SICH_T'. "SAVE ONLY

    CALL TRANSACTION 'VL02N' USING BDC_TAB MODE WWW
                                   UPDATE 'S'
                                   MESSAGES INTO MESS_TAB.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      IT_LOG-ZRESULT = 'E'.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = MESS_TAB-MSGID
                MSGNR               = MESS_TAB-MSGNR
                MSGV1               = MESS_TAB-MSGV1
                MSGV2               = MESS_TAB-MSGV2
                MSGV3               = MESS_TAB-MSGV3
                MSGV4               = MESS_TAB-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = IT_LOG-ZMESSAGE.
      MODIFY IT_LOG INDEX L_INDEX.
      CONCATENATE IT_LOG-ZKEY ':' IT_LOG-ZMESSAGE INTO L_MESS
                                SEPARATED BY SPACE.
      MESSAGE S000 WITH L_MESS.

    ELSE.
      READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                                   MSGID  = 'VL'
                                   MSGNR  = '311'.
      IF SY-SUBRC = 0.
        IT_LOG-ZRESULT = 'S'.
        IT_LOG-ZMESSAGE = ''.
        MODIFY IT_LOG INDEX L_INDEX.
        CONCATENATE IT_LOG-ZKEY ': Successfully re-processed'
                           INTO L_MESS.
        MESSAGE S000 WITH L_MESS.

      ELSE.
        READ TABLE MESS_TAB INDEX 1.
        IT_LOG-ZRESULT = 'E'.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  MSGID               = MESS_TAB-MSGID
                  MSGNR               = MESS_TAB-MSGNR
                  MSGV1               = MESS_TAB-MSGV1
                  MSGV2               = MESS_TAB-MSGV2
                  MSGV3               = MESS_TAB-MSGV3
                  MSGV4               = MESS_TAB-MSGV4
             IMPORTING
                  MESSAGE_TEXT_OUTPUT = IT_LOG-ZMESSAGE.
        MODIFY IT_LOG INDEX L_INDEX.
        CONCATENATE IT_LOG-ZKEY ':' IT_LOG-ZMESSAGE INTO L_MESS
                                SEPARATED BY SPACE.
        MESSAGE S000 WITH L_MESS.

      ENDIF.
    ENDIF.

  ENDLOOP.
  MODIFY ZTSD_VEH_LOG FROM TABLE IT_LOG.
ENDFORM.                    " batch_processing
