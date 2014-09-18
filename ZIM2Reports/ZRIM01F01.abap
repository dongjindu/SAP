*&---------------------------------------------------------------------*
*& INCLUDE ZRIM01F01                                                   *
*&---------------------------------------------------------------------*
*&  Program Name  :  B/L Sub MODULE Include                            *
*&  Created By    : INFOLINK Ltd.                                      *
*&  Created On    : 2000.02.12                                         *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&      Form  P2000_PRE_DOC_NO_INPUT
*&---------------------------------------------------------------------*
FORM P2000_PRE_DOC_NO_INPUT.

  SPOP-TITEL = 'Create B/L : Copy from...'.
  OPTION = 1.

  CALL SCREEN 0002 STARTING AT 27 6
                   ENDING   AT 80 10.

  IF ANTWORT EQ 'C' OR ANTWORT EQ 'N'.       " Cancel Or No
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_PRE_DOC_NO_INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SET_MESSAGE USING    P_SY_UCOMM.
  MOVE  OK-CODE  TO  W_OK_CODE.
  CASE P_SY_UCOMM.
    WHEN 'POST'.
      PERFORM  P2000_SAVE_MESSAGE.
    WHEN 'ANZG'.      " CHANGE  ==> DISPLAY
      PERFORM  P2000_ANZG_MESSAGE.
    WHEN 'IMPREQ'.
      PERFORM  P2000_IMPREQ_MESSAGE.
    WHEN 'GRPT'.                " ÀÔ°í.
      PERFORM  P2000_MIGO_MESSAGE.
    WHEN 'GRRV'.                " ÀÔ°íÃë¼Ò.
      PERFORM  P2000_MIGO_CANCEL_MESSAGE.
    WHEN 'MIRO' OR 'MIR1'.      " ¹°´ë INVOICE VERIFY
      PERFORM  P2000_MIRO_MESSAGE.
    WHEN 'MIR2'.
      PERFORM  P2000_MIR2_MESSAGE.
    WHEN 'PYMT'.
      PERFORM  P2000_PYMT_MESSAGE.
    WHEN 'PYCN'.
      PERFORM  P2000_PYCN_MESSAGE.
    WHEN 'TRTR'.
      PERFORM  P2000_TRTR_MESSAGE.
    WHEN 'TRTI'.
      PERFORM  P2000_TRTI_MESSAGE.
    WHEN 'TRCL'.      ">ÀÚ±Ý¹Ý¿µ Ãë¼Ò.
      PERFORM  P2000_TRCL_MESSAGE.
    WHEN 'TRCC'.
      PERFORM  P2000_TRCC_MESSAGE.
    WHEN 'SAVE'.      " ÀúÀå?
      PERFORM  P2000_SAVE_MESSAGE.
    WHEN 'CANC'.      " Ãë¼Ò?
      PERFORM  P2000_CANCEL_MESSAGE.
    WHEN 'BACK' OR 'EXIT'.   " ¾ÕÀ¸·Î or Á¾?
      PERFORM  P2000_EXIT_MESSAGE.
    WHEN 'DELE'.      " »èÁ¦?
      PERFORM  P2000_DELETE_MESSAGE.
    WHEN 'REVK'.      " flat DB Create Ãë¼Ò?
      PERFORM  P2000_DEL_CANCEL_MESSAGE.
    WHEN 'DELR'.      " Á¢¼ö Ãë¼Ò?
      PERFORM  P2000_REVOCK_MESSAGE.
    WHEN 'OPCL'.      " È®Á¤ Ãë¼Ò?
      PERFORM  P2000_OPEN_CANCEL_MESSAGE.
    WHEN 'EDIS'.      " EDI Send
      PERFORM  P2000_EDI_SEND_MESSAGE.
    WHEN 'SVCO'.      " Save and Confirm
      PERFORM  P2000_SAVE_CONFIRM_MESSAGE.
    WHEN 'SDSD'.
      PERFORM  P2000_DOC_SEND_MESSAGE.
    WHEN 'REAL'.
      PERFORM  P2000_DOC_REAL_MESSAGE.
    WHEN 'REOG'.      " ¶õ Àç±¸?
      PERFORM  P2000_REOG_MESSAGE.
    WHEN 'CUCL'.      " Åë°ü³»¿ë Àú?
      PERFORM  P2000_CUCL_MESSAGE.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " P2000_SET_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_ANZG_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_ANZG_MESSAGE.
  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING 'ÀÌµ¿ È®ÀÎ'  " Å¸ÀÌÆ²...
              'ÇöÀç ÀÔ·Â³»¿ªÀ» ÀúÀåÇÏÁö ¾Ê½À´Ï´Ù.'
              'ÀúÀå ÈÄ ÀÌµ¿ÇÏ½Ã°Ú½À´Ï±î?'                   " MSG2
              'Y'                  " Ãë¼Ò ¹öÆ° À¯/?
              '1'.                 " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Move confirm'
             'Do not save entered item.'
             'Do you want to move another screen after save?'
             'Y'
             '1'.
  ENDCASE.

ENDFORM.                    " P2000_ANZG_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SAVE_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING 'ÀúÀå È®ÀÎ' " Å¸ÀÌÆ²...
              'ÀÔ·ÂµÈ ³»¿ªÀ» ÀúÀåÇÕ´Ï´Ù.'
              'ÀúÀåÇÏ½Ã°Ú½À´Ï±î?'  " Message #2
              'Y'                  " Ãë¼Ò ¹öÆ° À¯/?
              '1'.                 " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Save confirm '
             'Being saved entered item.'
             'Do you want to save?'
             'Y'
             '1'.
  ENDCASE.
ENDFORM.                    " P2000_SAVE_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CANCEL_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING 'Ãë¼Ò È®ÀÎ'             " Å¸ÀÌÆ²...
                                   'º¯°æµÈ ³»¿ëÀ» ÀúÀå¾øÀÌ Á¾·áµË´Ï´Ù.'
                                        'Á¾·áÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                                    'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '2'.                 " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Cancel confirm'
             'Being ended without saving the changed item.'
             'Do you want to end?'
             'N'
             '2'.
  ENDCASE.


  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      MESSAGE  S957.
      IF SY-DYNNR EQ '9912'.
        PERFORM P2000_SET_LOCK_ZTMSHD USING 'U'.
        SET SCREEN '9911'. LEAVE TO SCREEN '9911'.
      ENDIF.
      LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
    WHEN OTHERS.
  ENDCASE.


ENDFORM.                    " P2000_CANCEL_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_EXIT_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING 'Á¾·á È®ÀÎ'             " Å¸ÀÌÆ²...
                              'ÇöÀç ÀÔ·Â³»¿ªÀ» ÀúÀåÇÏÁö ¾Ê½À´Ï´Ù.'" MSG1
                                    'ÀúÀå ÈÄ Á¾·áÇÏ½Ã°Ú½À´Ï±î?'" MSG2
                               'Y'                         " Ãë¼Ò ¹öÆ° ?
                            '1'.                        " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'End confirm'
             'Do not save the entered item.'
             'Do you want to end after save?'
             'Y'
             '1'.
  ENDCASE.
ENDFORM.                    " P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_BOX
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_BOX USING    TITLE  LIKE SPOP-TITEL
                                TEXT1  LIKE SPOP-TEXTLINE1
                                TEXT2  LIKE SPOP-TEXTLINE2
                                CANCEL LIKE CANCEL_OPTION
                                DEFAULT LIKE OPTION.

  SPOP-TITEL = TITLE.
  SPOP-TEXTLINE1 = TEXT1.
  SPOP-TEXTLINE2 = TEXT2.
  IF CANCEL EQ 'Y'.
    CANCEL_OPTION = 'Y'.
  ELSE.
    CLEAR : CANCEL_OPTION.
  ENDIF.
  OPTION = DEFAULT.
  TEXTLEN = 60.

  CASE SY-UCOMM.
    WHEN 'GRPT'.             " ÀÔ°í.
      IF ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' OR
         ZTIMIMG00-GRPARTX NE 'X'.
        CALL SCREEN 0020 STARTING AT 12 3
                         ENDING   AT 82 12.
      ELSE.
        CALL SCREEN 0021 STARTING AT 1  2
                         ENDING   AT 90 19.
      ENDIF.
    WHEN 'GRRV'.             " ÀÔ°íÃë¼Ò.
      IF W_COUNT EQ 1.
        CALL SCREEN 0030 STARTING AT 15 3
                         ENDING   AT 56 09.
      ELSE.
        CALL SCREEN 0031 STARTING AT 1  3
                         ENDING   AT 65 15.
      ENDIF.
    WHEN 'MIRO' OR 'MIR1'.   " ¹°´ë»ý¼º.
      SELECT SINGLE * FROM ZTIMIMG11
              WHERE    BUKRS   EQ   ZTCIVHD-BUKRS.
      IF SY-SUBRC NE 0.
        MESSAGE E987 WITH ZTCIVHD-BUKRS.
      ENDIF.
      ZTCIVHD-BLART = 'RE'.
      ZSCIVHD-ZFEXRT = ZTCIVHD-ZFEXRT.

      IF SY-SUBRC EQ 0.
        IF ZTIMIMG00-ZFIVTY EQ 'L'.
          CALL SCREEN 3515 STARTING AT 12 3
                           ENDING   AT 86 18.
        ELSE.
          CALL SCREEN 3515 STARTING AT 12 3
                           ENDING   AT 86 20.
        ENDIF.
      ELSE.
        RADIO_NONE = 'X'.
        CALL SCREEN 3515 STARTING AT 12 3
                         ENDING   AT 86 18.
      ENDIF.
      IF RADIO_NONE = 'X'.
        DISP_MODE = 'N'.
      ENDIF.
      IF RADIO_ALL = 'X'.
        DISP_MODE = 'A'.
      ENDIF.
      IF RADIO_ERROR = 'X'.
        DISP_MODE = 'E'.
      ENDIF.
    WHEN 'TRTR'.
      IF ZTPMTHD-BLART IS INITIAL.
        ZTPMTHD-BLART = 'RE'.
      ENDIF.
      IF ZTPMTHD-BUDAT IS INITIAL.
        ZTPMTHD-BUDAT = SY-DATUM.
      ENDIF.
      IF ZTPMTHD-BLDAT IS INITIAL.
        ZTPMTHD-BLDAT = SY-DATUM.
      ENDIF.
      CALL SCREEN 8211 STARTING AT 12 3
                       ENDING   AT 86 18.
    WHEN 'TRCC'.
      IF BSIS-BUDAT IS INITIAL.
        BSIS-BUDAT = SY-DATUM.
      ENDIF.

      CALL SCREEN 0022 STARTING AT 30 2
                       ENDING   AT 78 11.

    WHEN 'MIR2'.
      CALL SCREEN 3516 STARTING AT 34 3
                       ENDING   AT 72 14.
    WHEN OTHERS.
      CALL SCREEN 0001 STARTING AT 30 6
                       ENDING   AT 78 10.
  ENDCASE.

  IF ANTWORT = 'C'.       " Cancel
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ACTION_TITLE
*&---------------------------------------------------------------------*
FORM P2000_SET_ACTION_TITLE.

  CASE W_STATUS.
    WHEN C_REQ_C.   ASSIGN  W_CREATE       TO <FS_F>.       " Create
    WHEN C_REQ_U.   ASSIGN  W_CHANGE       TO <FS_F>.       " Change
    WHEN C_REQ_D.   ASSIGN  W_DISPLAY      TO <FS_F>.       " Display
    WHEN C_ADD_U.   ASSIGN  W_ADD_CON      TO <FS_F>.       " Container.
    WHEN C_OPEN_C.  ASSIGN  W_OPEN         TO <FS_F>.       " Settlement
    WHEN C_OPEN_U.  ASSIGN  W_STAUTS       TO <FS_F>.       " Status Chg
    WHEN C_BL_SEND. ASSIGN  W_SEND         TO <FS_F>.       " Send
    WHEN C_BL_REAL. ASSIGN  W_REAL         TO <FS_F>.       " Acctual
    WHEN C_BL_COST. ASSIGN  W_COST         TO <FS_F>.       " Cost
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_ACTION_TITLE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_DOC_TITLE
*&---------------------------------------------------------------------*
FORM P2000_SET_DOC_TITLE.

  CASE SY-DYNNR.
    WHEN '0101'.
      ASSIGN  C_BL              TO <FS_DOC>.
    WHEN '2601'.
      ASSIGN  C_LG              TO <FS_DOC>.
    WHEN '3510' OR '0141'.
      CASE ZTCIVHD-ZFREQTY.
        WHEN 'LO'.
          ASSIGN  C_LOV             TO <FS_DOC>.
        WHEN 'PU'.
          ASSIGN  C_PUV             TO <FS_DOC>.
        WHEN OTHERS.
          ASSIGN  C_CIV             TO <FS_DOC>.
      ENDCASE.
    WHEN '0811'.
      ASSIGN  C_CG              TO <FS_DOC>.
    WHEN '3110'.
      CASE ZTIV-ZFREQTY.
        WHEN 'LO'.
          ASSIGN  C_LO              TO <FS_DOC>.
        WHEN 'PU'.
          ASSIGN  C_PU              TO <FS_DOC>.
        WHEN OTHERS.
          ASSIGN  C_CC              TO <FS_DOC>.
      ENDCASE.
  ENDCASE.

  IF SY-TCODE(4) EQ 'ZIMB'.
    ASSIGN  'Insurance on Cargo'     TO <FS_DOC>.
  ENDIF.
ENDFORM.                    " P2000_SET_DOC_TITLE
*&---------------------------------------------------------------------*
*&      Form  P2000_STATUS_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_STATUS_DISPLAY.

  SPOP-TITEL = 'Display: Document Status'.
  CANCEL_OPTION = 'Y'.
*     CLEAR : CANCEL_OPTION.
  OPTION = 1.
  TEXTLEN = 60.
  CASE SY-TCODE.
    WHEN 'ZIM21' OR 'ZIM22' OR 'ZIM23'.                " B/L
      CALL SCREEN 0009 STARTING AT 15 3
                       ENDING   AT 93 14.
    WHEN 'ZIM26' OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'.     " L/G
      CALL SCREEN 0008 STARTING AT 15 3
                       ENDING   AT 93 14.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_STATUS_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_EXIT_PROCESS.

  IF NOT W_STATUS EQ C_REQ_D.
*> º¯°æ¿©ºÎ Ã¼Å©.
    PERFORM P2000_MODIFY_CHECK USING W_MODIF_BIT.
    IF W_MODIF_BIT EQ 'Y'.
      PERFORM P2000_SET_MESSAGE USING  OK-CODE.
    ELSE.
      ANTWORT = 'Y'.
    ENDIF.

    IF W_MODIF_BIT NE 'Y'.
      CLEAR OK-CODE.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
      EXIT.
    ENDIF.

    CASE ANTWORT.
      WHEN 'Y'.              " Yes...
*-----------------------------------------------------------------------
* DB Write
*-----------------------------------------------------------------------
        PERFORM  P3000_DB_MODIFY_SCRCOM.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_UNLOCK.
        PERFORM  P2000_SET_SCREEN_SCRCOM.
        LEAVE SCREEN.
      WHEN 'N'.              " No...
        MESSAGE  S957.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_UNLOCK.
        PERFORM  P2000_SET_SCREEN_SCRCOM.
        LEAVE SCREEN.
      WHEN 'C'.              " Cancel
      WHEN OTHERS.
    ENDCASE.

  ELSE.
    CLEAR OK-CODE.
    PERFORM  P2000_SET_SCREEN_SCRCOM.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.                    " P2000_EXIT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------*
FORM P2000_SET_SCREEN_SCRCOM.
*> B/L ¿îÀÓ »ç¿ë º¯¼ö.(ÇÑ¼ö¿ø 20021016JSY)
  CLEAR : W_FIRST_CHK.

* CALL TransactionÀÏ °æ¿ì, ÇÁ·Î±×·¥ Leave
  IF SY-CALLD EQ 'X'.
    LEAVE.
  ENDIF.

  CASE W_STATUS.
    WHEN C_REQ_C.     " »ý¼º.
      CASE SY-TCODE.
        WHEN 'ZIMB1'.     SET SCREEN 4100.  " ºÎº¸(BL)
        WHEN 'ZIM21'.     SET SCREEN 0100.  " B/L
        WHEN 'ZIM26'.     SET SCREEN 2600.  " L/G
        WHEN 'ZIM31'.     SET SCREEN 3100.  " Åë°ü¿äÃ».
        WHEN 'ZIM31L'.    SET SCREEN 3101.  " ÀÔ°í¿äÃ»(LOCAL).
        WHEN 'ZIM35'.     SET SCREEN 3500.  " COMMERCIAL INVOICE
        WHEN 'ZIM35L'.    SET SCREEN 3501.  " LOCAL ¹°´ë.
        WHEN 'ZIM81'.     SET SCREEN 0810.  " ÇÏ¿ª°ü¸®.
        WHEN 'ZIMI6'.     SET SCREEN 9214.  " ¹ÝÀÔ½Å°í»ý¼º.
        WHEN 'ZIMI1'.     SET SCREEN 9200.  " ¹ÝÀÔ¿¹Á¤Á¤º¸.
        WHEN 'ZIMO1'.     SET SCREEN 9221.  " ¹ÝÃâ½Å°í.
        WHEN 'ZIMO6'.     SET SCREEN 9226.  " ¹ÝÃâÃ³¸®.
        WHEN 'ZIMMS1'.
          IF SY-DYNNR = '9911'.
            SET SCREEN 9910.            " ¸ð¼±°ü¸® ÃÊ±â.
          ELSE.
            PERFORM P2000_SET_LOCK_ZTMSHD USING 'U'.
            PERFORM P1000_READ_MSHD.
            PERFORM P1000_SORT_MSHD.
            SET SCREEN 9911.
          ENDIF.
        WHEN OTHERS.      SET SCREEN 100.
      ENDCASE.
    WHEN C_REQ_U.     " º¯°æ.
      CASE SY-TCODE.
        WHEN 'ZIMB2'.     SET SCREEN 4200.  " ºÎº¸(BL).
        WHEN 'ZIM22'.     SET SCREEN 0200.  " B/L
        WHEN 'ZIM27'.     SET SCREEN 2700.  " L/G
        WHEN 'ZIM32'.     SET SCREEN 3200.  " Åë°ü¿äÃ».
        WHEN 'ZIM32L'.    SET SCREEN 3201.  " ÀÔ°í¿äÃ»(LOCAL).
        WHEN 'ZIM36'.     SET SCREEN 3600.  " »ó¾÷¼ÛÀå.
        WHEN 'ZIM36L'.    SET SCREEN 3601.  " LOCAL ¹°´ë.
        WHEN 'ZIM82'.     SET SCREEN 0820.  " ÇÏ¿ª°ü¸®.
        WHEN 'ZIMI2'.     SET SCREEN 9202.  " ¹ÝÀÔ¿¹Á¤Á¤º¸.
        WHEN 'ZIMI7'.     SET SCREEN 9211.  " ¹ÝÀÔ½Å°íº¯°æ.
        WHEN 'ZIMI8'.     SET SCREEN 9212.  " ¹ÝÀÔ½Å°íÁ¶È¸.
        WHEN 'ZIMO2'.     SET SCREEN 9222.  " ¹ÝÃâ½Å°í.
        WHEN 'ZIMA3'.     SET SCREEN 8500.  " ¼¼±Ý°è»ê¼­.
        WHEN 'ZIMA6'.     SET SCREEN 8700.  " ÀÎ¼öÁõ.
        WHEN 'ZIMMS1'.
          IF SY-DYNNR = '9911'.
            SET SCREEN 9910.            " ¸ð¼±°ü¸® ÃÊ±â.
          ELSE.
            PERFORM P2000_SET_LOCK_ZTMSHD USING 'U'.
            PERFORM P1000_READ_MSHD.
            PERFORM P1000_SORT_MSHD.
            SET SCREEN 9911.
          ENDIF.
        WHEN OTHERS.      SET SCREEN 200.
      ENDCASE.
    WHEN C_REQ_D.      " Á¶È¸.
      CASE SY-TCODE.
        WHEN 'ZIMB3'.     SET SCREEN 4300.  " ºÎº¸(BL)
        WHEN 'ZIM23'.     SET SCREEN 0300.  " B/L
        WHEN 'ZIM28'.     SET SCREEN 2800.  " L/G
        WHEN 'ZIM33'.     SET SCREEN 3300.  " »ó¾÷¼ÛÀå.
        WHEN 'ZIM33L'.    SET SCREEN 3301.  " ÀÔ°í¿äÃ»(LOCAL).
        WHEN 'ZIM37'.     SET SCREEN 3700.  " COMMARCIAL INVOICE
        WHEN 'ZIM37L'.    SET SCREEN 3701.  " LOCAL ¹°´ë.
        WHEN 'ZIM83'.     SET SCREEN 0830.  " ÇÏ¿ª°ü¸®.
        WHEN 'ZIMI3'.     SET SCREEN 9203.  " ¹ÝÀÔ¿¹Á¤Á¤º¸.
        WHEN 'ZIMI8'.     SET SCREEN 9212.  " ¹ÝÀÔ½Å°í.
        WHEN 'ZIMO3'.     SET SCREEN 9223.  " ¹ÝÃâ½Å°í.
        WHEN 'ZIMO6'.     SET SCREEN 9226.  " ¹ÝÃâÃ³¸®.
        WHEN 'ZIMA4'.     SET SCREEN 8600.  " ¼¼±Ý°è»ê¼­.
        WHEN 'ZIMA7'.     SET SCREEN 8800.  " ÀÎ¼öÁõ.
        WHEN 'ZIMMS1'.
          IF SY-DYNNR = '9911'.
            SET SCREEN 9910.            " ¸ð¼±°ü¸® ÃÊ±â.
          ELSE.
            PERFORM P1000_READ_MSHD.
            PERFORM P1000_SORT_MSHD.
            SET SCREEN 9911.
          ENDIF.
        WHEN OTHERS.      SET SCREEN 300.
      ENDCASE.
    WHEN C_ADD_U.
    WHEN C_ADD_D.
    WHEN C_OPEN_C.
      CASE SY-TCODE.
        WHEN 'ZIMB4'.     SET SCREEN 4400.  " ºÎº¸(BL)
        WHEN 'ZIM29'.     SET SCREEN 2900.  " L/G
        WHEN 'ZIMI9'.     SET SCREEN 9213.  " ¹ÝÀÔ½Å°í.
        WHEN 'ZIMO4'.     SET SCREEN 9224.  " ¹ÝÃâ½Å°í.
        WHEN 'ZIMO6'.     SET SCREEN 9226.  " ¹ÝÃâÃ³¸®.
        WHEN OTHERS.      SET SCREEN 0.
      ENDCASE.
    WHEN C_OPEN_U.    " »óÅÂº¯?
      CASE SY-TCODE.
        WHEN 'ZIM34'.     SET SCREEN 3400.  " Åë°ü¿äÃ».
*        WHEN 'ZIM34L'.    SET SCREEN 3401.  " ÀÔ°í¿äÃ»(LOCAL).
        WHEN 'ZIM38'.     SET SCREEN 3800.  " COMMERCIAL INVOICE.
*        WHEN 'ZIM38L'.    SET SCREEN 3801.  " LOCAL ¹°´ë.
        WHEN 'ZIMI9'.     SET SCREEN 9213.  " ¹ÝÀÔ½Å°í.
        WHEN 'ZIMO4'.     SET SCREEN 9224.  " ¹ÝÃâ½Å°í.
        WHEN OTHERS.
      ENDCASE.
    WHEN C_OPEN_D.
    WHEN C_INSU_I.

    WHEN C_BL_SEND OR C_BL_REAL OR C_BL_COST.
      W_FIRST_SCR0200 = 'Y'.
      SET SCREEN 0200.  " Åë°ü¿äÃ».

    WHEN OTHERS.
      SET SCREEN 0.
  ENDCASE.

* Invoice Processing
  CASE SY-TCODE.
    WHEN 'ZIM57'.     SET SCREEN 5700.
    WHEN 'ZIM58'.     SET SCREEN 5800.
    WHEN 'ZIM34'.     SET SCREEN 3400.
  ENDCASE.

* ¼öÀÔ½Å°í ¶Ç´Â ¼öÀÔ¸éÇã.
  CASE SY-TCODE.
    WHEN 'ZIM62'.     SET SCREEN 6200.
    WHEN 'ZIM63'.     SET SCREEN 6300.
  ENDCASE.

* °¨¸éÇã°¡Ç°¸ñ.
  CASE SY-TCODE.
    WHEN 'ZIM64'.     SET SCREEN 6400.
    WHEN 'ZIM65'.     SET SCREEN 6500.
    WHEN 'ZIM77'.     SET SCREEN 6450.
  ENDCASE.

* °¨¸éÇã°¡Ç°¸ñ »ç¿ë½Ç?
  CASE SY-TCODE.
    WHEN 'ZIM66'.     SET SCREEN 6600.
  ENDCASE.

* °ú¼¼Åë°ü  Invoice
  CASE SY-TCODE.
    WHEN 'ZIM67'.     SET SCREEN 6700.
    WHEN 'ZIM68'.     SET SCREEN 6800.
    WHEN 'ZIM69'.     SET SCREEN 6900.
  ENDCASE.

* ±ä±Þº¸¼¼¿î¼ÛÀÇ?
  CASE SY-TCODE.
    WHEN 'ZIM71'.     SET SCREEN 7100.
    WHEN 'ZIM72'.     SET SCREEN 7200.
    WHEN 'ZIM73'.     SET SCREEN 7300.
  ENDCASE.

* ¼öÀÔ¸é?
  CASE SY-TCODE.
    WHEN 'ZIM74'.     SET SCREEN 7400.
    WHEN 'ZIM75'.     SET SCREEN 7500.
    WHEN 'ZIM76'.     SET SCREEN 7600.
  ENDCASE.

* ¼¼±Ý°è»ê¼­¿ë Invoice Á¶?
  CASE SY-TCODE.
    WHEN 'ZIMA2'.     SET SCREEN 8400.
  ENDCASE.

* Payment Notice
  CASE SY-TCODE.
    WHEN 'ZIMP2'.     SET SCREEN 8100.
    WHEN 'ZIMP3'.     SET SCREEN 8110.
    WHEN 'ZIMP4'.     SET SCREEN 8120.
  ENDCASE.

* ÃÊ±âÈ­¸éÀ¸·Î exit½Ã Active TabÀ» Clearing?
  CLEAR :  TABSTRIP-ACTIVETAB.

ENDFORM.                    " P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------*
*&      Form  P2000_OK_CODE_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_OK_CODE_PROCESS.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN ''.
    WHEN 'ENTR'.
    WHEN 'COPY' OR 'REBL' OR 'LOPU'.     " COPY
    WHEN 'CRDC'.          ">>>> »ý¼º.
      CASE SY-TCODE.
        WHEN 'ZIMB1' OR 'ZIMB2' OR 'ZIMB3' OR 'ZIMB4'.    " ºÎº¸.
          LEAVE TO TRANSACTION 'ZIMB1'.
        WHEN 'ZIM21' OR 'ZIM22' OR 'ZIM23' OR             " B/L
             'ZIM221' OR 'ZIM222' OR 'ZIM223'.
          LEAVE TO TRANSACTION 'ZIM21'.
        WHEN 'ZIM31' OR 'ZIM32' OR 'ZIM33' OR 'ZIM34'.     " Åë°ü¿äÃ».
          LEAVE TO TRANSACTION 'ZIM31'.
        WHEN 'ZIM31L' OR 'ZIM32L' OR 'ZIM33L' OR 'ZIM34L'. "ÀÔ°í.
          LEAVE TO TRANSACTION 'ZIM31L'.
        WHEN 'ZIM35' OR 'ZIM36' OR 'ZIM37' OR 'ZIM38'.     " Commercial.
          LEAVE TO TRANSACTION 'ZIM35'.
        WHEN 'ZIM35L' OR 'ZIM36L' OR 'ZIM37L' OR 'ZIM38L'. "LO ¹°´ë.
          LEAVE TO TRANSACTION 'ZIM35L'.
        WHEN 'ZIM81' OR 'ZIM82' OR 'ZIM83'.                " ÇÏ¿ª°ü¸®.
          LEAVE TO TRANSACTION 'ZIM81'.
        WHEN 'ZIM26' OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'.     " L/G
          LEAVE TO TRANSACTION 'ZIM26'.
        WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.                " ¹ÝÀÔ¿¹Á¤Á¤?
          LEAVE TO TRANSACTION 'ZIMI1'.
        WHEN 'ZIMI6' OR 'ZIMI7' OR 'ZIMI8'.  "LGÈ­ÇÐ ¹ÝÀÔ¿¹Á¤Á¤?
          LEAVE TO TRANSACTION 'ZIMI6'.
        WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4'. " ¹ÝÃâÁ¤?
          LEAVE TO TRANSACTION 'ZIMO1'.
        WHEN 'ZIMP2' OR 'ZIMP3' OR 'ZIMP4'.            " Payment No.
          LEAVE TO TRANSACTION 'ZIMP2'.
        WHEN 'ZIM74' OR 'ZIM75' OR 'ZIM76'.            " Payment No.
          LEAVE TO TRANSACTION 'ZIM74'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'CHDC'.         " >>>> º¯°æ.
      CASE SY-TCODE.
        WHEN 'ZIMB1' OR 'ZIMB2' OR 'ZIMB3' OR  'ZIMB4'.
          LEAVE TO TRANSACTION 'ZIMB2'.
        WHEN 'ZIM21' OR 'ZIM22' OR 'ZIM23' OR     " B/L
             'ZIM221' OR 'ZIM222' OR 'ZIM223'.
          LEAVE TO TRANSACTION 'ZIM22'.
        WHEN 'ZIM31' OR 'ZIM32' OR 'ZIM33' OR 'ZIM34'. " Åë°ü¿äÃ».
          LEAVE TO TRANSACTION 'ZIM32'.
        WHEN 'ZIM31L' OR 'ZIM32L' OR 'ZIM33L' OR 'ZIM34L'. "ÀÔ°í.
          LEAVE TO TRANSACTION 'ZIM32L'.
        WHEN 'ZIM35' OR 'ZIM36' OR 'ZIM37' OR 'ZIM38'.  " Commercial.
          LEAVE TO TRANSACTION 'ZIM36'.
        WHEN 'ZIM35L' OR 'ZIM36L' OR 'ZIM37L' OR 'ZIM38L'. "LO ¹°´ë.
          LEAVE TO TRANSACTION 'ZIM36L'.
        WHEN 'ZIM26' OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'.  " L/G
          LEAVE TO TRANSACTION 'ZIM27'.
        WHEN 'ZIM81' OR 'ZIM82' OR 'ZIM83'.            " ÇÏ¿ª°ü¸®.
          LEAVE TO TRANSACTION 'ZIM82'.
        WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.    " ¹ÝÀÔ¿¹Á¤Á¤?
          LEAVE TO TRANSACTION 'ZIMI2'.
        WHEN 'ZIMI6' OR 'ZIMI7' OR 'ZIMI8'.    " LG CHEM ¹ÝÀÔ¿¹Á¤.
          LEAVE TO TRANSACTION 'ZIMI7'.
*        WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9'.    " ¹ÝÀÔ½Å?
*          LEAVE TO TRANSACTION 'ZIMI7'.
        WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4'. " ¹ÝÃâÁ¤?
          LEAVE TO TRANSACTION 'ZIMO2'.
        WHEN 'ZIMP2' OR 'ZIMP3' OR 'ZIMP4'.            " Payment No.
          LEAVE TO TRANSACTION 'ZIMP3'.
        WHEN 'ZIM62' OR 'ZIM63'.
          LEAVE TO TRANSACTION 'ZIM62'.
        WHEN 'ZIM74' OR 'ZIM75' OR 'ZIM76'.            " Payment No.
          LEAVE TO TRANSACTION 'ZIM75'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'DISP'.          " >>>>>Á¶È¸.
      CASE SY-TCODE.
        WHEN 'ZIMB1' OR 'ZIMB2' OR 'ZIMB3' OR 'ZIMB4'.
          LEAVE TO TRANSACTION 'ZIMB3'.
        WHEN 'ZIM21' OR 'ZIM22' OR 'ZIM23' OR          " B/L
             'ZIM221' OR 'ZIM222' OR 'ZIM223'.
          LEAVE TO TRANSACTION 'ZIM23'.
        WHEN 'ZIM31' OR 'ZIM32' OR 'ZIM33' OR 'ZIM34'. " Åë°ü¿äÃ».
          LEAVE TO TRANSACTION 'ZIM33'.
        WHEN 'ZIM31L' OR 'ZIM32L' OR 'ZIM33L' OR 'ZIM34L'. "ÀÔ°í.
          LEAVE TO TRANSACTION 'ZIM33L'.
        WHEN 'ZIM35' OR 'ZIM36' OR 'ZIM37' OR 'ZIM38'. " Commercial.
          LEAVE TO TRANSACTION 'ZIM37'.
        WHEN 'ZIM35L' OR 'ZIM36L' OR 'ZIM37L' OR 'ZIM38L'. "LO ¹°´ë.
          LEAVE TO TRANSACTION 'ZIM37L'.
        WHEN 'ZIM26' OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'.  " L/G
          LEAVE TO TRANSACTION 'ZIM28'.
        WHEN 'ZIM81' OR 'ZIM82' OR 'ZIM83'.            " ÇÏ¿ª°ü¸®.
          LEAVE TO TRANSACTION 'ZIM83'.
        WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.    " ¹ÝÀÔ¿¹Á¤Á¤?
          LEAVE TO TRANSACTION 'ZIMI3'.
        WHEN 'ZIMI6' OR 'ZIMI7' OR 'ZIMI8'.    "LG ¹ÝÀÔ½Å°í.
          LEAVE TO TRANSACTION 'ZIMI8'.
*       WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9'.    " ¹ÝÀÔ½Å?
*          LEAVE TO TRANSACTION 'ZIMI8'.
        WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4'. " ¹ÝÃâÁ¤?
          LEAVE TO TRANSACTION 'ZIMO3'.
        WHEN 'ZIMP2' OR 'ZIMP3' OR 'ZIMP4'.            " Payment No.
          LEAVE TO TRANSACTION 'ZIMP4'.
        WHEN 'ZIM62' OR 'ZIM63'.
          LEAVE TO TRANSACTION 'ZIM63'.
        WHEN 'ZIM74' OR 'ZIM75' OR 'ZIM76'.            " Payment No.
          LEAVE TO TRANSACTION 'ZIM76'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'DCSD'.          ">>>> ¼±Àû¼­·ù ¼ÛºÎ.
      LEAVE TO TRANSACTION 'ZIM221'.
    WHEN 'DCRP'.          ">>>> ½ÇÀÔÇ× ÀÔ·Â.
      LEAVE TO TRANSACTION 'ZIM222'.
    WHEN 'BLCT'.          ">>>> ºñ¿ë/Àü±â.
      LEAVE TO TRANSACTION 'ZIM223'.
    WHEN 'OPDC'.          ">>>> È®Á¤.
      CASE SY-TCODE.
        WHEN 'ZIMB1' OR 'ZIMB2' OR 'ZIMB3' OR 'ZIMB4'.  " ºÎº¸.
          LEAVE TO TRANSACTION 'ZIMB4'.
        WHEN 'ZIM26' OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'.  " L/G
          LEAVE TO TRANSACTION 'ZIM29'.
        WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9'.    " ¹ÝÀÔ½Å?
          LEAVE TO TRANSACTION 'ZIMI9'.
        WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4'. " ¹ÝÃâÁ¤?
          LEAVE TO TRANSACTION 'ZIMO4'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'STCH'.          " »óÅÂº¯°æ.
      CASE SY-TCODE.
        WHEN 'ZIM31' OR 'ZIM32' OR 'ZIM33' OR 'ZIM34'. " Åë°ü¿äÃ».
          LEAVE TO TRANSACTION 'ZIM34'.
*        WHEN 'ZIM31L' OR 'ZIM32L' OR 'ZIM33L' OR 'ZIM34L'. "ÀÔ°í.
*          LEAVE TO TRANSACTION 'ZIM34L'.
        WHEN 'ZIM35' OR 'ZIM36' OR 'ZIM37' OR 'ZIM38'.  ">>
          LEAVE TO TRANSACTION 'ZIM38'.
*        WHEN 'ZIM35L' OR 'ZIM36L' OR 'ZIM37L' OR 'ZIM38L'. "LO ¹°´ë.
*          LEAVE TO TRANSACTION 'ZIM38L'.
        WHEN OTHERS.
      ENDCASE.

    WHEN 'DISD'.          ">>>> »ý¼º.
    WHEN 'OTDC'.
      LEAVE TO TRANSACTION SY-TCODE.
    WHEN OTHERS.
      LEAVE TO TRANSACTION OK-CODE.
  ENDCASE.

ENDFORM.                    " P2000_OK_CODE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_DISABLE_MENU
*&---------------------------------------------------------------------*
FORM P2000_SET_DISABLE_MENU.
  CASE W_STATUS.
    WHEN C_REQ_C.             " »ý?
*      MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
      MOVE 'NEWL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " NEW ENTRY
      MOVE 'AEND' TO IT_EXCL-FCODE. APPEND IT_EXCL. " CHANGE
      MOVE 'CHDC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " CHANGE DOC.
    WHEN C_REQ_U.             " º¯?
      IF SY-TCODE EQ 'ZIMG02' OR SY-TCODE EQ 'ZIMG03'.
        MOVE 'MKAL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SELECT ALL
        MOVE 'MKLO' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DESELECT
        MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
        MOVE 'DELC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DEL CANCEL
      ENDIF.
      MOVE 'NEWL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " NEW ENTRY
      MOVE 'AEND' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " CHANGE
    WHEN C_REQ_D.             " Á¶?
      MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
      MOVE 'ANZG' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DISPLAY
      MOVE 'MKAL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SELECT ALL
      MOVE 'MKLO' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DESELECT
      MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
      MOVE 'DELC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE CANCE
      MOVE 'SAVE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SAVE
    WHEN OTHERS.             " ±â?
  ENDCASE.

ENDFORM.                    " P2000_SET_DISABLE_MENU
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTBLCST
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTBLCST.

  REFRESH IT_ZSBLCST.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLCST
                              FROM ZTBLCST
                              WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                              ORDER BY ZFCSQ.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTBLCST'.
  ENDIF.


ENDFORM.                    " P1000_READ_ZTBLCST
*&---------------------------------------------------------------------*
*&      Form  SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------*
FORM SET_CURR_CONV_TO_INTERNAL USING    P_AMOUNT
                                        P_WAERS.

  BAPICURR-BAPICURR = P_AMOUNT.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
       EXPORTING
            CURRENCY             = P_WAERS
            AMOUNT_EXTERNAL      = BAPICURR-BAPICURR
            MAX_NUMBER_OF_DIGITS = DIGITS
       IMPORTING
            AMOUNT_INTERNAL      = P_AMOUNT
       EXCEPTIONS
            OTHERS               = 1.

ENDFORM.                    " SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SAVE_PROCESS.
* MESSAGE
  W_OK_CODE = OK-CODE.
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.
* POPUP SCREEN
  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      PERFORM  P3000_DATABASE_MODIFY.
      PERFORM  P2000_SET_LOCK_ZTMSHD USING 'U'.
      MESSAGE  S953.
    WHEN 'N'.              " No...
      IF W_OK_CODE EQ 'BACK' OR W_OK_CODE EQ 'EXIT'.
        MESSAGE  S957.
        PERFORM P2000_SET_LOCK_ZTMSHD USING 'U'.
        PERFORM P1000_READ_MSHD.
        PERFORM P1000_SORT_MSHD.
        LEAVE TO SCREEN 9911.
      ELSE.
        MESSAGE  S957.
        PERFORM P2000_SET_LOCK_ZTMSHD USING 'U'.
        ZTMSHD       =  *ZTMSHD.
        IT_ZSMSIT[]  =  IT_ZSMSIT_ORG[].
        IT_ZSMSCST[] =  IT_ZSMSCST_ORG[].

      ENDIF.
    WHEN 'C'.              " Cancel
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SAVE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  P3000_DATABASE_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_DATABASE_MODIFY.
  CASE SY-TCODE.
    WHEN 'ZIMO6'.
      PERFORM  P3000_ZTBLOUR_MODIFY.
    WHEN 'ZIMMS1'.
      PERFORM  P3000_MSDATA_MODIFY.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " P3000_DATABASE_MODIFY

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTBLCST
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTBLCST.
  CLEAR : W_ZFCSQ.

* Internal Table Sort
  SORT IT_ZSBLCST    BY ZFCSQ.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSBLCST WHERE ZFCSQ <> ''.
    IF W_ZFCSQ   EQ IT_ZSBLCST-ZFCSQ AND SY-TABIX NE 1.
      MESSAGE E969 WITH W_ZFCSQ.
    ENDIF.

    MOVE : IT_ZSBLCST-ZFCSQ   TO W_ZFCSQ,
           SY-TABIX           TO W_TABIX.
* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ C_REQ_C.
      IF IT_ZSBLCST-LOEKZ NE 'X'.
        SELECT SINGLE * FROM ZTBLCST
                        WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                        AND   ZFCSQ   EQ IT_ZSBLCST-ZFCSQ.
        IF SY-SUBRC EQ 0.
          MESSAGE E969 WITH W_ZFCSQ.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSBLCST_DEL.
    DELETE FROM ZTBLCST
           WHERE ZFBLNO EQ ZTBL-ZFBLNO
           AND   ZFCSQ   EQ IT_ZSBLCST_DEL-ZFCSQ.

  ENDLOOP.
  REFRESH : IT_ZSBLCST_DEL.

* µ¥ÀÌÅ¸¸¦ Insert
  LOOP AT IT_ZSBLCST.
    IF IT_ZSBLCST-ZFCSQ IS INITIAL.
      SELECT MAX( ZFCSQ ) INTO IT_ZSBLCST-ZFCSQ
                          FROM ZTBLCST
                          WHERE ZFBLNO EQ ZTBL-ZFBLNO.

      IF IT_ZSBLCST-ZFCSQ IS INITIAL.
        IT_ZSBLCST-ZFCSQ = '00010'.
      ELSE.
        IT_ZSBLCST-ZFCSQ = IT_ZSBLCST-ZFCSQ + 10.
      ENDIF.
      SELECT SINGLE * FROM ZTBLCST
                   WHERE ZFBLNO EQ ZTBL-ZFBLNO
                   AND   ZFCSQ  EQ IT_ZSBLCST-ZFCSQ.
    ENDIF.

    MOVE-CORRESPONDING IT_ZSBLCST   TO ZTBLCST.
    MOVE : SY-UNAME         TO   ZTBLCST-UNAM,
           SY-DATUM         TO   ZTBLCST-UDAT,
           SY-MANDT         TO   ZTBLCST-MANDT,
           ZTBL-ZFBLNO      TO   ZTBLCST-ZFBLNO.

*    IF SY-SUBRC EQ 0.
    IF W_STATUS NE C_REQ_C.
      UPDATE ZTBLCST.
    ELSE.
      IF IT_ZSBLCST-LOEKZ EQ 'X'.
        CONTINUE.
      ENDIF.
      MOVE : SY-UNAME  TO   ZTBLCST-ERNAM,
             SY-DATUM  TO   ZTBLCST-CDAT.

      INSERT ZTBLCST.
    ENDIF.

    IF SY-SUBRC NE 0.
      MESSAGE E952.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTBLCST
*&---------------------------------------------------------------------*
*&      Form  P1000_DATA_REREAD
*&---------------------------------------------------------------------*
FORM P1000_DATA_REREAD.
  CASE SY-TCODE.
    WHEN  'ZIMO6'.

    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " P1000_DATA_REREAD
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_TAB_REFRESH
*&---------------------------------------------------------------------*
FORM P2000_IT_TAB_REFRESH.
  CASE SY-TCODE.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " P2000_IT_TAB_REFRESH
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_DELETE
*&---------------------------------------------------------------------*
FORM P2000_DATA_DELETE.

  CASE SY-TCODE.
    WHEN 'ZIMO6'.   " ½Ç¹Ý?
    WHEN 'ZIM74' OR 'ZIM75'.
      REFRESH IT_ZSCUCLCST_DEL.
      LOOP AT IT_ZSCUCLCST  WHERE ZFMARK = 'X'.
        IF  IT_ZSCUCLCST-ZFACDO NE SPACE.
          MESSAGE  E462.
        ENDIF.
        MOVE-CORRESPONDING IT_ZSCUCLCST TO IT_ZSCUCLCST_DEL.
        APPEND IT_ZSCUCLCST_DEL.
        DELETE IT_ZSCUCLCST.
      ENDLOOP.
    WHEN 'ZIMMS1'.
      IF W_OK_GUBN = '1'.
        LOOP AT IT_ZSMSIT WHERE ZFMARK = 'X'.
          IT_ZSMSIT_DEL = IT_ZSMSIT.
          APPEND  IT_ZSMSIT_DEL.
          DELETE  IT_ZSMSIT.
        ENDLOOP.
        IF SY-SUBRC NE 0. MESSAGE S951. EXIT. ENDIF.
      ELSE.
        LOOP AT IT_ZSMSCST WHERE ZFMARK = 'X'.
          IF IT_ZSMSCST-BELNR  NE SPACE  OR
             IT_ZSMSCST-BELNRV NE SPACE.
            MESSAGE  E237  WITH SY-TABIX.
          ENDIF.
          IT_ZSMSCST_DEL = IT_ZSMSCST.
          APPEND  IT_ZSMSCST_DEL.
          DELETE  IT_ZSMSCST.
        ENDLOOP.
        IF SY-SUBRC NE 0. MESSAGE S951. EXIT. ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_DATA_DELETE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_DEL_MARK
*&---------------------------------------------------------------------*
FORM P2000_SET_DEL_MARK.

  IF OK-CODE EQ 'DELC'.
    W_ROW_MARK  = ''.
  ELSEIF OK-CODE EQ 'DELE'.
    W_ROW_MARK  = 'X'.
  ENDIF.

  CASE SY-TCODE.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " P2000_SET_DEL_MARK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ROW_MARK
*&---------------------------------------------------------------------*
FORM P2000_SET_ROW_MARK.
  IF OK-CODE EQ 'MKAL' OR
     OK-CODE EQ 'MKA1' OR
     OK-CODE EQ 'MKA2' .      " ÀüÃ¼ ¼±?
    W_ROW_MARK = 'X'.
  ELSEIF OK-CODE EQ 'MKLO' OR
     OK-CODE EQ 'MKL1' OR
     OK-CODE EQ 'MKL2'.  " ¼±ÅÃ ÇØ?
    CLEAR W_ROW_MARK.
  ENDIF.

  CASE SY-TCODE.
    WHEN 'ZIMC1'.
      LOOP AT IT_ZSBLCST.
        IT_ZSBLCST-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSBLCST.
      ENDLOOP.
    WHEN 'ZIM74' OR 'ZIM75'.
      LOOP AT IT_ZSCUCLCST.
        IT_ZSCUCLCST-ZFMARK = W_ROW_MARK. MODIFY IT_ZSCUCLCST.
      ENDLOOP.
    WHEN 'ZIMMS1'.
      IF W_OK_GUBN = '1'.
        LOOP AT IT_ZSMSIT.
          IT_ZSMSIT-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSMSIT.
        ENDLOOP.
      ENDIF.
      IF W_OK_GUBN = '2'.
        LOOP AT IT_ZSMSCST.
          IT_ZSMSCST-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSMSCST.
        ENDLOOP.
      ENDIF.
      IF W_OK_GUBN = '3'.
        LOOP AT IT_ZSMSHD.
          IT_ZSMSHD-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSMSHD.
        ENDLOOP.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " P2000_SET_ROW_MARK
*&---------------------------------------------------------------------*
*&      Form  P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------*
FORM P3000_DB_MODIFY_SCRCOM.

  CASE SY-TCODE.
*>> 2002.09.06 NHJ in KE..
    WHEN 'ZIMB1' OR 'ZIMB2' OR 'ZIMB3' OR 'ZIMB4'. " Insurance..
      PERFORM  P3000_ZTINSB_MODIFY.
    WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.            " Carry-In Expected..
      PERFORM  P3000_ZTBLINOU_MODIFY.
    WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9' OR 'ZIMI6'. " Carry-In Declarati.
*>> 2001.09.14 LCK Modified..
      SELECT SINGLE * FROM ZTIMIMG00.
      IF ZTIMIMG00-ZFINOU EQ SPACE.
        PERFORM  P3000_ZTBLINR_TMP_MODIFY.
        EXIT.
      ENDIF.
      PERFORM  P3000_ZTBLINR_MODIFY.
    WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR       " Carry-Out Declaration.
         'ZIMO4' OR 'ZIMO6'.
      PERFORM  P3000_ZTBLOUR_MODIFY.
    WHEN 'ZIM26' OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'.  " L/G Create/Change.
      PERFORM  P3000_ZTLG_MODIFY.
    WHEN 'ZIM21' OR 'ZIM22' OR 'ZIM23' OR           " B/L Create/Change.
         'ZIM221' OR 'ZIM222' OR 'ZIM223'.
      PERFORM  P3000_BL_MASTER_MODIFY.
    WHEN 'ZIM31'  OR 'ZIM32'  OR 'ZIM33'  OR 'ZIM34' OR " Clearance..
         'ZIM31L' OR 'ZIM32L' OR 'ZIM33L' OR 'ZIM34L'.  " Request..
      PERFORM  P3000_ZTIV_MODIFY.
    WHEN 'ZIM35' OR 'ZIM36' OR 'ZIM37' OR 'ZIM38' " Commercial Inovice
      OR 'ZIM35L' OR 'ZIM36L' OR 'ZIM37L' OR 'ZIM38L'.
      PERFORM  P3000_ZTCIV_MODIFY.
    WHEN 'ZIM62' OR 'ZIM63'.                      " Import Declaration.
      PERFORM  P3000_ZTIDR_MODIFY_SCR6200.
    WHEN 'ZIM64' OR 'ZIM77'.            " Reduction Approval Item..
      PERFORM  P3000_ZTIDRCR_MODIFY_SCR6400.
    WHEN 'ZIM66'.                       " Reduction Approval Item Usage.
      PERFORM  P3000_ZTIDRDTU_MODIFY_SCR6600.
    WHEN 'ZIM67' OR 'ZIM68'.       " Taxation Customs Clearance Invoice.
      PERFORM  P3000_ZTCUCLIV_MODIFY_SCR6710.
    WHEN 'ZIM71' OR 'ZIM72'.            " Urgent Bonded Trans. Request.
      PERFORM  P3000_ZTBLUG_MODIFY.
    WHEN 'ZIM74' OR 'ZIM75' OR 'ZIM76'. " Import License.
      PERFORM  P3000_ZTIDS_MODIFY_SCR7410.
    WHEN 'ZIM81' OR 'ZIM82' OR 'ZIM83'. " Cargo Work.
      PERFORM  P3000_ZTCGHD_MODIFY.
    WHEN 'ZIMP2' OR 'ZIMP3' OR 'ZIMP4'. " Payment Notice
      PERFORM  P3000_ZTPMTHD_MODIFY_SCR8210.
    WHEN 'ZIMA3' OR 'ZIMA4'.            " Invoice for tax invoice.
      PERFORM  P3000_ZTVT_MODIFY_SCR8500.
    WHEN 'ZIMA6' OR 'ZIMA7'.            " red.
      PERFORM  P3000_ZTRED_MODIFY_SCR8710.
    WHEN 'ZIMMS1'.                      " Mother Ship Management..
      PERFORM P2000_SAVE_PROCESS.
*>> Add for PAYORD..
    WHEN 'ZIM35P'.
*>> EDI Check..
      PERFORM   P2000_PAYORD_DOC_CHECK.
      UPDATE ZTCIVHD SET ZFEDICK = ZTCIVHD-ZFEDICK
      WHERE  ZFCIVRN = ZTTTHD-ZFCIVRN.
      PERFORM   P2000_PAYORD_FIELD_MOVE USING 'M'.
  ENDCASE.

ENDFORM.                    " P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_NUMBER_NEXT
*&---------------------------------------------------------------------*
FORM P2000_GET_NUMBER_NEXT       USING    P_GUBUN     P_DOCNO.

  CALL FUNCTION 'ZIM_NUMBER_GET_NEXT'
       EXPORTING
            ZFREQTY         = P_GUBUN
       IMPORTING
            ZFREQNO         = P_DOCNO
       EXCEPTIONS
            NOT_INPUT       = 1
            NOT_TYPE        = 2
            NOT_RANGE       = 3
            NOT_FOUND       = 4
            LOCKED          = 6
            ERROR_DUPLICATE = 8.

  CASE SY-SUBRC.
    WHEN 1.     MESSAGE    E012.
    WHEN 2.     MESSAGE    E013      WITH  P_GUBUN.
    WHEN 3.     MESSAGE    E014      WITH  P_DOCNO.
    WHEN 4.     MESSAGE    E964.
    WHEN 6.
      MESSAGE    E510      WITH
                   SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    WHEN 8.     MESSAGE    E015      WITH  P_DOCNO.
  ENDCASE.

ENDFORM.                    " P2000_GET_NUMBER_NEXT
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DOC
*&---------------------------------------------------------------------*
FORM P1000_READ_BL_DOC.

  CALL FUNCTION 'ZIM_GET_BL_DOCUMENT'
       EXPORTING
            ZFBLNO          = ZTBL-ZFBLNO
       IMPORTING
            W_ZTBL          = ZTBL
       TABLES
            IT_ZSBLCST      = IT_ZSBLCST
            IT_ZSBLCST_ORG  = IT_ZSBLCST_ORG
            IT_ZSBLCST1     = IT_ZSBLCST1
            IT_ZSBLCST1_ORG = IT_ZSBLCST1_ORG
            IT_ZSBLCON      = IT_ZSBLCON
            IT_ZSBLCON_ORG  = IT_ZSBLCON_ORG
            IT_ZSBLIT       = IT_ZSBLIT
            IT_ZSBLIT_ORG   = IT_ZSBLIT_ORG
       EXCEPTIONS
            NOT_FOUND       = 4
            NOT_INPUT       = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E038 WITH ZTBL-ZFBLNO.
    WHEN 8.
      MESSAGE E039.
  ENDCASE.

*>> Charge Document Select..
  W_ZFIMDNO = ZTBL-ZFBLNO.
  CALL FUNCTION 'ZIM_GET_COST_DOCUMENT'
       EXPORTING
            ZFCSTGRP    = '004'
            ZFCSTGRP1   = '005'
            ZFIMDNO     = W_ZFIMDNO
       TABLES
            IT_ZSIMCOST = IT_ZSIMCOST.

  CLEAR : ZTIMIMG11.
  SELECT SINGLE * FROM ZTIMIMG11 WHERE BUKRS EQ ZTBL-BUKRS.

*-----------------------------------------------------------------------
* Change History..
*-----------------------------------------------------------------------
   *ZTBL = ZTBL.

* VIA
  MOVE : ZTBL-ZFVIA    TO      W_VIA.
*-----------------------------------------------------------------------
* Invoice Header Data Select
*-----------------------------------------------------------------------
  IF SY-TCODE EQ 'ZIM22'  OR SY-TCODE EQ 'ZIM23' OR
     SY-TCODE EQ 'ZIM221' OR SY-TCODE EQ 'ZIM222' OR
     SY-TCODE EQ 'ZIM223' .  "<= JSYÃß°¡ 20021105.
    IF SY-LANGU EQ '3'.
      CASE SY-TCODE.
        WHEN 'ZIM22'.
          CASE ZTBL-ZFBLST.
            WHEN '2'.  MESSAGE E668 WITH 'Sended shipping Doc'.
            WHEN '3'.
              MESSAGE E668 WITH
                           'Inputed actual arrival in port'.
            WHEN '4'.
              MESSAGE E668 WITH
                           'Completed clearance request'.
            WHEN 'P'.
              MESSAGE E668 WITH
                           'Requested partial clearance'.
          ENDCASE.
        WHEN 'ZIM221'.
          CASE ZTBL-ZFBLST.
            WHEN '2'.  MESSAGE W672 WITH 'Sended shipping Doc'.
            WHEN '3'.
              MESSAGE E668 WITH
                           'Inputed actual arrival in port'.
            WHEN '4'.
              MESSAGE E668 WITH
                           'completed clearance request'.
            WHEN 'P'.  MESSAGE E668 WITH 'Requested partial clearance'.
            WHEN 'N'.
              MESSAGE E668 WITH
                           'Not yet management of Doc status'.
          ENDCASE.
        WHEN 'ZIM222'.
          CASE ZTBL-ZFBLST.
            WHEN '1'.
              MESSAGE W672 WITH
                           'Shipment expected info'.
            WHEN '3'.
              MESSAGE W672 WITH
                           'Inputed actual arrival in port'.
            WHEN '4'.
              MESSAGE E668 WITH
                           'completed clearance request'.
            WHEN 'P'.
              MESSAGE E668 WITH
                           'requested partial clearance'.
            WHEN 'N'.
              MESSAGE E668 WITH
                   'Not yet management of Doc status'.
          ENDCASE.
        WHEN 'ZIM223'.  " <= JSY20021107 ºñ¿ëÀÔ·Â.
          CASE ZTBL-ZFMATGB.
            WHEN '4'.  MESSAGE E446(ZIM1) WITH 'fuel'.
          ENDCASE.
      ENDCASE.
    ELSE.
      CASE SY-TCODE.
        WHEN 'ZIM22'.
          CASE ZTBL-ZFBLST.
            WHEN '2'.  MESSAGE E668 WITH 'Send shipping doc.'.
            WHEN '3'.
              MESSAGE E668 WITH
                      'Entry the actual arrival date'.
            WHEN '4'.
              MESSAGE E668 WITH
                           'Clearenace request completion'.
            WHEN 'P'.
              MESSAGE E668 WITH
                           'Partial-clearenace request'.
          ENDCASE.
        WHEN 'ZIM221'.
          CASE ZTBL-ZFBLST.
            WHEN '2'.  MESSAGE W672 WITH 'Send shipping doc.'.
            WHEN '3'.
              MESSAGE E668 WITH
                           'Entry the actual arrival date'.
            WHEN '4'.
              MESSAGE E668 WITH
                          'Clearenace request completion'.
            WHEN 'P'.  MESSAGE E668 WITH 'Partial-clearenace request'.
            WHEN 'N'.
              MESSAGE E668 WITH
                           'Not object of maintain status'.
          ENDCASE.
        WHEN 'ZIM222'.
          CASE ZTBL-ZFBLST.
            WHEN '1'.  MESSAGE W672 WITH 'Entry shipping doc.'.
            WHEN '3'.
              MESSAGE W672 WITH
                           'Entry the actual arrival date'.
            WHEN '4'.
              MESSAGE E668 WITH
                           'Clearenace request completion'.
            WHEN 'P'.  MESSAGE E668 WITH 'Partial-clearenace request'.
            WHEN 'N'.
              MESSAGE E668 WITH
                           'Not object of maintain status'.
          ENDCASE.
      ENDCASE.

    ENDIF.
    CLEAR : CSKT, W_VENDOR_NM, W_ZFBENI_NM, W_FWDR_NM, W_HAYEK_NM,
            W_ORIGIN_NM, W_IMGR_NM, W_ORIGIN_NM1, W_TRUK_NM,
            W_CARGO_TYPE, W_ZFBNARM, W_TMP_TEXT, W_KUNNR_NM, W_KUNWE_NM.

*-- ÇÑ¼ö¿ø Ãß°¡(¿äÀ²Àû¿ëÁö¿ª)------------------>
    CLEAR: W_AREA_NM.
    IF NOT ZTBL-ZFCDTY IS INITIAL AND
       NOT ZTBL-ZFCD   IS INITIAL.
      SELECT SINGLE ZFCDNM INTO W_AREA_NM
               FROM ZTIMIMG08
              WHERE ZFCDTY = ZTBL-ZFCDTY
                AND ZFCD   = ZTBL-ZFCD.
    ENDIF.
*---------------------------------------------->
    IF NOT ZTBL-KOSTL IS INITIAL.
      SELECT * FROM CSKT UP TO 1 ROWS
               WHERE KOSTL EQ ZTBL-KOSTL.
      ENDSELECT.
    ENDIF.

*>> Get Vendor Name with Vendor Code..
    PERFORM  P1000_GET_VENDOR   USING      ZTBL-LIFNR
                                CHANGING   W_VENDOR_NM.
    PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFBENI
                                CHANGING   W_ZFBENI_NM.

    PERFORM P1000_GET_CUSTOMER_DATA  USING ZTBL-KUNNR
                                           W_KUNNR_NM.
    PERFORM P1000_GET_CUSTOMER_DATA  USING ZTBL-KUNWE
                                           W_KUNWE_NM.

    IF NOT ZTBL-ZFFORD IS INITIAL.
      PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFFORD
                                  CHANGING   W_FWDR_NM.
    ENDIF.

    IF NOT ZTBL-ZFHAYEK IS INITIAL.
      PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFHAYEK
                                  CHANGING   W_HAYEK_NM.
    ENDIF.

    IF NOT ZTBL-ZFPONC IS INITIAL.
      PERFORM  GET_PORT_NAME     USING    '001'   ZTBL-ZFPONC 'I'
                                 CHANGING   W_TMP_TEXT.
    ENDIF.

    IF NOT ZTBL-ZFTRCK IS INITIAL.
      PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFTRCK
                                  CHANGING   W_TRUK_NM.
    ENDIF.
    IF NOT ZTBL-ZFCAGTY IS INITIAL.
      PERFORM   GET_DD07T_SELECT USING      'ZDCAGTY'  ZTBL-ZFCAGTY
                               CHANGING   W_CARGO_TYPE.
    ENDIF.
    IF NOT ZTBL-ZFBNARCD IS INITIAL.
      CLEAR :  ZTIMIMG03.
      SELECT * FROM ZTIMIMG03 WHERE ZFBNARCD EQ ZTBL-ZFBNARCD.
      ENDSELECT.
      W_ZFBNARM = ZTIMIMG03-ZFBNARM.
    ENDIF.

    PERFORM   P1000_INVOICE_DOC_READ.
    PERFORM   P1000_GET_BL_AMOUNT.
  ENDIF.


ENDFORM.                    " P1000_READ_BL_DOC
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_INPUT_INFO
*&---------------------------------------------------------------------*
FORM P1000_READ_INPUT_INFO.

  SELECT MAX( ZFBTSEQ ) INTO W_ZFBTSEQ FROM ZTBLINOU
                                       WHERE ZFBLNO EQ ZTBL-ZFBLNO.

  IF W_ZFBTSEQ IS INITIAL OR W_ZFBTSEQ EQ '00000'.
    W_ZFBTSEQ = '00001'.
  ELSE.
    W_ZFBTSEQ = W_ZFBTSEQ +  1.
  ENDIF.

  ZTBLINOU-ZFBTSEQ = W_ZFBTSEQ.

ENDFORM.                    " P1000_READ_INPUT_INFO
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTBLINOU
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTBLINOU.










ENDFORM.                    " P3000_WRITE_ZTBLINOU

*&---------------------------------------------------------------------*
*&      Form  P3000_BL_MASTER_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_BL_MASTER_MODIFY.
*-----------------------------------------------------------------------
* Data °ËÁõ Ãß°¡ÇÒ ºÎºÐ.
*-----------------------------------------------------------------------
  IF W_OK_CODE NE 'DELE'.
    CLEAR : W_LINE.
    LOOP AT IT_ZSBLIT WHERE BLMENGE GT 0.
      ADD 1 TO W_LINE.
    ENDLOOP.
    IF W_LINE EQ 0.
      MESSAGE E940.
    ENDIF.
  ENDIF.

  IF ZTIMIMG00-BLSTYN EQ 'X'.
    IF SY-TCODE EQ 'ZIM222' AND ZTBL-ZFRPTTY IS INITIAL.
      MESSAGE W939 WITH ZTBL-ZFHBLNO.
    ENDIF.
  ELSE.
    IF ( SY-TCODE EQ 'ZIM21' OR SY-TCODE EQ 'ZIM22' )
                           AND ZTBL-ZFRPTTY IS INITIAL.
      MESSAGE W939 WITH ZTBL-ZFHBLNO.
    ENDIF.
  ENDIF.

  IF W_OK_CODE NE 'DELE' AND     ">»èÁ¦ ±â´É ¼±ÅÃ½Ã.
     W_OK_CODE NE 'SDSD' AND     ">¼±Àû¼­·ù ¼ÛºÎ.
     W_OK_CODE NE 'REAL'.        ">½ÇÀÔÇ× ÀÔ·Â.

    IF ZTIMIMG00-BLSTYN EQ 'X'.
      CASE SY-TCODE.
        WHEN 'ZIM221'.
          MOVE : '2'   TO  ZTBL-ZFBLST.
        WHEN 'ZIM222'.
          MOVE : '3'   TO  ZTBL-ZFBLST.
      ENDCASE.
    ENDIF.

    IF SY-TCODE EQ 'ZIM21' OR SY-TCODE EQ 'ZIM22' OR
       SY-TCODE EQ 'ZIM222'.
* NHJ COMMENT (TEMP)
*      IF ZTBL-ZFNEWT IS INITIAL.
*        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFNEWT'.
*      ENDIF.
*      IF ZTBL-ZFNEWTM IS INITIAL.
*        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFNEWTM'.
*      ENDIF.
*      IF ZTBL-ZFTOVLM IS INITIAL.
*        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFTOVLM'.
*      ENDIF.

*> ¹«È¯ÀÌ°Å³ª, SAMPLE Combine µÈ °æ¿ì.
      IF   ZTBL-ZFPOYN EQ 'N' OR
         ( ZTBL-ZFPOYN EQ 'M' AND W_SAMPLE EQ 'Y' ).
        IF ZTBL-ZFREBELN IS INITIAL.
          IF ZTBL-ZFUPT IS INITIAL.
            PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFUPT'.
          ENDIF.
          IF ZTBL-KOSTL IS INITIAL AND ZTBL-PS_POSID IS INITIAL.
            MESSAGE E211(ZIM1).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ZTBL-ZFFORD IS INITIAL AND SY-TCODE EQ 'ZIM222'.
      IF   ZTBL-INCO1 EQ 'FAS' OR ZTBL-INCO1 EQ 'FCA'
        OR ZTBL-INCO1 EQ 'FOB' OR ZTBL-INCO1 EQ 'EXW'.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFFORD'.
      ENDIF.
    ENDIF.
  ENDIF.

*>¹«È¯Á¾·ù SETTTING.
  CLEAR :  W_ZFPOTY.
  LOOP AT IT_ZSBLIT WHERE BLOEKZ  NE 'X'
                    AND   BLMENGE NE  0.

    IF NOT IT_ZSBLIT-ZFPOTY IS INITIAL.   ">¹«È¯ÀÏ °æ¿ì.
      ADD 1 TO W_POYNN.
      IF W_ZFPOTY IS INITIAL.
        MOVE IT_ZSBLIT-ZFPOTY TO W_ZFPOTY.
      ELSE.
        IF W_ZFPOTY NE IT_ZSBLIT-ZFPOTY.
          MOVE 'Z'  TO W_ZFPOTY.       ">±âÅ¸ ¹«È¯.
        ENDIF.
      ENDIF.
    ELSE.                                 ">À¯È¯ÀÏ °æ¿ì.
      ADD 1 TO W_POYNY.
    ENDIF.
  ENDLOOP.

  IF W_POYNY GT 0.
    IF W_POYNN EQ 0.
      ZTBL-ZFPOYN = 'Y'.
    ELSE.
      ZTBL-ZFPOYN = 'M'.
    ENDIF.
  ELSE.
    IF W_POYNN GT 0.
      ZTBL-ZFPOYN = 'N'.
    ENDIF.
  ENDIF.

  IF W_ZFPOTY EQ 'H'.
    MOVE:'H'    TO ZTBL-ZFPOTY,
         'N'    TO ZTBL-ZFPOYN.
  ELSE.
    MOVE W_ZFPOTY TO ZTBL-ZFPOTY.
  ENDIF.
  ">> Import Transaction Type Set.
  IF ZTBL-ZFPOTY IS INITIAL.
    MOVE  '26'   TO  ZTBL-ZFPONC.   " FTZ.
  ENDIF.

*>> P/O È¯À² UPDATE..
  IF W_OK_CODE NE 'DELE' AND
     W_OK_CODE NE 'SDSD' AND
     W_OK_CODE NE 'REAL'.

    IF ZTIMIMG00-ZFEXFIX EQ 'X' AND
       ( ZTIMIMG00-ZFEXMTD EQ 'B' OR
         ZTIMIMG00-ZFEXMTD EQ 'A' OR
         ZTIMIMG00-ZFEXMTD EQ 'R' ).
*>> È¯À² Ã¼Å©.
      IF ZTBL-ZFEXRT IS INITIAL.
        MESSAGE E167 WITH 'B/L Exchange Rate'.
      ENDIF.
      IT_ZSBLIT_PO[] = IT_ZSBLIT[].
      SORT IT_ZSBLIT_PO  BY EBELN.
      REFRESH : RETURN.
      CLEAR : IT_ZSBLIT_PO, W_EBELN, W_MAX_ETA.
      LOOP AT IT_ZSBLIT_PO.
        IF W_EBELN NE IT_ZSBLIT_PO-EBELN.

*>> ÇØ´ç PO¸¦ ÂüÁ¶ÇÑ BLÁß ¸¶Áö¸· ÀÚ·áÀÎÁö CHECK.
          SELECT MAX( H~ZFETA )  INTO W_MAX_ETA
          FROM   ZTBL  AS  H  INNER JOIN  ZTBLIT AS I
          ON     H~ZFBLNO     EQ    I~ZFBLNO
          WHERE  I~EBELN      EQ    IT_ZSBLIT_PO-EBELN.
          IF ZTBL-ZFETA LT W_MAX_ETA.
            CONTINUE.
          ENDIF.

          SELECT SINGLE * FROM EKKO
                 WHERE EBELN  EQ    IT_ZSBLIT-EBELN.
          IF EKKO-WAERS NE ZTBL-ZFBLAMC.
            MESSAGE E496 WITH EKKO-EBELN EKKO-WAERS ZTBL-ZFBLAMC.
          ENDIF.

          IF EKKO-WAERS EQ ZTBL-ZFBLAMC AND
             EKKO-WKURS EQ ZTBL-ZFEXRT  AND
             EKKO-KUFIX EQ 'X'.
            CONTINUE.
          ENDIF.

*     ON CHANGE OF IT_ZSBLIT-EBELN.
          CLEAR : BAPIMEPOHEADER,
                  BAPIMEPOHEADERX.

          MOVE : 'X'         TO      BAPIMEPOHEADERX-EXCH_RATE,
                 'X'         TO      BAPIMEPOHEADERX-EX_RATE_FX,
                 IT_ZSBLIT_PO-EBELN TO  BAPIMEPOHEADER-PO_NUMBER,
                 ZTBL-ZFEXRT TO      BAPIMEPOHEADER-EXCH_RATE,
                 'X'         TO      BAPIMEPOHEADER-EX_RATE_FX.

          CALL FUNCTION 'ZIM_BAPI_PO_CHANGE'
               EXPORTING
                    PURCHASEORDER = IT_ZSBLIT_PO-EBELN
                    POHEADER      = BAPIMEPOHEADER
                    POHEADERX     = BAPIMEPOHEADERX
               TABLES
                    RETURN        = XRETURN.

          REFRESH : RETURN.
          LOOP AT XRETURN WHERE TYPE EQ 'E'.
            MOVE-CORRESPONDING  XRETURN TO RETURN.
            APPEND RETURN.
          ENDLOOP.
          IF SY-SUBRC EQ 0.
            PERFORM  P2000_MULTI_MSG_MAKE
                                    TABLES  IT_ERR_LIST
                                    USING   IT_ZSBLIT-EBELN.
          ENDIF.
        ENDIF.
        W_EBELN = IT_ZSBLIT_PO-EBELN.
      ENDLOOP.

      DESCRIBE  TABLE IT_ERR_LIST   LINES  W_LINE.
      IF W_LINE GT 0.
        INCLUDE = 'POPU'.
        CALL SCREEN 0015            STARTING AT  05   3
                                    ENDING   AT  102 12.
        CLEAR : INCLUDE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

* BDC Transaction -> Perform Execute
  IF SY-BINPT EQ 'X'.
    PERFORM P1000_GET_BL_AMOUNT.
  ENDIF.

* »ý¼ºÀÏ °æ¿ì ´ÙÀ½ ¹øÈ£¸¦ Ã¤¹ø..
  IF W_STATUS EQ C_REQ_C.
    PERFORM   P2000_GET_NUMBER_NEXT  USING  'BL'  ZTBL-ZFBLNO.
  ENDIF.

* »ý¼ºÀÏ °æ¿ì¸¸ ¼±ÀûÂ÷¼ö Áõ°¡.
  IF W_STATUS EQ C_REQ_C.
    " ¼±ÀûÂ÷¼ö SET.
    CLEAR : W_ZFSHNO.
    SELECT MAX( ZFSHNO ) INTO  W_ZFSHNO
    FROM   ZTBL
    WHERE  ZFREBELN      EQ    ZTBL-ZFREBELN.
    IF W_ZFSHNO IS INITIAL.
      W_ZFSHNO  =  '01'.
    ELSE.
      W_ZFSHNO  =  W_ZFSHNO  +  1.
    ENDIF.
    MOVE   W_ZFSHNO   TO  ZTBL-ZFSHNO.
  ENDIF.

  CALL FUNCTION 'ZIM_BL_DOC_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFBLNO          = ZTBL-ZFBLNO
            ZFSTATUS        = W_STATUS
            W_ZTBL          = ZTBL
            W_ZTBL_OLD      = *ZTBL
       TABLES
            IT_ZSBLCST1     = IT_ZSBLCST1
            IT_ZSBLCST1_OLD = IT_ZSBLCST1_ORG
            IT_ZSBLCST      = IT_ZSBLCST
            IT_ZSBLCST_OLD  = IT_ZSBLCST_ORG
            IT_ZSBLCON      = IT_ZSBLCON
            IT_ZSBLCON_OLD  = IT_ZSBLCON_ORG
            IT_ZSBLIT       = IT_ZSBLIT
            IT_ZSBLIT_OLD   = IT_ZSBLIT_ORG
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    ROLLBACK WORK.
    MESSAGE  E041.
  ELSE.
    COMMIT WORK.
    SET PARAMETER ID 'ZPBLNO' FIELD  ZTBL-ZFBLNO.

    IF ZTIMIMG00-BLSTYN EQ 'X'.
      IF ZTBL-ZFBLST NE '4'   AND   SY-TCODE EQ 'ZIM222' AND
         ZTBL-ZFRPTTY IS INITIAL.

        CALL FUNCTION 'ZIM_BDC_CALL_TRANSACTION_ZIM31'
             EXPORTING
                  W_ZFBLNO  = ZTBL-ZFBLNO
             TABLES
                  RETURN    = RETURN
             EXCEPTIONS
                  REQ_ERROR = 4.

        IF SY-SUBRC EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ENDIF.
    MESSAGE  S037  WITH  ZTBL-ZFBLNO.
  ENDIF.

ENDFORM.                    " P3000_BL_MASTER_MODIFY

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBLINOU_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_ZTBLINOU_MODIFY.
*-----------------------------------------------------------------------
* Data °ËÁõ Ãß°¡ÇÒ ºÎ?
*-----------------------------------------------------------------------
* »ý¼ºÀÏ °æ¿ì ´ÙÀ½ ¹øÈ£¸¦ Ã¤¹ø..
  IF W_STATUS EQ C_REQ_C.
    PERFORM   P1000_READ_INPUT_INFO.
  ENDIF.

  CALL FUNCTION 'ZIM_ZTBLINOU_MODIFY'
     EXPORTING
             W_OK_CODE           =   W_OK_CODE
             ZFBLNO              =   ZTBLINOU-ZFBLNO
             ZFBTSEQ             =   ZTBLINOU-ZFBTSEQ
*            ZFDOCST             =   ZTBLINOU-ZFDOCST
*            ZFEDIST             =   ZTBLINOU-ZFEDIST
             ZFSTATUS            =   W_STATUS
             N_ZTBLINOU          =   ZTBLINOU
             O_ZTBLINOU          =  *ZTBLINOU
    EXCEPTIONS
             ERROR_UPDATE.

  IF W_OK_CODE EQ 'DELE'.
    IF SY-SUBRC NE  0.
      MESSAGE  E313.
    ELSE.
      MESSAGE  S314  WITH  ZTBLINOU-ZFBLNO  ZTBLINOU-ZFBTSEQ.
    ENDIF.
  ELSE.
    IF SY-SUBRC NE  0.
      MESSAGE  E040.
    ELSE.
      MESSAGE  S042  WITH  ZTBLINOU-ZFBLNO  ZTBLINOU-ZFBTSEQ.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_ZTBLINOU_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBLINR_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_ZTBLINR_MODIFY.

*  IF W_OK_CODE EQ 'DELE'.
*     DELETE ZTBLINR.
*     IF SY-SUBRC NE  0.
*        MESSAGE  E313.
*     ELSE.
*        UPDATE  ZTBLINOU  SET : ZFBINYN = ''
*                          WHERE ZFBLNO    EQ   ZTBLINR-ZFBLNO
*                          AND   ZFBTSEQ   EQ   ZTBLINR-ZFBTSEQ.
*
*        MESSAGE  S314  WITH  ZTBLINR-ZFBLNO  ZTBLINR-ZFBTSEQ.
*     ENDIF.
*  ELSE.
*-----------------------------------------------------------------------
* Data °ËÁõ Ãß°¡ÇÒ ºÎ?
*-----------------------------------------------------------------------
  IF W_OK_CODE NE 'DELE'.
    PERFORM   P2000_BLINR_CHECK.
  ENDIF.
*-----------------------------------------------------------------------
* ¼öÀÛ¾÷ °³¼³ÀÏ °æ¿ì, Status º¯?
*-----------------------------------------------------------------------
*     IF W_STATUS EQ C_OPEN_C.
*        IF W_NEW_DOCST NE 'N'.        " OPEN Ãë¼Ò°¡ ¾Æ´Ò °æ¿ì.
*           ZTBLINR-ZFDOCST = 'O'.
*           ZTREQHD-ZFOPNNO  = ZTREQST-ZFOPNNO.
*        ENDIF.
*        W_NEW_DOCST =  'O'.
*     ENDIF.
*     IF W_NEW_DOCST EQ 'N'.        " OPEN Ãë¼ÒÀÏ °æ¿ì.
*        ZTBLINR-ZFDOCST = 'N'.
*     ENDIF.
*     MOVE : SY-DATUM      TO    ZTBLINR-UDAT,
*            SY-UNAME      TO    ZTBLINR-UNAM.

*     UPDATE ZTBLINR.
  CALL FUNCTION 'ZIM_BLINR_DOC_MODIFY'
       EXPORTING
            W_OK_CODE     = W_OK_CODE
            ZFBLNO        = ZTBLINR-ZFBLNO
            ZFBTSEQ       = ZTBLINR-ZFBTSEQ
            ZFSTATUS      = W_STATUS
            W_ZTBLINR     = ZTBLINR
            W_ZTBLINR_OLD = *ZTBLINR
       EXCEPTIONS
            ERROR_UPDATE  = 4
            NOT_MODIFY    = 8.

  IF SY-SUBRC EQ  4.
    MESSAGE  S047  WITH  ZTBLINR-ZFBLNO  ZTBLINR-ZFBTSEQ.
  ELSEIF SY-SUBRC EQ 8.
    MESSAGE  S211.
  ELSE.
    MESSAGE  S042  WITH  ZTBLINR-ZFBLNO  ZTBLINR-ZFBTSEQ.
  ENDIF.

ENDFORM.                    " P3000_ZTBLINR_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM P2000_SET_PF_STATUS.
  CASE SY-TCODE.
    WHEN 'ZIMB1'  OR 'ZIMB2'  OR 'ZIMB3' OR 'ZIMB4'. " º¸Çè»ý¼º.
      MOVE 'INS' TO W_PFSTAT.
    WHEN 'ZIM21'  OR 'ZIM22'  OR 'ZIM23' OR        " B/L »ý¼º,º¯°æ,Á¶È¸.
         'ZIM221' OR 'ZIM222' OR 'ZIM223'.
      MOVE 'BLST' TO W_PFSTAT.
    WHEN 'ZIM26'  OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'. " L/G
      MOVE 'LGST' TO W_PFSTAT.
    WHEN 'ZIM31'  OR 'ZIM32' OR 'ZIM33' OR 'ZIM34'. " Åë°ü ¿äÃ».
      MOVE 'COIV' TO W_PFSTAT.
    WHEN 'ZIM31L' OR 'ZIM32L' OR 'ZIM33L'
                  OR 'ZIM34L'.                     " LOCAL ÀÔ°í¿äÃ».
      MOVE 'LOGR' TO W_PFSTAT.
    WHEN 'ZIM35'  OR 'ZIM36' OR 'ZIM37' OR 'ZIM38'.  " Commercial I/V.
      MOVE 'CINS' TO W_PFSTAT.
    WHEN 'ZIM35L' OR 'ZIM36L' OR 'ZIM37L'
                  OR 'ZIM38L'.                     " LOCAL ¹°´ë»ý¼º.
      MOVE 'CINS' TO W_PFSTAT.
*     WHEN 'ZIM57' OR 'ZIM58' OR 'ZIM34'.   " Invoice±Ý¾×È®Á¤,Á¶È¸,º¯°æ.
*        MOVE 'AMTF' TO W_PFSTAT.
    WHEN 'ZIM62'  OR 'ZIM63'.              " ¼öÀÔ½Å°í.
      MOVE 'IMDR' TO W_PFSTAT.
    WHEN 'ZIM74'  OR 'ZIM75' OR 'ZIM76'.   " ¼öÀÔ¸éÇã.
      MOVE 'IMDS' TO W_PFSTAT.
    WHEN 'ZIM67'  OR 'ZIM68' OR 'ZIM69'.   " °ú¼¼Åë°ü Invoice
      MOVE 'TXCU' TO W_PFSTAT.
    WHEN 'ZIM71'  OR 'ZIM72' OR 'ZIM73'.   " ±ä±Þº¸¼¼¿î¼Û ÀÇ·Ú.
      MOVE 'BTRE' TO W_PFSTAT.
    WHEN 'ZIM81'  OR 'ZIM82' OR 'ZIM83'.   " ÇÏ¿ª°ü¸®.
      MOVE 'CGST' TO W_PFSTAT.
    WHEN 'ZIMI1'  OR 'ZIMI2' OR 'ZIMI3'.   " ¹ÝÀÔ¿¹Á¤Á¤º¸.
      MOVE 'INST' TO W_PFSTAT.
*>> LCK Ãß°¡.2001.09.13.

    WHEN 'ZIMI7'  OR 'ZIMI8' OR 'ZIMI9' OR " ¹ÝÀÔ½Å°íº¯°æ/Á¶È¸/»óÅÂ.
         'ZIMI6'.
      SELECT SINGLE * FROM ZTIMIMG00.
      IF ZTIMIMG00-ZFINOU EQ SPACE.
        MOVE 'INRL' TO W_PFSTAT.
      ELSE.
        MOVE 'IRST' TO W_PFSTAT.
      ENDIF.
    WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR " ¹ÝÃâ½Å°í.
         'ZIMO4'.
      MOVE 'OUST' TO W_PFSTAT.
    WHEN 'ZIMO6'.    ">>>> ½Ç¹ÝÃâÃ³¸®.
      MOVE 'OUS1' TO W_PFSTAT.
    WHEN 'ZIMA0'.                         " ¼¼±Ý°è»ê¼­¿ë InvoiceÁý°è.
      MOVE 'TIVS' TO W_PFSTAT.
    WHEN 'ZIMA2'.                         " ¼¼±Ý°è»ê¼­¿ë InvoiceÁ¶È¸.
      MOVE 'TIVD' TO W_PFSTAT.
    WHEN 'ZIMA3' OR 'ZIMA4'.              " ¼¼±Ý°è»ê¼­.
      MOVE 'TXCA' TO W_PFSTAT.
    WHEN 'ZIMA6' OR 'ZIMA7'.              " ÀÎ¼öÁõ.
      MOVE 'REDO' TO W_PFSTAT.
    WHEN 'ZIM64' OR 'ZIM65' OR 'ZIM77'.  " °¨¸éÇã°¡Ç°¸ñ.
      MOVE 'DRDC' TO W_PFSTAT.
    WHEN 'ZIM66'.                         " °¨¸éÇã°¡Ç°¸ñ »ç¿ë½ÇÀû.
      MOVE 'DRTU' TO W_PFSTAT.
    WHEN 'ZIMP2' OR 'ZIMP3' OR 'ZIMP4'.   " Payment Notice
      MOVE 'PATM' TO W_PFSTAT.
    WHEN 'ZIMMS1'.
      MOVE 'MSST' TO W_PFSTAT.
    WHEN 'ZIM35P'.
      MOVE 'REQ' TO W_PFSTAT.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------*
FORM P2000_SET_GUI_TEXT.

  CASE W_STATUS.
    WHEN C_REQ_C.   ASSIGN W_CREATE  TO <FS_F>.
    WHEN C_REQ_U.
      IF SY-TCODE EQ 'ZIM62'.
        ASSIGN W_REQUEST TO <FS_F>.
      ELSE.
        ASSIGN W_CHANGE  TO <FS_F>.
      ENDIF.
    WHEN C_REQ_D.   ASSIGN W_DISPLAY TO <FS_F>.
    WHEN C_OPEN_C.  ASSIGN W_OPEN    TO <FS_F>.
    WHEN C_BL_REAL. ASSIGN W_REAL    TO <FS_F>.
    WHEN C_BL_COST. ASSIGN W_COST    TO <FS_F>.
    WHEN C_BL_SEND. ASSIGN W_SEND    TO <FS_F>.
*      IF SY-DYNNR EQ '9226' OR SY-DYNNR EQ '9227'.
*         ASSIGN W_PROCESS TO <FS_F>.
*      ENDIF.
    WHEN C_OPEN_U.  ASSIGN W_STAUTS  TO <FS_F>.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_STATUS_SCR_DISABLE.
*&---------------------------------------------------------------------*
FORM P2000_SET_STATUS_SCR_DISABLE.
  CASE SY-DYNNR.
* B/L  ÀÇ?
    WHEN   100.                " B/L »ý¼º ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'BLTI' WITH W_CREATE.     W_STATUS = C_REQ_C.
    WHEN   101.                " B/L »ý¼º ÃÊ±âÈ­?
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL." COPY.
      CASE ZTBL-ZFBLST.
        WHEN '1' OR 'N' OR '4' OR 'P'.
          MOVE 'SDSD' TO IT_EXCL-FCODE.
          APPEND IT_EXCL. " ¼ÛºÎÃë¼Ò.
          MOVE 'REAL' TO IT_EXCL-FCODE.
          APPEND IT_EXCL. " ½ÇÀÔÇ× Ãë¼Ò.
        WHEN '2'.
          MOVE 'REAL' TO IT_EXCL-FCODE.
          APPEND IT_EXCL. " ¼ÛºÎÃë¼Ò.
        WHEN '3'.
          MOVE 'SDSD' TO IT_EXCL-FCODE.
          APPEND IT_EXCL. " ¼ÛºÎÃë¼Ò.
        WHEN OTHERS.
      ENDCASE.
      IF ZTIMIMG00-BLSTYN EQ 'X'.   ">B/L »ó?
        CASE SY-TCODE.
          WHEN 'ZIM21' OR 'ZIM22' OR 'ZIM221'.
            MOVE 'POST' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
        ENDCASE.
      ENDIF.
*      IF ZTIMIMG00-BLCSTMD EQ 'X'.
*         IF ZTBL-INCO1 NE 'EXW' AND ZTBL-INCO1 NE 'FCA' AND
*            ZTBL-INCO1 NE 'FAS' AND ZTBL-INCO1 NE 'FOB'.
*            MOVE 'POST' TO IT_EXCL-FCODE.   APPEND IT_EXCL.
*         ENDIF.
*      ENDIF.
    WHEN   200.                " B/L º¯°æ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      CASE SY-TCODE.
        WHEN 'ZIM22'.
          SET TITLEBAR  'BLTI' WITH W_CHANGE.
          W_STATUS = C_REQ_U.
        WHEN 'ZIM221'.
          SET TITLEBAR  'BLTI' WITH W_SEND.
          W_STATUS = C_BL_SEND.
        WHEN 'ZIM222'.
          SET TITLEBAR  'BLTI' WITH W_REAL.
          W_STATUS = C_BL_REAL.
        WHEN 'ZIM223'.
          SET TITLEBAR  'BLTI' WITH W_COST.
          W_STATUS = C_BL_COST.
      ENDCASE.
    WHEN   300.                " B/L Á¶È¸ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'BLTI' WITH W_DISPLAY.    W_STATUS = C_REQ_D.
    WHEN  0810.              " ÇÏ¿ª°ü¸® »ý¼ºÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'BLST' WITH 'Create'
                     'Loading/Unloading management' 'Initial Screen'.
      W_STATUS = C_REQ_C.
    WHEN  0820.              " ÇÏ¿ª°ü¸® º¯°æÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'BLST'
        WITH 'Change' 'Cargo Work Management' 'Initial Screen'.
      W_STATUS = C_REQ_U.
    WHEN  0830.              " ÇÏ¿ª°ü¸® Á¶È¸ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'BLST'
        WITH 'Display' 'Cargo Work Management' 'Initial Screen'.
      W_STATUS = C_REQ_D.
* L/G »ý¼º/º¯°æ/Á¶È¸ ÃÊ±âÈ­?
    WHEN  2600.                " L/G »ý¼º ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'LGTI' WITH W_CREATE.     W_STATUS = C_REQ_C.
    WHEN  2601.                " B/L »ý¼º ÃÊ±âÈ­?
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " COPY
      MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " COPY
    WHEN  2700.                " L/G º¯°æ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'LGTI' WITH W_CHANGE.     W_STATUS = C_REQ_U.
    WHEN  2800.                " L/G Á¶È¸ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'LGTI' WITH W_DISPLAY.    W_STATUS = C_REQ_D.
    WHEN  2900.                " L/G È®Á¤ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'LGTI' WITH W_OPEN.    W_STATUS = C_OPEN_C.
*-----------------------------------------------------------------------
* Invoice Application
*-----------------------------------------------------------------------
    WHEN  3110.              " Åë°ü¿äÃ» ¼¼ºÎÈ­¸é.
*>> ¹ÌÅë°ü ´ë»ó.
      IF ZTIV-ZFCLCD EQ 'X'.
        MOVE 'IMPREQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">¼öÀÔÀÇ·Ú.
      ENDIF.

      IF SY-TCODE(5) = 'ZIM31'.
        IF SY-TCODE EQ 'ZIM31'.
          SET TITLEBAR  'COIVD' WITH W_CREATE.
        ELSE.
          IF ZTIV-ZFREQTY EQ 'LO'.
            SET TITLEBAR  'COIVL' WITH W_CREATE 'Local L/C'.
          ELSEIF ZTIV-ZFREQTY EQ 'PU'.
            SET TITLEBAR  'COIVL' WITH W_CREATE 'Purchase license'.
          ENDIF.
        ENDIF.

        MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »ý?
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      ENDIF.
      IF SY-TCODE(5) = 'ZIM32'.
        IF SY-TCODE EQ 'ZIM32'.
          SET TITLEBAR  'COIVD' WITH W_CHANGE.
        ELSE.
          IF ZTIV-ZFREQTY EQ 'LO'.
            SET TITLEBAR  'COIVL' WITH W_CHANGE 'Local L/C'.
          ELSEIF ZTIV-ZFREQTY EQ 'PU'.
            SET TITLEBAR  'COIVL' WITH W_CHANGE 'Purchase license'.
          ENDIF.
        ENDIF.
        MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
      ENDIF.
      IF SY-TCODE(5) = 'ZIM33'.
        IF SY-TCODE EQ 'ZIM33'.
          SET TITLEBAR  'COIVD' WITH W_DISPLAY.
        ELSE.
          IF ZTIV-ZFREQTY EQ 'LO'.
            SET TITLEBAR  'COIVL' WITH W_DISPLAY 'Local L/C'.
          ELSEIF ZTIV-ZFREQTY EQ 'PU'.
            SET TITLEBAR  'COIVL' WITH W_DISPLAY 'Purchase license'.
          ENDIF.
        ENDIF.
        MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      ENDIF.
      IF ( ZTIV-ZFCUST EQ 'Y' OR ZTIV-ZFCUST EQ 'N' ).
        IF ZTIV-ZFGRST EQ 'Y'.
          MOVE 'GRPT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " »èÁ¦.
        ELSEIF ZTIV-ZFGRST EQ 'N'.
          MOVE 'GRRV' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " »èÁ¦.
        ENDIF.
      ELSE.
        MOVE 'GRPT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀÔ°í,
        MOVE 'GRRV' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀÔ°íÃë¼Ò.
      ENDIF.
      MOVE 'LOPU' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÂüÁ¶.
    WHEN  3100 .              " Åë°ü¿äÃ» »ý¼º ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'COIVI' WITH W_CREATE.   W_STATUS = C_REQ_C.
    WHEN  3101 .              " LOCAL ÀÔ°í¿äÃ» »ý¼º ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'LOIVI' WITH W_CREATE.   W_STATUS = C_REQ_C.
    WHEN  3200.              " Åë°ü¿äÃ» º¯°æ ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'COIVI' WITH W_CHANGE.   W_STATUS = C_REQ_U.
    WHEN  3201 .              " LOCAL ÀÔ°í¿äÃ» º¯°æ ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'LOIVI' WITH W_CHANGE.   W_STATUS = C_REQ_U.
    WHEN  3300.              " Åë°ü¿äÃ» Á¶È¸ ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'COIVI' WITH W_DISPLAY.  W_STATUS = C_REQ_D.
    WHEN  3301 .              " LOCAL ÀÔ°í¿äÃ» »ý¼º ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'LOIVI' WITH W_DISPLAY.   W_STATUS = C_REQ_D.
    WHEN  3400.              " Åë°ü¿äÃ» »óÅÂº¯°æ ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'COIVI' WITH W_DISPLAY.  W_STATUS = C_OPEN_U.
    WHEN  3401.              " LOCAL ÀÔ°í¿äÃ» »ý¼º ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'LOIVI' WITH W_CREATE.   W_STATUS = C_REQ_C.
    WHEN 3500.               " Commercial Invoice Create init.
      PERFORM   P2000_SET_BL_INIT_SCR.

      SET TITLEBAR  'BLST'
          WITH 'Create' 'Commercial Invoice' 'Initial Screen'.
      W_STATUS = C_REQ_C.
    WHEN 3501.               ">LOCAL ¹°´ë.
      PERFORM   P2000_SET_BL_INIT_SCR.

      SET TITLEBAR  'BLST'
          WITH 'Create' 'Local Gross Price' 'Initial Screen'.
      W_STATUS = C_REQ_C.
    WHEN 3600.               " Commercial Invoice Change init.
      PERFORM   P2000_SET_BL_INIT_SCR.
      MOVE 'REBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L ÂüÁ¶.
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C ÂüÁ¶.
      SET TITLEBAR  'BLST'
          WITH 'Change' 'Commercial Invoice' 'Initial Screen'.
      W_STATUS = C_REQ_U.
    WHEN 3601.               ">LOCAL ¹°´ë.
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'BLST'
          WITH 'Change' 'Local gross price' 'Initial Screen'.
      W_STATUS = C_REQ_U.
    WHEN 3700.               " Commercial Invoice Display init.
      PERFORM   P2000_SET_BL_INIT_SCR.
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C ÂüÁ¶.
      MOVE 'REBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L ÂüÁ¶.
      SET TITLEBAR  'BLST'
          WITH 'Display' 'Commercial Invoice' 'Initial Screen'.
      W_STATUS = C_REQ_D.
    WHEN 3701.               ">LOCAL ¹°´ë.
      PERFORM   P2000_SET_BL_INIT_SCR.
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C ÂüÁ¶.
      MOVE 'REBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L ÂüÁ¶.
      SET TITLEBAR  'BLST'
          WITH 'Display' 'Local gross price' 'Initial Screen'.
      W_STATUS = C_REQ_D.
    WHEN 3800.               " Commercial Invoice Display init.
      PERFORM   P2000_SET_BL_INIT_SCR.
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C ÂüÁ¶.
      MOVE 'REBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L ÂüÁ¶.
      SET TITLEBAR  'BLST'
          WITH 'Status Change' 'Commercial Invoice' 'Initial Screen'.
      W_STATUS = C_OPEN_U.
    WHEN 3510.
      SELECT SINGLE * FROM ZTIMIMGTX WHERE BUKRS EQ ZTCIVHD-BUKRS.

      CASE ZTCIVHD-ZFREQTY.
        WHEN 'LO'.
          SET TITLEBAR  'BLST'
              WITH <FS_F> 'Local Gross Price' 'Detailed Screen'.
        WHEN 'PU'.
          SET TITLEBAR  'BLST'
              WITH <FS_F> 'Purchase license gross price'
                          'Detailed Screen'.
        WHEN OTHERS.
          SET TITLEBAR  'BLST'
              WITH <FS_F> 'Commercial Invoice' 'Detailed Screen'.
      ENDCASE.

      MOVE 'REBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L ÂüÁ¶.
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C ÂüÁ¶.
      MOVE 'LOPU' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀüÇ¥ CANCEL
      CASE ZTCIVHD-ZFIVST.
        WHEN 'Y'.
          MOVE 'MIRO' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        WHEN 'N'.
          MOVE 'MIR2' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
          MOVE 'RERQ'  TO IT_EXCL-FCODE.   APPEND IT_EXCL. "°áÀç¿äÃ».
        WHEN 'X'.
          MOVE 'MIRO' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
          MOVE 'MIR2' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDCASE.
      " < 2002.09.13 NHJ - EDI(PAYORD) »ç¿ë½Ã¿¡¸¸ DISPALY >
      IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
        IF ZTIMIMGTX-PAYORD NE 'X'.
          MOVE 'PAYORD'   TO  IT_EXCL-FCODE.  APPEND IT_EXCL.
        ENDIF.
      ELSE.
        MOVE 'PAYORD'      TO  IT_EXCL-FCODE.  APPEND IT_EXCL.
      ENDIF.
*-----------------------------------------------------------------------
* Invoice Processing
*-----------------------------------------------------------------------
    WHEN  5710.              " Invoice ±Ý¾× ¼¼ºÎÈ­?
      IF SY-TCODE = 'ZIM57'.
        SET TITLEBAR  'AMTFD' WITH 'Amount decision'.
        MOVE 'AMTF' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF SY-TCODE = 'ZIM58'.
        SET TITLEBAR  'AMTFD' WITH 'amount inquiry'.
        MOVE 'AMTI' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF SY-TCODE = 'ZIM34'.
        SET TITLEBAR  'AMTFD' WITH 'status change'.
        MOVE 'STCG' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
    WHEN  5700.              " Invoice ±Ý¾×È®Á¤ ÃÊ±âÈ­?
      SET TITLEBAR  'AMTFI'.
      MOVE 'AMTF' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ±Ý¾×È®?
      MOVE 'IVBD' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " I/V BDC
      MOVE 'GRBD' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " G/R BDC
      MOVE 'DSRQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C Á¶?
      MOVE 'DSBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶?
      MOVE 'MK03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¹°´ë Vendor
      MOVE 'FB031' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " ¹°´ëÈ¸°èÀü?
      MOVE 'FB032' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " Á¦ºñ¿ëÀü?
      MOVE 'MB03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀÚÀç¹®?
    WHEN  5800.              " Invoice ±Ý¾×Á¶È¸ ÃÊ±âÈ­?
      SET TITLEBAR  'AMTII'.
      MOVE 'AMTI' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ±Ý¾×Á¶?
      MOVE 'IVBD' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " I/V BDC
      MOVE 'GRBD' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " G/R BDC
      MOVE 'DSRQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C Á¶?
      MOVE 'DSBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶?
      MOVE 'MK03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¹°´ë Vendor
      MOVE 'FB031' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " ¹°´ëÈ¸°èÀü?
      MOVE 'FB032' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " Á¦ºñ¿ëÀü?
      MOVE 'MB03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀÚÀç¹®?
*      WHEN  3400.              " Invoice »óÅÂº¯°æ ÃÊ±âÈ­?
*         SET TITLEBAR  'STCGI'.
*         MOVE 'STCG' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ±Ý¾×Á¶?
*         MOVE 'IVBD' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " I/V BDC
*         MOVE 'GRBD' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " G/R BDC
*         MOVE 'DSRQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C Á¶?
*         MOVE 'DSBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶?
*         MOVE 'MK03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¹°´ë Vendor
*         MOVE 'FB031' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " ¹°´ëÈ¸°èÀü?
*         MOVE 'FB032' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " Á¦ºñ¿ëÀü?
*         MOVE 'MB03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀÚÀç¹®?
*-----------------------------------------------------------------------
* ¼öÀÔ½Å?
*-----------------------------------------------------------------------
    WHEN  6210.              " ¼öÀÔ½Å°í ¼¼ºÎÈ­.
      CLEAR ZTIMIMGTX.
      SELECT SINGLE * FROM ZTIMIMGTX WHERE BUKRS EQ ZTBL-BUKRS.
      IF ZTIDR-ZFDOCST NE 'N'.
        MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
      ENDIF.
      IF SY-TCODE = 'ZIM62'.
        SET TITLEBAR  'IMDRD' WITH W_REQUEST.
        MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
      ENDIF.
      IF SY-TCODE = 'ZIM63'.
        SET TITLEBAR  'IMDRD' WITH W_DISPLAY.
        MOVE 'REOG' TO IT_EXCL-FCODE. APPEND IT_EXCL. " ¶õ Àç±¸?
        MOVE 'SVCO' TO IT_EXCL-FCODE. APPEND IT_EXCL. " Save & Co
      ENDIF.
      " EDI »ç¿ë¿©ºÎ¿¡ µû¶ó¼­ MENU INVISIBLE, VISIBLE. <2002.09.13 NHJ >
      IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
        IF ZTIMIMGTX-IMPREQ NE 'X'.
          MOVE  'EDIS'  TO  IT_EXCL-FCODE.  APPEND IT_EXCL.
        ENDIF.
      ELSE.
        MOVE 'EDIS' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      ENDIF.
    WHEN  6200.              " ¼öÀÔ½Å°í º¯°æ ÃÊ±âÈ­?
      SET TITLEBAR  'IMDRI' WITH W_REQUEST.   W_STATUS = C_REQ_U.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'REOG' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¶õ Àç±¸?
      MOVE 'SVCO' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Save & Conf.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " »è?
      MOVE 'DSBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶?
      MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
    WHEN  6300.              " ¼öÀÔ½Å°í Á¶È¸ ÃÊ±âÈ­?
      SET TITLEBAR  'IMDRI' WITH W_DISPLAY.  W_STATUS = C_REQ_D.
      MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'REOG' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¶õ Àç±¸?
      MOVE 'SVCO' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Save & Conf.
      MOVE 'DSBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶?
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶?
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
*-----------------------------------------------------------------------
* ¼öÀÔ¸é?
*-----------------------------------------------------------------------
    WHEN  7410.              " ¼öÀÔ¸éÇã ¼¼ºÎÈ­?
      IF SY-TCODE = 'ZIM74'.
        SET TITLEBAR  'IMDSD' WITH W_CREATE.
        MOVE 'OTDC' TO IT_EXCL-FCODE. APPEND IT_EXCL.
        MOVE 'CRDC' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " »ý?
        MOVE 'CUCL' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " Åë?
        MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " Åë?
        MOVE 'PRES' TO IT_EXCL-FCODE. APPEND IT_EXCL. " ½Å°íÇÊÁõ.
      ENDIF.
      IF SY-TCODE = 'ZIM75'.
        SET TITLEBAR  'IMDSD' WITH W_CHANGE.
        MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
        MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF SY-TCODE = 'ZIM76'.
        SET TITLEBAR  'IMDSD' WITH W_DISPLAY.
*       MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " »è?
        MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
    WHEN  7400.              " ¼öÀÔ¸éÇã »ý¼º ÃÊ±âÈ­?
      SET TITLEBAR  'IMDSI' WITH W_CREATE.   W_STATUS = C_REQ_C.
      MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
*      MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
*      MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'PRES' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ½Å°íÇÊÁõ.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'DSBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶?
      MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶?
      MOVE 'COST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DSDR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'HIIT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
    WHEN  7500.              " ¼öÀÔ¸éÇã º¯°æ ÃÊ±âÈ­?
      SET TITLEBAR  'IMDSI' WITH W_CHANGE.   W_STATUS = C_REQ_U.
*      MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
*      MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
*      MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'PRES' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ½Å°íÇÊÁõ.
      MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'DSBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶?
      MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶?
      MOVE 'COST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DSDR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'HIIT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
    WHEN  7600.              " ¼öÀÔ¸éÇã Á¶È¸ ÃÊ±âÈ­?
      SET TITLEBAR  'IMDSI' WITH W_DISPLAY.  W_STATUS = C_REQ_D.
*     MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
*     MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " º¯È­.
*     MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'PRES' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ½Å°íÇÊÁõ.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " »èÁ¦.
      MOVE 'DSBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " B/L Á¶È¸.
      MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'COST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'HIIT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DSDR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
*-----------------------------------------------------------------------
* °¨¸éÇã°¡Ç°¸ñ.
*-----------------------------------------------------------------------
    WHEN  6410.              " °¨¸éÇã°¡Ç°¸ñ ¼¼ºÎÈ­?
      IF SY-TCODE = 'ZIM64'.
        SET TITLEBAR  'DRDCD' WITH W_CHANGE.
      ENDIF.
      IF SY-TCODE = 'ZIMI64'.
        SET TITLEBAR  'DRDCD' WITH W_CREATE.
      ENDIF.

      IF SY-TCODE = 'ZIM65'.
        SET TITLEBAR  'DRDCD' WITH W_DISPLAY.
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      ENDIF.

    WHEN  6400.              " °¨¸éÇã°¡Ç°¸ñ º¯°æ ÃÊ±âÈ­?
      SET TITLEBAR  'DRDCI' WITH W_CHANGE.   W_STATUS = C_REQ_U.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
    WHEN  6450.              " °¨¸éÇã°¡Ç°¸ñ »ý¼º ÃÊ±âÈ­?
      SET TITLEBAR  'DRDCI' WITH W_CREATE.   W_STATUS = C_REQ_C.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?

    WHEN  6500.              " °¨¸éÇã°¡Ç°¸ñ Á¶È¸ ÃÊ±âÈ­?
      SET TITLEBAR  'DRDCI' WITH W_DISPLAY.  W_STATUS = C_REQ_D.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
*-----------------------------------------------------------------------
* °¨¸éÇã°¡Ç°¸ñ »ç¿ë½Ç?
*-----------------------------------------------------------------------
    WHEN  6600.              " °¨¸éÇã°¡Ç°¸ñ »ç¿ë½ÇÀû ÃÊ±âÈ­?
      SET TITLEBAR  'DRTUI' WITH 'Change'.
      MOVE 'SAVE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " Save
      MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL. " Save
      MOVE 'MKAL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " Save
      MOVE 'MKLO' TO IT_EXCL-FCODE. APPEND IT_EXCL. " Save
      MOVE 'REFR' TO IT_EXCL-FCODE. APPEND IT_EXCL. " Save
    WHEN  6610.              " °¨¸éÇã°¡Ç°¸ñ »ç¿ë½ÇÀû ¼¼ºÎÈ­?
      SET TITLEBAR  'DRTUD' WITH 'Change'.
*-----------------------------------------------------------------------
* °ú¼¼Åë°ü Invoice
*-----------------------------------------------------------------------
    WHEN  6710.              " °ú¼¼Åë°ü Invoice ¼¼ºÎÈ­?
      IF SY-TCODE = 'ZIM67'.
        SET TITLEBAR  'TXCUD' WITH W_CREATE.
        MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »ý?
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      ENDIF.
      IF SY-TCODE = 'ZIM68'.
        SET TITLEBAR  'TXCUD' WITH W_CHANGE.
        MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
      ENDIF.
      IF SY-TCODE = 'ZIM69'.
        SET TITLEBAR  'TXCUD' WITH W_DISPLAY.
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      ENDIF.
    WHEN  6700.              " °ú¼¼Åë°ü Invoice »ý¼º ÃÊ±âÈ­?
      SET TITLEBAR  'TXCUI' WITH W_CREATE.   W_STATUS = C_REQ_C.
      MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »ý?
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
    WHEN  6800.              " °ú¼¼Åë°ü Invoice º¯°æ ÃÊ±âÈ­?
      SET TITLEBAR  'TXCUI' WITH W_CHANGE.   W_STATUS = C_REQ_U.
      MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
    WHEN  6900.              " °ú¼¼Åë°ü Invoice Á¶È¸ ÃÊ±âÈ­?
      SET TITLEBAR  'TXCUI' WITH W_DISPLAY.  W_STATUS = C_REQ_D.
      MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL.
*-----------------------------------------------------------------------
* ±ä±Þº¸¼¼¿î?
*-----------------------------------------------------------------------
    WHEN  7110.              " ±ä±Þº¸¼¼¿î¼Û ÀÇ·Ú ¼¼ºÎÈ­?
      IF SY-TCODE = 'ZIM71'.
        SET TITLEBAR  'BTRED' WITH W_CREATE.
        MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »ý?
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      ENDIF.
      IF SY-TCODE = 'ZIM72'.
        SET TITLEBAR  'BTRED' WITH W_CHANGE.
        MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
      ENDIF.
      IF SY-TCODE = 'ZIM73'.
        SET TITLEBAR  'BTRED' WITH W_DISPLAY.
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      ENDIF.
    WHEN  7100.              " ±ä±Þº¸¼¼¿î¼Û ÀÇ·Ú »ý¼º ÃÊ±âÈ­?
      SET TITLEBAR  'BTREI' WITH W_CREATE.  W_STATUS = C_REQ_C.
      MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »ý?
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
    WHEN  7200.              " ±ä±Þº¸¼¼¿î¼Û ÀÇ·Ú º¯°æ ÃÊ±âÈ­?
      SET TITLEBAR  'BTREI' WITH W_CHANGE.  W_STATUS = C_REQ_U.
      MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
    WHEN  7300.              " ±ä±Þº¸¼¼¿î¼Û ÀÇ·Ú Á¶È¸ ÃÊ±âÈ­?
      SET TITLEBAR  'BTREI' WITH W_DISPLAY. W_STATUS = C_REQ_D.
      MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL.
*-----------------------------------------------------------------------
* Payment Notice °ü?
*-----------------------------------------------------------------------
    WHEN  8210.              " Payment Notice °ü¸® ¼¼ºÎÈ­?

      IF SY-TCODE = 'ZIMP2'.
        SET TITLEBAR  'PATD' WITH W_CREATE.
        MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »ý?
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
        MOVE 'DSTR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF SY-TCODE = 'ZIMP3'.
        SET TITLEBAR  'PATD' WITH W_CHANGE.
        MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
      ENDIF.
      IF SY-TCODE = 'ZIMP4'.
        SET TITLEBAR  'PATD' WITH W_DISPLAY.
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
        MOVE 'SVCO' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
        MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      ENDIF.
    WHEN  8220.              " Payment Notice °ü¸® ¼¼ºÎÈ­¸é(Local L/C)
      IF SY-TCODE = 'ZIMP2'.
        SET TITLEBAR  'PATD' WITH W_CREATE.
        MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »ý?
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      ENDIF.
      IF SY-TCODE = 'ZIMP3'.
        SET TITLEBAR  'PATD' WITH W_CHANGE.
        MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " º¯?
      ENDIF.
      IF SY-TCODE = 'ZIMP4'.
        SET TITLEBAR  'PATD' WITH W_DISPLAY.
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
        MOVE 'SVCO' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      ENDIF.
    WHEN  8100.              " Payment Notice »ý¼º ÃÊ±âÈ­?
      SET TITLEBAR  'PATI' WITH W_CREATE.   W_STATUS = C_REQ_C.
      PERFORM  P2000_SET_PAYMENT_INT_SCREEN.
    WHEN  8110.              " Payment Notice º¯°æ ÃÊ±âÈ­?
      SET TITLEBAR  'PATI' WITH W_CHANGE.   W_STATUS = C_REQ_U.
      PERFORM  P2000_SET_PAYMENT_INT_SCREEN.
    WHEN  8120.              " Payment Notice Á¶È¸ ÃÊ±âÈ­?
      SET TITLEBAR  'PATI' WITH W_DISPLAY.  W_STATUS = C_REQ_D.
      PERFORM  P2000_SET_PAYMENT_INT_SCREEN.
*-----------------------------------------------------------------------
* ¼¼±Ý°è»ê¼­¿ë Invoice
*-----------------------------------------------------------------------
    WHEN  8300.              " ¼¼±Ý°è»ê¼­¿ë Invoice Áý?
      SET TITLEBAR  'TIVS'.
    WHEN  8400.              " ¼¼±Ý°è»ê¼­¿ë Invoice Á¶È¸ ÃÊ±âÈ­?
      SET TITLEBAR  'TIVDI'.
      MOVE 'BKPF' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
    WHEN  8410.              " ¼¼±Ý°è»ê¼­¿ë Invoice Á¶È¸ ¼¼ºÎÈ­?
      SET TITLEBAR  'TIVDD'.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
*-----------------------------------------------------------------------
* ¼¼±Ý°è»ê?
*-----------------------------------------------------------------------
    WHEN  8510.              " ¼¼±Ý°è»ê¼­ ¼¼ºÎÈ­?
      IF SY-TCODE = 'ZIMA3'.
        SET TITLEBAR  'TXCAD' WITH W_CHANGE.
        MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF SY-TCODE = 'ZIMA4'.
        SET TITLEBAR  'TXCAD' WITH W_DISPLAY.
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
        MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " ÀúÀå.
        MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " ¹°Ç°.
      ENDIF.
    WHEN  8500.              " ¼¼±Ý°è»ê¼­ º¯°æ ÃÊ±âÈ­?
      SET TITLEBAR  'TXCAI' WITH W_CHANGE.   W_STATUS = C_REQ_U.
      MOVE 'DELE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »èÁ¦.
      MOVE 'OTDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ´Ù¸¥.
      MOVE 'SAVE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ÀúÀå.
      MOVE 'CO'    TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " °ø±ÞÀÚ.
      MOVE 'IT'    TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
      MOVE 'CHDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
      MOVE 'DISTR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
    WHEN  8600.              " ¼¼±Ý°è»ê¼­ Á¶È¸ ÃÊ±âÈ­?
      SET TITLEBAR  'TXCAI' WITH W_DISPLAY.  W_STATUS = C_REQ_D.
      MOVE 'DELE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'OTDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'SAVE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'CO'    TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'IT'    TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'DISP'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
      MOVE 'DISTR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
*-----------------------------------------------------------------------
* ÀÎ¼ö?
*-----------------------------------------------------------------------
    WHEN  8710.              " ÀÎ¼öÁõ ¼¼ºÎÈ­?
      IF SY-TCODE = 'ZIMA6'.
        SET TITLEBAR  'REDOD' WITH W_CHANGE.
        MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      ENDIF.
      IF SY-TCODE = 'ZIMA7'.
        SET TITLEBAR  'REDOD' WITH W_DISPLAY.
        MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
        MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
        MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      ENDIF.
    WHEN  8700.              " ÀÎ¼öÁõ º¯°æ ÃÊ±âÈ­?
      SET TITLEBAR  'REDOI' WITH W_CHANGE.   W_STATUS = C_REQ_U.
      MOVE 'OTDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'CHDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'DELE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'SAVE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'LC'    TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'DIPO'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
      MOVE 'DILI'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
      MOVE 'DIVT'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
      MOVE 'DISCR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
    WHEN  8800.              " ÀÎ¼öÁõ Á¶È¸ ÃÊ±âÈ­?
      SET TITLEBAR  'REDOI' WITH W_DISPLAY.  W_STATUS = C_REQ_D.
      MOVE 'OTDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'DELE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'SAVE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'DISP'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'LC'    TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
      MOVE 'DIPO'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
      MOVE 'DILI'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
      MOVE 'DIVT'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " ¹°Ç°.
      MOVE 'DISCR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
*-----------------------------------------------------------------------
* ¹ÝÀÔ¿¹Á¤Á¤?
*-----------------------------------------------------------------------
    WHEN  9200.              " ¹ÝÀÔ¿¹Á¤Á¤º¸ »ý¼º ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'INTI' WITH W_CREATE.   W_STATUS = C_REQ_C.
    WHEN  9201.              " ¹ÝÀÔ¿¹Á¤Á¤º¸ ¼¼?
      SET TITLEBAR  'INT1' WITH <FS_F>.
    WHEN  9202.              " ¹ÝÀÔ¿¹Á¤Á¤º¸ º¯°æ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'INTI' WITH W_CHANGE.  W_STATUS = C_REQ_U.
    WHEN  9203.              " ¹ÝÀÔ¿¹Á¤Á¤º¸ Á¶È¸ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'INTI' WITH W_DISPLAY. W_STATUS = C_REQ_D.
*-----------------------------------------------------------------------
* ¹ÝÀÔ½Å?
*-----------------------------------------------------------------------
    WHEN  9210.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÀÔ½Å°í ¼¼ºÎÈ­?
      MOVE 'CRDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »ý?
      SET TITLEBAR  'INT2' WITH <FS_F>.
    WHEN  9214.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÀÔ½Å°í »ý¼º ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'INT2' WITH W_CREATE.    W_STATUS = C_REQ_C.
    WHEN  9211.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÀÔ½Å°í º¯°æ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'INT2' WITH W_CHANGE.    W_STATUS = C_REQ_U.
    WHEN  9212.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÀÔ½Å°í Á¶È¸ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'INT2' WITH W_DISPLAY.   W_STATUS = C_REQ_D.
    WHEN  9213.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÀÔ½Å°í »óÅÂº¯°æ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'INT2' WITH W_OPEN.      W_STATUS = C_OPEN_C.
*>> LCK 2001.09.13 Ãß°¡.
*    WHEN 9214.     " ¼öÀÔ½Ã½ºÅÛ ¹ÝÀÔ½Å°í »ý¼º ÃÊ±âÈ­?
*
*       PERFORM   P2000_SET_ZTBLINR_TEM_INIT_SCR.
*       SET TITLEBAR 'INT2' WITH W_STATUS.  W_STATUS = C_REQ_C.
    WHEN 9215.
      PERFORM   P2000_SET_ZTBLINR_TEM_INIT_SCR.
      SET TITLEBAR  'INT3' WITH <FS_F>.
*-----------------------------------------------------------------------
* ¹ÝÃâ½Å?
*-----------------------------------------------------------------------
    WHEN  9220.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÃâ½Å°í ¼¼ºÎÈ­?
      SET TITLEBAR  'OUT1' WITH <FS_F>.
    WHEN  9221.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÃâ½Å°í »ý¼º ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'OUT2' WITH W_CREATE.    W_STATUS = C_REQ_C.
    WHEN  9222.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÃâ½Å°í º¯°æ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'OUT2' WITH W_CHANGE.    W_STATUS = C_REQ_U.
    WHEN  9223.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÃâ½Å°í Á¶È¸ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'OUT2' WITH W_DISPLAY.   W_STATUS = C_REQ_D.
    WHEN  9224.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÃâ½Å°í »óÅÂº¯°æ ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'OUT2' WITH W_OPEN.    W_STATUS = C_OPEN_C.
*-----------------------------------------------------------------------
* ½Ç¹ÝÃâ Ã³?
*-----------------------------------------------------------------------
    WHEN  9227.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÃâ½Å°í ¼¼ºÎÈ­?
      SET TITLEBAR  'OUT4'.                W_STATUS = C_OPEN_C.
    WHEN  9226.              " ¼öÀÔ½Ã½ºÅÛ ¹ÝÃâÃ³¸® ÃÊ±âÈ­?
      PERFORM   P2000_SET_BL_INIT_SCR.
      SET TITLEBAR  'OUT3'.                W_STATUS = C_OPEN_C.
*-----------------------------------------------------------------------
* ¸ð¼±°ü¸®.
*-----------------------------------------------------------------------
    WHEN  9910.              " ¸ð¼±°ü¸®?
      SET TITLEBAR  'MSTT1'.
      MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'NEWL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'NEXT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'PREV' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'POSI' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DELC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'MKBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'MKLO' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'CHGE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'MKAL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.

    WHEN  9911.              " ¸ð¼±°ü¸® DATA ¼±ÅÃ.
      MOVE 'MKBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'MKLO' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'MKAL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'NEXT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'PREV' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'POSI' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DELC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      SET TITLEBAR  'MSTT2'.
      W_STATUS = C_REQ_D.

    WHEN  9912.
      MOVE 'MKBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'MKLO' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'MKAL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'POSI' TO IT_EXCL-FCODE.    APPEND IT_EXCL.

      CASE  W_STATUS.
        WHEN  C_REQ_D.
          SET TITLEBAR  'MSTT3' WITH W_DISPLAY.
          MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
          MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
          MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        WHEN  C_REQ_C.
          SET TITLEBAR  'MSTT3' WITH W_CREATE.
          MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
          MOVE 'NEWL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
          MOVE 'CHGE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
          MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        WHEN  C_REQ_U.
          SET TITLEBAR  'MSTT3' WITH W_CHANGE.
          MOVE 'CHGE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        WHEN OTHERS.
          SET TITLEBAR 'MSTT3' WITH ' '.
      ENDCASE.
*-----------------------------------------------------------------------
* INVOICE»ý¼º½Ã PAYORD ¹®¼­¸¸µé±â. MKIM 020604
*-----------------------------------------------------------------------
    WHEN 141.              "  PAYORD ¹®?
      MOVE 'MIR2' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀüÇ¥Ãë¼Ò.
      MOVE 'MIRO' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀüÇ¥»ý¼º.
      MOVE 'BANK' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀºÇà.
      MOVE 'MK03' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀºÇà.
      MOVE 'PAYORD' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " PAYORD.

*-----------------------------------------------------------------------
* ÀûÇÏº¸Çè °ü¸®(BL ±âÁØ)
*-----------------------------------------------------------------------
    WHEN  '4100' .                       "  »ý¼º ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'ZINS' WITH W_CREATE.     W_STATUS = C_REQ_C.
    WHEN  '4200' .                      "  º¯°æ ÃÊ±âÈ­¸é.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'ZINS' WITH W_CHANGE.     W_STATUS = C_REQ_U.
    WHEN  '4300' .
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'ZINS' WITH W_DISPLAY.    W_STATUS = C_REQ_D.
    WHEN  '4400' .
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'ZINS' WITH W_OPEN.    W_STATUS = C_OPEN_C.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_STATUS_TCODE_DISABLE
*&---------------------------------------------------------------------*
FORM P2000_SET_STATUS_TCODE_DISABLE.

  IF ZTIMIMG00-BLSTYN NE 'X'.
    MOVE 'DCRP'  TO IT_EXCL-FCODE. APPEND IT_EXCL. ">.
    MOVE 'DCSD'  TO IT_EXCL-FCODE. APPEND IT_EXCL. ">.
    MOVE 'SDSD'  TO IT_EXCL-FCODE. APPEND IT_EXCL. ">.
    MOVE 'REAL'  TO IT_EXCL-FCODE. APPEND IT_EXCL. ">.
  ENDIF.

* 2002³â 06¿ù 21ÀÏ NSH Insert.
  IF ZTIMIMG00-BLCSTMD NE 'X'.
    MOVE 'POST' TO IT_EXCL-FCODE. APPEND IT_EXCL. ">.
  ENDIF.

  CASE SY-TCODE.
*>> Create
    WHEN 'ZIM21' OR 'ZIM26' OR 'ZIMI1'  OR 'ZIMO1'  OR 'ZIM35' OR
         'ZIM81' OR 'ZIM31' OR 'ZIM31L' OR 'ZIM35L' OR 'ZIMI6' OR
         'ZIM74' OR 'ZIMB1'.
      MOVE 'CRDC'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " »ý¼º.
      MOVE 'DELE'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " »èÁ¦.
      MOVE 'OPCL'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " OPEN CANC
      MOVE 'REVK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " FLATÃë¼Ò.
      MOVE 'EDIS'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " EDI SEND
      MOVE 'IVCR'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " INVOICE
      MOVE 'ZRIM18' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " CON ÇöÈ².
      MOVE 'MIR2'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀüÇ¥ CANCEL
      MOVE 'BLSD'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¼ÛºÎÅëº¸¼­.
      MOVE 'SDSD'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¼ÛºÎ Ãë¼Ò.
      MOVE 'REAL'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ½ÇÀÔÇ×Ãë¼Ò.
      MOVE 'LGPT'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ½ÇÀÔÇ×Ãë¼Ò.
      MOVE 'PRES'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¼öÀÔ½Å°íÇÊÁõ.
      MOVE 'LGIS'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ºÎº¸ÀÇ·Ú.
      MOVE 'HIST'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " HEAD CHAN
      MOVE 'FLAT'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Flat Data
      MOVE 'DTRS'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¿îÀÓ°ËÅä¼­.
      MOVE 'DHYK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÇÏ¿ª°ËÅä¼­.
      MOVE 'CKEK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " CHECK
      MOVE 'CIPT'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Print..
      MOVE 'CIST'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Invoice..
      MOVE 'PKST'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Packing..
      MOVE 'CPK'    TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Pack & Inv..
*>> º¯°æ.
    WHEN 'ZIM22' OR 'ZIM27' OR 'ZIMI2' OR 'ZIMO2'  OR 'ZIMI7'  OR
         'ZIM32' OR 'ZIM36' OR 'ZIM82' OR 'ZIM32L' OR 'ZIM36L' OR
         'ZIM75' OR 'ZIM62' OR 'ZIMB2'.
      MOVE 'FLAT'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">FLAT DATA.
      MOVE 'COPY'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">º¹»ç.
      MOVE 'CHDC'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">º¯°æ.
      MOVE 'CKEK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">CHECK
      MOVE 'OPCL'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">OPEN CANC
      MOVE 'REVK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">FLATÃë¼Ò.
      MOVE 'LOPU'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">ÀüÇ¥ CANCEL
      MOVE 'SDSD'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">¼ÛºÎ Ãë¼Ò.
      MOVE 'REAL'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">½ÇÀÔÇ×Ãë¼Ò.
      MOVE 'LGIS'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">EDI SEND.
      MOVE 'IMPREQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">¼öÀÔÀÇ·Ú.
      MOVE 'DTRS'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">¿îÀÓ°ËÅä¼­.
      MOVE 'DHYK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">ÇÏ¿ª°ËÅä¼­.
      MOVE 'CIPT'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Print..
      MOVE 'CIST'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Invoice..
      MOVE 'PKST'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Packing..
      MOVE 'CPK'    TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Pack & Inv..
*>> Á¶È¸.
    WHEN 'ZIM23' OR 'ZIM28' OR 'ZIMI3' OR 'ZIMO3'  OR 'ZIMI8'  OR
         'ZIM33' OR 'ZIM37' OR 'ZIM83' OR 'ZIM33L' OR 'ZIM37L' OR
         'ZIM76' OR 'ZIM63' OR 'ZIMB3'.
      MOVE 'COPY'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">º¹»ç.
      MOVE 'DISP'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">Á¶È¸.
      MOVE 'SAVE'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">ÀúÀå.
      MOVE 'CKEK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">CHECK
      MOVE 'LOPU'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">ÀüÇ¥ CANCEL
      MOVE 'LGIS'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">EDI SEND
      MOVE 'IMPREQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">¼öÀÔÀÇ·Ú.
      MOVE 'DTRS'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">¿îÀÓ°ËÅä¼­.
      MOVE 'DHYK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">ÇÏ¿ª°ËÅä¼­.
*>> È®Á¤.
    WHEN 'ZIM29' OR 'ZIMI9' OR 'ZIMO4' OR 'ZIMB4'.        ">È®Á¤.
      MOVE 'COPY'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">º¹»ç.
      MOVE 'OPDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">OPEN
      MOVE 'EDIS'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">EDI SEND
      MOVE 'OPCL'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">OPEN CANC
      MOVE 'CKEK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">OPEN CANC
      MOVE 'REVK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">FLATÃë¼Ò.
      MOVE 'LOPU'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">ÀüÇ¥ CANCEL
      MOVE 'DTRS'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">¿îÀÓ°ËÅä¼­.
      MOVE 'DHYK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">ÇÏ¿ª°ËÅä¼­.
    WHEN 'ZIMO6'.                                         ">½Ç¹ÝÃâ Ã³¸®.
      MOVE 'CRDC'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">»ý¼º.
      MOVE 'DELE'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">»èÁ¦.
      MOVE 'REVK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">Ãë¼Ò.
      MOVE 'CHDC'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">º¯°æ.
*     JSY20021112Ãß°¡.(ÇÑ¼ö¿ø)
      MOVE 'DTRS'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.    ">¿îÀÓ°ËÅä¼­.
      MOVE 'DHYK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.    ">ÇÏ¿ª°ËÅä¼­.
    WHEN 'ZIM34' OR 'ZIM38' OR 'ZIM34L' OR 'ZIM38L'.
      MOVE 'STCH'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">»óÅÂº¯°æ.
      MOVE 'MIRO'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">¹°´ë»ý¼º.
      MOVE 'MIR1'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">¹°´ëÃë¼Ò.
      MOVE 'MIR2'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">ÀüÇ¥CANCEL
      MOVE 'LOPU'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">ÀüÇ¥CANCEL
      MOVE 'IMPREQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   ">¼öÀÔÀÇ·Ú.
*     JSY20021112Ãß°¡.(ÇÑ¼ö¿ø)
      MOVE 'DTRS'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.    ">¿îÀÓ°ËÅä¼­.
      MOVE 'DHYK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.    ">ÇÏ¿ª°ËÅä¼­.
      MOVE 'RERQ'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.    ">°áÀç¿äÃ».
      MOVE 'CIPT'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Print..
      MOVE 'CIST'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Invoice..
      MOVE 'PKST'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Packing..
      MOVE 'CPK'    TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " Pack & Inv..
    WHEN 'ZIM223'.
*      MOVE 'CRDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " »ý¼º.
*      MOVE 'CHDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " º¯°æ..
*      MOVE 'DISP'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " Á¶È¸.
*      MOVE 'DCSD'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">½ÇÀÔÇ×.
*      MOVE 'DCRP'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
      MOVE 'DELE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'ME23'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'ZIM03' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'ZIM28' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'MK03'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'BENI'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'FWDR'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'TRUC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'ZIM73' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'ZIM93' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'HIST'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'MM03'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'MD04'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'MMBE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'MB51'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'ME2M'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'ME03'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'HIIT'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ".,
*      MOVE 'OTDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'COPY'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " º¹»ç.
*         MOVE 'CHDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " º¯°æ.
      MOVE 'OPCL'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " OPEN CANC
      MOVE 'REVK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " FLATÃë¼Ò.
      MOVE 'LOPU'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀüÇ¥ CANCEL
      MOVE 'SDSD'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¼ÛºÎÃë¼Ò.
      MOVE 'REAL'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">½ÇÀÔÇ×.
      MOVE 'BLCT'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">½ÇÀÔÇ×.
*     JSY20021112Ãß°¡.(ÇÑ¼ö¿ø)
      MOVE 'DTRS'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "¿îÀÓ°ËÅä¼­.
      MOVE 'DHYK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "ÇÏ¿ª°ËÅä¼­.

    WHEN 'ZIM221'.
      MOVE 'COPY'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " º¹»ç.
*         MOVE 'CHDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " º¯°æ.
      MOVE 'OPCL'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " OPEN CANC
      MOVE 'REVK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " FLATÃë¼Ò.
      MOVE 'LOPU'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀüÇ¥ CANCEL
      MOVE 'SDSD'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¼ÛºÎÃë¼Ò.
      MOVE 'REAL'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">½ÇÀÔÇ×.
      MOVE 'DCSD'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">½ÇÀÔÇ×.
*     JSY20021112Ãß°¡.(ÇÑ¼ö¿ø)
      MOVE 'DTRS'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "¿îÀÓ°ËÅä¼­.
      MOVE 'DHYK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "ÇÏ¿ª°ËÅä¼­.
    WHEN 'ZIM222'.
      MOVE 'COPY'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " º¹»ç.
*         MOVE 'CHDC'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " º¯°æ.
      MOVE 'OPCL'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " OPEN CANC
      MOVE 'REVK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " FLATÃë¼Ò.
      MOVE 'LOPU'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀüÇ¥ CANCEL
      MOVE 'REAL'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ½ÇÀÔÇ×Ãë¼Ò.
      MOVE 'DCRP'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">½ÇÀÔÇ×.
*     JSY20021112Ãß°¡.(ÇÑ¼ö¿ø)
      MOVE 'DTRS'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "¿îÀÓ°ËÅä¼­.
      MOVE 'DHYK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "ÇÏ¿ª°ËÅä¼­.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_STATUS_TCODE_DISABLE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_LG_DOC
*&---------------------------------------------------------------------*
FORM P1000_READ_LG_DOC.
  CLEAR : ZTLG.
  REFRESH : IT_ZSLGGOD, IT_ZSLGGOD_ORG.

  CALL FUNCTION 'ZIM_GET_LG_DOCUMENT'
       EXPORTING
            ZFBLNO         = ZTBL-ZFBLNO
            ZFLGSEQ        = ZSREQHD-ZFLGSEQ
       IMPORTING
            W_ZTLG         = ZTLG
       TABLES
            IT_ZSLGGOD     = IT_ZSLGGOD
            IT_ZSLGGOD_ORG = IT_ZSLGGOD_ORG
       EXCEPTIONS
            NOT_FOUND      = 4
            NOT_INPUT      = 8.

  CASE SY-SUBRC.
    WHEN 0.
      IF SY-TCODE EQ 'ZIM26'.
        PERFORM P2000_SET_BL_REQDOC_LOCK    USING    'U'.
        ZSREQHD-ZFBLNO = ZTBL-ZFBLNO.
        MESSAGE E050 WITH ZTBL-ZFBLNO.
      ENDIF.
    WHEN 4.
      IF SY-TCODE NE 'ZIM26'.
        MESSAGE E053 WITH ZTBL-ZFBLNO.
      ENDIF.
    WHEN 8.
      MESSAGE E039.
  ENDCASE.
* º¯°æ ÀÌ·ÂÀ» À§?
   *ZTLG = ZTLG.

  IF SY-TCODE EQ 'ZIM26'.
    SELECT SINGLE * FROM ZTLG
                    WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                    AND   ZFREQNO EQ ZVREQHD_ST-ZFREQNO.
    IF SY-SUBRC EQ 0.
      MESSAGE E515 WITH ZTBL-ZFBLNO ZVREQHD_ST-ZFREQNO.
    ENDIF.
  ENDIF.
* »ý¼ºÀÏ °æ¿ì, exit
  CHECK : SY-TCODE NE 'ZIM26'.

* L/G »óÅÂ CHECK.....
  PERFORM   P2000_LG_DOC_STATUS_CHECK.

* LOCK OBJECT
  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D ).
    PERFORM P2000_SET_LG_REQDOC_LOCK USING    'L'.
  ENDIF.

ENDFORM.                    " P1000_READ_LG_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTLG_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_ZTLG_MODIFY.
* ¹®¼­ CHECK
  PERFORM    P2000_LG_STATUS_CHECK.

*-----------------------------------------------------------------------
* ¼öÀÛ¾÷ °³¼³ÀÏ °æ¿ì, Status º¯?
*-----------------------------------------------------------------------
  IF W_STATUS EQ C_OPEN_C.
    IF W_NEW_DOCST NE 'N'.        " OPEN Ãë¼Ò°¡ ¾Æ´Ò °æ¿ì.
      ZTLG-ZFDOCST = 'O'.
    ENDIF.
    W_NEW_DOCST =  'O'.
  ENDIF.
  IF W_NEW_DOCST EQ 'N'.        " OPEN Ãë¼ÒÀÏ °æ¿ì.
    CLEAR : ZTLG-ZFLGINO, ZTLG-ZFLGIDT.
  ENDIF.
* »ý¼ºÀÏ °æ¿ì ´ÙÀ½ ¹øÈ£¸¦ Ã¤¹ø..
  IF W_STATUS EQ C_REQ_C.
    SELECT MAX( ZFLGSEQ ) INTO ZTLG-ZFLGSEQ FROM ZTLG
                          WHERE ZFBLNO EQ ZTBL-ZFBLNO.
    IF ZTLG-ZFLGSEQ IS INITIAL.
      ZTLG-ZFLGSEQ = '00001'.
    ELSE.
      ZTLG-ZFLGSEQ = ZTLG-ZFLGSEQ + 1.
    ENDIF.
  ENDIF.

* L/G DATA MODIFY
  CALL FUNCTION 'ZIM_LG_DOC_MODIFY'
       EXPORTING
            W_OK_CODE      = W_OK_CODE
            ZFBLNO         = ZTBL-ZFBLNO
            ZFLGSEQ        = ZTLG-ZFLGSEQ
            ZFSTATUS       = W_STATUS
            W_ZTLG         = ZTLG
            W_ZTLG_OLD     = *ZTLG
       TABLES
            IT_ZSLGGOD     = IT_ZSLGGOD
            IT_ZSLGGOD_OLD = IT_ZSLGGOD_ORG
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    IF W_OK_CODE EQ 'DELE'.
      MESSAGE  E313.
    ELSE.
      MESSAGE  E052.
    ENDIF.
  ELSE.
    IF W_OK_CODE EQ 'DELE'.
      MESSAGE  S314  WITH  ZTBL-ZFBLNO ZTLG-ZFLGSEQ.
    ELSE.
      MESSAGE  S051  WITH  ZTBL-ZFBLNO ZTLG-ZFLGSEQ.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_ZTLG_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_IN_TO_OUT
*&---------------------------------------------------------------------*
FORM P2000_DATA_IN_TO_OUT.
  CLEAR : ZTBLOUR.

  MOVE : ZTBLINR-MANDT       TO     ZTBLOUR-MANDT,    "Client
         ZTBL-ZFBLNO         TO     ZTBLOUR-ZFBLNO,   "B/L °ü¸®¹ø?
         ZTBLINR-ZFBTSEQ     TO     ZTBLOUR-ZFBTSEQ,  "º¸¼¼¿î¼Û ÀÏ·Ã?
         ZTBLINR-ZFINRNO     TO     ZTBLOUR-ZFOURNO,  "¹ÝÃâ½Å°í¹ø?
         ZTBLINR-ZFABNAR     TO     ZTBLOUR-ZFABNAR,  "º¸¼¼±¸¿ª CODE
         ZTBLINR-ZFBNARCD    TO     ZTBLOUR-ZFBNARCD, "º¸¼¼±¸¿ª ID
         ZTBLINR-ZFYR        TO     ZTBLOUR-ZFYR,     "¿¬?
         ZTBLINR-ZFSEQ       TO     ZTBLOUR-ZFSEQ,    "ÀÏ·Ã¹ø?
         '9'                 TO     ZTBLOUR-ZFEDINF,  "ÀüÀÚ¹®¼­±â?
         ZTBLINR-ZFINRC      TO     ZTBLOUR-ZFINRC,   "½Å°íÁö ¼¼?
         ZTBLINR-ZFINRCD     TO     ZTBLOUR-ZFINRCD,  "¼¼°üÀÇ ´ã´ç °ú?
         ZTBLINR-ZFPINS      TO     ZTBLOUR-ZFPOUS,   "ºÐÇÒ¹ÝÃâ Â÷?
         ZTBLINR-ZFPRIN      TO     ZTBLOUR-ZFPROU,   "ºÐÇÒ¹ÝÃâ±¸?
         SPACE               TO     ZTBLOUR-ZFPRDS,   "¹ÝÃâ±â°£ ¿¬Àå?
         ZTBLINR-ZFCYCFS     TO     ZTBLOUR-ZFCYCFS,  "CY/CFS ±¸?
         SPACE               TO     ZTBLOUR-ZFPRDS,   "±â°£¿¬Àå ±Ù°Å?
         ZTBLINR-ZFPKCN      TO     ZTBLOUR-ZFOUQN,   "ÃÑ¹ÝÃâ°³?
         ZTBLINR-ZFPKCNM     TO     ZTBLOUR-ZFOUQNM,  "ÃÑ¹ÝÃâ°³¼ö ´Ü?
         ZTBLINR-ZFINWT      TO     ZTBLOUR-ZFOUWT,   "¹ÝÃâÁß?
         ZTBLINR-ZFINTWT     TO     ZTBLOUR-ZFOUTWT,  "´©°è¹ÝÃâÁß?
         ZTBLINR-ZFINTQN     TO     ZTBLOUR-ZFOUTQN,  "´©°è¹ÝÃâ°³?
         ZTBLINR-ZFKG        TO     ZTBLOUR-ZFKG,     "¹«°Ô´Ü?
         ZTBLINR-ZFCT        TO     ZTBLOUR-ZFCT,     "°³¼ö´Ü?
         ZTBLINR-ZFPINS      TO     ZTBLOUR-ZFPOUS,   "ºÐÇÒ¹ÝÃâÂ÷?
*         SPACE               TO     ZTBLOUR-ZFADNO,   "Á¢¼ö/¿À·ù ÀüÀÚ?
         '61'                TO     ZTBLOUR-ZFOUTY,   "¹ÝÃâÀ¯?
         SPACE               TO     ZTBLOUR-ZFOUANO,  "¹ÝÃâ±Ù°Å¹ø?
*         ZTBLINR-ZFBLNO      TO     ZTBLOUR-ZFOTDT,   "¹ÝÃâÇã°¡?
         SY-DATUM            TO     ZTBLOUR-ZFOTDT,   "¹ÝÃâ?
         SY-UZEIT            TO     ZTBLOUR-ZFOUTM,   "¹ÝÃâ½Ã?
*         ZTBLINR-ZFBLNO      TO     ZTBLOUR-ZFAOUDT,  "½Ç¹ÝÃâ?
*         ZTBLINR-ZFBLNO      TO     ZTBLOUR-ZFAOUTM,  "½Ç¹ÝÃâ½Ã?
*         SY-UNAME            TO     ZTBLOUR-ZFRENM,   "ÀÎ¼ö?
*         ZTBLINR-ZFBLNO      TO     ZTBLOUR-ZFREDP,   "ÀÎ¼öºÎ?
         'N'                 TO     ZTBLOUR-ZFDOCST,  "DOC. Status
         'N'                 TO     ZTBLOUR-ZFEDIST,  "EDI Status
         'X'                 TO     ZTBLOUR-ZFEDICK,  "EDI Check
         SPACE               TO     ZTBLOUR-ZFDOCNO,  "ÀüÀÚ¹®¼­¹ø?
         SPACE               TO     ZTBLOUR-ZFDOCNOR, "ÀüÀÚ¹®¼­¹ø?
         SY-UNAME            TO     ZTBLOUR-ERNAM,    "Name of person w
         SY-DATUM            TO     ZTBLOUR-CDAT,     "Created on
         SY-UNAME            TO     ZTBLOUR-UNAM,     "Last changed by
         SY-DATUM            TO     ZTBLOUR-UDAT.     "Last changed on


ENDFORM.                    " P2000_DATA_IN_TO_OUT
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBLOUR_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_ZTBLOUR_MODIFY.
*-----------------------------------------------------------------------
* Data °ËÁõ Ãß°¡ÇÒ ºÎ?
*-----------------------------------------------------------------------

  IF W_OK_CODE EQ 'DELE'.
    PERFORM P2000_BLOUR_CHECK.
  ENDIF.

*  IF W_OK_CODE EQ 'DELE'.
*     DELETE ZTBLOUR.
*     IF SY-SUBRC NE  0.
*        MESSAGE  E313.
*     ELSE.
*        UPDATE  ZTBLINOU  SET : ZFBOUYN = ''
*                          WHERE ZFBLNO    EQ   ZTBLOUR-ZFBLNO
*                          AND   ZFBTSEQ   EQ   ZTBLOUR-ZFBTSEQ.
*        MESSAGE  S314  WITH  ZTBLOUR-ZFBLNO  ZTBLOUR-ZFBTSEQ.
*     ENDIF.
*  ELSE.
*-----------------------------------------------------------------------
* Data °ËÁõ Ãß°¡ÇÒ ºÎ?
*-----------------------------------------------------------------------
*     PERFORM   P2000_BLOUR_CHECK.
*-----------------------------------------------------------------------
* ¼öÀÛ¾÷ °³¼³ÀÏ °æ¿ì, Status º¯?
*-----------------------------------------------------------------------
  IF SY-TCODE EQ 'ZIMO6'.
    W_STATUS = 'U'.
  ENDIF.

  CALL FUNCTION 'ZIM_BLOUR_DOC_MODIFY'
       EXPORTING
            W_OK_CODE     = W_OK_CODE
            ZFBLNO        = ZTBLOUR-ZFBLNO
            ZFBTSEQ       = ZTBLOUR-ZFBTSEQ
            ZFSTATUS      = W_STATUS
            W_ZTBLOUR     = ZTBLOUR
            W_ZTBLOUR_OLD = *ZTBLOUR
       EXCEPTIONS
            ERROR_UPDATE  = 4
            NOT_MODIFY    = 8.

  IF SY-SUBRC EQ  4.
    MESSAGE  S058  WITH  ZTBLINR-ZFBLNO  ZTBLINR-ZFBTSEQ.
  ELSEIF SY-SUBRC EQ 8.
    MESSAGE  S211.
  ELSE.
    MESSAGE  S042  WITH  ZTBLINR-ZFBLNO  ZTBLINR-ZFBTSEQ.
  ENDIF.

*     IF SY-TCODE NE 'ZIMO6'.
*        IF W_STATUS EQ C_OPEN_C.
*           IF W_NEW_DOCST NE 'N'.        " OPEN Ãë¼Ò°¡ ¾Æ´Ò °æ¿ì.
*              ZTBLOUR-ZFDOCST = 'O'.
*           ZTREQHD-ZFOPNNO  = ZTREQST-ZFOPNNO.
*           ENDIF.
*           W_NEW_DOCST =  'O'.
*        ENDIF.
*        IF W_NEW_DOCST EQ 'N'.        " OPEN Ãë¼ÒÀÏ °æ¿ì.
*           ZTBLOUR-ZFDOCST = 'N'.
*        ENDIF.
*     ENDIF.

*     MOVE : SY-DATUM      TO    ZTBLOUR-UDAT,
*            SY-UNAME      TO    ZTBLOUR-UNAM.

*     CASE W_STATUS.
*        WHEN C_REQ_C.
*           IF SY-TCODE EQ 'ZIMO6'.
*              UPDATE ZTBLOUR.
*           ELSE.
*              UPDATE  ZTBLINOU  SET : ZFBOUYN = 'X'
*                                WHERE ZFBLNO    EQ   ZTBLOUR-ZFBLNO
*                                AND   ZFBTSEQ   EQ   ZTBLOUR-ZFBTSEQ.
*              IF SY-SUBRC NE  0.
*                 MESSAGE  E208.
*              ENDIF.
*              IF ZTBLOUR-ZFOUTY EQ '61'.   " ÀÌ°í¹Ý?
*                 SELECT MAX( ZFMOVENO ) INTO  ZTBLOUR-ZFMOVENO
*                                        FROM  ZTBLOUR
*                                        WHERE ZFYR  EQ  ZTBLOUR-ZFYR.
*                 ZTBLOUR-ZFMOVENO = ZTBLOUR-ZFMOVENO + 1.
*              ENDIF.
*              INSERT ZTBLOUR.
*           ENDIF.
*        WHEN OTHERS.
*           UPDATE ZTBLOUR.
*     ENDCASE.

*     IF SY-SUBRC NE  0.
*        MESSAGE  S058  WITH  ZTBLOUR-ZFBLNO  ZTBLOUR-ZFBTSEQ.
*     ELSE.
*        MESSAGE  S042  WITH  ZTBLOUR-ZFBLNO  ZTBLOUR-ZFBTSEQ.
*     ENDIF.
*  ENDIF.
ENDFORM.                    " P3000_ZTBLOUR_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBLUG_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_ZTBLUG_MODIFY.

  MOVE SY-UNAME TO ZTBLUG-UNAM.
  MOVE SY-DATUM TO ZTBLUG-UDAT.
  CASE W_STATUS.
    WHEN  'C'.
      MOVE SY-UNAME TO ZTBLUG-ERNAM.
      MOVE SY-DATUM TO ZTBLUG-CDAT.
      INSERT ZTBLUG.
    WHEN  'U'.
      DELETE FROM ZTBLUGC
       WHERE ZFHBLNO = ZTBLUG-ZFHBLNO.
      UPDATE ZTBLUG.
  ENDCASE.
  IF SY-SUBRC NE  0.
    MESSAGE  E700.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSBLUGC.
    CLEAR : ZTBLUGC.
    MOVE-CORRESPONDING IT_ZSBLUGC   TO ZTBLUGC.
    MOVE ZTBLUG-ZFHBLNO             TO ZTBLUGC-ZFHBLNO.
    INSERT   ZTBLUGC.
    IF SY-SUBRC NE 0.
      MESSAGE  E700.
      EXIT.
    ENDIF.
  ENDLOOP.

  MESSAGE  S701.

ENDFORM.                    " P3000_ZTBLUG_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_SCR_MODE_SET
*&---------------------------------------------------------------------*
FORM P2000_SCR_MODE_SET.

  " Import IMG DATA GET.
  CLEAR : ZTIMIMGTX, ZTIMIMG00.
  SELECT SINGLE * FROM ZTIMIMGTX WHERE BUKRS  EQ  ZTBL-BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.

  LOOP AT SCREEN.
    CASE W_STATUS.
      WHEN C_REQ_C OR C_REQ_U.              " »ý¼º, º¯?
        IF SCREEN-GROUP1 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
        IF W_STATUS EQ C_REQ_U.
          IF SCREEN-GROUP2 EQ 'CR'.
            IF SCREEN-NAME(15) = 'ZSMSCST-ZFAPRTC' AND
               W_NEW_STATUS  = C_REQ_C.
              SCREEN-INPUT = '1'.
            ELSEIF SCREEN-NAME(15) = 'ZSMSCST-ZFAPRTC' AND
                   IT_ZSMSCST-ZFAPRTC  IS  INITIAL.
              SCREEN-INPUT = '1'.
            ELSE.
              SCREEN-INPUT = '0'.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN  C_ADD_U OR C_BL_SEND.                     " Ãß°¡º¯°æ.
        IF SCREEN-GROUP2 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_REQ_D OR C_ADD_D OR C_OPEN_D.  " Á¶È¸, Ãß°¡Á¶È¸, È®Á¤Á¶?
        IF SCREEN-GROUP1 = 'I'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      WHEN C_OPEN_C OR C_BL_REAL.                    " È®Á¤.
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_OPEN_U.                         " È®Á¤º¯°æ.
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_INSU_I.                        " º¸Çè°ü?
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          IF SCREEN-GROUP1 = 'I'.
            SCREEN-INPUT   = '1'.
          ELSE.
            SCREEN-INPUT   = '0'.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

*>> ÇÑ¼ö¿ø WEB¿ëÀ¸·Î »ý¼º½Ã ¿­±â.
    IF SY-DYNNR EQ '0102' AND W_STATUS EQ C_REQ_C
       AND SCREEN-NAME EQ 'ZTBL-ZFRMK3'.
      SCREEN-INPUT   = '1'.
    ENDIF.
    IF ZTIMIMG00-BLSTYN NE 'X'.
      IF SY-DYNNR EQ '0102'.
        IF SCREEN-NAME EQ 'ZTBL-ZFRETA'   OR
           SCREEN-NAME EQ 'ZTBL-ZFBLSDT'  OR
           SCREEN-NAME EQ 'ZTBL-ZFBLSDP'.
          SCREEN-INPUT  =  '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF SY-DYNNR EQ '0102' AND W_STATUS EQ C_REQ_C
       AND SCREEN-NAME EQ 'ZTBL-BUKRS'.
      IF ZTBL-BUKRS IS INITIAL.
        SCREEN-INPUT = '1'.
      ELSE.
        SCREEN-INPUT = '0'.
      ENDIF.
    ENDIF.
*>> Inbound delivery »ç¿ë¿©ºÎ.
    IF SY-DYNNR EQ '0112' AND  SCREEN-NAME EQ  'PB_2'.
      IF ZTIMIMG00-ZFIBYN EQ 'X'.
        IF ZTIMIMG00-ZFIBBD EQ 'ADA' AND
           ZTIMIMG00-BLSTYN EQ 'X'.
          IF SY-TCODE EQ 'ZIM222'.
            SCREEN-INPUT = '1'.
          ELSE.
            SCREEN-INPUT = 'O'.
          ENDIF.
          SCREEN-INVISIBLE = '0'.
        ENDIF.
      ELSE.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
    ENDIF.
*>> °¡ÀüÇ¥ »ç¿ëÇÒ °æ¿ì Ãë¼Ò½Ã ÀüÇ¥Á¶È¸ BUTTON INVISIBLE(NHJ)
    IF SY-DYNNR EQ '3519'.
      IF ZTIMIMG00-CSREALYN EQ 'X' AND ZTCIVHD-ZFIVST EQ 'N'.
        IF ( SCREEN-NAME(4)  EQ  'PB_1' OR
             SCREEN-NAME(4)  EQ  'PB_2' OR
             SCREEN-NAME(4)  EQ  'PB_3' ).
          SCREEN-INPUT = 'O'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ELSEIF ZTIMIMG00-CSREALYN EQ 'X' AND ZTCIVHD-ZFIVST EQ 'Y'.
        IF SCREEN-NAME(4)  EQ  'PB_3'.
          SCREEN-INPUT  =  '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
    ENDIF.

*>> º¸Çè·á ºñ¿ë ¹ß»ý DOCUMENT DATA °¡ ¾øÀ¸¸é È­¸é»ó¿¡¼­ INVISIBLE(NHJ).
    IF SY-DYNNR EQ '4102'.
      IF ZTINS-BELNR IS INITIAL.
        IF ( SCREEN-NAME(11) EQ 'ZTINSB-BUKRS' OR
             SCREEN-NAME(11) EQ 'ZTINSB-BELNR' OR
             SCREEN-NAME(11) EQ 'ZTINSB-GJAHR' ).
          SCREEN-INPUT     =  '0'.
          SCREEN-INVISIBLE =  '1'.
        ENDIF.
      ELSE.
        SCREEN-INVISIBLE = '0'.
      ENDIF.
    ENDIF.
*>> EDI »ç¿ë¿©ºÎ¿¡ µû¶ó¼­ TAB È­¸é DISPLAY,(NHJ)
    IF ZTIMIMGTX-ZFEDIYN IS INITIAL.
      IF ( SY-DYNNR     EQ '4101' OR SY-DYNNR     EQ '4501'  ) AND
         ( SCREEN-NAME  EQ 'ITM2' OR SCREEN-NAME  EQ 'ITM4'  OR
           SCREEN-NAME  EQ 'ITM3' ).
        SCREEN-INVISIBLE  =  '1'.
      ENDIF.
    ELSEIF ZTIMIMGTX-APPCIP IS INITIAL.
      IF ( SY-DYNNR     EQ '4101' OR SY-DYNNR     EQ '4501' )  AND
         ( SCREEN-NAME  EQ 'ITM2' OR SCREEN-NAME  EQ 'ITM4'  OR
           SCREEN-NAME  EQ 'ITM3' ).
        SCREEN-INVISIBLE  =  '1'.
      ENDIF.
    ENDIF.

*>> Use of freight auto calc.->Screen Invisible
    IF SY-DYNNR EQ '0101'.
      IF ZTIMIMG00-ZFPSMS NE '1' AND ZTIMIMG00-BLCSTMD IS INITIAL.
        IF SCREEN-NAME(4) = 'ITM3'   OR
           SCREEN-NAME(4) = 'ITM5'.
          SCREEN-INVISIBLE  =  '1'.
        ENDIF.
      ELSE.
        IF ZTBL-INCO1 EQ 'EXW' OR ZTBL-INCO1 EQ 'FCA' OR
           ZTBL-INCO1 EQ 'FAS' OR ZTBL-INCO1 EQ 'FOB'.
          IF SY-TCODE EQ 'ZIM223'.
            IF SCREEN-NAME EQ 'ITM3' OR SCREEN-NAME EQ 'ITM5'.
              SCREEN-INVISIBLE  =  '0'.
            ELSE.
              IF SCREEN-NAME(7) NE 'ZSBLCST'.
                IF SCREEN-NAME(3) NE 'ITM'.
                  SCREEN-INPUT      =  '0'.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            SCREEN-INVISIBLE  =  '0'.
          ENDIF.
        ELSE.
          IF SCREEN-NAME EQ 'ITM3' OR SCREEN-NAME EQ 'ITM5'.
*             SCREEN-INVISIBLE  =  '1'.
          ENDIF.
        ENDIF.
      ENDIF.

      IF SCREEN-NAME(4) EQ 'ITM4'.    " Container Screen Invisible
        SCREEN-INVISIBLE = '1'.
      ENDIF.

*> »ó¾÷¼ÛÀå Tab.. --> ÀÖÀ» °æ¿ì¸¸ È°¼ºÈ­.
      IF SCREEN-NAME EQ 'ITM7'.
        DESCRIBE TABLE IT_ZSCIVIT LINES W_LINE2.
        IF W_LINE2 EQ 0.
          SCREEN-INVISIBLE  =  '1'.
        ENDIF.
      ENDIF.

*> Åë°ü¿äÃ» Tab.. --> ÀÖÀ» °æ¿ì¸¸ È°¼ºÈ­.
      IF SCREEN-NAME EQ 'ITM2'.
        DESCRIBE TABLE IT_ZSIVIT LINES W_LINE2.
        IF W_LINE2 EQ 0.
          SCREEN-INVISIBLE  =  '1'.
        ENDIF.
      ENDIF.

*> º¸¼¼¿î¼Û Tab.. --> ÀÖÀ» °æ¿ì¸¸ È°¼ºÈ­.
      IF SCREEN-NAME EQ 'ITM4' AND W_STATUS EQ 'D'.
        DESCRIBE TABLE IT_ZSBLCON LINES W_LINE2.
        IF W_LINE2 EQ 0.
          SCREEN-INVISIBLE  =  '1'.
        ENDIF.
      ENDIF.
*> FCLÀÏ °æ¿ì¸¸ CONTAINER TAB È°¼ºÈ­.
      IF ZTBL-ZFSHTY NE 'F'.
        IF SCREEN-NAME EQ 'ITM4'.
          SCREEN-INVISIBLE  =  '1'.
        ENDIF.
      ENDIF.

      IF ZTIMIMG00-BLSTYN EQ 'X'.
        IF SY-TCODE EQ 'ZIM21' OR SY-TCODE EQ 'ZIM22' OR
           SY-TCODE EQ 'ZIM221'.
          IF SCREEN-NAME EQ 'ITM2' OR
             SCREEN-NAME EQ 'ITM3' OR
             SCREEN-NAME EQ 'ITM4' OR
             SCREEN-NAME EQ 'ITM5' OR
             SCREEN-NAME EQ 'ITM7'.
            SCREEN-INVISIBLE  =  '1'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '0102' OR SY-DYNNR EQ '0112' OR SY-DYNNR EQ '0105'.
      IF SY-TCODE EQ'ZIM223'.
        SCREEN-INPUT  =  '0'.
      ENDIF.
    ENDIF.

*>>È¯À² ÀÔ·ÂÈ­¸é.
    IF SY-DYNNR EQ '0109'.
      IF ZTIMIMG00-ZFEXFIX EQ 'X'.
        CASE ZTIMIMG00-ZFEXMTD.
          WHEN 'B' OR 'I'.
            IF SCREEN-NAME     NE 'ZTBL-ZFBLDT' AND
               SCREEN-NAME(04) EQ 'ZTBL'.
              SCREEN-INVISIBLE  =  '1'.
              SCREEN-INPUT      =  '0'.
            ENDIF.
          WHEN 'A'.
            IF SCREEN-NAME     NE 'ZTBL-ZFETA' AND
               SCREEN-NAME(04) EQ 'ZTBL'.
              SCREEN-INVISIBLE  =  '1'.
              SCREEN-INPUT      =  '0'.
            ENDIF.
          WHEN 'R'.
            IF SCREEN-NAME     NE 'ZTBL-ZFRETA' AND
               SCREEN-NAME(04) EQ 'ZTBL'.
              SCREEN-INVISIBLE  =  '1'.
              SCREEN-INPUT      =  '0'.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ELSE.
        IF SCREEN-NAME     NE 'ZTBL-ZFBLDT' AND
           SCREEN-NAME(04) EQ 'ZTBL'.
          SCREEN-INVISIBLE  =  '1'.
          SCREEN-INPUT      =  '0'.
        ENDIF.
      ENDIF.
    ENDIF.

* ÇÏ¿ªºñ¿ë.
    IF SY-DYNNR EQ '0811'.
      IF ZTIMIMG00-ZFPSMS NE '1'.
        IF SCREEN-NAME(4) = 'ITM2'.
          SCREEN-INVISIBLE  =  '1'.
        ENDIF.
      ELSE.
        SCREEN-INVISIBLE  =  '0'.
      ENDIF.
    ENDIF.
* Åë°üºñ¿ë.
    IF SY-DYNNR EQ '7410'.
      IF ZTIMIMG00-ZFPSMS NE '1'.
        IF SCREEN-NAME(4) = 'CST'.
          SCREEN-INVISIBLE  =  '1'.
        ENDIF.
      ELSE.
        SCREEN-INVISIBLE  =  '0'.
      ENDIF.
    ENDIF.
*>> Åë°ü(ÀÔ°í)¿äÃ»½Ã LOCAL DATA ÀÎ°æ¿ì ºÒÇÊ¿ä Ç×¸ñ INVISIBLE.
    IF SY-DYNNR EQ '3112'.
      IF ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU'.
        IF SCREEN-NAME(12) = 'ZTIV-ZFIVAMC' OR
           SCREEN-NAME(12) = 'ZTIV-ZFIVAMT' OR
           SCREEN-NAME(10) = 'ZTIV-ZFKRW'   OR
           SCREEN-NAME(12) = 'ZTIV-ZFIVAMK' OR
           SCREEN-NAME(12) = 'ZTIV-ZFPKCHG' OR
           SCREEN-NAME(12) = 'ZTIV-ZFHDCHG' OR
           SCREEN-NAME(11) = 'ZTIV-ZFEXRT'  OR
           SCREEN-NAME(10) = 'ZTIV-ZFCUT'   OR
           SCREEN-NAME(11) = 'ZTIV-ZFPONC'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ELSE.
        SCREEN-INVISIBLE = '0'.
      ENDIF.
    ENDIF.
*>> Åë°ü¿äÃ»½Ã IMG »çÇ×¿¡ µû¶ó¼­ °ü¼¼»ç VISIBLE À¯¹«(2001.07.10 NHJ).
    IF SY-DYNNR EQ '3112'.
      IF SCREEN-NAME(10)  EQ  'ZTIV-ZFCUT'.
        IF ZTIMIMG00-ZFIMPATH NE '3'.
          SCREEN-INPUT  =  '0'.
          SCREEN-INVISIBLE  =  '1'.
        ELSE.
          SCREEN-INVISIBLE  =  '0'.
        ENDIF.
      ENDIF.

*>> ¹ÌÅë°ü »óÅÂÀÏ °æ¿ì.
      IF ZTIV-ZFCLCD EQ 'X'.
        IF SCREEN-NAME EQ 'ZTIV-ZFPONC'       OR
           SCREEN-NAME EQ 'ZTIV-ZFCUT'        OR
           SCREEN-NAME EQ 'ZSIMIMG08-ZFCDNM'.
          SCREEN-INPUT  =  '0'.
          SCREEN-INVISIBLE  =  '1'.
        ENDIF.
      ENDIF.
    ENDIF.
*>> PAYMENT NOTICE¿¡¼­ USANCE, GSMÀÎ °æ¿ìÀÏ °æ¿ì¸¸ Ç×¸ñ DISPLAY.
    IF SY-DYNNR EQ '8210'.
      IF ZTPMTHD-ZFBLNO IS INITIAL AND ZTPMTHD-ZFHBLNO IS INITIAL.
        IF SCREEN-NAME EQ 'ZTPMTHD-ZFBLNO' OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFHBLNO'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
*>>ÀüÀÚ¹®¼­°ü¸®¹øÈ£....
      IF SCREEN-NAME EQ 'ZTPMTHD-ZFDOCNOR' AND
         ZTPMTHD-ZFDOCNOR IS INITIAL.
        SCREEN-INPUT     = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
*>> A/P ¹ÝÁ¦ÀüÇ¥...
      IF ZTPMTHD-GJAHR IS INITIAL OR ZTPMTHD-BELNR IS INITIAL.
        IF SCREEN-NAME EQ 'ZTPMTHD-GJAHR' OR
           SCREEN-NAME EQ 'ZTPMTHD-BELNR'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
*>> Banker's Usance ÀÌÀÚÀüÇ¥..
      IF ZTPMTHD-GJAHR_TR IS INITIAL OR ZTPMTHD-BELNR_TR IS INITIAL.
        IF SCREEN-NAME EQ 'ZTPMTHD-GJAHR_TR' OR
           SCREEN-NAME EQ 'ZTPMTHD-BELNR_TR'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
*>> ¾÷¹«°¡ºÒ ¹ÝÁ¦ÀüÇ¥...
      IF ZTPMTHD-ZFCLNO IS INITIAL OR ZTPMTHD-ZFCLYR IS INITIAL.
        IF SCREEN-NAME EQ 'ZTPMTHD-ZFCLNO' OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFCLYR'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.

      IF ZTPMTHD-ZFLCKN EQ '8'.    ">LOCAL ÀÏ °æ¿ì.
        IF ZTPMTHD-ZFDHDOC IS INITIAL.
          ZTPMTHD-ZFDHDOC = 'LDANTC'.
        ENDIF.
        IF SCREEN-NAME EQ 'ZTPMTHD-ZFDHDOC'.
          SCREEN-INPUT     = '0'.
        ENDIF.
        IF SCREEN-NAME EQ 'ZTPMTHD-ZFBLNO'   OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFHBLNO'  OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFBLNO'   OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFBKCH'   OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFBKCHC'  OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFUSITC'  OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFUSIRP'  OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFUSIT'   OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFUSIRP'  OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFUSIRDT' OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFUSITR'  OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFDSDT'   OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFPWDT'   OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFSTDT'   OR
           SCREEN-NAME EQ 'ZTPMTHD-ZFBKCHP'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ELSE.
        CASE ZTPMTHD-ZFDHDOC.
          WHEN 'LDANTC'.    ">³»±¹½Å¿ëÀå ¾îÀ½µµÂøÅëº¸¼­.
            IF ZTPMTHD-ZFDHDOC IS INITIAL.
              ZTPMTHD-ZFDHDOC = 'LDANTC'.
            ENDIF.
            IF SCREEN-NAME EQ 'ZTPMTHD-ZFDHDOC'.
              SCREEN-INPUT     = '0'.
            ENDIF.
            IF SCREEN-NAME EQ 'ZTPMTHD-ZFBLNO'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFHBLNO'  OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFBLNO'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFBKCH'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFBKCHC'  OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFUSITC'  OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFUSIRP'  OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFUSIT'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFUSIRDT' OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFUSITR'  OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFDSDT'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFPWDT'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFSTDT'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFISNO'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFBKCHP'.
              SCREEN-INPUT     = '0'.
              SCREEN-INVISIBLE = '1'.
            ENDIF.

          WHEN 'DOANTC'.    ">¼±Àû¼­·ù µµÂøÅëº¸¼­.
            IF SCREEN-NAME EQ 'ZTPMTHD-ZFUSITC'  OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFUSIT'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFUSIRP'  OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFUSITR'  OR
               SCREEN-NAME EQ 'ZTPMTHD-NEGODT'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFSTDT'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFPWDT'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFMOA'    OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFKRW'    OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFDSDT'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFISNO'.
              SCREEN-INPUT     = '0'.
              SCREEN-INVISIBLE = '1'.
            ENDIF.
          WHEN 'DISCHG'.    ">¼öÀÔ¾îÀ½ DISCOUNT ³»¿ª Åëº¸¼­.
            IF SCREEN-NAME EQ 'ZTPMTHD-ZFMOA'    OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFKRW'    OR
               SCREEN-NAME EQ 'ZTPMTHD-NEGODT'   OR
               SCREEN-NAME EQ 'ZTPMTHD-ZFISNO'.
              SCREEN-INPUT     = '0'.
              SCREEN-INVISIBLE = '1'.
            ENDIF.

          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ENDIF.
*>> PAYMENT NOTICE¿¡¼­ USANCEÀÏ °æ¿ì ¿ù/ÀÏ È¸¼ö COLUMN DISPLAY MODE·Î.
    IF SY-DYNNR = '8210' AND  SCREEN-NAME(14) = 'ZTPMTHD-ARHYTM' AND
       ( ZTPMTHD-ZFLCKN = '2'  OR  ZTPMTHD-ZFLCKN = '3' ).
      CLEAR  ZTPMTHD-ARHYTM.
      SCREEN-INPUT = '0'.
    ENDIF.
* MKIM 2001.05.17

    IF SY-DYNNR = '0102'.
      IF ZTBL-ZFPOYN EQ 'Y'.
        IF SCREEN-NAME EQ 'ZTBL-KOSTL'    OR
           SCREEN-NAME EQ 'ZTBL-PS_POSID' OR
           SCREEN-NAME EQ 'ZTBL-ZFUPT'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ELSEIF NOT ZTBL-ZFREBELN IS INITIAL.
        IF SCREEN-NAME EQ 'ZTBL-KOSTL'    OR
           SCREEN-NAME EQ 'ZTBL-PS_POSID' OR
           SCREEN-NAME EQ 'ZTBL-ZFUPT'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ELSE.
        IF SCREEN-NAME EQ 'ZTBL-PS_POSID'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
*>> SHIP-BACKÀÏ °æ¿ì.... SD ¹®¼­ ÀÔ·ÂÅä·Ï ¼öÁ¤....
      IF ZTBL-ZFPOTY EQ 'H'.
        IF SCREEN-NAME EQ 'ZTBL-EKORG'    OR
           SCREEN-NAME EQ 'ZTBL-EKGRP'    OR
           SCREEN-NAME EQ 'ZTBL-ZFREBELN' OR
           SCREEN-NAME EQ 'ZTBL-ZFSHNO'   OR
           SCREEN-NAME EQ 'ZTBL-ZFOPNNO'  OR
           SCREEN-NAME EQ 'ZTBL-LIFNR'    OR
           SCREEN-NAME EQ 'ZTBL-ZFBENI'   OR
           SCREEN-NAME EQ 'W_VENDOR_NM'   OR
           SCREEN-NAME EQ 'W_ZFBENI_NM'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ELSE.
        IF SCREEN-NAME EQ 'ZTBL-VBELN' OR
           SCREEN-NAME EQ 'ZTBL-VKORG' OR
           SCREEN-NAME EQ 'ZTBL-VTWEG' OR
           SCREEN-NAME EQ 'ZTBL-SPART' OR
           SCREEN-NAME EQ 'ZTBL-KUNNR' OR
           SCREEN-NAME EQ 'ZTBL-KUNWE' OR
           SCREEN-NAME EQ 'W_KUNNR_NM' OR
           SCREEN-NAME EQ 'W_KUNWE_NM'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.

      IF SY-TCODE NE 'ZIM23'.
        IF ZTBL-ZFRPTTY NE 'B'.
          IF SCREEN-NAME EQ 'ZTBL-ZFBNDT'   OR
             SCREEN-NAME EQ 'ZTBL-ZFGMNO'   OR
             SCREEN-NAME EQ 'ZTBL-ZFMSN'    OR
             SCREEN-NAME EQ 'ZTBL-ZFHSN'    OR
             SCREEN-NAME EQ 'ZTBL-ZFBNARCD'.
            SCREEN-INPUT = 0.
            SCREEN-INVISIBLE = '1'.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ZTIMIMG00-BLSTYN EQ 'X'.
        IF SY-TCODE EQ 'ZIM222'.
          IF SCREEN-NAME EQ 'ZTBL-ZFRPTTY'.
            IF ZTBL-ZFBLST EQ '4' OR ZTBL-ZFBLST EQ 'P'.
              SCREEN-INPUT = 0.
            ENDIF.
          ENDIF.
        ELSE.
          IF SY-TCODE NE 'ZIM23'.
            IF SCREEN-NAME EQ 'ZTBL-ZFRPTTY' AND ZTBL-ZFBLST NE 'N'.
              SCREEN-INPUT = '0'.
              SCREEN-INVISIBLE = '1'.
            ENDIF.
          ENDIF.
          IF ZTBL-ZFPOYN EQ 'N'.
            IF SCREEN-NAME EQ 'ZTBL-ZFREBELN' OR
               SCREEN-NAME EQ 'ZTBL-ZFSHNO'   OR
               SCREEN-NAME EQ 'ZTBL-ZFOPNNO'.
              SCREEN-INPUT = '0'.
              SCREEN-INVISIBLE = '1'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

*>> BL »óÀÇ ¹«È¯Ç° ¼ö·ÉÀÎ Ç×¸ñ DISPLAY OR UNDISPLAY.
      IF SCREEN-NAME = 'ZTBL-ZFRCVER' AND ZTBL-ZFPOYN = 'Y'.
        CLEAR  ZTBL-ZFRCVER.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
*>È¯À²...
      IF ZTIMIMG00-ZFEXMTD EQ 'I'.
        IF SCREEN-NAME EQ 'ZTBL-ZFEXRT' OR
           SCREEN-NAME EQ 'ZTBL-FFACT'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
*>> ¸ð¼±ÄÚµå...
      IF SCREEN-NAME EQ 'ZTBL-ZFMSNO' AND ZTIMIMG00-ZFMSYN NE 'X'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '0102' OR SY-DYNNR EQ '0131' OR
       SY-DYNNR EQ '0135'.
      IF ZTIMIMG11-ZFVNCT IS INITIAL.
        IF SCREEN-NAME   EQ 'ZTBL-ZFHAYEK' OR
           SCREEN-NAME   EQ 'W_HAYEK_NM'.
          SCREEN-INVISIBLE  =  '1'.
          SCREEN-INPUT      =  '0'.
        ENDIF.
      ENDIF.
    ENDIF.

*>> BL »óÀÇ WEIGHT & CHARGE ¿¡¼­ CURRENCY KRW ÀÌ¸é ºñ¿ë¿øÈ­±Ý¾× DISPLAY.
    IF SY-DYNNR EQ '0104' AND IT_ZSBLCST-WAERS EQ 'KRW'.
      IF SCREEN-NAME(15)  EQ 'ZSBLCST-ZFCKAMT' OR
         SCREEN-NAME(14)  EQ 'ZSBLCST-ZFEXRT'.
        SCREEN-INPUT = '0'.
      ENDIF.
    ENDIF.
    IF SY-DYNNR EQ '0104' AND NOT IT_ZSBLCST-ZFCSCD IS INITIAL.
      IF SCREEN-NAME(14)  EQ 'ZSBLCST-ZFCSCD'.
        SCREEN-INPUT = '0'.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '0106' AND NOT IT_ZSBLCST1-ZFCSCD IS INITIAL.
      IF SCREEN-NAME(14)  EQ 'ZSBLCST-ZFCSCD'.
        SCREEN-INPUT = '0'.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '0813' AND NOT IT_ZSCGCST-ZFCSCD IS INITIAL.
      IF SCREEN-NAME(14)  EQ 'ZSCGCST-ZFCSCD'.
        SCREEN-INPUT = '0'.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '0104' AND SCREEN-NAME(13)   EQ 'ZSBLCST-ZFVAT'
                         AND IT_ZSBLCST-WAERS  NE 'KRW'.
      CLEAR  IT_ZSBLCST-ZFVAT.
      SCREEN-INPUT = '0'.
    ENDIF.

    IF SY-DYNNR EQ '3510'.
      IF ZTCIVHD-ZFREQTY EQ 'LO' OR ZTCIVHD-ZFREQTY EQ 'PU'.
        IF SCREEN-NAME EQ 'ZTCIVHD-ZFCIVNO'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.

      " Use of Business Area
      IF ZTIMIMG00-ZFBALK NE 'X'.
        IF SCREEN-NAME EQ 'ZTCIVHD-GSBER'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
      " Use of Business Place
      IF ZTIMIMG00-ZFBPLK NE 'X'.
        IF SCREEN-NAME EQ 'ZTCIVHD-BUPLA'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.

*> È¯À² Á¶Á¤ ¿©ºÎ...
      IF ZTIMIMG00-ZFEXFIX EQ 'X' AND
         ( SCREEN-NAME EQ 'ZTCIVHD-ZFEXRT' OR
           SCREEN-NAME EQ 'ZTCIVHD-FFACT' ).
        CASE ZTIMIMG00-ZFEXMTD.
          WHEN 'B' OR 'R' OR 'A'.
            SCREEN-INPUT = '0'.
          WHEN 'G'.
            IF ZTCIVHD-ZFPRPYN EQ 'N'.
              SCREEN-INPUT = '0'.
            ELSE.
              LOOP AT IT_ZSCIVIT.
                CLEAR : W_CNT.
                SELECT COUNT( * ) INTO W_CNT FROM ZTCIVIT
                       WHERE  ZFREQNO  EQ  IT_ZSCIVIT-ZFREQNO
                       AND    ZFITMNO  EQ  IT_ZSCIVIT-ZFITMNO
                       AND    ZFPRPYN  EQ  'Y'.
                IF W_CNT GT 0.
                  SCREEN-INPUT = '0'.
                ENDIF.
              ENDLOOP.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.

*>> È¯À² Conversion Factor ¼öÁ¤¸øÇÏµµ?
      IF SCREEN-NAME(13) EQ 'ZTCIVHD-FFACT'.
        SCREEN-INPUT = '0'.
      ENDIF.
    ENDIF.

*>> À¯È¯ÀÏ °æ¿ì, È¸»çÄÚµå ¸·À½.
    IF SY-DYNNR EQ '0102' AND ZTBL-ZFPOYN EQ 'Y' AND
       SCREEN-NAME(10) EQ 'ZTBL-BUKRS' AND
       NOT ZTBL-BUKRS IS INITIAL.
      SCREEN-INPUT   = '0'.
    ENDIF.

*>> ¼±±Þ±Ý ÀüÇ¥ÀÏ °æ¿ì(commercial invoice).
    IF SY-DYNNR EQ '3510' AND ZTCIVHD-ZFPRPYN EQ 'N' AND
       SCREEN-NAME(14) EQ 'ZTCIVHD-ZFPRTE'.
      SCREEN-INPUT   = '0'.
    ENDIF.
    IF SY-DYNNR EQ '3510'.
      IF NOT ( ZTCIVHD-ZFREQTY EQ 'LO' OR ZTCIVHD-ZFREQTY EQ 'PU' ).
        IF SCREEN-NAME(15) EQ 'ZTCIVHD-ZFREQTY'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '3510'.
      IF SCREEN-NAME(09) EQ 'MAINAREA1' OR
         SCREEN-NAME(07) EQ 'TC_3519'.
        READ TABLE  IT_ZSCIVHST  INDEX   1.
        IF SY-SUBRC NE 0.
          SCREEN-INVISIBLE   = '1'.
        ENDIF.
      ENDIF.
    ENDIF.
*>> ÀÔ°í¿äÃ» »ý¼º ÃÊ±âÈ­¸é.
    IF SY-DYNNR EQ '3101'.
      IF ( ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P' ).
        IF SCREEN-NAME EQ 'ZSIV-ZFCSTYN'.
          SCREEN-INVISIBLE   = '1'.
          SCREEN-INPUT       = '0'.
        ENDIF.
      ENDIF.
      IF SCREEN-NAME EQ 'ZSIV-ZFTRIPLE' OR
         SCREEN-NAME EQ 'ZSIV-ZFGRYN'.
        SCREEN-INVISIBLE   = '1'.
        SCREEN-INPUT       = '0'.
      ENDIF.
    ENDIF.

*>> Åë°ü¿äÃ» »ý¼º ÃÊ±âÈ­¸é.
    IF SY-DYNNR EQ '3100'.
      IF SCREEN-NAME(13) EQ 'ZSIV-ZFTRIPLE' OR
         SCREEN-NAME(11) EQ 'ZSIV-ZFGRYN'   OR
         SCREEN-NAME     EQ 'ZSIV-ZFPOYN'   OR
         SCREEN-NAME     EQ 'ZSIV-ZFYSDST'.
        SCREEN-INVISIBLE   = '1'.
        SCREEN-INPUT       = '0'.
      ENDIF.
      IF ( ZTIMIMG00-ZFCSTMD EQ 'S'  OR ZTIMIMG00-ZFCSTMD EQ 'P' ) AND
         SCREEN-NAME EQ 'ZSIV-ZFCSTYN'.
        SCREEN-INVISIBLE   = '1'.
        SCREEN-INPUT       = '0'.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '3500'.
      IF SCREEN-NAME(16) EQ 'ZSCIVHD-ZFTRIPLE' OR
         SCREEN-NAME(06) EQ 'T_LINE'.
        SCREEN-INVISIBLE   = '1'.
        SCREEN-INPUT       = '0'.
      ENDIF.
    ENDIF.
*>> Åë°ü¿äÃ» ÀÚÀçÈ­¸é.
    IF SY-DYNNR EQ '3111' AND
       ( ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P' ).
      IF SCREEN-NAME(14) EQ 'ZSIVIT-ZFMATGB' OR
         SCREEN-NAME(13) EQ 'ZSIVIT-ZFDAMT'  OR
         SCREEN-NAME(14) EQ 'ZSIVIT-ZFUPCST' OR
         SCREEN-NAME(13) EQ 'ZSIVIT-ZFPCST'.
        SCREEN-INVISIBLE   = '1'.
        SCREEN-INPUT       = '0'.
      ENDIF.
      IF SCREEN-NAME(12) EQ 'ZSIVIT-WERKS'.
        IF NOT IT_ZSIVIT-WERKS IS INITIAL.
          SCREEN-INPUT       = '0'.
        ENDIF.
      ENDIF.
    ENDIF.
*>> 2003.10.23: NSH Inserted.
*>> ÇÏ¿ª°ü¸® »ç¿ë¿©ºÎ¿¡ µû¶ó BUTTON À¯¹Â Ã¼Å©..
    IF SY-DYNNR EQ '3111'.
      IF SCREEN-NAME(5) EQ 'PB_CG'.
        IF ZTIMIMG00-ZFCGYN EQ 'X'.
          SCREEN-INVISIBLE = '0'.  " ÇÏ¿ª°ü¸® »ç¿ë ½Ã, º¸ÀÓ.
        ELSE.
          SCREEN-INVISIBLE = '1'.  " ÇÏ¿ª°ü¸® ¹Ì»ç¿ë ½Ã, ¾Èº¸ÀÓ.
        ENDIF.
      ENDIF.
    ENDIF.
    IF SY-DYNNR EQ '3110'.
      IF SCREEN-NAME EQ 'ITM3'.
        DESCRIBE TABLE IT_ZSIVHST LINES LINE1.
        IF LINE1 EQ 0.
          SCREEN-INVISIBLE   = '1'.
        ENDIF.
      ENDIF.
      IF ZTIV-ZFREQTY EQ 'LO' OR  ZTIV-ZFREQTY EQ 'PU'.
        IF SCREEN-NAME EQ 'ZTIV-ZFBLNO' OR
           SCREEN-NAME EQ 'ZTBL-ZFHBLNO' OR
           SCREEN-NAME EQ 'ZTBL-ZFVIA' OR
           SCREEN-NAME EQ 'ZTBL-ZFETA' OR
           SCREEN-NAME EQ 'ZTBL-ZFMSNO'.
          SCREEN-INVISIBLE   = '1'.
        ENDIF.
      ELSE.
        IF SCREEN-NAME(12) EQ 'ZTIV-ZFREQTY'.
          SCREEN-INVISIBLE   = '1'.
        ENDIF.
      ENDIF.
      IF ZTIMIMG00-ZFMSYN IS INITIAL AND SCREEN-NAME EQ 'ZTBL-ZFMSNO'.
        SCREEN-INVISIBLE   = '1'.
        SCREEN-INPUT       = '0'.
*      ELSE.
*         SCREEN-INVISIBLE   = '0'.
*         SCREEN-INPUT       = '1'.
      ENDIF.
    ENDIF.
*>> Åë°ü¿äÃ» Çì´õÈ­¸é.
    IF SY-DYNNR EQ '3112' AND
       ( ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P' ).
      IF SCREEN-NAME(11) EQ 'ZTIV-ZFPHVN' OR
         SCREEN-NAME(11) EQ 'W_ZFOPBN_NM'.
        SCREEN-INVISIBLE   = '1'.
        SCREEN-INPUT       = '0'.
      ENDIF.
    ENDIF.

*>> Åë°ü¿äÃ» »óÅÂÈ­¸é.
    IF SY-DYNNR EQ '3113' AND
       ( ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P' ).
      IF SCREEN-NAME(11) EQ 'ZTIV-ZFCDST'  OR
         SCREEN-NAME(12) EQ 'ZTIV-ZFCIVST' OR
         SCREEN-NAME(11) EQ 'ZTIV-ZFDAMT'  OR
         SCREEN-NAME(12) EQ 'ZTIV-ZFUPCST' OR
         SCREEN-NAME(11) EQ 'ZTIV-ZFPCST'  OR
         SCREEN-NAME(04) EQ 'PB_1'.
        SCREEN-INVISIBLE   = '1'.
      ENDIF.
    ENDIF.
    IF SY-DYNNR EQ '3113'.
      IF ZTIV-ZFREQTY EQ 'LO' OR  ZTIV-ZFREQTY EQ 'PU'.
        IF SCREEN-NAME  EQ 'ZTIV-ZFPONMA' OR
           SCREEN-NAME  EQ 'ZTIV-ZFYSDST'.
          SCREEN-INPUT       = '0'.
          SCREEN-INVISIBLE   = '1'.
        ENDIF.
      ENDIF.
    ENDIF.
*>> Åë°ü¿äÃ» ÀÌ·ÂÈ­¸é.
    IF SY-DYNNR EQ '3114' AND
       ( ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P' ).
      IF SCREEN-NAME(09) EQ 'TC_3114_1'     OR
         SCREEN-NAME(10) EQ 'W_ROW_MAR1'    OR
         SCREEN-NAME(08) EQ 'ZSIVHST1'.
        SCREEN-INVISIBLE   = '1'.
      ENDIF.
    ENDIF.
*>> Á¶Ãâ/Ã¼¼± ÀÏ °æ¿ì¿¡ ÀüÇ¥¹ß»ý°ÇÀÎ °æ¿ì´Â DISPLAY MODE.
    IF SY-DYNNR EQ '9912' AND
       NOT ( IT_ZSMSCST-BELNR  IS INITIAL   AND
             IT_ZSMSCST-BELNRV IS INITIAL ) AND
       SCREEN-GROUP4 = 'ST'.
      SCREEN-INPUT  = '0'.
    ENDIF.
*>> Åë°üºñ¿ëÀÏ °æ¿ì¿¡ ÀüÇ¥¹ß»ý°ÇÀÎ °æ¿ì´Â DISPLAY MODE.
    IF SY-DYNNR EQ '7420' AND
       NOT ( IT_ZSCUCLCST-ZFACDO IS INITIAL ) AND
       SCREEN-GROUP4 = 'ST'.
      SCREEN-INPUT  = '0'.
    ENDIF.
*>> ÇÏ¿ªºñ¿ëÀÏ °æ¿ì¿¡ ÀüÇ¥¹ß»ý°ÇÀÎ °æ¿ì´Â DISPLAY MODE.
    IF SY-DYNNR EQ '0813' AND
       NOT ( IT_ZSCGCST-BELNR IS INITIAL ) AND
       SCREEN-GROUP4  = 'ST'.
      SCREEN-INPUT   = '0'.
    ENDIF.
*>> BL ºñ¿ë°ü·Ã »óÅÂº¯°æ.
    IF SY-DYNNR EQ '0104' AND
       NOT ( IT_ZSBLCST-ZFACDO IS INITIAL ) AND
       SCREEN-GROUP4 = 'ST'.
      SCREEN-INPUT  = '0'.
    ENDIF.
    IF SY-DYNNR EQ '0106'                   AND
       NOT ( IT_ZSBLCST1-ZFACDO IS INITIAL ) AND
       SCREEN-GROUP4 = 'ST'.
      SCREEN-INPUT  = '0'.
    ENDIF.

*>> ÇÑ¼ö¿ø.(¿îÀÓÀÚµ¿°è»êÈ­¸é) »óÅÂº¯°æ. - ºñ¿ëÀü±âÈÄ.==========
    IF SY-DYNNR EQ '0131' OR SY-DYNNR EQ '0135'.
*     > Çì´õ¿¡ È¯À².
      IF SCREEN-NAME = 'ZTBL-ZFEXDTT'.
        LOOP AT IT_ZSBLCST WHERE NOT ZFACDO IS INITIAL.
          SCREEN-INPUT  = '0'.
          EXIT.
        ENDLOOP.
      ENDIF.
*     >°¢ ¾ÆÀÌÅÛº°.
      IF NOT IT_ZSBLCST-ZFACDO IS INITIAL AND
         SCREEN-GROUP4 = 'ST'.
        SCREEN-INPUT  = '0'.
      ENDIF.
    ENDIF.
*---------< 2002.11.12 NHJ Ãß°¡ >---------------------------------------
    IF SY-DYNNR EQ '6216' .
      IF ZTIDR-ZFMATGB NE '1' AND SCREEN-NAME EQ 'NEHS'.
        SCREEN-INPUT     = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
    ENDIF.
*-----------------------------------------------------------------------
*---------< 2002.11.26 JSY Ãß°¡ >---------------------------------------
    IF SY-DYNNR EQ '6214' .
      IF ZTIMIMG00-ZFCUMTD NE '1'. "À§Å¹ÀÌ ¾Æ´Ï¸é.
        IF SCREEN-NAME EQ 'ZTIDR-ZFCTW1' OR
           SCREEN-NAME EQ 'ZTIDR-ZFCTW2' OR
           SCREEN-NAME EQ 'ZTIDR-ZFCTW3' OR
           SCREEN-NAME EQ 'ZTIDR-ZFCTW4' OR
           SCREEN-NAME EQ 'ZTIDR-ZFCTW5' OR
           SCREEN-NAME EQ 'TX1' .

          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '6217' .
      IF ZTIMIMG00-ZFCUMTD NE '2'. "ÀÚ°¡°¡ ¾Æ´Ï¸é.
        IF SCREEN-NAME EQ 'ZTIDRHS-ZFCTW1' OR
           SCREEN-NAME EQ 'ZTIDRHS-ZFCTW2' OR
           SCREEN-NAME EQ 'gb_2' OR
           SCREEN-NAME EQ 'ZTIDRHS-ZFTRRL' OR
           SCREEN-NAME EQ 'ZTIDRHS-ZFGDAL' OR
           SCREEN-NAME EQ 'ZTIDRHS-ZFEXOP'.

          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      ENDIF.
    ENDIF.
*-----------------------------------------------------------------------
*-< 2003.02.05 NHJ Ãß°¡ ¹«È¯ÀÌ°Å³ª ÁÖ±â±â ÀÏ °æ¿ì´Â PO ¹øÈ£ ENABLE >---*
    IF SY-DYNNR EQ '0112' .
      IF ZTBL-ZFPOYN EQ 'N' OR ZTBL-ZFMATGB EQ '1'.
        IF SCREEN-NAME(12) EQ 'ZSBLIT-EBELN' OR
           SCREEN-NAME(12) EQ 'ZSBLIT-EBELP'.
          SCREEN-INPUT = '1'.
        ENDIF.
      ENDIF.
    ENDIF.
*------------------------< END >---------------------------------------*
    IF SY-DYNNR EQ '0131' OR SY-DYNNR EQ '0132' OR
       SY-DYNNR EQ '0133' OR SY-DYNNR EQ '0134'.

      IF SCREEN-GROUP4 = 'ST2'.

        IF ZTBL-ZFVIA = 'AIR'.
          READ TABLE IT_ZSBLCST WITH KEY ZFCSCD = 'ABC'.
        ELSEIF ZTBL-ZFVIA = 'VSL'.
          READ TABLE IT_ZSBLCST WITH KEY ZFCSCD = 'OBC'.
        ENDIF.

        IF NOT IT_ZSBLCST-ZFACDO IS INITIAL.
          SCREEN-INPUT  = '0'.
        ENDIF.
      ENDIF.
    ENDIF.


*==============================================================

* DISPLAY MODE OR CHANGE MODE ½Ã¿¡µµ Á¶È¸ BUTTONÀº ENABLE.
    IF SY-DYNNR EQ '9912' AND
       ( SCREEN-NAME(4) = 'P_LC'  OR
         SCREEN-NAME(4) = 'P_BL'  OR
         SCREEN-NAME(5) = 'P_JUN' ).
      SCREEN-INPUT  =  '1'.
    ENDIF.
*>> »óÅÂº¯°æ POP-UP WINDOW MODE SET.
    IF ( SY-DYNNR EQ '9913'   OR
         SY-DYNNR EQ '0199'   OR
         SY-DYNNR EQ '0899' ) AND
       SCREEN-NAME(12) EQ 'SPOP-OPTION2'.
      SCREEN-INPUT  =  '1'.
    ENDIF.
*>> ºÎº¸(BL) ±âÁØÀÏ °æ¿ì¸¸ º¸ÇèºÎº¸ ¿©ºÎ DISPLAY< NHJ 2002.09.10>
    IF ZTIMIMG00-ZFINMT NE '2'.
      IF SY-DYNNR EQ '0102' AND SCREEN-NAME EQ 'ZTBL-ZFINSYN'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
    ENDIF.
*>> Åë°ü±¸ºÐÀÌ 'À§Å¹Åë°ü'ÀÎ °æ¿ì¸¸ °ü¼¼»ç DISPLAY< NHJ 2002.09.13 >
    IF ZTIMIMG00-ZFCUMTD NE '1'.
      IF SY-DYNNR EQ '6212' AND SCREEN-NAME EQ 'ZTIDR-ZFCUT'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
    ENDIF.
    IF ZTIMIMG00-ZFCUMTD NE '1'.
      IF SY-DYNNR EQ '7412' AND SCREEN-NAME EQ 'ZTIDS-ZFCUT'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME(10) EQ 'W_ROW_MARK'.
      SCREEN-INPUT   = '1'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " P2000_SCR_MODE_SET
*&---------------------------------------------------------------------*
*&      Form  P2000_FIELD_MODE_SET
*&---------------------------------------------------------------------*
FORM P2000_FIELD_MODE_SET USING    P_LOEKZ.

  IF P_LOEKZ EQ 'X'.
    LOOP AT SCREEN.
      IF SCREEN-NAME(10) EQ 'W_ROW_MARK'.
        SCREEN-INPUT   = '1'.
      ELSE.
        SCREEN-INPUT   = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    PERFORM P2000_SCR_MODE_SET.
  ENDIF.

ENDFORM.                    " P2000_FIELD_MODE_SET
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIV_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_ZTIV_MODIFY_SCR3110.

  DESCRIBE TABLE IT_ZSIVIT LINES LINE.
  IF LINE = 0.
    MESSAGE E726.
  ENDIF.

  MOVE SY-UNAME            TO ZTIV-UNAM.
  MOVE SY-DATUM            TO ZTIV-UDAT.
  CASE W_STATUS.
    WHEN  'C'.
      PERFORM   P2000_GET_NUMBER_NEXT  USING  'IV'  ZTIV-ZFIVNO.
      MOVE 'KRW'               TO ZTIV-ZFKRW.
      MOVE  ZTREQHD-ZFBENI     TO ZTIV-LIFNR.
*      MOVE  ZTREQHD-ZFPREPAY   TO ZTIV-ZFPRTE.
      MOVE '1'                 TO ZTIV-ZFCUST.
      MOVE 'N'                 TO ZTIV-ZFCDST.
*      MOVE 'N'                 TO ZTIV-ZFIVST.
      MOVE 'N'                 TO ZTIV-ZFGRST.
      MOVE 'N'                 TO ZTIV-ZFCIVST.
*      MOVE 'N'                 TO ZTIV-ZFPAYYN.
      MOVE SY-UNAME            TO ZTIV-ERNAM.
      MOVE SY-DATUM            TO ZTIV-CDAT.
*      IF ZTIV-ZFPRPYN = 'Y'.
*>> 2001/01/29 KSB
*        MOVE  0    TO ZTIV-ZFPRTE.
*         MOVE 'Y'   TO ZTIV-ZFCUST.
*         MOVE 'Y'   TO ZTIV-ZFCDST.
*      ELSE.
*         MOVE  0    TO ZTIV-ZFPRTE.
*      ENDIF.
      INSERT ZTIV.
    WHEN  'U'.
      DELETE FROM ZTIVIT
             WHERE ZFIVNO = ZTIV-ZFIVNO.
      UPDATE ZTIV.
  ENDCASE.
  IF SY-SUBRC NE  0.
    MESSAGE  E722.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSIVIT.
    CLEAR : ZTIVIT.
    IF IT_ZSIVIT-CCMENGE = 0.    CONTINUE.    ENDIF.
    MOVE-CORRESPONDING IT_ZSIVIT   TO ZTIVIT.
    MOVE ZTIV-ZFIVNO               TO ZTIVIT-ZFIVNO.
    MOVE 'KRW'                     TO ZTIVIT-ZFKRW.
    MOVE ZTIV-ZFIVAMC              TO ZTIVIT-ZFIVAMC.
    INSERT   ZTIVIT.
    IF SY-SUBRC NE 0.
      MESSAGE  E722.
      EXIT.
    ENDIF.
  ENDLOOP.

  PERFORM ZTBL_ZTIV_POYN.
  IF SY-SUBRC NE 0.
    MESSAGE  E727.
  ELSE.
    MESSAGE  S725  WITH  ZTIV-ZFIVNO.
  ENDIF.

ENDFORM.                    " P3000_ZTIV_MODIFY

*&---------------------------------------------------------------------*
*&      Form  ZTBL_ZTIV_POYN
*&---------------------------------------------------------------------*
* Invoice Update ½Ã ÇØ´ç B/LÀÇ ¹«È¯¿©ºÎ, ¼öÀÔ°Å·¡±¸ºÐÀ» Update ÇÑ´Ù.
* B/L ¹«È¯¿©ºÎ : Invoice °¡ À¯¹«È¯ÀÌ È¥ÀçµÇ¾úÀ» ½Ã B/L Àº À¯È¯À¸·Î ÇÑ´Ù.
* B/L ¼öÀÔ°Å·¡±¸ºÐ : À¯È¯½Ã InvoiceÀÇ À¯È¯ ¼öÀÔ°Å·¡±¸ºÐÁß ÃÖ¼Ò ?
*                    ¹«È¯½Ã InvoiceÀÇ ¹«È¯ ¼öÀÔ°Å·¡±¸ºÐÁß ÃÖ¼Ò ?
*----------------------------------------------------------------------*
FORM ZTBL_ZTIV_POYN.

  SELECT SINGLE * FROM   ZTBL
                  WHERE  ZFBLNO   EQ    ZTIV-ZFBLNO.
   *ZTBL  =  ZTBL.

  SELECT COUNT( DISTINCT ZFPOYN ) INTO W_COUNT
    FROM ZTIV
   WHERE ZFBLNO = ZTIV-ZFBLNO
     AND ZFPOYN EQ 'Y'.

  IF W_COUNT > 0.                                     ">À¯È¯ÀÏ °æ¿ì.
    MOVE 'Y' TO ZTBL-ZFPOYN.
    SELECT MIN( ZFPONC ) INTO ZTBL-ZFPONC
           FROM ZTIV
           WHERE ZFBLNO EQ ZTIV-ZFBLNO
           AND ZFPOYN   EQ 'Y'.
  ELSE.                                              ">¹«È¯ÀÏ °æ¿ì.
    MOVE 'N' TO ZTBL-ZFPOYN.
    SELECT MIN( ZFPONC ) INTO ZTBL-ZFPONC
           FROM ZTIV
           WHERE ZFBLNO EQ ZTIV-ZFBLNO
           AND ZFPOYN   EQ 'N'.
  ENDIF.

  IF NOT ( ZTBL-ZFREBELN IS INITIAL ).
    MOVE 'Y' TO ZTBL-ZFPOYN.
  ENDIF.

  IF ZTBL-ZFPOYN EQ *ZTBL-ZFPOYN AND
     ZTBL-ZFPONC EQ *ZTBL-ZFPONC.
    EXIT.
  ENDIF.

  UPDATE ZTBL
         SET ZFPOYN = ZTBL-ZFPOYN
             ZFPONC = ZTBL-ZFPONC
             UNAM   = SY-UNAME
             UDAT   = SY-DATUM
         WHERE ZFBLNO = ZTIV-ZFBLNO.

  CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BL'
       EXPORTING
            UPD_CHNGIND = 'U'
            N_ZTBL      = ZTBL
            O_ZTBL      = *ZTBL.

ENDFORM.                    " ZTBL_ZTIV_POYN
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTCUCLIV_MODIFY_SCR6710
*&---------------------------------------------------------------------*
FORM P3000_ZTCUCLIV_MODIFY_SCR6710.

  DESCRIBE TABLE IT_ZSCUCLIVIT LINES LINE.
  IF LINE = 0.
    MESSAGE E726.
  ENDIF.

  MOVE SY-UNAME TO ZTCUCLIV-UNAM.
  MOVE SY-DATUM TO ZTCUCLIV-UDAT.
  CASE W_STATUS.
    WHEN  'C'.
      PERFORM   P2000_GET_NUMBER_NEXT  USING  'IV'  ZTCUCLIV-ZFIVNO.
      IF ZTCUCLIV-ZFCLSEQ IS INITIAL.
        SELECT MAX( ZFCLSEQ ) INTO ZTCUCLIV-ZFCLSEQ         "Åë°ü¼ø?
          FROM ZTCUCLIV
         WHERE ZFBLNO = ZTBL-ZFBLNO.
        ADD 1 TO ZTCUCLIV-ZFCLSEQ.
      ENDIF.
      MOVE 'B'      TO ZTCUCLIV-ZFCLCD.
      MOVE '1'      TO ZTCUCLIV-ZFCUST.
      MOVE SY-UNAME TO ZTCUCLIV-ERNAM.
      MOVE SY-DATUM TO ZTCUCLIV-CDAT.
      INSERT ZTCUCLIV.
    WHEN  'U'.
      DELETE FROM ZTCUCLIVIT
       WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
      UPDATE ZTCUCLIV.
  ENDCASE.
  IF SY-SUBRC NE  0.
    MESSAGE  E722.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSCUCLIVIT.
    CLEAR : ZTCUCLIVIT.
    MOVE-CORRESPONDING IT_ZSCUCLIVIT   TO ZTCUCLIVIT.
    MOVE ZTCUCLIV-ZFIVNO               TO ZTCUCLIVIT-ZFIVNO.
    INSERT   ZTCUCLIVIT.
    IF SY-SUBRC NE 0.
      MESSAGE  E722.
      EXIT.
    ENDIF.
  ENDLOOP.
*>> ¼öÀÔ½Å°í ÀÚ·á°¡ Á¸ÀçÇÏÁö ¾ÊÀ¸¸é ¼öÀÔ½Å°íÀÚ·á FUNCTION CALL
  SELECT SINGLE * FROM ZTIDR
  WHERE  ZFBLNO   EQ   ZTCUCLIV-ZFBLNO
  AND    ZFCLSEQ  EQ   ZTCUCLIV-ZFCLSEQ.
  IF SY-SUBRC NE 0 AND ZTCUCLIV-ZFCUST = '1'.
*>> ¼öÀÔ IMG SETTING »çÇ× SELECT.
    SELECT  SINGLE * FROM ZTIMIMG00.
    IF ZTIMIMG00-ZFIMPATH NE '1'.
      CALL FUNCTION 'ZIM_CUDATA_PAY_CREATE'
           EXPORTING
                W_ZFIVNO    = ZTCUCLIV-ZFIVNO
           EXCEPTIONS
                ERROR_INSERT.
      IF SY-SUBRC NE 0.
        DELETE FROM ZTCUCLIV   WHERE ZFIVNO EQ ZTCUCLIV-ZFIVNO.
        DELETE FROM ZTCUCLIVIT WHERE ZFIVNO EQ ZTCUCLIVIT-ZFIVNO.
        MESSAGE  E722. EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  UPDATE ZTBL SET ZFPONC = ZTCUCLIV-ZFPONC
                  UNAM   = SY-UNAME
                  UDAT   = SY-DATUM
   WHERE ZFBLNO = ZTBL-ZFBLNO.

  SET PARAMETER ID 'ZPIVNO' FIELD ZTCUCLIV-ZFIVNO.
  MESSAGE  S725  WITH  ZTCUCLIV-ZFIVNO.

ENDFORM.                    " P3000_ZTCUCLIV_MODIFY_SCR6710
*&---------------------------------------------------------------------*
*&      Form  P2000_MOVE_BL_DATA
*&---------------------------------------------------------------------*
FORM P2000_MOVE_BL_DATA.

  REFRESH : IT_ZSBLCST, IT_ZSBLCST1, IT_ZSIV, IT_ZSBLCON, IT_ZSBLIT,
            IT_ZSBLCST_ORG, IT_ZSBLCST1_ORG, IT_ZSIV_ORG,
            IT_ZSBLCON_ORG, IT_ZSBLIT_ORG.

  CLEAR : ZTBL, CSKT,   W_VENDOR_NM, W_ZFBENI_NM,  W_FWDR_NM,
          W_ORIGIN_NM,  W_IMGR_NM,   W_ORIGIN_NM1, W_TRUK_NM,
          W_CARGO_TYPE, W_ZFBNARM,   W_KUNNR_NM,   W_KUNWE_NM,
          W_HAYEK_NM.

  W_STATUS = C_REQ_C.

*>> B/L Document Status
  IF ZTIMIMG00-BLSTYN EQ 'X'.
    MOVE : '1'    TO    ZTBL-ZFBLST.
  ELSE.
    MOVE : 'N'    TO    ZTBL-ZFBLST.
  ENDIF.

*>> Default Freight Vendor Put.
  CLEAR : ZTIMIMG11.
  SELECT SINGLE * FROM ZTIMIMG11 WHERE BUKRS EQ ZVREQHD_ST-BUKRS.
  IF ZTIMIMG11-ZFVNCT EQ 'X'.
    MOVE  ZTIMIMG11-ZFHAYEK  TO  ZTBL-ZFHAYEK.
  ENDIF.

*>> Main Machine Process Data Set.
  IF ZSREQHD-ZFMAIN EQ 'X'.
    PERFORM  P2000_MAINPO_DATA_MOVE.
    EXIT.
  ENDIF.

  MOVE : ZVREQHD_ST-ZFWERKS      TO     ZTBL-ZFWERKS,
         ZSREQHD-ZFPOTY          TO     ZTBL-ZFPOTY,
         ZVREQHD_ST-EBELN        TO     ZTBL-ZFREBELN,
         ZVREQHD_ST-WAERS        TO     ZTBL-ZFBLAMC,
         ZVREQHD_ST-ZFSHCU       TO     ZTBL-ZFCARC,
         ZVREQHD_ST-ZFARCU       TO     ZTBL-ZFAPPC,
         ZVREQHD_ST-ZFOPNNO      TO     ZTBL-ZFOPNNO,
         ZVREQHD_ST-ZFMATGB      TO     ZTBL-ZFMATGB,
         ZVREQHD_ST-BUKRS        TO     ZTBL-BUKRS,
         ZVREQHD_ST-EKORG        TO     ZTBL-EKORG,
         ZVREQHD_ST-EKGRP        TO     ZTBL-EKGRP,
         ZVREQHD_ST-LIFNR        TO     ZTBL-LIFNR,
         ZVREQHD_ST-ZFBENI       TO     ZTBL-ZFBENI,
         ZVREQHD_ST-ZFBENI       TO     ZTBL-ZFFORD,
         ZVREQHD_ST-MAKTX        TO     ZTBL-ZFRGDSR,
         ZVREQHD_ST-ZFPRNAM      TO     ZTBL-ZFPRNAM,
         ZVREQHD_ST-ZFSHCU       TO     ZTBL-ZFCARC,
         ZVREQHD_ST-IMTRD        TO     ZTBL-IMTRD,
         ZVREQHD_ST-ZFARCU       TO     ZTBL-ZFAPPC,
         ZVREQHD_ST-ZFSPRTC      TO     ZTBL-ZFSPRTC,
         ZVREQHD_ST-ZFSPRT       TO     ZTBL-ZFSPRT,
         ZVREQHD_ST-ZFAPRTC      TO     ZTBL-ZFAPRTC,
         ZVREQHD_ST-ZFAPRT       TO     ZTBL-ZFAPRT,
         ZVREQHD_ST-INCO1        TO     ZTBL-INCO1,
         'F'                     TO     ZTBL-ZFRPTTY,
         '001'                   TO     ZTBL-ZFRTCD.

*>> PO(KD) CASE -> Full Container Set.
  SELECT SINGLE * FROM EKKO WHERE EBELN EQ ZTBL-ZFREBELN.
  IF EKKO-BSART EQ 'KD'.
    MOVE  'F'     TO    ZTBL-ZFSHTY.
  ENDIF.
*>> Monetary B/L -> Original B/L
  IF ZSBL-ZFREBELN NE SPACE.
    CLEAR : ZTBL.
    IF NOT ZSBL-ZFBLNO IS INITIAL.
      SELECT SINGLE * FROM ZTBL WHERE ZFBLNO EQ ZSBL-ZFBLNO.
    ELSE.
      SELECT * UP TO 1 ROWS FROM ZTBL WHERE ZFREBELN EQ ZSBL-ZFREBELN
      ORDER BY ZFBLNO DESCENDING.
      ENDSELECT.
    ENDIF.
    CLEAR : ZTBL-ZFETD,   ZTBL-ZFETA,   ZTBL-ZFBLDT, ZTBL-ZFBLSDT,
            ZTBL-ZFBLSDP, ZTBL-ZFCARNM, ZTBL-ZFRETA, ZTBL-ZFELIKZ,
            ZTBL-ZFINSYN, ZTBL-ZFPKCN,  ZTBL-ZFNEWT, ZTBL-ZFTOVL,
            ZTBL-ZFTOWT.
*>> Default Freight Vendor Put.
    CLEAR : ZTIMIMG11.
    SELECT SINGLE * FROM ZTIMIMG11 WHERE BUKRS EQ ZTBL-BUKRS.
    IF ZTIMIMG11-ZFVNCT EQ 'X'.
      MOVE  ZTIMIMG11-ZFHAYEK  TO  ZTBL-ZFHAYEK.
    ENDIF.
  ENDIF.

  SELECT MAX( ZFMSNO ) INTO ZTBL-ZFMSNO
         FROM  ZTMSIT
         WHERE ZFREQNO  EQ   ZVREQHD_ST-ZFREQNO.

  IF NOT ZTBL-ZFMSNO IS INITIAL.
    SELECT SINGLE *
           FROM   ZTMSHD
           WHERE  ZFMSNO EQ ZTBL-ZFMSNO.
    IF ZTMSHD-BUKRS NE ZTBL-BUKRS.
      MESSAGE E540 WITH ZTBL-BUKRS ZTMSHD-BUKRS.
    ENDIF.
    IF ZTBL-ZFCARNM IS INITIAL.
      MOVE ZTMSHD-ZFMSNM   TO   ZTBL-ZFCARNM.
    ENDIF.
    IF ZTBL-ZFETD IS INITIAL.
      MOVE ZTMSHD-ZFSHSDT  TO   ZTBL-ZFETD.
    ENDIF.
    IF ZTBL-ZFETA IS INITIAL.
      MOVE ZTMSHD-ZFETA    TO   ZTBL-ZFETA.
    ENDIF.
    IF ZTBL-ZFCARC  IS INITIAL.
      SELECT MAX( LAND1 ) INTO ZTBL-ZFCARC
             FROM  ZTIEPORT
             WHERE PORT EQ ZTMSHD-ZFSPRTC.
    ENDIF.
    IF ZTBL-ZFSPRTC IS INITIAL.
      MOVE ZTMSHD-ZFSPRTC  TO   ZTBL-ZFSPRTC.
    ENDIF.
    IF ZTBL-ZFAPRTC IS INITIAL.
      MOVE ZTMSHD-ZFAPRTC  TO   ZTBL-ZFSPRTC.
    ENDIF.
  ENDIF.

  IF ZTBL-IMTRD IS INITIAL.
    ZTBL-IMTRD = 'S'.
  ENDIF.

  IF ZVREQHD_ST-ZFTRANS IS INITIAL.
    IF ZTBL-ZFVIA IS INITIAL.
      ZTBL-ZFVIA = 'VSL'.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
           EXPORTING
                INPUT          = 'KG'
           IMPORTING
                OUTPUT         = ZTBL-ZFNEWTM
           EXCEPTIONS
                UNIT_NOT_FOUND = 1
                OTHERS         = 2.
      IF SY-SUBRC NE 0.
        CLEAR ZTBL-ZFNEWTM.
      ENDIF.
    ENDIF.
  ELSE.
    IF ZVREQHD_ST-ZFTRANS EQ 'A'.
      ZTBL-ZFVIA = 'AIR'.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
           EXPORTING
                INPUT          = 'KG'
           IMPORTING
                OUTPUT         = ZTBL-ZFNEWTM
           EXCEPTIONS
                UNIT_NOT_FOUND = 1
                OTHERS         = 2.
      IF SY-SUBRC NE 0.
        CLEAR ZTBL-ZFNEWTM.
      ENDIF.

    ELSE.
      ZTBL-ZFVIA = 'VSL'.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
           EXPORTING
                INPUT          = 'KG'
           IMPORTING
                OUTPUT         = ZTBL-ZFNEWTM
           EXCEPTIONS
                UNIT_NOT_FOUND = 1
                OTHERS         = 2.
      IF SY-SUBRC NE 0.
        CLEAR ZTBL-ZFNEWTM.
      ENDIF.

    ENDIF.
  ENDIF.
*> Shipment Volume Default.
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
       EXPORTING
            INPUT          = 'CBM'
       IMPORTING
            OUTPUT         = ZTBL-ZFTOVLM
       EXCEPTIONS
            UNIT_NOT_FOUND = 1
            OTHERS         = 2.

  IF SY-SUBRC NE 0.
    CLEAR ZTBL-ZFTOVLM.
  ENDIF.

  IF ZTBL-ZFAPPC IS INITIAL.
    ZTBL-ZFAPPC = 'US'.
  ENDIF.
* Loading Port
  IF ZTBL-ZFCARC  IS INITIAL.
    IF NOT ZTBL-LIFNR IS INITIAL.
      SELECT SINGLE LAND1 INTO ZTBL-ZFCARC  FROM   LFA1
                          WHERE  LIFNR EQ ZTBL-LIFNR.
    ENDIF.
  ENDIF.

* Shipment Sequence GET.
  IF NOT ZTBL-ZFREBELN IS INITIAL.
    CLEAR : W_ZFSHNO.
    SELECT MAX( ZFSHNO ) INTO W_ZFSHNO
    FROM   ZTBL
    WHERE  ZFREBELN      EQ   ZTBL-ZFREBELN.
    IF W_ZFSHNO IS INITIAL.
      W_ZFSHNO  =  '01'.
    ELSE.
      W_ZFSHNO  =  W_ZFSHNO  +  1.
    ENDIF.
    MOVE  W_ZFSHNO   TO  ZTBL-ZFSHNO.
  ENDIF.

  ZTBL-ZFBLAMT = 0.
*-----------------------------------------------------------------------
  MOVE : 'CON'             TO  ZTBL-ZFPKCNM,          " Container
         ZTBL-ZFNEWTM      TO  ZTBL-ZFTOWTM,
         T001-WAERS        TO  ZSREQHD-ZFKRW,
         SY-UNAME          TO  ZTBL-ERNAM,
         SY-DATUM          TO  ZTBL-CDAT,
         SY-UNAME          TO  ZTBL-UNAM,
         SY-DATUM          TO  ZTBL-UDAT,
         ZSREQHD-ZFPOTY    TO  ZTBL-ZFPOTY,
         ZSREQHD-ZFHBLNO   TO  ZTBL-ZFHBLNO,
         ''                TO  ZTBL-ZFBLNO,
         ''                TO  ZTBL-ZFPONC,
         'Y'               TO  ZTBL-ZFPOYN.

*>> Monetary PROCESS
  IF RADIO_NP EQ 'X'.
    MOVE :  'B'           TO  ZTBL-ZFUPT,
            'N'           TO  ZTBL-ZFPOYN,
            SY-UNAME      TO  ZTBL-ZFRCVER.
  ENDIF.

* CODE TEXT SELECT
  PERFORM   P2000_GET_CODE_TEXT.

* 2000/06/20   DEFINE
  CLEAR : ZTBL-ZFTRTPM, ZTBL-ZFOTHPM.
  PERFORM P2000_CHARGE_PAY_METHOD.
* 2000/07/13   DEFINE
  CLEAR : W_ZFTRTEC.
  PERFORM P2000_GET_SHIP_PORT_CURRENCY.

  IF ZTBL-ZFPOYN EQ 'Y'.
    CLEAR : ZTBL-ZFPOTY.
  ELSE.
    MOVE : ZSREQHD-ZFPOTY          TO     ZTBL-ZFPOTY.
  ENDIF.

  PERFORM    P1000_READ_CHARGE_RECORD  USING  ZTBL-ZFVIA  'A'.
* 20001/02/09 KSB INSERT.
  PERFORM P1000_GET_BL_ITEM.
* MKIM 20010508
  READ TABLE IT_ZSBLIT INDEX 1.
  IF SY-SUBRC = 0.
*    MOVE : IT_ZSBLIT-MEINS TO ZTBL-ZFNEWTM,
*           IT_ZSBLIT-MEINS TO ZTBL-ZFTOWTM.
*        'KG'              TO  ZTBL-ZFNEWTM,          " KG
*        'KG'              TO  ZTBL-ZFTOWTM,          " KG
  ENDIF.
* 20001/02/15 KSB INSERT.
  PERFORM P1000_GET_BL_AMOUNT.

  MOVE : ZSREQHD-ZFPOTY          TO     ZTBL-ZFPOTY.

  CLEAR : W_ZFETA.

ENDFORM.                    " P2000_MOVE_BL_DATA
*&--------------------------------------------------------------------*
*&      Form  P1000_GET_VENDOR
*&---------------------------------------------------------------------*
FORM P1000_GET_VENDOR USING    P_LIFNR
                      CHANGING P_NAME1.
  DATA : L_TEXT(35).

  CLEAR : P_NAME1, W_LFA1.
  IF P_LIFNR IS INITIAL.
    EXIT.
  ENDIF.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
  CALL FUNCTION 'READ_LFA1'
       EXPORTING
            XLIFNR         = P_LIFNR
       IMPORTING
            XLFA1          = W_LFA1
       EXCEPTIONS
            KEY_INCOMPLETE = 01
            NOT_AUTHORIZED = 02
            NOT_FOUND      = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
  ENDCASE.
*   TRANSLATE W_LFA1 TO UPPER CASE.
  MOVE: W_LFA1-NAME1   TO   L_TEXT.
  TRANSLATE L_TEXT TO UPPER CASE.
  P_NAME1 = L_TEXT.

ENDFORM.                    " P1000_GET_VENDOR
*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
FORM GET_DD07T_SELECT USING    P_DOMNAME
                               P_FIELD
                      CHANGING P_W_NAME.
  DATA : L_TEXT(60).

  CLEAR : DD07T.

  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ P_FIELD
                      ORDER BY AS4VERS DESCENDING.
    EXIT.
  ENDSELECT.

*  TRANSLATE DD07T TO UPPER CASE.

  L_TEXT   = DD07T-DDTEXT.
  TRANSLATE L_TEXT TO UPPER CASE.
  P_W_NAME   = L_TEXT.

ENDFORM.                    " GET_DD07T_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INIT_SCREEN
*&---------------------------------------------------------------------*
FORM P2000_SET_INIT_SCREEN.

  W_OK_CODE = OK-CODE.

  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
           W_STATUS EQ C_OPEN_D ).
*> º¯°æ¿©ºÎ Ã¼Å©.
    PERFORM P2000_MODIFY_CHECK USING W_MODIF_BIT.
    IF W_MODIF_BIT EQ 'Y'.
      PERFORM P2000_SET_MESSAGE USING  'ANZG'.
    ELSE.
      ANTWORT = 'Y'.
    ENDIF.

    IF W_MODIF_BIT NE 'Y'.
      CLEAR OK-CODE.
      PERFORM   P2000_SET_TRANSACTION.
      EXIT.
    ENDIF.

    CASE ANTWORT.
      WHEN 'Y'.              " Yes...
        PERFORM  P3000_DB_MODIFY_SCRCOM.
        CLEAR OK-CODE.
      WHEN 'N'.              " No...
        MESSAGE  S957.
        CLEAR OK-CODE.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDIF.
* LEAVE TO TRANSACTION
  PERFORM   P2000_SET_TRANSACTION.

ENDFORM.                    " P2000_SET_INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INDICATE
*&---------------------------------------------------------------------*
FORM P2000_SET_INDICATE.
  W_OK_CODE = OK-CODE.
*-----------------------------------------------------------------------
* AUTHORITY CHECK
  PERFORM P2000_AUTHORITY_CHECK USING W_OK_CODE.
*-----------------------------------------------------------------------
* Lock Object Set
  IF W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D.
    PERFORM P2000_SET_LOCK.
  ENDIF.
* MESSAGE BOX
*  IF ( W_OK_CODE EQ 'TRTR' OR W_OK_CODE EQ 'TRCL' ) AND
*       W_STAUTS NE C_REQ_D.
  IF W_OK_CODE EQ 'POST' AND W_STATUS EQ C_REQ_D.
    ANTWORT = 'Y'.
  ELSE.
*> º¯°æ¿©ºÎ Ã¼Å©.
    IF SY-TCODE(4) EQ 'ZIM2'.
      PERFORM P2000_MODIFY_CHECK USING W_MODIF_BIT.
    ELSE.
      W_MODIF_BIT = 'Y'.
    ENDIF.

    IF W_MODIF_BIT EQ 'Y'.
      PERFORM P2000_SET_MESSAGE USING  OK-CODE.
    ELSE.
      ANTWORT = 'Y'.
    ENDIF.
  ENDIF.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      CASE W_OK_CODE.
        WHEN 'POST'.
          IF W_STATUS NE C_REQ_D.
            PERFORM  P3000_DB_MODIFY_SCRCOM.
          ENDIF.
          PERFORM  P2000_POST_DATA_MOVE.
          PERFORM  P2000_SET_UNLOCK.
          PERFORM  P2000_SET_SCREEN_SCRCOM.
          LEAVE SCREEN.
          EXIT.
        WHEN 'TRTR' OR 'TRTI' OR 'TRCC' OR 'TRCL' OR 'PYMT' OR 'PYCN'.
          PERFORM  P3000_DB_MODIFY_SCRCOM.
          CLEAR OK-CODE.
          PERFORM  P2000_SET_UNLOCK.
          PERFORM  P2000_SET_SCREEN_SCRCOM.
          LEAVE SCREEN.
        WHEN 'EDIS'.   " EDI DATA CREATE ½Ã.
          PERFORM P3000_EDI_SEND.
        WHEN 'SDSD'.   ">¼±Àû¼­·ù ¼ÛºÎ Ãë¼Ò.
          IF ZTBL-ZFBLST NE '2'.
            CASE ZTBL-ZFBLST.
              WHEN '1'.
                MESSAGE S668 WITH
                'Entry shipping Doc.'.
                EXIT.
              WHEN '3'.
                MESSAGE S668 WITH
                'Entry the actual arrival date'.
                EXIT.
              WHEN '4'.
                MESSAGE S668 WITH
                'Clearenace request completion'.
                EXIT.
              WHEN 'N'.
                MESSAGE S668 WITH
                'Not object of maintain status'.
                EXIT.
            ENDCASE.
          ELSE.
            ZTBL-ZFBLST = '1'.
          ENDIF.
        WHEN 'REAL'.   ">½ÇÀÔÇ× Ãë¼Ò.
          IF ZTBL-ZFBLST NE '3'.
            CASE ZTBL-ZFBLST.
              WHEN '1'.
                MESSAGE S668 WITH
                'Entry shipping Doc.'.
                EXIT.
              WHEN '2'.
                MESSAGE S668 WITH
                'Send shipping doc'.
                EXIT.
              WHEN '4'.
                MESSAGE S668 WITH
                'Clearenace request completion'.
                EXIT.
              WHEN 'N'.
                MESSAGE S668 WITH
                'Not object of maintain status'.
                EXIT.
            ENDCASE.
          ELSE.
            ZTBL-ZFBLST = '2'.
          ENDIF.
        WHEN 'DELE'.   " »èÁ¦.
          CASE SY-DYNNR.
            WHEN '4101'.            " ºÎº¸.
              PERFORM P2000_INS_DEL_STATUS_CHECK.
            WHEN '0101'.            " B/L
              PERFORM P2000_BL_DEL_STATUS_CHECK.
            WHEN '0811'.            " ÇÏ¿ª°ü¸®.
              PERFORM P2000_CG_DEL_STATUS_CHECK.
            WHEN '2601'.            " L/G
              IF ZTLG-ZFDOCST NE 'N'.
                MESSAGE E312 WITH ZTLG-ZFBLNO ZTLG-ZFDOCST.
              ENDIF.
            WHEN '3110'.            " Åë°ü¿äÃ»..
              PERFORM P2000_CC_DEL_STATUS_CHECK.
            WHEN '3510'.            " Commercial Invoice.
              IF ZTCIVHD-ZFIVST EQ 'Y'.
                MESSAGE E368 WITH ZTCIVHD-ZFCIVNO.
              ENDIF.
            WHEN '9201'.            " ¹ÝÀÔ¿¹Á¤Á¤?
              PERFORM P2000_BLINOU_DEL_STATUS_CHECK.
            WHEN '9210'.            " ¹ÝÀÔÁ¤?
              PERFORM P2000_BLINR_DEL_STATUS_CHECK.
            WHEN '9220'.            " ¹ÝÃâÁ¤?
              PERFORM P2000_BLOUR_DEL_STATUS_CHECK.
            WHEN '7410'.
              PERFORM P2000_IDS_DEL_STATUS_CHECK.
            WHEN '6210'.
              PERFORM P2000_IDR_DEL_STATUS_CHECK.
            WHEN '8510'.
              PERFORM P2000_TAX_DEL_STATUS_CHECK.
            WHEN '8210'.
              IF ZTPMTHD-ZFPYA EQ 'Y'.
                MESSAGE E945.
              ENDIF.
          ENDCASE.
        WHEN 'REVK'.   " FLAT DB STATUS º¯?
          CASE SY-DYNNR.
            WHEN '4101'.
              IF ZTINSB-ZFDOCNO IS INITIAL.  MESSAGE E186.  ENDIF.
              IF ZTINSB-ZFDOCST EQ 'O'.
                MESSAGE E105 WITH ZTINSB-ZFBLNO ZTINSB-ZFINSEQ
                                  ZTINSB-ZFEDIST.
              ENDIF.
              ZTINSB-ZFDOCST = 'N'.
              ZTINSB-ZFEDIST = 'N'.
            WHEN '2601'.            " L/G
              IF ZTLG-ZFDOCNO IS INITIAL.  MESSAGE E186.  ENDIF.
              IF ZTLG-ZFDOCST EQ 'O'.
                MESSAGE E105 WITH ZTLG-ZFBLNO '' ZTLG-ZFEDIST.
              ENDIF.
              ZTLG-ZFDOCST = 'N'.
              ZTLG-ZFEDIST = 'N'.
            WHEN '9210'.            " ¹ÝÀÔÁ¤?
              IF ZTBLINR-ZFDOCNO IS INITIAL.  MESSAGE E186.  ENDIF.
              IF ZTBLINR-ZFDOCST EQ 'O'.
                MESSAGE E105 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ
                                  ZTBLINR-ZFEDIST.
              ENDIF.
              ZTBLINR-ZFDOCST = 'N'.
              ZTBLINR-ZFEDIST = 'N'.
            WHEN '9220'.            " ¹ÝÃâÁ¤?
              IF ZTBLOUR-ZFDOCNO IS INITIAL.  MESSAGE E186.  ENDIF.
              IF ZTBLOUR-ZFDOCST EQ 'O'.
                MESSAGE E105 WITH ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ
                                  ZTBLOUR-ZFEDIST.
              ENDIF.
              ZTBLOUR-ZFDOCST = 'N'.
              ZTBLOUR-ZFEDIST = 'N'.
          ENDCASE.
        WHEN 'OPCL'.   " ÇÐÁ¤ Ãë?
          CASE SY-DYNNR.
            WHEN '4101'.            " ºÎº¸.
              IF ZTINSB-ZFEDIST NE 'N'.
                MESSAGE E105 WITH ZTINSB-ZFBLNO  ZTINSB-ZFINSEQ
                                                 ZTINSB-ZFEDIST.
              ENDIF.
              IF ZTINSB-ZFDOCST NE 'O'.
                MESSAGE E103 WITH ZTINSB-ZFBLNO ZTINSB-ZFINSEQ.
              ENDIF.
              ZTINSB-ZFDOCST = 'N'.
              W_NEW_DOCST = 'N'.
            WHEN '2601'.            " L/G
              IF ZTLG-ZFEDIST NE 'N'.
                MESSAGE E105 WITH ZTLG-ZFBLNO  ZTLG-ZFLGSEQ
                                               ZTLG-ZFEDIST.
              ENDIF.
              IF ZTLG-ZFDOCST NE 'O'.
                MESSAGE E103 WITH ZTLG-ZFBLNO ZTLG-ZFLGSEQ.
              ENDIF.
              ZTLG-ZFDOCST = 'N'.
              W_NEW_DOCST = 'N'.
            WHEN '9210'.            " ¹ÝÀÔÁ¤?
              IF ZTBLINR-ZFEDIST NE 'N'.
                MESSAGE E105 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ
                                                 ZTBLINR-ZFEDIST.
              ENDIF.
              IF ZTBLINR-ZFDOCST NE 'O'.
                MESSAGE E103 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ.
              ENDIF.

              SELECT COUNT( * ) INTO W_COUNT FROM ZTIV
                     WHERE ZFBLNO  EQ ZTBLINR-ZFBLNO.
              IF W_COUNT > 0.
                MESSAGE E492 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ.
              ENDIF.

              ZTBLINR-ZFDOCST = 'N'.
              W_NEW_DOCST = 'N'.
            WHEN '9220'.            " ¹ÝÃâÁ¤º¸.
              IF ZTBLOUR-ZFEDIST NE 'N'.
                MESSAGE E105 WITH ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ
                                                 ZTBLOUR-ZFEDIST.
              ENDIF.
              IF ZTBLOUR-ZFDOCST NE 'O'.
                MESSAGE E103 WITH ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ.
              ENDIF.

              SELECT COUNT( * ) INTO W_COUNT FROM ZTIV
                     WHERE ZFBLNO  EQ ZTBLOUR-ZFBLNO.
              IF W_COUNT > 0.
                MESSAGE E492 WITH ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ.
              ENDIF.

              ZTBLOUR-ZFDOCST = 'N'.
              W_NEW_DOCST = 'N'.
          ENDCASE.
      ENDCASE.

      PERFORM  P3000_DB_MODIFY_SCRCOM.
      CLEAR OK-CODE.
      PERFORM  P2000_SET_UNLOCK.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
    WHEN 'N'.              " No...
  ENDCASE.

ENDFORM.                    " P2000_SET_INDICATE

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK USING    P_OK_CODE.

*  IF SY-TCODE(5) EQ 'ZIM21'  OR SY-TCODE(5)  EQ  'ZIM22' OR
*     SY-TCODE(5) EQ 'ZIM23'.
**>>> B/L
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM22'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIM26' OR SY-TCODE(5) EQ 'ZIM27' OR
*         SY-TCODE(5) EQ 'ZIM28' OR SY-TCODE(5) EQ 'ZIM29'.
**>>> L/G
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM27'
*                               ID 'ZIACTVT' FIELD '*'.
*      WHEN 'EDIS' OR 'REVK'.
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIME01'
*                               ID 'ZIACTVT' FIELD '*'.
*      WHEN 'DELR'.
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM29'
*                               ID 'ZIACTVT' FIELD '*'.
*
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIM31' OR SY-TCODE(5) EQ 'ZIM32' OR
*         SY-TCODE(5) EQ 'ZIM33' OR SY-TCODE(5) EQ 'ZIM34'.
**>>> Åë°ü¿äÃ».
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM32'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIM35' OR SY-TCODE(5) EQ 'ZIM36' OR
*         SY-TCODE(5) EQ 'ZIM37' OR SY-TCODE(5) EQ 'ZIM38'.
**>>> Commercial Invoice
*    CASE P_OK_CODE.
*      WHEN 'DELE' OR 'MIRO' OR 'MIR1' OR 'MIR2'.
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM36'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIM62' OR SY-TCODE(5) EQ 'ZIM63' .
**>>> ¼öÀÔ½Å°í.
*    CASE P_OK_CODE.
*      WHEN 'DELE'.
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM62'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIM64' OR SY-TCODE(5) EQ 'ZIM65' OR
*         SY-TCODE(5) EQ 'ZIM66' .
**>>> °¨¸éÇã?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM64'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIM67' OR SY-TCODE(5) EQ 'ZIM68' OR
*         SY-TCODE(5) EQ 'ZIM69' .
**>>> °ú¼¼Åë?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM68'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIM74' OR SY-TCODE(5) EQ 'ZIM75' OR
*         SY-TCODE(5) EQ 'ZIM76'.
**>>> ±ä±Þº¸¼¼¿î¼ÛÀÇ?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM72'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIM74' OR SY-TCODE(5) EQ 'ZIM75' OR
*         SY-TCODE(5) EQ 'ZIM76' OR SY-TCODE(5) EQ 'ZIM78' .
**>>> ¼öÀÔ¸é?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM75'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIM81' OR SY-TCODE(5) EQ 'ZIM82' OR
*         SY-TCODE(5) EQ 'ZIM83' .
**>>> ÇÏ¿ª°ü?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIM82'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIMA2' OR SY-TCODE(5) EQ 'ZIMA3' OR
*         SY-TCODE(5) EQ 'ZIMA4' .
**>>> ¼¼±Ý°è»ê?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIMA3'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIMA6' OR SY-TCODE(5) EQ 'ZIMA7'.
**>>> ÀÎ¼ö?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIMA6'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIMI1' OR SY-TCODE(5) EQ 'ZIMI2' OR
*         SY-TCODE(5) EQ 'ZIMI3' .
**>>> ¹ÝÀÔ¿¹Á¤Á¤?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIMI2'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIMI7' OR SY-TCODE(5) EQ 'ZIMI8' OR
*         SY-TCODE(5) EQ 'ZIMI9' .
**>>> ¹ÝÀÔ½Å?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIMI7'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIMM2' OR SY-TCODE(5) EQ 'ZIMM3'.
**>>> °¨¸éÇã°¡ Ç°?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIMM3'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(5) EQ 'ZIMMS1'.
**>>> ¸ð¼±°ü?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIMMS1'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(4) EQ 'ZIMO'.
**>>> ¹ÝÃâ½Å?
*    CASE P_OK_CODE.
*      WHEN 'DELE' .
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIMO2'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ELSEIF SY-TCODE(4) EQ 'ZIMP'.
**>>> PAYMENT NOTICE.
*    CASE P_OK_CODE.
*      WHEN 'DELE' OR 'TRTR'.
*        AUTHORITY-CHECK OBJECT 'ZIM_ZIMP2'
*                               ID 'ZIACTVT' FIELD '*'.
*    ENDCASE.
*  ENDIF.
*
*  IF SY-SUBRC NE 0.
*    CASE P_OK_CODE.
*      WHEN 'TRTR'.
*        MESSAGE E077 WITH SY-UNAME 'TR reflection'.
*      WHEN 'EDIS'.            " EDI SEND
*        MESSAGE E077 WITH SY-UNAME 'EDI Send'.
*      WHEN 'DELE'.            " Delete.
*        MESSAGE E077 WITH SY-UNAME 'Delete'.
*      WHEN 'REVK'.            " »èÁ¦ Ãë?
*        MESSAGE E077 WITH SY-UNAME 'Delete Cancel'.
*      WHEN 'DELR'.            " Á¢¼ö Ãë?
*        MESSAGE E077 WITH SY-UNAME 'Registration cancel'.
*      WHEN 'OPCL'.            " Open Cancel
*        MESSAGE E077 WITH SY-UNAME 'Open Cancel'.
*    ENDCASE.
*  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DELETE_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING '»èÁ¦ È®ÀÎ'             " Å¸ÀÌÆ²...
                                   'ÇöÀç Document¸¦ »èÁ¦ÇÕ´Ï´Ù.'
                                   '»èÁ¦ÇÏ½Ã°Ú½À´Ï±î?'      " MSG2
                                   'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '1'.                " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Delete confirm'
             'Being deleted the current document.'
             'Do you want to delete?'
             'N'
             '1'.
  ENDCASE.

ENDFORM.                    " P2000_DELETE_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_DEL_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DEL_CANCEL_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING 'EDI Send Cancel'
                              'EDI Send µ¥ÀÌÅ¸¸¦ Ãë¼ÒÇÕ´Ï´Ù.'
                              'Ãë¼Ò ÇÏ½Ã°Ú½À´Ï±î?'          " MSG2
                              'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                              '1'.                " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'EDI send cancel'
             'Do cancel the EDI flat data.'
             'Do you want to cancel?'
             'N'
             '1'.
  ENDCASE.

ENDFORM.                    " P2000_DEL_CANCEL_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_REVOCK_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_REVOCK_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING 'Á¢¼öÃë¼Ò È®ÀÎ'       " Å¸ÀÌÆ²...
                              'Document¸¦ Á¢¼öÃë¼Ò ÇÕ´Ï´Ù.'
                              'Á¢¼öÃë¼Ò ÇÏ½Ã°Ú½À´Ï±î?'      " MSG2
                              'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                              '1'.                " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Accept cancel confirm'
             'Now cancel the acceptance of the document.'
             'Do you want to cancel?'
             'N'
             '1'.
  ENDCASE.


ENDFORM.                    " P2000_REVOCK_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_OPEN_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_OPEN_CANCEL_MESSAGE.

  CASE SY-LANGU.
    WHEN '3'.
    PERFORM P2000_MESSAGE_BOX USING 'Open Cancel È®ÀÎ'       " Å¸ÀÌÆ²...
                                    'Document¸¦ È®Á¤(Open)Ãë¼Ò ÇÕ´Ï´Ù.'
                                    'È®Á¤(Open)Ãë¼Ò ÇÏ½Ã°Ú½À´Ï±î?'" MSG2
                                    'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                                    '1'.                " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Open Cancel confirm'
             'Being canceled the current opened document.'
             'Do you want to cancel?'
             'N'
             '1'.
  ENDCASE.

ENDFORM.                    " P2000_OPEN_CANCEL_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_EDI_SEND_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_EDI_SEND_MESSAGE.

  IF W_STATUS EQ C_REQ_D.
    CASE SY-LANGU.
      WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING 'EDI Create È®ÀÎ'       " Å¸ÀÌÆ²..
                                   'ÇöÀç ¹®¼­ÀÇ EDI Data¸¦ »ý¼ºÇÕ´Ï´Ù.'
                                    '»ý¼º ÇÏ½Ã°Ú½À´Ï±î?'    " MSG2
                               'N'                      " Ãë¼Ò ¹öÆ° À¯/?
                               '1'.                     " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
               'EDI Create confirm'
               'Do create the EDI date for the current document.'
               'Do you want to create?'
               'N'
               '1'.
    ENDCASE.

  ELSE.
    CASE SY-LANGU.
      WHEN '3'.
      PERFORM P2000_MESSAGE_BOX USING 'EDI Create È®ÀÎ'       " Å¸ÀÌÆ²..
                                   'º¯°æµÈ Data´Â ÀúÀåÈÄ¿¡ ¹Ý¿µµË´Ï´Ù.'
                                  '±âÁ¸ Data·Î »ý¼º ÇÏ½Ã°Ú½À´Ï±î?'" MSG2
                               'Y'                      " Ãë¼Ò ¹öÆ° À¯/?
                               '1'.                     " default button
      WHEN OTHERS.
        PERFORM P2000_MESSAGE_BOX USING
               'EDI create confirm'
               'Being reflected after saving the changed date.'
               'Do you want to create with the old data?'
               'Y'
               '1'.
    ENDCASE.

  ENDIF.

ENDFORM.                    " P2000_EDI_SEND_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_BL_DEL_STATUS_CHECK.
* EDI ¼ö½Å Ã¼?
  IF NOT ZTBL-ZFDOCNO IS INITIAL.
    MESSAGE I307 WITH ZTBL-ZFBLNO.
  ENDIF.
* COMMERCIAL INVOICE.
  SELECT COUNT( * ) INTO W_COUNT
                    FROM ZTCIVIT WHERE ZFBLNO  EQ   ZTBL-ZFBLNO.
  IF W_COUNT > 0.
    MESSAGE E306 WITH ZTBL-ZFBLNO W_COUNT 'Invoice Items'.
  ENDIF.

* INVOICE Á¸Àç¿©ºÎ Ã¼?
  SELECT COUNT( * ) INTO W_COUNT
                    FROM ZTIV WHERE ZFBLNO  EQ   ZTBL-ZFBLNO.
  IF W_COUNT > 0.
    MESSAGE E306 WITH ZTBL-ZFBLNO W_COUNT 'Invoice Document'.
  ENDIF.
* L/G Á¸Àç¿©ºÎ Ã¼?
  SELECT COUNT( * ) INTO W_COUNT
                    FROM ZTLG WHERE ZFBLNO  EQ   ZTBL-ZFBLNO.
  IF W_COUNT > 0.
    MESSAGE E306 WITH ZTBL-ZFBLNO W_COUNT 'L/G Document'.
  ENDIF.
* ¹ÝÀÔ¿¹Á¤Á¤º¸ Á¸Àç¿©ºÎ Ã¼?
  SELECT COUNT( * ) INTO W_COUNT
                    FROM ZTBLINOU WHERE ZFBLNO  EQ   ZTBL-ZFBLNO.
  IF W_COUNT > 0.
    MESSAGE E306 WITH ZTBL-ZFBLNO W_COUNT
                 'Carry-in expected info'.
  ENDIF.
* ÇÏ¿ª Á¸Àç¿©ºÎ CHECK!
  SELECT COUNT( * ) INTO W_COUNT
                    FROM ZTCGIT  WHERE ZFBLNO  EQ  ZTBL-ZFBLNO.
  IF W_COUNT > 0.
    MESSAGE E306 WITH ZTBL-ZFBLNO W_COUNT 'loading/unloading'.
  ENDIF.

* 2002.08.30 JSY ÁÖ¼®Ã³¸®.
* Åë°ü Á¸Àç¿©ºÎ Ã¼?
*  SELECT COUNT( * ) INTO W_COUNT
*                    FROM ZTCUCL   WHERE ZFBLNO  EQ   ZTBL-ZFBLNO.
*  IF W_COUNT > 0.
*    MESSAGE E306 WITH ZTBL-ZFBLNO W_COUNT 'Clearance info'.
*  ENDIF.
* Åë°ü invoice Á¸Àç¿©ºÎ Ã¼?
*  SELECT COUNT( * ) INTO W_COUNT
*                    FROM ZTCUCLIV WHERE ZFBLNO  EQ   ZTBL-ZFBLNO.
*  IF W_COUNT > 0.
*    MESSAGE E306 WITH ZTBL-ZFBLNO W_COUNT 'Clearance invoice'.
*  ENDIF.
* ZTCUCL, ZTCUCLIV, ZTCUCLIVIT TABLE »èÁ¦·Î ÀÎÇÑ ÁÖ¼®Ã³¸®.

* ±ä±Þº¸¼¼¿î Á¸Àç¿©ºÎ Ã¼?
  IF NOT ZTBL-ZFHBLNO IS INITIAL.
    SELECT COUNT( * ) INTO W_COUNT
                      FROM ZTBLUG  WHERE ZFHBLNO  EQ   ZTBL-ZFHBLNO.
    IF W_COUNT > 0.
      MESSAGE E306 WITH ZTBL-ZFBLNO W_COUNT
                   'Urgent bonded transportation'.
    ENDIF.
  ENDIF.
* MKIM Ãß°¡ FROM
* B/Lºñ¿ë Ã¼?
  IF ZTIMIMG00-ZFPSMS = '2'.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTBSEG
     WHERE ZFIMDNO EQ ZTBL-ZFBLNO
       AND ( ZFCSTGRP  EQ '004' OR
             ZFCSTGRP  EQ '005' ).
    IF W_COUNT > 0.
      MESSAGE E469.
    ENDIF.
  ELSE.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTBLCST
     WHERE ZFBLNO EQ ZTBL-ZFBLNO.
    IF W_COUNT > 0.
      MESSAGE E469.
    ENDIF.
  ENDIF.
* MKIM Ãß°¡ TO
ENDFORM.                    " P2000_BL_DEL_STATUS_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_UNLOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_UNLOCK.

  CASE SY-TCODE.
*>> 2002.09.06 ÇÑ¼ö¿ø¿¡¼­ NHJ.
    WHEN 'ZIMB2' OR 'ZIMB3' OR 'ZIMB4'.
      PERFORM P2000_SET_BL_INSDOC_LOCK    USING    'U'.
    WHEN 'ZIM22' OR 'ZIM23' OR 'ZIM26' OR    " B/L
         'ZIM221' OR 'ZIM222' OR 'ZIM223'.
      PERFORM P2000_SET_BL_REQDOC_LOCK    USING    'U'.
    WHEN 'ZIM27' OR 'ZIM28' OR 'ZIM29'.    " L/G
      PERFORM P2000_SET_LG_REQDOC_LOCK    USING    'U'.
    WHEN 'ZIM81' OR 'ZIM82' OR 'ZIM83'.    " ÇÏ¿ª°ü¸®.
      PERFORM P2000_SET_ZTCGHD_LOCK    USING    'U'.
    WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.    " ¹ÝÀÔ¿¹Á¤Á¤º¸.
      PERFORM P2000_SET_BLINOU_LOCK    USING    'U'.
    WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9' OR 'ZIMI6'.    " ¹ÝÀÔ.
      SELECT SINGLE * FROM ZTIMIMG00.
      IF ZTIMIMG00-ZFINOU = SPACE.
        PERFORM P2000_SET_BLINR_TMP_LOCK    USING    'U'.
      ENDIF.
      PERFORM P2000_SET_BLINR_LOCK    USING    'U'.
    WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4' OR 'ZIMO6'. " ¹ÝÃâ.
      PERFORM P2000_SET_BLOUR_LOCK    USING    'U'.
    WHEN 'ZIM31'  OR 'ZIM32'  OR 'ZIM33'  OR 'ZIM34' OR  "> Åë°ü¿äÃ».
         'ZIM31L' OR 'ZIM32L' OR 'ZIM33L' OR 'ZIM34L'.  "> ÀÔ°í¿äÃ».
      PERFORM P2000_SET_IV_DOC_LOCK   USING    'U'.
      IF SY-TCODE EQ 'ZIM31' OR SY-TCODE EQ 'ZIM32'.
        PERFORM P2000_SET_IVIT_LOCK  USING    'U'.
      ENDIF.
    WHEN 'ZIM35' OR 'ZIM36' OR 'ZIM37' OR 'ZIM38'.   "> »ó¾÷¼ÛÀå.
      PERFORM P2000_SET_CIV_LOCK    USING    'U'.
      IF SY-TCODE EQ 'ZIM36' OR SY-TCODE EQ 'ZIM35'.
        PERFORM P2000_SET_CIVIT_LOCK  USING    'U'.
      ENDIF.
    WHEN 'ZIMP2' OR 'ZIMP3' OR 'ZIMP4'.         " PAYMENT NOTICE
      PERFORM   P2000_SET_ZTPMTHD_LOCK USING    'U'.
    WHEN 'ZIM62' OR 'ZIM63' OR 'ZIM6D'.         " ¼öÀÔ½Å°í.
      PERFORM   P2000_SET_ZTIDR_LOCK USING    'U'.
    WHEN 'ZIM75' OR 'ZIM76'.                    " ¼öÀÔ¸éÇã.
      PERFORM   P2000_SET_ZTIDS_LOCK USING    'U'.
    WHEN 'ZIM64' OR 'ZIM65' OR 'ZIM77'.         ">°ü¼¼°¨¸é.
      PERFORM   P2000_SET_ZTIDRCR_LOCK   USING   'U'.
    WHEN 'ZIM67' OR 'ZIM68' OR 'ZIM69'.         " °ú¼¼Åë°ü INVOICE
      PERFORM   P2000_SET_ZTCUCLIV_LOCK USING 'U'.
    WHEN 'ZIMA3' OR 'ZIMA4'.                    " ¼¼±Ý°è»ê¼­.
    WHEN 'ZIMA6' OR 'ZIMA7'.                    " ÀÎ¼öÁõ.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_UNLOCK
*&---------------------------------------------------------------------*
*&      Form  P3000_MASTER_LC_MOVE
*&---------------------------------------------------------------------*
FORM P3000_MASTER_LC_MOVE.
* ¿ø»ê?
  IF W_STATUS NE C_REQ_D.
    REFRESH : IT_ZSMLCSG7O.
    LOOP AT  IT_ZTREQORJ.
      MOVE-CORRESPONDING IT_ZTREQORJ TO IT_ZSMLCSG7O.
      APPEND IT_ZSMLCSG7O.
    ENDLOOP.
  ENDIF.
* ¼ö¼ö·á ºÎ´ã?
  PERFORM  P2000_BANK_CHARGE_MOVE.
* ºÐÇÒ ¼±?
  PERFORM  P2000_PARTIAL_SHIPMENT_MOVE.
* È¯Àû ¼±?
  PERFORM  P2000_TRANS_SHIPMENT_MOVE.

* ±âÅ¸ Information
  MOVE : ZTREQHD-ZFSPRT     TO ZTMLCHD-ZFSPRT,   " ¼±Àû?
         ZTREQHD-ZFAPRT     TO ZTMLCHD-ZFAPRT,   " µµÂø?
         ZTREQHD-ZFREQED    TO ZTMLCHD-ZFEXDT,   " À¯È¿±âÀÏ( E/D )
         '5'                TO ZTMLCHD-ZFLCTY,   " ½Å¿ëÀå Á¾?
         '9'                TO ZTMLCHD-ZFEDFN,   " ÀüÀÚ¹®¼­±â?
*        ZTREQHD-ZFCHG      TO ZTMLCHD-ZFCHG,    " ¼ö¼ö·á ºÎ´ã?
         ZTREQHD-ZFOPBN     TO ZTMLCHD-ZFOPBN,   " °³¼³ÀºÇà °Å·¡Ã³ÄÚ?
         ZTREQHD-ZFREQED    TO ZTMLCHD-ZFEXDT,   " ÀÇ·Ú À¯È¿?
         ZTREQHD-INCO1      TO ZTMLCHD-INCO1.    " Incoterms



ENDFORM.                    " P3000_MASTER_LC_MOVE

*&---------------------------------------------------------------------*
*&      Form  P3000_LOCAL_LC_MOVE
*&---------------------------------------------------------------------*
FORM P3000_LOCAL_LC_MOVE.

ENDFORM.                    " P3000_LOCAL_LC_MOVE

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_INCOTERMS_TEXT
*&---------------------------------------------------------------------*
FORM P1000_GET_INCOTERMS_TEXT USING    PA_INCO1
                              CHANGING PA_TEXT.
  CLEAR : PA_TEXT.

  SELECT SINGLE * FROM TINCT WHERE INCO1 EQ PA_INCO1
                             AND   SPRAS EQ SY-LANGU.

  IF SY-SUBRC EQ 0.
    PA_TEXT = TINCT-BEZEI.
    TRANSLATE PA_TEXT TO UPPER CASE.
  ENDIF.

ENDFORM.                    " P1000_GET_INCOTERMS_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_TRANS_OR_PART_SHIP_CHK
*&---------------------------------------------------------------------*
FORM P2000_TRANS_OR_PART_SHIP_CHK.
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      IF ZTREQHD-ZZPSHIP EQ 'X'.    " ºÐÇÒ¼±ÀûÇã¿ë¿©?
        ZTMLCHD-ZFPRMT = '9'.
      ELSE.
        ZTMLCHD-ZFPRMT = '10'.
      ENDIF.

      IF ZTREQHD-ZZTSHIP EQ 'X'.    " È¯Àû Çã¿ë¿©?
        ZTMLCHD-ZFTRMT = '7'.
      ELSE.
        ZTMLCHD-ZFTRMT = '8'.
      ENDIF.
    WHEN 'LO'.
      IF ZTREQHD-ZZPSHIP EQ 'X'.    " ºÐÇÒ¼±ÀûÇã¿ë¿©?
        ZTLLCHD-ZFPRAL = '9'.
      ELSE.
        ZTLLCHD-ZFPRAL = '10'.
      ENDIF.

    WHEN 'PU'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_TRANS_OR_PART_SHIP_CHK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_LFA1_SELECT
*&---------------------------------------------------------------------*
FORM P1000_GET_LFA1_SELECT USING    P_LIFNR
                           CHANGING P_LFA1.
  CLEAR : P_LFA1.
  IF P_LIFNR IS INITIAL.
    EXIT.
  ENDIF.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
  CALL FUNCTION 'READ_LFA1'
       EXPORTING
            XLIFNR         = P_LIFNR
       IMPORTING
            XLFA1          = P_LFA1
       EXCEPTIONS
            KEY_INCOMPLETE = 01
            NOT_AUTHORIZED = 02
            NOT_FOUND      = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
  ENDCASE.
*  TRANSLATE P_LIFNR TO UPPER CASE.

ENDFORM.                    " P1000_GET_LFA1_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_REQ_DOC_STATUS_CHECK
*&---------------------------------------------------------------------*
*  DESC : ¹®¼­»óÅÂº°·Î Æ®·£Àè¼Ç ºÐ±â ¿©ºÎ °Ë?
*----------------------------------------------------------------------*
FORM P2000_REQ_DOC_STATUS_CHECK.
* ¼öÀÔ¹®¼­ÀÇ ¸±¸®Áî(½ÂÀÎ) ¿©ºÎ¸¦ °ËÁõÇÔ.( ¸±¸®Áî¸¸ ÁøÇà °¡´É )
  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE E963.
  ENDIF.

* ¾÷¹«º°·Î ¹®¼­ »óÅÂ°ËÁõÇÔ.
  CASE W_STATUS.
    WHEN C_REQ_U.        " º¯?
      PERFORM P2000_LC_CHAGE_CHECK   CHANGING W_ERR_CHK.
    WHEN C_REQ_D.        " Á¶È¸....
      PERFORM P2000_LC_DISPLAY_CHECK.
    WHEN C_ADD_U.        " Ãß°¡ º¯?
      PERFORM P2000_ADD_CHANGE_CHECK CHANGING W_ERR_CHK.
    WHEN C_OPEN_C.       " OPEN
      PERFORM P2000_OPEN_CHECK       CHANGING W_ERR_CHK.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_REQ_DOC_STATUS_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_LC_DISPLAY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_LC_DISPLAY_CHECK.
* ¹®¼­ »ó?
  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTREQST-ZFEDIST = 'S'.
        MESSAGE I998 WITH ZTREQST-ZFREQNO 'EDI open requeste'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTREQST-ZFEDIST = 'R'.
        MESSAGE I998 WITH ZTREQST-ZFREQNO 'EDI opened'.
      ELSE.
        MESSAGE I998 WITH ZTREQST-ZFREQNO 'Non-EDI opened'.
      ENDIF.
    WHEN 'C'.          " CANCEL
      MESSAGE I998 WITH ZTREQST-ZFREQNO 'CALCELED'.
    WHEN 'A'.          " AMEND
      MESSAGE I998 WITH ZTREQST-ZFREQNO 'AMENDED'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_LC_DISPLAY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_LC_CHAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_LC_CHAGE_CHECK    CHANGING   W_ERR_CHK.

  W_ERR_CHK = 'Y'.

* ±¸¸Å ¸±¸®Áî ±â´É »ç?
  IF ZTIMIMG00-ZFRELYN1 EQ 'X' AND ZTREQST-ZFRLST1 EQ 'R'.
    MESSAGE E999 WITH ZTREQST-ZFREQNO
                 'purchased released' 'Change'.
    EXIT.
  ENDIF.
* Á¢¼öÀÏ?
  IF ZTREQST-ZFRVDT > '00000000'.
    MESSAGE E999 WITH ZTREQST-ZFREQNO
                 'Accepted in imort support team' 'Change'.
  ENDIF.
* ¹®¼­ »ó?
  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTREQST-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTREQST-ZFREQNO 'EDI open released' 'Change'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTREQST-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTREQST-ZFREQNO 'EDI opened' 'Change'.
      ELSE.
        MESSAGE E999
                WITH ZTREQST-ZFREQNO 'Non-EDI opened' 'Change'.
      ENDIF.
      EXIT.
    WHEN 'C'.          " CANCEL
      MESSAGE E999 WITH ZTREQST-ZFREQNO 'CALCELED' 'Change'.
    WHEN 'A'.          " AMEND
      MESSAGE E999 WITH ZTREQST-ZFREQNO 'Amended' 'Change'.
  ENDCASE.

  W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_LC_CHAGE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_ADD_CHANGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_ADD_CHANGE_CHECK    CHANGING W_ERR_CHK.

  W_ERR_CHK = 'Y'.

* ±¸¸Å ¸±¸®Áî ±â´É »ç?
  IF ZTIMIMG00-ZFRELYN1 EQ 'X' AND ZTREQST-ZFRLST1 NE 'R'.
    MESSAGE E999 WITH
      ZTREQHD-ZFREQNO
      'Not purchase released' 'Additional change'.
  ENDIF.

* °³¼³ ¸±¸®Áî ±â´É »ç?
  IF ZTIMIMG00-ZFRELYN2 EQ 'X' AND ZTREQST-ZFRLST2 EQ 'R'.
    MESSAGE E999
       WITH ZTREQHD-ZFREQNO 'open released' 'Additional change'.
  ENDIF.

* ¹®¼­ »ó?
  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTREQST-ZFEDIST = 'S'.
        MESSAGE E999
          WITH ZTREQHD-ZFREQNO 'EDI open release' 'Additional change'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTREQST-ZFEDIST = 'R'.
        MESSAGE E999
        WITH ZTREQHD-ZFREQNO 'EDI opened' 'Additional change'.
      ELSE.
        MESSAGE E999
        WITH ZTREQHD-ZFREQNO 'Non-EDI opened' 'Additional change'.
      ENDIF.
    WHEN 'C'.          " CANCEL
      MESSAGE E999
      WITH ZTREQHD-ZFREQNO 'CALCELED' 'ADDITIONAL CHANGE'.
      EXIT.
    WHEN 'A'.          " AMEND
      MESSAGE E999
      WITH ZTREQHD-ZFREQNO 'Amended' 'Additional change'.
  ENDCASE.

  W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_ADD_CHANGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_OPEN_CHECK
*&---------------------------------------------------------------------*
FORM P2000_OPEN_CHECK  CHANGING W_ERR_CHK.

  W_ERR_CHK = 'Y'.

* ±¸¸Å ¸±¸®Áî ±â´É »ç?
  IF ZTIMIMG00-ZFRELYN1 EQ 'X' AND ZTREQST-ZFRLST1 NE 'R'.
    MESSAGE E999
             WITH ZTREQHD-ZFREQNO
             'Not purchase released' 'Open'.
  ENDIF.
* Á¢¼öÀÏ?
  IF ZTREQST-ZFRVDT IS INITIAL.
    MESSAGE E999
         WITH ZTREQHD-ZFREQNO
              'Not accepted in import support team' 'Open'.
  ENDIF.
* Á¢¼ö¹Ý·Á ¿©?
  IF ZTREQST-ZFRTNYN EQ 'X'.
    MESSAGE E999 WITH ZTREQHD-ZFREQNO
                 'returned registration' 'Open'.
  ENDIF.
* °³¼³ ¸±¸®Áî ±â´É »ç?
  IF ZTIMIMG00-ZFRELYN2 EQ 'X' AND ZTREQST-ZFRLST2 NE 'R'.
    MESSAGE E999
            WITH ZTREQHD-ZFREQNO
            'Not open released' 'Open'.
    EXIT.
  ENDIF.
* ¹®¼­ »ó?
  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTREQST-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTREQHD-ZFREQNO
                     'EDI open requested'  'Open'.
      ELSE.
        MESSAGE E999 WITH ZTREQHD-ZFREQNO  'EDI Data Created'
                                                             'Open'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTREQST-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTREQHD-ZFREQNO 'EDI opened' 'Open'.
      ELSE.
*          MESSAGE I999 WITH ZTREQHD-ZFREQNO 'Non-EDI openedÈ' 'Open'.
        MESSAGE I998 WITH ZTREQHD-ZFREQNO 'Non-EDI opened'.
      ENDIF.
      EXIT.
    WHEN 'C'.          " CANCEL
      MESSAGE E999 WITH ZTREQHD-ZFREQNO 'CALCELED'  'Open'.
    WHEN 'A'.          " AMEND
      MESSAGE E999 WITH ZTREQHD-ZFREQNO 'AMENDED'  'Open'.
  ENDCASE.

  W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_OPEN_CHECK


*&---------------------------------------------------------------------*
*&      Form  P2000_CONVERT_TO_USD_CURR
*&---------------------------------------------------------------------*
FORM P2000_CONVERT_TO_USD_CURR USING    P_FROM_AMOUNT
                                        P_WAERS
                                        P_TO_WAERS
                               CHANGING W_LOCAL_AMT
                                        W_RATE
                                        W_FIXED_RATE.

  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
       EXPORTING
            DATE             = SY-DATUM
            FOREIGN_AMOUNT   = P_FROM_AMOUNT
            FOREIGN_CURRENCY = P_WAERS
            LOCAL_CURRENCY   = P_TO_WAERS
       IMPORTING
            EXCHANGE_RATE    = W_RATE
            LOCAL_AMOUNT     = W_LOCAL_AMT
            FIXED_RATE       = W_FIXED_RATE
       EXCEPTIONS
            OTHERS           = 01.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
               RAISING AMOUNT_CALCULATION.
  ENDIF.

ENDFORM.                    " P2000_CONVERT_TO_USD_CURR
*&---------------------------------------------------------------------*
*&      Form  P2000_OPEN_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
FORM P2000_OPEN_DOC_ITEM_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE  ZFOPNNO   EQ ZSREQHD-ZFOPNNO
           AND    ZFAMDNO   EQ '00000'
           ORDER  BY ZFREQNO.

*-----------------------------------------------------------------------
* TEXT SET
*-----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' 'Áßº¹ ¼öÀÔÀÇ·Ú LIST' INTO SPOP-TITEL
                                             SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.   OPTION = 1.   TEXTLEN = 60.

  CALL SCREEN 0011 STARTING AT 10 3
                   ENDING   AT 90 15.
  IF ANTWORT NE 'Y'.
    CLEAR : ZSREQHD.
    ZSREQHD-EBELN = W_EBELN.
    ZSREQHD-ZFREQNO = W_ZFREQNO.
    ZSREQHD-ZFOPNNO = W_ZFOPNNO.
  ENDIF.

ENDFORM.                    " P2000_OPEN_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_PO_UNLOCK
*&---------------------------------------------------------------------*
FORM P2000_PO_UNLOCK.

  CALL FUNCTION 'DEQUEUE_EMEKKOE'
       EXPORTING
            EBELN = ZTREQHD-EBELN.

ENDFORM.                    " P2000_PO_UNLOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_USD_CONVERT_AMOUNT
*&---------------------------------------------------------------------*
FORM P2000_USD_CONVERT_AMOUNT   USING    PA_MODE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  DESCRIBE TABLE IT_ZSREQIT   LINES G_PARAM_LINE.
  IF G_PARAM_LINE < 1.    " Ç°¸ñÀÌ Á¸ÀçÇÏÁö ¾ÊÀ» °æ¿ì..
    IF PA_MODE EQ 'I'.
      MESSAGE I901.
    ELSE.
      MESSAGE E901.
    ENDIF.
  ENDIF.

  W_TOT_AMOUNT = 0.    W_COUNT      = 0.    W_TOT_ITEM   = 0.
  CLEAR : ZSREQIT.

  LOOP AT IT_ZSREQIT WHERE LOEKZ NE 'X'.
    W_COUNT = W_COUNT + 1.
    IF W_COUNT EQ 1.
      MOVE-CORRESPONDING IT_ZSREQIT TO ZSREQIT.
    ENDIF.
    W_TOT_ITEM   = W_TOT_ITEM   +   ZSREQIT-MENGE.    " ITEM ¼ö?
    W_TOT_AMOUNT = W_TOT_AMOUNT +
                 ( IT_ZSREQIT-MENGE *
                 ( IT_ZSREQIT-NETPR / IT_ZSREQIT-PEINH ) ).
  ENDLOOP.
* ´ëÇ¥ Ç°¸ñ °»?
  ZTREQHD-MATNR   =  ZSREQIT-MATNR.
  ZTREQHD-MAKTX   =  ZSREQIT-TXZ01.
* LOCAL L/C
  IF ZTREQHD-ZFREQTY EQ 'LO' AND ZTLLCHD-ZFGDSC1 IS INITIAL.
    IF SY-TCODE(4) EQ 'ZIM0'.
      ZTLLCHD-ZFGDSC1 =  ZSREQIT-TXZ01.
    ELSE.
    ENDIF.
  ENDIF.
  IF ZTREQHD-ZFREQTY EQ 'LO' AND ZTLLCHD-ZFETC1  IS INITIAL.
    IF SY-TCODE(4) EQ 'ZIM0'.
      CONCATENATE ZSREQIT-STAWN(4) '.' ZSREQIT-STAWN+4(2) '.'
                  ZSREQIT-STAWN+6(4) INTO ZTLLCHD-ZFETC1.
    ELSE.
    ENDIF.
  ENDIF.
* ±¸¸Å½ÂÀÎ?
  IF ZTREQHD-ZFREQTY EQ 'PU'.
    IF SY-TCODE(4) EQ 'ZIM0'.
      ZTPUR-ZFTOCNM   =  ZSREQIT-MEINS.
      ZTPUR-ZFTOCN    =  W_TOT_ITEM.
    ELSE.
    ENDIF.
  ENDIF.
  IF SY-SUBRC NE 0.
    IF PA_MODE EQ 'I'.
      MESSAGE I901.
    ELSE.
      MESSAGE E901.
    ENDIF.
  ENDIF.
  IF W_TOT_AMOUNT <= 0.
    IF PA_MODE EQ 'I'.
      MESSAGE I076.
    ELSE.
      MESSAGE E076.
    ENDIF.
  ENDIF.
  IF SY-TCODE(4) EQ 'ZIM1'.
    CASE ZTREQHD-ZFREQTY.
      WHEN 'LC'.
        W_AMOUNT = W_TOT_AMOUNT - ZSREQHD-ZFOLDAMT.
        IF W_AMOUNT EQ 0.
          MOVE : ''                TO  ZTMLCAMHD-ZFIDCD,
                 0                 TO  ZTMLCAMHD-ZFIDAM,
                 0                 TO  ZTMLCAMHD-ZFNDAMT.
        ELSE.
          IF W_AMOUNT  > 0.
            MOVE : '+'               TO  ZTMLCAMHD-ZFIDCD,
                   W_AMOUNT          TO  ZTMLCAMHD-ZFIDAM,
                   W_TOT_AMOUNT      TO  ZTMLCAMHD-ZFNDAMT.
          ELSE.
            MOVE : '-'               TO  ZTMLCAMHD-ZFIDCD,
                   W_AMOUNT          TO  ZTMLCAMHD-ZFIDAM,
                   W_TOT_AMOUNT      TO  ZTMLCAMHD-ZFNDAMT.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
*-----------------------------------------------------------------------
* 2000/03/07 °­³ªÇü ´ë¸® DEFINE
*----------------------------------------------------------------------`
  PERFORM    P2000_CONVERT_TO_USD_CURR    USING W_TOT_AMOUNT
                                                ZTREQHD-WAERS
                                                W_USD
                                       CHANGING W_LOCAL_AMT
                                                W_RATE
                                                W_FIXED_RATE.
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        ZTMLCHD-WAERS    = ZTREQHD-WAERS.
        ZTMLCHD-ZFOPAMT  = W_TOT_AMOUNT.
      ENDIF.
    WHEN 'LO' OR 'PU'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        IF ZTREQHD-ZFREQTY EQ 'LO'.
          ZTLLCHD-ZFOPAMT  = W_TOT_AMOUNT.
          ZTLLCHD-ZFOPAMTC = ZTREQHD-WAERS.
        ELSE.
          ZTPUR-ZFUSD      = W_USD.
          ZTPUR-ZFTOAMC    = ZTREQHD-WAERS.
          ZTPUR-ZFTDAMC    = ZTREQHD-WAERS.
          ZTPUR-ZFTOAM     = W_TOT_AMOUNT.
          LOOP AT IT_ZSPURSG4.
            IF SY-TABIX EQ 1.
              IT_ZSPURSG4-ZFGOAMT = W_TOT_AMOUNT.
            ELSE.
              IT_ZSPURSG4-ZFGOAMT = 0.
            ENDIF.
            MODIFY IT_ZSPURSG4 INDEX SY-TABIX.
          ENDLOOP.
        ENDIF.
        IF ZTREQHD-ZFREQTY EQ 'LO'.
          PERFORM    P2000_CONVERT_TO_USD_CURR    USING W_TOT_AMOUNT
                                                  ZTREQHD-WAERS
                                                  W_KRW
                                         CHANGING W_LOCAL_AMT
                                                  W_RATE
                                                  W_FIXED_RATE.
          ZTLLCHD-ZFOPKAM = W_LOCAL_AMT.
        ELSE.
          ZTPUR-ZFTOAMU   = W_LOCAL_AMT.
        ENDIF.
      ENDIF.

  ENDCASE.

  ZTREQHD-ZFLASTAM = W_TOT_AMOUNT.
  ZTREQHD-ZFUSDAM  = W_LOCAL_AMT.
  ZTREQHD-ZFUSD    = W_USD.

  IF W_LOCAL_AMT > 10000.
    CASE ZTREQHD-INCO1.
      WHEN 'EXW' OR 'FCA' OR 'FAS' OR 'FOB' OR 'CPT' OR 'CFR'.
        IF ZTREQHD-ZFINSYN NE 'A'.
          MESSAGE I090 WITH ZTREQHD-ZFINSYN 'A'.
          ZTREQHD-ZFINSYN = 'A'.
        ENDIF.
      WHEN OTHERS.
*          IF NOT ZTREQHD-ZFINSYN IS INITIAL.
*             MESSAGE I090 WITH ZTREQHD-ZFINSYN SPACE.
*             CLEAR : ZTREQHD-ZFINSYN.
*          ENDIF.
    ENDCASE.
  ELSE.
    CASE ZTREQHD-INCO1.
      WHEN 'EXW' OR 'FCA' OR 'FAS' OR 'FOB' OR 'CPT' OR 'CFR'.
        IF NOT ZTREQHD-ZFINSYN IS INITIAL.
          MESSAGE I091.
          CLEAR : ZTREQHD-ZFINSYN.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_USD_CONVERT_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  P3000_ITEM_DESC_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ITEM_DESC_WRITE USING      W_DESC_TEXT
                           CHANGING   W_ZFLSG7G.

  W_ZFLSG7G = W_ZFLSG7G + 10.

  MOVE : W_ZFLSG7G        TO    IT_ZSMLCSG7G-ZFLSG7G,
         W_DESC_TEXT      TO    IT_ZSMLCSG7G-ZFDSOG1.
  CLEAR : IT_ZSMLCSG7G-LOEKZ.

  APPEND  IT_ZSMLCSG7G.

ENDFORM.                    " P3000_ITEM_DESC_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_BANK_CHARGE_MOVE
*&---------------------------------------------------------------------*
FORM P2000_BANK_CHARGE_MOVE.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF ZTREQHD-ZFCHG EQ 'S'.    "
      ZTMLCHD-ZFCHG = '16'.
    ELSEIF ZTREQHD-ZFCHG EQ 'B'.
      ZTMLCHD-ZFCHG = '19'.
    ENDIF.
  ENDIF.
ENDFORM.                    " P2000_BANK_CHARGE_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_PARTIAL_SHIPMENT_MOVE
*&---------------------------------------------------------------------*
FORM P2000_PARTIAL_SHIPMENT_MOVE.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF ZTREQHD-ZZPSHIP EQ 'X'.
      ZTMLCHD-ZFPRMT = '9'.
    ELSE.
      ZTMLCHD-ZFPRMT = '10'.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_PARTIAL_SHIPMENT_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_TRANS_SHIPMENT_MOVE
*&---------------------------------------------------------------------*
FORM P2000_TRANS_SHIPMENT_MOVE.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF ZTREQHD-ZZTSHIP EQ 'X'.
      ZTMLCHD-ZFTRMT = '7'.
    ELSE.
      ZTMLCHD-ZFTRMT = '8'.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_TRANS_SHIPMENT_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_EXPIRY_DATE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_EXPIRY_DATE_CHECK.
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      ZTMLCHD-ZFEXDT = ZTREQHD-ZFREQED.
      IF NOT ( ZTMLCHD-ZFLTSD IS INITIAL OR
               ZTREQHD-ZFREQED IS INITIAL ).
        IF ZTREQHD-ZFREQED < ZTMLCHD-ZFLTSD.
          ZTREQST-ZFEDICK = 'X'.
          MESSAGE E068 WITH ZTMLCHD-ZFLTSD ZTREQHD-ZFREQED.
          EXIT.
        ENDIF.
      ENDIF.
    WHEN 'LO'.
      ZTLLCHD-ZFEXDT = ZTREQHD-ZFREQED.
      ZTLLCHD-ZFGDDT = ZTREQHD-ZFREQED.

      IF NOT ( ZTLLCHD-ZFGDDT IS INITIAL OR    " ÀÎµµ±â?
               ZTREQHD-ZFREQED IS INITIAL ).
        IF ZTREQHD-ZFREQED < ZTLLCHD-ZFGDDT.
          ZTREQST-ZFEDICK = 'X'.
          MESSAGE E125 WITH ZTLLCHD-ZFGDDT ZTREQHD-ZFREQED.
          EXIT.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_EXPIRY_DATE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_REQ_ITEM_CHECK
*&---------------------------------------------------------------------*
FORM P2000_REQ_ITEM_CHECK TABLES   IT_ZSREQIT STRUCTURE IT_ZSREQIT.
  IF IT_ZSREQIT-LOEKZ EQ 'X'.
    EXIT.
  ENDIF.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
* HS Code Select....
    IF IT_ZSREQIT-STAWN IS INITIAL.
      MESSAGE E073 WITH IT_ZSREQIT-ZFITMNO.
    ENDIF.
  ENDIF.
* ¼öÀÔÀÇ·Ú ¼ö?
  W_OLD_MENGE = 0.
  IF W_STATUS NE 'C'.
    SELECT SINGLE MENGE INTO W_OLD_MENGE FROM ZTREQIT
                        WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                        AND   ZFITMNO EQ IT_ZSREQIT-ZFITMNO.
  ENDIF.

  IF IT_ZSREQIT-MENGE IS INITIAL.
    W_MENGE = IT_ZSREQIT-ZFPOMENGE - IT_ZSREQIT-ZFLCMENGE
                                   + W_OLD_MENGE.
    IF W_MENGE NE 0 AND IT_ZSREQIT-MENGE EQ 0.
      MESSAGE E074 WITH IT_ZSREQIT-ZFITMNO.
    ENDIF.
  ELSE.
    W_MENGE = IT_ZSREQIT-ZFPOMENGE - IT_ZSREQIT-ZFLCMENGE
                                   + W_OLD_MENGE - IT_ZSREQIT-MENGE.
    IF W_MENGE < 0 AND IT_ZSREQIT-MENGE > 0.
      MESSAGE E075 WITH IT_ZSREQIT-ZFITMNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_REQ_ITEM_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_DEL_STATUS_CHECK.
*-----------------------------------------------------------------------

ENDFORM.                    " P2000_SET_UNLOCK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_BL_REQDOC_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_BL_REQDOC_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLDOC'
         EXPORTING
              ZFBLNO = ZTBL-ZFBLNO
         EXCEPTIONS
              OTHERS = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'B/L Document'
                            ZTBL-ZFBLNO ''
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
    IF SY-TCODE EQ 'ZIM82'.
      PERFORM P2000_SET_ZTCGHD_LOCK    USING    'L'.
    ENDIF.

  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLDOC'
         EXPORTING
              ZFBLNO = ZTBL-ZFBLNO.
  ENDIF.

ENDFORM.                    " P2000_SET_REQDOC_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_FLAT
*&---------------------------------------------------------------------*
FORM P2000_SHOW_FLAT.
  CASE SY-TCODE.
    WHEN 'ZIM21' OR 'ZIM22' OR 'ZIM23'.               " B/L
      IF ZTBL-ZFDOCNO    IS INITIAL.   MESSAGE E974.   ENDIF.
      W_ZFDOCNO = ZTBL-ZFDOCNO.
    WHEN 'ZIM26' OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'.    " L/G
      IF ZTLG-ZFDOCNO    IS INITIAL.   MESSAGE E974.   ENDIF.
      W_ZFDOCNO = ZTLG-ZFDOCNO.
    WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.               " ¹ÝÀÔ¿¹Á¤Á¤?
      IF ZTBLINOU-ZFDOCNO    IS INITIAL.   MESSAGE E974.   ENDIF.
      W_ZFDOCNO = ZTBLINOU-ZFDOCNO.
    WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9'.               " ¹ÝÀÔÁ¤?
      IF ZTBLINR-ZFDOCNO    IS INITIAL.   MESSAGE E974.   ENDIF.
      W_ZFDOCNO = ZTBLINR-ZFDOCNO.
    WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4'     " ¹ÝÃâÁ¤?
                 OR 'ZIMO6'.
      IF ZTBLOUR-ZFDOCNO    IS INITIAL.   MESSAGE E974.   ENDIF.
      W_ZFDOCNO = ZTBLOUR-ZFDOCNO.
    WHEN OTHERS.
  ENDCASE.
* SELECT
  SELECT SINGLE * FROM ZTDHF1 WHERE ZFDHENO EQ W_ZFDOCNO.
  IF SY-SUBRC NE 0.   MESSAGE E975 WITH W_ZFDOCNO.   ENDIF.

  SUBMIT  ZRIMFLAT_DISP  WITH P_DDENO  EQ W_ZFDOCNO
          AND RETURN.

ENDFORM.                    " P2000_SHOW_FLAT
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSBLCST_UPDATE
*&---------------------------------------------------------------------*
FORM P2000_IT_ZSBLCST_UPDATE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSBLCST.
    IF IT_ZSBLCST-WAERS NE 'KRW'.
      IT_ZSBLCST-WAERS  =  ZTBL-ZFTRTEC.
    ENDIF.
    IT_ZSBLCST-ZFCSQ  =  ( SY-TABIX * 10 ) + 10000.
    MODIFY IT_ZSBLCST.
  ENDLOOP.

ENDFORM.                    " P2000_IT_ZSBLCST_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSBLCON_UPDATE
*&---------------------------------------------------------------------*
FORM P2000_IT_ZSBLCON_UPDATE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSBLCON.
    IT_ZSBLCON-ZFCONSEQ =   SY-TABIX * 10.
    MODIFY IT_ZSBLCON.
  ENDLOOP.

ENDFORM.                    " P2000_IT_ZSBLCON_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSBLCST1_UPDATE
*&---------------------------------------------------------------------*
FORM P2000_IT_ZSBLCST1_UPDATE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  SORT IT_ZSBLCST1 BY ZFCD2 ZFCSQ.

  LOOP AT IT_ZSBLCST1.
    IT_ZSBLCST1-ZFCSQ  =   SY-TABIX * 10.
    MODIFY IT_ZSBLCST1.
  ENDLOOP.

ENDFORM.                    " P2000_IT_ZSBLCST1_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
FORM P2000_BL_DOC_ITEM_SELECT.

  W_BLNO    = ZSREQHD-ZFBLNO.
  W_HBLNO   = ZSREQHD-ZFHBLNO.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZTBL
           WHERE  ZFHBLNO EQ ZSREQHD-ZFHBLNO
           ORDER  BY ZFBLNO.

  IF SY-TCODE EQ 'ZIM27' OR SY-TCODE EQ 'ZIM28' OR SY-TCODE EQ 'ZIM29'.
    LOOP AT IT_ZSREQHD.
      W_TABIX = SY-TABIX.
      SELECT SINGLE * FROM ZTLG WHERE ZFBLNO EQ IT_ZSREQHD-ZFBLNO.
      IF SY-SUBRC NE 0.
        DELETE IT_ZSREQHD INDEX W_TABIX.
      ENDIF.
    ENDLOOP.
  ELSEIF SY-TCODE EQ 'ZIMB2' OR SY-TCODE EQ 'ZIMB3' OR
         SY-TCODE EQ 'ZIMB4'.
    LOOP  AT  IT_ZSREQHD.
      W_TABIX = SY-TABIX.
      SELECT SINGLE * FROM ZTINSB WHERE ZFBLNO EQ IT_ZSREQHD-ZFBLNO.
      IF SY-SUBRC NE 0.
        DELETE IT_ZSREQHD INDEX  W_TABIX.
      ENDIF.
    ENDLOOP.
  ENDIF.
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ELSEIF TFILL = 1.
    READ TABLE IT_ZSREQHD INDEX 1.
    W_BLNO  = IT_ZSREQHD-ZFBLNO.
    W_HBLNO = IT_ZSREQHD-ZFHBLNO.
    ANTWORT = 'Y'.
    EXIT.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  IF SY-TCODE(4) NE 'ZIMB'.
    INCLUDE = 'LGCREATE'.
  ELSE.
    INCLUDE = 'INCREATE'.
  ENDIF.
  CALL SCREEN 0014 STARTING AT  07 3
                   ENDING   AT  87 15.

ENDFORM.                    " P2000_BL_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_ITEM_SELECT3 (ÇÑ¼ö¿ø)
*&---------------------------------------------------------------------*
FORM P2000_BL_DOC_ITEM_SELECT3.

  W_BLNO    = ZSREQHD-ZFBLNO.
  W_HBLNO   = ZSREQHD-ZFHBLNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZTBL
           WHERE  ZFREBELN EQ ZSREQHD-EBELN
           ORDER  BY ZFBLNO.

*  IF SY-TCODE EQ 'ZIM27' OR SY-TCODE EQ 'ZIM28' OR SY-TCODE EQ 'ZIM29'.
*    LOOP AT IT_ZSREQHD.
*      W_TABIX = SY-TABIX.
*      SELECT SINGLE * FROM ZTLG WHERE ZFBLNO EQ IT_ZSREQHD-ZFBLNO.
*      IF SY-SUBRC NE 0.
*        DELETE IT_ZSREQHD INDEX W_TABIX.
*      ENDIF.
*    ENDLOOP.
*  ELSEIF SY-TCODE EQ 'ZIMB2' OR SY-TCODE EQ 'ZIMB3' OR
*         SY-TCODE EQ 'ZIMB4'.
*    LOOP  AT  IT_ZSREQHD.
*      W_TABIX = SY-TABIX.
*      SELECT SINGLE * FROM ZTINSB WHERE ZFBLNO EQ IT_ZSREQHD-ZFBLNO.
*      IF SY-SUBRC NE 0.
*        DELETE IT_ZSREQHD INDEX  W_TABIX.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ELSEIF TFILL = 1.
    READ TABLE IT_ZSREQHD INDEX 1.
    W_BLNO  = IT_ZSREQHD-ZFBLNO.
    W_HBLNO = IT_ZSREQHD-ZFHBLNO.
    W_EBELN = IT_ZSREQHD-EBELN.
    ANTWORT = 'Y'.
    EXIT.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  IF SY-TCODE(4) NE 'ZIMB'.
    INCLUDE = 'LGCREATE'.
  ELSE.
    INCLUDE = 'INCREATE'.
  ENDIF.
  CALL SCREEN 0014 STARTING AT  07 3
                   ENDING   AT  87 15.

ENDFORM.                    " P2000_BL_DOC_ITEM_SELECT2
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_BL_INIT_SCR
*&---------------------------------------------------------------------*
FORM P2000_SET_BL_INIT_SCR.
  MOVE 'IVCR' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " INVOICE CREATE
  MOVE 'POST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " INVOICE CREATE
  MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " HEAD CHANGE DOC.
  MOVE 'HII1' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ITEM1 CHANGE DOC.
  MOVE 'HII2' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ITEM2 CHANGE DOC.
  MOVE 'ZIMG' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " IMPORT IMG
  MOVE 'DDLC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ´õºí Å¬¸¯.
  MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀúÀå.
  MOVE 'CKEK' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Á¡°Ë.
  MOVE 'ME23' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ±¸¸Å¹®¼­.
  MOVE 'MK23' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ±¸¸Å¹®¼­.
  MOVE 'MM03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀÚÀç.
  MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ´Ù¸¥¹®¼­.
  MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " »èÁ¦.
  MOVE 'ZIM73' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " ±ä±Þº¸¼¼¿î¼Û..
  MOVE 'GRPT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀÔ°í.
  MOVE 'GRRV' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀÔ°íÃë¼Ò.
  MOVE 'LGPT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/G PRINT.
  MOVE 'LOPU' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÂüÁ¶.
  MOVE 'BENI' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÂüÁ¶.
  MOVE 'ZRIM18' TO IT_EXCL-FCODE.  APPEND IT_EXCL. " CON ÇöÈ².
  MOVE 'SDSD' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¼ÛºÎÃë¼Ò.
  MOVE 'REAL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ½ÇÀÔÇ×Ãë¼Ò..
  MOVE 'IMPREQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">¼öÀÔÀÇ·Ú.
  MOVE 'PAYORD' TO IT_EXCL-FCODE.  APPEND IT_EXCL. ">Áö±ÞÁö½Ã."020827JSY
*>> ITEM.
  MOVE 'MD04' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Stock/requirement
  MOVE 'MMBE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Stock overview
  MOVE 'MB51' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Material document
  MOVE 'ME2M' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Open purchase orde
  MOVE 'ME03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Source list
  MOVE 'HIIT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Item Change doc.
  MOVE 'HICT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ºñ¿ë Change doc.
  MOVE 'HICT1' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ºñ¿ë Change doc.

  MOVE 'MIRO' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¹°´ë»ý¼º.
  MOVE 'MIR1' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¹°´ë»ý¼º.
  MOVE 'MIR2'  TO IT_EXCL-FCODE.   APPEND IT_EXCL. " ÀüÇ¥ CANCEL
* env
  MOVE 'STAT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " STATUS
  MOVE 'FLAT' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " FLAT DATA
  MOVE 'ZIM23' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " B/L Disp
  MOVE 'ZIM28' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " L/G Disp
  MOVE 'ZIM03' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " L/C Disp
  MOVE 'ZIMI3' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " ¹ÝÀÔ¿¹Á¤Á¤º¸.
  MOVE 'ZIMI8' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " ¹ÝÀÔ.
  MOVE 'ZIMO3' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " ¹ÝÃâ.
  MOVE 'ZIM93' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " L/C Disp
  MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " EDI SEND
  MOVE 'ZIBK' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " BANK
  MOVE 'MK03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¼ÛÈ­ÀÎ.
  MOVE 'ZISH' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ¼±¹ÚÈ¸»ç.
  MOVE 'FWDR' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " FWDR
  MOVE 'BANK' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " °³¼³ÀºÇà.
  MOVE 'TRUC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " TRUC
  MOVE 'REVK' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " FLAT CANCEL
  MOVE 'OPCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " OPEN CANCEL
  MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " »è?
  MOVE 'DSRQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " L/C Á¶?
  MOVE 'DSBL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " B/L Á¶?
  MOVE 'MK03' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Vendor
  MOVE 'BENE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Beneficiar
  MOVE 'FWDR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Forewarder
  MOVE 'ZIM58' TO IT_EXCL-FCODE.   APPEND IT_EXCL.   " Statistics
  MOVE 'COST'  TO IT_EXCL-FCODE.   APPEND IT_EXCL.   ">¼öÀÔºñ¿ë.
  MOVE 'BLSD'  TO IT_EXCL-FCODE.   APPEND IT_EXCL.   ">¼ÛºÎÅëº¸¼­.
*     JSY20021112Ãß°¡.(ÇÑ¼ö¿ø)
  MOVE 'DTRS'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "¿îÀÓ°ËÅä¼­.
  MOVE 'DHYK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "ÇÏ¿ª°ËÅä¼­.
  MOVE 'RERQ'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. "°áÀç¿äÃ».
  MOVE 'CIPT'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Print..
  MOVE 'CIST'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Invoice..
  MOVE 'PKST'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Packing..
  MOVE 'CPK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Pack & Inv..

ENDFORM.                    " P2000_SET_BL_INIT_SCR
*&---------------------------------------------------------------------*
*&      Form  GET_PORT_NAME
*&---------------------------------------------------------------------*
FORM GET_PORT_NAME USING    P_GUBUN       P_CODE     P_ERR_MODE
                   CHANGING P_TEXT.
  DATA: L_TEXT(70).

  CLEAR : ZTIMIMG08.
  SELECT SINGLE * FROM ZTIMIMG08 WHERE ZFCDTY   EQ   P_GUBUN
                                 AND   ZFCD     EQ   P_CODE.
  IF SY-SUBRC NE 0.
    MESSAGE ID 'ZIM' TYPE P_ERR_MODE NUMBER 308 WITH P_CODE.
  ENDIF.

  L_TEXT = ZTIMIMG08-ZFCDNM.
  TRANSLATE  L_TEXT TO UPPER CASE.
  P_TEXT = L_TEXT.

ENDFORM.                    " GET_PORT_NAME
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_CODE_TEXT
*&---------------------------------------------------------------------*
FORM P2000_GET_CODE_TEXT.
* ETA ¹× CURRENCY
  MOVE : ZTBL-ZFETA   TO     W_ZFETA,
         ZTBL-ZFTRTEC TO     W_ZFTRTEC.
* TOTAL CHG.
  PERFORM   P2000_TOTAL_CHG_CALC.

* Invoice È¯?
  IF ZTBL_ZFIVAMC IS INITIAL.
    ZTBL_ZFIVAMC = ZTBL-ZFBLAMC.
  ENDIF.

* VENDOR
  IF NOT ZTBL-LIFNR IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTBL-LIFNR
                                CHANGING   W_VENDOR_NM.
  ENDIF.
* Benificiary
  IF NOT ZTBL-ZFBENI IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFBENI
                                CHANGING   W_ZFBENI_NM.
  ENDIF.
* Forwarder
  IF NOT ZTBL-ZFFORD IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFFORD
                                CHANGING   W_FWDR_NM.
  ENDIF.
* ¿îÀÓÁöºÒ¾÷Ã¼.
  IF NOT ZTBL-ZFHAYEK IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFHAYEK
                                CHANGING   W_HAYEK_NM.
  ENDIF.
* GET  ¼±±â±¹Àû  NAME
  IF NOT ZTBL-ZFCARC IS INITIAL.
    CLEAR : T005T.
    SELECT SINGLE * FROM T005T WHERE   SPRAS EQ SY-LANGU
                               AND     LAND1 EQ ZTBL-ZFCARC.
    W_ORIGIN_NM = T005T-LANDX.
  ENDIF.

* GET  µµÂø±¹Àû  NAME
  IF NOT ZTBL-ZFAPPC IS INITIAL.
    CLEAR : T005T.
    SELECT SINGLE * FROM T005T WHERE   SPRAS EQ SY-LANGU
                               AND     LAND1 EQ ZTBL-ZFAPPC.

    W_ORIGIN_NM1 = T005T-LANDX.
  ENDIF.

  IF ZTBL-ZFSPRT IS INITIAL.
    IF NOT ZTBL-ZFSPRTC IS INITIAL.
      SELECT SINGLE * FROM  ZTIEPORT
                      WHERE LAND1    EQ  ZTBL-ZFCARC
                      AND   PORT     EQ  ZTBL-ZFSPRTC.
      IF SY-SUBRC NE 0.
        MESSAGE ID 'ZIM' TYPE 'I' NUMBER 308 WITH ZTBL-ZFSPRTC.
      ENDIF.

      ZTBL-ZFSPRT = ZTIEPORT-PORTT.
    ENDIF.
  ENDIF.

* 00/07/29 ±è¿¬Áß.
  IF ZTBL-ZFAPRT IS INITIAL.
    IF NOT ZTBL-ZFAPRTC IS INITIAL.
      SELECT SINGLE * FROM  ZTIEPORT
                      WHERE LAND1    EQ  ZTBL-ZFAPPC
                      AND   PORT     EQ  ZTBL-ZFAPRTC.
      IF SY-SUBRC NE 0.
        MESSAGE ID 'ZIM' TYPE 'I' NUMBER 308 WITH ZTBL-ZFAPRTC.
      ENDIF.

*      TRANSLATE  ZTIEPORT-PORTT TO UPPER CASE.
      ZTBL-ZFAPRT = ZTIEPORT-PORTT.
    ENDIF.
  ENDIF.
* GET Cuurency
* ZTBL_ZFIVAMC = ZTBL-ZFBLAMC.

* GET TRUCKER NAME
  PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFTRCK
                              CHANGING   W_TRUK_NM.
* GET cargo type Name
  PERFORM   GET_DD07T_SELECT USING      'ZDCAGTY'  ZTBL-ZFCAGTY
                             CHANGING   W_CARGO_TYPE.
* GET º¸¼¼Áö¿ª NAME
  CLEAR :  ZTIMIMG03.
  SELECT * FROM ZTIMIMG03 UP TO 1 ROWS
           WHERE ZFBNARCD EQ ZTBL-ZFBNARCD.
    EXIT.
  ENDSELECT.
  W_ZFBNARM = ZTIMIMG03-ZFBNARM.
* ¼öÀÔ°Å·¡ ±¸?
  IF ZTBL-ZFPONC IS INITIAL.   W_IMGR_NM = ''.   EXIT.   ENDIF.
  PERFORM  GET_PORT_NAME     USING    '001'   ZTBL-ZFPONC   'I'
                             CHANGING   W_TMP_TEXT.
  W_IMGR_NM = W_TMP_TEXT.

*---------------------< 2002.11.13 NHJ Ãß°¡ >--------------------------
  IF SY-TCODE EQ 'ZIM221'.
    MOVE  SY-DATUM    TO   ZTBL-ZFBLSDT.
  ENDIF.

ENDFORM.                    " P2000_GET_CODE_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_TRANSACTION
*&---------------------------------------------------------------------*
FORM P2000_SET_TRANSACTION.
  CASE SY-TCODE.
    WHEN 'ZIMB1' OR 'ZIMB2' OR 'ZIMB3' OR 'ZIMB4'.
      CASE W_OK_CODE.
        WHEN 'OTDC'.                                   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.                                        " CREATE
          LEAVE TO TRANSACTION 'ZIMB1'.
        WHEN 'CHDC'.                                        " CHANGE
          LEAVE TO TRANSACTION 'ZIMB2' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.                                        " DISPLAY
          LEAVE TO TRANSACTION 'ZIMB3' AND SKIP FIRST SCREEN.
        WHEN 'OPDC'.                                        " ADDITIONAL
          LEAVE TO TRANSACTION 'ZIMB4' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIM21' OR 'ZIM22' OR 'ZIM23' OR 'ZIM221' OR 'ZIM222' " B/L
                 OR 'ZIM223'.
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.   " CREATE
          LEAVE TO TRANSACTION 'ZIM21'.
        WHEN 'CHDC'.   " CHANGE
          LEAVE TO TRANSACTION 'ZIM22' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " DISPLAY
          LEAVE TO TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
        WHEN 'DCSD'.   " ¼±Àû¼­·ù ¼ÛºÎ.
          LEAVE TO TRANSACTION 'ZIM221' AND SKIP FIRST SCREEN.
        WHEN 'DCRP'.   " ½ÇÀÔÇ× ÀÔ·Â.
          LEAVE TO TRANSACTION 'ZIM222' AND SKIP FIRST SCREEN.
        WHEN 'BLCT'.   " ºñ¿ëÀÔ·Â.
          LEAVE TO TRANSACTION 'ZIM223' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIM26' OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'.    " L/G
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.   " CREATE
          LEAVE TO TRANSACTION 'ZIM26'.
        WHEN 'CHDC'.   " CHANGE
          LEAVE TO TRANSACTION 'ZIM27' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " DISPLAY
          LEAVE TO TRANSACTION 'ZIM28' AND SKIP FIRST SCREEN.
        WHEN 'OPDC'.   " OPEN
          LEAVE TO TRANSACTION 'ZIM29' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIM31' OR 'ZIM32' OR 'ZIM33' OR 'ZIM34'.    ">Åë°ü¿äÃ».
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.   " CREATE
          LEAVE TO TRANSACTION 'ZIM31'.
        WHEN 'CHDC'.   " CHANGE
          LEAVE TO TRANSACTION 'ZIM32' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " DISPLAY
          LEAVE TO TRANSACTION 'ZIM33' AND SKIP FIRST SCREEN.
        WHEN 'OPDC'.   " OPEN
          LEAVE TO TRANSACTION 'ZIM34' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIM35' OR 'ZIM36' OR 'ZIM37' OR 'ZIM38'.    ">Commercial IV
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.   " CREATE
          LEAVE TO TRANSACTION 'ZIM35'.
        WHEN 'CHDC'.   " CHANGE
          LEAVE TO TRANSACTION 'ZIM36' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " DISPLAY
          LEAVE TO TRANSACTION 'ZIM37' AND SKIP FIRST SCREEN.
        WHEN 'STCH'.   " OPEN
          LEAVE TO TRANSACTION 'ZIM38' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIM81' OR 'ZIM82' OR 'ZIM83'.              "ÇÏ¿ª°ü¸®.
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.   " CREATE
          LEAVE TO TRANSACTION 'ZIM81'.
        WHEN 'CHDC'.   " CHANGE
          LEAVE TO TRANSACTION 'ZIM82' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " DISPLAY
          LEAVE TO TRANSACTION 'ZIM83' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.              " ¹ÝÀÔ¿¹Á¤Á¤?
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.   " CREATE
          LEAVE TO TRANSACTION 'ZIMI1'.
        WHEN 'CHDC'.   " CHANGE
          LEAVE TO TRANSACTION 'ZIMI2' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " DISPLAY
          LEAVE TO TRANSACTION 'ZIMI3' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9' OR 'ZIMI6'.     " ¹Ý?
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.
          LEAVE TO TRANSACTION 'ZIMI6'.
        WHEN 'CHDC'.   " CHANGE
          LEAVE TO TRANSACTION 'ZIMI7' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " DISPLAY
          LEAVE TO TRANSACTION 'ZIMI8' AND SKIP FIRST SCREEN.
        WHEN 'OPDC'.   " OPEN
          LEAVE TO TRANSACTION 'ZIMI9' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIM62' OR 'ZIM63'.                         " ¼öÀÔ½Å°í.
      CASE W_OK_CODE.
        WHEN 'OTDC'.
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CHDC'.
          LEAVE TO TRANSACTION 'ZIM62' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.
          LEAVE TO TRANSACTION 'ZIM63' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIM74' OR 'ZIM75' OR 'ZIM76'.              " ¼öÀÔ¸éÇã.
      CASE W_OK_CODE.
        WHEN 'OTDC'.
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.
          LEAVE TO TRANSACTION 'ZIM74'.
        WHEN 'CHDC'.   " CHANGE
          LEAVE TO TRANSACTION 'ZIM75' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " DISPLAY
          LEAVE TO TRANSACTION 'ZIM76' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4'.   " ¹Ý?
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.   " CREATE
          LEAVE TO TRANSACTION 'ZIMO1'.
        WHEN 'CHDC'.   " CHANGE
          LEAVE TO TRANSACTION 'ZIMO2'  AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " DISPLAY
          LEAVE TO TRANSACTION 'ZIMO3'  AND SKIP FIRST SCREEN.
        WHEN 'OPDC'.   " OPEN
          LEAVE TO TRANSACTION 'ZIMO4'  AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIMP2' OR 'ZIMP3' OR 'ZIMP4'.   " PAYMENT NOTICE.
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.   " CREATE
          LEAVE TO TRANSACTION 'ZIMP2'.
        WHEN 'CHDC'.   " CHANGE
          LEAVE TO TRANSACTION 'ZIMP3'  AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " DISPLAY
          LEAVE TO TRANSACTION 'ZIMP4'  AND SKIP FIRST SCREEN.
*         WHEN 'OPDC'.   " OPEN
*            LEAVE TO TRANSACTION 'ZIMO5'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIMA4' OR 'ZIMA3'.    ">¼¼±Ý°è»ê¼­.
      CASE W_OK_CODE.
        WHEN 'OTDC'.
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CHDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION 'ZIMA3' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " CREATE
          LEAVE TO TRANSACTION 'ZIMA4' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'ZIMA6' OR 'ZIMA7'.    ">ÀÎ¼öÁõ..
      CASE W_OK_CODE.
        WHEN 'OTDC'.
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CHDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION 'ZIMA6' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.   " CREATE
          LEAVE TO TRANSACTION 'ZIMA7' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
  ENDCASE.

ENDFORM.                    " P2000_SET_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DATA_REF
*&---------------------------------------------------------------------*
FORM P2000_BL_DATA_REF.
  CLEAR   : ZTLG-ZFCIAM, W_ZFLGSEQ, W_MENGE, W_TOT_MENGE.
  REFRESH : IT_ZSBLIT, IT_ZSLGGOD.

* Electronic capacity
  ZTLG-ZFEDFN = '9'.
  IF ZTBL-ZFVIA EQ 'AIR'.
    ZTLG-ZFAOCDE = '2BB'.
    ZTLG-ZFBLTY  = 'AWB'.
  ELSEIF ZTBL-ZFVIA EQ 'VSL'.
    ZTLG-ZFAOCDE = '2AZ'.
    ZTLG-ZFBLTY  = 'BM'.
  ENDIF.
* Amount, Packing, Packing Unit SET!
  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_ZSBLIT
  FROM   ZTBLIT
  WHERE  ZFBLNO  =  ZTBL-ZFBLNO
  AND    ZFREQNO =  ZVREQHD_ST-ZFREQNO.
* Total Quantity SUM
  SELECT SUM( BLMENGE )   INTO  W_TOT_MENGE
  FROM   ZTBLIT
  WHERE  ZFBLNO  =  ZTBL-ZFBLNO.

  LOOP AT IT_ZSBLIT.
    CLEAR W_CAMT.
    W_MENGE = W_MENGE  +  IT_ZSBLIT-BLMENGE.
* L/G Gross Description GET!
    IT_ZSLGGOD-MANDT   = SY-MANDT.
    IT_ZSLGGOD-ZFBLNO  = ZTBL-ZFBLNO.
    IT_ZSLGGOD-ZFLGOD  = SY-TABIX *  10.
    CONCATENATE  IT_ZSBLIT-TXZ01 '/'  IT_ZSBLIT-STAWN
                                 INTO IT_ZSLGGOD-ZFGODS.
    APPEND IT_ZSLGGOD.
  ENDLOOP.

  ZTLG-ZFPKCN  =  ZTBL-ZFPKCN * ( W_MENGE / W_TOT_MENGE ).

  MOVE : ZTBL-ZFBLAMC       TO    ZTLG-ZFCIAMC,
         ZTBL-ZFPKCNM       TO    ZTLG-ZFPKCNM,
         ZTBL-BUKRS         TO    ZTLG-BUKRS,
         ZTBL-ZFBLAMT       TO    ZTLG-ZFCIAM,
         ZTBL-MANDT         TO    ZTLG-MANDT,
         ZTBL-ZFBLNO        TO    ZTLG-ZFBLNO,
         ZTBL-ZFHBLNO       TO    ZTLG-ZFHBLNO,
         ZTBL-ZFFORD        TO    ZTLG-ZFCARIR,
         ZTBL-ZFCARNM       TO    ZTLG-ZFCARNM,
         ZTBL-ZFBLDT        TO    ZTLG-ZFTBIDT,
         ZTBL-ZFCARC        TO    ZTLG-ZFSHCU,
         ZTBL-ZFSPRT        TO    ZTLG-ZFSHCUNM,
         'US'               TO    ZTLG-ZFARCUM,
         ZTBL-ZFAPRT        TO    ZTLG-ZFARCUNM,
         ZVREQHD_ST-ZFREQNO TO    ZTLG-ZFREQNO,
         ZVREQHD_ST-ZFOPBN  TO    ZTLG-ZFISBNC,
         SY-DATUM           TO    ZTLG-ZFAPDT,
         'AAC'              TO    ZTLG-ZFDOCTY,
         ZSREQHD-ZFOPNNO    TO    ZTLG-ZFDCNO,
         ZTBL-LIFNR         TO    ZTLG-ZFGSCD,
         'N'                TO    ZTLG-ZFDOCST,
         'N'                TO    ZTLG-ZFEDIST,
         'X'                TO    ZTLG-ZFEDICK,
         ZTBL-ZFETA         TO    ZTLG-ZFETA,
         SY-UNAME           TO    ZTLG-ERNAM,
         SY-DATUM           TO    ZTLG-CDAT,
         SY-UNAME           TO    ZTLG-UNAM,
         SY-DATUM           TO    ZTLG-UDAT.
* applicant.
  SELECT SINGLE * FROM ZTIMIMGTX
         WHERE    BUKRS   EQ   ZTBL-BUKRS.
  IF SY-SUBRC NE 0.
    MESSAGE E949 WITH ZTBL-BUKRS.
  ENDIF.

  MOVE : ZTIMIMGTX-ZFAPPAD1L    TO ZTLG-ZFAPPAD1,
         ZTIMIMGTX-ZFAPPAD2L    TO ZTLG-ZFAPPAD2,
         ZTIMIMGTX-ZFAPPAD3L    TO ZTLG-ZFAPPAD3,
         ZTIMIMGTX-ZFAPPNM      TO ZTLG-ZFELENM,
         ZTIMIMGTX-ZFREPRE      TO ZTLG-ZFREPRE,
         ZTIMIMGTX-ZFELEID      TO ZTLG-ZFELEID.
* Shipper  ==> VENDOR CODE
  PERFORM  P1000_GET_VENDOR   USING      ZTLG-ZFGSCD
                              CHANGING   ZTLG-ZFGSNM1.

  MOVE : W_LFA1-NAME1        TO ZTLG-ZFGSNM1,  " PULL NAME
         W_LFA1-STRAS        TO ZTLG-ZFGSNM2,  " Benificiary
         W_LFA1-ORT01        TO ZTLG-ZFGSNM3.  " Benificiary
* Issuing Bank
  IF NOT ZTLG-ZFISBNC IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTLG-ZFISBNC
                                CHANGING   ZTLG-ZFISBNM.

    MOVE : W_LFA1-NAME1    TO   ZTLG-ZFISBNM,
           W_LFA1-NAME2    TO   ZTLG-ZFISBB,
           W_LFA1-KRAUS    TO   ZTLG-ZFISBNCD.

  ENDIF.

ENDFORM.                    " P2000_BL_DATA_REF
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSLGGOD_UPDATE
*&---------------------------------------------------------------------*
FORM P2000_IT_ZSLGGOD_UPDATE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* ¹ÌÀÔ·Â »è?
  LOOP AT IT_ZSLGGOD   WHERE ZFGODS  EQ SPACE.
    DELETE IT_ZSLGGOD   INDEX SY-TABIX.
  ENDLOOP.

* INDEX ¼öÁ¤ ÀÛ?
  LOOP AT IT_ZSLGGOD.
    IT_ZSLGGOD-ZFLGOD  = SY-TABIX * 10.
    MODIFY IT_ZSLGGOD.
  ENDLOOP.

ENDFORM.                    " P2000_IT_ZSLGGOD_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_VENDOR_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_VENDOR_DISPLAY USING    P_VENDOR_CODE.

  IF P_VENDOR_CODE IS INITIAL.
    MESSAGE E136.
  ENDIF.
* È­¸é PARAMETER ID
*  SET PARAMETER ID 'KDY' FIELD '/320/310/130/120/110'.
  SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
  SET PARAMETER ID 'LIF' FIELD P_VENDOR_CODE.
  SET PARAMETER ID 'EKO' FIELD ''.
  EXPORT 'LIF'   TO MEMORY ID 'LIF'.
  EXPORT 'EKO'   TO MEMORY ID 'EKO'.

  CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_VENDOR_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_BL_DOC_DISPLAY USING    P_ZFBLNO.

  IF P_ZFBLNO IS INITIAL.
    MESSAGE E311.
  ENDIF.

  SET PARAMETER ID 'ZPBLNO' FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_BL_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P3000_EDI_SEND
*&---------------------------------------------------------------------*
FORM P3000_EDI_SEND.

  CASE SY-TCODE.
    WHEN 'ZIM26' OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'.   " L/G
      PERFORM    P3000_LG_EDI_SEND.
    WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9'.              " ¹ÝÀÔÁ¤?
      PERFORM    P3000_BLINR_EDI_SEND.
    WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4'.   " ¹ÝÃâÁ¤?
      PERFORM    P3000_BLOUR_EDI_SEND.
    WHEN 'ZIM62' OR 'ZIM63'.
      PERFORM    P3000_ZTIDR_EDI_SEND.
  ENDCASE.

ENDFORM.                    " P3000_EDI_SEND
*&---------------------------------------------------------------------*
*&      Form  P2000_LG_OPEN_CHECK
*&---------------------------------------------------------------------*
FORM P2000_LG_OPEN_CHECK.
* ¹®¼­ »ó?
  CASE ZTLG-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTLG-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTLG-ZFBLNO  'EDI open requeste'  'Open'.
      ELSE.
        MESSAGE E999 WITH ZTLG-ZFBLNO  'EDI Data Created'  'Open'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTLG-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTLG-ZFBLNO 'EDI opened' 'Open'.
      ELSE.
        MESSAGE I998 WITH ZTLG-ZFBLNO 'Non-EDI opened'.
      ENDIF.
      EXIT.
    WHEN 'C'.          " CANCEL
      MESSAGE E999 WITH ZTLG-ZFBLNO 'CALCELED'  'Open'.
    WHEN 'A'.          " AMEND
      MESSAGE E999 WITH ZTLG-ZFBLNO 'AMENDED'  'Open'.
  ENDCASE.

ENDFORM.                    " P2000_LG_OPEN_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_MASTER_LG_FLAT_CREATE
*&---------------------------------------------------------------------*
FORM P3000_MASTER_LG_FLAT_CREATE.
  W_ZFCDDOC = 'APPLOG'.
  W_ZFDHSRO = W_LFA1-BAHNS.        " ½Äº°?
  W_ZFDHREF = ZTLG-ZFBLNO.         " ÂüÁ¶¹øÈ£ ( L/G NO )
*  W_ZFDHDDB = ''.                  " ºÎ?
  W_ZFDHENO = ZTLG-ZFDOCNO.        " ¹®¼­¹ø?

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
             W_ZFCDDOC    =   W_ZFCDDOC
             W_ZFDHSRO    =   W_ZFDHSRO
             W_ZFDHREF    =   W_ZFDHREF
*             W_ZFDHDDB    =   W_ZFDHDDB
             W_BUKRS       =    ZTLG-BUKRS
       CHANGING
             W_ZFDHENO    =   W_ZFDHENO
       EXCEPTIONS
             DB_ERROR     =   4
             NO_TYPE      =   8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_APPLOG_EDI_SEND'
       EXPORTING
            W_ZFBLNO  = ZTLG-ZFBLNO
            W_ZFLGSEQ = ZTLG-ZFLGSEQ
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

ENDFORM.                    " P3000_MASTER_LG_FLAT_CREATE
*&---------------------------------------------------------------------*
*&      Form  P2000_LG_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_LG_STATUS_CHECK.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTBL-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    IF ZTIMIMGTX-APPLOG EQ 'X'.
      ZTLG-ZFEDICK = 'O'.
      ASSIGN ZTLG-ZFEDICK TO <FS_CHK>.
* Ç×°ø/ÇØ»ó±¸ºÐ EDI ÄÚµå.
      IF ZTLG-ZFAOCDE IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH
                     'Classification of air/appropriate Doc'.
        EXIT.
      ENDIF.
* ¼±¹ÚÈ¸»ç.
      IF ZTLG-ZFCARR1 IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Shipping company name'. EXIT.
      ENDIF.
      ASSIGN ZTLG-ZFCARR1 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.
      CHECK : ZTLG-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFCARR2 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.
      CHECK : ZTLG-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFCARR3 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.
      CHECK : ZTLG-ZFEDICK NE 'X'.
* ¼ÛÈ­ÀÎ.
      IF ZTLG-ZFGSNM1 IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Sender name'. EXIT.
      ENDIF.
      ASSIGN ZTLG-ZFGSNM1 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.
      CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFGSNM2 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.
      CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFGSNM3 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.
      CHECK : ZTREQST-ZFEDICK NE 'X'.
* ½ÅÃ»ÀÎ.
      IF ZTLG-ZFAPPAD1 IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Applicant address'. EXIT.
      ENDIF.
      IF ZTLG-ZFELENM IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Applicant firm name'. EXIT.
      ENDIF.
      IF ZTLG-ZFREPRE  IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Applicant electronic signature'. EXIT.
      ENDIF.
* 2002.10.17 NSH Insert.
*      IF ZTLG-ZFELEID  IS INITIAL.
*        ZTLG-ZFEDICK = 'X'.
*        MESSAGE I167 WITH 'Applicant'. EXIT.
*      ENDIF.
      ASSIGN ZTLG-ZFAPPAD1 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFAPPAD2 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFAPPAD3 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFELENM  TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFREPRE  TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.

* 2002.10.17 NSH Insert.
*      ASSIGN ZTLG-ZFELEID  TO <FS_F>.
*      PERFORM P2000_TEXT_FIELD_CHECK.
*      CHECK : ZTREQST-ZFEDICK NE 'X'.

* ¼ÛÀå±Ý¾× ¹× ÅëÈ­.
      IF ZTLG-ZFCIAM LE 0.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Commercial invoice amount'. EXIT.
      ENDIF.
      IF ZTLG-ZFCIAMC IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Commercial invoice currency unit'. EXIT.
      ENDIF.

* ½Å¿ëÀå ( °è¾à¼­ ) ±¸ºÐ ¹× ¹øÈ£.
      IF ZTLG-ZFDOCTY IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'L/C(contract) classification'.
        EXIT.
      ENDIF.
      IF ZTLG-ZFDCNO IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'L/C(contract) No'.
        EXIT.
      ENDIF.
      ASSIGN ZTLG-ZFDCNO TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.

* ¼±ÇÏÁõ±Ç ¹øÈ£.
      IF ZTLG-ZFHBLNO IS INITIAL.
        ZTLG-ZFEDICK = 'X'. MESSAGE I167 WITH 'B/L No'.  EXIT.
      ENDIF.
      ASSIGN ZTLG-ZFHBLNO TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.

* Ç×Â÷/Æí¸í.
      IF ZTLG-ZFCARNM IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Voyage No/Name'. EXIT.
      ENDIF.
      IF ZTLG-ZFSSEQ IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Voyage No/Name'. EXIT.
      ENDIF.
      ASSIGN ZTLG-ZFCARNM TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFSSEQ TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.

* µµÂø ¿¹Á¤ÀÏ.
      IF ZTLG-ZFETA IS INITIAL.
        ZTLG-ZFEDICK = 'X'. MESSAGE I167 WITH 'ETD'. EXIT.
      ENDIF.

* ¼±ÇÏÁõ±Ç¹ß±ÞÀÏÀÚ.
      IF ZTLG-ZFTBIDT IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'B/L issuing date'. EXIT.
      ENDIF.

* ½ÅÃ»ÀÏÀÚ.
      IF ZTLG-ZFAPDT IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'application date'.  EXIT.
      ENDIF.

* ½ÅÃ»ÀÏÀÚ >= ¼±ÇÏÁõ±Ç¹ß±ÞÀÏÀÚ.
      IF ZTLG-ZFAPDT LT ZTLG-ZFTBIDT.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I977
           WITH 'Application date is not before B/L issuing date.'.
        EXIT.
      ENDIF.

* ¼±ÀûÇ× /µµÂøÇ× ÄÚµå.
      IF ZTLG-ZFSHCU IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Port of loading'. EXIT.
      ENDIF.
      IF ZTLG-ZFARCUM IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Port of discharge'. EXIT.
      ENDIF.

* ¼±ÀûÇ× / µµÂøÇ× Text.
      IF ZTLG-ZFSHCUNM IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Port of loading Text'. EXIT.
      ENDIF.
      IF ZTLG-ZFARCUNM IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Port of discharge Text'. EXIT.
      ENDIF.

* ¼±ÀûÇ× / µµÂøÇ× TEXT
      ASSIGN ZTLG-ZFSHCUNM TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFARCUNM TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.

* È­¹°Ç¥½Ã ¹× ¹øÈ£ TEXT
      ASSIGN ZTLG-ZFGODR1 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.

      ASSIGN ZTLG-ZFGODR2 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFGODR3 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFGODR4 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFGODR5 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFGODR6 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFGODR7 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFGODR8 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFGODR9 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFGODR10 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.

* »óÇ°¸í¼¼.
      LOOP AT IT_ZSLGGOD.
        ASSIGN IT_ZSLGGOD-ZFGODS  TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDLOOP.
* °áÁ¦±â°£ TEXT
      ASSIGN ZTLG-ZFPTRMD         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* ¹ß±ÞÀºÇà(½ÂÀÎÀºÇà)
      IF ZTLG-ZFISBNCD IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Bank branch code'. EXIT.
      ENDIF.
      IF ZTLG-ZFISBNM  IS INITIAL.
        ZTLG-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Bank name'. EXIT.
      ENDIF.
      ASSIGN ZTLG-ZFISBNCD        TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFISBNM         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLG-ZFISBB          TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDIF.
  ENDIF.
ENDFORM.                    " P2000_LG_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_TEXT_FIELD_CHECK
*&---------------------------------------------------------------------*
FORM P2000_TEXT_FIELD_CHECK.

  PERFORM P2000_SPACE_CUT USING <FS_F>.
  PERFORM SPECIAL_CHAR_SEARCH USING <FS_F> 'I'.

ENDFORM.                    " P2000_TEXT_FIELD_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_SPACE_CUT
*&---------------------------------------------------------------------*
FORM P2000_SPACE_CUT USING    PARA_STRING.
  DATA : CTEXT     TYPE C,
         CPOSITION TYPE I,
         CLEN      TYPE I.

  CPOSITION = 0.
  CLEN = STRLEN( PARA_STRING ).

  MOVE PARA_STRING+CPOSITION(1) TO CTEXT.
  WHILE CTEXT EQ ' '.
    IF CPOSITION >= CLEN.
      EXIT.
    ENDIF.
    CPOSITION = CPOSITION + 1.
    MOVE PARA_STRING+CPOSITION(1) TO CTEXT.
  ENDWHILE.
  MOVE PARA_STRING+CPOSITION TO PARA_STRING.

ENDFORM.                    " P2000_SPACE_CUT
*&---------------------------------------------------------------------*
*&      Form  SPECIAL_CHAR_SEARCH
*&---------------------------------------------------------------------*
FORM SPECIAL_CHAR_SEARCH USING     SEARCH_TEXT    MSG_ID.

  PERFORM    SPECIAL_SEARCH_SUB    USING     '~'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '`'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '_'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '@'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '#'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '$'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '|'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '\'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '{'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '}'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '['   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.
  PERFORM    SPECIAL_SEARCH_SUB    USING     ']'   SEARCH_TEXT MSG_ID.
  CHECK : <FS_CHK>     NE 'X'.

ENDFORM.                    " SPECIAL_CHAR_SEARCH

*&---------------------------------------------------------------------*
*&      Form  SPECIAL_SEARCH_SUB
*&---------------------------------------------------------------------*
FORM SPECIAL_SEARCH_SUB USING    CHECK_CHAR
                                 SEARCH_TEXT    MSG_ID.

  SEARCH    SEARCH_TEXT    FOR   CHECK_CHAR.

  IF  SY-SUBRC       EQ       0.
    <FS_CHK> = 'X'.
    MESSAGE ID 'ZIM' TYPE MSG_ID NUMBER '151' WITH CHECK_CHAR.
  ENDIF.

ENDFORM.                    " SPECIAL_SEARCH_SUB
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_LG_REQDOC_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_LG_REQDOC_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTLGDOC'
         EXPORTING
              ZFBLNO = ZTLG-ZFBLNO
         EXCEPTIONS
              OTHERS = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'L/G Document'
                            ZTLG-ZFBLNO ''
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTLGDOC'
         EXPORTING
              ZFBLNO = ZTLG-ZFBLNO.
  ENDIF.

ENDFORM.                    " P2000_SET_LG_REQDOC_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_LG_DOC_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_LG_DOC_STATUS_CHECK.

  CASE W_STATUS.
    WHEN C_REQ_U.          " º¯?
      PERFORM  P2000_LG_CHANGE_CHECK.
    WHEN C_REQ_D.          " Á¶?
      PERFORM  P2000_LG_DISPLAY_CHECK.
    WHEN C_OPEN_C.         " È®?
      PERFORM  P2000_LG_OPEN_CHECK.
  ENDCASE.

ENDFORM.                    " P2000_LG_DOC_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_LOCK.

  CASE SY-TCODE.
    WHEN 'ZIMB2' OR 'ZIMB3' OR 'ZIMB4'.
      PERFORM P2000_SET_BL_INSDOC_LOCK    USING    'L'.
    WHEN 'ZIM22' OR 'ZIM23' OR 'ZIM221' OR 'ZIM222'  " B/L
                 OR 'ZIM223'.
      PERFORM P2000_SET_BL_REQDOC_LOCK    USING    'L'.
    WHEN 'ZIM27' OR 'ZIM28' OR 'ZIM29'.            ">>L/G
      PERFORM P2000_SET_LG_REQDOC_LOCK    USING    'L'.
    WHEN 'ZIM31' OR 'ZIM32' OR 'ZIM33' OR 'ZIM34' OR ">>Åë°ü¿äÃ».
       'ZIM31L' OR 'ZIM32L' OR 'ZIM33L' OR 'ZIM34L'. ">>ÀÔ°í¿äÃ».
      PERFORM P2000_SET_IV_DOC_LOCK       USING    'L'.
    WHEN 'ZIM35' OR 'ZIM36' OR 'ZIM37' OR 'ZIM38' OR ">>CIV
         'ZIM35L' OR 'ZIM36L' OR 'ZIM37L' OR 'ZIM38L'. ">>LOCAL ¹°´ë.
      PERFORM P2000_SET_CIV_LOCK          USING    'L'.
    WHEN 'ZIM81' OR 'ZIM82' OR 'ZIM83'.            ">>ÇÏ¿ª°ü¸®.
      PERFORM P2000_SET_ZTCGHD_LOCK          USING    'L'.
    WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.            ">>¹ÝÀÔ¿¹Á¤Á¤º¸.
      PERFORM P2000_SET_BLINOU_LOCK       USING    'L'.
    WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9'.            ">>¹ÝÀÔ.
      PERFORM P2000_SET_BLINR_LOCK        USING    'L'.
    WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4'. ">>¹ÝÃâ.
      PERFORM P2000_SET_BLOUR_LOCK        USING    'L'.
    WHEN 'ZIMP2' OR 'ZIMP3' OR 'ZIMP4'.            ">>PAYMENT NOTICE.
      PERFORM P2000_SET_ZTPMTHD_LOCK        USING    'L'.
    WHEN 'ZIM62' OR 'ZIM63' OR 'ZIM6D'.            ">>¼öÀÔ½Å°í.
      PERFORM   P2000_SET_ZTIDR_LOCK USING    'L'.
    WHEN 'ZIM75' OR 'ZIM76'.                       ">>¼öÀÔ¸éÇã.
      PERFORM   P2000_SET_ZTIDS_LOCK USING    'L'.
    WHEN 'ZIM64' OR 'ZIM65' OR 'ZIM77'.            ">>°ü¼¼°¨¸é.
      PERFORM   P2000_SET_ZTIDRCR_LOCK   USING   'L'.
    WHEN 'ZIM67' OR 'ZIM68' OR 'ZIM69'.            ">>°ú¼¼Åë°ü INVOICE.
      PERFORM   P2000_SET_ZTCUCLIV_LOCK USING 'L'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_LG_DISPLAY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_LG_DISPLAY_CHECK.
* ¹®¼­ »ó?
  CASE ZTLG-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTLG-ZFEDIST = 'S'.
        MESSAGE I998 WITH ZTLG-ZFBLNO 'EDI open requested'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTLG-ZFEDIST = 'R'.
        MESSAGE I998 WITH ZTLG-ZFBLNO 'EDI opened'.
      ELSE.
        MESSAGE I998 WITH ZTLG-ZFBLNO 'Non-EDI opened'.
      ENDIF.
    WHEN 'C'.          " CANCEL
      MESSAGE I998 WITH ZTLG-ZFBLNO 'CALCELED'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_LG_DISPLAY_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_LG_CHANGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_LG_CHANGE_CHECK.
* ¹®¼­ »ó?
  CASE ZTLG-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTREQST-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTLG-ZFBLNO 'EDI open requested' 'Change'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTREQST-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTLG-ZFBLNO 'EDI opened' 'Change'.
      ELSE.
        MESSAGE E999 WITH ZTLG-ZFBLNO 'Non-EDI opened' 'Change'.
      ENDIF.
    WHEN 'C'.          " CANCEL
      MESSAGE E999 WITH ZTLG-ZFBLNO 'CALCELED' 'Change'.
    WHEN 'A'.          " AMEND
      MESSAGE E999 WITH ZTLG-ZFBLNO 'Amended' 'Change'.
  ENDCASE.

ENDFORM.                    " P2000_LG_CHANGE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_LG_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_LG_DOC_DISPLAY USING    P_ZFBLNO.
* B/L °ü¸®¹ø?
  IF P_ZFBLNO IS INITIAL.
    MESSAGE E316.
  ENDIF.
* SELECT
  SELECT SINGLE * FROM ZTLG WHERE ZFBLNO EQ P_ZFBLNO.
  IF SY-SUBRC NE 0.
    MESSAGE E053  WITH P_ZFBLNO.
  ENDIF.

  SET PARAMETER ID 'ZPBLNO' FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  EXPORT 'ZPBLNO'    TO MEMORY ID 'ZPBLNO'.
  EXPORT 'ZPHBLNO'   TO MEMORY ID 'ZPHBLNO'.

  CALL TRANSACTION 'ZIM28' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_LG_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_PO_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_PO_DOC_DISPLAY USING    P_EBELN  P_EBELP.
  IF P_EBELN IS INITIAL.
    MESSAGE E003.
  ENDIF.

  SELECT SINGLE * FROM EKKO
         WHERE    EBELN  EQ   P_EBELN.

  CASE EKKO-BSTYP.
    WHEN  'L'.   ">³³Ç°ÀÏÁ¤°èÈ¹.
      SET PARAMETER ID 'SAG' FIELD P_EBELN.
      CALL TRANSACTION 'ME33L' AND SKIP  FIRST SCREEN.
    WHEN  'K'.   ">°è¾à.
      SET PARAMETER ID 'CTR' FIELD P_EBELN.
      CALL TRANSACTION 'ME33K' AND SKIP  FIRST SCREEN.
    WHEN  OTHERS.
      SET PARAMETER ID 'BSP' FIELD P_EBELP.
      SET PARAMETER ID 'BES' FIELD P_EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
  ENDCASE.

ENDFORM.                    " P2000_PO_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIV_MODIFY_SCR5710
*&---------------------------------------------------------------------*
FORM P3000_ZTIV_MODIFY_SCR5710.

  DESCRIBE TABLE IT_ZSIVIT LINES LINE.
  IF LINE = 0.
    MESSAGE E726.
  ENDIF.

  IF SY-TCODE = 'ZIM57'.
    DELETE FROM ZTIVIT
     WHERE ZFIVNO = ZTIV-ZFIVNO.

    LOOP AT IT_ZSIVIT.
      CLEAR : ZTIVIT.
      MOVE-CORRESPONDING IT_ZSIVIT   TO ZTIVIT.
      INSERT   ZTIVIT.
      IF SY-SUBRC NE 0.
        MESSAGE  E722.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
  MOVE SY-UNAME TO ZTIV-UNAM.
  MOVE SY-DATUM TO ZTIV-UDAT.
  UPDATE ZTIV.
  IF SY-SUBRC NE  0.
    MESSAGE  E722.
  ELSE.
    MESSAGE  S725  WITH  ZTIV-ZFIVNO.
  ENDIF.

ENDFORM.                    " P3000_ZTIV_MODIFY_SCR5710
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CHARGE_RECORD
*&---------------------------------------------------------------------*
FORM P1000_READ_CHARGE_RECORD USING    P_VIA
                                       GUBUN.
*>> B/L Expense Use
  IF ZTIMIMG00-ZFPSMS  EQ '2' AND
     ZTIMIMG00-BLCSTMD NE 'X'.
    EXIT.
  ENDIF.

  IF P_VIA EQ 'AIR'.
    WZ_VIA  =  'A'.
  ELSEIF P_VIA EQ 'VSL'.
    WZ_VIA  =  'O'.
  ENDIF.

  IF GUBUN EQ 'O' OR GUBUN EQ 'A'.
    REFRESH : IT_ZSIMIMG08.
    DELETE IT_ZSBLCST WHERE ZFACDO EQ SPACE.

    CLEAR : W_LFA1, W_ZFCSQ.
    IF ZTIMIMG11-ZFVNCT IS INITIAL.
      IF NOT ZTBL-ZFFORD IS INITIAL.
        SELECT SINGLE * INTO W_LFA1 FROM LFA1
                        WHERE LIFNR EQ ZTBL-ZFFORD.
      ENDIF.
    ELSE.
      IF NOT ZTBL-ZFHAYEK IS INITIAL.
        SELECT SINGLE * INTO W_LFA1 FROM LFA1
                        WHERE LIFNR EQ ZTBL-ZFHAYEK.
      ENDIF.
    ENDIF.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG08
                                          FROM ZTIMIMG08
                                          WHERE ZFCDTY EQ '004'
                                          AND ( ZFCD4  EQ WZ_VIA OR
                                                ZFCD4  EQ 'B' )
                                          ORDER BY ZFCD2.
    W_ZFCSQ = 10000.
    LOOP AT IT_ZSIMIMG08.
      CLEAR : IT_ZSBLCST.
      READ TABLE IT_ZSBLCST WITH KEY
                            ZFCSCD = IT_ZSIMIMG08-ZFCD.
      IF SY-SUBRC EQ 0. CONTINUE. ENDIF.

      IF W_ZFCSQ = 0.
        W_ZFCSQ = 10010.
      ELSE.
        W_ZFCSQ = W_ZFCSQ + 10.
      ENDIF.

      READ TABLE IT_ZSBLCST WITH KEY ZFCSQ = W_ZFCSQ.
      IF SY-SUBRC EQ 0. CONTINUE. ENDIF.

      IT_ZSBLCST-ZFCSQ    =    W_ZFCSQ.
      IT_ZSBLCST-BUKRS    =    ZTBL-BUKRS.
      IT_ZSBLCST-ZFBLNO   =    ZTBL-ZFBLNO.
      IT_ZSBLCST-ZFCSCD   =    IT_ZSIMIMG08-ZFCD.
      IT_ZSBLCST-ZFCDNM   =    IT_ZSIMIMG08-ZFCDNM.
      IT_ZSBLCST-ZFAOCD   =    IT_ZSIMIMG08-ZFCD1.
      IT_ZSBLCST-ZFCD2    =    IT_ZSIMIMG08-ZFCD2.
      IT_ZSBLCST-KRW      =    T001-WAERS.
      IT_ZSBLCST-ZFWERKS  =    ZTBL-ZFWERKS.
      IF IT_ZSIMIMG08-ZFCURR EQ 'B'.
        MOVE : ZTBL-ZFTRCUR        TO    IT_ZSBLCST-WAERS.
      ELSEIF IT_ZSIMIMG08-ZFCURR EQ 'K'.
        MOVE : T001-WAERS          TO    IT_ZSBLCST-WAERS.
      ENDIF.
      IT_ZSBLCST-ZFOCDT   =    SY-DATUM.

*----> Partner Function >>>>>
      IF ZTIMIMG11-ZFVNCT IS INITIAL.   " Forwarder
        IT_ZSBLCST-ZFVEN    =    ZTBL-ZFFORD.
        IF W_LFA1-LNRZA IS INITIAL.
          IT_ZSBLCST-ZFPAY = ZTBL-ZFFORD.
        ELSE.
          IT_ZSBLCST-ZFPAY =    W_LFA1-LNRZA.
        ENDIF.
      ELSE.
        IT_ZSBLCST-ZFVEN    =    ZTBL-ZFHAYEK.
        IF W_LFA1-LNRZA IS INITIAL.
          IT_ZSBLCST-ZFPAY = ZTBL-ZFHAYEK.
        ELSE.
          IT_ZSBLCST-ZFPAY =    W_LFA1-LNRZA.
        ENDIF.
      ENDIF.

      SELECT SINGLE ZTERM INTO IT_ZSBLCST-ZTERM   " Payment Term
      FROM   LFB1
      WHERE  LIFNR EQ IT_ZSBLCST-ZFVEN
      AND    BUKRS EQ IT_ZSBLCST-BUKRS.

      APPEND IT_ZSBLCST .
    ENDLOOP.
    SORT IT_ZSBLCST BY ZFCD2.
  ENDIF.

*-----------------------------------------------------------------------
* B/L º¸¼¼ ¿î¼Û.
  IF GUBUN EQ 'L' OR GUBUN EQ 'A'.
    REFRESH : IT_ZSIMIMG08.
    DELETE IT_ZSBLCST1 WHERE ZFACDO EQ SPACE.

    CLEAR : W_LFA1.
    IF ZTIMIMG11-ZFVNCT IS INITIAL.
      IF NOT ZTBL-ZFTRCK IS INITIAL.
        SELECT SINGLE * INTO W_LFA1 FROM LFA1
                        WHERE LIFNR EQ ZTBL-ZFTRCK.
      ENDIF.
    ELSE.
      IF NOT ZTBL-ZFHAYEK IS INITIAL.
        SELECT SINGLE * INTO W_LFA1 FROM LFA1
                        WHERE LIFNR EQ ZTBL-ZFHAYEK.
      ENDIF.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG08
                                          FROM ZTIMIMG08
                                          WHERE ZFCDTY EQ '005'
                                            AND ( ZFCD4  EQ WZ_VIA OR
                                                  ZFCD4  EQ 'B' )
                                          ORDER BY ZFCD2.
    W_ZFCSQ = 0.
    LOOP AT IT_ZSIMIMG08.
      IF IT_ZSIMIMG08-ZFCD = '1AB'.
        CONTINUE.
      ENDIF.
      CLEAR : IT_ZSBLCST1.
      READ TABLE IT_ZSBLCST1 WITH KEY
                             ZFCSCD = IT_ZSIMIMG08-ZFCD.
      IF SY-SUBRC EQ 0. CONTINUE. ENDIF.

      IF W_ZFCSQ = 0.
        W_ZFCSQ = 10.
      ELSE.
        W_ZFCSQ = W_ZFCSQ + 10.
      ENDIF.

      READ TABLE IT_ZSBLCST1 WITH KEY ZFCSQ = W_ZFCSQ.
      IF SY-SUBRC EQ 0. CONTINUE. ENDIF.

      IT_ZSBLCST1-ZFCSQ    =    W_ZFCSQ.
      IT_ZSBLCST1-BUKRS    =    ZTBL-BUKRS.
      IT_ZSBLCST1-ZFBLNO   =    ZTBL-ZFBLNO.
      IT_ZSBLCST1-ZFCSCD   =    IT_ZSIMIMG08-ZFCD.
      IT_ZSBLCST1-ZFCDNM   =    IT_ZSIMIMG08-ZFCDNM.
      IT_ZSBLCST1-ZFAOCD   =    IT_ZSIMIMG08-ZFCD1.
      IT_ZSBLCST1-ZFCD2    =    IT_ZSIMIMG08-ZFCD2.
      IT_ZSBLCST1-KRW      =    T001-WAERS.
      IT_ZSBLCST1-WAERS    =    ZTBL-ZFBLAMC.
      IT_ZSBLCST1-ZFWERKS  =    ZTBL-ZFWERKS.
      IT_ZSBLCST1-WAERS    =    T001-WAERS.
      IT_ZSBLCST1-MWSKZ    =    IT_ZSIMIMG08-ZFCD5.
      IF ZTIMIMG11-ZFVNCT IS INITIAL.
        IT_ZSBLCST1-ZFVEN    =    ZTBL-ZFTRCK.
        IF W_LFA1-LNRZA IS INITIAL.
          IT_ZSBLCST1-ZFPAY = ZTBL-ZFTRCK.
        ELSE.
          IT_ZSBLCST1-ZFPAY = W_LFA1-LNRZA.
        ENDIF.
      ELSE.
        IT_ZSBLCST1-ZFVEN    =    ZTBL-ZFHAYEK.
        IF W_LFA1-LNRZA IS INITIAL.
          IT_ZSBLCST1-ZFPAY = ZTBL-ZFHAYEK.
        ELSE.
          IT_ZSBLCST1-ZFPAY = W_LFA1-LNRZA.
        ENDIF.
      ENDIF.

      SELECT SINGLE ZTERM INTO IT_ZSBLCST1-ZTERM
      FROM   LFB1
      WHERE  LIFNR EQ IT_ZSBLCST1-ZFVEN
      AND    BUKRS EQ IT_ZSBLCST1-BUKRS.
      IF SY-SUBRC NE 0.
        CLEAR  IT_ZSBLCST1-ZTERM.
      ENDIF.
* ===> TAX RATE
      PERFORM   P1000_READ_KONP   USING   IT_ZSBLCST1-MWSKZ
                                          IT_ZSBLCST1-KBETR
                                          IT_ZSBLCST1-KONWA.
      APPEND IT_ZSBLCST1.
    ENDLOOP.
    SORT IT_ZSBLCST1 BY ZFCD2.
  ENDIF.

ENDFORM.                    " P1000_READ_CHARGE_RECORD
*&---------------------------------------------------------------------*
*&      Form  P2000_OPEN_DOC_SELECT
*&---------------------------------------------------------------------*
FORM P2000_OPEN_DOC_SELECT   USING   P_ZFOPNNO.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE  ZFOPNNO   EQ P_ZFOPNNO
*            AND   ZFREQTY    NE 'LO'
*            AND   ZFREQTY    NE 'PU'
*-----------------------------------------------------------------------
           AND   ZFAMDNO EQ '00000'
*            AND    ZFDOCST   EQ 'O'
*-----------------------------------------------------------------------
           ORDER  BY ZFREQNO.

  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.
* PERFORM   P2000_GET_POSITION.
  ANTWORT = 'N'.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'BLCREAT1'.                 " L/C º¯?
  CALL SCREEN 0014 STARTING AT  12 3
                   ENDING   AT  77 15.
* CALL SCREEN 0014 STARTING AT  X1 Y1
*                  ENDING   AT  X2 Y2.


ENDFORM.                    " P2000_OPEN_DOC_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_OPEN_DOC_SELECT1
*&---------------------------------------------------------------------*
FORM P2000_OPEN_DOC_SELECT1   USING P_EBELN.

  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE  EBELN     EQ P_EBELN
*            AND   ZFREQTY NE 'LO'
*            AND   ZFREQTY NE 'PU'
*-----------------------------------------------------------------------
           AND   ZFAMDNO EQ '00000'
* 2000/06/17 ÁÖ¼®Ã³¸®   -  °­³ªÇü ´ë¸® DEFINE...
*            AND    ZFDOCST EQ 'O'
*-----------------------------------------------------------------------
           ORDER  BY ZFREQNO.

  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.   MESSAGE E243 WITH P_EBELN.   ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'BLCREATE'.                 " L/C º¯?

  CALL SCREEN 0014 STARTING AT  08 3
                   ENDING   AT  88 15.

ENDFORM.                    " P2000_OPEN_DOC_SELECT1
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDR_MODIFY_SCR6200
*&---------------------------------------------------------------------*
FORM P3000_ZTIDR_MODIFY_SCR6200.
  DATA : L_ZFIDRNO   LIKE    ZTIDR-ZFIDRNO,
         L_MODE.

  IF W_OK_CODE NE 'DELE'.
* NHJ ÁÖ¼®Ã³¸®. < 2002.08.08>
*    CLEAR  ZTCUCL.
*   SELECT SINGLE * FROM ZTCUCL
*   WHERE  ZFBLNO  = ZTIDR-ZFBLNO
*   AND    ZFCLSEQ = ZTIDR-ZFCLSEQ.
*   IF ZTCUCL-ZFCUST NE '2'. MESSAGE W754. ENDIF.
* NHJ Ãß°¡ < 2002.09. 13 >
    SELECT SINGLE * FROM ZTIV
    WHERE  ZFIVNO   EQ   ZTIDR-ZFIVNO.
    IF ZTIV-ZFCUST NE '2'.  MESSAGE W754.  ENDIF.

    DESCRIBE TABLE IT_ZSIDRHS LINES LINE.
    IF LINE = 0. MESSAGE E762. ENDIF.

    DESCRIBE TABLE IT_ZSIDRHSD LINES LINE.
    IF LINE = 0. MESSAGE E763. ENDIF.
  ENDIF.

  IF W_OK_CODE EQ 'SVCO'.
    L_MODE = 'Y'.
  ELSE.
    L_MODE = 'N'.
  ENDIF.

*>> NHJ 2003.02.17 Åë°üºÎ¼­ ¿ä±¸·Î ÀÎÇÑ ÁÖ¼®Ã³¸®.
**>> EDI »óÅÂ Ã¼Å©.
*  SELECT SINGLE *
*           FROM ZTIMIMGTX
*          WHERE BUKRS = ZTIMIMGTX-BUKRS.
*  IF ZTIMIMGTX-ZFEDIYN EQ 'X' AND ZTIMIMGTX-IMPREQ EQ 'X'.
*
*    CALL FUNCTION 'ZIM_CUDATA_EDI_CHK'
*      EXPORTING
*        MODE        = L_MODE
*      TABLES
*        IT_ZTIDRHS  = IT_ZSIDRHS
*        IT_ZTIDRHSD = IT_ZSIDRHSD
*        IT_ZTIDRHSL = IT_ZSIDRHSL
*      CHANGING
*        ZTIDR       = ZTIDR.
*  ENDIF.

*>> ¼öÀÔ BASIC CONFIG CHECK.
  CLEAR : ZTIMIMG00.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF W_OK_CODE = 'SVCO'.
    IF ZTIMIMG00-ZFCUMTD EQ '1'.
      IF ZTIDR-ZFCUT IS INITIAL.
        MESSAGE  E476.
      ENDIF.
    ENDIF.
    IF ZTIDR-ZFIDWDT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIDR' 'ZFIDWDT'.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZIM_ZTIDR_DOC_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFBLNO          = ZTIDR-ZFBLNO
            ZFCLSEQ         = ZTIDR-ZFCLSEQ
            ZFSTATUS        = W_STATUS
            W_ZTIDR_OLD     = *ZTIDR
            W_ZTIDR         = ZTIDR
       TABLES
            IT_ZSIDRHS_OLD  = IT_ZSIDRHS_ORG
            IT_ZSIDRHS      = IT_ZSIDRHS
            IT_ZSIDRHSD_OLD = IT_ZSIDRHSD_ORG
            IT_ZSIDRHSD     = IT_ZSIDRHSD
            IT_ZSIDRHSL_OLD = IT_ZSIDRHSL_ORG
            IT_ZSIDRHSL     = IT_ZSIDRHSL
       EXCEPTIONS
            ERROR_UPDATE    = 1
            ERROR_DELETE    = 2.

*>>¼öÀÔ¸éÇã HEADER DATA º¯°æ¿©ºÎ CHECK!
*  SELECT  COUNT( DISTINCT ZFIDRNO )  INTO  W_COUNT
*  FROM    ZTIDR
*  WHERE   ZFBLNO  =  ZTIDR-ZFBLNO
*  AND     ZFCLSEQ =  ZTIDR-ZFCLSEQ .

*  IF W_COUNT = 0.
*     MOVE : SY-UNAME  TO   ZTIDR-ERNAM,
*            SY-DATUM  TO   ZTIDR-CDAT,
*            SY-UNAME  TO   ZTIDR-UNAM,
*            SY-DATUM  TO   ZTIDR-UDAT.
*    INSERT ZTIDR.
*  ELSE.
*   IF *ZTIDR NE ZTIDR.
*      MOVE  SY-UNAME TO ZTIDR-UNAM.
*      MOVE  SY-DATUM TO ZTIDR-UDAT.
*      UPDATE  ZTIDR.
*   ENDIF.
* ENDIF.
*
* DESCRIBE TABLE IT_ZSIDRHSD_DEL LINES LINE.
* IF LINE > 0.
* ¼öÀÔ¸éÇã ±Ô°Ý TABLE DELETE.
*     LOOP AT IT_ZSIDRHSD_DEL.
*       DELETE FROM ZTIDRHSD
*              WHERE ZFBLNO   EQ IT_ZSIDRHSD_DEL-ZFBLNO
*              AND   ZFCLSEQ  EQ IT_ZSIDRHSD_DEL-ZFCLSEQ
*              AND   ZFCONO   EQ IT_ZSIDRHSD_DEL-ZFCONO
*               AND   ZFRONO   EQ IT_ZSIDRHSD_DEL-ZFRONO.
*    ENDLOOP.
* ENDIF.
* REFRESH : IT_ZSIDRHSD_DEL.
*>> ¼öÀÔ¸éÇã¶õ »çÇ× TABLE CHECK!
* LOOP AT IT_ZSIDRHS.
*   READ TABLE IT_ZSIDRHS_ORG WITH KEY
*                             ZFBLNO  = IT_ZSIDRHS-ZFBLNO
*                             ZFCLSEQ = IT_ZSIDRHS-ZFCLSEQ
*                             ZFCONO  = IT_ZSIDRHS-ZFCONO.
*   IF SY-SUBRC NE 0.
*      MOVE-CORRESPONDING IT_ZSIDRHS  TO  ZTIDRHS.
*      INSERT  ZTIDRHS.
*   ELSE.
*      IF  IT_ZSIDRHS_ORG NE IT_ZSIDRHS.
*          MOVE-CORRESPONDING IT_ZSIDRHS  TO  ZTIDRHS.
*          UPDATE  ZTIDRHS.
*       ENDIF.
*    ENDIF.
* ENDLOOP.
*>> ¼öÀÔ¸éÇã±Ô°Ý TABLE CHECK!
* LOOP AT IT_ZSIDRHSD.
*   READ TABLE IT_ZSIDRHSD_ORG WITH KEY
*                              ZFBLNO  = IT_ZSIDRHSD-ZFBLNO
*                              ZFCLSEQ = IT_ZSIDRHSD-ZFCLSEQ
*                              ZFCONO  = IT_ZSIDRHSD-ZFCONO
*                              ZFRONO  = IT_ZSIDRHSD-ZFRONO.
*    IF SY-SUBRC NE 0.
*      MOVE-CORRESPONDING IT_ZSIDRHSD  TO  ZTIDRHSD.
*      INSERT  ZTIDRHSD.
*   ELSE.
*     IF  IT_ZSIDRHSD_ORG NE IT_ZSIDRHSD.
*         MOVE-CORRESPONDING IT_ZSIDRHSD  TO  ZTIDRHSD.
*         UPDATE  ZTIDRHSD.
*     ENDIF.
*   ENDIF.
* ENDLOOP.
*>> ¼öÀÔ¸éÇã¿ä°Ç È®ÀÎ TABLE CHECK!
* LOOP AT IT_ZSIDRHSL.
*   READ TABLE IT_ZSIDRHSL_ORG WITH KEY
*                       ZFBLNO  = IT_ZSIDRHSL-ZFBLNO
*                       ZFCLSEQ = IT_ZSIDRHSL-ZFCLSEQ
*                       ZFCONO  = IT_ZSIDRHSL-ZFCONO
*                       ZFCNDC  = IT_ZSIDRHSL-ZFCNDC
*                       ZFCNNO  = IT_ZSIDRHSL-ZFCNNO.
*   IF SY-SUBRC NE 0.
*      MOVE-CORRESPONDING IT_ZSIDRHSL  TO  ZTIDRHSL.
*      INSERT  ZTIDRHSL.
*   ELSE.
*      IF IT_ZSIDRHSL_ORG NE IT_ZSIDRHSL.
*         MOVE-CORRESPONDING IT_ZSIDRHSL  TO  ZTIDRHSL.
*         UPDATE  ZTIDRHSL.
*      ENDIF.
*   ENDIF.
*  ENDLOOP.
*
* IF W_OK_CODE = 'SVCO'.
*    IF ZTIDR-ZFCUT IS INITIAL.
*       MESSAGE  E476.
*    ENDIF.
*
*    MOVE ZTIDR-ZFIDWDT+0(4) TO W_YYYY.
*    CONCATENATE W_YYYY '0101' INTO W_YYYYMMDD_FROM.
*    CONCATENATE W_YYYY '1231' INTO W_YYYYMMDD_TO.
*    CLEAR W_ZFIDRNO.
*
*    CONCATENATE '_____' SY-DATUM+2(2) '8______' INTO L_ZFIDRNO.
*--------------> INFOLINKE DREAMKSB <-----------------------------------
*---> 2000/10/02 ¾È´ö±â °úÀå ¿äÃ» ( 800001 ÀÌÈÄ ¹øÈ£·Î Ã¤¹ø )
*---> ¿¬µµº°?
*                          AND ZFIDRNO LIKE '_______8_____'
*    SELECT MAX( ZFIDRNO ) INTO W_ZFIDRNO FROM ZTIDR
*                          WHERE ZFCUT = ZTIDR-ZFCUT
*                          AND ZFIDRNO LIKE L_ZFIDRNO
*                          AND ZFIDWDT >= W_YYYYMMDD_FROM
*                          AND ZFIDWDT <= W_YYYYMMDD_TO
*                          AND ( ZFBLNO NE ZTIDR-ZFBLNO
*                             OR ZFCLSEQ NE ZTIDR-ZFCLSEQ ).
*    IF W_ZFIDRNO IS INITIAL.
*       MOVE ZTIDR-ZFIDWDT+2(2) TO W_YEAR.
*       MOVE '800000'           TO W_SEQ.
*     ELSE.
*       MOVE W_ZFIDRNO+5(2)     TO W_YEAR.
*       MOVE W_ZFIDRNO+7(6)     TO W_SEQ.
*    ENDIF.
*    ADD  1                     TO W_SEQ.  " ¼öÀÔ½Å°í¹ø?
*    W_TMP = ZTIDR-ZFCUT+0(1) * 7 + ZTIDR-ZFCUT+1(1) * 3
*          + ZTIDR-ZFCUT+2(1) * 1 + ZTIDR-ZFCUT+3(1) * 7
*           + ZTIDR-ZFCUT+4(1) * 3
*          + W_YEAR+0(1) * 1 + W_YEAR+1(1) * 7
*          + W_SEQ+0(1) * 3 + W_SEQ+1(1) * 1
*          + W_SEQ+2(1) * 7 + W_SEQ+3(1) * 3
*          + W_SEQ+4(1) * 1 + W_SEQ+5(1) * 7.
*    W_TMP_1 = W_TMP MOD 10.
*    W_CHK   = 10 - W_TMP_1.
*    CONCATENATE ZTIDR-ZFCUT W_YEAR W_SEQ W_CHK INTO ZTIDR-ZFIDRNO.
*    CONCATENATE ZTIDR-ZFREBELN
*                W_YEAR W_SEQ INTO ZTIDR-ZFIMCR. " ¹«¿ª¾÷Ã¼ÂüÁ¶¹ø?
*
*    CLEAR ZTIDR-ZFNSCD.
*    MOVE SY-UNAME TO ZTIDR-UNAM.
*    MOVE SY-DATUM TO ZTIDR-UDAT.
*    UPDATE ZTIDR.
*    IF SY-SUBRC NE  0.  MESSAGE  E764. ENDIF.
*
*>> Åë°ü TABLE ÀÇ »óÅÂº¯°æ¹× ¼öÀÔ½Å°í ¹øÈ£ SETTING!
*    MOVE  '3'            TO  ZTCUCL-ZFCUST.
*    MOVE  ZTIDR-ZFIDRNO  TO  ZTCUCL-ZFIDRNO.
*     MOVE  ZTIDR-ZFIDWDT  TO  ZTCUCL-ZFIDWDT.
*    MOVE  SY-UNAME       TO  ZTCUCL-UNAM.
*    MOVE  SY-DATUM       TO  ZTCUCL-UDAT.
*    UPDATE ZTCUCL.
*    IF SY-SUBRC NE  0. MESSAGE  E764. ENDIF.
*>> Åë°ü¿äÃ» INVOICE »óÅÂº¯°æ.
*    UPDATE  ZTCUCLIV
*    SET     ZFCUST   =  '3'
*            UNAM     =  SY-UNAME
*            UDAT     =  SY-DATUM
*    WHERE   ZFBLNO   =  ZTIDR-ZFBLNO
*    AND     ZFCLSEQ  =  ZTIDR-ZFCLSEQ .
*    IF SY-SUBRC NE 0. MESSAGE  E764. ENDIF.
*
*>> INVOICE TABLE »óÅÂº¯°æ.
*   SELECT  *  FROM  ZTCUCLIV
*   WHERE      ZFBLNO  =  ZTIDR-ZFBLNO
*   AND        ZFCLSEQ =  ZTIDR-ZFCLSEQ.
*
*      UPDATE  ZTIV
*      SET     ZFCUST  =  '3'
*              UNAM    =  SY-UNAME
*              UDAT    =  SY-DATUM
*      WHERE   ZFIVNO  =  ZTCUCLIV-ZFIVNO.
*      IF SY-SUBRC NE 0. MESSAGE  E764. ENDIF.
*   ENDSELECT.
* ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE  E764.
  ELSE.
    SET PARAMETER ID 'ZPIDRNO' FIELD ZTIDR-ZFIDRNO.
    MESSAGE  S765.
  ENDIF.

ENDFORM.                    " P3000_ZTIDR_MODIFY_SCR6200
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_EXCHANGE_RATE
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_EXCHANGE_RATE.

  SPOP-TITEL = 'Exchange rate confirmation'.
*   SPOP-TEXTLINE1 = 'Not inputed Exchange rate'.
  SPOP-TEXTLINE2 ='Do you want to reflect the exchange rate of system?'.
  CLEAR : CANCEL_OPTION.
  OPTION = 1.
  TEXTLEN = 60.

*>> ÅëÈ­À¯Çü...
  IF V_TCURR-KURST IS INITIAL.
    V_TCURR-KURST = 'M'.
  ENDIF.

  CALL SCREEN 0109 STARTING AT 20 6
                   ENDING   AT 80 11.

*   IF ANTWORT = 'C'.       " Cancel
*      SET SCREEN SY-DYNNR.
*   ENDIF.

ENDFORM.                    " P2000_MESSAGE_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_CONFIRM_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SAVE_CONFIRM_MESSAGE.
  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING 'ÀúÀå È®ÀÎ'             " Å¸ÀÌÆ²...
                                   'ÀÔ·ÂµÈ ³»¿ªÀ» È®ÀÎÇÏ°í ÀúÀåÇÕ´Ï´Ù.'
                                        'ÀúÀåÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                                    'Y'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '1'.                 " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Save confirm'
             'Do save with confirm sign.'
             'Do you want to save?'
             'Y'
             '1'.
  ENDCASE.


ENDFORM.                    " P2000_SAVE_CONFIRM_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_REOG_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_REOG_MESSAGE.
  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING 'ÀúÀå È®ÀÎ'             " Å¸ÀÌÆ²...
                                         '¶õÀ» Àç±¸¼ºÇÏ°í  ÀúÀåÇÕ´Ï´Ù.'
                                        'ÀúÀåÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                                    'Y'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '1'.                 " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Save confirm'
             'Do restructure column and save.'
             'Do you want to save?'
             'Y'
             '1'.
  ENDCASE.


ENDFORM.                    " P2000_REOG_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDS_MODIFY_SCR7410
*&---------------------------------------------------------------------*
FORM P3000_ZTIDS_MODIFY_SCR7410.

  IF W_OK_CODE EQ 'DELE'.
    CLEAR : W_COUNT.
    SELECT COUNT( DISTINCT BELNR ) INTO W_COUNT
             FROM   ZVIMCOST
             WHERE  ZFCSTGRP EQ '006'
             AND    ZFIMDNO  EQ ZTIV-ZFIVNO
             AND    ZFPOSYN  NE 'N'.
    IF W_COUNT GT 0.
      MESSAGE E406(ZIM1).
    ENDIF.
  ELSE.
    DESCRIBE TABLE IT_ZSIDSHS LINES LINE.
    IF LINE = 0. MESSAGE E762. ENDIF.

    DESCRIBE TABLE IT_ZSIDSHSD LINES LINE.
    IF LINE = 0. MESSAGE E763. ENDIF.


    SELECT SINGLE * FROM  ZTIMIMG02
                    WHERE ZFCOTM EQ ZTIDS-ZFINRC.

    CALL FUNCTION 'FI_VENDOR_DATA'
         EXPORTING
              I_BUKRS        = ZTIDS-BUKRS
              I_LIFNR        = ZTIMIMG02-ZFVEN
         IMPORTING
              E_KRED         = VF_KRED
         EXCEPTIONS
              VENDOR_MISSING = 4.

    IF SY-SUBRC NE 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF ZTIDS-ZFCUAMTS IS INITIAL.
      MESSAGE W167 WITH 'Total customs'.
    ENDIF.
    IF ZTIDS-ZFVAAMTS IS INITIAL.
      MESSAGE W167 WITH 'Total VAT'.
    ENDIF.
* NHJ ÁÖ¼® Ã³¸®. CORECESS
*    IF ZTIDS-ZFCUTAMT IS INITIAL.
*      MESSAGE W167 WITH 'Clearance commission'.
*    ENDIF.
    IF ZTIDS-ZFTBAK IS INITIAL.
      MESSAGE W167 WITH 'Taxable price(KWD)'.
    ENDIF.
    IF ZTIDS-FWBAS IS INITIAL.
      MESSAGE W167 WITH 'Taxable standard price'.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZIM_ZTIDS_DOC_MODIFY'
       EXPORTING
            W_OK_CODE        = W_OK_CODE
            ZFBLNO           = ZTIDS-ZFBLNO
            ZFCLSEQ          = ZTIDS-ZFCLSEQ
            ZFSTATUS         = W_STATUS
            W_ZTIDS_OLD      = *ZTIDS
            W_ZTIDS          = ZTIDS
       TABLES
            IT_ZSCUCLCST_OLD = IT_ZSCUCLCST_ORG
            IT_ZSCUCLCST     = IT_ZSCUCLCST
            IT_ZSIDSHS_OLD   = IT_ZSIDSHS_ORG
            IT_ZSIDSHS       = IT_ZSIDSHS
            IT_ZSIDSHSD_OLD  = IT_ZSIDSHSD_ORG
            IT_ZSIDSHSD      = IT_ZSIDSHSD
            IT_ZSIDSHSL_OLD  = IT_ZSIDSHSL_ORG
            IT_ZSIDSHSL      = IT_ZSIDSHSL
       EXCEPTIONS
            ERROR_UPDATE     = 1
            ERROR_DELETE     = 2
            ERROR_INSERT     = 3.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    MESSAGE  S765.
  ELSE.
    ROLLBACK WORK.
    MESSAGE  E952.
  ENDIF.

ENDFORM.                    " P3000_ZTIDS_MODIFY_SCR7410

*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_LISTING
*&---------------------------------------------------------------------*
FORM P2000_DATA_LISTING.

  CASE INCLUDE.
    WHEN 'POPU'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE(96), /         SY-VLINE NO-GAP,
                'Type'          NO-GAP, SY-VLINE NO-GAP,
                'Message Text',      94 SY-VLINE NO-GAP,
                'T'             NO-GAP, SY-VLINE,
              / SY-ULINE(96).
      LOOP AT IT_ERR_LIST.
        W_MOD  =  SY-TABIX MOD 2.
        FORMAT RESET.
        IF W_MOD EQ 0.
          FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
        ELSE.
          FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
        ENDIF.
        WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)     NO-GAP,
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

        WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
        HIDE:IT_ERR_LIST.
      ENDLOOP.
      WRITE : / SY-ULINE(96).
      CLEAR : IT_ERR_LIST.
    WHEN 'CIVPO' OR 'CIVRQ' OR 'CIVNO'.   ">Gross Price
      LOOP AT IT_ZSCIVHD.
        W_MOD = SY-TABIX MOD 2.
        PERFORM P2000_ZTCIVHD_LOCAL_LIST.
        HIDE IT_ZSCIVHD.
      ENDLOOP.
      WRITE : / SY-ULINE(70).
      CLEAR : IT_ZSCIVHD.
    WHEN 'LOGR' OR 'LOGR1'.               ">G/R
      LOOP AT IT_ZSIV.
        W_MOD = SY-TABIX MOD 2.
        PERFORM P2000_ZTIV_LOCAL_LIST.
        HIDE IT_ZSIV.
      ENDLOOP.
      WRITE : / SY-ULINE(73).
      CLEAR : IT_ZSIV.
    WHEN 'CCHBL' OR 'CCBL'.            "> Clerance Dup.(HOUSE B/L Input)
      LOOP AT IT_ZSIV.
        W_MOD = SY-TABIX MOD 2.
        PERFORM P2000_ZTIV_DUP_LIST_1.
        HIDE IT_ZSIV.
      ENDLOOP.
      WRITE : / SY-ULINE(73).
      CLEAR : IT_ZSIV.
    WHEN 'CGLIST'.                    "> CARGO WORK.
      LOOP AT IT_ZSCGHD.
        W_MOD = SY-TABIX MOD 2.
        PERFORM P2000_ZTCGHD_DUP_LIST.
        HIDE IT_ZSCGHD.
      ENDLOOP.
      WRITE : / SY-ULINE(73).
    WHEN 'CGLIST1'.                   "> CARGO WORK.
      LOOP AT IT_ZSCGHD.
        W_MOD = SY-TABIX MOD 2.
        PERFORM P2000_ZTCGHD_PORT_LIST.
        HIDE IT_ZSCGHD.
      ENDLOOP.
      WRITE : / SY-ULINE(79).
    WHEN 'ZTLG'.             " L/G
      LOOP AT IT_ZTLG.
        W_MOD = SY-TABIX MOD 2.
        PERFORM P2000_ZTLG_DUP_LIST.
        HIDE IT_ZTLG.
      ENDLOOP.
    WHEN 'ZTINS'.             " Insuarance.
      LOOP AT IT_ZTINS.
        W_MOD = SY-TABIX MOD 2.
        PERFORM P2000_ZTINS_DUP_LIST.
        HIDE IT_ZTINS.
      ENDLOOP.
    WHEN 'COMMIV'.           " COMMERCIAL INVOICE
      LOOP AT IT_ZSCIVHD.
        W_MOD = SY-TABIX MOD 2.
        PERFORM P2000_ZTCIVHD_DUP_LIST.
        HIDE IT_ZSCIVHD.
      ENDLOOP.
    WHEN 'BLINOUCG'.         " ¹ÝÀÔ¿¹Á¤Á¤?

      LOOP AT IT_ZSBLINOU.
        W_MOD = SY-TABIX MOD 2.
        CASE INCLUDE.
          WHEN 'BLINOUCG'.    " ¹ÝÀÔ¿¹Á¤Á¤?
            PERFORM   P2000_INOU_DUP_LIST.
        ENDCASE.
        HIDE:IT_ZSBLINOU.
      ENDLOOP.
      CLEAR : IT_ZSBLINOU.
    WHEN 'ZTBLINRD'.            " ¹ÝÀÔÁ¤?
      LOOP AT IT_ZTBLINR.
        W_MOD = SY-TABIX MOD 2.
        CASE INCLUDE.
          WHEN 'ZTBLINRD'.    " ¹ÝÀÔÁ¤?
            PERFORM   P2000_INR_DUP_LIST.
        ENDCASE.
        HIDE : IT_ZTBLINR.
      ENDLOOP.
      CLEAR : IT_ZTBLINR.
    WHEN 'ZTBLOURD'.            " ¹ÝÃâÁ¤?
      LOOP AT IT_ZTBLOUR.
        W_MOD = SY-TABIX MOD 2.
        CASE INCLUDE.
          WHEN 'ZTBLOURD'.    " ¹ÝÃâÁ¤?
            PERFORM   P2000_OUR_DUP_LIST.
        ENDCASE.
        HIDE : IT_ZTBLOUR.
      ENDLOOP.
      CLEAR : IT_ZTBLOUR.
    WHEN 'MSNMFIND'.
      LOOP AT IT_MSNM.
        W_MOD = SY-TABIX MOD 2.
        PERFORM   P2000_MSNM_LIST.
        HIDE : IT_MSNM.
      ENDLOOP.
      CLEAR : IT_MSNM.

    WHEN OTHERS.
      LOOP AT IT_ZSREQHD.
        W_MOD = SY-TABIX MOD 2.
        CASE INCLUDE.
          WHEN 'BLCREATE'.    " BL   »ý¼º½Ã ( P/O¹øÈ£ Áßº¹½Ã )
            PERFORM   P2000_BL_CREATE_LIST.
          WHEN 'BLCREAT1'.    " BL   »ý¼º½Ã (¹®¼­¹øÈ£ Áßº¹½Ã )
            PERFORM   P2000_BL_CREATE_LIST_1.
          WHEN 'LGCREATE'.    " L/G  »ý¼º½Ã ( B/L¹øÈ£ Áßº¹½Ã )
            PERFORM   P2000_LG_CREATE_LIST.
          WHEN 'INCREATE'.    " º¸Çè »ý¼º½Ã( B/L¹øÈ£ Áßº¹½Ã )
            PERFORM   P2000_LG_CREATE_LIST.
        ENDCASE.
        HIDE:IT_ZSREQHD.
      ENDLOOP.
      CLEAR : IT_ZSREQHD.

  ENDCASE.

ENDFORM.                    " P2000_DATA_LISTING

*&---------------------------------------------------------------------*
*&      Form  P2000_BL_CREATE_LIST
*&---------------------------------------------------------------------*
FORM P2000_BL_CREATE_LIST.

  WRITE : / IT_ZSREQHD-ZFREQNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
* WRITE : IT_ZSREQHD-EBELN   NO-GAP,    SY-VLINE NO-GAP,
  WRITE : IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-LIFNR   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-NAME1   NO-GAP, 46 SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFOPNNO NO-GAP.

ENDFORM.                    " P2000_BL_CREATE_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_CREATE_LIST_1
*&---------------------------------------------------------------------*
FORM P2000_BL_CREATE_LIST_1.

  WRITE : / IT_ZSREQHD-ZFREQNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-EBELN   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-LIFNR   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-NAME1   NO-GAP.

ENDFORM.                    " P2000_BL_CREATE_LIST_1
*&---------------------------------------------------------------------*
*&      Form  P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------*
FORM P2000_SEARCH_FIELD_MOVE.

  ZSREQHD-ZFREQNO = W_ZFREQNO.
  ZSREQHD-ZFAMDNO = W_ZFAMDNO.
  ZSREQHD-ZFOPNNO = W_ZFOPNNO.
  ZSREQHD-EBELN   = W_EBELN.
  ZSREQHD-ZFBLNO  = W_BLNO.
  ZSREQHD-ZFTBLNO = W_TBLNO.
  ZSREQHD-ZFHBLNO = W_HBLNO.
  ZSCIVHD-ZFCIVRN = W_ZFCIVRN.
*  ZSCIVHD-ZFCIVNO = W_ZFCIVNO.
  ZSCGHD-ZFCGNO   = W_ZFCGNO.
  ZSIV-ZFIVNO     = W_ZFIVNO.

  ZSCIVHD-ZFREQNO = W_ZFREQNO.
  ZSCIVHD-ZFOPNNO = W_ZFOPNNO.
  ZSCIVHD-EBELN   = W_EBELN.
  ZSCIVHD-ZFCIVRN = W_ZFCIVRN.

ENDFORM.                    " P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_LG_CREATE_LIST
*&---------------------------------------------------------------------*
FORM P2000_LG_CREATE_LIST.

  WRITE : / IT_ZSREQHD-ZFBLNO  NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFBLAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFCARC  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFSPRT(12)  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFAPPC  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFAPRT(12)  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRCVER(10).

ENDFORM.                    " P2000_LG_CREATE_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_LC_DOC_NUM
*&---------------------------------------------------------------------*
FORM P1000_GET_LC_DOC_NUM USING    P_ZFOPNNO.
  CLEAR : W_ZFREQNO, W_ZFAMDNO.

  IF P_ZFOPNNO IS INITIAL.
    MESSAGE E319.
  ENDIF.

  SELECT COUNT( * ) INTO  W_COUNT
                    FROM  ZTREQST
                    WHERE ZFOPNNO EQ P_ZFOPNNO
*-----------------------------------------------------------------------
                    AND   ZFAMDNO EQ '00000'.
*                     AND   ZFDOCST EQ 'O'.
*-----------------------------------------------------------------------
  CASE W_COUNT.
    WHEN 0.     MESSAGE E067 WITH P_ZFOPNNO.
    WHEN 1.
      SELECT * UP TO 1 ROWS
                FROM   ZTREQST
                WHERE  ZFOPNNO EQ P_ZFOPNNO
*                AND   ZFREQTY NE 'LO'
*                AND   ZFREQTY NE 'PU'
*-----------------------------------------------------------------------
               AND   ZFAMDNO EQ '00000'.
* 2000/06/17 ÁÖ¼®Ã³?
*                    AND   ZFDOCST EQ 'O'.
*-----------------------------------------------------------------------
        EXIT.
      ENDSELECT.
      W_ZFREQNO = ZTREQST-ZFREQNO.
      W_ZFAMDNO = ZTREQST-ZFAMDNO.
    WHEN OTHERS.
      PERFORM P2000_OPEN_DOC_SELECT   USING   ZSREQHD-ZFOPNNO.
      IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
      PERFORM P2000_SEARCH_FIELD_MOVE.
  ENDCASE.

  IF SY-TCODE EQ 'ZIM26' OR SY-TCODE EQ 'ZIM27' OR
     SY-TCODE EQ 'ZIM28' OR SY-TCODE EQ 'ZIM29'.
    SELECT SINGLE * FROM ZTREQHD
           WHERE ZFREQNO EQ W_ZFREQNO.
    IF ZTREQHD-ZFREQTY EQ 'PU' OR ZTREQHD-ZFREQTY EQ 'LO' OR
       ZTREQHD-ZFREQTY EQ 'TT'.
*>> 2003.10.14 Add Check Logic for L/C Type 'DA', 'DP'.
*       ZTREQHD-ZFREQTY EQ 'DA' OR ZTREQHD-ZFREQTY EQ 'DP'.
      MESSAGE E635 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
    ENDIF.
  ENDIF.

*>> NHJ ÁÖ¼®Ã³¸®. CORECESS
*>> BL °ü·ÃµÈ LC Á¤º¸ÀÎÁö CHECK!
*  SELECT  COUNT( * )  INTO  W_COUNT1
* FROM    ZTBLIT
* WHERE   ZFREQNO  =  W_ZFREQNO.

*  IF W_COUNT1 = 0.  MESSAGE  E479. ENDIF.

*>> BL °ü·ÃµÈ LC Á¤º¸ÀÎÁö CHECK!
*  SELECT  COUNT( * )  INTO  W_COUNT1
* FROM    ZTBLIT
* WHERE   ZFREQNO  EQ  W_ZFREQNO
* AND     ZFBLNO   EQ  ZTBL-ZFBLNO.

*  IF W_COUNT1 = 0.
*   MESSAGE  E209(ZIM1) WITH ZSREQHD-ZFOPNNO.
* ENDIF.

ENDFORM.                    " P1000_GET_LC_DOC_NUM
*&---------------------------------------------------------------------*
*&      Form  P2000_CUCL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CUCL_MESSAGE.
  CASE SY-LANGU.
    WHEN '3'.
     PERFORM P2000_MESSAGE_BOX USING 'ÀúÀå È®ÀÎ'             " Å¸ÀÌÆ²...
                                        'Åë°ü³»¿ëÀ» ÀúÀåÇÕ´Ï´Ù.'
                                        'ÀúÀåÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                                    'Y'                 " Ãë¼Ò ¹öÆ° À¯/?
                                   '1'.                 " default button
    WHEN OTHERS.
      PERFORM P2000_MESSAGE_BOX USING
             'Save confirm'
             'Now do save the clearance data.'
             'Do you want to save?'
             'Y'
             '1'.
  ENDCASE.

ENDFORM.                    " P2000_CUCL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_INSERT_ZTIDRCR
*&---------------------------------------------------------------------*
FORM P3000_INSERT_ZTIDRCR USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  LOOP AT IT_ZSIDSHS.
    SELECT SINGLE *
      FROM ZTIDSHS
     WHERE ZFBLNO = ZTIDS-ZFBLNO
       AND ZFCLSEQ = ZTIDS-ZFCLSEQ
       AND ZFCONO = IT_ZSIDSHS-ZFCONO.
*    IF ( ZTIDSHS-ZFCDPNO >= 'A00700010001' AND
*         ZTIDSHS-ZFCDPNO <= 'A00790000000' ) OR
*       ( ZTIDSHS-ZFCDPNO >= 'A028070101  ' AND
*         ZTIDSHS-ZFCDPNO <= 'A02807010402' ) OR
*       ( ZTIDSHS-ZFCDPNO  = 'A028050109  ' ) OR
*       ( ZTIDSHS-ZFCDPNO  = 'A0370301    ' ) OR
*       ( ZTIDSHS-ZFCDPNO >= 'A02900010001' AND
*         ZTIDSHS-ZFCDPNO <= 'A02900010023' ) OR
*       ( ZTIDSHS-ZFCDPNO >= 'A034000001  ' AND
*         ZTIDSHS-ZFCDPNO <= 'A034000004  ' ).
*    ELSE.
*      CONTINUE.
*    ENDIF.

    CLEAR ZTIDRCR.
*      MOVE-CORRESPONDING ZTIDSHS TO ZTIDRCR.
    MOVE ZTIDSHS-ZFBLNO        TO ZTIDRCR-ZFBLNO.
    MOVE ZTIDSHS-ZFCLSEQ       TO ZTIDRCR-ZFCLSEQ.
    MOVE ZTIDSHS-ZFCONO        TO ZTIDRCR-ZFCONO.
    MOVE ZTIDSHS-STAWN         TO ZTIDRCR-STAWN.
    MOVE ZTIDSHS-ZFTBAK        TO ZTIDRCR-ZFTBAK.
    MOVE 'KRW'                 TO ZTIDRCR-ZFKRW.
    MOVE ZTIDSHS-ZFTBAU        TO ZTIDRCR-ZFTBAU.
    MOVE 'USD'                 TO ZTIDRCR-ZFUSD.
    MOVE ZTIDSHS-ZFCUAMT       TO ZTIDRCR-ZFCUAMT.
    MOVE ZTIDSHS-ZFHMAMT       TO ZTIDRCR-ZFHMTX.
    MOVE ZTIDSHS-ZFEDAMT       TO ZTIDRCR-ZFEDTX.
    MOVE ZTIDSHS-ZFAGAMT       TO ZTIDRCR-ZFATX.
    MOVE ZTIDSHS-ZFVAAMT       TO ZTIDRCR-ZFVTX.
    MOVE ZTIDSHS-ZFCCAMT       TO ZTIDRCR-ZFCUDAK.
    SELECT MAX( ZFAPLDT ) INTO W_ZFAPLDT
      FROM ZTIMIMG06
     WHERE WAERS = 'USD'
       AND ZFAPLDT <= ZTIDS-ZFIDSDT
       AND ZFEXPDT >= ZTIDS-ZFIDSDT.
    IF W_ZFAPLDT IS INITIAL.
      ZTIMIMG06-ZFEXRT = 1200.
    ELSE.
      CLEAR ZTIMIMG06.
      SELECT SINGLE *
        FROM ZTIMIMG06
       WHERE WAERS = ZTIDS-ZFSTAMC
         AND ZFAPLDT = W_ZFAPLDT.
    ENDIF.
    IF NOT ( ZTIMIMG06-ZFEXRT IS INITIAL ).
      ZTIDRCR-ZFCUDAU = ZTIDRCR-ZFCUDAK / ZTIMIMG06-ZFEXRT * 100.
    ENDIF.
    MOVE ZTIDSHS-ZFRDRT        TO ZTIDRCR-ZFRDRT.
*    IF ( ZTIDSHS-ZFCDPNO >= 'A00700010001' AND
*         ZTIDSHS-ZFCDPNO <= 'A00790000000' ).
*      MOVE '1'                 TO ZTIDRCR-ZFDETY. " Ã·?
*    ENDIF.
*    IF ( ZTIDSHS-ZFCDPNO >= 'A028070101  ' AND
*         ZTIDSHS-ZFCDPNO <= 'A02807010402' ).
*      MOVE '2'                 TO ZTIDRCR-ZFDETY. " ÀÚµ¿?
*    ENDIF.
*    IF ZTIDSHS-ZFCDPNO = 'A028050109  '.
*      IF ZTIDRCR-ZFMATGB = '4'.
*        MOVE '3'              TO ZTIDRCR-ZFDETY. " ¿¬±¸¼Ò(½Ã¼³Àç)
*      ELSE.
*        MOVE '4'              TO ZTIDRCR-ZFDETY. " ¿¬±¸¼Ò(¿øÀÚÀç)
*      ENDIF.
*    ENDIF.
*    IF ZTIDSHS-ZFCDPNO = 'A0370301    '.
*      MOVE '5'                 TO ZTIDRCR-ZFDETY. " ¿ëµµ¼¼?
*    ENDIF.
*    IF ( ZTIDSHS-ZFCDPNO >= 'A02900010001' AND
*         ZTIDSHS-ZFCDPNO <= 'A02900010023' ).
*      MOVE '6'                 TO ZTIDRCR-ZFDETY. " Àç¼öÃâÁ¶?
*    ENDIF.
*    IF ( ZTIDSHS-ZFCDPNO >= 'A034000001  ' AND
*         ZTIDSHS-ZFCDPNO <= 'A034000004  ' ).
*      MOVE '7'                 TO ZTIDRCR-ZFDETY. " Àç¼ö?
*    ENDIF.
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = ZTIDRCR-ZFBLNO.
    MOVE ZTBL-ZFMATGB          TO ZTIDRCR-ZFMATGB.
    MOVE SY-UNAME              TO ZTIDRCR-ERNAM.
    MOVE SY-DATUM              TO ZTIDRCR-CDAT.
    MOVE SY-UNAME              TO ZTIDRCR-UNAM.
    MOVE SY-DATUM              TO ZTIDRCR-UDAT.
    INSERT ZTIDRCR.

    SELECT *
      FROM ZTIDSHSD
     WHERE ZFBLNO = ZTIDSHS-ZFBLNO
       AND ZFCLSEQ = ZTIDSHS-ZFCLSEQ
       AND ZFCONO = ZTIDSHS-ZFCONO.
      CLEAR ZTIDRCRIT.
*             MOVE-CORRESPONDING ZTIDSHSD TO ZTIDRCRIT.
      MOVE ZTIDSHSD-ZFBLNO        TO ZTIDRCRIT-ZFBLNO.
      MOVE ZTIDSHSD-ZFCLSEQ       TO ZTIDRCRIT-ZFCLSEQ.
      MOVE ZTIDSHSD-ZFCONO        TO ZTIDRCRIT-ZFCONO.
      MOVE ZTIDSHSD-ZFRONO        TO ZTIDRCRIT-ZFRONO.
      MOVE ZTIDSHSD-ZFSTCD        TO ZTIDRCRIT-MATNR.
      MOVE ZTIDSHSD-ZFGDDS1       TO ZTIDRCRIT-MAKTX.
      MOVE ZTIDSHSD-ZFQNT         TO ZTIDRCRIT-ZFDEQN.
      MOVE ZTIDSHSD-ZFQNTM        TO ZTIDRCRIT-ZFDEQNM.
      MOVE ZTIDRCR-ZFDETY         TO ZTIDRCRIT-ZFDETY.
*             °ú¼¼°¡°Ý....
      SELECT MAX( ZFREQNO ) INTO ZTIDRCRIT-ZFREQNO
        FROM ZTREQHD
       WHERE EBELN = ZTBL-ZFREBELN.
      MOVE ZTBL-ZFMATGB           TO ZTIDRCRIT-ZFMATGB.
      MOVE ZTBL-ZFPRNAM           TO ZTIDRCRIT-ZFPRNAM.
      MOVE ZTIDS-ZFIDSDT          TO ZTIDRCRIT-ZFIDSDT.
      IF ZTIDRCRIT-ZFDETY = '1' OR  ZTIDRCRIT-ZFDETY = '3'.
        CALL FUNCTION 'ZIM_BEFORE_N_MONTH_DATE'
             EXPORTING
                  DATE                      = ZTIDRCRIT-ZFIDSDT
                  W_MONTH                   = 12
             IMPORTING
                  W_OUT_DATE                = W_DATE
             EXCEPTIONS
                  PLAUSIBILITY_CHECK_FAILED = 4.
        MOVE W_DATE   TO     ZTIDRCRIT-ZFEDDT.
      ENDIF.
      IF ZTIDRCRIT-ZFDETY = '2'.
        CALL FUNCTION 'ZIM_BEFORE_N_MONTH_DATE'
             EXPORTING
                  DATE                      = ZTIDRCRIT-ZFIDSDT
                  W_MONTH                   = 1
             IMPORTING
                  W_OUT_DATE                = W_DATE
             EXCEPTIONS
                  PLAUSIBILITY_CHECK_FAILED = 4.
        MOVE W_DATE   TO     ZTIDRCRIT-ZFEDDT.
      ENDIF.
      MOVE 'KRW'                  TO ZTIDRCRIT-ZFKRW.
      MOVE 'USD'                  TO ZTIDRCRIT-ZFUSD.
      MOVE SY-UNAME               TO ZTIDRCRIT-ERNAM.
      MOVE SY-DATUM               TO ZTIDRCRIT-CDAT.
      MOVE SY-UNAME               TO ZTIDRCRIT-UNAM.
      MOVE SY-DATUM               TO ZTIDRCRIT-UDAT.
      INSERT ZTIDRCRIT.
    ENDSELECT.

  ENDLOOP.

ENDFORM.                    " P3000_INSERT_ZTIDRCR
*&---------------------------------------------------------------------*
*&      Form  P2000_DDLC_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_DDLC_PROCESS.

  CASE F.
* ¼öÀÔ¹®?
    WHEN 'ZTBL-ZFOPNNO' OR 'ZTLG-ZFDCNO'.
      ASSIGN (F)   TO    <FS_F>.
      PERFORM  P2000_LC_DISPLAY    USING  '' '' <FS_F>.
    WHEN 'ZTLG-ZFREQNO'.
      PERFORM  P2000_LC_DISPLAY    USING  ZTLG-ZFREQNO '' ''.
* ±¸¸Å¹®?
    WHEN 'ZTBL-ZFREBELN' OR 'ZTBLINR_TMP-ZFREBELN'.   " ±¸¸Å¹®?
      ASSIGN (F)   TO    <FS_F>.
      PERFORM   P2000_PO_DOC_DISPLAY    USING  <FS_F> ''.
* vendor
    WHEN 'ZTBL-LIFNR' OR 'ZTBL-ZFFORD' OR 'ZTBL-ZFTRCK' OR
         'ZTLG-ZFISBNC' OR 'ZTLG-ZFCARIR' OR 'ZTLG-ZFGSCD' OR
         'ZTBLINR_TMP-LIFNR'.
      ASSIGN (F)   TO    <FS_F>.
      PERFORM  P2000_VENDOR_MASTER USING <FS_F>.
* B/L ¹®?
    WHEN 'ZTBLINOU-ZFBLNO' OR 'ZTBLINR-ZFBLNO'.
      ASSIGN (F)   TO    <FS_F>.
      PERFORM  P2000_BL_DOC_DISPLAY USING <FS_F>.
    WHEN 'ZTBL-ZFBLNO'.
      CASE SY-DYNNR.
        WHEN '2601'.        " L/G
          ASSIGN (F)   TO    <FS_F>.
          PERFORM  P2000_BL_DOC_DISPLAY USING <FS_F>.
        WHEN OTHERS.
      ENDCASE.
    WHEN OTHERS.
      IF F(5) EQ 'ZSIV-'.
        IF LINE GT 0.
          READ TABLE IT_ZSIV INDEX LINE.
          IF SY-SUBRC EQ 0.
            PERFORM  P2000_IV_DOC_DISPLAY USING IT_ZSIV-ZFIVNO.
          ENDIF.
        ENDIF.
      ELSEIF F(7) EQ 'ZSIVIT-'.
        IF LINE GT 0.
          READ TABLE IT_ZSIVIT INDEX LINE.
          IF SY-SUBRC EQ 0.
            IF SY-TCODE EQ 'ZIM33'.
              PERFORM  P1000_GET_CONTAINER_DATA_CC.
            ELSE.
              PERFORM  P2000_IV_DOC_DISPLAY USING IT_ZSIVIT-ZFIVNO.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSEIF F(7) EQ 'ZSBLIT-'.
        IF LINE GT 0.
          READ TABLE IT_ZSBLIT INDEX LINE.
          IF SY-SUBRC EQ 0.
            PERFORM  P1000_GET_CONTAINER_DATA.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.                    " P2000_DDLC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_VENDOR_MASTER
*&---------------------------------------------------------------------*
FORM P2000_VENDOR_MASTER USING    P_LIFNR.
  IF P_LIFNR IS INITIAL.
    MESSAGE S174.    EXIT.
  ENDIF.
* È­¸é PARAMETER ID
  SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
*  SET PARAMETER ID 'KDY' FIELD '/320/310/130/120/110'.
  SET PARAMETER ID 'LIF' FIELD P_LIFNR.
*  SET PARAMETER ID 'EKO' FIELD ZTREQST-EKORG.
  SET PARAMETER ID 'EKO' FIELD ''.

  CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_VENDOR_MASTER
*&---------------------------------------------------------------------*
*&      Form  P2000_LC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_LC_DISPLAY USING    P_ZFREQNO
                               P_ZFAMDNO
                               P_ZFOPNNO.

  IF P_ZFREQNO  IS INITIAL AND  P_ZFAMDNO  IS INITIAL AND
     P_ZFOPNNO  IS INITIAL.
    MESSAGE E066.
  ENDIF.

  SET PARAMETER ID 'BES'     FIELD ''.
  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO' FIELD P_ZFAMDNO.
  SET PARAMETER ID 'ZPOPNNO' FIELD P_ZFOPNNO.

  IF P_ZFAMDNO IS INITIAL.
    CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
  ELSE.
    CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
  ENDIF.
ENDFORM.                    " P2000_LC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_MOVE_BL_DATA_INOU
*&---------------------------------------------------------------------*
FORM P2000_MOVE_BL_DATA_INOU.

  DATA : W_ZTBLINOU   LIKE ZTBLINOU.

  W_ZTBLINOU    =     ZTBLINOU.

  CLEAR : ZTBLINOU.
* ÃÖÃÊ »ý¼º?
  IF W_ZFBTSEQ IS INITIAL.
    MOVE : ZTBL-ZFBLNO     TO     ZTBLINOU-ZFBLNO,     " B/L °ü¸®¹ø?
           W_ZFBTSEQ       TO     ZTBLINOU-ZFBTSEQ,    " SEQ. NO.
           ZTBL-ZFPKCN     TO     ZTBLINOU-ZFPKCN,     " Package
           ZTBL-ZFPKCNM    TO     ZTBLINOU-ZFPKCNM,    " Package Unit
*-----------------------------------------------------------------------
           ZTBL-ZFTOWT      TO     ZTBLINOU-ZFWEIG,
           ZTBL-ZFTOWTM     TO     ZTBLINOU-ZFWEINM,
*             ZTBL-ZFNEWT     TO     ZTBLINOU-ZFWEIG,     " Gross
*             ZTBL-ZFNEWTM    TO     ZTBLINOU-ZFWEINM,    "  Unit
*-----------------------------------------------------------------------
*>>>>>>>>> ±èÆ÷º¸¼¼ÀåÄ¡Àå DEFAULT VALUE
           ZTBL-ZFBNARCD   TO     ZTBLINOU-ZFABNARC,   " µµÂøÁö.
           ZTBL-ZFGMNO     TO     ZTBLINOU-ZFGMNO,     " ÀûÇÏ¸ñ·Ï.
           ZTBL-ZFMSN      TO     ZTBLINOU-ZFMSN,
           ZTBL-ZFHSN      TO     ZTBLINOU-ZFHSN,
           SY-UNAME        TO     ZTBLINOU-ERNAM,      " »ý¼ºÀÎ.
           SY-DATUM        TO     ZTBLINOU-CDAT,       " »ý¼ºÀÏ.
           SY-UNAME        TO     ZTBLINOU-UNAM,       " º¯°æÀÎ.
           SY-DATUM        TO     ZTBLINOU-UDAT,       " º¯°æÀÏ.
           SY-DATUM        TO     ZTBLINOU-ZFRCDT.     " ¼ö½ÅÀÎ.
  ELSE.
* ¹ÝÀÔÁ¤º¸ »ý¼º?
    MOVE : ZTBL-ZFBLNO      TO     ZTBLINOU-ZFBLNO,     " B/L °ü¸®¹ø?
           W_ZFBTSEQ        TO     ZTBLINOU-ZFBTSEQ,    " SEQ. NO.
*  Áß·®°ú Æ÷Àå¼ö·®ÀÌ ¹Ù²î¾úÀ½ - ±è¿¬Áß 2000.07.03
*            ZTBLOUR-ZFOUWT   TO     ZTBLINOU-ZFPKCN,     " Package
*            ZTBLOUR-ZFKG     TO     ZTBLINOU-ZFPKCNM,    " Package Unit
*            ZTBLOUR-ZFOUQN   TO     ZTBLINOU-ZFWEIG,     " Gross
*            ZTBLOUR-ZFOUQNM  TO     ZTBLINOU-ZFWEINM,    "  Unit
           ZTBLOUR-ZFOUQN   TO     ZTBLINOU-ZFPKCN,     " Package
           ZTBLOUR-ZFOUQNM  TO     ZTBLINOU-ZFPKCNM,    " Package Unit
           ZTBLOUR-ZFOUWT   TO     ZTBLINOU-ZFWEIG,     " Gross
           ZTBLOUR-ZFKG     TO     ZTBLINOU-ZFWEINM,    "  Unit
*
           ZTBLOUR-ZFBNARCD TO     ZTBLINOU-ZFDBNARC,   " ¹ß¼Û?
           ''               TO     ZTBLINOU-ZFABNARC,   " µµÂø?
           W_ZTBLINOU-ZFGMNO TO    ZTBLINOU-ZFGMNO,     " ÀûÇÏ¸ñ?
           W_ZTBLINOU-ZFMSN TO     ZTBLINOU-ZFMSN,      " MSN
           W_ZTBLINOU-ZFHSN TO     ZTBLINOU-ZFHSN,      " HSN
*             ZTBLOUR-ZFBNARCD TO     ZTBLINOU-ZFBTRNO,    "
*            ±è¿¬Áß 2000.07.03
           ZTBLOUR-ZFOUANO TO     ZTBLINOU-ZFBTRNO, " º¸¼¼¿î¼Û½Å°í¹ø?
           SY-UNAME         TO     ZTBLINOU-ERNAM,      " »ý¼º?
           SY-DATUM         TO     ZTBLINOU-CDAT,       " »ý¼º?
           SY-UNAME         TO     ZTBLINOU-UNAM,       " º¯°æ?
           SY-DATUM         TO     ZTBLINOU-UDAT,       " º¯°æ?
           SY-DATUM         TO     ZTBLINOU-ZFRCDT.     " ¼ö½Å?
  ENDIF.


* BENIFICIARY
  PERFORM  P1000_GET_VENDOR   USING      ZTBL-LIFNR
                              CHANGING   W_LFA1-NAME1.
  MOVE : W_LFA1-NAME1     TO     ZTBLINOU-ZFGSNM,
         W_LFA1-NAME1     TO     W_LFA1-NAME1.
* ¹ß¼ÛÁö.
  IF NOT ZTBLINOU-ZFDBNARC IS INITIAL.
    PERFORM   P1000_GET_BONDED_NAME   USING      ZTBLINOU-ZFDBNARC
                                      CHANGING   ZTBLINOU-ZFDBNAR
                                                 W_DEL_NM.
  ENDIF.
* µµÂøÁö.
  IF NOT ZTBLINOU-ZFABNARC IS INITIAL.
    PERFORM   P1000_GET_BONDED_NAME   USING      ZTBLINOU-ZFABNARC
                                      CHANGING   ZTBLINOU-ZFABNAR
                                                 W_ARR_NM.
  ENDIF.

ENDFORM.                    " P2000_MOVE_BL_DATA_INOU
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_BONDED_NAME
*&---------------------------------------------------------------------*
FORM P1000_GET_BONDED_NAME USING    P_INTER_CD
                           CHANGING P_EXTER_CD
                                    P_NAME.

  CLEAR : P_EXTER_CD, P_NAME, ZTIMIMG03.

  IF P_INTER_CD IS INITIAL.
    MESSAGE I167 WITH 'Internal code of bonded area '.   EXIT.
  ENDIF.

  SELECT * FROM ZTIMIMG03 UP TO 1 ROWS
                           WHERE ZFBNARCD EQ P_INTER_CD.
    EXIT.
  ENDSELECT.

  IF SY-SUBRC EQ 0.
    P_EXTER_CD = ZTIMIMG03-ZFBNAR.
    P_NAME     = ZTIMIMG03-ZFBNARM.
  ELSE.
    MESSAGE I245 WITH P_INTER_CD.
  ENDIF.

ENDFORM.                    " P1000_GET_BONDED_NAME
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_BONDED_NAME1
*&---------------------------------------------------------------------*
FORM P1000_GET_BONDED_NAME1 USING    P_INTER_CD
                            CHANGING P_NAME.


  CLEAR : P_NAME, ZTIMIMG03.

  IF P_INTER_CD IS INITIAL.   EXIT.    ENDIF.

  SELECT * FROM ZTIMIMG03 UP TO 1 ROWS
                           WHERE ZFBNARCD EQ P_INTER_CD.
    EXIT.
  ENDSELECT.

  IF SY-SUBRC EQ 0.
    P_NAME     = ZTIMIMG03-ZFBNARM.
  ENDIF.

ENDFORM.                    " P1000_GET_BONDED_NAME1
*&---------------------------------------------------------------------*
*&      Form  P2000_ZIM93_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_ZIM93_DISPLAY USING    P_ZFREQNO
                                  P_EBELN.

  IF P_ZFREQNO IS INITIAL AND  P_EBELN IS INITIAL.
    MESSAGE E063.
  ENDIF.

  SET PARAMETER ID 'BES'     FIELD P_EBELN.
  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.

  CALL TRANSACTION 'ZIM93' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_ZIM93_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_INOU_DUP_LIST
*&---------------------------------------------------------------------*
FORM P2000_INOU_DUP_LIST.

  WRITE : / IT_ZSBLINOU-ZFBTSEQ NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSBLINOU-ZFGMNO   NO-GAP,    '-'      NO-GAP,
          IT_ZSBLINOU-ZFMSN    NO-GAP,    '-'      NO-GAP,
          IT_ZSBLINOU-ZFHSN    NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSBLINOU-ZFDBNAR  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSBLINOU-ZFABNAR  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSBLINOU-ZFBTRNO  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSBLINOU-ZFTDDT   NO-GAP.

ENDFORM.                    " P2000_INOU_DUP_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BLINOU_DOC
*&---------------------------------------------------------------------*
FORM P1000_READ_BLINOU_DOC.

  IF SY-TCODE EQ 'ZIMI1'.    " ¹ÝÀÔ¿¹Á¤Á¤?

    SELECT COUNT( * ) INTO W_COUNT
           FROM    ZTIV
           WHERE   ZFBLNO  EQ   ZTBL-ZFBLNO.
    IF W_COUNT GT 0.
      MESSAGE E538 WITH ZTBL-ZFBLNO W_COUNT.
    ENDIF.
*-----------------------------------------------------------------------
* ÀÌ¹Ì »ý¼ºµÈ ¹ÝÀÔ¿¹Á¤Á¤º¸ »óÅÂ °Ë»ö.
*-----------------------------------------------------------------------
    SELECT MAX( ZFBTSEQ ) INTO W_ZFBTSEQ FROM ZTBLINOU
                          WHERE ZFBLNO EQ ZTBL-ZFBLNO.
    IF NOT W_ZFBTSEQ IS INITIAL.
      SELECT SINGLE * FROM ZTBLINOU WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                    AND   ZFBTSEQ EQ W_ZFBTSEQ.

* ¹ÝÀÔ»óÅÂ.
      IF ZTBLINOU-ZFBINYN EQ SPACE.
        MESSAGE E182 WITH ZSREQHD-ZFBLNO W_ZFBTSEQ 'Carry-in'.
      ENDIF.

      SELECT SINGLE * FROM ZTBLINR  WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                    AND   ZFBTSEQ EQ W_ZFBTSEQ.
      IF ZTBLINR-ZFDOCST NE 'O'.
        MESSAGE E183 WITH ZSREQHD-ZFBLNO W_ZFBTSEQ 'Carry-in'
                          ZTBLINR-ZFDOCST.
      ENDIF.

* ¹ÝÃâ»ó?
      IF ZTBLINOU-ZFBOUYN EQ SPACE.
        MESSAGE E182 WITH ZSREQHD-ZFBLNO W_ZFBTSEQ 'Carry-out'.
      ENDIF.
      SELECT SINGLE * FROM ZTBLOUR  WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                    AND   ZFBTSEQ EQ W_ZFBTSEQ.
      IF ZTBLOUR-ZFDOCST NE 'O'.
        MESSAGE E183 WITH ZSREQHD-ZFBLNO W_ZFBTSEQ 'Carry-out'
                          ZTBLOUR-ZFDOCST.
      ENDIF.
    ENDIF.
  ENDIF.
  W_COUNT = 1.

ENDFORM.                    " P1000_READ_BLINOU_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_INR_DUP_LIST
*&---------------------------------------------------------------------*
FORM P2000_INR_DUP_LIST.

  WRITE : / IT_ZTBLINR-ZFBTSEQ NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZTBLINR-ZFINRNO  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTBLINR-ZFABNAR  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTBLINR-ZFINTY   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTBLINR-ZFINDT   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTBLINR-ZFGINM.

ENDFORM.                    " P2000_INR_DUP_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_INR_CHANGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_INR_CHANGE_CHECK.
* ¹®¼­ »ó?
  CASE ZTBLINR-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTBLINR-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTBLINR-ZFBLNO 'EDI open requested' 'Change'.
      ELSEIF ZTBLINR-ZFEDIST = 'N'.
        MESSAGE E999 WITH ZTBLINR-ZFBLNO 'Flat Data created'
                                        'Change'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTBLINR-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTBLINR-ZFBLNO 'EDI opened' 'Change'.
      ELSE.
        MESSAGE I999
                WITH ZTBLINR-ZFBLNO 'Non-EDI opened' 'Change'.
      ENDIF.
      EXIT.
    WHEN 'C'.          " CANCEL
      MESSAGE E999 WITH ZTBLINR-ZFBLNO 'CALCELED' 'Change'.
  ENDCASE.

ENDFORM.                    " P2000_INR_CHANGE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_INR_DISPLAY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_INR_DISPLAY_CHECK.
* ¹®¼­ »ó?
  CASE ZTBLINR-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTBLINR-ZFEDIST = 'S'.
        MESSAGE S998 WITH ZTBLINR-ZFBLNO 'EDI open requested'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTBLINR-ZFEDIST = 'R'.
        MESSAGE S998 WITH ZTBLINR-ZFBLNO 'EDI opened'.
      ELSE.
        MESSAGE S998 WITH ZTBLINR-ZFBLNO 'Non-EDI opened'.
      ENDIF.
    WHEN 'C'.          " CANCEL
      MESSAGE S998 WITH ZTBLINR-ZFBLNO 'CALCELED'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_INR_DISPLAY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_INR_OPEN_CHECK
*&---------------------------------------------------------------------*
FORM P2000_INR_OPEN_CHECK.
* ¹®¼­ »ó?
  CASE ZTBLINR-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTBLINR-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTBLINR-ZFBLNO  'EDI open requested'  'Open'.
      ELSE.
        MESSAGE E999 WITH ZTBLINR-ZFBLNO  'EDI Data Created'
                                                             'Open'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTBLINR-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTBLINR-ZFBLNO 'EDI opened' 'Open'.
      ELSE.
        MESSAGE I998 WITH ZTBLINR-ZFBLNO 'Non-EDI opened'.
      ENDIF.
      EXIT.
    WHEN 'C'.          " CANCEL
      MESSAGE E999 WITH ZTBLINR-ZFBLNO 'CALCELED'  'Open'.
    WHEN 'A'.          " AMEND
      MESSAGE E999 WITH ZTBLINR-ZFBLNO 'AMENDED'  'Open'.
  ENDCASE.

ENDFORM.                    " P2000_INR_OPEN_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_BLINOU_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_BLINOU_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLINOU'
         EXPORTING
              ZFBLNO  = ZTBLINOU-ZFBLNO
              ZFBTSEQ = ZTBLINOU-ZFBTSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Carry-in expected info Document'
                            ZTBLINOU-ZFBLNO ZTBLINOU-ZFBTSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLINOU'
         EXPORTING
              ZFBLNO  = ZTBL-ZFBLNO
              ZFBTSEQ = ZTBLINOU-ZFBTSEQ.
  ENDIF.

ENDFORM.                    " P2000_SET_BLINOU_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_BLINR_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_BLINR_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLINR'
         EXPORTING
              ZFBLNO  = ZTBLINOU-ZFBLNO
              ZFBTSEQ = ZTBLINOU-ZFBTSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Carry-in Document'
                            ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLINR'
         EXPORTING
              ZFBLNO  = ZTBL-ZFBLNO
              ZFBTSEQ = ZTBLINOU-ZFBTSEQ.
  ENDIF.

ENDFORM.                    " P2000_SET_BLINR_LOCK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_BLOUR_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_BLOUR_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLOUR'
         EXPORTING
              ZFBLNO  = ZTBLINOU-ZFBLNO
              ZFBTSEQ = ZTBLINOU-ZFBTSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Carry-out Document'
                            ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    IF W_STATUS EQ C_REQ_C.
      PERFORM P2000_SET_BLINR_LOCK    USING    'U'.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLOUR'
         EXPORTING
              ZFBLNO  = ZTBL-ZFBLNO
              ZFBTSEQ = ZTBLINOU-ZFBTSEQ.
  ENDIF.

ENDFORM.                    " P2000_SET_BLOUR_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_BLINOU_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_BLINOU_DEL_STATUS_CHECK.
*  IF ZTBLINOU-ZFDOCST NE 'N'.
*     MESSAGE E104 WITH ZTBLINOU-ZFBLNO ZTBLINOU-ZFBTSEQ
*                       ZTBLINOU-ZFDOCST.
*  ENDIF.
*  IF ZTBLINOU-ZFEDIST NE 'N'.
*     MESSAGE E105 WITH ZTBLINOU-ZFBLNO ZTBLINOU-ZFBTSEQ
*                       ZTBLINOU-ZFDOCST.
*  ENDIF.
*-----------------------------------------------------------------------
* ÈÄ¼Ó ÀÛ¾÷ ÁøÇà ¿©ºÎ °Ë?
*-----------------------------------------------------------------------
  SELECT COUNT( * ) INTO W_COUNT FROM ZTBLINR
                    WHERE ZFBLNO  EQ ZTBLINOU-ZFBLNO
                    AND   ZFBTSEQ EQ ZTBLINOU-ZFBTSEQ.
  IF W_COUNT > 0.
    MESSAGE E184 WITH ZTBLINOU-ZFBLNO ZTBLINOU-ZFBTSEQ.
  ENDIF.

ENDFORM.                    " P2000_BLINOU_DEL_STATUS_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_BLINR_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_BLINR_DEL_STATUS_CHECK.
  IF ZTBLINR-ZFDOCST NE 'N'.
    MESSAGE E104 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ
                      ZTBLINR-ZFDOCST.
  ENDIF.
  IF ZTBLINR-ZFEDIST NE 'N'.
    MESSAGE E105 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ
                      ZTBLINR-ZFDOCST.
  ENDIF.

  SELECT COUNT( * ) INTO W_COUNT FROM ZTIV
                 WHERE ZFBLNO  EQ ZTBLINR-ZFBLNO.
  IF W_COUNT > 0.
    MESSAGE E492 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ.
  ENDIF.
*-----------------------------------------------------------------------
* ÈÄ¼Ó ÀÛ¾÷ ÁøÇà ¿©ºÎ °Ë?
*-----------------------------------------------------------------------
  SELECT COUNT( * ) INTO W_COUNT FROM ZTBLOUR
                    WHERE ZFBLNO  EQ ZTBLINR-ZFBLNO
                    AND   ZFBTSEQ EQ ZTBLINR-ZFBTSEQ.
  IF W_COUNT > 0.
    MESSAGE E185 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ.
  ENDIF.

ENDFORM.                    " P2000_BLINR_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_BLOUR_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_BLOUR_DEL_STATUS_CHECK.

  IF ZTBLOUR-ZFDOCST NE 'N'.
    MESSAGE E104 WITH ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ
                      ZTBLOUR-ZFDOCST.
  ENDIF.

*  IF ZTBLINR-ZFEDIST NE 'N'.
*     MESSAGE E105 WITH ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ
*                       ZTBLOUR-ZFDOCST.
*  ENDIF.

*-----------------------------------------------------------------------
* ÈÄ¼Ó ÀÛ¾÷ ÁøÇà ¿©ºÎ °Ë?
*-----------------------------------------------------------------------
  SELECT COUNT( * ) INTO W_COUNT FROM ZTBLINOU
                    WHERE ZFBLNO  EQ ZTBLOUR-ZFBLNO
                    AND   ZFBTSEQ GT ZTBLOUR-ZFBTSEQ.
  IF W_COUNT > 0.
    MESSAGE E259 WITH ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ.
  ENDIF.

ENDFORM.                    " P2000_BLOUR_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_LG_EDI_SEND
*&---------------------------------------------------------------------*
FORM P3000_LG_EDI_SEND.
*-----------------------------------------------------------------------
* »óÅÂ °Ë?
*-----------------------------------------------------------------------
  IF ZTLG-ZFEDICK EQ 'X'.
    MESSAGE E119 WITH ZTLG-ZFBLNO  ''.
  ENDIF.
  IF ZTLG-ZFEDIST NE 'N'.
    MESSAGE E105 WITH ZTLG-ZFBLNO   ''   ZTLG-ZFEDIST.
  ENDIF.
* STATUS CHECK
  PERFORM P2000_LG_OPEN_CHECK.
* °³¼³ÀºÇà NAME1 GET
  PERFORM  P1000_GET_VENDOR   USING      ZTLG-ZFISBNC
                              CHANGING   W_OPEN_NM.
* EDI CREATE
  PERFORM   P3000_MASTER_LG_FLAT_CREATE.
* ¹®¼­ STATUS º¯?
  ZTLG-ZFDOCST = 'R'.
  ZTLG-ZFDOCNO = W_ZFDHENO.
  ZTLG-UDAT    = SY-DATUM.
  ZTLG-UNAM    = SY-UNAME.

ENDFORM.                    " P3000_LG_EDI_SEND

*&---------------------------------------------------------------------*
*&      Form  P3000_BLINR_EDI_SEND
*&---------------------------------------------------------------------*
FORM P3000_BLINR_EDI_SEND.
*-----------------------------------------------------------------------
* »óÅÂ °Ë?
*-----------------------------------------------------------------------
  IF ZTBLINR-ZFEDICK EQ 'X'.
    MESSAGE E119 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ.
  ENDIF.
* IF ZTBLINR-ZFEDIST NE 'N'.
  IF ZTBLINR-ZFEDIST EQ 'S' OR
       ZTBLINR-ZFEDIST EQ 'R'.
    MESSAGE E105 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ ZTBLINR-ZFEDIST.
  ENDIF.

* STATUS CHECK
  PERFORM P2000_INR_OPEN_CHECK.
* °³¼³ÀºÇà NAME1 GET
* PERFORM  P1000_GET_VENDOR   USING      ZTLG-ZFISBNC
*                             CHANGING   W_OPEN_NM.

* EDI CREATE
  PERFORM   P3000_MASTER_BLINR_FLAT_CREATE.
* ¹®¼­ STATUS º¯?
  ZTBLINR-ZFDOCST = 'R'.
  ZTBLINR-ZFDOCNO = W_ZFDHENO.
  ZTBLINR-UDAT    = SY-DATUM.
  ZTBLINR-UNAM    = SY-UNAME.

ENDFORM.                    " P3000_BLINR_EDI_SEND

*&---------------------------------------------------------------------*
*&      Form  P3000_BLOUR_EDI_SEND
*&---------------------------------------------------------------------*
FORM P3000_BLOUR_EDI_SEND.
*-----------------------------------------------------------------------
* »óÅÂ °Ë?
*-----------------------------------------------------------------------
  IF ZTBLOUR-ZFEDICK EQ 'X'.
    MESSAGE E119 WITH ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ.
  ENDIF.
  IF ZTBLOUR-ZFEDIST NE 'N'.
    MESSAGE E105 WITH ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ ZTBLOUR-ZFEDIST.
  ENDIF.

* STATUS CHECK
  PERFORM P2000_OUR_OPEN_CHECK.
* °³¼³ÀºÇà NAME1 GET
* PERFORM  P1000_GET_VENDOR   USING      ZTLG-ZFISBNC
*                             CHANGING   W_OPEN_NM.

* EDI CREATE
  PERFORM   P3000_MASTER_BLOUR_FLAT_CREATE.

* ¹®¼­ STATUS º¯?
  ZTBLOUR-ZFDOCST = 'R'.
  ZTBLOUR-ZFDOCNO = W_ZFDHENO.
  ZTBLOUR-UDAT    = SY-DATUM.
  ZTBLOUR-UNAM    = SY-UNAME.

ENDFORM.                    " P3000_BLOUR_EDI_SEND
*&---------------------------------------------------------------------*
*&      Form  P3000_MASTER_BLINR_FLAT_CREATE
*&---------------------------------------------------------------------*
FORM P3000_MASTER_BLINR_FLAT_CREATE.

  DATA : L_ZFEDIID LIKE  ZTIMIMG03-ZFEDIID.

  W_ZFCDDOC = 'CUSCAR'.
*-----------------------------------------------------------------------
*>>> 2000/12/27 KSB ¼ö?
  PERFORM   P1000_GET_EDI_INDICATE   USING  ZTBLINR-ZFINRC
                                            W_ZFDHSRO.
*  IF ZTBLINR-ZFINRNO(3) EQ '121'.
*     W_ZFDHSRO = 'KCS1204'.           " ½Äº°?
*  ELSE.
*     W_ZFDHSRO = 'KCS0104'.           " ½Äº°?
*  ENDIF.
*-----------------------------------------------------------------------

  W_ZFDHREF = ZTBLINR-ZFINRNO.     " ÂüÁ¶¹øÈ£ ( ¹ÝÀÔ½Å°í¹øÈ£ )
*  W_ZFDHDDB = ''.                  " ºÎ¼­.
  W_ZFDHENO = ZTBLINR-ZFDOCNO.     " ¹®¼­¹øÈ£.

  CALL FUNCTION 'ZIM_GET_SEND_EDI_ID'
       EXPORTING
            W_ZFCDDOC  = W_ZFCDDOC
            W_ZFBNARCD = ZTBLINR-ZFBNARCD
       IMPORTING
            W_ZFEDIID  = L_ZFEDIID
       EXCEPTIONS
            NOT_FOUND  = 4
            NOT_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN 4.        MESSAGE E270.
    WHEN 8.        MESSAGE E271.
  ENDCASE.
*
  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
             W_ZFCDDOC    =   W_ZFCDDOC
             W_ZFDHSRO    =   W_ZFDHSRO
             W_ZFDHREF    =   W_ZFDHREF
*             W_ZFDHDDB    =   W_ZFDHDDB
             W_BUKRS        =   ZTBLINR-BUKRS
             W_ZFEDIID    =   L_ZFEDIID
       CHANGING
             W_ZFDHENO    =   W_ZFDHENO
       EXCEPTIONS
             DB_ERROR     =   4
             NO_TYPE      =   8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_CUSCAR_EDI_SEND'
       EXPORTING
            W_ZFBLNO  = ZTBLINR-ZFBLNO
            W_ZFBTSEQ = ZTBLINR-ZFBTSEQ
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

ENDFORM.                    " P3000_MASTER_BLINR_FLAT_CREATE

*&---------------------------------------------------------------------*
*&      Form  P2000_OUR_OPEN_CHECK
*&---------------------------------------------------------------------*
FORM P2000_OUR_OPEN_CHECK.
* ¹®¼­ »ó?
  CASE ZTBLOUR-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTBLOUR-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTBLOUR-ZFBLNO  'EDI open requested'  'Open'.
      ELSE.
        MESSAGE E999 WITH ZTBLOUR-ZFBLNO  'EDI Data Created'
                                                             'Open'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTBLOUR-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTBLOUR-ZFBLNO 'EDI opened' 'Open'.
      ELSE.
        MESSAGE I998 WITH ZTBLOUR-ZFBLNO 'Non-EDI opened'.
      ENDIF.
      EXIT.
    WHEN 'C'.          " CANCEL
      MESSAGE E999 WITH ZTBLOUR-ZFBLNO 'CALCELE'  'Open'.
    WHEN 'A'.          " AMEND
      MESSAGE E999 WITH ZTBLOUR-ZFBLNO 'AMENDED'  'Open'.
  ENDCASE.

ENDFORM.                    " P2000_OUR_OPEN_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_MASTER_BLOUR_FLAT_CREATE
*&---------------------------------------------------------------------*
FORM P3000_MASTER_BLOUR_FLAT_CREATE.

  DATA : L_ZFEDIID LIKE  ZTIMIMG03-ZFEDIID.

  W_ZFCDDOC = 'CUSBRR'.
*-----------------------------------------------------------------------
*>>> 2000/12/27 KSB ¼ö?
  PERFORM   P1000_GET_EDI_INDICATE   USING  ZTBLOUR-ZFINRC
                                            W_ZFDHSRO.
*  IF ZTBLINR-ZFINRNO(3) EQ '121'.
*     W_ZFDHSRO = 'KCS1204'.           " ½Äº°?
*  ELSE.
*     W_ZFDHSRO = 'KCS0104'.           " ½Äº°?
*  ENDIF.
*-----------------------------------------------------------------------

  W_ZFDHREF = ZTBLOUR-ZFOURNO.     " ÂüÁ¶¹øÈ£ ( ¹ÝÃâ½Å°í NO )
*  W_ZFDHDDB = ''.                  " ºÎ?
  W_ZFDHENO = ZTBLOUR-ZFDOCNO.     " ¹®¼­¹ø?

  CALL FUNCTION 'ZIM_GET_SEND_EDI_ID'
       EXPORTING
            W_ZFCDDOC  = W_ZFCDDOC
            W_ZFBNARCD = ZTBLOUR-ZFBNARCD
       IMPORTING
            W_ZFEDIID  = L_ZFEDIID
       EXCEPTIONS
            NOT_FOUND  = 4
            NOT_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN 4.        MESSAGE E270.
    WHEN 8.        MESSAGE E271.
  ENDCASE.
*
  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
             W_ZFCDDOC    =   W_ZFCDDOC
             W_ZFDHSRO    =   W_ZFDHSRO
             W_ZFDHREF    =   W_ZFDHREF
*             W_ZFDHDDB    =   W_ZFDHDDB
             W_BUKRS        =   ZTBLOUR-BUKRS
             W_ZFEDIID    =   L_ZFEDIID
       CHANGING
             W_ZFDHENO    =   W_ZFDHENO
       EXCEPTIONS
             DB_ERROR     =   4
             NO_TYPE      =   8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_CUSBRR_EDI_SEND'
       EXPORTING
            W_ZFBLNO  = ZTBLOUR-ZFBLNO
            W_ZFBTSEQ = ZTBLOUR-ZFBTSEQ
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

ENDFORM.                    " P3000_MASTER_BLOUR_FLAT_CREATE
*&---------------------------------------------------------------------*
*&      Form  P2000_BLINOU_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_BLINOU_DOC_DISPLAY USING    P_ZFBLNO
                                       P_ZFBTSEQ.
  IF P_ZFBLNO IS INITIAL OR  P_ZFBTSEQ IS INITIAL.
    MESSAGE  E188.
  ENDIF.
  SELECT COUNT( * ) INTO W_COUNT FROM ZTBLINOU
                         WHERE ZFBLNO   EQ   P_ZFBLNO
                         AND   ZFBTSEQ  EQ   P_ZFBTSEQ.
  IF W_COUNT EQ 0.
    MESSAGE  E191  WITH 'Carry-in expected Info'.
  ENDIF.
  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPBTSEQ' FIELD P_ZFBTSEQ.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.

  CALL TRANSACTION 'ZIMI3' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_BLINOU_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_BLINR_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_BLINR_DOC_DISPLAY USING    P_ZFBLNO
                                      P_ZFBTSEQ.

  IF P_ZFBLNO IS INITIAL OR  P_ZFBTSEQ IS INITIAL.
    MESSAGE  E189.
  ENDIF.

  SELECT COUNT( * ) INTO W_COUNT FROM ZTBLINR
                         WHERE ZFBLNO   EQ   P_ZFBLNO
                         AND   ZFBTSEQ  EQ   P_ZFBTSEQ.
  IF W_COUNT EQ 0.
    MESSAGE  E191  WITH 'Carry-in Info'.
  ENDIF.

  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPBTSEQ' FIELD P_ZFBTSEQ.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.

  CALL TRANSACTION 'ZIMI8' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_BLINR_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_BLOUR_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_BLOUR_DOC_DISPLAY USING    P_ZFBLNO
                                      P_ZFBTSEQ.

  IF P_ZFBLNO IS INITIAL OR  P_ZFBTSEQ IS INITIAL.
    MESSAGE  E189.
  ENDIF.

  SELECT COUNT( * ) INTO W_COUNT FROM ZTBLOUR
                         WHERE ZFBLNO   EQ   P_ZFBLNO
                         AND   ZFBTSEQ  EQ   P_ZFBTSEQ.
  IF W_COUNT EQ 0.
    MESSAGE  E191  WITH 'Carry-out Info'.
  ENDIF.

  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPBTSEQ' FIELD P_ZFBTSEQ.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.

  CALL TRANSACTION 'ZIMO3' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_BLOUR_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_EDI_DOC_CHECK
*&---------------------------------------------------------------------*
FORM P2000_EDI_DOC_CHECK.
  CASE SY-TCODE.
    WHEN 'ZIM26' OR 'ZIM27' OR 'ZIM28' OR 'ZIM29'.   " L/G
      PERFORM    P2000_LG_STATUS_CHECK.
      IF ZTLG-ZFEDICK = 'O'.
        MESSAGE I123 WITH ZTLG-ZFBLNO ZTLG-ZFLGSEQ.
      ENDIF.
    WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9'.            " ¹Ý?
      PERFORM   P2000_BLINR_CHECK.
      IF ZTBLINR-ZFEDICK = 'O'.
        MESSAGE I123 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ.
      ENDIF.
    WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4'. " ¹Ý?
      PERFORM   P2000_BLOUR_CHECK.
      IF ZTBLOUR-ZFEDICK = 'O'.
        MESSAGE I123 WITH ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ.
      ENDIF.
    WHEN 'ZIM35P'. "INVOICE-PAYORD 020828
      PERFORM   P2000_PAYORD_DOC_CHECK.
      IF ZTCIVHD-ZFEDICK = 'O'.
        MESSAGE I123 WITH ZTTTHD-ZFCIVRN ' '.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_EDI_DOC_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_BLINR_CHECK
*&---------------------------------------------------------------------*
FORM P2000_BLINR_CHECK.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTIMIMGTX-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    ZTBLINR-ZFEDICK = 'O'.
* ¹ÝÀÔ½Å°í¹ø?
    IF ZTBLINR-ZFINRNO IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Carry-in declaration No'.   EXIT.
    ENDIF.
* ÀüÀÚ¹®¼­±â?
    IF ZTBLINR-ZFEDINF IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Electronic Doc function'.   EXIT.
    ENDIF.
* ¹ÝÀÔ?
    IF ZTBLINR-ZFINDT  IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Carry-in date'.   EXIT.
    ENDIF.
* ¹ÝÀÔ½Ã?
    IF ZTBLINR-ZFINTM  IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Carry-in time'.  EXIT.
    ENDIF.
* ½Å°íÁö ¼¼?
    IF ZTBLINR-ZFINRC  IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH '½Å°íÁö customs'.  EXIT.
    ENDIF.
* ¼¼°üÀÇ ´ã´ç °úºÎ?
    IF ZTBLINR-ZFINRCD IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH '¼¼°üÀÇ ´ã´ç °úºÎÈ£'.  EXIT.
    ENDIF.
* È­¹°¹ÝÀÔÀ¯Çü±¸?
    IF ZTBLINR-ZFINTY  IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'È­¹°¹ÝÀÔÀ¯Çü±¸ºÐ'.  EXIT.
    ENDIF.
* ºÐÇÒ¹ÝÀÔ±¸?
    IF ZTBLINR-ZFPRIN  IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Classification of partial carry-in'.  EXIT.
    ENDIF.
* ºÐÇÒ¹ÝÀÔÂ÷?
    IF NOT ( ZTBLINR-ZFPRIN IS INITIAL OR ZTBLINR-ZFPRIN EQ 'A' ).
      IF ZTBLINR-ZFPINS IS INITIAL.
        ZTBLINR-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Partial carry-in balance'.  EXIT.
      ENDIF.
    ENDIF.
* ¹ÝÀÔÁ¤º¸ÀÇ ÀûÇÏ¸ñ·Ï°ü¸®¹ø?
    IF ZTBLINOU-ZFGMNO IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH '¹ÝÀÔÁ¤º¸ÀÇ ÀûÇÏ¸ñ·Ï°ü¸®¹øÈ£'.  EXIT.
    ENDIF.
* ¹ÝÀÔÁ¤º¸ÀÇ MSN
    IF ZTBLINOU-ZFMSN  IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'MSN of carry-in Info'.  EXIT.
    ENDIF.
* ¹ÝÀÔÁ¤º¸ÀÇ HSN
    IF ZTBLINOU-ZFMSN  IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'HSN of carry-in'.  EXIT.
    ENDIF.
* ¹ÝÀÔÁ¤º¸ÀÇ ¹ÝÀÔ±Ù°Å¹ø?
    IF ZTBLINOU-ZFBTRNO IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH '¹ÝÀÔÁ¤º¸ÀÇ ¹ÝÀÔ±Ù°Å¹øÈ£'.  EXIT.
    ENDIF.
* Æ÷Àå°³¼ö ´Ü?
    IF ZTBLINR-ZFPKCNM IS INITIAL.
      ZTBLINR-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Unit of packing number'.  EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " P2000_BLINR_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_BLOUR_CHECK
*&---------------------------------------------------------------------*
FORM P2000_BLOUR_CHECK.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTIMIMGTX-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.

    ZTBLOUR-ZFEDICK = 'O'.
* ¹ÝÀÔ½Å°í¹ø?
* IF ZTBLINR-ZFINRNO IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'. MESSAGE I167 WITH '¹ÝÀÔ½Å°í¹øÈ£'.   EXIT.
* ENDIF.
* ÀüÀÚ¹®¼­±â?
* IF ZTBLINR-ZFEDINF IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'. MESSAGE I167 WITH 'ÀüÀÚ¹®¼­±â´É'.   EXIT.
* ENDIF.
* ¹ÝÀÔ?
* IF ZTBLINR-ZFINDT  IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'. MESSAGE I167 WITH 'Carry-in date'.   EXIT.
* ENDIF.
* ¹ÝÀÔ½Ã?
* IF ZTBLINR-ZFINTM  IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'. MESSAGE I167 WITH 'Carry-in time'.  EXIT.
* ENDIF.
* ½Å°íÁö ¼¼?
* IF ZTBLINR-ZFINRC  IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'. MESSAGE I167 WITH '½Å°íÁö ¼¼°ü'.  EXIT.
* ENDIF.
* ¼¼°üÀÇ ´ã´ç °úºÎ?
* IF ZTBLINR-ZFINRCD IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'.
*    MESSAGE I167 WITH '¼¼°üÀÇ ´ã´ç °úºÎÈ£'.  EXIT.
* ENDIF.
* È­¹°¹ÝÀÔÀ¯Çü±¸?
* IF ZTBLINR-ZFINTY  IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'. MESSAGE I167 WITH 'È­¹°¹ÝÀÔÀ¯Çü±¸ºÐ'.  EXIT.
* ENDIF.
* ºÐÇÒ¹ÝÀÔ±¸?
* IF ZTBLINR-ZFPRIN  IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'. MESSAGE I167 WITH 'ºÐÇÒ¹ÝÀÔ±¸ºÐ'.  EXIT.
* ENDIF.
* ºÐÇÒ¹ÝÀÔÂ÷?
* IF NOT ( ZTBLINR-ZFPRIN IS INITIAL OR ZTBLINR-ZFPRIN EQ 'A' ).
*    IF ZTBLINR-ZFPINS IS INITIAL.
*       ZTBLINR-ZFEDICK = 'X'. MESSAGE I167 WITH 'ºÐÇÒ¹ÝÀÔÂ÷¼ö'.  EXIT.
*    ENDIF.
* ENDIF.
* ¹ÝÀÔÁ¤º¸ÀÇ ÀûÇÏ¸ñ·Ï°ü¸®¹ø?
* IF ZTBLINOU-ZFGMNO IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'.
*    MESSAGE I167 WITH '¹ÝÀÔÁ¤º¸ÀÇ ÀûÇÏ¸ñ·Ï°ü¸®¹øÈ£'.  EXIT.
* ENDIF.
* ¹ÝÀÔÁ¤º¸ÀÇ MSN
* IF ZTBLINOU-ZFMSN  IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'.
*    MESSAGE I167 WITH 'MSN of carry-in No'.  EXIT.
* ENDIF.
* ¹ÝÀÔÁ¤º¸ÀÇ HSN
* IF ZTBLINOU-ZFMSN  IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'.
*    MESSAGE I167 WITH 'HSN of carry-in Info'.  EXIT.
* ENDIF.
* ¹ÝÀÔÁ¤º¸ÀÇ ¹ÝÀÔ±Ù°Å¹ø?
* IF ZTBLINOU-ZFBTRNO IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'.
*    MESSAGE I167 WITH '¹ÝÀÔÁ¤º¸ÀÇ ¹ÝÀÔ±Ù°Å¹øÈ£'.  EXIT.
* ENDIF.
* Æ÷Àå°³¼ö ´Ü?
* IF ZTBLINR-ZFPKCNM IS INITIAL.
*    ZTBLINR-ZFEDICK = 'X'.
*    MESSAGE I167 WITH 'Unit of packing No'.  EXIT.
* ENDIF.
*
  ENDIF.






ENDFORM.                    " P2000_BLOUR_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_INVOICE_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_INVOICE_CREATE.

  SPOP-TITEL = 'Create Invoice : Input Commercial Invoice No.'.
  OPTION = 1.

  SET PARAMETER ID 'ZPCIVNO' FIELD ''.
  SET PARAMETER ID 'ZPHBLNO' FIELD ZTBL-ZFHBLNO.
  SET PARAMETER ID 'ZPBLNO'  FIELD ZTBL-ZFBLNO.
  SET PARAMETER ID 'BES'     FIELD ZTBL-ZFREBELN.
  SET PARAMETER ID 'ZPOPNNO' FIELD ZTBL-ZFOPNNO.
  SET PARAMETER ID 'ZPREQNO' FIELD ZVREQHD_ST-ZFREQNO.
  SET PARAMETER ID 'MJA'     FIELD MSEG-MJAHR.
  SET PARAMETER ID 'MBN'     FIELD MSEG-MBLNR.

  CALL SCREEN 0003 STARTING AT 20 5
                   ENDING   AT 80 15.

  IF ANTWORT EQ 'C' OR ANTWORT EQ 'N'.       " Cancel Or No
    SET SCREEN SY-DYNNR.
  ELSE.
    SET PARAMETER ID 'ZPCIVNO' FIELD ''.
    SET PARAMETER ID 'ZPHBLNO' FIELD ''.
    SET PARAMETER ID 'ZPBLNO'  FIELD ''.
    SET PARAMETER ID 'BES'     FIELD ''.
    SET PARAMETER ID 'ZPOPNNO' FIELD ''.
    SET PARAMETER ID 'ZPREQNO' FIELD ''.
    SET PARAMETER ID 'MJA'     FIELD ''.
    SET PARAMETER ID 'MBN'     FIELD ''.
* INVOICE No.
*     SET PARAMETER ID 'ZPCIVNO' FIELD ZTIV-ZFCIVNO.
* B/L No.
    SET PARAMETER ID 'ZPHBLNO' FIELD ''.
    SET PARAMETER ID 'ZPBLNO'  FIELD ZTBL-ZFBLNO.
* ¼öÀÔ¹®¼­¹ø?
    IF RADIO_RQ EQ 'X'.             " ¼öÀÔÀÇ·Ú °ü¸®¹øÈ£ ¼±ÅÃ?
      SET PARAMETER ID 'ZPREQNO' FIELD ZSREQHD-ZFREQNO.
      SET PARAMETER ID 'ZPOPNNO' FIELD ''.
      SET PARAMETER ID 'BES'     FIELD ''.
    ELSEIF RADIO_PO EQ 'X'.                 " P/O No. ¼±ÅÃ?
      SET PARAMETER ID 'ZPREQNO' FIELD ''.
      SET PARAMETER ID 'ZPOPNNO' FIELD ''.
      SET PARAMETER ID 'BES'     FIELD ZSREQHD-EBELN.
    ELSEIF RADIO_LC EQ 'X'.                 " P/O No. ¼±ÅÃ?
      SET PARAMETER ID 'ZPREQNO' FIELD ''.
      SET PARAMETER ID 'ZPOPNNO' FIELD ZSREQHD-ZFOPNNO.
      SET PARAMETER ID 'BES'     FIELD ''.
    ELSEIF RADIO_NPO EQ 'X'.                " ´ëÃ¼ ¹«?
      SET PARAMETER ID 'MJA'     FIELD MSEG-MJAHR.
      SET PARAMETER ID 'MBN'     FIELD MSEG-MBLNR.
    ELSEIF RADIO_NP EQ 'X'.                 " ¹«?
      SET PARAMETER ID 'ZPREQNO' FIELD ''.
      SET PARAMETER ID 'ZPOPNNO' FIELD ''.
      SET PARAMETER ID 'BES'     FIELD ''.
    ENDIF.

*========>
    CALL TRANSACTION 'ZIM31' AND SKIP  FIRST SCREEN.

* INVOICE DOCUMENT READ
    PERFORM   P1000_INVOICE_DOC_READ.
  ENDIF.

ENDFORM.                    " P3000_INVOICE_CREATE

*&---------------------------------------------------------------------*
*&      Form  P2000_IV_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_IV_DOC_DISPLAY USING    P_ZFIVNO.
*                                   P_ZFCIVNO.

*  IF P_ZFIVNO IS INITIAL AND P_ZFCIVNO IS INITIAL.
  IF P_ZFIVNO IS INITIAL.
    EXIT.
  ENDIF.

  SET PARAMETER ID 'ZPIVNO'  FIELD P_ZFIVNO.
*  SET PARAMETER ID 'ZPCIVNO' FIELD P_ZFCIVNO.
  CALL TRANSACTION 'ZIM33' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_IV_DOC_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  P1000_INVOICE_DOC_READ
*&---------------------------------------------------------------------*
FORM P1000_INVOICE_DOC_READ.
  REFRESH : IT_ZSIV, IT_ZSIVIT, IT_ZSCIVHD, IT_ZSCIVIT.
*>>> Åë°ü¿ë INVOICE
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIV
                FROM   ZTIV
                WHERE  ZFBLNO  EQ   ZTBL-ZFBLNO.
  IF SY-SUBRC EQ 0 AND SY-TCODE EQ 'ZIM22'.
    MESSAGE W533 WITH ZTBL-ZFBLNO.
  ENDIF.
*>> COMMERCIAL INVOICE.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSCIVIT
                FROM   ZTCIVIT
                WHERE  ZFBLNO  EQ   ZTBL-ZFBLNO.
  IF SY-SUBRC EQ 0 AND SY-TCODE EQ 'ZIM22'.
    MESSAGE I669 WITH ZTBL-ZFBLNO.
  ENDIF.
  SORT IT_ZSCIVIT BY ZFREQNO ZFITMNO ZFBLNO.

  ZTBL_ZFIVAMT = 0.
  LOOP AT IT_ZSIV.
    W_TABIX = SY-TABIX.
    IF NOT IT_ZSIV-ZFREQNO IS INITIAL.
* P/O NO
      SELECT SINGLE EBELN INTO IT_ZSIV-EBELN FROM ZTREQHD
                          WHERE ZFREQNO EQ IT_ZSIV-ZFREQNO.
* ¹°´ë °Å·¡?
      SELECT SINGLE NAME1 INTO IT_ZSIV-NAME2 FROM LFA1
                          WHERE LIFNR EQ IT_ZSIV-LIFNR.
      MODIFY IT_ZSIV INDEX W_TABIX.
      ZTBL_ZFIVAMT = ZTBL_ZFIVAMT + IT_ZSIV-ZFIVAMT.
      ZTBL_ZFIVAMC = IT_ZSIV-ZFIVAMC.
    ENDIF.
  ENDLOOP.

*-----------------------------------------------------------------------
* 2000/06/09   KSB Screen»ó Ref. Field Assign
* INTERNAL FORMATÀ» EXTERNAL FORMATÀ¸·Î º¯?
*    PERFORM SET_CURR_CONV_TO_EXTERNAL USING    ZTBL_ZFIVAMT
*                                               ZTBL_ZFIVAMC
*                                               ZTBL_ZFIVAMT.
*-----------------------------------------------------------------------

  SORT IT_ZSIV BY ZFIVNO.
* Item
  LOOP AT IT_ZSIV.
    SELECT *   APPENDING CORRESPONDING FIELDS OF TABLE IT_ZSIVIT
               FROM   ZTIVIT
               WHERE  ZFIVNO  EQ   IT_ZSIV-ZFIVNO.

  ENDLOOP.
  LOOP AT IT_ZSIVIT.
    W_TABIX = SY-TABIX.
*       SELECT R~EBELN R~ZFREQNO I~ZFIVNO
*           INTO (IT_ZSIVIT-EBELN, IT_ZSIVIT-ZFREQNO,
*                 IT_ZSIVIT-ZFIVNO)
*           FROM ZTREQHD AS R INNER JOIN ZTIV AS I
*           ON   R~ZFREQNO     EQ   I~ZFREQNO
*           WHERE I~ZFIVNO EQ IT_ZSIVIT-ZFIVNO.
*       ENDSELECT.
    IF NOT IT_ZSIVIT-ZFREQNO IS INITIAL.
*          SELECT SINGLE MENGE INTO IT_ZSIVIT-MENGE2 FROM ZTREQIT
*                              WHERE ZFREQNO EQ IT_ZSIVIT-ZFREQNO
*                              AND   ZFITMNO EQ IT_ZSIVIT-ZFIVDNO.
    ENDIF.
    IF SY-SUBRC EQ 0.
      MODIFY  IT_ZSIVIT   INDEX      W_TABIX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P1000_INVOICE_DOC_READ
*&---------------------------------------------------------------------*
*&      Form  P2000_HEADER_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P2000_HEADER_CHANGE_DOC.

  CASE SY-DYNNR.
    WHEN '0101'.          "> B/L
      OBJECTCLASS   =   'ZIBLDOC'.
      OBJEKTID      =   ZTBL+3(10).
    WHEN '0811'.          "> ÇÏ¿ª.
      OBJECTCLASS   =   'ZICGHD'.
      OBJEKTID      =   ZTCGHD+3(10).
    WHEN '2601'.          "> L/G
      OBJECTCLASS   =   'ZILGDOC'.
      OBJEKTID      =   ZTLG+3(15).
    WHEN '3510'.          "> »ó¾÷¼ÛÀå.
      OBJECTCLASS   =   'ZICIVHD'.
      OBJEKTID      =   ZTCIVHD+3(10).
    WHEN '3110'.          "> Åë°ü¿äÃ».
      OBJECTCLASS   =   'ZICCHD'.
      OBJEKTID      =   ZTIV+3(10).
    WHEN '7410'.
      OBJECTCLASS   =   'ZTIDS'.
      OBJEKTID      =   ZTIDS+3(15).
    WHEN '8210'.          "> PAYMENT NOTICE.
      OBJECTCLASS   =   'ZIPMTHD'.
      OBJEKTID      =   ZTPMTHD+3(10).
    WHEN '9201'.          "> ¹ÝÀÔ¿¹Á¤Á¤º¸.
      OBJECTCLASS   =   'ZIBLINOU'.
      OBJEKTID      =   ZTBLINOU+3(15).
    WHEN '9210'.          "> ¹ÝÀÔÁ¤º¸.
      OBJECTCLASS   =   'ZIBLINR'.
      OBJEKTID      =   ZTBLINR+3(15).
    WHEN '9215'.          "> ¹ÝÀÔÁ¤º¸.
      OBJECTCLASS   =   'ZIBLINR_TMP'.
      OBJEKTID      =   ZTBLINR_TMP+3(10).
    WHEN '9220' OR '9227'. "> ¹ÝÃâÁ¤º¸ OR ½Ç¹ÝÃâÃ³¸®.
      OBJECTCLASS   =   'ZIBLOUR'.
      OBJEKTID      =   ZTBLOUR+3(15).
    WHEN OTHERS.   EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_HEADER_CHANGE_DOC

*&---------------------------------------------------------------------*
*&      Form  SET_CURR_CONV_TO_EXTERNAL
*&---------------------------------------------------------------------*
FORM SET_CURR_CONV_TO_EXTERNAL USING    P_AMOUNT
                                        P_WAERS
                                        P_TO_AMOUNT.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
       EXPORTING
            CURRENCY        = P_WAERS
            AMOUNT_INTERNAL = P_AMOUNT
       IMPORTING
            AMOUNT_EXTERNAL = BAPICURR-BAPICURR.

  P_TO_AMOUNT = BAPICURR-BAPICURR.

ENDFORM.                    " SET_CURR_CONV_TO_EXTERNAL

*&---------------------------------------------------------------------*
*&      Form  P2000_CHARGE_CHANGE_MSG
*&---------------------------------------------------------------------*
FORM P2000_CHARGE_CHANGE_MSG.

  PERFORM P2000_MESSAGE_BOX USING 'Charge Record Change Confirm'
         'Update of the cost classfication becase of the changed via.'
         'Do you want to continue?'
                        'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                        '1'.                " default button

ENDFORM.                    " P2000_CHARGE_CHANGE_MSG

*&---------------------------------------------------------------------*
*&      Form  P2000_GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
FORM P2000_GET_EXCHANGE_RATE USING    P_WAERS
                                      P_DATE
                             CHANGING P_EXRATE
                                      P_FACTOR.


  DATA : L_TEXT_EXRATE(255)    TYPE C,
         L_FOREIGN_FACTOR(255) TYPE C,
         L_FACTOR              TYPE P,
         W_DATE_CON(8)         TYPE N,
         W_DATE(8)             TYPE C.

  CLEAR :  P_EXRATE, P_FACTOR, W_DATE.
  CLEAR :  TCURR.

* CURRENCY ¹× ÀÏÀÚ ¹ÌÀÔ·Â½Ã....
  IF P_WAERS IS INITIAL.   P_EXRATE = 0.  EXIT.   ENDIF.
  IF P_DATE  IS INITIAL.   P_EXRATE = 0.  EXIT.   ENDIF.

* MESSAGE BOX
  IF P_WAERS EQ 'USD'.             " KRW-> USD °íÄ§
    P_EXRATE = 1.
    EXIT.
  ENDIF.

  PERFORM   P2000_MESSAGE_EXCHANGE_RATE.
  CHECK ANTWORT EQ 'Y'.       " Yes

* P_DATE =  ZTBL-ZFBLDT.
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
       EXPORTING
            DATE             = P_DATE
            FOREIGN_AMOUNT   = 0
            FOREIGN_CURRENCY = P_WAERS
            LOCAL_CURRENCY   = 'USD'
            TYPE_OF_RATE     = V_TCURR-KURST
       IMPORTING
            EXCHANGE_RATE    = L_TEXT_EXRATE
            FOREIGN_FACTOR   = L_FOREIGN_FACTOR
            LOCAL_AMOUNT     = W_LOCAL_AMT
            FIXED_RATE       = W_FIXED_RATE
       EXCEPTIONS
            OTHERS           = 01.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
               RAISING AMOUNT_CALCULATION.
  ENDIF.

  PERFORM    P2000_WRITE_NO_MASK  CHANGING  L_TEXT_EXRATE.
  PERFORM    P2000_WRITE_NO_MASK  CHANGING  L_FOREIGN_FACTOR.

  P_EXRATE = L_TEXT_EXRATE.
  L_FACTOR = L_FOREIGN_FACTOR.
  IF P_EXRATE LT 0.
    P_EXRATE = P_EXRATE * -1.
  ENDIF.

  P_FACTOR = L_FACTOR.

  " DB Input Type Convert
  W_DATE     = P_DATE.
  W_DATE_CON = '99999999' - W_DATE.
  W_DATE     = W_DATE_CON.

*>> NCW ¼öÁ¤ 2003.12.02 - ISSUING DATE MESSAGE
  SELECT SINGLE * FROM TCURR
  WHERE  KURST    EQ   V_TCURR-KURST
  AND    FCURR    EQ   P_WAERS
  AND    TCURR    EQ   'USD'
  AND    GDATU    EQ   W_DATE.

  IF SY-SUBRC NE 0.
    MESSAGE I977 WITH
    'The exchange rate of issuing date does not exist'.
  ENDIF.

ENDFORM.                    " P2000_GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_EX_RATE_NODIALOG.
*&---------------------------------------------------------------------*
FORM P2000_GET_EX_RATE_NODIALOG        USING   P_WAERS
                                               P_WAERS_TO
                                               P_DATE
                                    CHANGING   P_EXRATE
                                               P_FACTOR.

  DATA : L_TEXT_EXRATE(255)    TYPE C,
         L_FOREIGN_FACTOR(255) TYPE C,
         L_FACTOR              TYPE P.

  CLEAR :  P_EXRATE, P_FACTOR.

*  No input CURRENCY , Exchage Date!
  IF P_WAERS IS INITIAL.   P_EXRATE = 0.  EXIT.   ENDIF.
  IF P_DATE  IS INITIAL.   P_EXRATE = 0.  EXIT.   ENDIF.

* MESSAGE BOX
  IF P_WAERS EQ P_WAERS_TO.
    P_EXRATE = 1.
    EXIT.
  ENDIF.

  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
       EXPORTING
            DATE             = P_DATE
            FOREIGN_AMOUNT   = 0
            FOREIGN_CURRENCY = P_WAERS
            LOCAL_CURRENCY   = P_WAERS_TO
            TYPE_OF_RATE     = 'M'
       IMPORTING
            EXCHANGE_RATE    = L_TEXT_EXRATE
            FOREIGN_FACTOR   = L_FOREIGN_FACTOR
            LOCAL_AMOUNT     = W_LOCAL_AMT
            FIXED_RATE       = W_FIXED_RATE
       EXCEPTIONS
            OTHERS           = 01.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
               RAISING AMOUNT_CALCULATION.
  ENDIF.

  PERFORM    P2000_WRITE_NO_MASK  CHANGING  L_TEXT_EXRATE.
  PERFORM    P2000_WRITE_NO_MASK  CHANGING  L_FOREIGN_FACTOR.

  P_EXRATE = L_TEXT_EXRATE.
  L_FACTOR = L_FOREIGN_FACTOR.
  IF P_EXRATE LT 0.
    P_EXRATE = P_EXRATE * -1.
  ENDIF.

  P_FACTOR = L_FACTOR.

ENDFORM.                    " P2000_GET_EX_RATE_NODIALOG

*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_NO_MASK
*&---------------------------------------------------------------------*
FORM P2000_WRITE_NO_MASK CHANGING P_TEXT_AMOUNT.

  SELECT SINGLE * FROM USR01 WHERE BNAME EQ SY-UNAME.

  CASE USR01-DCPFM.
    WHEN 'X'.    " Decimal point is period: N,NNN.NN
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT ',' ' '.
      CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
    WHEN 'Y'.    " Decimal point is N NNN NNN,NN
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  ',' '.'.
      CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
    WHEN OTHERS. " Decimal point is comma: N.NNN,NN
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  '.' ' '.
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  ',' '.'.
      CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
  ENDCASE.

ENDFORM.                    " P2000_WRITE_NO_MASK

*&---------------------------------------------------------------------*
*&      Form  P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
FORM P2000_CHANGE_SYMBOL USING    P_AMOUNT  P_FROM  P_TO.

  DO.
    REPLACE  P_FROM   WITH   P_TO  INTO    P_AMOUNT.
    IF  SY-SUBRC  <>    0.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_CHANGE_SYMBOL

*&---------------------------------------------------------------------*
*&      Form  P2000_TOTAL_CHG_CALC
*&---------------------------------------------------------------------*
FORM P2000_TOTAL_CHG_CALC.
  DATA : W_TOTAMT    LIKE   ZSREQHD-ZFTOTAMT.

  ZSREQHD-ZFKRW    = T001-WAERS.
  ZSREQHD-ZFTOTAMT = 0.
  ZSREQHD-ZFTOTWON = 0.

  LOOP AT IT_ZSBLCST.
    IF IT_ZSBLCST-WAERS NE T001-WAERS.
      ZSREQHD-ZFTOTAMT = ZSREQHD-ZFTOTAMT + IT_ZSBLCST-ZFCAMT.
      ZSREQHD-ZFTOTWON = ZSREQHD-ZFTOTWON + IT_ZSBLCST-ZFCKAMT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_TOTAL_CHG_CALC

*&---------------------------------------------------------------------*
*&      Form  P2000_ZTLG_DUP_LIST
*&---------------------------------------------------------------------*
FORM P2000_ZTLG_DUP_LIST.

  WRITE : / IT_ZTLG-ZFLGSEQ NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZTLG-ZFCARIR  NO-GAP,   SY-VLINE  NO-GAP,
          IT_ZTLG-ZFGSCD  NO-GAP,    SY-VLINE  NO-GAP,
          IT_ZTLG-ZFCIAM  CURRENCY IT_ZTLG-ZFCIAMC
                          NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTLG-ZFCIAMC NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTLG-ZFAPDT  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTLG-ZFISBNC  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTLG-ZFSHCU   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTLG-ZFARCUM  NO-GAP.

ENDFORM.                    " P2000_ZTLG_DUP_LIST

*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_SELECT
*&---------------------------------------------------------------------*
FORM P2000_LINE_SELECT.
  W_COUNT = 0.
  IF SY-DYNNR = '0104' OR SY-DYNNR = '0106' OR
     SY-DYNNR = '0131' OR SY-DYNNR = '0135'.  " ¿îÀÓÀÚµ¿°è»ê(ÇÑ¼ö¿ø)
    LOOP AT IT_ZSBLCST WHERE ZFMARK = 'X'.
      W_COUNT = W_COUNT + 1.
      MOVE-CORRESPONDING  IT_ZSBLCST  TO ZTBLCST.
    ENDLOOP.
  ELSEIF SY-DYNNR = '7420'.
    LOOP AT IT_ZSCUCLCST WHERE ZFMARK = 'X'.
      W_COUNT = W_COUNT + 1.
      MOVE-CORRESPONDING  IT_ZSCUCLCST TO ZTCUCLCST.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P2000_LINE_SELECT

*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_CALL_T_CODE
*&---------------------------------------------------------------------*
FORM P2000_LINE_CALL_T_CODE.
  DATA : L_AWKEY   LIKE   BKPF-AWKEY.
  CASE W_COUNT.
    WHEN 0.        MESSAGE S962.   EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965.   EXIT.
  ENDCASE.
  CASE OK-CODE.
    WHEN 'VEN1'.
      IF SY-DYNNR = '0104' OR SY-DYNNR = '0106' OR
         SY-DYNNR = '0131' OR SY-DYNNR = '0135'.  " ÇÑ¼ö¿ø(¿îÀÓÀÚµ¿)
        PERFORM  P2000_VENDOR_MASTER USING ZTBLCST-ZFVEN.
      ELSEIF SY-DYNNR = '7420'.
        PERFORM  P2000_VENDOR_MASTER USING ZTCUCLCST-ZFVEN.
      ENDIF.
    WHEN 'VEN2'.
      IF SY-DYNNR = '0104' OR SY-DYNNR = '0106' OR
         SY-DYNNR = '0131' OR SY-DYNNR = '0135'.  " ÇÑ¼ö¿ø(¿îÀÓÀÚµ¿)
        PERFORM  P2000_VENDOR_MASTER USING ZTBLCST-ZFPAY.
      ELSEIF SY-DYNNR = '7420'.
        PERFORM  P2000_VENDOR_MASTER USING ZTCUCLCST-ZFPAY.
      ENDIF.
    WHEN 'VEN3'.
      PERFORM  P2000_VENDOR_MASTER USING ZTBLCON-ZFTRCO.
    WHEN 'MKPF'.
      IF SY-DYNNR = '0104' OR SY-DYNNR = '0106' OR
         SY-DYNNR = '0131' OR SY-DYNNR = '0135'.  " ÇÑ¼ö¿ø(¿îÀÓÀÚµ¿)
        IF ZTBLCST-ZFACDO IS INITIAL OR ZTBLCST-ZFFIYR IS INITIAL.
          MESSAGE S252.   EXIT.
        ENDIF.
        SELECT * FROM EKBZ UP TO 1 ROWS
                 WHERE BELNR EQ ZTBLCST-ZFACDO
                 AND   GJAHR EQ ZTBLCST-ZFFIYR.
        ENDSELECT.
        IF SY-SUBRC EQ 0.
          SET PARAMETER  ID  'BUK'   FIELD ZTBLCST-BUKRS.
          SET  PARAMETER ID  'RBN'   FIELD   ZTBLCST-ZFACDO.
          SET  PARAMETER ID  'GJR'   FIELD   ZTBLCST-ZFFIYR.
          CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
        ELSE.
*               MESSAGE E443.
          SET PARAMETER ID 'BUK'     FIELD ZTBLCST-BUKRS.
          SET PARAMETER ID 'GJR'     FIELD ZTBLCST-ZFFIYR.
          SET PARAMETER ID 'BLN'     FIELD ZTBLCST-ZFACDO.
          CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
        ENDIF.
      ELSEIF SY-DYNNR = '7420'.
        IF ZTCUCLCST-ZFACDO IS INITIAL OR
           ZTCUCLCST-ZFFIYR IS INITIAL.
          MESSAGE S252.   EXIT.
        ENDIF.
        SELECT * FROM EKBZ UP TO 1 ROWS
                 WHERE BELNR EQ ZTCUCLCST-ZFACDO
                 AND   GJAHR EQ ZTCUCLCST-ZFFIYR.
        ENDSELECT.
        IF SY-SUBRC EQ 0.
          SET  PARAMETER ID  'RBN'   FIELD   ZTCUCLCST-ZFACDO.
          SET  PARAMETER ID  'GJR'   FIELD   ZTCUCLCST-ZFFIYR.
          CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
        ELSE.
          MESSAGE E443.
*               SET PARAMETER ID 'BUK'     FIELD ZTCUCLCST-BUKRS.
*               SET PARAMETER ID 'GJR'     FIELD ZTCUCLCST-ZFFIYR.
*               SET PARAMETER ID 'BLN'     FIELD ZTCUCLCST-ZFACDO.
*               CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
        ENDIF.
      ENDIF.
    WHEN 'BKPF'.
      IF SY-DYNNR = '0104' OR SY-DYNNR = '0106' OR
         SY-DYNNR = '0131' OR SY-DYNNR = '0135'.  " ÇÑ¼ö¿ø(¿îÀÓÀÚµ¿)
        IF ZTBLCST-ZFACDO IS INITIAL OR ZTBLCST-ZFFIYR IS INITIAL.
          MESSAGE S252.   EXIT.
        ENDIF.
        SELECT * FROM EKBZ UP TO 1 ROWS
                 WHERE BELNR EQ ZTBLCST-ZFACDO
                 AND   GJAHR EQ ZTBLCST-ZFFIYR.
        ENDSELECT.
        IF SY-SUBRC EQ 0.
          CLEAR : L_AWKEY.
          MOVE : ZTBLCST-ZFACDO  TO   L_AWKEY(10),
*                      ZTBLCST-BUKRS   TO   L_AWKEY+10(4),
                 ZTBLCST-ZFFIYR  TO   L_AWKEY+10(4).

          CLEAR : BKPF.
          SELECT * FROM BKPF UP TO 1 ROWS
                   WHERE AWKEY  EQ  L_AWKEY.
          ENDSELECT.
          IF SY-SUBRC EQ 0.
            SET  PARAMETER ID  'BUK'   FIELD   BKPF-BUKRS.
            SET  PARAMETER ID  'BLN'   FIELD   BKPF-BELNR.
            SET  PARAMETER ID  'GJR'   FIELD   BKPF-GJAHR.
            CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
          ENDIF.
        ELSE.
          SET PARAMETER ID 'BUK'     FIELD ZTBLCST-BUKRS.
          SET PARAMETER ID 'GJR'     FIELD ZTBLCST-ZFFIYR.
          SET PARAMETER ID 'BLN'     FIELD ZTBLCST-ZFACDO.
          CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
        ENDIF.
      ELSEIF SY-DYNNR = '7420'.
        IF ZTCUCLCST-ZFACDO IS INITIAL OR
           ZTCUCLCST-ZFFIYR IS INITIAL.
          MESSAGE S252.   EXIT.
        ENDIF.
        SELECT * FROM EKBZ UP TO 1 ROWS
                 WHERE BELNR EQ ZTCUCLCST-ZFACDO
                 AND   GJAHR EQ ZTCUCLCST-ZFFIYR.
        ENDSELECT.
        IF SY-SUBRC EQ 0.
          CLEAR : L_AWKEY.
          MOVE : ZTCUCLCST-ZFACDO  TO   L_AWKEY(10),
*                      ZTCUCLCST-BUKRS   TO   L_AWKEY+10(4),
                 ZTCUCLCST-ZFFIYR  TO   L_AWKEY+10(4).

          CLEAR : BKPF.
          SELECT * FROM BKPF UP TO 1 ROWS
                   WHERE AWKEY  EQ  L_AWKEY.
          ENDSELECT.
          IF SY-SUBRC EQ 0.
            SET  PARAMETER ID  'BUK'   FIELD   BKPF-BUKRS.
            SET  PARAMETER ID  'BLN'   FIELD   BKPF-BELNR.
            SET  PARAMETER ID  'GJR'   FIELD   BKPF-GJAHR.
            CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
          ENDIF.
        ELSE.
          SET PARAMETER ID 'BUK'     FIELD ZTCUCLCST-BUKRS.
          SET PARAMETER ID 'GJR'     FIELD ZTCUCLCST-ZFFIYR.
          SET PARAMETER ID 'BLN'     FIELD ZTCUCLCST-ZFACDO.
          CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_LINE_CALL_T_CODE

*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_SELECT_1
*&---------------------------------------------------------------------*
FORM P2000_LINE_SELECT_1.
  W_COUNT = 0.
  LOOP AT IT_ZSBLCST1 WHERE ZFMARK = 'X'.
    W_COUNT = W_COUNT + 1.
    MOVE-CORRESPONDING  IT_ZSBLCST1  TO ZTBLCST.
  ENDLOOP.

ENDFORM.                    " P2000_LINE_SELECT_1

*&---------------------------------------------------------------------*
*&      Form  P2000_CHARGE_PAY_METHOD
*&---------------------------------------------------------------------*
FORM P2000_CHARGE_PAY_METHOD.
* 2000/06/20   °­³ªÇü ´ë¸® DEFINE
* BASIC CHARGE ¿îÀÓÁöºÒ ¹æ?
  IF ZTBL-ZFTRTPM IS INITIAL.
    CASE ZTBL-INCO1.
      WHEN 'CFR'.    MOVE : 'P'     TO    ZTBL-ZFTRTPM.
      WHEN 'CIF'.    MOVE : 'P'     TO    ZTBL-ZFTRTPM.
      WHEN 'CIP'.    MOVE : 'P'     TO    ZTBL-ZFTRTPM.
      WHEN 'CPT'.    MOVE : 'P'     TO    ZTBL-ZFTRTPM.
      WHEN 'DAF'.    MOVE : 'P'     TO    ZTBL-ZFTRTPM.
      WHEN 'DDP'.    MOVE : 'P'     TO    ZTBL-ZFTRTPM.
      WHEN 'DDU'.    MOVE : 'P'     TO    ZTBL-ZFTRTPM.
      WHEN 'DEQ'.    MOVE : 'P'     TO    ZTBL-ZFTRTPM.
      WHEN 'DES'.    MOVE : 'P'     TO    ZTBL-ZFTRTPM.
      WHEN 'EXW'.    MOVE : 'C'     TO    ZTBL-ZFTRTPM.
      WHEN 'FAS'.    MOVE : 'C'     TO    ZTBL-ZFTRTPM.
      WHEN 'FCA'.    MOVE : 'C'     TO    ZTBL-ZFTRTPM.
      WHEN 'FH '.    MOVE : 'C'     TO    ZTBL-ZFTRTPM.
      WHEN 'FOB'.    MOVE : 'C'     TO    ZTBL-ZFTRTPM.
      WHEN 'UN '.    MOVE : 'C'     TO    ZTBL-ZFTRTPM.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
* OHTERS CHARGE ¿îÀÓÁöºÒ ¹æ?
  IF ZTBL-ZFOTHPM IS INITIAL.
    CASE ZTBL-INCO1.
      WHEN 'CFR'.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN 'CIF'.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN 'CIP'.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN 'CPT'.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN 'DAF'.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN 'DDP'.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN 'DDU'.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN 'DEQ'.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN 'DES'.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN 'EXW'.    MOVE : 'C'     TO    ZTBL-ZFOTHPM.
      WHEN 'FAS'.    MOVE : 'C'     TO    ZTBL-ZFOTHPM.
      WHEN 'FCA'.    MOVE : 'C'     TO    ZTBL-ZFOTHPM.
      WHEN 'FH '.    MOVE : 'C'     TO    ZTBL-ZFOTHPM.
      WHEN 'FOB'.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN 'UN '.    MOVE : 'P'     TO    ZTBL-ZFOTHPM.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_CHARGE_PAY_METHOD

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ZTBLINR_LOCK_MODE
*&---------------------------------------------------------------------*
FORM P2000_SET_ZTBLINR_LOCK_MODE USING    VALUE(PA_MODE).

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLINR'
         EXPORTING
              ZFBLNO  = ZTBLINR-ZFBLNO
              ZFBTSEQ = ZTBLINR-ZFBTSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Carry-in Info Document'
                            ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLINR'
         EXPORTING
              ZFBLNO  = ZTBLINR-ZFBLNO
              ZFBTSEQ = ZTBLINR-ZFBTSEQ.
  ENDIF.

ENDFORM.                    " P2000_SET_ZTBLINR_LOCK_MODE

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ZTBLOUR_LOCK_MODE
*&---------------------------------------------------------------------*
FORM P2000_SET_ZTBLOUR_LOCK_MODE USING    VALUE(PA_MODE).

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLOUR'
         EXPORTING
              ZFBLNO  = ZTBLOUR-ZFBLNO
              ZFBTSEQ = ZTBLOUR-ZFBTSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Carry-out Info Document'
                            ZTBLOUR-ZFBLNO ZTBLOUR-ZFBTSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLOUR'
         EXPORTING
              ZFBLNO  = ZTBLOUR-ZFBLNO
              ZFBTSEQ = ZTBLOUR-ZFBTSEQ.
  ENDIF.

ENDFORM.                    " P2000_SET_ZTBLOUR_LOCK_MODE
*&---------------------------------------------------------------------*
*&      Form  P2000_OUR_DUP_LIST
*&---------------------------------------------------------------------*
FORM P2000_OUR_DUP_LIST.

  WRITE : / IT_ZTBLOUR-ZFBTSEQ NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZTBLOUR-ZFOURNO  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTBLOUR-ZFABNAR  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTBLOUR-ZFOUTY   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTBLOUR-ZFOTDT   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZTBLOUR-ERNAM.

ENDFORM.                    " P2000_OUR_DUP_LIST

*&---------------------------------------------------------------------*
*&      Form  P2000_OUR_CHANGE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_OUR_CHANGE_CHECK.
* ¹®¼­ »ó?
  CASE ZTBLOUR-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTBLOUR-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTBLOUR-ZFBLNO 'EDI open requested' 'Change'.
      ELSEIF ZTBLINR-ZFEDIST = 'N'.
        MESSAGE E999 WITH ZTBLOUR-ZFBLNO 'Flat Data created'
                                        'Change'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTBLOUR-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTBLOUR-ZFBLNO 'EDI opened' 'Change'.
      ELSE.
        MESSAGE E999
                WITH ZTBLOUR-ZFBLNO 'Non-EDI opened' 'Change'.
      ENDIF.
      EXIT.
    WHEN 'C'.          " CANCEL
      MESSAGE E999 WITH ZTBLOUR-ZFBLNO 'CALCELµÈ' 'Change'.
  ENDCASE.

ENDFORM.                    " P2000_OUR_CHANGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_OUR_DISPLAY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_OUR_DISPLAY_CHECK.
* ¹®¼­ »ó?
  CASE ZTBLOUR-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTBLOUR-ZFEDIST = 'S'.
        MESSAGE I998 WITH ZTBLOUR-ZFBLNO 'EDI open requested'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTBLOUR-ZFEDIST = 'R'.
        MESSAGE I998 WITH ZTBLOUR-ZFBLNO 'EDI opened'.
      ELSE.
        MESSAGE I998 WITH ZTBLOUR-ZFBLNO 'Non-EDI opened'.
      ENDIF.
    WHEN 'C'.          " CANCEL
      MESSAGE I998 WITH ZTBLOUR-ZFBLNO 'CALCELED'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_OUR_DISPLAY_CHECK

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_KONP
*&---------------------------------------------------------------------*
FORM P1000_READ_KONP USING    P_MWSKZ
                              P_KBETR
                              P_KONWA.
  CLEAR : P_KBETR, P_KONWA.

  IF NOT P_MWSKZ IS INITIAL.
    SELECT * FROM  KONP
             WHERE KAPPL EQ 'TX'
             AND   KSCHL EQ 'KRIT'
             AND   MWSK1 EQ P_MWSKZ.

      MOVE: KONP-KBETR   TO   P_KBETR,         ">ºñÀ².
            KONP-KONWA   TO   P_KONWA.         ">´ÜÀ§.
* ======> À¯Àç¿À °úÀå '10'À¸·Î ³ª´®........
      IF NOT P_KBETR IS INITIAL.
        P_KBETR = P_KBETR / 10.
      ENDIF.
    ENDSELECT.
  ENDIF.

ENDFORM.                    " P1000_READ_KONP
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_SHIP_PORT_CURRENCY
*&---------------------------------------------------------------------*
FORM P2000_GET_SHIP_PORT_CURRENCY.

  IF NOT ZTBL-ZFCARC IS INITIAL.
    CLEAR : W_ZFTRTEC.
    SELECT WAERS   INTO W_ZFTRTEC FROM ZTIMIMG17 UP TO 1 ROWS
                   WHERE LAND1    EQ   ZTBL-ZFCARC
                   AND   ZFAPLDT  LE   SY-DATUM.
      EXIT.
    ENDSELECT.
    IF ZTBL-ZFTRTEC IS INITIAL.
      ZTBL-ZFTRTEC =  W_ZFTRTEC.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_GET_SHIP_PORT_CURRENCY

*&---------------------------------------------------------------------*
*&      Form  P2000_CHARGE_REFRESH_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_CHARGE_REFRESH_MSG.
  IF W_CHK_CNT > 0 .
    PERFORM P2000_MESSAGE_BOX USING 'Charge Record Refresh Confirm'
            'The data of accounts dealing exist.'
            'Do you want to continue?'
            'N'                            " Ãë¼Ò ¹öÆ° À¯/?
            '1'.                           " default button
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING 'Charge Record Refresh Confirm'
            'The cost classification will be updated.'
            'Do you want to continue?'
            'N'                 " Ãë¼Ò ¹öÆ° À¯/?
            '1'.                " default button
  ENDIF.

ENDFORM.                    " P2000_CHARGE_REFRESH_MSG

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_IMPORT_REQ_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_IMPORT_REQ_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = ZTREQST-ZFREQNO
              ZFAMDNO = ZTREQST-ZFAMDNO
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Import Document'
                            ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                   RAISING DOCUMENT_LOCKED.
    ENDIF.

  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = ZTREQST-ZFREQNO
              ZFAMDNO = ZTREQST-ZFAMDNO.
  ENDIF.

ENDFORM.                    " SET_IMPORT_REQ_LOCK

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTREQST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTREQHD_ZFREQNO  text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTREQST USING    P_ZFREQNO.

  SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO FROM ZTREQST
         WHERE ZFREQNO = P_ZFREQNO.

  SELECT SINGLE * FROM ZTREQST
         WHERE ZFREQNO = P_ZFREQNO
         AND   ZFAMDNO = W_ZFAMDNO.

  IF ZTREQST-ZFRLST1 NE 'R'.
*      MESSAGE E806 WITH ZTIV-ZFREQNO.
  ENDIF.

*   IF ZTIV-ZFPRPYN EQ 'Y'.
*      ZTIV-ZFPRTE = ZTREQHD-ZFPREPAY.
*   ENDIF.

* >> LOCK OBJECT ( ¼öÀÔÀÇ·Ú )
  IF SY-TCODE EQ 'ZIM31' OR SY-TCODE EQ 'ZIM32'.
    PERFORM  P2000_SET_IMPORT_REQ_LOCK    USING   'L'.
  ENDIF.

ENDFORM.                    " P1000_READ_ZTREQST

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_BL_DOC_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5004   text
*----------------------------------------------------------------------*
FORM P2000_SET_BL_DOC_LOCK USING   PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLDOC'
         EXPORTING
              ZFBLNO = ZTBL-ZFBLNO
         EXCEPTIONS
              OTHERS = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'B/L Document'
                            ZTBL-ZFBLNO ''
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLDOC'
         EXPORTING
              ZFBLNO = ZTBL-ZFBLNO.
  ENDIF.

ENDFORM.                    " P2000_SET_BL_DOC_LOCK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_IV_DOC_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_IV_DOC_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIV'
         EXPORTING
              ZFIVNO = ZTIV-ZFIVNO
         EXCEPTIONS
              OTHERS = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Clearance / G/R request'
                            ZTIV-ZFIVNO ''
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
         EXPORTING
              ZFIVNO = ZTIV-ZFIVNO.
  ENDIF.

ENDFORM.                    " P2000_SET_IV_DOC_LOCK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ZTPMTHD_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_ZTPMTHD_LOCK USING   PA_MODE.
  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTPMTHD'
         EXPORTING
              ZFPNNO = ZTPMTHD-ZFPNNO
         EXCEPTIONS
              OTHERS = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Payment Notice Document'
                            ZTPMTHD-ZFPNNO ''
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTPMTHD'
         EXPORTING
              ZFPNNO = ZTPMTHD-ZFPNNO.
  ENDIF.

ENDFORM.                    " P2000_SET_ZTPMTHD_LOCK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ZTIDR_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_ZTIDR_LOCK USING    PA_MODE.
  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIDR'
         EXPORTING
              ZFBLNO  = ZTIDR-ZFBLNO
              ZFCLSEQ = ZTIDR-ZFCLSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Import declaration Document'
                            ZTIDR-ZFBLNO ZTIDR-ZFCLSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIDR'
         EXPORTING
              ZFBLNO  = ZTIDR-ZFBLNO
              ZFCLSEQ = ZTIDR-ZFCLSEQ.
  ENDIF.

ENDFORM.                    " P2000_SET_ZTIDR_LOCK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ZTIDS_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_ZTIDS_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIDS'
         EXPORTING
              ZFBLNO  = ZTIDS-ZFBLNO
              ZFCLSEQ = ZTIDS-ZFCLSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Import license Document'
                            ZTIDS-ZFBLNO ZTIDS-ZFCLSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.

  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIDS'
         EXPORTING
              ZFBLNO  = ZTIDS-ZFBLNO
              ZFCLSEQ = ZTIDS-ZFCLSEQ.
  ENDIF.

ENDFORM.                    " P2000_SET_ZTIDS_LOCK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ZTCUCL_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_ZTCUCL_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTCUCL'
         EXPORTING
              ZFBLNO  = ZTCUCL-ZFBLNO
              ZFCLSEQ = ZTCUCL-ZFCLSEQ
         EXCEPTIONS
              OTHERS  = 1.
    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Clearance Document'
                            ZTCUCL-ZFBLNO  ZTCUCL-ZFCLSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCUCL'
         EXPORTING
              ZFBLNO  = ZTCUCL-ZFBLNO
              ZFCLSEQ = ZTCUCL-ZFCLSEQ.
  ENDIF.

ENDFORM.                    " P2000_SET_ZTCUCL_LOCK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ZTCUCLIV_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_ZTCUCLIV_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTCUCLIV'
         EXPORTING
              ZFIVNO = ZTCUCLIV-ZFIVNO
         EXCEPTIONS
              OTHERS = 1.
    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Clearance Invoice Document'
                            ZTCUCLIV-ZFIVNO  ''
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCUCLIV'
         EXPORTING
              ZFIVNO = ZTCUCLIV-ZFIVNO.
  ENDIF.


ENDFORM.                    " P2000_SET_ZTCUCLIV_LOCK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ZTIDRCR_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_ZTIDRCR_LOCK USING   PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIDRCR'
         EXPORTING
              ZFBLNO  = ZTIDRCR-ZFBLNO
              ZFCLSEQ = ZTIDRCR-ZFCLSEQ
              ZFCONO  = ZTIDRCR-ZFCONO
         EXCEPTIONS
              OTHERS  = 1.
    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Document of reduction permission'
                            ZTIDRCR-ZFBLNO ZTIDRCR-ZFCLSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIDRCR'
         EXPORTING
              ZFBLNO  = ZTIDRCR-ZFBLNO
              ZFCLSEQ = ZTIDRCR-ZFCLSEQ
              ZFCONO  = ZTIDRCR-ZFCONO.
  ENDIF.

ENDFORM.                    " P2000_SET_ZTIDRCR_LOCK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_IDRCR
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_IDRCR.

  REFRESH IT_ZSIDRCR.
  SELECT  * FROM ZTIDSHS
   WHERE ZFBLNO EQ ZTCUCL-ZFBLNO
     AND ZFCLSEQ EQ ZTCUCL-ZFCLSEQ.
    CLEAR IT_ZSIDRCR.
    MOVE ZTIDSHS-ZFBLNO        TO IT_ZSIDRCR-ZFBLNO.
    MOVE ZTIDSHS-ZFCLSEQ       TO IT_ZSIDRCR-ZFCLSEQ.
    MOVE ZTIDSHS-ZFCONO        TO IT_ZSIDRCR-ZFCONO.
    MOVE ZTIDSHS-STAWN         TO IT_ZSIDRCR-STAWN.
    MOVE ZTIDSHS-ZFTBAK        TO IT_ZSIDRCR-ZFTBAK.
    MOVE 'KRW'                 TO IT_ZSIDRCR-ZFKRW.
    MOVE ZTIDSHS-ZFTBAU        TO IT_ZSIDRCR-ZFTBAU.
    MOVE 'USD'                 TO IT_ZSIDRCR-ZFUSD.
    MOVE ZTIDSHS-ZFCUAMT       TO IT_ZSIDRCR-ZFCUAMT.
    MOVE ZTIDSHS-ZFHMAMT       TO IT_ZSIDRCR-ZFHMTX.
    MOVE ZTIDSHS-ZFEDAMT       TO IT_ZSIDRCR-ZFEDTX.
    MOVE ZTIDSHS-ZFAGAMT       TO IT_ZSIDRCR-ZFATX.
    MOVE ZTIDSHS-ZFVAAMT       TO IT_ZSIDRCR-ZFVTX.
    MOVE ZTIDSHS-ZFCCAMT       TO IT_ZSIDRCR-ZFCUDAK.
    SELECT MAX( ZFAPLDT ) INTO W_ZFAPLDT
      FROM ZTIMIMG06
     WHERE WAERS = 'USD'
       AND ZFAPLDT <= ZTIDS-ZFIDSDT
       AND ZFEXPDT >= ZTIDS-ZFIDSDT.
    IF W_ZFAPLDT IS INITIAL.
      ZTIMIMG06-ZFEXRT = 1200.
    ELSE.
      CLEAR ZTIMIMG06.
      SELECT SINGLE *
        FROM ZTIMIMG06
       WHERE WAERS = ZTIDS-ZFSTAMC
         AND ZFAPLDT = W_ZFAPLDT.
    ENDIF.
    IF NOT ( ZTIMIMG06-ZFEXRT IS INITIAL ).
      ZTIDRCR-ZFCUDAU = ZTIDRCR-ZFCUDAK / ZTIMIMG06-ZFEXRT * 100.
    ENDIF.
    MOVE ZTIDSHS-ZFRDRT        TO IT_ZSIDRCR-ZFRDRT.
*    IF ( ZTIDSHS-ZFCDPNO >= 'A00700010001' AND
*         ZTIDSHS-ZFCDPNO <= 'A00790000000' ).
*      MOVE '1'                 TO IT_ZSIDRCR-ZFDETY. " Ã·?
*    ENDIF.
*    IF ( ZTIDSHS-ZFCDPNO >= 'A028070101  ' AND
*         ZTIDSHS-ZFCDPNO <= 'A02807010402' ).
*      MOVE '2'                 TO IT_ZSIDRCR-ZFDETY. " ÀÚµ¿?
*    ENDIF.
*    IF ZTIDSHS-ZFCDPNO = 'A028050109  '.
*      IF ZTIDRCR-ZFMATGB = '4'.
*        MOVE '3'              TO IT_ZSIDRCR-ZFDETY. " ¿¬±¸¼Ò(½Ã¼³Àç)
*      ELSE.
*        MOVE '4'             TO IT_ZSIDRCR-ZFDETY. " ¿¬±¸¼Ò(¿øÀÚÀç)
*      ENDIF.
*    ENDIF.
*    IF ZTIDSHS-ZFCDPNO = 'A0370301    '.
*      MOVE '5'                TO IT_ZSIDRCR-ZFDETY. " ¿ëµµ¼¼?
*    ENDIF.
*    IF ( ZTIDSHS-ZFCDPNO >= 'A02900010001' AND
*         ZTIDSHS-ZFCDPNO <= 'A02900010023' ).
*      MOVE '6'                TO IT_ZSIDRCR-ZFDETY. " Àç¼öÃâÁ¶?
*    ENDIF.
*    IF ( ZTIDSHS-ZFCDPNO >= 'A034000001  ' AND
*         ZTIDSHS-ZFCDPNO <= 'A034000004  ' ).
*      MOVE '7'                TO IT_ZSIDRCR-ZFDETY. " Àç¼ö?
*    ENDIF.
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = ZTIDRCR-ZFBLNO.
    MOVE ZTBL-ZFMATGB         TO IT_ZSIDRCR-ZFMATGB.

    APPEND IT_ZSIDRCR.
  ENDSELECT.

ENDFORM.                    " P1000_GET_IT_IDRCR

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_IDRCRIT
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_IDRCRIT.

  REFRESH IT_ZSIDRCRIT.

  SELECT  * FROM ZTIDSHSD
   WHERE ZFBLNO EQ ZTCUCL-ZFBLNO
     AND ZFCLSEQ EQ ZTCUCL-ZFCLSEQ.
    CLEAR IT_ZSIDRCRIT.
*         MOVE-CORRESPONDING ZTIDSHSD TO IT_ZSIDRCRIT.
    MOVE ZTIDSHSD-ZFBLNO        TO IT_ZSIDRCRIT-ZFBLNO.
    MOVE ZTIDSHSD-ZFCLSEQ       TO IT_ZSIDRCRIT-ZFCLSEQ.
    MOVE ZTIDSHSD-ZFCONO        TO IT_ZSIDRCRIT-ZFCONO.
    MOVE ZTIDSHSD-ZFRONO        TO IT_ZSIDRCRIT-ZFRONO.
    MOVE ZTIDSHSD-ZFSTCD        TO IT_ZSIDRCRIT-MATNR.
    MOVE ZTIDSHSD-ZFGDDS1       TO IT_ZSIDRCRIT-MAKTX.
    MOVE ZTIDSHSD-ZFQNT         TO IT_ZSIDRCRIT-ZFDEQN.
    MOVE ZTIDSHSD-ZFQNTM        TO IT_ZSIDRCRIT-ZFDEQNM.
    MOVE ZTIDRCR-ZFDETY         TO IT_ZSIDRCRIT-ZFDETY.
*             °ú¼¼°¡°Ý....
    SELECT MAX( ZFREQNO ) INTO IT_ZSIDRCRIT-ZFREQNO
      FROM ZTREQHD
     WHERE EBELN = ZTBL-ZFREBELN.
    MOVE ZTBL-ZFMATGB           TO IT_ZSIDRCRIT-ZFMATGB.
    MOVE ZTBL-ZFPRNAM           TO IT_ZSIDRCRIT-ZFPRNAM.
    MOVE ZTIDS-ZFIDSDT          TO IT_ZSIDRCRIT-ZFIDSDT.
    IF IT_ZSIDRCRIT-ZFDETY = '1' OR IT_ZSIDRCRIT-ZFDETY = '3'.
      CALL FUNCTION 'ZIM_BEFORE_N_MONTH_DATE'
           EXPORTING
                DATE                      = IT_ZSIDRCRIT-ZFIDSDT
                W_MONTH                   = 12
           IMPORTING
                W_OUT_DATE                = W_DATE
           EXCEPTIONS
                PLAUSIBILITY_CHECK_FAILED = 4.
      MOVE W_DATE   TO     IT_ZSIDRCRIT-ZFEDDT.
    ENDIF.
    IF ZTIDRCRIT-ZFDETY = '2'.
      CALL FUNCTION 'ZIM_BEFORE_N_MONTH_DATE'
           EXPORTING
                DATE                      = IT_ZSIDRCRIT-ZFIDSDT
                W_MONTH                   = 1
           IMPORTING
                W_OUT_DATE                = W_DATE
           EXCEPTIONS
                PLAUSIBILITY_CHECK_FAILED = 4.
      MOVE W_DATE   TO     IT_ZSIDRCRIT-ZFEDDT.
    ENDIF.
    MOVE 'KRW'                  TO IT_ZSIDRCRIT-ZFKRW.
    MOVE 'USD'                  TO IT_ZSIDRCRIT-ZFUSD.

    APPEND IT_ZSIDRCRIT.
  ENDSELECT.

ENDFORM.                    " P1000_GET_IT_IDRCRIT

*&---------------------------------------------------------------------*
*&      Form  P3000_UPDATE_IV_STATUS
*&---------------------------------------------------------------------*
FORM P3000_UPDATE_IV_STATUS USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_ZFAPLDT.
  IF ZTIDS-ZFSTAMC EQ 'KRW'.
    ZTIMIMG06-ZFEXRT = 1.
  ELSE.
    SELECT MAX( ZFAPLDT ) INTO W_ZFAPLDT
      FROM ZTIMIMG06
     WHERE WAERS = ZTIDS-ZFSTAMC
       AND ZFAPLDT <= ZTIDS-ZFIDSDT
       AND ZFEXPDT >= ZTIDS-ZFIDSDT.
    IF W_ZFAPLDT IS INITIAL.
      MESSAGE E780 WITH ZTIDS-ZFIDSDT.
      EXIT.
    ENDIF.
    CLEAR ZTIMIMG06.
    SELECT SINGLE *
      FROM ZTIMIMG06
     WHERE WAERS = ZTIDS-ZFSTAMC
       AND ZFAPLDT = W_ZFAPLDT.
  ENDIF.

  UPDATE ZTCUCL
     SET ZFIDSDT = ZTIDS-ZFIDSDT
         ZFEXRT = ZTIMIMG06-ZFEXRT
         ZFIDRAMU = ZTIDS-ZFTBAU
         ZFIDRAM = ZTIDS-ZFTBAK
         ZFIDRNO = ZTIDS-ZFIDRNO
         ZFCUST = 'Y'
         UNAM = SY-UNAME
         UDAT = SY-DATUM
   WHERE ZFBLNO = ZTIDS-ZFBLNO
     AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
  IF SY-SUBRC NE 0.
    MOVE 'Y' TO W_ERR_CHK.
    EXIT.
  ENDIF.
*
  SELECT *
    FROM ZTCUCLIV
   WHERE ZFBLNO = ZTIDS-ZFBLNO
     AND ZFCLSEQ = ZTIDS-ZFCLSEQ.
* 01/01/15 ±è¿¬Áß.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
         EXPORTING
              CURRENCY        = ZTCUCLIV-ZFIVAMC
              AMOUNT_INTERNAL = ZTCUCLIV-ZFIVAMT
         IMPORTING
              AMOUNT_EXTERNAL = W_ZFIVAMT_EX.
    W_ZFIVAMT = ZTIMIMG06-ZFEXRT * W_ZFIVAMT_EX.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
         EXPORTING
              CURRENCY             = 'KRW'
              AMOUNT_EXTERNAL      = W_ZFIVAMT
              MAX_NUMBER_OF_DIGITS = DIGITS
         IMPORTING
              AMOUNT_INTERNAL      = W_ZFIVAMT
         EXCEPTIONS
              OTHERS               = 1.
    MOVE W_ZFIVAMT TO ZTCUCLIV-ZFIVAMK.
    UPDATE ZTCUCLIV
       SET ZFIVAMK = ZTCUCLIV-ZFIVAMK
           ZFCUST = 'Y'
           UNAM = SY-UNAME
           UDAT = SY-DATUM
     WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
    IF SY-SUBRC NE 0.
      MOVE 'Y' TO W_ERR_CHK.
      EXIT.
    ENDIF.
    SELECT SINGLE *
      FROM ZTIV
     WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
    IF SY-SUBRC NE 0.
      MOVE 'Y' TO W_ERR_CHK.
      EXIT.
    ENDIF.
    SELECT *
      FROM ZTIVIT
     WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
* 01/01/15 ±è¿¬Áß.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
           EXPORTING
                CURRENCY        = ZTIVIT-ZFIVAMC
                AMOUNT_INTERNAL = ZTIVIT-ZFIVAMT
           IMPORTING
                AMOUNT_EXTERNAL = W_ZFIVAMT_EX.
      W_ZFIVAMT = ZTIMIMG06-ZFEXRT * W_ZFIVAMT_EX.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
           EXPORTING
                CURRENCY             = 'KRW'
                AMOUNT_EXTERNAL      = W_ZFIVAMT
                MAX_NUMBER_OF_DIGITS = DIGITS
           IMPORTING
                AMOUNT_INTERNAL      = W_ZFIVAMT
           EXCEPTIONS
                OTHERS               = 1.
      MOVE W_ZFIVAMT TO ZTIVIT-ZFIVAMK.
      UPDATE ZTIVIT.
      IF SY-SUBRC NE 0.
        MOVE 'Y' TO W_ERR_CHK.
        EXIT.
      ENDIF.
    ENDSELECT. "ZTIVIT
    SELECT SUM( ZFIVAMK ) INTO ZTIV-ZFIVAMK
      FROM ZTIVIT
     WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.

* 01/01/15 ±è¿¬Áß.
    IF W_STATUS = 'C' AND ZTIV-ZFPOYN = 'N'. "¹«È¯.
      MOVE 'Y' TO ZTIV-ZFCDST.
*            MOVE 'Y' TO ZTIV-ZFIVST.
*            MOVE 'Y' TO ZTIV-ZFGRST.
*            MOVE 'Y' TO ZTIV-ZFCIVST.
*            IF ZTIV-ZFPONMA = 'Y'. "¹«È¯¼öÃâÀÔ.
*               MOVE 'N' TO ZTIV-ZFGRST.
*               MOVE 'N' TO ZTIV-ZFCIVST.
*            ENDIF.
    ENDIF.

    UPDATE ZTIV
       SET ZFIVAMK = ZTIV-ZFIVAMK
           ZFEXRT = ZTIMIMG06-ZFEXRT
           ZFCUST = 'Y'
           ZFCDST = ZTIV-ZFCDST
*                ZFIVST = ZTIV-ZFIVST
           ZFGRST = ZTIV-ZFGRST
           ZFCIVST = ZTIV-ZFCIVST
           UNAM = SY-UNAME
           UDAT = SY-DATUM
     WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
    IF SY-SUBRC NE 0.
      MOVE 'Y' TO W_ERR_CHK.
      EXIT.
    ENDIF.
  ENDSELECT.

ENDFORM.                    " P3000_UPDATE_IV_STATUS

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_IN_TO_OUT
*&---------------------------------------------------------------------*
FORM P3000_DATA_IN_TO_OUT USING    W_ERR_CHK.

  CLEAR W_ZFBTSEQ.
  SELECT MAX( ZFBTSEQ ) INTO W_ZFBTSEQ
    FROM ZTBLINR
   WHERE ZFBLNO = ZTIDS-ZFBLNO.
  IF W_ZFBTSEQ IS INITIAL.
    EXIT.
  ENDIF.
  SELECT SINGLE *
    FROM ZTBLINR
   WHERE ZFBLNO EQ ZTIDS-ZFBLNO
     AND ZFBTSEQ EQ W_ZFBTSEQ.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.
  DELETE FROM ZTBLOUR
   WHERE ZFBLNO = ZTIDS-ZFBLNO
     AND ZFBTSEQ EQ W_ZFBTSEQ.

  CLEAR : ZTBLOUR.
  MOVE : ZTBLINR-MANDT       TO     ZTBLOUR-MANDT,    "Client
         ZTBL-ZFBLNO         TO     ZTBLOUR-ZFBLNO,   "B/L °ü¸®¹ø?
         ZTBLINR-ZFBTSEQ     TO     ZTBLOUR-ZFBTSEQ,  "º¸¼¼¿î¼Û ÀÏ·Ã?
         ZTBLINR-ZFINRNO     TO     ZTBLOUR-ZFOURNO,  "¹ÝÃâ½Å°í¹ø?
         ZTBLINR-ZFABNAR     TO     ZTBLOUR-ZFABNAR,  "º¸¼¼±¸¿ª CODE
         ZTBLINR-ZFBNARCD    TO     ZTBLOUR-ZFBNARCD, "º¸¼¼±¸¿ª ID
         ZTBLINR-ZFYR        TO     ZTBLOUR-ZFYR,     "¿¬?
         ZTBLINR-ZFSEQ       TO     ZTBLOUR-ZFSEQ,    "ÀÏ·Ã¹ø?
         '9'                 TO     ZTBLOUR-ZFEDINF,  "ÀüÀÚ¹®¼­±â?
         ZTBLINR-ZFINRC      TO     ZTBLOUR-ZFINRC,   "½Å°íÁö ¼¼?
         ZTBLINR-ZFINRCD     TO     ZTBLOUR-ZFINRCD,  "¼¼°üÀÇ ´ã´ç °ú?
         ZTBLINR-ZFPINS      TO     ZTBLOUR-ZFPOUS,   "ºÐÇÒ¹ÝÃâ Â÷?
         ZTBLINR-ZFPRIN      TO     ZTBLOUR-ZFPROU,   "ºÐÇÒ¹ÝÃâ±¸?
         SPACE               TO     ZTBLOUR-ZFPRDS,   "¹ÝÃâ±â°£ ¿¬Àå?
         ZTBLINR-ZFCYCFS     TO     ZTBLOUR-ZFCYCFS,  "CY/CFS ±¸?
         SPACE               TO     ZTBLOUR-ZFPRDS,   "±â°£¿¬Àå ±Ù°Å?
         ZTBLINR-ZFPKCN      TO     ZTBLOUR-ZFOUQN,   "ÃÑ¹ÝÃâ°³?
         ZTBLINR-ZFPKCNM     TO     ZTBLOUR-ZFOUQNM,  "ÃÑ¹ÝÃâ°³¼ö ´Ü?
         ZTBLINR-ZFINWT      TO     ZTBLOUR-ZFOUWT,   "¹ÝÃâÁß?
         ZTBLINR-ZFINTWT     TO     ZTBLOUR-ZFOUTWT,  "´©°è¹ÝÃâÁß?
         ZTBLINR-ZFINTQN     TO     ZTBLOUR-ZFOUTQN,  "´©°è¹ÝÃâ°³?
         ZTBLINR-ZFKG        TO     ZTBLOUR-ZFKG,     "¹«°Ô´Ü?
         ZTBLINR-ZFCT        TO     ZTBLOUR-ZFCT,     "°³¼ö´Ü?
         ZTBLINR-ZFPINS      TO     ZTBLOUR-ZFPOUS,   "ºÐÇÒ¹ÝÃâÂ÷?
*         SPACE               TO     ZTBLOUR-ZFADNO,   "Á¢¼ö/¿À·ù ÀüÀÚ?
*          '61'                TO     ZTBLOUR-ZFOUTY,   "¹ÝÃâÀ¯?
*          SPACE               TO     ZTBLOUR-ZFOUANO,  "¹ÝÃâ±Ù°Å¹ø?
         '50'                TO     ZTBLOUR-ZFOUTY,   "¹ÝÃâÀ¯?
         ZTIDS-ZFIDRNO       TO     ZTBLOUR-ZFOUANO,  "¹ÝÃâ±Ù°Å¹ø?
*         ZTBLINR-ZFBLNO      TO     ZTBLOUR-ZFOTDT,   "¹ÝÃâÇã°¡?
         SY-DATUM            TO     ZTBLOUR-ZFOTDT,   "¹ÝÃâ?
         SY-UZEIT            TO     ZTBLOUR-ZFOUTM,   "¹ÝÃâ½Ã?
*         ZTBLINR-ZFBLNO      TO     ZTBLOUR-ZFAOUDT,  "½Ç¹ÝÃâ?
*         ZTBLINR-ZFBLNO      TO     ZTBLOUR-ZFAOUTM,  "½Ç¹ÝÃâ½Ã?
*         SY-UNAME            TO     ZTBLOUR-ZFRENM,   "ÀÎ¼ö?
*         ZTBLINR-ZFBLNO      TO     ZTBLOUR-ZFREDP,   "ÀÎ¼öºÎ?
         'O'                 TO     ZTBLOUR-ZFDOCST,  "DOC. Status
         'N'                 TO     ZTBLOUR-ZFEDIST,  "EDI Status
         'O'                 TO     ZTBLOUR-ZFEDICK,  "EDI Check
         SPACE               TO     ZTBLOUR-ZFDOCNO,  "ÀüÀÚ¹®¼­¹ø?
         SPACE               TO     ZTBLOUR-ZFDOCNOR, "ÀüÀÚ¹®¼­¹ø?
         SY-UNAME            TO     ZTBLOUR-ERNAM,    "Name of person w
         SY-DATUM            TO     ZTBLOUR-CDAT,     "Created on
         SY-UNAME            TO     ZTBLOUR-UNAM,     "Last changed by
         SY-DATUM            TO     ZTBLOUR-UDAT.     "Last changed on
  INSERT ZTBLOUR.
  IF SY-SUBRC NE 0.
    MESSAGE E057.
    EXIT.
  ENDIF.

ENDFORM.                    " P3000_DATA_IN_TO_OUT

*&---------------------------------------------------------------------*
*&      Form  P3000_DELETE_DOCUMENT6410
*&---------------------------------------------------------------------*
FORM P3000_DELETE_SCR6410.

  DELETE FROM ZTIDRCR WHERE ZFBLNO EQ  ZTCUCL-ZFBLNO AND
                              ZFCLSEQ EQ ZTCUCL-ZFCLSEQ.
  DELETE FROM ZTIDRCRIT WHERE  ZFBLNO EQ   ZTCUCL-ZFBLNO AND
                               ZFCLSEQ EQ  ZTCUCL-ZFCLSEQ.
  IF SY-SUBRC EQ 0.
    MESSAGE S339.
    EXIT.
  ELSE.
    MESSAGE E347.
  ENDIF.

ENDFORM.                    "P3000_DELETE_SCR6410
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_EDI_INDICATE
*&---------------------------------------------------------------------*
FORM P1000_GET_EDI_INDICATE USING    P_ZFINRC
                                     P_ZFDHSRO.
  IF P_ZFINRC IS INITIAL.
    MESSAGE E167 WITH 'Customs code'.
  ELSE.
    SELECT SINGLE * FROM ZTIMIMG02
                    WHERE ZFCOTM EQ P_ZFINRC.
    IF SY-SUBRC NE 0.
      MESSAGE E231 WITH P_ZFINRC.
    ELSE.
      SELECT SINGLE * FROM LFA1
                      WHERE LIFNR EQ ZTIMIMG02-ZFVEN.
      IF SY-SUBRC EQ 0.
        IF LFA1-BAHNS IS INITIAL.
          MESSAGE E198 WITH ZTIMIMG02-ZFVEN.
        ENDIF.
      ELSE.
        MESSAGE E020 WITH ZTIMIMG02-ZFVEN.
      ENDIF.
    ENDIF.
  ENDIF.

  P_ZFDHSRO = LFA1-BAHNS.

ENDFORM.                    " P1000_GET_EDI_INDICATE
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSBLIT_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_IT_ZSBLIT_UPDATE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSBLIT.
    IT_ZSBLIT-ZFBLIT  =  SY-TABIX * 10.
    MODIFY IT_ZSBLIT.
  ENDLOOP.

ENDFORM.                    " P2000_IT_ZSBLIT_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_SELECT_BL_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_LINE_SELECT_BL_ITEM.
  CLEAR : ZSBLIT.
  W_COUNT = 0.
  LOOP AT IT_ZSBLIT WHERE ZFMARK = 'X'.
    W_COUNT = W_COUNT + 1.
    MOVE-CORRESPONDING  IT_ZSBLIT  TO ZSBLIT.
  ENDLOOP.

ENDFORM.                    " P2000_LINE_SELECT_BL_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_CALL_TCODE_BL_ITEM
*&---------------------------------------------------------------------*
FORM P2000_LINE_CALL_TCODE_BL_ITEM.
  CASE W_COUNT.
    WHEN 0.        MESSAGE S962.   EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965.   EXIT.
  ENDCASE.
  CASE  W_OK_CODE.
*2002/12/18 JSY Ãß°¡ Ã»±¸ÀÚÁ¶È¸
    WHEN 'SU01'.
      PERFORM P2000_REQUESTER_DISPLAY USING ZSBLIT-EBELN
                                            ZSBLIT-EBELP.
    WHEN 'PODP'.          " P/O DISPLAY
      IF SY-DYNNR EQ '0113'.
        PERFORM  P2000_SO_DOC_DISPLAY USING ZSBLIT-VBELN.
      ELSE.
        PERFORM  P2000_PO_DOC_DISPLAY USING ZSBLIT-EBELN  ''.
      ENDIF.
    WHEN 'IMDP'.          " IMPORT L/C DISPLAY
      PERFORM  P2000_LC_DOC_DISPLAY USING ZSBLIT-ZFREQNO ''.
    WHEN 'VL31'.          ">> INBOUND
      IF ZTBL-LIFNR   IS INITIAL.
        MESSAGE S174.   EXIT.
      ENDIF.
      IF ZSBLIT-EBELN IS INITIAL.
        MESSAGE S003.   EXIT.
      ELSE.

        SELECT * FROM EKPO
                 WHERE EBELN  EQ  ZSBLIT-EBELN
                 AND   BSTAE  IN ( SELECT BSTAE FROM T163L ).
        ENDSELECT.
        IF SY-SUBRC NE 0.
          MESSAGE E148(VL).     EXIT.
        ENDIF.

        REFRESH : BDCDATA.
        PERFORM P2000_DYNPRO USING :
                'X' 'SAPMV50B'        '0107',
                ' ' 'LIKP-LIFNR'      ZTBL-LIFNR,
                ' ' 'LV50C-BSTNR'     ZSBLIT-EBELN,
                ' ' 'RV50A-LFDAT_LA'  ZTBL-ZFETA,
                ' ' 'BDC_OKCODE'      '/00'.

        CALL TRANSACTION 'VL31N'      USING       BDCDATA
                          MODE        'E'
*                             MODE        MODE
                          UPDATE      'S'
                          MESSAGES    INTO   MESSTAB.


*            SET PARAMETER ID 'LIF' FIELD  ZTBL-LIFNR.
*            SET PARAMETER ID 'BES' FIELD  ZSBLIT-EBELN.

*            CALL TRANSACTION 'VL31N' AND SKIP  FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_LINE_CALL_TCODE_BL_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_AIR_BASIC_CHG_CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_AIR_BASIC_CHG_CALC.

  SELECT MAX( ZFAPLDT ) INTO W_APLDT FROM ZTIMIMG17
            WHERE  LAND1    EQ    ZTBL-ZFCARC
            AND    WAERS    EQ    ZTBL-ZFTRTEC
            AND    ZFAPLDT  LE    SY-DATUM
            AND    ZFCD     EQ    WL_VIA.
  IF W_APLDT IS INITIAL.   MESSAGE E240 WITH WL_VIA.   ENDIF.
  CLEAR : ZTIMIMG17.
  SELECT SINGLE * FROM   ZTIMIMG17
            WHERE  LAND1    EQ    ZTBL-ZFCARC
            AND    WAERS    EQ    ZTBL-ZFTRTEC
            AND    ZFCD     EQ    WL_VIA
            AND    ZFAPLDT  EQ    W_APLDT.
  CHECK : SY-SUBRC EQ 0.

* Chargeable Weight
  IF ZTBL-ZFTOWT GE 3000.       " 3000K Great or Equal NepPrice
    W_ZTBL_ZFTRTE = ZTIMIMG17-ZF3000KGE.
  ELSEIF ZTBL-ZFTOWT GE 1000.   " 1000K - 3000K
    W_ZTBL_ZFTRTE = ZTIMIMG17-ZF3000KLT.
  ELSEIF ZTBL-ZFTOWT GE 100.    " 100K - 1000K
    W_ZTBL_ZFTRTE = ZTIMIMG17-ZF1000KLT.
  ELSEIF ZTBL-ZFTOWT GE 45.     " 45K - 100K
    W_ZTBL_ZFTRTE = ZTIMIMG17-ZF100KLT.
  ELSE.                                                     " 45K
    W_ZTBL_ZFTRTE = ZTIMIMG17-ZF45KLT.
  ENDIF.

  W_ZSBLCST_ZFCAMT2 = W_ZTBL_ZFTRTE * ZTBL-ZFTOWT.
  IF ZTIMIMG17-ZFMINPR GT W_ZSBLCST_ZFCAMT2.
    W_ZSBLCST_ZFCAMT2 = ZTIMIMG17-ZFMINPR.
  ENDIF.

*-----------------------------------------------------------------------

  SPOP-TITEL = 'Basic Charge ReCalcuration...'.
  OPTION = 1.
  CALL SCREEN 0107 STARTING AT 08 4
                   ENDING   AT 95 18.


ENDFORM.                    " P2000_AIR_BASIC_CHG_CALC
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_BL_ITEM
*&---------------------------------------------------------------------*
FORM P1000_GET_BL_ITEM.

*>> < NHJ 2002.09.16> Non-Monetary Transaction & for compensation
  IF ZSBL-ZFREBELN NE SPACE AND ZTBL-ZFPOYN = 'N'.
    IF NOT ZSBL-ZFBLNO IS INITIAL.
      SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZSBLIT
               FROM  ZTBLIT
               WHERE ZFBLNO    EQ  ZSBL-ZFBLNO
               AND   EBELN     EQ  ZSBL-ZFREBELN.
    ELSE.
      SELECT   ZFBLNO INTO ZSBL-ZFBLNO
               FROM  ZTBL UP TO 1 ROWS
               WHERE ZFREBELN  EQ  ZSBL-ZFREBELN
               ORDER BY ZFBLNO DESCENDING.
      ENDSELECT.

      SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZSBLIT
               FROM  ZTBLIT
               WHERE ZFBLNO    EQ  ZSBL-ZFBLNO
               AND   EBELN     EQ  ZSBL-ZFREBELN.
    ENDIF.

    W_COUNT = 0.
    LOOP AT IT_ZSBLIT.
      W_TABIX = SY-TABIX.
      ADD   1   TO   W_COUNT.
      CLEAR : IT_ZSBLIT-ZFBLNO, IT_ZSBLIT-ZFBLIT, EKPO.

      " P/O DATA GET.
      SELECT SINGLE * FROM EKPO
             WHERE  EBELN  EQ   IT_ZSBLIT-EBELN
             AND    EBELP  EQ   IT_ZSBLIT-EBELP.
      IF EKPO-PSTYP EQ '9'.
        MOVE 'X'   TO  ZTBL-ZFSVYN.
        DELETE  IT_ZSBLIT  INDEX  W_TABIX.
        CONTINUE.
      ENDIF.

      MOVE : ZTBL-ZFPOTY TO   IT_ZSBLIT-ZFPOTY,
             SY-UNAME    TO   IT_ZSBLIT-ERNAM,
             SY-DATUM    TO   IT_ZSBLIT-CDAT,
             SY-UNAME    TO   IT_ZSBLIT-UNAM,
             SY-DATUM    TO   IT_ZSBLIT-UDAT.

      MODIFY  IT_ZSBLIT INDEX W_TABIX.
    ENDLOOP.
    EXIT.
  ELSE.

  ENDIF.
*>> TO
  IMPORT  IT_ZSBLIT  FROM MEMORY ID 'BLIT'.
  FREE MEMORY ID 'BLIT'.
  IF IT_ZSBLIT[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLIT
             FROM ZTREQIT
             WHERE ZFREQNO EQ ZVREQHD_ST-ZFREQNO.
  ENDIF.

  W_COUNT = 0.

  LOOP AT IT_ZSBLIT.

    W_TABIX = SY-TABIX.
*-----------------------------------------------------------------------
*++++> P/O DATA Display.
    SELECT SINGLE * FROM EKPO
           WHERE  EBELN  EQ   IT_ZSBLIT-EBELN
           AND    EBELP  EQ   IT_ZSBLIT-EBELP.

    IF EKPO-PSTYP EQ '9'.
      MOVE 'X'   TO  ZTBL-ZFSVYN.
      DELETE  IT_ZSBLIT  INDEX  W_TABIX.
      CONTINUE.
    ENDIF.
    MOVE : EKPO-MENGE        TO   IT_ZSBLIT-MENGE_PO,
           EKPO-UEBTO        TO   IT_ZSBLIT-UEBTO,
           EKPO-UEBTK        TO   IT_ZSBLIT-UEBTK,
           EKPO-WEPOS        TO   IT_ZSBLIT-WEPOS,
           EKPO-LOEKZ        TO   IT_ZSBLIT-LOEKZ,
           EKPO-UNTTO        TO   IT_ZSBLIT-UNTTO,
           EKPO-WERKS        TO   IT_ZSBLIT-WERKS,
           EKPO-LGORT        TO   IT_ZSBLIT-LGORT,
           EKPO-MATKL        TO   IT_ZSBLIT-MATKL,
           EKPO-BPUMZ        TO   IT_ZSBLIT-BPUMZ,
           EKPO-BPUMN        TO   IT_ZSBLIT-BPUMN,
           EKPO-ELIKZ        TO   IT_ZSBLIT-ELIKZ.

    MOVE : IT_ZSBLIT-EBELN   TO   IT_ZSBLIT-EBELN,
           IT_ZSBLIT-EBELP   TO   IT_ZSBLIT-EBELP.

    IF SY-SUBRC EQ 0.
      IF IT_ZSBLIT-LOEKZ NE SPACE.
        DELETE IT_ZSBLIT   INDEX   W_TABIX.
        MESSAGE W069 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
        CONTINUE.
      ENDIF.
*      IF IT_ZSBLIT-ELIKZ EQ 'X'.
*        DELETE IT_ZSBLIT   INDEX   W_TABIX.
*        MESSAGE W359 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
*        CONTINUE.
*      ENDIF.
    ELSE.
      DELETE IT_ZSBLIT   INDEX   W_TABIX.
      MESSAGE W071 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-ZFITMNO.
      CONTINUE.
    ENDIF.

*----> B/L Material Information(formal Entry)
    SELECT SUM( BLMENGE ) INTO IT_ZSBLIT-ZFBLTOT
                FROM  ZTBLIT
                WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO
                AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO
                AND   BLOEKZ  NE 'X'.

*>>> B/L Basic Quantity
    IT_ZSBLIT-BLMENGE = IT_ZSBLIT-MENGE - IT_ZSBLIT-ZFBLTOT.
    IF IT_ZSBLIT-BLMENGE LE 0.
      IT_ZSBLIT-BLMENGE = 0.
    ENDIF.
    IT_ZSBLIT-EBELP = IT_ZSBLIT-EBELP.

    SELECT MAX( ZFSHNO ) INTO W_ZFSHNO
           FROM  ZTBLIT
           WHERE EBELN EQ IT_ZSBLIT-EBELN
           AND   EBELP EQ IT_ZSBLIT-EBELP.

    IF IT_ZSBLIT-BLMENGE IS INITIAL.
      CLEAR : IT_ZSBLIT-ZFSHNO.
    ELSE.
      IF W_ZFSHNO IS INITIAL.
        IT_ZSBLIT-ZFSHNO = '01'.
      ELSE.
        IT_ZSBLIT-ZFSHNO = W_ZFSHNO + 1.
      ENDIF.

    ENDIF.

    ADD   1   TO   W_COUNT.
    MOVE : SY-UNAME    TO   IT_ZSBLIT-ERNAM,
           SY-DATUM    TO   IT_ZSBLIT-CDAT,
           SY-UNAME    TO   IT_ZSBLIT-UNAM,
           SY-DATUM    TO   IT_ZSBLIT-UDAT.
    MODIFY  IT_ZSBLIT INDEX W_TABIX.

  ENDLOOP.
ENDFORM.                    " P1000_GET_BL_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_REF_BL_DOC_NO_INPUT
*&---------------------------------------------------------------------*
FORM P2000_REF_BL_DOC_NO_INPUT.

  SPOP-TITEL = 'Create Commercial Invoice : Copy from B/L'.
  OPTION = 1.

  CALL SCREEN 3512 STARTING AT 27 6
                   ENDING   AT 70 10.

  IF ANTWORT EQ 'C' OR ANTWORT EQ 'N'.       " Cancel Or No
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_REF_BL_DOC_NO_INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DATA_MOVE
*&---------------------------------------------------------------------*
FORM P2000_BL_DATA_MOVE.
  DATA : L_TEXT  LIKE  BKPF-AWKEY.
  SELECT SINGLE * FROM ZTIMIMG00.

  IF ZTBL-ZFPOYN EQ 'N'.
    MESSAGE E698 WITH ZTBL-ZFBLNO.
  ENDIF.

  ">> B/L Exchange Rate Fixed Setting Check.
  IF   ZTIMIMG00-ZFEXFIX EQ 'X' AND
     ( ZTIMIMG00-ZFEXMTD EQ 'B' OR ZTIMIMG00-ZFEXMTD EQ 'A' OR
       ZTIMIMG00-ZFEXMTD EQ 'R' ).
    IF ZTBL-ZFEXRT IS INITIAL.
      PERFORM P2000_NO_INPUT USING 'ZSREQHD' 'ZFBLNO'
                                   DFIES-SCRTEXT_M W_SUBRC.
      MESSAGE E167 WITH 'B/L Exchange Rate'.
    ENDIF.

    LOOP AT IT_ZSBLIT.
      W_TABIX = SY-TABIX.
      IF NOT IT_ZSBLIT-ZFPOTY IS INITIAL OR
         IT_ZSBLIT-BLOEKZ EQ 'X'.
        DELETE IT_ZSBLIT INDEX W_TABIX.
        CONTINUE.
      ENDIF.
      ON CHANGE OF IT_ZSBLIT-EBELN.
        SELECT SINGLE * FROM EKKO
               WHERE    EBELN  EQ  IT_ZSBLIT-EBELN.
        IF EKKO-WKURS NE ZTBL-ZFEXRT.
          MESSAGE E527 WITH IT_ZSBLIT-EBELN EKKO-WKURS
                            ZTBL-ZFBLNO     ZTBL-ZFEXRT.
        ENDIF.
        IF EKKO-KUFIX NE 'X'.
          MESSAGE E528 WITH IT_ZSBLIT-EBELN.
        ENDIF.
      ENDON.
    ENDLOOP.
  ENDIF.

  IF ZTIMIMG00-ZFEXFIX EQ 'X' AND ZTIMIMG00-ZFEXMTD EQ 'G'.
    LOOP AT IT_ZSBLIT.

      W_TABIX = SY-TABIX.
      IF NOT IT_ZSBLIT-ZFPOTY IS INITIAL OR IT_ZSBLIT-BLOEKZ EQ 'X'.
        DELETE IT_ZSBLIT INDEX W_TABIX.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * FROM EKBE
             WHERE  EBELN  EQ   IT_ZSBLIT-EBELN
             AND    EBELP  EQ   IT_ZSBLIT-EBELP
             AND    BELNR  EQ   ( SELECT MAX( BELNR )
                                  FROM   EKBE
                                  WHERE  EBELN  EQ IT_ZSBLIT-EBELN
                                  AND    EBELP  EQ IT_ZSBLIT-EBELP
                                  AND    VGABE  EQ '1'
                                  AND    BEWTP  EQ 'E' ).
      IF SY-SUBRC NE 0.
        MESSAGE  E110(ZIM1) WITH IT_ZSBLIT-EBELN
                                 IT_ZSBLIT-EBELP.
      ELSEIF EKBE-SHKZG EQ 'H'.
        MESSAGE  E110(ZIM1) WITH IT_ZSBLIT-EBELN
                                 IT_ZSBLIT-EBELP.
      ENDIF.

      " G/R Posting Date Exchange Rate Set.
      CALL FUNCTION 'ZIM_GET_EXCHANGE_RATE'
           EXPORTING
                P_WAERS    = EKBE-WAERS
                P_DATE     = EKBE-BUDAT
                P_KURST    = 'M'
                P_TO_WAERS = 'KRW'
           IMPORTING
                P_EXRT     = ZTBL-ZFEXRT
                P_FFACT    = ZTBL-FFACT
           EXCEPTIONS
                NO_INPUT   = 4
                NOT_FOUND  = 6.

      CASE SY-SUBRC.
        WHEN 4.
          MESSAGE E094.
        WHEN 6.
          MESSAGE E094.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.
  ENDIF.

  SELECT SINGLE * FROM T001W
         WHERE    WERKS  EQ  ZTBL-ZFWERKS.
  IF SY-SUBRC EQ 0.
    MOVE : T001W-J_1BBRANCH TO ZTCIVHD-BUPLA.
  ENDIF.

*>> HEADER DATA MOVE.
  MOVE : ZTBL-ZFBLAMC       TO     ZTCIVHD-ZFIVAMC,
         ZTBL-ZFBLAMC       TO     ZTCIVHD-ZFPKCUR,
         ZTBL-ZFBLAMC       TO     ZTCIVHD-ZFHDCUR,
         ZTBL-BUKRS         TO     ZTCIVHD-BUKRS,
         ZTBL-ZFBENI        TO     ZTCIVHD-ZFMAVN,
         ZTBL-ZFEXRT        TO     ZTCIVHD-ZFEXRT,
         ZTBL-FFACT         TO     ZTCIVHD-FFACT,
         ZTBL-ZFBLDT        TO     ZTCIVHD-ZFCIDT,
         SY-DATUM           TO     ZTCIVHD-BUDAT,
         ZTBL-ZFBLDT        TO     ZTCIVHD-ZFGSDT,
         'N'                TO     ZTCIVHD-ZFIVST,
         SY-UNAME           TO     ZTCIVHD-ERNAM,
         SY-DATUM           TO     ZTCIVHD-CDAT,
         SY-UNAME           TO     ZTCIVHD-UNAM,
         SY-DATUM           TO     ZTCIVHD-UDAT.

*>> Beneficialy Default Bank Set<2004.02.12-NHJ>.
  REFRESH : IT_OPBN_HELP.
  SELECT SINGLE * FROM LFA1
  WHERE  LIFNR    EQ   ZTCIVHD-ZFMAVN.
  IF LFA1-LNRZA   IS   INITIAL.
    SELECT * FROM LFZA WHERE LIFNR EQ ZTCIVHD-ZFMAVN.
      CLEAR : LFA1.
      SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFZA-EMPFK.
      MOVE : LFZA-EMPFK      TO   IT_OPBN_HELP-LIFNR,
             LFA1-NAME1      TO   IT_OPBN_HELP-NAME1,
             LFA1-ORT01      TO   IT_OPBN_HELP-ORT01.
      APPEND IT_OPBN_HELP.
    ENDSELECT.
  ELSE.
    SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ  LFA1-LNRZA.
    MOVE : LFA1-LIFNR      TO   IT_OPBN_HELP-LIFNR,
           LFA1-NAME1      TO   IT_OPBN_HELP-NAME1,
           LFA1-ORT01      TO   IT_OPBN_HELP-ORT01.
    APPEND IT_OPBN_HELP.
  ENDIF.
  DESCRIBE TABLE IT_OPBN_HELP LINES W_LINE.
  IF W_LINE EQ 1.
    READ TABLE IT_OPBN_HELP INDEX 1.
    MOVE  IT_OPBN_HELP-LIFNR  TO  ZTCIVHD-ZFOPBN.
  ENDIF.

*>> LOCAL CURRENCY SETTING. <2003.08.27-NHJ>
  CLEAR : T001.
  SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTBL-BUKRS.
  MOVE  T001-WAERS  TO  ZTCIVHD-ZFKRW.

  DESCRIBE  TABLE  IT_ZSCIVIT    LINES   W_LINE.
  REFRESH : IT_ZSCIVIT, IT_GSBER.

*>> Deleted Item
  DELETE IT_ZSBLIT   WHERE  BLOEKZ  EQ 'X'.

  LOOP AT IT_ZSBLIT.
    W_TABIX = SY-TABIX.

    IF NOT IT_ZSBLIT-ZFPOTY IS INITIAL OR IT_ZSBLIT-BLOEKZ EQ 'X'.
      DELETE IT_ZSBLIT INDEX W_TABIX.
      CONTINUE.
    ENDIF.

*++++> P/O DATA Get
    SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO EREKZ REPOS
           INTO (IT_ZSBLIT-MENGE_PO, IT_ZSBLIT-UEBTO,
                 IT_ZSBLIT-UEBTK,    IT_ZSBLIT-WEPOS,
                 IT_ZSBLIT-ELIKZ,    IT_ZSBLIT-LOEKZ,
                 IT_ZSBLIT-UNTTO,    W_EREKZ,
                 W_REPOS)
           FROM   EKPO
           WHERE  EBELN   EQ   IT_ZSBLIT-EBELN
           AND    EBELP   EQ   IT_ZSBLIT-EBELP.

    IF IT_ZSCIVIT-LOEKZ NE SPACE.
      CLEAR : ZSCIVIT.
      MESSAGE W069 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
      CONTINUE.
    ENDIF.
    IF W_EREKZ EQ 'X'.
      CLEAR : ZSCIVIT.   CONTINUE.
    ENDIF.
    IF W_REPOS IS INITIAL.
      CLEAR : ZSCIVIT.   CONTINUE.
    ENDIF.

*>> Open Bank
    IF ZTCIVHD-ZFOPBN IS INITIAL OR ZTCIVHD-ZTERM IS INITIAL.
      SELECT SINGLE * FROM ZTREQHD
                      WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO.

      IF ZTCIVHD-ZFOPBN IS INITIAL.
        ZTCIVHD-ZFOPBN  =   ZTREQHD-ZFOPBN.
      ENDIF.

      IF ZTCIVHD-ZTERM IS INITIAL.
        ZTCIVHD-ZTERM   =   ZTREQHD-ZTERM.
      ENDIF.
    ENDIF.

*+++++> Import Request Quantity
    SELECT SINGLE MENGE INTO IT_ZSBLIT-MENGE
           FROM  ZTREQIT
           WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO
           AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO.

*>>>>> B/L TOTAL Quantity.
    SELECT SUM( BLMENGE ) INTO IT_ZSBLIT-ZFBLTOT
           FROM  ZTBLIT
           WHERE ZFREQNO  EQ IT_ZSBLIT-ZFREQNO
           AND   ZFITMNO  EQ IT_ZSBLIT-ZFITMNO
           AND   BLOEKZ   NE 'X'.

    CLEAR : IT_ZSCIVIT.
    MOVE-CORRESPONDING   IT_ZSBLIT   TO   IT_ZSCIVIT.

    MOVE : IT_ZSBLIT-ZFBLIT          TO   IT_ZSCIVIT-ZFBLIT,
           IT_ZSBLIT-ZFBLNO          TO   IT_ZSCIVIT-ZFBLNO,
           IT_ZSBLIT-EBELN           TO   IT_ZSCIVIT-EBELN,
           IT_ZSBLIT-EBELP           TO   IT_ZSCIVIT-EBELP,
           IT_ZSBLIT-ZFREQNO         TO   IT_ZSCIVIT-ZFREQNO,
           IT_ZSBLIT-ZFITMNO         TO   IT_ZSCIVIT-ZFITMNO,
           IT_ZSBLIT-BLMENGE         TO   IT_ZSCIVIT-MENGE_BL,
           ZTBL-ZFBLAMC              TO   IT_ZSCIVIT-ZFIVAMC,
           T001-WAERS                TO   IT_ZSCIVIT-ZFKRW.

*>> B/L Information -> Prepaid Qty
    SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
           FROM ZTCIVIT
           WHERE ZFBLNO   EQ IT_ZSCIVIT-ZFBLNO
           AND   ZFBLIT   EQ IT_ZSCIVIT-ZFBLIT.

*>> B/L Information -> Preapaid Qty
    SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
           FROM ZTCIVIT
           WHERE ZFBLNO   EQ IT_ZSCIVIT-ZFBLNO
           AND   ZFBLIT   EQ IT_ZSCIVIT-ZFBLIT.

    ADD   1   TO    W_LINE.
    IT_ZSCIVIT-ZFPRQN = IT_ZSCIVIT-MENGE_BL - IT_ZSCIVIT-CIVTOT.
    IT_ZSCIVIT-CMENGE = IT_ZSCIVIT-MENGE_BL - IT_ZSCIVIT-CIVTOT1.
    IF IT_ZSCIVIT-ZFPRQN LT 0.
      IT_ZSCIVIT-ZFPRQN = 0.
    ENDIF.
    IF IT_ZSCIVIT-CMENGE  LT 0.
      IT_ZSCIVIT-CMENGE = 0.
    ENDIF.

    IT_ZSCIVIT-ZFCIVSQ    =     W_LINE  *   10.
    APPEND   IT_ZSCIVIT.
    ">> Business Area Get
    IF IT_ZSCIVIT-ZFPRQN NE 0.
      PERFORM  P1000_GET_ITEM_GSBER TABLES IT_GSBER
                                    USING  IT_ZSBLIT-MATNR
                                           IT_ZSBLIT-WERKS.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_GSBER LINES W_LINE.
  IF W_LINE EQ 1.
    READ TABLE IT_GSBER INDEX 1.
    MOVE IT_GSBER-GSBER TO ZTCIVHD-GSBER.
  ELSE.
    CLEAR : ZTCIVHD-GSBER.
  ENDIF.

*> Invoice Qty = 0 -> Deleted
  DELETE IT_ZSCIVIT WHERE ZFPRQN EQ 0.

  DESCRIBE TABLE  IT_ZSCIVIT  LINES W_LINE.
  IF W_LINE EQ 0.
    MESSAGE E384 WITH ZTBL-ZFBLNO.
  ENDIF.

  PERFORM P2000_SET_CIVIT_LOCK USING 'L'.

ENDFORM.                    " P2000_BL_DATA_MOVE
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTCIV_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_ZTCIV_MODIFY.

  IF ZTCIVHD-ZFSVYN NE 'X'.
    LOOP AT IT_ZSCIVIT WHERE ZFPRQN GT 0.

    ENDLOOP.

    IF SY-SUBRC NE 0.
      MESSAGE E108(ZIM1).
    ENDIF.
  ENDIF.

* »ý¼ºÀÏ °æ¿ì ´ÙÀ½ ¹øÈ£¸¦ Ã¤¹ø..
  IF W_STATUS EQ C_REQ_C.
    PERFORM   P2000_GET_NUMBER_NEXT  USING  'CI'  ZTCIVHD-ZFCIVRN.
    ZTCIVHD-ZFEDICK = 'O'.
    ZTCIVHD-ZFEDIST = 'N'.
    ZTCIVHD-ZFDOCST = 'N'.
  ENDIF.

*> ¼ö·®ÀÌ 0ÀÎ ¾ÆÀÌÅÛ »èÁ¦..
  IF ZTCIVHD-ZFSVYN NE 'X'.
    DELETE IT_ZSCIVIT WHERE ZFPRQN EQ 0.
  ENDIF.

  CALL FUNCTION 'ZIM_COMMERCIAL_INVOICE_MODIFY'
       EXPORTING
            W_OK_CODE      = W_OK_CODE
            ZFCIVRN        = ZTCIVHD-ZFCIVRN
            ZFSTATUS       = W_STATUS
            W_ZTCIVHD      = ZTCIVHD
            W_ZTCIVHD_OLD  = *ZTCIVHD
       TABLES
            IT_ZSCIVIT     = IT_ZSCIVIT
            IT_ZSCIVIT_OLD = IT_ZSCIVIT_ORG
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E952.
  ELSE.
    SET PARAMETER ID 'ZPCIVRN'  FIELD ZTCIVHD-ZFCIVRN.
    MESSAGE  S367  WITH  ZTCIVHD-ZFCIVRN.
  ENDIF.

*>>PAYORD¿¡ µ¥ÀÌÅÍ ¹Ý¿µ FROM 020604 ÃÖÃÊ¹ß»ý->º¯°æÀº 141È­¸é¿¡¼­.
  PERFORM   P2000_PAYORD_FIELD_MOVE USING W_STATUS.
**>> EDI Ã¼?
*  PERFORM   P2000_PAYORD_DOC_CHECK.
*  UPDATE ZTCIVHD.

ENDFORM.                    " P3000_ZTCIV_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_CIV_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_CIV_LOCK USING     PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTCIVHD'
         EXPORTING
              ZFCIVRN = ZTCIVHD-ZFCIVRN
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Commercial Invoice Document'
                            ZTCIVHD-ZFCIVRN ''
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCIVHD'
         EXPORTING
              ZFCIVRN = ZTCIVHD-ZFCIVRN.
  ENDIF.

ENDFORM.                    " P2000_SET_CIV_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSCIVIT_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IT_ZSCIVIT_UPDATE  USING  PA_MODE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  DESCRIBE TABLE IT_ZSCIVIT   LINES W_LINE.
  IF W_LINE < 1.
    IF PA_MODE EQ 'E'.
      MESSAGE ID 'ZIM' TYPE PA_MODE NUMBER '901'.
    ENDIF.
  ENDIF.

  W_TOT_AMOUNT  = 0.    W_COUNT      = 0.    W_TOT_ITEM   = 0.
  W_TOT_AMOUNT1 = 0.
  CLEAR : ZSCIVIT.

  LOOP AT IT_ZSCIVIT.
    W_TABIX = SY-TABIX.
    W_COUNT = W_COUNT + 1.

* INSTALLING CHG.
    IF IT_ZSCIVIT-KPEIN EQ 0.
      IT_ZSCIVIT-KWERT = 0.
    ELSE.
      IT_ZSCIVIT-KWERT = ( IT_ZSCIVIT-KBETR / IT_ZSCIVIT-KPEIN ) *
                           IT_ZSCIVIT-MENGE.
    ENDIF.

    ADD  IT_ZSCIVIT-ZFIVAMP TO  W_TOT_AMOUNT.
    ADD  IT_ZSCIVIT-ZFIVAMK TO  W_TOT_AMOUNT1.

    W_TOT_ITEM   = W_TOT_ITEM   +   IT_ZSREQIT-MENGE.       " ITEM ¼ö?

  ENDLOOP.

** Packing Chg. ¹× Handing CHG. ´õÇÏ±â.
  W_TOT_AMOUNT = W_TOT_AMOUNT + ZTCIVHD-ZFPKCHGP + ZTCIVHD-ZFHDCHGP.

  LOOP AT IT_ZSCIVIT.
    IT_ZSCIVIT-ZFCIVSQ  =  SY-TABIX * 10.
    MODIFY IT_ZSCIVIT.
  ENDLOOP.

  IF OK-CODE EQ 'CALC'.
    ZTCIVHD-ZFIVAMT = W_TOT_AMOUNT.
  ENDIF.

*>>> ¼±±Þ±Ý¾×.
  DESCRIBE  TABLE IT_ZSCIVIT LINES W_LINE.
  CLEAR : ZTCIVHD-ZFPIVAM.
  CHECK W_LINE GT 0.
  SELECT * FROM ZTCIVIT
           FOR ALL ENTRIES IN IT_ZSCIVIT
           WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
           AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
           AND   ZFPRPYN EQ 'Y'               ">> ¼±±Þ±ÝÀÎ °æ¿ì..
           AND   ZFCIVRN NE ZTCIVHD-ZFCIVRN.
    ADD ZTCIVIT-ZFIVAMP    TO   ZTCIVHD-ZFPIVAM.
  ENDSELECT.
ENDFORM.                    " P2000_IT_ZSCIVIT_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_SELECT_CIV_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_LINE_SELECT_CIV_ITEM.
  CLEAR : ZSCIVIT.
  W_COUNT = 0.
  LOOP AT IT_ZSCIVIT WHERE ZFMARK = 'X'.
    W_COUNT = W_COUNT + 1.
    MOVE-CORRESPONDING  IT_ZSCIVIT  TO ZSCIVIT.
  ENDLOOP.

ENDFORM.                    " P2000_LINE_SELECT_CIV_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_CALL_TCODE_CIV_ITEM
*&---------------------------------------------------------------------*
FORM P2000_LINE_CALL_TCODE_CIV_ITEM.
  DATA : L_AWKEY   LIKE   BKPF-AWKEY.

  CASE W_COUNT.
    WHEN 0.        MESSAGE S962.   EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965.   EXIT.
  ENDCASE.

  CASE  OK-CODE.
    WHEN 'PODP'.          " P/O DISPLAY
      PERFORM  P2000_PO_DOC_DISPLAY USING ZSCIVIT-EBELN
                                          ZSCIVIT-EBELP.
    WHEN 'IMDP'.          " IMPORT L/C DISPLAY
      PERFORM  P2000_LC_DOC_DISPLAY USING ZSCIVIT-ZFREQNO ''.
    WHEN 'BLDP'.          " B/L DISPLAY
      PERFORM  P2000_BL_DOC_DISPLAY USING ZSCIVIT-ZFBLNO.
    WHEN 'CIDP'.          " COMMERCIAL DISPLAY
      PERFORM  P2000_CIV_DOC_DISPLAY USING ZSCIVIT-ZFCIVRN.
    WHEN 'DIMI'.          "> ÀÚÀç¹®¼­.
      SET  PARAMETER ID  'RBN'   FIELD   ZSCIVHST-BELNR.
      SET  PARAMETER ID  'GJR'   FIELD   ZSCIVHST-GJAHR.
*         SET  PARAMETER ID  'BUK'   FIELD   l_call_trans-bukrs.

      CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
    WHEN 'DIM2'.          "> Ãë¼Ò¹®¼­.
      IF ZSCIVHST-CBELNR IS INITIAL.
        MESSAGE S167 WITH 'Cancel material Doc'.
        EXIT.
      ENDIF.
      SET  PARAMETER ID  'RBN'   FIELD   ZSCIVHST-CBELNR.
      SET  PARAMETER ID  'GJR'   FIELD   ZSCIVHST-CGJAHR.
      CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.

    WHEN 'DIFB'.          "> ÈÄ¼Ó¹®¼­.
      CLEAR : L_AWKEY.
      MOVE : ZSCIVHST-BELNR  TO   L_AWKEY(10),
*                ZSCIVHST-BUKRS  TO   L_AWKEY+10(4),
             ZSCIVHST-GJAHR  TO   L_AWKEY+10(4).

      CLEAR : BKPF.
      SELECT * FROM BKPF UP TO 1 ROWS
               WHERE AWKEY  EQ  L_AWKEY.
      ENDSELECT.
      IF SY-SUBRC EQ 0.
        SET  PARAMETER ID  'BUK'   FIELD   BKPF-BUKRS.
        SET  PARAMETER ID  'BLN'   FIELD   BKPF-BELNR.
        SET  PARAMETER ID  'GJR'   FIELD   BKPF-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
      ENDIF.
    WHEN 'CGDP'.          "> ÀÚÀç¹®¼­.

*> ÀâÀÌÀÍ ¹®¼­ÀÏ °æ¿ì.
      IF ZSIVHST-ZFGAIN EQ 'X'.
        PERFORM FIDOC_SHOW(ZRIMNDFTPT) USING ZTIV-BUKRS
                                             ZSIVHST-MJAHR
                                             ZSIVHST-MBLNR.
        EXIT.
      ENDIF.
      SET  PARAMETER ID  'MBN'   FIELD   ZSIVHST-MBLNR.
      SET  PARAMETER ID  'MJA'   FIELD   ZSIVHST-MJAHR.
      SET  PARAMETER ID  'BUK'   FIELD   ZTIV-BUKRS.
*         CALL TRANSACTION 'MB03' AND SKIP  FIRST SCREEN.
      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
           I_ACTION                  = 'A04'
           I_REFDOC                  = 'R02'
           I_NOTREE                  = 'X'
*             I_NO_AUTH_CHECK           =
           I_SKIP_FIRST_SCREEN       = 'X'
*             I_DEADEND                 = 'X'
           I_OKCODE                  = 'OK_GO'
*             I_LEAVE_AFTER_POST        =
*             i_new_rollarea            = 'X'
*             I_SYTCODE                 =
*             I_EBELN                   =
*             I_EBELP                   =
           I_MBLNR                   = ZSIVHST-MBLNR
           I_MJAHR                   = ZSIVHST-MJAHR
*             I_ZEILE                   =
        EXCEPTIONS
           ILLEGAL_COMBINATION       = 1
           OTHERS                    = 2.


    WHEN 'CGDP1'.          "> ÀÚÀç¹®¼­.
      SET  PARAMETER ID  'BUK'   FIELD   ZSIVHST1-BUKRS.
      SET  PARAMETER ID  'RBN'   FIELD   ZSIVHST1-BELNR.
      SET  PARAMETER ID  'GJR'   FIELD   ZSIVHST1-GJAHR.
      CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
    WHEN 'CGDP2'.          "> ÀÚÀç¹®¼­Ãë¼Ò.
      IF ZSIVHST-CMBLNR IS INITIAL.
        MESSAGE S167 WITH 'Cancel material Doc'.
        EXIT.
      ENDIF.

*> ÀâÀÌÀÍ ¹®¼­ÀÏ °æ¿ì.
      IF ZSIVHST-ZFGAIN EQ 'X'.
        PERFORM FIDOC_SHOW(ZRIMNDFTPT) USING ZTIV-BUKRS
                                             ZSIVHST-CMJAHR
                                             ZSIVHST-CMBLNR.
        EXIT.
      ENDIF.

      SET  PARAMETER ID  'MBN'   FIELD   ZSIVHST-CMBLNR.
      SET  PARAMETER ID  'MJA'   FIELD   ZSIVHST-CMJAHR.
      SET  PARAMETER ID  'BUK'   FIELD   ZTIV-BUKRS.
      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
           I_ACTION                  = 'A04'
           I_REFDOC                  = 'R02'
           I_NOTREE                  = 'X'
*             I_NO_AUTH_CHECK           =
           I_SKIP_FIRST_SCREEN       = 'X'
*             I_DEADEND                 = 'X'
           I_OKCODE                  = 'OK_GO'
*             I_LEAVE_AFTER_POST        =
*             i_new_rollarea            = 'X'
*             I_SYTCODE                 =
*             I_EBELN                   =
*             I_EBELP                   =
           I_MBLNR                   = ZSIVHST-CMBLNR
           I_MJAHR                   = ZSIVHST-CMJAHR
*             I_ZEILE                   =
        EXCEPTIONS
           ILLEGAL_COMBINATION       = 1
           OTHERS                    = 2.

*         CALL TRANSACTION 'MB03' AND SKIP  FIRST SCREEN.
  ENDCASE.

ENDFORM.                    " P2000_LINE_CALL_TCODE_CIV_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSCIVIT_RECALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IT_ZSCIVIT_RECALC.
  LOOP AT IT_ZSCIVIT.
    W_TABIX = SY-TABIX.

*>> INVOICE ÀÚÀçº° ±Ý¾×.
    IF ZTCIVHD-ZFSVYN NE 'X'.
      IT_ZSCIVIT-ZFIVAMT = ( IT_ZSCIVIT-CMENGE *
                           ( IT_ZSCIVIT-BPUMZ / IT_ZSCIVIT-BPUMN ) *
                           ( IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH ) ).

      IT_ZSCIVIT-ZFIVAMP = ( IT_ZSCIVIT-ZFPRQN *
                           ( IT_ZSCIVIT-BPUMZ / IT_ZSCIVIT-BPUMN ) *
                           ( IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH ) ).
    ENDIF.

*>> Ã³¸® ¿øÈ­ ±Ý¾× °è»ê...
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSCIVIT-ZFIVAMP
                                            IT_ZSCIVIT-ZFIVAMC
                                            IT_ZSCIVIT-ZFIVAMK.

    IF ZTCIVHD-FFACT IS INITIAL.
       *BAPICURR-BAPICURR = ZTCIVHD-ZFEXRT * IT_ZSCIVIT-ZFIVAMK.
    ELSE.
       *BAPICURR-BAPICURR = ( ZTCIVHD-ZFEXRT / ZTCIVHD-FFACT )
                                           * IT_ZSCIVIT-ZFIVAMK.
    ENDIF.

    PERFORM SET_CURR_CONV_TO_INTERNAL USING
            *BAPICURR-BAPICURR ZTCIVHD-ZFKRW.

    IF *BAPICURR-BAPICURR GT 9999999999999.
      MESSAGE W923 WITH *BAPICURR-BAPICURR.
      IT_ZSCIVIT-ZFIVAMK = 0.
    ELSE.
      IT_ZSCIVIT-ZFIVAMK = *BAPICURR-BAPICURR.
    ENDIF.
    MODIFY  IT_ZSCIVIT  INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P2000_IT_ZSCIVIT_RECALC
*&---------------------------------------------------------------------*
*&      Form  P2000_REF_REQ_DOC_NO_INPUT
*&---------------------------------------------------------------------*
FORM P2000_REF_REQ_DOC_NO_INPUT.

  SPOP-TITEL = 'Create Commercial Invoice : Copy from Import request'.
  OPTION = 1.

  CALL SCREEN 3514 STARTING AT 15 6
                   ENDING   AT 70 10.

  IF ANTWORT EQ 'C' OR ANTWORT EQ 'N'.       " Cancel Or No
    SET SCREEN SY-DYNNR.
  ELSE.
    IF ZSCIVHD-ZFTRIPLE NE ZTREQHD-ZFTRIPLE.
      MESSAGE E524 WITH ZTREQHD-ZFREQNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_REF_REQ_DOC_NO_INPUT

*&---------------------------------------------------------------------*
*&      Form  P2000_REQ_DATA_MOVE
*&---------------------------------------------------------------------*
FORM P2000_REQ_DATA_MOVE.

  IF ZSCIVHD-ZFPRPYN EQ 'Y'.
    IF ZTREQHD-ZFREQTY EQ 'PU' OR ZTREQHD-ZFREQTY EQ 'LO'.
      MESSAGE E505 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
    ENDIF.
  ELSE.
    IF NOT ( ZTREQHD-ZFREQTY EQ 'PU' OR ZTREQHD-ZFREQTY EQ 'LO' ).
      MESSAGE E505 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
    ENDIF.
  ENDIF.

  MOVE : ZTREQHD-WAERS       TO     ZTCIVHD-ZFIVAMC,
         ZTREQHD-WAERS       TO     ZTCIVHD-ZFPKCUR,
         ZTREQHD-WAERS       TO     ZTCIVHD-ZFHDCUR,
         ZTREQHD-ZFPREPAY    TO     ZTCIVHD-ZFPRTE,
         ZTREQHD-ZFREQTY     TO     ZTCIVHD-ZFREQTY,
         ZTREQHD-ZTERM       TO     ZTCIVHD-ZTERM,
         ZTREQHD-KURSF       TO     ZTCIVHD-ZFEXRT,
         ZTREQHD-FFACT       TO     ZTCIVHD-FFACT,
         'KRW'               TO     ZTCIVHD-ZFKRW,
         ZTREQHD-ZFBENI      TO     ZTCIVHD-ZFMAVN,  " ¼öÀÍÀÚ.
         ZTREQHD-ZFOPBN      TO     ZTCIVHD-ZFOPBN,  " ¼öÀÍÀÚ.
         ZTREQHD-BUKRS       TO     ZTCIVHD-BUKRS,
         'N'                 TO     ZTCIVHD-ZFIVST,
         ZTREQST-ZFOPNDT     TO     ZTCIVHD-ZFCIDT,
         ZTREQHD-ZFPKCHG     TO     ZTCIVHD-ZFPKCHGP,
         ZTREQHD-ZFHDCHG     TO     ZTCIVHD-ZFHDCHGP,
         SY-DATUM            TO     ZTCIVHD-BUDAT,
         SY-DATUM            TO     ZTCIVHD-ZFGSDT,
         SY-UNAME            TO     ZTCIVHD-ERNAM,
         SY-DATUM            TO     ZTCIVHD-CDAT,
         SY-UNAME            TO     ZTCIVHD-UNAM,
         SY-DATUM            TO     ZTCIVHD-UDAT.

*>> Beneficialy Default Bank Set<2004.02.12-NHJ>.
  REFRESH : IT_OPBN_HELP.
  SELECT SINGLE * FROM LFA1
  WHERE  LIFNR    EQ   ZTCIVHD-ZFMAVN.
  IF LFA1-LNRZA   IS   INITIAL.
    SELECT * FROM LFZA WHERE LIFNR EQ ZTCIVHD-ZFMAVN.
      CLEAR : LFA1.
      SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFZA-EMPFK.
      MOVE : LFZA-EMPFK      TO   IT_OPBN_HELP-LIFNR,
             LFA1-NAME1      TO   IT_OPBN_HELP-NAME1,
             LFA1-ORT01      TO   IT_OPBN_HELP-ORT01.
      APPEND IT_OPBN_HELP.
    ENDSELECT.
  ELSE.
    SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ  LFA1-LNRZA.
    MOVE : LFA1-LIFNR      TO   IT_OPBN_HELP-LIFNR,
           LFA1-NAME1      TO   IT_OPBN_HELP-NAME1,
           LFA1-ORT01      TO   IT_OPBN_HELP-ORT01.
    APPEND IT_OPBN_HELP.
  ENDIF.
  DESCRIBE TABLE IT_OPBN_HELP LINES W_LINE.
  IF W_LINE EQ 1.
    READ TABLE IT_OPBN_HELP INDEX 1.
    MOVE  IT_OPBN_HELP-LIFNR  TO  ZTCIVHD-ZFOPBN.
  ENDIF.

  IF NOT ZTREQHD-ZFWERKS IS INITIAL.
    SELECT SINGLE * FROM  T001W
                    WHERE WERKS EQ ZTREQHD-ZFWERKS.
    MOVE T001W-J_1BBRANCH TO ZTCIVHD-BUPLA.
  ENDIF.

  DESCRIBE  TABLE  IT_ZSCIVIT    LINES   W_LINE.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE  IT_ZSREQIT
           FROM ZTREQIT
           WHERE ZFREQNO  EQ  ZTREQHD-ZFREQNO.

  REFRESH : IT_GSBER.

  LOOP AT IT_ZSREQIT.
    CLEAR : IT_ZSCIVIT.
    MOVE-CORRESPONDING   IT_ZSREQIT   TO   IT_ZSCIVIT.
*++++> P/O DATA Á¶È¸.
    CLEAR : EKPO.
    SELECT SINGLE * FROM   EKPO
           WHERE    EBELN  EQ   IT_ZSREQIT-EBELN
           AND      EBELP  EQ   IT_ZSREQIT-EBELP.

    " Ç°¸ñ¹üÁÖ°¡ SERVICE ÀÏ °æ¿ì´Â SERVICE ENTRY ±Ý¾× GET.
    IF EKPO-PSTYP  EQ  '9'.
      PERFORM   P3000_SERVICE_ITEM_GET.
      IT_ZSCIVIT-ZFCIVSQ    =     W_LINE  *   10.
      APPEND   IT_ZSCIVIT.
      CONTINUE.
    ENDIF.

    " PO ITEM ÀÌ »èÁ¦ÇÑ °æ¿ì CONTINUE.
    IF EKPO-LOEKZ  NE  SPACE.
      CLEAR : ZSCIVIT.
      MESSAGE  W069  WITH EKPO-EBELN  EKPO-EBELP.
      CONTINUE.
    ENDIF.

    " ¼ÛÀåÃ³¸®°¡ Á¾°áµÈ °æ¿ì CONTINUE.
    IF EKPO-EREKZ  EQ  'X'.
      CLEAR : ZSCIVIT.
      MESSAGE  W220(ZIM1)  WITH  EKPO-EBELN  EKPO-EBELP.
      CONTINUE.
    ENDIF.

    " ¼ÛÀåÃ³¸® ÇÏÁö ¾ÊÀ» °æ¿ì´Â CONTINUE.
    IF EKPO-REPOS  NE  'X'.
      CLEAR : ZSCIVIT.
      MESSAGE  W221(ZIM1)  WITH  EKPO-EBELN  EKPO-EBELP.
      CONTINUE.
    ENDIF.

    MOVE  : EKPO-MENGE         TO    IT_ZSCIVIT-MENGE_PO,
            EKPO-UEBTO         TO    IT_ZSCIVIT-UEBTO,
            EKPO-UEBTK         TO    IT_ZSCIVIT-UEBTK,
            EKPO-WEPOS         TO    IT_ZSCIVIT-WEPOS,
            EKPO-ELIKZ         TO    IT_ZSCIVIT-ELIKZ,
            EKPO-LOEKZ         TO    IT_ZSCIVIT-LOEKZ,
            EKPO-UNTTO         TO    IT_ZSCIVIT-UNTTO,
            EKPO-BPUMN         TO    IT_ZSCIVIT-BPUMN,
            EKPO-BPUMZ         TO    IT_ZSCIVIT-BPUMZ,
            EKPO-WERKS         TO    IT_ZSCIVIT-WERKS,
            EKPO-LGORT         TO    IT_ZSCIVIT-LGORT,
            ZTREQHD-WAERS      TO    IT_ZSCIVIT-ZFIVAMC,
            EKPO-EBELN         TO    IT_ZSCIVIT-EBELN,
            EKPO-EBELP         TO    IT_ZSCIVIT-EBELP,
            'KRW'              TO    IT_ZSCIVIT-ZFKRW.

*>> ¼öÀÔÀÇ·Ú Á¤º¸.--> ±âÃ³¸® ¼ö·®.
    SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
           FROM ZTCIVIT
           WHERE ZFREQNO  EQ IT_ZSCIVIT-ZFREQNO
           AND   ZFITMNO  EQ IT_ZSCIVIT-ZFITMNO.

*>> B/L Á¤º¸.--> ±âÃ³¸® ¼ö·®.
    SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
           FROM ZTCIVIT
           WHERE ZFREQNO  EQ IT_ZSCIVIT-ZFREQNO
           AND   ZFITMNO  EQ IT_ZSCIVIT-ZFITMNO
           AND   ZFPRPYN  NE 'Y'.            " ¼±±Þ±ÝÀÌ ¾Æ´Ñ °Í.

    ADD   1   TO    W_LINE.
    IT_ZSCIVIT-ZFPRQN = IT_ZSCIVIT-MENGE - IT_ZSCIVIT-CIVTOT.
    IT_ZSCIVIT-CMENGE = IT_ZSCIVIT-MENGE - IT_ZSCIVIT-CIVTOT1.

*>> ÀÜ·®ÀÌ ¾ø´Â °æ¿ì ITEM »èÁ¦ Ã³¸®<2002.12.03 NHJ Ãß°¡>.
    IF ( IT_ZSCIVIT-ZFPRQN IS INITIAL OR IT_ZSCIVIT-ZFPRQN EQ 0 )  AND
       ( IT_ZSCIVIT-CMENGE IS INITIAL OR IT_ZSCIVIT-CMENGE EQ 0 ).
      CLEAR ZSCIVIT.
      CONTINUE.
    ENDIF.

*>>> ¼±±ÞºñÀ² °è»êÇÏ¿© ¼ö·® Á¶Á¤...
    IF NOT ZTCIVHD-ZFPRTE IS INITIAL.
      W_MENGE = ( IT_ZSCIVIT-MENGE * ZTCIVHD-ZFPRTE ) / 100.
      IF W_MENGE LE IT_ZSCIVIT-ZFPRQN.
        IT_ZSCIVIT-ZFPRQN = W_MENGE.
      ENDIF.
    ENDIF.
*>> INVOICE ÀÚÀçº° ±Ý¾×.
    IT_ZSCIVIT-ZFIVAMT = ( IT_ZSCIVIT-CMENGE *
                         ( IT_ZSCIVIT-BPUMZ / IT_ZSCIVIT-BPUMN ) *
                         ( IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH ) ).

    IT_ZSCIVIT-ZFIVAMP = ( IT_ZSCIVIT-ZFPRQN *
                         ( IT_ZSCIVIT-BPUMZ / IT_ZSCIVIT-BPUMN ) *
                         ( IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH ) ).

*>> Ã³¸® ¿øÈ­ ±Ý¾× °è»ê...
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSCIVIT-ZFIVAMP
                                            IT_ZSCIVIT-ZFIVAMC
                                            IT_ZSCIVIT-ZFIVAMK.
    IF ZTCIVHD-FFACT IS INITIAL.
      ZTCIVHD-FFACT = 1.
    ENDIF.

    IT_ZSCIVIT-ZFIVAMK = ( ZTCIVHD-ZFEXRT / ZTCIVHD-FFACT )
                                        * IT_ZSCIVIT-ZFIVAMK.

    PERFORM SET_CURR_CONV_TO_INTERNAL USING
           IT_ZSCIVIT-ZFIVAMK 'KRW'.

    IF NOT IT_ZSCIVIT-WERKS IS INITIAL AND
           ZTCIVHD-BUPLA    IS INITIAL.
      SELECT SINGLE * FROM  T001W
                      WHERE WERKS EQ IT_ZSCIVIT-WERKS.
      MOVE T001W-J_1BBRANCH TO ZTCIVHD-BUPLA.
    ENDIF.

    IT_ZSCIVIT-ZFCIVSQ    =     W_LINE  *   10.
    APPEND   IT_ZSCIVIT.
*> »ç¾÷¿µ¿ª Ã£±â.
    IF IT_ZSCIVIT-ZFPRQN NE 0.
      PERFORM  P1000_GET_ITEM_GSBER TABLES IT_GSBER
                                    USING  IT_ZSCIVIT-MATNR
                                           IT_ZSCIVIT-WERKS.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_GSBER LINES W_LINE.
  IF W_LINE EQ 1.
    READ TABLE IT_GSBER INDEX 1.
    MOVE IT_GSBER-GSBER TO ZTCIVHD-GSBER.
  ELSE.
    CLEAR : ZTCIVHD-GSBER.
  ENDIF.

  IF ZTCIVHD-ZFSVYN NE 'X'.
    DELETE IT_ZSCIVIT WHERE ZFPRQN EQ 0.
  ENDIF.

  DESCRIBE   TABLE IT_ZSCIVIT LINES W_LINE.
  IF W_LINE EQ 0.
    MESSAGE E026 WITH ZTREQHD-ZFREQNO.
  ENDIF.

  IF SY-CALLD NE 'X'.
    REFRESH : IT_ZFREQNO, IT_LOCKED.
    PERFORM P2000_SET_CIVIT_LOCK USING 'L'.
  ENDIF.

ENDFORM.                    " P2000_REQ_DATA_MOVE
*&---------------------------------------------------------------------*
*&      Form  P1000_COMMERCIAL_DOC_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_COMMERCIAL_DOC_READ.
* ¼öÀÔÀÇ·Ú Header Table
  CALL FUNCTION 'ZIM_GET_COMMERCIAL_DOC_HEADER'
       EXPORTING
            ZFCIVRN        = ZSCIVHD-ZFCIVRN
       IMPORTING
            W_ZTCIVHD      = ZTCIVHD
       TABLES
            IT_ZSCIVIT     = IT_ZSCIVIT
            IT_ZSCIVIT_ORG = IT_ZSCIVIT_ORG
            IT_ZSCIVHST    = IT_ZSCIVHST
       EXCEPTIONS
            NOT_FOUND      = 4
            NOT_INPUT      = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E374 WITH ZSCIVHD-ZFCIVRN.
    WHEN 8.
      MESSAGE E729.
  ENDCASE.

   *ZTCIVHD = ZTCIVHD.

  REFRESH : IT_GSBER.
  LOOP AT IT_ZSCIVIT.
*> »ç¾÷¿µ¿ª Ã£±â.
    PERFORM  P1000_GET_ITEM_GSBER TABLES IT_GSBER
                                  USING  IT_ZSCIVIT-MATNR
                                         IT_ZSCIVIT-WERKS.
  ENDLOOP.

*>>> LOCKING CHECK...
  REFRESH : IT_ZFREQNO, IT_LOCKED.
  ZSCIVHD-ZFPRPYN = ZTCIVHD-ZFPRPYN.

  CASE SY-TCODE.
    WHEN 'ZIM36' OR 'ZIM36L'.
      IF ZTCIVHD-ZFIVST EQ 'Y'.
        MESSAGE E383 WITH ZSCIVHD-ZFCIVRN.
      ENDIF.

      PERFORM P2000_SET_CIV_LOCK   USING 'L'.
      PERFORM P2000_SET_CIVIT_LOCK USING 'L'.
    WHEN 'ZIM38' OR 'ZIM38L'.
      IF ZTCIVHD-ZFIVST EQ 'Y'.
        MESSAGE I383 WITH ZTCIVHD-ZFCIVNO.
      ENDIF.

      PERFORM P2000_SET_CIV_LOCK USING 'L'.
  ENDCASE.

  SELECT SINGLE * FROM ZTBL WHERE ZFBLNO = IT_ZSCIVIT-ZFBLNO.
  IF SY-SUBRC EQ 0.
    MOVE ZTBL-ZFBLNO  TO ZSREQHD-ZFBLNO.
    MOVE ZTBL-ZFHBLNO TO ZSREQHD-ZFHBLNO.
  ENDIF.
** Changed by Furong on 12/03/09
  ZTCIVHD-BUDAT = SY-DATUM.
** End of change

ENDFORM.                    " P1000_COMMERCIAL_DOC_READ
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_COMMERCIAL_NO
*&---------------------------------------------------------------------*
FORM P2000_GET_COMMERCIAL_NO.
  W_ZFCIVRN = ZSCIVHD-ZFCIVRN.
  W_ZFCIVNO = ZSCIVHD-ZFCIVNO.

  REFRESH IT_ZSCIVHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSCIVHD
           FROM   ZTCIVHD
           WHERE  ZFCIVNO EQ ZSCIVHD-ZFCIVNO.

*----------------------------------------------------------------------
* position
*----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSCIVHD LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'COMMIV'.                 " COMMERCIAL INVOICE

  CALL SCREEN 0014 STARTING AT  08 3
                   ENDING   AT  80 15.

ENDFORM.                    " P2000_GET_COMMERCIAL_NO
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTCIVHD_DUP_LIST
*&---------------------------------------------------------------------*
FORM P2000_ZTCIVHD_DUP_LIST.

  WRITE : / IT_ZSCIVHD-ZFCIVRN NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSCIVHD-ZFMAVN   NO-GAP,    SY-VLINE  NO-GAP.
  IF IT_ZSCIVHD-ZFPRPYN EQ 'Y'.
    WRITE :    '±¸ºÐ'              NO-GAP,  SY-VLINE NO-GAP.
  ELSE.
    WRITE :    'ÀÏ¹Ý'              NO-GAP,  SY-VLINE NO-GAP.
  ENDIF.
  WRITE : IT_ZSCIVHD-ZFPRTE   NO-GAP,    SY-VLINE  NO-GAP,
          IT_ZSCIVHD-ZFIVAMP  CURRENCY   IT_ZSCIVHD-ZFIVAMC NO-GAP,
                                         SY-VLINE NO-GAP,
          IT_ZSCIVHD-ZFIVAMC  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSCIVHD-CDAT     NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSCIVHD-ZFIVST   NO-GAP,    SY-VLINE NO-GAP.

ENDFORM.                    " P2000_ZTCIVHD_DUP_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_BL_AMOUNT
*&---------------------------------------------------------------------*
*       ÀÚÀç AMOUNT ±¸ÇÏ±â...
*----------------------------------------------------------------------*
FORM P1000_GET_BL_AMOUNT.

  CLEAR : W_AMOUNT, W_POYNN, W_POYNY, W_ZFPOTY, W_ZFSHNO,W_TOT_FREIGHT.
  W_SAMPLE = 'N'.

  W_ZFPOTY = ZTBL-ZFPOTY.
  REFRESH : IT_ZFSHNO.

  LOOP AT IT_ZSBLIT WHERE BLOEKZ  NE 'X'
                    AND   BLMENGE NE  0.
    IF IT_ZSBLIT-BLMENGE GT 0 AND NOT IT_ZSBLIT-EBELN IS INITIAL.
      MOVE: IT_ZSBLIT-ZFSHNO TO IT_ZFSHNO-ZFSHNO,
            IT_ZSBLIT-EBELN  TO IT_ZFSHNO-EBELN.
      COLLECT IT_ZFSHNO.
    ENDIF.

    IF NOT IT_ZSBLIT-ZFPOTY IS INITIAL.   ">Non-Monetary
      ADD 1 TO W_POYNN.
      IF W_ZFPOTY IS INITIAL.
        MOVE IT_ZSBLIT-ZFPOTY TO W_ZFPOTY.
      ELSE.
        IF W_ZFPOTY NE IT_ZSBLIT-ZFPOTY.
          MOVE 'Z'  TO W_ZFPOTY.
        ENDIF.
      ENDIF.
      IF IT_ZSBLIT-ZFPOTY EQ 'S'.
        W_SAMPLE = 'Y'.
      ENDIF.
    ELSE.
      ADD 1 TO W_POYNY.
    ENDIF.

    IF IT_ZSBLIT-BPUMN IS INITIAL.
      IT_ZSBLIT-BPUMN = 1.
    ENDIF.
    IF IT_ZSBLIT-PEINH IS INITIAL.
      IT_ZSBLIT-PEINH = 1.
    ENDIF.
    IF IT_ZSBLIT-BPUMZ IS INITIAL.
      IT_ZSBLIT-BPUMZ  =  1.
    ENDIF.

    W_AMOUNT = W_AMOUNT + ( IT_ZSBLIT-BLMENGE *
                      ( IT_ZSBLIT-BPUMZ / IT_ZSBLIT-BPUMN ) *
                      ( IT_ZSBLIT-NETPR / IT_ZSBLIT-PEINH ) ).

    W_ITEM_AMOUNT = ( IT_ZSBLIT-BLMENGE *
                    ( IT_ZSBLIT-BPUMZ / IT_ZSBLIT-BPUMN ) *
                    ( IT_ZSBLIT-NETPR / IT_ZSBLIT-PEINH ) ).

    CLEAR : W_PLANED_FREIGHT.
    IF ZTBL-ZFVIA EQ 'VSL'.
      W_ZFCSCD  =  'OBC'.
    ELSE.
      W_ZFCSCD  =  'ABC'.
    ENDIF.

    PERFORM  P3000_PLANED_FREIGHT_CLAC  USING  W_ZFCSCD
                                               IT_ZSBLIT-EBELN
                                               IT_ZSBLIT-EBELP
                                               IT_ZSBLIT-BLMENGE
                                               W_ITEM_AMOUNT
                                      CHANGING W_PLANED_FREIGHT.
    W_TOT_FREIGHT  =  W_TOT_FREIGHT + W_PLANED_FREIGHT.

  ENDLOOP.
  ZTBL-ZFPLAMT  =  W_TOT_FREIGHT.

  IF W_POYNY GT 0.
    IF W_POYNN EQ 0.
      ZTBL-ZFPOYN = 'Y'.
    ELSE.
      ZTBL-ZFPOYN = 'M'.
    ENDIF.
  ELSE.
    IF W_POYNN GT 0.
      ZTBL-ZFPOYN = 'N'.
    ENDIF.
  ENDIF.

  IF W_ZFPOTY EQ 'H'.
    MOVE:'H'    TO ZTBL-ZFPOTY,
         'N'    TO ZTBL-ZFPOYN.
  ELSE.
    MOVE W_ZFPOTY TO ZTBL-ZFPOTY.
  ENDIF.

  IF ZTBL-ZFPONC IS INITIAL.
    MOVE  '26'  TO  ZTBL-ZFPONC.
  ENDIF.

  ZSREQHD-ZFBLAMT = W_AMOUNT.
  ZTBL-ZFBLAMT    = W_AMOUNT.
  ZSREQHD-ZFBLAMC = ZTBL-ZFBLAMC.

  CHECK : W_STATUS NE C_REQ_D.
  CHECK : W_STATUS NE C_OPEN_U.

  IF ZTBL-ZFBLAMT IS INITIAL.
    ZTBL-ZFBLAMT = W_AMOUNT.
  ELSE.
    IF ZTBL-ZFBLAMT NE W_AMOUNT.
      IF SY-DYNNR EQ '0200' OR SY-DYNNR EQ '0300'.
        MESSAGE S378.
      ELSE.
        MESSAGE W378.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P1000_GET_BL_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  P2000_CIV_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_CIV_DOC_DISPLAY USING    P_ZFCIVRN.

  IF P_ZFCIVRN IS INITIAL.
    MESSAGE E213.
  ENDIF.

  SET PARAMETER ID 'ZPCIVRN' FIELD P_ZFCIVRN.
  SET PARAMETER ID 'ZPCIVNO' FIELD ''.

  CALL TRANSACTION 'ZIM37' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_CIV_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_PRE_PAYMENT_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_PRE_PAYMENT_LOCK.

  LOOP AT IT_ZSCIVIT.
    READ TABLE IT_ZFREQNO  WITH KEY ZFREQNO = IT_ZSCIVIT-ZFREQNO.
    IF SY-SUBRC NE 0.
      CLEAR : IT_ZFREQNO.
      MOVE : IT_ZSCIVIT-ZFREQNO   TO    IT_ZFREQNO-ZFREQNO.
      APPEND  IT_ZFREQNO.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZFREQNO WHERE LOCKED = SPACE.
    W_TABIX = SY-TABIX.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = IT_ZFREQNO-ZFREQNO
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      W_MSGV1 = SY-MSGV1.
*>>> LOCKED ERROR½Ã......
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCIVHD'
           EXPORTING
                ZFCIVRN = ZTCIVHD-ZFCIVRN.

      PERFORM   P2000_COMMERICIAL_UNLOCK.

      MESSAGE E510 WITH W_MSGV1 'Import Document'
                   IT_ZFREQNO-ZFREQNO ''
                   RAISING DOCUMENT_LOCKED.
    ELSE.
      IT_ZFREQNO-LOCKED = 'X'.
      MODIFY  IT_ZFREQNO   INDEX  W_TABIX.

      CLEAR : IT_LOCKED.
      IT_LOCKED-ZFREQNO = IT_ZFREQNO-ZFREQNO.
      IT_LOCKED-LOCKED  = 'X'.
      APPEND IT_LOCKED.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_PRE_PAYMENT_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_GEN_PAYMENT_LOCK
*&---------------------------------------------------------------------*
FORM P2000_GEN_PAYMENT_LOCK.

  LOOP AT IT_ZSCIVIT.
    IF ZTCIVHD-ZFREQTY EQ 'LO' OR ZTCIVHD-ZFREQTY EQ 'PU'.
      READ TABLE IT_ZFREQNO  WITH KEY ZFREQNO = IT_ZSCIVIT-ZFREQNO.
      IF SY-SUBRC NE 0.
        MOVE : IT_ZSCIVIT-ZFREQNO   TO    IT_ZFREQNO-ZFREQNO.
        APPEND  IT_ZFREQNO.
      ENDIF.
    ELSE.
      READ TABLE IT_ZFREQNO  WITH KEY ZFBLNO = IT_ZSCIVIT-ZFBLNO.
      IF SY-SUBRC NE 0.
        MOVE : IT_ZSCIVIT-ZFBLNO   TO    IT_ZFREQNO-ZFBLNO.
        APPEND  IT_ZFREQNO.
      ENDIF.
    ENDIF.
  ENDLOOP.
*>>>
  LOOP AT IT_ZFREQNO WHERE LOCKED = SPACE.
    W_TABIX = SY-TABIX.
    IF ZTCIVHD-ZFREQTY EQ 'LO' OR ZTCIVHD-ZFREQTY EQ 'PU'.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
           EXPORTING
                ZFREQNO = IT_ZFREQNO-ZFREQNO
           EXCEPTIONS
                OTHERS  = 1.
      IF SY-SUBRC <> 0.
        W_MSGV1 = SY-MSGV1.
*>>> LOCKED ERROR½Ã......
        PERFORM   P2000_COMMERICIAL_UNLOCK.

        MESSAGE E510 WITH W_MSGV1 'Import request Document'
                     IT_ZFREQNO-ZFREQNO ''
                     RAISING DOCUMENT_LOCKED.
      ELSE.
        IT_ZFREQNO-LOCKED = 'X'.
        MODIFY  IT_ZFREQNO   INDEX  W_TABIX.

        CLEAR : IT_LOCKED.
        IT_LOCKED-ZFBLNO = IT_ZFREQNO-ZFBLNO.
        IT_LOCKED-LOCKED  = 'X'.
        APPEND IT_LOCKED.
      ENDIF.
    ELSE.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLDOC'
           EXPORTING
                ZFBLNO = IT_ZFREQNO-ZFBLNO
           EXCEPTIONS
                OTHERS = 1.
      IF SY-SUBRC <> 0.
        W_MSGV1 = SY-MSGV1.
*>>> LOCKED ERROR½Ã......
        PERFORM   P2000_COMMERICIAL_UNLOCK.

        MESSAGE E510 WITH W_MSGV1 'B/L Document'
                     IT_ZFREQNO-ZFBLNO ''
                     RAISING DOCUMENT_LOCKED.
      ELSE.
        IT_ZFREQNO-LOCKED = 'X'.
        MODIFY  IT_ZFREQNO   INDEX  W_TABIX.

        CLEAR : IT_LOCKED.
        IT_LOCKED-ZFBLNO = IT_ZFREQNO-ZFBLNO.
        IT_LOCKED-LOCKED  = 'X'.
        APPEND IT_LOCKED.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_GEN_PAYMENT_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_CIVIT_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0352   text
*----------------------------------------------------------------------*
FORM P2000_SET_CIVIT_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    IF ZSCIVHD-ZFPRPYN EQ 'Y'.      " ¼±±Þ±ÝÀÏ °æ¿ì..
      PERFORM  P2000_PRE_PAYMENT_LOCK.
    ELSE.
      PERFORM  P2000_GEN_PAYMENT_LOCK.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    PERFORM   P2000_COMMERICIAL_UNLOCK.
  ENDIF.

ENDFORM.                    " P2000_SET_CIVIT_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_COMMERICIAL_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_COMMERICIAL_UNLOCK.

*   CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCIVHD'
*        EXPORTING
*              ZFCIVRN                 =     ZTCIVHD-ZFCIVRN.
*
  IF ZTCIVHD-ZFPRPYN EQ 'Y'.      " ¼±±Þ±ÝÀÏ °æ¿ì..
    LOOP AT IT_LOCKED WHERE LOCKED = 'X'.
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
           EXPORTING
                ZFREQNO = IT_LOCKED-ZFREQNO.
    ENDLOOP.
  ELSE.                           " ÀÏ¹Ý¹®¼­ÀÏ °æ¿ì.
    IF ZTCIVHD-ZFREQTY EQ 'LO' OR ZTCIVHD-ZFREQTY EQ 'PU'.
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
           EXPORTING
                ZFREQNO = IT_LOCKED-ZFREQNO.
    ELSE.
      LOOP AT IT_LOCKED WHERE LOCKED = 'X'.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLDOC'
             EXPORTING
                  ZFBLNO = IT_LOCKED-ZFBLNO.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_COMMERICIAL_UNLOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_CIVIT_LOCK_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SET_CIVIT_LOCK_ITEM.

  IF ZTCIVHD-ZFPRPYN EQ 'Y'.      " ¼±±Þ±ÝÀÏ °æ¿ì..
    READ TABLE IT_ZFREQNO  WITH KEY ZFREQNO = IT_ZSCIVIT-ZFREQNO.
    IF SY-SUBRC NE 0.
      CLEAR : IT_ZFREQNO.
      MOVE : IT_ZSCIVIT-ZFREQNO   TO    IT_ZFREQNO-ZFREQNO.
      APPEND  IT_ZFREQNO.
    ENDIF.
    READ TABLE IT_ZFREQNO  WITH KEY ZFREQNO = IT_ZSCIVIT-ZFREQNO.
    W_TABIX = SY-TABIX.

    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = IT_ZFREQNO-ZFREQNO
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Import Document'
                   IT_ZFREQNO-ZFREQNO ''
                   RAISING DOCUMENT_LOCKED.
    ELSE.
      IT_ZFREQNO-LOCKED = 'X'.
      MODIFY  IT_ZFREQNO INDEX W_TABIX.

      CLEAR : IT_LOCKED.
      IT_LOCKED-ZFREQNO = IT_ZFREQNO-ZFREQNO.
      IT_LOCKED-LOCKED  = 'X'.
      APPEND IT_LOCKED.
    ENDIF.

  ELSE.

    READ TABLE IT_ZFREQNO  WITH KEY ZFBLNO = IT_ZSCIVIT-ZFBLNO.
    IF SY-SUBRC NE 0.
      MOVE : IT_ZSCIVIT-ZFBLNO   TO    IT_ZFREQNO-ZFBLNO.
      APPEND  IT_ZFREQNO.
    ENDIF.
    READ TABLE IT_ZFREQNO  WITH KEY ZFBLNO = IT_ZSCIVIT-ZFBLNO.
    W_TABIX = SY-TABIX.

    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLDOC'
         EXPORTING
              ZFBLNO = IT_ZFREQNO-ZFBLNO
         EXCEPTIONS
              OTHERS = 1.
    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'B/L Document'
                   IT_ZFREQNO-ZFBLNO ''
                   RAISING DOCUMENT_LOCKED.
    ELSE.
      IT_ZFREQNO-LOCKED = 'X'.
      MODIFY  IT_ZFREQNO   INDEX  W_TABIX.

      CLEAR : IT_LOCKED.
      IT_LOCKED-ZFBLNO = IT_ZFREQNO-ZFBLNO.
      IT_LOCKED-LOCKED  = 'X'.
      APPEND IT_LOCKED.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_SET_CIVIT_LOCK_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_EXEC_MAT_INVOICE_VERIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_EXEC_MAT_INVOICE_VERIFY.
  W_OK_CODE = OK-CODE.
*-----------------------------------------------------------------------
* AUTHORITY CHECK
  PERFORM P2000_AUTHORITY_CHECK USING W_OK_CODE.
*-----------------------------------------------------------------------
* Lock Object Set
  IF W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D.
    PERFORM P2000_SET_LOCK.
  ENDIF.

* MESSAGE BOX
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
      IF W_STATUS NE C_REQ_D.
        PERFORM  P3000_DB_MODIFY_SCRCOM.
      ENDIF.
      PERFORM  P3000_CALL_INVOICE_VERIFY
               USING ZTCIVHD-ZFCIVRN.

      PERFORM  P2000_SET_UNLOCK.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      CLEAR OK-CODE.
      IF W_ERR_CHK NE 'Y'.
** Changed by Furong on 12/03/09
        IF W_OK_CODE = 'MIRO'.
          SET PARAMETER ID 'ZPBLNO' FIELD  ZSREQHD-ZFBLNO.
          CALL TRANSACTION 'ZIM22'.
        ENDIF.
** End of change
        MESSAGE  S917.
      ENDIF.

      LEAVE SCREEN.
    WHEN 'N'.              " No...
      MESSAGE  S957.
      CLEAR OK-CODE.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_EXEC_MAT_INVOICE_VERIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_MIRO_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MIRO_MESSAGE.

  ZSCIVHD-BLDAT = ZTCIVHD-ZFCIDT.
  ZSCIVHD-ZFBDT = ZTCIVHD-ZFGSDT.
  ZSCIVHD-BUDAT = ZTCIVHD-BUDAT.

  IF W_STATUS EQ C_REQ_C OR W_STATUS EQ C_REQ_U
                         OR W_STATUS EQ C_OPEN_U.

    PERFORM P2000_MESSAGE_CHECK.

    CASE SY-LANGU.
      WHEN '3'.
        PERFORM P2000_MESSAGE_BOX USING '¹°´ë »ý¼º È®ÀÎ'
                       '¸ÕÀú ÀúÀå ÈÄ ¹°´ë ÀüÇ¥¸¦ »ý¼ºÇØ¾ß ÇÕ´Ï´Ù.'
                       'ÀúÀåÇÏ½Ã°Ú½À´Ï±î?'
                       'Y'
                       '1'.
      WHEN OTHERS.
        IF W_DOWN_PAY GT 0.
          WRITE W_DOWN_PAY CURRENCY 'USD' TO TEMP_WRBTR.
          CONCATENATE 'Downpayment ' TEMP_WRBTR ' exist'
                                      INTO W_MESSAGE1.
          CONCATENATE W_MESSAGE1 'Do you want to save?'
                                      INTO W_MESSAGE2.
        ELSE.
          MOVE 'Do you want to save?' TO W_MESSAGE2.
        ENDIF.
        PERFORM P2000_MESSAGE_BOX USING
             'Gross price create confirm'
             'You should create gross price after save.'
             W_MESSAGE2
             'Y'
             '1'.
    ENDCASE.
  ELSE.
    CASE SY-LANGU.
      WHEN '3'.
        PERFORM P2000_MESSAGE_BOX USING '¹°´ë »ý¼º È®ÀÎ'
                       '¹°´ë ÀüÇ¥¸¦ »ý¼º ÇÕ´Ï´Ù.'
                       '»ý¼ºÇÏ½Ã°Ú½À´Ï±î?' " Message #2
                       'Y'             " Ãë¼Ò ¹öÆ° À¯.
                       '1'.            " default button
      WHEN OTHERS.
        IF W_DOWN_PAY GT 0.
          WRITE W_DOWN_PAY CURRENCY 'USD' TO TEMP_WRBTR.
          CONCATENATE 'Downpayment ' TEMP_WRBTR ' exist'
                                      INTO W_MESSAGE1.
          CONCATENATE W_MESSAGE1 'Do you want to create?'
                                      INTO W_MESSAGE2.
        ELSE.
          MOVE 'Do you want to create?' TO W_MESSAGE2.
        ENDIF.
        PERFORM P2000_MESSAGE_BOX USING
               'Gross price create confirm'
               'Now create gross price.'
               W_MESSAGE2
               'Y'
               '1'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_MIRO_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_INVOICE_VERIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_CALL_INVOICE_VERIFY   USING  P_ZFCIVRN.

  MOVE 'N'  TO  W_ERR_CHK.

  CLEAR : ZTIMIMG11, ZTIMIMG00.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE S961.
    MOVE 'Y'  TO  W_ERR_CHK.
    EXIT.
  ENDIF.

*>> INVOICE VERIFY STATUS CHECKING...
  IF W_OK_CODE EQ 'MIRO'.
    IF ZTCIVHD-ZFIVST EQ 'Y'.
      MESSAGE S600 WITH 'Invoice Verification completion'.
      MOVE 'Y'  TO  W_ERR_CHK.
      EXIT.
    ENDIF.
  ELSE.
    IF ZTCIVHD-ZFIVST EQ 'N'.
      MESSAGE S600 WITH 'Invoice Verification incompletion'.
      MOVE 'Y'  TO  W_ERR_CHK.
      EXIT.
    ENDIF.
  ENDIF.

*>> INVOICE VERIFY Á¾·ù..
  IF ZTIMIMG00-ZFIVTY EQ 'L'.      ">Logistics Invoice Verification
    IF W_OK_CODE EQ 'MIRO'.
      PERFORM P3000_BAPI_CALL         USING ZTCIVHD-ZFCIVRN
                                            'X'  SPACE
                                            ZSCIVHD-BLDAT
                                            ZSCIVHD-BUDAT
                                            ZSCIVHD-ZFBDT.
    ELSE.
      PERFORM P3000_BAPI_CALL         USING ZTCIVHD-ZFCIVRN
                                            SPACE 'X'
                                            ZSCIVHD-BLDAT
                                            ZSCIVHD-BUDAT
                                            ZSCIVHD-ZFBDT.
    ENDIF.
*     REFRESH : BDCDATA.
*     PERFORM P3000_MIRO_DATA_MAKE    USING ZTIV-ZFIVNO.
*     PERFORM P2000_CALL_TRANSACTION  USING 'MIRO'
*                                     CHANGING  W_SUBRC.
  ELSEIF ZTIMIMG00-ZFIVTY EQ 'C'.  ">Conventional Invoice Verification
    REFRESH : BDCDATA.
    PERFORM P3000_MRHR_DATA_MAKE    USING ZTIV-ZFIVNO
                                          'X'.
    PERFORM P2000_CALL_TRANSACTION  USING 'MRHR'
                                    CHANGING  W_SUBRC.
  ELSE.
    MESSAGE E787 WITH ZTCIVHD-ZFCIVRN.
  ENDIF.

  IF W_SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " P3000_CALL_INVOICE_VERIFY
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTCGHD_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_ZTCGHD_MODIFY.
* »ý¼ºÀÏ °æ¿ì ´ÙÀ½ ¹øÈ£¸¦ Ã¤¹ø..
  IF W_STATUS EQ C_REQ_C.
    PERFORM   P2000_GET_NUMBER_NEXT  USING  'CG'  ZTCGHD-ZFCGNO.
    CLEAR : *ZTCGHD.
  ENDIF.

  CALL FUNCTION 'ZIM_CARGO_WORK_MODIFY'
       EXPORTING
            W_OK_CODE      = W_OK_CODE
            ZFCGNO         = ZTCGHD-ZFCGNO
            ZFSTATUS       = W_STATUS
            W_ZTCGHD       = ZTCGHD
            W_ZTCGHD_OLD   = *ZTCGHD
       TABLES
            IT_ZSCGIT      = IT_ZSCGIT
            IT_ZSCGIT_OLD  = IT_ZSCGIT_ORG
            IT_ZSCGCST     = IT_ZSCGCST
            IT_ZSCGCST_OLD = IT_ZSCGCST_ORG
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E952.
  ELSE.
    SET PARAMETER ID 'ZPCGNO' FIELD ZTCGHD-ZFCGNO.
    MESSAGE  S386  WITH  ZTCGHD-ZFCGNO.
  ENDIF.

ENDFORM.                    " P3000_ZTCGHD_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_SELECT_CG_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_LINE_SELECT_CG_ITEM.
  CLEAR : ZSCGIT.
  W_COUNT = 0.
  LOOP AT IT_ZSCGIT WHERE ZFMARK = 'X'.
    W_COUNT = W_COUNT + 1.
    MOVE-CORRESPONDING  IT_ZSCGIT  TO ZSCGIT.
  ENDLOOP.

ENDFORM.                    " P2000_LINE_SELECT_CG_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_CALL_TCODE_CG_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_LINE_CALL_TCODE_CG_ITEM.

  CASE W_COUNT.
    WHEN 0.        MESSAGE S962.   EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965.   EXIT.
  ENDCASE.

  CASE  W_OK_CODE.
    WHEN 'PODP'.          " P/O DISPLAY
      PERFORM  P2000_PO_DOC_DISPLAY USING ZSCGIT-EBELN ''.
    WHEN 'IMDP'.          " IMPORT L/C DISPLAY
      PERFORM  P2000_LC_DOC_DISPLAY USING ZSCGIT-ZFREQNO ''.
    WHEN 'BLDP'.          " IMPORT L/C DISPLAY
      PERFORM  P2000_BL_DOC_DISPLAY USING ZSCGIT-ZFBLNO.
  ENDCASE.


ENDFORM.                    " P2000_LINE_CALL_TCODE_CG_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ZTCGHD_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7307   text
*----------------------------------------------------------------------*
FORM P2000_SET_ZTCGHD_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTCGHD'
         EXPORTING
              ZFCGNO = ZTCGHD-ZFCGNO
         EXCEPTIONS
              OTHERS = 1.

    IF SY-SUBRC <> 0.
      W_MSGV1 = SY-MSGV1.
      IF SY-TCODE EQ 'ZIM82'.
        PERFORM P2000_SET_BL_REQDOC_LOCK    USING    'U'.
      ENDIF.

      MESSAGE E510 WITH W_MSGV1 'Loading/Unloading Document'
                            ZTCGHD-ZFCGNO ''
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCGHD'
         EXPORTING
              ZFCGNO = ZTCGHD-ZFCGNO.
  ENDIF.

ENDFORM.                    " P2000_SET_ZTCGHD_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_SELECT_CARGO
*&---------------------------------------------------------------------*
FORM P2000_LINE_SELECT_CARGO.
  W_COUNT = 0.
  LOOP AT IT_ZSCGCST WHERE ZFMARK = 'X'.
    W_COUNT = W_COUNT + 1.
    MOVE-CORRESPONDING  IT_ZSCGCST  TO ZSCGCST.
  ENDLOOP.

ENDFORM.                    " P2000_LINE_SELECT_CARGO
*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_CALL_TCODE_CARGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_LINE_CALL_TCODE_CARGO.
  CASE W_COUNT.
    WHEN 0.        MESSAGE S962.   EXIT.
    WHEN 1.
    WHEN OTHERS.   MESSAGE S965.   EXIT.
  ENDCASE.

  CASE OK-CODE.
    WHEN 'VEN1'.
      PERFORM  P2000_VENDOR_MASTER USING ZSCGCST-LIFNR.
    WHEN 'VEN2'.
      PERFORM  P2000_VENDOR_MASTER USING ZSCGCST-ZFPAY.
*      WHEN 'VEN3'.
*         PERFORM  P2000_VENDOR_MASTER USING ZTSCGST-ZFTRCO.
    WHEN 'MKPF'.
      IF ZSCGCST-GJAHR IS INITIAL OR ZSCGCST-BELNR IS INITIAL.
        MESSAGE S252.   EXIT.
      ENDIF.
      SET PARAMETER ID 'BUK'     FIELD ZSCGCST-BUKRS.
      SET PARAMETER ID 'GJR'     FIELD ZSCGCST-GJAHR.
      SET PARAMETER ID 'BLN'     FIELD ZSCGCST-BELNR.
      CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
  ENDCASE.

ENDFORM.                    " P2000_LINE_CALL_TCODE_CARGO
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CARGO_CHARGE_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CARGO_CHARGE_RECORD.
* B/L ÇØ¿Ü ¿î?
  CLEAR  W_ZFCSQ.
  REFRESH : IT_ZSIMIMG08.
  DELETE IT_ZSCGCST WHERE BELNR EQ SPACE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG08
                                        FROM ZTIMIMG08
                                        WHERE ZFCDTY EQ '007'.
  SORT IT_ZSIMIMG08  BY   ZFCD2.

  LOOP AT IT_ZSIMIMG08.
    CLEAR : IT_ZSCGCST.
    READ TABLE IT_ZSCGCST WITH KEY
                          ZFCSCD = IT_ZSIMIMG08-ZFCD.
    IF SY-SUBRC EQ 0. CONTINUE. ENDIF.

    IF W_ZFCSQ = 0.
      W_ZFCSQ = 10.
    ELSE.
      W_ZFCSQ = W_ZFCSQ + 10.
    ENDIF.

    READ TABLE IT_ZSCGCST WITH KEY ZFCSQ = W_ZFCSQ.
    IF SY-SUBRC EQ 0. CONTINUE. ENDIF.

    IT_ZSCGCST-ZFCSQ    =    W_ZFCSQ.
    IT_ZSCGCST-ZFCSCD   =    IT_ZSIMIMG08-ZFCD.
    IT_ZSCGCST-ZFCDNM   =    IT_ZSIMIMG08-ZFCDNM.
    IT_ZSCGCST-ZFKRW    =    'KRW'.
    IT_ZSCGCST-WERKS    =    ZTCGHD-WERKS.
    IT_ZSCGCST-MWSKZ    =    IT_ZSIMIMG08-ZFCD5.
    IT_ZSCGCST-BUKRS    =    ZTCGHD-BUKRS.

    IF NOT IT_ZSCGCST-LIFNR IS INITIAL.
      SELECT SINGLE ZTERM INTO IT_ZSCGCST-ZTERM   " Payment Term
             FROM LFB1
             WHERE LIFNR = IT_ZSCGCST-LIFNR
             AND BUKRS   = ZTCGHD-BUKRS.
* ===> TAX RATE
      PERFORM   P1000_READ_KONP   USING   IT_ZSCGCST-MWSKZ
                                          IT_ZSCGCST-KBETR
                                          IT_ZSCGCST-KONWA.

*-----------------------------------------------------------------------
* TAX CODE  ===> ºÎ°¡¼¼ ¿ø´ÜÀ§ Àý?
      IF NOT IT_ZSCGCST-KBETR IS INITIAL AND
         NOT IT_ZSCGCST-ZFCKAMT IS INITIAL.
        W_AMOUNT = IT_ZSCGCST-ZFCKAMT.
        W_AMOUNT = W_AMOUNT * IT_ZSCGCST-KBETR / 10.
        COMPUTE W_AMOUNT = TRUNC( W_AMOUNT ).
        W_AMOUNT = W_AMOUNT * 10.
        IT_ZSCGCST-ZFVAT = W_AMOUNT.
        PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSCGCST-ZFVAT
                                                   'KRW'.
      ENDIF.
    ENDIF.
*-----------------------------------------------------------------------
    APPEND IT_ZSCGCST.
  ENDLOOP.

ENDFORM.                    " P1000_READ_CARGO_CHARGE_RECORD
*&---------------------------------------------------------------------*
*&      Form  P2000_CARGO_DATA_MOVE
*&---------------------------------------------------------------------*
FORM P2000_CARGO_DATA_MOVE.
  CLEAR : ZTCGHD, *ZTCGHD, IT_ZSCGIT, IT_ZSCGIT_ORG,
          IT_ZSCGCST, IT_ZSCGCST_ORG.
  REFRESH : IT_ZSCGCST, IT_ZSCGCST_ORG, IT_ZSCGIT, IT_ZSCGIT_ORG.

  MOVE : ZSCGHD-ZFCGPT      TO     ZTCGHD-ZFCGPT,
         ZTBL-ZFETA         TO     ZTCGHD-ZFETA,
         ZTBL-ZFMSNO        TO     ZTCGHD-ZFMSNO,
         ZTBL-BUKRS         TO     ZTCGHD-BUKRS,
         SY-UNAME           TO     ZTCGHD-ZFKEYM,
         ZTBL-ZFBLAMC       TO     ZTCGHD-WAERS,
         ZSCGHD-WERKS       TO     ZTCGHD-WERKS,
         SY-UNAME           TO     ZTCGHD-ERNAM,
         SY-UNAME           TO     ZTCGHD-UNAM,
         SY-DATUM           TO     ZTCGHD-CDAT,
         SY-DATUM           TO     ZTCGHD-UDAT.

*> ¸ð¼±°ü¸®¹øÈ£.
  IF ZTBL-ZFMSNO IS INITIAL.
    MESSAGE W388 WITH ZTBL-ZFBLNO.
    CLEAR : ZTMSHD.
*   ELSE.
*      SELECT SINGLE * FROM ZTMSHD
*             WHERE ZFMSNO EQ ZTCGHD-ZFMSNO.
*      IF SY-SUBRC EQ 0.
*         ZTCGHD-BUKRS  =   ZTMSHD-BUKRS.
*      ENDIF.
  ENDIF.

*> ÀÚÀç³»¿ª...
  LOOP AT IT_ZSBLIT WHERE BLOEKZ EQ SPACE
                    AND   LOEKZ  EQ SPACE
                    AND   ELIKZ  EQ SPACE
                    AND   WERKS  EQ ZSCGHD-WERKS.
    CLEAR : IT_ZSCGIT.
    MOVE-CORRESPONDING   IT_ZSBLIT   TO    IT_ZSCGIT.
    MOVE : IT_ZSBLIT-BLMENGE         TO    IT_ZSCGIT-MENGE_BL.
*>> ±â Open ¼ö·®.
    SELECT SUM( CGMENGE )
           INTO IT_ZSCGIT-ZFCGTOT
           FROM   ZTCGIT
           WHERE  ZFBLNO   EQ   IT_ZSCGIT-ZFBLNO
           AND    ZFBLIT   EQ   IT_ZSCGIT-ZFBLIT.
    IT_ZSCGIT-CGMENGE = IT_ZSCGIT-MENGE_BL - IT_ZSCGIT-ZFCGTOT.
    IF IT_ZSCGIT-CGMENGE LT 0.
      IT_ZSCGIT-CGMENGE = 0.
    ENDIF.
    MOVE : ZTBL-ZFGMNO        TO  IT_ZSCGIT-ZFGMNO,
           ZTBL-ZFMSN         TO  IT_ZSCGIT-ZFMSN,
           ZTBL-ZFHSN         TO  IT_ZSCGIT-ZFHSN,
           ZTBL-ZFCGHNO       TO  IT_ZSCGIT-ZFCGHNO.
    CLEAR : IT_ZSCGIT-GRMENGE, IT_ZSCGIT-CCMENGE.
    APPEND   IT_ZSCGIT.
  ENDLOOP.

  CLEAR : W_LINE.
  DESCRIBE   TABLE   IT_ZSCGIT  LINES   W_LINE.
  IF W_LINE EQ 0.
    MESSAGE W384 WITH ZTBL-ZFBLNO.
  ENDIF.

*>> ÇÏ¿ªºñ¿ë INT.
  PERFORM    P1000_READ_CARGO_CHARGE_RECORD.

ENDFORM.                    " P2000_CARGO_DATA_MOVE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CARGO_WORK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CARGO_WORK_DOC.

  CALL FUNCTION 'ZIM_GET_CARGO_WORK_DOCUMENT'
       EXPORTING
            ZFCGNO         = ZSCGHD-ZFCGNO
       IMPORTING
            W_ZTCGHD       = ZTCGHD
       TABLES
            IT_ZSCGCST     = IT_ZSCGCST
            IT_ZSCGCST_ORG = IT_ZSCGCST_ORG
            IT_ZSCGIT      = IT_ZSCGIT
            IT_ZSCGIT_ORG  = IT_ZSCGIT_ORG
       EXCEPTIONS
            NOT_FOUND      = 4
            NOT_INPUT      = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E390 WITH ZSCGHD-ZFCGNO.
    WHEN 8.
      MESSAGE E391.
  ENDCASE.

*>> ºñ¿ëDOCUMENT SELECT.
  W_ZFIMDNO = ZSCGHD-ZFCGNO.
  CALL FUNCTION 'ZIM_GET_COST_DOCUMENT'
       EXPORTING
            ZFCSTGRP    = '007'
            ZFIMDNO     = W_ZFIMDNO
       TABLES
            IT_ZSIMCOST = IT_ZSIMCOST.

   *ZTCGHD = ZTCGHD.

*>> ÇÏ¿ªÇ×.
*  SELECT SINGLE   ZFCDNM INTO ZSIMIMG08-ZFCDNM
*         FROM   ZTIMIMG08
*         WHERE  ZFCDTY    EQ    '002'
*         AND    ZFCD      EQ    ZTCGHD-ZFCGPT.
*   SELECT MAX( PORTT ) INTO ZSIMIMG08-ZFCDNM
  SELECT SINGLE PORTT INTO ZSIMIMG08-ZFCDNM
         FROM  ZTIEPORT
         WHERE PORT  EQ ZTCGHD-ZFCGPT
         AND   LAND1 EQ 'KR'.

*> ¸ð¼±°ü¸®¹øÈ£.
  IF ZTCGHD-ZFMSNO IS INITIAL.
    CLEAR : ZTMSHD.
  ELSE.
    SELECT SINGLE * FROM ZTMSHD
           WHERE ZFMSNO EQ ZTCGHD-ZFMSNO.
  ENDIF.

*-----------------------------------------------------------------------
* º¯°æÀÌ·Â.
*-----------------------------------------------------------------------
   *ZTCGHD = ZTCGHD.

  IF SY-TCODE EQ 'ZIM81' OR SY-TCODE EQ 'ZIM82'.
    PERFORM P2000_SET_BL_REQDOC_LOCK USING    'L'.
  ENDIF.

ENDFORM.                    " P1000_READ_CARGO_WORK_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_CARGO_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_CARGO_DOC_ITEM_SELECT.
  W_ZFCGNO    = ZSCGHD-ZFCGNO.

  REFRESH IT_ZSCGHD.
* Table Multi-Select
  SELECT I~ZFBLNO H~ZFMSNO H~ZFCGNO H~ZFARVLDT
                  H~ZFCGPT H~ZFETA M~ZFMSNM B~ZFHBLNO
           INTO CORRESPONDING FIELDS OF TABLE IT_ZSCGHD
           FROM  ( ( ZTCGHD AS H INNER JOIN ZTCGIT AS I
                     ON  H~ZFCGNO  EQ  I~ZFCGNO )
                     INNER JOIN ZTMSHD AS M
                     ON H~ZFMSNO   EQ  M~ZFMSNO )
                     INNER JOIN ZTBL   AS B
                     ON I~ZFBLNO   EQ  B~ZFBLNO
           WHERE  I~ZFBLNO  EQ  ZTBL-ZFBLNO
           AND    H~ZFCGPT  EQ  ZSCGHD-ZFCGPT
           GROUP BY I~ZFBLNO H~ZFMSNO H~ZFCGNO H~ZFARVLDT
                    H~ZFCGPT H~ZFETA M~ZFMSNM B~ZFHBLNO.
  DESCRIBE TABLE IT_ZSCGHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'CGLIST'.                 " ÇÏ¿ª Áßº¹ µ¥ÀÌÅÍ.
  CALL SCREEN 0014 STARTING AT  07 3
                   ENDING   AT  87 15.

ENDFORM.                    " P2000_CARGO_DOC_ITEM_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTCGHD_DUP_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_ZTCGHD_DUP_LIST.

  WRITE : / SY-VLINE, IT_ZSCGHD-ZFCGNO COLOR COL_KEY INTENSIFIED,
            SY-VLINE.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE : IT_ZSCGHD-ZFMSNM,     SY-VLINE,
          IT_ZSCGHD-ZFETA,      SY-VLINE,
          IT_ZSCGHD-ZFARVLDT,   SY-VLINE.

ENDFORM.                    " P2000_ZTCGHD_DUP_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_CARGO_DOC_SELECT
*&---------------------------------------------------------------------*
FORM P2000_CARGO_DOC_SELECT.
  W_ZFCGNO    = ZSCGHD-ZFCGNO.

  REFRESH IT_ZSCGHD.
* Table Multi-Select
  SELECT I~ZFBLNO H~ZFMSNO H~ZFCGNO H~ZFARVLDT H~ZFCGPT
                  H~ZFCGPT H~ZFETA M~ZFMSNM B~ZFHBLNO
           INTO CORRESPONDING FIELDS OF TABLE IT_ZSCGHD
           FROM  ( ( ZTCGHD AS H INNER JOIN ZTCGIT AS I
                     ON  H~ZFCGNO  EQ  I~ZFCGNO )
                     INNER JOIN ZTMSHD AS M
                     ON H~ZFMSNO   EQ  M~ZFMSNO )
                     INNER JOIN ZTBL   AS B
                     ON I~ZFBLNO   EQ  B~ZFBLNO
           WHERE  I~ZFBLNO  EQ  ZTBL-ZFBLNO
*            AND    H~ZFCGPT  EQ  ZSCGHD-ZFCGPT
           GROUP BY I~ZFBLNO H~ZFMSNO H~ZFCGNO H~ZFARVLDT H~ZFCGPT
                    H~ZFCGPT H~ZFETA M~ZFMSNM B~ZFHBLNO.

  DESCRIBE TABLE IT_ZSCGHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'CGLIST1'.                 " ÇÏ¿ª Áßº¹ µ¥ÀÌÅÍ.
  CALL SCREEN 0014 STARTING AT  07 3
                   ENDING   AT  87 15.


ENDFORM.                    " P2000_CARGO_DOC_SELECT
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTCGHD_PORT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_ZTCGHD_PORT_LIST.

  WRITE : / SY-VLINE,
            IT_ZSCGHD-ZFCGNO COLOR COL_KEY INTENSIFIED,
            SY-VLINE.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE : IT_ZSCGHD-ZFMSNM,     SY-VLINE,
          IT_ZSCGHD-ZFCGPT,     SY-VLINE,
          IT_ZSCGHD-ZFETA,      SY-VLINE,
          IT_ZSCGHD-ZFARVLDT,   SY-VLINE.

ENDFORM.                    " P2000_ZTCGHD_PORT_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_REF_BL_DOC_INPUT
*&---------------------------------------------------------------------*
FORM P2000_REF_BL_DOC_INPUT.

  SPOP-TITEL = 'Create Åë°ü¿äÃ»/ÀÔ°í¿äÃ» : Copy from B/L'.
  OPTION = 1.

  CALL SCREEN 3118 STARTING AT 27 6
                   ENDING   AT 70 10.

  IF ANTWORT EQ 'C' OR ANTWORT EQ 'N'.       " Cancel Or No
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_REF_BL_DOC_INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_REF_REQ_DOC_INPUT
*&---------------------------------------------------------------------*
FORM P2000_REF_REQ_DOC_INPUT.

  SPOP-TITEL = 'Create Customs/G/R Request : Copy from Import Request'.
  OPTION = 1.

  CALL SCREEN 3119 STARTING AT 15 6
                   ENDING   AT 70 10.

  IF ANTWORT EQ 'C' OR ANTWORT EQ 'N'.       " Cancel Or No
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_REF_REQ_DOC_INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_TO_CC_DATA
*&---------------------------------------------------------------------*
FORM P2000_BL_TO_CC_DATA.

  DATA : L_TEXT_EXRATE(255)    TYPE C,
         L_FOREIGN_FACTOR(255) TYPE C,
         L_FACTOR              TYPE P.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE E963.
  ENDIF.

  MOVE : ZTBL-ZFBLNO       TO       ZTIV-ZFBLNO,
         ZTBL-ZFBLAMC      TO       ZTIV-ZFIVAMC,
         ZTBL-BUKRS        TO       ZTIV-BUKRS,
         ZTBL-LIFNR        TO       ZTIV-LIFNR,
         ZTBL-ZFRPTTY      TO       ZTIV-ZFRPTTY,
         ZTBL-ZFPONC       TO       ZTIV-ZFPONC,
         ZTBL-ZFPOYN       TO       ZTIV-ZFPOYN,
         ZTBL-ZFISYN       TO       ZTIV-ZFISYN,
         ZTBL-ZFVIA        TO       ZTIV-ZFVIA,
         ZTBL-ZFAPRTC      TO       ZTIV-ZFAPRTC,
         SY-DATUM          TO       ZTIV-ZFCCDT,
         SY-UNAME          TO       ZTIV-ERNAM,
         SY-DATUM          TO       ZTIV-CDAT,
         SY-UNAME          TO       ZTIV-UNAM,
         SY-DATUM          TO       ZTIV-UDAT.

  ">> Local Currency Get.
  SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTIV-BUKRS.
  MOVE  T001-WAERS        TO       ZTIV-ZFKRW.

  IF ZTIV-ZFIVAMC EQ ZTIV-ZFKRW.
    ZTIV-ZFEXRT = 1.
    ZTIV-FFACT  = 1.
  ELSE.
    ">> Exchage Rate
    PERFORM   P2000_GET_EX_RATE_NODIALOG   USING ZTIV-ZFIVAMC
                                                 ZTIV-ZFKRW
                                                 ZTIV-ZFCCDT
                                        CHANGING ZTIV-ZFEXRT
                                                 ZTIV-FFACT.
  ENDIF.

  W_TOT_AMOUNT = 0.      W_TOT_AMOUNT1 = 0.
  W_LINE       = 0.

  LOOP AT IT_ZSBLIT WHERE BLOEKZ EQ SPACE.

    IF SY-TABIX EQ 1.
      SELECT SINGLE ZFREQTY INTO ZTIV-ZFREQTY
                FROM ZTREQHD
               WHERE ZFREQNO = IT_ZSBLIT-ZFREQNO.
    ENDIF.

    MOVE-CORRESPONDING  IT_ZSBLIT   TO    IT_ZSIVIT.
    MOVE : IT_ZSBLIT-BLMENGE        TO    IT_ZSIVIT-MENGE_BL,
           ZTBL-ZFBLAMC             TO    IT_ZSIVIT-ZFIVAMC,
           ZTIV-ZFKRW               TO    IT_ZSIVIT-ZFKRW,
           IT_ZSBLIT-ZFBLIT         TO    IT_ZSIVIT-ZFIVDNO.

    IF NOT IT_ZSIVIT-LOEKZ IS INITIAL.
      CONTINUE.
    ENDIF.

    IT_ZSIVIT-ZFCCTOT = 0.
    SELECT SUM( CCMENGE ) SUM( GRMENGE )
                          INTO (IT_ZSIVIT-ZFCCTOT,
                                IT_ZSIVIT-ZFGRTOT)
                          FROM ZTIVIT
                          WHERE ZFBLNO  EQ   IT_ZSIVIT-ZFBLNO
                          AND   ZFBLIT  EQ   IT_ZSIVIT-ZFBLIT.

    IF IT_ZSIVIT-BPUMN IS INITIAL.
      IT_ZSIVIT-BPUMN = 1.
    ENDIF.
    IF IT_ZSIVIT-PEINH IS INITIAL.
      IT_ZSIVIT-PEINH = 1.
    ENDIF.
    IT_ZSIVIT-GRMENGE =  IT_ZSIVIT-MENGE_BL - IT_ZSIVIT-ZFGRTOT.
    IF IT_ZSIVIT-GRMENGE LT 0.
      IT_ZSIVIT-GRMENGE = 0.
    ENDIF.
    IT_ZSIVIT-CCMENGE =  IT_ZSIVIT-MENGE_BL - IT_ZSIVIT-ZFCCTOT.
    IF IT_ZSIVIT-CCMENGE GT 0.
      IF IT_ZSIVIT-BPUMN IS INITIAL.
        IT_ZSIVIT-ZFIVAMT = IT_ZSIVIT-CCMENGE *
                ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ).
      ELSE.
        IT_ZSIVIT-ZFIVAMT = IT_ZSIVIT-CCMENGE *
                ( IT_ZSIVIT-BPUMZ / IT_ZSIVIT-BPUMN ) *
                ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ).
      ENDIF.
      ">> Local Currency Compute
      PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIVIT-ZFIVAMT
                                              IT_ZSIVIT-ZFIVAMC
                                              IT_ZSIVIT-ZFIVAMK.
      IF ZTIV-FFACT IS INITIAL.
        ZTIV-FFACT = 1.
      ENDIF.
      IT_ZSIVIT-ZFIVAMK = ( ZTIV-ZFEXRT / ZTIV-FFACT )
                                      * IT_ZSIVIT-ZFIVAMK.

      PERFORM SET_CURR_CONV_TO_INTERNAL USING
              IT_ZSIVIT-ZFIVAMK ZTIV-ZFKRW.

    ELSE.
      IT_ZSIVIT-CCMENGE = 0.
    ENDIF.
    ADD IT_ZSIVIT-ZFIVAMT  TO  W_TOT_AMOUNT.
    ADD IT_ZSIVIT-ZFIVAMK  TO  W_TOT_AMOUNT1.

    IT_ZSIVIT-UMSON  =  'X'.

    ">> IF Quantity is zero Then Internal table delete
    IF IT_ZSIVIT-GRMENGE LE 0.
      CLEAR : IT_ZSIVIT-UMSON.
    ENDIF.
    IF NOT IT_ZSIVIT-ZFPOTY IS INITIAL AND IT_ZSIVIT-ZFPOTY NE 'S'.
      CLEAR : IT_ZSIVIT-UMSON.
    ENDIF.

    ">> Text Material G/R Mark Clear
    IF IT_ZSIVIT-EBELN  IS INITIAL.
      IF IT_ZSIVIT-ZFPOTY EQ 'S' AND IT_ZSIVIT-MATNR IS INITIAL.
        CLEAR : IT_ZSIVIT-GRMENGE.
      ENDIF.
    ENDIF.

    CLEAR : IT_ZSIVIT-REPOS.
    IF NOT IT_ZSIVIT-EBELN IS INITIAL AND
       NOT IT_ZSIVIT-EBELP IS INITIAL.
      SELECT SINGLE * FROM EKPO
             WHERE  EBELN  EQ   IT_ZSIVIT-EBELN
             AND    EBELP  EQ   IT_ZSIVIT-EBELP.
      ">> Based P/O Item G/R Marking
      MOVE EKPO-REPOS  TO   IT_ZSIVIT-REPOS.
      IF EKPO-REPOS IS INITIAL AND  NOT EKPO-MATNR IS INITIAL AND
         EKPO-WEPOS IS INITIAL.
        CLEAR : IT_ZSIVIT-UMSON.
      ENDIF.
    ENDIF.
    IT_ZSIVIT-ZFNOCCMN = IT_ZSIVIT-MENGE_BL - IT_ZSIVIT-ZFCCTOT.
    APPEND  IT_ZSIVIT.
  ENDLOOP.

  ZTIV-ZFIVAMT = W_TOT_AMOUNT.
  ZTIV-ZFIVAMK = W_TOT_AMOUNT1.

ENDFORM.                    " P2000_BL_TO_CC_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_CG_TO_CC_DATA
*&---------------------------------------------------------------------*
FORM P2000_CG_TO_CC_DATA.

  MOVE : ZTBL-ZFBLNO       TO       ZTIV-ZFBLNO,
         ZTBL-ZFBLAMC      TO       ZTIV-ZFIVAMC,
         ZTBL-BUKRS        TO       ZTIV-BUKRS,
         ZTBL-LIFNR        TO       ZTIV-LIFNR,
         ZTBL-ZFEXRT       TO       ZTIV-ZFEXRT,
         ZTBL-FFACT        TO       ZTIV-FFACT,
         ZTBL-ZFPONC       TO       ZTIV-ZFPONC,
         ZTBL-ZFPOYN       TO       ZTIV-ZFPOYN,
         SY-DATUM          TO       ZTIV-ZFCCDT,
         'KRW'             TO       ZTIV-ZFKRW,
         SY-UNAME          TO       ZTIV-ERNAM,
         SY-DATUM          TO       ZTIV-CDAT,
         SY-UNAME          TO       ZTIV-UNAM,
         SY-DATUM          TO       ZTIV-UDAT.

  IF ZTIV-ZFIVAMC EQ 'KRW'.
    ZTIV-ZFEXRT = 1.
  ENDIF.

  W_TOT_AMOUNT = 0.      W_TOT_AMOUNT1 = 0.
  W_LINE       = 0.

*>> 2001.05.08 KSB MODIFY START
*  LOOP AT IT_ZSCGIT WHERE CGLOEKZ EQ SPACE
  LOOP AT IT_ZSCGIT WHERE CGLOEKZ EQ SPACE
                    AND   ZFBLNO  EQ ZTBL-ZFBLNO.
*>> 2001.05.08 KSB MODIFY END
    MOVE-CORRESPONDING  IT_ZSCGIT   TO    IT_ZSIVIT.
    MOVE : IT_ZSCGIT-MENGE_BL       TO    IT_ZSIVIT-MENGE_BL,
           ZTBL-ZFBLAMC             TO    IT_ZSIVIT-ZFIVAMC,
           'KRW'                    TO    IT_ZSIVIT-ZFKRW,
           IT_ZSCGIT-ZFCGIT         TO    IT_ZSIVIT-ZFIVDNO.

    IF NOT IT_ZSIVIT-BLOEKZ IS INITIAL.
      CONTINUE.
    ENDIF.

    IF NOT IT_ZSIVIT-LOEKZ IS INITIAL.
      CONTINUE.
    ENDIF.

    IT_ZSIVIT-ZFCCTOT = 0.
    IT_ZSIVIT-ZFGRTOT = 0.
    SELECT SUM( CCMENGE ) SUM( GRMENGE )
           INTO (IT_ZSIVIT-ZFCCTOT, IT_ZSIVIT-ZFGRTOT)
                          FROM ZTIVIT
                          WHERE ZFBLNO  EQ   IT_ZSIVIT-ZFBLNO
                          AND   ZFBLIT  EQ   IT_ZSIVIT-ZFBLIT
                          AND   ZFCGNO  EQ   IT_ZSIVIT-ZFCGNO
                          AND   ZFCGIT  EQ   IT_ZSIVIT-ZFCGIT.

    IF NOT IT_ZSIVIT-BLOEKZ IS INITIAL.
      CONTINUE.
    ENDIF.

    IF NOT IT_ZSIVIT-LOEKZ IS INITIAL.
      CONTINUE.
    ENDIF.
*>> Åë°ü¼ö·®Àº B/L
    IT_ZSIVIT-CCMENGE =  IT_ZSIVIT-MENGE_BL - IT_ZSIVIT-ZFCCTOT.
*>> ÀÔ°í¼ö·®Àº ÇÏ¿ª....
    IT_ZSIVIT-GRMENGE =  IT_ZSIVIT-CGMENGE  - IT_ZSIVIT-ZFGRTOT.

    IF IT_ZSIVIT-GRMENGE LT 0.
      IT_ZSIVIT-GRMENGE = 0.
    ENDIF.

    IF IT_ZSIVIT-CCMENGE GT 0.
      IT_ZSIVIT-ZFIVAMT = IT_ZSIVIT-CCMENGE *
                 ( IT_ZSIVIT-BPUMZ / IT_ZSIVIT-BPUMN ) *
                 ( IT_ZSIVIT-NETPR / IT_ZSIVIT-PEINH ).
*>> Ã³¸® ¿øÈ­ ±Ý¾× °è»ê...
      PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSIVIT-ZFIVAMT
                                              IT_ZSIVIT-ZFIVAMC
                                              IT_ZSIVIT-ZFIVAMK.
      IF ZTIV-FFACT IS INITIAL.
        ZTIV-FFACT = 1.
      ENDIF.
      IT_ZSIVIT-ZFIVAMK = ( ZTIV-ZFEXRT / ZTIV-FFACT )
                        * IT_ZSIVIT-ZFIVAMK.

*  SELECT SINGLE * FROM  TCURX
*         WHERE  CURRKEY     = 'KRW'.
*  IF SY-SUBRC NE 0.
*     TCURX-CURRDEC = 2.
*  ENDIF.

*  IF TCURX-CURRDEC NE 0.
      PERFORM SET_CURR_CONV_TO_INTERNAL USING
                                        IT_ZSIVIT-ZFIVAMK 'KRW'.
    ELSE.
      IT_ZSIVIT-CCMENGE = 0.
    ENDIF.
    ADD IT_ZSIVIT-ZFIVAMT  TO  W_TOT_AMOUNT.
    ADD IT_ZSIVIT-ZFIVAMK  TO  W_TOT_AMOUNT1.
*      ADD 10                 TO  W_LINE.
*      MOVE W_LINE      TO    IT_ZSIVIT-ZFIVDNO.       ">ÀÏ·Ã¹øÈ£.
    IT_ZSIVIT-UMSON  =  'X'.
*>>> 2000/04/10 KSB  ¼ö·®ÀÌ ZEROÀÎ °ÍÀº GR ´©¶ô.
    IF IT_ZSIVIT-GRMENGE LE 0.
      CLEAR : IT_ZSIVIT-UMSON.
    ENDIF.
**> ÀÚÀç¹øÈ£°¡ ¾ø´Â °ÍÀ» ÀÔ°í ´©¶ô.
*    IF IT_ZSIVIT-MATNR IS INITIAL.
*      CLEAR : IT_ZSIVIT-UMSON.
*    ENDIF.
*>> ÀÔ°í ¿©ºÎ Ã¼Å©(P/O»ó ¹«È¯Ã¼Å© )...
    CLEAR : IT_ZSIVIT-REPOS.
    IF NOT IT_ZSIVIT-EBELN IS INITIAL AND
       NOT IT_ZSIVIT-EBELP IS INITIAL.
      SELECT SINGLE * FROM EKPO
             WHERE  EBELN  EQ   IT_ZSIVIT-EBELN
             AND    EBELP  EQ   IT_ZSIVIT-EBELP.
      MOVE EKPO-REPOS  TO   IT_ZSIVIT-REPOS.
      IF EKPO-REPOS IS INITIAL AND  NOT EKPO-MATNR IS INITIAL.
        CLEAR : IT_ZSIVIT-UMSON.
      ENDIF.
    ELSE.
      CLEAR : IT_ZSIVIT-UMSON.
    ENDIF.

    IF IT_ZSIVIT-CGMENGE IS INITIAL.
      IT_ZSIVIT-ZFNOCGMN = 0.
    ELSE.
      IT_ZSIVIT-ZFNOCGMN  = IT_ZSIVIT-CGMENGE  - IT_ZSIVIT-ZFGRTOT.
    ENDIF.
    IT_ZSIVIT-ZFNOCCMN = IT_ZSIVIT-MENGE_BL - IT_ZSIVIT-ZFCCTOT.

    APPEND  IT_ZSIVIT.
  ENDLOOP.

  ZTIV-ZFIVAMT = W_TOT_AMOUNT.
  ZTIV-ZFIVAMK = W_TOT_AMOUNT1.

ENDFORM.                    " P2000_CG_TO_CC_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_MIRO_DATA_MAKE
*&---------------------------------------------------------------------*
FORM P3000_MIRO_DATA_MAKE USING    P_ZFCIVNO.

  DATA : L_BUKRS   LIKE   BKPF-BUKRS.

  PERFORM P3000_DOCUMENT_TYPE.


  WRITE ZTIV-ZFIVAMT CURRENCY ZTIV-ZFIVAMC TO TEMP_WRBTR.
*>> COMPANY CODE.
  GET PARAMETER ID 'BUK' FIELD L_BUKRS.
  IF L_BUKRS IS INITIAL.
    PERFORM P2000_DYNPRO USING 'X' 'SAPLACHD' '1000'.
    PERFORM P2000_DYNPRO USING :
        ' ' 'BKPF-BUKRS' ZTIV-BUKRS,           " Company Code.
        ' ' 'BDC_OKCODE' '/00'.                " ENTER
  ENDIF.

  READ TABLE  IT_ZSIVIT  INDEX  1.
  CLEAR : ZTREQHD, ZTBL.
  SELECT SINGLE * FROM  ZTREQHD
                  WHERE ZFREQNO EQ IT_ZSIVIT-ZFREQNO.
  SELECT SINGLE * FROM ZTBL
                  WHERE ZFBLNO  EQ IT_ZSIVIT-ZFBLNO.
* ±âº»µ¥ÀÌÅ¸.
  PERFORM P2000_DYNPRO USING 'X' 'SAPLMR1M' '6000'.
  PERFORM P2000_DYNPRO USING :
      ' ' 'RM08M-VORGANG' '1',               ">¹®¼­ Á¾·ù.
*      ' ' 'INVFO-BLDAT' ZTIV-ZFIVDDT,        ">Document Date(¼ÛÀåÀÏ)
*      ' ' 'INVFO-BUDAT' ZTIV-ZFIVPDT,        ">Posting Date(Àü±âÀÏ)
      ' ' 'INVFO-XBLNR' ZTREQHD-ZFOPNNO,     ">L/C No.
      ' ' 'INVFO-MWSKZ' 'V0',                ">TAX CODE
      ' ' 'INVFO-WRBTR' TEMP_WRBTR,          " ±Ý¾×.
      ' ' 'INVFO-WAERS' ZTIV-ZFIVAMC,        " Currency
      ' ' 'BDC_OKCODE' '=HEADER_PAY'.
* Áö±Þ.
  PERFORM P2000_DYNPRO USING 'X' 'SAPLMR1M' '6000'.
  PERFORM P2000_DYNPRO USING :
      ' ' 'INVFO-ZLSPR' 'B',                 ">Payment Block
*      ' ' 'INVFO-ZFBDT' ZTIV-ZFIVDDT,        ">±â»êÀÏ(=¼ÛÀåÀÏ)
      ' ' 'INVFO-ZTERM' ZTREQHD-ZTERM,       ">Payment Terms
      ' ' 'BDC_OKCODE' 'HEADER_FI'.          ">¼¼ºÎ»çÇ×.

*-----------------------------------------------------------------------
* ¼¼ºÎ»çÇ×.
  PERFORM P2000_DYNPRO USING 'X' 'SAPLMR1M' '6000'.
*>> ºñ°èÈ¹ ¿î¼Ûºñ¿ë.
  W_ZFIVAMT = ZTIV-ZFPKCHG + ZTIV-ZFHDCHG.
  IF W_ZFIVAMT > 0.
    WRITE W_ZFIVAMT CURRENCY ZTIV-ZFIVAMC TO TEMP_WRBTR.
    PERFORM P2000_DYNPRO USING :
            ' ' 'RM08R-BEZNK' TEMP_WRBTR.    " Unplanned Cost
  ENDIF.
*>> È¯À².
  IF ZTIV-ZFIVAMC NE 'KRW'.
    MOVE ZTIV-ZFEXRT       TO TEMP_KURSF.
    PERFORM P2000_DYNPRO USING :
           ' ' 'INVFO-KURSF' TEMP_KURSF.        " Exchange Rate
  ENDIF.
  PERFORM P2000_DYNPRO USING :
      ' ' 'INVFO-LIFRE' ZTIV-LIFNR,         " Vendor(BANK???)
      ' ' 'INVFO-BKTXT' ZTBL-ZFHBLNO,        " Reference Header Text
      ' ' 'INVFO-ZUONR' ZTIV-ZFIVNO,         " ÁöÁ¤.
      ' ' 'RM08M-EBELN' ZTREQHD-EBELN,       " Purchase Order
      ' ' 'INVFO-BLART' 'RE',                " ¼ÛÀå Á¾·ù.
      ' ' 'BDC_OKCODE' '/00'.                " ENTER
* Text, Reference,

  CLEAR W_LOOP_CNT.
  PERFORM P2000_DYNPRO USING 'X' 'SAPLMR1M' '6000'.
  SELECT *
    FROM EKPO
   WHERE EBELN = ZTREQHD-EBELN
     AND LOEKZ = '' "»èÁ¦Áö½ÃÀÚ.
   ORDER BY EBELP.
*    and elikz = ''. "³³Ç°¿Ï·áÁö½ÃÀÚ.
    ADD 1 TO W_LOOP_CNT.

    SELECT SINGLE *
      FROM ZTIVIT
     WHERE ZFIVNO  = ZTIV-ZFIVNO
       AND ZFIVDNO = EKPO-EBELP.

    IF SY-SUBRC = 0.
      PERFORM P2000_DYNPRO USING :
          ' ' 'RM08M-SKIP_TO' W_LOOP_CNT,          " Skip-To
          ' ' 'BDC_OKCODE' '=POS'.               " Position
      PERFORM P2000_DYNPRO USING 'X' 'SAPLMR1M' '6000'.
* ±Ý¾×.
      CONCATENATE 'DRSEG-WRBTR' '(1)' INTO TEMP_FNAM.
      WRITE ZTIVIT-ZFIVAMT CURRENCY ZTIVIT-ZFIVAMC TO TEMP_WRBTR.
      PERFORM P2000_DYNPRO USING ' '    TEMP_FNAM TEMP_WRBTR.
* ¼ö·®.
      CONCATENATE 'DRSEG-MENGE' '(1)' INTO TEMP_FNAM.
      WRITE ZTCIVIT-ZFPRQN UNIT ZTIVIT-MEINS TO TEMP_MENGE.
      PERFORM P2000_DYNPRO USING ' '    TEMP_FNAM TEMP_MENGE.
* ¼±ÅÃÁö½ÃÀÚ.
      CONCATENATE 'DRSEG-OK' '(1)'   INTO TEMP_FNAM.
      PERFORM P2000_DYNPRO USING ' '   TEMP_FNAM 'X'.
    ENDIF.
  ENDSELECT.
  PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'BU'.


ENDFORM.                    " P3000_MIRO_DATA_MAKE
*&---------------------------------------------------------------------*
*&      Form  P3000_DOCUMENT_TYPE
*&---------------------------------------------------------------------*
FORM P3000_DOCUMENT_TYPE.

*-----------------------------------------------------------------------
* CJ.
  MOVE 'RE' TO TEMP_BLART.
  EXIT.
*-----------------------------------------------------------------------
*  CLEAR TEMP_BLART.
*  IF ZTREQHD-ZFJEWGB NE '1'.  " Å¸ÀÎÀÚ±Ý.
*    MOVE 'RF' TO TEMP_BLART.
*    EXIT.
*  ENDIF.
*  IF ZTREQHD-ZTERM = ZTIMIMG11-ZTERM3  OR  " »çÈÄ¼Û±Ý.
*     ZTREQHD-ZFREQTY = 'DA'  OR  " D/A
*     ZTREQHD-ZFREQTY = 'DP'.     " D/P
*     MOVE 'RF' TO TEMP_BLART.
*     EXIT.
*  ENDIF.
*  IF ZTREQHD-ZTERM = 'LC' . " At Sight
*    MOVE 'RF' TO TEMP_BLART.
*    EXIT.
*  ENDIF.
*  MOVE 'RH' TO TEMP_BLART.

ENDFORM.                    " P3000_DOCUMENT_TYPE
*&---------------------------------------------------------------------*
*&      Form  P3000_BAPI_CALL
*&---------------------------------------------------------------------*
*       BAPI LIV ..
*----------------------------------------------------------------------*
FORM P3000_BAPI_CALL USING    P_ZFCIVRN
                              P_INVOICE
                              P_CREDITMENO
                              P_BLDAT
                              P_BUDAT
                              P_ZFBDT.

  DATA : BEGIN OF IT_EBELN  OCCURS 0.
  INCLUDE  STRUCTURE EKKO.
  DATA : END   OF IT_EBELN.

*>> È¯À²°íÁ¤ Áö½ÃÀÚ ¹× È¯À² °íÁ¤ PROCESS.....
  IF P_INVOICE          EQ 'X'  AND ZTIMIMG00-ZFEXFIX  EQ 'X'  AND
     ZTIMIMG00-ZFEXMTD  EQ 'I'  AND ZTCIVHD-ZFREQTY    NE 'LO' AND
     ZTCIVHD-ZFREQTY    EQ 'PU'.

    REFRESH : IT_EBELN, RETURN.
    CLEAR : W_COUNT.
    LOOP AT IT_ZSCIVIT.
      CLEAR : IT_EBELN.
      IF NOT IT_ZSCIVIT-EBELN IS INITIAL.
        MOVE : IT_ZSCIVIT-EBELN TO IT_EBELN-EBELN.
        COLLECT IT_EBELN.
        ADD 1 TO W_COUNT.
      ENDIF.
    ENDLOOP.
    IF W_COUNT GT 0.
      LOOP AT IT_EBELN.
        W_TABIX = SY-TABIX.
        MOVE : 'X'             TO  IT_EBELN-KUFIX,
               ZTCIVHD-ZFEXRT  TO  IT_EBELN-WKURS.
        MODIFY IT_EBELN INDEX W_TABIX.
      ENDLOOP.
*>> P/O °íÁ¤È¯À².
      CALL FUNCTION 'ZIM_BAPI_PO_EXRT_CHANGE'
           TABLES
                IT_EBELN   = IT_EBELN
                RETURN     = RETURN
           EXCEPTIONS
                POST_ERROR = 4.
      IF SY-SUBRC NE 0.
        READ TABLE RETURN INDEX 1.
        IF SY-SUBRC NE 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          MESSAGE ID RETURN-ID TYPE   RETURN-TYPE
                               NUMBER RETURN-NUMBER
                      WITH   RETURN-MESSAGE_V1  RETURN-MESSAGE_V2
                             RETURN-MESSAGE_V3  RETURN-MESSAGE_V4.
        ENDIF.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

*>> INVOICE VERIFICATION FUNCTION....
  CALL FUNCTION 'ZIM_BAPI_INVOICE_CREATE'
       EXPORTING
            P_ZFCIVRN        = P_ZFCIVRN
            P_CHG_MODE       = 'X'
            I_INVOICE        = P_INVOICE
            I_CREDITMEMO     = P_CREDITMENO
            P_BLDAT          = P_BLDAT
            P_BUDAT          = P_BUDAT
            P_ZFBDT          = P_ZFBDT
            P_MAX_ITEM       = W_MAX_ITEMS
       IMPORTING
            INVOICEDOCNUMBER = INVOICEDOCNUMBER
            FISCALYEAR       = FISCALYEAR
       TABLES
            RETURN           = RETURN
       EXCEPTIONS
            OTHERS           = 4.

  IF SY-SUBRC NE 0.
    READ TABLE RETURN INDEX 1.
    IF SY-SUBRC NE 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      PERFORM  P2000_SET_UNLOCK.
      LEAVE TO TRANSACTION 'ZIM37' AND SKIP FIRST SCREEN.
    ELSE.
      MESSAGE ID RETURN-ID TYPE 'S'         NUMBER RETURN-NUMBER
*     MESSAGE ID RETURN-ID TYPE RETURN-TYPE NUMBER RETURN-NUMBER
                 WITH   RETURN-MESSAGE_V1  RETURN-MESSAGE_V2
                        RETURN-MESSAGE_V3  RETURN-MESSAGE_V4.
      PERFORM  P2000_SET_UNLOCK.
      LEAVE TO TRANSACTION 'ZIM37' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_BAPI_CALL
*&---------------------------------------------------------------------*
*&      Form  P3000_MRHR_DATA_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTIV_ZFIVNO  text
*      -->P_1499   text
*----------------------------------------------------------------------*
FORM P3000_MRHR_DATA_MAKE USING    P_ZTIV_ZFIVNO
                                   VALUE(P_1499).

ENDFORM.                    " P3000_MRHR_DATA_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_CON_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_CON_DISPLAY USING    P_ZFHBLNO
                                P_ZFBLNO.
*  CALL TRANSACTION 'ZIMR18' AND SKIP  FIRST SCREEN.
  SUBMIT ZRIMCNIO WITH S_BLNO EQ P_ZFBLNO SIGN 'I'
         AND RETURN.

ENDFORM.                    " P2000_CON_DISPLAY
*&---------------------------------------------------------------------*
*&      Form   P2000_MOVE_BL_DATA_INOU_INR.
*&---------------------------------------------------------------------*
FORM P2000_MOVE_BL_DATA_INOU_INR.

  CLEAR ZTBL.
  SELECT SINGLE *
         FROM ZTBL
        WHERE ZFBLNO = ZSREQHD-ZFBLNO.
  MOVE-CORRESPONDING ZTBL TO ZTBLINR_TMP.
  CLEAR: ZTBLINR_TMP-ZFTRCK.
  IF SY-SUBRC EQ 0.
    MOVE 'X'           TO  ZTBLINR_TMP-ZFMCYN.
    MOVE: '20'         TO  ZTBLINR_TMP-ZFINTY,
          ZTBL-ZFNEWT  TO  ZTBLINR_TMP-ZFTOWT,
          ZTBL-ZFFORD  TO  ZTBLINR_TMP-ZFFORD,
          ZTBL-ZFNEWTM TO  ZTBLINR_TMP-ZFTOWTM,
          ZTBL-ZFRETA  TO  ZTBLINR_TMP-ZFETA.
  ELSE.
    MOVE ZSREQHD-ZFHBLNO TO  ZTBLINR_TMP-ZFHBLNO.
  ENDIF.
  MOVE:  SY-DATUM TO ZTBLINR_TMP-ZFINDT.
*        'M3'  TO ZTBLINR_TMP-ZFTOVLM,
*        'TON' TO ZTBLINR_TMP-ZFTOWTM,
*        'GT'  TO ZTBLINR_TMP-ZFPKCNM.

  W_TOWTM = ZTBLINR_TMP-ZFTOWTM.
  W_PKCNM = ZTBLINR_TMP-ZFPKCNM.

  CLEAR: ZTBLINR_TMP-ERNAM,ZTBLINR_TMP-CDAT,
         ZTBLINR_TMP-UNAM, ZTBLINR_TMP-UDAT.
  REFRESH IT_ZSBLIT.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLIT
       FROM ZTBLIT
       WHERE ZFBLNO = W_BLNO.

ENDFORM.                    " P2000_MOVE_BL_DATA_INOU_INR
*&---------------------------------------------------------------------*
*&      Form  P2000_CHECK_ZTIMIMG00
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  P2000_CHECK_ZTBLINR_ZTBLIOU
*&---------------------------------------------------------------------*
FORM P2000_CHECK_ZTBLINR_ZTBLIOU.

  SELECT MAX( ZFBTSEQ ) INTO ZSREQHD-ZFBTSEQ
       FROM  ZTBLINR_TMP
       WHERE ZFBLNO = ZSREQHD-ZFBLNO.

  CLEAR ZTBLINR.
  SELECT SINGLE *
      FROM ZTBLINR
      WHERE ZFBLNO  = ZSREQHD-ZFBLNO
        AND ZFBTSEQ = ZSREQHD-ZFBTSEQ.
  IF SY-SUBRC EQ 0.
    CLEAR ZTBLOUR.
    SELECT SINGLE *
           FROM ZTBLOUR
          WHERE ZFBLNO  = ZTBLINR-ZFBLNO
            AND ZFBTSEQ = ZTBLINR-ZFBTSEQ.
    IF ZTBLOUR-ZFDOCST = 'O'.
      MESSAGE W695 WITH ZTBLINR-ZFBTSEQ.
    ELSE.
      MESSAGE E977 WITH 'Carry-in declaration already exits!!'.

    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_CHECK_ZTBLINR_ZTBLIOU
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA_ZTBL
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA_ZTBL.

  CLEAR : ZTIMIMG00, ZTBL.
  SELECT SINGLE * FROM ZTIMIMG00.

  SELECT SINGLE *
         FROM ZTBL
         WHERE ZFBLNO = ZSREQHD-ZFBLNO.
  IF NOT ( ZTBL-ZFRPTTY  EQ 'N' OR  ZTBL-ZFRPTTY  EQ 'B' OR
           ZTBL-ZFRPTTY  EQ 'W' ).
    MESSAGE E977 WITH 'It is not a case of bonded transportation!!!'.
  ENDIF.
  IF ZTIMIMG00-BLSTYN EQ 'X' AND ZTIMIMG00-ZFINOU IS INITIAL.
    IF ZTBL-ZFBLST EQ '1'.
      MESSAGE  W977 WITH 'Shipping Doc is not sended.'.
    ENDIF.
  ENDIF.
   *ZTBL = ZTBL.

*±¸¸ÅÃ³¸í
  PERFORM  P1000_GET_VENDOR   USING  ZTBL-LIFNR
                           CHANGING  LFA1-NAME1.

ENDFORM.                    " P1000_READ_DATA_ZTBL
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_BLNO
*&---------------------------------------------------------------------*
FORM P2000_GET_BLNO.

  SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
          FROM  ZTBL
         WHERE  ZFREBELN = ZSREQHD-EBELN.
    EXIT.
  ENDSELECT.

ENDFORM.                    " P2000_GET_BLNO
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_ITEM_SELECT1
*&---------------------------------------------------------------------*
FORM P2000_BL_DOC_ITEM_SELECT1.

  W_BLNO     = ZSREQHD-ZFBLNO.
  W_EBELN    = ZSREQHD-EBELN.
  W_HBLNO    = ZSREQHD-ZFHBLNO.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
*           FROM   ZTBL
*           WHERE  ZFREBELN EQ ZSREQHD-EBELN
*           ORDER  BY ZFBLNO.
  SELECT *
      FROM ZTBL
     WHERE ZFREBELN EQ ZSREQHD-EBELN
           ORDER  BY ZFBLNO.
    MOVE: ZTBL-ZFREBELN TO IT_ZSREQHD-EBELN,
          ZTBL-ZFBLNO   TO IT_ZSREQHD-ZFBLNO,
          ZTBL-ZFSHNO   TO IT_ZSREQHD-ZFSHNO,
          ZTBL-ZFHBLNO  TO IT_ZSREQHD-ZFHBLNO,
          ZTBL-LIFNR    TO IT_ZSREQHD-LLIEF,
          ZTBL-ZFETA    TO IT_ZSREQHD-ZFETA,
          ZTBL-ZFBLST   TO IT_ZSREQHD-ZFBLST.
    APPEND IT_ZSREQHD.
  ENDSELECT.

  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'BLLST'.                 ">B/L Á¶È¸.
  CALL SCREEN 0114 STARTING AT  07 3
                   ENDING   AT  110 20.
ENDFORM.                    "P2000_BL_DOC_ITEM_SELECT1
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
      MOVE : IT_ZSREQHD-ZFBLNO   TO IT_SELECTED-ZFBLNO,
             IT_ZSREQHD-ZFTBLNO  TO IT_SELECTED-ZFTBLNO,
             IT_ZSREQHD-EBELN    TO IT_SELECTED-EBELN.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.
  IF W_SELECTED_LINES = 0.
    MESSAGE E951.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_PO
*&---------------------------------------------------------------------*
FORM P2000_SHOW_PO USING    P_EBLNR.

  SET PARAMETER ID 'BES' FIELD P_EBLNR.
  CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_PO
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFBLNO.

  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  CALL TRANSACTION 'ZIM23'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_BL
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_ITEM_SELECT2
*&---------------------------------------------------------------------*
FORM P2000_BL_DOC_ITEM_SELECT2.

  W_BLNO     = ZSREQHD-ZFBLNO.
  W_EBELN    = ZSREQHD-EBELN.
  W_HBLNO    = ZSREQHD-ZFHBLNO.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT *
      FROM ZTBL
     WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO
           ORDER  BY ZFBLNO.
    MOVE: ZTBL-ZFREBELN TO IT_ZSREQHD-EBELN,
          ZTBL-ZFBLNO   TO IT_ZSREQHD-ZFBLNO,
          ZTBL-ZFSHNO   TO IT_ZSREQHD-ZFSHNO,
          ZTBL-ZFHBLNO  TO IT_ZSREQHD-ZFHBLNO,
          ZTBL-LIFNR    TO IT_ZSREQHD-LLIEF,
          ZTBL-ZFETA    TO IT_ZSREQHD-ZFETA,
          ZTBL-ZFBLST   TO IT_ZSREQHD-ZFBLST.
    APPEND IT_ZSREQHD.
  ENDSELECT.

  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'BLLST'.                 ">B/L Á¶È¸.
  CALL SCREEN 0114 STARTING AT  07 3
                   ENDING   AT  110 20.

ENDFORM.                    " P2000_BL_DOC_ITEM_SELECT2
*&---------------------------------------------------------------------*
*&      Form  P200_LEAV_SCREEN
*&---------------------------------------------------------------------*
FORM P200_LEAV_SCREEN.




ENDFORM.                    " P200_LEAV_SCREEN
*&---------------------------------------------------------------------*
*&      Form  P1000_SELECT_DATA_ZTBLINR
*&---------------------------------------------------------------------*
FORM P1000_SELECT_DATA_ZTBLINR.

  CASE SY-TCODE.
    WHEN 'ZIMI6'. "  ¼öÀÔ½Ã½ºÅÛ ¹ÝÀÔ½Å°í»ý¼º.
      PERFORM P2000_CHECK_ZTBLINR_ZTBLIOU.
      PERFORM P1000_READ_DATA_ZTBL.
    WHEN 'ZIMI7' OR 'ZIMI8'. " ¼öÀÔ½Ã½ºÅÛ ¹ÝÀÔ½Å°í º¯°æ/Á¶È¸.
      PERFORM P1000_READ_DATA_ZTBLINR.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P1000_SELECT_DATA_ZTBLINR
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA_ZTBLINR
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA_ZTBLINR.

  CLEAR: ZTBLINR_TMP.
  SELECT SINGLE *
         FROM  ZTBLINR_TMP
         WHERE ZFTBLNO = W_TBLNO.
   *ZTBLINR_TMP = ZTBLINR_TMP.
  REFRESH IT_ZSBLIT.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLIT
       FROM ZTBLIT
       WHERE ZFBLNO = W_BLNO.

  W_TOWTM = ZTBLINR_TMP-ZFTOWTM.
  W_PKCNM = ZTBLINR_TMP-ZFPKCNM.

*±¸¸ÅÃ³¸í
  PERFORM  P1000_GET_VENDOR   USING  ZTBLINR_TMP-LIFNR
                           CHANGING  LFA1-NAME1.

ENDFORM.                    " P1000_READ_DATA_ZTBLINR
*&---------------------------------------------------------------------*
*&      Form  P2000_NOTUSE_IMG00_ZFINOU
*&---------------------------------------------------------------------*
FORM P2000_NOTUSE_IMG00_ZFINOU.

  W_ERR_CHK = 'N'.
  CLEAR: W_BLNO,W_EBELN,W_HBLNO.
  W_COUNT = 1.
* ÀÔ·Â°ª CHECK.
  IF ZSREQHD-EBELN   IS INITIAL AND
     ZSREQHD-ZFHBLNO IS INITIAL AND
     ZSREQHD-ZFBLNO IS INITIAL.      " °ü¸®¹øÈ£°¡ ÀÔ·ÂÇÏÁö ¾ÊÀ» °æ?
    MESSAGE E193.
  ENDIF.
*>> º¯°æ. Á¶È¸.
  CASE  SY-TCODE .
    WHEN 'ZIMI7'.
      PERFORM P2000_SET_BLINR_TMP_LOCK    USING    'L'.
      PERFORM P2000_READ_DATA_ZTBLINR_TMP.
    WHEN 'ZIMI8'.
      PERFORM P2000_READ_DATA_ZTBLINR_TMP.
    WHEN 'ZIMI6'.
      PERFORM P2000_READ_DATA_ZTBL.
  ENDCASE.

ENDFORM.                    "P2000_NOTUSE_IMG00_ZFINOU
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBLINR_TMP_MODIFY
*&---------------------------------------------------------------------*
FORM P3000_ZTBLINR_TMP_MODIFY.
*-----------------------------------------------------------------------
* Data °ËÁõ Ãß°¡ÇÒ ºÎ?
*-----------------------------------------------------------------------
  IF W_OK_CODE NE 'DELE'.
    PERFORM   P2000_BLINR_TMP_CHECK.
  ENDIF.
*>> ¹ÝÀÔ°ü¸®¹øÈ£ ÀÚµ¿»ý¼º.
  IF W_STATUS EQ C_REQ_C.
    PERFORM P2000_GET_NUMBER_NEXT USING 'BI' ZTBLINR_TMP-ZFTBLNO.
  ENDIF.
  CALL FUNCTION 'ZIM_BLINRTMP_DOC_MODIFY'
       EXPORTING
            W_OK_CODE         = W_OK_CODE
            ZFTBLNO           = ZTBLINR_TMP-ZFTBLNO
            ZFSTATUS          = W_STATUS
            W_ZTBLINR_TMP     = ZTBLINR_TMP
            W_ZTBLINR_TMP_OLD = *ZTBLINR_TMP
       EXCEPTIONS
            ERROR_UPDATE      = 4
            NOT_MODIFY        = 8.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    IF W_OK_CODE = 'DELE'.
      MESSAGE S977 WITH
                   'Deleted carry-in declaration data successfully.'.
    ELSE.
      MESSAGE S977 WITH
                   'Saved carry-in declaration data successfully.'.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    IF W_OK_CODE = 'DELE'.
      MESSAGE S977 WITH
              'Occured error when delete carry-in declaration data.'.
    ELSE.
      MESSAGE S977 WITH
              'Occured error when save carry-in declaration data.'.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_ZTBLINR_TMP_MODIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_BLINR_TMP_CHECK
*&---------------------------------------------------------------------*
FORM P2000_BLINR_TMP_CHECK.


ENDFORM.                    " P2000_BLINR_TMP_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_ZTBLINR_TEM_INIT_SCR
*&---------------------------------------------------------------------*
FORM P2000_SET_ZTBLINR_TEM_INIT_SCR.

  CASE SY-TCODE.
    WHEN 'ZIMI6'. " CREATE.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " »èÁ¦.
      MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " »ý¼º.
    WHEN 'ZIMI7'.
      MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL. "  º¯°æ.
    WHEN 'ZIMI8'.
      MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Á¶È¸.
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " ÀúÀå.
  ENDCASE.

ENDFORM.                    " P2000_SET_ZTBLINR_TEM_INIT_SCR
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_BLINR_TMP_LOCK
*&---------------------------------------------------------------------*
FORM P2000_SET_BLINR_TMP_LOCK USING   PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLINRTMP'
         EXPORTING
              ZFTBLNO = ZTBLINR_TMP-ZFTBLNO
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Carry-in Document'
                             ZTBLINR_TMP-ZFTBLNO
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLINRTMP'
         EXPORTING
              ZFTBLNO = ZTBLINR_TMP-ZFTBLNO.
  ENDIF.

ENDFORM.                    " P2000_SET_BLINR_TMP_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_PAYORD_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_PAYORD_DISPLAY.

  SET PARAMETER ID 'ZPCIVRN'  FIELD  ZTCIVHD-ZFCIVRN.
  CALL TRANSACTION 'ZIM35P'. "AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_PAYORD_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_PAYORD_DOC_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAYORD_DOC_CHECK.
  ZTCIVHD-ZFEDICK = 'X'.
**----------------------------------------------------------------------
** 2000/03/10 Ç°¸ñ Ã¼?
**----------------------------------------------------------------------
*  LOOP AT IT_ZSREQIT.
*    PERFORM   P2000_REQ_ITEM_CHECK   TABLES  IT_ZSREQIT
*                                     USING   'E'.
*  ENDLOOP.
*-----------------------------------------------------------------------
* ÃÖÁ¾ °³¼³±Ý¾× ÀÚµ¿ °»?
*-----------------------------------------------------------------------
*  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

  ZTCIVHD-ZFEDICK = 'O'.
* ÀÔ±Ý°ü·Ã ¼­·ù¹ø?
  DESCRIBE TABLE IT_ZSTTSG5 LINES W_LINE.
  IF W_LINE LE 0.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Doc No of related deposit'.  EXIT.
  ENDIF.
* ½ÅÃ»ÀÏ?
  IF ZTTTHD-ZFAPPDT IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Application date'.  EXIT.
  ENDIF.
* Áö±ÞÁö¼­¼­ ¿ë?
  IF ZTTTHD-ZFBUSFUN IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Use of pay order'. EXIT.
  ELSE.
    IF ZTTTHD-ZFBUSFUN EQ '2AJ' AND ZTTTHD-ZFCOMMTY IS INITIAL.
      ZTCIVHD-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Type of payment commission'.   EXIT.
    ELSEIF ZTTTHD-ZFBUSFUN NE '2AJ' AND NOT ZTTTHD-ZFCOMMTY IS INITIAL.
      ZTCIVHD-ZFEDICK = 'X'.
      MESSAGE I215.   EXIT.
    ENDIF.
  ENDIF.
* ºÎ°¡¼ö¼ö·á ´ã´ç?
  IF ZTTTHD-ZFCFRG IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Payer of additional commission'. EXIT.
  ENDIF.
* Áö±ÞÀÇ·Ú?
  IF ZTTTHD-ZFAPPNM IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Payment requester'. EXIT.
  ENDIF.
  ASSIGN ZTTTHD-ZFAPPNM         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFAPPAD1        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFAPPAD2        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFAPPAD3        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFTELNO         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
* ¼öÀÍ?
  IF ZTTTHD-ZFBENI1 IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Beneficiary name'. EXIT.
  ENDIF.
  ASSIGN ZTTTHD-ZFBENI1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFBENI2         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFBENI3         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
* Áö±ÞÀÇ·ÚÀÎ Àº?
  IF ZTTTHD-ZFOBNM IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Payment requester bank'. EXIT.
  ENDIF.
  IF ZTTTHD-ZFOBAK IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Payment request Account No'. EXIT.
  ENDIF.
  ASSIGN ZTTTHD-ZFOBNM          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFOBAK          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFOBBR          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFOPBNCD        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.

* ¼öÀÍÀÚ Àº?
  IF ZTTTHD-ZFBENM IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Beneficiary bank'. EXIT.
  ENDIF.
  IF ZTTTHD-ZFOBAK1 IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Beneficiary account No'. EXIT.
  ENDIF.
  ASSIGN ZTTTHD-ZFOBNM          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFOBAK1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFBEBR          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFBENCD         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
* ÀüÀÚ¼­?
  IF ZTTTHD-ZFELENM IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Electronic signature firm name'. EXIT.
  ENDIF.
  IF ZTTTHD-ZFELEID IS INITIAL.
    ZTCIVHD-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Electronic signature'. EXIT.
  ENDIF.
  ASSIGN ZTTTHD-ZFELENM         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFREPRE         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFELEID         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
* ¼Û±Ý¾Ö?
  ASSIGN ZTTTHD-ZFSEND1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFSEND2         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFSEND3         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFSEND4         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFSEND5         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
* ±âÅ¸Á¤?
  ASSIGN ZTTTHD-ZFETC1          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFETC2          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFETC3          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFETC4          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFETC5          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.
* ±âÅ¸ ¹ø?
  ASSIGN ZTTTHD-ZFETCNO         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTCIVHD-ZFEDICK NE 'X'.

ENDFORM.                    " P2000_PAYORD_DOC_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_PAYORD_FIELD_MOVE
*&---------------------------------------------------------------------*
FORM P2000_PAYORD_FIELD_MOVE USING P_STATUS.
  IF P_STATUS = 'C' AND ZTCIVHD-ZFCIVRN EQ SPACE.
    EXIT.
  ENDIF.
  CASE P_STATUS.
    WHEN 'C'.               " »ý?
* Áö±ÞÀÇ·ÚÀÎ.
      SELECT SINGLE * FROM ZTIMIMGTX.
      MOVE : ZTIMIMGTX-ZFAPPNM  TO ZTTTHD-ZFAPPNM,  "»ó?
             ZTIMIMGTX-ZFAPPAD1 TO ZTTTHD-ZFAPPAD1, "ÁÖ¼Ò.
             ZTIMIMGTX-ZFAPPAD2 TO ZTTTHD-ZFAPPAD2, "ÁÖ¼Ò.
             ZTIMIMGTX-ZFTELNO  TO ZTTTHD-ZFTELNO.  "ÀüÈ­¹øÈ£.
*
      ZTTTHD-ZFELEID = ZTIMIMGTX-ZFELEID. "ÀüÀÚ¼­?
      ZTTTHD-ZFREPRE = ZTIMIMGTX-ZFREPREL. "´ëÇ¥ÀÚ¸í.
      ZTTTHD-ZFELENM = ZTIMIMGTX-ZFAPPNML. "»óÈ£.

* Benificiary Text Select
      PERFORM   P1000_GET_LFA1_SELECT     USING    ZTCIVHD-ZFMAVN
                                          CHANGING W_LFA1.

      MOVE : ZTCIVHD-ZFMAVN      TO ZTTTHD-ZFBENI,   " Beni CODE.
             W_LFA1-NAME1        TO ZTTTHD-ZFBENI1,  " PULL NAME
             W_LFA1-STRAS        TO ZTTTHD-ZFBENI2,  " Benificiary
             W_LFA1-ORT01        TO ZTTTHD-ZFBENI3.  " Benificiary
* Áö±ÞÀÇ·ÚÀÎ ÀºÇà.
      PERFORM   P1000_GET_LFA1_SELECT     USING    ZTCIVHD-ZFOPBN
                                          CHANGING W_LFA1.

      MOVE : ZTCIVHD-ZFOPBN      TO ZTTTHD-ZFOPBN,  " °³¼³ÀºÇàÄÚµå.
             W_LFA1-NAME1        TO ZTTTHD-ZFOBNM,  " PULL NAME
             W_LFA1-KRAUS        TO ZTTTHD-ZFOPBNCD." EDI

* ¼öÀÍÀÚ ÀºÇà.
      SELECT SINGLE * FROM LFBK
            WHERE LIFNR = ZTCIVHD-ZFMAVN.
      MOVE : LFBK-BANKN          TO ZTTTHD-ZFOBAK1. " °èÁÂ¹øÈ£.

      SELECT SINGLE * FROM BNKA
            WHERE BANKS = LFBK-BANKS
              AND BANKL = LFBK-BANKL.
      MOVE : BNKA-BANKA         TO ZTTTHD-ZFBENM,
             BNKA-BRNCH         TO ZTTTHD-ZFBEBR.   "ÁöÁ¡¸í.

*¼Û±Ý¹æ¹ý.( Default: Àü½Å¼Û±Ý )
      MOVE : '3'                 TO ZTTTHD-ZFSENDTY.


      IF ZTREQHD-ZFBACD EQ 'A'.       " »çÀü/»çÈÄ ±¸ºÐ.
        ZTTTHD-ZFSEND1 = 'Àü½Å¼Û±Ý (T/T) ( »çÈÄ¼Û±Ý )'.
      ELSEIF ZTREQHD-ZFBACD EQ 'B'.
        ZTTTHD-ZFSEND1 = 'Àü½Å¼Û±Ý (T/T) ( »çÀü¼Û±Ý )'.
      ENDIF.
      ZTTTHD-ZFBUSFUN = '2AM'.                    " Áö±ÞÁö½Ã¼­ ¿ë?

      MOVE: ZTCIVHD-ZFCIVRN     TO ZTTTHD-ZFCIVRN,     " ¹°´ë°ü¸®¹ø?
            '9'                 TO ZTTTHD-ZFEDFN,      " ¹®¼­±â´É.
            '14'                TO ZTTTHD-ZFCFRG.      " ¼ö¼ö·á.
      MOVE : ZTCIVHD-ZFIVAMT    TO ZTTTHD-ZFAMT,
             ZTCIVHD-ZFIVAMC    TO ZTTTHD-WAERS.
      INSERT ZTTTHD.

* ÀÔ±Ý°ü·Ã ¼­·ù¹®¼­.
      REFRESH : IT_ZSTTSG5. CLEAR : IT_ZSTTSG5.
      CLEAR : W_MAX_SEQ.

*L/C °³¼³ ½ÂÀÎ¹øÈ£¸¦ ³ÖÀ»¶§.
*     SELECT DISTINCT ZFOPNNO INTO IT_ZSTTSG5-ZFDOCNO "LC°³¼³¹øÈ£.
*       FROM ZTCIVIT AS A INNER JOIN ZTREQHD AS B
*       ON   A~ZFREQNO = B~ZFREQNO
*       WHERE ZFCIVRN = ZTCIVHD-ZFCIVRN.

* ±¸¸Å¹®¼­ ¹øÈ£¸¦ ³ÖÀ»¶§.
      SELECT DISTINCT EBELN   INTO IT_ZSTTSG5-ZFDOCNO "LC°³¼³¹øÈ£.
        FROM ZTCIVIT
        WHERE ZFCIVRN = ZTCIVHD-ZFCIVRN.

        MOVE: ZTCIVHD-ZFCIVRN     TO IT_ZSTTSG5-ZFCIVRN. " ¹°´ë°ü¸®¹ø?
        W_MAX_SEQ = W_MAX_SEQ + 10.
        IT_ZSTTSG5-ZFLSG5    =     W_MAX_SEQ.               "'00010'.
        IT_ZSTTSG5-ZFDOCCD   =     '105'.           " ÄÚµå(ÁÖ¹®¼­).
        APPEND IT_ZSTTSG5.
*       INSERT ZTTTSG5 FROM TABLE IT_ZSTTSG5.
        INSERT ZTTTSG5 FROM IT_ZSTTSG5.
        CLEAR IT_ZSTTSG5.
      ENDSELECT.

    WHEN 'D'. "»èÁ¦.
      DELETE FROM ZTTTHD  WHERE ZFCIVRN  EQ ZTCIVHD-ZFCIVRN.
      DELETE FROM ZTTTSG5 WHERE ZFCIVRN  EQ ZTCIVHD-ZFCIVRN.

    WHEN OTHERS.            " º¯°æ.
      IF P_STATUS <> 'M'. "ÀÎº¸ÀÌ½º¸¦ ¼öÁ¤ÇÒ °æ¿ì.
        MOVE : ZTCIVHD-ZFIVAMT    TO ZTTTHD-ZFAMT,
               ZTCIVHD-ZFIVAMC    TO ZTTTHD-WAERS.
      ENDIF.
      MODIFY ZTTTHD.
      IF SY-SUBRC NE  0.   RAISE ERROR_UPDATE.   ENDIF.

      DELETE FROM ZTTTSG5 WHERE ZFCIVRN = ZTTTHD-ZFCIVRN.
      LOOP AT IT_ZSTTSG5.
        MOVE-CORRESPONDING IT_ZSTTSG5   TO ZTTTSG5.
        MOVE : ZTTTHD-ZFCIVRN           TO ZTTTSG5-ZFCIVRN,
               SY-MANDT                 TO ZTTTSG5-MANDT.
        INSERT  ZTTTSG5.
        IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " P2000_PAYORD_FIELD_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSTTSG5_UPDATE
*&---------------------------------------------------------------------*
FORM P2000_IT_ZSTTSG5_UPDATE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSTTSG5 WHERE ZFLSG5 EQ SPACE.

    DELETE IT_ZSTTSG5 INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSTTSG5.
    IT_ZSTTSG5-ZFLSG5 = SY-TABIX * 10.
    MODIFY IT_ZSTTSG5 INDEX SY-TABIX.
  ENDLOOP.
ENDFORM.                    " P2000_IT_ZSTTSG5_UPDATE
*&---------------------------------------------------------------------*
*&      Form  GET_ORIJIN_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_ORIJIN_NAME USING    P_CODE
                     CHANGING P_NAME.
  DATA: L_TEXT(15).

  CLEAR : T005T.
  SELECT SINGLE * FROM T005T WHERE   SPRAS EQ 'E'
                             AND     LAND1 EQ P_CODE.

*  TRANSLATE T005T-LANDX TO UPPER CASE.
  MOVE : T005T-LANDX     TO   L_TEXT.   " ±¹°¡?
  TRANSLATE      L_TEXT  TO   UPPER CASE.
  L_TEXT = P_NAME.

ENDFORM.                    " GET_ORIJIN_NAME
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSINSSG2_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IT_ZSINSSG2_UPDATE.

* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSINSBSG2   WHERE ZFLSG2  EQ SPACE.
    DELETE IT_ZSINSBSG2  INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSINSBSG2.
    IT_ZSINSBSG2-ZFLSG2 = SY-TABIX * 10.
    MODIFY IT_ZSINSBSG2  INDEX SY-TABIX.
  ENDLOOP.

  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR
     SY-TCODE EQ 'ZIM47' OR SY-TCODE EQ 'ZIM48'.

    DESCRIBE TABLE IT_ZSINSBSG2     LINES  W_COUNTER.
    DESCRIBE TABLE IT_ZSINSBSG2_OLD LINES  W_COUNTER1.
    CLEAR :  ZTINS-ZFGDYN.

    IF W_COUNTER NE W_COUNTER1.
      ZTINSB-ZFGDYN = 'X'.
    ELSE.
      LOOP AT IT_ZSINSBSG2_OLD.
        READ TABLE IT_ZSINSBSG2 WITH KEY
                               ZFLSG2 = IT_ZSINSBSG2_OLD-ZFLSG2.
        IF SY-SUBRC NE 0.
          ZTINSB-ZFGDYN = 'X'. EXIT.
        ELSE.
          IF IT_ZSINSBSG2_OLD-ZFDSOG1 NE IT_ZSINSBSG2-ZFDSOG1 OR
             IT_ZSINSBSG2_OLD-ZFDSOG2 NE IT_ZSINSBSG2-ZFDSOG2 OR
             IT_ZSINSBSG2_OLD-ZFDSOG3 NE IT_ZSINSBSG2-ZFDSOG3 OR
             IT_ZSINSBSG2_OLD-ZFDSOG4 NE IT_ZSINSBSG2-ZFDSOG4 OR
             IT_ZSINSBSG2_OLD-ZFDSOG5 NE IT_ZSINSBSG2-ZFDSOG5 OR
             IT_ZSINSBSG2_OLD-ZFPKCN  NE IT_ZSINSBSG2-ZFPKCN  OR
             IT_ZSINSBSG2_OLD-ZFPKCNM NE IT_ZSINSBSG2-ZFPKCNM.
            ZTINSB-ZFGDYN = 'X'. EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_IT_ZSINSSG2_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSINSBAGR_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IT_ZSINSBAGR_UPDATE.

* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSINSBAGR  WHERE ZFLAGR  EQ SPACE.
    DELETE IT_ZSINSBAGR  INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSINSBAGR.
    IT_ZSINSBAGR-ZFLAGR  = SY-TABIX * 10.
    MODIFY IT_ZSINSBAGR  INDEX SY-TABIX.
  ENDLOOP.

  IF SY-DYNNR EQ '4505'.
    DESCRIBE TABLE IT_ZSINSBAGR     LINES  W_COUNTER.
    DESCRIBE TABLE IT_ZSINSBAGR_OLD LINES  W_COUNTER1.
    CLEAR : ZTINSB-ZFCDYN.

    IF W_COUNTER NE W_COUNTER1.
      ZTINSB-ZFCDYN = 'X'.
    ELSE.
      LOOP AT IT_ZSINSBAGR_OLD.
        READ TABLE IT_ZSINSBAGR WITH KEY
                                ZFLAGR = IT_ZSINSBAGR_OLD-ZFLAGR.
        IF SY-SUBRC NE 0.
          ZTINSB-ZFCDYN = 'X'. EXIT.
        ELSE.
          IF IT_ZSINSBAGR_OLD-ZFBCNYN  NE IT_ZSINSBAGR-ZFBCNYN OR
             IT_ZSINSBAGR_OLD-ZFINSCD  NE IT_ZSINSBAGR-ZFINSCD OR
             IT_ZSINSBAGR_OLD-ZFCNCDNM NE IT_ZSINSBAGR-ZFCNCDNM.
            ZTINSB-ZFCDYN = 'X'. EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_IT_ZSINSBAGR_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSINSBSG5_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IT_ZSINSBSG5_UPDATE.

* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSINSBSG5  WHERE ZFLSG5  EQ SPACE.
    DELETE IT_ZSINSBSG5  INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSINSBSG5.
    IT_ZSINSBSG5-ZFLSG5 = SY-TABIX * 10.
    MODIFY IT_ZSINSBSG5  INDEX SY-TABIX.
  ENDLOOP.

ENDFORM.                    " P2000_IT_ZSINSBSG5_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_REQ_INIT_SCR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_REQ_INIT_SCR.

  MOVE 'ZIM03' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Import Document
  MOVE 'DELE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Delete
  MOVE 'LGIS'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">ºÎº¸ÀÇ·Ú.
  MOVE 'FLAT'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Flat Data
  MOVE 'EDIS'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">EDI Create
  MOVE 'ME23'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Purchase Order
  MOVE 'CKEK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Check
  MOVE 'MK03'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Vendor
  MOVE 'COST'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">¼öÀÔÀÇ·Úºñ?
  MOVE 'HIST'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Change Document
  MOVE 'MM03'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Material Master
  MOVE 'MD04'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Stock/requirement
  MOVE 'MMBE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Stock overview
  MOVE 'MB51'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Material document
  MOVE 'ME2M'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Open purchase ord
  MOVE 'ME03'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">Source list
  MOVE 'ZIM93' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  ">ÁøÇà°ü¸®.

ENDFORM.                    " P2000_SET_REQ_INIT_SCR
*&---------------------------------------------------------------------*
*&      Form  P4000_DISPLAY_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_DISPLAY_DOCUMENT.

  SET  PARAMETER ID  'BUK'       FIELD   ZTINSB-BUKRS.
  SET  PARAMETER ID  'GJR'       FIELD   ZTINSB-GJAHR.
  SET  PARAMETER ID  'ZPBENR'    FIELD   ZTINSB-BELNR.
  CALL TRANSACTION 'ZIMY3'.

ENDFORM.                    " P4000_DISPLAY_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTINSB_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_ZTINSB_MODIFY.

*----------------------------------------------------------------------
* ¼öÀÛ¾÷ °³¼³ÀÏ °æ¿ì, Status º¯?
*----------------------------------------------------------------------
  IF W_STATUS EQ C_OPEN_C.
    IF W_NEW_DOCST NE 'N'.        " OPEN Ãë¼Ò°¡ ¾Æ´Ò °æ¿ì.
      ZTINSB-ZFDOCST = 'O'.
    ENDIF.
    W_NEW_DOCST =  'O'.
  ENDIF.

  IF W_NEW_DOCST EQ 'N'.           " OPEN Ãë¼ÒÀÏ °æ¿ì.
    CLEAR : ZTINSB-ZFINNO,     ZTINSBRSP-ZFISDT,  ZTINSB-ZFINRT,
            ZTINSBRSP-ZFEXRT,  ZTINSB-ZFINAMT,    ZTINSB-ZFINAMTC,
            ZTINSB-ZFKRWAMT,   ZTINSBRSP-ZFTAMI,  ZTINSBRSP-ZFTAMIC,
            ZTINSBRSP-ZFCAMI,  ZTINSBRSP-ZFCAMIC, ZTINSBRSP-ZFDAMI,
            ZTINSBRSP-ZFDAMIC, ZTINSBRSP-ZFIPR,   ZTINSBRSP-ZFIPRC,
            ZTINSBRSP-ZFTPR,   ZTINSBRSP-ZFTPRC,  ZTINSBRSP-ZFCPR,
            ZTINSBRSP-ZFCPRC,  ZTINSBRSP-ZFDPR,   ZTINSBRSP-ZFDPRC,
            ZTINSBRSP-ZFVPR,   ZTINSBRSP-ZFVPRC.
  ENDIF.

* ¹®¼­ TYPEº°·Î FUCNTION CALL
  IF ZTIMIMGTX-ZFEDIYN EQ 'X' AND ZTIMIMGTX-APPCIP EQ 'X'.
    IF W_STATUS EQ C_ADD_U OR W_STATUS EQ C_OPEN_C OR
       W_STATUS EQ C_REQ_C OR W_STATUS EQ C_REQ_U  OR
       W_STATUS EQ C_OPEN_U.
      PERFORM   P2000_INSURANCE_CHECK.
    ENDIF.
  ENDIF.

* INVOICE AMOUNT¿Í ¼öÀÔÀÇ·Ú ±Ý¾×°ú Â÷ÀÌ°¡ ¹ß»ý½Ã WARING ERROR MESSAGE
  IF W_OK_CODE EQ 'SAVE'.
    IF ZTBL-ZFBLAMT NE ZTINSB-ZFIVAMT.
      MESSAGE W217.
    ENDIF.
  ENDIF.

* »ý¼ºÀÏ °æ¿ì...
  IF W_STATUS EQ C_REQ_C.
*>> º¸Çè¹®¼­ »ý¼º½Ã... --> È½Â÷ Áõ°¡..
    IF ZTINSB-ZFINSEQ IS INITIAL.
      SELECT MAX( ZFINSEQ ) INTO ZTINSB-ZFINSEQ
             FROM ZTINSB
             WHERE ZFBLNO  EQ  ZTINSB-ZFBLNO.
      ZTINSB-ZFINSEQ = ZTINSB-ZFINSEQ + 1.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZIM_INSURANCE_BL_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFBLNO          = ZTINSB-ZFBLNO
            ZFINSEQ         = ZTINSB-ZFINSEQ
            ZFSTATUS        = W_STATUS
            W_ZTINSB        = ZTINSB
            W_ZTINSB_OLD    = *ZTINSB
            W_ZTINSBRSP     = ZTINSBRSP
            W_ZTINSBRSP_OLD = *ZTINSBRSP
            W_ZTINSBSG3     = ZTINSBSG3
            W_ZTINSBSG3_OLD = *ZTINSBSG3
       TABLES
            IT_ZSINSBAGR    = IT_ZSINSBAGR
            IT_ZSINSBSG2    = IT_ZSINSBSG2
            IT_ZSINSBSG5    = IT_ZSINSBSG5
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E168.
  ELSE.
    CASE W_OK_CODE.
      WHEN 'DELE'.
        PERFORM  P3000_FLAT_DATA_DELETE   USING  ZTINSB-ZFDOCNO.
        MESSAGE  S170  WITH  ZTINSB-ZFBLNO ZTINSB-ZFINSEQ
                                           'Delete'.
      WHEN 'REVK'.
* FLAT DATA DELETE
        PERFORM  P3000_FLAT_DATA_DELETE   USING  ZTINSB-ZFDOCNO.
        MESSAGE  S170  WITH  ZTINSB-ZFBLNO ZTINSB-ZFINSEQ
                                        'FLAT DB Cancel'.
      WHEN 'DELR'.
        MESSAGE  S170  WITH  ZTINSB-ZFBLNO ZTINSB-ZFINSEQ
                                        'Registration cancel'.
      WHEN OTHERS.
        MESSAGE  S170  WITH  ZTINSB-ZFBLNO ZTINSB-ZFINSEQ
                                         'Save'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P3000_ZTINSB_MODIFY

*&---------------------------------------------------------------------*
*&      Form  P2000_INSURANCE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INSURANCE_CHECK.

  ZTINSB-ZFEDICK = 'O'.

  IF ZTINSB-ZFIVAMT IS INITIAL.
    IF ZTINSB-ZFEDFU NE '2'.
      ZTINSB-ZFEDICK = 'X'. MESSAGE I221.   EXIT.
    ENDIF.
  ENDIF.

* ¿î¼Û¹æ¹ý.
  IF ZTINSB-ZFTRANS IS INITIAL.
    ZTINSB-ZFEDICK = 'X'. MESSAGE I176.   EXIT.
  ENDIF.
* º¸ÇèºÎº¸ÀÏÀÚ.
  IF ZTINSB-ZFINSDT IS INITIAL.
    ZTINSB-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Insuring date'.   EXIT.
  ENDIF.
* O.P No.
  IF NOT ZTINSB-ZFOPNO IS INITIAL.
    ASSIGN ZTINSB-ZFOPNO         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ENDIF.
* ÇÇº¸ÇéÀÚ »óÈ£.
  IF ZTINSB-ZFINSU1 IS INITIAL.
    ZTINSB-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Firm name of insured person'.   EXIT.
  ELSE.
    ASSIGN ZTINSB-ZFINSU1        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
    ASSIGN ZTINSB-ZFINSU1        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ENDIF.
* º¸Çè°è¾àÀÚ »óÈ£.
  IF ZTINSB-ZFELENM IS INITIAL.
    ZTINSB-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Policyholder firm name'. EXIT.
  ENDIF.
* ´ëÇ¥ÀÚ.
  IF ZTINSB-ZFREPRE IS INITIAL.
    ZTINSB-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Representative name'.        EXIT.
  ENDIF.
* »ç¾÷ÀÚ µî·Ï¹øÈ£.
  IF ZTINSB-ZFELTXN IS INITIAL.
    ZTINSB-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Registration number for taxation'.  EXIT.
  ENDIF.
  ASSIGN ZTINSB-ZFELENM        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFREPRE        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFELTXN        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFELEID        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
* ´ëÇ¥ .H/S CODE
  IF ZTINSB-ZFRSTAW IS INITIAL.
    ZTINSB-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Code of item classification'.  EXIT.
  ENDIF.
* »óÇ°¸í¼¼.
  LOOP AT IT_ZSINSBSG2.
    IF IT_ZSINSBSG2-ZFDSOG1 IS INITIAL.
      ZTINSB-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Item description'.  EXIT.
    ENDIF.
    ASSIGN IT_ZSINSBSG2       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    ZTINSB-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Item description'.  EXIT.
  ENDIF.
* Ç×Â÷ / ¼±±â¸í.
  IF ZTINSBSG3-ZFCARNU IS INITIAL.
    ZTINSB-ZFEDICK = 'X'. MESSAGE I167 WITH 'Voyage No'.  EXIT.
  ELSE.
    ASSIGN ZTINSBSG3-ZFCARNU     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ENDIF.
  IF ZTINSBSG3-ZFCARNM IS INITIAL.
    ZTINSB-ZFEDICK = 'X'. MESSAGE I167 WITH '¼±±â¸í'.  EXIT.
  ELSE.
    ASSIGN ZTINSBSG3-ZFCARNM     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ENDIF.
* ¹ßÇà ºÎ¼ö.
  IF ZTINSB-ZFNUCD IS INITIAL.
    ZTINSB-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'requested circulation for copy'. EXIT.
  ENDIF.
  IF ZTINSB-ZFNUOD IS INITIAL.
    ZTINSB-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'requested circulation for the original'. EXIT.
  ENDIF.
* INVOICE AMOUNT
  IF ZTINSB-ZFIVAMT IS INITIAL.
    IF SY-TCODE EQ 'ZIMB1' OR SY-TCODE EQ 'ZIMB2' OR
       SY-TCODE EQ 'ZIMB3' OR SY-TCODE EQ 'ZIMB4'.
      ZTINSB-ZFEDICK = 'X'. MESSAGE E167 WITH 'Invoice amount'.  EXIT.
    ELSE.
      MESSAGE I167 WITH 'Invoice amount'.
    ENDIF.
  ENDIF.
  IF ZTINSB-WAERS   IS INITIAL.
    ZTINSB-ZFEDICK = 'X'. MESSAGE E167 WITH 'Invoice Cur.'.  EXIT.
  ENDIF.
* ±âº»ºÎ°¡Á¶°Ç.
*> LG¸¦ À§ÇÑ ÇÊµå º¯°æ..
  IF ZTINSB-ZFBSINS IS INITIAL.
    ZTINSB-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Other additional condition'.  EXIT.
  ENDIF.
* AMEND »çÇ× CHECK.....
*  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR
*    SY-TCODE EQ 'ZIM47' OR SY-TCODE EQ 'ZIM48'.
*   IF ZTINS-ZFIVYN  NE 'X' AND ZTINS-ZFDUYN  NE 'X' AND
*      ZTINS-ZFINAYN NE 'X' AND ZTINS-ZFTAMYN NE 'X' AND
*      ZTINS-ZFPEYN  NE 'X' AND ZTINS-ZFPDYN  NE 'X' AND
*      ZTINS-ZFDTYN  NE 'X' AND ZTINS-ZFPRYN  NE 'X' AND
*      ZTINS-ZFDOYN  NE 'X' AND ZTINS-ZFTMYN  NE 'X' AND
*      ZTINS-ZFHSYN  NE 'X' AND ZTINS-ZFGDYN  NE 'X' AND
*      ZTINS-ZFPAYN  NE 'X' AND ZTINS-ZFEIYN  NE 'X' AND
*      ZTINS-ZFCDYN  NE 'X' AND ZTINS-ZFETYN  NE 'X' AND
*      ZTINS-ZFADYN  NE 'X'.
*     ZTINS-ZFEDICK = 'X'.    MESSAGE E211.
*   ENDIF.
* ENDIF.

* Æ¯¼ö¹®ÀÚ.
  ASSIGN ZTINSB-ZFFTX1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFFTX2         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFFTX3         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFFTX4         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFFTX5         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFETC1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFETC2         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFETC3         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFETC4         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.
  ASSIGN ZTINSB-ZFETC5         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINSB-ZFEDICK NE 'X'.

ENDFORM.                    " P2000_INSURANCE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_FLAT_DATA_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTINSB_ZFDOCNO  text
*----------------------------------------------------------------------*
FORM P3000_FLAT_DATA_DELETE USING    P_ZFDOCNO.

* FLAT HEADER TABLE
  DELETE FROM ZTDHF1 WHERE ZFDHENO EQ P_ZFDOCNO.
* FLAT ITEM TABLE
  DELETE FROM ZTDDF1 WHERE ZFDDENO EQ P_ZFDOCNO.

ENDFORM.                    " P3000_FLAT_DATA_DELETE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_BL_INSDOC_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2911   text
*----------------------------------------------------------------------*
FORM P2000_SET_BL_INSDOC_LOCK USING    PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTINSB'
         EXPORTING
              ZFBLNO  = ZTINSB-ZFBLNO
              ZFINSEQ = ZTINSB-ZFINSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Insuarance Document'
                            ZTINSB-ZFBLNO ZTINSB-ZFINSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.

  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTINSB'
         EXPORTING
              ZFBLNO  = ZTINSB-ZFBLNO
              ZFINSEQ = ZTINSB-ZFINSEQ.
  ENDIF.

ENDFORM.                    " P2000_SET_BL_INSDOC_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_INS_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INS_DEL_STATUS_CHECK.

  IF ZTINSB-ZFDOCST EQ 'O'.
    MESSAGE E104 WITH ZTINSB-ZFBLNO ZTINSB-ZFDOCST.
  ENDIF.
* º¸Çè Á¤º¸ °ËÁõ(ÈÄ¼Ó ÀÛ¾÷)
  SELECT COUNT( * ) INTO W_COUNT FROM ZTINSB
         WHERE ZFBLNO  EQ ZTINSB-ZFBLNO
         AND   ZFINSEQ NE ZTINSB-ZFINSEQ.
  IF W_COUNT GT 0.
    MESSAGE E108 WITH ZTINSB-ZFBLNO W_COUNT.
  ENDIF.
* º¸Çè·á ºñ¿ëÀÚ·á À¯¹« CHECK!
* MKIM Ãß°¡ FROM
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFPSMS EQ '2'.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTBSEG
    WHERE  ZFIMDNO   EQ ZTINSB-ZFBLNO
    AND    ZFCSTGRP  EQ '004'
    AND    ZFCD      EQ '1AB'.
    IF W_COUNT > 0. MESSAGE E469. ENDIF.
  ENDIF.
* MKIM Ãß°¡ TO
  SELECT COUNT( * ) INTO W_COUNT
  FROM   ZTBLCST AS A INNER JOIN ZTIMIMG08 AS B
  ON     A~ZFCSCD  EQ  B~ZFCD
  WHERE  B~ZFCDTY  EQ '004'
  AND    A~ZFBLNO  EQ ZTINSB-ZFBLNO
  AND NOT ( A~ZFACDO  IS NULL
  OR        A~ZFACDO  EQ SPACE ).
  IF W_COUNT > 0. MESSAGE E469. ENDIF.

ENDFORM.                    " P2000_INS_DEL_STATUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_TRANS_METHOD_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_TRANS_METHOD_SET.

  DATA : L_ZFLAGR   LIKE   ZTINSBAGR-ZFLAGR.

  " Ç×Â÷, ¼±±â¸í.
  CASE ZTINSB-ZFTRANS.
    WHEN 'A' OR 'B'.
      MOVE: 'A'                   TO ZTINSBSG3-ZFCARNU,
           'PER REGULAR AIRLINER' TO ZTINSBSG3-ZFCARNM.
    WHEN 'O'.
      MOVE:'T.B.D'                TO ZTINSBSG3-ZFCARNU,
           'TO BE DECLARED'       TO ZTINSBSG3-ZFCARNM.
  ENDCASE.

ENDFORM.                    " P2000_TRANS_METHOD_SET
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTINS_DUP_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_ZTINS_DUP_LIST.

  WRITE : / IT_ZTINS-ZFINSEQ NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZTINS-ZFOPCD  NO-GAP,   SY-VLINE  NO-GAP,
          IT_ZTINS-ZFIVAMT CURRENCY  IT_ZTINS-WAERS
                           NO-GAP,   SY-VLINE NO-GAP,
          IT_ZTINS-WAERS   NO-GAP,   SY-VLINE NO-GAP,
          IT_ZTINS-ZFINSDT NO-GAP,   SY-VLINE NO-GAP,
          IT_ZTINS-ZFOPNO(28) NO-GAP, SY-VLINE NO-GAP.

ENDFORM.                    " P2000_ZTINS_DUP_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_INS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_INS_DOC.

  IF NOT ( ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1 ).
    EXIT.
  ENDIF.

  CALL FUNCTION 'ZIM_GET_INSURANCE_BL_DOC'
       EXPORTING
            ZFBLNO           = ZSREQHD-ZFBLNO
            ZFINSEQ          = ZSREQHD-ZFINSEQ
       IMPORTING
            W_ZTINSB         = ZTINSB
            W_ZTINSBRSP      = ZTINSBRSP
            W_ZTINSBSG3      = ZTINSBSG3
       TABLES
            IT_ZSINSBAGR     = IT_ZSINSBAGR
            IT_ZSINSBAGR_ORG = IT_ZSINSBAGR_ORG
            IT_ZSINSBSG2     = IT_ZSINSBSG2
            IT_ZSINSBSG2_ORG = IT_ZSINSBSG2_ORG
            IT_ZSINSBSG5     = IT_ZSINSBSG5
            IT_ZSINSBSG5_ORG = IT_ZSINSBSG5_ORG
       EXCEPTIONS
            NOT_FOUND        = 4
            NOT_INPUT        = 8.

  CASE SY-SUBRC.
    WHEN 4.
      IF SY-TCODE NE 'ZIMB1'.
        MESSAGE E169 WITH ZSREQHD-ZFBLNO.
      ENDIF.
    WHEN 8.
      MESSAGE E173.
  ENDCASE.

* º¸Çè µî±Þ °Ë»ç.
  IF NOT ( ZTBL-ZFINSYN EQ 'A' OR ZTBL-ZFINSYN EQ 'Z' ).
    MESSAGE I220 WITH ZTBL-ZFBLNO ZTBL-ZFINSYN.
  ENDIF.

* º¯°æÀÌ·ÂÀ» À§?
   *ZTINSB    = ZTINSB.
   *ZTINSBRSP = ZTINSBRSP.
   *ZTINSBSG3 = ZTINSBSG3.

* AMEND¿ë TEMP

* »ý¼ºÀÏ °æ¿ì, exit
  CHECK : SY-TCODE NE 'ZIM41'.

* DOC »óÅÂ CHECK.....
  PERFORM   P2000_INS_STATUS_CHECK.

  W_ZFTRANS = ZTINSB-ZFTRANS.
* LOCK OBJECT
  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D ).
    PERFORM P2000_SET_INS_DOC_LOCK USING    'L'.
  ENDIF.

ENDFORM.                    " P1000_READ_INS_DOC
*&---------------------------------------------------------------------*
*&      Form  P2000_INS_STATUS_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INS_STATUS_CHECK.

* ¾÷¹«º°·Î ¹®¼­ »óÅÂ°ËÁõÇÔ.
  CASE W_STATUS.
    WHEN C_REQ_U.        " º¯?
      PERFORM P2000_INS_CHAGE_CHECK.
    WHEN C_REQ_D.        " Á¶È¸....
      PERFORM P2000_INS_DISPLAY_CHECK.
    WHEN C_OPEN_C.                                          " OPEN
      PERFORM P2000_INS_OPEN_CHECK       CHANGING W_ERR_CHK.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_INS_STATUS_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_INS_CHAGE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INS_CHAGE_CHECK.

* ¹®¼­ »ó?
  CASE ZTINSB-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTINSB-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTINS-ZFREQNO 'EDI open requested' 'Change'.
      ELSEIF ZTINSB-ZFEDIST = 'N'.
        MESSAGE E999 WITH ZTINS-ZFREQNO 'Flat Data created'
                                        'Change'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTINSB-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTINSB-ZFBLNO 'EDI opened' 'Change'.
      ELSE.
        MESSAGE E999
               WITH ZTINSB-ZFBLNO 'Non-EDI opened' 'Change'.
      ENDIF.
      EXIT.
    WHEN 'C'.                                               " CANCEL
      MESSAGE E999 WITH ZTINSB-ZFBLNO 'CANCELED' 'Change'.
    WHEN 'A'.                                               " AMEND
      MESSAGE E999 WITH ZTINSB-ZFBLNO 'Amended' 'Change'.
  ENDCASE.

ENDFORM.                    " P2000_INS_CHAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_INS_DISPLAY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INS_DISPLAY_CHECK.

* ¹®¼­ »óÅÂ.
  CASE ZTINSB-ZFDOCST.
    WHEN 'R'.          " ÀÇ·Ú.
      IF ZTINSB-ZFEDIST = 'S'.
        MESSAGE S998 WITH ZTINSB-ZFBLNO 'EDI open requested'.
      ENDIF.
    WHEN 'O'.          " È®Á¤.
      IF ZTINSB-ZFEDIST = 'R'.
        MESSAGE S998 WITH ZTINSB-ZFBLNO 'EDI opened'.
      ELSE.
        MESSAGE S998 WITH ZTINSB-ZFBLNO 'Non-EDI opened'.
      ENDIF.
    WHEN 'C'.                                               " CANCEL
      MESSAGE S998 WITH ZTINSB-ZFBLNO 'CALCELED'.
    WHEN 'A'.                                               " AMEND
      MESSAGE S998 WITH ZTINSB-ZFBLNO 'AMENDED'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_INS_DISPLAY_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_INS_OPEN_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P2000_INS_OPEN_CHECK CHANGING W_ERR_CHK.
* ¹®¼­ »óÅÂ.
  CASE ZTINSB-ZFDOCST.
    WHEN 'R'.          " ÀÇ?
      IF ZTINSB-ZFEDIST = 'S'.
        MESSAGE W999 WITH ZTINSB-ZFBLNO 'EDI open requested' 'Open'.
      ELSE.
        MESSAGE W999 WITH ZTINSB-ZFBLNO 'EDI Data Created' 'Open'.
      ENDIF.
    WHEN 'O'.          " È®?
      IF ZTINSB-ZFEDIST = 'R'.
        MESSAGE W999 WITH ZTINSB-ZFBLNO 'EDI opened' 'Open'.
      ELSE.
        MESSAGE S998 WITH ZTINSB-ZFBLNO 'Non-EDI opened'.
      ENDIF.
    WHEN 'C'.                                               " CANCEL
      MESSAGE E999 WITH ZTINSB-ZFBLNO 'CANCELED'  'Open'.
    WHEN 'A'.                                               " AMEND
      MESSAGE E999 WITH ZTINSB-ZFBLNO 'AMENDED'  'Open'.
  ENDCASE.

  W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_INS_OPEN_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INS_DOC_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5769   text
*----------------------------------------------------------------------*
FORM P2000_SET_INS_DOC_LOCK USING    PA_MODE.

  IF SY-TCODE EQ 'ZIMB1' AND PA_MODE EQ 'U'.
    IF SY-CALLD NE 'X'.
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLDOC'
           EXPORTING
                ZFBLNO = ZSREQHD-ZFBLNO.
    ENDIF.
  ENDIF.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTINSB'
         EXPORTING
              ZFBLNO  = ZTINSB-ZFBLNO
              ZFINSEQ = ZTINSB-ZFINSEQ
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Insurance document'
                            ZTINSB-ZFBLNO ZTINSB-ZFINSEQ
                  RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTINSB'
         EXPORTING
              ZFBLNO  = ZTINSB-ZFBLNO
              ZFINSEQ = ZTINS-ZFINSEQ.
  ENDIF.

ENDFORM.                    " P2000_SET_INS_DOC_LOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_INSURANCE_BIT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INSURANCE_BIT_SET.

* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CASE ZTBL-INCO1.
    WHEN 'EXW' OR 'FCA' OR 'FAS' OR 'FOB' OR 'CPT' OR 'CFR'.
      IF ZTBL-ZFINSYN IS INITIAL.
        IF SY-DYNNR NE '0100'.
          MESSAGE S090 WITH ZTBL-ZFINSYN 'A'.
        ENDIF.
        ZTBL-ZFINSYN = 'A'.
      ENDIF.
    WHEN OTHERS.
      IF ZTBL-ZFINSYN IS INITIAL.
        IF SY-DYNNR NE '0100'.
          ZTBL-ZFINSYN = 'N'.
        ENDIF.
      ELSE.
        IF ZTBL-ZFINSYN NE 'N'.
          IF SY-DYNNR NE '0100'.
            MESSAGE W349 WITH ZTBL-INCO1.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_INSURANCE_BIT_SET
*&---------------------------------------------------------------------*
*&      Form  P2000_MAINPO_DATA_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MAINPO_DATA_MOVE .

*-----------------------------------------------------------------------
* P/O ITEM TABLE SELECT ( EKPO )
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_GET_PO_BL_ITEM'
       EXPORTING
            EBELN          = ZSREQHD-ZFMAINPO
            LOEKZ          = SPACE
            ELIKZ          = SPACE
       IMPORTING
            W_TOT_AMOUNT   = W_TOT_AMOUNT
            W_ZFMAUD       = W_ZFMAUD
            W_ZFWERKS      = W_ZFWERKS
            W_BEDNR        = W_BEDNR
            W_MATNR        = W_MATNR
            W_TXZ01        = W_TXZ01
       TABLES
            IT_ZSBLIT      = IT_ZSBLIT
            IT_ZSBLIT_ORG  = IT_ZSBLIT_ORG
       EXCEPTIONS
            KEY_INCOMPLETE = 1
            NOT_FOUND      = 2
            NO_REFERENCE   = 3
            NO_AMOUNT      = 4.

  CASE SY-SUBRC.
    WHEN 1.    MESSAGE E003.
    WHEN 2.    MESSAGE E006     WITH ZSREQHD-ZFMAINPO.
    WHEN 3.    MESSAGE E006     WITH ZSREQHD-ZFMAINPO.
    WHEN 4.    MESSAGE E007     WITH ZSREQHD-ZFMAINPO.
  ENDCASE.

*>> P/O DATA GET.
  CALL FUNCTION 'ZIM_GET_PO_HEADER'
       EXPORTING
            EBELN          = ZSREQHD-ZFMAINPO
       IMPORTING
            W_EKKO         = EKKO
       EXCEPTIONS
            NOT_INPUT      = 1
            NOT_FOUND      = 2
            NOT_RELEASED   = 3
            NOT_TYPE       = 4
            NO_TRANSACTION = 5
            PO_DELETION    = 6.

  CASE SY-SUBRC.
    WHEN 1.    MESSAGE E003.
    WHEN 2.    MESSAGE E001     WITH ZSREQHD-ZFMAINPO.
    WHEN 3.    MESSAGE E390(ME) WITH ZSREQHD-ZFMAINPO.
    WHEN 4.    MESSAGE E002     WITH ZSREQHD-ZFMAINPO EKKO-BSTYP.
    WHEN 5.    MESSAGE E000     WITH 'ME23N'.
    WHEN 6.    MESSAGE E005     WITH ZSREQHD-ZFMAINPO.
  ENDCASE.

*>> ¼öÀÔ µ¥ÀÌÅ¸ GET.
  CLEAR : EIKP.
  SELECT SINGLE * FROM  EIKP
         WHERE    EXNUM EQ   EKKO-EXNUM.
  SELECT * FROM  EKPO  UP TO 1 ROWS
           WHERE EBELN EQ EKKO-EBELN.
  ENDSELECT.

  IF EIKP-VORPA EQ 'A'.
    MOVE   'AIR'    TO  ZTBL-ZFVIA.
  ELSEIF EIKP-VORPA EQ 'O'.
    MOVE   'VSL'    TO  ZTBL-ZFVIA.
  ENDIF.

  IF EKKO-LIFRE IS INITIAL.
    MOVE EKKO-LIFNR      TO ZTBL-ZFBENI.    " POÀÇ Vendor
  ELSE.
    MOVE EKKO-LIFRE      TO ZTBL-ZFBENI.    " POÀÇ Supply
  ENDIF.

*>> ¾÷Ã¼¼±Á¤ È¸»çÀÌ¸é DEFAULT ¾÷Ã¼ º¸¿©ÁÖ±â.
  CLEAR : ZTIMIMG11.
  SELECT SINGLE * FROM ZTIMIMG11 WHERE BUKRS EQ EKKO-BUKRS.
  IF ZTIMIMG11-ZFVNCT EQ 'X'.
    MOVE  ZTIMIMG11-ZFHAYEK  TO  ZTBL-ZFHAYEK.
  ENDIF.
  READ  TABLE  IT_ZSBLIT  INDEX  1.

  MOVE : IT_ZSBLIT-ZFSHNO        TO     ZTBL-ZFSHNO,     "¼±ÀûÂ÷¼ö.
         ZSREQHD-ZFHBLNO         TO     ZTBL-ZFHBLNO,    "HOUSE B/L NO.
         ZSREQHD-ZFMAINPO        TO     ZTBL-ZFREBELN,   "±¸¸Å¿À´õ.
         EKKO-WAERS              TO     ZTBL-ZFBLAMC,    "CURRENCY
         EIKP-STGBE              TO     ZTBL-ZFCARC,     "¼±Àû±¹.
         EIKP-STGBE              TO     ZTBL-IMTRD,      "¼öÀÔ±¹.
         'KR'                    TO     ZTBL-ZFAPPC,     "µµÂø±¹.
         EIKP-BEHOE              TO     ZTBL-ZFAPRTC,    "µµÂøÇ×.
         '1'                     TO     ZTBL-ZFMATGB,    "ÀÚÀç±¸ºÐ.
         EKKO-BUKRS              TO     ZTBL-BUKRS,      "È¸»çÄÚµå.
         EKKO-EKORG              TO     ZTBL-EKORG,      "±¸¸ÅÁ¶Á÷.
         EKKO-EKGRP              TO     ZTBL-EKGRP,      "±¸¸Å±×·ì.
         EKKO-LIFNR              TO     ZTBL-LIFNR,      "Vendor
         W_TXZ01                 TO     ZTBL-ZFRGDSR,    "´ëÇ¥Ç°¸í.
         EKPO-WERKS              TO     ZTBL-ZFWERKS,    "ÇÃ·£Æ®.
         EKKO-INCO1              TO     ZTBL-INCO1,      "INCORTERMS.
         EIKP-KZGBE              TO     ZTBL-ZFSPRT,     "¼±ÀûÇ×.
         'F'                     TO     ZTBL-ZFRPTTY,    "¼öÀÔ½Å°í.
         'Y'                     TO     ZTBL-ZFPOYN,     "À¯È¯¿©ºÎ.
         W_TOT_AMOUNT            TO     ZTBL-ZFBLAMT,    "BLAMOUNT.
         'S'                     TO     ZTBL-IMTRD,      "ÀÚ»ç±¸ºÐ.
         SY-UNAME                TO     ZTBL-ERNAM,      "»ý¼ºÀÎ.
         SY-DATUM                TO     ZTBL-CDAT.       "»ý¼ºÀÏ.

  IF ZTBL-ZFVIA EQ 'AIR'.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
         EXPORTING
              INPUT          = 'KG'
         IMPORTING
              OUTPUT         = ZTBL-ZFNEWTM
         EXCEPTIONS
              UNIT_NOT_FOUND = 1
              OTHERS         = 2.
    IF SY-SUBRC NE 0.
      CLEAR ZTBL-ZFNEWTM.
    ENDIF.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
         EXPORTING
              INPUT          = 'TON'
         IMPORTING
              OUTPUT         = ZTBL-ZFNEWTM
         EXCEPTIONS
              UNIT_NOT_FOUND = 1
              OTHERS         = 2.
    IF SY-SUBRC NE 0.
      CLEAR ZTBL-ZFNEWTM.
    ENDIF.

  ENDIF.
*> ¼±Àû¿ëÀû Default.
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
       EXPORTING
            INPUT          = 'CBM'
       IMPORTING
            OUTPUT         = ZTBL-ZFTOVLM
       EXCEPTIONS
            UNIT_NOT_FOUND = 1
            OTHERS         = 2.

  IF SY-SUBRC NE 0.
    CLEAR ZTBL-ZFTOVLM.
  ENDIF.
*-----------------------------------------------------------------------
  MOVE : 'GT'              TO  ZTBL-ZFPKCNM,          " CT
         ZTBL-ZFNEWTM      TO  ZTBL-ZFTOWTM,
         SY-UNAME          TO  ZTBL-ERNAM,
         SY-DATUM          TO  ZTBL-CDAT,
         SY-UNAME          TO  ZTBL-UNAM,
         SY-DATUM          TO  ZTBL-UDAT,
         ''                TO  ZTBL-ZFBLNO,
         ''                TO  ZTBL-ZFPONC,    "
         'Y'               TO  ZTBL-ZFPOYN.    " À¯È¯.

ENDFORM.                    " P2000_MAINPO_DATA_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_PO_ITEM_BL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_GET_PO_ITEM_BL .

*>> P/O Header SELECT(2001.06.12).
  SELECT SINGLE * FROM EKKO  WHERE  EBELN EQ ZSBLIT-EBELN.

* P/O ITEM SELECT
  SELECT SINGLE * FROM EKPO WHERE EBELN EQ ZSBLIT-EBELN
                          AND   EBELP EQ ZSBLIT-EBELP
                          AND   PSTYP NE '6'.

  IF SY-SUBRC NE 0.
    MESSAGE  E977 WITH 'Purchase Doc is not exist.'.
  ENDIF.

  CLEAR IT_ZSBLIT.
* data Move
  MOVE-CORRESPONDING EKPO TO IT_ZSBLIT.

  IF EKKO-BSTYP EQ 'F'.       ">±¸¸Å¿À´õ.
    MOVE EKPO-MENGE TO IT_ZSBLIT-MENGE_PO.     " ±¸¸Å¿À´õ ¼ö·®
  ELSEIF EKKO-BSTYP EQ 'L'.   ">³³Ç°ÀÏÁ¤°è¾à.
    MOVE EKPO-KTMNG TO IT_ZSBLIT-MENGE_PO.     " ¸ñÇ¥¼ö·®.
  ELSEIF EKKO-BSTYP EQ 'K'.   ">ÀÏ°ý°è¾à.
    MOVE EKPO-KTMNG TO IT_ZSBLIT-MENGE_PO.     " ¸ñÇ¥¼ö·®.
  ENDIF.

*-----------------------------------------------------------------------
* ¼öÀÔÀÇ·Ú Ç°¸ñº° Summary
*-----------------------------------------------------------------------
  W_MENGE = 0.
  SELECT SUM( BLMENGE ) INTO  W_MENGE
                        FROM  ZTBLIT
                        WHERE EBELN  EQ EKPO-EBELN
                        AND   EBELP  EQ EKPO-EBELP.

  IT_ZSBLIT-BLMENGE = IT_ZSBLIT-MENGE_PO - W_MENGE.
  W_TOT_MENGE = 0.
  IF IT_ZSBLIT-BLMENGE <= 0.
    MESSAGE  E977 WITH '±¸¸Å¹®¼­ÀÇ ¼ö·®À» ¸ðµÎ ¼ÒÁøÇÏ¿´½À´Ï´Ù.'.
  ENDIF.
*-----------------------------------------------------------------------
* B/L Amount
  IF IT_ZSBLIT-PEINH NE 0.
    W_TOT_AMOUNT = W_TOT_AMOUNT +
       ( IT_ZSBLIT-BLMENGE * ( EKPO-BPUMZ / EKPO-BPUMN )
     * ( IT_ZSBLIT-NETPR / IT_ZSBLIT-PEINH ) ).
  ENDIF.

*>> P/O ÀÇ ¿ÜÀÚ Á¶°Ç¿¡¼­ H/S code °¡Á®¿À±â.
  SELECT SINGLE STAWN
    INTO IT_ZSBLIT-STAWN
    FROM EIPO
   WHERE EXNUM  EQ  EKKO-EXNUM
     AND EXPOS  EQ  EKPO-EBELP.

*>> P/O¿¡ ¾øÀ¸¸é, ÀÚÀç¸¶½ºÅÍ¿¡¼­ H/S code °¡Á®¿À±â.
  IF IT_ZSBLIT-STAWN IS INITIAL.
    SELECT SINGLE STAWN
      INTO IT_ZSBLIT-STAWN
      FROM MARC
     WHERE MATNR EQ EKPO-EMATN
       AND WERKS EQ EKPO-WERKS.
  ENDIF.
*>> ¼±ÀûÂ÷¼ö GET.
  SELECT MAX( ZFSHNO ) INTO W_ZFSHNO
         FROM  ZTBLIT
         WHERE EBELN EQ IT_ZSBLIT-EBELN
         AND   EBELP EQ IT_ZSBLIT-EBELP.
  IF W_ZFSHNO IS INITIAL.
    IT_ZSBLIT-ZFSHNO = '01'.
  ELSE.
    IT_ZSBLIT-ZFSHNO = W_ZFSHNO + 1.
  ENDIF.

ENDFORM.                    " P2000_GET_PO_ITEM_BL
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_DATE_INPUT
*&---------------------------------------------------------------------*
FORM P2000_WRITE_DATE_INPUT CHANGING P_INPUT2.

  CONDENSE   P_INPUT2   NO-GAPS.

ENDFORM.                    " P2000_WRITE_DATE_INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_INCOTERMS_CHECK
*&---------------------------------------------------------------------*
FORM P2000_INCOTERMS_CHECK.

  IF ZTBL-INCO1 EQ 'EXW' OR ZTBL-INCO1 EQ 'FCA' OR
     ZTBL-INCO1 EQ 'FOB' OR ZTBL-INCO1 EQ 'FAS'.
  ELSE.
    MESSAGE  S008(ZIM1).
    EXIT.
  ENDIF.

ENDFORM.                    " P2000_INCOTERMS_CHECK
