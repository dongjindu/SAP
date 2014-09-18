*&---------------------------------------------------------------------
*& INCLUDE ZRIM00F01 .
*&---------------------------------------------------------------------
*&  ÇÁ·Î±×·¥¸í : ¼öÀÔÀÇ·Ú Main SUB MODULE Include
*&      ÀÛ¼ºÀÚ : °­¼®ºÀ INFOLINK Ltd.
*&      ÀÛ¼ºÀÏ : 2000.01.25
*&  Àû¿ëÈ¸»çPJT:
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*&---------------------------------------------------------------------
*&      Form  P2000_OK_CODE_PROCESS
*&---------------------------------------------------------------------
FORM P2000_OK_CODE_PROCESS.
  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'ENTR' OR 'DDLC'.
    WHEN 'COPY'.
    WHEN 'CRDC'.          " »ý?
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          LEAVE TO TRANSACTION 'ZIM01'.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          LEAVE TO TRANSACTION 'ZIM11'.
        WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
          LEAVE TO TRANSACTION 'ZIM41'.
        WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
          LEAVE TO TRANSACTION 'ZIM45'.
        WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
          LEAVE TO TRANSACTION 'ZIML1'.
      ENDCASE.
    WHEN 'CHDC'.         " º¯?
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          LEAVE TO TRANSACTION 'ZIM02'.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          LEAVE TO TRANSACTION 'ZIM12'.
        WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
          LEAVE TO TRANSACTION 'ZIM42'.
        WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
          LEAVE TO TRANSACTION 'ZIM46'.
        WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
          LEAVE TO TRANSACTION 'ZIML2'.
      ENDCASE.
    WHEN 'DISP'.
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          LEAVE TO  TRANSACTION 'ZIM03'.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          LEAVE TO  TRANSACTION 'ZIM13'.
        WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
          LEAVE TO  TRANSACTION 'ZIM43'.
        WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
          LEAVE TO  TRANSACTION 'ZIM47'.
        WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
          LEAVE TO  TRANSACTION 'ZIML3'.
      ENDCASE.
    WHEN 'ADDC'.
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          LEAVE TO TRANSACTION 'ZIM05'.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          LEAVE TO TRANSACTION 'ZIM15'.
      ENDCASE.
    WHEN 'OPDC'.
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          LEAVE TO TRANSACTION 'ZIM07'.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          LEAVE TO TRANSACTION 'ZIM17'.
        WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
          LEAVE TO  TRANSACTION 'ZIM44'.
        WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
          LEAVE TO  TRANSACTION 'ZIM48'.
      ENDCASE.
    WHEN OTHERS.
      LEAVE TO TRANSACTION OK-CODE.
  ENDCASE.

ENDFORM.                    " P2000_OK_CODE_PROCESS
*&---------------------------------------------------------------------
*&      Form  P2000_SET_MESSAGE
*&---------------------------------------------------------------------
FORM P2000_SET_MESSAGE USING    P_SY_UCOMM.

  CASE P_SY_UCOMM.
    WHEN 'ANZG'.      " CHANGE  ==> DISPLAY
      PERFORM  P2000_ANZG_MESSAGE.
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
    WHEN 'EDIS'.       " EDI Send
      PERFORM  P2000_EDI_SEND_MESSAGE.
    WHEN 'LGIS'.       ">ºÎº¸ÀÇ·Ú.
      PERFORM  P2000_LGI_SEND_MESSAGE.
    WHEN OTHERS.

  ENDCASE.
ENDFORM.                    " P2000_SET_MESSAGE

*&---------------------------------------------------------------------
*
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------
*
FORM P2000_CANCEL_MESSAGE.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'Ãë¼Ò È®ÀÎ'
                                 'º¯°æµÈ ³»¿ëÀ» ÀúÀå¾øÀÌ Á¾·áµË´Ï´Ù.'
                                 'Á¾·áÇÏ½Ã°Ú½À´Ï±î?'
                                 'N'
                                 '2'.
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
        'Cancel confirm'
        'Being ended without saving the changed item.'
        'Do you want to end?'
        'N'
        '2'.
  ENDIF.

  CASE ANTWORT.
    WHEN 'Y'.                                               " Yes...
      MESSAGE  S957.
      LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_CANCEL_MESSAGE

*&---------------------------------------------------------------------
*
*&      Form  P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------
*
FORM P2000_EXIT_MESSAGE.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'Á¾·á È®ÀÎ'
                         'ÇöÀç ÀÔ·Â³»¿ªÀ» ÀúÀåÇÏÁö ¾Ê½À´Ï´Ù.'
                         'ÀúÀå ÈÄ Á¾·áÇÏ½Ã°Ú½À´Ï±î?'
                         'Y'
                         '1'.
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
        'End confirm'
        'Do not save the entered item.'
        'Do you want to end after save?'
        'Y'
        '1'.
  ENDIF.

ENDFORM.                    " P2000_EXIT_MESSAGE

*&---------------------------------------------------------------------
*
*&      Form  P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------
*
FORM P2000_SAVE_MESSAGE.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'ÀúÀå È®ÀÎ'
                                 'ÀÔ·ÂµÈ ³»¿ªÀ» ÀúÀåÇÕ´Ï´Ù.'
                                 'ÀúÀåÇÏ½Ã°Ú½À´Ï±î?'
                                 'Y'
                                 '1'.
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
        'Save confirm '
        'Being saved entered item.'
        'Do you want to save?'
        'Y'
        '1'.
  ENDIF.
ENDFORM.                    " P2000_SAVE_MESSAGE

*&---------------------------------------------------------------------
*
*&      Form  P2000_ANZG_MESSAGE
*&---------------------------------------------------------------------
*
FORM P2000_ANZG_MESSAGE.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'ÀÌµ¿ È®ÀÎ'
                                 'ÇöÀç ÀÔ·Â³»¿ªÀ» ÀúÀåÇÏÁö ¾Ê½À´Ï´Ù.'
                                 'ÀúÀå ÈÄ ÀÌµ¿ÇÏ½Ã°Ú½À´Ï±î?'
                                 'Y'
                                 '1'.
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
        'Move confirm'
        'Do not save entered item.'
        'Do you want to move another screen after save?'
        'Y'
        '1'.
  ENDIF.
ENDFORM.                    " P2000_ANZG_MESSAGE

*&---------------------------------------------------------------------
*
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------
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
  TEXTLEN = 40.

  CALL SCREEN 0001 STARTING AT 30 6
                   ENDING   AT 78 10.

  IF ANTWORT = 'C'.                                         " Cancel
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX
*&---------------------------------------------------------------------
*
*&      Form  P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------
*
FORM P2000_SET_SCREEN_SCRCOM.
* CALL TransactionÀÏ °æ¿ì, ÇÁ·Î±×·¥ Leave
  IF SY-CALLD EQ 'X'.
    LEAVE.
  ENDIF.

  CASE W_STATUS.
    WHEN C_REQ_C.
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          PERFORM   P2000_PO_UNLOCK.
          SET SCREEN 100.    " »ý?
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          PERFORM   P2000_PO_UNLOCK.
          SET SCREEN 1100.    " »ý?
        WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
          SET SCREEN 4100.    " »ý?
        WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
          SET SCREEN 4500.    " »ý?
        WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
          SET SCREEN 2100.    " »ý?
        WHEN OTHERS.      SET SCREEN 0000.
      ENDCASE.

    WHEN C_REQ_U OR C_OPEN_U.     " º¯?
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          PERFORM   P2000_PO_UNLOCK.
          SET SCREEN 200.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          PERFORM   P2000_PO_UNLOCK.
          SET SCREEN 1200.
        WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
          SET SCREEN 4200.
        WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
          SET SCREEN 4600.    " º¯?
        WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
          SET SCREEN 2200.    " »ý?
        WHEN OTHERS.      SET SCREEN 0000.
      ENDCASE.

    WHEN C_REQ_D.         " Á¶?
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          SET SCREEN 300.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          SET SCREEN 1300.
        WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
          SET SCREEN 4300.
        WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
          SET SCREEN 4700.    " Á¶?
        WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
          SET SCREEN 2300.    " »ý?
        WHEN OTHERS.      SET SCREEN 0000.
      ENDCASE.
    WHEN C_ADD_U.          " Ãß°¡ ÀÔ?
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          PERFORM   P2000_PO_UNLOCK.
          SET SCREEN 500.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          PERFORM   P2000_PO_UNLOCK.
          SET SCREEN 1500.
        WHEN OTHERS.      SET SCREEN 0000.
      ENDCASE.

    WHEN C_OPEN_C.
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          PERFORM   P2000_PO_UNLOCK.
          SET SCREEN 700.    " »ý?
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          PERFORM   P2000_PO_UNLOCK.
          SET SCREEN 1700.    " »ý?
        WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
          SET SCREEN 4400.    " »ý?
        WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
          SET SCREEN 4800.    " Á¶?
        WHEN OTHERS.      SET SCREEN 0000.
      ENDCASE.

    WHEN OTHERS.
      SET SCREEN 0.
  ENDCASE.
* ÃÊ±âÈ­¸éÀ¸·Î exit½Ã Active TabÀ» Clearing?
  CLEAR :  TABSTRIP-ACTIVETAB.
ENDFORM.                    " P2000_SET_SCREEN_SCRCOM
*&---------------------------------------------------------------------
*
*&      Form  P2000_EXIT_PROCESS
*&---------------------------------------------------------------------
*
FORM P2000_EXIT_PROCESS.
  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
           W_STATUS EQ C_OPEN_D ).

    W_OK_CODE = OK-CODE.
    PERFORM P2000_SET_MESSAGE USING  OK-CODE.

    CASE ANTWORT.
      WHEN 'Y'.                                             " Yes...
*        IF SY-TCODE EQ 'ZIM01' AND ZTREQHD-ZFREQTY EQ 'PU'.
*          PERFORM   P2000_GET_PUR_DOC_NO.
*          IF ANTWORT NE 'Y'.
*            EXIT.
*          ENDIF.
*        ENDIF.
*----------------------------------------------------------------------
* IMPORT RECOMMENDATION
*----------------------------------------------------------------------
        IF SY-TCODE(4) EQ 'ZIM0' OR SY-TCODE(4) EQ 'ZIM1'.
*>>>>>> 2001.08.21 KSB MODIFY.
*          IF ( W_STATUS EQ C_REQ_C OR W_STATUS EQ C_REQ_U ) AND
*             SY-TCODE(4) EQ 'ZIM0'.
*            IF ZTREQHD-ZFMATGB NE '2' AND ZTREQHD-ZFRECYN EQ 'X'.
*              MESSAGE I192.
*            ENDIF.
*          ELSE.

*---------------------------------------------------------------------
* 2001/10/05 KSB DELETIONS START
*---------------------------------------------------------------------
*           IF ZTREQHD-ZFMATGB NE '2' AND ZTREQHD-ZFRECYN EQ 'X'.
*             W_LINE = 0.
*             LOOP AT IT_ZSREQIL WHERE ZFRECNO EQ SPACE.
*                ADD  1   TO   W_LINE.
*             ENDLOOP.
*             IF W_LINE GT 0.
*                PERFORM  P2000_RECOMMEND_MESSAGE.
*                IF ANTWORT EQ 'Y'.
*                  PERFORM  P2000_SET_RECOMMEND_SCR.  EXIT.
*                ENDIF.
*             ENDIF.
*           ENDIF.
*----------------------< DELETE ENDING >------------------------

*          ENDIF.
        ENDIF.
*----------------------------------------------------------------------
* database WRITE
*----------------------------------------------------------------------
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
      WHEN 'C'.                                             " Cancel
      WHEN OTHERS.
    ENDCASE.

  ELSE.
    CLEAR OK-CODE.
    PERFORM  P2000_SET_SCREEN_SCRCOM.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.                    " P2000_EXIT_PROCESS
*&---------------------------------------------------------------------
*
*&      Form  P2000_STATUS_DISPLAY
*&---------------------------------------------------------------------
*
FORM P2000_STATUS_DISPLAY.
  SPOP-TITEL = 'Display: Document Status'.
  CANCEL_OPTION = 'Y'.
*     CLEAR : CANCEL_OPTION.
  OPTION = 1.
  TEXTLEN = 40.
  CASE SY-TCODE.
    WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.     " OFFER SHEET
      CALL SCREEN 0013 STARTING AT 15 5
                       ENDING   AT 93 16.
    WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.      " º¸?
      CALL SCREEN 0015 STARTING AT 15 5
                       ENDING   AT 93 16.
    WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.      " º¸?
      CALL SCREEN 0016 STARTING AT 15 3
                       ENDING   AT 93 19.
    WHEN OTHERS.
      CALL SCREEN 0009 STARTING AT 15 3
                       ENDING   AT 93 17.
  ENDCASE.
ENDFORM.                    " P2000_STATUS_DISPLAY
*&---------------------------------------------------------------------
*
*&      Form  P2000_RECOMMAND_PROCESS
*&---------------------------------------------------------------------
*
FORM P2000_RECOMMAND_PROCESS.
  IF ZTREQHD-ZFRECYN IS INITIAL.
    MESSAGE E010.
  ENDIF.
  IF NOT ( ZTREQHD-ZFREQTY EQ 'LC' OR ZTREQHD-ZFREQTY EQ 'DA' OR
           ZTREQHD-ZFREQTY EQ 'TT' OR ZTREQHD-ZFREQTY EQ 'DP' OR
           ZTREQHD-ZFREQTY EQ 'GS' ).
    MESSAGE E011 WITH ZTREQHD-ZFREQTY.
  ENDIF.
*----------------------------------------------------------------------
* TEXT SET
*----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '¼öÀÔÃßÃµ°ü¸®' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.   W_COUNT = 0.

  CALL SCREEN 0003 STARTING AT 32 3
                   ENDING   AT 78 17.

ENDFORM.                    " P2000_RECOMMAND_PROCESS

*&----------------------------------------------------------------------
*&      Form  P2000_SET_ACTION_TITLE
*&----------------------------------------------------------------------
FORM P2000_SET_ACTION_TITLE.

  CASE W_STATUS.
    WHEN C_REQ_C.  ASSIGN  W_CREATE       TO <FS_F>.
    WHEN C_REQ_U.  ASSIGN  W_CHANGE       TO <FS_F>.
    WHEN C_REQ_D.  ASSIGN  W_DISPLAY      TO <FS_F>.
    WHEN C_ADD_U.  ASSIGN  W_ADD_CHG      TO <FS_F>.
    WHEN C_ADD_D.  ASSIGN  W_ADD_DIS      TO <FS_F>.
    WHEN C_OPEN_C. ASSIGN  W_OPEN         TO <FS_F>.
    WHEN C_OPEN_U. ASSIGN  W_OPEN_CHANGE  TO <FS_F>.
    WHEN C_OPEN_D. ASSIGN  W_OPEN_DISPLAY TO <FS_F>.
    WHEN C_INSU_I. ASSIGN  W_INSURANCE    TO <FS_F>.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_ACTION_TITLE
*&---------------------------------------------------------------------
*
*&      Form  P2000_SET_DOC_TITLE
*&---------------------------------------------------------------------
*
FORM P2000_SET_DOC_TITLE.
  IF SY-TCODE(4) EQ 'ZIM0'.
    CASE ZTREQHD-ZFREQTY.
      WHEN 'LC'.     ASSIGN  C_M_LC         TO <FS_DOC>.
      WHEN 'LO'.     ASSIGN  C_L_LC         TO <FS_DOC>.
      WHEN 'DA'.     ASSIGN  C_DA           TO <FS_DOC>.
      WHEN 'DP'.     ASSIGN  C_DP           TO <FS_DOC>.
      WHEN 'PU'.     ASSIGN  C_PU           TO <FS_DOC>.
      WHEN 'TT'.     ASSIGN  C_TT           TO <FS_DOC>.
      WHEN OTHERS.   ASSIGN  C_M_LC         TO <FS_DOC>.
    ENDCASE.
  ELSEIF SY-TCODE(4) EQ 'ZIM1'.
    CASE ZTREQHD-ZFREQTY.
      WHEN 'LC'.     ASSIGN  C_M_LC_A       TO <FS_DOC>.
      WHEN 'LO'.     ASSIGN  C_L_LC_A       TO <FS_DOC>.
      WHEN 'DA'.     ASSIGN  C_DA_A         TO <FS_DOC>.
      WHEN 'DP'.     ASSIGN  C_DP_A         TO <FS_DOC>.
      WHEN 'PU'.     ASSIGN  C_PU_A         TO <FS_DOC>.
      WHEN 'TT'.     ASSIGN  C_TT_A         TO <FS_DOC>.
      WHEN OTHERS.   ASSIGN  C_M_LC_A       TO <FS_DOC>.
    ENDCASE.
  ELSEIF SY-TCODE(4) EQ 'ZIM4'.
    CASE SY-DYNNR.
      WHEN 4101.
        ASSIGN  'Insurance on cargo'            TO <FS_DOC>.
      WHEN 4501.
        ASSIGN  'Insurance on cargo(Amend)'     TO <FS_DOC>.
      WHEN OTHERS.
    ENDCASE.
  ELSEIF SY-TCODE(4) EQ 'ZIML'.   " OFFER SHEET
    ASSIGN  C_OF       TO <FS_DOC>.
  ENDIF.

ENDFORM.                    " P2000_SET_DOC_TITLE

*&---------------------------------------------------------------------
*&      Form  P2000_SCR0123_PROCESS
*&---------------------------------------------------------------------
FORM P2000_SCR0123_PROCESS.

*----------------------------------------------------------------------
* TEXT SET
*----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' 'Supply items description' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  PERFORM   P2000_TAB_TO_SCR0123.

  CALL SCREEN 0008 STARTING AT 15 1
                   ENDING   AT 95 19.

  IF ANTWORT EQ 'Y'.
    PERFORM   P2000_SCR0123_TO_TAB.
  ENDIF.

ENDFORM.                    " P2000_SCR0123_PROCESS
*&---------------------------------------------------------------------
*
*&      Form  P2000_SCR0004_SHOW
*&---------------------------------------------------------------------
*
FORM P2000_SCR0004_SHOW.
  REFRESH : IT_ZSPURSG1G_SUB.
  LOOP AT IT_ZSPURSG1G WHERE ZFLSG1 EQ ZTPURSG1-ZFLSG1.
    CLEAR : IT_ZSPURSG1G_SUB.
    MOVE-CORRESPONDING IT_ZSPURSG1G  TO  IT_ZSPURSG1G_SUB.
    APPEND IT_ZSPURSG1G_SUB.
  ENDLOOP.

*----------------------------------------------------------------------
* TEXT SET
*----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '°ø±Þ¹°Ç°¸í¼¼ ±Ô°Ý' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  CALL SCREEN 0004 STARTING AT 15 5
                   ENDING   AT 95 15.

  IF ANTWORT EQ 'Y'.
    DELETE IT_ZSPURSG1G WHERE ZFLSG1 EQ ZTPURSG1-ZFLSG1.

    LOOP AT IT_ZSPURSG1G_SUB.
      MOVE-CORRESPONDING IT_ZSPURSG1G_SUB TO IT_ZSPURSG1G.
      MOVE : ZTPURSG1-ZFLSG1          TO IT_ZSPURSG1G-ZFLSG1,
             IT_ZSPURSG1G_SUB-ZFLSG1G TO IT_ZSPURSG1G-ZFLSG1G.
      APPEND IT_ZSPURSG1G.
    ENDLOOP.
  ENDIF.


ENDFORM.                    " P2000_SCR0004_SHOW
*&---------------------------------------------------------------------
*
*&      Form  P2000_SCR0125_PROCESS
*&---------------------------------------------------------------------
*
FORM P2000_SCR0125_PROCESS.
*----------------------------------------------------------------------
* TEXT SET
*----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '±Ù°Å¼­·ù' INTO SPOP-TITEL
                                    SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  MOVE-CORRESPONDING    IT_ZSPURSG4    TO   ZTPURSG4.

  CALL SCREEN 0005 STARTING AT 15 3
                   ENDING   AT 95 15.

  IF ANTWORT EQ 'Y'.
    PERFORM   P2000_SCR0005_TO_TAB.
  ENDIF.


ENDFORM.                    " P2000_SCR0125_PROCESS
*&---------------------------------------------------------------------
*&      Form  P2000_PRE_DOC_NO_INPUT
*&---------------------------------------------------------------------
FORM P2000_PRE_DOC_NO_INPUT.

  SPOP-TITEL = 'Create Import Request : Copy from...'.
  OPTION = 1.

  CALL SCREEN 0002 STARTING AT 27 6
                   ENDING   AT 80 10.

  IF ANTWORT EQ 'C' OR ANTWORT EQ 'N'.       " Cancel Or No
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_PRE_DOC_NO_INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------*
*  DESC : Document TYPE -> Table Update
*----------------------------------------------------------------------*
FORM P3000_DB_MODIFY_SCRCOM.

  CASE SY-TCODE.
* Import Request.
    WHEN 'ZIM01' OR 'ZIM02'  OR 'ZIM05' OR 'ZIM07' OR 'ZIM03'.
      PERFORM  P3000_LC_MODIFY.
* Amend.
    WHEN 'ZIM11' OR 'ZIM12'  OR 'ZIM15' OR 'ZIM17' OR 'ZIM13'.
      PERFORM  P3000_AMEND_MODIFY.
* Insuarance & AMEND
    WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44' OR
         'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
      PERFORM  P3000_ZTINS_MODIFY.
* LOCAL OFFER SHEET
    WHEN 'ZIML1' OR 'ZIML2'  OR 'ZIML3'.             " Offer Sheet
      PERFORM  P3000_OFFER_SHEET_MODIFY.
  ENDCASE.

ENDFORM.                    " P3000_DB_MODIFY_SCRCOM
*&---------------------------------------------------------------------
*
*&      Form  P2000_GET_NUMBER_NEXT
*&---------------------------------------------------------------------
*
FORM P2000_GET_NUMBER_NEXT    USING     P_GUBUN     P_DOCNO.

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
*&      Form  P2000_MOVE_PO_DATA
*&---------------------------------------------------------------------*
FORM P2000_MOVE_PO_DATA.

* Purchase Request Item Move.
  PERFORM   P2000_REQ_FIELD_MOVE.

*> Company information get.
  PERFORM  P1000_READ_COMPANY_DATA  USING ZTREQHD-BUKRS
                                          ZTREQHD-IMTRD.
*> Company EDI Text Data Set.
  PERFORM  P3000_SET_COMPANY_DATA.
  PERFORM  P2000_SET_SHIPING_TEXT.

*> Default Bank Vendor Master code Get..
  PERFORM  P1000_SET_DEFAULT_BANK.

* Supply Vendor Name (OFFER)
  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-LLIEF
                              CHANGING   W_LLIEF_NM.
* Vendor NAME1 GET
  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-LIFNR
                              CHANGING   W_LIFNR_NM.
* Benificiary
  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFBENI
                              CHANGING   W_ZFBENI_NM.
* Loan Organization Set.
  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFLEVN
                              CHANGING   W_ZFLEVN_NM.
* HEAD TEXT
  PERFORM  P1000_READ_HEADER_TEXTS.

* DATA MOVE
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.          " MASTER L/C
      PERFORM   P2000_LC_FIELD_MOVE.
    WHEN 'LO'.          " LOCAL L/C
      PERFORM   P2000_LOCAL_LC_FIELD_MOVE.
    WHEN 'PU'.          " Purchase Approve
      PERFORM   P2000_PURCH_FIELD_MOVE.
    WHEN 'TT'.          " Pay Order
      PERFORM   P2000_PAYORD_FIELD_MOVE.
    WHEN 'DA' OR 'DP' OR 'GS'.  " GSM or D/A or D/P
  ENDCASE.

  PERFORM  P2000_REFRESH_ADDRESS     USING   ZTREQHD-ZFBENI.

ENDFORM.                    " P2000_MOVE_PO_DATA

*&---------------------------------------------------------------------
*&      Form  P1000_READ_REQ_DOC
*&---------------------------------------------------------------------
FORM P1000_READ_REQ_DOC.

  IF SY-TCODE EQ 'ZIM02' OR  SY-TCODE EQ 'ZIM03' OR
     SY-TCODE EQ 'ZIM05' OR  SY-TCODE EQ 'ZIM07'.
    W_AMDNO   =    '00000'.
  ELSE.
    IF ZSREQHD-ZFAMDNO IS INITIAL.
      SELECT MAX( ZFAMDNO ) INTO ZSREQHD-ZFAMDNO FROM ZTREQST
                            WHERE ZFREQNO  EQ    ZSREQHD-ZFREQNO.
    ENDIF.
    W_AMDNO   =    ZSREQHD-ZFAMDNO.
  ENDIF.

* ¼öÀÔÀÇ·Ú Header Table
  CALL FUNCTION 'ZIM_GET_REQ_DOC_HEADER'
       EXPORTING
            ZFREQNO         = ZSREQHD-ZFREQNO
            ZFAMDNO         = W_AMDNO
       IMPORTING
            W_ZTREQHD       = ZTREQHD
            W_ZTREQST       = ZTREQST
       TABLES
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_ORG = IT_ZTREQORJ_ORG
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_ORG  = IT_ZSREQIL_ORG
       EXCEPTIONS
            NOT_FOUND       = 4
            NOT_INPUT       = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E018 WITH ZSREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.

*  IF ZTREQHD-ZFREQTY EQ 'TT' AND SY-TCODE(4) EQ 'ZIM1'.
*     MESSAGE E214 WITH ZSREQHD-ZFREQNO.
*  ENDIF.

* P/O Header Select
  SELECT SINGLE * FROM EKKO WHERE EBELN EQ ZTREQHD-EBELN.
*----------------------------------------------------------------------
* ¸±¸®Áî Ã¼Å©.
*----------------------------------------------------------------------
*  IF NOT ( EKKO-FRGKE EQ '2' OR EKKO-FRGKE EQ SPACE ).
*> < 2002.11.05 NHJ ÁÖ¼®Ã³¸® >
*  IF EKKO-FRGRL EQ 'X'.
*    MESSAGE E390(ME) WITH ZSREQHD-EBELN.
*  ENDIF.
* CLOSE ±¸ºÐ Ã¼Å©.
  IF ZTREQHD-ZFCLOSE NE SPACE.
    IF NOT ZTREQHD-ZFSUBYN IS INITIAL.
      PERFORM   GET_DD07T_SELECT USING      'ZESUBYN'
                                            ZTREQHD-ZFSUBYN
                                 CHANGING   W_TEXT40.
      IF W_STATUS EQ C_REQ_D.
        MESSAGE W254 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_TEXT40.
      ELSE.
        MESSAGE E254 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_TEXT40.
      ENDIF.
    ELSE.
      IF W_STATUS EQ C_REQ_D.
        MESSAGE W255 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
      ELSE.
        MESSAGE E255 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
      ENDIF.
    ENDIF.
  ENDIF.

* ¼öÀÔ¹®¼­ÀÇ ¸±¸®Áî(½ÂÀÎ) ¿©ºÎ¸¦ °ËÁõÇÔ.( ¸±¸®Áî¸¸ ÁøÇà °¡´É )
  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE E963.
  ENDIF.

*>> ºñ¿ëDOCUMENT SELECT.
  IF ( SY-TCODE(4) EQ 'ZIM0' OR SY-TCODE(4) EQ 'ZIM1' ) AND
       ZTIMIMG00-ZFPSMS EQ '2'.
    W_ZFIMDNO = ZTREQHD-ZFREQNO.
    CALL FUNCTION 'ZIM_GET_COST_DOCUMENT'
         EXPORTING
              ZFCSTGRP    = '003'
              ZFIMDNO     = W_ZFIMDNO
         TABLES
              IT_ZSIMCOST = IT_ZSIMCOST.
  ENDIF.
* BUSINESS TRANSACTION TYPE ( ³»ÀÚ±¸¸Å )
*>>KSB CHANGE
* IF EKKO-ZZBUSTYPE EQ 'D'.
*   MESSAGE E148 WITH EKKO-ZZBUSTYPE.
* ENDIF.

* L/C ºñ¿ëÀÏ °æ¿ì...
  IF SY-TCODE EQ 'ZIMC1'.
    W_READ_CHK = 'Y'.
  ENDIF.


* ¹®¼­ »óÅÂ Ã¼Å©.
  CASE SY-TCODE.
    WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
      PERFORM   P2000_REQ_DOC_STATUS_CHECK.
    WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
      PERFORM   P2000_AMEND_DOC_STATUS_CHECK.
    WHEN OTHERS.
*        EXIT.
  ENDCASE.

  CLEAR : ZTIMIMG11.
  SELECT SINGLE * FROM ZTIMIMG11
         WHERE    BUKRS  EQ   ZTREQHD-BUKRS.

*>> ºñ¿ëÀÌ ¾Æ´Ò °æ¿ì...
  IF SY-TCODE NE 'ZIMC1'.
* ¼öÀÔ Ç°¸ñ ITEM SELECT( ZTREQIT )
    CALL FUNCTION 'ZIM_GET_REQ_DOC_ITEM'
        EXPORTING
           EBELN            =   ZTREQHD-EBELN
           KNUMV            =   EKKO-KNUMV
*            KPOSN            =   '000000'
           KSCHL            =   ZTIMIMG00-ZFKSCHL3  ">Installing CHG.
           ZFREQNO          =   ZSREQHD-ZFREQNO
        IMPORTING
           W_ITEM_CNT      =   W_ITEM_CNT
           W_TOT_AMOUNT    =   W_TOT_AMOUNT
        TABLES
           IT_ZSREQIT      =   IT_ZSREQIT
           IT_ZSREQIT_ORG  =   IT_ZSREQIT_ORG
        EXCEPTIONS
           NOT_FOUND      =   1
           NOT_INPUT      =   2
           NO_REFERENCE   =   3
           NO_AMOUNT      =   4.

    CASE SY-SUBRC.
      WHEN 1.    MESSAGE W026 WITH ZSREQHD-ZFREQNO.
      WHEN 2.    MESSAGE E019.
      WHEN 3.    MESSAGE W026 WITH ZSREQHD-ZFREQNO.
      WHEN 4.    MESSAGE W027 WITH ZSREQHD-ZFREQNO.
    ENDCASE.
* Insurance Data Select
    PERFORM   P1000_INSURANCE_READ.
  ENDIF.

* Lock Object Set
  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
      W_STATUS EQ C_OPEN_D ).
    PERFORM P2000_SET_REQDOC_LOCK    USING    'L'   W_AMDNO.
  ENDIF.
*----------------------------------------------------------------------
* º¯°æ³»¿ª È®ÀÎÀ» À§?
*----------------------------------------------------------------------
   *ZTREQHD = ZTREQHD.
   *ZTREQST = ZTREQST.
  REFRESH : IT_ZSREQIT_OLD, IT_ZSREQIL_OLD, IT_ZTREQORJ_OLD.

  IT_ZSREQIT_OLD[]  = IT_ZSREQIT[].
  IT_ZTREQORJ_OLD[] = IT_ZTREQORJ[].
  IT_ZSREQIL_OLD[]  = IT_ZSREQIL[].

  IF  SY-TCODE EQ 'ZIM11'.
* TEMP TABLES MOVE
    PERFORM   P2000_TEMP_AREA_MOVE.
  ELSE.
    W_ZFAMDNO = ZTREQST-ZFAMDNO - 1.

    SELECT SINGLE * INTO ZTREQHD_TMP
                    FROM ZTREQHD_TMP WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                                     AND   ZFAMDNO EQ W_ZFAMDNO.

    REFRESH : IT_ZTREQORJ_TMP.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTREQORJ_TMP
             FROM ZTREQORJ_TMP
             WHERE  ZFREQNO      EQ   ZTREQHD-ZFREQNO
             AND    ZFAMDNO      EQ   W_ZFAMDNO.

    REFRESH : IT_ZSREQIT_TMP.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQIT_TMP
             FROM ZTREQIT_TMP
             WHERE  ZFREQNO      EQ   ZTREQHD-ZFREQNO
             AND    ZFAMDNO      EQ   W_ZFAMDNO.

    REFRESH : IT_ZSREQIL_TMP.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQIL_TMP
             FROM ZTREQIL_TMP
             WHERE  ZFREQNO      EQ   ZTREQHD-ZFREQNO
             AND    ZFAMDNO      EQ   W_ZFAMDNO.
  ENDIF.

  IF ZTREQST-ZFAMDNO IS INITIAL.
    ZSREQHD-ZFOLDAMT = ZTREQST-ZFOPAMT.
  ELSE.
    ZSREQHD-ZFOLDAMT = ZTREQHD_TMP-ZFLASTAM.
  ENDIF.
  ZSREQHD-WAERS    = ZTREQST-WAERS.

  W_ZFREQTY = ZTREQHD-ZFREQTY.
  W_COUNT = 1.

ENDFORM.                    " P1000_READ_REQ_DOC
*&---------------------------------------------------------------------
*&      Form  P2000_SET_REQ_SCR
*&---------------------------------------------------------------------
FORM P2000_SET_REQ_SCR.
  CASE SY-TCODE.
    WHEN 'ZIM01' OR 'ZIM11' OR 'ZIM41' OR 'ZIM46' OR 'ZIML1'.
      W_STATUS = C_REQ_C.
    WHEN 'ZIM02' OR 'ZIM12' OR 'ZIM42' OR 'ZIM47' OR 'ZIML2'.
      W_STATUS = C_REQ_U.
    WHEN 'ZIM03' OR 'ZIM13' OR 'ZIM43' OR 'ZIM48' OR 'ZIML3'.
      W_STATUS = C_REQ_D.
    WHEN 'ZIM05' OR 'ZIM15'.
      W_STATUS = C_ADD_U.
    WHEN 'ZIM07' OR 'ZIM17'.
      W_STATUS = C_OPEN_C.
  ENDCASE.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.

          IF W_STATUS EQ C_ADD_U.
            MOVE : '5'     TO ZTMLCHD-ZFLCTY,   " L/C Type
                   '9'     TO ZTMLCHD-ZFEDFN.   " Electronic Document.
            IF IT_ZSMLCSG7G[] IS INITIAL.
              PERFORM   P3000_SET_ITEM_MARK   USING   1     'X'.
              PERFORM   P3000_GOOD_DESC_CREATE.
              PERFORM   P3000_SET_ITEM_MARK   USING   1     ''.
            ENDIF.
          ENDIF.

          SET SCREEN 0101.  LEAVE TO SCREEN 0101.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          SET SCREEN 1101.  LEAVE TO SCREEN 1101.
      ENDCASE.
    WHEN 'LO'.
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          SET SCREEN 0111.  LEAVE TO SCREEN 0111.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          SET SCREEN 1111.  LEAVE TO SCREEN 1111.
        WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
          PERFORM   P2000_INIT_OFFER_DATA_MOVE.
          SET SCREEN 2101.  LEAVE TO SCREEN 2101.
      ENDCASE.
    WHEN 'PU'.
      SET SCREEN 0121.  LEAVE TO SCREEN 0121.
*-----------------------------------------------------------------------
* 2000/06/22 EDI ¹Ì»ç¿ë ¹× AMEND PROCESS¸¦ À§ÇØ ¸·?
    WHEN 'TT'.
      SET SCREEN 0141.  LEAVE TO SCREEN 0141.
*-----------------------------------------------------------------------
*    WHEN 'DP' OR 'DA' OR 'TT' OR 'GS'.
    WHEN 'DP' OR 'DA' OR 'GS'.
      CASE SY-TCODE.
        WHEN OTHERS.
          SET SCREEN 0131.  LEAVE TO SCREEN 0131.
      ENDCASE.
    WHEN OTHERS.
      CASE SY-TCODE.
        WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
          SET SCREEN 0101.  LEAVE TO SCREEN 0101.
        WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
          SET SCREEN 1101.  LEAVE TO SCREEN 1101.
      ENDCASE.
  ENDCASE.

ENDFORM.                    " P2000_SET_REQ_SCR
*&---------------------------------------------------------------------
*&      Form  P3000_LC_MODIFY
*&---------------------------------------------------------------------
FORM P3000_LC_MODIFY.

  IF  W_OK_CODE NE 'DELE'.
* EDI TEXT Configuration Get
    SELECT SINGLE * FROM ZTIMIMGTX WHERE BUKRS EQ ZTREQHD-BUKRS.

* DATA MOVE
    PERFORM   P2000_LC_SAVE_DATA_MOVE.
*----------------------------------------------------------------------
* Latest Opened Amount Auto Update
*----------------------------------------------------------------------
    PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

    " Open Amount & P/O Amount Comparison
    CLEAR : EKKO, KONV, W_PO_AMOUNT.
    SELECT SINGLE * FROM EKKO WHERE EBELN EQ ZTREQHD-EBELN.
    SELECT * FROM KONV
    WHERE  KNUMV  EQ  EKKO-KNUMV
    AND    KNTYP  EQ  'H'.
       W_PO_AMOUNT = W_PO_AMOUNT + KONV-KAWRT.
    ENDSELECT.
    IF W_PO_AMOUNT NE ZTREQHD-ZFLASTAM.
       MESSAGE W013(ZIM1).
    ENDIF.

*----------------------------------------------------------------------
* DATA MOVE
*----------------------------------------------------------------------
    CASE  ZTREQHD-ZFREQTY.
      WHEN 'LC'.           " IMPORT MASTER L/C
        PERFORM   P3000_MASTER_LC_MOVE.
      WHEN 'LO'.           " LOCAL L/C
        PERFORM   P3000_LOCAL_LC_MOVE.
      WHEN 'PU'.           " Purchase Approve
        PERFORM   P3000_PURCH_DOC_MOVE.
      WHEN 'TT'.           " T/T
        PERFORM   P3000_PAYORD_DOC_MOVE.
      WHEN OTHERS.
    ENDCASE.
*----------------------------------------------------------------------
* Data Check
*----------------------------------------------------------------------
*>> NCW ¼öÁ¤ - 2003.12.05 ÀúÀå½Ã EDI ÇÊµå Ã¼Å© ¾ÈÇÔ
    IF ZTIMIMG00-ZFRELYN1 EQ 'X'.
      IF W_STATUS EQ C_ADD_U OR W_STATUS EQ C_OPEN_C.
*        IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
*          CASE  ZTREQHD-ZFREQTY.
*            WHEN 'LC'.           " IMPORT MASTER L/C
*              IF ZTIMIMGTX-APP700 EQ 'X'.
*                PERFORM   P2000_MASTER_LC_CHECK.
*              ENDIF.
*            WHEN 'LO'.           " LOCAL L/C
*              IF ZTIMIMGTX-LOCAPP EQ 'X'.
*                PERFORM   P2000_LOCAL_LC_CHECK.
*              ENDIF.
*            WHEN 'PU'.           " ±¸¸Å½ÂÀÎ?
*              IF ZTIMIMGTX-APPPUR EQ 'X'.
*                PERFORM   P2000_PURCH_DOC_CHECK.
*              ENDIF.
**          WHEN 'TT'.           " Áö±ÞÁö½Ã?
**            PERFORM   P2000_PAYORD_DOC_CHECK.
*            WHEN 'DA' OR 'DP' OR 'GS'. " D/A or D/P or GSM

*          ENDCASE.
*        ENDIF.
      ELSEIF W_STATUS EQ C_REQ_C OR W_STATUS EQ C_REQ_U.
        IF ZTREQHD-ZFREQTY EQ 'LC'.
          W_LINE = 0.
          DESCRIBE TABLE IT_ZTREQORJ LINES W_LINE.
          IF W_LINE LE 0.
            MESSAGE E167 WITH 'Origin'.   EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
*      IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
*        CASE  ZTREQHD-ZFREQTY.
*          WHEN 'LC'.           " IMPORT MASTER L/C
*            IF ZTIMIMGTX-APP700 EQ 'X'.
*              PERFORM   P2000_MASTER_LC_CHECK.
*            ENDIF.
*          WHEN 'LO'.           " LOCAL L/C
*            IF ZTIMIMGTX-LOCAPP EQ 'X'.
*              PERFORM   P2000_LOCAL_LC_CHECK.
*            ENDIF.
*          WHEN 'PU'.
*            IF ZTIMIMGTX-APPPUR EQ 'X'.
*              PERFORM   P2000_PURCH_DOC_CHECK.
*            ENDIF.
*        WHEN 'TT'.
*          PERFORM   P2000_PAYORD_DOC_CHECK.
*          WHEN 'DA' OR 'DP' OR 'GS'. " D/A or D/P or GSM
*        ENDCASE.
*      ENDIF.
    ENDIF.
  ENDIF.

*>> Receipt Date, Created by
  IF W_STATUS  EQ  C_REQ_C.
    SELECT  SINGLE * FROM ZTIMIMG00.
    IF ZTIMIMG00-ZFRELYN1 NE 'X' .
      MOVE  ZTREQST-ZFREQDT  TO  ZTREQST-ZFAPPDT.
      MOVE  SY-DATUM         TO  ZTREQST-ZFRVDT.
      MOVE  SY-UNAME         TO  ZTREQST-ZFOPNNM.
    ENDIF.
    IF ZTIMIMG00-ZFRELYN1 NE 'X' AND ZTIMIMG00-ZFRELYN2 NE 'X'.
      IF ZTREQHD-ZFREQTY EQ 'TT' OR ZTREQHD-ZFREQTY EQ 'DA' OR
         ZTREQHD-ZFREQTY EQ 'DP'.

        IF ZTREQHD-ZFREQTY EQ 'TT'.
          CONCATENATE 'T' ZTREQHD-ZFBACD INTO W_TYPE.
        ELSE.
          W_TYPE = ZTREQHD-ZFREQTY.
        ENDIF.

        W_ZFOPNNO_TMP = SY-DATUM+2(4).
        CONCATENATE W_TYPE W_ZFOPNNO_TMP W_ZZBUSTYPE '%'
                    INTO  W_ZFOPNNO_TMP.
        SELECT MAX( ZFOPNNO ) INTO W_ZFOPNNO FROM ZTREQST
                              WHERE ZFOPNNO LIKE W_ZFOPNNO_TMP.

        IF W_ZFOPNNO IS INITIAL.
          W_ZFOPNNO_TMP+6(5) = '00001'.
        ELSE.
          W_SEQ = W_ZFOPNNO+6(5).
          W_SEQ = W_SEQ + 1.
          W_SEQ_TMP = W_SEQ.
          CONCATENATE W_ZFOPNNO(6) W_SEQ_TMP
                 INTO W_ZFOPNNO_TMP.
        ENDIF.
        ZTREQST-ZFOPNNO = W_ZFOPNNO_TMP.
        ZTREQHD-ZFOPNNO = W_ZFOPNNO_TMP.
        ZTREQST-ZFOPNDT = ZTREQST-ZFAPPDT.
        ZTREQST-ZFDOCST = 'O'.
      ENDIF.
    ENDIF.
  ENDIF.
*----------------------------------------------------------------------
  IF W_STATUS EQ C_REQ_C.
    PERFORM   P2000_GET_NUMBER_NEXT  USING ZTREQHD-ZFREQTY
                                           ZTREQHD-ZFREQNO.
  ENDIF.

  CASE  ZTREQHD-ZFREQTY.
    WHEN 'LC'.           " IMPORT MASTER L/C
      PERFORM   P3000_MASTER_LC_MODIFY.
    WHEN 'LO'.                                              " LOCAL L/C
      PERFORM   P3000_LOCAL_LC_MODIFY.
    WHEN 'PU'.
      PERFORM   P3000_PURCH_DOC_MODIFY.
*-----------------------------------------------------------------------
    WHEN 'TT'.
      PERFORM   P3000_PAYORD_DOC_MODIFY.
*-----------------------------------------------------------------------
    WHEN 'DA' OR 'DP' OR 'GS'.   " D/A or D/P or T/T
      PERFORM   P3000_IMPORT_DOC_MODIFY.
  ENDCASE.

  SET PARAMETER ID 'ZPREQNO' FIELD ZTREQHD-ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO' FIELD ZTREQST-ZFAMDNO.

  CASE W_OK_CODE.
    WHEN 'DELE'.
      PERFORM  P3000_FLAT_DATA_DELETE   USING  ZTREQST-ZFDOCNO.
      MESSAGE  S016  WITH  ZTREQHD-ZFREQNO 'Deleted'.
    WHEN 'REVK'.
      PERFORM  P3000_FLAT_DATA_DELETE   USING  ZTREQST-ZFDOCNO.
      MESSAGE  S016  WITH  ZTREQHD-ZFREQNO 'canceled FLAT DB'.
    WHEN 'DELR'.
      MESSAGE  S016  WITH  ZTREQHD-ZFREQNO 'canceled receipt'.
    WHEN OTHERS.
      MESSAGE  S016  WITH  ZTREQHD-ZFREQNO 'saved'.
  ENDCASE.

ENDFORM.                    " P3000_LC_MODIFY
*&---------------------------------------------------------------------
*&      Form  P3000_ZTINS_MODIFY
*&---------------------------------------------------------------------
*
FORM P3000_ZTINS_MODIFY.

*----------------------------------------------------------------------
* ¼öÀÛ¾÷ °³¼³ÀÏ °æ¿ì, Status º¯?
*----------------------------------------------------------------------
  IF W_STATUS EQ C_OPEN_C.
    IF W_NEW_DOCST NE 'N'.        " OPEN Ãë¼Ò°¡ ¾Æ´Ò °æ¿ì.
      ZTINS-ZFDOCST = 'O'.
    ENDIF.
    W_NEW_DOCST =  'O'.
  ENDIF.

  IF W_NEW_DOCST EQ 'N'.           " OPEN Ãë¼ÒÀÏ °æ¿ì.
    CLEAR : ZTINS-ZFINNO,    ZTINSRSP-ZFISDT, ZTINS-ZFINRT,
            ZTINSRSP-ZFEXRT, ZTINS-ZFINAMT,   ZTINS-ZFINAMTC,
            ZTINS-ZFKRWAMT,  ZTINSRSP-ZFTAMI, ZTINSRSP-ZFTAMIC,
            ZTINSRSP-ZFCAMI, ZTINSRSP-ZFCAMIC, ZTINSRSP-ZFDAMI,
            ZTINSRSP-ZFDAMIC, ZTINSRSP-ZFIPR,  ZTINSRSP-ZFIPRC,
            ZTINSRSP-ZFTPR,   ZTINSRSP-ZFTPRC, ZTINSRSP-ZFCPR,
            ZTINSRSP-ZFCPRC,  ZTINSRSP-ZFDPR,  ZTINSRSP-ZFDPRC,
            ZTINSRSP-ZFVPR,   ZTINSRSP-ZFVPRC.
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
    IF ZTREQHD-ZFLASTAM NE ZTINS-ZFIVAMT.
      MESSAGE W217.
    ENDIF.
  ENDIF.

  IF SY-TCODE EQ 'ZIM41' OR SY-TCODE EQ 'ZIM42' OR SY-TCODE EQ 'ZIM43'
     OR SY-TCODE EQ 'ZIM44'.
    CLEAR : W_AMEND_MARK.
  ELSE.
    W_AMEND_MARK = 'X'.
  ENDIF.

* »ý¼ºÀÏ °æ¿ì...
  IF W_STATUS EQ C_REQ_C.
    IF W_AMEND_MARK EQ 'X'.
      SELECT MAX( ZFAMDNO ) INTO ZTINS-ZFAMDNO  FROM ZTINS
                            WHERE ZFREQNO EQ ZTINS-ZFREQNO
                            AND   ZFINSEQ EQ ZTINS-ZFINSEQ.
      IF ZTINS-ZFAMDNO IS INITIAL.
        ZTINS-ZFAMDNO = '00001'.
      ELSE.
        ZTINS-ZFAMDNO = ZTINS-ZFAMDNO + 1.
      ENDIF.
    ELSE.
*>> º¸Çè¹®¼­ »ý¼º½Ã... --> È½Â÷ Áõ°¡..
      IF ZTINS-ZFINSEQ IS INITIAL.
        SELECT MAX( ZFINSEQ ) INTO ZTINS-ZFINSEQ
               FROM ZTINS
               WHERE ZFREQNO  EQ  ZTINS-ZFREQNO.
        ZTINS-ZFINSEQ = ZTINS-ZFINSEQ + 1.
      ENDIF.
      ZTINS-ZFAMDNO = '00000'.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZIM_INSURANCE_MODIFY'
       EXPORTING
            W_OK_CODE      = W_OK_CODE
            ZFREQNO        = ZTINS-ZFREQNO
            ZFINSEQ        = ZTINS-ZFINSEQ
            ZFAMDNO        = ZTINS-ZFAMDNO
            ZFSTATUS       = W_STATUS
            W_ZTINS        = ZTINS
            W_ZTINS_OLD    = *ZTINS
            W_ZTINSRSP     = ZTINSRSP
            W_ZTINSRSP_OLD = *ZTINSRSP
            W_ZTINSSG3     = ZTINSSG3
            W_ZTINSSG3_OLD = *ZTINSSG3
            W_AMEND        = W_AMEND_MARK
       TABLES
            IT_ZSINSAGR    = IT_ZSINSAGR
            IT_ZSINSSG2    = IT_ZSINSSG2
            IT_ZSINSSG5    = IT_ZSINSSG5
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E168.
  ELSE.
    CASE W_OK_CODE.
      WHEN 'DELE'.
        PERFORM  P3000_FLAT_DATA_DELETE   USING  ZTINS-ZFDOCNO.
        MESSAGE  S170  WITH  ZTINS-ZFREQNO ZTINS-ZFINSEQ
                                           ZTINS-ZFAMDNO 'delete'.
      WHEN 'REVK'.
* FLAT DATA DELETE
        PERFORM  P3000_FLAT_DATA_DELETE   USING  ZTINS-ZFDOCNO.
        MESSAGE  S170  WITH  ZTINS-ZFREQNO ZTINS-ZFINSEQ
                                           ZTINS-ZFAMDNO
                             'FLAT DB cancel'.
      WHEN 'DELR'.
        MESSAGE  S170  WITH  ZTINS-ZFREQNO ZTINS-ZFINSEQ
                                           ZTINS-ZFAMDNO
                             'registration cancel'.
      WHEN OTHERS.
        MESSAGE  S170  WITH  ZTINS-ZFREQNO ZTINS-ZFINSEQ
                                           ZTINS-ZFAMDNO 'save'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P3000_ZTINS_MODIFY

*&---------------------------------------------------------------------
*
*&      Form  P3000_MASTER_LC_MODIFY
*&---------------------------------------------------------------------
*
FORM P3000_MASTER_LC_MODIFY.

  CALL FUNCTION 'ZIM_MASTER_LC_MODIFY'
       EXPORTING
            W_OK_CODE        = W_OK_CODE
            ZFREQNO          = ZTREQHD-ZFREQNO
            ZFAMDNO          = ZTREQST-ZFAMDNO
            ZFSTATUS         = W_STATUS
            W_ZTREQHD        = ZTREQHD
            W_ZTREQHD_OLD    = *ZTREQHD
            W_ZTREQST        = ZTREQST
            W_ZTREQST_OLD    = *ZTREQST
            W_ZTMLCHD        = ZTMLCHD
            W_ZTMLCHD_OLD    = *ZTMLCHD
            W_ZTMLCSG2       = ZTMLCSG2
            W_ZTMLCSG2_OLD   = *ZTMLCSG2
            W_ZTMLCSG910     = ZTMLCSG910
            W_ZTMLCSG910_OLD = *ZTMLCSG910
       TABLES
            IT_ZSMLCSG7G     = IT_ZSMLCSG7G
            IT_ZSMLCSG7O     = IT_ZSMLCSG7O
            IT_ZSMLCSG8E     = IT_ZSMLCSG8E
            IT_ZSMLCSG9O     = IT_ZSMLCSG9O
            IT_ZSREQIT       = IT_ZSREQIT
            IT_ZSREQIT_OLD   = IT_ZSREQIT_OLD
            IT_ZTREQORJ      = IT_ZTREQORJ
            IT_ZTREQORJ_OLD  = IT_ZTREQORJ_OLD
            IT_ZSREQIL       = IT_ZSREQIL
            IT_ZSREQIL_OLD   = IT_ZSREQIL_OLD
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E017.
  ENDIF.

ENDFORM.                    " P3000_MASTER_LC_MODIFY

*&---------------------------------------------------------------------
*
*&      Form  P3000_LOCAL_LC_MODIFY
*&---------------------------------------------------------------------
*
FORM P3000_LOCAL_LC_MODIFY.

  CALL FUNCTION 'ZIM_LOCAL_LC_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFREQNO         = ZTREQHD-ZFREQNO
            ZFAMDNO         = ZTREQST-ZFAMDNO
            ZFSTATUS        = W_STATUS
            W_ZTREQHD       = ZTREQHD
            W_ZTREQHD_OLD   = *ZTREQHD
            W_ZTREQST       = ZTREQST
            W_ZTREQST_OLD   = *ZTREQST
            W_ZTLLCHD       = ZTLLCHD
            W_ZTLLCHD_OLD   = *ZTLLCHD
            W_ZTLLCSG23     = ZTLLCSG23
            W_ZTLLCSG23_OLD = *ZTLLCSG23
       TABLES
            IT_ZSLLCOF      = IT_ZSLLCOF
            IT_ZSREQIT      = IT_ZSREQIT
            IT_ZSREQIT_OLD  = IT_ZSREQIT_OLD
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_OLD = IT_ZTREQORJ_OLD
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_OLD  = IT_ZSREQIL_OLD
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E017.
  ENDIF.

  IF W_OK_CODE EQ 'DELE' OR W_OK_CODE EQ 'REVK'.
    SELECT SINGLE * FROM ZTOFF
                    WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.
    IF SY-SUBRC EQ 0.
      DISP_MODE = 'N'.
      IF ZTOFF-ZFDOCST NE 'N'.
*-----------------------------------------------------------------------
* LOCAL OFFER SHEET FLAT DATA DELETE
*-----------------------------------------------------------------------
        REFRESH : BDCDATA.
* ÃÊ±âÈ­¸é CALL
        PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '2300'.
* ÃÊ±âÈ­¸é FIELD
        PERFORM P2000_DYNPRO USING :
           ' ' 'ZSREQHD-EBELN'   '',                  " P/O No.
           ' ' 'ZSREQHD-ZFREQNO' ZTREQHD-ZFREQNO,     " Import No.
           ' ' 'BDC_OKCODE'      '/00'.                     " ENTER
* ÁÖÈ­¸é   CALL
        PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '2101'.
* Àú?
        PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'REVK'.
* ÀúÀå È®ÀÎ CALL
        PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '0001'.
* Àú?
        PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'YES'.
* CALL TRANSACTION

        PERFORM P2000_CALL_TRANSACTION  USING 'ZIML3'
                                        CHANGING  W_SUBRC.
      ENDIF.
*-----------------------------------------------------------------------
* LOCAL OFFER SHEET DELETE
*-----------------------------------------------------------------------
      REFRESH : BDCDATA.
* ÃÊ±âÈ­¸é CALL
      PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '2300'.
* ÃÊ±âÈ­¸é FIELD
      PERFORM P2000_DYNPRO USING :
         ' ' 'ZSREQHD-EBELN'   '',                  " P/O No.
         ' ' 'ZSREQHD-ZFREQNO' ZTREQHD-ZFREQNO,     " Import No.
         ' ' 'BDC_OKCODE'      '/00'.                       " ENTER
* ÁÖÈ­¸é   CALL
      PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '2101'.
* Àú?
      PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'DELE'.
* ÀúÀå È®ÀÎ CALL
      PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '0001'.
* Àú?
      PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'YES'.
* CALL TRANSACTION
      PERFORM P2000_CALL_TRANSACTION  USING 'ZIML3'
                                      CHANGING  W_SUBRC.
      IF W_SUBRC NE 0.
        MESSAGE I297 WITH ZTREQHD-ZFREQNO.
      ELSE.
        MESSAGE I298 WITH ZTREQHD-ZFREQNO.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_LOCAL_LC_MODIFY
*&---------------------------------------------------------------------
*
*&      Form  P1000_READ_LOCAL_LC
*&---------------------------------------------------------------------
*
FORM P1000_READ_LOCAL_LC.
  CALL FUNCTION 'ZIM_GET_LOCAL_LC_DATA'
       EXPORTING
            ZFREQNO        = ZTREQHD-ZFREQNO
       IMPORTING
            W_ZTLLCHD      = ZTLLCHD
            W_ZTLLCSG23    = ZTLLCSG23
       TABLES
            IT_ZSLLCOF     = IT_ZSLLCOF
            IT_ZSLLCOF_ORG = IT_ZSLLCOF_ORG
       EXCEPTIONS
            NOT_FOUND      = 4
            NOT_INPUT      = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E018 WITH ZTREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.
*----------------------------------------------------------------------
* º¯°æ³»¿ª È®ÀÎÀ» À§?
*----------------------------------------------------------------------
   *ZTLLCHD    = ZTLLCHD.
   *ZTLLCSG23  = ZTLLCSG23.

ENDFORM.                    " P1000_READ_LOCAL_LC
*&---------------------------------------------------------------------
*&      Form  P3000_IMPORT_DOC_MODIFY
*&---------------------------------------------------------------------
FORM P3000_IMPORT_DOC_MODIFY.

  CALL FUNCTION 'ZIM_OTHER_DOC_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFREQNO         = ZTREQHD-ZFREQNO
            ZFAMDNO         = ZTREQST-ZFAMDNO
            ZFSTATUS        = W_STATUS
            W_ZTREQHD       = ZTREQHD
            W_ZTREQHD_OLD   = *ZTREQHD
            W_ZTREQST       = ZTREQST
            W_ZTREQST_OLD   = *ZTREQST
       TABLES
            IT_ZSREQIT      = IT_ZSREQIT
            IT_ZSREQIT_OLD  = IT_ZSREQIT_OLD
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_OLD  = IT_ZSREQIL_OLD
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_OLD = IT_ZTREQORJ_OLD
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E017.
  ENDIF.

ENDFORM.                    " P3000_IMPORT_DOC_MODIFY
*&---------------------------------------------------------------------
*&      Form  P1000_READ_PURCH_DOC
*&---------------------------------------------------------------------
FORM P1000_READ_PURCH_DOC.
  CALL FUNCTION 'ZIM_GET_PURCH_DOC_DATA'
       EXPORTING
            ZFREQNO          = ZTREQHD-ZFREQNO
            ZFAMDNO          = ZTREQST-ZFAMDNO
       IMPORTING
            W_ZTPUR          = ZTPUR
       TABLES
            IT_ZSPURSG1      = IT_ZSPURSG1
            IT_ZSPURSG1G     = IT_ZSPURSG1G
            IT_ZSPURSG4      = IT_ZSPURSG4
            IT_ZSPURSG1_ORG  = IT_ZSPURSG1_ORG
            IT_ZSPURSG1G_ORG = IT_ZSPURSG1G_ORG
            IT_ZSPURSG4_ORG  = IT_ZSPURSG4_ORG
       EXCEPTIONS
            NOT_FOUND        = 4
            NOT_INPUT        = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E054 WITH ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.
*----------------------------------------------------------------------
* º¯°æ³»¿ª È®ÀÎÀ» À§?
*----------------------------------------------------------------------
   *ZTPUR    = ZTPUR.

ENDFORM.                    " P1000_READ_PURCH_DOC
*&---------------------------------------------------------------------
*&      Form  P3000_PURCH_DOC_MODIFY
*&---------------------------------------------------------------------
*
FORM P3000_PURCH_DOC_MODIFY.

  CALL FUNCTION 'ZIM_PURCH_DOC_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFREQNO         = ZTREQHD-ZFREQNO
            ZFAMDNO         = ZTREQST-ZFAMDNO
            ZFSTATUS        = W_STATUS
            W_ZTREQHD       = ZTREQHD
            W_ZTREQHD_OLD   = *ZTREQHD
            W_ZTREQST       = ZTREQST
            W_ZTREQST_OLD   = *ZTREQST
            W_ZTPUR         = ZTPUR
            W_ZTPUR_OLD     = *ZTPUR
       TABLES
            IT_ZSPURSG1     = IT_ZSPURSG1
            IT_ZSPURSG1G    = IT_ZSPURSG1G
            IT_ZSPURSG4     = IT_ZSPURSG4
            IT_ZSREQIT      = IT_ZSREQIT
            IT_ZSREQIT_OLD  = IT_ZSREQIT_OLD
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_OLD = IT_ZTREQORJ_OLD
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_OLD  = IT_ZSREQIL_OLD
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E017.
  ENDIF.

ENDFORM.                    " P3000_PURCH_DOC_MODIFY
*&---------------------------------------------------------------------
*
*&      Form  P2000_SET_DISABLE_MENU
*&---------------------------------------------------------------------
*
FORM P2000_SET_DISABLE_MENU.

  IF W_STATUS EQ 'D'.
    MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL.           " DELETE
    MOVE 'ANZG' TO IT_EXCL-FCODE. APPEND IT_EXCL.           " DISPLAY
    MOVE 'MKAL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SELECT ALL
    MOVE 'MKLO' TO IT_EXCL-FCODE. APPEND IT_EXCL.           " DESELECT
    MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL.           " DELETE
    MOVE 'DELC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE CANCE
    MOVE 'SAVE' TO IT_EXCL-FCODE. APPEND IT_EXCL.           " SAVE
  ELSEIF W_STATUS EQ 'C'.
    IF SY-TCODE EQ 'ZIMG02' OR SY-TCODE EQ 'ZIMG03'.
      MOVE 'MKAL' TO IT_EXCL-FCODE. APPEND IT_EXCL. " SELECT ALL
      MOVE 'MKLO' TO IT_EXCL-FCODE. APPEND IT_EXCL.         " DESELECT
      MOVE 'DELE' TO IT_EXCL-FCODE. APPEND IT_EXCL.         " DELETE
      MOVE 'DELC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DEL CANCEL
    ENDIF.
    MOVE 'NEWL' TO IT_EXCL-FCODE. APPEND IT_EXCL.           " NEW ENTRY
    MOVE 'AEND' TO IT_EXCL-FCODE. APPEND IT_EXCL.           " CHANGE
  ELSEIF W_STATUS EQ 'I'.
*     MOVE 'DELT' TO IT_EXCL-FCODE. APPEND IT_EXCL. " DELETE
    MOVE 'NEWL' TO IT_EXCL-FCODE. APPEND IT_EXCL.           " NEW ENTRY
    MOVE 'AEND' TO IT_EXCL-FCODE. APPEND IT_EXCL.           " CHANGE
    MOVE 'CHDC' TO IT_EXCL-FCODE. APPEND IT_EXCL. " CHANGE DOC.
  ENDIF.
*----------------------------------------------------------------------
* PF-STATUS SETTING
*----------------------------------------------------------------------
  SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.

ENDFORM.                    " P2000_SET_DISABLE_MENU
*&---------------------------------------------------------------------
*
*&      Form  P2000_SAVE_PROCESS
*&---------------------------------------------------------------------
*
FORM P2000_SAVE_PROCESS.
* MESSAGE
  W_OK_CODE = OK-CODE.
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.
* POPUP SCREEN
  CASE ANTWORT.
    WHEN 'Y'.                                               " Yes...
      PERFORM  P3000_DATABASE_MODIFY.
      MESSAGE  S953.
      CASE W_OK_CODE.
        WHEN 'ANZG' OR 'SAVE'.   " CHANGE ==> DISPLAY or SAVE
          PERFORM P2000_ZTRECST_LOCK_MODE    USING    'U'.
          MOVE 'D' TO W_STATUS.
        WHEN OTHERS.
*              PERFORM 2000_BACK_SCREEN_DEFINE.
      ENDCASE.
      PERFORM  P1000_DATA_REREAD.                           " DATA READ
    WHEN 'N'.              " No...
      IF W_OK_CODE EQ 'BACK' OR W_OK_CODE EQ 'EXIT'.
        MESSAGE  S957.
        PERFORM 2000_BACK_SCREEN_DEFINE.
      ENDIF.
      CASE W_OK_CODE.
        WHEN 'ANZG'.     " CHANGE ==> DISPLAY
          MOVE 'D' TO W_STATUS.
      ENDCASE.
      PERFORM  P1000_DATA_REREAD.                           " DATA READ
    WHEN 'C'.                                               " Cancel
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SAVE_PROCESS

*&---------------------------------------------------------------------
*
*&      Form  P3000_DATABASE_MODIFY
*&---------------------------------------------------------------------
*
FORM P3000_DATABASE_MODIFY.
  CASE SY-TCODE.
    WHEN 'ZIMC1'.            " ¼öÀÔºñ?
      PERFORM  P3000_WRITE_ZTRECST.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " P3000_DATABASE_MODIFY

*&---------------------------------------------------------------------
*
*&      Form  P3000_WRITE_ZTRECST
*&---------------------------------------------------------------------
*
FORM P3000_WRITE_ZTRECST.
  CLEAR : W_ZFCSQ.

* Internal Table Sort
  SORT IT_ZSRECST    BY ZFCSQ.

* Internal Table Looping( Áßº¹ °ËÁõ )
  LOOP AT IT_ZSRECST WHERE ZFCSQ <> ''.
    IF W_ZFCSQ   EQ IT_ZSRECST-ZFCSQ AND SY-TABIX NE 1.
      MESSAGE E969 WITH W_ZFCSQ.
    ENDIF.

    MOVE : IT_ZSRECST-ZFCSQ   TO W_ZFCSQ,
           SY-TABIX           TO W_TABIX.
* ½Å±ÔÀÏ °æ¿ì DB¿Í °ËÁõ ÀÛ¾÷( Áßº¹ ¿À·ù )
    IF W_STATUS EQ 'I'.
      IF IT_ZSRECST-LOEKZ NE 'X'.
        SELECT SINGLE * FROM ZTRECST
                        WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                        AND   ZFCSQ   EQ IT_ZSRECST-ZFCSQ.
        IF SY-SUBRC EQ 0.
          MESSAGE E969 WITH W_ZFCSQ.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_ZSRECST_DEL.
    IF IT_ZSRECST_DEL-ZFCSTYN = 'X'.
      MESSAGE E470 WITH IT_ZSRECST-ZFCSQ.
    ENDIF.
    DELETE FROM ZTRECST
           WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
           AND   ZFCSQ   EQ IT_ZSRECST_DEL-ZFCSQ.
* CNAHNGE DOCUMENT
    CLEAR : *ZTRECST.
    MOVE-CORRESPONDING IT_ZSRECST_DEL TO  ZTRECST.
    PERFORM   P3000_ZTRECST_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSRECST_DEL.

* µ¥ÀÌÅ¸¸¦ Insert
  LOOP AT IT_ZSRECST.
    CLEAR W_TOTAMT.
    SELECT SINGLE * FROM ZTRECST
                    WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                    AND   ZFCSQ   EQ IT_ZSRECST-ZFCSQ.

    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING ZTRECST      TO *ZTRECST.
      MOVE-CORRESPONDING IT_ZSRECST   TO  ZTRECST.
      MOVE : SY-UNAME         TO   ZTRECST-UNAM,
             SY-DATUM         TO   ZTRECST-UDAT.

      UPDATE ZTRECST.
      IF SY-SUBRC NE 0.    MESSAGE E952.   ENDIF.
* CNAHNGE DOCUMENT
      PERFORM   P3000_ZTRECST_CHANGE_DOC        USING  'U'.
    ELSE.
      IF IT_ZSRECST-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
* È¸»çÄÚµå INSERT.
      IF IT_ZSRECST-BUKRS IS INITIAL.
        SELECT  SINGLE *
        FROM    ZTREQHD
        WHERE   ZFREQNO  =  ZTREQHD-ZFREQNO.

        IF SY-SUBRC EQ 0.
          MOVE ZTREQHD-BUKRS TO IT_ZSRECST-BUKRS.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING IT_ZSRECST TO ZTRECST.
      MOVE : SY-UNAME         TO   ZTRECST-ERNAM,
             SY-DATUM         TO   ZTRECST-CDAT,
             SY-UNAME         TO   ZTRECST-UNAM,
             SY-DATUM         TO   ZTRECST-UDAT,
             SY-MANDT         TO   ZTRECST-MANDT,
             ZTREQHD-ZFREQNO  TO   ZTRECST-ZFREQNO.

      SELECT MAX( ZFCSQ ) INTO ZTRECST-ZFCSQ FROM ZTRECST
                          WHERE ZFREQNO  EQ  ZTREQHD-ZFREQNO.
      IF ZTRECST-ZFCSQ IS INITIAL.
        ZTRECST-ZFCSQ = '00010'.
      ELSE.
        ZTRECST-ZFCSQ = ZTRECST-ZFCSQ + 10.
      ENDIF.
      INSERT ZTRECST.
* CNAHNGE DOCUMENT
      CLEAR : *ZTRECST.
      PERFORM   P3000_ZTRECST_CHANGE_DOC        USING  'I'.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTRECST

*&---------------------------------------------------------------------
*
*&      Form  P1000_DATA_REREAD
*&---------------------------------------------------------------------
*
FORM P1000_DATA_REREAD.

  CASE SY-TCODE.
    WHEN  'ZIMC1'.
      PERFORM  P1000_READ_ZTRECST.    "
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P1000_DATA_REREAD

*&---------------------------------------------------------------------
*
*&      Form  P1000_READ_ZTRECST
*&---------------------------------------------------------------------
*
FORM P1000_READ_ZTRECST.
  CHECK ZTIMIMG00-ZFPSMS EQ '1'.
  CHECK SY-TCODE EQ 'ZIMC1'.

  REFRESH IT_ZSRECST.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSRECST
                              FROM ZTRECST
                              WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.
*                              ORDER BY ZFCSQ.

  IF SY-SUBRC NE 0.
    MESSAGE S967 WITH 'ZTRECST'.
  ENDIF.

  LOOP AT IT_ZSRECST.
    W_TABIX = SY-TABIX.
    PERFORM P2000_GET_ZTIMIMG08_DESC  USING     '003'
                                                IT_ZSRECST-ZFCSCD
                                      CHANGING  IT_ZSRECST-ZFCDNM.

    MODIFY IT_ZSRECST INDEX W_TABIX.
  ENDLOOP.

  REFRESH : IT_ZSRECST_ORG.
  IT_ZSRECST_ORG[] = IT_ZSRECST[].

ENDFORM.                    " P1000_READ_ZTRECST

*&---------------------------------------------------------------------
*
*&      Form  2000_BACK_SCREEN_DEFINE
*&---------------------------------------------------------------------
*
FORM 2000_BACK_SCREEN_DEFINE.
  CASE SY-TCODE.
    WHEN 'ZIMC1'.         " L/C COST
      SET SCREEN 0190.  LEAVE SCREEN.
    WHEN OTHERS.
      SET SCREEN 0.  LEAVE SCREEN.
  ENDCASE.

ENDFORM.                    " 2000_BACK_SCREEN_DEFINE
*&---------------------------------------------------------------------
*
*&      Form  P2000_IT_TAB_REFRESH
*&---------------------------------------------------------------------
*
FORM P2000_IT_TAB_REFRESH.

  CASE SY-TCODE.
    WHEN 'ZIMC1'.
      REFRESH IT_ZSRECST.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " P2000_IT_TAB_REFRESH
*&---------------------------------------------------------------------
*
*&      Form  P2000_DATA_DELETE
*&---------------------------------------------------------------------
*
FORM P2000_DATA_DELETE.
  CASE SY-TCODE.
    WHEN 'ZIMC1'.
      CLEAR W_COUNT.
* ¹Ý¿µµÈ ¹®¼­´Â »èÁ¦¸øÇÔ..( EDI ¹®¼­³ª È¸°èÃ³¸®µÈ ITEM )
      LOOP AT IT_ZSRECST WHERE ZFMARK = 'X'.
        IF NOT IT_ZSRECST-ZFACDO IS INITIAL.
          MESSAGE E237 WITH IT_ZSRECST-ZFCSQ.
        ENDIF.
        IF NOT IT_ZSRECST-ZFDOCNO IS INITIAL.
          MESSAGE E238 WITH IT_ZSRECST-ZFCSQ.
        ENDIF.
* ºñ¿ë¹èºÎµÈ ±Ý¾×Àº »èÁ¦ºÒ°¡.
        IF IT_ZSRECST-ZFCSTYN = 'X'.
          MESSAGE E470 WITH IT_ZSRECST-ZFCSQ.
        ENDIF.
      ENDLOOP.

      LOOP AT IT_ZSRECST WHERE ZFMARK = 'X'.
        IT_ZSRECST_DEL = IT_ZSRECST.
        APPEND IT_ZSRECST_DEL.
        DELETE  IT_ZSRECST.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " P2000_DATA_DELETE
*&---------------------------------------------------------------------
*
*&      Form  P2000_SET_DEL_MARK
*&---------------------------------------------------------------------
*
FORM P2000_SET_DEL_MARK.
  IF OK-CODE EQ 'DELC'.
    W_ROW_MARK  = ''.
  ELSEIF OK-CODE EQ 'DELE'.
    W_ROW_MARK  = 'X'.
  ENDIF.

  CASE SY-TCODE.
    WHEN 'ZIMC1'.
      LOOP AT IT_ZSRECST WHERE ZFMARK = 'X'.
        IT_ZSRECST-LOEKZ  = W_ROW_MARK.
        MODIFY  IT_ZSRECST.
      ENDLOOP.
      IF SY-SUBRC NE 0.   MESSAGE I951.   ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_DEL_MARK
*&---------------------------------------------------------------------
*
*&      Form  P2000_SET_ROW_MARK
*&---------------------------------------------------------------------
*
FORM P2000_SET_ROW_MARK.
  IF OK-CODE EQ 'MKAL'.      " ÀüÃ¼ ¼±?
    W_ROW_MARK = 'X'.
  ELSEIF OK-CODE EQ 'MKLO'.  " ¼±ÅÃ ÇØ?
    CLEAR : W_ROW_MARK.
  ENDIF.
  CASE SY-TCODE.
    WHEN 'ZIMC1'.
      LOOP AT IT_ZSRECST.
        IT_ZSRECST-ZFMARK = W_ROW_MARK.   MODIFY IT_ZSRECST.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " P2000_SET_ROW_MARK
*&---------------------------------------------------------------------
*
*&      Form  SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------
*
FORM SET_CURR_CONV_TO_INTERNAL USING    P_AMOUNT
                                        P_WAERS.

  BAPICURR-BAPICURR = P_AMOUNT.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
       EXPORTING
            CURRENCY             = P_WAERS
            AMOUNT_EXTERNAL      = BAPICURR-BAPICURR
            MAX_NUMBER_OF_DIGITS = DIGITS
       IMPORTING
            AMOUNT_INTERNAL      = P_AMOUNT.

ENDFORM.                    " SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------
*
*&      Module  TOTAL_LINE_GET_SCR0191  OUTPUT
*&---------------------------------------------------------------------
*
MODULE TOTAL_LINE_GET_SCR0191 OUTPUT.

  IF W_STATUS EQ 'I'.
    G_PARAM_LINE = TC_0191-TOP_LINE.
    TC_0191-LINES = G_PARAM_LINE + 18.              " LINE ¼ö Á¤?
  ELSE.
    DESCRIBE TABLE IT_ZSRECST   LINES G_PARAM_LINE.   " LINE ¼ö GET
    TC_0191-LINES = G_PARAM_LINE.                     " LINE ¼ö Á¤?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0191  OUTPUT
*&---------------------------------------------------------------------
*
*&      Module  MARK_SET_SCR0191  OUTPUT
*&---------------------------------------------------------------------
*
MODULE MARK_SET_SCR0191 OUTPUT.

  IF OK-CODE = 'SELA'                                       " mark all
     AND TC_0191-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'DSEL'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " MARK_SET_SCR0191  OUTPUT
*&---------------------------------------------------------------------
*
*&      Module  FILL_TC_SCR0191  OUTPUT
*&---------------------------------------------------------------------
*
MODULE FILL_TC_SCR0191 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINEÀÇ À¯È¿¼º ¿©ºÎ °ËÁõ.
  IF TC_0191-CURRENT_LINE GT TC_0191-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Lineº° )
  READ TABLE IT_ZSRECST   INDEX TC_0191-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING  IT_ZSRECST    TO  ZSRECST.
    MOVE : IT_ZSRECST-ZFMARK          TO  W_ROW_MARK.         " MARK SET
  ELSE.
    MOVE : 'KRW'   TO   ZSRECST-ZFKRW.
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR8101  OUTPUT
*&---------------------------------------------------------------------
*
*&      Form  P2000_MOVE_LC_AMAND
*&---------------------------------------------------------------------
*
FORM P2000_MOVE_LC_AMAND.

ENDFORM.                    " P2000_MOVE_LC_AMAND
*&---------------------------------------------------------------------
*
*&      Form  P2000_SCR2103_PROCESS
*&---------------------------------------------------------------------
*
FORM P2000_SCR2103_PROCESS.

*----------------------------------------------------------------------
* TEXT SET
*----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '°ø±Þ¹°Ç°¸í¼¼' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  CALL SCREEN 0007 STARTING AT 15 1
                   ENDING   AT 95 19.

  IF ANTWORT EQ 'Y'.
    PERFORM   P2000_SCR0007_TO_TAB.
  ENDIF.

ENDFORM.                    " P2000_SCR2103_PROCESS
*&---------------------------------------------------------------------
*
*&      Form  P2000_SCR0006_SHOW
*&---------------------------------------------------------------------
*
FORM P2000_SCR0006_SHOW.
*----------------------------------------------------------------------
* TEXT SET
*----------------------------------------------------------------------
  PERFORM  P2000_SET_ACTION_TITLE.
  CONCATENATE <FS_F> ':' '¹°Ç°³»¿ª ±Ô°Ý' INTO SPOP-TITEL
                                        SEPARATED BY SPACE.

  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.
* µ¥ÀÌÅ¸ MOVE
*   PERFORM   P2000_TAB_TO_SCR.

  CALL SCREEN 0006 STARTING AT 15 5
                   ENDING   AT 95 15.

  IF ANTWORT EQ 'Y'.
*      PERFORM   P2000_SCR_TO_TAB.
  ENDIF.

ENDFORM.                    " P2000_SCR0006_SHOW
*&---------------------------------------------------------------------
*
*&      Form  P2000_REQ_FIELD_MOVE
*&---------------------------------------------------------------------
*
FORM P2000_REQ_FIELD_MOVE.
* Vendor. ==> Vendor
  MOVE EKKO-LIFNR      TO ZTREQHD-LIFNR.    " PO Supplying vend

  IF EKKO-LIFRE IS INITIAL.
    MOVE EKKO-LIFNR      TO ZTREQHD-ZFBENI.   " PO Vendor
  ELSE.
    MOVE EKKO-LIFRE      TO ZTREQHD-ZFBENI.   " PO Beneficiary.
  ENDIF.

* Supply Vendor ==> Supply( Offer )
  SELECT LIFN2 INTO ZTREQHD-LLIEF
           FROM  EKPA
           WHERE EBELN EQ ZSREQHD-EBELN
           AND   PARVW EQ 'Z3'.
  ENDSELECT.

  IF SY-SUBRC NE 0.
    MOVE EKKO-LIFNR      TO ZTREQHD-LLIEF.    " Supply
  ENDIF.

  MOVE : ZSREQHD-ZFREQTY TO ZTREQHD-ZFREQTY,    " TYPE
         ZSREQHD-ZFREQTY TO ZTREQST-ZFREQTY,    " TYPE
         ZSREQHD-EBELN   TO ZTREQHD-EBELN,      " Purchase Order.
         LFA1-LAND1      TO ZTREQHD-ZFSHCU,     " Loading Country.
         'US'            TO ZTREQHD-ZFARCU,     " Arriving Country.
         'S'             TO ZTREQHD-IMTRD,      " Impoter.
         '2'             TO ZTREQHD-ZFMATGB,    " Material Type.
         SPACE           TO ZTREQHD-ZFINSYN,    " Insuarance Y/N.
         EKKO-LIFNR      TO ZTREQHD-LIFNR,      " beneficiary
         EKKO-ZTERM      TO ZTREQHD-ZTERM,      " Terms of Payment
         EKKO-EKORG      TO ZTREQST-EKORG,      " Purch. Org.
         EKKO-EKGRP      TO ZTREQST-EKGRP,      " Purch. Grp.
         EKKO-INCO1      TO ZTREQHD-INCO1,      " Incortoms
         'S'             TO ZTREQHD-ZFCHG,      " BANKING CHARGE(BENI)
         W_ZFMAUD        TO ZTREQHD-ZFMAUD,     " Delivery date.
         W_ZFMAUD        TO ZTREQHD-ZFEPSD,     " Shipping Date.
         W_ZFMAUD        TO ZTREQHD-ZFREQSD,    " Shipping Default Date.
         W_ZFWERKS       TO ZTREQHD-ZFWERKS,    " Plant
         W_MATNR         TO ZTREQHD-MATNR,      " Material
         W_TXZ01         TO ZTREQHD-MAKTX,      " Mat. Desc.
         ZTIMIMG01-ZFPREPAY TO ZTREQHD-ZFPREPAY," RATE.
         ZTIMIMG01-ZFBACD TO ZTREQHD-ZFBACD,    " AFTER/BEFORE GB.
         ZTIMIMG01-ZFLCKN TO ZTREQHD-ZFLCKN,    " L/C KIND.
         W_TOT_AMOUNT    TO ZTREQHD-ZFLASTAM,   " Open Amount.
         EKKO-WAERS      TO ZTREQHD-WAERS,      " Currency
         EKKO-WAERS      TO ZTREQHD-ZFPKCUR,    " Currency
         EKKO-WAERS      TO ZTREQHD-ZFHDCUR,    " Currency
         W_BEDNR         TO ZTREQHD-ZFPRNAM,    " Purchase Requisition
         EKKO-BUKRS      TO ZTREQHD-BUKRS,      " Company Code
         'N'             TO ZTREQST-ZFDOCST,    " Document Status.
         ''              TO ZTREQST-ZFRTNYN,    " Return Y/N.
         'N'             TO ZTREQST-ZFEDIST,    " EDI Status
         'X'             TO ZTREQST-ZFEDICK,    " EDI Check
         SY-DATUM        TO ZTREQST-ZFREQDT,    " Requested open date.
         '00000000'      TO ZTREQST-ZFRLDT1,    " Requeste Release date.
         '00000000'      TO ZTREQST-ZFRLDT2,    " Open Release date.
         '00000000'      TO ZTREQST-ZFAPPDT,    " Apply date.
         '00000000'      TO ZTREQST-ZFRVDT,     " received date.
         '00000000'      TO ZTREQST-ZFOPNDT,    " opened date.
         ''              TO ZTREQST-ZFRLNM1,    " requested Release .
         ''              TO ZTREQST-ZFRLNM2,    " opened Release .
         ''              TO ZTREQST-ZFOPNNM,    " Opened By.
         '00000000'      TO ZTREQHD-ZFLASTSD,   " latest shipping date
         '00000000'      TO ZTREQHD-ZFLASTED,   " latest expire date
         ''              TO ZTREQST-ZFDOCNO,    " electronic doument NO
         ''              TO ZTREQST-ZFOPNNO,    " approval No.
         SY-UNAME        TO ZTREQST-ERNAM,      " Created by.
         SY-DATUM        TO ZTREQST-CDAT,       " Created on.
         SY-UNAME        TO ZTREQST-UNAM,       " Last changed by
         SY-DATUM        TO ZTREQST-UDAT,       " Last changed on
         SY-UNAME        TO ZTREQHD-ZFPRNAM.    " .

*--------------------------------------------------------------------
* P/O Import View Column -> Import Request
*--------------------------------------------------------------------
  CLEAR : EIKP.
  SELECT SINGLE * FROM  EIKP
         WHERE    EXNUM EQ   EKKO-EXNUM.
  IF SY-SUBRC EQ 0.                         " TRANSHIPMENT.
    IF EIKP-CONTA EQ '0' .
      MOVE  'X'   TO  ZTREQHD-ZZTSHIP.
    ELSE.
      CLEAR : ZTREQHD-ZZTSHIP.
    ENDIF.
  ENDIF.
  MOVE : EIKP-STGBE  TO  ZTREQHD-ZFSHCU,     " Loading Country
         EIKP-KZGBE  TO  ZTREQHD-ZFSPRT,     " Loading Port Text
         EIKP-BEHOE  TO  ZTREQHD-ZFAPRTC,    " Arriving Port
         EIKP-VORPA  TO  ZTREQHD-ZFTRANS.    " Transportation
  SELECT * FROM  EKPO  UP TO 1 ROWS
           WHERE EBELN EQ EKKO-EBELN.
  ENDSELECT.
*----------------------------------------------------------------------
* Use of release Yes/No
*----------------------------------------------------------------------
  IF ZTIMIMG00-ZFRELYN1   EQ 'X'.
    MOVE: 'N'             TO ZTREQST-ZFRLST1.
  ELSE.
    MOVE: 'N'             TO ZTREQST-ZFRLST1,
          SY-DATUM        TO ZTREQST-ZFAPPDT.
  ENDIF.

  IF ZTIMIMG00-ZFRELYN2   EQ 'X'.
    MOVE: 'N'             TO ZTREQST-ZFRLST2.
  ELSE.
    MOVE: 'N'             TO ZTREQST-ZFRLST2.
  ENDIF.

*>> Import Type SET.
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LO' OR 'PU'.
      ZTREQHD-ZFLCKN  = '8'.
      ZTREQHD-ZFMATGB = '2'.
    WHEN 'DA'.
      ZTREQHD-ZFLCKN = '4'.
    WHEN 'DP'.
      ZTREQHD-ZFLCKN = '5'.
    WHEN 'GS'.
      ZTREQHD-ZFLCKN = 'G'.
    WHEN 'TT'.
      IF ZTIMIMG01-ZFBACD EQ 'B'.
        ZTREQHD-ZFLCKN = '6'.
      ELSEIF ZTIMIMG01-ZFBACD EQ 'A'.
        ZTREQHD-ZFLCKN = '7'.
      ENDIF.
    WHEN 'LC'.
      ZTREQHD-ZFLCKN = ZTIMIMG01-ZFLCKN.
  ENDCASE.

  PERFORM   P2000_CONV_OTHER_CURR_TO_USD   USING  W_TOT_AMOUNT
                                                  ZTREQHD-WAERS
                                         CHANGING W_LOCAL_AMT.

*----------------------------------------------------------------------
*  2000/04/04   Insuarance BIT SETTING
*----------------------------------------------------------------------
  PERFORM   P2000_INSURANCE_BIT_SET.

*>> Transportation Set
  PERFORM   P2000_DELIVERY_MODE_SET.

* Financial Classification
  CLEAR : ZTREQHD-ZFJEWGB, ZTREQHD-ZFCRTYN.

ENDFORM.                    " P2000_REQ_FIELD_MOVE

*&---------------------------------------------------------------------
*
*&      Form  P2000_LC_FIELD_MOVE
*&---------------------------------------------------------------------
*
FORM P2000_LC_FIELD_MOVE.
  DATA: L_TEXT(70).

* Partial / Transhipment Yes/No.
  PERFORM   P2000_TRANS_OR_PART_SHIP_CHK.
* Incoterms Text.
  PERFORM   P1000_GET_INCOTERMS_TEXT  USING    EKKO-INCO1
                                      CHANGING ZTMLCHD-ZFINCN.
* Benificiary Text Select
  PERFORM   P1000_GET_LFA1_SELECT     USING    ZTREQHD-ZFBENI
                                      CHANGING W_LFA1.

  IF W_OK_CODE NE 'COPY'.
    MOVE:'X'                 TO ZTMLCSG910-ZFCOMYN,
         'X'                 TO ZTMLCHD-ZFADCD3,
         '3'                 TO ZTMLCSG910-ZFNOCOM,
         'X'                 TO ZTMLCSG910-ZFPACYN,
         '3'                 TO ZTMLCSG910-ZFNOPAC.
  ENDIF.

  MOVE : W_TOT_AMOUNT        TO ZTMLCHD-ZFOPAMT,
         EKKO-WAERS          TO ZTMLCHD-WAERS,
         EKKO-INCO1          TO ZTMLCHD-INCO1,
         EKKO-INCO2          TO ZTMLCHD-ZFINCP,
         ZTIMIMG01-ZFPREPAY  TO ZTREQHD-ZFPREPAY,
         ZTIMIMG01-ZFBACD    TO ZTREQHD-ZFBACD,
         ZTIMIMG01-ZFTRMB    TO ZTMLCHD-ZFTRMB,
         ZTIMIMG01-ZFUSPR    TO ZTMLCHD-ZFUSPR,
         ZTIMIMG01-ZFTRTX1   TO ZTMLCHD-ZFTRTX1,
         ZTIMIMG01-ZFTRTX2   TO ZTMLCHD-ZFTRTX2,
         ZTIMIMG01-ZFTRTX3   TO ZTMLCHD-ZFTRTX3,
         ZTIMIMG01-ZFTRTX4   TO ZTMLCHD-ZFTRTX4,
         '5'                 TO ZTMLCHD-ZFLCTY,
         '9'                 TO ZTMLCHD-ZFEDFN,
         ZTREQHD-ZFBENI      TO ZTMLCHD-ZFBENI,
         'BENEFICIARY IN YOUR COUNTRY'
                             TO ZTMLCHD-ZFEXPL,
         'DF'                TO ZTMLCHD-ZFOPME,
         'DA'                TO ZTMLCHD-ZFCNIS,
         '21'                TO ZTMLCHD-ZFPFPR.

  L_TEXT = ZTMLCHD-ZFINCP.
  TRANSLATE L_TEXT TO UPPER CASE.
  ZTMLCHD-ZFINCP = L_TEXT.

  SELECT SINGLE * FROM ZTIMIMG01
                  WHERE ZTERM   EQ   ZTREQHD-ZTERM
                  AND   ZFAPLDT EQ   ( SELECT MAX( ZFAPLDT )
                                       FROM ZTIMIMG01
                                       WHERE ZTERM   EQ   ZTREQHD-ZTERM
                                       AND   ZFAPLDT LE   SY-DATUM ).

  CASE ZTIMIMG01-ZFUSAT.
    WHEN 'UB'.
      ZTMLCHD-ZFPAGR = '2AA'.
      ZTREQHD-ZFJEWGB = '3'.
    WHEN 'US'.
      ZTMLCHD-ZFPAGR = '2AB'.
      ZTREQHD-ZFJEWGB = '3'.
    WHEN 'AT'.
      ZTREQHD-ZFJEWGB = '1'.
    WHEN OTHERS.
      CLEAR : ZTMLCHD-ZFPAGR.
  ENDCASE.
  CLEAR : LFBK.
  SELECT * FROM  LFBK UP TO 1 ROWS
           WHERE LIFNR EQ ZTREQHD-ZFBENI.
    EXIT.
  ENDSELECT.

*>> Advising Bank, Beneficiary Bank DEFAULT Setting.
  CLEAR : LFA1, LFBK, BNKA.
  SELECT SINGLE *
         FROM   LFA1
         WHERE  LIFNR = ZTREQHD-ZFBENI.

  SELECT SINGLE *
           FROM LFBK
          WHERE LIFNR = ZTREQHD-ZFBENI
            AND BANKS = LFA1-LAND1.

  SELECT SINGLE *
           FROM BNKA
          WHERE BANKS = LFA1-LAND1
            AND BANKL = LFBK-BANKL.

  MOVE BNKA-BANKA        TO ZTMLCHD-ZFABNM.
  MOVE BNKA-BRNCH        TO ZTMLCHD-ZFABBR.

  PERFORM  P2000_REFRESH_ADDRESS     USING   ZTREQHD-ZFBENI.

ENDFORM.                    " P2000_LC_FIELD_MOVE

*&---------------------------------------------------------------------
*&      Form  P2000_LOCAL_LC_FIELD_MOVE
*&---------------------------------------------------------------------
FORM P2000_LOCAL_LC_FIELD_MOVE.
* Partial/Transhiment Yes/No
  PERFORM   P2000_TRANS_OR_PART_SHIP_CHK.
* Benificiary Text Select
  PERFORM   P1000_GET_LFA1_SELECT     USING    ZTREQHD-ZFBENI
                                      CHANGING W_LFA1.
* Shipping Document
  MOVE :  'X'               TO  ZTLLCSG23-ZFDOMYN,
          1                 TO  ZTLLCSG23-ZFNODOM,
          'X'               TO  ZTLLCSG23-ZFBILYN,
          1                 TO  ZTLLCSG23-ZFNOBIL,
          ''                TO  ZTLLCSG23-ZFINVYN,
          0                 TO  ZTLLCSG23-ZFNOINV,
          'X'               TO  ZTLLCSG23-ZFOFYN,
          1                 TO  ZTLLCSG23-ZFNOOF,
          'X'               TO  ZTLLCSG23-ZFLLCYN,
          1                 TO  ZTLLCSG23-ZFNOLLC.

* Others Information.
  MOVE : W_LFA1-NAME2       TO ZTLLCSG23-ZFBENI1,
         W_LFA1-J_1KFREPRE  TO ZTLLCSG23-ZFBENI2,
         ''                 TO ZTLLCSG23-ZFBENI3,
         'X'                TO ZTREQHD-ZZPSHIP,
         '9'                TO ZTLLCHD-ZFPRAL,
         '9'                TO ZTLLCHD-ZFEDFN,
         '7'                TO ZTLLCHD-ZFDPRP,
         '1'                TO ZTLLCHD-ZFOPCNT,
         '2AD'              TO ZTLLCHD-ZFUSG.
  IF EKKO-WAERS  EQ  'KRW'.
    MOVE  :  '2AC'       TO    ZTLLCHD-ZFLLCTY,
             '2AC'       TO    ZSREQHD-ZFLLCTY.
  ELSE.
    MOVE  :  '2AA'       TO    ZTLLCHD-ZFLLCTY,
             '2AA'       TO    ZSREQHD-ZFLLCTY.
  ENDIF.

  MOVE : W_TOT_AMOUNT        TO ZTLLCHD-ZFOPAMT,
         EKKO-WAERS          TO ZTLLCHD-WAERS,
         EKKO-WAERS          TO ZTLLCHD-ZFOPAMTC,
         W_KRW               TO ZTLLCHD-ZFKRW,
         ZTIMIMG01-ZFPREPAY  TO ZTREQHD-ZFPREPAY,
         ZTIMIMG01-ZFBACD    TO ZTREQHD-ZFBACD.

  REFRESH : IT_ZSLLCOF.
  IT_ZSLLCOF-ZFOFFER = ZTREQHD-EBELN.   IT_ZSLLCOF-ZFLSGOF = '00010'.
  APPEND  IT_ZSLLCOF.

ENDFORM.                    " P2000_LOCAL_LC_FIELD_MOVE

*&---------------------------------------------------------------------
*&      Form  P2000_PURCH_FIELD_MOVE
*&---------------------------------------------------------------------
FORM P2000_PURCH_FIELD_MOVE.
* Benificiary Text Select
  PERFORM   P1000_GET_LFA1_SELECT     USING    ZTREQHD-ZFBENI
                                      CHANGING W_LFA1.

  IF W_OK_CODE NE 'COPY'.
    MOVE : '9'                 TO ZTPUR-ZFEDFN.
  ENDIF.

  MOVE : W_TOT_AMOUNT        TO ZTPUR-ZFTOAM,
         EKKO-WAERS          TO ZTPUR-ZFTOAMC,
         W_USD               TO ZTPUR-ZFUSD.

  MOVE : W_LFA1-NAME2        TO ZTPUR-ZFVENNM1,
         ''                  TO ZTPUR-ZFVENNM2,
         W_LFA1-STRAS        TO ZTPUR-ZFVENAD1,
         W_LFA1-ORT01        TO ZTPUR-ZFVENAD2,
         W_LFA1-ORT02        TO ZTPUR-ZFVENAD3,
         W_LFA1-BAHNS        TO ZTPUR-ZFVENID.

* Basis Document
  REFRESH : IT_ZSPURSG4.   CLEAR : IT_ZSPURSG4.
  MOVE : ZTREQST-MANDT         TO    IT_ZSPURSG4-MANDT,
         ZTREQST-ZFREQNO       TO    IT_ZSPURSG4-ZFREQNO,
         ZTREQST-ZFAMDNO       TO    IT_ZSPURSG4-ZFAMDNO,
         '00010'               TO    IT_ZSPURSG4-ZFLSG4,
         ''                    TO    IT_ZSPURSG4-STAWN,
         ''                    TO    IT_ZSPURSG4-ZFGODS1,
         ZTPURSG4-ZFSDOC       TO    IT_ZSPURSG4-ZFSDOC,
         ZTPURSG4-ZFSDNO       TO    IT_ZSPURSG4-ZFSDNO.

  APPEND IT_ZSPURSG4.

ENDFORM.                    " P2000_PURCH_FIELD_MOVE
*&---------------------------------------------------------------------
*&      Form  P1000_GET_VENDOR
*&---------------------------------------------------------------------
FORM P1000_GET_VENDOR USING    P_LIFNR
                      CHANGING P_NAME1.
  DATA : L_NAME1(255),
         L_NAME2(255),
         L_NAME3(255),
         L_NAME4(255).

  CLEAR : P_NAME1, W_LFA1.
  IF P_LIFNR IS INITIAL.
    EXIT.
  ENDIF.

  CLEAR : W_LFA1, W_ADRC.

  CALL FUNCTION 'ZIM_GET_VENDOR_ADDRESS_FORMAT'
       EXPORTING
            LIFNR     = P_LIFNR
       IMPORTING
            NAME1     = L_NAME1
            NAME2     = L_NAME2
            NAME3     = L_NAME3
            NAME4     = L_NAME4
            P_LFA1    = W_LFA1
            P_ADRC    = W_ADRC
       EXCEPTIONS
            NO_INPUT  = 01
            NOT_FOUND = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
  ENDCASE.

  TRANSLATE : L_NAME1 TO UPPER CASE,
              L_NAME2 TO UPPER CASE,
              L_NAME3 TO UPPER CASE,
              L_NAME4 TO UPPER CASE.

  MOVE : L_NAME1      TO W_LFA1-NAME1,
         L_NAME2      TO W_LFA1-NAME2,
         L_NAME3      TO W_LFA1-NAME3,
         L_NAME4      TO W_LFA1-NAME4.

  MOVE: W_LFA1-NAME1   TO   P_NAME1.

ENDFORM.                    " P1000_GET_VENDOR
*&---------------------------------------------------------------------
*
*&      Form  P2000_PO_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------
*
FORM P2000_PO_DOCUMENT_DISPLAY.

  SELECT SINGLE * FROM EKKO
         WHERE    EBELN EQ ZTREQHD-EBELN.
  IF SY-SUBRC EQ 0.

    IF EKKO-BSTYP EQ 'K'.
      SET PARAMETER ID 'CTR' FIELD ZTREQHD-EBELN.
      CALL TRANSACTION 'ME33K' AND SKIP  FIRST SCREEN.
    ELSEIF EKKO-BSTYP EQ 'L'.
      SET PARAMETER ID 'SAG' FIELD ZTREQHD-EBELN.
      CALL TRANSACTION 'ME33L' AND SKIP  FIRST SCREEN.
    ELSE.
      SET PARAMETER ID 'BSP' FIELD ''.
      EXPORT 'BSP' TO MEMORY ID 'BSP'.
      SET PARAMETER ID 'BES' FIELD ZTREQHD-EBELN.
      EXPORT 'BES'  TO MEMORY ID 'BES'.
      CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_PO_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------
*
*&      Form  P2000_VENDOR_DISPLAY
*&---------------------------------------------------------------------
*
FORM P2000_VENDOR_DISPLAY.
* Screen PARAMETER ID
  SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
  SET PARAMETER ID 'LIF' FIELD ZTREQHD-LIFNR.
  SET PARAMETER ID 'EKO' FIELD ''.
  EXPORT 'LIF'   TO MEMORY ID 'LIF'.
  EXPORT 'EKO'   TO MEMORY ID 'EKO'.

  CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_VENDOR_DISPLAY
*&---------------------------------------------------------------------
*
*&      Form  P2000_OPEN_BANK_DISPLAY
*&---------------------------------------------------------------------
*
FORM P2000_OPEN_BANK_DISPLAY.
  IF ZTREQHD-ZFOPBN IS INITIAL.
    MESSAGE S060.    EXIT.
  ENDIF.
* Screen PARAMETER ID
  SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
*  SET PARAMETER ID 'KDY' FIELD '/320/310/130/120/110'.
  SET PARAMETER ID 'LIF' FIELD ZTREQHD-ZFOPBN.
*  SET PARAMETER ID 'EKO' FIELD ZTREQST-EKORG.
  SET PARAMETER ID 'EKO' FIELD ''.

  CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_OPEN_BANK_DISPLAY
*&---------------------------------------------------------------------
*
*&      Form  P2000_SELECT_MATERIAL
*&---------------------------------------------------------------------
*
FORM P2000_SELECT_MATERIAL.
  W_SEL_MAT_CNT = 0.

  LOOP AT IT_ZSREQIT WHERE ZFMARK EQ 'X'.
    W_SEL_MAT_CNT = W_SEL_MAT_CNT + 1.
    MOVE-CORRESPONDING  IT_ZSREQIT  TO  W_ZSREQIT.
  ENDLOOP.

ENDFORM.                    " P2000_SELECT_MATERIAL
*&---------------------------------------------------------------------
*
*&      Form  P2000_MATERIAL_DISPLAY
*&---------------------------------------------------------------------
*
FORM P2000_MATERIAL_DISPLAY.
  CASE W_SEL_MAT_CNT.
    WHEN 0.
      MESSAGE S951.
    WHEN 1.
      PERFORM P1000_EKPO_SELECT_ITEM.

      SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
      SET PARAMETER ID 'BUK' FIELD EKPO-BUKRS.
      SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
      SET PARAMETER ID 'LAG' FIELD ''.
      SET PARAMETER ID 'MXX' FIELD MM03_START_SICHT.
      CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.
    WHEN OTHERS.
      MESSAGE S965.
  ENDCASE.

ENDFORM.                    " P2000_MATERIAL_DISPLAY
*&---------------------------------------------------------------------
*
*&      Form  P1000_EKPO_SELECT_ITEM
*&---------------------------------------------------------------------
*
FORM P1000_EKPO_SELECT_ITEM.
  SELECT SINGLE * FROM EKPO WHERE EBELN EQ ZTREQHD-EBELN
                            AND   EBELP EQ W_ZSREQIT-ZFITMNO.

  IF SY-SUBRC EQ 0.
    IF EKPO-MATNR IS INITIAL.
      MESSAGE E336(ME) WITH W_ZSREQIT-ZFITMNO.
    ENDIF.
  ELSE.
    MESSAGE E071 WITH ZTREQHD-EBELN W_ZSREQIT-ZFITMNO.
  ENDIF.

ENDFORM.                    " P1000_EKPO_SELECT_ITEM
*&---------------------------------------------------------------------
*
*&      Form  P2000_MATERIAL_MD04
*&---------------------------------------------------------------------
*
FORM P2000_MATERIAL_MD04.
  CASE W_SEL_MAT_CNT.
    WHEN 0.
      MESSAGE S951.
    WHEN 1.
      PERFORM P1000_EKPO_SELECT_ITEM.

      SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
      SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
      CALL TRANSACTION 'MD04' AND SKIP  FIRST SCREEN.
    WHEN OTHERS.
      MESSAGE S965.
  ENDCASE.

ENDFORM.                    " P2000_MATERIAL_MD04
*&---------------------------------------------------------------------
*
*&      Form  P2000_MATERIAL_MMBE
*&---------------------------------------------------------------------
*
FORM P2000_MATERIAL_MMBE.
  CASE W_SEL_MAT_CNT.
    WHEN 0.
      MESSAGE S951.
    WHEN 1.
      PERFORM P1000_EKPO_SELECT_ITEM.

      SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
      SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
      SET PARAMETER ID 'LAG' FIELD ''.
      SET PARAMETER ID 'CHA' FIELD ''.
      CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
      MESSAGE S965.
  ENDCASE.

ENDFORM.                    " P2000_MATERIAL_MMBE
*&---------------------------------------------------------------------
*
*&      Form  P2000_MATERIAL_MB51
*&---------------------------------------------------------------------
*
FORM P2000_MATERIAL_MB51.
  CASE W_SEL_MAT_CNT.
    WHEN 0.
      MESSAGE S951.
    WHEN 1.
      PERFORM P1000_EKPO_SELECT_ITEM.

      SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
      SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
      SET PARAMETER ID 'BWA' FIELD ''.
      SET PARAMETER ID 'LIF' FIELD ZTREQHD-LIFNR.
      SET PARAMETER ID 'LAG' FIELD ''.
      SET PARAMETER ID 'CHA' FIELD ''.
      CALL TRANSACTION 'MB51' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      MESSAGE S965.
  ENDCASE.

ENDFORM.                    " P2000_MATERIAL_MB51
*&---------------------------------------------------------------------
*
*&      Form  P2000_MATERIAL_ME2M
*&---------------------------------------------------------------------
*
FORM P2000_MATERIAL_ME2M.
  CASE W_SEL_MAT_CNT.
    WHEN 0.
      MESSAGE S951.
    WHEN 1.
      PERFORM P1000_EKPO_SELECT_ITEM.

      SUBMIT RM06EM00
         WITH EM_MATNR EQ EKPO-MATNR
         WITH EM_WERKS EQ EKPO-WERKS
         WITH LISTU    EQ 'BEST'
         WITH SELPA    EQ 'WE101'
         AND RETURN.

    WHEN OTHERS.
      MESSAGE S965.
  ENDCASE.

ENDFORM.                    " P2000_MATERIAL_ME2M
*&---------------------------------------------------------------------
*
*&      Form  P2000_MATERIAL_ME03
*&---------------------------------------------------------------------
*
FORM P2000_MATERIAL_ME03.
  CASE W_SEL_MAT_CNT.
    WHEN 0.
      MESSAGE S951.
    WHEN 1.
      PERFORM P1000_EKPO_SELECT_ITEM.

      SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
      SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
      CALL TRANSACTION 'ME03' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      MESSAGE S965.
  ENDCASE.

ENDFORM.                    " P2000_MATERIAL_ME03
*&---------------------------------------------------------------------
*
*&      Form  P2000_SET_REQ_INIT_SCR
*&---------------------------------------------------------------------
*
FORM P2000_SET_REQ_INIT_SCR.
* document

  MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " HEADER CHANGE DOC
  MOVE 'UCUR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " Currency Change
  MOVE 'HIIT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " ITEM CHANGE DOC.
  MOVE 'ZIMG' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " IMPORT IMG
  MOVE 'DDLC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " DOUBLE CLICK
  MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " SAVE
  MOVE 'CKEK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " CHECK.
  MOVE 'OTDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " Others Document
  MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " EDI SEND
  MOVE 'PREV' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " PREVIOUS DOC.
  MOVE 'NEXT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " NEXT DOCUMENT
  MOVE 'COST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " Import Req Cost
  MOVE 'TTREQ' TO IT_EXCL-FCODE.   APPEND IT_EXCL.  " REPORT.
  MOVE 'APP700' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " REPORT.
  MOVE 'APP707' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " REPORT.
  MOVE 'INF700' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " REPORT.
  MOVE 'INF707' TO IT_EXCL-FCODE.   APPEND IT_EXCL. " REPORT.
  MOVE 'PUDOC'  TO IT_EXCL-FCODE.   APPEND IT_EXCL. " REPORT.
* header
  MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " DELETE.
  MOVE 'REVK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " CANCEL
*  ENDIF.
  MOVE 'CHFD'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Change Item.
  MOVE 'DELR'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Approve Cancel
  MOVE 'LGIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " Insuarance Req.
  MOVE 'OPCL'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Open Cancel
  MOVE 'RECO'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Recommend
  MOVE 'ME23'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " p/o history
  MOVE 'MK03'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Vendor
  MOVE 'MK23'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " p/o
  MOVE 'ZIM03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " p/o
  MOVE 'ZIM93' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " L/C history
  MOVE 'ZIBK'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Openning Bank
  MOVE 'ZIBE'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Beneficiary
  MOVE 'ZICI'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Loan Organization
* item
  MOVE 'MM03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Material Master
  MOVE 'MD04' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " MRP
  MOVE 'MMBE' TO IT_EXCL-FCODE.    APPEND IT_EXCL. "
  MOVE 'MB51' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Material Doc.
  MOVE 'ME2M' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Material Open P/O
  MOVE 'ME03' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " Source List
* env
  MOVE 'STAT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " STATUS
  MOVE 'FLAT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " FLAT DATA
  MOVE 'EDI1' TO IT_EXCL-FCODE.    APPEND IT_EXCL.  " EDI Approve
  MOVE 'EDI2' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "

  MOVE 'OFCR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Offer Create.

  IF SY-TCODE(4) EQ 'ZIM0'.
    IF ZTIMIMG00-ZFRELYN1 NE 'X'.
      MOVE 'ADDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DELR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
    ENDIF.
  ELSEIF SY-TCODE(4) EQ 'ZIM1'.
    IF ZTIMIMG00-ZFRELYN3 NE 'X'.
      MOVE 'ADDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DELR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_SET_REQ_INIT_SCR
*&---------------------------------------------------------------------
*&      Form  P2000_SET_REQDOC_LOCK
*&---------------------------------------------------------------------
FORM P2000_SET_REQDOC_LOCK USING    PA_MODE   PA_AMDNO.

  CHECK : SY-TCODE NE 'ZIMC1'.
  CHECK : SY-CALLD NE 'X'.

  IF PA_MODE EQ 'L'.
* P/O LOCK
    CALL FUNCTION 'ENQUEUE_EMEKKOE'
         EXPORTING
              EBELN  = ZTREQHD-EBELN
         EXCEPTIONS
              OTHERS = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Purchase Order'
                        ZTREQHD-EBELN SPACE
                   RAISING DOCUMENT_LOCKED.
    ENDIF.


    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = ZSREQHD-ZFREQNO
              ZFAMDNO = PA_AMDNO
         EXCEPTIONS
              OTHERS  = 1.
    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Import Document'
                            ZTREQHD-ZFREQNO PA_AMDNO
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
* P/O LOCK
    CALL FUNCTION 'DEQUEUE_EMEKKOE'
         EXPORTING
              EBELN = ZTREQHD-EBELN.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = ZSREQHD-ZFREQNO
              ZFAMDNO = PA_AMDNO.
  ENDIF.

ENDFORM.                    " P2000_SET_REQDOC_LOCK
*&---------------------------------------------------------------------
*&      Form  P2000_SET_UNLOCK
*&---------------------------------------------------------------------
FORM P2000_SET_UNLOCK.

  CASE SY-TCODE.
* Import Request No.
    WHEN 'ZIM02' OR 'ZIM05' OR 'ZIM07' OR 'ZIM03'.
      W_AMDNO   =    '00000'.
      PERFORM P2000_SET_REQDOC_LOCK    USING    'U'   W_AMDNO.
* LOCAL OFFER SHEET
    WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
      PERFORM P2000_SET_OFFER_DOC_LOCK USING    'U'.
* Insuarance.
    WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44' OR
         'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
      PERFORM P2000_SET_INS_DOC_LOCK USING 'U'.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_UNLOCK
*&---------------------------------------------------------------------
*&      Form  P2000_SET_INIT_SCREEN
*&---------------------------------------------------------------------
FORM P2000_SET_INIT_SCREEN.

  W_OK_CODE = OK-CODE.

  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
           W_STATUS EQ C_OPEN_D ).
    PERFORM P2000_SET_MESSAGE USING  'ANZG'.

    CASE ANTWORT.
      WHEN 'Y'.                                             " Yes...
        IF SY-TCODE EQ 'ZIM01' AND ZTREQHD-ZFREQTY EQ 'PU'.
          PERFORM   P2000_GET_PUR_DOC_NO.
          IF ANTWORT NE 'Y'.
            EXIT.
          ENDIF.
        ENDIF.
        PERFORM  P3000_DB_MODIFY_SCRCOM.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_UNLOCK.
      WHEN 'N'.              " No...
        MESSAGE  S957.
        CLEAR OK-CODE.
        PERFORM  P2000_SET_UNLOCK.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDIF.

* LEAVE TO TRANSACTION
  PERFORM   P2000_SET_TRANSACTION.

ENDFORM.                    " P2000_SET_INIT_SCREEN
*&---------------------------------------------------------------------
*
*&      Form  P1000_REQ_DOC_READ
*&---------------------------------------------------------------------
*
FORM P1000_REQ_DOC_READ.
* READ BIT SET...
  W_READ_CHK = 'N'.

  IF NOT ZSREQHD-ZFREQNO IS INITIAL.
* Import Request Doc Display
    PERFORM   P1000_READ_REQ_DOC.

    IF SY-TCODE NE 'ZIMC1'.
*----------------------------------------------------------------------
* Existed Document READ
*----------------------------------------------------------------------
      PERFORM   P1000_PRE_REQ_DOC_READ.
* Supply Vendor Name
      PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-LLIEF
                                  CHANGING   W_LLIEF_NM.
* Vendor NAME1 GET
      PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-LIFNR
                                  CHANGING   W_LIFNR_NM.
* Benificiary
      PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFBENI
                                  CHANGING   W_ZFBENI_NM.
* Loan Orgarnization NAME1 GET
      PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFLEVN
                                  CHANGING   W_ZFLEVN_NM.
* OPEN BANK NAME1 GET
      PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFOPBN
                                  CHANGING   W_OPEN_NM.
    ENDIF.
* READ BIT SET...
    W_READ_CHK = 'Y'.
  ENDIF.

ENDFORM.                    " P1000_REQ_DOC_READ
*&---------------------------------------------------------------------
*
*&      Form  P2000_DOC_ITEM_SELECT
*&---------------------------------------------------------------------
*
FORM P2000_DOC_ITEM_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
* INPUT VALUE
  W_ZSREQHD = ZSREQHD.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE  EBELN   EQ ZSREQHD-EBELN
           AND    ZFAMDNO EQ '00'
           ORDER  BY ZFREQNO.

*----------------------------------------------------------------------
* position
*----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.   MESSAGE E406.   ENDIF.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LCCHANGE'.
  CALL SCREEN 0014 STARTING AT  08 3
                   ENDING   AT  90 15.

ENDFORM.                    " P2000_DOC_ITEM_SELECT

*&---------------------------------------------------------------------
*&      Form  P3000_MASTER_LC_MOVE
*&---------------------------------------------------------------------
FORM P3000_MASTER_LC_MOVE.
* Origin
  IF W_STATUS NE C_REQ_D.
    REFRESH : IT_ZSMLCSG7O.
    LOOP AT  IT_ZTREQORJ.
      MOVE-CORRESPONDING IT_ZTREQORJ TO IT_ZSMLCSG7O.
      APPEND IT_ZSMLCSG7O.
    ENDLOOP.
  ENDIF.
* Bank Charge
  PERFORM  P2000_BANK_CHARGE_MOVE.
* Partial Shipment
  PERFORM  P2000_PARTIAL_SHIPMENT_MOVE.
* Transhipment
  PERFORM  P2000_TRANS_SHIPMENT_MOVE.
  IF NOT ( ZTREQHD-ZTERM IS INITIAL ).
    SELECT SINGLE *
    FROM   ZTIMIMG01
    WHERE  ZTERM    =  ZTREQHD-ZTERM
    AND    ZFAPLDT  =  ( SELECT MAX( ZFAPLDT )
                         FROM   ZTIMIMG01
                         WHERE  ZTERM   =  ZTREQHD-ZTERM
                         AND    ZFAPLDT <= SY-DATUM  )  .
    IF ZTMLCHD-ZFUSAT IS INITIAL.
      MOVE ZTIMIMG01-ZFUSAT TO ZTMLCHD-ZFUSAT.
    ENDIF.
    IF ZTMLCHD-ZFUSPR IS INITIAL.
      MOVE ZTIMIMG01-ZFUSPR TO ZTMLCHD-ZFUSPR.
    ENDIF.
  ENDIF.

* P/O Doc. No -> Others Information Field Move
  MOVE ZTREQHD-EBELN TO ZTMLCHD-ZFETC1.

* Other Information
  MOVE : ZTREQHD-ZFSPRT     TO ZTMLCHD-ZFSPRT,   " Loading Port
         ZTREQHD-ZFAPRT     TO ZTMLCHD-ZFAPRT,   " Arriving Port
         ZTREQHD-ZFREQED    TO ZTMLCHD-ZFEXDT,   " Expiry Date
         ZTREQHD-ZFREQSD    TO ZTMLCHD-ZFLTSD,   " S/D
         ZTREQHD-ZFOPBN     TO ZTMLCHD-ZFOPBN,   " Open Bank
         ZTREQHD-ZFREQED    TO ZTMLCHD-ZFEXDT,   " Request Expiry Date
         ZTREQHD-INCO1      TO ZTMLCHD-INCO1.    " Incoterms

  PERFORM  P2000_SET_SHIPING_TEXT.

ENDFORM.                    " P3000_MASTER_LC_MOVE

*&---------------------------------------------------------------------
*&      Form  P3000_LOCAL_LC_MOVE
*&---------------------------------------------------------------------
FORM P3000_LOCAL_LC_MOVE.
* Partial Shipment
  PERFORM  P2000_PARTIAL_SHIPMENT_MOVE.
ENDFORM.                    " P3000_LOCAL_LC_MOVE

*&---------------------------------------------------------------------
*&      Form  P3000_PURCH_DOC_MOVE
*&---------------------------------------------------------------------
FORM P3000_PURCH_DOC_MOVE.
* Goods Description
  IF SY-TCODE EQ 'ZIM01'.
    REFRESH : IT_ZSPURSG1.
    PERFORM   P2000_PURSG1_APPEND   USING ''.
  ENDIF.

* HS CODE MOVE
  LOOP  AT  IT_ZSREQIT.
    IF IT_ZSREQIT-STAWN NE SPACE.
      MOVE IT_ZSREQIT-STAWN  TO  ZTLLCHD-ZFETC1.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF ZTPUR-ZFVENID IS INITIAL.
    SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ ZTPUR-ZFBENI.
    ZTPUR-ZFVENID = LFA1-KRAUS.
  ENDIF.
  IF ZTPUR-ZFGDCD IS INITIAL.
    MOVE '2BM'     TO    ZTPUR-ZFGDCD.
  ENDIF.
ENDFORM.                    " P3000_PURCH_DOC_MOVE
*&---------------------------------------------------------------------
*&      Form  P1000_GET_INCOTERMS_TEXT
*&---------------------------------------------------------------------
FORM P1000_GET_INCOTERMS_TEXT USING    PA_INCO1
                              CHANGING PA_TEXT.
  CLEAR : PA_TEXT.

  SELECT SINGLE * FROM TINCT WHERE INCO1 EQ PA_INCO1
                             AND   SPRAS EQ 'E'.

  IF SY-SUBRC EQ 0.
    PA_TEXT = TINCT-BEZEI.
    TRANSLATE PA_TEXT TO UPPER CASE.
  ENDIF.

ENDFORM.                    " P1000_GET_INCOTERMS_TEXT
*&---------------------------------------------------------------------
*&      Form  P2000_TRANS_OR_PART_SHIP_CHK
*&---------------------------------------------------------------------
FORM P2000_TRANS_OR_PART_SHIP_CHK.
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      IF ZTREQHD-ZZPSHIP EQ 'X'.
        ZTMLCHD-ZFPRMT = '9'.
      ELSE.
        ZTMLCHD-ZFPRMT = '10'.
      ENDIF.

      IF ZTREQHD-ZZTSHIP EQ 'X'.
        ZTMLCHD-ZFTRMT = '7'.
      ELSE.
        ZTMLCHD-ZFTRMT = '8'.
      ENDIF.
    WHEN 'LO'.
      IF ZTREQHD-ZZPSHIP EQ 'X'.
        ZTLLCHD-ZFPRAL = '9'.
      ELSE.
        ZTLLCHD-ZFPRAL = '10'.
      ENDIF.

    WHEN 'PU'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_TRANS_OR_PART_SHIP_CHK
*&---------------------------------------------------------------------
*&      Form  P1000_GET_LFA1_SELECT
*&---------------------------------------------------------------------
FORM P1000_GET_LFA1_SELECT USING    P_LIFNR
                           CHANGING P_LFA1.
  CLEAR : P_LFA1.
  IF P_LIFNR IS INITIAL.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*----------------------------------------------------------------------
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
  TRANSLATE P_LFA1  TO UPPER CASE.

ENDFORM.                    " P1000_GET_LFA1_SELECT
*&---------------------------------------------------------------------
*&      Form  P2000_REQ_DOC_STATUS_CHECK
*&---------------------------------------------------------------------
FORM P2000_REQ_DOC_STATUS_CHECK.

  CASE W_STATUS.
    WHEN C_REQ_U.
      PERFORM P2000_LC_CHAGE_CHECK   CHANGING W_ERR_CHK.
    WHEN C_REQ_D.
      PERFORM P2000_LC_DISPLAY_CHECK.
    WHEN C_ADD_U.
      PERFORM P2000_ADD_CHANGE_CHECK CHANGING W_ERR_CHK.
    WHEN C_OPEN_C.
      PERFORM P2000_OPEN_CHECK       CHANGING W_ERR_CHK.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_REQ_DOC_STATUS_CHECK

*&---------------------------------------------------------------------
*&      Form  P2000_LC_DISPLAY_CHECK
*&---------------------------------------------------------------------
FORM P2000_LC_DISPLAY_CHECK.

  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.
      MESSAGE S998 WITH ZTREQST-ZFREQNO 'EDI open requested'.
    WHEN 'O'.
      IF ZTREQST-ZFEDIST = 'R'.
        MESSAGE S998 WITH ZTREQST-ZFREQNO 'EDI Open'.
      ELSE.
        MESSAGE S998 WITH ZTREQST-ZFREQNO 'Non-EDI Open'.
      ENDIF.
    WHEN 'C'.
      MESSAGE S998 WITH ZTREQST-ZFREQNO 'Cancel'.
    WHEN 'A'.
      MESSAGE S998 WITH ZTREQST-ZFREQNO 'Amend'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_LC_DISPLAY_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_LC_CHAGE_CHECK
*&---------------------------------------------------------------------
FORM P2000_LC_CHAGE_CHECK    CHANGING   W_ERR_CHK.

  W_ERR_CHK = 'Y'.

* Purchase Order Release Yes/No
  IF ZTIMIMG00-ZFRELYN1 EQ 'X' AND ZTREQST-ZFRLST1 EQ 'R'.
    MESSAGE E999 WITH ZTREQST-ZFREQNO 'request release(approve)'
                                      'Change'.
  ENDIF.

  IF ZTIMIMG00-ZFRELYN1 EQ 'X' AND  ZTREQST-ZFRVDT > '00000000'.
    MESSAGE E999 WITH ZTREQST-ZFREQNO 'accepted' 'Change'.
  ENDIF.

  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.
      IF ZTREQST-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTREQST-ZFREQNO 'EDI open request' 'Change'.
      ENDIF.
    WHEN 'O'.
      IF ZTREQST-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTREQST-ZFREQNO 'EDI open' 'Change'.
      ELSE.
        MESSAGE E999
                WITH ZTREQST-ZFREQNO 'Non-EDI open' 'Change'.
      ENDIF.
      EXIT.
    WHEN 'C'.
      MESSAGE E999 WITH ZTREQST-ZFREQNO 'Cancel' 'Change'.
    WHEN 'A'.                                               " AMEND
      MESSAGE E999 WITH ZTREQST-ZFREQNO 'Amend' 'Change'.
  ENDCASE.

  W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_LC_CHAGE_CHECK
*&---------------------------------------------------------------------
*
*&      Form  P2000_ADD_CHANGE_CHECK
*&---------------------------------------------------------------------
*
FORM P2000_ADD_CHANGE_CHECK    CHANGING W_ERR_CHK.

  W_ERR_CHK = 'Y'.

* Use of Purchase Order Release Yes/No
  IF ZTIMIMG00-ZFRELYN1 EQ 'X' AND ZTREQST-ZFRLST1 NE 'R'.
    MESSAGE E999 WITH
      ZTREQHD-ZFREQNO 'not release' 'Additional change'
.
  ENDIF.

* Use of Open Release Yes/No
  IF ZTIMIMG00-ZFRELYN2 EQ 'X' AND ZTREQST-ZFRLST2 EQ 'R'.
    MESSAGE E999
       WITH ZTREQHD-ZFREQNO 'open release' 'Additional change'.
  ENDIF.

  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.
      IF ZTREQST-ZFEDIST = 'S'.
        MESSAGE E999
          WITH ZTREQHD-ZFREQNO 'EDI open release' 'Additional change'.
      ENDIF.
    WHEN 'O'.
      IF ZTREQST-ZFEDIST = 'R'.
        MESSAGE E999
        WITH ZTREQHD-ZFREQNO 'EDI open' 'Additional change'.
      ELSE.
        MESSAGE E999
        WITH ZTREQHD-ZFREQNO 'Non-EDI open' 'Additional change'.
      ENDIF.
    WHEN 'C'.
      MESSAGE E999
      WITH ZTREQHD-ZFREQNO 'cancel' 'ADDITIONAL CHANGE'.
      EXIT.
    WHEN 'A'.
      MESSAGE E999
      WITH ZTREQHD-ZFREQNO 'Amend' 'Additional change'.
  ENDCASE.

  W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_ADD_CHANGE_CHECK

*&---------------------------------------------------------------------
*&      Form  P2000_OPEN_CHECK
*&---------------------------------------------------------------------
FORM P2000_OPEN_CHECK  CHANGING W_ERR_CHK.

  W_ERR_CHK = 'Y'.

* Use of Purchase Order Release Yes/No
  IF ZTIMIMG00-ZFRELYN1 EQ 'X' AND ZTREQST-ZFRLST1 NE 'R'.
    MESSAGE E999
             WITH ZTREQHD-ZFREQNO 'not request release' 'Open'.
  ENDIF.

  IF ZTREQST-ZFRVDT IS INITIAL.
    MESSAGE E999
         WITH ZTREQHD-ZFREQNO 'not accepted' 'Open'.
  ENDIF.

  IF ZTREQST-ZFRTNYN EQ 'X'.
    MESSAGE E999 WITH ZTREQHD-ZFREQNO 'cancel acceptance' 'Open'.
  ENDIF.

  IF ZTIMIMG00-ZFRELYN2 EQ 'X' AND ZTREQST-ZFRLST2 NE 'R'.
    MESSAGE E999
            WITH ZTREQHD-ZFREQNO 'not open release' 'Open'.
    EXIT.
  ENDIF.

  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.
      IF ZTREQST-ZFEDIST = 'S'.
        MESSAGE W999 WITH ZTREQHD-ZFREQNO 'EDI open request'  'Open'.
      ELSE.
        MESSAGE W999 WITH ZTREQHD-ZFREQNO 'EDI Data Create'  'Open'.
      ENDIF.
    WHEN 'O'.
      IF ZTREQST-ZFEDIST = 'R'.
        MESSAGE W999 WITH ZTREQHD-ZFREQNO 'EDI open' 'Open'.
        EXIT.
      ELSE.
        IF SY-CALLD EQ 'X'.
          MESSAGE S998 WITH ZTREQHD-ZFREQNO 'Non-EDI open'.
        ELSE.
          MESSAGE I998 WITH ZTREQHD-ZFREQNO 'Non-EDI open'.
        ENDIF.
      ENDIF.
    WHEN 'C'.
      MESSAGE E999 WITH ZTREQHD-ZFREQNO 'Cancel'  'Open'.
    WHEN 'A'.
      MESSAGE E999 WITH ZTREQHD-ZFREQNO 'Amend'  'Open'.
  ENDCASE.

  W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_OPEN_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------
FORM P2000_DELETE_MESSAGE.

    PERFORM P2000_MESSAGE_BOX USING  'Delete confirm'
                                 'Being deleted the current document.'
                                 'Do you want to delete?'
                                 'N'
                                 '1'.

ENDFORM.                    " P2000_DELETE_MESSAGE

*&---------------------------------------------------------------------
*
*&      Form  P2000_DEL_CANCEL_MESSAGE
*&---------------------------------------------------------------------
*
FORM P2000_DEL_CANCEL_MESSAGE.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'ºÎº¸ÀÇ·Ú Ãë¼Ò'
                         'ÀûÇÏº¸Çè ºÎº¸ÀÇ·Ú¸¦ Ãë¼ÒÇÕ´Ï´Ù.'
                         'Ãë¼Ò ÇÏ½Ã°Ú½À´Ï±î?'               " MSG2
                         'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                         '1'.                " default button
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
        'Covering request cancel'
        'Now do cancel the insurance covering request.'
        'Do you want to cancel?'
        'N'
        '1'.
  ENDIF.

ENDFORM.                    " P2000_DEL_CANCEL_MESSAGE

*&---------------------------------------------------------------------
*&      Form  P2000_REVOCK_MESSAGE
*&---------------------------------------------------------------------
FORM P2000_REVOCK_MESSAGE.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'Á¢¼öÃë¼Ò È®ÀÎ'
                         'Document(¼öÀÔÀÇ·Ú)¸¦ Á¢¼öÃë¼Ò ÇÕ´Ï´Ù.'
                         'Á¢¼öÃë¼Ò ÇÏ½Ã°Ú½À´Ï±î?'           " MSG2
                         'N'                 " Ãë¼Ò ¹öÆ° À¯/?
                         '1'.                " default button
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
        'Acceptance cancel confirm'
        'Do cancel the acceptence of the import request.'
        'Do you want to cancel the acceptance?'
        'N'
        '1'.
  ENDIF.
ENDFORM.                    " P2000_REVOCK_MESSAGE
*&---------------------------------------------------------------------
*&      Form  P2000_SET_INDICATE
*&---------------------------------------------------------------------
FORM P2000_SET_INDICATE.
  W_OK_CODE = OK-CODE.

  PERFORM P2000_AUTHORITY_CHECK USING W_OK_CODE.
* Lock Object Set
  IF W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D.
    CASE SY-TCODE.
      WHEN 'ZIM02' OR 'ZIM05' OR 'ZIM07' OR 'ZIM03'.
        W_AMDNO   =    '00000'.
        PERFORM P2000_SET_REQDOC_LOCK    USING    'L' ZTREQST-ZFAMDNO.
* LOCAL OFFER SHEET
      WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
        PERFORM P2000_SET_OFFER_DOC_LOCK USING    'L'.

      WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44' OR
           'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
        PERFORM P2000_SET_INS_DOC_LOCK USING 'L'.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
* MESSAGE BOX
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.                                               " Yes...
      CASE W_OK_CODE.
        WHEN 'LGIS'.
          PERFORM P3000_LGI_SEND.
        WHEN 'EDIS'.
          CASE SY-TCODE.
            WHEN 'ZIM02' OR 'ZIM05' OR 'ZIM07' OR 'ZIM03'.
              PERFORM P3000_EDI_SEND.
            WHEN 'ZIM12' OR 'ZIM15' OR 'ZIM17' OR 'ZIM13'.
              PERFORM P3000_AMEND_EDI_SEND.
            WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
              PERFORM P3000_INS_EDI_SEND.
            WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
              PERFORM P3000_INS_AMD_EDI_SEND.
            WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
              PERFORM P3000_OFFER_EDI_SEND.
          ENDCASE.
        WHEN 'DELE'.
          CASE SY-TCODE.
            WHEN 'ZIM02' OR 'ZIM05' OR 'ZIM07' OR 'ZIM03'.
              PERFORM P2000_DEL_STATUS_CHECK.
            WHEN 'ZIM12' OR 'ZIM15' OR 'ZIM17' OR 'ZIM13'.
              PERFORM P2000_AMD_DEL_STATUS_CHECK.
            WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44' OR
                 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
              IF ZTINS-ZFDOCST EQ 'O'.
                MESSAGE E104 WITH ZTINS-ZFREQNO ZTINS-ZFAMDNO
                                  ZTINS-ZFDOCST.
              ENDIF.
              SELECT COUNT( * ) INTO W_COUNT FROM ZTINS
                           WHERE ZFREQNO EQ ZTINS-ZFREQNO
                           AND   ZFAMDNO GT ZTINS-ZFAMDNO.
              IF W_COUNT GT 0.
                MESSAGE E108 WITH ZTINS-ZFREQNO ZTINS-ZFAMDNO
                                  W_COUNT.
              ENDIF.
              ">> Insuarance Cost existence Yes/No
              SELECT SINGLE * FROM ZTIMIMG00.
              IF ZTIMIMG00-ZFPSMS EQ '2'.
                SELECT COUNT( * ) INTO W_COUNT FROM ZTBSEG
                 WHERE ZFIMDNO EQ ZTINS-ZFREQNO
                   AND ZFCSTGRP  EQ '003'
                   AND   ZFCD    EQ '1AB'.
                IF W_COUNT > 0. MESSAGE E469. ENDIF.
              ENDIF.
              SELECT COUNT( * ) INTO W_COUNT
              FROM   ZTRECST AS A INNER JOIN ZTIMIMG08 AS B
              ON     A~ZFCSCD  EQ  B~ZFCD
              WHERE  B~ZFCDTY  EQ '003'
              AND    A~ZFREQNO EQ ZTINS-ZFREQNO
              AND NOT ( A~ZFACDO  IS NULL
              OR        A~ZFACDO  EQ SPACE ).
              IF W_COUNT > 0. MESSAGE E469. ENDIF.
          ENDCASE.
        WHEN 'REVK'.
          CASE SY-TCODE.
            WHEN 'ZIM02' OR 'ZIM05' OR 'ZIM07' OR 'ZIM03' OR
                 'ZIM12' OR 'ZIM15' OR 'ZIM17' OR 'ZIM13'.
              IF ZTREQST-ZFDOCNO IS INITIAL. MESSAGE E186.  ENDIF
.
              IF ZTREQST-ZFDOCST EQ 'O'.
                MESSAGE E104 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                  ZTREQST-ZFEDIST.
              ENDIF.
              ZTREQST-ZFDOCST = 'N'.   ZTREQST-ZFEDIST = 'N'.
            WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44' OR
                 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
              IF ZTINS-ZFEDIST NE 'S'.
                MESSAGE E104 WITH ZTINS-ZFREQNO  ZTINS-ZFAMDNO
                                  ZTINS-ZFEDIST.
              ENDIF.
              CALL FUNCTION 'ZIM_INSURANCE_LGI_DATA_CHECK'
                   EXPORTING
                        ZFREQNO    = ZTINS-ZFREQNO
                        ZFINSEQ    = ZTINS-ZFINSEQ
                        ZFAMDNO    = ZTINS-ZFAMDNO
                   EXCEPTIONS
                        NOT_FOUND  = 4
                        NOT_SELECT = 8.
              CASE SY-SUBRC.
                WHEN 4.
                  MESSAGE E169 WITH ZTINS-ZFREQNO.
                WHEN 8.
                  ZTINS-ZFDOCST = 'N'.
                  ZTINS-ZFEDIST = 'N'.
                WHEN 0.
                  MESSAGE E605(ZIM1).
              ENDCASE.

              ZTINS-ZFDOCST = 'N'.   ZTINS-ZFEDIST = 'N'.
          ENDCASE.
        WHEN 'DELR'.
          CASE SY-TCODE.
            WHEN 'ZIM02' OR 'ZIM05' OR 'ZIM07' OR 'ZIM03' OR
                 'ZIM12' OR 'ZIM15' OR 'ZIM17' OR 'ZIM13'.
* Import Config Select
              SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
              IF SY-SUBRC NE 0.  MESSAGE E961.  ENDIF.
              IF ZTIMIMG00-ZFRELYN2 EQ 'X'.
                IF ZTREQST-ZFRLST2 EQ 'R'.
                  MESSAGE E212.
                ENDIF.
              ENDIF.
              IF ZTREQST-ZFDOCST NE 'N'.
                MESSAGE E104 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                  ZTREQST-ZFDOCST.
              ENDIF.
              IF ZTREQST-ZFRVDT IS INITIAL OR
                ZTREQST-ZFRVDT EQ '00000000'.
                MESSAGE E102 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
              ENDIF.
              IF ZTREQST-ZFEDIST NE 'N'.
                MESSAGE E105 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                  ZTREQST-ZFEDIST.
              ENDIF.
              CLEAR : ZTREQST-ZFRVDT.
            WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44' OR
                 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
          ENDCASE.
        WHEN 'OPCL'.
          CASE SY-TCODE.
            WHEN 'ZIM02' OR 'ZIM05' OR 'ZIM07' OR 'ZIM03' OR
                 'ZIM12' OR 'ZIM15' OR 'ZIM17' OR 'ZIM13'.
              IF ZTREQST-ZFEDIST EQ 'R'.
                MESSAGE E105 WITH ZTREQST-ZFREQNO  ZTREQST-ZFAMDNO
                                  ZTREQST-ZFEDIST.
              ENDIF.
              IF ZTREQST-ZFDOCST NE 'O'.
                MESSAGE E103 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
              ENDIF.
              ZTREQST-ZFDOCST = 'N'.
              IF ZTREQST-ZFAMDNO EQ '00000'.
                CLEAR : ZTREQHD-ZFLASTED, ZTREQHD-ZFLASTSD,
                        ZTREQHD-ZFOPNNO,
                        ZTREQST-ZFOPNNO,  ZTREQST-ZFOPNDT.
              ELSE.
                MOVE : ZTREQHD_TMP-ZFLASTED TO ZTREQHD-ZFLASTED,
                       ZTREQHD_TMP-ZFLASTSD TO ZTREQHD-ZFLASTSD,
                       ZTREQHD_TMP-ZFOPNNO TO ZTREQHD-ZFOPNNO.
                CLEAR : ZTREQST-ZFOPNNO,  ZTREQST-ZFOPNDT.
              ENDIF.

              W_NEW_DOCST = 'N'.
            WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44' OR
                 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
              IF ZTINS-ZFEDIST EQ 'R'.
                MESSAGE E105 WITH ZTINS-ZFREQNO  ZTINS-ZFAMDNO
                                  ZTINS-ZFDOCST.
              ENDIF.
              IF ZTINS-ZFDOCST NE 'O'.
                MESSAGE E103 WITH ZTINS-ZFREQNO ZTINS-ZFAMDNO.
              ENDIF.
              ZTINS-ZFDOCST = 'N'.
              W_NEW_DOCST = 'N'.
          ENDCASE.
      ENDCASE.

      PERFORM  P3000_DB_MODIFY_SCRCOM.
      CLEAR OK-CODE.
      PERFORM  P2000_SET_UNLOCK.
      PERFORM  P2000_SET_SCREEN_SCRCOM.
      LEAVE SCREEN.
    WHEN OTHERS.              " No...
      IF W_STATUS EQ C_REQ_D.
        PERFORM  P2000_SET_UNLOCK.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_SET_INDICATE
*&---------------------------------------------------------------------
*&      Form  P2000_CONVERT_TO_USD_CURR
*&---------------------------------------------------------------------
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
*&---------------------------------------------------------------------
*&      Form  P2000_OPEN_DOC_ITEM_SELECT
*&---------------------------------------------------------------------
FORM P2000_OPEN_DOC_ITEM_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE  ZFOPNNO   EQ ZSREQHD-ZFOPNNO
           AND    ZFAMDNO EQ '00000'
           ORDER  BY ZFREQNO.

*----------------------------------------------------------------------
* position
*----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LCDISPLY'.
  CALL SCREEN 0014 STARTING AT  04 3
                   ENDING   AT  97 16.

ENDFORM.                    " P2000_OPEN_DOC_ITEM_SELECT
*&---------------------------------------------------------------------
*&      Form  P2000_PO_UNLOCK
*&---------------------------------------------------------------------
FORM P2000_PO_UNLOCK.

  CALL FUNCTION 'DEQUEUE_EMEKKOE'
       EXPORTING
            EBELN = ZTREQHD-EBELN.

ENDFORM.                    " P2000_PO_UNLOCK
*&---------------------------------------------------------------------
*&      Form  P2000_USD_CONVERT_AMOUNT
*&---------------------------------------------------------------------
FORM P2000_USD_CONVERT_AMOUNT   USING    PA_MODE.
* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  DESCRIBE TABLE IT_ZSREQIT   LINES G_PARAM_LINE.
  IF G_PARAM_LINE < 1.
    IF PA_MODE EQ 'E'.
      MESSAGE ID 'ZIM' TYPE PA_MODE NUMBER '901'.
    ENDIF.
  ENDIF.

  W_TOT_AMOUNT = 0.    W_COUNT      = 0.    W_TOT_ITEM   = 0.
  CLEAR : ZSREQIT.
*----------------------------------------------------------------------
* Delivery Date
*----------------------------------------------------------------------
  CLEAR : ZTREQHD-ZFMAUD, ZTREQHD-ZFRECYN.

  LOOP AT IT_ZSREQIT WHERE MENGE > 0.
    W_TABIX = SY-TABIX.
    W_COUNT = W_COUNT + 1.
    IF W_COUNT EQ 1.
      MOVE-CORRESPONDING IT_ZSREQIT TO ZSREQIT.
      ZTREQHD-ZFMAUD = ZSREQIT-ZFEEIND.
    ENDIF.

* INSTALLING CHG.
    IF IT_ZSREQIT-KPEIN EQ 0.
      IT_ZSREQIT-KWERT = 0.
    ELSE.
      IT_ZSREQIT-KWERT = ( IT_ZSREQIT-KBETR / IT_ZSREQIT-KPEIN ) *
                           IT_ZSREQIT-MENGE.
    ENDIF.

* Import Request Amount
    IF IT_ZSREQIT-PEINH NE 0 AND IT_ZSREQIT-BPUMN NE 0.
      W_TOT_AMOUNT = W_TOT_AMOUNT +
                 ( IT_ZSREQIT-MENGE *
                 ( IT_ZSREQIT-BPUMZ / IT_ZSREQIT-BPUMN )
               * ( IT_ZSREQIT-NETPR / IT_ZSREQIT-PEINH ) ).
    ENDIF.
    W_TOT_ITEM   = W_TOT_ITEM   +   IT_ZSREQIT-MENGE.

* Latest Delivery Date
    IF ZTREQHD-ZFMAUD > IT_ZSREQIT-ZFEEIND.
      ZTREQHD-ZFMAUD = IT_ZSREQIT-ZFEEIND.
    ENDIF.

    IF NOT IT_ZSREQIT-STAWN IS INITIAL.
      SELECT * FROM ZTIMIMG09 UP TO 1 ROWS
                              WHERE STAWN    EQ IT_ZSREQIT-STAWN
                              AND   ZFAPLDT  <= SY-DATUM
                              ORDER BY ZFAPLDT. "DESCENDING.
        EXIT.
      ENDSELECT.
      IF SY-SUBRC EQ 0.
        IT_ZSREQIT-ZFIRLW =  ZTIMIMG09-ZFIRLW.
        IF NOT ZTIMIMG09-ZFIRLW IS INITIAL.
          ZTREQHD-ZFRECYN = 'X'.
        ENDIF.
        MODIFY IT_ZSREQIT  INDEX  W_TABIX.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC' OR 'DA' OR 'DP' OR 'TT' OR 'GS'.
      LOOP AT IT_ZSREQIT.
        READ TABLE IT_ZSREQIL
                   WITH KEY ZFILSEQ = IT_ZSREQIT-ZFITMNO.
        IF SY-SUBRC NE 0.
          MOVE-CORRESPONDING  IT_ZSREQIT  TO   IT_ZSREQIL.
          MOVE IT_ZSREQIT-ZFITMNO         TO   IT_ZSREQIL-ZFILSEQ.
          APPEND IT_ZSREQIL.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

** Packing Chg. & Handing CHG.
  W_TOT_AMOUNT = W_TOT_AMOUNT + ZTREQHD-ZFPKCHG + ZTREQHD-ZFHDCHG.

* Representive Goods Description
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
* AMOUNT CHECK...
  IF W_TOT_AMOUNT <= 0.
    IF PA_MODE EQ 'E'.
      MESSAGE ID 'ZIM' TYPE PA_MODE NUMBER '076'.
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
            W_AMOUNT = W_AMOUNT * -1.
            MOVE : '-'               TO  ZTMLCAMHD-ZFIDCD,
                   W_AMOUNT          TO  ZTMLCAMHD-ZFIDAM,
                   W_TOT_AMOUNT      TO  ZTMLCAMHD-ZFNDAMT.
          ENDIF.
        ENDIF.
      WHEN 'LO'.
        W_AMOUNT = W_TOT_AMOUNT.
        IF W_AMOUNT NE 0.
          ZTLLCAMHD-ZFNOAMT = W_AMOUNT.
          ZTLLCAMHD-WAERS   = ZTREQHD-WAERS.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
  PERFORM   P2000_CONV_OTHER_CURR_TO_USD   USING  W_TOT_AMOUNT
                                                  ZTREQHD-WAERS
                                         CHANGING W_LOCAL_AMT.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        ZTMLCHD-WAERS    = ZTREQHD-WAERS.
        ZTMLCHD-ZFOPAMT  = W_TOT_AMOUNT.
      ENDIF.

    WHEN 'PU'.
      ZTPUR-ZFUSD     = W_USD.
      ZTPUR-ZFTOCNM   =  ZSREQIT-MEINS.
      ZTPUR-ZFTOCN    =  W_TOT_ITEM.
      ZTPUR-ZFTOAM    =  W_TOT_AMOUNT.
      ZTPUR-ZFTOAMC   =  ZTREQHD-WAERS.
      ZTPUR-ZFTDAMC   =  ZTREQHD-WAERS.
      ZTPUR-ZFTOAMU   =  W_LOCAL_AMT.
      IF NOT ZTPUR-ZFTDAM IS INITIAL.
        PERFORM   P2000_CONV_OTHER_CURR_TO_USD
                                 USING     ZTPUR-ZFTDAM
                                           ZTREQHD-WAERS
                                  CHANGING ZTPUR-ZFTDAMU.
      ENDIF.
* Goods Description
      LOOP AT IT_ZSPURSG1.
        W_TABIX = SY-TABIX.
        IF NOT IT_ZSPURSG1-NETPR IS INITIAL.
          PERFORM   P2000_CONV_OTHER_CURR_TO_USD
                                   USING     IT_ZSPURSG1-NETPR
                                             ZTREQHD-WAERS
                                    CHANGING IT_ZSPURSG1-ZFNETPRU.
        ENDIF.
        IF NOT IT_ZSPURSG1-ZFGOAMT IS INITIAL.
          PERFORM   P2000_CONV_OTHER_CURR_TO_USD
                                   USING    IT_ZSPURSG1-ZFGOAMT
                                            ZTREQHD-WAERS
                                   CHANGING IT_ZSPURSG1-ZFGOAMTU.
        ENDIF.
        IF NOT IT_ZSPURSG1-ZFDAM IS INITIAL.
          PERFORM   P2000_CONV_OTHER_CURR_TO_USD
                                    USING    IT_ZSPURSG1-ZFDAM
                                             ZTREQHD-WAERS
                                    CHANGING IT_ZSPURSG1-ZFDAMU.
        ENDIF.

        MODIFY IT_ZSPURSG1 INDEX W_TABIX.
      ENDLOOP.

    WHEN 'LO'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        ZTLLCHD-ZFOPAMT  = W_TOT_AMOUNT.
        ZTLLCHD-ZFOPAMTC = ZTREQHD-WAERS.

        PERFORM    P2000_CONVERT_TO_USD_CURR    USING W_TOT_AMOUNT
                                                ZTREQHD-WAERS
                                                W_KRW
                                       CHANGING W_LOCAL_AMT1
                                                W_RATE
                                                W_FIXED_RATE.
        ZTLLCHD-ZFOPKAM = W_LOCAL_AMT1.
      ENDIF.

  ENDCASE.

  ZTREQHD-ZFLASTAM = W_TOT_AMOUNT.
  ZTREQHD-ZFUSDAM  = W_LOCAL_AMT.
  ZTREQHD-ZFUSD    = W_USD.

ENDFORM.                    " P2000_USD_CONVERT_AMOUNT
*&---------------------------------------------------------------------
*&      Form  P3000_ITEM_DESC_WRITE
*&---------------------------------------------------------------------
FORM P3000_ITEM_DESC_WRITE USING      W_DESC_TEXT
                           CHANGING   W_ZFLSG7G.

  W_ZFLSG7G = W_ZFLSG7G + 10.

  MOVE : W_ZFLSG7G        TO    IT_ZSMLCSG7G-ZFLSG7G,
         W_DESC_TEXT      TO    IT_ZSMLCSG7G-ZFDSOG1.
  CLEAR : IT_ZSMLCSG7G-LOEKZ.

  APPEND  IT_ZSMLCSG7G.

ENDFORM.                    " P3000_ITEM_DESC_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_BANK_CHARGE_MOVE
*&---------------------------------------------------------------------
FORM P2000_BANK_CHARGE_MOVE.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF ZTREQHD-ZFCHG EQ 'S'.                                " Seller
      ZTMLCHD-ZFCHG = '19'.
    ELSEIF ZTREQHD-ZFCHG EQ 'B'.                            " Buyer
      ZTMLCHD-ZFCHG = '16'.
    ENDIF.
  ENDIF.
ENDFORM.                    " P2000_BANK_CHARGE_MOVE
*&---------------------------------------------------------------------
*&      Form  P2000_PARTIAL_SHIPMENT_MOVE
*&---------------------------------------------------------------------
FORM P2000_PARTIAL_SHIPMENT_MOVE.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF ZTREQHD-ZZPSHIP EQ 'X'.
      ZTMLCHD-ZFPRMT = '9'.
    ELSE.
      ZTMLCHD-ZFPRMT = '10'.
    ENDIF.
  ELSEIF ZTREQHD-ZFREQTY EQ 'LO'.
    IF ZTREQHD-ZZPSHIP EQ 'X'.
      ZTLLCHD-ZFPRAL = '9'.
    ELSE.
      ZTLLCHD-ZFPRAL = '10'.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_PARTIAL_SHIPMENT_MOVE
*&---------------------------------------------------------------------
*&      Form  P2000_TRANS_SHIPMENT_MOVE
*&---------------------------------------------------------------------
FORM P2000_TRANS_SHIPMENT_MOVE.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF ZTREQHD-ZZTSHIP EQ 'X'.
      ZTMLCHD-ZFTRMT = '7'.
    ELSE.
      ZTMLCHD-ZFTRMT = '8'.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_TRANS_SHIPMENT_MOVE
*&---------------------------------------------------------------------
*&      Form  P2000_EXPIRY_DATE_CHECK
*&---------------------------------------------------------------------
FORM P2000_EXPIRY_DATE_CHECK.
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        ZTMLCHD-ZFEXDT = ZTREQHD-ZFREQED.
        ZTMLCHD-ZFLTSD = ZTREQHD-ZFREQSD.
      ELSE.
*           IF ZTREQHD-ZFLASTED NE ZTREQHD-ZFREQED.
        IF ZTREQHD_TMP-ZFREQED NE ZTREQHD-ZFREQED.
          ZTMLCAMHD-ZFNEXDT = ZTREQHD-ZFREQED.
        ELSE.
          CLEAR : ZTMLCAMHD-ZFNEXDT.
        ENDIF.
*           IF ZTREQHD-ZFLASTSD NE ZTREQHD-ZFREQSD.
        IF ZTREQHD_TMP-ZFREQSD NE ZTREQHD-ZFREQSD.
          ZTMLCAMHD-ZFNLTSD = ZTREQHD-ZFREQSD.
        ELSE.
          CLEAR : ZTMLCAMHD-ZFNLTSD.
        ENDIF.
      ENDIF.
      IF ZTREQHD-ZFREQED < ZTREQHD-ZFREQSD.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE E068 WITH ZTREQHD-ZFREQSD ZTREQHD-ZFREQED.
        EXIT.
      ENDIF.
    WHEN 'LO'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        IF NOT ZTREQHD-ZFREQED IS INITIAL.
          ZTLLCHD-ZFEXDT = ZTREQHD-ZFREQED.
        ENDIF.
        IF NOT ZTREQHD-ZFREQSD IS INITIAL.
          IF ZTLLCHD-ZFGDDT IS INITIAL.
            ZTLLCHD-ZFGDDT = ZTREQHD-ZFREQED.
          ENDIF.
        ENDIF.
      ELSE.
        IF ZTREQHD_TMP-ZFREQED  NE ZTREQHD-ZFREQED.
          ZTLLCAMHD-ZFNEXDT = ZTREQHD-ZFREQED.
        ELSE.
          CLEAR : ZTLLCAMHD-ZFNEXDT.
        ENDIF.
        IF ZTREQHD_TMP-ZFREQSD  NE ZTREQHD-ZFREQSD.
          ZTLLCAMHD-ZFNGDDT = ZTREQHD-ZFREQSD.
        ELSE.
          CLEAR : ZTLLCAMHD-ZFNGDDT.
        ENDIF.
      ENDIF.
      IF ZTREQHD-ZFREQED < ZTREQHD-ZFREQSD.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE E125 WITH ZTREQHD-ZFREQSD ZTREQHD-ZFREQED.
        EXIT.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_EXPIRY_DATE_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_REQ_ITEM_CHECK
*&---------------------------------------------------------------------
FORM P2000_REQ_ITEM_CHECK TABLES   IT_ZSREQIT STRUCTURE IT_ZSREQIT
                          USING    ERR_MODE.
** changed by Furong on 15 Aug
* The Mark of Delete
*  IF IT_ZSREQIT-LOEKZ NE SPACE.
*    MESSAGE ID 'ZIM' TYPE ERR_MODE NUMBER 146 WITH IT_ZSREQIT-ZFITMNO.
*  ENDIF.
** end of change
* G/R Mark
  IF IT_ZSREQIT-ELIKZ NE SPACE.
    MESSAGE ID 'ZIM' TYPE 'S' NUMBER 147 WITH IT_ZSREQIT-ZFITMNO.
  ENDIF.
  IF ZTREQHD-ZFREQTY EQ 'LC' OR ZTREQHD-ZFREQTY EQ 'DA' OR
     ZTREQHD-ZFREQTY EQ 'DP' OR ZTREQHD-ZFREQTY EQ 'TT'.
* HS-CODE Check
   IF IT_ZSREQIT-STAWN IS INITIAL AND IT_ZSREQIT-MENGE GT 0.
      MESSAGE ID 'ZIM' TYPE ERR_MODE NUMBER 073 WITH IT_ZSREQIT-ZFITMNO.
   ENDIF.
  ENDIF.
* Import Reuest Check
  CLEAR : ZTIMIMG09.
  SELECT * FROM ZTIMIMG09 WHERE STAWN    EQ IT_ZSREQIT-STAWN
                          AND   ZFAPLDT  <= SY-DATUM
                          ORDER BY ZFAPLDT. "DESCENDING.
    EXIT.
  ENDSELECT.
  IT_ZSREQIT-ZFIRLW = ZTIMIMG09-ZFIRLW.

  W_OLD_MENGE = 0.
  IF SY-TCODE NE 'ZIM01'.
    SELECT SINGLE MENGE INTO W_OLD_MENGE FROM ZTREQIT
                        WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                        AND   ZFITMNO EQ IT_ZSREQIT-ZFITMNO.
  ENDIF.

  IF IT_ZSREQIT-MENGE IS INITIAL.
    W_MENGE = IT_ZSREQIT-ZFPOMENGE - IT_ZSREQIT-ZFLCMENGE
                                   + W_OLD_MENGE.
    IF W_MENGE NE 0 AND IT_ZSREQIT-MENGE EQ 0.
      MESSAGE ID 'ZIM' TYPE ERR_MODE NUMBER 074
                                           WITH IT_ZSREQIT-ZFITMNO.
    ENDIF.
  ELSE.
    W_MENGE = IT_ZSREQIT-ZFPOMENGE - IT_ZSREQIT-ZFLCMENGE
                                   + W_OLD_MENGE - IT_ZSREQIT-MENGE.
    IF W_MENGE < 0 AND IT_ZSREQIT-MENGE > 0.
      MESSAGE ID 'ZIM' TYPE ERR_MODE NUMBER 075
                                           WITH IT_ZSREQIT-ZFITMNO.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_REQ_ITEM_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_OPEN_CANCEL_MESSAGE
*&---------------------------------------------------------------------
FORM P2000_OPEN_CANCEL_MESSAGE.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'Open Cancel È®ÀÎ'
                         'ÇöÀç Document¸¦ È®Á¤(Open)Ãë¼Ò ÇÕ´Ï´Ù.'
                         'È®Á¤(Open)Ãë¼Ò ÇÏ½Ã°Ú½À´Ï±î?'
                         'N'
                         '1'.
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
        'Open Cancel confirm'
        'Being canceled the current opened document.'
        'Do you want to cancel?'
        'N'
        '1'.
  ENDIF.

ENDFORM.                    " P2000_OPEN_CANCEL_MESSAGE
*&---------------------------------------------------------------------
*&      Form  P2000_DEL_AUTHORITY_CHECK
*&---------------------------------------------------------------------
FORM P2000_AUTHORITY_CHECK USING    P_OK_CODE.

  IF SY-SUBRC NE 0.
    CASE P_OK_CODE.
      WHEN 'EDIS'.
        MESSAGE E077 WITH SY-UNAME 'EDI Send'.
      WHEN 'DELE'.
        MESSAGE E077 WITH SY-UNAME 'Delete'.
      WHEN 'REVK'.
        MESSAGE E077 WITH SY-UNAME 'Delete Cancel'.
      WHEN 'DELR'.
        MESSAGE E077 WITH SY-UNAME 'Registration cancel'.
      WHEN 'OPCL'.
        MESSAGE E077 WITH SY-UNAME 'Open Cancel'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------
*&      Form  P2000_DEL_STATUS_CHECK
*&---------------------------------------------------------------------
FORM P2000_DEL_STATUS_CHECK.
  CLEAR W_COUNT.

  IF ZTREQST-ZFDOCST NE 'N'.
    MESSAGE E104 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                      ZTREQST-ZFDOCST.
  ENDIF.
  IF ZTREQST-ZFEDIST NE 'N'.
    MESSAGE E105 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                      ZTREQST-ZFEDIST.
  ENDIF.
*----------------------------------------------------------------------
* Follow Document Existence
* Desc : L/C Cost, Amend, Invoice Existence
*----------------------------------------------------------------------
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFPSMS EQ '2'.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTBSEG
                      WHERE ZFIMDNO   EQ ZTREQST-ZFREQNO
                      AND   ZFCSTGRP  EQ '003'
                      AND   ZFCD      NE '1AB'.
    IF W_COUNT > 0.
      MESSAGE E122 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
    ENDIF.
  ELSE.
* L/C COST
    SELECT COUNT( * ) INTO W_COUNT FROM ZTRECST
                      WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
    IF W_COUNT > 0.
      MESSAGE E122 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
    ENDIF.
* FI Document
    SELECT COUNT( * ) INTO W_COUNT FROM ZTRECST
                     WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
    IF W_COUNT > 0. MESSAGE E462. ENDIF.
  ENDIF.
* Insuarance Information
  SELECT COUNT( * ) INTO W_COUNT FROM ZTINS
                    WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
  IF W_COUNT > 0.
    MESSAGE E108 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
  ENDIF.
* LOCAL OFFER SHEET
  IF ZTREQHD-ZFREQTY EQ 'LO'.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTOFF
                      WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
    IF W_COUNT > 0.
      MESSAGE E109 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
    ENDIF.
  ENDIF.
* Amend
  SELECT COUNT( * ) INTO W_COUNT FROM ZTREQST
                    WHERE ZFREQNO EQ ZTREQST-ZFREQNO
                    AND   ZFAMDNO GT ZTREQST-ZFAMDNO.

  IF W_COUNT > 0.
    MESSAGE E106 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
  ENDIF.
* Commercial Invoice Check!
  SELECT COUNT( * ) INTO W_COUNT FROM ZTCIVIT
                    WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
  IF W_COUNT > 0.
    MESSAGE E394 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
  ENDIF.
* B/L CHECK
  SELECT COUNT( * ) INTO W_COUNT FROM ZTBLIT
                    WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
  IF W_COUNT > 0.
    MESSAGE E269 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
  ENDIF.
* Invoice
  SELECT COUNT( * ) INTO W_COUNT FROM ZTIVIT
                    WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
  IF W_COUNT > 0.
    MESSAGE E107 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
  ENDIF.

ENDFORM.                    " P2000_DEL_STATUS_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_FIELD_MODE_SET
*&---------------------------------------------------------------------
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
*&---------------------------------------------------------------------
*&      Form  P2000_SCR_MODE_SET
*&---------------------------------------------------------------------
FORM P2000_SCR_MODE_SET.
  DATA : L_CHK   VALUE   'Y'.

  " Import IMG DATA GET.
  CLEAR : ZTIMIMGTX, ZTIMIMG00.
  SELECT SINGLE * FROM ZTIMIMGTX WHERE BUKRS  EQ  ZTREQHD-BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.

  LOOP AT SCREEN.

    CASE W_STATUS.
      WHEN C_REQ_C OR C_REQ_U.
        IF SCREEN-GROUP1 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN  C_ADD_U.
        IF SCREEN-GROUP2 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_REQ_D OR C_ADD_D OR C_OPEN_D.
        IF SCREEN-GROUP1 = 'I'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      WHEN C_OPEN_C OR C_OPEN_U.
        IF SCREEN-GROUP3 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_INSU_I.
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

*-----------------------------------------------------------------------
* Import Request( General Screen )
*-----------------------------------------------------------------------
*>> Allocation Bank
    IF ZTIMIMG00-ZFBKYN EQ 'X' AND SY-TCODE(4) EQ 'ZIM0' AND
       ZTREQHD-ZFUSDAM  GE ZTIMIMG00-ZFUSDAM.

      IF SY-DYNNR EQ '0102' OR SY-DYNNR EQ '0152'.
        L_CHK = 'Y'.
        CASE ZTREQHD-ZFREQTY.
          WHEN 'LC'.
            IF ZTIMIMG00-LCBKYN EQ 'X'.
              L_CHK = 'N'.
            ENDIF.
          WHEN 'LO'.
            IF ZTIMIMG00-LOBKYN EQ 'X'.
              L_CHK = 'N'.
            ENDIF.
          WHEN 'TT'.
            IF ZTIMIMG00-TTBKYN EQ 'X'.
              L_CHK = 'N'.
            ENDIF.
          WHEN 'PU'.
            IF ZTIMIMG00-PUBKYN EQ 'X'.
              L_CHK = 'N'.
            ENDIF.
          WHEN 'DA'.
            IF ZTIMIMG00-DABKYN EQ 'X'.
              L_CHK = 'N'.
            ENDIF.
          WHEN 'DP'.
            IF ZTIMIMG00-DPBKYN EQ 'X'.
              L_CHK = 'N'.
            ENDIF.
          WHEN 'GSM'.
            IF ZTIMIMG00-GSMBKYN EQ 'X'.
              L_CHK = 'N'.
            ENDIF.

        ENDCASE.
        IF SCREEN-NAME EQ 'ZTREQHD-ZFOPBN' AND L_CHK EQ 'N'.
          SCREEN-INPUT = '0'.
        ENDIF.
      ENDIF.

*----------------------------------------------------------------------
* No Allocation Bank
*----------------------------------------------------------------------
      IF ZTREQHD-ZFOPBN IS INITIAL.
*>> MASTER L/C
        IF SY-DYNNR EQ '0104'.
          IF SCREEN-NAME EQ 'ZTMLCHD-ZFOBNM' OR
             SCREEN-NAME EQ 'ZTMLCHD-ZFOBBR' OR
             SCREEN-NAME EQ 'ZTMLCHD-ZFOPBNCD' OR
             SCREEN-NAME EQ 'ZTMLCHD-ZFOBPH' OR
             SCREEN-NAME EQ 'PB_TEXT'.
            SCREEN-INPUT = '0'.
          ENDIF.
        ENDIF.

*>> LOCAL L/C
        IF SY-DYNNR EQ '0114'.
          IF SCREEN-NAME EQ 'ZTLLCHD-ZFOBNM' OR
             SCREEN-NAME EQ 'ZTLLCHD-ZFOBBR' OR
             SCREEN-NAME EQ 'PB_CAD1'.
            SCREEN-INPUT = '0'.
          ENDIF.
        ENDIF.
*>> Purchase Confirm
        IF SY-DYNNR EQ '0125'.
          IF SCREEN-NAME EQ 'ZTPUR-ZFACNM'   OR
             SCREEN-NAME EQ 'ZTPUR-ZFACBR'   OR
             SCREEN-NAME EQ 'ZTPUR-ZFACBNCD' OR
             SCREEN-NAME EQ 'PB_CADD'.
            SCREEN-INPUT = '0'.
          ENDIF.
        ENDIF.
*>> T/T
        IF SY-DYNNR EQ '0141'.
          IF SCREEN-NAME EQ 'ZTTTHD-ZFOBAK'   OR
             SCREEN-NAME EQ 'ZTTTHD-ZFOPBNCD' OR
             SCREEN-NAME EQ 'ZTTTHD-ZFOBNM'   OR
             SCREEN-NAME EQ 'ZTTTHD-ZFOBBR'   OR
             SCREEN-NAME EQ 'ZTTTHD-ZFOPBNCD'.
            SCREEN-INPUT = '0'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*>> Requested Open Date
    IF SY-DYNNR EQ '0102' OR SY-DYNNR EQ '0152'.
      IF ZTIMIMG00-ZFRELYN1 NE 'X'.
        IF SCREEN-NAME EQ 'ZTREQST-ZFREQDT' OR
           SCREEN-NAME EQ 'ZTREQST-ERNAM'.
          SCREEN-INVISIBLE = '1'.
          SCREEN-INPUT = '0'.
        ENDIF.
      ENDIF.
      IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
        IF SCREEN-NAME EQ 'ZTREQHD-ZFMATGB' OR
           SCREEN-NAME EQ 'ZTREQHD-ZFLCKN'.
          ZTREQHD-ZFMATGB = '2'.
          ZTREQHD-ZFLCKN  = '8'.
          SCREEN-INPUT = '0'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '1102'.
      IF ZTIMIMG00-ZFRELYN3 NE 'X'.
        IF SCREEN-NAME EQ 'ZTREQST-ZFREQDT' OR
           SCREEN-NAME EQ 'ZTREQST-ERNAM'.
          SCREEN-INVISIBLE = '1'.
          SCREEN-INPUT = '0'.
        ENDIF.
      ENDIF.
    ENDIF.
*>> Local -> Exchange Rate Visible
    IF SY-DYNNR EQ '0102' OR SY-DYNNR EQ '0152' OR SY-DYNNR EQ '1102'.

      IF W_STATUS NE C_REQ_D.

*> Currency Field Disable .
        IF SCREEN-NAME EQ 'ZTREQHD-ZFPKCUR' OR
           SCREEN-NAME EQ 'ZTREQHD-ZFHDCUR'.
          SCREEN-INPUT = '0'.
        ENDIF.

*> Purchase Confirm & Local L/C Exchange Rate visible
        IF NOT ( ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU' ).
          IF SCREEN-NAME EQ 'ZTREQHD-KURSF' OR
             SCREEN-NAME EQ 'ZTREQHD-FFACT'.
            SCREEN-INVISIBLE = '1'.
            SCREEN-INPUT = '0'.
          ENDIF.
        ENDIF.

        IF ZTREQHD-ZFPREPAY GT 0.
          IF SCREEN-NAME EQ 'ZTREQHD-KURSF'.
            SCREEN-INVISIBLE = '1'.
            SCREEN-INPUT = '0'.
          ENDIF.
        ENDIF.
      ENDIF.
*>> LC
      IF ZTREQHD-ZFREQTY NE 'LC'.
        IF SCREEN-NAME EQ 'ZTREQHD-ZFSHCU'   OR
           SCREEN-NAME EQ 'ZTREQHD-ZFSPRTC'   OR
           SCREEN-NAME EQ 'ZTREQHD-ZFSPRT'   OR
           SCREEN-NAME EQ 'ZTREQHD-ZFARCU'   OR
           SCREEN-NAME EQ 'ZTREQHD-ZFAPRTC'   OR
           SCREEN-NAME EQ 'ZTREQHD-ZFAPRT'   OR
           SCREEN-NAME EQ 'ZTREQHD-ZZTSHIP'  OR
           SCREEN-NAME EQ 'ZTREQHD-ZZPSHIP2'.

          SCREEN-INVISIBLE = '1'.
          SCREEN-INPUT = '0'.
        ENDIF.
        IF SCREEN-NAME EQ 'ZTREQHD-ZFLCKN'.
          SCREEN-INPUT = '0'.
        ENDIF.
        IF ZTREQHD-ZFREQTY NE 'TT' AND
           SCREEN-NAME EQ 'ZTREQHD-ZFCHG'.
          SCREEN-INVISIBLE = '1'.
          SCREEN-INPUT = '0'.
        ENDIF.
      ENDIF.

*>> PU/LO -> Importer/Incoterms/Transportation/Bank Charge/Trial.
      IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
        CASE SCREEN-NAME.
          WHEN 'ZTREQHD-IMTRD'    OR 'ZTREQHD-INCO1' OR
               'ZTREQHD-ZFTRANS'  OR 'ZTREQHD-ZFCHG' OR
               'ZTREQHD-ZFTRIPLE'.
            SCREEN-INVISIBLE = '1'.
            SCREEN-INPUT = '0'.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.

      IF ZTREQHD-ZFREQTY NE 'LO'.
        IF SCREEN-NAME(15) EQ 'ZSREQHD-ZFLLCTY'.
          SCREEN-INVISIBLE = '1'.
          SCREEN-INPUT = '0'.
        ENDIF.
      ENDIF.
      IF ZTREQHD-ZFREQTY NE 'TT'.
        CASE SCREEN-NAME.
          WHEN 'BLOCK12' OR 'ZTREQHD-ZFLTEX1' OR 'ZTREQHD-ZFLTEX2'
                         OR 'ZTREQHD-ZFLTEX3' OR 'ZTREQHD-ZFLTEX4'.
            SCREEN-INVISIBLE = '1'.
            SCREEN-INPUT = '0'.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
      IF ZTREQHD-ZFREQTY NE 'LO' AND ZTREQHD-ZFREQTY NE 'LC'.
        IF SCREEN-NAME EQ 'ZTREQHD-ZFREQSD' OR
           SCREEN-NAME EQ 'ZTREQHD-ZFREQED' OR
           SCREEN-NAME EQ 'ZTREQHD-ZZPSHIP' OR
           SCREEN-NAME EQ 'ZTREQHD-ZZPSHIP1'.
          SCREEN-INVISIBLE = '1'.
          SCREEN-INPUT = '0'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '4102'.
      IF ZTINS-BELNR IS INITIAL.
        IF ( SCREEN-NAME(11) EQ 'ZTINS-BUKRS' OR
             SCREEN-NAME(11) EQ 'ZTINS-BELNR' OR
             SCREEN-NAME(11) EQ 'ZTINS-GJAHR' ).
          SCREEN-INPUT     =  '0'.
          SCREEN-INVISIBLE =  '1'.
        ENDIF.
      ELSE.
        SCREEN-INVISIBLE = '0'.
      ENDIF.
    ENDIF.

    IF SY-DYNNR EQ '0192' AND
       ( SCREEN-NAME(12) EQ 'SPOP-OPTION1' OR
         SCREEN-NAME(12) EQ 'SPOP-OPTION2' ).
      SCREEN-INPUT  =  '1'.
    ENDIF.

    IF ZTIMIMG00-ZFPSMS NE '2' AND SY-DYNNR EQ '0101' AND
       SCREEN-NAME  EQ  'ITM9'.
      SCREEN-INPUT     =  '0'.
      SCREEN-INVISIBLE =  '1'.
    ENDIF.

    IF SY-DYNNR EQ '0141'.
      IF SCREEN-NAME(4) EQ 'ITM3' OR SCREEN-NAME(4) EQ 'ITM4'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
    ENDIF.

    IF ZTIMIMGTX-ZFEDIYN IS INITIAL.
      IF ( SY-DYNNR     EQ '0101'  )      AND
         ( SCREEN-NAME  EQ 'ITM3'         OR
           SCREEN-NAME  EQ 'ITM4'         OR
           SCREEN-NAME  EQ 'ITM5'         OR
           SCREEN-NAME  EQ 'ITM6'         OR
           SCREEN-NAME  EQ 'ITM7'  ).
        SCREEN-INVISIBLE  =  '1'.
      ENDIF.
    ELSEIF ZTIMIMGTX-APP700 IS INITIAL .
      IF ( SY-DYNNR     EQ '0101'  )      AND
        ( SCREEN-NAME  EQ 'ITM3'          OR
          SCREEN-NAME  EQ 'ITM4'          OR
          SCREEN-NAME  EQ 'ITM5'          OR
          SCREEN-NAME  EQ 'ITM6'          OR
          SCREEN-NAME  EQ 'ITM7'   ).
        SCREEN-INVISIBLE  =  '1'.
      ENDIF.
    ENDIF.

    IF ZTIMIMGTX-ZFEDIYN IS INITIAL.
      IF ( SY-DYNNR     EQ '0141'  )      AND
         ( SCREEN-NAME  EQ 'ITM3'         OR
           SCREEN-NAME  EQ 'ITM4'  ).
        SCREEN-INVISIBLE  =  '1'.
      ENDIF.
    ELSEIF ZTIMIMGTX-PAYORD IS INITIAL.
      IF ( SY-DYNNR     EQ '0141'  )      AND
        ( SCREEN-NAME  EQ 'ITM3'          OR
          SCREEN-NAME  EQ 'ITM4'   ).
        SCREEN-INVISIBLE  =  '1'.
      ENDIF.
    ENDIF.

    IF ZTIMIMGTX-ZFEDIYN IS INITIAL.
      IF ( SY-DYNNR     EQ '1101'  )      AND
         ( SCREEN-NAME  EQ 'ITM3'         OR
           SCREEN-NAME  EQ 'ITM4'         OR
           SCREEN-NAME  EQ 'ITM5'  ).
        SCREEN-INVISIBLE  =  '1'.
      ENDIF.
    ELSEIF ZTIMIMGTX-APP707 IS INITIAL .
      IF ( SY-DYNNR    EQ '1101'  )      AND
         ( SCREEN-NAME  EQ 'ITM3'        OR
           SCREEN-NAME  EQ 'ITM4'        OR
           SCREEN-NAME  EQ 'ITM5'  ).
        SCREEN-INVISIBLE  =  '1'.
      ENDIF.
    ENDIF.

    IF ZTIMIMGTX-ZFEDIYN IS INITIAL.
      IF ( SY-DYNNR     EQ '4101' OR SY-DYNNR     EQ '4501'  ) AND
         ( SCREEN-NAME  EQ 'ITM2' OR SCREEN-NAME  EQ 'ITM4'    OR
           SCREEN-NAME  EQ 'ITM3' ).
        SCREEN-INVISIBLE  =  '1'.
      ENDIF.
    ELSEIF ZTIMIMGTX-APPCIP IS INITIAL.
      IF ( SY-DYNNR     EQ '4101' OR SY-DYNNR     EQ '4501' )  AND
         ( SCREEN-NAME  EQ 'ITM2' OR SCREEN-NAME  EQ 'ITM4'    OR
           SCREEN-NAME  EQ 'ITM3' ).
        SCREEN-INVISIBLE  =  '1'.
      ENDIF.
    ENDIF.

    IF ZTIMIMG00-ZFINMT EQ '1' OR ZTIMIMG00-ZFINMT EQ '2'.
      IF ( SY-DYNNR EQ '0101' AND SCREEN-NAME EQ 'ITM8' ) OR
         ( SY-DYNNR EQ '0131' AND SCREEN-NAME EQ 'ITM3' ) OR
         ( SY-DYNNR EQ '0141' AND SCREEN-NAME EQ 'ITM5' ) OR
         ( SY-DYNNR EQ '1101' AND SCREEN-NAME EQ 'ITM6' ) .
        SCREEN-INVISIBLE = '1'.
      ENDIF.
    ENDIF.

    IF ZTIMIMG00-ZFINMT NE '1'.
      IF ( SY-DYNNR EQ '0102' AND SCREEN-NAME EQ 'ZTREQHD-ZFINSYN' ) OR
         ( SY-DYNNR EQ '0152' AND SCREEN-NAME EQ 'ZTREQHD-ZFINSYN' ) OR
         ( SY-DYNNR EQ '1102' AND SCREEN-NAME EQ 'ZTREQHD-ZFINSYN' ).
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
*&---------------------------------------------------------------------
*&      Form  P2000_ITEM_DEL_CHECK
*&---------------------------------------------------------------------
FORM P2000_ITEM_DEL_CHECK.

  LOOP AT IT_ZSREQIT   WHERE ZFMARK NE SPACE.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTREQST
                                   WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                                   AND   ZFAMDNO GT ZTREQST-ZFAMDNO.
    IF W_COUNT >= 1.
      MESSAGE E110 WITH IT_ZSREQIT-ZFITMNO W_COUNT.
    ENDIF.
* BL Document
    SELECT COUNT( * ) INTO W_COUNT FROM ZTBLIT
                      WHERE ZFREQNO = ZTREQHD-ZFREQNO
                      AND   ZFITMNO  = IT_ZSREQIT-ZFITMNO.
    IF W_COUNT > 0.
      MESSAGE E395 WITH IT_ZSREQIT-ZFITMNO W_COUNT.
    ENDIF.
* Customs Clearance Document
    SELECT COUNT( * ) INTO W_COUNT FROM ZTIVIT
                                   WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                                   AND   ZFITMNO EQ IT_ZSREQIT-ZFITMNO.
    IF W_COUNT > 0.
      MESSAGE E111 WITH IT_ZSREQIT-ZFITMNO W_COUNT.
    ENDIF.
* Commercial Invoice
    SELECT COUNT( * ) INTO W_COUNT FROM ZTCIVIT
                      WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                      AND   ZFITMNO EQ IT_ZSREQIT-ZFITMNO.
    IF W_COUNT > 0.
      MESSAGE E396 WITH IT_ZSREQIT-ZFITMNO W_COUNT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_ITEM_DEL_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_ZTREQORJ_UPDATE
*&---------------------------------------------------------------------
FORM P2000_ZTREQORJ_UPDATE.
* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  LOOP AT IT_ZTREQORJ WHERE ZFORIG EQ SPACE.
    DELETE IT_ZTREQORJ INDEX SY-TABIX.
  ENDLOOP.

* Origin Index Change
  REFRESH : IT_ZSMLCSG7O.
  LOOP AT IT_ZTREQORJ.
    IT_ZTREQORJ-ZFLSG7O  =   SY-TABIX * 10.
    MODIFY IT_ZTREQORJ.
    IF ZTREQHD-ZFREQTY EQ 'LC'.
      MOVE-CORRESPONDING   IT_ZTREQORJ   TO    IT_ZSMLCSG7O.
      APPEND IT_ZSMLCSG7O.
    ENDIF.
  ENDLOOP.

  IF ZTREQHD-ZFREQTY EQ 'LC' AND SY-SUBRC NE 0.
    MESSAGE W167 WITH 'Origin'.
  ENDIF.
ENDFORM.                    " P2000_ZTREQORJ_UPDATE
*&---------------------------------------------------------------------
*&      Form  P2000_IT_ZSMLCSG8E_UPDATE
*&---------------------------------------------------------------------
FORM P2000_IT_ZSMLCSG8E_UPDATE.
* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Space Delete
  LOOP AT IT_ZSMLCSG8E WHERE ZFOACD1 EQ SPACE.
    DELETE IT_ZSMLCSG8E INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSMLCSG8E .
    IT_ZSMLCSG8E-ZFLSG8E  =   SY-TABIX * 10.
    MODIFY IT_ZSMLCSG8E.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
    ZTMLCSG910-ZFOTDYN = 'X'.
  ELSE.
    ZTMLCSG910-ZFOTDYN = ''.
  ENDIF.

ENDFORM.                    " P2000_IT_ZSMLCSG8E_UPDATE
*&---------------------------------------------------------------------
*&      Form  P2000_IT_ZSMLCSG9O_UPDATE
*&---------------------------------------------------------------------
FORM P2000_IT_ZSMLCSG9O_UPDATE.
* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  LOOP AT IT_ZSMLCSG9O WHERE ZFODOC1 EQ SPACE.
    DELETE IT_ZSMLCSG9O INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSMLCSG9O.
    IT_ZSMLCSG9O-ZFLSG9O = SY-TABIX * 10.
    MODIFY IT_ZSMLCSG9O.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    ZTMLCHD-ZFADCD5 = ''.
  ELSE.
    ZTMLCHD-ZFADCD5 = 'X'.
  ENDIF.

ENDFORM.                    " P2000_IT_ZSMLCSG9O_UPDATE
*&---------------------------------------------------------------------
*&      Form  P2000_IT_ZSMLCSG7G_UPDATE
*&---------------------------------------------------------------------
FORM P2000_IT_ZSMLCSG7G_UPDATE.
* Display Mode -> Module Exit
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSMLCSG7G.
    IT_ZSMLCSG7G-ZFLSG7G = SY-TABIX * 10.
    MODIFY IT_ZSMLCSG7G INDEX SY-TABIX.
  ENDLOOP.

ENDFORM.                    " P2000_IT_ZSMLCSG7G_UPDATE
*&---------------------------------------------------------------------
*&      Form  P2000_PAY_YN_CHECK
*&---------------------------------------------------------------------
FORM P2000_PAY_YN_CHECK USING    PAY_YN_CODE.
* Display Mode -> Module Exit
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF PAY_YN_CODE IS INITIAL.
    PAY_YN_CODE = '32'.
    MESSAGE W083.   "ZTREQST-ZFEDICK = 'X'.
  ELSEIF NOT ( PAY_YN_CODE EQ '31' OR  PAY_YN_CODE EQ '32' ).
    MESSAGE E084.   "ZTREQST-ZFEDICK = 'X'.
  ENDIF.

ENDFORM.                    " P2000_PAY_YN_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_CIF_COB_CHECK
*&---------------------------------------------------------------------
FORM P2000_CIF_COB_CHECK.
* Display Mode -> Module Exit
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
ENDFORM.                    " P2000_CIF_COB_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_EDI_SEND_MESSAGE
*&---------------------------------------------------------------------
FORM P2000_EDI_SEND_MESSAGE.

  IF W_STATUS EQ C_REQ_D.
    PERFORM P2000_MESSAGE_BOX USING 'EDI Create Confirm'
                      'Being Document EDI Data Create.'
                      'Do you want Create'
                      'N'
                      '1'.

  ELSE.
    PERFORM P2000_MESSAGE_BOX USING 'EDI Create Confirm'
                      'Being ended without saving the changed item.'
                      'Do you want Create Original Data'
                      'Y'
                      '1'.
  ENDIF.

ENDFORM.                    " P2000_EDI_SEND_MESSAGE
*&---------------------------------------------------------------------
*&      Form  P1000_PRE_REQ_DOC_READ
*&---------------------------------------------------------------------
FORM P1000_PRE_REQ_DOC_READ.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      PERFORM   P1000_READ_MASTER_LC.
      CLEAR : ZSREQHD-ZFLLCTY.
      IF SY-TCODE(4) EQ 'ZIM1' AND W_STATUS NE C_REQ_C.
        PERFORM   P1000_READ_MASTER_LC_AMEND.
      ENDIF.
    WHEN 'LO'.
      PERFORM   P1000_READ_LOCAL_LC.
      IF SY-TCODE(4) EQ 'ZIM1'.
        IF W_STATUS EQ C_REQ_C.
          IF NOT ZSREQHD-ZFAMDNO IS INITIAL.
            W_ZFAMDNO = ZSREQHD-ZFAMDNO - 1.
            SELECT SINGLE * INTO ZTLLCAMHD_TMP FROM ZTLLCAMHD
                            WHERE ZFREQNO   EQ  ZTREQHD-ZFREQNO
                            AND   ZFAMDNO   EQ  W_ZFAMDNO.
          ENDIF.
        ELSE.
          PERFORM   P1000_READ_LOCAL_LC_AMEND USING ZSREQHD-ZFAMDNO.
        ENDIF.
      ENDIF.
      MOVE ZTLLCHD-ZFLLCTY   TO   ZSREQHD-ZFLLCTY.
      LOOP  AT  IT_ZSREQIT.
        IF SY-TABIX EQ 1.
          MOVE  IT_ZSREQIT-STAWN  TO  ZTLLCHD-ZFETC1.
          EXIT.
        ENDIF.
      ENDLOOP.

    WHEN 'PU'.
      PERFORM   P1000_READ_PURCH_DOC.
      MOVE ZTPUR-ZFLLCTY   TO   ZSREQHD-ZFLLCTY.

    WHEN 'TT'.
      PERFORM   P1000_READ_PAYORD_DOC.

    WHEN 'DA' OR 'DP' OR 'GS'.
      CLEAR : ZSREQHD-ZFLLCTY.
  ENDCASE.

  IF SY-TCODE(4) EQ 'ZIM1' AND W_STATUS NE C_REQ_C.
    SELECT SINGLE ZFOPNDT INTO W_ZFOPNDT FROM ZTREQST
                          WHERE ZFREQNO  EQ  ZTREQST-ZFREQNO
                          AND   ZFAMDNO  EQ '00000'.
  ENDIF.

ENDFORM.                    " P1000_PRE_REQ_DOC_READ
*&---------------------------------------------------------------------
*&      Form  P3000_EDI_SEND
*&---------------------------------------------------------------------
FORM P3000_EDI_SEND.

*----------------------------------------------------------------------
* Status Check
*----------------------------------------------------------------------
  IF ZTREQST-ZFEDICK EQ 'X'.
    MESSAGE E119 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
  ENDIF.
  IF ZTREQST-ZFEDIST NE 'N'.
    MESSAGE E105 WITH ZTREQST-ZFREQNO
                      ZTREQST-ZFAMDNO ZTREQST-ZFEDIST.
  ENDIF.
* STATUS CHECK
  PERFORM P2000_OPEN_CHECK       CHANGING W_ERR_CHK.
* Openning Bank NAME1 GET
  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFOPBN
                              CHANGING   W_OPEN_NM.
*----------------------------------------------------------------------
  IF W_LFA1-BAHNS IS INITIAL.
    MESSAGE E274 WITH ZTREQHD-ZFOPBN.
  ENDIF.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      PERFORM   P3000_MASTER_LC_FLAT_CREATE.
    WHEN 'LO'.
      PERFORM   P3000_LOCAL_LC_FLAT_CREATE.
    WHEN 'PU'.
      PERFORM   P3000_PURCH_DOC_FLAT_CREATE.
    WHEN 'TT'.
      PERFORM   P3000_TT_DOC_FLAT_CREATE.
    WHEN OTHERS.
      MESSAGE E997 WITH ZTREQHD-ZFREQTY.
  ENDCASE.

  ZTREQST-ZFDOCST = 'R'.
  ZTREQST-ZFDOCNO = W_ZFDHENO.
  ZTREQST-UDAT    = SY-DATUM.
  ZTREQST-UNAM    = SY-UNAME.

ENDFORM.                    " P3000_EDI_SEND
*&---------------------------------------------------------------------
*&      Form  P2000_MASTER_LC_CHECK
*&---------------------------------------------------------------------
FORM P2000_MASTER_LC_CHECK.

  ZTREQST-ZFEDICK = 'X'.
*----------------------------------------------------------------------
* Item Check
*----------------------------------------------------------------------
  LOOP AT IT_ZSREQIT.
    PERFORM   P2000_REQ_ITEM_CHECK   TABLES  IT_ZSREQIT
                                     USING   'E'.
  ENDLOOP.

*----------------------------------------------------------------------
* Open Amount Auto Create
*----------------------------------------------------------------------
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

  W_LINE = 0.
  DESCRIBE TABLE IT_ZTREQORJ LINES W_LINE.
  IF W_LINE LE 0.
    MESSAGE E167 WITH 'Origin'.   EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTREQHD-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    ZTREQST-ZFEDICK = 'O'.

    IF ZTMLCHD-ZFOPME IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I152.   EXIT.
    ENDIF.
* Bank Charge
    IF ZTMLCHD-ZFCHG  IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I167 WITH 'free charger'.   EXIT.
    ENDIF.
* Requested Open date
    IF ZTREQST-ZFAPPDT IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I153.   EXIT.
    ENDIF.
* Openning Bank Code
    IF ZTREQHD-ZFOPBN  IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE S154.
      EXIT.
    ENDIF.
* Openning Bank NAME1 GET
    PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFOPBN
                                CHANGING   W_OPEN_NM.

* Openning Bank Code
    IF ZTMLCHD-ZFOPBNCD IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I154.   EXIT.
    ENDIF.

* Openning Bank NAME
    IF ZTMLCHD-ZFOBNM  IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I155.   EXIT.
    ELSE.
      ASSIGN ZTMLCHD-ZFOBNM       TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCHD-ZFOBBR       TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDIF.

* Advising Bank Name
    IF ZTMLCHD-ZFABNM IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I843.   EXIT.
    ELSE.
    ENDIF.
* Man of respected
    IF ZTMLCSG2-ZFAPPNM IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I158.   EXIT.
    ELSE.
      ASSIGN ZTMLCSG2-ZFAPPNM     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFAPPAD1    TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFAPPAD2    TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFAPPAD3    TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFTELNO     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDIF.
* Beneficiary Code
    IF ZTMLCHD-ZFBENI  IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I159.   EXIT.
    ENDIF.
* Beneficiary Name
    IF ZTMLCSG2-ZFBENI1 IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I160.   EXIT.
    ELSE.
      ASSIGN ZTMLCSG2-ZFBENI1       TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFBENI2     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFBENI3     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFBENI4     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFBENIA     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDIF.
* Electronic Name
    IF ZTMLCSG2-ZFELENM IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I161.   EXIT.
    ENDIF.
    IF ZTMLCSG2-ZFELEID IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I162.   EXIT.
    ENDIF.
    ASSIGN ZTMLCSG2-ZFELENM     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFREPRE     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFELEID     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFELEAD1    TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFELEAD2    TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* Expiry Date
    IF ZTMLCHD-ZFEXDT  IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I163.   EXIT.
    ENDIF.
* Expiry Location
    IF ZTMLCHD-ZFEXPL  IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I164.   EXIT.
    ENDIF.
    ASSIGN ZTMLCHD-ZFEXPL       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* Additional Condition
    ASSIGN ZTMLCHD-ZFAAMT1        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCHD-ZFAAMT2        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCHD-ZFAAMT3        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCHD-ZFAAMT4        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* Loading Port
    IF ZTMLCHD-ZFSPRT  IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I165.   EXIT.
    ENDIF.
    ASSIGN ZTMLCHD-ZFSPRT       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* Arriving Port
    IF ZTMLCHD-ZFAPRT  IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I166.   EXIT.
    ENDIF.
    ASSIGN ZTMLCHD-ZFAPRT       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* Latest Shipping Date
    IF ZTMLCHD-ZFLTSD  IS INITIAL.
      IF ZTMLCHD-ZFSHPR1 IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'latest shipping date or period'.   EXIT.
      ENDIF.
    ENDIF.
    ASSIGN ZTMLCHD-ZFSHPR1      TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCHD-ZFSHPR2      TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCHD-ZFSHPR3      TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* Incoterms
    IF ZTMLCHD-INCO1   IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'incoterms(Terms of place)'.
      EXIT.
    ENDIF.
    ASSIGN ZTMLCHD-ZFINCP       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* equipped Document
    IF    ( ZTMLCSG910-ZFCOMYN   IS INITIAL ) AND
          ( ZTMLCSG910-ZFOCEYN   IS INITIAL ) AND
          ( ZTMLCSG910-ZFAIRYN   IS INITIAL ) AND
          ( ZTMLCSG910-ZFINYN    IS INITIAL ) AND
          ( ZTMLCSG910-ZFPACYN   IS INITIAL ) AND
          ( ZTMLCSG910-ZFCEOYN   IS INITIAL ) AND
          ( ZTMLCSG910-ZFOTDYN   IS INITIAL ).
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'equipped document'.   EXIT.
    ENDIF.
* Commercial Invoice
    IF ZTMLCSG910-ZFCOMYN IS INITIAL.
      IF NOT ( ZTMLCSG910-ZFNOCOM IS INITIAL ).
        MESSAGE I079.   ZTMLCSG910-ZFCOMYN = 'X'.
      ENDIF.
    ELSE.
      IF ZTMLCSG910-ZFNOCOM IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.   MESSAGE E080.   EXIT.
      ENDIF.
    ENDIF.
* Ocean Bill.
    IF ZTMLCSG910-ZFOCEYN IS INITIAL.
      IF NOT ( ZTMLCSG910-ZFOCEC1 IS INITIAL )
         AND NOT ( ZTMLCSG910-ZFOCEAC IS INITIAL )
         AND NOT ( ZTMLCSG910-ZFOCEAN IS INITIAL ).
        MESSAGE I081.    ZTMLCSG910-ZFOCEYN = 'X'.
      ENDIF.
    ELSE.
      IF ZTMLCSG910-ZFOCEC1 IS INITIAL.
        MESSAGE I082.  ZTREQST-ZFEDICK = 'X'.   EXIT.
      ENDIF.
      PERFORM P2000_PAY_YN_CHECK USING ZTMLCSG910-ZFOCEAC.
      CHECK : ZTREQST-ZFEDICK    NE 'X'.
      IF ZTMLCSG910-ZFOCEAN IS INITIAL.
        MESSAGE I085.  ZTREQST-ZFEDICK = 'X'.      EXIT.
      ENDIF.
    ENDIF.
    ASSIGN ZTMLCSG910-ZFOCEC1   TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG910-ZFOCEC2   TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG910-ZFOCEAN   TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* AIR BILL
    IF ZTMLCSG910-ZFAIRYN IS INITIAL.
      IF NOT ( ZTMLCSG910-ZFAIRC1 IS INITIAL )
         AND NOT ( ZTMLCSG910-ZFAIRAC IS INITIAL )
         AND NOT ( ZTMLCSG910-ZFAIRAN IS INITIAL ).
        MESSAGE I086.   ZTMLCSG910-ZFAIRYN = 'X'.
      ENDIF.
    ELSE.
      IF ZTMLCSG910-ZFAIRC1 IS INITIAL.
        MESSAGE I082.  ZTREQST-ZFEDICK = 'X'.   EXIT.
      ENDIF.
      PERFORM P2000_PAY_YN_CHECK USING ZTMLCSG910-ZFAIRAC.
      CHECK : ZTREQST-ZFEDICK NE 'X'.
      IF ZTMLCSG910-ZFAIRAN IS INITIAL.
        MESSAGE I085.  ZTREQST-ZFEDICK = 'X'.   EXIT.
      ENDIF.
    ENDIF.
    ASSIGN ZTMLCSG910-ZFAIRC1   TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG910-ZFAIRC2   TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG910-ZFAIRAN   TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* Insuarance Yes/No
    IF ZTMLCSG910-ZFINYN IS INITIAL.
      IF NOT ( ZTMLCSG910-ZFINCO1 IS INITIAL ).
        MESSAGE I087.  ZTMLCSG910-ZFINYN = 'X'.
      ENDIF.
    ELSE.
      IF ZTMLCSG910-ZFINCO1 IS INITIAL.
        MESSAGE I088.  ZTREQST-ZFEDICK = 'X'.  EXIT.
      ENDIF.
    ENDIF.
*----------------------------------------------------------------------
* Incoterms : 'CIF' or 'CIP' --> Insuarance Condition
*----------------------------------------------------------------------
    PERFORM  P2000_CIF_COB_CHECK.
    CHECK : ZTREQST-ZFEDICK NE 'X'.
*----------------------------------------------------------------------
    ASSIGN ZTMLCSG910-ZFINCO1   TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG910-ZFINCO2   TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* PACKING LIST
    IF ZTMLCSG910-ZFPACYN IS INITIAL.
      IF NOT ( ZTMLCSG910-ZFNOPAC IS INITIAL ).
        MESSAGE I095.   ZTMLCSG910-ZFPACYN = 'X'.
      ENDIF.
    ELSE.
      IF ZTMLCSG910-ZFNOPAC IS INITIAL.
        MESSAGE I096.
        ZTREQST-ZFEDICK = 'X'.  EXIT.
      ENDIF.
    ENDIF.
* Equipped Document
    DESCRIBE TABLE IT_ZSMLCSG8E LINES G_PARAM_LINE.
    IF ZTMLCSG910-ZFOTDYN EQ 'X'.
      IF G_PARAM_LINE EQ 0.
        MESSAGE I097.  ZTREQST-ZFEDICK = 'X'.  EXIT.
      ENDIF.
    ELSE.
      IF G_PARAM_LINE > 0.
        MESSAGE I098.     ZTMLCSG910-ZFOTDYN = 'X'.
      ENDIF.
    ENDIF.
    LOOP AT IT_ZSMLCSG8E.
      ASSIGN IT_ZSMLCSG8E-ZFOACD1 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDLOOP.
* SHIPMENT BY
    IF ZTMLCHD-ZFADCD1 IS INITIAL.
      IF NOT ( ZTMLCHD-ZFCARR IS INITIAL ).
        MESSAGE I120.   ZTMLCHD-ZFADCD1 = 'X'.   EXIT.
      ENDIF.
    ELSE.
      IF ZTMLCHD-ZFCARR IS INITIAL.
        MESSAGE I121.
        ZTREQST-ZFEDICK = 'X'.  EXIT.
      ENDIF.
    ENDIF.
    ASSIGN ZTMLCHD-ZFCARR       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

* equipped Document.
    DESCRIBE TABLE IT_ZSMLCSG9O LINES G_PARAM_LINE.
    IF ZTMLCHD-ZFADCD5 EQ 'X'.
      IF G_PARAM_LINE EQ 0.
        MESSAGE I113.  ZTREQST-ZFEDICK = 'X'.  EXIT.
      ENDIF.
    ELSE.
      IF G_PARAM_LINE > 0.
        MESSAGE I114.     ZTMLCHD-ZFADCD5 = 'X'.
      ENDIF.
    ENDIF.
    LOOP AT IT_ZSMLCSG9O.
      ASSIGN IT_ZSMLCSG9O-ZFODOC1 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDLOOP.

* L/C Goods Description
    LOOP AT IT_ZSMLCSG7G WHERE LOEKZ NE 'X'.
      ASSIGN IT_ZSMLCSG7G-ZFDSOG1 TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I150.   EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " P2000_MASTER_LC_CHECK

*&---------------------------------------------------------------------
*&      Form  P2000_LOCAL_LC_CHECK
*&---------------------------------------------------------------------
FORM P2000_LOCAL_LC_CHECK.
  ZTREQST-ZFEDICK = 'X'.
* L/C type
  PERFORM  P2000_LLCTY_CHECK   USING   ZSREQHD-ZFLLCTY.
*----------------------------------------------------------------------
* Item Check
*----------------------------------------------------------------------
  LOOP AT IT_ZSREQIT.
    PERFORM   P2000_REQ_ITEM_CHECK   TABLES  IT_ZSREQIT
                                     USING   'E'.
  ENDLOOP.

*----------------------------------------------------------------------
* Open Amount Auto Create
*----------------------------------------------------------------------
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTREQHD-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    IF ZTIMIMGTX-LOCAPP EQ 'X'.
      ZTREQST-ZFEDICK = 'O'.

      IF ZTREQST-ZFAPPDT IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I153.   EXIT.
      ENDIF.

      IF ZTREQHD-ZFOPBN  IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I154.   EXIT.
      ENDIF.

      PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFOPBN
                                  CHANGING   W_OPEN_NM.

      IF ZTLLCHD-ZFOBNM IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I155.   EXIT.
      ENDIF.
      IF ZTLLCHD-ZFOBBR IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I158 WITH 'open bank' 'branch name'.
        EXIT.
      ELSE.
        ASSIGN ZTLLCHD-ZFOBNM       TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTLLCHD-ZFOBBR       TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDIF.

      IF W_LFA1-BAHNS IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I157.   EXIT.
      ENDIF.

      IF W_LFA1-KRAUS IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I156.   EXIT.
      ENDIF.

      IF ZTLLCSG23-ZFAPPNM1 IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I158 WITH 'Applicant' 'Firm name'. EXIT.
      ELSEIF ZTLLCSG23-ZFAPPNM2 IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I158 WITH 'Applicant' 'Representative'. EXIT.
      ELSEIF ZTLLCSG23-ZFAPPNM3 IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I158 WITH 'Applicant' 'Address'. EXIT.
      ELSE.
        ASSIGN ZTLLCSG23-ZFAPPNM1 TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTLLCSG23-ZFAPPNM2 TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTLLCSG23-ZFAPPNM3 TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK. CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDIF.

      IF ZTLLCSG23-ZFBENI1  IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I158 WITH 'Receiver' 'Firm name'.   EXIT.
      ELSE.
        ASSIGN ZTLLCSG23-ZFBENI1      TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTLLCSG23-ZFBENI2      TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTLLCSG23-ZFBENI3      TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDIF.

      IF ZTLLCHD-ZFLLCTY    IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Local L/C type'.
        EXIT.
      ENDIF.

      IF ZTLLCHD-ZFUSG IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Use by opening basis'.
        EXIT.
      ENDIF.

      LOOP AT IT_ZSLLCOF.
        IF IT_ZSLLCOF-ZFOFFER IS INITIAL.
          ZTREQST-ZFEDICK = 'X'. MESSAGE I167 WITH 'Offer No'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Offer No'.
        EXIT.
      ENDIF.

      IF ZTLLCHD-ZFDPRP IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Time of presentation for shipping Doc'.
        EXIT.
      ENDIF.

      IF ZTLLCHD-ZFGDDT IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Goods delivery date'. EXIT.

      ENDIF.
      IF ZTLLCHD-ZFGDDT LT ZTREQST-ZFREQDT.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I977 WITH
             'Goods delivery date is before opening application date'.
        EXIT.
      ENDIF.

      IF ZTLLCHD-ZFEXDT IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Effective date'.
        EXIT.
      ENDIF.
      IF ZTLLCHD-ZFEXDT LT ZTLLCHD-ZFGDDT.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I977 WITH
                     'Effetive date is before goods delivery date'.
        EXIT.
      ENDIF.

      IF ZTLLCHD-ZFGDSC1     IS INITIAL.
       ZTREQST-ZFEDICK = 'X'.
       MESSAGE I167 WITH 'Rep supply goods name'. EXIT.
      ELSE.
        ASSIGN ZTLLCHD-ZFGDSC1        TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTLLCHD-ZFGDSC2        TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTLLCHD-ZFGDSC3        TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTLLCHD-ZFGDSC4        TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTLLCHD-ZFGDSC5        TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDIF.

      IF ZTLLCSG23-ZFNOBIL IS INITIAL AND ZTLLCSG23-ZFNOINV IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I131.  EXIT.
      ENDIF.

      ASSIGN ZTLLCHD-ZFETC1         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFETC2         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFETC3         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFETC4         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFETC5         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

      ASSIGN ZTLLCHD-ZFDCNO         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

      ASSIGN ZTLLCHD-ZFEXPR1        TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFEXPR2        TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFEXPR3        TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

      IF ZTLLCSG23-ZFELENM IS INITIAL AND
         ZTLLCSG23-ZFELEID IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Electronic signature'.  EXIT.
      ENDIF.
      ASSIGN ZTLLCSG23-ZFELENM      TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCSG23-ZFREPRE      TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCSG23-ZFELEID      TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

      ASSIGN ZTLLCHD-ZFISBN         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFISBNB        TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

      ASSIGN ZTLLCHD-ZFEXGNM1       TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFEXGNM2       TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFEXGNM3       TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFEXGNM4       TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTLLCHD-ZFEXGNM5       TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDIF.
  ENDIF.
ENDFORM.                    " P2000_LOCAL_LC_CHECK

*&---------------------------------------------------------------------
*&      Form  P2000_PURCH_DOC_CHECK
*&---------------------------------------------------------------------
FORM P2000_PURCH_DOC_CHECK.
  ZTREQST-ZFEDICK = 'X'.
  PERFORM  P2000_LLCTY_CHECK   USING   ZSREQHD-ZFLLCTY.
*-----------------------------------------------------------------------
* Item Check
*-----------------------------------------------------------------------
  LOOP AT IT_ZSREQIT.
    PERFORM   P2000_REQ_ITEM_CHECK   TABLES  IT_ZSREQIT
                                     USING   'E'.
  ENDLOOP.
*-----------------------------------------------------------------------
* Open Amount Auto Create
*-----------------------------------------------------------------------
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

  DESCRIBE TABLE IT_ZSPURSG1 LINES W_LINE.
  IF W_LINE LE 0.
    ZTREQST-ZFEDICK = 'X'.    MESSAGE I296.   EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTREQHD-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    ZTREQST-ZFEDICK = 'O'.

    IF ZTPUR-ZFACBN    IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Approval bank code'.   EXIT.
    ENDIF.

    PERFORM  P1000_GET_VENDOR   USING      ZTPUR-ZFACBN
                                CHANGING   W_OPEN_NM.

    IF ZTPUR-ZFACNM  IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I155.   EXIT.
    ELSE.
      ASSIGN ZTPUR-ZFACNM         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFACBR         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDIF.

    SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ ZTPUR-ZFACBN.

    IF ZTPUR-ZFAPPNM1     IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I158.   EXIT.
    ELSE.
      ASSIGN ZTPUR-ZFAPPNM1         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFAPPNM2         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFAPPNM3         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFAPPAD1         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFAPPAD2         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFAPPAD3         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDIF.

    IF ZTPUR-ZFVENNM1     IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. MESSAGE I158.   EXIT.
    ELSE.
      ASSIGN ZTPUR-ZFVENNM1         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFVENNM2         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFVENID          TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFVENAD1         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFVENAD2         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTPUR-ZFVENAD3         TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDIF.

    IF ZTPUR-ZFGDCD       IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Supply goods name type'.   EXIT.
    ENDIF.

    LOOP AT IT_ZSPURSG1.
      ASSIGN IT_ZSPURSG1-ZFHSDESC   TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDLOOP.

    LOOP AT IT_ZSPURSG4.
      IF IT_ZSPURSG4-ZFSDOC       IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Well-founded Doc code'.   EXIT
  .
      ENDIF.
      IF IT_ZSPURSG4-ZFSDNO       IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Well-founded Doc name'.   EXIT.
      ENDIF.
      IF SY-TABIX EQ 1.
        IF IT_ZSPURSG4-WAERS        IS INITIAL.
          ZTREQST-ZFEDICK = 'X'.
          MESSAGE I167 WITH 'Currency unit of well-founded Doc'.  EXIT.
        ENDIF.
        IF IT_ZSPURSG4-ZFGOAMT IS INITIAL.
          ZTREQST-ZFEDICK = 'X'.
          MESSAGE I167 WITH 'Payment amount of well-founded Doc'.  EXIT.
        ENDIF.
      ENDIF.
      ASSIGN IT_ZSPURSG4-ZFSDNO     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Well-founded Doc'.   EXIT.
    ENDIF.

    IF ZTPUR-ZFELEAD1    IS INITIAL AND
       ZTPUR-ZFELEID     IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Electronic signature'.  EXIT.
    ENDIF.
    ASSIGN ZTPUR-ZFELEAD1         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFELEAD2         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFELEID          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
ENDFORM.                    " P2000_PURCH_DOC_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_SPACE_CUT
*&---------------------------------------------------------------------
FORM P2000_SPACE_CUT USING   PARA_STRING.
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

*&---------------------------------------------------------------------
*&      Form  SPECIAL_CHAR_SEARCH
*&---------------------------------------------------------------------
FORM SPECIAL_CHAR_SEARCH USING     SEARCH_TEXT    MSG_ID.

  PERFORM    SPECIAL_SEARCH_SUB    USING     '~'   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '`'   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '_'   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '@'   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '#'   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '$'   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '|'   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '\'   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '{'   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '}'   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     '['   SEARCH_TEXT MSG_ID.
  PERFORM    SPECIAL_SEARCH_SUB    USING     ']'   SEARCH_TEXT MSG_ID.

ENDFORM.                    " SPECIAL_CHAR_SEARCH

*&---------------------------------------------------------------------
*&      Form  SPECIAL_SEARCH_SUB
*&---------------------------------------------------------------------
FORM SPECIAL_SEARCH_SUB USING    CHECK_CHAR
                                 SEARCH_TEXT    MSG_ID.

  SEARCH    SEARCH_TEXT    FOR   CHECK_CHAR.

  IF  SY-SUBRC       EQ       0.
    IF SY-TCODE(4) EQ 'ZIM0' OR SY-TCODE(4) EQ 'ZIM1'.
      ZTREQST-ZFEDICK = 'X'.
    ELSEIF SY-TCODE(4) EQ 'ZIM4'.
      ZTINS-ZFEDICK = 'X'.
    ELSEIF SY-TCODE(4) EQ 'ZIML'.
      ZTOFF-ZFEDICK = 'X'.
    ENDIF.

    MESSAGE ID 'ZIM' TYPE MSG_ID NUMBER '151' WITH CHECK_CHAR.
  ENDIF.

ENDFORM.                    " SPECIAL_SEARCH_SUB
*&---------------------------------------------------------------------
*&      Form  P2000_TEXT_FIELD_CHECK
*&---------------------------------------------------------------------
FORM P2000_TEXT_FIELD_CHECK.

  PERFORM P2000_SPACE_CUT USING <FS_F>.

ENDFORM.                    " P2000_TEXT_FIELD_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_SHOW_FLAT
*&---------------------------------------------------------------------
FORM P2000_SHOW_FLAT.
  CASE SY-TCODE.
    WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.                     " OFFER
      IF ZTOFF-ZFDOCNO    IS INITIAL.   MESSAGE E974.   ENDIF.
      W_ZFDOCNO = ZTOFF-ZFDOCNO.
    WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07' OR
         'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
      IF ZTREQST-ZFDOCNO    IS INITIAL.   MESSAGE E974.   ENDIF.
      W_ZFDOCNO = ZTREQST-ZFDOCNO.
    WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44' OR
         'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
      IF ZTINS-ZFDOCNO    IS INITIAL.   MESSAGE E974.   ENDIF.
      W_ZFDOCNO = ZTINS-ZFDOCNO.
  ENDCASE.
* SELECT
  SELECT SINGLE * FROM ZTDHF1 WHERE ZFDHENO EQ W_ZFDOCNO.
  IF SY-SUBRC NE 0.   MESSAGE E975 WITH W_ZFDOCNO.   ENDIF.

  SUBMIT  ZRIMFLAT_DISP  WITH P_DDENO  EQ W_ZFDOCNO
          AND RETURN.

ENDFORM.                    " P2000_SHOW_FLAT
*&---------------------------------------------------------------------
*&      Form  P2000_AMD_DOC_ITEM_SELECT
*&---------------------------------------------------------------------
FORM P2000_AMD_DOC_ITEM_SELECT.

  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE  ZFOPNNO   EQ ZSREQHD-ZFOPNNO
           AND    ZFDOCST EQ 'O'
           ORDER  BY ZFREQNO.

*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LCDISPLY'.
  CALL SCREEN 0014 STARTING AT  04 3
                   ENDING   AT  98 16.

ENDFORM.                    " P2000_AMD_DOC_ITEM_SELECT
*&---------------------------------------------------------------------
*&      Form  P2000_AMD_ITEM_SELECT
*&---------------------------------------------------------------------
FORM P2000_AMD_ITEM_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE  EBELN   EQ ZSREQHD-EBELN
           AND  ( ZFDOCST EQ 'O' )
           ORDER  BY ZFREQNO.

*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LCCHANGE'.
  CALL SCREEN 0014 STARTING AT  10 3
                   ENDING   AT  93 16.

ENDFORM.                    " P2000_AMD_ITEM_SELECT
*&---------------------------------------------------------------------
*&      Form  P3000_AMEND_MODIFY
*&---------------------------------------------------------------------
FORM P3000_AMEND_MODIFY.

  IF  W_OK_CODE NE 'DELE'.

    CLEAR : ZTIMIMGTX.
    SELECT SINGLE * FROM ZTIMIMGTX WHERE BUKRS EQ ZTREQHD-BUKRS.

* DATA MOVE
    PERFORM   P2000_LC_SAVE_DATA_MOVE.
    PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

*>> NCW ¼öÁ¤ - 2003.12.05 Amend ÀúÀå½Ã EDI ÇÊµå Ã¼Å© ¾ÈÇÔ.
    CASE  ZTREQHD-ZFREQTY.
      WHEN 'LC' OR 'GS'.           " IMPORT MASTER L/C AMEND
*        IF ZTIMIMGTX-ZFEDIYN EQ 'X' AND ZTIMIMGTX-APP707 EQ 'X'.
*          PERFORM   P2000_MASTER_LC_AMD_CHECK.
*        ENDIF.
      WHEN 'LO'.                   " LOCAL L/C
*        IF ZTIMIMGTX-ZFEDIYN EQ 'X' AND ZTIMIMGTX-LOCAMR EQ 'X'.
*          PERFORM   P2000_LOCAL_LC_AMD_CHECK.
*        ENDIF.
      WHEN 'PU'.                  " Purchase Approve
*        IF ZTIMIMGTX-ZFEDIYN EQ 'X' AND ZTIMIMGTX-APPPUR EQ 'X'.
*          PERFORM   P2000_PURCH_DOC_CHECK.
*        ENDIF.
      WHEN 'DA' OR 'DP' OR 'TT'.    " D/A or D/P or T/T
    ENDCASE.
  ENDIF.
*-----------------------------------------------------------------------

* Next Number Get
  IF W_STATUS EQ C_REQ_C.
    SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO FROM  ZTREQST
                          WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.
    ZTREQST-ZFAMDNO = W_ZFAMDNO + 1.

  ENDIF.

*>> Receipt Date Or Create on
  IF W_STATUS  EQ  C_REQ_C.
    SELECT  SINGLE * FROM ZTIMIMG00.
    IF ZTIMIMG00-ZFRELYN3 NE 'X' .
      MOVE  ZTREQST-ZFREQDT  TO  ZTREQST-ZFAPPDT.
      MOVE  SY-DATUM         TO  ZTREQST-ZFRVDT.
      MOVE  SY-UNAME         TO  ZTREQST-ZFOPNNM.
    ENDIF.
  ENDIF.

  CASE  ZTREQHD-ZFREQTY.
    WHEN 'LC'.                    " IMPORT MASTER L/C
      PERFORM   P3000_AMEND_LC_MODIFY.
    WHEN 'LO'.                    " LOCAL L/C
      PERFORM   P3000_AMEND_LOCAL_LC_MODIFY.
    WHEN 'PU'.                    " Purchase Approve
      PERFORM   P3000_PURCH_DOC_MODIFY.
    WHEN 'DA' OR 'DP' OR 'TT'.    " D/A or D/P or T/T
      PERFORM   P3000_IMPORT_DOC_MODIFY.
  ENDCASE.

  SET PARAMETER ID 'ZPREQNO' FIELD ZTREQHD-ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO' FIELD ZTREQST-ZFAMDNO.

  CASE W_OK_CODE.
    WHEN 'DELE'.
      PERFORM  P3000_FLAT_DATA_DELETE   USING  ZTREQST-ZFDOCNO.
      IF SY-LANGU EQ 'KO'.
        MESSAGE  S124  WITH  ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO 'delete'.
      ELSE.
        MESSAGE  S124  WITH  ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO
                                                          'Delete'.
      ENDIF.
    WHEN 'REVK'.
* FLAT DATA DELETE
      PERFORM  P3000_FLAT_DATA_DELETE   USING  ZTREQST-ZFDOCNO.
      IF SY-LANGU EQ 'KO'.
        MESSAGE  S124  WITH  ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO
                                          'FLAT DB Cancel'.
      ELSE.
        MESSAGE  S124  WITH  ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO
                                          'FLAT DB Cancel'.
      ENDIF.
    WHEN 'DELR'.
      IF SY-LANGU EQ 'KO'.
        MESSAGE  S124  WITH  ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO
                                             'Registration cancel'.
      ELSE.
        MESSAGE  S124  WITH  ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO
                                             'cancel receipt'.
      ENDIF.
    WHEN OTHERS.
      IF SY-LANGU EQ 'KO'.
        MESSAGE  S124  WITH  ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO 'Save'.
      ELSE.
        MESSAGE  S124  WITH  ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO 'save'.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P3000_AMEND_MODIFY

*&---------------------------------------------------------------------
*&      Form  P2000_ZSLLCOF_UPDATE
*&---------------------------------------------------------------------
FORM P2000_ZSLLCOF_UPDATE.

  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSLLCOF  WHERE ZFOFFER EQ SPACE.
    DELETE IT_ZSLLCOF  INDEX SY-TABIX.
  ENDLOOP.


  LOOP AT IT_ZSLLCOF.
    IT_ZSLLCOF-ZFLSGOF  =   SY-TABIX * 10.
    MODIFY IT_ZSLLCOF.
  ENDLOOP.

ENDFORM.                    " P2000_ZSLLCOF_UPDATE
*&---------------------------------------------------------------------
*&      Form  P2000_TAB_TO_SCR0123
*&---------------------------------------------------------------------
FORM P2000_TAB_TO_SCR0123.

  MOVE-CORRESPONDING    IT_ZSPURSG1    TO   ZTPURSG1.

ENDFORM.                    " P2000_TAB_TO_SCR0123
*&---------------------------------------------------------------------
*&      Form  P2000_SCR0123_TO_TAB
*&---------------------------------------------------------------------
FORM P2000_SCR0123_TO_TAB.

  READ TABLE IT_ZSPURSG1 WITH KEY ZFLSG1 = ZTPURSG1-ZFLSG1.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING   ZTPURSG1   TO IT_ZSPURSG1.
    MODIFY   IT_ZSPURSG1   INDEX    SY-TABIX.
  ENDIF.

ENDFORM.                    " P2000_SCR0123_TO_TAB
*&---------------------------------------------------------------------
*&      Form  SET_CURR_CONV_TO_EXTERNAL
*&---------------------------------------------------------------------
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
*&---------------------------------------------------------------------
*&      Form  P2000_PURSG1_APPEND
*&---------------------------------------------------------------------
FORM P2000_PURSG1_APPEND   USING   PA_MODE.

  REFRESH : IT_ZSPURSG1.
  LOOP AT IT_ZSREQIT WHERE ZFMARK EQ PA_MODE.
    IF IT_ZSREQIT-MENGE IS INITIAL.
      CONTINUE.
    ENDIF.

    PERFORM   P3000_PURSG1_APPEND_COM.

  ENDLOOP.
  IF SY-SUBRC NE 0.     MESSAGE I199.   EXIT.   ENDIF.

  SORT IT_ZSPURSG1 BY ZFLSG1.
  IF PA_MODE EQ 'X'.
    LOOP AT IT_ZSREQIT.
      CLEAR : IT_ZSREQIT-ZFMARK.   MODIFY IT_ZSREQIT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " P2000_PURSG1_APPEND
*&---------------------------------------------------------------------
*&      Form  P2000_PURSG4_APPEND
*&---------------------------------------------------------------------
FORM P2000_PURSG4_APPEND    USING  PA_MODE.

  LOOP AT IT_ZSREQIT WHERE ZFMARK EQ PA_MODE.
    W_TABIX = SY-TABIX.
    READ TABLE IT_ZSPURSG4 WITH KEY ZFLSG4 = IT_ZSREQIT-ZFITMNO
                           BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MESSAGE E132 WITH IT_ZSREQIT(5).
    ENDIF.
    CLEAR : IT_ZSPURSG4.
    MOVE : ZTREQST-MANDT         TO    IT_ZSPURSG4-MANDT,
           ZTREQST-ZFREQNO       TO    IT_ZSPURSG4-ZFREQNO,
           ZTREQST-ZFAMDNO       TO    IT_ZSPURSG4-ZFAMDNO,
           IT_ZSREQIT-ZFITMNO    TO    IT_ZSPURSG4-ZFLSG4,
           IT_ZSREQIT-STAWN      TO    IT_ZSPURSG4-STAWN,
           IT_ZSREQIT-TXZ01      TO    IT_ZSPURSG4-ZFGODS1.
    IT_ZSPURSG4-ZFEXDT  = ZTREQHD-ZFREQED.
    APPEND IT_ZSPURSG4.
  ENDLOOP.
  IF SY-SUBRC NE 0.     MESSAGE I199.   EXIT.   ENDIF.
  IF PA_MODE EQ 'X'.
    SORT IT_ZSPURSG1 BY ZFLSG1.
    LOOP AT IT_ZSREQIT.
      CLEAR : IT_ZSREQIT-ZFMARK.   MODIFY IT_ZSREQIT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " P2000_PURSG4_APPEND
*&---------------------------------------------------------------------
*&      Form  P2000_SCR0005_TO_TAB
*&---------------------------------------------------------------------
FORM P2000_SCR0005_TO_TAB.

  READ TABLE IT_ZSPURSG4 WITH KEY ZFLSG4 = ZTPURSG4-ZFLSG4.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING   ZTPURSG4   TO IT_ZSPURSG4.
    MODIFY   IT_ZSPURSG4   INDEX    SY-TABIX.
  ENDIF.

ENDFORM.                    " P2000_SCR0005_TO_TAB
*&---------------------------------------------------------------------
*&      Form  P3000_MASTER_LC_FLAT_CREATE
*&---------------------------------------------------------------------
FORM P3000_MASTER_LC_FLAT_CREATE.
  W_ZFCDDOC = 'APP700'.
  W_ZFDHSRO = W_LFA1-BAHNS.
  W_ZFDHREF = ZTREQHD-ZFREQNO.
  W_ZFDHENO = ZTREQST-ZFDOCNO.

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
            W_ZFCDDOC = W_ZFCDDOC
            W_ZFDHSRO = W_ZFDHSRO
            W_ZFDHREF = W_ZFDHREF
*            W_ZFDHDDB = W_ZFDHDDB
            W_BUKRS     = ZTREQHD-BUKRS
       CHANGING
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4
            NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
*>>>> HEI FLAT DATA?
*  CALL FUNCTION 'ZIM_APP700_EDI_SEND'
*       EXPORTING
*            W_ZFREQNO = ZTREQHD-ZFREQNO
*            W_ZFDHENO = W_ZFDHENO
*       EXCEPTIONS
*            DB_ERROR  = 4.


*>>>> READY KOREA LTD. INTERFACE(APP700)?
  CALL FUNCTION 'ZIM_IMPORT_EDI_APP700'
       EXPORTING
            W_ZFREQNO    = ZTREQHD-ZFREQNO
            W_ZFDHENO    = W_ZFDHENO
            W_BAHNS      = W_LFA1-BAHNS
       IMPORTING
            W_EDI_RECORD = W_EDI_RECORD
       EXCEPTIONS
            DB_ERROR     = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*>>> INTERNAL TABLE WRITE....
  REFRESH : IT_EDIFILE.
  IT_EDIFILE-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE.

*>>> READY KOREA LTD. SAM-FILE WRITE FUNCTION
  CALL FUNCTION 'ZIM_EDI_SAMFILE_WRITE'
       EXPORTING
            ZFCDDOC = W_ZFCDDOC
       TABLES
            EDIFILE = IT_EDIFILE.

ENDFORM.                    " P3000_MASTER_LC_FLAT_CREATE

*&---------------------------------------------------------------------
*&      Form  P3000_LOCAL_LC_FLAT_CREATE
*&---------------------------------------------------------------------
FORM P3000_LOCAL_LC_FLAT_CREATE.
  W_ZFCDDOC = 'LOCAPP'.
  W_ZFDHSRO = W_LFA1-BAHNS.
  W_ZFDHREF = ZTREQHD-ZFREQNO.
*  W_ZFDHDDB = ZTREQST-EKORG.
  W_ZFDHENO = ZTREQST-ZFDOCNO.

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
            W_ZFCDDOC = W_ZFCDDOC
            W_ZFDHSRO = W_ZFDHSRO
            W_ZFDHREF = W_ZFDHREF
*            W_ZFDHDDB = W_ZFDHDDB
            W_BUKRS     = ZTREQHD-BUKRS
       CHANGING
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4
            NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_LOCAPP_EDI_SEND'
       EXPORTING
            W_ZFREQNO = ZTREQHD-ZFREQNO
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.
*-----------------------------------------------------------------------
*>>>>> LOCAL OFFER SHEET CREATE
*--------------------------------------------------------------------
  SELECT SINGLE * FROM ZTOFF WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.
  CHECK : SY-SUBRC NE 0.

  DISP_MODE = 'N'.
  REFRESH : BDCDATA.
* Initial Screen CALL
  PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '2100'.
* Initial Screen FIELD
  PERFORM P2000_DYNPRO USING :
           ' ' 'ZSREQHD-EBELN'   '',
           ' ' 'ZSREQHD-ZFREQNO' ZTREQHD-ZFREQNO,
           ' ' 'BDC_OKCODE'      '/00'.
* Main Screen CALL
  PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '2101'.
* Save
  PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'SAVE'.
* Save Confirm Call
  PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '0001'.
* Save
  PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'YES'.
* CALL TRANSACTION
  PERFORM P2000_CALL_TRANSACTION  USING 'ZIML1'
                                  CHANGING  W_SUBRC.
  IF W_SUBRC NE 0.
    MESSAGE I292 WITH ZTREQHD-ZFREQNO.
  ELSE.
    MESSAGE I293 WITH ZTREQHD-ZFREQNO.
  ENDIF.

*-----------------------------------------------------------------------
* LOCAL OFFER SHEET FLAT DATA CREATE
*-----------------------------------------------------------------------
  CHECK W_SUBRC EQ 0.
  REFRESH : BDCDATA.
* Initial Screen CALL
  PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '2300'.
* Initial Screen FIELD
  PERFORM P2000_DYNPRO USING :
           ' ' 'ZSREQHD-EBELN'   '',                  " P/O No.
           ' ' 'ZSREQHD-ZFREQNO' ZTREQHD-ZFREQNO,     " Import No.
           ' ' 'BDC_OKCODE'      '/00'.               " ENTER
* Main Screen  CALL
  PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '2101'.
* Save
  PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'EDIS'.
* Save confirm CALL
  PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '0001'.
* Save
  PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'YES'.
* CALL TRANSACTION
  PERFORM P2000_CALL_TRANSACTION  USING 'ZIML3'
                                  CHANGING  W_SUBRC.
  IF W_SUBRC NE 0.
    MESSAGE I294 WITH ZTREQHD-ZFREQNO.
  ELSE.
    MESSAGE I295 WITH ZTREQHD-ZFREQNO.
  ENDIF.

ENDFORM.                    " P3000_LOCAL_LC_FLAT_CREATE
*&---------------------------------------------------------------------
*&      Form  P2000_GET_PUR_DOC_NO
*&---------------------------------------------------------------------
FORM P2000_GET_PUR_DOC_NO.
  SPOP-TITEL = 'Basis Document Code / Number Input Screen'.
  CANCEL_OPTION = 'Y'.
  OPTION = 1.
  TEXTLEN = 40.

  CALL SCREEN 0012 STARTING AT 28 6
                   ENDING   AT 80 10.

  IF ANTWORT EQ 'Y'.
    PERFORM   P2000_SCR0012_TO_TAB.
  ENDIF.

ENDFORM.                    " P2000_GET_PUR_DOC_NO
*&---------------------------------------------------------------------
*&      Form  P2000_SCR0012_TO_TAB
*&---------------------------------------------------------------------
FORM P2000_SCR0012_TO_TAB.

  LOOP AT IT_ZSPURSG4.
    MOVE : ZTPURSG4-ZFSDOC   TO     IT_ZSPURSG4-ZFSDOC,
           ZTPURSG4-ZFSDNO   TO     IT_ZSPURSG4-ZFSDNO.
    MODIFY   IT_ZSPURSG4    INDEX    SY-TABIX.
  ENDLOOP.

ENDFORM.                    " P2000_SCR0012_TO_TAB
*&---------------------------------------------------------------------
*&      Form  P3000_PURSG1_APPEND_COM
*&---------------------------------------------------------------------
FORM P3000_PURSG1_APPEND_COM.
  CLEAR : IT_ZSPURSG1.
  MOVE : ZTREQST-MANDT         TO    IT_ZSPURSG1-MANDT,
         ZTREQST-ZFREQNO       TO    IT_ZSPURSG1-ZFREQNO,
         ZTREQST-ZFAMDNO       TO    IT_ZSPURSG1-ZFAMDNO,
         IT_ZSREQIT-ZFITMNO    TO    IT_ZSPURSG1-ZFLSG1,
         IT_ZSREQIT-STAWN      TO    IT_ZSPURSG1-STAWN,
         IT_ZSREQIT-TXZ01      TO    IT_ZSPURSG1-ZFHSDESC,
         IT_ZSREQIT-MENGE      TO    IT_ZSPURSG1-MENGE,
         IT_ZSREQIT-MEINS      TO    IT_ZSPURSG1-MEINS,
         IT_ZSREQIT-NETPR      TO    IT_ZSPURSG1-NETPR,
         IT_ZSREQIT-PEINH      TO    IT_ZSPURSG1-PEINH,
         IT_ZSREQIT-BPRME      TO    IT_ZSPURSG1-BPRME,
         ZTREQHD-WAERS         TO    IT_ZSPURSG1-WAERS,
         ZTREQHD-WAERS         TO    IT_ZSPURSG1-ZFDAMC.


  IT_ZSPURSG1-ZFGOAMT = IT_ZSPURSG1-MENGE *
                     ( IT_ZSPURSG1-NETPR / IT_ZSPURSG1-PEINH ).
*-----------------------------------------------------------------------
*  Internal ==> External
*-----------------------------------------------------------------------
  PERFORM    SET_CURR_CONV_TO_EXTERNAL USING IT_ZSPURSG1-NETPR
                                            ZTREQHD-WAERS
                                            W_LOCAL_AMT.
  PERFORM   P2000_CONV_OTHER_CURR_TO_USD   USING  W_LOCAL_AMT
                                                  ZTREQHD-WAERS
                                         CHANGING W_LOCAL_AMT.



*-----------------------------------------------------------------------
*  External ==> Internal
*-----------------------------------------------------------------------
  PERFORM    SET_CURR_CONV_TO_INTERNAL USING W_LOCAL_AMT
                                             ZTREQHD-WAERS.
  IT_ZSPURSG1-ZFNETPRU = W_LOCAL_AMT.
  IT_ZSPURSG1-ZFUSD    = W_USD.

*-----------------------------------------------------------------------
* Internal ==> External
*-----------------------------------------------------------------------
  PERFORM    SET_CURR_CONV_TO_EXTERNAL USING IT_ZSPURSG1-ZFGOAMT
                                             ZTREQHD-WAERS
                                             W_LOCAL_AMT.
  PERFORM   P2000_CONV_OTHER_CURR_TO_USD   USING  W_LOCAL_AMT
                                                  ZTREQHD-WAERS
                                         CHANGING W_LOCAL_AMT.


*-----------------------------------------------------------------------
* External ==> Internal
*-----------------------------------------------------------------------
  PERFORM    SET_CURR_CONV_TO_INTERNAL USING W_LOCAL_AMT
                                             ZTREQHD-WAERS.
  IT_ZSPURSG1-ZFGOAMTU = W_LOCAL_AMT.

  APPEND IT_ZSPURSG1.

ENDFORM.                    " P3000_PURSG1_APPEND_COM
*&---------------------------------------------------------------------
*&      Form  P3000_PURSG1_MODIFY_COM
*&---------------------------------------------------------------------
FORM P3000_PURSG1_MODIFY_COM.
  READ TABLE IT_ZSPURSG1 WITH KEY ZFLSG1 = IT_ZSREQIT-ZFITMNO.
  W_TABIX = SY-TABIX.
  W_SY_SUBRC = SY-SUBRC.

  MOVE : IT_ZSREQIT-MENGE     TO    IT_ZSPURSG1-MENGE.

  IT_ZSPURSG1-ZFGOAMT = IT_ZSPURSG1-MENGE *
                      ( IT_ZSPURSG1-NETPR / IT_ZSPURSG1-PEINH ).

*-----------------------------------------------------------------------
* Internal ==> External
*-----------------------------------------------------------------------
  PERFORM    SET_CURR_CONV_TO_EXTERNAL USING IT_ZSPURSG1-ZFGOAMT
                                             ZTREQHD-WAERS
                                             W_LOCAL_AMT.
  PERFORM   P2000_CONV_OTHER_CURR_TO_USD   USING  W_LOCAL_AMT
                                                  ZTREQHD-WAERS
                                         CHANGING W_LOCAL_AMT.


*-----------------------------------------------------------------------
* External ==> Internal
*-----------------------------------------------------------------------
  PERFORM    SET_CURR_CONV_TO_INTERNAL USING W_LOCAL_AMT
                                              ZTREQHD-WAERS.
  IT_ZSPURSG1-ZFGOAMTU = W_LOCAL_AMT.
  MOVE : IT_ZSREQIT-STAWN     TO    IT_ZSPURSG1-STAWN.
  MOVE : IT_ZSREQIT-MENGE     TO    IT_ZSPURSG1-MENGE.
  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSPURSG1 INDEX W_TABIX.
  ENDIF.
ENDFORM.                    " P3000_PURSG1_MODIFY_COM
*&---------------------------------------------------------------------
*&      Form  P3000_PURCH_DOC_FLAT_CREATE
*&---------------------------------------------------------------------
FORM P3000_PURCH_DOC_FLAT_CREATE.

  W_ZFCDDOC = 'APPPUR'.
  W_ZFDHSRO = W_LFA1-BAHNS.
  IF ZTREQST-ZFAMDNO IS INITIAL.
    W_ZFDHREF = ZTREQST-ZFREQNO.
  ELSE.
    IF ZTREQST-ZFOPNNO IS INITIAL.
      W_ZFDHREF = ZTREQHD_TMP-ZFOPNNO.
    ELSE.
      W_ZFDHREF = ZTREQST-ZFOPNNO.
    ENDIF.
  ENDIF.
*  W_ZFDHDDB = ZTREQST-EKORG.
  W_ZFDHENO = ZTREQST-ZFDOCNO.

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
            W_ZFCDDOC = W_ZFCDDOC
            W_ZFDHSRO = W_ZFDHSRO
            W_ZFDHREF = W_ZFDHREF
*            W_ZFDHDDB = W_ZFDHDDB
            W_BUKRS     = ZTREQHD-BUKRS
       CHANGING
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4
            NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_APPPUR_EDI_SEND'
       EXPORTING
            W_ZFREQNO = ZTREQST-ZFREQNO
            W_ZFAMDNO = ZTREQST-ZFAMDNO
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

ENDFORM.                    " P3000_PURCH_DOC_FLAT_CREATE
*&---------------------------------------------------------------------
*&      Form  P2000_AMEND_DOC_STATUS_CHECK
*&---------------------------------------------------------------------
FORM P2000_AMEND_DOC_STATUS_CHECK.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE E963.
  ENDIF.

  CASE W_STATUS.
    WHEN C_REQ_C.
      PERFORM P2000_AMD_CREATE_CHECK.
    WHEN C_REQ_U.
      PERFORM P2000_AMD_CHAGE_CHECK.
    WHEN C_REQ_D.
      PERFORM P2000_AMD_DISPLAY_CHECK.
    WHEN C_ADD_U.
      PERFORM P2000_AMD_ADD_CHANGE_CHECK.
    WHEN C_OPEN_C.
      PERFORM P2000_AMD_OPEN_CHECK.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_AMEND_DOC_STATUS_CHECK

*&---------------------------------------------------------------------
*&      Form  P2000_AMD_CREATE_CHECK
*&---------------------------------------------------------------------
FORM P2000_AMD_CREATE_CHECK.


  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.
      IF SY-LANGU EQ '3'.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'open requested' 'Amend Create'.
      ELSE.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'open request' 'Amend Create'.
      ENDIF.
    WHEN 'C'.                                               " CANCEL
      IF SY-LANGU EQ '3'.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'canceled' 'Amend Create'.
      ELSE.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'cancel' 'Amend Create'.
      ENDIF.
    WHEN 'A'.                                               " AMEND
      IF SY-LANGU EQ '3'.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                    'Amend in progress' 'Amend Create'.
      ELSE.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                    'on amend' 'Amend Create'.
      ENDIF.
    WHEN 'N'.                                               " AMEND
      IF SY-LANGU EQ '3'.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                      'import requested' 'Amend Create'.
      ELSE.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                      'import request' 'Amend Create'.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_AMD_CREATE_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_SET_INIT_DATA_MOVE
*&---------------------------------------------------------------------
FORM P2000_SET_INIT_DATA_MOVE  USING WL_ERR_FLAG.
  CLEAR WL_ERR_FLAG.
  IF SY-TCODE EQ 'ZIM11'.
    PERFORM      P2000_STATUS_FIELD_MOVE.
    CASE ZTREQHD-ZFREQTY.
      WHEN 'LC'.
        MOVE : '9'               TO    ZTMLCAMHD-ZFEDFN,
               ZTMLCHD-ZFOPME    TO    ZTMLCAMHD-ZFOPME,
               ZTMLCHD-WAERS     TO    ZTMLCAMHD-WAERS,
               ZTMLCSG2-ZFBENI1  TO    ZTMLCAMHD-ZFBENI1,
               ZTMLCSG2-ZFBENI2  TO    ZTMLCAMHD-ZFBENI2,
               ZTMLCSG2-ZFBENI3  TO    ZTMLCAMHD-ZFBENI3,
               ZTMLCSG2-ZFBENI4  TO    ZTMLCAMHD-ZFBENI4,
               ZTMLCSG2-ZFBENIA  TO    ZTMLCAMHD-ZFBENIA,
               ZTMLCHD-ZFBENI    TO    ZTMLCAMHD-ZFBENI.
      WHEN 'LO'.              " LOCAL OFFER SHEET
        REFRESH : IT_ZSLLCAMSGOF.
        LOOP AT IT_ZSLLCOF.
          MOVE-CORRESPONDING IT_ZSLLCOF TO IT_ZSLLCAMSGOF.
          MOVE: IT_ZSLLCOF-ZFOFFER      TO IT_ZSLLCAMSGOF-ZFSGOF.
          APPEND IT_ZSLLCAMSGOF.
        ENDLOOP.

        MOVE : ZTLLCHD-ZFLLCTY   TO    ZTLLCAMHD-ZFNLLCTY,
               ZTLLCHD-ZFBENI    TO    ZTLLCAMHD-ZFBENI,
               ZTLLCHD-ZFEXDT    TO    ZTLLCAMHD-ZFNEXDT,
               ZTLLCSG23-ZFBENI1 TO    ZTLLCAMHD-ZFBENI1,
               ZTLLCSG23-ZFBENI2 TO    ZTLLCAMHD-ZFBENI2,
               ZTLLCHD-ZFOPKAM   TO    ZTLLCAMHD-ZFNOPKAM,
               ZTLLCHD-ZFKRW     TO    ZTLLCAMHD-ZFKRW.
* benificary NAME1 GET
        PERFORM  P1000_GET_VENDOR   USING      ZTLLCAMHD-ZFBENI
                                    CHANGING   W_ZFBENI_NM.
        IF W_LFA1-BAHNS IS INITIAL.
          MESSAGE W198 WITH ZTLLCHD-ZFBENI.
        ENDIF.
        CONCATENATE '/' W_LFA1-BAHNS INTO ZTLLCAMHD-ZFBENI3.
      WHEN 'PU'.
        MOVE : ZTREQST-ZFOPNNO   TO    ZTPUR-ZFAOPNNO,
               ''                TO    ZTREQST-ZFOPNNO.
        DESCRIBE TABLE IT_ZSPURSG4 LINES W_COUNT.
        W_COUNT = ( W_COUNT + 1 ) * 10.
        IT_ZSPURSG4-ZFLSG4 = W_COUNT.
        IT_ZSPURSG4-ZFSDOC = '2BM'.
        IT_ZSPURSG4-ZFSDNO = ZTPUR-ZFAOPNNO.
        IT_ZSPURSG4-ZFGODS1 = 'Purchase Approve Document'.
        APPEND IT_ZSPURSG4.
      WHEN OTHERS.
        WL_ERR_FLAG = 'Y'.
    ENDCASE.
  ENDIF.
* TEMP TABLE MOVE
*   PERFORM   P2000_TEMP_AREA_MOVE.

ENDFORM.                    " P2000_SET_INIT_DATA_MOVE
*&---------------------------------------------------------------------
*&      Form  P2000_STATUS_FIELD_MOVE
*&---------------------------------------------------------------------
FORM P2000_STATUS_FIELD_MOVE.

  MOVE : 'N'             TO ZTREQST-ZFDOCST,
         ''              TO ZTREQST-ZFRTNYN,
         'N'             TO ZTREQST-ZFEDIST,
         'X'             TO ZTREQST-ZFEDICK,
         'N'             TO ZTREQST-ZFRLST1,
         'N'             TO ZTREQST-ZFRLST2,
         SY-DATUM        TO ZTREQST-ZFREQDT,
         '00000000'      TO ZTREQST-ZFRLDT1,
         '00000000'      TO ZTREQST-ZFRLDT2,
         '00000000'      TO ZTREQST-ZFAPPDT,
         '00000000'      TO ZTREQST-ZFRVDT,
         '00000000'      TO ZTREQST-ZFOPNDT,
         ''              TO ZTREQST-ZFRLNM1,
         ''              TO ZTREQST-ZFRLNM2,
         ''              TO ZTREQST-ZFOPNNM,
         ''              TO ZTREQST-ZFDOCNO,
         ''              TO ZTREQST-ZFDOCNOR,
*        ''              TO ZTREQST-ZFOPNNO,
         SY-UNAME        TO ZTREQST-ERNAM,
         SY-DATUM        TO ZTREQST-CDAT,
         SY-UNAME        TO ZTREQST-UNAM,
         SY-DATUM        TO ZTREQST-UDAT.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFRELYN3   EQ 'X'.
    MOVE: 'N'             TO ZTREQST-ZFRLST1.
  ELSE.
    MOVE: 'N'             TO ZTREQST-ZFRLST1,
          SY-DATUM        TO ZTREQST-ZFAPPDT.
  ENDIF.

  IF ZTIMIMG00-ZFRELYN4   EQ 'X'.
    MOVE: 'N'             TO ZTREQST-ZFRLST2.
  ELSE.
    MOVE: 'N'             TO ZTREQST-ZFRLST2.
  ENDIF.

  SELECT SINGLE ZFOPNDT INTO W_ZFOPNDT FROM ZTREQST
                        WHERE ZFREQNO  EQ  ZTREQST-ZFREQNO
                        AND   ZFAMDNO  EQ '00000'.

ENDFORM.                    " P2000_STATUS_FIELD_MOVE
*&---------------------------------------------------------------------
*&      Form  P2000_IT_ZSMLCAMNARR_UPDATE
*&---------------------------------------------------------------------
FORM P2000_IT_ZSMLCAMNARR_UPDATE.

  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSMLCAMNARR WHERE ZFNARR EQ SPACE.
    DELETE IT_ZSMLCAMNARR INDEX SY-TABIX.
  ENDLOOP.


  LOOP AT IT_ZSMLCAMNARR.
    IT_ZSMLCAMNARR-ZFLNARR = SY-TABIX * 10.
    MODIFY IT_ZSMLCAMNARR.
  ENDLOOP.

ENDFORM.                    " P2000_IT_ZSMLCAMNARR_UPDATE
*&---------------------------------------------------------------------
*&      Form  P2000_SET_PF_STATUS
*&---------------------------------------------------------------------
FORM P2000_SET_PF_STATUS.
  CASE SY-TCODE.
    WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
      MOVE 'REQ' TO W_PFSTAT.
    WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
      MOVE 'REQA' TO W_PFSTAT.
    WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
      MOVE 'INS' TO W_PFSTAT.
    WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.   " Offer Sheet
      MOVE 'OFFS' TO W_PFSTAT.
    WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
      MOVE 'INSA' TO W_PFSTAT.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " P2000_SET_PF_STATUS
*&---------------------------------------------------------------------
*&      Form  P2000_SET_STATUS_TCODE_DISABLE
*&---------------------------------------------------------------------
FORM P2000_SET_STATUS_TCODE_DISABLE.

  CASE SY-TCODE.

    WHEN 'ZIM01' OR 'ZIM41' OR 'ZIM45' OR 'ZIML1' OR
         'ZIM11'.
      MOVE 'APP700' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'INF700' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'APP707' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'INF707' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'TTREQ'  TO IT_EXCL-FCODE.  APPEND IT_EXCL.

      MOVE 'CRDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DELE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.

      IF SY-TCODE EQ 'ZIML1'.
        MOVE 'CKEK' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      ENDIF.
      MOVE 'REVK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'DELR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'OPCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'FLAT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'HIST' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'HIIT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'EDI1' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'EDI2' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      MOVE 'APP700' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'INF700' TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'PUDOC'  TO IT_EXCL-FCODE.  APPEND IT_EXCL.
      MOVE 'OFCR'   TO IT_EXCL-FCODE.  APPEND IT_EXCL.
* Change t-code
    WHEN 'ZIM02' OR 'ZIM42' OR 'ZIM46' OR 'ZIML2' OR
         'ZIM12'.
      MOVE 'CHDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Change.
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " COPY
      IF SY-TCODE EQ 'ZIML2'.
        MOVE 'CKEK' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " EDI Check
      ENDIF.
      MOVE 'REVK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Cancel
      MOVE 'DELR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Approve Canc.
      MOVE 'OPCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Open Cancel
      MOVE 'EDI1' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'EDI2' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'OFCR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Offer Create.
* Display t-code
    WHEN 'ZIM03' OR 'ZIM43' OR 'ZIM47' OR 'ZIML3' OR
         'ZIM13'.
      MOVE 'DISP' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Display
      MOVE 'SAVE' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Save
      MOVE 'CKEK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " EDI Check.
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " COPY
      MOVE 'UCUR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Currency Chan
      MOVE 'OFCR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " OFFER Create
* Additional t-code
    WHEN 'ZIM05' OR 'ZIM15'.
      MOVE 'ADDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Addition
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " COPY
      MOVE 'OPCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Open Cancel.
      MOVE 'REVK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Cancel
      MOVE 'EDI1' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'EDI2' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'OFCR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " OFFER Create.

* OPEN t-code
    WHEN 'ZIM07' OR 'ZIM17' OR 'ZIM44' OR 'ZIM48'.
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " COPY
      MOVE 'OPDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " OPEN
      MOVE 'CKEK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " EDI Check
      MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " EDI Send
      MOVE 'DELR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Approve Canc.
      MOVE 'REVK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " Cancel
      MOVE 'FLAT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " FLAT DATA
      MOVE 'EDI1' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'EDI2' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'LGIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   "
      MOVE 'OFCR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " OFFER Create
    WHEN OTHERS.
  ENDCASE.
*>>> No Amend
  IF SY-TCODE(4) NE 'ZIM1'.
    MOVE 'PREV' TO IT_EXCL-FCODE.    APPEND IT_EXCL.        " PREVIOUS
    MOVE 'NEXT' TO IT_EXCL-FCODE.    APPEND IT_EXCL.        " NEXT DOC.
  ENDIF.
  IF SY-TCODE EQ 'ZIM01' OR SY-TCODE EQ 'ZIM02' OR
     SY-TCODE EQ 'ZIM11' OR SY-TCODE EQ 'ZIM12' OR
     SY-TCODE EQ 'ZIM41' OR SY-TCODE EQ 'ZIM45' OR
     SY-TCODE EQ 'ZIML1'.
    MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.   " EDI Send
  ENDIF.

ENDFORM.                    " P2000_SET_STATUS_TCODE_DISABLE
*&---------------------------------------------------------------------
*&      Form  P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------
FORM P2000_SET_STATUS_SCR_DISABLE.
  CASE SY-DYNNR.
    WHEN   '0100'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'IREQ' WITH W_CREATE.     W_STATUS = C_REQ_C.
    WHEN   '0151' OR '0101' OR '0121' OR '0131' OR '0141' OR '0111'.
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL.      " COPY
      IF ZTREQST-ZFDOCST NE 'N'.
        MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF ZTREQST-ZFDOCST NE 'R'.
        MOVE 'REVK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF ZTREQST-ZFDOCST NE 'O'.
        MOVE 'OPCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF ZTIMIMG00-ZFRELYN1 NE 'X'.
        MOVE 'ADDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'DELR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF NOT ( ZTREQST-ZFDOCST EQ 'O' OR ZTREQST-ZFDOCST EQ 'A' ).
        MOVE 'INF700' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'INF707' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF ZTREQHD-ZFREQTY EQ 'DA' OR
         ZTREQHD-ZFREQTY EQ 'DP' OR
         ZTREQHD-ZFREQTY EQ 'GS' OR
         ZTREQHD-ZFREQTY EQ 'PU'.
        MOVE 'APP700' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'APP707' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'INF700' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'INF707' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
*>> Telegraphic Transfer
      IF ZTREQHD-ZFREQTY NE 'TT'.
        MOVE 'TTREQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.

      IF ZTREQHD-ZFREQTY NE 'PU'.
        MOVE 'PUDOC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.

      IF ZTREQHD-ZFREQTY EQ 'TT'.
        MOVE 'APP700' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'APP707' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'INF700' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'INF707' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'TTREQ'  TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'CKEK'   TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
    WHEN   '0200'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'IREQ' WITH W_CHANGE.     W_STATUS = C_REQ_U.
    WHEN   '0300'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'IREQ' WITH W_DISPLAY.    W_STATUS = C_REQ_D.
    WHEN   '0500'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'IREQ' WITH W_ADD_CHG.    W_STATUS = C_ADD_U.
    WHEN   '0700'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'IREQ' WITH W_OPEN.       W_STATUS = C_OPEN_C.
    WHEN  '1100'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'IAMD' WITH W_CREATE.     W_STATUS = C_REQ_C.
    WHEN  '1200'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'IAMD' WITH W_CHANGE.     W_STATUS = C_REQ_U.
    WHEN  '1300'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'IAMD' WITH W_DISPLAY.    W_STATUS = C_REQ_D.
    WHEN  '1500'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'IAMD' WITH W_ADD_CHG.    W_STATUS = C_ADD_U.
    WHEN  '1700'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'IAMD' WITH W_OPEN.       W_STATUS = C_OPEN_C.
    WHEN  '1101' OR '1111'.
      MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL.      " COPY
      IF ZTREQST-ZFDOCST NE 'N'.
        MOVE 'EDIS' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF ZTREQST-ZFDOCST NE 'R'.
        MOVE 'REVK' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF ZTREQST-ZFDOCST NE 'O'.
        MOVE 'OPCL' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF ZTIMIMG00-ZFRELYN3 NE 'X'.
        MOVE 'ADDC' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'DELR' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF NOT ( ZTREQST-ZFDOCST EQ 'O' OR ZTREQST-ZFDOCST EQ 'A' ).
        MOVE 'INF700' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
        MOVE 'INF707' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.
      IF ZTREQHD-ZFREQTY NE 'TT'.
        MOVE 'TTREQ' TO IT_EXCL-FCODE.    APPEND IT_EXCL.
      ENDIF.

    WHEN  '4100' OR '4500'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      IF SY-DYNNR EQ '4100'.
        SET TITLEBAR  'ZINS' WITH W_CREATE.     W_STATUS = C_REQ_C.
      ELSE.
        SET TITLEBAR  'ZINA' WITH W_CREATE.     W_STATUS = C_REQ_C.
      ENDIF.
    WHEN  '4200' OR '4600'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      IF SY-DYNNR EQ '4200'.
        SET TITLEBAR  'ZINS' WITH W_CHANGE.     W_STATUS = C_REQ_U.
      ELSE.
        SET TITLEBAR  'ZINA' WITH W_CHANGE.     W_STATUS = C_REQ_U.
      ENDIF.
    WHEN  '4300' OR '4700'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      IF SY-DYNNR EQ '4300'.
        SET TITLEBAR  'ZINS' WITH W_DISPLAY.    W_STATUS = C_REQ_D.
      ELSE.
        SET TITLEBAR  'ZINA' WITH W_DISPLAY.    W_STATUS = C_REQ_D.
      ENDIF.
    WHEN  '4400' OR '4800'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      IF SY-DYNNR EQ '4400'.
        SET TITLEBAR  'ZINS' WITH W_OPEN.    W_STATUS = C_OPEN_C.
      ELSE.
        SET TITLEBAR  'ZINA' WITH W_OPEN.    W_STATUS = C_OPEN_C.
      ENDIF.
*-----------------------------------------------------------------------
* OFFER SHEET
*-----------------------------------------------------------------------
    WHEN  '2100'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'OFF1' WITH W_CREATE.     W_STATUS = C_REQ_C.
    WHEN  '2200'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'OFF1' WITH W_CHANGE.     W_STATUS = C_REQ_U.
    WHEN  '2300'.
      PERFORM   P2000_SET_REQ_INIT_SCR.
      SET TITLEBAR  'OFF1' WITH W_DISPLAY.    W_STATUS = C_REQ_D.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------
*&      Form  P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------
FORM P2000_SET_GUI_TEXT.

  CASE W_STATUS.
    WHEN C_REQ_C.   ASSIGN W_CREATE  TO <FS_F>.
    WHEN C_REQ_U.   ASSIGN W_CHANGE  TO <FS_F>.
    WHEN C_REQ_D.   ASSIGN W_DISPLAY TO <FS_F>.
    WHEN C_OPEN_C.  ASSIGN W_OPEN    TO <FS_F>.
    WHEN C_OPEN_U.  ASSIGN W_STAUTS  TO <FS_F>.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_GUI_TEXT
*&---------------------------------------------------------------------
*&      Form  P2000_SET_TRANSACTION
*&---------------------------------------------------------------------
FORM P2000_SET_TRANSACTION.

  CASE SY-TCODE.
    WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.                                        " CREATE
          LEAVE TO TRANSACTION 'ZIM01'.
        WHEN 'CHDC'.                                        " CHANGE
          LEAVE TO TRANSACTION 'ZIM02' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.                                        " DISPLAY
          LEAVE TO TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
        WHEN 'ADDC'.   " ADDITIONAL
          LEAVE TO TRANSACTION 'ZIM05' AND SKIP FIRST SCREEN.
        WHEN 'OPDC'.   " ADDITIONAL
          LEAVE TO TRANSACTION 'ZIM07' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.

    WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.                                        " CREATE
          LEAVE TO TRANSACTION 'ZIM11'.
        WHEN 'CHDC'.                                        " CHANGE
          LEAVE TO TRANSACTION 'ZIM12' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.                                        " DISPLAY
          LEAVE TO TRANSACTION 'ZIM13' AND SKIP FIRST SCREEN.
        WHEN 'ADDC'.   " ADDITIONAL
          LEAVE TO TRANSACTION 'ZIM15' AND SKIP FIRST SCREEN.
        WHEN 'OPDC'.   " ADDITIONAL
          LEAVE TO TRANSACTION 'ZIM17' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.

    WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.                                        " CREATE
          LEAVE TO TRANSACTION 'ZIM41'.
        WHEN 'CHDC'.                                        " CHANGE
          LEAVE TO TRANSACTION 'ZIM42' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.                                        " DISPLAY
          LEAVE TO TRANSACTION 'ZIM43' AND SKIP FIRST SCREEN.
        WHEN 'OPDC'.   " ADDITIONAL
          LEAVE TO TRANSACTION 'ZIM44' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.

    WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.                                        " CREATE
          LEAVE TO TRANSACTION 'ZIM45'.
        WHEN 'CHDC'.                                        " CHANGE
          LEAVE TO TRANSACTION 'ZIM46' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.                                        " DISPLAY
          LEAVE TO TRANSACTION 'ZIM47' AND SKIP FIRST SCREEN.
        WHEN 'OPDC'.   " ADDITIONAL
          LEAVE TO TRANSACTION 'ZIM48' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.

    WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
      CASE W_OK_CODE.
        WHEN 'OTDC'.   " OTHERS DOCUMENT
          LEAVE TO TRANSACTION SY-TCODE.
        WHEN 'CRDC'.                                        " CREATE
          LEAVE TO TRANSACTION 'ZIML1'.
        WHEN 'CHDC'.                                        " CHANGE
          LEAVE TO TRANSACTION 'ZIML2' AND SKIP FIRST SCREEN.
        WHEN 'DISP'.                                        " DISPLAY
          LEAVE TO TRANSACTION 'ZIML3' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SET_TRANSACTION
*&---------------------------------------------------------------------
*&      Form  P2000_LC_SAVE_DATA_MOVE
*&---------------------------------------------------------------------
FORM P2000_LC_SAVE_DATA_MOVE.

  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

*-----------------------------------------------------------------------
* Item Check
*-----------------------------------------------------------------------
  LOOP AT IT_ZSREQIT.
    PERFORM   P2000_REQ_ITEM_CHECK   TABLES  IT_ZSREQIT
                                     USING   'E'.
  ENDLOOP.

*-----------------------------------------------------------------------
* Approve Yes/No
*-----------------------------------------------------------------------
  IF W_STATUS EQ C_ADD_U.

    IF NOT ( ZTREQST-ZFRVDT > '00000000' )
       OR NOT ZTREQST-ZFRVDT IS INITIAL.
      IF  W_OK_CODE NE 'DELR'.
        MOVE : SY-DATUM TO ZTREQST-ZFRVDT.
      ENDIF.
    ENDIF.

    MOVE : SY-UNAME TO ZTREQST-ZFOPNNM.
  ENDIF.
*-----------------------------------------------------------------------
* Manual Open
*-----------------------------------------------------------------------
  IF W_STATUS EQ C_OPEN_C.
    IF W_NEW_DOCST NE 'N'.
      ZTREQST-ZFDOCST = 'O'.
      ZTREQHD-ZFLASTED = ZTREQHD-ZFREQED.
      ZTREQHD-ZFLASTSD = ZTREQHD-ZFREQSD.
      ZTREQHD-ZFOPNNO  = ZTREQST-ZFOPNNO.
    ENDIF.
    W_NEW_DOCST =  'O'.
  ENDIF.
  IF W_NEW_DOCST EQ 'N'.
    IF ZTREQST-ZFAMDNO EQ '00000'.
      CLEAR : ZTREQHD-ZFLASTED, ZTREQHD-ZFLASTSD, ZTREQHD-ZFOPNNO,
              ZTREQST-ZFOPNNO,  ZTREQST-ZFOPNDT.
    ELSE.
      MOVE : ZTREQHD_TMP-ZFLASTED  TO   ZTREQHD-ZFLASTED,
             ZTREQHD_TMP-ZFLASTSD  TO   ZTREQHD-ZFLASTSD,
             ZTREQHD_TMP-ZFOPNNO   TO   ZTREQHD-ZFOPNNO.
      CLEAR : ZTREQST-ZFOPNNO,  ZTREQST-ZFOPNDT.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_LC_SAVE_DATA_MOVE

*&---------------------------------------------------------------------
*&      Form  P3000_OFFER_SHEET_MODIFY
*&---------------------------------------------------------------------
FORM P3000_OFFER_SHEET_MODIFY.

  IF W_STATUS EQ C_ADD_U OR W_STATUS EQ C_OPEN_C OR W_STATUS EQ C_REQ_C
  OR W_STATUS EQ C_REQ_U OR W_STATUS EQ C_OPEN_U.
    PERFORM   P2000_OFFER_SHEET_CHECK.
  ENDIF.

  IF W_STATUS EQ C_REQ_C.
    PERFORM   P2000_GET_NUMBER_NEXT  USING  'OF'  ZTOFF-ZFOFFN0.
  ENDIF.

  CALL FUNCTION 'ZIM_OFFER_SHEET_MODIFY'
       EXPORTING
            W_OK_CODE      = W_OK_CODE
            ZFREQNO        = ZTOFF-ZFREQNO
            ZFSTATUS       = W_STATUS
            W_ZTOFF        = ZTOFF
            W_ZTOFF_OLD    = *ZTOFF
            W_ZTOFFFTX     = ZTOFFFTX
            W_ZTOFFFTX_OLD = *ZTOFFFTX
       TABLES
            IT_ZSOFFO      = IT_ZSOFFO
            IT_ZSOFFSDE    = IT_ZSOFFSDE
            IT_ZSOFFSG6    = IT_ZSOFFSG6
            IT_ZSOFFSG6G   = IT_ZSOFFSG6G
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E138.
  ELSE.
    CASE W_OK_CODE.
      WHEN 'DELE'.
        PERFORM  P3000_FLAT_DATA_DELETE   USING  ZTOFF-ZFDOCNO.
        MESSAGE  S137  WITH  ZTOFF-ZFOFFN0 'Delete'.
      WHEN 'REVK'.
* FLAT DATA DELETE
        PERFORM  P3000_FLAT_DATA_DELETE   USING  ZTOFF-ZFDOCNO.
        MESSAGE  S137  WITH  ZTOFF-ZFOFFN0 'FLAT DB cancel'.
      WHEN 'DELR'.
        MESSAGE  S137  WITH  ZTOFF-ZFOFFN0 'Registration cancel'.
      WHEN OTHERS.
        MESSAGE  S137  WITH  ZTOFF-ZFOFFN0 'Save'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P3000_OFFER_SHEET_MODIFY
*&---------------------------------------------------------------------
*
*&      Form  P1000_REQ_DOC_READ_OFFER
*&---------------------------------------------------------------------
*
FORM P1000_REQ_DOC_READ_OFFER.

  IF NOT ZSREQHD-ZFREQNO IS INITIAL.
    PERFORM   P1000_READ_REQ_DOC_OFFER.
  ENDIF.

ENDFORM.                    " P1000_REQ_DOC_READ_OFFER
*&---------------------------------------------------------------------
*
*&      Form  P1000_READ_OFFER_SHEET
*&---------------------------------------------------------------------
*
FORM P1000_READ_OFFER_SHEET.

  CALL FUNCTION 'ZIM_GET_OFFER_SHEET_DOC'
       EXPORTING
            ZFREQNO          = ZTOFF-ZFREQNO
       IMPORTING
            W_ZTOFF          = ZTOFF
            W_ZTOFFFTX       = ZTOFFFTX
       TABLES
            IT_ZSOFFO        = IT_ZSOFFO
            IT_ZSOFFO_ORG    = IT_ZSOFFO_ORG
            IT_ZSOFFSDE      = IT_ZSOFFSDE
            IT_ZSOFFSDE_ORG  = IT_ZSOFFSDE_ORG
            IT_ZSOFFSG6      = IT_ZSOFFSG6
            IT_ZSOFFSG6_ORG  = IT_ZSOFFSG6_ORG
            IT_ZSOFFSG6G     = IT_ZSOFFSG6G
            IT_ZSOFFSG6G_ORG = IT_ZSOFFSG6G_ORG
       EXCEPTIONS
            NOT_FOUND        = 4
            NOT_INPUT        = 8.

   *ZTOFF    =  ZTOFF.
   *ZTOFFFTX =  ZTOFFFTX.

  PERFORM   GET_DD07T_SELECT USING      'ZDTRME'  ZTOFF-ZFTRME
                             CHANGING   W_ZFTRME_TEXT.

  CASE SY-SUBRC.
    WHEN 0.
      IF SY-TCODE EQ 'ZIML1'.
        IF SY-CALLD NE 'X'.
          CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
               EXPORTING
                    ZFREQNO = ZTREQST-ZFREQNO
                    ZFAMDNO = ZTREQST-ZFAMDNO.
          MESSAGE E141 WITH ZTREQST-ZFREQNO.
        ENDIF.
      ENDIF.
    WHEN 4.
      IF SY-TCODE NE 'ZIML1'.
        MESSAGE E139 WITH ZTOFF-ZFREQNO.
      ENDIF.
    WHEN 8.
      MESSAGE E140.
  ENDCASE.

  CHECK : SY-TCODE NE 'ZIML1'.

  PERFORM   P2000_OFFER_STATUS_CHECK.

* LOCK OBJECT
  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D ).
    PERFORM P2000_SET_OFFER_DOC_LOCK USING    'L'.
  ENDIF.

ENDFORM.                    " P1000_READ_OFFER_SHEET
*&---------------------------------------------------------------------
*
*&      Form  P2000_ZSOFFO_UPDATE
*&---------------------------------------------------------------------
*
FORM P2000_ZSOFFO_UPDATE.

  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSOFFO WHERE ZFORIG EQ SPACE.
    DELETE IT_ZSOFFO INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSOFFO.
    IT_ZSOFFO-ZFLO  =   SY-TABIX * 10.
    MODIFY IT_ZSOFFO INDEX SY-TABIX.
  ENDLOOP.

ENDFORM.                    " P2000_ZSOFFO_UPDATE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT_OFFER_DATA_MOVE
*&---------------------------------------------------------------------
FORM P2000_INIT_OFFER_DATA_MOVE.

  CHECK : SY-TCODE EQ 'ZIML1'.
  CLEAR : ZTOFF, ZTOFFFTX.
  REFRESH : IT_ZSOFFO, IT_ZSOFFO_ORG, IT_ZSOFFSDE, IT_ZSOFFSDE_ORG,
            IT_ZSOFFSG6, IT_ZSOFFSG6G, IT_ZSOFFSG6_ORG,
            IT_ZSOFFSG6G_ORG.

  MOVE : 'N'                    TO ZTOFF-ZFDOCST,
         'N'                    TO ZTOFF-ZFEDIST,
         'X'                    TO ZTOFF-ZFEDICK,
         SY-UNAME               TO ZTOFF-ERNAM,
         SY-DATUM               TO ZTOFF-CDAT,
         SY-UNAME               TO ZTOFF-UNAM,
         SY-DATUM               TO ZTOFF-UDAT.

  SELECT SINGLE * FROM ZTIMIMG00.

  MOVE : ZTIMIMGTX-ZFAPPAD1L    TO ZTOFF-ZFVENAD1,
         ZTIMIMGTX-ZFAPPAD2L    TO ZTOFF-ZFVENAD2,
         ZTIMIMGTX-ZFAPPAD3L    TO ZTOFF-ZFVENAD3,
         ZTIMIMGTX-ZFELEID      TO ZTOFF-ZFVENID,
         ZTIMIMGTX-ZFELTXN      TO ZTOFF-ZFVETXN,
         ZTIMIMGTX-ZFELENML     TO ZTOFF-ZFVENNM,
         ZTIMIMGTX-ZFREPREL     TO ZTOFF-ZFVENRE.

  WRITE : ZTREQHD-ZFREQED        TO ZTOFFFTX-ZFED1.
  MOVE : ZTREQHD-ZFREQNO         TO ZTOFF-ZFREQNO,
         '31'                    TO ZTOFF-ZFTRME,
         'KR'                    TO ZTOFF-ZFSHCU,
         'KOREA'                 TO ZTOFF-ZFSHCUNM,
         'KR'                    TO ZTOFF-ZFARCU,
         'KOREA'                 TO ZTOFF-ZFARCUNM,
         'Manufactures inspection to be final'
                                 TO ZTOFFFTX-ZFISM1,
         ZTREQHD-ZFBENI          TO ZTOFF-ZFBENI,
         'By Irrevocable L/C to be opened in favor of us'
                                 TO ZTOFFFTX-ZFPAM1,
         'Export Standard Packing'
                                 TO ZTOFFFTX-ZFPKM1,
         SY-DATUM                TO ZTOFF-ZFISSDT,
         'WITHIN 30 DAYS AFTER RECEIPT OF L/C'
                                 TO ZTOFF-ZFGADT1.
  CALL FUNCTION 'ZIM_GET_PO_NUMBER_EXTERNAL'
       EXPORTING
             W_WERKS      =   ZTREQHD-ZFWERKS
*              W_ZZBUSTYPE  =   EKKO-ZZBUSTYPE
             W_EBELN      =   ZTREQHD-EBELN
       IMPORTING
             W_ADD_EBELN  =   ZTOFF-ZFISSNO.
  MOVE : ZTREQHD-ZFREQNO TO ZTOFF-ZFVENRN.
*  CONCATENATE ZTREQHD-EBELN '-' ZTREQHD-ZFREQNO INTO ZTOFF-ZFISSNO.
*-----------------------------------------------------------------------

* Benificiary Text Select
*  PERFORM   P1000_GET_LFA1_SELECT     USING    ZTOFF-ZFBENI
*                                      CHANGING W_LFA1.
  PERFORM   P1000_GET_VENDOR          USING    ZTOFF-ZFBENI
                                      CHANGING W_LFA1-NAME1.

  SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ ZTOFF-ZFBENI.
  IF LFA1-BAHNS IS INITIAL.
    MESSAGE W282.
  ENDIF.
  CLEAR : ZTOFF-ZFELNO.
  MOVE : W_LFA1-NAME1          TO ZTOFF-ZFELENM,
         LFA1-NAME1            TO  ZTLLCSG23-ZFBENI1,
         LFA1-J_1KFREPRE       TO ZTOFF-ZFREPRE,
         LFA1-BAHNS            TO ZTOFF-ZFELEID,
         W_LFA1-NAME2          TO ZTOFF-ZFAPPAD1,
         W_LFA1-NAME3          TO ZTOFF-ZFAPPAD2,
         ''                    TO ZTOFF-ZFAPPAD3.

*-----------------------------------------------------------------------
* Item
*-----------------------------------------------------------------------
  W_TOT_AMOUNT = 0.    W_MENGE      = 0.
  LOOP AT IT_ZSREQIT.
    CLEAR : IT_ZSOFFSG6.
    W_ZSREQIT = IT_ZSREQIT.
    MOVE : SY-TABIX            TO IT_ZSOFFSG6-ZFLSG6,
           IT_ZSREQIT-STAWN    TO IT_ZSOFFSG6-STAWN,
           IT_ZSREQIT-TXZ01    TO IT_ZSOFFSG6-ZFGD1,
           IT_ZSREQIT-MENGE    TO IT_ZSOFFSG6-MENGE,
           IT_ZSREQIT-MEINS    TO IT_ZSOFFSG6-MEINS,
           IT_ZSREQIT-NETPR    TO IT_ZSOFFSG6-NETPR,
           IT_ZSREQIT-PEINH    TO IT_ZSOFFSG6-PEINH,
           IT_ZSREQIT-BPRME    TO IT_ZSOFFSG6-BPRME,
           ZTREQHD-WAERS       TO IT_ZSOFFSG6-WAERS.
    IT_ZSOFFSG6-ZFGOAMT = IT_ZSOFFSG6-MENGE *
           ( IT_ZSOFFSG6-NETPR / IT_ZSOFFSG6-PEINH ).
    W_TOT_AMOUNT = W_TOT_AMOUNT + IT_ZSOFFSG6-ZFGOAMT.
    W_MENGE      = W_MENGE      + IT_ZSOFFSG6-MENGE.
    AT LAST.
      IT_ZSOFFSG6-ZFMENGES = W_MENGE.
      IT_ZSOFFSG6-ZFGOAMTS = W_TOT_AMOUNT.
      ZTOFF-ZFTOCN         = W_MENGE.
      ZTOFF-ZFTOCNM        = IT_ZSOFFSG6-MEINS.
      ZTOFF-ZFTOAM         = W_TOT_AMOUNT.
      ZTOFF-ZFTOAMC        = ZTREQHD-WAERS.
    ENDAT.
    APPEND IT_ZSOFFSG6.
  ENDLOOP.

*-----------------------------------------------------------------------
* Origin
*-----------------------------------------------------------------------
  LOOP AT IT_ZTREQORJ.
    MOVE : IT_ZTREQORJ-ZFLSG7O  TO   IT_ZSOFFO-ZFLO,
           IT_ZTREQORJ-ZFORIG   TO   IT_ZSOFFO-ZFORIG,
           IT_ZTREQORJ-ZFORNM   TO   IT_ZSOFFO-ZFORNM.
    APPEND IT_ZSOFFO.
  ENDLOOP.

  PERFORM   GET_DD07T_SELECT USING      'ZDTRME'  ZTOFF-ZFTRME
                             CHANGING   W_ZFTRME_TEXT.

ENDFORM.                    " P2000_INIT_OFFER_DATA_MOVE
*&---------------------------------------------------------------------
*&      Form  P2000_OFFER_STATUS_CHECK
*&---------------------------------------------------------------------
FORM P2000_OFFER_STATUS_CHECK.

  CASE W_STATUS.
    WHEN C_REQ_U.
      PERFORM  P2000_OFFER_CHANGE_CHECK.
    WHEN C_REQ_D.
      PERFORM  P2000_OFFER_DISPLAY_CHECK.
  ENDCASE.

ENDFORM.                    " P2000_OFFER_STATUS_CHECK

*&---------------------------------------------------------------------
*&      Form  P2000_OFFER_CHANGE_CHECK
*&---------------------------------------------------------------------
FORM P2000_OFFER_CHANGE_CHECK.

  CASE ZTOFF-ZFDOCST.
    WHEN 'R'.
      IF ZTOFF-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTOFF-ZFREQNO 'EDI open requested' 'Change'.
      ELSEIF ZTOFF-ZFEDIST = 'N'.
        MESSAGE E999 WITH ZTOFF-ZFREQNO 'EDI Created' 'Change'.
      ENDIF.
    WHEN 'O'.
      IF ZTOFF-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTOFF-ZFREQNO 'EDI Created''Change'.
      ELSE.
        MESSAGE E999 WITH ZTOFF-ZFREQNO 'Non-EDI Created' 'Change'.
      ENDIF.
    WHEN 'C'.                                               " CANCEL
      MESSAGE E999 WITH ZTOFF-ZFREQNO 'CanceledÈ' 'Change'.
    WHEN 'A'.                                               " AMEND
      MESSAGE E999 WITH ZTOFF-ZFREQNO 'Amended' 'Change'.
  ENDCASE.

ENDFORM.                    " P2000_OFFER_CHANGE_CHECK

*&---------------------------------------------------------------------
*
*&      Form  P2000_OFFER_DISPLAY_CHECK
*&---------------------------------------------------------------------
*
FORM P2000_OFFER_DISPLAY_CHECK.

  CASE ZTOFF-ZFDOCST.
    WHEN 'R'.
      IF ZTOFF-ZFEDIST = 'S'.
        MESSAGE I998 WITH ZTOFF-ZFREQNO 'EDI open requested'.
      ENDIF.
    WHEN 'O'.
      IF ZTOFF-ZFEDIST = 'R'.
        MESSAGE I998 WITH ZTOFF-ZFREQNO 'EDI opened'.
      ELSE.
        MESSAGE I998 WITH ZTOFF-ZFREQNO 'Non-EDI opened'.
      ENDIF.
    WHEN 'C'.
      MESSAGE I998 WITH ZTOFF-ZFREQNO 'Canceled'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_OFFER_DISPLAY_CHECK

*&---------------------------------------------------------------------
*
*&      Form  P2000_SET_OFFER_DOC_LOCK
*&---------------------------------------------------------------------
*
FORM P2000_SET_OFFER_DOC_LOCK USING    PA_MODE.

  IF SY-TCODE EQ 'ZIML1' AND PA_MODE EQ 'U'.
    IF SY-CALLD NE 'X'.
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
           EXPORTING
                ZFREQNO = ZSREQHD-ZFREQNO
                ZFAMDNO = ZTREQST-ZFAMDNO.
    ENDIF.
  ENDIF.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTOFF'
         EXPORTING
              ZFREQNO = ZTOFF-ZFREQNO
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Offer Sheet'
                            ZTOFF-ZFREQNO ''
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTOFF'
         EXPORTING
              ZFREQNO = ZTOFF-ZFREQNO.
  ENDIF.

ENDFORM.                    " P2000_SET_OFFER_DOC_LOCK
*&---------------------------------------------------------------------
*
*&      Form  P1000_READ_REQ_DOC_OFFER
*&---------------------------------------------------------------------
*
FORM P1000_READ_REQ_DOC_OFFER.
  SELECT MAX( ZFAMDNO ) INTO ZSREQHD-ZFAMDNO FROM ZTREQST
                        WHERE ZFREQNO  EQ    ZSREQHD-ZFREQNO.

  W_AMDNO   =    ZSREQHD-ZFAMDNO.

  CALL FUNCTION 'ZIM_GET_REQ_DOC_HEADER'
       EXPORTING
            ZFREQNO         = ZSREQHD-ZFREQNO
            ZFAMDNO         = W_AMDNO
       IMPORTING
            W_ZTREQHD       = ZTREQHD
            W_ZTREQST       = ZTREQST
       TABLES
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_ORG  = IT_ZSREQIL_ORG
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_ORG = IT_ZTREQORJ_ORG
       EXCEPTIONS
            NOT_FOUND       = 4
            NOT_INPUT       = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E018 WITH ZSREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.

  IF NOT ZTREQHD-ZFREQTY EQ 'LO'.
    MESSAGE E059 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
  ENDIF.

* item SELECT
  CALL FUNCTION 'ZIM_GET_REQ_DOC_ITEM'
      EXPORTING
            EBELN            =   ZTREQHD-EBELN
            KNUMV            =   EKKO-KNUMV
*            KPOSN            =   '000000'
            KSCHL            =   'ZM06'
            ZFREQNO          =   ZSREQHD-ZFREQNO
*               ZFAMDNO          =   ZTREQST-ZFAMDNO
      IMPORTING
            W_ITEM_CNT      =   W_ITEM_CNT
            W_TOT_AMOUNT    =   W_TOT_AMOUNT
      TABLES
            IT_ZSREQIT      =   IT_ZSREQIT
            IT_ZSREQIT_ORG  =   IT_ZSREQIT_ORG
      EXCEPTIONS
            NOT_FOUND      =   1
            NOT_INPUT      =   2
            NO_REFERENCE   =   3
            NO_AMOUNT      =   4.

  CASE SY-SUBRC.
    WHEN 1.    MESSAGE E026 WITH ZSREQHD-ZFREQNO.
    WHEN 2.    MESSAGE E019.
    WHEN 3.    MESSAGE E026 WITH ZSREQHD-ZFREQNO.
    WHEN 4.    MESSAGE E027 WITH ZSREQHD-ZFREQNO.
  ENDCASE.

* Lock Object Set
  IF SY-TCODE EQ 'ZIML1' OR SY-TCODE EQ 'ZIM41'.
    IF SY-CALLD NE 'X'.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
           EXPORTING
                ZFREQNO = ZSREQHD-ZFREQNO
                ZFAMDNO = W_AMDNO
           EXCEPTIONS
                OTHERS  = 1.
      IF SY-SUBRC <> 0.
        MESSAGE E510 WITH SY-MSGV1 'Import Document'
                             ZTREQHD-ZFREQNO W_AMDNO
                    RAISING DOCUMENT_LOCKED.
      ENDIF.
    ENDIF.
  ENDIF.

  W_ZFREQTY = ZTREQHD-ZFREQTY.
  W_COUNT = 1.

ENDFORM.                    " P1000_READ_REQ_DOC_OFFER
*&---------------------------------------------------------------------
*
*&      Form  P2000_OFF_SELECT
*&---------------------------------------------------------------------
*
FORM P2000_OFF_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
  SELECT R~ZFREQNO R~ZFAMDNO R~ZFREQTY R~EKORG R~EKGRP R~WAERS
         R~ZFOPAMT R~ZFREQDT R~ZFRVDT R~ZFDOCST R~ZFRLST1 R~ZFRLST2
         R~ZFRTNYN
     INTO (IT_ZSREQHD-ZFREQNO, IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFREQTY,
           IT_ZSREQHD-EKORG,   IT_ZSREQHD-EKGRP,   IT_ZSREQHD-WAERS,
           IT_ZSREQHD-ZFOPAMT, IT_ZSREQHD-ZFREQDT, IT_ZSREQHD-ZFRVDT,
           IT_ZSREQHD-ZFDOCST, IT_ZSREQHD-ZFRLST1, IT_ZSREQHD-ZFRLST2,
           IT_ZSREQHD-ZFRTNYN )
           FROM  ZVREQHD_ST AS R INNER JOIN ZTOFF AS O
                 ON    R~ZFREQNO  EQ  O~ZFREQNO
           WHERE R~EBELN   EQ ZSREQHD-EBELN
           AND   R~ZFAMDNO EQ '00000'.
    APPEND IT_ZSREQHD.
*            ORDER  BY R~ZFREQNO.
  ENDSELECT.

  DESCRIBE TABLE IT_ZSREQHD LINES W_LINE.
  IF W_LINE < 1.
    MESSAGE E064 WITH ZSREQHD-EBELN.
  ENDIF.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LCCHANGE'.
  CALL SCREEN 0014 STARTING AT  08 3
                   ENDING   AT  90 15.
* CALL SCREEN 0014 STARTING AT  X1 Y1
*                  ENDING   AT  X2 Y2.

ENDFORM.                    " P2000_OFF_SELECT
*&---------------------------------------------------------------------
*
*&      Form  P2000_LC_DOC_DISPLAY
*&---------------------------------------------------------------------
*
FORM P2000_LC_DOC_DISPLAY USING    P_ZFREQNO
                                   P_ZFOPNNO.

  IF P_ZFREQNO IS INITIAL AND  P_ZFOPNNO IS INITIAL.
    MESSAGE E063.
  ENDIF.

  SET PARAMETER ID 'BES' FIELD ''.
* EXPORT 'BES'  TO MEMORY ID 'BES'.
  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
* EXPORT 'ZPREQNO'  TO MEMORY ID 'ZPREQNO'.
  SET PARAMETER ID 'ZPOPNNO' FIELD P_ZFOPNNO.
* EXPORT 'ZPOPNNO'  TO MEMORY ID 'ZPOPNNO'.

  CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_LC_DOC_DISPLAY
*&---------------------------------------------------------------------
*
*&      Form  P2000_SET_INDICATE_OFFER
*&---------------------------------------------------------------------
*
FORM P2000_SET_INDICATE_OFFER.
  W_OK_CODE = OK-CODE.
* AUTHORITY CHECK
  PERFORM P2000_AUTHORITY_CHECK USING W_OK_CODE.
* Lock Object Set
  IF W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D.
    PERFORM P2000_SET_OFFER_DOC_LOCK USING    'L'.
  ENDIF.
* MESSAGE BOX
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.
      CASE W_OK_CODE.
        WHEN 'EDIS'.
          PERFORM P3000_OFFER_EDI_SEND.
        WHEN 'DELE'.
          IF ZTOFF-ZFDOCST NE 'N'.
            MESSAGE E312 WITH ZTOFF-ZFREQNO ZTOFF-ZFDOCST.
          ENDIF.
        WHEN 'REVK'.
          IF ZTOFF-ZFDOCST EQ 'O'.
            MESSAGE E105 WITH ZTOFF-ZFREQNO '' ZTOFF-ZFEDIST.
          ENDIF.
          ZTOFF-ZFDOCST = 'N'.   ZTOFF-ZFEDIST = 'N'.
        WHEN 'OPCL'.
          IF ZTOFF-ZFEDIST NE 'N'.
            MESSAGE E105 WITH ZTOFF-ZFREQNO '' ZTOFF-ZFEDIST.
          ENDIF.
          ZTOFF-ZFDOCST = 'N'.
          W_NEW_DOCST = 'N'.
      ENDCASE.

      PERFORM  P3000_DB_MODIFY_SCRCOM.
      CLEAR OK-CODE.
      PERFORM  P2000_SET_UNLOCK.                            " UNLOCK
      PERFORM  P2000_SET_SCREEN_SCRCOM.                     " INIT
      LEAVE SCREEN.
    WHEN 'N'.              " No...
  ENDCASE.

ENDFORM.                    " P2000_SET_INDICATE_OFFER

*&---------------------------------------------------------------------
*
*&      Form  P3000_OFFER_EDI_SEND
*&---------------------------------------------------------------------
FORM P3000_OFFER_EDI_SEND.

*-----------------------------------------------------------------------
* Status Check
*-----------------------------------------------------------------------
  IF ZTOFF-ZFEDICK EQ 'X'.
    MESSAGE E119 WITH ZTOFF-ZFREQNO ''.
  ENDIF.
  IF ZTOFF-ZFEDIST NE 'N'.
    MESSAGE E105 WITH ZTOFF-ZFREQNO ''  ZTOFF-ZFEDIST.
  ENDIF.
* STATUS CHECK
  PERFORM P2000_OFFER_OPEN_CHECK.

  PERFORM  P1000_GET_VENDOR   USING      ZTOFF-ZFBENI
                              CHANGING   W_OPEN_NM.
*-----------------------------------------------------------------------
  PERFORM   P3000_OFFER_FLAT_CREATE.

  ZTOFF-ZFDOCST = 'R'.
  ZTOFF-ZFDOCNO = W_ZFDHENO.
  ZTOFF-UDAT    = SY-DATUM.
  ZTOFF-UNAM    = SY-UNAME.

ENDFORM.                    " P3000_OFFER_EDI_SEND

*&---------------------------------------------------------------------
*&      Form  P2000_OFFER_OPEN_CHECK
*&---------------------------------------------------------------------
FORM P2000_OFFER_OPEN_CHECK.

  CASE ZTOFF-ZFDOCST.
    WHEN 'R'.
      IF ZTOFF-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTOFF-ZFREQNO  'EDI open requested'  'Open'.
      ELSE.
        MESSAGE E999 WITH ZTOFF-ZFREQNO  'EDI Data created'  'Open'.
      ENDIF.
    WHEN 'O'.
      IF ZTOFF-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTOFF-ZFREQNO 'EDI created' 'Open'.
      ELSE.
        MESSAGE I998 WITH ZTOFF-ZFREQNO 'Non-EDI created'.
      ENDIF.
      EXIT.
    WHEN 'C'.                                               " CANCEL
      MESSAGE E999 WITH ZTOFF-ZFREQNO 'Canceled'  'Open'.
    WHEN 'A'.                                               " AMEND
      MESSAGE E999 WITH ZTOFF-ZFREQNO 'Amended'  'Open'.
  ENDCASE.

ENDFORM.                    " P2000_OFFER_OPEN_CHECK

*&---------------------------------------------------------------------
*
*&      Form  P3000_OFFER_FLAT_CREATE
*&---------------------------------------------------------------------
*
FORM P3000_OFFER_FLAT_CREATE.
  W_ZFCDDOC = 'DOMOFR'.
  W_ZFDHSRO = W_LFA1-BAHNS.
  W_ZFDHREF = ZTOFF-ZFREQNO.
*  W_ZFDHDDB = ''.
  W_ZFDHENO = ZTOFF-ZFDOCNO.

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
            W_ZFCDDOC = W_ZFCDDOC
            W_ZFDHSRO = W_ZFDHSRO
            W_ZFDHREF = W_ZFDHREF
*            W_ZFDHDDB = W_ZFDHDDB
            W_BUKRS     = ZTREQHD-BUKRS
       CHANGING
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4
            NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_DOMOFR_EDI_SEND'
       EXPORTING
            W_ZFREQNO = ZTOFF-ZFREQNO
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

ENDFORM.                    " P3000_OFFER_FLAT_CREATE
*&---------------------------------------------------------------------
*&      Form  P2000_OFFER_SHEET_CHECK
*&---------------------------------------------------------------------
FORM P2000_OFFER_SHEET_CHECK.

  ZTOFF-ZFEDICK = 'O'.

  IF ZTOFF-ZFISSNO  IS INITIAL.
    ZTOFF-ZFEDICK = 'X'. MESSAGE I167 WITH 'Isuuing No'.   EXIT.
  ENDIF.

  IF ZTOFF-ZFISSDT  IS INITIAL.
    ZTOFF-ZFEDICK = 'X'. MESSAGE I167 WITH 'Issuing date'.   EXIT.
  ENDIF.

  IF ZTOFF-ZFVENRN  IS INITIAL.
    ZTOFF-ZFEDICK = 'X'. MESSAGE I167 WITH 'Reference No'.   EXIT.
  ENDIF.

  IF ZTOFF-ZFTRME   IS INITIAL.
    ZTOFF-ZFEDICK = 'X'. MESSAGE I167 WITH 'Transportation type'.
    EXIT.
  ENDIF.

  IF ZTOFF-ZFTOAM   IS INITIAL.
    ZTOFF-ZFEDICK = 'X'. MESSAGE I167 WITH 'Total amount'.     EXIT.
  ENDIF.

  IF ZTOFF-ZFELENM  IS INITIAL.
    ZTOFF-ZFEDICK = 'X'. MESSAGE I167 WITH 'Issuer firm nam'. EXIT.
  ENDIF.

  IF ZTOFF-ZFREPRE  IS INITIAL.
    ZTOFF-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Representative name(issuer)'.   EXIT.
  ENDIF.

  IF ZTOFF-ZFELEID  IS INITIAL.
    ZTOFF-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Electronic signature(issuer)'. EXIT.
  ENDIF.

  IF ZTOFF-ZFVENNM   IS INITIAL.
    ZTOFF-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Demander firm name'.   EXIT.
  ENDIF.

  IF ZTOFF-ZFVENRE   IS INITIAL.
    ZTOFF-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Representative name(demander)'.  EXIT.
  ENDIF.

  ASSIGN ZTOFF-ZFISSNO        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFVENRN        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFELENM        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFREPRE        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFELEID        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFELNO         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFAPPAD1       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFAPPAD2       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFAPPAD3       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFVENNM        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFVENRE        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFVENID        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFVETXN        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFVENAD1       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFVENAD2       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFVENAD3       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFSHCUNM       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFARCUNM       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.

  ASSIGN ZTOFFFTX-ZFPAM1      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFPAM2      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFPAM3      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFPAM4      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFPAM5      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFED1       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFED2       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFED3       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFED4       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFED5       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFISM1      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFISM2      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFISM3      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFISM4      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFISM5      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFPKM1      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFPKM2      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFPKM3      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFPKM4      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFFFTX-ZFPKM5      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFSHDT1        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFSHDT2        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFSHDT3        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFSHDT4        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFSHDT5        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFGADT1        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFGADT2        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFGADT3        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFGADT4        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.
  ASSIGN ZTOFF-ZFGADT5        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTOFF-ZFEDICK NE 'X'.

  LOOP AT IT_ZSOFFSG6.
    ASSIGN IT_ZSOFFSG6          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.
    IF ZTOFF-ZFEDICK EQ 'X'.   EXIT.   ENDIF.
  ENDLOOP.
  CHECK : ZTOFF-ZFEDICK NE 'X'.

  LOOP AT IT_ZSOFFO.
  ENDLOOP.

* IF W_LFA1-BAHNS IS INITIAL.
ENDFORM.                    " P2000_OFFER_SHEET_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_SCR_MODE_SET1
*&---------------------------------------------------------------------
FORM P2000_SCR_MODE_SET1.
  CASE W_STATUS.
    WHEN C_REQ_C OR C_REQ_U.
      IF SCREEN-GROUP1 = 'IO'.
        SCREEN-INPUT   = '1'.
      ELSE.
        SCREEN-INPUT   = '0'.
      ENDIF.
      IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
    WHEN  C_ADD_U.
      IF SCREEN-GROUP2 = 'IO'.
        SCREEN-INPUT   = '1'.
      ELSE.
        SCREEN-INPUT   = '0'.
      ENDIF.
      IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
    WHEN C_REQ_D OR C_ADD_D OR C_OPEN_D.
      IF SCREEN-GROUP1 = 'I'.
        SCREEN-INPUT   = '1'.
      ELSE.
        SCREEN-INPUT   = '0'.
      ENDIF.
    WHEN C_OPEN_C OR C_OPEN_U.
      IF SCREEN-GROUP3 = 'IO'.
        SCREEN-INPUT   = '1'.
      ELSE.
        SCREEN-INPUT   = '0'.
      ENDIF.
      IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
    WHEN C_INSU_I.
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
  IF SCREEN-NAME(10) EQ 'W_ROW_MARK'.
    SCREEN-INPUT   = '1'.
  ENDIF.

ENDFORM.                    " P2000_SCR_MODE_SET1
*&---------------------------------------------------------------------
*&      Form  P2000_MENGE_MODE_SET
*&---------------------------------------------------------------------
FORM P2000_MENGE_MODE_SET.
*-----------------------------------------------------------------------
  IF ZTREQHD-ZFLCKN  NE '8'.       ...
    PERFORM P2000_SCR_MODE_SET.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-NAME(10) EQ 'W_ROW_MARK'.
        SCREEN-INPUT   = '1'.
      ELSE.
        IF SCREEN-NAME EQ 'ZSREQIT-STAWN' OR
           SCREEN-NAME EQ 'ZSREQIT-MENGE' OR
           SCREEN-NAME EQ 'ZSREQIT-EBELN' OR
           SCREEN-NAME EQ 'ZSREQIT-EBELP' .
          PERFORM   P2000_SCR_MODE_SET1.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P2000_MENGE_MODE_SET
*&---------------------------------------------------------------------
*&      Form  P3000_AMEND_LC_MODIFY
*&---------------------------------------------------------------------
FORM P3000_AMEND_LC_MODIFY.

  CALL FUNCTION 'ZIM_MASTER_LC_AMEND_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFREQNO         = ZTREQHD-ZFREQNO
            ZFAMDNO         = ZTREQST-ZFAMDNO
            ZFSTATUS        = W_STATUS
            W_ZTREQHD       = ZTREQHD
            W_ZTREQHD_OLD   = *ZTREQHD
            W_ZTREQST       = ZTREQST
            W_ZTREQST_OLD   = *ZTREQST
            W_ZTMLCAMHD     = ZTMLCAMHD
            W_ZTMLCAMHD_OLD = *ZTMLCAMHD
       TABLES
            IT_ZSMLCAMNARR  = IT_ZSMLCAMNARR
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_OLD  = IT_ZSREQIL_OLD
            IT_ZSREQIT      = IT_ZSREQIT
            IT_ZSREQIT_OLD  = IT_ZSREQIT_OLD
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_OLD = IT_ZTREQORJ_OLD
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E017.
  ENDIF.

ENDFORM.                    " P3000_AMEND_LC_MODIFY
*&---------------------------------------------------------------------
*&      Form  P3000_GOOD_DESC_CREATE
*&---------------------------------------------------------------------
FORM P3000_GOOD_DESC_CREATE.
  DATA : L_TEXT(70).

  REFRESH : IT_ZSMLCSG7G.

  W_LINE = 0.
  W_ZFLSG7G = 0.
  TC_0103_1-TOP_LINE = 1.
  W_COUNTER = 0.
  W_CNT = 0.

  LOOP AT IT_ZSREQIT WHERE LOEKZ NE 'X'.
    ADD 1 TO W_CNT.
  ENDLOOP.

  LOOP AT IT_ZSREQIT WHERE ZFMARK NE SPACE
                     AND   LOEKZ  NE 'X'.
    W_COUNTER = W_COUNTER + 1.
  ENDLOOP.

  LOOP AT IT_ZSREQIT WHERE ZFMARK NE SPACE
                     AND   LOEKZ  NE 'X'.

    W_LINE = W_LINE + 1.
*    IF SY-TABIX EQ 1.
*H/S Code
    CONCATENATE 'H.S CODE : ' IT_ZSREQIT-STAWN
                INTO W_DESC_TEXT SEPARATED BY SPACE.
    PERFORM   P3000_ITEM_DESC_WRITE   USING      W_DESC_TEXT
                                      CHANGING   W_ZFLSG7G.

    CONCATENATE : 'DESCRIPTION : ' IT_ZSREQIT-TXZ01
              INTO W_DESC_TEXT SEPARATED BY SPACE.
    PERFORM   P3000_ITEM_DESC_WRITE   USING      W_DESC_TEXT
                                      CHANGING   W_ZFLSG7G.

    PERFORM  P1000_GET_PO_ITEM_TEXT  USING  'BEST'
                                            IT_ZSREQIT-EBELN
                                            IT_ZSREQIT-EBELP
                                     CHANGING   W_ZFLSG7G.

    CLEAR : W_DESC_TEXT.
    PERFORM   P3000_ITEM_DESC_WRITE   USING      W_DESC_TEXT
                                      CHANGING   W_ZFLSG7G.

    MOVE : 'QUANTITY : '  TO W_DESC_TEXT.
    WRITE IT_ZSREQIT-MENGE UNIT IT_ZSREQIT-MEINS  TO W_TEXT_17.
    CONDENSE W_TEXT_17.
    CONCATENATE  W_DESC_TEXT W_TEXT_17 IT_ZSREQIT-MEINS
                 INTO W_DESC_TEXT SEPARATED BY SPACE.
    PERFORM   P3000_ITEM_DESC_WRITE   USING      W_DESC_TEXT
                                      CHANGING   W_ZFLSG7G.

    WRITE IT_ZSREQIT-NETPR CURRENCY ZTREQHD-WAERS TO W_TEXT_18.
    CONDENSE W_TEXT_18.
    WRITE IT_ZSREQIT-PEINH                        TO W_TEXT_5.
    CONDENSE W_TEXT_5.
    CONCATENATE 'UNIT PRICE : ' ZTMLCHD-WAERS
                W_TEXT_18 '/' W_TEXT_5 IT_ZSREQIT-BPRME
                INTO W_DESC_TEXT
                SEPARATED BY SPACE.
    PERFORM   P3000_ITEM_DESC_WRITE   USING      W_DESC_TEXT
                                      CHANGING   W_ZFLSG7G.

    W_TOT_AMOUNT =
               ( IT_ZSREQIT-MENGE *
               ( IT_ZSREQIT-BPUMZ / IT_ZSREQIT-BPUMN )
             * ( IT_ZSREQIT-NETPR / IT_ZSREQIT-PEINH ) ).
    WRITE W_TOT_AMOUNT     CURRENCY ZTMLCHD-WAERS TO W_TEXT1_18.
    CONDENSE W_TEXT1_18.
    CONCATENATE 'AMOUNT : ' ZTMLCHD-WAERS W_TEXT1_18 INTO W_DESC_TEXT
                SEPARATED BY SPACE.
    PERFORM   P3000_ITEM_DESC_WRITE   USING      W_DESC_TEXT
                                      CHANGING   W_ZFLSG7G.

    WRITE IT_ZSREQIT-ZFEEIND TO W_DDATE.
    CONCATENATE 'Delivery Date : ' W_DDATE
                INTO W_DESC_TEXT SEPARATED BY SPACE.
    PERFORM   P3000_ITEM_DESC_WRITE   USING      W_DESC_TEXT
                                       CHANGING   W_ZFLSG7G.

  ENDLOOP.

ENDFORM.                    " P3000_GOOD_DESC_CREATE
*&---------------------------------------------------------------------
*&      Form  P2000_IL_DATA_MOVE
*&---------------------------------------------------------------------
FORM P2000_IL_DATA_MOVE.

  REFRESH : IT_ZSREQIL.
  IF ZTREQHD-ZFRECYN EQ 'X'.
    LOOP AT  IT_ZSREQIT WHERE MENGE  > 0.
      MOVE-CORRESPONDING IT_ZSREQIT  TO IT_ZSREQIL.
      MOVE IT_ZSREQIT-ZFITMNO   TO IT_ZSREQIL-ZFILSEQ.
      APPEND IT_ZSREQIL.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P2000_IL_DATA_MOVE
*&---------------------------------------------------------------------
*&      Form  P2000_INS_DOC_ITEM_SELECT
*&---------------------------------------------------------------------
FORM P2000_INS_DOC_ITEM_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO
           AND   ZFRLST1 IN R_ZFRLST1
*           AND   ZFRLST2 IN R_ZFRLST2
*           AND   ZFDOCST EQ 'O'
*           AND   ZFREQTY NE 'PU'
*           AND   ZFREQTY NE 'LO'
           AND   ZFRTNYN EQ SPACE
           AND   ZFCLOSE EQ SPACE
           AND   ZFINSYN NE SPACE
           ORDER  BY ZFREQNO.

*-----------------------------------------------------------------------
* TEXT SET
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'INCREAT1'.
  CALL SCREEN 0014 STARTING AT  12 3
                   ENDING   AT  77 15.
* CALL SCREEN 0014 STARTING AT  X1 Y1
*                  ENDING   AT  X2 Y2.

ENDFORM.                    " P2000_INS_DOC_ITEM_SELECT
*&---------------------------------------------------------------------
*&      Form  P2000_DATA_LISTING
*&---------------------------------------------------------------------
FORM P2000_DATA_LISTING.

  LOOP AT IT_ZSREQHD.
    W_MOD = SY-TABIX MOD 2.
    CASE INCLUDE.
      WHEN 'INCREATE'.
        PERFORM   P2000_INS_CREATE_LIST.
      WHEN 'INCREAT1'.
        PERFORM   P2000_INS_CREATE_LIST_1.
      WHEN 'INDISPLY'.
        PERFORM   P2000_INS_CHANGE_LIST.
      WHEN 'INDISPL1'.
        PERFORM   P2000_INS_CHANGE_LIST1.
      WHEN 'INDISPL2'.
        PERFORM   P2000_INS_CHANGE_LIST2.
      WHEN 'INDISPL3'.
        PERFORM   P2000_INS_CHANGE_LIST3.
      WHEN 'LCCHANGE'.
        PERFORM   P2000_LC_CHANGE_LIST.
      WHEN 'LCDISPLY'.
        PERFORM   P2000_LC_DISPLAY_LIST.
      WHEN 'LCDISPL1'.
        PERFORM   P2000_LC_DISPLAY_LIST_1.
    ENDCASE.
    HIDE:IT_ZSREQHD.
  ENDLOOP.
  CLEAR : IT_ZSREQHD.

ENDFORM.                    " P2000_DATA_LISTING
*&---------------------------------------------------------------------
*&      Form  P2000_INS_ITEM_SELECT
*&---------------------------------------------------------------------
FORM P2000_INS_ITEM_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
           FROM   ZVREQHD_ST
           WHERE  EBELN   EQ ZSREQHD-EBELN
*            AND   ZFDOCST EQ 'O'
           AND   ZFRLST1 IN R_ZFRLST1
*           AND   ZFRLST2 IN R_ZFRLST2
*           AND   ZFREQTY NE 'PU'
*           AND   ZFREQTY NE 'LO'
           AND   ZFRTNYN EQ SPACE
           AND   ZFCLOSE EQ SPACE
           AND   ZFINSYN NE SPACE
           ORDER  BY ZFREQNO.

*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
*  PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'INCREATE'.
  CALL SCREEN 0014 STARTING  AT 10 3
                   ENDING    AT 90 15.

ENDFORM.                    " P2000_INS_ITEM_SELECT
*&---------------------------------------------------------------------
*&      Form  P2000_GET_POSITION
*&---------------------------------------------------------------------
FORM P2000_GET_POSITION.
  X1 = SY-CUCOL.
  Y1 = SY-CUROW.
  IF X1 >= SY-SCOLS.
    X1 = 1.
  ENDIF.
  IF Y1 >= SY-SROWS.
    Y1 = 1.
  ENDIF.
  X2 = X1 + 64.
  IF TFILL > 15.
    Y2 = Y1 + 17.
  ELSE.
    Y2 = Y1 + TFILL + 2.
  ENDIF.

ENDFORM.                    " P2000_GET_POSITION
*&---------------------------------------------------------------------
*&      Form  P2000_INS_CREATE_LIST
*&---------------------------------------------------------------------
FORM P2000_INS_CREATE_LIST.

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

ENDFORM.                    " P2000_INS_CREATE_LIST
*&---------------------------------------------------------------------
*&      Form  P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------
FORM P2000_SEARCH_FIELD_MOVE.

  ZSREQHD-ZFREQNO = W_ZFREQNO.
  ZSREQHD-ZFAMDNO = W_ZFAMDNO.
  ZSREQHD-ZFOPNNO = W_ZFOPNNO.
  ZSREQHD-EBELN   = W_EBELN.
  ZSREQHD-ZFINSEQ = W_ZFINSEQ.

ENDFORM.                    " P2000_SEARCH_FIELD_MOVE
*&---------------------------------------------------------------------
*&      Form  P2000_INS_CREATE_LIST_1
*&---------------------------------------------------------------------
FORM P2000_INS_CREATE_LIST_1.

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

ENDFORM.                    " P2000_INS_CREATE_LIST_1
*&---------------------------------------------------------------------
*&      Form  P2000_LC_CHANGE_LIST
*&---------------------------------------------------------------------
FORM P2000_LC_CHANGE_LIST.

  WRITE : / IT_ZSREQHD-ZFREQNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-ZFAMDNO NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EKORG   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EKGRP   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
*          IT_ZSREQHD-ZFLASTAM CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          IT_ZSREQHD-ZFOPAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFREQDT NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRVDT  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFDOCST NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRLST1 NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRLST2 NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRTNYN NO-GAP.

ENDFORM.                    " P2000_LC_CHANGE_LIST
*&---------------------------------------------------------------------
*&      Form  P2000_LC_DISPLAY_LIST
*&---------------------------------------------------------------------
FORM P2000_LC_DISPLAY_LIST.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : / IT_ZSREQHD-EBELN   NO-GAP,    SY-VLINE NO-GAP,
            IT_ZSREQHD-ZFREQNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-ZFAMDNO NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
*         IT_ZSREQHD-LIFNR   NO-GAP,    SY-VLINE NO-GAP,
*         IT_ZSREQHD-NAME1   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EKORG   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EKGRP   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
*          IT_ZSREQHD-ZFLASTAM CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          IT_ZSREQHD-ZFOPAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFREQDT NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRVDT  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFDOCST NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRLST1 NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRLST2 NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRTNYN NO-GAP.

ENDFORM.                    " P2000_LC_DISPLAY_LIST
*&---------------------------------------------------------------------
*&      Form  P2000_INSURANCE_CHECK
*&---------------------------------------------------------------------
FORM P2000_INSURANCE_CHECK.

  ZTINS-ZFEDICK = 'O'.

  IF ZTINS-ZFEDFU IS INITIAL.
    CASE SY-TCODE.
      WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
        ZTINS-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Electronic Doc function'. EXIT.
    ENDCASE.
  ENDIF.

  IF ZTINS-ZFIVAMT IS INITIAL.
    IF ZTINS-ZFEDFU NE '2'.
      ZTINS-ZFEDICK = 'X'. MESSAGE I221.   EXIT.
    ENDIF.
  ENDIF.

  IF ZTINS-ZFTRANS IS INITIAL. " OR ZTINS-ZFTRANS EQ 'B'.
    ZTINS-ZFEDICK = 'X'. MESSAGE I176.   EXIT.
  ENDIF.

  IF ZTINS-ZFINSDT IS INITIAL.
    ZTINS-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Insurance policy date'.   EXIT.
  ENDIF.

  IF NOT ZTINS-ZFOPNO IS INITIAL.
    ASSIGN ZTINS-ZFOPNO         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ENDIF.

  IF ZTINS-ZFINSU1 IS INITIAL.
    ZTINS-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Insured person firm name'.   EXIT.
  ELSE.
    ASSIGN ZTINS-ZFINSU1        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
    ASSIGN ZTINS-ZFINSU1        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ENDIF.

  IF ZTINS-ZFELENM IS INITIAL.
    ZTINS-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Policyholder firm name'. EXIT.
  ENDIF.

  IF ZTINS-ZFREPRE IS INITIAL.
    ZTINS-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Representative name'.        EXIT.
  ENDIF.

  IF ZTINS-ZFELTXN IS INITIAL.
    ZTINS-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Registration number for taxation'.  EXIT.
  ENDIF.
  ASSIGN ZTINS-ZFELENM        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFREPRE        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFELTXN        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFELEID        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.

  IF ZTINS-ZFRSTAW IS INITIAL.
    ZTINS-ZFEDICK = 'X'. MESSAGE I167 WITH 'H/S CODE'.  EXIT.
  ENDIF.

  LOOP AT IT_ZSINSSG2.
    IF IT_ZSINSSG2-ZFDSOG1 IS INITIAL.
      ZTINS-ZFEDICK = 'X'. MESSAGE I167 WITH 'Goods detail'.  EXIT.
    ENDIF.
    ASSIGN IT_ZSINSSG2       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    ZTINS-ZFEDICK = 'X'. MESSAGE I167 WITH 'Goods detail'.  EXIT.
  ENDIF.

  IF ZTINSSG3-ZFCARNU IS INITIAL.
    ZTINS-ZFEDICK = 'X'. MESSAGE I167 WITH 'Voyage No'.  EXIT.
  ELSE.
    ASSIGN ZTINSSG3-ZFCARNU     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ENDIF.
  IF ZTINSSG3-ZFCARNM IS INITIAL.
    ZTINS-ZFEDICK = 'X'. MESSAGE I167 WITH 'Vessel name'.  EXIT.
  ELSE.
    ASSIGN ZTINSSG3-ZFCARNM     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ENDIF.

  IF ZTINS-ZFNUCD IS INITIAL.
    ZTINS-ZFEDICK = 'X'.
    MESSAGE I167 WITH
                 'Number of requested circulation for the copy'.
    EXIT.
  ENDIF.
  IF ZTINS-ZFNUOD IS INITIAL.
    ZTINS-ZFEDICK = 'X'.
    MESSAGE I167 WITH
                 'Number of requested circulation for the original'.
    EXIT.
  ENDIF.

  IF ZTINS-ZFIVAMT IS INITIAL.
    IF SY-TCODE EQ 'ZIM41' OR SY-TCODE EQ 'ZIM42' OR
       SY-TCODE EQ 'ZIM43' OR SY-TCODE EQ 'ZIM44'.
      ZTINS-ZFEDICK = 'X'. MESSAGE E167 WITH 'Invoice amount'.  EXIT.
    ELSE.
      MESSAGE I167 WITH 'Invoice amount'.
    ENDIF.
  ENDIF.
  IF ZTINS-WAERS   IS INITIAL.
    ZTINS-ZFEDICK = 'X'. MESSAGE E167 WITH 'Invoice Cur.'.  EXIT.
  ENDIF.

  IF ZTINS-ZFBSINS IS INITIAL.
    ZTINS-ZFEDICK = 'X'.
    MESSAGE I167 WITH 'Basic additional condition'.  EXIT.
  ENDIF.
* AMEND CHECK.....
  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR
     SY-TCODE EQ 'ZIM47' OR SY-TCODE EQ 'ZIM48'.
    IF ZTINS-ZFIVYN  NE 'X' AND ZTINS-ZFDUYN  NE 'X' AND
       ZTINS-ZFINAYN NE 'X' AND ZTINS-ZFTAMYN NE 'X' AND
       ZTINS-ZFPEYN  NE 'X' AND ZTINS-ZFPDYN  NE 'X' AND
       ZTINS-ZFDTYN  NE 'X' AND ZTINS-ZFPRYN  NE 'X' AND
       ZTINS-ZFDOYN  NE 'X' AND ZTINS-ZFTMYN  NE 'X' AND
       ZTINS-ZFHSYN  NE 'X' AND ZTINS-ZFGDYN  NE 'X' AND
       ZTINS-ZFPAYN  NE 'X' AND ZTINS-ZFEIYN  NE 'X' AND
       ZTINS-ZFCDYN  NE 'X' AND ZTINS-ZFETYN  NE 'X' AND
       ZTINS-ZFADYN  NE 'X'.
      ZTINS-ZFEDICK = 'X'.    MESSAGE E211.
    ENDIF.
  ENDIF.

  ASSIGN ZTINS-ZFFTX1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFFTX2         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFFTX3         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFFTX4         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFFTX5         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFETC1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFETC2         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFETC3         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFETC4         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.
  ASSIGN ZTINS-ZFETC5         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTINS-ZFEDICK NE 'X'.

ENDFORM.                    " P2000_INSURANCE_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_INS_DOC_DISP_SELECT
*&---------------------------------------------------------------------
FORM P2000_INS_DOC_DISP_SELECT.

  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO I~ZFINAMT
          I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST I~ZFINSEQ
       INTO (IT_ZSREQHD-ZFREQNO,
             IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
             IT_ZSREQHD-ZFOPNO, IT_ZSREQHD-ZFINAMT,
             IT_ZSREQHD-WAERS, IT_ZSREQHD-EBELN,
             IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
             IT_ZSREQHD-ZFINSEQ)
        FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
        ON    R~ZFREQNO    EQ   I~ZFREQNO
        WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
        AND   R~ZFAMDNO EQ '00000'
        AND   I~ZFAMDNO EQ '00000'
        ORDER  BY I~ZFREQNO.
    APPEND IT_ZSREQHD.
  ENDSELECT.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'INDISPLY'.
  CALL SCREEN 0014 STARTING AT  14 3
                   ENDING   AT  77 15.

ENDFORM.                    " P2000_INS_DOC_DISP_SELECT
*&---------------------------------------------------------------------
*&      Form  P2000_INS_DOC_DISP_SELECT1
*&---------------------------------------------------------------------
FORM P2000_INS_DOC_DISP_SELECT1.

  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO I~ZFINAMT
          I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST I~ZFINSEQ
       INTO (IT_ZSREQHD-ZFREQNO,
             IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
             IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
             IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
             IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
             IT_ZSREQHD-ZFINSEQ)
        FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
              ON R~ZFREQNO  EQ I~ZFREQNO
        WHERE R~EBELN   EQ ZSREQHD-EBELN
        AND   R~ZFAMDNO EQ '00000'
        AND   I~ZFAMDNO EQ '00000'.

    APPEND IT_ZSREQHD.

  ENDSELECT.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.         " GET POSITION
  W_STATUS_CHK = 'C'.
  INCLUDE = 'INDISPL1'.

  CALL SCREEN 0014 STARTING AT  01 3
                   ENDING   AT  90 15.

ENDFORM.                    " P2000_INS_DOC_DISP_SELECT1
*&---------------------------------------------------------------------
*&      Form  P2000_INS_DOC_DISP_SELECT2
*&---------------------------------------------------------------------
FORM P2000_INS_DOC_DISP_SELECT2.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO I~ZFINAMT
          I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST I~ZFINSEQ
          INTO (IT_ZSREQHD-ZFREQNO,
                IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
                IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
                IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
                IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
                IT_ZSREQHD-ZFINSEQ)
           FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                 ON R~ZFREQNO  EQ I~ZFREQNO
           WHERE R~ZFREQNO EQ  ZSREQHD-ZFREQNO
           AND   R~ZFAMDNO EQ '00000'
           AND   I~ZFAMDNO EQ '00000'.

    APPEND IT_ZSREQHD.

  ENDSELECT.

*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.

  IF TFILL EQ 1.
    READ TABLE IT_ZSREQHD INDEX 1.
    W_ZFOPNNO = IT_ZSREQHD-ZFOPNNO.
    W_ZFREQNO = IT_ZSREQHD-ZFREQNO.
    W_EBELN   = IT_ZSREQHD-EBELN.
    W_ZFINSEQ = IT_ZSREQHD-ZFINSEQ.
    ANTWORT = 'Y'.
  ELSE.
* PERFORM   P2000_GET_POSITION.         " GET POSITION
    W_STATUS_CHK = 'C'.
    INCLUDE = 'INDISPL2'.

    CALL SCREEN 0014 STARTING AT  05 3
                     ENDING   AT  92 15.
  ENDIF.
ENDFORM.                    " P2000_INS_DOC_DISP_SELECT2
*&---------------------------------------------------------------------
*&      Form  P2000_SET_INS_DOC_LOCK
*&---------------------------------------------------------------------
FORM P2000_SET_INS_DOC_LOCK USING    PA_MODE.

  IF SY-TCODE EQ 'ZIM41' AND PA_MODE EQ 'U'.
    IF SY-CALLD NE 'X'.
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
           EXPORTING
                ZFREQNO = ZSREQHD-ZFREQNO
                ZFAMDNO = ZTREQST-ZFAMDNO.
    ENDIF.
  ENDIF.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTINS'
         EXPORTING
              ZFREQNO = ZTINS-ZFREQNO
              ZFINSEQ = ZTINS-ZFINSEQ
              ZFAMDNO = ZTINS-ZFAMDNO
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Insurance document'
                            ZTINS-ZFREQNO ZTINS-ZFINSEQ
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTINS'
         EXPORTING
              ZFREQNO = ZTINS-ZFREQNO
              ZFINSEQ = ZTINS-ZFINSEQ
              ZFAMDNO = ZTINS-ZFAMDNO.
  ENDIF.


ENDFORM.                    " P2000_SET_INS_DOC_LOCK
*&---------------------------------------------------------------------
*&      Form  P2000_INS_DOC_DISPLAY
*&---------------------------------------------------------------------
FORM P2000_INS_DOC_DISPLAY.
  SET PARAMETER ID 'ZPOPNNO' FIELD ''.
  SET PARAMETER ID 'BES'     FIELD ''.
  SET PARAMETER ID 'ZPREQNO' FIELD ZSINSHD-ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO' FIELD ZSINSHD-ZFAMDNO.

  IF ZSINSHD-ZFAMDNO IS INITIAL.
    CALL TRANSACTION 'ZIM43' AND SKIP  FIRST SCREEN.
  ELSE.
    CALL TRANSACTION 'ZIM47' AND SKIP  FIRST SCREEN.
  ENDIF.
ENDFORM.                    " P2000_INS_DOC_DISPLAY
*&---------------------------------------------------------------------
*&      Form  P2000_INS_CHANGE_LIST2
*&---------------------------------------------------------------------
FORM P2000_INS_CHANGE_LIST2.

  WRITE : / IT_ZSREQHD-ZFINSEQ NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP,
            IT_ZSREQHD-ZFAMDNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EBELN   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFINAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFDOCST NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFOPNNO NO-GAP.

ENDFORM.                    " P2000_INS_CHANGE_LIST2
*&---------------------------------------------------------------------
*&      Form  P2000_INS_CHANGE_LIST1
*&---------------------------------------------------------------------
FORM P2000_INS_CHANGE_LIST1.

  WRITE : / IT_ZSREQHD-ZFREQNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP,
            IT_ZSREQHD-ZFINSEQ NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP,
            IT_ZSREQHD-ZFAMDNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFINAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFDOCST NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFOPNNO.

ENDFORM.                    " P2000_INS_CHANGE_LIST1
*&---------------------------------------------------------------------
*&      Form  P2000_INS_CHANGE_LIST
*&---------------------------------------------------------------------
*
FORM P2000_INS_CHANGE_LIST.
  WRITE : / IT_ZSREQHD-ZFREQNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP,
            IT_ZSREQHD-ZFINSEQ NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP,
            IT_ZSREQHD-ZFAMDNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EBELN   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFINAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFDOCST.

ENDFORM.                    " P2000_INS_CHANGE_LIST
*&---------------------------------------------------------------------
*&      Form  P1000_REQ_DOC_READ_INS
*&---------------------------------------------------------------------
FORM P1000_REQ_DOC_READ_INS.
  SELECT MAX( ZFAMDNO ) INTO W_AMDNO
                        FROM ZTREQST
                        WHERE ZFREQNO  EQ    ZSREQHD-ZFREQNO.

* Header Table
  CALL FUNCTION 'ZIM_GET_REQ_DOC_HEADER'
       EXPORTING
            ZFREQNO         = ZSREQHD-ZFREQNO
            ZFAMDNO         = W_AMDNO
       IMPORTING
            W_ZTREQHD       = ZTREQHD
            W_ZTREQST       = ZTREQST
       TABLES
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_ORG  = IT_ZSREQIL_ORG
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_ORG = IT_ZTREQORJ_ORG
       EXCEPTIONS
            NOT_FOUND       = 4
            NOT_INPUT       = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E018 WITH ZSREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.


* Lock Object Set
  IF SY-TCODE EQ 'ZIM41'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = ZSREQHD-ZFREQNO
              ZFAMDNO = ZSREQHD-ZFAMDNO
         EXCEPTIONS
              OTHERS  = 1.
    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Import Document'
                           ZSREQHD-ZFREQNO ZSREQHD-ZFAMDNO
                  RAISING DOCUMENT_LOCKED.
    ENDIF.
  ENDIF.

  W_ZFREQTY = ZTREQHD-ZFREQTY.
  W_COUNT = 1.

ENDFORM.                    " P1000_REQ_DOC_READ_INS
*&---------------------------------------------------------------------
*&      Form  P3000_AMEND_LOCAL_LC_MODIFY
*&---------------------------------------------------------------------
FORM P3000_AMEND_LOCAL_LC_MODIFY.

  CALL FUNCTION 'ZIM_LOCAL_LC_AMEND_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFREQNO         = ZTREQHD-ZFREQNO
            ZFAMDNO         = ZTREQST-ZFAMDNO
            ZFSTATUS        = W_STATUS
            W_ZTREQHD       = ZTREQHD
            W_ZTREQHD_OLD   = *ZTREQHD
            W_ZTREQST       = ZTREQST
            W_ZTREQST_OLD   = *ZTREQST
            W_ZTLLCAMHD     = ZTLLCAMHD
            W_ZTLLCAMHD_OLD = *ZTLLCAMHD
       TABLES
            IT_ZSLLCAMSGOF  = IT_ZSLLCAMSGOF
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_OLD  = IT_ZSREQIL_OLD
            IT_ZSREQIT      = IT_ZSREQIT
            IT_ZSREQIT_OLD  = IT_ZSREQIT_OLD
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_OLD = IT_ZTREQORJ_OLD
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E017.
  ENDIF.

ENDFORM.                    " P3000_AMEND_LOCAL_LC_MODIFY
*&---------------------------------------------------------------------
*&      Form  P2000_AMEND_DOC_SELECT
*&---------------------------------------------------------------------
FORM P2000_AMEND_DOC_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  IF ZSREQHD-ZFAMDNO IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
          FROM   ZVREQHD_ST
          WHERE  ZFOPNNO   EQ ZSREQHD-ZFOPNNO
          AND    ZFAMDNO   GT '00000'
          ORDER  BY ZFREQNO.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
          FROM   ZVREQHD_ST
          WHERE  ZFOPNNO   EQ ZSREQHD-ZFOPNNO
          AND    ZFAMDNO   EQ ZSREQHD-ZFAMDNO
          ORDER  BY ZFREQNO.
  ENDIF.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LCDISPLY'.
  CALL SCREEN 0014 STARTING AT  04 3
                   ENDING   AT  97 16.

ENDFORM.                    " P2000_AMEND_DOC_SELECT
*&---------------------------------------------------------------------
*&      Form  P2000_AMEND_DOC_SELECT_1
*&---------------------------------------------------------------------
FORM P2000_AMEND_DOC_SELECT_1.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  IF ZSREQHD-ZFAMDNO IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
          FROM   ZVREQHD_ST
          WHERE  EBELN     EQ ZSREQHD-EBELN
          AND    ZFAMDNO   GT '00000'
          ORDER  BY ZFREQNO.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
          FROM   ZVREQHD_ST
          WHERE  EBELN     EQ ZSREQHD-EBELN
          AND    ZFAMDNO   EQ ZSREQHD-ZFAMDNO
          ORDER  BY ZFREQNO.
  ENDIF.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LCCHANGE'.
  CALL SCREEN 0014 STARTING AT  10 3
                   ENDING   AT  93 16.

ENDFORM.                    " P2000_AMEND_DOC_SELECT_1
*&---------------------------------------------------------------------
*&      Form  P2000_AMEND_DOC_SELECT_2
*&---------------------------------------------------------------------
FORM P2000_AMEND_DOC_SELECT_2.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_EBELN   = ZSREQHD-EBELN.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  IF ZSREQHD-ZFAMDNO IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
          FROM   ZVREQHD_ST
          WHERE  ZFREQNO   EQ ZSREQHD-ZFREQNO
          AND    ZFAMDNO   GT '00000'
          ORDER  BY ZFREQNO.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
          FROM   ZVREQHD_ST
          WHERE  ZFREQNO   EQ ZSREQHD-ZFREQNO
          AND    ZFAMDNO   EQ ZSREQHD-ZFAMDNO
          ORDER  BY ZFREQNO.
  ENDIF.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'LCDISPL1'.
  CALL SCREEN 0014 STARTING AT  13 3
                   ENDING   AT  85 16.

ENDFORM.                    " P2000_AMEND_DOC_SELECT_2
*&---------------------------------------------------------------------
*&      Form  P2000_LC_DISPLAY_LIST_1
*&---------------------------------------------------------------------
FORM P2000_LC_DISPLAY_LIST_1.
  WRITE : / IT_ZSREQHD-ZFAMDNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.
  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EKORG   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EKGRP   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
*          IT_ZSREQHD-ZFLASTAM CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          IT_ZSREQHD-ZFOPAMT  CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFREQDT NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRVDT  NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFDOCST NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRLST1 NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRLST2 NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFRTNYN NO-GAP.

ENDFORM.                    " P2000_LC_DISPLAY_LIST_1
*&---------------------------------------------------------------------
*&      Form  P2000_ZIM93_DISPLAY
*&---------------------------------------------------------------------
FORM P2000_ZIM93_DISPLAY USING    P_ZFREQNO
                                  P_EBELN.

  IF P_ZFREQNO IS INITIAL AND  P_EBELN IS INITIAL.
    MESSAGE E063.
  ENDIF.

  SET PARAMETER ID 'BES'     FIELD P_EBELN.
  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
*  SET PARAMETER ID 'BES'     FIELD ''.
*  SET PARAMETER ID 'ZPREQNO' FIELD ''.
*  EXPORT P_EBELN    TO MEMORY ID 'BES'.
*  EXPORT P_ZFREQNO  TO MEMORY ID 'ZPREQNO'.
  CALL TRANSACTION 'ZIM93' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_ZIM93_DISPLAY
*&---------------------------------------------------------------------
*&      Form  P2000_DDLC_PROCESS
*&---------------------------------------------------------------------
FORM P2000_DDLC_PROCESS.

  CASE F.
* Import Document
    WHEN 'ZTREQHD-ZFREQNO' OR 'ZTREQST-ZFAMDNO' OR
         'ZTREQST-ZFOPNNO' OR 'ZTOFF-ZFREQNO'.
      CASE SY-TCODE(4).
        WHEN 'ZIM0' OR 'ZIM1'.
        WHEN OTHERS.
          IF SY-TCODE(4) EQ 'ZIML'.
            SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO FROM ZTREQST
                               WHERE ZFREQNO  EQ   ZTOFF-ZFREQNO
.
            PERFORM  P2000_LC_DISPLAY    USING  ZTOFF-ZFREQNO
                                                W_ZFAMDNO
                                                ''.
          ELSE.
            PERFORM  P2000_LC_DISPLAY    USING  ZTREQHD-ZFREQNO
                                                ZTREQST-ZFAMDNO
                                                ZTREQST-ZFOPNNO.
          ENDIF.
      ENDCASE.
* Purchase Order Document
    WHEN 'ZTREQHD-EBELN'.
      PERFORM  P2000_PO_DOCUMENT_DISPLAY.
    WHEN 'ZTREQHD-MATNR'.
      SET PARAMETER ID 'MAT' FIELD ZTREQHD-MATNR.
      SET PARAMETER ID 'BUK' FIELD ZTREQHD-BUKRS.
      SET PARAMETER ID 'WRK' FIELD ZTREQHD-ZFWERKS.
      SET PARAMETER ID 'LAG' FIELD ''.
      SET PARAMETER ID 'MXX' FIELD MM03_START_SICHT.
      CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.
* vendor
    WHEN 'ZTREQHD-LIFNR'  OR 'ZTREQHD-ZFBENI' OR 'ZTREQHD-ZFOPBN'   OR
         'ZTREQHD-ZFLEVN' OR 'ZTINS-ZFOPBN'   OR 'ZTOFF-ZFBENI'     OR
         'ZTTTHD-ZFBENI'  OR 'ZTTTHD-ZFOPBN'  OR 'ZTLLCAMHD-ZFBENI' OR
         'ZTLLCHD-ZFOPBN' OR 'ZTMLCHD-ZFBENI' OR 'ZTMLCHD-ZFOPBN'   OR
         'ZTREQHD-LLIEF'.

      ASSIGN (F)   TO    <FS_F>.
      PERFORM  P2000_VENDOR_MASTER USING <FS_F>.
* USER id
*      WHEN 'ZTREQST-ZFOPNNM' OR 'ZTREQST-ERNAM'.
*         ASSIGN (F)   TO    <FS_F>.
*         PERFORM  P2000_USER_MASTER   USING <FS_F>.
    WHEN OTHERS.

      IF F(7)  EQ 'ZSREQIT'.
        IF W_LINE0103 GT 0.
          READ TABLE IT_ZSREQIT  INDEX  W_LINE0103.
          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING  IT_ZSREQIT  TO  W_ZSREQIT.
            W_SEL_MAT_CNT = 1.
            PERFORM P2000_PO_ITEM_DISPLAY.
*                  PERFORM P2000_MATERIAL_DISPLAY.
          ENDIF.
        ENDIF.
      ENDIF.

      IF F(8)  EQ 'ZSPURSG1'.
        IF LINE GT 0.
          READ TABLE IT_ZSPURSG1 INDEX  LINE.
          IF SY-SUBRC EQ 0.
            PERFORM   P2000_SCR0123_PROCESS.
          ENDIF.
        ENDIF.
      ENDIF.

      IF F(8)  EQ 'ZSPURSG4'.
        IF LINE GT 0.
          READ TABLE IT_ZSPURSG4 INDEX  LINE.
          IF SY-SUBRC EQ 0.
            PERFORM   P2000_SCR0125_PROCESS.
          ENDIF.
        ENDIF.
      ENDIF.
      IF F(8)  EQ 'ZSOFFSG6'.
        IF W_LINE2103 GT 0.
          READ TABLE IT_ZSOFFSG6 INDEX  W_LINE2103.
          MOVE-CORRESPONDING    IT_ZSOFFSG6   TO   ZTOFFSG6.
          IF SY-SUBRC EQ 0.
            PERFORM   P2000_SCR2103_PROCESS.
          ENDIF.
        ENDIF.
      ENDIF.

      IF F(7)  EQ 'ZSREQIL'.
        IF LINE GT 0.
          READ TABLE IT_ZSREQIL  INDEX  LINE.
          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING   IT_ZSREQIL   TO   ZSREQIL.
            PERFORM  P2000_RECOMMAND_PROCESS.
          ENDIF.
        ENDIF.
      ENDIF.

      IF F(7)  EQ 'ZSINSHD'.
        IF LINE GT 0.
          READ TABLE IT_ZSINSHD  INDEX  LINE.
          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING   IT_ZSINSHD   TO   ZSINSHD.
            PERFORM  P2000_INS_DOC_DISPLAY.
          ENDIF.
        ENDIF.
      ENDIF.

      IF F(7)  EQ 'ZSLLCOF'.
        IF LINE GT 0.
          READ TABLE IT_ZSLLCOF INDEX  LINE.
          IF SY-SUBRC EQ 0.
            SET PARAMETER ID 'BSP' FIELD ''.
            EXPORT 'BSP' TO MEMORY ID 'BSP'.
            SET PARAMETER ID 'BES' FIELD IT_ZSLLCOF-ZFOFFER.
            EXPORT 'BES'  TO MEMORY ID 'BES'.
            CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_DDLC_PROCESS
*&---------------------------------------------------------------------
*
*&      Form  P2000_VENDOR_MASTER
*&---------------------------------------------------------------------
*
FORM P2000_VENDOR_MASTER USING    P_LIFNR.
  IF P_LIFNR IS INITIAL.
    MESSAGE S174.    EXIT.
  ENDIF.

  SET PARAMETER ID 'KDY' FIELD '/110/120/130'.

  SET PARAMETER ID 'LIF' FIELD P_LIFNR.

  SET PARAMETER ID 'EKO' FIELD ''.

  CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_VENDOR_MASTER
*&---------------------------------------------------------------------
*
*&      Form  P2000_LC_DISPLAY
*&---------------------------------------------------------------------
*
FORM P2000_LC_DISPLAY USING    P_ZFREQNO
                               P_ZFAMDNO
                               P_ZFOPNNO.

  IF P_ZFREQNO IS INITIAL AND P_ZFAMDNO IS INITIAL AND
     P_ZFOPNNO IS INITIAL.
    MESSAGE E066.
  ENDIF.

  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
  SET PARAMETER ID 'ZPOPNNO'   FIELD P_ZFOPNNO.

  IF P_ZFAMDNO IS INITIAL.
    CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
  ELSE.
    CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                    " P2000_LC_DISPLAY
*&---------------------------------------------------------------------
*&      Form  P3000_INS_EDI_SEND
*&---------------------------------------------------------------------
FORM P3000_INS_EDI_SEND.
*-----------------------------------------------------------------------
* Status Check
*-----------------------------------------------------------------------
  IF ZTINS-ZFEDICK EQ 'X'.
    MESSAGE E119 WITH ZTINS-ZFREQNO ZTINS-ZFAMDNO.
  ENDIF.
  IF ZTINS-ZFEDIST NE 'N'.
    MESSAGE E105 WITH ZTINS-ZFREQNO
                      ZTINS-ZFAMDNO ZTINS-ZFEDIST.
  ENDIF.
* STATUS CHECK
  PERFORM  P2000_INS_OPEN_CHECK       CHANGING W_ERR_CHK.
*-----------------------------------------------------------------------
  W_ZFCDDOC = 'APPCIP'.
  CLEAR : W_ZFDHSRO.
  LOOP AT IT_ZSINSSG5.
    SELECT SINGLE * FROM ZTIMIMG08 WHERE ZFCDTY EQ '010'
                                   AND   ZFCD   EQ IT_ZSINSSG5-ZFINSC.
    W_ZFDHSRO = ZTIMIMG08-ZFCD4.
    EXIT.
  ENDLOOP.
  IF W_ZFDHSRO IS INITIAL.
    MESSAGE E281 WITH IT_ZSINSSG5-ZFINSC.
  ENDIF.
  W_ZFDHREF = ZTINS-ZFREQNO.
*  W_ZFDHDDB = ZTREQST-EKORG.
  W_ZFDHENO = ZTINS-ZFDOCNO.

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
            W_ZFCDDOC = W_ZFCDDOC
            W_ZFDHSRO = W_ZFDHSRO
            W_ZFDHREF = W_ZFDHREF
*            W_ZFDHDDB = W_ZFDHDDB
            W_BUKRS     = ZTINS-BUKRS
       CHANGING
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4
            NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_APPCIP_EDI_SEND'
       EXPORTING
            W_ZFREQNO = ZTINS-ZFREQNO
            W_ZFINSEQ = ZTINS-ZFINSEQ
            W_ZFAMDNO = ZTINS-ZFAMDNO
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

  ZTINS-ZFDOCST = 'R'.
  ZTINS-ZFDOCNO = W_ZFDHENO.
  ZTINS-UDAT    = SY-DATUM.
  ZTINS-UNAM    = SY-UNAME.

ENDFORM.                    " P3000_INS_EDI_SEND

*&---------------------------------------------------------------------
*
*&      Form  P2000_INS_OPEN_CHECK
*&---------------------------------------------------------------------
*
FORM P2000_INS_OPEN_CHECK CHANGING W_ERR_CHK.

  CASE ZTINS-ZFDOCST.
    WHEN 'R'.
      IF ZTINS-ZFEDIST = 'S'.
        MESSAGE W999 WITH ZTINS-ZFREQNO 'EDI open requested' 'Open'.
      ELSE.
        MESSAGE W999 WITH ZTINS-ZFREQNO 'EDI Data created' 'Open'.
      ENDIF.
    WHEN 'O'.
      IF ZTINS-ZFEDIST = 'R'.
        MESSAGE W999 WITH ZTINS-ZFREQNO 'EDI created' 'Open'.
      ELSE.
        MESSAGE S998 WITH ZTINS-ZFREQNO 'Non-EDI created'.
      ENDIF.
    WHEN 'C'.                                               " CANCEL
      MESSAGE E999 WITH ZTINS-ZFREQNO 'Canceled'  'Open'.
    WHEN 'A'.                                               " AMEND
      MESSAGE E999 WITH ZTINS-ZFREQNO 'Amended'  'Open'.
  ENDCASE.

  W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_INS_OPEN_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_EDI_DOC_CHECK
*&---------------------------------------------------------------------
FORM P2000_EDI_DOC_CHECK.
  CASE SY-TCODE.
    WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
      CASE  ZTREQHD-ZFREQTY.
        WHEN 'LC'.           " IMPORT MASTER L/C
          PERFORM   P2000_MASTER_LC_CHECK.
        WHEN 'LO'.           " LOCAL L/C
          PERFORM   P2000_LOCAL_LC_CHECK.
        WHEN 'PU'.           " Purchase Approve
          PERFORM   P2000_PURCH_DOC_CHECK.
        WHEN 'TT'.           " Pay Order
          PERFORM   P2000_PAYORD_DOC_CHECK.
        WHEN 'DA' OR 'DP'.   " D/A or D/P
          MESSAGE E997 WITH ZTREQHD-ZFREQTY.
      ENDCASE.
      IF ZTREQST-ZFEDICK = 'O'.
        MESSAGE I123 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
      ENDIF.
    WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
      CASE  ZTREQHD-ZFREQTY.
        WHEN 'LC'.           " IMPORT MASTER L/C AMEND
          PERFORM   P2000_MASTER_LC_AMD_CHECK.
        WHEN 'LO'.
          PERFORM   P2000_LOCAL_LC_AMD_CHECK.
        WHEN 'PU'.
          PERFORM   P2000_PURCH_DOC_CHECK.
        WHEN 'DA' OR 'DP' OR 'TT'.    " D/A or D/P or T/T
          MESSAGE E997 WITH ZTREQHD-ZFREQTY.
      ENDCASE.
      IF ZTREQST-ZFEDICK = 'O'.
        MESSAGE I123 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
      ENDIF.

    WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3'.
      PERFORM   P2000_OFFER_SHEET_CHECK.
      IF ZTOFF-ZFEDICK = 'O'.
        MESSAGE I123 WITH ZTOFF-ZFREQNO ''.
      ENDIF.
    WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44'.
      PERFORM   P2000_INSURANCE_CHECK.
      IF ZTINS-ZFEDICK = 'O'.
        MESSAGE I123 WITH ZTINS-ZFREQNO ZTINS-ZFAMDNO.
      ENDIF.
    WHEN 'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
      PERFORM   P2000_INSURANCE_CHECK.
      IF ZTINS-ZFEDICK = 'O'.
        MESSAGE I123 WITH ZTINS-ZFREQNO ZTINS-ZFAMDNO.
      ENDIF.

  ENDCASE.
ENDFORM.                    " P2000_EDI_DOC_CHECK
*&---------------------------------------------------------------------
*
*&      Form  P2000_REQ_IL_DISPLAY
*&---------------------------------------------------------------------
*
FORM P2000_REQ_IL_DISPLAY.

  LOOP AT IT_ZSREQIL WHERE ZFMARK EQ 'X'.
    W_COUNT = W_COUNT + 1.
    MOVE-CORRESPONDING   IT_ZSREQIL   TO   ZSREQIL.
  ENDLOOP.
  CASE W_COUNT.
    WHEN 0.
      MESSAGE E951.
    WHEN 1.    PERFORM  P2000_RECOMMAND_PROCESS.
    WHEN OTHERS.
      MESSAGE E965.
  ENDCASE.

ENDFORM.                    " P2000_REQ_IL_DISPLAY
*&---------------------------------------------------------------------
*
*&      Form  P2000_INS_STATUS_CHECK
*&---------------------------------------------------------------------
*
FORM P2000_INS_STATUS_CHECK.

  CASE W_STATUS.
    WHEN C_REQ_C.
      IF SY-TCODE EQ 'ZIM45'.
        PERFORM   P2000_INS_AMEND_CHECK.
      ENDIF.
    WHEN C_REQ_U.
      PERFORM P2000_INS_CHAGE_CHECK.
    WHEN C_REQ_D.
      PERFORM P2000_INS_DISPLAY_CHECK.
    WHEN C_OPEN_C.
      PERFORM P2000_INS_OPEN_CHECK       CHANGING W_ERR_CHK.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_INS_STATUS_CHECK

*&---------------------------------------------------------------------
*&      Form  P2000_INS_CHAGE_CHECK
*&---------------------------------------------------------------------
FORM P2000_INS_CHAGE_CHECK.

  CASE ZTINS-ZFDOCST.
    WHEN 'R'.
      IF ZTINS-ZFEDIST = 'S'.
        MESSAGE E999 WITH ZTINS-ZFREQNO 'EDI open requested' 'Change'.
      ELSEIF ZTINS-ZFEDIST = 'N'.
        MESSAGE E999 WITH ZTINS-ZFREQNO 'Flat Data created'
                                        'Change'.
      ENDIF.
    WHEN 'O'.
      IF ZTINS-ZFEDIST = 'R'.
        MESSAGE E999 WITH ZTINS-ZFREQNO 'EDI created' 'Change'.
      ELSE.
        MESSAGE E999
                WITH ZTINS-ZFREQNO 'Non-EDI created' 'Change'.
      ENDIF.
      EXIT.
    WHEN 'C'.                                               " CANCEL
      MESSAGE E999 WITH ZTINS-ZFREQNO 'Canceled' 'Change'.
    WHEN 'A'.                                               " AMEND
      MESSAGE E999 WITH ZTINS-ZFREQNO 'Amended' 'Change'.
  ENDCASE.

ENDFORM.                    " P2000_INS_CHAGE_CHECK

*&---------------------------------------------------------------------
*
*&      Form  P2000_INS_DISPLAY_CHECK
*&---------------------------------------------------------------------
*
FORM P2000_INS_DISPLAY_CHECK.

  CASE ZTINS-ZFDOCST.
    WHEN 'R'.
      IF ZTINS-ZFEDIST = 'S'.
        MESSAGE S998 WITH ZTINS-ZFREQNO 'EDI open requested'.
      ENDIF.
    WHEN 'O'.
      IF ZTINS-ZFEDIST = 'R'.
        MESSAGE S998 WITH ZTINS-ZFREQNO 'EDI opened'.
      ELSE.
        MESSAGE S998 WITH ZTINS-ZFREQNO 'Non-EDI opened'.
      ENDIF.
    WHEN 'C'.                                               " CANCEL
      MESSAGE S998 WITH ZTINS-ZFREQNO 'Canceled'.
    WHEN 'A'.                                               " AMEND
      MESSAGE S998 WITH ZTINS-ZFREQNO 'Amended'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_INS_DISPLAY_CHECK
*&---------------------------------------------------------------------
*
*&      Form  P2000_SCR0007_TO_TAB
*&---------------------------------------------------------------------
*
FORM P2000_SCR0007_TO_TAB.

  READ TABLE IT_ZSOFFSG6 WITH KEY ZFLSG6 = ZSOFFSG6-ZFLSG6.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING   ZTOFFSG6   TO IT_ZSOFFSG6.
    MODIFY   IT_ZSOFFSG6   INDEX    SY-TABIX.
  ENDIF.

ENDFORM.                    " P2000_SCR0007_TO_TAB
*&---------------------------------------------------------------------
*
*&      Form  P2000_PO_ITEM_DISPLAY
*&---------------------------------------------------------------------
*
FORM P2000_PO_ITEM_DISPLAY.

  SET PARAMETER ID 'BSP' FIELD W_ZSREQIT-ZFITMNO.
  EXPORT 'BSP' TO MEMORY ID 'BSP'.
  SET PARAMETER ID 'BES' FIELD ZTREQHD-EBELN.
  EXPORT 'BES'  TO MEMORY ID 'BES'.

  CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_PO_ITEM_DISPLAY
*&---------------------------------------------------------------------
*
*&      Form  P3000_SET_ITEM_MARK
*&---------------------------------------------------------------------
*
FORM P3000_SET_ITEM_MARK USING    P_INDEX    P_MARK.

  IF P_INDEX EQ 0.
    LOOP AT IT_ZSREQIT.
      IT_ZSREQIT-ZFMARK = P_MARK.
      MODIFY IT_ZSREQIT INDEX SY-TABIX.
    ENDLOOP.
  ELSE.
    READ TABLE IT_ZSREQIT INDEX P_INDEX.
    IT_ZSREQIT-ZFMARK = P_MARK.
    MODIFY IT_ZSREQIT INDEX P_INDEX.
  ENDIF.

ENDFORM.                    " P3000_SET_ITEM_MARK
*&---------------------------------------------------------------------
*&      Form  P1000_INSURANCE_READ
*&---------------------------------------------------------------------
*
FORM P1000_INSURANCE_READ.

  REFRESH : IT_ZSINSHD.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSINSHD
           FROM  ZTINS
           WHERE ZFREQNO EQ ZSREQHD-ZFREQNO
           ORDER  BY ZFREQNO ZFINSEQ ZFAMDNO.

ENDFORM.                    " P1000_INSURANCE_READ
*&---------------------------------------------------------------------
*&      Form  P1000_PREV_NEXT_DOC
*&---------------------------------------------------------------------
*
FORM P1000_PREV_NEXT_DOC USING    P_ZFREQNO
                                  P_AMEND.

  SELECT SINGLE * INTO W_ZTREQST FROM ZTREQST
                  WHERE ZFREQNO EQ P_ZFREQNO
                  AND   ZFAMDNO EQ P_AMEND.

  IF SY-SUBRC EQ 0.
    PERFORM  P2000_LC_DISPLAY    USING  P_ZFREQNO
                                        P_AMEND
                                        ''.
  ELSE.
    MESSAGE E179.
  ENDIF.

ENDFORM.                    " P1000_PREV_NEXT_DOC
*&---------------------------------------------------------------------
*
*&      Form  P2000_RECOMMEND_MESSAGE
*&---------------------------------------------------------------------
*
FORM P2000_RECOMMEND_MESSAGE.

  PERFORM P2000_MESSAGE_BOX USING 'Import Recommendation Confirm'
                                  'This Case is Import Recommendation'
                                  'Do you want input Now?'
                                  'N'
                                  '2'.

ENDFORM.                    " P2000_RECOMMEND_MESSAGE
*&---------------------------------------------------------------------
*&      Form  P2000_SET_RECOMMEND_SCR
*&---------------------------------------------------------------------
FORM P2000_SET_RECOMMEND_SCR.

  IF SY-TCODE(4) EQ 'ZIM0'.
    CASE ZTREQHD-ZFREQTY.
      WHEN 'LC'.                 OK-CODE = 'ITM8'.
      WHEN 'TT' OR 'DA' OR 'DP'. OK-CODE = 'ITM3'.
    ENDCASE.
  ELSEIF SY-TCODE(4) EQ 'ZIM1'.
    CASE ZTREQHD-ZFREQTY.
      WHEN 'LC'.                 OK-CODE = 'ITM6'.
      WHEN 'TT' OR 'DA' OR 'DP'. OK-CODE = 'ITM3'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_SET_RECOMMEND_SCR
*&---------------------------------------------------------------------
*
*&      Form  P2000_AMD_CHAGE_CHECK
*&---------------------------------------------------------------------
*
FORM P2000_AMD_CHAGE_CHECK.

  W_ERR_CHK = 'Y'.
*>> IMG SETTING CHECK!
  SELECT  SINGLE * FROM  ZTIMIMG00.

  IF ZTIMIMG00-ZFRELYN3 EQ 'X' AND ZTREQST-ZFRLST1 EQ 'R'.
    IF SY-LANGU EQ 'KO'.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                              'requet released' 'Change'.
    ELSE.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                              'request released' 'Change'.
    ENDIF.

  ENDIF.

  IF ZTIMIMG00-ZFRELYN3 EQ 'X' AND  ZTREQST-ZFRVDT > '00000000'.
    IF SY-LANGU EQ 'KO'.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                        'registered from import support team' 'Change'.

    ELSE.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                        'registered from import support team' 'Change'.
    ENDIF.
  ENDIF.

  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.
      IF ZTREQST-ZFEDIST = 'S'.
        IF SY-LANGU EQ 'KO'.
          MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                   'EDI open requested' 'Change'.
        ELSE.
          MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                   'EDI Open requested' 'Change'.
        ENDIF.
      ENDIF.
    WHEN 'O'.
      IF ZTREQST-ZFEDIST = 'R'.
        IF SY-LANGU EQ 'KO'.
          MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                         'EDI opened' 'Change'.
        ELSE.
          MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                         'EDI Opened' 'Change'.
        ENDIF.
      ELSE.
        IF SY-LANGU EQ 'KO'.
          MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                    'Non-EDI opened' 'Change'.
        ELSE.
          MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                    'Non-EDI opened' 'Change'.
        ENDIF.
      ENDIF.
      EXIT.
    WHEN 'C'.                                               " CANCEL
      IF SY-LANGU EQ '3'.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'Canceled' 'Change'.
      ELSE.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'cancel' 'Change'.
      ENDIF.
    WHEN 'A'.                                               " AMEND
      IF SY-LANGU EQ '3'.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'Amended' 'Change'.
      ELSE.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'Amend' 'Change'.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_AMD_CHAGE_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_AMD_DISPLAY_CHECK
*&---------------------------------------------------------------------
*
FORM P2000_AMD_DISPLAY_CHECK.

  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.
      IF SY-LANGU EQ 'KO'.
        MESSAGE S998 WITH ZTREQST-ZFREQNO 'EDI open requested'.
      ELSE.
        MESSAGE S998 WITH ZTREQST-ZFREQNO 'EDI Open requested'.
      ENDIF.
    WHEN 'O'.
      IF ZTREQST-ZFEDIST = 'R'.
        IF SY-LANGU EQ 'KO'.
          MESSAGE S998 WITH ZTREQST-ZFREQNO 'EDI opened'.
        ELSE.
          MESSAGE S998 WITH ZTREQST-ZFREQNO 'EDI oened'.
        ENDIF.
      ELSE.
        IF SY-LANGU EQ '3'.
          MESSAGE S998 WITH ZTREQST-ZFREQNO 'Non-EDI opened'.
        ELSE.
          MESSAGE S998 WITH ZTREQST-ZFREQNO 'Non-EDI oended'.
        ENDIF.
      ENDIF.
    WHEN 'C'.                                               " CANCEL
      IF SY-LANGU EQ '3'.
        MESSAGE S998 WITH ZTREQST-ZFREQNO 'Canceled'.
      ELSE.
        MESSAGE S998 WITH ZTREQST-ZFREQNO 'CALCELED'.
      ENDIF.
    WHEN 'A'.                                               " AMEND
      IF SY-LANGU EQ 'KO'.
        MESSAGE S998 WITH ZTREQST-ZFREQNO 'Amended'.
      ELSE.
        MESSAGE S998 WITH ZTREQST-ZFREQNO 'AMENDED'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_AMD_DISPLAY_CHECK

*&---------------------------------------------------------------------
*
*&      Form  P2000_AMD_ADD_CHANGE_CHECK
*&---------------------------------------------------------------------
*
FORM P2000_AMD_ADD_CHANGE_CHECK.

  W_ERR_CHK = 'Y'.

  IF ZTIMIMG00-ZFRELYN3 EQ 'X' AND ZTREQST-ZFRLST1 NE 'R'.
    IF SY-LANGU EQ '3'.
      MESSAGE E999 WITH
     ZTREQHD-ZFREQNO 'Not request released' 'Additional change'.
    ELSE.
      MESSAGE E999 WITH
     ZTREQHD-ZFREQNO 'Not request released' 'Additional change'.
    ENDIF.
  ENDIF.

  IF ZTIMIMG00-ZFRELYN4 EQ 'X' AND ZTREQST-ZFRLST2 EQ 'R'.
    IF SY-LANGU EQ 'KO'.
      MESSAGE E999
      WITH ZTREQHD-ZFREQNO 'Open released' 'Additional change'.
    ELSE.
      MESSAGE E999
      WITH ZTREQHD-ZFREQNO 'Open released' 'Additional change'.
    ENDIF.
  ENDIF.

  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.
      IF ZTREQST-ZFEDIST = 'S'.
        IF SY-LANGU EQ 'KO'.
          MESSAGE E999
         WITH ZTREQHD-ZFREQNO 'EDI open released' 'Additional change'.
        ELSE.
          MESSAGE E999
         WITH ZTREQHD-ZFREQNO 'EDI Open requested' 'Additional change'.
        ENDIF.
      ENDIF.
    WHEN 'O'.
      IF ZTREQST-ZFEDIST = 'R'.
        IF SY-LANGU EQ 'KO'.
          MESSAGE E999
          WITH ZTREQHD-ZFREQNO 'EDI opened' 'Additional change'.
        ELSE.
          MESSAGE E999
          WITH ZTREQHD-ZFREQNO 'EDI Opened' 'Additional change'.
        ENDIF.
      ELSE.
        IF SY-LANGU EQ 'KO'.
          MESSAGE E999
          WITH ZTREQHD-ZFREQNO 'Non-EDI opened' 'Additional change'.
        ELSE.
          MESSAGE E999
          WITH ZTREQHD-ZFREQNO 'Non-EDI Opened' 'Additional change'.
        ENDIF.
      ENDIF.
    WHEN 'C'.                                               " CANCEL
      IF SY-LANGU EQ 'KO'.
        MESSAGE E999
        WITH ZTREQHD-ZFREQNO 'CANCELED' 'ADDITIONAL CHANGE'.
        EXIT.
      ELSE.
        MESSAGE E999
        WITH ZTREQHD-ZFREQNO 'CANCELED' 'ADDITIONAL CHANGE'.
        EXIT.
      ENDIF.
    WHEN 'A'.                                               " AMEND
      IF SY-LANGU EQ 'KO'.
        MESSAGE E999
        WITH ZTREQHD-ZFREQNO 'Amended' 'Additional change'.
      ELSE.
        MESSAGE E999
        WITH ZTREQHD-ZFREQNO 'Amended' 'Additional change'.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_AMD_ADD_CHANGE_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_AMD_OPEN_CHECK
*&---------------------------------------------------------------------
FORM P2000_AMD_OPEN_CHECK.

  IF ZTIMIMG00-ZFRELYN3 EQ 'X' AND ZTREQST-ZFRLST1 NE 'R'.
    IF SY-LANGU EQ '3'.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                   'Not request released' 'Open'.
    ELSE.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                   'Not request released' 'Open'.
    ENDIF.
  ENDIF.

  IF ZTREQST-ZFRVDT IS INITIAL.
    IF SY-LANGU EQ '3'.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                   'Not registered in import support team' 'Open'.
    ELSE.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                   'Not registered in import support team' 'Open'.
    ENDIF.
  ENDIF.

  IF ZTREQST-ZFRTNYN EQ 'X'.
    IF SY-LANGU EQ '3'.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                   'Registration returned' 'Open'.
    ELSE.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                   'Registration returned' 'Open'.
    ENDIF.
  ENDIF.

  IF ZTIMIMG00-ZFRELYN4 EQ 'X' AND ZTREQST-ZFRLST2 NE 'R'.
    IF SY-LANGU EQ '3'.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                   'Not open released' 'Open'.
      EXIT.
    ELSE.
      MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                   'Not Open Release' 'Open'.
      EXIT.
    ENDIF.
  ENDIF.

  CASE ZTREQST-ZFDOCST.
    WHEN 'R'.
      IF ZTREQST-ZFEDIST = 'S'.
        IF SY-LANGU EQ '3'.
          MESSAGE W994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                            'EDI Open requested'  'Open'.
        ELSE.
          MESSAGE W994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                            'EDI Open requested'  'Open'.
        ENDIF.
      ELSE.
        IF SY-LANGU EQ '3'.
          MESSAGE W994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                        'EDI Data Created'  'Open'.
        ELSE.
          MESSAGE W994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                        'EDI Data Created'  'Open'.
        ENDIF.
      ENDIF.
    WHEN 'O'.
      IF ZTREQST-ZFEDIST = 'R'.
        IF SY-LANGU EQ '3'.
          MESSAGE W994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                         'EDI opened' 'Open'.
          EXIT.
        ELSE.
          MESSAGE W994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                         'EDI Opened' 'Open'.
          EXIT.
        ENDIF.
      ELSE.
        IF SY-CALLD EQ 'X'.
          IF SY-LANGU EQ '3'.
            MESSAGE S995 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                         'Non-EDI Opened'.
          ELSE.
            MESSAGE S995 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                         'Non-EDI Opened'.
          ENDIF.
        ELSE.
          IF SY-LANGU EQ '3'.
            MESSAGE I995 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                          'Non-EDI Opened'.
          ELSE.
            MESSAGE I995 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                          'Non-EDI Opened'.
          ENDIF.

        ENDIF.
      ENDIF.
    WHEN 'C'.                                               " CANCEL
      IF SY-LANGU EQ '3'.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'CANCELED'  'Open'.
      ELSE.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'CANCELED'  'Open'.
      ENDIF.
    WHEN 'A'.                                               " AMEND
      IF SY-LANGU EQ '3'.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'AMENDED'  'Open'.
      ELSE.
        MESSAGE E994 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                                       'AMENDED'  'Open'.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_AMD_OPEN_CHECK
*&---------------------------------------------------------------------
*
*&      Form  P3000_AMEND_EDI_SEND
*&---------------------------------------------------------------------
FORM P3000_AMEND_EDI_SEND.

  IF ZTREQST-ZFEDICK EQ 'X'.
    MESSAGE E119 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
  ENDIF.
  IF ZTREQST-ZFEDIST NE 'N'.
    MESSAGE E105 WITH ZTREQST-ZFREQNO
                      ZTREQST-ZFAMDNO ZTREQST-ZFEDIST.
  ENDIF.
* STATUS CHECK
  PERFORM P2000_OPEN_CHECK       CHANGING W_ERR_CHK.

  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFOPBN
                              CHANGING   W_OPEN_NM.
  IF W_LFA1-BAHNS IS INITIAL.
    MESSAGE E274 WITH ZTREQHD-ZFOPBN.
  ENDIF.
*-----------------------------------------------------------------------
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      PERFORM   P3000_MLC_AMD_FLAT_CREATE.
    WHEN 'LO'.
      PERFORM   P3000_LOCAL_AMD_FLAT_CREATE.
    WHEN 'PU'.
      PERFORM   P3000_PURCH_DOC_FLAT_CREATE.
    WHEN OTHERS.
      MESSAGE E997 WITH ZTREQHD-ZFREQTY.
  ENDCASE.

  ZTREQST-ZFDOCST = 'R'.
  ZTREQST-ZFDOCNO = W_ZFDHENO.
  ZTREQST-UDAT    = SY-DATUM.
  ZTREQST-UNAM    = SY-UNAME.

ENDFORM.                    " P3000_AMEND_EDI_SEND

*&---------------------------------------------------------------------
*
*&      Form  P2000_AMD_DEL_STATUS_CHECK
*&---------------------------------------------------------------------
*
FORM P2000_AMD_DEL_STATUS_CHECK.

  IF ZTREQST-ZFDOCST NE 'N'.
    MESSAGE E104 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                      ZTREQST-ZFDOCST.
  ENDIF.
  IF ZTREQST-ZFEDIST NE 'N'.
    MESSAGE E105 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO
                      ZTREQST-ZFEDIST.
  ENDIF.
*-----------------------------------------------------------------------
* Follow Document Check
*-----------------------------------------------------------------------
* Amend
  SELECT COUNT( * ) INTO W_COUNT FROM ZTREQST
                    WHERE ZFREQNO EQ ZTREQST-ZFREQNO
                    AND   ZFAMDNO GT ZTREQST-ZFAMDNO.

  IF W_COUNT > 0.
    MESSAGE E106 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
  ENDIF.
* B/L CHECK
  SELECT COUNT( * ) INTO W_COUNT FROM ZTBLIT
                    WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
  IF W_COUNT > 0.
    MESSAGE E269 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
  ENDIF.
* Commercial Invoice Check!
  SELECT COUNT( * ) INTO W_COUNT FROM ZTCIVIT
                    WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
  IF W_COUNT > 0.
    MESSAGE E394 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
  ENDIF.
* Invoice
  SELECT COUNT( * ) INTO W_COUNT FROM ZTIVIT
                    WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
  IF W_COUNT > 0.
    MESSAGE E107 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO W_COUNT.
  ENDIF.

ENDFORM.                    " P2000_AMD_DEL_STATUS_CHECK
*&---------------------------------------------------------------------
*&      Form  P1000_READ_MASTER_LC_AMEND
*&---------------------------------------------------------------------
FORM P1000_READ_MASTER_LC_AMEND.

  CALL FUNCTION 'ZIM_GET_MASTER_LC_AMEND_DATA'
       EXPORTING
            ZFREQNO            = ZTREQHD-ZFREQNO
            ZFAMDNO            = ZSREQHD-ZFAMDNO
       IMPORTING
            W_ZTMLCAMHD        = ZTMLCAMHD
       TABLES
            IT_ZSMLCAMNARR     = IT_ZSMLCAMNARR
            IT_ZSMLCAMNARR_ORG = IT_ZSMLCAMNARR_ORG
       EXCEPTIONS
            NOT_FOUND          = 4
            NOT_INPUT          = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E194 WITH ZTREQHD-ZFREQNO ZSREQHD-ZFAMDNO.
    WHEN 8.
      MESSAGE E195.
  ENDCASE.

   *ZTMLCAMHD = ZTMLCAMHD.

ENDFORM.                    " P1000_READ_MASTER_LC_AMEND
*&---------------------------------------------------------------------
*&      Form  P1000_READ_LOCAL_LC_AMEND
*&---------------------------------------------------------------------
FORM P1000_READ_LOCAL_LC_AMEND USING P_ZFAMDNO.

  CALL FUNCTION 'ZIM_GET_LOCAL_LC_AMEND_DATA'
       EXPORTING
            ZFREQNO            = ZTREQHD-ZFREQNO
            ZFAMDNO            = P_ZFAMDNO
       IMPORTING
            W_ZTLLCAMHD        = ZTLLCAMHD
       TABLES
            IT_ZSLLCAMSGOF     = IT_ZSLLCAMSGOF
            IT_ZSLLCAMSGOF_ORG = IT_ZSLLCAMSGOF_ORG
       EXCEPTIONS
            NOT_FOUND          = 4
            NOT_INPUT          = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E194 WITH ZTREQHD-ZFREQNO P_ZFAMDNO.
    WHEN 8.
      MESSAGE E195.
  ENDCASE.

   *ZTLLCAMHD = ZTLLCAMHD.

ENDFORM.                    " P1000_READ_LOCAL_LC_AMEND
*&---------------------------------------------------------------------
*&      Form  P3000_MLC_AMD_FLAT_CREATE
*&---------------------------------------------------------------------
FORM P3000_MLC_AMD_FLAT_CREATE.

  W_ZFCDDOC = 'APP707'.
  W_ZFDHSRO = W_LFA1-BAHNS.
  IF ZTREQST-ZFOPNNO IS INITIAL.
    W_ZFDHREF = ZTREQHD_TMP-ZFOPNNO.
  ELSE.
    W_ZFDHREF = ZTREQST-ZFOPNNO.
  ENDIF.
*  W_ZFDHDDB = ZTREQST-EKORG.
  W_ZFDHENO = ZTREQST-ZFDOCNO.

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
            W_ZFCDDOC = W_ZFCDDOC
            W_ZFDHSRO = W_ZFDHSRO
            W_ZFDHREF = W_ZFDHREF
*            W_ZFDHDDB = W_ZFDHDDB
            W_BUKRS     = ZTREQHD-BUKRS
       CHANGING
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4
            NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_APP707_EDI_SEND'
       EXPORTING
            W_ZFREQNO = ZTREQHD-ZFREQNO
            W_ZFAMDNO = ZTREQST-ZFAMDNO
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

ENDFORM.                    " P3000_MLC_AMD_FLAT_CREATE
*&---------------------------------------------------------------------
*&      Form  P3000_LOCAL_AMD_FLAT_CREATE
*&---------------------------------------------------------------------
FORM P3000_LOCAL_AMD_FLAT_CREATE.

  W_ZFCDDOC = 'LOCAMR'.
  W_ZFDHSRO = W_LFA1-BAHNS.
  IF ZTREQST-ZFOPNNO IS INITIAL.
    W_ZFDHREF = ZTREQHD_TMP-ZFOPNNO.
  ELSE.
    W_ZFDHREF = ZTREQST-ZFOPNNO.
  ENDIF.
*  W_ZFDHDDB = ZTREQST-EKORG.
  W_ZFDHENO = ZTREQST-ZFDOCNO.

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
            W_ZFCDDOC = W_ZFCDDOC
            W_ZFDHSRO = W_ZFDHSRO
            W_ZFDHREF = W_ZFDHREF
            W_BUKRS     = ZTREQHD-BUKRS
*            W_ZFDHDDB = W_ZFDHDDB
       CHANGING
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4
            NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_LOCAMR_EDI_SEND'
       EXPORTING
            W_ZFREQNO = ZTREQHD-ZFREQNO
            W_ZFAMDNO = ZTREQST-ZFAMDNO
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

ENDFORM.                    " P3000_LOCAL_AMD_FLAT_CREATE
*&---------------------------------------------------------------------
*&      Form  P2000_MASTER_LC_AMD_CHECK
*&---------------------------------------------------------------------
FORM P2000_MASTER_LC_AMD_CHECK.

  LOOP AT IT_ZSREQIT.
    PERFORM   P2000_REQ_ITEM_CHECK   TABLES  IT_ZSREQIT
                                     USING   'E'.
  ENDLOOP.
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTREQHD-BUKRS.

  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    IF ZTIMIMGTX-APP707 EQ 'X'.
      ZTREQST-ZFEDICK = 'O'.

      IF ZTMLCAMHD-ZFOPME IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I152.   EXIT.
      ENDIF.

      IF ZTREQST-ZFAPPDT IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I153.   EXIT.
      ENDIF.

      IF ZTREQHD-ZFOPBN  IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I154.   EXIT.
      ENDIF.

      PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFOPBN
                                  CHANGING   W_OPEN_NM.


      IF ZTMLCHD-ZFOBNM  IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I155.   EXIT.
      ELSE.
        ASSIGN ZTMLCHD-ZFOBNM       TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK.
        CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTMLCHD-ZFOBBR       TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK.
        CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDIF.

      IF W_LFA1-BAHNS IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I157.   EXIT.
      ENDIF.
      IF ZTMLCSG2-ZFAPPNM IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I158.   EXIT.
      ELSE.
        ASSIGN ZTMLCSG2-ZFAPPNM     TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK.
        CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTMLCSG2-ZFAPPAD1    TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK.
        CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTMLCSG2-ZFAPPAD2    TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK.
        CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTMLCSG2-ZFAPPAD3    TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK.
        CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTMLCSG2-ZFTELNO     TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK.
        CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDIF.

      IF ZTMLCAMHD-ZFBENI  IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I159.   EXIT.
      ENDIF.

      IF ZTMLCAMHD-ZFBENI1 IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I160.   EXIT.
      ELSE.
        ASSIGN ZTMLCAMHD-ZFBENI1       TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTMLCAMHD-ZFBENI2    TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTMLCAMHD-ZFBENI3    TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTMLCAMHD-ZFBENI4    TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
        ASSIGN ZTMLCAMHD-ZFBENIA    TO <FS_F>.
       PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDIF.

      IF ZTMLCSG2-ZFELENM IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I161.   EXIT.
      ENDIF.
      IF ZTMLCSG2-ZFELEID IS INITIAL.
        ZTREQST-ZFEDICK = 'X'. MESSAGE I162.   EXIT.
      ENDIF.
      ASSIGN ZTMLCSG2-ZFELENM     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFREPRE     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFELEID     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFELEAD1    TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCSG2-ZFELEAD2    TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

      W_FIELD_CHK = 0.
   IF NOT ( ZTMLCAMHD-ZFETC1 IS INITIAL AND ZTMLCAMHD-ZFETC2 IS INITIAL
        AND ZTMLCAMHD-ZFETC3 IS INITIAL AND ZTMLCAMHD-ZFETC4 IS INITIAL
                                      AND ZTMLCAMHD-ZFETC4 IS INITIAL ).
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.
      ASSIGN ZTMLCAMHD-ZFETC1     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCAMHD-ZFETC2     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCAMHD-ZFETC3     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCAMHD-ZFETC4     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCAMHD-ZFETC5     TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

      IF NOT ZTMLCAMHD-ZFIDCD IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.

      IF NOT ZTMLCAMHD-ZFALCQ IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.

      IF NOT ZTMLCAMHD-ZFALCP IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.

      IF NOT ZTMLCAMHD-ZFALCM IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.

      IF NOT ZTMLCAMHD-ZFALCQ IS INITIAL.
        IF ZTMLCAMHD-ZFIDAM IS INITIAL.
          ZTREQST-ZFEDICK = 'X'.
          IF SY-LANGU EQ 'KO'.
            MESSAGE I977 WITH
       'When input More or Less rate, Amount after change is mandatory'.
          ELSE.
            MESSAGE I977
            WITH 'Input Amount'.
          ENDIF.
        ENDIF.
      ENDIF.

      IF NOT ( ZTMLCAMHD-ZFNAMT1 IS INITIAL AND
               ZTMLCAMHD-ZFNAMT2 IS INITIAL AND
               ZTMLCAMHD-ZFNAMT3 IS INITIAL AND
               ZTMLCAMHD-ZFNAMT4 IS INITIAL ).
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.
      ASSIGN ZTMLCAMHD-ZFNAMT1    TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCAMHD-ZFNAMT2    TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCAMHD-ZFNAMT3    TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
      ASSIGN ZTMLCAMHD-ZFNAMT4    TO <FS_F>.
      PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

      IF NOT ZTMLCAMHD-ZFNEXDT IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.

      IF NOT ZTMLCAMHD-ZFNLTSD IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.

      IF NOT ZTMLCAMHD-ZFNSPRT IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
        ASSIGN ZTMLCAMHD-ZFNSPRT    TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK.
        CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDIF.

      IF NOT ZTMLCAMHD-ZFNAPRT IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
        ASSIGN ZTMLCAMHD-ZFNAPRT    TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK.
        CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDIF.

      LOOP AT IT_ZSMLCAMNARR.
        W_FIELD_CHK = W_FIELD_CHK + 1.
        ASSIGN IT_ZSMLCAMNARR-ZFNARR TO <FS_F>.
        PERFORM P2000_TEXT_FIELD_CHECK.
        CHECK : ZTREQST-ZFEDICK NE 'X'.
      ENDLOOP.
      IF W_FIELD_CHK EQ 0.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE E211.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_MASTER_LC_AMD_CHECK
*&---------------------------------------------------------------------
*&      Form  P2000_LOCAL_LC_AMD_CHECK
*&---------------------------------------------------------------------
FORM P2000_LOCAL_LC_AMD_CHECK.

  ZTREQST-ZFEDICK = 'X'.
  LOOP AT IT_ZSREQIT.
    PERFORM   P2000_REQ_ITEM_CHECK   TABLES  IT_ZSREQIT
                                     USING   'E'.
  ENDLOOP.
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTREQHD-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    IF ZTIMIMGTX-LOCAMR EQ 'X'.
      ZTREQST-ZFEDICK = 'O'.

      W_FIELD_CHK = 0.

      IF SY-TCODE EQ 'ZIM11'.
        IF ZTREQST-ZFAMDNO IS INITIAL.
          IF ZTLLCAMHD-ZFBENI1 NE ZTLLCSG23-ZFBENI1.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
          IF ZTLLCAMHD-ZFBENI2 NE ZTLLCSG23-ZFBENI2.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
          IF ZTLLCAMHD-ZFBENI3 NE ZTLLCSG23-ZFBENI3.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
        ELSE.
          IF ZTLLCAMHD-ZFBENI1 NE ZTLLCAMHD_TMP-ZFBENI1.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
          IF ZTLLCAMHD-ZFBENI1 NE ZTLLCAMHD_TMP-ZFBENI1.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
          IF ZTLLCAMHD-ZFBENI1 NE ZTLLCAMHD_TMP-ZFBENI1.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
        ENDIF.
      ELSE.
        IF ZTREQST-ZFAMDNO EQ '00001'.
          IF ZTLLCAMHD-ZFBENI1 NE ZTLLCSG23-ZFBENI1.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
          IF ZTLLCAMHD-ZFBENI2 NE ZTLLCSG23-ZFBENI2.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
          IF ZTLLCAMHD-ZFBENI3 NE ZTLLCSG23-ZFBENI3.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
        ELSE.
          IF ZTLLCAMHD-ZFBENI1 NE ZTLLCAMHD_TMP-ZFBENI1.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
          IF ZTLLCAMHD-ZFBENI1 NE ZTLLCAMHD_TMP-ZFBENI1.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
          IF ZTLLCAMHD-ZFBENI1 NE ZTLLCAMHD_TMP-ZFBENI1.
            W_FIELD_CHK = W_FIELD_CHK + 1.
          ENDIF.
        ENDIF.
      ENDIF.

      IF NOT ZTLLCAMHD-ZFECON1 IS INITIAL AND
         NOT ZTLLCAMHD-ZFECON2 IS INITIAL AND
         NOT ZTLLCAMHD-ZFECON3 IS INITIAL AND
         NOT ZTLLCAMHD-ZFECON4 IS INITIAL AND
         NOT ZTLLCAMHD-ZFECON5 IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.

      IF NOT ZTLLCAMHD-ZFNEXDT IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.

      IF NOT ZTLLCAMHD-ZFNOAMT IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.

      IF NOT ZTLLCAMHD-ZFNGDDT IS INITIAL.
        W_FIELD_CHK = W_FIELD_CHK + 1.
      ENDIF.

      IF W_FIELD_CHK EQ 0.
        ZTREQST-ZFEDICK = 'X'.    MESSAGE E211.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " P2000_LOCAL_LC_AMD_CHECK
*&---------------------------------------------------------------------
*&      Module  READ_DOC_SCR4500  INPUT
*&---------------------------------------------------------------------
MODULE READ_DOC_SCR4500 INPUT.
*-----------------------------------------------------------------------
* OK-CODE
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-EBELN IS INITIAL AND  ZSREQHD-ZFREQNO IS INITIAL AND
     ZSREQHD-ZFOPNNO IS INITIAL.
    MESSAGE E066.
  ENDIF.
*
  IF NOT ZSREQHD-ZFOPNNO IS INITIAL.

    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                      ON    R~ZFREQNO    EQ   I~ZFREQNO
                      WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
                      AND   R~ZFAMDNO EQ '00000'
*                      AND   I~ZFAMDNO GE '00000'
                      AND   I~ZFDOCST EQ 'O'.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E545 WITH ZSREQHD-ZFOPNNO.
      WHEN 1.
        SELECT R~ZFREQNO I~ZFINSEQ I~ZFAMDNO
                  INTO (ZSREQHD-ZFREQNO,
                        ZSREQHD-ZFINSEQ, ZSREQHD-ZFAMDNO)
                  UP TO 1 ROWS
                  FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                  ON    R~ZFREQNO    EQ   I~ZFREQNO
                  WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
                  AND   R~ZFAMDNO EQ '00000'
*                  AND   I~ZFAMDNO GE '00000'
                  AND   I~ZFDOCST EQ 'O'.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_INS_AMD_CRT_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.

    PERFORM   P1000_REQ_DOC_READ_INS.
    EXIT.
  ENDIF.

  IF NOT ZSREQHD-EBELN IS INITIAL.
* P/O NO Count
    SELECT COUNT( * ) INTO  W_COUNT
          FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
          ON    R~ZFREQNO    EQ   I~ZFREQNO
          WHERE R~EBELN   EQ ZSREQHD-EBELN
          AND   R~ZFAMDNO EQ '00000'
*          AND   I~ZFAMDNO GE '00000'
          AND   I~ZFDOCST EQ 'O'.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E546 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT R~ZFREQNO I~ZFINSEQ I~ZFAMDNO
               INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFINSEQ, ZSREQHD-ZFAMDNO)
               UP TO 1 ROWS
               FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
               ON    R~ZFREQNO    EQ   I~ZFREQNO
               WHERE R~EBELN   EQ ZSREQHD-EBELN
               AND   R~ZFAMDNO EQ '00000'
*               AND   I~ZFAMDNO GE '00000'
               AND   I~ZFDOCST EQ 'O'.
          EXIT.
        ENDSELECT.

      WHEN OTHERS.
        PERFORM P2000_INS_AMD_CRT_SELECT1.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.

    PERFORM   P1000_REQ_DOC_READ_INS.
    EXIT.
  ENDIF.

  IF ZSREQHD-ZFINSEQ IS INITIAL.
* P/O NO Count
    SELECT COUNT( * ) INTO  W_COUNT
          FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
          ON    R~ZFREQNO    EQ   I~ZFREQNO
          WHERE R~ZFREQNO EQ ZSREQHD-ZFREQNO
          AND   R~ZFAMDNO EQ '00000'
*           AND   I~ZFAMDNO EQ '00000'
          AND   I~ZFDOCST EQ 'O'.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E547 WITH ZSREQHD-ZFREQNO.
      WHEN 1.
        SELECT R~ZFREQNO I~ZFINSEQ I~ZFAMDNO
                 INTO (ZSREQHD-ZFREQNO, ZSREQHD-ZFINSEQ,
                       ZSREQHD-ZFAMDNO)
                 UP TO 1 ROWS
                 FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                 ON    R~ZFREQNO    EQ   I~ZFREQNO
                 WHERE R~ZFREQNO EQ ZSREQHD-ZFREQNO
                 AND   R~ZFAMDNO EQ '00000'
                 AND   I~ZFAMDNO EQ '00000'
                 AND   I~ZFDOCST EQ 'O'.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_INS_DOC_DISP_SELECT3.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.

    PERFORM   P1000_REQ_DOC_READ_INS.
    EXIT.

  ENDIF.

  PERFORM   P1000_REQ_DOC_READ_INS.

ENDMODULE.                 " READ_DOC_SCR4500  INPUT
*&---------------------------------------------------------------------
*
*&      Form  P2000_INS_AMD_DISP_SELECT
*&---------------------------------------------------------------------
*
FORM P2000_INS_AMD_DISP_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
*  IF ZSREQHD-ZFINSEQ IS INITIAL.
  IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend seq.
    SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
            I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
            I~ZFINSEQ
      INTO (IT_ZSREQHD-ZFREQNO,
            IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
            IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
            IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
            IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
            IT_ZSREQHD-ZFINSEQ)
*     SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
         FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
         ON    R~ZFREQNO    EQ   I~ZFREQNO
         WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
         AND   R~ZFAMDNO EQ '00000'
         AND   I~ZFAMDNO NE '00000'
         ORDER  BY I~ZFREQNO.
      APPEND IT_ZSREQHD.
    ENDSELECT.
  ELSE.
    SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
            I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
            I~ZFINSEQ
      INTO (IT_ZSREQHD-ZFREQNO,
            IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
            IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
            IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
            IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
            IT_ZSREQHD-ZFINSEQ)
*     SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSREQHD
          FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
          ON    R~ZFREQNO    EQ   I~ZFREQNO
          WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
          AND   R~ZFAMDNO EQ '00000'
          AND   I~ZFAMDNO EQ ZSREQHD-ZFAMDNO
          ORDER  BY I~ZFREQNO.
      APPEND IT_ZSREQHD.
    ENDSELECT.
  ENDIF.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'INDISPLY'.
  CALL SCREEN 0014 STARTING AT  01 03
                   ENDING   AT  65 15.
* CALL SCREEN 0014 STARTING AT  X1 Y1
*                  ENDING   AT  X2 Y2.

ENDFORM.                    " P2000_INS_AMD_DISP_SELECT
*&---------------------------------------------------------------------
*
*&      Form  P2000_INS_AMD_DISP_SELECT1
*&---------------------------------------------------------------------
*
FORM P2000_INS_AMD_DISP_SELECT1.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
*   IF ZSREQHD-ZFINSEQ IS INITIAL.
  IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend Seq.
    SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
            I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
            I~ZFINSEQ
      INTO (IT_ZSREQHD-ZFREQNO,
            IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
            IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
            IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
            IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
            IT_ZSREQHD-ZFINSEQ)
       FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
             ON R~ZFREQNO  EQ I~ZFREQNO
       WHERE R~EBELN   EQ ZSREQHD-EBELN
       AND   R~ZFAMDNO EQ '00000'
       AND   I~ZFAMDNO NE '00000'.
      APPEND IT_ZSREQHD.
    ENDSELECT.
  ELSE.
    SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
            I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
            I~ZFINSEQ
      INTO (IT_ZSREQHD-ZFREQNO,
            IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
            IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
            IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
            IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
            IT_ZSREQHD-ZFINSEQ)
       FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
             ON R~ZFREQNO  EQ I~ZFREQNO
       WHERE R~EBELN   EQ ZSREQHD-EBELN
       AND   R~ZFAMDNO EQ '00000'
       AND   I~ZFAMDNO EQ ZSREQHD-ZFAMDNO.
      APPEND IT_ZSREQHD.
    ENDSELECT.
  ENDIF.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.         " GET POSITION
  W_STATUS_CHK = 'C'.
  INCLUDE = 'INDISPL1'.

  CALL SCREEN 0014 STARTING AT  01 03
                   ENDING   AT  90 15.
* CALL SCREEN 0014 STARTING AT  12 3
*                  ENDING   AT  77 15.

ENDFORM.                    " P2000_INS_AMD_DISP_SELECT1
*&---------------------------------------------------------------------
*
*&      Form  P2000_AMD_DOC_DISP_SELECT2
*&---------------------------------------------------------------------
*
FORM P2000_AMD_DOC_DISP_SELECT2.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  IF ZSREQHD-ZFAMDNO IS INITIAL.
    SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
            I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
            I~ZFINSEQ
         INTO (IT_ZSREQHD-ZFREQNO,
               IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
               IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
               IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
               IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
               IT_ZSREQHD-ZFINSEQ)
          FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                ON R~ZFREQNO  EQ I~ZFREQNO
          WHERE R~ZFREQNO EQ  ZSREQHD-ZFREQNO
          AND   R~ZFAMDNO EQ '00000'
          AND   I~ZFAMDNO NE '00000'.
      APPEND IT_ZSREQHD.
    ENDSELECT.
  ELSE.
    SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
            I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
            I~ZFINSEQ
         INTO (IT_ZSREQHD-ZFREQNO,
               IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
               IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
               IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
               IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
               IT_ZSREQHD-ZFINSEQ)
          FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                ON R~ZFREQNO  EQ I~ZFREQNO
          WHERE R~ZFREQNO EQ  ZSREQHD-ZFREQNO
          AND   R~ZFAMDNO EQ '00000'
          AND   I~ZFAMDNO EQ ZSREQHD-ZFAMDNO.
      APPEND IT_ZSREQHD.
    ENDSELECT.
  ENDIF.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.

  IF TFILL EQ 1.
    READ TABLE IT_ZSREQHD INDEX 1.
    W_ZFOPNNO = IT_ZSREQHD-ZFOPNNO.
    W_ZFREQNO = IT_ZSREQHD-ZFREQNO.
    W_EBELN   = IT_ZSREQHD-EBELN.
    W_ZFAMDNO = IT_ZSREQHD-ZFAMDNO.
    W_ZFINSEQ = IT_ZSREQHD-ZFINSEQ.
    ANTWORT = 'Y'.
  ELSE.
* PERFORM   P2000_GET_POSITION.         " GET POSITION
    W_STATUS_CHK = 'C'.
    INCLUDE = 'INDISPL2'.

    CALL SCREEN 0014 STARTING AT  01 3
                     ENDING   AT  90 15.
  ENDIF.

ENDFORM.                    " P2000_AMD_DOC_DISP_SELECT2
*&---------------------------------------------------------------------
*&      Form  P2000_AMD_DOC_DISP_SELECT3
*&---------------------------------------------------------------------
FORM P2000_AMD_DOC_DISP_SELECT3.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
          I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
          I~ZFINSEQ
       INTO (IT_ZSREQHD-ZFREQNO,
             IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
             IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
             IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
             IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
             IT_ZSREQHD-ZFINSEQ)
        FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
              ON R~ZFREQNO  EQ I~ZFREQNO
        WHERE R~ZFREQNO EQ  ZSREQHD-ZFREQNO
*         AND   I~ZFINSEQ EQ  ZSREQHD-ZFINSEQ
        AND   R~ZFAMDNO EQ '00000'
        AND   I~ZFAMDNO NE '00000'.
    APPEND IT_ZSREQHD.
  ENDSELECT.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.

  IF TFILL EQ 1.
    READ TABLE IT_ZSREQHD INDEX 1.
    W_ZFOPNNO = IT_ZSREQHD-ZFOPNNO.
    W_ZFREQNO = IT_ZSREQHD-ZFREQNO.
    W_EBELN   = IT_ZSREQHD-EBELN.
    W_ZFAMDNO = IT_ZSREQHD-ZFAMDNO.
    W_ZFINSEQ = IT_ZSREQHD-ZFINSEQ.
    ANTWORT = 'Y'.
  ELSE.
* PERFORM   P2000_GET_POSITION.         " GET POSITION
    W_STATUS_CHK = 'C'.
    INCLUDE = 'INDISPL2'.                 " º¸Çè º¯?

    CALL SCREEN 0014 STARTING AT  01 3
                     ENDING   AT  90 15.
  ENDIF.

ENDFORM.                    " P2000_AMD_DOC_DISP_SELECT3
*&---------------------------------------------------------------------
*&      Form  P2000_INS_AMD_CRT_SELECT
*&---------------------------------------------------------------------
FORM P2000_INS_AMD_CRT_SELECT.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO I~ZFINAMT
          I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST I~ZFINSEQ
          INTO (IT_ZSREQHD-ZFREQNO,
                IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
                IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
                IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
                IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
                IT_ZSREQHD-ZFINSEQ)
           FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
           ON    R~ZFREQNO    EQ   I~ZFREQNO
           WHERE R~ZFOPNNO EQ ZSREQHD-ZFOPNNO
           AND   R~ZFAMDNO EQ '00000'
           AND   I~ZFAMDNO GE '00000'
           AND   I~ZFDOCST EQ 'O'
           ORDER  BY I~ZFREQNO.
    APPEND IT_ZSREQHD.
  ENDSELECT.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'INDISPLY'.                 " º¸Çè »ý?
  CALL SCREEN 0014 STARTING AT  01 3
                   ENDING   AT  65 15.

ENDFORM.                    " P2000_INS_AMD_CRT_SELECT

*&---------------------------------------------------------------------
*&      Form  P2000_INS_AMD_CRT_SELECT1
*&---------------------------------------------------------------------
FORM P2000_INS_AMD_CRT_SELECT1.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO I~ZFINAMT
          I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST I~ZFINSEQ
          INTO (IT_ZSREQHD-ZFREQNO,
                IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
                IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
                IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
                IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
                IT_ZSREQHD-ZFINSEQ)
           FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                 ON R~ZFREQNO  EQ I~ZFREQNO
           WHERE R~EBELN   EQ ZSREQHD-EBELN
           AND   R~ZFAMDNO EQ '00000'
           AND   I~ZFAMDNO GE '00000'
           AND   I~ZFDOCST EQ 'O'.

    APPEND IT_ZSREQHD.

  ENDSELECT.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.         " GET POSITION
  W_STATUS_CHK = 'C'.
  INCLUDE = 'INDISPL1'.                 " º¸Çè º¯?

  CALL SCREEN 0014 STARTING AT  01 03
                   ENDING   AT  90 15.

ENDFORM.                    " P2000_INS_AMD_CRT_SELECT1

*&---------------------------------------------------------------------
*&      Form  P2000_INS_AMD_CRT_SELECT2
*&---------------------------------------------------------------------
FORM P2000_INS_AMD_CRT_SELECT2.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.
*WRITE : /EQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO I~ZFINAMT
          I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST I~ZFINSEQ
          INTO (IT_ZSREQHD-ZFREQNO,
                IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
                IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
                IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
                IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
                IT_ZSREQHD-ZFINSEQ)
           FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
                 ON R~ZFREQNO  EQ I~ZFREQNO
           WHERE R~ZFREQNO EQ  ZSREQHD-ZFREQNO
           AND   R~ZFAMDNO EQ '00000'
           AND   I~ZFAMDNO EQ '00000'
           AND   I~ZFDOCST EQ 'O'.
    APPEND IT_ZSREQHD.

  ENDSELECT.

*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.

  IF TFILL EQ 1.
    READ TABLE IT_ZSREQHD INDEX 1.
    W_ZFOPNNO = IT_ZSREQHD-ZFOPNNO.
    W_ZFREQNO = IT_ZSREQHD-ZFREQNO.
    W_EBELN   = IT_ZSREQHD-EBELN.
    W_ZFAMDNO = IT_ZSREQHD-ZFAMDNO.
    W_ZFINSEQ = IT_ZSREQHD-ZFINSEQ.
    ANTWORT = 'Y'.
  ELSE.
* PERFORM   P2000_GET_POSITION.         " GET POSITION
    W_STATUS_CHK = 'C'.
    INCLUDE = 'INDISPL2'.                 " º¸Çè º¯?

    CALL SCREEN 0014 STARTING AT  01 3
                     ENDING   AT  90 15.
  ENDIF.

ENDFORM.                    " P2000_INS_AMD_CRT_SELECT2
*&---------------------------------------------------------------------
*&      Form  P3000_INS_AMD_EDI_SEND
*&---------------------------------------------------------------------
FORM P3000_INS_AMD_EDI_SEND.
*-----------------------------------------------------------------------
* »óÅÂ °ËÁõ Ãß?
*-----------------------------------------------------------------------
  IF ZTINS-ZFEDICK EQ 'X'.
    MESSAGE E119 WITH ZTINS-ZFREQNO ZTINS-ZFAMDNO.
  ENDIF.
  IF ZTINS-ZFEDIST NE 'N'.
    MESSAGE E105 WITH ZTINS-ZFREQNO
                      ZTINS-ZFAMDNO ZTINS-ZFEDIST.
  ENDIF.
* STATUS CHECK
  PERFORM  P2000_INS_OPEN_CHECK       CHANGING W_ERR_CHK.
*-----------------------------------------------------------------------
  W_ZFCDDOC = 'APPEND'.
  CLEAR : W_ZFDHSRO.
  LOOP AT IT_ZSINSSG5.
    SELECT SINGLE * FROM ZTIMIMG08 WHERE ZFCDTY EQ '010'
                                   AND   ZFCD   EQ IT_ZSINSSG5-ZFINSC.
    W_ZFDHSRO = ZTIMIMG08-ZFCD4.        " ½Äº°?
    EXIT.
  ENDLOOP.
  W_ZFDHREF = ZTINS-ZFREQNO.     " ÂüÁ¶¹ø?
*  W_ZFDHDDB = ZTREQST-EKORG.       " ºÎ?
  W_ZFDHENO = ZTINS-ZFDOCNO.     " ¹®¼­¹ø?

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
            W_ZFCDDOC = W_ZFCDDOC
            W_ZFDHSRO = W_ZFDHSRO
            W_ZFDHREF = W_ZFDHREF
*            W_ZFDHDDB = W_ZFDHDDB
            W_BUKRS     = ZTINS-BUKRS
       CHANGING
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4
            NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_APPEND_EDI_SEND'
       EXPORTING
            W_ZFREQNO = ZTINS-ZFREQNO
            W_ZFAMDNO = ZTINS-ZFAMDNO
            W_ZFINSEQ = ZTINS-ZFINSEQ
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

  ZTINS-ZFDOCST = 'R'.
  ZTINS-ZFDOCNO = W_ZFDHENO.
  ZTINS-UDAT    = SY-DATUM.
  ZTINS-UNAM    = SY-UNAME.

ENDFORM.                    " P3000_INS_AMD_EDI_SEND
*&---------------------------------------------------------------------
*
*&      Form  P2000_ITEM_CHANGE_HISTORY
*&---------------------------------------------------------------------
*
FORM P2000_ITEM_CHANGE_HISTORY.
  CASE W_SEL_MAT_CNT.
    WHEN 0.
      MESSAGE S951.
    WHEN 1.
      OBJECTCLASS   =   'ZIREQIT'.
      SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                       WITH  OBJEKTID =   W_ZSREQIT+3(15)
                       AND   RETURN.

    WHEN OTHERS.
      MESSAGE S965.
  ENDCASE.

ENDFORM.                    " P2000_ITEM_CHANGE_HISTORY

*&---------------------------------------------------------------------
*
*&      Form  P2000_HEADER_CHANGE_DOC
*&---------------------------------------------------------------------
*
*       text
*----------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
*
FORM P2000_HEADER_CHANGE_DOC.
  CASE SY-TCODE.
* ¼öÀÔÀÇ·Ú¹®?
    WHEN 'ZIM01' OR 'ZIM02' OR 'ZIM03' OR 'ZIM05' OR 'ZIM07'.
      CASE ZTREQHD-ZFREQTY.
        WHEN 'LC'.
          OBJECTCLASS   =   'ZILCDOC'.
          OBJEKTID = ZTREQST+3(15).
        WHEN 'LO'.
          OBJECTCLASS   =   'ZILODOC'.
          OBJEKTID = ZTREQST+3(15).
        WHEN 'PU'.
          OBJECTCLASS   =   'ZIPUDOC'.
          OBJEKTID = ZTREQST+3(15).
        WHEN 'TT'.
          OBJECTCLASS   =   'ZITTDOC'.
          OBJEKTID = ZTREQST+3(10).
        WHEN 'DA' OR 'DP'.
          OBJECTCLASS   =   'ZIOTDOC'.
          OBJEKTID = ZTREQST+3(15).
        WHEN OTHERS.               EXIT.
      ENDCASE.
* ¼öÀÔÀÇ·Ú AMEND
    WHEN 'ZIM11' OR 'ZIM12' OR 'ZIM13' OR 'ZIM15' OR 'ZIM17'.
      CASE ZTREQHD-ZFREQTY.
        WHEN 'LC'.
          OBJECTCLASS   =   'ZILCADOC'.
          OBJEKTID = ZTREQST+3(15).
        WHEN 'LO'.
          OBJECTCLASS   =   'ZILOADOC'.
          OBJEKTID = ZTREQST+3(15).
        WHEN 'PU'.
          OBJECTCLASS   =   'ZIPUDOC'.
          OBJEKTID = ZTREQST+3(15).
*           WHEN 'TT'.                 OBJECTCLASS   =   'ZITTADOC'.
*          OBJEKTID = ztreqst+3(15).
        WHEN 'DA' OR 'DP'.
          OBJECTCLASS   =   'ZIOTDOC'.
          OBJEKTID = ZTREQST+3(15).
        WHEN OTHERS.               EXIT.
      ENDCASE.
* º¸Çè ¹®?
    WHEN 'ZIM41' OR 'ZIM42' OR 'ZIM43' OR 'ZIM44' OR
         'ZIM45' OR 'ZIM46' OR 'ZIM47' OR 'ZIM48'.
      OBJECTCLASS   =   'ZIINDOC'.
      OBJEKTID      =   ZTINS+3(20).
* LOCAL OFFER SHEET ¹®?
    WHEN 'ZIML1' OR 'ZIML2' OR 'ZIML3' OR 'ZIML4'.
      OBJECTCLASS   =   'ZIOFDOC'.
      OBJEKTID      =   ZTOFF+3(10).
* ¼öÀÔºñ?
    WHEN 'ZIMC1'.
      W_CNT = 0.
      LOOP AT IT_ZSRECST WHERE ZFMARK EQ 'X'.
        W_CNT = W_CNT + 1.
        MOVE-CORRESPONDING  IT_ZSRECST  TO  ZTRECST.
      ENDLOOP.
      CASE W_CNT.
        WHEN 0.      MESSAGE S951.   EXIT.
        WHEN 1.
          OBJECTCLASS   =   'ZTRECST'.
          OBJEKTID      =   ZTRECST(18).
        WHEN OTHERS. MESSAGE S965.   EXIT.
      ENDCASE.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  SUBMIT  RCS00120 WITH  OBJEKT   =   OBJECTCLASS
                   WITH  OBJEKTID =   OBJEKTID
                   AND   RETURN.

ENDFORM.                    " P2000_HEADER_CHANGE_DOC

*&---------------------------------------------------------------------
*&      Form  P1000_READ_HEADER_TEXTS
*&---------------------------------------------------------------------
FORM P1000_READ_HEADER_TEXTS.
  DATA : L_TXID LIKE ZTIMIMG00-ADVBKID.
* Payment Type
  IF NOT ( ZTREQHD-ZFREQTY EQ 'LC' OR ZTREQHD-ZFREQTY EQ 'TT' ).
    EXIT.
  ENDIF.

  REFRESH : THEADTAB.
  CLEAR THEADTAB.
  IF ZTREQHD-ZFREQTY EQ 'LC'.
    L_TXID = ZTIMIMG00-ADVBKID.
  ELSEIF ZTREQHD-ZFREQTY EQ 'TT'.
    L_TXID = ZTIMIMG00-SDBKID.
  ENDIF.

  SELECT * FROM TTXID WHERE TDOBJECT EQ 'EKKO      '
                      AND   TDID     EQ L_TXID.
    CHECK TTXID-TDID(1) EQ EKKO-BSTYP.
    THEADTAB-TDOBJECT   = TTXID-TDOBJECT.
    THEADTAB-TDID       = TTXID-TDID.
    THEADTAB-TDSPRAS    = EKKO-SPRAS.
    THEADTAB-TDNAME(10) = EKKO-EBELN.
    MOVE-CORRESPONDING THEADTAB TO HTEXT.
    READ TABLE THEADTAB WITH KEY HTEXT BINARY SEARCH.
    MOVE-CORRESPONDING HTEXT TO THEADTAB.
    THEADTAB-NEU = 'X'.
    CASE SY-SUBRC.
      WHEN 0. MODIFY THEADTAB INDEX SY-TABIX.
      WHEN 4. INSERT THEADTAB INDEX SY-TABIX.
      WHEN 8. APPEND THEADTAB.
    ENDCASE.
  ENDSELECT.
* text Line
  LOOP AT THEADTAB WHERE TDOBJECT EQ 'EKKO'.
    THEADTAB-NEU = 'X'.
    MODIFY THEADTAB INDEX SY-TABIX.
  ENDLOOP.
*--- Loop ?er die Referenztexte --------------------------------------*
  LOOP AT THEADTAB.

    CLEAR HTEXT.
*   move-corresponding reftext to htext.
    MOVE-CORRESPONDING THEADTAB TO HTEXT.
    HTEXT-TDSPRAS       = EKKO-SPRAS.
    HTEXT-TDNAME(10)    = EKKO-EBELN.
*--- Initialisieren eines neuen Textes--------------------------------*
    CALL FUNCTION 'INIT_TEXT'
         EXPORTING
              ID       = HTEXT-TDID
              LANGUAGE = HTEXT-TDSPRAS
              NAME     = HTEXT-TDNAME
              OBJECT   = HTEXT-TDOBJECT
         IMPORTING
              HEADER   = THEAD
         TABLES
              LINES    = TLINETAB
         EXCEPTIONS
              ID       = 1
              LANGUAGE = 2
              NAME     = 3
              OBJECT   = 4.
    IF SY-SUBRC NE 0.
    ENDIF.
*--- Fortschreiben der Theadtab---------------------------------------*
    CLEAR HTEXT.
    MOVE-CORRESPONDING THEAD TO HTEXT.
    READ TABLE THEADTAB WITH KEY HTEXT BINARY SEARCH.
    MOVE-CORRESPONDING THEAD TO THEADTAB.
    CLEAR THEADTAB-NEU.
    CASE SY-SUBRC.
      WHEN 00.
        MODIFY THEADTAB INDEX SY-TABIX."sollte nich sein
      WHEN 04.
        INSERT THEADTAB INDEX SY-TABIX.
      WHEN 08.
        APPEND THEADTAB.
    ENDCASE.
*--- Schl?sel setzen f? Lesen Referenztext --------------------------*
    CLEAR HTEXT.
    MOVE-CORRESPONDING THEADTAB TO HTEXT.
*    move-corresponding reftext to htext.
*--- Lesen des Referenztextes-----------------------------------------*
    REFRESH TLINETAB.
    CALL FUNCTION 'READ_TEXT'
         EXPORTING
              ID        = HTEXT-TDID
              LANGUAGE  = HTEXT-TDSPRAS
              NAME      = HTEXT-TDNAME
              OBJECT    = HTEXT-TDOBJECT
         IMPORTING
              HEADER    = THEAD
         TABLES
              LINES     = TLINETAB
         EXCEPTIONS
              ID        = 1
              LANGUAGE  = 2
              NAME      = 3
              NOT_FOUND = 4
              OBJECT    = 5
              OTHERS    = 6.

    IF SY-SUBRC EQ 0.                                       "NE 4.
      CHECK THEAD NE SPACE.
* Document Type
      CASE ZTREQHD-ZFREQTY.
        WHEN 'LC'.
          IF THEAD-TDID EQ ZTIMIMG00-ADVBKID.   " ADVISING BANK
            LOOP AT  TLINETAB FROM 1 TO 2.
              IF SY-TABIX EQ 1.
                ZTMLCHD-ZFABNM = TLINETAB-TDLINE.
              ELSEIF SY-TABIX EQ 2.
                ZTMLCHD-ZFABBR = TLINETAB-TDLINE.
              ENDIF.
            ENDLOOP.
          ENDIF.
        WHEN 'LO'.
        WHEN 'PU'.
*-----------------------------------------------------------------------
        WHEN 'TT'.
          IF THEAD-TDID EQ ZTIMIMG00-SDBKID.   " ¼Û±ÝÀºÇà¸í,
            LOOP AT  TLINETAB FROM 1 TO 4.
              IF SY-TABIX EQ 1.
                ZTREQHD-ZFLTEX1 = TLINETAB-TDLINE.
              ELSEIF SY-TABIX EQ 2.
                ZTREQHD-ZFLTEX2 = TLINETAB-TDLINE.
              ELSEIF SY-TABIX EQ 3.
                ZTREQHD-ZFLTEX3 = TLINETAB-TDLINE.
              ELSEIF SY-TABIX EQ 4.
                ZTREQHD-ZFLTEX4 = TLINETAB-TDLINE.
              ENDIF.
            ENDLOOP.
          ENDIF.
*-----------------------------------------------------------------------
        WHEN 'DA'.
        WHEN 'DP'.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

  ENDLOOP.                             "?er reftext

ENDFORM.                    " P1000_READ_HEADER_TEXTS

*&---------------------------------------------------------------------
*&      Form  P2000_PAYORD_FIELD_MOVE
*&---------------------------------------------------------------------
FORM P2000_PAYORD_FIELD_MOVE.
* Benificiary Text Select
  PERFORM   P1000_GET_LFA1_SELECT     USING    ZTREQHD-ZFBENI
                                      CHANGING W_LFA1.

  MOVE : W_LFA1-NAME2        TO ZTTTHD-ZFBENI1,             " PULL NAME
         W_LFA1-STRAS        TO ZTTTHD-ZFBENI2,  " Benificiary
         W_LFA1-ORT01        TO ZTTTHD-ZFBENI3.  " Benificiary

  MOVE : ZTREQHD-ZFBENI      TO ZTTTHD-ZFBENI,
         '3'                 TO ZTTTHD-ZFSENDTY.

  IF ZTREQHD-ZFBACD EQ 'A'.
    ZTTTHD-ZFSEND1 = 'TELEGRAPHIC TRANSFER(After)'.
  ELSEIF ZTREQHD-ZFBACD EQ 'B'.
    ZTTTHD-ZFSEND1 = 'TELEGRAPHIC TRANSFER(Before)'.
  ENDIF.
  ZTTTHD-ZFBUSFUN = '2AM'.

* ÀÔ±Ý°ü·Ã ¼­·ù¹®¼­.
  REFRESH : IT_ZSTTSG5.
  IT_ZSTTSG5-ZFLSG5    =     '00010'.
  IT_ZSTTSG5-ZFDOCCD   =     '105'.

  IT_ZSTTSG5-ZFDOCNO   =     ZTREQHD-EBELN.
  IT_ZSTTSG5-ZFISSDT   =     EKKO-BEDAT.
  APPEND IT_ZSTTSG5.

  IF W_OK_CODE NE 'COPY'.
    MOVE: '9'                 TO ZTTTHD-ZFEDFN,
          '13'                TO ZTTTHD-ZFCFRG.
  ENDIF.

  MOVE : W_TOT_AMOUNT        TO ZTTTHD-ZFAMT,
         EKKO-WAERS          TO ZTTTHD-WAERS,
         ZTIMIMG01-ZFPREPAY  TO ZTREQHD-ZFPREPAY,
         ZTIMIMG01-ZFBACD    TO ZTREQHD-ZFBACD.

* CLEAR : LFBK.
* SELECT * FROM  LFBK UP TO 1 ROWS
*          WHERE LIFNR EQ ZTREQHD-ZFBENI.
*           AND   BANKS EQ 'KR'.
*    EXIT.
* ENDSELECT.

* TRANSLATE LFBK TO UPPER CASE.

ENDFORM.                    " P2000_PAYORD_FIELD_MOVE

*&---------------------------------------------------------------------
*&      Form  P3000_PAYORD_DOC_MOVE
*&---------------------------------------------------------------------
*
FORM P3000_PAYORD_DOC_MOVE.
* ¼öÀÔÃßÃµ ÀÇ?
*  PERFORM  P2000_IL_DATA_MOVE.
ENDFORM.                    " P3000_PAYORD_DOC_MOVE

*&---------------------------------------------------------------------
*&      Form  P2000_PAYORD_DOC_CHECK
*&---------------------------------------------------------------------
*
FORM P2000_PAYORD_DOC_CHECK.

  ZTREQST-ZFEDICK = 'X'.
*-----------------------------------------------------------------------
* 2000/03/10 Ç°¸ñ Ã¼?
*-----------------------------------------------------------------------
  LOOP AT IT_ZSREQIT.
    PERFORM   P2000_REQ_ITEM_CHECK   TABLES  IT_ZSREQIT
                                     USING   'E'.
  ENDLOOP.
*-----------------------------------------------------------------------
* ÃÖÁ¾ °³¼³±Ý¾× ÀÚµ¿ °»?
*-----------------------------------------------------------------------
*  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'E'.
  PERFORM   P2000_USD_CONVERT_AMOUNT   USING   'I'.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTREQHD-BUKRS.
  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    ZTREQST-ZFEDICK = 'O'.
* ÀÔ±Ý°ü·Ã ¼­·ù¹ø?
    DESCRIBE TABLE IT_ZSTTSG5 LINES W_LINE.
    IF W_LINE LE 0.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Related deposit Doc No'.  EXIT.
    ENDIF.
* ½ÅÃ»ÀÏ?
    IF ZTREQST-ZFAPPDT IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Application date'.  EXIT.
    ENDIF.
* Áö±ÞÁö¼­¼­ ¿ë?
    IF ZTTTHD-ZFBUSFUN IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'use of Pay order'. EXIT.
    ELSE.
      IF ZTTTHD-ZFBUSFUN EQ '2AJ' AND ZTTTHD-ZFCOMMTY IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I167 WITH 'Payment commission type'.   EXIT.
     ELSEIF ZTTTHD-ZFBUSFUN NE '2AJ' AND NOT ZTTTHD-ZFCOMMTY IS INITIAL
              .
        ZTREQST-ZFEDICK = 'X'.
        MESSAGE I215.   EXIT.
      ENDIF.
    ENDIF.
* ºÎ°¡¼ö¼ö·á ´ã´ç?
    IF ZTTTHD-ZFCFRG IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Additional commission payer'. EXIT.
    ENDIF.
* Áö±ÞÀÇ·Ú?
    IF ZTTTHD-ZFAPPNM IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Payment requester'. EXIT.
    ENDIF.
    ASSIGN ZTTTHD-ZFAPPNM         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFAPPAD1        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFAPPAD2        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFAPPAD3        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFTELNO         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* ¼öÀÍ?
    IF ZTTTHD-ZFBENI1 IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Beneficiary name'. EXIT.
    ENDIF.
    ASSIGN ZTTTHD-ZFBENI1         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFBENI2         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFBENI3         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* Áö±ÞÀÇ·ÚÀÎ Àº?
    IF ZTTTHD-ZFOBNM IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Payment requester bank'. EXIT.
    ENDIF.
    IF ZTTTHD-ZFOBAK IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Payment request account No'. EXIT.
    ENDIF.
    ASSIGN ZTTTHD-ZFOBNM          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFOBAK          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFOBBR          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFOPBNCD        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

* ¼öÀÍÀÚ Àº?
    IF ZTTTHD-ZFBENM IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Beneficiary bank'. EXIT.
    ENDIF.
    IF ZTTTHD-ZFOBAK1 IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Beneficiary account No'. EXIT.
    ENDIF.
    ASSIGN ZTTTHD-ZFOBNM          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFOBAK1         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFBEBR          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFBENCD         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* ÀüÀÚ¼­?
    IF ZTTTHD-ZFELENM IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Electronic signature firm name'. EXIT.
    ENDIF.
    IF ZTTTHD-ZFELEID IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.
      MESSAGE I167 WITH 'Electronic signature'. EXIT.
    ENDIF.
    ASSIGN ZTTTHD-ZFELENM         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFREPRE         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFELEID         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* ¼Û±Ý¾Ö?
    ASSIGN ZTTTHD-ZFSEND1         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFSEND2         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFSEND3         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFSEND4         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFSEND5         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* ±âÅ¸Á¤?
    ASSIGN ZTTTHD-ZFETC1          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFETC2          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFETC3          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFETC4          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTTTHD-ZFETC5          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* ±âÅ¸ ¹ø?
    ASSIGN ZTTTHD-ZFETCNO         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
ENDFORM.                    " P2000_PAYORD_DOC_CHECK

*&---------------------------------------------------------------------
*&      Form  P3000_PAYORD_DOC_MODIFY
*&---------------------------------------------------------------------
FORM P3000_PAYORD_DOC_MODIFY.

  CALL FUNCTION 'ZIM_PAYORD_DOC_MODIFY'
       EXPORTING
            W_OK_CODE       = W_OK_CODE
            ZFREQNO         = ZTREQHD-ZFREQNO
            ZFAMDNO         = ZTREQST-ZFAMDNO
            ZFSTATUS        = W_STATUS
            W_ZTREQHD       = ZTREQHD
            W_ZTREQHD_OLD   = *ZTREQHD
            W_ZTREQST       = ZTREQST
            W_ZTREQST_OLD   = *ZTREQST
*            W_ZTTTHD        = ZTTTHD
*            W_ZTTTHD_OLD    = *ZTTTHD
       TABLES
*            IT_ZSTTSG5      = IT_ZSTTSG5
            IT_ZSREQIT      = IT_ZSREQIT
            IT_ZSREQIT_OLD  = IT_ZSREQIT_OLD
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_OLD = IT_ZTREQORJ_OLD
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_OLD  = IT_ZSREQIL_OLD
       EXCEPTIONS
            ERROR_UPDATE.

  IF SY-SUBRC NE  0.
    MESSAGE  E017.
  ENDIF.


ENDFORM.                    " P3000_PAYORD_DOC_MODIFY
*&---------------------------------------------------------------------
*
*&      Form  P2000_IT_ZSTTSG5_UPDATE
*&---------------------------------------------------------------------
*
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

*&---------------------------------------------------------------------
*
*&      Form  P1000_READ_PAYORD_DOC
*&---------------------------------------------------------------------
*
FORM P1000_READ_PAYORD_DOC.

*  CALL FUNCTION 'ZIM_GET_PAYORD_DOC_DATA'
*       EXPORTING
*            ZFREQNO        = ZTREQHD-ZFREQNO
*       IMPORTING
*            W_ZTTTHD       = ZTTTHD
*       TABLES
*            IT_ZSTTSG5     = IT_ZSTTSG5
*            IT_ZSTTSG5_ORG = IT_ZSTTSG5_ORG
*       EXCEPTIONS
*            NOT_FOUND      = 4
*            NOT_INPUT      = 8.
*
*  CASE SY-SUBRC.
*    WHEN 4.
*      MESSAGE E054 WITH ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO.
*    WHEN 8.
*      MESSAGE E019.
*  ENDCASE.

*-----------------------------------------------------------------------
* º¯°æ³»¿ª È®ÀÎÀ» À§?
*-----------------------------------------------------------------------
   *ZTTTHD   = ZTTTHD.

*>> H/S CODE ±âÅ¸ Á¤º¸¿¡ SET!
  LOOP  AT  IT_ZSREQIT.
    IF SY-TABIX EQ 1.
      MOVE  IT_ZSREQIT-STAWN  TO  ZTTTHD-ZFETC1.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P1000_READ_PAYORD_DOC
*&---------------------------------------------------------------------
*&      Form  P3000_TT_DOC_FLAT_CREATE
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
FORM P3000_TT_DOC_FLAT_CREATE.
  W_ZFCDDOC = 'PAYORD'.
  W_ZFDHSRO = W_LFA1-BAHNS.        " ½Äº°?
  W_ZFDHREF = ZTREQHD-ZFREQNO.     " ÂüÁ¶¹ø?
*  W_ZFDHDDB = ZTREQST-EKORG.       " ºÎ?
  W_ZFDHENO = ZTREQST-ZFDOCNO.     " ¹®¼­¹ø?

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
            W_ZFCDDOC = W_ZFCDDOC
            W_ZFDHSRO = W_ZFDHSRO
            W_ZFDHREF = W_ZFDHREF
*            W_ZFDHDDB = W_ZFDHDDB
            W_BUKRS     = ZTREQHD-BUKRS
       CHANGING
            W_ZFDHENO = W_ZFDHENO
       EXCEPTIONS
            DB_ERROR  = 4
            NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
*  CALL FUNCTION 'ZIM_PAYORD_EDI_SEND'
*       EXPORTING
*             W_ZFREQNO    =    ZTREQST-ZFREQNO
*             W_ZFAMDNO    =    ZTREQST-ZFAMDNO
*             W_ZFDHENO    =    W_ZFDHENO
*       EXCEPTIONS
*             DB_ERROR     =   4.

*  CASE SY-SUBRC.
*    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
*    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
*  ENDCASE.


ENDFORM.                    " P3000_TT_DOC_FLAT_CREATE

*&---------------------------------------------------------------------
*&      Form  P2000_TEMP_AREA_MOVE
*&---------------------------------------------------------------------
FORM P2000_TEMP_AREA_MOVE.
  MOVE-CORRESPONDING ZTREQHD TO ZTREQHD_TMP.
  ZTREQHD_TMP-ZFAMDNO = ZTREQST-ZFAMDNO.
  REFRESH : IT_ZSREQIT_TMP.
  LOOP AT IT_ZSREQIT.
    MOVE-CORRESPONDING IT_ZSREQIT TO IT_ZSREQIT_TMP.
    IT_ZSREQIT_TMP-ZFAMDNO = ZTREQST-ZFAMDNO.
    APPEND IT_ZSREQIT_TMP.
  ENDLOOP.
  REFRESH : IT_ZSREQIL_TMP.
  LOOP AT IT_ZSREQIL.
    MOVE-CORRESPONDING IT_ZSREQIL TO IT_ZSREQIL_TMP.
*           IT_ZSREQIL_TMP = IT_ZSREQIL.
    IT_ZSREQIL_TMP-ZFAMDNO = ZTREQST-ZFAMDNO.
    APPEND IT_ZSREQIL_TMP.
  ENDLOOP.
  REFRESH : IT_ZTREQORJ_TMP.
  LOOP AT IT_ZTREQORJ.
    MOVE-CORRESPONDING IT_ZTREQORJ TO IT_ZTREQORJ_TMP.
*           IT_ZTREQORJ_TMP = IT_ZTREQORJ.
    IT_ZTREQORJ_TMP-ZFAMDNO = ZTREQST-ZFAMDNO.
    APPEND IT_ZTREQORJ_TMP.
  ENDLOOP.

ENDFORM.                    " P2000_TEMP_AREA_MOVE

*&---------------------------------------------------------------------*
*&      Module  GET_TRME_TEXT_SCR2102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_TRME_TEXT_SCR2102 INPUT.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

* ¿î¼ÛÀ¯Çü TEXT
  PERFORM   GET_DD07T_SELECT USING      'ZDTRME'  ZTOFF-ZFTRME
                             CHANGING   W_ZFTRME_TEXT.

ENDMODULE.                 " GET_TRME_TEXT_SCR2102  INPUT

*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
FORM GET_DD07T_SELECT USING    P_DOMNAME
                               P_FIELD
                      CHANGING P_W_NAME.
  CLEAR : DD07T, P_W_NAME.
  IF P_FIELD IS INITIAL.   EXIT.   ENDIF.

  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ P_FIELD
                      ORDER BY AS4VERS DESCENDING.
    EXIT.
  ENDSELECT.
*   TRANSLATE DD07T-DDTEXT TO UPPER CASE.
  P_W_NAME   = DD07T-DDTEXT.
  TRANSLATE P_W_NAME TO UPPER CASE.
ENDFORM.                    " GET_DD07T_SELECT

*&---------------------------------------------------------------------*
*&      Form  P3000_FLAT_DATA_DELETE
*&---------------------------------------------------------------------*
FORM P3000_FLAT_DATA_DELETE USING    P_ZFDOCNO.
* FLAT HEADER TABLE
  DELETE FROM ZTDHF1 WHERE ZFDHENO EQ P_ZFDOCNO.
* FLAT ITEM TABLE
  DELETE FROM ZTDDF1 WHERE ZFDDENO EQ P_ZFDOCNO.
ENDFORM.                    " P3000_FLAT_DATA_DELETE

*&---------------------------------------------------------------------*
*&      Form  P2000_ZSLLCAMSGOF_UPDATE
*&---------------------------------------------------------------------*
FORM P2000_ZSLLCAMSGOF_UPDATE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* ¹ÌÀÔ·Â »è?
  LOOP AT IT_ZSLLCAMSGOF  WHERE ZFSGOF EQ SPACE.
    DELETE IT_ZSLLCAMSGOF INDEX SY-TABIX.
  ENDLOOP.

* INDEX ¼öÁ¤ ÀÛ?
  LOOP AT IT_ZSLLCAMSGOF.
    IT_ZSLLCAMSGOF-ZFLSGOF  =   SY-TABIX * 10.
    MODIFY IT_ZSLLCAMSGOF.
  ENDLOOP.

ENDFORM.                    " P2000_ZSLLCAMSGOF_UPDATE

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

  LOOP AT IT_ZSINSSG2  WHERE ZFLSG2  EQ SPACE.
    DELETE IT_ZSINSSG2  INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSINSSG2.
    IT_ZSINSSG2-ZFLSG2 = SY-TABIX * 10.
    IT_ZSINSSG2-ZFAMDNO = ZTINS-ZFAMDNO.
    MODIFY IT_ZSINSSG2  INDEX SY-TABIX.
  ENDLOOP.

  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR
     SY-TCODE EQ 'ZIM47' OR SY-TCODE EQ 'ZIM48'.
    DESCRIBE TABLE IT_ZSINSSG2     LINES  W_COUNTER.
    DESCRIBE TABLE IT_ZSINSSG2_OLD LINES  W_COUNTER1.
    CLEAR :  ZTINS-ZFGDYN.

    IF W_COUNTER NE W_COUNTER1.
      ZTINS-ZFGDYN = 'X'.
    ELSE.
      LOOP AT IT_ZSINSSG2_OLD.
        READ TABLE IT_ZSINSSG2 WITH KEY
                               ZFLSG2 = IT_ZSINSSG2_OLD-ZFLSG2.
        IF SY-SUBRC NE 0.
          ZTINS-ZFGDYN = 'X'. EXIT.
        ELSE.
          IF IT_ZSINSSG2_OLD-ZFDSOG1 NE IT_ZSINSSG2-ZFDSOG1 OR
             IT_ZSINSSG2_OLD-ZFDSOG2 NE IT_ZSINSSG2-ZFDSOG2 OR
             IT_ZSINSSG2_OLD-ZFDSOG3 NE IT_ZSINSSG2-ZFDSOG3 OR
             IT_ZSINSSG2_OLD-ZFDSOG4 NE IT_ZSINSSG2-ZFDSOG4 OR
             IT_ZSINSSG2_OLD-ZFDSOG5 NE IT_ZSINSSG2-ZFDSOG5 OR
             IT_ZSINSSG2_OLD-ZFPKCN  NE IT_ZSINSSG2-ZFPKCN  OR
             IT_ZSINSSG2_OLD-ZFPKCNM NE IT_ZSINSSG2-ZFPKCNM.
            ZTINS-ZFGDYN = 'X'. EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_IT_ZSINSSG2_UPDATE

*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSINSAGR_UPDATE
*&---------------------------------------------------------------------*
FORM P2000_IT_ZSINSAGR_UPDATE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSINSAGR  WHERE ZFLAGR  EQ SPACE.
*                      AND   LOEKZ   EQ 'X'.
    DELETE IT_ZSINSAGR  INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSINSAGR.
    IT_ZSINSAGR-ZFLAGR  = SY-TABIX * 10.
    IT_ZSINSAGR-ZFAMDNO = ZTINS-ZFAMDNO.
    MODIFY IT_ZSINSAGR  INDEX SY-TABIX.
  ENDLOOP.

  IF SY-DYNNR EQ '4505'.
    DESCRIBE TABLE IT_ZSINSAGR     LINES  W_COUNTER.
    DESCRIBE TABLE IT_ZSINSAGR_OLD LINES  W_COUNTER1.
    CLEAR : ZTINS-ZFCDYN.

    IF W_COUNTER NE W_COUNTER1.
      ZTINS-ZFCDYN = 'X'.
    ELSE.
      LOOP AT IT_ZSINSAGR_OLD.
        READ TABLE IT_ZSINSAGR WITH KEY
                               ZFLAGR = IT_ZSINSAGR_OLD-ZFLAGR.
        IF SY-SUBRC NE 0.
          ZTINS-ZFCDYN = 'X'. EXIT.
        ELSE.
          IF IT_ZSINSAGR_OLD-ZFBCNYN  NE IT_ZSINSAGR-ZFBCNYN OR
             IT_ZSINSAGR_OLD-ZFINSCD  NE IT_ZSINSAGR-ZFINSCD OR
*             IT_ZSINSAGR_OLD-ZFCNCD   NE IT_ZSINSAGR-ZFCNCD  OR
             IT_ZSINSAGR_OLD-ZFCNCDNM NE IT_ZSINSAGR-ZFCNCDNM.
            ZTINS-ZFCDYN = 'X'. EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_IT_ZSINSAGR_UPDATE

*&---------------------------------------------------------------------*
*&      Form  P2000_IT_ZSINSSG5_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IT_ZSINSSG5_UPDATE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSINSSG5  WHERE ZFLSG5  EQ SPACE.
*                      AND   LOEKZ   EQ 'X'.
    DELETE IT_ZSINSSG5  INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSINSSG5.
    IT_ZSINSSG5-ZFLSG5 = SY-TABIX * 10.
    MODIFY IT_ZSINSSG5  INDEX SY-TABIX.
  ENDLOOP.


ENDFORM.                    " P2000_IT_ZSINSSG5_UPDATE

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_HEADERL_CHARGE
*&---------------------------------------------------------------------*
FORM P1000_READ_HEADER_CHARGE USING    P_KNUMV
                                       P_EBELN.

  CALL FUNCTION 'ZIM_GET_KNOV_CHARGE'
       EXPORTING
            KNUMV       = P_KNUMV
            EBELN       = P_EBELN
            W_AMOUNT    = W_TOT_AMOUNT
       IMPORTING
            W_KBETR1    = ZTREQHD-ZFPKCHG
            W_WAERS1    = ZTREQHD-ZFPKCUR
            W_KBETR2    = ZTREQHD-ZFHDCHG
            W_WAERS2    = ZTREQHD-ZFHDCUR
            W_PO_AMOUNT = W_PO_AMOUNT.

ENDFORM.                    " P1000_READ_HEADER_CHARGE

*&---------------------------------------------------------------------*
*&      Form  P2000_GET_ZTIMIMG08_DESC
*&---------------------------------------------------------------------*
FORM P2000_GET_ZTIMIMG08_DESC USING    P_TYPE
                                       P_CODE
                              CHANGING P_TEXT.

  CALL FUNCTION 'ZIM_GET_ZTIMIMG08_RECORD'
       EXPORTING
            W_ZFCDTY    = P_TYPE
            W_ZFCD      = P_CODE
       IMPORTING
            W_ZTIMIMG08 = W_ZTIMIMG08
       EXCEPTIONS
            NOT_FOUND   = 4
            NOT_INPUT   = 8.

  CASE SY-SUBRC.
    WHEN 4.    MESSAGE E112 WITH    ZSRECST-ZFCSCD.
    WHEN 8.    MESSAGE E167 WITH    'ÄÚµå'.
  ENDCASE.

  MOVE : W_ZTIMIMG08-ZFCDNM   TO   P_TEXT.

ENDFORM.                    " P2000_GET_ZTIMIMG08_DESC

*&---------------------------------------------------------------------*
*&      Form  P2000_ZTRECST_LOCK_MODE
*&---------------------------------------------------------------------*
FORM P2000_ZTRECST_LOCK_MODE USING    PA_MODE.
  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTRECST'
         EXPORTING
              MANDT   = SY-MANDT
              ZFREQNO = ZTREQHD-ZFREQNO
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC NE 0.
      MESSAGE E510 WITH SY-MSGV1 'L/C Cost'
                        ZTREQHD-ZFREQNO SPACE
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTRECST'
         EXPORTING
              MANDT   = SY-MANDT
              ZFREQNO = ZTREQHD-ZFREQNO.
  ENDIF.


ENDFORM.                    " P2000_ZTRECST_LOCK_MODE

*&---------------------------------------------------------------------*
*&      Form  P2000_COST_MODIFY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_COST_MODIFY_CHECK USING    W_GUBUN.

  W_GUBUN = 'Y'.

  DESCRIBE TABLE IT_ZSRECST         LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSRECST_ORG     LINES W_COUNTER1.
  W_LOOP_CNT = 0.

  IF W_STATUS EQ 'I'.                                       " New Entry
    IF W_COUNTER EQ 0.
      W_GUBUN = 'N'.
    ENDIF.
  ELSE.                  " Change Mode
    IF W_COUNTER EQ W_COUNTER1.
      LOOP AT IT_ZSRECST_ORG.
        READ TABLE IT_ZSRECST WITH KEY
                              ZFCSQ = IT_ZSRECST_ORG-ZFCSQ
                              BINARY SEARCH.
        IF IT_ZSRECST_ORG NE IT_ZSRECST.
          W_LOOP_CNT = 1.    EXIT.
        ENDIF.
      ENDLOOP.
      IF W_LOOP_CNT EQ 0.
        W_GUBUN = 'N'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_COST_MODIFY_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTRECST_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTRECST_CHANGE_DOC USING    UPD_CHNGIND.

  OBJECID = ZTRECST(18).

  CALL FUNCTION 'ZTRECST_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTRECST          =    ZTRECST
           O_ZTRECST          =   *ZTRECST
           UPD_ZTRECST        =    UPD_CHNGIND
*            CDOC_UPD_OBJECT    =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTRECST      = ''
     TABLES
           ICDTXT_ZTRECST     =    IT_CDTXT.

ENDFORM.                    " P3000_ZTRECST_CHANGE_DOC

*&---------------------------------------------------------------------*
*&      Form  P2000_CONV_OTHER_CURR_TO_USD
*&---------------------------------------------------------------------*
FORM P2000_CONV_OTHER_CURR_TO_USD USING    W_TOT_AMOUNT
                                           ZTREQHD_WAERS
                                  CHANGING W_LOCAL_AMT.

  DATA : L_TEXT_EXRATE(255)    TYPE C,
         L_FOREIGN_FACTOR(255) TYPE C,
         L_FACTOR              TYPE P,
         L_FIXED_RATE          LIKE TCURS-SPRED,
         L_LOCAL_AMT           LIKE ZTREQHD-ZFLASTAM.  " USD È¯»ê Amount

  IF ZTREQHD-WAERS EQ W_KRW.
    ZTREQHD-KURSF = 1.
  ELSE.
    CHECK : SY-TCODE(4) EQ 'ZIM0' OR
            SY-TCODE(4) EQ 'ZIM1'.

*    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
*         EXPORTING
*              DATE             = SY-DATUM
*              FOREIGN_AMOUNT   = 0
*              FOREIGN_CURRENCY = ZTREQHD_WAERS
*              LOCAL_CURRENCY   = W_KRW
*         IMPORTING
*              FOREIGN_FACTOR   = L_FOREIGN_FACTOR
*              EXCHANGE_RATE    = L_TEXT_EXRATE
*              LOCAL_AMOUNT     = L_LOCAL_AMT
*              FIXED_RATE       = L_FIXED_RATE
*         EXCEPTIONS
*              OTHERS           = 01.

    CALL FUNCTION 'MS_CONVERT_TO_OTHER_CURRENCY'
      EXPORTING
        DATE                   = SY-DATUM
        FROM_CURRENCY          = W_KRW
        FROM_AMOUNT            = W_LOCAL_AMT
        TO_CURRENCY            = W_USD
        COMPANY_CURRENCY       = W_KRW
      IMPORTING
        TO_AMOUNT              = W_LOCAL_AMT.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                 RAISING AMOUNT_CALCULATION.
    ENDIF.

    PERFORM    P2000_WRITE_NO_MASK(SAPMZIM01)
                                   CHANGING L_TEXT_EXRATE.
    PERFORM    P2000_WRITE_NO_MASK(SAPMZIM01)
                                   CHANGING L_FOREIGN_FACTOR.
    W_EXRATE = L_TEXT_EXRATE.
    L_FACTOR = L_FOREIGN_FACTOR.
    IF W_EXRATE LT 0.
      W_EXRATE = W_EXRATE * -1.
    ENDIF.
    IF ZTREQHD-KURSF IS INITIAL.
      ZTREQHD-KURSF = W_EXRATE.
    ENDIF.
    IF ZTREQHD-FFACT IS INITIAL.
      IF L_FACTOR IS INITIAL.
        ZTREQHD-FFACT = 1.
      ELSE.
        ZTREQHD-FFACT = L_FACTOR.
      ENDIF.
    ENDIF.
  ENDIF.

  IF L_FACTOR NE 0.
    W_EXRATE = W_EXRATE / L_FACTOR.
  ENDIF.

*>> USD È¯»ê±Ý¾×À¸·Î °è»ê.
  IF ZTREQHD-WAERS EQ W_USD.
    W_LOCAL_AMT = W_TOT_AMOUNT.
  ELSE.
    IF ZTREQHD-WAERS EQ W_KRW.
      W_LOCAL_AMT = W_TOT_AMOUNT.
    ELSE.

      PERFORM    P2000_CONVERT_TO_USD_CURR    USING W_TOT_AMOUNT
                                                    ZTREQHD-WAERS
                                                    W_KRW
                                           CHANGING W_LOCAL_AMT
                                                    W_RATE
                                                    W_FIXED_RATE.
    ENDIF.

    PERFORM    P2000_CONVERT_TO_USD_CURR    USING W_LOCAL_AMT
                                                  W_KRW
                                                  W_USD
                                         CHANGING W_LOCAL_AMT
                                                  W_RATE
                                                  W_FIXED_RATE.
  ENDIF.

ENDFORM.                    " P2000_CONV_OTHER_CURR_TO_USD

*&---------------------------------------------------------------------*
*&      Form  P2000_CHANGE_ITEM
*&---------------------------------------------------------------------*
FORM P2000_CHANGE_ITEM.
  SPOP-TITEL = 'Display: Document Status'.
  CANCEL_OPTION = 'Y'.
*     CLEAR : CANCEL_OPTION.
  OPTION = 1.
  TEXTLEN = 40.

  PERFORM   P1000_AMEND_LIST_MAKE.
  DESCRIBE TABLE IT_ZSAMDLIST LINES W_COUNT.
  IF W_COUNT EQ 0.  MESSAGE I249.  EXIT.    ENDIF.


  CALL SCREEN 1199 STARTING AT 10 3
                   ENDING   AT 95 17.

ENDFORM.                    " P2000_CHANGE_ITEM

*&---------------------------------------------------------------------*
*&      Form  P3000_ZSPURSG4_INDEX_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_ZSPURSG4_INDEX_UPDATE.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  LOOP AT IT_ZSPURSG4.
    W_TABIX = SY-TABIX.
    IT_ZSPURSG4-ZFLSG4 = W_TABIX * 10.
    MODIFY IT_ZSPURSG4 INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P3000_ZSPURSG4_INDEX_UPDATE

*&---------------------------------------------------------------------*
*&      Form  P2000_REFRESH_ADDRESS
*&---------------------------------------------------------------------*
FORM P2000_REFRESH_ADDRESS USING    P_VENDOR.
  DATA : L_TEXT(18).
* Benificiary
  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFBENI
                              CHANGING   W_ZFBENI_NM.
  CASE ZTREQHD-ZFREQTY.
    WHEN 'PU'.
      MOVE : ZTREQHD-ZFBENI     TO ZTPUR-ZFBENI.
*      SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ ZTPUR-ZFBENI.
      IF ZTPUR-ZFVENNM1   IS INITIAL.
        MOVE : W_LFA1-NAME1    TO   ZTPUR-ZFVENNM1,         " PULL NAME
               W_LFA1-NAME2    TO   ZTPUR-ZFVENNM2,
               W_LFA1-BAHNS    TO   ZTPUR-ZFVENID.
      ENDIF.
      IF ZTPUR-ZFVENAD1   IS INITIAL.
        MOVE : W_LFA1-NAME3    TO   ZTPUR-ZFVENAD1,         " PULL NAME
               W_LFA1-NAME4    TO   ZTPUR-ZFVENAD2,         " PULL NAME
               SPACE           TO   ZTPUR-ZFVENAD3.         " PULL NAME
      ENDIF.
      IF SY-UCOMM EQ 'CADD'.    " ADDRESS UPDATE.
        MOVE : W_LFA1-NAME1    TO   ZTPUR-ZFVENNM1,         " PULL NAME
               W_LFA1-NAME2    TO   ZTPUR-ZFVENNM2,
               W_LFA1-BAHNS    TO   ZTPUR-ZFVENID.

        MOVE : W_LFA1-NAME3    TO   ZTPUR-ZFVENAD1,         " PULL NAME
               W_LFA1-NAME4    TO   ZTPUR-ZFVENAD2,         " PULL NAME
               SPACE           TO   ZTPUR-ZFVENAD3.         " PULL NAME
      ENDIF.
    WHEN 'LC'.

      MOVE : ZTREQHD-ZFBENI     TO ZTMLCHD-ZFBENI.
      IF ZTMLCSG2-ZFBENI1 IS INITIAL.
        CLEAR : LFBK.
        SELECT * FROM  LFBK UP TO 1 ROWS
                 WHERE LIFNR EQ ZTREQHD-ZFBENI.
*                   AND   BANKS EQ 'KR'.
          EXIT.
        ENDSELECT.
*        TRANSLATE LFBK TO UPPER CASE.
        L_TEXT = LFBK-BANKN.
        TRANSLATE L_TEXT TO UPPER CASE.
        MOVE : W_LFA1-NAME1    TO   ZTMLCSG2-ZFBENI1,       " PULL NAME
               W_LFA1-NAME2    TO   ZTMLCSG2-ZFBENI2,
               W_LFA1-NAME3    TO   ZTMLCSG2-ZFBENI3,
               W_LFA1-NAME4    TO   ZTMLCSG2-ZFBENI4,
               L_TEXT          TO   ZTMLCSG2-ZFBENIA.   " ACCOUNT NO.
      ENDIF.
      IF SY-UCOMM EQ 'CADD'.    " ADDRESS UPDATE.
        CLEAR : LFBK.
        SELECT * FROM  LFBK UP TO 1 ROWS
                 WHERE LIFNR EQ ZTREQHD-ZFBENI.
        ENDSELECT.
        MOVE : W_LFA1-NAME1    TO   ZTMLCSG2-ZFBENI1,       " PULL NAME
               W_LFA1-NAME2    TO   ZTMLCSG2-ZFBENI2,
               W_LFA1-NAME3    TO   ZTMLCSG2-ZFBENI3,
               W_LFA1-NAME4    TO   ZTMLCSG2-ZFBENI4,
               LFBK-BANKN      TO   ZTMLCSG2-ZFBENIA.   " ACCOUNT NO.
      ENDIF.
    WHEN 'LO'.
      MOVE : ZTREQHD-ZFBENI     TO ZTLLCHD-ZFBENI.
      IF ZTLLCSG23-ZFBENI1 IS INITIAL.

        MOVE : W_LFA1-NAME1       TO ZTLLCSG23-ZFBENI1,     " PULL NAME
               W_LFA1-J_1KFREPRE  TO ZTLLCSG23-ZFBENI2,
               W_LFA1-NAME2       TO ZTLLCSG23-ZFBENI3.
      ENDIF.
      IF SY-UCOMM EQ 'CADD'.    " ADDRESS UPDATE.
        MOVE : W_LFA1-NAME1       TO ZTLLCSG23-ZFBENI1,     " PULL NAME
               W_LFA1-J_1KFREPRE  TO ZTLLCSG23-ZFBENI2,
               W_LFA1-NAME2       TO ZTLLCSG23-ZFBENI3.
      ENDIF.
    WHEN 'TT'.
      MOVE : ZTREQHD-ZFBENI     TO ZTTTHD-ZFBENI.
      IF ZTTTHD-ZFBENI1 IS INITIAL.
        MOVE : W_LFA1-NAME1        TO ZTTTHD-ZFBENI1,       " PULL NAME
               W_LFA1-NAME2        TO ZTTTHD-ZFBENI2,  " Benificiary
               W_LFA1-NAME3        TO ZTTTHD-ZFBENI3.  " Benificiary
      ENDIF.
      IF SY-UCOMM EQ 'CADD'.    " ADDRESS UPDATE.
        MOVE : W_LFA1-NAME1        TO ZTTTHD-ZFBENI1,       " PULL NAME
               W_LFA1-NAME2        TO ZTTTHD-ZFBENI2,  " Benificiary
               W_LFA1-NAME3        TO ZTTTHD-ZFBENI3.  " Benificiary
      ENDIF.
  ENDCASE.


ENDFORM.                    " P2000_REFRESH_ADDRESS

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0104  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0104 INPUT.

  IF SY-UCOMM EQ 'CADD'.    " ADDRESS UPDATE.
    PERFORM  P1000_SET_OPEN_BANK.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR0104  INPUT

*&---------------------------------------------------------------------*
*&      Form  P1000_SET_OPEN_BANK
*&---------------------------------------------------------------------*
FORM P1000_SET_OPEN_BANK.

  PERFORM  P1000_GET_BANK   USING      ZTREQHD-ZFOPBN
                            CHANGING   W_LFA1.
  W_OPEN_NM = W_LFA1-NAME1.

*  PERFORM  P1000_GET_VENDOR   USING      ZTREQHD-ZFOPBN
*                              CHANGING   W_OPEN_NM.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC' OR 'LO' OR 'PU' OR 'TT' OR 'DA' OR 'DP' OR 'GS'.
      IF ZTREQHD-ZFOPBN IS INITIAL.
        MESSAGE E167 WITH 'Opening bank'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  PERFORM  P2000_BANK_TEXT_MOVE.


ENDFORM.                    " P1000_SET_OPEN_BANK

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_BANK
*&---------------------------------------------------------------------*
*      -->P_ZTREQHD_ZFOPBN  text
*      <--P_W_LFA1  text
*----------------------------------------------------------------------*
FORM P1000_GET_BANK USING    P_LIFNR
                    CHANGING W_LFA1    STRUCTURE   LFA1.
  DATA : L_NAME1(255),
         L_NAME2(255),
         L_NAME3(255),
         L_NAME4(255).

  CLEAR : W_LFA1.
  IF P_LIFNR IS INITIAL.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*----------------------------------------------------------------------
*  CALL FUNCTION 'READ_LFA1'
*       EXPORTING
*             XLIFNR          = P_LIFNR
*       IMPORTING
*             XLFA1           = W_LFA1
*       EXCEPTIONS
*             KEY_INCOMPLETE  = 01
*             NOT_AUTHORIZED  = 02
*             NOT_FOUND       = 03.
*
*  CASE SY-SUBRC.
*     WHEN 01.     MESSAGE I025.
*     WHEN 02.     MESSAGE E950.
*     WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
*  ENDCASE.
*  TRANSLATE W_LFA1 TO UPPER CASE.
*  MOVE: W_LFA1-NAME1   TO   P_NAME1.

  CLEAR : W_LFA1, W_ADRC.

  CALL FUNCTION 'ZIM_GET_VENDOR_ADDRESS_FORMAT'
       EXPORTING
            LIFNR     = P_LIFNR
       IMPORTING
            NAME1     = L_NAME1
            NAME2     = L_NAME2
            NAME3     = L_NAME3
            NAME4     = L_NAME4
            P_LFA1    = W_LFA1
            P_ADRC    = W_ADRC
       EXCEPTIONS
            NO_INPUT  = 01
            NOT_FOUND = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
  ENDCASE.

  TRANSLATE : L_NAME1 TO UPPER CASE,
              L_NAME2 TO UPPER CASE,
              L_NAME3 TO UPPER CASE,
              L_NAME4 TO UPPER CASE.

  MOVE : L_NAME1      TO W_LFA1-NAME1,
         L_NAME2      TO W_LFA1-NAME2,
         L_NAME3      TO W_LFA1-NAME3.
*          L_NAME4      TO W_LFA1-NAME4.

ENDFORM.                    " P1000_GET_BANK


*&---------------------------------------------------------------------*
*&      Form  P2000_LLCTY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_LLCTY_CHECK USING    P_CODE.
*>> 2001/06/12 KSB MODIFY
* IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
  IF ZTREQHD-ZFREQTY EQ 'LO'.
    IF P_CODE IS INITIAL.
      MESSAGE E265.
    ELSE.
      IF ZTREQHD-ZFREQTY EQ 'LO'.
        ZTLLCHD-ZFLLCTY = P_CODE.
      ELSEIF ZTREQHD-ZFREQTY EQ 'PU'.
        IF ( ZTREQHD-WAERS EQ 'KRW  ' ) AND
           ( P_CODE        NE '2AC'   ).
          MESSAGE E407.
        ENDIF.
        IF ( ZTREQHD-WAERS NE 'KRW  ' ) AND
           ( P_CODE        NE '2AA'   ).
          MESSAGE E408.
        ENDIF.
        ZTPUR-ZFLLCTY = P_CODE.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_LLCTY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_REFRESH_RECORD_CHK
*&---------------------------------------------------------------------*
FORM P2000_REFRESH_RECORD_CHK.

  LOOP AT IT_ZSREQIT.
    W_TABIX = SY-TABIX.
    CLEAR : IT_ZSREQIT-ZFMARK.
    READ TABLE IT_ZSREQIT_ORG WITH KEY ZFITMNO = IT_ZSREQIT-ZFITMNO.
    IF SY-SUBRC NE 0.
      IT_ZSREQIT-ZFMARK = 'X'.
      MODIFY IT_ZSREQIT INDEX W_TABIX.
    ENDIF.
  ENDLOOP.

*>>> ÈÄ¼ÓÀÛ¾÷ °ËÁõ....
  PERFORM  P2000_ITEM_DEL_CHECK.

ENDFORM.                    " P2000_REFRESH_RECORD_CHK

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_RELEASE_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_RELEASE_DATA.

  REFRESH : R_ZFRLST1, R_ZFRLST2.

  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE E961.   EXIT.
  ENDIF.
*-----------------------------------------------------------------------
* Import Request Release Yes/No
*-----------------------------------------------------------------------
  CLEAR R_ZFRLST1.
  IF  ZTIMIMG00-ZFRELYN1 EQ 'X'.
    MOVE: 'I'      TO R_ZFRLST1-SIGN,
          'EQ'     TO R_ZFRLST1-OPTION,
          'R'      TO R_ZFRLST1-LOW.
    APPEND R_ZFRLST1.
  ELSE.
    MOVE: 'I'      TO R_ZFRLST1-SIGN,
          'EQ'     TO R_ZFRLST1-OPTION,
          'N'      TO R_ZFRLST1-LOW.
    APPEND R_ZFRLST1.
  ENDIF.

*-----------------------------------------------------------------------
* Open Release Yes/No
*-----------------------------------------------------------------------
  CLEAR R_ZFRLST2.
  IF  ZTIMIMG00-ZFRELYN2 EQ 'X'.
    MOVE: 'I'      TO R_ZFRLST2-SIGN,
          'EQ'     TO R_ZFRLST2-OPTION,
          'R'      TO R_ZFRLST2-LOW.
    APPEND R_ZFRLST2.
  ELSE.
    MOVE: 'I'      TO R_ZFRLST2-SIGN,
          'EQ'     TO R_ZFRLST2-OPTION,
          'N'      TO R_ZFRLST2-LOW.
    APPEND R_ZFRLST2.
  ENDIF.

ENDFORM.                    " P1000_GET_RELEASE_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SHIPING_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_SHIPING_TEXT.
* Open Bank.
  PERFORM  P1000_GET_BANK     USING      ZTREQHD-ZFOPBN
                              CHANGING   W_LFA1.

  IF ZTREQHD-ZFREQTY EQ 'LC'.
    CASE ZTREQHD-ZFTRANS.
      WHEN 'A'.                                             " AIR
        ZTMLCSG910-ZFAIRYN = 'X'.
        CASE ZTREQHD-INCO1.
          WHEN 'EXW' OR 'FAS' OR 'FCA' OR 'FOB'.
            ZTMLCSG910-ZFAIRAC = '32'.
          WHEN OTHERS.
            ZTMLCSG910-ZFAIRAC = '31'.
        ENDCASE.
        ZTMLCSG910-ZFAIRAN = XZTIMIMGTX-ZFAPPNM.
        IF ZTMLCSG910-ZFAIRC1 IS INITIAL.
          IF ZTMLCSG910-ZFOCEC1 IS INITIAL.
            ZTMLCSG910-ZFAIRC1 = W_LFA1-NAME1.
            ZTMLCSG910-ZFAIRC2 = ''.
          ELSE.
            ZTMLCSG910-ZFAIRC1 = ZTMLCSG910-ZFOCEC1.
            ZTMLCSG910-ZFAIRC2 = ZTMLCSG910-ZFOCEC2.
          ENDIF.
        ENDIF.
        ZTMLCSG910-ZFOCEYN = ''.
        ZTMLCSG910-ZFOCEC1 = ''.
        ZTMLCSG910-ZFOCEC2 = ''.
        ZTMLCSG910-ZFOCEAC = ''.
        ZTMLCSG910-ZFOCEAN = ''.
      WHEN 'O'.                                             " OCEAN
        ZTMLCSG910-ZFOCEYN = 'X'.
        CASE ZTREQHD-INCO1.
          WHEN 'EXW' OR 'FAS' OR 'FCA' OR 'FOB'.
            ZTMLCSG910-ZFOCEAC = '32'.
          WHEN OTHERS.
            ZTMLCSG910-ZFOCEAC = '31'.
        ENDCASE.
        ZTMLCSG910-ZFOCEAN = XZTIMIMGTX-ZFAPPNM.
        IF ZTMLCSG910-ZFOCEC1 IS INITIAL.
          IF ZTMLCSG910-ZFAIRC1 IS INITIAL.
            ZTMLCSG910-ZFOCEC1 = W_LFA1-NAME1.
            ZTMLCSG910-ZFOCEC2 = ''.
          ELSE.
            ZTMLCSG910-ZFOCEC1 = ZTMLCSG910-ZFAIRC1.
            ZTMLCSG910-ZFOCEC2 = ZTMLCSG910-ZFAIRC2.
          ENDIF.
        ENDIF.
        ZTMLCSG910-ZFAIRYN = ''.
        ZTMLCSG910-ZFAIRC1 = ''.
        ZTMLCSG910-ZFAIRC2 = ''.
        ZTMLCSG910-ZFAIRAC = ''.
        ZTMLCSG910-ZFAIRAN = ''.
      WHEN 'B'.      " AIR + OCEAN
        CASE ZTREQHD-INCO1.
          WHEN 'EXW' OR 'FAS' OR 'FCA' OR 'FOB'.
            ZTMLCSG910-ZFOCEAC = '32'.
            ZTMLCSG910-ZFAIRAC = '32'.
          WHEN OTHERS.
            ZTMLCSG910-ZFOCEAC = '31'.
            ZTMLCSG910-ZFAIRAC = '31'.
        ENDCASE.
        ZTMLCSG910-ZFOCEYN = 'X'.
        ZTMLCSG910-ZFOCEAN = XZTIMIMGTX-ZFAPPNM.
        ZTMLCSG910-ZFAIRYN = 'X'.
        ZTMLCSG910-ZFAIRAN = XZTIMIMGTX-ZFAPPNM.
        IF ZTMLCSG910-ZFAIRC1 IS INITIAL.
          ZTMLCSG910-ZFAIRC1 = W_LFA1-NAME4.
        ENDIF.
        IF ZTMLCSG910-ZFOCEC1 IS INITIAL.
          ZTMLCSG910-ZFOCEC1 = W_LFA1-NAME4.
        ENDIF.
      WHEN OTHERS.
        CLEAR : ZTMLCSG910-ZFOCEYN, ZTMLCSG910-ZFOCEAC,
                ZTMLCSG910-ZFOCEAN, ZTMLCSG910-ZFAIRYN,
                ZTMLCSG910-ZFAIRAC, ZTMLCSG910-ZFAIRAN,
                ZTMLCSG910-ZFAIRC1, ZTMLCSG910-ZFOCEC1,
                ZTMLCSG910-ZFAIRC2, ZTMLCSG910-ZFOCEC2.
    ENDCASE.
  ENDIF.

ENDFORM.                    " P2000_SET_SHIPING_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_EDI_SEND_REPORT
*&---------------------------------------------------------------------*
FORM P2000_EDI_SEND_REPORT.

  IF ZTREQHD-ZFREQTY EQ 'LC'.
    IF ZTREQST-ZFDOCNO(6) EQ 'APP700'.
*      SUBMIT  ZRIMMLCO01
*              WITH P_ZFDHNO EQ ZTREQST-ZFDOCNO SIGN 'I'
*              AND RETURN.
    ELSEIF ZTREQST-ZFDOCNO(6) EQ 'APP707'.
*      SUBMIT  ZRIMMLCA01
*              WITH P_ZFDHNO EQ ZTREQST-ZFDOCNO SIGN 'I'
*              AND RETURN.
    ENDIF.
  ELSE.
*    MESSAGE I299 WITH  ZTREQST-ZFDOCNO.
*    SUBMIT  ZRIMFLAT_DISP  WITH P_DDENO  EQ ZTREQST-ZFDOCNO
*            AND RETURN.
  ENDIF.

ENDFORM.                    " P2000_EDI_SEND_REPORT

*&---------------------------------------------------------------------*
*&      Form  P2000_LC_EDI_RCV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_LC_EDI_RCV_REPORT.
  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      IF ZTREQST-ZFDOCNOR(6) EQ 'INF700'.
*        SUBMIT  ZRIMMLCO02
*                WITH P_ZFDHNO EQ ZTREQST-ZFDOCNOR SIGN 'I'
*                AND RETURN.
      ELSEIF ZTREQST-ZFDOCNOR(6) EQ 'INF707'.
*        SUBMIT  ZRIMMLCA02
*                WITH P_ZFDHNO EQ ZTREQST-ZFDOCNOR SIGN 'I'
*                AND RETURN.
      ENDIF.
    WHEN 'LO'.
      IF ZTREQST-ZFDOCNOR(6) EQ 'LOCADV'.
*        SUBMIT  ZRIMLLCO02
*                WITH P_ZFDHNO EQ ZTREQST-ZFDOCNOR SIGN 'I'
*                AND RETURN.
      ELSEIF ZTREQST-ZFDOCNOR(6) EQ 'LOCAMA'.
*        SUBMIT  ZRIMLLCA02
*                WITH P_ZFDHNO EQ ZTREQST-ZFDOCNOR SIGN 'I'
*                AND RETURN.
      ENDIF.
    WHEN 'PU'.
      IF ZTREQST-ZFDOCNOR(6) EQ 'PURLIC'.
*        SUBMIT  ZRIMPURO02
*                WITH P_ZFDHNO EQ ZTREQST-ZFDOCNOR SIGN 'I'
*                AND RETURN.
      ENDIF.
    WHEN OTHERS.
      MESSAGE I299 WITH  ZTREQST-ZFDOCNOR.
*      SUBMIT  ZRIMFLAT_DISP  WITH P_DDENO  EQ ZTREQST-ZFDOCNOR
*              AND RETURN.

  ENDCASE.

ENDFORM.                    " P2000_LC_EDI_RCV_REPORT

*&---------------------------------------------------------------------*
*&      Form  P2000_INS_EDI_RCV_REPORT
*&---------------------------------------------------------------------*
FORM P2000_INS_EDI_RCV_REPORT.
  IF ZTINS-ZFDOCNOR(6) EQ 'CIPADV'.
*>>>KSB 2001/01/10
*    SUBMIT  ZRIMCARG02  WITH P_ZFDHNO EQ ZTINS-ZFDOCNOR SIGN 'I'
*            AND RETURN.
*   ELSEIF ZTINS-ZFDOCNOR(6) EQ 'ENDADV'.
  ELSE.
    MESSAGE I299 WITH  ZTREQST-ZFDOCNOR.
*    SUBMIT  ZRIMFLAT_DISP  WITH P_DDENO  EQ ZTINS-ZFDOCNOR
*            AND RETURN.

  ENDIF.

ENDFORM.                    " P2000_INS_EDI_RCV_REPORT

*&---------------------------------------------------------------------*
*&      Form  P2000_COVERT_CURRENCY
*&---------------------------------------------------------------------*
FORM P2000_COVERT_CURRENCY.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK W_WKURS NE ZSREQHD-WKURS.
*  CHECK ZSREQHD-WKURS NE 1.

  IF ZTREQHD-WAERS EQ ZSREQHD-WAERS AND
     ZTREQST-ZFOPAMT EQ ZSREQHD-ZFOPAMT.
    EXIT.
  ENDIF.

  MOVE : ZSREQHD-ZFUPYN   TO ZTREQHD-ZFUPYN,
         ZSREQHD-WKURS    TO ZTREQHD-WKURS,
*         EKKO-WAERS       TO ZTREQHD-ZFWAERS,
         ZSREQHD-WAERS    TO ZTREQHD-WAERS,
         ZSREQHD-WAERS    TO ZTREQHD-ZFHDCUR,
         ZSREQHD-WAERS    TO ZTREQHD-ZFPKCUR,
         ZSREQHD-WAERS    TO ZTREQST-WAERS,
         ZSREQHD-ZFOPAMT  TO W_TOT_AMOUNT1.

  LOOP AT IT_ZSREQIT.
    W_TABIX = SY-TABIX.
    IT_ZSREQIT-WAERS = ZSREQHD-WAERS.
    W_BAPICURR_FR = IT_ZSREQIT-ZFNETPR * ZTREQHD-WKURS.
    IT_ZSREQIT-NETPR = TRUNC( W_BAPICURR_FR ).
    IF *ZTREQHD-ZFWAERS NE ZSREQHD-WAERS.
      PERFORM SET_CURR_CONV_TO_INTERNAL USING    IT_ZSREQIT-NETPR
                                                 ZTREQHD-WAERS.
    ENDIF.

    IF NOT IT_ZSREQIT-KWERT IS INITIAL.
      IT_ZSREQIT-KWERT = IT_ZSREQIT-ZFKWERT * ZTREQHD-WKURS.
*        IT_ZSREQIT_ORG-KWERT = TRUNC( IT_ZSREQIT_ORG-KWERT ).
      IF *ZTREQHD-ZFWAERS NE ZSREQHD-WAERS.
        PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSREQIT-KWERT
                                                IT_ZSREQIT-WAERS.
      ENDIF.
    ENDIF.
    MODIFY IT_ZSREQIT INDEX W_TABIX.
  ENDLOOP.

  LOOP AT IT_ZSREQIT_ORG.
    W_TABIX = SY-TABIX.
    IT_ZSREQIT_ORG-WAERS = ZTREQHD-WAERS.
    W_BAPICURR_FR = IT_ZSREQIT_ORG-ZFNETPR * ZTREQHD-WKURS.
    IT_ZSREQIT_ORG-NETPR = TRUNC( W_BAPICURR_FR ).
    IF *ZTREQHD-ZFWAERS NE ZSREQHD-WAERS.
      PERFORM SET_CURR_CONV_TO_INTERNAL USING    IT_ZSREQIT_ORG-NETPR
                                                 ZTREQHD-WAERS.
    ENDIF.

    IF NOT IT_ZSREQIT_ORG-KWERT IS INITIAL.
      IT_ZSREQIT_ORG-KWERT = IT_ZSREQIT_ORG-ZFKWERT * ZTREQHD-WKURS.
*        IT_ZSREQIT_ORG-KWERT = TRUNC( IT_ZSREQIT_ORG-KWERT ).

      IF *ZTREQHD-ZFWAERS NE ZSREQHD-WAERS.
        PERFORM SET_CURR_CONV_TO_INTERNAL USING IT_ZSREQIT_ORG-KWERT
                                                IT_ZSREQIT_ORG-WAERS.
      ENDIF.
    ENDIF.
    MODIFY IT_ZSREQIT_ORG INDEX W_TABIX.
  ENDLOOP.

  W_TOT_AMOUNT = 0.    W_COUNT      = 0.    W_TOT_ITEM   = 0.
  CLEAR : ZSREQIT.
  LOOP AT IT_ZSREQIT WHERE MENGE > 0.
* INSTALLING CHG.
    IF IT_ZSREQIT-KPEIN EQ 0.
      IT_ZSREQIT-KWERT = 0.
    ELSE.
      IT_ZSREQIT-KWERT = ( IT_ZSREQIT-KBETR / IT_ZSREQIT-KPEIN ) *
                           IT_ZSREQIT-MENGE.
    ENDIF.

* ¼öÀÔÀÇ·Ú Amount
    IF IT_ZSREQIT-PEINH NE 0.
      W_TOT_AMOUNT = W_TOT_AMOUNT +
                   ( IT_ZSREQIT-MENGE *
                   ( IT_ZSREQIT-NETPR / IT_ZSREQIT-PEINH ) ).
    ENDIF.
    W_TOT_ITEM   = W_TOT_ITEM   +   IT_ZSREQIT-MENGE.       " ITEM ¼ö?
  ENDLOOP.

** Packing Chg. ¹× Handing CHG. ´õÇÏ?
  W_TOT_AMOUNT = W_TOT_AMOUNT + ZTREQHD-ZFPKCHG + ZTREQHD-ZFHDCHG.
*                 - ZSREQHD-ZFPKCHG.
*  ZTREQHD-ZFPKCHG = ZTREQHD-ZFPKCHG - ZSREQHD-ZFPKCHG.
  IF ZSREQHD-ZFPKCHG IS INITIAL.
    ZTREQHD-ZFPKCHG =  ZTREQHD-ZFPKCHG  +
                       ( W_TOT_AMOUNT1 - W_TOT_AMOUNT ).
  ELSE.
    ZTREQHD-ZFPKCHG =  ZTREQHD-ZFPKCHG  - ZSREQHD-ZFPKCHG +
                       ( W_TOT_AMOUNT1 - W_TOT_AMOUNT ).
  ENDIF.
*  ZTREQHD-ZFPKCHG = ZTREQHD-ZFPKCHG + ZSREQHD-ZFPKCHG.


  ZTREQHD-ZFLASTAM = W_TOT_AMOUNT1.
  ZTREQST-ZFOPAMT = W_TOT_AMOUNT1.
  W_TOT_AMOUNT = W_TOT_AMOUNT1.

*>>> AMEND?
  IF SY-TCODE(4) EQ 'ZIM1'.
    CASE ZTREQHD-ZFREQTY.
      WHEN 'LC'.
        ZSREQHD-ZFOLDAMT = ZSREQHD-ZFOLDAMT * ZTREQHD-WKURS.
        ZTMLCAMHD-WAERS  = ZTREQHD-WAERS.
        ZTMLCAMHD-ZFILCUR = ZTREQHD-WAERS.
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
            W_AMOUNT = W_AMOUNT * -1.
            MOVE : '-'               TO  ZTMLCAMHD-ZFIDCD,
                   W_AMOUNT          TO  ZTMLCAMHD-ZFIDAM,
                   W_TOT_AMOUNT      TO  ZTMLCAMHD-ZFNDAMT.
          ENDIF.
        ENDIF.
      WHEN 'LO'.
        ZSREQHD-ZFOLDAMT = ZSREQHD-ZFOLDAMT * ZTREQHD-WKURS.
        W_AMOUNT = W_TOT_AMOUNT - ZSREQHD-ZFOLDAMT.
        IF W_AMOUNT NE 0.
          ZTLLCAMHD-ZFNOAMT = W_AMOUNT.
          ZTLLCAMHD-WAERS   = ZTREQHD-WAERS.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
*----------------------------------------------------------------------
* 2000/03/07 °­³ªÇü ´ë¸® DEFINE
*----------------------------------------------------------------------]
  PERFORM   P2000_CONV_OTHER_CURR_TO_USD   USING  W_TOT_AMOUNT
                                                  ZTREQHD-WAERS
                                         CHANGING W_LOCAL_AMT.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        ZTMLCHD-WAERS    = ZTREQHD-WAERS.
        ZTMLCHD-ZFOPAMT  = W_TOT_AMOUNT.
      ENDIF.

    WHEN 'PU'.
      ZTPUR-ZFUSD     = W_USD.
      ZTPUR-ZFTOCNM   =  ZSREQIT-MEINS.
      ZTPUR-ZFTOCN    =  W_TOT_ITEM.        "
      ZTPUR-ZFTOAM    =  W_TOT_AMOUNT.                      " AMOUNT
      ZTPUR-ZFTOAMC   =  ZTREQHD-WAERS.                     " CURRENCY
      ZTPUR-ZFTDAMC   =  ZTREQHD-WAERS.                     " CURRENCY
      ZTPUR-ZFTOAMU   =  W_LOCAL_AMT.       " USD È¯»ê±Ý?
* ÃÑÇÒÀÎ/ÇÒÁõ/º¯µ¿ ±Ý?
      IF NOT ZTPUR-ZFTDAM IS INITIAL.
        PERFORM   P2000_CONV_OTHER_CURR_TO_USD
                                 USING     ZTPUR-ZFTDAM
                                           ZTREQHD-WAERS
                                  CHANGING ZTPUR-ZFTDAMU.
      ENDIF.
* Ç°¸ñ ³»?
      LOOP AT IT_ZSPURSG1.
        W_TABIX = SY-TABIX.
        IF NOT IT_ZSPURSG1-NETPR IS INITIAL.
          PERFORM   P2000_CONV_OTHER_CURR_TO_USD
                                   USING     IT_ZSPURSG1-NETPR
                                             ZTREQHD-WAERS
                                    CHANGING IT_ZSPURSG1-ZFNETPRU.
        ENDIF.
        IF NOT IT_ZSPURSG1-ZFGOAMT IS INITIAL.
          PERFORM   P2000_CONV_OTHER_CURR_TO_USD
                                   USING    IT_ZSPURSG1-ZFGOAMT
                                            ZTREQHD-WAERS
                                   CHANGING IT_ZSPURSG1-ZFGOAMTU.
        ENDIF.
        IF NOT IT_ZSPURSG1-ZFDAM IS INITIAL.
          PERFORM   P2000_CONV_OTHER_CURR_TO_USD
                                    USING    IT_ZSPURSG1-ZFDAM
                                             ZTREQHD-WAERS
                                    CHANGING IT_ZSPURSG1-ZFDAMU.
        ENDIF.

        MODIFY IT_ZSPURSG1 INDEX W_TABIX.
      ENDLOOP.
* ±Ù°Å¼­·ù ³»?
*      LOOP AT IT_ZSPURSG4.
*        IF SY-TABIX EQ 1.
*          IT_ZSPURSG4-ZFGOAMT = W_TOT_AMOUNT.
*        ELSE.
*          IT_ZSPURSG4-ZFGOAMT = 0.
*        ENDIF.
*        IT_ZSPURSG4-WAERS = ZTREQHD-WAERS.
*        MODIFY IT_ZSPURSG4 INDEX SY-TABIX.
*      ENDLOOP.

    WHEN 'LO'.
      IF SY-TCODE(4) EQ 'ZIM0'.
        ZTLLCHD-ZFOPAMT  = W_TOT_AMOUNT.
        ZTLLCHD-ZFOPAMTC = ZTREQHD-WAERS.

        PERFORM    P2000_CONVERT_TO_USD_CURR    USING W_TOT_AMOUNT
                                                ZTREQHD-WAERS
                                                W_KRW
                                       CHANGING W_LOCAL_AMT1
                                                W_RATE
                                                W_FIXED_RATE.
        ZTLLCHD-ZFOPKAM = W_LOCAL_AMT1.
      ENDIF.

  ENDCASE.

  ZTREQHD-ZFLASTAM = W_TOT_AMOUNT.
  ZTREQHD-ZFUSDAM  = W_LOCAL_AMT.
  ZTREQHD-ZFUSD    = W_USD.
*----------------------------------------------------------------------
*  2000/04/04   °­³ªÇü ´ë¸® DEFINE
*----------------------------------------------------------------------
  IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
    IF NOT ZTREQHD-ZFINSYN IS INITIAL AND SY-DYNNR NE '0100'.
      CLEAR : ZTREQHD-ZFINSYN.
      MESSAGE I149 WITH ZTREQHD-ZFREQTY.
    ENDIF.
  ELSE.
    IF W_LOCAL_AMT > 10000.
      CASE ZTREQHD-INCO1.
        WHEN 'EXW' OR 'FCA' OR 'FAS' OR 'FOB' OR 'CPT' OR 'CFR'.
          IF ZTREQHD-ZFINSYN NE 'A'  AND SY-DYNNR NE '0100'.
            ZTREQHD-ZFINSYN = 'A'.
            MESSAGE I090 WITH ZTREQHD-ZFINSYN 'A'.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ELSE.
      CASE ZTREQHD-INCO1.
        WHEN 'EXW' OR 'FCA' OR 'FAS' OR 'FOB' OR 'CPT' OR 'CFR'.
          IF NOT ZTREQHD-ZFINSYN IS INITIAL  AND SY-DYNNR NE '0100'
.
            CLEAR : ZTREQHD-ZFINSYN.
            MESSAGE I091.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_COVERT_CURRENCY
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

  IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
    IF ZTREQHD-ZFINSYN IS INITIAL.
      ZTREQHD-ZFINSYN = 'N'.
      IF SY-DYNNR NE '0100'.
        MESSAGE S149 WITH ZTREQHD-ZFREQTY.
      ENDIF.
    ELSE.
      IF ZTREQHD-ZFINSYN NE 'N'.
        IF SY-DYNNR NE '0100'.
          MESSAGE W348.
        ENDIF.
      ELSE.
        IF ZTREQHD-ZFINSYN EQ 'A'.
          IF SY-DYNNR NE '0100'.
            MESSAGE E652 WITH ZTREQHD-ZFREQTY.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*    IF W_LOCAL_AMT >= 10000.
    CASE ZTREQHD-INCO1.
      WHEN 'EXW' OR 'FCA' OR 'FAS' OR 'FOB' OR 'CPT' OR 'CFR'.
        IF ZTREQHD-ZFINSYN IS INITIAL.
          IF SY-DYNNR NE '0100'.
            MESSAGE S090 WITH ZTREQHD-ZFINSYN 'A'.
          ENDIF.
          ZTREQHD-ZFINSYN = 'A'.
*          ELSEIF ZTREQHD-ZFINSYN NE 'A'.
*             MESSAGE W350 WITH ZTREQHD-INCO1.
        ENDIF.
      WHEN SPACE.
*          IF ZTREQHD-ZFINSYN IS INITIAL.
*          CLEAR : ZTREQHD-ZFINSYN.
*          ENDIF.
      WHEN OTHERS.
        IF ZTREQHD-ZFINSYN IS INITIAL.
          IF SY-DYNNR NE '0100'.
            ZTREQHD-ZFINSYN = 'N'.
          ENDIF.
        ELSE.
          IF ZTREQHD-ZFINSYN NE 'N'.
            IF SY-DYNNR NE '0100'.
              MESSAGE W349 WITH ZTREQHD-INCO1.
            ENDIF.
          ENDIF.
        ENDIF.
*          CLEAR : ZTREQHD-ZFINSYN.
    ENDCASE.
*    ELSE.
*      CLEAR : ZTREQHD-ZFINSYN.
*    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_INSURANCE_BIT_SET
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

      MOVE: KONP-KBETR   TO   P_KBETR,
            KONP-KONWA   TO   P_KONWA.
* ======> À¯Àç¿À °úÀå '10'À¸·Î ³ª´®........
      IF NOT P_KBETR IS INITIAL.
        P_KBETR = P_KBETR / 10.
      ENDIF.
    ENDSELECT.
  ENDIF.

ENDFORM.                    " P1000_READ_KONP
*&---------------------------------------------------------------------*
*&      Form  P3000_INIT_CURR_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_INIT_CURR_SET.

  IF ZTINSRSP-ZFTAMIC IS INITIAL.
    MOVE ZTINS-ZFINAMTC       TO       ZTINSRSP-ZFTAMIC.
  ENDIF.
  IF ZTINSRSP-ZFCAMIC IS INITIAL.
    MOVE ZTINS-ZFINAMTC       TO       ZTINSRSP-ZFCAMIC.
  ENDIF.
  IF ZTINSRSP-ZFDAMIC IS INITIAL.
    MOVE ZTINS-ZFINAMTC       TO       ZTINSRSP-ZFDAMIC.
  ENDIF.
  IF ZTINSRSP-ZFTPRC IS INITIAL.
    MOVE 'KRW'                TO       ZTINSRSP-ZFTPRC.
  ENDIF.
  IF ZTINSRSP-ZFCPRC IS INITIAL.
    MOVE 'KRW'                TO       ZTINSRSP-ZFCPRC.
  ENDIF.
  IF ZTINSRSP-ZFDPRC IS INITIAL.
    MOVE 'KRW'                TO       ZTINSRSP-ZFDPRC.
  ENDIF.
  IF ZTINSRSP-ZFVPRC IS INITIAL.
    MOVE 'KRW'                TO       ZTINSRSP-ZFVPRC.
  ENDIF.
  IF ZTINSRSP-ZFIPRC IS INITIAL.
    MOVE 'KRW'                TO       ZTINSRSP-ZFIPRC.
  ENDIF.
ENDFORM.                    " P3000_INIT_CURR_SET
*&---------------------------------------------------------------------*
*&      Form  P2000_INS_DOC_DISP_SELECT3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INS_DOC_DISP_SELECT3.

  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
  SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO I~ZFINAMT
          I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST I~ZFINSEQ
       INTO (IT_ZSREQHD-ZFREQNO,
             IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
             IT_ZSREQHD-ZFOPNO, IT_ZSREQHD-ZFINAMT,
             IT_ZSREQHD-WAERS, IT_ZSREQHD-EBELN,
             IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
             IT_ZSREQHD-ZFINSEQ)
        FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
        ON    R~ZFREQNO    EQ   I~ZFREQNO
        WHERE R~ZFREQNO EQ ZSREQHD-ZFREQNO
        AND   R~ZFAMDNO EQ '00000'
        AND   I~ZFAMDNO EQ '00000'
        ORDER  BY I~ZFREQNO.
    APPEND IT_ZSREQHD.
  ENDSELECT.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.
  W_STATUS_CHK = 'C'.
  INCLUDE = 'INDISPL3'.                 " º¸Çè »ý?
  CALL SCREEN 0014 STARTING AT  01 3
                   ENDING   AT  90 15.

ENDFORM.                    " P2000_INS_DOC_DISP_SELECT3
*&---------------------------------------------------------------------*
*&      Form  P2000_INS_CHANGE_LIST3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INS_CHANGE_LIST3.

  WRITE : / IT_ZSREQHD-ZFINSEQ NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP,
            IT_ZSREQHD-ZFAMDNO NO-GAP COLOR COL_KEY INTENSIFIED,
            SY-VLINE NO-GAP.

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_ZSREQHD-ZFREQTY NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-EBELN   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-WAERS   NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFINAMT CURRENCY IT_ZSREQHD-WAERS NO-GAP,
          SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFDOCST NO-GAP,    SY-VLINE NO-GAP,
          IT_ZSREQHD-ZFOPNNO NO-GAP.

ENDFORM.                    " P2000_INS_CHANGE_LIST3
*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_COST_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_CALL_COST_SCREEN.

  DESCRIBE  TABLE  IT_ZSIMCOST  LINES  W_LINE.
  IF W_LINE GT 0.
    W_DYNNR  =  SY-DYNNR.
    SET SCREEN 0050.   LEAVE TO SCREEN 0050.
  ELSE.
    MESSAGE S608.
  ENDIF.

ENDFORM.                    " P2000_CALL_COST_SCREEN

*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSIMCOST_BUKRS  text
*      -->P_IT_ZSIMCOST_GJAHR  text
*      -->P_IT_ZSIMCOST_BELNR  text
*----------------------------------------------------------------------*
FORM P2000_ALV_COMMAND USING R_UCOMM      LIKE SY-UCOMM
                              RS_SELFIELD TYPE SLIS_SELFIELD.
*
  CASE R_UCOMM.
    WHEN  'WAHL'.             " ºñ¿ë¹®¼­ & doubleclick
      READ TABLE IT_ZSIMCOST INDEX RS_SELFIELD-TABINDEX. "cursorposit.
      IF SY-SUBRC EQ 0.
        PERFORM  P2000_FI_DOCUMENT_DISPLAY
                               USING   IT_ZSIMCOST-BUKRS
                                       IT_ZSIMCOST-ZFFIYR
                                       IT_ZSIMCOST-ZFACDO.
      ELSE.
        MESSAGE S962.
      ENDIF.
      CLEAR R_UCOMM.
    WHEN 'WAH2' OR '&IC1'.
      READ TABLE IT_ZSIMCOST INDEX RS_SELFIELD-TABINDEX. "cursorposit.
      IF SY-SUBRC EQ 0.
        PERFORM  P2000_COST_DOCUMENT_DISPLAY
                               USING   IT_ZSIMCOST-BUKRS
                                       IT_ZSIMCOST-GJAHR
                                       IT_ZSIMCOST-BELNR.
      ELSE.
        MESSAGE S962.
      ENDIF.
      CLEAR R_UCOMM.
  ENDCASE.

ENDFORM.                    " P2000_ALV_COMMAND

*&---------------------------------------------------------------------*
*&      Form  P2000_FI_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSIMCOST_BUKRS  text
*      -->P_IT_ZSIMCOST_ZFGJAHR  text
*      -->P_IT_ZSIMCOST_ZFBELNR  text
*----------------------------------------------------------------------*
FORM P2000_FI_DOCUMENT_DISPLAY USING    P_BUKRS
                                        P_GJAHR
                                        P_BELNR.

  IF P_BELNR IS INITIAL.
    MESSAGE S588.   EXIT.
  ELSE.
*>>> LIV ÀüÇ¥¹øÈ£ÀÎÁö, È¸°èÀüÇ¥ÀÎÁö¸¦ ±¸ºÐ.
    SELECT * FROM EKBZ UP TO 1 ROWS
             WHERE BELNR EQ P_BELNR
             AND   GJAHR EQ P_GJAHR.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      SELECT * FROM EKBE UP TO 1 ROWS
               WHERE BELNR EQ P_BELNR
               AND   GJAHR EQ P_GJAHR.
      ENDSELECT.
    ENDIF.
    IF SY-SUBRC EQ 0.
      SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
      SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
      SET PARAMETER ID 'RBN'    FIELD P_BELNR.
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
    ELSE.
      SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
      SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
      SET PARAMETER ID 'BLN'    FIELD P_BELNR.
      CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_FI_DOCUMENT_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  P2000_FI_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSIMCOST_BUKRS  text
*      -->P_IT_ZSIMCOST_GJAHR  text
*      -->P_IT_ZSIMCOST_BELNR  text
*----------------------------------------------------------------------*
FORM P2000_COST_DOCUMENT_DISPLAY USING    P_BUKRS
                                          P_GJAHR
                                          P_BELNR.

  IF P_BELNR IS INITIAL.
    MESSAGE S167 WITH 'Import expense Doc'.   EXIT.
  ELSE.
    SET PARAMETER ID 'BUK'    FIELD P_BUKRS.
    SET PARAMETER ID 'GJR'    FIELD P_GJAHR.
    SET PARAMETER ID 'ZPBENR' FIELD P_BELNR.
    CALL TRANSACTION 'ZIMY3' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                    " P2000_COST_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_COST_DOCUMENT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_BANK_TEXT_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_BANK_TEXT_MOVE.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      MOVE : ZTREQHD-ZFOPBN TO ZTMLCHD-ZFOPBN. " °³¼³ÀºÇà °Å·¡Ã³ÄÚ?
*      MOVE : W_LFA1-NAME4    TO   ZTMLCHD-ZFOBNM,     " ÀºÇà¸í..
      MOVE : W_OPEN_NM       TO   ZTMLCHD-ZFOBNM,     " ÀºÇà¸í..
*            W_LFA1-NAME2    TO   ZTMLCHD-ZFOBBR,     " ÁöÁ¡?
             SPACE           TO   ZTMLCHD-ZFOBBR,     " ÁöÁ¡?
             W_LFA1-TELF1    TO   ZTMLCHD-ZFOBPH,     " ÀüÈ­¹ø?
*            W_LFA1-BAHNS    TO   ZTMLCHD-ZFOPBNCD." ¼ö¹ß½Å½Äº°ÀÚ.
             W_LFA1-KRAUS    TO   ZTMLCHD-ZFOPBNCD." ÇÑ±¹ÀºÇàºÎ¿©ÄÚµå.
    WHEN 'LO'.
      MOVE : ZTREQHD-ZFOPBN TO ZTLLCHD-ZFOPBN. " °³¼³ÀºÇà °Å·¡Ã³ÄÚ?

      MOVE : W_LFA1-NAME1    TO   ZTLLCHD-ZFOBNM,     " ÀºÇà?
*             W_LFA1-NAME2    TO   ZTLLCHD-ZFOBBR,     " ÁöÁ¡?
              SPACE           TO   ZTLLCHD-ZFOBBR,     " ÁöÁ¡?
              W_LFA1-KRAUS    TO   ZTLLCHD-ZFOPBNCD. " ÇÑ±¹ÀºÇàºÎ¿©ÄÚµå.
*             W_LFA1-BAHNS    TO   ZTLLCHD-ZFOPBNCD. " ¼ö¹ß½Å½Äº°ÀÚ.
    WHEN 'PU'.
      MOVE : ZTREQHD-ZFOPBN TO ZTPUR-ZFACBN.   " °³¼³ÀºÇà °Å·¡Ã³ÄÚ?
      MOVE : W_LFA1-NAME1    TO   ZTPUR-ZFACNM,       " ÀºÇà¸í.
*            W_LFA1-NAME2    TO   ZTPUR-ZFACBR,       " ÁöÁ¡¸í.
             SPACE           TO   ZTPUR-ZFACBR,       " ÁöÁ¡ÀüÈ­¹øÈ£.
*             W_LFA1-BAHNS    TO   ZTPUR-ZFACBNCD.  " ¼ö¹ß½Å½Äº°ÀÚ.
             W_LFA1-KRAUS    TO   ZTPUR-ZFACBNCD.  " ÇÑ±¹ÀºÇàºÎ¿©ÄÚµå.
    WHEN 'TT'.
      MOVE : ZTREQHD-ZFOPBN TO ZTTTHD-ZFOPBN. " °³¼³ÀºÇà °Å·¡Ã³ÄÚ?
      MOVE : W_LFA1-NAME1    TO   ZTTTHD-ZFOBNM,      " ÀºÇà¸í.
*            W_LFA1-NAME2    TO   ZTTTHD-ZFOBBR,      " ÁöÁ¡¸í.
             SPACE           TO   ZTTTHD-ZFOBBR,      " ÁöÁ¡¸í.
*            W_LFA1-BAHNS    TO   ZTTTHD-ZFOPBNCD. " ¼ö¹ß½Å½Äº°ÀÚ.
             W_LFA1-KRAUS    TO   ZTTTHD-ZFOPBNCD. " ÇÑ±¹ÀºÇàºÎ¿©ÄÚµå.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_BANK_TEXT_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_DELIVERY_MODE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_DELIVERY_MODE_SET.

  CLEAR : EIKP.

  IF NOT EKKO-EXNUM IS INITIAL.
     SELECT SINGLE * FROM EIKP
                     WHERE EXNUM   EQ   EKKO-EXNUM.

     SELECT SINGLE * FROM  ZTIMIMG12
            WHERE  EXPVZ  EQ  EIKP-EXPVZ.
     IF SY-SUBRC EQ 0.
        MOVE ZTIMIMG12-ZFTRANS  TO  ZTREQHD-ZFTRANS.
     ELSE.
        MOVE EIKP-EXPVZ         TO  ZTREQHD-ZFTRANS.
     ENDIF.
  ENDIF.
  IF ZTREQHD-ZFTRANS IS INITIAL.
     MOVE 'O'   TO  ZTREQHD-ZFTRANS.
  ENDIF.

ENDFORM.                    " P2000_DELIVERY_MODE_SET
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_GET_EXCHANGE_RATE USING    P_WAERS
                                      P_DATE
                             CHANGING P_EXRATE
                                      P_FFACT.


  DATA : L_TEXT_EXRATE(255)    TYPE C,
         L_FOREIGN_FACTOR(255) TYPE C,
         L_RATE                TYPE P,
         L_FACTOR              TYPE P.

  CLEAR :  P_EXRATE.

* CURRENCY ¹× ÀÏÀÚ ¹ÌÀÔ·Â½Ã....
  IF P_WAERS IS INITIAL.   P_EXRATE = 0.  EXIT.   ENDIF.
  IF P_DATE  IS INITIAL.   P_EXRATE = 0.  EXIT.   ENDIF.

* MESSAGE BOX
  IF P_WAERS EQ 'KRW'.
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
            LOCAL_CURRENCY   = 'KRW'
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

  PERFORM    P2000_WRITE_NO_MASK(SAPMZIM01) CHANGING  L_TEXT_EXRATE.
  PERFORM    P2000_WRITE_NO_MASK(SAPMZIM01) CHANGING  L_FOREIGN_FACTOR.

  P_EXRATE = L_TEXT_EXRATE.
  L_FACTOR = L_FOREIGN_FACTOR.
  P_FFACT  = L_FACTOR.


  IF P_EXRATE LT 0.
    P_EXRATE = P_EXRATE * -1.
  ENDIF.

*  IF L_FACTOR NE 0.
*     P_EXRATE = P_EXRATE / L_FACTOR.
*  ENDIF.

ENDFORM.                    " P2000_GET_EXCHANGE_RATE

*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_EXCHANGE_RATE.

  SPOP-TITEL = 'Exchange rate È®ÀÎ'.
*   SPOP-TEXTLINE1 = 'Exchange rate¸¦ ÀÔ·ÂÇÏÁö ¾Ê¾Ò½À´Ï´Ù.'.
  SPOP-TEXTLINE2 = 'SytemÀÇ Exchange rateÀ» ¹Ý¿µÇÏ½Ã°Ú½À´Ï±î?'.
  CLEAR : CANCEL_OPTION.
  OPTION = 1.
  TEXTLEN = 40.

*>> ÅëÈ­À¯Çü...
  IF V_TCURR-KURST IS INITIAL.
    V_TCURR-KURST = 'M'.
  ENDIF.

  CALL SCREEN 0020 STARTING AT 20 6
                   ENDING   AT 80 11.

*   IF ANTWORT = 'C'.       " Cancel
*      SET SCREEN SY-DYNNR.
*   ENDIF.

ENDFORM.                    " P2000_MESSAGE_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*&      Module  GET_SHIP_PORT_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SHIP_PORT_NAME_SCR0102 INPUT.
* if display mode, exit.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF NOT ZTREQHD-ZFSPRTC IS INITIAL
     AND ZTREQHD-ZFSPRT IS INITIAL.
*>> Shipping Country.
    IF ZTREQHD-ZFSHCU IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFSHCU'.
    ENDIF.
    SELECT SINGLE PORTT INTO ZTREQHD-ZFSPRT
           FROM   ZTIEPORT
           WHERE  LAND1 EQ ZTREQHD-ZFSHCU
           AND    PORT  EQ ZTREQHD-ZFSPRTC.
    IF SY-SUBRC NE 0.
      MESSAGE E421(ZIM1) WITH ZTREQHD-ZFSHCU ZTREQHD-ZFSPRTC.
    ENDIF.
  ENDIF.

ENDMODULE.                 " GET_SHIP_PORT_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ARR_PORT_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ARR_PORT_NAME_SCR0102 INPUT.
* Á¶È¸ MODE½Ã MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF NOT ZTREQHD-ZFAPRTC IS INITIAL
     AND ZTREQHD-ZFAPRT IS INITIAL.
*>> µµÂø±¹.
    IF ZTREQHD-ZFARCU IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTREQHD' 'ZFARCU'.
    ENDIF.
    SELECT SINGLE PORTT INTO ZTREQHD-ZFAPRT
           FROM   ZTIEPORT
           WHERE  LAND1 EQ ZTREQHD-ZFARCU
           AND    PORT  EQ ZTREQHD-ZFAPRTC.
    IF SY-SUBRC NE 0.
      MESSAGE E421(ZIM1) WITH ZTREQHD-ZFARCU ZTREQHD-ZFAPRTC.
    ENDIF.
  ENDIF.

ENDMODULE.                 " GET_ARR_PORT_NAME_SCR0102  INPUT
