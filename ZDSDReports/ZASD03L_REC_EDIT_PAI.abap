*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.

  CASE SY-DYNNR.
    WHEN '9000'.
      CASE SAVE_OK_CODE.
        WHEN 'BACK'.
          LEAVE TO SCREEN 0.
        WHEN 'EXIT'.
          LEAVE TO SCREEN 0.
        WHEN 'CANC'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  W_ZISSN = W_ZISSN_SCR.
  CASE SAVE_OK_CODE.
*    WHEN 'REFR'.
*      PERFORM REFRESH_1.  " need to delete this function
    WHEN 'CALC'.
      PERFORM PROCESS_CALCULATE.
    WHEN 'RECA'.
      PERFORM PROCESS_RECALCULATE.
    WHEN 'CONF'.
      PERFORM CONFIRM_DMR.
    WHEN 'CABK'.
      PERFORM PROCESS_CALCULATE_BK.
    WHEN 'REBK'.
      PERFORM PROCESS_RECALCULATE_BK.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT




















************************************************************************
***INCLUDE ZISD05L_CMR_CREATE_PAI .
* INPUT MODULE FOR TABLECONTROL 'TC_9000': MARK TABLE
MODULE TC_9000_MARK INPUT.
  MODIFY G_TC_9000_ITAB
    FROM G_TC_9000_WA
    INDEX TC_9000-CURRENT_LINE
    TRANSPORTING FLAG.
ENDMODULE.

* INPUT MODULE FOR TABLECONTROL 'TC_9000': PROCESS USER COMMAND
MODULE TC_9000_USER_COMMAND INPUT.
***  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TC_9000'
                              'G_TC_9000_ITAB'
                              'FLAG'
                     CHANGING OK_CODE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  REFRESH_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form REFRESH_1.

endform.                    " REFRESH_1
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CALCULATE_BK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form PROCESS_CALCULATE.
  DATA: wa_jobname LIKE tbtcjob-jobname VALUE 'ZASDA05_CAL',
        wa_jobcount LIKE tbtcjob-jobcount,
        valid TYPE C,
        user_print_params type PRI_PARAMS occurs 0 with header line.

  CLEAR: w_err.
  PERFORM check_issue_no USING save_ok_code.
  IF w_err = 'X'.
    EXIT.
  ENDIF.

  REFRESH it_list. CLEAR it_list.
  LOOP AT   g_tc_9000_itab
       INTO g_tc_9000_wa
       WHERE flag = 'X'
       AND   zrcfg = ' '.
    MOVE-CORRESPONDING g_tc_9000_wa TO it_list.
    it_list-zissn = w_zissn.
    it_list-okcode = 'CALC'.
    APPEND it_list. CLEAR it_list.
  ENDLOOP.

  EXPORT it_list p_one TO   DATABASE indx(zs) ID variant.

*  MESSAGE I000 WITH 'STARTING BATCH JOB'.
*
*  EVENTID = 'ZASD03_01'.
*
*  CALL FUNCTION 'BP_EVENT_RAISE'
*    EXPORTING
*      EVENTID                      = EVENTID
**     EVENTPARM                    = ' '
**     TARGET_INSTANCE              = ' '
*   EXCEPTIONS
*     BAD_EVENTID                  = 1
*     EVENTID_DOES_NOT_EXIST       = 2
*     EVENTID_MISSING              = 3
*     RAISE_FAILED                 = 4
*     OTHERS                       = 5.
  SET PARAMETER ID 'PONE' FIELD p_one.

  SUBMIT zasd03m_rec_edit_bg AND RETURN.

  IMPORT it_list it_rec_l it_rec_h it_rec_i
    FROM DATABASE indx(zr)
    ID variant.

  PERFORM refresh.

endform.                    " PROCESS_CALCULATE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_RECALCULATE_BK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form PROCESS_RECALCULATE.
  DATA: flag(1).
  DATA: wa_jobname LIKE tbtcjob-jobname VALUE 'ZASDA05_CAL',
        wa_jobcount LIKE tbtcjob-jobcount,
        valid TYPE C,
        user_print_params type PRI_PARAMS occurs 0 with header line.
  CLEAR: w_err.
  PERFORM check_issue_no USING save_ok_code.
  IF w_err = 'X'.
    EXIT.
  ENDIF.

  REFRESH it_list. CLEAR it_list.
  LOOP AT   g_tc_9000_itab
       INTO g_tc_9000_wa
       WHERE flag = 'X'
       AND   ( zrcfg = '1' OR zrcfg = '2' ).
    MOVE-CORRESPONDING g_tc_9000_wa TO it_list.
    it_list-zissn = w_zissn.
    it_list-okcode = 'RECA'.
    APPEND it_list. CLEAR it_list.
  ENDLOOP.

  EXPORT it_list p_one TO   DATABASE indx(zs) ID variant.

*  set parameter id 'PONE' FIELD P_ONE.

  SUBMIT zasd03m_rec_edit_bg AND RETURN.

IMPORT it_list it_rec_l it_rec_h it_rec_i
  FROM DATABASE indx(zr)
  ID variant.

PERFORM refresh.

endform.                    " PROCESS_RECALCULATE
