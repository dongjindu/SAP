FUNCTION ZF4IF_SHLP_EXIT_ZPPCLMN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

* EXIT immediately, if you do not want to handle this step
*  IF CALLCONTROL-STEP <> 'SELONE' AND
*     CALLCONTROL-STEP <> 'SELECT' AND
*     " AND SO ON
*     CALLCONTROL-STEP <> 'DISP'.
*     EXIT.
*  ENDIF.
*

*"----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
*"----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normaly only SHLP-SELOPT should be changed in this step.
  IF CALLCONTROL-STEP = 'PRESEL1 '.
    DATA: LW_SHLP TYPE DDSHIFACE,
          LT_SHLP TYPE DDSHIFACES.
*    data: type SHLP_DESCT
    LT_SHLP = SHLP-INTERFACE.
    LOOP AT LT_SHLP INTO LW_SHLP.
*      delete Lt_SHLP index sy-tabix.
      CLEAR: LW_SHLP-VALUE.
      MODIFY LT_SHLP FROM LW_SHLP TRANSPORTING VALUE. "index sy-tabix.
    ENDLOOP.
    SHLP-INTERFACE = LT_SHLP.
    CLEAR: CALLCONTROL-MAXRECORDS.

    EXIT.
  ENDIF.
*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step.
* Standard function module F4UT_RESULTS_MAP may be very helpfull in this
* step.
  IF CALLCONTROL-STEP = 'SELECT'.
*   PERFORM STEP_SELECT TABLES RECORD_TAB SHLP_TAB
*                       CHANGING SHLP CALLCONTROL RC.
*   IF RC = 0.
*     CALLCONTROL-STEP = 'DISP'.
*   ELSE.
*     CALLCONTROL-STEP = 'EXIT'.
*   ENDIF.
    EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.
*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
* This step is called, before the selected data is displayed.
* You can e.g. modify or reduce the data in RECORD_TAB
* according to the users authority.
* If you want to get the standard display dialog afterwards, you
* should not change CALLCONTROL-STEP.
* If you want to overtake the dialog on you own, you must return
* the following values in CALLCONTROL-STEP:
* - "RETURN" if one line was selected. The selected line must be
*   the only record left in RECORD_TAB. The corresponding fields of
*   this line are entered into the screen.
* - "EXIT" if the values request should be aborted
* - "PRESEL" if you want to return to the selection dialog
* Standard function modules F4UT_PARAMETER_VALUE_GET and
* F4UT_PARAMETER_RESULTS_PUT may be very helpfull in this step.
  IF CALLCONTROL-STEP = 'DISP'.
*   PERFORM AUTHORITY_CHECK TABLES RECORD_TAB SHLP_TAB
*                           CHANGING SHLP CALLCONTROL.
    DATA: L_MODEL(2).
    DATA: WA_TEMP LIKE RECORD_TAB,
          IT_TEMP LIKE RECORD_TAB OCCURS 0.

    IMPORT L_MODEL FROM MEMORY ID 'ZMOD'.
*  GET PARAMETER ID 'ZMOD' FIELD L_MODEL.
    IF NOT L_MODEL IS INITIAL.
      LOOP AT RECORD_TAB INTO WA_TEMP.
        IF WA_TEMP-STRING+3(2) = L_MODEL.
          APPEND WA_TEMP TO IT_TEMP.
        ENDIF.
        CLEAR: WA_TEMP.
      ENDLOOP.
      CLEAR: RECORD_TAB[].
      RECORD_TAB[] = IT_TEMP[].
      CLEAR: IT_TEMP[].
    ENDIF.
  ENDIF.
ENDFUNCTION.
