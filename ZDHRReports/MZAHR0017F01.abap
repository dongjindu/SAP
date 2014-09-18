*----------------------------------------------------------------------*
*   INCLUDE MZAHR0005F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  get_data_from_pcp01
*&---------------------------------------------------------------------*
FORM GET_DATA_FROM_ET01.
  CLEAR IT_ET01. REFRESH IT_ET01.
  CLEAR DEL_ET01. REFRESH DEL_ET01.
*
  CLEAR ZTHR_ET01.
  SELECT ZMODL ZGRUP ZMTXT ZGTXT ZGRMK
         ERDAT ERZET ERNAM AEDAT AEZET AENAM
    INTO CORRESPONDING FIELDS OF TABLE IT_ET01
    FROM ZTHR_ET01 WHERE ERDAT > '20000101'.
*
  IF SY-SUBRC = 0.
    SORT IT_ET01 BY ZMODL.
  ELSE.
    W_COUNT = 1.
    DO 50 TIMES.
      IT_ET01-TABIX = W_COUNT.
      APPEND IT_ET01. CLEAR IT_ET01.
      W_COUNT = W_COUNT + 1.
    ENDDO.
  ENDIF.
*
  SORT IT_ET01 BY ZMODL ZGRUP.
  REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
  DESCRIBE TABLE IT_ET01 LINES TC9000-LINES.
*
  CLEAR IT_STATS. REFRESH IT_STATS.
  IT_STATS-FCODE = 'SAVE'. APPEND IT_STATS.
  IT_STATS-FCODE = 'DELE'. APPEND IT_STATS.
  IT_STATS-FCODE = 'NEWN'. APPEND IT_STATS.
  W_MODES = 0.
  CALL SCREEN 9000.
ENDFORM.                    " get_data_from_ET01
*&---------------------------------------------------------------------*
*&      Form  delete_line_ET01
*&---------------------------------------------------------------------*
FORM DELETE_LINE_ET01.
  CLEAR IT_ET01.
  READ TABLE IT_ET01 WITH KEY CHKBX = 'X'.
  MOVE-CORRESPONDING IT_ET01 TO DEL_ET01.
  APPEND DEL_ET01. CLEAR DEL_ET01.
*
  DELETE IT_ET01 WHERE CHKBX = 'X'.
  REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
  DESCRIBE TABLE IT_ET01 LINES TC9000-LINES.
ENDFORM.                    " delete_line_ET01
*&---------------------------------------------------------------------*
*&      Form  append_new_entries
*&---------------------------------------------------------------------*
FORM APPEND_NEW_ENTRIES.
  DESCRIBE TABLE IT_ET01 LINES W_COUNT.
*
*  CLEAR IT_ET01.
  DO 1 TIMES.
    W_COUNT = W_COUNT + 1.
    clear it_et01.
    IT_ET01-TABIX = W_COUNT.
    APPEND IT_ET01. CLEAR IT_ET01.
  ENDDO.
*
  REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
  DESCRIBE TABLE IT_ET01 LINES TC9000-LINES.
ENDFORM.                    " append_new_entries
*&---------------------------------------------------------------------*
*&      Form  save_ET01_data
*&---------------------------------------------------------------------*
FORM SAVE_ET01_DATA.
  DELETE IT_ET01 WHERE ZMODL = 0.
  REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
  DESCRIBE TABLE IT_ET01 LINES TC9000-LINES.
*
  MODIFY ZTHR_ET01 FROM TABLE IT_ET01.
  IF SY-SUBRC = 0.
    DELETE ZTHR_ET01 FROM TABLE DEL_ET01.
    MESSAGE S001 WITH 'DATA SAVED'.
  ELSE.
    MESSAGE E001 WITH 'Data Save Failed'.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " save_ET01_data
*&---------------------------------------------------------------------*
*&      Form  screen_value_update
*&---------------------------------------------------------------------*
FORM SCREEN_VALUE_UPDATE.
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
*
  READ TABLE IT_MOVAL INDEX W_TABIX.
  DYNPFIELDS-FIELDNAME  = 'W_ZMTXT'.
  DYNPFIELDS-FIELDVALUE = IT_MOVAL-ZMTXT.
  APPEND DYNPFIELDS. CLEAR DYNPFIELDS.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME               = SY-CPROG
            DYNUMB               = SY-DYNNR
       TABLES
            DYNPFIELDS           = DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA = 1
            INVALID_DYNPROFIELD  = 2
            INVALID_DYNPRONAME   = 3
            INVALID_DYNPRONUMMER = 4
            INVALID_REQUEST      = 5
            NO_FIELDDESCRIPTION  = 6
            UNDEFIND_ERROR       = 7
            OTHERS               = 8.
ENDFORM.                    " screen_value_update
*&---------------------------------------------------------------------*
*&      Form  screen_value_update_1
*&---------------------------------------------------------------------*
FORM SCREEN_VALUE_UPDATE_1.
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
*
  READ TABLE IT_GRVAL INDEX W_TABIX.
  DYNPFIELDS-FIELDNAME  = 'W_ZGTXT'.
  DYNPFIELDS-FIELDVALUE = IT_GRVAL-ZGTXT.
  APPEND DYNPFIELDS. CLEAR DYNPFIELDS.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME               = SY-CPROG
            DYNUMB               = SY-DYNNR
       TABLES
            DYNPFIELDS           = DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA = 1
            INVALID_DYNPROFIELD  = 2
            INVALID_DYNPRONAME   = 3
            INVALID_DYNPRONUMMER = 4
            INVALID_REQUEST      = 5
            NO_FIELDDESCRIPTION  = 6
            UNDEFIND_ERROR       = 7
            OTHERS               = 8.
ENDFORM.                    " screen_value_update_1
*&---------------------------------------------------------------------*
*&      Form  get_data_from_ET02
*&---------------------------------------------------------------------*
FORM GET_DATA_FROM_ET02.
  CLEAR IT_ET02. REFRESH IT_ET02.
  CLEAR DEL_ET02. REFRESH DEL_ET02.
*
  CLEAR ZTHR_ET02.
  SELECT ZMODL ZGRUP ZCODE ZCTXT ZVALD
         ZVAL1 ZVAL2 ZVAL3 ZVAL4 ZVAL5 ZRMRK
         ERDAT ERZET ERNAM AEDAT AEZET AENAM
    INTO CORRESPONDING FIELDS OF TABLE IT_ET02
    FROM ZTHR_ET02 WHERE ZMODL = W_ZMODL
                      AND ZGRUP = W_ZGRUP
                      AND ERDAT > '20000101'.
*
  IF SY-SUBRC = 0.
    SORT IT_ET02 BY ZCODE.
  ELSE.
    W_COUNT = 1.
    DO 50 TIMES.
      IT_ET02-ZMODL = W_ZMODL.
      IT_ET02-ZGRUP = W_ZGRUP.
      IT_ET02-TABIX = W_COUNT.
      APPEND IT_ET02. CLEAR IT_ET02.
      W_COUNT = W_COUNT + 1.
    ENDDO.
  ENDIF.
*
  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE IT_ET02 LINES TC9100-LINES.
ENDFORM.                    " get_data_from_ET02
*&---------------------------------------------------------------------*
*&      Form  init_value_and_call_scr
*&---------------------------------------------------------------------*
FORM INIT_VALUE_AND_CALL_SCR.
  CLEAR IT_STATS. REFRESH IT_STATS.
  IT_STATS-FCODE = 'SAVE'. APPEND IT_STATS.
  IT_STATS-FCODE = 'DELE'. APPEND IT_STATS.
  IT_STATS-FCODE = 'NEWN'. APPEND IT_STATS.
*
  CLEAR: W_ZMODL, W_ZMTXT, W_ZGRUP, W_ZGTXT.
  CLEAR IT_ET02. REFRESH IT_ET02.
  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE IT_ET02 LINES TC9100-LINES.
*
  W_MODES = 0.
  CALL SCREEN 9100.
ENDFORM.                    " init_value_and_call_scr
*&---------------------------------------------------------------------*
*&      Form  delete_line_ET02
*&---------------------------------------------------------------------*
FORM DELETE_LINE_ET02.
  CLEAR IT_ET02.
  READ TABLE IT_ET02 WITH KEY CHKBX = 'X'.
  MOVE-CORRESPONDING IT_ET02 TO DEL_ET02.
  APPEND DEL_ET02. CLEAR DEL_ET02.
*
  DELETE IT_ET02 WHERE CHKBX = 'X'.
  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE IT_ET02 LINES TC9100-LINES.
ENDFORM.                    " delete_line_ET02
*&---------------------------------------------------------------------*
*&      Form  save_ET02_data
*&---------------------------------------------------------------------*
FORM SAVE_ET02_DATA.
  DELETE IT_ET02 WHERE ZCODE = 0.
  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE IT_ET02 LINES TC9100-LINES.
*
  MODIFY ZTHR_ET02 FROM TABLE IT_ET02.
  IF SY-SUBRC = 0.
    DELETE ZTHR_ET02 FROM TABLE DEL_ET02.
    MESSAGE S001 WITH 'DATA SAVED'.
  ELSE.
    MESSAGE E001 WITH 'Data Save Failed'.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " save_ET02_data
*&---------------------------------------------------------------------*
*&      Form  append_new_lines
*&---------------------------------------------------------------------*
FORM APPEND_NEW_LINES.
  DESCRIBE TABLE IT_ET02 LINES W_COUNT.
*
  CLEAR IT_ET02.
  DO 50 TIMES.
    W_COUNT = W_COUNT + 1.
    IT_ET02-ZMODL = W_ZMODL.
    IT_ET02-ZGRUP = W_ZGRUP.
    IT_ET02-TABIX = W_COUNT.
    APPEND IT_ET02. CLEAR IT_ET02.
  ENDDO.
*
  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE IT_ET02 LINES TC9100-LINES.
ENDFORM.                    " append_new_lines
*&---------------------------------------------------------------------*
*&      Form  go_to_9100_screen
*&---------------------------------------------------------------------*
FORM GO_TO_9100_SCREEN.
  CLEAR IT_ET01.
  READ TABLE IT_ET01 WITH KEY CHKBX = 'X'.
*
  IF SY-SUBRC = 0.
    W_ZMODL = IT_ET01-ZMODL.
    W_ZMTXT = IT_ET01-ZMTXT.
    W_ZGRUP = IT_ET01-ZGRUP.
    W_ZGTXT = IT_ET01-ZGTXT.
    PERFORM GET_DATA_FROM_ET02.
    W_MODES = 0.
    CALL SCREEN 9100.
  ELSE.
    MESSAGE W001 WITH 'choose the item'.
  ENDIF.
ENDFORM.                    " go_to_9100_screen
*&---------------------------------------------------------------------*
*&      Form  ascending_sort
*&---------------------------------------------------------------------*
FORM ASCENDING_SORT.
  FIELD-SYMBOLS <TC>  TYPE CXTAB_CONTROL.
  DATA: COLS LIKE LINE OF <TC>-COLS,
        TC_NAME   TYPE DYNFNAM ,
        SORT_DUMY(100) TYPE C,
        SORT_NAME(100) TYPE C.

  TC_NAME = 'TC9000'.

  ASSIGN (TC_NAME) TO <TC>.
  READ TABLE <TC>-COLS INTO COLS WITH KEY SELECTED = 'X'.
  IF SY-SUBRC = 0.
    SPLIT COLS-SCREEN-NAME AT '-' INTO SORT_DUMY SORT_NAME .
    SORT IT_ET01 STABLE BY (SORT_NAME) ASCENDING.
    COLS-SELECTED = ' '.
    MODIFY <TC>-COLS FROM COLS INDEX SY-TABIX.
  ENDIF.
ENDFORM.                    " ascending_sort
*&---------------------------------------------------------------------*
*&      Form  descending_sort
*&---------------------------------------------------------------------*
FORM DESCENDING_SORT.
  FIELD-SYMBOLS <TC>  TYPE CXTAB_CONTROL.
  DATA: COLS LIKE LINE OF <TC>-COLS,
        TC_NAME   TYPE DYNFNAM ,
        SORT_DUMY(100) TYPE C,
        SORT_NAME(100) TYPE C.

  TC_NAME = 'TC9000'.

  ASSIGN (TC_NAME) TO <TC>.
  READ TABLE <TC>-COLS INTO COLS WITH KEY SELECTED = 'X'.
  IF SY-SUBRC = 0.
    SPLIT COLS-SCREEN-NAME AT '-' INTO SORT_DUMY SORT_NAME .
    SORT IT_ET01 STABLE BY (SORT_NAME) DESCENDING.
    COLS-SELECTED = ' '.
    MODIFY <TC>-COLS FROM COLS INDEX SY-TABIX.
  ENDIF.

ENDFORM.                    " descending_sort
