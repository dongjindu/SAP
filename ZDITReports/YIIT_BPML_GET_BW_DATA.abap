*&---------------------------------------------------------------------*
*& Report  YIIT_BPML_GET_BW_DATA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  yiit_bpml_get_bw_data MESSAGE-ID zmpp.

TABLES : s021.

DATA : BEGIN OF it_month OCCURS 0,
      spmon LIKE s021-spmon,
       END OF it_month.

DATA : l_date LIKE sy-datum.

DATA : it_data LIKE zthrappusge OCCURS 0 WITH HEADER LINE.
DATA : it_save LIKE zthrappusge OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECT-OPTIONS : s_month FOR s021-spmon OBLIGATORY.




START-OF-SELECTION.
  CLEAR : it_month[], it_month.

  it_month-spmon  = s_month-high.
  APPEND it_month. CLEAR it_month.

  IF s_month-high <> s_month-low.
    DO 100 TIMES.
      IF sy-index = 1.
        CONCATENATE s_month-high '01' INTO   l_date .
      ELSE.
        CONCATENATE it_month-spmon  '01' INTO l_date.
      ENDIF.

      l_date = l_date - 1.

      IF   l_date+0(6) < s_month-low.
        EXIT.
      ELSE.
        it_month-spmon  = l_date+0(6).
        APPEND it_month.
      ENDIF.
    ENDDO.
  ENDIF.


  DATA : v_dest(30).   "Interface Destination.

  IF  sy-sysid = 'UD1'.
    v_dest = 'UB1CLNT100'.
  ELSEIF sy-sysid = 'UP2'.
    v_dest = 'UB0CLNT100'.
  ELSE.
    MESSAGE s000 WITH 'No RFC connection in this System'.
    STOP.
  ENDIF.

  IF it_month[] IS NOT INITIAL.
    LOOP AT it_month.
      CALL FUNCTION 'YFIT_OB_BPML05' DESTINATION v_dest
        EXPORTING
          i_month  = it_month-spmon
        IMPORTING
          e_return = e_return
        TABLES
          t_data   = it_data.

      IF sy-subrc = 0 AND it_data[] IS NOT INITIAL.
*        APPEND LINES OF it_data TO it_save.
        MODIFY zthrappusge  FROM TABLE it_data.
        IF sy-subrc = 0.
          COMMIT WORK.
          READ TABLE it_data INDEX 1.
          WRITE :/ it_data-ldate+0(6), 'was Saved'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  IF it_save[] IS NOT INITIAL.
**    MODIFY zthrappusge  FROM TABLE it_save.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*    ENDIF.
*  ENDIF.
