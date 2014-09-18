*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PAIO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      CLEAR: w_new, w_refresh.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR: w_new, w_refresh.
      LEAVE TO SCREEN 0.
    WHEN 'INBDEL'.
      PERFORM call_vl06i.
    WHEN 'MIT'.
      PERFORM call_mit.
    WHEN 'WMSTK'.
      PERFORM call_ls24.
    WHEN 'MD04'.
      PERFORM call_md04.
    WHEN 'MB51'.
      PERFORM call_mb51.
    WHEN 'MD05'.
      PERFORM call_md05.
    WHEN 'IDOC-ASN'.
      PERFORM call_zmmr203_asn.
    WHEN 'IDOC-JIT'.
      PERFORM call_zmmr203_jit.
    WHEN 'REFRESH'.
      w_refresh = 'X'.
      LEAVE TO SCREEN 0.
    WHEN 'MM02'.
      PERFORM call_mm02.
    WHEN 'MM03'.
      PERFORM call_mm03.
    WHEN 'ME01'.
      PERFORM call_me01.
    WHEN 'COGI'.
      PERFORM call_cogi.
    WHEN 'CRPO'.
      PERFORM call_crpo.
*      PERFORM REFRESH_DATA.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  CALL_MD04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  call_md04.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD it_output-matnr.
  SET PARAMETER ID 'BERID' FIELD it_output-werks.
  SET PARAMETER ID 'WRK' FIELD it_output-werks.
  CALL TRANSACTION 'MD04' AND SKIP FIRST SCREEN.

ENDFORM.                                                    " CALL_MD04
*&---------------------------------------------------------------------*
*&      Form  call_md05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_md05.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD it_output-matnr.
  SET PARAMETER ID 'BERID' FIELD it_output-werks.
  SET PARAMETER ID 'WRK' FIELD it_output-werks.
  CALL TRANSACTION 'MD05' AND SKIP FIRST SCREEN.

ENDFORM.                                                    " call_md05
*&---------------------------------------------------------------------*
*&      Form  call_zmmr203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_zmmr203_asn.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.
  RANGES: lr_matnr FOR mara-matnr,
         lr_lifnr FOR lfa1-lifnr.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  lr_matnr-sign = 'I'.
  lr_matnr-option = 'EQ'.
  lr_matnr-low = it_output-matnr.
  APPEND lr_matnr.

  lr_lifnr-sign = 'I'.
  lr_lifnr-option = 'EQ'.
  lr_lifnr-low = it_output-lifnr.
  APPEND lr_lifnr.
*  SET PARAMETER ID 'MAT' FIELD it_output-matnr.
*  SET PARAMETER ID 'LIF' FIELD it_output-lifnr.
*  SET PARAMETER ID 'MES' FIELD 'DESADV'.
  SUBMIT zrmm_idoc_list    "via selection-screen
         WITH s_matnr IN lr_matnr
         WITH s_lifnr IN lr_lifnr
         AND RETURN.
*  CALL TRANSACTION 'ZMMR203'.
ENDFORM.                    " call_zmmr203_asn
*&---------------------------------------------------------------------*
*&      Form  call_VL06I
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_vl06i.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD it_output-matnr.
  SET PARAMETER ID 'BERID' FIELD it_output-werks.
  SET PARAMETER ID 'WRK' FIELD it_output-werks.
  CALL TRANSACTION 'VL06I'.

ENDFORM.                    " call_VL06I
*&---------------------------------------------------------------------*
*&      Form  call_LS24
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_ls24.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD it_output-matnr.
  SET PARAMETER ID 'BERID' FIELD it_output-werks.
  SET PARAMETER ID 'WRK' FIELD it_output-werks.
  CALL TRANSACTION 'LS24'.

ENDFORM.                                                    " call_LS24


*---------------------------------------------------------------------*
*       FORM call_mb51                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM call_mb51.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD it_output-matnr.
  SET PARAMETER ID 'BERID' FIELD it_output-werks.
  SET PARAMETER ID 'WRK' FIELD it_output-werks.
  CALL TRANSACTION 'MB51'.   " AND SKIP FIRST SCREEN.

ENDFORM.                                                    " call_mb51
*&---------------------------------------------------------------------*
*&      Form  call_MIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_mit.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.
  RANGES: lr_matnr FOR mara_matnr.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
*
  LOOP AT lt_rows.
    READ TABLE it_output INDEX lt_rows-index.
    lr_matnr-sign = 'I'.
    lr_matnr-option = 'EQ'.
    lr_matnr-low = it_output-matnr.
    APPEND lr_matnr.
    CLEAR: lr_matnr.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM lr_matnr.

  SUBMIT zrmm_mit WITH s_matnr IN lr_matnr
     AND RETURN.

* SET PARAMETER ID 'MAT' FIELD it_output-matnr.
* CALL TRANSACTION 'ZRMM_MIT' AND SKIP FIRST SCREEN.

ENDFORM.                    " call_MIT
*&---------------------------------------------------------------------*
*&      Form  call_MM02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_mm02.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD it_output-matnr.
  CALL TRANSACTION 'MM02' AND SKIP FIRST SCREEN.

ENDFORM.                                                    " call_MM02
*&---------------------------------------------------------------------*
*&      Form  call_MM03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_mm03.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD it_output-matnr.
  CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
ENDFORM.                                                    " call_MM03
*&---------------------------------------------------------------------*
*&      Form  call_ME01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_me01.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD it_output-matnr.
  SET PARAMETER ID 'WRK' FIELD it_output-werks.
  CALL TRANSACTION 'ME01' AND SKIP FIRST SCREEN.

ENDFORM.                                                    " call_ME01
*&---------------------------------------------------------------------*
*&      Form  call_CRPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_crpo.
  DATA: lt_columns TYPE lvc_t_col WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_index TYPE i,
        l_text(30),
        l_rcno(2) TYPE n,
        l_pur_group LIKE usr05-parva.

  DATA: poheader LIKE bapimepoheader,
        poheaderx LIKE bapimepoheaderx,
        poitem LIKE TABLE OF bapimepoitem WITH HEADER LINE,
        poitemx LIKE TABLE OF bapimepoitemx  WITH HEADER LINE,
        return LIKE TABLE OF bapiret2  WITH HEADER LINE,
        schedule LIKE TABLE OF bapimeposchedule  WITH HEADER LINE,
        schedulex LIKE TABLE OF bapimeposchedulx WITH HEADER LINE,
        ponumber LIKE bapimepoheader-po_number.

  CALL METHOD alv_grid->get_selected_columns
           IMPORTING et_index_columns = lt_columns[].
*                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_columns INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  CASE lt_columns-fieldname.
    WHEN 'RQTY_01'.
      l_rcno = '01'.
    WHEN 'RQTY_02'.
      l_rcno = '02'.
    WHEN 'RQTY_03'.
      l_rcno = '03'.
    WHEN 'RQTY_04'.
      l_rcno = '04'.
    WHEN 'RQTY_05'.
      l_rcno = '05'.
    WHEN 'RQTY_06'.
      l_rcno = '06'.
    WHEN 'RQTY_07'.
      l_rcno = '07'.
    WHEN 'RQTY_08'.
      l_rcno = '08'.
    WHEN 'RQTY_09'.
      l_rcno = '09'.
    WHEN 'RQTY_10'.
      l_rcno = '10'.
    WHEN 'RQTY_11'.
      l_rcno = '11'.
    WHEN 'RQTY_12'.
      l_rcno = '12'.
    WHEN 'RQTY_13'.
      l_rcno = '13'.
    WHEN 'RQTY_14'.
      l_rcno = '14'.
    WHEN 'RQTY_15'.
      l_rcno = '15'.
    WHEN 'RQTY_16'.
      l_rcno = '16'.
    WHEN 'RQTY_17'.
      l_rcno = '17'.
    WHEN 'RQTY_18'.
      l_rcno = '18'.
    WHEN 'RQTY_19'.
      l_rcno = '19'.
    WHEN 'RQTY_20'.
      l_rcno = '20'.
    WHEN 'RQTY_21'.
      l_rcno = '21'.
  ENDCASE.
  IF r01 = 'X'.
    READ TABLE it_day WITH KEY seq = l_rcno.
    IF sy-subrc = 0.
      p_del_date = it_day-datum.
    ENDIF.
  ELSE.
    READ TABLE it_week WITH KEY seq = l_rcno.
    p_del_date = it_week-datum.
  ENDIF.

  CONCATENATE 'IT_OUTPUT-RQTY_' l_rcno INTO l_text.
  ASSIGN (l_text) TO <fs01>.
  IF sy-subrc = 0.

    PERFORM prepare_for_creation.
    CALL SCREEN 0210.

*
*    poheader-comp_code = 'H201'.
*    poheader-doc_type = 'KD'.
*
*    SELECT SINGLE parva INTO l_pur_group
*     FROM usr05
*     WHERE bname = sy-uname
*       AND parid = 'EKG'.
*
*    CONDENSE l_pur_group.
*    poheader-pur_group = l_pur_group.
*    poheader-creat_date = sy-datum.
*    READ TABLE it_output INDEX 1.
*    poheader-vendor = it_output-lifnr.
*    poheader-purch_org = 'PU01'.
*
*    poheaderx-comp_code = 'X'.
*    poheaderx-doc_type = 'X'.
*    poheaderx-creat_date = 'X'.
*    poheaderx-vendor = 'X'.
*    poheaderx-purch_org = 'X'.
*    poheaderx-pur_group = 'X'.
*
*    CLEAR: l_index.
*    LOOP AT it_output.
*      IF NOT it_output-matnr IS INITIAL.
*        poitem-material = it_output-matnr.
*        poitem-ematerial = it_output-matnr.
*        l_index = l_index + 1.
*        poitem-po_item = l_index.
*        poitem-plant = 'P001'.
*        poitem-stge_loc = 'P400'.
*        SELECT SINGLE meins INTO poitem-po_unit
*         FROM  mara
*         WHERE matnr = it_output-matnr.
*        poitem-po_unit_iso = poitem-po_unit.
*      ENDIF.
*      IF it_output-matnr IS INITIAL AND
*           it_output-seq = '3'.
*        poitem-quantity = - <fs01>.
*        APPEND poitem.
*
*        poitemx-po_item =  l_index.
*        poitemx-material = 'X'.
*        poitem-ematerial = 'X'.
*        poitemx-plant = 'X'.
*        poitemx-stge_loc = 'X'.
*        poitemx-quantity = 'X'.
*        poitemx-po_unit = 'X'.
*        poitemx-po_unit_iso = 'X'.
*        APPEND poitemx.
*        CLEAR: poitemx.
*
*        schedule-po_item =  l_index.
*        schedule-sched_line  =  l_index.
*        schedule-delivery_date = p_del_date.
*        poitem-quantity = <fs01>.
*        APPEND schedule.
*        CLEAR: schedule.
*
*        schedulex-po_item =  l_index.
*        schedulex-sched_line  =  l_index.
*        schedulex-delivery_date = 'X'.
*        schedulex-quantity = 'X'.
*        APPEND schedulex.
*        CLEAR: schedulex.
*
*      ENDIF.
*    ENDLOOP.
*
*    CALL FUNCTION 'BAPI_PO_CREATE1'
*      EXPORTING
*        poheader                     = poheader
*        poheaderx                    = poheaderx
**     POADDRVENDOR                 =
**     TESTRUN                      =
**     MEMORY_UNCOMPLETE            =
**     MEMORY_COMPLETE              =
**     NO_MESSAGING                 =
**     NO_MESSAGE_REQ               =
**     NO_AUTHORITY                 =
**     NO_PRICE_FROM_PO             =
*     IMPORTING
*       exppurchaseorder             = ponumber
**     EXPHEADER                    =
*    TABLES
*       return                       = return
*       poitem                       = poitem
*       poitemx                      = poitemx
**     POADDRDELIVERY               =
*       poschedule                   = schedule
*       poschedulex                  = schedulex
**     POACCOUNT                    =
**     POACCOUNTPROFITSEGMENT       =
**     POACCOUNTX                   =
**     POCONDHEADER                 =
**     POCONDHEADERX                =
**     POCOND                       =
**     POCONDX                      =
**     POLIMITS                     =
**     POCONTRACTLIMITS             =
**     POSERVICES                   =
**     POSRVACCESSVALUES            =
**     POSERVICESTEXT               =
**     EXTENSIONIN                  =
**     EXTENSIONOUT                 =
**     POTEXTHEADER                 =
**     POTEXTITEM                   =
**     POPARTNER                    =
*              .
*
*    READ TABLE return WITH KEY type = 'S'
*                                id = '06'
*                                number = '017'.
*    IF sy-subrc = 0.
*      MESSAGE s000(zz) WITH text-t02 ponumber.
*      COMMIT WORK.
*    ELSE.
*      READ TABLE return WITH KEY type = 'E'.
*      MESSAGE e000(zz) WITH text-t01 return-message.
*    ENDIF.
  ELSE.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
ENDFORM.                    " call_CRPO
*&---------------------------------------------------------------------*
*&      Form  prepare_for_creation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_for_creation.
  DATA: l_round_qty LIKE it_po_data-quantity,
        l_rem LIKE it_po_data-quantity.
  CLEAR: it_po_data, it_po_data[].
  LOOP AT it_output.
    IF NOT it_output-matnr IS INITIAL.
      it_po_data-vendor = it_output-lifnr.
      it_po_data-matl_group = it_output-matkl.
      it_po_data-material = it_output-matnr.
      it_po_data-plant = 'P001'.
*      it_po_data-stge_loc = 'P400'.
      SELECT SINGLE meins INTO it_po_data-po_unit
       FROM  mara
       WHERE matnr = it_output-matnr.
      SELECT SINGLE maktx INTO it_po_data-desc
       FROM  makt
       WHERE matnr = it_output-matnr
         AND spras = 'E'.
    ENDIF.
    IF it_output-matnr IS INITIAL AND
         it_output-seq = '3'.
      it_po_data-quantity = - <fs01>.
      IF it_po_data-quantity > 0.
        SELECT SINGLE bstrf INTO l_round_qty
        FROM marc
        WHERE matnr = it_po_data-material
          AND werks = it_po_data-plant.
        IF l_round_qty > 0.
          IF it_po_data-quantity <= l_round_qty.
            it_po_data-order_qty = l_round_qty.
          ELSE.
            l_rem = it_po_data-quantity MOD l_round_qty.
            IF l_rem = 0.
              it_po_data-order_qty = it_po_data-quantity.
            ELSE.
              it_po_data-order_qty = ( it_po_data-quantity
                                DIV l_round_qty ) * l_round_qty
                                + l_round_qty.
            ENDIF.
          ENDIF.
          it_po_data-round_qty = l_round_qty.
        ELSE.
          it_po_data-order_qty = it_po_data-quantity.
        ENDIF.
        APPEND it_po_data.
      ENDIF.
      CLEAR: it_po_data.
    ENDIF.
  ENDLOOP.
  SORT it_po_data BY vendor matl_group MATERIAL PLANT.
ENDFORM.                    " prepare_for_creation

*---------------------------------------------------------------------*
*       MODULE user_command_0210 INPUT                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE
user_command_0210 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      CLEAR: w_new, w_refresh.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR: w_new, w_refresh.
      LEAVE TO SCREEN 0.
    WHEN 'INCQTY'.
      PERFORM call_change_qty USING '+'.
    WHEN 'DECQTY'.
      PERFORM call_change_qty USING '-'.
    WHEN 'CREPO'.
      PERFORM call_create_po.
    WHEN 'RESETQTY'.
      PERFORM call_reset_qty.
    WHEN 'REFRESH'.
      PERFORM refresh_po.
    WHEN 'ME23N'.
      PERFORM CALL_ME23N.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  REFRESH_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_po.
  CALL METHOD alv_grid_po->refresh_table_display
  EXPORTING
*       IS_STABLE      =
    i_soft_refresh =  'X'
  EXCEPTIONS
    finished       = 1
    OTHERS         = 2.
  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " REFRESH_PO
*&---------------------------------------------------------------------*
*&      Form  call_INCREASE_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_change_qty USING p_sign.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid_po->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  LOOP AT lt_rows.
    READ TABLE it_po_data  INDEX lt_rows-index.
    IF sy-subrc = 0.
      IF p_sign = '+'.
        it_po_data-order_qty =  it_po_data-order_qty +
                                it_po_data-round_qty.
        MODIFY it_po_data INDEX lt_rows-index TRANSPORTING order_qty.
      ELSE.
        IF it_po_data-order_qty > it_po_data-round_qty.
          it_po_data-order_qty =  it_po_data-order_qty -
                                  it_po_data-round_qty.
          MODIFY it_po_data INDEX lt_rows-index TRANSPORTING order_qty.
        ELSE.
          MESSAGE i000(zz) WITH text-m14.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CALL METHOD alv_grid_po->refresh_table_display
 EXPORTING
*       IS_STABLE      =
   i_soft_refresh =  'X'
 EXCEPTIONS
   finished       = 1
   OTHERS         = 2.
  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " call_INCREASE_QTY
*&---------------------------------------------------------------------*
*&      Form  call_reset_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_reset_qty.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i,
        l_rem LIKE it_po_data-quantity.

  CALL METHOD alv_grid_po->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  LOOP AT lt_rows.
    READ TABLE it_po_data  INDEX lt_rows-index.
    IF sy-subrc = 0.
      IF it_po_data-round_qty > 0.
        IF it_po_data-quantity <= it_po_data-round_qty.
          it_po_data-order_qty = it_po_data-round_qty.
        ELSE.
          l_rem = it_po_data-quantity MOD
                  it_po_data-round_qty.
          IF l_rem = 0.
            it_po_data-order_qty = it_po_data-quantity.
          ELSE.
            it_po_data-order_qty = ( it_po_data-quantity
                              DIV it_po_data-round_qty ) *
                              it_po_data-round_qty
                              + it_po_data-round_qty.
          ENDIF.
        ENDIF.
      ELSE.
        it_po_data-order_qty = it_po_data-quantity.
      ENDIF.
      MODIFY it_po_data INDEX lt_rows-index TRANSPORTING order_qty.
    ENDIF.
  ENDLOOP.

  CALL METHOD alv_grid_po->refresh_table_display
 EXPORTING
*       IS_STABLE      =
   i_soft_refresh =  'X'
 EXCEPTIONS
   finished       = 1
   OTHERS         = 2.
  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " call_reset_qty
*&---------------------------------------------------------------------*
*&      Form  call_CREATE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_create_po.
  AUTHORITY-CHECK OBJECT 'M_BEST_EKO'
           ID 'ACTVT' FIELD '01'
           ID 'EKORG' dummy.
  IF SY-SUBRC NE 0.
     MESSAGE E009 with 'No-Authorization to create PO'.
  ENDIF.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.
  DATA: it_po_bapi LIKE TABLE OF it_po_data WITH HEADER LINE.
  DATA: BEGIN OF lw_po_group OCCURS 0,
       vendor LIKE ekko-lifnr,
       matl_group LIKE mara-matkl,
       END OF lw_po_group.

  DATA: l_index TYPE i,
        l_text(30),
        l_rcno(2) TYPE n,
*        p_del_date LIKE sy-datum,
        l_pur_group LIKE usr05-parva.
*** bapi_po_create1
*  DATA: poheader LIKE bapimepoheader,
*        poheaderx LIKE bapimepoheaderx,
*        poitem LIKE TABLE OF bapimepoitem WITH HEADER LINE,
*        poitemx LIKE TABLE OF bapimepoitemx  WITH HEADER LINE,
*        return LIKE TABLE OF bapiret2  WITH HEADER LINE,
*        schedule LIKE TABLE OF bapimeposchedule  WITH HEADER LINE,
*        schedulex LIKE TABLE OF bapimeposchedulx WITH HEADER LINE,
*        ponumber LIKE bapimepoheader-po_number.
*** bapi_po_create
  DATA: po_header LIKE bapiekkoc,
        po_items LIKE TABLE OF bapiekpoc WITH HEADER LINE,
        po_item_schedules LIKE TABLE OF bapieket  WITH HEADER LINE,
        ponumber LIKE bapiekkoc-po_number,
        return LIKE TABLE OF bapireturn WITH HEADER LINE.

  CALL METHOD alv_grid_po->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  LOOP AT lt_rows.
    READ TABLE it_po_data  INDEX lt_rows-index.
    IF sy-subrc = 0.
      it_po_bapi = it_po_data.
      APPEND it_po_bapi.
      lw_po_group-vendor = it_po_bapi-vendor.
      lw_po_group-matl_group = it_po_bapi-matl_group.
      COLLECT lw_po_group.
      CLEAR:it_po_bapi, it_po_data,lw_po_group.
    ENDIF.
  ENDLOOP.

  SELECT SINGLE parva INTO l_pur_group
  FROM usr05
     WHERE bname = sy-uname
       AND parid = 'EKG'.
  CONDENSE l_pur_group.

*  poheader-comp_code = 'H201'.
*  poheader-doc_type = 'KD'.
*  poheader-pur_group = l_pur_group.
*  poheader-creat_date = sy-datum.
*  poheader-purch_org = 'PU01'.
*
*  poheaderx-comp_code = 'X'.
*  poheaderx-doc_type = 'X'.
*  poheaderx-creat_date = 'X'.
*  poheaderx-vendor = 'X'.
*  poheaderx-purch_org = 'X'.
*  poheaderx-pur_group = 'X'.

  po_header-co_code = 'H201'.
  po_header-doc_type = 'KD'.
  po_header-pur_group = l_pur_group.
** changed by Furong on 03/17/10
*  po_header-doc_date = sy-datum.
  po_header-doc_date = w_prd_date.
** End of change
  po_header-purch_org = 'PU01'.

  SORT it_po_bapi BY vendor matl_group.
  LOOP AT lw_po_group.
*    poheader-vendor = lw_po_group-vendor.
    po_header-vendor = lw_po_group-vendor.
    CLEAR: l_index.
*    CLEAR: poitem, poitem[], poitemx, poitemx[],
*           schedule, schedule[], schedulex, schedulex[].
    CLEAR: po_items, po_items[], po_item_schedules, po_item_schedules[].
    CLEAR: return, return[], ponumber.
    LOOP AT it_po_bapi WHERE vendor = lw_po_group-vendor
                        AND matl_group = lw_po_group-matl_group.

      po_items-material = it_po_bapi-material.
      po_items-pur_mat = it_po_bapi-material.
      l_index = l_index + 1.
      po_items-po_item = l_index.
      po_items-plant = it_po_bapi-plant.
*        poitem-stge_loc = 'P400'.
      po_items-unit = it_po_bapi-po_unit.
*      poitem-po_unit_iso = poitem-po_unit.
*      poitem-quantity = it_po_bapi-order_qty.
      APPEND po_items.
      CLEAR: po_items.

*      poitemx-po_item =  l_index.
*      poitemx-material = 'X'.
*      poitem-ematerial = 'X'.
*      poitemx-plant = 'X'.
*      poitemx-stge_loc = 'X'.
*      poitemx-quantity = 'X'.
*      poitemx-po_unit = 'X'.
*      poitemx-po_unit_iso = 'X'.
*      APPEND poitemx.
*      CLEAR: poitemx.
*
*      schedule-po_item =  l_index.
*      schedule-sched_line  =  l_index.
*      schedule-delivery_date = p_del_date.
*      schedule-quantity = poitem-quantity.
*      APPEND schedule.
*      CLEAR: schedule.

      po_item_schedules-po_item =  l_index.
      po_item_schedules-serial_no  =  l_index.
      po_item_schedules-deliv_date = p_del_date.
      po_item_schedules-quantity = it_po_bapi-order_qty.
      APPEND po_item_schedules.
      CLEAR: po_item_schedules.

*      schedulex-po_item =  l_index.
*      schedulex-sched_line  =  l_index.
*      schedulex-delivery_date = 'X'.
*      schedulex-quantity = 'X'.
*      APPEND schedulex.
*      CLEAR: schedulex.

    ENDLOOP.
*
*    CALL FUNCTION 'BAPI_PO_CREATE1'
*      EXPORTING
*        poheader                     = poheader
*        poheaderx                    = poheaderx
**     POADDRVENDOR                 =
**     TESTRUN                      =
**     MEMORY_UNCOMPLETE            =
**     MEMORY_COMPLETE              =
**     NO_MESSAGING                 =
**     NO_MESSAGE_REQ               =
**     NO_AUTHORITY                 =
**     NO_PRICE_FROM_PO             =
*     IMPORTING
*       exppurchaseorder             = ponumber
**     EXPHEADER                    =
*    TABLES
*       return                       = return
*       poitem                       = poitem
*       poitemx                      = poitemx
**     POADDRDELIVERY               =
*       poschedule                   = schedule
*       poschedulex                  = schedulex
**     POACCOUNT                    =
**     POACCOUNTPROFITSEGMENT       =
**     POACCOUNTX                   =
**     POCONDHEADER                 =
**     POCONDHEADERX                =
**     POCOND                       =
**     POCONDX                      =
**     POLIMITS                     =
**     POCONTRACTLIMITS             =
**     POSERVICES                   =
**     POSRVACCESSVALUES            =
**     POSERVICESTEXT               =
**     EXTENSIONIN                  =
**     EXTENSIONOUT                 =
**     POTEXTHEADER                 =
**     POTEXTITEM                   =
**     POPARTNER                    =
*              .
*
*    READ TABLE return WITH KEY type = 'S'
*                                id = '06'
*                                number = '017'.


    CALL FUNCTION 'BAPI_PO_CREATE'
      EXPORTING
        po_header                        = po_header
*   PO_HEADER_ADD_DATA               =
*   HEADER_ADD_DATA_RELEVANT         =
*   PO_ADDRESS                       =
*   SKIP_ITEMS_WITH_ERROR            = 'X'
*   ITEM_ADD_DATA_RELEVANT           =
     IMPORTING
        purchaseorder                    = ponumber
      TABLES
        po_items                         = po_items
*   PO_ITEM_ADD_DATA                 =
        po_item_schedules                = po_item_schedules
*   PO_ITEM_ACCOUNT_ASSIGNMENT       =
*   PO_ITEM_TEXT                     =
        return                           = return
*   PO_LIMITS                        =
*   PO_CONTRACT_LIMITS               =
*   PO_SERVICES                      =
*   PO_SRV_ACCASS_VALUES             =
*   PO_SERVICES_TEXT                 =
*   PO_BUSINESS_PARTNER              =
*   EXTENSIONIN                      =
*   POADDRDELIVERY                   =
              .

    READ TABLE return WITH KEY type = 'S'
                               code = '06017'.

    IF sy-subrc = 0.
      LOOP AT it_po_bapi WHERE vendor = lw_po_group-vendor
                          AND matl_group = lw_po_group-matl_group.

        LOOP AT it_po_data WHERE vendor = lw_po_group-vendor
                           AND matl_group = lw_po_group-matl_group
                           AND  material = it_po_bapi-material
                           AND plant = it_po_bapi-plant.
          it_po_data-po_no = ponumber.
          MODIFY it_po_data.
        ENDLOOP.
      ENDLOOP.
      MESSAGE s000(zz) WITH text-t02 ponumber.
      COMMIT WORK.
    ELSE.
      READ TABLE return WITH KEY type = 'E'.
      MESSAGE e000(zz) WITH text-t01 return-message.
    ENDIF.
    CLEAR: lw_po_group.
  ENDLOOP.
ENDFORM.                    " call_CREATE_PO

FORM call_me23n.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid_PO->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_PO_DATA INDEX lt_rows-index.
  IF it_PO_DATA-PO_NO = ' '.
    MESSAGE e000(zz) WITH text-m15.
  ENDIF.
  SET PARAMETER ID 'BES' FIELD it_PO_DATA-PO_NO.
  CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  call_zmmr203_jit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_zmmr203_jit.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.
  RANGES: lr_matnr FOR mara-matnr,
         lr_lifnr FOR lfa1-lifnr.

  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  lr_matnr-sign = 'I'.
  lr_matnr-option = 'EQ'.
  lr_matnr-low = it_output-matnr.
  APPEND lr_matnr.

  lr_lifnr-sign = 'I'.
  lr_lifnr-option = 'EQ'.
  lr_lifnr-low = it_output-lifnr.
  APPEND lr_lifnr.
*  SET PARAMETER ID 'MAT' FIELD it_output-matnr.
*  SET PARAMETER ID 'LIF' FIELD it_output-lifnr.
*  SET PARAMETER ID 'MES' FIELD 'DESADV'.
  SUBMIT zrmm_idoc_list    "via selection-screen
         with p_mestyp = 'DELJIT'
         WITH s_matnr IN lr_matnr
         WITH s_lifnr IN lr_lifnr
         AND RETURN.
*  CALL TRANSACTION 'ZMMR203'.

ENDFORM.                    " call_zmmr203_jit
