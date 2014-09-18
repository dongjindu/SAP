************************************************************************
* Program Name      : ZMMR_SES_BY_INVOICE
* Creation Date     : 02/07/2013
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zmmr_ses_by_invoicet NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TABLES: essr.
TYPE-POOLS slis .
DATA : BEGIN OF it_itab OCCURS 0,
      xblnr LIKE essr-xblnr,
      ebeln LIKE essr-ebeln,
      ebelp LIKE essr-ebelp,
      txz01 LIKE ekpo-txz01,
      lblni  LIKE essr-lblni,
      qty_r(13),
      qty_o(13),
      qty_d(13),
      amt_r(13),
      amt_o(13),
      amt_d(13),
      tot_amt(13),
      END OF it_itab.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.


DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
      w_fieldname LIKE LINE OF it_fieldname,
      WA_IS_PRINT TYPE LVC_S_PRNT.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt   TYPE   i.

*---- LIST BOX DATA
DATA: xname    TYPE vrm_id,
      xlist    TYPE vrm_values,
      xvalue   LIKE LINE OF xlist,
      BEGIN OF ylist     OCCURS 0,
         key(40) TYPE c,
         text(80) TYPE c,
      END OF ylist      .

*SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
*
*SELECT-OPTIONS : s_matnr FOR lips-matnr,
*                 s_vbeln FOR lips-vbeln,
*                 s_date FOR likp-zzarrdt.
*SELECTION-SCREEN END OF BLOCK block1.
*
*AT SELECTION-SCREEN OUTPUT.
**  PERFORM check_screen.
*
*AT SELECTION-SCREEN.
**  PERFORM check_input_value.
*
*START-OF-SELECTION.
*  PERFORM get_data.
*  IF it_tab[] IS INITIAL.
*    MESSAGE s999 WITH text-m01.
*  ELSE.
*    SORT it_tab BY matnr.
CALL SCREEN 200.

*---------------------------------------------------------------------*
*       FORM get_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_data.
  DATA: l_total LIKE esll-brtwr,
        l_subtotal LIKE esll-brtwr,
        l_text(50),
        l_length TYPE i,
        l_char(1),
        l_int type i.

  FIELD-SYMBOLS: <fs>.

  DATA : BEGIN OF lt_temp OCCURS 0,
         xblnr LIKE essr-xblnr,
         lblni  LIKE essr-lblni,
         packno LIKE  essr-packno,
         sub_packno LIKE esll-sub_packno,
         ebeln LIKE essr-ebeln,
         ebelp LIKE essr-ebelp,
         txz01 LIKE ekpo-txz01,
      END OF lt_temp.

  DATA : BEGIN OF lt_esll OCCURS 0,
         packno LIKE esll-packno,
         introw LIKE esll-introw,
         srvpos LIKE esll-srvpos,
         menge LIKE esll-menge,
         brtwr LIKE esll-brtwr,
         END OF lt_esll.

  SELECT xblnr lblni a~packno sub_packno a~ebeln a~ebelp b~txz01
     INTO CORRESPONDING FIELDS OF TABLE lt_temp
     FROM essr AS a
    INNER JOIN ekpo AS b
    ON a~ebeln = b~ebeln
    AND a~ebelp = b~ebelp
    INNER JOIN esll AS c
    ON a~packno = c~packno
    WHERE xblnr = essr-xblnr.

  LOOP AT lt_temp.
    MOVE-CORRESPONDING lt_temp TO it_itab.
    REFRESH: lt_esll.
    CLEAR: l_subtotal, l_int.
    SELECT packno introw srvpos menge brtwr
      INTO TABLE lt_esll
      FROM esll
      WHERE packno = lt_temp-sub_packno.
    LOOP AT lt_esll.
      CLEAR: l_length, l_char, l_text.
      UNASSIGN <fs>.
      IF NOT lt_esll-srvpos IS INITIAL.
        l_length = strlen( lt_esll-srvpos ).
        l_length = l_length - 1.
        l_char = lt_esll-srvpos+l_length(1).
        CONCATENATE 'IT_ITAB-QTY_' l_char INTO l_text.
        ASSIGN (l_text) TO <fs>.
        IF sy-subrc = 0.
          l_int = lt_esll-menge.
          <fs> = l_int.
        ENDIF.
        UNASSIGN <fs>.
        CONCATENATE 'IT_ITAB-AMT_' l_char INTO l_text.
        ASSIGN (l_text) TO <fs>.
        IF sy-subrc = 0.
          <fs> = lt_esll-brtwr.
        ENDIF.
        l_subtotal = l_subtotal + lt_esll-brtwr.
        UNASSIGN <fs>.
      ENDIF.
    ENDLOOP.
    it_itab-tot_amt = l_subtotal.
    APPEND it_itab.
    l_total =  l_total +  l_subtotal.
  ENDLOOP.
  CLEAR: it_itab.
  it_itab-xblnr = 'Total'.
  it_itab-tot_amt = l_total.
  APPEND it_itab.
  SORT it_itab BY xblnr ebeln ebelp.
ENDFORM.                    "get_data
*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.

ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM check_screen.
*  LOOP AT SCREEN.
*    IF screen-name = 'P_EXCEL'.
*      screen-input = 0.
*      screen-invisible = 1.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
*ENDFORM.                    " check_screen

*---------------------------------------------------------------------*
*       FORM display_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 200.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_ITAB'.
    PERFORM assign_itab_to_alv.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.
ENDMODULE.                 " display_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'DISPLAY'.
      PERFORM get_data.
*      perform alv_display.
    WHEN 'PRINT'.
*      PERFORM print_report.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      IS_PRINT             = WA_IS_PRINT
      i_save               = wa_save
      is_variant           = wa_variant
      i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_itab[]
      it_sort              = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  CLEAR: w_repid.
  CREATE OBJECT grid_container
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = grid_container
      i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*  WA_IS_PRINT-PRNT_TITLE = 'X'.
*  WA_IS_PRINT-PRINT = 'X'.
*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_sortcat_display.

  it_sort-spos           = 1.
  it_sort-fieldname      = 'XBLNR'.
  it_sort-up             = 'X'.
  it_sort-subtot         = 'X'.
  APPEND it_sort.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname.
*        lw_waers LIKE t001-waers,

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                  'S' 'XBLNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Invoice No.',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'EBELN'       ' ',
                                  ' ' 'COLTEXT'     'PO Number',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'COLTEXT'     'Item',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'TXZ01'     ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'LBLNI'        ' ',
                                  ' ' 'COLTEXT'     'SES Number',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'QTY_R'       ' ',
                                  ' ' 'COLTEXT'     'REG Qty',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'QTY_O'       ' ',
                                  ' ' 'COLTEXT'     'OVT Qty',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'QTY_D'       ' ',
                                  ' ' 'COLTEXT'     'DBL Qty',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'AMT_R'       ' ',
                                  ' ' 'COLTEXT'     'REG Amt',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'AMT_O'       ' ',
                                  ' ' 'COLTEXT'     'OVT Amt',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'AMT_D'       ' ',
                                  ' ' 'COLTEXT'     'DBL Amt',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'TOT_AMT'       ' ',
                                  ' ' 'COLTEXT'     'Total Amt',
*                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '14'.


ENDFORM.                    "build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_report .
  WA_IS_PRINT-PRNT_TITLE = 'X'.
  WA_IS_PRINT-PRINT = 'X'.
  PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_ITAB'.
    PERFORM assign_itab_to_alv.
    WA_IS_PRINT-PRINT = ' '.
    LEAVE PROGRAM.
  ENDFORM.                    " PRINT_REPORT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_help OUTPUT.
  PERFORM list_box_invocie.
ENDMODULE.                 " SEARCH_HELP  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_INVOCIE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_box_invocie .
  DATA: BEGIN OF lt_temp OCCURS 0,
        xblnr LIKE essr-xblnr,
        END OF lt_temp.

  DATA: l_date LIKE sy-datum.

  l_date = sy-datum - 1095.
  SELECT xblnr
    INTO TABLE lt_temp
    FROM essr
    WHERE erdat > l_date
    GROUP BY xblnr.

  LOOP AT lt_temp.
    xvalue-text = lt_temp-xblnr.
    APPEND xvalue TO xlist .
  ENDLOOP.

 PERFORM list_box_function USING 'ESSR-XBLNR'.

ENDFORM.                    " LIST_BOX_INVOCIE
*&---------------------------------------------------------------------*
*&      Form  list_box_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0617   text
*----------------------------------------------------------------------*
FORM list_box_function USING   p_list_name .
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = p_list_name  " list box
      values          = xlist
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
ENDFORM.                    " list_box_function
**&---------------------------------------------------------------------
*
**&      Form  ALV_DISPLAY
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*form ALV_DISPLAY .
*   PERFORM create_container_n_object.
*    PERFORM set_attributes_alv_grid.
*    PERFORM build_sortcat_display.
*    PERFORM build_field_catalog USING 'IT_ITAB'.
*    PERFORM assign_itab_to_alv.
*endform.                    " ALV_DISPLAY
