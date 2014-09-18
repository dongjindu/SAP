************************************************************************
* Program Name      : ZRQM_SUPP_TO_SUPP_COMPLETE
* Creation Date     : 11/28/12
* Development Request No :
* Addl Documentation:
* Description       : Supplier to Supplier Complete
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zrqm_supp_to_supp_complete NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.
TABLES: lfa1.

TYPE-POOLS: slis, vrm.

*TYPES: BEGIN OF data_ty,
*       qmnum TYPE qmel-qmnum,
*       matnr TYPE qmel-matnr,
*       qmart TYPE qmel-qmart,
*       udate TYPE jcds-udate,
**       rkmng  type qmel-rkmng,
*       erfmg TYPE bseg-erfmg,
*       belnr TYPE bseg-belnr,
*       gjahr TYPE bseg-gjahr,
*       wrbtr TYPE bseg-wrbtr,
*       awkey TYPE bkpf-awkey,
*       END OF data_ty.

*DATA: it_data type SORTED TABLE OF data_ty
*      with UNIQUE key qmnum matnr WITH HEADER LINE.

*DATA: it_data TYPE TABLE OF data_ty WITH HEADER LINE.

DATA: BEGIN OF it_data OCCURS 0,
       qmnum LIKE qmel-qmnum,
       matnr LIKE qmel-matnr,
       qmart LIKE qmel-qmart,
       udate LIKE jcds-udate,
*       rkmng  type qmel-rkmng,
       erfmg LIKE bseg-erfmg,
       belnr LIKE bseg-belnr,
       gjahr LIKE bseg-gjahr,
       wrbtr LIKE bseg-wrbtr,
       awkey LIKE bkpf-awkey,
       mblnr LIKE mkpf-mblnr,
       maktx LIKE makt-maktx,
       END OF it_data.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldname.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt   TYPE i.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_datum FOR sy-datum DEFAULT sy-datum.
PARAMETERS: p_vendor LIKE lfa1-lifnr MODIF ID q1.

SELECTION-SCREEN END OF BLOCK b1.

*AT SELECTION-SCREEN OUTPUT.
INITIALIZATION.
  PERFORM modify_screen_all.

START-OF-SELECTION.

  PERFORM get_data.
  IF it_data[] IS INITIAL.
    MESSAGE i009 WITH 'No data found'.
  ELSE.
    PERFORM display_data.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  TYPES: BEGIN OF ty_objnr,
         objnr LIKE jcds-objnr,
         udate LIKE jcds-udate,
         qmnum LIKE qmel-qmnum,
         END OF ty_objnr.

  TYPES: BEGIN OF ty_bseg,
          belnr LIKE bseg-belnr,
          matnr LIKE bseg-matnr,
          wrbtr LIKE bseg-wrbtr,
          erfmg LIKE bseg-erfmg,
*          MAKTX like makt-MAKTX,
          END OF ty_bseg.

  TYPES: BEGIN OF ty_bkpf,
          awkey LIKE bkpf-awkey,
          belnr LIKE bseg-belnr,
          gjahr LIKE bkpf-gjahr,
          END OF ty_bkpf.

*  DATA: lt_objnr TYPE SORTED TABLE OF ty_objnr
*        WITH UNIQUE KEY qmnum WITH HEADER LINE.

  DATA: lt_objnr TYPE TABLE OF ty_objnr WITH HEADER LINE.

  DATA: lt_bkpf TYPE SORTED TABLE OF ty_bkpf
        WITH UNIQUE KEY awkey WITH HEADER LINE.

  DATA: lt_bseg TYPE SORTED TABLE OF ty_bseg
        WITH UNIQUE KEY belnr matnr WITH HEADER LINE.

*data: lw TYPE ty_OBJNR.

  DATA: BEGIN OF lt_qmnum OCCURS 0,
        qmnum LIKE qmel-qmnum,
        qmart LIKE qmel-qmart,
        matnr LIKE qmel-matnr,
        maktx LIKE makt-maktx,
        END OF lt_qmnum.

  DATA: lt_tline LIKE TABLE OF tline WITH HEADER LINE.

  SELECT objnr udate INTO CORRESPONDING FIELDS OF TABLE lt_objnr
     FROM jcds
    WHERE udate IN s_datum
      AND stat = 'I0072'
      AND inact = ' '.

  CHECK sy-subrc = 0.
  SORT lt_objnr BY objnr udate.
  DELETE ADJACENT DUPLICATES FROM lt_objnr COMPARING objnr udate.

  LOOP AT lt_objnr.
    lt_objnr-qmnum = lt_objnr-objnr+2(12).
    MODIFY lt_objnr.
  ENDLOOP.

  SELECT a~qmnum qmart b~matnr maktx
    INTO TABLE lt_qmnum
    FROM qmfe AS a
    INNER JOIN qmel AS b
    ON a~qmnum = b~qmnum
     INNER JOIN makt AS c
    ON b~matnr = c~matnr
    FOR ALL ENTRIES IN lt_objnr
    WHERE a~qmnum = lt_objnr-qmnum
      AND fecod = '0015'
      AND lifnum = p_vendor
      AND spras = 'EN'..

  CHECK sy-subrc = 0.
  LOOP AT lt_qmnum.
    CLEAR: lt_tline[], lt_tline.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lt_qmnum-qmnum
      IMPORTING
        output = lt_qmnum-qmnum.
    .

    CALL FUNCTION 'Z_GET_MBLNR_FROM_QMNUM'
      EXPORTING
        qmnum = lt_qmnum-qmnum
      TABLES
        tline = lt_tline.
    IF sy-subrc = 0.
      READ TABLE lt_tline INDEX 2.
      it_data-mblnr = lt_tline-tdline+0(10).
      it_data-gjahr = lt_tline-tdline+11(4).
    ENDIF.
    CONCATENATE it_data-mblnr it_data-gjahr INTO it_data-awkey.
    it_data-qmnum = lt_qmnum-qmnum.
    it_data-qmart = lt_qmnum-qmart.
    it_data-matnr = lt_qmnum-matnr.
    it_data-maktx = lt_qmnum-maktx.
    READ TABLE lt_objnr WITH KEY qmnum = it_data-qmnum .
    it_data-udate = lt_objnr-udate.
    APPEND it_data.
    CLEAR: it_data.
  ENDLOOP.

  SELECT awkey belnr gjahr INTO TABLE lt_bkpf
    FROM bkpf
    FOR ALL ENTRIES IN it_data
    WHERE awkey = it_data-awkey.

  CHECK sy-subrc = 0.

  SELECT belnr matnr wrbtr erfmg INTO TABLE lt_bseg
    FROM bseg
    FOR ALL ENTRIES IN lt_bkpf
    WHERE belnr = lt_bkpf-belnr
      AND gjahr = lt_bkpf-gjahr
      AND bschl = '81'.

  LOOP AT it_data.
    READ TABLE lt_bkpf WITH TABLE KEY awkey = it_data-awkey.
    it_data-belnr = lt_bkpf-belnr.
    READ TABLE lt_bseg WITH TABLE KEY belnr = it_data-belnr
                                      matnr = it_data-matnr.
    it_data-erfmg = lt_bseg-erfmg.
    it_data-wrbtr = lt_bseg-wrbtr.
    MODIFY it_data.
    CLEAR: it_data, lt_bkpf,lt_bseg.
  ENDLOOP.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen_all.

  IF sy-uname = 'HIS20037' OR sy-uname = 'HIS20109'
      OR sy-uname = '100311' .
  ELSE.
    SELECT SINGLE lifnr FROM ztqm_vendor_id INTO p_vendor
        WHERE uname = sy-uname.
    IF sy-subrc = 0.
      LOOP AT SCREEN.
        IF screen-group1 = 'Q1'.
          screen-input = 0.
          screen-output = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e009 WITH 'No Authorization to access the function'.
    ENDIF.
  ENDIF.
ENDFORM.                    " MODIFY_SCREEN_ALL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 800.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0800 OUTPUT.
  SET PF-STATUS 'ST800'.
  SET TITLEBAR 'ST800'.

ENDMODULE.                 " STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_DATA'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT
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

ENDFORM.                    " CREATE_CONTAINER_N_OBJECT

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
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

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

*  IT_SORT-SPOS           = 1.
*  IT_SORT-FIELDNAME      = 'MATNR'.
*  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
*  APPEND IT_SORT.

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

                                  'S' 'QMNUM'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Notification',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'QMART'       ' ',
                                  ' ' 'COLTEXT'     'Type',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'UDATE'       ' ',
                                  ' ' 'COLTEXT'     'Comp. Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'COLTEXT'     'Material No',
                                  'E' 'OUTPUTLEN'   '20',

                                 'S' 'MAKTX'       ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'ERFMG'       ' ',
                                  ' ' 'COLTEXT'     'Confirm Qty',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'BELNR'       ' ',
                                  ' ' 'COLTEXT'     'Invoice No',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'GJAHR'       ' ',
                                  ' ' 'COLTEXT'     'Year',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'WRBTR'       ' ',
                                  ' ' 'COLTEXT'     'Amount',
                                  'E' 'OUTPUTLEN'   '13'.

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
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv.
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
      i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_data[]
      it_sort              = it_sort[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0800 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0800  INPUT
