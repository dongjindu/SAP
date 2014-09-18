************************************************************************
* Program Name      : ZMM_MASS_TAX_MAINTENANCE
* Creation Date     : 09/27/12
* Development Request No :
* Addl Documentation:
* Description       : Mass Tax Maintenance
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zmm_mass_tax_maintenance NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TYPE-POOLS: slis, vrm.
TABLES: ekko.
*DATA: IT_DATA LIKE TABLE OF ZTMM_PILOT_MATL WITH HEADER LINE.

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
      w_cnt   TYPE i,
      w_mtart LIKE mara-mtart,
      w_ebeln LIKE ekko-ebeln.

DATA: v_file_table TYPE filetable,
      v_rc TYPE i.

DATA: w_lifnr LIKE ekko-lifnr.

DATA: BEGIN OF it_data OCCURS 0,
      lifnr LIKE ekko-lifnr,
      matnr LIKE ekpo-matnr,
      mwskz LIKE ekpo-mwskz,
      result(1),
      message(80) TYPE c,
      END OF it_data.

DATA: BEGIN OF it_temp OCCURS 0,
       matnr LIKE ekpo-matnr,
       mwskz LIKE ekpo-mwskz,
       END OF it_temp.

DATA: BEGIN OF it_12 OCCURS 0,
       matnr LIKE ekpo-matnr,
       mwskz LIKE ekpo-mwskz,
       END OF it_12.

DATA: BEGIN OF it_bdc_log OCCURS 0,
      lifnr LIKE ekko-lifnr,
      flag(1),
      msg(255),
      END OF it_bdc_log.

DATA: BEGIN OF it_bapi_log OCCURS 0,
      ebeln LIKE ekko-ebeln,
      flag(1),
      msg(255),
      END OF it_bapi_log.

DATA: BEGIN OF it_po OCCURS 0,
      ebeln LIKE ekko-ebeln,
      ebelp LIKE ekpo-ebelp,
      matnr LIKE ekpo-matnr,
      meins LIKE ekpo-meins,
      menge LIKE ekpo-menge,
      knumv LIKE ekko-knumv,
      invqty LIKE ekbe-menge,
      shkzg LIKE ekbe-shkzg,
      mwskz LIKE ekpo-mwskz,
      mwsk1 LIKE konp-mwsk1,
      END OF it_po.

DATA: it_chpo LIKE TABLE OF it_po WITH HEADER LINE.

DATA: intern TYPE alsmex_tabline OCCURS 0 WITH HEADER LINE.

DATA: bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
DATA: messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: ctumode LIKE ctu_params-dismode VALUE 'N'.
*                                      "A: show all dynpros
*                                      "E: show dynpro on error only
*                                      "N: do not display dynpro,
DATA: cupdate LIKE ctu_params-updmode VALUE  'L'.
*                                      "S: synchronously
*                                      "A: asynchronously
*                                      "L: local.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_file TYPE rlgrap-filename MODIF ID tax.
SELECTION-SCREEN SKIP.

SELECT-OPTIONS: s_bsart FOR ekko-bsart MODIF ID po ,
                s_bedat FOR ekko-bedat MODIF ID po,
                s_lifnr FOR ekko-lifnr MODIF ID po.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-u01 FOR FIELD p_upl.
PARAMETERS: p_upl RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 30(30) text-u12 FOR FIELD p_chpo.
PARAMETERS: p_chpo RADIOBUTTON GROUP grp1.
SELECTION-SCREEN  END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen_all.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Input File'
      initial_directory       = 'C:\'
    CHANGING
      file_table              = v_file_table
      rc                      = v_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 4.

  READ TABLE v_file_table INDEX 1 INTO p_file.


START-OF-SELECTION.

  IF p_upl = 'X'.
** Tax code uplaod
    PERFORM upload_data.
    PERFORM process_data.
    IF it_bdc_log[] IS INITIAL.
      MESSAGE s009 WITH 'Successfully Update'.
    ELSE.
      PERFORM display_error_data.
      MESSAGE s009 WITH 'Update Error'.
    ENDIF.
  ELSE.
** Open PO check
    PERFORM get_po_data.
    PERFORM change_po.
    IF it_bapi_log[] IS INITIAL.
      MESSAGE s009 WITH 'Successfully Changed'.
    ELSE.
      PERFORM display_error_data.
      MESSAGE s009 WITH 'Error'.
    ENDIF.
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
FORM process_data.
  data: l_counter type i.
  SORT it_data BY lifnr matnr.
*  LOOP AT IT_DATA.
*     LT_LIFNR-LIFNR = IT_DATA-LIFNR.
*     COLLECT LT_LIFNR.
*  ENDLOOP.
  cleaR: l_counter.
  LOOP AT it_data.
    AT NEW lifnr.
      w_lifnr = it_data-lifnr.
      REFRESH it_temp.
      CLEAR: it_temp.
    ENDAT.
    it_temp-matnr = it_data-matnr.
    it_temp-mwskz = it_data-mwskz.
    APPEND it_temp.
    l_counter = l_counter + 1.
    AT END OF lifnr.
      CLEAR: it_bdc_log.
      PERFORM create_condition.
    ENDAT.
  ENDLOOP.
  IF NOT it_temp[] IS INITIAL.
    CLEAR: it_bdc_log.
    PERFORM create_condition.
  ENDIF.
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

  LOOP AT SCREEN.
    IF screen-group1 EQ 'PO'.
      IF p_upl = 'X'.
        screen-invisible = 1.
        screen-active    = 0.
        screen-input     = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ 'TAX'.
      IF p_chpo = 'X'.
        screen-invisible = 1.
        screen-active    = 0.
        screen-input     = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

*    IF p_upl = 'X'.
*      IF screen-group1 EQ 'PO'.
*        screen-invisible = 1.
*        screen-active    = 0.
*        screen-input     = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ELSE.
*     IF screen-group1 EQ 'TAX'.
*        screen-invisible = 1.
*        screen-active    = 0.
*        screen-input     = 0.
*     ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN_ALL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv.
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
  wa_is_layout-info_fname = 'IF'.
  wa_is_layout-ctab_fname = 'CT'.
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

                                  'S' 'PLANT'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'EBELN'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'PO Number',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Item',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'STEEL_MAT'       ' ',
                                  ' ' 'COLTEXT'     'Steel Material',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'COATING'       ' ',
                                  ' ' 'COLTEXT'     'Coating',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'THICK'       ' ',
                                  ' ' 'COLTEXT'     'Thickness',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'WIDTH'       ' ',
                                  ' ' 'COLTEXT'     'Width',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'LENGTH'       ' ',
                                  ' ' 'COLTEXT'     'Length',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'KIND'       ' ',
                                  ' ' 'COLTEXT'     'Kind of Steel',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'EDGE'       ' ',
                                  ' ' 'COLTEXT'     'Edge',
                                  'E' 'OUTPUTLEN'   '30'.

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
*&---------------------------------------------------------------------*
*&      Form  get_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6019_01>_MATNR  text
*      -->P_<FS_ZTMM_6019_01>_UDATE  text
*      <--P_<FS_ZTMM_6019_01>_LIFNR  text
*----------------------------------------------------------------------*
FORM get_lifnr_fr_sourcelist
               USING    value(im_matnr)
                        value(im_date)
               CHANGING value(ex_lifnr).

  SELECT SINGLE lifnr
   INTO ex_lifnr
   FROM eord
   WHERE matnr = im_matnr AND
         vdatu =< im_date AND
            "Source list record valid from
         bdatu => im_date.
  "Source list record valid to
ENDFORM.                    "get_lifnr_fr_sourcelist

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_data .
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 3
      i_end_row               = 45000
    TABLES
      intern                  = intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc NE 0.
    MESSAGE i000 WITH  'EXEL file upload error!'.
    EXIT.
  ENDIF.

  LOOP AT intern.
    CASE intern-col.
      WHEN 1.
        it_data-lifnr = intern-value.
      WHEN 2.
        it_data-matnr = intern-value.


        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = it_data-matnr
          IMPORTING
            output = it_data-matnr.
      WHEN 3.
        it_data-mwskz = intern-value.
        APPEND it_data.
        CLEAR it_data.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_MEK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_condition.
  DATA: l_count TYPE i.

  CLEAR: l_count.
  LOOP AT it_temp.
    l_count = l_count + 1.
    IF l_count > 12.
      PERFORM call_mek1.
      REFRESH: it_12.
      CLEAR: it_12, l_count.
      it_12 = it_temp.
      APPEND it_12.
    ELSE.
      it_12 = it_temp.
      APPEND it_12.
    ENDIF.
  ENDLOOP.
  IF NOT it_12[] IS INITIAL.
    PERFORM call_mek1.
    REFRESH: it_12.
    CLEAR: it_12.
  ENDIF.
ENDFORM.                                                    "CALL_MEK1
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TCODE    text
*----------------------------------------------------------------------*
FORM bdc_transaction USING p_tcode.
  DATA: l_subrc LIKE sy-subrc,
        msg(255).

  REFRESH: messtab.


  CALL TRANSACTION p_tcode USING bdcdata
                   MODE   ctumode
                   UPDATE cupdate
                   MESSAGES INTO messtab.
  l_subrc = sy-subrc.

  READ TABLE messtab WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = sy-msgid
        msgnr               = sy-msgno
        msgv1               = sy-msgv1
        msgv2               = sy-msgv2
        msgv3               = sy-msgv3
        msgv4               = sy-msgv4
      IMPORTING
        message_text_output = msg.

    REFRESH bdcdata.
    it_bdc_log-lifnr = w_lifnr.
    it_bdc_log-flag = 'E'.
    it_bdc_log-msg = msg.
    APPEND it_bdc_log.
    CLEAR: it_bdc_log.
  ELSE.
    READ TABLE messtab WITH KEY msgtyp = 'A'.
    IF sy-subrc = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = sy-msgid
          msgnr               = sy-msgno
          msgv1               = sy-msgv1
          msgv2               = sy-msgv2
          msgv3               = sy-msgv3
          msgv4               = sy-msgv4
        IMPORTING
          message_text_output = msg.

      REFRESH bdcdata.
      it_bdc_log-flag = 'E'.
      it_bdc_log-msg = msg.
      APPEND it_bdc_log.
      CLEAR: it_bdc_log.
*    ELSE.
*      it_bdc_log-flag = 'S'.
*    it_bdc_log-msg = 'Successfully updated'.
*        append it_bdc_log.
    ENDIF.
  ENDIF.
  REFRESH: messtab, bdcdata.

ENDFORM.                    " BDC_TRANSACTION               " CALL_MEK1
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_error_data.

  WRITE: 'Conditon creation error (MEK1)'.
  WRITE /1 'Vendor'.
  WRITE 10 'Remarks'.

  WRITE /1 '============'.
  WRITE 10 '======================================='.
  WRITE 40 '======================================='.
  IF p_upl = 'X'.
    LOOP AT it_bdc_log.
      WRITE /1 it_bdc_log-lifnr.
      WRITE 10 it_bdc_log-msg.
    ENDLOOP.
  ELSE.
    LOOP AT it_bapi_log.
      WRITE /1 it_bapi_log-ebeln.
      WRITE 10 it_bapi_log-msg.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_po_data .
  DATA: l_index LIKE sy-tabix,
        l_index_inv LIKE sy-tabix.

  DATA: BEGIN OF lt_ekbe OCCURS 0,
      ebeln LIKE ekbe-ebeln,
      ebelp LIKE ekbe-ebelp,
      matnr LIKE ekbe-matnr,
      menge LIKE ekbe-menge,
      shkzg LIKE ekbe-shkzg,
      END OF lt_ekbe.

  SELECT a~ebeln b~ebelp b~matnr b~meins b~menge
         knumv b~mwskz e~mwsk1
         INTO CORRESPONDING FIELDS OF TABLE it_po
    FROM ekko AS a INNER JOIN ekpo AS b
    ON a~ebeln = b~ebeln
    INNER JOIN a951 AS d
    ON b~matnr = d~matnr
     AND a~lifnr = d~lifnr
    INNER JOIN konp AS e
    ON d~knumh = e~knumh
    AND  d~kschl = e~kschl
    WHERE a~bedat IN s_bedat
      AND a~bsart IN s_bsart
      AND a~lifnr IN s_lifnr
      AND a~loekz = ' '
      AND b~loekz = ' '
      AND e~kschl = 'NAVS'
      AND b~mwskz <> e~mwsk1.

  SORT it_po BY ebeln ebelp.
  IF sy-subrc = 0.
    SELECT ebeln ebelp matnr menge shkzg
      INTO TABLE lt_ekbe
      FROM ekbe
      FOR ALL ENTRIES IN it_po
      WHERE ebeln = it_po-ebeln
        AND ebelp = it_po-ebelp.
    IF sy-subrc = 0.
      SORT lt_ekbe BY ebeln ebelp.
      LOOP AT it_po.
        l_index = sy-tabix.
        READ TABLE lt_ekbe WITH KEY ebeln = it_po-ebeln
                                    ebelp = it_po-ebelp
                                    BINARY SEARCH.
        IF sy-subrc = 0.
          l_index_inv = sy-tabix.
          LOOP AT lt_ekbe FROM l_index_inv WHERE ebeln = it_po-ebeln
                                           AND ebelp = it_po-ebelp.
            IF lt_ekbe-shkzg = 'S'.
              it_po-invqty = it_po-invqty + lt_ekbe-menge.
            ELSE.
              it_po-invqty = it_po-invqty - lt_ekbe-menge.
            ENDIF.
          ENDLOOP.
          IF it_po-menge <= it_po-invqty.
            DELETE it_po INDEX l_index.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
    MESSAGE s009 WITH 'No Data'.
  ENDIF.
ENDFORM.                    " GET_PO_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_MEK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_12  text
*----------------------------------------------------------------------*
FORM call_mek1.
  DATA: l_no(2) TYPE n,
        l_matnr_t(20),
        l_kbetr_t(20),
        l_datab_t(20),
        l_datbi_t(20),
        l_mwsk1_t(20),
        l_kbetr(8),
        l_datab(10),
        l_datbi(10).

  PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-KSCHL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RV13A-KSCHL'
                                'NAVS'.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '1951'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONP-MWSK1(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'KOMG-LIFNR'
                                w_lifnr.

  l_no = '01'.
  LOOP AT it_12.
    CONCATENATE 'KOMG-MATNR(' l_no ')' INTO l_matnr_t.
    CONCATENATE 'KONP-KBETR(' l_no ')' INTO l_kbetr_t.
    CONCATENATE 'RV13A-DATAB(' l_no ')' INTO l_datab_t.
    CONCATENATE 'RV13A-DATBI(' l_no ')' INTO l_datbi_t.
    CONCATENATE 'KONP-MWSK1(' l_no ')' INTO l_mwsk1_t.

    l_datab = '01/01/2010'.
    l_datbi = '12/31/2999'.

    CASE it_12-mwskz.
      WHEN 'U1'.
        l_kbetr = 9.
      WHEN 'U3'.
        l_kbetr = 10.
      WHEN 'U5'.
        l_kbetr = '0.13'.
      WHEN 'U6'.
        l_kbetr = '0.13'.
      WHEN 'U7'.
        l_kbetr = '3.58'.
      WHEN 'U8'.
        l_kbetr = '3.58'.
    ENDCASE.

    PERFORM bdc_field       USING l_matnr_t
                            it_12-matnr.
    PERFORM bdc_field       USING l_kbetr_t
                            l_kbetr.
    PERFORM bdc_field       USING l_datab_t
                                  l_datab.
    PERFORM bdc_field       USING l_datbi_t
                                  l_datbi.
    PERFORM bdc_field       USING l_mwsk1_t
                                  it_12-mwskz.

*  perform bdc_field       using 'KOMG-MATNR(01)'
*                              record-MATNR_01_003.
*perform bdc_field       using 'KONP-KBETR(01)'
*                              record-KBETR_01_004.
*    PERFORM bdc_field       USING 'RV13A-DATAB(01)'
*                                  record-datab_01_005.
*    PERFORM bdc_field       USING 'RV13A-DATBI(01)'
*                                  record-datbi_01_006.
*    PERFORM bdc_field       USING 'KONP-MWSK1(01)'
*                                  record-mwsk1_01_007.

    PERFORM bdc_dynpro      USING 'SAPMV13A' '1951'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  l_mwsk1_t.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    l_no = l_no + 1.
  ENDLOOP.

  PERFORM bdc_dynpro      USING 'SAPMV13A' '1951'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONP-MWSK1(12)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BNEW'.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '1951'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KOMG-LIFNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
*    PERFORM bdc_field       USING 'KOMG-LIFNR'
*                                  record-lifnr_063.

  PERFORM bdc_transaction USING 'MEK1'.

ENDFORM.                                                    " CALL_MEK1
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1580   text
*      -->P_1581   text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_po .
  LOOP AT it_po.
    AT NEW ebeln.
      w_ebeln = it_po-ebeln.
      REFRESH it_chpo.
      CLEAR: it_chpo.
    ENDAT.
    it_chpo = it_po.
    APPEND it_chpo.
    AT END OF ebeln.
      CLEAR: it_bapi_log.
      PERFORM change_po_bapi.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " CHANGE_PO
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PO_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_po_bapi .
  DATA: i_poheader        LIKE          bapimepoheader,
        i_poheaderx       LIKE          bapimepoheaderx,
        t_poitem TYPE TABLE OF bapimepoitem WITH HEADER LINE,
        t_poitemx       TYPE TABLE OF bapimepoitemx    WITH HEADER LINE,
        t_poaccount     TYPE TABLE OF bapimepoaccount  WITH HEADER LINE,
        t_poaccountx    TYPE TABLE OF bapimepoaccountx WITH HEADER LINE,
        t_poschedule    TYPE TABLE OF bapimeposchedule WITH HEADER LINE,
        t_poschedulex   TYPE TABLE OF bapimeposchedulx WITH HEADER LINE,
        t_pocond        TYPE TABLE OF bapimepocond     WITH HEADER LINE,
        t_pocondx       TYPE TABLE OF bapimepocondx    WITH HEADER LINE,
        t_potext        TYPE TABLE OF bapimepotext     WITH HEADER LINE,
        t_polimits      TYPE TABLE OF bapiesuhc        WITH HEADER LINE,
        t_poservices    TYPE TABLE OF bapiesllc        WITH HEADER LINE.

  DATA: ret2 LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  LOOP AT it_chpo.

    t_poitem-po_item    = it_chpo-ebelp.
    t_poitem-material    = it_chpo-matnr.
    t_poitem-tax_code   = it_chpo-mwsk1.

    t_poitemx-po_item    = it_chpo-ebelp.
    t_poitemx-po_itemx   = 'X'.
    t_poitemx-material    = 'X'.
    t_poitemx-tax_code    = 'X'.

    APPEND t_poitem.
    APPEND t_poitemx.

    t_pocond-itm_number    = it_chpo-ebelp.
*    t_pocond-condition_no  = it_chpo-knumv.
    t_pocond-cond_type     =  'NAVS'.

    t_pocondx-itm_number   = it_chpo-ebelp.
*    t_pocondx-condition_no = 'X'.
    t_pocondx-cond_type    = 'X'.

    APPEND t_pocond.
    APPEND t_pocondx.

    CLEAR it_chpo.

  ENDLOOP.

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = w_ebeln
    TABLES
      return        = ret2
      poitem        = t_poitem
      poitemx       = t_poitemx
      pocond        = t_pocond
      pocondx       = t_pocondx.

  READ TABLE ret2 WITH KEY type = 'S'.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*      IMPORTING
*        return = commit_return.

    MOVE: 'S'      TO it_bapi_log-flag,
          'Successfully changed' TO it_bapi_log-msg.
    APPEND it_bapi_log.
  ELSE.
    LOOP AT ret2 WHERE type = 'E'
                    OR type = 'A'
                    OR type = 'X'.
      MOVE: ret2-type TO it_bapi_log-flag,
          ret2-message_v1 TO it_bapi_log-msg.
      APPEND it_bapi_log.
    ENDLOOP.
  ENDIF.

  REFRESH it_chpo.
  CLEAR: it_chpo, w_ebeln, it_bapi_log.
ENDFORM.                    " CHANGE_PO_BAPI
