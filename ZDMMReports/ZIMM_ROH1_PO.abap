************************************************************************
* Program Name      : ZIMM_ROH1_PO
* Creation Date     : 10/29/09
* Development Request No :
* Addl Documentation:
* Description       : Send ROH1 PO/GR to HMC
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zimm_roh1_po NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TYPE-POOLS: slis, vrm.
TABLES: ztmm_roh1_po, mara.
*DATA: IT_DATA LIKE TABLE OF ZTMM_PILOT_MATL WITH HEADER LINE.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztmm_roh1_po.
DATA: END OF it_data.
DATA: it_error LIKE TABLE OF it_data WITH HEADER LINE.

*CONSTANTS: C_DEST(10) VALUE 'WMPM01'.

DATA: w_dest(10).
*DATA: W_FILENAME LIKE RLGRAP-FILENAME.

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
      w_mtart LIKE mara-mtart.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_date LIKE sy-datum DEFAULT sy-datum.
SELECT-OPTIONS: s_matkl FOR mara-matkl.
PARAMETERS: p_bukrs LIKE t001-bukrs DEFAULT 'H201' OBLIGATORY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
PARAMETERS: p_batch AS CHECKBOX  USER-COMMAND chal.
SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-u01 FOR FIELD p_sd.
PARAMETERS: p_sd RADIOBUTTON GROUP grp1 MODIF ID abc.
SELECTION-SCREEN COMMENT 20(8) text-u12 FOR FIELD p_eo.
PARAMETERS: p_eo RADIOBUTTON GROUP grp1 MODIF ID abc.
SELECTION-SCREEN COMMENT 36(15) text-u13 FOR FIELD p_es.
PARAMETERS: p_es RADIOBUTTON GROUP grp1 MODIF ID abc.
SELECTION-SCREEN COMMENT 60(11) text-u14 FOR FIELD p_rp.
PARAMETERS: p_rp RADIOBUTTON GROUP grp1 MODIF ID abc.
SELECTION-SCREEN  END OF LINE.

SELECTION-SCREEN ULINE.
SELECTION-SCREEN SKIP.
PARAMETERS: p_rver like SOMLRECI1-RECEIVER OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen_all.

START-OF-SELECTION.

  PERFORM get_data.
  IF it_data[] IS INITIAL.
    MESSAGE i009 WITH 'No data found'.
  ELSE.
    IF p_batch = 'X'.
      PERFORM save_send_data.
    ELSE.
      IF p_es = 'X' OR p_rp = 'X'.
        PERFORM save_send_data.
      ENDIF.
      PERFORM display_data.
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
FORM get_data.

  DATA: l_variable LIKE TABLE OF zspp_vin_value WITH HEADER LINE.
  DATA: l_qty_101 LIKE ekbe-menge,
        l_qty_102 LIKE ekbe-menge,
        l_date_c LIKE eket-eindt.

  DATA: BEGIN OF lt_ekbe OCCURS 0,
        ebeln LIKE ekbe-ebeln,
        ebelp  LIKE ekbe-ebelp,
        matnr LIKE ekbe-matnr,
        menge LIKE ekbe-menge,
        werks LIKE ekbe-werks,
        budat LIKE ekbe-budat,
*        menge like ekpo-menge,
*        lifnr like ekko-lifnr,
*        netpr like ekpo-netpr,
*        waers like ekko-waers,
*        peinh like ekpo-peinh,
*        meins like ekpo-meins,
        END OF lt_ekbe.

  IF p_bukrs = 'H201'.
    w_mtart = 'ROH1'.
  ELSEIF p_bukrs = 'K201'.
    w_mtart = 'RAW1'.
  ENDIF.

  CASE 'X'.
    WHEN p_sd.  " AND p_batch IS INITIAL.
      SELECT * INTO TABLE it_data
        FROM ztmm_roh1_po
        WHERE budat = p_date.
    WHEN p_rp.
      SELECT * INTO TABLE it_data
       FROM ztmm_roh1_po
       WHERE budat = p_date
       AND rflag <> 'Z'.

      LOOP AT it_data.
        CLEAR: it_data-if_date_change, it_data-if_time_change,
               it_data-rflag, it_data-message.
        MODIFY it_data.
      ENDLOOP.

    WHEN OTHERS.
      SELECT ebeln ebelp a~matnr menge werks budat
        INTO TABLE lt_ekbe
        FROM mara AS a
        INNER JOIN ekbe AS b
        ON a~matnr = b~matnr
        WHERE a~mtart = w_mtart
         AND matkl IN s_matkl     " 'AM'
         AND b~budat = p_date
         AND b~bwart = '101'.
      IF sy-subrc = 0.
        LOOP AT lt_ekbe.
          it_data-ebeln = lt_ekbe-ebeln.
          it_data-ebelp = lt_ekbe-ebelp.
          it_data-budat = lt_ekbe-budat.
          it_data-matnr = lt_ekbe-matnr.
          it_data-menge = lt_ekbe-menge.
          it_data-plant = lt_ekbe-werks.
          it_data-weemg = lt_ekbe-menge.
          COLLECT it_data.
          CLEAR: it_data.
        ENDLOOP.
        l_variable-atnam = 'ZSTEEL_MATPROPERTY'.
        APPEND l_variable .
        l_variable-atnam = 'ZFRONT_FINISHING_THICKNESS'.
        APPEND l_variable .
        l_variable-atnam = 'ZSPEC_THICK'.
        APPEND l_variable .
        l_variable-atnam = 'ZSPEC_WIDTH'.
        APPEND l_variable .
        l_variable-atnam = 'ZSPEC_LENGTH'.
        APPEND l_variable .
        l_variable-atnam = 'ZKIND_OF_STEEL'.
        APPEND l_variable .
        l_variable-atnam = 'ZIN_OR_OUT'.
        APPEND l_variable .
        l_variable-atnam = 'ZEDGE'.
        APPEND l_variable .

        LOOP AT it_data.

          CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
               EXPORTING
                    object       = it_data-matnr
                    mode         = 'R'
                    ctype        = '001'
               TABLES
                    val_table    = l_variable
               EXCEPTIONS
                    no_data      = 1
                    error_mode   = 2
                    error_object = 3.

          IF sy-subrc = 0.
            LOOP AT l_variable.
              CASE l_variable-atnam.
                WHEN 'ZSTEEL_MATPROPERTY'.
                  it_data-steel_mat = l_variable-atwrt.
                WHEN 'ZFRONT_FINISHING_THICKNESS'.
                  it_data-coating = l_variable-atwrt.
                WHEN 'ZSPEC_THICK'.
                  it_data-thick = l_variable-atwrt.
                WHEN 'ZSPEC_WIDTH'.
                  it_data-width = l_variable-atwrt.
                WHEN 'ZSPEC_LENGTH'.
                  it_data-length = l_variable-atwrt.
                WHEN 'ZKIND_OF_STEEL'.
                  it_data-kind = l_variable-atwrt.
                WHEN 'ZIN_OR_OUT'.
                  it_data-in_out = l_variable-atwrt.
                WHEN 'ZEDGE'.
                  it_data-edge = l_variable-atwrt.
              ENDCASE.
            ENDLOOP.
          ENDIF.

          CLEAR: l_date_c.
          SELECT SINGLE menge netpr peinh meins lifnr waers
             a~aedat INTO
            (it_data-menge, it_data-netpr, it_data-peinh,
            it_data-meins, it_data-lifnr, it_data-waers,
            l_date_c)
            FROM ekko AS a
            INNER JOIN ekpo AS b
            ON a~ebeln = b~ebeln
            WHERE a~ebeln = it_data-ebeln
              AND b~ebelp = it_data-ebelp.
          it_data-aedat = l_date_c.

          CLEAR: l_date_c.
          SELECT SINGLE eindt INTO l_date_c
               FROM eket
               WHERE ebeln = it_data-ebeln
                 AND ebelp = it_data-ebelp.
          it_data-eindt = l_date_c.

          SELECT SUM( menge ) INTO l_qty_101
           FROM ekbe
           WHERE ebeln = it_data-ebeln
             AND ebelp = it_data-ebelp
             AND budat <= p_date
             AND bwart IN ('101', '123').

          SELECT SUM( menge ) INTO l_qty_102
            FROM ekbe
            WHERE ebeln = it_data-ebeln
              AND ebelp = it_data-ebelp
              AND budat <= p_date
              AND bwart IN ('102', '122').

          it_data-balance = it_data-menge - l_qty_101 + l_qty_102.

          MODIFY it_data.

          LOOP AT l_variable.
            CLEAR: l_variable-atwrt.
            MODIFY l_variable.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
  ENDCASE.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_send_data.
  DATA: l_result(1),
        l_totrec TYPE i,
        l_srec TYPE i,
        l_frec TYPE i,
        l_msgtxt(60).

*  DATA: LT_OUTPUT LIKE TABLE OF ZTMM_ROH1_PO WITH HEADER LINE.
*
*  LOOP AT IT_DATA.
*    MOVE-CORRESPONDING IT_DATA TO LT_OUTPUT.
*    APPEND LT_OUTPUT.
*  ENDLOOP.

  IF p_bukrs = 'H201'.
    w_dest = 'WMPM01'.
  ELSE.

  ENDIF.
  DESCRIBE TABLE it_data LINES l_totrec.

  CALL FUNCTION 'Z_FMM_ROH1_PO'
     DESTINATION w_dest
     IMPORTING
       flag          = l_result
     TABLES
       i_pilot_matl  = it_data
     EXCEPTIONS
            communication_failure = 1 MESSAGE l_msgtxt
            system_failure        = 2 MESSAGE l_msgtxt.
*  IF SY-SUBRC = 0.
*  IF l_result = 'S' OR l_result = 's'.
*    WRITE: / 'Total record number(s) are : ', l_totrec,
*           'were sent successfully'.
*    LOOP AT it_data.
*      it_data-budat = sy-datum.
*      it_data-if_date = sy-datum.
*      it_data-if_time = sy-uzeit.
*      it_data-flag = 'S'.
*      MODIFY it_data.
*    ENDLOOP.
*
*  ELSE.
*    WRITE: / 'EAI Failed, ', l_msgtxt.
*    LOOP AT it_data.
*      it_data-budat = sy-datum.
*      it_data-if_date = sy-datum.
*      it_data-if_time = sy-uzeit.
*      it_data-flag = 'E'.
*      MODIFY it_data.
*    ENDLOOP.
*  ENDIF.
  IF l_result = 'S' OR l_result = 's'.

    LOOP AT it_data.
      IF it_data-rflag = 'F'.
        l_frec = l_frec + 1.
        it_error = it_data.
        APPEND it_error.
      ELSE.
        l_srec = l_srec + 1.
      ENDIF.
      IF p_rp = 'X'.
        it_data-if_date_change = sy-datum.
        it_data-if_time_change = sy-uzeit.
      ELSE.
        it_data-budat = sy-datum.
        it_data-if_date = sy-datum.
        it_data-if_time = sy-uzeit.
      ENDIF.
      it_data-flag = 'S'.
      MODIFY it_data.
    ENDLOOP.
    WRITE: / 'Total records are : ', l_totrec.
    WRITE: / 'Successfully sent records are : ', l_srec.
    WRITE: / 'Unsuccessfully sent records are : ', l_frec.
    IF l_totrec = l_srec.
      it_data-flag = 'S'.
      MODIFY TABLE it_data TRANSPORTING flag.
    ELSE.
      it_data-flag = 'E'.
      MODIFY TABLE it_data TRANSPORTING flag.
    ENDIF.
  ELSE.
    WRITE: / 'EAI Failed, Total records are: ', l_totrec.
    LOOP AT it_data.
      IF p_rp = 'X'.
        it_data-if_date_change = sy-datum.
        it_data-if_time_change = sy-uzeit.
      ELSE.
        it_data-budat = sy-datum.
        it_data-if_date = sy-datum.
        it_data-if_time = sy-uzeit.
      ENDIF.
      it_data-flag = 'E'.
      MODIFY it_data.
    ENDLOOP.
    it_error[] = it_data[].
  ENDIF.

  MODIFY ztmm_roh1_po FROM TABLE it_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  IF NOT it_error[] IS INITIAL.
    PERFORM send_email.
  ENDIF.

ENDFORM.
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
    IF p_batch = 'X' AND screen-group1 EQ 'ABC'.
      screen-invisible = 1.
      screen-active    = 0.
      screen-input     = 0.
      MODIFY SCREEN.
    ENDIF.
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
          EXPORTING container_name = wa_custom_control
          EXCEPTIONS
           cntl_error = 1
           cntl_system_error = 2
           create_error = 3
           lifetime_error = 4
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
         EXPORTING i_parent = grid_container
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
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'IN_OUT'       ' ',
                                  ' ' 'COLTEXT'     'In or Out',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'MENGE'       ' ',
                                  ' ' 'COLTEXT'     'Scheduled QTY',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'WEEMG'       ' ',
                                  ' ' 'COLTEXT'     'GR Received',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'BALANCE'       ' ',
                                  ' ' 'COLTEXT'     'Open GR QTY',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'EINDT'       ' ',
                                  ' ' 'COLTEXT'     'Del. Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'NETPR'       ' ',
                                  ' ' 'COLTEXT'     'Net Price',
                                  'E' 'OUTPUTLEN'   '13',

                                 'S' 'WAERS'       ' ',
                                  ' ' 'COLTEXT'     'Currency',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'PEINH'       ' ',
                                  ' ' 'COLTEXT'     'PR Unit',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'MEINS'       ' ',
                                  ' ' 'COLTEXT'     'UOM',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'BUDAT'       ' ',
                                  ' ' 'COLTEXT'     'Cr Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'FLAG'        ' ',
                                  ' ' 'COLTEXT'     'EAI',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'RFLAG'        ' ',
                                  ' ' 'COLTEXT'     'HMC',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'MESSAGE'       ' ',
                                  ' ' 'COLTEXT'     'HMC Message',
                                  'E' 'OUTPUTLEN'   '80'.



ENDFORM.
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

   EXPORTING   is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_data[]
               it_sort          = it_sort[].

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
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_inforecord_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6019_01>_MATNR  text
*      -->P_<FS_ZTMM_6019_01>_LIFNR  text
*      <--P_<FS_ZTMM_6019_01>_NETPR  text
*      <--P_<FS_ZTMM_6019_01>_EFFPR  text
*      <--P_<FS_ZTMM_6019_01>_WAERS  text
*      <--P_<FS_ZTMM_6019_01>_BPUMZ  text
*      <--P_<FS_ZTMM_6019_01>_BPUMN  text
*----------------------------------------------------------------------*
FORM get_inforecord_data USING value(im_matnr)
                               value(im_lifnr)
                      CHANGING value(ex_kbetr)
                               value(ex_konwa)
                               value(ex_kpein)
                               value(ex_kmein).

  DATA: BEGIN OF lt_datab OCCURS 0,
          datab LIKE a018-datab,
          knumh LIKE konp-knumh,
          END OF lt_datab.
  DATA: l_knumh LIKE lt_datab-knumh.

  SELECT datab knumh INTO TABLE lt_datab
    FROM a018
    WHERE kschl = 'PB00'
      AND matnr = im_matnr
      AND lifnr = im_lifnr.

  SORT lt_datab BY datab DESCENDING.
  READ TABLE lt_datab INDEX 1.
  l_knumh = lt_datab-knumh.

  SELECT SINGLE kbetr konwa kpein kmein
    INTO (ex_kbetr, ex_konwa, ex_kpein, ex_kmein)
    FROM konp
    WHERE knumh = l_knumh
     AND kschl = 'PB00'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.
  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.

  DATA: l_subject TYPE p15_text150,
        l_p_rec_type	LIKE	somlreci1-rec_type.

  MOVE 'Following items with EAI errors:' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '================================' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Material No' TO lt_body+0(20),
        'PO' TO lt_body+20(15).

  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '--------------------' TO  lt_body+0(20),
        '---------------' TO  lt_body+20(15).
  APPEND lt_body.
  CLEAR: lt_body.

  LOOP AT it_data.
    MOVE: it_data-matnr TO lt_body+0(20),
          it_data-ebeln TO lt_body+20(15).
    APPEND lt_body.
  ENDLOOP.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
       EXPORTING
            p_subject  = 'V-Steel interface error - BLANK'
            p_rec_type = 'C'
            p_receiver = p_rver
       TABLES
            pt_body    = lt_body.

ENDFORM.                    " SEND_EMAIL
