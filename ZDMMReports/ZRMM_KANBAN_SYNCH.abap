************************************************************************
* Program Name      : ZRMM_KANBAN_SYNCH
* Creation Date     : 06/23/2014
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zrmm_kanban_synch NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.
TABLES: pkps.
TYPE-POOLS: slis, vrm.

DATA: it_data LIKE TABLE OF zmmt0038 WITH HEADER LINE.

DATA: w_dest(10),
      ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      g_docking_container    TYPE REF TO cl_gui_docking_container.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_saedt FOR pkps-saedt OBLIGATORY,
                s_pknum FOR pkps-pknum.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_init_data.

START-OF-SELECTION.

  PERFORM read_data.

  IF it_data[] IS INITIAL.
    MESSAGE i009 WITH text-004.
    EXIT.
  ENDIF.

  PERFORM process_data.


END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM get_req_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM read_data.

  DATA: l_index LIKE sy-tabix.
  DATA: BEGIN OF lt_temp OCCURS 0,
        rsnum LIKE resb-rsnum,
        rspos LIKE resb-rspos,
        pkkey LIKE pkps-pkkey,
        saedt LIKE pkps-saedt,
        saeuz LIKE pkps-saeuz,
        werks LIKE pkhd-werks,
        prvbe LIKE pkhd-prvbe,
        zfeeder LIKE pkhd-zfeeder,
        matnr LIKE pkhd-matnr,
        ablad LIKE pkhd-ablad,
        rksta LIKE pkhd-rksta,
        umlgo LIKE pkhd-umlgo,
        kwbzm LIKE pkhd-kwbzm,
        pkgpzg LIKE pkps-pkgpzg,
        pkbmg LIKE pkps-pkbmg,
        END OF lt_temp.

  CLEAR : it_data, it_data[].

  SELECT b~rsnum b~rspos b~pkkey saedt  saeuz
         a~werks a~prvbe zfeeder c~matnr
         a~ablad rksta a~umlgo kwbzm pkgpzg pkbmg
     INTO TABLE lt_temp
      FROM pkhd AS a
          INNER JOIN pkps AS b
    ON a~pknum = b~pknum
      INNER JOIN resb AS c
      ON b~rsnum = c~rsnum AND
       b~rspos = c~rspos
      WHERE b~saedt IN s_saedt
       AND  b~pkbst = '1'
       AND  b~rsnum <> ' '
       AND  c~xloek = ' '
       AND  c~enmng = 0.

  SORT lt_temp BY rsnum rspos.
*  DELETE ADJACENT DUPLICATES FROM lt_temp
*   COMPARING rsnum rspos.

  LOOP AT lt_temp.
    MOVE-CORRESPONDING lt_temp TO it_data.
    it_data-reversed = 'X'.
    it_data-lgort = lt_temp-umlgo.
    it_data-lgpro = 'P400'.
    it_data-meins = 'EA'.
    it_data-pktim = lt_temp-pkgpzg.
    it_data-sfgsn = ' '.
    it_data-type = 'E'.
    it_data-etdat = sy-datum.
    it_data-ettim = sy-uzeit.
    it_data-etnam = sy-uname.
    APPEND it_data.
    CLEAR: it_data.
  ENDLOOP.


ENDFORM.                    "READ_DATA
*&---------------------------------------------------------------------*
*&      Form  set_init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_init_data.
  s_saedt-option = 'BT'.
  s_saedt-sign = 'I'.
  s_saedt-low = sy-datum.
  s_saedt-high = sy-datum.
  APPEND s_saedt.
ENDFORM.                    " set_init_data

*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  DATA:   l_repid LIKE sy-repid,
          l_dynnr LIKE sy-dynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  CREATE OBJECT g_docking_container
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = l_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
*         EXPORTING i_parent = grid_container
         EXPORTING i_parent = g_docking_container
                   i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8),
        l_cn(2) TYPE n.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_structure_name   = 'ZSMM_CKD_ENG_PO'
*     i_internal_tabname = lw_itab
*     i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                'S' 'ZZKDWEBPO'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'HMC PO',
                                  'E' 'OUTPUTLEN'   '10',

                                   'S' 'EBELN'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'PO Number',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'PO Item',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'MATNR'         ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'MENGE'       ' ',
                                  ' ' 'COLTEXT'     'QTY',
                                  ' ' 'DECIMALS_O'  '0',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '13',

                                 'S' 'PEINH'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Pr Unit',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'WAERS'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Curr',
                                  'E' 'OUTPUTLEN'   '5',

                                 'S' 'PMATN'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Pr Ref Mat',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'STAWN'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Comm/Imp ocde no',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'ERDAT'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Date',
                                  'E' 'OUTPUTLEN'   '10'.

ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
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

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_data[].
*               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.

  IF g_docking_container IS INITIAL.

    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_DATA'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  CALL SCREEN 0200.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .

  LOOP AT it_data.
    UPDATE resb SET xloek = 'X'
       WHERE rsnum = it_data-rsnum
         AND rspos = it_data-rspos.
    IF sy-subrc = 0.
      MODIFY zmmt0038 FROM it_data.
      IF sy-subrc = 0.
        COMMIT WORK.
        WRITE: /1 it_data-rsnum.
        WRITE: 12 it_data-rspos.
        WRITE: 22 'Succesfully updated'.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
      WRITE: /1 it_data-rsnum.
      WRITE: 12 it_data-rspos.
      WRITE: 22 'Update failed'.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " PROCESS_DATA
