************************************************************************
* Program Name      : ZRMM_MIT
* Creation Date     : 04/07/2006
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zrmm_mit NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TABLES: lips, likp.
TYPE-POOLS slis .
DATA : BEGIN OF it_tab OCCURS 0,
      matnr LIKE lips-matnr,
      vbeln LIKE lips-vbeln,
      lfimg LIKE lips-lfimg,
      zzarrdt LIKE likp-zzarrdt,
      lifex LIKE likp-lifex,
      bolnr LIKE likp-bolnr,
      traid LIKE likp-traid,
      kdmat LIKE lips-kdmat,
      vgbel LIKE lips-vgbel,
      zfreta LIKE ztbl-zfreta,
      END OF it_tab.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
      w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt   TYPE   i.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_matnr FOR lips-matnr,
                 s_vbeln FOR lips-vbeln,
                 s_date FOR likp-zzarrdt.
SELECTION-SCREEN END OF BLOCK block1.

AT SELECTION-SCREEN OUTPUT.
*  PERFORM check_screen.

AT SELECTION-SCREEN.
*  PERFORM check_input_value.

START-OF-SELECTION.
  PERFORM get_data.
  IF it_tab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    SORT it_tab BY matnr.
    PERFORM display_data.
  ENDIF.
*---------------------------------------------------------------------*
*       FORM get_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_data.
*  SELECT matnr a~vbeln lfimg zzarrdt lifex bolnr traid kdmat vgbel
*  INTO CORRESPONDING FIELDS OF TABLE it_tab
*  FROM likp AS a
*  INNER JOIN lips AS b
*  ON a~vbeln = b~vbeln
*  INNER JOIN vbuk AS c
*  ON a~vbeln = c~vbeln
*  WHERE a~vbeln IN s_vbeln
*    AND matnr IN s_matnr
*    AND wbstk = 'A'
*    AND zzarrdt IN s_date.

  SELECT B~matnr c~vbeln d~menge as lfimg d~eindt as zzarrdt lifex
         bolnr traid
         kdmat
         a~ebeln as vgbel
   INTO CORRESPONDING FIELDS OF TABLE it_tab
   FROM ekko AS a INNER JOIN ekpo AS b
   ON a~ebeln = b~ebeln
   INNER JOIN ekes AS d
   ON d~ebeln = b~ebeln
   AND d~ebelp = b~ebelp
   INNER JOIN vbuk AS c
   ON d~vbeln = c~vbeln
  INNER JOIN likp AS e
   ON d~vbeln = e~vbeln
   INNER JOIN lips AS f
   ON e~vbeln = f~vbeln
   WHERE c~vbeln IN s_vbeln
   AND B~matnr IN s_matnr
     AND c~wbstk = 'A'
     AND c~vbtyp = '7'
     AND eindt IN s_date
     and d~MENGE > 0.

  DELETE ADJACENT DUPLICATES FROM it_tab.
  LOOP AT it_tab.
    SELECT SINGLE zfreta INTO it_tab-zfreta
      FROM ztbl
     WHERE ZFHBLNO = it_tab-lifex.
    IF sy-subrc = 0.
      MODIFY it_tab.
    ENDIF.
    CLEAR: it_tab.
  ENDLOOP.

  SORT it_tab BY matnr vbeln.
ENDFORM.
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
FORM check_screen.
  LOOP AT SCREEN.
    IF screen-name = 'P_EXCEL'.
      screen-input = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen

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
    PERFORM build_field_catalog USING 'IT_TAB'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
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
    WHEN 'INBDEL'.
      PERFORM display_inbound_delivery.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_INBOUND_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_inbound_delivery.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i,
        l_trvog LIKE t180-trvog.

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
  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_tab INDEX lt_rows-index.

  SELECT SINGLE trvog INTO l_trvog FROM t180
   WHERE tcode = 'VL33N'.

  CASE l_trvog.
    WHEN 'D'.
      SET PARAMETER ID 'VLM' FIELD it_tab-vbeln.
    WHEN 'G'.
      SET PARAMETER ID 'VLG' FIELD it_tab-vbeln.
    WHEN OTHERS.
      SET PARAMETER ID 'VL' FIELD it_tab-vbeln.
  ENDCASE.
  CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.

*  DATA: bdcdata TYPE TABLE OF bdcdata.
*
*  DATA: itab TYPE TABLE OF bdcmsgcoll.
*
*  DATA: program LIKE sy-repid,
*
*        wa_bdcdata TYPE bdcdata.
*
*  wa_bdcdata-program  = 'SAPMV50A'.
*  wa_bdcdata-dynpro   = '4104'.
*  wa_bdcdata-dynbegin = 'X'.
*  APPEND wa_bdcdata TO bdcdata.
*  CLEAR wa_bdcdata.
*  wa_bdcdata-fnam     = 'LIKP-VBELN'.
*  wa_bdcdata-fval     = it_tab-vbeln.
*  APPEND wa_bdcdata TO bdcdata.
**CLEAR WA_BDCDATA.
**WA_BDCDATA-FNAM     = 'BDC_OKCODE'.
**WA_BDCDATA-FVAL     = '/00'.
*
*  APPEND wa_bdcdata TO bdcdata.
*
*  CALL TRANSACTION 'VL33N'  USING bdcdata." MODE 'N'.
**                         MESSAGES INTO ITAB.

ENDFORM.                    " DISPLAY_INBOUND_DELIVERY

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display

   EXPORTING   is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_tab[]
               it_sort          = it_sort[].

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
  it_sort-fieldname      = 'MATNR'.
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

                                  'S' 'MATNR'       ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'VBELN'       ' ',
                                  ' ' 'COLTEXT'     'Inbound Del',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'LFIMG'       ' ',
                                  ' ' 'COLTEXT'     'Quantity',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'ZZARRDT'     ' ',
                                  ' ' 'COLTEXT'     'ETA',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'LIFEX'        ' ',
                                  ' ' 'COLTEXT'     'BOL',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'BOLNR'       ' ',
                                  ' ' 'COLTEXT'     'Invoice',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'TRAID'         ' ',
                                  ' ' 'COLTEXT'     'Container',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'KDMAT'       ' ',
                                  ' ' 'COLTEXT'     'Case',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'VGBEL'       ' ',
                                  ' ' 'COLTEXT'     'SAP-PO',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ZFRETA'      ' ',
                                  ' ' 'COLTEXT'     'Arrival Date',
                                  'E' 'OUTPUTLEN'   '10'.

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
