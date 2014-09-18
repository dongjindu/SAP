************************************************************************
* Program Name      : ZPPR_3C_DUMMY
* Creation Date     : 10/03/12
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zppr_3c_dummy NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TYPE-POOLS: slis, vrm.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztpperm_3c_dummy.
DATA: END OF it_data.
DATA: it_error LIKE TABLE OF it_data WITH HEADER LINE.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldname.  "IT_FIELDCAT.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_date FOR sy-datum.

PARAMETERS: p_rver  LIKE somlreci1-receiver OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM init_data.

START-OF-SELECTION.

  PERFORM get_data.
  IF it_data[] IS INITIAL.
    MESSAGE i009 WITH 'No data found'.
  ELSE.
    PERFORM save_send_data.
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
  DATA: lt_temp LIKE TABLE OF it_data WITH HEADER LINE.
  RANGES: r_erpid FOR ztpperm-erpid.

  r_erpid-option = 'EQ'.
  r_erpid-sign = 'I'.
  r_erpid-low = 'E01'.
  APPEND r_erpid.
  r_erpid-low = 'E02'.
  APPEND r_erpid.
  r_erpid-low = 'E03'.
  APPEND r_erpid.
  r_erpid-low = 'E05'.
  APPEND r_erpid.

  SELECT tseq erpid rseq eitem eassyid rdate rtime block_srl_no
         crank_srl_no head_srl_n head_srl_n_rh en_reserve_02
    INTO CORRESPONDING FIELDS OF TABLE lt_temp
    FROM ztpperm
    WHERE rdate IN s_date
     AND erpid IN r_erpid.
  IF sy-subrc = 0.
    LOOP AT lt_temp.
      CASE lt_temp-erpid.
        WHEN 'E01' OR 'E02' OR 'E03'.
          IF lt_temp-eassyid+11(1) = '8'.
            it_data = lt_temp.
            it_data-ZUSER = sy-uname.
            it_data-cre_date = sy-datum.
            it_data-cre_time = sy-uzeit.
            APPEND it_data.
            CLEAR: it_data.
          ENDIF.
        WHEN 'E05'.
          IF lt_temp-block_srl_no+5(10) = '1111111111' OR
             lt_temp-block_srl_no+5(10) = '__________' OR
             lt_temp-crank_srl_no+5(10) = '1111111111' OR
             lt_temp-crank_srl_no+5(10) = '__________' OR
             lt_temp-head_srl_n+5(10)  = '1111111111' OR
             lt_temp-head_srl_n+5(10)  = '__________' OR
             lt_temp-head_srl_n_rh+5(10)  = '1111111111' OR
             lt_temp-head_srl_n_rh+5(10)  = '__________'.
            it_data = lt_temp.
            it_data-ZUSER = sy-uname.
            it_data-cre_date = sy-datum.
            it_data-cre_time = sy-uzeit.
            APPEND it_data.
            CLEAR: it_data.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDIF.

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

  MODIFY ztpperm_3c_dummy FROM TABLE it_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  PERFORM send_email.
ENDFORM.                    "save_send_data

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
  SET TITLEBAR 'T800'.

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

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

*                                  'S' 'MAKTX'        ' ',
*                                  ' ' 'COLTEXT'     'Description',
*                                  'E' 'OUTPUTLEN'   '45',
*
*                                  'S' 'UDATE'       ' ',
*                                  ' ' 'COLTEXT'     'Ch. Date',
*                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'PLANT'       ' ',
                                  ' ' 'COLTEXT'     'Plant',
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

                                  'S' 'IN_OUT'       ' ',
                                  ' ' 'COLTEXT'     'In or Out',
                                  'E' 'OUTPUTLEN'   '30',


                                  'S' 'EDGE'       ' ',
                                  ' ' 'COLTEXT'     'Edge',
                                  'E' 'OUTPUTLEN'   '30',


                                  'S' 'ERSDA'       ' ',
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
        l_p_rec_type  LIKE  somlreci1-rec_type.

  MOVE 'Following items with Dummy engines' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '==================================' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'T SEQ' TO lt_body+0(5),
        'RP ID' TO lt_body+5(5),
        'Sub SEQ' TO lt_body+10(10),
        'Engine Item Code' TO lt_body+20(20),
        'Engine Assy ID' TO lt_body+40(20),
        'RP Date' TO lt_body+60(10),
        'RP Time' TO lt_body+70(10),
        'Block SRL NO' TO lt_body+80(25),
        'Crank SRL No' TO lt_body+105(25),
        'Head SRL No LH' TO lt_body+130(25),
        'Head SRL No LR' TO lt_body+155(25),
        'Engine Plant Code' TO lt_body+155(10).

  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '-----' TO lt_body+0(5),
       '------' TO lt_body+5(5),
       '----------' TO lt_body+10(10),
       '--------------------' TO lt_body+20(20),
       '--------------------' TO lt_body+40(20),
       '----------' TO lt_body+60(10),
       '----------' TO lt_body+70(10),
       '-------------------------' TO lt_body+80(25),
       '-------------------------' TO lt_body+105(25),
       '-------------------------' TO lt_body+130(25),
       '-------------------------' TO lt_body+155(25),
       '-------------------------' TO lt_body+155(10).

  APPEND lt_body.
  CLEAR: lt_body.

  LOOP AT it_data.
    MOVE: it_data-tseq TO lt_body+0(5),
          it_data-erpid TO lt_body+5(5),
          it_data-rseq TO lt_body+10(10),
          it_data-eitem TO lt_body+20(20),
          it_data-eassyid TO lt_body+40(20),
          it_data-rdate TO lt_body+60(10),
          it_data-rtime TO lt_body+70(10),
          it_data-block_srl_no TO lt_body+80(25),
          it_data-crank_srl_no TO lt_body+105(25),
          it_data-head_srl_n TO lt_body+130(25),
          it_data-head_srl_n_rh TO lt_body+155(25),
          it_data-en_reserve_02 TO lt_body+155(10).
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
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data .
  s_date-option = 'EQ'.
  s_date-sign = 'I'.
  s_date-low = sy-datum - 1.
  APPEND s_date.
ENDFORM.                    " INIT_DATA
