*&---------------------------------------------------------------------*
*& Program ID     : ZFMC0009                                           *
*& Program Name   : Fund Budget upload using Excel                     *
*& Created by     : YN.Kim                                             *
*& Created on     : 08/18/2011                                         *
*& Reference Pgm  :                                                    *
*&                                                                     *
*& Modification Log                                                    *
*----------------------------------------------------------------------*
* DATE      |  NAME          |Transport | Issue #  |      DESC         *
*----------------------------------------------------------------------*
*                                                                      *
*&=====================================================================*
REPORT  ZFMC0009 NO STANDARD PAGE HEADING
                             MESSAGE-ID  zmfi.
*&

TABLES: zsfm0007, fmfincode.
type-pools zfmcm.
INCLUDE <icon>.

DATA: zsfm0008_9000 LIKE zsfm0008_9000.

*-----/// Internal tables
DATA: it_9000 TYPE STANDARD TABLE OF zsfm0008_9000
                                     WITH HEADER LINE.

DATA: BEGIN OF it_fictr OCCURS 0,
        fictr   LIKE   fmdy-fictr,
      END   OF it_fictr.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

*-----/// Global variable
DATA: v_selected   TYPE   i.

*-----/// Constants
DATA: c_euro   LIKE   bkpf-waers   VALUE   zfmcm_euro.
CONSTANTS: c_true                           VALUE 'X',
           c_yes                            VALUE 'J'.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

DATA: v_container(50),
      v_control(50),
      v_alv(50),
      v_itab(50),
      v_ans,
      v_work,
      v_structure LIKE dd02l-tabname.

FIELD-SYMBOLS: <container> TYPE REF TO cl_gui_custom_container,
               <control>   TYPE        scrfname,
               <alv>       TYPE REF TO cl_gui_alv_grid,
               <itab>      TYPE STANDARD TABLE.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS lcl_event_receiver DEFINITION DEFERRED. "/ALV Event Handling

DATA : event_receiver TYPE REF TO lcl_event_receiver.

* Interal tables for ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

* Global variable for ALV GRID
DATA : v_is_layout TYPE lvc_s_layo,
       v_variant   TYPE disvariant,          "for parameter IS_VARIANT
       v_fieldname LIKE LINE OF it_fieldname,
       v_repid     LIKE sy-repid,
       v_cnt       TYPE i,                   "Field count
       v_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE

CONSTANTS: c_structure(100) VALUE 'ZSFM0008_'.

*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

*-----/// ALV Control : END

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

START-OF-SELECTION.
  SET PARAMETER ID 'FIK' FIELD zfmcm_fm_area.
  PERFORM initial_value.

  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
  ENDCASE.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC' or 'BACK'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'UPLOAD'.
      CLEAR: sy-ucomm.
      PERFORM upload_rtn.
    WHEN 'GENERATION'.
      CLEAR: sy-ucomm.
      PERFORM generate_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  PERFORM create_alv_object USING sy-dynnr.
ENDMODULE.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_alv_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM create_alv_object USING p_dynnr.
  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO v_container.
  ASSIGN:      (v_container)           TO   <container>.

  IF <container> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM create_container_n_object USING p_dynnr.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
    PERFORM sssign_event USING p_dynnr.
  ELSE.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
  ENDIF.
ENDFORM.                    " create_alv_object
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM create_container_n_object USING p_dynnr.
*- Create Container('GRID_CONTAINER') with Custom Control on screen

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO v_container,
               'WC_CONTROL_'   p_dynnr INTO v_control,
               'WC_ALV_'       p_dynnr INTO v_alv.

  ASSIGN: (v_container) TO <container>,
          (v_control)   TO <control>,
          (v_alv)       TO <alv>.

  CREATE OBJECT <container>
    EXPORTING
      container_name              = <control>
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT <alv>
    EXPORTING
      i_parent      = <container>
      i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid USING p_dynnr.
  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_attributes_alv_9000.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9000.
  CLEAR : v_is_layout, v_variant.

  v_is_layout-edit       = ' '.      "/Edit Mode Enable
*  v_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  v_is_layout-language   = sy-langu. "/Language Key
  v_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
  v_is_layout-no_merging = 'X'.      "/Disable cell merging
  v_variant-report       = sy-repid.
  v_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9000
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_dynnr.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname USING p_dynnr.
  PERFORM set_screen_fields USING p_dynnr.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  set_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_fieldname USING p_dynnr.
  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  MOVE: sy-repid TO v_repid.
  CONCATENATE c_structure p_dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = v_repid
      i_internal_tabname = lw_itab
      i_inclname         = v_repid
    CHANGING
      ct_fieldcat        = it_fieldname.
ENDFORM.                    " set_fieldname
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_screen_fields USING p_dynnr.
  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_screen_fields_9000.
  ENDCASE.
ENDFORM.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9000.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'FUND'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'FICTR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'FIPEX'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X'.
ENDFORM.                    " set_screen_fields_9000
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv USING p_dynnr.
  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv,
               c_structure  p_dynnr      INTO v_structure,
               'IT_'        p_dynnr '[]' INTO v_itab.

  ASSIGN: (v_alv)       TO <alv>,
          (v_itab)      TO <itab>.

  CALL METHOD <alv>->set_table_for_first_display
    EXPORTING
      i_structure_name = v_structure
      is_layout        = v_is_layout
      i_save           = v_save
      is_variant       = v_variant
      i_default        = space
    CHANGING
      it_fieldcatalog  = it_fieldcat[]
      it_outtab        = <itab>.
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event USING p_dynnr.
  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

*--  Regist event for Edit
  IF sy-batch IS INITIAL.
    CALL METHOD <alv>->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  ENDIF.
ENDFORM.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_1033   text
*      -->P_1034   text
*      -->P_1035   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : lv_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO v_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check filed catalog'.
    ENDIF.

    MOVE: v_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO lv_col.
  ASSIGN (lv_col) TO <fs>.
  MOVE   p_value  TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    IF p_fieldcat-col_pos IS INITIAL.
      ADD 1 TO v_cnt.
      p_fieldcat-col_pos = v_cnt.
    ENDIF.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  initial_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_value .
  CLEAR: zsfm0007.

  MOVE: zfmcm_fm_area        TO zsfm0007-fikrs,
        sy-datum(4)          TO zsfm0007-gjahr,
        'c:\temp\budget.xls' TO zsfm0007-fname.

*---// BDC MODE, DEFAULT SIZE, UPDATE MODE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
ENDFORM.                    " initial_value
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_rtn .
  DATA: BEGIN OF lt_excel OCCURS 0.
          INCLUDE STRUCTURE alsmex_tabline.
  DATA: END OF lt_excel.

  DATA: l_total LIKE it_9000-per01.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = zsfm0007-fname
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 16
      i_end_row               = 40000
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: it_9000, it_9000[].
  CLEAR: zsfm0007-total,   zsfm0007-ready,
         zsfm0007-success, zsfm0007-error.

  SORT lt_excel BY row col.
  LOOP AT lt_excel.
    CASE lt_excel-col.
      WHEN  1.
        MOVE: lt_excel-value TO it_9000-fund.
*      WHEN  2.
*        MOVE: lt_excel-value TO it_9000-fictr.
*      WHEN  3.
*        MOVE: lt_excel-value TO it_9000-fipex.
*      WHEN  4.
*        MOVE: lt_excel-value TO it_9000-bezei.
*      WHEN  5.
*        MOVE: lt_excel-value TO it_9000-sgtxt.
      WHEN  2.
        MOVE: lt_excel-value TO it_9000-per01.
      WHEN  3.
        MOVE: lt_excel-value TO it_9000-per02.
      WHEN  4.
        MOVE: lt_excel-value TO it_9000-per03.
      WHEN  5.
        MOVE: lt_excel-value TO it_9000-per04.
      WHEN  6.
        MOVE: lt_excel-value TO it_9000-per05.
      WHEN  7.
        MOVE: lt_excel-value TO it_9000-per06.
      WHEN  8.
        MOVE: lt_excel-value TO it_9000-per07.
      WHEN  9.
        MOVE: lt_excel-value TO it_9000-per08.
      WHEN 10.
        MOVE: lt_excel-value TO it_9000-per09.
      WHEN 11.
        MOVE: lt_excel-value TO it_9000-per10.
      WHEN 12.
        MOVE: lt_excel-value TO it_9000-per11.
      WHEN 13.
        MOVE: lt_excel-value TO it_9000-per12.
      WHEN 14.
        MOVE: lt_excel-value TO it_9000-sgtxt.

    ENDCASE.

    AT END OF row.
*>
      CLEAR l_total.
      l_total = it_9000-per01 + it_9000-per02 + it_9000-per03
              + it_9000-per04 + it_9000-per05 + it_9000-per06
              + it_9000-per07 + it_9000-per08 + it_9000-per09
              + it_9000-per10 + it_9000-per11 + it_9000-per12.
      IF l_total = 0.
        zsfm0007-total = zsfm0007-total + 1.
        zsfm0007-error = zsfm0007-error + 1.
        MOVE: c_euro          TO it_9000-waers,
              'Zero Amount'   TO it_9000-zmsg,
              icon_led_red    TO it_9000-icon.
      ELSE.
*<
        zsfm0007-total = zsfm0007-total + 1.
        zsfm0007-ready = zsfm0007-ready + 1.
        MOVE: c_euro          TO it_9000-waers,
              icon_led_yellow TO it_9000-icon.
      ENDIF.
      it_9000-fictr = zfmcm_fmctr_h9990.
      it_9000-fipex = zfmcm_fipex_invest.
      APPEND it_9000.
      CLEAR: it_9000.
    ENDAT.
  ENDLOOP.

  DELETE it_9000 WHERE fund EQ space
                    OR fictr EQ space
                    OR fipex EQ space.

  CLEAR: zsfm0007-total,   zsfm0007-ready,
         zsfm0007-success, zsfm0007-error.
  LOOP AT it_9000.
    SELECT SINGLE * FROM fmfincode
                    WHERE fikrs = zfmcm_fm_area
                      AND fincode = it_9000-fund.

    IF sy-subrc NE 0.
      MOVE: icon_led_red    TO it_9000-icon.
      MODIFY it_9000.
    ENDIF.
    ADD 1       TO zsfm0007-total.
    CASE it_9000-icon.
      WHEN icon_led_yellow.
        ADD 1   TO zsfm0007-ready.
      WHEN icon_led_red.
        ADD 1   TO zsfm0007-error.
      WHEN icon_led_green.
        ADD 1   TO zsfm0007-success.
    ENDCASE.
  ENDLOOP.

  IF NOT zsfm0007-error IS INITIAL.
    MESSAGE i000(zz) WITH
*        'Count Error Data ( Amount 0 ) :' zsfm0007-error.
        text-m02 zsfm0007-error.
  ENDIF.

ENDFORM.                    " UPLOAD_RTN
*&---------------------------------------------------------------------*
*&      Form  GENERATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_rtn .

  DATA: text1(40),
        text2(40),
        titl(40).

  text1 = text-m04.
  text2 = text-m05.
  titl  = text-m06.
  PERFORM popup_confirm   USING text1 text2 titl
                                'X'   'A'   v_ans.
  CHECK v_ans = c_yes.

*  PERFORM get_fund_center.

  CLEAR: v_selected.

  LOOP AT it_9000 WHERE icon eq icon_led_yellow.
    DATA: l_total LIKE it_9000-per01.
    CLEAR l_total.
    l_total = it_9000-per01 + it_9000-per02 + it_9000-per03
            + it_9000-per04 + it_9000-per05 + it_9000-per06
            + it_9000-per07 + it_9000-per08 + it_9000-per09
            + it_9000-per10 + it_9000-per11 + it_9000-per12.
    IF l_total = 0.
      CONTINUE.
    ENDIF.

    PERFORM generate_bdc.
    PERFORM posting_rtn.
  ENDLOOP.

  PERFORM count_rtn.
ENDFORM.                    " GENERATE_RTN
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc .
  DATA: lv_peri1_01(21), lv_peri1_02(21), lv_peri1_03(21),
        lv_peri1_04(21), lv_peri1_05(21), lv_peri1_06(21),
        lv_peri1_07(21), lv_peri1_08(21), lv_peri1_09(21),
        lv_peri1_10(21), lv_peri1_11(21), lv_peri1_12(21).

  CLEAR: it_bdc, it_bdc[].
  DATA : l_date TYPE dats.
  CONCATENATE zsfm0007-gjahr '0101' INTO l_date.
  PERFORM dynpro USING:
     'X' 'SAPLKBPB'    '0200',
     ' ' 'FMDY-FINCODE' it_9000-fund,   "Fund
     ' ' 'FMDY-FIKRS'  zsfm0007-fikrs,  "FM Area
     ' ' 'BPDY-GJAHR'  zsfm0007-gjahr,  "Fiscal year
     ' ' 'BPDY-BLDAT'  l_date,
     ' ' 'BPDY-VERSN'  zsfm0007-versn,  "Version
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLKBPB'    '0400',               "Detail screen
     ' ' 'BDC_OKCODE'  '=DOCH',              "Header

     'X' 'SAPLKBPB'    '0150',               "Header
     ' ' 'BPDY-SGTXT'  it_9000-sgtxt,        "Text
     ' ' 'BDC_OKCODE'  '/00'.

  CLEAR v_work.

  v_work = c_true.
  v_selected = v_selected + 1.

  WRITE: it_9000-per01 TO lv_peri1_01(21) CURRENCY c_euro,
         it_9000-per02 TO lv_peri1_02(21) CURRENCY c_euro,
         it_9000-per03 TO lv_peri1_03(21) CURRENCY c_euro,
         it_9000-per04 TO lv_peri1_04(21) CURRENCY c_euro,
         it_9000-per05 TO lv_peri1_05(21) CURRENCY c_euro,
         it_9000-per06 TO lv_peri1_06(21) CURRENCY c_euro,
         it_9000-per07 TO lv_peri1_07(21) CURRENCY c_euro,
         it_9000-per08 TO lv_peri1_08(21) CURRENCY c_euro,
         it_9000-per09 TO lv_peri1_09(21) CURRENCY c_euro,
         it_9000-per10 TO lv_peri1_10(21) CURRENCY c_euro,
         it_9000-per11 TO lv_peri1_11(21) CURRENCY c_euro,
         it_9000-per12 TO lv_peri1_12(21) CURRENCY c_euro.

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'         '0400',           "Detail Screen
     ' ' 'G_TABLECON_INFO-MARK(01)' 'X',      "Check
     ' ' 'BDC_OKCODE'       '=INSL',          "Insert

     'X' 'SAPLKBPB'         '0400',           "Detail Screen
     ' ' 'FMDY-FICTR(01)'   it_9000-fictr,    "Fund Center
     ' ' 'FMDY-FIPEX(01)'   it_9000-fipex,    "Commitment Item
     ' ' 'BPDY-SPRED1(01)'  '0',              "DK
     ' ' 'G_TABLECON_INFO-MARK(01)' 'X',      "Check
     ' ' 'BDC_OKCODE'       '=PERI',

     'X' 'SAPLKBPP'         '0600',           "Period Screen
     ' ' 'BPDY-PERI1(01)'   lv_peri1_01,      " 01
     ' ' 'BPDY-PERI1(02)'   lv_peri1_02,      " 02
     ' ' 'BPDY-PERI1(03)'   lv_peri1_03,      " 03
     ' ' 'BPDY-PERI1(04)'   lv_peri1_04,      " 04
     ' ' 'BPDY-PERI1(05)'   lv_peri1_05,      " 05
     ' ' 'BPDY-PERI1(06)'   lv_peri1_06,      " 06
     ' ' 'BPDY-PERI1(07)'   lv_peri1_07,      " 07
     ' ' 'BPDY-PERI1(08)'   lv_peri1_08,      " 08
     ' ' 'BPDY-PERI1(09)'   lv_peri1_09,      " 09
     ' ' 'BPDY-PERI1(10)'   lv_peri1_10,      " 10
     ' ' 'BPDY-PERI1(11)'   lv_peri1_11,      " 11
     ' ' 'BPDY-PERI1(12)'   lv_peri1_12,      " 12
     ' ' 'G_SCREEN_0600-DK' '0',              "DK
     ' ' 'BDC_OKCODE'  '=CLOS'.


  PERFORM dynpro USING:
     'X' 'SAPLKBPB'         '0400',           "Detail Screen
     ' ' 'BDC_OKCODE'       '=POST'.          "Save

ENDFORM.                    " GENERATE_BDC
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1212   text
*      -->P_1213   text
*      -->P_1214   text
*----------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-program,
          value TO it_bdc-dynpro,
          dynbegin TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  get_err_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9001_MSG  text
*----------------------------------------------------------------------*
FORM get_err_msg USING pw_msg.
  DATA: lw_msg LIKE cfgnl-msglin.

  CALL FUNCTION 'RKC_MSG_STRING'
    EXPORTING
      id      = sy-msgid
      mtype   = sy-msgty
      number  = sy-msgno
      par1    = sy-msgv1
      par2    = sy-msgv2
      par3    = sy-msgv3
      par4    = sy-msgv4
    IMPORTING
      msg_lin = lw_msg.

  MOVE: lw_msg TO pw_msg.
ENDFORM.                    " get_err_msg
*&---------------------------------------------------------------------*
*&      Form  posting_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_rtn .
  DATA: l_total LIKE it_9000-per01,
        l_msg   LIKE it_9000-zmsg,
        l_belnr LIKE it_9000-belnr.

  CHECK v_work EQ c_true.

  CLEAR: l_msg, l_belnr.

  CALL TRANSACTION 'FR50'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '043'.
    PERFORM get_err_msg USING l_msg.
    MOVE: icon_led_red TO it_9000-icon,
          l_msg        TO it_9000-zmsg.

    MODIFY it_9000.   CLEAR it_9000.
  ELSE.
    MOVE: sy-msgv1       TO l_belnr,
          icon_led_green          TO it_9000-icon,
          l_belnr                 TO it_9000-belnr,
          l_msg                   TO it_9000-zmsg.
    MODIFY it_9000.  CLEAR it_9000.
  ENDIF.
ENDFORM.                    " posting_rtn
*&---------------------------------------------------------------------*
*&      Form  GET_FUND_CENTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fund_center .
*  CLEAR: it_fictr, it_fictr[].
*
*  LOOP AT it_9000 WHERE icon = icon_led_yellow
*                     OR icon = icon_led_red.
*    MOVE: it_9000-fictr TO it_fictr-fictr.
*
*    COLLECT it_fictr.
*  ENDLOOP.
ENDFORM.                    " GET_FUND_CENTER
*&---------------------------------------------------------------------*
*&      Form  count_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM count_rtn .
  CLEAR: zsfm0007-ready,
         zsfm0007-error,
         zsfm0007-success,
         zsfm0007-total.

  LOOP AT it_9000.
    ADD 1      TO zsfm0007-total.
    CASE it_9000-icon.
      WHEN icon_led_yellow.
        ADD 1      TO zsfm0007-ready.
      WHEN icon_led_red.
        ADD 1      TO zsfm0007-error.
      WHEN icon_led_green.
        ADD 1      TO zsfm0007-success.
    ENDCASE.
  ENDLOOP.

  MESSAGE s000(zz) WITH v_selected text-m03.
ENDFORM.                    " count_rtn
*&---------------------------------------------------------------------*
*&      Module  versn_pov  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE versn_pov INPUT.
  PERFORM versn_pov.
ENDMODULE.                 " versn_pov  INPUT
*&---------------------------------------------------------------------*
*&      Module  fname_pov  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fname_pov INPUT.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
*      mask          = '*.xls'
      static        = 'X'
    CHANGING
      file_name     = zsfm0007-fname
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
ENDMODULE.                 " fname_pov  INPUT
*&---------------------------------------------------------------------*
*&      Form  versn_pov
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM versn_pov.
  DATA: BEGIN OF lt_dynp OCCURS 0.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF lt_dynp.

  DATA: lv_objhi   LIKE bpin-objhi,
        lv_version LIKE bpin-versn,
        lv_fikrs   LIKE bpin-fikrs.

  MOVE  'ZSFM0007-FIKRS' TO lt_dynp-fieldname.
  APPEND lt_dynp.

*----Manueller Feldtransport
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynp.

  READ TABLE lt_dynp INDEX 1.
  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  MOVE lt_dynp-fieldvalue TO lv_fikrs.

  CALL FUNCTION 'KBPS_OBJHI_NAME_GENERATE'
    EXPORTING
      i_application            = 'M'
      i_fm_area                = lv_fikrs
      i_object_initial_allowed = 'X'
    IMPORTING
      e_objhi                  = lv_objhi.


*----Für alle Fonds sollen im Benutzerdialog
*    benötigten Versionen ausgewählt werden können
  CALL FUNCTION 'KBPS_SHOW_VERSIONS'
    EXPORTING
      applik       = 'M'
      object       = lv_objhi
      wrttp        = '43'
      geber_all    = 'X'
      im_kokrs     = ' '
      im_cop_versn = ' '
    IMPORTING
      version      = lv_version
    EXCEPTIONS
      no_versions  = 1.

  IF sy-subrc IS INITIAL.
    MOVE lv_version TO zsfm0007-versn.
  ENDIF.
ENDFORM.                    " versn_pov
*&---------------------------------------------------------------------*
*&      Form  popup_confirm
*&---------------------------------------------------------------------*
FORM popup_confirm USING   value(p_txt1)
                           value(p_txt2)
                           value(p_titl)
                           value(p_cancel_flg)
                           value(p_def_val)
                           p_ans.

  CLEAR p_ans.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = p_def_val
      textline1      = p_txt1
      textline2      = p_txt2
      titel          = p_titl
      cancel_display = p_cancel_flg
    IMPORTING
      answer         = p_ans.

ENDFORM.                    " POPUP_CONFIRM
