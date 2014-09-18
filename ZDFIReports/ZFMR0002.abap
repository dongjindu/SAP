*&----------------------------------------------------------------------
*& Development ID : ZRFIF02
*& Program ID     : ZFMR0002
*& Program Name   : Budget Detail Report
*& Created by     : Byung Sung Bae
*& Created on     : 07/28/2005
*& Reference Pgm  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*&
*&----------------------------------------------------------------------

REPORT  zfmr0002 LINE-SIZE 170 LINE-COUNT 58
                 NO STANDARD PAGE HEADING.
INCLUDE <icon>.
INCLUDE zfm_auth_form.

TABLES: bppe, fmfctr, fmci.
type-pools zfmcm.
DATA: zsfm0009_9000 LIKE zsfm0009_9000.

*---// Internal tables
DATA: it_9000 TYPE STANDARD TABLE OF zsfm0009_9000 WITH HEADER LINE.

*---// Ranges
RANGES: r_geber  FOR bppe-geber.

*---// Constants
CONSTANTS: c_original   TYPE i              VALUE  1,
           c_supplement TYPE i              VALUE  2,
           c_transfer   TYPE i              VALUE  3,
           c_return     TYPE i              VALUE  4,
           c_current    TYPE i              VALUE  5,
           c_released   TYPE i              VALUE  6,
           c_commitment TYPE i              VALUE  7,
           c_invoice    TYPE i              VALUE  8,
           c_payments   TYPE i              VALUE  9,
           c_available  TYPE i              VALUE 10,
           c_unknown    TYPE i              VALUE 11.

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

CONSTANTS: c_structure(100) VALUE 'ZSFM0009_'.

DATA : l_fund TYPE bp_geber VALUE '',
       l_fictr TYPE fistl VALUE '',
       l_fipex TYPE fm_fipex VALUE ''.
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

*---// Selection screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_fikrs LIKE fmci-fikrs    OBLIGATORY DEFAULT zfmcm_fm_area,
            p_gjahr LIKE fmci-gjahr    OBLIGATORY DEFAULT sy-datum(4),
            p_perio TYPE fc_tperi      OBLIGATORY DEFAULT sy-datum+4(2).

SELECT-OPTIONS: s_fictr  FOR fmfctr-fictr,
                s_fipex  FOR fmci-fipex,
                s_potyp  FOR fmci-potyp DEFAULT '3'.
SELECTION-SCREEN END OF BLOCK bl1.

INITIALIZATION.
  SET PARAMETER ID 'FIC' FIELD l_fund.
  SET PARAMETER ID 'FIS' FIELD l_fictr.
  SET PARAMETER ID 'FPS' FIELD l_fipex.
  PERFORM initialization.

*---// Input value check & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM user_auth_check TABLES s_fictr[]
                          USING p_fikrs.
  check g_auth_check is INITIAL.
  PERFORM read_data.

START-OF-SELECTION.
  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data .
  DATA: lt_budget LIKE zsfm0008 OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'Z_FM_GET_MONTHLY_BUDGET'
    EXPORTING
      i_fikrs            = p_fikrs
      i_gjahr            = p_gjahr
    TABLES
      t_geber            = r_geber
      t_fictr            = s_fictr
      t_fipex            = s_fipex
      t_potyp            = s_potyp
      t_itab             = lt_budget
    EXCEPTIONS
      no_fm_area         = 1
      no_fund            = 2
      no_funds_center    = 3
      no_commitment_item = 4
      no_profile         = 5
      no_category        = 6
      no_original        = 7
      OTHERS             = 8.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE e000(zz) WITH text-m02.
    WHEN 2.
      MESSAGE e000(zz) WITH text-m03.
    WHEN 3.
      MESSAGE e000(zz) WITH text-m04.
    WHEN 4.
      MESSAGE e000(zz) WITH text-m05.
    WHEN 5.
      MESSAGE e000(zz) WITH text-m06.
    WHEN 6.
      MESSAGE e000(zz) WITH text-m07.
    WHEN 7.
      MESSAGE e000(zz) WITH text-m08.
    WHEN 8.
      MESSAGE e000(zz) WITH text-m09.
  ENDCASE.

  LOOP AT lt_budget.
    CLEAR: it_9000.

    MOVE: lt_budget-geber  TO it_9000-geber,
          lt_budget-fictr  TO it_9000-fictr,
          lt_budget-fipex  TO it_9000-fipex,
          lt_budget-bezei  TO it_9000-bezei,
          lt_budget-profil TO it_9000-profil,
          lt_budget-potyp  TO it_9000-potyp,
          lt_budget-waers  TO it_9000-waers.

    PERFORM calculate_amount USING lt_budget.

    CASE lt_budget-ctgry.
      WHEN c_original.
        MOVE: lt_budget-total TO it_9000-original.
      WHEN c_supplement.
        MOVE: lt_budget-total TO it_9000-supplement.
      WHEN c_transfer.
        MOVE: lt_budget-total TO it_9000-transfer.
      WHEN c_return.
        MOVE: lt_budget-total TO it_9000-return.
      WHEN c_current.
        MOVE: lt_budget-total TO it_9000-current.
      WHEN c_released.
        MOVE: lt_budget-total TO it_9000-released.
      WHEN c_commitment.
        MOVE: lt_budget-total TO it_9000-commitment.
      WHEN c_invoice.
        MOVE: lt_budget-total TO it_9000-invoice.
      WHEN c_payments.
        MOVE: lt_budget-total TO it_9000-payments.
      WHEN c_available.
        MOVE: lt_budget-total TO it_9000-available.
      WHEN c_unknown.
    ENDCASE.

    COLLECT it_9000.
  ENDLOOP.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .
  MOVE: 'I'   TO r_geber-sign,
        'EQ'  TO r_geber-option,
        space TO r_geber-low.

  APPEND r_geber.
ENDFORM.                    " initialization
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
    WHEN 'EXIT' OR 'CANC'.
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
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
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
         EXPORTING container_name = <control>
         EXCEPTIONS
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
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
         EXPORTING i_parent      = <container>
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
                                  'S' 'FICTR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'FIPEX'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'BEZEI'       ' ',
                                  'E' 'EMPHASIZE'   'C200',

                                  'S' 'PROFIL'      ' ',
                                  'E' 'EMPHASIZE'   'C200',

                                  'S' 'POTYP'       ' ',
                                  'E' 'EMPHASIZE'   'C200',

                                  'S' 'ORIGINAL'    ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'SUPPLEMENT'  ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'TRANSFER'    ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'RETURN'      ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'CURRENT'     ' ',
                                  'E' 'EMPHASIZE'   'C100',

                                  'S' 'RELEASED'    ' ',
                                  'E' 'EMPHASIZE'   'C100',

                                  'S' 'COMMITMENT'  ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'INVOICE'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'PAYMENTS'    ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'AVAILABLE'   ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'WAERS'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'GEBER'       ' ',
                                  'E' 'NO_OUT'      'X'.

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
*      MESSAGE e000(zz) WITH 'Check filed catalog'.
      MESSAGE e000(zz) WITH text-m01.
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
*&      Form  calculate_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_BUDGET  text
*----------------------------------------------------------------------*
FORM calculate_amount USING ps_budget STRUCTURE zsfm0008.
  DATA: lv_loop_cnt(2) TYPE n,
        lv_month_f(2)  TYPE n,
        lv_month_t(2)  TYPE n,
        lv_amount(50).

  FIELD-SYMBOLS: <amount>.

  CASE ps_budget-profil.
    WHEN 'M' or 'B'.
      lv_month_f = lv_month_t = p_perio.
    WHEN 'Q'.
      CASE p_perio.
        WHEN '001' OR '002' OR '003'.
          lv_month_f = '01'. lv_month_t = '03'.
        WHEN '004' OR '005' OR '006'.
          lv_month_f = '04'. lv_month_t = '06'.
        WHEN '007' OR '008' OR '009'.
          lv_month_f = '07'. lv_month_t = '09'.
        WHEN '010' OR '011' OR '012'.
          lv_month_f = '10'. lv_month_t = '12'.
      ENDCASE.
    WHEN 'H'.
      CASE p_perio.
        WHEN '001' OR '002' OR '003' OR '004' OR '005' OR '006'.
          lv_month_f = '01'. lv_month_t = '06'.
        WHEN '007' OR '008' OR '009' OR '010' OR '011' OR '012'.
          lv_month_f = '07'. lv_month_t = '12'.
      ENDCASE.
    WHEN 'Y'.
      lv_month_f = '01'. lv_month_t = '12'.
  ENDCASE.

  CLEAR: ps_budget-total.

  DO 12 TIMES.
    MOVE: sy-index TO lv_loop_cnt.

    CHECK lv_loop_cnt >= lv_month_f AND
          lv_loop_cnt <= lv_month_t.

    CONCATENATE 'PS_BUDGET-WTP' lv_loop_cnt INTO lv_amount.
    ASSIGN (lv_amount) TO <amount>.

    ps_budget-total = ps_budget-total + <amount>.
  ENDDO.
ENDFORM.                    " calculate_amount
