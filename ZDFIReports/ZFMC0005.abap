*&----------------------------------------------------------------------
*& Program ID     : ZFMC0005
*& Program Name   : FM Control Board (Posting Parked Documents)
*& Created by     : YN.Kim
*& Created on     : 19/08/2011
*& Reference Pgm  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*&
*&----------------------------------------------------------------------
REPORT  zfmc0005                                .

TABLES: bpdk,
        bpbk,
        bpej,
        bpep,
        fmci,
        fmfctr.

type-pools zfmcm.
INCLUDE <icon>.

*---// Constants
CONSTANTS: c_versn      LIKE bppe-versn     VALUE zfmcm_versn_0,      "Version
           c_fikrs      LIKE fmci-fikrs     VALUE zfmcm_fm_area,
           c_eur        LIKE bppe-twaer     VALUE zfmcm_euro,
           c_text_ap(12)                    VALUE 'Release(+)',
           c_text_am(12)                    VALUE 'Release(-)',
           c_text_s(12)                     VALUE 'Supplement',
           c_text_t(12)                     VALUE 'Transfer',
           c_text_r(12)                     VALUE 'Return',
           c_true                           VALUE 'X',
           c_yes                            VALUE 'J'.

*---// Internal tables
DATA: BEGIN OF st_sorc,
        bldat  LIKE bpdk-bldat,
        belnr  LIKE bpdk-belnr,
        wljhr  LIKE bpej-wljhr,
        waers  LIKE bpep-twaer,
        rpublaw LIKE bpdk-rpublaw,
        dtype(12),
        icon(04),
        docnr  LIKE bpdk-belnr,
        zmsg(100),
      END   OF st_sorc.

DATA: it_list LIKE st_sorc OCCURS 0 WITH HEADER LINE.
DATA: it_list_tar  LIKE st_sorc OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF st_doc,            " for reject.
        belnr  LIKE bpdk-belnr,
      END   OF st_doc.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

RANGES r_bldat  FOR bpdk-bldat.
*---// Global Variables
DATA: ok_code LIKE sy-ucomm,
      sv_code LIKE sy-ucomm.

DATA: v_total(5)    TYPE n,
      v_ready(5)    TYPE n,
      v_error(5)    TYPE n,
      v_success(5)  TYPE n,
      v_selected(5) TYPE n.

DATA: sw_err,
      v_ans.

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
*-----/// ALV Control : END
****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS : handle_double_click
                 FOR EVENT double_click OF cl_gui_alv_grid
                 IMPORTING e_row
                           e_column
                           es_row_no.
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM double_click USING e_row e_column es_row_no.
  ENDMETHOD.                    "handle_double_click

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*---// Selection screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_belnr   FOR bpdk-belnr,
                s_datum   FOR sy-datum.
PARAMETERS: p_gjahr       LIKE bpbk-gjahr DEFAULT sy-datum+0(4).
PARAMETERS: p_mode        LIKE wa_opt-dismode.
SELECTION-SCREEN END OF BLOCK bl1.

INITIALIZATION.
  PERFORM initialization.
  SET PARAMETER ID 'FIK' FIELD zfmcm_fm_area.
*---// Input value check & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_condition.

START-OF-SELECTION.
  PERFORM move_parameters.
  PERFORM read_data.
  IF sw_err EQ c_true.
    CLEAR sw_err. EXIT.
  ENDIF.

  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
FORM initialization .

*---// BDC MODE, DEFAULT SIZE, UPDATE MODE
  wa_opt-defsize = 'X'.
  wa_opt-updmode = 'S'.
*  wa_opt-dismode = 'N'.
  wa_opt-dismode = p_mode = 'N'.


ENDFORM.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
FORM read_data .

  DATA: l_lin TYPE i.

  IF NOT s_datum[] IS INITIAL.
    r_bldat[] = s_datum[].
  ENDIF.

  IF NOT p_gjahr   IS INITIAL  AND
         r_bldat[] IS INITIAL.
    r_bldat = 'IBT'.
    r_bldat-low+0(4) = p_gjahr.
    r_bldat-low+4(4) = '0101'.
    r_bldat-high+0(4) = p_gjahr.
    r_bldat-high+4(4) = '1231'.
    APPEND r_bldat.
  ENDIF.

  PERFORM select_supl. "supplement
  PERFORM select_tran. " Transfer
  PERFORM select_retn. " release(-) Document
  PERFORM select_retn_rt. " Return

  DESCRIBE TABLE it_list LINES l_lin.
  IF l_lin < 1.
    sw_err = c_true.
    MESSAGE s000(zz) WITH text-m02.
    EXIT.
  ENDIF.

  v_total = v_ready = l_lin.

  SORT it_list BY bldat belnr.

ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  move_parameters
*&---------------------------------------------------------------------*
FORM move_parameters .

  wa_opt-dismode = p_mode.

ENDFORM.                    " move_parameters
*&---------------------------------------------------------------------*
*&      Form  check_condition
*&---------------------------------------------------------------------*
FORM check_condition .

  IF s_belnr[] IS INITIAL AND
     s_datum[] IS INITIAL AND
     p_gjahr   IS INITIAL.
    MESSAGE e000(zz) WITH text-m01.
*    sw_err = c_true.
  ENDIF.
ENDFORM.                    " check_condition
*&---------------------------------------------------------------------*
*&      Form  select_supl
*&---------------------------------------------------------------------*
FORM select_supl .

  DATA: lt_list LIKE st_sorc OCCURS 0 WITH HEADER LINE.

  SELECT a~bldat a~belnr b~belnr AS docnr c~wljhr
    INTO CORRESPONDING FIELDS OF TABLE lt_list
    FROM bpdk AS a INNER JOIN bpbk AS b
                      ON a~belnr  =  b~bpdk_belnr
                   INNER JOIN bpej AS c
                      ON b~belnr  =  c~belnr
                     AND c~buzei  =  '001'
                     AND c~versn  = zfmcm_versn_0
  WHERE a~belnr   IN s_belnr
*    AND a~bldat   IN s_datum
    AND a~bldat   IN r_bldat
    AND a~bstat   =  '1'
    AND b~vorga   = 'KBN0'
    AND c~objnr LIKE 'FSK201%'.

  lt_list-waers = c_eur.
  lt_list-dtype = c_text_s.
  lt_list-icon  = icon_led_yellow.

  MODIFY lt_list TRANSPORTING waers dtype icon WHERE waers = ''.

  APPEND LINES OF lt_list TO it_list.

ENDFORM.                    " select_supl
*&---------------------------------------------------------------------*
*&      Form  select_tran
*&---------------------------------------------------------------------*
FORM select_tran .

  DATA: lt_list LIKE st_sorc OCCURS 0 WITH HEADER LINE.
*  subquery
  SELECT a~bldat a~belnr a~rpublaw b~belnr AS docnr c~wljhr
    INTO CORRESPONDING FIELDS OF TABLE lt_list
    FROM bpdk AS a INNER JOIN bpbk AS b
                      ON a~belnr  =  b~bpdk_belnr
                   INNER JOIN bpej AS c
                      ON b~belnr  =  c~belnr
                     AND c~buzei  =  '001'
                     AND c~versn  = zfmcm_versn_0
  WHERE a~belnr IN s_belnr
*    AND a~bldat   IN s_datum
    AND a~bldat   IN r_bldat
    AND a~bstat   =  '1'
    AND EXISTS
        ( SELECT * FROM bpbk
                 WHERE bpdk_belnr = a~rpublaw
                   AND vorga  =  'KBFR'  )
    AND b~vorga   = '*KBU'
    AND c~objnr LIKE 'FSK201%'.


  lt_list-waers = c_eur.
  lt_list-dtype = c_text_t.
  lt_list-icon  = icon_led_yellow.

  MODIFY lt_list TRANSPORTING waers dtype icon WHERE waers = ''.

  APPEND LINES OF lt_list TO it_list.

ENDFORM.                    " select_tran
*&---------------------------------------------------------------------*
*&      Form  select_retn
*&---------------------------------------------------------------------*
FORM select_retn .

  DATA: lt_list LIKE st_sorc OCCURS 0 WITH HEADER LINE.

  SELECT a~bldat a~belnr b~belnr AS docnr c~wljhr
    INTO CORRESPONDING FIELDS OF TABLE lt_list
    FROM bpdk AS a INNER JOIN bpbk AS b
                      ON a~belnr  =  b~bpdk_belnr
                   INNER JOIN bpej AS c
                      ON b~belnr  =  c~belnr
                     AND c~buzei  =  '001'
                     AND c~versn  = zfmcm_versn_0
  WHERE a~belnr   IN s_belnr
    AND NOT EXISTS
        ( SELECT * FROM bpdk
                 WHERE rpublaw = a~belnr )
*    AND a~bldat   IN s_datum
    AND a~bldat   IN r_bldat
    AND a~bstat   =  '1'
    AND a~rpublaw = space
    AND b~vorga   = 'KBFR'
    AND c~objnr LIKE 'FSK201%'.

  lt_list-waers = c_eur.
  lt_list-dtype = c_text_r.
  lt_list-icon  = icon_led_yellow.

  MODIFY lt_list TRANSPORTING waers dtype icon WHERE waers = ''.

  APPEND LINES OF lt_list TO it_list.

ENDFORM.                    " select_retn
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.

  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
  ENDCASE.

ENDMODULE.                 " STATUS  OUTPUT
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
FORM create_alv_object  USING    p_dynnr.

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
FORM set_attributes_alv_9000.
  CLEAR : v_is_layout, v_variant.

  v_is_layout-edit       = ' '.      "/Edit Mode Enable
  v_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  v_is_layout-language   = sy-langu. "/Language Key
  v_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
  v_is_layout-no_merging = 'X'.      "/Disable cell merging
  v_variant-report       = sy-repid.
  v_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9000

*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
FORM build_field_catalog USING p_dynnr.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  CLEAR: it_fieldcat,  it_fieldcat[].

  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_screen_fields_9000.
  ENDCASE.

ENDFORM.                    " build_field_catalog

*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9000
*&---------------------------------------------------------------------*
FORM set_screen_fields_9000.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                    'S' 'FIELDNAME'   'BLDAT',
                    ' ' 'REF_FIELD'   'BLDAT',
                    ' ' 'REF_TABLE'   'BPDK',
                    'E' 'KEY'         'X',

                    'S' 'FIELDNAME'   'BELNR',
                    ' ' 'REF_FIELD'   'BELNR',
                    ' ' 'REF_TABLE'   'BPDK',
                    'E' 'KEY'         'X',

                    'S' 'FIELDNAME'   'DTYPE',
                    ' ' 'OUTPUTLEN'   '12',
                    'E' 'COLTEXT'     'Request Typ',

                    'S' 'FIELDNAME'   'ICON',
                    ' ' 'KEY'         'X',
                    'E' 'COLTEXT'     'Icon',

                    'S' 'FIELDNAME'   'WLJHR',
                    ' ' 'REF_FIELD'   'WLJHR',
                    ' ' 'REF_TABLE'   'BPEJ',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Amount',

*                    'S' 'FIELDNAME'   'DOCNR',
*                    ' ' 'REF_FIELD'   'BELNR',
*                    ' ' 'REF_TABLE'   'BPPK',
*                    ' ' 'NO_ZERO'     'X',
*                    'E' 'COLTEXT'     'Doc. No',

                    'S' 'FIELDNAME'   'ZMSG',
                    ' ' 'OUTPUTLEN'   '40',
                    'E' 'COLTEXT'     'Message'.

ENDFORM.                    " set_screen_fields_9000

*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING p_gub  p_fname  p_value.

  IF p_gub = 'S'.
    CLEAR p_fieldcat.
  ENDIF.

  DATA l_fname(40).
  FIELD-SYMBOLS <fs> TYPE ANY.
  CONCATENATE 'p_fieldcat-' p_fname INTO l_fname.

  ASSIGN (l_fname) TO <fs>.
  <fs> = p_value.

  IF p_gub = 'E'.
    APPEND p_fieldcat.
  ENDIF.

ENDFORM.                    " setting_fieldcat

*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
FORM assign_itab_to_alv USING p_dynnr.
  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv.

*  MOVE 'IT_SORC'        TO v_itab.
  ASSIGN: (v_alv)       TO <alv>.

  CALL METHOD <alv>->set_table_for_first_display
    EXPORTING
      is_layout       = v_is_layout
      i_save          = v_save
      is_variant      = v_variant
      i_default       = space
    CHANGING
      it_fieldcatalog = it_fieldcat[]
      it_outtab       = it_list[].

ENDFORM.                    " assign_itab_to_alv

*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
FORM sssign_event USING p_dynnr.

  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

*--  Regist event for Edit
  IF sy-batch IS INITIAL.
    CALL METHOD <alv>->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CREATE OBJECT event_receiver.
    SET HANDLER :
        event_receiver->handle_double_click  FOR <alv>.
  ENDIF.

ENDFORM.                    " sssign_event
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
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  sv_code = ok_code.
  CLEAR: ok_code.

  CASE sv_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXECUTE'.
      PERFORM execute_rtn.
    WHEN 'REJECT'.
      PERFORM reject_rtn.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  execute_rtn
*&---------------------------------------------------------------------*
FORM execute_rtn .

  DATA: lt_doc LIKE st_doc OCCURS 0 WITH HEADER LINE.

  DATA: text1(40),
        text2(40),
        titl(40),
        l_err,
        l_msg(60),
        l_msg2(60),
        l_docnr1 LIKE bpdk-belnr,
        l_docnr2 LIKE bpdk-belnr,
        l_docnr3 LIKE bpdk-belnr.

  PERFORM get_target_data USING sy-dynnr.

  text1 = text-m06.
*  text2 = text-m07.
  titl  = text-m08.
  PERFORM popup_confirm   USING text1 text2 titl
                                'X'   'A'   v_ans.
  CHECK v_ans = c_yes.

  LOOP AT it_list_tar.
    CLEAR: l_docnr1, l_docnr2, l_docnr3, l_err, l_msg, l_msg2.
    CASE it_list_tar-dtype.
      WHEN c_text_s.         " Supplement
        PERFORM execute_transaction_fr70
                                 USING it_list_tar-belnr
                                       l_err
                                       l_docnr1
                                       l_msg.
        IF l_err <> c_true.
          PERFORM excute_transaction_fr51
                                  USING it_list_tar-belnr
                                        it_list_tar-docnr
                                        l_err
                                        l_docnr2
                                        l_msg2
                                        'FR51'.
        ENDIF.
        IF l_msg2 IS INITIAL.
          MOVE l_msg       TO it_list_tar-zmsg.
        ELSE.
          CONCATENATE l_msg ',' l_msg2 INTO it_list_tar-zmsg.
        ENDIF.
        MODIFY it_list_tar.

      WHEN c_text_r.         " Return
        clear : l_err.
        PERFORM execute_transaction_fr70
                                 USING it_list_tar-belnr
                                       l_err
                                       l_docnr2
                                       l_msg.
        IF l_err = c_true.
          PERFORM excute_transaction_fr51
                                  USING it_list_tar-belnr
                                        it_list_tar-docnr
                                        l_err
                                        l_docnr2
                                        l_msg2
                                        'FR53'.
        ENDIF.
        IF l_msg2 IS INITIAL.
          MOVE l_msg       TO it_list_tar-zmsg.
        ELSE.
          CONCATENATE l_msg ',' l_msg2 INTO it_list_tar-zmsg.
        ENDIF.
        MODIFY it_list_tar.

      WHEN c_text_t.         " Trnasfer
        CLEAR: lt_doc, lt_doc[].
        PERFORM find_reject_doc TABLES lt_doc.
        SORT lt_doc BY belnr.
        LOOP AT lt_doc.
          CASE sy-tabix.
            WHEN 1.
              PERFORM execute_transaction_fr70
                                       USING lt_doc-belnr
                                             l_err
                                             l_docnr1
                                             l_msg.
              IF l_err <> c_true.
                CONCATENATE 'Document posted:' l_docnr1
                                               INTO l_msg2.
              ELSE.
                l_msg2 = l_msg.
                EXIT.
              ENDIF.

            WHEN 2.
              PERFORM execute_transaction_fr70
                                       USING lt_doc-belnr
                                             l_err
                                             l_docnr2
                                             l_msg.
              IF l_err <> c_true.
                CONCATENATE l_msg2 ',' l_docnr2  INTO l_msg2.
              ELSE.
                CONCATENATE l_msg2 ',' l_msg INTO l_msg2.
                EXIT.
              ENDIF.

            WHEN 3.
              PERFORM execute_transaction_fr70
                                       USING lt_doc-belnr
                                             l_err
                                             l_docnr3
                                             l_msg.
              IF l_err <> c_true.
                CONCATENATE l_msg2 ',' l_docnr3  INTO l_msg2.
              ELSE.
                CONCATENATE l_msg2 ',' l_msg INTO l_msg2.
                EXIT.
              ENDIF.

          ENDCASE.
        ENDLOOP.
        it_list_tar-zmsg = l_msg2.
        MODIFY it_list_tar.
    ENDCASE.

  ENDLOOP.

  PERFORM update_it_list.
  PERFORM count_status.
  PERFORM assign_itab_to_alv USING sy-dynnr.

  MESSAGE s000(zz) WITH text-m12.

ENDFORM.                    " execute_rtn
*&---------------------------------------------------------------------*
*&      Form  reject_rtn
*&---------------------------------------------------------------------*
FORM reject_rtn .

  PERFORM get_target_data USING sy-dynnr.

  DATA: text1(40),
        text2(40),
        titl(40).

  text1 = text-m06.
*  text2 = text-m07.
  titl  = text-m08.
  PERFORM popup_confirm   USING text1 text2 titl
                                'X'   'A'   v_ans.
  CHECK v_ans = c_yes.

  PERFORM excute_fr71.
  PERFORM update_it_list.
  PERFORM count_status.
  PERFORM assign_itab_to_alv USING sy-dynnr.

  MESSAGE s000(zz) WITH text-m12.
ENDFORM.                    " reject_rtn

*&---------------------------------------------------------------------*
*&      Form  double_click
*&---------------------------------------------------------------------*
*       double_click
*----------------------------------------------------------------------*
FORM double_click  USING  p_row  p_column  p_s_row_no.

  CASE p_column.
    WHEN 'BELNR'.
      READ TABLE it_list INDEX p_row.
      SET PARAMETER ID 'BPB' FIELD it_list-belnr.
      CALL TRANSACTION 'FR72' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.                    " double_click

*&---------------------------------------------------------------------*
*&      Form  get_target_data
*&---------------------------------------------------------------------*
FORM get_target_data  USING    pv_dynnr.

  "/Indexes of Selected Rows
  DATA: lt_rows   TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CONCATENATE: 'WC_ALV_' pv_dynnr INTO v_alv.
  ASSIGN: (v_alv) TO <alv>.

  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m10.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m11.
  ENDIF.

  PERFORM set_target_data TABLES lt_rows.

ENDFORM.                    " get_target_data

*&---------------------------------------------------------------------*
*&      Form  set_target_data
*&---------------------------------------------------------------------*
FORM set_target_data TABLES pt_rows STRUCTURE lvc_s_row.

  CLEAR: v_selected.
  CLEAR: it_list_tar, it_list_tar[].

  LOOP AT pt_rows WHERE index NE 0.
    CLEAR it_list.
    READ TABLE it_list INDEX pt_rows-index.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m05.
    ENDIF.

    v_selected = v_selected + 1.

    CHECK it_list-icon EQ icon_led_yellow OR
          it_list-icon EQ icon_led_red.

    MOVE it_list TO it_list_tar.
    APPEND it_list_tar.  CLEAR it_list_tar.

  ENDLOOP.

ENDFORM.                    " set_target_data
*&---------------------------------------------------------------------*
*&      Form  excute_fr71
*&---------------------------------------------------------------------*
FORM excute_fr71 .

  DATA: lt_doc LIKE st_doc OCCURS 0 WITH HEADER LINE,
        l_msg(60),
        l_msg1(60),
        l_err,
        l_par.

  LOOP AT it_list_tar.
    CLEAR: l_par, l_err, l_msg, l_msg1.
    CASE it_list_tar-dtype.
      WHEN c_text_s OR c_text_r.
        l_par = c_true.
        PERFORM excute_transaction_fr71 USING it_list_tar-belnr
                                              l_msg
                                              l_err
                                              l_par.
        IF l_err = c_true.
          it_list_tar-zmsg = l_msg.
        ELSE.
          CONCATENATE 'Reversed :' it_list_tar-belnr
                                   INTO it_list_tar-zmsg.
        ENDIF.
        MODIFY it_list_tar.
      WHEN c_text_t.
        CLEAR: lt_doc, lt_doc[].
        PERFORM find_reject_doc TABLES lt_doc.
        SORT lt_doc BY belnr DESCENDING.
        LOOP AT lt_doc.
          CLEAR: l_par, l_msg.
          IF it_list_tar-belnr = lt_doc-belnr.
            l_par = c_true.
          ENDIF.
          PERFORM excute_transaction_fr71 USING lt_doc-belnr
                                                l_msg
                                                l_err
                                                l_par.
          IF l_err = c_true.
            IF l_msg1 IS INITIAL.
              l_msg1 = l_msg.
              EXIT.
            ELSE.
              CONCATENATE l_msg1 ',' l_msg INTO l_msg1.
              EXIT.
            ENDIF.
          ELSE.
            IF l_msg1 IS INITIAL.
              CONCATENATE 'Reversed :' lt_doc-belnr INTO l_msg1.
            ELSE.
              CONCATENATE l_msg1 ',' lt_doc-belnr  INTO l_msg1.
            ENDIF.
          ENDIF.
        ENDLOOP.
        MOVE l_msg1     TO it_list_tar-zmsg.
        MODIFY it_list_tar.
    ENDCASE.
    CLEAR it_list_tar.
  ENDLOOP.

ENDFORM.                    " excute_fr71
*&---------------------------------------------------------------------*
*&      Form  excute_transaction_fr71
*&---------------------------------------------------------------------*
FORM excute_transaction_fr71  USING  p_belnr
                                     p_msg
                                     p_err
                                     p_par.

  CLEAR: it_bdc, it_bdc[].

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'     '0100',
     ' ' 'BPDY-DOCNR'   p_belnr,              "Document number
     ' ' 'BDC_OKCODE'   '/00',

     'X' 'SAPLKBPB'     '0400',
     ' ' 'BDC_OKCODE'   '=DCAN'.

  CALL TRANSACTION 'FR71'  USING it_bdc
                           OPTIONS FROM wa_opt.

  IF p_par = c_true.
    IF sy-subrc NE 0.
      MOVE: icon_led_red TO it_list_tar-icon.
      PERFORM get_err_msg USING p_msg.
      p_err = c_true.
*      MODIFY it_list_tar.
    ELSE.
      MOVE: icon_led_green TO it_list_tar-icon.
*      PERFORM get_err_msg USING p_msg.
*      MODIFY it_list_tar.
    ENDIF.
  ENDIF.

ENDFORM.                    " excute_transaction_fr71

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
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
*&      Form  find_reject_doc
*&---------------------------------------------------------------------*
FORM find_reject_doc  TABLES   pt_doc STRUCTURE st_doc.

  MOVE it_list_tar-belnr     TO pt_doc-belnr.
  APPEND pt_doc.  CLEAR pt_doc.

  MOVE it_list_tar-rpublaw   TO pt_doc-belnr.
  APPEND pt_doc.  CLEAR pt_doc.

  SELECT belnr INTO pt_doc-belnr
         FROM bpdk
         WHERE rpublaw =  it_list_tar-rpublaw
           AND belnr   <> it_list_tar-belnr.
    EXIT.
  ENDSELECT.
  IF sy-subrc = 0.
    APPEND pt_doc.  CLEAR pt_doc.
  ENDIF.

ENDFORM.                    " find_reject_doc
*&---------------------------------------------------------------------*
*&      Form  update_it_list
*&---------------------------------------------------------------------*
FORM update_it_list .

  LOOP AT it_list_tar.
    READ TABLE it_list WITH KEY belnr = it_list_tar-belnr.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m05.
    ENDIF.

    MOVE: it_list_tar-icon  TO it_list-icon,
          it_list_tar-zmsg  TO it_list-zmsg.

    MODIFY it_list INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " update_it_list
*&---------------------------------------------------------------------*
*&      Form  count_status
*&---------------------------------------------------------------------*
FORM count_status .

  CLEAR: v_total, v_ready, v_error, v_success.

  LOOP AT it_list.
    CASE it_list-icon.
      WHEN icon_led_yellow.
        v_ready   = v_ready   + 1.
      WHEN icon_led_red.
        v_error   = v_error   + 1.
      WHEN icon_led_green.
        v_success = v_success + 1.
    ENDCASE.

    v_total = v_total + 1.
  ENDLOOP.

ENDFORM.                    " count_status
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
*&---------------------------------------------------------------------*
*&      Form  execute_transaction_fr70
*&---------------------------------------------------------------------*
FORM execute_transaction_fr70  USING    p_belnr
                                        p_err
                                        p_docnr
                                        p_msg.

  CLEAR: it_bdc, it_bdc[].

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'     '0100',
     ' ' 'BPDY-DOCNR'   p_belnr,              "Document number
     ' ' 'BDC_OKCODE'   '/00',

     'X' 'SAPLKBPB'     '0400',
     ' ' 'BDC_OKCODE'   '=PPOS'.

  CALL TRANSACTION 'FR70'  USING it_bdc
                           OPTIONS FROM wa_opt.

  IF sy-subrc NE 0.
    MOVE: icon_led_red TO it_list_tar-icon.
    PERFORM get_err_msg USING p_msg.
    CONCATENATE 'Step1:' p_msg INTO p_msg.
    p_err = c_true.
  ELSE.
    MOVE: icon_led_green TO it_list_tar-icon,
*          sy-msgv1       TO p_docnr.
          p_belnr        TO p_docnr.
    CONCATENATE 'Document posted:' p_docnr
                         INTO p_msg.
  ENDIF.

ENDFORM.                    " execute_transaction_fr70
*&---------------------------------------------------------------------*
*&      Form  excute_transaction_fr51
*&---------------------------------------------------------------------*
FORM excute_transaction_fr51  USING    p_belnr
                                       p_docnr
                                       p_err
                                       p_docnr2
                                       p_msg
                                       p_tcode.

  DATA: BEGIN OF lt_tab OCCURS 0,
          buzei  LIKE bpep-buzei,
          geber  LIKE bpep-geber,
          versn  LIKE bpep-versn,
          gjahr  LIKE bpep-gjahr,
          fictr  LIKE fmfctr-fictr,
          fipex  LIKE fmci-fipex,
          wlp01  LIKE bpep-wlp01,
          wlp02  LIKE bpep-wlp02,
          wlp03  LIKE bpep-wlp03,
          wlp04  LIKE bpep-wlp04,
          wlp05  LIKE bpep-wlp05,
          wlp06  LIKE bpep-wlp06,
          wlp07  LIKE bpep-wlp07,
          wlp08  LIKE bpep-wlp08,
          wlp09  LIKE bpep-wlp09,
          wlp10  LIKE bpep-wlp10,
          wlp11  LIKE bpep-wlp11,
          wlp12  LIKE bpep-wlp12,
        END   OF lt_tab.

  DATA: lv_peri1_01(21), lv_peri1_02(21), lv_peri1_03(21),
        lv_peri1_04(21), lv_peri1_05(21), lv_peri1_06(21),
        lv_peri1_07(21), lv_peri1_08(21), lv_peri1_09(21),
        lv_peri1_10(21), lv_peri1_11(21), lv_peri1_12(21).

  CLEAR: it_bdc, it_bdc[].

  SELECT a~buzei  a~geber  a~versn  a~gjahr
         b~fictr  c~fipex
         a~wlp01  a~wlp02  a~wlp03  a~wlp04
         a~wlp05  a~wlp06  a~wlp07  a~wlp08
         a~wlp09  a~wlp10  a~wlp11  a~wlp12
    INTO CORRESPONDING FIELDS OF TABLE lt_tab
    FROM bpep AS a INNER JOIN fmfctr AS b
                      ON a~objnr  =  b~ctr_objnr
                     AND b~fikrs  =  c_fikrs
                   INNER JOIN fmci AS c
                      ON a~posit  =  c~posit
                     AND c~fikrs  =  c_fikrs
                     AND c~gjahr  =  '0000'
    WHERE a~belnr  =  p_docnr
      AND c~kateg  =  '2'.
  IF sy-subrc <> 0.
    p_err = c_true.
    EXIT.
  ENDIF.

  CLEAR lt_tab.
  READ TABLE lt_tab INDEX 1.
  PERFORM dynpro USING:
     'X' 'SAPLKBPB'     '0200',
     ' ' 'FMDY-FIKRS'   c_fikrs,              "FM Area
     ' ' 'BPDY-GJAHR'   lt_tab-gjahr,         "Fiscal year
     ' ' 'BPDY-VERSN'   lt_tab-versn,         "Version
     ' ' 'FMDY-FINCODE' lt_tab-geber,         "Fund
     ' ' 'BDC_OKCODE'   '/00'.

  LOOP AT lt_tab.
    WRITE: lt_tab-wlp01 CURRENCY c_eur
                             TO lv_peri1_01(21),
           lt_tab-wlp02 CURRENCY c_eur
                             TO lv_peri1_02(21),
           lt_tab-wlp03 CURRENCY c_eur
                             TO lv_peri1_03(21),
           lt_tab-wlp04 CURRENCY c_eur
                             TO lv_peri1_04(21),
           lt_tab-wlp05 CURRENCY c_eur
                             TO lv_peri1_05(21),
           lt_tab-wlp06 CURRENCY c_eur
                             TO lv_peri1_06(21),
           lt_tab-wlp07 CURRENCY c_eur
                             TO lv_peri1_07(21),
           lt_tab-wlp08 CURRENCY c_eur
                             TO lv_peri1_08(21),
           lt_tab-wlp09 CURRENCY c_eur
                             TO lv_peri1_09(21),
           lt_tab-wlp10 CURRENCY c_eur
                             TO lv_peri1_10(21),
           lt_tab-wlp11 CURRENCY c_eur
                             TO lv_peri1_11(21),
           lt_tab-wlp12 CURRENCY c_eur
                             TO lv_peri1_12(21).

    PERFORM dynpro USING:
       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=INSL',              "Insert

       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'FMDY-FICTR(01)'   lt_tab-fictr,         "Fund Center
       ' ' 'FMDY-FIPEX(01)'   lt_tab-fipex,         "Commitment Item
       ' ' 'BPDY-SPRED1(01)'  '0',                  "DK
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=PERI',

       'X' 'SAPLKBPP'         '0600',               "Period Screen
       ' ' 'BPDY-PERI1(01)'   lv_peri1_01,          " 01
       ' ' 'BPDY-PERI1(02)'   lv_peri1_02,          " 02
       ' ' 'BPDY-PERI1(03)'   lv_peri1_03,          " 03
       ' ' 'BPDY-PERI1(04)'   lv_peri1_04,          " 04
       ' ' 'BPDY-PERI1(05)'   lv_peri1_05,          " 05
       ' ' 'BPDY-PERI1(06)'   lv_peri1_06,          " 06
       ' ' 'BPDY-PERI1(07)'   lv_peri1_07,          " 07
       ' ' 'BPDY-PERI1(08)'   lv_peri1_08,          " 08
       ' ' 'BPDY-PERI1(09)'   lv_peri1_09,          " 09
       ' ' 'BPDY-PERI1(10)'   lv_peri1_10,          " 10
       ' ' 'BPDY-PERI1(11)'   lv_peri1_11,          " 11
       ' ' 'BPDY-PERI1(12)'   lv_peri1_12,          " 12
       ' ' 'G_SCREEN_0600-DK' '0',                  "DK
       ' ' 'BDC_OKCODE'  '=CLOS'.

    AT LAST.
      PERFORM dynpro USING:
         'X' 'SAPLKBPB'         '0400',                 "Detail Screen
         ' ' 'BDC_OKCODE'       '=POST'.                "Save
    ENDAT.
  ENDLOOP.

  PERFORM call_program USING p_tcode
                             p_err
                             p_msg.

ENDFORM.                    " excute_transaction_fr51
*&---------------------------------------------------------------------*
*&      Form  call_program
*&---------------------------------------------------------------------*
FORM call_program  USING    pv__tcode
                            pv__err
                            pv__msg.

  CALL TRANSACTION pv__tcode USING it_bdc
                             OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '043'.
    MOVE: icon_led_red TO it_list_tar-icon.
    PERFORM get_err_msg USING pv__msg.
    CONCATENATE 'Step2:' pv__msg INTO pv__msg.
    pv__err = c_true.
  ELSE.
    MOVE: icon_led_green TO it_list_tar-icon,
          sy-msgv1       TO pv__msg.

  ENDIF.

ENDFORM.                    " call_program
*&---------------------------------------------------------------------*
*&      Form  select_retn_rt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_retn_rt .

  DATA: lt_list LIKE st_sorc OCCURS 0 WITH HEADER LINE.
refresh : lt_list.
  SELECT a~bldat a~belnr b~belnr AS docnr c~wljhr
    INTO CORRESPONDING FIELDS OF TABLE lt_list
    FROM bpdk AS a INNER JOIN bpbk AS b
                      ON a~belnr  =  b~bpdk_belnr
                   INNER JOIN bpej AS c
                      ON b~belnr  =  c~belnr
                     AND c~buzei  =  '001'
                     AND c~versn  = zfmcm_versn_0
  WHERE a~belnr   IN s_belnr
    AND NOT EXISTS
        ( SELECT * FROM bpdk
                 WHERE rpublaw = a~belnr )
*    AND a~bldat   IN s_datum
    AND a~bldat   IN r_bldat
    AND a~bstat   =  '1'
    AND a~rpublaw = space
    AND b~vorga   = 'KBR0'
    AND c~objnr LIKE 'FSK201%'.

  lt_list-waers = c_eur.
  lt_list-dtype = c_text_r.
  lt_list-icon  = icon_led_yellow.

  MODIFY lt_list TRANSPORTING waers dtype icon WHERE waers = ''.

  APPEND LINES OF lt_list TO it_list.

endform.                    " select_retn_rt
