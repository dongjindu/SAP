REPORT zrmmgm03r_inforecord_bk .
TABLES: mara, zvmm_inforecord.

DATA: zsmm_rmmgm03_9000 LIKE zsmm_rmmgm03_9000.

*----- Internal tables
DATA: it_9000 TYPE STANDARD TABLE OF zsmm_rmmgm03_9000
                                     WITH HEADER LINE.

DATA: BEGIN OF it_knumh OCCURS 0,
        matnr LIKE mara-matnr,
        lifnr LIKE lfa1-lifnr,
        name1 LIKE lfa1-name1,
        knumh LIKE konh-knumh,
        datab LIKE konh-datab,
        datbi LIKE konh-datbi,
      END   OF it_knumh.

DATA: it_zvmm_info_condi LIKE zvmm_info_condi OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_info OCCURS 0,                "Info Condition
        matnr   LIKE   mara-matnr,
        lifnr   LIKE   lfa1-lifnr,
        name1   LIKE   lfa1-name1,
        datab   LIKE   a018-datab,
        datbi   LIKE   a018-datbi,
        kzust   LIKE   konh-kzust,
        konwa   LIKE   konp-konwa,
        kbetr   LIKE   konp-kbetr,
        kpein   LIKE   konp-kpein,
        kmein   LIKE   konp-kmein,
      END   OF it_info.

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,
        maktx   LIKE   makt-maktx,
      END   OF it_matnr.

*---// Working area
DATA: w_datum_f   LIKE   sy-datum,
      w_datum_t   LIKE   sy-datum.

CONSTANTS: c_check                  VALUE 'X',
           c_ekorg LIKE ekko-ekorg  VALUE 'PU01',"Purchase Org.
           c_kschl LIKE konp-kschl  VALUE 'PB00',"Type of amount
           c_frght LIKE konp-kschl  VALUE 'FRA1',"Type of freight
           c_duty  LIKE konp-kschl  VALUE 'ZOA1',"Type of duty
           c_con01 LIKE konp-kschl  VALUE 'ZOTH',"Type of ETC rate
           c_con02 LIKE konp-kschl  VALUE 'ZOTI'."Type of ETC rate

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

DATA: w_container(50),
      w_control(50),
      w_alv(50),
      w_itab(50),
      w_structure LIKE dd02l-tabname.

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
DATA : w_is_layout TYPE lvc_s_layo,
       w_variant   TYPE disvariant,          "for parameter IS_VARIANT
       w_fieldname LIKE LINE OF it_fieldcat,
       w_repid     LIKE sy-repid,
       w_cnt       TYPE i,                   "Field count
       w_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE

CONSTANTS: c_structure(100) VALUE 'ZSMM_RMMGM03_'.

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
*  PUBLIC SECTION.
*    METHODS:
*
*    handle_double_click
*        FOR EVENT double_click OF cl_gui_alv_grid
*            IMPORTING e_row
*                      e_column
*                      es_row_no.
*
*    handle_user_command
*        FOR EVENT user_command OF cl_gui_alv_grid
*            IMPORTING e_ucomm,
*
*    handle_data_changed
*        FOR EVENT data_changed OF cl_gui_alv_grid
*            IMPORTING er_data_changed
*                      e_onf4
*                      e_onf4_before
*                      e_onf4_after.
ENDCLASS.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS : s_matnr FOR zvmm_inforecord-matnr,
                 s_lifnr FOR zvmm_inforecord-lifnr,
                 s_ekgrp FOR zvmm_inforecord-ekgrp,
                 s_date  FOR sy-datum NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK bl1.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_rtn.
  PERFORM get_data.

START-OF-SELECTION.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  CLEAR: it_knumh, it_knumh[].
  SELECT matnr lifnr knumh datab datbi
    INTO CORRESPONDING FIELDS OF TABLE it_knumh
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr IN s_matnr
     AND lifnr IN s_lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datbi >= w_datum_f
     AND datab <= w_datum_t.

  READ TABLE it_knumh INDEX 1.
  IF sy-subrc NE 0.
    MOVE: 'NIGIMIJOTO' TO it_knumh-knumh.
    APPEND it_knumh.
  ENDIF.

  LOOP AT it_knumh.
    SELECT SINGLE name1
      INTO it_knumh-name1
      FROM eina AS a INNER JOIN eine AS b
                        ON a~infnr =  b~infnr
                     INNER JOIN lfa1 AS c
                        ON a~lifnr = c~lifnr
     WHERE a~matnr =  it_knumh-matnr
       AND a~lifnr =  it_knumh-lifnr
       AND a~loekz =  ' '
       AND b~werks =  ' '
       AND b~ekorg =  c_ekorg
       AND b~ekgrp IN s_ekgrp
       AND b~loekz =  ' '.
    IF sy-subrc NE 0.
      DELETE it_knumh. CONTINUE.
    ENDIF.

    MODIFY it_knumh.
  ENDLOOP.

  READ TABLE it_knumh INDEX 1.
  IF sy-subrc NE 0.
    MOVE: 'NIGIMIJOTO' TO it_knumh-knumh.
    APPEND it_knumh.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_zvmm_info_condi
    FROM zvmm_info_condi
     FOR ALL ENTRIES IN it_knumh
   WHERE knumh    = it_knumh-knumh
     AND kschl    = c_kschl
     AND loevm_ko = ' '.

  LOOP AT it_zvmm_info_condi.
    READ TABLE it_knumh WITH KEY knumh = it_zvmm_info_condi-knumh.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    MOVE: it_knumh-matnr TO it_info-matnr,
          it_knumh-lifnr TO it_info-lifnr,
          it_knumh-name1 TO it_info-name1,
          it_knumh-datab TO it_info-datab,
          it_knumh-datbi TO it_info-datbi,
          it_zvmm_info_condi-kbetr TO it_info-kbetr,
          it_zvmm_info_condi-kzust TO it_info-kzust,
          it_zvmm_info_condi-kpein TO it_info-kpein,
          it_zvmm_info_condi-kmein TO it_info-kmein,
          it_zvmm_info_condi-konwa TO it_info-konwa.

    APPEND it_info.
  ENDLOOP.

  SORT it_info.

  READ TABLE s_ekgrp INDEX 1.
  IF sy-subrc EQ 0.
    LOOP AT it_info.
      READ TABLE it_matnr WITH KEY matnr = it_info-matnr
                          BINARY SEARCH.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.
      MOVE-CORRESPONDING: it_info  TO it_9000,
                          it_matnr TO it_9000.
      APPEND it_9000.
    ENDLOOP.
  ELSE.
    LOOP AT it_matnr.
      LOOP AT it_info WHERE matnr = it_matnr-matnr.
        MOVE-CORRESPONDING: it_info  TO it_9000,
                            it_matnr TO it_9000.
        APPEND it_9000.
      ENDLOOP.
      IF sy-subrc NE 0.
        MOVE-CORRESPONDING: it_matnr TO it_9000.
        APPEND it_9000.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM check_matnr.
  PERFORM check_datum.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr.
  SELECT a~matnr b~maktx
    INTO CORRESPONDING FIELDS OF TABLE it_matnr
    FROM mara AS a INNER JOIN makt AS b
                      ON a~matnr = b~matnr
                     AND b~spras = sy-langu
   WHERE a~matnr IN s_matnr
     AND a~mtart IN ('ROH','ROH1')
     AND a~lvorm EQ space.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  check_datum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_datum.
  IF     s_date-low EQ '00000000' AND s_date-high EQ '00000000'.
    MOVE: '99991231' TO w_datum_t.
  ELSEIF s_date-low NE '00000000' AND s_date-high EQ '00000000'.
    MOVE: s_date-low TO w_datum_f,
          s_date-low TO w_datum_t.
  ELSEIF s_date-low EQ '00000000' AND s_date-high NE '00000000'.
    MOVE: s_date-high TO w_datum_t.
  ELSEIF s_date-low NE '00000000' AND s_date-high NE '00000000'.
    MOVE: s_date-low  TO w_datum_f,
          s_date-high TO w_datum_t.
  ENDIF.
ENDFORM.                    " check_datum
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  SORT it_9000 BY matnr lifnr datab .
  CALL SCREEN 9000.
ENDFORM.                    " display_data
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
  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO w_container.
  ASSIGN:      (w_container)           TO   <container>.

  IF <container> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM create_container_n_object USING p_dynnr.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
    PERFORM sssign_event.
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

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO w_container,
               'WC_CONTROL_'   p_dynnr INTO w_control,
               'WC_ALV_'       p_dynnr INTO w_alv.

  ASSIGN: (w_container) TO <container>,
          (w_control)   TO <control>,
          (w_alv)       TO <alv>.

  CREATE OBJECT <container>
         EXPORTING container_name = <control>
         EXCEPTIONS
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
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
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
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

  MOVE: sy-repid TO w_repid.
  CONCATENATE c_structure p_dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
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
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'LIFNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'NAME1'       ' ',
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
  DATA: lw_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO w_alv,
               c_structure  p_dynnr      INTO w_structure,
               'IT_'        p_dynnr '[]' INTO w_itab.

  ASSIGN: (w_alv)       TO <alv>,
          (w_itab)      TO <itab>.

  CALL METHOD <alv>->set_table_for_first_display
     EXPORTING i_structure_name = w_structure
               is_layout        = w_is_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     CHANGING  it_fieldcatalog  = it_fieldcat[]
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
FORM sssign_event.

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
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check filed catalog'.
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
    IF p_fieldcat-col_pos IS INITIAL.
      ADD 1 TO w_cnt.
      p_fieldcat-col_pos = w_cnt.
    ENDIF.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
