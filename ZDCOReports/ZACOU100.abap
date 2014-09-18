*----------------------------------------------------------------------
* Program ID        : ZACOU100
* Title             : [CO] Select FSC/MIP/Module
* Created on        : 07/31/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Maintain FSC/MIP/Module.
*                     Create, Update and delete
*                     Unit cost Information Table ZTCOU100.
*                     Change Material master.
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  05/10/2012  Valerian  UD1K954655  Include logic to check deleted
*                                    material in plant level
*  06/17/2013  T00303    UD1K957358  U1: Apply Archiving
*----------------------------------------------------------------------
REPORT zacou100 NO STANDARD PAGE HEADING MESSAGE-ID zmco.

INCLUDE zacoui00.

*----------------------------------------------------------------------*
*  Define Variant & tables & local class
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Define Variant & tables
*----------------------------------------------------------------------*
TABLES: keko,                " Product Costing - Header Data
        mara,
        tka01.

TYPES: BEGIN OF ty_ztcou100.
        INCLUDE STRUCTURE ztcou100.
TYPES:  maktx TYPE maktx,
       END OF ty_ztcou100.

* Internal Table for Display
TYPES: BEGIN OF ty_out.
        INCLUDE TYPE ty_ztcou100.
TYPES:  prv,
        chk,
        celltab TYPE lvc_t_styl,
       END OF ty_out.

* Internal Table for Possible Entry of Costing type
TYPES: BEGIN OF ty_kalka,
         kalka TYPE ck_kalka,
         txkla TYPE ck_txkla,
       END OF ty_kalka.

* UD1K940522 by IG.MOON
* Internal Table for Possible Entry of Version
TYPES: BEGIN OF ty_verid,
         verid TYPE verid,
         text1 TYPE vers_text,
         bstmi TYPE sa_losvn,
         bstma TYPE sa_losbs,
         adatu TYPE adatm,
         bdatu TYPE bdatm,
       END OF ty_verid.
* end

TYPES ddshretval_table TYPE TABLE OF ddshretval.

DATA: gv_cnt      TYPE i,              " Total count
      gv_ldate    TYPE sydatum,        " last date of month
      gv_pyear    TYPE bdatj,          " Pervious year
      gv_ppoper   TYPE poper,          " Pervious period
      gt_ztcou100 TYPE TABLE OF ty_ztcou100 WITH HEADER LINE,
      gt_out      TYPE TABLE OF ty_out      WITH HEADER LINE,
      gt_kalka    TYPE TABLE OF ty_kalka    WITH HEADER LINE,
* UD1K940522 by IG.MOON
      gt_verid    TYPE TABLE OF ty_verid    WITH HEADER LINE,
* end
      gv_mm02 TYPE i.

*- U1 Start
DATA: gt_ckmlmv013_a TYPE TABLE OF ckmlmv013 WITH HEADER LINE,
      gt_ckmlmv013_a_tmp TYPE TABLE OF ckmlmv013 WITH HEADER LINE.

DATA : BEGIN OF gt_ckmlmv003_a OCCURS 0,
         bwkey      LIKE ckmlmv001-bwkey,
         matnr      LIKE ckmlmv001-matnr,
         aufnr      LIKE ckmlmv013-aufnr,
         verid_nd   LIKE ckmlmv001-verid_nd,
         meinh      LIKE ckmlmv003-meinh,
         out_menge  LIKE ckmlmv003-out_menge,
       END OF  gt_ckmlmv003_a.

DATA: gt_ztcou103_a TYPE TABLE OF ztcou103 WITH HEADER LINE,
      gt_ztcou103_a_tmp TYPE TABLE OF ztcou103 WITH HEADER LINE.

DATA: BEGIN OF gt_103_a OCCURS 0,
      matnr   LIKE ztcou103-compn,
      werks   LIKE ztcou103-werks,
      maktx   LIKE makt-maktx,
      END OF gt_103_a.
*- U1 End

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS: data_changed
                 FOR EVENT data_changed OF cl_gui_alv_grid
                 IMPORTING er_data_changed,

             data_changed_finished
                 FOR EVENT data_changed_finished OF cl_gui_alv_grid
                 IMPORTING e_modified,

             double_click
                  FOR EVENT double_click OF cl_gui_alv_grid
                  IMPORTING e_row
                            e_column
                            es_row_no,

             on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
                   IMPORTING sender
                             e_fieldname
                             e_fieldvalue
                             es_row_no
                             er_event_data
                             et_bad_cells
                             e_display,

             my_f4 IMPORTING sender        TYPE REF TO cl_gui_alv_grid
                             et_bad_cells  TYPE lvc_t_modi
                             es_row_no     TYPE lvc_s_roid
                             er_event_data TYPE REF TO cl_alv_event_data
                             e_display     TYPE c
                             e_fieldname   TYPE lvc_fname
                   EXPORTING lt_f4         TYPE ddshretval_table.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
* Change data
  METHOD data_changed.
    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

  METHOD data_changed_finished.
    PERFORM data_changed_finished USING e_modified.
  ENDMETHOD.                    " handle_data_changed

* Setting for Double Click
  METHOD double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_data_changed


* Get values of possible entries
  METHOD on_f4.
    PERFORM on_f4 USING sender
                        e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  ENDMETHOD.                                                "on_f4

  METHOD my_f4.
    PERFORM my_f4 TABLES lt_f4
                  USING  sender
                         et_bad_cells
                         es_row_no
                         er_event_data
                         e_display
                         e_fieldname.
  ENDMETHOD.                    "my_f4

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA  g_event_receiver  TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME.
PARAMETERS: p_kokrs LIKE keko-kokrs OBLIGATORY
                                    MEMORY ID cac
                                    MATCHCODE OBJECT fc_kokrs,
            p_kalka LIKE keko-kalka OBLIGATORY MEMORY ID kka,
            p_year  LIKE keko-bdatj OBLIGATORY MEMORY ID bdtj,
            p_poper LIKE keko-poper OBLIGATORY MEMORY ID popr.
SELECTION-SCREEN END OF BLOCK b0.
SELECT-OPTIONS s_cnfg FOR gt_ztcou100-cnfg.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_ml AS CHECKBOX.
SELECT-OPTIONS: s_matnr FOR keko-matnr,
                s_mtart FOR mara-mtart DEFAULT 'FERT',
                s_werks FOR keko-werks DEFAULT 'P001'.
SELECTION-SCREEN END OF BLOCK b2.

*NCLUDE zacou100_f01.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_kalka.
  PERFORM popup_kalka USING p_kalka 'P_KALKA'.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_default.

  IF p_ml = 'X'.
    IF p_kalka = 'M1'.  "module costing
      PERFORM fill_from_103.
    ELSE.
      PERFORM fill_from_ml.
    ENDIF.
  ELSE.
    PERFORM get_data.
  ENDIF.

  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
MODULE create_alv_control OUTPUT.
  IF g_custom_container IS INITIAL.
*   Create internal table GT_OUT
    PERFORM get_gt_out.

*   Create object
    PERFORM create_object.

*   Exclude toolbar
    PERFORM exclude_functions USING 'GT_EXCLUDE'.

*   Create field category
    PERFORM create_field_category.

*   Setting for layout
    gs_layo-edit       = 'X'.
    gs_layo-zebra      = 'X'.
    gs_layo-sel_mode   = 'A'.       " Column and row selection
    gs_layo-stylefname = 'CELLTAB'.

*   Define possible entry fields
    PERFORM create_f4_fields.

*   Define editable field
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*   Setting for event
    PERFORM set_event.

*   Define cell attribute
    PERFORM build_cell_attr.

*   Define variant
    gs_variant-report = sy-repid.

*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo
        it_toolbar_excluding = gt_exclude
        i_save               = gc_var_save
        is_variant           = gs_variant
      CHANGING
        it_outtab            = gt_out[]
        it_fieldcatalog      = gt_fcat[].

  ENDIF.

ENDMODULE.                 " CREATE_ALV_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      LEAVE PROGRAM.

*   Copy data of previous period to current period
    WHEN 'COPY'.
      PERFORM copy_prv_data.

*   Save data to table ZTCOU100
    WHEN 'SAVE'.
      PERFORM save_data.

*   Excute Update material master
    WHEN 'MM02' OR 'PVER'.
      PERFORM bdc_mat.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_verid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_FIELDNAME  text
*----------------------------------------------------------------------*
FORM get_verid USING    p_row_id
                        p_e_fieldname.
* UD1K940522
  DATA : $matnr LIKE ztcou100-matnr,
         $werks LIKE ztcou100-werks.

  READ TABLE gt_out INDEX p_row_id.
  IF sy-subrc EQ 0.
    $matnr = gt_out-matnr.
    $werks = gt_out-werks.
  ELSE.
    EXIT.
  ENDIF.

  CLEAR gt_verid.
  REFRESH gt_verid.

  SELECT verid
         text1
         bstmi
         bstma
         adatu
         bdatu
    INTO TABLE gt_verid
    FROM mkal
   WHERE matnr EQ $matnr
     AND werks EQ $werks.
  LOOP AT gt_verid.
    gt_values-string = gt_verid-verid.
    APPEND gt_values.

    gt_values-string = gt_verid-text1.
    APPEND gt_values.

    gt_values-string = gt_verid-bstmi.
    APPEND gt_values.

    gt_values-string = gt_verid-bstma.
    APPEND gt_values.

    gt_values-string = gt_verid-adatu.
    APPEND gt_values.

    gt_values-string = gt_verid-bdatu.
    APPEND gt_values.
  ENDLOOP.

  CLEAR gt_fields.REFRESH gt_fields.

  gt_fields-fieldname = p_e_fieldname.
  gt_fields-position  = 1.
  gt_fields-intlen    = 4.
  gt_fields-outputlen = 4.
  gt_fields-reptext   = 'Ver.'.
  APPEND gt_fields.CLEAR gt_fields.

  gt_fields-fieldname = 'TEXT1'.
  gt_fields-position  = 2.
  gt_fields-intlen    = 30.
  gt_fields-outputlen = 30.
  gt_fields-reptext = 'Desc.'.
  APPEND gt_fields.CLEAR gt_fields.

  gt_fields-fieldname = 'BSTMI'.
  gt_fields-position  = 3.
  gt_fields-intlen    = 13.
  gt_fields-outputlen = 13.
  gt_fields-reptext = 'Lower'.
  APPEND gt_fields.CLEAR gt_fields.

  gt_fields-fieldname = 'BSTMA'.
  gt_fields-position  = 4.
  gt_fields-intlen    = 13.
  gt_fields-outputlen = 13.
  gt_fields-reptext = 'Upper'.
  APPEND gt_fields.CLEAR gt_fields.

  gt_fields-fieldname = 'ADATU'.
  gt_fields-position  = 5.
  gt_fields-intlen    = 8.
  gt_fields-outputlen = 8.
  gt_fields-reptext = 'Valid-from'.
  APPEND gt_fields.CLEAR gt_fields.

  gt_fields-fieldname = 'BDATM'.
  gt_fields-position  = 6.
  gt_fields-intlen    = 8.
  gt_fields-outputlen = 8.
  gt_fields-reptext = 'Valid-to'.

  APPEND gt_fields.CLEAR gt_fields.
* END

ENDFORM.                    " get_verid
*&---------------------------------------------------------------------*
*&      Form  get_default
*&---------------------------------------------------------------------*
FORM get_default.

* Check Controlling Area
  SELECT SINGLE lmona INTO tka01-lmona FROM tka01
    WHERE kokrs = p_kokrs.

  IF sy-subrc <> 0.
    MESSAGE e038 WITH p_kokrs.
  ENDIF.

* Set Validity Date (Start)
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = p_year
      i_periv        = tka01-lmona
      i_poper        = p_poper
    IMPORTING
      e_date         = gv_ldate
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.

ENDFORM.                    " get_default
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Find data of entered period
*----------------------------------------------------------------------*
FORM get_data.
* Get data from table ZTCOU100
  REFRESH gt_ztcou100.
  CLEAR gt_ztcou100.

  SELECT kokrs kalka bdatj poper
         a~matnr a~verid a~werks a~stlal a~stlan
         cnfg crext crint rstat lstat aedat aenam maktx
    INTO CORRESPONDING FIELDS OF TABLE gt_ztcou100
    FROM ztcou100 AS a
    JOIN makt AS b
      ON b~matnr = a~matnr
     AND b~spras = sy-langu
    JOIN marc AS c
      ON c~matnr = a~matnr
     AND c~werks = a~werks
   WHERE kokrs = p_kokrs
     AND kalka = p_kalka
     AND bdatj = p_year
     AND poper = p_poper
     AND cnfg  IN s_cnfg.


  DESCRIBE TABLE gt_ztcou100 LINES gv_cnt.

  IF gv_cnt = 0.
    MESSAGE s000 WITH 'Data not found.'.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
*       Create internal tale GT_OUT
*----------------------------------------------------------------------*
FORM get_gt_out.
  CLEAR gt_out.
  REFRESH gt_out.

*  IF GT_ZTCOU100[] IS INITIAL.
*    DO 25 TIMES.
*      CLEAR GT_OUT.
*      APPEND GT_OUT.
*    ENDDO.
*
*  ELSE.
  LOOP AT gt_ztcou100.
    MOVE-CORRESPONDING gt_ztcou100 TO gt_out.

    APPEND gt_out.
    CLEAR gt_out.
  ENDLOOP.
*  ENDIF.

ENDFORM.                    " GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       Exclude function code
*----------------------------------------------------------------------*
FORM exclude_functions USING p_tabname.
  PERFORM append_exclude_functions
          TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_refresh.

ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Create ALV control: Field catalog
*----------------------------------------------------------------------*
FORM create_field_category.
  DATA lv_cnt TYPE i.

  CLEAR: gt_fcat, lv_cnt.

  PERFORM fill_field_category USING:
          1  'WERKS'  'Plant'              '5'  'CHAR',
          2  'CNFG'   'Configurable'       '10' 'CHAR',
          3  'MATNR'  'FSC / MIP / Module' '18' 'CHAR',
          4  'MAKTX'  'Description'        '30' 'CHAR',
          5  'VERID'  'Vers'               '5'  'CHAR',
          6  'STLAN'  'BoM'                '5'  'CHAR',
          7  'STLAL'  'Alternative BOM'    '3'  'CHAR',
          8  'CREXT'  'Ext-Color'          '5'  'CHAR',
          9  'CRINT'  'Int-Color'          '5'  'CHAR'.

  LOOP AT gt_fcat INTO gs_fcat.
    lv_cnt = lv_cnt + 1.
    CASE gs_fcat-fieldname.
      WHEN 'KALKA'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-ref_table = 'TCK01'.
        gs_fcat-f4availabl = 'X'.
        MODIFY gt_fcat INDEX lv_cnt FROM gs_fcat
          TRANSPORTING ref_field ref_table f4availabl.

      WHEN 'CNFG'.
        gs_fcat-checkbox = 'X'.
        MODIFY gt_fcat INDEX lv_cnt FROM gs_fcat
                       TRANSPORTING checkbox.

      WHEN 'MATNR'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-ref_table = 'MARA'.
        MODIFY gt_fcat INDEX lv_cnt FROM gs_fcat
               TRANSPORTING ref_field ref_table.

      WHEN 'VERID'.
* UD1K940522 by IG.MOON
*        gs_fcat-ref_field = gs_fcat-fieldname.
*        gs_fcat-ref_table = 'ZTCOU100'.
*        modify gt_fcat index lv_cnt from gs_fcat
*                       transporting ref_field ref_table.
        gs_fcat-ref_field = gs_fcat-fieldname.
*        gs_fcat-ref_table = 'MKAL'.
        gs_fcat-f4availabl = 'X'.
        MODIFY gt_fcat INDEX lv_cnt FROM gs_fcat
          TRANSPORTING ref_field ref_table f4availabl.
* end
      WHEN 'CRINT' OR 'CREXT'.
        gs_fcat-f4availabl = 'X'.
        MODIFY gt_fcat INDEX lv_cnt FROM gs_fcat
                        TRANSPORTING f4availabl.

      WHEN 'STLAN'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-ref_table = 'H_T416'.
        gs_fcat-f4availabl = 'X'.
        MODIFY gt_fcat INDEX lv_cnt FROM gs_fcat
                       TRANSPORTING ref_field ref_table f4availabl.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SET_EVENT
*&---------------------------------------------------------------------*
*       Setting for event
*----------------------------------------------------------------------*
FORM set_event.
  CREATE OBJECT g_event_receiver.

  SET HANDLER g_event_receiver->data_changed          FOR g_grid.
  SET HANDLER g_event_receiver->data_changed_finished FOR g_grid.
  SET HANDLER g_event_receiver->double_click          FOR g_grid.
  SET HANDLER g_event_receiver->on_f4                 FOR g_grid.

ENDFORM.                    " SET_EVENT
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*       Create attributes of cell
*----------------------------------------------------------------------*
FORM build_cell_attr.
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl,
        lv_index   TYPE i.

  LOOP AT gt_out.
    CLEAR: lv_index, lt_celltab.
    REFRESH lt_celltab.

    lv_index = sy-tabix.

    CLEAR gs_fcat.
    LOOP AT gt_fcat INTO gs_fcat.
      ls_celltab-fieldname = gs_fcat-fieldname.

      IF ls_celltab-fieldname = 'CNFG' OR
         ls_celltab-fieldname = 'STLAL' OR
*        ls_celltab-fieldname = 'STLAN' or
         ls_celltab-fieldname = 'WERKS' OR
         ls_celltab-fieldname = 'MAKTX'.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      ELSE.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      ENDIF.

      CASE p_kalka.
        WHEN 'M1'.
          IF ls_celltab-fieldname = 'STLAN' OR
             ls_celltab-fieldname = 'VERID' OR
             ls_celltab-fieldname = 'CRINT' OR
             ls_celltab-fieldname = 'CREXT'.
            ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
          ENDIF.

        WHEN 'BP'.
          IF ls_celltab-fieldname = 'VERID'.
            ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
          ENDIF.

        WHEN OTHERS.
          IF ls_celltab-fieldname = 'STLAN'.
            ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
          ENDIF.
      ENDCASE.

      INSERT ls_celltab INTO TABLE lt_celltab.
    ENDLOOP.

    CLEAR gt_out-celltab.
    INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
    MODIFY gt_out INDEX lv_index.

  ENDLOOP.

ENDFORM.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  CREATE_F4_FIELDS
*&---------------------------------------------------------------------*
*       Define possible entry fields
*----------------------------------------------------------------------*
FORM create_f4_fields.
* F4 FIELD
  gs_f4-fieldname  = 'CREXT'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

  gs_f4-fieldname  = 'CRINT'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

  gs_f4-fieldname  = 'KALKA'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

* UD1K940522 by IG.MOON 5/10/2007
  gs_f4-fieldname  = 'VERID'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.
* end

  CALL METHOD g_grid->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4.

ENDFORM.                    " CREATE_F4_FIELDS
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save data to table ZTCOU100
*----------------------------------------------------------------------*
FORM save_data.
  TYPES: BEGIN OF ty_ztcou100.
          INCLUDE STRUCTURE ztcou100.
  TYPES:   prv,
           del,
         END OF ty_ztcou100.

  DATA: lt_ztcou100 TYPE TABLE OF ty_ztcou100 WITH HEADER LINE,
        lt_row      TYPE lvc_t_row,
        ls_row      TYPE lvc_s_row,
        lt_roid     TYPE lvc_t_roid,
        lv_cnt      TYPE i,
        lv_icnt     TYPE i,          " Count of saved data
        lv_dcnt     TYPE i,          " Count of deleted data
        lv_icntc(5),                 " Count of saved data
        lv_dcntc(5),                 " Count of deleted data
        lv_msg(200).                 " Message

* Save seleted data to table ZTCOU100
  CLEAR: lv_cnt, lv_dcnt, lt_ztcou100, lt_row[], lt_roid[].
  REFRESH lt_ztcou100.

  DELETE gt_out WHERE matnr IS INITIAL.

  IF gt_out[] IS INITIAL.
    IF gv_cnt > 0.
      DELETE FROM ztcou100
       WHERE kokrs = p_kokrs
         AND kalka = p_kalka
         AND bdatj = p_year
         AND poper = p_poper.
      IF sy-subrc = 0.
        MESSAGE s000 WITH 'You have deleted' gv_cnt 'records.'.
      ENDIF.
    ENDIF.

  ELSE.
    CLEAR lt_ztcou100.
    REFRESH lt_ztcou100.

*save only BoM Usage has value...
    LOOP AT gt_out WHERE werks <> space AND stlan <> space.
      MOVE-CORRESPONDING gt_out TO lt_ztcou100.
      lt_ztcou100-bdatj = p_year.
      lt_ztcou100-poper = p_poper.
      lt_ztcou100-kokrs = p_kokrs.
      lt_ztcou100-kalka = p_kalka.
      lt_ztcou100-aedat = sy-datum.
      lt_ztcou100-aenam = sy-uname.

      APPEND lt_ztcou100.
      CLEAR lt_ztcou100.

      lv_cnt = lv_cnt + 1.
    ENDLOOP.

    DELETE FROM ztcou100
     WHERE kokrs = p_kokrs
       AND kalka = p_kalka
       AND bdatj = p_year
       AND poper = p_poper.

    INSERT ztcou100 FROM TABLE lt_ztcou100.

    IF sy-subrc = 0.
      MESSAGE s000 WITH 'You have saved. Total:' sy-dbcnt.
    ENDIF.

  ENDIF.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PRV_PERIOD
*&---------------------------------------------------------------------*
*       Get previos period
*----------------------------------------------------------------------
FORM get_prv_period.
  IF p_poper = '1'.
    gv_pyear = p_year - 1.
    gv_ppoper = '12'.
  ELSE.
    gv_pyear = p_year.
    gv_ppoper = p_poper - 1.
  ENDIF.

ENDFORM.                    " GET_PRV_PERIOD
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       Event of changed data
*----------------------------------------------------------------------*
*      -->RR_DATA_CHANGED  Log is Visible
*----------------------------------------------------------------------*
FORM data_changed USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_cells     TYPE lvc_s_modi.

*  DELETE GT_OUT WHERE MATNR IS INITIAL.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.
    CHECK NOT ls_mod_cells-value IS INITIAL.

    READ TABLE gt_out INDEX ls_mod_cells-row_id.
    IF sy-subrc <> 0.
      APPEND gt_out.
    ENDIF.

    CASE ls_mod_cells-fieldname.
      WHEN 'MATNR'.
        PERFORM changed_matnr USING rr_data_changed
                                    ls_mod_cells.
    ENDCASE.

    MODIFY gt_out INDEX ls_mod_cells-row_id.

  ENDLOOP.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       Event of changed data finished
*----------------------------------------------------------------------*
FORM data_changed_finished USING e_modified TYPE char01.
  DATA: lv_stdpd     TYPE stdpd,          " Confi.Material
        lv_beskz     TYPE beskz,          " Procurement type
        lv_cuobj     TYPE cuobm,          " Object
        lv_crext     TYPE atwrt,
        lv_crint     TYPE atwrt.

  IF e_modified = 'X'.
    LOOP AT gt_out WHERE NOT matnr IS INITIAL
                      AND maktx IS INITIAL.
      CLEAR: lv_stdpd, lv_beskz.

      PERFORM chk_mat USING    gt_out-matnr
                      CHANGING lv_stdpd
                               gt_out-verid
                               lv_beskz
                               gt_out-stlal
                               gt_out-stlan
                               lv_cuobj
                               gt_out-maktx
                               gt_out-werks.
*     Configurable
      IF NOT lv_stdpd IS INITIAL.
        gt_out-cnfg = 'X'.
      ENDIF.

*     In-Color, Out-Color
      CLEAR lv_cuobj.
      SELECT SINGLE cuobj INTO lv_cuobj
        FROM marc
       WHERE matnr = gt_out-matnr
         AND cuobj <> '0'.

      IF NOT lv_cuobj IS INITIAL.
        CLEAR: gt_out-crext, gt_out-crint.

        PERFORM change_color USING lv_cuobj
                             CHANGING lv_crint
                                      lv_crext.
        gt_out-crint = lv_crint.
        gt_out-crext = lv_crext.

      ENDIF.

      MODIFY gt_out.
    ENDLOOP.

    CALL METHOD g_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = gt_fcat.

    CALL METHOD g_grid->set_frontend_layout
      EXPORTING
        is_layout = gs_layo.

*   Define cell attribute
    PERFORM build_cell_attr.

*   Define variant
    gs_variant-report = sy-repid.

*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo
        it_toolbar_excluding = gt_exclude
        i_save               = gc_var_save
        is_variant           = gs_variant
      CHANGING
        it_outtab            = gt_out[]
        it_fieldcatalog      = gt_fcat[].

    CALL METHOD cl_gui_cfw=>flush.

  ENDIF.

ENDFORM.                    " DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       move the detail screen when double click
*----------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.
  READ TABLE gt_out INDEX e_row-index.
  IF sy-subrc = 0.
    SET PARAMETER ID 'MAT'  FIELD gt_out-matnr.
    CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
*       Define possible entries
*----------------------------------------------------------------------*
FORM on_f4 USING sender         TYPE REF TO cl_gui_alv_grid
                 e_fieldname    TYPE lvc_fname
                 e_fieldvalue   TYPE lvc_value
                 es_row_no      TYPE lvc_s_roid
                 er_event_data  TYPE REF TO cl_alv_event_data
                 et_bad_cells   TYPE lvc_t_modi
                 e_display      TYPE char01.

  DATA lt_f4 TYPE TABLE OF ddshretval.

* Call my personal f4-help
  CALL METHOD g_event_receiver->my_f4
    EXPORTING
      sender        = sender
      es_row_no     = es_row_no
      er_event_data = er_event_data
      et_bad_cells  = et_bad_cells
      e_display     = e_display
      e_fieldname   = e_fieldname
    IMPORTING
      lt_f4         = lt_f4.

* Assign the cell table fieldsymbol to the dereferenced data table and
* fill the table.
  ASSIGN er_event_data->m_data->* TO <f4tab>.

  READ TABLE lt_f4 INTO ls_f4 INDEX 1.

  CHECK NOT ls_f4 IS INITIAL.

  PERFORM f4_aply USING es_row_no-row_id
                        ls_f4-fieldname.

* To avoid standard f4-help.
  er_event_data->m_event_handled = 'X'.

ENDFORM.                                                    " ON_F4
*&---------------------------------------------------------------------*
*&      Form  COPY_PRV_DATA
*&---------------------------------------------------------------------*
*       Copy data of previous period to current period
*----------------------------------------------------------------------*
FORM copy_prv_data.
  DATA: ls_out LIKE gt_out,          " Structure for display
        lv_answer.

* Get previous period
  PERFORM get_prv_period.

* Get data from table ZTCOU100
  REFRESH gt_ztcou100.
  CLEAR gt_ztcou100.

  SELECT * INTO TABLE gt_ztcou100
    FROM ztcou100
   WHERE bdatj = gv_pyear
     AND poper = gv_ppoper
     AND kalka = p_kalka.

  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'Data of previous period not found.'.
    EXIT.
  ENDIF.

* Copy data of previous period to current period
  DELETE gt_out WHERE bdatj IS INITIAL.

  LOOP AT gt_ztcou100.
    READ TABLE gt_out WITH KEY matnr = gt_ztcou100-matnr.
    IF sy-subrc <> 0.
      CLEAR ls_out.
      MOVE-CORRESPONDING gt_ztcou100 TO ls_out.
      ls_out-prv = 'X'.
      APPEND ls_out TO gt_out.

      gt_ztcou100-kokrs = p_kokrs.       " Conroling area
      gt_ztcou100-bdatj = p_year.        " Fiscal year
      gt_ztcou100-poper = p_poper.       " Period
      gt_ztcou100-aedat = sy-datum.      " Last changed by
      gt_ztcou100-aenam = sy-uname.      " Last changed on
      MODIFY gt_ztcou100.
    ENDIF.
  ENDLOOP.

  IF sy-subrc = 0.
*   Refresh ALV grid Control
    PERFORM refresh_field.
  ENDIF.

ENDFORM.                    " COPY_PRV_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       Delete Data
*----------------------------------------------------------------------*
FORM delete_data.
  DATA: lv_cnt(5),
        lv_index TYPE sytabix,
        lt_row   TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lt_roid  TYPE lvc_t_roid.

  CLEAR: lv_cnt, lv_index, lt_row[], lt_roid[].

* Delete selected data of table ZTCOU100
  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_row
      et_row_no     = lt_roid.

  LOOP AT lt_row INTO ls_row.
    READ TABLE gt_out INDEX ls_row-index.

    IF sy-subrc = 0.
      DELETE FROM ztcou100
       WHERE bdatj = gt_out-bdatj
         AND poper = gt_out-poper
         AND kalka = gt_out-kalka
         AND matnr = gt_out-matnr
         AND verid = gt_out-verid.

      IF sy-subrc = 0.
        gt_out-chk = 'X'.
        MODIFY gt_out INDEX ls_row-index TRANSPORTING chk.

        lv_cnt = lv_cnt + 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE gt_out WHERE chk = 'X'.

  IF lv_cnt > 0.
    MESSAGE s000 WITH 'You have deleted' lv_cnt 'records.'.
    PERFORM refresh_field.
  ENDIF.

ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  MY_F4
*&---------------------------------------------------------------------*
FORM my_f4 TABLES et_f4         STRUCTURE ddshretval
           USING  sender        TYPE REF TO cl_gui_alv_grid
                  et_bad_cells  TYPE lvc_t_modi
                  es_row_no     TYPE lvc_s_roid
                  er_event_data TYPE REF TO cl_alv_event_data
                  e_display     TYPE c
                  e_fieldname   TYPE lvc_fname.

  DATA : ls_out        LIKE LINE OF gt_out,
         lt_fcat       TYPE lvc_t_fcat,
         ls_fieldcat   TYPE lvc_s_fcat,
         lv_tabname    TYPE dd03v-tabname,
         lv_fieldname  TYPE dd03v-fieldname,
         lv_help_value TYPE help_info-fldvalue,
         lt_bad_cell   TYPE lvc_t_modi,
         l_wa          TYPE REF TO data.

  FIELD-SYMBOLS : <l_field_value> TYPE any,
                  <ls_wa>         TYPE any.

  CALL METHOD sender->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = lt_fcat.

  READ TABLE gt_out INDEX es_row_no-row_id INTO ls_out.

  IF sy-subrc = 0.
    CREATE DATA l_wa LIKE LINE OF gt_out.
    ASSIGN l_wa->* TO <ls_wa>.
    <ls_wa> = ls_out.
  ENDIF.

  READ TABLE lt_fcat WITH KEY fieldname = e_fieldname
                     INTO ls_fieldcat.
  IF sy-subrc = 0.
    lv_tabname = ls_fieldcat-ref_table.
    lv_fieldname = ls_fieldcat-fieldname.

    ASSIGN COMPONENT ls_fieldcat-fieldname
                  OF STRUCTURE ls_out TO <l_field_value>.

    WRITE <l_field_value> TO lv_help_value.
  ENDIF.

  PERFORM f4_set IN PROGRAM bcalv_f4
                 USING sender
                       lt_fcat
                       lt_bad_cell
                       es_row_no-row_id
                       <ls_wa>.

* Fill internal table for possible entry
  CLEAR  : gs_values, gt_values, gs_fields, gt_fields.
  REFRESH: gt_values, gt_fields.

  IF e_fieldname = 'CRINT' OR e_fieldname = 'CREXT'.
    PERFORM get_color USING e_fieldname.
*
  ELSEIF e_fieldname = 'KALKA'.
    PERFORM get_costing_type.

* UD1K940522 by IG. MOON 5/10/2007
  ELSEIF e_fieldname = 'VERID'.
    PERFORM get_verid USING es_row_no-row_id e_fieldname.
  ENDIF.
* end

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = e_fieldname
    TABLES
      field_tab       = gt_fields
      value_tab       = gt_values
      return_tab      = et_f4[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                                                    " MY_F4
*&---------------------------------------------------------------------*
*&      Form  F4_APLY
*&---------------------------------------------------------------------*
FORM f4_aply USING  es_row_no_row_id
                    e_fieldname TYPE fieldname.
  ls_modi-row_id    = es_row_no_row_id.
  ls_modi-fieldname = e_fieldname.
  ls_modi-value     = ls_f4-fieldval.
  APPEND ls_modi TO <f4tab>.

  CASE e_fieldname.
    WHEN 'KALKA'.
      gt_out-kalka = ls_f4-fieldval.
    WHEN 'CRINT'.
      gt_out-crint = ls_f4-fieldval.
    WHEN 'CREXT'.
      gt_out-crext = ls_f4-fieldval.
    WHEN 'VERID'.
      gt_out-verid = ls_f4-fieldval.
  ENDCASE.

  READ TABLE gt_out INDEX es_row_no_row_id.

ENDFORM.                                                    " F4_APLY
*&---------------------------------------------------------------------*
*&      Form  GET_COLOR
*&---------------------------------------------------------------------*
*       Get charactristic value of color
*----------------------------------------------------------------------*
FORM get_color USING e_fieldname TYPE lvc_fname.
  DATA lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE.

  PERFORM get_characteristics TABLES lt_values.

* Out-color
  IF e_fieldname = 'CREXT'.
    LOOP AT lt_values WHERE name_char = 'COLOREXT'.
      gs_values-string = lt_values-char_value.
      APPEND gs_values TO gt_values.
    ENDLOOP.

    gs_fields-fieldname = 'CREXT'.
    gs_fields-reptext = 'Out-Color'.

* In-color
  ELSEIF e_fieldname = 'CRINT'.
    LOOP AT lt_values WHERE name_char = 'COLORINT'.
      gs_values-string = lt_values-char_value.
      APPEND gs_values TO gt_values.
    ENDLOOP.

    gs_fields-fieldname = 'CRINT'.
    gs_fields-reptext   = 'In-Color'.
  ENDIF.

  gs_fields-intlen    =  2.
  gs_fields-leng      =  2.
  gs_fields-outputlen =  2.

  APPEND gs_fields TO gt_fields.

ENDFORM.                    " GET_COLOR
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       Setting for layout
*----------------------------------------------------------------------*
FORM set_lvc_layout.
  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-cwidth_opt = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-stylefname = 'CELLTAB'.

ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  CHK_MAT
*&---------------------------------------------------------------------*
*       Check FSC / MIP / Module
*----------------------------------------------------------------------*
*      -->P_MATNR  Material
*      -->P_STDPD  Confi.Material
*      -->P_VERID  Production Version
*      -->P_BESKZ  Procurement Type
*      -->P_STLAL  Alternative BOM
*      -->P_MAKTX  Material Description
*      -->P_WERKS  Plant
*----------------------------------------------------------------------*
FORM chk_mat USING    p_matnr TYPE matnr
             CHANGING p_stdpd TYPE stdpd
                      p_verid TYPE verid
                      p_beskz TYPE beskz
                      p_stlal TYPE stalt
                      p_stlan TYPE stlan
                      p_cuobj TYPE cuobm
                      p_maktx TYPE maktx
                      p_werks TYPE werks_d.

  TYPES: BEGIN OF ty_mkal,
           verid TYPE verid,   " Production Version
           stlal TYPE stalt,   " Alternative BOM
           stlan TYPE stlan,
         END OF ty_mkal.

  DATA: lt_mkal  TYPE TABLE OF ty_mkal WITH HEADER LINE,
        lv_date  TYPE sydatum,
        l_stlnr  TYPE stnum,
        l_stlal  TYPE stalt,
        l_datuv  TYPE datuv.

  RANGES: r_verid  FOR mkal-verid.

* [FSC]
*   1. Material type: HALB or FERT or KMAT(Configurable)
*   2. Costing date in the period of validity
*   3. has BOM : exist BOM Usage(MKAL-STLAN)
*   4. has Routing
*     : task list type(MKAL-PLTYG):R, exist task list group(MKAL-PLNNG)
*   5. Status: Green or Yellow (MKAL-PRTG_S: 1 or 2)
*   6. Unchecked 'No Costing' (MARC-NCOST)

* [MIP]
*   - Material type: HALB
*   - other conditions are same FSC

  CLEAR:  lv_date, lt_mkal.
  REFRESH lt_mkal.

*valid on middle of period; FIXME - ANDY
  CONCATENATE p_year  p_poper+1(2) '15' INTO lv_date.

* Material Description
  SELECT SINGLE maktx INTO p_maktx
    FROM makt
   WHERE matnr = p_matnr
     AND spras = sy-langu.

* EXCEPT MODULE COSTING
  CASE p_kalka.

* [Module]
*   - Procurement type : F ONLY; X-ERROR DURING COSTING
*     BOM Usage: 2
    WHEN 'M1'.
      SELECT SINGLE a~beskz fvidk a~werks
        INTO (p_beskz, p_verid, p_werks)
        FROM marc AS a
        JOIN mast AS b
          ON b~matnr = a~matnr
         AND b~werks = a~werks
         AND b~stlan = '2'                      " BOM Usage
       WHERE a~matnr = p_matnr
         AND a~beskz = 'F'.                     "MUST be 'F'

      p_stlan = '2'.

    WHEN 'BP'.

      SELECT SINGLE a~beskz a~werks a~stdpd
                    b~stlnr b~stlal
        INTO (p_beskz, p_werks, p_stdpd,
              l_stlnr, l_stlal)
        FROM marc AS a LEFT OUTER JOIN mast AS b
          ON b~matnr = a~matnr
         AND b~werks = a~werks
         AND b~stlan = '6'                      " BOM Usage : ABP
       WHERE a~matnr = p_matnr
         AND ( a~beskz = 'E' OR a~beskz = 'X' )
* by ig.moon 10/15/12 {
         AND a~lvorm EQ space.
* }

      IF sy-subrc = 0 AND l_stlnr <> space.
        CLEAR l_datuv.
        SELECT MAX( datuv ) INTO l_datuv FROM stko
          WHERE stlty = 'M'
            AND stlnr = l_stlnr
            AND stlal = l_stlal.
        IF l_datuv <= lv_date.
          p_stlan = '6'.
        ELSE.
          p_stlan = '1'.  "???
        ENDIF.
      ELSE.
        p_stlan = '1'.  "???
      ENDIF.

* UNIT COST
    WHEN OTHERS.
      SELECT SINGLE b~beskz b~werks b~fvidk b~stdpd
        INTO (p_beskz, p_werks, p_verid, p_stdpd)
        FROM mara AS a
        JOIN marc AS b
          ON b~matnr = a~matnr
       WHERE a~matnr = p_matnr
         AND ( b~beskz = 'E' OR b~beskz = 'X' )
         AND ( a~mtart = 'HALB' OR        "1. Material Type
               a~mtart = 'FERT' OR
               a~mtart = 'KMAT' )
         AND b~ncost <> 'X'               "6. Unchecked 'No Costing'
         AND b~lvorm = ' '.                                 "UD1K954655

      REFRESH r_verid.
      IF NOT p_verid IS INITIAL.
        r_verid-low = p_verid.
        r_verid-option = 'EQ'. r_verid-sign = 'I'.
        APPEND r_verid.
      ENDIF.

      SELECT SINGLE verid stlal stlan INTO lt_mkal
        FROM mkal
       WHERE matnr  = p_matnr
         AND werks  = p_werks
         AND bdatu >= gv_ldate         " 2. period of validity
         AND adatu <= gv_ldate         " 2. period of validity
         AND stlan <> space            " 3. exist BOM Usage
     AND prfg_s <> '3'                                      " 5. Status
         AND verid IN r_verid.

      IF sy-subrc = 0.
        p_verid = lt_mkal-verid.            " Production Version
        p_stlal = lt_mkal-stlal.            " Alternative BOM
        p_stlan = '1'.                      " production
      ENDIF.

  ENDCASE.



ENDFORM.                    " CHK_MAT
*&---------------------------------------------------------------------*
*&      Form  GET_CHARACTERISTICS
*&---------------------------------------------------------------------*
*       Get Characteristics
*----------------------------------------------------------------------*
FORM get_characteristics TABLES lt_values STRUCTURE bapi_char_values.
  DATA lt_chars TYPE TABLE OF bapi_char WITH HEADER LINE.

  CLEAR:   lt_chars, lt_values.
  REFRESH: lt_chars, lt_values.

  CALL FUNCTION 'BAPI_CLASS_GET_CHARACTERISTICS'
    EXPORTING
      classnum        = 'NFA'
      classtype       = '300'
    TABLES
      characteristics = lt_chars
      char_values     = lt_values.

ENDFORM.                    " GET_CHARACTERISTICS
*&---------------------------------------------------------------------*
*&      Form  CHANGED_MATNR
*&---------------------------------------------------------------------*
*       when changed material
*----------------------------------------------------------------------*
FORM changed_matnr USING  rr_data_changed
                         TYPE REF TO cl_alv_changed_data_protocol
                      ls_mod_cells TYPE lvc_s_modi.

  DATA: lv_matnr     TYPE matnr,          " Material
        lv_stdpd     TYPE stdpd,          " Confi.Material
        lv_beskz     TYPE beskz,          " Procurement type
        lv_verid     TYPE verid,          " Production version
        lv_stlal     TYPE stalt,          " Alternative BOM
        lv_stlan     TYPE stlan,
        lv_cuobj     TYPE cuobm,          " Object
        lv_maktx     TYPE maktx,          " Material Description
        lv_werks     TYPE werks_d,        " Plant
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE,
        lw_msg       TYPE symsgv.

  CLEAR: lv_matnr, lv_stdpd, lv_verid, lv_beskz, lv_stlal, lv_stlan.
  lv_matnr = ls_mod_cells-value.

  PERFORM chk_mat USING    lv_matnr
                  CHANGING lv_stdpd
                           lv_verid
                           lv_beskz
                           lv_stlal
                           lv_stlan
                           lv_cuobj
                           lv_maktx
                           lv_werks.

*Unit costing only--- Error Check
  IF lv_stlan IS INITIAL OR
     lv_beskz IS INITIAL.
    CONCATENATE ls_mod_cells-value ' : check material & bom'
           INTO lw_msg.
    PERFORM data_input_error
            USING rr_data_changed
                  ls_mod_cells
                  'W'
                  lw_msg
                  'MATNR'.

  ELSE.
*   Modify Material & Configurable & Version
    PERFORM modify_cell_matnr_info
                USING rr_data_changed
                      ls_mod_cells
                      lv_matnr
                      lv_stdpd
                      lv_verid
                      lv_stlal
                      lv_stlan
                      lv_maktx
                      lv_werks.
  ENDIF.

ENDFORM.                    " CHANGED_MATNR
*&---------------------------------------------------------------------*
*&      Form  MODIFY_CELL_MATNR_INFO
*&---------------------------------------------------------------------*
*       Modify Configurable, Version, Alternative BOM
*----------------------------------------------------------------------*
*      -->P_MATNR  Material
*      -->P_MTART  Material Code
*      -->P_VERID  Production Version
*      -->P_STLAL  Alternative BOM
*      -->P_MAKTX  Material Descrption
*      -->P_WERKS  Plant
*----------------------------------------------------------------------*
FORM modify_cell_matnr_info
       USING rr_data_changed TYPE REF TO cl_alv_changed_data_protocol
             ls_mod_cells STRUCTURE lvc_s_modi
             p_matnr TYPE matnr
             p_stdpd TYPE stdpd
             p_verid TYPE verid
             p_stlal TYPE stalt
             p_stlan TYPE stlan
             p_maktx TYPE maktx
             p_werks TYPE werks_d.

  CHECK NOT p_matnr IS INITIAL.

  CALL METHOD rr_data_changed->modify_cell
    EXPORTING
      i_row_id    = ls_mod_cells-row_id
      i_fieldname = ls_mod_cells-fieldname
      i_value     = ls_mod_cells-value.

* Modify Material Description
  IF NOT p_maktx IS INITIAL.
    CALL METHOD rr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_mod_cells-row_id
        i_fieldname = 'MAKTX'
        i_value     = p_maktx.
    gt_out-maktx = p_maktx.
  ENDIF.

* Modify Configurable
  IF NOT p_stdpd IS INITIAL.
    CALL METHOD rr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_mod_cells-row_id
        i_fieldname = 'CNFG'
        i_value     = 'X'.
    gt_out-cnfg = 'X'.
  ENDIF.

*BOM Usage
  IF NOT p_stlan IS INITIAL.
    CALL METHOD rr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_mod_cells-row_id
        i_fieldname = 'STLAN'
        i_value     = p_stlan.
    gt_out-stlan = p_stlan.
  ENDIF.

* Modify Version
*  if not p_verid is initial.
  CALL METHOD rr_data_changed->modify_cell
    EXPORTING
      i_row_id    = ls_mod_cells-row_id
      i_fieldname = 'VERID'
      i_value     = p_verid.
  gt_out-verid = p_verid.
*  endif.

* Modify Alternative BOM
  IF NOT p_stlal IS INITIAL.
    CALL METHOD rr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_mod_cells-row_id
        i_fieldname = 'STLAL'
        i_value     = p_stlal.
    gt_out-stlal = p_stlal.
  ENDIF.

* Modify In-Color, Out-Color
  DATA lv_cuobj TYPE cuobm.

  CLEAR lv_cuobj.
  SELECT SINGLE cuobj INTO lv_cuobj
    FROM marc
   WHERE matnr = p_matnr
     AND cuobj <> '0'.

  IF NOT lv_cuobj IS INITIAL.
    DATA: lv_crext    TYPE atwrt,
          lv_crint    TYPE atwrt.

    CLEAR: lv_crext, lv_crint.

* UD1K940523
* Bug fix by IG.MOON 5/10/2007
*    PERFORM change_color USING lv_cuobj
*                         CHANGING lv_crint
*                                  lv_crext.
    PERFORM change_color USING lv_cuobj
                         CHANGING lv_crext
                                  lv_crint.
* end

    IF NOT lv_crint IS INITIAL.
      CALL METHOD rr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_mod_cells-row_id
          i_fieldname = 'CRINT'
          i_value     = lv_crint.
      gt_out-crint = lv_crint.
    ENDIF.

    IF NOT lv_crext IS INITIAL.
      CALL METHOD rr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_mod_cells-row_id
          i_fieldname = 'CREXT'
          i_value     = lv_crext.

      gt_out-crext = lv_crext.
    ENDIF.

  ENDIF.

* Modify Plant
  IF NOT p_werks IS INITIAL.
    CALL METHOD rr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_mod_cells-row_id
        i_fieldname = 'WERKS'
        i_value     = p_werks.
    gt_out-werks = p_werks.
  ENDIF.


ENDFORM.                    " MODIFY_CELL_MATNR_CNFG_VERID
*&---------------------------------------------------------------------*
*&      Form  CHANGED_COLOR
*&---------------------------------------------------------------------*
*       when changed color
*----------------------------------------------------------------------*
FORM changed_color USING rr_data_changed
                             TYPE REF TO cl_alv_changed_data_protocol
                         ls_mod_cells TYPE lvc_s_modi.
  DATA: lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE,
        lv_field  TYPE lvc_fname,
        lv_msgv1  TYPE symsgv.

  PERFORM get_characteristics TABLES lt_values.

  READ TABLE lt_values WITH KEY char_value = ls_mod_cells-value.

  IF sy-subrc = 0.
    IF ls_mod_cells-fieldname = 'CRINT'.
      gt_out-crint = ls_mod_cells-value.       " In-Color
    ELSEIF ls_mod_cells-fieldname = 'CREXT'.
      gt_out-crext = ls_mod_cells-value.       " Out-Color
    ENDIF.

*   Modify cell
    CALL METHOD rr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_mod_cells-row_id
        i_fieldname = ls_mod_cells-fieldname
        i_value     = ls_mod_cells-value.
  ELSE.
    CLEAR: lv_field, lv_msgv1.

    IF ls_mod_cells-fieldname = 'CRINT'.
      lv_field = 'CRINT'.
      lv_msgv1 = 'Not matched internal color.'.
    ELSEIF ls_mod_cells-fieldname = 'CREXT'.
      lv_field = 'CREXT'.
      lv_msgv1 = 'Not matched external color.'.
    ENDIF.

*   Error Message Display
    PERFORM data_input_error USING rr_data_changed
                                   ls_mod_cells
                                   'E'
                                   lv_msgv1
                                   lv_field.
  ENDIF.

ENDFORM.                    " CHANGED_COLOR
*&---------------------------------------------------------------------*
*&      Form  GET_COSTING_TYPE
*&---------------------------------------------------------------------*
*       Get Costing Type Possible Entry
*----------------------------------------------------------------------*
FORM get_costing_type.
  PERFORM get_gt_kalka.

  gs_fields-fieldname = 'KALKA'.
  gs_fields-reptext   = 'Costing Type'.

  gs_fields-intlen    =  2.
  gs_fields-leng      =  2.
  gs_fields-outputlen =  2.

  APPEND gs_fields TO gt_fields.

ENDFORM.                    " GET_COSTING_TYPE
*&---------------------------------------------------------------------*
*&      Form  GET_GT_KALKA
*&---------------------------------------------------------------------*
*       Get Costing Type
*----------------------------------------------------------------------*
FORM get_gt_kalka.
  DATA lv_nym(10) VALUE '0123456789'.

  CLEAR gt_kalka.
  REFRESH gt_kalka.

  SELECT a~kalka b~txkla
    INTO TABLE gt_kalka
    FROM tck01 AS a
    JOIN tck02 AS b
      ON b~spras = sy-langu
     AND b~kalka = a~kalka
   WHERE a~bzobj = '0'.

  DELETE gt_kalka
    WHERE kalka+0(1) CA lv_nym
       OR kalka = 'P1'.

  LOOP AT gt_kalka.
    gs_values-string = gt_kalka-kalka.
    APPEND gs_values TO gt_values.
  ENDLOOP.

ENDFORM.                    " GET_GT_KALKA
*&---------------------------------------------------------------------*
*&      Form  CHANGED_KALKA
*&---------------------------------------------------------------------*
*       Check Costing type
*----------------------------------------------------------------------*
FORM changed_kalka USING
               rr_data_changed TYPE REF TO cl_alv_changed_data_protocol
               ls_mod_cells    TYPE lvc_s_modi.

  PERFORM get_gt_kalka.
  READ TABLE gt_kalka WITH KEY kalka = ls_mod_cells-value.

  IF sy-subrc = 0.
    gt_out-kalka = ls_mod_cells-value.
    CALL METHOD rr_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_mod_cells-row_id
        i_fieldname = ls_mod_cells-fieldname
        i_value     = ls_mod_cells-value.

  ELSE.
    PERFORM data_input_error USING rr_data_changed
                                   ls_mod_cells
                                   'E'
                                   'Wrong costing type.'
                                   'KALKA'.
  ENDIF.

ENDFORM.                    " CHANGED_KALKA
*&---------------------------------------------------------------------*
*&      Form  GET_COLOR_VAL
*&---------------------------------------------------------------------*
*       Color Value
*----------------------------------------------------------------------*
FORM get_color_val USING    p_in_recno TYPE ib_recno
                   CHANGING p_crext TYPE atwrt
                            p_crint TYPE atwrt.
  TYPES: BEGIN OF ty_cabn,
           atinn TYPE atinn,
           atnam TYPE atnam,
         END OF ty_cabn.

  DATA lt_cabn TYPE TABLE OF ty_cabn WITH HEADER LINE.

  CLEAR: lt_cabn, p_crext, p_crint.
  REFRESH lt_cabn.

  SELECT atinn atnam INTO TABLE lt_cabn
    FROM cabn
   WHERE atnam = 'COLOREXT' OR atnam = 'COLORINT'.

  IF sy-subrc = 0.
    LOOP AT lt_cabn.
      CASE lt_cabn-atnam.
        WHEN 'COLOREXT'.
          SELECT SINGLE atwrt INTO p_crext
            FROM v_ibin_syval
           WHERE atinn = lt_cabn-atinn
             AND in_recno = p_in_recno.

        WHEN 'COLORINT'.
          SELECT SINGLE atwrt INTO p_crint
            FROM v_ibin_syval
           WHERE atinn = lt_cabn-atinn
             AND in_recno = p_in_recno.
      ENDCASE.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_COLOR_VAL
*&---------------------------------------------------------------------*
*&      Form  CHANGE_COLOR
*&---------------------------------------------------------------------*
*       Change In-color, Out-color
*----------------------------------------------------------------------*
FORM change_color USING    p_cuobj TYPE cuobm
                  CHANGING p_crext TYPE atwrt
                           p_crint TYPE atwrt.

  DATA: lv_in_recno TYPE ib_recno.

  CLEAR: lv_in_recno, p_crext, p_crint.

  SELECT SINGLE in_recno INTO lv_in_recno
    FROM ibin
   WHERE instance = p_cuobj.

  IF sy-subrc = 0.
    PERFORM get_color_val USING lv_in_recno
                          CHANGING p_crext
                                   p_crint.
  ENDIF.

ENDFORM.                    " CHANGE_COLOR
*&---------------------------------------------------------------------*
*&      Form  BDC_MAT
*&---------------------------------------------------------------------*
FORM bdc_mat.
* Setting BDC options
  CLEAR gs_opt.
  gs_opt-dismode  = 'E'.
  gs_opt-updmode  = 'X'.
  gs_opt-racommit = 'X'.

  CLEAR: gt_row[], gt_roid[].

* Get selected rows
  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_row
      et_row_no     = gt_roid.

* Change Material Master
  LOOP AT gt_row INTO gs_row.
    READ TABLE gt_out INDEX gs_row-index.

    IF sy-subrc = 0.
      PERFORM bdc_mm02.
    ENDIF.
  ENDLOOP.

* Refresh ALV grid
  PERFORM refresh_field.

ENDFORM.                    " BDC_MAT
*&---------------------------------------------------------------------*
*&      Form  BDC_MM02
*&---------------------------------------------------------------------*
*       BDC Process for Change Material
*----------------------------------------------------------------------*
FORM bdc_mm02.
* [MM02] : Excute Change Material Master BDC
* : Change Production version.
*   If the costing type is MIP, change BOM usage.
*   If the material is configuable, change color.

*[PVER] Excute Update material master: Clear Prod.Ver
* : Clear Production version.
  REFRESH: gt_bdc, gt_msg.
  CLEAR  : gt_bdc, gt_msg.

  PERFORM dynpro USING:
     'X'  'SAPLMGMM'               '0060',
     ' '  'RMMG1-MATNR'            gt_out-matnr,
     ' '  'BDC_OKCODE'             '=AUSW',        " [Select View]

     'X'  'SAPLMGMM'                '0070',
     ' '  'BDC_OKCODE'             '=SELA',        " [Select All]

     'X'  'SAPLMGMM'                '0070',
     ' '  'BDC_OKCODE'             '=ENTR',        " [Enter]

     'X'  'SAPLMGMM'                '0080',
     ' '  'BDC_OKCODE'             '=ENTR',        " [Enter]
     ' '  'RMMG1-WERKS'            gt_out-werks,   " Plant

     'X'  'SAPLMGMM'               '5004',
     ' '  'BDC_OKCODE'             '=SP26'.        " [Costing 1]

* [Costing 1]
  PERFORM dynpro USING:
     'X'  'SAPLMGMM'               '5000'.

* BOM Usage
  IF gt_out-kalka = 'M1' OR
     gt_out-kalka = 'BP'.

*ANDY - CK40N is not working...
*   perform dynpro using   ' '  'MARC-STLAN'  '2'.

  ELSE.
* Version only for unit costing
    IF ok_code EQ 'PVER'.
      PERFORM dynpro USING ' '  'MARC-FVIDK' ' '.
    ELSE.
      PERFORM dynpro USING ' '  'MARC-FVIDK' gt_out-verid.
    ENDIF.
  ENDIF.

**** UD1K940522
**** Disable Color change
***  CLEAR gt_out-cnfg.
**** end
***
**** [MRP 3]: Color
***  IF gt_out-cnfg = 'X'.            " -> Configuable Material
***    PERFORM dynpro USING:
***      ' '  'BDC_OKCODE'             '=SP14',                " [MRP 3]
***
***      'X'  'SAPLMGMM'               '5000',
***    ' '  'BDC_OKCODE'             '=PB19',          " [Config.
*Variant]
***
***      'X'  'SAPLCEI0'               '0109',
***      ' '  'BDC_OKCODE'             '=BACK',          " [Back]
***      ' '  'RCTMS-MNAME(01)'        'COLOREXT',
***      ' '  'RCTMS-MWERT(01)'        gt_out-crext,     " External
*Color
***      ' '  'RCTMS-MNAME(02)'        'COLORINT',
***      ' '  'RCTMS-MWERT(02)'        gt_out-crint,     " Internal
*Color
***
***      'X'  'SAPLMGMM'               '5000',
***      ' '  'BDC_OKCODE'             '=BU'.            " [Save]
***
**  ELSE.
  PERFORM dynpro USING  ' '  'BDC_OKCODE'  '=BU'.   " [Save]
***  ENDIF.


  CALL TRANSACTION 'MM02'  USING         gt_bdc
                           OPTIONS FROM  gs_opt
                           MESSAGES INTO gt_msg.

  READ TABLE gt_msg WITH KEY msgtyp = 'S'
                             msgid  = 'M3'
                             msgnr  = '801'.

  IF sy-subrc = 0.
    PERFORM save_ztcou100.      " Update Table ZTCOU100
    MESSAGE s000 WITH 'Changed Material Master.'.
  ENDIF.

ENDFORM.                                                    " BDC_MM02
*&---------------------------------------------------------------------*
*&      Form  SAVE_ZTCOU100
*&---------------------------------------------------------------------*
*       Save Table ZTCOU100
*----------------------------------------------------------------------*
FORM save_ztcou100.
  DATA ls_ztcou100 LIKE ztcou100.

  CLEAR ls_ztcou100.

  DELETE FROM ztcou100
    WHERE bdatj = p_year
      AND poper = p_poper
      AND kokrs = p_kokrs
      AND kalka = p_kalka
      AND matnr = gt_out-matnr.

  MOVE-CORRESPONDING gt_out TO ls_ztcou100.
  ls_ztcou100-bdatj = p_year.
  ls_ztcou100-poper = p_poper.
  ls_ztcou100-kokrs = p_kokrs.
  ls_ztcou100-kalka = p_kalka.
  ls_ztcou100-aedat = sy-datum.
  ls_ztcou100-aenam = sy-uname.

  INSERT INTO ztcou100 VALUES ls_ztcou100.

ENDFORM.                    " SAVE_ZTCOU100
*&---------------------------------------------------------------------*
*&      Form  fill_from_ml
*&---------------------------------------------------------------------*
FORM fill_from_ml.
  DATA : BEGIN OF it_ckmlmv003 OCCURS 0,
           bwkey      LIKE ckmlmv001-bwkey,
           matnr      LIKE ckmlmv001-matnr,
           aufnr      LIKE ckmlmv013-aufnr,
           verid_nd   LIKE ckmlmv001-verid_nd,
           meinh      LIKE ckmlmv003-meinh,
           out_menge  LIKE ckmlmv003-out_menge,
         END OF  it_ckmlmv003.

  DATA : BEGIN OF it_proc_gr OCCURS 0,
          werks LIKE ztco_shop_sum-bwkey,
          artnr     LIKE ztco_shop_sum-artnr,
        END OF it_proc_gr.

  DATA : it_ckmlmv003_temp LIKE it_ckmlmv003 OCCURS 0 WITH HEADER LINE.

  SELECT  b~bwkey b~matnr b~verid_nd
          c~aufnr
          a~out_menge a~meinh
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlmv003_temp
    FROM
    ( ( ckmlmv003 AS a
    INNER JOIN ckmlmv001 AS b
       ON b~kalnr = a~kalnr_bal )
    INNER JOIN ckmlmv013 AS c
       ON c~kalnr_proc = a~kalnr_in
    INNER JOIN mara AS d
       ON d~matnr = c~pmatn )
   WHERE a~mgtyp EQ '00001'
     AND a~gjahr EQ p_year
     AND a~perio EQ p_poper
*    AND a~werks EQ it_proc_gr-werks
     AND a~matnr IN s_matnr
     AND a~werks IN s_werks
     AND d~mtart IN s_mtart
     AND b~btyp  =  'BF'
     AND b~bwkey = a~werks
     AND c~flg_wbwg = 'X'
     AND c~autyp = '05' .

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_ckmlmv013  TABLES it_ckmlmv003_temp.
  ENDIF.
*- U1 End

  LOOP AT it_ckmlmv003_temp.
    MOVE-CORRESPONDING it_ckmlmv003_temp TO it_ckmlmv003.
    CLEAR: it_ckmlmv003-verid_nd,
           it_ckmlmv003-aufnr.
    COLLECT it_ckmlmv003. CLEAR it_ckmlmv003.
  ENDLOOP.

  SORT it_ckmlmv003 BY matnr.


*
  DATA: lv_stdpd     TYPE stdpd,          " Confi.Material
        lv_beskz     TYPE beskz,          " Procurement type
        lv_verid     TYPE verid,          " Production version
        lv_stlal     TYPE stalt,          " Alternative BOM
        lv_stlan     TYPE stlan,
        lv_cuobj     TYPE cuobm,          " Object
        lv_maktx     TYPE maktx,          " Material Description
        lv_werks     TYPE werks_d,        " Plant
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE,
        lw_msg       TYPE symsgv.

  LOOP AT it_ckmlmv003.
    CLEAR: lv_stdpd, lv_verid, lv_beskz, lv_stlal, lv_stlan.
    PERFORM chk_mat USING    it_ckmlmv003-matnr
                    CHANGING lv_stdpd
                             lv_verid
                             lv_beskz
                             lv_stlal
                             lv_stlan
                             lv_cuobj
                             lv_maktx
                             lv_werks.
    CLEAR gt_ztcou100.
    gt_ztcou100-kokrs = p_kokrs.
    gt_ztcou100-kalka = p_kalka.
    gt_ztcou100-bdatj = p_year.
    gt_ztcou100-poper = p_poper.
    gt_ztcou100-matnr = it_ckmlmv003-matnr.
    gt_ztcou100-werks = lv_werks.

    IF NOT lv_stdpd IS INITIAL.
      gt_ztcou100-cnfg  = 'X'.
    ENDIF.

    gt_ztcou100-verid = lv_verid.
    gt_ztcou100-stlal = lv_stlal.
    gt_ztcou100-stlan = lv_stlan.
    gt_ztcou100-maktx = lv_maktx.
    APPEND gt_ztcou100.

  ENDLOOP.


  DESCRIBE TABLE gt_ztcou100 LINES gv_cnt.

ENDFORM.                    " fill_from_ml
*&---------------------------------------------------------------------*
*&      Form  fill_from_103
*&---------------------------------------------------------------------*
FORM fill_from_103.
  DATA : BEGIN OF lt_103 OCCURS 0,
           matnr   LIKE ztcou103-compn,
           werks   LIKE ztcou103-werks,
           maktx   LIKE makt-maktx,
         END OF  lt_103.

  SELECT DISTINCT a~compn a~splnt k~maktx
     INTO TABLE lt_103
     FROM ztcou103 AS a
        JOIN marc AS b
          ON a~compn = b~matnr
         AND a~splnt = b~werks
        JOIN mast AS c
          ON c~matnr = b~matnr
         AND c~werks = b~werks
         AND c~stlan = '2'                      " BOM Usage
        JOIN makt AS k
          ON k~matnr = b~matnr
         AND k~spras = sy-langu

   WHERE a~kokrs = p_kokrs
     AND a~bdatj = p_year
     AND a~kalka = 'U1'
     AND a~poper = p_poper
     AND a~compn IN s_matnr
     AND b~beskz = 'F'.                      "MUST be 'F'
*   group by a~compn a~werks k~maktx.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_ztcou103 TABLES lt_103.
  ENDIF.
*- U1 End
*
  DATA: lv_stdpd     TYPE stdpd,          " Confi.Material
        lv_beskz     TYPE beskz,          " Procurement type
        lv_verid     TYPE verid,          " Production version
        lv_stlal     TYPE stalt,          " Alternative BOM
        lv_stlan     TYPE stlan,
        lv_cuobj     TYPE cuobm,          " Object
        lv_maktx     TYPE maktx,          " Material Description
        lv_werks     TYPE werks_d,        " Plant
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE,
        lw_msg       TYPE symsgv.

  LOOP AT lt_103.
    CLEAR: lv_stdpd, lv_verid, lv_beskz, lv_stlal, lv_stlan.
    PERFORM chk_mat USING    lt_103-matnr
                    CHANGING lv_stdpd
                             lv_verid
                             lv_beskz
                             lv_stlal
                             lv_stlan
                             lv_cuobj
                             lv_maktx
                             lv_werks.
    CLEAR gt_ztcou100.
    gt_ztcou100-kokrs = p_kokrs.
    gt_ztcou100-kalka = p_kalka.
    gt_ztcou100-bdatj = p_year.
    gt_ztcou100-poper = p_poper.
    gt_ztcou100-matnr = lt_103-matnr.
    gt_ztcou100-werks = lv_werks.

    IF NOT lv_stdpd IS INITIAL.
      gt_ztcou100-cnfg  = 'X'.
    ENDIF.

    gt_ztcou100-verid = lv_verid.
    gt_ztcou100-stlal = lv_stlal.
    gt_ztcou100-stlan = lv_stlan.
    gt_ztcou100-maktx = lv_maktx.
    APPEND gt_ztcou100.

  ENDLOOP.


  DESCRIBE TABLE gt_ztcou100 LINES gv_cnt.

ENDFORM.                    " fill_from_103
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_CKMLMV013
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CKMLMV003_TEMP  text
*----------------------------------------------------------------------*
FORM archive_read_ckmlmv013 TABLES pt_ckmlmv003 STRUCTURE gt_ckmlmv003_a.

  TYPES: BEGIN OF ty_ckmlmv013,
         aufnr      TYPE aufnr,
         autyp      TYPE auftyp,
         kalnr_proc TYPE ckml_f_procnr,
         pkosa      TYPE pkosa_d,
         aedat      TYPE ckml_aedat,
         flg_wbwg   TYPE ckml_flg_wbwg,
         loekz      TYPE ckml_loekz,
         prwrk      TYPE werks_d,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ckmlmv013.

  DATA: l_handle    TYPE sytabix,
        lt_ckmlmv013 TYPE TABLE OF ckmlmv013 WITH HEADER LINE,
        lt_mara     TYPE TABLE OF mara WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ckmlmv013 TYPE TABLE OF ty_ckmlmv013,
        ls_inx_ckmlmv013 TYPE ty_ckmlmv013.

  DATA: lt_ckmlmv003 LIKE gt_ckmlmv003_a OCCURS 0 WITH HEADER LINE,
        lt_ckmlmv003_tmp LIKE gt_ckmlmv003_a OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_ckmlmv003_2 OCCURS 0,
        bwkey      LIKE ckmlmv001-bwkey,
        matnr      LIKE ckmlmv001-matnr,
        aufnr      LIKE ckmlmv013-aufnr,
        verid_nd   LIKE ckmlmv001-verid_nd,
        meinh      LIKE ckmlmv003-meinh,
        out_menge  LIKE ckmlmv003-out_menge,
          kalnr_in LIKE ckmlmv003-kalnr_in,
        END OF lt_ckmlmv003_2.
  DATA: lt_ckmlmv003_2_tmp LIKE lt_ckmlmv003_2 OCCURS 0 WITH HEADER LINE.

  CONSTANTS: c_zckmlmv013_001(14) VALUE 'ZCKMLMV013_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zckmlmv013_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

  SELECT  b~bwkey b~matnr b~verid_nd
          "c~aufnr
          a~out_menge a~meinh
            a~kalnr_in
    INTO CORRESPONDING FIELDS OF TABLE lt_ckmlmv003_2
    FROM
      ( ckmlmv003 AS a
    INNER JOIN ckmlmv001 AS b
       ON b~kalnr = a~kalnr_bal )
*    INNER JOIN ckmlmv013 AS c
*       ON c~kalnr_proc = a~kalnr_in
*    INNER JOIN mara AS d
*       ON d~matnr = c~pmatn )
   WHERE a~mgtyp EQ '00001'
     AND a~gjahr EQ p_year
     AND a~perio EQ p_poper
     AND a~matnr IN s_matnr
     AND a~werks IN s_werks
  "AND d~mtart IN s_mtart
     AND b~btyp  =  'BF'
     AND b~bwkey = a~werks.
  "AND c~flg_wbwg = 'X'
  "AND c~autyp = '05' .

  CLEAR: lt_ckmlmv003_2_tmp, lt_ckmlmv003_2_tmp[].
  lt_ckmlmv003_2_tmp[] = lt_ckmlmv003_2[].
  SORT lt_ckmlmv003_2_tmp BY kalnr_in.
  DELETE ADJACENT DUPLICATES FROM lt_ckmlmv003_2_tmp COMPARING kalnr_in.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ckmlmv013[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ckmlmv013
    FROM (l_gentab)
     FOR ALL ENTRIES IN lt_ckmlmv003_2_tmp
   WHERE kalnr_proc = lt_ckmlmv003_2_tmp-kalnr_in
     AND flg_wbwg   = 'X'
     AND autyp      = '05'.

  CHECK NOT lt_inx_ckmlmv013[] IS INITIAL.

* 4. Get more archived data looping structure table
  LOOP AT lt_inx_ckmlmv013 INTO ls_inx_ckmlmv013.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'CO_ORDER'
        archivkey                 = ls_inx_ckmlmv013-archivekey
        offset                    = ls_inx_ckmlmv013-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_ckmlmv013, lt_ckmlmv013[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'CKMLMV013'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_ckmlmv013
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_ckmlmv013[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_ckmlmv013 INTO TABLE gt_ckmlmv013_a.
  ENDLOOP.

  SORT gt_ckmlmv013_a BY aufnr.
  DELETE ADJACENT DUPLICATES FROM gt_ckmlmv013_a COMPARING aufnr.

  CHECK NOT gt_ckmlmv013_a[] IS INITIAL.

  CLEAR: gt_ckmlmv013_a_tmp, gt_ckmlmv013_a_tmp[].
  gt_ckmlmv013_a_tmp[] = gt_ckmlmv013_a[].
  SORT gt_ckmlmv013_a_tmp BY pmatn.
  DELETE ADJACENT DUPLICATES FROM gt_ckmlmv013_a_tmp COMPARING pmatn.

* mara
  CLEAR: lt_mara, lt_mara[].
  SELECT *
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FOR ALL ENTRIES IN gt_ckmlmv013_a_tmp
   WHERE matnr = gt_ckmlmv013_a_tmp-pmatn
     AND mtart IN s_mtart.

  CHECK NOT lt_mara[] IS INITIAL.

  CLEAR: lt_ckmlmv003, lt_ckmlmv003[].
  LOOP AT lt_ckmlmv003_2.
    MOVE-CORRESPONDING lt_ckmlmv003_2 TO lt_ckmlmv003.

    CLEAR gt_ckmlmv013_a.
    READ TABLE gt_ckmlmv013_a WITH KEY kalnr_proc = lt_ckmlmv003_2-kalnr_in.
    CHECK sy-subrc = 0.

    CLEAR lt_mara.
    READ TABLE lt_mara WITH KEY matnr = gt_ckmlmv013_a-pmatn.
    CHECK sy-subrc = 0.

    APPEND lt_ckmlmv003. CLEAR lt_ckmlmv003.
  ENDLOOP.

  INSERT LINES OF lt_ckmlmv003 INTO TABLE pt_ckmlmv003.

ENDFORM.                    " ARCHIVE_READ_CKMLMV013
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTCOU103
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_103  text
*----------------------------------------------------------------------*
FORM archive_read_ztcou103 TABLES pt_103 STRUCTURE gt_103_a.

  TYPES: BEGIN OF ty_ztcou103,
         kokrs TYPE kokrs,
         bdatj TYPE bdatj,
         kalka TYPE ck_kalka,
         poper TYPE poper,
         compn TYPE idnrk,
         splnt TYPE werkq,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ztcou103.

  DATA: l_handle    TYPE sytabix,
        lt_ztcou103 TYPE TABLE OF ztcou103 WITH HEADER LINE,
        lt_marc     TYPE TABLE OF marc     WITH HEADER LINE,
        lt_marc_tmp TYPE TABLE OF marc     WITH HEADER LINE,
        lt_mast     TYPE TABLE OF mast     WITH HEADER LINE,
        lt_makt     TYPE TABLE OF makt     WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ztcou103 TYPE TABLE OF ty_ztcou103,
        lt_inx_ztcou103_tmp TYPE TABLE OF ty_ztcou103,
        ls_inx_ztcou103 TYPE ty_ztcou103.

  DATA: lt_103_a LIKE gt_103_a OCCURS 0 WITH HEADER LINE.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZTCOU103_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ztcou103[].
  SELECT DISTINCT compn splnt
     INTO CORRESPONDING FIELDS OF TABLE lt_inx_ztcou103
    FROM (l_gentab)
   WHERE kokrs = p_kokrs
     AND bdatj = p_year
     AND kalka = 'U1'
     AND poper = p_poper
     AND compn IN s_matnr.

  CHECK NOT lt_inx_ztcou103[] IS INITIAL.

  CLEAR lt_inx_ztcou103_tmp[].
  lt_inx_ztcou103_tmp[] = lt_inx_ztcou103[].
  SORT lt_inx_ztcou103_tmp BY compn splnt.
  DELETE ADJACENT DUPLICATES FROM lt_inx_ztcou103_tmp COMPARING compn splnt.

  CLEAR: lt_marc, lt_marc[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_marc
    FROM marc
    FOR ALL ENTRIES IN lt_inx_ztcou103_tmp
   WHERE matnr = lt_inx_ztcou103_tmp-compn
     AND werks = lt_inx_ztcou103_tmp-splnt
     AND beskz = 'F'.

  LOOP AT lt_inx_ztcou103 INTO ls_inx_ztcou103.
    CLEAR lt_marc.
    READ TABLE lt_marc WITH KEY matnr = ls_inx_ztcou103-compn
                                werks = ls_inx_ztcou103-splnt.
    IF sy-subrc <> 0.
      DELETE lt_inx_ztcou103.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  CLEAR: gt_ztcou103_a, gt_ztcou103_a[].
  LOOP AT lt_inx_ztcou103 INTO ls_inx_ztcou103.
    MOVE-CORRESPONDING ls_inx_ztcou103 TO gt_ztcou103_a.

    APPEND gt_ztcou103_a. CLEAR gt_ztcou103_a.
  ENDLOOP.

** 4. Get more archived data looping structure table
*  CLEAR: gt_ztcou103_a, gt_ztcou103_a[].
*  LOOP AT lt_inx_ztcou103 INTO ls_inx_ztcou103.
**  4.1 Read information from archivekey & offset
*    CLEAR l_handle.
*    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
*      EXPORTING
*        object                    = 'ZTCOU103'
*        archivkey                 = ls_inx_ztcou103-archivekey
*        offset                    = ls_inx_ztcou103-archiveofs
*      IMPORTING
*        archive_handle            = l_handle
*      EXCEPTIONS
*        no_record_found           = 1
*        file_io_error             = 2
*        internal_error            = 3
*        open_error                = 4
*        cancelled_by_user         = 5
*        archivelink_error         = 6
*        object_not_found          = 7
*        filename_creation_failure = 8
*        file_already_open         = 9
*        not_authorized            = 10
*        file_not_found            = 11
*        error_message             = 12
*        OTHERS                    = 13.
*
*    CHECK sy-subrc = 0.
*
**  4.2 Read table from information
*    CLEAR: lt_ztcou103, lt_ztcou103[].
*    CALL FUNCTION 'ARCHIVE_GET_TABLE'
*      EXPORTING
*        archive_handle          = l_handle
*        record_structure        = 'ZTCOU103'
*        all_records_of_object   = 'X'
*      TABLES
*        table                   = lt_ztcou103
*      EXCEPTIONS
*        end_of_object           = 1
*        internal_error          = 2
*        wrong_access_to_archive = 3
*        OTHERS                  = 4.
*
*    CHECK sy-subrc = 0 AND NOT lt_ztcou103[] IS INITIAL.
*
** 5. Append archived data table to finally interal table
*    INSERT LINES OF lt_ztcou103 INTO TABLE gt_ztcou103_a.
*  ENDLOOP.

*  SORT gt_ztcou103_a BY kokrs bdatj kalka poper artnr ver werks indx.
*  DELETE ADJACENT DUPLICATES FROM gt_ztcou103_a
*         COMPARING kokrs bdatj kalka poper artnr ver werks indx.

* marc
  CLEAR: gt_ztcou103_a_tmp, gt_ztcou103_a_tmp[].
  gt_ztcou103_a_tmp[] = gt_ztcou103_a[].
  SORT gt_ztcou103_a_tmp BY compn splnt.
  DELETE ADJACENT DUPLICATES FROM gt_ztcou103_a_tmp COMPARING compn splnt.

  CLEAR: lt_marc, lt_marc[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_marc
    FROM marc
    FOR ALL ENTRIES IN gt_ztcou103_a_tmp
   WHERE matnr = gt_ztcou103_a_tmp-compn
     AND werks = gt_ztcou103_a_tmp-splnt
     AND beskz = 'F'.                    "MUST be 'F'

* mast
  CLEAR: lt_marc_tmp, lt_marc_tmp.
  lt_marc_tmp[] = lt_marc[].
  SORT lt_marc_tmp BY matnr werks.
  DELETE ADJACENT DUPLICATES FROM lt_marc_tmp COMPARING matnr werks.

  CLEAR: lt_mast, lt_mast[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_mast
    FROM mast
    FOR ALL ENTRIES IN lt_marc_tmp
   WHERE matnr = lt_marc_tmp-matnr
     AND werks = lt_marc_tmp-werks
     AND stlan = '2'.

  CHECK NOT lt_mast[] IS INITIAL.

* makt
  CLEAR: lt_marc_tmp, lt_marc_tmp.
  lt_marc_tmp[] = lt_marc[].
  SORT lt_marc_tmp BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_marc_tmp COMPARING matnr.

  CLEAR: lt_makt, lt_makt[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_makt
    FROM makt
    FOR ALL ENTRIES IN lt_marc_tmp
   WHERE matnr = lt_marc_tmp-matnr
     AND spras = sy-langu.

  CHECK NOT lt_makt[] IS INITIAL.

  CLEAR: lt_103_a, lt_103_a[].
  LOOP AT gt_ztcou103_a.
    "MOVE-CORRESPONDING gt_ztcou103_a TO lt_103_a.
    lt_103_a-matnr = gt_ztcou103_a-compn.
    lt_103_a-werks = gt_ztcou103_a-splnt.

    CLEAR lt_marc.
    READ TABLE lt_marc WITH KEY matnr = gt_ztcou103_a-compn
                                werks = gt_ztcou103_a-splnt.
    CHECK sy-subrc = 0.

    CLEAR lt_mast.
    READ TABLE lt_mast WITH KEY matnr = lt_marc-matnr
                                werks = lt_marc-werks.
    CHECK sy-subrc = 0.

    CLEAR lt_makt.
    READ TABLE lt_makt WITH KEY matnr = lt_marc-matnr
                                spras = sy-langu.
    CHECK sy-subrc = 0.

    lt_103_a-maktx = lt_makt-maktx.

    APPEND lt_103_a.  CLEAR lt_103_a.
  ENDLOOP.

  CHECK NOT lt_103_a[] IS INITIAL.

  INSERT LINES OF lt_103_a INTO TABLE pt_103.

ENDFORM.                    " ARCHIVE_READ_ZTCOU103
