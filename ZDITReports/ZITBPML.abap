*----------------------------------------------------------------------
* Program ID        : ZITBPML
* Title             : Create & Manage APM/BPML Data
* Created on        : 07/12/2013
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : BPML Master Data
*----------------------------------------------------------------------

REPORT  zitbpml MESSAGE-ID zmfi.

TABLES: ztitbpml, ztitbpmlh, zthrappusge, bmenuname,
        ztitdevc, ztitbph,
        ttree,
        tadir, tdevc, *tstc, agr_tcodes, agr_1251, agr_users, tbtcp.
* Define Variables and Internal Tables for ALV
TYPE-POOLS: slis.
TYPE-POOLS: truxs.

INCLUDE <icon>.
INCLUDE <symbol>.
CONSTANTS: gc_formname_top_of_page TYPE slis_formname
                                   VALUE 'TOP_OF_PAGE',
           gc_var_save       TYPE c VALUE  'A',
           gc_pf_status_set  TYPE slis_formname VALUE 'PF_STATUS_SET',
           gc_user_command   TYPE slis_formname VALUE 'USER_COMMAND',
           gc_tvers          TYPE ck_tvers      VALUE '01'.

DATA:
      gt_specialcol        TYPE slis_t_specialcol_alv,
      gs_specialcol        TYPE slis_specialcol_alv.

DATA: gv_default(1)  TYPE c,
      gs_variant  LIKE disvariant,
      gs_variant1 LIKE disvariant,
      gv_repid    LIKE sy-repid.

*** for ALV Grid 100
DATA : gt_exclude   TYPE ui_functions,
       container    TYPE scrfname VALUE 'G_CUSTOM_CONTAINER',
       gs_fcat      TYPE lvc_s_fcat,
       gt_fcat      TYPE lvc_t_fcat,
       gs_layo      TYPE lvc_s_layo,
       gs_sort      TYPE lvc_s_sort,
       gt_sort      TYPE lvc_t_sort.

DATA : g_custom_container  TYPE REF TO cl_gui_custom_container,
       g_grid              TYPE REF TO cl_gui_alv_grid.

DATA: fcode TYPE TABLE OF sy-ucomm.

*DATA: BEGIN OF it_level OCCURS 0,
*      l1 LIKE ztitbpml-l1,
*      l2 LIKE ztitbpml-l2,
*      l2TEXT LIKE DD07T-DDTEXT,
*      L3 LIKE ztitbpml-L3,
*      L3TEXT LIKE DD07T-DDTEXT,
*      l4 LIKE ztitbpml-l4,
*      l4f LIKE ztitbpml-l4f,
*      l4t LIKE ztitbpml-l4t,
*      l5 LIKE ztitbpml-l5,
*      END OF it_level.

DATA: BEGIN OF it_ztitbph OCCURS 0.
        INCLUDE STRUCTURE ztitbph.
DATA:    menudesc   TYPE hier_text,
      END OF it_ztitbph.
DATA: BEGIN OF g_code_l5t OCCURS 0,
        menuid TYPE bmenextkey,
        tcode  TYPE sobj_name,
        l5t    LIKE ztitbpml-l5t,
      END OF g_code_l5t.

** ALV 200
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : it_fieldname    TYPE slis_t_fieldcat_alv,
       wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldname,  "IT_FIELDCAT.
       wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
       wa_variant TYPE disvariant.      "for parameter IS_VARIANT


DATA:   wa_custom_control TYPE  scrfname VALUE 'ALV_CONTAINER',
        g_docking_container TYPE REF TO cl_gui_docking_container,
        g_grid_0200      TYPE REF TO cl_gui_alv_grid.

DATA:   w_cnt   TYPE i.

DATA : ok_code      TYPE sy-ucomm,
       save_ok_code TYPE sy-ucomm.

* Define internal tables &sstructures for Possible Entry
DATA : gs_values TYPE seahlpres,
       gt_fields TYPE TABLE OF dfies WITH HEADER LINE,
       gt_values TYPE TABLE OF seahlpres WITH HEADER LINE,
       gs_fields TYPE dfies,
       ls_f4     TYPE ddshretval,
       ls_modi   TYPE lvc_s_modi.


* define fields and field-symbols for data-update
FIELD-SYMBOLS : <f4tab> TYPE lvc_t_modi.

* reference to custom container: neccessary to bind ALV Control
CLASS cl_gui_resources DEFINITION LOAD.
*CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA: gt_row     TYPE lvc_t_row,
      gs_row     TYPE lvc_s_row,
      gt_roid    TYPE lvc_t_roid.

* define internal table for BDC
DATA: gt_bdc TYPE TABLE OF bdcdata    WITH HEADER LINE,
      gt_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      gs_opt LIKE ctu_params.

* for possible entry
DATA: BEGIN OF dynpfields OCCURS 3.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpfields.

DATA: dyname         TYPE progname,
      dynumb         TYPE sychar04,
      exc_exctab     TYPE slis_t_extab,
      popup_fieldcat TYPE slis_t_fieldcat_alv,
      f              TYPE slis_fieldcat_alv,
      selfield       TYPE slis_selfield,
      exitfield,
      color_active(3)  VALUE 'C50',
      tabix LIKE sy-tabix.

*check change fields
TYPES: BEGIN OF ty_chg,
        apm           LIKE ztitbpml-apm,
        tctyp         LIKE ztitbpml-tctyp,
        rstat         LIKE ztitbpml-rstat,
        tcfrq         LIKE ztitbpml-tcfrq,
        zsstvind      LIKE ztitbpml-zsstvind,

        l1            LIKE ztitbpml-l1,
        l2            LIKE ztitbpml-l2,
        l3            LIKE ztitbpml-l3,
        l4            LIKE ztitbpml-l4,
        devclass      LIKE ztitbpml-devclass,
        pgmna         LIKE ztitbpml-pgmna,
        l4t           LIKE ztitbpml-l4t,
        l5t           LIKE ztitbpml-l5t,
        skey          LIKE ztitbpml-skey,
        role          LIKE ztitbpml-role,
       END OF ty_chg.

* possible entry for reason code
TYPES: BEGIN OF ty_ztcoum02,
         rgrp2 TYPE zrgrp2,
         text  TYPE zrtext,
       END OF ty_ztcoum02.

TYPES: BEGIN OF ty_rsn,
         kzust TYPE kzust,
         text  TYPE zrtext,
       END OF ty_rsn.

TYPES: BEGIN OF ty_tadir,
        obj_name  TYPE sobj_name, " obj_name,
        devclass  TYPE devclass,
        text      TYPE hier_text,
*NCLUDE  TYPE tadir.
        area_name(50).
TYPES: END OF ty_tadir.

*DATA gt_tdevct LIKE tdevct   OCCURS 0 WITH HEADER LINE.
DATA gt_tadir  TYPE ty_tadir OCCURS 0 WITH HEADER LINE.

DATA: gt_ztcoum02 TYPE TABLE OF ty_ztcoum02 WITH HEADER LINE,
      gt_rsn      TYPE TABLE OF ty_rsn      WITH HEADER LINE.

DATA: stable        TYPE lvc_s_stbl.

DEFINE __set_refresh_mode.
  stable-row = &1.
  stable-col = &1.
END-OF-DEFINITION.

TYPES: BEGIN OF ty_out.
TYPES   area_name(50).
        INCLUDE STRUCTURE ztitbpml.
TYPES   celltab TYPE lvc_t_styl.
TYPES   tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

DATA: gt_out TYPE TABLE OF ty_out WITH HEADER LINE.
DATA: gw_outtab TYPE TABLE OF ty_out WITH HEADER LINE.

DATA: BEGIN OF gt_ref OCCURS 0,
        tcode    TYPE tcode,
        l4f      TYPE zitbpl4f,
        cnt      TYPE i,
      END OF gt_ref.

DATA: BEGIN OF gt_1251 OCCURS 0,
        agr_name LIKE agr_1251-agr_name,
        low      LIKE agr_1251-low,
        high     LIKE agr_1251-high,
      END OF gt_1251.

DATA: gt_uptab  TYPE TABLE OF ztitbpml.
DATA: gt_cur    TYPE TABLE OF ztitbpml WITH HEADER LINE,
      gt_prv    TYPE TABLE OF ztitbpml WITH HEADER LINE.

DATA: it_raw TYPE truxs_t_text_data.

*dev.class info.
DATA: BEGIN OF gt_tdevc OCCURS 0,
        devclass  LIKE tdevc-devclass,
        ctext     LIKE tdevc-ctext,
      END OF gt_tdevc.
DATA: gt_tdevc2   TYPE ztitdevc OCCURS 0 WITH HEADER LINE.
DATA: gt_ttree    LIKE ttree    OCCURS 0 WITH HEADER LINE.

DATA save_ok LIKE sy-ucomm.

TYPE-POOLS : slis,
             cxtab.
TYPE-POOLS: truxs.

DATA: gv_dest TYPE rfcdest.

TYPES ddshretval_table TYPE TABLE OF ddshretval.

****************************** Macros *********************************
DEFINE __focus.
  call method cl_gui_control=>set_focus
    exporting
      control = &1.
END-OF-DEFINITION.
****************************** constants ******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
*---------------------------------------------------------------------*
DATA: bezei       TYPE bezei,
      gv_cnt      TYPE i,
      info(80),
      gv_index    TYPE i.
DATA  flag_data_changed.

*************************** Selection Screen **************************

PARAMETERS: p_comp   LIKE ztitbpml-comp OBLIGATORY MEMORY ID buk.
PARAMETERS: p_objtyp TYPE trobjtype     DEFAULT 'TRAN' NO-DISPLAY.
SELECT-OPTIONS: s_objs  FOR tadir-obj_name.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-001.


PARAMETERS : p_disp TYPE xfeld RADIOBUTTON GROUP meth DEFAULT 'X',
             p_usg  TYPE xfeld RADIOBUTTON GROUP meth,
*            P_REV  TYPE XFELD RADIOBUTTON GROUP METH,
             p_ar   TYPE xfeld RADIOBUTTON GROUP meth.
SELECT-OPTIONS: s_area  FOR bmenuname-id.
PARAMETERS : p_gen  TYPE xfeld RADIOBUTTON GROUP meth.
PARAMETERS : p_del  TYPE xfeld NO-DISPLAY. " RADIOBUTTON GROUP meth.
PARAMETERS p_up  TYPE xfeld NO-DISPLAY.
PARAMETERS   p_test  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK b4.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE text-011.
SELECT-OPTIONS s_l1  FOR ztitbpml-l1.
SELECT-OPTIONS s_l2  FOR ztitbpml-l2.
SELECT-OPTIONS s_apm FOR ztitbpml-apm.

*SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
*SELECTION-SCREEN END OF BLOCK b5.
SELECTION-SCREEN END OF BLOCK b6.

SELECTION-SCREEN BEGIN OF BLOCK b7 WITH FRAME TITLE text-012.
SELECT-OPTIONS: s_roles FOR agr_tcodes-agr_name.
SELECT-OPTIONS: s_users FOR agr_users-uname.
PARAMETERS: p_file TYPE  rlgrap-filename NO-DISPLAY.
SELECT-OPTIONS: p_usgdt   FOR sy-datum OBLIGATORY.
*PARAMETERS: p_devc AS CHECKBOX.
PARAMETERS: p_rfc  TYPE c NO-DISPLAY.
*PARAMETERS: p_max TYPE i DEFAULT '3000'.
SELECTION-SCREEN END OF BLOCK b7.

** At selection screen
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  CALL FUNCTION 'F4_FILENAME'
*    EXPORTING
*      field_name = 'P_FILE'
*    IMPORTING
*      file_name  = p_file.

*---------------------------------------------------------------------*
* Define local class
*---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    TYPES BEGIN OF ztitbpml_key.
    TYPES   comp TYPE ztitbpco.
    TYPES   id TYPE id.
    TYPES END OF ztitbpml_key.

    TYPES: ztitbpml_keys TYPE STANDARD TABLE OF ztitbpml_key,
           ztitbpml_table TYPE STANDARD TABLE OF ztitbpml.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,
                       get_deleted_rows
             EXPORTING
                       deleted_rows TYPE ztitbpml_table,

      refresh_delta_tables,

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row
                       e_column
                       es_row_no.


  PRIVATE SECTION.
    DATA deleted_rows TYPE STANDARD TABLE OF ztitbpml.

* This flag is set if any error occured in one of the
* following methods:
    DATA: error_in_data TYPE c.
    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*---------------------------------------------------------------------*
* Implementation local class
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.

* remember deleted lines for saving
    CALL METHOD update_delta_tables( er_data_changed ).

    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

  METHOD get_deleted_rows.
    deleted_rows = me->deleted_rows.
  ENDMETHOD.                    "GET_DELETED_ROWS

  METHOD refresh_delta_tables.
    CLEAR me->deleted_rows[].
  ENDMETHOD.                    "REFRESH_DELTA_TABLES

  METHOD update_delta_tables.
    DATA: l_del_row TYPE lvc_s_moce,
          ls_key TYPE ztitbpml_key,
          ls_ztitbpml TYPE ztitbpml,
          ls_outtab LIKE LINE OF gt_out.

    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
      READ TABLE gt_out INTO ls_outtab INDEX l_del_row-row_id.
      IF sy-subrc NE 0.
        MESSAGE i000(0k) WITH text-e01. "Internal error
      ELSE.
        MOVE-CORRESPONDING ls_outtab TO ls_ztitbpml.
        APPEND ls_ztitbpml TO deleted_rows.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "UPDATE_DELTA_TABLES

* Double Click
  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_data_changed

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

*&--------------------------------------------------------------------*
INITIALIZATION.
*&--------------------------------------------------------------------*
* s_objs-low = 'Z*'.
* s_objs-sign   = 'I'.
* s_objs-option = 'EQ'.
* append s_objs.
  PERFORM init_data.
  DATA: h_dontpanic   LIKE sy-datlo.

  DATA: lv_date LIKE sy-datum.
  REFRESH p_usgdt.
  p_usgdt-high = sy-datum.
  CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
    EXPORTING
      months  = -13
      olddate = p_usgdt-high
    IMPORTING
      newdate = p_usgdt-low.
  p_usgdt-sign = 'I'.
  p_usgdt-option = 'BT'.
  APPEND p_usgdt.

*&--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*&--------------------------------------------------------------------*
  LOOP AT SCREEN.
    IF screen-name = 'P_DEL' OR screen-name = 'P_GENS'.
      GET PARAMETER ID 'DONTPANIC' FIELD h_dontpanic.
      IF h_dontpanic = sy-datlo.
        screen-input = '1'.
      ELSE.
*       screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
*&--------------------------------------------------------------------*
  PERFORM alv_variant_f4 CHANGING p_vari.

*&--------------------------------------------------------------------*
START-OF-SELECTION.
*&--------------------------------------------------------------------*
* original table
  SELECT * INTO TABLE gt_prv FROM ztitbpml WHERE comp = p_comp.
  SORT gt_prv BY l4f tcode.
  PERFORM read_tdevc.

  IF p_usgdt-low IS INITIAL.
    MESSAGE s000 WITH 'Usage till date is Mandatory'.
    STOP.
  ENDIF.

  IF p_ar = 'X'.
*    IF s_area[] IS INITIAL.
*      MESSAGE s000 WITH 'Please put Area Menu to import !'
*      DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.
    PERFORM generate_bpml.
    IF ( p_test = 'X' AND sy-batch = ' ' )
       OR s_area[] IS NOT INITIAL.
      CALL SCREEN 100.
    ELSE.
      PERFORM data_save_batch.
    ENDIF.

  ELSEIF p_gen = 'X' .
    PERFORM generate_bpml.

    IF p_test = 'X' AND sy-batch = ' '.
      CALL SCREEN 100.
    ELSE.
      PERFORM data_save_batch.
    ENDIF.

  ELSEIF p_disp = 'X' .
    CHECK sy-batch = ' '.
    PERFORM select_data_and_init_style.
    CALL SCREEN 100.

  ELSEIF p_usg = 'X' .
    PERFORM update_usage_data.
    MESSAGE s000 WITH 'Updated...'.

  ELSEIF p_del = 'X'.
    IF p_test = space.
      DELETE FROM ztitbpmlh
        WHERE ( comp = p_comp OR comp = space )
          AND tcode IN s_objs.
      DELETE FROM ztitbpml
        WHERE ( comp = p_comp OR comp = space )
          AND tcode IN s_objs.

      MESSAGE s000 WITH 'Deleted...' sy-dbcnt.
    ENDIF.

  ELSEIF p_up = 'X'.
    PERFORM upload_excel.

  ENDIF.

*&--------------------------------------------------------------------*
END-OF-SELECTION.
*&--------------------------------------------------------------------*

*&--------------------------------------------------------------------*
*&      Form  get_call_tcode
*&--------------------------------------------------------------------*
FORM get_call_tcode USING    i_tcode
                    CHANGING o_tcode.
  DATA: param_beg TYPE i.

  TABLES: tstcp.
*-----  LSEUKF01
  SELECT SINGLE * FROM tstcp WHERE tcode = i_tcode.

  IF tstcp-param(1) = '/'.
*    rsstcd-st_tcode = 'X'.
*    rsstcd-st_prog  = space.
*    if tstcp-param+1(1) = '*'.
*      rsstcd-st_skip_1 = 'X'.
*    else.
*      clear rsstcd-st_skip_1.
*    endif.
    IF tstcp-param CA ' '. ENDIF.
    param_beg = sy-fdpos + 1.
    SUBTRACT 2 FROM sy-fdpos.
    IF sy-fdpos GT 0.
      o_tcode = tstcp-param+2(sy-fdpos).
    ENDIF.
  ENDIF.

ENDFORM.                    " get_call_tcode
*&--------------------------------------------------------------------*
*&
*&--------------------------------------------------------------------*
FORM get_tcode_info_rfc.

  CHECK gv_dest <> space.

  CALL FUNCTION 'Z_IT_GET_TCODE_INFO'
    DESTINATION gv_dest
    EXPORTING
      i_tcode      = gt_cur-tcode
    IMPORTING
      o_devclass   = gt_cur-devclass
      o_ctext      = gt_cur-l4t
      o_pgmna      = gt_cur-pgmna
      o_ttext      = gt_cur-ttext
      o_call_tcode = gt_cur-call_tcode
      o_tctyp      = gt_cur-tctyp
      o_taskv      = gt_cur-taskv.

*function z_it_get_tcode_info.
**"---------------------------------------------------------------------
**"*"Local Interface:
**"  IMPORTING
**"     VALUE(I_TCODE) TYPE  TCODE
**"  EXPORTING
**"     VALUE(O_DEVCLASS) TYPE  DEVCLASS
**"     VALUE(O_CTEXT) TYPE  AS4TEXTOLD
**"     VALUE(O_PGMNA) TYPE  PROGRAM_ID
**"     VALUE(O_TTEXT) TYPE  TTEXT_STCT
**"     VALUE(O_CALL_TCODE) TYPE  TCODE
**"     VALUE(O_TCTYP) TYPE  ZITTCTYP
**"---------------------------------------------------------------------
**LSEUKTOP
*  data: hex_tra type x value '00',             " Transaktion         T
*        hex_men type x value '01',             " Area menu           -
*        hex_par type x value '02',             " Parametertrans.     P
*        hex_rep type x value '80',             " Report              R
*        hex_rpv type x value '10',             " Report  w Variante  V
*        hex_obj type x value '08',             " Objekttransaktionen
*        hex_chk type x value '04',             " mit Prüfobjekt
*        hex_enq type x value '20'.             " Gesperrt über SM01
*
*  data: l_tcode type tcode,
*        param_beg type i,
*        l_cinfo type syhex01.
*
*  tables: tstcp.
*  l_tcode = i_tcode.
*  condense l_tcode no-gaps.
*
*  select single devclass into o_devclass from tadir
*     where pgmid = 'R3TR'
*       and object = 'TRAN'
*       and obj_name = l_tcode.
*
*  select single ctext into o_ctext from tdevc
*    where devclass = o_devclass.
*
*  select single pgmna cinfo into (o_pgmna, l_cinfo)
*     from tstc
*     where tcode = l_tcode.
*
*  select single ttext into o_ttext
*     from tstct
*     where sprsl = sy-langu
*       and tcode = l_tcode.
*
*  if l_cinfo o hex_rep.             " Report
*    o_tctyp = 'R'.
*  elseif l_cinfo o hex_par.
*    o_tctyp = 'P'.              " Trans w. param inc. view maintenance
*  elseif l_cinfo o hex_men.         " Menü
*    o_tctyp = ' '.
*  else.                                " Transaktion
*    o_tctyp = 'T'.
*  endif.
*
*
*  if o_pgmna is initial.
**-----  LSEUKF01
*    select single * from tstcp where tcode = l_tcode.
*
*    if tstcp-param(1) = '/'.
*      if tstcp-param ca ' '. endif.
*      param_beg = sy-fdpos + 1.
*      subtract 2 from sy-fdpos.
*      if sy-fdpos gt 0.
*        o_call_tcode = tstcp-param+2(sy-fdpos).
*      endif.
*    endif.
*
*    select single pgmna into o_pgmna
*       from tstc
*       where tcode = o_call_tcode.
*
*  endif.
*
*
*endfunction.
ENDFORM.                    "GET_TCODE_INFO_RFC
*&--------------------------------------------------------------------*
*&      Form  make_L4F
*&--------------------------------------------------------------------*
FORM make_l4f.

  IF gt_cur-l4 <> space.
    CONCATENATE 'Z' gt_cur-l1(1) gt_cur-l2 gt_cur-l3 gt_cur-l4
           INTO gt_cur-l4f.
  ELSEIF gt_cur-l3 <> space.
    CONCATENATE 'Z' gt_cur-l1(1) gt_cur-l2 gt_cur-l3
           INTO gt_cur-l4f.

  ELSEIF gt_cur-l2 <> space.
    CONCATENATE 'Z' gt_cur-l1(1) gt_cur-l2
           INTO gt_cur-l4f.
  ELSEIF gt_cur-l1 <> space.
    MOVE gt_cur-l1(1)
           TO gt_cur-l4f.
  ENDIF.

ENDFORM.                    "make_l4f

*&--------------------------------------------------------------------*
*&      Form  fill_to_itab
*&--------------------------------------------------------------------*
*FORM fill_to_itab.
*  DATA: lv_outtab TYPE TABLE OF ty_out WITH HEADER LINE.
*  DATA: id_5 LIKE sy-tabix,
*        id_6 LIKE sy-tabix,
*        ix   LIKE sy-tabix.
*
*  CLEAR: id_5, id_6.
*  LOOP AT gt_uptab INTO gt_cur.
*    ix = sy-tabix.
**    IF P_DEVC = ' '.
*
*    IF gt_cur-l1 <> wa_prv-l1.
*      CLEAR: id_5, id_6.
*    ELSE.
*      IF gt_cur-l2 <> wa_prv-l2.
*        CLEAR: id_5, id_6.
*      ELSE.
*        IF gt_cur-l3 <> wa_prv-l3.
*          CLEAR: id_5, id_6.
*        ELSE.
*          IF gt_cur-l4 <> wa_prv-l4.
*            CLEAR: id_5, id_6.
*          ELSE.
*            IF gt_cur-l5t <> space
*            AND gt_cur-l5t <> wa_prv-l5t.
*              id_5 = id_5 + 1.
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    IF id_5 > 0.
*      gt_cur-l5 = id_5.
*    ENDIF.
*
**    ENDIF. "    IF P_DEVC = ' '.
*
*
*    IF gt_cur-tcode <> space.
*      PERFORM get_tcode_info.
*    ENDIF.
*
*    IF gt_cur-tcode <> space AND gt_cur-devclass = space.
*      gt_cur-tctyp = 'M'. "manual
*    ENDIF.
*
*    IF gt_cur-tctyp = space
*    AND gt_cur-tcode = space AND gt_cur-task <> space.
*      gt_cur-tctyp = 'M'.
*    ENDIF.
*
*
*    IF p_renu = 'X'. gt_cur-id = ix. ENDIF.
*
**    gt_cur-cnt = 1.
*
*    PERFORM make_l4f.
*    gt_cur-l2t = gt_cur-l2.    gt_cur-l3t = gt_cur-l3.
*
*    MOVE-CORRESPONDING gt_cur TO lv_outtab .
*    APPEND lv_outtab TO gt_out.
*    wa_prv = gt_cur.
*  ENDLOOP.
*
*ENDFORM.                    " fill_to_itab
*&--------------------------------------------------------------------*
*&      Form  READ_TDEVC
*&--------------------------------------------------------------------*
FORM read_tdevc .

  SELECT tdevc~devclass tdevct~ctext INTO TABLE gt_tdevc
    FROM tdevc INNER JOIN tdevct
      ON tdevc~devclass = tdevct~devclass
    WHERE spras = sy-langu
      AND tdevc~devclass LIKE 'Z%'.

  SORT gt_tdevc BY devclass.


  SELECT * INTO TABLE gt_tdevc2 FROM ztitdevc.
  SORT gt_tdevc2 BY devclass.

  REFRESH it_ztitbph.
  SELECT *
    FROM ztitbph.
    CLEAR it_ztitbph.
    MOVE-CORRESPONDING ztitbph TO it_ztitbph.
    SELECT SINGLE text INTO it_ztitbph-menudesc FROM ttreet
           WHERE id = ztitbph-l4f AND spras = sy-langu.
    APPEND it_ztitbph.
  ENDSELECT.
  SORT it_ztitbph BY l4f.


  SELECT agr_name low high INTO TABLE gt_1251
     FROM agr_1251
     WHERE agr_name LIKE 'Z%'
       AND agr_name IN s_roles
       AND object   EQ   'S_TCODE'.


ENDFORM.                    " READ_TDEVC
*----------------------------------------------------------*
*       FORM USER_COMMAND                                  *
*----------------------------------------------------------*
*       --> R_UCOMM                                        *
*       --> RS_SELFIELD                                    *
*----------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.

** Any Changes you have entered on screen will now be stored within
** the original internal table which the ALV was build from (it_ekko)

* Check function code
  CASE r_ucomm.
    WHEN '&DATA_SAVE'. "or what even event you want
*      loop at ititbpml. "to wa_ekko.
*        process each line of table including new values
*      ENDLOOP.

    WHEN '&IC1'.                       "Pick
      READ TABLE gt_out INDEX rs_selfield-tabindex INTO gw_outtab.
      IF sy-subrc = 0.
        RANGES: r_tcode1 FOR tstca-tcode.
        r_tcode1-option = 'EQ'.
        r_tcode1-sign = 'I'.
        r_tcode1-low = gw_outtab-tcode.
        APPEND r_tcode1.
        SUBMIT rsusr070
                WITH tcode1 IN r_tcode1
                WITH s_roles = 'X'
                WITH c_roles = ' '
                AND RETURN.
      ENDIF.

  ENDCASE.
ENDFORM.                    "user_command
*--------------------------------------------------------------------*
*       MODULE PAI INPUT                                             *
*--------------------------------------------------------------------*
MODULE pai INPUT.

  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      PERFORM exit_program.
    WHEN 'SAVE'.
      PERFORM data_save_alv.
    WHEN 'SWITCH'.
      PERFORM switch_edit_mode.
      __focus g_grid.
    WHEN 'DISPLAY'.
      PERFORM display_hierarchy.
    WHEN 'EDIT'.
      PERFORM edit_hierarchy.
  ENDCASE.

ENDMODULE.                    "pai INPUT
*--------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                            *
*--------------------------------------------------------------------*
FORM exit_program.
  LEAVE TO SCREEN 0.
*  leave PROGRAM.
ENDFORM.                    "exit_program
*&--------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&--------------------------------------------------------------------*
FORM switch_edit_mode.
  DATA answer.
  IF g_grid->is_ready_for_input( ) EQ 0.
    CLEAR flag_data_changed.
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
    SET PF-STATUS 'MAIN100'.
    PERFORM info_text_set USING true.
  ELSE.
    IF flag_data_changed EQ true.
      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
        EXPORTING
          textline1     = 'Data has not been saved yet.'
          textline2     = 'Do you want to continue anyway? '
          titel         = 'Confirmation'
          defaultoption = 'N'
        IMPORTING
          answer        = answer.
      CHECK answer EQ 'J'.
    ENDIF.
    CLEAR flag_data_changed.
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.
    SET PF-STATUS '100' EXCLUDING 'SAVE'.
    PERFORM info_text_set USING false.
  ENDIF.
ENDFORM.                               " SWITCH_EDIT_MODE
*&--------------------------------------------------------------------*
*&      Form  SELECT_DATA_AND_INIT_STYLE
*&--------------------------------------------------------------------*
FORM select_data_and_init_style.

  DATA: lv_outtab TYPE ty_out.
  DATA: lt_bpml    TYPE TABLE OF ztitbpml WITH HEADER LINE,
        lt_celltab TYPE lvc_t_styl,
        l_index TYPE i.

  SELECT * FROM ztitbpml INTO TABLE lt_bpml " UP TO p_max ROWS
      WHERE comp = p_comp
        AND l1    IN s_l1
        AND l2    IN s_l2
        AND apm   IN s_apm
        AND tcode IN s_objs
        AND l4f   IN s_area.


  LOOP AT lt_bpml.
*   get usage
    IF p_usg = 'X'.
      PERFORM get_usage USING lt_bpml.
    ENDIF.

    MOVE-CORRESPONDING lt_bpml TO lv_outtab.

    READ TABLE it_ztitbph WITH KEY l4f = lt_bpml-l4f
         BINARY SEARCH.
    IF sy-subrc = 0.
      lv_outtab-skey = it_ztitbph-skey.
    ENDIF.

    APPEND lv_outtab TO gt_out.
  ENDLOOP.

ENDFORM.                               " SELECT_DATA_AND_INIT_STYLE
*&--------------------------------------------------------------------*
*&      Form  DATA_SAVE_ALV
*&-------------------------------------------------------------------*
FORM data_save_alv.
  DATA: ls_outtab LIKE gt_out,
        ls_bpml   LIKE ztitbpml.
  DATA: lt_chg    TYPE TABLE OF ztitbpml WITH HEADER LINE.

  DATA: ls_chg1   TYPE ty_chg,
        ls_chg2   TYPE ty_chg,
        l_idx     LIKE sy-tabix.

  REFRESH: lt_chg.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_row
      et_row_no     = gt_roid.

  LOOP AT gt_row INTO gs_row.
    READ TABLE gt_out INDEX gs_row-index INTO ls_outtab.
    IF sy-subrc = 0.

      READ TABLE gt_prv WITH KEY l4f   = ls_outtab-l4f
                                 tcode = ls_outtab-tcode BINARY SEARCH.
      l_idx = sy-tabix.
*--- check changes.
      MOVE-CORRESPONDING ls_outtab TO ls_chg1.
      MOVE-CORRESPONDING gt_prv    TO ls_chg2.
      IF ls_chg1 <> ls_chg2.
        "append to change itab.
        MOVE-CORRESPONDING ls_outtab TO ls_bpml.
        APPEND ls_bpml TO lt_chg.

        "change back to original itab
        MOVE-CORRESPONDING ls_chg1 TO gt_prv.
        MODIFY gt_prv INDEX l_idx.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_chg LINES l_idx.
  CHECK l_idx > 0.

  DATA: lv_uzeit LIKE sy-uzeit.
  lv_uzeit = sy-uzeit.
  LOOP AT lt_chg INTO ls_bpml.
*    ls_bpml-mandt = sy-mandt.
    ls_bpml-comp  = p_comp.
    ls_bpml-aedat = sy-datum..
    ls_bpml-aetim = lv_uzeit.
    ls_bpml-aenam = sy-uname.
    MODIFY ztitbpml FROM ls_bpml.

    MOVE-CORRESPONDING ls_bpml TO ztitbpmlh.
    CONVERT DATE sy-datum TIME sy-uzeit
     INTO TIME STAMP ztitbpmlh-timestamp TIME ZONE 'UTC   '.
    ztitbpmlh-chgind = 'U'.
    INSERT ztitbpmlh.

  ENDLOOP.
  MESSAGE s000 WITH 'Data saved: ' l_idx.

ENDFORM.                    " data_save_alv
*&--------------------------------------------------------------------*
*&      Form  UPLOAD_EXCEL
*&--------------------------------------------------------------------*
FORM upload_excel .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
      i_line_header        = 'X'
      i_tab_raw_data       = it_raw       " WORK TABLE
      i_filename           = p_file
    TABLES
      i_tab_converted_data = gt_uptab[]    "ACTUAL DATA
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT gt_uptab INTO gt_cur.
    MOVE-CORRESPONDING gt_cur TO ztitbpml.
    MODIFY ztitbpml.
  ENDLOOP.
ENDFORM.                    " UPLOAD_EXCEL
**&--------------------------------------------------------------------*
**&      Form  SAVE_ZITBPML
**&--------------------------------------------------------------------*
*FORM save_zitbpml .
*
*  DATA: ls_bpml  TYPE ztitbpml.
*
*  SORT gt_bpml BY tcode.
** clean up non-exist t-code
*  SELECT * FROM ztitbpml WHERE comp = p_comp.
*    CHECK NOT ztitbpml-tctyp CA 'MX'.
*    READ TABLE gt_bpml WITH KEY tcode = ztitbpml-tcode BINARY SEARCH.
*    IF sy-subrc <> 0.
*      DELETE ztitbpml.
*    ENDIF.
*  ENDSELECT.
*
*  LOOP AT gt_bpml.
*    SELECT SINGLE * INTO ls_bpml FROM ztitbpml
*           WHERE tcode = gt_bpml-tcode.
*
*    ztitbpml = gt_bpml.
*    ztitbpml-comp  = p_comp.
*    ztitbpml-apm   = ls_bpml-apm.
*    ztitbpml-tcfrq = ls_bpml-tcfrq.
*    ztitbpml-l5    = ls_bpml-l5.
*    ztitbpml-l5t   = ls_bpml-l5t.
*    MODIFY ztitbpml.
*  ENDLOOP.
*
*ENDFORM.                    " SAVE_ZITBPML
*&--------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  REFRESH: fcode.
  APPEND 'DISPLAY' TO fcode.
  SET TITLEBAR 'T100'.

  IF g_grid->is_ready_for_input( ) EQ 0.
    APPEND 'SAVE' TO fcode.
    SET PF-STATUS 'MAIN100' EXCLUDING fcode. " 'SAVE'.
  ELSE.
    SET PF-STATUS 'MAIN100' EXCLUDING fcode.
  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&--------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.

  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.
*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo
        it_toolbar_excluding = gt_exclude
        i_save               = gc_var_save
        is_variant           = gs_variant
      CHANGING
        it_outtab            = gt_out[]
        it_fieldcatalog      = gt_fcat[]
        it_sort              = gt_sort[].

    info = text-015.

  ENDIF.
  __focus g_grid.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&--------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM create_and_init_alv .

*   Create object
  PERFORM create_object.

*   Exclude toolbar
  PERFORM exclude_functions.

*  Create Object to verify input values.
  CREATE OBJECT g_event_receiver.
  SET HANDLER : g_event_receiver->handle_data_changed FOR g_grid,
                g_event_receiver->handle_double_click FOR g_grid.

*   Create field category
  PERFORM create_field_category USING false.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.

  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  PERFORM set_lvc_layout.

*   Set colors
*  PERFORM SET_COLOR.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " CREATE_AND_INIT_ALV

*&--------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*---------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.

  DATA ls_outtab LIKE LINE OF gt_out.
  RANGES tcode1 FOR tstca-tcode.
  RANGES r_tcode FOR zthrappusge-tcode.

  CLEAR gv_index.
  gv_index = e_row-index.

  READ TABLE gt_out INTO ls_outtab INDEX gv_index.
  IF sy-subrc = 0.

    CASE e_column.
      WHEN 'ROLE'.
        CHECK ls_outtab-tcode NE space.
        tcode1 = 'IEQ'.
        tcode1-low = ls_outtab-tcode.
        APPEND tcode1.
        SUBMIT rsusr070 WITH tcode1 IN tcode1 AND RETURN.
      WHEN 'CNT'.
        CHECK ls_outtab-tcode NE space.
        r_tcode = 'IEQ'.
        r_tcode-low = ls_outtab-tcode.
        APPEND r_tcode.
        SUBMIT zitbpmu WITH s_tcode = ls_outtab-tcode AND RETURN.
      WHEN 'TCODE'.
        CALL TRANSACTION ls_outtab-tcode .
    ENDCASE.

  ENDIF.

ENDFORM.                    " DOUBLE_CLICK
*&--------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_FALSE  text
*---------------------------------------------------------------------*
FORM create_field_category USING mode_edit.
  DATA: l_pos       TYPE i.
  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    gs_fcat-just      = &7.
    gs_fcat-edit      = &8.
    gs_fcat-no_zero   = &9.
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'L4F'        'L4'                  10   'CHAR' '' 'C' '' 'X',
    'X'  'TCODE'      'T-code'              14   'CHAR' '' 'L' '' 'X',
    ' '  'L1'         'L1'                   1   'CHAR' '' 'C' '' 'X',
    ' '  'L2'         'L2'                   3   'CHAR' '' 'C' '' 'X',
    ' '  'L3'         'L3'                   3   'CHAR' '' 'C' '' 'X',
    ' '  'L4'         'L4'                   2   'CHAR' '' 'L' '' 'X',
    ' '  'L5'         'L5'                   2   'CHAR' '' 'L' '' 'X',
    ' '  'TASK'       'Task'                30   'CHAR' '' 'L' '' 'X',
    ' '  'APM'        'APM'                  3   'CHAR' '' 'L' 'X' 'X',
    ' '  'TCTYP'      'Typ'                  1   'CHAR' '' 'L' 'X' 'X',
    ' '  'RSTAT'      'Stat'                 4   'CHAR' '' 'L' 'X' 'X',
    ' '  'TCFRQ'      'Frq'                  3   'CHAR' '' 'L' 'X' 'X',
    ' '  'ZSSTVIND'   'Sensitive'           10   'CHAR' '' 'L' 'X' 'X',
    ' '  'CNT'        'Usg Cnt'              7   'DECS' '' 'R' '' 'X',
    ' '  'L2T'        'L2T'                  4   'CHAR' '' 'L' '' 'X',
    ' '  'L3T'        'L3T'                  4   'CHAR' '' 'L' '' 'X',
    ' '  'L4T'        'L4 Desc.'            40   'CHAR' '' 'L' '' 'X',
    ' '  'L5T'        'L5T'                 20   'CHAR' '' 'L' '' 'X',
    ' '  'DEVCLASS'   'Package'             30   'CHAR' '' 'L' '' 'X',
    ' '  'PGMNA'      'Program Name'        10   'CHAR' '' 'L' '' 'X',
    ' '  'CALL_TCODE' 'Calling tcd'         10   'CHAR' '' 'L' '' 'X',
    ' '  'TASKV'      'Variant'             10   'CHAR' '' 'L' '' 'X',
    ' '  'ROLE'       'Users'                5   'DECS' '' 'R' '' 'X',
    ' '  'CNT_SE38'   'SE38'                10   'DECS' '' 'R' '' 'X',
    ' '  'LAST_USED'  'LAST U'               6   'CHAR' '' 'C' '' 'X',
    ' '  'RESPMT'     'AvgR(s)'              6   'DECS' '' 'R' '' 'X',
    ' '  'TTEXT'      'Transaction Text'    30   'CHAR' '' 'L' '' 'X',
    ' '  'SKEY'       'SortK'                5   'CHAR' '' 'C' '' 'X'.
*   ' '  'ID'         'Seq'                  4   'NUMC' '' 'L' '' 'X',
*   'X'  'COMP'       'Comp'                 4   'CHAR' '' 'C' ''  'X'.

  LOOP AT gt_fcat INTO gs_fcat.
*    CASE gs_fcat-fieldname.
*      WHEN 'RSTAT' OR 'TCTYP' OR 'APM' OR 'ZSSTVIND' OR 'TCFRQ' OR 'CNT'.
    gs_fcat-ref_field = gs_fcat-fieldname.
    gs_fcat-ref_table = 'ZTITBPML'.
    MODIFY gt_fcat FROM gs_fcat.
*    ENDCASE.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
FORM exclude_functions .
*  PERFORM APPEND_EXCLUDE_FUNCTIONS
*           TABLES GT_EXCLUDE[]
*           USING: CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
*                  CL_GUI_ALV_GRID=>MC_FC_AVERAGE,
*                  CL_GUI_ALV_GRID=>MC_FC_GRAPH,
*                  CL_GUI_ALV_GRID=>MC_FC_INFO,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
FORM data_changed USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  flag_data_changed = true.

  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_cells     TYPE lvc_s_modi,
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.
    READ TABLE gt_out INDEX ls_mod_cells-row_id.
    IF sy-subrc = 0.
      CALL METHOD rr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_mod_cells-row_id
          i_fieldname = ls_mod_cells-fieldname
          i_value     = ls_mod_cells-value.

      MODIFY gt_out INDEX ls_mod_cells-row_id.
    ENDIF.
  ENDLOOP.

*  __SET_REFRESH_MODE TRUE.
*  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
*       EXPORTING IS_STABLE = STABLE.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  F4_APLY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_LS_F4_FIELDNAME  text
*      -->P_P_TAB  text
*----------------------------------------------------------------------*
FORM f4_aply USING  es_row_no_row_id
                    e_fieldname TYPE fieldname
                    p_tab.
  ls_modi-row_id    = es_row_no_row_id.
  ls_modi-fieldname = e_fieldname.
  ls_modi-value     = ls_f4-fieldval.
  APPEND ls_modi TO <f4tab>.
  READ TABLE gt_out INDEX es_row_no_row_id.

ENDFORM.                                                    " F4_APLY
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
FORM set_color.
  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  DEFINE __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  END-OF-DEFINITION.

*  __COLOR : 'ID'     '1' 0,
*            'UPGVC'  '2' 0,
*            'UPGTX'  '2' 1,
*            'KZUST'  '4' 0,
*            'COMPN'  '2' 0,
*            'COMTX'  '2' 1,
*            'LIFNR'  '5' 0,
*            'PMENGE' '7' 0,
*            'MENGE'  '3' 0,
*            'DMENGE' '6' 0.
*  GT_OUT-TABCOLOR[] = GT_SPECIALCOL[].
*  MODIFY GT_OUT TRANSPORTING TABCOLOR WHERE TABCOLOR IS INITIAL.
ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
FORM set_lvc_layout .
  CLEAR gs_layo.

  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
*  gs_layo-stylefname = 'CELLTAB'.

ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
FORM sort_build USING ft_sort TYPE lvc_t_sort.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

*  SORT_TAB : 'ID'       '1' 'X' 'X' '' 'X',
*             'UPGVC'    '2' 'X' 'X' '' 'X',
*             'UPGTX'    ' ' 'X' 'X' '' 'X',
*             'KZUST'    '3' 'X' 'X' '' 'X',
*             'COMPN'    '4' 'X' 'X' '' 'X',
*             'COMTX'    ' ' 'X' 'X' '' 'X',
*             'LIFNR'    ' ' 'X' 'X' '' 'X',
*             'TMATNR'   ' ' 'X' 'X' '' 'X'.

ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
FORM info_text_set USING p_true.
  IF p_true EQ true.
    info = text-016.
  ELSE.
    info = text-015.
  ENDIF.
ENDFORM.                    " info_text_set
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Build field catalog for ALV grid
*----------------------------------------------------------------------*
*      -->P_FNAME Field name
*      -->P_TXT   Column heading
*      -->P_LEN   Column width
*      -->P_TYPE  Data type
*----------------------------------------------------------------------*
FORM fill_field_category USING p_pos   TYPE lvc_colpos
                               p_fname TYPE lvc_fname
                               p_txt   TYPE lvc_txtcol
                               p_len   TYPE lvc_outlen
                               p_type  TYPE datatype_d.

  CLEAR gs_fcat.

  gs_fcat-col_pos   = p_pos.     " Column position
  gs_fcat-fieldname = p_fname.   " Field name
  gs_fcat-coltext   = p_txt.     " Column heading
  gs_fcat-outputlen = p_len.     " Column width
  gs_fcat-datatype  = p_type.    " Data type

  APPEND gs_fcat TO gt_fcat.

ENDFORM.                    " FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       Append excluding functions
*----------------------------------------------------------------------*
*      -->P_TABNAME   Table name
*      -->P_VALUE     Excluding value
*----------------------------------------------------------------------*
FORM append_exclude_functions TABLES p_table
                              USING p_value.
  DATA ls_exclude TYPE ui_func.

  ls_exclude = p_value.
  APPEND ls_exclude TO p_table.

ENDFORM.                    " APPEND_EXCLUDE_FUNCTIONS

*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
FORM set_layout USING    p_edit_mode
                         p_box_fname
                CHANGING cs_layo TYPE slis_layout_alv.
  cs_layo-edit_mode         = p_edit_mode.
  cs_layo-numc_sum          = 'X'.
  cs_layo-box_fieldname     = p_box_fname.
  cs_layo-group_buttons     = 'X'.
  cs_layo-group_change_edit = 'X'.
  cs_layo-coltab_fieldname  = 'TABCOLOR'.
  cs_layo-colwidth_optimize = 'X'.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_EVENTS
*&---------------------------------------------------------------------*
FORM set_events CHANGING ct_events TYPE slis_t_event.
  DATA ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = ct_events.

  READ TABLE ct_events WITH KEY name =  slis_ev_top_of_page
                            INTO ls_event.
  IF     sy-subrc = 0.
    MOVE   gc_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO ct_events.
  ENDIF.

ENDFORM.                    " SET_EVENTS
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
  DATA ls_line TYPE slis_listheader.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Date:'.
  CONCATENATE sy-datum+0(4) sy-datum+4(2) sy-datum+6(2)
         INTO ls_line-info SEPARATED BY '.'.

  APPEND ls_line TO lt_top_of_page.

  ls_line-key  = 'User:'.
  ls_line-info = sy-uname.

  APPEND ls_line TO lt_top_of_page.

  ls_line-key  = ''.
  ls_line-info = ''.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    " COMMENT_BUILD
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
*       Create custom container control & instance
*----------------------------------------------------------------------*
FORM create_object.
* create a custom container control for our alv control
  CREATE OBJECT g_custom_container
    EXPORTING
      container_name              = container
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  gv_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = gv_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

* Create an Instance of ALV Control
  CREATE OBJECT g_grid
    EXPORTING
      i_parent = g_custom_container.

ENDFORM.                    " CREATE_OBJECT

* For BDC
*---------------------------------------------------------------------*
*       Form DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM dynpro USING p_dynbegin p_name p_value.
  CLEAR gt_bdc.

  IF p_dynbegin = 'X'.
    gt_bdc-program = p_name.
    gt_bdc-dynpro = p_value.
    gt_bdc-dynbegin = p_dynbegin.
  ELSE.
    gt_bdc-fnam = p_name.
    gt_bdc-fval = p_value.
  ENDIF.

  APPEND gt_bdc.

ENDFORM.                    " DYNPRO
*---------------------------------------------------------------------*
*       Form GET_OPT                                                   *
*---------------------------------------------------------------------*
FORM get_opt USING p_abpe.
  CLEAR gs_opt.

  gs_opt-dismode  = p_abpe.
  gs_opt-updmode  = 'X'.
  gs_opt-racommit = 'X'.
  gs_opt-nobinpt  = 'X'.

ENDFORM.                    " GET_OPT
*---------------------------------------------------------------------*
*       Form GET_MSG                                                   *
*---------------------------------------------------------------------*
FORM get_msg CHANGING p_msg.
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
      msg_lin = p_msg
    EXCEPTIONS
      OTHERS  = 1.

ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  DATA_INPUT_ERROR
*&---------------------------------------------------------------------*
*       Error Message Display
*----------------------------------------------------------------------*
FORM data_input_error
        USING rr_data_changed TYPE REF TO cl_alv_changed_data_protocol
              rs_mod_cells    TYPE lvc_s_modi
              p_msgty         TYPE symsgty
              p_msgv1         TYPE symsgv
              p_fieldname     TYPE lvc_fname.

* Error Message Display
  CALL METHOD rr_data_changed->add_protocol_entry
    EXPORTING
      i_msgid     = '0K'
      i_msgno     = '000'
      i_msgty     = p_msgty
      i_msgv1     = p_msgv1
      i_msgv2     = ' '
      i_msgv3     = ' '
      i_fieldname = p_fieldname
      i_row_id    = rs_mod_cells-row_id.

ENDFORM.                    " DATA_INPUT_ERROR

*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
FORM alv_variant_f4 CHANGING p_vari.
  DATA: rs_variant LIKE disvariant,
        lv_nof4 TYPE c.

  CLEAR lv_nof4.
  LOOP AT SCREEN.
    IF screen-name = 'PA_VARI'.
      IF screen-input = 0.
        lv_nof4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR rs_variant.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc = 0 AND lv_nof4 = space.
    p_vari = rs_variant-variant.
  ENDIF.

ENDFORM.                    " ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BPML
*&---------------------------------------------------------------------*
FORM generate_bpml .

  __cls gt_tadir.

  DATA ls_bpml  TYPE ztitbpml.
  DATA lv_outtab TYPE ty_out.
  DATA $ix TYPE i.
  CLEAR : g_code_l5t[], g_code_l5t.

  IF p_ar EQ 'X'.
    PERFORM get_tcode_from_areamenu.
  ELSE.
    SELECT obj_name devclass INTO TABLE gt_tadir FROM tadir
       WHERE pgmid  = 'R3TR'
         AND object = p_objtyp
         AND obj_name IN s_objs
       ORDER BY obj_name devclass.
  ENDIF.

  SORT g_code_l5t BY menuid tcode.

  LOOP AT gt_tadir.
    $ix = sy-tabix.

    CLEAR:ls_bpml, gt_cur.

    gt_cur-tcode = gt_tadir-obj_name.
    PERFORM get_tcode_info  USING gt_cur.

    IF gt_cur-tctyp EQ 'T'.
      SELECT SINGLE tcode INTO *tstc-tcode
        FROM tstc WHERE tcode EQ gt_cur-tcode.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    ls_bpml-comp       = p_comp  .
    ls_bpml-devclass   = gt_cur-devclass.
    ls_bpml-ttext      = gt_cur-ttext.
    IF gt_tadir-text NE space.
      ls_bpml-task       = gt_tadir-text.   "area menu text instead of t-code
    ELSE.
      ls_bpml-task       = gt_cur-ttext.
    ENDIF.
    ls_bpml-tcode      = gt_cur-tcode.
    ls_bpml-call_tcode = gt_cur-call_tcode.
    ls_bpml-tctyp      = gt_cur-tctyp.
    ls_bpml-pgmna      = gt_cur-pgmna.
    ls_bpml-taskv      = gt_cur-taskv.
    ls_bpml-role       = gt_cur-role.
    ls_bpml-users      = gt_cur-users.
    ls_bpml-diastepcnt = gt_cur-diastepcnt.
    ls_bpml-cnt        = gt_cur-cnt.        "used count
    ls_bpml-rstat      = gt_cur-rstat.
    ls_bpml-cnt_se38   = gt_cur-cnt_se38.   "used count by SE38,SA38
    ls_bpml-last_used  = gt_cur-last_used.
    ls_bpml-respmt     = gt_cur-respmt.     "avg. response time
    ls_bpml-cpumt      = gt_cur-cpumt.
    ls_bpml-dbmt       = gt_cur-dbmt.

* determine BPML hiarchy
    IF p_ar EQ true.  "from area menu

      ls_bpml-l1  =  gt_tadir-area_name+1(1).
      ls_bpml-l2  =  gt_tadir-area_name+2(2).
      ls_bpml-l3  =  gt_tadir-area_name+4(2).
      ls_bpml-l4  =  gt_tadir-area_name+6(2).

      CONCATENATE 'Z' ls_bpml-l1 ls_bpml-l2 ls_bpml-l3 ls_bpml-l4
       INTO ls_bpml-l4f.

      ls_bpml-l2t =  gt_tadir-area_name+2(2).
      ls_bpml-l3t =  gt_tadir-area_name+4(2).

*---- get from area menu hierarchy... - devclass -> ztitbph
      READ TABLE it_ztitbph WITH KEY l4f = gt_tadir-area_name
           BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bpml-l4t  = it_ztitbph-l4t.
        ls_bpml-skey = it_ztitbph-skey.
      ELSE.
        ls_bpml-l4t = '**no description'.
      ENDIF.

*---  for z-dev class, check with Area menu ID.
*FIXME
*      IF gt_cur-devclass(1) = 'Z' AND ls_bpml-l4f <> ls_bpml-devclass.
*        ls_bpml-rstat = 'X'.
*      ENDIF.

*--- Level 5 description
      READ TABLE g_code_l5t WITH KEY menuid = ls_bpml-l4f
                                     tcode  = ls_bpml-tcode BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_bpml-l5t = g_code_l5t-l5t.
      ENDIF.


    ELSE.

      IF gt_cur-devclass(1) = 'Z' AND strlen( gt_cur-devclass ) = 8.
        ls_bpml-l1  =  gt_cur-devclass+1(1).
        ls_bpml-l2  =  gt_cur-devclass+2(2).
        ls_bpml-l3  =  gt_cur-devclass+4(2).
        ls_bpml-l4  =  gt_cur-devclass+6(2).
        CONCATENATE 'Z' ls_bpml-l1 ls_bpml-l2 ls_bpml-l3 ls_bpml-l4
         INTO ls_bpml-l4f.

        ls_bpml-l2t =  gt_cur-devclass+2(2).
        ls_bpml-l3t =  gt_cur-devclass+4(2).
        ls_bpml-l4t =  gt_cur-l4t.

      ELSE.
        READ TABLE gt_tdevc2 WITH KEY devclass = ls_bpml-devclass
         BINARY SEARCH.
        ls_bpml-l1  =  gt_tdevc2-ztdevc1.
        ls_bpml-l2  =  gt_tdevc2-ztdevc2.
        ls_bpml-l3  =  gt_tdevc2-ztdevc3.
      ENDIF.

    ENDIF.

*    ls_bpml-cnt =  1.
*    ls_bpml-id  = $ix. "SY-TABIX.

    MOVE-CORRESPONDING ls_bpml TO lv_outtab.
    APPEND lv_outtab TO gt_out.

    gt_ref-tcode = ls_bpml-tcode.
    gt_ref-cnt   = 1.
    COLLECT gt_ref.

  ENDLOOP.


* check reference t-code - FIXME
  PERFORM check_ref_tcode_full.

ENDFORM.                    " GENERATE_BPML
*&---------------------------------------------------------------------*
*&      Form  GET_TCODE_FROM_AREAMENU
*&---------------------------------------------------------------------*
FORM get_tcode_from_areamenu.

  DATA: structure_id           LIKE ttree-id,
        message                TYPE hier_mess,
        uuid                   LIKE sdokphio-phio_id,
        list_of_nodes          TYPE hier_iface OCCURS 0
                               WITH HEADER LINE,
        list_of_references     TYPE hier_ref   OCCURS 0
                               WITH HEADER LINE,
        list_of_texts          TYPE hier_texts OCCURS 0
                               WITH HEADER LINE.

  DATA: BEGIN OF t_code_tab OCCURS 0,
        t_object TYPE sobj_name,
        text     TYPE hier_text,
        END OF t_code_tab.

  RANGES r_id FOR gt_ttree-id.
  DATA:  l_tabix LIKE sy-tabix.

  DESCRIBE TABLE s_area LINES l_tabix.
  IF l_tabix = 0.
    SELECT * FROM ztitbph.
      r_id-option = 'EQ'.
      r_id-sign   = 'I'.
      r_id-low = ztitbph-l4f.
      APPEND r_id.
    ENDSELECT.
  ELSE.
    r_id[] = s_area[].
  ENDIF.

  SELECT * INTO TABLE gt_ttree FROM ttree
                  WHERE id IN r_id.
  SORT gt_ttree BY id.

  LOOP AT gt_ttree.

    CLEAR : list_of_nodes,list_of_references,list_of_texts,
            list_of_nodes[],list_of_references[],list_of_texts[].

    structure_id = gt_ttree-id.
    CALL FUNCTION 'STREE_HIERARCHY_READ'
      EXPORTING
        structure_id       = structure_id
        read_also_texts    = 'X'
        language           = sy-langu
      TABLES
        list_of_nodes      = list_of_nodes
        list_of_references = list_of_references
        list_of_texts      = list_of_texts.

    CLEAR : t_code_tab,t_code_tab[].

    DELETE list_of_references WHERE ref_type NE 'TCOD'.
    SORT  list_of_references BY node_id.
*    DELETE list_of_texts WHERE spras NE sy-langu..
    SORT list_of_texts BY node_id.

    LOOP AT list_of_references WHERE ref_type = 'TCOD'.
      CHECK list_of_references-ref_object IN s_objs.
      t_code_tab-t_object = list_of_references-ref_object.

      READ TABLE list_of_texts WITH KEY node_id = list_of_references-node_id BINARY SEARCH.
      t_code_tab-text = list_of_texts-text.
      APPEND t_code_tab.
    ENDLOOP.

* for level 5 description
    LOOP AT list_of_nodes.
      IF list_of_nodes-parent_id NE space.
        READ TABLE list_of_texts WITH KEY node_id = list_of_nodes-parent_id BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE list_of_references WITH KEY node_id = list_of_nodes-node_id BINARY SEARCH.
          IF sy-subrc EQ 0.
            g_code_l5t-menuid = gt_ttree-id.
            g_code_l5t-tcode  = list_of_references-ref_object.
            g_code_l5t-l5t = list_of_texts-text.
            APPEND g_code_l5t.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

*    IF NOT t_code_tab[] IS INITIAL.
    LOOP AT t_code_tab.
      SELECT * FROM tadir
*         obj_name devclass APPENDING CORRESPONDING FIELDS OF TABLE gt_tadir
*         FROM tadir
*      FOR ALL ENTRIES IN t_code_tab
         WHERE pgmid  = 'R3TR'
           AND object = p_objtyp
           AND obj_name = t_code_tab-t_object.

        gt_tadir-obj_name  = tadir-obj_name.
        gt_tadir-devclass  = tadir-devclass.
        gt_tadir-area_name = structure_id.
        gt_tadir-text      = t_code_tab-text.
        APPEND gt_tadir.
*     MODIFY gt_tadir TRANSPORTING area_name WHERE area_name EQ space.
      ENDSELECT.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " GET_TCODE_FROM_AREAMENU
*&---------------------------------------------------------------------*
*&      Form  get_tcode_info
*&---------------------------------------------------------------------*
FORM get_tcode_info USING  ls_bpml  TYPE ztitbpml.

  DATA: hex_tra TYPE x VALUE '00',              " Transaktion         T
        hex_men TYPE x VALUE '01',              " Area menu           -
        hex_par TYPE x VALUE '02',              " Parametertrans.     P
        hex_rep TYPE x VALUE '80',              " Report              R
        hex_rpv TYPE x VALUE '10',              " Report  w Variante  V
        hex_obj TYPE x VALUE '08',              " Objekttransaktionen
        hex_chk TYPE x VALUE '04',              " mit Prüfobjekt
        hex_enq TYPE x VALUE '20'.              " Gesperrt über SM01

  DATA: param_beg TYPE i,
        lv_cnt    TYPE i,
        l_cinfo TYPE syhex01.

* "HMMA system haeccd00
  CLEAR: gv_dest.
  IF p_rfc = 'X'.

    IF ( syst-sysid(1) = 'U' AND p_comp = 'KMMG' ).
      IF syst-sysid = 'UP2'.
*        GV_DEST = 'EPGCLNT300'.
        gv_dest = 'KMMG_EPG300'.
      ELSE.
*        GV_DEST = 'EDGCLNT300'.
        gv_dest = 'KMMG DEV 120'.
      ENDIF.
      PERFORM get_tcode_info_rfc.

    ELSEIF  ( syst-host(2) <> 'ha' AND p_comp = 'HMMA' ).
      IF syst-sysid = 'EPG'.
*        GV_DEST = 'UP2300'.
        gv_dest = 'HMMA_UP2'.
      ELSE.
        gv_dest = 'UD1300'.
      ENDIF.
      PERFORM get_tcode_info_rfc.
    ENDIF.

*-NO RFC
  ELSE.
    CONDENSE ls_bpml-tcode NO-GAPS.
*    if p_devc = 'X' and ls_bpml-tcode+3(1) <> space.

    SELECT SINGLE devclass INTO ls_bpml-devclass FROM tadir
       WHERE pgmid = 'R3TR'
         AND object = 'TRAN'
         AND obj_name = ls_bpml-tcode.

    READ TABLE gt_tdevc WITH KEY devclass = ls_bpml-devclass
     BINARY SEARCH.
    ls_bpml-l4t = gt_tdevc-ctext.

    SELECT SINGLE pgmna cinfo INTO (ls_bpml-pgmna, l_cinfo)
       FROM tstc
       WHERE tcode = ls_bpml-tcode.

    SELECT SINGLE ttext INTO ls_bpml-ttext
       FROM tstct
       WHERE sprsl = sy-langu
         AND tcode = ls_bpml-tcode.

    IF l_cinfo O hex_rep.             " Report
      ls_bpml-tctyp = 'R'.
    ELSEIF l_cinfo O hex_par.
      ls_bpml-tctyp = 'P'.        " Trans w. param inc.view maintenance
    ELSEIF l_cinfo O hex_men.         " Menü
      ls_bpml-tctyp = ' '.
    ELSE.                                " Transaktion
      ls_bpml-tctyp = 'T'.
    ENDIF.

* get parameter of t-code
    IF ls_bpml-pgmna IS INITIAL.
*-----  LSEUKF01
      SELECT SINGLE * FROM tstcp WHERE tcode = ls_bpml-tcode.

      IF tstcp-param(1) = '\'.             " OO-Transaktion ohne FR
* perform split_parameters_comp(LSEUKF01)
*         using c_oo_program tstcp-param
*                                           changing tstc-pgmna.

      ELSEIF tstcp-param(1) = '@'.         " Transaktionsvariante

      ELSEIF tstcp-param(1) = '/'.
        IF tstcp-param CA ' '. ENDIF.
        param_beg = sy-fdpos + 1.
        SUBTRACT 2 FROM sy-fdpos.
        IF sy-fdpos GT 0.
          ls_bpml-call_tcode = tstcp-param+2(sy-fdpos).
        ENDIF.

        SELECT SINGLE pgmna INTO ls_bpml-pgmna
           FROM tstc
           WHERE tcode = ls_bpml-call_tcode.

        SHIFT tstcp-param BY param_beg PLACES.

        DATA: param TYPE rsparam OCCURS 0 WITH HEADER LINE.
        FIELD-SYMBOLS <f>.

        DO 254 TIMES.
          IF tstcp-param = space. EXIT. ENDIF.
          CLEAR param.
          CONDENSE tstcp-param NO-GAPS.
          IF tstcp-param CA '='.
            CHECK sy-fdpos NE 0.
            ASSIGN tstcp-param(sy-fdpos) TO <f>.
            param-field = <f>.
            IF param-field(1) = space. SHIFT  param-field. ENDIF.
            sy-fdpos = sy-fdpos + 1.
            SHIFT tstcp-param BY sy-fdpos PLACES.
            IF tstcp-param CA ';'.
              IF sy-fdpos NE 0.
                ASSIGN tstcp-param(sy-fdpos) TO <f>.
                param-value = <f>.
                IF param-value(1) = space. SHIFT  param-value. ENDIF.
              ENDIF.
            ENDIF.
            IF param-field = 'VIEWNAME'.
              ls_bpml-taskv = param-value.
              EXIT.
            ENDIF.
          ENDIF.
        ENDDO.

      ENDIF.

    ENDIF.

*   check batch job logs
    IF ls_bpml-pgmna <> space.
      SELECT COUNT( * ) INTO lv_cnt FROM tbtcp
          WHERE progname = ls_bpml-pgmna
            AND authcknam NOT IN s_users.
      IF sy-subrc = 0.
        ls_bpml-tctyp = 'B'.
      ENDIF.
    ENDIF.

* get attribute of program
*    SELECT SINGLE RSTAT INTO ls_bpml-RSTAT FROM REPOSRC
*       WHERE PROGNAME = ls_bpml-PGMNA.

* default is 'K'

    IF ls_bpml-tcode(1) EQ 'Z'.
      ls_bpml-rstat = 'K'.
    ELSEIF ls_bpml-tcode(1) EQ 'Y'.
      ls_bpml-rstat = 'T'.
    ELSE.
      ls_bpml-rstat = 'P'.
    ENDIF.

    PERFORM get_usage USING ls_bpml.

  ENDIF.

ENDFORM.                    " get_tcode_info
*&---------------------------------------------------------------------*
*&      Form  CHECK_REF_TCODE_FULL
*&---------------------------------------------------------------------*
FORM check_ref_tcode_full .

  DATA: lv_out LIKE gt_out,
        lv_idx LIKE sy-index.

* more than 1 used in area menu/bpml
  DELETE gt_ref WHERE cnt = 1.
  SORT: gt_out BY tcode rstat,
        gt_ref BY tcode.

  LOOP AT gt_out.
    lv_idx = sy-tabix.
    CLEAR gt_ref.

    READ TABLE gt_ref WITH KEY tcode = gt_out-tcode BINARY SEARCH.
    lv_idx = sy-tabix.
    CHECK gt_ref-l4f EQ space.

*FIXME !!!
** check dev.class
*    READ TABLE gt_tdevc2 WITH KEY devclass = gt_out-devclass
*     BINARY SEARCH.
*    IF sy-subrc = 0.
*      IF gt_tdevc2-ztdevc1 <> gt_out-l1.
*        gt_out-rstat = 'X'.
*      ELSE.
*        IF gt_tdevc2-ztdevc2 <> gt_out-l2.
*          gt_out-rstat = 'X'.
*        ELSE.
*          IF gt_tdevc2-ztdevc3 <> space AND
*             gt_tdevc2-ztdevc3 <> gt_out-l3.
*            gt_out-rstat = 'X'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      MESSAGE s000 WITH gt_out-tcode ' - Dev.class' gt_out-devclass ' has no assignment in ZTITDEVC'.
*    ENDIF.

**-- even though fail to search from ZTITDEVC,
*---use first item... by default... need enhancement
    IF gt_out-rstat NE 'X'.
      gt_ref-l4f = gt_out-l4f.
      MODIFY gt_ref INDEX lv_idx TRANSPORTING l4f.
    ENDIF.
  ENDLOOP.

* if not original t-code, update "reference indicator"
  LOOP AT gt_ref.
    gt_out-rstat = 'X'.
*   gt_out-apm   = space.  "reset APM
    MODIFY gt_out TRANSPORTING rstat   "apm
                  WHERE tcode = gt_ref-tcode
                    AND l4f   NE gt_ref-l4f.
  ENDLOOP.

*  SORT gt_out BY id.

ENDFORM.                    " CHECK_REF_TCODE_FULL
*&---------------------------------------------------------------------*
*&      Form  data_save_batch
*&---------------------------------------------------------------------*
FORM data_save_batch .
  DATA: ls_bpml  LIKE ztitbpml.
  DATA: lv_uzeit LIKE sy-uzeit.
  DATA: ls_chg1   TYPE ty_chg,
        ls_chg2   TYPE ty_chg.

  DATA: lt_new    TYPE TABLE OF ztitbpml WITH HEADER LINE,
        lt_chg    TYPE TABLE OF ztitbpml WITH HEADER LINE,
        lt_del    TYPE TABLE OF ztitbpml WITH HEADER LINE,
        lt_upd    TYPE TABLE OF ztitbpml WITH HEADER LINE,
        lt_log    TYPE TABLE OF ztitbpml WITH HEADER LINE.

  DATA: lv_n TYPE i, lv_c TYPE i, lv_d TYPE i.

  CLEAR : lt_log[], lt_log.

  lv_uzeit = sy-uzeit.
  LOOP AT gt_out.
    READ TABLE gt_prv WITH KEY l4f   = gt_out-l4f
                               tcode = gt_out-tcode BINARY SEARCH.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING gt_out TO ls_bpml.
      APPEND ls_bpml TO lt_new.
    ELSE.
*--- check changes.
      MOVE-CORRESPONDING gt_out TO ls_chg1.
      MOVE-CORRESPONDING gt_prv TO ls_chg2.
      MOVE-CORRESPONDING gt_out TO ls_bpml.
** Batch run not update key characteristics
      MOVE: gt_prv-apm TO ls_chg1-apm,
*           gt_prv-tctyp to ls_chg1-tctyp,
           gt_prv-rstat TO ls_chg1-rstat,
           gt_prv-tcfrq TO ls_chg1-tcfrq,
           gt_prv-zsstvind TO ls_chg1-zsstvind.

      MOVE: gt_prv-apm TO ls_bpml-apm,
*           gt_prv-tctyp to ls_bpml-tctyp,
           gt_prv-rstat TO ls_bpml-rstat,
           gt_prv-tcfrq TO ls_bpml-tcfrq,
           gt_prv-zsstvind TO ls_bpml-zsstvind.
** End
      IF ls_chg1 <> ls_chg2.
        APPEND ls_bpml TO lt_chg.
      ELSE.
        APPEND ls_bpml TO lt_upd.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT gt_out BY l4f tcode.
  LOOP AT gt_prv.
    READ TABLE gt_out WITH KEY l4f   = gt_prv-l4f
                               tcode = gt_prv-tcode BINARY SEARCH.
    IF sy-subrc <> 0.
      APPEND gt_prv TO lt_del.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_new LINES lv_n.
  DESCRIBE TABLE lt_chg LINES lv_c.
  DESCRIBE TABLE lt_del LINES lv_d.
  MESSAGE s000 WITH 'New/Change/Delete:' lv_n lv_c lv_d.
*  CHECK p_test = space.

* - process into table.
  LOOP AT lt_chg.
*    lt_chg-mandt = sy-mandt.
    lt_chg-comp  = p_comp.
    MOVE-CORRESPONDING lt_chg TO ztitbpmlh.

    lt_chg-aedat = sy-datum..
    lt_chg-aetim = lv_uzeit.
    lt_chg-aenam = sy-uname.
    MODIFY ztitbpml FROM lt_chg.

    CONVERT DATE sy-datum TIME sy-uzeit
     INTO TIME STAMP ztitbpmlh-timestamp TIME ZONE 'UTC   '.
    ztitbpmlh-chgind = 'U'.
    INSERT ztitbpmlh.
  ENDLOOP.
  LOOP AT lt_upd.
*    lt_upd-mandt = sy-mandt.
    lt_upd-comp  = p_comp.
    MODIFY ztitbpml FROM lt_upd.
  ENDLOOP.

  LOOP AT lt_del.
    DELETE FROM ztitbpml WHERE l4f   = lt_del-l4f
                           AND tcode = lt_del-tcode.

    MOVE-CORRESPONDING lt_del TO ztitbpmlh.
    CONVERT DATE sy-datum TIME sy-uzeit
     INTO TIME STAMP ztitbpmlh-timestamp TIME ZONE 'UTC   '.
    ztitbpmlh-chgind = 'D'.
    IF p_ar = 'X'.  "from area menu
      READ TABLE gt_ttree WITH KEY id = lt_new-l4f BINARY SEARCH.
      ztitbpmlh-aedat = gt_ttree-ldate.
      ztitbpmlh-aetim = gt_ttree-ltime.
      ztitbpmlh-aenam = gt_ttree-luser.
    ELSE.           "from tcode
      ztitbpmlh-aedat = sy-datum..
      ztitbpmlh-aetim = lv_uzeit.
      ztitbpmlh-aenam = sy-uname.
    ENDIF.

    INSERT ztitbpmlh.
  ENDLOOP.

  LOOP AT lt_new.
*    lt_new-mandt = sy-mandt.
    lt_new-comp  = p_comp.

    IF p_ar = 'X'.  "from area menu
      READ TABLE gt_ttree WITH KEY id = lt_new-l4f BINARY SEARCH.
      lt_new-aedat = gt_ttree-ldate.
      lt_new-aetim = gt_ttree-ltime.
      lt_new-aenam = gt_ttree-luser.
    ELSE.           "from tcode
      lt_new-aedat = sy-datum..
      lt_new-aetim = lv_uzeit.
      lt_new-aenam = sy-uname.
    ENDIF.

    INSERT ztitbpml FROM lt_new.

    MOVE-CORRESPONDING lt_new TO ztitbpmlh.
    CONVERT DATE sy-datum TIME sy-uzeit
     INTO TIME STAMP ztitbpmlh-timestamp TIME ZONE 'UTC   '.
    ztitbpmlh-chgind = 'I'.
    INSERT ztitbpmlh.
  ENDLOOP.

  APPEND LINES OF lt_new TO lt_log.
  APPEND LINES OF lt_chg TO lt_log.
  APPEND LINES OF lt_upd TO lt_log.

  SORT lt_log BY l4f tcode.
  DELETE ADJACENT DUPLICATES FROM lt_log COMPARING l4f tcode.

  PERFORM save_data_bpml_log TABLES lt_log.

ENDFORM.                    " data_save_batch
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES      p_fieldcat_t  TYPE slis_t_fieldcat_alv
                   USING       p_fieldname       " FIELD name
                               p_title           " field titlw
                               p_outputlen       " length
                               p_key             "
                               p_just            "
                               p_noout           "
                               p_round           "
                               p_cfield          " currency field nam
                               p_qfield          " quantity field nam
                               p_dosum           " make sum
                               .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO p_fieldcat_t.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_HIERARCHY
*&---------------------------------------------------------------------*
FORM display_hierarchy .
*--- ALV
  TYPE-POOLS: slis.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        lt_sort     TYPE slis_t_sortinfo_alv,
        ls_layout   TYPE slis_layout_alv,
        l_repid     LIKE sy-repid.
  DATA: ls_sort     TYPE slis_sortinfo_alv.
*--- ALV
  DEFINE sort_tab.
    clear ls_sort.
    ls_sort-fieldname = &1.
    ls_sort-spos      = &2.
    ls_sort-up        = &3.
    ls_sort-group     = &4.
    ls_sort-subtot    = &5.
    ls_sort-comp      = &6.
    append ls_sort to lt_sort.
  END-OF-DEFINITION.

  PERFORM field_setting TABLES lt_fieldcat USING :
 'L1'        'L1'             '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'L2'        'L2'             '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'L2T'       'L2-Txt'         '25' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'L3'        'L3'             '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'L3T'       'L3-Txt'         '25' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'L4F'       'L4'             '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'L4T'       'L4-Txt'         '30' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'SKEY'      'Sort Key'       '08' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'MENUDESC'  'Areamenu Desc.' '30' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  sort_tab : 'SKEY'     '1' 'X' 'X' '' 'X',
             'L1'       '2' 'X' 'X' '' 'X',
             'L2'       '3' 'X' 'X' '' 'X',
             'L2T'      '4' 'X' 'X' '' 'X',
             'L3'       '5' 'X' 'X' '' 'X',
             'L3T'      '6' 'X' 'X' '' 'X',
             'L4F'      '7' 'X' 'X' '' 'X'.

  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = l_repid
      it_fieldcat        = lt_fieldcat
      it_sort            = lt_sort
*     i_save             = 'A'
      i_default          = space
    TABLES
      t_outtab           = it_ztitbph
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

* CALL SCREEN 0200.

ENDFORM.                    " DISPLAY_HIERARCHY
*&---------------------------------------------------------------------*
*      FORM display_hierarchy .
*&---------------------------------------------------------------------*
FORM edit_hierarchy .
*  SUBMIT zitbphm WITH p_edit = 'X'
*     AND RETURN.
ENDFORM.                    " EDIT_HIERARCHY
*ALV for Hierarchy display - start
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE user_command_0200 INPUT.
*  CASE ok_code.
*    WHEN 'BACK'.
*      LEAVE TO SCREEN 0.
*    WHEN 'EXIT'.
*      LEAVE PROGRAM.
*  ENDCASE.
*ENDMODULE.                 " USER_COMMAND_0200  INPUT
**&---------------------------------------------------------------------*
**&      Module  STATUS_0200  OUTPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE status_0200 OUTPUT.
*  SET PF-STATUS 'ST200'.
*  SET TITLEBAR 'T200'.
*
*ENDMODULE.                 " STATUS_0200  OUTPUT
**&---------------------------------------------------------------------*
**&      Module  DISPLAY_ALV_0200  OUTPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE display_alv_0200 OUTPUT.
*  IF g_docking_container IS INITIAL. "/Not Created Control for ALV GRID
*    PERFORM create_container_n_object.
*    PERFORM set_attributes_alv_grid.
*    PERFORM build_sortcat_display.
*    PERFORM build_field_catalog USING 'IT_LEVEL'.
*    PERFORM assign_itab_to_alv.
*  ELSE.
*    CALL METHOD g_grid_0200->refresh_table_display.
*  ENDIF.
*ENDMODULE.                 " DISPLAY_ALV_0200  OUTPUT
*
**&---------------------------------------------------------------------*
**&      Form  CREATE_CONTAINER_N_OBJECT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM create_container_n_object.
*  DATA: l_dynnr LIKE sy-dynnr.
*
*  CLEAR: gv_repid.
*
*  gv_repid = sy-repid.
*  l_dynnr = sy-dynnr.
*
*  CREATE OBJECT g_docking_container
*    EXPORTING
*      repid     = gv_repid
*      dynnr     = l_dynnr
*      side      = cl_gui_docking_container=>dock_at_bottom
**     RATIO     = 90
*      extension = 2000.
*
*  IF sy-subrc NE 0.
*    CALL FUNCTION 'POPUP_TO_INFORM'
*      EXPORTING
*        titel = gv_repid
*        txt2  = sy-subrc
*        txt1  = 'The control can not be created'.
*  ENDIF.
*  CREATE OBJECT g_grid_0200
*    EXPORTING
*      i_parent      = g_docking_container
*      i_appl_events = 'X'.
*
*ENDFORM.                    " CREATE_CONTAINER_N_OBJECT
*
**---------------------------------------------------------------------*
**       FORM set_attributes_alv_grid                                  *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM set_attributes_alv_grid.
*  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings
*
*  CLEAR : wa_is_layout, wa_variant.
*
**//-- Set Layout Structure
*  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
*  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
*  wa_is_layout-language   = sy-langu. "/Language Key
*  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
**  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
*
**//-- Set Variant Structure
*  wa_variant-report       = sy-repid.
*  wa_variant-username     = sy-uname.
*  wa_variant-variant     = p_vari.
*
*
*ENDFORM.                    " set_attributes_alv_grid
**---------------------------------------------------------------------*
**       FORM build_field_catalog                                      *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
**  -->  P_ITAB                                                        *
**---------------------------------------------------------------------*
*FORM build_field_catalog USING p_itab.
*
*  DATA: lw_itab TYPE slis_tabname.
**        lw_waers LIKE t001-waers,
*
*  CLEAR: it_fieldcat,  it_fieldcat[],
*         it_fieldname, it_fieldname[].
*  CLEAR: gv_repid.
*
*  lw_itab = p_itab.
*
*  gv_repid = sy-repid.
*
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name     = gv_repid
*      i_structure_name   = 'ZTITBPH'
**     i_internal_tabname = lw_itab
*      i_inclname         = gv_repid
*    CHANGING
*      ct_fieldcat        = it_fieldname.
*
*  PERFORM setting_fieldcat TABLES it_fieldcat USING :
*
*                      'S' 'L1'       ' ',
**                      ' ' 'KEY'         'X',
*                      ' ' 'COLTEXT'     'A',
*                      'E' 'OUTPUTLEN'   '1',
*
*                      'S' 'L2'          ' ',
**                      ' ' 'KEY'         'X',
*                      ' ' 'COLTEXT'     'M',
*                      'E' 'OUTPUTLEN'   '2',
*
*                      'S' 'L2T'         ' ',
*                      ' ' 'COLTEXT'     'Description (Module)',
*                      'E' 'OUTPUTLEN'   '30',
*
*                      'S' 'L3'       ' ',
**                      ' ' 'KEY'         'X',
*                      ' ' 'COLTEXT'     'Sub',
*                      'E' 'OUTPUTLEN'   '3',
*                      'S' 'L3T'         ' ',
*                      ' ' 'COLTEXT'     'Description (Sub Mod)',
*                      'E' 'OUTPUTLEN'   '60',
*
*                      'S' 'SKEY'        ' ',
*                      ' ' 'COLTEXT'     'SKey',
*                      'E' 'OUTPUTLEN'   '5',
*
*                      'S' 'L4'       ' ',
*                      ' ' 'COLTEXT'     'PG',
*                      'E' 'OUTPUTLEN'   '4',
*
*                      'S' 'L4F'       ' ',
*                      ' ' 'COLTEXT'     'DevCls',
*                      'E' 'OUTPUTLEN'   '10',
*
*                      'S' 'L4T'       ' ',
*                      ' ' 'COLTEXT'     'Dev Description',
*                      'E' 'OUTPUTLEN'   '40'.
*
*
*
*ENDFORM.                    "build_field_catalog
**&--------------------------------------------------------------------*
**&      Form  setting_fieldcat
**&--------------------------------------------------------------------*
**       text
**---------------------------------------------------------------------*
**      -->P_IT_FIELDCAT  text
**      -->P_0584   text
**      -->P_0585   text
**      -->P_0586   text
**---------------------------------------------------------------------*
*FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
*                      USING    p_gubun
*                               p_field
*                               p_value.
*  DATA : l_col(40).
*
*  FIELD-SYMBOLS <fs>.
*
** START - FIELD ATTRIBUTE SETTING
*  IF p_gubun = 'S'.
*    CLEAR: p_fieldcat.
*
*    READ TABLE it_fieldname INTO w_fieldname
*                            WITH KEY fieldname  = p_field.
*    IF sy-subrc NE 0.
*      MESSAGE e000(zz) WITH 'Check field catalog'.
*    ENDIF.
*
*    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
*    EXIT.
*  ENDIF.
*
** Setting The Field's Attributes
*  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
*  ASSIGN (l_col) TO <fs>.
*  MOVE   p_value TO <fs>.
*
** END - FIELD ATTRIBUTE SETTING
*  IF p_gubun = 'E'.
*    ADD 1 TO w_cnt.
*    p_fieldcat-col_pos = w_cnt.
*    APPEND p_fieldcat.
*  ENDIF.
*ENDFORM.                    " setting_fieldcat
**&--------------------------------------------------------------------*
**&      Form  ASSIGN_ITAB_TO_ALV
**&--------------------------------------------------------------------*
**       text
**---------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**---------------------------------------------------------------------*
*FORM assign_itab_to_alv.
*  CALL METHOD g_grid_0200->set_table_for_first_display
*    EXPORTING
*      is_layout            = wa_is_layout
**     i_save               = wa_save
**     is_variant           = wa_variant
**     i_default            = space
**     it_toolbar_excluding = it_toolbar_excluding[]
*    CHANGING
*      it_fieldcatalog      = it_fieldcat[]
*      it_outtab            = it_ztitbph[]
*      it_sort              = it_sort[].
*
*ENDFORM.                    " ASSIGN_ITAB_TO_ALV
**&--------------------------------------------------------------------*
**&      Form  BUILD_SORTCAT_DISPLAY
**&--------------------------------------------------------------------*
**       text
**---------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**---------------------------------------------------------------------*
*FORM build_sortcat_display .
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'L1'.
*  it_sort-up             = 'X'.
**  IT_SORT-SUBTOT         = 'X'.
*  APPEND it_sort.
*
*  it_sort-spos           = 2.
*  it_sort-fieldname      = 'L2'.
*  it_sort-up             = 'X'.
*  APPEND it_sort.
*
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'L2T'.
*  it_sort-up             = 'X'.
*  APPEND it_sort.
*
*  it_sort-spos           = 4.
*  it_sort-fieldname      = 'L3'.
*  it_sort-up             = 'X'.
*  APPEND it_sort.
*
*  it_sort-spos           = 5.
*  it_sort-fieldname      = 'L3T'.
*  it_sort-up             = 'X'.
*  APPEND it_sort.
*
*  it_sort-spos           = 6.
*  it_sort-fieldname      = 'SKEY'.
*  it_sort-up             = 'X'.
*  APPEND it_sort.
*
*ENDFORM.                    " BUILD_SORTCAT_DISPLAY
*ALV - end - display hierarchy

*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data .
*  s_l1-option = 'NE'.
*  s_l1-sign = 'I'.
*  s_l1-low = 'I'.
*  APPEND s_l1.
ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_USAGE
*&---------------------------------------------------------------------*
FORM get_usage  USING  ls_bpml  TYPE ztitbpml.
  DATA:   l_cnt TYPE i.
  RANGES: r_roles FOR agr_tcodes-agr_name.

*   get authorization usage
  REFRESH: r_roles.
  r_roles-option = 'EQ'. r_roles-sign = 'I'.
*    SELECT * FROM agr_tcodes
*      WHERE type  = 'TR'
*        AND tcode = ls_bpml-tcode
*        AND agr_name IN s_roles.

* tuning
  LOOP AT gt_1251.
    IF gt_1251-low = ls_bpml-tcode
    OR ( ls_bpml-tcode BETWEEN gt_1251-low AND gt_1251-high ).
      r_roles-low = gt_1251-agr_name.
      APPEND r_roles.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE r_roles LINES l_cnt.
  IF l_cnt > 0.
    SELECT COUNT( DISTINCT uname ) INTO l_cnt
       FROM agr_users
       WHERE agr_name IN r_roles.
    ls_bpml-role = l_cnt.
  ENDIF.

* get usage
  CLEAR ls_bpml-cnt.
*  SELECT COUNT(  DISTINCT ldate ) INTO l_cnt
  SELECT COUNT( * ) INTO l_cnt   "01/13/2014 Victor
    FROM zthrappusge CLIENT SPECIFIED
    WHERE mandt = 300
      AND tcode = ls_bpml-tcode
      AND ldate IN p_usgdt   " BETWEEN lv_date AND p_usgdt
      AND account IN s_users.
  ls_bpml-cnt = l_cnt.

* get usage by SE38
  CLEAR ls_bpml-cnt_se38.
  SELECT COUNT( * ) INTO l_cnt FROM zthrappusge CLIENT SPECIFIED
    WHERE mandt = 300
      AND tcode = ls_bpml-pgmna
      AND ldate IN p_usgdt    "<= p_usgdt
      AND account IN s_users.
  ls_bpml-cnt_se38 = l_cnt.

* get lastest usage date
  DATA l_last_used TYPE datum.

  SELECT ldate INTO l_last_used FROM zthrappusge CLIENT SPECIFIED
    WHERE mandt = 300
      AND ( tcode = ls_bpml-tcode OR tcode = ls_bpml-pgmna )
      AND ldate IN p_usgdt   "<= p_usgdt
      AND account IN s_users ORDER BY ldate DESCENDING.

    EXIT.
  ENDSELECT.

  ls_bpml-last_used = l_last_used(6).

* get avg. response time
* get usage
  CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
    EXPORTING
      months  = -1
      olddate = p_usgdt-high
    IMPORTING
      newdate = lv_date.

  DATA: lv_respmt TYPE p DECIMALS 1.

*  SELECT MAX( respmt ) INTO ls_bpml-respmt  FROM zthrappusge CLIENT SPECIFIED
*  SELECT  AVG( respmt ) INTO ls_bpml-respmt
*    FROM zthrappusge CLIENT SPECIFIED
*    WHERE mandt = 300
*      AND tcode = ls_bpml-tcode
*      AND ldate   IN p_usgdt " BETWEEN lv_date AND p_usgdt
*      AND account IN s_users.

*-Added on 10.31.2013   later merge with upper statement
  SELECT  AVG( respmt ) AVG( cpumt ) AVG( dbmt ) COUNT( DISTINCT account ) SUM( diastepcnt )
        INTO (ls_bpml-respmt, ls_bpml-cpumt, ls_bpml-dbmt, ls_bpml-users, ls_bpml-diastepcnt )
    FROM zthrappusge CLIENT SPECIFIED
    WHERE mandt = 300
      AND tcode = ls_bpml-tcode
      AND ldate   IN p_usgdt " BETWEEN lv_date AND p_usgdt
      AND account IN s_users.

ENDFORM.                    " GET_USAGE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_USAGE_DATA
*&---------------------------------------------------------------------*
FORM update_usage_data .

  DATA: lt_bpml    TYPE TABLE OF ztitbpml WITH HEADER LINE.

  SELECT * FROM ztitbpml INTO TABLE lt_bpml " UP TO p_max ROWS
      WHERE comp = p_comp
        AND l1    IN s_l1
        AND l2    IN s_l2
        AND apm   IN s_apm
        AND tcode IN s_objs
        AND l4f   IN s_area.


  LOOP AT lt_bpml.
*   get usage
    PERFORM get_usage USING lt_bpml.

    MOVE-CORRESPONDING lt_bpml TO ztitbpml.
    MODIFY ztitbpml.
  ENDLOOP.

ENDFORM.                    " UPDATE_USAGE_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_BPML_LOG
*&---------------------------------------------------------------------*
FORM save_data_bpml_log  TABLES   pt_log STRUCTURE ztitbpml.
  DATA : lt_bpml_log TYPE TABLE OF ztitbpml_log WITH HEADER LINE.
  DATA : l_zseq(6) TYPE n.

  CLEAR : lt_bpml_log[], lt_bpml_log.

  SELECT zseq INTO l_zseq
    FROM ztitbpml_log
    UP TO 1 ROWS
    WHERE zdate = p_usgdt-low
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT pt_log.
    MOVE-CORRESPONDING pt_log TO lt_bpml_log.

    lt_bpml_log-zdate = p_usgdt-low.
    lt_bpml_log-zseq  = l_zseq + sy-tabix.
    lt_bpml_log-ztime = sy-uzeit.
    lt_bpml_log-aenam = sy-uname.
    lt_bpml_log-aedat =  sy-datum.
    lt_bpml_log-aetim =  sy-uzeit.

    APPEND lt_bpml_log.
    CLEAR  lt_bpml_log.
  ENDLOOP.
  INSERT ztitbpml_log FROM TABLE  lt_bpml_log.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    MESSAGE e000 WITH 'Error when Updating ztitbpml_log table'.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " SAVE_DATA_BPML_LOG
