*----------------------------------------------------------------------
* Program ID        : ZR_MM_VEHICLE_CHANGE
* Title             : Change Test Vehicle Tracking record
* Created on        : 11/16/2007
* Created by        : Rakesh Gandhi
* Specifications By : Paul Shrewsbury
* Description       : This program changes test vehicle tracking records
*----------------------------------------------------------------------
REPORT zr_mm_vehicle_change MESSAGE-ID zmco.
TYPE-POOLS: slis,
            icon.

TABLES: mcha,
        mch1,
        mchb,
        klah,
        csks,
        ska1,
        bseg,
        zyesno.
*
*DATA: ZYESNO.
*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: BEGIN OF it_charg OCCURS 0   ,
        charg LIKE mcha-charg      ,
      END OF it_charg              .

DATA: vin_num        TYPE ze_vin        ,
      full_spec_cd   TYPE ze_fsc        ,
      ask_dept       TYPE ze_ask_dept   ,
      usage_dept     TYPE ze_usage_dept ,
      gl_act         TYPE ze_glact      .

DATA: BEGIN OF it_charg1 OCCURS 0             ,
        charg              LIKE mcha-charg    ,
        vin_num            TYPE ze_vin        ,
        fsc                TYPE ze_fsc        ,
        ext_clr(3)         TYPE c             ,
        int_clr(3)         TYPE c             ,
        rp18_shop_date(10) TYPE c             ,
        car_type(2)        TYPE c             ,
        usage(1)           TYPE c             ,

* by ig.moon 6/1/2010 {
        vtype(1)           TYPE c             ,
* }

** Changed by Furong on 02/20/08  "UD1K942932
        ent_typ(10)         TYPE c             ,
        ftz_entry(13)         TYPE c             ,
        duty_paid(10)         TYPE c             ,
        duty_amt(11)          TYPE c             ,
        temp_rem(17)         TYPE c             ,
        ret_date(10)         TYPE c             ,
        scrap_rev(10)         TYPE c             ,
** End of change   on 02/20/08   "UD1K942932
        usage_text(30)     TYPE c             ,

**Changed by Furong on 06/21/10
        doc_pvt(1)         TYPE c             ,
        doc_trf(1)         TYPE c             ,
        doc_meraf(1)         TYPE c             ,
        doc_bol(1)          TYPE c             ,
** End of change

        ask_div(20)        TYPE c             ,
        ask_dept           TYPE ze_ask_dept   ,
        usage_dept         TYPE ze_usage_dept ,
        ref_doc(15)        TYPE c             ,
        final_dest(15)     TYPE c             ,
        ship_date(10)      TYPE c             ,
        ship_docs(12)      TYPE c             ,
        gl_acct(10)        TYPE c             ,
        acc_doc(10)        TYPE c             ,
        invoice            TYPE ze_invoice    ,
**Changed by Furong on 01/23/12
        yes(1)              TYPE c             ,
        desc(30)           TYPE c             ,
** end on 01/23/12
        chkbox(1)          TYPE c             ,
        icon               TYPE icon_d        ,
      END OF it_charg1                        ,

      BEGIN OF it_ausp_tmp OCCURS 0           ,
        objek LIKE ausp-objek                 ,
        atwrt LIKE ausp-atwrt                 ,
        atflv LIKE ausp-atflv                 ,
      END OF it_ausp_tmp                      ,

      BEGIN OF it_inob OCCURS 0               ,
        objek LIKE inob-objek                 ,
      END OF it_inob                          ,

      BEGIN OF it_objek OCCURS 0              ,
        cuobj LIKE inob-cuobj                 ,
      END OF it_objek                         ,

      BEGIN OF it_cabn OCCURS 0               ,
        atinn LIKE cabn-atinn                 ,
        atnam LIKE cabn-atnam                 ,
        atfor LIKE cabn-atfor                 ,
      END OF it_cabn                          ,

      BEGIN OF it_ksml OCCURS 0               ,
        imerk LIKE ksml-imerk                 ,
      END OF it_ksml                          ,

      BEGIN OF it_shop_dt OCCURS 0,
        date LIKE sy-datum        ,
      END OF it_shop_dt           ,

      BEGIN OF it_ship_dt OCCURS 0,
        date LIKE sy-datum        ,
      END OF it_ship_dt           ,

      BEGIN OF it_vin OCCURS 0    ,
        vin(18) TYPE c            ,
      END OF it_vin               ,

      BEGIN OF it_fsc OCCURS 0    ,
        fsc(19) TYPE c            ,
      END OF it_fsc               ,

      BEGIN OF it_askdep OCCURS 0 ,
        askdept(10) TYPE c        ,
      END OF it_askdep            ,

      BEGIN OF it_usagedep OCCURS 0,
        usage_dept(10) TYPE c      ,
      END OF it_usagedep           ,

      BEGIN OF it_glno OCCURS 0   ,
        glno(6) TYPE n            ,
      END OF it_glno              ,
** Changed by Furong on 02/21/08
      BEGIN OF it_enttyp OCCURS 0 ,
        enttyp(10) TYPE c            ,
      END OF it_enttyp              ,
      BEGIN OF it_ftz OCCURS 0 ,
        ftz(13) TYPE c         ,
      END OF it_ftz              ,
      BEGIN OF it_dpaid OCCURS 0 ,
        dpaid(10) TYPE c            ,
      END OF it_dpaid              ,
      BEGIN OF it_trem OCCURS 0 ,
        trem(17) TYPE c            ,
      END OF it_trem              ,
      BEGIN OF it_rdate OCCURS 0 ,
        rdate  TYPE d            ,
      END OF it_rdate              ,
** End of change on 02/21/08
** Changed by Furong on 01/20/12
      BEGIN OF it_yes OCCURS 0 ,
        yes(1) TYPE c            ,
      END OF it_yes              ,
** eND ON 01/20/12
      BEGIN OF it_rettab OCCURS 0 .
        INCLUDE STRUCTURE bapiret2.
DATA: END OF it_rettab            ,

      BEGIN OF it_object OCCURS 0             .
        INCLUDE STRUCTURE bapi1003_object_keys.
DATA: END OF it_object                        ,

*-Holds data for charcateristics with type NUM
      BEGIN OF it_numtab OCCURS 0                  .
        INCLUDE STRUCTURE bapi1003_alloc_values_num.
DATA: END OF it_numtab                             ,

*-Holds data for charcateristics with type CHAR/DATE
      BEGIN OF it_chatab OCCURS 0                   .
        INCLUDE STRUCTURE bapi1003_alloc_values_char.
DATA: END OF it_chatab                              ,

      BEGIN OF it_curtab OCCURS 0                   .
        INCLUDE STRUCTURE bapi1003_alloc_values_curr.
DATA: END OF it_curtab                              .

DATA: it_charg3 LIKE it_charg OCCURS 0 WITH HEADER LINE   ,
      it_charg2 LIKE it_charg OCCURS 0 WITH HEADER LINE   ,
      it_clbatch LIKE clbatch OCCURS 0 WITH HEADER LINE   ,
      custom_control TYPE   scrfname VALUE 'ALV_CONTAINER',
      alv_grid       TYPE   REF TO cl_gui_alv_grid        ,
      grid_container TYPE   REF TO cl_gui_custom_container,
      event_receiver TYPE   REF TO lcl_event_receiver     .

*-ALV Data declaration
DATA : gt_fieldcat TYPE lvc_t_fcat WITH HEADER LINE,
       gv_repid    LIKE sy-repid           ,
       gv_variant  TYPE disvariant         , " for parameter IS_VARIANT
       gv_save     TYPE c   VALUE 'A'      , " for Parameter I_SAVE
       gv_charg    LIKE mcha-charg         ,
       gv_object   LIKE bapi1003_key-object,
       ok_code     LIKE sy-ucomm           ,
       gv_dokar    LIKE draw-dokar VALUE 'ICD',
       gv_doknr    LIKE draw-doknr            ,
       gv_tabix    LIKE sy-tabix              ,
       gv_txt(100) TYPE c                     ,
       gv_vin      LIKE cabn-atinn            ,
       gv_fsc      LIKE cabn-atinn            ,
       gv_askdept  LIKE cabn-atinn            ,
       gv_usagedep LIKE cabn-atinn            ,
       gv_glno     LIKE cabn-atinn            ,
       gv_acc_doc  LIKE cabn-atinn            ,
       gv_ent_typ   LIKE cabn-atinn            ,
       gv_ftz_entry LIKE cabn-atinn            ,
       gv_duty_paid LIKE cabn-atinn            ,
       gv_duty_amt  LIKE cabn-atinn            ,
       gv_temp_rem  LIKE cabn-atinn            ,
       gv_ret_date  LIKE cabn-atinn            ,

* by ig.moon 6/3/2010 {
       gv_test_doc  LIKE cabn-atinn            ,
       gv_used_for  LIKE cabn-atinn            ,
* }
** by Furong on 06/21/10
      gv_doc_pvt   LIKE cabn-atinn            ,
      gv_doc_trf   LIKE cabn-atinn            ,
      gv_doc_meraf LIKE cabn-atinn            ,
      gv_doc_bol   LIKE cabn-atinn            .
** end of change

DATA : gs_fieldcat TYPE lvc_s_fcat,
       gs_layout   TYPE lvc_s_layo. " The Layout Structure

* Define internal tables &sstructures for Possible Entry
DATA : gs_values TYPE seahlpres,
       gt_fields TYPE TABLE OF dfies WITH HEADER LINE,
       gt_values TYPE TABLE OF seahlpres WITH HEADER LINE,
       gs_fields TYPE dfies,
       ls_f4     TYPE ddshretval,
       ls_modi   TYPE lvc_s_modi.

FIELD-SYMBOLS : <f4tab> TYPE lvc_t_modi.

TYPES ddshretval_table TYPE TABLE OF ddshretval.

DATA :  gs_f4        TYPE lvc_s_f4,
        gt_f4        TYPE lvc_t_f4.

DATA : BEGIN OF gt_vtype OCCURS 3,
           vtype(1),
           text(50),
       END OF gt_vtype.

*DATA: char10(10) TYPE c.
CONSTANTS: c_matnr LIKE mara-matnr  VALUE 'TEST VEHICLE' ,
           c_werks LIKE t001w-werks VALUE 'P001'         ,
           c_klart LIKE klah-klart  VALUE '022'          ,
           c_class LIKE klah-class  VALUE 'P_VEHICLE_TRACKING'.


*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
    handle_toolbar FOR EVENT toolbar
                         OF cl_gui_alv_grid
                         IMPORTING e_object e_interactive,

    handle_user_command  FOR EVENT user_command
                         OF cl_gui_alv_grid
                         IMPORTING e_ucomm sender,
* by ig.moon 6/1/2010 {

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

* }

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_toolbar.
    CONSTANTS: c_separator TYPE i VALUE 3.

    DATA: ls_toolbar  TYPE stb_button.
*-Append seperator to the normal toolbar
    CLEAR ls_toolbar.
    MOVE c_separator TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.
*-Append a new button that to the toolbar. Use E_OBJECT of
*-event toolbar. E_OBJECT is of type CL_ALV_EVENT_TOOLBAR_SET.
*-This class has one attribute MT_TOOLBAR which is of table type
*-TTB_BUTTON. The structure is STB_BUTTON
*-Change button
    CLEAR ls_toolbar.
    MOVE 'CH_BATCH'     TO ls_toolbar-function.
    MOVE  icon_change   TO ls_toolbar-icon.
    MOVE 'CHANGE BATCH' TO ls_toolbar-quickinfo.
    MOVE 'Change Batch' TO ls_toolbar-text.
    MOVE ' '            TO ls_toolbar-disabled.
    APPEND ls_toolbar   TO e_object->mt_toolbar.
*-Delete button
    MOVE 'DL_BATCH'     TO ls_toolbar-function.
    MOVE  icon_delete   TO ls_toolbar-icon.
    MOVE 'DELETE BATCH' TO ls_toolbar-quickinfo.
    MOVE 'Delete Batch' TO ls_toolbar-text.
    MOVE ' '            TO ls_toolbar-disabled.
    APPEND ls_toolbar   TO e_object->mt_toolbar.

*-Create Document button
    MOVE 'CR_DOC'          TO ls_toolbar-function.
    MOVE  icon_document    TO ls_toolbar-icon.
    MOVE 'CREATE DOCUMENT' TO ls_toolbar-quickinfo.
    MOVE 'Create Document' TO ls_toolbar-text.
    MOVE ' '               TO ls_toolbar-disabled.
    APPEND ls_toolbar      TO e_object->mt_toolbar.

*-Create Document button 2
*    MOVE 'CR_DOC2'          TO ls_toolbar-function.
*    MOVE  icon_document    TO ls_toolbar-icon.
*    MOVE 'CREATE FILEPATH' TO ls_toolbar-quickinfo.
*    MOVE 'Create File Path' TO ls_toolbar-text.
*    MOVE ' '               TO ls_toolbar-disabled.
*    APPEND ls_toolbar      TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.
    PERFORM sub_event_ucomm USING e_ucomm.
  ENDMETHOD.                 "handle_user_command

* by ig.moon 6/1/2010 {
* Get values of possible entries
  METHOD on_f4.
    PERFORM on_f4 USING sender
                        e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display
                        'IT_AUSP1'.
  ENDMETHOD.                                                " ON_F4

  METHOD my_f4.
    PERFORM my_f4 TABLES lt_f4
                  USING  sender
                         et_bad_cells
                         es_row_no
                         er_event_data
                         e_display
                         e_fieldname
                         'GT_DTL'.
  ENDMETHOD.                                                "MY_F4
* }

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-011.
PARAMETERS: p_body RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND ucom,
            p_vin  RADIOBUTTON GROUP g1.

SELECT-OPTIONS: s_body FOR mcha-charg NO INTERVALS MODIF ID 127." Obj No
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-012.
SELECT-OPTIONS: s_vin    FOR vin_num MODIF ID 128     , " VIN No
                s_fsc    FOR full_spec_cd MODIF ID 128, " Full Spec cod
                s_shopdt FOR sy-datum MODIF ID 128    , " RP18 shopdate
                s_shipdt FOR sy-datum MODIF ID 128    , " Ship Out Date
*                S_ASKDEP FOR ASK_DEPT MODIF ID 128    , " Asking Dept
                s_askdep FOR csks-kostl MODIF ID 128    , " Asking Dept
                s_udept  FOR usage_dept MODIF ID 128  , " Dest/Using Dep
*               S_GLNO   FOR GL_ACT MODIF ID 128      , " G/L Account No
                s_glno   FOR bseg-hkont MODIF ID 128  , " G/L Account No
                s_enttyp FOR it_charg1-ent_typ MODIF ID 128,
                s_ftz FOR it_charg1-ftz_entry MODIF ID 128,
                s_dpaid FOR it_charg1-duty_paid MODIF ID 128,
                s_trem FOR it_charg1-temp_rem MODIF ID 128,
                s_rdate FOR sy-datum MODIF ID 128,
** Furong on 01/23/12
                s_yes FOR zyesno-zyes MODIF ID 128.
** end on 01/23/12
SELECTION-SCREEN END OF BLOCK b2.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = 'Test Vehicle Tracking - Change'.
  PERFORM get_characteristics.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF sy-ucomm <> 'UCOM'.
*-Body Number
    IF p_body = 'X'.
      CLEAR: s_vin[], s_vin, s_fsc[], s_fsc, s_shopdt[], s_shopdt,
             s_shipdt[], s_shipdt, s_askdep[], s_askdep, s_udept[],
             s_udept, s_glno[], s_glno, s_enttyp[], s_ftz[], s_dpaid[],
             s_trem[], s_rdate[], s_enttyp, s_ftz, s_dpaid,
             s_trem, s_rdate, s_yes, s_yes[].
      IF s_body[] IS INITIAL.
        MESSAGE e000 WITH text-013.
      ENDIF.
*-Other Characteristics
    ELSEIF p_vin = 'X'.
      CLEAR s_body[].
      IF s_vin[]    IS INITIAL AND s_fsc[]      IS INITIAL AND
         s_shopdt[] IS INITIAL AND s_shipdt[]   IS INITIAL AND
         s_askdep[] IS INITIAL AND s_udept[] IS INITIAL AND
         s_glno[]   IS INITIAL AND s_enttyp[] IS INITIAL AND
         s_ftz[]    IS INITIAL AND s_dpaid[]  IS INITIAL AND
         s_trem[]   IS INITIAL AND s_rdate[]  IS INITIAL.
        MESSAGE e000 WITH text-014.
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  PERFORM change_screen_runtime.

*----------------------------------------------------------------------*
* START OF SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_data.

  CHECK NOT it_charg[] IS INITIAL.

*-VIN Number
  IF NOT s_vin[] IS INITIAL.
    REFRESH it_vin.
    CLEAR   it_vin.
    LOOP AT s_vin.
      it_vin-vin = s_vin-low.
      APPEND it_vin.
      CLEAR  it_vin.
    ENDLOOP.
    SORT it_vin BY vin.
  ENDIF.

*-Full Spec Code
  IF NOT s_fsc[] IS INITIAL.
    REFRESH it_fsc.
    CLEAR   it_fsc.
    LOOP AT s_fsc.
      it_fsc-fsc = s_fsc-low.
      APPEND it_fsc.
      CLEAR  it_fsc.
    ENDLOOP.
    SORT it_fsc BY fsc.
  ENDIF.

*-Shop Date
  IF NOT s_shopdt[] IS INITIAL.
    REFRESH it_shop_dt.
    CLEAR   it_shop_dt.

    LOOP AT s_shopdt.
      it_shop_dt-date = s_shopdt-low.
      APPEND it_shop_dt.
      CLEAR  it_shop_dt.
    ENDLOOP.
    SORT it_shop_dt BY date.
  ENDIF.

*-Ship out date
  IF NOT s_shipdt[] IS INITIAL.
    REFRESH it_ship_dt.
    CLEAR   it_ship_dt.
    LOOP AT s_shipdt.
      it_ship_dt-date = s_shipdt-low.
      APPEND it_ship_dt.
      CLEAR  it_ship_dt.
    ENDLOOP.
    SORT it_ship_dt BY date.
  ENDIF.

*-Asking Department
  IF NOT s_askdep[] IS INITIAL.
    REFRESH it_askdep.
    CLEAR   it_askdep.
    LOOP AT s_askdep.
      it_askdep-askdept = s_askdep-low.
      APPEND it_askdep.
      CLEAR  it_askdep.
    ENDLOOP.
    SORT it_askdep BY askdept.
  ENDIF.

*-Destination/Using Department
  IF NOT s_udept[] IS INITIAL.
    REFRESH it_usagedep.
    CLEAR   it_usagedep.
    LOOP AT s_udept.
      it_usagedep-usage_dept = s_udept-low.
      APPEND it_usagedep.
      CLEAR  it_usagedep.
    ENDLOOP.
    SORT it_usagedep BY usage_dept.
  ENDIF.

*-G/L Account Number
  IF NOT s_glno[] IS INITIAL.
    REFRESH it_glno.
    CLEAR   it_glno.
    LOOP AT s_glno.
      it_glno-glno = s_glno-low.
      APPEND it_glno.
      CLEAR  it_glno.
    ENDLOOP.
    SORT it_glno BY glno.
  ENDIF.

** Changed by Furong on 02/21/08
  IF NOT s_enttyp[] IS INITIAL.
    REFRESH it_enttyp.
    CLEAR   it_enttyp.
    LOOP AT s_enttyp.
      it_enttyp-enttyp = s_enttyp-low.
      APPEND it_enttyp.
      CLEAR  it_enttyp.
    ENDLOOP.
    SORT it_enttyp BY enttyp.
  ENDIF.

  IF NOT s_ftz[] IS INITIAL.
    REFRESH it_ftz.
    CLEAR   it_ftz.
    LOOP AT s_ftz.
      it_ftz-ftz = s_ftz-low.
      APPEND it_ftz.
      CLEAR  it_ftz.
    ENDLOOP.
    SORT it_ftz BY ftz.
  ENDIF.

  IF NOT s_dpaid[] IS INITIAL.
    REFRESH it_dpaid.
    CLEAR   it_dpaid.
    LOOP AT s_dpaid.
      it_dpaid-dpaid = s_ftz-low.
      APPEND it_dpaid.
      CLEAR  it_dpaid.
    ENDLOOP.
    SORT it_dpaid BY dpaid.
  ENDIF.

  IF NOT s_trem[] IS INITIAL.
    REFRESH it_trem.
    CLEAR   it_trem.
    LOOP AT s_trem.
      it_trem-trem = s_ftz-low.
      APPEND it_trem.
      CLEAR  it_trem.
    ENDLOOP.
    SORT it_trem BY trem.
  ENDIF.

  IF NOT s_rdate[] IS INITIAL.
    REFRESH it_rdate.
    CLEAR   it_rdate.
    LOOP AT s_rdate.
      it_rdate-rdate = s_ftz-low.
      APPEND it_rdate.
      CLEAR  it_rdate.
    ENDLOOP.
    SORT it_rdate BY rdate.
  ENDIF.

** End of change

** Changed by Furong on 01/20/12
  IF NOT s_yes[] IS INITIAL.
    REFRESH it_yes.
    CLEAR   it_yes.
    LOOP AT s_yes.
      it_yes-yes = s_yes-low.
      APPEND it_yes.
      CLEAR  it_yes.
    ENDLOOP.
    SORT it_yes BY yes.
  ENDIF.
** End of change

  LOOP AT it_charg.
    PERFORM extract_original_batch.
    IF NOT it_clbatch[] IS INITIAL.
      PERFORM fill_intern_table.
    ENDIF.
  ENDLOOP.

  IF p_vin = 'X'.
*-Check for deleted records
    IF NOT it_charg1[] IS INITIAL.
      SELECT charg
           FROM mcha
           INTO TABLE it_charg2
           FOR ALL ENTRIES IN it_charg1
           WHERE matnr = c_matnr AND
                 werks = c_werks AND
                 lvorm = 'X'     AND
                 charg = it_charg1-charg.

      SORT it_charg2 BY charg.
      DELETE ADJACENT DUPLICATES FROM it_charg2 COMPARING charg.
      LOOP AT it_charg1.
        CLEAR gv_tabix.
        gv_tabix = sy-tabix.
        READ TABLE it_charg2 WITH KEY charg = it_charg1-charg.
        IF sy-subrc = 0.
          DELETE it_charg1 INDEX gv_tabix.
        ENDIF.
      ENDLOOP.

      LOOP AT it_charg2.
        CLEAR gv_txt.
        CONCATENATE text-018 it_charg2-charg text-019 INTO gv_txt
                                      SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = gv_txt.
      ENDLOOP.
      IF it_charg1[] IS INITIAL.
        EXIT.
      ENDIF.

    ELSE.
      MESSAGE i000 WITH text-001.
      LEAVE LIST-PROCESSING.
    ENDIF.      " IF NOT it_charg1[] IS INITIAL.
  ELSE.
    IF it_charg1[] IS INITIAL.
      MESSAGE i000 WITH text-001.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
  CALL SCREEN 9000.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Subroutine to get data from DB table
*----------------------------------------------------------------------*
FORM get_data.
  RANGES: r_rdate FOR ausp-atflv.
  DATA: l_date(10).

  REFRESH: it_charg, it_charg2, it_ausp_tmp.
  CLEAR:   it_charg, it_charg2, it_ausp_tmp.
*-All Batches
  IF p_body = 'X'.
    SELECT charg
           FROM mcha
           INTO TABLE it_charg
           WHERE matnr = c_matnr AND
                 werks = c_werks AND
                 charg IN s_body.

*-Deleted Batches
    SELECT charg
           FROM mcha
           INTO TABLE it_charg2
           WHERE matnr = c_matnr AND
                 werks = c_werks AND
                 lvorm = 'X'     AND
                 charg IN s_body.

*-If characteristic search is choosen
  ELSEIF p_vin = 'X'.
*-If VIN Number is entered
    IF NOT s_vin[] IS INITIAL.
      SELECT objek atwrt FROM ausp
                   INTO TABLE it_ausp_tmp
                   WHERE atinn = gv_vin AND
                         atwrt IN s_vin AND
                         klart = '022'.
    ENDIF.
*-If Full Spec Code is entered
    IF NOT s_fsc[] IS INITIAL.
      SELECT objek atwrt FROM ausp
                   APPENDING TABLE it_ausp_tmp
                   WHERE atinn = gv_fsc AND
                         atwrt IN s_fsc AND
                         klart = '022'.
    ENDIF.
*-If Asking Department is entered
    IF NOT s_askdep[] IS INITIAL.
      SELECT objek atwrt FROM ausp
                   APPENDING TABLE it_ausp_tmp
                   WHERE atinn = gv_askdept AND
                         atwrt IN s_askdep  AND
                         klart = '022'.
    ENDIF.
*-If Destination/Using Department is entered
    IF NOT s_udept[] IS INITIAL.
      SELECT objek atwrt FROM ausp
                   APPENDING TABLE it_ausp_tmp
                   WHERE atinn = gv_usagedep AND
                         atwrt IN s_udept    AND
                         klart = '022'.
    ENDIF.
** Changed by Furong on 02/21/08
    IF NOT s_enttyp[] IS INITIAL.
      SELECT objek atwrt FROM ausp
                   APPENDING TABLE it_ausp_tmp
                   WHERE atinn = gv_ent_typ AND
                         atwrt IN s_enttyp    AND
                         klart = '022'.
    ENDIF.
    IF NOT s_ftz[] IS INITIAL.
      SELECT objek atwrt FROM ausp
                   APPENDING TABLE it_ausp_tmp
                   WHERE atinn = gv_ftz_entry AND
                         atwrt IN s_ftz    AND
                         klart = '022'.
    ENDIF.
    IF NOT s_dpaid[] IS INITIAL.
      SELECT objek atwrt FROM ausp
                   APPENDING TABLE it_ausp_tmp
                   WHERE atinn = gv_duty_paid AND
                         atwrt IN s_dpaid    AND
                         klart = '022'.
    ENDIF.
    IF NOT s_trem[] IS INITIAL.
      SELECT objek atwrt FROM ausp
                   APPENDING TABLE it_ausp_tmp
                   WHERE atinn = gv_temp_rem AND
                         atwrt IN s_trem    AND
                         klart = '022'.
    ENDIF.
    IF NOT s_rdate[] IS INITIAL.
      LOOP AT s_rdate.
        r_rdate-option = s_rdate-option.
        r_rdate-sign = s_rdate-sign.
        r_rdate-low = l_date = s_rdate-low.
        r_rdate-high = l_date = s_rdate-high.
        APPEND r_rdate.
      ENDLOOP.

      SELECT objek atwrt FROM ausp
                   APPENDING TABLE it_ausp_tmp
                   WHERE atinn = gv_ret_date AND
                         atflv IN r_rdate    AND
                         klart = '022'.
    ENDIF.
** End of change on 02/21/08

*-If Ship or Shop Date is entered
    IF s_vin[]    IS INITIAL AND s_fsc[]   IS INITIAL AND
       s_askdep[] IS INITIAL AND s_udept[] IS INITIAL AND
       s_enttyp[] IS INITIAL AND s_ftz[]   IS INITIAL AND
       s_dpaid[]  IS INITIAL AND s_trem[]  IS INITIAL AND
       s_rdate[]  IS INITIAL.

      SELECT charg
             FROM mcha
             INTO TABLE it_charg
             WHERE matnr = c_matnr AND
                   werks = c_werks.
    ENDIF.

    IF it_ausp_tmp[] IS INITIAL AND it_charg[] IS INITIAL.
      MESSAGE i000 WITH text-001.
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF it_charg[] IS INITIAL.

      SORT it_ausp_tmp BY objek.
      DELETE ADJACENT DUPLICATES FROM it_ausp_tmp COMPARING objek.

      LOOP AT it_ausp_tmp.
        it_objek-cuobj = it_ausp_tmp-objek.
        APPEND it_objek.
        CLEAR  it_objek.
      ENDLOOP.

      SELECT objek FROM inob
                 INTO TABLE it_inob
                 FOR ALL ENTRIES IN it_objek
                 WHERE cuobj = it_objek-cuobj.

      LOOP AT it_inob.
        it_charg3-charg = it_inob-objek+22(10).
        APPEND it_charg3.
        CLEAR  it_charg3.
      ENDLOOP.

      SELECT charg
             FROM mcha
             INTO TABLE it_charg
             FOR ALL ENTRIES IN it_charg3
             WHERE matnr = c_matnr AND
                   werks = c_werks AND
                   charg = it_charg3-charg.
    ENDIF.      " IF it_charg[] IS INITIAL
  ENDIF.    " IF p_body = 'X'.

  IF it_charg[] IS INITIAL.
    MESSAGE i000 WITH text-001.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT it_charg BY charg.
  DELETE ADJACENT DUPLICATES FROM it_charg COMPARING charg.

  IF p_body = 'X'.
    IF NOT it_charg2[] IS INITIAL.
      SORT it_charg2 BY charg.
      DELETE ADJACENT DUPLICATES FROM it_charg2 COMPARING charg.
      LOOP AT it_charg.
        CLEAR gv_tabix.
        gv_tabix = sy-tabix.
        READ TABLE it_charg2 WITH KEY charg = it_charg-charg.
        IF sy-subrc = 0.
          DELETE it_charg INDEX gv_tabix.
        ENDIF.
      ENDLOOP.

      LOOP AT it_charg2.
        CLEAR gv_txt.
        CONCATENATE text-018 it_charg2-charg text-019 INTO gv_txt
                                      SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = gv_txt.
      ENDLOOP.
      IF it_charg[] IS INITIAL.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------
*
*&      Form  PF_STATUS
*&---------------------------------------------------------------------
*       Subroutine to Set PF Status
*----------------------------------------------------------------------
FORM pf_status USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'PF_STATUS'.

ENDFORM.                    " PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  sub_event_ucomm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_event_ucomm USING e_ucomm TYPE sy-ucomm.
  DATA:
*-Internal table for indexes of selected rows
  gi_index_rows TYPE lvc_t_row ,
*-Information about 1 row
  g_selected_row LIKE lvc_s_row,
  l_lines TYPE i             ,
  l_charg LIKE mcha-charg    ,
  l_flag(1)   TYPE c         ,
  l_succ(2)   TYPE c         ,
  l_fail(2)   TYPE c         ,
  l_text(90)  TYPE c         ,
  l_answer(1) TYPE c         ,
  l_kostl     LIKE csks-kostl,
  l_kostl1    LIKE csks-kostl  ,
  l_saknr     LIKE ska1-saknr  .

  DATA: BEGIN OF l_msg OCCURS 100 ,
          txt(100) TYPE c         ,
        END OF l_msg              .

  DATA: it_mcha LIKE mcha OCCURS 0 WITH HEADER LINE,
        it_mch1 LIKE mch1 OCCURS 0 WITH HEADER LINE,
        it_mchb LIKE mchb OCCURS 0 WITH HEADER LINE.

  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = gi_index_rows.

  DESCRIBE TABLE gi_index_rows LINES l_lines.
  IF l_lines = 0.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
        textline1 = 'You must choose a valid line'.
    EXIT.
  ENDIF.

  CASE e_ucomm.
*-Change Batch
    WHEN 'CH_BATCH'.
      CLEAR: l_succ,
             l_fail.

      LOOP AT gi_index_rows INTO g_selected_row.
        READ TABLE it_charg1 INDEX g_selected_row-index.
        IF sy-subrc = 0.
          CLEAR: gv_charg,
                 l_charg .
          gv_charg = it_charg1-charg.
          SELECT SINGLE charg
                   FROM mcha
                   INTO l_charg
                   WHERE matnr = c_matnr AND
                         werks = c_werks AND
                         charg = gv_charg.
          IF NOT l_charg IS INITIAL.
            CLEAR: csks    ,
                   l_kostl ,
                   l_kostl1.
            l_kostl = it_charg1-ask_dept.
            l_kostl1 = it_charg1-usage_dept.
            IF NOT l_kostl IS INITIAL.
              SELECT kostl UP TO 1 ROWS
                                   FROM csks
                                   INTO csks-kostl
                                   WHERE kostl = l_kostl.
              ENDSELECT.
              IF sy-subrc <> 0.
                CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
                  EXPORTING
                    textline1 = text-027.
                EXIT.
              ENDIF.
            ENDIF.
            CLEAR csks.
            IF NOT l_kostl1 IS INITIAL.
              SELECT kostl UP TO 1 ROWS
                                   FROM csks
                                   INTO csks-kostl
                                   WHERE kostl = l_kostl1.
              ENDSELECT.
              IF sy-subrc <> 0.
                CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
                  EXPORTING
                    textline1 = text-028.
                EXIT.
              ENDIF.
            ENDIF.

            l_saknr = it_charg1-gl_acct.
            IF l_saknr IS INITIAL OR l_saknr = '0'.
            ELSE.
              CLEAR ska1.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = l_saknr
                IMPORTING
                  output = l_saknr.
              SELECT saknr UP TO 1 ROWS
                                   FROM ska1
                                   INTO ska1-saknr
                                   WHERE saknr = l_saknr.
              ENDSELECT.
              IF sy-subrc <> 0.
                CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
                  EXPORTING
                    textline1 = text-029.
                EXIT.
              ENDIF.
            ENDIF.


            PERFORM build_object_key USING gv_charg.
            PERFORM extract_original_batch1.
            PERFORM update_original_batch.
            READ TABLE it_numtab WITH KEY charact = 'P_SHIPOUT_DATE'.
            IF sy-subrc = 0.
              IF NOT it_numtab-value_from IS INITIAL.
                DELETE it_numtab INDEX sy-tabix.
              ENDIF.
            ENDIF.

            PERFORM bapi_change.
            PERFORM bapi_commit.
*-Get number of success and failure records
            CLEAR l_flag.
            LOOP AT it_rettab.
              IF it_rettab-type = 'E'.
                l_flag = 'X'.
                CONCATENATE gv_charg it_rettab-message INTO
                                      l_msg SEPARATED BY ':'.
              ENDIF.
            ENDLOOP.
            IF l_flag = 'X'.
              l_fail = l_fail + 1.
              APPEND l_msg.
              CLEAR  l_msg.
            ELSE.
              l_succ = l_succ + 1.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDLOOP.

*-Status of Batches changed
      LOOP AT l_msg.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_msg-txt.
      ENDLOOP.
      IF NOT l_succ IS INITIAL.
        CLEAR l_text.
        CONCATENATE text-006 l_succ INTO l_text.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_text.
      ENDIF.
      IF NOT l_fail IS INITIAL.
        CLEAR l_text.
        CONCATENATE text-007 l_fail INTO l_text.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_text.
      ENDIF.

*-Delete Batch
    WHEN 'DL_BATCH'.
      CLEAR: l_succ,
             l_fail,
             l_flag.
      LOOP AT gi_index_rows INTO g_selected_row.
        READ TABLE it_charg1 INDEX g_selected_row-index.
        IF sy-subrc = 0.
          IF l_flag = space.
            CLEAR l_answer.
            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
              EXPORTING
                textline1      = text-010
                textline2      = ' '
                titel          = 'Check!'
                cancel_display = ' '
              IMPORTING
                answer         = l_answer.
          ENDIF.
          IF l_answer = 'J' OR l_flag = '1'.
            l_flag = '1'.
            SELECT * FROM mcha
                            INTO TABLE it_mcha
                            WHERE matnr = c_matnr AND
                                  werks = c_werks AND
                                  charg = it_charg1-charg.
            LOOP AT it_mcha.
              it_mcha-lvorm = 'X'.
              MODIFY it_mcha INDEX sy-tabix TRANSPORTING lvorm.
            ENDLOOP.

            SELECT * FROM mch1
                            INTO TABLE it_mch1
                            WHERE matnr = c_matnr AND
                                  charg = it_charg1-charg.
            LOOP AT it_mch1.
              it_mch1-lvorm = 'X'.
              MODIFY it_mch1 INDEX sy-tabix TRANSPORTING lvorm.
            ENDLOOP.

            SELECT * FROM mchb
                            INTO TABLE it_mchb
                            WHERE matnr = c_matnr AND
                                  werks = c_werks AND
                                  charg = it_charg1-charg.
            LOOP AT it_mchb.
              it_mchb-lvorm = 'X'.
              MODIFY it_mchb INDEX sy-tabix TRANSPORTING lvorm.
            ENDLOOP.

            CALL FUNCTION 'VB_UPDATE_BATCH'
              TABLES
                zmcha = it_mcha
                zmch1 = it_mch1
                zmchb = it_mchb.
            IF sy-subrc = 0.
              l_succ = l_succ + 1.
            ELSE.
              l_fail = l_fail + 1.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.  " IF l_answer = 'J' OR l_flag = '1'.
        ENDIF.    " IF sy-subrc = 0.
      ENDLOOP.    " LOOP AT gi_index_rows INTO g_selected_row

*-Status of Batches Deleted
      IF NOT l_succ IS INITIAL.
        CLEAR l_text.
        CONCATENATE text-008 l_succ INTO l_text.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_text.
      ENDIF.
      IF NOT l_fail IS INITIAL.
        CLEAR l_text.
        CONCATENATE text-009 l_fail INTO l_text.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_text.
      ENDIF.

*-Create Document
    WHEN 'CR_DOC'.
      LOOP AT gi_index_rows INTO g_selected_row.
        READ TABLE it_charg1 INDEX g_selected_row-index.
        IF sy-subrc = 0.
          vin_num = it_charg1-vin_num.
          SELECT SINGLE doknr
                       FROM draw
                       INTO gv_doknr
                       WHERE dokar = gv_dokar         AND
                             doknr = it_charg1-charg AND
                             dokvr = '00'            AND
                             doktl = '000'.
          IF sy-subrc = 0.
            CLEAR l_text.
            CONCATENATE text-015 gv_doknr text-016 INTO l_text
                                            SEPARATED BY space.
            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
              EXPORTING
                textline1      = l_text                     " text-004
                textline2      = text-017
                titel          = 'Check!'
                cancel_display = ' '
              IMPORTING
                answer         = l_answer.
            IF l_answer = 'J'.
              SET PARAMETER ID 'CV1' FIELD it_charg1-charg.
              SET PARAMETER ID 'CV2' FIELD gv_dokar.
              SET PARAMETER ID 'CV3' FIELD '00' .
              SET PARAMETER ID 'CV4' FIELD '000'.
              CALL TRANSACTION 'CV02N' AND SKIP FIRST SCREEN.
              CLEAR l_charg.
              SELECT SINGLE charg
                       FROM mcha
                       INTO l_charg
                       WHERE matnr = c_matnr AND
                             werks = c_werks AND
                             charg = it_charg1-charg.
              IF NOT l_charg IS INITIAL.
                PERFORM build_object_key USING it_charg1-charg.
                PERFORM extract_original_batch1.
                PERFORM update_original_batch1.
                PERFORM bapi_change.
                PERFORM bapi_commit.
              ENDIF.
            ENDIF.
          ELSE.
            SET PARAMETER ID 'CV1' FIELD it_charg1-charg.
            SET PARAMETER ID 'CV2' FIELD gv_dokar.
            SET PARAMETER ID 'CV3' FIELD '00' .
            SET PARAMETER ID 'CV4' FIELD '000'.
            CALL TRANSACTION 'CV01N' AND SKIP FIRST SCREEN.
            CLEAR l_charg.
            SELECT SINGLE charg
                     FROM mcha
                     INTO l_charg
                     WHERE matnr = c_matnr AND
                           werks = c_werks AND
                           charg = it_charg1-charg.
            IF NOT l_charg IS INITIAL.
              PERFORM build_object_key USING it_charg1-charg.
              PERFORM extract_original_batch1.
              PERFORM update_original_batch1.
              PERFORM bapi_change.
              PERFORM bapi_commit.
              PERFORM  create_doc_with_bapi USING it_charg1-charg.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE i000 WITH text-003.
        ENDIF.
      ENDLOOP.  " LOOP AT gi_index_rows INTO g_selected_row.

*-Create File Path
* by ig.moon 6/3/2010
    WHEN 'CR_DOC2'.
      LOOP AT gi_index_rows INTO g_selected_row.

        READ TABLE it_charg1 INDEX g_selected_row-index.
        IF sy-subrc = 0.
          CLEAR gv_doknr.
          SELECT SINGLE doknr
                       FROM draw
                       INTO gv_doknr
                       WHERE dokar = gv_dokar       AND
                             doknr = it_charg1-charg AND
                             dokvr = '00'           AND
                             doktl = '000'.
          IF sy-subrc = 0.
            PERFORM  create_doc_with_bapi USING it_charg1-charg.
          ELSE.
            MESSAGE i000 WITH 'Please create document first.'.
            EXIT.
          ENDIF.
        ELSE.
          MESSAGE i000 WITH text-003.
        ENDIF.
      ENDLOOP.  " LOOP AT gi_index_rows INTO g_selected_row.

  ENDCASE.

ENDFORM.                    " sub_event_ucomm
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'STATUS_9000'. " EXCLUDING lt_excl.
  SET TITLEBAR  'TITLE_9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       Subroutine to create custom container control & instance
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.

  DATA: wa_renewal_flg(1) TYPE c.
  IF grid_container IS INITIAL. " Not Created Container for ALV GRID
    PERFORM create_object.
    PERFORM set_layout.
    PERFORM alv_fieldcat.
    PERFORM display_alv.

  ELSEIF NOT grid_container IS INITIAL AND
             wa_renewal_flg = 'X'.

    CLEAR wa_renewal_flg.
*-Set layout
    PERFORM set_layout.

*-Create field catalog
    PERFORM alv_fieldcat.

*-Display data on ALV GRID Control using method
    PERFORM set_new_table_data.
    PERFORM refresh_alv_grid_data_disp.

    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        control = alv_grid.

  ENDIF.

ENDMODULE.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_object
*&---------------------------------------------------------------------*
*       Subroutine to create ALV Object
*----------------------------------------------------------------------*
FORM create_object.
*-Create object
  CREATE OBJECT grid_container
    EXPORTING
      container_name = custom_control.

*-Create an Instance of ALV Control
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent = grid_container.

ENDFORM.                    " create_object
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       Set Layout
*----------------------------------------------------------------------*
FORM set_layout.
  CLEAR gs_layout.
  gs_layout-language   = sy-langu.      " Language Key
  gs_layout-cwidth_opt = 'X'.
  gs_layout-sel_mode   = 'A'.
  gs_layout-box_fname  = 'CHKBOX' .
  gs_layout-edit_mode  = 'X'.

ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat
*&---------------------------------------------------------------------*
*       Prepare ALV fieldcatalogue
*----------------------------------------------------------------------*
FORM alv_fieldcat.
  DATA: l_pos TYPE i.
  CLEAR:  gs_fieldcat,
          gt_fieldcat.
  REFRESH gt_fieldcat.

  DEFINE append_fieldcat.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-scrtext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-edit          = &6.
    gs_fieldcat-fix_column    = &7.
    append gs_fieldcat to gt_fieldcat.
  END-OF-DEFINITION.

  append_fieldcat :

     ' '  'CHARG'          'Body Number'       10 'CHAR' ' ' 'X',
     ' '  'VIN_NUM'        'VIN Number'        18 'CHAR' ' ' 'X',
     ' '  'FSC'            'Full Spec Code'    20 'CHAR' ' ' ' ',
     ' '  'EXT_CLR'        'Exterior Color'    14 'CHAR' ' ' ' ',
     ' '  'INT_CLR'        'Interior Color'    14 'CHAR' ' ' ' ',
     ' '  'RP18_SHOP_DATE' 'RP18 shopdate'     13 'CHAR' ' ' ' ',
     ' '  'CAR_TYPE'       'Test Car Type'     13 'CHAR' ' ' ' ',
     ' '  'USAGE'          'Usage'              5 'CHAR' ' ' ' ',
* by ig.moon 6/1/2010 {
     ' '  'VTYPE'          'Use Type'       1 'CHAR' 'X' ' ',
* }
** Changed by Furong on 06/21/10
     ' '  'DOC_PVT'        'PVT'            1 'CHAR' 'X' ' ',
     ' '  'DOC_TRF'        'TRF'            1 'CHAR' 'X' ' ',
     ' '  'DOC_MERAF'      'MERAF'          1 'CHAR' 'X' ' ',
     ' '  'DOC_BOL'        'BOL'            1 'CHAR' 'X' ' ',
** End of change


** Changed by Furong on 02/20/08  "UD1K942932
     ' '  'ENT_TYP'        'Entry Type'        10 'CHAR' 'X' ' ',
     ' '  'FTZ_ENTRY'      'FTZ Entry   '      13 'CHAR' 'X' ' ',
     ' '  'DUTY_PAID'      'Duty Paid'         10 'CHAR' 'X' ' ',
     ' '  'DUTY_AMT'       'Duty Amount'       11 'CHAR' 'X' ' ',
** End of change
     ' '  'USAGE_TEXT'     'Usage Text'        30 'CHAR' 'X' ' ',
     ' '  'ASK_DIV'        'Asking Division'   20 'CHAR' 'X' ' ',
     ' '  'ASK_DEPT'       'Asking Dept.'      10 'CHAR' ' ' ' ',
     ' '  'USAGE_DEPT'     'Using Dept.'       11 'CHAR' ' ' ' ',
     ' '  'REF_DOC'        'Reference Doc #'   15 'CHAR' ' ' ' ',
     ' '  'FINAL_DEST'     'Final Destination' 15 'CHAR' 'X' ' ',
** Changed by Furong on 02/2/08  "UD1K942932
    ' '  'TEMP_REM'        'Temp.Rem'          17 'CHAR' 'X' ' ',
    ' '  'RET_DATE'        'Return Date'       10 'CHAR' 'X' ' ',
** End of cahnge on 02/20/08
     ' '  'SHIP_DATE'      'Ship Out Date'     13 'CHAR' 'X' ' ',
     ' '  'SHIP_DOCS'      'Ship Out Doc'      12 'CHAR' 'X' ' ',
     ' '  'GL_ACCT'        'G/L Account'       11 'NUMC' 'X' ' ',
     ' '  'ACC_DOC'        'Accounting Doc #'  16 'CHAR' 'X' ' ',
     ' '  'INVOICE'        'Invoice Doc #'     13 'NUMC' 'X' ' ',

** Changed by Furong on 01/20/12
    ' '  'YES'             'Y/N'               5  'CHAR' 'X' ' ',
    ' '  'DESC'            'Description'       30 'CHAR' 'X' ' '.
** End on 01/21/12
  LOOP AT gt_fieldcat.
    IF gt_fieldcat-fieldname = 'ASK_DEPT'.
    ELSEIF gt_fieldcat-fieldname = 'GL_ACCT'.
      gs_fieldcat-edit = ' '.
      gt_fieldcat-f4availabl = 'X'.
      gt_fieldcat-ref_field = 'SAKNR'.
      gt_fieldcat-ref_table = 'SKA1'.
      MODIFY gt_fieldcat INDEX sy-tabix TRANSPORTING edit f4availabl
                                         ref_field ref_table.
    ELSEIF gt_fieldcat-fieldname = 'USAGE_DEPT'.
*      gs_fieldcat-edit = ' '.
*      gt_fieldcat-f4availabl = 'X'.
*      gt_fieldcat-ref_field = 'KOSTL'.
*      gt_fieldcat-ref_table = 'CSKS'.
*      MODIFY gt_fieldcat INDEX sy-tabix TRANSPORTING edit f4availabl
*                                         ref_field ref_table.
    ENDIF.

* by ig.moon 6/1/2010 {
    IF gt_fieldcat-fieldname = 'VTYPE'.
      gs_fieldcat-edit = ' '.
      gt_fieldcat-f4availabl = 'X'.
      MODIFY gt_fieldcat INDEX sy-tabix TRANSPORTING edit f4availabl
                                         ref_field ref_table.
    ENDIF.

* on 01/23/2012 {
    IF gt_fieldcat-fieldname = 'YES'.
      gs_fieldcat-edit = ' '.
      gt_fieldcat-f4availabl = 'X'.
      gt_fieldcat-ref_field = 'ZYES'.
      gt_fieldcat-ref_table = 'ZYESNO'.

      MODIFY gt_fieldcat INDEX sy-tabix TRANSPORTING edit f4availabl
                                         ref_field ref_table.
    ENDIF.
** end

* }
*    IF GT_FIELDCAT-FIELDNAME = 'DOC_PVT' OR
*       GT_FIELDCAT-FIELDNAME = 'DOC_TRF' OR
*       GT_FIELDCAT-FIELDNAME = 'DOC_MERAF' OR
*       GT_FIELDCAT-FIELDNAME = 'DOC_BOL'.
*      GS_FIELDCAT-EDIT = ' '.
*      GT_FIELDCAT-F4AVAILABL = 'X'.
*      MODIFY GT_FIELDCAT INDEX SY-TABIX TRANSPORTING EDIT F4AVAILABL
*                                         REF_FIELD REF_TABLE.
*    ENDIF.
*
  ENDLOOP.

* by ig.moon 6/1/2010 {
  PERFORM create_f4_fields USING 'ALV_GRID'.
* }


ENDFORM.                    " alv_fieldcat
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       Subroutine to display ALV Grid data
*----------------------------------------------------------------------*
FORM display_alv.
  DATA: mt_excluding_toolbar  TYPE ui_functions,
        l_func TYPE ui_func.
  REFRESH mt_excluding_toolbar[].
  APPEND '&ABC'   TO mt_excluding_toolbar.
  APPEND '&INFO'  TO mt_excluding_toolbar.
  APPEND '&UMC'   TO mt_excluding_toolbar.
  APPEND '&SUM'   TO mt_excluding_toolbar.
  APPEND '&GRAPH' TO mt_excluding_toolbar.
  APPEND '&ILD' TO mt_excluding_toolbar.
  APPEND '&LOCAL&APPEND' TO mt_excluding_toolbar.
  APPEND '&LOCAL&COPY' TO mt_excluding_toolbar.
  APPEND '&LOCAL&COPY_ROW' TO mt_excluding_toolbar.
  APPEND '&LOCAL&CUT' TO mt_excluding_toolbar.
  APPEND '&LOCAL&DELETE_ROW' TO mt_excluding_toolbar.
  APPEND '&LOCAL&INSERT_ROW' TO mt_excluding_toolbar.

  SORT it_charg1 BY charg.
*-Display data on ALV GRID Control
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      i_structure_name     = 'IT_CHARG1'
      is_layout            = gs_layout
      i_save               = gv_save
      is_variant           = gv_variant
      i_default            = 'X'
      it_toolbar_excluding = mt_excluding_toolbar
    CHANGING
      it_fieldcatalog      = gt_fieldcat[]
      it_outtab            = it_charg1[].
*            IT_SORT               = <internal table of type LVC_T_SORT>
*            IT_FILTER             = <internal table of type LVC_T_FILT>

*-Create Object to receive events and link them to handler methods.
*-When ALV Control raises the event for the specified instance,
*-corresponding method is automatically called.
  CREATE OBJECT event_receiver.

  SET HANDLER event_receiver->handle_toolbar       FOR alv_grid.
  SET HANDLER event_receiver->handle_user_command  FOR alv_grid.
  SET HANDLER event_receiver->on_f4                FOR alv_grid.

*-Call method 'set_toolbar_interactive' to raise event TOOLBAR.
  CALL METHOD alv_grid->set_toolbar_interactive.

  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = alv_grid.

ENDFORM.                    " display_alv
*&---------------------------------------------------------------------*
*&      Form  SET_NEW_TABLE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_new_table_data.

ENDFORM.                    " SET_NEW_TABLE_DATA
*&---------------------------------------------------------------------*
*&      Form  refresh_alv_grid_data_disp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_alv_grid_data_disp.

ENDFORM.                    " refresh_alv_grid_data_disp
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       Module to exit from screen
*----------------------------------------------------------------------*
MODULE exit INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'EXIT'.

      PERFORM free_alv_grid.
      LEAVE TO SCREEN 0.

    WHEN 'RW'.

      PERFORM free_alv_grid.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Form  free_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM free_alv_grid.
  CALL METHOD alv_grid->free.
  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    gv_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = gv_repid
        txt2  = sy-subrc
        txt1  = text-002.
  ENDIF.

ENDFORM.                    " free_alv_grid
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'RW'.

      PERFORM free_alv_grid.
      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  create_batch
*&---------------------------------------------------------------------*
*       Subroutine to create batch
*----------------------------------------------------------------------*
FORM create_batch.
  CLEAR:   it_rettab  .
  REFRESH: it_rettab  .
  gv_charg = it_charg1-charg.
*-Create the batch using screen values selected by the user
  CALL FUNCTION 'BAPI_BATCH_CREATE'
    EXPORTING
      material             = c_matnr
      batch                = gv_charg
      plant                = c_werks
      batchstoragelocation = space
    TABLES
      return               = it_rettab.

ENDFORM.                    " create_batch
*&---------------------------------------------------------------------*
*&      Form  bapi_commit
*&---------------------------------------------------------------------*
*       Subroutine to Commit BAPI changes
*----------------------------------------------------------------------*
FORM bapi_commit.
*-Commit the changes
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
ENDFORM.                    " bapi_commit
*&---------------------------------------------------------------------*
*&      Form  update_original_batch
*&---------------------------------------------------------------------*
*       Sub. to update newly created Batch with char name and values
*----------------------------------------------------------------------*
FORM update_original_batch.
  DATA: l_dktxt LIKE drat-dktxt,
        l_dktxt1(15) TYPE c.
  READ TABLE it_cabn WITH KEY atnam = 'P_VIN'.      " VIN Number
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                 it_charg1-vin_num.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_FSC'.  " Full Spec code
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam it_charg1-fsc.

  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_EXT_COLOR'.  " Exterior Color
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                     it_charg1-ext_clr.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_INT_COLOR'.  " Interior Color
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                      it_charg1-int_clr.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_USAGE_CAR'.  " Usage Car
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                        it_charg1-usage.
  ENDIF.

* ig.moon 6/3/2010 {
  READ TABLE it_cabn WITH KEY atnam = 'P_USED_FOR'.  " Use Type
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                           it_charg1-vtype.
  ENDIF.
* }

  READ TABLE it_cabn WITH KEY atnam = 'P_USAGE_TEXT'.  " Usage text
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                   it_charg1-usage_text.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_ASK_DIV'.  " Asking Division
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                      it_charg1-ask_div.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_ASK_DEPT'.  " Asking Dept
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                     it_charg1-ask_dept.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_USAGE_DEPT'." Usage Dept
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                   it_charg1-usage_dept.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_REF_DOC'.  " Reference Document
  IF sy-subrc = 0.
    SELECT SINGLE dktxt FROM drat
                  INTO l_dktxt
                       WHERE dokar = gv_dokar AND
                             doknr = it_charg1-charg AND
                             dokvr = '00'     AND
                             doktl = '000'    AND
                             langu = sy-langu.
    IF NOT l_dktxt IS INITIAL.
      l_dktxt1 = l_dktxt.
      PERFORM update_table USING it_cabn-atfor it_cabn-atnam l_dktxt1.
    ENDIF.
  ENDIF.

  READ TABLE it_cabn WITH KEY atnam = 'P_FINAL_DEST'.  " Final Dest
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                   it_charg1-final_dest.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_SHIPOUT_DOC'.  " Ship Out Doc
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                    it_charg1-ship_docs.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_RP18_SHOP_DATE'. " RP18 shopdt
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                              it_charg1-rp18_shop_date.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_SHIPOUT_DATE'. " Ship Out Dt
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                it_charg1-ship_date.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_GL_ACCOUNT'.  " GL Account
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                      it_charg1-gl_acct.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_ACC_DOC'.     " Accounting Doc
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                     it_charg1-acc_doc.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_INV_NO'.  " Invoice Number
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                     it_charg1-invoice.
  ENDIF.
**Changed by Furong on 02/20/08  "UD1K942932
  READ TABLE it_cabn WITH KEY atnam = 'P_ENT_TYP'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-ent_typ.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_FTZ_ENTRY'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-ftz_entry.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DUTY_PAID'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-duty_paid.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DUTY_AMT'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-duty_amt.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_TEMP_REM'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-temp_rem.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_RET_DATE'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-ret_date.
  ENDIF.
**End of change on 02/20/08  "UD1K942932

**Changed by Furong on 06/21/10
  READ TABLE it_cabn WITH KEY atnam = 'P_DOC_PVT'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-doc_pvt.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DOC_TRF'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-doc_trf.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DOC_MERAF'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-doc_meraf.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DOC_BOL'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-doc_bol.
  ENDIF.
** end of change

**Changed by Furong on 01/20/12
  READ TABLE it_cabn WITH KEY atnam = 'P_YES'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-yes.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DESCRIPTION'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_charg1-desc.
  ENDIF.
** eND ON 01/20/12
ENDFORM.                    " update_original_batch
*&---------------------------------------------------------------------*
*&      Form  get_characteristics
*&---------------------------------------------------------------------*
*       Subroutine to get characterstic names
*----------------------------------------------------------------------*
FORM get_characteristics.
*-Extract the characteristic names for KLART/CLASS
  CLEAR: klah, it_cabn.
  REFRESH it_cabn.

  SELECT SINGLE clint FROM klah
                INTO klah-clint
                WHERE klart = c_klart AND
                      class = c_class.

  IF syst-subrc = 0.
    SELECT imerk FROM ksml
                  INTO TABLE it_ksml
                  WHERE clint = klah-clint.
    IF sy-subrc = 0.
      SELECT atinn atnam atfor
                   FROM cabn
                   INTO TABLE it_cabn
                   FOR ALL ENTRIES IN it_ksml
                   WHERE atinn = it_ksml-imerk.
      IF sy-subrc = 0.
        LOOP AT it_cabn.
          IF it_cabn-atnam = 'P_VIN'.               " VIN Number
            gv_vin = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_FSC'.           " Full Spec code
            gv_fsc = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_ASK_DEPT'.      " Asking Dept
            gv_askdept = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_USAGE_DEPT'.    " Dest/Using Dept
            gv_usagedep = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_GL_ACCOUNT'.    " GL Account
            gv_glno = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_ACC_DOC'.       " Accounting Doc
            gv_acc_doc = it_cabn-atinn.
**Changed by Furong on 02/20/08  "UD1K942932
          ELSEIF it_cabn-atnam = 'P_ENT_TYP'.
            gv_ent_typ = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_FTZ_ENTRY'.
            gv_ftz_entry = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_DUTY_PAID'.
            gv_duty_paid = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_DUTY_AMT'.
            gv_duty_amt = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_TEMP_REM'.
            gv_temp_rem = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_RET_DATE'.
            gv_ret_date = it_cabn-atinn.
**End of change on 02/20/08  "UD1K942932

**Changed by Furong on 06/21/10
          ELSEIF it_cabn-atnam = 'P_DOC_PVT'.
            gv_doc_pvt = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_DOC_TRF'.
            gv_doc_trf = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_DOC_MERAF'.
            gv_doc_meraf = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_DOC_BOL'.
            gv_doc_bol = it_cabn-atinn.
**End of change on 06/21/10

* by ig.moon 6/3/2010 {
          ELSEIF it_cabn-atnam = 'P_USED_FOR'.
            gv_used_for = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_TEST_DOC'.
            gv_test_doc = it_cabn-atinn.
* }

          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDIF.
ENDFORM.                    " get_characteristics
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CABN_ATFOR  text
*      -->P_IT_CABN_ATNAM  text
*      -->P_1727   text
*----------------------------------------------------------------------*
FORM update_table USING atfor atnam value.
*-Depending on data format, start building the characteristics table
*-ready for update
  CASE atfor.
    WHEN 'NUM'.
      CLEAR it_numtab.
      READ TABLE it_numtab WITH KEY charact = atnam.
      IF syst-subrc = 0.
        it_numtab-value_from = value.
        MODIFY it_numtab INDEX syst-tabix.
      ELSE.
        it_numtab-charact = atnam.
        it_numtab-value_from = value.
        APPEND it_numtab.
      ENDIF.
    WHEN 'CURR'.
      CLEAR it_curtab.
      READ TABLE it_curtab WITH KEY charact = atnam.
      IF syst-subrc = 0.
        it_curtab-value_from = value.
        MODIFY it_curtab INDEX syst-tabix.
      ELSE.
        it_curtab-charact = atnam.
        it_curtab-value_from = value.
        APPEND it_curtab.
      ENDIF.

    WHEN 'CHAR' OR 'DATE'.
      CLEAR it_chatab.
      READ TABLE it_chatab WITH KEY charact = atnam.
      IF syst-subrc = 0.
        it_chatab-value_neutral = value.
** Added by Furong on 06/22/10
        it_chatab-value_char = value.
** End of change
        MODIFY it_chatab INDEX syst-tabix.
      ELSE.
        it_chatab-charact = atnam.
        it_chatab-value_neutral = value.
** Added by Furong on 06/22/10
        it_chatab-value_char = value.
** End of change
        APPEND it_chatab.
      ENDIF.
  ENDCASE.

ENDFORM.                    " update_table

*&---------------------------------------------------------------------*
*&      Form  bapi_change
*&---------------------------------------------------------------------*
*       Subroutine to update Batch with characteristic values
*----------------------------------------------------------------------*
FORM bapi_change.

*-Apply the characteristics to the batch.
  CALL FUNCTION 'BAPI_OBJCL_CHANGE'
    EXPORTING
      objectkey          = gv_object
      objecttable        = 'MCHA'
      classnum           = c_class
      classtype          = c_klart
    TABLES
      allocvaluesnumnew  = it_numtab
      allocvaluescharnew = it_chatab
      allocvaluescurrnew = it_curtab
      return             = it_rettab.

ENDFORM.                    " bapi_change
*&---------------------------------------------------------------------*
*&      Form  extract_original_batch
*&---------------------------------------------------------------------*
*       Subroutine to get Characteristic values for batches
*----------------------------------------------------------------------*
FORM extract_original_batch.

*-Extract the original characteristic data if exists
  CLEAR:   it_numtab, it_chatab, it_curtab, it_rettab.
  REFRESH: it_numtab, it_chatab, it_curtab, it_rettab.

  CALL FUNCTION 'VB_BATCH_GET_DETAIL'
    EXPORTING
      matnr                    = c_matnr
      charg                    = it_charg-charg
      werks                    = c_werks
      get_classification       = 'X'
*   EXISTENCE_CHECK          =
*   READ_FROM_BUFFER         =
*   NO_CLASS_INIT            = ' '
*   LOCK_BATCH               = ' '
* IMPORTING
*   YMCHA                    =
*   CLASSNAME                =
   TABLES
     char_of_batch            = it_clbatch
   EXCEPTIONS
     no_material              = 1
     no_batch                 = 2
     no_plant                 = 3
     material_not_found       = 4
     plant_not_found          = 5
     no_authority             = 6
     batch_not_exist          = 7
     lock_on_batch            = 8
     OTHERS                   = 9.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " extract_original_batch
*&---------------------------------------------------------------------*
*&      Form  fill_intern_table
*&---------------------------------------------------------------------*
*       Populate internal table with batch and char values
*----------------------------------------------------------------------*
FORM fill_intern_table.
  DATA: l_date    LIKE sy-datum.

  it_charg1-charg = it_charg-charg.

  READ TABLE it_clbatch WITH KEY atnam = 'P_VIN'.
  IF sy-subrc = 0.
    it_charg1-vin_num = it_clbatch-atwtb.
  ELSE.
    it_charg1-vin_num = space.
  ENDIF.

*-Validations for VIN Number
  IF s_vin-low IS INITIAL AND s_vin-high IS INITIAL.
  ELSEIF NOT ( s_vin-low IS INITIAL ) AND s_vin-high IS INITIAL.
    READ TABLE it_vin WITH KEY vin = it_charg1-vin_num BINARY SEARCH.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ELSEIF s_vin-low IS INITIAL AND NOT ( s_vin-high IS INITIAL ).
    IF it_charg1-vin_num > s_vin-high.
      EXIT.
    ENDIF.
  ELSEIF NOT ( s_vin-low IS INITIAL AND s_vin-high IS INITIAL ).
    IF ( it_charg1-vin_num < s_vin-low OR it_charg1-vin_num > s_vin-high ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_FSC'.
  IF sy-subrc = 0.
    it_charg1-fsc = it_clbatch-atwtb.
  ELSE.
    it_charg1-fsc = space.
  ENDIF.

*-Validations for Full spec code
  IF s_fsc-low IS INITIAL AND s_fsc-high IS INITIAL.
  ELSEIF NOT ( s_fsc-low IS INITIAL ) AND s_fsc-high IS INITIAL.
    READ TABLE it_fsc WITH KEY fsc = it_charg1-fsc BINARY SEARCH.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ELSEIF s_fsc-low IS INITIAL AND NOT ( s_fsc-high IS INITIAL ).
    IF it_charg1-fsc > s_fsc-high.
      EXIT.
    ENDIF.
  ELSEIF NOT ( s_fsc-low IS INITIAL AND s_fsc-high IS INITIAL ).
    IF ( it_charg1-fsc < s_fsc-low OR it_charg1-fsc > s_fsc-high ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_RP18_SHOP_DATE'.
  IF sy-subrc = 0.
    it_charg1-rp18_shop_date = it_clbatch-atwtb.
  ELSE.
    it_charg1-rp18_shop_date = space.
  ENDIF.

*-Validations for Shop Date
  CLEAR l_date.
  IF NOT it_charg1-rp18_shop_date IS INITIAL.
    PERFORM user_specific_date USING  it_charg1-rp18_shop_date
                               CHANGING l_date.
  ENDIF.
  IF s_shopdt-low IS INITIAL AND s_shopdt-high IS INITIAL.
  ELSEIF NOT ( s_shopdt-low IS INITIAL ) AND s_shopdt-high IS INITIAL.
    READ TABLE it_shop_dt WITH KEY date = l_date BINARY SEARCH.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ELSEIF s_shopdt-low IS INITIAL AND NOT ( s_shopdt-high IS INITIAL ).
    IF l_date > s_shopdt-high.
      EXIT.
    ENDIF.
  ELSEIF NOT ( s_shopdt-low IS INITIAL AND s_shopdt-high IS INITIAL ).
    IF ( l_date < s_shopdt-low OR l_date > s_shopdt-high ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_SHIPOUT_DATE'.
  IF sy-subrc = 0.
    it_charg1-ship_date = it_clbatch-atwtb.
  ELSE.
    it_charg1-ship_date = space.
  ENDIF.

*-Validations for Ship out Date
  CLEAR l_date.
  IF NOT it_charg1-ship_date IS INITIAL.
    PERFORM user_specific_date USING  it_charg1-ship_date
                               CHANGING l_date.
  ENDIF.
  IF s_shipdt-low IS INITIAL AND s_shipdt-high IS INITIAL.
  ELSEIF NOT ( s_shipdt-low IS INITIAL ) AND s_shipdt-high IS INITIAL.
    READ TABLE it_ship_dt WITH KEY date = l_date BINARY SEARCH.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ELSEIF s_shipdt-low IS INITIAL AND NOT ( s_shipdt-high IS INITIAL ).
    IF l_date > s_shipdt-high.
      EXIT.
    ENDIF.
  ELSEIF NOT ( s_shipdt-low IS INITIAL AND s_shipdt-high IS INITIAL ).
    IF ( l_date < s_shipdt-low OR l_date > s_shipdt-high ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_ASK_DEPT'.
  IF sy-subrc = 0.
    it_charg1-ask_dept = it_clbatch-atwtb.
  ELSE.
    it_charg1-ask_dept = space.
  ENDIF.

*-Validations for Asking Department
  IF s_askdep-low IS INITIAL AND s_askdep-high IS INITIAL.
  ELSEIF NOT ( s_askdep-low IS INITIAL ) AND s_askdep-high IS INITIAL.
    READ TABLE it_askdep WITH KEY askdept = it_charg1-ask_dept
                                                 BINARY SEARCH.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ELSEIF s_askdep-low IS INITIAL AND NOT ( s_askdep-high IS INITIAL ).
    IF it_charg1-ask_dept > s_askdep-high.
      EXIT.
    ENDIF.
  ELSEIF NOT ( s_askdep-low IS INITIAL AND s_askdep-high IS INITIAL ).
    IF ( it_charg1-ask_dept < s_askdep-low OR
                                  it_charg1-ask_dept > s_askdep-high ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_FINAL_DEST'.
  IF sy-subrc = 0.
    it_charg1-final_dest = it_clbatch-atwtb.
  ELSE.
    it_charg1-final_dest = space.
  ENDIF.

*-Validations for Destination/Using Department
  READ TABLE it_clbatch WITH KEY atnam = 'P_USAGE_DEPT'.
  IF sy-subrc = 0.
    it_charg1-usage_dept = it_clbatch-atwtb.
  ENDIF.

  IF s_udept-low IS INITIAL AND s_udept-high IS INITIAL.
  ELSEIF NOT ( s_udept-low IS INITIAL ) AND
                                   s_udept-high IS INITIAL.
    READ TABLE it_usagedep WITH KEY usage_dept = it_charg1-usage_dept
                                             BINARY SEARCH.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ELSEIF s_udept-low IS INITIAL AND
                          NOT ( s_udept-high IS INITIAL ).
    IF it_charg1-usage_dept > s_udept-high.
      EXIT.
    ENDIF.
  ELSEIF NOT ( s_udept-low IS INITIAL AND
                                s_udept-high IS INITIAL ).
    IF ( it_charg1-usage_dept < s_udept-low OR
                    it_charg1-usage_dept > s_udept-high ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_GL_ACCOUNT'.
  IF sy-subrc = 0.
    it_charg1-gl_acct = it_clbatch-atwtb.
  ELSE.
    it_charg1-gl_acct = space.
  ENDIF.

*-Validations for G/L Account
  IF s_glno-low IS INITIAL AND s_glno-high IS INITIAL.
  ELSEIF NOT ( s_glno-low IS INITIAL ) AND s_glno-high IS INITIAL.
    READ TABLE it_glno WITH KEY glno = it_charg1-gl_acct BINARY SEARCH.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ELSEIF s_glno-low IS INITIAL AND NOT ( s_glno-high IS INITIAL ).
    IF it_charg1-gl_acct > s_glno-high.
      EXIT.
    ENDIF.
  ELSEIF NOT ( s_glno-low IS INITIAL AND s_glno-high IS INITIAL ).
    IF ( it_charg1-gl_acct < s_glno-low OR
                                 it_charg1-gl_acct > s_glno-high ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_EXT_COLOR'.
  IF sy-subrc = 0.
    it_charg1-ext_clr = it_clbatch-atwtb.
  ELSE.
    it_charg1-ext_clr = space.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_INT_COLOR'.
  IF sy-subrc = 0.
    it_charg1-int_clr = it_clbatch-atwtb.
  ELSE.
    it_charg1-int_clr = space.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_USAGE_CAR'.
  IF sy-subrc = 0.
    it_charg1-usage = it_clbatch-atwtb.
  ELSE.
    it_charg1-usage = space.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_USED_FOR'.
  IF sy-subrc = 0.
    it_charg1-vtype = it_clbatch-atwtb.
  ELSE.
    it_charg1-vtype = space.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_USAGE_TEXT'.
  IF sy-subrc = 0.
    it_charg1-usage_text = it_clbatch-atwtb.
  ELSE.
    it_charg1-usage_text = space.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_ASK_DIV'.
  IF sy-subrc = 0.
    it_charg1-ask_div = it_clbatch-atwtb.
  ELSE.
    it_charg1-ask_div = space.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_ACC_DOC'.
  IF sy-subrc = 0.
    it_charg1-acc_doc = it_clbatch-atwtb.
  ELSE.
    it_charg1-acc_doc = space.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_REF_DOC'.
  IF sy-subrc = 0.
    it_charg1-ref_doc = it_clbatch-atwtb.
  ELSE.
    it_charg1-ref_doc = space.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_SHIPOUT_DOC'.
  IF sy-subrc = 0.
    it_charg1-ship_docs = it_clbatch-atwtb.
  ELSE.
    it_charg1-ship_docs = space.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_INV_NO'.
  IF sy-subrc = 0.
    it_charg1-invoice = it_clbatch-atwtb.
  ELSE.
    it_charg1-invoice = space.
  ENDIF.

**Changed by Furong on 02/20/08  "UD1K942932
  READ TABLE it_clbatch WITH KEY atnam = 'P_ENT_TYP'.
  IF sy-subrc = 0.
    it_charg1-ent_typ = it_clbatch-atwtb.
  ELSE.
    it_charg1-ent_typ = space.
  ENDIF.
  READ TABLE it_clbatch WITH KEY atnam = 'P_FTZ_ENTRY'.
  IF sy-subrc = 0.
    it_charg1-ftz_entry = it_clbatch-atwtb.
  ELSE.
    it_charg1-ftz_entry = space.
  ENDIF.
  READ TABLE it_clbatch WITH KEY atnam = 'P_DUTY_PAID'.
  IF sy-subrc = 0.
    it_charg1-duty_paid = it_clbatch-atwtb.
  ELSE.
    it_charg1-duty_paid = space.
  ENDIF.
  READ TABLE it_clbatch WITH KEY atnam = 'P_DUTY_AMT'.
  IF sy-subrc = 0.
    it_charg1-duty_amt = it_clbatch-atwtb.
  ELSE.
    it_charg1-duty_amt = space.
  ENDIF.
  READ TABLE it_clbatch WITH KEY atnam = 'P_TEMP_REM'.
  IF sy-subrc = 0.
    it_charg1-temp_rem = it_clbatch-atwtb.
  ELSE.
    it_charg1-temp_rem = space.
  ENDIF.
  READ TABLE it_clbatch WITH KEY atnam = 'P_RET_DATE'.
  IF sy-subrc = 0.
    it_charg1-ret_date = it_clbatch-atwtb.
  ELSE.
    it_charg1-ret_date = space.
  ENDIF.
**End of change on 02/20/08  "UD1K942932

**Changed by Furong on 06/21/10
  READ TABLE it_clbatch WITH KEY atnam = 'P_DOC_PVT'.
  IF sy-subrc = 0.
    IF it_clbatch-atwtb+0(1) = 'E'.
      it_charg1-doc_pvt = 'Y'.
    ELSE.
      it_charg1-doc_pvt = it_clbatch-atwtb.
    ENDIF.
  ELSE.
    it_charg1-doc_pvt = space.
  ENDIF.
  READ TABLE it_clbatch WITH KEY atnam = 'P_DOC_TRF'.
  IF sy-subrc = 0.
    IF it_clbatch-atwtb+0(1) = 'E'.
      it_charg1-doc_trf = 'Y'.
    ELSE.
      it_charg1-doc_trf = it_clbatch-atwtb.
    ENDIF.
  ELSE.
    it_charg1-doc_trf = space.
  ENDIF.
  READ TABLE it_clbatch WITH KEY atnam = 'P_DOC_MERAF'.
  IF sy-subrc = 0.
    IF it_clbatch-atwtb+0(1) = 'E'.
      it_charg1-doc_meraf = 'Y'.
    ELSE.
      it_charg1-doc_meraf = it_clbatch-atwtb.
    ENDIF.
  ELSE.
    it_charg1-doc_meraf = space.
  ENDIF.
  READ TABLE it_clbatch WITH KEY atnam = 'P_DOC_BOL'.
  IF sy-subrc = 0.
    IF it_clbatch-atwtb+0(1) = 'E'.
      it_charg1-doc_bol =  'Y'.
    ELSE.
      it_charg1-doc_bol = it_clbatch-atwtb.
    ENDIF.
  ELSE.
    it_charg1-doc_bol = space.
  ENDIF.
** End of change

**Changed by Furong on 01/20/12
  READ TABLE it_clbatch WITH KEY atnam = 'P_YES'.
  IF sy-subrc = 0.
    it_charg1-yes = it_clbatch-atwtb.
  ELSE.
    it_charg1-yes = space.
  ENDIF.

*-Validations for Y/N
  IF s_yes-low IS INITIAL AND s_yes-high IS INITIAL.
  ELSEIF NOT ( s_yes-low IS INITIAL ) AND
                                   s_yes-high IS INITIAL.
    READ TABLE it_yes WITH KEY yes = it_charg1-yes
                                             BINARY SEARCH.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ELSEIF s_yes-low IS INITIAL AND
                          NOT ( s_yes-high IS INITIAL ).
    IF it_charg1-yes > s_yes-high.
      EXIT.
    ENDIF.
  ELSEIF NOT ( s_yes-low IS INITIAL AND
                                s_yes-high IS INITIAL ).
    IF ( it_charg1-yes < s_yes-low OR
                    it_charg1-yes > s_yes-high ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE it_clbatch WITH KEY atnam = 'P_DESCRIPTION'.
  IF sy-subrc = 0.
    it_charg1-desc = it_clbatch-atwtb.
  ELSE.
    it_charg1-desc = space.
  ENDIF.
** end on 01/20/12

  it_charg1-car_type = 'XA'.

  APPEND it_charg1.
  CLEAR  it_charg1.
ENDFORM.                    " fill_intern_table
*&---------------------------------------------------------------------*
*&      Form  build_object_key
*&---------------------------------------------------------------------*
*       Subroutine to build Object Key
*----------------------------------------------------------------------*
*      -->P_CHARG  Batch Number
*----------------------------------------------------------------------*
FORM build_object_key USING p_charg TYPE mcha-charg.
*-Build the object key
  CLEAR:   it_object, it_rettab, gv_object.
  REFRESH: it_object, it_rettab.

  it_object-key_field = 'MATNR'.
  it_object-value_int = c_matnr.
  APPEND it_object.

  it_object-key_field = 'WERKS'.
  it_object-value_int = c_werks.
  APPEND it_object.

  it_object-key_field = 'CHARG'.
  it_object-value_int = p_charg.
  APPEND it_object.

  CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
    EXPORTING
      objecttable    = 'MCHA'
    IMPORTING
      objectkey_conc = gv_object
    TABLES
      objectkeytable = it_object
      return         = it_rettab.

ENDFORM.                    " build_object_key
*&---------------------------------------------------------------------*
*&      Form  change_screen_runtime
*&---------------------------------------------------------------------*
*       Dynamic screen display
*----------------------------------------------------------------------*
FORM change_screen_runtime.
  LOOP AT SCREEN.
    IF p_body = 'X'.
*-Make VIN Number invisible
      IF screen-group1 = '128'.
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
*-Make Body Number invisible
    IF p_vin = 'X'.
      IF screen-group1 = '127'.
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " change_screen_runtime
*&---------------------------------------------------------------------*
*&      Form  extract_original_batch1
*&---------------------------------------------------------------------*
*       Subroutine to get Characteristic values for batches
*----------------------------------------------------------------------*
FORM extract_original_batch1.

*-Extract the original characteristic data if exists
  CLEAR:   it_numtab, it_chatab, it_curtab, it_rettab.
  REFRESH: it_numtab, it_chatab, it_curtab, it_rettab.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey       = gv_object
      objecttable     = 'MCHA'
      classnum        = c_class
      classtype       = c_klart
    TABLES
      allocvaluesnum  = it_numtab
      allocvalueschar = it_chatab
      allocvaluescurr = it_curtab
      return          = it_rettab.
ENDFORM.                    " extract_original_batch1
*&---------------------------------------------------------------------*
*&      Form  user_specific_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATA     Date in generic form
*      <--P_EXTDATE  Date in User specific date format
*----------------------------------------------------------------------*
FORM user_specific_date USING    p_pdate
                        CHANGING p_extdate.
  DATA: w_datfm(1) TYPE c.
  CALL FUNCTION 'ITS_GET_USER_DEFAULTS'
    EXPORTING
      bname = sy-uname
    IMPORTING
      datfm = w_datfm.

  CASE w_datfm.
    WHEN 1.
      CONCATENATE p_pdate+6(4) p_pdate+3(2) p_pdate(2) INTO p_extdate.
    WHEN 2.
      CONCATENATE p_pdate+6(4) p_pdate(2) p_pdate+3(2) INTO p_extdate.
    WHEN 3.
      CONCATENATE p_pdate+6(4) p_pdate(2) p_pdate+3(2) INTO p_extdate.
    WHEN 4.
      CONCATENATE p_pdate+0(4) p_pdate+5(2) p_pdate+7(2) INTO p_extdate.
    WHEN 5.
      CONCATENATE p_pdate+0(4) p_pdate+5(2) p_pdate+7(2) INTO p_extdate.
    WHEN 6.
      CONCATENATE p_pdate+0(4) p_pdate+5(2) p_pdate+7(2) INTO p_extdate.
  ENDCASE.

ENDFORM.                    " user_specific_date
*&---------------------------------------------------------------------*
*&      Form  update_original_batch1
*&---------------------------------------------------------------------*
*       Sub. to update newly created Batch with char name and values
*----------------------------------------------------------------------*
FORM update_original_batch1.
  DATA: l_dktxt LIKE drat-dktxt,
        l_dktxt1(15) TYPE c.
  READ TABLE it_cabn WITH KEY atnam = 'P_REF_DOC'.  " Reference Document
  IF sy-subrc = 0.
    SELECT SINGLE dktxt FROM drat
                  INTO l_dktxt
                       WHERE dokar = gv_dokar AND
                             doknr = it_charg1-charg AND
                             dokvr = '00'     AND
                             doktl = '000'    AND
                             langu = sy-langu.
    IF NOT l_dktxt IS INITIAL.
      l_dktxt1 = l_dktxt.
      PERFORM update_table USING it_cabn-atfor it_cabn-atnam l_dktxt1.
    ENDIF.
  ENDIF.
ENDFORM.                    " update_original_batch1
*---------------------------------------------------------------------*
*       FORM my_f4                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ET_F4                                                         *
*  -->  SENDER                                                        *
*  -->  ET_BAD_CELLS                                                  *
*  -->  ES_ROW_NO                                                     *
*  -->  ER_EVENT_DATA                                                 *
*  -->  E_DISPLAY                                                     *
*  -->  E_FIELDNAME                                                   *
*  -->  P_TAB                                                         *
*---------------------------------------------------------------------*
FORM my_f4 TABLES et_f4         STRUCTURE ddshretval
           USING  sender        TYPE REF TO cl_gui_alv_grid
                  et_bad_cells  TYPE lvc_t_modi
                  es_row_no     TYPE lvc_s_roid
                  er_event_data TYPE REF TO cl_alv_event_data
                  e_display     TYPE c
                  e_fieldname   TYPE lvc_fname
                  p_tab.

  DATA : ls_out        LIKE LINE OF  it_charg1,
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

  READ TABLE  it_charg1 INDEX es_row_no-row_id INTO ls_out.

  IF sy-subrc = 0.
    CREATE DATA l_wa LIKE LINE OF  it_charg1.
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

  IF e_fieldname = 'VTYPE'.
    PERFORM f4_vtype USING e_fieldname.
  ENDIF.

*  IF E_FIELDNAME = 'DOC_PVT'.
*    PERFORM F4_DOC USING E_FIELDNAME.
*  ENDIF.
*  IF  E_FIELDNAME = 'DOC_TRF'.
*    PERFORM F4_DOC USING E_FIELDNAME.
*  ENDIF.
*
*  IF  E_FIELDNAME = 'DOC_MERAF'.
*    PERFORM F4_DOC USING E_FIELDNAME.
*  ENDIF.
*
*  IF E_FIELDNAME = 'DOC_BOL'.
*    PERFORM F4_DOC USING E_FIELDNAME.
*  ENDIF.

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
*&      Form  f4_vtype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*----------------------------------------------------------------------*
FORM f4_vtype USING    e_fieldname.

* Fill internal table for possible entry
  CLEAR  : gt_values, gt_fields.
  REFRESH: gt_values, gt_fields.

  PERFORM get_vtype_for_possible_entry.

  LOOP AT gt_vtype.
    gt_values-string = gt_vtype-vtype.
    APPEND gt_values.
    gt_values-string = gt_vtype-text.
    APPEND gt_values.
  ENDLOOP.

  CLEAR gt_fields.
  REFRESH gt_fields.

  gt_fields-fieldname = e_fieldname.
  gt_fields-position  = 1.
  gt_fields-intlen    = 1.
  gt_fields-outputlen = 1.
  gt_fields-reptext   = 'Usage Type'.
  APPEND gt_fields.
  CLEAR gt_fields.

  gt_fields-fieldname = 'TEXT'.
  gt_fields-position  = 2.
  gt_fields-intlen    = 50.
  gt_fields-outputlen = 50.
  gt_fields-reptext = 'Desc.'.
  APPEND gt_fields.
  CLEAR gt_fields.

ENDFORM.                                                    " f4_vtype
*&---------------------------------------------------------------------*
*&      Form  get_vtype_for_possible_entry
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_vtype_for_possible_entry.

  CLEAR : gt_vtype[], gt_vtype.

  gt_vtype-vtype = 'E'.
  gt_vtype-text = 'External'.
  APPEND gt_vtype.

  gt_vtype-vtype = 'I'.
  gt_vtype-text = 'Internal'.
  APPEND gt_vtype.

  gt_vtype-vtype = 'T'.
  gt_vtype-text = 'Temporary Removal'.
  APPEND gt_vtype.

ENDFORM.                    " GET_VTYPE_FOR_POSSIBLE_ENTRY
*&---------------------------------------------------------------------*
*&      Form  on_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SENDER  text
*      -->P_E_FIELDNAME  text
*      -->P_E_FIELDVALUE  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*      -->P_0963   text
*----------------------------------------------------------------------*
FORM on_f4 USING sender         TYPE REF TO cl_gui_alv_grid
                 e_fieldname    TYPE lvc_fname
                 e_fieldvalue   TYPE lvc_value
                 es_row_no      TYPE lvc_s_roid
                 er_event_data  TYPE REF TO cl_alv_event_data
                 et_bad_cells   TYPE lvc_t_modi
                 e_display      TYPE char01
                 p_tab.

  DATA lt_f4 TYPE TABLE OF ddshretval.

* Call my personal f4-help
  CLEAR: lt_f4, lt_f4[].

  CALL METHOD event_receiver->my_f4
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
                        ls_f4-fieldname
                        p_tab.

* To avoid standard f4-help.
  er_event_data->m_event_handled = 'X'.

ENDFORM.                                                    " ON_F4
*&---------------------------------------------------------------------*
*&      Form  f4_aply
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

  CASE e_fieldname.
    WHEN 'VTYPE'.
      it_charg1-vtype = ls_f4-fieldval.
    WHEN 'DOC_PVT'.
*      IT_CHARG1-DOC_PVT = LS_F4-FIELDVAL.
*    WHEN 'DOC_TRF'.
*      IT_CHARG1-DOC_TRF = LS_F4-FIELDVAL.
*    WHEN 'DOC_MERAF'.
*      IT_CHARG1-DOC_MERAF = LS_F4-FIELDVAL.
*    WHEN 'DOC_BOL'.
*      IT_CHARG1-DOC_BOL = LS_F4-FIELDVAL.
*
  ENDCASE.

ENDFORM.                                                    " F4_APLY
*&---------------------------------------------------------------------*
*&      Form  create_f4_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3657   text
*----------------------------------------------------------------------*
FORM create_f4_fields USING p_grid.
  CLEAR: gs_f4, gt_f4, gt_f4[].

* F4 FIELD
  gs_f4-fieldname  = 'VTYPE'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

  CALL METHOD alv_grid->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4.

*  CLEAR: GS_F4, GT_F4, GT_F4[].
*  GS_F4-FIELDNAME  = 'DOC_PVT'.
*  GS_F4-REGISTER   = 'X'.
*  APPEND GS_F4 TO GT_F4.
*  CALL METHOD ALV_GRID->REGISTER_F4_FOR_FIELDS
*        EXPORTING IT_F4 = GT_F4.
*
*  CLEAR: GS_F4, GT_F4, GT_F4[].
*  GS_F4-FIELDNAME  = 'DOC_TRF'.
*  GS_F4-REGISTER   = 'X'.
*  APPEND GS_F4 TO GT_F4.
*  CALL METHOD ALV_GRID->REGISTER_F4_FOR_FIELDS
*        EXPORTING IT_F4 = GT_F4.
*
*  CLEAR: GS_F4, GT_F4, GT_F4[].
*  GS_F4-FIELDNAME  = 'DOC_MERAF'.
*  GS_F4-REGISTER   = 'X'.
*  APPEND GS_F4 TO GT_F4.
*  CALL METHOD ALV_GRID->REGISTER_F4_FOR_FIELDS
*        EXPORTING IT_F4 = GT_F4.
*
*  CLEAR: GS_F4, GT_F4, GT_F4[].
*  GS_F4-FIELDNAME  = 'DOC_BOL'.
*  GS_F4-REGISTER   = 'X'.
*  APPEND GS_F4 TO GT_F4.
*  CALL METHOD ALV_GRID->REGISTER_F4_FOR_FIELDS
*        EXPORTING IT_F4 = GT_F4.


ENDFORM.                    " CREATE_F4_FIELDS

*---------------------------------------------------------------------*
*       FORM create_doc_with_bapi                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_DOC_NO                                                      *
*---------------------------------------------------------------------*
FORM create_doc_with_bapi USING p_doc_no.

  DATA longtext LIKE bapitline OCCURS 1 WITH HEADER LINE.
  DATA return LIKE bapiret2 OCCURS 1 WITH HEADER LINE.

  DATA: file_out  LIKE bapi_doc_draw-docfile1.

**..... Document data
  DATA: ls_doc    LIKE bapi_doc_draw2,
**..... Indicator for change relevance
        ls_docx   LIKE bapi_doc_drawx2,
**..... Bapi return structure
        ls_return LIKE bapiret2,
**.... Originals that are checked in simultaneously
*      lt_files LIKE bapi_doc_files OCCURS 0 WITH HEADER LINE,
        lt_files LIKE bapi_doc_files2 OCCURS 10 WITH HEADER LINE,
**.... Short texts
        lt_drat  LIKE bapi_doc_drat OCCURS 0 WITH HEADER LINE,
**.... Object links
        lt_drad  LIKE bapi_doc_drad OCCURS 0 WITH HEADER LINE.

* get file path
  CALL FUNCTION 'BAPI_CHARACT_GETLONGTEXT'
    EXPORTING
      charactname = 'P_TEST_DOC'
    TABLES
      longtext    = longtext
      return      = return.

  READ TABLE longtext INDEX 1.


  DATA : BEGIN OF it_atwrt OCCURS 0,
           objek  TYPE objnum,
           atwrt TYPE atwrt,
         END OF it_atwrt.

  DATA $objek LIKE ausp-objek.

  PERFORM get_objek USING  p_doc_no CHANGING $objek.

  SELECT objek atwrt INTO TABLE it_atwrt
               FROM ausp
               WHERE objek = $objek AND
                     atinn = gv_test_doc AND
                     klart = '022'.


  ls_doc-documenttype    = 'ICD'.
  ls_doc-documentnumber  = p_doc_no.
  ls_doc-documentversion = '00'.
  ls_doc-documentpart    = '000'.

  ls_doc-description    = 'Test'.
  ls_doc-laboratory     = ''.

**  Set indicator for change relevance
  ls_docx-description    = 'X'.
  ls_docx-laboratory     = 'X'.

  LOOP AT it_atwrt.
    CONCATENATE
    longtext-tdline  p_doc_no '-' it_atwrt-atwrt '.' INTO file_out.

    lt_files-documenttype = 'PDF'.
    lt_files-documentnumber = p_doc_no.
    lt_files-documentpart =  ls_doc-documentpart.
    lt_files-documentversion = ls_doc-documentversion.
    lt_files-description = it_atwrt-atwrt.
    lt_files-sourcedatacarrier = ''.
    lt_files-wsapplication = 'PDF'.
    lt_files-originaltype = '2'.           " Original 2
    lt_files-docfile = 'PDF'.
    lt_files-docpath = file_out.
    APPEND lt_files.
  ENDLOOP.

**
** Change document
**
  CALL FUNCTION 'BAPI_DOCUMENT_CHANGE2'
    EXPORTING
      documenttype    = ls_doc-documenttype
      documentnumber  = ls_doc-documentnumber
      documentpart    = ls_doc-documentpart
      documentversion = ls_doc-documentversion
      documentdata    = ls_doc
      documentdatax   = ls_docx
    IMPORTING
      return          = ls_return
    TABLES
      documentfiles   = lt_files.

** Did an error occur ??
  IF ls_return-type CA 'EA'.
    ROLLBACK WORK.
    MESSAGE ID '26' TYPE 'I' NUMBER '000'
            WITH ls_return-message.
  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  ENDIF.
ENDFORM.                    "CREATE_DOC_WITH_BAPI
*&---------------------------------------------------------------------*
*&      Form  get_objek
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_objek USING  p_doc_no CHANGING p_objek.


  CLEAR p_objek.

  SELECT SINGLE objek INTO p_objek FROM ausp
         WHERE
         atinn = gv_vin AND
         klart = '022' AND
         atwrt EQ vin_num.

* by ig.moon 9/22/2010 {
*  DATA $ATWRT LIKE AUSP-ATWRT.
*  CONCATENATE '%' P_DOC_NO+3 INTO $ATWRT.
*
*  CLEAR P_OBJEK.
*
*  SELECT SINGLE OBJEK INTO P_OBJEK FROM AUSP
*         WHERE
*         ATINN = GV_VIN AND
*         KLART = '022' AND
*         ATWRT LIKE $ATWRT.
* }

ENDFORM.                    " get_objek
*&---------------------------------------------------------------------*
*&      Form  F4_doc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*----------------------------------------------------------------------*
FORM f4_doc USING e_fieldname.
**   Fill internal table for possible entry
  CLEAR  : gt_values, gt_fields.
  REFRESH: gt_values, gt_fields.

  PERFORM get_doc_for_possible_entry.

  LOOP AT gt_vtype.
    gt_values-string = gt_vtype-vtype.
    APPEND gt_values.
    gt_values-string = gt_vtype-text.
    APPEND gt_values.
  ENDLOOP.

  CLEAR gt_fields.
  REFRESH gt_fields.

  gt_fields-fieldname = e_fieldname.
  gt_fields-position  = 1.
  gt_fields-intlen    = 1.
  gt_fields-outputlen = 1.
  gt_fields-reptext   = 'Doc'.
  APPEND gt_fields.
  CLEAR gt_fields.

  gt_fields-fieldname = 'TEXT'.
  gt_fields-position  = 2.
  gt_fields-intlen    = 50.
  gt_fields-outputlen = 50.
  gt_fields-reptext = 'Desc.'.
  APPEND gt_fields.
  CLEAR gt_fields.

ENDFORM.                                                    " F4_doc
*&---------------------------------------------------------------------*
*&      Form  GET_doc_FOR_POSSIBLE_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_doc_for_possible_entry.
  CLEAR : gt_vtype[], gt_vtype.

  gt_vtype-vtype = 'Y'.
  gt_vtype-text = 'Exist'.
  APPEND gt_vtype.

  gt_vtype-vtype = ' '.
  gt_vtype-text = 'No Entry'.
  APPEND gt_vtype.

ENDFORM.                    " GET_doc_FOR_POSSIBLE_ENTRY
