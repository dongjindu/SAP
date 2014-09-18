*----------------------------------------------------------------------
* Program ID        : ZR_MM_VEHICLE_DISPLAY
* Title             : Display Test Vehicle Tracking record
* Created on        : 11/20/2007
* Created by        : Rakesh Gandhi
* Specifications By : Paul Shrewsbury
* Description       : This program display test vehicle tracking records
*----------------------------------------------------------------------
REPORT zr_mm_vehicle_display MESSAGE-ID zmco.
TYPE-POOLS: slis,
            icon.

TABLES: mcha,
        mch1,
        mchb,
        klah,
        bseg,
        csks,
        zyesno.

*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: BEGIN OF it_charg OCCURS 0   ,
        charg LIKE mcha-charg      ,
        lvorm LIKE mcha-lvorm      ,
      END OF it_charg              .

DATA: vin_num        TYPE ze_vin        ,
      full_spec_cd   TYPE ze_fsc        ,
      ask_dept       TYPE ze_ask_dept   ,
      usage_dept     TYPE ze_usage_dept ,
      gl_act         TYPE ze_glact      .

DATA: BEGIN OF it_charg1 OCCURS 0             ,
        charg              LIKE mcha-charg    ,
        vin_num            TYPE ze_vin        ,
        name(50)           TYPE c             ,
        username           LIKE cdhdr-username,
        udate              LIKE cdhdr-udate   ,
        utime              LIKE cdhdr-utime   ,
        fsc                TYPE ze_fsc        ,
        ext_clr(3)         TYPE c             ,
        int_clr(3)         TYPE c             ,
        rp18_shop_date(10) TYPE c             ,
        car_type(2)        TYPE c             ,
        usage(1)           TYPE c             ,
** Changed by Furong on 02/22/08  "UD1K942932
        ent_typ(10)         TYPE c             ,
        ftz_entry(13)         TYPE c             ,
        duty_paid(10)         TYPE c             ,
        duty_amt(11)          TYPE c             ,
        temp_rem(17)         TYPE c             ,
        ret_date(10)         TYPE c             ,
        scrap_res(10)         TYPE c             ,
** End of change   on 02/22/08   "UD1K942932
        ship_date(10)      TYPE c             ,
        ship_docs(12)      TYPE c             ,
        final_dest(15)     TYPE c             ,
        ask_dept           TYPE ze_ask_dept   ,
        usage_dept         TYPE ze_ask_dept   ,
        usage_text(30)     TYPE c             ,
        scrap_date(10)     TYPE c             ,  " P_SCRAP_DATE
        scrap_tag_no(10)   TYPE c             ,  " P_SCRAP_APP_DOC
        ref_doc(15)        TYPE c             ,
        gl_acct(10)        TYPE c             ,
        acc_doc(10)        TYPE c             ,
        invoice            TYPE ze_invoice    ,
**Changed by Furong on 01/23/12
        yes(1)              TYPE c             ,
        desc(30)           TYPE c             ,
** end on 01/23/12
        del_indicator(1)   TYPE c             ,
        chkbox(1)          TYPE c             ,
        icon               TYPE icon_d        ,
      END OF it_charg1                        ,

      BEGIN OF it_ausp_tmp OCCURS 0           ,
        objek LIKE ausp-objek                 ,
        atwrt LIKE ausp-atwrt                 ,
        atflv LIKE ausp-atflv                 ,
      END OF it_ausp_tmp                      ,

      BEGIN OF it_inob OCCURS 0               ,
        cuobj LIKE inob-cuobj                 ,
        objek LIKE inob-objek                 ,
        charg LIKE mcha-charg                 ,
        objectid LIKE cdpos-objectid          ,
        lvorm LIKE mcha-lvorm                 ,
      END OF it_inob                          ,

      BEGIN OF it_inob1 OCCURS 0              ,
        cuobj LIKE inob-cuobj                 ,
      END OF it_inob1                         ,

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

      BEGIN OF it_users OCCURS 0              ,
        bname      LIKE usr02-bname           ,
        name_first LIKE adrp-name_first       ,
        name_last  LIKE adrp-name_last        ,
      END OF it_users                         ,

      BEGIN OF it_shop_dt OCCURS 0,
        date LIKE sy-datum        ,
      END OF it_shop_dt           ,

      BEGIN OF it_ship_dt OCCURS 0,
        date LIKE sy-datum        ,
      END OF it_ship_dt           ,

      BEGIN OF it_scrp_dt OCCURS 0,
        date LIKE sy-datum        ,
      END OF it_scrp_dt           ,

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

** Changed by Furong on 02/22/08  "UD1K942932
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
      BEGIN OF it_scrap_res OCCURS 0 ,
        scrapr(10)  TYPE c            ,
      END OF it_scrap_res         ,
** End of change on 02/22/08
** Changed by Furong on 01/23/12
      BEGIN OF it_yes OCCURS 0 ,
        yes(1) TYPE c            ,
      END OF it_yes              ,
** eND ON 01/23/12
      BEGIN OF it_rettab OCCURS 0 .
        INCLUDE STRUCTURE bapiret2.
DATA: END OF it_rettab            ,

BEGIN OF it_cdhdr OCCURS 0        ,
  objectclas LIKE cdhdr-objectclas,
  objectid LIKE cdhdr-objectid    ,
  changenr LIKE cdhdr-changenr    ,
  udate LIKE cdhdr-udate          ,
  utime LIKE cdhdr-utime          ,
  username LIKE cdhdr-username    ,
END OF it_cdhdr                   ,

BEGIN OF it_cdpos OCCURS 0.
        INCLUDE STRUCTURE cdpos .
DATA: END OF it_cdpos           ,

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
      it_cdpos1 LIKE it_cdpos OCCURS 0 WITH HEADER LINE   ,
      custom_control TYPE   scrfname VALUE 'ALV_CONTAINER',
      alv_grid       TYPE   REF TO cl_gui_alv_grid        ,
      grid_container TYPE   REF TO cl_gui_custom_container,
      event_receiver TYPE   REF TO lcl_event_receiver     .

*-ALV Data declaration
DATA : gt_fieldcat TYPE lvc_t_fcat WITH HEADER LINE,
       gv_repid    LIKE sy-repid              ,
       gv_variant  TYPE disvariant            , " for paramtr IS_VARIANT
       gv_save     TYPE c   VALUE 'A'         , " for Parameter I_SAVE
       gv_charg    LIKE mcha-charg            ,
       gv_object   LIKE bapi1003_key-object   ,
       ok_code     LIKE sy-ucomm              ,
       gv_dokar    LIKE draw-dokar VALUE 'ICD',
       gv_doknr    LIKE draw-doknr            ,
       gv_tabix    LIKE sy-tabix              ,
       gv_txt(100) TYPE c                     ,
       gv_vin      LIKE cabn-atinn            ,
       gv_fsc      LIKE cabn-atinn            ,
       gv_askdept  LIKE cabn-atinn            ,
       gv_usagedep LIKE cabn-atinn            ,
       gv_glno     LIKE cabn-atinn            ,
       gv_exclr    LIKE cabn-atinn            ,
       gv_inclr    LIKE cabn-atinn            ,
       gv_shopdt   LIKE cabn-atinn            ,
       gv_usgcar   LIKE cabn-atinn            ,
       gv_shipdt   LIKE cabn-atinn            ,
       gv_shipdoc  LIKE cabn-atinn            ,
       gv_finaldes LIKE cabn-atinn            ,
       gv_usgtxt   LIKE cabn-atinn            ,
       gv_scrpdt   LIKE cabn-atinn            ,
       gv_scrpdoc  LIKE cabn-atinn            ,
       gv_refdoc   LIKE cabn-atinn            ,
       gv_acc_doc  LIKE cabn-atinn            ,
       gv_invno     LIKE cabn-atinn           ,
       gv_ent_typ   LIKE cabn-atinn            ,
       gv_ftz_entry LIKE cabn-atinn            ,
       gv_duty_paid LIKE cabn-atinn            ,
       gv_duty_amt  LIKE cabn-atinn            ,
       gv_temp_rem  LIKE cabn-atinn            ,
       gv_ret_date  LIKE cabn-atinn            ,
       gv_scrap_rev  LIKE cabn-atinn           ,
       gv_yes       LIKE cabn-atinn            ,
       gv_desc  LIKE cabn-atinn          .

DATA : gs_fieldcat TYPE lvc_s_fcat,
       gs_layout   TYPE lvc_s_layo. " The Layout Structure

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

    handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row
                e_column
                es_row_no.

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_toolbar.
    DATA: ls_toolbar  TYPE stb_button.

    CONSTANTS: c_separator TYPE i VALUE 3.

*-Append seperator to the normal toolbar
    CLEAR ls_toolbar.
    MOVE c_separator TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.
*-Append a new button that to the toolbar. Use E_OBJECT of
*-event toolbar. E_OBJECT is of type CL_ALV_EVENT_TOOLBAR_SET.
*-This class has one attribute MT_TOOLBAR which is of table type
*-TTB_BUTTON. The structure is STB_BUTTON

*-Create Document button
    CLEAR ls_toolbar.
    MOVE 'DIS_DOC'          TO ls_toolbar-function.
    MOVE  icon_document     TO ls_toolbar-icon.
    MOVE 'DISPLAY DOCUMENT' TO ls_toolbar-quickinfo.
    MOVE 'Display Document' TO ls_toolbar-text.
    MOVE ' '                TO ls_toolbar-disabled.
    APPEND ls_toolbar       TO e_object->mt_toolbar.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.
    PERFORM sub_event_ucomm USING e_ucomm.
  ENDMETHOD.                    "handle_user_command

  METHOD handle_double_click.
    DATA: lw_temp LIKE LINE OF it_charg1.
    DATA: l_year(4).

    READ TABLE it_charg1 INTO lw_temp INDEX es_row_no-row_id.
    CHECK sy-subrc EQ 0.

    IF e_column = 'ACC_DOC' AND NOT lw_temp-acc_doc IS INITIAL.
      l_year = lw_temp-udate+0(4).
      SET PARAMETER ID 'BLN' FIELD lw_temp-acc_doc.
      SET PARAMETER ID 'GJR' FIELD l_year.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDIF.
    IF e_column = 'INVOICE' AND NOT lw_temp-invoice IS INITIAL.
      l_year = lw_temp-udate+0(4).
      SET PARAMETER ID 'BLN' FIELD lw_temp-invoice.
      SET PARAMETER ID 'GJR' FIELD l_year.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

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
                s_scrdt  FOR sy-datum MODIF ID 128    , " Ship Out Date
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
                s_scrapr   FOR it_charg1-scrap_res MODIF ID 128,
** Furong on 01/23/12
                s_yes FOR zyesno-zyes MODIF ID 128.
** end on 01/23/12
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = 'Test Vehicle Tracking - Display'.
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
             s_udept, s_glno[], s_glno, s_scrdt, s_scrdt[],
             s_scrapr, s_scrapr[], s_yes,s_yes[].
      IF s_body[] IS INITIAL.
        MESSAGE e000 WITH text-013.
      ENDIF.
*-VIN Number
    ELSEIF p_vin = 'X'.
      CLEAR s_body[].
      IF s_vin[]    IS INITIAL AND s_fsc[]    IS INITIAL AND
         s_shopdt[] IS INITIAL AND s_shipdt[] IS INITIAL AND
         s_askdep[] IS INITIAL AND s_udept[]  IS INITIAL AND
         s_glno[]   IS INITIAL AND s_scrdt[]  IS INITIAL AND
         s_scrapr[] IS INITIAL AND s_enttyp[] IS INITIAL AND
         s_ftz[] IS INITIAL AND s_dpaid[] IS INITIAL AND
         s_trem[] IS INITIAL AND s_rdate[] IS INITIAL AND
         s_yes[] IS INITIAL.
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
  IF p_vin = 'X'.

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

*-Scrap date
    IF NOT s_scrdt[] IS INITIAL.
      REFRESH it_scrp_dt.
      CLEAR   it_scrp_dt.
      LOOP AT s_scrdt.
        it_scrp_dt-date = s_scrdt-low.
        APPEND it_scrp_dt.
        CLEAR  it_scrp_dt.
      ENDLOOP.
      SORT it_scrp_dt BY date.
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
  ENDIF.

** Changed by Furong on 02/22/08  "UD1K942932
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

  IF NOT s_scrapr[] IS INITIAL.
    REFRESH it_scrap_res.
    CLEAR   it_scrap_res.
    LOOP AT s_scrapr.
      it_scrap_res-scrapr = s_scrapr-low.
      APPEND it_scrap_res.
      CLEAR  it_scrap_res.
    ENDLOOP.
    SORT it_scrap_res BY scrapr.
  ENDIF.
** End of change on 02/22/08

** Changed by Furong on 01/23/12
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

  PERFORM get_data.

  CHECK NOT it_charg[] IS INITIAL.

** Changed by Furong on 07/08/08

*  IF P_BODY = 'X'.
*    LOOP AT IT_CHARG.
*      PERFORM BUILD_OBJECT_KEY USING IT_CHARG-CHARG.
*      PERFORM GET_CHAR_CHANGE.
*    ENDLOOP.
*  ELSEIF P_VIN = 'X'.
*      PERFORM GET_CHAR_CHANGE4.
*  ENDIF.
  CLEAR: it_charg1, it_charg1[].

  LOOP AT it_charg.
    PERFORM build_object_key USING it_charg-charg.
    PERFORM get_char_change.
  ENDLOOP.

** End of change on 07/08/08

  IF it_charg1[] IS INITIAL.
    MESSAGE i000 WITH text-001.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SELECT u~bname a~name_first a~name_last
               FROM usr21 AS u
               INNER JOIN adrp AS a
               ON u~persnumber = a~persnumber
               INTO TABLE it_users
               FOR ALL ENTRIES IN it_charg1
               WHERE bname = it_charg1-username.
  IF sy-subrc = 0.
    LOOP AT it_charg1.
      CLEAR gv_tabix.
      gv_tabix = sy-tabix.
      READ TABLE it_users WITH KEY bname = it_charg1-username.
      IF sy-subrc = 0.
        CONCATENATE it_users-name_first it_users-name_last
                       INTO it_charg1-name SEPARATED BY space.
        MODIFY it_charg1 INDEX gv_tabix TRANSPORTING name.

      ENDIF.
    ENDLOOP.
  ENDIF.

  CALL SCREEN 9000.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Subroutine to get data from DB table
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_vin(12)      TYPE c,
        l_fsc(12)      TYPE c,
        l_askdept(12)  TYPE c,
        l_usagedep(12) TYPE c,
        l_glno(12)     TYPE c,
        l_shipdt(12)   TYPE c,
        l_scrpdt(12)   TYPE c,
        l_shopdt(12)   TYPE c,
        l_scrapr(12) TYPE c,
        l_entty(12)   TYPE c,
        l_ftz(12) TYPE c,
        l_dpaid(13) TYPE c,
        l_trem(12)  TYPE c,
        l_rdate(12)  TYPE c,
        l_yes(1) TYPE c,
        l_tabix LIKE sy-tabix.

  REFRESH: it_charg, it_charg2, it_charg3, it_ausp_tmp.
  CLEAR:   it_charg, it_charg2, it_charg3, it_ausp_tmp.
*-All Batches
  IF p_body = 'X'.
    SELECT charg lvorm
           FROM mcha
           INTO TABLE it_charg
           WHERE matnr = c_matnr AND
                 werks = c_werks AND
                 charg IN s_body.
    IF it_charg[] IS INITIAL.
      MESSAGE i000 WITH text-001.
      LEAVE LIST-PROCESSING.
    ENDIF.

    SORT it_charg BY charg.
    DELETE ADJACENT DUPLICATES FROM it_charg COMPARING charg.

*-If characteristic search is choosen
  ELSEIF p_vin = 'X'.
*-If VIN Number is entered
    IF NOT s_vin[] IS INITIAL.
      l_vin = gv_vin.
      CONCATENATE '%' l_vin '%' INTO l_vin.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_vin AND
                         value_new IN s_vin AND
                         objectclas = 'CLASSIFY' AND
                         tabname = 'ABAUSP'  AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
*-If Full Spec Code is entered
    IF NOT s_fsc[] IS INITIAL.
      l_fsc = gv_fsc.
      CONCATENATE '%' l_fsc '%' INTO l_fsc.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_fsc AND
                         value_new IN s_fsc AND
                         objectclas = 'CLASSIFY' AND
                         tabname = 'ABAUSP'  AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
*-If Asking Department is entered
    IF NOT s_askdep[] IS INITIAL.
      l_askdept = gv_askdept.
      CONCATENATE '%' l_askdept '%' INTO l_askdept.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_askdept AND
                         value_new IN s_askdep AND
                         objectclas = 'CLASSIFY' AND
                         tabname = 'ABAUSP'  AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
*-If Destination/Using Department is entered
    IF NOT s_udept[] IS INITIAL.
      l_usagedep = gv_usagedep.
      CONCATENATE '%' l_usagedep '%' INTO l_usagedep.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_usagedep AND
                         value_new IN s_udept AND
                         objectclas = 'CLASSIFY' AND
                         tabname = 'ABAUSP'  AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
*-If G/L No is entered
    IF NOT s_glno[] IS INITIAL.
      l_glno = gv_glno.
      CONCATENATE '%' l_glno '%' INTO l_glno.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_glno      AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATFLV'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

*-Shop Date
    IF NOT s_shopdt[] IS INITIAL.
      l_shopdt = gv_shopdt.
      CONCATENATE '%' l_shopdt '%' INTO l_shopdt.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_shopdt    AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATFLV'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
*-Ship Date
    IF NOT s_shipdt[] IS INITIAL.
      l_shipdt = gv_shipdt.
      CONCATENATE '%' l_shipdt '%' INTO l_shipdt.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_shipdt    AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATFLV'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
*-Scrap Date
    IF NOT s_scrdt[] IS INITIAL.
      l_scrpdt = gv_scrpdt.
      CONCATENATE '%' l_scrpdt '%' INTO l_scrpdt.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_scrpdt    AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATFLV'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
** Changed by Furong on 02/22/08  "UD1K942932
    IF NOT s_scrapr[] IS INITIAL.
      l_scrapr = gv_scrap_rev.
      CONCATENATE '%' l_scrapr '%' INTO l_scrapr.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_scrapr AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATWRT'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

    IF NOT s_enttyp[] IS INITIAL.
      l_entty = gv_ent_typ.
      CONCATENATE '%' l_entty '%' INTO l_entty.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_entty AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATWRT'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

    IF NOT s_ftz[] IS INITIAL.
      l_ftz = gv_ftz_entry.
      CONCATENATE '%' l_ftz '%' INTO l_ftz.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_ftz AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATWRT'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

    IF NOT s_dpaid[] IS INITIAL.
      l_dpaid = gv_duty_paid.
      CONCATENATE '%' l_dpaid '%' INTO l_dpaid.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_dpaid AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATWRT'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

    IF NOT s_trem[] IS INITIAL.
      l_trem = gv_temp_rem.
      CONCATENATE '%' l_trem '%' INTO l_trem.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_trem AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATWRT'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

    IF NOT s_rdate[] IS INITIAL.
      l_rdate = gv_ret_date.
      CONCATENATE '%' l_rdate '%' INTO l_rdate.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                   WHERE tabkey LIKE l_rdate AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATFLV'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
** End of change

** Changed b Furong on 01/23/12
    IF NOT s_yes[] IS INITIAL.
      l_yes = gv_yes.
      CONCATENATE '%' l_yes '%' INTO l_yes.
      SELECT * FROM cdpos
                   APPENDING TABLE it_cdpos
                         WHERE tabkey LIKE l_yes AND
                         objectclas = 'CLASSIFY' AND
                         fname      = 'ATWRT'    AND
                         tabname = 'ABAUSP'      AND
                         chngind = 'U' AND
                         text_case = '1'.
      IF it_cdpos[] IS INITIAL.
        MESSAGE i000 WITH text-001.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
** end on 01/23/12

*-Validations for user entered values
    SELECT objectclas objectid changenr udate utime username
           FROM cdhdr
           INTO TABLE it_cdhdr
           FOR ALL ENTRIES IN it_cdpos
           WHERE objectclas = 'CLASSIFY'        AND
                 objectid   = it_cdpos-objectid AND
                 changenr   = it_cdpos-changenr.

    PERFORM get_char_change3.

    IF it_cdpos[] IS INITIAL.
      MESSAGE i000 WITH text-001.
      LEAVE LIST-PROCESSING.
    ENDIF.

    REFRESH: it_cdpos1, it_cdhdr.
    CLEAR  : it_cdpos1, it_cdhdr.

    SELECT * FROM cdpos
             INTO TABLE it_cdpos1
             FOR ALL ENTRIES IN it_cdpos
             WHERE objectclas = 'CLASSIFY'      AND
                   objectid = it_cdpos-objectid AND
                   changenr = it_cdpos-changenr AND
                   tabname = 'ABAUSP'  AND
                   chngind = 'U' AND
                   text_case = '1'.

    SELECT objectclas objectid changenr udate utime username
           FROM cdhdr
           INTO TABLE it_cdhdr
           FOR ALL ENTRIES IN it_cdpos1
           WHERE objectclas = 'CLASSIFY'         AND
                 objectid   = it_cdpos1-objectid AND
                 changenr   = it_cdpos1-changenr.

    IF it_cdpos1[] IS INITIAL.
      MESSAGE i000 WITH text-001.
      LEAVE LIST-PROCESSING.
    ENDIF.
    SORT it_cdpos1 BY objectid changenr tabname tabkey.

    LOOP AT it_cdpos1.
      it_inob1-cuobj = it_cdpos1-objectid.
      APPEND it_inob1.
      CLEAR  it_inob1.
    ENDLOOP.
    SORT it_inob1 BY cuobj.

    CHECK NOT it_inob1[] IS INITIAL.
    SELECT cuobj objek
                 FROM inob
                 INTO TABLE it_inob
                 FOR ALL ENTRIES IN it_inob1
                 WHERE cuobj = it_inob1-cuobj.

    LOOP AT it_inob.
      it_inob-charg = it_inob-objek+22(10).
      it_inob-objectid = it_inob-cuobj.
      CONCATENATE it_inob-objectid 'O' INTO it_inob-objectid.
      MODIFY it_inob INDEX sy-tabix TRANSPORTING charg objectid.
    ENDLOOP.

    CHECK NOT it_inob[] IS INITIAL.
    SELECT charg lvorm
           FROM mcha
           INTO TABLE it_charg
           FOR ALL ENTRIES IN it_inob
           WHERE matnr = c_matnr AND
                 werks = c_werks AND
                 charg = it_inob-charg.

    SORT it_charg BY charg.
    LOOP AT it_inob.
      l_tabix = sy-tabix.
      READ TABLE it_charg WITH KEY charg = it_inob-charg BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE it_cdpos1 WHERE objectid = it_inob-objectid.
        DELETE it_inob INDEX l_tabix.
      ELSE.
        it_inob-lvorm = it_charg-lvorm.
        MODIFY it_inob INDEX l_tabix TRANSPORTING lvorm.
      ENDIF.
    ENDLOOP.
    DELETE it_cdpos1 WHERE fname = 'ATCOD'.
    DELETE it_cdpos1 WHERE chngind = 'I'.

  ENDIF.    " IF p_body = 'X'.
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
     ' '  'NAME'           'User Name'         50 'CHAR' ' ' ' ',
     ' '  'UDATE'          'Date'              10 'CHAR' ' ' ' ',
     ' '  'UTIME'          'Time'               8 'CHAR' ' ' ' ',
     ' '  'FSC'            'Full Spec Code'    20 'CHAR' ' ' ' ',
     ' '  'EXT_CLR'        'Exterior Color'    14 'CHAR' ' ' ' ',
     ' '  'INT_CLR'        'Interior Color'    14 'CHAR' ' ' ' ',
     ' '  'RP18_SHOP_DATE' 'RP18 shopdate'     13 'CHAR' ' ' ' ',
     ' '  'CAR_TYPE'       'Test Car Type'     13 'CHAR' ' ' ' ',
     ' '  'USAGE'          'Usage'              5 'CHAR' ' ' ' ',
     ' '  'SHIP_DATE'      'Ship Out Date'     13 'CHAR' ' ' ' ',
     ' '  'SHIP_DOCS'      'Ship Out Doc'      12 'CHAR' ' ' ' ',
     ' '  'FINAL_DEST'     'Final Destination' 15 'CHAR' ' ' ' ',
** Changed by Furong on 02/22/08  "UD1K942932
     ' '  'TEMP_REM'        'Temp.Rem'          17 'CHAR' ' ' ' ',
     ' '  'RET_DATE'        'Return Date'       10 'CHAR' ' ' ' ',
** End of change on 02/22/08
     ' '  'ASK_DEPT'       'Asking Dept.'      10 'CHAR' ' ' ' ',
     ' '  'USAGE_DEPT'     'Using Dept.'       11 'CHAR' ' ' ' ',
     ' '  'USAGE_TEXT'     'Usage Text'        30 'CHAR' ' ' ' ',
** Changed by Furong on 02/22/08  "UD1K942932
     ' '  'ENT_TYP'        'Entry Type'        10 'CHAR' ' ' ' ',
     ' '  'FTZ_ENTRY'      'FTZ Entry   '      13 'CHAR' ' ' ' ',
     ' '  'DUTY_PAID'      'Duty Paid'         10 'CHAR' ' ' ' ',
     ' '  'DUTY_AMT'       'Duty Amount'       11 'CHAR' ' ' ' ',
** End of change on 02/22/08
     ' '  'SCRAP_DATE'     'Scrap Date'        10 'CHAR' ' ' ' ',
     ' '  'SCRAP_TAG_NO'   'Scrap Tag #'       12 'CHAR' ' ' ' ',
** Changed by Furong on 02/22/08  "UD1K942932
     ' '  'SCRAP_RES'      'Scrap Reversed'    12 'CHAR' ' ' ' ',
** End of change on 02/22/08
     ' '  'REF_DOC'        'Reference Doc #'   15 'CHAR' ' ' ' ',
     ' '  'GL_ACCT'        'G/L Account'       11 'NUMC' ' ' ' ',
     ' '  'ACC_DOC'        'Accounting Doc #'  16 'CHAR' ' ' ' ',
     ' '  'INVOICE'        'Invoice Doc #'     13 'NUMC' ' ' ' ',
     ' '  'DEL_INDICATOR'  'Del Indicator'     13 'CHAR' ' ' ' ',
** Changed by Furong on 01/20/12
    ' '  'YES'             'Y/N'               5  'CHAR' ' ' ' ',
    ' '  'DESC'            'Description'       30 'CHAR' ' ' ' '.
** End on 01/21/12
  LOOP AT gt_fieldcat.
    IF gt_fieldcat-fieldname = 'DEL_INDICATOR'.
      gs_fieldcat-edit = ' '.
      gt_fieldcat-checkbox = 'X'.
      MODIFY gt_fieldcat INDEX sy-tabix TRANSPORTING edit checkbox.
    ENDIF.
  ENDLOOP.
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

  SORT it_charg1 BY charg udate utime.
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
  SET HANDLER event_receiver->handle_double_click FOR alv_grid.

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

*    WHEN 'FB03'.
*      PERFORM CALL_FB03.
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

  READ TABLE it_cabn WITH KEY atnam = 'P_VIN'.      " VIN Number
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                               it_charg1-vin_num.

  READ TABLE it_cabn WITH KEY atnam = 'P_FSC'.  " Full Spec code
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam it_charg1-fsc.

  READ TABLE it_cabn WITH KEY atnam = 'P_EXT_COLOR'.  " Exterior Color
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                      it_charg1-ext_clr.

  READ TABLE it_cabn WITH KEY atnam = 'P_INT_COLOR'.  " Interior Color
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                      it_charg1-int_clr.

  READ TABLE it_cabn WITH KEY atnam = 'P_USAGE_CAR'.  " Usage Car
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                      it_charg1-usage.

  READ TABLE it_cabn WITH KEY atnam = 'P_USAGE_TEXT'.  " Usage text
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                  it_charg1-usage_text.

  READ TABLE it_cabn WITH KEY atnam = 'P_ASK_DEPT'.  " Asking Dept
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                     it_charg1-ask_dept.

  READ TABLE it_cabn WITH KEY atnam = 'P_REF_DOC'.  " Reference Document
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                      it_charg1-ref_doc.

  READ TABLE it_cabn WITH KEY atnam = 'P_USAGE_DEPT'.  " Final Dest
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                   it_charg1-usage_dept.

  READ TABLE it_cabn WITH KEY atnam = 'P_SCRAP_APP_DOC'.  " Scrap Doc No
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                 it_charg1-scrap_tag_no.

  READ TABLE it_cabn WITH KEY atnam = 'P_RP18_SHOP_DATE'. " RP18 shopdt
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                              it_charg1-rp18_shop_date.

  READ TABLE it_cabn WITH KEY atnam = 'P_SHIPOUT_DATE'.  " ShipOut Date
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                   it_charg1-ship_date.

  READ TABLE it_cabn WITH KEY atnam = 'P_SCRAP_DATE'. " Scrap Date
  PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                              it_charg1-scrap_date.

** Changed by Furong on 02/22/08  "UD1K942932
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

  READ TABLE it_cabn WITH KEY atnam = 'P_SCRAP_REV'.  " Scrap Reserve
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                             it_charg1-scrap_res.
  ENDIF.
**End of change on 02/20/08  "UD1K942932

**Changed by Furong on 01/23/12
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
** End ON 01/23/12


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
          ELSEIF it_cabn-atnam = 'P_EXT_COLOR'.     " Ext Color
            gv_exclr = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_INT_COLOR'.     " Int Color
            gv_inclr = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_RP18_SHOP_DATE'." Shop Dt
            gv_shopdt = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_USAGE_CAR'.     " Usage Car
            gv_usgcar = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_SHIPOUT_DATE'.  " Ship Dt
            gv_shipdt = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_SHIPOUT_DOC'.  " Ship Out Doc
            gv_shipdoc = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_FINAL_DEST'.   " Final Dest
            gv_finaldes = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_USAGE_TEXT'.    " Usage Text
            gv_usgtxt = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_SCRAP_DATE'.    " Scrap Date
            gv_scrpdt = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_SCRAP_APP_DOC'. " Scrp Doc
            gv_scrpdoc = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_REF_DOC'.       " Ref doc
            gv_refdoc = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_ACC_DOC'.       " Accounting Doc
            gv_acc_doc = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_INV_NO'.        " Invoice Number
            gv_invno = it_cabn-atinn.
** Changed by Furong on 02/22/08  "UD1K942932
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
          ELSEIF it_cabn-atnam = 'P_SCRAP_REV'.    " Scrap Reserve
            gv_scrap_rev = it_cabn-atinn.
** End of change on 02/22/08  "UD1K942932
** Changed by Furong on 01/23/12
          ELSEIF it_cabn-atnam = 'P_YES'.
            gv_yes = it_cabn-atinn.
          ELSEIF it_cabn-atnam = 'P_DESCRIPTION'.
            gv_desc = it_cabn-atinn.
** End on 01/23/12
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

    WHEN 'CHAR' OR 'DATE'.
      READ TABLE it_chatab WITH KEY charact = atnam.
      IF syst-subrc = 0.
        it_chatab-value_neutral = value.
        MODIFY it_chatab INDEX syst-tabix.
      ELSE.
        it_chatab-charact = atnam.
        it_chatab-value_neutral = value.
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

ENDFORM.                    " extract_original_batch
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
*&      Form  get_char_change
*&---------------------------------------------------------------------*
*       Subroutine to get characteristic values
*----------------------------------------------------------------------*
FORM get_char_change.
  DATA: l_gl_acct TYPE ze_glact    ,
        l_inv_doc TYPE ze_invoice  ,
        l_rp18_shop_date(10) TYPE c,
        l_shipout_date(10)   TYPE c,
        l_ship_date(10)      TYPE c,
        l_date2(8)           TYPE c,
        l_tabkey LIKE cdpos-tabkey  ,
        wa_cdpos             LIKE cdpos.

  CALL FUNCTION 'CLLA_CHANGE_DOC_CLASSIFICATION'
    EXPORTING
      object          = gv_object
      classtype       = c_klart
      object_type     = 'MCHA'
*     OBJ_EQ_CUOBJ    =
*     CHARACTERISTIC  =
*     DATUM           =
      with_listing    = ' '
*     ONLY_VALIDATION = ' '
    TABLES
*     CD_HEADER       =
      cd_position     = it_cdpos
    EXCEPTIONS
      no_data         = 1
      OTHERS          = 2.

  DELETE it_cdpos WHERE fname = 'ATCOD'.
  DELETE it_cdpos WHERE chngind = 'I'.
  IF NOT it_cdpos[] IS INITIAL.
    REFRESH: it_cdhdr.
    CLEAR  : it_cdhdr.
    SELECT objectclas objectid changenr udate utime username
           FROM cdhdr
           INTO TABLE it_cdhdr
           FOR ALL ENTRIES IN it_cdpos
           WHERE objectclas = it_cdpos-objectclas AND
                 objectid   = it_cdpos-objectid.
    SORT it_cdhdr BY objectclas objectid changenr.
    DELETE ADJACENT DUPLICATES FROM it_cdhdr
                COMPARING objectclas objectid changenr.
  ENDIF.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT it_cdpos BY objectid changenr tabname tabkey.
  SORT it_cdhdr BY objectclas objectid changenr udate utime username.
  LOOP AT it_cdhdr.
    LOOP AT it_cdpos WHERE changenr = it_cdhdr-changenr.
      IF it_cdpos-tabkey+18(10) = gv_vin.         " P_VIN
*-Validations for VIN Number
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-vin_num = it_cdpos-value_new.

*-Validations for Full spec code
      ELSEIF it_cdpos-tabkey+18(10) = gv_fsc.     " P_FSC
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-fsc = it_cdpos-value_new.
*-Exterior Color
      ELSEIF it_cdpos-tabkey+18(10) = gv_exclr.   " P_EXT_COLOR
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-ext_clr = it_cdpos-value_new.
*-Interior color
      ELSEIF it_cdpos-tabkey+18(10) = gv_inclr.   " P_INT_COLOR
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-int_clr = it_cdpos-value_new.
*-Validations for Shop date
      ELSEIF it_cdpos-tabkey+18(10) = gv_shopdt.  " P_RP18_SHOP_DATE
        CONDENSE it_cdpos-value_new NO-GAPS.
        CLEAR l_date2.
        REPLACE '.' WITH space INTO it_cdpos-value_new.
        CONDENSE it_cdpos-value_new NO-GAPS.
        l_date2 = it_cdpos-value_new(8).
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        PERFORM user_specific_date1 USING  l_date2
                                   CHANGING it_charg1-rp18_shop_date.
*-Validations for Ship Out Date
      ELSEIF it_cdpos-tabkey+18(10) = gv_shipdt.  " P_SHIPOUT_DATE
        CONDENSE it_cdpos-value_new NO-GAPS.
        CLEAR l_date2.
        REPLACE '.' WITH space INTO it_cdpos-value_new.
        CONDENSE it_cdpos-value_new NO-GAPS.
        l_date2 = it_cdpos-value_new(8).
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        PERFORM user_specific_date1 USING  l_date2
                                   CHANGING it_charg1-ship_date.
*-Validations for Ship Out Docs
      ELSEIF it_cdpos-tabkey+18(10) = gv_shipdoc. " P_SHIPOUT_DOC
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-ship_docs = it_cdpos-value_new.
*-Validations for Final Destination
      ELSEIF it_cdpos-tabkey+18(10) = gv_finaldes. " P_FINAL_DEST
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-final_dest = it_cdpos-value_new.
*-Validations for Asking Department
      ELSEIF it_cdpos-tabkey+18(10) = gv_askdept. " P_ASK_DEPT
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-ask_dept = it_cdpos-value_new.
*-Validations for Destination/Using Department
      ELSEIF it_cdpos-tabkey+18(10) = gv_usagedep.    " P_USAGE_DEPT
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-usage_dept = it_cdpos-value_new.
*-Validations for G/L Account
      ELSEIF it_cdpos-tabkey+18(10) = gv_glno.  " P_GL_ACCOUNT
        CONDENSE it_cdpos-value_new NO-GAPS.
        REPLACE '.' WITH space INTO it_cdpos-value_new.
        CONDENSE it_cdpos-value_new NO-GAPS.
        l_gl_acct = it_cdpos-value_new(6).

        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_gl_acct IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_gl_acct IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-gl_acct = l_gl_acct.
*-Validations for Car Usage
      ELSEIF it_cdpos-tabkey+18(10) = gv_usgcar.  " P_USAGE_CAR
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-usage = it_cdpos-value_new.
*-Validations for Usage Text
      ELSEIF it_cdpos-tabkey+18(10) = gv_usgtxt.  " P_USAGE_TEXT
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-usage_text = it_cdpos-value_new.
*-Validations for Scrap Date
      ELSEIF it_cdpos-tabkey+18(10) = gv_scrpdt.  " P_SCRAP_DATE
        CLEAR l_date2.
        CONDENSE it_cdpos-value_new NO-GAPS.
        REPLACE '.' WITH space INTO it_cdpos-value_new.
        CONDENSE it_cdpos-value_new NO-GAPS.
        l_date2 = it_cdpos-value_new(8).
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        PERFORM user_specific_date1 USING  l_date2
                                   CHANGING it_charg1-scrap_date.
*-Validations for Scrap Doc
      ELSEIF it_cdpos-tabkey+18(10) = gv_scrpdoc. " P_SCRAP_APP_DOC
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-scrap_tag_no = it_cdpos-value_new.
*-Validations for Reference Doc
      ELSEIF it_cdpos-tabkey+18(10) = gv_refdoc.  " P_REF_DOC
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-ref_doc = it_cdpos-value_new.
*-Validations for Accounting Doc
      ELSEIF it_cdpos-tabkey+18(10) = gv_acc_doc. " P_ACC_DOC
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-acc_doc = it_cdpos-value_new.
*-Validations for Invoice Doc
      ELSEIF it_cdpos-tabkey+18(10) = gv_invno.  " P_INV_NO
        CONDENSE it_cdpos-value_new NO-GAPS.
        REPLACE '.' WITH space INTO it_cdpos-value_new.
        CONDENSE it_cdpos-value_new NO-GAPS.
        l_inv_doc = it_cdpos-value_new(10).

        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_inv_doc IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_inv_doc IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-invoice = l_inv_doc.

** Changed by Furong on 02/22/08  "UD1K942932
      ELSEIF it_cdpos-tabkey+18(10) = gv_ent_typ.
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-ent_typ = it_cdpos-value_new.

      ELSEIF it_cdpos-tabkey+18(10) = gv_ftz_entry.
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-ftz_entry = it_cdpos-value_new.

      ELSEIF it_cdpos-tabkey+18(10) = gv_duty_paid.
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-duty_paid  = it_cdpos-value_new.

      ELSEIF it_cdpos-tabkey+18(10) = gv_duty_amt.
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-duty_amt = it_cdpos-value_new.

      ELSEIF it_cdpos-tabkey+18(10) = gv_temp_rem.
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-temp_rem = it_cdpos-value_new.

      ELSEIF it_cdpos-tabkey+18(10) = gv_ret_date.
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-ret_date = it_cdpos-value_new.

      ELSEIF it_cdpos-tabkey+18(10) = gv_scrap_rev.
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-scrap_res = it_cdpos-value_new.
** End of change on 02/22/08

** Furong on 01/23/12
      ELSEIF it_cdpos-tabkey+18(10) = gv_yes.
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-yes = it_cdpos-value_new.

      ELSEIF it_cdpos-tabkey+18(10) = gv_desc.
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-desc = it_cdpos-value_new.
** end on 01/23/12

      ENDIF.
    ENDLOOP.    " LOOP AT it_cdpos

    it_charg1-username = it_cdhdr-username.
    it_charg1-udate = it_cdhdr-udate.
    it_charg1-utime = it_cdhdr-utime.
    it_charg1-charg = it_charg-charg.
    it_charg1-del_indicator = it_charg-lvorm.
    it_charg1-car_type = 'XA'.
    APPEND it_charg1.
    CLEAR  it_charg1.

  ENDLOOP.      " LOOP AT it_cdhdr
ENDFORM.                    " get_char_change
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
*&      Form  user_specific_date
*&---------------------------------------------------------------------*
*       Subroutine to get user specific date format
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
      CONCATENATE p_pdate+6(2) p_pdate+4(2) p_pdate+0(4) INTO p_extdate
      SEPARATED BY '.'.

    WHEN 2.
      CONCATENATE p_pdate+4(2) p_pdate+6(2) p_pdate+0(4) INTO p_extdate
      SEPARATED BY '/'.

    WHEN 3.
      CONCATENATE p_pdate+4(2) p_pdate+6(2) p_pdate+0(4) INTO p_extdate
      SEPARATED BY '-'.

    WHEN 4.
      CONCATENATE p_pdate+0(4) p_pdate+4(2) p_pdate+6(2) INTO p_extdate
      SEPARATED BY '.'.

    WHEN 5.
      CONCATENATE p_pdate+0(4) p_pdate+4(2) p_pdate+6(2) INTO p_extdate
      SEPARATED BY '/'.

    WHEN 6.
      CONCATENATE p_pdate+0(4) p_pdate+4(2) p_pdate+6(2) INTO p_extdate
      SEPARATED BY '-'.

  ENDCASE.
ENDFORM.                    " user_specific_date
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
  l_answer(1) TYPE c         .

  DATA: BEGIN OF l_msg OCCURS 100 ,
          txt(100) TYPE c         ,
        END OF l_msg              .

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
*-Display Document
    WHEN 'DIS_DOC'.
      CLEAR l_charg.
      LOOP AT gi_index_rows INTO g_selected_row.
        READ TABLE it_charg1 INDEX g_selected_row-index.
        IF sy-subrc = 0.
          IF it_charg1-charg = l_charg.
            CONTINUE.
          ELSE.
            l_charg = it_charg1-charg.
            SELECT SINGLE doknr
                         FROM draw
                         INTO gv_doknr
                         WHERE dokar = gv_dokar         AND
                               doknr = it_charg1-charg AND
                               dokvr = '00'            AND
                               doktl = '000'.
            IF sy-subrc = 0.
              SET PARAMETER ID 'CV1' FIELD it_charg1-charg.
              SET PARAMETER ID 'CV2' FIELD gv_dokar.
              SET PARAMETER ID 'CV3' FIELD '00' .
              SET PARAMETER ID 'CV4' FIELD '000'.
              CALL TRANSACTION 'CV03N' AND SKIP FIRST SCREEN.
            ELSE.
              CLEAR gv_txt.
              CONCATENATE text-017 it_charg1-charg text-020 INTO gv_txt
                                                    SEPARATED BY space.
              CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
                EXPORTING
                  textline1 = gv_txt.
            ENDIF.
          ENDIF.  " IF it_charg1-charg = l_charg.
        ELSE.
          MESSAGE i000 WITH text-003.
        ENDIF.    " IF sy-subrc = 0.
      ENDLOOP.    " LOOP AT gi_index_rows INTO g_selected_row.
  ENDCASE.
ENDFORM.                    " sub_event_ucomm
*&---------------------------------------------------------------------*
*&      Form  user_specific_date1
*&---------------------------------------------------------------------*
*       Subroutine to get user specific date format
*----------------------------------------------------------------------*
*      -->P_DATA     Date in generic form
*      <--P_EXTDATE  Date in User specific date format
*----------------------------------------------------------------------*
FORM user_specific_date1 USING    p_pdate
                        CHANGING p_extdate.
  DATA: w_datfm(1) TYPE c.
  CALL FUNCTION 'ITS_GET_USER_DEFAULTS'
    EXPORTING
      bname = sy-uname
    IMPORTING
      datfm = w_datfm.

  CASE w_datfm.
    WHEN 1.
      CONCATENATE p_pdate+6(2) p_pdate+4(2) p_pdate+0(4) INTO p_extdate
      SEPARATED BY '.'.

    WHEN 2.
      CONCATENATE p_pdate+4(2) p_pdate+6(2) p_pdate+0(4) INTO p_extdate
      SEPARATED BY '/'.

    WHEN 3.
      CONCATENATE p_pdate+4(2) p_pdate+6(2) p_pdate+0(4) INTO p_extdate
      SEPARATED BY '-'.

    WHEN 4.
      CONCATENATE p_pdate+0(4) p_pdate+4(2) p_pdate+6(2) INTO p_extdate
      SEPARATED BY '.'.

    WHEN 5.
      CONCATENATE p_pdate+0(4) p_pdate+4(2) p_pdate+6(2) INTO p_extdate
      SEPARATED BY '/'.

    WHEN 6.
      CONCATENATE p_pdate+0(4) p_pdate+4(2) p_pdate+6(2) INTO p_extdate
      SEPARATED BY '-'.

  ENDCASE.
ENDFORM.                    " user_specific_date1
*&---------------------------------------------------------------------*
*&      Form  get_char_change3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_char_change3.
  DATA: l_gl_acct TYPE ze_glact,
        l_rp18_shop_date(10) TYPE c,
        l_shipout_date(10)   TYPE c,
        l_ship_date(10)      TYPE c,
        l_date2(8)           TYPE c,
        l_tabkey LIKE cdpos-tabkey ,
        l_flag(1)            TYPE c,
        wa_cdpos             LIKE cdpos,
        l_vin(1)             TYPE c,
        l_vin1(1)            TYPE c,
        l_fsc(1)             TYPE c,
        l_fsc1(1)            TYPE c,
        l_shipdt(1)          TYPE c,
        l_shipdt1(1)         TYPE c,
        l_shopdt(1)          TYPE c,
        l_shopdt1(1)         TYPE c,
        l_scrdt(1)           TYPE c,
        l_scrdt1(1)          TYPE c,
        l_askdept(1)         TYPE c,
        l_askdept1(1)        TYPE c,
        l_udept(1)           TYPE c,
        l_udept1(1)          TYPE c,
        l_glno(1)            TYPE c,
        l_glno1(1)           TYPE c,
        l_enttyp(1)          TYPE c,
        l_ftz(1)             TYPE c,
        l_dpaid(1)           TYPE c,
        l_trem(1)            TYPE c,
        l_rdate(1)           TYPE c,
        l_scrapr(1)          TYPE c,
        l_yes(1)             TYPE c,
        l_yes1(1)            TYPE c..

  DELETE it_cdpos WHERE fname = 'ATCOD'.
  DELETE it_cdpos WHERE chngind = 'I'.

  SORT it_cdpos BY objectid changenr tabname tabkey.
  SORT it_cdhdr BY objectclas objectid changenr udate utime username.

  IF NOT s_vin[] IS INITIAL.
    l_vin = '1'.
  ENDIF.
  IF NOT s_fsc[] IS INITIAL.
    l_fsc = '1'.
  ENDIF.
  IF NOT s_shopdt[] IS INITIAL.
    l_shopdt = '1'.
  ENDIF.
  IF NOT s_shipdt[] IS INITIAL.
    l_shipdt = '1'.
  ENDIF.
  IF NOT s_scrdt[] IS INITIAL.
    l_scrdt = '1'.
  ENDIF.
  IF NOT s_askdep[] IS INITIAL.
    l_askdept = '1'.
  ENDIF.
  IF NOT s_udept[] IS INITIAL.
    l_udept = '1'.
  ENDIF.
  IF NOT s_glno[] IS INITIAL.
    l_glno = '1'.
  ENDIF.
** Changed by Furong on 02/22/08  "UD1K942932
  IF NOT s_enttyp[] IS INITIAL.
    l_enttyp = '1'.
  ENDIF.
  IF NOT s_ftz[] IS INITIAL.
    l_ftz = '1'.
  ENDIF.
  IF NOT s_dpaid[] IS INITIAL.
    l_dpaid = '1'.
  ENDIF.
  IF NOT s_trem[] IS INITIAL.
    l_trem = '1'.
  ENDIF.
  IF NOT s_rdate[] IS INITIAL.
    l_rdate = '1'.
  ENDIF.
  IF NOT s_scrapr[] IS INITIAL.
    l_scrapr = '1'.
  ENDIF.
** End of change on 02/22/08
  IF NOT s_yes[] IS INITIAL.
    l_yes = '1'.
  ENDIF.

  LOOP AT it_cdhdr.
    LOOP AT it_cdpos WHERE changenr = it_cdhdr-changenr.
      l_flag = '0'.
      IF it_cdpos-tabkey+18(10) = gv_vin.         " P_VIN
*-Validations for VIN Number
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        IF s_vin-low IS INITIAL AND s_vin-high IS INITIAL.
        ELSEIF NOT ( s_vin-low IS INITIAL ) AND s_vin-high IS INITIAL.
          READ TABLE it_vin WITH KEY vin = it_cdpos-value_new
                                                        BINARY SEARCH.
          IF sy-subrc <> 0.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF s_vin-low IS INITIAL AND NOT ( s_vin-high IS INITIAL ).
          IF it_cdpos-value_new > s_vin-high.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF NOT ( s_vin-low IS INITIAL AND s_vin-high IS INITIAL ).
          IF ( it_cdpos-value_new < s_vin-low OR
                                    it_cdpos-value_new > s_vin-high ).
            l_flag = '1'.
            EXIT.
          ENDIF.
        ENDIF.
        it_charg1-vin_num = it_cdpos-value_new.
        l_vin1 = '1'.
*-Validations for FSC
      ELSEIF it_cdpos-tabkey+18(10) = gv_fsc.     " P_FSC
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'

        IF s_fsc-low IS INITIAL AND s_fsc-high IS INITIAL.
        ELSEIF NOT ( s_fsc-low IS INITIAL ) AND s_fsc-high IS INITIAL.
          READ TABLE it_fsc WITH KEY fsc = it_cdpos-value_new
                                                        BINARY SEARCH.
          IF sy-subrc <> 0.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF s_fsc-low IS INITIAL AND NOT ( s_fsc-high IS INITIAL ).
          IF it_cdpos-value_new > s_fsc-high.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF NOT ( s_fsc-low IS INITIAL AND s_fsc-high IS INITIAL ).
          IF ( it_cdpos-value_new < s_fsc-low OR
                                    it_cdpos-value_new > s_fsc-high ).
            l_flag = '1'.
            EXIT.
          ENDIF.
        ENDIF.
        it_charg1-fsc = it_cdpos-value_new.
        l_fsc1 = '1'.

*-Validations for Shop date
      ELSEIF it_cdpos-tabkey+18(10) = gv_shopdt.  " P_RP18_SHOP_DATE
        CONDENSE it_cdpos-value_new NO-GAPS.
        CLEAR l_date2.
        REPLACE '.' WITH space INTO it_cdpos-value_new.
        CONDENSE it_cdpos-value_new NO-GAPS.
        l_date2 = it_cdpos-value_new(8).
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        PERFORM user_specific_date1 USING  l_date2
                                   CHANGING it_charg1-rp18_shop_date.
        IF s_shopdt-low IS INITIAL AND s_shopdt-high IS INITIAL.
        ELSEIF NOT ( s_shopdt-low IS INITIAL ) AND
                                s_shopdt-high IS INITIAL.
          READ TABLE it_shop_dt WITH KEY date = l_date2 BINARY SEARCH.
          IF sy-subrc <> 0.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF s_shopdt-low IS INITIAL AND
                                  NOT ( s_shopdt-high IS INITIAL ).
          IF l_date2 > s_shopdt-high.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF NOT ( s_shopdt-low IS INITIAL AND
                                          s_shopdt-high IS INITIAL ).
          IF ( l_date2 < s_shopdt-low OR l_date2 > s_shopdt-high ).
            l_flag = '1'.
            EXIT.
          ENDIF.
        ENDIF.
        l_shopdt1 = '1'.
*-Validations for Ship Out Date
      ELSEIF it_cdpos-tabkey+18(10) = gv_shipdt.  " P_SHIPOUT_DATE
        CONDENSE it_cdpos-value_new NO-GAPS.
        CLEAR l_date2.
        REPLACE '.' WITH space INTO it_cdpos-value_new.
        CONDENSE it_cdpos-value_new NO-GAPS.
        l_date2 = it_cdpos-value_new(8).
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        PERFORM user_specific_date1 USING  l_date2
                                   CHANGING it_charg1-ship_date.
        IF s_shipdt-low IS INITIAL AND s_shipdt-high IS INITIAL.
        ELSEIF NOT ( s_shipdt-low IS INITIAL ) AND
                                             s_shipdt-high IS INITIAL.
          READ TABLE it_ship_dt WITH KEY date = l_date2 BINARY SEARCH.
          IF sy-subrc <> 0.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF s_shipdt-low IS INITIAL AND
                                       NOT ( s_shipdt-high IS INITIAL ).
          IF l_date2 > s_shipdt-high.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF NOT ( s_shipdt-low IS INITIAL AND
                                             s_shipdt-high IS INITIAL ).
          IF ( l_date2 < s_shipdt-low OR l_date2 > s_shipdt-high ).
            l_flag = '1'.
            EXIT.
          ENDIF.
        ENDIF.
        l_shipdt1 = '1'.

*-Validations for Asking Department
      ELSEIF it_cdpos-tabkey+18(10) = gv_askdept. " P_ASK_DEPT
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-ask_dept = it_cdpos-value_new.
        IF s_askdep-low IS INITIAL AND s_askdep-high IS INITIAL.
        ELSEIF NOT ( s_askdep-low IS INITIAL ) AND
                                            s_askdep-high IS INITIAL.
          READ TABLE it_askdep WITH KEY askdept = it_charg1-ask_dept
                                                       BINARY SEARCH.
          IF sy-subrc <> 0.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF s_askdep-low IS INITIAL AND
                                     NOT ( s_askdep-high IS INITIAL ).
          IF it_charg1-ask_dept > s_askdep-high.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF NOT ( s_askdep-low IS INITIAL AND
                                           s_askdep-high IS INITIAL ).
          IF ( it_charg1-ask_dept < s_askdep-low OR
                                 it_charg1-ask_dept > s_askdep-high ).
            l_flag = '1'.
            EXIT.
          ENDIF.
        ENDIF.
        l_askdept1 = '1'.
*-Validations for Destination/Using Department
      ELSEIF it_cdpos-tabkey+18(10) = gv_usagedep.    " P_USAGE_DEPT
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-usage_dept = it_cdpos-value_new.
        IF s_udept-low IS INITIAL AND s_udept-high IS INITIAL.
        ELSEIF NOT ( s_udept-low IS INITIAL ) AND
                                              s_udept-high IS INITIAL.
          READ TABLE it_usagedep WITH KEY
                      usage_dept = it_charg1-usage_dept BINARY SEARCH.
          IF sy-subrc <> 0.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF s_udept-low IS INITIAL AND NOT
                                          ( s_udept-high IS INITIAL ).
          IF it_charg1-usage_dept > s_udept-high.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF NOT ( s_udept-low IS INITIAL AND
                                            s_udept-high IS INITIAL ).
          IF ( it_charg1-usage_dept < s_udept-low OR
                                it_charg1-usage_dept > s_udept-high ).
            l_flag = '1'.
            EXIT.
          ENDIF.
        ENDIF.
        l_udept1 = '1'.
*-Validations for G/L Account
      ELSEIF it_cdpos-tabkey+18(10) = gv_glno.  " P_GL_ACCOUNT
        CONDENSE it_cdpos-value_new NO-GAPS.
        REPLACE '.' WITH space INTO it_cdpos-value_new.
        CONDENSE it_cdpos-value_new NO-GAPS.
        l_gl_acct = it_cdpos-value_new(6).

        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_gl_acct IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_gl_acct IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        IF s_glno-low IS INITIAL AND s_glno-high IS INITIAL.
        ELSEIF NOT ( s_glno-low IS INITIAL ) AND s_glno-high IS INITIAL.
          READ TABLE it_glno WITH KEY glno = l_gl_acct BINARY SEARCH.
          IF sy-subrc <> 0.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF s_glno-low IS INITIAL AND NOT ( s_glno-high IS INITIAL ).
          IF l_gl_acct > s_glno-high.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF NOT ( s_glno-low IS INITIAL AND s_glno-high IS INITIAL ).
          IF ( l_gl_acct < s_glno-low OR l_gl_acct > s_glno-high ).
            l_flag = '1'.
            EXIT.
          ENDIF.
        ENDIF.
        l_glno1 = '1'.

** furong on 01/23/12
      ELSEIF it_cdpos-tabkey+18(10) = gv_yes.    " P_YES
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        it_charg1-yes = it_cdpos-value_new.
        IF s_yes-low IS INITIAL AND s_yes-high IS INITIAL.
        ELSEIF NOT ( s_yes-low IS INITIAL ) AND
                                              s_yes-high IS INITIAL.
          READ TABLE it_yes WITH KEY
                      yes = it_charg1-yes BINARY SEARCH.
          IF sy-subrc <> 0.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF s_yes-low IS INITIAL AND NOT
                                          ( s_yes-high IS INITIAL ).
          IF it_charg1-yes > s_yes-high.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF NOT ( s_yes-low IS INITIAL AND
                                            s_yes-high IS INITIAL ).
          IF ( it_charg1-yes < s_yes-low OR
                                it_charg1-yes > s_yes-high ).
            l_flag = '1'.
            EXIT.
          ENDIF.
        ENDIF.
        l_yes1 = '1'.

** end on 01/23/12
*-Validations for Scrap Date
      ELSEIF it_cdpos-tabkey+18(10) = gv_scrpdt.  " P_SCRAP_DATE
        CONDENSE it_cdpos-value_new NO-GAPS.
        CLEAR l_date2.
        REPLACE '.' WITH space INTO it_cdpos-value_new.
        CONDENSE it_cdpos-value_new NO-GAPS.
        l_date2 = it_cdpos-value_new(8).
        IF it_cdpos-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos-tabkey+30(1) = '1'
        PERFORM user_specific_date1 USING  l_date2
                                   CHANGING it_charg1-scrap_date.
        IF s_scrdt-low IS INITIAL AND s_scrdt-high IS INITIAL.
        ELSEIF NOT ( s_scrdt-low IS INITIAL ) AND
                                             s_scrdt-high IS INITIAL.
          READ TABLE it_scrp_dt WITH KEY date = l_date2 BINARY SEARCH.
          IF sy-subrc <> 0.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF s_scrdt-low IS INITIAL AND
                                       NOT ( s_scrdt-high IS INITIAL ).
          IF l_date2 > s_scrdt-high.
            l_flag = '1'.
            EXIT.
          ENDIF.
        ELSEIF NOT ( s_scrdt-low IS INITIAL AND
                                             s_scrdt-high IS INITIAL ).
          IF ( l_date2 < s_scrdt-low OR l_date2 > s_scrdt-high ).
            l_flag = '1'.
            EXIT.
          ENDIF.
        ENDIF.
        l_scrdt1 = '1'.
      ENDIF.    " IF it_cdpos-tabkey+18(10) = gv_vin
    ENDLOOP.    " LOOP AT it_cdpos

    IF l_flag = '1'.
      DELETE it_cdpos WHERE changenr = it_cdhdr-changenr.
    ELSE.
      IF l_vin = '1'.
        IF l_vin1 <> '1'.
          DELETE it_cdpos WHERE changenr = it_cdhdr-changenr.
        ENDIF.
      ENDIF.

      IF l_fsc = '1'.
        IF l_fsc1 <> '1'.
          DELETE it_cdpos WHERE changenr = it_cdhdr-changenr.
        ENDIF.
      ENDIF.

      IF l_shopdt = '1'.
        IF l_shopdt1 <> '1'.
          DELETE it_cdpos WHERE changenr = it_cdhdr-changenr.
        ENDIF.
      ENDIF.

      IF l_shipdt = '1'.
        IF l_shipdt1 <> '1'.
          DELETE it_cdpos WHERE changenr = it_cdhdr-changenr.
        ENDIF.
      ENDIF.

      IF l_scrdt = '1'.
        IF l_scrdt1 <> '1'.
          DELETE it_cdpos WHERE changenr = it_cdhdr-changenr.
        ENDIF.
      ENDIF.

      IF l_askdept = '1'.
        IF l_askdept1 <> '1'.
          DELETE it_cdpos WHERE changenr = it_cdhdr-changenr.
        ENDIF.
      ENDIF.

      IF l_udept = '1'.
        IF l_udept1 <> '1'.
          DELETE it_cdpos WHERE changenr = it_cdhdr-changenr.
        ENDIF.
      ENDIF.

      IF l_glno = '1'.
        IF l_glno1 <> '1'.
          DELETE it_cdpos WHERE changenr = it_cdhdr-changenr.
        ENDIF.
      ENDIF.

** Fuorng on 02/23/12
      IF l_yes = '1'.
        IF l_yes1 <> '1'.
          DELETE it_cdpos WHERE changenr = it_cdhdr-changenr.
        ENDIF.
      ENDIF.
** end pn 01/23/12

    ENDIF.
    CLEAR: l_vin1, l_fsc1, l_shopdt1, l_shipdt1, l_scrdt1,
           l_askdept1, l_udept1, l_glno1.
  ENDLOOP.      " LOOP AT it_cdhdr

ENDFORM.                    " get_char_change3
*&---------------------------------------------------------------------*
*&      Form  get_char_change4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_char_change4.
  DATA: l_gl_acct TYPE ze_glact,
        l_rp18_shop_date(10) TYPE c,
        l_shipout_date(10)   TYPE c,
        l_ship_date(10)      TYPE c,
        l_date2(8)           TYPE c,
        l_tabkey LIKE cdpos-tabkey  ,
        wa_cdpos             LIKE cdpos.

  LOOP AT it_cdhdr.
    LOOP AT it_cdpos1 WHERE changenr = it_cdhdr-changenr.
      IF it_cdpos1-tabkey+18(10) = gv_vin.         " P_VIN
*-Validations for VIN Number
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-vin_num = it_cdpos1-value_new.
*-FSC
      ELSEIF it_cdpos1-tabkey+18(10) = gv_fsc.     " P_FSC
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-fsc = it_cdpos1-value_new.
*-Exterior Color
      ELSEIF it_cdpos1-tabkey+18(10) = gv_exclr.   " P_EXT_COLOR
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-ext_clr = it_cdpos1-value_new.
*-Interior color
      ELSEIF it_cdpos1-tabkey+18(10) = gv_inclr.   " P_INT_COLOR
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-int_clr = it_cdpos1-value_new.
*-Validations for Shop date
      ELSEIF it_cdpos1-tabkey+18(10) = gv_shopdt.  " P_RP18_SHOP_DATE
        CONDENSE it_cdpos1-value_new NO-GAPS.
        CLEAR l_date2.
        REPLACE '.' WITH space INTO it_cdpos1-value_new.
        CONDENSE it_cdpos1-value_new NO-GAPS.
        l_date2 = it_cdpos1-value_new(8).
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        PERFORM user_specific_date1 USING  l_date2
                                   CHANGING it_charg1-rp18_shop_date.
*-Validations for Ship Out Date
      ELSEIF it_cdpos1-tabkey+18(10) = gv_shipdt.  " P_SHIPOUT_DATE
        CONDENSE it_cdpos1-value_new NO-GAPS.
        CLEAR l_date2.
        REPLACE '.' WITH space INTO it_cdpos1-value_new.
        CONDENSE it_cdpos1-value_new NO-GAPS.
        l_date2 = it_cdpos1-value_new(8).
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        PERFORM user_specific_date1 USING  l_date2
                                   CHANGING it_charg1-ship_date.
*-Validations for Asking Department
      ELSEIF it_cdpos1-tabkey+18(10) = gv_askdept. " P_ASK_DEPT
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-ask_dept = it_cdpos1-value_new.
*-Validations for Destination/Using Department
      ELSEIF it_cdpos1-tabkey+18(10) = gv_usagedep.    " P_USAGE_DEPT
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-usage_dept = it_cdpos1-value_new.
*-Validations for G/L Account
      ELSEIF it_cdpos1-tabkey+18(10) = gv_glno.  " P_GL_ACCOUNT
        CONDENSE it_cdpos-value_new NO-GAPS.
        REPLACE '.' WITH space INTO it_cdpos-value_new.
        CONDENSE it_cdpos-value_new NO-GAPS.
        l_gl_acct = it_cdpos-value_new(6).

        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_gl_acct IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_gl_acct IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
*-Usage Car
      ELSEIF it_cdpos1-tabkey+18(10) = gv_usgcar.  " P_USAGE_CAR
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-usage = it_cdpos1-value_new.
*-Usage Text
      ELSEIF it_cdpos1-tabkey+18(10) = gv_usgtxt.  " P_USAGE_TEXT
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-usage_text = it_cdpos1-value_new.
*-Scrap Date
      ELSEIF it_cdpos1-tabkey+18(10) = gv_scrpdt.  " P_SCRAP_DATE
        CLEAR l_date2.
        CONDENSE it_cdpos1-value_new NO-GAPS.
        REPLACE '.' WITH space INTO it_cdpos1-value_new.
        CONDENSE it_cdpos1-value_new NO-GAPS.
        l_date2 = it_cdpos1-value_new(8).
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF l_date2 CS '00000000'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        PERFORM user_specific_date1 USING  l_date2
                                   CHANGING it_charg1-scrap_date.
*-Scrap Document
      ELSEIF it_cdpos1-tabkey+18(10) = gv_scrpdoc. " P_SCRAP_APP_DOC
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-scrap_tag_no = it_cdpos1-value_new.
*-Reference Document
      ELSEIF it_cdpos1-tabkey+18(10) = gv_refdoc.  " P_REF_DOC
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-ref_doc = it_cdpos1-value_new.

** Changed by Furong on 02/22/08  "UD1K942932
      ELSEIF it_cdpos1-tabkey+18(10) = gv_ent_typ.
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-ent_typ = it_cdpos1-value_new.

      ELSEIF it_cdpos1-tabkey+18(10) = gv_ftz_entry.
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-ftz_entry = it_cdpos1-value_new.

      ELSEIF it_cdpos1-tabkey+18(10) = gv_duty_paid.
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-duty_paid = it_cdpos1-value_new.

      ELSEIF it_cdpos1-tabkey+18(10) = gv_duty_amt.
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-duty_amt = it_cdpos1-value_new.

      ELSEIF it_cdpos1-tabkey+18(10) = gv_temp_rem.
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-temp_rem = it_cdpos1-value_new.

      ELSEIF it_cdpos1-tabkey+18(10) = gv_ret_date.
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-ret_date = it_cdpos1-value_new.

      ELSEIF it_cdpos1-tabkey+18(10) = gv_scrap_rev.
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-scrap_res = it_cdpos1-value_new.
** End of change on 02/22/08

** Furong on 01/23/12
      ELSEIF it_cdpos1-tabkey+18(10) = gv_yes.
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-yes = it_cdpos1-value_new.
      ELSEIF it_cdpos1-tabkey+18(10) = gv_desc.
        IF it_cdpos1-tabkey+30(1) = '1'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '2'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF it_cdpos1-tabkey+30(1) = '2'.
          CLEAR l_tabkey.
          l_tabkey = it_cdpos1-tabkey.
          l_tabkey+30(1) = '1'.
          READ TABLE it_cdpos1 INTO wa_cdpos WITH KEY
                      changenr = it_cdhdr-changenr tabkey = l_tabkey.
          IF sy-subrc = 0.
            IF it_cdpos1-value_new IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.    " IF it_cdpos1-tabkey+30(1) = '1'
        it_charg1-desc = it_cdpos1-value_new.

** end on 01/23/12

      ENDIF.
    ENDLOOP.    " LOOP AT it_cdpos1

    READ TABLE it_inob WITH KEY objectid = it_cdhdr-objectid.
    IF sy-subrc = 0.
      it_charg1-charg = it_inob-charg.
      it_charg1-username = it_cdhdr-username.
      it_charg1-udate = it_cdhdr-udate.
      it_charg1-utime = it_cdhdr-utime.
      it_charg1-del_indicator = it_inob-lvorm.
      it_charg1-car_type = 'XA'.
      APPEND it_charg1.
      CLEAR  it_charg1.
    ENDIF.
  ENDLOOP.      " LOOP AT it_cdhdr
ENDFORM.                    " get_char_change4
*&---------------------------------------------------------------------*
*&      Form  CALL_FB03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_fb03.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i,
        l_year(4).

  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = sy-repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH 'No data was selected'.
  ENDIF.
  READ TABLE it_charg1 INDEX lt_rows-index.
  IF it_charg1-acc_doc = ' '.
    MESSAGE e000 WITH text-m13.
  ENDIF.
  l_year = it_charg1-udate+0(4).
  SET PARAMETER ID 'BLN' FIELD it_charg1-acc_doc.
  SET PARAMETER ID 'GJR' FIELD l_year.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

ENDFORM.                                                    " CALL_FB03
