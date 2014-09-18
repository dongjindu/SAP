*----------------------------------------------------------------------
* Program ID        : ZR_MM_VEHICLE_CREATE
* Title             : Create Test Vehicle Tracking record
* Created on        : 11/09/2007
* Created by        : Rakesh Gandhi
* Specifications By : Paul Shrewsbury
* Description       : This program creates test vehicle tracking records
*----------------------------------------------------------------------
REPORT zr_mm_vehicle_create MESSAGE-ID zmco.
TYPE-POOLS : slis,
             icon.

TABLES: ausp,
        t100,
        klah,
        csks,
        ska1.


*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: BEGIN OF it_ausp OCCURS 0    ,
        objek LIKE ausp-objek      ,
        atinn LIKE ausp-atinn      ,
        atwrt LIKE ausp-atwrt      ,
        atflv LIKE ausp-atflv      ,
      END OF it_ausp               ,

      BEGIN OF it_ausp_tmp OCCURS 0,
        objek LIKE ausp-objek      ,
        atwrt LIKE ausp-atwrt      ,
        atflv LIKE ausp-atflv      ,
      END OF it_ausp_tmp           ,

      BEGIN OF it_charg OCCURS 0   ,
        charg LIKE mcha-charg      ,
      END OF it_charg              .

DATA: vin_num    TYPE ze_vin       ,
      usage_dept TYPE ze_usage_dept.

DATA: BEGIN OF it_ausp1 OCCURS 0              ,
        objek              LIKE ausp-objek    ,
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

**Changed by Furong on 02/20/08  "UD1K942932
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
        usage_dept         TYPE ze_ask_dept   ,
        ref_doc(15)        TYPE c             ,
        final_dest(15)     TYPE c             ,
        ship_date(10)      TYPE c             ,
        ship_docs(12)      TYPE c             ,
        gl_acct(10)        TYPE c             ,
        acc_doc(10)        TYPE c             ,
        invoice            TYPE ze_invoice    ,
        chkbox(1)          TYPE c             ,
        icon               TYPE icon_d        ,
        tabcolor           TYPE slis_t_specialcol_alv,
      END OF it_ausp1                         ,

      BEGIN OF it_cabn OCCURS 0               ,
        atinn LIKE cabn-atinn                 ,
        atnam LIKE cabn-atnam                 ,
        atfor LIKE cabn-atfor                 ,
      END OF it_cabn                          ,

      BEGIN OF it_ksml OCCURS 0               ,
        imerk LIKE ksml-imerk                 ,
      END OF it_ksml                          ,

      BEGIN OF it_rettab OCCURS 0 .
        INCLUDE STRUCTURE bapiret2.
DATA: END OF it_rettab            ,

      BEGIN OF it_rettab1 OCCURS 0.
        INCLUDE STRUCTURE bapiret2.
DATA: END OF it_rettab1           ,

      BEGIN OF it_object OCCURS 0 .
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

DATA: BEGIN OF it_email OCCURS 0,
        objek   LIKE ausp-objek,
      END OF it_email.

DATA: it_ausp_tmp1   LIKE it_ausp_tmp OCCURS 0 WITH HEADER LINE,
      it_cabn1 LIKE it_cabn OCCURS 0 WITH HEADER LINE     ,
      custom_control TYPE   scrfname VALUE 'ALV_CONTAINER',
      alv_grid       TYPE   REF TO cl_gui_alv_grid        ,
      grid_container TYPE   REF TO cl_gui_custom_container,
      event_receiver TYPE   REF TO lcl_event_receiver     .

*-ALV Data declaration
DATA : gt_fieldcat TYPE lvc_t_fcat WITH HEADER LINE,
       gv_repid     LIKE sy-repid              ,
       gv_variant   TYPE disvariant            , "for paramtr IS_VARIANT
       gv_save      TYPE c   VALUE 'A'         , " for Parameter I_SAVE
       gv_charg     LIKE mcha-charg            ,
       gv_object    LIKE bapi1003_key-object   ,
       gv_answer(1) TYPE c                     ,
       ok_code      LIKE sy-ucomm              ,
       gv_dokar     LIKE draw-dokar VALUE 'ICD',
       gv_doknr     LIKE draw-doknr            ,
       gv_vin       LIKE cabn-atinn            ,
       gv_askdept   LIKE cabn-atinn            ,
       gv_usagedep  LIKE cabn-atinn            ,
       gv_dest      LIKE cabn-atinn            ,
       gv_glno      LIKE cabn-atinn            ,
       gv_exclr     LIKE cabn-atinn            ,
       gv_inclr     LIKE cabn-atinn            ,
       gv_shopdt    LIKE cabn-atinn            ,
       gv_usgcar    LIKE cabn-atinn            ,
       gv_shipdt    LIKE cabn-atinn            ,
       gv_usgtxt    LIKE cabn-atinn            ,
       gv_refdoc    LIKE cabn-atinn            ,
       gv_ask_div   LIKE cabn-atinn            ,
       gv_ship_doc  LIKE cabn-atinn            ,
       gv_acc_doc   LIKE cabn-atinn            ,
       gv_invno     LIKE cabn-atinn            ,
       gv_modelyr   LIKE cabn-atinn            ,
       gv_nat_code  LIKE cabn-atinn            ,
       gv_mi        LIKE cabn-atinn            ,
       gv_dist_cd   LIKE cabn-atinn            ,
       gv_ocn       LIKE cabn-atinn            ,
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

RANGES: r_atinn FOR ausp-atinn,
        r_atnam FOR cabn-atnam.
CONSTANTS: c_matnr LIKE mara-matnr  VALUE 'TEST VEHICLE',
           c_werks LIKE t001w-werks VALUE 'P001'         ,
           c_klart LIKE klah-klart  VALUE '022'          ,
           c_class LIKE klah-class  VALUE 'P_VEHICLE_TRACKING'.

DATA :  gs_f4        TYPE lvc_s_f4,
        gt_f4        TYPE lvc_t_f4.

DATA : BEGIN OF gt_vtype OCCURS 3,
           vtype(1),
           text(50),
       END OF gt_vtype.

* Define internal tables &sstructures for Possible Entry
DATA : gs_values TYPE seahlpres,
       gt_fields TYPE TABLE OF dfies WITH HEADER LINE,
       gt_values TYPE TABLE OF seahlpres WITH HEADER LINE,
       gs_fields TYPE dfies,
       ls_f4     TYPE ddshretval,
       ls_modi   TYPE lvc_s_modi.

FIELD-SYMBOLS : <f4tab> TYPE lvc_t_modi.

TYPES ddshretval_table TYPE TABLE OF ddshretval.


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
    DATA: ls_toolbar  TYPE stb_button.
    CONSTANTS: c_separator               TYPE i VALUE 3.

*-Append seperator to the normal toolbar
    CLEAR ls_toolbar.
    MOVE c_separator TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.
*-Append a new button to the toolbar. Use E_OBJECT of event
*-toolbar. E_OBJECT is of type CL_ALV_EVENT_TOOLBAR_SET.
*-This class has one attribute MT_TOOLBAR which is of table type
*-TTB_BUTTON. The structure is STB_BUTTON

*-Create Batch button
    CLEAR ls_toolbar.
    MOVE 'CR_BATCH'      TO ls_toolbar-function.
    MOVE  icon_create    TO ls_toolbar-icon.
    MOVE 'CREATE BATCH'  TO ls_toolbar-quickinfo.
    MOVE 'Create Batch'  TO ls_toolbar-text.
    MOVE ' '             TO ls_toolbar-disabled.
    APPEND ls_toolbar    TO e_object->mt_toolbar.

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

  ENDMETHOD.                    " handle_toolbar

  METHOD handle_user_command.
    PERFORM sub_event_ucomm USING e_ucomm.
  ENDMETHOD.                    " handle_user_command

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
  ENDMETHOD.                    "MY_F4
* }
ENDCLASS.                    " LCL_EVENT_RECEIVER IMPLEMENTATION
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-016.
PARAMETERS: p_body RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND ucom,
            p_vin  RADIOBUTTON GROUP g1.

SELECT-OPTIONS: s_body FOR ausp-objek NO INTERVALS MODIF ID 127," Obj No
                s_vin  FOR vin_num    NO INTERVALS MODIF ID 128." VIN No

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = 'Test Vehicle Tracking - Create'.
  PERFORM get_characteristics.
  PERFORM populate_ranges.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF sy-ucomm <> 'UCOM'.
*-Body Number
    IF p_body = 'X'.
      IF s_body[] IS INITIAL.
        MESSAGE e000 WITH text-017.
      ENDIF.
*-VIN Number
    ELSEIF p_vin = 'X'.
      IF s_vin[] IS INITIAL.
        MESSAGE e000 WITH text-018.
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

  CHECK NOT it_ausp1[] IS INITIAL.
  CALL SCREEN 9000.
*&---------------------------------------------------------------------*
*&      Form  populate_ranges
*&---------------------------------------------------------------------*
*       Subroutine to populate ranges for internal characteristic
*----------------------------------------------------------------------*
FORM populate_ranges.
  r_atinn-sign = 'I'.
  r_atinn-option = 'EQ'.
  LOOP AT it_cabn1.
    r_atinn-low = it_cabn1-atinn.
    APPEND r_atinn.
  ENDLOOP.
ENDFORM.                    " populate_ranges
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Subroutine to get data from DB table
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_tabix        LIKE sy-tabix,
        l_answer(1)    TYPE c       ,
        l_txt(100)     TYPE c       ,
        l_model_yr(1)  TYPE c       ,  " Model year
        l_model_idx(9) TYPE c       ,  " Model Index
        l_code(3)      TYPE c       ,  " Nation Code for SD
        l_dist(2)      TYPE c       ,  " Distributor for SD
        l_ocn(4)       TYPE c       ,  " O.C.N
        l_date         LIKE cawn-atwrt.

  IF p_body = 'X'.
    IF NOT it_cabn1[] IS INITIAL.
      SELECT objek FROM ausp
                   INTO TABLE it_ausp_tmp
                   WHERE objek IN s_body  AND
                         atinn IN r_atinn AND
                         klart = '002'.
    ENDIF.
  ELSEIF p_vin = 'X'.
    SELECT objek atwrt FROM ausp
                 INTO TABLE it_ausp_tmp
                 WHERE atinn = 'P_VIN'  AND
                       atwrt IN s_vin   AND
                       klart = '002'.
  ENDIF.

  IF it_ausp_tmp[] IS INITIAL.
    MESSAGE i000 WITH text-001.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT it_ausp_tmp BY objek.
  DELETE ADJACENT DUPLICATES FROM it_ausp_tmp COMPARING objek.

  IF NOT it_ausp_tmp[] IS INITIAL.
    SELECT charg FROM mcha
           INTO TABLE it_charg
           FOR ALL ENTRIES IN it_ausp_tmp
           WHERE matnr = c_matnr AND
                 werks = c_werks AND
                 charg = it_ausp_tmp-objek(10).
    IF NOT it_charg[] IS INITIAL.
      SORT it_charg BY charg.
      LOOP AT it_ausp_tmp.
        l_tabix = sy-tabix.
        READ TABLE it_charg WITH KEY charg = it_ausp_tmp-objek
                                                BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING it_ausp_tmp TO it_ausp_tmp1.
          APPEND it_ausp_tmp1.
          CLEAR  it_ausp_tmp1.
          DELETE it_ausp_tmp INDEX l_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.  " IF NOT it_charg[] IS INITIAL.
    IF it_ausp_tmp[] IS INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          textline1      = text-019
          textline2      = text-020
          titel          = 'Check!'
          cancel_display = ' '
        IMPORTING
          answer         = l_answer.

      IF l_answer = 'J'.
        IF p_body = 'X'.
          SUBMIT zr_mm_vehicle_change
                 WITH s_body IN s_body.
        ELSEIF p_vin = 'X'.
          CLEAR s_body[].
          s_body-sign = 'I'.
          s_body-option = 'EQ'.
          LOOP AT it_ausp_tmp1.
            s_body-low = it_ausp_tmp1-objek.
            APPEND s_body.
          ENDLOOP.
          SUBMIT zr_mm_vehicle_change
         WITH s_body IN s_body.
        ENDIF.
      ELSE.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ELSEIF NOT it_ausp_tmp1[] IS INITIAL.
      IF p_body = 'X'.
        LOOP AT it_ausp_tmp1.
          CLEAR l_txt.
          CONCATENATE text-006 it_ausp_tmp1-objek text-022 INTO l_txt
                                                  SEPARATED BY space.
          CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
            EXPORTING
              textline1 = l_txt.
        ENDLOOP.
      ELSEIF p_vin = 'X'.
        LOOP AT it_ausp_tmp1.
          CONCATENATE text-006 it_ausp_tmp1-objek
                      text-025 it_ausp_tmp1-atwrt
                      text-022 INTO l_txt SEPARATED BY space.
          CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
            EXPORTING
              textline1 = l_txt.
        ENDLOOP.
      ENDIF.    " IF p_body = 'X'.
    ENDIF.      " IF it_ausp_tmp[] IS INITIAL.

    SELECT objek atinn atwrt atflv
                 FROM ausp
                 INTO TABLE it_ausp
                 FOR ALL ENTRIES IN it_ausp_tmp
                 WHERE objek = it_ausp_tmp-objek AND
                       atinn IN r_atinn          AND
                       klart = '002'.
  ELSE.
    MESSAGE i000 WITH text-001.
    LEAVE LIST-PROCESSING.
  ENDIF.    " IF NOT it_ausp_tmp[] IS INITIAL.

  SORT it_ausp BY objek.
  LOOP AT it_ausp.
    IF it_ausp-atinn = gv_vin.           " VIN Number
      it_ausp1-vin_num = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_exclr.     " Exterior Color
      it_ausp1-ext_clr = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_inclr.     " Interior Color
      it_ausp1-int_clr = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_shopdt.    " RP18 shopdate
      CALL FUNCTION 'CTCV_CONVERT_FLOAT_TO_DATE'
        EXPORTING
          float = it_ausp-atflv
        IMPORTING
          date  = l_date.
      PERFORM user_specific_date USING  l_date
                                 CHANGING it_ausp1-rp18_shop_date.

    ELSEIF it_ausp-atinn = gv_usgcar.    " Usage Car
      it_ausp1-usage = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_usgtxt.    " Usage Text
      it_ausp1-usage_text = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_ask_div.   " Asking Division
      it_ausp1-ask_div = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_askdept.   " Asking Dept
      it_ausp1-ask_dept = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_usagedep.  " Using Dept/Destination
      it_ausp1-usage_dept = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_refdoc.    " Reference Doc
      it_ausp1-ref_doc = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_dest.      " Final Destination
      it_ausp1-final_dest = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_shipdt.    " Ship Out Date
      it_ausp1-ship_date = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_ship_doc.  " Ship Out Docuement
      it_ausp1-ship_docs = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_glno.      " GL Account
      it_ausp1-gl_acct = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_acc_doc.   " Accounting Doc
      it_ausp1-acc_doc = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_invno.     " Invoice Number
      it_ausp1-invoice = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_modelyr.   " Model Year
      l_model_yr = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_nat_code.  " Nation Code
      l_code = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_dist_cd.   " Distributor for SD
      l_dist = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_mi.        " Model index
      l_model_idx = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_ocn.       " O.C.N.
      l_ocn = it_ausp-atwrt.
**Changed by Furong on 02/20/08  "UD1K942932
    ELSEIF it_ausp-atinn = gv_ent_typ.
      it_ausp1-ent_typ = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_ftz_entry.
      it_ausp1-ftz_entry = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_duty_paid.
      it_ausp1-duty_paid = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_duty_amt.
      it_ausp1-duty_amt = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_temp_rem.
      it_ausp1-temp_rem = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_ret_date.
      it_ausp1-ret_date = it_ausp-atwrt.
**End of change on 02/20/08  "UD1K942932

**Changed by Furong on 06/21/10
    ELSEIF it_ausp-atinn = gv_doc_pvt.
      it_ausp1-doc_pvt = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_doc_trf.
      it_ausp1-doc_trf = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_doc_meraf.
      it_ausp1-doc_meraf = it_ausp-atwrt.
    ELSEIF it_ausp-atinn = gv_doc_bol.
      it_ausp1-doc_bol = it_ausp-atwrt.
**End of change
* by ig.moon 6/3/2010 {
    ELSEIF it_ausp-atinn = gv_used_for.
      it_ausp1-vtype = it_ausp-atwrt.
* }
    ENDIF.

    AT END OF objek.
*      CONCATENATE l_model_yr l_code l_dist l_model_idx l_ocn
*                                INTO it_ausp1-fsc.  " Full Spec code
      CONCATENATE l_model_yr l_code l_dist l_model_idx
                                 INTO it_ausp1-fsc.  " Full Spec code
      CONCATENATE it_ausp1-fsc l_ocn INTO it_ausp1-fsc
                                SEPARATED BY space.  " Full Spec code
      it_ausp1-objek = it_ausp-objek.
      it_ausp1-car_type = 'XA'.

      APPEND it_ausp1.
      CLEAR  it_ausp1.
    ENDAT.
  ENDLOOP.

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
  gi_index_rows TYPE lvc_t_row,
*-Information about 1 row
  g_selected_row LIKE lvc_s_row,
  l_lines TYPE i,
  l_charg    LIKE mcha-charg   ,
  l_flag(1)  TYPE c            ,
  l_succ(2)  TYPE c            ,
  l_fail(2)  TYPE c            ,
  l_succ1(2) TYPE c            ,
  l_fail1(2) TYPE c            ,
  l_text(90) TYPE c            ,
  l_answer(1) TYPE c           ,
  l_kostl     LIKE csks-kostl  ,
  l_kostl1    LIKE csks-kostl  ,
  l_saknr     LIKE ska1-saknr  .

  DATA: BEGIN OF l_msg OCCURS 100 ,
          txt(100) TYPE c         ,
        END OF l_msg              ,

        BEGIN OF l_msg1 OCCURS 100,
          txt(100) TYPE c         ,
        END OF l_msg1             .

  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = gi_index_rows.
  DESCRIBE TABLE gi_index_rows LINES l_lines.
  IF l_lines = 0.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
        textline1 = 'You must choose a valid line'.
    EXIT.
  ELSE.
  ENDIF.

  CASE e_ucomm.
*-Create Batch
    WHEN 'CR_BATCH'.

      LOOP AT gi_index_rows INTO g_selected_row.
        READ TABLE it_ausp1 INDEX g_selected_row-index.
        IF sy-subrc = 0.
          CLEAR: gv_charg,
                 l_charg .
          gv_charg = it_ausp1-objek.
          SELECT SINGLE charg
                   FROM mcha
                   INTO l_charg
                   WHERE matnr = c_matnr AND
                         werks = c_werks AND
                         charg = gv_charg.
          IF NOT l_charg IS INITIAL.
            CLEAR l_text.
            CONCATENATE text-006 gv_charg text-008 INTO l_text
                                            SEPARATED BY space.
*-If Batch already exists
            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
              EXPORTING
                textline1      = l_text
                textline2      = text-009
                titel          = 'Check!'
                cancel_display = ' '
              IMPORTING
                answer         = gv_answer.
            IF gv_answer = 'J'.
*-Change the existing Batch
              CLEAR: csks,
                     l_kostl ,
                     l_kostl1.
              l_kostl = it_ausp1-ask_dept.
              l_kostl1 = it_ausp1-usage_dept.
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

              l_saknr = it_ausp1-gl_acct.
              IF NOT l_saknr IS INITIAL.
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

              PERFORM build_object_key.
              PERFORM extract_original_batch.
              PERFORM update_original_batch.
              PERFORM bapi_change.
              PERFORM bapi_commit.
*-Get number of success and failure records
              CLEAR l_flag.
              LOOP AT it_rettab1.
                IF it_rettab1-type = 'E'.
                  l_flag = 'X'.
                  CONCATENATE gv_charg it_rettab1-message INTO
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
          ELSE.
*-Create new batch
            CLEAR: csks,
                   l_kostl,
                   l_kostl1.
            l_kostl = it_ausp1-ask_dept.
            l_kostl1 = it_ausp1-usage_dept.
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
            IF NOT l_kostl1 IS INITIAL.
              CLEAR csks.
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

            l_saknr = it_ausp1-gl_acct.
            IF NOT l_saknr IS INITIAL.
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

            PERFORM create_batch.

** On 07/08/13 by Furong - Requested by Mr. Bae
*            PERFORM BAPI_COMMIT.
*            WAIT UP TO 1 SECONDS.
*            PERFORM build_object_key.
*            PERFORM update_original_batch.
*            PERFORM bapi_change.
*            PERFORM bapi_commit.
*            CLEAR l_flag.
*            LOOP AT it_rettab.
*              IF it_rettab1-type = 'E'.
*                l_flag = 'X'.
*                CONCATENATE gv_charg it_rettab1-message INTO
*                                      l_msg1 SEPARATED BY ':'.
*              ENDIF.
*            ENDLOOP.    " LOOP AT it_rettab1
            LOOP AT it_rettab WHERE type = 'E'
                     OR type = 'A'.
              l_flag = 'X'.
              CONCATENATE gv_charg it_rettab1-message INTO
                          l_msg1 SEPARATED BY ':'.
              EXIT.
            ENDLOOP.    " LOOP AT it_rettab1
            IF sy-subrc NE 0.
              DO.
                SELECT SINGLE charg INTO l_charg
                  FROM mcha
                 WHERE matnr = c_matnr
                   AND werks = c_werks
                   AND charg = gv_charg.
                IF sy-subrc EQ 0.
                  EXIT.
                ELSE.
                  WAIT UP TO 1 SECONDS.
                ENDIF.
              ENDDO.
              PERFORM build_object_key.
              PERFORM update_original_batch.
              PERFORM bapi_change.
              PERFORM bapi_commit.
            ENDIF.
** End on 07/08/13
            IF l_flag = 'X'.
              l_fail1 = l_fail1 + 1.
              APPEND l_msg1.
              CLEAR  l_msg1.
            ELSE.
** Chnaged by Furong on 02/21/08
              it_email-objek = it_ausp1-objek.
              APPEND it_email.
              CLEAR: it_email.
** End of change
              l_succ1 = l_succ1 + 1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.  " LOOP AT gi_index_rows INTO g_selected_row.

*-Status of Batches changed
      LOOP AT l_msg.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_msg-txt.
      ENDLOOP.
      IF NOT l_succ IS INITIAL.
        CLEAR l_text.
        CONCATENATE text-010 l_succ INTO l_text SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_text.
      ENDIF.
      IF NOT l_fail IS INITIAL.
        CLEAR l_text.
        CONCATENATE text-011 l_fail INTO l_text SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_text.
      ENDIF.
*-Status of Batches created
      LOOP AT l_msg1.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_msg1-txt.
      ENDLOOP.
      IF NOT l_succ1 IS INITIAL.
        CLEAR l_text.
        CONCATENATE text-012 l_succ1 INTO l_text SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_text.
** Changed by Furong on 02/21/08
        PERFORM send_email.
** End of change
      ENDIF.
      IF NOT l_fail1 IS INITIAL.
        CLEAR l_text.
        CONCATENATE text-026 l_fail1 INTO l_text SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            textline1 = l_text.
      ENDIF.

*-Create Document
    WHEN 'CR_DOC'.
      LOOP AT gi_index_rows INTO g_selected_row.
        READ TABLE it_ausp1 INDEX g_selected_row-index.
        IF sy-subrc = 0.

* by ig.moon 9/22/2010 {
          vin_num = it_ausp1-vin_num.
* }

          CLEAR gv_doknr.
          SELECT SINGLE doknr
                       FROM draw
                       INTO gv_doknr
                       WHERE dokar = gv_dokar       AND
                             doknr = it_ausp1-objek AND
                             dokvr = '00'           AND
                             doktl = '000'.
          IF sy-subrc = 0.
            CLEAR l_text.
            CONCATENATE text-023 gv_doknr text-024 INTO l_text
                                            SEPARATED BY space.
            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
              EXPORTING
                textline1      = l_text
                textline2      = text-005
                titel          = 'Check!'
                cancel_display = ' '
              IMPORTING
                answer         = l_answer.
            IF l_answer = 'J'.
              SET PARAMETER ID 'CV1' FIELD it_ausp1-objek.
              SET PARAMETER ID 'CV2' FIELD gv_dokar.
              SET PARAMETER ID 'CV3' FIELD '00' .
              SET PARAMETER ID 'CV4' FIELD '000'.
              CALL TRANSACTION 'CV02N' AND SKIP FIRST SCREEN.
              COMMIT WORK.
              CLEAR l_charg.
              SELECT SINGLE charg
                       FROM mcha
                       INTO l_charg
                       WHERE matnr = c_matnr AND
                             werks = c_werks AND
                             charg = gv_charg.
              IF NOT l_charg IS INITIAL.
                PERFORM build_object_key.
                PERFORM extract_original_batch.
                PERFORM update_original_batch1.
                PERFORM bapi_change.
                PERFORM bapi_commit.
              ENDIF.
            ENDIF.
          ELSE.

            SET PARAMETER ID 'CV1' FIELD it_ausp1-objek.
            SET PARAMETER ID 'CV2' FIELD gv_dokar.
            SET PARAMETER ID 'CV3' FIELD '00' .
            SET PARAMETER ID 'CV4' FIELD '000'.
            CALL TRANSACTION 'CV01N' AND SKIP FIRST SCREEN.
            COMMIT WORK.
            CLEAR l_charg.
            SELECT SINGLE charg
                     FROM mcha
                     INTO l_charg
                     WHERE matnr = c_matnr AND
                           werks = c_werks AND
                           charg = gv_charg.
            IF NOT l_charg IS INITIAL.
              PERFORM build_object_key.
              PERFORM extract_original_batch.
              PERFORM update_original_batch1.
              PERFORM bapi_change.
              PERFORM bapi_commit.
              PERFORM  create_doc_with_bapi USING it_ausp1-objek.
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

        READ TABLE it_ausp1 INDEX g_selected_row-index.
        IF sy-subrc = 0.
          CLEAR gv_doknr.
          SELECT SINGLE doknr
                       FROM draw
                       INTO gv_doknr
                       WHERE dokar = gv_dokar       AND
                             doknr = it_ausp1-objek AND
                             dokvr = '00'           AND
                             doktl = '000'.
          IF sy-subrc = 0.
            PERFORM  create_doc_with_bapi USING it_ausp1-objek.
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

* Create an Instance of ALV Control
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

     ' '  'OBJEK'          'Body Number'       10 'CHAR' ' ' 'X',
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

** Changed by Furong on 02/20/08
     ' '  'ENT_TYP'        'Entry Type'        10 'CHAR' 'X' ' ',
     ' '  'FTZ_ENTRY'      'FTZ Entry   '      13 'CHAR' 'X' ' ',
     ' '  'DUTY_PAID'      'Duty Paid'         10 'CHAR' 'X' ' ',
     ' '  'DUTY_AMT'       'Duty Amount'       11 'CHAR' 'X' ' ',
** End of change

     ' '  'USAGE_TEXT'     'Usage Text'        30 'CHAR' 'X' ' ',
     ' '  'ASK_DIV'        'Asking Division'   20 'CHAR' 'X' ' ',
     ' '  'ASK_DEPT'       'Asking Dept.'      10 'CHAR' 'X' ' ',
     ' '  'USAGE_DEPT'     'Using Dept.'       11 'CHAR' 'X' ' ',
     ' '  'REF_DOC'        'Reference Doc #'   15 'CHAR' ' ' ' ',
     ' '  'FINAL_DEST'     'Final Destination' 15 'CHAR' 'X' ' ',
** Changed by Furong on 02/20/08
    ' '  'TEMP_REM'        'Temp.Rem'          17 'CHAR' 'X' ' ',
    ' '  'RET_DATE'        'Return Date'       10 'CHAR' 'X' ' ',
** End of cahnge on 02/20/08
     ' '  'SHIP_DATE'      'Ship Out Date'     13 'CHAR' 'X' ' ',
     ' '  'SHIP_DOCS'      'Ship Out Doc'      12 'CHAR' 'X' ' ',
     ' '  'GL_ACCT'        'G/L Account'       11 'NUMC' 'X' ' ',
     ' '  'ACC_DOC'        'Accounting Doc #'  16 'CHAR' 'X' ' ',
     ' '  'INVOICE'        'Invoice Doc #'     13 'NUMC' 'X' ' '.

  LOOP AT gt_fieldcat.
    IF gt_fieldcat-fieldname = 'ASK_DEPT'.
      gs_fieldcat-edit = ' '.
      gt_fieldcat-f4availabl = 'X'.
      gt_fieldcat-ref_field = 'KOSTL'.
      gt_fieldcat-ref_table = 'CSKS'.
      MODIFY gt_fieldcat INDEX sy-tabix TRANSPORTING edit f4availabl
                                         ref_field ref_table.
    ELSEIF gt_fieldcat-fieldname = 'GL_ACCT'.
      gs_fieldcat-edit = ' '.
      gt_fieldcat-f4availabl = 'X'.
      gt_fieldcat-ref_field = 'SAKNR'.
      gt_fieldcat-ref_table = 'SKA1'.
      MODIFY gt_fieldcat INDEX sy-tabix TRANSPORTING edit f4availabl
                                         ref_field ref_table.
*    ELSEIF gt_fieldcat-fieldname = 'USAGE_DEPT'.
*      gs_fieldcat-edit = ' '.
*      gt_fieldcat-f4availabl = 'X'.
*      gt_fieldcat-ref_field = 'KOSTL'.
*      gt_fieldcat-ref_table = 'CSKS'.
*      MODIFY gt_fieldcat INDEX sy-tabix TRANSPORTING f4availabl
*                                         ref_field ref_table.
    ENDIF.
* by ig.moon 6/1/2010 {
    IF gt_fieldcat-fieldname = 'VTYPE'.
      gs_fieldcat-edit = ' '.
      gt_fieldcat-f4availabl = 'X'.
      MODIFY gt_fieldcat INDEX sy-tabix TRANSPORTING edit f4availabl
                                         ref_field ref_table.
    ENDIF.
* }
*  IF GT_FIELDCAT-FIELDNAME = 'DOC_PVT' or
*       GT_FIELDCAT-FIELDNAME = 'DOC_TRF' or
*       GT_FIELDCAT-FIELDNAME = 'DOC_MERAF' or
*       GT_FIELDCAT-FIELDNAME = 'DOC_BOL'.
*
*      GS_FIELDCAT-EDIT = ' '.
*      GT_FIELDCAT-F4AVAILABL = 'X'.
*      MODIFY GT_FIELDCAT INDEX SY-TABIX TRANSPORTING EDIT F4AVAILABL
*                                         REF_FIELD REF_TABLE.
*    ENDIF.

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

  SORT it_ausp1 BY objek.
*-- Display data on ALV GRID Control
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      i_structure_name     = 'IT_AUSP1'
      is_layout            = gs_layout
      i_save               = gv_save
      is_variant           = gv_variant
      i_default            = 'X'
      it_toolbar_excluding = mt_excluding_toolbar
    CHANGING
      it_fieldcatalog      = gt_fieldcat[]
      it_outtab            = it_ausp1[].
*            IT_SORT               = <internal table of type LVC_T_SORT>
*            IT_FILTER             = <internal table of type LVC_T_FILT>

*-Create Object to receive events and link them to handler methods.
*-When ALV Control raises the event for the specified instance,
*-corresponding method is automatically called.
  CREATE OBJECT event_receiver.

  SET HANDLER event_receiver->handle_toolbar       FOR alv_grid.
  SET HANDLER event_receiver->handle_user_command  FOR alv_grid.
  SET HANDLER event_receiver->on_f4                FOR alv_grid.

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
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
  gv_charg = it_ausp1-objek.
*-Create the batch using screen values selected by the user
  CALL FUNCTION 'BAPI_BATCH_CREATE'
    EXPORTING
      material             = c_matnr
      batch                = gv_charg
      plant                = c_werks
      batchstoragelocation = space
    TABLES
      return               = it_rettab.
** Furong on 07/08/13
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
** End
ENDFORM.                    " create_batch
*&---------------------------------------------------------------------*
*&      Form  bapi_commit
*&---------------------------------------------------------------------*
*       Subroutine to Commit BAPI changes
*----------------------------------------------------------------------*
FORM bapi_commit.
*-Commit the changes
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*  COMMIT WORK.
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
   EXPORTING
     WAIT          = 'X'.

ENDFORM.                    " bapi_commit
*&---------------------------------------------------------------------*
*&      Form  build_object_key
*&---------------------------------------------------------------------*
*       Subroutine to build Object Key
*----------------------------------------------------------------------*
FORM build_object_key.
*-Build the object key
  CLEAR:   it_object, it_rettab.
  REFRESH: it_object, it_rettab.

  it_object-key_field = 'MATNR'.
  it_object-value_int = c_matnr.
  APPEND it_object.

  it_object-key_field = 'WERKS'.
  it_object-value_int = c_werks.
  APPEND it_object.

  it_object-key_field = 'CHARG'.
  it_object-value_int = gv_charg.
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
                                                 it_ausp1-vin_num.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_FSC'.  " Full Spec code
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam it_ausp1-fsc.

  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_EXT_COLOR'.  " Exterior Color
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                       it_ausp1-ext_clr.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_INT_COLOR'.  " Interior Color
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                       it_ausp1-int_clr.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_USAGE_CAR'.  " Usage Car
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                     it_ausp1-usage.
  ENDIF.

* by ig.moon 6/1/2010 {
  READ TABLE it_cabn WITH KEY atnam = 'P_USED_FOR'.  " Use Type
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                     it_ausp1-vtype.
  ENDIF.
* }

  READ TABLE it_cabn WITH KEY atnam = 'P_USAGE_TEXT'.  " Usage text
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                    it_ausp1-usage_text.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_ASK_DIV'.  " Asking Division
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                       it_ausp1-ask_div.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_ASK_DEPT'.  " Asking Dept
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                      it_ausp1-ask_dept.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_USAGE_DEPT'.  " Usage Dept
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                    it_ausp1-usage_dept.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_REF_DOC'.  " Reference Document
  IF sy-subrc = 0.
    SELECT SINGLE dktxt FROM drat
                  INTO l_dktxt
                       WHERE dokar = gv_dokar AND
                             doknr = gv_charg AND
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
                                                    it_ausp1-final_dest.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_SHIPOUT_DOC'.  " Ship Out Doc
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                     it_ausp1-ship_docs.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_GL_ACCOUNT'.  " GL Account
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                      it_ausp1-gl_acct.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_ACC_DOC'.     " Accounting Doc
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                     it_ausp1-acc_doc.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_INV_NO'.  " Invoice Number
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                                      it_ausp1-invoice.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_SHIPOUT_DATE'.  "ShipOut Dt
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                             it_ausp1-ship_date.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_RP18_SHOP_DATE'." RP18 shopdt
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-rp18_shop_date.
  ENDIF.

**Changed by Furong on 02/20/08  "UD1K942932
  READ TABLE it_cabn WITH KEY atnam = 'P_ENT_TYP'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-ent_typ.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_FTZ_ENTRY'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-ftz_entry.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DUTY_PAID'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-duty_paid.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DUTY_AMT'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-duty_amt.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_TEMP_REM'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-temp_rem.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_RET_DATE'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-ret_date.
  ENDIF.
**End of change on 02/20/08  "UD1K942932

**Changed by Furong on 06/21/10
  READ TABLE it_cabn WITH KEY atnam = 'P_DOC_PVT'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-doc_pvt.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DOC_TRF'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-doc_trf.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DOC_MERAF'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-doc_meraf.
  ENDIF.
  READ TABLE it_cabn WITH KEY atnam = 'P_DOC_BOL'.
  IF sy-subrc = 0.
    PERFORM update_table USING it_cabn-atfor it_cabn-atnam
                                            it_ausp1-doc_bol.
  ENDIF.
**End of change
ENDFORM.                    " update_original_batch
*&---------------------------------------------------------------------*
*&      Form  get_characteristics
*&---------------------------------------------------------------------*
*       Subroutine to get characterstic names
*----------------------------------------------------------------------*
FORM get_characteristics.
*-Extract the characteristic names for KLART/CLASS
  CLEAR: klah, it_cabn, it_cabn1.
  REFRESH: it_cabn, it_cabn1.

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
    ENDIF.
  ENDIF.

  r_atnam-sign = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low = 'P_VIN'.
  APPEND r_atnam.
  r_atnam-low = 'P_FSC'.
  APPEND r_atnam.
  r_atnam-low = 'P_EXT_COLOR'.
  APPEND r_atnam.
  r_atnam-low = 'P_INT_COLOR'.
  APPEND r_atnam.
  r_atnam-low = 'P_RP18_SHOP_DATE'.
  APPEND r_atnam.
  r_atnam-low = 'P_USAGE_CAR'.
  APPEND r_atnam.

* by ig.moon 6/1/2010 {
  r_atnam-low = 'P_USED_FOR'.
  APPEND r_atnam.
  r_atnam-low = 'P_TEST_DOC'.
  APPEND r_atnam.

* }

**Changed by Furong on 02/20/08  "UD1K942932
  r_atnam-low = 'P_ENT_TYP'.
  APPEND r_atnam.
  r_atnam-low = 'P_FTZ_ENTRY'.
  APPEND r_atnam.
  r_atnam-low = 'P_DUTY_PAID'.
  APPEND r_atnam.
  r_atnam-low = 'P_DUTY_AMT'.
  APPEND r_atnam.
  r_atnam-low = 'P_TEMP_REM'.
  APPEND r_atnam.
  r_atnam-low = 'P_RET_DATE'.
  APPEND r_atnam.
**End of change on 02/20/08  "UD1K942932
  r_atnam-low = 'P_USAGE_TEXT'.
  APPEND r_atnam.
  r_atnam-low = 'P_ASK_DIV'.
  APPEND r_atnam.
  r_atnam-low = 'P_ASK_DEPT'.
  APPEND r_atnam.
  r_atnam-low = 'P_USAGE_DEPT'.
  APPEND r_atnam.
  r_atnam-low = 'P_REF_DOC'.
  APPEND r_atnam.
  r_atnam-low = 'P_FINAL_DEST'.
  APPEND r_atnam.
  r_atnam-low = 'P_SHIPOUT_DATE'.
  APPEND r_atnam.
  r_atnam-low = 'P_SHIPOUT_DOC'.
  APPEND r_atnam.
  r_atnam-low = 'P_GL_ACCOUNT'.
  APPEND r_atnam.
  r_atnam-low = 'P_ACC_DOC'.
  APPEND r_atnam.
  r_atnam-low = 'P_INV_NO'.
  APPEND r_atnam.
  r_atnam-low = 'P_MODEL_YEAR'.
  APPEND r_atnam.
  r_atnam-low = 'P_NATN_CODE'.
  APPEND r_atnam.
  r_atnam-low = 'P_DIST_CODE'.
  APPEND r_atnam.
  r_atnam-low = 'P_MI'.
  APPEND r_atnam.
  r_atnam-low = 'P_OCN'.
**Changed by Furong on 06/21/10
  r_atnam-low = 'P_DOC_PVT'.
  APPEND r_atnam.
  r_atnam-low = 'P_DOC_TRF'.
  APPEND r_atnam.
  r_atnam-low = 'P_DOC_MERAF'.
  APPEND r_atnam.
  r_atnam-low = 'P_DOC_BOL'.
  APPEND r_atnam.
**End of change on 06/21/10

  APPEND r_atnam.
  SELECT atinn atnam atfor
               FROM cabn
               INTO TABLE it_cabn1
               WHERE atnam IN r_atnam.
  IF sy-subrc = 0.
    LOOP AT it_cabn1.
      IF it_cabn1-atnam = 'P_VIN'.               " VIN Number
        gv_vin = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_ASK_DEPT'.      " Asking Dept
        gv_askdept = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_USAGE_DEPT'.    " Dest/Using Dept
        gv_usagedep = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_FINAL_DEST'.    " Final Dest
        gv_dest = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_GL_ACCOUNT'.    " GL Account
        gv_glno = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_EXT_COLOR'.     " Ext Color
        gv_exclr = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_INT_COLOR'.     " Int Color
        gv_inclr = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_RP18_SHOP_DATE'." Shop Dt
        gv_shopdt = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_USAGE_CAR'.     " Usage Car
        gv_usgcar = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_SHIPOUT_DATE'.  " Ship Dt
        gv_shipdt = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_USAGE_TEXT'.    " Usage Text
        gv_usgtxt = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_REF_DOC'.       " Ref doc
        gv_refdoc = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_ASK_DIV'.       " Asking Division
        gv_ask_div = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_SHIPOUT_DOC'.   " Ship Out Docs
        gv_ship_doc = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_ACC_DOC'.       " Accounting Doc
        gv_acc_doc = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_INV_NO'.        " Invoice Number
        gv_invno = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_MODEL_YEAR'.    " Model Year
        gv_modelyr = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_NATN_CODE'.     " Nation Code
        gv_nat_code = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_MI'.            " Model index
        gv_mi = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_DIST_CODE'.     " Distributor for SD
        gv_dist_cd = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_OCN'.           " O.C.N.
        gv_ocn = it_cabn1-atinn.
**Changed by Furong on 02/20/08  "UD1K942932
      ELSEIF it_cabn1-atnam = 'P_ENT_TYP'.
        gv_ent_typ = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_FTZ_ENTRY'.
        gv_ftz_entry = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_DUTY_PAID'.
        gv_duty_paid = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_DUTY_AMT'.
        gv_duty_amt = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_TEMP_REM'.
        gv_temp_rem = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_RET_DATE'.
        gv_ret_date = it_cabn1-atinn.
**End of change on 02/20/08  "UD1K942932

**Changed by Furong on 06/21/10
      ELSEIF it_cabn1-atnam = 'P_DOC_PVT'.
        gv_doc_pvt = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_DOC_TRF'.
        gv_doc_trf = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_DOC_MERAF'.
        gv_doc_meraf = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_DOC_BOL'.
        gv_doc_bol = it_cabn1-atinn.
**End of change

* by ig.moon 6/3/2010 {
      ELSEIF it_cabn1-atnam = 'P_USED_FOR'.
        gv_used_for = it_cabn1-atinn.
      ELSEIF it_cabn1-atnam = 'P_TEST_DOC'.
        gv_test_doc = it_cabn1-atinn.
* }
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_characteristics
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ATFOR  text
*      -->ATNAM  text
*      -->VALUE  text
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
      return             = it_rettab1.


ENDFORM.                    " bapi_change
*&---------------------------------------------------------------------*
*&      Form  extract_original_batch
*&---------------------------------------------------------------------*
*       Subroutine to extract original characteristic values
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
                             doknr = gv_charg AND
                             dokvr = '00'     AND
                             doktl = '000'    AND
                             langu = sy-langu.
    IF NOT l_dktxt IS INITIAL.
      l_dktxt1 = l_dktxt.

      PERFORM update_table USING it_cabn-atfor it_cabn-atnam l_dktxt1.
    ENDIF.
  ENDIF.
ENDFORM.                    " update_original_batch1
*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.
  DATA:   it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
              it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
              it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
              it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
              gd_cnt TYPE i,
              gd_sent_all(1) TYPE c,
              gd_doc_data LIKE sodocchgi1,
              gd_error TYPE sy-subrc,
*            l_eknam like t024-eknam,
*            l_ektel like t024-ektel,
         psubject(40) TYPE c VALUE  'NEW TEST VEHICLE(S) ARE CREATED',
              p_email(40)   TYPE c VALUE 'ZTVT_CREATED'.

  DATA:   it_message TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                  WITH HEADER LINE.

* Populate Message Body .

  APPEND psubject TO it_message.

  APPEND '------------------------------------------' TO it_message.
*  concatenate 'Body No'  v_po-ebeln into  it_message.
  APPEND it_message.

  LOOP AT it_email.
    MOVE  'Body No. :' TO it_message(10).
    MOVE  it_email-objek TO it_message+11(20).
    APPEND it_message. CLEAR it_message.
  ENDLOOP.
  APPEND '------------------------------------------' TO it_message.
* Fill the document data.
  gd_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = 'SAPRPT'.
  gd_doc_data-obj_descr = psubject.
  gd_doc_data-sensitivty = 'F'.

* Describe the body of the message
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE it_message LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

* Add the recipients email address
  CLEAR it_receivers.
  REFRESH it_receivers.
*  it_receivers-receiver = p_email.
*  it_receivers-rec_type = 'U'.
*  it_receivers-com_type = 'INT'.
*  it_receivers-notif_del = 'X'.
*  it_receivers-notif_ndel = 'X'.

  it_receivers-receiver = p_email.
** Changed by Furong
*  it_receivers-rec_type = 'U'.  " internet email
*  it_receivers-com_type = 'INT'.
  it_receivers-rec_type = 'C'.
  it_receivers-com_type = 'INT'.

  APPEND it_receivers.

* Call the FM to send the message to SAPMAIL
* asynchronously
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      STARTING NEW TASK  'V1'
       EXPORTING
            document_data              = gd_doc_data
*            PUT_IN_OUTBOX              = 'X'
            commit_work                = 'X'
*       IMPORTING
*            sent_to_all                = gd_sent_all
       TABLES
            packing_list               = it_packing_list
            contents_txt               = it_message
            receivers                  = it_receivers
       EXCEPTIONS
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            OTHERS                     = 8.
  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.

ENDFORM.                    " send_email
*&---------------------------------------------------------------------*
*&      Form  create_f4_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2766   text
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
*&---------------------------------------------------------------------*
*&      Form  ON_F4
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
*      -->P_0761   text
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
      it_ausp1-vtype = ls_f4-fieldval.
*    WHEN 'DOC_PVT'.
*      IT_AUSP1-DOC_PVT = LS_F4-FIELDVAL.
*    WHEN 'DOC_TRF'.
*      IT_AUSP1-DOC_TRF = LS_F4-FIELDVAL.
*    WHEN 'DOC_MERAF'.
*      IT_AUSP1-DOC_MERAF = LS_F4-FIELDVAL.
*    WHEN 'DOC_BOL'.
*      IT_AUSP1-DOC_BOL = LS_F4-FIELDVAL.

  ENDCASE.

ENDFORM.                                                    " F4_APLY
*&---------------------------------------------------------------------*
*&      Form  MY_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_F4  text
*      -->P_SENDER  text
*      -->P_ET_BAD_CELLS  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_E_DISPLAY  text
*      -->P_E_FIELDNAME  text
*      -->P_0776   text
*----------------------------------------------------------------------*
FORM my_f4 TABLES et_f4         STRUCTURE ddshretval
           USING  sender        TYPE REF TO cl_gui_alv_grid
                  et_bad_cells  TYPE lvc_t_modi
                  es_row_no     TYPE lvc_s_roid
                  er_event_data TYPE REF TO cl_alv_event_data
                  e_display     TYPE c
                  e_fieldname   TYPE lvc_fname
                  p_tab.

  DATA : ls_out        LIKE LINE OF it_ausp1,
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

  READ TABLE it_ausp1 INDEX es_row_no-row_id INTO ls_out.

  IF sy-subrc = 0.
    CREATE DATA l_wa LIKE LINE OF it_ausp1.
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

*  IF E_FIELDNAME = 'DOC_PVT' OR
*    E_FIELDNAME = 'DOC_TRF' OR
*    E_FIELDNAME = 'DOC_MERAF' OR
*    E_FIELDNAME = 'DOC_BOL'.
*    PERFORM F4_DOC USING E_FIELDNAME.
*  ENDIF.
*
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
*&      Form  GET_VTYPE_FOR_POSSIBLE_ENTRY
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
