*&---------------------------------------------------------------------*
*& Program: ZRFIF07N                                                    *
*& Type   : Report                                                     *
*& Author :
*& Title  : Expense Budget Report (FM)                                 *
*&---------------------------------------------------------------------*
*   Requested by:        Andy Choi                                     *
*&---------------------------------------------------------------------*

* RFFMEP1A

REPORT zrfif07 LINE-SIZE 132 LINE-COUNT 65
                             NO STANDARD PAGE HEADING MESSAGE-ID db .

*&---------------------------------------------------------------------*
*&  Include           ZRFIF07NTOP
*&---------------------------------------------------------------------*
*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES : fmioi,v_fmifi,
         ekbe,
         eban, ekpo,
         ebkn,       "PR account assignment
         fmhictr, fmfctr, tbpfm, fmfpo, fmfincode.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.


*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*

*Commitment reading from DB
DATA : BEGIN OF gt_cmmt OCCURS 0,
        gjahr LIKE fmioi-gjahr,
        poper TYPE co_perio,

        fistl LIKE fmioi-fistl,
        fonds LIKE fmioi-fonds,
        fipex LIKE fmioi-fipex,

*------ for PM
        objnrz TYPE j_objnr,
*        awtyp  TYPE awtyp,

        btart LIKE fmioi-btart,
        wrttp LIKE fmioi-wrttp,

        fkbtr LIKE fmioi-fkbtr,  "AMOUNT

        hkont LIKE fmioi-hkont,
        budat LIKE fmioi-budat,

        lifnr LIKE fmioi-lifnr,

        vrefbt LIKE fmioi-vrefbt, "Predecessor document
        vrefbn LIKE fmioi-vrefbn,
*       VRFORG like fmioi-VRFORG,
        vrfpos LIKE fmioi-vrfpos,

        kngjahr TYPE fm_kngjahr,
        refbn  LIKE fmioi-refbn,
*       RFORG  like fmioi-RFORG,
        rfpos  LIKE fmioi-rfpos,
        refbt  LIKE fmioi-refbt,   "Reference document category

**------ for PM
*        aufnr  TYPE aufnr,
*        txz01  LIKE ekpo-txz01,
**        txz01 TYPE txz01,
        loekz  LIKE fmioi-loekz,
       END OF gt_cmmt.

*actual data reading
DATA : gt_fi LIKE gt_cmmt OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_fmfpo OCCURS 0,
         fipos    LIKE fmfpo-fipos,
         posit    LIKE fmep-posit,
         bezeich  LIKE fmfpot-bezeich,
         profil   TYPE bp_bprofil,
       END OF it_fmfpo.

DATA : BEGIN OF it_pr OCCURS 0,
         banfn   LIKE eban-banfn,
         bnfpo   LIKE eban-bnfpo,
       END OF it_pr.

DATA : BEGIN OF it_po OCCURS 0,
         ebeln   LIKE ekpo-ebeln,
         ebelp   LIKE ekpo-ebelp,
       END OF   it_po.

DATA e_level(3) TYPE c.
DATA l_cousertype(3) TYPE c.
DATA $ix TYPE i.

TYPE-POOLS: slis.

*structure: ZSFMLINE
TYPES: BEGIN OF ty_output,
*         sort(1),
         fonds   LIKE fmioi-fonds ,
         fistl   LIKE fmioi-fistl ,
         kostl   LIKE ebkn-kostl,
         fipex   LIKE fmioi-fipex,
         hkont   LIKE fmioi-hkont,
         aufnr   TYPE aufnr,
         lifnr   LIKE fmioi-lifnr,
         cffld(01),
         doctx(05),
         refbt LIKE fmioi-refbt,
         refbn LIKE fmioi-refbn,
         rfpos LIKE fmioi-rfpos,
         fkbtr LIKE fmioi-fkbtr,  "commitment/actual
*         fm_orig LIKE fmioi-fkbtr,
*         prghr  TYPE gjahr,
         prdoc(11),
         prpos LIKE fmioi-vrfpos,
         pramt  TYPE dmbtr,       "PR Amount
*         poghr  TYPE gjahr,
         podoc(11),
         popos LIKE fmioi-vrfpos,
         poamt  TYPE dmbtr,       "PO Amount
         grghr  TYPE gjahr,
         grdoc(11),
         grcnt TYPE i,
         grpos LIKE fmioi-vrfpos,
         gramt  TYPE dmbtr,       "GR Amount
         ivghr  TYPE gjahr,
         ivdoc  TYPE belnr_d,
         ivdocs(11),
         ivcnt  TYPE i,
         ivamt  TYPE dmbtr,       "IV Amount
         dpghr  TYPE gjahr,
         dpdoc(11),
         dpcnt  TYPE i,
         dpamt  TYPE dmbtr,       "DP Amount
         tfghr  TYPE gjahr,
         tfdoc(11),
         tfcnt  TYPE i,
         tfamt  TYPE dmbtr,       "transfer posting
         augdt  LIKE bsak-augdt,  "clearing date

         pyghr  TYPE gjahr,
         pydoc(11),  "Payment doc
         pyamt  TYPE dmbtr,       "Payment Amount

         blamt  TYPE dmbtr,       "commitment balance
       END OF ty_output.

*DATA : gt_hash TYPE hashed TABLE OF zsfmline WITH HEADER LINE.
DATA : gt_out TYPE ty_output OCCURS 0. " WITH HEADER LINE.
DATA : gt_inv TYPE ty_output OCCURS 0, " WITH HEADER LINE,
       gt_dnp TYPE ty_output OCCURS 0, " WITH HEADER LINE,
       gt_trf TYPE ty_output OCCURS 0. "" WITH HEADER LINE.
*       INITIAL SIZE 0.

TYPES: BEGIN OF gty_prpo,
        refbn TYPE fm_vrefbn,
        rfpos TYPE fm_vrfpos,
*        gjahr TYPE gjahr,
        fkbtr LIKE fmioi-fkbtr,
        vrefbn TYPE fm_vrefbn,
        vrfpos TYPE fm_vrfpos,
*        vrfghr type gjahr,
        hkont TYPE hkont,
      END OF gty_prpo.
DATA: gt_pr   TYPE TABLE OF gty_prpo. " WITH NON-UNIQUE KEY refbn rfpos gjahr.
DATA: gt_po   TYPE TABLE OF gty_prpo. " WITH NON-UNIQUE KEY refbn rfpos gjahr.

*GR, IV, DP, TRF documents
DATA : BEGIN OF wa_detail,
         refbn LIKE fmioi-refbn,
         rfpos LIKE fmioi-rfpos,
         doctx(02),
         gjahr TYPE gjahr,
         docnr LIKE fmioi-refbn,
         docid LIKE fmioi-rfpos,
         amtxx  TYPE dmbtr,       "PO Amount
       END OF wa_detail.
*DATA : it_detail LIKE STANDARD TABLE OF wa_detail  WITH HEADER LINE
*       INITIAL SIZE 0.
DATA : it_detail2 LIKE STANDARD TABLE OF wa_detail  WITH HEADER LINE
       INITIAL SIZE 0.

TYPES: BEGIN OF ty_ebkn,
        banfn LIKE ebkn-banfn,
        bnfpo LIKE ebkn-bnfpo,
        aufnr LIKE ebkn-aufnr,
        kostl LIKE ebkn-kostl,
        sakto LIKE ebkn-sakto,
        fistl LIKE ebkn-fistl,
        geber LIKE ebkn-geber,
        fipos LIKE ebkn-fipos,
        lifnr LIKE ekko-lifnr,
        bedat LIKE ekko-bedat,
      END OF ty_ebkn.
DATA: it_ebkn TYPE TABLE OF ty_ebkn WITH HEADER LINE.

TYPES: BEGIN OF ty_ekkn,
        ebeln LIKE ekkn-ebeln,
        ebelp LIKE ekkn-ebelp,
        aufnr LIKE ekkn-aufnr,
        kostl LIKE ekkn-kostl,
        sakto LIKE ekkn-sakto,
        fistl LIKE ekkn-fistl,
        geber LIKE ekkn-geber,
        fipos LIKE ekkn-fipos,
      END OF ty_ekkn.
DATA: it_ekkn TYPE TABLE OF ty_ekkn WITH HEADER LINE.

TYPES: BEGIN OF ty_gr,
         ebeln LIKE ekbe-ebeln,
         ebelp LIKE ekbe-ebelp,
         gjahr LIKE ekbe-gjahr,
         belnr LIKE ekbe-belnr,
         buzei LIKE ekbe-buzei,
         bwart LIKE ekbe-bwart,
         budat LIKE ekbe-budat,
         shkzg LIKE ekbe-shkzg,
         menge LIKE ekbe-menge,
         wrbtr LIKE ekbe-wrbtr,
         netpr LIKE ekpo-netpr,
         peinh LIKE ekpo-peinh,
       END OF ty_gr.
DATA: gt_gr TYPE TABLE OF ty_gr.

DATA: BEGIN OF it_aufk OCCURS 0,
        aufnr LIKE aufk-aufnr,
        kostl LIKE aufk-kostl,
        kostv LIKE aufk-kostv,
      END OF it_aufk.

DATA: BEGIN OF it_ekpo OCCURS 0,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        erekz LIKE ekpo-erekz,  "Final invoice indicator
        netpr LIKE ekpo-netpr,
        peinh LIKE ekpo-peinh,
        pstyp LIKE ekpo-pstyp,  "Item category in purchasing document
        lifnr LIKE ekko-lifnr,
        bedat LIKE ekko-bedat,
      END OF it_ekpo.

*--------------------------------------------------------------------*
* ALV FIELD START
*--------------------------------------------------------------------*

*-- ALV Global Field
* ALV Variant
DATA: alv_variant   LIKE disvariant,
      alv_repid     LIKE sy-repid.

* ALV Attribute
DATA: gt_exclude   TYPE ui_functions,   " Tool Bar ButtonAuAa
      gs_fieldcat  TYPE lvc_s_fcat,     " CE#a 糘己 Set
      gt_fieldcat  TYPE lvc_t_fcat,     " CE#a 糘己 AuAa
      gs_f4        TYPE lvc_s_f4,                           " F4 CE#a
      gt_f4        TYPE lvc_t_f4,
      gs_layocat   TYPE lvc_s_layo,     " Grid 糘己 AuAa(Display)
      gs_sort      TYPE lvc_s_sort,
      gt_sort      TYPE lvc_t_sort,
      g_repid      TYPE sy-repid,
      g_title      TYPE lvc_title ,
      ls_col       TYPE sy-tabix,
      l_scroll     TYPE lvc_s_stbl.

DATA: alv_variant2  LIKE disvariant,
      alv_repid2    LIKE sy-repid.
DATA: gt_exclude2   TYPE ui_functions,   " Tool Bar ButtonAuAa
      gs_fieldcat2  TYPE lvc_s_fcat,     " CE#a 糘己 Set
      gt_fieldcat2  TYPE lvc_t_fcat,     " CE#a 糘己 AuAa
      gs_f42        TYPE lvc_s_f4,                           " F4 CE#a
      gt_f42        TYPE lvc_t_f4,
      gs_layocat2   TYPE lvc_s_layo,     " Grid 糘己 AuAa(Display)
      gs_sort2      TYPE lvc_s_sort,
      gt_sort2      TYPE lvc_t_sort,
      g_repid2      TYPE sy-repid,
      g_title2      TYPE lvc_title ,
      ls_col2       TYPE sy-tabix,
      l_scroll2     TYPE lvc_s_stbl.

DATA: f_color TYPE lvc_t_scol WITH HEADER LINE,
      t_col   TYPE lvc_s_colo.

*-- Drop-Down List Box List.
DATA: gt_dropdown TYPE lvc_t_dral,
      gs_dropdown TYPE lvc_s_dral.

DATA: e_valid     TYPE c,
      c_refresh   TYPE c.

* search help
TYPE-POOLS : f4typ.
DATA: gt_help_value  LIKE help_value       OCCURS 0
      WITH HEADER LINE,
      gt_heading_tab TYPE f4typ_head_struc OCCURS 0
      WITH HEADER LINE,
      g_choices      LIKE sy-tabix.
DATA  gt_good_cells.

**-- CLASS Reference

CLASS :lcl_alv_grid      DEFINITION DEFERRED ,
       lcl_event_handler DEFINITION DEFERRED .

DATA : g_customer         TYPE REF TO cl_gui_custom_container,
       g_event_handler    TYPE REF TO lcl_event_handler,
       gc_docking_container TYPE REF TO cl_gui_docking_container,
       g_grid             TYPE REF TO lcl_alv_grid .
DATA : g_customer2        TYPE REF TO cl_gui_custom_container,
       g_event_handler2   TYPE REF TO lcl_event_handler,
       g_grid2            TYPE REF TO lcl_alv_grid .
DATA container TYPE REF TO cl_gui_custom_container.
DATA g_splitter TYPE REF TO cl_gui_splitter_container.
DATA g_container_1 TYPE REF TO cl_gui_container.
DATA g_container_2  TYPE REF TO cl_gui_container.

DATA : x_check(1).
DATA : ok_code LIKE sy-ucomm.
DATA : g_total TYPE dmbtr.
*--------------------------------------------------------------------*
* CONSTANTS
*--------------------------------------------------------------------*
CONSTANTS : c_container   TYPE char20 VALUE 'MY_CONTAINER' .

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE  text-001.

PARAMETERS: p_fikrs LIKE fmioi-fikrs OBLIGATORY MEMORY ID fik,
            p_gjahr LIKE fmioi-gjahr OBLIGATORY MEMORY ID gjr.

PARAMETERS : p_dfctr TYPE xfeld RADIOBUTTON GROUP meth DEFAULT 'X',
             p_dfund TYPE xfeld RADIOBUTTON GROUP meth,
             p_dopen TYPE xfeld RADIOBUTTON GROUP meth.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE  text-002.
SELECT-OPTIONS: s_fistl  FOR fmioi-fistl MEMORY ID fis,
                s_fonds  FOR fmioi-fonds. " MEMORY ID fic.

SELECT-OPTIONS: s_gjahr FOR fmioi-gjahr NO INTERVALS,
                s_perio  FOR fmioi-perio,
                s_fipex  FOR fmioi-fipex MEMORY ID fps,
                s_prof   FOR tbpfm-profil.  "default 'B' option NE

SELECTION-SCREEN END OF BLOCK b2.
SELECT-OPTIONS: s_refbt  FOR fmioi-refbt.
SELECT-OPTIONS: s_refbn  FOR fmioi-refbn.


*SELECT-OPTIONS :
*                p_prof  FOR tbpfm-profil,
*                p_knz   FOR fmfpo-knzaepo DEFAULT '3'.  " item category

*SELECT-OPTIONS : s_lifnr  FOR fmioi-lifnr,
*                 s_hkont  FOR fmioi-hkont,
*                 s_refbn  FOR fmioi-refbn,
*                 s_rfpos  FOR fmioi-rfpos,
*
*                 s_vrefbt FOR fmioi-vrefbt,
*                 s_vrefbn FOR fmioi-vrefbn,
*                 s_vrfpos FOR fmioi-vrfpos,
*                 s_budat  FOR fmioi-budat.
**                 s_aufnr FOR ebkn-aufnr.

*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE  text-002.
*
*PARAMETERS p_op0   RADIOBUTTON GROUP radi. "Summary
*PARAMETERS p_op1   RADIOBUTTON GROUP radi. "Detail
*
*SELECTION-SCREEN END OF BLOCK b2.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
Parameter p_head as checkbox.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

RANGES: s_ebeln FOR ekpo-ebeln,
        s_banfn FOR ebkn-banfn,
        s_bnfpo FOR ebkn-bnfpo,
        s_ebelp FOR ekpo-ebelp.

*
*&---------------------------------------------------------------------*
*&  Include           ZRFIR07NC01
*&---------------------------------------------------------------------*
************************************************************************
* LOCAL CLASSES: Definition
************************************************************************
CLASS lcl_alv_grid DEFINITION INHERITING FROM cl_gui_alv_grid.

  PUBLIC SECTION.

    METHODS:  set_toolbar_buttons1 . " REDEFINITION.
    METHODS:  set_toolbar_buttons2 . " REDEFINITION.


ENDCLASS.                    "LCL_ALV_GRID DEFINITION
*---------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_alv_grid IMPLEMENTATION.

  METHOD set_toolbar_buttons1 .

    DATA lt_button  TYPE ttb_button .

    lt_button = me->mt_toolbar_set .

    FIELD-SYMBOLS : <f1>  TYPE stb_button .

    LOOP AT lt_button ASSIGNING <f1> .
      CASE <f1>-function .
        WHEN 'FUN1' OR 'FUN2' OR 'FUN3' OR 'FUN4' OR 'FUN5'.

          <f1>-disabled = 'X' .
      ENDCASE .
    ENDLOOP .

    CALL METHOD me->set_toolbar_buttons
      EXPORTING
        toolbar_table = lt_button.

  ENDMETHOD .                    "SET_TOOLBAR_BUTTONS

  METHOD set_toolbar_buttons2 .

    DATA lt_button  TYPE ttb_button .

    lt_button = me->mt_toolbar_set .

    FIELD-SYMBOLS : <f1>  TYPE stb_button .

    LOOP AT lt_button ASSIGNING <f1> .
      CASE <f1>-function .
        WHEN 'FUN3' .
*          READ TABLE gt_list  WITH KEY confm = c_b
*                              TRANSPORTING NO FIELDS .
*          CHECK sy-subrc IS INITIAL .
*          <f1>-disabled = 'X' .
      ENDCASE .
    ENDLOOP .

    CALL METHOD me->set_toolbar_buttons
      EXPORTING
        toolbar_table = lt_button.

  ENDMETHOD .                    "SET_TOOLBAR_BUTTONS

ENDCLASS.                    "LCL_ALV_GRID IMPLEMENTATION



*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS : handle_toolbar
                 FOR EVENT toolbar OF cl_gui_alv_grid
                 IMPORTING e_object e_interactive.

    METHODS : handle_user_command
                 FOR EVENT user_command OF cl_gui_alv_grid
                 IMPORTING e_ucomm.

    METHODS : handle_after_user_command
                  FOR EVENT after_user_command OF cl_gui_alv_grid
                  IMPORTING  e_ucomm .

    METHODS : handle_data_changed
                 FOR EVENT data_changed OF cl_gui_alv_grid
                 IMPORTING er_data_changed e_onf4.

    METHODS : handle_data_changed_finished
                 FOR EVENT data_changed_finished OF cl_gui_alv_grid
                 IMPORTING e_modified
                           et_good_cells.

    METHODS : handle_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
                 IMPORTING sender
                           e_fieldname
                           e_fieldvalue
                           es_row_no
                           er_event_data
                           et_bad_cells
                           e_display.

    METHODS : handle_hotspot_click FOR EVENT hotspot_click
                                        OF cl_gui_alv_grid
                 IMPORTING e_row_id
                           e_column_id
                           es_row_no.

ENDCLASS.  "(lcl_event_handler DEFINITION)
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS lcl_event_handler IMPLEMENTATION.
*-- ToolBar
  METHOD handle_toolbar.
*    PERFORM toolbar_pros  USING e_object e_interactive.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.
*    PERFORM user_command_pros USING e_ucomm.
  ENDMETHOD.                    "handle_user_command

  METHOD handle_after_user_command.
*    PERFORM after_user_command_pros USING e_ucomm.
  ENDMETHOD.                    "handle_user_command

*-- Data change##
  METHOD handle_data_changed.
*    PERFORM data_changed  USING er_data_changed e_onf4.
  ENDMETHOD.                    "handle_data_changed

*-- Data change# #
  METHOD handle_data_changed_finished.
*    PERFORM data_changed_finished USING e_modified
*                                        et_good_cells.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED_FINISHED

*-- On help f4 - Search Help
  METHOD handle_onf4.
*    PERFORM on_f4 USING sender
*                        e_fieldname
*                        e_fieldvalue
*                        es_row_no
*                        er_event_data
*                        et_bad_cells
*                        e_display.
  ENDMETHOD.    "handle_onf4_click

  METHOD handle_hotspot_click.
    PERFORM hotspot_click USING e_row_id
                                e_column_id
                                es_row_no.
  ENDMETHOD.    "hotspot_click

ENDCLASS. "lcl_event_handler IMPLEMENTATION


*global variables
RANGES : r_btart   FOR fmioi-btart.
RANGES : r_btart_o FOR fmioi-btart.
RANGES : r_btart_c FOR fmioi-btart.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*-------------------------------------------------------------*
INITIALIZATION.

  REFRESH: r_btart, r_btart_o, r_btart_c.
  r_btart+0(3) = 'IEQ'.
  r_btart-low = '0100'.    APPEND r_btart.  "original
  r_btart-low = '0150'.    APPEND r_btart.  "change
  r_btart-low = '0350'.    APPEND r_btart.  "c/f prev year
*no need to consider c/f next year in the report - ANDY
*  r_btart-low = '0300'.    APPEND r_btart.  "c/f next year
  r_btart-low = '0200'.    APPEND r_btart.  "reduction
  r_btart-low = '0500'.    APPEND r_btart.  "adjust by f.u.doc

  r_btart_o+0(3) = 'IEQ'.
  r_btart_o-low = '0100'.    APPEND r_btart_o.  "original
*  r_btart_o-low = '0150'.    APPEND r_btart_o.  "change
  r_btart_o-low = '0350'.    APPEND r_btart_o.  "c/f prev year

  r_btart_c+0(3) = 'IEQ'.
*  r_btart_c-low = '0300'.    APPEND r_btart_c.  "c/f next year
  r_btart_c-low = '0200'.    APPEND r_btart_c.  "reduction
  r_btart_c-low = '0500'.    APPEND r_btart_c.  "adjust by f.u.doc


  REFRESH s_prof.
  s_prof-option = 'EQ'. s_prof-sign = 'I'.
  s_prof-low = 'F'. APPEND s_prof.
  s_prof-low = 'M'. APPEND s_prof.
  s_prof-low = 'Q'. APPEND s_prof.
  s_prof-low = 'H'. APPEND s_prof.
  s_prof-low = 'Y'. APPEND s_prof.


*-------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------*
  PERFORM get_master.

* Select Data from  FMIOI
  PERFORM get_data1.
* Process PO & PR Data
  PERFORM process_commitment.

* Select Data from  actual
  PERFORM get_data2.
  PERFORM process_fidocs.
  PERFORM merge_to_out.

  PERFORM get_data_gr.
  PERFORM fill_pay_info.


* Display ALV
  CALL SCREEN 0100.
*-------------------------------------------------------------*
END-OF-SELECTION.
*--------------------------------------------------------------*


*NCLUDE ZRFIR07NO01.
*&---------------------------------------------------------------------*
*&  Include           ZRFIR07O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_screen OUTPUT.

  PERFORM display_alv_ouput.

ENDMODULE.                 " INIT_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.


ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_SCREEN_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_screen_0200 OUTPUT.

  PERFORM display_alv_ouput_0200.

ENDMODULE.                 " INIT_SCREEN_0200  OUTPUT

*NCLUDE ZRFIR07NI01.
*&---------------------------------------------------------------------*
*&  Include           ZRFIR07I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CALL METHOD g_grid->check_changed_data.  "changed data

  DATA : l_code TYPE sy-ucomm.
  l_code = ok_code.
  CLEAR ok_code.
  CASE l_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMNAD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_commnad INPUT.
  CASE ok_code.
    WHEN 'EXIT' OR 'CANC'.
      IF sy-dynnr = '0110' OR
         sy-dynnr = '0120'.
        LEAVE TO SCREEN 0.
      ELSE.
        LEAVE PROGRAM.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " EXIT_COMMNAD  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CALL METHOD g_grid2->check_changed_data.  "changed data

  DATA : l_code2 TYPE sy-ucomm.
  l_code2 = ok_code.
  CLEAR ok_code.
  CASE l_code2.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'ENTR'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*NCLUDE ZRFIR07NF01.

*&---------------------------------------------------------------------*
*&  Include           ZRFIR07F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_VARI  text
*----------------------------------------------------------------------*
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
*&      Form  get_data1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data1 .

*  RANGES : r_erlkz FOR fmioi-erlkz.
*  r_erlkz+0(3) = 'EEQ'.
*  r_erlkz-low = 'F'.    "PR reduced
*  APPEND r_erlkz.
*  r_erlkz-low = 'X'.    "PO completed
*  APPEND r_erlkz.




* select pr/po details.
  IF p_dopen = 'X'.
    SELECT gjahr
           fistl
           fonds
           fipex
           objnrz
           btart
           wrttp
           fkbtr
           hkont
           lifnr
           vrefbt
           vrefbn
           vrfpos
           refbn
           rfpos
           refbt
           loekz
      INTO CORRESPONDING FIELDS OF TABLE gt_cmmt
      FROM  fmioi
     WHERE gjahr  =  p_gjahr   AND
           fistl  IN s_fistl   AND
           fonds  IN s_fonds   AND
           erlkz  = space      AND
           fipex  IN s_fipex   AND
*         gjahr  IN s_gjahr   AND   "ANDY
           perio  IN s_perio   AND
           btart  IN r_btart   AND
*          budat  IN s_budat   AND
*          hkont  IN s_hkont   AND
           refbt  IN s_refbt   AND
           refbn  IN s_refbn   AND
*           erlkz  IN r_erlkz   AND
           fikrs  =  p_fikrs   AND
           fkbtr <> 0.
  ELSEIF p_dfctr = 'X'.   "optimized for fund center
    SELECT gjahr
           fistl
           fonds
           fipex
           objnrz
           btart
           wrttp
           fkbtr
           hkont
           lifnr
           vrefbt
           vrefbn
           vrfpos
           refbn
           rfpos
           refbt
           loekz
    INTO CORRESPONDING FIELDS OF TABLE gt_cmmt
      FROM  fmioi
     WHERE gjahr  =  p_gjahr   AND
           fistl  IN s_fistl   AND
           fonds  IN s_fonds   AND
           fipex  IN s_fipex   AND
*         gjahr  IN s_gjahr   AND   "ANDY
           perio  IN s_perio   AND
           btart  IN r_btart   AND
*          budat  IN s_budat   AND
*          hkont  IN s_hkont   AND
           refbt  IN s_refbt   AND
           refbn  IN s_refbn   AND
*           erlkz  IN r_erlkz   AND
           fikrs  =  p_fikrs   AND
           fkbtr <> 0.
  ELSE.
    SELECT gjahr
           fistl
           fonds
           fipex
           objnrz
           btart
           wrttp
           fkbtr
           hkont
           lifnr
           vrefbt
           vrefbn
           vrfpos
           refbn
           rfpos
           refbt
           loekz
    INTO CORRESPONDING FIELDS OF TABLE gt_cmmt
      FROM  fmioi
     WHERE fonds  IN s_fonds   AND
           fipex  IN s_fipex   AND
           fistl  IN s_fistl   AND
           gjahr  = p_gjahr    AND   "ANDY
*        gjahr  IN s_gjahr   AND   "ANDY
           perio  IN s_perio   AND
           btart  IN r_btart   AND
*          budat  IN s_budat   AND
*          hkont  IN s_hkont   AND
           refbt  IN s_refbt   AND
           refbn  IN s_refbn   AND
*           erlkz  IN r_erlkz   AND
           fikrs  =  p_fikrs   AND
           fkbtr <> 0.
  ENDIF.

  DATA $ix TYPE i.
  DATA $profil  LIKE tbpfm-profil.

*** : C/F data that marked deletion are deleted.
*FIXME - ANDY
*  DELETE lt_tab WHERE btart = '0350'
*                  AND loekz = 'X'.

  EXIT.

* select only related budget profile of commitment item
  LOOP AT gt_cmmt.
    $ix = sy-tabix.
    CLEAR $profil.

    READ TABLE it_fmfpo WITH KEY fipos = gt_cmmt-fipex BINARY SEARCH.
*    PERFORM get_budget_period USING gt_cmmt-fistl
*                                    gt_cmmt-fipex
*                                    gt_cmmt-fonds
*                                    gt_cmmt-gjahr
*                              CHANGING $profil.
*    IF NOT $profil IN s_prof.
    IF sy-subrc NE 0.
      DELETE gt_cmmt INDEX $ix.
    ENDIF.

*    if gt_cmmt-refbt = '010'. "PR
*       append gt_cmmt to gt_cmmt_pr.
*    endif.
  ENDLOOP.

*  sort gt_cmmt_pr by REFBN RFPOS.

ENDFORM.                    " get_data1
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_OUPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_ouput .

  DATA : l_offline           TYPE char1.

  CALL METHOD cl_gui_alv_grid=>offline
    RECEIVING
      e_offline = l_offline.


*** : Full Screen
  IF gc_docking_container IS INITIAL.

    IF l_offline EQ 0.
      CREATE OBJECT gc_docking_container
        EXPORTING
          repid     = sy-repid
          dynnr     = sy-dynnr
          side      = gc_docking_container->dock_at_left
          extension = 2000.
    ENDIF.
  ENDIF.

if p_head eq 'X'.

* Create a splitter with 2 rows and 1 column
  CREATE OBJECT g_splitter
    EXPORTING
      parent  = gc_docking_container
      rows    = 2
      columns = 1.

** Upper Container
  CALL METHOD g_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = g_container_1.

** Lower Container
  CALL METHOD g_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = g_container_2.

** Upper Container height

  CALL METHOD g_splitter->set_row_height
    EXPORTING
      id     = 1
      height = 13.

  CREATE OBJECT g_grid
    EXPORTING
      i_parent          = g_container_2
*     i_appl_events     = 'X'
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4.

  IF sy-subrc <> 0.
    MESSAGE a004 WITH 'Screen Initialization Error' .
  ENDIF.

  PERFORM header.

else.

* create an instance of alv control
  CREATE OBJECT g_grid
    EXPORTING
      i_parent          = gc_docking_container  "g_customer
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4.

endif.

*- Tool Bar
  PERFORM exclude_of_toolbar_button USING 'GT_EXCLUDE'.

*- GRID (Display): Display
  PERFORM display_layout_attribute USING gs_layocat.

  PERFORM set_field_catalogs .

*- Edit Event   Event Handler
  PERFORM event_handler_register.

*- Sorting
  PERFORM build_sort_field.

*-- F4 FIELD
  PERFORM set_f4_field.

  g_repid = sy-repid.

*- At least field REPORT of this structure has to be filled!
  alv_variant-report = g_repid.

*- ALV Grid Display
  PERFORM alv_grid_display.

  CALL METHOD g_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.


  CLEAR g_title .

*   READ TABLE gt_extype  INTO ls_002
*                         WITH KEY zzcode = w_extype .
*   IF sy-subrc IS INITIAL .
*     CONCATENATE 'test' '[' '#### :'
*     INTO  g_title .
*   ENDIF .

*  CALL METHOD g_grid->set_gridtitle
*    EXPORTING
*      i_gridtitle = g_title.

ENDFORM.                    " DISPLAY_ALV_OUPUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_OF_TOOLBAR_BUTTON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0495   text
*----------------------------------------------------------------------*
FORM exclude_of_toolbar_button  USING  p_tabname.           "#EC *

  DATA : l_tab_name LIKE feld-name.

*-?
  FIELD-SYMBOLS : <table> TYPE ui_functions.

*-?
  CONCATENATE p_tabname '[]' INTO  l_tab_name.
  ASSIGN     (l_tab_name)    TO <table>.

*-
  PERFORM add_exclude_toolbar_button
         TABLES <table>
        USING : cl_gui_alv_grid=>mc_fc_excl_all. " **
*        USING : cl_gui_alv_grid=>mc_fc_loc_undo, "
*                cl_gui_alv_grid=>mc_fc_auf,      "
*                cl_gui_alv_grid=>mc_fc_average,  " &AVERAGE
**                cl_gui_alv_grid=>mc_fc_back_classic,
**                cl_gui_alv_grid=>mc_fc_call_abc, " &ABC
**                cl_gui_alv_grid=>mc_fc_call_chain,
**                cl_gui_alv_grid=>mc_fc_call_crbatch,
**                cl_gui_alv_grid=>mc_fc_call_crweb,
**                cl_gui_alv_grid=>mc_fc_call_lineitems,
**                cl_gui_alv_grid=>mc_fc_call_master_data,
**                cl_gui_alv_grid=>mc_fc_call_more,
**                cl_gui_alv_grid=>mc_fc_call_report,
**                cl_gui_alv_grid=>mc_fc_call_xint,
**                cl_gui_alv_grid=>mc_fc_call_xxl,
**                cl_gui_alv_grid=>mc_fc_col_invisible,
**                cl_gui_alv_grid=>mc_fc_col_optimize,
**                cl_gui_alv_grid=>mc_fc_current_variant,
**                cl_gui_alv_grid=>mc_fc_data_save,
**                cl_gui_alv_grid=>mc_fc_delete_filter,
**                cl_gui_alv_grid=>mc_fc_deselect_all,
**                cl_gui_alv_grid=>mc_fc_detail,
**                cl_gui_alv_grid=>mc_fc_expcrdata,
**                cl_gui_alv_grid=>mc_fc_expcrdesig,
**                cl_gui_alv_grid=>mc_fc_expcrtempl,
**                cl_gui_alv_grid=>mc_fc_expmdb,
**                cl_gui_alv_grid=>mc_fc_extend,
**                cl_gui_alv_grid=>mc_fc_f4,
**                cl_gui_alv_grid=>mc_fc_filter,
**                cl_gui_alv_grid=>mc_fc_find,
**                cl_gui_alv_grid=>mc_fc_fix_columns,
*                cl_gui_alv_grid=>mc_fc_graph,
**                cl_gui_alv_grid=>mc_fc_help,
**                cl_gui_alv_grid=>mc_fc_info,
**                cl_gui_alv_grid=>mc_fc_load_variant,
*                cl_gui_alv_grid=>mc_fc_loc_copy,          "
**                cl_gui_alv_grid=>mc_fc_html,
*                cl_gui_alv_grid=>mc_fc_loc_copy_row,      "
*                cl_gui_alv_grid=>mc_fc_loc_cut,           "
*                cl_gui_alv_grid=>mc_fc_loc_delete_row,    "
*                cl_gui_alv_grid=>mc_fc_loc_insert_row,    "
*                cl_gui_alv_grid=>mc_fc_loc_move_row,
*                cl_gui_alv_grid=>mc_fc_loc_append_row,    "
*                cl_gui_alv_grid=>mc_fc_loc_paste,         "
*                cl_gui_alv_grid=>mc_fc_loc_paste_new_row. "
**                cl_gui_alv_grid=>mc_fc_maintain_variant,
**                cl_gui_alv_grid=>mc_fc_maximum,
**                cl_gui_alv_grid=>mc_fc_minimum,
**                cl_gui_alv_grid=>mc_fc_pc_file,
**                cl_gui_alv_grid=>mc_fc_print,
**                cl_gui_alv_grid=>mc_fc_print_back,
**                cl_gui_alv_grid=>mc_fc_print_prev,
**                cl_gui_alv_grid=>mc_fc_refresh,
**                cl_gui_alv_grid=>mc_fc_reprep,
**                cl_gui_alv_grid=>mc_fc_save_variant,
**                cl_gui_alv_grid=>mc_fc_select_all,
**                cl_gui_alv_grid=>mc_fc_send,
**                cl_gui_alv_grid=>mc_fc_separator,
**                cl_gui_alv_grid=>mc_fc_sort,
**                cl_gui_alv_grid=>mc_fc_sort_asc,
**                cl_gui_alv_grid=>mc_fc_sort_dsc,
**                cl_gui_alv_grid=>mc_fc_subtot,
**                cl_gui_alv_grid=>mc_mb_sum,
**                cl_gui_alv_grid=>mc_fc_sum.
**                cl_gui_alv_grid=>mc_fc_to_office,
**                cl_gui_alv_grid=>mc_fc_to_rep_tree,
**                cl_gui_alv_grid=>mc_fc_unfix_columns,
**                cl_gui_alv_grid=>mc_fc_views,
**                cl_gui_alv_grid=>mc_fc_view_crystal,
**                cl_gui_alv_grid=>mc_fc_view_excel,
**                cl_gui_alv_grid=>mc_fc_view_grid,
**                cl_gui_alv_grid=>mc_fc_word_processor.

ENDFORM.                    " EXCLUDE_OF_TOOLBAR_BUTTON
*&---------------------------------------------------------------------*
*&      Form  ADD_EXCLUDE_TOOLBAR_BUTTON
*&---------------------------------------------------------------------*
FORM add_exclude_toolbar_button  TABLES   p_table
                                 USING    p_value.          "#EC *

  DATA: l_exclude TYPE ui_func.

  l_exclude = p_value.
  APPEND l_exclude TO p_table. "

ENDFORM.                    " ADD_EXCLUDE_TOOLBAR_BUTTON
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CATALOGS
*&---------------------------------------------------------------------*
FORM set_field_catalogs .  "

  CLEAR   gt_fieldcat.

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'SORT' ,
*          ' ' 'OUTPUTLEN'  '1' ,
*          ' ' 'COLTEXT'     'Type',
*          ' ' 'NO_OUT'     'X',
*          'E' 'KEY'         'X' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'FONDS' ,
          ' ' 'OUTPUTLEN'  '8' ,
          ' ' 'COLTEXT'     'Fund',
          'E' 'KEY'         'X' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'FISTL' ,
          ' ' 'OUTPUTLEN'  '8' ,
          ' ' 'COLTEXT'     'FundCtr',
          'E' 'KEY'         'X' .

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'KOSTL' ,
*          ' ' 'OUTPUTLEN'  '10' ,
*          ' ' 'COLTEXT'     'Cost Ctr',
*          ' ' 'NO_OUT'      'X',
*          'E' 'KEY'         'X' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'FIPEX' ,
          ' ' 'OUTPUTLEN'  '8' ,
          ' ' 'COLTEXT'     'CommitItm',
          'E' 'KEY'         'X' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'HKONT' ,
          ' ' 'OUTPUTLEN'  '8' ,
          ' ' 'COLTEXT'     'G/L Account',
          ' ' 'NO_OUT'      'X',
          'E' 'KEY'         'X' .

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'AUFNR' ,
*          ' ' 'OUTPUTLEN'  '12' ,
*          ' ' 'COLTEXT'     'Order',
*          ' ' 'NO_OUT'      'X',
*          'E' 'KEY'         ' ' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'LIFNR' ,
          ' ' 'OUTPUTLEN'  '10' ,
          ' ' 'COLTEXT'     'Vendor',
          'E' 'KEY'         'X' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'CFFLD' ,
          ' ' 'OUTPUTLEN'   '3' ,
          ' ' 'COLTEXT'     'C/F',
          'E' 'KEY'         'X' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'DOCTX' ,
          ' ' 'OUTPUTLEN'  '5' ,
          ' ' 'COLTEXT'     'Stat',
          'E' 'KEY'         'X' .

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'REFBN' ,
*          ' ' 'OUTPUTLEN'  '10' ,
*          ' ' 'COLTEXT'     'Document',
*          ' ' 'HOTSPOT'     'X',
*          ' ' 'NO_OUT'      'X',
*          'E' 'KEY'         'X' .
*
*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'RFPOS' ,
*          ' ' 'OUTPUTLEN'  '5' ,
*          ' ' 'COLTEXT'     'Itm',
*          ' ' 'NO_OUT'      'X',
*          'E' 'KEY'         'X' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'FKBTR' ,
          ' ' 'OUTPUTLEN'  '10' ,
          ' ' 'COLTEXT'    'Cmmts/Act',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'FM_ORIG' ,
*          ' ' 'OUTPUTLEN'  '10' ,
*          ' ' 'COLTEXT'    'CI-Org',
*          ' ' 'REF_TABLE'   'FMIOI',
*          ' ' 'REF_FIELD'   'FKBTR',
*          ' ' 'NO_OUT'      'X',
*          ' ' 'DO_SUM'      'X',
*          'E' 'KEY'         '' .
*
  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'PRDOC' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'PR Document',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'PRPOS' ,
          ' ' 'OUTPUTLEN'   '2' ,
          ' ' 'COLTEXT'     'Itm',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'PRAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'PR Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'PODOC' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'PO Document',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'POPOS' ,
          ' ' 'OUTPUTLEN'   '2' ,
          ' ' 'COLTEXT'     'Itm',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'POAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'PO Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'GRDOC' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'GR Document',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'GRPOS' ,
          ' ' 'OUTPUTLEN'   '2' ,
          ' ' 'COLTEXT'     'Itm',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'GRAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'GR Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'IVDOCS' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'IV Document',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'IVAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'IV Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'DPDOC' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Downpay Doc',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'DPAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Downpay Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'TFDOC' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Trf Doc',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'TFAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Trf Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'AUGDT' ,
          ' ' 'OUTPUTLEN'   '08' ,
          ' ' 'COLTEXT'     'Pymt Date',
          'E' 'KEY'         '' .

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'PYDOC' ,
*          ' ' 'OUTPUTLEN'   '10' ,
*          ' ' 'COLTEXT'     'Pymt Doc.',
*          ' ' 'HOTSPOT'     'X',
*          'E' 'KEY'         '' .
*
*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'PYAMT' ,
*          ' ' 'OUTPUTLEN'   '10' ,
*          ' ' 'COLTEXT'     'Pymt Amt',
*          ' ' 'REF_TABLE'   'FMIOI',
*          ' ' 'REF_FIELD'   'FKBTR',
*          ' ' 'DO_SUM'      'X',
*          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs USING :
          'S' 'FIELDNAME'   'BLAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Balance Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .


ENDFORM.                    " SET_FIELD_CATALOGS

*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATALOGS
*&---------------------------------------------------------------------*
FORM fill_field_catalogs  USING  p_gub  p_fname  p_value.

*- 'S' -> Start
*- 'E' -> End

  IF p_gub = 'S'.

    CLEAR gs_fieldcat.

  ENDIF.

*-
  DATA l_fname(40).
  FIELD-SYMBOLS <fs> TYPE any.
  CONCATENATE 'GS_FIELDCAT-' p_fname INTO l_fname.

  ASSIGN (l_fname) TO <fs>.
  <fs> = p_value.

  IF p_gub = 'E'.

    APPEND gs_fieldcat TO gt_fieldcat.

  ENDIF.

ENDFORM.                    " FILL_FIELD_CATALOGS

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LAYOUT_ATTRIBUTE
*&---------------------------------------------------------------------*
FORM display_layout_attribute  USING  p_layocat TYPE lvc_s_layo.

*- General display options
*  p_layocat-cwidth_opt = 'X'.
  p_layocat-sel_mode   = 'A'.
*  p_layocat-edit       = C_X .
  p_layocat-smalltitle  = 'X' .
  p_layocat-stylefname = 'CELLTAB'.
*  P_LAYOCAT-CTAB_FNAME = 'F_COL'.


*  p_layocat-excp_fname = 'CHK' .
*  p_layocat-excp_led   = 'X' .


ENDFORM.                    " DISPLAY_LAYOUT_ATTRIBUTE
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
FORM alv_grid_display .
*-- Display
  CALL METHOD g_grid->set_table_for_first_display
       EXPORTING is_layout            = gs_layocat
*                 it_toolbar_excluding = gt_exclude
                 i_save               = 'A'   "
*                                       'U'
*                                       'X'   "
*                                       ' '   "
*                i_default            = 'X'   "
*  LAYOUT   Standard   BCALV_GRID_09  ##.
                 is_variant           = alv_variant  " ## ## display
       CHANGING  it_outtab            = gt_out[]
                 it_sort              = gt_sort
                 it_fieldcatalog      = gt_fieldcat[].

ENDFORM.                    " ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT_FIELD
*&---------------------------------------------------------------------*
FORM build_sort_field .

** Sort FIELD SETTING
  gs_sort-fieldname = 'DOCTX'.
  gs_sort-spos      = '1'.
  gs_sort-up        = 'X'.
  gs_sort-subtot    = 'X'.
  APPEND gs_sort TO gt_sort.

ENDFORM.                    " build_sort_field
*&---------------------------------------------------------------------*
*&      Form  event_handler_register
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM event_handler_register .

*- Edit  REGISTER EVENT
* IF MC_EVT_MODIFY
  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*- Event Handler
  CREATE OBJECT g_event_handler.

  SET HANDLER g_event_handler->handle_toolbar            FOR g_grid.
  SET HANDLER g_event_handler->handle_user_command       FOR g_grid.
  SET HANDLER g_event_handler->handle_after_user_command FOR g_grid.
  SET HANDLER g_event_handler->handle_onf4               FOR g_grid.
  SET HANDLER g_event_handler->handle_data_changed       FOR g_grid.
  SET HANDLER g_event_handler->handle_data_changed_finished
                                                         FOR g_grid.
  SET HANDLER g_event_handler->handle_hotspot_click      FOR g_grid.
ENDFORM.                    " event_handler_register
*&---------------------------------------------------------------------*
*&      Form  refresh_table_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_table_display .

  l_scroll-row = 'X'.
  l_scroll-col = 'X'.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      i_soft_refresh = ' '
      is_stable      = l_scroll.     "refresh

*  CALL METHOD g_grid->refresh_table_display
*    EXPORTING
*      i_soft_refresh = c_x.

*  CALL METHOD g_grid->set_toolbar_buttons2( ) .

ENDFORM.                    " refresh_table_display
*&---------------------------------------------------------------------*
*&      Form  SET_F4_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_f4_field .

  REFRESH gt_f4 .
*-- register F4 FIELD
**-- [Caution] have to register fieldname ascending.

*   gs_f4-fieldname  = '*****'.
*   gs_f4-register   = 'X'.
*   APPEND gs_f4 TO gt_f4.
*
*   gs_f4-fieldname  = '*****'.
*   gs_f4-register   = 'X'.
*   APPEND gs_f4 TO gt_f4.
*

  CALL METHOD g_grid->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4.


ENDFORM.                    " SET_F4_FIELD
*&---------------------------------------------------------------------*
*&      Form  GET_BUDGET_PERIOD
*&---------------------------------------------------------------------*
FORM get_budget_period USING    f_fictr f_fipos f_geber f_gjahr
                       CHANGING f_profil .

  READ TABLE it_fmfpo  WITH KEY fipos = f_fipos.
  IF sy-subrc = 0.
    PERFORM determine_profile_fs USING    p_fikrs
                                          f_fictr
                                          it_fmfpo-posit
                                          f_geber
                                          f_gjahr
                                 CHANGING f_profil.
  ELSE.
    f_profil = ' '.
  ENDIF.

ENDFORM.                    " get_budget_period
*&---------------------------------------------------------------------*
*&      Form  determine_profile_fs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FIK  text
*      -->P_F_FICTR  text
*      -->P_IT_FMFPO_POSIT  text
*      -->P_F_GEBER  text
*      -->P_P_GJR  text
*      <--P_F_PROFIL  text
*----------------------------------------------------------------------*
FORM determine_profile_fs USING    l_fikrs
                                   l_fictr
                                   l_posit
                                   l_geber
                                   l_gjahr
                          CHANGING l_bprof.
  DATA: l_objnr LIKE fmfctr-ctr_objnr.
  DATA: l_farea LIKE  bpja-farea.


  l_objnr(2) = 'FS'.
  l_objnr+2(4) = l_fikrs.
  l_objnr+6  = l_fictr.

* Profile from TBPFM table.
  CALL FUNCTION 'KBPA_FIFM_GET_PROFIL'
    EXPORTING
      i_objnr         = l_objnr
      i_posit         = l_posit
      i_geber         = l_geber
      i_gjahr         = l_gjahr
      i_farea         = l_farea
    IMPORTING
      e_profil        = l_bprof
    EXCEPTIONS
      no_profil_found = 01.

  IF NOT sy-subrc IS INITIAL.
*   Profile from FundMgt Area
    CALL FUNCTION 'FM5B_GET_PROFILE'
      EXPORTING
        i_fikrs           = l_fikrs
        i_fincode         = l_geber
      IMPORTING
        e_profil          = l_bprof
      EXCEPTIONS
        fm_area_not_found = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_BUDGET_PERIOD
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM hotspot_click  USING    p_row
                             p_column
                             ps_row_no.
  DATA: ls_out   TYPE ty_output,
        ls_inv   TYPE ty_output,
        ls_dnp   TYPE ty_output,
        ls_trf   TYPE ty_output,
        ls_gr    TYPE ty_gr.

  DATA: L_NUM(11) TYPE C VALUE ' 0123456789'.

  CASE p_column.
*    WHEN 'REFBN'.
*
*      REFRESH it_detail2.
*
*      READ TABLE gt_out INDEX p_row into ls_out.
*
*      CHECK ls_out-podoc IS NOT INITIAL.
*
*      IF ls_out-doctx = 'PR'.
*        SET PARAMETER ID: 'BAN' FIELD ls_out-prdoc.
*        CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.
*      ELSEIF ls_out-doctx = 'PO'.
*        SET PARAMETER ID: 'BES' FIELD ls_out-podoc.
*        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
*      ELSEIF ls_out-doctx = 'IV'.
*        SET PARAMETER ID: 'BLN' FIELD ls_out-ivdocs,
*                          'BUK' FIELD p_fikrs,
*                          'GJR' FIELD ls_out-ivghr.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.

    WHEN 'PRDOC'.
** On 09/05/13
      CHECK p_row CO L_NUM.
** end on 09/05/13
      READ TABLE gt_out INDEX p_row INTO ls_out.
      CHECK ls_out-prdoc IS NOT INITIAL.

      SET PARAMETER ID: 'BAN' FIELD ls_out-prdoc.
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

    WHEN 'PODOC'.
** On 09/05/13
      CHECK p_row CO L_NUM.
** end on 09/05/13
      REFRESH it_detail2.

      READ TABLE gt_out INDEX p_row INTO ls_out.
      CHECK ls_out-podoc IS NOT INITIAL.

*      IF ls_out-podoc+0(1) = '*'.
*        LOOP AT it_detail WHERE doctx = 'PO'
*                            AND refbn = ls_out-podoc
*                            AND rfpos = ls_out-prpos.
*          APPEND it_detail TO it_detail2.
*        ENDLOOP.
*
*        CALL SCREEN 0200 STARTING AT 10 10
*                         ENDING   AT 80 20.
*      ELSE.
      SET PARAMETER ID: 'BES' FIELD ls_out-podoc.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
*      ENDIF.

    WHEN 'GRDOC'.
** On 09/05/13
      CHECK p_row CO L_NUM.
** end on 09/05/13
      REFRESH it_detail2.

      READ TABLE gt_out INDEX p_row INTO ls_out.

      CHECK ls_out-grdoc IS NOT INITIAL.

      IF ls_out-grdoc+0(1) = '*'.
        LOOP AT gt_gr INTO ls_gr
                      WHERE ebeln = ls_out-podoc
                        AND ebelp = ls_out-popos.
          it_detail2-refbn = ls_gr-ebeln.
          it_detail2-rfpos = ls_gr-ebelp.
          it_detail2-doctx = 'GR'.
          it_detail2-gjahr = ls_gr-gjahr.
          it_detail2-docnr = ls_gr-belnr.
          it_detail2-docid = ls_gr-buzei.
          it_detail2-amtxx = ls_gr-wrbtr.

          APPEND it_detail2.
        ENDLOOP.

        CALL SCREEN 0200 STARTING AT 10 10
                         ENDING   AT 80 20.
      ELSE.
        SET PARAMETER ID: 'MBN' FIELD ls_out-grdoc.
        SET PARAMETER ID: 'MJA' FIELD ls_out-grghr.
        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'IVDOCS'.
** On 09/05/13
      CHECK p_row CO L_NUM.
** end on 09/05/13
      REFRESH it_detail2.

      READ TABLE gt_out INDEX p_row INTO ls_out.
      CHECK ls_out-ivdocs IS NOT INITIAL.

      IF ls_out-ivdocs+0(1) = '*'.

        LOOP AT gt_inv INTO ls_inv
           WHERE podoc = ls_out-podoc
             AND popos = ls_out-popos
             AND prdoc = ls_out-prdoc
             AND prpos = ls_out-prpos
             AND fonds = ls_out-fonds
             AND fistl = ls_out-fistl
             AND fipex = ls_out-fipex
             AND hkont = ls_out-hkont
             AND lifnr = ls_out-lifnr.

          it_detail2-doctx = 'IV'.
          it_detail2-gjahr = ls_inv-ivghr.
          it_detail2-docnr = ls_inv-ivdocs.
          it_detail2-amtxx = ls_inv-ivamt.

          APPEND it_detail2.
        ENDLOOP.

        CALL SCREEN 0200 STARTING AT 10 10
                         ENDING   AT 70 20.
      ELSE.
        SET PARAMETER ID: 'BLN' FIELD ls_out-ivdocs,
                          'BUK' FIELD  p_fikrs,
                          'GJR' FIELD ls_out-ivghr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'DPDOC'.
** On 09/05/13
      CHECK p_row CO L_NUM.
** end on 09/05/13
      REFRESH it_detail2.

      READ TABLE gt_out INDEX p_row INTO ls_out.
      CHECK ls_out-dpdoc IS NOT INITIAL.

      IF ls_out-dpdoc+0(1) = '*'.
        LOOP AT gt_dnp INTO ls_dnp
           WHERE podoc = ls_out-podoc
             AND popos = ls_out-popos
             AND prdoc = ls_out-prdoc
             AND prpos = ls_out-prpos
             AND fonds = ls_out-fonds
             AND fistl = ls_out-fistl
             AND fipex = ls_out-fipex
             AND hkont = ls_out-hkont
             AND lifnr = ls_out-lifnr.

          it_detail2-doctx = 'IV'.
          it_detail2-gjahr = ls_dnp-dpghr.
          it_detail2-docnr = ls_dnp-dpdoc.
          it_detail2-amtxx = ls_dnp-dpamt.

          APPEND it_detail2.
        ENDLOOP.

        CALL SCREEN 0200 STARTING AT 10 10
                         ENDING   AT 80 20.
      ELSE.
        SET PARAMETER ID: 'BLN' FIELD ls_out-dpdoc,
                          'BUK' FIELD  p_fikrs,
                          'GJR' FIELD ls_out-dpghr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'TFDOC'.
** On 09/05/13
      CHECK p_row CO L_NUM.
** end on 09/05/13
      REFRESH it_detail2.

      READ TABLE gt_out INDEX p_row INTO ls_out.
      CHECK ls_out-tfdoc IS NOT INITIAL.

      IF ls_out-tfdoc+0(1) = '*'.
        LOOP AT gt_trf INTO ls_trf
           WHERE podoc = ls_out-podoc
             AND popos = ls_out-popos
             AND prdoc = ls_out-prdoc
             AND prpos = ls_out-prpos
             AND fonds = ls_out-fonds
             AND fistl = ls_out-fistl
             AND fipex = ls_out-fipex
             AND hkont = ls_out-hkont.
*             and lifnr = ls_out-lifnr.

          it_detail2-doctx = 'TF'.
          it_detail2-gjahr = ls_dnp-tfghr.
          it_detail2-docnr = ls_dnp-tfdoc.
          it_detail2-amtxx = ls_dnp-tfamt.

          APPEND it_detail2.
        ENDLOOP.

        CALL SCREEN 0200 STARTING AT 10 10
                         ENDING   AT 80 20.
      ELSE.
        SET PARAMETER ID: 'BLN' FIELD ls_out-tfdoc,
                          'BUK' FIELD  p_fikrs,
                          'GJR' FIELD ls_out-tfghr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.


*** --> Screen 0200 command
    WHEN 'DOCNR'.

      READ TABLE it_detail2 INDEX p_row.

      CASE it_detail2-doctx.
        WHEN 'IV' OR 'DP' OR 'TF' OR 'P2'.
          SET PARAMETER ID: 'BLN' FIELD it_detail2-docnr,
                            'BUK' FIELD  p_fikrs,
                            'GJR' FIELD it_detail2-gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'PO'.
          SET PARAMETER ID: 'BES' FIELD it_detail2-docnr.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        WHEN 'GR'.
          SET PARAMETER ID: 'MBN' FIELD it_detail2-docnr.
          SET PARAMETER ID: 'MJA' FIELD it_detail2-gjahr.
          CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.

  ENDCASE.

ENDFORM.                    " HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_OUPUT_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_ouput_0200 .

  IF g_customer2 IS INITIAL.

    CREATE OBJECT g_customer2
      EXPORTING
        container_name              = c_container
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0 .

    ENDIF.

*   create an instance of alv control
    CREATE OBJECT g_grid2
      EXPORTING
        i_parent          = g_customer2
*       i_appl_events     = 'X'
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4.

    IF sy-subrc <> 0.
      MESSAGE a004 WITH 'Screen Initialization Error' .
    ENDIF.

  ENDIF.

  CLEAR : gt_fieldcat2[].
*- Tool Bar
  PERFORM exclude_of_toolbar_button2 USING 'GT_EXCLUDE2'.

*- GRID (Display): Display
  PERFORM display_layout_attribute2 USING gs_layocat2.

  PERFORM set_field_catalogs2 .

*- Edit Event   Event Handler
  PERFORM event_handler_register2.

*- Sorting
*  PERFORM build_sort_field.

  g_repid2 = sy-repid.

*- At least field REPORT of this structure has to be filled!
  alv_variant2-report = g_repid2.

*- ALV Grid Display
  PERFORM alv_grid_display2.

  CALL METHOD g_grid2->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.


  CLEAR g_title2 .

*   READ TABLE gt_extype  INTO ls_002
*                         WITH KEY zzcode = w_extype .
*   IF sy-subrc IS INITIAL .
*     CONCATENATE g_title '[' '#### :'    ls_002-zzcdname ']'
*     INTO  g_title .
*   ENDIF .

  CALL METHOD g_grid2->set_gridtitle
    EXPORTING
      i_gridtitle = g_title2.

ENDFORM.                    " DISPLAY_ALV_OUPUT_0200
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_OF_TOOLBAR_BUTTON2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2786   text
*----------------------------------------------------------------------*
FORM exclude_of_toolbar_button2  USING  p_tabname.

  DATA : l_tab_name LIKE feld-name.

*-?
  FIELD-SYMBOLS : <table> TYPE ui_functions.

*-?
  CONCATENATE p_tabname '[]' INTO  l_tab_name.
  ASSIGN     (l_tab_name)    TO <table>.

*-
  PERFORM add_exclude_toolbar_button
         TABLES <table>
        USING : cl_gui_alv_grid=>mc_fc_excl_all. " **

ENDFORM.                    " EXCLUDE_OF_TOOLBAR_BUTTON2
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LAYOUT_ATTRIBUTE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_LAYOCAT2  text
*----------------------------------------------------------------------*
FORM display_layout_attribute2  USING  p_layocat TYPE lvc_s_layo.

*- General display options
  p_layocat-cwidth_opt = 'X'.
  p_layocat-sel_mode   = 'A'.
*  p_layocat-edit       = C_X .
  p_layocat-smalltitle  = 'X' .
*  p_layocat-stylefname = 'CELLTAB'.
*  P_LAYOCAT-CTAB_FNAME = 'F_COL'.


*  p_layocat-excp_fname = 'CHK' .
*  p_layocat-excp_led   = 'X' .

ENDFORM.                    " DISPLAY_LAYOUT_ATTRIBUTE2
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CATALOGS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_catalogs2 .

  CLEAR   gt_fieldcat2.

  PERFORM fill_field_catalogs2 USING :
          'S' 'FIELDNAME'   'DOCTX' ,
          ' ' 'OUTPUTLEN'  '2' ,
          ' ' 'COLTEXT'     'Doc.Category',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs2 USING :
          'S' 'FIELDNAME'   'GJAHR' ,
          ' ' 'OUTPUTLEN'  '4' ,
          ' ' 'COLTEXT'     'Fiscal Year',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs2 USING :
          'S' 'FIELDNAME'   'DOCNR' ,
          ' ' 'OUTPUTLEN'  '10' ,
          ' ' 'COLTEXT'     'Document',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs2 USING :
          'S' 'FIELDNAME'   'DOCID' ,
          ' ' 'OUTPUTLEN'  '5' ,
          ' ' 'COLTEXT'     'Itm',
          'E' 'KEY'         '' .

  PERFORM fill_field_catalogs2 USING :
          'S' 'FIELDNAME'   'AMTXX' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Amount',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          'E' 'KEY'         '' .

ENDFORM.                    " SET_FIELD_CATALOGS2
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATALOGS2
*&---------------------------------------------------------------------*
FORM fill_field_catalogs2  USING  p_gub  p_fname  p_value.

*- 'S' -> Start
*- 'E' -> End

  IF p_gub = 'S'.

    CLEAR gs_fieldcat2.

  ENDIF.

*-
  DATA l_fname(40).
  FIELD-SYMBOLS <fs> TYPE any.
  CONCATENATE 'GS_FIELDCAT2-' p_fname INTO l_fname.

  ASSIGN (l_fname) TO <fs>.
  <fs> = p_value.

  IF p_gub = 'E'.

    APPEND gs_fieldcat2 TO gt_fieldcat2.

  ENDIF.

ENDFORM.                    " FILL_FIELD_CATALOGS2
*&---------------------------------------------------------------------*
*&      Form  EVENT_HANDLER_REGISTER2
*&---------------------------------------------------------------------*
FORM event_handler_register2 .
*- Edit  REGISTER EVENT
* IF MC_EVT_MODIFY
  CALL METHOD g_grid2->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*- Event Handler
  CREATE OBJECT g_event_handler2.

  SET HANDLER g_event_handler2->handle_hotspot_click      FOR g_grid2.

ENDFORM.                    " EVENT_HANDLER_REGISTER2
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY2
*&---------------------------------------------------------------------*
FORM alv_grid_display2 .
*-- Display
  CALL METHOD g_grid2->set_table_for_first_display
       EXPORTING
                 is_layout            = gs_layocat2
                 it_toolbar_excluding = gt_exclude2
                 i_save               = 'A'   "
*                                       'U'
*                                       'X'   "
*                                       ' '   "
*                i_default            = 'X'   "
*                is_variant           = alv_variant2  " ## ## display
       CHANGING  it_outtab            = it_detail2[]
                 it_sort              = gt_sort2
                 it_fieldcatalog      = gt_fieldcat2[].

ENDFORM.                    " ALV_GRID_DISPLAY2
*&---------------------------------------------------------------------*
*&      Form  HEADER
*&---------------------------------------------------------------------*
FORM header .

  DATA: l_document TYPE REF TO cl_dd_document,
        l_doctable TYPE REF TO cl_dd_table_element,
        l_column1  TYPE REF TO cl_dd_area,
        l_column2  TYPE REF TO cl_dd_area.

  CREATE OBJECT l_document.
*  CALL METHOD l_document->add_text
*    EXPORTING
*      text      = 'Commitment/Actual Line Items for Assigned Annual Budget'
*      sap_style = cl_dd_area=>small. "heading.

  CALL METHOD l_document->add_table
    EXPORTING
      no_of_columns               = 1
      cell_background_transparent = 'X'
      border                      = '0'
    IMPORTING
      table                       = l_doctable.
*
  CALL METHOD l_doctable->add_column
    IMPORTING
      column = l_column1.

    PERFORM titles CHANGING l_column1.

  CALL METHOD l_doctable->add_column
    IMPORTING
      column = l_column2.

  CALL METHOD l_column2->add_gap
    EXPORTING
      width = 150.

*  CALL METHOD l_column2->add_picture
*    EXPORTING
*      picture_id = 'ZPICTURE'. "WHAT EVER GRAPHIC YOU NEED

  CALL METHOD l_document->merge_document.
  CALL METHOD l_document->display_document
    EXPORTING
      parent = g_container_1.

ENDFORM.                    " HEADER
*&---------------------------------------------------------------------*
*&      Form  TITLES
*&---------------------------------------------------------------------*
FORM titles CHANGING dg_dyndoc_id TYPE REF TO cl_dd_area.

  DATA : dl_text(255) TYPE c.  "Text
  DATA : l_bezei TYPE fm_bezeich.

*  dl_text = sy-title.
*
*
*  CONDENSE dl_text.
*  CALL METHOD dg_dyndoc_id->add_text
*    EXPORTING
*      text         = dl_text
*      sap_fontsize = cl_dd_area=>large.
*
*  CALL METHOD dg_dyndoc_id->new_line.
*  CLEAR : dl_text.

*  CALL METHOD dg_dyndoc_id->new_line.

  dl_text = 'Funds Center :'.

  CALL METHOD dg_dyndoc_id->add_gap.

  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text = dl_text.

  CLEAR dl_text.

  SELECT SINGLE bezeich
    INTO l_bezei
    FROM fmfctrt
   WHERE spras = 'E'
     AND fikrs =  p_fikrs
     AND fictr = s_fistl-low.

  CONCATENATE s_fistl-low ' - ' l_bezei INTO dl_text SEPARATED BY space.

  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text = dl_text.

  CALL METHOD dg_dyndoc_id->new_line.

  CLEAR : dl_text.

  dl_text = 'Commitment Item :'.

  CALL METHOD dg_dyndoc_id->add_gap.

  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text = dl_text.

  CLEAR dl_text.

  READ TABLE s_fipex INDEX 1.

  SELECT SINGLE bezeich INTO l_bezei
    FROM fmfpot
   WHERE spras = 'E'
     AND fikrs =  p_fikrs
     AND fipos = s_fipex-low.

  CONCATENATE s_fipex-low ' - ' l_bezei INTO dl_text  SEPARATED BY space.

  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text = dl_text.

  CALL METHOD dg_dyndoc_id->new_line.

  dl_text = 'Cmmt/Actual : '.

  CALL METHOD dg_dyndoc_id->add_gap.

  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text = dl_text.

  CLEAR dl_text.

  WRITE g_total TO dl_text CURRENCY 'USD'.

  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text = dl_text.

ENDFORM.                    "TITLES
*&---------------------------------------------------------------------*
*&      Form  GET_DATA2
*&---------------------------------------------------------------------*
FORM get_data2 .

  REFRESH: gt_fi.

*FM Actual Line Items from FI
*paid next year... no need to consider...just show clearing document

  SELECT gjahr  perio AS poper
         fistl  fonds fipex hkont
         objnrz             "Object number (order)
         awtyp
         btart
         wrttp
         lifnr
         kngjahr
         knbelnr AS refbn   "FI
         vrefbt
         vrefbn             "PO
         vrfpos
         SUM( fkbtr ) AS fkbtr
    INTO CORRESPONDING FIELDS OF TABLE gt_fi
    FROM  v_fmifi
   WHERE fikrs  =  p_fikrs   AND
         gjahr  =  p_gjahr   AND    "ANDY
         btart  =  '0100'    AND    " ANDY , '0350' original only
         fonds  IN s_fonds   AND
         fistl  IN s_fistl   AND
         fipex  IN s_fipex   AND
         ( vrefbn IN s_refbn   OR fmbelnr IN s_refbn ) AND
         perio  IN s_perio   AND
*         zhldt  IN s_zhldt   AND
         fkbtr <> 0
  GROUP BY gjahr perio fistl fonds fipex hkont
         objnrz awtyp btart wrttp
         lifnr vrefbt vrefbn vrfpos
         kngjahr knbelnr
%_HINTS ORACLE 'FIRST_ROWS(10)'.


ENDFORM.                    " GET_DATA2
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMMITMENT
*&---------------------------------------------------------------------*
FORM process_commitment.
  DATA: ls_cmmt   LIKE gt_cmmt.
  DATA: ls_cmmt_t LIKE gt_cmmt.
  DATA: ls_out    TYPE ty_output.
  DATA: ls_pr     TYPE gty_prpo.
  DATA: ls_po     TYPE gty_prpo.

  DATA: lt_tab_pr LIKE ls_cmmt OCCURS 0 WITH HEADER LINE.
  DATA: lt_tab_po LIKE ls_cmmt OCCURS 0 WITH HEADER LINE.

* proces PO, PR sequence
  SORT gt_cmmt BY refbt DESCENDING
                  refbn rfpos btart.

** : seperate PR/PO
  DATA: lv_idx    LIKE sy-tabix.
  DATA: lv_sorted TYPE char1.
  CLEAR lv_sorted.
  LOOP AT gt_cmmt INTO ls_cmmt.
    lv_idx = sy-tabix.
*    CHECK ls_cmmt-btart IN r_btart_o.   "original commitment only

    CLEAR ls_out.

    MOVE-CORRESPONDING ls_cmmt TO ls_out.
    ls_out-fkbtr   = ls_cmmt-fkbtr * -1 .

*-- need to summarize
    CLEAR: ls_out-refbt, ls_out-refbn, ls_out-rfpos.

*purchase request only w/o PO
    IF ls_cmmt-refbt = '010'.
      IF lv_sorted = space.
        SORT gt_pr BY refbn rfpos.
      ENDIF.
      READ TABLE gt_pr WITH KEY refbn = ls_cmmt-refbn
                                rfpos = ls_cmmt-rfpos BINARY SEARCH
                            TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.  "processed already
      ELSE.
*        ls_out-doctx = 'PR'. "later...
*        ls_out-prghr = ls_cmmt-gjahr.
        ls_out-prdoc = ls_cmmt-refbn.
        ls_out-prpos = ls_cmmt-rfpos.

        IF ls_cmmt-btart IN r_btart_o.  "original
          MOVE-CORRESPONDING ls_cmmt TO ls_pr.
          ls_pr-fkbtr = ls_cmmt-fkbtr.
          APPEND ls_pr TO gt_pr.       "merge later due to collect
        ENDIF.

      ENDIF.

*"purchase order
    ELSEIF ls_cmmt-refbt = '020'.

*---consider c/f only for PO... (no meaning PR...)
      IF ls_cmmt-btart = '0300'. "c/f
        ls_out-cffld = 'X'.
      ELSE.
        ls_out-cffld = ''.
      ENDIF.

*      ls_out-doctx = 'PO'. " later...

      ls_out-podoc = ls_cmmt-refbn.
      ls_out-popos = ls_cmmt-rfpos.
*      ls_out-poghr = ls_cmmt-gjahr.

      IF ls_cmmt-btart IN r_btart_o.  "original
        MOVE-CORRESPONDING ls_cmmt TO ls_po.
*        ls_po-vrfghr = ls_cmmt-gjahr.
        APPEND ls_po TO gt_po.       "merge later due to collect
      ENDIF.


      IF ls_cmmt-vrefbn = space. " PO w/o PR

      ELSE.   "find reference PR

        ls_out-prdoc = ls_cmmt-vrefbn.
        ls_out-prpos = ls_cmmt-vrfpos.
*        ls_out-prghr = ls_cmmt-gjahr.  "FIXME later

        CLEAR ls_pr.
        READ TABLE gt_cmmt INTO ls_cmmt_t
              WITH KEY refbt = '010'
                       refbn = ls_cmmt-vrefbn
                       rfpos = ls_cmmt-vrfpos
                       btart = '0100'     "original
                   BINARY SEARCH.
        IF sy-subrc <> 0.   "if failed, search c/f PR
          READ TABLE gt_cmmt INTO ls_cmmt_t
                WITH KEY refbt = '010'
                         refbn = ls_cmmt-vrefbn
                         rfpos = ls_cmmt-vrfpos
                         btart = '0350'     "c/f
                     BINARY SEARCH.
        ENDIF.
        IF sy-subrc <> 0.  "if not found, search table
          SELECT SINGLE refbn rfpos gjahr fkbtr INTO CORRESPONDING FIELDS OF ls_cmmt_t
            FROM  fmioi
           WHERE refbn  = ls_cmmt-vrefbn
             AND refbt  = '010'          "FIXME what about PR change???
*            AND RFORG  = ls_cmmt-VRFORG
             AND rfpos  = ls_cmmt-vrfpos
             AND btart  IN r_btart_o.                       " '0100'
        ENDIF.

*----    fill PR data
        IF sy-subrc = 0 AND ls_cmmt-btart IN r_btart_o.  "original
          READ TABLE gt_pr WITH KEY refbn = ls_cmmt-vrefbn
                                    rfpos = ls_cmmt-rfpos BINARY SEARCH
                                TRANSPORTING NO FIELDS.

          IF sy-subrc <> 0.
            ls_pr-refbn = ls_cmmt-vrefbn.
            ls_pr-rfpos = ls_cmmt-vrfpos.
*            ls_pr-gjahr = ls_cmmt_t-gjahr.
            ls_pr-fkbtr  = ls_cmmt_t-fkbtr.
            ls_pr-vrefbn = ls_cmmt-refbn.
            ls_pr-vrfpos = ls_cmmt-rfpos.
            APPEND ls_pr TO gt_pr.  "to skip PR processing
          ENDIF.
        ENDIF.


      ENDIF.

    ENDIF.

*now add to master table with accumulation of commitment total
    ls_out-blamt  = ls_cmmt-fkbtr * -1 .
    COLLECT ls_out INTO gt_out.

  ENDLOOP.


ENDFORM.                    " PROCESS_COMMITMENT
*&---------------------------------------------------------------------*
*&      Form  PROCESS_FIDOCS
*&---------------------------------------------------------------------*
FORM process_fidocs .
  DATA: ls_po TYPE gty_prpo,
        ls_pr TYPE gty_prpo.
  DATA: BEGIN OF ls_ekkn,
          brtwr LIKE ekpo-brtwr,
          aedat LIKE ekpo-aedat,
          banfn LIKE ekpo-banfn,
          bnfpo LIKE ekpo-bnfpo,
          hkont LIKE ekkn-sakto,
        END OF ls_ekkn.

  DATA : ls_out    TYPE ty_output.

  SORT gt_pr BY vrefbn vrfpos.

* park/invoice (no downpay ???  61)
  LOOP AT gt_fi.
    CLEAR : ls_out.
    MOVE-CORRESPONDING gt_fi TO ls_out.

*-- without PO reference
    IF gt_fi-vrefbn = space.
      IF p_dopen = 'X'. CONTINUE. ENDIF.     "open item only display
*---  performance issue for commitment, show in detail
*      IF gt_fi-objnrz IS NOT INITIAL.
*        ls_out-aufnr = gt_fi-objnrz+2(12).
*      ENDIF.

*-- with PO reference
    ELSE.

*---  check PO reading is done or not...
      READ TABLE gt_po INTO ls_po WITH KEY refbn = gt_fi-vrefbn
                                           rfpos = gt_fi-vrfpos
                                  BINARY SEARCH.
      IF sy-subrc <> 0.
        IF p_dopen = 'X'. CONTINUE. ENDIF.     "open item only display

        SELECT SINGLE brtwr ekpo~aedat banfn bnfpo sakto INTO ls_ekkn
           FROM ekpo
           INNER JOIN ekkn ON ekkn~ebeln = ekpo~ebeln AND ekkn~ebelp = ekpo~ebelp
           WHERE ekpo~ebeln = gt_fi-vrefbn
             AND ekpo~ebelp = gt_fi-vrfpos.

        IF sy-subrc = 0.
          ls_po-vrefbn = ls_ekkn-banfn.
          ls_po-vrfpos = ls_ekkn-bnfpo.
*          ls_po-vrfghr = ls_ekkn-aedat(4). "year

          ls_po-hkont  = ls_ekkn-bnfpo.

*          ls_po-gjahr  = ls_ekkn-aedat(4). "year
          ls_po-refbn = gt_fi-vrefbn.
          ls_po-rfpos = gt_fi-vrfpos.
          ls_po-fkbtr = - ls_po-fkbtr.  "reverse sign in FM.
          APPEND ls_po TO gt_po.
          SORT gt_po BY refbn rfpos.
        ELSE.
*          BREAK-POINT. "critical problem...
          " PO WAS ARCHIVED?????????????????
        ENDIF.
      ENDIF.

*      ls_out-poghr = ls_po-gjahr.
      ls_out-podoc = ls_po-refbn.
      ls_out-popos = ls_po-rfpos.

* check if PR exist
*      ls_out-prghr = ls_po-gjahr.
      ls_out-prdoc = ls_po-vrefbn.
      ls_out-prpos = ls_po-vrfpos.

* redetermine G/L account, since FM-HKONT is wrong
      ls_out-hkont = ls_po-hkont.
    ENDIF.

*-- need to summarize
    CLEAR: ls_out-refbt, ls_out-refbn, ls_out-rfpos.

    IF gt_fi-wrttp EQ '61'. "If downpayment
      ls_out-dpamt  = - gt_fi-fkbtr. "for budget checking in the system
      ls_out-dpcnt = 1.
    ELSEIF gt_fi-wrttp EQ '54'. "invoice
      ls_out-fkbtr  = - gt_fi-fkbtr. "for budget checking in the system
      ls_out-ivamt  = - gt_fi-fkbtr.   "invoice = normal iv + downpayment
      ls_out-ivcnt = 1.
    ELSE.                    "transfer posting
      ls_out-fkbtr  = - gt_fi-fkbtr. "for budget checking in the system
      ls_out-tfamt  = - gt_fi-fkbtr.   "invoice = normal iv + downpayment
      ls_out-tfcnt = 1.
    ENDIF.
    ls_out-blamt  = - gt_fi-fkbtr.   "for total balance of spending

*ISSUE: if AP vendor is different from PO vendor... FIXME

    COLLECT ls_out INTO gt_out.

    IF gt_fi-objnrz(2) = 'OR'.
      ls_out-aufnr = gt_fi-objnrz+2(12).
    ENDIF.
    IF gt_fi-wrttp EQ '61'. "Downpayment
      ls_out-dpghr  = gt_fi-kngjahr.
      ls_out-dpdoc  = gt_fi-refbn.
      APPEND ls_out TO gt_dnp.
    ELSEIF gt_fi-wrttp EQ '54'. "invoice
      ls_out-ivghr   = gt_fi-kngjahr.
      ls_out-ivdoc   = gt_fi-refbn.  "for BSAK select purpose
      ls_out-ivdocs  = gt_fi-refbn.
      APPEND ls_out TO gt_inv.
    ELSE.                   "transfer posting
      ls_out-tfghr  = gt_fi-kngjahr.
      ls_out-tfdoc  = gt_fi-refbn.
      APPEND ls_out TO gt_trf.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " PROCESS_FIDOCS
*&---------------------------------------------------------------------*
*&      Form  MERGE_TO_OUT
*&---------------------------------------------------------------------*
FORM merge_to_out .
  DATA : ls_out    TYPE ty_output,
         ls_inv    TYPE ty_output, ls_dnp TYPE ty_output, ls_trf  TYPE ty_output.

  DATA: ls_check  LIKE gt_cmmt.
  DATA: ls_check2 LIKE gt_cmmt.
  DATA: lt_tab_pr LIKE gt_cmmt OCCURS 0 WITH HEADER LINE.
  DATA: lt_tab_po LIKE gt_cmmt OCCURS 0 WITH HEADER LINE.

  DATA : lt_fi     LIKE gt_cmmt OCCURS 0 WITH HEADER LINE.
  DATA : l_cnt TYPE i.

  SORT gt_inv BY podoc popos prdoc prpos
                 fonds fistl fipex hkont aufnr lifnr.
  SORT gt_dnp BY podoc popos prdoc prpos
                 fonds fistl fipex hkont aufnr lifnr.
  SORT gt_trf BY podoc popos prdoc prpos
                 fonds fistl fipex hkont aufnr lifnr.

  LOOP AT gt_out INTO ls_out.
    CHECK ls_out-ivamt NE 0 OR ls_out-dpamt NE 0 OR ls_out-tfamt NE 0.
    $ix = sy-tabix.

    READ TABLE gt_inv INTO ls_inv WITH KEY
             podoc = ls_out-podoc
             popos = ls_out-popos
             prdoc = ls_out-prdoc
             prpos = ls_out-prpos
             fonds = ls_out-fonds
             fistl = ls_out-fistl
             fipex = ls_out-fipex
             hkont = ls_out-hkont
             lifnr = ls_out-lifnr BINARY SEARCH.
    READ TABLE gt_dnp INTO ls_dnp WITH KEY
             podoc = ls_out-podoc
             popos = ls_out-popos
             prdoc = ls_out-prdoc
             prpos = ls_out-prpos
             fonds = ls_out-fonds
             fistl = ls_out-fistl
             fipex = ls_out-fipex
             hkont = ls_out-hkont
             lifnr = ls_out-lifnr BINARY SEARCH.
    READ TABLE gt_trf INTO ls_trf WITH KEY
             podoc = ls_out-podoc
             popos = ls_out-popos
             prdoc = ls_out-prdoc
             prpos = ls_out-prpos
             fonds = ls_out-fonds
             fistl = ls_out-fistl
             fipex = ls_out-fipex
             hkont = ls_out-hkont
              BINARY SEARCH.

    IF ls_out-ivcnt = 1.
      MOVE ls_inv-ivdocs TO ls_out-ivdocs.
      MOVE ls_inv-ivghr TO ls_out-ivghr.
    ELSEIF ls_out-ivcnt > 1.
      CONCATENATE '*' ls_inv-ivdocs INTO ls_out-ivdocs.
      MOVE ls_inv-ivghr TO ls_out-ivghr.
    ENDIF.

    IF ls_out-dpcnt = 1.
      MOVE ls_dnp-dpdoc TO ls_out-dpdoc.
      MOVE ls_dnp-dpghr TO ls_out-dpghr.
    ELSEIF ls_out-dpcnt > 1.
      CONCATENATE '*' ls_inv-dpdoc INTO ls_out-dpdoc.
      MOVE ls_dnp-dpghr TO ls_out-dpghr.
    ENDIF.
    IF ls_out-tfcnt = 1.
      MOVE ls_trf-tfdoc TO ls_out-tfdoc.
      MOVE ls_trf-tfghr TO ls_out-tfghr.
    ELSEIF ls_out-tfcnt > 1.
      CONCATENATE '*' ls_trf-tfdoc INTO ls_out-tfdoc.
      MOVE ls_trf-tfghr TO ls_out-tfghr.
    ENDIF.

    MODIFY gt_out INDEX $ix FROM ls_out TRANSPORTING
        ivdocs ivghr dpdoc dpghr tfdoc tfghr.

  ENDLOOP.


ENDFORM.                    " MERGE_TO_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_GR
*&---------------------------------------------------------------------*
FORM get_data_gr .
  DATA : ls_out    TYPE ty_output,
         lt_gr     TYPE TABLE OF ty_gr,
         ls_gr     TYPE ty_gr,
         ls_pr     TYPE gty_prpo,
         ls_po     TYPE gty_prpo.

  DATA: lv_idx LIKE sy-tabix.

  SORT gt_pr BY refbn rfpos.

  LOOP AT gt_out INTO ls_out.
    lv_idx = sy-tabix.

* PO is used for IV, DP, other -> consider only IV, open commitment
    IF ls_out-podoc NE space AND
      ( ls_out-ivamt > 0 OR ls_out-fkbtr NE 0 ).
      REFRESH lt_gr.
      SELECT ekbe~ebeln ekbe~ebelp
             gjahr belnr buzei bwart budat shkzg
             ekbe~menge ekbe~wrbtr
             ekpo~netpr ekpo~peinh
        INTO TABLE lt_gr
        FROM ekbe
        INNER JOIN ekpo
           ON ekbe~ebeln = ekpo~ebeln
          AND ekbe~ebelp = ekpo~ebelp
       WHERE ekbe~ebeln = ls_out-podoc AND
             ekbe~ebelp = ls_out-popos AND
             ekbe~vgabe = '1'.

      ls_out-grcnt = sy-dbcnt.
      LOOP AT lt_gr INTO ls_gr.
        IF ls_gr-wrbtr = 0.  "if GR non valued...
          ls_gr-wrbtr = ls_gr-menge * ls_gr-netpr / ls_gr-peinh.
        ENDIF.
        IF ls_gr-shkzg = 'H'.
          ls_gr-wrbtr = - ls_gr-wrbtr.
          ls_gr-menge = - ls_gr-menge.
        ENDIF.
        APPEND ls_gr TO gt_gr.
        ls_out-gramt = ls_out-gramt + ls_gr-wrbtr.
      ENDLOOP.

* - save back to out
      IF ls_out-grcnt = 1.
        MOVE ls_gr-belnr  TO ls_out-grdoc.
      ELSEIF ls_out-grcnt > 1.
        CONCATENATE '*' ls_gr-belnr INTO ls_out-grdoc.
      ENDIF.
      MOVE ls_gr-gjahr TO ls_out-grghr.

      MODIFY gt_out INDEX lv_idx FROM ls_out TRANSPORTING
          grdoc grghr gramt.
    ENDIF.


* fill back PO amount
    IF ls_out-podoc NE space.
      READ TABLE gt_po INTO ls_po
                 WITH KEY refbn = ls_out-podoc
                          rfpos = ls_out-popos BINARY SEARCH.
      IF sy-subrc = 0.
        ls_out-poamt = - ls_po-fkbtr.
      ELSE.
        SELECT SINGLE brtwr AS poamt INTO ls_out-poamt
           FROM ekpo
           WHERE ebeln = ls_out-podoc
             AND ebelp = ls_out-popos.
      ENDIF.
      MODIFY gt_out INDEX lv_idx FROM ls_out TRANSPORTING poamt.
    ENDIF.

* fill back PR amount
    IF ls_out-prdoc NE space.
      READ TABLE gt_pr INTO ls_pr
                 WITH KEY refbn = ls_out-prdoc
                          rfpos = ls_out-prpos BINARY SEARCH.
      IF sy-subrc = 0.
        ls_out-pramt = - ls_pr-fkbtr.
      ELSE.
        SELECT SINGLE * FROM eban
           WHERE banfn = ls_out-prdoc
             AND bnfpo = ls_out-prpos.
        IF sy-subrc = 0.
          ls_out-poamt = eban-menge * eban-preis / eban-peinh.
        ENDIF.
      ENDIF.
      MODIFY gt_out INDEX lv_idx FROM ls_out TRANSPORTING pramt.
    ENDIF.


  ENDLOOP.

ENDFORM.                    " GET_DATA_GR
*&---------------------------------------------------------------------*
*&      Form  READ_PR_ACCOUNT
*&---------------------------------------------------------------------*
FORM read_pr_account USING fp_refbn fp_rfpos
                     tables lt_ebkn TYPE table of ty_ebkn.

  SELECT a~banfn a~bnfpo
         a~aufnr a~kostl a~sakto a~fistl a~geber a~fipos
         b~lifnr b~bedat
       INTO TABLE it_ebkn
       FROM  ebkn AS a
       INNER JOIN eban AS b
               ON a~banfn = b~banfn
              AND a~bnfpo = b~bnfpo
       WHERE   a~banfn =   fp_refbn AND
               a~bnfpo =   fp_rfpos .

*    SORT it_ebkn BY banfn bnfpo.

ENDFORM.                    " READ_PR_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  READ_PO_ACCOUNT
*&---------------------------------------------------------------------*
FORM read_po_account USING fp_refbn fp_rfpos
                     tables lt_ekkn TYPE table of ty_ekkn.

  SELECT a~ebeln a~ebelp
         a~erekz a~netpr a~peinh a~pstyp
         b~lifnr b~bedat
     INTO TABLE lt_ekkn
     FROM ekpo AS a
          INNER JOIN ekko AS b
             ON a~ebeln = b~ebeln
     WHERE a~ebeln = fp_refbn
       AND a~ebelp = fp_rfpos.

*    SORT it_ekpo BY ebeln ebelp.

ENDFORM.                    " READ_PO_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  FILL_PAY_INFO
*&---------------------------------------------------------------------*
FORM fill_pay_info .
  DATA : ls_out    TYPE ty_output,
         lv_idx    LIKE sy-tabix,
         lv_belnr  LIKE bsak-belnr.

  TYPES: BEGIN OF ts_bsak,
          belnr LIKE bsak-belnr,
          gjahr LIKE bsak-gjahr,
          augdt LIKE bsak-augdt,
          augbl LIKE bsak-augbl,
         END OF ts_bsak.
  DATA : lt_bsak TYPE TABLE OF ts_bsak,
         ls_bsak TYPE ts_bsak.

  IF gt_inv[] IS NOT INITIAL.
    SELECT belnr gjahr augdt augbl
      INTO TABLE lt_bsak
      FROM bsak
      FOR ALL ENTRIES IN gt_inv
     WHERE bukrs =  p_fikrs
       AND lifnr = gt_inv-lifnr
       AND gjahr = gt_inv-ivghr
       AND belnr = gt_inv-ivdoc.
  ENDIF.
  SORT lt_bsak BY belnr gjahr.

  LOOP AT gt_out INTO ls_out WHERE ivdocs NE space.
    lv_idx = sy-tabix.

    CLEAR : ls_bsak.

    IF ls_out-ivdocs+0(1) = '*'.
      lv_belnr = ls_out-ivdocs+1(10).
    ELSE.
      lv_belnr = ls_out-ivdocs.
    ENDIF.
    READ TABLE lt_bsak INTO ls_bsak
                       WITH KEY belnr = lv_belnr
                                gjahr = ls_out-ivghr BINARY SEARCH.
    IF sy-subrc = 0.
      ls_out-augdt = ls_bsak-augdt.
      MODIFY gt_out INDEX lv_idx FROM ls_out TRANSPORTING augdt .
    ENDIF.

  ENDLOOP.

ENDFORM.                    " FILL_PAY_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_MASTER
*&---------------------------------------------------------------------*
FORM get_master .

* commitmemt master
  SELECT fmfpo~fipos fmfpo~posit fmfpot~bezeich
     INTO CORRESPONDING FIELDS OF TABLE it_fmfpo
     FROM fmfpo
     INNER JOIN fmfpot
        ON fmfpo~fikrs  = fmfpot~fikrs
       AND fmfpo~fipos  = fmfpot~fipos
       AND fmfpo~datbis = fmfpot~datbis
     WHERE fmfpo~fikrs =  p_fikrs
       AND fmfpo~fipos IN s_fipex.

*       AND knzaepo IN p_knz.
  DATA: $ix LIKE sy-tabix.
  LOOP AT it_fmfpo.
    $ix = sy-tabix.
    PERFORM determine_profile_fs USING    p_fikrs
                                          ''
                                          it_fmfpo-posit
                                          ''
                                          p_gjahr
                                 CHANGING it_fmfpo-profil.

    IF it_fmfpo-profil NOT IN s_prof.
      DELETE it_fmfpo INDEX $ix.
    ENDIF.

  ENDLOOP.

  SORT it_fmfpo BY fipos.

ENDFORM.                    " GET_MASTER
