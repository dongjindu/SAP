REPORT zrfii18  MESSAGE-ID  zmfi.
*&--------------------------------------------------------------------
*& Author                 : Hs.Jeong
*& Creation Date          : 21/11/2003
*& Specification By       : Hs.Jeong
*& Pattern                : Report 1-1
*& Development Request No : UD1K904358
*& Addl documentation     :
*& Description  : list for print out slip
*&                This is developed use ALV.
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*& 12/4/03  ANDY
*&   - Consolidate IM/IO/FM
*&   - Performance Tuning
*&   - Add Display Option
*&   - Others
** 12/07/2011 KDM        UD1K953447    ECC Upgrade hierachy Logic Modi
*& 07/03/2013  T00303    UD1K957565    U1: Apply Archiving
*&--------------------------------------------------------------------
TYPE-POOLS: slis.

INCLUDE <icon>.
INCLUDE <symbol>.

CLASS cl_gui_resources DEFINITION LOAD.

CONSTANTS:
  c_f2code               LIKE sy-ucomm                    VALUE '&ETA'.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.


DATA: wa_repid LIKE sy-repid,
      wa_var_save(1) TYPE c             VALUE  'A',
      wa_default(1)  TYPE c,
      wa_exit(1) TYPE c,
      wa_variant LIKE disvariant,
      wa_var LIKE disvariant,
      wa_alv_function_name(30) TYPE c VALUE 'REUSE_ALV_GRID_LIST',
      wa_alv_get_info_name(40) TYPE c,
      wa_mode.

*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
TABLES : imak, impr, bpbk, aufk, usr21, bpep,
         ztfi_reason, bp_v_ej, bp_v_eg.
TABLES : t001,
         fmfincode, fmfint, "Fund
         fmci, fmcit,    "commitment
         fmfctr, fmfctrt,  "fund center
         fmhictr.          "fund center hier

*----------------------------------------------------------------------
RANGES: gr_posid  FOR impr-posid.

DATA : it_impr    LIKE impr  OCCURS 0 WITH HEADER LINE,
       it_imzo    LIKE imzo  OCCURS 0 WITH HEADER LINE,
       it_impu    LIKE impu  OCCURS 0 WITH HEADER LINE,
       it_coas    LIKE coas  OCCURS 0 WITH HEADER LINE,
       it_aufk    LIKE aufk  OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF ifmfctr OCCURS 0,
         fictr      LIKE fmfctr-fictr,
         bezeich    LIKE fmfctrt-bezeich,
         ctr_objnr  LIKE fmfctr-ctr_objnr,
       END OF ifmfctr.
*commitment
DATA : BEGIN OF ifmci OCCURS 0,
         posit    LIKE fmep-posit,
         bezei   LIKE fmcit-bezei ,
         fipos    LIKE fmci-fipos,
       END OF ifmci.

DATA: BEGIN OF it_bpej1 OCCURS 0,
        objnr    LIKE bpej-objnr,     " PI, IO, FundCenter
        trgkz    LIKE bpej-trgkz,
        vorga    LIKE bpej-vorga,
        belnr    LIKE bpej-belnr,
        gjahr    LIKE bpej-gjahr,      "year
        wtjhr    LIKE bpej-wtjhr,      "amount
        posit    LIKE bpej-posit,      "commitment
        geber    LIKE bpej-geber,      "fund
        sgtext   LIKE bpbk-sgtext,
        bldat    LIKE bpbk-bldat,      "Document Date
        cpudt    LIKE bpbk-cpudt,      "System Date
        usnam    LIKE bpbk-usnam,      "user
        fipos    LIKE fmci-fipos,     "commitment
      END OF it_bpej1.
DATA: BEGIN OF it_bpej_dum1 OCCURS 0,
        objnr    LIKE bpej-objnr,     " PI, IO, FundCenter
        trgkz    LIKE bpej-trgkz,
        vorga    LIKE bpej-vorga,
        belnr    LIKE bpej-belnr,
        gjahr    LIKE bpej-gjahr,      "year
        wtjhr    LIKE bpej-wtjhr,      "amount
        posit    LIKE bpej-posit,      "commitment
        geber    LIKE bpej-geber,      "fund
        sgtext   LIKE bpbk-sgtext,
        bldat    LIKE bpbk-bldat,      "Document Date
        cpudt    LIKE bpbk-cpudt,      "System Date
        usnam    LIKE bpbk-usnam,      "user
        fipos    LIKE fmci-fipos,     "commitment
      END OF it_bpej_dum1.

*----- Changed by JIPARK 2004/04/08
DATA: BEGIN OF it_bpej OCCURS 0,
        objnr    LIKE bpej-objnr,     " PI, IO, FundCenter
        trgkz    LIKE bpej-trgkz,
        vorga    LIKE bpej-vorga,
        belnr    LIKE bpej-belnr,
        gjahr    LIKE bpej-gjahr,      "year
        wtp01    LIKE bpep-wtp01,                           "P1.amount
        wtp02    LIKE bpep-wtp01,                           "P2.amount
        wtp03    LIKE bpep-wtp01,                           "P3.amount
        wtp04    LIKE bpep-wtp01,                           "P4.amount
        wtp05    LIKE bpep-wtp01,                           "P5.amount
        wtp06    LIKE bpep-wtp01,                           "P6.amount
        wtp07    LIKE bpep-wtp01,                           "P7.amount
        wtp08    LIKE bpep-wtp01,                           "P8.amount
        wtp09    LIKE bpep-wtp01,                           "P9.amount
        wtp10    LIKE bpep-wtp01,                           "P10.amount
        wtp11    LIKE bpep-wtp01,                           "P11.amount
        wtp12    LIKE bpep-wtp01,                           "P12.amount
        posit    LIKE bpej-posit,      "commitment
        geber    LIKE bpej-geber,      "fund
        sgtext   LIKE bpbk-sgtext,
        bldat    LIKE bpbk-bldat,      "Document Date
        cpudt    LIKE bpbk-cpudt,      "System Date
        usnam    LIKE bpbk-usnam,      "user
        fipos    LIKE fmci-fipos,     "commitment
      END OF it_bpej.

DATA: BEGIN OF it_bpej_dum OCCURS 0,
        objnr    LIKE bpej-objnr,     " PI, IO, FundCenter
        trgkz    LIKE bpej-trgkz,
        vorga    LIKE bpej-vorga,
        belnr    LIKE bpej-belnr,
        gjahr    LIKE bpej-gjahr,      "year
        wtp01    LIKE bpep-wtp01,                           "P1.amount
        wtp02    LIKE bpep-wtp01,                           "P2.amount
        wtp03    LIKE bpep-wtp01,                           "P3.amount
        wtp04    LIKE bpep-wtp01,                           "P4.amount
        wtp05    LIKE bpep-wtp01,                           "P5.amount
        wtp06    LIKE bpep-wtp01,                           "P6.amount
        wtp07    LIKE bpep-wtp01,                           "P7.amount
        wtp08    LIKE bpep-wtp01,                           "P8.amount
        wtp09    LIKE bpep-wtp01,                           "P9.amount
        wtp10    LIKE bpep-wtp01,                           "P10.amount
        wtp11    LIKE bpep-wtp01,                           "P11.amount
        wtp12    LIKE bpep-wtp01,                           "P12.amount
        posit    LIKE bpej-posit,      "commitment
        geber    LIKE bpej-geber,      "fund
        sgtext   LIKE bpbk-sgtext,
        bldat    LIKE bpbk-bldat,      "Document Date
        cpudt    LIKE bpbk-cpudt,      "System Date
        usnam    LIKE bpbk-usnam,      "user
        fipos    LIKE fmci-fipos,     "commitment
      END OF it_bpej_dum.
*-----

DATA: BEGIN OF it_bpeg OCCURS 0,
        objnr    LIKE bpeg-objnr,     " PI, IO, FundCenter
        trgkz    LIKE bpeg-trgkz,
        vorga    LIKE bpeg-vorga,
        belnr    LIKE bpeg-belnr,
        wtges    LIKE bpeg-wtges,      "overall amount
        posit    LIKE bpej-posit,      "commitment
        geber    LIKE bpeg-geber,      "fund
        sgtext   LIKE bpbk-sgtext,
        bldat    LIKE bpbk-bldat,      "Document Date
        cpudt    LIKE bpbk-cpudt,      "System Date
        usnam    LIKE bpbk-usnam,      "user
        fipos    LIKE fmci-fipos,     "commitment
      END OF it_bpeg.

DATA: BEGIN OF it_bpeg_dum OCCURS 0,
        objnr    LIKE bpeg-objnr,     " PI, IO, FundCenter
        trgkz    LIKE bpeg-trgkz,
        vorga    LIKE bpeg-vorga,
        belnr    LIKE bpeg-belnr,
        wtges    LIKE bpeg-wtges,      "overall amount
        posit    LIKE bpej-posit,      "commitment
        geber    LIKE bpeg-geber,      "fund
        sgtext   LIKE bpbk-sgtext,
        bldat    LIKE bpbk-bldat,      "Document Date
        cpudt    LIKE bpbk-cpudt,      "System Date
        usnam    LIKE bpbk-usnam,      "user
        fipos    LIKE fmci-fipos,     "commitment
      END OF it_bpeg_dum.

RANGES: r_objnr FOR impr-objnr,
        r_posnr FOR imzo-posnr.

DATA: it_reason LIKE ztfi_reason OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gt_out OCCURS 0,
        posid   LIKE   impr-posid,
        post1   LIKE   impu-post1,
        prnam   LIKE   impr-prnam,
        aufnr   LIKE   aufk-aufnr,
        fipos   LIKE fmci-fipos,
        ktext   LIKE   coas-ktext,
        kostl   LIKE   impr-kostl,
        act_txt(10),
        seq     LIKE   ztfi_imfm-seq,
        reson   LIKE   ztfi_reason-reson,
        descr   LIKE   ztfi_reason-descr,
        sgtext  LIKE   bpbk-sgtext,
        gjahr   LIKE   bp_v_ej-gjahr,
*-------Changed by JIPARK 2004/04/08
        wtjhr   LIKE   bp_v_ej-wtjhr,  "if overall amount
        wtp01   LIKE bpep-wtp01,                            "P1.amount
        wtp02   LIKE bpep-wtp01,                            "P2.amount
        wtp03   LIKE bpep-wtp01,                            "P3.amount
        wtp04   LIKE bpep-wtp01,                            "P4.amount
        wtp05   LIKE bpep-wtp01,                            "P5.amount
        wtp06   LIKE bpep-wtp01,                            "P6.amount
        wtp07   LIKE bpep-wtp01,                            "P7.amount
        wtp08   LIKE bpep-wtp01,                            "P8.amount
        wtp09   LIKE bpep-wtp01,                            "P9.amount
        wtp10   LIKE bpep-wtp01,                            "P10.amount
        wtp11   LIKE bpep-wtp01,                            "P11.amount
        wtp12   LIKE bpep-wtp01,                            "P12.amount
*-------
        wtges   LIKE  bpge-wtges,
        cpudt   LIKE  bpbk-cpudt,
        belnr   LIKE  bpbk-belnr,
        usnam   LIKE  bpbk-usnam,
        trgkz   LIKE  bpej-trgkz,
        vorga   LIKE  bpej-vorga,

        chkbox TYPE c,
        light   TYPE c,
        ap_gjahr   LIKE   bp_v_ej-gjahr,
        tabcolor     TYPE slis_t_specialcol_alv,
      END OF gt_out.
*------
DATA: BEGIN OF gt_temp OCCURS 0,
        chkbox TYPE c,
        light   TYPE c,
        tabcolor     TYPE slis_t_specialcol_alv,
      END OF gt_temp.
*===============================================================*
DATA : BEGIN OF it_imakt OCCURS 0.
        INCLUDE STRUCTURE imakt.
DATA : END OF it_imakt.
*=====WorK area
DATA : wa_cnt TYPE i,
       wa_d_cnt TYPE i,
       wa_objnr LIKE jest-objnr,
       wa_posnr LIKE imak-posnr,
       wa_gjahr LIKE imak-gjahr,
       wa_code(30),
       wa_code_txt(50),
       ok_code(4),
       wa_okcode(4),
       wa_selfield TYPE slis_selfield,
       wa_type LIKE ztfi_reason-type,
       wa_comm.
*- POSS.ENTRY
DATA : BEGIN OF it_value OCCURS 0,
        reson LIKE ztfi_reason-reson,
        descr LIKE ztfi_reason-descr,
       END OF it_value.

*====FOR BDC
DATA : it_bdc      LIKE bdcdata OCCURS 0 WITH HEADER LINE.

DATA:  it_messtab  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

RANGES : r_vorga FOR bpbk-vorga,
         r_wrttp FOR bpeg-wrttp.

*- U1 start
DATA: gt_aufk_a TYPE TABLE OF aufk WITH HEADER LINE.
*- U1 End

*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
*==============*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
PARAMETERS: p_bukrs LIKE bkpf-bukrs MEMORY ID buk.
*------
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_pi.
PARAMETERS : r_1  RADIOBUTTON GROUP r1.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:
  s_posid   FOR imak-posnr.   "FOR   impr-posid.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS:
  s_prnam   FOR   impr-prnam.

*parameters: p_gnjhr like impr-gjahr memory id gjr.
SELECT-OPTIONS:
            s_gnjhr FOR impr-gjahr MEMORY ID gjr.

*------
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_or.
PARAMETERS : r_2  RADIOBUTTON GROUP r1.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:
  s_aufnr  FOR   aufk-aufnr.
SELECTION-SCREEN END OF LINE.
*------
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_fm.
PARAMETERS : r_3  RADIOBUTTON GROUP r1.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS:
  s_fifctr  FOR   fmfctr-fictr.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS:
  s_fipos FOR fmci-fipos.   "default '600000' option GE,

SELECTION-SCREEN SKIP.
SELECT-OPTIONS:
  s_gjahr   FOR   bp_v_ej-gjahr,
  s_cpudt   FOR   bpbk-cpudt.

SELECTION-SCREEN END OF BLOCK b0.
*========================*
*SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-020.
SELECT-OPTIONS:
   s_kostl   FOR   impr-kostl,
   s_belnr   FOR   bpep-belnr,
   s_usnam   FOR   usr21-bname , "DEFAULT sy-uname,
   s_reson   FOR   ztfi_reason-reson MATCHCODE OBJECT zhfi_reason.
*  s_prnam   FOR   impr-prnam.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-040.
*--check
PARAMETERS : p_total AS CHECKBOX DEFAULT ' '.
PARAMETERS : p_chk1  AS CHECKBOX DEFAULT 'X',
             p_chk2  AS CHECKBOX DEFAULT 'X',
             p_chk3  AS CHECKBOX DEFAULT 'X',
             p_chk4  AS CHECKBOX DEFAULT 'X',
             p_chk5  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-030.
PARAMETERS : p_low  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
PARAMETERS :
  p_layout LIKE disvariant-variant.   "LAYOUT
SELECTION-SCREEN END OF BLOCK b3.
*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End
*----------------------------------------------------------------------
*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*
*----------------------------------------------------------------------
* Mr Jung. Please correct followings...
*AT SELECTION-SCREEN FOR p_gnjhr.
*  if r_1 = 'X' and p_gnjhr = space.
*    message error...
*  endif.
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f4_variant CHANGING p_layout.
*----------------------------------------------------------------------
*
* INITIALIZATION
*
*----------------------------------------------------------------------
*
INITIALIZATION.

  wa_repid = sy-repid.
* ==> Change Variant saving type
*                         U-???, X-??(??), A-??, ' '-????
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
  wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
* wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.

  c_pi = 'Position ID'.
  c_or = 'Internal Order'.
  c_fm = 'Fund Mgt'.
**--SYSTEM DATE
*  CONCATENATE sy-datum+0(6) '01' INTO s_cpudt-low.
*  s_cpudt-high = sy-datum.
*  APPEND s_cpudt.
*
*---------------------------------------------------------------------
*    M   A   I   N
*
*---------------------------------------------------------------------
START-OF-SELECTION.
  SELECT SINGLE * FROM t001
    WHERE bukrs = p_bukrs.

  CHECK sy-subrc = 0.

END-OF-SELECTION.

  PERFORM build_layout.

* ==> 6. build sorts info
*  REFRESH gt_sorts.
*  PERFORM build_sort_table
*    USING :
*       '1'    'VTWEG'   'X'   'X'   '*'.
* ==> 1. select data from db

  PERFORM select_data.

  PERFORM fill_gt_out.

  IF gt_out[] IS INITIAL.
    MESSAGE s000(zmfi) WITH 'No Data'.
    EXIT.
  ENDIF.
* ==> 2. set variant default
  PERFORM set_variant CHANGING wa_var.
* ==> 3. set layout for alv style
  PERFORM set_layout CHANGING gs_layout.
* ==> 4. set events for alv
  PERFORM set_events CHANGING gt_events.
* ==> 7. call function display alv.
  PERFORM display_alv.

***********************************************************************

*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_LAYOUT  text
*----------------------------------------------------------------------*
FORM f4_variant CHANGING c_variant TYPE disvariant-variant.
  DATA: ls_variant TYPE disvariant,
        l_exit     TYPE char1.


  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant          = ls_variant
      i_save              = 'A'
*     it_default_fieldcat =
    IMPORTING
      e_exit              = l_exit
      es_variant          = ls_variant
    EXCEPTIONS
      not_found           = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF l_exit EQ space.
      c_variant = ls_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " f4_variant
*&---------------------------------------------------------------------*
*&      Form  build_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0218   text
*      -->P_0219   text
*      -->P_0220   text
*      -->P_0221   text
*      -->P_0222   text
*      -->P_0223   text
*      -->P_0224   text
*      -->P_0225   text
*      -->P_0226   text
*----------------------------------------------------------------------*
FORM build_field_category USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_edit            "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l = p_title.
  ls_fieldcat-outputlen = p_outputlen.
  ls_fieldcat-key       = p_key.
  ls_fieldcat-just      = p_just.
  ls_fieldcat-edit      = p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.
  IF p_fieldname = 'RESON'.
    ls_fieldcat-emphasize = 'C300'.
  ENDIF.
  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " build_field_category
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  REFRESH :  gt_out, it_impr, it_aufk, it_bpej, it_bpeg.
  CLEAR   :  gt_out, it_impr, it_aufk, it_bpej, it_bpeg.


  SELECT * INTO TABLE it_reason FROM ztfi_reason.

  PERFORM select_objnr.

  CLEAR : wa_cnt.
  DESCRIBE TABLE r_objnr LINES wa_cnt.
  IF wa_cnt > 0.
    PERFORM set_value_type.

    PERFORM select_lineitem_value.

    PERFORM select_master_data.
  ELSE.
    EXIT.
  ENDIF.
ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  set_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_VAR  text
*----------------------------------------------------------------------*
FORM set_variant CHANGING cs_vari TYPE disvariant.

  CHECK p_layout NE space.

  cs_vari-report      = sy-repid.
  cs_vari-handle      = space.
  cs_vari-log_group   = space.
  cs_vari-username    = space.
  cs_vari-variant     = p_layout.
  cs_vari-text        = space.
  cs_vari-dependvars  = space.

ENDFORM.                    " set_variant
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM set_layout CHANGING cs_layo TYPE slis_layout_alv.

*... Display options
  cs_layo-colwidth_optimize      = space. "'X'.
  "?????
  cs_layo-no_colhead             = space.
  cs_layo-no_hotspot             = space.
  cs_layo-zebra                  = ' '.
  cs_layo-no_vline               = space.
  cs_layo-cell_merge             = space.
  cs_layo-no_min_linesize        = space.
  cs_layo-min_linesize           = space.
  cs_layo-max_linesize           = space.
  cs_layo-window_titlebar        = space.
  cs_layo-no_uline_hs            = space.
*... Edit
  cs_layo-edit                   = ' '."space.
  cs_layo-edit_mode              = ' '."space.
*... Exceptions
  cs_layo-lights_fieldname       = ' '. "LIGHT'.
  "=> ??? ??? ???
  cs_layo-lights_tabname         = space.
  cs_layo-lights_rollname        = space.
  cs_layo-lights_condense        = space.
*... Sums
  cs_layo-no_sumchoice           = space.
  cs_layo-no_totalline           = space.
  cs_layo-totals_before_items    = space.
  cs_layo-totals_only            = space.
  cs_layo-totals_text            = space.
  cs_layo-no_subchoice           = space.
  cs_layo-no_subtotals           = space.
  cs_layo-subtotals_text         = space.
  cs_layo-numc_sum               = 'X'.
  cs_layo-no_unit_splitting      = space.
*... Interaction
  cs_layo-box_fieldname          = 'CHKBOX'.
  cs_layo-box_tabname            = space.
  cs_layo-box_rollname           = space.
  cs_layo-expand_fieldname       = space.
  cs_layo-hotspot_fieldname      = space.
  cs_layo-no_input               = ' '.
  cs_layo-f2code                 = space.
  cs_layo-confirmation_prompt    = space.
  cs_layo-key_hotspot            = space.
  cs_layo-flexible_key           = space.
  cs_layo-reprep                 = space.
  cs_layo-group_buttons          = 'X'.
  cs_layo-no_keyfix              = space.
  cs_layo-get_selinfos           = space.
  cs_layo-group_change_edit      = 'X'.
  cs_layo-no_scrolling           = space.
  cs_layo-expand_all             = space.
  cs_layo-no_author              = space.
*... Detailed screen
  cs_layo-detail_popup           = 'X'.
  cs_layo-detail_initial_lines   = space.
  cs_layo-detail_titlebar        = space.
*... PF-status
  cs_layo-def_status             = space.
*... Display variants
  cs_layo-header_text            = space.
  cs_layo-item_text              = space.
  cs_layo-default_item           = space.
*... colour
  cs_layo-info_fieldname         = space.
  cs_layo-coltab_fieldname       = 'TABCOLOR'.
*... others
  cs_layo-list_append            = space.


ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  set_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM set_events CHANGING ct_events TYPE slis_t_event.

  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  DATA: l_event TYPE lvc_fname.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = ct_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    REFRESH ct_events.

*    DELETE ct_events WHERE name NE 'END_OF_PAGE'
*                       AND name NE 'TOP_OF_PAGE'
*                       AND name NE 'TOP_OF_LIST'
*                       AND name NE 'END_OF_LIST'.
*
*    loop at ct_events assigning <ls_event>.
*      concatenate 'ALV_EVENT_'
*                  <ls_event>-name
*                  into <ls_event>-form.
*    endloop.
  ENDIF.

ENDFORM.                    " set_events

*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM alv_event_pf_status_set USING rt_extab TYPE slis_t_extab.
                                                            "#EC *
  IF wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    SET PF-STATUS 'STANDARD_GRID' EXCLUDING rt_extab.
  ELSE.
    SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
  ENDIF.
  SET TITLEBAR  'STANDARD'.


ENDFORM.                    "alv_event_pf_status_set

*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
FORM alv_event_user_command USING r_ucomm     LIKE sy-ucomm
                                  rs_selfield TYPE slis_selfield.
                                                            "#EC *


  CASE r_ucomm.
*   ---------------------------------- processing on double click.
    WHEN '&IC1'.
      READ TABLE gt_out INDEX rs_selfield-tabindex.
      wa_selfield-tabindex = rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'RESON'.
          IF r_1 = 'X'.
            wa_code = gt_out-posid.
            wa_code_txt = gt_out-post1.
          ENDIF.
          CALL SCREEN 9000   STARTING AT 32 18  ENDING AT 100 26.
          rs_selfield-refresh = 'X'.
        WHEN 'POSID'.
          SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
          SET PARAMETER ID 'IMP' FIELD gt_out-posid.
*FIXME
          SET PARAMETER ID 'GJR' FIELD gt_out-gjahr.
          CALL TRANSACTION 'IM13' AND SKIP FIRST SCREEN.

        WHEN 'AUFNR'.
          SET PARAMETER ID 'ANR' FIELD gt_out-aufnr.
          CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.

        WHEN 'FIPOS'.   "Commitment
          SET PARAMETER ID 'FIK' FIELD 'H201'.
          SET PARAMETER ID 'FIP' FIELD  gt_out-fipos.
          CALL TRANSACTION 'FM3S' AND SKIP FIRST SCREEN.

        WHEN 'WTJHR'. " OR 'BELNR'.
          IF r_1 = 'X'.
            IF gt_out-act_txt = 'Original'.
              SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
              SET PARAMETER ID 'IMP' FIELD gt_out-posid.
              SET PARAMETER ID 'GJR' FIELD s_gnjhr-low.
              CALL TRANSACTION 'IM33' AND SKIP FIRST SCREEN.
            ELSEIF gt_out-act_txt = 'Supplement'.
              SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
              SET PARAMETER ID 'IMP' FIELD gt_out-posid.
              SET PARAMETER ID 'GJR' FIELD s_gnjhr-low.
              CALL TRANSACTION 'IM31' AND SKIP FIRST SCREEN.
            ELSEIF gt_out-act_txt = 'Return'.
              SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
              SET PARAMETER ID 'IMP' FIELD gt_out-posid.
              SET PARAMETER ID 'GJR' FIELD s_gnjhr-low.
              CALL TRANSACTION 'IM39' AND SKIP FIRST SCREEN.
            ENDIF.
          ELSEIF r_2 = 'X'.
            IF gt_out-act_txt = 'Original'.
              SET PARAMETER ID 'CAC' FIELD 'H201'.
              SET PARAMETER ID 'ANR' FIELD gt_out-aufnr.
              CALL TRANSACTION 'KO23' AND SKIP FIRST SCREEN.
            ELSEIF gt_out-act_txt = 'Supplement'.
              SET PARAMETER ID 'CAC' FIELD 'H201'.
              SET PARAMETER ID 'ANR' FIELD gt_out-aufnr.
              CALL TRANSACTION 'KO25' AND SKIP FIRST SCREEN.
            ELSEIF gt_out-act_txt = 'Return'.
              SET PARAMETER ID 'CAC' FIELD 'H201'.
              SET PARAMETER ID 'ANR' FIELD gt_out-aufnr.
              CALL TRANSACTION 'KO27' AND SKIP FIRST SCREEN.
            ENDIF.
          ELSE.    "FUND
            SET PARAMETER ID 'BPB' FIELD gt_out-belnr.
            CALL TRANSACTION 'FM2F' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'BELNR'.
          IF r_1 = 'X'.
          ELSEIF r_2 = 'X'.
            SET PARAMETER ID 'BPB' FIELD gt_out-belnr.
            CALL TRANSACTION 'KO2B' AND SKIP FIRST SCREEN.
          ELSEIF r_3 = 'X'.
            SET PARAMETER ID 'BPB' FIELD gt_out-belnr.
            CALL TRANSACTION 'FM2F' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'GJAHR'.
*          if r_1 = 'X'.
*            wa_gjahr = gt_out-gjahr.
*            wa_posnr = gt_out-posid.
*            export wa_gjahr    wa_posnr to memory.
*            submit zrfii09    and return.
*            exit.
*          endif.
      ENDCASE.
*---------------------------------- switching view type grid or list
    WHEN 'LIST' OR 'GRID'.
      PERFORM switch_list_or_grid USING r_ucomm.

    WHEN '&REC'.
      IF r_1 = 'X'.
        PERFORM im_budget_reconcile  USING rs_selfield.
      ENDIF.
  ENDCASE.

  CHECK r_ucomm EQ 'LIST' OR
        r_ucomm EQ 'GRID'.

  rs_selfield-exit = 'X'.

ENDFORM.                    "alv_event_user_command
*&---------------------------------------------------------------------*
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_UCOMM  text
*----------------------------------------------------------------------*
FORM switch_list_or_grid USING r_ucomm.

  DATA: ls_vari      TYPE disvariant,
       ls_slis_layo TYPE slis_layout_alv,
       lt_slis_fcat TYPE slis_t_fieldcat_alv,
       lt_slis_sort TYPE slis_t_sortinfo_alv,
       lt_slis_filt TYPE slis_t_filter_alv,
       ls_slis_prnt TYPE slis_print_alv.


  IF r_ucomm = 'LIST' AND
     wa_alv_function_name = 'REUSE_ALV_LIST_DISPLY'.
    EXIT.
  ENDIF.
  IF r_ucomm = 'GRID' AND
     wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    EXIT.
  ENDIF.
  CASE wa_alv_function_name.
    WHEN 'REUSE_ALV_LIST_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
    WHEN 'REUSE_ALV_GRID_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.

  ENDCASE.

  CALL FUNCTION wa_alv_get_info_name
    IMPORTING
      es_layout     = ls_slis_layo
      et_fieldcat   = lt_slis_fcat
      et_sort       = lt_slis_sort
      et_filter     = lt_slis_filt
      es_variant    = ls_vari
    EXCEPTIONS
      no_infos      = 1
      program_error = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF r_ucomm = 'LIST'.
    wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
    CALL FUNCTION wa_alv_function_name
      EXPORTING
        i_callback_program       = wa_repid
        i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
        i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
        is_layout                = ls_slis_layo
        it_fieldcat              = lt_slis_fcat
        it_sort                  = lt_slis_sort
        it_filter                = lt_slis_filt
        i_default                = ' '  "gs_test-vari_default
        i_save                   = wa_var_save
        is_variant               = ls_vari
        is_print                 = ls_slis_prnt
        it_events                = gt_events[]
      TABLES
        t_outtab                 = gt_out
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
  ENDIF.
  IF r_ucomm = 'GRID'.
    wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    CALL FUNCTION wa_alv_function_name
      EXPORTING
        i_callback_program       = wa_repid
        i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
        i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
        is_layout                = ls_slis_layo
        it_fieldcat              = lt_slis_fcat
        it_sort                  = lt_slis_sort
        it_filter                = lt_slis_filt
        i_default                = ' '  "gs_test-vari_default
        i_save                   = wa_var_save
        is_variant               = ls_vari
        is_print                 = ls_slis_prnt
        it_events                = gt_events[]
      TABLES
        t_outtab                 = gt_out
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " switch_list_or_grid
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv.
  CALL FUNCTION wa_alv_function_name
    EXPORTING
      i_callback_program       = wa_repid
      i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
      i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
      it_special_groups        = gt_sp_group[]
      it_sort                  = gt_sorts[]
*     IT_FILTER                =
      i_default                = wa_default
      i_save                   = wa_var_save
      is_variant               = wa_var
      it_events                = gt_events[]
      is_print                 = gs_prnt
*     IT_EVENT_EXIT            =
*     I_SCREEN_START_COLUMN    = 10
*     I_SCREEN_START_LINE      = 2
*     I_SCREEN_END_COLUMN      = 80
*     I_SCREEN_END_LINE        = 23
    TABLES
      t_outtab                 = gt_out.

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  select_objnr
*&---------------------------------------------------------------------*
*       select PI, Order, ...
*----------------------------------------------------------------------*
FORM select_objnr.
* select position or order number

  r_objnr-option   = 'EQ'.  r_objnr-sign     = 'I'.

  PERFORM convert_s_posid.

* position id ................................
  IF r_1 = 'X'.
    IF p_low = 'X'.  "low level pi
      SELECT * FROM impr
        WHERE  gjahr IN s_gnjhr
          AND  posid IN gr_posid  "s_posid
          AND  prnam IN s_prnam
          AND  kostl IN s_kostl.
* Andy Added
* check if PI is parent node...
        CHECK impr-posid+7(4) <> 0000.
        APPEND impr TO it_impr.
      ENDSELECT.
    ELSE.
      SELECT * INTO TABLE it_impr FROM impr
        WHERE  gjahr IN s_gnjhr
          AND  posid IN gr_posid  "s_posid
          AND  prnam IN s_prnam
          AND  kostl IN s_kostl.
    ENDIF.

* fill object ranges
    LOOP AT it_impr.
      r_objnr-low = it_impr-objnr.
      APPEND r_objnr.
    ENDLOOP.
    CLEAR : wa_cnt.
    DESCRIBE TABLE r_objnr LINES wa_cnt.
    IF wa_cnt < 1.
      EXIT.
    ENDIF.
*   PI text
    SELECT * INTO TABLE it_impu FROM impu
      FOR ALL ENTRIES IN it_impr
      WHERE posnr = it_impr-posnr.

* internal order.............................
  ELSEIF r_2 = 'X'.
*=======2004/03/20.
    IF s_prnam-low <> ' '.
      SELECT * INTO TABLE it_impr FROM impr
        WHERE  gjahr IN s_gnjhr
        AND    prnam IN s_prnam
        AND    kostl IN s_kostl.
      LOOP AT it_impr.
        r_posnr-sign = 'I'.
        r_posnr-option = 'EQ'.
        r_posnr-low = it_impr-posnr.
        APPEND r_posnr.
      ENDLOOP.
      CLEAR wa_cnt.
      DESCRIBE TABLE r_posnr LINES wa_cnt.
      IF wa_cnt > 0.
        SELECT * INTO TABLE it_imzo FROM imzo
        WHERE posnr IN r_posnr.
      ENDIF.
      LOOP AT it_imzo.
        r_objnr-sign = 'I'.
        r_objnr-option = 'EQ'.
        r_objnr-low = it_imzo-objnr.
        APPEND r_objnr.
      ENDLOOP.

      SELECT * INTO TABLE it_aufk FROM aufk
      WHERE  objnr IN r_objnr
       AND  aufnr IN s_aufnr
       AND  kostv IN s_kostl.
*- U1 Start
      IF p_arch EQ 'X'.
        PERFORM archive_read_aufk USING s_prnam-low.
      ENDIF.
*- U1 End
      REFRESH : r_objnr.
      CLEAR   : r_objnr.
    ELSE.
      SELECT * INTO TABLE it_aufk FROM aufk
       WHERE  aufnr IN s_aufnr
         AND  kostv IN s_kostl.
*- U1 Start
      IF p_arch EQ 'X'.
        PERFORM archive_read_aufk USING s_prnam-low.
      ENDIF.
*- U1 End
    ENDIF.
*======================================*
    r_objnr-option   = 'EQ'.  r_objnr-sign     = 'I'.
* fill object ranges
    LOOP AT it_aufk.
      r_objnr-low = it_aufk-objnr.
      APPEND r_objnr.
    ENDLOOP.
    CLEAR : wa_cnt.
    DESCRIBE TABLE r_objnr LINES wa_cnt.
    IF wa_cnt < 1.
      EXIT.
    ENDIF.
*   OR text
    CLEAR wa_cnt.
    DESCRIBE TABLE it_aufk LINES wa_cnt.
    IF wa_cnt > 0.
      SELECT * INTO TABLE it_coas FROM coas
        FOR ALL ENTRIES IN it_aufk
        WHERE aufnr = it_aufk-aufnr.
    ENDIF.
* Fund Management
******************************************************
  ELSE.
** --> Block old logic(12/07/2011) UD1K953447
** fund center
*    SELECT l~fictr l~ctr_objnr
*       INTO CORRESPONDING FIELDS OF TABLE ifmfctr
*       FROM fmfctr AS l INNER JOIN fmhictr AS h
*         ON l~ctr_objnr = h~ctr_objnr
*       WHERE l~fikrs   =  t001-fikrs
*         AND l~fictr IN s_fifctr
*         AND h~parent_obj <> space.

    SELECT fictr ctr_objnr
       INTO CORRESPONDING FIELDS OF TABLE ifmfctr
       FROM fmfctr
       WHERE fikrs   =  t001-fikrs
         AND fictr IN s_fifctr.

    DELETE ifmfctr WHERE fictr = 'HMMA'.
** --> Block old logic(12/07/2011) UD1K953447

    LOOP AT ifmfctr.
      SELECT SINGLE bezeich  INTO ifmfctr-bezeich
         FROM fmfctrt
         WHERE spras = sy-langu
           AND fikrs = t001-fikrs
           AND fictr = ifmfctr-fictr.
      MODIFY ifmfctr.
    ENDLOOP.

* fill object ranges
    LOOP AT ifmfctr.
      r_objnr-low = ifmfctr-ctr_objnr.
      APPEND r_objnr.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " select_objnr
*&---------------------------------------------------------------------*
*&      Form  select_lineitem_value
*&---------------------------------------------------------------------*
FORM select_lineitem_value.
* object number: IM = IPxxxx, IO = ORxxxx, FM = FSxxxx

* IM/IO Yearly Display or FM Budget
* IF p_total = space OR r_3 = 'X'.  "jhs 2004/04/09
* select yearly amount

*-----Changed by JIPARK 2004/04/08
  IF p_total = ' ' AND r_3 = ' '.
    SELECT l~objnr l~trgkz l~vorga l~belnr l~gjahr
           l~wtjhr l~posit l~geber h~sgtext h~bldat h~cpudt h~usnam
      INTO TABLE it_bpej1
      FROM bpej AS l INNER JOIN bpbk AS h
        ON l~belnr  = h~belnr
      WHERE lednr = '0001'
        AND l~objnr IN r_objnr
        AND l~wrttp IN r_wrttp
        AND l~vorga IN r_vorga
        AND h~cpudt IN s_cpudt
*        AND h~belnr IN s_belnr
        AND h~usnam IN s_usnam
        AND l~gjahr IN s_gjahr.
    REFRESH : it_bpej_dum1.
    CLEAR   : it_bpej_dum1.

    LOOP AT it_bpej1.
      CHECK it_bpej1-objnr IN r_objnr.
      MOVE-CORRESPONDING it_bpej1 TO it_bpej_dum1.
      APPEND it_bpej_dum1.
      CLEAR  it_bpej_dum1.
    ENDLOOP.

    REFRESH : it_bpej1.
    CLEAR   : it_bpej1.

    LOOP AT it_bpej_dum1.
      MOVE-CORRESPONDING it_bpej_dum1 TO it_bpej1.
      APPEND it_bpej1.
      CLEAR  it_bpej1.
    ENDLOOP.

* ==> select periodic amount of fund management
  ELSEIF  r_3 = 'X'.
    SELECT l~objnr l~trgkz l~vorga l~belnr l~gjahr l~posit l~geber
           l~wtp01 l~wtp02 l~wtp03 l~wtp04 l~wtp05 l~wtp06
           l~wtp07 l~wtp08 l~wtp09 l~wtp10 l~wtp11 l~wtp12
           h~sgtext h~bldat h~cpudt h~usnam
     INTO CORRESPONDING FIELDS OF TABLE it_bpej
     FROM bpep AS l INNER JOIN bpbk AS h
       ON l~belnr  = h~belnr
     WHERE lednr = '0001'
       AND l~objnr IN r_objnr
       AND l~wrttp IN r_wrttp
       AND l~vorga IN r_vorga
       AND h~cpudt IN s_cpudt
*      AND h~belnr IN s_belnr
       AND h~usnam IN s_usnam
       AND l~gjahr IN s_gjahr
       AND l~belnr IN s_belnr.
*       and l~posit in s_fipos.
*------

*-------1209
    REFRESH : it_bpej_dum.
    CLEAR   : it_bpej_dum.

    LOOP AT it_bpej.
      CHECK it_bpej-objnr IN r_objnr.
      MOVE-CORRESPONDING it_bpej TO it_bpej_dum.
      APPEND it_bpej_dum.
      CLEAR  it_bpej_dum.
    ENDLOOP.

    REFRESH : it_bpej.
    CLEAR   : it_bpej.

    LOOP AT it_bpej_dum.
      MOVE-CORRESPONDING it_bpej_dum TO it_bpej.
      APPEND it_bpej.
      CLEAR  it_bpej.
    ENDLOOP.

  ELSE.
* select overall amount
    SELECT l~objnr l~trgkz l~vorga l~belnr
           l~wtges l~posit l~geber h~sgtext h~bldat h~cpudt h~usnam
      INTO TABLE it_bpeg
      FROM bpeg AS l INNER JOIN bpbk AS h
        ON l~belnr  = h~belnr
      WHERE lednr = '0001'
*        AND l~objnr IN r_objnr
        AND l~wrttp IN r_wrttp
        AND l~vorga IN r_vorga
        AND h~cpudt IN s_cpudt
*        AND h~belnr IN s_belnr
        AND h~usnam IN s_usnam.
*---1209
    REFRESH : it_bpeg_dum.
    CLEAR   : it_bpeg_dum.

    LOOP AT it_bpeg.
      CHECK it_bpeg-objnr IN r_objnr.
      MOVE-CORRESPONDING it_bpeg TO it_bpeg_dum.
      APPEND it_bpeg_dum.
      CLEAR  it_bpeg_dum.
    ENDLOOP.

    REFRESH : it_bpeg.
    CLEAR   : it_bpeg.

    LOOP AT it_bpeg_dum.
      MOVE-CORRESPONDING it_bpeg_dum TO it_bpeg.
      APPEND it_bpeg.
      CLEAR  it_bpeg.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " select_lineitem_value
*&---------------------------------------------------------------------*
*&      Form  fill_gt_out
*&---------------------------------------------------------------------*
*       Fill display structure
*----------------------------------------------------------------------*
FORM fill_gt_out.
  SORT it_impr BY objnr.
* IM/IO : Overall
  IF p_total = 'X' AND ( r_1 = 'X' OR r_2 = 'X' ).
    LOOP AT it_bpeg.
      MOVE-CORRESPONDING it_bpeg TO gt_out.
      PERFORM get_obj_info USING it_bpeg-objnr  it_bpeg-posit.
      PERFORM split_sgtext USING it_bpeg-trgkz it_bpeg-vorga  it_bpeg-sgtext.

      gt_out-gjahr = '0000'.
      gt_out-wtjhr = it_bpeg-wtges.
      CHECK gt_out-reson IN s_reson.
      CHECK gt_out-usnam IN s_usnam.
      APPEND gt_out.
    ENDLOOP.
*---2004/04/09
  ELSEIF p_total = ' ' AND r_3 = ' '.
    LOOP AT it_bpej1.
      MOVE-CORRESPONDING it_bpej1 TO gt_out.
      PERFORM get_obj_info USING it_bpej1-objnr  it_bpej1-posit.
      PERFORM split_sgtext USING it_bpej1-trgkz  it_bpej1-vorga  it_bpej1-sgtext.
      CHECK gt_out-reson IN s_reson.
      CHECK gt_out-usnam IN s_usnam.
      APPEND gt_out.

    ENDLOOP.

* FM/IM/IO Yearly
  ELSEIF r_3 = 'X'.
    LOOP AT it_bpej.
      MOVE-CORRESPONDING it_bpej TO gt_out.
      PERFORM get_obj_info USING it_bpej-objnr  it_bpej-posit.
*      CHECK wa_comm EQ 'Q'.
      PERFORM split_sgtext USING it_bpej-trgkz it_bpej-vorga  it_bpej-sgtext.
      CHECK gt_out-reson IN s_reson.
      CHECK gt_out-usnam IN s_usnam.
      APPEND gt_out.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " fill_gt_out

*&---------------------------------------------------------------------*
*&      Form  split_sgtext
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VORGA  text
*      -->SGTEXT text
*----------------------------------------------------------------------*
FORM split_sgtext USING f_trgkz f_vorga f_sgtext.

* VORGA: kbud - origin, kbn0 - supp, kbr0 - return,
*        kbus - transfer(sender)
*        kbue - transfer(receiver)
*        kbfr - release

  CASE f_vorga.
    WHEN 'KBUD'.
      IF f_trgkz = 'B'.
        gt_out-act_txt = 'C/F'.
      ELSE.
        gt_out-act_txt = 'Original'.
      ENDIF.
    WHEN 'KBN0'.
      gt_out-act_txt = 'Supplement'.
    WHEN 'KBR0'.
      gt_out-act_txt = 'Return'.
    WHEN 'KBUS'.
      gt_out-act_txt = 'Trf-Sender'.
    WHEN 'KBUE'.
      gt_out-act_txt = 'Trf-Receiv'.
    WHEN 'KBFR'.
      gt_out-act_txt = 'Release'.
      CLEAR:  gt_out-reson, gt_out-sgtext.
    WHEN OTHERS.
      gt_out-act_txt = 'Others'.
      gt_out-sgtext  = f_sgtext(40).
  ENDCASE.

  IF f_vorga = 'KBUD'
  OR f_vorga = 'KBN0'
  OR f_vorga = 'KBR0'.
    READ TABLE it_reason WITH KEY reson = f_sgtext+1(2).
    IF sy-subrc = 0.
      gt_out-reson   = f_sgtext+1(2).
      gt_out-descr   = it_reason-descr.
      gt_out-sgtext  = f_sgtext+3(40).
    ELSE.
      READ TABLE it_reason WITH KEY reson = f_sgtext+5(2).
      IF sy-subrc = 0.
        gt_out-seq     = f_sgtext+0(4).
        gt_out-reson   = f_sgtext+5(2).
        gt_out-descr   = it_reason-descr.
        gt_out-sgtext  = f_sgtext+7(40).
      ELSE.
        gt_out-reson   = '..'.
        gt_out-descr   = '...'.
        gt_out-sgtext  = f_sgtext.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " split_sgtext
*&---------------------------------------------------------------------*
*&      Form  set_value_type
*&---------------------------------------------------------------------*
FORM set_value_type.
  REFRESH : r_vorga. CLEAR   : r_vorga.

  r_vorga-sign   = 'I'.  r_vorga-option = 'EQ'.

*--Original
  IF p_chk1 = 'X'.
    r_vorga-low = 'KBUD'. APPEND r_vorga.
  ENDIF.

*---supplement
  IF p_chk2 = 'X'.
    r_vorga-low = 'KBN0'. APPEND r_vorga.
  ENDIF.

*---Return
  IF p_chk3 = 'X'.
    r_vorga-low = 'KBR0'. APPEND r_vorga.
  ENDIF.

*--transfer
  IF p_chk4 = 'X'.
    r_vorga-low = 'KBUS'. APPEND r_vorga.
    r_vorga-low = 'KBUE'. APPEND r_vorga.
  ENDIF.

**--
  IF p_chk5 = 'X'.
    r_vorga-low = 'KBFR'. APPEND r_vorga.
  ENDIF.
*

* Amount Type
  r_wrttp-sign   = 'I'.  r_wrttp-option = 'EQ'.

* FM org/sup/ret
  r_wrttp-low = '43'.  APPEND r_wrttp.
* FM release
  r_wrttp-low = '46'.  APPEND r_wrttp.
* PI budget
  r_wrttp-low = '47'.  APPEND r_wrttp.
* ORDER budget
  r_wrttp-low = '41'.  APPEND r_wrttp.

ENDFORM.                    " set_value_type
*&---------------------------------------------------------------------*
*&      Form  get_obj_info
*&---------------------------------------------------------------------*
FORM get_obj_info USING    f_objnr  f_posit.
  CASE f_objnr(2).
*PI
    WHEN 'IP'.
      READ TABLE it_impr WITH KEY objnr = f_objnr BINARY SEARCH.
      gt_out-ap_gjahr = it_impr-gjahr.
      gt_out-posid = it_impr-posid.
      gt_out-kostl = it_impr-kostl.
      gt_out-prnam = it_impr-prnam.
      READ TABLE it_impu WITH KEY posnr = it_impr-posnr.
      gt_out-post1 = it_impu-post1.
*IO
    WHEN 'OR'.
      READ TABLE it_impr WITH KEY objnr = f_objnr BINARY SEARCH.
      gt_out-ap_gjahr = it_impr-gjahr.

      READ TABLE it_aufk WITH KEY objnr = f_objnr.
      gt_out-aufnr = it_aufk-aufnr.
      gt_out-kostl = it_aufk-kostl.

      READ TABLE it_coas WITH KEY aufnr = it_aufk-aufnr.
      gt_out-ktext = it_coas-ktext.

*FM
    WHEN 'FS'.
      READ TABLE it_impr WITH KEY objnr = f_objnr BINARY SEARCH.
      gt_out-ap_gjahr = it_impr-gjahr.

      READ TABLE ifmfctr WITH KEY ctr_objnr = f_objnr.
      gt_out-kostl = ifmfctr-fictr.
      CLEAR wa_comm.
      READ TABLE ifmci  WITH KEY posit = f_posit.
      IF sy-subrc = 0.
        wa_comm = 'Q'.
      ENDIF.
      gt_out-fipos = ifmci-fipos.
      gt_out-ktext = ifmci-bezei .

  ENDCASE.
ENDFORM.                    " get_obj_info
*&---------------------------------------------------------------------*
*&      Form  select_master_data
*&---------------------------------------------------------------------*
FORM select_master_data.
* commitment Item
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ifmci
     FROM fmci
     WHERE fikrs =  t001-fikrs
     AND   fipos IN s_fipos.

* commitment item text
  LOOP AT ifmci.
    SELECT SINGLE bezei  INTO ifmci-bezei
       FROM fmcit
       WHERE spras = sy-langu
         AND fikrs = t001-fikrs
         AND fipex = ifmci-fipos.
    MODIFY ifmci.
  ENDLOOP.

ENDFORM.                    " select_master_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CLEAR wa_okcode.
  wa_okcode = ok_code.
  CASE wa_okcode.
    WHEN 'SAVE'.

      IF gt_out-act_txt = 'Original'.
        wa_type = '1'.
      ELSEIF gt_out-act_txt = 'Supplement'.
        wa_type = '2'.
      ELSEIF gt_out-act_txt = 'Return'.
        wa_type = '3'.
      ELSEIF gt_out-act_txt = 'Transfer'.
        wa_type = '4'.
      ENDIF.

      SELECT SINGLE * FROM ztfi_reason
      WHERE type = wa_type
      AND   reson = gt_out-reson.

*check...
      IF sy-subrc <> 0.
        MESSAGE w000(zmfi) WITH 'Reason code check'.
        EXIT.
      ELSE.
        MODIFY gt_out INDEX wa_selfield-tabindex.
        PERFORM update_process.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_screen INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_RESON  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_reson INPUT.
  REFRESH : it_value.
  CLEAR   : it_value.

  IF gt_out-act_txt = 'Original'.
    wa_type = '1'.
  ELSEIF gt_out-act_txt = 'Supplement'.
    wa_type = '2'.
  ELSEIF gt_out-act_txt = 'Return'.
    wa_type = '3'.
  ELSEIF gt_out-act_txt = 'Transfer'.
    wa_type = '4'.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_value
  FROM ztfi_reason
  WHERE type = wa_type.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'RESON'
      dynpprog        = 'ZRFII18'
      dynpnr          = '9000'
      dynprofield     = 'GT_OUT-RESON'
      window_title    = 'Reason Code'
      value_org       = 'S'
    TABLES
      value_tab       = it_value
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
*WA_CODE_TXT = WA_CODE_TXT.
ENDMODULE.                 " F4_RESON  INPUT
*&---------------------------------------------------------------------*
*&      Module  UPDATE_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_text INPUT.
  IF gt_out-act_txt = 'Original'.
    wa_type = '1'.
  ELSEIF gt_out-act_txt = 'Supplement'.
    wa_type = '2'.
  ELSEIF gt_out-act_txt = 'Return'.
    wa_type = '3'.
  ELSEIF gt_out-act_txt = 'Transfer'.
    wa_type = '4'.
  ENDIF.


  CONCATENATE wa_type+0(1) gt_out-reson gt_out-sgtext
              INTO gt_out-sgtext.

ENDMODULE.                 " UPDATE_TEXT  INPUT
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3037   text
*      -->P_3038   text
*      -->P_3039   text
*----------------------------------------------------------------------*
FORM make_bdc_rtn USING   dynbegin program dynpro.
  CLEAR it_bdc.

  IF dynbegin = 'X'.
    it_bdc-program  = program.
    it_bdc-dynpro   = dynpro.
    it_bdc-dynbegin = 'X'.
  ELSE.
    it_bdc-fnam     = program.
    it_bdc-fval     = dynpro.
  ENDIF.

  APPEND it_bdc.

ENDFORM.                    " make_bdc_rtn
*&---------------------------------------------------------------------*
*&      Form  update_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_process.
  DATA : wa_fobel LIKE bpbk-fobel.
* if PI budget... update CBO table...
  IF gt_out-act_txt = 'Original'.
    wa_type = '1'.
  ELSEIF gt_out-act_txt = 'Supplement'.
    wa_type = '2'.
  ELSEIF gt_out-act_txt = 'Return'.
    wa_type = '3'.
  ELSEIF gt_out-act_txt = 'Transfer'.
    wa_type = '4'.
  ENDIF.

  CONCATENATE wa_type+0(1) gt_out-reson gt_out-sgtext
           INTO gt_out-sgtext.
  IF r_1 = 'X'.
    UPDATE ztfi_imfm SET reson = gt_out-reson
                         text  = gt_out-sgtext
      WHERE posid = gt_out-posid
      AND   belnr = gt_out-belnr.
  ENDIF.

  UPDATE bpbk SET sgtext = gt_out-sgtext
  WHERE belnr = gt_out-belnr.
*--2003/12/11
  CLEAR wa_fobel.
  SELECT SINGLE fobel INTO wa_fobel
  FROM bpbk
  WHERE belnr = gt_out-belnr.
  IF wa_fobel <> ' '.
    UPDATE bpbk SET sgtext = gt_out-sgtext
    WHERE belnr = wa_fobel.
  ENDIF.

  IF sy-subrc = 0.
    MESSAGE s000(zmfi) WITH 'Sucess process'.
  ELSE.
    MESSAGE e000(zmfi) WITH 'Failure'.
  ENDIF.
ENDFORM.                    " update_process
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
FORM build_layout.
  IF r_1 = 'X'.
    PERFORM build_field_category
    USING :
     'POSID'    'Position ID'   '12' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'POST1'    'Description'   '20' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'AP_GJAHR' 'Aprv.Y'         '04' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'ACT_TXT'  'Activity'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'KOSTL'    'CCtr'          '05' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'RESON'    'Re.Code'       '02' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'DESCR'    'Reason Text'   '16' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'SGTEXT'   'Document Text' '30' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'GJAHR'    'Year'          '04' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'WTJHR'    'Amount'        '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'CPUDT'    'CPU Date'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'BELNR'    'Document'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'USNAM'    'Create By'     '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'TRGKZ'    'Ind'           '03' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'SEQ'      'SEQ'           '04' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'VORGA'    'Budg.Typ'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' '.
  ELSEIF r_2 = 'X'.
    PERFORM build_field_category
    USING :
     'AUFNR'    'Order      '   '12' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'KTEXT'    'Description'   '20' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'ACT_TXT'  'Activity'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'KOSTL'    'CCtr'          '05' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'RESON'    'Re.Code'       '02' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'DESCR'    'Reason Text'   '16' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'SGTEXT'   'Document Text' '30' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'GJAHR'    'Year'          '04' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'WTJHR'    'Amount'        '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'CPUDT'    'CPU Date'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'BELNR'    'Document'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'USNAM'    'Create By'     '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'TRGKZ'    'Ind'           '03' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'SEQ'      'SEQ'           '04' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'VORGA'    'Budg.Typ'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' '.
  ELSE.
*    PERFORM build_field_category
*    USING :
*     'FIPOS'     'Commmt Item'  '12' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*     'KTEXT'     'Description'  '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*     'ACT_TXT'   'Activity'     '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*     'KOSTL'     'CCtr'         '05' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*     'RESON'     'Re.Code'      '02' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*     'DESCR'     'Reason Text'  '16' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*     'SGTEXT'    'Document Text' '30' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*     'GJAHR'     'Year'          '4' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*     'WTJHR'     'Amount'       '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
*     'CPUDT'     'CPU Date'     '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*     'BELNR'     'Document'     '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*     'USNAM'     'Create By'    '10' ' ' 'L'  ' '  ' '  '  ' '  ' .

*-----Changed by JIPARK 2004/04/08
    PERFORM build_field_category
    USING :
     'FIPOS'    'Commmt Item'   '12' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'KTEXT'    'Description'   '20' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'ACT_TXT'  'Activity'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'KOSTL'    'CCtr'          '05' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'RESON'    'Re.Code'       '02' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'DESCR'    'Reason Text'   '16' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'SGTEXT'   'Document Text' '30' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'GJAHR'    'Year'          '04' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'WTP01'    'Jan      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP02'    'Feb      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP03'    'Mar      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP04'    'Apr      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP05'    'May      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP06'    'Jun      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP07'    'Jul      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP08'    'Aug      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP09'    'Sep      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP10'    'Oct      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP11'    'Nov      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'WTP12'    'Dec      '     '13' ' ' 'R'  ' '  ' '  '  ' '  ' 'X',
     'CPUDT'    'CPU Date'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'BELNR'    'Document'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' ',
     'USNAM'    'Create By'     '10' ' ' 'L'  ' '  ' '  '  ' '  ' ' '.
*-----

  ENDIF.
ENDFORM.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  convert_s_posid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_s_posid.
  CHECK NOT s_posid[] IS INITIAL.

  REFRESH: gr_posid.
  gr_posid-option = 'EQ'.
  gr_posid-sign   = 'I'.
  SELECT * FROM imak
    WHERE posid IN s_posid.

    gr_posid-low = imak-posnr.
    APPEND gr_posid.
  ENDSELECT.

*  LOOP AT s_posid.
*    REPLACE '0000' WITH '' INTO : s_posid-low, s_posid-high.
*    CONDENSE : s_posid-low, s_posid-high.
*    MODIFY s_posid.
*  ENDLOOP.

ENDFORM.                    " convert_s_posid
*&---------------------------------------------------------------------*
*&      Form  im_budget_reconcile
*&---------------------------------------------------------------------*
FORM im_budget_reconcile
  USING i_selfield TYPE slis_selfield.

  DATA: l_out LIKE gt_out,
        l_seq LIKE ztfi_imfm-seq,
        l_belnr LIKE ztfi_imfm-belnr,
        l_ins TYPE i,
        l_upd TYPE i.
  TABLES: ztfi_imfm.
  CLEAR : l_ins, l_upd.
*  if i_selfield-tabindex > 0.

  LOOP AT gt_out INTO l_out WHERE chkbox EQ 'X'.
    clear ztfi_imfm.

*    READ TABLE gt_out  INDEX i_selfield-tabindex INTO l_out.
*    IF sy-subrc = 0.

*----- check if document exist...
    SELECT SINGLE belnr INTO l_belnr FROM ztfi_imfm
      WHERE posid = l_out-posid
        AND ayear = l_out-ap_gjahr
        AND gjahr = l_out-gjahr
        AND belnr = l_out-belnr.
    IF sy-subrc = 0.
      UPDATE ztfi_imfm SET vorga  = l_out-vorga
                           status = 'X'
         WHERE posid = l_out-posid
           AND belnr = l_out-belnr.
      l_upd = l_upd + 1.
      CONTINUE.
    ENDIF.

*-- check same date with same amount
    SELECT SINGLE * FROM ztfi_imfm
      WHERE posid = l_out-posid
        AND ayear = l_out-ap_gjahr
        and gubun NE 'P'
        AND gjahr = l_out-gjahr
        AND tot   = l_out-wtjhr
        AND status NE 'X'.
*        AND ddate = l_out-cpudt
*        and vorga = l_out-vorga.
    IF sy-subrc = 0.
      ztfi_imfm-belnr = l_out-belnr.
      ztfi_imfm-ddate = l_out-cpudt.
      ztfi_imfm-vorga = l_out-vorga.
      ztfi_imfm-status = 'X'.
      MODIFY ztfi_imfm.
      l_upd = l_upd + 1.
      CONTINUE.
    ENDIF.

*-- missing -> insert
    ztfi_imfm-posid = l_out-posid.
    ztfi_imfm-posnr = l_out-posid.
    ztfi_imfm-ayear = l_out-ap_gjahr.
    ztfi_imfm-gjahr = l_out-gjahr.
    ztfi_imfm-prnam  = l_out-prnam.
    ztfi_imfm-twaer  = 'USD'.
    ztfi_imfm-tot    = l_out-wtjhr.
*---store amount in december....!!!
    ztfi_imfm-wtp12  = l_out-wtjhr.
    ztfi_imfm-type   = '1'.

    SELECT MAX( seq ) INTO l_seq FROM ztfi_imfm
      WHERE posid = l_out-posid
        AND ayear = l_out-ap_gjahr.
    ztfi_imfm-seq   = l_seq + 1.

    IF l_out-trgkz = 'B'.  "IM c/f
      ztfi_imfm-gubun   = 'B'. "C/F
      ztfi_imfm-wtp00   = l_out-wtjhr.
    ELSE.
      CASE l_out-vorga.
        WHEN 'KBUD'.   ztfi_imfm-gubun   = '1'. "orig
        WHEN 'KBN0'.   ztfi_imfm-gubun   = '2'. "sup
        WHEN 'KBR0'.   ztfi_imfm-gubun   = '3'. "return
        WHEN OTHERS.   ztfi_imfm-gubun   = ' '. "unknown
      ENDCASE.
    ENDIF.
    ztfi_imfm-vorga = l_out-vorga.

    ztfi_imfm-status = 'X'.
    ztfi_imfm-belnr = l_out-belnr.
*      ZTFI_IMFM-TRANS =
    ztfi_imfm-uname = l_out-usnam.
    ztfi_imfm-ddate = l_out-cpudt.
    ztfi_imfm-zdate = l_out-cpudt.
    ztfi_imfm-reson = l_out-reson.
    IF l_out-seq ne '0000'.
      ztfi_imfm-seq   = l_out-seq  .
    ENDIF.
    ztfi_imfm-text  = l_out-sgtext.
    INSERT ztfi_imfm.
    l_ins = l_ins + 1.
  ENDLOOP.

  MESSAGE s000 WITH 'Inserted: ' l_ins '   Updated: ' l_upd.

ENDFORM.                    " im_budget_reconcile
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_AUFK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_PRNAM_LOW  text
*----------------------------------------------------------------------*
FORM archive_read_aufk  USING  p_prnam_low.

  TYPES: BEGIN OF ty_aufk,
         aufnr TYPE aufnr,
         auart TYPE aufart,
         erdat TYPE auferfdat,
         kostv TYPE aufkostv,
         objnr TYPE j_objnr,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_aufk.

  DATA: l_handle    TYPE sytabix,
        lt_aufk     TYPE TABLE OF aufk WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_aufk TYPE TABLE OF ty_aufk,
        ls_inx_aufk TYPE ty_aufk.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZAUFK_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_aufk[].
  IF p_prnam_low <> ' '.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_aufk
      FROM (l_gentab)
     WHERE objnr IN r_objnr
       AND aufnr IN s_aufnr
       AND kostv IN s_kostl.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_aufk
      FROM (l_gentab)
     WHERE aufnr IN s_aufnr
       AND kostv IN s_kostl.
  ENDIF.

  CHECK NOT lt_inx_aufk[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_aufk_a, gt_aufk_a[].
  LOOP AT lt_inx_aufk INTO ls_inx_aufk.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'CO_ORDER'
        archivkey                 = ls_inx_aufk-archivekey
        offset                    = ls_inx_aufk-archiveofs
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
    CLEAR: lt_aufk, lt_aufk[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'AUFK'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_aufk
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_aufk[] IS INITIAL.


* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_aufk INTO TABLE gt_aufk_a.
  ENDLOOP.

  SORT gt_aufk_a.
  DELETE ADJACENT DUPLICATES FROM gt_aufk_a COMPARING ALL FIELDS.

  LOOP AT gt_aufk_a.
    CLEAR it_aufk.
    MOVE-CORRESPONDING gt_aufk_a TO it_aufk.
    APPEND it_aufk.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_AUFK
