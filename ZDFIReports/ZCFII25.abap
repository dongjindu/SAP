REPORT zcfii25  MESSAGE-ID  zmfi.
*&--------------------------------------------------------------------
*& Author                 : Hs.Jeong
*& Creation Date          : 24/02/2004
*& Specification By       : Hs.Jeong
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  :  Collect Actuals & calculate EVA
*&
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------

TYPE-POOLS: slis.
INCLUDE <icon>.
INCLUDE <symbol>.

CLASS cl_gui_resources DEFINITION LOAD.

CONSTANTS:
  c_f2code               LIKE sy-ucomm                    VALUE '&ETA'.

* for combobox
TYPE-POOLS: vrm.
DATA: it_rt TYPE vrm_values,
      w_rt_line LIKE LINE OF it_rt.

DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.

*--- ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: wa_repid LIKE sy-repid,
      wa_var_save(1) TYPE c             VALUE  'A',
      wa_default(1)  TYPE c,
      wa_exit(1) TYPE c,
      wa_variant LIKE disvariant,
      wa_var LIKE disvariant,
      wa_alv_function_name(30) TYPE c VALUE 'REUSE_ALV_GRID_LIST',
      wa_alv_get_info_name(40) TYPE c,
      wa_mode,

       wa_year1  LIKE bapiappreqplanyearmulti-fiscal_year,
       wa_year2  LIKE bapiappreqplanyearmulti-fiscal_year,
       wa_year3  LIKE bapiappreqplanyearmulti-fiscal_year,
       wa_year4  LIKE bapiappreqplanyearmulti-fiscal_year,
       wa_year5  LIKE bapiappreqplanyearmulti-fiscal_year,
       wa_year6  LIKE bapiappreqplanyearmulti-fiscal_year,
       wa_year7  LIKE bapiappreqplanyearmulti-fiscal_year,
       wa_year8  LIKE bapiappreqplanyearmulti-fiscal_year,
       wa_year9  LIKE bapiappreqplanyearmulti-fiscal_year.

DATA: BEGIN OF gt_out OCCURS 0,
        posid  LIKE imak-posid,
        txt50  LIKE imakt-txt50,
        ivart  LIKE imak-ivart,
        gjahr  LIKE imak-gjahr,
        prnam  LIKE impr-prnam,
        status LIKE bapiappreqstatus-status,
        text	 LIKE bapiappreqstatus-text,
        overall(15) TYPE p,
        versi    LIKE imavz-versi,
        varnt    LIKE imav-varnt,
        versi_a  LIKE imavz-versi,
        varnt_a  LIKE imav-varnt,
        fiscal_year(4), " LIKE bapiappreqplanyearmulti-fiscal_year,
        overhead_costs LIKE bapiappreqplanyearmulti-overhead_costs,
        investment_costs LIKE bapiappreqplanyearmulti-investment_costs,
        revenue          LIKE bapiappreqplanyearmulti-revenue,
        year1(14)   TYPE p, " DECIMALS 2,
        year2(14)   TYPE p, " DECIMALS 2,
        year3(14)   TYPE p, " DECIMALS 2,
        year4(14)   TYPE p, " DECIMALS 2,
        year5(14)   TYPE p, " DECIMALS 2,
        year6(14)   TYPE p, " DECIMALS 2,
        year7(14)   TYPE p, " DECIMALS 2,
        year8(14)   TYPE p, " DECIMALS 2,
        year9(14)   TYPE p, " DECIMALS 2,
        chkbox TYPE c,
        light   TYPE c,
*        tabcolor     TYPE slis_t_specialcol_alv,
      END OF gt_out.
*------
*===============================================================*
DATA: it_out TYPE TABLE OF imak WITH HEADER LINE,
      it_imavz TYPE TABLE OF imavz WITH HEADER LINE.

DATA: it_status LIKE bapiappreqstatus OCCURS 0 WITH HEADER LINE.
DATA: it_user_status LIKE bapiappreqstatus OCCURS 0 WITH HEADER LINE.
DATA: it_progtree    LIKE bapiprogstruc    OCCURS 0 WITH HEADER LINE.

*--PI BUDGET
DATA : it_pi_plan LIKE zfi_pi_budget OCCURS 0 WITH HEADER LINE.
*--------------------------*
DATA : BEGIN OF it_imakt OCCURS 0.
        INCLUDE STRUCTURE imakt.
DATA : END OF it_imakt.
*----CBO
DATA : BEGIN OF it_imfm OCCURS 0.
        INCLUDE STRUCTURE ztfi_imfm.
DATA : END OF it_imfm.
*-----Detail
DATA : wa_master_data	LIKE	bapiappreqmaster.
DATA : wa_user_field       LIKE   bapiapprequser.
DATA : wa_parent  LIKE bapiprogaux-parent.

DATA : wa_co_area LIKE bapi_appreq_id-cntrl_area.

DATA : it_variant LIKE bapiappreqvarntmulti OCCURS 0 WITH HEADER LINE.
DATA : it_variant_to_version LIKE bapiappreqvarntassignmulti OCCURS 0
                                                 WITH HEADER LINE.
DATA : it_invest_reson LIKE bapiappreqinvreason OCCURS 0
                                             WITH HEADER LINE.
DATA : it_env_invest   LIKE bapiappreqenvinvest  OCCURS 0
                                             WITH HEADER LINE.
DATA : it_org_units LIKE bapiappreqorgunit OCCURS 0
                                             WITH HEADER LINE.
DATA : BEGIN OF it_plan_tot OCCURS 0.
        INCLUDE STRUCTURE bapiappreqplantotalmulti.
DATA : END OF it_plan_tot.

DATA : BEGIN OF it_plan_year OCCURS 0.
        INCLUDE STRUCTURE bapiappreqplanyearmulti.
DATA : END OF it_plan_year.
*----ar plan set
DATA : it_ar_plan LIKE bapiappreqplanyear OCCURS 0
                          WITH HEADER LINE.
DATA : wa_ar_total LIKE bapiappreqplantotal.

DATA : it_actual LIKE zfi_pi_actual OCCURS 0
                          WITH HEADER LINE.
DATA : it_io_actual LIKE zfi_io_actual OCCURS 0
                          WITH HEADER LINE.
*---Temp internal table
DATA : BEGIN OF it_plan_tot_ar OCCURS 0.
DATA :   posid  LIKE imak-posid.
        INCLUDE STRUCTURE bapiappreqplantotalmulti.
DATA : END OF it_plan_tot_ar.

DATA : BEGIN OF it_plan_year_ar OCCURS 0.
DATA:  posid  LIKE imak-posid.
        INCLUDE STRUCTURE bapiappreqplanyearmulti.
DATA : END OF it_plan_year_ar.

*===== PI Create
DATA : BEGIN OF it_ania OCCURS 0,
          objnr   LIKE ania-objnr,
          anlkl   LIKE ania-anlkl,
          kostl   LIKE ania-kostl,
          aufpr   LIKE ania-aufpr,
       END OF it_ania.
*---PI CHECK BEFORE PI CREATE
DATA : BEGIN OF it_impr OCCURS 0,
          posid LIKE impr-posid,
          objnr LIKE impr-objnr,
          gjahr LIKE impr-gjahr,
          prnam LIKE impr-prnam,
       END OF it_impr.
*---pi plan step
DATA : BEGIN OF it_bpge OCCURS 0,
          objnr LIKE bpge-objnr,
          wtges LIKE bpge-wtges,
       END OF it_bpge.

DATA : it_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
*---AR-PI Link
DATA : BEGIN OF it_imzo OCCURS 0,
          objnr LIKE imzo-objnr,
          gjahr LIKE imzo-gjahr,
       END OF it_imzo.

*=====WorK area
DATA : wk_cnt TYPE i,
       wa_cnt TYPE i,
       wa_d_cnt TYPE i,
       wa_objnr LIKE jest-objnr,
       wa_stat  LIKE bapiappreqstatus-status,
       wa_chk,
       wk_t_cnt TYPE i,
       wa_objnr1 LIKE imzo-objnr,
       wa_objnr2 LIKE ania-objnr,
       wa_bdc_ok,
       wa_varnt LIKE imav-varnt,
       wa_aufpr(6),
       wa_date LIKE sy-datum,
       wa_f_date LIKE sy-datum,
       wa_t_date LIKE sy-datum,
       wa_f_year LIKE imak-gjahr,
       wa_t_year LIKE imak-gjahr,
       wa_conver_date LIKE sy-datum,
       wa_datfm  LIKE usr01-datfm,
       wa_appreqvrnt  LIKE bapiappreqvarntassignmulti,
       wa_amt(15), "  LIKE BPDY-WERT1.
       wa_type  LIKE ifmfincode-type,
       wa_aufnr LIKE aufk-aufnr,
       wa_capex LIKE imtp-capex,
       wa_vernr LIKE impr-vernr,
       wa_plan_tot-overhead_costs LIKE
              bapiappreqplantotalmulti-overhead_costs,
       wa_posid    LIKE  impr-posid,
       wa_min         LIKE imak-gjahr,
       wa_max         LIKE imak-gjahr,
       wa_f_gjahr LIKE imak-gjahr,
       wa_t_gjahr LIKE imak-gjahr.

DATA : wa_year_cnt TYPE i.
DATA : wa_posid1(20),
       wa_pos TYPE i.

*====FOR BDC
DATA : it_bdc      LIKE bdcdata OCCURS 0 WITH HEADER LINE.
DATA:  it_messtab  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA : tcode LIKE tstc-tcode.

DATA: g_fik   LIKE fmfctr-fikrs. " memory id fik obligatory,
*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
TABLES : imak, impr, bpge, bpja,sscrfields.

DATA: i_bpge LIKE bpge OCCURS 0 WITH HEADER LINE.
DATA: i_bpja LIKE bpja OCCURS 0 WITH HEADER LINE.

DATA: it_pi_actual LIKE zfi_pi_actual_act OCCURS 0 WITH HEADER LINE.
RANGES : so_versn FOR bpge-versn.
* possible entries
DATA: hlp_program            LIKE d020s-prog VALUE 'ZCFII21',
      hlp_dynpro             LIKE sy-dynnr   VALUE '1000'.

DATA: g_repid             LIKE sy-repid,           " Name of the report
      g_cursor_field(20)  TYPE c VALUE 'P_OUTG', " default cursor pos.
      g_sscr_ucomm        TYPE sscrfields-ucomm.   " PAI function code

DATA  : wa_bu_chk,
        wa_rfi(6)  TYPE   p DECIMALS 2,
        wa_rp(6)   TYPE   p DECIMALS 2,
        wa_bt(6)   TYPE   p DECIMALS 2,
        wa_efc(6)  TYPE   p DECIMALS 2,
        wa_lc(6)   TYPE   p DECIMALS 2,
        wa_ect(6)  TYPE   p DECIMALS 2,
        wa_wacc(16) TYPE p DECIMALS 2.
RANGES : r_versn FOR imavz-versi.
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
*---0 version source
PARAMETERS : p_prnam  LIKE   impr-prnam OBLIGATORY MEMORY ID imt.
SELECTION-SCREEN BEGIN OF BLOCK b100 WITH FRAME TITLE text-100.
PARAMETERS : pa_gjahr  LIKE  imtp-gjahr OBLIGATORY MEMORY ID gjr.

PARAMETERS:  pa_versn LIKE raip3-versn  DEFAULT '0'
                          OBLIGATORY MEMORY ID bp11.
SELECTION-SCREEN END OF BLOCK b100.
PARAMETERS : p_varnt  LIKE  imavz-varnt OBLIGATORY.
*-----*category
PARAMETERS  : p_categ LIKE tai08-ippos
                                 DEFAULT '1' NO-DISPLAY.
*SELECT-OPTIONS:
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
  s_ivart   FOR   imak-ivart,
  s_posnr   FOR   imak-posnr,
  s_vkostl  FOR   imak-vkostl.
*  s_fiscal  FOR   imak-gjahr OBLIGATORY ."  default sy-datum+0(4).
SELECTION-SCREEN END OF BLOCK b0.
*----------------*
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
*SELECT-OPTIONS: s_gjahr FOR imak-gjahr.
*
*SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS :
  p_layout LIKE disvariant-variant.   "LAYOUT
SELECTION-SCREEN END OF BLOCK b2.
*-----*
*PARAMETERS p_cf  AS CHECKBOX DEFAULT 'X'.   "PI Plan = AR - PI C/F
* BDC mode

*PARAMETERS session AS CHECKBOX .  "create session

PARAMETERS cupdate LIKE ctu_params-updmode DEFAULT 'S' NO-DISPLAY.
"S: synchronously
"A: asynchronously
"L: local

*========================================*
INITIALIZATION.
*========================================*
*----------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f4_variant CHANGING p_layout.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_versn.
*
*  CALL FUNCTION 'AIPA_F4_PLAN_VERSION'
*       EXPORTING
*            i_program           = hlp_program
*            i_dynnr             = hlp_dynpro
*            i_fn_gjahr_proposal = 'P_GJAHR'
*            i_fn_prnam_proposal = 'P_PRNAM'
*            i_fn_versn          = 'P_VERSN'
*            i_takeover_allowed  = 'X'.

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
*  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.


  AUTHORITY-CHECK OBJECT 'Z_FICTR'
           ID 'FM_FIKRS'   FIELD 'H201' "g_fik
           ID 'FM_FICTR'   FIELD '*'.
*---------------------------------------------------------------------
*    M   A   I   N
*
*---------------------------------------------------------------------
END-OF-SELECTION.
*

  PERFORM build_field_category
  USING :
   'POSID'     'AR Number'    '12' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'TXT50'     'Description'  '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'VERSI'     'Version'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'VARNT'     'Variant'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'VARNT_A'   'Changed Var'  '10' ' ' 'L'  ' '  ' '  '  ' '  ' .
*   'FISCAL_YEAR'   'Year'   '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
*   'INVESTMENT_COSTS'
*           'Investment Cost'   '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
*   'OVERHEAD_COSTS'
*           'Overhead Cost'   '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
*   'REVENUE'
*           'Revenue'   '15' ' ' 'R'  ' '  ' '  '  ' '  ' .
* ==> 6. build sorts info
  REFRESH gt_sorts.
  PERFORM build_sort_table
    USING :
       '1'    'POSID'   'X'   'X'   '*'.
* ==> 1. select data from db
  PERFORM select_data.
*PERFORM get_data.
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
*           it_default_fieldcat =
       IMPORTING
            e_exit              = l_exit
            es_variant          = ls_variant
       EXCEPTIONS
            not_found = 2.
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
*  if p_fieldname = 'KUNNR'.
*    ls_fieldcat-emphasize = 'C100'.
*  endif.
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
  REFRESH : it_out, gt_out, it_impr, it_imfm, it_plan_tot_ar,
            it_plan_year_ar.
  CLEAR   : it_out, gt_out, it_impr, it_imfm.
*---set ranges
  MOVE 'I' TO r_versn-sign.
  MOVE 'EQ' TO r_versn-option.
  MOVE pa_versn TO r_versn-low.
  APPEND r_versn.
* read program
  SELECT SINGLE capex INTO wa_capex
  FROM   imtp
  WHERE  prnam = p_prnam
  AND    gjahr = pa_gjahr.
* read imavz -- year + version '0'
  SELECT * INTO TABLE it_imavz FROM imavz
  WHERE  gjahr = pa_gjahr
  AND    versi IN r_versn.

* read AR
  SELECT * INTO TABLE it_out FROM imak
    WHERE posnr  IN s_posnr AND
          ivart  IN s_ivart AND
*          gjahr  EQ pa_gjahr AND
          vkostl IN s_vkostl.

  DESCRIBE TABLE it_out LINES wk_cnt.

  IF wk_cnt > 0.
*
* read AR description
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_imakt
    FROM imakt
    FOR ALL ENTRIES IN it_out
    WHERE spras = sy-langu
    AND   posnr EQ it_out-posnr.

*--PI CHECK
    SELECT posid objnr gjahr prnam
          INTO CORRESPONDING FIELDS OF TABLE it_impr
    FROM impr
    FOR ALL ENTRIES IN it_out
    WHERE  posid = it_out-posid
      AND  gjahr = pa_gjahr.
*      AND  gjahr IN s_gjahr.
*   AND   prnam  = P_prnam.

  ENDIF.

  CLEAR gt_out.

  LOOP AT it_out.

    MOVE-CORRESPONDING it_out TO gt_out.
*--year + version check '0' version.
    READ TABLE it_imavz WITH KEY gjahr = pa_gjahr
                                 posnr = it_out-posnr
                                 versi = pa_versn.
*                                BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
*---'0' version
    MOVE it_imavz-versi TO gt_out-versi.
    MOVE it_imavz-varnt TO gt_out-varnt.

    READ TABLE it_imakt WITH KEY posnr = it_out-posid.
    IF sy-subrc = 0.
      MOVE it_imakt-txt50 TO gt_out-txt50.
    ENDIF.
*
    APPEND gt_out.
    CLEAR gt_out.
  ENDLOOP.
  SORT gt_out ASCENDING BY posid fiscal_year.
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
    DELETE ct_events WHERE name NE 'END_OF_PAGE'
                       AND name NE 'TOP_OF_PAGE'
                       AND name NE 'TOP_OF_LIST'
                       AND name NE 'END_OF_LIST'.
    LOOP AT ct_events ASSIGNING <ls_event>.
      CONCATENATE 'ALV_EVENT_'
                  <ls_event>-name
                  INTO <ls_event>-form.
    ENDLOOP.
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
*-----GET AR Detail.
      PERFORM call_bapi_ar_detail USING gt_out-posid.

      CASE rs_selfield-fieldname.
        WHEN 'POSID'.
          SET PARAMETER ID 'IAF' FIELD gt_out-posid.
          CALL TRANSACTION 'IMA3N' AND SKIP FIRST SCREEN.
      ENDCASE.
*---Assign varaint
    WHEN '&VAR'.
      LOOP AT gt_out WHERE chkbox = 'X'.
        CHECK gt_out-varnt_a =  ' '.
        PERFORM change_varaint USING gt_out-posid gt_out-varnt.
      ENDLOOP.
      rs_selfield-refresh = 'X'.

*----
    WHEN '&PRO'.
      LOOP AT gt_out WHERE chkbox = 'X'.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
*   ---------------------------------- switching view type grid or list
    WHEN 'LIST' OR 'GRID'.
      PERFORM switch_list_or_grid USING r_ucomm.
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
*                it_events               = gt_events[]
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
*&      Form  create_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM create_bdc USING    u_posid.
  REFRESH : it_bdc.
  CLEAR   : it_bdc.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIA1'        '0100',
                      ' '  'IMAK-POSNR'      u_posid,
                     ' '  'BDC_OKCODE'      '/00'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIA1'        '0200',
                     ' '  'BDC_OKCODE'      '=STAV'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIA1'        '0200',
                     ' '  'BDC_OKCODE'      '=BUCH'.
  CALL TRANSACTION tcode   USING it_bdc
                           MODE  'N'
                           UPDATE 'S'
*                    OPTIONS  FROM CTU_PARAMS
                    MESSAGES INTO it_messtab.

ENDFORM.                    " create_bdc
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1444   text
*      -->P_1445   text
*      -->P_1446   text
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
*&      Form  CALL_BAPI_AR_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM call_bapi_ar_detail USING    u_posid.
  REFRESH : it_variant,
            it_plan_tot, it_plan_year,
            it_invest_reson,
            it_org_units,
            it_invest_reson,
            it_variant_to_version,
            it_env_invest.

  CLEAR : wa_master_data, wa_user_field, wa_co_area.

  CALL FUNCTION 'BAPI_APPREQUEST_GETDETAIL'
    EXPORTING
      externalnumber                 = u_posid
      language                       = sy-langu
*   LANGUAGE_ISO                   =
 IMPORTING
   master_data                    = wa_master_data
   user_fields                    = wa_user_field
   controlling_area               = wa_co_area
  TABLES
    org_units                      = it_org_units
*   DIVISION                       =
*   MATERIAL_GROUP                 =
   invest_reason                  =  it_invest_reson
   environmnt_invest              =  it_env_invest
*   ASSETS_EQUIS                   =
*   ORDER                          =
*   WBS_ELEMENT                    =
*   PARTNER                        =
*   ASSIGNMENT_TO_POS              =
*   ASSIGNMENT_TO_BUDG_CATEG       =
    variant                        = it_variant
   variant_to_version              = it_variant_to_version
*   ASSIGNED_APPREQUESTS           =
    plan_total                     = it_plan_tot
    plan_year                      = it_plan_year
*   RETURN                         =
            .

* get variant number from year link
  CLEAR wa_varnt.
*  PERFORM get_variant CHANGING wa_varnt.


ENDFORM.                    " CALL_BAPI_AR_DETAIL
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
         i_callback_program      = wa_repid
         i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
         i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
         is_layout               = gs_layout
         it_fieldcat             = gt_fieldcat[]
         it_special_groups       = gt_sp_group[]
         it_sort                 = gt_sorts[]
*         IT_FILTER               =
         i_default               = wa_default
         i_save                  = wa_var_save
         is_variant              = wa_var
*         it_events               = gt_events[]    "list
         it_events               = w_eventcat[]    "grid
         is_print                = gs_prnt
*        IT_EVENT_EXIT           =
*           I_SCREEN_START_COLUMN   = 10
*           I_SCREEN_START_LINE     = 2
*           I_SCREEN_END_COLUMN     = 80
*           I_SCREEN_END_LINE       = 23
    TABLES
         t_outtab                = gt_out.

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  PI_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pi_status USING u_posid.
  READ TABLE it_impr WITH KEY posid = u_posid.
  IF sy-subrc = 0.
    wa_chk = 'Q'.
  ELSE.
    wa_chk = 'X'.
  ENDIF.

ENDFORM.                    " PI_STATUS
*&---------------------------------------------------------------------*
*&      Form  PI_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM pi_create USING   u_posid.

ENDFORM.                    " PI_CREATE
*&---------------------------------------------------------------------*
*&      Form  fill_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PRNAM  text
*      <--P_P_PRNAM  text
*----------------------------------------------------------------------*
FORM fill_data USING    u_field
               CHANGING c_field.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = u_field
       IMPORTING
            output = c_field.

ENDFORM.                    " fill_data
*&---------------------------------------------------------------------*
*&      Form  AR_PI_LINK_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM ar_pi_link_proc USING    u_posid.

ENDFORM.                    " AR_PI_LINK_PROC
*&---------------------------------------------------------------------*
*&      Form  GET_DETAIL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_detail_data   USING u_posid.
  REFRESH : it_progtree.
  CLEAR   : it_progtree.
  PERFORM fill_data USING u_posid
                  CHANGING u_posid.
  wa_parent = u_posid+0(7).
  it_progtree-level = '01'.
  it_progtree-position = u_posid.
  it_progtree-description = wa_master_data-req_txt.

  READ TABLE it_variant WITH KEY appreqvrnt = wa_varnt.
  it_progtree-valid_from_fy = it_variant-start_up_date+0(4).
  it_progtree-valid_to_fy   = it_variant-completion_date+0(4).

  it_progtree-scale         = wa_master_data-scale.
  it_progtree-priority      = wa_master_data-priority.

  READ TABLE it_invest_reson INDEX 1.
  it_progtree-reason        = it_invest_reson-inv_reason.
*---C C
  it_progtree-responsible   = wa_master_data-rsp_cost_center.
*--Controlling Area
  it_progtree-co_area       = wa_co_area.
*--Reauest Cost center
  it_progtree-cost_center   = it_org_units-req_cost_center.
*--Company Code
  it_progtree-company_code = wa_master_data-rsp_comp_code.
*--Asset
  it_progtree-bal_sheet_item = wa_user_field-user03.
*--PLANT.
  it_progtree-plant          = wa_master_data-plant.
  READ TABLE it_env_invest INDEX 1.
  APPEND it_progtree.
  CLEAR  it_progtree.

  PERFORM fill_data USING  gt_out-prnam
                    CHANGING gt_out-prnam.
  PERFORM fill_data USING wa_parent
                    CHANGING wa_parent.
*

ENDFORM.                    " GET_DETAIL_DATA
*&---------------------------------------------------------------------*
*&      Form  IO_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM io_create USING    u_posid.

  REFRESH : it_bdc, it_messtab.
  CLEAR   : it_bdc, it_messtab.
  CLEAR   : wa_f_date, wa_t_date, wa_type, wa_aufnr.
  DATA : wa_amt(15) TYPE c.
  CLEAR : wa_amt.
  wa_amt = wa_plan_tot-overhead_costs.
  TRANSLATE  wa_amt  USING ', '.
  CONDENSE   wa_amt NO-GAPS.

  tcode = 'KO01'.

* CONCATENATE   u_posid  INTO wa_aufnr.
  CONCATENATE  '01' '01' wa_master_data-orig_appr_year
                                              INTO wa_f_date.

  CONCATENATE it_variant-completion_date+4(2)
              it_variant-completion_date+6(2)
              it_variant-completion_date+0(4) INTO wa_t_date.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMKAUF'        '0100',
                      ' '  'COAS-AUART'      'Y',
                      ' '  'BDC_OKCODE'      '/00'.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMKAUF'        '0600',
                      ' '  'COAS-AUFNR'      u_posid,
                      ' '  'COAS-KTEXT'      wa_master_data-req_txt,
                      ' '  'COAS-BUKRS'
                                    wa_master_data-rsp_comp_code,
                      ' '  'COAS-KOSTV'
                                    wa_master_data-rsp_cost_center,
                      ' '  'COAS-AKSTL'
                                    it_org_units-req_cost_center,
                      ' '  'BDC_OKCODE'      '=BUT5'.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMKAUF'        '0600',
*                      ' '  'COAS-AUFNR'      '0PAAB2RB0025',
*                      ' '  'COAS-KTEXT'      '2525',
*----2003/12/18 add
                      ' '  'COAS-IVPRO'       '999',
                      ' '  'COAS-IZWEK'
                              it_invest_reson-inv_reason, "ZZ',
*                      ' '  'COAS-UMWKZ'      ' ',
*                      ' '  'RAIP1-PRNAM'      p_prnam,    "AA',
*                      ' '  'RAIP1-GJAHR'
*                                 wa_master_data-orig_appr_year,"2003',
*                      ' '  'RAIP1-POSID'      u_posid,
                      ' '  'BDC_OKCODE'      '/00'.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIPP'        '0200',
                      ' '  'RAIP1-BAPRZ(02)' '100',
                      ' '  'BDC_OKCODE'      '=TAKE'.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMKAUF'        '0600',
*                      ' '  'COAS-AUFNR'      '0PAAB2RB0025',
*                      ' '  'COAS-KTEXT'      '2525',
*                      ' '  'COAS-IZWEK'      'ZZ',
                      ' '  'BDC_OKCODE'      '=BUT2'.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMKAUF'        '0600',
*                      ' '  'COAS-AUFNR'      '0PAAB2RB0025',
*                      ' '  'COAS-KTEXT'      '2525',
                      ' '  'COAS-WAERS'      'USD',
                      ' '  'COAS-CYCLE'
                                   it_org_units-req_cost_center,
                      ' '  'BDC_OKCODE'      '=VARI'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMKAUF'        '0600',
*                      ' '  'COAS-AUFNR'      '0PAAB2RB0025',
*                      ' '  'COAS-KTEXT'      '2525',
*                      ' '  'COAS-WAERS'      'USD',
                      ' '  'COAS-USER4'        wa_amt,
*                                        WA_PLAN_TOT-OVERHEAD_COSTS,
*                      ' '  'COAS-CYCLE'
*                                   it_org_units-req_cost_center,
                      ' '  'BDC_OKCODE'      '=BUT5'.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMKAUF'        '0600',
*                      ' '  'COAS-AUFNR'      '0PAAB2RB0025',
*                      ' '  'COAS-KTEXT'      '2525',
*                      ' '  'COAS-WAERS'      'USD',
*                      ' '  'COAS-CYCLE'
*                                   it_org_units-req_cost_center,
                      ' '  'BDC_OKCODE'      '=SICH'.

  CALL TRANSACTION tcode   USING it_bdc
                           MODE   'E'
                           UPDATE 'S'
                    MESSAGES INTO it_messtab.

*  READ TABLE it_messtab INDEX 1.
*
*  IF sy-subrc <> 0.
*    MOVE 'Created'   TO gt_out-proc7.
*    MOVE 'A'         TO gt_out-status7.
*  ELSE.
*    IF it_messtab-msgtyp = 'S'.
*      MOVE 'Created'   TO gt_out-proc7.
*      MOVE 'A'         TO gt_out-status7.
*    ELSE.
*      MOVE 'BDC Error' TO gt_out-proc7.
*      MOVE 'Q'         TO gt_out-status7.
*    ENDIF.
*  ENDIF.


ENDFORM.                    " IO_CREATE
*&---------------------------------------------------------------------*
*&      Form  PI_PLAN_step_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM pi_plan_step_check USING    u_posid.
ENDFORM.                    " PI_PLAN_step_check
*&---------------------------------------------------------------------*
*&      Form  INCLUDE_PROGRAM_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM include_program_check USING    u_posid
                          CHANGING c_chk.

  READ TABLE it_impr WITH KEY posid = u_posid.
  IF sy-subrc = 0.
    MOVE it_impr-prnam  TO gt_out-prnam.
  ELSE.
    c_chk = 'X'.
  ENDIF.
ENDFORM.                    " INCLUDE_PROGRAM_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHK_OVERHEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_VARNT  text
*----------------------------------------------------------------------*
FORM chk_overhead USING    u_varnt
                  CHANGING c_plan_tot-overhead_costs.
  READ TABLE it_plan_tot WITH KEY appreqvrnt = u_varnt.
  IF sy-subrc = 0.
    IF  it_plan_tot-overhead_costs <> 0.
      c_plan_tot-overhead_costs = it_plan_tot-overhead_costs.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHK_OVERHEAD

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
* batch input session
*  IF session = 'X'.
*    CALL FUNCTION 'BDC_INSERT'
*         EXPORTING
*              tcode     = tcode
*         TABLES
*              dynprotab = it_bdc.
** call transaction using
*  ELSE.
*    CALL TRANSACTION tcode USING it_bdc
*                     MODE   'E'        "error/all/no disp
*                     UPDATE cupdate    "sync/async/local
*                     MESSAGES INTO it_messtab.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*      <--P_WA_STAT  text
*      <--P_WA_CHK  text
*----------------------------------------------------------------------*
FORM get_status USING    u_posid
                CHANGING c_stat LIKE bapiappreqstatus-status
                         c_chk.
  REFRESH : it_status, it_user_status.
  CLEAR   : it_status, it_user_status.
  CALL FUNCTION 'BAPI_APPREQUEST_GETSTATUS'
    EXPORTING
      externalnumber                    = u_posid
      language                          = sy-langu
*   LANGUAGE_ISO                      =
   TABLES
     apprequest_status                 = it_status
     apprequest_user_status            = it_user_status
*   APPREQUESTVARNT_STATUS            =
*   APPREQUESTVARNT_USER_STATUS       =
*   RETURN                            =
            .
*  WAIT  UP TO '2.0' SECONDS.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  READ TABLE it_status INDEX 1.
  IF sy-subrc = 0.
    c_stat = it_status-status.
  ENDIF.
  CLEAR : wk_t_cnt.
  DESCRIBE TABLE it_user_status LINES wk_t_cnt.
  IF wk_t_cnt < 1.
    MOVE 'X' TO c_chk.
  ELSE.
    MOVE ' '  TO c_chk.
  ENDIF.

ENDFORM.                    " get_status
*&---------------------------------------------------------------------*
*&      Form  make_ar_posid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM make_ar_posid USING    u_posid.

  LOOP AT it_plan_tot.
    MOVE u_posid TO it_plan_tot_ar-posid.
    MOVE-CORRESPONDING it_plan_tot TO it_plan_tot_ar.
    APPEND it_plan_tot_ar.
    CLEAR  it_plan_tot_ar.
  ENDLOOP.

  LOOP AT it_plan_year.
    MOVE u_posid TO it_plan_year_ar-posid.
    MOVE-CORRESPONDING it_plan_year TO it_plan_year_ar.
    APPEND it_plan_year_ar.
    CLEAR  it_plan_year_ar.
  ENDLOOP.

ENDFORM.                    " make_ar_posid
*&---------------------------------------------------------------------*
*&      Form  ADD_PI_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*      -->P_GT_OUT_PRNAM  text
*----------------------------------------------------------------------*
FORM add_pi_plan USING    u_posid
                          u_prnam.

ENDFORM.                    " ADD_PI_PLAN
*&---------------------------------------------------------------------*
*&      Form  build_sort_table
*&---------------------------------------------------------------------*
*&      Form  build_sort_table
*&---------------------------------------------------------------------*
FORM build_sort_table USING  p_spos
                             p_fieldname
                             p_up
                             p_subtot
                             p_group.
  DATA: ls_sort TYPE slis_sortinfo_alv.

  ls_sort-spos      = p_spos.
  ls_sort-fieldname = p_fieldname.
  ls_sort-up        = p_up.
  ls_sort-subtot    = p_subtot.
  ls_sort-group     = p_group.
  APPEND ls_sort TO gt_sorts.
ENDFORM.                    " build_sort_table
*&---------------------------------------------------------------------*
*&      Form  get_actual_io
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*      -->P_MOVE  text
*      -->P_1      text
*      -->P_TO  text
*      -->P_GT_OUT_LIGHT  text
*----------------------------------------------------------------------*
FORM get_actual_io USING    u_posid.
  REFRESH : it_io_actual.
  CLEAR   : it_io_actual.
  wa_aufnr = u_posid.
  CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
    EXPORTING
      aufnr         = wa_aufnr
* IMPORTING
*   AMT           =
    TABLES
      out           = it_io_actual
* EXCEPTIONS
*   NO_DATA       = 1
*   OTHERS        = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " get_actual_io
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

*  SELECT a~posid b~txt50 d~versi d~varnt c~prnam
*        into corresponding fields of table gt_out
*  from imak  as a inner join imavz as d
*     on d~posnr = a~posnr
*  inner join imakt as b
*     on  b~posnr eq a~posnr
*  inner join  impr as c
*  on  c~posid = a~posid
**    and c~gjahr = p_gjahr
*   where a~posnr  in s_posnr  and
*         a~ivart  in s_ivart  and
*         a~vkostl in s_vkostl and
*         b~spras = sy-langu   and
*         d~gjahr = pa_gjahr   and
*         d~versi in r_versn   and
*         c~prnam = p_prnam  and
*         d~versi = pa_versn.
**  SELECT a~posid b~txt50 d~versi d~varnt
**        into corresponding fields of table gt_out
**  from imak  as a inner join imavz as d
**     on d~posnr = a~posnr
**  inner join imakt as b
**     on  b~posnr eq a~posnr
**   where a~posnr  in s_posnr  and
**         a~ivart  in s_ivart  and
**         a~vkostl in s_vkostl and
**         b~spras = sy-langu   and
**         d~gjahr = pa_gjahr   and
**         d~versi in r_versn   and
**  d~versi = pa_versn.
**
*  SORT gt_out ASCENDING BY posid fiscal_year.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_io_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM get_io_actual USING    u_posid.
  DATA : wa_aufnr LIKE aufk-aufnr.

  REFRESH : it_io_actual.
  CLEAR   : it_io_actual.
  CALL FUNCTION 'CONVERSION_EXIT_AUFNR_INPUT'
       EXPORTING
            input  = u_posid
       IMPORTING
            output = u_posid.
  wa_aufnr = u_posid.          .
  CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
    EXPORTING
      aufnr         = wa_aufnr
* IMPORTING
*   AMT           =
    TABLES
      out           = it_io_actual
* EXCEPTIONS
*   NO_DATA       = 1
*   OTHERS        = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " get_io_actual
*&---------------------------------------------------------------------*
*&      Form  change_varaint
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM change_varaint USING    u_posid u_varnt.
*---set ranges
  MOVE 'I' TO r_versn-sign.
  MOVE 'EQ' TO r_versn-option.
  MOVE pa_versn TO r_versn-low.
  APPEND r_versn.
 DATA : it_version LIKE bapiappreqvarntassign OCCURS 0 WITH HEADER LINE.
  REFRESH : it_version.
  CLEAR   : it_version.
  MOVE pa_gjahr TO  it_version-appr_year.
  MOVE pa_versn TO  it_version-plan_version.
  APPEND it_version.
  CALL FUNCTION 'BAPI_APPREQUEST_UASSGNVRNTVRSN'
    EXPORTING
      externalnumber                    = u_posid
      appropriationrequestvariant       =  u_varnt
*   TEST_RUN                          = ' '
   TABLES
     variant_to_version                = it_version.

*   RETURN                            = it_return  .


  CALL FUNCTION 'BAPI_APPREQUEST_ASSGNVRNTVERSN'
    EXPORTING
      externalnumber                    = u_posid
      appropriationrequestvariant       = p_varnt
*   TEST_RUN                          = ' '
   TABLES
     variant_to_version                = it_version.
*   RETURN                            =
  .



  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
* EXPORTING
*  WAIT          =
   IMPORTING
     return        =  it_return.

*  READ TABLE it_return INDEX 1.
*  IF sy-subrc = 0.
*    MESSAGE s000(zmfi) WITH it_return-message.
*  ELSE.
*    MESSAGE s000(zmfi) WITH 'Success Data save'.
**    MOVE 'Q' TO wa_save_check.
*  ENDIF.
  .
  CLEAR : wa_cnt.
  DESCRIBE TABLE it_return LINES wa_cnt.
  IF wa_cnt < 1.
    MOVE  p_varnt    TO gt_out-varnt_a.
    MESSAGE s000(zmfi) WITH 'Changed Varnt'.
**---2004/03/18
    MODIFY gt_out TRANSPORTING varnt_a
                WHERE posid = u_posid.
  ELSE.
    MESSAGE s000(zmfi) WITH 'Varaint Not assign'.
  ENDIF.

ENDFORM.                    " change_varaint
