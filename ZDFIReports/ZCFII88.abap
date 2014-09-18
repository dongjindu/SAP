REPORT zcfii88  MESSAGE-ID  zmfi.
*&--------------------------------------------------------------------
*& Author                 : Hs.Jeong
*& Creation Date          : 09/18/2003
*& Specification By       : Hs.Jeong
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  :  Roll Up Plan Value from App.Requests
*&
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*& #1 02/08/2005 wskim        UD1K914199      not used Budget category..
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
        objnr  LIKE imak-objnr,
        status LIKE bapiappreqstatus-status,
        text	 LIKE bapiappreqstatus-text,
        overall(15) TYPE p,
        varnt    LIKE imav-varnt,
        year1(14)   TYPE p, " DECIMALS 2,
        year2(14)   TYPE p, " DECIMALS 2,
        year3(14)   TYPE p, " DECIMALS 2,
        year4(14)   TYPE p, " DECIMALS 2,
        year5(14)   TYPE p, " DECIMALS 2,
        year6(14)   TYPE p, " DECIMALS 2,
        year7(14)   TYPE p, " DECIMALS 2,
        year8(14)   TYPE p, " DECIMALS 2,
        year9(14)   TYPE p, " DECIMALS 2,
*        proc1(20) TYPE c,
*        status2,
*        proc2(16) TYPE c,
*        status3,
*        proc3(16) TYPE c,
*        status4,
*        proc4(16) TYPE c,
*        status5,
*        proc5(16) TYPE c,
*        status6,
*        proc6(16) TYPE c,
*        status7,
*        proc7(16) TYPE c,
        chkbox TYPE c,
        light   TYPE c,
        tabcolor     TYPE slis_t_specialcol_alv,
      END OF gt_out.
*------
*===============================================================*
DATA: it_out TYPE TABLE OF imak WITH HEADER LINE.
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
          posnr LIKE impr-posnr,
       END OF it_impr.
*---pi plan step
DATA : BEGIN OF it_bpge OCCURS 0,
          objnr LIKE bpge-objnr,
          wtges LIKE bpge-wtges,
       END OF it_bpge.

DATA : it_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
*---AR-PI Link
DATA : BEGIN OF it_imzo OCCURS 0.
        INCLUDE STRUCTURE imzo.
DATA : END OF it_imzo.

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


DATA : wa_year_cnt TYPE i,
         wa_baprz LIKE imzo-baprz.


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
TABLES : imak, impr, bpge, bpja.
DATA: i_bpge LIKE bpge OCCURS 0 WITH HEADER LINE.
*-----start #1 wskim
*DATA: i_bpja LIKE bpja OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF i_bpja OCCURS 0,
        gjahr LIKE bpja-gjahr,
        wtjhr LIKE bpja-wtjhr,
       END OF i_bpja.
*-----end
RANGES : so_versn FOR bpge-versn.
CONSTANTS p_categ  LIKE tai08-ippos VALUE '1'.

*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------

PARAMETERS : p_prnam  LIKE   bapiprogposid-program OBLIGATORY.  "
* AR variant year, PI approval year
PARAMETERS : p_gjahr  LIKE  imtp-gjahr OBLIGATORY MEMORY ID gjr.

PARAMETERS: p_versn LIKE bpge-versn OBLIGATORY.
*-----*category
*-----Start #1 wskim
*PARAMETERS  : p_categ LIKE tai08-ippos OBLIGATORY DEFAULT '1'.
*-----End
*SELECT-OPTIONS:
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
  s_ivart   FOR   imak-ivart,
  s_posnr   FOR   imak-posnr,
  s_vkostl  FOR   imak-vkostl,
* AR approval year
*  s_gjahr   FOR   imak-gjahr,
  s_fiscal  FOR   imak-gjahr OBLIGATORY ."  default sy-datum+0(4).
SELECTION-SCREEN END OF BLOCK b0.
*----------------*
PARAMETERS: p_rollup AS CHECKBOX DEFAULT 'X'.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS : r_over  RADIOBUTTON GROUP gr1 DEFAULT 'X',
             r_add   RADIOBUTTON GROUP gr1,
             r_reset RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS :
  p_layout LIKE disvariant-variant.   "LAYOUT
SELECTION-SCREEN END OF BLOCK b2.
*-----*
PARAMETERS p_cf  AS CHECKBOX DEFAULT 'X'.   "PI Plan = AR - PI C/F
* BDC mode

PARAMETERS session AS CHECKBOX.  "create session
PARAMETERS cupdate LIKE ctu_params-updmode DEFAULT 'S' NO-DISPLAY.
"S: synchronously
"A: asynchronously
"L: local
PARAMETERS p_mode TYPE c DEFAULT 'E'.

*-----------------*
INITIALIZATION.
  wa_f_year = sy-datum+0(4) - 3.
  wa_t_year = sy-datum+0(4) + 6.
  MOVE : 'I'    TO s_fiscal-sign,
         'EQ'   TO s_fiscal-option,
         wa_f_year TO s_fiscal-low,
         wa_t_year TO s_fiscal-high.
  APPEND s_fiscal.
*----------------------------------------------------------------------
*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*
*----------------------------------------------------------------------
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
*  wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
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

  PERFORM set_year.

* ==> 1. select data from db
  PERFORM select_data.
  IF gt_out[] IS INITIAL.
    MESSAGE s000(zmfi) WITH 'No Data'.
    EXIT.
  ENDIF.

  PERFORM build_field_category
  USING :
   'POSID'     'AR Number'    '12' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'TXT50'     'Description'  '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'IVART'     'Type'         '02' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'VARNT'     'Variant'      '04' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'OVERALL'   'AR Overall'   '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR1'     wa_year1       '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR2'     wa_year2       '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR3'     wa_year3       '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR4'     wa_year4       '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR5'     wa_year5       '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR6'     wa_year6       '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR7'     wa_year7       '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR8'     wa_year8       '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR9'     wa_year9       '15' ' ' 'R'  ' '  ' '  '  ' '  ' .
* ==> 6. build sorts info
*  REFRESH gt_sorts.
*  PERFORM build_sort_table
*    USING :
*       '1'    'VTWEG'   'X'   'X'   '*'.

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

* read program
  SELECT SINGLE capex INTO wa_capex
  FROM   imtp
  WHERE  prnam = p_prnam
  AND    gjahr = p_gjahr.

* read AR
  SELECT * INTO TABLE it_out FROM imak
    WHERE posnr  IN s_posnr AND
          ivart  IN s_ivart AND
*          gjahr  eq p_gjahr AND
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
    SELECT posid objnr gjahr prnam posnr
          INTO CORRESPONDING FIELDS OF TABLE it_impr
    FROM impr
    FOR ALL ENTRIES IN it_out
    WHERE  posid = it_out-posid
      AND  gjahr = p_gjahr
*      AND  gjahr IN s_gjahr.
      AND  prnam  = p_prnam.
*---2004/04/21
    CLEAR wa_cnt.
    DESCRIBE  TABLE it_impr LINES wa_cnt.
    IF wa_cnt > 0.
      SELECT posnr objnr ippos baprz prozu
      INTO CORRESPONDING FIELDS OF TABLE it_imzo
      FROM imzo
      FOR ALL ENTRIES IN it_impr
      WHERE  posnr EQ it_impr-posnr
      AND    gjahr EQ it_impr-gjahr.
*-----Start #1 wskim
*     AND    ippos EQ p_categ.
*-----End
    ENDIF.

*--PI plan step CHECK - NO meaning... ANDY
    SELECT objnr wtges INTO CORRESPONDING FIELDS OF TABLE it_bpge
    FROM bpge
    FOR ALL ENTRIES IN it_impr
    WHERE  objnr = it_impr-objnr
      AND  wrttp = '48'.             " program plan

  ENDIF.

  CLEAR gt_out.
  LOOP AT it_out.
    MOVE-CORRESPONDING it_out TO gt_out.
    READ TABLE it_imakt WITH KEY posnr = it_out-posid.
    IF sy-subrc = 0.
      MOVE it_imakt-txt50 TO gt_out-txt50.
    ENDIF.
*---program check.
    READ TABLE it_impr WITH KEY posid = it_out-posid
                                prnam = p_prnam.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
*---OBJECT check.
    READ TABLE it_imzo WITH KEY posnr = it_impr-posnr.
*-----Start #1 wskim
*                               ippos = p_categ.
*-----End
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    CLEAR : wa_stat, wa_chk.
    PERFORM get_status USING it_out-posid
                       CHANGING wa_stat
                                wa_chk.
    IF wa_stat <> 'I0364'.
      CONTINUE.
    ENDIF.

*---GET Status
    CLEAR : wa_stat, wa_chk.

    PERFORM pi_status  USING it_out-posid.

    CHECK wa_chk EQ 'Q'.      " PI Create Check
*---version check 2004/01/05
*-----GET AR Detail.
    PERFORM call_bapi_ar_detail USING it_out-posid.
    CLEAR : wa_varnt.
    PERFORM get_variant CHANGING wa_varnt.
    IF wa_varnt = ' '.
      EXIT.
    ENDIF.
    MOVE wa_varnt TO gt_out-varnt.
*-----*
*    PERFORM make_ar_posid USING it_out-posid.

    MOVE 1 TO gt_out-light.
    APPEND gt_out.
    CLEAR gt_out.
  ENDLOOP.
  SORT gt_out ASCENDING BY posid.
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
*---PI Plan Step
    WHEN '&PIP'.
      LOOP AT gt_out WHERE chkbox = 'X'.
*        CLEAR : wa_varnt.
        PERFORM call_bapi_ar_detail1 USING gt_out-posid.
        PERFORM get_variant1 CHANGING wa_varnt.
        PERFORM pi_plan_step USING gt_out-posid gt_out-varnt
                                   gt_out-objnr.
        MODIFY gt_out.
      ENDLOOP.
      IF p_rollup = 'X'.
*        EXPORT p_prnam  p_gjahr P_VERSN
*                TO MEMORY.
*        SUBMIT ZCFII77   AND RETURN.
        MOVE  : 'I'      TO so_versn-sign,
                'EQ'     TO so_versn-option,
                 p_versn TO so_versn-low.
        APPEND so_versn.

        SUBMIT zaimbpup AND RETURN
               WITH program  =  p_prnam
*               WITH POSITION =  POSITION
               WITH app_year =  p_gjahr
               WITH plan     =  'X'
               WITH fromleaf =  'X'
               WITH so_versn IN so_versn.
      ENDIF.
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
  PERFORM get_variant CHANGING wa_varnt.


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
         it_events               = gt_events[]
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

  APPEND it_progtree.
  CLEAR  it_progtree.
*    ENDLOOP.
  PERFORM fill_data USING   p_prnam
                    CHANGING p_prnam.
  PERFORM fill_data USING wa_parent
                    CHANGING wa_parent.
*
  REFRESH : it_bdc, it_messtab.
  CLEAR   : it_bdc.
  tcode = 'IM22'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIP2'        '0500',
                      ' '  'IMPR-PRNAM'      p_prnam,
                      ' '  'IMPR-POSID'      wa_parent,
                      ' '  'IMPR-GJAHR'      p_gjahr,
*                             wa_master_data-orig_appr_year,
                     ' '  'BDC_OKCODE'      '=STRU'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMSSY0'        '0120',
                     ' '  'BDC_OKCODE'      '=NPOS'.
  PERFORM make_bdc_rtn USING :
                      'X'  'RAIMHIER'        '0100',
                      ' '  'EL1(01)'        u_posid,
                      ' '  'EL2(01)'        wa_master_data-req_txt,
                      ' '  'EL3(01)'        it_invest_reson-inv_reason,
                      ' '  'EL4(01)'        wa_master_data-priority,
                      ' '  'EL5(01)'        wa_master_data-scale,
                     ' '  'EL7(01)'
                           wa_master_data-desired_start+0(4),
                     ' '  'EL8(01)'   it_variant-completion_date+0(4),
                      ' '  'ELA(01)'  wa_master_data-rsp_cost_center,
                      ' '  'ELC(01)'  wa_master_data-rsp_comp_code,
                      ' '  'ELF(01)' wa_master_data-plant,
                     ' '  'BDC_OKCODE'      '=TAKE'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMSSY0'        '0120',
                     ' '  'BDC_OKCODE'      '=UPD'.
  CALL TRANSACTION tcode   USING it_bdc
                           MODE   'E'
                           UPDATE 'S'
*                    OPTIONS  FROM CTU_PARAMS
                    MESSAGES INTO it_messtab.
*  READ TABLE it_messtab INDEX 1.
*  IF it_messtab-msgtyp = 'E'.
*    MOVE 'Q'        TO gt_out-status2.
*    MOVE 'PI Create Error' TO gt_out-proc2.
*    MOVE ' '         TO wa_bdc_ok.
*  ELSE.
*    MOVE 'A'         TO gt_out-status2.
*    MOVE 'Created'   TO gt_out-proc2.
*    MOVE 'Q'         TO wa_bdc_ok.
*  ENDIF.
*

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
  REFRESH : it_bdc.
  CLEAR   : it_bdc.
  tcode = 'IMA2'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIA1'        '0100',
                      ' '  'IMAK-POSNR'      u_posid,
                     ' '  'BDC_OKCODE'      '/00'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIA1'        '0200',
                     ' '  'BDC_OKCODE'      '=ANFO'.
  IF u_posid+0(1) = 'C'.
    PERFORM make_bdc_rtn USING :
                    'X'  'SAPLAIA1'        '0205',
                    ' '  'RAIP1-PRNAM'     p_prnam,
                    ' '  'RAIP1-GJAHR'     p_gjahr,
                    ' '  'RAIP1-POSID'     u_posid,
                   ' '  'BDC_OKCODE'      '=BUCH'.

  ELSEIF u_posid+0(1) = 'P'.
    PERFORM make_bdc_rtn USING :
                    'X'  'SAPLAIA1'        '0205',
                    ' '  'RAIP1-PRNAM'     p_prnam,
                    ' '  'RAIP1-GJAHR'     p_gjahr,
                    ' '  'RAIP1-POSID'     u_posid,
                   ' '  'BDC_OKCODE'      '/00'.
    PERFORM make_bdc_rtn USING :
                    'X'  'SAPLAIPP'        '0200',
                    ' '  'RAIP1-BAPRZ(01'  '100',
                   ' '  'BDC_OKCODE'      '=TAKE'.
    PERFORM make_bdc_rtn USING :
                    'X'  'SAPLAIA1'        '0208',
                   ' '  'BDC_OKCODE'      '=BUCH'.

  ENDIF.

  CALL TRANSACTION tcode   USING it_bdc
                           MODE   'E'
                           UPDATE 'S'
*                    OPTIONS  FROM CTU_PARAMS
                    MESSAGES INTO it_messtab.
*  READ TABLE it_messtab   INDEX 1.
*  IF it_messtab-msgtyp = 'E'.
*    MOVE 'Q'         TO gt_out-status3.
*    MOVE 'BDC Error' TO gt_out-proc3.
*  ELSE.
*    MOVE 'A'         TO gt_out-status3.
*    MOVE 'Created'   TO gt_out-proc3.
*  ENDIF.
*

ENDFORM.                    " AR_PI_LINK_PROC
*&---------------------------------------------------------------------*
*&      Form  PI_PLAN_STEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM pi_plan_step USING    u_posid u_varnt u_objnr.

  REFRESH : it_bdc, it_messtab.
  CLEAR   : it_bdc, it_messtab.


* check carryforward amount
  PERFORM get_cf_amount USING u_posid.

* BDC or Batchjob
  PERFORM open_group.
*--- In case add
  IF r_add = 'X'.
    PERFORM add_pi_plan USING gt_out-posid
                              gt_out-prnam.
  ENDIF.

  PERFORM create_pi_plan_bdc USING u_posid u_varnt u_objnr.

* End of BDC/Job
  PERFORM close_group.

*-------------------------------------------------*
*  READ TABLE it_messtab INDEX 1.
*  IF sy-subrc <> 0.
*    MOVE 'A'         TO gt_out-status4.
*    MOVE 'Created'   TO gt_out-proc4.
*  ELSE.
*    IF it_messtab-msgtyp = 'S'.
*      MOVE 'A'         TO gt_out-status4.
*      MOVE 'Created'   TO gt_out-proc4.
*    ELSE.
*      MOVE 'Q'         TO gt_out-status4.
*      MOVE 'BDC Error' TO gt_out-proc4.
*    ENDIF.
*  ENDIF.
*
ENDFORM.                    " PI_PLAN_STEP
*&---------------------------------------------------------------------*
*&      Form  pi_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM pi_change USING    u_posid.
  DATA : wa_l_cnt TYPE i.
  REFRESH : it_ania.
  CLEAR   : it_ania,  wa_objnr2, wa_l_cnt.
  CONCATENATE 'IO' gt_out-posid INTO wa_objnr2.
*  MOVE wa_varnt TO wa_objnr2+14(4).
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ania
*  FROM ania
*  WHERE objnr = wa_objnr2. "'IOPAAB2RB0002 0010'.
  .
*  CONCATENATE it_variant-completion_date+4(2)
*              it_variant-completion_date+6(2)
*              it_variant-completion_date+0(4) INTO wa_date.
*
  CONCATENATE wa_user_field-user09_date+4(2)
              wa_user_field-user09_date+6(2)
              wa_user_field-user09_date+0(4) INTO wa_date.

  REFRESH : it_bdc, it_messtab.
  CLEAR   : it_bdc.
  wa_vernr = wa_master_data-rsp_cost_center.

* BDC or Batchjob
  PERFORM open_group.


  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIP2'        '0500',
                      ' '  'IMPR-PRNAM'      p_prnam,
                      ' '  'IMPR-POSID'      u_posid,
                      ' '  'IMPR-GJAHR'      p_gjahr,
                     ' '  'BDC_OKCODE'      '/00'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIP2'        '0600',
                      ' '  'IMPU-POST1'    wa_master_data-req_txt,
                      ' '  'IMPR-IZWEK'    it_invest_reson-inv_reason,
                      ' '  'IMPR-PPRIO'    wa_master_data-priority,
                      ' '  'IMPR-SIZECL'   wa_master_data-scale,
                      ' '  'IMPR-VERNR'
                                     wa_vernr,
*                             wa_master_data-rsp_cost_center,
                      ' '  'IMPR-ABJHR'
                              wa_master_data-desired_start+0(4),
                      ' '  'IMPR-BIJHR'
                             it_variant-completion_date+0(4),
                      ' '  'BDC_OKCODE'      '=PUSH2'.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIP2'        '0600',
              ' '  'IMPR-KOSTL'  wa_master_data-rsp_cost_center,
                      ' '  'IMPR-BUKRS'  wa_master_data-rsp_comp_code,
                      ' '  'IMPR-WERKS'  wa_master_data-plant,
                      ' '  'BDC_OKCODE'      '=PUSH3'.
**  LOOP AT it_ania.
**    wa_aufpr = it_ania-aufpr.
**    wa_l_cnt = wa_l_cnt + 1.
**    IF wa_l_cnt = 1.
  PERFORM make_bdc_rtn USING :
              'X'  'SAPLAIP2'        '0600',
              ' '  'ANIA-ANLKL'  wa_user_field-user03,
              ' '  'ANIA-AKTIV'  wa_date,
              ' '  'BDC_OKCODE'      '=AUFT'.

  PERFORM make_bdc_rtn USING :
              'X'  'SAPLAIPS'        '0191',
              ' '  'ANIA-KOSTL'  wa_master_data-rsp_cost_center,
              ' '  'BDC_OKCODE'      '=RW'.
*      PERFORM make_bdc_rtn USING :
*                  'X'  'SAPLAIPS'        '0310',
*                  ' '  'RAIPS02-ABJHR'
*                              wa_master_data-orig_appr_year,
*                  ' '  'RAIPS_CTRL-KOSTL(01)'
*                              wa_master_data-rsp_cost_center,
*                  ' '  'RAIPS_CTRL-AUFPR(01)' '100', "wa_aufpr,
*                  ' '  'BDC_OKCODE'      '/00'.
**   ELSE.
**      PERFORM make_bdc_rtn USING :
**                  'X'  'SAPLAIPS'        '0310',
**                  ' '  'RAIPS02-ABJHR'
**                              wa_master_data-orig_appr_year,
**                  ' '  'RAIPS_CTRL-ANLKL(02)'  it_ania-anlkl,
**                  ' '  'RAIPS_CTRL-KOSTL(02)'
**                              wa_master_data-rsp_cost_center,
**                  ' '  'RAIPS_CTRL-AKTIV(02)' wa_date,
**                  ' '  'RAIPS_CTRL-AUFPR(02)'
**                                        wa_aufpr.     "it_ania-aufpr,
**    ENDIF.
**
**  ENDLOOP.
*
  PERFORM make_bdc_rtn USING :
                  'X'  'SAPLAIPS'        '0310',
                  ' '  'BDC_OKCODE'      '=RW'.
*
  PERFORM make_bdc_rtn USING :
                    'X'  'SAPLAIP2'        '0600',
                    ' '  'BDC_OKCODE'      '=PUSH4'.
*
  PERFORM make_bdc_rtn USING :
                    'X'  'SAPLAIP2'        '0600',
                    ' '  'IMPR-USR02'      wa_user_field-user02,
                    ' '  'BDC_OKCODE'      '=UPD'.

  PERFORM bdc_transaction USING 'IM12'.

*  CALL TRANSACTION tcode   USING it_bdc
*                           MODE   'E'
*                           UPDATE 'S'
**                    OPTIONS  FROM CTU_PARAMS
*                    MESSAGES INTO it_messtab.

* End of BDC/Job
  PERFORM close_group.



ENDFORM.                    " pi_CHANGE
*&---------------------------------------------------------------------*
*&      Form  GET_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_VARNT  text
*----------------------------------------------------------------------*
FORM get_variant CHANGING c_varnt.
**-- YEARLY PLAN
*  read table it_imzo with key objnr = it_out-objnr
*                              ippos = p_categ.
  READ TABLE it_imzo WITH KEY posnr = it_impr-posnr.
*-----Start #1 wskim
*                              ippos = p_categ.
*-----End
  IF it_imzo-baprz <> 0.
    wa_baprz =  it_imzo-baprz / 100.
  ELSE.
    wa_baprz = 1.
  ENDIF.
  CLEAR : c_varnt.
  READ TABLE it_variant_to_version
            WITH KEY appr_year    = p_gjahr
                     plan_version = p_versn.                "'000'.
  IF sy-subrc = 0.
    MOVE it_variant_to_version-appreqvrnt  TO c_varnt.
*---Overall
    READ TABLE it_plan_tot WITH KEY appreqvrnt = c_varnt.
    IF sy-subrc = 0.
      gt_out-overall =  it_plan_tot-investment_costs *  wa_baprz.
    ENDIF.
  ELSE.
    MOVE '  '  TO  c_varnt.
  ENDIF.
  LOOP AT it_plan_year WHERE appreqvrnt = c_varnt.
    CASE  it_plan_year-fiscal_year.
      WHEN wa_year1.
        gt_out-year1 = it_plan_year-investment_costs * wa_baprz.
      WHEN wa_year2.
        gt_out-year2 = it_plan_year-investment_costs * wa_baprz.
      WHEN wa_year3.
        gt_out-year3 = it_plan_year-investment_costs * wa_baprz.
      WHEN wa_year4.
        gt_out-year4 = it_plan_year-investment_costs * wa_baprz.
      WHEN wa_year5.
        gt_out-year5 = it_plan_year-investment_costs * wa_baprz.
      WHEN wa_year6.
        gt_out-year6 = it_plan_year-investment_costs * wa_baprz.
      WHEN wa_year7.
        gt_out-year7 = it_plan_year-investment_costs * wa_baprz.
      WHEN wa_year8.
        gt_out-year8 = it_plan_year-investment_costs * wa_baprz.
      WHEN wa_year9.
        gt_out-year9 = it_plan_year-investment_costs * wa_baprz.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " GET_VARIANT
*&---------------------------------------------------------------------*
*&      Form  FUND_step
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM fund_step USING    u_posid   u_create.

  REFRESH : it_bdc, it_messtab.
  CLEAR   : it_bdc, it_messtab.
  CLEAR   : wa_f_date, wa_t_date, wa_type.

*  CONCATENATE  '01' '01' wa_master_data-orig_appr_year
*                                              INTO wa_f_date.
*
*  CONCATENATE it_variant-completion_date+4(2)
*              it_variant-completion_date+6(2)
*              it_variant-completion_date+0(4) INTO wa_t_date.
*----
  CONCATENATE  wa_master_data-desired_start+4(2)
               wa_master_data-desired_start+6(2)
               wa_master_data-desired_start+0(4)  INTO wa_f_date.

  CONCATENATE  wa_user_field-user09_date+4(2)
               wa_user_field-user09_date+6(2)
               wa_user_field-user09_date+0(4)     INTO wa_t_date.
*--------*
*---replace 2003.11.19 fund type u_posid+0(1).
*  IF u_posid+0(1) = 'P'.
*    wa_type = 'CAPEX'.
*  ELSEIF u_posid+0(1) = 'C'.
*    wa_type = 'GEN'.
*  ENDIF.

* BDC or Batchjob
  PERFORM open_group.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLFM52'        '0100',
                      ' '  'IFMFINCODE-FIKRS'
                                   wa_master_data-rsp_comp_code,
                      ' '  'IFMFINCODE-FINCODE' u_posid+1(10),
                      ' '  'BDC_OKCODE'      '/00'.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLFM52'        '0200',
                      ' '  'IFMFINCODE-BEZEICH'
                                        wa_master_data-req_txt+0(20),
                      ' '  'IFMFINCODE-BESCHR'   wa_master_data-req_txt,
*                      ' '  'IFMFINCODE-DATAB'    wa_f_date,
*                      ' '  'IFMFINCODE-DATBIS'   wa_t_date,
                      ' '  'IFMFINCODE-DATAB'
                                          wa_f_date,
                      ' '  'IFMFINCODE-DATBIS'
                                          wa_t_date,
                      ' '  'IFMFINCODE-TYPE'   u_posid+0(1), " 'CAPEX',
                      ' '  'BDC_OKCODE'      '=SICH'.


* create
  IF u_create = 'X'.
    PERFORM bdc_transaction USING 'FM5I'.
  ELSE.
* change
    PERFORM bdc_transaction USING 'FM5U'.
  ENDIF.

* BDC or Batchjob
  PERFORM close_group.

*  READ TABLE it_messtab INDEX 1.
*  IF sy-subrc <> 0.
*    MOVE 'Created'   TO gt_out-proc5.
*    MOVE 'A'   TO gt_out-status5.
*  ELSE.
*    IF it_messtab-msgtyp = 'S'.
*      MOVE 'Created'   TO gt_out-proc5.
*      MOVE 'A'   TO gt_out-status5.
*    ELSE.
*      MOVE 'BDC Error' TO gt_out-proc5.
*      MOVE 'Q'   TO gt_out-status5.
*    ENDIF.
*  ENDIF.
*

ENDFORM.                    " FUND_step
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
*&---------------------------------------------------------------------*
*&      Form  create_pi_plan_bdc
*&---------------------------------------------------------------------*
FORM create_pi_plan_bdc USING    u_posid u_varnt u_objnr.
**-- YEARLY PLAN
  READ TABLE it_imzo WITH KEY objnr = u_objnr.
*-----Start #1 wskim
*                              ippos = p_categ.
*-----End
  IF it_imzo-baprz <> 0.
    wa_baprz =  it_imzo-baprz / 100.
  ELSE.
    wa_baprz = 1.
  ENDIF.

*=if inv.program use budget category.
  SORT it_plan_year BY fiscal_year ASCENDING.
  CLEAR wa_d_cnt.
  DESCRIBE TABLE it_plan_year LINES wa_d_cnt.
  CHECK wa_d_cnt > 0.
*---
* IF wa_capex = 'X'.   "USE CATEGORY
  IF p_categ = '1'.
    CLEAR wa_cnt.
    wa_cnt = wa_cnt + 1.

    REFRESH : it_bdc, it_messtab.
    CLEAR   : it_bdc, it_messtab.

    PERFORM make_bdc_rtn USING :
             'X'  'SAPMKBUD'        '0800',
             ' '  'IMTP-PRNAM'      p_prnam,
             ' '  'IMPR-POSID'      u_posid,
             ' '  'IMPR-GJAHR'      p_gjahr,
             ' '  'RAIP3-VERSN'     p_versn,                "'0',
             ' '  'BDC_OKCODE'      '/00'.
* budget category
    IF p_categ = '1'.
      PERFORM make_bdc_rtn USING :
          'X'  'SAPLAIPA'        '0400',
          ' '  'MARKER(01)'      'X',
          ' '  'BDC_OKCODE'      '=TAKE'.
    ELSE.
      PERFORM make_bdc_rtn USING :
          'X'  'SAPLAIPA'        '0400',
          ' '  'MARKER(02)'      'X',
          ' '  'BDC_OKCODE'      '=TAKE'.
    ENDIF.
* yearly plan
    LOOP AT it_plan_year WHERE  appreqvrnt = u_varnt.
      CLEAR wa_amt.
      IF p_categ = '1'.     "category = 1
        IF r_add = 'X'.
          READ TABLE it_pi_plan WITH KEY posid = u_posid
                                  gjahr = it_plan_year-fiscal_year.
          IF sy-subrc = 0.
            wa_amt = ceil( it_plan_year-investment_costs
                       + it_pi_plan-plan ).
*            wa_amt =  it_plan_year-investment_costs
*                       + it_pi_plan-plan.
          ELSE.
            wa_amt = ceil( it_plan_year-investment_costs ).
*            wa_amt =  it_plan_year-investment_costs.
          ENDIF.
        ELSEIF r_reset = 'X'.   "reset
          wa_amt = 0.
        ELSEIF r_over = 'X'.    "overwrite
          wa_amt = ceil( it_plan_year-investment_costs ).
*          wa_amt =  it_plan_year-investment_costs .
        ENDIF.
      ELSE.              "category = 2
        IF r_add = 'X'.
          READ TABLE it_pi_plan WITH KEY posid = u_posid
                                  gjahr = it_plan_year-fiscal_year.
          IF sy-subrc = 0.
            wa_amt = ceil( it_plan_year-overhead_costs
                       + it_pi_plan-plan ).
*            wa_amt =  it_plan_year-overhead_costs
*                       + it_pi_plan-plan.
          ELSE.
            wa_amt = ceil( it_plan_year-overhead_costs ).
*            wa_amt =  it_plan_year-overhead_costs .
          ENDIF.
        ELSEIF r_reset = 'X'.   "reset
          wa_amt = 0.
        ELSEIF r_over = 'X'.    "overwrite
          wa_amt = ceil( it_plan_year-overhead_costs ).
*          wa_amt =  it_plan_year-overhead_costs.
        ENDIF.
      ENDIF.
      wa_amt = wa_amt * wa_baprz.
*        CHECK wa_amt <> 0.
      PERFORM make_bdc_rtn USING :
               'X'  'SAPLKBPP'        '0320',
               ' '  'DROPT-PTIME'     it_plan_year-fiscal_year,
               ' '  'BDC_OKCODE'      '=DROT'.
      PERFORM make_bdc_rtn USING :
               'X'  'SAPLKBPP'        '0320',
              ' '  'DROPT-PTIME'     it_plan_year-fiscal_year,
               ' '  'BPDY-WERT1(01)'  wa_amt,
               ' '  'BDC_OKCODE'      '/00'.
    ENDLOOP.
*---Overall
    READ TABLE it_plan_tot WITH KEY  appreqvrnt = u_varnt.
    IF sy-subrc = 0.
      CLEAR wa_amt.
      IF p_categ = '1'.     "category = 1
        IF r_add = 'X'.     "add
          READ TABLE it_pi_plan WITH KEY posid = u_posid
                                          gjahr = '1111'.
          IF sy-subrc = 0.
            wa_amt = ceil( it_plan_tot-investment_costs
                       + it_pi_plan-plan ).
*            wa_amt = ( it_plan_tot-investment_costs
*                       + it_pi_plan-plan ).
          ELSE.
            wa_amt = ceil( it_plan_tot-investment_costs ).
*            wa_amt = ( it_plan_tot-investment_costs ).
          ENDIF.
        ELSEIF r_reset = 'X'.   "reset
          wa_amt = 0.
        ELSEIF r_over = 'X'.    "overwrite
          wa_amt = ceil( it_plan_tot-investment_costs ).
*          wa_amt = ( it_plan_tot-investment_costs ).
        ENDIF.
      ELSE.            "category = 2
        IF r_add = 'X'.     "add
          READ TABLE it_pi_plan WITH KEY posid = u_posid
                                          gjahr = '1111'.
          IF sy-subrc = 0.
            wa_amt = ceil( it_plan_tot-overhead_costs
                       + it_pi_plan-plan ).
*            wa_amt = ( it_plan_tot-overhead_costs
*                       + it_pi_plan-plan ).
          ELSE.
            wa_amt = ceil( it_plan_tot-overhead_costs ).
*            wa_amt = ( it_plan_tot-overhead_costs ).
          ENDIF.
        ELSEIF r_reset = 'X'.   "reset
          wa_amt = 0.
        ELSEIF r_over = 'X'.    "overwrite
          wa_amt = ceil( it_plan_tot-overhead_costs ).
*          wa_amt = ( it_plan_tot-overhead_costs ).
        ENDIF.
      ENDIF.

      wa_amt = wa_amt * wa_baprz.

      PERFORM make_bdc_rtn USING :
              'X'  'SAPLKBPP'        '0320',
              ' '  'DROPT-PTIME'     '0',
              ' '  'BDC_OKCODE'      '=DROT'.
      PERFORM make_bdc_rtn USING :
              'X'  'SAPLKBPP'        '0320',
              ' '  'DROPT-PTIME'     '0',
              ' '  'BPDY-WERT1(01)'  wa_amt,
              ' '  'BDC_OKCODE'      '/00'.
    ENDIF.

    PERFORM make_bdc_rtn USING :
              'X'  'SAPLKBPP'        '0320',
              ' '  'BDC_OKCODE'      '=POST'.


    PERFORM bdc_transaction USING 'IM35'.
*===========NOT USE CATEGORY=======*
  ELSE.    "
    REFRESH : it_bdc, it_messtab.
    CLEAR   : it_bdc, it_messtab.
    PERFORM make_bdc_rtn USING :
                   'X'  'SAPMKBUD'        '0800',
                   ' '  'IMTP-PRNAM'      p_prnam,
                   ' '  'IMPR-POSID'      u_posid,
                   ' '  'IMPR-GJAHR'      p_gjahr,
                   ' '  'RAIP3-VERSN'     p_versn,          " '0',
                   ' '  'BDC_OKCODE'      '/00'.
*
* budget category
    IF p_categ = '1'.
      PERFORM make_bdc_rtn USING :
          'X'  'SAPLAIPA'        '0400',
          ' '  'MARKER(01)'      'X',
          ' '  'BDC_OKCODE'      '=TAKE'.
    ELSE.
      PERFORM make_bdc_rtn USING :
          'X'  'SAPLAIPA'        '0400',
          ' '  'MARKER(02)'      'X',
          ' '  'BDC_OKCODE'      '=TAKE'.
    ENDIF.

    LOOP AT it_plan_year WHERE   appreqvrnt = u_varnt.
      CLEAR wa_amt.
      IF r_add = 'X'.
        READ TABLE it_pi_plan WITH KEY posid = u_posid
                                gjahr = it_plan_year_ar-fiscal_year.
        IF sy-subrc = 0.
          wa_amt = ceil( it_plan_year-investment_costs
                     + it_pi_plan-plan ).
        ELSE.
          wa_amt = ceil( it_plan_year-investment_costs ).
        ENDIF.
      ELSEIF r_reset = 'X'.   "reset
        wa_amt = 0.
      ELSEIF r_over = 'X'.    "overwrite
        wa_amt = ceil( it_plan_year-investment_costs ).
      ENDIF.
      wa_amt = wa_amt * wa_baprz.

      PERFORM make_bdc_rtn USING :
                  'X'  'SAPLKBPP'        '0320',
                  ' '  'DROPT-PTIME'     it_plan_year-fiscal_year,
                  ' '  'BDC_OKCODE'      '=DROT'.
      PERFORM make_bdc_rtn USING :
                  'X'  'SAPLKBPP'        '0320',
                 ' '  'DROPT-PTIME'     it_plan_year-fiscal_year,
                  ' '  'BPDY-WERT1(01)'  wa_amt,
                  ' '  'BDC_OKCODE'      '/00'.
    ENDLOOP.
*---Overall
    READ TABLE it_plan_tot WITH KEY     appreqvrnt = u_varnt.
    IF sy-subrc = 0.
      CLEAR wa_amt.
      IF r_add = 'X'.     "add
        READ TABLE it_pi_plan WITH KEY posid = u_posid
                                        gjahr = '1111'.
        IF sy-subrc = 0.
          wa_amt = ceil( it_plan_tot-investment_costs
                     + it_pi_plan-plan ).
        ELSE.
          wa_amt = ceil( it_plan_tot-investment_costs ).
        ENDIF.
      ELSEIF r_reset = 'X'.   "reset
        wa_amt = 0.
      ELSEIF r_over = 'X'.    "overwrite
        wa_amt = ceil( it_plan_tot-investment_costs ).
      ENDIF.
      wa_amt = wa_amt * wa_baprz.

      PERFORM make_bdc_rtn USING :
                  'X'  'SAPLKBPP'        '0320',
                  ' '  'DROPT-PTIME'     '0',
                  ' '  'BDC_OKCODE'      '=DROT'.
      PERFORM make_bdc_rtn USING :
                  'X'  'SAPLKBPP'        '0320',
                  ' '  'DROPT-PTIME'     '0',
                  ' '  'BPDY-WERT1(01)'  wa_amt,
                  ' '  'BDC_OKCODE'      '/00'.
      PERFORM make_bdc_rtn USING :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'BDC_OKCODE'      '=POST'.

      PERFORM bdc_transaction USING 'IM35'.

    ENDIF.

  ENDIF.

ENDFORM.                    " create_pi_plan_bdc

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*   (not for call transaction using...)                                *
*----------------------------------------------------------------------*
FORM open_group.
  IF session = 'X'.
*   open batchinput group
    CALL FUNCTION 'BDC_OPEN_GROUP'
         EXPORTING
              client = sy-mandt
              group  = 'ZRFII12'
              user   = sy-uname
              keep   = 'X'. "no delete session if finished
*             HOLDDATE = SPACE.  "lock date
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*   end batchinput session                                             *
*   (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
FORM close_group.
  IF session = 'X'.
*   close batchinput group
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
* batch input session
  IF session = 'X'.
    CALL FUNCTION 'BDC_INSERT'
         EXPORTING
              tcode     = tcode
         TABLES
              dynprotab = it_bdc.
* call transaction using
  ELSE.
    CALL TRANSACTION tcode USING it_bdc
                     MODE   p_mode        "error/all/no disp
                     UPDATE cupdate    "sync/async/local
                     MESSAGES INTO it_messtab.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_cf_amount
*&---------------------------------------------------------------------*
FORM get_cf_amount USING    u_posid.
  CHECK p_cf = 'X'.

  REFRESH i_bpja.  CLEAR i_bpja.
* read position infomation
  READ TABLE it_impr WITH KEY posid = u_posid.

* select carry forward amount
*-----start #1 wskim
*  SELECT SINGLE * FROM bpge
*     WHERE lednr = '0001'
*       AND objnr  = it_impr-objnr
*       AND trgkz  = 'B'   "C/F
*       AND wrttp  = '47'.  "budget
  SELECT SUM( wtges ) INTO bpge-wtges FROM bpge
     WHERE lednr = '0001'
       AND objnr  = it_impr-objnr
       AND trgkz  = 'B'   "C/F
       AND wrttp  = '47'.  "budget

*-----end
  IF sy-subrc = 0.
    SORT it_plan_tot BY appreqvrnt ASCENDING.
* calculate plan = AR plan - C/F budget
    LOOP AT it_plan_tot WHERE  appreqvrnt = wa_varnt.
      it_plan_tot-investment_costs =
                  it_plan_tot-investment_costs - bpge-wtges.
      MODIFY it_plan_tot.
    ENDLOOP.
  ENDIF.

* select carry forward amount
*-----start #1 wskim
*  SELECT * INTO TABLE i_bpja FROM bpja
*     WHERE lednr = '0001'
*       AND objnr  = it_impr-objnr
*       AND trgkz  = 'B'   "C/F
*       AND wrttp  = '47'.  "budget
  SELECT gjahr SUM( wtjhr ) INTO TABLE i_bpja
    FROM bpja
     WHERE lednr = '0001'
       AND objnr  = it_impr-objnr
       AND trgkz  = 'B'   "C/F
       AND wrttp  = '47'
       GROUP by gjahr.
*----end

* calculate plan = AR plan - C/F budget
* if amount is negative... then '0' and carry forward...
  DATA: lc_amt LIKE bpja-wtjhr.
  CLEAR lc_amt.
*--delete not varant.
  LOOP AT it_plan_year.
    IF it_plan_year-appreqvrnt <> wa_varnt.
      DELETE it_plan_year.
    ENDIF.
  ENDLOOP.

*==============*
  CLEAR wa_cnt.
  DESCRIBE TABLE i_bpja LINES wa_cnt.
************************************************
  IF wa_cnt >  0.
*---2004/01/12 year setting.------------------------*
    CALL FUNCTION 'CONVERSION_EXIT_POSID_INPUT'
         EXPORTING
              input  = u_posid+0(3)
         IMPORTING
              output = wa_posid.

    SELECT  SINGLE abjhr bijhr
    INTO  (wa_min, wa_max)
    FROM impr
    WHERE gjahr = p_gjahr
    AND   posid = wa_posid.

    wa_year_cnt = wa_max - wa_min + 1.
    wa_f_gjahr = wa_min - 1.

    DO wa_year_cnt TIMES.
      ADD 1 TO wa_f_gjahr.
      READ TABLE it_plan_year WITH KEY appreqvrnt = wa_varnt
                                       fiscal_year = wa_f_gjahr.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      MOVE wa_varnt   TO it_plan_year-appreqvrnt.
      MOVE wa_f_gjahr TO it_plan_year-fiscal_year.
      it_plan_year-investment_costs = 0.
      it_plan_year-overhead_costs = 0.
      it_plan_year-revenue = 0.

      APPEND it_plan_year.
      CLEAR : it_plan_year.
    ENDDO.

*================*
    SORT it_plan_year BY appreqvrnt fiscal_year ASCENDING.
    SORT i_bpja  BY gjahr  ASCENDING.

    DATA : wa_invest_costs LIKE
               bapiappreqplanyearmulti-investment_costs,
           wa_invest_costs_tot LIKE
               bapiappreqplanyearmulti-investment_costs,
           wa_invest_costs_last LIKE
               bapiappreqplanyearmulti-investment_costs,
           wa_last LIKE
               bapiappreqplanyearmulti-investment_costs,
           wa_last_tot LIKE
               bapiappreqplanyearmulti-investment_costs,
           wa_re   LIKE
               bapiappreqplanyearmulti-investment_costs,
           wa_re1   LIKE
               bapiappreqplanyearmulti-investment_costs.
    DATA : wa_year111(4).

    CLEAR : wa_invest_costs,  wa_invest_costs_tot, wa_invest_costs_last.
    CLEAR : wa_last, wa_re, wa_last_tot.

    LOOP AT it_plan_year WHERE   appreqvrnt = wa_varnt.
      READ TABLE i_bpja WITH KEY gjahr = it_plan_year-fiscal_year.
*---2004/01/28
      wa_invest_costs =
                     it_plan_year-investment_costs - i_bpja-wtjhr.
      IF wa_invest_costs < 0.
        wa_re = wa_re + wa_invest_costs.
        it_plan_year-investment_costs = 0.
      ELSE.
        it_plan_year-investment_costs = wa_invest_costs.
      ENDIF.
      MODIFY it_plan_year.
      CLEAR wa_invest_costs.
      CLEAR i_bpja-wtjhr.
    ENDLOOP.
  ENDIF.

  CLEAR  : wa_invest_costs, wa_re1.
*====re calculate
*-----Start
  DATA : c_flag.
  CLEAR c_flag.
*-----end
  IF wa_re < 0.
    LOOP AT it_plan_year WHERE   appreqvrnt = wa_varnt.
      wa_invest_costs = it_plan_year-investment_costs
                            + wa_re.
      IF wa_invest_costs < 0.
        it_plan_year-investment_costs = 0.
        wa_re = wa_invest_costs.
        MODIFY it_plan_year.
        c_flag = 'X'.
      ELSE.
        it_plan_year-investment_costs = wa_invest_costs.
        MODIFY it_plan_year.
        c_flag = space.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF wa_re < 0 AND c_flag = 'X'.
    LOOP AT it_plan_year WHERE appreqvrnt = wa_varnt
                           AND fiscal_year = p_gjahr.

      it_plan_year-investment_costs = wa_re.
      MODIFY it_plan_year TRANSPORTING investment_costs
       WHERE fiscal_year = p_gjahr.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " get_cf_amount
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
  REFRESH : it_pi_plan.
  CLEAR   : it_pi_plan.
  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET_BY_VERSN'
       EXPORTING
            posid = u_posid
            prnam = u_prnam
            gjahr = p_gjahr
            versn = p_versn
            categ = p_categ
       TABLES
            out   = it_pi_plan.

ENDFORM.                    " ADD_PI_PLAN
*&---------------------------------------------------------------------*
*&      Form  set_year
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_year.
  DO 9 TIMES.
    IF sy-index = 1.
      wa_year1 = s_fiscal-low.
    ELSEIF sy-index = 2.
      wa_year2 = s_fiscal-low + 1.
    ELSEIF sy-index = 3.
      wa_year3 = s_fiscal-low + 2.
    ELSEIF sy-index = 4.
      wa_year4 = s_fiscal-low + 3.
    ELSEIF sy-index = 5.
      wa_year5 = s_fiscal-low + 4.
    ELSEIF sy-index = 6.
      wa_year6 = s_fiscal-low + 5.
    ELSEIF sy-index = 7.
      wa_year7 = s_fiscal-low + 6.
    ELSEIF sy-index = 8.
      wa_year8 = s_fiscal-low + 7.
    ELSEIF sy-index = 9.
      wa_year9 = s_fiscal-low + 8.
    ENDIF.
  ENDDO.
ENDFORM.                    " set_year
*&---------------------------------------------------------------------*
*&      Form  get_variant1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_VARNT  text
*----------------------------------------------------------------------*
FORM get_variant1 CHANGING c_varnt.
  CLEAR : c_varnt.
  READ TABLE it_variant_to_version
            WITH KEY appr_year    = p_gjahr
                     plan_version = p_versn.                "'000'.
  IF sy-subrc = 0.
    MOVE it_variant_to_version-appreqvrnt  TO c_varnt.
*---Overall
    READ TABLE it_plan_tot WITH KEY appreqvrnt = c_varnt.
    IF sy-subrc = 0.
      gt_out-overall =  it_plan_tot-investment_costs.
    ENDIF.
  ELSE.
    MOVE '  '  TO  c_varnt.
  ENDIF.


ENDFORM.                    " get_variant1
*&---------------------------------------------------------------------*
*&      Form  call_bapi_ar_detail_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM call_bapi_ar_detail1 USING    u_posid.

  REFRESH : it_variant,
            it_plan_tot, it_plan_year,
            it_invest_reson,
            it_org_units,
            it_invest_reson,
            it_variant_to_version,
            it_env_invest.
**---2004/01/12 year setting.------------------------*
*  CALL FUNCTION 'CONVERSION_EXIT_POSID_INPUT'
*       EXPORTING
*            input  = u_posid+0(3)
*       IMPORTING
*            output = wa_posid.
*
*  SELECT  SINGLE abjhr bijhr
*  INTO  (wa_min, wa_max)
*  FROM impr
*  WHERE gjahr = p_gjahr
*  AND   posid = wa_posid.
*  wa_year_cnt = wa_max - wa_min + 1.
*  wa_f_gjahr = wa_min.
*
*  DO wa_year_cnt TIMES.
*    MOVE wa_f_gjahr TO it_plan_year-fiscal_year.
*    APPEND it_plan_year.
*    CLEAR : it_plan_year.
*    ADD 1 TO wa_f_gjahr.
*  ENDDO.
*
*-----------------------------------------------------*
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


ENDFORM.                    " call_bapi_ar_detail_1
