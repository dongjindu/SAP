*&-------------------------------------------------------------------
*& Author                 : Hs.Jeong
*& Creation Date          : 05/11/2003
*& Specification By       : Andy Choi
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  : list for print out slip
*&                This is developed use ALV.
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*& log #1
*& 02/03/2005 wskim        UD1K914199
*& issue # 20050102-002    IM Closing 2004 & open 2005 IM Planning
*&--------------------------------------------------------------------

*
* Don't use this program until Andy resolve issue.
*
REPORT zcfii16  MESSAGE-ID  zmfi.

TYPE-POOLS: slis.
TABLES : impr, ztfi_imfm, bpbk, ztfi_reason,imak,ztfi_im_num.


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

DATA: BEGIN OF it_impr OCCURS 0,
        posnr   LIKE impr-posnr,
        gjahr   LIKE impr-gjahr,
        posid   LIKE impr-posid,
        prnam   LIKE impr-prnam,
        objnr   LIKE impr-objnr,
      END OF it_impr.

DATA: BEGIN OF it_imprcf OCCURS 0,
        posnr   LIKE impr-posnr,
        gjahr   LIKE impr-gjahr,
        posid   LIKE impr-posid,
        prnam   LIKE impr-prnam,
        objnr   LIKE impr-objnr,
      END OF it_imprcf.

DATA: BEGIN OF it_impu OCCURS 0,
        posnr   LIKE impu-posnr,
        gjahr   LIKE impu-gjahr,
        post1   LIKE impu-post1,
      END OF it_impu.

DATA: BEGIN OF it_cfyear OCCURS 0,
        reson   LIKE ztfi_reason-reson,
        posid   LIKE ztfi_imfm-posid,
        belnr   LIKE bpej-belnr,
        gjahr   LIKE ztfi_imfm-gjahr,
        wtjhr   LIKE ztfi_imfm-tot,
      END OF it_cfyear.

DATA: BEGIN OF it_cftot OCCURS 0,
        reson   LIKE ztfi_reason-reson,
        posid   LIKE ztfi_imfm-posid,
*-----Start # 1 wskim
        belnr   LIKE bpej-belnr,
*-----End
*       GJAHR   LIKE ZTFI_IMFM-GJAHR,
        wtjhr   LIKE ztfi_imfm-tot,
        cfjhr   LIKE ztfi_imfm-tot,
      END OF it_cftot.

DATA: BEGIN OF gt_out OCCURS 0,
        posnr   LIKE impr-posnr,
        posid   LIKE impr-posid,
        gjahr   LIKE impr-gjahr,
        post1   LIKE impu-post1,
        prnam   LIKE impr-prnam,
*-----Start # 1 wskim
        icon(4),
        belnr   LIKE bpej-belnr,
*-----End
        reson   LIKE ztfi_reason-reson,
*        amt     LIKE bpja-wtjhr,
        tot     LIKE ztfi_imfm-tot,
        cfamt   LIKE ztfi_imfm-tot,
        bdc(10),
        chkbox TYPE c,
        light   TYPE c,
        tabcolor     TYPE slis_t_specialcol_alv,
      END OF gt_out.
*------
*DATA : it_pi_budget LIKE zfi_pi_budget OCCURS 0 WITH HEADER LINE.

DATA : gt_pi_budget LIKE zfi_pi_budget OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_imakt OCCURS 0.
        INCLUDE STRUCTURE imakt.
DATA : END OF it_imakt.
DATA : it_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
*=====WorK area
DATA : wk_cnt TYPE i,
       wa_cnt TYPE i,
       wa_d_cnt TYPE i,
       wa_gjahr LIKE impr-gjahr,
       wa_posid LIKE impr-posid,
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
       wa_conver_date LIKE sy-datum,
       wa_datfm  LIKE usr01-datfm,
       wa_appreqvrnt  LIKE bapiappreqvarntassignmulti,
       wc_amt(15)  TYPE c, "  LIKE BPDY-WERT1.
       wi_amt     TYPE i, "  for numeric conversion
       wa_text(100),      "  for sgtext
       wa_ava         LIKE  bpdy-wert1,
       wa_type  LIKE ifmfincode-type,
       wa_aufnr LIKE aufk-aufnr,
       wa_capex LIKE imtp-capex,
       wa_vernr LIKE impr-vernr,
       wa_plan_tot-overhead_costs LIKE
              bapiappreqplantotalmulti-overhead_costs,
       wa_post1 LIKE impu-post1,
       wa_belnr  LIKE ztfi_imfm-belnr.
*====FOR BDC
DATA : it_bdc      LIKE bdcdata OCCURS 0 WITH HEADER LINE.

DATA:  it_messtab  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_err OCCURS 0,
          posid  LIKE  impr-posid,
          msgid  LIKE  bdcmsgcoll-msgid,
          msgv1  LIKE  bdcmsgcoll-msgv1,
       END OF it_err.

DATA : tcode LIKE tstc-tcode,
       a_gjahr  LIKE bpja-gjahr,
       wa_seq LIKE ztfi_im_num-seq.

DATA:   it_reason LIKE ztfi_reason OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_bpej OCCURS 0,
        objnr    LIKE bpej-objnr,      "object number
        vorga    LIKE bpej-vorga,      "amount type
        gjahr    LIKE bpej-gjahr,      "year
        wtjhr    LIKE bpej-wtjhr,      "amount
        sgtext   LIKE bpbk-sgtext,     "reason code, text
        bldat    LIKE bpbk-bldat,
*-----Start #1 wskim
        belnr    LIKE bpej-belnr,
*-----End
      END OF it_bpej.

DATA: BEGIN OF it_bpeg OCCURS 0,
        objnr    LIKE bpeg-objnr,      "object number
        vorga    LIKE bpeg-vorga,      "amount type
        wtges    LIKE bpeg-wtges,      "amount
        sgtext   LIKE bpbk-sgtext,     "reason code, text
      END OF it_bpeg.
RANGES: r_objnr FOR impr-objnr,
        r_vorga FOR bpbk-vorga,
        r_wrttp FOR bpeg-wrttp.
*-----Start #1 wskim
*---// Constants
CONSTANTS: c_icon_equal(4)          VALUE icon_green_light,
           c_icon_diff(4)           VALUE icon_yellow_light,
           c_icon_err(4)            VALUE icon_red_light,
           c_icon_none(4)           VALUE icon_light_out.
*-----End
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
SELECT-OPTIONS : s_prnam  FOR  impr-prnam OBLIGATORY.
PARAMETERS : p_gjahr  LIKE  imtp-gjahr OBLIGATORY MEMORY ID gjr.

SELECT-OPTIONS : s_bldat  FOR  bpbk-bldat OBLIGATORY.
SELECT-OPTIONS : p_rc     FOR  ztfi_reason-reson
*OBLIGATORY
*                            default '11'
                            MATCHCODE OBJECT zhfi_reason.

PARAMETERS p_mode LIKE ctu_params-dismode DEFAULT 'N'.
"A: show all dynpros
"E: show dynpro on error only
"N: do not display dynpro

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
  s_posid   FOR   impr-posid,
  s_kostl   FOR   impr-kostl.

SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS :
  p_layout LIKE disvariant-variant.   "LAYOUT
SELECTION-SCREEN END OF BLOCK b2.

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
  CLEAR wa_gjahr.
*---------------------------------------------------------------------
*    M   A   I   N
*
*---------------------------------------------------------------------
END-OF-SELECTION.
  CLEAR wa_gjahr.
  wa_gjahr = p_gjahr + 1.
* ==> 1. select data from db
  PERFORM select_data.
  PERFORM select_cfdone.
  PERFORM fill_display.

  IF gt_out[] IS INITIAL.
    MESSAGE s000(zmfi) WITH 'No Data'.
    EXIT.
  ENDIF.

  PERFORM build_field_category
   USING :
    'POSID'     'Position'     '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
    'POST1'     'Description'  '40' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*-----Start wskim
    'ICON'      'Status'       '6'  ' ' ' '  ' '  ' '  '  ' '  ' ,
    'BELNR'     'Doc.Num'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*-----End
    'RESON'     'Reason'       '6' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*    'AMT'       'Amount'       '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
    'TOT'       'To be C/F'    '16' ' ' 'R'  ' '  ' '  '  ' '  ' ,
    'CFAMT'     'Processed C/F'  '16' ' ' 'R'  ' '  ' '  '  ' '  ' ,
    'BDC'       'Result'       '6' ' ' 'R'  ' '  ' '  '  ' '  ' .
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

* get reason code for carry forward.
  SELECT * INTO TABLE it_reason FROM ztfi_reason.

* get object number
  r_objnr-option   = 'EQ'.  r_objnr-sign     = 'I'.
  SELECT * FROM impr
    WHERE  gjahr = p_gjahr
      AND  posid IN s_posid
      AND  kostl IN s_kostl.
* check if PI is parent node...
    CHECK impr-posid+7(4) <> 0000.

    MOVE-CORRESPONDING impr TO it_impr.
    APPEND it_impr.
  ENDSELECT.

* fill object ranges
  LOOP AT it_impr.
    r_objnr-low = it_impr-objnr.
    APPEND r_objnr.
  ENDLOOP.

*   PI text
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_impu FROM impu
    FOR ALL ENTRIES IN it_impr
    WHERE posnr = it_impr-posnr.

* fill amount type for carry forward
  r_vorga-sign   = 'I'.  r_vorga-option = 'EQ'.
  r_vorga-low = 'KBUD'. APPEND r_vorga. " origin
  r_vorga-low = 'KBN0'. APPEND r_vorga. " supplement
  r_vorga-low = 'KBR0'. APPEND r_vorga. " return
* Amount Type
  r_wrttp-sign   = 'I'.  r_wrttp-option = 'EQ'.
  r_wrttp-low = '47'.  APPEND r_wrttp.  " PI budget


* select yearly budget amount
  SELECT l~objnr l~vorga l~gjahr l~wtjhr h~sgtext h~bldat
         l~belnr
    INTO TABLE it_bpej
    FROM bpej AS l INNER JOIN bpbk AS h
      ON l~belnr  = h~belnr
    WHERE lednr = '0001'
      AND l~objnr IN r_objnr
      AND l~wrttp IN r_wrttp
      AND l~vorga IN r_vorga
      AND h~bldat IN s_bldat.

  LOOP AT it_bpej.
* check if not carryforward...
    CHECK it_bpej-sgtext(1) <> '9'.

* filter reason code select-option
    CHECK it_bpej-sgtext+1(2) IN p_rc.
    READ TABLE it_impr WITH KEY objnr = it_bpej-objnr.
    CHECK sy-subrc = 0.

    it_cfyear-reson = it_bpej-sgtext+1(2).
    it_cfyear-posid = it_impr-posid.
*-----Start #1 wskim
    it_cfyear-belnr  = it_bpej-belnr.
*-----End
    it_cfyear-gjahr = it_bpej-gjahr.
    it_cfyear-wtjhr = it_bpej-wtjhr.
    COLLECT it_cfyear.
  ENDLOOP.

* create overall amount
  SORT it_cfyear BY reson posid belnr.
  LOOP AT it_cfyear.
    it_cftot-reson = it_cfyear-reson.
*-----Start # 1 wskim
*    AT END OF posid.
    AT END OF belnr.
*-----End
      SUM.
      it_cftot-posid = it_cfyear-posid.
*-----Start # 1 wskim
      it_cftot-belnr = it_cfyear-belnr.
*-----End
      it_cftot-wtjhr = it_cfyear-wtjhr.
      COLLECT it_cftot.
    ENDAT.
  ENDLOOP.

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

  DATA : roll_up.
  CLEAR roll_up.
  CASE r_ucomm.
*   ---------------------------------- processing on double click.
    WHEN '&IC1'.
      READ TABLE gt_out INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'POSID'.
          SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
          SET PARAMETER ID 'IMP' FIELD gt_out-posid.
          SET PARAMETER ID 'GJR' FIELD gt_out-gjahr.
          CALL TRANSACTION 'IM23' AND SKIP FIRST SCREEN.
        WHEN 'AMT'.
          LOOP AT gt_pi_budget WHERE posid = gt_out-posid.
            WRITE : / gt_pi_budget-gjahr,
                      gt_pi_budget-org,
                      gt_pi_budget-supp,
                      gt_pi_budget-ret.
          ENDLOOP.
*          MODIFY  gt_out INDEX rs_selfield-tabindex.
*          rs_selfield-refresh = 'X'.
      ENDCASE.
*===================================*
*---Approval
    WHEN '&PRO'.
*--Next year
      LOOP AT gt_out WHERE chkbox = 'X'.
        IF gt_out-icon = c_icon_equal.
          MESSAGE s000(zmfi) WITH 'Already processed'.
          CONTINUE.
        ENDIF.

        REFRESH : it_err, it_messtab.
        CLEAR   : it_err, it_messtab,wa_belnr.
*-----Start # 1
        PERFORM bdc_process USING gt_out-posid gt_out-belnr '1'
                            CHANGING  wa_belnr wa_text a_gjahr.
*        PERFORM bdc_process USING gt_out-posid '2'.
*        PERFORM bdc_process USING gt_out-posid '3'.
*        PERFORM bdc_process USING gt_out-posid '4'.
*        PERFORM bdc_process USING gt_out-posid '5'.
*        PERFORM bdc_process USING gt_out-posid '6'.
*-----End
        CLEAR : wa_d_cnt.
        DESCRIBE TABLE it_err LINES wa_d_cnt.
        IF wa_d_cnt < 1.
*-----Start # 1
          PERFORM create_cf_cbo USING gt_out a_gjahr wa_belnr
                                      wa_text.
          MOVE c_icon_equal TO gt_out-icon.
*-----End
          MOVE 'OK'     TO gt_out-bdc.
          roll_up = 'X'.
        ELSE.
          MOVE 'Error'  TO gt_out-bdc.
        ENDIF.
        MODIFY  gt_out. " INDEX rs_selfield-tabindex.
      ENDLOOP.
*-----Start # 1
      IF roll_up EQ 'X'.
*Roll up
        SUBMIT zaimbpup AND RETURN
               WITH program  =  gt_out-prnam
               WITH app_year =  a_gjahr   " Approval year  + 1
               WITH budget   =  'X'
               WITH fromleaf =  'X'.
      ENDIF.
*-----End
      rs_selfield-refresh = 'X'.
*      PERFORM SELECT_DATA.
    WHEN '&RET'.
      LOOP AT it_err.
        WRITE : /10(24)  it_err-posid,
                 40(20)  it_err-msgid,
                 64(50)  it_err-msgv1.
      ENDLOOP.
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
*&      Form  GET_PI_BUDGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_IMPR_POSID  text
*      -->P_P_GJAHR  text
*      -->P_IT_IMPR_PRNAM  text
*----------------------------------------------------------------------*
*FORM get_pi_budget USING    u_posid
*                            u_gjahr
*                            u_prnam.
*
*
*
*  CLEAR : it_pi_budget, it_pi_budget[].
*  REFRESH : IT_PI_BUDGET.
*
*  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET'
*       EXPORTING
*            posid = u_posid
*            prnam = u_prnam
*            gjahr = u_gjahr
*       TABLES
*            out   = it_pi_budget.
*
*  LOOP AT it_pi_budget.
*    IF it_pi_budget-gjahr <> '1111'.
*      gt_out-amt = gt_out-amt + it_pi_budget-supp + it_pi_budget-ret.
*    ENDIF.
*    MOVE-CORRESPONDING it_pi_budget TO gt_pi_budget.
*    APPEND gt_pi_budget.
*    CLEAR  gt_pi_budget.
*  ENDLOOP.
*
*ENDFORM.                    " GET_PI_BUDGET
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1098   text
*      -->P_1099   text
*      -->P_1100   text
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
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM bdc_process USING    u_posid u_belnr  u_level
                 CHANGING  wa_belnr wa_text a_gjahr.
*-----Start #1 wskim
*  tcode = 'IM30'.
*  IF u_level = '1'.
*    wa_posid = u_posid+0(3).
*  ELSEIF u_level = '2'.
*    wa_posid = u_posid+0(4).
*  ELSEIF u_level = '3'.
*    wa_posid = u_posid+0(5).
*  ELSEIF u_level = '4'.
*    wa_posid = u_posid+0(6).
*  ELSEIF u_level = '5'.
*    wa_posid = u_posid+0(7).
*  ELSEIF u_level = '6'.
*    wa_posid = u_posid.
*  ENDIF.
*
*
*  PERFORM make_bdc_rtn USING :
*                 'X'  'SAPMKBUD'        '0800',
*                 ' '  'IMTP-PRNAM'      gt_out-prnam,
*                 ' '  'IMPR-POSID'      wa_posid,
*                 ' '  'IMPR-GJAHR'      wa_gjahr,
*                 ' '  'BDC_OKCODE'      '/00'.
*  PERFORM make_bdc_rtn USING :
*                 'X'  'SAPLAIPA'        '0400',
*                 ' '  'MARKER(01) '     'X',
*                 ' '  'BDC_OKCODE'      '=TAKE'.
*
** overall budget...................................................
*  READ TABLE it_cftot WITH KEY posid = u_posid
*                               reson = gt_out-reson.
*  MOVE it_cftot-wtjhr TO wi_amt.
*  wc_amt = wi_amt.
*  CONDENSE   wc_amt NO-GAPS.
*
*  PERFORM make_bdc_rtn USING :
*                 'X'  'SAPLKBPP'        '0320',
*                 ' '  'DROPT-PTIME'     '0',
*                 ' '  'BDC_OKCODE'      '=DROT'.
*  PERFORM make_bdc_rtn USING :
*                 'X'  'SAPLKBPP'        '0320',
*                 ' '  'DROPT-PTIME'     '0',
*                 ' '  'BPDY-WERT1(01)'  wc_amt,
*                 ' '  'BDC_OKCODE'      '/00' .
*
** yearly budget......................................................
*  LOOP AT it_cfyear WHERE posid = u_posid AND reson = gt_out-reson.
*    CHECK it_cfyear-wtjhr <> 0.
*    MOVE it_cfyear-wtjhr TO wi_amt.
*    wc_amt = wi_amt.
*    CONDENSE   wc_amt NO-GAPS.
*
*    PERFORM make_bdc_rtn USING :
*                   'X'  'SAPLKBPP'        '0320',
*                   ' '  'BDC_CURSOR'      'DROPT-PTIME',
*                   ' '  'DROPT-PTIME'     it_cfyear-gjahr,
*                   ' '  'BDC_OKCODE'      '=DROT'.
**
*    PERFORM make_bdc_rtn USING :
*                   'X'  'SAPLKBPP'        '0320',
*                   ' '  'BPDY-WERT1(01)'  wc_amt,
*                   ' '  'BDC_OKCODE'      '/00'.
*  ENDLOOP.
*
*
*
**---text
*  CONCATENATE '9' it_cfyear-reson 'C/F' INTO wa_text.
*  PERFORM make_bdc_rtn USING :
*                 ' '  'BDC_OKCODE'      '=BELE',
*                 'X'  'SAPLKBPP'        '0702',
**                 ' '  'BPDY-BLDAT'      sy-datum,
*                 ' '  'BPDY-SGTXT'       wa_text,
*                 ' '  'BDC_OKCODE'      '=ENTE'.
*
*
** Post..............................................................
*  PERFORM bdc_posting USING   u_posid u_level.
*
*
*  REFRESH : it_bdc.
*  CLEAR   : it_bdc.
*
  tcode = 'IM32'.


  REFRESH : it_bdc, it_messtab.
  CLEAR a_gjahr.

  a_gjahr = p_gjahr + 1.

  PERFORM make_bdc_rtn USING :
                 'X'  'SAPMKBUD'        '0800',
                 ' '  'IMTP-PRNAM'      gt_out-prnam,
                 ' '  'IMPR-POSID'      u_posid, "wa_posid,
                 ' '  'IMPR-GJAHR'      a_gjahr,
                 ' '  'BDC_OKCODE'      '/00'.
*  IF p_ippos = '1'.
  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLAIPA'        '0400',
                 ' '  'MARKER(01) '     'X',
                 ' '  'BDC_OKCODE'      '=TAKE'.
**----GET ANNUAL.
  LOOP AT it_cfyear WHERE posid EQ u_posid
                      AND belnr EQ u_belnr.

    PERFORM get_year_budget USING it_cfyear-posid
                                  it_cfyear-gjahr
                                  it_cfyear-wtjhr
                                  a_gjahr.

  ENDLOOP.
*-----Get Overal
  PERFORM get_overal_budget USING  u_posid gt_out-tot
                            CHANGING wa_text.

  CALL TRANSACTION tcode   USING it_bdc
                           MODE  p_mode
                           UPDATE 'S'
                    MESSAGES INTO it_messtab.

  LOOP AT it_messtab.
    IF it_messtab-msgtyp = 'S'.
      IF it_messtab-msgid = 'BP'.
        IF it_messtab-msgnr = '043'.
          CLEAR wa_belnr.
          wa_belnr = it_messtab-msgv1.
          CONTINUE.
        ENDIF.
      ENDIF.
    ELSE.
      MOVE-CORRESPONDING it_messtab TO it_err.
      MOVE wa_posid     TO  it_err-posid.
      APPEND it_err.
      CLEAR  it_err.
    ENDIF.
  ENDLOOP.

  REFRESH : it_bdc, it_messtab.
  CLEAR   : it_bdc, it_messtab.

ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CREATE_CF_CBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM create_cf_cbo USING u_gjahr u_posid.
*  LOOP AT gt_pi_budget  WHERE posid = u_posid.
*    CLEAR wi_amt.
*    wi_amt = gt_pi_budget-supp + gt_pi_budget-ret.
*    ztfi_imfm-mandt    = sy-mandt.
*    ztfi_imfm-posnr    = gt_out-posnr.
*    ztfi_imfm-ayear    = WA_gjahr.
*    ZTFI_IMFM-PRNAM    = S_PRNAM-LOW.
*    ztfi_imfm-gjahr    = GT_pi_budget-gjahr.
*    ztfi_imfm-gubun    = '9'.
*    ztfi_imfm-seq      = '01'.
*    ztfi_imfm-posid    = u_posid.
*    ztfi_imfm-tot      = wi_amt.
*    ZTFI_IMFM-UNAME    = SY-UNAME.
*    ZTFI_IMFM-ZDATE    = SY-DATUM.
*    MODIFY ztfi_imfm.
*  ENDLOOP.
*ENDFORM.                    " CREATE_CF_CBO
*&---------------------------------------------------------------------*
*&      Form  bdc_posting
*&---------------------------------------------------------------------*
FORM bdc_posting USING    u_posid
                          u_level.
* post.............................................................
  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'BDC_OKCODE'      '=POST'.

  CALL TRANSACTION tcode   USING it_bdc
                           MODE   p_mode   "'N'
                           UPDATE 'S'
                    MESSAGES INTO it_messtab.
  LOOP AT it_messtab.
    IF it_messtab-msgtyp = 'S'.
      IF it_messtab-msgid = 'BP'.
        IF it_messtab-msgnr = '043'.
*          IF u_level = '6'.
*              PERFORM create_cf_cbo USING
*              WA_gjahr  wa_posid.
*          ENDIF.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING it_messtab TO it_err.
    MOVE wa_posid     TO  it_err-posid.
    APPEND it_err.
    CLEAR  it_err.
  ENDLOOP.

ENDFORM.                    " bdc_posting
*&---------------------------------------------------------------------*
*&      Form  fill_display
*&---------------------------------------------------------------------*
FORM fill_display.
* fill display form.
  SORT it_cftot BY posid reson.
  CLEAR   : gt_out, gt_pi_budget, gt_pi_budget[].


  LOOP AT it_cftot.
*--Check C/F exist...
    READ TABLE it_impr WITH KEY posid = it_cftot-posid.
    CHECK sy-subrc = 0.

*--Get C/F Amount
    MOVE it_cftot-wtjhr    TO  gt_out-tot.
    MOVE it_cftot-cfjhr    TO  gt_out-cfamt.
    MOVE it_cftot-reson    TO  gt_out-reson.
*-----Start #1 wskim
    MOVE it_cftot-belnr    TO  gt_out-belnr.
*check already processed or to be processing : display  signal
    CLEAR a_gjahr.
    a_gjahr = p_gjahr + 1.

    SELECT SINGLE * FROM ztfi_imfm
     WHERE posid EQ it_cftot-posid
       AND ayear EQ a_gjahr
       AND gubun EQ '1'
       AND reson EQ '91'
       AND status EQ 'A'
       AND trans EQ it_cftot-belnr .
    IF sy-subrc = 0.
      gt_out-icon = c_icon_equal.
    ELSE.
      gt_out-icon = c_icon_diff.
    ENDIF.
*-----End
    MOVE-CORRESPONDING it_impr TO gt_out.
*---Get PI name
    READ TABLE it_impu WITH KEY posnr = it_impr-posnr.
    IF sy-subrc = 0.
      MOVE it_impu-post1 TO gt_out-post1.
    ENDIF.
*---Get pi budget   supplement + return
*    PERFORM get_pi_budget USING it_impr-posid p_gjahr it_impr-prnam.

*  GT_OUT-AVA = GT_OUT-AMT - GT_OUT-TOT.

    APPEND gt_out.
    CLEAR  gt_out.
  ENDLOOP.


  SORT gt_out BY posid ASCENDING.
ENDFORM.                    " fill_display
*&---------------------------------------------------------------------*
*&      Form  select_cfdone
*&---------------------------------------------------------------------*
FORM select_cfdone.
* check if C/F is done...in next year
* select yearly budget amount
* get object number
  REFRESH: r_objnr.

  r_objnr-option   = 'EQ'.  r_objnr-sign     = 'I'.
  SELECT * FROM impr
    WHERE  gjahr = wa_gjahr
      AND  posid IN s_posid
      AND  kostl IN s_kostl.
* check if PI is parent node...
    CHECK impr-posid+7(4) <> 0000.

    MOVE-CORRESPONDING impr TO it_imprcf.
    APPEND it_imprcf.
  ENDSELECT.

* fill object ranges
  LOOP AT it_imprcf.
    r_objnr-low = it_imprcf-objnr.
    APPEND r_objnr.
  ENDLOOP.


* select c/f amount in next year
  SELECT l~objnr l~vorga l~wtges h~sgtext
    INTO TABLE it_bpeg
    FROM bpeg AS l INNER JOIN bpbk AS h
      ON l~belnr  = h~belnr
          WHERE lednr = '0001'
            AND l~objnr IN r_objnr
            AND l~wrttp IN r_wrttp
            AND l~vorga IN r_vorga.

  LOOP AT it_bpeg.
    IF it_bpeg-sgtext(1) = '9'.
      READ TABLE it_imprcf WITH KEY objnr = it_bpej-objnr.
      CLEAR it_cftot.
*      read table it_cftot with key posid = it_imprcf-posid
*                                   reson = it_bpeg-sgtext+1(2).
      it_cftot-posid = it_imprcf-posid.
      it_cftot-reson = it_bpeg-sgtext+1(2).
      it_cftot-cfjhr = it_bpeg-wtges.
      COLLECT it_cftot.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " select_cfdone
*&---------------------------------------------------------------------*
*&      Form  get_year_budget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CFYEAR_POSID  text
*      -->P_IT_CFYEAR_GJAHR  text
*      -->P_IT_CFYEAR_WTJHR  text
*----------------------------------------------------------------------*
FORM get_year_budget USING    p_posid
                              p_cfyear_gjahr
                              p_cfyear_wtjhr
                              p_a_gjahr.
  DATA : wa_sap_sum(15) TYPE p,
         wa_amt(15).

  CLEAR wa_sap_sum.
*get current budget : t-code im33
  PERFORM get_sap_budget_im33 USING  p_posid
                                     p_cfyear_gjahr
                                     p_a_gjahr
                                     wa_sap_sum.

  wa_amt = p_cfyear_wtjhr + wa_sap_sum.

  TRANSLATE  wa_amt USING ', '.
  CONDENSE   wa_amt NO-GAPS.

  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'DROPT-PTIME'      p_cfyear_gjahr,
                 ' '  'BDC_OKCODE'      '=DROT'.

  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'BPDY-WERT1(01)'  wa_amt,
                 ' '  'BDC_OKCODE'      '/00'.

ENDFORM.                    " get_year_budget
*&---------------------------------------------------------------------*
*&      Form  get_sap_budget_im33
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_POSID  text
*      -->P_P_CFYEAR_GJAHR  text
*      -->P_WA_SAP_SUM  text
*----------------------------------------------------------------------*
FORM get_sap_budget_im33 USING u_posid
                               u_ayear
                               u_a_gjahr
                               la_sap_sum.
  DATA : bp_objnr LIKE bpja-objnr.

  CLEAR bp_objnr.

  SELECT SINGLE * FROM impr WHERE posid EQ u_posid
                       AND gjahr EQ  u_a_gjahr.
  IF sy-subrc = 0.

    CONCATENATE 'IP' impr-posnr INTO bp_objnr.

    SELECT  SINGLE wtjhr INTO la_sap_sum FROM bpja
     WHERE objnr EQ bp_objnr
       AND trgkz EQ 'N'
       AND wrttp EQ '47'
       AND gjahr EQ u_ayear.

  ENDIF.
ENDFORM.                    " get_sap_budget_im33
*&---------------------------------------------------------------------*
*&      Form  get_overal_budget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*      -->P_GT_OUT_TOT  text
*----------------------------------------------------------------------*
FORM get_overal_budget USING    u_posid
                                p_tot
                       CHANGING wa_text.
  DATA : wa_sap_sum(15) TYPE p,
         wa_amt(15).

  CLEAR : wa_sap_sum,wa_text.

  PERFORM get_sap_budget_im33_ov USING u_posid p_gjahr wa_sap_sum.

  wa_amt = p_tot + wa_sap_sum.
  wa_text = text-002.

  TRANSLATE  wa_amt  USING ', '.
  CONDENSE   wa_amt NO-GAPS.
  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'DROPT-PTIME'     '0',
                 ' '  'BDC_OKCODE'      '=DROT'.
  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'DROPT-PTIME'     '0',
                 ' '  'BPDY-WERT1(01)'  wa_amt,
*                ' '  'BDC_OKCODE'      '/00'.
                 ' '  'BDC_OKCODE'      '=BELE'.
*---text
  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLKBPP'        '0702',
                 ' '  'BPDY-SGTXT'       wa_text,
                 ' '  'BDC_OKCODE'      '=ENTE'.
  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'BDC_OKCODE'      '=SAVE'.


ENDFORM.                    " get_overal_budget
*&---------------------------------------------------------------------*
*&      Form  get_sap_budget_im33_ov
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*      -->P_P_GJAHR  text
*----------------------------------------------------------------------*
FORM get_sap_budget_im33_ov USING   u_posid p_gjahr  la_sap_sum.

  DATA : bp_objnr LIKE bpja-objnr.


  CLEAR : bp_objnr.


  SELECT SINGLE * FROM impr WHERE posid EQ u_posid
                       AND gjahr EQ a_gjahr.
  IF sy-subrc = 0.

    CONCATENATE 'IP' impr-posnr INTO bp_objnr.

    SELECT  SINGLE wtges INTO la_sap_sum FROM bpge
     WHERE objnr EQ bp_objnr
       AND trgkz EQ 'N'
       AND wrttp EQ '47'.

  ENDIF.

ENDFORM.                    " get_sap_budget_im33_ov
*&---------------------------------------------------------------------*
*&      Form  create_cf_cbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GJAHR  text
*      -->P_GT_OUT_POSID  text
*      -->P_WA_BELNR  text
*----------------------------------------------------------------------*
FORM create_cf_cbo USING lp_out LIKE gt_out
                         p_a_gjahr
                         p_wa_belnr
                         p_text.

*get number
  PERFORM get_number USING  p_a_gjahr
                            lp_out-posid.

  SELECT SINGLE * FROM imak
    WHERE posid = lp_out-posid.

  CHECK sy-subrc = 0.
*CBO Table ZTFI_IMFM

  MOVE : lp_out-posid      TO ztfi_imfm-posid,
         p_a_gjahr         TO ztfi_imfm-ayear,
         lp_out-gjahr      TO ztfi_imfm-gjahr,
         '1'               TO ztfi_imfm-gubun,
         wa_seq            TO ztfi_imfm-seq,
         lp_out-prnam      TO ztfi_imfm-prnam,
         lp_out-posnr      TO ztfi_imfm-posnr,
*         imak-vkostl       TO ztfi_imfm-kostl,
         imak-waers        TO ztfi_imfm-twaer,
         lp_out-tot        TO ztfi_imfm-tot,
         lp_out-tot        TO ztfi_imfm-wtp01,
         '1'               TO ztfi_imfm-type,
         '91'              TO ztfi_imfm-reson,
         'A'               TO ztfi_imfm-status,
         lp_out-belnr      TO ztfi_imfm-trans,
         sy-uname          TO ztfi_imfm-uname,
         sy-datum          TO ztfi_imfm-ddate,
         sy-datum          TO ztfi_imfm-zdate,
         p_wa_belnr        TO ztfi_imfm-belnr,
         p_text            TO ztfi_imfm-text.

  INSERT ztfi_imfm.

  IF sy-subrc EQ 0.
    WRITE:/ lp_out-posid, p_a_gjahr, wa_seq,
            'Successfully Updated.'.
  ELSE.
    WRITE:/ lp_out-posid, p_a_gjahr, wa_seq,
            'Update failed.'.
  ENDIF.

ENDFORM.                    " create_cf_cbo
*&---------------------------------------------------------------------*
*&      Form  get_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_number USING  p_a_gjahr
                       p_posid.

  CLEAR ztfi_imfm-seq.
  CLEAR wa_seq.

  SELECT SINGLE MAX( seq ) INTO wa_seq
*  FROM ztfi_im_num
  FROM ztfi_imfm
  WHERE posid = p_posid
  AND   gjahr = p_a_gjahr.

  wa_seq = wa_seq + 1.

  MOVE p_posid      TO   ztfi_im_num-posid.
  MOVE p_a_gjahr    TO   ztfi_im_num-gjahr.
  MOVE wa_seq       TO   ztfi_im_num-seq.
  MODIFY ztfi_im_num.

ENDFORM.                    " get_number
