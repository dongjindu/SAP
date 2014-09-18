REPORT zrfii19  MESSAGE-ID  zmfi.
*&-------------------------------------------------------------------
*& Author                 : Hs.Jeong
*& Creation Date          : 06/01/2004
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
*&--------------------------------------------------------------------
TABLES : imak.
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

DATA: it_reason LIKE ztfi_reason OCCURS 0 WITH HEADER LINE.
DATA: it_tab    LIKE ztfi_imfm   OCCURS 0 WITH HEADER LINE.
DATA: it_impu   LIKE impu        OCCURS 0 WITH HEADER LINE.
DATA: it_impr   LIKE impr        OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gt_out OCCURS 0,
        posid   LIKE   impr-posid,
        post1   LIKE   impu-post1,
        gjahr   LIKE   bp_v_ej-gjahr,
        prnam   LIKE   impr-prnam,
        ktext   LIKE   coas-ktext,
        kostl   LIKE   impr-kostl,
        act_txt(10),
        reson   LIKE   ztfi_reason-reson,
        descr   LIKE   ztfi_reason-descr,
        sgtext  LIKE   bpbk-sgtext,
        status_txt(20),
        tot     LIKE   ztfi_imfm-wtp01,
        wtp01   LIKE   ztfi_imfm-wtp01,
        wtp02   LIKE   ztfi_imfm-wtp01,
        wtp03   LIKE   ztfi_imfm-wtp01,
        wtp04   LIKE   ztfi_imfm-wtp01,
*       wtp04(10) TYPE p DECIMALS 2,
        wtp05   LIKE   ztfi_imfm-wtp01,
        wtp06   LIKE   ztfi_imfm-wtp01,
        wtp07   LIKE   ztfi_imfm-wtp01,
        wtp08   LIKE   ztfi_imfm-wtp01,
        wtp09   LIKE   ztfi_imfm-wtp01,
        wtp10   LIKE   ztfi_imfm-wtp01,
        wtp11   LIKE   ztfi_imfm-wtp01,
        wtp12   LIKE   ztfi_imfm-wtp01,
        chkbox TYPE c,
        light   TYPE c,
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
       wa_type LIKE ztfi_reason-type.
*- POSS.ENTRY
DATA : BEGIN OF it_value OCCURS 0,
        reson LIKE ztfi_reason-reson,
        descr LIKE ztfi_reason-descr,
       END OF it_value.


RANGES : r_vorga FOR bpbk-vorga,
         r_wrttp FOR bpeg-wrttp,
         r_gubun FOR ztfi_imfm-gubun,
         r_status FOR ztfi_imfm-status.
*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
TABLES : t001, usr21, ztfi_reason, ztfi_imfm, bpbk, impr.
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
PARAMETERS: p_bukrs LIKE bkpf-bukrs MEMORY ID buk.
*------
SELECT-OPTIONS:
  s_posid   FOR   impr-posid.
PARAMETERS : p_posid(20).
*   s_posnr   FOR   imak-posnr.
PARAMETERS: p_gjahr LIKE impr-gjahr   obligatory MEMORY ID gjr.

*SELECTION-SCREEN SKIP.
*SELECT-OPTIONS:
*  s_cpudt   FOR   bpbk-cpudt.
*
SELECTION-SCREEN END OF BLOCK b0.
*========================*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-020.
SELECT-OPTIONS:
   s_kostl   FOR   impr-kostl,
   s_usnam   FOR   usr21-bname , "DEFAULT sy-uname,
   s_reson   FOR   ztfi_reason-reson MATCHCODE OBJECT zhfi_reason,
   s_prnam   FOR   impr-prnam.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-040.
*--check
PARAMETERS : p_chk1  AS CHECKBOX DEFAULT 'X',
             p_chk2  AS CHECKBOX DEFAULT 'X',
             p_chk3  AS CHECKBOX DEFAULT 'X'.
*             p_chk4  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-050.
*--check
PARAMETERS : p_chk11  AS CHECKBOX DEFAULT 'X',
             p_chk22  AS CHECKBOX DEFAULT 'X',
             p_chk33  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b5.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
PARAMETERS :
  p_layout LIKE disvariant-variant.   "LAYOUT
SELECTION-SCREEN END OF BLOCK b3.

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
*  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.

*  c_pi = 'Position ID'.
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
  PERFORM build_field_category
  USING :
   'POSID'     'Position ID'  '12' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'POST1'     'Description'  '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'GJAHR'     'Year'          '4' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'ACT_TXT'   'Activity'     '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'KOSTL'     'CCtr'         '05' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'RESON'     'Re.Code'      '02' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'DESCR'     'Reason Text'  '16' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'STATUS_TXT'    'Status'   '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*   'SGTEXT'    'Document Text' '30' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*   'WTJHR'     'Amount'       '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'TOT'       'Total'        '16' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP01'     'Jan'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP02'     'Fab'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP03'     'Mar'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP04'     'Apri'         '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP05'     'Mar'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP06'     'Jun'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP07'     'Jul'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP08'     'Aug'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP09'     'Set'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP10'     'Oct'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP11'     'Nov'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'WTP12'     'Dec'          '13' ' ' 'R'  ' '  ' '  '  ' '  ' .
*   'CPUDT'     'CPU Date'     '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*   'BELNR'     'Document'     '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*   'USNAM'     'Create By'    '10' ' ' 'L'  ' '  ' '  '  ' '  ' .
* ==> 6. build sorts info
*  REFRESH gt_sorts.
*  PERFORM build_sort_table
*    USING :
*       '1'    'POSID'   'X'   'X'   '*'.
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

  DATA : wa_posid(20),
         wa_pos TYPE i.
**  SEARCH p_posid FOR ' '. "space.
**  IF sy-subrc = 0.
**    wa_pos = sy-fdpos.
**    CONCATENATE p_posid+0(wa_pos) '%'  INTO wa_posid.
**  else.
**    wa_posid = p_posid.
**  ENDIF.
**
  CONCATENATE p_posid '%'  INTO wa_posid.
*--
  REFRESH :  gt_out, it_tab.
  CLEAR   :  gt_out, it_tab.

  SELECT * INTO TABLE it_reason FROM ztfi_reason.
*---set activity
  PERFORM set_act.
*--set status
  PERFORM set_atatus.
*--2004/03/18
  IF p_posid <> ' '.
      SELECT * INTO TABLE it_tab  FROM ztfi_imfm
      WHERE  ayear = p_gjahr
        AND  posid LIKE wa_posid
        AND  gubun IN r_gubun
*        AND  kostl IN s_kostl
        AND  prnam IN s_prnam
        AND  reson IN s_reson
        AND  status IN r_status
        AND  uname IN s_usnam.
  ELSE.
     SELECT * INTO TABLE it_tab  FROM ztfi_imfm
     WHERE  ayear = p_gjahr
       AND  posid IN s_posid
       AND  gubun IN r_gubun
*       AND  kostl IN s_kostl
       AND  prnam IN s_prnam
       AND  reson IN s_reson
       AND  status IN r_status
       AND  uname IN s_usnam.
  ENDIF.
*----2004/03/18
  LOOP AT it_tab.
    IF it_tab-wtp01 = 0 AND it_tab-wtp02 = 0 AND it_tab-wtp03 = 0 AND
       it_tab-wtp04 = 0 AND it_tab-wtp05 = 0 AND it_tab-wtp06 = 0 AND
       it_tab-wtp07 = 0 AND it_tab-wtp08 = 0 AND it_tab-wtp09 = 0 AND
       it_tab-wtp10 = 0 AND it_tab-wtp11 = 0 AND it_tab-wtp12 = 0.
      DELETE it_tab.
    ENDIF.
  ENDLOOP.

*  perform fix_sign  tables it_tab.

  DESCRIBE TABLE it_tab LINES wa_cnt.
*   PI text
  IF wa_cnt > 0.
    SELECT * INTO TABLE it_impr FROM impr
      FOR ALL ENTRIES IN it_impr
      WHERE posid = it_impr-posid.
    SELECT * INTO TABLE it_impu FROM impu
      FOR ALL ENTRIES IN it_impr
      WHERE posnr = it_impr-posnr.
  ELSE.
    EXIT.
  ENDIF.

  LOOP AT it_tab.
    MOVE-CORRESPONDING it_tab TO gt_out.
*---get pi text
    READ TABLE it_impr WITH KEY posid = it_tab-posid.
    IF sy-subrc = 0.
      READ TABLE it_impu WITH KEY posnr = it_impr-posnr.
      IF sy-subrc = 0.
        gt_out-post1 = it_impu-post1.
      ENDIF.
    ENDIF.
*---get activiry
    PERFORM get_activity USING it_tab-gubun.
*---get reason text
    PERFORM get_reason_txt USING it_tab-gubun it_tab-reson.
*---STATUS TEXT
    IF it_tab-status = 'P'.
      gt_out-status_txt = 'Parked'.
    ELSEIF it_tab-status = 'R'.
      gt_out-status_txt = 'Requested'.
    ELSEIF it_tab-status = 'A'.
      gt_out-status_txt = 'Approved'.
    ELSE.
      gt_out-status_txt = ' '.
    ENDIF.


    APPEND gt_out.
    CLEAR  gt_out.
  ENDLOOP.

  SORT gt_out BY posid ASCENDING.

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
    refresh ct_events.

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
        WHEN 'POSID'.
          SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
          SET PARAMETER ID 'IMP' FIELD gt_out-posid.
          SET PARAMETER ID 'GJR' FIELD p_gjahr.
          CALL TRANSACTION 'IM13' AND SKIP FIRST SCREEN.

        WHEN 'TOT'. "'WTJHR'. " OR 'BELNR'.
          IF gt_out-act_txt = 'Original'.
            SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_gjahr.
            CALL TRANSACTION 'IM33' AND SKIP FIRST SCREEN.
          ELSEIF gt_out-act_txt = 'Supplement'.
            SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_gjahr.
            CALL TRANSACTION 'IM31' AND SKIP FIRST SCREEN.
          ELSEIF gt_out-act_txt = 'Return'.
            SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_gjahr.
            CALL TRANSACTION 'IM39' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
*---------------------------------- switching view type grid or list
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
*&      Form  fill_gt_out
*&---------------------------------------------------------------------*
*       Fill display structure
*----------------------------------------------------------------------*
FORM fill_gt_out.


ENDFORM.                    " fill_gt_out

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
*&      Form  get_activity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_GUBUN  text
*----------------------------------------------------------------------*
FORM get_activity USING    u_gubun.
  CASE u_gubun.
    WHEN '1'.
      gt_out-act_txt = 'Original'.
    WHEN '2'.
      gt_out-act_txt = 'Supplement'.
    WHEN '3'.
      gt_out-act_txt = 'Return'.
*        gt_out-tot  = -1 *  gt_out-tot.
*        gt_out-wtp01 = -1 *  gt_out-wtp01.
*        gt_out-wtp02 = -1 *  gt_out-wtp02.
*        gt_out-wtp03 = -1 *  gt_out-wtp03.
*        gt_out-wtp04 = -1 *  gt_out-wtp04.
*        gt_out-wtp05 = -1 *  gt_out-wtp05.
*        gt_out-wtp06 = -1 *  gt_out-wtp06.
*        gt_out-wtp07 = -1 *  gt_out-wtp07.
*        gt_out-wtp08 = -1 *  gt_out-wtp08.
*        gt_out-wtp09 = -1 *  gt_out-wtp09.
*        gt_out-wtp10 = -1 *  gt_out-wtp10.
*        gt_out-wtp11 = -1 *  gt_out-wtp11.
*        gt_out-wtp12 = -1 *  gt_out-wtp12.
    WHEN '4'.
      gt_out-act_txt = 'Transfer'.
  ENDCASE.
ENDFORM.                    " get_activity
*&---------------------------------------------------------------------*
*&      Form  get_reason_txt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_RESON  text
*----------------------------------------------------------------------*
FORM get_reason_txt USING   u_gubun u_reson.
  READ TABLE it_reason WITH KEY type = u_gubun
                                reson = u_reson.
  IF sy-subrc = 0.
    gt_out-descr = it_reason-descr.
  ELSE.
    gt_out-descr = ' '.
  ENDIF.
ENDFORM.                    " get_reason_txt
*&---------------------------------------------------------------------*
*&      Form  set_act
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_act.
  MOVE : 'I'    TO  r_gubun-sign,
         'EQ'   TO  r_gubun-option.
  IF p_chk1 = 'X'.
    MOVE '1'   TO r_gubun-low.
    APPEND r_gubun.
  ENDIF.
  IF p_chk2 = 'X'.
    MOVE '2'   TO r_gubun-low.
    APPEND r_gubun.
  ENDIF.
  IF p_chk3 = 'X'.
    MOVE '3'   TO r_gubun-low.
    APPEND r_gubun.
  ENDIF.
*  IF p_chk4 = 'X'.
*    MOVE '4'   TO r_gubun-low.
*    APPEND r_gubun.
*  ENDIF.
ENDFORM.                    " set_act
*&---------------------------------------------------------------------*
*&      Form  set_atatus
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_atatus.

  MOVE : 'I'    TO  r_status-sign,
         'EQ'   TO  r_status-option.
  IF p_chk11 = 'X'.
    MOVE 'R'   TO r_status-low.
    APPEND r_status.
  ENDIF.
  IF p_chk22 = 'X'.
    MOVE 'F'   TO r_status-low.
    APPEND r_status.
  ENDIF.
  IF p_chk33 = 'X'.
    MOVE 'A'   TO r_status-low.
    APPEND r_status.
  ENDIF.

ENDFORM.                    " set_atatus
*&---------------------------------------------------------------------*
*&      Form  fix_sign
*&---------------------------------------------------------------------*
*FORM fix_sign tables   P_tab structure ztfi_imfm.
**sign for return
*  loop at p_tab where gubun = '3'.
*    p_tab-wtp01 = p_tab-wtp01 * -1.
*    p_tab-wtp02 = p_tab-wtp02 * -1.
*    p_tab-wtp03 = p_tab-wtp03 * -1.
*    p_tab-wtp04 = p_tab-wtp04 * -1.
*    p_tab-wtp05 = p_tab-wtp05 * -1.
*    p_tab-wtp06 = p_tab-wtp06 * -1.
*    p_tab-wtp07 = p_tab-wtp07 * -1.
*    p_tab-wtp08 = p_tab-wtp08 * -1.
*    p_tab-wtp09 = p_tab-wtp09 * -1.
*    p_tab-wtp10 = p_tab-wtp10 * -1.
*    p_tab-wtp11 = p_tab-wtp11 * -1.
*    p_tab-wtp12 = p_tab-wtp12 * -1.
*    p_tab-tot   = p_tab-tot   * -1.
*    modify p_tab index sy-tabix.
*  endloop.
*
*ENDFORM.                    " fix_sign
