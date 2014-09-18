*&--------------------------------------------------------------------
*& Author                 : HS.Jeong
*& Creation Date          : 06/10/2003
*& Specification By       : hs.jeong
*& Pattern                : Report 1-1
*& Development Request No : UD1K902603
*& Addl documentation     :
*& Description            : PI,I/O Budget/Actual Report
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*& 06/13/2013  T00303       UD1K957358      U1: Apply Archiving
*&--------------------------------------------------------------------
REPORT zrfii01 MESSAGE-ID  zmfi.
TYPE-POOLS: slis, vrm.
INCLUDE <icon>.
INCLUDE <symbol>.
CLASS cl_gui_resources DEFINITION LOAD.


CONSTANTS:
  c_f2code     LIKE sy-ucomm  VALUE '&ETA'.

CONSTANTS:
    c_pln(02)   TYPE c VALUE '01',
    c_borg(02)  TYPE c VALUE '03',
    c_bsup(02)  TYPE c VALUE '04',
    c_bret(02)  TYPE c VALUE '05',
    c_bcur(02)  TYPE c VALUE '06',
    c_bava(02)  TYPE c VALUE '10',
    c_inv(02)   TYPE c VALUE '07',
    c_com(02)   TYPE c VALUE '08',
    c_dp(02)    TYPE c VALUE '09'.

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
      wa_alv_get_info_name(40) TYPE c.
*--- ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.


*DATA: gt_list_top_of_page TYPE slis_t_listheader.

*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
*
TABLES: aufk, impr, fmfctr, ripasw, codia, imak.

DATA: gt_aufk  TYPE TABLE OF aufk WITH HEADER LINE.
DATA: gt_imzo TYPE TABLE OF imzo WITH HEADER LINE.
DATA: it_impr TYPE TABLE OF impr WITH HEADER LINE.

DATA : it_budget LIKE zfi_io_budget OCCURS 0 WITH HEADER LINE.
DATA : it_actual LIKE zfi_io_actual OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gt_out OCCURS 0,
        aufnr         LIKE  aufk-aufnr,
        ktext         LIKE  aufk-ktext,
        akstl         LIKE  aufk-akstl,
*        KOSTL         LIKE  AUFK-KOSTL,
        kostv         LIKE  aufk-kostv,
        user0         LIKE  aufk-user0,

        key(2),
        act(20),
        gjahr        LIKE  imzo-gjahr,
        tot          LIKE  cosp-wtg001,
        before       LIKE  cosp-wtg001,
        last         LIKE  cosp-wtg001,
        year1        LIKE  cosp-wtg001,
        year2        LIKE  cosp-wtg001,
        year3        LIKE  cosp-wtg001,
        year4        LIKE  cosp-wtg001,
        year5        LIKE  cosp-wtg001,
        after        LIKE  cosp-wtg001,
*        POSNR        LIKE  IMPR-POSNR,
        posnr        LIKE  impr-posnr,
        posid        LIKE  impr-posid,
        chkbox       TYPE c,
        light        TYPE c,
*        tabcolor     TYPE slis_t_specialcol_alv,

        akstl_text    TYPE ktext,
        kostv_text    TYPE ktext,
        posid_text    TYPE im_pos_txt,

      END OF gt_out.

DATA: BEGIN OF i_kostv_text OCCURS 0,
        kostl         TYPE kostl,
        ktext         TYPE ktext,
      END OF i_kostv_text.

DATA: BEGIN OF i_posid_text OCCURS 0,
        posid         TYPE im_posid,
        ktext         TYPE im_pos_txt,
        appr_year     TYPE im_gnjhr,
      END OF i_posid_text.

*---Temp
DATA: BEGIN OF gt_itab OCCURS 0,
        aufnr         LIKE  aufk-aufnr,
        ktext         LIKE  aufk-ktext,
        akstl         LIKE  aufk-akstl, "Req.CC
*        KOSTL         LIKE  AUFK-KOSTL, "Res.CC
*        act(10),
*        tot          LIKE  cosp-wtg001,
*        before       LIKE  cosp-wtg001,
*        last         LIKE  cosp-wtg001,
*        year         LIKE  cosp-wtg001,
*        YEAR2        LIKE  cosp-wtg001,
*        YEAR3        LIKE  cosp-wtg001,
*        YEAR4        LIKE  cosp-wtg001,
*        YEAR5        LIKE  cosp-wtg001,
*        after        LIKE  cosp-wtg001,
        user0         LIKE  aufk-user0,
        posnr        LIKE  impr-posnr,
*        posnr        LIKE  impr-posnr,
        posid        LIKE  impr-posid,
*        chkbox       TYPE c,
*        light        TYPE c,
*        tabcolor     TYPE slis_t_specialcol_alv,
        kostv LIKE aufk-kostv,

      END OF gt_itab.
*----i/o assign asset
DATA : BEGIN OF it_asset OCCURS 0,
        eaufn   LIKE  anla-eaufn,
        anln1   LIKE  anla-anln1,
        zujhr   LIKE  anla-zujhr,
       END OF it_asset.
*----for combox
DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.
*---WORK AREA
DATA : wa_t_cnt TYPE i,
       wa_before  LIKE  imzo-gjahr,
       wa_last    LIKE  imzo-gjahr,
       wa_year1   LIKE  imzo-gjahr,
       wa_year2   LIKE  imzo-gjahr,
       wa_year3   LIKE  imzo-gjahr,
       wa_year4   LIKE  imzo-gjahr,
       wa_year5   LIKE  imzo-gjahr,
       wa_after   LIKE  imzo-gjahr,
       wa_before_txt(10),
       wa_after_txt(10).

DATA : wa_before_amt   LIKE  cosp-wtg001,
       wa_after_amt    LIKE  cosp-wtg001.
*---Currenty
DATA :  wa_n_tot          LIKE  cosp-wtg001,
        wa_n_before       LIKE  cosp-wtg001,
        wa_n_last         LIKE  cosp-wtg001,
        wa_n_year1        LIKE  cosp-wtg001,
        wa_n_year2        LIKE  cosp-wtg001,
        wa_n_year3        LIKE  cosp-wtg001,
        wa_n_year4        LIKE  cosp-wtg001,
        wa_n_year5        LIKE  cosp-wtg001,
        wa_n_after        LIKE  cosp-wtg001.
*---Actual
DATA :  wa_a_tot          LIKE  cosp-wtg001,
        wa_a_before       LIKE  cosp-wtg001,
        wa_a_last         LIKE  cosp-wtg001,
        wa_a_year1        LIKE  cosp-wtg001,
        wa_a_year2        LIKE  cosp-wtg001,
        wa_a_year3        LIKE  cosp-wtg001,
        wa_a_year4        LIKE  cosp-wtg001,
        wa_a_year5        LIKE  cosp-wtg001,
        wa_a_after        LIKE  cosp-wtg001.
*---Commitment
DATA :  wa_c_tot          LIKE  cosp-wtg001,
        wa_c_before       LIKE  cosp-wtg001,
        wa_c_last         LIKE  cosp-wtg001,
        wa_c_year1        LIKE  cosp-wtg001,
        wa_c_year2        LIKE  cosp-wtg001,
        wa_c_year3        LIKE  cosp-wtg001,
        wa_c_year4        LIKE  cosp-wtg001,
        wa_c_year5        LIKE  cosp-wtg001,
        wa_c_after        LIKE  cosp-wtg001.
*---Downpayment
DATA :  wa_d_tot          LIKE  cosp-wtg001,
        wa_d_before       LIKE  cosp-wtg001,
        wa_d_last         LIKE  cosp-wtg001,
        wa_d_year1        LIKE  cosp-wtg001,
        wa_d_year2        LIKE  cosp-wtg001,
        wa_d_year3        LIKE  cosp-wtg001,
        wa_d_year4        LIKE  cosp-wtg001,
        wa_d_year5        LIKE  cosp-wtg001,
        wa_d_after        LIKE  cosp-wtg001.
*---WORK
DATA : wa_okcode(4),
       ok_code(4),
       wa_cursor(14),
       wa_line TYPE i,
       wa_pick_line TYPE i,
       wa_chk.

*- U1 Start
DATA: gt_aufk_a TYPE TABLE OF aufk WITH HEADER LINE.
*- U1 End
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE c010.
SELECT-OPTIONS: s_prnam FOR impr-prnam MEMORY ID imt. "OBLIGATORY.
PARAMETERS:  p_ayear LIKE impr-gjahr   MEMORY ID gjr OBLIGATORY
                                      DEFAULT sy-datum+0(4).

PARAMETERS: p_auth(1) TYPE c DEFAULT 'X' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK s1.
*--------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK s3 WITH FRAME TITLE c030.

SELECT-OPTIONS:
  s_aufnr   FOR   codia-aufnr MATCHCODE OBJECT orde,
  s_posid   FOR   impr-posid,
  s_akstl   FOR   aufk-akstl,
  s_kostv   FOR   aufk-kostv.

SELECT-OPTIONS: s_usr02   FOR imak-usr02.
SELECT-OPTIONS: so_ippos  FOR ripasw-ippos.
SELECTION-SCREEN END OF BLOCK s3.
*------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK s4 WITH FRAME TITLE c040.
PARAMETER:            p_gjahr   LIKE bpja-gjahr DEFAULT sy-datum(4).

PARAMETERS : p_actual    AS CHECKBOX.

PARAMETERS :
  p_layout LIKE disvariant-variant.   "LAYOUT
SELECTION-SCREEN END OF BLOCK s4.
*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End
*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_ACT'
      values = it_val.
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
* ==> Change Variant saving type
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
  wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
*wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.
*--title set
  c010 = 'Run Parameter'.
  c030 = 'Select option'.
  c040 = 'Display option'.
  wa_repid = sy-repid.

** for combo box
*  REFRESH : it_val.
*  CLEAR   : it_val.
*  w_line-key  = '01'.
*  w_line-text = 'All'.
*  APPEND w_line TO it_val.
*  w_line-key  = '02'.
*  w_line-text = 'Plan'.
*  APPEND w_line TO it_val.
*  w_line-key  = '03'.
*  w_line-text = 'Ori Bud'.
*  APPEND w_line TO it_val.
*  w_line-key  = '04'.
*  w_line-text = 'Supplement'.
*  APPEND w_line TO it_val.
*  w_line-key  = '05'.
*  w_line-text = 'Return'.
*  APPEND w_line TO it_val.
*  w_line-key  = '06'.
*  w_line-text = 'Current Budget'.
*  APPEND w_line TO it_val.
*  w_line-key  = '07'.
*  w_line-text = 'Actual'.
*  APPEND w_line TO it_val.
*  w_line-key  = '08'.
*  w_line-text = 'Comm'.
*  APPEND w_line TO it_val.
*  w_line-key  = '09'.
*  w_line-text = 'Downpayment'.
*  APPEND w_line TO it_val.
*  w_line-key  = '10'.
*  w_line-text = 'Residual'.
*  APPEND w_line TO it_val.

*---------------------------------------------------------------------
*    M   A   I   N
*
*---------------------------------------------------------------------
START-OF-SELECTION.

* ==> 1. select data from db
  PERFORM set_year.
  PERFORM select_data.

END-OF-SELECTION.

  IF gt_out[] IS INITIAL.
    MESSAGE s000(zmfi) WITH 'No found data '.
    EXIT.
  ENDIF.

* ==> 5. build field category
  PERFORM build_field_category
  USING :
   'AUFNR'     'Order'       '12' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'KTEXT'     'Description' '15' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'AKSTL'     'Req.CC'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'AKSTL_TEXT'     'Req.CC'      '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'KOSTV'     'Res.CC'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'KOSTV_TEXT'     'Res.CC'      '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'USER0'     'Applicant'   '15' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'KEY'       'Key'         '02' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'ACT'       'Activity'    '16' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'TOT'       'Total'       '15' ' ' 'R'  ' '  ' '  ' ' ' ' ,
   'BEFORE'    wa_before_txt '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'LAST'      wa_last       '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR1'     wa_year1      '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR2'     wa_year2      '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR3'     wa_year3      '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR4'     wa_year4      '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR5'     wa_year5      '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'AFTER'     wa_after_txt  '15' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'POSID'     'Position ID' '15' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'POSID_TEXT' 'Position ID' '30' ' ' 'L'  ' '  ' '  '  ' '  ' .

* ==> 6. build sorts info
*  REFRESH gt_sorts.
*  PERFORM build_sort_table
*    USING :
*       '1'    'AUFNR'   'X'   'X'   '*'.
* ==> 2. set variant default
  PERFORM set_variant CHANGING wa_var.
* ==> 3. set layout for alv style
  PERFORM set_layout CHANGING gs_layout.
* ==> 4. set events for alv
  PERFORM set_events CHANGING gt_events.
*===> 5. set event for top-of-page grid.
  PERFORM set_build_event.
*===>
  PERFORM comment_build USING  w_top_of_page[].

* ==> 7. call function display alv.

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
*     it_events                = gt_events[]
      it_events                = w_eventcat[]
      is_print                 = gs_prnt
*     IT_EVENT_EXIT            =
*     I_SCREEN_START_COLUMN    = 10
*     I_SCREEN_START_LINE      = 2
*     I_SCREEN_END_COLUMN      = 80
*     I_SCREEN_END_LINE        = 23
    TABLES
      t_outtab                 = gt_out.

***********************************************************************
*

*&---------------------------------------------------------------------
*&      Form  select_data
*&---------------------------------------------------------------------
FORM select_data.
*--Get Order
  SELECT * INTO TABLE gt_aufk FROM aufk
    WHERE aufnr IN s_aufnr
    AND   akstl IN s_akstl
    AND   kostv IN s_kostv
    AND   autyp = '01'           "internal order;so_ippos
    AND   user0  IN s_usr02.
*--GET IMZO

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_aufk.
  ENDIF.
*- U1 End

  CLEAR wa_t_cnt.
  DESCRIBE TABLE gt_aufk LINES wa_t_cnt.
  IF wa_t_cnt > 0.
    SELECT * INTO TABLE gt_imzo FROM imzo
      FOR ALL ENTRIES IN gt_aufk
      WHERE objnr = gt_aufk-objnr.
  ENDIF.
*---GET IMPR
  CLEAR wa_t_cnt.
  DESCRIBE TABLE gt_imzo LINES wa_t_cnt.
  IF wa_t_cnt > 0.
    SELECT * INTO TABLE it_impr FROM impr
      FOR ALL ENTRIES IN gt_imzo
      WHERE posnr = gt_imzo-posnr
      AND   posid IN s_posid
      AND   gjahr = gt_imzo-gjahr
      AND   prnam IN s_prnam.
  ENDIF.
*====*
  CLEAR gt_out.
  DATA: l_idx LIKE sy-tabix.

  SORT gt_imzo BY objnr gjahr.
  SORT  it_impr BY posnr gjahr.
  LOOP AT gt_aufk.
    l_idx = sy-tabix.
    READ TABLE gt_imzo WITH KEY objnr = gt_aufk-objnr
                                gjahr = p_ayear BINARY SEARCH.

    IF sy-subrc <> 0.
      DELETE gt_aufk INDEX l_idx.
    ELSE.
      READ TABLE it_impr WITH KEY posnr = gt_imzo-posnr
                                  gjahr = gt_imzo-gjahr BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE gt_aufk INDEX l_idx.
      ELSE.
        MOVE-CORRESPONDING gt_aufk TO gt_itab.
        MOVE gt_imzo-posnr TO gt_itab-posnr.
        MOVE it_impr-posid TO gt_itab-posid.
        APPEND gt_itab.
      ENDIF.
    ENDIF.
    CLEAR  gt_itab.
  ENDLOOP.

*---make gt_out by activity
  PERFORM make_output_structure.
  CLEAR wa_chk.

  PERFORM fill_text.

  PERFORM fill_output.

ENDFORM.                    " select_data

*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM alv_event_pf_status_set USING rt_extab TYPE slis_t_extab.
                                                            "#EC *
  IF wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    SET PF-STATUS 'STANDARD_GRID' . " EXCLUDING rt_extab.
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
      CASE rs_selfield-fieldname.
        WHEN 'AUFNR'.
          SET PARAMETER ID 'ANR' FIELD gt_out-aufnr.
          CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
        WHEN 'ACT'.
          IF gt_out-key = '02'.
            SET PARAMETER ID 'ANR' FIELD gt_out-aufnr.
            CALL TRANSACTION 'KO13' AND SKIP FIRST SCREEN.
          ELSEIF gt_out-key = '03'.
            SET PARAMETER ID 'ANR' FIELD gt_out-aufnr.
            CALL TRANSACTION 'KO23' AND SKIP FIRST SCREEN.
          ELSEIF gt_out-key = '04'.
            SET PARAMETER ID 'ANR' FIELD gt_out-aufnr.
            CALL TRANSACTION 'KO25' AND SKIP FIRST SCREEN.
          ELSEIF gt_out-key = '05'.
            SET PARAMETER ID 'ANR' FIELD gt_out-aufnr.
            CALL TRANSACTION 'KO27' AND SKIP FIRST SCREEN.
          ELSEIF gt_out-key =  '07' OR gt_out-key = '08' OR
                 gt_out-key = '09'.
            SET PARAMETER ID 'IMT' FIELD s_prnam-low.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_ayear.
            CALL TRANSACTION 'ZIMR' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'POSID'.
          SET PARAMETER ID 'IMT' FIELD s_prnam-low.
          SET PARAMETER ID 'IMP' FIELD gt_out-posid.
          SET PARAMETER ID 'GJR' FIELD p_ayear.
          SET PARAMETER ID 'IPPOS' FIELD so_ippos-low.
          CALL TRANSACTION 'ZIMR' AND SKIP FIRST SCREEN.
*----- Issue No FI-20040716-001
*----- Changed by BSBAE
*----- Changed on 2004.08.04
*----- Start
        WHEN 'BEFORE' OR 'TOT' OR 'LAST' OR 'AFTER' OR
             'YEAR1' OR 'YEAR2' OR 'YEAR3' OR 'YEAR4' OR
             'YEAR5'.
          DATA: lw_pgm(30).

          CONCATENATE 'GP5RHGZCGK9AUROXBPTS86Z3YF3' sy-mandt(3)
                 INTO lw_pgm.

          SUBMIT (lw_pgm)
            WITH $6-kokrs = 'H201'
            WITH _6ordgrp-low = gt_out-aufnr
             AND RETURN.
*----- End
      ENDCASE.
*--
    WHEN '&ASSET'.
      REFRESH : it_asset.
      CLEAR   : it_asset.
      LOOP AT gt_out WHERE chkbox = 'X'.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE it_asset
        FROM anla
        WHERE bukrs  = 'H201'
*        AND   zujhr  = p_ayear
        AND   eaufn  = gt_out-aufnr.
      ENDLOOP.
      CLEAR wa_t_cnt.
      DESCRIBE TABLE it_asset LINES wa_t_cnt.
      IF wa_t_cnt > 0.
        CALL SCREEN 9000 STARTING AT 22 04  ENDING AT 100 24 .
      ENDIF.
*   ---------------------------------- switching view type grid or list
    WHEN 'LIST' OR 'GRID'.
      PERFORM switch_list_or_grid USING r_ucomm.
  ENDCASE.

  CHECK r_ucomm EQ 'LIST' OR
        r_ucomm EQ 'GRID'.

  rs_selfield-exit = 'X'.

ENDFORM.                    "alv_event_user_command
*&---------------------------------------------------------------------
*&      Form  set_variant
*&---------------------------------------------------------------------
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

*&---------------------------------------------------------------------
*&      Form  set_events
*&---------------------------------------------------------------------
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

ENDFORM.                    " f01_set_evts


*&---------------------------------------------------------------------
*&      Form  set_layout
*&---------------------------------------------------------------------
FORM set_layout CHANGING cs_layo TYPE slis_layout_alv.

*... Display options
  cs_layo-colwidth_optimize      = space.
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
  cs_layo-lights_fieldname       = ' '.
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
*  cs_layo-coltab_fieldname       = 'TABCOLOR'.
*... others
  cs_layo-list_append            = space.

ENDFORM.                    " set_layout


*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
FORM alv_event_top_of_page.                                 "#EC CALLED
*  WRITE : /(10) 'nvestment Program' , p_prnam.
*          /(10) 'BBBBBBB',  BKPF-BUKRS INVERSE COLOR 1 INPUT ON,
*           (20) 'CCCCCCC',  BKPF-BELNR INPUT ON.
ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
FORM alv_event_top_of_list.                                 "#EC CALLED


ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_page
*---------------------------------------------------------------------*
FORM alv_event_end_of_page.
*  NEW-LINE.
*  ULINE.
*  DATA: l_page(10).
*  WRITE : sy-pagno TO l_page.
*  WRITE: /(120) l_page CENTERED.
*
ENDFORM.                    "alv_event_end_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_list
*---------------------------------------------------------------------*
FORM alv_event_end_of_list.


ENDFORM.                    "alv_event_end_of_list
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM display_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'Z_HYUNDAI_LOGO'
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = w_top_of_page.
ENDFORM.                    " top_of_page

*&---------------------------------------------------------------------
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------
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
*       it_events                = gt_events[]
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
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM build_field_category USING
                                  p_fieldname       " field name
                                  p_title           " field title
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
  IF p_fieldname = 'TOT'.
    ls_fieldcat-emphasize = 'C700'.
  ENDIF.
  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category

*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
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
*&      Form  set_line_color
*&---------------------------------------------------------------------*
FORM set_line_color USING    p_color.
  DATA: ls_fieldcat   TYPE slis_fieldcat_alv,
        lt_color      TYPE slis_t_specialcol_alv,
        ls_color      TYPE slis_specialcol_alv.

  REFRESH lt_color.
  CLEAR   lt_color.
  LOOP AT gt_fieldcat INTO ls_fieldcat.
    ls_color-fieldname = ls_fieldcat-fieldname.
    ls_color-color-col = p_color.
*    "cl_gui_resources=>list_col_positive.
    ls_color-color-int = cl_gui_resources=>list_intensified.
    ls_color-color-inv = 0.
    ls_color-nokeycol  = 'X'.
    APPEND ls_color TO lt_color.
*    gt_out-tabcolor = lt_color.
  ENDLOOP.

ENDFORM.                    " set_line_color
*&---------------------------------------------------------------------*
*&      Form  build_field_category1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0634   text
*      -->P_0635   text
*      -->P_0636   text
*      -->P_0637   text
*      -->P_0638   text
*      -->P_0639   text
*      -->P_0640   text
*      -->P_0641   text
*      -->P_0642   text
*----------------------------------------------------------------------*
FORM build_field_category1 USING
                                  p_fieldname       " field name
                                  p_title           " field title
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
ENDFORM.                    " build_field_category1
*&---------------------------------------------------------------------*
*&      Form  GET_IO_BUDGET
*&---------------------------------------------------------------------*
FORM  make_io_budget.

  LOOP AT it_budget.
    CASE gt_out-key.
      WHEN c_pln.       "Plan
        PERFORM move_amt_to_out USING it_budget-gjahr it_budget-plan.
      WHEN c_borg.      "Org
        PERFORM move_amt_to_out USING it_budget-gjahr it_budget-org.
      WHEN c_bsup.      "Supplement
        PERFORM move_amt_to_out USING it_budget-gjahr it_budget-supp.
      WHEN c_bret.      "Ret
        PERFORM move_amt_to_out USING it_budget-gjahr it_budget-ret.
    ENDCASE.

*    modify gt_out.
  ENDLOOP.

ENDFORM.                    " GET_IO_BUDGET
*&---------------------------------------------------------------------*
*&      Form  set_year
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_year.
  CLEAR : wa_before, wa_last, wa_year1, wa_year2, wa_year3,
          wa_year4.
  wa_last  = p_gjahr - 1.
  wa_year1 = p_gjahr.
  wa_year2 = p_gjahr + 1.
  wa_year3 = p_gjahr + 2.
  wa_year4 = p_gjahr + 3.
  wa_year5 = p_gjahr + 4.
  CONCATENATE '~' wa_last  INTO wa_before_txt.
  CONCATENATE wa_year5 '~' INTO wa_after_txt.

ENDFORM.                    " set_year
*&---------------------------------------------------------------------*
*&      Form  GET_IO_ACTUAL
*&---------------------------------------------------------------------*
FORM make_io_actual.

  LOOP AT it_actual.
    CASE gt_out-key.
      WHEN c_inv.       "Actual
        CASE it_actual-wrttp.
          WHEN '04' OR '11'.
            PERFORM move_amt_to_out USING it_actual-gjahr it_actual-tot.
            PERFORM move_amt_to_out USING '1111'          it_actual-tot.
        ENDCASE.

      WHEN c_com.       "Commitment
        CASE it_actual-wrttp.
          WHEN '22' OR '21'.
            PERFORM move_amt_to_out USING it_actual-gjahr it_actual-tot.
            PERFORM move_amt_to_out USING '1111'          it_actual-tot.
        ENDCASE.

      WHEN c_dp.       "Downpayment
        CASE it_actual-wrttp.
          WHEN '12'.
            PERFORM move_amt_to_out USING it_actual-gjahr it_actual-tot.
            PERFORM move_amt_to_out USING '1111'          it_actual-tot.
        ENDCASE.
    ENDCASE.

*---update back to internal table
*    MODIFY gt_out.
  ENDLOOP.

ENDFORM.                    " GET_IO_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING  lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
          l_manager(50),
          l_date(50),
          l_list(50),
          l_dsnam LIKE t024d-dsnam,
          l_h_dsnam LIKE t024d-dsnam,
          l_ldate(10),
          l_hdate(10).
*-------------- HEADER
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-h01.     "HEADER TITLE (H001)
  APPEND ls_line TO lt_top_of_page.

  ls_line-typ  = 'S'.
  ls_line-key  = 'Investment program : '.
  ls_line-info = s_prnam-low.
  APPEND ls_line TO lt_top_of_page.
*--
  ls_line-typ  = 'S'.
  ls_line-key  = 'Approval Year : '.
  ls_line-info = p_ayear.
  APPEND ls_line TO lt_top_of_page.
*--
  ls_line-typ  = 'S'.
  ls_line-key  = 'Order no : '.
  CONCATENATE   s_aufnr-low  ' ~'  s_aufnr-high INTO l_list.
  ls_line-info = l_list.
  APPEND ls_line TO lt_top_of_page.

*
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  set_build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_build_event.
  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'DISPLAY_HEADER'.
  APPEND w_eventcat.
ENDFORM.                    " set_build_event
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DUMMY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_dummy.
  CLEAR : wa_n_tot, wa_n_before, wa_n_last, wa_n_year1, wa_n_year2,
          wa_n_year3, wa_n_year4, wa_n_year5, wa_n_year5,
          wa_n_after.
  CLEAR : wa_a_tot, wa_a_before, wa_a_last, wa_a_year1, wa_a_year2,
          wa_a_year3, wa_a_year4, wa_a_year5, wa_a_year5,
          wa_a_after.
  CLEAR : wa_c_tot, wa_c_before, wa_c_last, wa_c_year1, wa_c_year2,
          wa_c_year3, wa_c_year4, wa_c_year5, wa_c_year5,
          wa_c_after.
  CLEAR : wa_d_tot, wa_d_before, wa_d_last, wa_d_year1, wa_d_year2,
          wa_d_year3, wa_d_year4, wa_d_year5, wa_d_year5,
          wa_d_after.
ENDFORM.                    " CLEAR_DUMMY

*&spwizard: declaration of tablecontrol 'TC_ASSET' itself
CONTROLS: tc_asset TYPE TABLEVIEW USING SCREEN 9000.

*&spwizard: output module for tc 'TC_ASSET'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE tc_asset_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_asset LINES tc_asset-lines.
ENDMODULE.                    "TC_ASSET_CHANGE_TC_ATTR OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS   '9000'.
  SET TITLEBAR    '9000'.

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
    WHEN 'PICK'.
      GET CURSOR FIELD wa_cursor LINE wa_line.
      wa_pick_line = tc_asset-top_line + wa_line - 1.
      IF ( wa_cursor NE 'IT_ASSET-ANLN1' OR
           wa_pick_line LE 0 OR wa_pick_line GT wa_t_cnt ).
        CLEAR wa_pick_line.
      ELSE.
        READ TABLE it_asset INDEX wa_pick_line.
        IF sy-subrc = 0.
          SET PARAMETER ID 'AN1' FIELD it_asset-anln1.
          CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.

      CLEAR ok_code.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit_screen  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_screen INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " exit_screen  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_io_budget2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*      -->P_GT_OUT_KEY  text
*----------------------------------------------------------------------*
*FORM get_io_budget2 USING    u_aufnr u_key.
*  REFRESH : it_budget.
*  CLEAR   : it_budget.
*  CALL FUNCTION 'Z_FFI_GET_IO_BUDGET'
*       EXPORTING
*            aufnr = u_aufnr
*       TABLES
*            out   = it_budget.
*  LOOP AT it_budget.
*    CASE it_budget-gjahr.
*      WHEN '1111'.
*        gt_out-tot  =
*             it_budget-org + it_budget-supp + it_budget-ret.
*        wa_n_tot = gt_out-tot.
*      WHEN wa_year.
*        gt_out-year =
*             it_budget-org + it_budget-supp + it_budget-ret.
*        wa_n_year = gt_out-year1.
*      WHEN wa_YEAR2.
*        gt_out-YEAR2 =
*             it_budget-org + it_budget-supp + it_budget-ret.
*        wa_n_YEAR2 = gt_out-YEAR2.
*      WHEN wa_YEAR3.
*        gt_out-YEAR3 =
*             it_budget-org + it_budget-supp + it_budget-ret.
*        wa_n_YEAR3 = gt_out-YEAR3.
*      WHEN wa_YEAR4.
*        gt_out-YEAR4 =
*             it_budget-org + it_budget-supp + it_budget-ret.
*        wa_n_YEAR4 = gt_out-YEAR4.
*      WHEN wa_YEAR5.
*        gt_out-YEAR5 =
*             it_budget-org + it_budget-supp + it_budget-ret.
*        wa_n_YEAR5 = gt_out-YEAR5.
*      WHEN OTHERS.
*        IF it_budget-gjahr < wa_last.
*          gt_out-before = gt_out-before + it_budget-org
*                        + it_budget-supp + it_budget-ret.
*          wa_n_before = gt_out-before.
*        ELSEIF it_budget-gjahr > wa_YEAR5.
*          gt_out-after  = gt_out-after  + it_budget-org
*                        + it_budget-supp + it_budget-ret.
*          wa_n_after = gt_out-after.
*        ENDIF.
*    ENDCASE.
*  ENDLOOP.
*ENDFORM.                    " get_io_budget2
*&---------------------------------------------------------------------*
*&      Form  get_io_buget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*----------------------------------------------------------------------*
FORM get_io_buget USING    u_aufnr.
  REFRESH : it_budget.
  CLEAR   : it_budget.
  CALL FUNCTION 'Z_FFI_GET_IO_BUDGET'
    EXPORTING
      aufnr = u_aufnr
    TABLES
      out   = it_budget.

*FUNCTION z_ffi_get_io_budget.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(AUFNR) LIKE  AUFK-AUFNR
*"  TABLES
*"      OUT STRUCTURE  ZFI_IO_BUDGET
*"----------------------------------------------------------------------
*  DATA : wa_objnr LIKE aufk-objnr.
*  DATA : BEGIN OF it_bpja OCCURS 0.
*          INCLUDE STRUCTURE bpja.
*  DATA : END OF it_bpja.
*
*  DATA : BEGIN OF it_bpge OCCURS 0.
*          INCLUDE STRUCTURE bpge.
*  DATA : END OF it_bpge.
*
*
*  CLEAR : wa_objnr.
*  SELECT SINGLE objnr INTO wa_objnr
*  FROM aufk
*  WHERE aufnr EQ aufnr.
**---Annual
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpja
*  FROM bpja
*  WHERE lednr EQ '0001'
*  AND   objnr EQ wa_objnr.
**--Overall
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpge
*  FROM bpge
*  WHERE lednr EQ '0001'
*  AND   objnr EQ wa_objnr.
*
*  REFRESH : OUT.
*  CLEAR   : OUT.
**---Annual
*  LOOP AT it_bpja.
*    MOVE AUFNR    TO OUT-AUFNR.
*    MOVE IT_BPJA-GJAHR TO OUT-GJAHR.
*    CASE it_bpja-vorga.
*      WHEN 'KSTP'.        "Plan
*           MOVE it_bpja-wtjhr TO OUT-PLAN.
*      WHEN 'KBUD'.        "Ori Budget
*           MOVE it_bpja-wtjhr TO OUT-ORG.
*      WHEN 'KBN0'.        "Supplement
*           MOVE it_bpja-wtjhr TO OUT-SUPP.
*      WHEN 'KBR0'.        "Return
*           MOVE it_bpja-wtjhr TO OUT-RET.
*    ENDCASE.
*    COLLECT OUT.
*    CLEAR   OUT.
*  ENDLOOP.
**-------------------Overall data process
*  LOOP AT it_bpge.
*    MOVE AUFNR    TO OUT-AUFNR.
*    MOVE '1111' TO OUT-GJAHR.
*    CASE it_bpge-vorga.
*      WHEN 'KSTP'.        "Plan
*        MOVE it_bpge-wtges  TO  OUT-PLAN.
*      WHEN 'KBUD'.        "Ori Budget
*        MOVE it_bpge-wtges  TO  OUT-ORG.
*      WHEN 'KBN0'.        "Supplement
*        MOVE it_bpge-wtges  TO  OUT-SUPP.
*      WHEN 'KBR0'.        "Return
*        MOVE it_bpge-wtges  TO  OUT-RET.
*    ENDCASE.
*    COLLECT OUT.
*    CLEAR   OUT.
*  ENDLOOP.
*
*
*ENDFUNCTION.
ENDFORM.                    " get_io_buget
*&---------------------------------------------------------------------*
*&      Form  get_io_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*----------------------------------------------------------------------*
FORM get_io_actual USING    u_aufnr.
  REFRESH it_actual.

  CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
    EXPORTING
      aufnr         = u_aufnr
* IMPORTING
*   AMT           =
    TABLES
      out           = it_actual
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
*&      Form  make_output_structure
*&---------------------------------------------------------------------*
FORM make_output_structure.

  LOOP AT gt_itab.
    gt_out-aufnr = gt_itab-aufnr.
    gt_out-ktext = gt_itab-ktext.
    gt_out-akstl = gt_itab-akstl.
*    GT_OUT-KOSTL = GT_ITAB-KOSTL.
    gt_out-posid = gt_itab-posid.
    gt_out-user0 = gt_itab-user0.
* by ig.moon 6/19/2009 {
    gt_out-kostv = gt_itab-kostv.

* }
    gt_out-key = c_pln.  gt_out-act = 'Plan'.            APPEND gt_out.
    gt_out-key = c_borg. gt_out-act = 'Budget-Orginal'.  APPEND gt_out.
    gt_out-key = c_bsup. gt_out-act = 'Budget-Suppl. '.  APPEND gt_out.
    gt_out-key = c_bret. gt_out-act = 'Budget-Return '.  APPEND gt_out.
    gt_out-key = c_bcur. gt_out-act = 'Budget-Current'.  APPEND gt_out.

    gt_out-key = c_inv.  gt_out-act = 'Actual-IV'.        APPEND gt_out.
    gt_out-key = c_com.  gt_out-act = 'Actual-Commit'.    APPEND gt_out.
    gt_out-key = c_dp.   gt_out-act = 'Actual-DP'.        APPEND gt_out.

  ENDLOOP.

*  SORT gt_out BY aufnr key ASCENDING.

ENDFORM.                    " make_output_structure
*&---------------------------------------------------------------------*
*&      Form  fill_output
*&---------------------------------------------------------------------*
FORM fill_output.

  LOOP AT gt_out.
*----get i/o budget
    AT NEW aufnr.
      PERFORM get_io_actual USING gt_out-aufnr.
      IF p_actual = space.
        PERFORM get_io_buget USING gt_out-aufnr.
      ENDIF.
    ENDAT.

    PERFORM make_io_actual.

    PERFORM make_io_budget.

    MODIFY gt_out.
    PERFORM clear_dummy.
  ENDLOOP.

  IF p_actual = space.
    DATA : $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.

    LOOP AT gt_out.
      $gt_out = gt_out.
      $gt_out-key = c_bava.
      $gt_out-act = 'Budget-Avail.'.
      CASE gt_out-key.
        WHEN c_borg OR c_bsup OR c_bret.
          COLLECT $gt_out.
        WHEN c_inv OR c_com OR c_dp.
          $gt_out-tot     =   - $gt_out-tot.
          $gt_out-before  =   - $gt_out-before.
          $gt_out-last    =   - $gt_out-last.
          $gt_out-year1   =   - $gt_out-year1.
          $gt_out-year2   =   - $gt_out-year2.
          $gt_out-year3   =   - $gt_out-year3.
          $gt_out-year4   =   - $gt_out-year4.
          $gt_out-year5   =   - $gt_out-year5.
          $gt_out-after   =   - $gt_out-after.
          COLLECT $gt_out.
      ENDCASE.
    ENDLOOP.

    APPEND LINES OF $gt_out TO gt_out.
  ENDIF.

  DATA $ix TYPE i.

  LOOP AT gt_out.

    $ix = sy-tabix.
    READ TABLE i_kostv_text WITH KEY kostl = gt_out-akstl
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-akstl_text = i_kostv_text-ktext.
    ENDIF.

    READ TABLE i_kostv_text WITH KEY kostl = gt_out-kostv
     BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-kostv_text = i_kostv_text-ktext.
    ENDIF.


    READ TABLE i_posid_text WITH KEY posid = gt_out-posid
     BINARY SEARCH.

    IF sy-subrc EQ 0.
      gt_out-posid_text = i_posid_text-ktext .
    ENDIF.

    MODIFY gt_out INDEX $ix TRANSPORTING akstl_text
                        kostv_text posid_text .

  ENDLOOP.

ENDFORM.                    " fill_output
*&---------------------------------------------------------------------*
*&      Form  move_amt_to_out
*&---------------------------------------------------------------------*
FORM move_amt_to_out USING    f_gjahr
                              f_amount.

  CASE f_gjahr.
    WHEN '1111'.
      gt_out-tot    = f_amount.
    WHEN wa_before.
      gt_out-before = f_amount.
    WHEN wa_last.
      gt_out-last   = f_amount.

    WHEN wa_year1.
      gt_out-year1   = f_amount.
    WHEN wa_year2.
      gt_out-year2  = f_amount.
    WHEN wa_year3.
      gt_out-year3  = f_amount.
    WHEN wa_year4.
      gt_out-year4  = f_amount.
    WHEN wa_year5.
      gt_out-year5  = f_amount.

    WHEN OTHERS.
      IF f_gjahr < wa_last.
        gt_out-before = gt_out-before + f_amount.
      ELSE.
        gt_out-after  = gt_out-after  + f_amount.
      ENDIF.
  ENDCASE.

ENDFORM.                    " move_amt_to_out
*&---------------------------------------------------------------------*
*&      Form  fill_KOSTV_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_text.

  IF i_kostv_text[] IS INITIAL.
    SELECT kostl ktext INTO TABLE i_kostv_text
    FROM cskt WHERE spras EQ sy-langu
                AND kokrs EQ 'H201'
                AND datbi >= sy-datum.
    SORT i_kostv_text BY kostl.
  ENDIF.



  IF i_posid_text[] IS INITIAL.

    SELECT prog_pos prgpos_txt appr_year INTO TABLE i_posid_text
    FROM imceppt WHERE langu EQ sy-langu.
    SORT i_posid_text BY posid ASCENDING
                         appr_year DESCENDING.

    DELETE ADJACENT DUPLICATES FROM i_posid_text
      COMPARING posid appr_year.

    SORT i_posid_text BY posid.

  ENDIF.

ENDFORM.                    " fill_KOSTV_TEXT
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_AUFK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_aufk .

  TYPES: BEGIN OF ty_aufk,
         aufnr TYPE aufnr,
         auart TYPE aufart,
         erdat TYPE auferfdat,
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
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_aufk
    FROM (l_gentab)
   WHERE aufnr IN s_aufnr.

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

    DELETE lt_aufk WHERE akstl NOT IN s_akstl
                      OR kostv NOT IN s_kostv
                      OR autyp NE '01'
                      OR user0 NOT IN s_usr02.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_aufk INTO TABLE gt_aufk_a.
  ENDLOOP.

  CHECK NOT gt_aufk_a[] IS INITIAL.

  SORT gt_aufk_a.
  DELETE ADJACENT DUPLICATES FROM gt_aufk_a COMPARING ALL FIELDS.

  INSERT LINES OF gt_aufk_a INTO TABLE gt_aufk.

ENDFORM.                    " ARCHIVE_READ_AUFK
