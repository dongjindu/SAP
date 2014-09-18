REPORT zrfii12  MESSAGE-ID  zmfi.
*&--------------------------------------------------------------------
*& Author                 : Hs.Jeong
*& Creation Date          : 09/18/2003
*& Specification By       : Hs.Jeong
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  : list for print out slip
*&                This is developed use ALV.
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------

TYPE-POOLS: slis.
INCLUDE <icon>.
INCLUDE <symbol>.

TABLES: imtp, imtt, t020.

CLASS cl_gui_resources DEFINITION LOAD.

CONSTANTS:
  c_f2code               LIKE sy-ucomm    VALUE '&ETA'.
*  c_projp                like impr-prnam  value'HMMA0001',
*  c_projh                like impr-prnam  value'HMMA0002'.


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

DATA: BEGIN OF gt_out OCCURS 0,
        posid  LIKE imak-posid,
        txt50  LIKE imakt-txt50,
        ivart  LIKE imak-ivart,
        gjahr  LIKE imak-gjahr,
        prnam  LIKE impr-prnam,
        status LIKE bapiappreqstatus-status,
        text   LIKE bapiappreqstatus-text,
        proc1(20) TYPE c,
        status2,
        proc2(16) TYPE c,
        status3,
        proc3(16) TYPE c,
        status4,
        proc4(16) TYPE c,
        status5,
        proc5(16) TYPE c,
        status6,
        proc6(16) TYPE c,
        status7,
        proc7(16) TYPE c,
        chkbox TYPE c,
        light   TYPE c,
* by ig.moon {
        icon  TYPE icon_d,
        ydist TYPE im_ydist,
* }
        tabcolor     TYPE slis_t_specialcol_alv,
      END OF gt_out.
*------
DATA: BEGIN OF gt_temp OCCURS 0,
        posid  LIKE imak-posid,
        txt50  LIKE imakt-txt50,
        ivart  LIKE imak-ivart,
        gjahr  LIKE imak-gjahr,
        prnam  LIKE impr-prnam,
        status LIKE bapiappreqstatus-status,
        text   LIKE bapiappreqstatus-text,
        proc1(20) TYPE c,
        status2,
        proc2(16) TYPE c,
        status3,
        proc3(16) TYPE c,
        status4,
        proc4(16) TYPE c,
        status5,
        proc5(16) TYPE c,
        status6,
        proc6(16) TYPE c,
        status7,
        proc7(16) TYPE c,
        chkbox TYPE c,
        light   TYPE c,
* by ig.moon {
        icon  TYPE icon_d,
* }
        tabcolor     TYPE slis_t_specialcol_alv,
      END OF gt_temp.
*===============================================================*
DATA: it_out TYPE TABLE OF imak WITH HEADER LINE.
DATA: it_status LIKE bapiappreqstatus OCCURS 0 WITH HEADER LINE.
DATA: it_user_status LIKE bapiappreqstatus OCCURS 0 WITH HEADER LINE.
DATA: it_progtree    LIKE bapiprogstruc    OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_imakt OCCURS 0.
        INCLUDE STRUCTURE imakt.
DATA : END OF it_imakt.
*----CBO
DATA : BEGIN OF it_imfm OCCURS 0.
        INCLUDE STRUCTURE ztfi_imfm.
DATA : END OF it_imfm.
*-----Detail
DATA : wa_master_data LIKE  bapiappreqmaster.
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
*---Found step
DATA : BEGIN OF it_fmfint OCCURS 0,
          fincode LIKE fmfint-fincode,
       END OF it_fmfint.
*---I/O Create CHECK
DATA : BEGIN OF it_aufk OCCURS 0,
          aufnr LIKE aufk-aufnr,
       END OF it_aufk.

DATA : it_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
*---AR-PI Link
DATA : BEGIN OF it_imzo OCCURS 0,
          objnr LIKE imzo-objnr,
          gjahr LIKE imzo-gjahr,
       END OF it_imzo.

*=====WorK area
DATA : wk_cnt TYPE i,
       wa_cnt TYPE i,
       wa_create_chk,
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
       wa_conver_date LIKE sy-datum,
       wa_datfm  LIKE usr01-datfm,
       wa_appreqvrnt  LIKE bapiappreqvarntassignmulti,
       wa_amt(15), "  LIKE BPDY-WERT1.
       wa_type  LIKE ifmfincode-type,
       wa_aufnr LIKE aufk-aufnr,
       wa_vernr LIKE impr-vernr,
       wa_posid    LIKE  impr-posid,
       wa_min         LIKE imak-gjahr,
       wa_max         LIKE imak-gjahr,
       wa_plan_tot-overhead_costs LIKE
              bapiappreqplantotalmulti-overhead_costs.

DATA: gt_capex TYPE STANDARD TABLE OF imtp WITH HEADER LINE.

*====FOR BDC
DATA : it_bdc      LIKE bdcdata OCCURS 0 WITH HEADER LINE.
DATA:  it_messtab  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA : tcode LIKE tstc-tcode.

DATA: BEGIN OF upd_imtp OCCURS 1.
        INCLUDE STRUCTURE rimtp.
DATA: END   OF upd_imtp.
DATA: BEGIN OF upd_imtt OCCURS 1.
        INCLUDE STRUCTURE rimtt.
DATA: END   OF upd_imtt.

DATA: strg_aktyp LIKE t020-aktyp.

DATA: BEGIN OF zimtp.
        INCLUDE STRUCTURE imtp.
DATA: END OF zimtp.
DATA: BEGIN OF zimtt.
        INCLUDE STRUCTURE imtt.
DATA: END OF zimtt.
DATA: BEGIN OF zimpr.
        INCLUDE STRUCTURE impr.
DATA: END OF zimpr.
DATA: BEGIN OF zimpu.
        INCLUDE STRUCTURE impu.
DATA: END OF zimpu.

DATA: BEGIN OF upd_impr OCCURS 1.
        INCLUDE STRUCTURE rimpr.
DATA: END   OF upd_impr.
DATA: BEGIN OF upd_impu OCCURS 1.
        INCLUDE STRUCTURE rimpu.
DATA: END   OF upd_impu.

DATA: on(1)  TYPE c VALUE 'X',
      off(1) TYPE c VALUE ' '.

* Update-Kennzeichen.
DATA: con_insert TYPE im_updkz VALUE 'I',
      con_update TYPE im_updkz VALUE 'U',
      con_delete TYPE im_updkz VALUE 'D'.

*DATA: BEGIN OF IT_out OCCURS 0,
*        posnr  LIKE imak-posnr,
*        txt50  LIKE imakt-TXT50,
*        proc(50) TYPE c,
*      END OF IT_out.
*DATA: gt_list_top_of_page TYPE slis_t_listheader.

*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
TABLES : imak, impr, bpge, bpja.
DATA: i_bpge LIKE bpge OCCURS 0 WITH HEADER LINE.
DATA: i_bpja LIKE bpja OCCURS 0 WITH HEADER LINE.
DATA : e_flag.
*--------------------------*
RANGES: r_objnr FOR impr-objnr.

DATA : g_prnam  LIKE   bapiprogposid-program. " OBLIGATORY.  "

*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
PARAMETERS : p_gjahr  LIKE  imtp-gjahr OBLIGATORY MEMORY ID gjr.
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
* AR variant year, PI approval year
*Issue number :
*Requested by CYYOON ,Changed by wskim,on 11/10/04
*-----Start
*-----End

SELECT-OPTIONS:
  s_ivart   FOR   imak-ivart,
  s_posnr   FOR   imak-posnr,
  s_vkostl  FOR   imak-vkostl,
* AR approval year
  s_gjahr   FOR   imak-gjahr.
SELECTION-SCREEN END OF BLOCK b0.
*--check
PARAMETERS : p_versn LIKE imavz-versi DEFAULT '0',
             p_prnam LIKE bapiprogposid-program.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-011.
PARAMETERS : p_chk1  AS CHECKBOX DEFAULT 'X',
             p_chk2  AS CHECKBOX DEFAULT 'X',
             p_chk3  AS CHECKBOX DEFAULT 'X',
             p_chk4  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-012.
PARAMETERS :
  p_layout LIKE disvariant-variant.   "LAYOUT
SELECTION-SCREEN END OF BLOCK b2.

*parameters p_cf  as checkbox default 'X'.   "PI Plan = AR - PI C/F
** BDC mode
*
*PARAMETERS SESSION as checkbox.  "create session
PARAMETERS cupdate LIKE ctu_params-updmode DEFAULT 'S' NO-DISPLAY.
"S: synchronously
"A: asynchronously
"L: local

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
*---------------------------------------------------------------------
*    M   A   I   N
*
*---------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM build_field_category
  USING :
   'POSID'     'AR Number'    '12' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'TXT50'     'Description'  '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'IVART'     'Type'         '02' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PROC1'     'Status'       '13' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'ICON'      'Variant'      '4'  ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PROC2'     'PI Creation'  '13' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PROC3'     'AR-PI Link'   '13' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*   'PROC4'     'PI Plan Step' '13' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PROC5'     'Fund Step'    '13' ' ' 'L'  ' '  ' '  '  ' '  ' ,
*   'PROC6'     'Fund Budget'  '13' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PROC7'     'Overh.IO Creation'  '17' ' ' 'L'  ' '  ' '  '  ' '  ' .
* ==> 6. build sorts info
*  REFRESH gt_sorts.
*  PERFORM build_sort_table
*    USING :
*       '1'    'VTWEG'   'X'   'X'   '*'.
* ==> 1. select data from db
  g_prnam = p_prnam.

  PERFORM select_data USING e_flag.
*issue 20050120-003
*Requested by CYYOON ,Changed by wskim,on 02/03/05
*-----Start
  CASE e_flag.
    WHEN 'N'.
      MESSAGE s000(zmfi) WITH 'AR does not exist'.
      EXIT.
  ENDCASE.
*    IF gt_out[] IS INITIAL.
*    MESSAGE s000(zmfi) WITH 'No Data'.
*    EXIT.
*  ENDIF.
*-----End
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

  IF  p_fieldname EQ 'ICON'.
    ls_fieldcat-datatype = 'ICON'.
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
FORM select_data USING e_flag.

  DATA: BEGIN OF lt_posid OCCURS 0,
          posid TYPE ima_posnr,
        END OF lt_posid.

  DATA : c_flag.
  REFRESH : it_out, gt_out, it_impr, it_imfm.
  CLEAR   : it_out, gt_out, it_impr, it_imfm.

* read program
  SELECT * INTO TABLE gt_capex
  FROM   imtp
  WHERE  gjahr = p_gjahr.

* read AR
  SELECT * INTO TABLE it_out FROM imak
    WHERE posnr  IN s_posnr AND
          ivart  IN s_ivart AND
          gjahr  IN s_gjahr AND
          vkostl IN s_vkostl.

  DESCRIBE TABLE it_out LINES wk_cnt.

  IF wk_cnt > 0.

    LOOP AT it_out.
      lt_posid-posid = gt_out-posid.
      APPEND lt_posid.
    ENDLOOP.

*   read monthly plan (orginal)
*FIXME SUM
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_imfm
    FROM  ztfi_imfm
    FOR ALL ENTRIES IN lt_posid
    WHERE posid = lt_posid-posid
    AND   ayear = p_gjahr
    AND   gubun = 'P'.
*    and   seq   = '0000'.   "Plan

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
      AND  gjahr = p_gjahr.
*      AND  gjahr IN s_gjahr.
*    AND   prnam  = g_prnam.


*--PI plan step CHECK

** Furong on 10/31/11 Performance {
    REFRESH: it_bpge.
    IF NOT it_impr[] IS INITIAL.
** End on 10/31/11
      SELECT objnr wtges INTO CORRESPONDING FIELDS OF TABLE it_bpge
      FROM bpge
      FOR ALL ENTRIES IN it_impr
      WHERE  objnr = it_impr-objnr
        AND  wrttp = '48'.             " program plan
    ENDIF.
*

*--Fund  step CHECK
    SELECT fincode INTO CORRESPONDING FIELDS OF TABLE it_fmfint
    FROM fmfint
    FOR ALL ENTRIES IN it_out
    WHERE   spras = sy-langu
    AND     fikrs = 'H201'
    AND     fincode = it_out-posid+1(10).

*--I/O  Create CHECK
    SELECT aufnr INTO CORRESPONDING FIELDS OF TABLE it_aufk
    FROM aufk
    WHERE  auart = 'Y'.
  ELSE.
    e_flag = 'N'.
    EXIT.
  ENDIF.

  CLEAR : c_flag,gt_out.
  DATA $ix TYPE i.
  DATA err.

  LOOP AT it_out.
    $ix = sy-tabix.

*Issue number :
*Requested by CYYOON ,Changed by wskim,on 11/10/04
*-----Start
    CLEAR err.
    PERFORM check_bapi_apprequest USING it_out-posid
                                  CHANGING c_flag
                                           err.
    IF c_flag <> 'X'.
      CONTINUE.
    ENDIF.
*-----End
    MOVE-CORRESPONDING it_out TO gt_out.
    READ TABLE it_imakt WITH KEY posnr = it_out-posid.
    IF sy-subrc = 0.
      MOVE it_imakt-txt50 TO gt_out-txt50.
    ENDIF.
**----PI in program
*    CLEAR wa_chk.
*    PERFORM include_program_check USING it_out-posid
*                                  CHANGING wa_chk.
**    IF   wa_chk = 'X'.
**      CONTINUE.
**    ENDIF.
*---GET Status
    CLEAR : wa_stat, wa_chk.
    PERFORM get_status USING it_out-posid
                       CHANGING wa_stat
                                wa_chk.
    IF wa_chk = 'X'.
      CONTINUE.
    ENDIF.

    PERFORM get_status_name.
**----PI Status
*    IF wa_stat <> 'I0364'.
*      MOVE 'Not Possible'  TO gt_out-proc2.
*      MOVE 'X'             TO gt_out-status2.
*      MOVE 'X'             TO gt_out-status3.
*      MOVE 'Not Possible'  TO gt_out-proc3.
*      MOVE 'Not Possible'  TO gt_out-proc4.
*      MOVE 'Not Possible'  TO gt_out-proc5.
*    ELSE.
    PERFORM pi_status  USING it_out-posid.
*    ENDIF.
*----AR-PI Link
    CLEAR wa_objnr1.
    CONCATENATE 'IQ'  it_out-posid INTO wa_objnr1.
    IF gt_out-status2 <> 'X'.
      PERFORM ar_pi_link_check USING wa_objnr1 p_gjahr.
    ENDIF.

*----PI Plna Step check
*    PERFORM PI_PLAN_step_check USING it_out-posid.

*----FUND Step check
    PERFORM fund_step_check USING it_out-posid.
*---I/O Create Check
    PERFORM io_create_check USING it_out-posid.
    MOVE 1 TO gt_out-light.

    IF err EQ 'X'.
      gt_out-icon = icon_led_yellow.
    ELSE.
      gt_out-icon = icon_led_green.
    ENDIF.

    APPEND gt_out.
    CLEAR gt_out.
  ENDLOOP.
*---Select Condition.
  REFRESH : gt_temp.
  CLEAR   : gt_temp.
  IF p_chk1 = 'X'.
    LOOP AT gt_out WHERE status = ' '.
      MOVE-CORRESPONDING gt_out TO gt_temp.
      APPEND gt_temp.
      CLEAR  gt_temp.
    ENDLOOP.
  ENDIF.
  IF p_chk2 = 'X'.
    LOOP AT gt_out WHERE status = 'R'.
      MOVE-CORRESPONDING gt_out TO gt_temp.
      APPEND gt_temp.
      CLEAR  gt_temp.
    ENDLOOP.
  ENDIF.
  IF p_chk3 = 'X'.
    LOOP AT gt_out WHERE status = 'F'.
      MOVE-CORRESPONDING gt_out TO gt_temp.
      APPEND gt_temp.
      CLEAR  gt_temp.
    ENDLOOP.
  ENDIF.
  IF p_chk4 = 'X'.
    LOOP AT gt_out WHERE status = 'A'.
      MOVE-CORRESPONDING gt_out TO gt_temp.
      APPEND gt_temp.
      CLEAR  gt_temp.
    ENDLOOP.
  ENDIF.

  REFRESH : gt_out.
  CLEAR   : gt_out.

  LOOP AT gt_temp.
    MOVE-CORRESPONDING gt_temp TO gt_out.

    SELECT SINGLE ydist INTO gt_out-ydist
      FROM   imtp
     WHERE  prnam = g_prnam
       AND  gjahr = p_gjahr.

    APPEND gt_out.
    CLEAR  gt_out.
  ENDLOOP.
*---date convert.
*  clear wa_datfm.
*  select single datfm into wa_datfm
*  from usr01
*  where bname = sy-uname.
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
*--2003/11/19 MODIFY -> MOVE TO MONTHLY PLAN
*        WHEN 'PROC1'.
*          IF gt_out-status = 'I0354'.
*            tcode = 'IMA2'.
*            PERFORM create_bdc USING gt_out-posid.
*            MOVE 'Approved'  TO gt_out-proc1.
**--PI Statua
*            PERFORM pi_status USING gt_out-posid.
*            MODIFY  gt_out INDEX rs_selfield-tabindex.
*            rs_selfield-refresh = 'X'.
*          ENDIF.
*-------PI CREATION Double Click.
        WHEN 'PROC2'.
*--2004/01/09
*          IF gt_out-status2 = 'Q'.
*         IF gt_out-proc1 = 'A'.
          IF gt_out-status2 <> 'A'.
            PERFORM call_bapi_ar_detail USING gt_out-posid.
            CLEAR : wk_cnt.
            DESCRIBE TABLE it_variant_to_version LINES wk_cnt.
            IF wk_cnt < 1.
              MOVE 'No Version'  TO gt_out-proc2.
            ELSE.
              CLEAR : wa_varnt.
              PERFORM get_variant CHANGING wa_varnt.
              PERFORM pi_create USING gt_out-posid.
              IF wa_bdc_ok = 'Q'.
                PERFORM pi_change USING gt_out-posid.
              ENDIF.
            ENDIF.
            MODIFY  gt_out INDEX rs_selfield-tabindex.
            rs_selfield-refresh = 'X'.
          ELSEIF gt_out-status2 = 'A'.
*            PERFORM pi_change USING gt_out-posid.
            SET PARAMETER ID 'IMT' FIELD g_prnam.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_gjahr.  "gt_out-gjahr.
            CALL TRANSACTION 'IM13' AND SKIP FIRST SCREEN.
          ENDIF.
*-------AR-PI Link Double click
        WHEN 'PROC3'.
          IF gt_out-status3 = 'Q'.
*            PERFORM ar_pi_link_proc USING gt_out-posid.
            PERFORM ar_pi_link_proc_bapi USING gt_out-posid.
            MODIFY  gt_out INDEX rs_selfield-tabindex.
            rs_selfield-refresh = 'X'.
          ELSEIF gt_out-status3 = 'A'.
            SET PARAMETER ID 'IAF' FIELD gt_out-posid.
            CALL TRANSACTION 'IMA3N' AND SKIP FIRST SCREEN.
          ENDIF.
*-------PI Plan step Double click
        WHEN 'PROC4'.
          IF gt_out-status4 = 'A'.
            SET PARAMETER ID 'IMT' FIELD g_prnam.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_gjahr. "gt_out-gjahr.
            SET PARAMETER ID 'BP2' FIELD '0'.
            CALL TRANSACTION 'IM36' AND SKIP FIRST SCREEN.
          ELSE.
            CLEAR : wk_cnt.
            DESCRIBE TABLE it_variant_to_version LINES wk_cnt.
            IF wk_cnt < 1.
              MOVE 'No Version'  TO gt_out-proc4.
            ELSE.
              CLEAR : wa_varnt.
              PERFORM get_variant CHANGING wa_varnt.
              PERFORM pi_plan_step USING gt_out-posid.
            ENDIF.
            MODIFY  gt_out INDEX rs_selfield-tabindex.
            rs_selfield-refresh = 'X'.
          ENDIF.
*-------Fund Step
        WHEN 'PROC5'.
          IF gt_out-status5 = 'Q'.
            CLEAR : wk_cnt.
            DESCRIBE TABLE it_variant_to_version LINES wk_cnt.
            IF wk_cnt < 1.
              MOVE 'No Version'  TO gt_out-proc5.
            ELSE.
              CLEAR : wa_varnt.
              PERFORM get_variant CHANGING wa_varnt.
              PERFORM get_detail_data USING gt_out-posid.
*             create BDC
              PERFORM fund_step USING gt_out-posid 'X'.
            ENDIF.
            MODIFY  gt_out INDEX rs_selfield-tabindex.
            rs_selfield-refresh = 'X'.
          ELSEIF gt_out-status5 = 'A'.
            SET PARAMETER ID 'FIK' FIELD 'H201'.
            SET PARAMETER ID 'FIC' FIELD  gt_out-posid+1(10).
            CALL TRANSACTION 'FM5S' AND SKIP FIRST SCREEN.
          ENDIF.
*-------I/O Create
        WHEN 'PROC7'.
*          CHECK gt_out-posid+0(1) = 'P'.
          IF gt_out-status7 = 'Q'.
            CLEAR : wk_cnt.
            DESCRIBE TABLE it_variant_to_version LINES wk_cnt.
            IF wk_cnt < 1.
              MOVE 'No Version'  TO gt_out-proc7.
            ELSE.
              CLEAR : wa_varnt.
              PERFORM get_variant CHANGING wa_varnt.
*-------overhead cost
              CLEAR : wa_plan_tot-overhead_costs.
*--20031218 don't care overheadcost
              PERFORM call_bapi_ar_detail USING gt_out-posid.
*              PERFORM chk_overhead USING wa_varnt
*                                   CHANGING wa_plan_tot-overhead_costs.
*              IF wa_plan_tot-overhead_costs > 0.
              PERFORM get_detail_data USING gt_out-posid.
              PERFORM io_create USING gt_out-posid.
*              ELSE.
*                MOVE 'No overheadcost' TO gt_out-proc7.
*              ENDIF.
            ENDIF.
            MODIFY  gt_out INDEX rs_selfield-tabindex.
            rs_selfield-refresh = 'X'.

          ELSEIF gt_out-status7 = 'A'.
            CLEAR wa_aufnr.
*            CONCATENATE '0' gt_out-posid INTO wa_aufnr.
            SET PARAMETER ID 'ANR' FIELD gt_out-posid. "wa_aufnr.
            CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
*===================================*
*---Approval 2003.11.19  MODIFY
*    WHEN '&APP'.
*      LOOP AT gt_out WHERE chkbox = 'X'.
*        CHECK gt_out-status = 'I0354'.
*        tcode = 'IMA2'.
*        PERFORM create_bdc USING gt_out-posid.
*        MOVE 'Approved'  TO gt_out-proc1.
**--PI Statua
*        PERFORM pi_status USING gt_out-posid.
*        MODIFY gt_out.
*      ENDLOOP.
*      rs_selfield-refresh = 'X'.
*---PI Creation
    WHEN '&PIC'.
      LOOP AT gt_out WHERE chkbox = 'X'.
*-----GET AR Detail.
        IF gt_out-status2 <> 'A'.
          PERFORM call_bapi_ar_detail USING gt_out-posid.
          CLEAR : wk_cnt.
          DESCRIBE TABLE it_variant_to_version LINES wk_cnt.
          IF wk_cnt < 1.
            MOVE 'No Version'  TO gt_out-proc2.
          ELSE.
            CLEAR : wa_varnt.
            PERFORM get_variant CHANGING wa_varnt.
            PERFORM pi_create USING gt_out-posid.
            IF wa_bdc_ok = 'Q'.
              PERFORM pi_change USING gt_out-posid.
            ENDIF.
          ENDIF.
        ENDIF.
        MODIFY gt_out.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
*---AR-PI Link
    WHEN '&ARPI'.
      LOOP AT gt_out WHERE chkbox = 'X'.
        IF gt_out-status3 = 'Q'.
*          PERFORM ar_pi_link_proc USING gt_out-posid.
          PERFORM ar_pi_link_proc_bapi USING gt_out-posid.
        ENDIF.
        MODIFY gt_out.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
*---PI Plan Step
    WHEN '&PIP'.
      LOOP AT gt_out WHERE chkbox = 'X'.
        PERFORM call_bapi_ar_detail USING gt_out-posid.
        CLEAR : wk_cnt.
        DESCRIBE TABLE it_variant_to_version LINES wk_cnt.
        IF wk_cnt < 1.
          MOVE 'No Version'  TO gt_out-proc4.
        ELSE.
          CLEAR : wa_varnt.
          PERFORM get_variant CHANGING wa_varnt.
          PERFORM pi_plan_step USING gt_out-posid.
        ENDIF.
        MODIFY gt_out.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
*---Fund Step
    WHEN '&FUND'.
      LOOP AT gt_out WHERE chkbox = 'X'.
*        CHECK gt_out-status5 = 'Q'.
        PERFORM call_bapi_ar_detail USING gt_out-posid.
        CLEAR : wk_cnt.
        DESCRIBE TABLE it_variant_to_version LINES wk_cnt.
        IF wk_cnt < 1.
          MOVE 'No Version'  TO gt_out-proc5.
        ELSE.
          CLEAR : wa_varnt.
          PERFORM get_variant CHANGING wa_varnt.
          PERFORM get_detail_data USING gt_out-posid.
*         create
          IF gt_out-status5 = 'Q'.
            PERFORM fund_step USING gt_out-posid 'X'.
*         change
          ELSE.
            PERFORM fund_step USING gt_out-posid ' '.
          ENDIF.
        ENDIF.
        MODIFY gt_out.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
*--IO Creation
    WHEN '&IOC'.
      LOOP AT gt_out WHERE chkbox = 'X'.
        CHECK gt_out-status7 = 'Q'.
        CHECK gt_out-posid+0(1) = 'P'.
        PERFORM call_bapi_ar_detail USING gt_out-posid.
        CLEAR : wk_cnt.
        DESCRIBE TABLE it_variant_to_version LINES wk_cnt.
        IF wk_cnt < 1.
          MOVE 'No Version'  TO gt_out-proc7.
        ELSE.
          CLEAR : wa_varnt.
          PERFORM get_variant CHANGING wa_varnt.
          CLEAR : wa_plan_tot-overhead_costs.
          PERFORM chk_overhead USING wa_varnt
                               CHANGING wa_plan_tot-overhead_costs.
          IF wa_plan_tot-overhead_costs > 0.
            PERFORM get_detail_data USING gt_out-posid.
            PERFORM io_create USING gt_out-posid.
          ELSE.
            MOVE 'No overheadcost' TO gt_out-proc7.
          ENDIF.

          PERFORM get_detail_data USING gt_out-posid.
          PERFORM io_create USING gt_out-posid.
        ENDIF.
        MODIFY gt_out.
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
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSNR  text
*      <--P_C_STAT  text
*----------------------------------------------------------------------*
FORM get_status USING    u_posid
                CHANGING c_stat LIKE bapiappreqstatus-status
                         c_chk.
  REFRESH : it_status, it_user_status.
  CLEAR   : it_status, it_user_status.
  CALL FUNCTION 'BAPI_APPREQUEST_GETSTATUS'
    EXPORTING
      externalnumber              = u_posid
      language                    = sy-langu
*     LANGUAGE_ISO                =
    TABLES
      apprequest_status           = it_status
      apprequest_user_status      = it_user_status
*     APPREQUESTVARNT_STATUS      =
*     APPREQUESTVARNT_USER_STATUS =
*     RETURN                      =
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
ENDFORM.                    " GET_STATUS
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
*&      Form  GET_STATUS_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_status_name.
  READ TABLE it_imfm WITH KEY posid = gt_out-posid
                              ayear = p_gjahr
                              gubun = '1'
                              seq   = '0000'.
  IF sy-subrc = 0.
    CASE it_imfm-status.
      WHEN ' '.
        MOVE 'Initial'  TO gt_out-proc1.
        MOVE it_imfm-status TO gt_out-status.
      WHEN 'R'.
        MOVE 'Request'  TO gt_out-proc1.
        MOVE it_imfm-status TO gt_out-status.
      WHEN 'F'.
        MOVE 'Ready for approval'  TO gt_out-proc1.
        MOVE it_imfm-status TO gt_out-status.
      WHEN 'A'.
        MOVE 'Approved'  TO gt_out-proc1.
        MOVE it_imfm-status TO gt_out-status.
    ENDCASE.
  ELSE.
    MOVE 'Planning'   TO  gt_out-proc1.
  ENDIF.
*
ENDFORM.                    " GET_STATUS_NAME
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
      externalnumber           = u_posid
      language                 = sy-langu
*     LANGUAGE_ISO             =
    IMPORTING
      master_data              = wa_master_data
      user_fields              = wa_user_field
      controlling_area         = wa_co_area
    TABLES
      org_units                = it_org_units
*     DIVISION                 =
*     MATERIAL_GROUP           =
      invest_reason            = it_invest_reson
      environmnt_invest        = it_env_invest
*     ASSETS_EQUIS             =
*     ORDER                    =
*     WBS_ELEMENT              =
*     PARTNER                  =
*     ASSIGNMENT_TO_POS        =
*     ASSIGNMENT_TO_BUDG_CATEG =
      variant                  = it_variant
      variant_to_version       = it_variant_to_version
*     ASSIGNED_APPREQUESTS     =
      plan_total               = it_plan_tot
      plan_year                = it_plan_year
*     RETURN                   =
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
    MOVE 'A'        TO gt_out-status2.
    MOVE 'Created'  TO gt_out-proc2.

    READ TABLE it_bpge WITH KEY objnr = it_impr-objnr.
    IF sy-subrc = 0 AND it_bpge-wtges > 0.
      MOVE 'A'        TO gt_out-status4.
      MOVE 'Created'  TO gt_out-proc4.
    ELSE.
      MOVE 'Q'        TO gt_out-status4.
      MOVE 'No Exist' TO gt_out-proc4.
    ENDIF.
  ELSE.
    MOVE 'Q'        TO gt_out-status2.
    MOVE 'Q'        TO gt_out-status4.
    MOVE 'No Exist' TO gt_out-proc2.
    MOVE 'Not Possible' TO gt_out-proc4.
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
  REFRESH : it_progtree, it_messtab.
  CLEAR   : it_progtree, it_messtab.
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
**On 03/19/14
  it_progtree-bal_sheet_item = wa_user_field-user00.
*  it_progtree-bal_sheet_item = wa_user_field-user03.
** End
*--PLANT.
  it_progtree-plant          = wa_master_data-plant.

  APPEND it_progtree.
  CLEAR  it_progtree.
*    ENDLOOP.
  PERFORM fill_data USING   g_prnam
                    CHANGING g_prnam.
  PERFORM fill_data USING wa_parent
                    CHANGING wa_parent.
*
  REFRESH : it_bdc, it_messtab.
  CLEAR   : it_bdc.
  tcode = 'IM22'.
*Issue number :
*Requested by YCY,Changed by wskim,on 11/10/04
*-----Start
*  if u_posid+0(1) eq 'P'.
*    g_prnam = c_projp.
*  else.
*    g_prnam = c_projh.
*  endif.

*-----End

* 10/22/2013 - T00306 Start
  DATA: l_xaktb LIKE impr-xaktb.

  IF gt_out-ydist EQ 'X'.
    l_xaktb = 'X'.
  ENDIF.
* 10/22/2013 - T00306 Emd

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIP2'        '0500',
                      ' '  'IMPR-PRNAM'      g_prnam,
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
                      ' '  'EL3(01)'        it_invest_reson-inv_reason.
  IF l_xaktb EQ 'X'.
    PERFORM make_bdc_rtn USING : ' '  'EL_CROSS_1(01)' l_xaktb.
  ENDIF.
  PERFORM make_bdc_rtn USING : ' '  'EL4(01)'        wa_master_data-priority,
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
  READ TABLE it_messtab INDEX 1.
  IF it_messtab-msgtyp = 'E'.
    MOVE 'Q'        TO gt_out-status2.
    MOVE 'PI Create Error' TO gt_out-proc2.
    MOVE ' '         TO wa_bdc_ok.
  ELSE.
    MOVE 'A'         TO gt_out-status2.
    MOVE 'Created'   TO gt_out-proc2.
    MOVE 'Q'         TO wa_bdc_ok.
  ENDIF.

*    CALL FUNCTION 'BAPI_EXPENDITUREPROGTREE_CREAT'
*      EXPORTING
*        program_in            = g_prnam
*        approvalyear_in       = P_GJAHR
*        PARENT                = WA_PARENT
*       PREDECESSOR           = ' '
*        TEST_RUN              = ' '
*       LANGUAGE              =
*       LANGUAGE_ISO          =
*     IMPORTING
*       PROGRAM               =
*       APPROVALYEAR          =
*       POSITION              =
*      tables
*        progtree              = IT_PROGTREE
*        RETURN                = IT_RETURN.
*
*CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
* EXPORTING
*   WAIT          =
* IMPORTING
*   RETURN        =
*          .

ENDFORM.                    " PI_CREATE
*&---------------------------------------------------------------------*
*&      Form  fill_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_g_prnam  text
*      <--P_g_prnam  text
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
*&      Form  AR_PI_LINK_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_OBJNR1  text
*      -->P_IT_OUT_GJAHR  text
*----------------------------------------------------------------------*
FORM ar_pi_link_check USING    u_objnr1
                               u_gjahr.

* Just check AR - PI without year.
  SELECT SINGLE COUNT(*) INTO wa_d_cnt
                FROM  imzo
                WHERE objnr = u_objnr1.
*                AND   gjahr = u_gjahr.
  IF wa_d_cnt > 0.
    MOVE 'Linked  '    TO  gt_out-proc3.
    MOVE 'A'          TO  gt_out-status3.
  ELSE.
    MOVE 'No Link '   TO  gt_out-proc3.
    MOVE 'Q'          TO  gt_out-status3.
  ENDIF.
ENDFORM.                    " AR_PI_LINK_CHECK
*&---------------------------------------------------------------------*
*&      Form  AR_PI_LINK_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM ar_pi_link_proc USING    u_posid.
  REFRESH : it_bdc, it_messtab.
  CLEAR   : it_bdc, it_messtab.
  tcode = 'IMA2'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIA1'        '0100',
                      ' '  'IMAK-POSNR'      u_posid,
                     ' '  'BDC_OKCODE'      '/00'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIA1'        '0200',
                     ' '  'BDC_OKCODE'      '=ANFO'.
  IF u_posid+0(1) = 'P'.
    PERFORM make_bdc_rtn USING :
                    'X'  'SAPLAIA1'        '0205',
                    ' '  'RAIP1-PRNAM'     g_prnam,
                    ' '  'RAIP1-GJAHR'     p_gjahr,
                    ' '  'RAIP1-POSID'     u_posid,
                   ' '  'BDC_OKCODE'      '/00'.
    PERFORM make_bdc_rtn USING :
                    'X'  'SAPLAIPP'        '0200',
                    ' '  'RAIP1-BAPRZ(01)' '100',
                   ' '  'BDC_OKCODE'      '=TAKE'.
    PERFORM make_bdc_rtn USING :
                    'X'  'SAPLAIA1'        '0208',
                   ' '  'BDC_OKCODE'      '=BUCH'.

  ELSE. "IF u_posid+0(1) = 'C'.
    PERFORM make_bdc_rtn USING :
                    'X'  'SAPLAIA1'        '0205',
                    ' '  'RAIP1-PRNAM'     g_prnam,
                    ' '  'RAIP1-GJAHR'     p_gjahr,
                    ' '  'RAIP1-POSID'     u_posid,
                   ' '  'BDC_OKCODE'      '=BUCH'.

  ENDIF.

  CALL TRANSACTION tcode   USING it_bdc
                           MODE   'E'
                           UPDATE 'S'
*                    OPTIONS  FROM CTU_PARAMS
                    MESSAGES INTO it_messtab.
*  READ TABLE it_messtab   INDEX 1.
*  IF IT_MESSTAB-MSGTYP = 'S'.
*      IF IT_MESSTAB-MSGNR = '010'.
*         MOVE 'A'         TO gt_out-status3.
*         MOVE 'Linked'   TO gt_out-proc3.
*       ENDIF.
*  ELSE.
*    MOVE 'Q'         TO gt_out-status3.
*    MOVE 'BDC Error' TO gt_out-proc3.
*  ENDIF.
  READ TABLE it_messtab   WITH KEY msgtyp = 'S'
                                   msgnr = '010'.
  IF sy-subrc = 0.
    MOVE 'A'         TO gt_out-status3.
    MOVE 'Linked'   TO gt_out-proc3.
  ELSE.
    MOVE 'Q'         TO gt_out-status3.
    MOVE 'BDC Error' TO gt_out-proc3.
  ENDIF.


ENDFORM.                    " AR_PI_LINK_PROC
*&---------------------------------------------------------------------*
*&      Form  PI_PLAN_STEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM pi_plan_step USING    u_posid.

  REFRESH : it_bdc, it_messtab.
  CLEAR   : it_bdc, it_messtab.

*---Verion GET
  READ TABLE it_variant_to_version
                WITH KEY appr_year    = p_gjahr
                         plan_version = '000'.

  CLEAR wa_appreqvrnt.
  IF sy-subrc = 0.
    wa_appreqvrnt = it_variant_to_version-appreqvrnt.
  ELSE.
    MESSAGE s000(zmfi) WITH 'Not assing version 0 '.
    EXIT.
  ENDIF.

* check carryforward amount
  PERFORM get_cf_amount USING u_posid.

* BDC or Batchjob
  PERFORM open_group.

  PERFORM create_pi_plan_bdc USING u_posid.

* End of BDC/Job
  PERFORM close_group.

*-------------------------------------------------*
  READ TABLE it_messtab INDEX 1.
  IF sy-subrc <> 0.
    MOVE 'A'         TO gt_out-status4.
    MOVE 'Created'   TO gt_out-proc4.
  ELSE.
    IF it_messtab-msgtyp = 'S'.
      MOVE 'A'         TO gt_out-status4.
      MOVE 'Created'   TO gt_out-proc4.
    ELSE.
      MOVE 'Q'         TO gt_out-status4.
      MOVE 'BDC Error' TO gt_out-proc4.
    ENDIF.
  ENDIF.

ENDFORM.                    " PI_PLAN_STEP
*&---------------------------------------------------------------------*
*&      Form  pi_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM pi_change USING    u_posid.
  DATA : wa_l_cnt TYPE i,
         l_date(8).
  CONCATENATE  '1231' p_gjahr INTO l_date.
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


*  perform make_bdc_rtn using :
*                      'X'  'SAPLAIP2'        '0500',
*                      ' '  'IMPR-PRNAM'      g_prnam,
*                      ' '  'IMPR-POSID'      u_posid,
*                      ' '  'IMPR-GJAHR'      p_gjahr,
*                     ' '  'BDC_OKCODE'      '/00'.
*  perform make_bdc_rtn using :
*                      'X'  'SAPLAIP2'        '0600',
*                      ' '  'IMPU-POST1'    wa_master_data-req_txt,
*                      ' '  'IMPR-IZWEK'    it_invest_reson-inv_reason,
*                      ' '  'IMPR-PPRIO'    wa_master_data-priority,
*                      ' '  'IMPR-SIZECL'   wa_master_data-scale,
*                      ' '  'IMPR-VERNR'
*                                     wa_vernr,
***--2004/01/15
**                      ' '  'IMPR-KOSTL'
**                             wa_master_data-rsp_cost_center,
*                      ' '  'IMPR-ABJHR'
*                              wa_master_data-desired_start+0(4),
*                      ' '  'IMPR-BIJHR'
*                             it_variant-completion_date+0(4),
*                      ' '  'BDC_OKCODE'      '=PUSH2'.
*
*  perform make_bdc_rtn using :
*                      'X'  'SAPLAIP2'        '0600',
*              ' '  'IMPR-KOSTL'  wa_master_data-rsp_cost_center,
*                      ' '  'IMPR-BUKRS'  wa_master_data-rsp_comp_code,
*                      ' '  'IMPR-WERKS'  wa_master_data-plant,
*                      ' '  'BDC_OKCODE'      '=PUSH3'.
***  LOOP AT it_ania.
***    wa_aufpr = it_ania-aufpr.
***    wa_l_cnt = wa_l_cnt + 1.
***    IF wa_l_cnt = 1.
*  perform make_bdc_rtn using :
*              'X'  'SAPLAIP2'        '0600',
*** Changed by Furong on 03/21/14
*** On 03/20/14 By Furong (
*             ' '  'ANIA-ANLKL'  wa_user_field-user00,
*              ' '  'ANIA-AKTIV' l_date,
**             ' '  'ANIA-ANLKL'  wa_user_field-user03,
**              ' '  'ANIA-AKTIV' p_gjahr,
*** ) End
**              ' '  'ANIA-ANLKL'  space, "wa_user_field-user03,
**              ' '  'ANIA-AKTIV'  space, "wa_date,
*** End
*              ' '  'BDC_OKCODE'      '=AUFT'.
**----2004/01/13-------------------*
*  if wa_bdc_ok = 'Q'.
*    perform make_bdc_rtn using :
*                'X'  'SAPLAIPS'        '0191',
*                ' '  'ANIA-KOSTL'  wa_master_data-rsp_cost_center,
*                ' '  'BDC_OKCODE'      '=RW'.
*  endif.
**--------------------------------*
**  perform make_bdc_rtn using :
**                  'X'  'SAPLAIPS'        '0310',
**                  ' '  'BDC_OKCODE'      '=RW'.
**-------
**  perform make_bdc_rtn using :
**                    'X'  'SAPLAIP2'        '0600',
**                    ' '  'BDC_OKCODE'      '=PUSH4'.
**
*  perform make_bdc_rtn using :
*                    'X'  'SAPLAIP2'        '0600',
*                    ' '  'IMPR-USR02'      wa_user_field-user02,
*                    ' '  'BDC_OKCODE'      '=UPD'.



  PERFORM make_bdc_rtn USING :
                  'X'  'SAPLAIP2' '0500',
                  ' ' 'BDC_CURSOR' 'IMPR-POSID',
                  ' ' 'BDC_OKCODE' '/00',
                  ' '  'IMPR-PRNAM'  g_prnam,
                  ' '  'IMPR-POSID'  u_posid,
                  ' '  'IMPR-GJAHR'  p_gjahr.

  PERFORM make_bdc_rtn USING :
                  'X'  'SAPLAIP2'  '0600',
                  ' ' 'BDC_OKCODE' '=PUSH2',
                  ' '  'IMPU-POST1'  wa_master_data-req_txt,
                  ' '  'IMPR-IZWEK'  it_invest_reson-inv_reason,
                  ' '  'IMPR-PPRIO'  wa_master_data-priority,
                  ' '  'IMPR-SIZECL' wa_master_data-scale,
                  ' '  'IMPR-VERNR' wa_vernr,
                  ' '  'IMPR-ABJHR' wa_master_data-desired_start+0(4).
*                  ' '  'IMPR-BIJHR'  it_variant-completion_date+0(4).
  IF gt_out-ydist EQ 'X'.
    PERFORM make_bdc_rtn USING ' '  'IMPR-XAKTB' 'X'.
  ENDIF.

  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLAIP2'  '0600',
                 ' ' 'BDC_OKCODE' '=PUSH3',
                 ' '  'IMPR-KOSTL'  wa_master_data-rsp_cost_center,
                 ' '  'IMPR-BUKRS'  wa_master_data-rsp_comp_code,
                 ' '  'IMPR-WERKS'  wa_master_data-plant.

  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLAIP2'  '0600',
                 ' ' 'BDC_OKCODE' '=PUSH4',
                 ' ' 'BDC_CURSOR' 'ANIA-ANLKL',
                 ' '  'ANIA-ANLKL'  wa_user_field-user00,
                 ' '  'ANIA-AKTIV' l_date.

  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLAIP2'        '0600',
*                ' '  'IMPR-USR02'      wa_user_field-user02,
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
  CLEAR : c_varnt.
  READ TABLE it_variant_to_version
            WITH KEY appr_year    = p_gjahr
                     plan_version = '000'.
  IF sy-subrc = 0.
    MOVE it_variant_to_version-appreqvrnt  TO c_varnt.
    READ TABLE it_plan_tot WITH KEY appreqvrnt = c_varnt.
    IF sy-subrc = 0.
      wa_amt = it_plan_tot-overhead_costs.
    ENDIF.
  ELSE.
    MOVE 'Check Version'  TO gt_out-proc4.
  ENDIF.
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
*  CONCATENATE '0101' wa_min  INTO wa_f_date.
*  CONCATENATE '1231' wa_max  INTO wa_t_date.
**--------*
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

  READ TABLE it_messtab INDEX 1.
  IF sy-subrc <> 0.
    MOVE 'Created'   TO gt_out-proc5.
    MOVE 'A'   TO gt_out-status5.
  ELSE.
    IF it_messtab-msgtyp = 'S'.
      MOVE 'Created'   TO gt_out-proc5.
      MOVE 'A'   TO gt_out-status5.
    ELSE.
      MOVE 'BDC Error' TO gt_out-proc5.
      MOVE 'Q'   TO gt_out-status5.
    ENDIF.
  ENDIF.


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
** oN 03/19/14
  it_progtree-bal_sheet_item = wa_user_field-user00.
*  it_progtree-bal_sheet_item = wa_user_field-user03.
** End
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
*&      Form  FUND_STEP_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM fund_step_check USING    u_posid.
  READ TABLE it_fmfint WITH KEY   fincode = u_posid+1(10).
  IF sy-subrc = 0.
    MOVE 'Created'      TO  gt_out-proc5.
    MOVE 'A'            TO  gt_out-status5.
  ELSE.
    MOVE 'No Exist'     TO  gt_out-proc5.
    MOVE 'Q'            TO  gt_out-status5.
  ENDIF.
ENDFORM.                    " FUND_STEP_CHECK
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
*----2003/12/18 add
                      ' '  'COAS-IVPRO'       '999',
                      ' '  'COAS-IZWEK'
                              it_invest_reson-inv_reason, "ZZ',
                      ' '  'BDC_OKCODE'      '=BUT4'.

*---------------------*
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMKAUF'        '0600',
*                      ' '  'COAS-USER4'       wa_amt,
*                                        WA_PLAN_TOT-OVERHEAD_COSTS,
*                      ' '  'COAS-USER8'        '31129999',
*                                   it_org_units-req_cost_center,
                      ' '  'BDC_OKCODE'      '=SICH'.

  CALL TRANSACTION tcode   USING it_bdc
                           MODE   'E'
                           UPDATE 'S'
                    MESSAGES INTO it_messtab.

  READ TABLE it_messtab INDEX 1.

  IF sy-subrc <> 0.
    MOVE 'Created'   TO gt_out-proc7.
    MOVE 'A'         TO gt_out-status7.
  ELSE.
    IF it_messtab-msgtyp = 'S'.
      MOVE 'Created'   TO gt_out-proc7.
      MOVE 'A'         TO gt_out-status7.
    ELSE.
      MOVE 'BDC Error' TO gt_out-proc7.
      MOVE 'Q'         TO gt_out-status7.
    ENDIF.
  ENDIF.


ENDFORM.                    " IO_CREATE
*&---------------------------------------------------------------------*
*&      Form  IO_CREATE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM io_create_check USING    u_posid.
  CLEAR wa_aufnr.
*  CONCATENATE '0' u_posid INTO wa_aufnr.
  READ TABLE it_aufk WITH KEY  aufnr = u_posid. "wa_aufnr..
  IF sy-subrc = 0.
    MOVE 'Created'      TO  gt_out-proc7.
    MOVE 'A'            TO  gt_out-status7.
  ELSE.
    MOVE 'No Exist'     TO  gt_out-proc7.
    MOVE 'Q'            TO  gt_out-status7.
  ENDIF.
ENDFORM.                    " IO_CREATE_CHECK
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
*&      Form  PI_TOTAL_UP_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM pi_total_up_rtn.
*  REFRESH : it_bdc, it_messtab.
*  CLEAR   : it_bdc, it_messtab.
*
*  PERFORM make_bdc_rtn USING :
*           'X'  'SAPMKBUD'        '0800',
*           ' '  'IMTP-PRNAM'      g_prnam,
*           ' '  'IMPR-POSID'      GT_OUT-posid+0(3),
*           ' '  'IMPR-GJAHR'      p_gjahr,
*           ' '  'RAIP3-VERSN'     '0',
*           ' '  'BDC_OKCODE'      '/00'.
*
*  PERFORM make_bdc_rtn USING :
*                 'X'  'SAPLAIPA'        '0400',
*                 ' '  'MARKER(01)'      'X',
*                 ' '  'BDC_OKCODE'      '=TAKE'.
*
*  PERFORM make_bdc_rtn USING :
*            'X'  'SAPLKBPP'        '0320',
*            ' '  'BDC_OKCODE'      '=MRKA'.
*
*  PERFORM make_bdc_rtn USING :
*            'X'  'SAPLKBPP'        '0320',
*            ' '  'BDC_OKCODE'      '=SYNC'.
*
*  PERFORM make_bdc_rtn USING :
*            'X'  'SAPLKBPP'        '0705',
*            ' '  'BPDY-TI_JAHR_1'  'X',
*            ' '  'BPDY-JAHR_VON'   '2002',
*            ' '  'BPDY-JAHR_BIS'   '2009',
*            ' '  'BPDY-TI_GES_1'   'X',
*            ' '  'BDC_OKCODE'      '=FORW'.
*
*  PERFORM make_bdc_rtn USING :
*             'X'  'SAPLKBPP'        '0320',
*             ' '  'BDC_OKCODE'      '=POST'.
*
*  CALL TRANSACTION tcode   USING it_bdc
*                  MODE   'E'
*                  UPDATE 'S'
**                    OPTIONS  FROM CTU_PARAMS
*           MESSAGES INTO it_messtab.
*
*ENDFORM.                    " PI_TOTAL_UP_RTN
*&---------------------------------------------------------------------*
*&      Form  create_pi_plan_bdc
*&---------------------------------------------------------------------*
FORM create_pi_plan_bdc USING    u_posid.
*=if inv.program use budget category.
  IF u_posid+0(1) EQ 'P'.
    READ TABLE gt_capex WITH KEY prnam = p_prnam.
  ELSE.
    READ TABLE gt_capex WITH KEY prnam = p_prnam.
  ENDIF.
  CHECK sy-subrc = 0.

  IF gt_capex-capex = 'X'.

    CLEAR wa_d_cnt.
    DESCRIBE TABLE it_plan_year LINES wa_d_cnt.
    CHECK wa_d_cnt > 0.

    CLEAR wa_cnt.
*    DO 2 TIMES.
    wa_cnt = wa_cnt + 1.

    REFRESH : it_bdc, it_messtab.
    CLEAR   : it_bdc, it_messtab.

    PERFORM make_bdc_rtn USING :
             'X'  'SAPMKBUD'        '0800',
             ' '  'IMTP-PRNAM'      g_prnam,
             ' '  'IMPR-POSID'      u_posid,
             ' '  'IMPR-GJAHR'      p_gjahr,
             ' '  'RAIP3-VERSN'     '0',
             ' '  'BDC_OKCODE'      '/00'.
* budget category
    IF wa_cnt = 1.
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
    LOOP AT it_plan_year WHERE appreqvrnt = wa_appreqvrnt.
      CLEAR wa_amt.
      IF wa_cnt = 1.
        wa_amt = ceil( it_plan_year-investment_costs ).
      ELSE.
        wa_amt = ceil( it_plan_year-overhead_costs ).
      ENDIF.

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
    READ TABLE it_plan_tot WITH KEY appreqvrnt = wa_appreqvrnt.
    IF sy-subrc = 0.
      CLEAR wa_amt.
      IF wa_cnt = 1.
        wa_amt = ceil( it_plan_tot-investment_costs ).
      ELSE.
        wa_amt = ceil( it_plan_tot-overhead_costs ).
      ENDIF.

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

* roll-up ????????????????????????????????????????????????
*       PERFORM  pi_total_up_rtn.
*    ENDDO.

*-No budget category used..
  ELSE.
    REFRESH : it_bdc, it_messtab.
    CLEAR   : it_bdc, it_messtab.
    PERFORM make_bdc_rtn USING :
                   'X'  'SAPMKBUD'        '0800',
                   ' '  'IMTP-PRNAM'      g_prnam,
                   ' '  'IMPR-POSID'      u_posid,
                   ' '  'IMPR-GJAHR'      p_gjahr,
                   ' '  'RAIP3-VERSN'     '0',
                   ' '  'BDC_OKCODE'      '/00'.
    PERFORM make_bdc_rtn USING :
                   'X'  'SAPLAIPA'        '0400',
                   ' '  'MARKER(01)'      'X',
                   ' '  'BDC_OKCODE'      '=TAKE'.

    LOOP AT it_plan_year WHERE appreqvrnt = wa_appreqvrnt.
*      CHECK it_plan_year-investment_costs <> 0.
      CLEAR wa_amt.
      wa_amt = ceil( it_plan_year-investment_costs ).
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
    READ TABLE it_plan_tot WITH KEY appreqvrnt = wa_appreqvrnt.
    IF sy-subrc = 0.
      CLEAR wa_amt.
      wa_amt = ceil( it_plan_tot-investment_costs ).
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

*      CALL TRANSACTION tcode   USING it_bdc
*                      MODE   'A'
*                      UPDATE 'S'
**                    OPTIONS  FROM CTU_PARAMS
*               MESSAGES INTO it_messtab.
    ENDIF.
  ENDIF.

ENDFORM.                    " create_pi_plan_bdc

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*   (not for call transaction using...)                                *
*----------------------------------------------------------------------*
FORM open_group.
*  IF SESSION = 'X'.
**   open batchinput group
*    CALL FUNCTION 'BDC_OPEN_GROUP'
*         EXPORTING
*              CLIENT = SY-MANDT
*              GROUP  = 'ZRFII12'
*              USER   = SY-UNAME
*              KEEP   = 'X'. "no delete session if finished
**             HOLDDATE = SPACE.  "lock date
*  ENDIF.
ENDFORM.                    "open_group
*----------------------------------------------------------------------*
*   end batchinput session                                             *
*   (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
FORM close_group.
*  IF SESSION = 'X'.
**   close batchinput group
*    CALL FUNCTION 'BDC_CLOSE_GROUP'.
*  ENDIF.
ENDFORM.                    "close_group
*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
* batch input session
*  IF SESSION = 'X'.
*    CALL FUNCTION 'BDC_INSERT'
*         EXPORTING
*              TCODE     = TCODE
*         TABLES
*              DYNPROTAB = it_bdc.
** call transaction using
*  ELSE.
  CALL TRANSACTION tcode USING it_bdc
                   MODE   'E'        "error/all/no disp
                   UPDATE cupdate    "sync/async/local
                   MESSAGES INTO it_messtab.
*  endif.
ENDFORM.                    "bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  get_cf_amount
*&---------------------------------------------------------------------*
FORM get_cf_amount USING    u_posid.
*  check p_cf = 'X'.
*
*  refresh i_bpja.  clear i_bpja.
** read position infomation
*  read table it_impr with key posid = u_posid.
*
** select carry forward amount
*  select single * from bpge
*     where lednr = '0001'
*       and objnr  = it_impr-objnr
*       and TRGKZ  = 'B'   "C/F
*       and WRTTP  = '47'.  "budget
*  if sy-subrc = 0.
*
** calculate plan = AR plan - C/F budget
*    loop at it_plan_tot where APPREQVRNT = wa_varnt.
*      it_plan_tot-investment_costs =
*                  it_plan_tot-investment_costs - bpge-WTGES.
*      modify it_plan_tot.
*    endloop.
*  endif.
*
** select carry forward amount
*  select * into table i_bpja from bpja
*     where lednr = '0001'
*       and objnr  = it_impr-objnr
*       and TRGKZ  = 'B'   "C/F
*       and WRTTP  = '47'.  "budget
*
** calculate plan = AR plan - C/F budget
** if amount is negative... then '0' and carry forward...
*  data: lc_amt like bpja-wtjhr.
*  clear lc_amt.
*  loop at it_plan_year where APPREQVRNT = wa_varnt.
*    read table i_bpja with key GJAHR = it_plan_year-FISCAL_YEAR.
*
*    it_plan_year-investment_costs =
*                it_plan_year-investment_costs - i_bpja-WTJHR.
*    if lc_amt < 0.
*      it_plan_year-investment_costs =
*                it_plan_year-investment_costs + lc_amt.
*      lc_amt = 0.
*    endif.
*
*    if it_plan_year-investment_costs < 0.
*      lc_amt = it_plan_year-investment_costs.
*      it_plan_year-investment_costs = 0.
*    endif.
*    modify it_plan_year.
*  endloop.
*
ENDFORM.                    " get_cf_amount
*&---------------------------------------------------------------------*
*&      Form  check_BAPI_APPREQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*      <--P_C_FLAG  text
*----------------------------------------------------------------------*
FORM check_bapi_apprequest USING    p_posid
                           CHANGING pc_flag p_err.
  DATA : BEGIN OF it_plan_year OCCURS 0.
          INCLUDE STRUCTURE bapiappreqplanyearmulti.
  DATA : END OF it_plan_year.
  DATA : it_variant_to_version LIKE bapiappreqvarntassignmulti OCCURS 0
                                                 WITH HEADER LINE.

  DATA : wa_master_data LIKE  bapiappreqmaster.
  DATA : wa_user_field       LIKE   bapiapprequser.
  DATA : wa_parent  LIKE bapiprogaux-parent.

  REFRESH :it_plan_year,it_variant_to_version.

  CALL FUNCTION 'BAPI_APPREQUEST_GETDETAIL'
    EXPORTING
      externalnumber     = p_posid
      language           = sy-langu
*     LANGUAGE_ISO       =
    IMPORTING
      master_data        = wa_master_data
      user_fields        = wa_user_field
      controlling_area   = wa_co_area
    TABLES
*     org_units          = it_org_units
*     invest_reason      =  it_invest_reson
*     environmnt_invest  =  it_env_invest
*     variant            = it_variant
      variant_to_version = it_variant_to_version
*     plan_total         = it_plan_tot
      plan_year          = it_plan_year.

  READ TABLE it_variant_to_version
      WITH KEY appr_year    = p_gjahr
               plan_version = p_versn.
  IF sy-subrc = 0.
    pc_flag = 'X'.
*Issue # 20050120-003 requested by robbin changed by wskim,
*on 02/03/2005
*-----Start
*    READ TABLE it_plan_year WITH KEY fiscal_year = p_gjahr
*                                     appreqvrnt  =
*                              it_variant_to_version-appreqvrnt .
*    IF sy-subrc = 0.
*      pc_flag = 'X'.
*    ELSE.
*      pc_flag = ' '.
*    ENDIF.
  ELSE.
    pc_flag = 'X'.
*    MESSAGE I000 WITH 'No assigned variant' p_posid.
    p_err = 'X'.
*-----End

  ENDIF.
ENDFORM.                    " check_BAPI_APPREQUEST
*&---------------------------------------------------------------------*
*&      Form  AR_PI_LINK_PROC_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM ar_pi_link_proc_bapi  USING    u_posid.

* for BAPI ///////////////////////////////////////////////////////////

  DATA : l_posid   TYPE im_posid.
  DATA : lt_assign TYPE TABLE OF bapiappreqexpprogassgn,
         ls_assign TYPE bapiappreqexpprogassgn.
  DATA : lt_budget TYPE TABLE OF bapiappreqexpprogassgnbudgcatg,
         ls_budget TYPE bapiappreqexpprogassgnbudgcatg.
  DATA : lt_return TYPE TABLE OF bapiret2,
         ls_return TYPE bapiret2.
  DATA : lt_tai08  TYPE TABLE OF tai08,
         ls_tai08  TYPE tai08.

*  CALL FUNCTION 'CONVERSION_EXIT_POSID_OUTPUT'
*    EXPORTING
*      INPUT         = u_posid
*    IMPORTING
*      OUTPUT        = l_posid .

  l_posid = u_posid.
  ls_assign-inv_prog         = g_prnam.
  READ TABLE it_impr WITH KEY posid = l_posid.
  IF sy-subrc EQ 0.
    ls_assign-inv_prog         = it_impr-prnam.
  ENDIF.
  ls_assign-appr_year        = p_gjahr.
  ls_assign-program_pos      = l_posid.
  ls_assign-percent_prog_pos = 100.

  APPEND ls_assign TO lt_assign.

  SELECT * INTO TABLE lt_tai08
    FROM tai08
   WHERE prart = 'H201'.

  LOOP AT lt_tai08 INTO ls_tai08.
    CLEAR ls_budget.
    ls_budget-inv_prog         = g_prnam.
    READ TABLE it_impr WITH KEY posid = l_posid.
    IF sy-subrc EQ 0.
      ls_budget-inv_prog         = it_impr-prnam.
    ENDIF.
    ls_budget-appr_year        = p_gjahr.
    ls_budget-program_pos      = l_posid.
    ls_budget-budget_category  = ls_tai08-ippos.

    IF l_posid+0(1) = 'P' AND ls_tai08-ippos = '1'.
      ls_budget-percent_budg_cat = 100.
    ENDIF.
    APPEND ls_budget TO lt_budget.
  ENDLOOP.

  IF gt_out-ydist EQ 'X'.
    CALL FUNCTION 'BAPI_APPREQUEST_ASSGNTOPROGPOS'
      EXPORTING
        externalnumber    = l_posid
*       TEST_RUN          = 'X'
      TABLES
        assignment_to_pos = lt_assign
        return            = lt_return.
  ELSE.
    CALL FUNCTION 'BAPI_APPREQUEST_ASSGNTOPROGPOS'
      EXPORTING
        externalnumber           = l_posid
*       TEST_RUN                 = 'X'
      TABLES
        assignment_to_pos        = lt_assign
        assignment_to_budg_categ = lt_budget
        return                   = lt_return.
  ENDIF.

  READ TABLE lt_return INTO ls_return  WITH KEY type = 'E'.
  IF sy-subrc <> 0.
    MOVE 'A'         TO gt_out-status3.
    MOVE 'Linked'   TO gt_out-proc3.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ELSE.
    MOVE 'Q'         TO gt_out-status3.
    MOVE 'BAPI Error' TO gt_out-proc3.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .

  ENDIF.

ENDFORM.                    " AR_PI_LINK_PROC_BAPI
