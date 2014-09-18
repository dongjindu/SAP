*&---------------------------------------------------------------------*
*& Report  ZITAPM0
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zitapm0.

TABLES: ztitbpml, tadir, tdevc, tstc.

TYPES: BEGIN OF ty_tadir,
         tcode LIKE tstc-tcode,
         pgmna LIKE tstc-pgmna.
TYPES: END OF ty_tadir.

DATA gt_tadir  TYPE ty_tadir OCCURS 0 WITH HEADER LINE.

*dev.class info.
DATA: BEGIN OF gt_tdevc OCCURS 0,
        devclass  LIKE tdevc-devclass,
        ctext     LIKE tdevc-ctext,
      END OF gt_tdevc.
DATA: gt_bpml TYPE ztitbpml OCCURS 0 WITH HEADER LINE.
DATA: gs_bpml TYPE ztitbpml.
DATA: gt_out  TYPE ztitbpml OCCURS 0 WITH HEADER LINE.

*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV

*&---------------------------------------------------------------------*
PARAMETERS: p_objtyp TYPE trobjtype     DEFAULT 'TRAN' NO-DISPLAY.
SELECT-OPTIONS: s_objs  FOR tadir-obj_name.

INITIALIZATION.
  s_objs-low = 'Z*'.
  s_objs-option = 'CP'.
  s_objs-sign = 'I'.
  APPEND s_objs.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  SELECT tdevc~devclass tdevct~ctext INTO TABLE gt_tdevc
    FROM tdevc INNER JOIN tdevct
      ON tdevc~devclass = tdevct~devclass
    WHERE spras = sy-langu.
  SORT gt_tdevc BY devclass.

  SELECT tcode pgmna INTO TABLE gt_tadir FROM tadir
     INNER JOIN tstc
        ON obj_name = tstc~tcode
     WHERE pgmid  = 'R3TR'
       AND object = p_objtyp
       AND obj_name IN s_objs
     ORDER BY devclass obj_name.

  SELECT * INTO TABLE gt_bpml FROM ztitbpml.
  SORT gt_bpml BY tcode.

  DATA: l_idx LIKE sy-index.
  LOOP AT gt_tadir.
    l_idx = sy-tabix.

    READ TABLE gt_bpml WITH KEY tcode = gt_tadir-tcode BINARY SEARCH.
    IF sy-subrc <> 0.
      CLEAR gs_bpml.
      gs_bpml-tcode = gt_tadir-tcode.
      PERFORM get_tcode_info(zitbpml)  USING gs_bpml.
      APPEND gs_bpml TO gt_out.

    ENDIF.

  ENDLOOP.

  PERFORM display_out.

*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
FORM display_out.
*  PERFORM field_setting TABLES gt_fieldcat USING :
* 'WERKS'     'Plant'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
* 'BKLAS'     'Val.C'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
* 'MATNR'     'Material'       '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
* 'MENGE'     'Qty'            '10' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
* 'VA000'     'Var'            '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
* 'PBPOPO'    'Qty SubAdj'     '10' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
* 'NDIS'      'MatOvhd-NDI'    '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
* 'NINS'      'MatOvhd-NIN'    '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
* 'COGS'      'COGS'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
* 'HKONT'     'Account'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  'X'.

* build event table.
  PERFORM alv_build_eventtab CHANGING gt_events.
  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
*     it_fieldcat        = gt_fieldcat
      i_structure_name   = 'ZTITBPML'
      it_events          = gt_events
      i_save             = 'A'
    TABLES
      t_outtab           = gt_out
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " display_out
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES         p_fieldcat_t LIKE gt_fieldcat
                   USING          p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO p_fieldcat_t.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD_EVENTTAB
*&---------------------------------------------------------------------*
FORM alv_build_eventtab CHANGING et_alv_events TYPE slis_t_event.

  DATA: ls_alv_events    TYPE slis_alv_event,
        l_dummy_ucomm    LIKE sy-ucomm,
        l_dummy_selfield TYPE slis_selfield,
        l_dummy_excl_tab TYPE slis_t_extab.

  REFRESH: et_alv_events.

* event 'USER_COMMAND'.
  CLEAR ls_alv_events.
  ls_alv_events-name = slis_ev_user_command.
  ls_alv_events-form = 'ALV_USER_COMMAND'.
  APPEND ls_alv_events TO et_alv_events.

ENDFORM.                               " ALV_BUILD_EVENTTAB
*---------------------------------------------------------------------*
*       FORM ALV_USER_COMMAND                                         *
*---------------------------------------------------------------------*
*       user command for ALV processing                               *
*---------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  DATA: ls_out LIKE gt_out.
  DATA: bdcdata_wa  TYPE bdcdata,
        bdcdata_tab TYPE TABLE OF bdcdata.
  DATA opt TYPE ctu_params.

  CHECK r_ucomm = '&IC1'.                       "Pick
  READ TABLE gt_out INDEX rs_selfield-tabindex INTO ls_out.
  CHECK sy-subrc = 0.

  IF rs_selfield-fieldname = 'PGMNA'.
    CLEAR bdcdata_wa.
    bdcdata_wa-program = 'SAPLWBABAP'.
    bdcdata_wa-dynpro  = '0100'.
    bdcdata_wa-dynbegin = 'X'.
    APPEND bdcdata_wa TO bdcdata_tab.


    CLEAR bdcdata_wa.
    bdcdata_wa-fnam = 'BDC_CURSOR'.
    bdcdata_wa-fval = 'RS38M-FUNC_HEAD'.
    APPEND bdcdata_wa TO bdcdata_tab.

    CLEAR bdcdata_wa.
    bdcdata_wa-fnam = 'RS38M-PROGRAMM'.
    bdcdata_wa-fval = rs_selfield-value.
    APPEND bdcdata_wa TO bdcdata_tab.

    CLEAR bdcdata_wa.
    bdcdata_wa-fnam = 'RS38M-FUNC_HEAD'.
    bdcdata_wa-fval = 'X'.
    APPEND bdcdata_wa TO bdcdata_tab.

    CLEAR bdcdata_wa.
    bdcdata_wa-fnam = 'BDC_OKCODE'.
    bdcdata_wa-fval = '=SHOP'.
    APPEND bdcdata_wa TO bdcdata_tab.

    opt-dismode = 'E'.
    opt-defsize = 'X'.

    CALL TRANSACTION 'SE38' USING bdcdata_tab OPTIONS FROM opt.

  ELSE.
    SET PARAMETER ID 'TCD' FIELD ls_out-tcode.
    CALL TRANSACTION 'SE93' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                               "ALV_USER_COMMAND
