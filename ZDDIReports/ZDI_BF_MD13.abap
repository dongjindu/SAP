*&---------------------------------------------------------------------*
*& Report  ZDI_BF_MD13
*&  - Display Planned Order
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*
* spec by Andy Choi
*

REPORT  zdi_bf_md13 MESSAGE-ID zmpp.


PARAMETERS:
  p_plnum LIKE ppc_comp_conf-plnum OBLIGATORY MEMORY ID paf.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-sl1.
PARAMETERS:
  p_sort  AS CHECKBOX DEFAULT ' ',
  p_reset AS CHECKBOX default ' '.
SELECTION-SCREEN END OF BLOCK b1.
*&---------------------------------------------------------------------*

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

DATA: ls_plaf TYPE ppc1tp_plaf,
      gt_resb TYPE ppc1tp_resb OCCURS 0 WITH HEADER LINE.
*DATA: BEGIN OF gt_plaf_resb OCCURS 0,
*	plnum      TYPE	plnum,
*	head_matnr TYPE	plmat,
*	gsmng      TYPE	gsmng,
*	wemng      TYPE	wemng,
*	wamng      TYPE	wamng.
*        INCLUDE STRUCTURE ppc1tp_resb.
*DATA: END OF gt_plaf_resb.

*&---------------------------------------------------------------------*
DATA: h_dontpanic   LIKE sy-datlo.
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*&---------------------------------------------------------------------*
  LOOP AT SCREEN.
    IF screen-name = 'P_RESET' or screen-name = 'P_SORT'.
      GET PARAMETER ID 'DONTPANIC' FIELD h_dontpanic.
      IF h_dontpanic = sy-datlo.
        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*

  SELECT SINGLE * FROM ppc1tp_plaf
    INTO ls_plaf
    WHERE plnum = p_plnum.

  IF p_sort = 'X' OR p_reset = 'X'.
    PERFORM update_resb_sortf.
  ENDIF.

  SELECT * FROM ppc1tp_resb
      INTO TABLE gt_resb
      WHERE rsnum = ls_plaf-rsnum.

  SORT gt_resb BY sortf prvbe matnr.

  PERFORM display_out.


*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
FORM display_out.

*  PERFORM field_setting TABLES gt_fieldcat USING :
*'MATNR' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'WERKS' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'LGORT' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'PRVBE' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'SOBKZ' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'BDMNG' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'MEINS' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'SHKZG' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'ENMNG' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'ERFMG' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'ERFME' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'BWART' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'SORTF' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
*'ESMNG' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
*      it_fieldcat        = gt_fieldcat
      i_structure_name   = 'ppc1tp_resb'
      i_save             = 'A'
    TABLES
      t_outtab           = gt_resb
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

  IF p_dosum = 'R'. "reference field / table
    ls_fieldcat-ref_tabname   = p_cfield.
    ls_fieldcat-ref_fieldname = p_qfield.
  ELSE.
    ls_fieldcat-currency   = p_cfield.
    ls_fieldcat-qfieldname = p_qfield.

  ENDIF.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO p_fieldcat_t.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  UPDATE_RESB_SORTF
*&---------------------------------------------------------------------*
FORM update_resb_sortf .
  DATA:
      ls_plaf     TYPE plaf.
  DATA: idx LIKE sy-tabix.
* read RP
  DATA: lv_ppc_rp TYPE ppc_rp,
        lv_reppoint TYPE ppc_reppoint_int,
        lv_sortp  TYPE sortp,
        lv_prvbe  TYPE prvbe.
  DATA: lt_kmmg05 LIKE ztpp0001 OCCURS 0 WITH HEADER LINE,
        lt_kmmg07 LIKE ztpp0001 OCCURS 0 WITH HEADER LINE,
        ls_kmmg TYPE ztpp0001.
  DATA: BEGIN OF lt_prvbe OCCURS 0,
          prvbe  TYPE prvbe,
          sortp  TYPE sortp,
        END OF lt_prvbe.

  CALL FUNCTION 'GET_PLAF_WITH_PLNUM'
    EXPORTING
      plnum                  = p_plnum
    IMPORTING
      oplaf                  = ls_plaf
    EXCEPTIONS
      no_plnum_specified     = 1
      object_not_found       = 2
      plaf_logically_deleted = 3
      OTHERS                 = 4.
  IF sy-subrc <> 0.
    MESSAGE e000(zmpp) WITH p_plnum ' is not found' RAISING order_error.
  ENDIF.

*
  SELECT * INTO TABLE lt_kmmg05 FROM ztpp0001 WHERE code = '05'.
  SELECT * INTO TABLE lt_kmmg07 FROM ztpp0001 WHERE code = '07'.
  SORT lt_kmmg05 BY key1.

  LOOP AT lt_kmmg07.
    idx = sy-tabix.
    READ TABLE lt_kmmg05 WITH KEY key1 = lt_kmmg07-key2. "nary search.
    IF sy-subrc = 0.
      lt_kmmg07-key5 = lt_kmmg05-key2+2(2).
      MODIFY lt_kmmg07 INDEX idx TRANSPORTING key5.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_kmmg07.
    IF p_sort = 'X'.
      UPDATE ppc1tp_resb
              SET sortf = lt_kmmg07-key5
              WHERE rsnum = ls_plaf-rsnum
                AND prvbe = lt_kmmg07-key1.
    ENDIF.
*
    IF p_reset = 'X'.
      UPDATE ppc1tp_resb
              SET enmng = 0
              WHERE rsnum = ls_plaf-rsnum
                AND prvbe = lt_kmmg07-key1.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " UPDATE_RESB_SORTF
