*&---------------------------------------------------------------------*
*& Report  ZRPP_SPEC_APS
*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZRPP_SPEC_APS
*& Program Name   : Spec Order Interface to APS
*& Created by     : Victor Park
*& Created on     : 05.28.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*&
*&----------------------------------------------------------------------

REPORT  zrpp_spec_aps MESSAGE-ID zmpp.

TABLES : ztpp_wosum.

*- ALV
TYPE-POOLS: slis.
DATA: gt_fieldcat         TYPE slis_t_fieldcat_alv,
      gs_layout           TYPE slis_layout_alv,
      gs_sort             TYPE slis_sortinfo_alv,
      gt_sort             TYPE slis_t_sortinfo_alv,
      gs_light            TYPE lvc_s_layo,
      gs_print            TYPE slis_print_alv,
      gt_sp_group         TYPE slis_t_sp_group_alv,
      gt_events           TYPE slis_t_event,
      gs_events           LIKE  LINE OF gt_events,
      g_save              VALUE 'A',
      gx_variant          LIKE disvariant,
      g_variant           LIKE disvariant.

DATA : ls_title         TYPE slis_listheader, "alv header
       alv_t_listheader TYPE slis_t_listheader.

DATA : g_extab          TYPE slis_t_extab,
       g_extab_ln       LIKE   LINE  OF  g_extab.

DATA : g_user_command  TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : t_colinfo_table TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA : g_status        TYPE slis_formname VALUE 'ALV_STATUS'.
DATA : g_repid         LIKE sy-repid.

DATA : BEGIN OF it_spec OCCURS 0,
         wo_ser  LIKE ztpp_wosum-wo_ser,
         nation  LIKE ztpp_wosum-nation,
         dealer  LIKE ztpp_wosum-dealer,
         extc    LIKE ztpp_wosum-extc,
         intc    LIKE ztpp_wosum-intc,
         modqty  LIKE ztpp_wosum-modqty,
         fsc     LIKE ztpp_wosum-fsc,
         wocredate LIKE ztpp_wosum-wocredate,
         s219      LIKE zspseg-s219,
      END OF it_spec.

DATA: it_result  LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_woser FOR ztpp_wosum-wo_ser
                                   DEFAULT 'E*Z*' OPTION CP SIGN I
                                    OBLIGATORY,
                 s_nation FOR ztpp_wosum-nation
                                  NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b1.


*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.


*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.
  PERFORM modify_data.

  PERFORM pro_alv.
*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
  RANGES : r_woser FOR ztpp_wosum-wo_ser.

  CLEAR : r_woser[], r_woser.

  r_woser-sign    = 'I'.
  r_woser-option  = 'CP'.
  r_woser-low     = 'E++++Z*'.
  APPEND r_woser.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_spec
  FROM ztpp_wosum
  WHERE wo_ser IN s_woser
    AND wo_ser IN r_woser
    AND nation IN s_nation.

  IF it_spec[] IS INITIAL.
    MESSAGE s000 WITH 'No data found'.
    STOP.
  ENDIF.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .

  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'IT_SPEC'.
  PERFORM list_header_write USING alv_t_listheader[].
  PERFORM append_alv_event  CHANGING   gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
      i_callback_pf_status_set = g_status
      i_callback_user_command  = g_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort
      i_save                   = g_save
*     is_variant               = g_variant
      it_events                = gt_events[]
    TABLES
      t_outtab                 = it_spec[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.                    " PRO_ALV
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
FORM layout_build USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.

ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SORTTAB_BUILD
*&---------------------------------------------------------------------*
FORM sorttab_build  USING   p_sort TYPE slis_t_sortinfo_alv.

  CLEAR: gs_sort, p_sort[].

  gs_sort-spos      = '1'.
  gs_sort-tabname   = 'IT_SPEC'.
  gs_sort-fieldname = 'WO_SER'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_SPEC'.
  gs_sort-fieldname = 'NATION'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.
ENDFORM.                    " SORTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat  TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv
               USING    p_name      TYPE  slis_tabname.

  DATA: l_datum(08).

  sy-datum = sy-datum + 1.
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
*     i_structure_name   = p_name
      i_internal_tabname = p_name
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = pt_fieldcat[].

  LOOP AT pt_fieldcat.
    CASE pt_fieldcat-fieldname.
      WHEN 'WO_SER'.
        pt_fieldcat-seltext_m    = 'Spec Order'.
      WHEN 'S219'.
        pt_fieldcat-seltext_m    = '219 Code'.
      WHEN OTHERS.

    ENDCASE.
    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.

    MODIFY pt_fieldcat.

  ENDLOOP.

ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM list_header_write  USING alv_t_listheader TYPE slis_t_listheader.

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DATA : h_title(30), s_title(60),  a_title(60).

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*Spec Order : '  s_woser-low INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

ENDFORM.                    " LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
*&      Form  APPEND_ALV_EVENT
*&---------------------------------------------------------------------*
FORM append_alv_event  CHANGING p_alv_event TYPE slis_t_event.
* TOP-OF-PAGE Event

  DATA ls_events TYPE slis_alv_event.
  ls_events-name  =  'TOP_OF_PAGE'.
  ls_events-form  =  'TOP_OF_PAGE'.
  APPEND ls_events TO p_alv_event.

ENDFORM.                    " APPEND_ALV_EVENT

*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = alv_t_listheader.

ENDFORM. " TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  alv_status
*&---------------------------------------------------------------------*
*      main  pf status
*----------------------------------------------------------------------*
FORM alv_status USING extab TYPE slis_t_extab .
  DATA : l_extab TYPE slis_extab.

*    l_extab-fcode = 'SAVE'.
*    append l_extab to extab.

  SET PF-STATUS '0100' EXCLUDING extab.
ENDFORM.                    " alv_status


*&--------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&--------------------------------------------------------------------*
FORM  user_command USING ucomm    LIKE sy-ucomm
                    p_selfield    TYPE slis_selfield.
* double click : UCOMM = &IC1
  CASE ucomm.
    WHEN 'SEND'.
      PERFORM call_programs.
  ENDCASE.

  p_selfield-refresh = 'X'.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .

  g_repid  = sy-repid.
ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA: l_wo LIKE mara-matnr.

  LOOP AT it_spec.
    CONCATENATE it_spec-wo_ser it_spec-nation it_spec-dealer
                                           INTO l_wo.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object       = l_wo
        ctype        = '001'
      TABLES
        val_table    = it_result
      EXCEPTIONS
        no_data      = 1
        error_mode   = 2
        error_object = 3
        OTHERS       = 4.

    IF sy-subrc = 0.
      PERFORM fill_219.
      CLEAR: it_result, it_result[].

      MODIFY it_spec.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_219
*&---------------------------------------------------------------------*
FORM fill_219 .
  PERFORM fill_1_to_9.
  PERFORM fill_10_to_99.
  PERFORM fill_100_to_219.
ENDFORM.                    " FILL_219
*&---------------------------------------------------------------------*
*&      Form  FILL_10_TO_99
*&---------------------------------------------------------------------*
FORM fill_10_to_99 .
  FIELD-SYMBOLS: <field_pos> ,
                 <field_val> .

  DATA: l_name(20)             TYPE c,
        l_cntidx(2)            TYPE n,
        l_219_digit(2)         TYPE n.

  l_cntidx = 9.
  DO 90 TIMES .
    l_cntidx = l_cntidx + 1.
    l_219_digit = l_cntidx - 1.
    CONCATENATE 'P_219_' l_cntidx INTO l_name .
    READ TABLE it_result  WITH KEY atnam = l_name .
    CONCATENATE 'IT_SPEC-S219+' l_219_digit '(1)' INTO l_name .
    ASSIGN (l_name)       TO        <field_val> .
    IF sy-subrc = 0 .
      <field_val>          =  it_result-atwrt .
    ENDIF.
  ENDDO.
ENDFORM.                    " FILL_10_TO_99
*&---------------------------------------------------------------------*
*&      Form  FILL_1_TO_9
*&---------------------------------------------------------------------*
FORM fill_1_to_9 .
  FIELD-SYMBOLS: <field_pos> ,
                 <field_val> .

  DATA: l_name(20)             TYPE c,
        l_cntidx(1)            TYPE n,
        l_219_digit(1)         TYPE n.

  DO 9 TIMES .
    l_cntidx    = l_cntidx + 1.
    l_219_digit = l_cntidx - 1.
    CONCATENATE 'P_219_' l_cntidx INTO l_name .
    READ TABLE it_result  WITH KEY atnam = l_name .
    CONCATENATE 'IT_SPEC-S219+' l_219_digit '(1)' INTO l_name .
    ASSIGN (l_name)       TO        <field_val>  .
    IF sy-subrc = 0 .
      <field_val>          =  it_result-atwrt  .
    ENDIF.
  ENDDO.
ENDFORM.                    " FILL_1_TO_9
*&---------------------------------------------------------------------*
*&      Form  FILL_100_TO_219
*&---------------------------------------------------------------------*
FORM fill_100_to_219 .
  FIELD-SYMBOLS: <field_pos> ,
                 <field_val> .

  DATA: l_name(20)             TYPE c,
        l_cntidx(3)            TYPE n,
        l_219_digit(3)         TYPE n.

  l_cntidx = 99.
  DO 120 TIMES .
    l_cntidx = l_cntidx + 1.
    l_219_digit = l_cntidx - 1.
    CONCATENATE 'P_219_' l_cntidx INTO l_name .
    READ TABLE it_result  WITH KEY atnam = l_name .
    CONCATENATE 'IT_SPEC-S219+' l_219_digit '(1)' INTO l_name .
    ASSIGN (l_name)       TO        <field_val>  .
    IF sy-subrc = 0 .
      <field_val>          =  it_result-atwrt  .
    ENDIF.
  ENDDO.
ENDFORM.                    " FILL_100_TO_219
*&---------------------------------------------------------------------*
*&      Form  CALL_PROGRAMS
*&---------------------------------------------------------------------*
FORM call_programs .
  RANGES : r_woser FOR ztpp_wosum-wo_ser.
  DATA : l_type(1),
         l_msg1(70), l_msg2(70).

  CLEAR : l_msg1, l_msg2.

  LOOP AT it_spec.
    AT NEW wo_ser.
      r_woser-sign = 'I'.
      r_woser-option = 'EQ'.
      r_woser-low    = it_spec-wo_ser.
      APPEND r_woser.
    ENDAT.
  ENDLOOP.

  SUBMIT zipp105i_aps_3fb1_wan WITH p_run = 'X'
                               WITH s_wo_ser IN r_woser
                               AND RETURN.

  SUBMIT zipp104i_aps_3bb1 WITH p_run = 'X'
                               AND RETURN.

  SUBMIT zipp105i_aps_3fb2_2 WITH p_run = 'X'
                               AND RETURN.

  SUBMIT zipp104i_aps_3bb2_2 WITH p_run = 'X'
                               AND RETURN.

*-message handling
  SELECT logtype INTO l_type
  FROM ztpp_pp_log_head
    UP TO 1 ROWS
  WHERE programm = 'ZIPP105I_APS_3FB2_2'
    AND ldate    = sy-datum
  ORDER BY ltime DESCENDING.
  ENDSELECT.
  IF l_type = 'E'.
    l_msg1  = ' Transfer PMT03FB to APS'.
  ENDIF.

  SELECT logtype INTO l_type
  FROM ztpp_pp_log_head
    UP TO 1 ROWS
  WHERE programm = 'ZIPP104I_APS_3BB2_2'
    AND ldate    = sy-datum
  ORDER BY ltime DESCENDING.
  ENDSELECT.
  IF l_type = 'E'.
    l_msg2  = ' Transfer PMT03BB to APS'.
  ENDIF.

  IF l_msg1 IS INITIAL AND l_msg2 IS INITIAL.
    MESSAGE s000 WITH 'APS programs have been called successfully'.
  ELSEIF l_msg1 IS NOT INITIAL AND l_msg2  IS NOT INITIAL.
    MESSAGE e000 WITH 'I/F Fail:' l_msg1 '&' l_msg2.
  ELSE.
    MESSAGE e000 WITH 'I/F Fail:' l_msg1 l_msg2.
  ENDIF.
ENDFORM.                    " CALL_PROGRAMS
