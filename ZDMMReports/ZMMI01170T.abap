*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZMMI01170T
*& Program Name   : Non Purchase part number from G-pos
*& Created by     : T00304
*& Created on     : 08.08.2013
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. : Z_MM_IF_IB_09_NO_PURCHASE
*& Stru.     : ZMMS_GS004
*&----------------------------------------------------------------------

REPORT zmmi01170t MESSAGE-ID zmpp.

TABLES : zmmt_gs004.                   "Material Info Record

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
DATA : g_repid         LIKE sy-repid.

DATA : BEGIN OF lt_mseg OCCURS 100,
        matnr  LIKE mseg-matnr,
        lifnr  LIKE mseg-lifnr,
        shkzg  LIKE mseg-shkzg,
        zbudat LIKE mseg-zbudat,
        menge  LIKE mseg-menge,
        meins  LIKE mseg-meins,
       END   OF lt_mseg.

DATA : BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zmmt_gs004.
DATA : descr TYPE char40.
DATA : END OF it_data.

DATA : it_send    LIKE zmms_gs003 OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS : p_bukrs     LIKE mseg-bukrs MEMORY ID buk OBLIGATORY
                         DEFAULT 'H201' .
SELECT-OPTIONS : s_zcebes FOR zmmt_gs004-zcebes  ,
                 s_matnr  FOR zmmt_gs004-matnr   ,
                 s_zrslt  FOR zmmt_gs004-zrslt   ,
                 s_erdat  FOR zmmt_gs004-erdat   .

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

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_spmon-low.
*  PERFORM pov_month USING s_spmon-low.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.
  PERFORM modify_data.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM pro_alv.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM zmmt_gs004
  WHERE bukrs    EQ  p_bukrs
    AND zcebes   IN  s_zcebes
    AND matnr    IN  s_matnr
    AND zrslt    IN  s_zrslt
    AND erdat    IN  s_erdat .

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .

  LOOP AT it_data.
    CASE it_data-zcebes.
      WHEN 'J'. it_data-descr  = 'Module Parts'.
      WHEN 'A'. it_data-descr  = 'BOM error(SPEC)'.
      WHEN 'B'. it_data-descr  = 'BOM error(Parts Number)'.
      WHEN 'C'. it_data-descr  = 'BOM error(SUB Parts)'.
      WHEN 'D'. it_data-descr  = 'BOM error(installation diagram)'.
      WHEN 'E'. it_data-descr  = 'Team or Vender error'.
      WHEN 'F'. it_data-descr  = 'Oversea Developing Parts'.
      WHEN 'G'. it_data-descr  = 'Discontinued Parts'.
      WHEN 'H'. it_data-descr  = 'Mutual Exchange Parts'.
      WHEN 'I'. it_data-descr  = 'ETC'.
    ENDCASE.

    MODIFY it_data.
  ENDLOOP.

ENDFORM.                    " MODIFY_DATA
*&--------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .
  CHECK sy-batch EQ space.

  PERFORM layout_build       USING   gs_layout.
*  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'ZMMT_GS004'.

  PERFORM list_header_write USING alv_t_listheader[].
  PERFORM append_alv_event  CHANGING   gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
*      i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command  = g_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort
      i_save                   = g_save
      it_events                = gt_events[]
    TABLES
      t_outtab                 = it_data[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.                    " PRO_ALV
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
FORM layout_build  USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.
*  p_layout-key_hotspot = 'X'.
*  p_layout-box_fieldname  =    'MARK'.  "SELECTION FIELD
*  p_layout-coltab_fieldname = 'COL_COLOR'. "color field of itabe
*  p_layout-cell_merge        = 'X'.
*  p_layout-detail_popup      = 'X'.
*  p_layout-detail_titlebar   = sy-title.
*  p_layout-no_subtotals      = ''.

ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SORTTAB_BUILD
*&---------------------------------------------------------------------*
FORM sorttab_build  USING   p_sort TYPE slis_t_sortinfo_alv.

  CLEAR: gs_sort, p_sort[].


  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_SEND'.
  gs_sort-fieldname = 'MATNR'.
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
      i_program_name        = g_repid
      i_structure_name      = p_name
      i_bypassing_buffer    = 'X'
*      i_internal_tabname = p_name
*      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = pt_fieldcat[].

  LOOP AT pt_fieldcat.
    CASE pt_fieldcat-fieldname.
      WHEN 'ZCEBES'.
        pt_fieldcat-fieldname  = 'DESCR'.
        pt_fieldcat-seltext_l  = 'Description'.
        pt_fieldcat-seltext_m  = 'Description'.
        pt_fieldcat-seltext_s  = 'Desc'.

        pt_fieldcat-intlen        = '40'.

        pt_fieldcat-ref_fieldname = ''.
        pt_fieldcat-ref_tabname   = ''.

        pt_fieldcat-reptext_ddic   = 'Description'.
        pt_fieldcat-ddic_outputlen  = '40'.

        APPEND  pt_fieldcat.

      WHEN 'SNAME'.
        pt_fieldcat-seltext_l  = 'Employee Name'.
        pt_fieldcat-seltext_m  = 'Employee Name'.
        pt_fieldcat-seltext_s  = 'Name'.
        pt_fieldcat-reptext_ddic   = 'Name'.

        MODIFY pt_fieldcat.

      WHEN 'PERNR'.
        pt_fieldcat-seltext_l  = 'Employee ID'.
        pt_fieldcat-seltext_m  = 'Employee ID'.
        pt_fieldcat-seltext_s  = 'Employee'.
        pt_fieldcat-reptext_ddic   = 'Employee'.

        MODIFY pt_fieldcat.
      WHEN 'ZRSLT'.
        pt_fieldcat-seltext_l  = 'IF Result'.
        pt_fieldcat-seltext_m  = 'IF Result'.
        pt_fieldcat-seltext_s  = 'IF Result'.
        pt_fieldcat-reptext_ddic   = 'IF Result'.

        MODIFY pt_fieldcat.

      WHEN 'ZMSG'.
        pt_fieldcat-seltext_l  = 'IF Msg'.
        pt_fieldcat-seltext_m  = 'IF Msg'.
        pt_fieldcat-seltext_s  = 'IF Msg'.
        pt_fieldcat-reptext_ddic   = 'IF Msg'.

        MODIFY pt_fieldcat.

      WHEN OTHERS.

    ENDCASE.

  ENDLOOP.


ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM list_header_write  USING alv_t_listheader TYPE slis_t_listheader.

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DATA : h_title(30), s_title(60),  a_title(60).
  DATA : lv_date(10),
         lv_lines(10),
         lv_count TYPE i .

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  ls_title-key = 'Company Code '.
  ls_title-info = 'A1'.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  WRITE  sy-datum TO lv_date.
  ls_title-key = 'Date'.
  ls_title-info = lv_date.
  APPEND ls_title TO alv_t_listheader.




  DESCRIBE TABLE it_send LINES  lv_count.
  WRITE lv_count TO lv_lines.
  ls_title-typ = 'S'.
  ls_title-key = ''.
  CONCATENATE '*Lines : ' lv_lines  INTO ls_title-info.
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
*&      Form  PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pf_status USING lt_extab  TYPE slis_t_extab  .

  SET PF-STATUS 'PF_001'
    EXCLUDING lt_extab.

ENDFORM.                    "PF_STATUS
*&--------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&--------------------------------------------------------------------*
FORM  user_command USING ucomm    LIKE sy-ucomm
                    p_selfield    TYPE slis_selfield.
* double click : UCOMM = &IC1
  CASE ucomm.

    WHEN 'ZSEND'.


      p_selfield-refresh    = 'X'.
      p_selfield-col_stable = 'X'.
      p_selfield-row_stable = 'X'.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .
  g_repid = sy-repid.

ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  CAL_MONTH_LASTDATE
*&---------------------------------------------------------------------*
FORM cal_month_lastdate  USING    p_spmon
                                  p_edate.
  DATA : lv_date TYPE sy-datum.
  CONCATENATE p_spmon '01' INTO lv_date.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_date
    IMPORTING
      last_day_of_month = p_edate
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CAL_MONTH_LASTDATE
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_BAR
*&---------------------------------------------------------------------*
FORM progress_bar  USING    p_tabix
                            p_lines.
  DATA : lv_per TYPE i .
  DATA : lv_text(50).
  DATA : lv_mode TYPE i.
  DATA : lv_lines(7) TYPE n,
         lv_tabix(7)  TYPE n.

  lv_lines  = p_lines.
  lv_tabix  = p_tabix.

  lv_per = ( p_tabix * 100 / p_lines ) .
  CONCATENATE 'Processing : ' lv_tabix ' / ' lv_lines INTO lv_text.
  lv_mode =  lv_per MOD 5.
  IF lv_mode   =  0.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_per
        text       = lv_text.
  ENDIF.
ENDFORM.                    " PROGRESS_BAR
*&---------------------------------------------------------------------*
*&      Form  POV_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_SPMON_LOW  text
*----------------------------------------------------------------------*
FORM pov_month  USING pv_spmon.
  DATA: lv_spmon TYPE spmon.

  MOVE: sy-datum(6) TO lv_spmon.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month               = lv_spmon
    IMPORTING
      selected_month             = pv_spmon
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      month_not_found            = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " POV_MONTH
