*&---------------------------------------------------------------------*
*& Report  ZMMR_IF_MM_LOG001                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zmmr_if_mm_log002 MESSAGE-ID 00
        NO STANDARD PAGE HEADING.

************************************************************************
*                       TABLE & VARIANTS                               *
************************************************************************
TYPE-POOLS : slis.

CONTROLS: tc1100 TYPE TABLEVIEW USING SCREEN '1100'.

TABLES: adrp,
        lfa1,
        t001,
        t001l,
        t001w,
        t023t,
        t024,
        t024e,
        t052u,
        t134t,                " Material Type Descriptions
        t685t,
        tinct,
        usr21,
        ztmm_if001,           " Material Master Log Table
        ztmm_if015,           " Material Master Log Message Table
        ztmm_if020.           " Material Master Classfication Data

RANGES: ra_ztype FOR ztmm_if015-type.

DATA: gt_if001 LIKE ztmm_if001 OCCURS 0 WITH HEADER LINE,
      gt_if015 LIKE ztmm_if015 OCCURS 0 WITH HEADER LINE,
      gt_if020 LIKE ztmm_if020 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gt_ztype OCCURS 0,
      zseq     LIKE ztmm_if015-zseq,
      type     LIKE ztmm_if015-type.
DATA: END OF gt_ztype.

DATA: BEGIN OF gt_maind OCCURS 0,
      icon(4).
        INCLUDE STRUCTURE ztmm_if001.
DATA: chkbx,
      waers   LIKE t001-waers,
      END OF gt_maind.

DATA: BEGIN OF st_if001,
        icon(4).
        INCLUDE STRUCTURE ztmm_if001.
DATA: waers   LIKE t001-waers,
      END OF st_if001.

DATA: BEGIN OF st_ntext,
      mtbez    LIKE t134t-mtbez,
      name1    LIKE t001w-name1,
      lgobe    LIKE t001l-lgobe,
      wgbez    LIKE t023t-wgbez60.
DATA: END OF st_ntext.

*... ALV parameter
DATA: g_fieldcat_t        TYPE slis_t_fieldcat_alv,
      g_events_t          TYPE slis_t_event,
      g_keyinfo_s         TYPE slis_keyinfo_alv,
      g_evts_exit_s       TYPE slis_t_event_exit,
      gt_list_top_of_page TYPE slis_t_listheader,
      gt_list_top_of_list TYPE slis_t_listheader,
      g_sort_t            TYPE slis_t_sortinfo_alv,
      g_sort_s            TYPE slis_sortinfo_alv,
      g_layout_s          TYPE slis_layout_alv,
      g_vari              TYPE disvariant,
      gs_gridset          TYPE lvc_s_glay,
      gt_exclude          TYPE slis_t_extab.

DATA: g_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
      g_top_of_list  TYPE slis_formname,
      g_status       TYPE slis_formname VALUE 'SET_PF_STATUS',
      g_user_command TYPE slis_formname VALUE 'USER-COMMAND',
      g_data_changed TYPE slis_formname VALUE 'DATA_CHANGED'.

DATA: g_repid LIKE sy-repid,
      g_tabname TYPE slis_tabname,
      pos       TYPE i VALUE 1.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      alv_grid           TYPE REF TO cl_gui_alv_grid.

DATA: g_st_detail_layout TYPE lvc_s_layo,
      g_st_detail_fdcat  TYPE lvc_s_fcat,
      g_it_detail_fdcat  TYPE lvc_t_fcat.

DATA: fieldname(30),
      fieldvalu(10).

DATA : ok_code LIKE sy-ucomm.


************************************************************************
*                        SELECT OPTION                                 *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-m01.
SELECT-OPTIONS: sa_matnr FOR ztmm_if001-matnr,
                sa_zzseq FOR ztmm_if001-zseq NO-EXTENSION NO INTERVALS,
                sa_erdat FOR ztmm_if001-erdat,
                sa_ernam FOR ztmm_if001-ernam NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-m02.
PARAMETERS: pa_rbtn1 RADIOBUTTON GROUP gr1 DEFAULT 'X',  " s
            pa_rbtn2 RADIOBUTTON GROUP gr1,              " e
            pa_rbtn4 RADIOBUTTON GROUP gr1,              " re-processing
            pa_rbtn3 RADIOBUTTON GROUP gr1.              " a
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN.
  PERFORM check_select_option.

************************************************************************
*                         MAIN PROGRAM                                 *
************************************************************************
START-OF-SELECTION.
*..1 select data
  PERFORM select_data.
  IF gt_if001[] IS INITIAL.
    MESSAGE s325.
  ENDIF.
*..2 make ALV list data
  PERFORM set_main_data.
*..3 call ALV function
  PERFORM alv_grid_list.

************************************************************************
*                         SUB ROUTINE                                  *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECT_OPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_select_option .
  CLEAR: ra_ztype, ra_ztype[].
*
  IF pa_rbtn1 = 'X'.
    ra_ztype-sign = 'I'. ra_ztype-option = 'EQ'.
    ra_ztype-low = 'S'. APPEND ra_ztype.
  ELSEIF pa_rbtn2 = 'X'.
    ra_ztype-sign = 'I'. ra_ztype-option = 'EQ'.
    ra_ztype-low = 'E'. APPEND ra_ztype.
  ENDIF.
ENDFORM.                    " CHECK_SELECT_OPTION
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_data .
  CLEAR: gt_if001, gt_if001[], gt_ztype, gt_ztype[].
*

**---// Error type setting using log message table - temporary usage
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_if001
*    FROM ztmm_if001 WHERE matnr IN sa_matnr
*                      AND erdat IN sa_erdat
*                      AND ernam IN sa_ernam
*                      AND zseq  IN sa_zzseq.
*
*  LOOP AT gt_if001.
*    CLEAR : ztmm_if015.
*    SELECT SINGLE type INTO ztmm_if015-type
*                       FROM ztmm_if015
*                      WHERE zseq EQ gt_if001-zseq.
*    IF sy-subrc EQ 0.
*      UPDATE ztmm_if001 SET type = ztmm_if015-type
*                      WHERE zseq EQ gt_if001-zseq.
*      IF sy-subrc EQ 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
**--- end

  CASE 'X'.
    WHEN pa_rbtn1.     " success
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_if001
        FROM ztmm_if001 WHERE matnr IN sa_matnr
                          AND erdat IN sa_erdat
                          AND ernam IN sa_ernam
                          AND zseq  IN sa_zzseq
                          AND type IN ('S', 'R').
    WHEN pa_rbtn2.     " error
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_if001
        FROM ztmm_if001 WHERE matnr IN sa_matnr
                          AND erdat IN sa_erdat
                          AND ernam IN sa_ernam
                          AND zseq  IN sa_zzseq
                          AND type EQ 'E'.
      DELETE gt_if001 WHERE zr2pro EQ 'S'.
    WHEN pa_rbtn4.     " re-processing
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_if001
        FROM ztmm_if001 WHERE matnr IN sa_matnr
                          AND erdat IN sa_erdat
                          AND ernam IN sa_ernam
                          AND zseq  IN sa_zzseq
                          AND ( type EQ 'R' OR zr2pro EQ 'S' ).
    WHEN OTHERS.       " all
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_if001
        FROM ztmm_if001 WHERE matnr IN sa_matnr
                          AND erdat IN sa_erdat
                          AND ernam IN sa_ernam
                          AND zseq  IN sa_zzseq.
  ENDCASE.

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_if001
*    FROM ztmm_if001 WHERE matnr IN sa_matnr
*                      AND erdat IN sa_erdat
*                      AND ernam IN sa_ernam
*                      AND zseq  IN sa_zzseq.

  IF sy-subrc = 0.
    SORT gt_if001 BY zseq.
    SELECT zseq type INTO CORRESPONDING FIELDS OF TABLE gt_ztype
      FROM ztmm_if015 FOR ALL ENTRIES IN gt_if001
     WHERE zseq = gt_if001-zseq.
    SORT gt_ztype BY zseq type.
    DELETE ADJACENT DUPLICATES FROM gt_ztype.
  ENDIF.
ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  set_main_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_main_data .
  CLEAR: gt_maind, gt_maind[].
*
  SELECT SINGLE waers INTO t001-waers
    FROM t001 WHERE bukrs = 'H201'.
*
  LOOP AT gt_if001.
    MOVE-CORRESPONDING gt_if001 TO gt_maind.
*-- 2006.02.27 Modify
    CLEAR gt_ztype.
    READ TABLE gt_ztype WITH KEY zseq = gt_if001-zseq.
    gt_maind-type = gt_ztype-type.
*-- Modify End
    gt_maind-waers = t001-waers.
*...
    CASE gt_maind-type.
      WHEN 'S'.
        MOVE : '@08@' TO gt_maind-icon.
      WHEN 'E'.
        IF gt_maind-zr2pro EQ 'S'.
          MOVE : '@09@' TO gt_maind-icon.
        ELSE.
          MOVE : '@0A@' TO gt_maind-icon.
        ENDIF.
      WHEN 'R'.
        MOVE : '@09@' TO gt_maind-icon.
    ENDCASE.
*...
    APPEND gt_maind. CLEAR gt_maind.
  ENDLOOP.
*
*  IF pa_rbtn1 = 'X'.
*    DELETE gt_maind WHERE type = 'E'.
*    DELETE gt_maind WHERE type = space.
*  ELSEIF pa_rbtn2 = 'X'.
*    DELETE gt_maind WHERE type = space.
*    DELETE gt_maind WHERE type = 'S'.
*  ENDIF.
ENDFORM.                    " set_main_data
*&---------------------------------------------------------------------*
*&      Form  alv_grid_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_list .
  CLEAR: g_layout_s, g_vari.
  CLEAR: g_events_t[], g_fieldcat_t[].
  CLEAR: gt_list_top_of_page[].
  CLEAR  g_tabname.
*
  g_repid = sy-repid.
  g_tabname = 'GT_MAIND'.
*.. Field Catalog
  PERFORM fieldcat_build.
*.. Event ??
  PERFORM eventtab_build.
*.. Layout ??
  PERFORM layout_build USING g_layout_s.
*.. Sort Catalog
  PERFORM sortcat_build.
*.. ALV Call
  PERFORM call_display_function.
ENDFORM.                    " alv_grid_list
*&---------------------------------------------------------------------*
*&      Form  fieldcat_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fieldcat_build .
  PERFORM reuse_alv USING :
 'ICON'   'Status'          '04' '' ' ' ' ' ' ' ''      '' 'GT_MAIND',
 'ZSEQ'   'Seq. No.'        '10' '' 'X' 'C' 'A' 'ALPHA' '' 'GT_MAIND',
 'ZFLAG'  'Division'        '1'  '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
 'TYPE'   'Type'            '1'  '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
 'ERDAT'  'Created Date'    '10' '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
 'ERZET'  'Created Time'    '8'  '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
 'ERNAM'  'Creation Person' '10' '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
 'ZR2PRO' 'Re-Processing Ind' '' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'ZREDOC' 'Re-Processing Doc' '' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'MATNR'  'Material'        '18' '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
 'MTART'  'Material Type'   '4'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'WERKS'  'Plant'           '4'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'LGORT'  'Strg. Loc.'      '4'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'SUB_IND' 'Subcontrain'    '10' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'MAKTX'  'Description'     '20' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'MAKTX2' 'Description2'    '20' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'MEINS'  'UoM'             '3'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'MATKL'  'Mat. Group'      '9'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'MSTAE'  'X-Plant Mat. st' '2'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'LABOR'  'LP/KD/MIP'       '3'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'BRGEW'  'Gross weight'    '13' '' ' ' 'R' 'Q' 'GEWEI' '' 'GT_MAIND',
 'GEWEI'  'Weight Unit'     '3'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'NTGEW'  'Net weight'      '13' '' ' ' 'R' 'Q' 'GEWEI' '' 'GT_MAIND',
 'WRKST'  'Mat. Add.Info'   '30' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'CLASS'  'Calss No.'       '18' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'EKGRP'  'Purchasing Grp'  '3'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'KAUTB'  'Auto PO allowed' '1'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'XCHAR'  'Batch manag.'    '1'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'STAWN'  'Import code'     '17' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'COUN_ORG' 'Country'       '2'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'DISGR'  'MRP Group'       '4'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'DISMM'  'MRP Type'        '2'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'MINBE'  'Reorder point'   '13' '' ' ' 'R' 'Q' 'MEINS' '' 'GT_MAIND',
 'DISPO'  'MRP Controller'  '3'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'DISLS'  'Lot size'        '2'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'BSTMI'  'Min. lot size'   '13' '' ' ' 'R' 'Q' 'MEINS' '' 'GT_MAIND',
 'BSTMA'  'Max. lot size'   '13' '' ' ' 'R' 'Q' 'MEINS' '' 'GT_MAIND',
 'MABST'  'Max.stock level' '13' '' ' ' 'R' 'Q' 'MEINS' '' 'GT_MAIND',
 'BSTRF'  'Rounding value'  '13' '' ' ' 'R' 'Q' 'MEINS' '' 'GT_MAIND',
 'LGPRO'  'Issue strg.loc.' '4'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'RGEKZ'  'Backflush'       '1'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'LGORT_EP' 'Strg.Loc.'     '4'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'PLIFZ'  'Planned deliv.'  '3'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'EISBE'  'Safety stock'    '13' '' ' ' 'R' 'Q' 'MEINS' '' 'GT_MAIND',
 'LGPBE'  'Storage bin'     '10' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'BKLAS'  'Valuation Class' '4'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'VPRSV'  'Price Control'   '1'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
 'VERPR'  'Moving Avrg. pr' '11' '' ' ' 'R' 'C' 'WAERS' '' 'GT_MAIND',
 'HKMAT'  'Mat. Origin'     '1'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND'.
ENDFORM.                    " fieldcat_build
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM reuse_alv  USING fieldname
                      fieldtext
                      outputlen
                      key
                      color
                      just
                      ref_fld
                      ref_fld2
                      no_out
                      p_tabname.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
*
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = fieldname.
  ls_fieldcat-reptext_ddic   = fieldtext.
  ls_fieldcat-outputlen      = outputlen.
  ls_fieldcat-fix_column     = key.
  ls_fieldcat-key            = color.
  ls_fieldcat-just           = just.
  ls_fieldcat-no_out         = no_out.
*
  CASE ref_fld.
    WHEN 'C'. ls_fieldcat-cfieldname = ref_fld2.
    WHEN 'Q'. ls_fieldcat-qfieldname = ref_fld2.
    WHEN 'A'. ls_fieldcat-edit_mask  = '==ALPHA'.
    WHEN 'E'. ls_fieldcat-edit_mask  = '==EXCRT'.
  ENDCASE.
*
  IF fieldname = 'MATNR'.
    ls_fieldcat-hotspot = 'X'.
  ENDIF.
*
  APPEND ls_fieldcat TO g_fieldcat_t.
  CLEAR ls_fieldcat.
  pos = pos + 1.
ENDFORM.                    " REUSE_ALV
*&---------------------------------------------------------------------*
*&      Form  eventtab_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM eventtab_build .
  DATA: ls_event  TYPE slis_alv_event.
*
  ls_event-name = 'USER_COMMAND'.
  ls_event-form = 'USER_COMMAND'.
  APPEND ls_event TO g_events_t.
*
  ls_event-name = 'PF_STATUS_SET'.
  ls_event-form = 'PF_STATUS_SET'.
  APPEND ls_event TO g_events_t.
ENDFORM.                    " eventtab_build
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM layout_build  USING l_layout_s TYPE slis_layout_alv.
  l_layout_s-colwidth_optimize = 'X'.
  l_layout_s-zebra             = 'X'.
  l_layout_s-box_fieldname     = 'CHKBX'.
ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  CALL_DISPLAY_FUNCTION
*&---------------------------------------------------------------------*
FORM call_display_function .
  SET SCREEN 0.
  SET PARAMETER ID 'ALVBUFFER' FIELD sy-uzeit.
  gs_gridset-edt_cll_cb = 'X'.
*
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = g_repid
            i_callback_user_command = g_user_command
            it_fieldcat             = g_fieldcat_t[]
            it_excluding            = gt_exclude[]
            it_events               = g_events_t[]
            it_sort                 = g_sort_t[]
            is_layout               = g_layout_s
            is_variant              = g_vari
            i_save                  = 'X'
            i_grid_settings         = gs_gridset
       TABLES
            t_outtab                = gt_maind
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
*
  CASE sy-subrc.
    WHEN '1'.
      MESSAGE e007 WITH 'PROGRAM_ERROR'.
    WHEN '2'.
      MESSAGE e007 WITH 'OTHERS'.
  ENDCASE.
ENDFORM.                    " CALL_DISPLAY_FUNCTION
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
FORM top_of_page.

ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM USER-COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command  USING  rf_ucomm  LIKE  sy-ucomm
                          rs        TYPE  slis_selfield .
  rs-refresh = 'X'.
  CASE rf_ucomm.
    WHEN 'REFR'.
      PERFORM select_data.
      PERFORM set_main_data.
    WHEN '&IC1'.
      READ TABLE gt_maind INDEX rs-tabindex.
      IF sy-subrc = 0 AND gt_maind-matnr NE space.
        SET PARAMETER ID 'MAT' FIELD gt_maind-matnr.
        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      ENDIF.
*    WHEN 'DETA'.
*      READ TABLE gt_maind WITH KEY chkbx = 'X'.
*      IF sy-subrc = 0.
*        PERFORM get_detail_info.
*        PERFORM get_classfication_info.
*        PERFORM get_log_message.
*        CALL SCREEN 1100.
*      ENDIF.
    WHEN 'REPV'.
      READ TABLE gt_maind WITH KEY chkbx = 'X'.
      IF sy-subrc EQ 0.
        PERFORM get_detail_info.
        PERFORM get_classfication_info.
        PERFORM get_log_message.
        CALL SCREEN 1100.
      ENDIF.
  ENDCASE.
ENDFORM.                    "USER-COMMAND
*&---------------------------------------------------------------------*
*&      Form  pf_status_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pf_status_set  USING    p_rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULL'.
ENDFORM.                    " pf_status_set
*&---------------------------------------------------------------------*
*&      Form  get_detail_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_detail_info .
  CLEAR: st_if001, st_ntext.
  MOVE-CORRESPONDING gt_maind TO st_if001.
*
  CLEAR t134t.
  SELECT SINGLE mtbez INTO t134t-mtbez
    FROM t134t WHERE spras = sy-langu
                 AND mtart = st_if001-mtart.
  st_ntext-mtbez = t134t-mtbez.
*
  CLEAR t001w.
  SELECT SINGLE name1 INTO t001w-name1
    FROM t001w WHERE werks = st_if001-werks.
  st_ntext-name1 = t001w-name1.
*
  CLEAR t001l.
  SELECT SINGLE lgobe INTO t001l-lgobe
    FROM t001l WHERE werks = st_if001-werks
                 AND lgort = st_if001-lgort.
  st_ntext-lgobe = t001l-lgobe.
*
  CLEAR t023t.
  SELECT SINGLE wgbez60 INTO t023t-wgbez60
    FROM t023t WHERE spras = sy-langu
                 AND matkl = st_if001-matkl.
  st_ntext-wgbez = t023t-wgbez60.
ENDFORM.                    " get_detail_info
*&---------------------------------------------------------------------*
*&      Form  get_classfication_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_classfication_info .
  CLEAR: gt_if020, gt_if020.
*
  SELECT name_char char_value
    INTO CORRESPONDING FIELDS OF TABLE gt_if020
    FROM ztmm_if020 WHERE zseq = gt_maind-zseq.
*
  DESCRIBE TABLE gt_if020 LINES tc1100-lines.
ENDFORM.                    " get_classfication_info
*&---------------------------------------------------------------------*
*&      Form  get_log_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_log_message .
  CLEAR: gt_if015, gt_if015[].
*
  SELECT type message INTO CORRESPONDING FIELDS OF TABLE gt_if015
    FROM ztmm_if015 WHERE zseq = gt_maind-zseq
                      AND type IN ra_ztype.
ENDFORM.                    " get_log_message
*&---------------------------------------------------------------------*
*&      Module  STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1100 OUTPUT.
  IF gt_maind-icon NE '@0A@'.
    SET PF-STATUS 'PS1100' EXCLUDING 'REP'.
  ELSE.
    SET PF-STATUS 'PS1100'.
  ENDIF.

  SET TITLEBAR '110'.
ENDMODULE.                 " STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  back_exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " back_exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  create_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_object OUTPUT.
  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING container_name = 'CC_MSG'.
    CREATE OBJECT alv_grid
      EXPORTING i_parent = g_custom_container.
  ENDIF.
ENDMODULE.                 " create_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_grid OUTPUT.
  PERFORM detail_layout.
  PERFORM detail_field_cat.
*
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout       = g_st_detail_layout
    CHANGING
      it_fieldcatalog = g_it_detail_fdcat
      it_outtab       = gt_if015[].

ENDMODULE.                 " ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DETAIL_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM detail_layout .
  CLEAR g_st_detail_layout.
  g_st_detail_layout-cwidth_opt = 'X'.
  g_st_detail_layout-zebra      = 'X'.
ENDFORM.                    " DETAIL_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DETAIL_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM detail_field_cat .
  REFRESH g_it_detail_fdcat.
*
  g_st_detail_fdcat-fieldname = 'TYPE'.
  g_st_detail_fdcat-coltext   = 'Type'.
  APPEND g_st_detail_fdcat TO g_it_detail_fdcat.
*
  g_st_detail_fdcat-fieldname = 'MESSAGE'.
  g_st_detail_fdcat-coltext   = 'Message'.
  APPEND g_st_detail_fdcat TO g_it_detail_fdcat.
ENDFORM.                    " DETAIL_FIELD_CAT

*&---------------------------------------------------------------------*
*&      Module  screen_control  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_control OUTPUT.
  CHECK st_if001-icon EQ '@0A@'.  " error
  CHECK st_if001-zflag NE 'D'.    " delete

  LOOP AT SCREEN.
    screen-input = 1.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " screen_control  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1100 INPUT.
  DATA : answer,
         save_okcode LIKE ok_code.

  MOVE : ok_code TO save_okcode.

  CLEAR : ok_code.

  CASE save_okcode.
    WHEN 'REP'.
      CLEAR : save_okcode.
      PERFORM popup_to_confirm USING    text-p01
                                        text-p02
                               CHANGING answer.
      CHECK answer = 'J'.

      PERFORM re_process_material.
  ENDCASE.
ENDMODULE.                 " user_command_1100  INPUT

*&---------------------------------------------------------------------*
*&      Form  popup_to_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_P01  text
*      -->P_TEXT_P02  text
*      <--P_ANSWER  text
*----------------------------------------------------------------------*
FORM popup_to_confirm USING    p_text_p01
                               p_text_p02
                      CHANGING p_answer.
*----------------------------------------------------------------------*
*  MESSAGE POPUP
*----------------------------------------------------------------------*
  DATA: BEGIN OF pop,
        titel     LIKE spop-titel,
        diagnose1 LIKE spop-diagnose1,
        diagnose2 LIKE spop-diagnose2,
        diagnose3 LIKE spop-diagnose3,
        textline1 LIKE spop-textline1,
        textline2 LIKE spop-textline2,
        textline3 LIKE spop-textline3,
        option1   LIKE spop-varoption1,
        option2   LIKE spop-varoption2,
        default,
        answer,
        END OF pop.

  DATA: cancel_display.

  MOVE: p_text_p01 TO pop-textline1,
        p_text_p02 TO pop-titel.

  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
    EXPORTING
*     DEFAULTOPTION        = 'Y'
      diagnosetext1        = pop-diagnose1
*     DIAGNOSETEXT2        = ' '
*     DIAGNOSETEXT3        = ' '
      textline1            = pop-textline1
      textline2            = pop-textline2
      titel                = pop-titel
*     START_COLUMN         = 25
*     START_ROW            = 6
      cancel_display       = cancel_display
    IMPORTING
      answer               = pop-answer
    EXCEPTIONS
      othsers              = 1.

  p_answer = pop-answer.
ENDFORM.                    " popup_to_confirm
*&---------------------------------------------------------------------*
*&      Form  re_process_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM re_process_material.
*---
  DATA : wa_material LIKE zsmm_if021,
         wa_class LIKE rmclf-class,
         wa_check(1).

  DATA : it_zsmm_if015 LIKE zsmm_if015 OCCURS 0 WITH HEADER LINE, "class
         it_return LIKE bapireturn OCCURS 0 WITH HEADER LINE.

  CLEAR : wa_material, wa_class, wa_check,
          it_zsmm_if015, it_zsmm_if015[], it_return, it_return[].

*---
  MOVE-CORRESPONDING st_if001 TO wa_material.

  MOVE : st_if001-class TO wa_class,
         st_if001-zflag TO wa_check.

  LOOP AT gt_if020.
    MOVE-CORRESPONDING gt_if020 TO it_zsmm_if015.
    APPEND it_zsmm_if015.
    CLEAR : gt_if020, it_zsmm_if015.
  ENDLOOP.

*--- call function (re-processing)
  CALL FUNCTION 'ZMMF_IF_BAPI_MATERIAL_CREATE_R'
       EXPORTING
            i_material     = wa_material
            i_class        = wa_class
            i_check        = wa_check
       TABLES
            classification = it_zsmm_if015
            e_return       = it_return.

**--- update log table
*  DATA : it_ztmm_if001 LIKE ztmm_if001 OCCURS 0 WITH HEADER LINE, "main
*         it_ztmm_if020 LIKE ztmm_if020 OCCURS 0 WITH HEADER LINE.
  PERFORM select_data.
  PERFORM set_main_data.
  LEAVE TO SCREEN 0.
ENDFORM.                    " re_process_material

*&---------------------------------------------------------------------*
*&      Module  modify_1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_1100 INPUT.
*---
  MODIFY gt_if020 INDEX tc1100-current_line.
ENDMODULE.                 " modify_1100  INPUT

*&---------------------------------------------------------------------*
*&      Form  sortcat_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sortcat_build.
*---
  DATA : l_sort LIKE LINE OF g_sort_t.

  CLEAR : l_sort, g_sort_t[].

  l_sort-spos = 1.
  l_sort-fieldname = 'MATNR'.
  l_sort-up = 'X'.

  APPEND l_sort TO g_sort_t.
ENDFORM.                    " sortcat_build
