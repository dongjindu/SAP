REPORT zite_bpml_update_field
                NO STANDARD PAGE HEADING
                LINE-SIZE  100
                LINE-COUNT 65
                MESSAGE-ID zmbm.

DATA: BEGIN OF it_excl OCCURS 0,
      tcode LIKE ztitbpml-tcode,
      field(20),
      value(1),
      END OF it_excl.

DATA: BEGIN OF it_data OCCURS 0,
      tcode LIKE ztitbpml-tcode,
      field(20),
      value(1),
      task LIKE ztitbpml-task,
      result(1),
      message(40),
      END OF it_data.

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


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-100.
*PARAMETERS:
*  p_name(20) DEFAULT 'APM' OBLIGATORY,
*  p_value DEFAULT 'X'.
*SELECTION-SCREEN SKIP.
PARAMETERS:
  p_file  LIKE rlgrap-filename DEFAULT 'C:\.TXT' OBLIGATORY,
  p_filety LIKE rlgrap-filetype DEFAULT 'DAT',
  p_alv AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK b1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.

START-OF-SELECTION.
  PERFORM upload_data.
  IF it_excl[] IS INITIAL.
    MESSAGE e001 WITH 'No Data'.
  ELSE.
    IF p_alv IS INITIAL.
      PERFORM read_process.
    ELSE.
      PERFORM make_alv_data.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename
                                          mode     TYPE c.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.

* Build Filter for Fileselektor

*  IF GLOBAL_FILEMASK_MASK IS INITIAL.
  tmp_mask = ',*.*,*.*.'.
*  ELSE.
*    TMP_MASK = ','.
*    WRITE GLOBAL_FILEMASK_TEXT TO TMP_MASK+1.
*    WRITE ',' TO TMP_MASK+21.
*    WRITE GLOBAL_FILEMASK_MASK TO TMP_MASK+22.
*    WRITE '.' TO TMP_MASK+42.
*    CONDENSE TMP_MASK NO-GAPS.
*  ENDIF.

*  IF NOT GLOBAL_FILEMASK_ALL IS INITIAL.
*    TMP_MASK = GLOBAL_FILEMASK_ALL.
*  ENDIF.
*
  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = p_file
      def_path         = def_path
*     MASK             = ',*.*,*.*.'
      mask             = tmp_mask
      mode             = mode
*     TITLE            = ' '
    IMPORTING
      filename         = tmp_filename
*     RC               =
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM upload_data.

  CALL FUNCTION 'UPLOAD'
   EXPORTING
*   CODEPAGE                      = ' '
     filename                      = p_file
     filetype                      = p_filety
*   ITEM                          = ' '
*   FILEMASK_MASK                 = ' '
*   FILEMASK_TEXT                 = ' '
*   FILETYPE_NO_CHANGE            = ' '
*   FILEMASK_ALL                  = ' '
*    FILETYPE_NO_SHOW              = 'X'
*   LINE_EXIT                     = ' '
*   USER_FORM                     = ' '
*   USER_PROG                     = ' '
   silent                        =  'X'
* IMPORTING
*   FILESIZE                      =
*   CANCEL                        =
*   ACT_FILENAME                  =
*   ACT_FILETYPE                  =
    TABLES
      data_tab                      = it_excl.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CASE sy-subrc.
    WHEN 0.
      DATA l_text(132).
      CONCATENATE p_file text-001
                  INTO l_text.
*      WRITE: / L_TEXT.
*      SKIP.
    WHEN 2.
      MESSAGE e000 WITH text-002.
    WHEN 3.
      MESSAGE e000 WITH text-003.
    WHEN OTHERS.
      MESSAGE e000 WITH text-004.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_process.
  DATA: lt_master LIKE TABLE OF ztitbpml WITH HEADER LINE.
  DATA: l_exp  TYPE string,
        l_sign(1) VALUE '`',
        l_value(3),
        l_cn TYPE i.
**'`'

  SELECT * INTO TABLE lt_master
   FROM ztitbpml
   FOR ALL ENTRIES IN it_excl
   WHERE tcode = it_excl-tcode.

  SORT lt_master BY tcode.

  LOOP AT it_excl.
    CLEAR: it_data.
    MOVE-CORRESPONDING it_excl TO it_data.
    READ TABLE lt_master WITH KEY tcode = it_excl-tcode.
    IF sy-subrc = 0.
      it_data-task = lt_master-task.
** ?? CHECK FILED NAME AND VAULE.
***
      CONCATENATE l_sign it_excl-value l_sign INTO l_value.
      l_exp = it_excl-field && ' = ' && l_value.
      UPDATE ztitbpml SET (l_exp)
            WHERE tcode = it_data-tcode.
      IF sy-subrc <> 0.
        it_data-result = 'E'.
        it_data-message = 'Update error'.
      ELSE.
        it_data-result = 'S'.
        l_cn = l_cn + 1.
      ENDIF.
    ELSE.
      it_data-task = 'Not in BPML MASTER TABLE'.
    ENDIF.
    APPEND it_data.
  ENDLOOP.
  COMMIT WORK.
  WRITE:/ 'Total Successfully Updated:  ', l_cn.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv.

  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'IT_DATA'.

*  PERFORM list_header_write USING alv_t_listheader[].
*  PERFORM append_alv_event  CHANGING   gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
*     i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command  = g_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort
      i_save                   = g_save
*     is_variant               = g_variant
      it_events                = gt_events[]
    TABLES
      t_outtab                 = it_data[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.                    " display_alv
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
FORM layout_build  USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.
*  p_layout-key_hotspot = 'X'.
*  p_layout-box_fieldname  =    'CHK'.  "SELECTION FIELD
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

  gs_sort-spos      = '1'.
  gs_sort-tabname   = 'IT_DATA'.
  gs_sort-fieldname = 'TCODE'.
  gs_sort-up        = 'X'.
*  gs_sort-group     = 'OBJ'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.


ENDFORM.                    " SORTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat  TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv
               USING    p_name      TYPE  slis_tabname.

  DATA: l_datum(08).
  g_repid  = sy-repid.
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
      WHEN 'TYPE_DESC'.
        pt_fieldcat-seltext_m    = 'Type'.
      WHEN 'RESULT'.
        pt_fieldcat-seltext_m    = 'Status'.

*      PT_FIELDCAT-NO_OUT       = 'X'.
*      WHEN 'WERKS'.
*        PT_FIELDCAT-SELTEXT_M    = 'Plant'.
*        PT_FIELDCAT-NO_OUT       = 'X'.
      WHEN OTHERS.

    ENDCASE.
    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.

    MODIFY pt_fieldcat.

  ENDLOOP.

ENDFORM.                    " FIELDCAT
*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  P_SELFIELD                                                    *
*---------------------------------------------------------------------*
FORM  user_command USING ucomm    LIKE sy-ucomm
                    p_selfield    TYPE slis_selfield.
* double click : UCOMM = &IC1
  CASE ucomm.
    WHEN 'EXIT'.
      EXIT.
  ENDCASE.

  p_selfield-refresh = 'X'.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM list_header_write  USING alv_t_listheader TYPE slis_t_listheader.

*  CLEAR   : ls_title, alv_t_listheader.
*  REFRESH : alv_t_listheader.
*
*  DATA : h_title(30), s_title(60),  a_title(60).
*
*  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
*  CONCATENATE '*Company Code : '  p_bukrs INTO ls_title-info.
*  APPEND ls_title TO alv_t_listheader.

*  ls_title-typ = 'S'.
*  CONCATENATE '*Month : ' s_spmon-low   INTO ls_title-info.
*  APPEND ls_title TO alv_t_listheader.

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
*&      Form  MAKE_ALV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_data.
  DATA: lt_master LIKE TABLE OF ztitbpml WITH HEADER LINE.

  SELECT * INTO TABLE lt_master
   FROM ztitbpml
   FOR ALL ENTRIES IN it_excl
   WHERE tcode = it_excl-tcode.

  SORT lt_master BY tcode.

  LOOP AT it_excl.
    CLEAR: it_data.
    MOVE-CORRESPONDING it_excl TO it_data.
    READ TABLE lt_master WITH KEY tcode = it_excl-tcode.
    IF sy-subrc = 0.
      it_data-task = lt_master-task.
    ELSE.
      it_data-task = 'Not in BPML MASTER TABLE'.
    ENDIF.
    APPEND it_data.
  ENDLOOP.
ENDFORM.                    " MAKE_ALV_DATA
