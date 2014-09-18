*&---------------------------------------------------------------------*
*& Report  ZGHRM00100
*&
*&---------------------------------------------------------------------*
REPORT  zghrm00100 NO STANDARD PAGE HEADING.

TABLES: sscrfields,dd02l.

TYPE-POOLS:icon.

* Excel header field line
DATA: BEGIN OF gt_layout OCCURS 0,
        name01(50), name02(50), name03(50), name04(50),
        name05(50), name06(50), name07(50), name08(50),
        name09(50), name10(50), name11(50), name12(50),
        name13(50), name14(50), name15(50), name16(50),
        name17(50), name18(50), name19(50), name20(50),
        name21(50), name22(50), name23(50), name24(50),
        name25(50), name26(50), name27(50), name28(50),
        name29(50), name30(50), name31(50), name32(50),
        name33(50), name34(50), name35(50), name36(50),
        name37(50), name38(50), name39(50), name40(50),
        name41(50), name42(50), name43(50), name44(50),
        name45(50), name46(50), name47(50), name48(50),
        name49(50), name50(50), name51(50), name52(50),
        name53(50), name54(50), name55(50), name56(50),
        name57(50), name58(50), name59(50), name60(50),
      END OF gt_layout.


SELECTION-SCREEN BEGIN OF BLOCK b00 WITH FRAME TITLE text-b00.
*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
PARAMETERS : pa_tab   TYPE dd02l-tabname MEMORY ID dtb OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS : pa_fname LIKE rlgrap-filename MEMORY ID fil
                                           DEFAULT '*.txt'.
PARAMETERS: p_num TYPE i DEFAULT '10000'.
SELECTION-SCREEN COMMENT  /1(55) text-002 for field p_num.
SELECTION-SCREEN END   OF BLOCK blk1.


SELECTION-SCREEN END OF BLOCK b00.


SELECTION-SCREEN: FUNCTION KEY 1,
                  FUNCTION KEY 2,
                  FUNCTION KEY 3.

INITIALIZATION.
  sscrfields-functxt_01 = 'Display'.
  sscrfields-functxt_02 = 'Export'.
  sscrfields-functxt_03 = 'Delete All'.


AT SELECTION-SCREEN OUTPUT.


AT SELECTION-SCREEN.
  IF pa_tab IS INITIAL.
*    MESSAGE 'Error with Selected Table Name, Only Possible CBO Table' TYPE 'W'.
    MESSAGE 'Error with Selected Table Name' TYPE 'E'.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM set_disp.
    WHEN 'FC02'.
      PERFORM set_export.
    WHEN 'FC03'.
      PERFORM set_delete.
    WHEN 'ONLI'.
      IF pa_fname IS INITIAL OR pa_fname EQ '*.txt'.
        MESSAGE 'Select uploading file...' TYPE 'E'.
      ENDIF.
      PERFORM import_from_local.
  ENDCASE.
  CLEAR sy-ucomm.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_fname.

  PERFORM selection_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_tab.

  CALL FUNCTION 'RS_DD_F4_OBJECT'
    EXPORTING
      objname            = pa_tab
      objtype            = 'T'
      suppress_selection = 'X'
    IMPORTING
      selobjname         = pa_tab.


*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       Local File# ### ####.
*----------------------------------------------------------------------*
FORM selection_screen .
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = pa_fname
      def_path         = 'C:\'
      mask             = ',*.txt;.'
      mode             = 'O'
      title            = 'Select uploading file...'
    IMPORTING
      filename         = pa_fname
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  CASE sy-subrc.

    WHEN 1.
      MESSAGE 'File selector not available on this windows system'
         TYPE 'S'.
    WHEN 2.
      MESSAGE 'Front end function cannot be executed in batch' TYPE 'I'.
    WHEN 3.
      MESSAGE 'Selection was cancelled' TYPE 'S'.
    WHEN 4.
      MESSAGE 'Communication error' TYPE 'S'.
    WHEN 5.
      MESSAGE 'OTHERS ERROR' TYPE 'S'.
  ENDCASE.

ENDFORM.                    " SELECTION_SCREEN


*&---------------------------------------------------------------------*
*&      Form  import-from-local
*&---------------------------------------------------------------------*
*       Local File# ### ## Database# ####.
*----------------------------------------------------------------------*
FORM import_from_local .
  FIELD-SYMBOLS: <all_table>       TYPE table.
  DATA: gt_fieldcat                TYPE lvc_t_fcat.
  DATA: ldtab_cell                 TYPE REF TO data.
  REFRESH gt_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_buffer_active        = 'X'
      i_structure_name       = pa_tab
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = gt_fieldcat
    IMPORTING
      ep_table                  = ldtab_cell
    EXCEPTIONS
      generate_subpool_dir_full = 9.
  IF sy-subrc = 9.
    MESSAGE i122(wusl).
    LEAVE TO TRANSACTION sy-tcode.
  ENDIF.
  ASSIGN ldtab_cell->* TO <all_table>.
  REFRESH <all_table>.

  DATA lv_fname TYPE string.
  MOVE pa_fname TO lv_fname.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                      = lv_fname
     FILETYPE                      = 'ASC'
     has_field_separator           = 'X'
*     HEADER_LENGTH                 = 0
*     READ_BY_LINE                  = 'X'
*     DAT_MODE                      = ' '
*     CODEPAGE                      = ' '
*     IGNORE_CERR                   = ABAP_TRUE
*     REPLACEMENT                   = '#'
*     CHECK_BOM                     = ' '
*   IMPORTING
*     FILELENGTH                    =
*     HEADER                        =
    TABLES
      data_tab                      = <all_table>
   EXCEPTIONS
     file_open_error               = 1
     file_read_error               = 2
     no_batch                      = 3
     gui_refuse_filetransfer       = 4
     invalid_type                  = 5
     no_authority                  = 6
     unknown_error                 = 7
     bad_data_format               = 8
     header_not_allowed            = 9
     separator_not_allowed         = 10
     header_too_long               = 11
     unknown_dp_error              = 12
     access_denied                 = 13
     dp_out_of_memory              = 14
     disk_full                     = 15
     dp_timeout                    = 16
     OTHERS                        = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  FIELD-SYMBOLS: <all_table_temp> LIKE <all_table>.
  DATA: lv_from_idx TYPE syindex,
        lv_to_idx TYPE syindex,
        lv_tab_cnt TYPE i.

  DESCRIBE TABLE <all_table> LINES lv_tab_cnt.

  IF p_num IS INITIAL.
    MODIFY (pa_tab) FROM TABLE <all_table>.
    COMMIT WORK.
  ELSE.
 ASSIGN <all_table> TO <all_table_temp>.
    DO.
      lv_from_idx = lv_to_idx + 1.
      lv_to_idx   = lv_from_idx + p_num - 1.

      IF lv_tab_cnt < lv_from_idx.
        EXIT.
      ENDIF.

      APPEND LINES OF <all_table> FROM lv_from_idx TO lv_to_idx TO <all_table_temp>.
      MODIFY (pa_tab) FROM TABLE <all_table_temp>.
      COMMIT WORK.
      CLEAR <all_table_temp>.
    ENDDO.
  ENDIF.

ENDFORM.                    " import-from-local

*&---------------------------------------------------------------------*
*&      Form  SET_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_disp .
  DATA: l_tabnm(30),
        l_memid(16) VALUE 'TABELLENANZEIGER',
        l_auth(4),
        action TYPE sy-ucomm VALUE 'ANZE'.

  CONCATENATE '/1BCDWB/DB' pa_tab INTO l_tabnm.
  dd02l-mainflag = 'X'.
  EXPORT action dd02l-mainflag l_auth TO MEMORY ID l_memid.

  SUBMIT (l_tabnm) VIA SELECTION-SCREEN AND RETURN.

ENDFORM.                    " SET_DISP
*&---------------------------------------------------------------------*
*&      Form  SET_EXPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_export .


  TYPE-POOLS: ole2.

  INCLUDE: ole2incl.

*-Excel ##
  DATA: g_application_ole TYPE ole2_object,
        g_workbook        TYPE ole2_object,
        g_sheet           TYPE ole2_object,
        g_cells           TYPE ole2_object,
        g_counts          TYPE syindex.

  FIELD-SYMBOLS: <all_table>      TYPE table,
                 <fs_str>         TYPE ANY,
                 <fs_field>       TYPE ANY,
                 <fs_field2>      TYPE ANY.

  DATA: lt_dd03l TYPE TABLE OF dd03l WITH HEADER LINE.
  DATA: l_index(2)  TYPE n,
        lv_cnt TYPE i,
        l_fidx TYPE i,
        l_field(25).
  DATA: gt_fieldcat                TYPE lvc_t_fcat.
  DATA: ldtab_cell                 TYPE REF TO data.
  DATA: l_fullpath TYPE string.

  REFRESH gt_layout.
  CLEAR: gt_layout, g_counts.


  REFRESH gt_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_buffer_active        = 'X'
      i_structure_name       = pa_tab
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = gt_fieldcat
    IMPORTING
      ep_table                  = ldtab_cell
    EXCEPTIONS
      generate_subpool_dir_full = 9.
  IF sy-subrc = 9.
    MESSAGE i122(wusl).
    LEAVE TO TRANSACTION sy-tcode.
  ENDIF.
  ASSIGN ldtab_cell->* TO <all_table>.
  REFRESH <all_table>.


  SELECT * FROM (pa_tab)
    INTO TABLE <all_table>.


  IF <all_table> IS INITIAL .
    MESSAGE 'Table has no Data.' TYPE 'E'.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = l_fullpath
      def_path         = 'C:\'
      mask             = ',All Files(*.*),*.*,Text Files(*.txt),*.txt,Excel Files(*.xls),*.xls.'
      mode             = '0'
      title            = 'Download'
    IMPORTING
      filename         = l_fullpath
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  CHECK l_fullpath IS NOT INITIAL.

  SELECT * FROM dd03l
     INTO TABLE lt_dd03l
     WHERE tabname = pa_tab
     ORDER BY position.

  LOOP AT lt_dd03l.
    PERFORM get_fieldname USING  lt_dd03l-fieldname CHANGING l_index.
    MOVE l_index TO g_counts.
  ENDLOOP.
  APPEND gt_layout.

  lv_cnt = LINES( lt_dd03l ).

  CLEAR: l_index.

  LOOP AT <all_table> ASSIGNING  <fs_str> .

    CLEAR: l_index, l_fidx.
    DO lv_cnt TIMES.
      ADD: 1 TO l_index, 1 TO l_fidx.

      ASSIGN COMPONENT l_fidx OF STRUCTURE <fs_str> TO <fs_field>.
      IF <fs_field> IS ASSIGNED.

        CONCATENATE 'gt_layout-name' l_index INTO l_field.

        ASSIGN (l_field) TO <fs_field2>.
        <fs_field2> = <fs_field>.
      ENDIF.
    ENDDO.

    APPEND gt_layout.
  ENDLOOP.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = l_fullpath
      filetype                = 'DAT'
    TABLES
      data_tab                = gt_layout[]
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

ENDFORM.                    " SET_EXPORT
*&---------------------------------------------------------------------*
*&      Form  SET_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_delete .

  DATA: l_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirm'
      text_question         = 'Do you want delete all data?'
      text_button_1         = 'Yes'
      text_button_2         = 'No'(002)
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = l_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*confirm

  IF l_answer EQ '1'.
    DELETE FROM (pa_tab).
    COMMIT WORK.

    IF sy-subrc EQ 0.
      MESSAGE 'Data Deleted.' TYPE 'S'.
    ENDIF.
  ENDIF.


ENDFORM.                    " SET_DELETE
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DD03L_FIELDNAME  text
*      <--P_L_INDEX  text
*----------------------------------------------------------------------*
FORM get_fieldname  USING    pu_fname
                    CHANGING pc_index.

  DATA: l_field(25).

  FIELD-SYMBOLS: <fs_field> TYPE ANY.

  ADD 1 TO pc_index.

  CONCATENATE 'gt_layout-name' pc_index INTO l_field.
  ASSIGN (l_field) TO <fs_field>.
  CHECK <fs_field> IS ASSIGNED.
  MOVE pu_fname TO <fs_field>.
ENDFORM.                    " GET_FIELDNAME
