***********************************************************
* Program Name      : ZMMR_SERVICE_ENTRYSHEET
* Author            : Furong
* Creation Date     : 12/19/12
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Upload the retaining vacation payment
*                     Source copy from KMMG program
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zppa_planorder_delete MESSAGE-ID zmpp
     NO STANDARD PAGE HEADING LINE-SIZE 105 .

TYPE-POOLS: slis, vrm.

DATA: w_line_total TYPE i.

DATA: BEGIN OF it_data OCCURS 0,
* header
      ref_date LIKE bapiessrc-ref_date,
      begdate LIKE bapiessrc-begdate,
      enddate LIKE bapiessrc-enddate,
      short_text LIKE bapiessrc-short_text,
      location LIKE bapiessrc-location,
      po_number LIKE bapiessrc-po_number,
      po_item LIKE bapiessrc-po_item,
*      ref_doc_no LIKE bapiessrc-ref_doc_no,
      ref_doc_no LIKE bapiessrc-EXT_NUMBER,
* item
*     userf1_txt LIKE bapiesllc-userf1_txt,
     service LIKE bapiesllc-service,
     quantity LIKE bapiesllc-quantity,
*     userf1_num LIKE bapiesllc-userf1_num,
*     userf2_txt LIKE bapiesllc-userf2_txt,
** output message
     message LIKE bapireturn-message,
     po_nodis LIKE bapiessrc-po_number,
     po_item_nodis LIKE bapiessrc-po_item,
      END OF it_data.

DATA: BEGIN OF it_header OCCURS 0,
      ref_date LIKE bapiessrc-ref_date,
      begdate LIKE bapiessrc-begdate,
      enddate LIKE bapiessrc-enddate,
      short_text LIKE bapiessrc-short_text,
      location LIKE bapiessrc-location,
      po_number LIKE bapiessrc-po_number,
      po_item LIKE bapiessrc-po_item,
*      ref_doc_no LIKE bapiessrc-ref_doc_no,
      ref_doc_no LIKE bapiessrc-EXT_NUMBER,
      message LIKE bapireturn-message,
      END OF it_header.

DATA: BEGIN OF it_item OCCURS 0,
      po_number LIKE bapiessrc-po_number,
      po_item LIKE bapiessrc-po_item,
*     userf1_txt LIKE bapiesllc-userf1_txt,
     service LIKE bapiesllc-service,
     quantity LIKE bapiesllc-quantity,
*     userf1_num LIKE bapiesllc-userf1_num,
*     userf2_txt LIKE bapiesllc-userf2_txt,
      END OF it_item.

DATA: intern TYPE alsmex_tabline OCCURS 0 WITH HEADER LINE.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldname.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_cnt   TYPE i,
      w_repid LIKE sy-repid.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-100.
PARAMETERS: p_file LIKE rlgrap-filename OBLIGATORY.
" DEFAULT 'C:\.xlsx' OBLIGATORY,
* p_filety LIKE rlgrap-filetype DEFAULT 'DAT' NO-DISPLAY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file.

START-OF-SELECTION.
*  PERFORM upload_data.
  PERFORM upload_data_text.
  DESCRIBE TABLE it_header LINES w_line_total.
  IF w_line_total > 0.
    PERFORM display_data.
  ELSE.
    MESSAGE i000 WITH  'No Data'.
  ENDIF.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_data .
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 4
      i_end_col               = 15
      i_end_row               = 3000
    TABLES
      intern                  = intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc NE 0.
    MESSAGE i000 WITH  'EXEL file upload error!'.
    EXIT.
  ENDIF.

*  DELETE intern FROM 1 TO 2.

  LOOP AT intern.
    CASE intern-col.
      WHEN 1.
        it_header-ref_date = intern-value.
        CONCATENATE '20' it_header-ref_date+6(2)
                    it_header-ref_date+0(2)
                    it_header-ref_date+3(2)
               INTO it_header-ref_date.
      WHEN 2.
        it_header-begdate = intern-value.
        CONCATENATE '20' it_header-begdate+6(2)
                   it_header-begdate+0(2)
                   it_header-begdate+3(2)
              INTO it_header-begdate.

      WHEN 3.
        it_header-enddate = intern-value.
        CONCATENATE '20' it_header-enddate+6(2)
                   it_header-enddate+0(2)
                   it_header-enddate+3(2)
              INTO it_header-enddate.
      WHEN 4.
        it_header-short_text = intern-value.
      WHEN 5.
        it_header-location = intern-value.
      WHEN 6.
        it_header-po_number = intern-value.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = it_header-po_number
          IMPORTING
            output = it_header-po_number.
      WHEN 7.
        it_header-po_item = intern-value.
      WHEN 8.
        it_header-ref_doc_no = intern-value.
*      WHEN 9.
*        it_item-userf1_txt = intern-value.
      WHEN 9.
        it_item-service = intern-value.
      WHEN 10.
        it_item-quantity = intern-value.
*      WHEN 12.
*        it_item-userf1_num = intern-value.
*
** last
*      WHEN 13.
*        it_item-userf2_txt = intern-value.

        IF NOT it_header-po_number IS INITIAL.
          APPEND it_header.
          it_item-po_number = it_header-po_number.
          it_item-po_item = it_header-po_item.
        ENDIF.
*       IF NOT it_item-quantity IS INITIAL.
        APPEND it_item.
        MOVE-CORRESPONDING it_header TO it_data.
        MOVE-CORRESPONDING it_item TO it_data.
        IF it_header-po_number IS INITIAL.
          CLEAR: it_data-po_number, it_data-po_item.
        ENDIF.
        APPEND it_data.
*       ENDIF.
        CLEAR: it_header, it_item-service, it_item-quantity.
*         it_item-userf1_txt,
*        it_item-userf1_num, it_item-userf2_txt.
    ENDCASE.

  ENDLOOP.

  SORT it_item BY po_number po_item. " userf1_txt.

ENDFORM.                    " UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .
  DATA: wa_header TYPE bapiessrc,
        i_return TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
        ws_entrysheet_no TYPE bapiessr-sheet_no,
        i_service TYPE bapiesllc OCCURS 0 WITH HEADER LINE,
        i_service_acc TYPE bapiesklc OCCURS 0 WITH HEADER LINE,
        i_service_text TYPE bapieslltx OCCURS 0 WITH HEADER LINE,
        i_account TYPE bapiesknc OCCURS 0 WITH HEADER LINE,
        ws_pack_no TYPE packno,
        l_index TYPE sy-tabix..

  DATA: ws_wait TYPE bapita-wait VALUE 60.

  DATA: l_po TYPE bapiekko-po_number,
        po_items TYPE bapiekpo OCCURS 0 WITH HEADER LINE,
        po_services TYPE bapiesll OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lw_po_header OCCURS 1.
          INCLUDE STRUCTURE bapiekkol.
  DATA: END OF lw_po_header.

  DATA: BEGIN OF bapi_return_po OCCURS 1.
          INCLUDE STRUCTURE bapireturn.
  DATA: END OF bapi_return_po.

  DATA: serial_no LIKE bapiesknc-serial_no,
        line_no LIKE bapiesllc-line_no.

  DATA: bapi_esll LIKE bapiesllc OCCURS 1 WITH HEADER LINE.

  DATA: l_subpckg_no LIKE po_services-subpckg_no.

  READ TABLE it_header INDEX 1.
  l_po = it_header-po_number.
  CALL FUNCTION 'BAPI_PO_GETDETAIL'
    EXPORTING
      purchaseorder    = l_po
      items            = 'X'
      services         = 'X'
    IMPORTING
      po_header        = lw_po_header
    TABLES
      po_items         = po_items
      po_item_services = po_services
      return           = bapi_return_po.

  MESSAGE s000 WITH 'Please wait, the data is processing ....'.

  LOOP AT it_header.
    l_index = sy-tabix.
    IF it_header-message IS INITIAL.
      REFRESH: i_return,  bapi_esll.
      line_no = 1.
      CLEAR: wa_header,ws_entrysheet_no.

      wa_header-po_number = l_po.
      wa_header-po_item = it_header-po_item.
      wa_header-short_text = it_header-short_text.
      wa_header-location = it_header-location.
      wa_header-ref_doc_no = it_header-ref_doc_no.
      wa_header-begdate = it_header-begdate.
      wa_header-enddate = it_header-enddate.

      READ TABLE po_items WITH KEY po_number = it_header-po_number
                                   po_item = it_header-po_item.
      wa_header-pckg_no = po_items-pckg_no.

      READ TABLE po_services WITH KEY pckg_no = po_items-pckg_no.
      l_subpckg_no = po_services-subpckg_no.

      bapi_esll-pckg_no = po_services-pckg_no.
      bapi_esll-line_no = line_no.        "o_services-line_no.
      bapi_esll-outl_level = po_services-outl_level.
      bapi_esll-outl_ind = 'X'.
      bapi_esll-subpckg_no = po_services-subpckg_no.
      APPEND bapi_esll.

      LOOP AT it_item WHERE po_number = it_header-po_number
                        AND po_item = it_header-po_item.

        READ TABLE po_services WITH KEY pckg_no = l_subpckg_no
                                        outl_ind = ' '
                                        service = it_item-service.

        IF sy-subrc = 0.
          CLEAR bapi_esll.
          line_no  = line_no  + 1.
          bapi_esll-pckg_no = po_services-pckg_no.
          bapi_esll-line_no = line_no.   "po_services-line_no.
          bapi_esll-ext_line = po_services-ext_line.
          bapi_esll-service = it_item-service.
          bapi_esll-outl_level = po_services-outl_level.
          bapi_esll-outl_ind = 'X'.
          bapi_esll-subpckg_no = po_services-subpckg_no.
          bapi_esll-quantity = it_item-quantity.
          bapi_esll-gr_price = po_services-gr_price.
          bapi_esll-price_unit = po_services-price_unit.
*          bapi_esll-userf1_txt = it_item-userf1_txt.  " date
*          bapi_esll-userf1_num = it_item-userf1_num.  "headcount
*          bapi_esll-userf2_txt = it_item-userf2_txt. " shift
          APPEND bapi_esll.
        ENDIF.
      ENDLOOP.

      IF bapi_esll[] IS INITIAL.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'BAPI_ENTRYSHEET_CREATE'
        EXPORTING
          entrysheetheader            = wa_header
*         testrun                     = 'X'
        IMPORTING
          entrysheet                  = ws_entrysheet_no
        TABLES
*         ENTRYSHEETACCOUNTASSIGNMENT =
          entrysheetservices          = bapi_esll
*         entrysheetsrvaccassvalues   = i_service_acc
          return                      = i_return
*         entrysheetservicestexts     = i_service_text
*         ENTRYSHEETHEADERTEXT        =
        .

      IF ws_entrysheet_no IS INITIAL.
        READ TABLE i_return WITH KEY type = 'E'.
        it_data-message = i_return-message.
        MODIFY it_data FROM it_data TRANSPORTING message
         WHERE po_number = it_header-po_number
           AND po_item = it_header-po_item.
        it_header-message =  it_data-message.
        MODIFY it_header INDEX l_index TRANSPORTING message .
      ELSE.
        it_data-message = ws_entrysheet_no.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*          EXPORTING
*            wait = ws_wait.
        WAIT UP TO 5 SECONDS.
        MESSAGE s002 WITH ws_entrysheet_no ' is successfully created'.

        DELETE it_data
         WHERE po_nodis = it_header-po_number
           AND po_item_nodis = it_header-po_item.
        DELETE it_header INDEX l_index.
      ENDIF.
    ELSE.
      MESSAGE s000 WITH 'Item: ' it_header-po_item
       ' has been processed'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  CALL SCREEN 800.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0800 OUTPUT.
  SET PF-STATUS 'ST800'.
  SET TITLEBAR 'ST800'.

ENDMODULE.                 " STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_DATA'.
    PERFORM assign_itab_to_alv.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  CLEAR: w_repid.
  CREATE OBJECT grid_container
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = grid_container
      i_appl_events = 'X'.

ENDFORM.                    " CREATE_CONTAINER_N_OBJECT

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_sortcat_display.

*  IT_SORT-SPOS           = 1.
*  IT_SORT-FIELDNAME      = 'MATNR'.
*  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
*  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname.
*        lw_waers LIKE t001-waers,

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                  'S' 'REF_DATE'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Enter Date',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'BEGDATE'       ' ',
                                  ' ' 'COLTEXT'     'From Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ENDDATE'       ' ',
                                  ' ' 'COLTEXT'     'To Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'SHORT_TEXT'       ' ',
                                  ' ' 'COLTEXT'     'Short Text',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'LOCATION'    ' ',
                                  ' ' 'COLTEXT'     'CC',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'PO_NUMBER'    ' ',
                                  ' ' 'COLTEXT'     'PO Number',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'PO_ITEM'       ' ',
                                  ' ' 'COLTEXT'     'PO Item',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'REF_DOC_NO'       ' ',
                                  ' ' 'COLTEXT'     'Ref Doc No',
                                  'E' 'OUTPUTLEN'   '20',
*
*                                  'S' 'USERF1_TXT'       ' ',
*                                  ' ' 'COLTEXT'     'Date',
*                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'SERVICE'       ' ',
                                  ' ' 'COLTEXT'     'Currency',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'QUANTITY'       ' ',
                                  ' ' 'COLTEXT'     'Quantity',
                                  ' ' 'DECIMALS_O'  '0',
                                  'E' 'OUTPUTLEN'   '8',
*
*                                 'S' 'USERF1_NUM'       ' ',
*                                  ' ' 'COLTEXT'     'Headcount',
*                                  'E' 'OUTPUTLEN'   '5',
*
*                                  'S' 'USERF2_TXT'    ' ',
*                                  ' ' 'COLTEXT'     'Shift',
*                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'MESSAGE'        ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '20'.

ENDFORM.                    "BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv.
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
      i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_data[]
      it_sort              = it_sort[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0800 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'POST'.
      PERFORM process_data.
      PERFORM display_data.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0800  INPUT

*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0040   text
*----------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.

*  fieldln = STRLEN( def_path ) - 1.
*  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
*  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
*    CLEAR <tmp_sym>.
*  ENDIF.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = sy-cprog
      dynpro_number = sy-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = tmp_filename.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
    MESSAGE e000 WITH 'FILE SELECT WINDOW OPEN ERROR!'.
  ENDIF.

ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_data_text .
  DATA: l_filename TYPE string.
  DATA: l_filety TYPE char10 VALUE 'ASC'.
  DATA: l_po_item_nodis LIKE it_header-po_item,
        l_po_nodis LIKE it_header-po_number,
        l_first.

  DATA: BEGIN OF lt_temp OCCURS 0,
* header
        ref_date(8),
        begdate(8),
        enddate(8),
        short_text(40),
        location(15),
        po_number(10),
        po_item(5),
        ref_doc_no(16),
* item
*       userf1_txt(40),
       service(40),
       quantity(13),
*       userf1_num(10),
*       userf2_txt(40),
** output message
*     message LIKE bapireturn-message,
        END OF lt_temp.

  l_filename = p_file.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                      = l_filename
      filetype                      =  l_filety
      has_field_separator           = '#'
*      HEADER_LENGTH                 = 0
      read_by_line                  = 'X'
      dat_mode                      = ' '
*     CODEPAGE                      = ' '
*     IGNORE_CERR                   = ABAP_TRUE
*     REPLACEMENT                   = '#'
*     CHECK_BOM                     = ' '
*     VIRUS_SCAN_PROFILE            =
*     NO_AUTH_CHECK                 = ' '
*   IMPORTING
*     FILELENGTH                    =
*     HEADER                        =
    TABLES
      data_tab                      = lt_temp

*   EXCEPTIONS
*     FILE_OPEN_ERROR               = 1
*     FILE_READ_ERROR               = 2
*     NO_BATCH                      = 3
*     GUI_REFUSE_FILETRANSFER       = 4
*     INVALID_TYPE                  = 5
*     NO_AUTHORITY                  = 6
*     UNKNOWN_ERROR                 = 7
*     BAD_DATA_FORMAT               = 8
*     HEADER_NOT_ALLOWED            = 9
*     SEPARATOR_NOT_ALLOWED         = 10
*     HEADER_TOO_LONG               = 11
*     UNKNOWN_DP_ERROR              = 12
*     ACCESS_DENIED                 = 13
*     DP_OUT_OF_MEMORY              = 14
*     DISK_FULL                     = 15
*     DP_TIMEOUT                    = 16
*     OTHERS                        = 17.
.
  IF sy-subrc = 0.
    DELETE lt_temp FROM 1 TO 3.
  ELSE.
    MESSAGE i000 WITH  'File upload error!'.
  ENDIF.

  LOOP AT lt_temp.
    IF NOT lt_temp-ref_date IS INITIAL.
      CLEAR: it_header.
      it_header-ref_date = lt_temp-ref_date.
      CONCATENATE '20' it_header-ref_date+6(2)
                  it_header-ref_date+0(2)
                  it_header-ref_date+3(2)
             INTO it_header-ref_date.
      it_header-begdate = lt_temp-begdate.
      CONCATENATE '20' it_header-begdate+6(2)
                 it_header-begdate+0(2)
                 it_header-begdate+3(2)
            INTO it_header-begdate.
      it_header-enddate = lt_temp-enddate.
      CONCATENATE '20' it_header-enddate+6(2)
                 it_header-enddate+0(2)
                 it_header-enddate+3(2)
            INTO it_header-enddate.
      it_header-short_text =  lt_temp-short_text.
      it_header-location = lt_temp-location.
      it_header-po_number = lt_temp-po_number.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = it_header-po_number
        IMPORTING
          output = it_header-po_number.
      it_header-po_item = lt_temp-po_item.
      it_header-ref_doc_no = lt_temp-ref_doc_no.
      APPEND it_header.
      it_item-po_number = it_header-po_number.
      it_item-po_item = it_header-po_item.
      l_po_item_nodis = it_header-po_item.
      l_po_nodis = it_header-po_number.
      CLEAR: l_first.
    ENDIF.

*    it_item-userf1_txt = lt_temp-userf1_txt.
    it_item-service = lt_temp-service.
    it_item-quantity = lt_temp-quantity.
*    it_item-userf1_num = lt_temp-userf1_num.
*    it_item-userf2_txt = lt_temp-userf2_txt.

    IF NOT it_item-quantity IS INITIAL.
      APPEND it_item.
      MOVE-CORRESPONDING it_header TO it_data.
      MOVE-CORRESPONDING it_item TO it_data.
      it_data-po_item_nodis = l_po_item_nodis.
      it_data-po_nodis = l_po_nodis.
*      IF lt_temp-po_number IS INITIAL.
*        CLEAR: it_data-po_number, it_data-po_item.
*      ENDIF.
      IF l_first = 'X' AND lt_temp-po_number IS INITIAL.
        CLEAR: it_data-po_number, it_data-po_item.
      ENDIF.
      APPEND it_data.
      l_first = 'X'.
      CLEAR: it_header, it_item-service, it_item-quantity.
*             it_data.
    ENDIF.

*     it_item-userf1_txt,
*     it_item-userf1_num, it_item-userf2_txt.
  ENDLOOP.

ENDFORM.                    " UPLOAD_DATA_TEXT
