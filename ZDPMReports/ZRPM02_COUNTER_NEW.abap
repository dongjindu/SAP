************************************************************************
* Program Name      : ZRPM02_COUNTER
* Author            : Myoung ho, Park
* Creation Date     : 2003.11.29.
* Specifications By :
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : Breakdown Countermeasure report
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 05.21.2014 Victor       Added comment for easy understanding
* 08.31.2014 Victor       Microsoft no longer support the link server,
*                         so program has been changed with other object
************************************************************************

************************************************************************
************************************************************************
*   Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
*    (Demonstration for Excel 97 Integration (Optimized))
*   Reference : SAP Library - Desktop Office Integration (BC-CI)
*                           - Business Document Service (BC-SRV-BDS)
*
* Screen 0100 for This Report
* Screen 0101 for PMIS (SAPMZAPM10_INFO)
* Two Screen same logic exempt Status & Tile...
************************************************************************
REPORT  zrpm02_counter   MESSAGE-ID demoofficeintegratio.

INCLUDE zrpmoffice_top.

**** Copy from SAPRDEMOEXCELINTEGRATION2
CONSTANTS: true VALUE 1, false VALUE 0.

TYPES: t_oi_ret_string TYPE soi_ret_string.

DATA: control TYPE REF TO i_oi_container_control.
DATA: container TYPE REF TO cl_gui_custom_container.
DATA: document TYPE REF TO i_oi_document_proxy.
DATA: r_excel     TYPE REF TO i_oi_spreadsheet. "Victor
DATA: r_error     TYPE REF TO i_oi_error.
DATA: link_server TYPE REF TO i_oi_link_server.
DATA: bds_instance TYPE REF TO cl_bds_document_set.

DATA: retcode TYPE t_oi_ret_string,
      document_type(80) VALUE soi_doctype_excel97_sheet,
*      DOCUMENT_TYPE(80) VALUE SOI_DOCTYPE_EXCEL_SHEET, "'Excel.Sheet'
      document_format(80) TYPE c,
      descr TYPE document_descr.

DATA: data_table TYPE sbdst_content,
      data_size TYPE i, doc_url TYPE t_url,
      has_changed TYPE i,
      document_mimetype TYPE bapicompon-mimetype.

DATA: first_open VALUE '1'.
DATA: open_document(1).

CONSTANTS: c_line_count TYPE i VALUE 5.
**********************************************************************
**********************************************************************

*---Global Variables & Tables
*** Screen Structure...
TABLES: zspm_comp,        "//Order Completetion Confirmation
        zspm_time_conf,   "//Time Confirmation
        zspm_counter.     "//Countermeasure

*** Data Select Table...
TABLES: afru,             "//Order completion confirmations
        itob,             "//PM technical objects
                          "//(EQUI, funcational location)
        qmur,             "//Quality notification - causes
        qmma,             "//Quality notification - activities.
        viqmfe,           "//PM Notification - Item
        mkpf,             "//Header: Material Document
        mseg.             "//Document Segment: Material

DATA : wa_ename LIKE pa0001-ename.  "//Equipment Personnel
"//(Empl./appl.name)
DATA : wa_fing LIKE t357-fing.    "//shop name(Plant section)
DATA : wa_ktext LIKE crtx-ktext.  "//line name(Work center)
DATA : wa_name LIKE adrp-name_text. "//Reported by

DATA : BEGIN OF it_counter OCCURS 0.
        INCLUDE STRUCTURE zspm_counter2.
DATA : END OF it_counter.

DATA : it_temp_counter LIKE zspm_counter2 OCCURS 0.

*** for Long text....
*** SAPscript: Text Header
DATA : headltx    LIKE thead.

*** Text Header Names
DATA: wa_tdname01 LIKE headltx-tdname.
DATA: wa_tdname02 LIKE headltx-tdname.
DATA: wa_tdname03 LIKE headltx-tdname.
DATA: wa_tdname04 LIKE headltx-tdname.

***  text internal tables
DATA: BEGIN OF it_ltxttab01 OCCURS 100.
        INCLUDE STRUCTURE tline.
DATA: END OF it_ltxttab01.

DATA: BEGIN OF it_ltxttab02 OCCURS 100.
        INCLUDE STRUCTURE tline.
DATA: END OF it_ltxttab02.

DATA: BEGIN OF it_ltxttab03 OCCURS 100.
        INCLUDE STRUCTURE tline.
DATA: END OF it_ltxttab03.

DATA: BEGIN OF it_ltxttab04_01 OCCURS 100.
        INCLUDE STRUCTURE tline.
DATA: END OF it_ltxttab04_01.

DATA: BEGIN OF it_ltxttab04_02 OCCURS 100.
        INCLUDE STRUCTURE tline.
DATA: END OF it_ltxttab04_02.

DATA: BEGIN OF it_ltxttab04_03 OCCURS 100.
        INCLUDE STRUCTURE tline.
DATA: END OF it_ltxttab04_03.

**** Temp text internal tables for LinkServer
DATA : it_temp_ltxttab01 LIKE tline OCCURS 100.
DATA : it_temp_ltxttab02 LIKE tline OCCURS 100.
DATA : it_temp_ltxttab03 LIKE tline OCCURS 100.

DATA : it_temp_ltxttab04_01 LIKE tline OCCURS 100.
DATA : it_temp_ltxttab04_02 LIKE tline OCCURS 100.
DATA : it_temp_ltxttab04_03 LIKE tline OCCURS 100.

DATA: wa_shop_line(30).
DATA: wa_period(25).
DATA: wa_time(20),
      wa_duration(15).

FIELD-SYMBOLS: <txttab> TYPE text_line_tab.
*---------------------------------------------------------------------*
*       CLASS c_event_handler DEFINITION
*---------------------------------------------------------------------*
*      Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
*
*---------------------------------------------------------------------*
CLASS c_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: close_event_handler
              FOR EVENT on_close_document OF i_oi_document_proxy
              IMPORTING document_proxy has_changed.

    CLASS-METHODS: custom_event_handler
              FOR EVENT on_custom_event OF i_oi_document_proxy
              IMPORTING document_proxy event_name param_count
                        param1 param2 param3.

ENDCLASS.                    "C_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS c_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
*        Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
*
*---------------------------------------------------------------------*
CLASS c_event_handler IMPLEMENTATION.
  METHOD close_event_handler.
    DATA: answer.
    PERFORM save_document TABLES data_table
                USING 'X' 'X'
                CHANGING data_size document_proxy retcode.
    open_document = false.
  ENDMETHOD.                    "CLOSE_EVENT_HANDLER

  METHOD custom_event_handler.

  ENDMETHOD.                    "CUSTOM_EVENT_HANDLER
ENDCLASS.                    "C_EVENT_HANDLER IMPLEMENTATION

*********** SELECTION-SCREEN ***********************************
****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

PARAMETER : p_aufnr LIKE zspm_comp-aufnr MEMORY ID  anr.

SELECTION-SCREEN END OF BLOCK block1.

******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.


***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.
      CLEAR: sy-ucomm.
      CALL SCREEN '0100'.

    WHEN OTHERS.
      CLEAR: sy-ucomm.

  ENDCASE.
****************START-OF-SELECTION ************************************
***********************************************************************
START-OF-SELECTION.

END-OF-SELECTION.
****************** END-OF-SELECTION **********************************
**********************************************************************

*&---------------------------------------------------------------------*
*&      Form  SAVE_DOCUMENT
*&---------------------------------------------------------------------*
*       Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM save_document TABLES   doc_table TYPE table
                   USING    do_ask TYPE c do_release TYPE c
                   CHANGING doc_size TYPE i
                            document TYPE REF TO i_oi_document_proxy
                            retcode TYPE t_oi_ret_string.

  DATA: is_closed TYPE i, answer TYPE c, has_changed TYPE i.

  CALL METHOD document->is_destroyed
    IMPORTING
      ret_value = is_closed.

  IF is_closed IS INITIAL.
    CALL METHOD document->close_document
      EXPORTING
        do_save     = 'X'
      IMPORTING
        has_changed = has_changed
        retcode     = retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.
  ENDIF.

  IF NOT do_release IS INITIAL.
    CALL METHOD document->release_document
      IMPORTING
        retcode = retcode.
  ENDIF.

ENDFORM.                    " SAVE_DOCUMENT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Object container create
*       GET_LINK_SERVER
*       START_LINK_SERVER
*       GET_DOCUMENT_PROXY
*       REFRESH_LINKS
*       PERFORM OPEN_DOCUMENT
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

*** set Title & Status
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

  retcode = c_oi_errors=>ret_ok.

*** create BusinessDocument object
  IF bds_instance IS INITIAL.
    CREATE OBJECT bds_instance.
  ENDIF.

  IF control IS INITIAL.

    DATA: b_has_activex.
*** Test Whether ActiveX Controls are Supported
    CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
        return = b_has_activex.
    IF b_has_activex IS INITIAL. MESSAGE e007. ENDIF.
**** Create Container Control Factory
    CALL METHOD c_oi_container_control_creator=>get_container_control
      IMPORTING
        control = control
        retcode = retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.

**** Create Container Control 'CONTAINER_01'
    CREATE OBJECT container
      EXPORTING
        container_name = 'CONTAINER'.

**** Creates and Initializes the Control
    CALL METHOD control->init_control
      EXPORTING
        r3_application_name      = 'PM Information System'  "#EC NOTEXT
        inplace_enabled          = 'X'
        inplace_scroll_documents = 'X'
        parent                   = container
        register_on_close_event  = 'X'
        register_on_custom_event = 'X'
        no_flush                 = 'X'
      IMPORTING
        retcode                  = retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.

**** Creates an Instance for the Link Server
    CALL METHOD control->get_link_server
      IMPORTING
        link_server = link_server
        retcode     = retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.
***** Activates the Link Server
****  Link Server name SUFFIX : 'COUNT'
    CALL METHOD link_server->start_link_server
                      EXPORTING link_server_mode =
*                                LINK_SERVER->LINK_SERVER_STANDARDNAME
                                link_server->link_server_customname
                                server_name_suffix = 'COUNT'
                      IMPORTING retcode = retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.
*****Creates an Instance for Document Management
    CALL METHOD control->get_document_proxy
      EXPORTING
        document_type   = document_type
*       document_FORMAT = 'OLE' "VICTOR
      IMPORTING
        document_proxy  = document
        retcode         = retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.

    SET HANDLER c_event_handler=>close_event_handler FOR document.
    SET HANDLER c_event_handler=>custom_event_handler FOR document.


*** Refresh Links ...
    PERFORM refresh_links.
*** Open document...
    PERFORM open_document.

  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_LINKS
*&---------------------------------------------------------------------*
*       Link LinkServer to Internal Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_links.

  PERFORM read_data.

  IF NOT link_server IS INITIAL.

    PERFORM link_breakdown_long_text.
    PERFORM link_countermeasure_detail.
    PERFORM link_countermeasure_long_text.

    PERFORM link_header_info.
    PERFORM link_breakdown_detail.

  ENDIF.
ENDFORM.                    " REFRESH_LINKS
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CALL METHOD cl_gui_cfw=>dispatch.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR: sy-ucomm.
      PERFORM free_object.
      LEAVE PROGRAM.
*      LEAVE TO TRANSACTION SY-TCODE.


    WHEN 'EXCEL'.
      CLEAR: sy-ucomm.
      PERFORM refresh_links.
      PERFORM open_document.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  LOAD_DOCUMENT
*&---------------------------------------------------------------------*
*        Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
*----------------------------------------------------------------------*
*      <--P_DOCUMENT_TYPE  text
*      <--P_DOCUMENT_FORMAT  text
*      <--P_DOC_URL  text
*----------------------------------------------------------------------*
FORM load_document CHANGING document_type   TYPE c
                            document_format TYPE c
                            doc_url         TYPE t_url.


*** Documents are managed BDS (Business Document Service)
*** T-code : OAOR

* Tables and WAs:
  DATA: doc_signature TYPE sbdst_signature,
        wa_doc_signature LIKE LINE OF doc_signature,
        doc_components TYPE sbdst_components,
        wa_doc_components LIKE LINE OF doc_components,
        doc_properties TYPE sbdst_properties,
        wa_doc_properties LIKE LINE OF doc_properties,
        doc_uris TYPE sbdst_uri,
        wa_doc_uris LIKE LINE OF doc_uris.
* IDs:
*  DATA: DOC_CLASSNAME TYPE SBDST_CLASSNAME VALUE 'ZPMDOCUMENT',
  DATA: doc_classname TYPE sbdst_classname VALUE 'ZPMDOCUMENT1',
        doc_classtype TYPE sbdst_classtype VALUE 'OT',
        doc_object_key TYPE sbdst_object_key VALUE 'COUNT',
        doc_mimetype LIKE bapicompon-mimetype.


  CLEAR doc_url.

  wa_doc_signature-prop_name = 'DESCRIPTION'.
  wa_doc_signature-prop_value = 'Breakdown Countermeasure Report'.
  "'Technical Spec Template(Report).doc'.
  "//DESCR-DOCUMENT_ID.
  APPEND wa_doc_signature TO doc_signature.

  CALL METHOD bds_instance->get_info
    EXPORTING
      classname       = doc_classname
      classtype       = doc_classtype
      object_key      = doc_object_key
    CHANGING
      components      = doc_components
      signature       = doc_signature
    EXCEPTIONS
      nothing_found   = 1
      error_kpro      = 2
      internal_error  = 3
      parameter_error = 4
      not_authorized  = 5
      not_allowed     = 6.
  IF sy-subrc NE 0 AND sy-subrc NE 1.
    MESSAGE e016.
  ENDIF.
  IF sy-subrc = 1.
    MESSAGE e017.
  ENDIF.
****** get document URL on sap server...
  CALL METHOD bds_instance->get_with_url
    EXPORTING
      classname       = doc_classname
      classtype       = doc_classtype
      object_key      = doc_object_key
    CHANGING
      uris            = doc_uris
      signature       = doc_signature
    EXCEPTIONS
      nothing_found   = 1
      error_kpro      = 2
      internal_error  = 3
      parameter_error = 4
      not_authorized  = 5
      not_allowed     = 6.
  IF sy-subrc NE 0 AND sy-subrc NE 1.
    MESSAGE e016.
  ENDIF.
  IF sy-subrc = 1.
    MESSAGE e017.
  ENDIF.

  READ TABLE doc_components INTO wa_doc_components INDEX 1.
  READ TABLE doc_uris INTO wa_doc_uris INDEX 1.
  doc_mimetype = wa_doc_components-mimetype.
  document_mimetype = doc_mimetype.
  doc_url = wa_doc_uris-uri.

  CASE doc_mimetype.
    WHEN 'application/x-rtf' OR 'text/rtf'.
      document_format = soi_docformat_rtf.
    WHEN 'application/x-oleobject'.
      document_format = soi_docformat_compound.
    WHEN 'text/plain'.
      document_format = soi_docformat_text.
    WHEN OTHERS.
      document_format = soi_docformat_native.
*      DOCUMENT_FORMAT = SOI_DOCFORMAT_COMPOUND. "VICTOR
  ENDCASE.


ENDFORM.                    " LOAD_DOCUMENT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN '%EX'.
      CLEAR: sy-ucomm.
      PERFORM free_object.
      LEAVE PROGRAM.
*      LEAVE TO TRANSACTION SY-TCODE.

    WHEN 'RW'.
      CLEAR: sy-ucomm.
      PERFORM free_object.
      LEAVE PROGRAM.
*      LEAVE TO TRANSACTION SY-TCODE.

  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_object.
  DATA: is_closed TYPE i.

  IF NOT document IS INITIAL.
    PERFORM save_document TABLES data_table
               USING 'X' 'X'
               CHANGING data_size document retcode.
    FREE document.
  ENDIF.

  IF NOT link_server IS INITIAL.
    CALL METHOD link_server->stop_link_server
      IMPORTING
        retcode = retcode.
    FREE link_server.
  ENDIF.

  IF NOT control IS INITIAL.
    CALL METHOD control->destroy_control.
    FREE control.
  ENDIF.

  IF NOT bds_instance IS INITIAL.
    FREE bds_instance.
  ENDIF.

*  LEAVE PROGRAM.
ENDFORM.                    " FREE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       Read data each Shop's Breakdown Rate...
*       By annually and monthly
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.

  CLEAR: zspm_comp.
  PERFORM read_header_info.
  PERFORM read_breakdown_detail.
  PERFORM read_breakdown_long_text.
  PERFORM read_countermeasure_detail.
  PERFORM read_countermeasure_long_text.
  PERFORM fill_empty_cell.

ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  OPEN_DOCUMENT
*&---------------------------------------------------------------------*
*        Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM open_document.

  IF NOT document IS INITIAL.
    PERFORM save_document TABLES data_table
               USING 'X' 'X'
               CHANGING data_size document retcode.
  ENDIF.

  IF NOT control IS INITIAL.

    PERFORM load_document CHANGING
                                  document_type
                                  document_format
                                  doc_url.

    IF NOT doc_url IS INITIAL.
      CALL METHOD document->open_document
        EXPORTING
          document_url = doc_url
          open_inplace = 'X'
        IMPORTING
          retcode      = retcode.
      CALL METHOD c_oi_errors=>show_message
        EXPORTING
          type = 'E'.

*-<Victor
      CALL METHOD document->get_spreadsheet_interface
        IMPORTING
          sheet_interface = r_excel
          retcode         = retcode.

      IF retcode NE c_oi_errors=>ret_ok.
        CALL METHOD c_oi_errors=>show_message
          EXPORTING
            type = 'E'.
      ENDIF.

      PERFORM display_table_data.


*->

*-< comment by Victor
* Document shall also be available in ITAB for respective operations:
*      CALL METHOD document->save_document_to_table
**                        EXPORTING NO_FLUSH = 'X'  "Victor
*                        IMPORTING retcode = retcode
*                        CHANGING  document_table = data_table
*                                  document_size = data_size.
*      CALL METHOD c_oi_errors=>show_message
*        EXPORTING
*          type = 'E'.
*->
      first_open = false.
      open_document = true.

    ELSE.
      MESSAGE e010.
    ENDIF.
  ENDIF.
ENDFORM.                    " OPEN_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  READ_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXTTAB01  text
*      -->P_HEADLTX  text
*----------------------------------------------------------------------*
FORM read_long_text TABLES   pt_ltxttab STRUCTURE tline
                    USING    p_headltx  LIKE thead.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id        = p_headltx-tdid
      language  = p_headltx-tdspras
      name      = p_headltx-tdname
      object    = p_headltx-tdobject
    IMPORTING
      header    = p_headltx
    TABLES
      lines     = pt_ltxttab
    EXCEPTIONS
      not_found = 1.

ENDFORM.                    " READ_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  LINK_HEADER_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM link_header_info.
*** Order Number
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = p_aufnr
*    IMPORTING
*      output = p_aufnr.
*
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Order_No'
*      item_value = p_aufnr
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

*** Breakdown_Period
  DATA : wa_ausvn(10).
  DATA : wa_ausbs(10).

  WRITE : zspm_comp-ausvn TO wa_ausvn MM/DD/YYYY.
  WRITE : zspm_comp-ausbs TO wa_ausbs MM/DD/YYYY.
  CONCATENATE wa_ausvn ' - ' wa_ausbs INTO wa_period.

*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Period'
*      item_value = wa_period
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

**** Breakdwon Time
  DATA : wa_auztv(8).
  DATA : wa_auztb(8).

  WRITE zspm_comp-auztv TO wa_auztv. "TIME ZONE SY-TZONE.
  WRITE zspm_comp-auztb TO wa_auztb. "TIME ZONE SY-TZONE.
  CONCATENATE wa_auztv ' - ' wa_auztb INTO wa_time.

*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Time'
*      item_value = wa_time
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

****  Breakdown Time
  DATA: wa_down(25),
        wa_auszt(22).

  CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
    EXPORTING
      char_unit       = zspm_comp-maueh
      decimals        = 0
      exponent        = 0
      fltp_value_si   = zspm_comp-auszt
      indicator_value = 'X'
      masc_symbol     = ' '
    IMPORTING
      char_value      = wa_auszt.

*  WRITE ZSPM_COMP-AUSZT TO WA_AUSZT EXPONENT 0 DECIMALS 0.
  CONCATENATE wa_auszt ' ' zspm_comp-maueh INTO wa_down.
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Down_Time'
*      item_value = wa_down
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

**** Line Down Time(Actual duration)
  DATA: wa_idaur(7).
  WRITE zspm_comp-idaur TO wa_idaur UNIT zspm_comp-idaue.
  CONCATENATE wa_idaur ' - ' zspm_comp-idaue INTO wa_duration.

*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Duration'
*      item_value = wa_duration
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

*** Title
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Title'
*      item_value = zspm_comp-ktext
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

*** Shop/Line
  CONCATENATE  wa_fing ' / ' wa_ktext INTO wa_shop_line.

*  CALL METHOD link_server->add_string_item
*              EXPORTING item_name = 'Shop_Line'
** changed by 100565 07_10_04
**                        ITEM_VALUE  = WA_SHOP_LINE
*                        item_value  = wa_fing
** end changed by 100565 07_10_04
*                        no_flush    = 'X'
*              IMPORTING retcode = retcode.
**** Equipment
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = zspm_comp-equnr
    IMPORTING
      output = zspm_comp-equnr.

*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Equipment'
*      item_value = zspm_comp-equnr
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.
*
***** Equipment Name
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Equipment_Name'
*      item_value = itob-shtxt
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.
*
**** Equipment Personnel
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Equip_Personnel'
*      item_value = wa_ename
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

**** Executor
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Executor'
*      item_value = zspm_comp-qmnam
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.
*
***** Process(Room)
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Process'
*      item_value = itob-msgrp
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.
*
***** Dept.name (Shop)
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Dept'
*      item_value = wa_fing
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.
*****change by 100565 07_10_04
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Shop_Name'
*      item_value = wa_fing
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.
*
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Line_Name'
*      item_value = wa_ktext
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.


****endchange by 100565 07_10_04


**** Report by
*  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*              EXPORTING ITEM_NAME = 'User'
*                        ITEM_VALUE  = WA_NAME
*                        NO_FLUSH    = 'X'
*              IMPORTING RETCODE = RETCODE.

***** Report Date
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'report_date'
*      item_value = sy-datum
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

ENDFORM.                    " LINK_HEADER_INFO
*&---------------------------------------------------------------------*
*&      Form  LINK_BREAKDOWN_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM link_breakdown_detail.
***Damage Code
*  ZSPM_COMP-FECOD = '01'.
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Damage_Code'
*      item_value = zspm_comp-fecod
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

**** Damage Code text
  SHIFT zspm_comp-txtcdfe BY 4 PLACES RIGHT.
*  ZSPM_COMP-TXTCDOT = 'Damage_Text'.
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Damage_Text'
*      item_value = zspm_comp-txtcdfe
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

**** Cause Code
*  ZSPM_COMP-URCOD = 'Cause_Code'.
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Cause_Code'
*      item_value = zspm_comp-urcod
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

**** Cause Code text
  SHIFT zspm_comp-txtcdur BY 4 PLACES RIGHT.
*  ZSPM_COMP-TXTCDUR = 'Cause_Text'.
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Cause_Text'
*      item_value = zspm_comp-txtcdur
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

***** Activity Code
*  ZSPM_COMP-MNCOD = 'Activity_Code'.
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Activity_Code'
*      item_value = zspm_comp-mncod
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode.

***** Activity Code text
  SHIFT zspm_comp-txtcdma BY 4 PLACES RIGHT.
*  ZSPM_COMP-TXTCDMA = 'Activity_Text'.
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Activity_Text'
*      item_value = zspm_comp-txtcdma
*      no_flush   = ' '
*    IMPORTING
*      retcode    = retcode.

ENDFORM.                    " LINK_BREAKDOWN_DETAIL
*&---------------------------------------------------------------------*
*&      Form  link_breakdown_long_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM link_breakdown_long_text.
*  DATA: L_TXTCDFE LIKE ZSPM_COMP-TXTCDFE,
*        L_TXTCDUR LIKE TLINE-TLINE.
  DATA: l_text LIKE tline-tdline.

****** Damage Long text
* CHANGED BY 100565
  LOOP AT it_ltxttab01.
    IF sy-tabix = '1'.
      SHIFT it_ltxttab01-tdline BY 4 PLACES RIGHT.
      MODIFY it_ltxttab01.
    ENDIF.
  ENDLOOP.
*sort IT_LTXTTAB01 by tdline descending.


*  IT_TEMP_LTXTTAB01[] = IT_LTXTTAB01[].
*
*  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
*           EXPORTING ITEM_NAME   = 'Damage_Long'
*                     ITEM_TITLE  = 'Damage Long text'
*                     DDIC_NAME   = 'TLINE'
*                     NO_FLUSH    = 'X'
*           IMPORTING RETCODE = RETCODE
*           CHANGING  DATA_TABLE   = IT_TEMP_LTXTTAB01.
*  L_TXTCDFE = ZSPM_COMP-TXTCDFE.

*  READ TABLE it_ltxttab01 INDEX 1.
*  IF sy-subrc = 0.
*    l_text = it_ltxttab01-tdline.
*
*    CALL METHOD link_server->add_string_item
*      EXPORTING
*        item_name  = 'Damage_L1'
*        item_value = l_text   "ZSPM_COMP-TXTCDFE
*        no_flush   = 'X'
*      IMPORTING
*        retcode    = retcode.
*  ENDIF.
*  READ TABLE it_ltxttab01 INDEX 2.
*  IF sy-subrc = 0.
**    ZSPM_COMP-TXTCDFE = IT_LTXTTAB01-TDLINE.
*    l_text = it_ltxttab01-tdline.
*    CALL METHOD link_server->add_string_item
*      EXPORTING
*        item_name  = 'Damage_L2'
*        item_value = l_text  "ZSPM_COMP-TXTCDFE
*        no_flush   = 'X'
*      IMPORTING
*        retcode    = retcode.
*  ENDIF.
*  ZSPM_COMP-TXTCDFE = L_TXTCDFE.

****** Cause Long text
*  MOVE 'Cause Long text' TO IT_LTXTTAB02-TDLINE.
*  APPEND IT_LTXTTAB02.

* CHANGED BY 100565
  LOOP AT it_ltxttab02.
    IF sy-tabix = '1'.
      SHIFT it_ltxttab02-tdline BY 4 PLACES RIGHT.
      MODIFY it_ltxttab02.
    ENDIF.
  ENDLOOP.
*  IT_TEMP_LTXTTAB02[] = IT_LTXTTAB02[].
*  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
*           EXPORTING ITEM_NAME   = 'Cause_Long'
*                     ITEM_TITLE  = 'Cause Long text'
*                     DDIC_NAME   = 'TLINE'
*                     NO_FLUSH    = 'X'
*           IMPORTING RETCODE = RETCODE
*           CHANGING  DATA_TABLE   = IT_TEMP_LTXTTAB02.

*  L_TXTCDUR =  ZSPM_COMP-TXTCDUR.
*  READ TABLE it_ltxttab02 INDEX 1.
*  IF sy-subrc = 0.
**    ZSPM_COMP-TXTCDUR = IT_LTXTTAB02-TDLINE.
*    l_text = it_ltxttab02-tdline.
*    CALL METHOD link_server->add_string_item
*      EXPORTING
*        item_name  = 'Cause_L1'
*        item_value = l_text    "ZSPM_COMP-TXTCDUR
*        no_flush   = 'X'
*      IMPORTING
*        retcode    = retcode.
*  ENDIF.
*  READ TABLE it_ltxttab02 INDEX 2.
*  IF sy-subrc = 0.
**    ZSPM_COMP-TXTCDUR = IT_LTXTTAB02-TDLINE.
*    l_text = it_ltxttab02-tdline.
*    CALL METHOD link_server->add_string_item
*      EXPORTING
*        item_name  = 'Cause_L2'
*        item_value = l_text  " ZSPM_COMP-TXTCDUR
*        no_flush   = 'X'
*      IMPORTING
*        retcode    = retcode.
*  ENDIF.
*  READ TABLE it_ltxttab02 INDEX 3.
*  IF sy-subrc = 0.
**    ZSPM_COMP-TXTCDUR = IT_LTXTTAB02-TDLINE.
*    l_text = it_ltxttab02-tdline.
*    CALL METHOD link_server->add_string_item
*      EXPORTING
*        item_name  = 'Cause_L3'
*        item_value = l_text  " ZSPM_COMP-TXTCDUR
*        no_flush   = 'X'
*      IMPORTING
*        retcode    = retcode.
*  ENDIF.

*  ZSPM_COMP-TXTCDUR = L_TXTCDUR.

****** Activity Long text
*  MOVE 'Activity Long text1' TO IT_LTXTTAB03-TDLINE.
*  APPEND IT_LTXTTAB03.
*  MOVE 'Activity Long text2' TO IT_LTXTTAB03-TDLINE.
*  APPEND IT_LTXTTAB03.

* CHANGED BY 100565
  LOOP AT it_ltxttab03.
    IF sy-tabix = '1'.
      SHIFT it_ltxttab03-tdline BY 4 PLACES RIGHT.
      MODIFY it_ltxttab03.
    ENDIF.
  ENDLOOP.

*  IT_TEMP_LTXTTAB03[] = IT_LTXTTAB03[].
*  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
*           EXPORTING ITEM_NAME   = 'Activity_Long'
*                     ITEM_TITLE  = 'Activity Long text'
*                     DDIC_NAME   = 'TLINE'
*                     NO_FLUSH    = 'X'
*           IMPORTING RETCODE = RETCODE
*           CHANGING  DATA_TABLE   = IT_TEMP_LTXTTAB03.

*  READ TABLE it_ltxttab03 INDEX 1.
*  IF sy-subrc = 0.
**    ZSPM_COMP-TXTCDUR = IT_LTXTTAB02-TDLINE.
*    l_text = it_ltxttab03-tdline.
*
*    CALL METHOD link_server->add_string_item
*      EXPORTING
*        item_name  = 'Activity_L1'
*        item_value = l_text   "ZSPM_COMP-TXTCDMA
*        no_flush   = ' '
*      IMPORTING
*        retcode    = retcode.
*  ENDIF.
*  READ TABLE it_ltxttab03 INDEX 2.
*  IF sy-subrc = 0.
**    ZSPM_COMP-TXTCDUR = IT_LTXTTAB02-TDLINE.
*    l_text = it_ltxttab03-tdline.
*
*    CALL METHOD link_server->add_string_item
*      EXPORTING
*        item_name  = 'Activity_L2'
*        item_value = l_text   "ZSPM_COMP-TXTCDMA
*        no_flush   = ' '
*      IMPORTING
*        retcode    = retcode.
*  ENDIF.
*  READ TABLE it_ltxttab03 INDEX 3.
*  IF sy-subrc = 0.
**    ZSPM_COMP-TXTCDUR = IT_LTXTTAB02-TDLINE.
*    l_text = it_ltxttab03-tdline.
*
*    CALL METHOD link_server->add_string_item
*      EXPORTING
*        item_name  = 'Activity_L3'
*        item_value = l_text   "ZSPM_COMP-TXTCDMA
*        no_flush   = ' '
*      IMPORTING
*        retcode    = retcode.
*  ENDIF.
*  READ TABLE it_ltxttab03 INDEX 4.
*  IF sy-subrc = 0.
**    ZSPM_COMP-TXTCDUR = IT_LTXTTAB02-TDLINE.
*    l_text = it_ltxttab03-tdline.
*
*    CALL METHOD link_server->add_string_item
*      EXPORTING
*        item_name  = 'Activity_L4'
*        item_value = l_text   "ZSPM_COMP-TXTCDMA
*        no_flush   = ' '
*      IMPORTING
*        retcode    = retcode.
*  ENDIF.
*  READ TABLE it_ltxttab03 INDEX 5.
**  IF SY-SUBRC = 0.
**    ZSPM_COMP-TXTCDUR = IT_LTXTTAB02-TDLINE.
*  l_text = it_ltxttab03-tdline.
*
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Activity_L5'
*      item_value = l_text   "ZSPM_COMP-TXTCDMA
*      no_flush   = ' '
*    IMPORTING
*      retcode    = retcode.
**  ENDIF.
*
*  READ TABLE it_ltxttab03 INDEX 6.
**  IF SY-SUBRC = 0.
**    ZSPM_COMP-TXTCDUR = IT_LTXTTAB02-TDLINE.
*  l_text = it_ltxttab03-tdline.
*
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Activity_L6'
*      item_value = l_text   "ZSPM_COMP-TXTCDMA
*      no_flush   = ' '
*    IMPORTING
*      retcode    = retcode.
**  ENDIF.
*  READ TABLE it_ltxttab03 INDEX 7.
**  IF SY-SUBRC = 0.
**    ZSPM_COMP-TXTCDUR = IT_LTXTTAB02-TDLINE.
*  l_text = it_ltxttab03-tdline.
*
*  CALL METHOD link_server->add_string_item
*    EXPORTING
*      item_name  = 'Activity_L7'
*      item_value = l_text   "ZSPM_COMP-TXTCDMA
*      no_flush   = ' '
*    IMPORTING
*      retcode    = retcode.
**  ENDIF.

ENDFORM.                    " link_breakdown_long_text
*&---------------------------------------------------------------------*
*&      Form  LINK_COUNTERMEASURE_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM link_countermeasure_detail.

*  LOOP AT it_counter WHERE aufnr NE space.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = it_counter-aufnr
*      IMPORTING
*        output = it_counter-aufnr.
*    MODIFY it_counter TRANSPORTING aufnr.
*  ENDLOOP.
*
*  it_temp_counter[] = it_counter[].
*
*  CALL METHOD link_server->add_table_item2
*    EXPORTING
*      item_name  = 'Counter'
*      item_title = 'Countermeasure info'
*      ddic_name  = 'ZSPM_COUNTER2'
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode
*    CHANGING
*      data_table = it_temp_counter.
ENDFORM.                    " LINK_COUNTERMEASURE_DETAIL
*&---------------------------------------------------------------------*
*&      Form  LINK_COUNTERMEASURE_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM link_countermeasure_long_text.
****** Countermeasure Long text
***** # 1
*  MOVE 'Countermeasure Long text #1' TO IT_LTXTTAB04_1-TDLINE.
*  APPEND IT_LTXTTAB04_1.

*  it_temp_ltxttab04_01[] = it_ltxttab04_01[].
*  CALL METHOD link_server->add_table_item2
*    EXPORTING
*      item_name  = 'Counter_Long1'
*      item_title = 'Countermeasure Long text #1'
*      ddic_name  = 'TLINE'
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode
*    CHANGING
*      data_table = it_temp_ltxttab04_01.
****** # 2
**  MOVE 'Countermeasure Long text #2' TO IT_LTXTTAB04_2-TDLINE.
**  APPEND IT_LTXTTAB04_2.
*
*  it_temp_ltxttab04_02[] = it_ltxttab04_02[].
*  CALL METHOD link_server->add_table_item2
*    EXPORTING
*      item_name  = 'Counter_Long2'
*      item_title = 'Countermeasure Long text #2'
*      ddic_name  = 'TLINE'
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode
*    CHANGING
*      data_table = it_temp_ltxttab04_02.
****** # 3
**  MOVE 'Countermeasure Long text #3' TO IT_LTXTTAB04_3-TDLINE.
**  APPEND IT_LTXTTAB04_3.
*
*  it_temp_ltxttab04_03[] = it_ltxttab04_03[].
*  CALL METHOD link_server->add_table_item2
*    EXPORTING
*      item_name  = 'Counter_Long3'
*      item_title = 'Countermeasure Long text #3'
*      ddic_name  = 'TLINE'
*      no_flush   = 'X'
*    IMPORTING
*      retcode    = retcode
*    CHANGING
*      data_table = it_temp_ltxttab04_03.
ENDFORM.                    " LINK_COUNTERMEASURE_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_HEADER_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_header_info.
**** Read Order detail info
**** Check Notification & Read Description
  SELECT SINGLE a~qmnum b~ktext a~ausvn a~ausbs
                a~auztv a~auztb a~auszt a~maueh
                a~equnr a~qmnam a~aufnr
         INTO  (zspm_comp-qmnum, zspm_comp-ktext,
                zspm_comp-ausvn, zspm_comp-ausbs,
                zspm_comp-auztv, zspm_comp-auztb,
                zspm_comp-auszt, zspm_comp-maueh,
                zspm_comp-equnr, zspm_comp-qmnam, zspm_comp-aufnr)
                         FROM   viqmel AS a
                                INNER JOIN aufk AS b
                                      ON a~aufnr = b~aufnr
                         WHERE  a~aufnr = p_aufnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zmpm) WITH text-001 zspm_comp-aufnr text-m01.
  ELSE.
**** Line Down Time..(Actual duration)
    CLEAR : afru.
    SELECT SINGLE idaur idaue
           INTO  (zspm_comp-idaur, zspm_comp-idaue)
           FROM  afru
           WHERE aufnr = p_aufnr
           AND   rmzhl = '00000001'.

**** Equipment master data
    CLEAR : itob.
    SELECT SINGLE *
           FROM  itob
           WHERE equnr = zspm_comp-equnr.
    IF sy-subrc EQ 0.
      SELECT SINGLE fing
             INTO wa_fing
             FROM t357
             WHERE beber = itob-beber.

      SELECT SINGLE ktext
             INTO wa_ktext
             FROM crtx
             WHERE objty  = 'A'
             AND    objid = itob-wkctr.

      SELECT SINGLE  b~ename
             INTO  wa_ename
             FROM  ihpa AS a
                   INNER JOIN pa0001 AS b
                   ON a~parnr = b~pernr
             WHERE a~objnr = itob-objnr.
    ENDIF.
  ENDIF.
*** Reported by
  CLEAR: wa_name.
  SELECT SINGLE b~name_text
         INTO wa_name
         FROM usr21 AS a
              INNER JOIN adrp AS b
              ON a~persnumber = b~persnumber
         WHERE a~bname = sy-uname.

ENDFORM.                    " READ_HEADER_INFO
*&---------------------------------------------------------------------*
*&      Form  read_breakdown_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_breakdown_detail.
  CLEAR: qmur, qmma, viqmfe.
**** Read Object Part Code , Damage & Notification Item Short Text
  SELECT SINGLE  posnr oteil  fecod fetxt
                 INTO (zspm_comp-posnr, zspm_comp-oteil,
                       zspm_comp-fecod, zspm_comp-fetxt)
                 FROM viqmfe
                 WHERE qmnum = zspm_comp-qmnum.
  IF sy-subrc EQ 0.
**** Read Object Part Code text
    IF NOT zspm_comp-oteil IS INITIAL.
      CALL FUNCTION 'QPK1_CODE_TEXT'
        EXPORTING
          i_katalogart = 'B'
          i_codegruppe = 'PM01'
          i_code       = zspm_comp-oteil
        IMPORTING
          e_text       = zspm_comp-txtcdot.

    ENDIF.
**** Read Damage Code text
    IF NOT zspm_comp-fecod IS INITIAL.
      CALL FUNCTION 'QPK1_CODE_TEXT'
        EXPORTING
          i_katalogart = 'C'
          i_codegruppe = 'PM01'
          i_code       = zspm_comp-fecod
        IMPORTING
          e_text       = zspm_comp-txtcdfe.
    ENDIF.
  ENDIF.
**** Read Cause Code & Short Text for Cause Code
  SELECT SINGLE fenum urnum urcod urtxt
         INTO  (zspm_comp-fenum,
                zspm_comp-urnum,
                zspm_comp-urcod,
                zspm_comp-urtxt)
         FROM   qmur
         WHERE  qmnum = zspm_comp-qmnum.
  IF sy-subrc EQ 0.
**** Read Cause Code text
    CALL FUNCTION 'QPK1_CODE_TEXT'
      EXPORTING
        i_katalogart = '5'
        i_codegruppe = 'PM01'
        i_code       = zspm_comp-urcod
      IMPORTING
        e_text       = zspm_comp-txtcdur.
  ENDIF.
***** Activity Code & Short Text for Activity Code
  SELECT SINGLE manum mncod matxt
         INTO  (zspm_comp-manum,
                zspm_comp-mncod,
                zspm_comp-matxt)
         FROM   qmma
         WHERE  qmnum = zspm_comp-qmnum.
  IF sy-subrc EQ 0.
***** Activity Code text
    CALL FUNCTION 'QPK1_CODE_TEXT'
      EXPORTING
        i_katalogart = 'A'
        i_codegruppe = 'PM01'
        i_code       = zspm_comp-mncod
      IMPORTING
        e_text       = zspm_comp-txtcdma.
  ENDIF.

ENDFORM.                    " read_breakdown_detail
*&---------------------------------------------------------------------*
*&      Form  READ_BREAKDOWN_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_breakdown_long_text.
**** Read Damage Long text
  CONCATENATE zspm_comp-qmnum
              zspm_comp-fenum
              INTO wa_tdname01.

  headltx-tdid    = 'LTXT'.
  headltx-tdspras = 'E'.
  headltx-tdname  = wa_tdname01.
  headltx-tdobject = 'QMFE'.
  headltx-tdlinesize = 072.

  PERFORM read_long_text TABLES it_ltxttab01
                         USING  headltx.

  IF it_ltxttab01 IS INITIAL.
    PERFORM read_damage_short_text  TABLES it_ltxttab01
                                    USING  zspm_comp-qmnum
                                           zspm_comp-fenum.
  ENDIF.

**** Read Cause Long text
  CONCATENATE zspm_comp-qmnum
              zspm_comp-fenum
              zspm_comp-urnum
              INTO wa_tdname02.

  headltx-tdid    = 'LTXT'.
  headltx-tdspras = 'E'.
  headltx-tdname  = wa_tdname02.
  headltx-tdobject = 'QMUR'.
  headltx-tdlinesize = 072.

  PERFORM read_long_text TABLES it_ltxttab02
                        USING  headltx.

  IF it_ltxttab02 IS INITIAL.
    PERFORM read_cause_short_text   TABLES it_ltxttab02
                                    USING  zspm_comp-qmnum
                                           zspm_comp-fenum.
  ENDIF.

***** Activity Long text
  CONCATENATE zspm_comp-qmnum
              zspm_comp-manum
              INTO wa_tdname03.

  headltx-tdid    = 'LTXT'.
  headltx-tdspras = 'E'.
  headltx-tdname  = wa_tdname03.
  headltx-tdobject = 'QMMA'.
  headltx-tdlinesize = 072.

  PERFORM read_long_text TABLES it_ltxttab03
                        USING  headltx.

  IF it_ltxttab03 IS INITIAL.
    PERFORM read_activity_short_text  TABLES it_ltxttab03
                                      USING  zspm_comp-qmnum
                                             zspm_comp-fenum.
  ENDIF.
ENDFORM.                    " READ_BREAKDOWN_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_COUNTERMEASURE_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_countermeasure_detail.

  SELECT  a~qmnum b~aufnr b~gltrp
          INTO CORRESPONDING FIELDS OF TABLE it_counter
          FROM  viqmel AS a
                INNER JOIN afko AS b
                ON a~aufnr = b~aufnr
          WHERE a~qwrnum  = zspm_comp-qmnum.


*  SELECT  VORNR LTXA1
*      INTO CORRESPONDING FIELDS OF TABLE IT_OPERATION
*      FROM CAUFV AS A
*           INNER JOIN AFVC AS B
*           ON  A~AUFPL = B~AUFPL
*      WHERE A~AUFNR = IT_COUNTER-AUFNR.

ENDFORM.                    " READ_COUNTERMEASURE_DETAIL
*&---------------------------------------------------------------------*
*&      Form  READ_COUNTERMEASURE_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_countermeasure_long_text.
  DATA: wa_tab_name(20),
        wa_tabix(2) TYPE n.

  DATA: wa_aufpl LIKE afvc-aufpl,
        wa_aplzl LIKE afvc-aplzl.

  LOOP AT it_counter.
    IF sy-tabix > 3. EXIT. ENDIF.
    wa_tabix = sy-tabix.


    SELECT SINGLE  a~aufpl b~aplzl
           INTO (wa_aufpl, wa_aplzl)
           FROM caufv  AS a
                 INNER JOIN afvc AS b
                 ON a~aufpl = b~aufpl
           WHERE  a~aufnr =  it_counter-aufnr.

    CALL FUNCTION 'CO_ZK_TEXTKEY_AFVG'
      EXPORTING
        aplzl = wa_aplzl
        aufpl = wa_aufpl
      IMPORTING
        ltsch = wa_tdname04.


    headltx-tdid       = 'AVOT'.
    headltx-tdspras    = 'E'.
    headltx-tdname     = wa_tdname04.
    headltx-tdobject   = 'AUFK'.
    headltx-mandt      = sy-mandt.
    headltx-tdlinesize = 072.
*    HEADLTX-TDMACODE1  = 'IW22SAPLIQS0'.
    headltx-tdform     = 'SYSTEM'.

*    HEADLTX-TDID       = 'LTXT'.
*    HEADLTX-TDSPRAS    = 'E'.
*    HEADLTX-TDNAME     = WA_TDNAME04.
*    HEADLTX-TDOBJECT   = 'QMSM'.
*    HEADLTX-TDLINESIZE = 072.

*    CONCATENATE 'IT_LTXTTAB04_' WA_TABIX INTO WA_TAB_NAME.
*    ASSIGN (WA_TAB_NAME) TO <TXTTAB>.
*    PERFORM READ_LONG_TEXT  TABLES <TXTTAB>
*                            USING  HEADLTX.

    CASE sy-tabix.
      WHEN 01.
        PERFORM read_long_text  TABLES it_ltxttab04_01
                                USING  headltx.

        IF it_ltxttab04_01 IS INITIAL.
          PERFORM read_short_text  TABLES it_ltxttab04_01
                                   USING  wa_aufpl.
        ENDIF.
      WHEN 02.
        PERFORM read_long_text  TABLES it_ltxttab04_02
                                USING  headltx.
        IF it_ltxttab04_02 IS INITIAL.
          PERFORM read_short_text  TABLES it_ltxttab04_02
                                   USING  wa_aufpl.
        ENDIF.
      WHEN 03.
        PERFORM read_long_text  TABLES it_ltxttab04_03
                                USING  headltx.
        IF it_ltxttab04_03 IS INITIAL.
          PERFORM read_short_text  TABLES it_ltxttab04_03
                                   USING  wa_aufpl.
        ENDIF.
    ENDCASE.

  ENDLOOP.
ENDFORM.                    " READ_COUNTERMEASURE_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  FILL_EMPTY_CELL
*&---------------------------------------------------------------------*
*  Append space fields to internal table for filling empty cells
*  Excel display empty cell "N/A"
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_empty_cell.
  DATA: wa_lines TYPE i.

**** for Damage Long text (max 2 lines)
  DO 2 TIMES .
    DESCRIBE TABLE it_ltxttab01 LINES wa_lines.
    IF wa_lines < 2.
      CLEAR it_ltxttab01.
      APPEND it_ltxttab01.
    ELSE.
      CLEAR: wa_lines.
      EXIT.
    ENDIF.
  ENDDO.

****for Cause Long text (max 2 lines)
  DO 2 TIMES .
    DESCRIBE TABLE it_ltxttab02 LINES wa_lines.
    IF wa_lines < 2.
      CLEAR it_ltxttab02.
      APPEND it_ltxttab02.
    ELSE.
      CLEAR: wa_lines.
      EXIT.
    ENDIF.
  ENDDO.

****for Activity Long text (max 4 lines)
  DO 4 TIMES .
    DESCRIBE TABLE it_ltxttab03 LINES wa_lines.
    IF wa_lines < 4.
      CLEAR it_ltxttab03.
      APPEND it_ltxttab03.
    ELSE.
      CLEAR: wa_lines.
      EXIT.
    ENDIF.
  ENDDO.

****for Countermeasure Long text 1 (max 3 lines)
  DO 4 TIMES .
    DESCRIBE TABLE it_ltxttab04_01 LINES wa_lines.
    IF wa_lines < 3.
      CLEAR it_ltxttab04_01.
      APPEND it_ltxttab04_01.
    ELSE.
      CLEAR: wa_lines.
      EXIT.
    ENDIF.
  ENDDO.

****for Countermeasure Long text 2 (max 3 lines)
  DO 4 TIMES .
    DESCRIBE TABLE it_ltxttab04_02 LINES wa_lines.
    IF wa_lines < 3.
      CLEAR it_ltxttab04_02.
      APPEND it_ltxttab04_02.
    ELSE.
      CLEAR: wa_lines.
      EXIT.
    ENDIF.
  ENDDO.

****for Countermeasure Long text 3 (max 3 lines)
  DO 4 TIMES .
    DESCRIBE TABLE it_ltxttab04_03 LINES wa_lines.
    IF wa_lines < 3.
      CLEAR it_ltxttab04_03.
      APPEND it_ltxttab04_03.
    ELSE.
      CLEAR: wa_lines.
      EXIT.
    ENDIF.
  ENDDO.

  DO 3 TIMES .
    DESCRIBE TABLE it_counter LINES wa_lines.
    IF wa_lines < 3.
      CLEAR it_counter.
      APPEND it_counter.
    ELSE.
      CLEAR: wa_lines.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " FILL_EMPTY_CELL
*&---------------------------------------------------------------------*
*&      Form  READ_SHORT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXTTAB04_01  text
*      -->P_WA_AUFPL  text
*      -->P_ENDIF  text
*----------------------------------------------------------------------*
FORM read_short_text TABLES   pt_ltxttab STRUCTURE tline
                     USING    p_aufpl.

  SELECT SINGLE  ltxa1 INTO pt_ltxttab-tdline
                 FROM afvc WHERE aufpl = p_aufpl.
  IF sy-subrc EQ 0.
    APPEND pt_ltxttab.
  ENDIF.
ENDFORM.                    " READ_SHORT_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_DAMAGE_SHORT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXTTAB01  text
*      -->P_WA_AUFPL  text
*----------------------------------------------------------------------*
FORM read_damage_short_text TABLES  pt_ltxttab STRUCTURE tline
                            USING    p_qmnum
                                     p_fenum.

  SELECT SINGLE  fetxt  INTO pt_ltxttab-tdline
                 FROM  viqmfe
                 WHERE qmnum = p_qmnum
                 AND   fenum = p_fenum.
  IF sy-subrc EQ 0.
    APPEND pt_ltxttab.
  ENDIF.
ENDFORM.                    " READ_DAMAGE_SHORT_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_CAUSE_SHORT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXTTAB02  text
*      -->P_ZSPM_COMP_QMNUM  text
*      -->P_ZSPM_COMP_FENUM  text
*----------------------------------------------------------------------*
FORM read_cause_short_text TABLES   pt_ltxttab STRUCTURE tline
                            USING    p_qmnum
                                     p_fenum.

  SELECT SINGLE  urtxt  INTO pt_ltxttab-tdline
                 FROM  qmur
                 WHERE qmnum = p_qmnum
                 AND   fenum = p_fenum.
  IF sy-subrc EQ 0.
    APPEND pt_ltxttab.
  ENDIF.
ENDFORM.                    " READ_CAUSE_SHORT_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_ACTIVITY_SHORT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXTTAB03  text
*      -->P_ZSPM_COMP_QMNUM  text
*      -->P_ZSPM_COMP_FENUM  text
*----------------------------------------------------------------------*
FORM read_activity_short_text TABLES   pt_ltxttab STRUCTURE tline
                              USING    p_qmnum
                                       p_fenum.

  SELECT SINGLE  matxt  INTO pt_ltxttab-tdline
                 FROM  qmma
                 WHERE qmnum = p_qmnum
                 AND   fenum = p_fenum.
  IF sy-subrc EQ 0.
    APPEND pt_ltxttab.
  ENDIF.
ENDFORM.                    " READ_ACTIVITY_SHORT_TEXT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TABLE_DATA
*&---------------------------------------------------------------------*
FORM display_table_data .
  DATA: locint_fields TYPE TABLE OF rfc_fields.
  DATA: lv_lines     TYPE i.
  DATA: l_range TYPE char255.

  DATA : BEGIN OF it_head OCCURS 0,
          value(80),
         END OF it_head.

  DATA : BEGIN OF it_line OCCURS 0,
         tdline TYPE tdline,
         END OF it_line.

  CLEAR : it_head[], it_head, it_line[], it_line.

**--Reporter
  it_head-value = zspm_comp-qmnam.
  APPEND it_head.

  l_range = 'REPORTER'.
  PERFORM create_range USING 4
                             21
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

*--Order No
  it_head-value = zspm_comp-aufnr.
  APPEND it_head.

  l_range = 'ORDER_NO'.
  PERFORM create_range USING 4
                             28
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--Shop
*  it_head-value = wa_shop_line.
  it_head-value = wa_fing.
  APPEND it_head.

  l_range = 'SHOP'.
  PERFORM create_range USING 5
                             4
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--Line
  it_head-value = wa_ktext.
  APPEND it_head.

  l_range = 'LINE'.
  PERFORM create_range USING 5
                             12
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--Equipment
  it_head-value = itob-shtxt.
  APPEND it_head.

  l_range = 'EQUIPMENT'.
  PERFORM create_range USING 5
                             20
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--Date
  it_head-value = wa_period.
  APPEND it_head.

  l_range = 'DATE'.
  PERFORM create_range USING 5
                             28
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--Subject
  it_head-value = zspm_comp-ktext.
  APPEND it_head.

  l_range = 'SUBJECT'.
  PERFORM create_range USING 6
                             4
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--Time
  it_head-value = wa_time.
  APPEND it_head.

  l_range = 'TIME'.
  PERFORM create_range USING 6
                             20
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--Downtime
  it_head-value = wa_duration.
  APPEND it_head.

  l_range = 'DOWNTIME'.
  PERFORM create_range USING 6
                             28
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--damage text
  it_head-value = zspm_comp-txtcdfe.
  APPEND  it_head.
  l_range = 'DAMAGE_TEXT'.

  PERFORM create_range USING 8
                             1
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--damage L1 ~ L2
  LOOP AT it_ltxttab01.
    it_line-tdline = it_ltxttab01-tdline.
    APPEND  it_line.
  ENDLOOP.

  DESCRIBE TABLE it_line LINES lv_lines.
  l_range = 'DAMAGE_LINE'.

  PERFORM create_range USING 9
                             1
                             lv_lines
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_line
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_line[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_line[], it_line.

**--activity text
  it_head-value = zspm_comp-txtcdma.
  APPEND  it_head.
  l_range = 'ACTIVITY_TEXT'.

  PERFORM create_range USING 8
                             17
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--activity L1 ~ L7
  LOOP AT it_ltxttab03.
    it_line-tdline = it_ltxttab03-tdline.
    APPEND  it_line.
  ENDLOOP.

  DESCRIBE TABLE it_line LINES lv_lines.
  l_range = 'ACTIVITY_LINE'.

  PERFORM create_range USING 9
                             17
                             lv_lines
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_line
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_line[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_line[], it_line.

**--cause text
  it_head-value = zspm_comp-txtcdur.
  APPEND  it_head.
  l_range = 'CAUSE_TEXT'.

  PERFORM create_range USING 17
                             3
                             1
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_head
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_head[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_head[], it_head.

**--cause L1 ~ L2
  LOOP AT it_ltxttab02.
    it_line-tdline = it_ltxttab02-tdline.
    APPEND  it_line.
  ENDLOOP.

  DESCRIBE TABLE it_line LINES lv_lines.
  l_range = 'CAUSE_LINE'.

  PERFORM create_range USING 18
                             3
                             lv_lines
                             1
                             l_range.

* Get field attributes of the table to be displayed
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data             = it_line
      fields           = locint_fields
    EXCEPTIONS
      dp_invalid_table = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Insert the table entries into Excel
  CALL METHOD r_excel->insert_one_table
    EXPORTING
      fields_table = locint_fields[]  "Defn of fields
      data_table   = it_line[]     "Data
      rangename    = l_range         "Range Name
    IMPORTING
      error        = r_error
      retcode      = retcode.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
  CLEAR : it_line[], it_line.
ENDFORM.                    " DISPLAY_TABLE_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_range  USING    l_top    TYPE i
                            l_left   TYPE i
                            l_row    TYPE i
                            l_column TYPE i
                            l_range  TYPE char255.

* Select area for entries to be displayed
  CALL METHOD r_excel->set_selection
    EXPORTING
      top     = l_top
      left    = l_left
      rows    = l_row
      columns = l_column.

* Define Range
  CALL METHOD r_excel->insert_range
    EXPORTING
      name    = l_range
      rows    = l_row
      columns = l_column
    IMPORTING
      error   = r_error.

  IF r_error->has_failed = abap_true.
    CALL METHOD r_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.
ENDFORM.                    " CREATE_RANGE
