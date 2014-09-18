*----------------------------------------------------------------------
* Program ID        : ZFIR_CUSTOMER_ACCT_STATEMENT
* Title             : [FI] - Customer Account Statement
* Created on        : 01/28/2011
* Created by        : Valerian Utama
* Specifications By : Calvin Kong
* Description       : [FI] - Customer Account Statement
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer  Request    Description
* 01/28/2011 VALERIAN   UD1K950777 Initial Coding
* 02/28/2011 VALERIAN   UD1K950969 Get Amount and Quantity from billing
*                                  document if available. (in local
*                                  currency).
*                                  Exclude document type "RR".
*                                  Add logic to download statement to
*                                  Excel format.
* 03/10/2011 VALERIAN   UD1K951085 Adjust Excel Layout.
* 03/11/2011 VALERIAN   UD1K951091 Fix the layout bug.
* 03/14/2011 VALERIAN   UD1K951099 Fix duplicated records bug.
* 04/13/2011 VALERIAN   UD1K951099 Fix another duplicated records bug.
* 06/19/2013  T00303    UD1K957415  U1: Apply Archiving
* 06/17/2014 furong    Adjust Excel/PDF/Smartform Layout.
*&--------------------------------------------------------------------&*

REPORT zfir_cust_acct_stmnt MESSAGE-ID zmfi.

INCLUDE ole2incl.
INCLUDE excel__c.

CONSTANTS: c_blart TYPE blart VALUE 'RR'.

TYPES: BEGIN OF szadr_printform_table_line,
         line_type TYPE ad_line_tp,
         address_line LIKE adrs-line0,
       END OF szadr_printform_table_line.

DATA: g_fm_name TYPE rs38l_fnam.

DATA: p_control_param TYPE ssfctrlop,
      p_output_option TYPE ssfcompop.

DATA: BEGIN OF gt_bsid OCCURS 0.
        INCLUDE STRUCTURE zfi_cust_acct_stmnt.
DATA:   shkzg TYPE shkzg,
        vbeln TYPE vbeln_vf,
        rebzt TYPE rebzt,
        wrbtr TYPE wrbtr,
      END OF gt_bsid.

DATA: BEGIN OF gt_bill OCCURS 0,
        vbeln    TYPE vbeln_vf,
        vgbel(3) TYPE c,
        fkimg    TYPE fkimg,
        netwr    TYPE netwr_fp,
      END OF gt_bill.

DATA: gt_bill_tot LIKE gt_bill OCCURS 0 WITH HEADER LINE.

DATA: gt_item LIKE zfi_cust_acct_stmnt OCCURS 0 WITH HEADER LINE.

DATA: wa_kna1 LIKE kna1,
      wa_knb1 LIKE knb1,
      wa_t001 LIKE t001,
      g_ddate TYPE sy-datum,
      g_qty(18) TYPE n,
      g_ovd30 TYPE dmbtr,
      g_ovd60 TYPE dmbtr,
      g_ovd90 TYPE dmbtr.

DATA: BEGIN OF gt_freq OCCURS 0,
       fdate(2) TYPE n,
       tdate(2) TYPE n,
      END OF gt_freq.

DATA: BEGIN OF gt_excel OCCURS 0,
        bdate TYPE dzfbdt,
        zfbdt TYPE dzfbdt,
        xblnr TYPE xblnr,
        belnr TYPE belnr_d,
        bldat TYPE bldat,
        zuonr TYPE i,
        dmbtr TYPE dmbtr,
        sgtxt TYPE sgtxt,
        curnt TYPE dmbtr,
        ovd30 TYPE dmbtr,
        ovd60 TYPE dmbtr,
        ovd90 TYPE dmbtr,
        ovdxx TYPE dmbtr,
        total TYPE dmbtr,
** Furong on 06/19/14 (
        waers TYPE waers,
** )
      END OF gt_excel.

DATA: excel     TYPE ole2_object,      "Excel object
      workbooks TYPE ole2_object,      "list of workbooks
      sheets    TYPE ole2_object,      "list of worksheets
      sheet     TYPE ole2_object,      "worksheets
      cells     TYPE ole2_object,      "cells
      font      TYPE ole2_object,      "font
      columns   TYPE ole2_object.      "column

RANGES: r_blart FOR bsid-blart.

*- U1 Start
DATA: gt_vbrp_a TYPE TABLE OF vbrp WITH HEADER LINE.
*- U1 End

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-t01.
PARAMETERS:
  p_bukrs TYPE bsid-bukrs OBLIGATORY MEMORY ID buk,
  p_kunnr TYPE bsid-kunnr OBLIGATORY MEMORY ID kun,
  p_augdt TYPE bsid-augdt OBLIGATORY.

SELECTION-SCREEN SKIP.
PARAMETERS:
  p_paydt TYPE bsid-augdt OBLIGATORY.

SELECTION-SCREEN SKIP.
PARAMETERS:
  p_excld AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-t02.
PARAMETERS:
  p_prevw RADIOBUTTON GROUP rad1 DEFAULT 'X',
  p_print RADIOBUTTON GROUP rad1,
  p_pdf   RADIOBUTTON GROUP rad1,
  p_excel RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK block2.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

START-OF-SELECTION.
  PERFORM set_intervals.
  PERFORM get_data.
  PERFORM process_data.

  CASE 'X'.
    WHEN p_excel.
      PERFORM download_excel.
    WHEN OTHERS.
      PERFORM call_smartform.
  ENDCASE.

  MESSAGE s153(fv).

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  set_freq
*&---------------------------------------------------------------------*
*       Set baseline date
*----------------------------------------------------------------------*
*      -->P_FDATE  From date
*      -->P_TDATE  To date
*----------------------------------------------------------------------*
FORM set_freq USING    p_fdate TYPE n
                       p_tdate TYPE n.

  gt_freq-fdate = p_fdate.
  gt_freq-tdate = p_tdate.
  APPEND gt_freq.
ENDFORM.                    " set_freq
*&---------------------------------------------------------------------*
*&      Form  set_intervals
*&---------------------------------------------------------------------*
*       Set intervals for baseline date and due date
*----------------------------------------------------------------------*
FORM set_intervals.
* Show progress indicator.
  PERFORM show_progress2 USING 'Processing Data, Please Wait...' 0.

  g_ddate = p_augdt - 1.
  g_ovd30 = p_augdt - 30.
  g_ovd60 = p_augdt - 60.
  g_ovd90 = p_augdt - 90.

  CASE p_kunnr.
    WHEN 'C001'.
      PERFORM set_freq USING: '01' '10',
                              '11' '20',
                              '21' '31'.
    WHEN 'A875'.
      PERFORM set_freq USING: '01' '15',
                              '16' '31'.
    WHEN 'AG5I'.
      PERFORM set_freq USING: '01' '31'.

    WHEN OTHERS.
      PERFORM set_freq USING: '01' '31'.
  ENDCASE.

  IF NOT p_excld IS INITIAL.
* Exclude document type
    r_blart-sign   = 'I'.
    r_blart-option = 'NE'.
    r_blart-low    = c_blart.
    APPEND r_blart.
  ENDIF.

ENDFORM.                    " set_intervals
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Get Data
*----------------------------------------------------------------------*
FORM get_data.
* Get Company Code
  SELECT SINGLE * INTO wa_t001
    FROM t001
   WHERE bukrs = p_bukrs.

* Get Customer Master
  SELECT SINGLE * INTO wa_kna1
    FROM kna1
   WHERE kunnr = p_kunnr.

  SELECT SINGLE * INTO wa_knb1
    FROM knb1
   WHERE kunnr = p_kunnr
     AND bukrs = p_bukrs.

* Get Transaction Data
  SELECT *
     INTO CORRESPONDING FIELDS OF TABLE gt_bsid
     FROM bsid
    WHERE bukrs =  p_bukrs
      AND kunnr =  p_kunnr
      AND budat <= p_augdt
      AND blart IN r_blart.

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE gt_bsid
      FROM bsad
     WHERE bukrs =  p_bukrs
       AND kunnr =  p_kunnr
       AND augdt >  p_augdt
       AND budat <= p_augdt
       AND blart IN r_blart.

  IF gt_bsid[] IS INITIAL.
    MESSAGE i057(ei).
    STOP.
  ENDIF.

* Get Billing Document
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_bill
    FROM vbrp
    FOR ALL ENTRIES IN gt_bsid
    WHERE vbeln = gt_bsid-vbeln.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_vbrp.
  ENDIF.
*- U1 End

* Set local currency
*  gt_bsid-lwaer = wa_t001-waers.
* read table gt_bsid index 1.
*  gt_bsid-lwaer = gt_bsid-waers.
*  MODIFY gt_bsid TRANSPORTING lwaer WHERE belnr <> space.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       Process data
*----------------------------------------------------------------------*
FORM process_data.
  DATA: gt_bsid_all LIKE gt_bsid OCCURS 0,
        wa_bsid     LIKE gt_bsid.

  SORT gt_bill BY vbeln vgbel.

  LOOP AT gt_bill.
    COLLECT gt_bill INTO gt_bill_tot.
  ENDLOOP.

  FREE gt_bill.

  SORT gt_bsid BY zfbdt.

  LOOP AT gt_bsid.
* Adjust sign
    IF gt_bsid-shkzg = 'H'.
      gt_bsid-dmbtr = gt_bsid-dmbtr * -1.
    ENDIF.

* Convert quantity format
    g_qty = gt_bsid-zuonr.
    gt_bsid-zuonr = g_qty.

* Check if it has original document
    IF gt_bsid-rebzt IS INITIAL.

* Get billing information if any
      LOOP AT gt_bill_tot WHERE vbeln = gt_bsid-vbeln.
        wa_bsid = gt_bsid.

        gt_bsid-zuonr = gt_bill_tot-fkimg.

        IF gt_bsid-dmbtr = gt_bsid-wrbtr.
          gt_bsid-dmbtr = gt_bill_tot-netwr.

          IF gt_bsid-shkzg = 'H'.
            gt_bsid-dmbtr = gt_bsid-dmbtr * -1.
          ENDIF.

        ELSE.
** By Furong on 06/11/14  for currency issue (
          gt_bsid-dmbtr = gt_bill_tot-netwr.
*          gt_bsid-dmbtr = gt_bsid-dmbtr * gt_bill_tot-netwr
*                                        / gt_bsid-wrbtr.
** By Furong on 07/07/14 (
           IF gt_bsid-shkzg = 'H'.
            gt_bsid-dmbtr = gt_bsid-dmbtr * -1.
          ENDIF.
** )
        ENDIF.

        IF gt_bsid-sgtxt IS INITIAL.
          CASE gt_bill_tot-vgbel.
            WHEN 'INF' OR 'C2F'.
              gt_bsid-sgtxt = 'SONATA'.
            WHEN 'TCF'.
              gt_bsid-sgtxt = 'ELANTRA'.
            WHEN OTHERS.
              gt_bsid-sgtxt = 'OTHERS'.
          ENDCASE.
        ENDIF.

        APPEND gt_bsid TO gt_bsid_all.

        gt_bsid = wa_bsid.
      ENDLOOP.

* Billing information is not found
      IF sy-subrc <> 0.
        APPEND gt_bsid TO gt_bsid_all.
      ENDIF.

    ELSE.
      APPEND gt_bsid TO gt_bsid_all.
    ENDIF.

  ENDLOOP.

  gt_bsid[] = gt_bsid_all[].
  FREE: gt_bsid_all, gt_bill_tot.

  LOOP AT gt_bsid.
* Get Due Date buckets
    IF gt_bsid-zfbdt >= p_augdt.
      gt_bsid-curnt = gt_bsid-dmbtr.
    ELSEIF gt_bsid-zfbdt < p_augdt AND gt_bsid-zfbdt >= g_ovd30.
      gt_bsid-ovd30 = gt_bsid-dmbtr.
    ELSEIF gt_bsid-zfbdt < g_ovd30 AND gt_bsid-zfbdt >= g_ovd60.
      gt_bsid-ovd60 = gt_bsid-dmbtr.
    ELSEIF gt_bsid-zfbdt < g_ovd60 AND gt_bsid-zfbdt >= g_ovd90.
      gt_bsid-ovd90 = gt_bsid-dmbtr.
    ELSEIF gt_bsid-zfbdt < g_ovd90.
      gt_bsid-ovdxx = gt_bsid-dmbtr.
    ENDIF.

* Get Base Line Date
    IF gt_bsid-zfbdt <= g_ddate.
      gt_bsid-bdate = g_ddate.
    ELSE.
      LOOP AT gt_freq WHERE fdate <= gt_bsid-zfbdt+6(2)
                        AND tdate >= gt_bsid-zfbdt+6(2).
        IF gt_freq-tdate = '31'.
          CALL FUNCTION 'LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = gt_bsid-zfbdt
            IMPORTING
              last_day_of_month = gt_bsid-bdate
            EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ELSE.
          gt_bsid-bdate = gt_bsid-zfbdt.
          gt_bsid-bdate+6(2) = gt_freq-tdate.
        ENDIF.

      ENDLOOP.
    ENDIF.

    MODIFY gt_bsid.
  ENDLOOP.

  SORT gt_bsid BY zfbdt bdate bldat belnr dmbtr DESCENDING.

  LOOP AT gt_bsid.
    MOVE-CORRESPONDING gt_bsid TO gt_item.
    APPEND gt_item.
  ENDLOOP.

  FREE gt_bsid.
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  call_smartform
*&---------------------------------------------------------------------*
*       Call smartform to present the statement
*----------------------------------------------------------------------*
FORM call_smartform.
  DATA: t_otf TYPE itcoo OCCURS 0 WITH HEADER LINE,
        t_otf_from_fm TYPE ssfcrescl,
        t_pdf_tab LIKE tline OCCURS 0 WITH HEADER LINE.

  DATA: w_bin_filesize TYPE i,
        w_file_name TYPE string,
        w_file_path TYPE string,
        w_full_path TYPE string,
        w_dflt_name TYPE string,
        w_trnc_path TYPE string,
        w_answer    TYPE i.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFI_CUST_ACCT_STMNT'
    IMPORTING
      fm_name            = g_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK sy-subrc = 0.

  p_control_param-no_dialog = 'X'.
  p_control_param-preview   = p_prevw.
  p_control_param-getotf    = p_pdf.

  CALL FUNCTION g_fm_name
    EXPORTING
      control_parameters = p_control_param
      output_options     = p_output_option
      t001               = wa_t001
      kna1               = wa_kna1
      knb1               = wa_knb1
      augdt              = p_augdt
      paydt              = p_paydt
    IMPORTING
      job_output_info    = t_otf_from_fm
    TABLES
      bsid               = gt_item
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSEIF NOT t_otf_from_fm-spoolids IS INITIAL.
    MESSAGE s000 WITH 'Customer Account Statement'
                      p_kunnr
                      p_bukrs
                      'Succesfully Sent to Printer'.
  ENDIF.

  CHECK sy-subrc = 0.

  IF NOT p_pdf IS INITIAL.
    t_otf[] = t_otf_from_fm-otfdata[].

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
        max_linewidth         = 132
      IMPORTING
        bin_filesize          = w_bin_filesize
      TABLES
        otf                   = t_otf
        lines                 = t_pdf_tab
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        OTHERS                = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CHECK sy-subrc = 0.

    CONCATENATE 'CUSTOMER' p_kunnr p_bukrs p_augdt
           INTO w_dflt_name SEPARATED BY '_'.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title      = 'Select File Name'
        default_extension = 'pdf'
        default_file_name = w_dflt_name
        file_filter       = 'PDF (*.pdf)|*.pdf| All (*.*)|*.*'
      CHANGING
        filename          = w_file_name
        path              = w_file_path
        fullpath          = w_full_path
        user_action       = w_answer
      EXCEPTIONS
        cntl_error        = 1
        error_no_gui      = 2
        OTHERS            = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSEIF w_answer NE 0.
      MESSAGE s549(fibl).
    ENDIF.

    CHECK NOT w_full_path IS INITIAL.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = w_bin_filesize
        filename                = w_full_path
        filetype                = 'BIN'
      TABLES
        data_tab                = t_pdf_tab
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

    IF sy-subrc <> 0.
      IF sy-subrc = 15.
        MESSAGE s011 WITH 'Access Denied'.
      ELSEIF NOT sy-msgid IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE s011 WITH 'Error when creating the file'.
      ENDIF.
    ELSE.
      PERFORM trunc_path USING w_full_path CHANGING w_trnc_path.
      MESSAGE s000 WITH 'File is written to:' w_trnc_path ' ' ' '.
    ENDIF.
  ENDIF.

ENDFORM.                    " call_smartform
*&---------------------------------------------------------------------*
*&      Form  trunc_path
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FULL_PATH  Full Path
*      <--P_TRNC_PATH  Truncated Path
*----------------------------------------------------------------------*
FORM trunc_path USING    p_full_path
                CHANGING p_trnc_path.
  DATA: BEGIN OF t_path OCCURS 0,
          dir(1024) TYPE c,
        END OF t_path.

  DATA: l_count TYPE i.

  p_trnc_path = p_full_path.

  SPLIT p_full_path AT '\' INTO TABLE t_path.
  DELETE t_path WHERE dir = space.
  DESCRIBE TABLE t_path LINES l_count.
  IF l_count <= 2.
    EXIT.
  ENDIF.

  READ TABLE t_path INDEX 1.
  p_trnc_path = t_path-dir.
  IF p_trnc_path NA ':'.
    CONCATENATE '\\' p_trnc_path INTO p_trnc_path.
  ENDIF.

  IF l_count GT 3.
    CONCATENATE p_trnc_path '\' INTO p_trnc_path.
  ENDIF.

  IF l_count GT 2.
    l_count = l_count - 2.
    DELETE t_path FROM 1 TO l_count.
  ENDIF.

  LOOP AT t_path.
    CONCATENATE p_trnc_path '\' t_path-dir INTO p_trnc_path.
  ENDLOOP.
ENDFORM.                    " trunc_path
*---------------------------------------------------------------------*
*       FORM FILL_CELL                                                *
*---------------------------------------------------------------------*
*       sets cell at coordinates i,j to value val boldtype bold       *
*---------------------------------------------------------------------*
FORM fill_cell USING i j bold val.
  CALL METHOD OF
      sheet
      'Cells' = cells
    EXPORTING
      #1      = i
      #2      = j.
  SET PROPERTY OF cells 'Value' = val.
  GET PROPERTY OF cells 'Font'  = font.
  SET PROPERTY OF font 'Bold'   = bold.

  SET PROPERTY OF cells 'HorizontalAlignment' = xlleft.
ENDFORM.                    "fill_cell
*---------------------------------------------------------------------*
*       FORM FILL_CELL_FNT                                            *
*---------------------------------------------------------------------*
*       sets cell at coordinates i,j to value val boldtype bold       *
*---------------------------------------------------------------------*
FORM font_size USING i j size.
  CALL METHOD OF
      sheet
      'Cells' = cells
    EXPORTING
      #1      = i
      #2      = j.
  GET PROPERTY OF cells 'Font' = font.
  SET PROPERTY OF font 'Size' = size.
ENDFORM.                    "font_size
*---------------------------------------------------------------------*
*       FORM FILL_CELL_NUM                                            *
*---------------------------------------------------------------------*
*       sets cell at coordinates i,j to value val boldtype bold       *
*---------------------------------------------------------------------*
FORM fill_cell_num USING i j bold val digit.
  CALL METHOD OF
      sheet
      'Cells' = cells
    EXPORTING
      #1      = i
      #2      = j.
  SET PROPERTY OF cells 'Value' = val.
  GET PROPERTY OF cells 'Font'  = font.
  SET PROPERTY OF font 'Bold'   = bold.

  IF digit = 1.
    SET PROPERTY OF cells 'NumberFormat' = '#,###.0'.
  ELSEIF digit = 2.
    SET PROPERTY OF cells 'NumberFormat' = '#,###.00'.
  ELSE.
    SET PROPERTY OF cells 'NumberFormat' = '#,###'.
  ENDIF.
ENDFORM.                    "fill_cell_num
*---------------------------------------------------------------------*
*       FORM FILL_CELL_ALG                                            *
*---------------------------------------------------------------------*
*       sets cell at coordinates i,j to value val boldtype bold       *
*---------------------------------------------------------------------*
FORM fill_cell_alg USING i j bold val align.
  CALL METHOD OF
      sheet
      'Cells' = cells
    EXPORTING
      #1      = i
      #2      = j.
  SET PROPERTY OF cells 'Value' = val.
  GET PROPERTY OF cells 'Font'  = font.
  SET PROPERTY OF font 'Bold'   = bold.

  IF align = 'L'.
    SET PROPERTY OF cells 'HorizontalAlignment' = xlleft.
  ELSEIF align = 'R'.
    SET PROPERTY OF cells 'HorizontalAlignment' = xlright.
  ELSEIF align = 'C'.
    SET PROPERTY OF cells 'HorizontalAlignment' = xlcenter.
  ENDIF.
ENDFORM.                    "fill_cell_alg
*&---------------------------------------------------------------------*
*&      Form  download_excel
*&---------------------------------------------------------------------*
*       Download data to excel
*----------------------------------------------------------------------*
FORM download_excel.
  DATA: lt_adrdata TYPE szadr_printform_table_line OCCURS 0,
        lt_adrcocd TYPE szadr_printform_table_line OCCURS 0,
        wa_adrdata TYPE szadr_printform_table_line.

  DATA: l_payterm  TYPE text1_052,
        l_attn(50) TYPE c,
        l_row      TYPE i,
        l_row1     TYPE i,
        l_row2     TYPE i,
        l_add_line TYPE adrs-anzzl,
        l_rec      TYPE i,
        l_add_cnt  TYPE i,
        l_add_cnt1 TYPE i.

  DATA: l_mcol     TYPE i.
  l_mcol = 8.

  SELECT text1 INTO l_payterm
    FROM t052u
    UP TO 1 ROWS
   WHERE spras = sy-langu
     AND zterm = wa_knb1-zterm
     AND text1 NE space.
  ENDSELECT.

* Get Company Address
  CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
    EXPORTING
      address_type            = '1'
      address_number          = wa_t001-adrnr
      sender_country          = wa_t001-land1
      street_has_priority     = 'X'
    IMPORTING
      address_printform_table = lt_adrcocd.

* Get Billing Address
  l_add_line = 3.
  IF wa_kna1-land1 <> 'US'.
    l_add_line = 4.
  ENDIF.

  CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
    EXPORTING
      address_type            = '1'
      address_number          = wa_kna1-adrnr
      sender_country          = wa_t001-land1
      number_of_lines         = l_add_line
      street_has_priority     = 'X'
    IMPORTING
      address_printform_table = lt_adrdata.

  CREATE OBJECT excel 'EXCEL.APPLICATION'.

* SET PROPERTY OF excel 'Visible' = 1.

  CALL METHOD OF
      excel
      'Workbooks' = workbooks.

  CALL METHOD OF
      workbooks
      'Add'.

  CALL METHOD OF
      excel
      'Worksheets' = sheets.

  CALL METHOD OF
      sheets
      'Add'  = sheet.

  SET PROPERTY OF sheet 'NAME' = 'STATEMENT'.

  LOOP AT gt_item.
    MOVE-CORRESPONDING gt_item TO gt_excel.
    gt_excel-total = gt_excel-curnt +
                     gt_excel-ovd30 +
                     gt_excel-ovd60 +
                     gt_excel-ovd90 +
                     gt_excel-ovdxx.
    APPEND gt_excel.
  ENDLOOP.
  FREE gt_item.

* Get logo
  PERFORM insert_logo USING 'Z_HYUNDAI_LARGE_LOGO'.

* Fill Company Code Address
  l_row1 = 2.
  l_row2 = l_row1 + 1.
  l_row  = 4.

  LOOP AT lt_adrcocd INTO wa_adrdata.
    l_row = l_row + 1.

    AT FIRST.
      PERFORM fill_cell_alg USING l_row1 5 1 'Account Statement' 'R'.
      PERFORM font_size USING l_row1 5 20.
** On 06/12/14  adding one column Wares (
*      PERFORM merge_cell USING l_row1 5 l_row2 7.
      PERFORM merge_cell USING l_row1 5 l_row2 l_mcol.
** )
    ENDAT.

    PERFORM fill_cell USING l_row 1 1 wa_adrdata-address_line.
    PERFORM merge_cell USING l_row 1 l_row 3.
  ENDLOOP.

* Fill Billing address
  l_row = l_row + 2.
  DESCRIBE TABLE lt_adrdata LINES l_add_cnt.
  l_add_cnt = l_add_cnt + 1.                     "Add 1 for attention
  LOOP AT lt_adrdata INTO wa_adrdata.
    l_row = l_row + 1.

    AT FIRST.
      l_row1 = l_row.
** On 06/12/14  adding one column Wares (
*      PERFORM fill_shade USING l_row 1 l_row 7.
      PERFORM fill_shade USING l_row 1 l_row l_mcol.
** )
      PERFORM fill_cell USING l_row 1 1 'BILLING ADDRESS'.
      PERFORM merge_cell USING l_row 1 l_row 3.
      l_row = l_row + 1.
    ENDAT.

    PERFORM fill_cell USING l_row 1 0 wa_adrdata-address_line.
    PERFORM merge_cell USING l_row 1 l_row 3.

    AT LAST.
      l_row = l_row + 1.
      CONCATENATE 'Attn:' wa_knb1-zsabe INTO l_attn SEPARATED BY space.
      PERFORM fill_cell USING l_row 1 0 l_attn.
      PERFORM merge_cell USING l_row 1 l_row 3.
    ENDAT.

  ENDLOOP.
  IF l_add_cnt < 4.
    l_add_cnt1 = 4 - l_add_cnt.
    DO l_add_cnt1 TIMES.
      l_row = l_row + 1.
      PERFORM merge_cell USING l_row 1 l_row 3.
    ENDDO.
  ENDIF.

* Fill Information
  l_row = l_row1.

  PERFORM fill_cell USING l_row 4 1 'INFORMATION'.
*  PERFORM merge_cell USING l_row 4 l_row 7.
  PERFORM merge_cell USING l_row 4 l_row l_mcol.

  l_row = l_row + 1.
  PERFORM fill_cell USING l_row 4 1 'Customer Number'.
  PERFORM merge_cell USING l_row 4 l_row 5.
  PERFORM fill_cell USING l_row 6 0 wa_kna1-kunnr.
*  PERFORM merge_cell USING l_row 6 l_row 7.
  PERFORM merge_cell USING l_row 6 l_row l_mcol.

  l_row = l_row + 1.
  PERFORM fill_cell USING l_row 4 1 'Statement Date'.
  PERFORM merge_cell USING l_row 4 l_row 5.
  PERFORM fill_cell USING l_row 6 0 p_augdt.
*  PERFORM merge_cell USING l_row 6 l_row 7.
  PERFORM merge_cell USING l_row 6 l_row l_mcol.

  l_row = l_row + 1.
  PERFORM fill_cell USING l_row 4 1 'Last Payment Received'.
  PERFORM merge_cell USING l_row 4 l_row 5.
  PERFORM fill_cell USING l_row 6 0 p_paydt.
*  PERFORM merge_cell USING l_row 6 l_row 7.
  PERFORM merge_cell USING l_row 6 l_row l_mcol.

  l_row = l_row + 1.
  PERFORM fill_cell USING l_row 4 1 'Terms'.
  PERFORM merge_cell USING l_row 4 l_row 5.
  PERFORM fill_cell USING l_row 6 0 l_payterm.
*  PERFORM merge_cell USING l_row 6 l_row 7.
  PERFORM merge_cell USING l_row 6 l_row l_mcol.

* set start row of statement item
  IF l_add_cnt > 4.
    l_add_cnt1 = l_add_cnt - 4.
    DO l_add_cnt1 TIMES.
      l_row = l_row + 1.
      PERFORM merge_cell USING l_row 4 l_row 5.
*      PERFORM merge_cell USING l_row 6 l_row 7.
      PERFORM merge_cell USING l_row 6 l_row l_mcol.
    ENDDO.
  ENDIF.
*  PERFORM set_border USING l_row1 1 l_row 7.
  PERFORM set_border USING l_row1 1 l_row l_mcol.

* Generate statement
  l_row = l_row + 2.

  DESCRIBE TABLE gt_excel LINES l_rec.
  LOOP AT gt_excel.

    PERFORM show_progress2 USING 'Downloading Data to Excel...' l_rec.

    AT FIRST.
      l_row1 = l_row.
*      PERFORM fill_shade USING l_row 1 l_row 7.
      PERFORM fill_shade USING l_row 1 l_row l_mcol.

      PERFORM fill_cell USING l_row 1 1 'Ref.Doc.'.
      PERFORM fill_cell USING l_row 2 1 'Doc.No.'.
      PERFORM fill_cell USING l_row 3 1 'Doc.Date'.
      PERFORM fill_cell USING l_row 4 1 'Due Date'.
      PERFORM fill_cell_alg USING l_row 5 1 'Quantity' 'R'.
      PERFORM fill_cell_alg USING l_row 6 1 'Amount' 'R'.
** Furong on 06/12/14 (
*      PERFORM fill_cell USING l_row 7 1 'Item'.
      PERFORM fill_cell USING l_row 7 1 'Curr'.
      PERFORM fill_cell USING l_row 8 1 'Item'.
** )
    ENDAT.

    l_row = l_row + 1.
    PERFORM fill_cell USING l_row 1 0 gt_excel-xblnr.
    PERFORM fill_cell USING l_row 2 0 gt_excel-belnr.
    PERFORM fill_cell USING l_row 3 0 gt_excel-bldat.
    PERFORM fill_cell USING l_row 4 0 gt_excel-zfbdt.
    PERFORM fill_cell_num USING l_row 5 0 gt_excel-zuonr 0.
    PERFORM fill_cell_num USING l_row 6 0 gt_excel-dmbtr 2.
*    PERFORM fill_cell USING l_row 7 0 gt_excel-sgtxt.
    PERFORM fill_cell USING l_row 7 0 gt_excel-waers.
    PERFORM fill_cell USING l_row 8 0 gt_excel-sgtxt.
    AT END OF bdate.
      SUM.
      l_row = l_row + 1.

      IF gt_excel-bdate >= p_augdt.
        PERFORM fill_cell USING l_row 3 1 'Total Due'.
      ELSE.
        PERFORM fill_cell USING l_row 3 1 'Past Due'.
      ENDIF.

      PERFORM fill_cell USING l_row 4 1 gt_excel-bdate.
      PERFORM fill_cell_num USING l_row 5 1 gt_excel-zuonr 0.
      PERFORM fill_cell_num USING l_row 6 1 gt_excel-dmbtr 2.
    ENDAT.

    AT LAST.
*      PERFORM set_border USING l_row1 1 l_row 7.
      PERFORM set_border USING l_row1 1 l_row l_mcol.

      SUM.
      l_row = l_row + 2.
      l_row1 = l_row.
      PERFORM fill_shade USING l_row 1 l_row 6.
      PERFORM fill_cell_alg USING l_row 1 1 'CURRENT' 'R'.
      PERFORM fill_cell_alg USING l_row 2 1 'OVERDUE 1-30' 'R'.
      PERFORM fill_cell_alg USING l_row 3 1 'OVERDUE 31-60' 'R'.
      PERFORM fill_cell_alg USING l_row 4 1 'OVERDUE 11-90' 'R'.
      PERFORM fill_cell_alg USING l_row 5 1 'OVERDUE 91+' 'R'.
      PERFORM fill_cell_alg USING l_row 6 1 'ACCT BALANCE' 'R'.

      l_row = l_row + 1.
      l_row2 = l_row.
      PERFORM fill_cell_num USING l_row 1 0 gt_excel-curnt 2.
      PERFORM fill_cell_num USING l_row 2 0 gt_excel-ovd30 2.
      PERFORM fill_cell_num USING l_row 3 0 gt_excel-ovd60 2.
      PERFORM fill_cell_num USING l_row 4 0 gt_excel-ovd90 2.
      PERFORM fill_cell_num USING l_row 5 0 gt_excel-ovdxx 2.
      PERFORM fill_cell_num USING l_row 6 0 gt_excel-total 2.

      PERFORM set_border USING l_row1 1 l_row2 6.
    ENDAT.
  ENDLOOP.

  CALL METHOD OF
      sheet
      'Columns' = columns.
  CALL METHOD OF
      columns
      'Autofit'
      NO
      FLUSH.

  PERFORM save_file.

  CALL METHOD OF
      workbooks
      'CLOSE'.
  CALL METHOD OF
      excel
      'QUIT'.

ENDFORM.                    " download_excel
*&---------------------------------------------------------------------*
*&      Form  save_file
*&---------------------------------------------------------------------*
*       Save File
*----------------------------------------------------------------------*
FORM save_file.
  DATA: w_file_name TYPE string,
        w_file_path TYPE string,
        w_full_path TYPE string,
        w_dflt_name TYPE string,
        w_answer    TYPE i.

  CONCATENATE 'CUSTOMER' p_kunnr p_bukrs p_augdt
         INTO w_dflt_name SEPARATED BY '_'.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select File Name'
      default_extension = 'xls'
      default_file_name = w_dflt_name
      file_filter       = 'Excel (*.xls)|*.xls| All (*.*)|*.*'
    CHANGING
      filename          = w_file_name
      path              = w_file_path
      fullpath          = w_full_path
      user_action       = w_answer
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF w_answer NE 0.
    MESSAGE s549(fibl).
  ENDIF.

  CHECK NOT w_full_path IS INITIAL.

  CALL METHOD OF
      sheet
      'SAVEAS'

    EXPORTING
      #1       = w_full_path
      #2       = 1.
ENDFORM.                    " save_file
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS2
*&---------------------------------------------------------------------*
*       Show progress indicator
*----------------------------------------------------------------------*
*      -->PF_TEXT  Display Text
*      -->PF_VAL   Percentage calculated base
*----------------------------------------------------------------------*
FORM show_progress2 USING   pf_text
                            pf_val.

  DATA:    l_prctx(3) TYPE c,
           l_tex1(50) TYPE c.

  STATICS: l_text(50) TYPE c,
           l_baseval  TYPE i,
           l_percent  TYPE i,
           l_counter  TYPE i.

  IF l_text NE pf_text.
    l_text = pf_text.
    CLEAR: l_baseval,
           l_percent,
           l_counter.
  ENDIF.

  IF NOT l_baseval IS INITIAL.
    l_counter = l_counter - 1.
    CHECK l_counter LE 0.
    l_percent = l_percent + 10.
    CHECK l_percent LE 100.
    l_counter = l_baseval.
  ELSE.
    l_baseval = pf_val DIV 10.
    l_counter = l_baseval.
  ENDIF.

  IF NOT pf_val IS INITIAL.
    IF NOT l_percent IS INITIAL.
      l_prctx = l_percent.
    ELSE.
      l_prctx = 1.
    ENDIF.

    CONCATENATE pf_text l_prctx '%' INTO l_tex1 SEPARATED BY space.
  ELSE.
    l_tex1 = pf_text.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = l_tex1.

ENDFORM.                    " SHOW_PROGRESS2
*&---------------------------------------------------------------------*
*&      Form  fill_shade
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I  text
*      -->P_J  text
*      -->P_K  text
*      -->P_L  text
*----------------------------------------------------------------------*
FORM fill_shade USING i j k l.
  DATA:cell1   TYPE ole2_object,
       cell2   TYPE ole2_object,
       range   TYPE ole2_object,
       shading TYPE ole2_object.

  CALL METHOD OF
      excel
      'Cells' = cell1
    EXPORTING
      #1      = i     "down
      #2      = j.    "across

  CALL METHOD OF
      excel
      'Cells' = cell2
    EXPORTING
      #1      = k     "down
      #2      = l.    "across

  CALL METHOD OF
      excel
      'Range' = range
    EXPORTING
      #1      = cell1
      #2      = cell2.
  CALL METHOD OF
      range
      'INTERIOR' = shading.
  SET PROPERTY OF shading 'ColorIndex' = 15.

ENDFORM.                    " fill_shade
*&---------------------------------------------------------------------*
*&      Form  merge_cell
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I  text
*      -->P_J  text
*      -->P_K  text
*      -->P_L  text
*----------------------------------------------------------------------*
FORM merge_cell USING i j k l.
  DATA:cell1   TYPE ole2_object,
       cell2   TYPE ole2_object,
       range   TYPE ole2_object.

  CALL METHOD OF
      excel
      'Cells' = cell1
    EXPORTING
      #1      = i     "down
      #2      = j.    "across

  CALL METHOD OF
      excel
      'Cells' = cell2
    EXPORTING
      #1      = k     "down
      #2      = l.    "across

  CALL METHOD OF
      excel
      'Range' = range
    EXPORTING
      #1      = cell1
      #2      = cell2.

  SET PROPERTY OF range 'Mergecells' = 'True'.

ENDFORM.                    " merge_cell
*&---------------------------------------------------------------------*
*&      Form  set_border
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I  text
*      -->P_J  text
*      -->P_K  text
*      -->P_L  text
*----------------------------------------------------------------------*
FORM set_border USING i j k l.
  DATA:cell1   TYPE ole2_object,
       cell2   TYPE ole2_object,
       range   TYPE ole2_object,
       borders TYPE ole2_object.

  CALL METHOD OF
      excel
      'Cells' = cell1
    EXPORTING
      #1      = i     "down
      #2      = j.    "across

  CALL METHOD OF
      excel
      'Cells' = cell2
    EXPORTING
      #1      = k     "down
      #2      = l.    "across

  CALL METHOD OF
      excel
      'Range' = range
    EXPORTING
      #1      = cell1
      #2      = cell2.

* SET BORDER PROPERTIES of range
  CALL METHOD OF
      range
      'BORDERS' = borders
    EXPORTING
      #1        = '1'.
  SET PROPERTY OF borders 'LineStyle' = '1'.
  SET PROPERTY OF borders 'WEIGHT' = 2.

  CALL METHOD OF
      range
      'BORDERS' = borders
    EXPORTING
      #1        = '2'.
  SET PROPERTY OF borders 'LineStyle' = '1'.
  SET PROPERTY OF borders 'WEIGHT' = 2.

  CALL METHOD OF
      range
      'BORDERS' = borders
    EXPORTING
      #1        = '3'.
  SET PROPERTY OF borders 'LineStyle' = '1'.
  SET PROPERTY OF borders 'WEIGHT' = 2.

  CALL METHOD OF
      range
      'BORDERS' = borders
    EXPORTING
      #1        = '4'.
  SET PROPERTY OF borders 'LineStyle' = '1'.
  SET PROPERTY OF borders 'WEIGHT' = 2.

ENDFORM.                    " set_border
*&---------------------------------------------------------------------*
*&      Form  insert_logo
*&---------------------------------------------------------------------*
*       Insert Company Logo
*----------------------------------------------------------------------*
*      -->P_FILENAME  Path Name of the BMP File
*----------------------------------------------------------------------*
FORM insert_logo USING p_logo.
  DATA: l_bytecount TYPE i,
        l_content   TYPE STANDARD TABLE OF bapiconten INITIAL SIZE 0.

  DATA: graphic_size TYPE i.
  DATA: l_temppath TYPE string.
  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.

  DATA: shapes TYPE ole2_object.

  CALL METHOD cl_gui_frontend_services=>get_temp_directory
    CHANGING
      temp_dir     = l_temppath
    EXCEPTIONS
      cntl_error   = 1
      error_no_gui = 2
      OTHERS       = 3.
  CHECK sy-subrc = 0.
  CALL METHOD cl_gui_cfw=>flush.

  CONCATENATE l_temppath '\' p_logo '.bmp' INTO l_temppath.

  GET PROPERTY OF sheet 'Shapes' = shapes.
  CALL METHOD OF
      shapes
      'AddPicture'

    EXPORTING
      #1           = l_temppath
      #2           = '1'
      #3           = '1'
      #4           = 1     "left
      #5           = 1     "top
      #6           = 75    "right
      #7           = 45.   "bottom

  CHECK sy-subrc <> 0.

  CALL FUNCTION 'SAPSCRIPT_GET_GRAPHIC_BDS'
    EXPORTING
      i_object       = 'GRAPHICS'
      i_name         = p_logo
      i_id           = 'BMAP'
      i_btype        = 'BCOL'
    IMPORTING
      e_bytecount    = l_bytecount
    TABLES
      content        = l_content
    EXCEPTIONS
      not_found      = 1
      bds_get_failed = 2
      bds_no_content = 3
      OTHERS         = 4.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'SAPSCRIPT_CONVERT_BITMAP'
    EXPORTING
      old_format               = 'BDS'
      new_format               = 'BMP'
      bitmap_file_bytecount_in = l_bytecount
    IMPORTING
      bitmap_file_bytecount    = graphic_size
    TABLES
      bds_bitmap_file          = l_content
      bitmap_file              = graphic_table
    EXCEPTIONS
      OTHERS                   = 1.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      bin_filesize            = graphic_size
      filename                = l_temppath
      filetype                = 'BIN'
    TABLES
      data_tab                = graphic_table
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

  IF sy-subrc <> 0.
    IF sy-subrc = 15.
      MESSAGE s011 WITH 'Access Denied'.
    ELSEIF NOT sy-msgid IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE s011 WITH 'Error when downloading the logo'.
    ENDIF.
  ELSE.
    GET PROPERTY OF sheet 'Shapes' = shapes.
    CALL METHOD OF
        shapes
        'AddPicture'

      EXPORTING
        #1           = l_temppath
        #2           = '1'
        #3           = '1'
        #4           = 1     "left
        #5           = 1     "top
        #6           = 75    "right
        #7           = 45.   "bottom
  ENDIF.

ENDFORM.                    " insert_logo
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_VBRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_vbrp .

  TYPES: BEGIN OF ty_vbrk,
         vbeln TYPE vbeln_vf,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_vbrk.

  DATA: l_handle    TYPE sytabix,
        lt_vbrp     TYPE TABLE OF vbrp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_vbrk TYPE TABLE OF ty_vbrk,
        ls_inx_vbrk TYPE ty_vbrk.

  CONSTANTS: c_zvbrk_001(9) VALUE 'ZVBRK_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zvbrk_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

  CHECK NOT gt_bsid[] IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_vbrk[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_vbrk
    FROM (l_gentab)
     FOR ALL ENTRIES IN gt_bsid
   WHERE vbeln = gt_bsid-vbeln.

  CHECK NOT lt_inx_vbrk[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_vbrp_a, gt_vbrp_a[].
  LOOP AT lt_inx_vbrk INTO ls_inx_vbrk.
    CALL FUNCTION 'ASH_SD_VBRK_READ'
      EXPORTING
        i_archivekey           = ls_inx_vbrk-archivekey
        i_offset               = ls_inx_vbrk-archiveofs
      TABLES
*       et_vbrk                = lt_vbrk
        et_vbrp                = lt_vbrp
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK sy-subrc = 0 AND NOT lt_vbrp[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_vbrp INTO TABLE gt_vbrp_a.
  ENDLOOP.

  SORT gt_vbrp_a BY vbeln posnr.
  DELETE ADJACENT DUPLICATES FROM gt_vbrp_a COMPARING vbeln posnr.

  LOOP AT gt_vbrp_a.
    "MOVE-CORRESPONDING gt_vbrp_a to gt_bill.
    gt_vbrp_a-vbeln    = gt_bill-vbeln.
    gt_vbrp_a-vgbel(3) = gt_bill-vgbel.
    gt_vbrp_a-fkimg    = gt_bill-fkimg.
    gt_vbrp_a-netwr    = gt_bill-netwr.

    APPEND gt_bill.  CLEAR gt_bill.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_VBRP
