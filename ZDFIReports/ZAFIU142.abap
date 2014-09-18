*----------------------------------------------------------------------
* Program ID        : ZAFIU142
* Title             : Generate International Wire Transfer file Format
* Created on        : 4/15/2010
* Created by        : Valerian Utama
* Specifications By : Calvin Kong
* Description       : International Wire Transfer file Format will only
*                     be generated if the payment method used is 'Y'
*----------------------------------------------------------------------*
* Modification Logs
* Date       Developer    Description
* 04/15/2010 VALERIAN     Initial program development
*            HIS20094
*----------------------------------------------------------------------*

REPORT zafiu142.

TABLES: reguh, lfbk, adrc.

DATA: BEGIN OF it_out OCCURS 0,
        tseqnum(6)   TYPE c,    "Transfer Sequence Number
        ttypeid(8)   TYPE c,    "Transfer Type ID
        valdate(8)   TYPE c,    "Value Date
        debacct(17)  TYPE c,    "Debit Account
        benamnt(14)  TYPE c,    "Beneficiary Amount
        curcode(3)   TYPE c,    "Currency Code
        custref(12)  TYPE c,    "Customer Reference
        orgacty(1)   TYPE c,    "Originator Account Type
        orgacct(34)  TYPE c,    "Originator Account
        orgname(35)  TYPE c,    "Originator Name
        orgadr1(35)  TYPE c,    "Originator Address Line 1
        orgadr2(35)  TYPE c,    "Originator Address Line 2
        orgadr3(35)  TYPE c,    "Originator Address Line 3
        xchrate(13)  TYPE c,    "Exchange Rate
        usdequi(14)  TYPE c,    "USD Equivalent for User Limit
        contrno(8)   TYPE c,    "Contract Number
        benacty(1)   TYPE c,    "Beneficiary Account Type
        benacct(34)  TYPE c,    "Beneficiary Account
        benname(35)  TYPE c,    "Beneficiary Name
        benadr1(35)  TYPE c,    "Beneficiary Address Line 1
        benadr2(35)  TYPE c,    "Beneficiary Address Line 2
        benadr3(32)  TYPE c,    "Beneficiary Address Line 3
        benctcd(2)   TYPE c,    "Beneficiary Country Code
        benfiat(1)   TYPE c,    "Beneficiary FI Institution Acct Type
        benfisn(24)  TYPE c,    "Beneficiary FI Institution Swift No.
        benfinm(35)  TYPE c,    "Beneficiary FI Institution Name
        benfia1(35)  TYPE c,    "Beneficiary FI Institution Adr.Line 1
        benfia2(35)  TYPE c,    "Beneficiary FI Institution Adr.Line 2
        benfia3(32)  TYPE c,    "Beneficiary FI Institution Adr.Line 3
        benficc(2)   TYPE c,    "Beneficiary FI Institution Cntry.Code
        benfisc(25)  TYPE c,    "Beneficiary Sort Code
        detchrg(3)   TYPE c,    "Details of Charges
        detpymt(140) TYPE c,    "Details of Payment
        itfacty(1)   TYPE c,    "Intermediary FI Account Type
        itfacct(24)  TYPE c,    "Intermediary FI Account Number
        itfname(35)  TYPE c,    "Intermediary FI Name
        itfadr1(35)  TYPE c,    "Intermediary FI Address Line 1
        itfadr2(35)  TYPE c,    "Intermediary FI Address Line 2
        itfadr3(35)  TYPE c,    "Intermediary FI Address Line 3
        orfacty(1)   TYPE c,    "Originator FI Account Type
        orfacct(24)  TYPE c,    "Originator FI Account Number
        orfname(35)  TYPE c,    "Originator FI Name
        orfadr1(35)  TYPE c,    "Originator FI Address Line 1
        orfadr2(35)  TYPE c,    "Originator FI Address Line 2
        orfadr3(35)  TYPE c,    "Originator FI Address Line 3
        refacty(1)   TYPE c,    "Receiving FI Account Type
        refacct(24)  TYPE c,    "Receiving FI Account Number
        refname(35)  TYPE c,    "Receiving FI Name
        benadcd(3)   TYPE c,    "Beneficiary Advice Code
        benadtx(98)  TYPE c,    "Beneficiary Advice Text
        benfacd(3)   TYPE c,    "Beneficiary FI Advice Code
        benfatx(98)  TYPE c,    "Beneficiary FI Advice Text
        itfadcd(3)   TYPE c,    "Intermediary FI Advice Code
        itfadtx(98)  TYPE c,    "Intermediary FI Advice Text
        misfins(210) TYPE c,    "Miscellaneous FI Instructions
        recfins(205) TYPE c,    "Receiving FI Instructions
        itfinst(205) TYPE c,    "Intermediary FI Instructions
        benfins(205) TYPE c,    "Beneficiary FI Instructions
        orgamnt(14)  TYPE c,    "Originator Amount
      END OF it_out.

DATA: filename TYPE string.

PARAMETERS: p_file(1000) TYPE c LOWER CASE OBLIGATORY.

*- File Catalog
TYPE-POOLS: slis.
DATA : gt_fieldcat TYPE slis_t_fieldcat_alv.

GET reguh.
  CHECK reguh-rzawe = 'Y'.
  PERFORM populate_data CHANGING it_out.
  APPEND it_out. CLEAR it_out.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'ZW_XVORL'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'ZW_LAUFD' OR screen-name = 'ZW_LAUFI'.
      screen-required = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM browser CHANGING filename.
  IF NOT filename IS INITIAL.
    p_file = filename.
    PERFORM update_sfield USING 'P_FILE' p_file.
  ENDIF.

END-OF-SELECTION.
  IF it_out[] IS INITIAL.
    MESSAGE i208(00) WITH text-m01.
    LEAVE PROGRAM.
  ENDIF.

  PERFORM download USING p_file CHANGING it_out[].
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PAR_FILE  text
*----------------------------------------------------------------------*
FORM browser CHANGING filename.

  DATA: $filename TYPE string,
        l_filename TYPE string,
        l_path     TYPE string,
        l_action   TYPE i.

  $filename = filename.

* Create default File Name
  CONCATENATE 'IWIRE-' sy-datum+2(6) '-' sy-uzeit INTO $filename.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select File Name'
*     default_extension = '*.txt'
      default_file_name = $filename
      file_filter       = '*.*'
      initial_directory = 'c:\temp\'
    CHANGING
      filename          = l_filename
      path              = l_path
      fullpath          = filename
      user_action       = l_action
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3
          .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME  text
*      <--P_IT_OUT[]  text
*----------------------------------------------------------------------*
FORM download USING    p_filename
              CHANGING p_it_out.

  DATA: filename TYPE string.

  CHECK NOT p_filename IS INITIAL.
  filename = p_filename.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                = filename
    CHANGING
      data_tab                = p_it_out
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
    OTHERS                  = 22
          .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MESSAGE s368(00) WITH 'Downloaded Filename:' p_file.
  ENDIF.

ENDFORM.                    " download

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM alv_grid_display  TABLES it_out.
ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i,
        gs_fieldcat TYPE slis_fieldcat_alv.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_s     = &3.        " Column heading
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-lowercase     = 'X'.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'TSEQNUM'    'Trans.Seq.No'     06  'CHAR' '' '',
    ' '  'TTYPEID'    'Trans.Type ID'    08  'CHAR' '' '',
    ' '  'VALDATE'    'Value Date'       08  'CHAR' '' '',
    ' '  'DEBACCT'    'Debit Account'    17  'CHAR' '' '',
    ' '  'BENAMNT'    'Ben.Amount'       14  'CHAR' '' '',
    ' '  'CURCODE'    'Curr.'            03  'CHAR' '' '',
    ' '  'CUSTREF'    'Cust. Ref.'       12  'CHAR' '' '',
    ' '  'ORGACTY'    'Org.Acc.Ty.'      01  'CHAR' '' '',
    ' '  'ORGACCT'    'Org.Account'      34  'CHAR' '' '',
    ' '  'ORGNAME'    'Org.Name'         35  'CHAR' '' '',
    ' '  'ORGADR1'    'Org.Addr.1'       35  'CHAR' '' '',
    ' '  'ORGADR2'    'Org.Addr.2'       35  'CHAR' '' '',
    ' '  'ORGADR3'    'Org.Addr.3'       35  'CHAR' '' '',
    ' '  'XCHRATE'    'Xchng.Rate'       13  'CHAR' '' '',
    ' '  'USDEQUI'    'USD Eq.Lim.'      14  'CHAR' '' '',
    ' '  'CONTRNO'    'Contract No'      08  'CHAR' '' '',
    ' '  'BENACTY'    'Ben.Acc.Ty.'      01  'CHAR' '' '',
    ' '  'BENACCT'    'Ben.Account'      34  'CHAR' '' '',
    ' '  'BENNAME'    'Ben.Name'         35  'CHAR' '' '',
    ' '  'BENADR1'    'Ben.Addr. 1'      35  'CHAR' '' '',
    ' '  'BENADR2'    'Ben.Addr. 2'      35  'CHAR' '' '',
    ' '  'BENADR3'    'Ben.Addr. 3'      32  'CHAR' '' '',
    ' '  'BENCTCD'    'Ben.Cnty.Cd'      02  'CHAR' '' '',
    ' '  'BENFIAT'    'Ben.FI.Acc.Ty'    01  'CHAR' '' '',
    ' '  'BENFISN'    'Ben.FI.SWIFT'     24  'CHAR' '' '',
    ' '  'BENFINM'    'Ben.FI.Name'      35  'CHAR' '' '',
    ' '  'BENFIA1'    'Ben.FI.Adr.1'     35  'CHAR' '' '',
    ' '  'BENFIA2'    'Ben.FI.Adr.2'     35  'CHAR' '' '',
    ' '  'BENFIA3'    'Ben.FI.Adr.3'     32  'CHAR' '' '',
    ' '  'BENFICC'    'Ben.FI.Cnty.Cd'   02  'CHAR' '' '',
    ' '  'BENFISC'    'Ben.Sort Cd.'     25  'CHAR' '' '',
    ' '  'DETCHRG'    'Det.Chrg.'        03  'CHAR' '' '',
    ' '  'DETPYMT'    'Det.Pymnt'       140  'CHAR' '' '',
    ' '  'ITFACTY'    'Inter.Acc.Ty.'    01  'CHAR' '' '',
    ' '  'ITFACCT'    'Inter.Acc.No.'    24  'CHAR' '' '',
    ' '  'ITFNAME'    'Inter.FI.Name'    35  'CHAR' '' '',
    ' '  'ITFADR1'    'Inter.Addr. 1'    35  'CHAR' '' '',
    ' '  'ITFADR2'    'Inter.Addr. 2'    35  'CHAR' '' '',
    ' '  'ITFADR3'    'Inter.Addr. 3'    35  'CHAR' '' '',
    ' '  'ORFACTY'    'Org.FI.Acc.Ty.'   01  'CHAR' '' '',
    ' '  'ORFACCT'    'Org.FI.Acc.No.'   24  'CHAR' '' '',
    ' '  'ORFNAME'    'Org.FI.Name'      35  'CHAR' '' '',
    ' '  'ORFADR1'    'Org.FI.Addr.1'    35  'CHAR' '' '',
    ' '  'ORFADR2'    'Org.FI.Addr.2'    35  'CHAR' '' '',
    ' '  'ORFADR3'    'Org.FI.Addr.3'    35  'CHAR' '' '',
    ' '  'REFACTY'    'Rec.FI Acc.Ty.'   01  'CHAR' '' '',
    ' '  'REFACCT'    'Rec.FI Acc.No.'   24  'CHAR' '' '',
    ' '  'REFNAME'    'Rec.FI Name'      35  'CHAR' '' '',
    ' '  'BENADCD'    'Ben.Adv.Cd.'      03  'CHAR' '' '',
    ' '  'BENADTX'    'Ben.Adv.Tx.'      98  'CHAR' '' '',
    ' '  'BENFACD'    'Ben.FI.Adv.Cd.'   03  'CHAR' '' '',
    ' '  'BENFATX'    'Ben.FI.Adv.Tx.'   98  'CHAR' '' '',
    ' '  'ITFADCD'    'Inter.Adv.Cd'     03  'CHAR' '' '',
    ' '  'ITFADTX'    'Inter.Adv.Tx'     98  'CHAR' '' '',
    ' '  'MISFINS'    'Misc.FI.Inst.'   210  'CHAR' '' '',
    ' '  'RECFINS'    'Rec.FI.Inst.'    205  'CHAR' '' '',
    ' '  'ITFINST'    'Inter.FI.Inst.'  205  'CHAR' '' '',
    ' '  'BENFINS'    'Ben.FI.Inst.'    205  'CHAR' '' '',
    ' '  'ORGAMNT'    'Org.Amount'       14  'CHAR' '' ''.

  LOOP AT ft_fieldcat INTO gs_fieldcat WHERE fieldname = 'BENAMNT'
                                          OR fieldname = 'ORGAMNT'.
    gs_fieldcat-just = 'R'.
    MODIFY ft_fieldcat FROM gs_fieldcat TRANSPORTING just.
  ENDLOOP.

ENDFORM.                    " fieldcat_init

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab.

  DATA : gs_layout TYPE slis_layout_alv,
         l_repid   TYPE sy-repid.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.
  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = l_repid
            is_layout          = gs_layout
            it_fieldcat        = gt_fieldcat
       TABLES
            t_outtab           = ft_outtab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  update_sfield
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0409   text
*      -->P_P_FILE  text
*----------------------------------------------------------------------*
FORM update_sfield USING    p_fname
                            p_file.

  DATA: it_fieldtab TYPE dynpread OCCURS 0 WITH HEADER LINE,
        l_repid TYPE sy-repid.

  l_repid = sy-repid.

  it_fieldtab-fieldname  = p_fname.
  it_fieldtab-fieldvalue = p_file.
  APPEND it_fieldtab.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname     = l_repid
            dynumb     = sy-dynnr
       TABLES
            dynpfields = it_fieldtab.

ENDFORM.                    " update_sfield
*&---------------------------------------------------------------------*
*&      Form  populate_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_OUT[]  text
*----------------------------------------------------------------------*
FORM populate_data CHANGING it_out LIKE it_out.
  STATICS: tseqnum(6) TYPE n.

  DATA: l_lifnr TYPE reguh-lifnr,
        l_adrnr TYPE lfa1-adrnr.

* Populate Transfer Sequence Number
  tseqnum = tseqnum + 1.
  it_out-tseqnum = tseqnum.

* Populate Transfer Type ID
  it_out-ttypeid = 'INTLWIRE'.

* Populate Value Date
  it_out-valdate = reguh-zaldt.
  SHIFT it_out-valdate BY 4 PLACES CIRCULAR.

* Populate Debit Account
  CONCATENATE 'WBCS' reguh-ubknt INTO it_out-debacct.

* Populate Currency Code
  it_out-curcode = 'USD'.

* Populate Originator Account Type
  it_out-orgacty = 'D'.

* Populate Originator Account Number
  it_out-orgacct = reguh-ubknt.

* Populate Originator Name
  it_out-orgname = 'HYUNDAI MOTOR MANUFACTURING ALA LLC'.

* Populate Originator address Line 2
  it_out-orgadr2 = '700 HYUNDAI BLVD'.

* Populate Originator address Line 3
  it_out-orgadr3 = 'MONTGOMERY AL'.

* Populate Exchange Rate
  WRITE '1.00000000' TO it_out-xchrate RIGHT-JUSTIFIED.

* Populate Beneficiary Account Type
  it_out-benacty = 'D'.

* Get Beneficiary Details
  SELECT SINGLE adrnr INTO l_adrnr
    FROM lfa1
   WHERE lifnr = reguh-lifnr.

  SELECT * FROM adrc UP TO 1 ROWS
   WHERE addrnumber = l_adrnr.
  ENDSELECT.

* Populate Beneficiary Name
  it_out-benname = adrc-name1.

* Populate Beneficiary Address Line 1
  it_out-benadr1 = adrc-street(35).

* Populate Beneficiary Address Line 2
  it_out-benadr2 = adrc-street+35(25).

* Populate Beneficiary Address Line 3
  CONCATENATE adrc-str_suppl1 adrc-str_suppl2
         INTO it_out-benadr3 SEPARATED BY ', '.

* Populate Beneficiary Financial Institution Account Type
  it_out-benfiat = 'B'.

* Get Vendor bank details
  SELECT * FROM lfbk UP TO 1 ROWS
   WHERE lifnr = reguh-lifnr.
  ENDSELECT.

  IF sy-subrc EQ 0.
* Populate Beneficiary Account
    it_out-benacct = lfbk-bankn.

* Populate Beneficiary Financial Institution Name
* Populate Beneficiary Financial Institution Address Line 1 & 2
* Populate Beneficiary Institution Swift Number

    SELECT SINGLE banka stras ort01 swift
      INTO (it_out-benfinm, it_out-benfia1,
            it_out-benfia2, it_out-benfisn)
      FROM bnka
     WHERE banks = lfbk-banks
       AND bankl = lfbk-bankl.
  ENDIF.

  SHIFT reguh-empfg LEFT DELETING LEADING '>'.
  l_lifnr = reguh-empfg.

  IF NOT l_lifnr IS INITIAL.
* Get Alternative Vendor bank details
    SELECT * FROM lfbk UP TO 1 ROWS
     WHERE lifnr = l_lifnr.
    ENDSELECT.

    IF sy-subrc EQ 0.
* Populate Intermediary FI Account type
      it_out-itfacty = 'C'.

* Populate Intermediary Financial Institution Name
* Populate Intermediary Financial Institution Address Line 1 & 2
* Populate Intermediary Institution CHIPS Account

      SELECT SINGLE banka stras ort01 bankl
        INTO (it_out-itfname, it_out-itfadr1,
              it_out-itfadr2, it_out-itfacct)
        FROM bnka
       WHERE banks = lfbk-banks
         AND bankl = lfbk-bankl.
    ENDIF.

  ENDIF.

* Populate Details of Charges.
  it_out-detchrg = 'SHR'.

* Populate Originator Amount (last field)
  reguh-rbetr = reguh-rbetr * -1.
  it_out-orgamnt = reguh-rbetr.
  WRITE it_out-orgamnt TO it_out-orgamnt RIGHT-JUSTIFIED.

ENDFORM.                    " populate_data
