REPORT ZC02FIC_CHECK no standard page heading message-id 0.

*&---------------------------------------------------------------------*
*& Program Name :  ZC02FIC_CHECK
*& Description  :  Checks upload
*
*& Module : FI                                                         *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*& Date        Author           Description/Reason for Change          *
*& ----------  --------------   ------------------------------------   *
*& 02/10/03    Cyril Alex       Initial Development                    *
*&---------------------------------------------------------------------*
*& This program downloads an excel sheet with check details which can
*& then be used to upload FI transactioins using program YFI
*&---------------------------------------------------------------------*



*************************************
*             TABLES                *
*************************************

Tables: LFB1,   " Vendor Master (Company Code)
        T074.   " Special G/L Accounts
* DATA declarations.

***********************************  Internal Tables

* Vendor Internal table
Data : begin of t_ven occurs 0,   " store vendor mappings (old and new)
         altkn like lfb1-altkn,   " Previous Master Record Number
         lifnr like lfb1-lifnr,   " Account number of vendor or creditor
       end of t_ven.


* Structure of the Flat file
************************************************************************


Data: begin of T_flat occurs 0,
         SLNO(6),       " **sl. no   - serial number **
         INVOIS(15),    " ** Invoice number
         DOCTYP(1),     " **Inv. type **
         VENDIV(5),     " ** Div **
         Vendor(10),    " **Old Vendor Number **
         SORCJ(2),      " **Source Journal **
         SORCB(6),      " ** Source batch
         TRANDT(10),    " Transaction date
         check(10),     " Check number
         WRBTR(016),    "   data element: WRBTR  : Amount
 end of T_FLAT.
data: WA_flat like t_flat.

data: begin of t_downld occurs 0,
       slno(6),      " Serial numbaer
       endoft(1),    " End of transaction marker 'X'
       Docdt(10),     " Document date
       doctyp(5), "  Document type
       Postdt(10),    " Posting date
       Refer(16),    " Reference (check no)
       Hedrtxt(25),  " Header text
       pstkey(5),    " Posting key
       account(15),  " Account
       splldgr(1),   " Spl ledger - blank
       amount(16),   " Amount
       taxcode(1) value ' ',   " Blank
       taxamt(1) value ' ',    " Blank
       costcent(1)  value ' ',  " blank
       intord(1) value ' ',    " blank internal order
       bldat(1) value ' ',     " blank baseline date
       valdat(1) value ' ',    " blank value date
       payt(1) value ' ',      " blank payment term
       paymtd(1) value ' ',    " Blank payment method
       payblk(1) value ' ',    " payment block
       assign(15) value ' ',   " Assignment (invoice number)
       text(25) value ' ',      " blank
     end of t_downld.

data: w_endkey,
      w_doccnt like sy-tabix,
      w_tbx like sy-tabix,
      w_len type i.
data: w_amt1 type p, w_amt2 type p .

* End of declarations
************************************************************************

* Selection screen
selection-screen begin of block blk1 with frame title text-001.
parameters p_flname like rlgrap-filename obligatory. " SOURCE file name
parameters p_tfname like rlgrap-filename.   " TARGET FILE NAME
*parameter: p_mode(1) type c obligatory default 'N'. " call trans. mode
selection-screen begin of line.
selection-screen comment  1(11) text-002 for field p_list.
selection-screen position 33.
parameters p_list as checkbox default 'X'.  " list prn at end?
selection-screen end of line.
selection-screen end of block blk1.

selection-screen begin of block blk2 with frame title text-003.
selection-screen begin of line.
selection-screen comment  1(60) cmt1.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment  1(60) cmt2.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment  1(60) cmt3.
selection-screen end of line.
selection-screen end of block blk2.

*******************************
* Initialization.
*******************************
Initialization.

p_flname ='C:\Documents and Settings\Cyril V.Alex\Desktop\Hyundai\data'.
cmt1 = 'F4 help is available for the file names'.
cmt2 = 'Please format the amount column of the XL sheet to "Number" '.
cmt3 = 'CHECK: Does not download the file'.

*******************************
* at selection-screen
*******************************
at selection-screen on value-request for p_flname.

* F4 on filename
  call function 'WS_FILENAME_GET'
       EXPORTING
            mask             = ',*.*,*.*.'
            mode             = 'O'
            title            = 'Select File'
       IMPORTING
            filename         = p_flname
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            others           = 5.

at selection-screen on value-request for p_tfname.

* F4 on filename
  call function 'WS_FILENAME_GET'
       EXPORTING
            mask             = ',*.*,*.*.'
            mode             = 'O'
            title            = 'Select File'
       IMPORTING
            filename         = p_tfname
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            others           = 5.

at selection-screen on p_LIST.
  IF P_LIST ne 'X' AND P_TFNAME IS INITIAL.
    MESSAGE W999 WITH 'Please enter target file name'.
    call selection-screen '1000'.
  endif.

*******************************
* Start of selection
*******************************
start-of-selection.

  call function 'WS_UPLOAD'
       EXPORTING
            filename = p_flname
            filetype = 'DAT'
       TABLES
            data_tab = t_flat.

* delete entries without check number
  loop at t_flat where check is initial.
    delete t_flat index sy-tabix.
  endloop.

* select SAP vendor number against the old vendor number

 select lifnr altkn  from lfb1 into corresponding fields of table t_ven.
 sort T_ven.

read table t_flat index 1.
 if t_flat-wrbtr cs 'mount'.
  delete t_flat index 1.   " delete the title line.
 endif.


  loop at t_flat.
    w_tbx = sy-tabix.
    w_amt1 = t_flat-wrbtr.
    loop at t_flat into wa_flat from w_tbx where check = t_flat-check
      and slno ne t_flat-slno.
      w_amt2 = wa_flat-wrbtr.
      w_amt1 = w_amt1 + w_amt2.

      if w_amt1 = 0.
        delete t_flat index w_tbx.
        delete t_flat.
        exit.
      endif.
    endloop.

    if w_amt1 ne 0.

      PERFORM FILL_DOWN using w_ENDKEY.
      w_ENDKEY = 'X'.
      PERFORM FILL_DOWN using w_ENDKEY.
      clear w_endkey.
    endif.
  ENDLOOP.

  uline.uline.

  perform write_list.
if p_list ne 'X'.
  perform down_ld.
endif.

  top-of-page.
  perform write_list_hdr.

*---------------------------------------------------------------------*
*       FORM FILL_DOWN                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  EKEY                                                          *
*---------------------------------------------------------------------*
FORM FILL_DOWN USING EKEY.

  DATA: W_TEXT(25).
  DATA: W_PKEY(2) VALUE 'BC'.
  DATA: w_amt type f.
  w_amt =  t_flat-wrbtr.

  CONCATENATE T_FLAT-SORCJ T_FLAT-SORCB INTO W_TEXT.


  t_downld-slno     =  T_FLAT-SLNO.
  t_downld-Docdt    =  T_FLAT-TRANDT.     " Document date
  t_downld-Postdt   =  T_FLAT-TRANDT.     " Posting date
  t_downld-Refer    =  T_FLAT-CHECK.      " Reference (check no)
  t_downld-Hedrtxt  =  W_TEXT.            " Header text
  t_downld-amount   =  T_FLAT-WRBTR.      " Amount
  t_downld-assign   =  T_FLAT-INVOIS.     " Assignment(inv. number)
  t_downld-doctyp   =  'BC'.


  IF EKEY = 'X'.
w_doccnt = w_doccnt + 1.
    t_downld-account  =   '113000'.        " Bank GL accn no.
    t_downld-endoft   =  EKEY.             " End of transa marker 'X'
    if w_amt < 0.
      t_downld-pstkey   = '40'.
      replace '-' with '' into t_downld-amount.
    else.
      t_downld-pstkey   = '50'.              " Posting key
    endif.


  else.             " Vendor

    t_downld-endoft   = ' '.
    if w_amt < 0.
      t_downld-pstkey   = '35'.
      replace '-' with '' into t_downld-amount.
      t_downld-text = 'CORRECTED  Vendor'.
    else.
      t_downld-pstkey   = '25'.
    endif.

    if not t_flat-vendor is initial.
    read table t_ven with key altkn = t_flat-vendor.
*      loop at t_ven where altkn = t_flat-vendor.  endloop.
      if sy-subrc = 0.
        t_downld-account = t_ven-lifnr.
      else.
        t_downld-account  = t_flat-vendor.
        t_downld-text = 'Vendor not found in SAP'.
      endif.
    endif.
  endif.
  append t_downld.
  clear t_downld.
endform.



*---------------------------------------------------------------------*
*       FORM write_list                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form write_list_hdr.
format color 5.
  Write:/ 'Totally', w_doccnt, ' documents'.
format reset.
format color 1.
write:/0   sy-uline(175).
write:/1   sy-vline,2 'END',
       6   sy-vline,7 'Doc. Dt',
       18  sy-vline,19 'DT',
       22  sy-vline,23 'Post dt.',
       24  sy-vline,25 'Reference',
       36  sy-vline,37 'Hdr.',
       44  sy-vline,45 'PK',
       48  sy-vline,49 'Account',
       60  sy-vline,61 'Spl.Gl',
       68  sy-vline,69 'Amount',
       88  sy-vline,89 'TaxCD',
       95  sy-vline,96 'Cost cent',
       106 sy-vline,107 'Int Ord',
       126 sy-vline,127 'Bl Dat',
       139 sy-vline,140 'Pay ty',
       148 sy-vline,149 'Pay blk',
       157 sy-vline,158 'Assign',
       175 sy-vline,
       /0  SY-ULINE(175),
       /1  sy-vline,
        5  sy-vline,7 'Text',
       175 sy-vline.

write:/0 sy-uline(175).
  format reset.
endform.

form write_list.


w_LEN = 1.
  loop at t_downld.
IF w_LEN = 1.
FORMAT COLOR 2 INTENSIFIED ON.
W_LEN = 2.
ELSE.
W_LEN = 1.
FORMAT COLOR 2 INTENSIFIED OFF.
ENDIF.


*    if  t_downld-text cs 'CORRECTED'.
    write:/1   SY-VLINE,
           2   t_downld-endoft,    " End of transaction marker 'X'
           7   t_downld-Docdt,     " Document date
           19  t_downld-doctyp,     "  Document type
           23  t_downld-Postdt,    " Posting date
           25  t_downld-Refer,    " Reference (check no)
           37  t_downld-Hedrtxt,  " Header text
           45  t_downld-pstkey,    " Posting key
           49  t_downld-account,  " Account
           61  t_downld-splldgr,   " Spl ledger - blank
           69  t_downld-amount,   " Amount
           89  t_downld-taxcode ,   " taxcode
*              t_downld-taxamt(1) ,    " Blank
           96  t_downld-costcent ,  " Const cent
           107 t_downld-intord ,    " internal order
           127 t_downld-bldat ,     " baseline date
*              t_downld-valdat,        " value date
           140 t_downld-payt ,      " blank payment term
*              t_downld-paymtd(1) ,    " Blank payment method
           149 t_downld-payblk ,     " payment block
           158 t_downld-assign,    " Assignment (invoice number)
           175 sy-vline,
           /0 SY-VLINE.
           if not t_downld-text is initial.
           write:17 t_downld-text color 7. " blank
            endif.
           write: 175 sy-vline.

IF T_DOWNLD-ENDOFT = 'X'.
write:/0 sy-uline(175).
ENDIF.
  endloop.
  ULINE.

endform.


*---------------------------------------------------------------------*
*       FORM down_ld                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form down_ld.

  if p_list ne 'X'.

    CALL FUNCTION 'WS_DOWNLOAD'
     EXPORTING
       FILENAME                      = p_tfname
       FILETYPE                      = 'DAT'
       MODE                          = 'O'
      TABLES
        DATA_TAB                      =  t_downld
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.
endform.
