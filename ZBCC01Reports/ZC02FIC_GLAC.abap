REPORT ZC02FIC_GLAC no standard page heading message-id 0 line-size 200.

*&---------------------------------------------------------------------*
*& Program Name : ZC02FIC_GLAC
*& Description  : BDC Program for FB01 for GL / PR
*
*& Module : FI                                                         *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*& Date        Author           Description/Reason for Change          *
*& ----------  --------------   ------------------------------------   *
*& 02/08/2003            Cyril Alex       Initial Development          *
*&---------------------------------------------------------------------*
*& This program downloads an excel sheet with GL details which can
*& then be used to upload FI transactioins using program YFI
*&---------------------------------------------------------------------*



*************************************
*             TABLES                *
*************************************

Tables: LFB1,   " Vendor Master (Company Code)
        SKB1,   " G/L account master (company code)
        T074,   " Special G/L Accounts
        AUFK,   " Order master data
        ANLA.   " Asset Master Record Segment

* DATA declarations.

***********************************  Internal Tables

* Vendor Internal table
Data : begin of t_ven occurs 0,   " store vendor mappings (old and new)
         altkn like lfb1-altkn,   " Previous Master Record Number
         lifnr like lfb1-lifnr,   " Account number of vendor or creditor
       end of t_ven.

* GL Accounts list
DATA: begin of t_GLAC occurs 0,
        SAKNR like skb1-saknr,
        MITKZ type i,
      end of t_GLAC.

* Asset accounts list
DATA: begin of t_ast occurs 0,
        anln1 like anla-anln1,    " SAP Asset number
        invnr like anla-invnr,    " Old Asset number
      end of t_ast.

* INTERNAL ORDERS
data: begin of T_into occurs 0,
        aufex like aufk-aufex,   " External order number
        aufnr like aufk-aufnr,   " Order Number
      end of t_into.


DATA: G_ACCT LIKE SKB1-SAKNR.


* Structure of the Flat file
************************************************************************


Data: begin of T_flat occurs 0,
*         SLNO(6),       " **sl. no   - serial number **
         ACCTNO(15),    " Account number
         SAPGL(15),     " SAP GL Account number
         ACCTDES(40),   " A/C  description
         COSTC(10),      " Cost Center
         INTORD(20),     " Internal Order
         ASSNO(15),     " Asset number
         TRANDT(10),    " Transaction date
         SORCJ(2),      " **Source Journal **
         SORCB(6),      " ** Source batch
         srcsyst(4),    " Source system
         RFDESC(40),    " Reference Description
         VENDIV(5),     " ** Div **
         VENDOR(10),    " **Old Vendor Number **
         DOCTYP(1),     " **Inv. type **
         INVOIS(15),    " ** Invoice number
         INVDT(10),     " Document date
         TERMCD(5),     " Term code
         TAXSC(15),     " Tax schedule
         DRCR(2),       " DR/CR indicator
         WRBTR(016),    " WRBTR  : Amount
 end of T_FLAT.

* FILE USED TO DOWNLOAD THE DAT FILE AT THE END
data: begin of t_downld occurs 0,
       slno(6),                    " Serial number
       endoft(1),                  " End of transaction marker 'X'
       Docdt(10),                  " Document date
       doctyp(5),                  "  Document type
       Postdt(10),                 " Posting date
       Refer(16),                  " Reference (check no)
       Hedrtxt(25),                " Header text
       pstkey(5),                  " Posting key
       account(15),                " Account
       splldgr(10),                 " Spl ledger - blank
       amount(16),                 " Amount
       taxcode(1) value ' ',       " Blank
       taxamt(1) value ' ',        " Blank
       costcent(10)  value ' ',     " blank
       intord(13) value ' ',        " blank internal order
       bldat(10) value ' ',         " blank baseline date
       valdat(10) value ' ',        " value date
       payt(1) value ' ',          " blank payment term
       paymtd(1) value ' ',        " Blank payment method
       payblk(1) value ' ',        " payment block
       assign(15) value ' ',       " Assignment (invoice number)
       text(40) ,         " blank
     end of t_downld.

* TABLE TO TRAP VITAL ERRORS
DATA: BEGIN OF T_ERR OCCURS 5.
        INCLUDE STRUCTURE T_FLAT.
DATA:   MESSAGE(50),
      END OF T_ERR.

* WORK VARIABLES
data : w_endkey,                   " End of transaction
       w_mitkz like skb1-mitkz,    " Asset/Vendor ID
       w_UMSKZ like t074-UMSKZ,    " Spl GL Indicator
       w_pstkey(5),                " Posting key
       w_len like sy-tabix,        " Table length
       w_cntr like sy-tabix value 1,  " SL. no cntr
       w_gldummy type i,            " Dummy GL numeric
       w_doccnt like sy-tabix,      " Documents count
       w_srcb(5).                  " Source batch number

* macro increament
define incr.
  &1  = &1 + 1.
end-of-definition.


* End of declarations
************************************************************************

*  Selection screen

selection-screen begin of block blk1 with frame title text-001.
parameters p_flname like rlgrap-filename obligatory. " SOURCE file name
parameters p_tfname like rlgrap-filename.   " TARGET FILE NAME
*parameter p_mode(1) type c obligatory default 'N'. " call trans. mode
selection-screen begin of line.
selection-screen comment  1(11) text-002 for field p_list.
selection-screen position 33.
parameters p_list as checkbox default 'X'.  " CHECK or & DOWNLD
selection-screen end of line.
selection-screen end of block blk1.

* comments/Notes
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
  perform get_file using p_flname.

at selection-screen on value-request for p_tfname.
  perform get_file using p_tfname.

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


  loop at t_flat.     " Clear Quotes from Amount field
    do 2 times.
      replace '"' with '' into t_flat-wrbtr.
    enddo.
    modify t_flat.
  endloop.

* select SAP vendor numbers against the old vendor numbers
 select lifnr altkn  from lfb1 into corresponding fields of table t_ven.
  sort t_ven.
* select SAP Asset numbers against old asset numbers
  select anln1 invnr from anla into corresponding fields of table t_ast.
  sort t_ast.
* select SAP Internal Orders against old Internal order numbers

  select aufnr aufex from aufk into (aufk-aufnr, aufk-aufex).
    write aufk-aufnr to t_into-aufnr.
    write aufk-aufex to t_into-aufex.
    append t_into.
  endselect.
  sort t_into.

*loop at t_into . "where aufex = '2'.
*write:/ t_into-aufex, t_into-aufnr.
*endloop.

  read table t_flat index 1.
  if t_flat-wrbtr cs 'mount'.
    delete t_flat index 1.   " delete the title line.
  endif.

* check if the GL account is Asset/or Vendor)
  select saknr mitkz from skb1 into (t_glac-saknr, w_mitkz)
           where bukrs = 'H201'.
    if sy-subrc = 0 and w_mitkz CS 'A'.      " Asset
      t_glac-mitkz = 1.
    elseif w_mitkz CS 'K'.                   " Vendor
      t_glac-mitkz = 2.
    endif.
    append t_glac.

    clear t_glac.
  endselect.

  read table t_flat index 1 transporting sorcb.  " initialize w_srcb
  w_srcb   = t_flat-sorcb .


* begin processing download file
  loop at t_flat.

    if w_srcb ne t_flat-sorcb.                "  Next Posting/end of doc
      describe table t_downld lines w_len.
      if w_len ne 0 .
        t_downld-endoft = 'X'.
        incr w_doccnt.
*  change last record to end of transaction 'X'
        modify t_downld index w_len transporting endoft.
        clear t_downld.
      endif.
    endif.

    w_srcb =  t_flat-sorcb.

    CLEAR T_GLAC.

    loop at t_glac where saknr CS t_flat-sapgl.endloop.
    IF SY-SUBRC NE 0. CLEAR T_GLAC. ENDIF.

    CLEAR: W_UMSKZ.
    case t_glac-mitkz.

      when 2.                              "  <<<<<   GL is vendor
*        loop at t_ven where altkn = t_flat-vendor.
*        endloop.                             " <<<<<  get SAP vendor no
        read table t_ven with key altkn = t_flat-vendor.
        IF SY-SUBRC = 0.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
               EXPORTING
                    input  = t_flat-sapgl
               IMPORTING
                    output = G_ACCT
               EXCEPTIONS
                    OTHERS = 1.

          select single UMSKZ from T074 into W_UMSKZ
                      where KTOPL = 'HNA1'
                        AND SKONT = G_ACCT
                        and UMSKZ <> 'F'.


          if sy-subrc = 0.                     "  <<<< SPL/GL a/c found
            if t_flat-drcr = 'C'.
              PERFORM FILL_DOWN using '39' t_ven-lifnr 'A'.
            elseif t_flat-drcr = 'D'.
              PERFORM FILL_DOWN using '29' t_ven-lifnr 'A'.
            endif.
          else.
            if t_flat-drcr = 'C'.
              PERFORM FILL_DOWN using '31' t_ven-lifnr 'A'.
            elseif t_flat-drcr = 'D'.
              PERFORM FILL_DOWN using '21' t_ven-lifnr 'A'.
            endif.
          endif.
        ELSE.
          if t_flat-drcr = 'C'.
            PERFORM FILL_DOWN using '31' t_flat-vendor 'E'.
          elseif t_flat-drcr = 'D'.
            PERFORM FILL_DOWN using '21' t_flat-vendor 'E'.
          endif.

          MOVE-CORRESPONDING T_FLAT TO T_ERR.
          T_ERR-MESSAGE = 'VENDOR NOT IN SAP'.
          APPEND T_ERR. CLEAR T_ERR.

        ENDIF.

      when 1.                               "  <<<<<<  GL is Asset
*        loop at t_ast where invnr = t_flat-assno.
*        endloop.                              "   <<< locate SAP Ast no
        read table t_ast with key invnr = t_flat-assno.
        IF SY-SUBRC = 0.
          if t_flat-drcr = 'C'.
            PERFORM FILL_DOWN using '75' t_ast-anln1 ''.
          elseif t_flat-drcr = 'D'.
            PERFORM FILL_DOWN using '70' t_ast-anln1 ''.
          endif.
        ELSE.
          if t_flat-drcr = 'C'.
            PERFORM FILL_DOWN using '75' t_flat-assno 'E'.
          elseif t_flat-drcr = 'D'.
            PERFORM FILL_DOWN using '70' t_flat-assno 'E'.
          endif.

          MOVE-CORRESPONDING T_FLAT TO T_ERR.
          T_ERR-MESSAGE = 'ASSET NOT IN SAP'.
          APPEND T_ERR. CLEAR T_ERR.
        ENDIF.
      when others.                       " <<<<<<<<<<<other A/Cs
*    write:/ 'Others'.
        if t_flat-drcr = 'C'.
          PERFORM FILL_DOWN using '50' t_flat-SAPGL ''.
        elseif t_flat-drcr = 'D'.
          PERFORM FILL_DOWN using '40' t_flat-SAPGL ''.
        endif.
    endcase.

    clear: t_ven, t_ast,w_umskz, w_endkey.

    incr w_cntr.
  ENDLOOP.

* Add end of transaction to the last record.

  describe table t_downld lines w_len.
  if w_len ne 0.
    t_downld-endoft = 'X'.
    modify t_downld index w_len transporting endoft.
  endif.
* Write the list

  PERFORM WRITE_LIST . " IN PROGRAM ZCVAFBO5.

* Download file
  IF P_LIST NE 'X'.
    PERFORM DOWN_LD.  "  IN PROGRAM ZCVAFBo3.
  ENDIF.

top-of-page.
  perform write_list_hdr.

*---------------------------------------------------------------------*
*       FORM FILL_DOWN                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  EKEY                                                          *
*---------------------------------------------------------------------*
FORM FILL_DOWN USING PKEY acct pblk.

  DATA: W_TEXT(25).
  DATA: W_PKEY(2) VALUE 'BC'.
  CONCATENATE T_FLAT-SORCJ T_FLAT-SORCB INTO W_TEXT.
*  concatenate t_flat-intord '%' into t_flat-intord.

  CLEAR: T_DOWNLD.

* if t_flat-sapgl > 600000.
    if not t_flat-intord is initial.
*      loop at t_into where aufex cs t_flat-intord.
*      endloop.
      read table t_into with key aufex = t_flat-intord.
*     select single aufnr from aufk into aufk-aufnr
*     where aufex = t_flat-intord.
      if sy-subrc = 0.
        t_downld-intord = t_into-aufnr.
      else.
        concatenate t_flat-intord 'Not found' into t_downld-intord.
      endif.
      clear t_into.
    endif.

    if not t_flat-costc is initial.
      t_downld-costcent = t_flat-costc .
    endif.
* endif.

  if t_flat-sapgl CS '113000'.
    t_downld-valdat = t_flat-trandt.
  endif.


  t_downld-slno     =  w_cntr.
  t_downld-Docdt    =  T_FLAT-TRANDT.     " Document date
  t_downld-Postdt   =  T_FLAT-TRANDT.     " Posting date
* t_downld-Refer    =  T_FLAT-CHECK.      " Reference (check no)
  t_downld-Hedrtxt  =  W_TEXT.            " Header text
  t_downld-amount   =  T_FLAT-WRBTR.      " Amount
  t_downld-assign   =  T_FLAT-INVOIS.     " Assignment(inv. number)
  t_downld-doctyp   =  'BC'.
* t_downld-endoft   =  EKEY.           " End of transa marker 'X'
  t_downld-pstkey   =  PKEY.              " Posting key
  t_downld-account  =  acct.        " Bank GL accn no.
  t_downld-text     = t_flat-rfdesc. " Long text


  if t_downld-pstkey = '29' or t_downld-pstkey = '39'.
    t_downld-splldgr  = W_UMSKZ.      " Special ledger indicator
    T_DOWNLD-bldat    = T_FLAT-TRANDT.
  ENDIF.

*  t_downld-payblk   = pblk.          " Payment block
  if t_downld-pstkey = '31' or t_downld-pstkey = '21'.
    t_downld-payblk = 'A'.
    T_DOWNLD-bldat    = T_FLAT-TRANDT.
  else.
    clear t_downld-payblk.
  endif.

  append t_downld.
  clear: t_downld, W_UMSKZ,W_TEXT.
endform.




*---------------------------------------------------------------------*
*       FORM get_file                                                 *
*---------------------------------------------------------------------*
*  F4 help for file names                                             *
*---------------------------------------------------------------------*
*  -->  FLNAME                                                        *
*---------------------------------------------------------------------*
form get_file using flname.

* F4 on filename
  call function 'WS_FILENAME_GET'
       EXPORTING
            mask             = ',*.*,*.*.'
            mode             = 'O'
            title            = 'Select File'
       IMPORTING
            filename         = flname
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            others           = 5.


endform.




*---------------------------------------------------------------------*
*       FORM write_list                                               *
*---------------------------------------------------------------------*
*   Write the contents of the Download file                           *
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

*---------------------------------------------------------------------*
*       FORM write_list                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
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
           /0 SY-VLINE,
           17 t_downld-text, 175 sy-vline.       " blank

    IF T_DOWNLD-ENDOFT = 'X'.
      write:/0 sy-uline(175).
    ENDIF.
  endloop.
  ULINE.
  FORMAT COLOR 7.
  write:/ 'ERRORS IN THE PROCESS'.
  ULINE.
  LOOP AT T_ERR.
    WRITE:/ T_ERR-SORCJ, T_ERR-SORCB, T_ERR-VENDOR, T_ERR-WRBTR,
            T_ERR-INVOIS,T_ERR-SAPGL,T_ERR-ASSNO,T_ERR-MESSAGE.
  ENDLOOP.
  if sy-subrc ne 0.
    skip 3.
    write:/  'No errors reported' color 5.
  endif.
endform.


*---------------------------------------------------------------------*
*       FORM down_ld                                                  *
*---------------------------------------------------------------------*
*       Download the formatted XL file                                *
*---------------------------------------------------------------------*
form down_ld.

  if p_list ne 'X'.

    CALL FUNCTION 'WS_DOWNLOAD'
         EXPORTING
              FILENAME = p_tfname
              FILETYPE = 'DAT'
              MODE     = 'O'
         TABLES
              DATA_TAB = t_downld.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.
endform.
