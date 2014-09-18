REPORT ZC02FIC_APAP no standard page heading message-id 0 line-size 200.

*&---------------------------------------------------------------------*
*& Program Name :  ZC02FIC_APAP
*& Description  :  Checks upload
*
*& Module : FI                                                         *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*& Date        Author           Description/Reason for Change          *
*& ----------  --------------   ------------------------------------   *
*& 02/11/03    Cyril Alex       Initial Development                    *
*&---------------------------------------------------------------------*
*& This program downloads an excel sheet with AP details which can
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
        invnr like anla-invnr,    " Old Asset number
        anln1 like anla-anln1,    " SAP Asset number
      end of t_ast.

* INTERNAL ORDERS
data: begin of T_into occurs 0,
        aufex like aufk-aufex,   " External order number
        aufnr like aufk-aufnr,   " Order Number
      end of t_into.

* Structure of the Flat file
************************************************************************


Data: begin of T_flat occurs 0,
*         SLNO(6),       " **sl. no   - serial number **
         ACCTNO(15),    " Account number
         SAPGL(10),     " SAP GL Account number
         ACCTDES(40),   " A/C  description
         COSTC(10),      " Cost Center
         INTORD(10),     " Internal Order
         ASSNO(15),     " Asset number
         TRANDT(10),    " Transaction date
         SORCJ(2),      " **Source Journal **
         SORCB(6),      " ** Source batch
         srcsyst(2),    " Source system
         RFDESC(40),    " Reference Description
         VENDIV(5),     " ** Div **
         VENDOR(10),    " **Old Vendor Number **
         DOCTYP(1),     " **Inv. type **
         INVOIS(15),    " ** Invoice number
         INVDT(10),     " Document date
         TERMCD(2),     " Term code
         TAXSC(15),     " Tax schedule
         DRCR(2),       " DR/CR indicator
         WRBTR(16),    " WRBTR  : Amount
 end of T_FLAT.

* Duplicate of flat (with Invois as first field)
DATA: BEGIN OF T_FLAT1 OCCURS 0,
*         SLNO(6),       " **sl. no   - serial number **
         INVOIS(15),    " ** Invoice number
         VENDOR(10),    " **Old Vendor Number **
         ACCTNO(15),    " Account number
         SAPGL(15),     " SAP GL Account number
         ACCTDES(40),   " A/C  description
         COSTC(10),      " Cost Center
         INTORD(10),     " Internal Order
         ASSNO(15),     " Asset number
         TRANDT(10),    " Transaction date
         SORCJ(2),      " **Source Journal **
         SORCB(6),      " ** Source batch
         srcsyst(2),    " Source system
         RFDESC(40),    " Reference Description
         VENDIV(5),     " ** Div **
         DOCTYP(1),     " **Inv.   type **
         INVDT(10),     " Document date
         TERMCD(6),     " Term code
         TAXSC(15),     " Tax schedule
         DRCR(2),       " DR/CR indicator
         WRBTR(16),    " WRBTR  : Amount
END OF T_FLAT1.

* work area for t_FLAT
DATA WA_FLAT LIKE T_FLAT.

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
       splldgr(10),                 " Spl ledger
       amount(16),                 " Amount
       taxcode(3) value ' ',       " Blank
       taxamt(1) value ' ',        " Blank
       costcent(10)  value ' ',     " blank
       intord(12) value ' ',        " blank internal order
       bldat(10) value ' ',         " blank baseline date
       valdat(10) value ' ',        " value date
       payt(6) value ' ',          " blank payment term
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
       w_tabix like sy-tabix,        " Table position
       w_invois like t_flat-invois,  " Invoice no.
       W_AMOUNT LIKE BSEG-WRBTR,    " Amount for total
       W_AMT LIKE BSEG-WRBTR,       " Amount for total
       W_INVDT LIKE T_FLAT-INVDT,   " Invoice date
       W_VENDOR LIKE T_FLAT-VENDOR, " Vendor
       W_FLET,                      " FIRST LETTER OF GL A/C
       W_PMTERM(2),                " PAYMENT TERM
       w_taxcd(3),                 " Tax code

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
* value help for Parameters
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

  call functioN 'WS_UPLOAD'
       EXPORTING
            filename = p_flname
            filetype = 'DAT'
       TABLES
            data_tab = t_flat.

* Clear Quotes from Amount field
  loop at t_flat.
    do 2 times.
      replace '"' with '' into t_flat-wrbtr.
    enddo.
    modify t_flat.
  endloop.

* INSERT INVOICE NUMBER and date WHEN NOT AVAILABLE
  loop at t_flat.
    w_invois = T_FLAT-INVOIS.
    W_INVDT  = T_FLAT-INVDT.

   LOOP AT T_FLAT WHERE TRANDT = T_FLAT-TRANDT AND SORCB = T_FLAT-SORCB.

      IF T_FLAT-INVOIS IS INITIAL
         AND T_FLAT-RFDESC CS W_INVOIS.
        T_FLAT-INVOIS = W_INVOIS.
        T_FLAT-INVDT  = W_INVDT.
        MODIFY T_FLAT.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

* copy contents of T_flat into t_flat1
  LOOP AT T_FLAT.
    MOVE-CORRESPONDING T_FLAT TO T_FLAT1.
    APPEND T_FLAT1.

  ENDLOOP.

* delete contents of T_flat
  REFRESH T_FLAT.

  SORT T_FLAT1 BY TRANDT SORCB INVOIS VENDOR ASCENDING.

  LOOP AT T_FLAT1 WHERE INVOIS IS INITIAL.
    delete t_flat1.
  ENDLOOP.

* process T-flat1 and reconstruct t_flat
  loop at t_flat1.
* write:/ t_flat1.

    MOVE-CORRESPONDING T_FLAT1 TO T_FLAT.
    CLEAR: T_FLAT-TERMCD.

    APPEND T_FLAT.
    MOVE-CORRESPONDING T_FLAT1 TO WA_FLAT.

    IF W_VENDOR IS INITIAL.
      W_VENDOR = T_FLAT1-VENDOR.
    ENDIF.

    IF W_PMTERM is initial.
      w_pmterm = t_flat1-termcd.
    endif.

    W_AMT    = T_FLAT1-WRBTR.
    IF T_FLAT1-DRCR = 'C'.
      W_AMOUNT = W_AMOUNT - W_AMT.
    ELSE.
      W_AMOUNT = W_AMOUNT + W_AMT.
    ENDIF.

*    AT END OF INVOIS.
    AT END OF VENDOR.

*        WRITE:/ T_FLAT1-ACCTNO, T_FLAT1-INVOIS,T_FLAT1-SORCB, W_AMOUNT.

      MOVE-CORRESPONDING WA_FLAT TO T_FLAT.
      IF T_FLAT1-DRCR  = 'C'.
        T_FLAT-DRCR  = 'D'.
      ELSE.
        T_FLAT-DRCR  = 'C'.
      ENDIF.

      T_FLAT-WRBTR  = W_AMOUNT.
      T_FLAT-VENDOR = W_VENDOR.
      t_flat-termcd = w_pmterm.
      T_FLAT-ACCTNO  = 'VEN'.
      CLEAR T_FLAT-sapgl.
      APPEND T_FLAT.
      CLEAR: T_FLAT,T_FLAT1.
      CLEAR: W_AMOUNT, W_AMT, W_VENDOR, w_pmterm.
    ENDAT.

  ENDLOOP.


*  LOOP AT T_FLAT.
*    WRITE:/ T_FLAT-SAPGL,T_FLAT-INVOIS, T_FLAT-DRCR, T_FLAT-VENDOR,
*    T_FLAT-WRBTR.
*  ENDLOOP.


* select SAP vendor numbers against the old vendor numbers
 select lifnr altkn  from lfb1 into corresponding fields of table t_ven.
  sort t_ven.
* select SAP Asset numbers against old asset numbers
  select anln1 invnr from anla into corresponding fields of table t_ast.
  Sort t_ast.
* select SAP Internal Orders against old Internal order numbers
  select aufnr aufex from aufk into (aufk-aufnr, aufk-aufex).
    write aufk-aufnr to t_into-aufnr.
    write aufk-aufex to t_into-aufex.
    append t_into.
  endselect.
  sort t_into.
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



  loop at t_flat.
*WRITE:/ T_FLAT.
    CLEAR T_GLAC.
    loop at t_glac where saknr CS t_flat-sapgl.endloop.
    IF SY-SUBRC NE 0. CLEAR T_GLAC. ENDIF.
    IF T_FLAT-ACCTNO CS 'VEN'. t_glac-mitkz = 2.ENDIF.
*    write:/ t_glac-mitkz.
    case t_glac-mitkz.

      when 2.                              "  <<<<<   GL is vendor
*      read table t_ven with key altkn = t_flat-vendor.
*        loop at t_ven where altkn = t_flat-vendor.
*        endloop.                             " <<<<<  get SAP vendor no
        read table t_ven with key altkn = t_flat-vendor.
        IF SY-SUBRC = 0.
          CONCATENATE '0000' t_flat-sapgl INTO t_flat-sapgl.
          select single UMSKZ from T074 into W_UMSKZ
                        where KTOPL = 'HNA1'
                          AND SKONT = t_flat-sapgl
                          and umskz <> 'F'.
*   WRITE:/ t_flat-sapgl.
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
            PERFORM FILL_DOWN using '31' t_ven-lifnr 'E'.
          elseif t_flat-drcr = 'D'.
            PERFORM FILL_DOWN using '21' t_ven-lifnr 'E'.
          endif.

          MOVE-CORRESPONDING T_FLAT TO T_ERR.
          T_ERR-MESSAGE = 'VENDOR NOT IN SAP'.
          APPEND T_ERR. CLEAR T_ERR.

        ENDIF.

      when 1.                               "  <<<<<<  GL is Asset
*        loop at t_ast where invnr CS t_flat-assno.
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
            PERFORM FILL_DOWN using '75' t_ast-anln1 'E'.
          elseif t_flat-drcr = 'D'.
            PERFORM FILL_DOWN using '70' t_ast-anln1 'E'.
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

  loop at t_downld where taxcode cs 'U'.
    w_taxcd = t_downld-taxcode.
    loop at t_downld where assign = t_downld-assign.
      t_downld-taxcode  = w_taxcd.
      modify t_downld.
    endloop.
  endloop.

*WRITE:/ 'BEFORE'.
*PERFORM WRITE_LIST.

  loop at t_downld. " where endoft ne 'X'.
*w_len = sy-tabix + 1.

    if t_downld-endoft = 'X'.

      case w_pstkey.
        when '39' or '31' or '75' or '50'.
*       IF  T_DOWNLD-SPLLDGR IS INITIAL.
          t_downld-pstkey = '21'.
*        ELSE.
*        t_downld-pstkey = '29'.
*        ENDIF.
*        t_downld-taxcode = w_pstkey.
        when '29' or '21' or '70' or '40'.
*     IF  T_DOWNLD-SPLLDGR IS INITIAL.
          t_downld-pstkey = '31'.
*        ELSE.
*        t_downld-pstkey = '39'.
*      ENDIF.
*        t_downld-taxcode = w_pstkey.
      endcase.
      clear t_downld-taxcode.
      modify t_downld.
    else.
      if t_downld-account = '217049' or
         t_downld-account = '217059' or
         t_downld-account = '217069' or
         t_downld-account = '602220' .
      else.

        w_pstkey = t_downld-pstkey.
      endif.
    endif.
  endloop.

  loop at t_downld.
    case t_downld-pstkey.
      when '31' or '21' or '39' or '29'.
        t_downld-bldat = t_downld-docdt.
      when others.
        clear t_downld-bldat.
    endcase.

    if t_downld-pstkey = '31' or t_downld-pstkey = '21'.
      t_downld-payblk = 'A'.
    else.
      clear t_downld-payblk.
    endif.
    modify t_downld.

  endloop.

*WRITE:/ 'AFTER'.
  PERFORM WRITE_LIST . " IN PROGRAM ZCVAFBO3.
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

  CONCATENATE T_FLAT-SORCJ T_FLAT-SORCB INTO W_TEXT.

  if t_flat-invdt is initial.
    t_flat-invdt = t_flat-trandt.
  endif.
  if NOT t_flat-ACCTNO CS 'VEN'. " AND t_flat-sapgl > 600000.
    if not t_flat-intord is initial.

      read table t_into with key aufex =  t_flat-intord.
      if sy-subrc = 0.
        t_downld-intord = t_into-aufnr.
      else.
        t_downld-intord = 'Not found'.
      endif.
      clear t_into.
    endif.

    if not t_flat-costc is initial.
      t_downld-costcent = t_flat-costc .
    endif.
  endif.

  if t_flat-sapgl CS '113000'.
    t_downld-valdat = t_flat-trandt.
  endif.


  if t_flat-ACCTNO = 'VEN'.
    t_downld-endoft = 'X'.
    INCR W_DOCCNT.
  endif.

  REPLACE '-' WITH '' INTO T_FLAT-WRBTR.

  t_downld-slno     =  w_cntr.
  t_downld-Docdt    =  T_FLAT-invdt.     " Document date
  t_downld-Postdt   =  T_FLAT-TRANDT.     " Posting date
  t_downld-Refer    =  T_FLAT-invois.      " Reference (invoice)
  t_downld-Hedrtxt  =  W_TEXT.            " Header text
  t_downld-amount   =  T_FLAT-WRBTR.      " Amount
  t_downld-assign   =  T_FLAT-INVOIS.     " Assignment(inv. number)
  t_downld-doctyp   =  'BC'.
*t_downld-endoft   =  EKEY.           " End of transa marker 'X'
  t_downld-pstkey   =  PKEY.              " Posting key
  t_downld-account  =  acct.        " Bank GL accn no.
  t_downld-splldgr  = W_UMSKZ.      " Special ledger indicator
  t_downld-text     = t_flat-rfdesc. " Long text
  t_downld-payblk   = pblk.          " Payment block

*read table t_ven with key altkn = t_flat-vendor.
*IF SY-SUBRC = 0.
*      t_downld-bldat    =  t_flat-invdt.
*      ENDIF.
  IF T_flat-ACCTNO = 'VEN'.

    case t_flat-termcd.
      when '00'.
        t_downld-payt = '0N00'.
      when '01'.
        t_downld-payt = '1N10'.
      when '10'.
        t_downld-payt = 'N010'.
      when '15'.
        t_downld-payt = 'N015'.
      when '20'.
        t_downld-payt = 'N020'.
      when '25'.
        t_downld-payt = 'N025'.
      when '30'.
        t_downld-payt = 'N030'.
      when '45'.
        t_downld-payt = 'N045'.
    ENDCASE.
  ENDIF.

  IF  NOT T_FLAT-TAXSC IS INITIAL.
    CASE T_FLAT-TAXSC.
      WHEN 'NONTAX'.
        T_DOWNLD-TAXCODE = 'U0'.
      WHEN 'MTG CTY'.
        T_DOWNLD-TAXCODE = 'U1'.

      WHEN OTHERS.
        T_DOWNLD-TAXCODE = 'U2'.
    ENDCASE.
  ENDIF.

  append t_downld.
  clear: t_downld, W_UMSKZ,W_TEXT.
endform.




*---------------------------------------------------------------------*
*       FORM get_file                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
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
           /0 SY-VLINE,
           17 t_downld-text, 175 sy-vline.       " blank
*   endif.
    IF T_DOWNLD-ENDOFT = 'X'.
      write:/0 sy-uline(175).
    ENDIF.
  endloop.
  ULINE.
  FORMAT COLOR 7.
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
*       ........                                                      *
*---------------------------------------------------------------------*
form down_ld.

  if p_list ne 'X'.

    CALL FUNCTION 'WS_DOWNLOAD'
     EXPORTING
*      BIN_FILESIZE                  = ' '
*      CODEPAGE                      = ' '
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
