*----------------------------------------------------------------------*
* Program ID        : ZAFIU134
* Title             : Print Account Summary Document - Excel
* Created on        : 04/13/2009
* Created by        : IG. Moon
* Specifications By : Andy Choi
* Description       : Print Summary Accounting Document via Excel
*----------------------------------------------------------------------*
report zafiu134     message-id zmfi
                    no standard page heading
                    line-size   130   line-count   90.

include: <symbol>.
************************************************************************
************************* Global data **********************************
************************************************************************
tables: t001,    "Company code
        t005,    "Countries

        ska1, skb1,
        bsec,
        t041c,
        t007s,   "Tax Code Names
        bkpf,    "Accounting Document Header
        vbkpf,   "Document Header for Document Parking
        bseg,    "Accounting Document Segment
        vbsegs,  "Document Segment for G/L Accounts Document Parking
        vbsegk,  "Document Segment for Vendor Document Parking
        vbsegd,  "Document Segment for Customer Document Parking
        vbsega,  "Document Segment for Assets Document Parking
        vbset,   "Document Segment for Taxes Document Parking
        usr21,   "Assign User Name Address Key
        adcp,    "Person/ Address Assignment
        anla,    "Asset Master Record Segment
        kna1,    "General Data in Customer Master
        lfa1,    "Vendor Master (General Section)
        tgsbt,   "Business Area Names
        skat,    "G/L Account Master Record
        aufk,    "Order Master Data
        csks,    "Cost Center Master Data
        cskt,    "Cost Center Texts
        makt,    "Material Text
        fmfctrt, "Funds Center Texts
        mseg,    "Material document
        rbkp,    "Invoice document
        ekko, ekkn,t008,lfbw,t059fb,t059z.

*******************************************************************
************************** Global data ****************************
*******************************************************************

data: begin of it_bkpf occurs 0,
      blart    like  bkpf-blart,       "Doc. Type.
      budat    like  bkpf-budat,       "Posting Date
      bldat    like  bkpf-bldat,       "doc. date
      cpudt    like  bkpf-cpudt,       "Enrry data
      usnam    like  bkpf-usnam,       "User Name
      ppnam    like  bkpf-ppnam,       "Parked by
      waers    like  bkpf-waers,       "Currency key
      tcode    like  bkpf-tcode,       "Transaction Code
      xblnr    like  bkpf-xblnr,       "Reference
      bstat    like  bkpf-bstat,       "Doc. Status
      depart   like  adcp-department,  "Dept Name
      xprfg    like  vbkpf-xprfg,      "Doc. Complete
      stblg    like  bkpf-stblg,       "Reverse doc no
      stjah    like  bkpf-stjah,       "Reverse year
      stgrd    like  bkpf-stgrd,       "Reverse reason
      bukrs    like  bkpf-bukrs,       "Company Code
      gjahr    like  bkpf-gjahr,       "Fiscal Year
      belnr    like  bkpf-belnr,       "Doc. No.
      awtyp    like  bkpf-awtyp,       "Reference procedure
      awkey    like  bkpf-awkey,       "Objectkey
      revtx(7),                        "Reverse
      cputm    like  bkpf-cputm,
      end of it_bkpf.

* BSEG : G/L Data segment
data: begin of it_bseg occurs 0,
      sortk(10) type c,
      bukrs    like  bkpf-bukrs,
      gjahr    like  bkpf-gjahr,
      belnr    like  bkpf-belnr,
      buzei    like  bseg-buzei,   "Line Item
      lifnr    like  bseg-lifnr,   "Vendor
      tabix    type i,
      ppage    type i,
      xblnr    like  bkpf-xblnr,

      revtx(7) type c,
      stgrd    like  bkpf-stgrd,   "Reverse reason
      empfb    like  bseg-empfb,   "payer/payee
      hkont    like  bseg-hkont,   "G/L Account
      txt20    like  skat-txt20,   "Short Text
      bschl    like  bseg-bschl,   "Posting Key
      accnt    like  bseg-kunnr,   "C/V/A
      name1    like  kna1-name1,   "C/V/A Name
      aufnr    like  bseg-aufnr,   "Order No.
      fistl    like  bseg-fistl,   "Funds Center
      kostl    like  bseg-kostl,   "Cost Center
      fkber    like  bseg-fkber,   "Func
      ktext    like  cepct-ktext,  "Cost Obj Text
      zuonr    like  bseg-zuonr,   "Assignment
      umskz    like  bseg-umskz,   "Sp. G/L Ind.
      sgtxt    like  bseg-sgtxt,   "Text
      mwskz    like  vbsega-mwskz, "Tax Code
      fwbas    like  bseg-fwbas,
      txjcd    like  vbsega-txjcd, "Jurisdiction
      bupla    like  bseg-bupla,   "Biz Place (KR)
      jrtxt    like  ttxjt-text1,  "Tax Name
      wmwst    like  vbsegd-wmwst, "Tax Amt
      dmbtr    like  bseg-dmbtr,   "Amt in Loc. Cur.
      s_gumak  like  bseg-dmbtr,   "Debit Amt
      h_gumak  like  bseg-dmbtr,   "Credit Amt
      shkzg    like  bseg-shkzg,   "Dr/Cr
      kunnr    like  bseg-kunnr,   "Customer
      anln1    like  bseg-anln1,   "Asset
      anln2    like  bseg-anln2,   "Asset sub
      anbwa    like  bseg-anbwa,   "Asset Transaction(3)
      koart    like  bseg-koart,   "Account Type
      saknr    like  bseg-saknr,   "G/L Acct
      valut    like  bseg-valut,   "Value Date
      zterm    like  bseg-zterm,   "Pay Term
      zlsch    like  bseg-zlsch,   "Pay Method
      zlspr    like  bseg-zlspr,   "Pay Block
      augbl    like  bseg-augbl,   "Clearing Doc
      pswsl    like  bseg-pswsl,   "Currency
      zfbdt    like  bseg-zfbdt,   "
      fdtag    like  bseg-fdtag,
      dmbth    like  bseg-dmbtr,
      dmbts    like  bseg-dmbtr,
      matnr    like  bseg-matnr,   "Material#
      menge    like  bseg-menge,   "qty
      meins    like  bseg-meins,   "unit
      ebeln    like  bseg-ebeln,   "PO#
      ebelp    like  bseg-ebelp,   "poitem
      buzid    like  bseg-buzid,   "internal key
      ktosl    like  bseg-ktosl,   "internal acct key
    end of it_bseg.

data: begin of it_key occurs 0,
      sortk(10) type c,
      xblnr    like  bkpf-xblnr,
    end of it_key.

data: begin of it_tmp_file occurs 0,
      tmp_file like rlgrap-filename,
      end of it_tmp_file.

data: begin of it_bseg_sum_v  occurs 0,
      bukrs    like  bkpf-bukrs,
      gjahr    like  bkpf-gjahr,
      lifnr    like  bseg-lifnr,   "Vendor
      belnr    like  bkpf-belnr,
      buzei    like  bseg-buzei,   "Line Item
      hkont    like  bseg-hkont,   "G/L Account
      txt20    like  skat-txt20,   "Short Text
      bschl    like  bseg-bschl,   "Posting Key
      accnt    like  bseg-kunnr,   "C/V/A
      name1    like  kna1-name1,   "C/V/A Name
      aufnr    like  bseg-aufnr,   "Order No.
      fistl    like  bseg-fistl,   "Funds Center
      kostl    like  bseg-kostl,   "Cost Center
      ktext    like  cepct-ktext,  "Cost Obj Text
      zuonr    like  bseg-zuonr,   "Assignment
      umskz    like  bseg-umskz,   "Sp. G/L Ind.
      sgtxt    like  bseg-sgtxt,   "Text
      mwskz    like  vbsega-mwskz, "Tax Code
      fwbas    like  bseg-fwbas,
      txjcd    like  vbsega-txjcd, "Jurisdiction
      bupla    like  bseg-bupla,   "Biz Place (KR)
      jrtxt    like  ttxjt-text1,  "Tax Name
      wmwst    like  vbsegd-wmwst, "Tax Amt
      dmbtr    like  bseg-dmbtr,   "Amt in Loc. Cur.
      s_gumak  like  bseg-dmbtr,   "Debit Amt
      h_gumak  like  bseg-dmbtr,   "Credit Amt
      shkzg    like  bseg-shkzg,   "Dr/Cr
      kunnr    like  bseg-kunnr,   "Customer
      anln1    like  bseg-anln1,   "Asset
      anln2    like  bseg-anln2,   "Asset sub
      anbwa    like  bseg-anbwa,   "Asset Transaction(3)
      koart    like  bseg-koart,   "Account Type
      saknr    like  bseg-saknr,   "G/L Acct
      valut    like  bseg-valut,   "Value Date
      zterm    like  bseg-zterm,   "Pay Term
      zlsch    like  bseg-zlsch,   "Pay Method
      zlspr    like  bseg-zlspr,   "Pay Block
      augbl    like  bseg-augbl,   "Clearing Doc
      pswsl    like  bseg-pswsl,   "Currency
      zfbdt    like  bseg-zfbdt,   "
      fdtag    like  bseg-fdtag,
      dmbth    like  bseg-dmbtr,
      dmbts    like  bseg-dmbtr,
      matnr    like  bseg-matnr,   "Material#
      menge    like  bseg-menge,   "qty
      meins    like  bseg-meins,   "unit
      ebeln    like  bseg-ebeln,   "PO#
      buzid    like  bseg-buzid,   "internal key
      ktosl    like  bseg-ktosl,   "internal acct key
      end of it_bseg_sum_v.

data: begin of it_info,
        info1(20),      " account short text
        info2(20),
        info3(16),
        info4(5),
        taxinfo(2),

        assign1(18),
        assign2(10),
        assign3(4),

        rate(4),
        ordno(30),
        text(48),
        date(12),
        dr  like bseg-dmbtr,
        cr  like bseg-dmbtr,
        drt  like bseg-dmbtr, "tax
        crt  like bseg-dmbtr, "tax
        name1 like lfa1-name1,
        maktx like makt-maktx,
        code(20),
    end of it_info.

data : it_t041c like t041c occurs 0 with header line.

data :  wa_sdmbth_g   like bseg-dmbtr,
        wa_sdmbts_g   like bseg-dmbtr,
        wa_sdmbth_d   like bseg-dmbtr,
        wa_sdmbts_d   like bseg-dmbtr,
        wa_sdmbth_t   like bseg-dmbtr,
        wa_sdmbts_t   like bseg-dmbtr,
        wa_ttext      like tstct-ttext,
        wa_scnt_g     type i,
        wa_hcnt_g     type i,
        wa_total_page type i,
        wa_total_page1 type i,
        wa_first_page  type i,
        wa_revtx(7),
        wa_txt10(10)      type c.
*
data: stripes  type c value ' ',        " Stripes Y/N
      keys     type c value ' ',        " Key columns Y/N
      wa_width type i value 130,
      wa_ul  like sy-uline value '-',
      wa_vl  like sy-vline value '|',
      wa_sp  like space    value ' ',
      l_s(1) type c value ';'.

type-pools: vrm.
data: name  type vrm_id,
      list  type vrm_values,
      value like line of list.

data: wa_l_name1(40), wa_l_name2(40), wa_l_company_name(80).
data : end_line,
       f_num type i.

************************************************************************

data tmp_dir like rlgrap-filename.
data tmp_file like rlgrap-filename.

data: begin of new_dir occurs 0,
       $index  like sy-tabix,
       dirname like rlgrap-filename,
      end of new_dir.

include ole2incl.
include <icon>.

data: excelsheet type ole2_object,
      excel  type ole2_object,
      workbooks  type ole2_object,
      open_workbooks  type ole2_object,
      close_workbooks  type ole2_object,
      sheet  type ole2_object,
      cells  type ole2_object,
      subrc like sy-subrc,
      exl_activate type ole2_object,
      exl_print type ole2_object,
      exl_preview type ole2_object,
      exl_quit type ole2_object,
      exl_saveas type ole2_object,
*      exl_merge TYPE ole2_object,
      e_work type ole2_object.",
*      activesheet TYPE ole2_object,
*      oleobjects type  ole2_object.

data out_times type i.
data pri_fr_line type i.
data pri_to_line type i.
data x_lin type i.
data answer.


data : begin of it_dupinv occurs 0,
       bukrs like bkpf-bukrs,
       belnr like bkpf-belnr,
       gjahr like bkpf-gjahr,
       end of it_dupinv.

data : begin of it_dupinvh occurs 0,
         belnr like bseg-belnr,
         dmbtr like bseg-dmbtr,
         bschl like bseg-bschl,
         koart like bseg-koart,
       end of it_dupinvh.

data: it_dupinvh1 like table of it_dupinvh with header line.
data: g_curr_page type i.

****************************** constants ******************************
constants:  false value ' ',
            true  value 'X'.
****************************** macros *********************************
define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.

define __dos.                          " clear & refresh
  append dos_command.clear dos_command.
end-of-definition.

*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------

parameter     p_bukrs  like bkpf-bukrs memory id buk.

select-options:
  s_belnr  for   bkpf-belnr," memory id bln,
  s_gjahr  for   bkpf-gjahr memory id gjr.

*SELECTION-SCREEN SKIP.

*SELECTION-SCREEN skip.

selection-screen begin of line.
selection-screen comment 1(10) text-040 for field r_ven.
parameter r_ven radiobutton group r_gr default 'X'. " Vendor
selection-screen comment 30(10) text-050 for field r_cus.
parameter r_cus radiobutton group r_gr.         " Cust.(not sup)
selection-screen comment 50(10) text-060 for field r_nor.
parameter r_nor radiobutton group r_gr.         " Normal
selection-screen end of line.

selection-screen begin of block b0 with frame title text-010.

select-options:
  s_blart  for   bkpf-blart memory id bar,
  s_budat  for   bkpf-budat,
  s_xblnr  for   bkpf-xblnr,
  s_cpudt  for   bkpf-cpudt.

selection-screen end of block b0.


selection-screen begin of block b1 with frame title text-020.
parameter:  p_own    type c as checkbox default 'X',
            p_detail type c as checkbox,
            p_exrev  as checkbox.

select-options:
  s_zlspr  for t008-zahls,
  s_lifnr  for bseg-lifnr.

selection-screen end of block b1.

selection-screen begin of block b3 with frame title text-021.
parameters : pr_dest like pri_params-pdest,
             pr_imm type c.
selection-screen end of block b3.

selection-screen comment 3(40) text-c01.
parameter : p_parked as checkbox default ' ',
            p_limit  type i  default '2000'.

selection-screen begin of block b4 with frame title text-023.
selection-screen begin of line.
selection-screen comment 1(19) text-p50 for field p_tp2.
parameter p_tp2 radiobutton group r_tp default 'X'.

selection-screen comment 41(24) text-p40 for field p_tp1.
parameter p_tp1 radiobutton group r_tp.
selection-screen end of line.
parameters  filename(50) default 'C:\SapXls\IV2_template.xls'
no-display.
*PARAMETERS p_scr AS CHECKBOX.
data p_scr value space.

parameters nex default '' no-display.
selection-screen end of block b4.

selection-screen begin of block b5 with frame title text-025.
selection-screen begin of line.
selection-screen comment 1(20) tx_bart.
selection-screen comment 22(4) tx_bar.
selection-screen position 33.
parameters p_barb like toacm_c-ar_object default 'ZFIIV'.
selection-screen end of line.
selection-screen end of block b5.

parameters p_save default ''.
parameters p_view default ''.

*SELECTION-SCREEN BEGIN OF BLOCK bp WITH FRAME TITLE text-p10.
*SELECTION-SCREEN END OF BLOCK bp.

*-------------------------------------------------------*
at selection-screen.
*-------------------------------------------------------*
  if p_bukrs is initial.
    message e000(zmfi) with 'Company code is not defined'.
  endif.

  if s_gjahr-low is initial.
    message w000(zmfi) with 'Fiscal year is not defined'.
  endif.

*-------------------------------------------------------*
at selection-screen output.
*-------------------------------------------------------*
** Authority check
  authority-check object 'Z_BKPF_BES' id 'BRGRU'
                  field '*'.
  if sy-subrc <> 0.
    authority-check object 'Z_BKPF_BES' id 'BRGRU'
                    field 'FI'.
    if sy-subrc <> 0.
      p_own = 'X'.
      loop at screen.
        if screen-name = 'P_OWN'.
          screen-input =  0.
          modify screen.
        endif.
        if screen-name = 'P_SAVE'.
          if sy-uname eq '103569'.
            screen-invisible = 0.
          else.
            screen-invisible = 1.
          endif.
          modify screen.
        endif.
        if screen-name = 'P_VIEW'.
          if sy-uname eq '103569'.
            screen-invisible = 0.
          else.
            screen-invisible = 1.
          endif.
          modify screen.
        endif.
      endloop.
    endif.
  endif.

************************************************************************
*************************** Main Program *******************************
************************************************************************
initialization.

  s_cpudt-low  = sy-datum.
  s_cpudt-high = sy-datum.
  append s_cpudt.
  clear:pr_dest,pr_imm.

  write : icon_binary_document as icon to tx_bar,
         'Bar-Code Object' to tx_bart.

start-of-selection.

  set pf-status 'LIBS1'.

  perform get_company_info.

  perform get_bkpf_data.

  data: l_lines like sy-index.
  describe table it_bkpf lines l_lines.
  if l_lines = 0.
    message s000(zmfi) with 'No data found'.
    exit.
  elseif l_lines > p_limit.
    message s000(zmfi) with 'Too many documents selected'.
    exit.
  endif.

* read detail information.
  perform get_bseg_details.

  perform process_ap_filter.

  perform set_debit_credit.

*  PERFORM print_option.

end-of-selection.

  if p_exrev = 'X'.
    delete it_bseg where revtx <> space.
  endif.

  perform adjust_data.
  perform writing_data.

top-of-page.
  perform heading.

end-of-page.
  perform end_of_page.

at line-selection.
*  IF sy-lisel = 'BELNR'.
  if it_bseg-belnr <> space.
    set parameter id:'BLN' field it_bseg-belnr,
                     'BUK' field p_bukrs,
                     'GJR' field it_bseg-gjahr.
    call transaction 'FB03' and skip first screen.
  endif.

************************************************************************
*************************** Form Routines ******************************
************************************************************************

*---------------------------------------------------------------------*
*       FORM writing_data                                             *
*---------------------------------------------------------------------*
*       List output                                                   *
*---------------------------------------------------------------------*
form writing_data.

  if r_nor = 'X'.
    if p_detail = 'X'.
      sort it_bseg by gjahr belnr buzei.
    else.
      sort it_bseg by gjahr belnr bschl.
    endif.
  elseif r_ven = 'X'.
    if p_detail = 'X'.
      sort it_bseg by sortk gjahr belnr buzei.
    else.
      sort it_bseg by sortk gjahr belnr bschl.
    endif.
  else.
    if p_detail = 'X'.
      sort it_bseg by sortk gjahr belnr buzei.
    else.
      sort it_bseg by sortk gjahr belnr bschl.
    endif.
  endif.

  perform write_body.

  perform draw_sign_box using space.
  skip.
  new-line no-scrolling.


endform.                    "writing_data
*---------------------------------------------------------------------*
*       FORM HEADING                                                  *
*---------------------------------------------------------------------*
*       Writes the heading of the list                                *
*---------------------------------------------------------------------*
form heading.

  format intensified off.
  write :/.
  new-line no-scrolling.
  write: /(130)'S u m m a r y    D o c u m e n t' centered.
  " List title
  skip.
**-> t001: Company Codes
  select single * from t001
         where  bukrs = it_bkpf-bukrs.
  check sy-subrc = 0.
  write:/    'Company code :', it_bkpf-bukrs, '-',  wa_l_company_name,
        98   'Print date : ' no-gap,
              sy-datum  mm/dd/yyyy,
              sy-uzeit  using edit mask '__:__:__'.

  write:/    'Currency : USD'.
  if r_ven eq 'X' or r_cus eq 'X'.
    write:      110 'Page:' no-gap, wa_first_page no-gap,
               '/ '    no-gap,
               (2) wa_total_page1 no-gap left-justified.
  else.
    write:      110 'Page:' no-gap, sy-pagno no-gap,
               '/ '    no-gap,
               (3) wa_total_page no-gap left-justified.
  endif.

*  WRITE :/115 'page : ', (3) sy-pagno no-gap,
*              '/' no-gap, (3) wa_total_page no-gap.
  new-line.
  uline at (wa_width).
  write: / sy-vline no-gap,            " Left border
          (12) 'Document No'   no-gap color col_heading intensified off,
          (03) 'PK'           no-gap color col_heading intensified off,
          (11) 'Account'      no-gap color col_heading intensified off,
*         (03) 'PK'           no-gap color col_heading intensified off,
          (20) 'Text'         no-gap left-justified
                                     color col_heading intensified off,
          (31) 'Info'         no-gap centered
                                     color col_heading intensified off,
          (5)  'Tax'          no-gap right-justified
                                     color col_heading intensified off,
          (22) 'Debit'        no-gap right-justified
                                     color col_heading intensified off,
          (22) 'Credit'       no-gap right-justified
                                     color col_heading intensified off,
          (2) ' '             no-gap right-justified
                                     color col_heading intensified off,
          sy-vline .
  uline at (wa_width).                    " Line below titles


endform.                    "heading

*---------------------------------------------------------------------*
*       FORM WRITE_BODY
*---------------------------------------------------------------------*
*       Writes the body of the list                                   *
*---------------------------------------------------------------------*
* Important:                                                          *
* Use SY-VLINE or '|' for vertival lines.                             *
* Use NO-GAP for avoiding free space behind fields.                   *
* Use FORMAT COLOR COL_NORMAL INTENSIFIED OFF for light list body.    *
* Use COLOR behind fields for individualized coloring.                *
*                                                                     *
* Caution: the actual field values are pure dummies!                  *
*                                                                     *
* Stripes in the list body:                                           *
* If You want a striped list body, You have to set up a counter.      *
* If the counter starts with zero You have to set the body color to:  *
* even counter values: COL_NORMAL INTENSIFIED                         *
* odd counter values: COL_NORMAL INTENSIFIED OFF                      *
*                                                                     *
*---------------------------------------------------------------------*
form write_body.

  data: count   type i value 0,          " Loop counter for list body
        count2  type i value 0,          " Counter for stripes
        help    type i,
        wa_save_belnr like bkpf-belnr,
        wa_blart      like bkpf-blart,
        wa_sgtxt      like bseg-sgtxt,
        wa_sdmbth     like bseg-dmbtr,
        wa_sdmbts     like bseg-dmbtr,
        wa_scnt       type i,
        wa_hcnt       type i.

  data: begin of l,
          txt20(20),      " account short text
          info2(48),
          mwskz,
          rate(4),
          ordno(30),
          assgn1(17),
          assgn2(12),
          text(48),
          date(12),
          dr  like bseg-dmbtr,
          cr  like bseg-dmbtr,
          name1 like lfa1-name1,
          code(20),
      end of l.

  clear : wa_sdmbth_g, wa_sdmbts_g.
  clear : wa_hcnt_g,   wa_scnt_g.

*  stripes = 'X'.
  loop at it_bseg.
    clear it_info.
    if r_nor <> 'X'.  "customer, vendor
      at new sortk.  "lifnr.
        wa_first_page  = 1.
        wa_total_page1 = 1.
        f_num = f_num + 1.
        if f_num >= 85.
          wa_total_page1 = wa_total_page1 + 1.
        endif.
        new-page.
      endat.
    endif.

    at new belnr.
      read table it_bkpf with key belnr = it_bseg-belnr
                                  gjahr = it_bseg-gjahr.
      wa_revtx = it_bkpf-revtx.
    endat.

    perform get_account_text.
    perform get_account_info.
    perform get_tax_rate.

* print document line.................................................
    perform handle_stripe using count2.
    perform write_doc_lines  using wa_save_belnr   wa_blart  wa_revtx.

    if r_nor = 'X'.
      add it_bseg-dmbth to wa_sdmbth.
      add it_bseg-dmbts to wa_sdmbts.
    elseif r_ven = 'X'.
      if it_bseg-koart = 'K' and it_bkpf-revtx is initial.
        add it_bseg-dmbth to wa_sdmbth_g.
        add it_bseg-dmbts to wa_sdmbts_g.

        add it_bseg-dmbth to wa_sdmbth.
        add it_bseg-dmbts to wa_sdmbts.
      endif.
    else.
      if it_bseg-koart = 'D'  and it_bkpf-revtx is initial.
        add it_bseg-dmbth to wa_sdmbth.
        add it_bseg-dmbts to wa_sdmbts.

        add it_bseg-dmbth to wa_sdmbth.
        add it_bseg-dmbts to wa_sdmbts.
      endif.
    endif.

    if it_bseg-dmbth ne 0.
      add 1 to wa_hcnt.
      if it_bkpf-revtx is initial.
        add 1 to wa_hcnt_g.
      endif.
    else.
      add 1 to wa_scnt.
      if it_bkpf-revtx is initial.
        add 1 to wa_scnt_g.
      endif.
    endif.

* print document end.................................................
    at end of belnr.
      perform write_doc_end  using wa_sdmbth
                                   wa_sdmbts
                                   wa_hcnt
                                   wa_scnt.
      clear : wa_sdmbth, wa_sdmbts.
      clear : wa_hcnt, wa_scnt.
    endat.

    add 1 to count.
    add 1 to count2.


    if r_nor <> 'X'.  "customer, vendors
      at end of sortk.  "lifnr.
        do.
          if sy-linno > 54. "26
            exit.
          endif.
          perform write_empty_line.
        enddo.

*** 07/23/2013 - T00306 Start
        perform ckeck_duplicate_invoice.
*** 07/23/2013 - T00306 End

        perform print_vendor_cust_sum.

        perform draw_sign_box using 'X'.
      endat.
    endif.

    hide: it_bseg-belnr, it_bseg-gjahr.

    at last.
      sum.
      wa_sdmbts_g = it_bseg-dmbts.
      wa_sdmbth_g = it_bseg-dmbth.
    endat.

  endloop.

  clear: it_bseg-belnr, it_bseg-gjahr.

endform.                    "write_body

*---------------------------------------------------------------------*
*       FORM SUM_PRINT
*---------------------------------------------------------------------*
*       Writes row with final totals                                  *
*---------------------------------------------------------------------*
form sum_print.
  uline at (wa_width).
  format color col_total intensified.  " Color of final totals
  write : / sy-vline,
          (59) ' ',
          '( Dr:',
          (6) wa_scnt_g no-gap,
          '/ CR:',
          (6) wa_hcnt_g no-gap,
          ')',
          (2) '     ',
          (18) wa_sdmbts_g no-zero,
          (18) wa_sdmbth_g no-zero,
          sy-vline.
  uline at (wa_width).
  format color col_normal intensified off.  " Color of final totals

endform.                    "sum_print

************************************************************************
*************************** Events *************************************
************************************************************************

at user-command.
  case sy-ucomm.

    when 'STRI'.
      sy-lsind = 0.
      if stripes eq 'X'.
        stripes = ' '.
      else.
        stripes = 'X'.
      endif.
      perform heading.
      perform writing_data.

    when 'PRIE'.

      perform excel_form_check.
      if p_tp1 eq false.
        perform print_via_excel_by_ref.
      else.
        perform print_via_excel_normal.
      endif.
    when 'BACK'.
      leave to screen 0.
    when others.

  endcase.
*&------------------------------------------------------*
*&      Form  BSEG_READ_PROCESS
*&------------------------------------------------------*
form bseg_read_process.
  select *
    from bseg
*   appending corresponding fields of table it_bseg
   where bukrs = it_bkpf-bukrs
     and gjahr = it_bkpf-gjahr
     and belnr = it_bkpf-belnr.

    move-corresponding bseg to it_bseg.
    if it_bseg-bschl > 39.
      clear: it_bseg-lifnr, it_bseg-kunnr.

    endif.

    it_bseg-xblnr = it_bkpf-xblnr.
    it_bseg-revtx = it_bkpf-revtx.
    it_bseg-stgrd = it_bkpf-stgrd.

* append/collect
    perform append_collect_bseg.

  endselect.
endform.                    "bseg_read_process

*&------------------------------------------------------*
*&      Form  VBSEGS_READ_PROCESS
*&------------------------------------------------------*
form vbsegs_read_process.
  select *
    from vbsegs
*   APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   where ausbk = it_bkpf-bukrs
     and gjahr = it_bkpf-gjahr
     and belnr = it_bkpf-belnr.
    move-corresponding vbsegs to it_bseg.
    it_bseg-hkont = it_bseg-saknr.
    it_bseg-koart = 'S'.

    it_bseg-xblnr = it_bkpf-xblnr.

* append/collect
    perform append_collect_bseg.

  endselect.
endform.                    "vbsegs_read_process

*&------------------------------------------------------*
*&      Form  VBSEGK_READ_PROCESS
*&------------------------------------------------------*
form vbsegk_read_process.
  select *
    from vbsegk
*    APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   where ausbk = it_bkpf-bukrs
     and gjahr = it_bkpf-gjahr
     and belnr = it_bkpf-belnr.
    move-corresponding vbsegk to it_bseg.
    it_bseg-koart = 'K'.

    it_bseg-xblnr = it_bkpf-xblnr.

* append/collect
    perform append_collect_bseg.

  endselect.

endform.                    "vbsegk_read_process

*&------------------------------------------------------*
*&      Form  VBSEGD_READ_PROCESS
*&------------------------------------------------------*
form vbsegd_read_process.
  select *
    from vbsegd
*   APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   where ausbk = it_bkpf-bukrs
     and gjahr = it_bkpf-gjahr
     and belnr = it_bkpf-belnr.

    move-corresponding vbsegd to it_bseg.
    it_bseg-koart = 'D'.

    it_bseg-xblnr = it_bkpf-xblnr.

* append/collect
    perform append_collect_bseg.

  endselect.

endform.                    "vbsegd_read_process

*&------------------------------------------------------*
*&      Form  VBSEGA_READ_PROCESS
*&------------------------------------------------------*
form vbsega_read_process.
  select *
    from vbsega
*    APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   where ausbk = it_bkpf-bukrs
     and gjahr = it_bkpf-gjahr
     and belnr = it_bkpf-belnr.

    move-corresponding vbsega to it_bseg.
    it_bseg-koart = 'A'.

    it_bseg-xblnr = it_bkpf-xblnr.

    perform append_collect_bseg.

  endselect.

endform.                    "vbsega_read_process


*&---------------------------------------------------------------------*
*&      Form  draw_sign_box
*&---------------------------------------------------------------------*
form draw_sign_box  using f_split.
  if f_split = space.
*---end
    if sy-linno >= 57.
      do.
        if sy-linno > 87  ."56.
          exit.
        endif.
*    WRITE :/.
        perform write_empty_line.
      enddo.
      new-page.
*    PERFORM heading.
    endif.

    do.
      if sy-linno > 54 ."56.
        exit.
      endif.
*    WRITE :/.
      perform write_empty_line.
    enddo.

    perform sum_print.

    format   intensified off.
    new-line. uline at (wa_width).
  endif.

*
*  IF sy-linno > 56.
*    NEW-PAGE.
**    PERFORM heading.
*  ENDIF.
*  DO.
*    IF sy-linno > 56.
*      EXIT.
*    ENDIF.
**    WRITE :/.
*    PERFORM write_empty_line.
*  ENDDO.
*  PERFORM sum.
*  FORMAT   INTENSIFIED OFF.
*  NEW-LINE. ULINE AT (wa_width).

*
*---2003/09/08 jhs modify Requestor

* Begin of changes - UD1K918802
* Footer changes
*  write:/  wa_vl no-gap, (11)  space    no-gap centered,
*           wa_vl no-gap, (40)  'Name'   no-gap centered,
*           wa_vl no-gap, (40)  'Signature'   no-gap centered,
*           wa_vl no-gap, (34)  'Date'   no-gap centered,
*           wa_vl no-gap.

*  new-line. uline at (wa_width).

*  write:/  wa_vl no-gap, (11) 'Requestor'    no-gap centered,
*           wa_vl no-gap, (40) space no-gap centered,
*           wa_vl no-gap, (40) space no-gap centered,
*           wa_vl no-gap, (34) space no-gap centered,
*           wa_vl no-gap.

*  write:/  wa_vl no-gap, (11) space no-gap centered,
*           wa_vl no-gap, (40) space no-gap centered,
*           wa_vl no-gap, (40) space no-gap centered,
*           wa_vl no-gap, (34) space no-gap centered,
*           wa_vl no-gap.
*
*  write:/  wa_vl no-gap, (11) space no-gap centered,
*           wa_vl no-gap, (40) space no-gap centered,
*           wa_vl no-gap, (40) space no-gap centered,
*           wa_vl no-gap, (34) space no-gap centered,
*           wa_vl no-gap.

  write:/  wa_vl no-gap, (35)  'Document Processing Warnings:'
            no-gap centered,
             (93)  space       no-gap centered,
           wa_vl no-gap.


  write:/  wa_vl no-gap, (128)  space
            no-gap centered,
           wa_vl no-gap.


  write:/  wa_vl no-gap, (128) space    no-gap centered,
           wa_vl no-gap.
  write:/  wa_vl no-gap, (128) space    no-gap centered,
           wa_vl no-gap.
  write:/  wa_vl no-gap, (128) space    no-gap centered,
           wa_vl no-gap.
* End of changes - UD1K918802

  new-line. uline at (wa_width).

  new-line. uline at (wa_width).

*--------Department approval  date finance approval date
  write:/  wa_vl no-gap, (22)  'Req.Department'   no-gap centered,
           wa_vl no-gap, (29)  'Approval'     no-gap centered,
           wa_vl no-gap, (12)  'Date'         no-gap centered,
           wa_vl no-gap, (20)  'Finance Div.'      no-gap centered,
           wa_vl no-gap, (29)  'Approval'     no-gap centered,
           wa_vl no-gap, (11)  'Date'         no-gap centered,
           wa_vl no-gap.
  new-line. uline at (wa_width).

*---------------------Head of department
  write:/  wa_vl no-gap, (22)  space   no-gap centered,
           wa_vl no-gap, (29)  space   no-gap centered,
           wa_vl no-gap, (12)  space   no-gap centered,
           wa_vl no-gap, (20)  space   no-gap centered,
           wa_vl no-gap, (29)  space   no-gap centered,
           wa_vl no-gap, (11)  space   no-gap centered,
           wa_vl no-gap.
*

*write:/wa_vl no-gap, (22)'Head of Department'no-gapcentered,UD1K918802
  write:/  wa_vl no-gap, (22)  'Requestor'   no-gap centered,
           wa_vl no-gap, (29)  space    no-gap centered,
           wa_vl no-gap, (12)  space    no-gap centered,
*           wa_vl no-gap, (20)  'Cost Accounting'  no-gap centered,
           wa_vl no-gap, (20)  'Processor' no-gap centered,
           wa_vl no-gap, (29)  space     no-gap centered,
           wa_vl no-gap, (11)  space     no-gap centered,
           wa_vl no-gap.
*
*  write:/  wa_vl no-gap, (22)  space   no-gap centered,UD1K918802
  write:/  wa_vl no-gap, (22)  space  no-gap centered,
          wa_vl no-gap, (29)  space   no-gap centered,
          wa_vl no-gap, (12)  space   no-gap centered,
          wa_vl no-gap, (20)  space   no-gap centered,
          wa_vl no-gap, (29)  space   no-gap centered,
          wa_vl no-gap, (11)  space   no-gap centered,
          wa_vl no-gap.
  new-line. uline at (wa_width).
*---------------------Head of sub division
  write:/  wa_vl no-gap, (22)  space   no-gap centered,
           wa_vl no-gap, (29)  space   no-gap centered,
           wa_vl no-gap, (12)  space   no-gap centered,
           wa_vl no-gap, (20)  space   no-gap centered,
           wa_vl no-gap, (29)  space   no-gap centered,
           wa_vl no-gap, (11)  space   no-gap centered,
           wa_vl no-gap.
*
*write:/wa_vl no-gap,(22)'Head of Sub Division'no-gapcentered,UD1K918802
  write:/  wa_vl no-gap, (22)  'Head of Department'   no-gap centered,
               wa_vl no-gap, (29)  space    no-gap centered,
               wa_vl no-gap, (12)  space    no-gap centered,
*             wa_vl no-gap, (20)  'General Accounting'  no-gap centered,
              wa_vl no-gap, (20)  'Manager'  no-gap centered,
               wa_vl no-gap, (29)  space     no-gap centered,
               wa_vl no-gap, (11)  space     no-gap centered,
               wa_vl no-gap.
*
*  write:/  wa_vl no-gap, (22)  space   no-gap centered,"UD1K918802
  write:/  wa_vl no-gap, (22)  space   no-gap centered,
          wa_vl no-gap, (29)  space   no-gap centered,
          wa_vl no-gap, (12)  space   no-gap centered,
          wa_vl no-gap, (20)  space   no-gap centered,
          wa_vl no-gap, (29)  space   no-gap centered,
          wa_vl no-gap, (11)  space   no-gap centered,
          wa_vl no-gap.
  new-line. uline at (wa_width).
*---------------------Head of  division
  write:/  wa_vl no-gap, (22)  space   no-gap centered,
           wa_vl no-gap, (29)  space   no-gap centered,
           wa_vl no-gap, (12)  space   no-gap centered,
           wa_vl no-gap, (20)  'Head of Department'  no-gap centered,
           wa_vl no-gap, (29)  space   no-gap centered,
           wa_vl no-gap, (11)  space   no-gap centered,
           wa_vl no-gap.
*
* write:/wa_vl no-gap, (22)'Head of Division'no-gap centered,"UD1K918802




*  write:/  wa_vl no-gap, (22)  'Chief of Division'   no-gap centered,
*          wa_vl no-gap, (29)  space    no-gap centered,
*          wa_vl no-gap, (12)  space    no-gap centered,
**           wa_vl no-gap, (20)  'Treasurer'  no-gap centered,
*"UD1K918802
*          wa_vl no-gap, (20)  'Treasury'  no-gap centered,
*          wa_vl no-gap, (29)  space     no-gap centered,
*          wa_vl no-gap, (11)  space     no-gap centered,
*          wa_vl no-gap.
**
**  write:/  wa_vl no-gap, (22)  space   no-gap centered,"UD1K918802
*  write:/  wa_vl no-gap, (22)  space   no-gap centered,
*          wa_vl no-gap, (29)  space   no-gap centered,
*          wa_vl no-gap, (12)  space   no-gap centered,
*          wa_vl no-gap, (20)  space   no-gap centered,
*          wa_vl no-gap, (29)  space   no-gap centered,
*          wa_vl no-gap, (11)  space   no-gap centered,
*          wa_vl no-gap.
*  new-line. uline at (wa_width).


  write:/  wa_vl no-gap, (22)  'Chief of Division'   no-gap centered,
           wa_vl no-gap, (29)  space    no-gap centered,
           wa_vl no-gap, (12)  space    no-gap centered,
           wa_vl no-gap, (20)  '          ----------' no-gap centered,
           wa_vl no-gap, (29)  '                 -------------'
           no-gap centered,
           wa_vl no-gap, (11)  '-------------'    no-gap centered,
           wa_vl no-gap.
* End of changes - UD1K918802
*
  write:/  wa_vl no-gap, (22)  space   no-gap centered,
           wa_vl no-gap, (29)  space   no-gap centered,
           wa_vl no-gap, (12)  space   no-gap centered,
           wa_vl no-gap, (20)  '          |Treasury '   no-gap centered,
           wa_vl no-gap, (29)  '      |              '
           no-gap centered,
           wa_vl no-gap, (11)  space   no-gap centered,
           wa_vl no-gap.

*  WRITE :/ wa_vl NO-GAP,
*           (128) wa_ul  NO-GAP,
*           wa_vl NO-GAP.

  new-line. uline at (wa_width).

*---------------------President / CEO
  write:/  wa_vl no-gap, (22)  space   no-gap centered,
           wa_vl no-gap, (29)  space   no-gap centered,
           wa_vl no-gap, (12)  space   no-gap centered,
           wa_vl no-gap, (20)  space   no-gap centered,
           wa_vl no-gap, (29)  space   no-gap centered,
           wa_vl no-gap, (11)  space   no-gap centered,
           wa_vl no-gap.
*
* write:/wa_vl no-gap, (22)'President / CEO' no-gap centered,"UD1K918802
  write:/  wa_vl no-gap, (22)  'President/CEO'   no-gap centered,
          wa_vl no-gap, (29)  space    no-gap centered,
          wa_vl no-gap, (12)  space    no-gap centered,
          wa_vl no-gap, (20)  'CFO'  no-gap centered,
          wa_vl no-gap, (29)  space     no-gap centered,
          wa_vl no-gap, (11)  space     no-gap centered,
          wa_vl no-gap.
*
*  write:/  wa_vl no-gap, (22)  space   no-gap centered, "UD1K918802
  write:/ wa_vl no-gap, (22) space no-gap centered,
          wa_vl no-gap, (29)  space   no-gap centered,
          wa_vl no-gap, (12)  space   no-gap centered,
          wa_vl no-gap, (20)  space   no-gap centered,
          wa_vl no-gap, (29)  space   no-gap centered,
          wa_vl no-gap, (11)  space   no-gap centered,
          wa_vl no-gap.
  new-line. uline at (wa_width).


*---2003/09/08 jhs modify
*  WRITE:/  wa_vl NO-GAP, (11) 'Requestor'    NO-GAP CENTERED,
*           wa_vl NO-GAP, (116) space NO-GAP CENTERED,
*           wa_vl NO-GAP.
*  NEW-LINE. ULINE AT (wa_width).
**
*  WRITE:/  wa_vl NO-GAP, (06) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'Head of '        NO-GAP CENTERED,
*           wa_vl NO-GAP, (30) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (06) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'Cost Accounting'       NO-GAP CENTERED,
*           wa_vl NO-GAP, (31) space             NO-GAP CENTERED,
*           wa_vl NO-GAP.
*  WRITE:/  wa_vl NO-GAP, (06) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'Department' NO-GAP CENTERED,
*           wa_vl NO-GAP, (30) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (06) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (31) space        NO-GAP CENTERED,
*           wa_vl NO-GAP.
*
*  WRITE:/  sy-vline NO-GAP, (06) space        NO-GAP CENTERED,
*           sy-vline NO-GAP, (56) sy-uline        NO-GAP,
*           sy-vline NO-GAP, (06) space        NO-GAP,
*           sy-vline NO-GAP, (57) sy-uline        NO-GAP,
*           sy-vline NO-GAP.
*
*  WRITE:/  wa_vl NO-GAP, (06) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'Head of'     NO-GAP CENTERED,
*           wa_vl NO-GAP, (30) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (06) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'General '   NO-GAP CENTERED,
*           wa_vl NO-GAP, (31) space             NO-GAP CENTERED,
*           wa_vl NO-GAP.
*  WRITE:/  wa_vl NO-GAP, (06) 'Dept'    NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'Sub department'   NO-GAP CENTERED,
*           wa_vl NO-GAP, (30) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (06) 'Acct'    NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'Accounting'  NO-GAP CENTERED,
*           wa_vl NO-GAP, (31) space        NO-GAP CENTERED,
*           wa_vl NO-GAP.
*
*  WRITE:/  sy-vline NO-GAP, (06) space        NO-GAP CENTERED,
*           sy-vline NO-GAP, (56) sy-uline        NO-GAP,
*           sy-vline NO-GAP, (06) space        NO-GAP,
*           sy-vline NO-GAP, (57) sy-uline        NO-GAP,
*           sy-vline NO-GAP.
*
*  WRITE:/  wa_vl NO-GAP, (06) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'Head of Division'  NO-GAP CENTERED,
*           wa_vl NO-GAP, (30) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (06) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'Treasurer'     NO-GAP CENTERED,
*           wa_vl NO-GAP, (31) space             NO-GAP CENTERED,
*           wa_vl NO-GAP.
*  WRITE:/  wa_vl NO-GAP, (06) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (30) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (06) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (31) space        NO-GAP CENTERED,
*           wa_vl NO-GAP.
*
*  WRITE:/  sy-vline NO-GAP, (06) space        NO-GAP CENTERED,
*           sy-vline NO-GAP, (56) sy-uline        NO-GAP,
*           sy-vline NO-GAP, (06) space        NO-GAP,
*           sy-vline NO-GAP, (57) sy-uline        NO-GAP,
*           sy-vline NO-GAP.
*
**
*  WRITE:/  wa_vl NO-GAP, (06) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'President / C E O'    NO-GAP CENTERED,
*           wa_vl NO-GAP, (30) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (06) space             NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) 'C F O'        NO-GAP CENTERED,
*           wa_vl NO-GAP, (31) space             NO-GAP CENTERED,
*           wa_vl NO-GAP.
*
*  WRITE:/  wa_vl NO-GAP, (06) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (30) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (06) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (25) space        NO-GAP CENTERED,
*           wa_vl NO-GAP, (31) space        NO-GAP CENTERED,
*           wa_vl NO-GAP.
*
*  NEW-LINE. ULINE AT (wa_width).
*

endform.                    " draw_sign_box

*&---------------------------------------------------------------------*
*&      Form  get_position_id
*&---------------------------------------------------------------------*
form get_position_id changing p_code p_name.
  data: l_objnr like imzo-objnr,
        l_posnr like imzo-posnr,
        l_posid like impr-posid.

*  p_assgn1 = '#############'.
  concatenate 'OR'  it_bseg-aufnr into l_objnr.
  select single posnr into l_posnr
    from imzo where objnr eq l_objnr.
  if sy-subrc ne 0.
    exit.
  endif.
  select single a~posid b~post1 into (p_code, p_name)
    from impr as a inner join impu as b on b~posnr eq a~posnr
    where a~posnr eq l_posnr and
          b~spras eq sy-langu.
endform.                    " get_position_id
*&---------------------------------------------------------------------*
*&      Form  write_empty_line
*&---------------------------------------------------------------------*
form write_empty_line.
  do 3 times.
    format   intensified off.
    format color col_normal intensified off.
    write :/(01) sy-vline,
           (127) ' ' no-gap,
          (01) sy-vline.
  enddo.
  new-line. uline at (wa_width).
endform.                    " write_empty_line

*&---------------------------------------------------------------------*
*&      Form  print_vendor_cust_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form print_vendor_cust_sum_excel using p_next.

  data: l_str type char100.
  data: l_pos type i.
  define __fill_itema.
    l_pos = l_pos + 1.
    perform fill_cells using 1 l_pos &1.
  end-of-definition.

  if p_next eq true and
     ( out_times ne g_curr_page ).

    perform excel_sheet using 5 changing subrc.
    __fill_itema : 'Continue on next page                            >>'.

  else.

    if wa_sdmbts_d ne '0'.
      wa_sdmbts_g = wa_sdmbts_g - wa_sdmbts_d.
      wa_sdmbts_t = wa_sdmbts_g + wa_sdmbts_d.
    elseif wa_sdmbts_d eq '0'.
      wa_sdmbts_t =  wa_sdmbts_g.
    endif.
    if wa_sdmbth_d ne '0'.
      wa_sdmbth_g = wa_sdmbth_g - wa_sdmbth_d.
      wa_sdmbth_t = wa_sdmbth_g + wa_sdmbth_d.
    elseif wa_sdmbth_d eq '0'.
      wa_sdmbth_t = wa_sdmbth_g .
    endif.

    if not wa_sdmbts_g is initial or
       not wa_sdmbth_g is initial.

      perform excel_sheet using 5 changing subrc.

      read table it_dupinvh index 1.

      data: l_lines(3) type c.
      describe table it_dupinvh lines l_lines.

      if sy-subrc eq 0.
        concatenate 'Duplicate Invoice :'
                   it_dupinvh-belnr into l_str separated by space.

        concatenate l_str
                    '& another' l_lines 'documents accounted'
                    'Receivable/Payable:' into l_str
                    separated by space.

        __fill_itema : l_str,
                         wa_sdmbts_g,
                         wa_sdmbth_g.

      else.
        __fill_itema : 'Receivable/Payable:',
                         wa_sdmbts_g,
                         wa_sdmbth_g.
      endif.

    endif.

  clear : wa_scnt_g, wa_hcnt_g, wa_sdmbts_g, wa_sdmbth_g,
          wa_sdmbts_d,
          wa_sdmbth_d.

  endif.
*  endif.

  perform excel_sheet using 3 changing subrc.

endform.                    "print_vendor_cust_sum_excel

*---------------------------------------------------------------------*
*       FORM print_vendor_cust_sum                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form print_vendor_cust_sum.
  data: l_str type char100.

  uline at (wa_width).
  format color col_total intensified.  " Color of final totals
  if wa_sdmbts_d ne '0'.
    wa_sdmbts_g = wa_sdmbts_g - wa_sdmbts_d.
    wa_sdmbts_t = wa_sdmbts_g + wa_sdmbts_d.
  elseif wa_sdmbts_d eq '0'.
    wa_sdmbts_t =  wa_sdmbts_g.
  endif.
  if wa_sdmbth_d ne '0'.
    wa_sdmbth_g = wa_sdmbth_g - wa_sdmbth_d.
    wa_sdmbth_t = wa_sdmbth_g + wa_sdmbth_d.
  elseif wa_sdmbth_d eq '0'.
    wa_sdmbth_t = wa_sdmbth_g .
  endif.

  if not wa_sdmbts_g is initial or
     not wa_sdmbth_g is initial.

    read table it_dupinvh index 1.

    data: l_lines(3) type c.
    describe table it_dupinvh lines l_lines.

    if sy-subrc eq 0.

      concatenate 'Duplicate Invoice :'
                  it_dupinvh-belnr
                  '& another' l_lines 'documents accounted' into l_str
                  separated by space.

      write : / sy-vline, 2 l_str.
      write : sy-vline, 75 'Receivable/Payable:',
              93(18)  wa_sdmbts_g no-zero,
              111(18) wa_sdmbth_g no-zero,
           sy-vline.
    else.
      write : / sy-vline, 75 'Receivable/Payable:',
              93(18)  wa_sdmbts_g no-zero,
              111(18) wa_sdmbth_g no-zero,
           sy-vline.
    endif.

  endif.

  uline at (wa_width).
  format color col_normal intensified off.  " Color of final totals
  clear : wa_scnt_g, wa_hcnt_g, wa_sdmbts_g, wa_sdmbth_g,
          wa_sdmbts_d,
          wa_sdmbth_d.
endform.                    "print_vendor_cust_sum

*&---------------------------------------------------------------------*
*&      Form  print_option
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form print_option.
  check not pr_dest is initial ."

  new-page print on
         destination pr_dest
            immediately pr_imm
            keep in spool 'X'
            line-count 90
            line-size 130
            layout 'ZX_90_130'
            no dialog.

endform.                    " print_option
*&---------------------------------------------------------------------*
*&      Form  end_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form end_of_page.
*  IF end_line EQ space AND sy-pagno <> wa_total_page.
*    NEW-LINE.
*    ULINE AT (wa_width).
*  ENDIF.

endform.                    " end_of_page
*&---------------------------------------------------------------------*
*&      Form  filtering_bseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1473   text
*----------------------------------------------------------------------*
form filtering_bseg using p_mark.
  data w_int type i.
  clear w_int.
  loop at it_bseg.
    if it_bseg-koart ne p_mark.
      read table it_bseg with key  belnr = it_bseg-belnr
                                       koart = p_mark.
      if sy-subrc <> 0.
        delete table it_bseg from it_bseg.
      endif.
    endif.
  endloop.
  describe table it_bseg lines w_int.
  if w_int = 0.
    case p_mark.
      when 'K'.
        message i011 with text-090.
        leave to transaction  'ZRFIG02'.
      when 'D'.
        message i011 with text-091.
        leave to transaction  'ZRFIG02'.
    endcase.
  endif.

endform.                    " filtering_bseg
*&---------------------------------------------------------------------*
*&      Form  get_bkpf_data
*&---------------------------------------------------------------------*
form get_bkpf_data.
  ranges : r_usnam for bkpf-usnam,
           r_ppnam for bkpf-ppnam. "UD1K918802.
  if p_own eq 'X'.
    r_usnam-sign   = 'I'.
    r_usnam-option = 'EQ'.
    r_usnam-low    = sy-uname.
    append r_usnam.
* Begin of changes - UD1K918802
    r_ppnam-sign = 'I'.
    r_ppnam-option = 'EQ'.
    r_ppnam-low = sy-uname.
    append r_ppnam.
* End of changes - UD1K918802
  endif.


  ranges : r_bstat for bkpf-bstat.
  r_bstat-sign   = 'I'.   r_bstat-option = 'EQ'.

  if p_parked = space.
    r_bstat-low    = ' '.  append r_bstat.
    r_bstat-low    = 'A'.  append r_bstat.
    r_bstat-low    = 'B'.  append r_bstat.
    r_bstat-low    = 'D'.  append r_bstat.
    r_bstat-low    = 'S'.  append r_bstat.
  else.  "'X'.
    r_bstat-low    = 'V'.  append r_bstat.
  endif.


  select bukrs belnr gjahr bstat
       budat usnam ppnam tcode xblnr
       bldat cpudt blart bktxt waers
       stblg stjah stgrd awtyp awkey
  from bkpf    into corresponding fields of table it_bkpf
  where bukrs =  p_bukrs and
        belnr in s_belnr and
        gjahr in s_gjahr and
        blart in s_blart and
        budat in s_budat and
        xblnr in s_xblnr and
        cpudt in s_cpudt and
       ( usnam in r_usnam or ppnam in r_ppnam ) and         "UD1K918802
        bstat in r_bstat.

  data: l_idx like sy-tabix.

  loop at it_bkpf.
    l_idx = sy-tabix.
*----- Appended by BSBAE. 2004.06.08
    case it_bkpf-awtyp.
      when 'MKPF'.
        data: lv_awkey like bkpf-awkey.

*        select single *
*          from mseg
*         where mblnr = it_bkpf-awkey(10)
*           and mjahr = it_bkpf-awkey+10(4)
*           and bwart in ('101','102').
*        if sy-subrc eq 0.
*          it_bkpf-revtx = 'REV'.
*          modify it_bkpf index l_idx.
*
*          concatenate mseg-lfbnr mseg-lfbja into lv_awkey.
*          read table it_bkpf with key awtyp = 'MKPF'
*                                      awkey = lv_awkey.
*          if sy-subrc eq 0.
*            it_bkpf-revtx = 'REV'.
*            modify it_bkpf index sy-tabix.
*          endif.
*        endif.
        if it_bkpf-tcode = 'VL09'
        or it_bkpf-tcode = 'MBST'.

          if it_bkpf-stgrd is initial.
            select single stgrd into it_bkpf-stgrd from bkpf
              where bukrs = it_bkpf-bukrs and
                    belnr = it_bkpf-stblg and
                    gjahr = it_bkpf-stjah.
          endif.

          it_bkpf-revtx = 'REV'.
          modify it_bkpf index l_idx.
* FIXME
* original document....use reference, entry date/time...

        endif.

      when 'RMRP'.
        select single *
          from rbkp
         where stblg = it_bkpf-awkey(10)
           and stjah = it_bkpf-awkey+10(4).
        if sy-subrc eq 0.
          it_bkpf-revtx = 'REV'.
          modify it_bkpf index l_idx.

          concatenate rbkp-belnr rbkp-stjah into lv_awkey.
          read table it_bkpf with key awtyp = 'RMRP'
                                      awkey = lv_awkey.
          if sy-subrc eq 0.
            it_bkpf-revtx = 'REV'.
            modify it_bkpf index sy-tabix transporting revtx.
          endif.
        endif.
      when others.
    endcase.
*----- Appended by BSBAE. 2004.06.08


  endloop.
endform.                    " get_bkpf_data
*&---------------------------------------------------------------------*
*&      Form  get_bseg_details
*&---------------------------------------------------------------------*
form get_bseg_details.
  loop at it_bkpf.

    case  it_bkpf-bstat.
      when 'V'.
        perform  vbsegs_read_process.  "G/L Account Document Parking
        perform  vbsegk_read_process.  "Vendor Document Parking
        perform  vbsegd_read_process.  "Customer Document Parking
        perform  vbsega_read_process.  "Asset Document Parking
*        PERFORM  vbset_read_process.   "Taxes Document Parking

* Normal Document
      when others.
        perform  bseg_read_process.
    endcase.
  endloop.

endform.                    " get_bseg_details
*&---------------------------------------------------------------------*
*&      Form  set_debit_credit
*&---------------------------------------------------------------------*
form set_debit_credit.
  loop at it_bseg.
    clear it_t041c.
    if it_bseg-stgrd <> space.
      read table it_t041c with key stgrd = it_bseg-stgrd.
    endif.

    if it_t041c-xnegp = 'X'.   "negative posting
      if it_bseg-shkzg = 'H'.
        it_bseg-dmbts = - it_bseg-dmbtr.
        it_bseg-dmbth = 0.
      else.
        it_bseg-dmbth = - it_bseg-dmbtr.
        it_bseg-dmbts = 0.
      endif.
    else.
      if it_bseg-shkzg = 'H'.
        move it_bseg-dmbtr to it_bseg-dmbth.
        move 0             to it_bseg-dmbts.
      else.
        move it_bseg-dmbtr to it_bseg-dmbts.
        move 0             to it_bseg-dmbth.
      endif.
    endif.

    modify it_bseg   transporting dmbth dmbts.
  endloop.

endform.                    " set_debit_credit
*&---------------------------------------------------------------------*
*&      Form  process_ap_filter
*&---------------------------------------------------------------------*
form process_ap_filter.
*---START#1 WSKIM 03/01/2005
  data: l_chk(1) type c.

  check s_zlspr-low <> space or s_lifnr-low <> space.

  loop at  it_bseg where ( koart eq 'K' ).
    if  it_bseg-zlspr in s_zlspr
    and it_bseg-lifnr in s_lifnr.

    else.
      delete it_bkpf  where belnr = it_bseg-belnr.
      delete it_bseg  where belnr = it_bseg-belnr.
    endif.
  endloop.

endform.                    " process_ap_filter
*&---------------------------------------------------------------------*
*&      Form  get_company_info
*&---------------------------------------------------------------------*
form get_company_info.
**-> t001: Company Codes
  select single * from t001
     where  bukrs = p_bukrs.
  check sy-subrc = 0.
  select single name1 name2 into (wa_l_name1, wa_l_name2)
  from adrc
  where addrnumber = t001-adrnr and
      date_from <= sy-datum.
*  CONCATENATE wa_l_name1 ' ' Wa_l_name2 INTO wa_l_company_name.
  wa_l_company_name =  wa_l_name1.
  data: l_pos type i.
  l_pos = strlen( wa_l_company_name ) + 1.
  wa_l_company_name+l_pos = wa_l_name2.


  select * into table it_t041c
    from t041c.

endform.                    " get_company_info
*&---------------------------------------------------------------------*
*&      Form  write_doc_end
*&---------------------------------------------------------------------*
form write_doc_end  using wa_sdmbth
                          wa_sdmbts
                          wa_hcnt
                          wa_scnt.



  data: wa_l_reverse(30).
  data $awkey type awkey.

* check reverse doc
  if not it_bkpf-stblg is initial or it_bkpf-revtx <> space.

    if it_bkpf-awtyp eq 'BKPF'.
      if it_bkpf-stgrd is initial.
        select single stgrd into it_bkpf-stgrd from bkpf
          where bukrs = it_bkpf-bukrs and
                belnr = it_bkpf-stblg and
                gjahr = it_bkpf-stjah.
      endif.
    else.
      select single * from rbkp
        where belnr = it_bkpf-awkey(10)
          and gjahr = it_bkpf-gjahr.

      if sy-subrc eq 0.
        concatenate rbkp-stblg rbkp-stjah into $awkey.

        select single belnr into it_bkpf-stblg
            from bkpf
          where awkey = $awkey
            and bukrs eq rbkp-bukrs.
      endif.
    endif.

    concatenate ' REVERSED->'
                it_bkpf-stblg '/'
                it_bkpf-stgrd
          into  wa_l_reverse.

  endif.

  format color col_total intensified off.
  write : / sy-vline,
          'PstDt:',(10) it_bkpf-budat, '   DocDt:', it_bkpf-bldat,
          '  ', (16) it_bkpf-xblnr,
          (30) wa_l_reverse,
         92(18) wa_sdmbts no-zero,
           (18) wa_sdmbth no-zero,
          sy-vline.
  uline at (wa_width).
endform.                    " write_doc_end
*&---------------------------------------------------------------------*
*&      Form  append_collect_bseg
*&---------------------------------------------------------------------*
form append_collect_bseg.
  if p_detail = 'X'.
    append it_bseg.
  else.
    if it_bseg-matnr <> space.
      clear: it_bseg-zuonr.
    endif.
* Begin of changes - UD1K918802
    if it_bseg-ktosl eq 'RAP'.
      clear: it_bseg-buzei, it_bseg-meins,
            it_bseg-sgtxt, it_bseg-zuonr.
      clear: it_bseg-ebeln, it_bseg-ebelp.
      clear: it_bseg-meins, it_bseg-augbl.

    else.
      clear: it_bseg-buzei, it_bseg-matnr, it_bseg-meins,
            it_bseg-sgtxt, it_bseg-zuonr.
      clear: it_bseg-ebeln, it_bseg-ebelp.
      clear: it_bseg-meins, it_bseg-augbl,
             it_bseg-ktosl.
    endif.
* End of changes - UD1K918802
    collect it_bseg.
  endif.
endform.                    " append_collect_bseg
*&---------------------------------------------------------------------*
*&      Form  write_doc_lines
*&---------------------------------------------------------------------*
form write_doc_lines  using wa_save_belnr  wa_blart  wa_revtx.
  if wa_save_belnr ne it_bseg-belnr.
    wa_save_belnr = it_bseg-belnr.
    read table it_bkpf with key belnr = it_bseg-belnr.
    if sy-subrc = 0.
      wa_blart = it_bkpf-blart.
    endif.

    write : / sy-vline,
             it_bseg-belnr,
             it_bseg-bschl,
*              it_bseg-buzei,
             it_bseg-hkont+4(6) right-justified,
*              it_bseg-sgtxt,
           (20) it_info-info1,
                it_info-info2,
                it_info-info3,
                it_info-info4 right-justified,
                it_info-taxinfo,
           (18) it_bseg-dmbts no-zero,
           (18) it_bseg-dmbth no-zero,
             sy-vline.
  else.
    write : / sy-vline,
            wa_blart,
            wa_revtx,
            it_bseg-bschl,
*             it_bseg-buzei,
            it_bseg-hkont+4(6) right-justified,
*              it_bseg-sgtxt,
          (20) it_info-info1,
               it_info-info2,
               it_info-info3,
               it_info-info4 right-justified,
               it_info-taxinfo,
          (18) it_bseg-dmbts no-zero,
          (18) it_bseg-dmbth no-zero,
            sy-vline.
    wa_blart = ' '.
    wa_revtx = ' '.
  endif.

endform.                    " write_doc_lines
*&---------------------------------------------------------------------*
*&      Form  get_account_text
*&---------------------------------------------------------------------*
form get_account_text.
  select single txt20 into it_info-info1
    from skat
   where spras = sy-langu
     and ktopl = t001-ktopl
     and saknr = it_bseg-hkont.
endform.                    " get_account_text
*&---------------------------------------------------------------------*
*&      Form  get_account_info
*&---------------------------------------------------------------------*
form get_account_info.
  select single * from skb1
    where bukrs eq it_bkpf-bukrs and
          saknr eq it_bseg-hkont.

*---------------------------------------------------------------------
  case it_bseg-koart.
    when 'A'. "asset
      perform fill_asset_info.
    when 'D'. "customers
      perform fill_cust_info.
    when 'K'. "vendors
      perform fill_vend_info.
    when 'M'. "material
      if p_detail = 'X'.
        it_info-info2 = it_bseg-matnr.
        write: it_bseg-menge to  it_info-info3 right-justified."left
      endif.
    when 'S'. "G/L accounts
      if it_bseg-hkont+4(2) = '16'    or
         it_bseg-hkont+4(3) = '901'.
        perform fill_invest_info.
      elseif it_bseg-hkont+4(1) = '6'.
        perform fill_expense_info.
      else.
        if it_bseg-bschl >= '80'.
          perform fill_material_info.
        elseif it_bseg-buzid = 'F'.   " PO freight, duty, ...
          if p_detail = 'X'.
            it_info-info2 = it_bseg-matnr.
            it_info-info3 = it_bseg-zuonr.
            write: it_bseg-menge to  it_info-info4 decimals 0.
          endif.
*****>>> GRIR - Misc Import Expense
        elseif it_bkpf-blart = 'KI'.
          it_info-info2 = it_bseg-sgtxt.
        elseif skb1-fdlev(1) ca 'CB'.
          concatenate   it_bseg-fdtag+4(2) '/'
                        it_bseg-fdtag+6(2) '/'
                        it_bseg-fdtag+0(4)
                  into it_info-info3.
* Default
        else.
          it_info-info2 = it_bseg-zuonr.
          it_info-info3 = it_bseg-sgtxt.
        endif.

      endif.

  endcase.

endform.                    " get_account_info
*&---------------------------------------------------------------------*
*&      Form  adjust_data
*&---------------------------------------------------------------------*
form adjust_data.
  data: ind type i,
        l_lifnr like bseg-lifnr,
        l_belnr like bseg-belnr.

  check r_nor <> 'X'.

*Sort for vendor,customer
  sort it_bseg by gjahr belnr bschl .

  loop at it_bseg.
    ind = sy-tabix.

    if l_lifnr = space and l_belnr <> it_bseg-belnr.
      l_lifnr = it_bseg-lifnr.
      l_belnr = it_bseg-belnr.
    else.
      l_belnr = it_bseg-belnr.
    endif.

    if l_belnr = it_bseg-belnr.
      it_bseg-sortk = l_lifnr.
      modify it_bseg index ind.
    else.
      clear: l_lifnr, l_belnr.
    endif.

*    MODIFY it_bseg TRANSPORTING sortk
*             WHERE belnr = it_bseg-belnr
*               AND gjahr = it_bseg-gjahr
*               AND koart <> 'K'.
*                   AND bschl = '40'.
  endloop.

*  DELETE it_bseg WHERE lifnr = space.
  delete it_bseg where sortk = space.


  if r_ven eq 'X'.
    perform filtering_bseg using 'K'.
  elseif r_cus eq 'X'.
    perform filtering_bseg using 'D'.
  endif.
endform.                    " adjust_data
*&---------------------------------------------------------------------*
*&      Form  get_tax_rate
*&---------------------------------------------------------------------*
form get_tax_rate.
  data: l_rate type p decimals 1.
  data: ls_rate(4).

*    move it_bseg-mwskz to it_info-mwskz.
  if it_bseg-bschl > 39.

    it_info-taxinfo = it_bseg-mwskz.

* tax account...
    if it_bseg-fwbas ne 0.
      l_rate = it_bseg-dmbtr / it_bseg-fwbas * 100.
      write l_rate to ls_rate.
      it_info-info3 = ls_rate.

      select single text1 into it_info-info2 from t007s
        where spras = sy-langu and
              kalsm = 'TAXUS' and
              mwskz = it_bseg-mwskz.
    endif.
  endif.


*park document
  data: begin of it_tax occurs 0,
           kschl like konp-kschl,
           kbetr like konp-kbetr,
        end of it_tax.
  data: wa_l_tax like bseg-dmbtr.

  if it_bseg-mwskz ne space and  " exist tax code
     it_bseg-shkzg eq 'S'   and  " only debit
     it_bkpf-bstat eq 'V'.       " only parked doc.


    select b~kschl b~kbetr into table it_tax
      from a003 as a inner join konp as b on b~knumh eq a~knumh
      where a~kappl = 'TX' and
            a~aland = 'US' and
            a~mwskz eq it_bseg-mwskz.


    loop at it_tax where kschl+3(1) eq 'I'.    "case A/P
      wa_l_tax = it_bseg-dmbtr * it_tax-kbetr / 1000.
      add wa_l_tax to it_info-drt.
    endloop.

    it_info-info3 = it_info-drt.
  endif.

* Begin of changes - UD1K921056
  if it_bseg-koart eq 'K'.
    perform get_withholding_tax.
  endif.

* End of changes - UD1K921056

endform.                    " get_tax_rate
*&---------------------------------------------------------------------*
*&      Form  fill_material_info
*&---------------------------------------------------------------------*
form fill_material_info.
*
*80	G	D	Stock initial entry
*81	G	D	Costs
*83	G	D	Price difference
*84	G	D	Consumption
*85	G	D	Change in stock
*86	G	D	GR/IR debit
*89	M	D	Stock inwrd movement
*90	G	C	Stock initial entry
*91	G	C	Costs
*93	G	C	Price difference
*94	G	C	Consumption
*95	G	C	Change in stock
*96	G	C	GR/IR credit
*99	M	C	Stock outwd movement

  if it_bseg-bschl = '81' or it_bseg-bschl = '91'.  "Cost
    if it_bseg-aufnr <> space.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = it_bseg-aufnr
        importing
          output = it_info-info3.
    else.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = it_bseg-kostl
        importing
          output = it_info-info3.
    endif.

  elseif it_bseg-bschl = '83' or it_bseg-bschl = '93'.
* Begin of changes - UD1K918802
* For Retro Pricing line item Print Material/Qty
    if it_bseg-ktosl eq 'RAP'.
      write it_bseg-matnr to it_info-info2.
      write it_bseg-menge to it_info-info3.
      condense it_info-info3.
    else.
      concatenate it_bseg-ebeln it_bseg-ebelp into it_info-info3.
    endif.
* End of changes - UD1K918802

*****>>> GRIR
  elseif it_bseg-bschl = '96' or it_bseg-bschl = '86'.
    if p_detail = 'X'.
      if it_bkpf-tcode = 'MRER'.
        it_info-info3 = it_bseg-ebeln(10).
        it_info-info2 = it_bseg-sgtxt.
        write: it_bseg-menge to  it_info-info4 decimals 0.
      else.
        it_info-info3 = it_bseg-ebeln(10).
        it_info-info2 = it_bseg-matnr.
        write: it_bseg-menge to  it_info-info4 decimals 0.
      endif.
    endif.
  endif.

endform.                    " fill_material_info
*&---------------------------------------------------------------------*
*&      Form  fill_asset_info
*&---------------------------------------------------------------------*
form fill_asset_info.
  select single txt50
    from anla       "Asset Master Record Segment
    into it_info-name1
   where bukrs = it_bkpf-bukrs
     and anln1 = it_bseg-anln1  "Asset
     and anln2 = it_bseg-anln2. "Sub-Number

  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = it_bseg-anln1
    importing
      output = it_info-code.

  concatenate it_info-code     '-'
              it_bseg-anln2 '('
              it_bseg-anbwa ')'
         into it_info-code.
  if it_bseg-aufnr is initial.
    select single eaufn into it_bseg-aufnr
      from anla
      where bukrs eq it_bseg-bukrs and
            anln1 eq it_bseg-anln1 and
            anln2 eq it_bseg-anln2.
  endif.
  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = it_bseg-aufnr
    importing
      output = it_info-info3.

*      MOVE it_bseg-aufnr  TO  it_info-assgn2.
  data: wa_l_temp(20).
  perform get_position_id changing it_info-info2 wa_l_temp.

endform.                    " fill_asset_info
*&---------------------------------------------------------------------*
*&      Form  fill_cust_info
*&---------------------------------------------------------------------*
form fill_cust_info.
  select single name1
    from kna1       "General Data in Customer Master
    into it_info-name1
   where kunnr = it_bseg-kunnr.
  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = it_bseg-kunnr
    importing
      output = it_info-info2.
  if it_bseg-umskz <> ' '.
    concatenate it_info-info2 '('  it_bseg-umskz  ')'
                into it_info-info2.
  endif.

*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)
  concatenate it_bseg-zterm it_bseg-zlsch it_bseg-zlspr
         into it_info-info3 separated by '/'.

endform.                    " fill_cust_info
*&---------------------------------------------------------------------*
*&      Form  fill_vend_info
*&---------------------------------------------------------------------*
form fill_vend_info.
  data: l_lifnr like lfa1-lifnr,
        l_payterm(8) type c.

*   --> account type (name + vendor + space.gl)
  if it_bseg-empfb <> space.  "Alt.Payee
    select single * from lfa1
      where lifnr eq it_bseg-empfb.
  else.
    select single * from lfa1
      where lifnr eq it_bseg-lifnr.
  endif.

  if lfa1-xcpdk = 'X'.  " one time
    select single * from bsec
        where bukrs = it_bseg-bukrs
          and gjahr = it_bseg-gjahr
          and belnr = it_bseg-belnr.
    it_info-info2 = bsec-name1.
  else.
    it_info-info2 = lfa1-name1.
  endif.

  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = it_bseg-lifnr
    importing
      output = l_lifnr.

  clear l_payterm.
  if it_bseg-zterm <> space.
    concatenate it_bseg-zterm '/'
                it_bseg-zlsch '/'
                it_bseg-zlspr
           into l_payterm.
  endif.

  if it_bseg-umskz <> ' '.
    concatenate l_lifnr ';'
                '('  it_bseg-umskz  ')'
                l_payterm
           into it_info-info3.
  else.
    concatenate l_lifnr ';'
                l_payterm
           into it_info-info3.
  endif.

  if it_bseg-fdtag <> space.
    concatenate it_bseg-fdtag+4(2) '/' it_bseg-fdtag+6(2)
           into it_info-info4.
  endif.

endform.                    " fill_vend_info
*&---------------------------------------------------------------------*
*&      Form  fill_expense_info
*&---------------------------------------------------------------------*
form fill_expense_info.
  data: it_list type table of ifmkacoo with header line.
  ranges: r_kostl for bseg-kostl.

  if ( it_bseg-fistl is initial ) and
     ( not it_bseg-kostl is initial ).
    refresh r_kostl.
    r_kostl-sign = 'I'.  r_kostl-option = 'EQ'.
    r_kostl-low  = it_bseg-kostl.
    append r_kostl.
    call function 'HHM_KONT_READ_FROM_CO_OBJEKT'
      exporting
        i_fikrs = 'H201'
        i_kokrs = 'H201'
        i_kostl = 'X'
      tables
        t_kostl = r_kostl
        t_list  = it_list.

    loop at it_list.
      move it_list-fistl to it_bseg-fistl.
    endloop.
  endif.

  select single bezeich from fmfctrt into it_info-name1
   where fictr = it_bseg-fistl.  "Funds Center

*.......cc/fc
  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = it_bseg-kostl
    importing
      output = it_info-info2.

  concatenate it_info-info2 '(' it_bseg-fkber ')/' it_bseg-fistl
         into it_info-info2.
*.......order
  it_info-info3 = it_bseg-aufnr.


endform.                    " fill_expense_info
*&---------------------------------------------------------------------*
*&      Form  fill_invest_info
*&---------------------------------------------------------------------*
form fill_invest_info.
  it_info-info3 = it_bseg-aufnr.
  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = it_bseg-aufnr
    importing
      output = it_info-assign1.
  perform get_position_id changing it_info-info2
                                   it_info-name1.
endform.                    " fill_invest_info
*&---------------------------------------------------------------------*
*&      Form  handle_stripe
*&---------------------------------------------------------------------*
form handle_stripe using f_cnt.
  data: even    type i value 0.          " Checker even/odd

* Stripe handling (list write_body)
  if stripes = ' '.                 " No Stripes
    format color col_normal intensified off.
  else.                              " Stripes
    even = f_cnt mod 2.              " Check for even/odd
    if even = '0'.
      format color col_normal intensified.
    else.
      format color col_normal intensified off.
    endif.
  endif.

endform.                    " handle_stripe
*&---------------------------------------------------------------------*
*&      Form  get_withholding_tax
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_withholding_tax.
  select single * from lfbw
      where   lifnr eq it_bseg-lifnr and
              bukrs eq it_bseg-bukrs.
  if sy-subrc eq 0.
    select single * from t059z  where land1 = 'US'
                             and witht = lfbw-witht
                             and wt_withcd = lfbw-wt_withcd.
    if t059z-xqfor eq 'X' .
      select single * from t059fb where land1 = t059z-land1 and
                                        witht =  t059z-witht and
                                       wt_withcd = t059z-wt_withcd.
      it_info-info4 =  t059fb-qsatz.
      it_info-taxinfo =    t059fb-wt_withcd.
    else.
      it_info-info4 =  t059z-qsatz.
      it_info-taxinfo =    t059z-wt_withcd.
    endif.
  endif.


endform.                    " get_withholding_tax
*&---------------------------------------------------------------------*
*&      Form  print_via_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form print_via_excel_by_ref.


  data $it_bseg like it_bseg occurs 0 with header line.

* for develop. {
*  READ TABLE it_bseg INDEX 1.
*  MODIFY it_bseg TRANSPORTING xblnr WHERE xblnr NE it_bseg-xblnr.
* }

  $it_bseg[] = it_bseg[].
  __cls it_key.

  loop at it_bseg.
    move-corresponding it_bseg to it_key.
    collect it_key.
  endloop.

  sort it_key.

  data : subrc,
         $total_page(10), $curr_page(3),
         $out_times(3).

  x_lin = 40.

  if p_save eq true.
    perform check_tmp_dir changing subrc.
    check subrc ne '4'.
  endif.

  __cls it_tmp_file.

  loop at it_key.
    __cls it_bseg.

    loop at $it_bseg where sortk eq it_key-sortk
                       and xblnr eq it_key-xblnr.
      it_bseg = $it_bseg.
      append it_bseg.
    endloop.

    perform calc_sheet tables it_bseg
                       changing out_times subrc.

    pri_fr_line = 1.

    clear it_bkpf.
    read table it_bkpf with key xblnr = it_key-xblnr.

    clear : wa_sdmbth_g, wa_sdmbts_g.
    clear : wa_hcnt_g,   wa_scnt_g.
    clear : g_curr_page.

    do out_times times.

      g_curr_page = sy-index.
      $curr_page = sy-index.
      $out_times = out_times.

      concatenate $curr_page '/' $out_times
        into $total_page.

      pri_to_line = sy-index * x_lin.

      perform write_excel using sy-index
                                $total_page
                                it_key-xblnr.

      pri_fr_line =  pri_to_line + 1.

      clear nex.

      if p_scr eq true and nex eq true and sy-index < out_times.

        call function 'POPUP_TO_CONFIRM_STEP'
          exporting
            defaultoption = 'Y'
            textline1     = 'Please confirm,'
            textline2     = 'Continue to next page for printing?'
            titel         = 'Next page'
            start_column  = 25
            start_row     = 6
          importing
            answer        = answer.

        if answer ne 'J'. exit. endif.

      endif.

    enddo.
  endloop.

  __cls it_bseg.

  it_bseg[] = $it_bseg[].

endform.                    " print_via_excel
*&---------------------------------------------------------------------*
*&      Form  excel_form_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form excel_form_check.

  clear : tmp_file, tmp_dir.

  tmp_dir = 'C:\SapXls\'.
  perform find_dir using tmp_dir changing subrc.
  if subrc ne '1'.
    perform make_directory changing tmp_dir subrc.
    perform find_dir using tmp_dir changing subrc.
    if subrc ne '1'.
      message e011 with
       'Could not make the tmp directory for I/V printing'.
      subrc = '4'.
      exit.
    endif.
  endif.

  tmp_file = 'C:\SapXls\IV2_TEMPLATE.XLS'.

  call function 'WS_QUERY'
    exporting
      filename       = tmp_file
      query          = 'FE'
    importing
      return         = subrc
    exceptions
      inv_query      = 1
      no_batch       = 2
      frontend_error = 3
      others         = 4.

  if subrc eq 1. exit. endif.

  submit zafiu132 with p_file eq 'C:\SapXls\IV2_Template.xls'
                  with r_b eq 'X'
                  and return.


endform.                    " excel_form_check
*&---------------------------------------------------------------------*
*&      Form  find_dir
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TMP_DIR  text
*      <--P_SUBRC  text
*----------------------------------------------------------------------*
form find_dir using    p_$dirname
              changing p_subrc.

  call function 'WS_QUERY'
    exporting
      filename       = p_$dirname
      query          = 'DE'
    importing
      return         = p_subrc
    exceptions
      inv_query      = 1
      no_batch       = 2
      frontend_error = 3
      others         = 4.

endform.                               " FIND_DIR
*---------------------------------------------------------------------*
*       FORM make_directory                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_P_DIRECTORY                                                 *
*  -->  P_SUBRC                                                       *
*---------------------------------------------------------------------*
form make_directory changing p_p_directory p_subrc.

  data  valid_dir like rlgrap-filename.
  data  save_dir(50).
  save_dir = valid_dir = p_p_directory.
  clear subrc.

  perform get_valid_dir tables new_dir changing valid_dir subrc.
  if subrc eq 0.
    perform mk_dir tables new_dir using valid_dir changing subrc.
    perform get_dirname using p_p_directory changing p_p_directory.
    perform find_dir using p_p_directory changing subrc.
  endif.
  p_p_directory = save_dir.

endform.                    " MAKE_DIRECTORY

*---------------------------------------------------------------------*
*       FORM get_valid_dir                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  NEW_DIR                                                       *
*  -->  $DIRNAME                                                      *
*  -->  SUBRC                                                         *
*---------------------------------------------------------------------*
form get_valid_dir tables new_dir structure new_dir
                 changing $dirname subrc.
  __cls new_dir.
  perform get_dirname using $dirname changing $dirname.
  perform find_dir using $dirname changing subrc.
  check subrc ne 1.
  while subrc ne 1.
    if $dirname eq space. subrc = 0. exit. endif.
    if subrc ne 1.
      new_dir-dirname = $dirname.
      new_dir-$index  = sy-index.
      append new_dir.
    endif.
    perform get_dirname using $dirname changing $dirname.
    perform find_dir using $dirname changing subrc.
  endwhile.

  read table new_dir index 1.
  subrc = sy-subrc.

endform.                               " GET_VALID_DIR

*&---------------------------------------------------------------------*
*&      Form  GET_DIRNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_P_DIRECTORY  text                                        *
*      <--P_$DIRNAME  text                                             *
*----------------------------------------------------------------------*
form get_dirname using    p_p_directory
                 changing $dirname.
  data $strlen type i.
  data $offset type i.
  data $point  type i.
  data $str.
  perform filter_dir changing p_p_directory.
  $strlen = strlen( p_p_directory ).

  do $strlen times.
    $offset = $strlen - sy-index.
    $str    = p_p_directory+$offset(1).
    if $str eq '\'.
      $point = $offset.
      exit.
    endif.
  enddo.
  if $point > 0.
    $dirname = p_p_directory+0($point).
  else.
    clear $dirname.
  endif.
endform.                               " GET_DIRNAME

*&---------------------------------------------------------------------*
*&      Form  FILTER_DIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_P_DIRECTORY  text                                        *
*----------------------------------------------------------------------*
form filter_dir changing p_directory.

  do 256 times.
    replace '\\' with '\' into p_directory.
  enddo.

endform.                               " FILTER_DIR
*&---------------------------------------------------------------------*
*&      Form  CALC_SHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TAB_LINES  text
*      <--P_OUT_TIMES  text
*      <--P_SUBRC  text
*----------------------------------------------------------------------*
form calc_sheet tables p_it_bseg structure it_bseg
                changing p_out_times
                         p_subrc.

*  DATA $p_time TYPE p DECIMALS 2.
*  DATA $p_mod  TYPE p DECIMALS 2.
*  $p_time = ( p_tab_lines + p_nr_cnt ) / x_lin.
*
*  IF $p_time > 0.
*    $p_mod  = $p_time * 100.
*    $p_mod  = $p_mod MOD 100.
*    $p_mod  = $p_mod / 100.
*    $p_time = $p_time - $p_mod.
*  ENDIF.
*  p_out_times = $p_time.
*  $p_mod  = p_tab_lines MOD x_lin.
*  IF  $p_mod > 0.
*    ADD 1 TO p_out_times.
*  ENDIF.
  data : $line type i,
         $page type i,
         $ix type i, $tabix type i,
         $flag,
         $last_line type i,
         $fr_index type i.

  loop at p_it_bseg.
    $ix = sy-tabix.
    p_it_bseg-tabix = $ix.
    modify p_it_bseg index $ix transporting tabix.
  endloop.

  $page = 1.
  $fr_index = 1.

  do 1000 times.

    read table p_it_bseg with key ppage = '0'.
    if sy-subrc ne 0.
      exit.
    endif.

    clear $line.

    loop at p_it_bseg from $fr_index.

      $ix = sy-tabix.
      $tabix = p_it_bseg-tabix.
      add 1 to $line.

      if $line = x_lin.
        p_it_bseg-ppage = $page.
        modify p_it_bseg transporting ppage where tabix <= $last_line
                                             and  ppage = '0'.
        add 1 to $page.
        $fr_index = $last_line + 1.
        exit.
      endif.

      at end of belnr.
        add 1 to $line.
        $last_line = $tabix.
      endat.

      if $line = x_lin.
        p_it_bseg-ppage = $page.
        modify p_it_bseg transporting ppage where tabix <= $ix
                                             and  ppage = '0'.
        add 1 to $page.

* by ig.moon 3/22/11 {
        clear $line.
* }
      endif.

      at last.
        $flag = true.
      endat.

    endloop.

    if $flag eq true.
      p_it_bseg-ppage = $page.
      modify p_it_bseg transporting ppage where tabix <= $ix
                                           and  ppage = '0'.
    endif.
  enddo.


  p_out_times = p_it_bseg-ppage.

endform.                               " CALC_SHEET

*---------------------------------------------------------------------*
*       FORM mk_dir                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  NEW_DIR                                                       *
*  -->  VALID_DIR                                                     *
*  -->  SUBRC                                                         *
*---------------------------------------------------------------------*
form mk_dir tables   new_dir structure new_dir
            using    valid_dir
          changing   subrc.

  data $len type i.
  data dos_command(79) occurs 0 with header line.
  data winsys(3).
  sort new_dir by $index descending.

  perform find_dir using valid_dir changing subrc.
  if subrc ne 1.exit.endif.

  write '@ECHO OFF'            to dos_command.__dos.
  write  valid_dir             to dos_command(1).
  write: ':' to dos_command+1.__dos.
  write: 'CD' to dos_command,'\' to dos_command+3.__dos.
  $len = strlen( valid_dir ).
  if $len > 2.
    write: 'CD' to dos_command,valid_dir to dos_command+3.__dos.
  endif.
  loop at new_dir.
    write: 'MD' to dos_command,new_dir-dirname to dos_command+3.__dos.
    write: 'CD' to dos_command,new_dir-dirname to dos_command+3.__dos.
  endloop.

  call function 'WS_QUERY'
    exporting
      query  = 'WS'
    importing
      return = winsys.

  check ( winsys(2) eq 'WN') or ( winsys(2) eq 'PM' ).

  call function 'WS_DOWNLOAD'
    exporting
      filename            = 'C:\_$$IV$$.BAT'
      filetype            = 'ASC'
    tables
      data_tab            = dos_command
    exceptions
      file_open_error     = 1
      file_write_error    = 2
      invalid_filesize    = 3
      invalid_table_width = 4
      invalid_type        = 5
      no_batch            = 6
      unknown_error       = 7
      others              = 8.


  call function 'WS_EXECUTE'
    exporting
      commandline = ''
      inform      = 'X'
      program     = 'C:\_$$IV$$.BAT'.

  call function 'WS_FILE_DELETE'
    exporting
      file   = 'C:\_$$IV$$.BAT'
    exceptions
      others = 1.

endform.                               " MK_DIR
*&---------------------------------------------------------------------*
*&      Form  check_tmp_dir
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SUBRC  text
*----------------------------------------------------------------------*
form check_tmp_dir changing subrc.

  clear : tmp_file, tmp_dir.

  tmp_dir = 'C:\TEMP\'.
  perform find_dir using tmp_dir changing subrc.
  if subrc ne '1'.
    perform make_directory changing tmp_dir subrc.
    perform find_dir using tmp_dir changing subrc.
    if subrc ne '1'.
      message e011 with
       'Could not make the tmp directory for I/V printing'.
      subrc = '4'.
      exit.
    endif.
  endif.

endform.                    "check_tmp_dir
*&---------------------------------------------------------------------*
*&      Form  write_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PRI_FR_LINE  text
*      -->P_PRI_TO_LINE  text
*      -->P_TOTAL_PAGE  text
*----------------------------------------------------------------------*
form write_excel using    p_index
                          p_total_page
                          $xblnr.

  data $barcode(40).

  clear subrc.

  perform excel_start.

  if subrc eq 0.

    perform excel_sheet using 2 changing subrc.

    perform write_header using p_total_page $xblnr.

    perform excel_sheet using 3 changing subrc.

    clear $barcode.
    perform write_item  using p_index $barcode.

    if p_index eq 1.
      perform excel_sheet using 6 changing subrc.
      perform write_barcode using p_index $barcode.
    endif.

    perform excel_save.

    if p_scr eq true.

      set property of excel 'VISIBLE' = 1.
      perform excel_preview.
      set property of excel 'VISIBLE' = 0.
      perform excel_quit.

    else.
      perform excel_print.
      perform excel_quit.
    endif.
  endif.


  perform free_excel.


endform.                    " write_excel
*&---------------------------------------------------------------------*
*&      Form  excel_start
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form excel_start.

  if excel-header = space or excel-handle = -1.
    create object excel 'EXCEL.APPLICATION'.
  endif.

  if sy-subrc ne 0.
    message i011 with
    'Error occurred while opening excel'.
    subrc = '4'.
    exit.
  endif.

  set property of excel 'DisplayAlerts' = 0.

  if p_view eq true.
    set property of  excel 'VISIBLE' = 1.
  else.
    set property of  excel 'VISIBLE' = 0.
  endif.

  call method of
      excel
      'WORKBOOKS' = workbooks.
  call method of
      workbooks
      'Open'

    exporting
      #1        = filename.

  if sy-subrc ne 0.
    message i011 with
    'Error occurred while opening excel'.
    subrc = '4'.
  endif.

endform.                    " excel_start
*&---------------------------------------------------------------------*
*&      Form  excel_sheet
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2      text
*      <--P_SUBRC  text
*----------------------------------------------------------------------*
form excel_sheet using p_sheet changing $subrc.

  if sheet-handle > 0.
    free object sheet.
    sheet-handle = -1.
  endif.

  call method of
      excel
      'Worksheets' = sheet
    exporting
      #1           = p_sheet.

  if sy-subrc ne 0. $subrc = sy-subrc. exit. endif.

  call method of
      sheet
      'Activate'.

  if sheet-handle > 0.
    free object sheet.
    sheet-handle = -1.
  endif.

endform.                               " EXCEL_SHEET
*&---------------------------------------------------------------------*
*&      Form  write_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_header using p_total_page $xblnr.

  data: l_pos type i.

  define __fill_header.
    l_pos = l_pos + 1.
    perform fill_cells using &1 l_pos &2.
  end-of-definition.

  select single * from t001
         where  bukrs = it_bkpf-bukrs.

  check sy-subrc = 0.

  data: l_u_department   type ad_dprtmnt,
        l_u_name_text    type ad_namtext.
  data: l_p_department   type ad_dprtmnt,
        l_p_name_text    type ad_namtext.


  call function 'Z_FFI_GET_PERSON_INFO'
    exporting
      i_usnam      = it_bkpf-usnam
    importing
      e_department = l_u_department
      e_name_text  = l_u_name_text.

  call function 'Z_FFI_GET_PERSON_INFO'
    exporting
      i_usnam      = it_bkpf-ppnam
    importing
      e_department = l_p_department
      e_name_text  = l_p_name_text.

  __fill_header 2 :
                  it_bkpf-bukrs,
                  wa_l_company_name,
                  sy-datum, " Print date
                  sy-uzeit, " Print time
                  'USD',
                  p_total_page,
                  $xblnr,
                  p_tp1.
  l_pos = 0.
  __fill_header 4 :
                  it_bkpf-gjahr,
                  it_bkpf-blart,
                  it_bkpf-budat,
                  it_bkpf-bldat,
                  it_bkpf-cpudt,
                  it_bkpf-usnam.

  l_pos = 0.
  __fill_header 6 :
                  it_bkpf-ppnam,
                  it_bkpf-waers,
                  it_bkpf-tcode,
                  it_bkpf-xblnr,
                  it_bkpf-bstat,
                  it_bkpf-depart.

  l_pos = 0.
  __fill_header 8 :
                  it_bkpf-xprfg,
                  it_bkpf-stblg,
                  it_bkpf-stjah,
                  it_bkpf-stgrd,
                  it_bkpf-belnr,
                  it_bkpf-awtyp.

  l_pos = 0.
  __fill_header 10 :
                  it_bkpf-awkey,
                  it_bkpf-revtx(7),                        "Reverse
                  it_bkpf-cputm.

  l_pos = 0.
  __fill_header 12 :
                   it_bkpf-usnam,
                   l_u_department,
                   l_u_name_text,
                   it_bkpf-ppnam,
                   l_p_department,
                   l_p_name_text.


endform.                    " write_header
*&---------------------------------------------------------------------*
*&      Form  write_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FROM_LINE  text
*      -->P_TO_LINE  text
*----------------------------------------------------------------------*
form write_item using p_index
             changing p_barcode.

*/////////////// *

  data: count   type i value 0,          " Loop counter for list body
        count2  type i value 0,          " Counter for stripes
        help    type i,
        wa_save_belnr like bkpf-belnr,
        wa_blart      like bkpf-blart,
        wa_sgtxt      like bseg-sgtxt,
        wa_sdmbth     like bseg-dmbtr,
        wa_sdmbts     like bseg-dmbtr,
        wa_scnt       type i,
        wa_hcnt       type i.

  data: begin of l,
          txt20(20),      " account short text
          info2(48),
          mwskz,
          rate(4),
          ordno(30),
          assgn1(17),
          assgn2(12),
          text(48),
          date(12),
          dr  like bseg-dmbtr,
          cr  like bseg-dmbtr,
          name1 like lfa1-name1,
          code(20),
      end of l.

  data $blart_revtx(10).
  data $barcode(40).
  data $flag.

  data $lifnr type lifnr.
  data : fr_date type d,
         to_date type d.
  data : l_xblnr like bkpf-xblnr.

*  CLEAR : wa_sdmbth_g, wa_sdmbts_g.
*  CLEAR : wa_hcnt_g,   wa_scnt_g.
*
*/////////////// *

  data: l_pos type i.
  data: $ix type i.
  define __fill_item.
    l_pos = l_pos + 1.
    perform fill_cells using &1 l_pos &2.
  end-of-definition.

  $ix = 1.

  loop at it_bseg where ppage eq p_index.

    add 1 to $ix.

    clear it_info.

    at new belnr.
      read table it_bkpf with key belnr = it_bseg-belnr
                                  gjahr = it_bseg-gjahr.
      wa_revtx = it_bkpf-revtx.

      call function 'Z_FI_PICK_ONE_BARCODE'
        exporting
          ar_object             = p_barb  "'ZFIIV'
          sap_object            = 'BKPF'
          bukrs                 = it_bseg-bukrs
          gjahr                 = it_bseg-gjahr
          belnr                 = it_bseg-belnr
          i_barcode             = p_barcode
        importing
          barcode               = $barcode
        exceptions
          error_connectiontable = 1
          error_parameter       = 2
          others                = 3.
      if p_barcode is initial.
        p_barcode = $barcode.
      endif.
    endat.


    perform get_account_text.
    perform get_account_info.
    perform get_tax_rate.

    if wa_save_belnr ne it_bseg-belnr.
      wa_save_belnr = it_bseg-belnr.
      read table it_bkpf with key belnr = it_bseg-belnr.
      if sy-subrc = 0.
        wa_blart = it_bkpf-blart.
      endif.

      if it_info-info4 eq '.0000'.
        clear it_info-info4.
      endif.

      __fill_item $ix :
                  it_bseg-belnr,
                  it_bseg-bschl,
                  it_bseg-hkont+4(6),
                  it_info-info1,
                  it_info-info2,
                  it_info-info3,
                  it_info-info4,
                  it_info-taxinfo,
                  it_bseg-dmbts,
                  it_bseg-dmbth.
    else.
      clear $blart_revtx.

      concatenate wa_blart wa_revtx
            into $blart_revtx separated by space.

      __fill_item $ix :
                  $blart_revtx,
                  it_bseg-bschl,
                  it_bseg-hkont+4(6),
                  it_info-info1,
                  it_info-info2,
                  it_info-info3,
                  it_info-info4,
                  it_info-taxinfo,
                  it_bseg-dmbts,
                  it_bseg-dmbth.
      wa_blart = ' '.
      wa_revtx = ' '.
    endif.

    if r_nor = 'X'.
      add it_bseg-dmbth to wa_sdmbth.
      add it_bseg-dmbts to wa_sdmbts.
    elseif r_ven = 'X'.
      if it_bseg-koart = 'K' and it_bkpf-revtx is initial.
        add it_bseg-dmbth to wa_sdmbth_g.
        add it_bseg-dmbts to wa_sdmbts_g.

        add it_bseg-dmbth to wa_sdmbth.
        add it_bseg-dmbts to wa_sdmbts.
      endif.
    else.
      if it_bseg-koart = 'D'  and it_bkpf-revtx is initial.
        add it_bseg-dmbth to wa_sdmbth.
        add it_bseg-dmbts to wa_sdmbts.

        add it_bseg-dmbth to wa_sdmbth.
        add it_bseg-dmbts to wa_sdmbts.
      endif.
    endif.

    if it_bseg-dmbth ne 0.
      add 1 to wa_hcnt.
      if it_bkpf-revtx is initial.
        add 1 to wa_hcnt_g.
      endif.
    else.
      add 1 to wa_scnt.
      if it_bkpf-revtx is initial.
        add 1 to wa_scnt_g.
      endif.
    endif.

    at end of belnr.

      perform write_doc_end_excel  using wa_sdmbth
                                   wa_sdmbts
                                   wa_hcnt
                                   wa_scnt
                                   it_bkpf-xblnr
                                   changing $ix.
      clear : wa_sdmbth, wa_sdmbts.
      clear : wa_hcnt, wa_scnt.
    endat.

    add 1 to count.
    add 1 to count2.

    if r_nor <> 'X'.  "customer, vendors
      at end of sortk.  "lifnr.
        perform print_vendor_cust_sum_excel using false.
        $flag = true.
      endat.
    endif.

    at last.
      sum.
      wa_sdmbts_g = it_bseg-dmbts.
      wa_sdmbth_g = it_bseg-dmbth.
    endat.

    l_pos = 0.

  endloop.
  clear: it_bseg-belnr, it_bseg-gjahr.

  if $flag is initial.
    perform print_vendor_cust_sum_excel using true.
  endif.

endform.                    " write_item
*&---------------------------------------------------------------------*
*&      Form  fill_cells
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_&1  text
*      -->P_L_POS  text
*      -->P_&2  text
*----------------------------------------------------------------------*
form fill_cells using i j val.

  call method of
      excel
      'CELLS' = cells
    exporting
      #1      = i
      #2      = j.

  set property of cells 'VALUE' = val.
  if cells-handle > 0.
    free object cells.
  endif.


endform.                    "fill_cells
*&---------------------------------------------------------------------*
*&      Form  write_barcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FROM_LINE  text
*      -->P_TO_LINE  text
*      -->P_P_BAR_CODE  text
*----------------------------------------------------------------------*
form write_barcode using p_index
                         p_bar_code.
  data $bar_code(50).

  if p_bar_code ne space.
    concatenate '*' p_bar_code '*' into $bar_code.
    perform fill_cells using 1 1 $bar_code.
  endif.

endform.                    " write_barcode
*&---------------------------------------------------------------------*
*&      Form  excel_preview
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form excel_preview.

  if sheet-handle > 0. free object sheet. endif.
  call method of
      excel
      'Worksheets' = sheet
    exporting
      #1           = 1.
  call method of
      sheet
      'Activate' = exl_activate.
  free object exl_activate.

  call method of
      sheet
      'PrintPreview' = exl_preview.
  free object exl_preview.


endform.                               " EXCEL_PRINT
*&---------------------------------------------------------------------*
*&      Form  excel_quit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form excel_quit.

*  CALL METHOD OF workbooks 'CLOSE' = close_workbooks.
*  FREE OBJECT close_workbooks.
*  FREE OBJECT workbooks.

*  CALL METHOD OF excel 'QUIT' = exl_quit..
*  FREE OBJECT exl_quit.
*
endform.                               " EXCEL_QUIT
*&---------------------------------------------------------------------*
*&      Form  excel_print
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form excel_print.

* by ig.moon 5/14/2009 {
*  IF sheet-handle > 0. FREE OBJECT sheet. ENDIF.
*
*  CALL METHOD OF excel 'Worksheets' = sheet
*                                  EXPORTING #1 = 1.
*
*  CALL METHOD OF sheet 'Activate' = exl_activate.
*  FREE OBJECT exl_activate.

*  CALL METHOD OF sheet 'PRINTOUT'. " = exl_print.
*  FREE OBJECT exl_print.
*  FREE sheet.
*}

  get property of excel 'ACTIVEWORKBOOK' = e_work.
  call method of
      e_work
      'PRINTOUT'

    exporting
      #1         = 1
      #2         = 1.
  free object e_work.

endform.                               " EXCEL_PRINT
*&---------------------------------------------------------------------*
*&      Form  excel_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form excel_save.

  check p_save eq true.

  if sheet-handle > 0. free object sheet. endif.

  call method of
      excel
      'Worksheets' = sheet
    exporting
      #1           = 1.
  call method of
      sheet
      'Activate' = exl_activate.

  if exl_activate-handle > 0.
    free object exl_activate.
  endif.

  perform get_file_name changing subrc.

  call method of
      sheet
      'SAVEAS' = exl_saveas
    exporting
      #1       = tmp_file.
  if sy-subrc ne 0.
  endif.

  free object : sheet, exl_saveas.

  it_tmp_file-tmp_file = tmp_file.
  append it_tmp_file.

endform.                    " excel_save
*&---------------------------------------------------------------------*
*&      Form  free_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form free_excel.

**  CALL METHOD OF workbooks 'Close'. " = close_workbooks.
**  FREE OBJECT close_workbooks.
*  FREE OBJECT workbooks.
*  workbooks-HANDLE = -1.
*
**  FREE OBJECT excelsheet.
**  FREE OBJECT exl_activate.
*
*  CALL METHOD OF excel 'QUIT'. " = exl_quit..
**  FREE OBJECT exl_quit.

  SET PROPERTY OF  workbooks 'SAVED' = 1.

  CALL METHOD OF workbooks 'Close'.  " = close_workbooks.
  FREE OBJECT close_workbooks.
  FREE OBJECT workbooks.
  workbooks-HANDLE = -1.

  FREE OBJECT excelsheet.
  FREE OBJECT exl_activate.

  CALL METHOD OF excel 'QUIT'. " = exl_quit..
  FREE OBJECT exl_quit.

endform.                               " FREE_EXCEL
*&---------------------------------------------------------------------*
*&      Form  get_file_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SUBRC  text
*----------------------------------------------------------------------*
form get_file_name changing p_subrc.

  do 100 times.

    tmp_file = 'C:\TEMP\$XLSTMP$$'.
    write sy-datlo to tmp_file+17(6) yymmdd.
    tmp_file+23 = sy-timlo.
    concatenate tmp_file '.xls' into tmp_file.
    condense tmp_file no-gaps.

    call function 'WS_QUERY'
      exporting
        filename       = tmp_file
        query          = 'FE'
      importing
        return         = subrc
      exceptions
        inv_query      = 1
        no_batch       = 2
        frontend_error = 3
        others         = 4.

    if subrc ne 1. exit. endif.

*    PERFORM file_delete.
*
*    CALL FUNCTION 'WS_FILE_DELETE'
*         EXPORTING
*              file = tmp_file.
  enddo.

endform.                    " get_file_name

*---------------------------------------------------------------------*
*       FORM write_doc_end_excel                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  WA_SDMBTH                                                     *
*  -->  WA_SDMBTS                                                     *
*  -->  WA_HCNT                                                       *
*  -->  WA_SCNT                                                       *
*---------------------------------------------------------------------*
form write_doc_end_excel  using wa_sdmbth
                          wa_sdmbts
                          wa_hcnt
                          wa_scnt
                          p_xblnr
                          changing $ix.

  data l_pos type i.

  l_pos = 0.

  define __fill_item.
    perform fill_cells using &1 &2 &3.
  end-of-definition.

  data: wa_l_reverse(30).
  data $awkey type awkey.

* check reverse doc
  if not it_bkpf-stblg is initial or it_bkpf-revtx <> space.

    if it_bkpf-awtyp eq 'BKPF'.
      if it_bkpf-stgrd is initial.
        select single stgrd into it_bkpf-stgrd from bkpf
          where bukrs = it_bkpf-bukrs and
                belnr = it_bkpf-stblg and
                gjahr = it_bkpf-stjah.
      endif.
    else.
      select single * from rbkp
        where belnr = it_bkpf-awkey(10)
          and gjahr = it_bkpf-gjahr.

      if sy-subrc eq 0.
        concatenate rbkp-stblg rbkp-stjah into $awkey.

        select single belnr into it_bkpf-stblg
            from bkpf
          where awkey = $awkey
            and bukrs eq rbkp-bukrs.
      endif.
    endif.

    concatenate 'REVERSED->'
                it_bkpf-stblg '/'
                it_bkpf-stgrd
          into  wa_l_reverse.

  endif.

  add 1 to $ix.

  data $string(80).

  concatenate 'PstDt:' it_bkpf-budat '^^^' 'DocDt:' it_bkpf-bldat '^^^'
              'PstBy:' it_bkpf-usnam '^^^' wa_l_reverse into $string.
*              SEPARATED BY space.

  data $p_xblnr like it_bkpf-xblnr.

  $p_xblnr = p_xblnr.
  if p_tp1 eq true.
  else.
    clear $p_xblnr.
  endif.

  if p_tp1 eq true.

    replace 'REVERSED' with 'REV' into $string.
    concatenate $p_xblnr '^^^' $string into $string.
    condense $string.
  endif.

  do 50 times.  replace '^^^' with ' ' into $string.  enddo.

  __fill_item $ix :  "1  $p_xblnr,
                     4  $string.
*                    9  wa_sdmbts,
*                    10  wa_sdmbth.

endform.                    " write_doc_end
*&---------------------------------------------------------------------*
*&      Form  print_via_excel_normal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form print_via_excel_normal.

* for develop. {
*  READ TABLE it_bseg INDEX 1.
*  MODIFY it_bseg TRANSPORTING xblnr WHERE xblnr NE it_bseg-xblnr.
* }

  data : subrc,
         $total_page(10), $curr_page(3),
         $out_times(3).

  x_lin = 40.

  if p_save eq true.
    perform check_tmp_dir changing subrc.
    check subrc ne '4'.
  endif.

  __cls it_tmp_file.


  perform calc_sheet tables it_bseg
                     changing out_times subrc.

  pri_fr_line = 1.

  clear : wa_sdmbth_g, wa_sdmbts_g.
  clear : wa_hcnt_g,   wa_scnt_g.

  do out_times times.

    g_curr_page = sy-index.
    $curr_page = sy-index.
    $out_times = out_times.

    concatenate $curr_page '/' $out_times
      into $total_page.

    pri_to_line = sy-index * x_lin.

    perform write_excel using sy-index
                              $total_page
                              it_key-xblnr.

    pri_fr_line =  pri_to_line + 1.

    clear nex.

    if p_scr eq true and nex eq true and sy-index < out_times.

      call function 'POPUP_TO_CONFIRM_STEP'
        exporting
          defaultoption = 'Y'
          textline1     = 'Please confirm,'
          textline2     = 'Continue to next page for printing?'
          titel         = 'Next page'
          start_column  = 25
          start_row     = 6
        importing
          answer        = answer.

      if answer ne 'J'. exit. endif.

    endif.

  enddo.

endform.                    " print_via_excel_normal
*** 07/23/2013 - T00306 Start
*&---------------------------------------------------------------------*
*&      Form  ckeck_duplicate_invoice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form ckeck_duplicate_invoice.

  data : fr_date type d,
         to_date type d.
  data : l_xblnr like bkpf-xblnr.
  data : l_augdt type dats.

  data $lifnr type lifnr.

  clear: it_dupinv[], it_dupinvh1[], it_dupinvh[].

  select single lifnr into $lifnr
    from bsik
   where bukrs eq it_bkpf-bukrs
     and gjahr eq it_bkpf-gjahr
     and belnr eq it_bkpf-belnr
     and shkzg eq 'H'.
  if sy-subrc ne 0.
    select lifnr augdt into ($lifnr, l_augdt)
      from bsak
     where bukrs eq it_bkpf-bukrs
       and gjahr eq it_bkpf-gjahr
       and belnr eq it_bkpf-belnr
       and xblnr eq it_bkpf-xblnr
       and bstat eq ' '
    order by augdt descending.
      exit.
    endselect.
  endif.

  check sy-subrc eq 0.
  concatenate it_bkpf-bldat(4) : '0101' into fr_date,
                                 '1231' into to_date.
  select bukrs belnr gjahr
    from bkpf
    into table it_dupinv
   where bukrs = it_bkpf-bukrs
     and xblnr = it_bkpf-xblnr
     and bldat between fr_date and to_date.

  delete it_dupinv where belnr eq it_bkpf-belnr.
  read table it_dupinv index 1.
  if sy-subrc ne 0.
    l_xblnr = it_bkpf-xblnr.
    replace '_' with space into l_xblnr.
    replace '*' with space into l_xblnr.
    condense l_xblnr no-gaps.
    if not l_xblnr is initial.                              "UD1K921847
      select bukrs belnr gjahr   from bkpf
            into table it_dupinv
         where bukrs  =  it_bkpf-bukrs
           and xblnr =  l_xblnr
           and gjahr =  it_bkpf-gjahr.
    endif.
  endif.

  delete it_dupinv where belnr eq it_bkpf-belnr.
  if not it_dupinv[] is initial.

    select belnr dmbtr bschl
         into table it_dupinvh1  from bseg
         for all entries in it_dupinv
        where bukrs   = it_dupinv-bukrs
          and belnr   = it_dupinv-belnr
          and gjahr   = it_dupinv-gjahr
          and koart   = 'K'
          and lifnr   = $lifnr. " 12/15/2008

    select belnr dmbtr bschl
         appending table it_dupinvh1  from vbsegk
         for all entries in it_dupinv
        where ausbk   = it_dupinv-bukrs
          and belnr   = it_dupinv-belnr
          and gjahr   = it_dupinv-gjahr
          and lifnr   = $lifnr. " 12/15/2008

    loop at it_dupinvh1.
      move-corresponding it_dupinvh1 to  it_dupinvh.
      it_dupinvh-bschl = ''.
      collect it_dupinvh.
    endloop.
  endif.

  sort it_dupinvh1 by belnr descending.

endform.                    "ckeck_duplicate_invoice
*** 07/23/2013 - T00306 End
