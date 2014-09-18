REPORT zrfi02       MESSAGE-ID zmfi
                    NO STANDARD PAGE HEADING
                    LINE-SIZE   130  LINE-COUNT 90 .

*&--------------------------------------------------------------------
*& Author                 : hw.zhang
*& Creation Date          : 07/10/2003
*& Specification By       : hw.zhang
*& Development Request No : UD1K901600
*& Addl documentation     :
*& Description  : list for print out slip
*&
*& Modification Log
*&    Date      Developer      Request ID   Description
*& 02/26/2005   WSKIM          UD1K914557   Field missing
*& 01/04/06     Manjunath      UD1K918802   Bug fix / Print
*&                                          Material& Qty for retro-
*&                                          pricing line item / footer
*&                                          changes
*& 06/13/06    Manjunath      UD1K921056    Program changes to print
*&                                          tax cd & rate
*& 06/13/2013  T00303         UD1K957364    U1: Apply Archiving
*&--------------------------------------------------------------------
INCLUDE: <symbol>.
************************************************************************
************************* Global data **********************************
************************************************************************
TABLES: t001,    "Company code
        t005,    "Countris
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
DATA: BEGIN OF it_bkpf OCCURS 0,
      blart    LIKE  bkpf-blart,       "Doc. Type.
      budat    LIKE  bkpf-budat,       "Posting Date
      bldat    LIKE  bkpf-bldat,       "doc. date
      cpudt    LIKE  bkpf-cpudt,       "Enrry data
      usnam    LIKE  bkpf-usnam,       "User Name
      ppnam    LIKE  bkpf-ppnam,       "Parked by
      waers    LIKE  bkpf-waers,       "Currency key
      tcode    LIKE  bkpf-tcode,       "Transaction Code
      xblnr    LIKE  bkpf-xblnr,       "Reference
      bstat    LIKE  bkpf-bstat,       "Doc. Status
      depart   LIKE  adcp-department,  "Dept Name
      xprfg    LIKE  vbkpf-xprfg,      "Doc. Complete
      stblg    LIKE  bkpf-stblg,       "Reverse doc no
      stjah    LIKE  bkpf-stjah,       "Reverse year
      stgrd    LIKE  bkpf-stgrd,       "Reverse reason
      bukrs    LIKE  bkpf-bukrs,       "Company Code
      gjahr    LIKE  bkpf-gjahr,       "Fiscal Year
      belnr    LIKE  bkpf-belnr,       "Doc. No.
*----- Appended by BSBAE. 2004.06.08
      awtyp    LIKE  bkpf-awtyp,       "Reference procedure
      awkey    LIKE  bkpf-awkey,       "Objectkey
      revtx(7),                        "Reverse
*----- Appended by BSBAE. 2004.06.08
      cputm    LIKE  bkpf-cputm,
      END OF it_bkpf.

* BSEG : G/L Data segment
DATA: BEGIN OF it_bseg OCCURS 0,
      sortk(10) TYPE c,
      bukrs    LIKE  bkpf-bukrs,
      gjahr    LIKE  bkpf-gjahr,
      belnr    LIKE  bkpf-belnr,
      buzei    LIKE  bseg-buzei,   "Line Item
      lifnr    LIKE  bseg-lifnr,   "Vendor
      xblnr    LIKE  bkpf-xblnr,

      revtx(7) TYPE c,
      stgrd    LIKE  bkpf-stgrd,       "Reverse reason
      empfb    LIKE  bseg-empfb,   "payer/payee
      hkont    LIKE  bseg-hkont,   "G/L Account
      txt20    LIKE  skat-txt20,   "Short Text
      bschl    LIKE  bseg-bschl,   "Posting Key
      accnt    LIKE  bseg-kunnr,   "C/V/A
      name1    LIKE  kna1-name1,   "C/V/A Name
      aufnr    LIKE  bseg-aufnr,   "Order No.
      fistl    LIKE  bseg-fistl,   "Funds Center
      kostl    LIKE  bseg-kostl,   "Cost Center
      fkber    LIKE  bseg-fkber,   "Func
      ktext    LIKE  cepct-ktext,  "Cost Obj Text
      zuonr    LIKE  bseg-zuonr,   "Assignment
      umskz    LIKE  bseg-umskz,   "Sp. G/L Ind.
      sgtxt    LIKE  bseg-sgtxt,   "Text
      mwskz    LIKE  vbsega-mwskz, "Tax Code
      fwbas    LIKE  bseg-fwbas,
      txjcd    LIKE  vbsega-txjcd, "Jurisdiction
      bupla    LIKE  bseg-bupla,   "Biz Place (KR)
      jrtxt    LIKE  ttxjt-text1,  "Tax Name
      wmwst    LIKE  vbsegd-wmwst, "Tax Amt
      dmbtr    LIKE  bseg-dmbtr,   "Amt in Loc. Cur.
      s_gumak  LIKE  bseg-dmbtr,   "Debit Amt
      h_gumak  LIKE  bseg-dmbtr,   "Credit Amt
      shkzg    LIKE  bseg-shkzg,   "Dr/Cr
      kunnr    LIKE  bseg-kunnr,   "Customer
*      lifnr    LIKE  bseg-lifnr,   "Vendor
      anln1    LIKE  bseg-anln1,   "Asset
      anln2    LIKE  bseg-anln2,   "Asset sub
      anbwa    LIKE  bseg-anbwa,   "Asset Transaction(3)
      koart    LIKE  bseg-koart,   "Account Type
      saknr    LIKE  bseg-saknr,   "G/L Acct
      valut    LIKE  bseg-valut,   "Value Date
      zterm    LIKE  bseg-zterm,   "Pay Term
      zlsch    LIKE  bseg-zlsch,   "Pay Method
      zlspr    LIKE  bseg-zlspr,   "Pay Block
      augbl    LIKE  bseg-augbl,   "Clearing Doc
      pswsl    LIKE  bseg-pswsl,   "Currency
      zfbdt    LIKE  bseg-zfbdt,   "
      fdtag    LIKE  bseg-fdtag,
      dmbth    LIKE  bseg-dmbtr,
      dmbts    LIKE  bseg-dmbtr,
      matnr    LIKE  bseg-matnr,   "Material#
      menge    LIKE  bseg-menge,   "qty
      meins    LIKE  bseg-meins,   "unit
      ebeln    LIKE  bseg-ebeln,   "PO#
      ebelp    LIKE  bseg-ebelp,   "poitem
      buzid    LIKE  bseg-buzid,   "internal key
      ktosl    LIKE  bseg-ktosl,   "internal acct key
    END OF it_bseg.
*
*----- Appended by BSBAE. 2004.06.08
*data: it_bseg_sum like it_bseg occurs 0 with header line.

DATA: BEGIN OF it_bseg_sum_v  OCCURS 0,
      bukrs    LIKE  bkpf-bukrs,
      gjahr    LIKE  bkpf-gjahr,
      lifnr    LIKE  bseg-lifnr,   "Vendor
      belnr    LIKE  bkpf-belnr,
      buzei    LIKE  bseg-buzei,   "Line Item
      hkont    LIKE  bseg-hkont,   "G/L Account
      txt20    LIKE  skat-txt20,   "Short Text
      bschl    LIKE  bseg-bschl,   "Posting Key
      accnt    LIKE  bseg-kunnr,   "C/V/A
      name1    LIKE  kna1-name1,   "C/V/A Name
      aufnr    LIKE  bseg-aufnr,   "Order No.
      fistl    LIKE  bseg-fistl,   "Funds Center
      kostl    LIKE  bseg-kostl,   "Cost Center
      ktext    LIKE  cepct-ktext,  "Cost Obj Text
      zuonr    LIKE  bseg-zuonr,   "Assignment
      umskz    LIKE  bseg-umskz,   "Sp. G/L Ind.
      sgtxt    LIKE  bseg-sgtxt,   "Text
      mwskz    LIKE  vbsega-mwskz, "Tax Code
      fwbas    LIKE  bseg-fwbas,
      txjcd    LIKE  vbsega-txjcd, "Jurisdiction
      bupla    LIKE  bseg-bupla,   "Biz Place (KR)
      jrtxt    LIKE  ttxjt-text1,  "Tax Name
      wmwst    LIKE  vbsegd-wmwst, "Tax Amt
      dmbtr    LIKE  bseg-dmbtr,   "Amt in Loc. Cur.
      s_gumak  LIKE  bseg-dmbtr,   "Debit Amt
      h_gumak  LIKE  bseg-dmbtr,   "Credit Amt
      shkzg    LIKE  bseg-shkzg,   "Dr/Cr
      kunnr    LIKE  bseg-kunnr,   "Customer
*      lifnr    LIKE  bseg-lifnr,   "Vendor
      anln1    LIKE  bseg-anln1,   "Asset
      anln2    LIKE  bseg-anln2,   "Asset sub
      anbwa    LIKE  bseg-anbwa,   "Asset Transaction(3)
      koart    LIKE  bseg-koart,   "Account Type
      saknr    LIKE  bseg-saknr,   "G/L Acct
      valut    LIKE  bseg-valut,   "Value Date
      zterm    LIKE  bseg-zterm,   "Pay Term
      zlsch    LIKE  bseg-zlsch,   "Pay Method
      zlspr    LIKE  bseg-zlspr,   "Pay Block
      augbl    LIKE  bseg-augbl,   "Clearing Doc
      pswsl    LIKE  bseg-pswsl,   "Currency
      zfbdt    LIKE  bseg-zfbdt,   "
      fdtag    LIKE  bseg-fdtag,
      dmbth  LIKE bseg-dmbtr,
      dmbts  LIKE bseg-dmbtr,
      matnr    LIKE  bseg-matnr,   "Material#
      menge    LIKE  bseg-menge,   "qty
      meins    LIKE  bseg-meins,   "unit
      ebeln    LIKE  bseg-ebeln,   "PO#
      buzid    LIKE  bseg-buzid,   "internal key
      ktosl    LIKE  bseg-ktosl,   "internal acct key
      END OF it_bseg_sum_v.

*----- Appended by BSBAE. 2004.06.08

DATA: BEGIN OF it_info,
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
        dr  LIKE bseg-dmbtr,
        cr  LIKE bseg-dmbtr,
        drt  LIKE bseg-dmbtr, "tax
        crt  LIKE bseg-dmbtr, "tax
        name1 LIKE lfa1-name1,
        maktx LIKE makt-maktx,
        code(20),
    END OF it_info.

DATA : it_t041c LIKE t041c OCCURS 0 WITH HEADER LINE.

DATA :  wa_sdmbth_g   LIKE bseg-dmbtr,
        wa_sdmbts_g   LIKE bseg-dmbtr,
        wa_sdmbth_d   LIKE bseg-dmbtr,
        wa_sdmbts_d   LIKE bseg-dmbtr,
        wa_sdmbth_t   LIKE bseg-dmbtr,
        wa_sdmbts_t   LIKE bseg-dmbtr,
        wa_ttext      LIKE tstct-ttext,
        wa_scnt_g     TYPE i,
        wa_hcnt_g     TYPE i,
        wa_total_page TYPE i,
        wa_total_page1 TYPE i,
        wa_first_page  TYPE i,
        wa_revtx(7),
        wa_txt10(10)      TYPE c.
*
DATA: stripes  TYPE c VALUE ' ',        " Stripes Y/N
      keys     TYPE c VALUE ' ',        " Key columns Y/N
      wa_width TYPE i VALUE 130,
      wa_ul  LIKE sy-uline VALUE '-',
      wa_vl  LIKE sy-vline VALUE '|',
      wa_sp  LIKE space    VALUE ' ',
      l_s(1) TYPE c VALUE ';'.

TYPE-POOLS: vrm.
DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.


DATA: wa_l_name1(40), wa_l_name2(40), wa_l_company_name(80).
DATA : end_line,
       f_num TYPE i.

*- U1 start
DATA: gt_bkpf_a TYPE TABLE OF bkpf WITH HEADER LINE,
      gt_bseg_a TYPE TABLE OF bseg WITH HEADER LINE,
      gt_konp_a TYPE TABLE OF konp WITH HEADER LINE.
DATA: BEGIN OF gt_tax_a OCCURS 0,
      kschl LIKE konp-kschl,
      kbetr LIKE konp-kbetr,
      END OF gt_tax_a.
*- U1 End
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
PARAMETER     p_bukrs  LIKE bkpf-bukrs MEMORY ID buk.

SELECT-OPTIONS:
  s_belnr  FOR   bkpf-belnr," memory id bln,
  s_gjahr  FOR   bkpf-gjahr MEMORY ID gjr.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-040 FOR FIELD r_ven.
PARAMETER r_ven RADIOBUTTON GROUP r_gr DEFAULT 'X'.
SELECTION-SCREEN COMMENT 30(10) text-050 FOR FIELD r_cus.
PARAMETER r_cus RADIOBUTTON GROUP r_gr.
SELECTION-SCREEN COMMENT 50(10) text-060 FOR FIELD r_nor.
PARAMETER r_nor RADIOBUTTON GROUP r_gr.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
  s_blart  FOR   bkpf-blart MEMORY ID bar,
  s_budat  FOR   bkpf-budat,
  s_xblnr  FOR   bkpf-xblnr,
  s_cpudt  FOR   bkpf-cpudt.

SELECTION-SCREEN END OF BLOCK b0.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-020.
PARAMETER:  p_own    TYPE c AS CHECKBOX DEFAULT 'X',
            p_detail TYPE c AS CHECKBOX,
            p_exrev  AS CHECKBOX.
*---start#1 wskim 03/01/2005
SELECT-OPTIONS:
  s_zlspr  FOR t008-zahls,
  s_lifnr  FOR bseg-lifnr.
*ELECTION-SCREEN SKIP 1.

*---end
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-021.
PARAMETERS : pr_dest LIKE pri_params-pdest,
             pr_imm TYPE c.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN COMMENT 3(40) text-c01.
*---start#1 wskim 02/26/2005
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-030.
PARAMETER : p_parked AS CHECKBOX DEFAULT ' ',
            p_limit  TYPE i  DEFAULT '2000'.
*            p_posted AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK b2.
*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End
*-------------------------------------------------------*
AT SELECTION-SCREEN.
*-------------------------------------------------------*
  IF p_bukrs IS INITIAL.
    MESSAGE e000(zmfi) WITH 'Company code is not defined'.
  ENDIF.

  IF s_gjahr-low IS INITIAL.
    MESSAGE w000(zmfi) WITH 'Fiscal year is not defined'.
  ENDIF.
*  IF p_parked IS INITIAL AND p_posted IS INITIAL.
*    MESSAGE e000(zmfi) WITH 'You have to check in parked or in posted'.
*  ENDIF.
*
*  IF s_gjahr-low IS INITIAL.
*    MESSAGE w000(zmfi) WITH 'Fiscal year is not defined'.
*  ENDIF.

*-------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*-------------------------------------------------------*
** Authority check
  AUTHORITY-CHECK OBJECT 'Z_BKPF_BES' ID 'BRGRU'
                  FIELD '*'.
  IF sy-subrc <> 0.
    AUTHORITY-CHECK OBJECT 'Z_BKPF_BES' ID 'BRGRU'
                    FIELD 'FI'.
    IF sy-subrc <> 0.
      p_own = 'X'.
      LOOP AT SCREEN.
        IF screen-name = 'P_OWN'.
          screen-input =  0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

************************************************************************
*************************** Main Program *******************************
************************************************************************
INITIALIZATION.
*  p_parked = 'X'.
*  p_posted = 'X'.
* List output
*  wa_width = 130.                          " wa_width of list
*  s_gjahr-low = sy-datum(4).
*  APPEND s_gjahr.

  s_cpudt-low  = sy-datum.
  s_cpudt-high = sy-datum.
  APPEND s_cpudt.
*---START#1 WSKIM 03/02/2005
  CLEAR:pr_dest,pr_imm.
*---END

START-OF-SELECTION.
* Authority check
*  DATA: wa_fi(2) VALUE 'FI'.
*  AUTHORITY-CHECK OBJECT 'Z_BKPF_BES'
*               ID 'BRGRU'   FIELD wa_fi.
*  IF sy-subrc NE 0.
*    MESSAGE s000 WITH 'You are not authorized to use ' sy-repid.
*    EXIT.
*  ENDIF.
*

  SET PF-STATUS 'LIBS1'.

*  READ TEXTPOOL sy-repid INTO tp LANGUAGE sy-langu.
*  LOOP AT tp WHERE id = 'R'.
*    SET TITLEBAR '001' WITH tp-text.
*    EXIT.
*  ENDLOOP.
*
  PERFORM get_company_info.

  PERFORM get_bkpf_data.

  DATA: l_lines LIKE sy-index.
  DESCRIBE TABLE it_bkpf LINES l_lines.
  IF l_lines = 0.
    MESSAGE s000(zmfi) WITH 'No data found'.
    EXIT.
  ELSEIF l_lines > p_limit.
    MESSAGE s000(zmfi) WITH 'Too many documents selected'.
    EXIT.
  ENDIF.

* read detail information.
  PERFORM get_bseg_details.

  PERFORM process_ap_filter.

  PERFORM set_debit_credit.

  PERFORM print_option.

END-OF-SELECTION.
  IF p_exrev = 'X'.
    DELETE it_bseg WHERE revtx <> space.
  ENDIF.

  PERFORM adjust_data.
  PERFORM writing_data.


TOP-OF-PAGE.
  PERFORM heading.

END-OF-PAGE.
  PERFORM end_of_page.

AT LINE-SELECTION.
*  IF sy-lisel = 'BELNR'.
  IF it_bseg-belnr <> space.
    SET PARAMETER ID:'BLN' FIELD it_bseg-belnr,
                     'BUK' FIELD p_bukrs,
                     'GJR' FIELD it_bseg-gjahr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDIF.

************************************************************************
*************************** Form Routines ******************************
************************************************************************

*---------------------------------------------------------------------*
*       FORM writing_data                                             *
*---------------------------------------------------------------------*
*       List output                                                   *
*---------------------------------------------------------------------*
FORM writing_data.
* PERFORM heading.

*SORT IT FOR PRINT
  IF r_nor = 'X'.
    IF p_detail = 'X'.
      SORT it_bseg BY gjahr belnr buzei.
    ELSE.
      SORT it_bseg BY gjahr belnr bschl.
    ENDIF.
  ELSEIF r_ven = 'X'.
    IF p_detail = 'X'.
      SORT it_bseg BY sortk gjahr belnr buzei.
    ELSE.
      SORT it_bseg BY sortk gjahr belnr bschl.
    ENDIF.
  ELSE.
    IF p_detail = 'X'.
      SORT it_bseg BY sortk gjahr belnr buzei.
    ELSE.
      SORT it_bseg BY sortk gjahr belnr bschl.
    ENDIF.
  ENDIF.

  PERFORM write_body.

  PERFORM draw_sign_box USING space.
  SKIP.
  NEW-LINE NO-SCROLLING.


ENDFORM.                    "writing_data
*---------------------------------------------------------------------*
*       FORM HEADING                                                  *
*---------------------------------------------------------------------*
*       Writes the heading of the list                                *
*---------------------------------------------------------------------*
FORM heading.

  FORMAT INTENSIFIED OFF.
  WRITE :/.
  NEW-LINE NO-SCROLLING.
  WRITE: /(130)'S u m m a r y    D o c u m e n t' CENTERED.
  " List title
  SKIP.
**-> t001: Company Codes
  SELECT SINGLE * FROM t001
         WHERE  bukrs = it_bkpf-bukrs.
  CHECK sy-subrc = 0.
  WRITE:/    'Company code :', it_bkpf-bukrs, '-',  wa_l_company_name,
        98   'Print date : ' NO-GAP,
              sy-datum  MM/DD/YYYY,
              sy-uzeit  USING EDIT MASK '__:__:__'.

  WRITE:/    'Currency : USD'.
  IF r_ven EQ 'X' OR r_cus EQ 'X'.
    WRITE:      110 'Page:' NO-GAP, wa_first_page NO-GAP,
               '/ '    NO-GAP,
               (2) wa_total_page1 NO-GAP LEFT-JUSTIFIED.
  ELSE.
    WRITE:      110 'Page:' NO-GAP, sy-pagno NO-GAP,
               '/ '    NO-GAP,
               (3) wa_total_page NO-GAP LEFT-JUSTIFIED.
  ENDIF.

*  WRITE :/115 'page : ', (3) sy-pagno no-gap,
*              '/' no-gap, (3) wa_total_page no-gap.
  NEW-LINE.
  ULINE AT (wa_width).
  WRITE: / sy-vline NO-GAP,            " Left border
          (12) 'Document No'   NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
          (03) 'PK'           NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
          (11) 'Account'      NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
*         (03) 'PK'           no-gap color col_heading intensified off,
          (20) 'Text'         NO-GAP LEFT-JUSTIFIED
                                     COLOR COL_HEADING INTENSIFIED OFF,
          (31) 'Info'         NO-GAP CENTERED
                                     COLOR COL_HEADING INTENSIFIED OFF,
          (5)  'Tax'          NO-GAP RIGHT-JUSTIFIED
                                     COLOR COL_HEADING INTENSIFIED OFF,
          (22) 'Debit'        NO-GAP RIGHT-JUSTIFIED
                                     COLOR COL_HEADING INTENSIFIED OFF,
          (22) 'Credit'       NO-GAP RIGHT-JUSTIFIED
                                     COLOR COL_HEADING INTENSIFIED OFF,
          (2) ' '             NO-GAP RIGHT-JUSTIFIED
                                     COLOR COL_HEADING INTENSIFIED OFF,
          sy-vline .
  ULINE AT (wa_width).                    " Line below titles


ENDFORM.                    "heading

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
FORM write_body.
  DATA: count   TYPE i VALUE 0,          " Loop counter for list body
        count2  TYPE i VALUE 0,          " Counter for stripes
        help    TYPE i,
        wa_save_belnr LIKE bkpf-belnr,
        wa_blart      LIKE bkpf-blart,
        wa_sgtxt      LIKE bseg-sgtxt,
        wa_sdmbth     LIKE bseg-dmbtr,
        wa_sdmbts     LIKE bseg-dmbtr,
        wa_scnt       TYPE i,
        wa_hcnt       TYPE i.
  DATA: BEGIN OF l,
          txt20(20),      " account short text
          info2(48),
          mwskz,
          rate(4),
          ordno(30),
          assgn1(17),
          assgn2(12),
          text(48),
          date(12),
          dr  LIKE bseg-dmbtr,
          cr  LIKE bseg-dmbtr,
          name1 LIKE lfa1-name1,
          code(20),
      END OF l.
  CLEAR : wa_sdmbth_g, wa_sdmbts_g.
  CLEAR : wa_hcnt_g,   wa_scnt_g.


*  stripes = 'X'.
  LOOP AT it_bseg.
    CLEAR it_info.
*---start wskim 03/11/2005
    IF r_nor <> 'X'.  "customer, vendor
      AT NEW sortk.  "lifnr.
        wa_first_page  = 1.
        wa_total_page1 = 1.
        f_num = f_num + 1.
        IF f_num >= 85.
          wa_total_page1 = wa_total_page1 + 1.
        ENDIF.
        NEW-PAGE.
      ENDAT.
    ENDIF.
*---end

*    AT NEW lifnr.
*      NEW-PAGE.
*    ENDAT.

*------------------------------------------------------------
    AT NEW belnr.
      READ TABLE it_bkpf WITH KEY belnr = it_bseg-belnr
                                  gjahr = it_bseg-gjahr.
      wa_revtx = it_bkpf-revtx.
    ENDAT.

*------------------------------------------------------------

    PERFORM get_account_text.
    PERFORM get_account_info.
    PERFORM get_tax_rate.

* print document line.................................................
    PERFORM handle_stripe USING count2.
    PERFORM write_doc_lines  USING wa_save_belnr   wa_blart  wa_revtx.


*
    IF r_nor = 'X'.
      ADD it_bseg-dmbth TO wa_sdmbth.
      ADD it_bseg-dmbts TO wa_sdmbts.
    ELSEIF r_ven = 'X'.
      IF it_bseg-koart = 'K' AND it_bkpf-revtx IS INITIAL.
        ADD it_bseg-dmbth TO wa_sdmbth_g.
        ADD it_bseg-dmbts TO wa_sdmbts_g.

        ADD it_bseg-dmbth TO wa_sdmbth.
        ADD it_bseg-dmbts TO wa_sdmbts.
      ENDIF.
    ELSE.
      IF it_bseg-koart = 'D'  AND it_bkpf-revtx IS INITIAL.
        ADD it_bseg-dmbth TO wa_sdmbth.
        ADD it_bseg-dmbts TO wa_sdmbts.

        ADD it_bseg-dmbth TO wa_sdmbth.
        ADD it_bseg-dmbts TO wa_sdmbts.
      ENDIF.
    ENDIF.

    IF it_bseg-dmbth NE 0.
      ADD 1 TO wa_hcnt.
      IF it_bkpf-revtx IS INITIAL.
        ADD 1 TO wa_hcnt_g.
      ENDIF.
    ELSE.
      ADD 1 TO wa_scnt.
      IF it_bkpf-revtx IS INITIAL.
        ADD 1 TO wa_scnt_g.
      ENDIF.
    ENDIF.

* print document end.................................................
    AT END OF belnr.
      PERFORM write_doc_end  USING wa_sdmbth
                                   wa_sdmbts
                                   wa_hcnt
                                   wa_scnt.
      CLEAR : wa_sdmbth, wa_sdmbts.
      CLEAR : wa_hcnt, wa_scnt.
    ENDAT.

    ADD 1 TO count.
    ADD 1 TO count2.


*-------Start
    IF r_nor <> 'X'.  "customer, vendors
*      at end of xblnr.
*
*      endat.
      AT END OF sortk.  "lifnr.
        DO.
          IF sy-linno > 54. "26
            EXIT.
          ENDIF.
*    WRITE :/.
          PERFORM write_empty_line.
        ENDDO.

        PERFORM print_vendor_cust_sum.

        PERFORM draw_sign_box USING 'X'.
*          NEW-PAGE.
      ENDAT.
    ENDIF.
*--------END

    HIDE: it_bseg-belnr, it_bseg-gjahr.

*------- LAST
    AT LAST.
      SUM.
      wa_sdmbts_g = it_bseg-dmbts.
      wa_sdmbth_g = it_bseg-dmbth.
    ENDAT.

  ENDLOOP.
  CLEAR: it_bseg-belnr, it_bseg-gjahr.
ENDFORM.                    "write_body

*---------------------------------------------------------------------*
*       FORM SUM_PRINT
*---------------------------------------------------------------------*
*       Writes row with final totals                                  *
*---------------------------------------------------------------------*
FORM sum_print.
  ULINE AT (wa_width).
  FORMAT COLOR COL_TOTAL INTENSIFIED.  " Color of final totals
  WRITE : / sy-vline,
          (59) ' ',
          '( Dr:',
          (6) wa_scnt_g NO-GAP,
          '/ CR:',
          (6) wa_hcnt_g NO-GAP,
          ')',
          (2) '     ',
          (18) wa_sdmbts_g NO-ZERO,
          (18) wa_sdmbth_g NO-ZERO,
          sy-vline.
  ULINE AT (wa_width).
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.  " Color of final totals

ENDFORM.                    "sum_print

************************************************************************
*************************** Events *************************************
************************************************************************

AT USER-COMMAND.
  CASE sy-ucomm.
*    WHEN 'COLA'.
*      PERFORM sh_left(ergphelp).
*
*    WHEN 'COL-'.
*      PERFORM sh_minus(ergphelp).
*
*    WHEN 'COL+'.
*      PERFORM sh_plus(ergphelp).
*
*    WHEN 'COLZ'.
*      PERFORM sh_right(ergphelp).

*    WHEN 'ERLE'.
*      txt_report = 'ERGP2530'.
*      CALL FUNCTION 'ERGO_TEXT_SHOW'
*           EXPORTING
*                textname       = txt_report
*                id             = 'RE'
*                langu          = sy-langu
*           EXCEPTIONS
*                text_not_found = 01.

    WHEN 'STRI'.
      sy-lsind = 0.
      IF stripes EQ 'X'.
        stripes = ' '.
      ELSE.
        stripes = 'X'.
      ENDIF.
      PERFORM heading.
      PERFORM writing_data.

*    WHEN 'KEY'.
*      sy-lsind = 0.
*      IF keys EQ 'X'.
*        keys = ' '.
*      ELSE.
*        keys = 'X'.
*      ENDIF.
*      PERFORM writing_data.
*
  ENDCASE.
*&------------------------------------------------------*
*&      Form  BSEG_READ_PROCESS
*&------------------------------------------------------*
FORM bseg_read_process.
  SELECT *
    FROM bseg
*   appending corresponding fields of table it_bseg
   WHERE bukrs = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.

    MOVE-CORRESPONDING bseg TO it_bseg.
    IF it_bseg-bschl > 39.
      CLEAR: it_bseg-lifnr, it_bseg-kunnr.

    ENDIF.

    it_bseg-xblnr = it_bkpf-xblnr.
    it_bseg-revtx = it_bkpf-revtx.
    it_bseg-stgrd = it_bkpf-stgrd.

* append/collect
    PERFORM append_collect_bseg.

  ENDSELECT.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bseg.
    LOOP AT gt_bseg_a.
      MOVE-CORRESPONDING gt_bseg_a TO it_bseg.
      IF it_bseg-bschl > 39.
        CLEAR: it_bseg-lifnr, it_bseg-kunnr.

      ENDIF.

      it_bseg-xblnr = it_bkpf-xblnr.
      it_bseg-revtx = it_bkpf-revtx.
      it_bseg-stgrd = it_bkpf-stgrd.

* append/collect
      PERFORM append_collect_bseg.
    ENDLOOP.
  ENDIF.
*- U1 End

ENDFORM.                    "bseg_read_process

*&------------------------------------------------------*
*&      Form  VBSEGS_READ_PROCESS
*&------------------------------------------------------*
FORM vbsegs_read_process.
  SELECT *
    FROM vbsegs
*   APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   WHERE ausbk = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.
    MOVE-CORRESPONDING vbsegs TO it_bseg.
    it_bseg-hkont = it_bseg-saknr.
    it_bseg-koart = 'S'.

    it_bseg-xblnr = it_bkpf-xblnr.

* append/collect
    PERFORM append_collect_bseg.

  ENDSELECT.
ENDFORM.                    "vbsegs_read_process

*&------------------------------------------------------*
*&      Form  VBSEGK_READ_PROCESS
*&------------------------------------------------------*
FORM vbsegk_read_process.
  SELECT *
    FROM vbsegk
*    APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   WHERE ausbk = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.
    MOVE-CORRESPONDING vbsegk TO it_bseg.
    it_bseg-koart = 'K'.

    it_bseg-xblnr = it_bkpf-xblnr.

* append/collect
    PERFORM append_collect_bseg.

  ENDSELECT.

ENDFORM.                    "vbsegk_read_process

*&------------------------------------------------------*
*&      Form  VBSEGD_READ_PROCESS
*&------------------------------------------------------*
FORM vbsegd_read_process.
  SELECT *
    FROM vbsegd
*   APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   WHERE ausbk = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.

    MOVE-CORRESPONDING vbsegd TO it_bseg.
    it_bseg-koart = 'D'.

    it_bseg-xblnr = it_bkpf-xblnr.

* append/collect
    PERFORM append_collect_bseg.

  ENDSELECT.

ENDFORM.                    "vbsegd_read_process

*&------------------------------------------------------*
*&      Form  VBSEGA_READ_PROCESS
*&------------------------------------------------------*
FORM vbsega_read_process.
  SELECT *
    FROM vbsega
*    APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   WHERE ausbk = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.

    MOVE-CORRESPONDING vbsega TO it_bseg.
    it_bseg-koart = 'A'.

    it_bseg-xblnr = it_bkpf-xblnr.

    PERFORM append_collect_bseg.

  ENDSELECT.

ENDFORM.                    "vbsega_read_process


*&---------------------------------------------------------------------*
*&      Form  draw_sign_box
*&---------------------------------------------------------------------*
FORM draw_sign_box  USING f_split.
  IF f_split = space.
*---end
    IF sy-linno >= 57.
      DO.
        IF sy-linno > 87  ."56.
          EXIT.
        ENDIF.
*    WRITE :/.
        PERFORM write_empty_line.
      ENDDO.
      NEW-PAGE.
*    PERFORM heading.
    ENDIF.

    DO.
      IF sy-linno > 54 ."56.
        EXIT.
      ENDIF.
*    WRITE :/.
      PERFORM write_empty_line.
    ENDDO.

    PERFORM sum_print.

    FORMAT   INTENSIFIED OFF.
    NEW-LINE. ULINE AT (wa_width).
  ENDIF.

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

  WRITE:/  wa_vl NO-GAP, (35)  'Document Processing Warnings:'
            NO-GAP CENTERED,
             (93)  space       NO-GAP CENTERED,
           wa_vl NO-GAP.


  WRITE:/  wa_vl NO-GAP, (128)  space
            NO-GAP CENTERED,
           wa_vl NO-GAP.


  WRITE:/  wa_vl NO-GAP, (128) space    NO-GAP CENTERED,
           wa_vl NO-GAP.
  WRITE:/  wa_vl NO-GAP, (128) space    NO-GAP CENTERED,
           wa_vl NO-GAP.
  WRITE:/  wa_vl NO-GAP, (128) space    NO-GAP CENTERED,
           wa_vl NO-GAP.
* End of changes - UD1K918802

  NEW-LINE. ULINE AT (wa_width).

  NEW-LINE. ULINE AT (wa_width).

*--------Department approval  date finance approval date
  WRITE:/  wa_vl NO-GAP, (22)  'Req.Department'   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  'Approval'     NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  'Date'         NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  'Finance Div.'      NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  'Approval'     NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  'Date'         NO-GAP CENTERED,
           wa_vl NO-GAP.
  NEW-LINE. ULINE AT (wa_width).

*---------------------Head of department
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
*

*write:/wa_vl no-gap, (22)'Head of Department'no-gapcentered,UD1K918802
  WRITE:/  wa_vl NO-GAP, (22)  'Requestor'   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
*           wa_vl no-gap, (20)  'Cost Accounting'  no-gap centered,
           wa_vl NO-GAP, (20)  'Processor' NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
           wa_vl NO-GAP.
*
*  write:/  wa_vl no-gap, (22)  space   no-gap centered,UD1K918802
  WRITE:/  wa_vl NO-GAP, (22)  space  NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
          wa_vl NO-GAP.
  NEW-LINE. ULINE AT (wa_width).
*---------------------Head of sub division
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
*
*write:/wa_vl no-gap,(22)'Head of Sub Division'no-gapcentered,UD1K918802
  WRITE:/  wa_vl NO-GAP, (22)  'Head of Department'   NO-GAP CENTERED,
               wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
               wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
*             wa_vl no-gap, (20)  'General Accounting'  no-gap centered,
              wa_vl NO-GAP, (20)  'Manager'  NO-GAP CENTERED,
               wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
               wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
               wa_vl NO-GAP.
*
*  write:/  wa_vl no-gap, (22)  space   no-gap centered,"UD1K918802
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
          wa_vl NO-GAP.
  NEW-LINE. ULINE AT (wa_width).
*---------------------Head of  division
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  'Head of Department'  NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
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


  WRITE:/  wa_vl NO-GAP, (22)  'Chief of Division'   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  '          ----------' NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  '                 -------------'
           NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  '-------------'    NO-GAP CENTERED,
           wa_vl NO-GAP.
* End of changes - UD1K918802
*
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  '          |Treasury '   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  '      |              '
           NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.

*  WRITE :/ wa_vl NO-GAP,
*           (128) wa_ul  NO-GAP,
*           wa_vl NO-GAP.

  NEW-LINE. ULINE AT (wa_width).

*---------------------President / CEO
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
*
* write:/wa_vl no-gap, (22)'President / CEO' no-gap centered,"UD1K918802
  WRITE:/  wa_vl NO-GAP, (22)  'President/CEO'   NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
          wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
          wa_vl NO-GAP, (20)  'CFO'  NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
          wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
          wa_vl NO-GAP.
*
*  write:/  wa_vl no-gap, (22)  space   no-gap centered, "UD1K918802
  WRITE:/ wa_vl NO-GAP, (22) space NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
          wa_vl NO-GAP.
  NEW-LINE. ULINE AT (wa_width).


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

ENDFORM.                    " draw_sign_box

*&---------------------------------------------------------------------*
*&      Form  get_position_id
*&---------------------------------------------------------------------*
FORM get_position_id CHANGING p_code p_name.
  DATA: l_objnr LIKE imzo-objnr,
        l_posnr LIKE imzo-posnr,
        l_posid LIKE impr-posid.

*  p_assgn1 = '#############'.
  CONCATENATE 'OR'  it_bseg-aufnr INTO l_objnr.
  SELECT SINGLE posnr INTO l_posnr
    FROM imzo WHERE objnr EQ l_objnr.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.
  SELECT SINGLE a~posid b~post1 INTO (p_code, p_name)
    FROM impr AS a INNER JOIN impu AS b ON b~posnr EQ a~posnr
    WHERE a~posnr EQ l_posnr AND
          b~spras EQ sy-langu.
ENDFORM.                    " get_position_id
*&---------------------------------------------------------------------*
*&      Form  write_empty_line
*&---------------------------------------------------------------------*
FORM write_empty_line.
  DO 3 TIMES.
    FORMAT   INTENSIFIED OFF.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE :/(01) sy-vline,
           (127) ' ' NO-GAP,
          (01) sy-vline.
  ENDDO.
  NEW-LINE. ULINE AT (wa_width).
ENDFORM.                    " write_empty_line

*&---------------------------------------------------------------------*
*&      Form  print_vendor_cust_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_vendor_cust_sum.
  ULINE AT (wa_width).
  FORMAT COLOR COL_TOTAL INTENSIFIED.  " Color of final totals
  IF wa_sdmbts_d NE '0'.
    wa_sdmbts_g = wa_sdmbts_g - wa_sdmbts_d.
    wa_sdmbts_t = wa_sdmbts_g + wa_sdmbts_d.
  ELSEIF wa_sdmbts_d EQ '0'.
    wa_sdmbts_t =  wa_sdmbts_g.
  ENDIF.
  IF wa_sdmbth_d NE '0'.
    wa_sdmbth_g = wa_sdmbth_g - wa_sdmbth_d.
    wa_sdmbth_t = wa_sdmbth_g + wa_sdmbth_d.
  ELSEIF wa_sdmbth_d EQ '0'.
    wa_sdmbth_t = wa_sdmbth_g .
  ENDIF.
*1st line : General
* write : / sy-vline, 75 'Payable/Receivable:', "UD1K918802
  WRITE : / sy-vline, 75 'Receivable/Payable:',             "UD1K918802
           93(18)  wa_sdmbts_g NO-ZERO,
           111(18) wa_sdmbth_g NO-ZERO,
        sy-vline.
**2nd line Difference
*  write : / sy-vline,
*          (45) ' ',
*          '( Dr:',
*          (6) wa_scnt_g no-gap,
*          '/ CR:',
*          (6) wa_hcnt_g no-gap,
*          ')',
*          (2) ' ',
*          75 text-093,    "Difference account
*          93(18) wa_sdmbts_d no-zero,
*          111(18) wa_sdmbth_d no-zero,
*          sy-vline.
**3rd Total
*  write : / sy-vline, 75 text-094,  "Total Amount
*           93(18)  wa_sdmbts_t no-zero,
*           111(18) wa_sdmbth_t no-zero,
*        sy-vline.

  ULINE AT (wa_width).
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.  " Color of final totals
  CLEAR : wa_scnt_g, wa_hcnt_g, wa_sdmbts_g, wa_sdmbth_g,
          wa_sdmbts_d,
          wa_sdmbth_d.
ENDFORM.                    "print_vendor_cust_sum
*&---------------------------------------------------------------------*
*&      Form  print_option
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_option.
  CHECK NOT pr_dest IS INITIAL ."

  NEW-PAGE PRINT ON
         DESTINATION pr_dest
            IMMEDIATELY pr_imm
            KEEP IN SPOOL 'X'
            LINE-COUNT 90
            LINE-SIZE 130
            LAYOUT 'ZX_90_130'
            NO DIALOG.

ENDFORM.                    " print_option
*&---------------------------------------------------------------------*
*&      Form  end_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM end_of_page.
*  IF end_line EQ space AND sy-pagno <> wa_total_page.
*    NEW-LINE.
*    ULINE AT (wa_width).
*  ENDIF.

ENDFORM.                    " end_of_page
*&---------------------------------------------------------------------*
*&      Form  filtering_bseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1473   text
*----------------------------------------------------------------------*
FORM filtering_bseg USING p_mark.
  DATA w_int TYPE i.
  CLEAR w_int.
  LOOP AT it_bseg.
    IF it_bseg-koart NE p_mark.
      READ TABLE it_bseg WITH KEY  belnr = it_bseg-belnr
                                       koart = p_mark.
      IF sy-subrc <> 0.
        DELETE TABLE it_bseg FROM it_bseg.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE it_bseg LINES w_int.
  IF w_int = 0.
    CASE p_mark.
      WHEN 'K'.
        MESSAGE i011 WITH text-090.
        LEAVE TO TRANSACTION  'ZRFIG02'.
      WHEN 'D'.
        MESSAGE i011 WITH text-091.
        LEAVE TO TRANSACTION  'ZRFIG02'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " filtering_bseg
*&---------------------------------------------------------------------*
*&      Form  get_bkpf_data
*&---------------------------------------------------------------------*
FORM get_bkpf_data.
  RANGES : r_usnam FOR bkpf-usnam,
           r_ppnam FOR bkpf-ppnam. "UD1K918802.
  IF p_own EQ 'X'.
    r_usnam-sign   = 'I'.
    r_usnam-option = 'EQ'.
    r_usnam-low    = sy-uname.
    APPEND r_usnam.
* Begin of changes - UD1K918802
    r_ppnam-sign = 'I'.
    r_ppnam-option = 'EQ'.
    r_ppnam-low = sy-uname.
    APPEND r_ppnam.
* End of changes - UD1K918802
  ENDIF.


  RANGES : r_bstat FOR bkpf-bstat.
  r_bstat-sign   = 'I'.   r_bstat-option = 'EQ'.

  IF p_parked = space.
    r_bstat-low    = ' '.  APPEND r_bstat.
    r_bstat-low    = 'A'.  APPEND r_bstat.
    r_bstat-low    = 'B'.  APPEND r_bstat.
    r_bstat-low    = 'D'.  APPEND r_bstat.
    r_bstat-low    = 'S'.  APPEND r_bstat.
  ELSE.  "'X'.
    r_bstat-low    = 'V'.  APPEND r_bstat.
  ENDIF.


  SELECT bukrs belnr gjahr bstat
       budat usnam ppnam tcode xblnr
       bldat cpudt blart bktxt waers
       stblg stjah stgrd awtyp awkey
  FROM bkpf    INTO CORRESPONDING FIELDS OF TABLE it_bkpf
  WHERE bukrs =  p_bukrs AND
        belnr IN s_belnr AND
        gjahr IN s_gjahr AND
        blart IN s_blart AND
        budat IN s_budat AND
        xblnr IN s_xblnr AND
        cpudt IN s_cpudt AND
       ( usnam IN r_usnam OR ppnam IN r_ppnam ) AND         "UD1K918802
        bstat IN r_bstat.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bkpf.
  ENDIF.
*- U1 End

  DATA: l_idx LIKE sy-tabix.

  LOOP AT it_bkpf.
    l_idx = sy-tabix.
*----- Appended by BSBAE. 2004.06.08
    CASE it_bkpf-awtyp.
      WHEN 'MKPF'.
        DATA: lv_awkey LIKE bkpf-awkey.

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
        IF it_bkpf-tcode = 'VL09'
        OR it_bkpf-tcode = 'MBST'.

          IF it_bkpf-stgrd IS INITIAL.
            SELECT SINGLE stgrd INTO it_bkpf-stgrd FROM bkpf
              WHERE bukrs = it_bkpf-bukrs AND
                    belnr = it_bkpf-stblg AND
                    gjahr = it_bkpf-stjah.
*- U1 Start
            IF p_arch EQ 'X' AND it_bkpf-stgrd IS INITIAL.
              PERFORM archive_read_bkpf_2
                USING it_bkpf-bukrs it_bkpf-stblg it_bkpf-stjah
                CHANGING it_bkpf-stgrd.
            ENDIF.
*- U1 End
          ENDIF.

          it_bkpf-revtx = 'REV'.
          MODIFY it_bkpf INDEX l_idx.
* FIXME
* original document....use reference, entry date/time...

        ENDIF.

      WHEN 'RMRP'.
        SELECT SINGLE *
          FROM rbkp
         WHERE stblg = it_bkpf-awkey(10)
           AND stjah = it_bkpf-awkey+10(4).
        IF sy-subrc EQ 0.
          it_bkpf-revtx = 'REV'.
          MODIFY it_bkpf INDEX l_idx.

          CONCATENATE rbkp-belnr rbkp-stjah INTO lv_awkey.
          READ TABLE it_bkpf WITH KEY awtyp = 'RMRP'
                                      awkey = lv_awkey.
          IF sy-subrc EQ 0.
            it_bkpf-revtx = 'REV'.
            MODIFY it_bkpf INDEX sy-tabix TRANSPORTING revtx.
          ENDIF.
*- U1 Start
        ELSE.
          IF p_arch EQ 'X'.
            PERFORM archive_read_rbkp USING it_bkpf-awkey(10)
                                            it_bkpf-awkey+10(4).
            IF sy-subrc EQ 0.
              it_bkpf-revtx = 'REV'.
              MODIFY it_bkpf INDEX l_idx.

              CONCATENATE rbkp-belnr rbkp-stjah INTO lv_awkey.
              READ TABLE it_bkpf WITH KEY awtyp = 'RMRP'
                                          awkey = lv_awkey.
              IF sy-subrc EQ 0.
                it_bkpf-revtx = 'REV'.
                MODIFY it_bkpf INDEX sy-tabix TRANSPORTING revtx.
              ENDIF.
            ENDIF.
          ENDIF.
*- U1 End
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
*----- Appended by BSBAE. 2004.06.08


  ENDLOOP.
ENDFORM.                    " get_bkpf_data
*&---------------------------------------------------------------------*
*&      Form  get_bseg_details
*&---------------------------------------------------------------------*
FORM get_bseg_details.
  LOOP AT it_bkpf.

    CASE  it_bkpf-bstat.
      WHEN 'V'.
        PERFORM  vbsegs_read_process.  "G/L Account Document Parking
        PERFORM  vbsegk_read_process.  "Vendor Document Parking
        PERFORM  vbsegd_read_process.  "Customer Document Parking
        PERFORM  vbsega_read_process.  "Asset Document Parking
*        PERFORM  vbset_read_process.   "Taxes Document Parking

* Normal Document
      WHEN OTHERS.
        PERFORM  bseg_read_process.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " get_bseg_details
*&---------------------------------------------------------------------*
*&      Form  set_debit_credit
*&---------------------------------------------------------------------*
FORM set_debit_credit.
  LOOP AT it_bseg.
    CLEAR it_t041c.
    IF it_bseg-stgrd <> space.
      READ TABLE it_t041c WITH KEY stgrd = it_bseg-stgrd.
    ENDIF.

    IF it_t041c-xnegp = 'X'.   "negative posting
      IF it_bseg-shkzg = 'H'.
        it_bseg-dmbts = - it_bseg-dmbtr.
        it_bseg-dmbth = 0.
      ELSE.
        it_bseg-dmbth = - it_bseg-dmbtr.
        it_bseg-dmbts = 0.
      ENDIF.
    ELSE.
      IF it_bseg-shkzg = 'H'.
        MOVE it_bseg-dmbtr TO it_bseg-dmbth.
        MOVE 0             TO it_bseg-dmbts.
      ELSE.
        MOVE it_bseg-dmbtr TO it_bseg-dmbts.
        MOVE 0             TO it_bseg-dmbth.
      ENDIF.
    ENDIF.

    MODIFY it_bseg   TRANSPORTING dmbth dmbts.
  ENDLOOP.

ENDFORM.                    " set_debit_credit
*&---------------------------------------------------------------------*
*&      Form  process_ap_filter
*&---------------------------------------------------------------------*
FORM process_ap_filter.
*---START#1 WSKIM 03/01/2005
  DATA: l_chk(1) TYPE c.

  CHECK s_zlspr-low <> space OR s_lifnr-low <> space.

  LOOP AT  it_bseg WHERE ( koart EQ 'K' ).
    IF  it_bseg-zlspr IN s_zlspr
    AND it_bseg-lifnr IN s_lifnr.

    ELSE.
      DELETE it_bkpf  WHERE belnr = it_bseg-belnr.
      DELETE it_bseg  WHERE belnr = it_bseg-belnr.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " process_ap_filter
*&---------------------------------------------------------------------*
*&      Form  get_company_info
*&---------------------------------------------------------------------*
FORM get_company_info.
**-> t001: Company Codes
  SELECT SINGLE * FROM t001
     WHERE  bukrs = p_bukrs.
  CHECK sy-subrc = 0.
  SELECT SINGLE name1 name2 INTO (wa_l_name1, wa_l_name2)
  FROM adrc
  WHERE addrnumber = t001-adrnr AND
      date_from <= sy-datum.
*  CONCATENATE wa_l_name1 ' ' Wa_l_name2 INTO wa_l_company_name.
  wa_l_company_name =  wa_l_name1.
  DATA: l_pos TYPE i.
  l_pos = strlen( wa_l_company_name ) + 1.
  wa_l_company_name+l_pos = wa_l_name2.


  SELECT * INTO TABLE it_t041c
    FROM t041c.

ENDFORM.                    " get_company_info
*&---------------------------------------------------------------------*
*&      Form  write_doc_end
*&---------------------------------------------------------------------*
FORM write_doc_end  USING wa_sdmbth
                          wa_sdmbts
                          wa_hcnt
                          wa_scnt.

  DATA: wa_l_reverse(30).

* check reverse doc
  IF NOT it_bkpf-stblg IS INITIAL OR it_bkpf-revtx <> space.
    IF it_bkpf-stgrd IS INITIAL.
      SELECT SINGLE stgrd INTO it_bkpf-stgrd FROM bkpf
        WHERE bukrs = it_bkpf-bukrs AND
              belnr = it_bkpf-stblg AND
              gjahr = it_bkpf-stjah.
*- U1 Start
      IF p_arch EQ 'X' AND it_bkpf-stgrd IS INITIAL.
        PERFORM archive_read_bkpf_2
          USING it_bkpf-bukrs it_bkpf-stblg it_bkpf-stjah
          CHANGING it_bkpf-stgrd.
      ENDIF.
*- U1 End

    ENDIF.
    CONCATENATE ' REVERSED->'
                it_bkpf-stblg '/'
                it_bkpf-stgrd
          INTO  wa_l_reverse.
  ENDIF.

*    IF it_info-info2 = ' '.
*      READ TABLE it_bkpf WITH KEY belnr = it_bseg-belnr.
*      if sy-subrc = 0.
*        clear wa_ttext.
*        select single ttext into wa_ttext
*        from TSTCT
*        where SPRSL = SY-LANGU
*        AND   Tcode = it_bkpf-tcode.
*
*        it_info-info2 = it_bkpf-tcode.
*        concatenate
*          it_bkpf-tcode ' ' wa_ttext+0(20) into it_info-info2.
*      endif.
*    ENDIF.

  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE : / sy-vline,
          'PstDt:',(10) it_bkpf-budat, '   DocDt:', it_bkpf-bldat,
          '  ', (16) it_bkpf-xblnr,
          (30) wa_l_reverse,
*          (8) ' ',
*          '(Dr:'  NO-GAP,
*          (3) wa_scnt NO-GAP,
*          '/ CR:' NO-GAP,
*          (3) wa_hcnt NO-GAP,
*          ')',
         92(18) wa_sdmbts NO-ZERO,
           (18) wa_sdmbth NO-ZERO,
          sy-vline.
  ULINE AT (wa_width).
ENDFORM.                    " write_doc_end
*&---------------------------------------------------------------------*
*&      Form  append_collect_bseg
*&---------------------------------------------------------------------*
FORM append_collect_bseg.
  IF p_detail = 'X'.
    APPEND it_bseg.
  ELSE.
    IF it_bseg-matnr <> space.
      CLEAR: it_bseg-zuonr.
    ENDIF.
* Begin of changes - UD1K918802
    IF it_bseg-ktosl EQ 'RAP'.
      CLEAR: it_bseg-buzei, it_bseg-meins,
            it_bseg-sgtxt, it_bseg-zuonr.
      CLEAR: it_bseg-ebeln, it_bseg-ebelp.
      CLEAR: it_bseg-meins, it_bseg-augbl.

    ELSE.
      CLEAR: it_bseg-buzei, it_bseg-matnr, it_bseg-meins,
            it_bseg-sgtxt, it_bseg-zuonr.
      CLEAR: it_bseg-ebeln, it_bseg-ebelp.
      CLEAR: it_bseg-meins, it_bseg-augbl,
             it_bseg-ktosl.
    ENDIF.
* End of changes - UD1K918802
    COLLECT it_bseg.
  ENDIF.
ENDFORM.                    " append_collect_bseg
*&---------------------------------------------------------------------*
*&      Form  write_doc_lines
*&---------------------------------------------------------------------*
FORM write_doc_lines  USING wa_save_belnr  wa_blart  wa_revtx.
  IF wa_save_belnr NE it_bseg-belnr.
    wa_save_belnr = it_bseg-belnr.
    READ TABLE it_bkpf WITH KEY belnr = it_bseg-belnr.
    IF sy-subrc = 0.
      wa_blart = it_bkpf-blart.
    ENDIF.

    WRITE : / sy-vline,
             it_bseg-belnr,
             it_bseg-bschl,
*              it_bseg-buzei,
             it_bseg-hkont+4(6) RIGHT-JUSTIFIED,
*              it_bseg-sgtxt,
           (20) it_info-info1,
                it_info-info2,
                it_info-info3,
                it_info-info4 RIGHT-JUSTIFIED,
                it_info-taxinfo,
           (18) it_bseg-dmbts NO-ZERO,
           (18) it_bseg-dmbth NO-ZERO,
             sy-vline.
  ELSE.
    WRITE : / sy-vline,
            wa_blart,
            wa_revtx,
            it_bseg-bschl,
*             it_bseg-buzei,
            it_bseg-hkont+4(6) RIGHT-JUSTIFIED,
*              it_bseg-sgtxt,
          (20) it_info-info1,
               it_info-info2,
               it_info-info3,
               it_info-info4 RIGHT-JUSTIFIED,
               it_info-taxinfo,
          (18) it_bseg-dmbts NO-ZERO,
          (18) it_bseg-dmbth NO-ZERO,
            sy-vline.
    wa_blart = ' '.
    wa_revtx = ' '.
  ENDIF.

ENDFORM.                    " write_doc_lines
*&---------------------------------------------------------------------*
*&      Form  get_account_text
*&---------------------------------------------------------------------*
FORM get_account_text.
  SELECT SINGLE txt20 INTO it_info-info1
    FROM skat
   WHERE spras = sy-langu
     AND ktopl = t001-ktopl
     AND saknr = it_bseg-hkont.
ENDFORM.                    " get_account_text
*&---------------------------------------------------------------------*
*&      Form  get_account_info
*&---------------------------------------------------------------------*
FORM get_account_info.
  SELECT SINGLE * FROM skb1
    WHERE bukrs EQ it_bkpf-bukrs AND
          saknr EQ it_bseg-hkont.

*---------------------------------------------------------------------
  CASE it_bseg-koart.
    WHEN 'A'. "asset
      PERFORM fill_asset_info.
    WHEN 'D'. "customers
      PERFORM fill_cust_info.
    WHEN 'K'. "vendors
      PERFORM fill_vend_info.
    WHEN 'M'. "material
      IF p_detail = 'X'.
        it_info-info2 = it_bseg-matnr.
        WRITE: it_bseg-menge TO  it_info-info3 RIGHT-JUSTIFIED."left
      ENDIF.
    WHEN 'S'. "G/L accounts
      IF it_bseg-hkont+4(2) = '16'    OR
         it_bseg-hkont+4(3) = '901'.
        PERFORM fill_invest_info.
      ELSEIF it_bseg-hkont+4(1) = '6'.
        PERFORM fill_expense_info.
      ELSE.
        IF it_bseg-bschl >= '80'.
          PERFORM fill_material_info.
        ELSEIF it_bseg-buzid = 'F'.   " PO freight, duty, ...
          IF p_detail = 'X'.
            it_info-info2 = it_bseg-matnr.
            it_info-info3 = it_bseg-zuonr.
            WRITE: it_bseg-menge TO  it_info-info4 DECIMALS 0.
          ENDIF.
*****>>> GRIR - Misc Import Expense
        ELSEIF it_bkpf-blart = 'KI'.
          it_info-info2 = it_bseg-sgtxt.
        ELSEIF skb1-fdlev(1) CA 'CB'.
          CONCATENATE   it_bseg-fdtag+4(2) '/'
                        it_bseg-fdtag+6(2) '/'
                        it_bseg-fdtag+0(4)
                  INTO it_info-info3.
* Default
        ELSE.
          it_info-info2 = it_bseg-zuonr.
          it_info-info3 = it_bseg-sgtxt.
        ENDIF.

      ENDIF.

  ENDCASE.

ENDFORM.                    " get_account_info
*&---------------------------------------------------------------------*
*&      Form  adjust_data
*&---------------------------------------------------------------------*
FORM adjust_data.
  DATA: ind TYPE i,
        l_lifnr LIKE bseg-lifnr,
        l_belnr LIKE bseg-belnr.

  CHECK r_nor <> 'X'.

*Sort for vendor,customer
  SORT it_bseg BY gjahr belnr bschl .

  LOOP AT it_bseg.
    ind = sy-tabix.

    IF l_lifnr = space AND l_belnr <> it_bseg-belnr.
      l_lifnr = it_bseg-lifnr.
      l_belnr = it_bseg-belnr.
    ELSE.
      l_belnr = it_bseg-belnr.
    ENDIF.

    IF l_belnr = it_bseg-belnr.
      it_bseg-sortk = l_lifnr.
      MODIFY it_bseg INDEX ind.
    ELSE.
      CLEAR: l_lifnr, l_belnr.
    ENDIF.

*    MODIFY it_bseg TRANSPORTING sortk
*             WHERE belnr = it_bseg-belnr
*               AND gjahr = it_bseg-gjahr
*               AND koart <> 'K'.
*                   AND bschl = '40'.
  ENDLOOP.

*  DELETE it_bseg WHERE lifnr = space.
  DELETE it_bseg WHERE sortk = space.


  IF r_ven EQ 'X'.
    PERFORM filtering_bseg USING 'K'.
  ELSEIF r_cus EQ 'X'.
    PERFORM filtering_bseg USING 'D'.
  ENDIF.
ENDFORM.                    " adjust_data
*&---------------------------------------------------------------------*
*&      Form  get_tax_rate
*&---------------------------------------------------------------------*
FORM get_tax_rate.
  DATA: l_rate TYPE p DECIMALS 1.
  DATA: ls_rate(4).

*    move it_bseg-mwskz to it_info-mwskz.
  IF it_bseg-bschl > 39.

    it_info-taxinfo = it_bseg-mwskz.

* tax account...
    IF it_bseg-fwbas NE 0.
      l_rate = it_bseg-dmbtr / it_bseg-fwbas * 100.
      WRITE l_rate TO ls_rate.
      it_info-info3 = ls_rate.

      SELECT SINGLE text1 INTO it_info-info2 FROM t007s
        WHERE spras = sy-langu AND
              kalsm = 'TAXUS' AND
              mwskz = it_bseg-mwskz.
    ENDIF.
  ENDIF.


*park document
  DATA: BEGIN OF it_tax OCCURS 0,
           kschl LIKE konp-kschl,
           kbetr LIKE konp-kbetr,
        END OF it_tax.
  DATA: wa_l_tax LIKE bseg-dmbtr.

  IF it_bseg-mwskz NE space AND  " exist tax code
     it_bseg-shkzg EQ 'S'   AND  " only debit
     it_bkpf-bstat EQ 'V'.       " only parked doc.


    SELECT b~kschl b~kbetr INTO TABLE it_tax
      FROM a003 AS a INNER JOIN konp AS b ON b~knumh EQ a~knumh
      WHERE a~kappl = 'TX' AND
            a~aland = 'US' AND
            a~mwskz EQ it_bseg-mwskz.

*- U1 start
    IF p_arch EQ 'X'.
      PERFORM archive_read_konp USING it_bseg-mwskz.
      IF NOT gt_tax_a[] IS INITIAL.
        INSERT LINES OF gt_tax_a INTO TABLE it_tax.
      ENDIF.
    ENDIF.
*- U1 End

    LOOP AT it_tax WHERE kschl+3(1) EQ 'I'.    "case A/P
      wa_l_tax = it_bseg-dmbtr * it_tax-kbetr / 1000.
      ADD wa_l_tax TO it_info-drt.
    ENDLOOP.

    it_info-info3 = it_info-drt.
  ENDIF.

* Begin of changes - UD1K921056
  IF it_bseg-koart EQ 'K'.
    PERFORM get_withholding_tax.
  ENDIF.

* End of changes - UD1K921056

ENDFORM.                    " get_tax_rate
*&---------------------------------------------------------------------*
*&      Form  fill_material_info
*&---------------------------------------------------------------------*
FORM fill_material_info.
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

  IF it_bseg-bschl = '81' OR it_bseg-bschl = '91'.  "Cost
    IF it_bseg-aufnr <> space.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = it_bseg-aufnr
        IMPORTING
          output = it_info-info3.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = it_bseg-kostl
        IMPORTING
          output = it_info-info3.
    ENDIF.

  ELSEIF it_bseg-bschl = '83' OR it_bseg-bschl = '93'.
* Begin of changes - UD1K918802
* For Retro Pricing line item Print Material/Qty
    IF it_bseg-ktosl EQ 'RAP'.
      WRITE it_bseg-matnr TO it_info-info2.
      WRITE it_bseg-menge TO it_info-info3.
      CONDENSE it_info-info3.
    ELSE.
      CONCATENATE it_bseg-ebeln it_bseg-ebelp INTO it_info-info3.
    ENDIF.
* End of changes - UD1K918802

*****>>> GRIR
  ELSEIF it_bseg-bschl = '96' OR it_bseg-bschl = '86'.
    IF p_detail = 'X'.
      IF it_bkpf-tcode = 'MRER'.
        it_info-info3 = it_bseg-ebeln(10).
        it_info-info2 = it_bseg-sgtxt.
        WRITE: it_bseg-menge TO  it_info-info4 DECIMALS 0.
      ELSE.
        it_info-info3 = it_bseg-ebeln(10).
        it_info-info2 = it_bseg-matnr.
        WRITE: it_bseg-menge TO  it_info-info4 DECIMALS 0.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " fill_material_info
*&---------------------------------------------------------------------*
*&      Form  fill_asset_info
*&---------------------------------------------------------------------*
FORM fill_asset_info.
  SELECT SINGLE txt50
    FROM anla       "Asset Master Record Segment
    INTO it_info-name1
   WHERE bukrs = it_bkpf-bukrs
     AND anln1 = it_bseg-anln1  "Asset
     AND anln2 = it_bseg-anln2. "Sub-Number

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = it_bseg-anln1
    IMPORTING
      output = it_info-code.

  CONCATENATE it_info-code     '-'
              it_bseg-anln2 '('
              it_bseg-anbwa ')'
         INTO it_info-code.
  IF it_bseg-aufnr IS INITIAL.
    SELECT SINGLE eaufn INTO it_bseg-aufnr
      FROM anla
      WHERE bukrs EQ it_bseg-bukrs AND
            anln1 EQ it_bseg-anln1 AND
            anln2 EQ it_bseg-anln2.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = it_bseg-aufnr
    IMPORTING
      output = it_info-info3.

*      MOVE it_bseg-aufnr  TO  it_info-assgn2.
  DATA: wa_l_temp(20).
  PERFORM get_position_id CHANGING it_info-info2 wa_l_temp.

ENDFORM.                    " fill_asset_info
*&---------------------------------------------------------------------*
*&      Form  fill_cust_info
*&---------------------------------------------------------------------*
FORM fill_cust_info.
  SELECT SINGLE name1
    FROM kna1       "General Data in Customer Master
    INTO it_info-name1
   WHERE kunnr = it_bseg-kunnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = it_bseg-kunnr
    IMPORTING
      output = it_info-info2.
  IF it_bseg-umskz <> ' '.
    CONCATENATE it_info-info2 '('  it_bseg-umskz  ')'
                INTO it_info-info2.
  ENDIF.

*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)
  CONCATENATE it_bseg-zterm it_bseg-zlsch it_bseg-zlspr
         INTO it_info-info3 SEPARATED BY '/'.

ENDFORM.                    " fill_cust_info
*&---------------------------------------------------------------------*
*&      Form  fill_vend_info
*&---------------------------------------------------------------------*
FORM fill_vend_info.
  DATA: l_lifnr LIKE lfa1-lifnr,
        l_payterm(8) TYPE c.

*   --> account type (name + vendor + space.gl)
  IF it_bseg-empfb <> space.  "Alt.Payee
    SELECT SINGLE * FROM lfa1
      WHERE lifnr EQ it_bseg-empfb.
  ELSE.
    SELECT SINGLE * FROM lfa1
      WHERE lifnr EQ it_bseg-lifnr.
  ENDIF.

  IF lfa1-xcpdk = 'X'.  " one time
    SELECT SINGLE * FROM bsec
        WHERE bukrs = it_bseg-bukrs
          AND gjahr = it_bseg-gjahr
          AND belnr = it_bseg-belnr.
    it_info-info2 = bsec-name1.
  ELSE.
    it_info-info2 = lfa1-name1.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = it_bseg-lifnr
    IMPORTING
      output = l_lifnr.

  CLEAR l_payterm.
  IF it_bseg-zterm <> space.
    CONCATENATE it_bseg-zterm '/'
                it_bseg-zlsch '/'
                it_bseg-zlspr
           INTO l_payterm.
  ENDIF.

  IF it_bseg-umskz <> ' '.
    CONCATENATE l_lifnr ';'
                '('  it_bseg-umskz  ')'
                l_payterm
           INTO it_info-info3.
  ELSE.
    CONCATENATE l_lifnr ';'
                l_payterm
           INTO it_info-info3.
  ENDIF.

  IF it_bseg-fdtag <> space.
    CONCATENATE it_bseg-fdtag+4(2) '/' it_bseg-fdtag+6(2)
           INTO it_info-info4.
  ENDIF.

ENDFORM.                    " fill_vend_info
*&---------------------------------------------------------------------*
*&      Form  fill_expense_info
*&---------------------------------------------------------------------*
FORM fill_expense_info.
  DATA: it_list TYPE TABLE OF ifmkacoo WITH HEADER LINE.
  RANGES: r_kostl FOR bseg-kostl.

  IF ( it_bseg-fistl IS INITIAL ) AND
     ( NOT it_bseg-kostl IS INITIAL ).
    REFRESH r_kostl.
    r_kostl-sign = 'I'.  r_kostl-option = 'EQ'.
    r_kostl-low  = it_bseg-kostl.
    APPEND r_kostl.
    CALL FUNCTION 'HHM_KONT_READ_FROM_CO_OBJEKT'
      EXPORTING
        i_fikrs = 'H201'
        i_kokrs = 'H201'
        i_kostl = 'X'
      TABLES
        t_kostl = r_kostl
        t_list  = it_list.

    LOOP AT it_list.
      MOVE it_list-fistl TO it_bseg-fistl.
    ENDLOOP.
  ENDIF.

  SELECT SINGLE bezeich FROM fmfctrt INTO it_info-name1
   WHERE fictr = it_bseg-fistl.  "Funds Center

*.......cc/fc
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = it_bseg-kostl
    IMPORTING
      output = it_info-info2.

  CONCATENATE it_info-info2 '(' it_bseg-fkber ')/' it_bseg-fistl
         INTO it_info-info2.
*.......order
  it_info-info3 = it_bseg-aufnr.


ENDFORM.                    " fill_expense_info
*&---------------------------------------------------------------------*
*&      Form  fill_invest_info
*&---------------------------------------------------------------------*
FORM fill_invest_info.
  it_info-info3 = it_bseg-aufnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = it_bseg-aufnr
    IMPORTING
      output = it_info-assign1.
  PERFORM get_position_id CHANGING it_info-info2
                                   it_info-name1.
ENDFORM.                    " fill_invest_info
*&---------------------------------------------------------------------*
*&      Form  handle_stripe
*&---------------------------------------------------------------------*
FORM handle_stripe USING f_cnt.
  DATA: even    TYPE i VALUE 0.          " Checker even/odd

* Stripe handling (list write_body)
  IF stripes = ' '.                 " No Stripes
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.                              " Stripes
    even = f_cnt MOD 2.              " Check for even/odd
    IF even = '0'.
      FORMAT COLOR COL_NORMAL INTENSIFIED.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDIF.
  ENDIF.

ENDFORM.                    " handle_stripe
*&---------------------------------------------------------------------*
*&      Form  get_withholding_tax
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_withholding_tax.
  SELECT SINGLE * FROM lfbw
      WHERE   lifnr EQ it_bseg-lifnr AND
              bukrs EQ it_bseg-bukrs.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM t059z  WHERE land1 = 'US'
                             AND witht = lfbw-witht
                             AND wt_withcd = lfbw-wt_withcd.
    IF t059z-xqfor EQ 'X' .
      SELECT SINGLE * FROM t059fb WHERE land1 = t059z-land1 AND
                                        witht =  t059z-witht AND
                                       wt_withcd = t059z-wt_withcd.
      it_info-info4 =  t059fb-qsatz.
      it_info-taxinfo =    t059fb-wt_withcd.
    ELSE.
      it_info-info4 =  t059z-qsatz.
      it_info-taxinfo =    t059z-wt_withcd.
    ENDIF.
  ENDIF.


ENDFORM.                    " get_withholding_tax
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bkpf .

  TYPES: BEGIN OF ty_bkpf,
         bukrs      TYPE bukrs,
         belnr      TYPE belnr_d,
         gjahr      TYPE gjahr,
         blart      TYPE blart,
         budat      TYPE budat,
         xblnr      TYPE xblnr1,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_bkpf.

  DATA: l_handle    TYPE sytabix,
        lt_bkpf     TYPE TABLE OF bkpf WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bkpf TYPE TABLE OF ty_bkpf,
        ls_inx_bkpf TYPE ty_bkpf.

  RANGES : lr_usnam FOR bkpf-usnam,
           lr_ppnam FOR bkpf-ppnam.

  IF p_own EQ 'X'.
    lr_usnam-sign   = 'I'.
    lr_usnam-option = 'EQ'.
    lr_usnam-low    = sy-uname.
    APPEND lr_usnam.
    lr_ppnam-sign = 'I'.
    lr_ppnam-option = 'EQ'.
    lr_ppnam-low = sy-uname.
    APPEND lr_ppnam.
  ENDIF.

  RANGES : lr_bstat FOR bkpf-bstat.
  lr_bstat-sign   = 'I'.   lr_bstat-option = 'EQ'.

  IF p_parked = space.
    lr_bstat-low    = ' '.  APPEND lr_bstat.
    lr_bstat-low    = 'A'.  APPEND lr_bstat.
    lr_bstat-low    = 'B'.  APPEND lr_bstat.
    lr_bstat-low    = 'D'.  APPEND lr_bstat.
    lr_bstat-low    = 'S'.  APPEND lr_bstat.
  ELSE.  "'X'.
    lr_bstat-low    = 'V'.  APPEND lr_bstat.
  ENDIF.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'SAP_FI_DOC_002'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bkpf[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bkpf
    FROM (l_gentab)
   WHERE bukrs =  p_bukrs AND
         belnr IN s_belnr AND
         gjahr IN s_gjahr AND
         blart IN s_blart AND
         budat IN s_budat AND
         xblnr IN s_xblnr.

  CHECK NOT lt_inx_bkpf[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bkpf_a, gt_bkpf_a[].
  LOOP AT lt_inx_bkpf INTO ls_inx_bkpf.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bkpf-archivekey
        offset                    = ls_inx_bkpf-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bkpf, lt_bkpf[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BKPF'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bkpf
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bkpf[] IS INITIAL.

    DELETE lt_bkpf WHERE cpudt NOT IN s_cpudt
                      OR ( usnam NOT IN lr_usnam AND ppnam NOT IN lr_ppnam )
                      OR bstat NOT IN lr_bstat.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bkpf INTO TABLE gt_bkpf_a.
  ENDLOOP.

  SORT gt_bkpf_a.
  DELETE ADJACENT DUPLICATES FROM gt_bkpf_a COMPARING ALL FIELDS.

  LOOP AT gt_bkpf_a.
    MOVE-CORRESPONDING gt_bkpf_a TO it_bkpf.
    APPEND it_bkpf.  CLEAR it_bkpf.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_BKPF
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BKPF_BUKRS  text
*      -->P_IT_BKPF_STBLG  text
*      -->P_IT_BKPF_STJAH  text
*      <--P_IT_BKPF_STGRD  text
*----------------------------------------------------------------------*
FORM archive_read_bkpf_2  USING    p_bukrs
                                   p_belnr
                                   p_gjahr
                          CHANGING p_stgrd.

  TYPES: BEGIN OF ty_bkpf,
         bukrs      TYPE bukrs,
         belnr      TYPE belnr_d,
         gjahr      TYPE gjahr,
         blart      TYPE blart,
         budat      TYPE budat,
         xblnr      TYPE xblnr1,
         archivekey TYPE arkey,
         archiveofs TYPE admi_offst.
  TYPES: END OF ty_bkpf.

  DATA: l_handle    TYPE sytabix,
        lt_bkpf     TYPE TABLE OF bkpf WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bkpf TYPE TABLE OF ty_bkpf,
        ls_inx_bkpf TYPE ty_bkpf.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'SAP_FI_DOC_002'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bkpf[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bkpf
    FROM (l_gentab)
   WHERE bukrs = p_bukrs AND
         belnr = p_belnr AND
         gjahr = p_gjahr.

  CHECK NOT lt_inx_bkpf[] IS INITIAL.

* 4. Get more archived data looping structure table
  LOOP AT lt_inx_bkpf INTO ls_inx_bkpf.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bkpf-archivekey
        offset                    = ls_inx_bkpf-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bkpf, lt_bkpf[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BKPF'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bkpf
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bkpf[] IS INITIAL.

* 5. Append archived data table to finally interal table
    READ TABLE lt_bkpf INDEX 1.
    IF sy-subrc = 0.
      CLEAR p_stgrd.
      p_stgrd = lt_bkpf-stgrd.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_BKPF_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_RBKP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_rbkp USING p_stblg  p_stjah.

  TYPES: BEGIN OF ty_rbkp,
           stblg TYPE re_stblg,
           stjah TYPE re_stjah,
         archivekey TYPE arkey,
         archiveofs TYPE admi_offst.
  TYPES: END OF ty_rbkp.

  DATA: l_handle    TYPE sytabix,
        lt_rbkp     TYPE TABLE OF rbkp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_rbkp TYPE TABLE OF ty_rbkp,
        ls_inx_rbkp TYPE ty_rbkp.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZRBKP_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_rbkp[].
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_inx_rbkp
    FROM (l_gentab)
   WHERE stblg = p_stblg
     AND stjah = p_stjah.

  CHECK sy-subrc = 0.

*  CHECK NOT lt_inx_rbkp[] IS INITIAL.
*
** 4. Get more archived data looping structure table
*  LOOP AT lt_inx_rbkp INTO ls_inx_rbkp.
*    CLEAR: lt_rbkp, lt_rbkp[].
*    CALL FUNCTION 'ASH_MM_REBEL_READ'
*      EXPORTING
*        i_archivekey           = ls_inx_rbkp-archivekey
*        i_offset               = ls_inx_rbkp-archiveofs
*      TABLES
*        et_rbkp                = lt_rbkp
*      EXCEPTIONS
*        not_in_infostructure   = 1
*        not_in_archive         = 2
*        no_instructure_defined = 3
*        OTHERS                 = 4.
*
*    IF sy-subrc <> 0 AND lt_rbkp[] IS NOT INITIAL.
*      CLEAR p_subrc.
*      p_subrc = 'X'.
*      EXIT.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_RBKP
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAX  text
*      -->P_IT_BSEG_MWSKZ  text
*----------------------------------------------------------------------*
FORM archive_read_konp  USING  p_mwskz.

  TYPES: BEGIN OF ty_konp,
         knumh TYPE knumh,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_konp.

  DATA: l_handle    TYPE sytabix,
        lt_konp     TYPE TABLE OF konp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_a003     TYPE TABLE OF a003 WITH HEADER LINE.

  DATA: lt_inx_konp TYPE TABLE OF ty_konp,
        ls_inx_konp TYPE ty_konp.

  CLEAR: gt_tax_a, gt_tax_a[].

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZKONP_002'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

  CLEAR: lt_a003[], lt_a003[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_a003
    FROM a003
   WHERE kappl = 'TX'
     AND aland = 'US'
     AND mwskz = p_mwskz.

  CHECK NOT lt_a003[] IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_konp[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_konp
    FROM (l_gentab)
   FOR ALL ENTRIES IN lt_a003
   WHERE knumh = lt_a003-knumh.

  CHECK NOT lt_inx_konp[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_konp_a, gt_konp_a[].
  LOOP AT lt_inx_konp INTO ls_inx_konp.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'MM_EKKO'
        archivkey                 = ls_inx_konp-archivekey
        offset                    = ls_inx_konp-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_konp, lt_konp[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'KONP'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_konp
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_konp[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_konp INTO TABLE gt_konp_a.
  ENDLOOP.

  SORT gt_konp_a.
  DELETE ADJACENT DUPLICATES FROM gt_konp_a COMPARING ALL FIELDS.

*  CLEAR: gt_tax_a, gt_tax_a[].
  LOOP AT gt_konp_a.
    MOVE-CORRESPONDING gt_konp_a TO gt_tax_a.
    APPEND gt_tax_a.  CLEAR gt_tax_a.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bseg .

  TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,
         gsber TYPE gsber,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_bseg.

  DATA: l_handle    TYPE sytabix,
        lt_bseg     TYPE TABLE OF bseg WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bseg TYPE TABLE OF ty_bseg,
        ls_inx_bseg TYPE ty_bseg.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBSEG_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bseg[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bseg
    FROM (l_gentab)
   WHERE bukrs = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.

  CHECK NOT lt_inx_bseg[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bseg_a, gt_bseg_a[].
  LOOP AT lt_inx_bseg INTO ls_inx_bseg.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bseg-archivekey
        offset                    = ls_inx_bseg-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bseg, lt_bseg[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BSEG'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bseg
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bseg[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bseg INTO TABLE gt_bseg_a.
  ENDLOOP.

  SORT gt_bseg_a.
  DELETE ADJACENT DUPLICATES FROM gt_bseg_a COMPARING ALL FIELDS.

ENDFORM.                    " ARCHIVE_READ_BSEG
