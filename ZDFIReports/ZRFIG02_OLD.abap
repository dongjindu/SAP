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
*&    Date      Developer      Request ID      Description
*&#1 02/26/2005 WSKIM          UD1K914557      Field missing
*&--------------------------------------------------------------------
INCLUDE: <symbol>.
************************************************************************
************************* Global data **********************************
************************************************************************
TABLES: t001,    "Company code
        t005,    "Countris
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
        rbkp.    "Invoice document
TABLES: ekko, ekkn,t008.
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
      END OF it_bkpf.

* BSEG : G/L Data segment
DATA: BEGIN OF it_bseg OCCURS 0,
      bukrs    LIKE  bkpf-bukrs,
      gjahr    LIKE  bkpf-gjahr,
      belnr    LIKE  bkpf-belnr,
      lifnr    LIKE  bseg-lifnr,   "Vendor
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
    END OF it_bseg.
*
*----- Appended by BSBAE. 2004.06.08
DATA: it_bseg_sum LIKE it_bseg OCCURS 0 WITH HEADER LINE.

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

DATA: BEGIN OF it_l,
        txt20(20),      " account short text
        acct_type(48),
        taxcd(7),
        rate(4),
        ordno(30),
        assgn1(17),
        assgn2(12),
        text(48),
        date(12),
        dr  LIKE bseg-dmbtr,
        cr  LIKE bseg-dmbtr,
        drt  LIKE bseg-dmbtr, "tax
        crt  LIKE bseg-dmbtr, "tax
        name1 LIKE lfa1-name1,
        maktx LIKE makt-maktx,
        code(20),
    END OF it_l.


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
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
  s_blart  FOR   bkpf-blart MEMORY ID bar,
  s_budat  FOR   bkpf-budat,
  s_cpudt  FOR   bkpf-cpudt.

SELECTION-SCREEN END OF BLOCK b0.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-020.
PARAMETER   p_own    TYPE c AS CHECKBOX.
PARAMETER   p_detail TYPE c AS CHECKBOX.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-060 FOR FIELD r_ven.
PARAMETER r_nor RADIOBUTTON GROUP r_gr DEFAULT 'X'.
SELECTION-SCREEN COMMENT 30(10) text-040 FOR FIELD r_ven.
PARAMETER r_ven RADIOBUTTON GROUP r_gr.
SELECTION-SCREEN COMMENT 50(10) text-050 FOR FIELD r_cus.
PARAMETER r_cus RADIOBUTTON GROUP r_gr.
SELECTION-SCREEN END OF LINE.
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
PARAMETER : p_parked AS CHECKBOX DEFAULT ' '.
*            p_posted AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK b2.
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

AT SELECTION-SCREEN OUTPUT.
** Authority check
  DATA: wa_fi(2) VALUE 'FI'.

  AUTHORITY-CHECK OBJECT 'Z_BKPF_BES'
               ID 'BRGRU'   FIELD wa_fi.
  IF sy-subrc NE 0.
    p_own = 'X'.
    LOOP AT SCREEN.
      IF screen-name = 'P_OWN'.
        screen-input =  0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

TOP-OF-PAGE.
  PERFORM heading.
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
  PERFORM select_data.

*---2003/09/02 jhs modify
  IF it_bkpf[] IS INITIAL.
    MESSAGE s000(zmfi) WITH 'No data found'.
    EXIT.
  ENDIF.

*---start#1 wskim 03/02/2005
  PERFORM print_option.
*---end
END-OF-SELECTION.
*Write
  PERFORM writing_data.

END-OF-PAGE.
  PERFORM end_of_page.

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
  IF r_nor EQ 'X'.
    IF p_detail EQ 'X'.
      PERFORM body USING 'R_NOR'.
    ELSE.
      PERFORM body_summery USING 'R_NOR'.
    ENDIF.
*  perform sum.
*  PERFORM finish(ergphelp) USING wa_width.
    PERFORM draw_sign_box USING 'R_NOR'.
    SKIP.
    NEW-LINE NO-SCROLLING.
  ELSEIF r_ven EQ 'X'.
    IF p_detail EQ 'X'.
      PERFORM body USING 'R_VEN'.
**---start#1 wskim 02/26/2005
*      PERFORM draw_sign_box USING 'R_VEN'.
**---end
    ELSE.
      PERFORM body_summery USING 'R_VEN'.
    ENDIF.
*  perform sum.
*  PERFORM finish(ergphelp) USING wa_width.
    PERFORM draw_sign_box USING 'R_VEN'.
    SKIP.
    NEW-LINE NO-SCROLLING.

  ELSEIF r_cus EQ 'X'.
    IF p_detail EQ 'X'.
      PERFORM body USING 'R_CUS'.
*---start#1 wskim 02/26/2005
*      PERFORM draw_sign_box USING 'R_CUS'.
*---end
    ELSE.
      PERFORM body_summery USING 'R_CUS'.
    ENDIF.
    PERFORM draw_sign_box USING 'R_CUS'.
    SKIP.
    NEW-LINE NO-SCROLLING.

  ENDIF.

ENDFORM.
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
           (11) 'Doc. No.'     NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
           (04) 'Itm'          NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
           (12) 'Account'      NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
           (03) 'PK'           NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
           (51) 'Text'         NO-GAP CENTERED
                                      COLOR COL_HEADING INTENSIFIED OFF,
           (22) 'Debit'        NO-GAP RIGHT-JUSTIFIED
                                      COLOR COL_HEADING INTENSIFIED OFF,
           (23) 'Credit'       NO-GAP RIGHT-JUSTIFIED
                                      COLOR COL_HEADING INTENSIFIED OFF,
           (2) ' '             NO-GAP RIGHT-JUSTIFIED
                                      COLOR COL_HEADING INTENSIFIED OFF,
           sy-vline .
  ULINE AT (wa_width).                    " Line below titles


ENDFORM.

*---------------------------------------------------------------------*
*       FORM BODY                                                     *
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
FORM body USING radio.
  DATA: count   TYPE i VALUE 0,          " Loop counter for list body
        count2  TYPE i VALUE 0,          " Counter for stripes
        even    TYPE i VALUE 0,          " Checker even/odd
        help    TYPE i,
        ind TYPE i,
        wa_save_belnr LIKE bkpf-belnr,
        wa_blart      LIKE bkpf-blart,
        wa_sgtxt      LIKE bseg-sgtxt,
        wa_sdmbth     LIKE bseg-dmbtr,
        wa_sdmbts     LIKE bseg-dmbtr,
        wa_scnt       TYPE i,
        wa_hcnt       TYPE i.
  DATA: BEGIN OF l,
          txt20(20),      " account short text
          acct_type(48),
          taxcd(7),
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
*Start
  CASE radio.
    WHEN 'R_VEN' OR 'R_CUS'.
      LOOP AT it_bseg.
        IF it_bseg-lifnr EQ space.
          ind = sy-tabix - 1.
          READ TABLE it_bseg INDEX ind.
*     IND = SY-TABIX.
          MOVE   it_bseg-lifnr TO  it_bseg_sum-lifnr.
          MODIFY it_bseg TRANSPORTING lifnr
                 WHERE belnr = it_bseg-belnr.
*                   AND bschl = '40'.
        ENDIF.
      ENDLOOP.
      SORT it_bseg BY lifnr belnr  bschl .
      DELETE it_bseg WHERE lifnr = space.

      IF r_ven EQ 'X'.
        PERFORM filtering_bseg USING 'K'.
      ELSEIF r_cus EQ 'X'.
        PERFORM filtering_bseg USING 'D'.
      ENDIF.
  ENDCASE.
*end

*  stripes = 'X'.
  LOOP AT it_bseg.
    CLEAR it_l.
*---start wskim 03/11/2005
    CASE radio.
      WHEN 'R_VEN' OR 'R_CUS'.
        AT NEW lifnr.
          wa_first_page  = 1.
          wa_total_page1 = 1.
          f_num = f_num + 1.
          IF f_num >= 85.
            wa_total_page1 = wa_total_page1 + 1.
          ENDIF.
          NEW-PAGE.
        ENDAT.
    ENDCASE.
*---end
*    AT NEW lifnr.
*      NEW-PAGE.
*    ENDAT.
    AT NEW belnr.
      READ TABLE it_bkpf WITH KEY belnr = it_bseg-belnr
                                  gjahr = it_bseg-gjahr.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.

      wa_revtx = it_bkpf-revtx.
    ENDAT.

**-> Account type
    CASE it_bseg-koart.
      WHEN 'A'. "asset
        SELECT SINGLE txt50
          FROM anla       "Asset Master Record Segment
          INTO it_l-name1
         WHERE bukrs = it_bkpf-bukrs
           AND anln1 = it_bseg-anln1  "Asset
           AND anln2 = it_bseg-anln2. "Sub-Number

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg-anln1
             IMPORTING
                  output = it_l-code.

        CONCATENATE it_l-code     '-'
                    it_bseg-anln2 '('
                    it_bseg-anbwa ')'
               INTO it_l-code.
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
                  output = it_l-assgn2.

*      MOVE it_bseg-aufnr  TO  it_l-assgn2.
        DATA: wa_l_temp(20).
        PERFORM get_position_id CHANGING it_l-assgn1 wa_l_temp.

      WHEN 'D'. "customers
        SELECT SINGLE name1
          FROM kna1       "General Data in Customer Master
          INTO it_l-name1
         WHERE kunnr = it_bseg-kunnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg-kunnr
             IMPORTING
                  output = it_l-code.
*---jhs modify 2003/09/02
        IF it_bseg-umskz <> ' '.
          CONCATENATE it_l-code '/'
                      '('  it_bseg-umskz  ')'
               INTO it_l-code.
        ENDIF.
*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)
        CONCATENATE it_bseg-zterm '/'
                    it_bseg-zlsch '/'
                    it_bseg-zlspr
               INTO it_l-assgn1.

      WHEN 'K'. "vendors
*   --> account type (name + vendor + space.gl)
        SELECT SINGLE name1 INTO it_l-name1 FROM lfa1
          WHERE lifnr EQ it_bseg-lifnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg-lifnr
             IMPORTING
                  output = it_l-code.
*--jhs modify 2003/09/02
        IF it_bseg-umskz <> ' '.
          CONCATENATE it_l-code '/'
                      '('  it_bseg-umskz  ')'
               INTO it_l-code.
        ENDIF.
*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)
        CONCATENATE it_bseg-zterm '/'
                    it_bseg-zlsch '/'
                    it_bseg-zlspr
               INTO it_l-assgn1.

      WHEN 'M'. "material
        PERFORM fill_grir USING it_bseg.

      WHEN 'S'. "G/L accounts
******>>> Investment
        IF it_bseg-hkont+4(2) = '16'    OR
           it_bseg-hkont+4(3) = '901'.
*    move  it_bseg-aufnr to it_l-assgn2.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
               EXPORTING
                    input  = it_bseg-aufnr
               IMPORTING
                    output = it_l-assgn1.
          PERFORM get_position_id CHANGING it_l-code
                                           it_l-name1.


        ENDIF.
******>>> Expense
        IF it_bseg-hkont+4(1) = '6'.
          DATA: it_list TYPE TABLE OF ifmkacoo WITH HEADER LINE.
          RANGES: r_kostl FOR bseg-kostl.
          IF ( it_bseg-fistl IS INITIAL ) AND
             ( NOT it_bseg-kostl IS INITIAL ).
            r_kostl-sign = 'I'.
            r_kostl-option = 'EQ'.
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

          SELECT SINGLE bezeich
            FROM fmfctrt                 "Funds Center Text
            INTO it_l-name1
           WHERE fictr = it_bseg-fistl.  "Funds Center
          MOVE it_bseg-fistl TO it_l-code.

        ENDIF.
*****>>> Cash :
        DATA: wa_l_fdlev LIKE skb1-fdlev.
        SELECT SINGLE fdlev INTO wa_l_fdlev FROM skb1
          WHERE bukrs EQ it_bkpf-bukrs AND
                saknr EQ it_bseg-hkont.
        IF wa_l_fdlev(1) = 'C' OR wa_l_fdlev(1) = 'B'.
          CONCATENATE   it_bseg-sgtxt(37) '('
                        it_bseg-fdtag+4(2) '/'
                        it_bseg-fdtag+6(2) '/'
                        it_bseg-fdtag+0(4) ')'
                  INTO it_l-acct_type.
*
*           concatenate it_bseg-sgtxt(37) ':' it_bseg-fdtag
        ENDIF.

*****>>>> Cost
        IF it_bseg-bschl >= '80'.
          PERFORM fill_material USING it_bseg.
        ENDIF.
*****>>> GRIR
        IF it_bseg-bschl = '96' OR it_bseg-bschl = '86'.
          PERFORM fill_grir USING it_bseg.
        ENDIF.
        IF it_bseg-buzid = 'F'.   " PO freight, duty, ...
          PERFORM fill_import USING it_bseg.
        ENDIF.
*****>>> GRIR - Misc Import Expense
        IF it_bkpf-blart = 'KI'.
          it_l-acct_type = it_bseg-sgtxt.
        ENDIF.
    ENDCASE.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
         EXPORTING
              input  = it_l-code
         IMPORTING
              output = it_l-code.
    IF it_l-acct_type IS INITIAL.
      IF ( it_l-name1 IS INITIAL AND it_l-code IS INITIAL ) .
        CLEAR : it_l-acct_type.
      ELSE.
        CONCATENATE it_l-name1 '/' it_l-code INTO it_l-acct_type.
      ENDIF.
    ENDIF.



**--> Tax rate
    DATA: l_rate TYPE p DECIMALS 1.
    DATA: ls_rate(4).
    MOVE it_bseg-mwskz TO it_l-taxcd.
    IF it_bseg-fwbas NE 0.
      l_rate = it_bseg-dmbtr / it_bseg-fwbas * 100.
      it_l-rate = l_rate.
      SELECT SINGLE text1 INTO it_l-acct_type FROM t007s
        WHERE spras = sy-langu AND
              kalsm = 'TAXUS' AND
              mwskz = it_bseg-mwskz.
      WRITE l_rate TO ls_rate.
      CONCATENATE it_l-acct_type '/' it_bseg-mwskz '/' ls_rate
        INTO it_l-acct_type.

    ENDIF.

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


      LOOP AT it_tax WHERE kschl+3(1) EQ 'I'.    "case A/P
        wa_l_tax = it_bseg-dmbtr * it_tax-kbetr / 1000.
        ADD wa_l_tax TO it_l-drt.
      ENDLOOP.
    ENDIF.

* Stripe handling (list body)
    IF stripes EQ ' '.                 " No Stripes
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ELSE.                              " Stripes
      even = count2 MOD 2.              " Check for even/odd
      IF even = '0'.
        FORMAT COLOR COL_NORMAL INTENSIFIED.
      ELSE.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      ENDIF.
    ENDIF.
*    IF it_l-acct_type = ' '.
*      READ TABLE it_bkpf WITH KEY belnr = it_bseg-belnr.
*      if sy-subrc = 0.
*        clear wa_ttext.
*        select single ttext into wa_ttext
*        from TSTCT
*        where SPRSL = SY-LANGU
*        AND   Tcode = it_bkpf-tcode.
*
*        it_l-acct_type = it_bkpf-tcode.
*        concatenate
*          it_bkpf-tcode ' ' wa_ttext+0(20) into it_l-acct_type.
*      endif.
*    ENDIF.


    CONCATENATE it_l-acct_type '/' it_l-assgn1
           INTO it_l-acct_type.


    IF wa_save_belnr NE it_bseg-belnr.
      wa_save_belnr = it_bseg-belnr.
      READ TABLE it_bkpf WITH KEY belnr = it_bseg-belnr.
      IF sy-subrc = 0.
        wa_blart = it_bkpf-blart.
      ENDIF.
      WRITE : / sy-vline,
               it_bseg-belnr,
               it_bseg-buzei,
               it_bseg-hkont,
               it_bseg-bschl,
*              it_bseg-sgtxt,
             (51) it_l-acct_type,
             (22) it_bseg-dmbts NO-ZERO,
             (22)  it_bseg-dmbth NO-ZERO,
               sy-vline.
    ELSE.
      WRITE : / sy-vline,
              wa_blart,
              wa_revtx,
              it_bseg-buzei,
              it_bseg-hkont,
              it_bseg-bschl,
*              it_bseg-sgtxt,
            (51) it_l-acct_type,
            (22) it_bseg-dmbts NO-ZERO,
            (22) it_bseg-dmbth NO-ZERO,
              sy-vline.
      wa_blart = ' '.
      wa_revtx = ' '.
    ENDIF.

    ADD it_bseg-dmbth TO wa_sdmbth.
    ADD it_bseg-dmbts TO wa_sdmbts.

    IF it_bkpf-revtx IS INITIAL.
      ADD it_bseg-dmbth TO wa_sdmbth_g.
      ADD it_bseg-dmbts TO wa_sdmbts_g.
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

    AT END OF belnr.
*---START#1 WSKIM 03/02/2005
      DATA: wa_l_reverse(30).
      CLEAR wa_l_reverse.
*---END
* check reverse doc
      IF NOT it_bkpf-stblg IS INITIAL.
        IF it_bkpf-stgrd IS INITIAL.
          SELECT SINGLE stgrd INTO it_bkpf-stgrd FROM bkpf
            WHERE bukrs = it_bkpf-bukrs AND
                  belnr = it_bkpf-stblg AND
                  gjahr = it_bkpf-stjah.
        ENDIF.

        CONCATENATE 'REV->'
                    it_bkpf-stblg '/'
                    it_bkpf-stgrd
              INTO  wa_l_reverse.
      ENDIF.

      FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
      WRITE : / sy-vline,
              (10) it_bkpf-budat,
              (18) wa_l_reverse NO-GAP, "29
              (22) it_bkpf-xblnr,
              '( Dr:',
              (4) wa_scnt NO-GAP,
              '/ CR:',
              (4) wa_hcnt NO-GAP,
              ')',
              (6) ' ',
              (22) wa_sdmbts NO-ZERO,
              (22) wa_sdmbth NO-ZERO,
              sy-vline.
      ULINE AT (wa_width).
      CLEAR : wa_sdmbth, wa_sdmbts.
      CLEAR : wa_hcnt, wa_scnt.

    ENDAT.

    ADD 1 TO count.
    ADD 1 TO count2.
*-------Start
    CASE radio.
      WHEN 'R_VEN' OR 'R_CUS'.
        AT END OF lifnr.
          DO.
            IF sy-linno > 54. "26
              EXIT.
            ENDIF.
*    WRITE :/.
            PERFORM write_empty_line.
          ENDDO.
          PERFORM sum_v1.
          PERFORM draw_sign_box USING '  '.
*          NEW-PAGE.
        ENDAT.
    ENDCASE.
*--------END

  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SUM                                                      *
*---------------------------------------------------------------------*
*       Writes row with final totals                                  *
*---------------------------------------------------------------------*
FORM sum.
  ULINE AT (wa_width).
  FORMAT COLOR COL_TOTAL INTENSIFIED.  " Color of final totals
  WRITE : / sy-vline,
          (51) ' ',
          '( Dr:',
          (6) wa_scnt_g NO-GAP,
          '/ CR:',
          (6) wa_hcnt_g NO-GAP,
          ')',
          (2) ' ',
          (22) wa_sdmbts_g NO-ZERO,
          (22) wa_sdmbth_g NO-ZERO,
          sy-vline.
  ULINE AT (wa_width).
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.  " Color of final totals

ENDFORM.

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
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_data.

  PERFORM get_company_info.

  PERFORM get_bkpf_data.

  PERFORM get_bseg_details.

  PERFORM process_ap_filter.

  PERFORM set_debit_credit.

*Request by ghlee 2004.07.15: (issue  number FI-20040705-004)
*Start : delete
** calculate for total page number
*  DATA: wa_l_bkpf TYPE i,
*        wa_l_bseg TYPE i,
*        wa_l_mod  TYPE i.
*
*  LOOP AT it_bkpf.
*    ADD 1 TO wa_l_bkpf.
*  ENDLOOP.
*  LOOP AT it_bseg.
*    ADD 1 TO wa_l_bseg.
*  ENDLOOP.
*  wa_total_page = ( wa_l_bkpf * 2 + wa_l_bseg + 3 ) / 84.
*  wa_l_mod      = ( wa_l_bkpf * 2 + wa_l_bseg + 3 ) MOD 84.
*  IF wa_l_mod < 75.
*    wa_total_page = wa_total_page + 1.
*  ELSE.
*    wa_total_page = wa_total_page + 2.
*  ENDIF.
*end



*----- Appended by BSBAE. 2004.06.08
**-> Collect IT_BSEG_SUM
  LOOP AT it_bseg.
    IF ( it_bseg-bschl >= '80' AND
       it_bseg-bschl <= '99'     ) OR
       it_bseg-bschl = '40'        OR
       it_bseg-bschl = '50'.
      READ TABLE it_bseg_sum WITH KEY belnr = it_bseg-belnr
                                      gjahr = it_bseg-gjahr
                                      bschl = it_bseg-bschl
                                      hkont = it_bseg-hkont.
      IF sy-subrc EQ 0.
        it_bseg_sum-dmbth = it_bseg_sum-dmbth + it_bseg-dmbth.
        it_bseg_sum-dmbts = it_bseg_sum-dmbts + it_bseg-dmbts.
        CLEAR: it_bseg_sum-buzei.
        MODIFY it_bseg_sum INDEX sy-tabix.
      ELSE.
        MOVE it_bseg TO it_bseg_sum.
        APPEND it_bseg_sum.
      ENDIF.
    ELSE.
      MOVE it_bseg TO it_bseg_sum.
      APPEND it_bseg_sum.
    ENDIF.
  ENDLOOP.
*----- Appended by BSBAE. 2004.06.08
*Request by ghlee 2004.07.15: (issue  number FI-20040705-004)
*Start
* calculate for total page number
  DATA: wa_l_bkpf TYPE i,
    wa_l_bseg TYPE i,
    wa_l_mod  TYPE i,
    wa_total_temp(5) TYPE p DECIMALS 2.
  IF p_detail <> 'X'.
    LOOP AT it_bkpf.
      ADD 1 TO wa_l_bkpf.
    ENDLOOP.
    LOOP AT it_bseg_sum.
      ADD 1 TO wa_l_bseg.
    ENDLOOP.
    wa_total_temp = ( wa_l_bkpf * 2 + wa_l_bseg + 3 ) / 84.    "84.
*    wa_total_page = ( wa_l_bkpf * 2 + wa_l_bseg ) / 81.     "84.+3
    wa_l_mod = ( wa_l_bkpf * 2 + wa_l_bseg + 3 ) MOD 84. "84
    wa_total_page = trunc( wa_total_temp ).
    IF wa_total_page >= 1.
      IF wa_l_mod < 56.
        wa_total_page = wa_total_page + 1.
      ELSEIF wa_l_mod >= 56.
        wa_total_page = wa_total_page + 2.
      ENDIF.
    ELSEIF wa_total_temp < 1.
      wa_total_page = 1.
    ENDIF.

  ELSE.
    LOOP AT it_bkpf.
      ADD 1 TO wa_l_bkpf.
    ENDLOOP.
    LOOP AT it_bseg.
      ADD 1 TO wa_l_bseg.
    ENDLOOP.
    wa_total_temp = ( wa_l_bkpf * 2 + wa_l_bseg + 3 ) / 84. "84.+
*    wa_total_page = ( wa_l_bkpf * 2 + wa_l_bseg ) / 81.     "84.+3
    wa_l_mod = ( wa_l_bkpf * 2 + wa_l_bseg + 3 ) MOD 84. "84
    wa_total_page = trunc( wa_total_temp ).
    IF wa_total_page >= 1.
      IF wa_l_mod =< 56.
        wa_total_page = wa_total_page + 1.
      ELSEIF wa_l_mod > 56.
        wa_total_page = wa_total_page + 2.
      ENDIF.
    ELSEIF wa_total_temp < 1.
      wa_total_page = 1.
    ENDIF.
  ENDIF.
*end
ENDFORM.                    " select_data
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
    APPEND it_bseg.
  ENDSELECT.
ENDFORM.

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
    APPEND it_bseg.

  ENDSELECT.
ENDFORM.

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
    APPEND it_bseg.
  ENDSELECT.

ENDFORM.

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
    APPEND it_bseg.
  ENDSELECT.

ENDFORM.

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
    APPEND it_bseg.
  ENDSELECT.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  draw_sign_box
*&---------------------------------------------------------------------*
FORM draw_sign_box USING radio.
  CASE radio.
*---Start#1 wskim 02/26/2005
*    WHEN 'R_NOR'.
    WHEN 'R_NOR' OR 'R_VEN' OR 'R_CUS'.
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
      PERFORM sum.
      FORMAT   INTENSIFIED OFF.
      NEW-LINE. ULINE AT (wa_width).

    WHEN OTHERS.

  ENDCASE.

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
  WRITE:/  wa_vl NO-GAP, (11)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (40)  'Name'   NO-GAP CENTERED,
           wa_vl NO-GAP, (40)  'Signature'   NO-GAP CENTERED,
           wa_vl NO-GAP, (34)  'Date'   NO-GAP CENTERED,
           wa_vl NO-GAP.
  NEW-LINE. ULINE AT (wa_width).

  WRITE:/  wa_vl NO-GAP, (11) 'Requestor'    NO-GAP CENTERED,
           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
           wa_vl NO-GAP, (34) space NO-GAP CENTERED,
           wa_vl NO-GAP.

  WRITE:/  wa_vl NO-GAP, (11) space NO-GAP CENTERED,
           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
           wa_vl NO-GAP, (34) space NO-GAP CENTERED,
           wa_vl NO-GAP.

  WRITE:/  wa_vl NO-GAP, (11) space NO-GAP CENTERED,
           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
           wa_vl NO-GAP, (34) space NO-GAP CENTERED,
           wa_vl NO-GAP.
  NEW-LINE. ULINE AT (wa_width).

  NEW-LINE. ULINE AT (wa_width).

*--------Department approval  date finance approval date
  WRITE:/  wa_vl NO-GAP, (22)  'Department'   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  'Approval'     NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  'Date'         NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  'Finance'      NO-GAP CENTERED,
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
  WRITE:/  wa_vl NO-GAP, (22)  'Head of Department'   NO-GAP CENTERED,
             wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
             wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  'Cost Accounting'      NO-GAP CENTERED,
             wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
             wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
             wa_vl NO-GAP.
*
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
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
  WRITE:/  wa_vl NO-GAP, (22)  'Head of Sub Division'   NO-GAP CENTERED,
               wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
               wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
              wa_vl NO-GAP, (20)  'General Accounting'  NO-GAP CENTERED,
               wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
               wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
               wa_vl NO-GAP.
*
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
           wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
*
  WRITE:/  wa_vl NO-GAP, (22)  'Head of Division'   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  'Treasurer'  NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
           wa_vl NO-GAP.
*
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
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
  WRITE:/  wa_vl NO-GAP, (22)  'President / CEO'   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  'CFO'  NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
           wa_vl NO-GAP.
*
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
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
*&      Form  fill_grir
*&---------------------------------------------------------------------*
FORM fill_grir USING pt_bseg STRUCTURE it_bseg.
  DATA: l_term(2) TYPE c.
  WRITE: pt_bseg-menge TO wa_txt10 RIGHT-JUSTIFIED."left

  IF pt_bseg-ebeln = space.
    CONCATENATE pt_bseg-matnr
            l_s wa_txt10
       INTO it_l-acct_type.
  ELSE.
* select PO - payment term
    SELECT SINGLE zterm INTO l_term FROM ekko
      WHERE ebeln = pt_bseg-ebeln.
* import PO
    IF l_term = 'LC' OR l_term = 'DA'
    OR l_term = 'DA' OR l_term = 'DP'.

    ELSE.

    ENDIF.
*----start#1 wskim 02/26/2005 add sgtxt
*    CONCATENATE pt_bseg-matnr
*            l_s wa_txt10
*            l_s pt_bseg-ebeln
*       INTO it_l-acct_type.
    CONCATENATE pt_bseg-matnr
            l_s wa_txt10
            l_s pt_bseg-ebeln l_s pt_bseg-sgtxt
       INTO it_l-acct_type.
*---end
  ENDIF.
  EXIT.

*eject
  SELECT SINGLE maktx INTO it_l-maktx FROM makt
     WHERE matnr = pt_bseg-matnr.

  WRITE: pt_bseg-menge TO wa_txt10 LEFT-JUSTIFIED.
  CONCATENATE it_bseg-matnr l_s it_l-maktx l_s wa_txt10
     INTO it_l-acct_type.

ENDFORM.                    " fill_grir
*&---------------------------------------------------------------------*
*&      Form  fill_material
*&---------------------------------------------------------------------*
FORM fill_material USING pt_bseg STRUCTURE it_bseg.
  CASE pt_bseg-bschl.
    WHEN '81' OR '91'.  "Cost
      it_l-acct_type = pt_bseg-aufnr.
    WHEN '83' OR '93'.
      it_l-acct_type = 'Price Difference'.
    WHEN OTHERS.

  ENDCASE.
ENDFORM.                    " fill_material
*&---------------------------------------------------------------------*
*&      Form  fill_import
*&---------------------------------------------------------------------*
FORM fill_import USING pt_bseg STRUCTURE it_bseg.
  CONCATENATE pt_bseg-ktosl l_s pt_bseg-zuonr
     INTO it_l-acct_type.


ENDFORM.                    " fill_import
*&---------------------------------------------------------------------*
*&      Form  BODY_SUMMERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM body_summery USING radio.
  DATA : ind TYPE i.
*Request by ghlee 2004.07.15: (issue  number FI-20040705-004)
*Start
  CASE radio.
    WHEN 'R_VEN' OR 'R_CUS'.
      LOOP AT it_bseg_sum.
        IF it_bseg_sum-lifnr EQ space.
          ind = sy-tabix - 1.
          READ TABLE it_bseg_sum INDEX ind.
*     IND = SY-TABIX.
          MOVE   it_bseg_sum-lifnr TO  it_bseg_sum-lifnr.
          MODIFY it_bseg_sum TRANSPORTING lifnr
                 WHERE belnr = it_bseg_sum-belnr.
*                   AND bschl = '40'.
        ENDIF.
      ENDLOOP.
      SORT it_bseg_sum BY lifnr belnr  bschl .
      DELETE it_bseg_sum WHERE lifnr = space.

      IF r_ven EQ 'X'.
        PERFORM filtering_bsegsum USING 'K'.
      ELSEIF r_cus EQ 'X'.
        PERFORM filtering_bsegsum USING 'D'.
      ENDIF.
  ENDCASE.
*end
*  stripes = 'X'.
*Request by ghlee 2004.07.15: (issue  number FI-20040705-004)
*Start
  CASE radio.
    WHEN 'R_VEN' OR 'R_CUS'.
      PERFORM body_summary_vencus USING radio.
    WHEN OTHERS.
      PERFORM body_summary_normal USING radio.
  ENDCASE.
*end
ENDFORM.                    " BODY_SUMMERY
*&---------------------------------------------------------------------*
*&      Form  sum_v1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sum_v1.
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
  WRITE : / sy-vline, 75 text-092,
           93(13) wa_sdmbts_g NO-ZERO,
           116(13) wa_sdmbth_g NO-ZERO,
        sy-vline.
*2nd line Difference
  WRITE : / sy-vline,
          (45) ' ',
          '( Dr:',
          (6) wa_scnt_g NO-GAP,
          '/ CR:',
          (6) wa_hcnt_g NO-GAP,
          ')',
          (2) ' ',
          75 text-093,
          93(13) wa_sdmbts_d NO-ZERO,
          116(13) wa_sdmbth_d NO-ZERO,
          sy-vline.
*3rd Total
  WRITE : / sy-vline, 75 text-094,
           93(13) wa_sdmbts_t NO-ZERO,
           116(13) wa_sdmbth_t NO-ZERO,
        sy-vline.

  ULINE AT (wa_width).
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.  " Color of final totals
  CLEAR : wa_scnt_g, wa_hcnt_g,wa_sdmbts_g,wa_sdmbth_g,wa_sdmbts_d,
          wa_sdmbth_d.
ENDFORM.                                                    " sum_v1
*&---------------------------------------------------------------------*
*&      Form  filtering_bsegsum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3878   text
*----------------------------------------------------------------------*
FORM filtering_bsegsum USING p_mark.
  DATA w_int TYPE i.
  CLEAR w_int.
  LOOP AT it_bseg_sum.
    IF it_bseg_sum-koart NE p_mark.
      READ TABLE it_bseg_sum WITH KEY  belnr = it_bseg_sum-belnr
                                       koart = p_mark.
      IF sy-subrc <> 0.
        DELETE TABLE it_bseg_sum FROM it_bseg_sum.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE it_bseg_sum LINES w_int.
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
ENDFORM.                    " filtering_bsegsum
*&---------------------------------------------------------------------*
*&      Form  body_summary_vencus
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM body_summary_vencus USING radio.
  DATA: count   TYPE i VALUE 0,          " Loop counter for list body
          count2  TYPE i VALUE 0,          " Counter for stripes
          even    TYPE i VALUE 0,          " Checker even/odd
          help    TYPE i,
          wa_save_belnr LIKE bkpf-belnr,
          wa_blart      LIKE bkpf-blart,
          wa_sgtxt      LIKE bseg-sgtxt,
          wa_sdmbth     LIKE bseg-dmbtr,
          wa_sdmbts     LIKE bseg-dmbtr,
          wa_scnt       TYPE i,
          wa_hcnt       TYPE i,
          w_int         TYPE i,
          f_num TYPE i,
          ind TYPE i.
  DATA: BEGIN OF l,
          txt20(20),      " account short text
          acct_type(48),
          taxcd(7),
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
  CLEAR : wa_hcnt_g,   wa_scnt_g,ind, it_bseg_sum_v[].
*request by ghlee 2004.07.15: (issue  number fi-20040705-004)
*Start
  LOOP AT  it_bseg_sum.
    MOVE-CORRESPONDING  it_bseg_sum TO  it_bseg_sum_v.
    APPEND  it_bseg_sum_v.
  ENDLOOP.
  LOOP AT it_bseg_sum_v.
    CASE radio.
      WHEN 'R_VEN' OR 'R_CUS'.
        AT NEW lifnr.
          wa_first_page  = 1.
          wa_total_page1 = 1.
          f_num = f_num + 1.
          IF f_num >= 85.
            wa_total_page1 = wa_total_page1 + 1.
          ENDIF.
          NEW-PAGE.
        ENDAT.
    ENDCASE.
*end
    CLEAR it_l.
    AT NEW belnr.
      READ TABLE it_bkpf WITH KEY belnr = it_bseg_sum_v-belnr
                                  gjahr = it_bseg_sum_v-gjahr.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.

      wa_revtx = it_bkpf-revtx.
    ENDAT.

**-> Account type
    CASE it_bseg_sum_v-koart.
      WHEN 'A'. "asset
        SELECT SINGLE txt50
          FROM anla       "Asset Master Record Segment
          INTO it_l-name1
         WHERE bukrs = it_bkpf-bukrs
           AND anln1 = it_bseg_sum_v-anln1  "Asset
           AND anln2 = it_bseg_sum_v-anln2. "Sub-Number

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg_sum_v-anln1
             IMPORTING
                  output = it_l-code.

        CONCATENATE it_l-code     '-'
                    it_bseg_sum_v-anln2 '('
                    it_bseg_sum_v-anbwa ')'
               INTO it_l-code.
        IF it_bseg_sum_v-aufnr IS INITIAL.
          SELECT SINGLE eaufn INTO it_bseg_sum_v-aufnr
            FROM anla
            WHERE bukrs EQ it_bseg_sum_v-bukrs AND
                  anln1 EQ it_bseg_sum_v-anln1 AND
                  anln2 EQ it_bseg_sum_v-anln2.
        ENDIF.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg_sum_v-aufnr
             IMPORTING
                  output = it_l-assgn2.

*      MOVE it_bseg_sum_v_v-aufnr  TO  it_l-assgn2.
        DATA: wa_l_temp(20).
        PERFORM get_position_id CHANGING it_l-assgn1 wa_l_temp.

      WHEN 'D'. "customers
        SELECT SINGLE name1
          FROM kna1       "General Data in Customer Master
          INTO it_l-name1
         WHERE kunnr = it_bseg_sum_v-kunnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg_sum_v-kunnr
             IMPORTING
                  output = it_l-code.
*---jhs modify 2003/09/02
        IF it_bseg_sum_v-umskz <> ' '.
          CONCATENATE it_l-code '/'
                      '('  it_bseg_sum_v-umskz  ')'
               INTO it_l-code.
        ENDIF.
*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)
        CONCATENATE it_bseg_sum_v-zterm '/'
                    it_bseg_sum_v-zlsch '/'
                    it_bseg_sum_v-zlspr
               INTO it_l-assgn1.

      WHEN 'K'. "vendors
*   --> account type (name + vendor + space.gl)
        SELECT SINGLE name1 INTO it_l-name1 FROM lfa1
          WHERE lifnr EQ it_bseg_sum_v-lifnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg_sum_v-lifnr
             IMPORTING
                  output = it_l-code.
*--jhs modify 2003/09/02
        IF it_bseg_sum_v-umskz <> ' '.
          CONCATENATE it_l-code '/'
                      '('  it_bseg_sum_v-umskz  ')'
               INTO it_l-code.
        ENDIF.
*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)
        CONCATENATE it_bseg_sum_v-zterm '/'
                    it_bseg_sum_v-zlsch '/'
                    it_bseg_sum_v-zlspr
               INTO it_l-assgn1.

      WHEN 'M'. "material
        PERFORM fill_grir USING it_bseg.

      WHEN 'S'. "G/L accounts
******>>> Investment
        IF it_bseg_sum_v-hkont+4(2) = '16'    OR
           it_bseg_sum_v-hkont+4(3) = '901'.
*    move  it_bseg_sum_v-aufnr to it_l-assgn2.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
               EXPORTING
                    input  = it_bseg_sum_v-aufnr
               IMPORTING
                    output = it_l-assgn1.
          PERFORM get_position_id CHANGING it_l-code
                                           it_l-name1.

        ENDIF.
******>>> Expense
        IF it_bseg_sum_v-hkont+4(1) = '6'.
          DATA: it_list TYPE TABLE OF ifmkacoo WITH HEADER LINE.
          RANGES: r_kostl FOR bseg-kostl.
          IF ( it_bseg_sum_v-fistl IS INITIAL ) AND
             ( NOT it_bseg_sum_v-kostl IS INITIAL ).
            r_kostl-sign = 'I'.
            r_kostl-option = 'EQ'.
            r_kostl-low  = it_bseg_sum_v-kostl.
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
              MOVE it_list-fistl TO it_bseg_sum_v-fistl.
            ENDLOOP.
          ENDIF.

          SELECT SINGLE bezeich
            FROM fmfctrt                 "Funds Center Text
            INTO it_l-name1
           WHERE fictr = it_bseg_sum_v-fistl.  "Funds Center
          MOVE it_bseg_sum_v-fistl TO it_l-code.

        ENDIF.
******>>> Cash :
*        DATA: wa_l_fdlev LIKE skb1-fdlev.
*        SELECT SINGLE fdlev INTO wa_l_fdlev FROM skb1
*          WHERE bukrs EQ it_bkpf-bukrs AND
*                saknr EQ it_bseg_sum_v-hkont.
*        IF wa_l_fdlev(1) = 'C' OR wa_l_fdlev(1) = 'B'.
*          CONCATENATE   it_bseg_sum_v-sgtxt(37) '('
*                        it_bseg_sum_v-fdtag+4(2) '/'
*                        it_bseg_sum_v-fdtag+6(2) '/'
*                        it_bseg_sum_v-fdtag+0(4) ')'
*                  INTO it_l-acct_type.
**
**           concatenate it_bseg_sum_v-sgtxt(37) ':'
**it_bseg_sum_v-fdtag
*        ENDIF.
*
******>>>> Cost
*        IF it_bseg_sum_v-bschl >= '80'.
*          PERFORM fill_material USING it_bseg_sum_v.
*        ENDIF.
******>>> GRIR
*        IF it_bseg_sum_v-bschl = '96' OR it_bseg_sum_v-bschl = '86'.
*          PERFORM fill_grir USING it_bseg_sum_v.
*        ENDIF.
*        IF it_bseg_sum_v-buzid = 'F'.   " PO freight, duty, ...
*          PERFORM fill_import USING it_bseg_sum_v.
*        ENDIF.
******>>> GRIR - Misc Import Expense
*        IF it_bkpf-blart = 'KI'.
*          it_l-acct_type = it_bseg_sum_v-sgtxt.
*        ENDIF.
    ENDCASE.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
         EXPORTING
              input  = it_l-code
         IMPORTING
              output = it_l-code.
    IF it_l-acct_type IS INITIAL.
      IF ( it_l-name1 IS INITIAL AND it_l-code IS INITIAL ) .
        CLEAR : it_l-acct_type.
      ELSE.
        CONCATENATE it_l-name1 '/' it_l-code INTO it_l-acct_type.
      ENDIF.
    ENDIF.

**--> Tax rate
    DATA: l_rate TYPE p DECIMALS 1.
    DATA: ls_rate(4).
    MOVE it_bseg_sum_v-mwskz TO it_l-taxcd.
    IF it_bseg_sum_v-fwbas NE 0.
      l_rate = it_bseg_sum_v-dmbtr / it_bseg_sum_v-fwbas * 100.
      it_l-rate = l_rate.
      SELECT SINGLE text1 INTO it_l-acct_type FROM t007s
        WHERE spras = sy-langu AND
              kalsm = 'TAXUS' AND
              mwskz = it_bseg_sum_v-mwskz.
      WRITE l_rate TO ls_rate.
      CONCATENATE it_l-acct_type '/' it_bseg_sum_v-mwskz '/' ls_rate
        INTO it_l-acct_type.

    ENDIF.

    DATA: BEGIN OF it_tax OCCURS 0,
             kschl LIKE konp-kschl,
             kbetr LIKE konp-kbetr,
          END OF it_tax.
    DATA: wa_l_tax LIKE bseg-dmbtr.

    IF it_bseg_sum_v-mwskz NE space AND  " exist tax code
       it_bseg_sum_v-shkzg EQ 'S'   AND  " only debit
       it_bkpf-bstat EQ 'V'.       " only parked doc.


      SELECT b~kschl b~kbetr INTO TABLE it_tax
        FROM a003 AS a INNER JOIN konp AS b ON b~knumh EQ a~knumh
        WHERE a~kappl = 'TX' AND
              a~aland = 'US' AND
              a~mwskz EQ it_bseg_sum_v-mwskz.


      LOOP AT it_tax WHERE kschl+3(1) EQ 'I'.    "case A/P
        wa_l_tax = it_bseg_sum_v-dmbtr * it_tax-kbetr / 1000.
        ADD wa_l_tax TO it_l-drt.
      ENDLOOP.
    ENDIF.

* Stripe handling (list body)
    IF stripes EQ ' '.                 " No Stripes
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ELSE.                              " Stripes
      even = count2 MOD 2.              " Check for even/odd
      IF even = '0'.
        FORMAT COLOR COL_NORMAL INTENSIFIED.
      ELSE.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      ENDIF.
    ENDIF.
*    IF it_l-acct_type = ' '.
*      READ TABLE it_bkpf WITH KEY belnr = it_bseg_sum_v-belnr.
*      if sy-subrc = 0.
*        clear wa_ttext.
*        select single ttext into wa_ttext
*        from TSTCT
*        where SPRSL = SY-LANGU
*        AND   Tcode = it_bkpf-tcode.
*
*        it_l-acct_type = it_bkpf-tcode.
*        concatenate
*          it_bkpf-tcode ' ' wa_ttext+0(20) into it_l-acct_type.
*      endif.
*    ENDIF.

*----- Appended by BSBAe. 2004.06.09
    IF it_bseg_sum_v-buzei IS INITIAL.
      CLEAR: it_l-acct_type.
      SELECT SINGLE * FROM t001 WHERE bukrs = it_bseg_sum_v-bukrs.

      SELECT SINGLE txt20 INTO it_l-acct_type(20)
        FROM skat
       WHERE spras = sy-langu
         AND ktopl = t001-ktopl
         AND saknr = it_bseg_sum_v-hkont.
    ENDIF.

    IF it_bseg_sum_v-buzei EQ '001'.
      CONCATENATE it_l-acct_type '/' it_bseg_sum_v-zlsch '/'
                  it_bseg-zterm  '/' it_bseg_sum_v-fdtag
             INTO it_l-acct_type.
    ENDIF.
*----- Appended by BSBAe. 2004.06.09

    IF it_bseg_sum_v-koart CA 'KD'.
      CONCATENATE it_l-acct_type  '/' it_bseg_sum_v-zlsch '/'
                    it_bseg-zterm '/' it_bseg_sum_v-fdtag
               INTO it_l-acct_type.
    ELSEIF it_bseg_sum_v-koart = 'S' OR
      it_bseg_sum_v-buzei IS INITIAL.
      CLEAR: it_l-acct_type.
      SELECT SINGLE * FROM t001 WHERE bukrs = it_bseg_sum_v-bukrs.

      SELECT SINGLE txt20 INTO it_l-acct_type(20)
        FROM skat
       WHERE spras = sy-langu
         AND ktopl = t001-ktopl
         AND saknr = it_bseg_sum_v-hkont.
    ENDIF.
*----------------------------------------------------------------


    IF wa_save_belnr NE it_bseg_sum_v-belnr.
      wa_save_belnr = it_bseg_sum_v-belnr.
      READ TABLE it_bkpf WITH KEY belnr = it_bseg_sum_v-belnr.
      IF sy-subrc = 0.
        wa_blart = it_bkpf-blart.
      ENDIF.
      WRITE : / sy-vline,
              it_bseg_sum_v-belnr,
              it_bseg_sum_v-buzei,
              it_bseg_sum_v-hkont,
              it_bseg_sum_v-bschl,
*              it_bseg_sum_v-sgtxt,
            (51) it_l-acct_type,
            (22) it_bseg_sum_v-dmbts NO-ZERO,
            (22)  it_bseg_sum_v-dmbth NO-ZERO,
              sy-vline.
    ELSE.
      WRITE : / sy-vline,
              wa_blart,
              wa_revtx,
              it_bseg_sum_v-buzei,
              it_bseg_sum_v-hkont,
              it_bseg_sum_v-bschl,
*              it_bseg_sum_v-sgtxt,
            (51) it_l-acct_type,
            (22) it_bseg_sum_v-dmbts NO-ZERO,
            (22) it_bseg_sum_v-dmbth NO-ZERO,
              sy-vline.
      wa_blart = ' '.
      wa_revtx = ' '.
    ENDIF.

    ADD it_bseg_sum_v-dmbth TO wa_sdmbth.
    ADD it_bseg_sum_v-dmbts TO wa_sdmbts.

    IF it_bkpf-revtx IS INITIAL.
      ADD it_bseg_sum_v-dmbth TO wa_sdmbth_g.
      ADD it_bseg_sum_v-dmbts TO wa_sdmbts_g.
    ENDIF.

    IF it_bseg_sum_v-dmbth NE 0.
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
*request by ghlee 2004.07.15: (issue  number fi-20040705-004)
*start
    IF it_bseg_sum_v-hkont+4(1) EQ '5'.
      CASE it_bseg_sum_v-shkzg.
        WHEN 'S'.
          ADD it_bseg_sum_v-dmbts TO wa_sdmbts_d.
        WHEN 'H'.
          ADD it_bseg_sum_v-dmbth TO wa_sdmbth_d.
      ENDCASE.
    ENDIF.
*end
    AT END OF belnr.
* check reverse doc
      IF NOT it_bkpf-stblg IS INITIAL.
        IF it_bkpf-stgrd IS INITIAL.
          SELECT SINGLE stgrd INTO it_bkpf-stgrd FROM bkpf
            WHERE bukrs = it_bkpf-bukrs AND
                  belnr = it_bkpf-stblg AND
                  gjahr = it_bkpf-stjah.
        ENDIF.
        DATA: wa_l_reverse(30).
        CONCATENATE 'REVERSED->'
                    it_bkpf-stblg '/'
                    it_bkpf-stgrd
              INTO  wa_l_reverse.
      ENDIF.

      FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
      WRITE : / sy-vline,
              (10) it_bkpf-bldat,
              (30) wa_l_reverse NO-GAP,
              (16) it_bkpf-xblnr,
              '( Dr:',
              (3) wa_scnt NO-GAP,
              '/ CR:',
              (3) wa_hcnt NO-GAP,
              ')',
              (2) ' ',
              (22) wa_sdmbts NO-ZERO,
              (22) wa_sdmbth NO-ZERO,
              sy-vline.
      ULINE AT (wa_width).

      CLEAR : wa_sdmbth, wa_sdmbts.
      CLEAR : wa_hcnt, wa_scnt.

    ENDAT.

    ADD 1 TO count.
    ADD 1 TO count2.
*Request by ghlee 2004.07.15: (issue  number FI-20040705-004)
*-------Start
    CASE radio.
      WHEN 'R_VEN' OR 'R_CUS'.
        AT END OF lifnr.
          DO.
            IF sy-linno > 54. "26
              EXIT.
            ENDIF.
*    WRITE :/.
            PERFORM write_empty_line.
          ENDDO.
          PERFORM sum_v1.
          PERFORM draw_sign_box USING '  '.
*          NEW-PAGE.
        ENDAT.
    ENDCASE.
*--------END
  ENDLOOP.

ENDFORM.                    " body_summary_vencus
*&---------------------------------------------------------------------*
*&      Form  body_summary_normal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM body_summary_normal USING radio.
  DATA: count   TYPE i VALUE 0,          " Loop counter for list body
         count2  TYPE i VALUE 0,          " Counter for stripes
         even    TYPE i VALUE 0,          " Checker even/odd
         help    TYPE i,
         wa_save_belnr LIKE bkpf-belnr,
         wa_blart      LIKE bkpf-blart,
         wa_sgtxt      LIKE bseg-sgtxt,
         wa_sdmbth     LIKE bseg-dmbtr,
         wa_sdmbts     LIKE bseg-dmbtr,
         wa_scnt       TYPE i,
         wa_hcnt       TYPE i,
         w_int         TYPE i,
         ind TYPE i.
  DATA: BEGIN OF l,
          txt20(20),      " account short text
          acct_type(48),
          taxcd(7),
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
  CLEAR : wa_hcnt_g,   wa_scnt_g,ind.
  LOOP AT it_bseg_sum.
    CLEAR it_l.
    AT NEW belnr.
      READ TABLE it_bkpf WITH KEY belnr = it_bseg_sum-belnr
                                  gjahr = it_bseg_sum-gjahr.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.

      wa_revtx = it_bkpf-revtx.
    ENDAT.

**-> Account type
    CASE it_bseg_sum-koart.
      WHEN 'A'. "asset
        SELECT SINGLE txt50
          FROM anla       "Asset Master Record Segment
          INTO it_l-name1
         WHERE bukrs = it_bkpf-bukrs
           AND anln1 = it_bseg_sum-anln1  "Asset
           AND anln2 = it_bseg_sum-anln2. "Sub-Number

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg_sum-anln1
             IMPORTING
                  output = it_l-code.

        CONCATENATE it_l-code     '-'
                    it_bseg_sum-anln2 '('
                    it_bseg_sum-anbwa ')'
               INTO it_l-code.
        IF it_bseg_sum-aufnr IS INITIAL.
          SELECT SINGLE eaufn INTO it_bseg_sum-aufnr
            FROM anla
            WHERE bukrs EQ it_bseg_sum-bukrs AND
                  anln1 EQ it_bseg_sum-anln1 AND
                  anln2 EQ it_bseg_sum-anln2.
        ENDIF.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg_sum-aufnr
             IMPORTING
                  output = it_l-assgn2.

*      MOVE IT_BSEG_SUM-aufnr  TO  it_l-assgn2.
        DATA: wa_l_temp(20).
        PERFORM get_position_id CHANGING it_l-assgn1 wa_l_temp.

      WHEN 'D'. "customers
        SELECT SINGLE name1
          FROM kna1       "General Data in Customer Master
          INTO it_l-name1
         WHERE kunnr = it_bseg_sum-kunnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg_sum-kunnr
             IMPORTING
                  output = it_l-code.
*---jhs modify 2003/09/02
        IF it_bseg_sum-umskz <> ' '.
          CONCATENATE it_l-code '/'
                      '('  it_bseg_sum-umskz  ')'
               INTO it_l-code.
        ENDIF.
*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)
        CONCATENATE it_bseg_sum-zterm '/'
                    it_bseg_sum-zlsch '/'
                    it_bseg_sum-zlspr
               INTO it_l-assgn1.

      WHEN 'K'. "vendors
*   --> account type (name + vendor + space.gl)
        SELECT SINGLE name1 INTO it_l-name1 FROM lfa1
          WHERE lifnr EQ it_bseg_sum-lifnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg_sum-lifnr
             IMPORTING
                  output = it_l-code.
*--jhs modify 2003/09/02
        IF it_bseg_sum-umskz <> ' '.
          CONCATENATE it_l-code '/'
                      '('  it_bseg_sum-umskz  ')'
               INTO it_l-code.
        ENDIF.
*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)
        CONCATENATE it_bseg_sum-zterm '/'
                    it_bseg_sum-zlsch '/'
                    it_bseg_sum-zlspr
               INTO it_l-assgn1.

      WHEN 'M'. "material
        PERFORM fill_grir USING it_bseg.

      WHEN 'S'. "G/L accounts
******>>> Investment
        IF it_bseg_sum-hkont+4(2) = '16'    OR
           it_bseg_sum-hkont+4(3) = '901'.
*    move  IT_BSEG_SUM-aufnr to it_l-assgn2.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
               EXPORTING
                    input  = it_bseg_sum-aufnr
               IMPORTING
                    output = it_l-assgn1.
          PERFORM get_position_id CHANGING it_l-code
                                           it_l-name1.

        ENDIF.
******>>> Expense
        IF it_bseg_sum-hkont+4(1) = '6'.
          DATA: it_list TYPE TABLE OF ifmkacoo WITH HEADER LINE.
          RANGES: r_kostl FOR bseg-kostl.
          IF ( it_bseg_sum-fistl IS INITIAL ) AND
             ( NOT it_bseg_sum-kostl IS INITIAL ).
            r_kostl-sign = 'I'.
            r_kostl-option = 'EQ'.
            r_kostl-low  = it_bseg_sum-kostl.
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
              MOVE it_list-fistl TO it_bseg_sum-fistl.
            ENDLOOP.
          ENDIF.

          SELECT SINGLE bezeich
            FROM fmfctrt                 "Funds Center Text
            INTO it_l-name1
           WHERE fictr = it_bseg_sum-fistl.  "Funds Center
          MOVE it_bseg_sum-fistl TO it_l-code.
        ENDIF.
*----start#1 wskim 03/01/2005
******>>> Cash :
*        DATA: wa_l_fdlev LIKE skb1-fdlev.
*        SELECT SINGLE fdlev INTO wa_l_fdlev FROM skb1
*          WHERE bukrs EQ it_bkpf-bukrs AND
*                saknr EQ it_bseg_sum-hkont.
*        IF wa_l_fdlev(1) = 'C' OR wa_l_fdlev(1) = 'B'.
*          CONCATENATE   it_bseg_sum-sgtxt(37) '('
*                        it_bseg_sum-fdtag+4(2) '/'
*                        it_bseg_sum-fdtag+6(2) '/'
*                        it_bseg_sum-fdtag+0(4) ')'
*                  INTO it_l-acct_type.
**
**           concatenate IT_BSEG_SUM-sgtxt(37) ':' IT_BSEG_SUM-fdtag
*        ENDIF.
******>>>> Cost
*        IF it_bseg_sum-bschl >= '80'.
*          PERFORM fill_material USING it_bseg_sum.
*        ENDIF.
******>>> GRIR
*        IF it_bseg_sum-bschl = '96' OR it_bseg_sum-bschl = '86'.
*          PERFORM fill_grir USING it_bseg_sum.
*        ENDIF.
*        IF it_bseg_sum-buzid = 'F'.   " PO freight, duty, ...
*          PERFORM fill_import USING it_bseg_sum.
*        ENDIF.
******>>> GRIR - Misc Import Expense
*        IF it_bkpf-blart = 'KI'.
*          it_l-acct_type = it_bseg_sum-sgtxt.
*        ENDIF.
*---end
    ENDCASE.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
         EXPORTING
              input  = it_l-code
         IMPORTING
              output = it_l-code.
    IF it_l-acct_type IS INITIAL.
      IF ( it_l-name1 IS INITIAL AND it_l-code IS INITIAL ) .
        CLEAR : it_l-acct_type.
      ELSE.
        CONCATENATE it_l-name1 '/' it_l-code INTO it_l-acct_type.
      ENDIF.
    ENDIF.

**--> Tax rate
    DATA: l_rate TYPE p DECIMALS 1.
    DATA: ls_rate(4).
    MOVE it_bseg_sum-mwskz TO it_l-taxcd.
    IF it_bseg_sum-fwbas NE 0.
      l_rate = it_bseg_sum-dmbtr / it_bseg_sum-fwbas * 100.
      it_l-rate = l_rate.
      SELECT SINGLE text1 INTO it_l-acct_type FROM t007s
        WHERE spras = sy-langu AND
              kalsm = 'TAXUS' AND
              mwskz = it_bseg_sum-mwskz.
      WRITE l_rate TO ls_rate.
      CONCATENATE it_l-acct_type '/' it_bseg_sum-mwskz '/' ls_rate
        INTO it_l-acct_type.

    ENDIF.

    DATA: BEGIN OF it_tax OCCURS 0,
             kschl LIKE konp-kschl,
             kbetr LIKE konp-kbetr,
          END OF it_tax.
    DATA: wa_l_tax LIKE bseg-dmbtr.

    IF it_bseg_sum-mwskz NE space AND  " exist tax code
       it_bseg_sum-shkzg EQ 'S'   AND  " only debit
       it_bkpf-bstat EQ 'V'.       " only parked doc.


      SELECT b~kschl b~kbetr INTO TABLE it_tax
        FROM a003 AS a INNER JOIN konp AS b ON b~knumh EQ a~knumh
        WHERE a~kappl = 'TX' AND
              a~aland = 'US' AND
              a~mwskz EQ it_bseg_sum-mwskz.


      LOOP AT it_tax WHERE kschl+3(1) EQ 'I'.    "case A/P
        wa_l_tax = it_bseg_sum-dmbtr * it_tax-kbetr / 1000.
        ADD wa_l_tax TO it_l-drt.
      ENDLOOP.
    ENDIF.

* Stripe handling (list body)
    IF stripes EQ ' '.                 " No Stripes
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ELSE.                              " Stripes
      even = count2 MOD 2.              " Check for even/odd
      IF even = '0'.
        FORMAT COLOR COL_NORMAL INTENSIFIED.
      ELSE.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      ENDIF.
    ENDIF.
*    IF it_l-acct_type = ' '.
*      READ TABLE it_bkpf WITH KEY belnr = IT_BSEG_SUM-belnr.
*      if sy-subrc = 0.
*        clear wa_ttext.
*        select single ttext into wa_ttext
*        from TSTCT
*        where SPRSL = SY-LANGU
*        AND   Tcode = it_bkpf-tcode.
*
*        it_l-acct_type = it_bkpf-tcode.
*        concatenate
*          it_bkpf-tcode ' ' wa_ttext+0(20) into it_l-acct_type.
*      endif.
*    ENDIF.

*----- Appended by BSBAe. 2004.06.09
    IF p_detail <> 'X'.
      CASE it_bseg_sum-koart.
        WHEN 'K' OR 'D'.
          CONCATENATE it_l-acct_type '/' it_bseg_sum-zlsch '/'
                      it_bseg-zterm  '/' it_bseg_sum-fdtag
                 INTO it_l-acct_type.
        WHEN 'A'.

        WHEN OTHERS.
*      IF it_bseg_sum-buzei IS INITIAL.
          CLEAR: it_l-acct_type.
          SELECT SINGLE * FROM t001 WHERE bukrs = it_bseg_sum-bukrs.

          SELECT SINGLE txt20 INTO it_l-acct_type(20)
            FROM skat
           WHERE spras = sy-langu
             AND ktopl = t001-ktopl
             AND saknr = it_bseg_sum-hkont.
*      ENDIF.

      ENDCASE.
    ENDIF.
*----- Appended by BSBAe. 2004.06.09
    IF wa_save_belnr NE it_bseg_sum-belnr.
      wa_save_belnr = it_bseg_sum-belnr.
      READ TABLE it_bkpf WITH KEY belnr = it_bseg_sum-belnr.
      IF sy-subrc = 0.
        wa_blart = it_bkpf-blart.
      ENDIF.
      WRITE : / sy-vline,
              it_bseg_sum-belnr,
              it_bseg_sum-buzei,
              it_bseg_sum-hkont,
              it_bseg_sum-bschl,
*              IT_BSEG_SUM-sgtxt,
            (51) it_l-acct_type,
            (22) it_bseg_sum-dmbts NO-ZERO,
            (22)  it_bseg_sum-dmbth NO-ZERO,
              sy-vline.
    ELSE.
      WRITE : / sy-vline,
              wa_blart,
              wa_revtx,
              it_bseg_sum-buzei,
              it_bseg_sum-hkont,
              it_bseg_sum-bschl,
*              IT_BSEG_SUM-sgtxt,
            (51) it_l-acct_type,
            (22) it_bseg_sum-dmbts NO-ZERO,
            (22) it_bseg_sum-dmbth NO-ZERO,
              sy-vline.

      wa_blart = ' '.
      wa_revtx = ' '.
    ENDIF.

    ADD it_bseg_sum-dmbth TO wa_sdmbth.
    ADD it_bseg_sum-dmbts TO wa_sdmbts.

    IF it_bkpf-revtx IS INITIAL.
      ADD it_bseg_sum-dmbth TO wa_sdmbth_g.
      ADD it_bseg_sum-dmbts TO wa_sdmbts_g.
    ENDIF.

    IF it_bseg_sum-dmbth NE 0.
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
*------start
    IF it_bseg_sum-buzid EQ 'P'.
      ADD it_bseg_sum-dmbth TO wa_sdmbth_d.
      ADD it_bseg_sum-dmbts TO wa_sdmbts_d.
    ENDIF.
*------end
    AT END OF belnr.
* check reverse doc
      IF NOT it_bkpf-stblg IS INITIAL.
        IF it_bkpf-stgrd IS INITIAL.
          SELECT SINGLE stgrd INTO it_bkpf-stgrd FROM bkpf
            WHERE bukrs = it_bkpf-bukrs AND
                  belnr = it_bkpf-stblg AND
                  gjahr = it_bkpf-stjah.
        ENDIF.
        DATA: wa_l_reverse(30).
        CONCATENATE ' REVERSED->'
                    it_bkpf-stblg '/'
                    it_bkpf-stgrd
              INTO  wa_l_reverse.
      ENDIF.
      FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
      WRITE : / sy-vline,
         (10) it_bkpf-bldat,
         (30) wa_l_reverse NO-GAP,
         (16) it_bkpf-xblnr,
         '( Dr:',
         (3) wa_scnt NO-GAP,
         '/ CR:',
         (3) wa_hcnt NO-GAP,
         ')',
         (2) ' ',
         (22) wa_sdmbts NO-ZERO,
         (22) wa_sdmbth NO-ZERO,
         sy-vline.
      ULINE AT (wa_width).
      CLEAR : wa_sdmbth, wa_sdmbts.
      CLEAR : wa_hcnt, wa_scnt.

    ENDAT.

    ADD 1 TO count.
    ADD 1 TO count2.
**Request by ghlee 2004.07.15: (issue  number FI-20040705-004)
**-------Start
*    CASE radio.
*      WHEN 'R_VEN' OR 'R_CUS'.
*        AT END OF lifnr.
*          DO.
*            IF sy-linno > 56. "26
*              EXIT.
*            ENDIF.
**    WRITE :/.
*            PERFORM write_empty_line.
*          ENDDO.
*          PERFORM sum_v1.
*          PERFORM draw_sign_box USING '  '.
**          NEW-PAGE.
*        ENDAT.
*    ENDCASE.
**--------END
  ENDLOOP.

ENDFORM.                    " body_summary_normal
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
  RANGES : r_usnam FOR bkpf-usnam.
  IF p_own EQ 'X'.
    r_usnam-sign   = 'I'.
    r_usnam-option = 'EQ'.
    r_usnam-low    = sy-uname.
    APPEND r_usnam.
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
        cpudt IN s_cpudt AND
        usnam IN r_usnam AND
        bstat IN r_bstat.

ENDFORM.                    " get_bkpf_data
*&---------------------------------------------------------------------*
*&      Form  get_bseg_details
*&---------------------------------------------------------------------*
FORM get_bseg_details.
  LOOP AT it_bkpf.
*----- Appended by BSBAE. 2004.06.08
    CASE it_bkpf-awtyp.
      WHEN 'MKPF'.
        DATA: lv_awkey LIKE bkpf-awkey.

        SELECT SINGLE *
          FROM mseg
         WHERE mblnr = it_bkpf-awkey(10)
           AND mjahr = it_bkpf-awkey+10(4)
           AND bwart IN ('101','102').
        IF sy-subrc EQ 0.
          it_bkpf-revtx = 'REV'.
          MODIFY it_bkpf.

          CONCATENATE mseg-lfbnr mseg-lfbja INTO lv_awkey.
          READ TABLE it_bkpf WITH KEY awtyp = 'MKPF'
                                      awkey = lv_awkey.
          IF sy-subrc EQ 0.
            it_bkpf-revtx = 'REV'.
            MODIFY it_bkpf INDEX sy-tabix.
          ENDIF.
        ENDIF.
      WHEN 'RMRP'.
        SELECT SINGLE *
          FROM rbkp
         WHERE stblg = it_bkpf-awkey(10)
           AND stjah = it_bkpf-awkey+10(4).
        IF sy-subrc EQ 0.
          it_bkpf-revtx = 'REV'.
          MODIFY it_bkpf.

          CONCATENATE rbkp-belnr rbkp-stjah INTO lv_awkey.
          READ TABLE it_bkpf WITH KEY awtyp = 'RMRP'
                                      awkey = lv_awkey.
          IF sy-subrc EQ 0.
            it_bkpf-revtx = 'REV'.
            MODIFY it_bkpf INDEX sy-tabix.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
*----- Appended by BSBAE. 2004.06.08

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
    IF it_bseg-shkzg = 'H'.
      MOVE it_bseg-dmbtr TO it_bseg-dmbth.
      MOVE 0             TO it_bseg-dmbts.
    ELSE.
      MOVE it_bseg-dmbtr TO it_bseg-dmbts.
      MOVE 0             TO it_bseg-dmbth.
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

ENDFORM.                    " get_company_info
