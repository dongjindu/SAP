************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: REPORT ZAFI_WARRANTY_POSTING                               *
*& Type   : Report                                                     *
*& Author : Manjunath   Venkatesh                                      *
*& Title  : Warranty Posting Programs                                  *
*&---------------------------------------------------------------------*
* Help Desk Request No  :                                              *
* System Id:    5CD963571A                                             *
*                                                                      *
*   Requested by:   Andy Choi                                          *
*   Assigned to:    Sudhakar                                           *
*   Original Request #:                                                *
*   ABAP Analyst:  Manjunath Venkatesh                                 *
*                                                                      *
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
*                                                                      *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
*                                                                      *
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >                               *
*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:                                                     *
*       Online Report                                                  *
*                                                                      *
* Authorization Checks:                                                *
*     < Document if any authorization objects are checked >            *
*                                                                      *
* Direct Update Database Tables:                                       *
*   < No direct updates to SAP tables are allowed.List custom tables > *
*                                                                      *
* Outstanding Issues:                                                  *
*     < If the program is being delivered with any known open issues   *
*       document them here; they could be planned for next release >   *
*                                                                      *
* Instructions on how to test this program:                            *
*     < If this program needs any special inputs like an inbound file  *
*       from an EDI subsystem for testing, document it here >          *
*                                                                      *
* Instructions on how to re-start this program:                        *
*                                                                      *
* Volume Estimates:                                                    *
*                                                                      *
* Frequency of Execution:                                              *
*   o On demand                                                        *
*                                                                      *
* Execution Mode:                                                      *
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 07/14/06    Manjunath    UD1K921321   Initial Coding
* 08/22/06    Manju        UD1K921767   Warranty Calculation Changes
* 12/02/09    IG.MOON      UD1K948021   Fix bug in messasge
* 28/04/10    Valerian     UD1K948903   Enhance Warranty Program. The
*             HIS20094                  program will replace current
*                                       model which is maintain using
*                                       spread sheet.
* 07/07/10    Valerian     UD1K949377   Change doc type from AC to SA
*             HIS20094                  (Step-2)
* 07/13/10    Valerian     UD1K949401   Activate save/change layout
*             HIS20094                  functionality.
* 02/14/11    Valerian     UD1K950854   Correct formula to calculate
*                                       Present Value.
* 08/23/11    YN.Kim       UD1K950854   ECC 6.0 Upgrade.
* 01/09/2012  Valerian     UD1K953691   Exclude 'Material Number' in
*                                       Collect Criteria
************************************************************************
REPORT  zafi_warranty_posting  LINE-SIZE 132 LINE-COUNT 65
                               NO STANDARD PAGE HEADING
                               MESSAGE-ID db .

TYPES: ty_pack(16) TYPE p DECIMALS 14.                      "HIS20094
*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES : ztfi_warranty,
         ztfi_war_sales,
         ztfi_war_post,
         bseg,
         ztco_model_map.

INCLUDE: <icon>.
*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
* Internal Tables
DATA : it_tab LIKE TABLE OF ztfi_war_sales WITH HEADER LINE,
       it_war LIKE TABLE OF ztfi_warranty WITH HEADER LINE,
       gt_list LIKE TABLE OF zfi_warranty_alv_str.

DATA : BEGIN OF it_alv OCCURS 0,
       bukrs   LIKE ztfi_war_sales-bukrs,
       p_gjahr LIKE ztfi_war_sales-s_gjahr,                 "VALERIAN
       p_monat LIKE ztfi_war_sales-monat,                   "VALERIAN
       land1   LIKE ztfi_war_sales-land1,
       model   LIKE ztfi_war_sales-model,
       m_gjahr LIKE ztfi_war_sales-m_gjahr,
       s_gjahr LIKE ztfi_war_sales-s_gjahr,
       monat   LIKE ztfi_war_sales-monat,
       matnr   LIKE ztfi_war_sales-matnr,
       sal_qty LIKE ztfi_war_sales-sal_qty,
       wrbtr   LIKE ztfi_warranty-wrbtr,
       versn   LIKE ztfi_warranty-versn,
       mod_yr  TYPE bkpf-gjahr,                             "HIS20094
       mod_nm(30) TYPE c,                                   "HIS20094
       cnty_tx TYPE t005t-landx,                            "HIS20094
       cnty_cd(2) TYPE c,                                   "HIS20094
** On 03/07/14  peroid from 2 digits to 3 digits (
*       war_per TYPE ztfi_war_sales-monat,                   "HIS20094
       war_per TYPE ztfi_war_post-war_per,
** ) End
       gross   TYPE bseg-dmbtr,                             "HIS20094
       gross_pp TYPE bseg-dmbtr,                            "HIS20094
       gross_var TYPE bseg-dmbtr,                           "HIS20094
       gross_st TYPE bseg-dmbtr,                            "HIS20094
       gross_lt TYPE bseg-dmbtr,                            "HIS20094
       net      TYPE bseg-dmbtr,                            "HIS20094
       net_pp   TYPE bseg-dmbtr,                            "HIS20094
       net_var  TYPE bseg-dmbtr,                            "HIS20094
       net_st   TYPE bseg-dmbtr,                            "HIS20094
       net_lt   TYPE bseg-dmbtr,                            "HIS20094
       pvnet     TYPE bseg-dmbtr,                           "HIS20094
       pvnet_pp  TYPE bseg-dmbtr,                           "HIS20094
       pvnet_var TYPE bseg-dmbtr,                           "HIS20094
       pvnet_st  TYPE bseg-dmbtr,                           "HIS20094
       pvnet_lt  TYPE bseg-dmbtr,                           "HIS20094
       suppl_fl  TYPE bseg-dmbtr,                           "HIS20094
       suppl_st  TYPE bseg-dmbtr,                           "HIS20094
       suppl_lt  TYPE bseg-dmbtr,                           "HIS20094
       pvsnet    TYPE bseg-dmbtr,                           "HIS20094
       pvsnet_st TYPE bseg-dmbtr,                           "HIS20094
       pvsnet_lt TYPE bseg-dmbtr,                           "HIS20094
       tgross   TYPE bseg-dmbtr,                            "HIS20094
       tgross_pp TYPE bseg-dmbtr,                           "HIS20094
       tgross_var TYPE bseg-dmbtr,                          "HIS20094
       tgross_st TYPE bseg-dmbtr,                           "HIS20094
       tgross_lt TYPE bseg-dmbtr,                           "HIS20094
       tnet      TYPE bseg-dmbtr,                           "HIS20094
       tnet_pp   TYPE bseg-dmbtr,                           "HIS20094
       tnet_var  TYPE bseg-dmbtr,                           "HIS20094
       tnet_st   TYPE bseg-dmbtr,                           "HIS20094
       tnet_lt   TYPE bseg-dmbtr,                           "HIS20094
       tpvnet     TYPE bseg-dmbtr,                          "HIS20094
       tpvnet_pp  TYPE bseg-dmbtr,                          "HIS20094
       tpvnet_var TYPE bseg-dmbtr,                          "HIS20094
       tpvnet_st  TYPE bseg-dmbtr,                          "HIS20094
       tpvnet_lt  TYPE bseg-dmbtr,                          "HIS20094
       itrst      TYPE bseg-dmbtr,                          "HIS20094
       itrst_a    TYPE bseg-dmbtr,                          "HIS20094
       amort      TYPE bseg-dmbtr,                          "HIS20094
       tsuppl_fl  TYPE bseg-dmbtr,                          "HIS20094
       tsuppl_st  TYPE bseg-dmbtr,                          "HIS20094
       tsuppl_lt  TYPE bseg-dmbtr,                          "HIS20094
       tpvsnet    TYPE bseg-dmbtr,                          "HIS20094
       tpvsnet_st TYPE bseg-dmbtr,                          "HIS20094
       tpvsnet_lt TYPE bseg-dmbtr,                          "HIS20094
       descr      TYPE ZFI_WARRANTY_ALV_STR-DESCR,          "VALERIAN
       icon    LIKE zfi_warranty_alv_str-icon,
       belum(25)  TYPE c,                                   "HIS20094
*      belum   TYPE zfi_warranty_alv_str-belum,             "HIS20094
       msg     LIKE zfi_warranty_alv_str-msg,
       END OF it_alv.

DATA : out_tab LIKE TABLE OF it_alv WITH HEADER LINE.

DATA :  p_date LIKE vtbbewe-dbervon,
        l_dated LIKE vtbbewe-dbervon,
        w_repid LIKE sy-repid,
        p_per(6) TYPE c.

** Internal table  for Model
DATA : BEGIN OF it_model OCCURS 0,
       model   LIKE ztfi_war_sales-model,
       land1   LIKE ztfi_war_sales-land1,
       wrbtr   LIKE ztfi_warranty-wrbtr,
       tpvnet  TYPE bseg-dmbtr,                             "HIS20094
       itrst_a TYPE bseg-dmbtr,                             "HIS20094
*//    2011.08.11 insert  by kim.YN
*      matnr   like mara-matnr,                 "2011.08.11 "UD1K953691
       END OF it_model.
** Internal table
DATA  : BEGIN OF it_prod OCCURS 0,
         matnr   LIKE ztfi_war_sales-matnr,
         wrbtr   LIKE ztfi_warranty-wrbtr,
        END OF it_prod.

* ALV Declarations
DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.
*      Global variables for attributes or etc of ALV GRID
DATA :
       it_fieldcat1    TYPE lvc_t_fcat WITH HEADER LINE,
*       it_fieldcat     TYPE slis_t_fieldcat_alv,
       it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout   TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA : wa_variant TYPE disvariant. "for parameter IS_VARIANT

DATA: wa_save    TYPE c   VALUE 'A',
      l_mode(1)  TYPE c   VALUE 'E' .

DATA :lt_return  LIKE TABLE OF bapiret2 WITH HEADER LINE  ,
      l_date  LIKE sy-datum,
      l_date1 LIKE sy-datum,
      l_date2(10) TYPE c,
      l_date3(10) TYPE c,
      l_amt(15) TYPE c,
      l_text(40) TYPE c,
      l_sysdate(10) TYPE c..


* FOR BDC
* BDC Table
DATA:BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA:END OF bdcdata.

* Message table
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

* BEGIN OF HIS20094

* Calculation Structure
DATA: BEGIN OF it_calc,
       gross       TYPE dmbtr,
       gross_pp    TYPE dmbtr,
       gross_var   TYPE dmbtr,
       gross_st    TYPE dmbtr,
       gross_lt    TYPE dmbtr,
       net         TYPE dmbtr,
       net_pp      TYPE dmbtr,
       net_var     TYPE dmbtr,
       net_st      TYPE dmbtr,
       net_lt      TYPE dmbtr,
       pvnet       TYPE dmbtr,
       pvnet_pp    TYPE dmbtr,
       pvnet_var   TYPE dmbtr,
       pvnet_st    TYPE dmbtr,
       pvnet_lt    TYPE dmbtr,
       suppl_fl    TYPE dmbtr,
       suppl_st    TYPE dmbtr,
       suppl_lt    TYPE dmbtr,
       pvsnet      TYPE dmbtr,
       pvsnet_st   TYPE dmbtr,
       pvsnet_lt   TYPE dmbtr,
       tgross      TYPE dmbtr,
       tgross_pp   TYPE dmbtr,
       tgross_var  TYPE dmbtr,
       tgross_st   TYPE dmbtr,
       tgross_lt   TYPE dmbtr,
       tnet        TYPE dmbtr,
       tnet_pp     TYPE dmbtr,
       tnet_var    TYPE dmbtr,
       tnet_st     TYPE dmbtr,
       tnet_lt     TYPE dmbtr,
       tpvnet      TYPE dmbtr,
       tpvnet_pp   TYPE dmbtr,
       tpvnet_var  TYPE dmbtr,
       tpvnet_st   TYPE dmbtr,
       tpvnet_lt   TYPE dmbtr,
       itrst       TYPE dmbtr,
       itrst_a     TYPE dmbtr,
       amort       TYPE dmbtr,
       tsuppl_fl   TYPE dmbtr,
       tsuppl_st   TYPE dmbtr,
       tsuppl_lt   TYPE dmbtr,
       tpvsnet     TYPE dmbtr,
       tpvsnet_st  TYPE dmbtr,
       tpvsnet_lt  TYPE dmbtr,
      END OF it_calc.

DATA: v_itrst_a    TYPE dmbtr,
      v_tpvnet_st  TYPE dmbtr,
      v_tpvnet_lt  TYPE dmbtr,
      v_tpvsnet_st TYPE dmbtr,
      v_tpvsnet_lt TYPE dmbtr,
      v_recl_rate  TYPE dmbtr,
      v_disc_rate  TYPE dmbtr.

* END OF HIS20094

* 2011.08.24 insert by YN.Kim
data: gv_matnr   like  mara-matnr,
      gv_prdha   like  mara-prdha.   "sales order

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETER :  p_year  LIKE bseg-gjahr OBLIGATORY,
             p_month LIKE bsak-monat OBLIGATORY.
*            p_p1 LIKE ztfi_war_sales-pdate NO-DISPLAY.
SELECTION-SCREEN END   OF BLOCK b1.
SELECT-OPTIONS : s_cty FOR ztfi_war_sales-land1,
                 s_model   FOR ztfi_war_sales-model,
                 s_myear   FOR ztfi_war_sales-m_gjahr,
                 s_matnr   FOR ztfi_war_sales-matnr.

* BEGIN OF HIS20094
PARAMETERS : p_vm(1) TYPE c DEFAULT 'X' NO-DISPLAY,
             p_prod(1) TYPE c NO-DISPLAY.


*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
*
*PARAMETERS : p_vm RADIOBUTTON GROUP r1,
*             p_prod  RADIOBUTTON GROUP r1.
*
*SELECTION-SCREEN END   OF BLOCK b2.
* END OF HIS20094

PARAMETERS: p_ver LIKE ztfi_warranty-versn.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN COMMENT /1(65) text-901.
SELECTION-SCREEN COMMENT /1(60) text-902.
SELECTION-SCREEN COMMENT /1(60) text-903.

SELECTION-SCREEN ULINE.

*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
START-OF-SELECTION.

* Select Data
  PERFORM select_data.

* Calculate Warranty Amount.
  PERFORM do_processing.

* Display ALV Grid
  PERFORM display_alv.

*-------------------------------------------------------------*
* End-of-selection
*--------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  CONCATENATE  p_year p_month INTO p_per.
  SELECT * INTO TABLE it_tab FROM ztfi_war_sales
       WHERE  land1 IN s_cty
         AND  model IN s_model
         AND  m_gjahr IN s_myear
         AND  matnr   IN s_matnr
         AND  s_gjahr < p_year.

  SELECT * APPENDING TABLE it_tab FROM ztfi_war_sales
       WHERE  land1 IN s_cty
         AND  model IN s_model
         AND  m_gjahr IN s_myear
         AND  matnr   IN s_matnr
         AND  s_gjahr = p_year
         AND  monat  <= p_month.

*  DELETE it_tab WHERE ( pdate EQ  p_per AND
*                         flag = 'X' ).

  IF it_tab[] IS INITIAL.
    MESSAGE e000(07) WITH ' No Data to Process'.
  ENDIF.

  IF p_ver = space.
    SELECT * INTO TABLE it_war FROM ztfi_warranty
             WHERE versn = ( SELECT MAX( versn ) FROM ztfi_warranty ).

* BEGIN OF HIS20094
    SELECT SINGLE recl_rate disc_rate
      INTO (v_recl_rate, v_disc_rate)
      FROM ztfi_warranty_h
     WHERE versn = ( SELECT MAX( versn ) FROM ztfi_warranty ).

    IF sy-subrc NE 0.
      MESSAGE e208(00)
         WITH 'Maintain Recl.Rate/Disc.Rate for latest version'.
      LEAVE PROGRAM.
    ENDIF.
* END OF HIS20094

  ELSE.
    SELECT * INTO TABLE it_war FROM ztfi_warranty
             WHERE versn = p_ver.

* BEGIN OF HIS20094
    IF sy-subrc EQ 0.
      SELECT SINGLE recl_rate disc_rate
        INTO (v_recl_rate, v_disc_rate)
        FROM ztfi_warranty_h
       WHERE versn = p_ver.

      IF sy-subrc NE 0.
        MESSAGE e368(00)
           WITH 'Maintain Reclaim Rate/Discount Rate for version'
                 p_ver.
        LEAVE PROGRAM.
      ENDIF.
    ELSE.
      MESSAGE e368(00) WITH 'Maintain Full Warranty Rate for version'
                             p_ver.
      LEAVE PROGRAM.
    ENDIF.
* END OF HIS20094
  ENDIF.

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  do_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_processing.

  DATA : e_days   LIKE  vtbbewe-atage,
         e_months LIKE  vtbbewe-atage,
         e_years  LIKE  vtbbewe-atage,
         p_amt    LIKE  ztfi_warranty-wrbtr,
         p_yamt    LIKE  ztfi_warranty-wrbtr,
         p_mamt    LIKE  ztfi_warranty-wrbtr,
         w_field(14) TYPE c,
         w_field1(14) TYPE c,
         l_year(2) TYPE c,
         l_month(2) TYPE n,
         l_year1(2) TYPE c.
  FIELD-SYMBOLS: <fs>,<fs1>.

* BEGIN OF HIS20094
  DATA: l_war_per     TYPE i,
        l_dum_amt     TYPE ty_pack,
        l_suppl_prcnt TYPE f,
        l_period      TYPE i,
        l_period1     TYPE i,
        l_hmma_prcnt  TYPE f,
        l_pvdisc_base TYPE f.                              "UD1K950854

  l_hmma_prcnt = 1 - ( v_recl_rate / 100 ).
  l_suppl_prcnt = v_recl_rate / 100.                       "UD1K950854
  l_pvdisc_base = 1 + v_disc_rate / 100.                   "UD1K950854
* END OF HIS20094

* Posting Date
  CONCATENATE  p_year p_month '01' INTO  p_date.

  LOOP AT it_tab.

* Sales Date
    CONCATENATE it_tab-s_gjahr it_tab-monat '01'
                INTO  l_dated.

* Determine number of years & month between Posting date &
* Sales Date.

    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
         EXPORTING
              i_date_from    = l_dated
              i_date_to      = p_date
              i_flg_separate = 'X'
         IMPORTING
              e_days         = e_days
              e_months       = e_months
              e_years        = e_years.

    l_month = e_months.

* Read Warranty Data
    READ TABLE it_war WITH KEY land1   = it_tab-land1
                               model   = it_tab-model
                               m_gjahr = it_tab-m_gjahr.

    IF  sy-subrc EQ 0.
* BEGIN OF HIS20094

      l_period = e_years * 12 + e_months.

      l_war_per = l_period + 1.
      it_alv-war_per = l_war_per.

      PERFORM get_keydata.

      PERFORM get_amortization USING l_period CHANGING it_calc-gross.

      l_period1 = l_period - 1.
     PERFORM get_amortization USING l_period1 CHANGING it_calc-gross_pp.
      it_calc-gross_var = it_calc-gross_pp - it_calc-gross.

      l_period1 = l_period + 12.
     PERFORM get_amortization USING l_period1 CHANGING it_calc-gross_lt.
      it_calc-gross_st = it_calc-gross - it_calc-gross_lt.

      it_calc-net     = it_calc-gross * l_hmma_prcnt.
      it_calc-net_pp  = it_calc-gross_pp * l_hmma_prcnt.
      it_calc-net_var = it_calc-gross_var * l_hmma_prcnt.
      it_calc-net_st  = it_calc-gross_st * l_hmma_prcnt.
      it_calc-net_lt  = it_calc-gross_lt * l_hmma_prcnt.

      PERFORM get_present_val USING l_period l_hmma_prcnt l_pvdisc_base
                           CHANGING it_calc-pvnet it_calc-pvnet_st.

      l_period1 = l_period - 1.
      PERFORM get_present_val USING l_period1 l_hmma_prcnt l_pvdisc_base
                           CHANGING it_calc-pvnet_pp l_dum_amt.

      it_calc-pvnet_var = it_calc-pvnet_pp - it_calc-pvnet.
      it_calc-pvnet_lt  = it_calc-pvnet - it_calc-pvnet_st.

      it_calc-suppl_fl = it_calc-gross - it_calc-net.
      it_calc-suppl_st = it_calc-gross_st - it_calc-net_st.
      it_calc-suppl_lt = it_calc-gross_lt - it_calc-net_lt.

*     l_suppl_prcnt = v_recl_rate / 100.                   "UD1K950854
      PERFORM get_present_val USING l_period l_suppl_prcnt l_pvdisc_base
                           CHANGING it_calc-pvsnet it_calc-pvsnet_st.
      it_calc-pvsnet_lt  = it_calc-pvsnet - it_calc-pvsnet_st.

      it_calc-tgross     = it_calc-gross * it_tab-sal_qty.
      it_calc-tgross_pp  = it_calc-gross_pp * it_tab-sal_qty.
      it_calc-tgross_var = it_calc-tgross_pp - it_calc-tgross.
      it_calc-tgross_st  = it_calc-gross_st * it_tab-sal_qty.
      it_calc-tgross_lt  = it_calc-gross_lt * it_tab-sal_qty.

      it_calc-tnet     = it_calc-net * it_tab-sal_qty.
      it_calc-tnet_pp  = it_calc-net_pp * it_tab-sal_qty.
      it_calc-tnet_var = it_calc-tnet_pp - it_calc-tnet.
      it_calc-tnet_st  = it_calc-net_st * it_tab-sal_qty.
      it_calc-tnet_lt  = it_calc-net_lt * it_tab-sal_qty.

      it_calc-tpvnet     = it_calc-pvnet * it_tab-sal_qty.
      it_calc-tpvnet_pp  = it_calc-pvnet_pp * it_tab-sal_qty.
      it_calc-tpvnet_var = it_calc-tpvnet_pp - it_calc-tpvnet.
      it_calc-tpvnet_st  = it_calc-pvnet_st * it_tab-sal_qty.
      it_calc-tpvnet_lt  = it_calc-pvnet_lt * it_tab-sal_qty.

      it_calc-itrst   = it_calc-tnet - it_calc-tpvnet.
      IF NOT it_calc-tpvnet_var IS INITIAL.
       it_calc-itrst_a = it_calc-tpvnet_pp * ( v_disc_rate / 12 / 100 ).
      ENDIF.
      it_calc-amort   = it_calc-tpvnet_var - it_calc-itrst_a.


      it_calc-tsuppl_fl = it_calc-suppl_fl * it_tab-sal_qty.
      it_calc-tsuppl_st = it_calc-suppl_st * it_tab-sal_qty.
      it_calc-tsuppl_lt = it_calc-suppl_lt * it_tab-sal_qty.

      it_calc-tpvsnet    = it_calc-pvsnet * it_tab-sal_qty.
      it_calc-tpvsnet_st = it_calc-pvsnet_st * it_tab-sal_qty.
      it_calc-tpvsnet_lt = it_calc-pvsnet_lt * it_tab-sal_qty.

      MOVE-CORRESPONDING it_calc TO it_alv. CLEAR it_calc.

      v_itrst_a    = v_itrst_a + it_alv-itrst_a.
      v_tpvnet_st  = v_tpvnet_st + it_alv-tpvnet_st.
      v_tpvnet_lt  = v_tpvnet_lt + it_alv-tpvnet_lt.
      v_tpvsnet_st = v_tpvsnet_st + it_alv-tpvsnet_st.
      v_tpvsnet_lt = v_tpvsnet_lt + it_alv-tpvsnet_lt.
* END OF HIS20094

      it_alv-p_gjahr = p_year.                              "VALERIAN
      it_alv-p_monat = p_month.                             "VALERIAN

* If warranty period is > 1 year & Month > 1
      IF   e_years  > 0 AND l_month > 0.
        l_year = e_years.                                   "UD1K921767
        l_year1 = e_years.
* Calculate  Amortization per Year
        p_yamt = it_war-wrbtr.
        WHILE l_year1 <> 0.
          CONCATENATE  'it_war-WRBTR' l_year1 INTO w_field.
          ASSIGN (w_field) TO <fs>.
          p_yamt =  p_yamt - <fs> .
          l_year1 = l_year1 - 1 .
        ENDWHILE.
* Calculate  Amortization per Period
        l_year = l_year + 1.
        CONCATENATE  'it_war-WRBTR' l_year INTO w_field1.
        ASSIGN (w_field1) TO <fs1>.
        p_mamt = <fs1> / 12 .
        p_mamt = p_mamt * l_month.
        p_amt = p_yamt - p_mamt.
**        l_year = e_years.             "UD1K921767
*        CONCATENATE  'it_war-WRBTR' l_year INTO w_field.
*        l_year = l_year + 1.
*        CONCATENATE  'it_war-WRBTR' l_year INTO w_field1.
*        CHECK  sy-subrc EQ 0.
*        ASSIGN (w_field) TO <fs>.
*        ASSIGN (w_field1) TO <fs1>.
*        p_amt =  it_war-wrbtr  -
*                     ( <fs> +  ( <fs1> * l_month )  / 12  ) .
* Warranty Period is absoulte Year and Month is ZERO
      ELSEIF   e_years  > 0 AND l_month EQ 0.
        l_year = e_years.
        p_amt = it_war-wrbtr.
        WHILE l_year <> 0.
          CONCATENATE  'it_war-WRBTR' l_year INTO w_field.
          ASSIGN (w_field) TO <fs>.
          p_amt =  p_amt - <fs> .
          l_year = l_year - 1 .
        ENDWHILE.

*        CONCATENATE  'it_war-WRBTR' l_year INTO w_field.
*        CHECK  sy-subrc EQ 0.
*        ASSIGN (w_field) TO <fs>.
*        p_amt =  it_war-wrbtr  - <fs> .
* Warranty Period is less than a Year
      ELSE.
        l_year = 1.
        CONCATENATE  'it_war-WRBTR' l_year INTO w_field.
        CHECK  sy-subrc EQ 0.
        ASSIGN (w_field) TO <fs>.
        p_amt =  it_war-wrbtr  - ( <fs> * l_month ) / 12 .
      ENDIF.
      p_amt = p_amt *   it_tab-sal_qty.
      MOVE-CORRESPONDING it_tab TO it_alv.
      MOVE-CORRESPONDING it_war TO it_alv.
      it_alv-wrbtr = p_amt.
      MOVE: icon_light_out TO it_alv-icon.
    ELSE.
* Otherwise Display Warning Message
      MOVE-CORRESPONDING it_tab TO it_alv.
*     MOVE-CORRESPONDING it_war TO it_alv.
      MOVE: icon_yellow_light TO it_alv-icon.
      MOVE 'Warranty Data not Maintained' TO it_alv-msg.
    ENDIF.

* BEGIN OF VALERIAN
    IF it_alv-p_gjahr = it_alv-s_gjahr AND it_alv-p_monat = it_alv-monat.
      it_alv-descr = 'NEW'.
    ELSE.
      it_alv-descr = 'AMORTIZATION'.
    ENDIF.
* END OF VALERIAN

    APPEND it_alv. CLEAR it_alv.
  ENDLOOP.

ENDFORM.                    " do_processing
*&---------------------------------------------------------------------*
*&      Form  display_ALV
*&---------------------------------------------------------------------*
FORM display_alv.

  CALL SCREEN 100.

ENDFORM.                    " display_ALV
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'MYPF'.
  SET TITLEBAR 'MYT'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'SIMU'.
      CLEAR: sy-ucomm.
      PERFORM accrual_simulation.
    WHEN  'POST'.
      CLEAR: sy-ucomm.
      PERFORM accural_postings.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  create_container  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_container OUTPUT.
  IF grid_container IS INITIAL.
    CREATE OBJECT grid_container
            EXPORTING container_name = 'MY_CONT'
            EXCEPTIONS
             cntl_error = 1
             cntl_system_error = 2
             create_error = 3
             lifetime_error = 4
             lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
      w_repid = sy-repid.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                titel = w_repid
                txt2  = sy-subrc
                txt1  = 'The control can not be created'.
    ENDIF.

    CREATE OBJECT alv_grid
           EXPORTING i_parent = grid_container.
*                    i_appl_events = 'X'.                   "HIS20094

  ELSE.

    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.
ENDMODULE.                 " create_container  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_attribute  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_attribute OUTPUT.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDMODULE.                 " set_attribute  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_grid  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_grid OUTPUT.
  DATA it_sort1 TYPE lvc_t_sort WITH HEADER LINE.
**  it_sort1-spos = '1'.
**  it_sort1-fieldname = 'MONAT'.
**  APPEND it_sort1.
*
*  it_sort1-spos = '2'.
*  it_sort1-fieldname = 'WRBTR'.
*  it_sort1-subtot = 'X'.
*  APPEND it_sort1.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZFI_WARRANTY_ALV_STR'
       CHANGING
            ct_fieldcat      = it_fieldcat[].

  LOOP AT it_fieldcat.
    CASE it_fieldcat-fieldname.

      WHEN 'MONAT'
        OR 'BUKRS'                                          "HIS20094
        OR 'LAND1'                                          "HIS20094
        OR 'MODEL'                                          "HIS20094
        OR 'M_GJAHR'                                        "HIS20094
        OR 'S_GJAHR'                                        "HIS20094
        OR 'MATNR'                                          "HIS20094

        OR 'P_GJAHR'                                        "VALERIAN
        OR 'P_MONAT'.                                       "VALERIAN

        it_fieldcat-key = 'X'.

* BEGIN OF VALERIAN
      IF it_fieldcat-fieldname = 'P_MONAT'.
        PERFORM rename_field USING 'Posting Period'
                            'Posting Period'.
      ENDIF.
* END OF VALERIAN

      WHEN 'SAL_QTY'.
        it_fieldcat-do_sum = 'X'.
      WHEN 'WRBTR'.
        it_fieldcat-do_sum = 'X'.

* BEGIN OF HIS20094
      WHEN 'MOD_YR'.
        PERFORM rename_field USING 'Mod.Yr.'
                                   'Model Year'.

      WHEN 'MOD_NM'.
        PERFORM rename_field USING 'Mod.Nm.'
                                   'Model Name'.

      WHEN 'CNTY_TX'.
        PERFORM rename_field USING 'Cnty.Nm.'
                                   'Country'.

      WHEN 'CNTY_CD'.
        PERFORM rename_field USING 'Cnty.Cd.'
                                   'Country Code'.

      WHEN 'WAR_PER'.
        PERFORM rename_field USING 'War.Per.'
                                   'Warranty Period'.

      WHEN 'GROSS'.
        PERFORM rename_field USING 'Gross'
                                   'Gross (Full Warranty/Unit)'.

      WHEN 'GROSS_PP'.
        PERFORM rename_field USING 'Gross PP.'
                            'Gross Prior Period (Full Warranty/Unit)'.

      WHEN 'GROSS_VAR'.
        PERFORM rename_field USING 'Gross Var.'
                                   'Period Change (Full Warranty/Unit)'.

      WHEN 'GROSS_ST'.
        PERFORM rename_field USING 'Gross ST.'
                                   'Short Term (Full Warranty/Unit)'.

      WHEN 'GROSS_LT'.
        PERFORM rename_field USING 'Gross LT.'
                                   'Long Term (Full Warranty/Unit)'.

      WHEN 'NET'.
        PERFORM rename_field USING 'Net'
                                   'Net (HMMA Liability/Unit)'.

      WHEN 'NET_PP'.
        PERFORM rename_field USING 'Net PP.'
                            'Net Prior Period (HMMA Liability/Unit)'.

      WHEN 'NET_VAR'.
        PERFORM rename_field USING 'Net Var.'
                             'Period Change (HMMA Liability/Unit)'.

      WHEN 'NET_ST'.
        PERFORM rename_field USING 'Net ST.'
                                   'Short Term (HMMA Liability/Unit)'.

      WHEN 'NET_LT'.
        PERFORM rename_field USING 'Net LT.'
                                   'Long Term (HMMA Liability/Unit)'.

      WHEN 'PVNET'.
        PERFORM rename_field USING 'PV.Net'
                                   'Net (Present Value/Unit)'.

      WHEN 'PVNET_PP'.
        PERFORM rename_field USING 'PV.Net PP.'
                            'Net Prior Period (Present Value/Unit)'.

      WHEN 'PVNET_VAR'.
        PERFORM rename_field USING 'PV.Net Var.'
                                   'Period Change (Present Value/Unit)'.

      WHEN 'PVNET_ST'.
        PERFORM rename_field USING 'PV.Net ST.'
                                   'Short Term (Present Value/Unit)'.

      WHEN 'PVNET_LT'.
        PERFORM rename_field USING 'PV.Net LT.'
                                   'Long Term (Present Value/Unit)'.

      WHEN 'SUPPL_FL'.
        PERFORM rename_field USING 'A/R Full'
                                   'Full A/R (Supplier/Unit)'.

      WHEN 'SUPPL_ST'.
        PERFORM rename_field USING 'A/R ST.'
                                   'Short Term A/R (Supplier/Unit)'.

      WHEN 'SUPPL_LT'.
        PERFORM rename_field USING 'A/R LT.'
                                   'Long Term A/R (Supplier/Unit)'.

      WHEN 'PVSNET'.
        PERFORM rename_field USING 'PV.A/R'
                                   'Net A/R (Present Value/Unit)'.

      WHEN 'PVSNET_ST'.
        PERFORM rename_field USING 'PV.A/R ST.'
                            'Short Term A/R (Present Value/Unit)'.

      WHEN 'PVSNET_LT'.
        PERFORM rename_field USING 'PV.A/R LT.'
                                   'Long Term A/R (Present Value/Unit)'.

      WHEN 'TGROSS'.
        PERFORM rename_field USING 'Tot.Gross'
                                   'Gross (Full Warranty Total)'.

      WHEN 'TGROSS_PP'.
        PERFORM rename_field USING 'Tot.Gross PP.'
                            'Gross Prior Period (Full Warranty Total)'.

      WHEN 'TGROSS_VAR'.
        PERFORM rename_field USING 'Tot.Gross Var.'
                             'Period Change (Full Warranty Total)'.

      WHEN 'TGROSS_ST'.
        PERFORM rename_field USING 'Tot.Gross ST.'
                                   'Short Term (Full Warranty Total)'.

      WHEN 'TGROSS_LT'.
        PERFORM rename_field USING 'Tot.Gross LT.'
                                   'Long Term (Full Warranty Total)'.

      WHEN 'TNET'.
        PERFORM rename_field USING 'Tot.Net'
                                   'Net (HMMA Liability Total)'.

      WHEN 'TNET_PP'.
        PERFORM rename_field USING 'Tot.Net PP.'
                            'Net Prior Period (HMMA Liability Total)'.

      WHEN 'TNET_VAR'.
        PERFORM rename_field USING 'Tot.Net Var.'
                            'Period Change (HMMA Liability Total)'.

      WHEN 'TNET_ST'.
        PERFORM rename_field USING 'Tot.Net ST.'
                                   'Short Term (HMMA Liability Total)'.

      WHEN 'TNET_LT'.
        PERFORM rename_field USING 'Tot.Net LT.'
                                   'Long Term (HMMA Liability Total)'.

      WHEN 'TPVNET'.
        PERFORM rename_field USING 'Tot.PV.Net'
                                   'Net (Present Value Total)'.

      WHEN 'TPVNET_PP'.
        PERFORM rename_field USING 'Tot.PV.Net PP.'
                            'Net Prior Period (Present Value Total)'.

      WHEN 'TPVNET_VAR'.
        PERFORM rename_field USING 'Tot.PV.Net Var.'
                            'Period Change (Present Value Total)'.

      WHEN 'TPVNET_ST'.
        PERFORM rename_field USING 'Tot.PV.Net ST.'
                                   'Short Term (Present Value Total)'.

      WHEN 'TPVNET_LT'.
        PERFORM rename_field USING 'Tot.PV.Net LT.'
                                   'Long Term (Present Value Total)'.

      WHEN 'ITRST'.
        PERFORM rename_field USING 'Interest'
                                   'Interest'.

      WHEN 'ITRST_A'.
        PERFORM rename_field USING 'Interest A'
                                   'Interest A'.

      WHEN 'AMORT'.
        PERFORM rename_field USING 'Amortiz.'
                                   'Amortization'.

      WHEN 'TSUPPL_FL'.
        PERFORM rename_field USING 'Tot A/R Full'
                                   'Full A/R (Supplier Total)'.

      WHEN 'TSUPPL_ST'.
        PERFORM rename_field USING 'Tot A/R ST.'
                                   'Short Term A/R (Supplier Total)'.

      WHEN 'TSUPPL_LT'.
        PERFORM rename_field USING 'Tot A/R LT.'
                                   'Long Term A/R (Supplier Total)'.

      WHEN 'TPVSNET'.
        PERFORM rename_field USING 'Tot.PV.A/R'
                                   'Net A/R (Present Value Total)'.

      WHEN 'TPVSNET_ST'.
        PERFORM rename_field USING 'Tot.PV.A/R ST.'
                            'Short Term A/R (Present Value Total)'.

      WHEN 'TPVSNET_LT'.
        PERFORM rename_field USING 'Tot.PV.A/R LT.'
                            'Long Term A/R (Present Value Total)'.

* BEGIN OF VALERIAN
      WHEN 'DESCR'.
        PERFORM rename_field USING 'Description'
                            'Description'.
* END OF VALERIAN

* END OF HIS20094

    ENDCASE.
    MODIFY it_fieldcat.
  ENDLOOP.

  SORT it_alv BY bukrs s_gjahr monat model m_gjahr land1    "HIS20094
                 matnr.                                     "HIS20094

  CALL METHOD alv_grid->set_table_for_first_display
      EXPORTING i_structure_name = 'ZFI_WARRANTY_ALV_STR'
                is_layout        = wa_is_layout
                i_save           = wa_save
                is_variant       = wa_variant
                i_default        = 'X'                      "HIS20094
*               i_default        = space                    "HIS20094
      CHANGING  it_fieldcatalog  = it_fieldcat[]
*                it_sort          = it_sort1[]
                it_outtab        =  it_alv[].

ENDMODULE.                 " display_grid  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  accrual_simulation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM accrual_simulation.
  PERFORM get_selected_rows.
* Call BDC in Simulation Mode
  PERFORM do_simulation.
ENDFORM.                    " accrual_simulation
*&---------------------------------------------------------------------*
*&      Form  Accural_postings
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM accural_postings.
  PERFORM get_selected_rows.
* Call BDC to  Post Warranty Accurals
  PERFORM do_postings.

* Update FLAG & Posting date back in Table.
  PERFORM update_table.
ENDFORM.                    " Accural_postings
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_selected_rows.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.
  REFRESH :out_tab,it_model,it_prod. CLEAR: out_tab,it_model,it_prod.
  CALL METHOD alv_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
  READ TABLE lt_rows INDEX 1.
  IF sy-subrc EQ 0.
    REFRESH out_tab. CLEAR out_tab.
    LOOP AT lt_rows WHERE index NE 0.
      READ TABLE it_alv INDEX lt_rows-index.
      IF sy-subrc EQ 0.

* WHAT is OUT_TAB??? - ANDY
*        MOVE-CORRESPONDING it_alv TO out_tab.
*        MOVE: icon_light_out TO out_tab-icon.
*        APPEND out_tab.

        IF p_vm   = 'X' .
          MOVE-CORRESPONDING it_alv TO it_model.
          COLLECT it_model.
        ELSE.
          MOVE-CORRESPONDING it_alv TO it_prod.
          COLLECT it_prod.
        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDIF.

  CLEAR: out_tab,it_model,it_prod.
ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  do_simulation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_simulation.
  DATA : l_sysdate(10) TYPE c.
  CONCATENATE 'Warranty Estimated' p_month '/' p_year INTO l_text.
  CONCATENATE p_year p_month  '01'    INTO l_date2.
  WRITE  l_date2 TO l_date1  .

  WRITE sy-datum TO l_sysdate MM/DD/YYYY.
* Get Last day of Posting  Month
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = l_date1
       IMPORTING
            last_day_of_month = l_date1
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.

  WRITE l_date1 TO l_date2 MM/DD/YYYY.

* FIRST DAY of Next Month
  l_date = l_date1 + 1.
  WRITE l_date TO l_date3 MM/DD/YYYY.
* IF vehicle Model is choosen
  IF p_vm   = 'X' .
    PERFORM simulate_for_model.
  ELSE.
* If Product is choosen
    PERFORM simulate_for_product.
  ENDIF.


ENDFORM.                    " do_simulation
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0823   text
*      -->P_0824   text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING    program
                         dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.


ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0828   text
*      -->P_0829   text
*----------------------------------------------------------------------*
FORM bdc_field USING    fnam
                        fval.
  IF fval <> ''.
    CLEAR bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    APPEND bdcdata.
  ENDIF.
ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  do_postings
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_postings.


* write sy-datum to l_date MM/DD/YYYY.
  CONCATENATE 'Warranty Estimated' p_month '/' p_year INTO l_text.

  CONCATENATE p_year p_month  '01'    INTO l_date2.
  WRITE  l_date2 TO l_date1.

  WRITE sy-datum TO l_sysdate MM/DD/YYYY.

* Get Last day of Posting  Month
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = l_date1
       IMPORTING
            last_day_of_month = l_date1
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.

  WRITE l_date1 TO l_date2 MM/DD/YYYY.

* FIRST DAY of Next Month
  l_date = l_date1 + 1.
  WRITE l_date TO l_date3 MM/DD/YYYY.

* IF vehicle Model is choosen
  IF p_vm   = 'X' .
    PERFORM posting_by_model.
  ELSE.
    PERFORM posting_by_prod.
  ENDIF.


ENDFORM.                    " do_postings
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  LOOP AT it_alv WHERE belum NE ''.

    SELECT SINGLE * FROM ztfi_war_post
                       WHERE  bukrs = it_alv-bukrs
                         AND  matnr = it_alv-matnr
                         AND  gjahr = p_year
                         AND  monat = p_month
                         AND  s_gjahr = it_alv-s_gjahr
                         AND  s_monat = it_alv-monat
                         AND  m_gjahr = it_alv-m_gjahr.
    IF sy-subrc = 0.
      WRITE:/ 'Already posted; ',
              it_alv-matnr, it_alv-m_gjahr,
              it_alv-s_gjahr, it_alv-monat.
    ELSE.
      MOVE-CORRESPONDING it_alv TO ztfi_war_post.           "VALERIAN

      ztfi_war_post-bukrs   = it_alv-bukrs.

      ztfi_war_post-gjahr = p_year.
      ztfi_war_post-monat = p_month.

      ztfi_war_post-land1   = it_alv-land1.
      ztfi_war_post-model   = it_alv-model.
      ztfi_war_post-m_gjahr = it_alv-m_gjahr.
      ztfi_war_post-s_gjahr = it_alv-s_gjahr.
      ztfi_war_post-s_monat = it_alv-monat.
      ztfi_war_post-matnr   = it_alv-matnr.
      ztfi_war_post-sal_qty = it_alv-sal_qty.
      ztfi_war_post-waramt  = it_alv-wrbtr.
      ztfi_war_post-versn   = it_alv-versn.

      INSERT ztfi_war_post.
      CLEAR ztfi_war_post.                                  "VALERIAN
*    UPDATE ztfi_war_sales SET flag = 'X'
*                              pdate = p_per
*                              unam = sy-uname
*                              udat = sy-datum
*                              versn = it_alv-versn
*                       WHERE  bukrs = it_alv-bukrs AND
*                              land1 = it_alv-land1 AND
*                              model = it_alv-model AND
*                              m_gjahr = it_alv-m_gjahr AND
*                              s_gjahr = it_alv-s_gjahr AND
*                              monat  = it_alv-monat AND
*                              matnr = it_alv-matnr.
    ENDIF.

  ENDLOOP.
  COMMIT WORK.
ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  simulate_for_model
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM simulate_for_model.
  DATA: l_tpvnet_s    TYPE dmbtr,
        l_itrst_a     TYPE dmbtr,
        l_tot_itrst_a TYPE dmbtr,
        l_amt2(15)    TYPE c,
        l_newbs(2)    TYPE c VALUE '40',
        l_newko(10)   TYPE c VALUE '620109'.
*// 2011.08.11 insert by Kim.YN
  data: lv_kunnr      like  kna1-kunnr.

  clear: gv_matnr, gv_prdha.

  IF it_model[] IS INITIAL.
    MESSAGE i208(00) WITH 'Please Make Selection(s)'.
    EXIT.
  ENDIF.

* POSTING DOCUMENT-1
  LOOP AT it_model.
    clear: lv_kunnr.
    l_tpvnet_s = it_model-tpvnet - it_model-itrst_a.
    WRITE l_tpvnet_s TO l_amt2.
    SHIFT l_amt2 LEFT DELETING LEADING space.

* BEGIN OF UD1K953691
*// ==== 2011.08.24 insert by Kim.YN  For ECC6.0.
*    clear: gv_matnr, gv_prdha.
*    gv_matnr = it_model-matnr.
*    lv_kunnr = it_model-matnr+1(5).
*
**   sales order.
*    select single prdha into gv_prdha
*      from mara
*     where matnr = gv_matnr.
*// ==== 2011.08.24 insert end. ================ //*
* END OF UD1K953691


    AT FIRST.
* FBS1 Initial Screen
      PERFORM bdc_dynpro      USING  'SAPMF05A' '0100'.
      PERFORM bdc_field USING : 'BKPF-BLDAT'  l_date2,
                                'BKPF-BLART'  'AC',
                                'BKPF-BUKRS'  'H201',
                                'BKPF-BUDAT'  l_date2,
                                'BKPF-MONAT'   p_month,
                                'BKPF-WAERS'   'USD',
                                'BKPF-XBLNR'   'Warranty Posting',
                                'BKPF-BKTXT'  'Allowance for warranty',
                                'BKPF-STGRD'  '05',
                                'BKPF-STODT'   l_date3,
                                'BDC_CURSOR'  'RF05A-NEWKO',
                                'RF05A-NEWBS' l_newbs,
                                'RF05A-NEWKO' l_newko,
                                'BDC_OKCODE'  '/00'.
    ENDAT.

    AT LAST.
      l_newbs = '50'.
      l_newko = '216800'.
    ENDAT.

* Next Screen - For Other Posting KEY
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                              'BSEG-WRBTR'     l_amt2,
                              'BSEG-SGTXT'   'Allowance for Warranties',
                              'RF05A-NEWBS'   l_newbs,
                              'RF05A-NEWKO'   l_newko,
                              'BDC_OKCODE'    '/00'.

    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : " 'BDC_CURSOR'    'COBL-KOSTL',
                              'COBL-KOSTL'    'SA001',
                              'DKACB-XERGO'   'X',
                              'BDC_OKCODE'    '=ENTE'.

    SELECT SINGLE * FROM ztco_model_map
     WHERE modl1 = it_model-model.

    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
*// ==== 2011.08.11 change by Kim.YN  For ECC6.0.
*                              'RKEAK-FIELD(01)' 'US',
*                              'RKEAK-FIELD(01)' lv_kunnr,
*                              'RKEAK-FIELD(04)' ztco_model_map-paph2,
                              'RKEAK-FIELD(15)' it_model-land1,
                              'BDC_OKCODE'    '=P+'.

    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(02)',
                              'RKEAK-FIELD(02)' ztco_model_map-paph2,
                              'BDC_OKCODE'    '=WEIT'.

    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

  ENDLOOP.

  WRITE v_tpvnet_st TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '50',
                            'RF05A-NEWKO'   '218100',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

* Little Blocking code POP-Up
  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

  WRITE v_tpvnet_lt TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '40',
                            'RF05A-NEWKO'   '712084',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

* Little Blocking code POP-Up
  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

  WRITE v_itrst_a TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '40',
                            'RF05A-NEWKO'   '126055',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : " 'BDC_CURSOR'    'COBL-KOSTL',
                            'COBL-KOSTL'     'SA001',
                            'BDC_OKCODE'    '=ENTE'.

  WRITE v_tpvsnet_st TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '40',
                            'RF05A-NEWKO'   '193040',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : " 'BDC_CURSOR'    'COBL-KOSTL',
*// 2011.08.24 insert
***                            'COBL-KOSTL'     'SA001',
                            'BDC_OKCODE'    '=ENTE'.

  WRITE v_tpvsnet_lt TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '50',
                            'RF05A-NEWKO'   '216800',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : "'BDC_CURSOR'    'COBL-AUFNR',
                            'BDC_OKCODE'    '=ENTE'.

  WRITE v_tpvsnet_st TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '50',
                            'RF05A-NEWKO'   '218100',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

* Call Simulation Mode Screen
  WRITE v_tpvsnet_lt TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR' 'BSEG-WRBTR',
                            'BSEG-WRBTR'  l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '=BS'.

  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

  PERFORM bdc_dynpro      USING  'SAPMF05A' '0700'.
  PERFORM bdc_field USING : 'BDC_CURSOR' 'RF05A-NEWBS',
                            'BDC_OKCODE'    '=RW'.

  PERFORM bdc_dynpro      USING  'SAPLSPO1' '0200'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=YES'.

* CALL FBS1 in Simulation Mode
  CALL TRANSACTION 'FBS1' USING bdcdata
                    MODE l_mode MESSAGES INTO messtab.

  IF sy-subrc NE 0.
    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
         TABLES
              imt_bdcmsgcoll = messtab
              ext_return     = lt_return.
    LOOP AT lt_return .
      it_alv-msg = lt_return-message.
    ENDLOOP.
    it_alv-icon = icon_red_light.
    MODIFY it_alv TRANSPORTING msg icon WHERE
                                        model NE space AND
                                        land1 NE space.
    EXIT.
  ENDIF.

  REFRESH: messtab, bdcdata, lt_return.
  CLEAR:   lt_return, messtab, bdcdata.

* POSTING DOCUMENT-2
  l_newbs = '50'.
  l_newko = '620109'.

  LOOP AT it_model.
    clear: lv_kunnr.
    l_itrst_a = it_model-itrst_a.
    WRITE l_itrst_a TO l_amt2.
    SHIFT l_amt2 LEFT DELETING LEADING space.

*   lv_kunnr = it_model-matnr+1(5).                         "UD1K953691

    l_tot_itrst_a = l_tot_itrst_a + it_model-itrst_a.

    AT FIRST.
* FBS1 Initial Screen
      PERFORM bdc_dynpro      USING  'SAPMF05A' '0100'.
      PERFORM bdc_field USING : 'BKPF-BLDAT'  l_date2,
                                'BKPF-BLART'  'AC',
                                'BKPF-BUKRS'  'H201',
                                'BKPF-BUDAT'  l_date2,
                                'BKPF-MONAT'   p_month,
                                'BKPF-WAERS'   'USD',
                                'BKPF-XBLNR'   'Warranty Posting',
                                'BKPF-BKTXT'  'Allowance for warranty',
                                'BKPF-STGRD'  '05',
                                'BKPF-STODT'   l_date3,
                                'BDC_CURSOR'  'RF05A-NEWKO',
                                'RF05A-NEWBS' l_newbs,
                                'RF05A-NEWKO' l_newko,
                                'BDC_OKCODE'  '/00'.
    ENDAT.

    AT LAST.
      l_newbs = '40'.
      l_newko = '712084'.
    ENDAT.

* Next Screen - For Other Posting KEY
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                              'BSEG-WRBTR'     l_amt2,
                              'BSEG-SGTXT'   'Allowance for Warranties',
                              'RF05A-NEWBS'   l_newbs,
                              'RF05A-NEWKO'   l_newko,
                              'BDC_OKCODE'    '/00'.

    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'COBL-KOSTL',
                              'COBL-KOSTL'    'SA001',
                              'DKACB-XERGO'   'X',
                              'BDC_OKCODE'    '=ENTE'.

    SELECT SINGLE * FROM ztco_model_map
    WHERE modl1 = it_model-model.
    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
*// ==== 2011.08.11 change by Kim.YN  For ECC6.0.
*                              'RKEAK-FIELD(01)' 'US',
*                              'RKEAK-FIELD(01)' lv_kunnr,
*                              'RKEAK-FIELD(04)' ztco_model_map-paph2,
                              'RKEAK-FIELD(15)' it_model-land1,
                              'BDC_OKCODE'    '=P+'.

    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(02)',
                              'RKEAK-FIELD(02)' ztco_model_map-paph2,
                              'BDC_OKCODE'    '=WEIT'.

    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

  ENDLOOP.

  WRITE l_tot_itrst_a TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '=BS'.

* Little Blocking code POP-Up
  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : " 'BDC_CURSOR'    'COBL-KOSTL',
                            'COBL-KOSTL'     'SA001',
                            'BDC_OKCODE'    '=ENTE'.

  PERFORM bdc_dynpro      USING  'SAPMF05A' '0700'.
  PERFORM bdc_field USING : 'BDC_CURSOR' 'RF05A-NEWBS',
                            'BDC_OKCODE'    '=RW'.

  PERFORM bdc_dynpro      USING  'SAPLSPO1' '0200'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=YES'.

* CALL FBS1 in Simulation Mode
  CALL TRANSACTION 'FBS1' USING bdcdata
                    MODE l_mode MESSAGES INTO messtab.
  IF sy-subrc EQ 0.
    it_alv-msg = 'Simulation Successfull'.
    it_alv-icon = icon_green_light.
    MODIFY it_alv TRANSPORTING msg icon WHERE
                                        model NE space AND
                                        land1 NE space.
  ELSE.
    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
         TABLES
              imt_bdcmsgcoll = messtab
              ext_return     = lt_return.
    LOOP AT lt_return .
      it_alv-msg = lt_return-message.
    ENDLOOP.
    it_alv-icon = icon_red_light.
    MODIFY it_alv TRANSPORTING msg icon WHERE
                                        model NE space AND
                                        land1 NE space.
  ENDIF.
  REFRESH: messtab, bdcdata, lt_return.
  CLEAR:   lt_return, messtab, bdcdata.

ENDFORM.                    " simulate_for_model

* BEGIN OF HIS20094 - Logic is changed, need new BDC recording
**&---------------------------------------------------------------------
**
**&      Form  simulate_for_model
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM simulate_for_model.
*  LOOP AT it_model.
*    WRITE it_model-wrbtr TO l_amt.
*    SHIFT l_amt LEFT DELETING LEADING space.
** FBS1 Initial Screen
*    PERFORM bdc_dynpro      USING  'SAPMF05A' '0100'.
*    PERFORM bdc_field USING : 'BKPF-BLDAT'  l_date2,
*                              'BKPF-BLART'  'AC',
*                              'BKPF-BUKRS'  'H201',
*                              'BKPF-BUDAT'  l_date2,
*                              'BKPF-MONAT'   p_month,
*                              'BKPF-WAERS'   'USD',
*                              'BKPF-XBLNR'   'Warranty Posting',
*                              'BKPF-BKTXT'  'Allowance for warranty',
*                              'BKPF-STGRD'  '05',
*                              'BKPF-STODT'   l_date3,
*                              'BDC_CURSOR'  'RF05A-NEWKO',
*                              'RF05A-NEWBS' '50',
*                              'RF05A-NEWKO' '218100',
*                              'BDC_OKCODE'  '/00'.
** Next Screen - For Other Posting KEY
*    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
*    PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
*                              'BSEG-WRBTR'     l_amt,
*                              'BSEG-SGTXT'   'Allowance for Warranties'
*,
*                              'RF05A-NEWBS'   '40',
*                              'RF05A-NEWKO'   '620109',
*                              'DKACB-FMORE'   'X',
*                              'BDC_OKCODE'    '/00'.
** Little Blocking code POP-Up
*    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
*    PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.
*
** Screen to Enter Warranty TEXT
*    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
*    PERFORM bdc_field USING : 'BDC_CURSOR' 'BSEG-SGTXT',
*                              'BSEG-SGTXT'  l_text,
*                              'BSEG-WRBTR'   l_amt,
*                              'BDC_OKCODE'   '/00'.
*
** COST Center POP Screen.
*    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
*    PERFORM bdc_field USING : 'BDC_CURSOR'    'COBL-KOSTL',
*                              'COBL-KOSTL'    'SA001',
*                              'BDC_OKCODE'    '=ENTE'.
*
*
** Vehicle  Model / Product Option is choosen
*
*    SELECT SINGLE * FROM ztco_model_map
*    WHERE modl1 = it_model-model.
*    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
*    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
*                              'RKEAK-FIELD(01)' 'US',
*                              'RKEAK-FIELD(04)' ztco_model_map-paph2,
*                              'BDC_OKCODE'    '=WEIT'.
** Call Simulation Mode Screen
*    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
*    PERFORM bdc_field USING : 'BDC_CURSOR' 'BSEG-WRBTR',
*                              'BSEG-WRBTR'  l_amt,
*                              'BSEG-SGTXT'  l_text,
*                              'BDC_OKCODE'    '=BS'.
*
** Cost center Pop up Screen
*    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
*    PERFORM bdc_field USING : 'BDC_CURSOR' 'COBL-KOSTL',
*                              'COBL-KOSTL'  'SA001',
*                              'BDC_OKCODE'    '=ENTE'.
*
** After Simulation - Exit Screen
*    PERFORM bdc_dynpro      USING  'SAPMF05A' '0700'.
*    PERFORM bdc_field USING : 'BDC_CURSOR' 'RF05A-NEWBS',
*                              'BDC_OKCODE'    '=RW'.
*
*    PERFORM bdc_dynpro      USING  'SAPLSPO1' '0200'.
*    PERFORM bdc_field USING 'BDC_OKCODE'    '=YES'.
*
** CALL FBS1 in Simulation Mode
*    CALL TRANSACTION 'FBS1' USING bdcdata
*                      MODE l_mode MESSAGES INTO messtab.
*    IF sy-subrc EQ 0.
*      it_alv-msg = 'Simulation Successfull'.
*      it_alv-icon = icon_green_light.
*      MODIFY it_alv TRANSPORTING msg icon WHERE
*                                        model EQ it_model-model and
*                                        land1 eq it_model-land1.
**                                       BUKRS eq out_tab-bukrs and
**                                       LAND1 eq out_tab-land1 and
**                                       model eq out_tab-model and
**                                       M_GJAHR eq out_tab-M_GJAHR and
**                                       S_GJAHR eq out_tab-s_gjahr and
**                                       MONAT   eq out_tab-monat and
**                                       matnr   eq out_tab-matnr.
*    ELSE.
*      CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
*           TABLES
*                imt_bdcmsgcoll = messtab
*                ext_return     = lt_return.
*      LOOP AT lt_return .
*        it_alv-msg = lt_return-message.
*      ENDLOOP.
*      it_alv-icon = icon_red_light.
*      MODIFY it_alv TRANSPORTING msg icon WHERE
*                                      model EQ it_model-model and
*                                      land1 EQ it_model-land1.
**                                     BUKRS eq out_tab-bukrs and
**                                     LAND1 eq out_tab-land1 and
**                                     model eq out_tab-model and
**                                     M_GJAHR eq out_tab-M_GJAHR and
**                                     S_GJAHR eq out_tab-s_gjahr and
**                                     MONAT   eq out_tab-monat and
**                                     matnr   eq out_tab-matnr.
*
*    ENDIF.
*    REFRESH:  messtab,bdcdata,lt_return. CLEAR: lt_return,messtab,
*bdcdata.
*  ENDLOOP.
*
*ENDFORM.                    " simulate_for_model
* END OF HIS20094

*&---------------------------------------------------------------------*
*&      Form  simulate_for_product
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM simulate_for_product.
  LOOP AT it_prod.
    CLEAR l_amt.
    WRITE it_prod-wrbtr TO l_amt.
    SHIFT l_amt LEFT DELETING LEADING space.
* FBS1 Initial Screen
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0100'.
    PERFORM bdc_field USING : 'BKPF-BLDAT'  l_date2,
                              'BKPF-BLART'  'AC',
                              'BKPF-BUKRS'  'H201',
                              'BKPF-BUDAT'  l_date2,
                              'BKPF-MONAT'   p_month,
                              'BKPF-WAERS'   'USD',
                              'BKPF-XBLNR'   'Warranty Posting',
                              'BKPF-BKTXT'  'Allowance for warranty',
                              'BKPF-STGRD'  '05',
                              'BKPF-STODT'   l_date3,
                              'BDC_CURSOR'  'RF05A-NEWKO',
                              'RF05A-NEWBS' '50',
                              'RF05A-NEWKO' '218100',
                              'BDC_OKCODE'  '/00'.
* Next Screen - For Other Posting KEY
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                              'BSEG-WRBTR'     l_amt,
                              'BSEG-SGTXT'   'Allowance for Warranties',
                              'RF05A-NEWBS'   '40',
                              'RF05A-NEWKO'   '620109',
                              'DKACB-FMORE'   'X',
                              'BDC_OKCODE'    '/00'.
* Little Blocking code POP-Up
    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

* Screen to Enter Warranty TEXT
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'BSEG-SGTXT',
                              'BSEG-SGTXT'  l_text,
                              'BSEG-WRBTR'   l_amt,
                              'BDC_OKCODE'   '/00'.

* COST Center POP Screen.
    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'COBL-KOSTL',
                              'COBL-KOSTL'    'SA001',
                              'BDC_OKCODE'    '=ENTE'.


*  Product Option is choosen

    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
                              'RKEAK-FIELD(01)' 'US',
                              'RKEAK-FIELD(05)' it_prod-matnr,
                              'BDC_OKCODE'    '=WEIT'.


* Call Simulation Mode Screen
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'BSEG-WRBTR',
                              'BSEG-WRBTR'  l_amt,
                              'BSEG-SGTXT'  l_text,
                              'BDC_OKCODE'    '=BS'.

* Cost center Pop up Screen
    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'COBL-KOSTL',
                              'COBL-KOSTL'  'SA001',
                              'BDC_OKCODE'    '=ENTE'.

* After Simulation - Exit Screen
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0700'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RF05A-NEWBS',
                              'BDC_OKCODE'    '=RW'.

    PERFORM bdc_dynpro      USING  'SAPLSPO1' '0200'.
    PERFORM bdc_field USING 'BDC_OKCODE'    '=YES'.

* CALL FBS1 in Simulation Mode
    CALL TRANSACTION 'FBS1' USING bdcdata
                      MODE l_mode MESSAGES INTO messtab.
    IF sy-subrc EQ 0.
      it_alv-msg = 'Simulation Successfull'.
      it_alv-icon = icon_green_light.
      MODIFY it_alv TRANSPORTING msg icon WHERE
                                        matnr   EQ it_prod-matnr.
*                                       BUKRS eq out_tab-bukrs and
*                                       LAND1 eq out_tab-land1 and
*                                       model eq out_tab-model and
*                                       M_GJAHR eq out_tab-M_GJAHR and
*                                       S_GJAHR eq out_tab-s_gjahr and
*                                       MONAT   eq out_tab-monat and
*                                       matnr   eq out_tab-matnr.
    ELSE.
      CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
           TABLES
                imt_bdcmsgcoll = messtab
                ext_return     = lt_return.
      LOOP AT lt_return WHERE type EQ 'E' .
        it_alv-msg = lt_return-message.
      ENDLOOP.
      it_alv-icon = icon_red_light.
      MODIFY it_alv TRANSPORTING msg icon WHERE
                                        matnr   EQ it_prod-matnr.
*                                     BUKRS eq out_tab-bukrs and
*                                     LAND1 eq out_tab-land1 and
*                                     model eq out_tab-model and
*                                     M_GJAHR eq out_tab-M_GJAHR and
*                                     S_GJAHR eq out_tab-s_gjahr and
*                                     MONAT   eq out_tab-monat and
*                                     matnr   eq out_tab-matnr.

    ENDIF.
    REFRESH: lt_return, messtab,bdcdata. CLEAR:lt_return, messtab,
bdcdata.
  ENDLOOP.
ENDFORM.                    " simulate_for_product
*&---------------------------------------------------------------------*
*&      Form  posting_by_model
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_by_model.
  DATA: l_tpvnet_s    TYPE dmbtr,
        l_itrst_a     TYPE dmbtr,
        l_tot_itrst_a TYPE dmbtr,
        l_amt2(15)    TYPE c,
        l_newbs(2)    TYPE c VALUE '40',
        l_newko(10)   TYPE c VALUE '620109',
        l_belnr       TYPE belnr_d.

  IF it_model[] IS INITIAL.
    MESSAGE i208(00) WITH 'Please Make Selection(s)'.
    EXIT.
  ENDIF.

* POSTING DOCUMENT-1
  LOOP AT it_model.
    l_tpvnet_s = it_model-tpvnet - it_model-itrst_a.
    WRITE l_tpvnet_s TO l_amt2.
    SHIFT l_amt2 LEFT DELETING LEADING space.

    AT FIRST.
* FBS1 Initial Screen
      PERFORM bdc_dynpro      USING  'SAPMF05A' '0100'.
      PERFORM bdc_field USING : 'BKPF-BLDAT'  l_date2,
                                'BKPF-BLART'  'AC',
                                'BKPF-BUKRS'  'H201',
                                'BKPF-BUDAT'  l_date2,
                                'BKPF-MONAT'   p_month,
                                'BKPF-WAERS'   'USD',
                                'BKPF-XBLNR'   'Warranty Posting',
                                'BKPF-BKTXT'  'Allowance for warranty',
                                'BKPF-STGRD'  '05',
                                'BKPF-STODT'   l_date3,
                                'BDC_CURSOR'  'RF05A-NEWKO',
                                'RF05A-NEWBS' l_newbs,
                                'RF05A-NEWKO' l_newko,
                                'BDC_OKCODE'  '/00'.
    ENDAT.

    AT LAST.
      l_newbs = '50'.
      l_newko = '216800'.
    ENDAT.

* Next Screen - For Other Posting KEY
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                              'BSEG-WRBTR'     l_amt2,
                              'BSEG-SGTXT'   'Allowance for Warranties',
                              'RF05A-NEWBS'   l_newbs,
                              'RF05A-NEWKO'   l_newko,
                              'BDC_OKCODE'    '/00'.

    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'COBL-KOSTL',
                              'COBL-KOSTL'    'SA001',
                              'DKACB-XERGO'   'X',
                              'BDC_OKCODE'    '=ENTE'.

    clear: ztco_model_map.
    SELECT SINGLE * FROM ztco_model_map
    WHERE modl1 = it_model-model.

    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
*// ==== 2011.08.11 change by Kim.YN  For ECC6.0.
***    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
***                              'RKEAK-FIELD(01)' 'US',
***                              'RKEAK-FIELD(04)' ztco_model_map-paph2,
***                              'BDC_OKCODE'    '=WEIT'.

    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
*                              'RKEAK-FIELD(01)' 'US',
*                              'RKEAK-FIELD(01)' lv_kunnr,
*                              'RKEAK-FIELD(04)' ztco_model_map-paph2,
                              'RKEAK-FIELD(15)' it_model-land1,
                              'BDC_OKCODE'    '=P+'.

    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(02)',
                              'RKEAK-FIELD(02)' ztco_model_map-paph2,
                              'BDC_OKCODE'    '=WEIT'.

    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.


  ENDLOOP.

  WRITE v_tpvnet_st TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '50',
                            'RF05A-NEWKO'   '218100',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

* Little Blocking code POP-Up
  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

  WRITE v_tpvnet_lt TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '40',
                            'RF05A-NEWKO'   '712084',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

* Little Blocking code POP-Up
  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

  WRITE v_itrst_a TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '40',
                            'RF05A-NEWKO'   '126055',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'COBL-KOSTL',
                            'COBL-KOSTL'     'SA001',
                            'BDC_OKCODE'    '=ENTE'.

  WRITE v_tpvsnet_st TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '40',
                            'RF05A-NEWKO'   '193040',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

  WRITE v_tpvsnet_lt TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '50',
                            'RF05A-NEWKO'   '216800',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'COBL-AUFNR',
                            'BDC_OKCODE'    '=ENTE'.

  WRITE v_tpvsnet_st TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'RF05A-NEWBS'   '50',
                            'RF05A-NEWKO'   '218100',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '/00'.

  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

* Call Posting Mode Screen
  WRITE v_tpvsnet_lt TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR' 'BSEG-WRBTR',
                            'BSEG-WRBTR'  l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '=BU'.

  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

* CALL FBS1 in Simulation Mode
  CALL TRANSACTION 'FBS1' USING bdcdata
                    MODE l_mode MESSAGES INTO messtab.


  IF sy-subrc EQ 0.
    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
         TABLES
              imt_bdcmsgcoll = messtab
              ext_return     = lt_return.
    LOOP AT lt_return WHERE type EQ 'S'.
      it_alv-msg = lt_return-message.
      it_alv-belum = lt_return-message_v1.
    ENDLOOP.
    l_belnr     = it_alv-belum.
    it_alv-icon = icon_green_light.
    MODIFY it_alv TRANSPORTING msg icon belum WHERE model NE space
                                                AND land1 NE space.
  ELSE.
    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
         TABLES
              imt_bdcmsgcoll = messtab
              ext_return     = lt_return.
    LOOP AT lt_return WHERE type EQ 'E' .
      it_alv-msg = lt_return-message.
    ENDLOOP.
    it_alv-icon = icon_red_light.
    MODIFY it_alv TRANSPORTING msg icon WHERE model NE space
                                          AND land1 NE space.
    EXIT.
  ENDIF.

  REFRESH: messtab, bdcdata, lt_return.
  CLEAR:   lt_return, messtab, bdcdata.

* POSTING DOCUMENT-2
  l_newbs = '50'.
  l_newko = '620109'.

  LOOP AT it_model.
    l_itrst_a = it_model-itrst_a.
    WRITE l_itrst_a TO l_amt2.
    SHIFT l_amt2 LEFT DELETING LEADING space.

    l_tot_itrst_a = l_tot_itrst_a + it_model-itrst_a.

    AT FIRST.
* FBS1 Initial Screen
      PERFORM bdc_dynpro      USING  'SAPMF05A' '0100'.
      PERFORM bdc_field USING : 'BKPF-BLDAT'  l_date2,
                                'BKPF-BLART'  'SA',         "HIS20094
*                               'BKPF-BLART'  'AC',         "HIS20094
                                'BKPF-BUKRS'  'H201',
                                'BKPF-BUDAT'  l_date2,
                                'BKPF-MONAT'   p_month,
                                'BKPF-WAERS'   'USD',
                                'BKPF-XBLNR'   'Warranty Posting',
                                'BKPF-BKTXT'  'Allowance for warranty',
                                'BKPF-STGRD'  '05',
                                'BKPF-STODT'   l_date3,
                                'BDC_CURSOR'  'RF05A-NEWKO',
                                'RF05A-NEWBS' l_newbs,
                                'RF05A-NEWKO' l_newko,
                                'BDC_OKCODE'  '/00'.
    ENDAT.

    AT LAST.
      l_newbs = '40'.
      l_newko = '712084'.
    ENDAT.

* Next Screen - For Other Posting KEY
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                              'BSEG-WRBTR'     l_amt2,
                              'BSEG-SGTXT'   'Allowance for Warranties',
                              'RF05A-NEWBS'   l_newbs,
                              'RF05A-NEWKO'   l_newko,
                              'BDC_OKCODE'    '/00'.

    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'COBL-KOSTL',
                              'COBL-KOSTL'    'SA001',
                              'DKACB-XERGO'   'X',
                              'BDC_OKCODE'    '=ENTE'.

    SELECT SINGLE * FROM ztco_model_map
    WHERE modl1 = it_model-model.
    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
***    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
***                              'RKEAK-FIELD(01)' 'US',
***                              'RKEAK-FIELD(04)' ztco_model_map-paph2,
***                              'BDC_OKCODE'    '=WEIT'.

*// ==== 2011.08.11 change by Kim.YN  For ECC6.0.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
*                              'RKEAK-FIELD(01)' 'US',
*                              'RKEAK-FIELD(01)' lv_kunnr,
*                              'RKEAK-FIELD(04)' ztco_model_map-paph2,
                              'RKEAK-FIELD(15)' it_model-land1,
                              'BDC_OKCODE'    '=P+'.

    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(02)',
                              'RKEAK-FIELD(02)' ztco_model_map-paph2,
                              'BDC_OKCODE'    '=WEIT'.

    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.


  ENDLOOP.

  WRITE l_tot_itrst_a TO l_amt2.
  SHIFT l_amt2 LEFT DELETING LEADING space.
  PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                            'BSEG-WRBTR'     l_amt2,
                            'BSEG-SGTXT'   'Allowance for Warranties',
                            'DKACB-FMORE'   'X',
                            'BDC_OKCODE'    '=BU'.

* Little Blocking code POP-Up
  PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
  PERFORM bdc_field USING : 'BDC_CURSOR'    'COBL-KOSTL',
                            'COBL-KOSTL'     'SA001',
                            'BDC_OKCODE'    '=ENTE'.

* CALL FBS1 in Simulation Mode
  CALL TRANSACTION 'FBS1' USING bdcdata
                    MODE l_mode MESSAGES INTO messtab.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
         TABLES
              imt_bdcmsgcoll = messtab
              ext_return     = lt_return.
    LOOP AT lt_return WHERE type EQ 'S'.
      it_alv-msg = lt_return-message.
      it_alv-belum = lt_return-message_v1.
    ENDLOOP.

    CONCATENATE l_belnr it_alv-belum INTO it_alv-belum
      SEPARATED BY ' / '.
    it_alv-icon = icon_green_light.
    MODIFY it_alv TRANSPORTING msg icon belum WHERE model NE space
                                                AND land1 NE space.
  ELSE.
    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
         TABLES
              imt_bdcmsgcoll = messtab
              ext_return     = lt_return.
    LOOP AT lt_return WHERE type EQ 'E' .
      it_alv-msg = lt_return-message.
    ENDLOOP.
    it_alv-icon = icon_red_light.
    MODIFY it_alv TRANSPORTING msg icon WHERE model NE space
                                          AND land1 NE space.
  ENDIF.

  REFRESH: bdcdata, messtab,lt_return.
  CLEAR:   bdcdata, lt_return, messtab.

ENDFORM.                    " posting_by_model

* BEGIN OF HIS20094 - Logic is changed, need new BDC recording
**&---------------------------------------------------------------------
**
**&      Form  posting_by_model
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM posting_by_model.
*
*  LOOP AT it_model.
*    CLEAR l_amt.
*    WRITE it_model-wrbtr TO l_amt.
*    SHIFT l_amt LEFT DELETING LEADING space.
** FBS1 Initial Screen
*    PERFORM bdc_dynpro      USING  'SAPMF05A' '0100'.
*    PERFORM bdc_field USING : 'BKPF-BLDAT'  l_sysdate ,
*                              'BKPF-BLART'  'AC',
*                              'BKPF-BUKRS'  'H201',
*                              'BKPF-BUDAT'  l_date2 ,
*                              'BKPF-MONAT'   p_month,
*                              'BKPF-WAERS'   'USD',
*                              'BKPF-XBLNR'   'Warranty Posting',
*                              'BKPF-BKTXT'  'Allowance for warranty',
*                              'BKPF-STGRD'  '05',
*                              'BKPF-STODT'   l_date3,
*                              'BDC_CURSOR'  'RF05A-NEWKO',
*                              'RF05A-NEWBS' '50',
*                              'RF05A-NEWKO' '218100',
*                              'BDC_OKCODE'  '/00'.
** Next Screen
*    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
*    PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
*                              'BSEG-WRBTR'     l_amt,
*                              'BSEG-SGTXT'   'Allowance for Warranties'
*,
*                              'RF05A-NEWBS'   '40',
*                              'RF05A-NEWKO'   '620109',
*                              'DKACB-FMORE'   'X',
*                              'BDC_OKCODE'    '/00'.
** Little Blocking code POP-Up
*    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
*    PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.
*
** Screen to Other Posting Key
*    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
*    PERFORM bdc_field USING : 'BDC_CURSOR' 'BSEG-SGTXT',
*                              'BSEG-SGTXT'  l_text,
*                              'BSEG-WRBTR'   l_amt,
*                              'BDC_OKCODE'   '/00'.
*
** COST Center POP up Screen.
*    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
*    PERFORM bdc_field USING : 'BDC_CURSOR'    'COBL-KOSTL',
*                              'COBL-KOSTL'    'SA001',
*                              'BDC_OKCODE'    '=ENTE'.
*
*
** Vehicle  Model / Product
**    if p_VM   = 'X' .
*    SELECT SINGLE * FROM ztco_model_map WHERE modl1 = it_model-model.
*    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
*    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
*                              'RKEAK-FIELD(01)'  it_model-land1,
*                              'RKEAK-FIELD(04)'  ztco_model_map-paph2,
*                              'BDC_OKCODE'    '=WEIT'.
**    else.
**      perform BDC_DYNPRO      using  'SAPLKEAK' '0300'.
**      perform BDC_FIELD using : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
**                                'RKEAK-FIELD(01)' out_tab-land1,
**                                'RKEAK-FIELD(05)'  out_tab-matnr,
**                                'BDC_OKCODE'    '=WEIT'.
**    endif.
** Back to Previous Screen
*    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
*    PERFORM bdc_field USING : 'BDC_CURSOR' 'BSEG-WRBTR',
*                              'BSEG-WRBTR'  l_amt,
*                              'BSEG-SGTXT'  l_text,
*                              'BDC_OKCODE'    '=BU'.
*
** Cost center Pop up Screen
*    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
*    PERFORM bdc_field USING : 'BDC_CURSOR' 'COBL-KOSTL',
*                              'COBL-KOSTL'  'SA001',
*                              'BDC_OKCODE'    '=ENTE'.
*
** CALL FBS1 to POST Warranty Accural
*
*    CALL TRANSACTION 'FBS1' USING bdcdata
*                      MODE l_mode MESSAGES INTO messtab.
*
*    IF sy-subrc EQ 0.
*      CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
*           TABLES
*                imt_bdcmsgcoll = messtab
*                ext_return     = lt_return.
*      LOOP AT lt_return WHERE type EQ 'S'.
*        it_alv-msg = lt_return-message.
*        it_alv-belum = lt_return-message_v1.
*      ENDLOOP.
*      it_alv-icon = icon_green_light.
*      MODIFY it_alv TRANSPORTING msg icon belum
*                    WHERE model EQ it_model-model
*                      AND land1 EQ it_model-land1.
*    ELSE.
*      CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
*           TABLES
*                imt_bdcmsgcoll = messtab
*                ext_return     = lt_return.
*      LOOP AT lt_return WHERE type EQ 'E' .
*        it_alv-msg = lt_return-message.
*      ENDLOOP.
*      it_alv-icon = icon_red_light.
*      MODIFY it_alv TRANSPORTING msg icon
*                    WHERE model EQ it_model-model
*                      AND land1 EQ it_model-land1.
*    ENDIF.
*    REFRESH: bdcdata, messtab,lt_return.
*    CLEAR:bdcdata,lt_return,messtab.
*  ENDLOOP.
*
*ENDFORM.                    " posting_by_model
* END OF HIS20094

*&---------------------------------------------------------------------*
*&      Form  posting_by_prod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_by_prod.
  LOOP AT it_prod.
    WRITE it_prod-wrbtr TO l_amt.
    SHIFT l_amt LEFT DELETING LEADING space.
* FBS1 Initial Screen
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0100'.
    PERFORM bdc_field USING : 'BKPF-BLDAT'  l_sysdate,
                              'BKPF-BLART'  'AC',
                              'BKPF-BUKRS'  'H201',
                              'BKPF-BUDAT'  l_date2 ,
                              'BKPF-MONAT'   p_month,
                              'BKPF-WAERS'   'USD',
                              'BKPF-XBLNR'   'Warranty Posting',
                              'BKPF-BKTXT'  'Allowance for warranty',
                              'BKPF-STGRD'  '05',
                              'BKPF-STODT'   l_date3,
                              'BDC_CURSOR'  'RF05A-NEWKO',
                              'RF05A-NEWBS' '50',
                              'RF05A-NEWKO' '218100',
                              'BDC_OKCODE'  '/00'.
* Next Screen
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'RF05A-NEWKO',
                              'BSEG-WRBTR'     l_amt,
                              'BSEG-SGTXT'   'Allowance for Warranties',
                              'RF05A-NEWBS'   '40',
                              'RF05A-NEWKO'   '620109',
                              'DKACB-FMORE'   'X',
                              'BDC_OKCODE'    '/00'.
* Little Blocking code POP-Up
    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.

* Screen to Other Posting Key
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'BSEG-SGTXT',
                              'BSEG-SGTXT'  l_text,
                              'BSEG-WRBTR'   l_amt,
                              'BDC_OKCODE'   '/00'.

* COST Center POP up Screen.
    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_CURSOR'    'COBL-KOSTL',
                              'COBL-KOSTL'    'SA001',
                              'BDC_OKCODE'    '=ENTE'.


* Vehicle  Model / Product
*    if p_VM   = 'X' .
*      select single * from ZTCO_MODEL_MAP where MODL1 = out_tab-model.
*
*      perform BDC_DYNPRO      using  'SAPLKEAK' '0300'.
*      perform BDC_FIELD using : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
*                                'RKEAK-FIELD(01)' out_tab-land1,
*                                'RKEAK-FIELD(04)'  ZTCO_MODEL_MAP-PAPH2
*,
*                                'BDC_OKCODE'    '=WEIT'.
    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
***    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
****                             'RKEAK-FIELD(01)' 'US',
***                              'RKEAK-FIELD(05)'  it_prod-matnr,
***                              'BDC_OKCODE'    '=WEIT'.

*// ==== 2011.08.11 change by Kim.YN  For ECC6.0.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(04)',
*                              'RKEAK-FIELD(01)' 'US',
*                              'RKEAK-FIELD(01)' lv_kunnr,
*                              'RKEAK-FIELD(04)' ztco_model_map-paph2,
                              'RKEAK-FIELD(15)' it_model-land1,
                              'BDC_OKCODE'    '=P+'.

    PERFORM bdc_dynpro      USING  'SAPLKEAK' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'RKEAK-FIELD(02)',
                              'RKEAK-FIELD(02)' ztco_model_map-paph2,
                              'BDC_OKCODE'    '=WEIT'.

    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_OKCODE'    '=ENTE'.


* Back to Previous Screen
    PERFORM bdc_dynpro      USING  'SAPMF05A' '0300'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'BSEG-WRBTR',
                              'BSEG-WRBTR'  l_amt,
                              'BSEG-SGTXT'  l_text,
                              'BDC_OKCODE'    '=BU'.

* Cost center Pop up Screen
    PERFORM bdc_dynpro      USING  'SAPLKACB' '0002'.
    PERFORM bdc_field USING : 'BDC_CURSOR' 'COBL-KOSTL',
                              'COBL-KOSTL'  'SA001',
                              'BDC_OKCODE'    '=ENTE'.

* CALL FBS1 to POST Warranty Accural

    CALL TRANSACTION 'FBS1' USING bdcdata
                      MODE l_mode MESSAGES INTO messtab.

    IF sy-subrc EQ 0.
      CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
           TABLES
                imt_bdcmsgcoll = messtab
                ext_return     = lt_return.
      LOOP AT lt_return WHERE type EQ 'S'.
        it_alv-msg = lt_return-message.
        it_alv-belum = lt_return-message_v1.
      ENDLOOP.
      it_alv-icon = icon_green_light.
      MODIFY it_alv TRANSPORTING msg icon belum WHERE
                                       matnr   EQ it_prod-matnr.
*                                      BUKRS eq out_tab-bukrs and
*                                      LAND1 eq out_tab-land1 and
*                                      model eq out_tab-model and
*                                      M_GJAHR eq out_tab-M_GJAHR and
*                                      S_GJAHR eq out_tab-s_gjahr and
*                                      MONAT   eq out_tab-monat and
*                                      matnr   eq out_tab-matnr.
    ELSE.
      CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
           TABLES
                imt_bdcmsgcoll = messtab
                ext_return     = lt_return.
      LOOP AT lt_return WHERE type EQ 'E' .
        it_alv-msg = lt_return-message.
      ENDLOOP.
      it_alv-icon = icon_red_light.
      MODIFY it_alv TRANSPORTING msg icon WHERE
                                      matnr   EQ it_prod-matnr.
*                                     BUKRS eq out_tab-bukrs and
*                                     LAND1 eq out_tab-land1 and
*                                     model eq out_tab-model and
*                                     M_GJAHR eq out_tab-M_GJAHR and
*                                     S_GJAHR eq out_tab-s_gjahr and
*                                     MONAT   eq out_tab-monat and
*                                     matnr   eq out_tab-matnr.
    ENDIF.
    REFRESH :bdcdata, messtab,lt_return. CLEAR:bdcdata,lt_return,
messtab.
  ENDLOOP.
ENDFORM.                    " do_postings
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM update_table.
*  loop at it_alv where belum ne ''.
*
*    update ZTFI_WAR_SALES set flag = 'X'
*                              pdate = p_per
*                              UNAM = sy-uname
*                              udat = sy-datum
*                         where  BUKRS = it_alv-bukrs and
*                                LAND1 = it_alv-land1 and
*                                model = it_alv-model and
*                                M_GJAHR = it_alv-M_GJAHR and
*                                S_GJAHR = it_alv-s_gjahr and
*                                MONAT  = it_alv-monat and
*                                MATNR = it_alv-matnr.
*
*  endloop.
*
*ENDFORM.                    " posting_by_prod
*&---------------------------------------------------------------------*
*&      Form  GET_KEYDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_keydata.
  DATA: l_mod_yr TYPE i.

  DATA: BEGIN OF it_model OCCURS 0,
          model(2) TYPE c,
          name(30) TYPE c,
        END OF it_model.

  DATA: BEGIN OF it_cntry OCCURS 0,
          land1   TYPE t005-land1,
          code(2) TYPE c,
        END OF it_cntry.

  IF sy-abcde CS it_tab-m_gjahr.
    l_mod_yr = sy-fdpos + 10 + 2000.
    it_alv-mod_yr = l_mod_yr.
  ELSE.
    CONCATENATE '200' it_tab-m_gjahr INTO it_alv-mod_yr.
  ENDIF.

* Populate Model.
  it_model-model = 'CR'.
  it_model-name  = 'Santa Fe'.
  APPEND it_model.

  it_model-model = 'EM'.
  it_model-name  = 'Sonata'.
  APPEND it_model.

  it_model-model = 'IN'.
  it_model-name  = 'Sonata'.
  APPEND it_model.

** Furong on 04/01/14
  it_model-model = 'TC'.
  it_model-name  = 'Elantra'.
  APPEND it_model.
** End on 04/01/14

* Populate Country Code.
  it_cntry-land1 = 'CA'.
  it_cntry-code  = '06'.
  APPEND it_cntry.

** Furong on 04/01/14
  it_cntry-land1 = 'MX'.
  it_cntry-code  = '20'.
  APPEND it_cntry.
** End

  it_cntry-land1 = 'US'.
  it_cntry-code  = '28'.
  APPEND it_cntry.

  it_cntry-land1 = 'PR'.
  it_cntry-code  = '35'.
  APPEND it_cntry.

  it_cntry-land1 = 'GU'.
  it_cntry-code  = '31'.
  APPEND it_cntry.

  READ TABLE it_model WITH KEY model = it_tab-model.
  IF sy-subrc EQ 0.
    it_alv-mod_nm = it_model-name.
  ENDIF.

  READ TABLE it_cntry WITH KEY land1 = it_tab-land1.
  IF sy-subrc EQ 0.
    it_alv-cnty_cd = it_cntry-code.
  ENDIF.

  SELECT SINGLE landx INTO it_alv-cnty_tx
    FROM t005t
   WHERE spras = sy-langu
     AND land1 = it_tab-land1.

ENDFORM.                    " GET_KEYDATA
*&---------------------------------------------------------------------*
*&      Form  get_amortization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_PERIOD  text
*      <--P_L_AMOUNT  text
*----------------------------------------------------------------------*
FORM get_amortization USING    p_period TYPE i
                      CHANGING p_amount TYPE p.

  DATA: l_year      TYPE i,
        l_month     TYPE i,
        l_year1     TYPE i,
        p_yamt      TYPE ty_pack,
        p_mamt      TYPE ty_pack,
        l_yearn(2)  TYPE c,
        w_field(14) TYPE c.

  FIELD-SYMBOLS: <fs>.

  CHECK p_period BETWEEN 0 AND 119.

  l_year  = p_period DIV 12.
  l_month = p_period MOD 12.

  l_year1 = l_year.
  l_yearn = l_year1.

* Calculate  Amortization per Year
  p_yamt = it_war-wrbtr.
  WHILE l_year1 GT 0.
    CONCATENATE  'IT_WAR-WRBTR' l_yearn INTO w_field.
    ASSIGN (w_field) TO <fs>.
    p_yamt =  p_yamt - <fs> .
    l_year1 = l_year1 - 1 .
    l_yearn = l_year1.
  ENDWHILE.

* Calculate  Amortization per Period
  IF l_month GT 0.
    l_year = l_year + 1.
    l_yearn = l_year.
    CONCATENATE  'IT_WAR-WRBTR' l_yearn INTO w_field.
    ASSIGN (w_field) TO <fs>.
    p_mamt = <fs> * l_month / 12.
  ENDIF.

  p_amount = p_yamt - p_mamt.
ENDFORM.                    " get_amortization
*&---------------------------------------------------------------------*
*&      Form  get_present_val
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_PERIOD  text
*      <--P_IT_CALC_PVNET  text
*----------------------------------------------------------------------*
FORM get_present_val USING    p_period TYPE i
                              p_prcnt  TYPE f
                              p_pvdisc TYPE f              "UD1K950854
                     CHANGING p_amount TYPE p
                              p_amount_st TYPE p.

  DATA: l_year      TYPE i,
        l_month     TYPE i,
        l_year1     TYPE i,
        l_month1    TYPE i,
        p_mamt      TYPE ty_pack,
        l_yearn(2)  TYPE c,
        w_field(14) TYPE c,
        l_power     TYPE i,
        l_counter   TYPE i.

  FIELD-SYMBOLS: <fs>.

  CHECK p_period BETWEEN 0 AND 119.

  l_year  = p_period DIV 12.
  l_month = p_period MOD 12.

  l_year1 = l_year + 1.
  l_yearn = l_year1.

* Calculate present value for part of year
  IF l_month GT 0.
    CONCATENATE  'IT_WAR-WRBTR' l_yearn INTO w_field.
    ASSIGN (w_field) TO <fs>.

    l_month1 = 12 - l_month.
    DO l_month1 TIMES.
      p_mamt = p_mamt + ( <fs> * p_prcnt / 12 ) /
               p_pvdisc ** ( l_power / 12 ).               "UD1K950854
*              '1.052' ** ( l_power / 12 ).                "UD1K950854
      l_power = l_power + 1.

      l_counter = l_counter + 1.
      IF l_counter LE 12.
        p_amount_st = p_mamt.
      ENDIF.
    ENDDO.

    l_year1 = l_year1 + 1.
    l_yearn = l_year1.
  ENDIF.

* Calculate present value for full year
  WHILE l_year1 LE 10.
    CONCATENATE  'IT_WAR-WRBTR' l_yearn INTO w_field.
    ASSIGN (w_field) TO <fs>.

    DO 12 TIMES.
      p_mamt = p_mamt + ( <fs> * p_prcnt / 12 ) /
               p_pvdisc ** ( l_power / 12 ).               "UD1K950854
*              '1.052' ** ( l_power / 12 ).                "UD1K950854
      l_power = l_power + 1.

      l_counter = l_counter + 1.
      IF l_counter LE 12.
        p_amount_st = p_mamt.
      ENDIF.
    ENDDO.

    l_year1 = l_year1 + 1.
    l_yearn = l_year1.
  ENDWHILE.

  p_amount = p_mamt.

ENDFORM.                    " get_present_val
*&---------------------------------------------------------------------*
*&      Form  rename_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1624   text
*----------------------------------------------------------------------*
FORM rename_field USING p_field TYPE c p_tooltip TYPE c.
  it_fieldcat-scrtext_s = p_field.
  it_fieldcat-scrtext_m = p_field.
  it_fieldcat-scrtext_l = p_field.
  it_fieldcat-reptext   = p_field.
  it_fieldcat-tooltip   = p_tooltip.

  CLEAR it_fieldcat-ref_table.
ENDFORM.                    " rename_field
