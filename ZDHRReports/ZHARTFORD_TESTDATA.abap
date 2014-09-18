REPORT zhartford_testdata.
*-----------------------------------------------------------------------
* Name: ZH_hartford_out - Hartford Outbound data extract
* Tech. Resource: Hassan A. Siddiqui
* Desc: Hartford Single Client Feed (SCF) outbound (from SAP) records.
*       SCF consists of PAYROLL and INDICATIVE (MASTER DATA) files.

*----------------------------------------------------------------------
*
*----------------------------------------------------------------------
* For quick reference, this is an outline of the logic:
*----------------------------------------------------------------------
*  Initialize variables
*   For each employee (meeting selection criteria):
*    a) Read the P0000 actions and determine which should be processed
*    b) Read all infotypes needed, using ENDDA from action (P0000)
*    c) Create all record types for the employee. Save in internal tbl.
*    Create the actual output file from the internal table.
*    Write the summary and error reports.
* New zhhartforout layout:
*pernr/rec_id/record/eff_dt/aedtm/uzeit/uname

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Title          : ZH_HARTFORD_OUT
*  Author         : Hassan A. Siddiqui
*  Creation Data  : 02/25/2007
*  Requirements by: Karla Sterling
*  Description    : Hartford Single Client Feed (SCF) outbound (from
*                   SAP) records. SCF consists of PAYROLL and INDICATIVE
*                   (MASTER DATA) files.

************************************************************************
* CHANGE LOG
*-----------------------------------------------------------------------
* DATE      |  NAME          |Transport | Issue #  |      DESC
*-----------------------------------------------------------------------
*02/25/07  Hassan Siddiqui  UD1K940040  73KD623139
*02/25/07  Mohammad Haseeb                          Created RFC Function
*                                                   for Web Method.
*03/27/06  Hassan Siddiqui  UDIK940169  73KD623139  Remove 'O' from
*                                                   Admin I.D Fieldand
*                                                   add logic for
*                                                   Military 'ML'
*10/15/07  IG Moon          UD1K941878  7AF9356218
* {
*     Add cost centers :
*      55015 - Theta Engine Shop
*        (1a) MXEY13-2nd Engine C/Block Line
*        (1b) MXEY15-Engine C/Shaft Line
*        (1c) MXEY17-Engine C/Head Line
*        (1d) MXEYA1-2nd Engine Assembly Line
*      55025 - Engine Prod Support
*      55205 - Engine Subdivision
*      22001 - Accounting
*      22101 - Finance Subdivision
* }

*11/05/07  IG Moon          UD1K942066
* {
*  Add the logic for picking hourly-pay. as view point of pay period
*  which is entered at selection-screen.
*
* }
*04/12/11  Valerian Utama   UD1K951420  Modification related to Kronos
*                                       implementation
*                                       Fix logic for Military Leave
*07/27/11  Valerian Utama   UD1K952586  Modify Hartford interface for
*                                       KRONOS interface
*08/02/11  Valerian Utama   UD1K952650  Change Cut Off Date for Kronos
*                                       implementation.
*09/13/11  Valerian Utama   UD1K952967  Include Inpatriate time balance
*09/14/11  Valerian Utama   UD1K952973  Fix ABAP Dump
*09/22/11  Valerian Utama   UD1K953009  Various Fix
*09/14/11  change by Yn.Kim  for ECC6.  Index : Yn.Kim 01
*10/17/11  KDM              UP1K930010  day_psp --> day_psp2 : Index : KDM01
*02/22/11  Valerian Utama   UD1K954027  Add New Cost Center for Engine
*                                       Shop Implementation
*05/15/12  Valerian Utama   UD1K954872  Fix military leave calculation
*                                       error after Kronos implement.
*10/26/12  Valerian Utama   UD1K955726  Apply HMMA address to TM w/o
*                                       mailing address
***********************************************************************

*                      --- TABLES ---
*----------------------------------------------------------------------
TABLES:
*        pernr,     "Structure - HR personnel data
        p0001_af,
        m_premc, 	       "Match code tbl. for mapping SSN <-> EE
*	 pa0015, 	       "Infotype 15 table
        pa0001,             "Infotype 0001 - org. data
        pa0002,             "Person Info
        pa0168,             "Insurance
        pa0008,
        pa0169,
        pa0105,pa2002,pa2001,
        t510,
        t508z.                                              "UD1K954872
*        p1000.
*----------------------------------------------------------------------
*                     --- INFOTYPES ---
*----------------------------------------------------------------------
*
INFOTYPES:  0000,   "EE Actions
              0302,          "EE Additional Actions     "<<<< JMS006A
*   0001,     "EE Name
*   0002,     "EE Personal Data
    0006,     "EE Address
*             0007,         "EE Work time
*   0008,     "EE Salary
    0041,     "EE Key Dates
              0105,         "email
              0169,         "401k vested date
              0168,         "insurance
              0208,         "Work Tax area
              0375.         "HIghly Compensated
*             9169,                     "
*             0035.                     "
*field-symbols: <status>.
*field-symbols: <status2>.


*-----------------------------------------------------------------------
* Name: ZH_HARTFORD_OUT_RECS - data record definition.
* Tech. Resource: Hassan A. Siddiqui
* Desc: Hartford Single Client Feed (SCF) outbound (from SAP) records.
*       SCF consists of PAYROLL and INDICATIVE (MASTER DATA) files.
*
* Note: Most records will be a concatenation of record CONST and
*       another, though there are noted exceptions.
*-----------------------------------------------------------------------
* Modification Log
*-----------------------------------------------------------------------
* Date      Pgmr ID   mod ID  Description
* --------  --------  ------  ------------------------------------------
*
*-----------------------------------------------------------------------
* Commonly used fields by the interfaces to build records.
*-----------------------------------------------------------------------
DATA: BEGIN OF saved,
  pernr LIKE pernr-pernr,
  perid LIKE p0002-perid.
DATA: END OF saved.


* Constant record, to be used with each of the data records to make the
* actual record for the file. Each record has a 'constant' portion and
* that's what this record is for.
DATA: BEGIN OF const,
  plannum(5),   "DC Plan #
  batch(4),   "batch group ID/filler on some (blank)
  f1(3),    "Filler
  perid(11),    "SSN
  f2(6),    "Filler
  recid(2).   "Record Identifier
DATA: END OF const.
*

*------------------------------------------------------------*
*   Records used on the HARTFORD Feed                        *
*------------------------------------------------------------*
*Hartford Rec.
DATA: BEGIN OF rec_01 OCCURS 0,
      ssn(9),             "S.S
      nachn(30),            "Last Name
      vorna(30),            "First Name
      mid_initial(1),
      suffix(5),
      addrline(40),	       "Address
      addrline2(40),
      ort01(40),    "City
      state(2),   "State
      pstlz(10),    "Zip Code
      country(2),
      birtdate(10),	        "Date of birth (MM/DD/YYYY)
      gesch(1),    "Gender
      home_phone(15),                                       "PA0006
      status(15),
      status_date(10),       "(MM/DD/YYYY)
      hiredate(10),	         "Hire date
      rehiredate(10),
      terminatdate(10),
      salryclass(2),          "Salary class " Employment type
      comp_date(10),          "Scheduled annual base pay effective date
      comp_amt(12),	         "Scheduled annual base pay
      comp_code(1),           "Salary Basis
      employeeid(20),         "O ID # Team Member #
      emailaddr(50),          "email
      jobtitle(50),           "O Job Title
      costcen(40),            "O Department ID
      compid(20),             "O Company ID
      loc_id(10),             "O Location ID
      loc_description(40),    "O Location Description
      disabled(25),
      term_reason(35),
      workcen(35),
      exempt(10),
      emp_work_phone(15),
      total_work_hours(10),
      weekhrs(10),
      supervisor_last(30),
      supervisor_first(20),
      super_id(10),
      super_phone(15),
      super_ext(4),
      super_email(50),
      hrm_last(30),
      hrm_first(20),
      hrm_phone(15),
      hrm_phone_ext(4),
      hrm_email(50).
DATA: END OF rec_01.

DATA: BEGIN OF it_calc OCCURS 0,
        pernr(10),
        flag,
        begda LIKE pa0000-begda,
        endda LIKE pa0000-endda,
        massn TYPE massn,
        cnt TYPE i,
        massg TYPE massg,
      END OF it_calc.

DATA $it_calc_pernr LIKE it_calc OCCURS 0 WITH HEADER LINE.
DATA it_mili_leave LIKE it_calc OCCURS 0 WITH HEADER LINE.
DATA psp LIKE pc2ba OCCURS 0 WITH HEADER LINE.

DATA pdpsp LIKE pdpsp OCCURS 0 WITH HEADER LINE.
*DATA day_psp LIKE pdpsp OCCURS 0 WITH HEADER LINE.
DATA pdpnr LIKE pdpnr OCCURS 0 WITH HEADER LINE.
DATA day_psp2 LIKE PDSPPSP OCCURS 0 WITH HEADER LINE.  "Yn.Kim 01


DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X',
            cutoff_date TYPE datum VALUE '20110919'.        "UD1K953009
*           cutoff_date TYPE datum VALUE '20110725'.        "UD1K952650


*-----Globale Daten
INCLUDE rptbal01.
*-----Reportparamter/Selektionsbedingungen
*INCLUDE rptbal02.
*---------------------------------------------------------------------

*---------------------------------------------------------------------
*                    --- Global Data ---
*---------------------------------------------------------------------
** Itab to hold errors found in processing
DATA: BEGIN OF i_error OCCURS 0,
  type(3),          "type of error
  pernr LIKE p0000-pernr,   "EE #
  id(3),            "record ID
  var(50),        "variable info. for
END OF i_error.

* BEGIN OF UD1K952967
DATA: BEGIN OF period_inpat_kronos OCCURS 0,
        pernr LIKE p0001-pernr,
        begda LIKE p0001-begda,
        endda LIKE p0001-endda,
      END OF period_inpat_kronos.

DATA: saldo_inpat_kronos LIKE pc2b5 OCCURS 0 WITH HEADER LINE.
* END OF UD1K952967

* Misc. variables:
DATA: 2byte(2),
      w_addr_chg(1),
      w_cntr TYPE i,
      w_filetype(1),
*      w_record_cnt type i,
      w_frozen_base_salary_total(13) TYPE p DECIMALS 2,
      w_loan_total(14) TYPE p DECIMALS 2,
      w_change(1),
      w_applicable(1),
      w_betrg_work(13) TYPE p DECIMALS 2,
      w_cnt_work(13) TYPE p DECIMALS 0,
      w_betrg7(7) TYPE c,
      w_betrg14(14) TYPE c,
      subrc LIKE sy-subrc,
      subrc_main LIKE sy-subrc,
      w_dat LIKE sy-datum,
      ret(1),
      g_recruiter,                               "Flag -  recruiter?
      g_p0000 LIKE p0000,                        "currently valid P0000
      g_tot_ee TYPE i,                           "total EE processed
      g_rej_ee TYPE i,                           "total EE rejected
      f_loadate TYPE d,
      l_stell LIKE p0001,
      l_stltx LIKE t513s-stltx,
      l_kostl LIKE p0001-kostl,
      output_string LIKE p0001-kostl,
      l_persk LIKE p0001-persk,
      l_massn LIKE p0000-massn,
      l_ommng_na LIKE p0001_af-omngr_na,
      s_pernr LIKE p0001-pernr,
      e_pernr LIKE p0002-pernr,
      s_vorna LIKE p0002-vorna,
      s_nachn LIKE p0002-nachn,
      kostl_des(10),
      pltyp_des LIKE p0168-pltyp,
      l_begda LIKE p0168-begda,
      l_endda LIKE p0168-endda,
      l_betrg LIKE t510,
      w_hours TYPE p DECIMALS 2,
      w_deduct TYPE p DECIMALS 2,
      w_military TYPE p DECIMALS 2,
      v_loadate TYPE d.

DATA: high(10),
      low(10),
      pkostl(10).

DATA: p_filepc  LIKE rlgrap-filename.
DATA: p_lpath   LIKE filenameci-pathintern.
DATA: p_fileun  LIKE rlgrap-filename.
DATA: r_kostl LIKE pa0001-kostl.
DATA: schkz LIKE p0007-schkz,
*      pernr      LIKE pernr-pernr,
      gesch(10),
      clstb2_per LIKE rpclxxxx-clstb2_per,
      datum      LIKE sy-datum,
*      ztart      LIKE zes-ztart,
      ztext      LIKE t555b-ztext,
      anzhl      TYPE p DECIMALS 2.

DATA: counter TYPE i VALUE 5.
DATA: xy TYPE c VALUE '0'.

* by IG.MOON 11/06/2007 {
DATA: end_of_pay_period LIKE sy-datum.
* }

*----------------------------------------------------------------------*
*                   --- Selection Screen ---                           *
*----------------------------------------------------------------------*
DATA: w_tothr(8) TYPE p DECIMALS 2.

DATA: w_permo  LIKE t549a-permo,   " Period parameters
      w_abkrt  LIKE t549t-atext.   " Payroll Area Text

*** File name, display only option.
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-t01.
PARAMETERS:        p_disp AS CHECKBOX.
* PC file selections
SELECTION-SCREEN BEGIN OF BLOCK pcsel WITH FRAME TITLE text-p24.
* download to pc checkbox
PARAMETERS: p_pc  AS CHECKBOX.
*parameters: p_filepc  like rlgrap-filename
*                      default 'c:\temp\hartford_out.txt'.
SELECTION-SCREEN END OF BLOCK pcsel.

* Server file selections
SELECTION-SCREEN BEGIN OF BLOCK svsel WITH FRAME TITLE text-p25.
* download to server checkbox
*parameters: p_unix as checkbox."UNCOMMENT-CREATE A CHECK BOX FOR UNIX**
*parameters: p_lpath   like filenameci-pathintern
*                     default 'Z_HART_SAP' obligatory.

*parameters: p_fileun  like rlgrap-filename
*                      default 'hartford.txt'.
SELECTION-SCREEN END OF BLOCK svsel.

SELECTION-SCREEN END OF BLOCK a1.

****************************************************************

*** Processing options
SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME TITLE text-t02.
PARAMETERS:
      p_update AS CHECKBOX DEFAULT 'X',
      p_datum LIKE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK a3.

*********************************************
*Date range selection for Actual Hours Worked.
**********************************************
SELECT-OPTIONS it_range FOR pernr-dayps OBLIGATORY."NO INTERVALS.

PARAMETERS p_abkrs LIKE  pa0001-abkrs DEFAULT '11'
NO-DISPLAY.
PARAMETERS: p_abrpr  LIKE t549q-pabrp MODIF ID p1 NO-DISPLAY,
            p_abrjr  LIKE t549q-pabrj MODIF ID p1 NO-DISPLAY.

*-----------------------------------------------------------------------
*                     --- INITIALIZATION ---
*-----------------------------------------------------------------------
INITIALIZATION.

*  CALL FUNCTION '/SAPHT/DRM_CALC_DATE'
*       EXPORTING
*            date      = sy-datum
*            days      = 0
*            months    = 0
*            sign      = '-'
*            years     = 1
*       IMPORTING
*            calc_date = it_range-low.
*
**  IT_RANGE-LOW = SY-DATUM.
*  it_range-high = sy-datum .
*  it_range-sign = 'I'.
*  it_range-option = 'BT'.
*  APPEND it_range.

  __cls it_range.

  PERFORM get_payroll_period USING p_abkrs
                          CHANGING w_permo it_range-low it_range-high
                                   w_abkrt p_abrpr p_abrjr.

  it_range-sign = 'I'.
  it_range-option = 'BT'.
  APPEND it_range.

START-OF-SELECTION.

  IF p_datum IS INITIAL.
    p_datum = sy-datum.
  ENDIF.

  PERFORM get_end_of_paydate.
  IF end_of_pay_period IS INITIAL.
    end_of_pay_period = p_datum.
  ENDIF.

  PERFORM fill_i549q USING pn-begda pn-endda.
*-----------------------------------------------------------------------
GET pernr.

  FREE graf_tab. CLEAR graf_tab.
  CLEAR w_hours.

  DELETE p0000 WHERE begda > p_datum.
  CLEAR : p0000.

  SORT p0000 BY begda DESCENDING.
  READ TABLE p0000 INDEX 1.
  IF sy-subrc EQ 0.
    p0000-endda = '99991231'.
    MODIFY p0000 INDEX 1 TRANSPORTING endda.
  ENDIF.
  SORT p0000 BY begda.

  READ TABLE p0000 WITH KEY endda = '99991231'.

  IF sy-subrc EQ 0.
    CHECK p0001-persg NE '4'.
*    CHECK p0000-stat2 EQ '3' OR p0000-stat2 EQ '1'.
*    CHECK p0000-massn NE 'ZE' AND
*          p0000-massn NE 'ZI' AND
*          p0000-massn NE 'ZX' AND
*          p0000-massn NE 'ZW' AND
*          p0000-massn NE 'ZY' AND
*          p0000-massn NE 'Z5' .
  ELSE.
  ENDIF.
* Read the actions for the employee; eliminate ones already processed.
  PERFORM z001_read_p0000 USING pernr-pernr subrc.
  CHECK subrc = 0.

* Read period inpat from PA0001
  PERFORM get_period_inpat.                                 "UD1K952967

* Complete processing for the employee - read other ITs and format data.
  PERFORM z001_read_current_infotypes USING pernr-pernr subrc p_datum.
  CHECK subrc = 0.

  PERFORM z100_process_employee.

*-----------------------------------------------------------------------
END-OF-SELECTION.
  CHECK subrc_main = 0.          "Exit if we hit any probs in start-sel.

**---------------------------------------------------------------

* Write the records accummulated to the file:
  PERFORM z900_write_file USING subrc.
  CHECK subrc = 0.

* Write a report/error log:
  SORT rec_01 BY employeeid.
  DATA $ix TYPE i.
  DATA $empid(20).


  LOOP AT i_error.

    $ix =  sy-tabix.
    CONCATENATE '0000000000' i_error-pernr INTO $empid.
    READ TABLE rec_01 WITH KEY employeeid = $empid BINARY SEARCH.

    IF sy-subrc EQ 0.
    ELSE.
      DELETE i_error INDEX $ix.
    ENDIF.
  ENDLOOP.

  PERFORM z930_report_errors.

***********************************************************************
*  FORM Z001_READ_p0000                                               *
*---------------------------------------------------------------------*
* Read only the P0000 data. Broken out from the form that reads all
* infotypes to support multiple event handling.
*---------------------------------------------------------------------*
FORM z001_read_p0000 USING s_pernr p_subrc.
  DATA: l_begda LIKE sy-datum,
        l_p0000 LIKE LINE OF p0000 OCCURS 0 WITH HEADER LINE."<<<< JMS00

  p_subrc = sy-subrc.
  IF p_subrc <> 0.
    ADD 1 TO g_tot_ee.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z001_READ_CURRENT_INFOTYPES
*&---------------------------------------------------------------------*
* This form uses SAP HR macros to read the infotypes valid for the P0000
* being used. Note that we can read multiple P0000 records. We are also
* logging errors when reads fail.
*----------------------------------------------------------------------*
FORM z001_read_current_infotypes USING s_pernr subrc p_datum.

  DATA        l_lines TYPE i.
  DATA l_var LIKE i_error-var.

  CLEAR p0001.  REFRESH p0001.
  CLEAR p0002.  REFRESH p0002.
  CLEAR p0006.  REFRESH p0006.
  CLEAR p0008.  REFRESH p0008.
  CLEAR p0041.  REFRESH p0041.
  CLEAR p0105.  REFRESH p0105.
  CLEAR p0375.  REFRESH p0375.
  CLEAR p0208.  REFRESH p0208.
  CLEAR p0168.  REFRESH p0168.

  rp-read-infotype s_pernr 0001 p0001 p_datum p_datum.

  DELETE p0001 WHERE begda > p_datum.
  READ TABLE p0001 INDEX 1.
  subrc = sy-subrc.
  IF subrc <> 0.
    WRITE subrc TO l_var(1).
    CONCATENATE 'RC:' l_var(1) p_datum INTO l_var SEPARATED BY space.
    PERFORM log_error USING s_pernr '001' ' ' l_var.
    EXIT. "leave form
  ENDIF.

  rp-read-infotype s_pernr 0208 p0208 p_datum p_datum.
  DELETE p0208 WHERE begda > p_datum.
  READ TABLE p0208 INDEX 1.
  subrc = sy-subrc.

  IF subrc <> 0.
    WRITE subrc TO l_var(1).
    CONCATENATE 'RC:' l_var(1) p_datum INTO l_var SEPARATED BY space.
    PERFORM log_error USING s_pernr '208' ' ' l_var.
    EXIT. "leave form
  ENDIF.

* JMS005: Add logic:
* Need to check that certain data is in sel. options...
* GET PERNR is not checking any date, so if there is an emp.
* who had changed data, and the old data meets criteria, he
* would be included even though his current data does not!
  IF NOT p0001-abkrs IN pnpabkrs        "Payroll area
  OR NOT p0001-bukrs IN pnpbukrs        "Company code
  OR NOT p0001-werks IN pnpwerks        "Pers. area
  OR NOT p0001-persg IN pnppersg        "Employee group
  OR NOT p0001-persk IN pnppersk        "Employee Subgroup
  OR NOT p0001-btrtl IN pnpbtrtl.       "Personnel Subarea
    subrc = 5.

    EXIT. "leave form
* No need to log error!
  ENDIF.

  rp-read-infotype s_pernr 0002 p0002 p_datum p_datum.

  DELETE p0002 WHERE begda > p_datum.
  READ TABLE p0002 INDEX 1.
  subrc = sy-subrc.
  IF subrc <> 0.
    WRITE subrc TO l_var(1).
    CONCATENATE 'RC:' l_var(1) p_datum INTO l_var SEPARATED BY space.
    PERFORM log_error USING s_pernr '002' ' ' l_var.
    EXIT. "leave form
  ENDIF.

  rp-read-infotype s_pernr 0006 p0006 p_datum p_datum.
* Only use address type 1 - primary residence,
  DELETE p0006 WHERE subty <> '5'.
  DELETE p0006 WHERE begda > p_datum.
  READ TABLE p0006 INDEX 1.
* error handling/logging:
* BEGIN OF UD1K955726
* subrc = sy-subrc.
* IF subrc <> 0.
*   WRITE subrc TO l_var(1).
*   CONCATENATE 'RC:' l_var(1) p_datum INTO l_var SEPARATED BY space.
*   PERFORM log_error USING s_pernr '006' ' ' l_var.
*   EXIT. "leave form
* ENDIF.
* END OF UD1K955726

  rp-read-infotype s_pernr 0375 p0375 p_datum p_datum.
  rp-read-infotype s_pernr 0168 p0168 p_datum p_datum.
  rp-read-infotype s_pernr 0041 p0041 p_datum p_datum.
  rp-read-infotype s_pernr 0008 p0008 p_datum p_datum.

  DELETE p0375 WHERE begda > p_datum.
  DELETE p0168 WHERE begda > p_datum.
  DELETE p0041 WHERE begda > p_datum.
  DELETE p0008 WHERE begda > p_datum.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM log_error                                                *
*---------------------------------------------------------------------*
* This form tracks an error in the errors table for later outout.     *
*---------------------------------------------------------------------*
FORM log_error USING p_pernr p_type p_rec p_var.

  i_error-pernr = p_pernr.
  i_error-type  = p_type.
  i_error-id    = p_rec.
  i_error-var   = p_var.
  APPEND i_error.
  CLEAR i_error.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z100_PROCESS_EMPLOYEE
*&---------------------------------------------------------------------*
* This form will generate each type of record. No checking
* on SAP action will occur at this point.
*----------------------------------------------------------------------*
FORM z100_process_employee.

  DATA:
  l_bg_fd(1),                       "Flag to track whether we have a BG.
  l_rc LIKE sy-subrc,               "Temp. return code  - BG creation
  l_it_rc LIKE sy-subrc,            "Temp. return code  - infotype read
  l_p0000 LIKE p0000,               "To hold the currently valid P0000,
                                    "or the one assoc. w/ the BG record.
  l_xp0000 LIKE p0000,              "Hold unchgd P0000 for copy <JMS004
  l_var LIKE i_error-var,          "Variable for error logging
  l_p0208 LIKE p0208.

* ------------------------- Remaining Records ------------------------ *

  PERFORM z110_process_rec_01.

ENDFORM.                    " Z100_PROCESS_EMPLOYEE

*&---------------------------------------------------------------------*
*&      Form  Z001_BUILD_CONST
*&---------------------------------------------------------------------*
* This form generates the 'constant' portion for each type of record.
*----------------------------------------------------------------------*
FORM z001_build_const USING recid.

  CLEAR const.
  IF saved-pernr <> pernr-pernr.
    SELECT * FROM m_premc
      WHERE pernr = pernr-pernr
      AND begda_0002 <= p_datum
      AND endda_0002 >= p_datum.
      EXIT.
    ENDSELECT.
    IF sy-subrc <> 0.
      MOVE '000000000'   TO saved-perid.
      MOVE pernr-pernr   TO saved-pernr.
    ELSE.
      MOVE m_premc-pernr TO saved-pernr.
      MOVE m_premc-perid TO saved-perid.
    ENDIF.
  ENDIF.
  WRITE saved-perid TO rec_01-ssn.

ENDFORM.                    " Z001_BUILD_CONST
*&---------------------------------------------------------------------*
*&      Form  GET_0041_DATE
*&---------------------------------------------------------------------*
* This form looks through the most of the date variable fields for the
* specific date type passed in, and if it is found, returns the date.
*----------------------------------------------------------------------*
FORM get_0041_date USING    dar
                            dat
                            ret.
  ret = '1'.
  dat = '00000000'.
  IF p0041-dar01 = dar.
    dat = p0041-dat01.
    ret = 0.
  ELSEIF p0041-dar02 = dar.
    dat = p0041-dat02.
    ret = 0.
  ELSEIF p0041-dar03 = dar.
    dat = p0041-dat03.
    ret = 0.
  ELSEIF p0041-dar04 = dar.
    dat = p0041-dat04.
    ret = 0.
  ELSEIF p0041-dar05 = dar.
    dat = p0041-dat05.
    ret = 0.
  ELSEIF p0041-dar06 = dar.
    dat = p0041-dat06.
    ret = 0.
  ELSEIF p0041-dar07 = dar.
    dat = p0041-dat07.
    ret = 0.
  ELSEIF p0041-dar08 = dar.
    dat = p0041-dat08.
    ret = 0.
  ELSEIF p0041-dar09 = dar.
    dat = p0041-dat09.
    ret = 0.
  ELSEIF p0041-dar10 = dar.
    dat = p0041-dat10.
    ret = 0.
  ELSEIF p0041-dar11 = dar.
    dat = p0041-dat11.
    ret = 0.
  ELSEIF p0041-dar12 = dar.
    dat = p0041-dat12.
    ret = 0.
  ENDIF.
ENDFORM.                    " GET_0041_DATE

*&---------------------------------------------------------------------*
*&      Form  Z300_APPEND_OUTFIL
*&---------------------------------------------------------------------*
* This form gets the Kostl and the next step checks the gets LOA rec
*----------------------------------------------------------------------*
FORM z300_append_outfil USING r1 r2 cntr.
*----------------------------------------------------------------------
  cntr = 0.
*----------------------------------------------------------------------
  CLEAR rec_01-employeeid.

  rec_01-employeeid = pernr-pernr.                    "Employee #
  PERFORM get_super_dept_devi USING rec_01-employeeid."Kostl

  IF rec_01-status <> 'T' AND rec_01-status <> 'D'.
    PERFORM get_salar_dept USING rec_01-employeeid.     "LOA rec
  ENDIF.

*  add 1 to w_record_cnt.
  cntr = 1.
ENDFORM.

*****************************************************
FORM get_anual_and_week_hrs USING  value(p_flag).

  __cls psp.
  CLEAR : w_hours, w_tothr.

  CLEAR : time_data_saldo[], time_data_saldo.
  CLEAR : graf_tab[], graf_tab.

*-----Ausgabe einer Status-Information
  PERFORM progress_indicator USING text-beg.
*-----Import time results and populate the data tables
  CLEAR w_deduct.

  LOOP AT i549q.

*** blocks switched - NOTE 375711
    IF i549q-begda < it_range-low.
      begda = it_range-low.
    ELSE.
      begda = i549q-begda.
    ENDIF.
    IF i549q-endda > it_range-high.
      endda = it_range-high.
    ELSE.
      endda = i549q-endda.
    ENDIF.

    IF endda < begda.
      endda = begda.
    ENDIF.

* blocks switched - NOTE 375711
*-----Lesen der Infotypdaten
*    PERFORM get_infty_data USING begda high-date. "<<<< NOTE 375711
*-----Füllen der Struktur PME51 und Auswerten
*    PERFORM fill_pme51.
*-----Import der Tabellen ZES, SALDO und ZL aus Cluster B2
    CALL FUNCTION 'HR_TIME_RESULTS_IN_INTERVAL'
         EXPORTING
              int_pernr             = pernr-pernr
              int_begda             = begda
              int_endda             = endda
         TABLES
              int_time_results      = time_results
         EXCEPTIONS
              wrong_cluster_version = 1
              no_read_authority     = 2
              cluster_archived      = 3
              technical_error       = 4
              OTHERS                = 5.
    CASE sy-subrc.
      WHEN 1.                          "falsche Version Cluster B2
        PERFORM error_handling USING pernr-pernr '72' 'E' '101'
                                     space space space space.
      WHEN 2.  "keine Leseberechtigung Cluster B2
        PERFORM error_handling USING pernr-pernr '72' 'E' '102'
                                     space space space space.
      WHEN 3.                          "Cluster archiviert
        PERFORM error_handling USING pernr-pernr '72' 'E' '139'
                                     space space space space.
      WHEN 4.                          "technischer Fehler
        PERFORM error_handling USING pernr-pernr '72' 'E' '140'
                                     space space space space.
      WHEN OTHERS.
        CLEAR: zl[],saldo[],zes[].                          "L9CK006744
        CLEAR: zl, saldo, zes.                              "L9CK006744
        CLEAR: saldo_inpat_kronos, saldo_inpat_kronos[].    "UD1K952973
        LOOP AT time_results INTO time_results_wa.
          APPEND LINES OF time_results_wa-zes   TO zes.
          APPEND LINES OF time_results_wa-saldo TO saldo.
          APPEND LINES OF time_results_wa-zl    TO zl.
*          APPEND LINES OF time_results_wa-psp   TO psp.
        ENDLOOP.
    ENDCASE.
*-----Füllen der internen Datentabellen
    CHECK NOT zes[]   IS INITIAL OR
          NOT saldo[] IS INITIAL OR
          NOT zl[]    IS INITIAL OR
          NOT psp[]   IS INITIAL.

*    PERFORM fill_time_data.

* act_period+0(4) = i549q-pabrj.
* act_period+4(2) = i549q-pabrp.
    act_period+0(4) = it_range-low.
    act_period+4(2) = it_range-high.

    IF i549q-begda(6) GE it_range-low(6)
      AND i549q-endda(6) LE it_range-high(6).

      CLEAR : saldo, saldo[].
      CLEAR : saldo_inpat_kronos, saldo_inpat_kronos[].     "UD1K952973

      LOOP AT zes.
*       MOVE-CORRESPONDING zes TO saldo.                    "UD1K953009
* BEGIN OF UD1K952586
*       IF ( zes-ztart = '9201'
*       OR   zes-ztart = '9202'
*       OR   zes-ztart = '9203'
*       OR   zes-ztart = '9304'
*       IF ( zes-ztart = '9305'
*       OR   zes-ztart = '9306'
*       OR   zes-ztart = '9307'
*       OR   zes-ztart = '9406'
*       OR   zes-ztart = '9407'
*       OR   zes-ztart = '9408' ).
*       AND  begda > cutoff_date.
*         break his20084.
*       ENDIF.
* END OF UD1K952586
*       COLLECT saldo.                                      "UD1K953009

* BEGIN OF UD1K952967
        DATA: l_inpat_date LIKE sy-datum.

        CLEAR: saldo_inpat_kronos.
        CONCATENATE i549q-begda(6) zes-reday INTO l_inpat_date.

        LOOP AT period_inpat_kronos WHERE begda <= l_inpat_date
                                      AND endda >= l_inpat_date.
          MOVE-CORRESPONDING zes TO saldo_inpat_kronos.
          COLLECT saldo_inpat_kronos.
          EXIT.
        ENDLOOP.

* BEGIN OF UD1K953009
        IF sy-subrc <> 0.
          MOVE-CORRESPONDING zes TO saldo.
          COLLECT saldo.
        ENDIF.
* END OF UD1K953009

* END OF UD1K952967

      ENDLOOP.

      LOOP AT zl.
        IF zl-lgart EQ '0312'.
          ADD zl-anzhl TO w_deduct.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF p_flag EQ 'Y'. " Annual
      PERFORM process_saldo.
    ELSE .
      PERFORM process_saldo_week.
    ENDIF.

  ENDLOOP.

  LOOP AT graf_tab.
    w_hours =  w_hours + graf_tab-anzhl .
  ENDLOOP.

  SELECT * FROM pa2002 WHERE pernr EQ pernr-pernr
                        AND ( awart EQ '1006' OR awart EQ '1007' )
                        AND begda >= it_range-low
                        AND begda <= it_range-high.
    w_hours =  w_hours - pa2002-stdaz.
  ENDSELECT.

* BEGIN OF UD1K951420
  IF p_flag NE 'Y'.
    SELECT * FROM pa2001 WHERE pernr EQ pernr-pernr
                           AND ( awart EQ '1033' OR awart EQ '1071' )
                           AND begda >= it_range-low
                           AND begda <= it_range-high
                           AND begda <  cutoff_date.
      w_hours =  w_hours - pa2001-stdaz.
    ENDSELECT.
  ENDIF.

*  IF p_flag EQ 'Y'.
*    SELECT * FROM pa2001 WHERE pernr EQ pernr-pernr
*                           AND awart EQ '1033'
*                           AND begda >= it_range-low
*                           AND begda <= it_range-high.
*      w_hours =  w_hours + pa2001-stdaz.
*    ENDSELECT.
*  ENDIF.
* END OF UD1K951420

* calculate Military Leave.
*  CLEAR w_military.
*  SORT psp BY datum.
*  DELETE ADJACENT DUPLICATES FROM psp COMPARING datum.
  CLEAR w_military.
  READ TABLE it_mili_leave INDEX 1.

  IF sy-subrc EQ 0.
*    __cls : pdpnr,pdpsp,day_psp.  "" KDM01
    __cls : pdpnr,pdpsp,day_psp2.  "" KDM01
    pdpnr-pernr = pernr-pernr.
    APPEND pdpnr.

    CALL FUNCTION 'HR_PERSON_READ_WORK_SCHEDULE'
      EXPORTING
        begin_date                      = it_range-low
        end_date                        = it_range-high
*     GROUPING_DWS                    =
*     GROUPING_ATTENDENCE             =
*     GROUPING_SUBSTITUTE             =
        read_from_database              = 'X'
*     IM_READ_NO_LOCKED_RECORDS       =
      TABLES
        pernr_tab                       = pdpnr
        psp                             = pdpsp
*// 2011.09.14 change by Yn.Kim  for ECC6.
        day_psp                         = day_psp2  " Yn.kim 01
*        day_psp                         = day_psp
*   EXCEPTIONS
*     ERROR_IN_BUILD_PSP              = 1
*     OTHERS                          = 2
              .

    IF sy-subrc <> 0. ENDIF.

* BEGIN OF UD1K954872
    TABLES: t552a, t550a.

    DATA:   l_prvdt TYPE p0001-begda,
            l_schkz TYPE p0007-schkz,
            l_tprog TYPE t550a-tprog,
            l_ftk   TYPE t552a-ftk01,
            l_fname(30) TYPE c.

    FIELD-SYMBOLS: <fs>.
* END OF UD1K954872

    LOOP AT it_mili_leave.

* BEGIN OF UD1K954872
      CLEAR: l_prvdt, l_schkz.

      l_prvdt = it_mili_leave-begda - 1.
      SELECT schkz INTO l_schkz
        FROM pa0007
       UP TO 1 ROWS
       WHERE pernr = pernr-pernr
         AND endda >= l_prvdt
         AND begda <= l_prvdt.
      ENDSELECT.
* END OF UD1K954872

* BEGIN OF UD1K954872
      LOOP AT pdpsp WHERE datum BETWEEN it_mili_leave-begda
                                    AND it_mili_leave-endda.

*     LOOP AT pdpsp.
*       IF pdpsp-datum >= it_mili_leave-begda AND
*          pdpsp-datum <= it_mili_leave-endda.

        CLEAR: t552a, t550a.
        SELECT SINGLE * FROM t552a
         WHERE zeity = t503-zeity
           AND mofid = t001p-mofid
           AND mosid = t001p-mosid
           AND schkz = l_schkz
           AND kjahr = pdpsp-datum(4)
           AND monat = pdpsp-datum+4(2).

        IF sy-subrc = 0.
          CONCATENATE 'T552A-TPR' pdpsp-datum+6(2)
                 INTO l_fname.
          ASSIGN (l_fname) TO <fs>.
          IF sy-subrc = 0.
            l_tprog = <fs>.
            CONCATENATE 'T552A-FTK' pdpsp-datum+6(2)
                   INTO l_fname.
            ASSIGN (l_fname) TO <fs>.
            l_ftk = <fs>.

            SELECT * FROM t550a
             UP TO 1 ROWS
             WHERE motpr = t508z-motpr
               AND tprog = l_tprog
               AND endda >= pdpsp-datum
               AND begda <= pdpsp-datum.
            ENDSELECT.
          ENDIF.
        ENDIF.
* END OF UD1K954872

        IF p_flag EQ 'Y'.

* BEGIN OF UD1K954872
          IF l_ftk = ' '.
            ADD t550a-sollz TO w_military.
          ENDIF.
*         IF pdpsp-ftkla EQ '0'. " Annual
*           ADD pdpsp-stdaz TO w_military.
*         ENDIF.
* END OF UD1K954872

        ELSE.
          ADD t550a-sollz TO w_military.                    "UD1K954872
*         ADD pdpsp-stdaz TO w_military.                    "UD1K954872
        ENDIF.
*       ENDIF.                                              "UD1K954872
      ENDLOOP.
    ENDLOOP.

  ENDIF.

  w_hours = w_hours + w_military.

  IF p_flag EQ 'Y'. " Annual
    w_tothr = w_hours - w_deduct.
  ELSE.
    w_tothr = w_hours / '52.14'.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z110_PROCESS_REC_01
*&---------------------------------------------------------------------*
* Generate the 01 record: Participant Name
*----------------------------------------------------------------------*
FORM z110_process_rec_01.
  DATA $ix TYPE i.
  LOOP AT p0000.
    $ix = sy-tabix.
    IF p0000-massn EQ 'Z0' AND p0000-massg = '99'.
      PERFORM get_0041_date USING 'Z1' p0000-begda ret.
      MODIFY p0000 INDEX $ix TRANSPORTING begda.
    ENDIF.
  ENDLOOP.

  DATA $date TYPE datum.

  IF it_range-high IS INITIAL.
    it_range-high = it_range-low.
  ENDIF.

  CALL FUNCTION '/SAPHT/DRM_CALC_DATE'
       EXPORTING
            date      = p_datum  "it_range-high
            days      = '90'
            months    = '00'
            sign      = '-'
            years     = '00'
       IMPORTING
            calc_date = $date.
  CLEAR rec_01.
  PERFORM z001_build_const USING '01'.

  MOVE-CORRESPONDING p0002 TO rec_01.

  rec_01-nachn       = p0002-nachn.
  rec_01-vorna       = p0002-vorna.
  rec_01-mid_initial = p0002-midnm.
  rec_01-suffix      = p0002-namzu.

  IF p0002-gesch = '1'.
    MOVE 'M' TO rec_01-gesch.
  ELSE.
    MOVE 'F' TO rec_01-gesch.
  ENDIF.

  TRANSLATE rec_01 TO UPPER CASE.

  PERFORM create_it_mili_leave.
************************************
* BEGIN OF UD1K954872
  IF NOT it_mili_leave[] IS INITIAL.
    CLEAR: t001p, t503, t508z.

    SELECT SINGLE * FROM t001p WHERE werks = p0001-werks
                                 AND btrtl = p0001-btrtl.

    SELECT SINGLE * FROM t503 WHERE persg = p0001-persg
                                AND persk = p0001-persk.

    SELECT SINGLE * FROM t508z WHERE mosid = t001p-mosid.
  ENDIF.
* END OF UD1K954872

  PERFORM get_anual_and_week_hrs USING 'Y'.

*  UNPACK w_tothr TO rec_01-total_work_hours.
  MOVE w_tothr TO rec_01-total_work_hours.

*-----------------------
*Exempt or Non-Exempt.
*Cost Center
*---------------------
  IF p0001-persk = 'U0'
  OR p0001-persk = 'U3'.
    MOVE 'N' TO rec_01-exempt.
*    rec_01-costcen = p0001-kostl.
    l_stell = p0001-stell.
  ELSE.
    MOVE 'E' TO rec_01-exempt.
* rec_01-costcen = p0001-kostl.
    l_stell = p0001-stell.

  ENDIF.

  SELECT SINGLE stltx INTO l_stltx
      FROM t513s WHERE stell = l_stell.
  MOVE l_stltx TO rec_01-jobtitle.

*----------------
* Address line 1
*----------------
  IF p0006-anssa = '5'.
    rec_01-addrline = p0006-stras.
    rec_01-addrline2 = p0006-locat.
    rec_01-ort01 = p0006-ort01.                             "UD1K955726
    rec_01-state = p0006-state.                             "UD1K955726
    rec_01-pstlz = p0006-pstlz.                             "UD1K955726
    rec_01-country = p0006-land1.                           "UD1K955726
  ELSE.                                                     "UD1K955726
    rec_01-addrline = '700 HYUNDAI BLVD'.                   "UD1K955726
    rec_01-ort01 = 'MONTGOMERY'.                            "UD1K955726
    rec_01-state = 'AL'.                                    "UD1K955726
    rec_01-pstlz = '36105'.                                 "UD1K955726
    rec_01-country = 'US'.                                  "UD1K955726
  ENDIF.
*  TRANSLATE  rec_01-addrline TO UPPER CASE.
*  TRANSLATE  rec_01-addrline2 TO UPPER CASE.

***
* MOVE-CORRESPONDING p0006 TO rec_01.                       "UD1K955726

* If we have a foreign address, do not populate the STATE or ZIP fields.
*  IF p0006-land1+0(2) = 'US'.
*    rec_01-country = '001'.   "U.S.A
  rec_01-salryclass = 'F'.  "Full time
**    rec_01-weekhrs = '4000'.  "Weekly Scheduled Hrs
*  ENDIF.

* rec_01-country = p0006-land1.                             "UD1K955726

  IF p0001-persk EQ 'U0' OR
        p0001-persk EQ 'U3' .

    PERFORM get_anual_and_week_hrs USING 'W'.
*    UNPACK w_tothr TO rec_01-weekhrs.
    MOVE w_tothr TO rec_01-weekhrs.

  ELSE.
    rec_01-weekhrs = '40.00'.  "Weekly Scheduled Hrs
  ENDIF.

  TRANSLATE rec_01 TO UPPER CASE.

*----Determine birthdate
  WRITE p0002-gbdat TO rec_01-birtdate.

***Status

* REC#11 STATUS
  IF
*p0000-stat2 eq '3' or
  p0000-massn EQ 'ZD' OR
  p0000-massn EQ 'Z9' OR
  p0000-massn EQ 'Z0' OR
  p0000-massn EQ 'Z1' OR
  p0000-massn EQ 'Z4' OR
  p0000-massn EQ 'Z3' AND
( p0001-persk EQ  'U0' OR
  p0001-persk EQ  'U3' ).
*     endda >= sy-datum and
*     begda <= sy-datum.
    rec_01-status = 'A'.
    WRITE p0000-begda TO rec_01-status_date.

  ENDIF.

*  if p0000-stat2 eq '1' or
*     p0000-massn eq 'ZC'.
*    rec_01-status = 'A'.
*concatenate p0000-begda+0(4) p0000-begda+4(4) into rec_01-status_date
*.
*
*  endif.

  CLEAR v_loadate.

  IF p0000-massn EQ 'ZC' AND
    ( p0000-massg EQ '05' OR
      p0000-massg EQ '12' OR
      p0001-persk EQ 'U0' OR
      p0001-persk EQ  'U3' ).

    rec_01-status = 'L'.

    v_loadate = p0000-begda.
    SUBTRACT 3 FROM v_loadate.

*    CONCATENATE v_loadate+0(4) v_loadate+4(4) INTO rec_01-status_date.
    WRITE v_loadate TO rec_01-status_date.

  ELSEIF
  p0000-massn EQ 'ZC' AND
  ( p0000-massg EQ '02' OR
    p0000-massg EQ '03' OR
    p0000-massg EQ '06' OR
    p0000-massg EQ '07' OR
    p0000-massg EQ '08' OR
    p0000-massg EQ '09' OR
    p0000-massg EQ '10' OR
*   p0000-massg EQ '11' OR                                  "UD1K952586
*    p0000-massg eq '13' or " comment out by ig.moon 4/19/2010
    p0000-massg EQ '15' OR
    p0001-persk EQ  'U0' OR
    p0001-persk EQ  'U3' ).

    rec_01-status = 'L'.
    WRITE p0000-begda TO rec_01-status_date.

  ENDIF.

**********************************
  CLEAR v_loadate.

  IF p0000-massn EQ 'ZB' AND
       p0000-massg EQ '07' .

    rec_01-status = 'L'.
    v_loadate = p0000-begda.
    SUBTRACT 3 FROM v_loadate.

    WRITE v_loadate TO rec_01-status_date.

  ELSEIF
  p0000-massn EQ 'ZB' AND
  ( p0000-massg EQ '06' OR
    p0000-massg EQ '05' OR
    p0000-massg EQ '10' OR
*   p0000-massg EQ '11' OR                                  "UD1K952586
    p0000-massg EQ '12' OR
    p0000-massg EQ '13' ).

    rec_01-status = 'L'.
    WRITE p0000-begda TO rec_01-status_date.
  ENDIF.

  IF p0000-massn = 'ZE' OR
     p0000-massn = 'ZX' OR
     p0000-massn = 'ZW' OR
     p0000-massn = 'ZI' OR
     p0000-massn = 'Z5'.

    rec_01-status = 'T'.
    WRITE p0000-begda TO rec_01-terminatdate.
    WRITE p0000-begda TO rec_01-status_date.

    IF $date <= p0000-begda.
    ELSE.
      CLEAR rec_01.
      EXIT.
    ENDIF.

  ENDIF.

*-----------------------------------------------------------------------
* Check to see if Retiree has deceased.  Then override status
*-----------------------------------------------------------------------
  IF p0000-massn EQ 'ZY'.
    rec_01-status = 'D'.

    WRITE p0000-begda TO rec_01-status_date.
    WRITE p0000-begda TO rec_01-terminatdate.

    IF p0000-begda <= $date.
      CLEAR rec_01.
      EXIT.
    ENDIF.

  ENDIF.


**HIREDATE
*  PERFORM get_0041_date USING 'Z2' w_dat ret.
  PERFORM get_0041_date USING 'Z1' w_dat ret.
  IF ret <> 0.
*    perform get_0041_date using 'Z1' w_dat ret.
  ENDIF.
  IF ret = 0.
    WRITE  w_dat TO rec_01-hiredate.
  ELSE.                                     "Still send, just log error
    PERFORM log_error USING pernr-pernr 'HIR' '01' ' '.
  ENDIF.

  IF p0000-massn = 'Z6'.
    WRITE p0000-begda TO rec_01-rehiredate.
  ENDIF.

  rec_01-term_reason = ''.

*Salary Class
  DATA: w_ansal_nodec(16) TYPE p DECIMALS 2.

*BASE-------------------------------------------------------
*---determine amount here
  CLEAR pa0008-endda.
  CLEAR pa0008-begda.

* UD1K942066 - 11/05/2007 by IG.MOON {
  CLEAR pa0008-bet01.
* }

  SELECT * FROM pa0008
  WHERE pernr = pernr-pernr
  AND endda <= '99991231'
* UD1K942066 - 11/05/2007 by IG.MOON {
*  and begda le p_datum.
  AND begda LE end_of_pay_period
  ORDER BY endda.
    IF pa0008-endda GE end_of_pay_period.
      EXIT.
    ENDIF.
* }
  ENDSELECT.

  IF pa0008-endda <= '99991231'
  AND ( pa0008-trfgb = 'H1' OR
        pa0008-trfgb = 'H2' OR
        pa0008-trfgb = 'H3' OR
        pa0008-trfgb = 'H9' ).

    w_ansal_nodec = pa0008-ansal.
*    w_ansal_nodec = ( pa0008-ansal * 100 )." 26.
*    UNPACK w_ansal_nodec   TO rec_01-comp_amt.
    MOVE w_ansal_nodec   TO rec_01-comp_amt.

  ELSEIF
      pa0008-endda <= '99991231'
    AND ( pa0008-trfgb = 'H4' OR
          pa0008-trfgb = 'H5' ).   "2011.08.05 change by kim.yn 01

*////////////////////////////////////////////////////////////
* block 1 { This block is needed to disable after HR' confirm
    SELECT * FROM t510  WHERE
     trfgb = pa0008-trfgb
AND trfgr = pa0008-trfgr
AND trfst = pa0008-trfst
AND endda <= '99991231'

* UD1K942583 - 01/04/2008 by IG.MOON {
AND trfkz = '1'
* }

* UD1K942066 - 11/05/2007 by IG.MOON {
  ORDER BY endda.
      IF t510-endda GE end_of_pay_period.
        EXIT.
      ENDIF.

    ENDSELECT.
    CHECK sy-subrc = 0.
    w_ansal_nodec = ( t510-betrg * 2080 ) .
* } -- end of block 1
*////////////////////////////////////////////////////////////

* UD1K942066 - 11/05/2007 by IG.MOON {
    IF NOT pa0008-bet01 IS INITIAL.
      w_ansal_nodec = ( pa0008-bet01 * 2080 ) .
    ENDIF.
* }

*    UNPACK w_ansal_nodec   TO rec_01-comp_amt.
    MOVE w_ansal_nodec   TO rec_01-comp_amt.
  ELSE.
    rec_01-comp_amt = '000000000'.

  ENDIF.

*---determine date here
  WRITE p0008-begda TO rec_01-comp_date.

  TRANSLATE rec_01 TO UPPER CASE.
*---determine Salary basis
  LOOP AT p0001
      WHERE begda LE p_datum
        AND endda GE p_datum.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0 AND
    p0001-abkrs = '11'.
    rec_01-comp_code = 'B'.
  ELSEIF
    p0001-abkrs = '13'.
    rec_01-comp_code = 'M'.
  ENDIF.
****************
**Determine Work State
*
*  LOOP AT p0208
*
*      WHERE begda LE p_datum
*        AND endda GE p_datum.
*    EXIT.
*  ENDLOOP.
*
*  IF sy-subrc = 0.
*    MOVE p0208-wtart TO rec_01-worktax.
*  ENDIF.
***************
**Determine highly compensated.
*  LOOP AT p0375
*        WHERE hicmp = 'X'.
*    EXIT.
*  ENDLOOP.
*  IF sy-subrc = 0.
*    rec_01-highce = 'Y'.
*  ELSE.
*    rec_01-highce = 'N'.
*
*  ENDIF.
*
************************
*Determine E-mail address
*************************

* by ig.moon 2/26/2008 {

*  delete p0105 where usrty ne '0010'.

* }

*  loop at p0105
*            where begda le p_datum
*            and endda ge p_datum.
**Employee Email.
**    if p0105-usrty = '0010'.
**      move p0105-usrid_long to rec_01-eemail.
**      translate rec_01-eemail to lower case.
**    endif.
*
** by ig.moon 2/26/2008 {
*
*
****Supervisor Email
***    if p0105-usrty = '0012'.
***      move p0105-usrid to rec_01-eemail.
***      translate rec_01-eemail to lower case.
***    endif.
***
****    if p0105-usrty.
****    else.
****      rec_01-eemail = 'janechamness@hmmausa.com'.
****      translate rec_01-eemail to lower case.
****    endif.
*
****Supervisor I.D
***    if p0105-usrty = '0013'.
***      move p0105-usrid to rec_01-super_id.
***      translate rec_01-super_id to lower case.
***    endif.
****    else.
****      rec_01-super_id = '103494'.
****      translate rec_01-super_id to lower case.
****    endif.
**    perform get_super_name using rec_01-super_id.
*
** }
*
*  endloop.

* by ig.moon 2/26/2008 {

  DATA :
  fields TYPE rhtext_field OCCURS 0 WITH HEADER LINE ,
  molga TYPE molga ,
  p0001_af LIKE p0001_af ,
  $strlen TYPE i.

  REFRESH fields .
  APPEND 'OMNGR_NR' TO fields .

  IF NOT pnpxabkr  IS INITIAL.
    molga = pnpxabkr.
  ELSE.
    molga = '11'.
  ENDIF.

  CLEAR p0105.  REFRESH p0105.

  CALL FUNCTION 'RPAQ_GET_AF_0001'
       EXPORTING
            c_it0001  = p0001
            begda_it  = p_datum
            endda_it  = '99991231'
            begda_sel = p_datum
            endda_sel = '99991231'
            tclas     = 'A'
            molga     = molga
       IMPORTING
            af_values = p0001_af
       TABLES
            af_fields = fields
       EXCEPTIONS
            OTHERS    = 1.

*  IF sy-subrc EQ 0 AND NOT p0001_af-omngr_nr IS INITIAL.

*    MOVE p0001_af-omngr_nr TO rec_01-super_id.
*    PERFORM get_super_name USING rec_01-super_id.
*
*    rp-read-infotype rec_01-super_id 0105 p0105 p_datum p_datum.
*
*    DELETE p0105 WHERE usrty NE '0010'.
*
*    LOOP AT p0105
*              WHERE begda LE p_datum
*              AND endda GE p_datum.
*
*      IF p0105-usrty = '0010'.
*        MOVE p0105-usrid_long TO rec_01-emailaddr.
*        TRANSLATE rec_01-emailaddr TO LOWER CASE.
*      ENDIF.
*
*    ENDLOOP.
*
*  ELSE.

  rp-read-infotype pernr-pernr 0105 p0105 p_datum p_datum.

  LOOP AT p0105
            WHERE begda LE p_datum
            AND endda GE p_datum.

*Employee Email.
    IF p0105-usrty = '0010'.
      MOVE p0105-usrid_long TO rec_01-emailaddr.
      TRANSLATE rec_01-emailaddr TO LOWER CASE.
    ENDIF.

** On 02/07/13
*Supervisor Email
*    IF p0105-usrty = '0012'.
*      MOVE p0105-usrid TO rec_01-super_email.
*      TRANSLATE rec_01-super_email TO LOWER CASE.
*    ENDIF.
*
**Supervisor I.D
*    IF p0105-usrty = '0013'.
*      MOVE p0105-usrid TO rec_01-super_id.
*      TRANSLATE rec_01-super_id TO LOWER CASE.
*      PERFORM get_super_name USING rec_01-super_id.
*    ENDIF.
** End On 02/07/13

  ENDLOOP.

*  ENDIF.

* }

  $strlen = strlen( rec_01-super_id ).
  IF $strlen EQ 6.
    CONCATENATE '00' rec_01-super_id INTO rec_01-super_id.
  ENDIF.

  PERFORM z300_append_outfil USING const rec_01 w_cntr.
*  IF rec_01-status EQ 'ML'.
*    rec_01-weekhrs = '40.00'.
*  ENDIF.

*  IF rec_01-status EQ 'A'.

** On 02/07/13
  rec_01-super_email = 'jamie.spaulding@hmmausa.com'.
  rec_01-hrm_email = 'julie.pitts@hmmausa.com'.
** End of change

  APPEND rec_01.

ENDFORM.                                   "<<<<End REC_01

*&----------------------------------------------------------
*&     Form get_super_name
*&----------------------------------------------------------
FORM get_super_name USING rec_01-super_id.

  CLEAR : s_vorna, s_nachn.

  e_pernr = rec_01-super_id.
  SELECT SINGLE vorna nachn INTO (s_vorna, s_nachn)
    FROM pa0002 WHERE pernr = e_pernr
*    and begda le p_datum.
        AND endda = '99991231' .
  MOVE s_nachn TO rec_01-supervisor_last.
  MOVE s_vorna TO rec_01-supervisor_first.
ENDFORM.

*&----------------------------------------------------------
*&     Form get_super_dept_devi
*&----------------------------------------------------------
FORM get_super_dept_devi USING rec_01-f14.
  s_pernr = rec_01-f14.
  SELECT SINGLE kostl INTO l_kostl
    FROM pa0001 WHERE pernr = s_pernr
*    and begda le p_datum.
        AND endda = '99991231' .

  IF
*case sy-subrc.
l_kostl = 'MXSX1B' OR
l_kostl = 'MXSX51' OR
l_kostl = 'MXSX53' OR
l_kostl = 'MXBXB1' OR
l_kostl = 'MXBXC1' OR
l_kostl = 'MXBXF1' OR
l_kostl = 'MXBXM1' OR
l_kostl = 'MXPX10' OR
l_kostl = 'MXPX20' OR
l_kostl = 'MXPX30' OR
l_kostl = 'MXPX50' OR
l_kostl = 'MXTX10' OR
l_kostl = 'MXTX11' OR
l_kostl = 'MXTX13' OR
l_kostl = 'MXTX14' OR
l_kostl = 'MXTX15' OR
l_kostl = 'MXTX17' OR
l_kostl = 'MXTX19' OR
l_kostl = 'MXTX51' OR
l_kostl = 'MXTX53' OR
l_kostl = 'MXTX12' OR
l_kostl = 'MXEX13' OR
l_kostl = 'MXEX15' OR
l_kostl = 'MXEX17' OR
l_kostl = 'MXEXA1' OR

* UD1K941878 - by IG.MOON 10/15/07 {
l_kostl = 'MXEY13' OR
l_kostl = 'MXEY15' OR
l_kostl = 'MXEY17' OR
l_kostl = 'MXEYA1' OR
l_kostl = 'MXEZ13' OR                                       "UD1K954027
l_kostl = 'MXEZ15' OR                                       "UD1K954027
l_kostl = 'MXEZ17' OR                                       "UD1K954027
l_kostl = 'MXEZA1'.                                         "UD1K954027
* }

*************
    PERFORM initial.
*    move l_kostl to rec_01-costcen.
  ELSE.
    MOVE '000000' TO rec_01-workcen.

    output_string = l_kostl.

    DO counter TIMES.

      REPLACE xy WITH space INTO output_string .

    ENDDO.


    WRITE output_string LEFT-JUSTIFIED TO rec_01-costcen .
  ENDIF.

***********************
**reading the description of kostl
*  select SINGLE ktExt into kostl_des
*    from cskT
*    where kostl = l_kostl.
**    AND     DATBI GE SY-DATUM.
*
*  move kostl_des to rec_01-workcen.
*  clear l_kostl.
*  clear kostl_des.
*
ENDFORM.
****************************************
*Form Initial to Translate cost center
***************************************
FORM initial.

  CASE l_kostl .
    WHEN 'MXSX1B' OR
         'MXSX51' OR
         'MXSX53' .
      MOVE '55001' TO rec_01-costcen.
      MOVE l_kostl TO rec_01-workcen.

    WHEN 'MXBXB1' OR
         'MXBXC1' OR
         'MXBXF1' OR
         'MXBXM1'.
      MOVE '55002' TO rec_01-costcen.
      MOVE l_kostl TO rec_01-workcen.

    WHEN 'MXPX10' OR
         'MXPX20' OR
         'MXPX30' OR
         'MXPX50'.
      MOVE '55003' TO rec_01-costcen.
      MOVE l_kostl TO rec_01-workcen.

    WHEN 'MXTX10' OR
         'MXTX11' OR
         'MXTX12' OR
         'MXTX13' OR
         'MXTX14' OR
         'MXTX15' OR
         'MXTX17' OR
         'MXTX19' OR
         'MXTX51' OR
         'MXTX53'.
      MOVE '55004' TO rec_01-costcen.
      MOVE l_kostl TO rec_01-workcen.

    WHEN "---- by ig.moon 4/5/2010 'MXTX12' or
         'MXEX13' OR
         'MXEX15' OR
         'MXEX17' OR
         'MXEXA1' OR
         'MXEZ13' OR                                        "UD1K954027
         'MXEZ15' OR                                        "UD1K954027
         'MXEZ17' OR                                        "UD1K954027
         'MXEZA1'.                                          "UD1K954027
      MOVE '55005' TO rec_01-costcen.
      MOVE l_kostl TO rec_01-workcen.

* UD1K941878 - by IG.MOON 10/15/07 {
    WHEN 'MXEY13' OR
         'MXEY15' OR
         'MXEY17' OR
         'MXEYA1'.
      MOVE '55015' TO rec_01-costcen.
      MOVE l_kostl TO rec_01-workcen.
* }

  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM get_salar_dept                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  REC_01-F14                                                    *
*---------------------------------------------------------------------*

FORM get_salar_dept USING rec_01-f14.

******
*SALARY
******
  s_pernr = rec_01-f14.
  SELECT SINGLE persk INTO l_persk
    FROM pa0001 WHERE pernr = s_pernr
*    and persk = 'U0'
*    and begda le p_datum.
        AND endda = '99991231' .

  __cls it_calc.
  LOOP AT p0000.
    MOVE-CORRESPONDING p0000 TO it_calc.
    APPEND it_calc.
  ENDLOOP.

  IF l_persk = 'U2'.

*******************
*clear v_loadate.

    LOOP AT p0000
*    where endda le '99991231'
       WHERE pernr = s_pernr.

      IF p0000-massn = 'ZD' AND
         p0000-endda LE '99991231'.
        rec_01-status = 'A'.
        WRITE p0000-begda TO rec_01-status_date.
      ELSEIF
           p0000-massn EQ 'ZB' AND
           p0000-massg EQ '07' AND
           p0000-endda LE '99991231'.

        rec_01-status = 'L'.

        v_loadate = p0000-begda.
        SUBTRACT 3 FROM v_loadate.

        WRITE v_loadate TO rec_01-status_date.

      ELSEIF
            p0000-massn = 'ZB' AND
             p0000-endda LE '99991231'.
        rec_01-status = 'L'.

*  CONCATENATE p0000-begda+0(4) p0000-begda+4(4) INTO rec_01-status_date
*.
        WRITE p0000-begda TO rec_01-status_date.

      ELSEIF

        p0000-massn = 'ZB' AND
                 p0000-endda LE '99991231' AND
                ( p0000-massg EQ '05' OR
                 p0000-massg EQ '06' OR
                 p0000-massg EQ '10' OR
*                 p0000-massg eq '11' or    "military
                 p0000-massg EQ '12' OR
                 p0000-massg EQ '13' ).


        rec_01-status = 'L'.

*  CONCATENATE p0000-begda+0(4) p0000-begda+4(4) INTO rec_01-status_date
*.
        WRITE p0000-begda TO rec_01-status_date.

*********
*Military/Salary
*************
      ELSEIF

            p0000-massn = 'ZC' AND                          "UD1K952586
*           p0000-massn = 'ZB' AND                          "UD1K952586
            p0000-massg EQ '11' AND
                     p0000-endda LE '99991231'.

** Changd on 02/07/13
*        rec_01-status = 'ML'.
        rec_01-status = 'L'.
** End

        WRITE p0000-begda TO rec_01-status_date.

      ELSEIF p0000-massn = 'Z0' OR
             p0000-massn = 'Z9' AND
             p0000-endda LE '99991231'.

        rec_01-status = 'A'.
        WRITE p0000-begda TO rec_01-status_date.
      ELSEIF p0000-massn = 'Z6' AND
             p0000-endda LE '99991231'.

        rec_01-status = 'A'.
        WRITE p0000-begda TO rec_01-status_date.
      ENDIF.

    ENDLOOP.
************************************************************************
*HOURLY/SALARY HOURLY
*********************
  ELSEIF
      l_persk = 'U0' OR
      l_persk = 'U3'.

    LOOP AT p0000 WHERE endda LE '99991231'.
*       where pernr = s_pernr.
**************************************
      IF
       p0000-massn EQ 'ZD'
       AND p0000-endda LE '99991231'.
        rec_01-status = 'A'.
*  CONCATENATE p0000-begda+0(4) p0000-begda+4(4) INTO rec_01-status_date
*.
        WRITE p0000-begda TO rec_01-status_date.

      ELSEIF

         p0000-massn EQ 'ZC' AND
          ( p0000-massg EQ '05' OR
            p0000-massg EQ '12' ) AND
            p0000-endda LE '99991231'.


        rec_01-status = 'L'.

        v_loadate = p0000-begda.
        SUBTRACT 3 FROM v_loadate.

*      CONCATENATE v_loadate+0(4) v_loadate+4(4) INTO rec_01-status_date
*.
        WRITE v_loadate TO rec_01-status_date.


      ELSEIF
      p0000-massn EQ 'ZC' AND
      ( p0000-massg EQ '02' OR
        p0000-massg EQ '03' OR
        p0000-massg EQ '06' OR
        p0000-massg EQ '07' OR
        p0000-massg EQ '08' OR
        p0000-massg EQ '09' OR
        p0000-massg EQ '10' OR
*        p0000-massg eq '11' or    "Military
*        p0000-massg eq '13' "comment out by ig.moon 4/19/2010
        p0000-massg EQ '15' "added by ig.moon 4/19/2010
        ) AND
        p0000-endda LE '99991231'.


        rec_01-status = 'L'.
*  CONCATENATE p0000-begda+0(4) p0000-begda+4(4) INTO rec_01-status_date
*.
        WRITE p0000-begda TO rec_01-status_date.

*******
***Military Leave
*********
      ELSEIF
         p0000-massn EQ 'ZC' AND
                    p0000-massg EQ '11'
                    AND p0000-endda LE '99991231'.

** Changd on 02/07/13
*        rec_01-status = 'ML'.
        rec_01-status = 'L'.
** End

        WRITE p0000-begda TO rec_01-status_date.

      ELSEIF   p0000-massn = 'Z0' OR
               p0000-massn = 'Z9' AND
               p0000-endda LE '99991231'.
        rec_01-status = 'A'.
*  CONCATENATE p0000-begda+0(4) p0000-begda+4(4) INTO rec_01-status_date
*.
        WRITE p0000-begda TO rec_01-status_date.
      ELSEIF p0000-massn = 'Z6' AND
             p0000-endda LE '99991231'.

        rec_01-status = 'A'.
        WRITE p0000-begda TO rec_01-status_date.
      ENDIF.

    ENDLOOP.
  ENDIF.

  IF rec_01-status = 'L'.
    CLEAR rec_01-status_date.
    PERFORM get_cnt TABLES it_calc
                    CHANGING rec_01-status_date.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z900_WRITE_FILE
*&---------------------------------------------------------------------*
* This form copies the data from the internal table to the actual output
* file.
*----------------------------------------------------------------------*
FORM z900_write_file USING p_rc.              "<<<< JMS012 add P_RC.

  DATA l_lines TYPE i.

  p_rc = 0.     "<<<< JMS013: Reset in case it comes in with a non-zero.


  DESCRIBE TABLE rec_01 LINES l_lines.

  g_tot_ee = g_rej_ee + l_lines.

  SKIP.
  WRITE: / 'S U M M A R Y'.
  ULINE.
  WRITE: / 'Number of employees processed: ', g_tot_ee,
        / 'Number of employees rejected (no actions found): ', g_rej_ee.


*describe table rec_01 lines l_lines.

  WRITE: / 'Number of records to be written to file: ',
         l_lines.

  SKIP.

  IF p_disp = 'X'.
    SKIP.
    WRITE: /1 'Display Hartford Indicative Outbound file only'.
    SKIP.
    LOOP AT rec_01.
      WRITE: /1 rec_01.
    ENDLOOP.
  ENDIF.
*----------------------------------------------------------
* If Display only, then do not write a file
*----------------------------------------------------------
  CHECK p_disp = space.

******Put file in c: drive
  CONCATENATE 'c:\temp\' 'hartford_out' sy-datum+4(2)
  sy-datum+6(2) sy-datum+0(4) sy-uzeit'.txt' INTO p_filepc.



*****Download test file to Presentation Server if selected
  IF p_pc = 'X'.
    CALL FUNCTION 'WS_DOWNLOAD'
         EXPORTING
              filename        = p_filepc
              filetype        = 'DAT'
         TABLES
              data_tab        = rec_01
         EXCEPTIONS
              file_open_error = 1
              OTHERS          = 2.
    IF sy-subrc <> 0.
      p_rc = sy-subrc.
      SKIP 2.
      WRITE:   '**** ERROR OPENING/DOWNLOADING TO PC FILE. ****'.
      WRITE: / '**** BASELINE TABLE & EVTS TABLE NOT UPD.  ****'.
      WRITE: / '**** RETURN CODE: ', p_rc, '****'.
    ENDIF.
  ENDIF.


******Put file in UNIX
  CONCATENATE '/usr/sap/401K/' 'Hartford_out' sy-datum+4(2)
  sy-datum+6(2) sy-datum+0(4) sy-uzeit'.txt' INTO p_fileun.

*Write transmission file to Application Server if selected.
****UNCOMMENT THIS SECTION TO CREATE A FILE IN UNIX****
***********************************************************
*  if p_unix = 'X'.
*    open dataset p_fileun for output in text mode.
*    if sy-subrc <> 0.
*      write: '***ERROR opening file', p_fileun, 'rc=', sy-subrc.
*      stop.
*    endif.
*
*    loop at rec_01.
*      transfer rec_01 to p_fileun.
*
*      if sy-subrc <> 0.
*        write: /'***ERROR writing to file', p_fileun, 'rc=', sy-subrc.
*        write: /'Record:'.
*        write: / p_fileun.
*        stop.
*      endif.
*    endloop.
*    close dataset p_fileun.
*    if sy-subrc <> 0.
*      write: /'***ERROR closing file', p_fileun, 'rc=', sy-subrc.
*      stop.
*    endif.
*  endif.
*

************************************************************************
**RFC FUNCTION CALL. .
**Developed by Mohammad Haseeb to send the file via EAI.
************************************************************************

*  call function 'Z_HR_HARTFORD'
*       destination 'WMHR01'
**        TABLES ZSTRUCHART = it_rfchart .
*        tables zstruchart = rec_01.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_pme51                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

************************************
FORM error_handling USING pernr arbgb msgty msgno
                          msgv1 msgv2 msgv3 msgv4.
  CLEAR error_tab.
  error_tab-pernr = pernr.
  error_tab-arbgb = arbgb.
  error_tab-msgty = msgty.
  error_tab-msgno = msgno.
  error_tab-msgv1 = msgv1.
  error_tab-msgv2 = msgv2.
  error_tab-msgv3 = msgv3.
  error_tab-msgv4 = msgv4.
  APPEND error_tab.
  REJECT.
ENDFORM.

***********************************

FORM fill_graf_data USING fgd_ident fgd_perio fgd_anzhl.
  READ TABLE graf_tab WITH KEY perio = fgd_perio
                               ident = fgd_ident.
  IF sy-subrc EQ 0.
    graf_tab-anzhl = graf_tab-anzhl + fgd_anzhl.
    MODIFY graf_tab INDEX sy-tabix.
  ELSE.
    graf_tab-perio = fgd_perio.
    graf_tab-ident = fgd_ident.
    graf_tab-anzhl = fgd_anzhl.
    APPEND graf_tab.
  ENDIF.


ENDFORM.
*----------------------------------------------------------------------*
*                      Unterprogramme                                  *
*----------------------------------------------------------------------*
FORM progress_indicator USING prog_text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text   = prog_text
       EXCEPTIONS
            OTHERS = 1.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM fill_i549q                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  fill_begda                                                    *
*  -->  fill_endda                                                    *
*---------------------------------------------------------------------*
FORM fill_i549q USING fill_begda fill_endda.

  IF fill_begda EQ it_range-low AND fill_endda EQ it_range-high.
    fill_begda = fill_endda = sy-datum.
  ENDIF.

  CALL FUNCTION 'HR_PAYROLL_PERIODS_GET'
       EXPORTING
            get_begda       = it_range-low
            get_endda       = it_range-high
            get_permo       = rptime_period
       TABLES
            get_periods     = i549q
       EXCEPTIONS
            no_valid_permo  = 1
            no_period_found = 2.

  CASE sy-subrc.
    WHEN 1.
      PERFORM error_handling USING space         '72'  'E'  '025'
                                   rptime_period space space space.
      STOP.
    WHEN 2. STOP.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
********************************
*form process_saldo.
************************************
FORM process_saldo.

*  clear : time_data_saldo[], time_data_saldo.
*  clear : graf_tab[], graf_tab.

  MOVE-CORRESPONDING p0001 TO time_data_saldo.
  MOVE-CORRESPONDING p0001_text TO time_data_saldo.       "XPXPH4K013099
  MOVE-CORRESPONDING p0007 TO time_data_saldo.
  MOVE-CORRESPONDING schkz_text TO time_data_saldo.       "XPXPH4K013099
  time_data_saldo-gesch = p0002-gesch.
*  PERFORM transform_gender USING time_data_saldo-gesch.
  time_data_saldo-clstb2_per = act_period.
* BEGIN OF UD1K951420
  LOOP AT saldo WHERE ztart = '9201'
                   OR ztart = '9202'
                   OR ztart = '9203'
                   OR ztart = '9301'
                   OR ztart = '9302'
                   OR ztart = '9303'
                   OR ztart = '9304'
                   OR ztart = '9305'
                   OR ztart = '9306'
                   OR ztart = '9307'
                   OR ztart = '9406'
                   OR ztart = '9407'
                   OR ztart = '9408'
                   OR ztart = '9501'
                   OR ztart = '9502'
                   OR ztart = '9503'
                   OR ztart = '1033'.

*  LOOP AT saldo WHERE ztart = '9201'
*                   OR ztart = '9202'
*                   OR ztart = '9203'
*                   OR ztart = '9301'
*                   OR ztart = '9302'
*                   OR ztart = '9303'
*                   OR ztart = '9304'
*                   OR ztart = '9305'
*                   OR ztart = '9306'
*                   OR ztart = '9307'
*                   OR ztart = '9406'
*                   OR ztart = '9407'
*                   OR ztart = '9408'
*                   OR ztart = '9501'
*                   OR ztart = '9502'
*                   OR ztart = '9503'.
* END OF UD1K951420
*

*LOOP AT saldo WHERE ztart IN saldi.
*    time_data_saldo-pernr = pernr-pernr.
*    time_data_saldo-ename = p0001-ename.
*    time_data_saldo-ztart = saldo-ztart.
*

    time_data_saldo-pernr = pernr-pernr.
    time_data_saldo-ename = p0001-ename.
    time_data_saldo-ztart = saldo-ztart.
*    PERFORM re555b USING t001p-mobde saldo-ztart time_data_saldo-ztext.
    time_data_saldo-anzhl = saldo-anzhl.
    APPEND time_data_saldo.
*-----Bereitstellen der Informationen für Präsentationsgrafik
    PERFORM fill_graf_data USING saldo-ztart act_period saldo-anzhl."HAS
*  perform fill_graf_tab.

  ENDLOOP.
* BEGIN OF UD1K952967
  LOOP AT saldo_inpat_kronos WHERE ztart = '0002'
                                OR ztart = '9500'.
    time_data_saldo-pernr = pernr-pernr.
    time_data_saldo-ename = p0001-ename.
    time_data_saldo-ztart = saldo_inpat_kronos-ztart.
    IF saldo_inpat_kronos-ztart = '0002'.
      time_data_saldo-anzhl = saldo_inpat_kronos-anzhl.
    ELSE.
* BEGIN OF UD1K953009
      saldo_inpat_kronos-anzhl = saldo_inpat_kronos-anzhl * -1.
      time_data_saldo-anzhl = saldo_inpat_kronos-anzhl.
* END OF UD1K953009
    ENDIF.
    APPEND time_data_saldo.
    PERFORM fill_graf_data USING saldo_inpat_kronos-ztart
                                 act_period
                                 saldo_inpat_kronos-anzhl.
  ENDLOOP.
* END OF UD1K952967
ENDFORM.

*---------------------------------------------------------------------*
*       FORM z930_report_errors                                       *
*---------------------------------------------------------------------*
* This form writes out all of the errors that have been logged.
*---------------------------------------------------------------------*
FORM z930_report_errors.

  IF NOT i_error[] IS INITIAL.
    SORT i_error BY type pernr id.
* JMS003 - temporarily delete certain messages from the log:
    DELETE i_error WHERE type = 'ADJ'.
    DELETE i_error WHERE type = 'TTT'.
    DELETE i_error WHERE type = 'WTH'.

    SKIP.
    WRITE: / '* * * E R R O R   L O G * * *'.
    ULINE.
    LOOP AT i_error.
      AT NEW type.
        SKIP.
        CASE i_error-type.
          WHEN '000'.
            WRITE / 'Error: No valid infotype 0000 found for:'.
          WHEN '208'.
            WRITE / 'Error: No valid infotype 0208 found for:'.
          WHEN '001'.
            WRITE / 'Error: No valid infotype 0001 found for:'.
          WHEN '002'.
            WRITE / 'Error: No valid infotype 0002 found for:'.
          WHEN '006'.
            WRITE / 'Error: No valid infotype 0006 found for:'.
          WHEN '008'.
            WRITE / 'Error: No valid infotype 0008 found for:'.
* JMS006 - new type: needs intervention - status change/action...
          WHEN '041'.
            WRITE / 'Error: No valid infotype 0041 found for:'.
          WHEN '171'.
            WRITE / 'Error: No valid infotype 0171 found for:'.
          WHEN 'BEN'.
            WRITE / 'Error: Invalid 0171 group/status combo. for:'.
          WHEN 'RIC'.
            WRITE / 'Error: Could not determine RIC code for:'.

          WHEN '036'. " not used
            WRITE /
            'Warning: check 0171/actions. Chg of status - 36 rec for:'.
          WHEN 'TER'. " not used
            WRITE / 'Warning: Could not find term date for:'.
          WHEN 'SAL'. " not used
            WRITE / 'Warning: Annual salary of $0. Check 0008 for:'.

          WHEN 'HIR'.
            WRITE / 'Warning: Could not find hire date for:'.
* JMS006 - new type: dummy XFR created due to elig. status change.
          WHEN 'DUM'.
*            write /
*    'Warning: These Employees are Terminated and did not get selected '
*.

          WHEN 'ODD'.
            WRITE: 'Problem w/ HA status - alternate logic section.'.
*          WHEN 'ADJ'.
*            WRITE / 'Warning: Could not find adj. hire date for:'.
          WHEN 'EVE'.
            WRITE /
           'Error reading proc. events table ZHHART_PROC_EVT'.
* Added new error types:
          WHEN 'AAA'.
            WRITE / 'Error with actions - no currently valid action.'.
*          WHEN 'TTT'.
*            WRITE /
*           'Could not find Z5 for withdrawn EE; no recs. created.'.
*          WHEN 'WTH'.
*            WRITE /
*           'Withdrawn EE - BG record has changed data - force resend?'.
        ENDCASE.
        WRITE: /5 'Emp.#.', AT 20 'Rec. Type', AT 30 'Misc. Info.'.
        ULINE.
      ENDAT.

      IF i_error-type NE 'DUM'.
        WRITE: /5 i_error-pernr, AT 20 i_error-id, AT 30 i_error-var.
      ENDIF.

    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_end_of_paydate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_end_of_paydate.

  DATA $fpper LIKE pc261-fpper.
  DATA ip_beg LIKE sy-datum.

  CLEAR end_of_pay_period.

  IF NOT pnpendda  IS INITIAL.
    end_of_pay_period = pnpendda.
  ENDIF.

  IF NOT pnpendps  IS INITIAL.
    end_of_pay_period = pnpendps.
  ENDIF.

  CHECK NOT : pnppabrj IS INITIAL,
              pnppabrp IS INITIAL,
              pnpxabkr IS INITIAL.
  CONCATENATE pnppabrj pnppabrp INTO $fpper.

  PERFORM check_abkrs_pabrp_pabrj USING
                        $fpper
                        pnpxabkr
                        CHANGING ip_beg end_of_pay_period.


ENDFORM.                    " get_end_of_paydate
*&---------------------------------------------------------------------*
*&      Form  check_abkrs_pabrp_pabrj
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$FPPER  text
*      -->P_P_ABKRS  text
*      <--P_IP_BEG  text
*      <--P_IP_END  text
*----------------------------------------------------------------------*
FORM check_abkrs_pabrp_pabrj USING
        p_period
        value(iv_abkr_cal)
        CHANGING p_iv_fp_beg p_iv_fp_end.

  DATA lv_pnpxabkr TYPE abkrs.
  DATA lv_permo LIKE t549a-permo.
  DATA ov_abrp_cal LIKE t569v-pabrp.
  DATA ov_abrj_cal LIKE t569v-pabrj.

  DATA $p_iv_fp_beg LIKE sy-datum.
  DATA $p_iv_fp_end LIKE sy-datum.

  lv_pnpxabkr = iv_abkr_cal. "the types are different.

  ov_abrp_cal = p_period+4(2).
  ov_abrj_cal = p_period(4).

  CALL FUNCTION 'PA03_PERIODDATES_GET'
       EXPORTING
            f_abkrs               = lv_pnpxabkr
       IMPORTING
            f_permo               = lv_permo
            f_current_begda       = $p_iv_fp_beg
            f_current_endda       = $p_iv_fp_end
       CHANGING
            f_current_period      = ov_abrp_cal
            f_current_year        = ov_abrj_cal
       EXCEPTIONS
            pcr_does_not_exist    = 1
            abkrs_does_not_exist  = 2
            period_does_not_exist = 3
            OTHERS                = 4.

  IF NOT $p_iv_fp_beg IS INITIAL.
    p_iv_fp_beg = $p_iv_fp_beg.
  ENDIF.
  IF NOT $p_iv_fp_end IS INITIAL.
    p_iv_fp_end = $p_iv_fp_end.
  ENDIF.

ENDFORM.                    " check_abkrs_pabrp_pabrj
*&---------------------------------------------------------------------*
*&      Form  process_saldo_week
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_saldo_week.
*  clear : time_data_saldo[], time_data_saldo.
*  clear : graf_tab[], graf_tab.

  MOVE-CORRESPONDING p0001 TO time_data_saldo.
  MOVE-CORRESPONDING p0001_text TO time_data_saldo.       "XPXPH4K013099
  MOVE-CORRESPONDING p0007 TO time_data_saldo.
  MOVE-CORRESPONDING schkz_text TO time_data_saldo.       "XPXPH4K013099
  time_data_saldo-gesch = p0002-gesch.
*  PERFORM transform_gender USING time_data_saldo-gesch.
  time_data_saldo-clstb2_per = act_period.

* BEGIN OF UD1K951420
  LOOP AT saldo WHERE ztart = '9201'
                   OR ztart = '9202'
                   OR ztart = '9203'
                   OR ztart = '9301'
                   OR ztart = '9302'
                   OR ztart = '9303'
                   OR ztart = '9304'
                   OR ztart = '9305'
                   OR ztart = '9306'
                   OR ztart = '9307'
                   OR ztart = '9406'
                   OR ztart = '9407'
                   OR ztart = '9408'
                   OR ztart = '9501'
                   OR ztart = '9403'
                   OR ztart = '1033'
                   OR ztart = '9500'.

*  LOOP AT saldo WHERE ztart = '9201'
*                   OR ztart = '9202'
*                   OR ztart = '9203'
*                   OR ztart = '9301'
*                   OR ztart = '9302'
*                   OR ztart = '9303'
*                   OR ztart = '9304'
*                   OR ztart = '9305'
*                   OR ztart = '9306'
*                   OR ztart = '9307'
*                   OR ztart = '9406'
*                   OR ztart = '9407'
*                   OR ztart = '9408'
*                   OR ztart = '9501'
*                   OR ztart = '9403'.
* END OF UD1K951420

    time_data_saldo-pernr = pernr-pernr.
    time_data_saldo-ename = p0001-ename.
    time_data_saldo-ztart = saldo-ztart.
*    PERFORM re555b USING t001p-mobde saldo-ztart time_data_saldo-ztext.
    time_data_saldo-anzhl = saldo-anzhl.
    APPEND time_data_saldo.
*-----Bereitstellen der Informationen für Präsentationsgrafik
    PERFORM fill_graf_data USING saldo-ztart act_period saldo-anzhl."HAS
*  perform fill_graf_tab.

  ENDLOOP.

ENDFORM.                    " process_saldo_week
*&---------------------------------------------------------------------*
*&      Form  get_cnt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CALC  text
*----------------------------------------------------------------------*
FORM get_cnt TABLES p_it_calc_pernr STRUCTURE it_calc
             CHANGING p_date.

  DATA : $begda TYPE begda,
         $endda TYPE endda,
         $tmpda TYPE endda,
         $cnt TYPE i,
         $flag1,$flag2.

  DATA  $$begda TYPE begda.

  DATA $ix TYPE i.
  DATA $ixp TYPE i.

  DELETE p_it_calc_pernr WHERE ( massn NE 'ZC' AND massn NE 'Z3' AND
massn NE 'ZB' ).

  SORT p_it_calc_pernr BY begda DESCENDING.

  READ TABLE p_it_calc_pernr INDEX 1.
  CHECK sy-subrc EQ 0.

  $begda = p_it_calc_pernr-begda.
  $endda = p_it_calc_pernr-endda.

  IF $endda < pn-endda.
    EXIT.
  ELSE.
  ENDIF.

  p_it_calc_pernr-flag = true.
  MODIFY p_it_calc_pernr INDEX 1 TRANSPORTING flag.

  LOOP AT p_it_calc_pernr FROM 2.

    $ix = sy-tabix.
    $tmpda = p_it_calc_pernr-endda + 1.

    IF $tmpda EQ $begda.
      p_it_calc_pernr-flag = true.
      MODIFY p_it_calc_pernr INDEX $ix TRANSPORTING flag.
      $begda = p_it_calc_pernr-begda.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.

  DELETE p_it_calc_pernr WHERE flag EQ false.

  __cls $it_calc_pernr.

  DATA: BEGIN OF it_final_reason OCCURS 0,
          massn TYPE massn,
          massg TYPE massg,
        END OF it_final_reason.

  LOOP AT p_it_calc_pernr.
    it_final_reason-massn = p_it_calc_pernr-massn.
    it_final_reason-massg = p_it_calc_pernr-massg.
    APPEND it_final_reason.
  ENDLOOP.

  SORT p_it_calc_pernr BY begda.

  DO 30 TIMES.
    READ TABLE p_it_calc_pernr INDEX 1.
    IF sy-subrc EQ 0.
      IF p_it_calc_pernr-massn EQ 'Z3'.
        DELETE p_it_calc_pernr INDEX 1.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  READ TABLE p_it_calc_pernr INDEX 1.
  $$begda = p_it_calc_pernr-begda.

  DO 30 TIMES.
    READ TABLE it_final_reason INDEX 1.
    IF sy-subrc EQ 0.

* Begin of HIS20094 - Fix the action type
*      IF it_final_reason-massn EQ 'Z3' AND (
*         it_final_reason-massg EQ '12' OR
*         it_final_reason-massg EQ '14' ).
* End of HIS20094

      IF it_final_reason-massn EQ 'Z3'.
        DELETE it_final_reason INDEX 1.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

  LOOP AT p_it_calc_pernr.
    $it_calc_pernr  = p_it_calc_pernr.
    CLEAR : $it_calc_pernr-flag,
            $it_calc_pernr-begda,
            $it_calc_pernr-endda.
    READ TABLE it_final_reason INDEX 1.
    $it_calc_pernr-massn = it_final_reason-massn.
    $it_calc_pernr-massg = it_final_reason-massg.
    COLLECT  $it_calc_pernr.
  ENDLOOP.

  $it_calc_pernr-begda = $$begda.

  IF $it_calc_pernr-endda IS INITIAL OR $it_calc_pernr-endda > pn-endda.
    $it_calc_pernr-cnt = pn-endda - $it_calc_pernr-begda.
  ELSE.
    $it_calc_pernr-cnt = $it_calc_pernr-endda - $it_calc_pernr-begda.
  ENDIF.

  MODIFY $it_calc_pernr INDEX 1 TRANSPORTING begda cnt.
  WRITE $$begda TO p_date.

ENDFORM.                    " get_cnt
*&---------------------------------------------------------------------*
*&      Form  create_it_mili_leave
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_it_mili_leave.
  DATA new_begda LIKE it_mili_leave-begda.
  DATA new_endda LIKE it_mili_leave-begda.

  DATA tindex TYPE i.
  DATA tindex2 TYPE i.


  __cls it_mili_leave.

  l_persk = p0001-persk.

  IF l_persk = 'U2'.


    LOOP AT p0000.

      IF p0000-massn = 'ZC' AND                             "UD1K952586
*     IF p0000-massn = 'ZB' AND                             "UD1K952586
         p0000-massg EQ '11' AND
         p0000-endda LE '99991231'.
        MOVE-CORRESPONDING p0000 TO it_mili_leave.
        APPEND it_mili_leave.
      ENDIF.

      IF p0000-massn EQ 'Z2' OR
         p0000-massn EQ 'Z4' OR
         p0000-massn EQ 'ZG' OR
         p0000-massn EQ 'ZT'.
        MOVE-CORRESPONDING p0000 TO it_mili_leave.
        it_mili_leave-massn = 'Z3'.
        APPEND it_mili_leave.
        CONTINUE.
      ENDIF.

      IF p0000-massn = 'Z3' AND
         p0000-endda LE '99991231'.
        MOVE-CORRESPONDING p0000 TO it_mili_leave.
        APPEND it_mili_leave.
      ENDIF.

    ENDLOOP.
************************************************************************
*HOURLY/SALARY HOURLY
*********************
  ELSEIF
      l_persk = 'U0' OR
      l_persk = 'U3'.

    LOOP AT p0000 WHERE endda LE '99991231'.

      IF p0000-massn EQ 'Z2' OR
         p0000-massn EQ 'Z4' OR
         p0000-massn EQ 'ZG' OR
         p0000-massn EQ 'ZT'.
        MOVE-CORRESPONDING p0000 TO it_mili_leave.
        it_mili_leave-massn = 'Z3'.
        APPEND it_mili_leave.
        CONTINUE.
      ENDIF.

      IF p0000-massn EQ 'ZC' AND
         p0000-massg EQ '11' AND
         p0000-endda LE '99991231'.
        MOVE-CORRESPONDING p0000 TO it_mili_leave.
        APPEND it_mili_leave.
      ENDIF.


      IF p0000-massn = 'Z3' AND
         p0000-endda LE '99991231'.
        MOVE-CORRESPONDING p0000 TO it_mili_leave.
        APPEND it_mili_leave.
      ENDIF.
    ENDLOOP.

  ENDIF.

  SORT it_mili_leave BY begda DESCENDING.
  tindex = 0.

  DO 30 TIMES.
    tindex = tindex + 1.
    tindex2 = tindex + 1.
    READ TABLE it_mili_leave INDEX tindex.
    IF sy-subrc EQ 0 AND it_mili_leave-massn EQ 'Z3'.
      new_begda = it_mili_leave-begda - 1.
      new_endda = it_mili_leave-endda.
      READ TABLE it_mili_leave INDEX tindex2.
      IF sy-subrc NE 0.
        DELETE it_mili_leave INDEX tindex.
        EXIT.
      ELSE.
        IF new_begda NE it_mili_leave-endda.
          DELETE it_mili_leave INDEX tindex.
        ELSE.
          it_mili_leave-endda = new_endda.
          MODIFY it_mili_leave INDEX tindex2 TRANSPORTING endda.
          DELETE it_mili_leave INDEX tindex.
          tindex = tindex - 1.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDDO.

  SORT it_mili_leave BY pernr begda endda massn massg.

  DATA prev_massn TYPE massn.
  DATA prev_massg TYPE massg.
  DATA $ix TYPE i.

  LOOP AT it_mili_leave.
    $ix = sy-tabix.
    IF it_mili_leave-massn EQ 'Z3'.
      IF ( prev_massn EQ 'ZB' AND prev_massg EQ '11' ) OR
         ( prev_massn EQ 'ZC' AND prev_massg EQ '11' ).
        CONTINUE.
      ELSE.
        DELETE it_mili_leave INDEX $ix.
      ENDIF.
    ENDIF.

    prev_massn = it_mili_leave-massn.
    prev_massg = it_mili_leave-massg.

  ENDLOOP.

  DATA $date LIKE sy-datum.
  DATA $times TYPE i.
  DATA $it_mili_leave LIKE it_mili_leave OCCURS 0 WITH HEADER LINE.

  DATA $keep.
  $times = it_range-high - it_range-low.

  DO 100 TIMES.

    $keep = space.
    READ TABLE it_mili_leave INDEX 1.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    $date = it_range-low.
    DO $times TIMES.
      IF $date >= it_mili_leave-begda AND $date <= it_mili_leave-endda.
        $keep = 'X'.
      ENDIF.
      ADD 1 TO $date.
    ENDDO.

    IF $keep IS INITIAL.
    ELSE.
      $it_mili_leave = it_mili_leave.
      APPEND $it_mili_leave.
    ENDIF.
    DELETE it_mili_leave INDEX 1.

  ENDDO.
  __cls it_mili_leave.
  it_mili_leave[] = $it_mili_leave[].

  SORT it_mili_leave BY begda DESCENDING.

  tindex = 0.

  DO 30 TIMES.
    tindex = tindex + 1.
    tindex2 = tindex + 1.
    READ TABLE it_mili_leave INDEX tindex.
    IF sy-subrc EQ 0 AND it_mili_leave-massn EQ 'Z3'.
      new_begda = it_mili_leave-begda - 1.
      READ TABLE it_mili_leave INDEX tindex2.
      IF sy-subrc NE 0.
        DELETE it_mili_leave INDEX tindex.
        EXIT.
      ELSE.
        IF new_begda NE it_mili_leave-endda.
          DELETE it_mili_leave INDEX tindex.
          tindex = tindex - 1.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDDO.
  DATA $lines TYPE i.
  READ TABLE it_mili_leave INDEX 1.
  IF sy-subrc EQ 0.
    DESCRIBE TABLE it_mili_leave LINES $lines .
    IF $lines EQ 1 AND it_mili_leave-massn EQ 'Z3'.
      DELETE it_mili_leave INDEX 1.
    ENDIF.
  ENDIF.

  SORT it_mili_leave BY pernr begda endda massn massg.

ENDFORM.                    " create_it_mili_leave

*---------------------------------------------------------------------*
*       FORM get_payroll_period                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  V_ABKRS                                                       *
*  -->  V_PERMO                                                       *
*  -->  V_BEGDA                                                       *
*  -->  V_ENDDA                                                       *
*  -->  V_ABKRT                                                       *
*  -->  V_PABRP                                                       *
*  -->  V_PABRJ                                                       *
*---------------------------------------------------------------------*
FORM get_payroll_period USING v_abkrs
                     CHANGING v_permo v_begda v_endda
                              v_abkrt v_pabrp v_pabrj.

  CALL FUNCTION 'PA03_PERIODDATES_GET'
       EXPORTING
            f_abkrs               = v_abkrs
       IMPORTING
            f_permo               = v_permo
            f_current_begda       = v_begda
            f_current_endda       = v_endda
            f_abkrs_text          = v_abkrt
       CHANGING
            f_current_period      = v_pabrp
            f_current_year        = v_pabrj
       EXCEPTIONS
            pcr_does_not_exist    = 1
            abkrs_does_not_exist  = 2
            period_does_not_exist = 3
            OTHERS                = 4.
  IF sy-subrc <> 0.
*      message e003 with v_pabrp v_pabrj.
  ENDIF.

  v_begda = v_endda - '364'.
ENDFORM.
* BEGIN OF UD1K952967
*&---------------------------------------------------------------------*
*&      Form  get_period_inpat
*&---------------------------------------------------------------------*
*       Get period inpat after Kronos go-live
*----------------------------------------------------------------------*
FORM get_period_inpat.
  DATA: lt_cmpdate TYPE datum OCCURS 0.                     "UD1K953009

  CLEAR: period_inpat_kronos, period_inpat_kronos[].
  LOOP AT p0001 WHERE endda >= cutoff_date
                  AND begda <= it_range-high
                  AND endda >= it_range-low
                  AND persg = '9'.                          "UD1K953009
*                 AND persg = '1'.                          "UD1K953009

* BEGIN OF UD1K953009
    period_inpat_kronos-pernr = p0001-pernr.

* Get the most recent date
    CLEAR lt_cmpdate[].
    APPEND: cutoff_date  TO lt_cmpdate,
            it_range-low TO lt_cmpdate,
            p0001-begda  TO lt_cmpdate.
    SORT lt_cmpdate DESCENDING.
    READ TABLE lt_cmpdate INDEX 1 INTO period_inpat_kronos-begda.

* Get the oldest date
    CLEAR lt_cmpdate[].
    APPEND: it_range-high TO lt_cmpdate,
            p0001-endda   TO lt_cmpdate.
    SORT lt_cmpdate ASCENDING.
    READ TABLE lt_cmpdate INDEX 1 INTO period_inpat_kronos-endda.

*   MOVE-CORRESPONDING p0001 TO period_inpat_kronos.
* END OF UD1K953009
    APPEND period_inpat_kronos.
  ENDLOOP.
ENDFORM.                    " get_period_inpat
* END OF UD1K952967
