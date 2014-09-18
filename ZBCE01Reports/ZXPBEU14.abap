**----------------------------------------------------------------------
**
**   INCLUDE ZXPBEU14
**
**----------------------------------------------------------------------
**
*
**&--------------------------------------------------------------------&
**
**&   Program: ZXPBEU14.
**&   Author: Shiva Gnanaguru.
**&   Specification: Calculate the Employer contribution w.r.t 401K.
**&
**&                  If employee contribution percentage is:
**&                    a. 1%, 2% or 3% then Emplyr. Contri. eq 60% of
**&                       base salary multiply employee contribution %.
**&                    b. More than 3% then Emplyr. Contri. eq 60% of
**&                       base salary multiply by 3%
**&--------------------------------------------------------------------&
**
**& Date        User Id   Name    Transport         Description.
**& 04/20/2004  100471    Shiva   UD1K909761   intial program.
**& 11/03/2006  103088    Hassan  UD1K922906   Epmlr 4% for PP 2007 @
*60%
**& 04/09/2007  103088    Hassan  UD1K940281   Get current 401K value.
**& 04/16/2007  P00181    Manju   UD1K940327   Replace formula with IMG
**                                             settings
**& 06/21/2007 103088     Hassan  UD1K940865   Comment out user exit.
**                                             This user exit will not
**                                             be envoked since SAP
**                                             config is in place now.
**& 08/19/2013 T00306     Sunho.J UD1K957427   Currently when temp EEs
**                                             become perm EEs, they need
**                                             to wait for 1 year to be
**                                             eligible for 401K ER match.
**                                             HMMA is going to give them
**                                             credit hours for their
**                                             temporary service regarding
**                                             the 401K ER match.
**&--------------------------------------------------------------------&
**
** INFO :  _ER_CONTRIB_RULE holds values from  IMG Table  V_74FF_C
**         Benefit Employer contribution rule saving Plan.
** Note :- When Employer contribution changes the user exit & IMG
**settings has to be modified.
*
**


* 08/19/2013 - Sunho.J Start

DATA: ls_pa0041 TYPE pa0041,
      ls_pa0041_ze TYPE pa0041,
      lv_date_revised  TYPE dats,
      lv_date_expected TYPE dats,
      l_field(20),
      l_num(2)  TYPE n.

DATA: lv_molga TYPE molga.
DATA: in_rgdir LIKE pc261 OCCURS 0 WITH HEADER LINE.
DATA: ls_rgdir LIKE LINE OF in_rgdir,
      lv_relid LIKE pcl2-relid,
      lv_seqnr LIKE pc261-seqnr,
      ls_result TYPE pay99_result.

DATA: ls_rt LIKE pc207 OCCURS 0 WITH HEADER LINE.
DATA: lv_8337  TYPE pc207-betrg,
      lv_102   TYPE pc207-betrg,
      lv_contb TYPE pc207-betrg,
      lv_limit TYPE pc207-betrg,
      lv_ercst TYPE pc207-betrg.

DATA: l_er_limit_step TYPE pc207-betrg.

DATA: l_endda LIKE sy-datum.

RANGES: r_datum FOR sy-datum.

FIELD-SYMBOLS: <fs_expected> TYPE any.

* Check if TM has a date type 'ZD' in current PA0041.

SELECT SINGLE *
  FROM pa0041
  INTO ls_pa0041
 WHERE pernr = _pernr
   AND ( dar01 = 'ZD' OR dar02 = 'ZD' OR dar03 = 'ZD'
      OR dar04 = 'ZD' OR dar05 = 'ZD' OR dar06 = 'ZD'
      OR dar07 = 'ZD' OR dar08 = 'ZD' OR dar09 = 'ZD'
      OR dar10 = 'ZD' OR dar11 = 'ZD' OR dar12 = 'ZD' ).

IF sy-subrc NE 0.
  RAISE standard_calculation.
ENDIF.

* Check if TM has a date type 'ZE' in current PA0041.
SELECT SINGLE * FROM pa0041 INTO ls_pa0041_ze
  WHERE pernr = _pernr
   AND ( dar01 = 'ZE' OR dar02 = 'ZE' OR dar03 = 'ZE'
      OR dar04 = 'ZE' OR dar05 = 'ZE' OR dar06 = 'ZE'
      OR dar07 = 'ZE' OR dar08 = 'ZE' OR dar09 = 'ZE'
      OR dar10 = 'ZE' OR dar11 = 'ZE' OR dar12 = 'ZE' ).
IF sy-subrc NE 0.
  RAISE standard_calculation.
ENDIF.

* Read the endda date of the pay period in which the missing match needs to be paid.

DO 12 TIMES.
  l_num = sy-index.
  CLEAR l_field.

  CONCATENATE 'LS_PA0041_ZE-DAR' l_num INTO l_field.
  ASSIGN (l_field) TO <fs_expected>.

  IF sy-subrc EQ 0 AND <fs_expected> EQ 'ZE'.
    UNASSIGN <fs_expected>.
    CONCATENATE 'LS_PA0041-DAT' l_num INTO l_field.
    ASSIGN (l_field) TO <fs_expected>.
    IF sy-subrc EQ 0.
      l_endda = <fs_expected>.
      UNASSIGN <fs_expected>.
      EXIT.
    ENDIF.
  ENDIF.
ENDDO.

IF _endda NE l_endda.
  RAISE standard_calculation.
ENDIF.

* Read MATCH_DATE_EXPECTED
DO 12 TIMES.
  l_num = sy-index.
  CLEAR l_field.

  CONCATENATE 'LS_PA0041-DAR' l_num INTO l_field.
  ASSIGN (l_field) TO <fs_expected>.

  IF sy-subrc EQ 0 AND <fs_expected> EQ 'ZD'.
    UNASSIGN <fs_expected>.
    CONCATENATE 'LS_PA0041-DAT' l_num INTO l_field.
    ASSIGN (l_field) TO <fs_expected>.
    IF sy-subrc EQ 0.
      lv_date_expected = <fs_expected>.
      UNASSIGN <fs_expected>.
      EXIT.
    ENDIF.
  ENDIF.
ENDDO.

IF sy-subrc NE 0.
  RAISE standard_calculation.
ENDIF.

* Read Payroll Control Records
CLEAR lv_molga.
CALL FUNCTION 'CU_READ_RGDIR'
  EXPORTING
    persnr          = _pernr
  IMPORTING
    molga           = lv_molga
  TABLES
    in_rgdir        = in_rgdir
  EXCEPTIONS
    no_record_found = 1
    OTHERS          = 2.

* Delete voided payroll data.
DELETE in_rgdir WHERE voidr NE space.
DELETE in_rgdir WHERE srtza NE 'A'. "Active

* Get MATCH_DATE_REVISED
DELETE in_rgdir WHERE fpend <= lv_date_expected
                   OR fpend+0(6) = lv_date_expected+0(6).
READ TABLE in_rgdir INDEX 1.
lv_date_revised = in_rgdir-fpbeg.


* Cluster id for US
* Personnel Country Grouping
CLEAR lv_relid.
SELECT SINGLE relid INTO lv_relid
              FROM t500l
              WHERE molga = lv_molga.
IF   lv_relid IS INITIAL.
  lv_relid = 'RU'.
ENDIF.


*** Looping For ER Match
LOOP AT in_rgdir INTO ls_rgdir.
  lv_seqnr = ls_rgdir-seqnr.

* Read Payroll cluster Data for each payroll control record
  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
    EXPORTING
      clusterid                    = lv_relid
      employeenumber               = _pernr
      sequencenumber               = lv_seqnr
      read_only_international      = 'X'
    CHANGING
      payroll_result               = ls_result
    EXCEPTIONS
      illegal_isocode_or_clusterid = 1
      error_generating_import      = 2
      import_mismatch_error        = 3
      subpool_dir_full             = 4
      no_read_authority            = 5
      no_record_found              = 6
      versions_do_not_match        = 7
      error_reading_archive        = 8
      error_reading_relid          = 9
      OTHERS                       = 10.

*  clear : ws_8387, ws_401k,lv_8337, ws_bc31, ws_gsal .

  READ TABLE ls_result-inter-rt INTO ls_rt WITH KEY lgart = '8387'.
  CHECK sy-subrc NE 0.
  READ TABLE ls_result-inter-rt INTO ls_rt WITH KEY lgart = '8337'.
  CHECK sy-subrc EQ 0.

* ER Matching
  LOOP AT ls_result-inter-rt INTO ls_rt
    WHERE lgart = '8337' OR lgart = '/102'.
    CASE ls_rt-lgart.
      WHEN '8337'.
        lv_8337 = abs( ls_rt-betrg ).
      WHEN '/102'.
        lv_102 = abs( ls_rt-betrg ).
    ENDCASE.
  ENDLOOP.

  IF ls_rgdir-fpend >= '20110321'.
    lv_contb = lv_8337.
    lv_limit = lv_102 * ( 4 / 100 ).
  ELSEIF ls_rgdir-fpend >= '20100419' AND ls_rgdir-fpend < '20110321'.
    lv_contb = lv_8337 * ( 80 / 100 ).
    lv_limit = lv_102 * ( 32 / 1000 ).
  ELSEIF ls_rgdir-fpend >= '20061218' AND ls_rgdir-fpend < '20100419'.
    lv_contb = lv_8337 * ( 60 / 100 ).
    lv_limit = lv_102 * ( 24 / 1000 ).
  ELSEIF ls_rgdir-fpend >= '20040501' AND ls_rgdir-fpend < '20061218'.
    lv_contb = lv_8337 * ( 60 / 100 ).
    lv_limit = lv_102 * ( 18 / 1000 ).
  ELSE.
*    ERROR
  ENDIF.

  lv_ercst = lv_contb.
  IF lv_limit <= lv_contb.
    lv_ercst = lv_limit.
  ENDIF.

  ADD lv_ercst TO _ercst.

  CLEAR: lv_ercst, lv_8337, lv_102, lv_contb, lv_limit. "08.29.2014
ENDLOOP.


* the amount for Process 2
l_er_limit_step = _salry * ( 4 / 100 ).
IF _eecst > l_er_limit_step.
  ADD l_er_limit_step TO _ercst.
ELSE.
  ADD _eecst TO _ercst.
ENDIF.




* 08/19/2013 - Sunho.J End


*data:  w_eepct like pa0169-eepct,
*       w_ercst like t5ubi-ercst,
*       w_begda like pa0169-begda,
*       w_endda like pa0169-endda.
*
*field-symbols: <status>.

*  ASSIGN ('(RPCALCU0)pnpdisbd') TO <status>.
*select single eepct into (w_eepct)
*  from pa0169
* where pernr = _pernr
*   and pltyp = '401K'
*   and endda >= _endda.
*                     and endda >= <status>.

**check sy-subrc eq 0 .
** Select Employee Contribution % for given period
**select single eepct into (w_eepct)
**                     from pa0169
**                     where pernr = _pernr
**                     and pltyp = '401K'
**                     and endda >= _endda.
***                     and endda >= <status>.
**assign _endda to <status>.
*****
**
***Even if employee contribute more than 3%, HMMA will contribute only 3%
***till 12/17/06.The HMMA contribution will be 4% as of
***12/18/2006(pp2007)
***_ER_CONTRIB_RULE-CNPCT  = ( ( 60 / 100 ) * ( w_eepct / 100 ) )
**
*** if Payroll period less than 12007 then max ER contribution is 3%
**if <status> <= '20061217'
**and w_eepct > 3.
***  w_eepct = 3.
**  w_ercst =  _salry * _er_contrib_rule-cnpct / 100.
**
*** if Payroll period greater than 12007 then max ER contribution is 4%
**elseif
** <status> > '20061217'
**and w_eepct > 4.
***  w_eepct = 4.
**  w_ercst =  _salry * _er_contrib_rule-cnpct / 100.
**
**else.
*** if Payroll period is of any period then max ER contribution is equal
*** Employee contribution from Infotype PA0169
*** w_eepct = w_eepct.
**  w_ercst =  _salry * ( ( 60 / 100 ) * ( w_eepct / 100 ) ).
**
**endif.
**
*** Begin of changes -  UD1K940327
***w_ercst =  _salry * ( ( 60 / 100 ) * ( w_eepct / 100 ) ).
*** end of changes - UD1K940327
**
**_ercst = w_ercst.
**
**if _eepre = 0.
**  _ercst = 0.
**endif.
