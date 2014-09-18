*&---------------------------------------------------------------------*
*   Program-ID   : ZRHRY01_PYJ                                         *
*   Pattern      : Report 1-1                                          *
*   Module       : HR                                                  *
*   Created by   : LEE, JIN SOON                                       *
*   Created on   : 2003.10.28                                          *
*   Description  : Payroll Journal                                     *
*   Corrections  : 4.6C UD1K903402                                     *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  01/10/2012  Valerian  UD1K953699  Correct Structure of CD Version
*  5/9/2012   t-code is deleted by APM Monitoring
*----------------------------------------------------------------------

REPORT ZRHRY02_PYJ MESSAGE-ID ZMHR
                   LINE-COUNT 65
                   LINE-SIZE 110
                   NO STANDARD PAGE HEADING.

*INCLUDE: ZRHRY02_PYJtop.
TABLES : PERNR, t512t, cskt.
Infotypes : 0001, 0002, 0210.
data : w_permo       like t549a-permo,   " Period parameters
       w_abkrt       like t549t-atext.   " Payroll Area Text
data : w_FTEXT like t502t-FTEXT,         " Marital satatus text
       w_lgtxt like t512t-lgtxt.         " Wage type description

data : w_lgart like pc207-lgart.
*data : begin of it_tab occurs 0,
*       kostl like p0001-kostl,
*       kostx like q0001-kostx,
*       pernr like p0000-pernr,
*       perid like p0002-perid,
*       fatxt like q0002-fatxt,
*       nbrex like p0210-nbrex,
*       ename like p0001-ename,
*       lgart like pc207-lgart,
*       lgtxt like t512t-lgtxt,
*       betrg like pc207-betrg,
*       end of it_tab.
data: begin of rt occurs 150.               " Payroll Results
       include structure pc207.
data: end of rt.

data: begin of it_tot occurs 0,             " internal table for sums
      ccode  like p0001-bukrs,
      parea  like p0001-werks,
      psuba  like p0001-btrtl,
      betrg1 like rt-betrg,
      betrg2 like rt-betrg,
      betrg3 like rt-betrg,
      end of it_tot.

data: tmp_tot like it_tot occurs 0 with header line.

data: begin of cd-key,
      pernr like p0001-pernr,               "key to cluster directory
      end of cd-key.

data: begin of rgdir occurs 100.            " Cluster Directory
        include structure pc261.
data: end of rgdir.

data: begin of rx-key.
        include structure pc200.              " Payroll Results Key
data: end of rx-key.

data: begin of tmp occurs 10,
      num like rgdir-seqnr,
      flag.
data: end of tmp.

data: begin of cd-version.
        include structure pc2_cd.                           "UD1K953699
*       include structure pc201.                            "UD1K953699
*data: molga  like t001p-molga,         "country identifier "UD1K953699
data:                                                       "UD1K953699
      end of cd-version.

data: begin of okr-version.
        include structure pc201.
data: end of okr-version.

data: w_fpper like rgdir-fpper,
      cd-next_seq   type i,           "Next available seq number
      cd-last_pay   type d.           "Last payroll run date
data: w_sum like rt-betrg.
************************************************************************
*                         SELECTION-SCREEN                             *
************************************************************************

selection-screen begin of block frm1 with frame title text-f01.

selection-screen begin of line.                  " payroll area
selection-screen comment 1(31) text-001 for field p_abkrc.
parameters: p_abkrc  like t569v-abkrs default '11' obligatory.
selection-screen position 50.
parameters: p_begca  like t549q-begda modif id pe4.
selection-screen comment (2) text-004 for field p_endca modif id pe4.
parameters: p_endca  like t549q-endda modif id pe4.
selection-screen end of line.

selection-screen begin of line.                  " payroll period
parameters pa_curr radiobutton group prd.
selection-screen comment 4(15) text-002 for field pa_curr.
selection-screen end of line.
selection-screen begin of line.
parameters pa_othr radiobutton group prd.
selection-screen comment 4(15) text-003 for field pa_othr.
selection-screen position 33.
parameters: p_abrpc  like t549q-pabrp,
            p_abrjc  like t549q-pabrj.
selection-screen end of line.

selection-screen end of block frm1.

************************************************************************
*                        AT SELECTION-SCREEN                           *
************************************************************************

At selection-screen.
*...  initialization payroll period

  perform get_payroll_period using p_abkrc
                             changing w_permo p_begca p_endca
                                      w_abkrt p_abrpc p_abrjc.

*... screen modify
At selection-screen output.
  loop at screen.
    if screen-group1 = 'PE4'.
      screen-input = 0.
      modify screen.
    endif.
  endloop.

TOP-OF-PAGE.
*  check sy-ucomm = 'ONLI'.
  perform write_heading.

START-OF-SELECTION.

get pernr.
  perform get_seqno.
  loop at tmp.
    at first.
      RP-PROVIDE-FROM-LAST p0001 space p_begca p_endca.
      RP-PROVIDE-FROM-LAST p0002 space p_begca p_endca.
    endat.
    perform get_results_table.
    perform write_body.
  endloop.

END-OF-SELECTION.
  describe table it_tot lines sy-tfill.
  if sy-tfill = 0.
    message s001 with 'No data selected'.
  else.
    perform write_sum1.            "  personnel subarea
    perform write_sum2.            "  personnel area
    perform write_sum3.            "  Company code
    perform write_sum4.            "  All
  endif.
*  write : 'e'.
*  loop at it_rt.
*   perform get_results_table.
*  endloop.

*&---------------------------------------------------------------------*
*&      Form  get_payroll_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ABKRC  text
*      <--P_W_PERMO  text
*      <--P_P_BEGCA  text
*      <--P_P_ENDCA  text
*      <--P_W_ABKRT  text
*      <--P_P_ABRPC  text
*      <--P_P_ABRJC  text
*----------------------------------------------------------------------*
FORM get_payroll_period USING    v_abkrs
                        Changing v_permo v_begda v_endda
                                 v_abkrt v_pabrp v_pabrj.
  if v_abkrs ne space.
    CALL FUNCTION 'PA03_PERIODDATES_GET'
         EXPORTING
              F_ABKRS               = v_abkrs
         IMPORTING
              F_PERMO               = v_permo
              F_CURRENT_BEGDA       = v_begda
              F_CURRENT_ENDDA       = v_endda
              F_ABKRS_TEXT          = v_abkrt
         CHANGING
              F_CURRENT_PERIOD      = v_pabrp
              F_CURRENT_YEAR        = v_pabrj
         EXCEPTIONS
              PCR_DOES_NOT_EXIST    = 1
              ABKRS_DOES_NOT_EXIST  = 2
              PERIOD_DOES_NOT_EXIST = 3
              OTHERS                = 4.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.
ENDFORM.                    " get_payroll_period
*&---------------------------------------------------------------------*
*&      Form  write_heading
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_heading.
  Format COLOR 1 INTENSIFIED.
  WRITE :/'Company Code: ', p0001-bukrs, ' Tax Company: *     ',
          '   PPBegin: ',p_begca,'     PPEnd: ',p_endca,'  ',
          'Page: ',sy-pagno,(4) '  '.
  WRITE :/'OrgUnit: ',p0001-orgeh, '  Personnel Area: ', p0001-werks,
          ' Personnel Subarea: ', p0001-btrtl, ' Payroll Area: *      ',
          'period: ',p_abrpc,p_abrjc,(1) ' '.
  FORMAT COLOR OFF.
  SKIP 1.
  do 110 times.
    write:'*' no-gap.
  enddo.
  skip 1.
ENDFORM.                    " write_heading
*&---------------------------------------------------------------------*
*&      Form  get_seqno
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_seqno.
  concatenate p_abrjc p_abrpc into w_fpper.
*
  clear: rgdir, tmp.
  refresh: rgdir, tmp.

  cd-key = pernr-pernr.
  import cd-version cd-next_seq cd-last_pay rgdir
  from database pcl2(CU) client sy-mandt id cd-key.
*
  if sy-subrc = 0.
    loop at rgdir where abkrs = p_abkrc
                    and fpper = w_fpper
                    and fpbeg = p_begca
                    and fpend = p_endca
                    and srtza = 'A'.
      tmp-num = rgdir-seqnr.
      append tmp. "clear tmp.
    endloop.
  else.
    reject.
  endif.


ENDFORM.                    " get_seqno

*&---------------------------------------------------------------------*
*&      Form  get_results_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_results_table.
  clear rt. refresh rt.

  rx-key-pernr = pernr-pernr.
  rx-key-seqno = tmp-num.

  import kr-version to okr-version rt
    from database pcl2(RU) id rx-key.
ENDFORM.                    " get_results_table
*&---------------------------------------------------------------------*
*&      Form  write_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_body.
  clear: it_tot.
  select single ktext into cskt-ktext from cskt
         where spras = sy-langu and
               kokrs = 'H201' and
               kostl = p0001-kostl.

  select single FTEXT into w_FTEXT from t502t
         where SPRSL = sy-langu
           and famst = p0002-famst.


  write:/ 'Cost Center:', p0001-kostl.
  write:/ 'Cost Center Name:', cskt-ktext.
  skip 1.
  write: (18) 'Personnel Number:', p0001-pernr,'    ',
         (12) 'SSN Number:', p0002-perid(10),'    ',
         (8)  'Status:', w_FTEXT,'    ',
         (13) 'Exemptation:',p0210-nbrex.
  skip 1.
  write:/ 'Name' ,41(4)'Code', 57(25)'Description',94(15)'Amount'.
  write:/1(4) sy-uline, 41(4) sy-uline, 57(11) sy-uline, 94(6) sy-uline.
  clear w_sum.
  w_lgart ='/101'.
  perform write_payroll using w_lgart.
  perform get_data.              " insert data into it_tot
  skip 1.
  w_lgart ='/110'.
  perform write_payroll using w_lgart.
  skip 1.
  w_lgart ='9A01'.
  perform write_payroll using w_lgart.
  w_sum = rt-betrg + w_sum.
  perform get_data.              " insert data into it_tot
  w_lgart ='9F01'.
  perform write_payroll using w_lgart.
  w_sum = rt-betrg + w_sum.
  perform get_data.              " insert data into it_tot
  w_lgart ='/403'.
  perform write_payroll using w_lgart.
  w_sum = rt-betrg + w_sum.
  w_lgart ='/405'.
  perform write_payroll using w_lgart.
  w_sum = rt-betrg + w_sum.
  write:/1(56) ' ', 57(25) 'Tax Total' , 88(15) w_sum.
  skip 2.
  w_lgart ='/5U0'.
  perform write_payroll using w_lgart.

  skip 1.
  do 110 times.
    write:'*' no-gap.
  enddo.
  skip 1.
ENDFORM.                    " write_body
*&---------------------------------------------------------------------*
*&      Form  write_payroll
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_LGART  text
*----------------------------------------------------------------------*
FORM write_payroll USING    P_LGART.
  clear rt.
  clear w_lgtxt.

  select single lgtxt into w_lgtxt from t512t
          where SPRSL = sy-langu
            and lgart = p_lgart.

  if p_lgart ='/101'.
    write:/1(40) p0001-ename, 41(4) p_lgart, 57(25) w_lgtxt.
    read table rt with key lgart = p_lgart.
    if sy-subrc = 0.
      write: 88(15) rt-betrg.
    else.
      write: 98(4) '0.00'.
    endif.
  else.
    write:/1(40) ' ', 41(4) p_lgart, 57(25) w_lgtxt.
    read table rt with key lgart = p_lgart.
    if sy-subrc = 0.
      write: 88(15) rt-betrg.
    else.
      write: 98(4) '0.00'.
    endif.
  endif.
ENDFORM.                    " write_payroll
*&--------------------------------------------------------------------
*&      Form  get_data
*&--------------------------------------------------------------------
*       text
*---------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
FORM get_data.
  it_tot-ccode = p0001-bukrs.
  it_tot-parea = p0001-werks.
  it_tot-psuba = p0001-btrtl.
  it_tot-betrg1 = it_tot-betrg2 = it_tot-betrg3 = 0.
  case w_lgart.
    when '/101'.
      it_tot-betrg1 = rt-betrg.
    when '9F01'.
      it_tot-betrg2 = rt-betrg.
    when '9A01'.
      it_tot-betrg3 = rt-betrg.
  endcase.
  collect it_tot.
ENDFORM.                    " get_data
*&--------------------------------------------------------------------
*&      Form  write_sum1
*&--------------------------------------------------------------------
*       text
*---------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
FORM write_sum1.
  clear tmp_tot. refresh tmp_tot.

  loop at it_tot.
    tmp_tot-psuba  = it_tot-psuba.
    tmp_tot-betrg1 = it_tot-betrg1.
    tmp_tot-betrg2 = it_tot-betrg2.
    tmp_tot-betrg3 = it_tot-betrg3.
    collect tmp_tot. clear tmp_tot.
  endloop.
  sort tmp_tot by psuba.
  perform write_heading1.
  perform sum1_body using w_lgart.
ENDFORM.                    " write_sum1
*&---------------------------------------------------------------------*
*&      Form  write_heading1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_heading1.
  Format COLOR 1 INTENSIFIED.
  WRITE :/'Company Code: ', p0001-bukrs, ' Tax Company: *     ',
          '   PPBegin: ',p_begca,'     PPEnd: ',p_endca,'  ',
          'Page: ',sy-pagno,(4) '  '.
  WRITE :/'OrgUnit: ',p0001-orgeh, '  Personnel Area: ', p0001-werks,
          ' Personnel Subarea: ', p0001-btrtl, ' Payroll Area: *      ',
          'period: ',p_abrpc,p_abrjc,(1) ' '.
  FORMAT COLOR OFF.
  SKIP 1.

ENDFORM.                    " write_heading1
*&--------------------------------------------------------------------
*&      Form  sum1_body
*&--------------------------------------------------------------------
*       text
*---------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
FORM sum1_body using p_lgart.

  do 110 times.
    write:'*' no-gap.
  enddo.
  write:/1(110) 'Sum of the structure'.
  do 110 times.
    write:'*' no-gap.
  enddo.
  clear w_lgtxt.
  read table tmp_tot index  1.

  p_lgart = '/101'.
    select single lgtxt into w_lgtxt from t512t
         where SPRSL = sy-langu
           and lgart = p_lgart.
    write:/41(4) p_lgart, 57(25) w_lgtxt, 94(15) tmp_tot-betrg1.
  clear w_lgtxt.
  clear t512t.
  p_lgart = '9A01'.
    select single lgtxt into w_lgtxt from t512t
         where SPRSL = sy-langu
           and lgart = p_lgart.

    write:/41(4) p_lgart, 57(25) w_lgtxt, 94(15) tmp_tot-betrg2.
    clear w_lgtxt.
    clear t512t.
  p_lgart = '9F01'.
      select single lgtxt into w_lgtxt from t512t
         where SPRSL = sy-langu
           and lgart = p_lgart.

      write:/41(4) p_lgart, 57(25) w_lgtxt, 94(15) tmp_tot-betrg3.

*  select single ktext into cskt-ktext from cskt
*         where spras = sy-langu and
*               kokrs = 'H201' and
*               kostl = p0001-kostl.

*  select single FTEXT into w_FTEXT from t502t
*         where SPRSL = sy-langu
*           and famst = p0002-famst.
*

*  write:/ 'Cost Center:', p0001-kostl.
*  write:/ 'Cost Center Name:', cskt-ktext.
*  skip 1.
*  write: (18) 'Personnel Number:', p0001-pernr,'    ',
*         (12) 'SSN Number:', p0002-perid(10),'    ',
*         (8)  'Status:', w_FTEXT,'    ',
*         (13) 'Exemptation:',p0210-nbrex.
*  skip 1.
*  write:/41(4)'Code', 57(25)'Description',94(15)'Amount'.
*  write:/41(4) sy-uline, 57(11) sy-uline, 94(6) sy-uline.

ENDFORM.                                                     " sum1_bod
*&---------------------------------------------------------------------*
*&      Form  write_sum2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_sum2.
  clear tmp_tot. refresh tmp_tot.

  loop at it_tot.
    tmp_tot-parea  = it_tot-parea.
    tmp_tot-betrg1 = it_tot-betrg1.
    tmp_tot-betrg2 = it_tot-betrg2.
    tmp_tot-betrg3 = it_tot-betrg3.
    collect tmp_tot. clear tmp_tot.
  endloop.
  sort tmp_tot by parea.
  perform write_heading2.
  perform sum1_body using w_lgart.

ENDFORM.                    " write_sum2
*&---------------------------------------------------------------------*
*&      Form  write_heading2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_heading2.
  Format COLOR 1 INTENSIFIED.
  WRITE :/'Company Code: ', p0001-bukrs, ' Tax Company: *     ',
          '   PPBegin: ',p_begca,'     PPEnd: ',p_endca,'  ',
          'Page: ',sy-pagno,(4) '  '.
  WRITE :/'OrgUnit: ',p0001-orgeh, '  Personnel Area: ', p0001-werks,
          ' Personnel Subarea: *    ', ' Payroll Area: *      ',
          'period: ',p_abrpc,p_abrjc,(1) ' '.
  FORMAT COLOR OFF.
  SKIP 1.
*  do 110 times.
*    write:'*' no-gap.
*  enddo.
*  skip 1.

ENDFORM.                    " write_heading2
*&---------------------------------------------------------------------*
*&      Form  write_sum3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_sum3.
  clear tmp_tot. refresh tmp_tot.

  loop at it_tot.
    tmp_tot-ccode  = it_tot-ccode.
    tmp_tot-betrg1 = it_tot-betrg1.
    tmp_tot-betrg2 = it_tot-betrg2.
    tmp_tot-betrg3 = it_tot-betrg3.
    collect tmp_tot. clear tmp_tot.
  endloop.
  sort tmp_tot by ccode.
  perform write_heading3.
  perform sum1_body using w_lgart.

ENDFORM.                    " write_sum3
*&---------------------------------------------------------------------*
*&      Form  write_heading3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_heading3.
  format COLOR 1 INTENSIFIED.
  WRITE :/'Company Code: ', p0001-bukrs, ' Tax Company: *     ',
          '   PPBegin: ',p_begca,'     PPEnd: ',p_endca,'  ',
          'Page: ',sy-pagno,(4) '  '.
  WRITE :/'OrgUnit: ',p0001-orgeh, '  Personnel Area: *    ',
          ' Personnel Subarea: *    ', ' Payroll Area: *      ',
          'period: ',p_abrpc,p_abrjc,(1) ' '.
  FORMAT COLOR OFF.
  SKIP 1.
*  do 110 times.
*    write:'*' no-gap.
*  enddo.
*  skip 1.

ENDFORM.                    " write_heading3
*&---------------------------------------------------------------------*
*&      Form  write_sum4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_sum4.
  clear tmp_tot. refresh tmp_tot.

  loop at it_tot.
    tmp_tot-ccode  = it_tot-ccode.
    tmp_tot-betrg1 = it_tot-betrg1.
    tmp_tot-betrg2 = it_tot-betrg2.
    tmp_tot-betrg3 = it_tot-betrg3.
    collect tmp_tot. clear tmp_tot.
  endloop.
  sort tmp_tot by ccode.
  perform write_heading3.
  perform sum1_body using w_lgart.

ENDFORM.                    " write_sum4
