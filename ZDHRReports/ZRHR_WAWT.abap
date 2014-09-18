*&---------------------------------------------------------------------*
*   Program-ID   : ZRHR_WAWT                                           *
*   Pattern      : Report 1-1                                          *
*   Module       : HR                                                  *
*   Created by   : LEE, JIN SOON                                       *
*   Created on   : 2004.01.07                                          *
*   Description  : EE Wage Amount for A Wage Type                      *
*   Corrections  : 4.6C UD1K903402                                     *
*&---------------------------------------------------------------------*
REPORT ZRHR_WAWT MESSAGE-ID ZMHR
                 LINE-COUNT 65
                 LINE-SIZE 132
                 NO STANDARD PAGE HEADING.

TABLES : pernr, pa0001.

************************************************************************
*                           VARIABLES                                  *
************************************************************************
*... internal tables
data: begin of cd-key,
      pernr like p0001-pernr,               " key to cluster directory
      end of cd-key.

data: begin of rgdir occurs 100.            " Cluster Directory
      include structure pc261.
data: end of rgdir.

data: begin of rx-key.
      include structure pc200.              " Payroll Results Key
data: end of rx-key.

data: begin of rt occurs 150.               " Payroll Results
      include structure pc207.
data: end of rt.

data: begin of tax occurs 0.                " Payroll Results: Tax (US)
      include structure pc22t.
data: end of tax.

data: begin of adr.
      include structure pc22s.              " Address
data: end of adr.

data: begin of versc.
      include structure pc202.
data: end of versc.

data: begin of perm.                        " Personal Characteristics
      include structure pc22r.
data: end of perm.

data: begin of crt occurs 0.
      include structure pc208.
data: end of crt.

DATA: begin of tmp occurs 10,
      num like rgdir-seqnr,
      flag.
data: end of tmp.

data: begin of cd-version.
      include structure pc201.
data: molga  like t001p-molga,           " country identifier
      end of cd-version.

data: begin of okr-version.
      include structure pc201.
data: end of okr-version.

*data: it_form like pc408 occurs 0 with header line.

data: begin of pr_itab occurs 0,
      pernr like pa0001-pernr,
      ename like pa0001-ename,
      end of pr_itab.

data: begin of main_itab occurs 0,
      pernr like pa0001-pernr,
      ename like pa0001-ename,
      pabrj like t569v-pabrj,
      pabrp like t569v-pabrp,
      ytdam like crt-betrg,
      crtam like rt-betrg,
      end of main_itab.
data : begin of it_7004 occurs 0.
        include structure BAPI7004_RL.
data : end of it_7004.
*... variants

DATA : w_sabrj like t569v-pabrj,
       w_eabrj like t569v-pabrj.
DATA : w_sabrp like t569v-pabrp,
       w_eabrp like t569v-pabrp.
DATA : w_abkrs like t569v-abkrs.
DATA : w_lgart like pc207-lgart.

data : w_width   type i,
       w_subrc   like sy-subrc.
data : w_years(4),
       w_yearh like sy-datum,
       w_yearl like sy-datum,
       w_fprdl(6),
       w_fprdh(6).
data : w_lgtxt like t512t-lgtxt.
*      w_atext   like t549t-atext,
*      w_begda   like t549q-begda,
*      w_endda   like t549q-endda,
*      w_perid   like pa0002-perid.
*
*data: w_stxt1   type short_d,
*      w_stxt2   type stext.
*
*data: w_taixl   like sy-tabix,
*      w_taixh   like sy-tabix,
*      w_lgart   like t512t-lgart,
*      w_lgtxt   like t512t-lgtxt.
*
*data: cd-next_seq   type i,             "Next available seq number
*      cd-last_pay   type d,             "Last payroll run date
*      w_fpper   like rgdir-fpper.
*
***********************************************************************
*                           MAIN SOURCE
***********************************************************************
*
set pf-status 'PSHRY01'.
*
clear w_subrc.

** Furong on 08/31/12 for deactivate the unused program
Initialization.
Leave program.
** End on 08/31/12

perform init_display_screen.

***********************************************************************
*                          USER COMMNAD
***********************************************************************
at user-command.
  clear w_subrc.
  case sy-ucomm.
    when 'EXEC' or 'ENTR'.
      perform read_select_options.
*    when 'NEXT'.
*      perform read_select_options.
*      w_pernr = w_pernr + 1.
*    when 'PREV'.
*      perform read_select_options.
*      w_pernr = w_pernr - 1.
  endcase.
*  perform get_payroll_period.
*  perform check_payroll_area.
   sy-lsind = 0.
     perform get_payroll_data.
     if w_subrc = 0.
     perform write_result.
   else.
     perform init_display_screen.
   endif.
  clear sy-ucomm.
*
*&---------------------------------------------------------------------
*&      Form  init_display_screen
*&---------------------------------------------------------------------
FORM init_display_screen.

  w_width = 121.

  uline at (w_width).
  write: / sy-vline no-gap.
  write: (10) 'Year     :',
         (4)  w_sabrj input on no-gap,
         (3)  ' - ' no-gap,
         (4)  w_eabrj input on no-gap,
         (32) ' ',
         (10) 'Wage Type :',
         (4)  w_lgart input on, at w_width sy-vline no-gap.
  write: / sy-vline no-gap.
  write: (10) 'Period   :',
         (2)  w_sabrp input on no-gap,
         (3)  ' - ' no-gap,
         (2)  w_eabrp input on, at w_width sy-vline no-gap.
  write: / sy-vline no-gap.
  write: (10) 'Area     :',
         (2)  w_abkrs input on, at w_width sy-vline no-gap.
  uline at (w_width).

  write: / sy-vline no-gap.
  write: (17) 'PERSONNEL NUMBER' no-gap, (4) ' ' no-gap,
         (30) 'NAME' no-gap, (4) ' ' no-gap,
         (12) 'YEAR' no-gap, (4) ' ' no-gap,
         (12) 'PERIOD' no-gap, (4) ' ' no-gap,
         (15) 'CURRENT' right-justified,
         (15) 'YTD' right-justified, sy-vline no-gap.
  uline at (w_width).

  do 26 times.
    write: / sy-vline no-gap.
    write: (119) ' ' no-gap, at w_width sy-vline no-gap.
  enddo.
  uline at (w_width).
ENDFORM.                    " init_display_screen
*&---------------------------------------------------------------------*
*&      Form  get_payroll_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_payroll_data.

  clear : pr_itab, pr_itab[].
  select pernr ename into corresponding fields of table pr_itab
         from pa0001
         where endda = '99991231'
         and   persg in ('1' , '9')
         and   abkrs = w_abkrs.
  append pr_itab.
  perform get_payroll_result.
  if not main_itab[] is initial.
  w_subrc = 0.
  else.
  w_subrc = 4.
  endif.
ENDFORM.                    " get_payroll_data
*&---------------------------------------------------------------------*
*&      Form  get_payroll_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_payroll_result.
   clear: main_itab, main_itab[].
   w_years = w_sabrj.
   do.
     if w_years > w_eabrj.
     exit.
     endif.
      clear : w_yearl,w_yearh,w_fprdl,w_fprdh.
      concatenate : w_years '0101' into w_yearl,
                    w_years '1231' into w_yearh,
                    w_years w_sabrp into w_fprdl,
                    w_years w_eabrp into w_fprdh.

      loop at pr_itab.
         clear: it_7004, it_7004[].

         CALL FUNCTION 'BAPI_GET_PAYROLL_RESULT_LIST'
           EXPORTING
            EMPLOYEENUMBER       = pr_itab-pernr
            FROMDATE             = w_yearl
            TODATE               = w_yearh
*          IMPORTING
*            RETURN              =
           TABLES
             RESULTS             = it_7004.


      delete it_7004 where fpperiod < w_fprdl or
                           fpperiod > w_fprdh.

      loop at it_7004.
         perform get_results_table.
         read table RT with key lgart = w_lgart.
         read table CRT with key lgart = w_lgart.

         main_itab-pernr = pr_itab-pernr.
         main_itab-ename = pr_itab-ename.
         main_itab-pabrj = w_years.
         main_itab-pabrp = it_7004-fpperiod+4(2).
         main_itab-crtam = rt-betrg.
         main_itab-ytdam = crt-betrg.

         append main_itab." clear main_itab.
      endloop.
     endloop.
   w_years = w_years + 1.
   enddo.

ENDFORM.                    " get_payroll_result
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
  clear crt. refresh crt.
  rx-key-pernr = pr_itab-pernr.
  rx-key-seqno = it_7004-SEQUENCENUMBER.

  import kr-version to okr-version rt crt
    from database pcl2(RU) id rx-key.

ENDFORM.                    " get_results_table
*&---------------------------------------------------------------------*
*&      Form  read_select_options
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_select_options.
   do.
    read line sy-index field value:
      w_sabrj, w_eabrj, w_sabrp, w_eabrp, w_abkrs, w_lgart.
    if sy-subrc <> 0. exit. endif.
   enddo.
ENDFORM.                    " read_select_options
*&---------------------------------------------------------------------*
*&      Form  write_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_result.
*  clear main_itab.
  clear : w_lgtxt.
   select single lgtxt into w_lgtxt from t512t
          where lgart = w_lgart
          and   sprsl = sy-langu.
  w_width = 121.

  uline at (w_width).
  write: / sy-vline no-gap.
  write: (10) 'Year     :',
         (4)  w_sabrj input on no-gap,
         (3)  ' - ' no-gap,
         (4)  w_eabrj input on no-gap,
         (32) ' ',
         (10) 'Wage Type :',
         (4)  w_lgart input on no-gap,
         (1)  ' ',
         (20) w_lgtxt color 6 inverse , at w_width sy-vline no-gap.
  write: / sy-vline no-gap.
  write: (10) 'Period   :',
         (2)  w_sabrp input on no-gap,
         (3)  ' - ' no-gap,
         (2)  w_eabrp input on, at w_width sy-vline no-gap.
  write: / sy-vline no-gap.
  write: (10) 'Area     :',
         (2)  w_abkrs input on, at w_width sy-vline no-gap.
  uline at (w_width).

  write: / sy-vline no-gap.
  write: (17) 'PERSONNEL NUMBER' no-gap, (4) ' ' no-gap,
         (30) 'NAME' no-gap, (4) ' ' no-gap,
         (12) 'YEAR' no-gap, (4) ' ' no-gap,
         (12) 'PERIOD' no-gap, (4) ' ' no-gap,
         (15) 'CURRENT' right-justified,
         (15) 'YTD' right-justified, sy-vline no-gap.
  uline at (w_width).

  loop at main_itab.
    write: / sy-vline no-gap,
          (17) main_itab-pernr no-gap, (4) ' ' no-gap,
          (30) main_itab-ename no-gap, (4) ' ' no-gap,
          (12) main_itab-pabrj no-gap, (4) ' ' no-gap,
          (12) main_itab-pabrp no-gap, (4) ' ' no-gap,
          (15) main_itab-crtam right-justified,
          (15) main_itab-ytdam right-justified, sy-vline no-gap.
  endloop.
  uline at (w_width).


ENDFORM.                    " write_result
