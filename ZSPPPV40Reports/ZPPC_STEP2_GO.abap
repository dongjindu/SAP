*{
*\REPORT ZPPC_STEP2_GO .
* Author:       chris Li
* Specification:MY Hur
* Creation Date:07/22/2005
* description:  This program is copied from PPC_STEP2_GO and modified .
*               to call the customer step 2 function which will run
*               in parallel mode
*               Logic was changed to the following steps
*                1. Reading source data from PPC_STEP2 and other info
*                2. Split component into packages for each pcc order
*                3. Submit all packages of one pcc order to run in
*                   parallel mode
*                4. receiving message from each parallel process
*                5. waiting all processes finish
*                6. check the result and delete component in source tabl
*                7. go to step 3 to submit packages of next pcc order
*                8. Write message into log
*                9. After all finish, check if rollback happened. If
*                   remaining components exist, try again up to 3 times
*                   (Normally, one time all success) .
*
*Object List: zppc_step2_go--copied from ppc_step2_go
*             zpp_ppc1pr_step2_exe ---copied from PP_PPC1PR_STEP2_EXE
*                          and modified for parallel process
*             z_fpp_ppc1pr_step2_single_exe ---New Function module which
*                          is a RFC function, execute rollback or commit
*                          for each package.
*             zpp_ppc1pr_step2_single_exe---copied from *
*                          pp_ppc1pr_step2_single_exe. Deleted log
*                          creation logic and transfer message by
*                          function parameters
*
*Note:       This program does not change the input and output data
*            structure. It's has the same data source and output as the
*            the standard program PPC_STEP2_GO, just the log message is
*            not exact same.
*
*


report  zppc_step2_go.

constants:
  charx       type c value 'X',
  chars       type c value 'S',
  gc_lowlimit type i value 1,     " min. nr. of lines in mat. document
  gc_maxlimit type i value 9999,  " max. nr. of lines in mat. document
  gc_maxitems type i value 500,   " proposed nr. of lines in document
  gc_maxerrs  type i value 9999.  " max. nr. of errors allowed in log

type-pools: ppcpr.

data:
  if_date    type budat,
  if_unam    type ppc_confuname,
  date_range type ppcpr_postdate_range,
  unam_range type ppcpr_username_range,
  wa_seldate like line of date_range,
  wa_selunam like line of date_range,
  w_classname type RZLLI_APCL,
  lf_lognr   type balognr,
  w_count    type i.
DATA: LT_LOG  LIKE ZTPP_BFSTEP2_LOG OCCURS 0 WITH HEADER LINE.

selection-screen begin of block b2 with frame title text-002.
select-options:
  sel_date   for if_date default sy-datlo,
  sel_unam   for if_unam default sy-uname.
selection-screen end of block b2.

selection-screen begin of block b1 with frame title text-001.
parameters:
  pa_proto   type ppc_proto default charx,
  pa_limit   type ppc_count default gc_maxitems.
parameters: p_srvgrp like rzllitab-classname obligatory.
parameters: p_flag   type c no-display.
selection-screen end of block b1.



*-------------------
at selection-screen.
*-------------------

* parameter check
  if pa_limit gt gc_maxlimit or pa_limit lt gc_lowlimit.
    message e004(ppc1pa) with pa_limit gc_lowlimit gc_maxitems.
  endif.

  select single classname into w_classname
                          from rzllitab
                          where classname = p_srvgrp.
  if sy-subrc ne 0.
    message id 'ZMPP' type 'E' number '001' with text-003.
  endif.

*------------------
start-of-selection.
*------------------
* delete the old error log
  delete from ztpp_bfstep2_log client specified
         where mandt = sy-mandt.
  commit work.
*
  loop at sel_date into wa_seldate.
    append wa_seldate to date_range.
  endloop.
  loop at sel_unam into wa_selunam.
    append wa_selunam to unam_range.
  endloop.

*{   REPLACE        UP1K900079                                        1
*\    call function 'ZPP_PPC1PR_STEP2_EXE'
    call function 'ZZPP_PPC1PR_STEP2_EXE'
*}   REPLACE
      exporting
        if_date_range    = date_range
        if_user_range    = unam_range
        if_limit         = pa_limit
        if_protocol_show = pa_proto
        group_name       = p_srvgrp
        i_flag           = p_flag
      importing
        ef_lognumber     = lf_lognr
      exceptions
        protocol_error   = 1
        enqueue_error    = 2
        nothing_selected = 3
        others           = 4.
    if sy-subrc <> 0.
      message id sy-msgid type chars number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      message s017(ppc1pr) with lf_lognr.
    endif.

*{   INSERT         UP1K900081                                        2
*
*}   INSERT


* That's it. The end.



*}   REPLACE
