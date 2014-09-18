*{
*\REPORT ZSAP_PPC_STEP2_GO
* Author:       Richard Mesteri (SAP)
* Creation Date: 10/01/201REPORT  zsap_ppc_step2_go.
report zsap_ppc_step2_go.
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
  w_classname type rzlli_apcl,
  lf_lognr   type balognr,
  w_count    type i.
data: lt_log  like ztpp_bfstep2_log occurs 0 with header line.
data: ls_par type zsap_step2_par_setting.

selection-screen begin of block b2 with frame title text-002.
select-options:
  sel_date   for if_date default sy-datlo,
  sel_unam   for if_unam default sy-uname.
selection-screen end of block b2.

selection-screen begin of block b1 with frame title text-001.
parameters:
  pa_proto   type ppc_proto default charx,
  pa_limit   type ppc_count default gc_maxitems.
selection-screen end of block b1.

selection-screen begin of block b3 with frame title text-004.
parameters: p_no_par radiobutton group grp1 default 'X',
            p_do_par radiobutton group grp1 ,

            p_srvgrp type serv_group,  "LIKE rzllitab-classname,
            p_max_wp type /sdf/trfc_wps,
            p_cpr    type zppc_cpr no-display.
selection-screen skip.
parameters: p_wttime type ppc_mwwaittime default 20,
            p_retryc type ppc_retry_com default 3,
            p_retrys type ppc_retry_sys default 3,
            p_retryr type ppc_retry_res default 3,
            p_st_new type ppc_retry_res default 1,
p_flag   type c no-display.
selection-screen end of block b3.

*----------------------------------------------------------------------*
*  MODULE user_command INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module user_command input.
  loop at screen.
    if screen-name = 'P_SRVGRP' or
       screen-name = 'P_MAX_WP' or
       screen-name = 'P_RETRYC' or
       screen-name = 'P_RETRYS' or
       screen-name = 'P_RETRYR' or
       screen-name = 'P_WTTIME'.
      if p_do_par is initial.
        clear screen-input.
      else.
        screen-input = 1.
      endif.
    endif.
    modify screen.
  endloop.
endmodule.                    "user_command INPUT

*----------------------------------------------------------------------*
*  MODULE user_command OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module user_command output.
  loop at screen.
    if screen-name = 'P_SRVGRP' or
       screen-name = 'P_MAX_WP' or
       screen-name = 'P_RETRYC' or
       screen-name = 'P_RETRYS' or
       screen-name = 'P_RETRYR' or
       screen-name = 'P_WTTIME'.
      if p_do_par is initial.
        clear screen-input.
      else.
        screen-input = 1.
      endif.
    endif.
    modify screen.
  endloop.
endmodule.                    "user_command INPUT

*-------------------
at selection-screen.
*-------------------
  if sy-ucomm eq 'ONLI'.
* parameter check
    if pa_limit gt gc_maxlimit or pa_limit lt gc_lowlimit.
      message e004(ppc1pa) with pa_limit gc_lowlimit gc_maxitems.
    endif.

    select single classname into w_classname
                            from rzllitab
                            where classname = p_srvgrp.
    if sy-subrc ne 0 and p_do_par is not initial.
      message id 'ZMPP' type 'E' number '001' with text-003.
    endif.
  endif.

*------------------
start-of-selection.
*------------------
  clear p_cpr.
  loop at sel_date into wa_seldate.
    append wa_seldate to date_range.
  endloop.
  loop at sel_unam into wa_selunam.
    append wa_selunam to unam_range.
  endloop.

*Maintain settings for parallelization
  ls_par-parallel_flg = p_do_par.
  ls_par-p_srvgrp = p_srvgrp.
  ls_par-p_max_wp = p_max_wp.
  ls_par-p_wttime = p_wttime.
  ls_par-p_retryc = p_retryc.
  ls_par-p_retrys = p_retrys.
  ls_par-p_retryr = p_retryr.
  ls_par-p_st_new = p_st_new.
  ls_par-p_cpr    = p_cpr.


  call function 'ZSAP_PPC1PR_STEP2_EXE'
    exporting
      if_date_range    = date_range
      if_user_range    = unam_range
      if_limit         = pa_limit
      if_protocol_show = pa_proto
      group_name       = p_srvgrp
      i_flag           = p_flag
      is_par           = ls_par
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
* That's it. The end.
