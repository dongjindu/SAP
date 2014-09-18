report rpcpcc00 message-id rp line-size 250.
type-pools: slis.
tables: pcl1, pcl2.

include <icon>.                        "Icon List
* International includes
include rpc2cd09.                      "Cluster CD Data-Definition
include rpc2ruu0.                      "Cluster RU Data-Definition
include rpc2rx09.                      "Cluster RU Data-Definition
include rpppxd00.                      "Data definition buffer PCL1/PCL2
include rpppxd10.                      "Common part buffer PCL1/PCL2
include rpppxm00.                      "Buffer handling routine

* Program includes
include rpcpcc0d.
include rpcpcc0s.
include rpcpcc0f.
include rpcpcc0o.
include rpcpcc0i.

selection-screen function key 2.


initialization.
** changed by Furong on 10/27/2006
*  CHECK sy-batch IS INITIAL.
*  PERFORM determine_role.
  e_mode = 'ADM'.
** end of change
  if e_mode is initial.
    message s016 with text-001.
    leave program.
  endif.
  if e_mode eq manager.
    clear p_ex_acc.
    move 'Cost Center List'(002) to sscrfields-functxt_02.
    free_sel_button-text = space.
    free_sel_button-icon_id = icon_mapped_relation.
    free_sel_button-icon_text = 'Cost Center List'(002).
    sscrfields-functxt_02 = free_sel_button.
    set titlebar '001'.
    perform get_cc_for_manager.
    perform move_struc_to_cctab.
  else.
    set titlebar '002'.
    ex_func-fcode = 'FC02'.
    append ex_func.
    call function 'RS_SET_SELSCREEN_STATUS'
         exporting
              p_status  = sy-pfkey
              p_program = sy-cprog
         tables
              p_exclude = ex_func.
  endif.

start-of-selection.
  perform initialize.
  perform set_flags.
  perform get_runids_for_employee_select.
** changed by Furong on 10/27/2006
*  PERFORM get_kostl_aufnr_for_pernr.
** end of change
  perform get_wage_types.
  perform get_cc_and_post_date.
  if select_kostl[] is initial.
    message i016 with 'No Data Selected'(i05).
    stop.
  endif.

  perform get_payroll_runid_posted.
  perform select_relative_postings.

  if select_kostl[] is initial.
    message i016 with 'No Data Selected'(i05).
    stop.
  endif.

  perform get_crossref_runid_docnum.
  perform z_get_detail_empl_info. " by ig.moon 4/6/2010
  perform get_wage_type_text.
  perform extract_relative_info_only.
  perform check_authorizations.

  if not list_table[] is initial.
    export list_table[] to memory id 'LIST'.
*    PERFORM display_progress USING 'D' '80'.
*    PERFORM set_default_sort_order
*      USING 'KOSTL' 'PERNR' 'HKONT' 'LGART' 'BUDAT'.
*    PERFORM load_variant.
*    IF sy-batch IS INITIAL.
*      CALL SCREEN '0100'.
*    ELSE.
*      PERFORM create_alv_spool.
*    ENDIF.
  else.
    message i016 with 'No Data Selected'(i05).
    stop.
  endif.

end-of-selection.

*---------------------------------------------------------------------*
*       FORM z_get_detail_empl_info                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form z_get_detail_empl_info.

  perform display_progress using 'C' '50'.
*This table is the index of payroll results (posted lines).
* select data from ppoix (will be used for 'A' Sets Calculations)
  if p_ex_acc = 'X'.
    select pernr                       "EE Number
           seqno                       "Sequential number
           actsign                     "Indicator: Status of record
           runid                       "Number of posting run
           postnum                     "Number
           tslin                       "Line number of data transfer
           lgart                       "Wage Type
           betrg                       "Amount
           waers                       "Currency
           anzhl                       "Number
           meins                       "Base unit of measure
           spprc         "Special processing of posting items
           momag         "Transfer to FI/CO:EE grouping for acct determi
           komok         "Transfer to FI/CO: Symbolic account
           mcode                       "Matchcode search term
           koart                       "Account assignment type
           auart                       "Expenditure type
           nofin         "Indicator: Expenditure type is not funded
             into corresponding fields of table i_ppoix
             from ppoix
             for all entries in run_doc_xref
             where runid  = run_doc_xref-runid
               and tslin  = run_doc_xref-linum
               and spprc  <> 'A'
               and lgart in wgtyp
               and pernr in g_org_pernrs.
  else.
    select pernr                       "EE Number
           seqno                       "Sequential number
           actsign                     "Indicator: Status of record
           runid                       "Number of posting run
           postnum                     "Number
           tslin                       "Line number of data transfer
           lgart                       "Wage Type
           betrg                       "Amount
           waers                       "Currency
           anzhl                       "Number
           meins                       "Base unit of measure
           spprc         "Special processing of posting items
           momag         "Transfer to FI/CO:EE grouping for acct determi
           komok         "Transfer to FI/CO: Symbolic account
           mcode                       "Matchcode search term
           koart                       "Account assignment type
           auart                       "Expenditure type
           nofin         "Indicator: Expenditure type is not funded
             into corresponding fields of table i_ppoix
             from ppoix
             for all entries in run_doc_xref
             where runid  = run_doc_xref-runid
               and tslin  = run_doc_xref-linum
               and lgart in wgtyp
               and pernr in g_org_pernrs.
  endif.

* select data from ppopx (Will be used for 'P'Sets Calculations)
  select pernr                         "EE Number
         seqno                         "Sequential Number
         runid                         "Number of posting run
         postnum                       "Number
         tslin                         "Line number of data transfer
         actsign                       "Indicator: Status of record
           into corresponding fields of table i_ppopx
           from ppopx
           for all entries in run_doc_xref
           where runid  = run_doc_xref-runid
           and   tslin  = run_doc_xref-linum
           and   pernr in g_org_pernrs.

************************************************************************
**** Do       Any        Recalculations    due   to   Retroactivity ****
************************************************************************
*Transfer Structure for FB HR_EVAL_PROD_A_RUN_GET_TABLE
 data: productive_runs like hrpp_pernr_runtab occurs 0 with header line.
  data: prod_runs_db type sorted table of hrpp_pernr_runtab
                  with unique key pernr seqno with header line,
        ppoix_for_ppopx  type sorted table of ppoix
                   with unique key pernr seqno postnum with header line.
*
  data: begin of lt_ppoix_key occurs 0,
         pernr    like ppoix-pernr,
         seqno    like ppoix-seqno,
         actsign  like ppoix-actsign,
         runid    like ppoix-runid,
         postnum  like ppoix-postnum,
        end of lt_ppoix_key.

  data: tabix like sy-tabix.

*i_ppopx = p-sets of data
  sort i_ppopx by pernr seqno postnum.
*
* read corresponding productive ppoix-runids with Function
  loop at i_ppopx.
    move-corresponding i_ppopx to productive_runs.
    clear productive_runs-runid.
    collect productive_runs.
  endloop.

  call function 'HR_EVAL_PROD_A_RUN_GET_TABLE'
       tables
            result_table = productive_runs.

* check if every runid was found
  clear productive_runs-runid.
  read table productive_runs with key runid = productive_runs-runid.
  if sy-subrc eq 0.
    write:/ 'There is no PPOIX entry for the PPOPX entry EE'(i55),
                productive_runs-pernr, 'SEQNO', productive_runs-seqno.
  endif.
  prod_runs_db[] = productive_runs[].
* fill ppoix-keys for needed ppoix
  tabix = 0.
  clear prod_runs_db.
  loop at i_ppopx.
    while i_ppopx-pernr ne prod_runs_db-pernr or
          i_ppopx-seqno ne prod_runs_db-seqno.
      tabix = tabix + 1.
      read table prod_runs_db index tabix.
    endwhile.
    clear lt_ppoix_key.
    move-corresponding prod_runs_db to lt_ppoix_key.
    lt_ppoix_key-postnum = i_ppopx-postnum.
    lt_ppoix_key-actsign = 'A'.
    collect lt_ppoix_key.
  endloop.
*
* read corresponding ppoix-data from database
  clear ppoix_for_ppopx. refresh ppoix_for_ppopx.

  read table prod_runs_db index 1.
  if sy-subrc eq 0.
    select pernr                       "EE Number
         seqno                         "Sequential number
         actsign                       "Indicator: Status of record
         runid                         "Number of posting run
         postnum                       "Number
         tslin                         "Line number of data transfer
         lgart                         "Wage Type
         betrg                         "Amount
         waers                         "Currency
         anzhl                         "Number
         meins                         "Base unit of measure
         spprc           "Special processing of posting items
         momag           "Transfer to FI/CO:EE grouping for acct determi
         komok           "Transfer to FI/CO: Symbolic account
         mcode                         "Matchcode search term
         koart                         "Account assignment type
         auart                         "Expenditure type
         nofin           "Indicator: Expenditure type is not funded
           from ppoix
           into corresponding fields of table ppoix_for_ppopx
                             for all entries in lt_ppoix_key
                             where pernr   = lt_ppoix_key-pernr
                             and   seqno   = lt_ppoix_key-seqno
                             and   lgart   in wgtyp
                             and   actsign = 'A'
                             and   runid   = lt_ppoix_key-runid
                             and   postnum = lt_ppoix_key-postnum.

  endif.

* fill rest-info from corresponding ppoix-set into ppopx-sets
* and switch sign of ppopx-amounts and append them to ppoix-lines

  tabix = 0.
  loop at i_ppopx.
    clear i_ppoix.
    loop at ppoix_for_ppopx where pernr   = i_ppopx-pernr
                            and   seqno   = i_ppopx-seqno
                            and   postnum = i_ppopx-postnum.
      move-corresponding ppoix_for_ppopx to i_ppoix.
      move-corresponding i_ppopx  to i_ppoix.
* Switch sign for P-entries
      if i_ppoix-actsign <> 'A'.
        i_ppoix-betrg = - i_ppoix-betrg.
        i_ppoix-anzhl = - i_ppoix-anzhl.
      endif.
      append i_ppoix.
    endloop.
  endloop.

  sort run_doc_xref  by runid linum.

*fill in information from i_ppdix line
  loop at i_ppoix.

    read table run_doc_xref  with key runid  = i_ppoix-runid
                                      linum  = i_ppoix-tslin
* by ig.moon 4/6/2010 {
                                      binary search.
* }
    if sy-subrc ne 0.
      write:/ 'No corresponding entry found for Line Item:'(i54),
                                            i_ppoix-runid,
                                            i_ppoix-tslin.
    endif.

    move-corresponding run_doc_xref to i_ppoix.
    modify i_ppoix.

  endloop.


*Fill the employee table.
  clear:empl_account.  refresh: empl_account.
*
  clear molgas.
  molgas-sign = 'I'.
  molgas-option = 'EQ'.

  empl_account-molga = '10'.

  loop at i_ppoix.

*    call function 'HR_COUNTRYGROUPING_GET'
*         exporting
*              pernr     = i_ppoix-pernr
*         importing
*              molga     = empl_account-molga
*         exceptions
*              not_found = 1
*              others    = 2.

*    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    else.
      if molgas-low <> empl_account-molga.
        read table molgas with key low = empl_account-molga.
        if sy-subrc <> 0.
          molgas-low = empl_account-molga.
          append molgas.
        endif.
      endif.
      move-corresponding i_ppoix to empl_account.
      append empl_account.
*    endif.
  endloop.
*
endform.                               " GET_DETAIL_EMPL_INFO
