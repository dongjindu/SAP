*&---------------------------------------------------------------------*
*& Report  ZDI_BF_WIP_PPCCOMAT
*&
*&---------------------------------------------------------------------*
*& spec by Andy Choi
*&
*&---------------------------------------------------------------------*
report  zdi_bf_wip_ppccomat_fix.
tables: ppc_comat, cpzp.

types: begin of ty_cpzpk,
         f_objnr  type f_objnr,
         gjper    type co_gjper,
         objnr    type j_objnr,
       end of ty_cpzpk.
data: it_cpzpk type sorted table of ty_cpzpk
               with unique key f_objnr gjper objnr.
data: lv_cpzpk   type ty_cpzpk.

data: it_cpzp_fr like cpzp occurs 0,
      it_cpzp_to like cpzp occurs 0.
data: lv_cpzp1 type cpzp,
      lv_cpzp2 type cpzp.

data: it_ppccomat like ppc_comat occurs 0 with header line.

data: $ix like sy-tabix.
ranges : r_fobj1  for  cpzp-f_objnr,
         r_objid  for  ppc_comat-objid.
data:    lv_fobj  like cpzp-f_objnr.

*--- ALV
type-pools: slis.
data : w_fieldcat type slis_t_fieldcat_alv with header line,
       w_eventcat type slis_t_event with header line,
       w_selfield type slis_selfield,
       w_sortcat  type slis_t_sortinfo_alv with header line,
       w_col_pos  type i,
       w_program  like sy-repid,
       w_top_of_page type slis_t_listheader,
       w_line1 type slis_listheader.

data: gt_fieldcat type slis_t_fieldcat_alv,
      gs_layout   type slis_layout_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_sorts    type slis_t_sortinfo_alv with header line,
      gs_prnt     type slis_print_alv,
      g_repid     like sy-repid.
*---- ALV

*&---------------------------------------------------------------------*
select-options: s_objid  for  ppc_comat-objid.
parameters:     p_objid  like ppc_comat-objid.

parameters:     p_upd as checkbox.
parameters:     p_mode(1) type c.

*&---------------------------------------------------------------------*
data: h_dontpanic   like sy-datlo.
*&---------------------------------------------------------------------*
at selection-screen output.
*&---------------------------------------------------------------------*
  get parameter id 'DONTPANIC' field h_dontpanic.
  loop at screen.
    if screen-name = 'P_UPD'.
      if h_dontpanic = sy-datlo.
        screen-input = '1'.
      else.
        screen-input = '0'.
      endif.
      modify screen.
    endif.
  endloop.
*&---------------------------------------------------------------------*
start-of-selection.
*&---------------------------------------------------------------------*

  case p_mode.
    when 'D'.
      perform fix_dup_comat_del.
    when 'C'.
      perform fix_dup_comat_copy.
    when 'W'.
      perform fix_cpzp_delete.

    when others.

      perform fix_dup_cpzp_read using sy-subrc.
      if sy-subrc = 0.
        perform fix_dup_cpzp_prepare.
        perform display_alv.

        if p_upd = 'X'.
          perform fix_dup_cpzp_update.
        endif.
      endif.
  endcase.

*&---------------------------------------------------------------------*
end-of-selection.
*&---------------------------------------------------------------------*


*ALV
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
form display_alv.
*  perform field_setting tables gt_fieldcat using :
*'TYPPS'   'Type'           '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

  g_repid = sy-repid.
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program = g_repid
*     it_fieldcat        = gt_fieldcat
      i_structure_name   = 'CPZP'
      i_save             = 'A'
    tables
      t_outtab           = it_cpzp_to
    exceptions
      program_error      = 1
      others             = 2.


endform.                    " display_alv
*&--------------------------------------------------------------------
*&      Form  field_setting
*&--------------------------------------------------------------------
form field_setting tables         p_fieldcat_t like gt_fieldcat
                   using          p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  data: ls_fieldcat type slis_fieldcat_alv.
  clear ls_fieldcat.

  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.

  if p_dosum = 'R'. "reference field / table
    ls_fieldcat-ref_tabname   = p_cfield.
    ls_fieldcat-ref_fieldname = p_qfield.
  else.
    ls_fieldcat-currency   = p_cfield.
    ls_fieldcat-qfieldname = p_qfield.

  endif.
  ls_fieldcat-do_sum     = p_dosum.

  append ls_fieldcat to p_fieldcat_t.

endform.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  FIX_DUP_CPZP_READ
*&---------------------------------------------------------------------*
form fix_dup_cpzp_read changing p_rc like sy-subrc.

  p_rc = 1.

  select * into table it_ppccomat from ppc_comat
     where objid in s_objid.
  check sy-subrc = 0.
  loop at it_ppccomat.
    r_fobj1-sign   = 'I'.
    r_fobj1-option = 'EQ'.
    concatenate 'MK00' it_ppccomat-objid into r_fobj1-low.
    append r_fobj1.

    r_objid-sign   = 'I'.
    r_objid-option = 'EQ'.
    r_objid-low    = it_ppccomat-objid.
    append r_objid.
  endloop.

* target ppc_comat
  select count( * ) from ppc_comat
     where objid = p_objid.
  check sy-subrc = 0.

* get all cpzp records
  select * into table it_cpzp_fr from cpzp
     where f_objnr in r_fobj1.
  sort it_cpzp_fr by gjper objnr.

  concatenate 'MK00' p_objid into lv_fobj.
  select * into table it_cpzp_to from cpzp
     where f_objnr = lv_fobj.

*-clean up unnecessary obj,period
  loop at it_cpzp_to into lv_cpzp2.
    $ix = sy-tabix.

    read table it_cpzp_fr into lv_cpzp1
               with key gjper = lv_cpzp2-gjper
                        objnr = lv_cpzp2-objnr
               binary search.
    if sy-subrc ne 0.
      delete it_cpzp_to index $ix.
    endif.
  endloop.

  sort it_cpzp_to by gjper objnr.
  p_rc = 0.

endform.                    " FIX_DUP_CPZP_READ
*&---------------------------------------------------------------------*
*&      Form  FIX_DUP_CPZP_PREPARE
*&---------------------------------------------------------------------*
form fix_dup_cpzp_prepare .

  loop at it_cpzp_fr into lv_cpzp1.

    read table it_cpzp_to into lv_cpzp2
        with key gjper = lv_cpzp1-gjper
                 objnr = lv_cpzp1-objnr
                 binary search.
    $ix = sy-tabix.
    if sy-subrc = 0.
      lv_cpzp2-istmn  = lv_cpzp2-istmn  + lv_cpzp1-istmn.
      lv_cpzp2-gmper  = lv_cpzp2-gmper  + lv_cpzp1-gmper.
      lv_cpzp2-xmper  = lv_cpzp2-xmper  + lv_cpzp1-xmper.
      lv_cpzp2-gmsum  = lv_cpzp2-gmsum  + lv_cpzp1-gmsum.
      lv_cpzp2-xmsum  = lv_cpzp2-xmsum  + lv_cpzp1-xmsum.
      lv_cpzp2-rewqty = lv_cpzp2-rewqty + lv_cpzp1-rewqty.
      lv_cpzp2-varmn  = lv_cpzp2-varmn  + lv_cpzp1-varmn.
      modify it_cpzp_to index $ix from lv_cpzp2.

    else.
      lv_cpzp1-f_objnr = lv_fobj.
      append lv_cpzp1 to it_cpzp_to.
      sort it_cpzp_to by gjper objnr.

    endif.
  endloop.

endform.                    " FIX_DUP_CPZP_PREPARE
*&---------------------------------------------------------------------*
*&      Form  FIX_DUP_CPZP_UPDATE
*&---------------------------------------------------------------------*
form fix_dup_cpzp_update .

  loop at it_cpzp_to into lv_cpzp2.
    modify cpzp from lv_cpzp2.
  endloop.
  delete from ppc_comat where objid in r_objid.

endform.                    " FIX_DUP_CPZP_UPDATE
*&---------------------------------------------------------------------*
*&      Form  FIX_DUP_COMAT_DEL
*&---------------------------------------------------------------------*
form fix_dup_comat_del.
  data: l_fobj like cpzp-f_objnr.

  select * into table it_ppccomat
     from ppc_comat
     where objid in s_objid.

  check sy-subrc = 0.

  loop at it_ppccomat.
    concatenate 'MK00' it_ppccomat-objid into l_fobj.
    select count( * ) from cpzp
       where f_objnr = l_fobj.

    if sy-dbcnt = 0.
      if p_upd = 'X'.
        delete from ppc_comat where objid = it_ppccomat-objid.
        write:/ 'Deleted...',  it_ppccomat-objid.
      else.
        write:/ 'Deleting...', it_ppccomat-objid.
      endif.
    endif.
  endloop.

endform.                    " FIX_DUP_COMAT_DEL
*&---------------------------------------------------------------------*
*&      Form  FIX_DUP_COMAT_COPY
*&---------------------------------------------------------------------*
form fix_dup_comat_copy.
  data: l_fobj like cpzp-f_objnr.

  select single * from ppc_comat
     where objid in s_objid.
  check sy-subrc = 0.

  ppc_comat-objid = p_objid.
  ppc_comat-bwtar = 'DUP'.
  if p_upd = 'X'.
    insert ppc_comat.
    write:/ 'Inserted...',  ppc_comat-objid.
  else.
    write:/ 'Inserting...', ppc_comat-objid.
  endif.

endform.                    " FIX_DUP_COMAT_DEL
*&---------------------------------------------------------------------*
*&      Form  FIX_CPZP_DELETE
*&---------------------------------------------------------------------*
form fix_cpzp_delete .

  concatenate 'MK00' p_objid into lv_fobj.
  select * into table it_cpzp_to from cpzp
     where f_objnr = lv_fobj.


  if p_upd = 'X'.
    insert ztco_cpzp from table it_cpzp_to accepting duplicate keys.
    delete from cpzp  where f_objnr = lv_fobj.

  endif.
endform.                    " FIX_CPZP_DELETE
