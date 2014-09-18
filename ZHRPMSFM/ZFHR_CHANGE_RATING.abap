function zfhr_change_rating.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_STATUS) TYPE  HAP_AP_STATUS
*"  EXPORTING
*"     VALUE(E_ROW_IID) TYPE  HAP_ROW_IID
*"  TABLES
*"      T_BODY_ELEMENTS STRUCTURE  HAP_S_BODY_ELEMENTS
*"      T_BODY_CELLS STRUCTURE  HAP_S_BODY_CELLS
*"----------------------------------------------------------------------

  data: lt_hrp9873 type table of hrp9873.
  data: ls_hrp9873 like line of lt_hrp9873,
        ls_hrp9871 type hrp9871,
        ls_rtrg  type zthr_rtrg,
        ls_t77s0 type t77s0,
        l_dec(3) type p decimals 2,
        l_rating type rating,
        l_score  type zdhr_score.
  data: l_pstext type profc_text.

  data: l_myey type zdhr_myey.
  data: lt_body_elements type hap_t_body_elements,
        lt_body_cells    type hap_t_body_cells,
        ls_body_elements type hap_s_body_elements,
        ls_body_cells    type hap_s_body_cells.
  data: lt_hrp1001       type table of hrp1001,
        ls_hrp1001       type hrp1001.

  data: rt_objid         type range of hrobjid,
        rs_objid         like line of rt_objid.

  data: c_column_iid_zp16 type hap_column_iid value '10'.
  data: c_column_iid_zp15 type hap_column_iid value '06'.
  data: c_column_iid_fapp type hap_column_iid value '09'.

  case i_status.
    when '3'.
      l_myey = 'MY'.
    when others.
      l_myey = 'EY'.
  endcase.

  lt_body_elements = t_body_elements[].
  lt_body_cells = t_body_cells[].

  select * from hrp9873
    into table lt_hrp9873
    where plvar = '01'
      and otype = 'VC'
      and istat in ('3', '4')
      and begda <= sy-datum
      and endda >= sy-datum
      and role_id in ('Z0','Z7','Z9')
      and myey = l_myey.
  sort lt_hrp9873 by role_id.


  sort lt_body_elements by row_iid.
  sort lt_body_cells by row_iid column_iid.

  " read VA element
  read table lt_body_elements into ls_body_elements
                              index 1.
  if sy-subrc ne 0.
    exit.
  endif.

  " get VB elements
  clear lt_hrp1001[].
  select * from hrp1001
    into table lt_hrp1001
    where otype = ls_body_elements-element_type
      and objid = ls_body_elements-element_id
      and plvar = '01'
      and rsign = 'B'
      and relat = '605'
      and istat in ('3','4')
      and begda <= sy-datum
      and endda >= sy-datum.

  check lines( lt_hrp1001 ) > 0.
  sort lt_hrp1001 by sobid.
  delete adjacent duplicates from lt_hrp1001 comparing sobid.

  clear rt_objid[].
  loop at lt_hrp1001 into ls_hrp1001.
    rs_objid-sign = 'I'.
    rs_objid-option = 'EQ'.
    rs_objid-low = ls_hrp1001-sobid.
    append rs_objid to rt_objid.clear rs_objid.
  endloop.

  select single * from hrp9871
    into ls_hrp9871
   where plvar = '01'
     and otype = 'VB'
     and objid in rt_objid
     and istat in ('3','4')
     and begda <= sy-datum
     and endda >= sy-datum
     and igid = 6.

  sort lt_body_elements by element_id.
  sort lt_body_cells by row_iid column_iid.

  loop at lt_hrp9873 into ls_hrp9873.
    read table lt_body_elements into ls_body_elements
                                with key element_id = ls_hrp9873-objid
                                binary search.
    if sy-subrc = 0.
      read table lt_body_cells into ls_body_cells
                               with key row_iid = ls_body_elements-row_iid
                                        column_iid = c_column_iid_zp16
                               binary search.
      if ls_body_cells-value_num > 0.
        l_score = ls_body_cells-value_num.
      endif.
    endif.
  endloop.

  if l_score >= 0.
    l_rating = l_score.
* get rating id
    select single * from zthr_rtrg
      into ls_rtrg
     where zdhrfr <= l_score
       and zdhrto >= l_score.
    if sy-subrc = 0.
      l_dec = l_rating - l_score.
      if abs( l_dec ) = '0.50'.
        subtract 1 from l_rating.
      endif.
* get rating text
      select single * from t77s0
        into ls_t77s0
        where grpid = 'ZPMS'
          and semid = 'RATNG'.
      select single pstext from t77tp
        into l_pstext
        where langu = sy-langu
          and scale_id = ls_t77s0-gsval
          and rating = l_rating.
    endif.
  endif.

  if not l_pstext is initial.
    read table lt_body_elements into ls_body_elements
                                with key element_id = ls_hrp9871-objid.
    if sy-subrc eq 0.
      ls_body_cells-value_num  = l_score.
      ls_body_cells-value_text = l_pstext.

      case i_status.
        when '3'.
          modify t_body_cells from ls_body_cells
                      transporting value_num value_text
                             where row_iid = ls_body_elements-row_iid
                               and column_iid = c_column_iid_zp15.
        when others.
          modify t_body_cells from ls_body_cells
                      transporting value_text
                             where row_iid = ls_body_elements-row_iid
                               and column_iid = c_column_iid_fapp.
          modify t_body_cells from ls_body_cells
                      transporting value_num
                             where row_iid = ls_body_elements-row_iid
                               and column_iid = c_column_iid_zp16.
      endcase.

    endif.
  endif.

  e_row_iid = ls_body_elements-row_iid.

endfunction.
