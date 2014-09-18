function z_co_rate_generic_get.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_KOKRS) LIKE  ZTCOU140-KOKRS
*"     REFERENCE(I_KOSTL) LIKE  CSKS-KOSTL DEFAULT SPACE
*"     REFERENCE(I_KSTAR) LIKE  CSKA-KSTAR DEFAULT SPACE
*"  TABLES
*"      T_RATE STRUCTURE  ZCO140_GEN
*"----------------------------------------------------------------------


  data l_c_kostl like zco140_gen-cc.
  data l_c_kstar like zco140_gen-ce.
  ranges l_r_cc for zco140_gen-cc.
  ranges l_r_ce for zco140_gen-ce.

  write i_kostl to l_c_kostl no-zero.
  write i_kstar to l_c_kstar no-zero.
  __cls t_rate.

  call function 'Z_CO_RATE_GENERIC_RANGES'
       exporting
            i_cc   = l_c_kostl
            i_ce   = l_c_kstar
       tables
            t_r_cc = l_r_cc
            t_r_ce = l_r_ce.

*  select * from ztcou140
*  where kokrs = i_kokrs
*    and cc in l_r_cc
*    and ce in l_r_ce
*    order by kokrs cc ce descending.
*
*    move: ztcou140-cc   to t_rate-cc,
*          ztcou140-ce   to t_rate-ce,
*          ztcou140-rate to t_rate-rate,
*          sy-dbcnt      to t_rate-tabix.
*
*    append t_rate.
*
*  endselect.

*  data t_ztcou140 like ztcou140 occurs 0 with header line.

  data: begin of t_ztcou140 occurs 0.
          include structure ztcou140.
  data: $order(20).
  data: end of t_ztcou140.

  select * into table t_ztcou140
  from ztcou140 where kokrs = i_kokrs
                  and cc in l_r_cc
                  and ce in l_r_ce.

  loop at t_ztcou140.
    concatenate t_ztcou140-cc t_ztcou140-ce into t_ztcou140-$order.
    modify t_ztcou140 transporting $order.
  endloop.

  sort t_ztcou140 by $order descending.

*  loop at t_ztcou140 where kokrs = i_kokrs
*                         and cc in l_r_cc
*                         and ce in l_r_ce.
  loop at t_ztcou140.

    move: t_ztcou140-cc   to t_rate-cc,
          t_ztcou140-ce   to t_rate-ce,
          t_ztcou140-rate_e to t_rate-rate_e,
          t_ztcou140-rate_d to t_rate-rate_d,
          sy-tabix        to t_rate-tabix.

    append t_rate.

  endloop.

endfunction.
