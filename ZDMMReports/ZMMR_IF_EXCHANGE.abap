*&---------------------------------------------------------------------*
*& Report  ZMMR_IF_EXCHANGE                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

report  zmmr_if_exchange                                            .

data: begin of it_rate occurs 0.
        include structure zsmm_if019.
data: bedat2 like vtbbewe-dbervon.
data : conv_date(10) type c,
      l_day like vtbbewe-atage.
data: end of it_rate.

data: t_zsmm_if019 like zsmm_if019 occurs 0 with header line.
data: t_return like bapireturn occurs 0 with header line.

data: t_zsmm_if023 like ztmm_if023 occurs 0 with header line.
data: v_zseq like ztmm_if023-zseq.
data: l_serno like ztmm_if023-zsen.

clear : t_zsmm_if023, t_zsmm_if023[].
clear : v_zseq, l_serno.
clear: it_rate, t_zsmm_if019, t_return.
refresh: it_rate, t_zsmm_if019, t_return.



select * into
corresponding fields of table it_rate
from tcurr
where kurst = 'M'
and tcurr = 'USD'.

*SELECT kurst fcurr tcurr
*  MAX( gdatu ) AS gdatu
*  INTO
*  CORRESPONDING FIELDS OF TABLE it_rate
*  FROM tcurr
* WHERE kurst = 'M'
* GROUP BY kurst fcurr tcurr.
*DATA: l_DAY LIKE VTBBEWE-DBERBIS.

if sy-subrc eq 0.
  loop at it_rate.

*    call function 'CONVERSION_EXIT_INVDT_OUTPUT'
*         EXPORTING
*              input  = it_rate-gdatu
*         IMPORTING
*              output = it_rate-conv_date.
*
*    translate it_rate-conv_date using '/ '.
*    condense it_rate-conv_date no-gaps.
**    it_rate-bedat = it_rate-conv_date.
*    concatenate it_rate-conv_date+4(4)
*                it_rate-conv_date+0(2)
*                it_rate-conv_date+2(2)
*                into it_rate-conv_date.
*    write it_rate-conv_date to it_rate-bedat.

    it_rate-bedat = sy-datum.

    modify it_rate.
    clear it_rate.
  endloop.

  sort it_rate ascending by fcurr tcurr gdatu.
  delete adjacent duplicates from it_rate comparing fcurr tcurr.
  data : l_length type i.
  loop at it_rate.
    clear l_length.
    l_length = strlen( it_rate-fcurr ).
    if l_length < 4.
      move-corresponding it_rate to t_zsmm_if019.
      t_zsmm_if019-usd_curr = 1.

      append t_zsmm_if019.

    endif.
    clear it_rate.
  endloop.

  t_zsmm_if019-kurst = 'M'.
  t_zsmm_if019-fcurr = 'USD'.
  t_zsmm_if019-tcurr = 'USD'.
  t_zsmm_if019-ukurs = 1.
  t_zsmm_if019-bedat = sy-datum.
  append t_zsmm_if019.

  call function 'ZMM_IF_EXCHANGE_RATE_OUTBOUND'
        destination 'VAATZ_HMMA'
    tables
      t_zsmm_if019       = t_zsmm_if019
      t_return           = t_return
    EXCEPTIONS
     COMMUNICATION_FAILURE = 1
     SYSTEM_FAILURE        = 2.

  if sy-subrc = 0.
    select max( zseq ) into v_zseq
    from ztmm_if023.

    if v_zseq is initial.
      v_zseq = 1.
    else.
      v_zseq = v_zseq + 1.
    endif.

    read table t_return.
    loop at t_zsmm_if019.
      move-corresponding t_zsmm_if019 to t_zsmm_if023.
      l_serno = l_serno + 1.
      move: v_zseq           to t_zsmm_if023-zseq,
            l_serno          to t_zsmm_if023-zsen,
            sy-datum         to t_zsmm_if023-erdat,
            sy-uzeit         to t_zsmm_if023-erzet,
            sy-uname         to t_zsmm_if023-ernam,
            t_return-type    to t_zsmm_if023-type,
            t_return-message to t_zsmm_if023-message.
      append t_zsmm_if023.
    endloop.
    insert ztmm_if023 from table t_zsmm_if023.
    commit work and wait.
  else.
    select max( zseq ) into v_zseq
    from ztmm_if023.

    if v_zseq is initial.
      v_zseq = 1.
    else.
      v_zseq = v_zseq + 1.
    endif.

    loop at t_zsmm_if019.
      move-corresponding t_zsmm_if019 to t_zsmm_if023.
      l_serno = l_serno + 1.
      move: v_zseq           to t_zsmm_if023-zseq,
            l_serno          to t_zsmm_if023-zsen,
            sy-datum         to t_zsmm_if023-erdat,
            sy-uzeit         to t_zsmm_if023-erzet,
            sy-uname         to t_zsmm_if023-ernam,
            'E'              to t_zsmm_if023-type,
            'COMMUNICATION FAILURE' to t_zsmm_if023-message.
      append t_zsmm_if023.
    endloop.
    insert ztmm_if023 from table t_zsmm_if023.
    commit work and wait.
  endif.

endif.
