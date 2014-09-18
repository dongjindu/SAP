*////////////////////////////////////////////////////////////
* Update ztpp_pmt07jb_b table
*////////////////////////////////////////////////////////////

report zpp_error_corr_mar_25 no standard page heading
                         line-size  100
                         line-count 65
                         message-id zmco.

tables : ztpp_pmt07jb_b, ztpp_wosum .

parameters p_modl like ztpp_pmt07jb_b-modl default 'CRA'.
parameters p_test as checkbox default 'X'.

data: begin of itab1 occurs 0,
        ordr	type zordr,
        dist	type zdist,
        extc	type zextc,
        intc	type zintc,
      end   of itab1.

data: begin of itab2 occurs 0,
        wo_ser	type zwo_ser,
        nation	type znation,
        dealer	type zdealer,
        extc	type zextc,
        intc	type zintc,
        version type zvers,
        dist type zdist,
      end   of itab2.

start-of-selection.

  clear : itab1[], itab1, itab2[], itab2.

  select ordr dist extc intc into table itab1
  from ztpp_pmt07jb_b
  where modl eq p_modl.

  if sy-subrc ne 0 .
    message s000 with 'No data was found ztpp_pmt07jb_b '.
    stop.
  endif.

  select
        wo_ser
        nation
        dealer
        extc
        intc
        version
    into table itab2
    from ztpp_wosum
    for all entries in itab1
    where wo_ser eq itab1-ordr
      and nation eq itab1-dist(3)
      and dealer eq itab1-dist+3(2)
      and extc   eq itab1-extc
      and intc   eq itab1-intc.

  if sy-subrc ne 0 .
    message s000 with 'No data was found in ztpp_wosum '.
    stop.
  endif.

  loop at itab2 .
    concatenate itab2-nation  itab2-dealer into itab2-dist.

    update ztpp_pmt07jb_b set vers = itab2-version
                              pver = itab2-version

    where ordr eq itab2-wo_ser
      and dist eq itab2-dist
      and extc eq itab2-extc
      and intc eq itab2-intc
      and modl eq p_modl.

  endloop.

  if p_test eq 'X'.
    rollback work.
  else.
    if sy-subrc eq 0.
      commit work.
    else.
      rollback work.
    endif.
  endif.
