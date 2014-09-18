function z_co_get_vendor_source.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(MATNR) TYPE  MATNR
*"     REFERENCE(WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(AVAILABLE_DATE) TYPE  DATUM
*"     REFERENCE(SUB_PART) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(USE_SOURCE_LIST) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(LIFNR) TYPE  LIFNR
*"     REFERENCE(USED_SOURCE) TYPE  TABNAME16
*"     REFERENCE(EKORG) TYPE  EKORG
*"     REFERENCE(INFNR) TYPE  INFNR
*"  EXCEPTIONS
*"      NO_SOURCE_FOUND
*"      INVALID_WERKS
*"----------------------------------------------------------------------

  tables: a018, eina, eine, konp.
  data: i_a018 like a018 occurs 0 with header line.

  data $lifnr type lifnr.
  data:   wa_tab(72) type c,
          atab like standard table of wa_tab with non-unique
                    default key initial size 5,
          btab like standard table of wa_tab with non-unique
                    default key initial size 5.
  data $matnr like matnr.

  $matnr = matnr.
  if matnr eq 'R16N'.
    $matnr = 'R18N'.
  endif.

  if sub_part eq 'X'.

    if werks is initial.
      raise invalid_werks.
    endif.

    perform get_vendor_info using matnr werks available_date
                         changing $lifnr ekorg infnr.

    used_source = 'Info'.

  endif.

  if sub_part eq space.

    if not werks is initial.
      concatenate 'WERKS eq^' '''' werks '''' into wa_tab.
      replace '^' with '' into wa_tab.
      append wa_tab to atab.
    endif.

    if not bukrs is initial.
      concatenate 'BUKRS eq^' '''' bukrs '''' into wa_tab.
      replace '^' with '' into wa_tab.
      append wa_tab to btab.
    endif.

    if use_source_list eq 'X'.
      select single lifnr into $lifnr
                   from eord
                  where matnr eq $matnr
                    and vdatu <= available_date
                    and bdatu >= available_date
                    and (atab).
      if sy-subrc eq 0.
        used_source = 'EORD'.
      endif.
    else.
      select single lifnr into $lifnr
                   from ztcou137
                  where (btab)
                    and matnr eq $matnr
                    and zdtfr <= available_date
                    and zdtto >= available_date .
      if sy-subrc eq 0.
        used_source = 'ZTCOU137'.
      endif.
    endif.

  endif.

  if $lifnr is initial.
    raise no_source_found.
  else.
    if infnr is initial.
      perform get_infnr using $matnr
                              $lifnr
                     changing infnr ekorg.
    endif.
    lifnr = $lifnr.
  endif.

endfunction.

*---------------------------------------------------------------------*
*       FORM get_infnr                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_MATNR                                                       *
*  -->  P_LIFNR                                                       *
*  -->  P_INFNR                                                       *
*---------------------------------------------------------------------*
form get_infnr using p_matnr
                     p_lifnr
            changing p_infnr p_ekorg.

  select single a~infnr b~ekorg into (p_infnr,p_ekorg)
    from eina as a
    join eine as b
    on b~infnr eq a~infnr
   where a~matnr = p_matnr
     and a~lifnr = p_lifnr.

endform.

*---------------------------------------------------------------------*
*       FORM get_vendor_info                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_BUKRS                                                       *
*  -->  P_MATNR                                                       *
*  -->  P_WERKS                                                       *
*  -->  P_AVAILABLE_DATE                                              *
*---------------------------------------------------------------------*
form get_vendor_info using p_matnr p_werks p_available_date
                  changing p_lifnr p_ekorg p_infnr.

  data: i_a018 like a018 occurs 0 with header line.

  data   $lifnr type lifnr.
  data:   wa_tab(72) type c,
          atab like standard table of wa_tab with non-unique
                    default key initial size 5.

  data: l_cnt  type i,
        l_send_notice type i.
  data: l_flg_low(1) type c,
        l_categ(8)   type c.
  clear l_flg_low.
  data : l_ekorg type t001w-ekorg.

  data: begin of it_knumh occurs 0,
          knumh  like konh-knumh,
          datab  like konh-datab, "Valid-from date
          datbi  like konh-datbi,
          lifnr  like lfa1-lifnr,
          kstbmt type kstbmt,                               "3 digit
          kbetr  like konp-kbetr, "rate
          kpein  like konp-kpein, "pricing unit
          ekorg  like eine-ekorg,
          infnr  like eina-infnr,
        end   of it_knumh.
  data: begin  of it_info occurs 0,
          lifnr  like eina-lifnr,
          infnr  like eina-infnr,
        end of it_info.

  select single ekorg into l_ekorg from t001w
         where werks = p_werks.

  select * into corresponding fields of table it_info
    from eina as a inner join eine as b
      on a~infnr = b~infnr
   where a~matnr = p_matnr
     and a~loekz = ' '
     and b~werks = ' '
     and b~ekorg = l_ekorg
     and b~loekz = ' '.

  loop at it_info.
*---Material Info-Price Record
    select knumh datab lifnr ekorg
      into corresponding fields of it_knumh
      from a018
     where kappl =  'M'
       and kschl =  'PB00'
       and matnr =  p_matnr
       and lifnr =  it_info-lifnr
       and esokz =  '0'
       and datbi >=  p_available_date   "Valid To
       and datab <=  p_available_date.  "Valid from
      if sy-subrc = 0.
        it_knumh-infnr = it_info-infnr.
        append it_knumh.
      endif.
    endselect.
  endloop.

  describe table it_knumh lines sy-index.
**----- OK. Determine Vendor.
  if sy-index = 1.

    p_lifnr = it_knumh-lifnr.  "Fixed vendor.
    p_ekorg = it_knumh-ekorg.
    p_infnr = it_knumh-infnr.

**----- Determine Newest Valid From, lowest price
  elseif sy-index > 1.

    if l_flg_low = 'X'.
      loop at it_knumh.
        select single * from konp
         where knumh = it_knumh-knumh
           and kappl = 'M'
           and kschl = 'PB00'.
        it_knumh-kstbmt = konp-kbetr / konp-kpein.
        it_knumh-kbetr  = konp-kbetr.
        it_knumh-kpein  = konp-kpein.
        modify it_knumh.
      endloop.

      sort it_knumh by datab  descending
                       kstbmt ascending.
      read table it_knumh index 1.
      if sy-subrc = 0.
        p_lifnr = it_knumh-lifnr.  "Fixed vendor.
        p_ekorg = it_knumh-ekorg.
        p_infnr = it_knumh-infnr.
      endif.
    endif.
  endif.
endform.
