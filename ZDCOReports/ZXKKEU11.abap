*----------------------------------------------------------------------*
*   by Andy
*----------------------------------------------------------------------*
tables: mvke, mara, kna1.
data: ce0_h201 like ce0h201,
      l_mtart  like mara-mtart,
      l_prodh  like mvke-prodh,
      l_mvgr3  like mvke-mvgr3,
      l_mvgr4  like mvke-mvgr4,
      l_mvgr5  like mvke-mvgr5,
      l_kunnr  like kna1-kunnr.

ce0_h201 = i_copa_item.

if i_operating_concern = 'H201'.
  e_exit_is_active = 'X'.
* INCLUDE zxkkeu11_h201.

*----------------------------------------------------------------------*
*   INCLUDE ZXKKEU11_H201                                              *
*----------------------------------------------------------------------*

*KE4S

*--Reset CUSTOMER NUMBER
* except Wholesale, Part Sales, Scrap Sale
  if ce0_h201-kndnr <> space.
    select count( * ) into sy-index from knvv
      where kunnr = ce0_h201-kndnr
        and ( vtweg = '10' or vtweg = '30' or vtweg = '50' )
        and loevm = space.    "NOT DELETED
    if sy-index = 0.
      clear: ce0_h201-kndnr,
             ce0_h201-kmland.
    endif.
  endif.


*--Reset PRODUCT NUMBER
*Dist.Ch=warranty/campaign
  if ce0_h201-vtweg = '40'.
    clear: ce0_h201-artnr.
  endif.
* Billing type=space & non-sellable one

  if ce0_h201-artnr+13(1) = ' '.  "old BOM
    if  ce0_h201-fkart = space and
    ( ce0_h201-artnr+5(2) ca 'XXY' ).
      clear: ce0_h201-artnr.
    endif.
  else.

*+ by ig.moon 3/31/2008 {
    if  ce0_h201-fkart = space and
    ( ce0_h201-artnr+5(1) ca 'WP' ).
      clear: ce0_h201-artnr.
    endif.
* }
  endif.

* Reset for MM auto-posting
  if ce0_h201-kstar = '0000531000'.
    clear: ce0_h201-artnr.
* clear plant also... temporary ;;; FIXME
    clear: ce0_h201-werks.
  endif.


  if ce0_h201-artnr <> space.
*..get product type
    select single mtart prdha into (l_mtart, l_prodh)
           from mara
           where matnr = ce0_h201-artnr.

* derive customer  (FIXME)
*--- actual; A004 - Vehicle; ZV00
*            A005 - MIP; ZS00
*--- plan; derive from A9xx...
    if ce0_h201-kndnr = space.
      if l_mtart = 'FERT'.
        if ce0_h201-artnr+13(1) = ' '.  "old BOM
          l_kunnr = ce0_h201-artnr+1(5).
        else.
          select single old_dealer into l_kunnr from ztebpp_deal_conv
                where new_dealer = ce0_h201-artnr+4(1).
          concatenate ce0_h201-artnr+1(3) l_kunnr(2) into l_kunnr.
        endif.
        select single kunnr into l_kunnr from kna1
                                         where kunnr = l_kunnr.
        if sy-subrc = 0.
          ce0_h201-kndnr = l_kunnr.
        endif.

      else.
*...get from pricing (MIP; ZS00)
        select single kunnr into ce0_h201-kndnr
               from a005
               where kappl = 'V'
                 and kschl = 'ZP00'
                 and matnr = ce0_h201-artnr
                 and datab <= sy-datum
                 and datbi >= sy-datum.
      endif.
    endif.
  endif.

*derive partner of customer
*if ce0_h201-kndnr <> space.
*  data: l_kunn2 like knvp-kunn2.
*  select single kunn2 into l_kunn2
*    from knvp
*    where kunnr = ce0_h201-kndnr
*      and parvw = 'RE'.
*  if sy-subrc = 0.
*    ce0_h201-kndnr = l_kunn2.
*  endif.
*endif.

*-Derive plant, if blank, user default plant 'P001'
  if ce0_h201-artnr = space.
    clear: ce0_h201-werks.
  else.
    if ce0_h201-werks = space.
      ce0_h201-werks = 'P001'.
    endif.
  endif.


* derive sales org
*IF ce0_h201-vkorg = space and ce0_h201-kndnr <> space.
*  SELECT SINGLE * FROM kna1 WHERE kunnr = ce0_h201-kndnr.
*  IF kna1-land1 = 'US'.
*    ce0_h201-vkorg = 'D100'.
*  ELSE.
*    ce0_h201-vkorg = 'E100'.
*  ENDIF.
*ENDIF.

* general product...
*if ce0_h201-prodh   = space and ce0_h201-artnr <> space.

  if l_mtart = 'FERT'.
* derive material group info
    select single prodh mvgr3 mvgr4 mvgr5
           into  (l_prodh, l_mvgr3, l_mvgr4, l_mvgr5)
           from mvke
           where matnr = ce0_h201-artnr.


    if sy-dbcnt = 0.
      e_failed = 'X'.
      message s000(zmco) with 'No Material Group'.
    else.
      ce0_h201-prodh = l_prodh.
      ce0_h201-mvgr3 = l_mvgr3.
      ce0_h201-mvgr4 = l_mvgr4.
      ce0_h201-mvgr5 = l_mvgr5.  "Sales View
    endif.

*production view (GL, GLS)
    if ce0_h201-artnr+13(1) = ' '.  "old BOM
      ce0_h201-wwtrm = ce0_h201-artnr+9(1).
    else.
*+ by ig.moon 3/31/2008 {
      ce0_h201-wwtrm = l_mvgr5.
* }
    endif.

  elseif l_prodh <> space.
    ce0_h201-prodh = l_prodh.
  endif.

*endif.

*if prod.hier blank, fill from model
  if ce0_h201-prodh = space.
    if ce0_h201-paph2 <> space.
      ce0_h201-prodh = ce0_h201-paph2.
      ce0_h201-paph1 = ce0_h201-paph2(5).
    endif.
  else.
    if ce0_h201-prodh+10(5) <> space.
      ce0_h201-paph2 = ce0_h201-prodh.
    endif.
    if ce0_h201-prodh+5(5) <> space.
      ce0_h201-paph2 = ce0_h201-prodh(10).
    endif.
    ce0_h201-paph1 = ce0_h201-prodh(5).
  endif.

*return
  e_copa_item = ce0_h201.
endif.
