*----------------------------------------------------------------------*
*   INCLUDE ZXKKEU11_H201                                              *
*----------------------------------------------------------------------*

*KE4S

*--Reset CUSTOMER NUMBER
* except Wholesale, Part Sales, Scrap Sale
IF ce0_h201-kndnr <> space.
  SELECT COUNT( * ) INTO sy-index FROM knvv
    WHERE kunnr = ce0_h201-kndnr
      AND ( vtweg = '10' OR vtweg = '30' OR vtweg = '50' )
      AND loevm = space.    "NOT DELETED
  IF sy-index = 0.
    CLEAR: ce0_h201-kndnr,
           ce0_h201-kmland.
  ENDIF.
ENDIF.


*--Reset PRODUCT NUMBER
*Dist.Ch=warranty/campaign
IF ce0_h201-vtweg = '40'.
  CLEAR: ce0_h201-artnr.
ENDIF.
* Billing type=space & non-sellable one
IF  ce0_h201-fkart = space AND ( ce0_h201-artnr+5(2) CA 'XXY' ).
  CLEAR: ce0_h201-artnr.
ENDIF.



IF ce0_h201-artnr <> space.
*..get product type
  SELECT SINGLE mtart prdha INTO (l_mtart, l_prodh)
         FROM mara
         WHERE matnr = ce0_h201-artnr.

* derive customer  (FIXME)
*--- actual; A004 - Vehicle; ZV00
*            A005 - MIP; ZS00
*--- plan; derive from A9xx...
  IF ce0_h201-kndnr = space.
    IF l_mtart = 'FERT'.
      l_kunnr = ce0_h201-artnr+1(5).
      SELECT SINGLE * FROM kna1 WHERE kunnr = l_kunnr.
      IF sy-subrc = 0.
        ce0_h201-kndnr = ce0_h201-artnr+1(5).
      ENDIF.
    ELSE.
*...get from pricing (MIP; ZS00)
      SELECT SINGLE kunnr INTO ce0_h201-kndnr
             FROM a005
             WHERE kappl = 'V'
               AND kschl = 'ZP00'
               AND matnr = ce0_h201-artnr
               AND datab <= sy-datum
               AND datbi >= sy-datum.
    ENDIF.
  ENDIF.
ENDIF.

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
IF ce0_h201-artnr = space.
  CLEAR: ce0_h201-werks.
ELSE.
  IF ce0_h201-werks = space.
    ce0_h201-werks = 'P001'.
  ENDIF.
ENDIF.


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

IF l_mtart = 'FERT'.
* derive material group info
  SELECT SINGLE prodh mvgr3 mvgr4 mvgr5
         INTO  (l_prodh, l_mvgr3, l_mvgr4, l_mvgr5)
         FROM mvke
         WHERE matnr = ce0_h201-artnr.


  IF sy-dbcnt = 0.
    e_failed = 'X'.
    MESSAGE s000(zmco) WITH 'NO Material Group'.
  ELSE.
    ce0_h201-prodh = l_prodh.
    ce0_h201-mvgr3 = l_mvgr3.
    ce0_h201-mvgr4 = l_mvgr4.
    ce0_h201-mvgr5 = l_mvgr5.  "Sales View
  ENDIF.

*production view (GL, GLS)
  ce0_h201-wwtrm = ce0_h201-artnr+9(1).

ELSEIF l_prodh <> space.
  ce0_h201-prodh = l_prodh.
ENDIF.

*endif.

*if prod.hier blank, fill from model
IF ce0_h201-prodh = space.
  IF ce0_h201-paph2 <> space.
    ce0_h201-prodh = ce0_h201-paph2.
    ce0_h201-paph1 = ce0_h201-paph2(5).
  ENDIF.
ELSE.
  IF ce0_h201-prodh+10(5) <> space.
    ce0_h201-paph2 = ce0_h201-prodh.
  ENDIF.
  IF ce0_h201-prodh+5(5) <> space.
    ce0_h201-paph2 = ce0_h201-prodh(10).
  ENDIF.
  ce0_h201-paph1 = ce0_h201-prodh(5).
ENDIF.
