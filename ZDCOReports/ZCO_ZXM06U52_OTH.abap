*----------------------------------------------------------------------*
*   INCLUDE ZCO_ZXM06U52_OTH                                           *
*----------------------------------------------------------------------*

  LOOP AT t_sources.
*-- check info-record exist...
    SELECT COUNT( * ) INTO l_cnt FROM a018
     WHERE kappl =  'M'
       AND kschl =  'PB00'
       AND matnr =  i_bqpim-matnr    "material
       AND lifnr =  t_sources-lifnr  "vendor
       AND ekorg =  t_sources-ekorg  "Pur.Org
       AND esokz =  '0'
       AND datbi >=  i_bqpim-nedat   "Valid To
       AND datab <=  i_bqpim-nedat.  "Valid from
    IF l_cnt = 0.
      DELETE t_sources INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  READ TABLE t_sources INDEX 1.
  IF sy-subrc = 0.
    c_bqpex-flief = it_knumh-lifnr.  "Fixed vendor.
    c_bqpex-ekorg = it_knumh-ekorg.
    c_bqpex-infnr = it_knumh-infnr.
  ENDIF.
