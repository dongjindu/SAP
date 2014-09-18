FUNCTION Z_MM_CREATE_DELIVERY_VL01N.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     REFERENCE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0034
*"----------------------------------------------------------------------

 DATA : l_vbeln      LIKE vbak-vbeln,
         l_posnr      LIKE vbap-posnr,
         l_flg,
         l_tabix      LIKE sy-tabix.

  REFRESH:gt_keyt,gt_vbsk,gt_vbls,
          gt_late,gt_vorg.
  CLEAR  :gt_keyt,gt_vbsk,gt_vbls,
          gt_late,gt_vorg,gs_order, l_flg.

  CLEAR  :return.

  CHECK not it_body[] IS INITIAL.

  l_ledat   = sy-datum.
  LOOP AT it_body.
    l_tabix = sy-tabix.
    SELECT SINGLE a~vbtyp
                  b~vbeln
                  b~posnr
                  b~vrkme
                  b~gewei
           INTO (gs_keyt-vbtyp, l_vbeln, l_posnr, gs_vorg-vrkme,
gs_vorg-meins)
           FROM vbak AS a INNER JOIN vbap AS b
              ON a~vbeln EQ b~vbeln
           WHERE a~auart = 'ZPLZ'
             AND a~vkorg = C_EKORG
             AND a~vtweg IN ('16', '26')
             AND a~spart = '51'
             AND b~matnr = it_body-matnr
             AND a~kunnr = it_body-lifnr.

    IF sy-subrc NE 0.
*      e_return-type    = it_body-type    = 'E'.
      e_return-type    = 'E'.
      it_body-type    = 'E'.
      e_return-message = it_body-message =
      'There is no Scheduling Agreement'.
    ELSE.
      it_body-vbeln   = l_vbeln.
      it_body-posnr   = l_posnr.
      e_return-type    = it_body-type    = 'S'.
      e_return-message = it_body-message =
      'There is Scheduling Agreement'.
    ENDIF.

    gs_keyt-panum = '1'             .
*   GS_KEYT-VBTYP = 'C'             . "SD ????
    gs_keyt-vbeln = l_vbeln.
    gs_keyt-posnr = l_posnr.
    gs_keyt-kzazu = 'X'             . "???????
    gs_keyt-ledat = sy-datum        . "Delivery Date
    gs_keyt-tabix = sy-tabix        . "SY-TABIX
    gs_keyt-vstel = 'P300'   . "Shipping Point
*   GS_KEYT-KUNWE = ST_LHD2-KUNWE   .

    SELECT SINGLE kunnr INTO gs_keyt-kunwe "???
    FROM  vbpa
    WHERE vbeln EQ l_vbeln
      AND parvw EQ 'WE'.

    APPEND gs_keyt TO gt_keyt.
    CLEAR gs_keyt.

*   .........................................
    SELECT MAX( edatu ) INTO gs_vorg-mbdat
    FROM vbep
    WHERE vbeln = l_vbeln
      AND posnr = l_posnr
      AND bmeng NE 0.

    gs_vorg-vgbel = l_vbeln. "S/O
    gs_vorg-vgpos = l_posnr.
    gs_vorg-lfimg = it_body-menge."??????
*   GS_VORG-BWART = it_body-BWART. "?? ??
*   GS_VORG-BDART = it_body-BDART. "?????
*   GS_VORG-PLART = it_body-PLART. "????
*   GS_VORG-LGMNG = it_body-CUQTY * IT_LDT2-UMVKZ. "????? ????
*   GS_VORG-lfimg_flo = it_ldt2-cuqty.
*   GS_VORG-lgmng_flo = it_ldt2-cuqty.
*   GS_VORG-umvkz = it_ldt2-umvkz.
*   GS_VORG-umvkn = it_ldt2-umvkn.
*   GS_VORG-umrev = it_ldt2-umziz.
*
    gs_vorg-akmng = 'A'. "??????? ????? ?? ?? ????
*   GS_VORG-VRKME = GS_VBAP-VRKME. "?? ??
*   GS_VORG-MEINS = GS_VBAP-GEWEI. "?? ??
*   GS_VORG-MBDAT = it_body-MBDAT. "?????
    gs_vorg-wadat = it_body-budat.
    gs_vorg-lfdat = it_body-budat."
*    gs_vorg-kodat = it_body-budat.
    gs_vorg-lgort = it_body-lgort. "?? ??
*   GS_VORG-CHARG = it_body-CHARG. "BATCH
    gs_vorg-nornd_lp = 'X'       . "????? ??
    gs_vorg-postab_tabix = sy-tabix."
    gs_vorg-panum = '1'          .

*    gs_vorg-lifex  = it_body-bktxt  . "
*    gs_vorg-lprio  = '30'          . "
*    gs_vorg-kzech  = '3'           . "BATCH
*    gs_vorg-kdmat  =  it_body-ltext.
    COLLECT gs_vorg INTO gt_vorg. CLEAR gs_vorg.

    MODIFY it_body INDEX l_tabix.
  ENDLOOP.

  READ TABLE it_body WITH KEY type = 'E'.

  CHECK sy-subrc NE 0.

  CHECK not gt_vorg[] IS INITIAL.

  CALL FUNCTION 'SHP_VL10_DELIVERY_CREATE'
  EXPORTING
    if_ledat           = l_ledat  "SY-DATUM
    if_nur_vorgabe_pos = 'X'
    it_key_enque_read  = gt_keyt
*   IT_KOMDLGN         = GT_KOMD
  CHANGING
    cx_sd_order        = gs_order
    ct_vbsk_all        = gt_vbsk
    ct_vbls            = gt_vbls
    ct_key_late        = gt_late
    ct_vorgabe_daten   = gt_vorg.
*   CT_VORGABE_LATE    = GT_VOLA.


  SORT gt_vbls BY vbeln posnr.

  LOOP AT it_body.
    l_tabix = sy-tabix.

    READ TABLE gt_vbls WITH KEY
                        vbeln  = it_body-vbeln
                        posnr  = it_body-posnr
                        INTO gs_vbls.
    IF  sy-subrc EQ 0.
      it_body-type       = 'S'.
      it_body-message    = 'Success'.
      it_body-vbeln_vl   = gs_vbls-vbeln_lif.
      it_body-posnr_vl   = gs_vbls-posnr_lif.

    ELSE .
      it_body-type      = 'E'.
      it_body-message   = 'D/O not created!'.
    ENDIF.

    e_return-type    = it_body-type.
    e_return-message = it_body-message.
    MODIFY  it_body INDEX l_tabix.
  ENDLOOP.





ENDFUNCTION.
