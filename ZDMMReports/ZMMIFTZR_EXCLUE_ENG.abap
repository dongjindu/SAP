REPORT zmmiftzr.
INCLUDE zmmfztop.
*&--------------------------------------------------------------------&*
*&    Program: ZMMIFTZR.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: Send FTZ related Goods receipt information.      &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 03/10/2005  Shiva    UD1K914918      initial program.
*& 04/18/2005  Shiva    UD1K915611      Change the logic of adding FSC
*&                                   code for "KD' parts to "MIP" parts.
*& 04/21/2005  Shiva    UD1k915691      Change FTZ tcode for Mvt.type
*&                                   '309' & '310' based on mseg-shkzg.
*& 08/26/2005  Shiva    UD1K917075      FTZ Consumption info for FIFO.
*&                      UD1K917581      add oredr num for grouping.
*& 10/14/2005  Shiva    UD1K917970   Fixed: 1. 'APPC' ord.numship info.
*&                                          2. country of origin value.
*& 10/17/2005  Shiva    UD1K917992      Don't change Transid if BOL
*&                                        exists - FTZ.
*& 10/18/2005  Shiva    UD1K918009   Fixes based on BCV client test.
*& 10/19/2005  Shiva    UD1K918027   Fixes based on BCV client test.
*& 10/20/2005  Shiva    UD1K918058   Fixes based on BCV client test.
*& 10/21/2005  Shiva    UD1K918078   Fixes based on BCV client test.
*& 10/27/2005  Shiva    UD1K918147   Added Mov.type '991' .
*& 10/28/2005  Shiva    UD1K918169   include Mov.type '991' for sales
*&                                   doc. selection for 'Shipto' info.
*  04/10/2006  Furong                for 991, add work order and HTS to
*                                    the consolidation check
*  11/03/09    Furong                Excluding KMMG Engine delivery
*  08/19/2010  Matthew  UD1K949680   Adding logic to FORM GET_TCODE to
*                                    use the GL_ACCOUNT in addition to
*                                    the Movement Type to determine the
*                                    proper TCODE.
*  08/25/2010  Matthew  UD1K949718   Added logic to FORM GET_TCODE to
*                                    use the GL_ACCOUNT in addition to
*                                    the Movement Type to determine the
*                                    proper TCODE for 202 movements.
*&--------------------------------------------------------------------&*

PARAMETERS: p_date LIKE sy-datum OBLIGATORY.
SELECT-OPTIONS s_mblnr FOR mkpf-mblnr.
PARAMETERS: p_srvgrp LIKE rzllitab-classname OBLIGATORY
                     DEFAULT 'PG_FTZ'.

DATA: w_color1(3) TYPE c,
      l_index LIKE sy-index,
      l_qty LIKE mseg-menge.

DATA: l_ebeln_log LIKE mseg-ebeln,
      l_ebelp_log LIKE mseg-ebelp.
*      L_MBLNR_LOG LIKE MSEG-MBLNR,
*      L_MJAHR_LOG LIKE MSEG-MJAHR.
DATA: BEGIN OF lt_mseg OCCURS 0,
      ebeln  LIKE mseg-ebeln,
      ebelp  LIKE mseg-ebelp,
      zbudat LIKE mseg-zbudat,
      END OF lt_mseg.

DATA: BEGIN OF it_list_price OCCURS 0,
      matnr LIKE mseg-matnr,
      ebeln  LIKE mseg-ebeln,
      ebelp  LIKE mseg-ebelp,
      std LIKE mseg-dmbtr,
      po  LIKE mseg-dmbtr,
      info LIKE mseg-dmbtr,
      END OF it_list_price.

DATA: l_netpruom LIKE wa_eina_po-kbetr,
      lw_mseg LIKE wa_mseg.

DATA: lt_mseg_aknh LIKE TABLE OF wa_mseg WITH HEADER LINE.
data: lt_temp LIKE TABLE OF wa_mseg1 WITH HEADER LINE.

RANGES: lr_bwart FOR mseg-bwart.
PERFORM assign_valid_movtypes.


lr_bwart-sign = 'I'.
lr_bwart-option = 'EQ'.
lr_bwart-low = '601'.
APPEND lr_bwart.
lr_bwart-sign = 'I'.
lr_bwart-option = 'EQ'.
lr_bwart-low = '602'.
APPEND lr_bwart.
lr_bwart-sign = 'I'.
lr_bwart-option = 'EQ'.
lr_bwart-low = '991'.
APPEND lr_bwart.


** Changed by Furong on 04/23/09

*SELECT MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW GEWEI XBLNR
*                       XABLN BWART SHKZG WERKS MAT_KDAUF SUM( MENGE )
*                       ERFME UMMAT EBELN EBELP MAKTX
*                        INTO TABLE IT_MSEG
*                        FROM MARA
*                        INNER JOIN MSEG
*                        ON MSEG~MATNR = MARA~MATNR
*                        INNER JOIN MKPF
*                        ON MSEG~MBLNR = MKPF~MBLNR
*                        AND MSEG~MJAHR = MKPF~MJAHR
*                        INNER JOIN MAKT
*                        ON MAKT~MATNR = MARA~MATNR
*                        WHERE ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'ROH' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART )
*                        OR    ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'ROH1' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART )
*                        OR    ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'HALB' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART )
*                        OR    ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'FERT' AND
*                              SPRAS = SY-LANGU AND BWART = '991' )
*          GROUP BY MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW
*                   GEWEI XBLNR XABLN BWART SHKZG WERKS MAT_KDAUF ERFME
*                   UMMAT EBELN EBELP MAKTX.


** Begin of changes by Matthew Cupples on 08/19/2010
* Added SAKTO to the SELECT and GROUP BY.

*SELECT MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW GEWEI XBLNR
*                       XABLN BWART SHKZG WERKS MAT_KDAUF AS KDAUF
*                       SUM( MENGE )
*                      ERFME UMMAT EBELN EBELP MAKTX
*                        INTO TABLE IT_MSEG
*                        FROM MARA
*                        INNER JOIN MSEG
*                        ON MSEG~MATNR = MARA~MATNR
*                        INNER JOIN MKPF
*                        ON MSEG~MBLNR = MKPF~MBLNR
*                        AND MSEG~MJAHR = MKPF~MJAHR
*                        INNER JOIN MAKT
*                        ON MAKT~MATNR = MARA~MATNR
*                        WHERE ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'ROH' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART AND
*                              LGORT <> 'XMIT' )
*                        OR    ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'ROH1' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART AND
*                              LGORT <> 'XMIT' )
*                        OR    ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'HALB' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART AND
*                             LGORT <> 'XMIT' )
*                       OR    ( MKPF~MBLNR IN S_MBLNR AND
*                               CPUDT = P_DATE AND MTART = 'FERT' AND
*                             SPRAS = SY-LANGU AND BWART = '991' )
*         GROUP BY MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW
*                  GEWEI XBLNR XABLN BWART SHKZG WERKS MAT_KDAUF ERFME
*                  UMMAT EBELN EBELP MAKTX.
** End of change on 04/23/09

** Changed by Furong on 02/03/12
*SELECT MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW GEWEI XBLNR
*                       XABLN BWART SHKZG WERKS MAT_KDAUF AS KDAUF
*                       SUM( MENGE )
*                      ERFME UMMAT EBELN EBELP MAKTX SAKTO
*                        INTO TABLE IT_MSEG
*                        FROM MARA
*                        INNER JOIN MSEG
*                        ON MSEG~MATNR = MARA~MATNR
*                        INNER JOIN MKPF
*                        ON MSEG~MBLNR = MKPF~MBLNR
*                        AND MSEG~MJAHR = MKPF~MJAHR
*                        INNER JOIN MAKT
*                        ON MAKT~MATNR = MARA~MATNR
*                        WHERE ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'ROH' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART AND
*                              LGORT <> 'XMIT' )
*                        OR    ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'ROH1' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART AND
*                              LGORT <> 'XMIT' )
*                        OR    ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'HALB' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART AND
*                             LGORT <> 'XMIT' )
*                       OR    ( MKPF~MBLNR IN S_MBLNR AND
*                               CPUDT = P_DATE AND MTART = 'FERT' AND
*                             SPRAS = SY-LANGU AND BWART = '991' )
*         GROUP BY MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW
*                  GEWEI XBLNR XABLN BWART SHKZG WERKS MAT_KDAUF ERFME
*                  UMMAT EBELN EBELP MAKTX SAKTO.
*** End changes on 08/20/2010

DATA: lt_mseg_temp LIKE TABLE OF wa_mseg.

SELECT mara~matnr lifnr mtart mara~meins profl ntgew gewei xblnr
                       xabln bwart shkzg werks mat_kdauf AS kdauf
                       menge
                      erfme ummat ebeln ebelp maktx sakto ZEILE
                        INTO TABLE lt_mseg_temp
                        FROM mara
                        INNER JOIN mseg
                        ON mseg~matnr = mara~matnr
                        INNER JOIN mkpf
                        ON mseg~mblnr = mkpf~mblnr
                        AND mseg~mjahr = mkpf~mjahr
                        INNER JOIN makt
                        ON makt~matnr = mara~matnr
                        WHERE ( mkpf~mblnr IN s_mblnr AND
                                cpudt = p_date AND mtart = 'ROH' AND
                              spras = sy-langu AND bwart IN r_bwart AND
                              lgort <> 'XMIT' )
                        OR    ( mkpf~mblnr IN s_mblnr AND
                                cpudt = p_date AND mtart = 'ROH1' AND
                              spras = sy-langu AND bwart IN r_bwart AND
                              lgort <> 'XMIT' )
                        OR    ( mkpf~mblnr IN s_mblnr AND
                                cpudt = p_date AND mtart = 'HALB' AND
                              spras = sy-langu AND bwart IN r_bwart AND
                             lgort <> 'XMIT' )
                       OR    ( mkpf~mblnr IN s_mblnr AND
                               cpudt = p_date AND mtart = 'FERT' AND
                             spras = sy-langu AND bwart = '991' ).


LOOP AT lt_mseg_temp INTO lw_mseg.
  SELECT SINGLE lichn INTO lw_mseg-lichn
   FROM lips
   WHERE vgbel = lw_mseg-ebeln
     AND vgpos = lw_mseg-ebelp
     and vbeln = lw_mseg-xblnr
     and POSNR = lw_msEG-ZEILE.

  COLLECT lw_mseg INTO it_mseg.
  CLEAR: lw_mseg.
ENDLOOP.

*SELECT mara~matnr lifnr mara~mtart mara~meins mara~profl
*       mara~ntgew mara~gewei xblnr
*       xabln mseg~bwart mseg~shkzg mseg~werks
*        mat_kdauf AS kdauf
*               SUM( menge )
*              erfme mseg~ummat ebeln ebelp maktx sakto
**              mseg~mblnr mseg~zeile lips~posnr
*                lips~lichn
*                INTO TABLE it_mseg
*                FROM mara
*                INNER JOIN mseg
*                ON mseg~matnr = mara~matnr
*                INNER JOIN mkpf
*                ON mseg~mblnr = mkpf~mblnr
*                AND mseg~mjahr = mkpf~mjahr
*                INNER JOIN lips
*                ON mkpf~xblnr =  lips~vbeln
*                AND mseg~ebeln = lips~vgbel
*                AND mseg~ebelp = lips~vgpos
*                INNER JOIN makt
*                ON makt~matnr = mara~matnr
*
*                WHERE ( mkpf~mblnr IN s_mblnr AND
*                        cpudt = p_date AND mara~mtart = 'ROH'
*                  AND spras = sy-langu
*                   AND mseg~bwart IN r_bwart
*                   AND mseg~lgort <> 'XMIT' )
*                OR    ( mkpf~mblnr IN s_mblnr AND
*                        cpudt = p_date AND mara~mtart = 'ROH1'
*                     AND spras = sy-langu
*                     AND mseg~bwart IN r_bwart
*                     AND mseg~lgort <> 'XMIT' )
*                OR    ( mkpf~mblnr IN s_mblnr AND
*                        cpudt = p_date
*                        AND mara~mtart = 'HALB' AND
*                      spras = sy-langu
*                       AND mseg~bwart IN r_bwart AND
*                     mseg~lgort <> 'XMIT' )
*               OR    ( mkpf~mblnr IN s_mblnr AND
*                       cpudt = p_date
*                       AND mara~mtart = 'FERT' AND
*                     spras = sy-langu AND mseg~bwart = '991' )
* GROUP BY mara~matnr lifnr mara~mtart mara~meins
*          mara~profl mara~ntgew
*          mara~gewei xblnr xabln mseg~bwart mseg~shkzg
*          mseg~werks mat_kdauf erfme
*          mseg~ummat ebeln ebelp maktx sakto
**          mseg~mblnr mseg~zeile lips~posnr
*          lips~lichn.

** End on 02/03/12


** Furong on 07/15/14 for Performacne issue (
** Changed by Furong on 11/03/09
*SELECT mara~matnr lifnr mtart mara~meins profl ntgew gewei xblnr
*                       xabln bwart shkzg werks mat_kdauf  AS kdauf
*                       SUM( menge )
*                       erfme ummat ebeln ebelp maktx
*                        INTO TABLE lt_mseg_aknh
*                        FROM mara
*                        INNER JOIN mseg
*                        ON mseg~matnr = mara~matnr
*                        INNER JOIN mkpf
*                        ON mseg~mblnr = mkpf~mblnr
*                        AND mseg~mjahr = mkpf~mjahr
*                        INNER JOIN makt
*                        ON makt~matnr = mara~matnr
*                        WHERE mkpf~mblnr IN s_mblnr AND
*                              cpudt = p_date AND mtart = 'HALB' AND
*                              spras = sy-langu AND bwart IN lr_bwart
*AND
*                              ( werks = 'E001' AND lgort = 'E302' OR
*** Added on 12/19/11 for E002
*                              werks = 'E002' AND lgort = 'N302')
*** End on 12/19/11
*                              AND mseg~kunnr = 'AKNH'
*          GROUP BY mara~matnr lifnr mtart mara~meins profl ntgew
*                   gewei xblnr xabln bwart shkzg werks mat_kdauf erfme
*                   ummat ebeln ebelp maktx.

SELECT mara~matnr lifnr mtart mara~meins profl ntgew gewei xblnr
                       xabln bwart shkzg werks mat_kdauf  AS kdauf
                       menge
                       erfme ummat ebeln ebelp maktx LGORT
                        INTO TABLE lt_temp
                        FROM mara
                        INNER JOIN mseg
                        ON mseg~matnr = mara~matnr
                        INNER JOIN mkpf
                        ON mseg~mblnr = mkpf~mblnr
                        AND mseg~mjahr = mkpf~mjahr
                        INNER JOIN makt
                        ON makt~matnr = mara~matnr
                        WHERE  mtart = 'HALB' AND
                              cpudt = p_date AND
                              mkpf~mblnr IN s_mblnr AND
                              spras = sy-langu AND bwart IN lr_bwart
                              AND mseg~kunnr = 'AKNH'.

loop at lt_temp.
  IF  ( lt_temp-werks = 'E001' AND lt_temp-lgort = 'E302' ) OR
      ( lt_temp-werks = 'E002' AND lt_temp-lgort = 'N302').
      MOVE-CORRESPONDING lt_temp to lt_mseg_aknh.
      COLLECT lt_mseg_aknh.
 ENDIF.
endloop.
** )

LOOP AT lt_mseg_aknh.
  READ TABLE it_mseg INTO lw_mseg
                     WITH KEY matnr = lt_mseg_aknh-matnr
                              lifnr = lt_mseg_aknh-lifnr
                              xblnr = lt_mseg_aknh-xblnr
                              xabln = lt_mseg_aknh-xabln
                              bwart = lt_mseg_aknh-bwart
                              shkzg = lt_mseg_aknh-shkzg
                              werks = lt_mseg_aknh-werks
                              kdauf = lt_mseg_aknh-kdauf
                              ebeln = lt_mseg_aknh-ebeln
                              ebelp = lt_mseg_aknh-ebelp.
  IF sy-subrc = 0.
*    L_INDEX = SY-INDEX.
    l_qty = lw_mseg-menge - lt_mseg_aknh-menge.
    IF l_qty > 0.
      lw_mseg-menge = l_qty.
      MODIFY it_mseg FROM lw_mseg TRANSPORTING menge.
    ELSE.
      DELETE TABLE it_mseg FROM lw_mseg.
    ENDIF.
  ENDIF.
ENDLOOP.
** End of change on 11/03/09


*IF SY-SUBRC NE 0.
IF it_mseg[] IS INITIAL.
  MESSAGE ID 'ZMM' TYPE 'I' NUMBER '999' WITH text-001.
  EXIT.
ENDIF.

wa_mseg-mngko = 1.
MODIFY it_mseg FROM wa_mseg TRANSPORTING mngko
                      WHERE mngko IS INITIAL.
CLEAR wa_mseg.
*&----For FTZ reporting we just change only the internal table for 991.
wa_mseg-mtart = 'HALB'.
MODIFY it_mseg FROM wa_mseg TRANSPORTING mtart
                      WHERE bwart = '991'.
CLEAR wa_mseg.

PERFORM get_bom_info.
PERFORM get_salepart_ccode USING 'R'.
SORT: it_mseg BY matnr bwart,
      it_lfa1 BY lifnr.
LOOP AT it_mseg ASSIGNING <fs_mseg>.
  CLEAR: w_matnr.
  wa_ztmm_6026_01-partnerid = '100300'.
  w_date = sy-datum.
  w_time = sy-uzeit.
  CONCATENATE w_date 'T' w_time  INTO wa_ztmm_6026_01-effdate.
  PERFORM get_tcode USING <fs_mseg>-mtart
                          <fs_mseg>-bwart
                          <fs_mseg>-shkzg
                          <fs_mseg>-profl
                          <fs_mseg>-xabln
* Begin of changes by Matthew Cupples on 08/19/2010
                          <fs_mseg>-sakto
* End changes on 08/20/2010
                    CHANGING w_matnr
                             wa_ztmm_6026_01-txncode
                             wa_ztmm_6026_01-ordernumreceipt
                             wa_ztmm_6026_01-ordernumship
                             wa_ztmm_6026_01-transportid
                             wa_ztmm_6026_01-billoflading
                             wa_ftztcode.
  CONCATENATE p_date 'T' '000000' INTO wa_ztmm_6026_01-txndate.
  IF <fs_mseg>-bwart = '601' OR <fs_mseg>-bwart = '991' .
    IF <fs_mseg>-profl = 'M' OR <fs_mseg>-pprofl = 'M'.
      CONCATENATE <fs_mseg>-pmatnr p_date
                                   INTO wa_ztmm_6026_01-ordernumwork.
    ELSEIF <fs_mseg>-profl = 'V' OR <fs_mseg>-pprofl = 'V'.
      wa_ztmm_6026_01-ordernumwork = wa_ztmm_6026_01-ordernumreceipt.
      CLEAR: wa_ztmm_6026_01-ordernumreceipt,
             wa_ztmm_6026_01-transportid.
    ENDIF.
  ELSE.
    wa_ztmm_6026_01-ordernumwork = space.
  ENDIF.
  CASE wa_ztmm_6026_01-txncode.
    WHEN 'ANPC'.
      CONCATENATE <fs_mseg>-pmatnr p_date
                                  INTO wa_ztmm_6026_01-ordernumship.
      wa_ztmm_6026_01-countryshipto = 'US'.
    WHEN 'SPPC'.
      CASE <fs_mseg>-profl.
        WHEN 'K'.
          CONCATENATE 'GIKD' p_date INTO wa_ztmm_6026_01-ordernumship.
        WHEN 'V'.
          CONCATENATE 'GILP' p_date INTO wa_ztmm_6026_01-ordernumship.
        WHEN 'M'.
          CONCATENATE 'GIMIP' p_date INTO wa_ztmm_6026_01-ordernumship.
      ENDCASE.
      READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = <fs_mseg>-kdauf
                                                         BINARY SEARCH.
      IF sy-subrc NE 0.
       READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = <fs_mseg>-lifnr
                                                          BINARY SEARCH.
        IF sy-subrc NE 0.
          wa_ztmm_6026_01-countryshipto = space.
        ELSE.
          wa_ztmm_6026_01-countryshipto = wa_lfa1-land1.
        ENDIF.
      ELSE.
        wa_ztmm_6026_01-countryshipto = wa_vbpa-land1.
      ENDIF.
    WHEN OTHERS.
      wa_ztmm_6026_01-ordernumship = space.
  ENDCASE.
  IF NOT w_matnr IS INITIAL.
    wa_ztmm_6026_01-matnr = w_matnr.
  ELSE.
    wa_ztmm_6026_01-matnr = <fs_mseg>-matnr.
  ENDIF.
  CASE <fs_mseg>-profl.
    WHEN space.
      IF <fs_mseg>-mtart = 'FERT'.
        wa_ztmm_6026_01-ptc = 'IM'.
      ENDIF.
    WHEN 'K' OR 'V'.
      wa_ztmm_6026_01-ptc = 'PC'.
    WHEN 'M'.
      wa_ztmm_6026_01-ptc = 'IM'.
  ENDCASE.
  wa_ztmm_6026_01-ptcsrc = c_erp_source.
  wa_ztmm_6026_01-maktx = <fs_mseg>-maktx.
  wa_ztmm_6026_01-maktxsrc = c_erp_source.
  wa_ztmm_6026_01-naftacertified = space.
  wa_ztmm_6026_01-naftacertifiedsc = c_ftzlink_source.
  CASE wa_ftztcode-shkzg.
    WHEN 'H'.
      wa_ztmm_6026_01-menge = abs( <fs_mseg>-menge ).
    WHEN 'S'.
      IF <fs_mseg>-menge GT 0.
        wa_ztmm_6026_01-menge = <fs_mseg>-menge * -1.
      ELSE.
        wa_ztmm_6026_01-menge = <fs_mseg>-menge.
      ENDIF.
  ENDCASE.
  IF wa_ztmm_6026_01-txncode = 'CNPC'.
    IF wa_ztmm_6026_01-menge GT 0.
      wa_ztmm_6026_01-menge  = wa_ztmm_6026_01-menge  * -1.
    ENDIF.
  ENDIF.
  wa_ztmm_6026_01-meins = <fs_mseg>-meins.
  wa_ztmm_6026_01-meinssrc = c_erp_source.
  wa_ztmm_6026_01-qtyperlm = abs( <fs_mseg>-mngko ).

  IF <fs_mseg>-ntgew IS INITIAL.
    wa_ztmm_6026_01-ntgewsrc = c_ftzlink_source.
    wa_ztmm_6026_01-geweisrc = c_ftzlink_source.
  ELSE.
    wa_ztmm_6026_01-ntgew  = <fs_mseg>-ntgew.
    wa_ztmm_6026_01-ntgewsrc = c_erp_source.
    wa_ztmm_6026_01-gewei = <fs_mseg>-gewei.
    wa_ztmm_6026_01-geweisrc = c_erp_source.
  ENDIF.
  PERFORM separate_color USING <fs_mseg>-ummat <fs_mseg>-mtart
                         CHANGING <fs_mseg>-ummat w_color1.
  wa_ztmm_6026_01-adjproductnum = <fs_mseg>-ummat.
*wa_ztmm_6026_01-receiptdocid        "No value passed
*wa_ztmm_6026_01-exitdocid           "No value passed
*wa_ztmm_6026_01-adjreceiptdocid     "No value passed
*wa_ztmm_6026_01-fromzoneid          "No value passed
*wa_ztmm_6026_01-tozoneid            "No value passed
  READ TABLE it_ztbl INTO wa_ztbl WITH KEY ebeln = <fs_mseg>-ebeln
                                           ebelp = <fs_mseg>-ebelp
                                           xblnr = <fs_mseg>-xblnr
                                  TRANSPORTING zfhblno zfrptty zfvia.
  IF sy-subrc NE 0.
    wa_ztmm_6026_01-modeoftransport = 'L'.
    IF wa_ztmm_6026_01-profl EQ 'M'.
      wa_ztmm_6026_01-statuscode    = 'F'.
      wa_ztmm_6026_01-statuscodesrc = ''.
    ELSE.
      wa_ztmm_6026_01-statuscode    = 'D'.
      wa_ztmm_6026_01-statuscodesrc = ''.
    ENDIF.
  ELSE.
    wa_po_bol-matnr = <fs_mseg>-matnr.
    wa_po_bol-ebeln = <fs_mseg>-ebeln.
    wa_po_bol-zfhblno = wa_ztbl-zfhblno.
    APPEND wa_po_bol TO it_po_bol.
    CLEAR wa_po_bol.
    CASE wa_ztbl-zfvia.
      WHEN 'VSL'.
        wa_ztmm_6026_01-modeoftransport = 'O'.
      WHEN 'AIR'.
        wa_ztmm_6026_01-modeoftransport = 'A'.
    ENDCASE.
    IF wa_ztbl-zfrptty = 'F'.
      wa_ztmm_6026_01-statuscode    = ''.
      wa_ztmm_6026_01-statuscodesrc = 'I'.
    ELSE.
      wa_ztmm_6026_01-statuscode    = 'I'.
      wa_ztmm_6026_01-statuscodesrc = ''.
    ENDIF.
  ENDIF.
*wa_ztmm_6026_01-receiptdate         "No value passed
*wa_ztmm_6026_01-itnum               "No value passed
*wa_ztmm_6026_01-exportdate          "No value passed
*wa_ztmm_6026_01-manifestqty         "No value passed
  wa_ztmm_6026_01-validflag        = 'N'.
  wa_ztmm_6026_01-assignmentflag   = 'N'.
  wa_ztmm_6026_01-fifoflag         = 'N'.
  wa_ztmm_6026_01-deletedflag      = 'N'.
  wa_ztmm_6026_01-keepduringrollba = 'N'.
*&----------FIFO HTS value
  IF <fs_mseg>-bwart = '991' AND <fs_mseg>-xabln = 'T'.
    wa_ztmm_6026_01-stawn    = '9817.85.01'.
    wa_ztmm_6026_01-stawnsrc = c_erp_source.

* i.g. moon 5/26/10 {
  ELSEIF <fs_mseg>-bwart = '655' AND <fs_mseg>-mtart EQ 'ROH'.

    wa_ztmm_6026_01-stawn    = '0000.00.00'.
    wa_ztmm_6026_01-stawnsrc = c_erp_source.
    wa_ztmm_6026_01-statuscode = 'D'.
    wa_ztmm_6026_01-transportid = 'Domestic Returns'.
* }

  ELSE.
    READ TABLE it_mat_info INTO wa_mat_info
                           WITH TABLE KEY matnr = <fs_mseg>-matnr
                                          werks = <fs_mseg>-werks
                                          TRANSPORTING stawn.
    IF sy-subrc NE 0.
      wa_ztmm_6026_01-stawn = space.
      wa_ztmm_6026_01-stawnsrc = 'H'.
    ELSE.
      IF wa_mat_info-stawn IS INITIAL.
        wa_ztmm_6026_01-stawn = space.
        wa_ztmm_6026_01-stawnsrc = 'H'.
      ELSE.
        wa_ztmm_6026_01-stawn = wa_mat_info-stawn.
        wa_ztmm_6026_01-stawnsrc = c_erp_source.
      ENDIF.
    ENDIF.
  ENDIF.

** Changed by Furong on 02/03/12 for FTA
  IF  <fs_mseg>-lichn = 'Y'.
    wa_ztmm_6026_01-spicode1 = 'KR'.
    wa_ztmm_6026_01-spicode1src = space.
  ELSE.
    wa_ztmm_6026_01-spicode1 = space.
    wa_ztmm_6026_01-spicode1src = 'H'.
  ENDIF.
*  wa_ztmm_6026_01-spicode1src = 'H'.
** End ON 02/03/12

  wa_ztmm_6026_01-spicode2src = 'H'.
  wa_ztmm_6026_01-lifnrsrc = c_ftzlink_source.
  wa_ztmm_6026_01-relflagsrc = 'M'.
  wa_ztmm_6026_01-htsindexsrc = c_ftzlink_source.
  wa_ztmm_6026_01-htsdescsrc = 'H'.
  wa_ztmm_6026_01-htsnum2src = c_ftzlink_source.

** changed by Furong on 08/11/08

*  READ TABLE IT_EINA INTO WA_EINA WITH KEY MATNR = <FS_MSEG>-MATNR
*                                                   BINARY SEARCH.
*  IF SY-SUBRC NE 0.
*    READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
*                           WITH TABLE KEY MATNR = <FS_MSEG>-MATNR
*                                          WERKS = <FS_MSEG>-WERKS
*                                          TRANSPORTING STPRS PEINH.
*    IF SY-SUBRC NE 0.
*      WA_ZTMM_6026_01-NETPR = 0.
*      WA_ZTMM_6026_01-EFFPR = 0.
*      WA_ZTMM_6026_01-NETPRUOM = 0.
*      WA_ZTMM_6026_01-EFFPRUOM = 0.
*    ELSE.
*      WA_ZTMM_6026_01-NETPR = WA_MAT_INFO-STPRS.
*      WA_ZTMM_6026_01-EFFPR = WA_MAT_INFO-STPRS.
*      WA_ZTMM_6026_01-NETPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH.
*      WA_ZTMM_6026_01-EFFPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH.
*    ENDIF.
*  ELSE.
*    IF <FS_MSEG>-MEINS NE <FS_MSEG>-ERFME.
*      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
*           EXPORTING
*                INPUT                = WA_EINA-KBETR
*                MATNR                = <FS_MSEG>-MATNR
*                MEINH                = <FS_MSEG>-ERFME
*                MEINS                = <FS_MSEG>-MEINS
*           IMPORTING
*                OUTPUT               = WA_EINA-KBETR
*           EXCEPTIONS
*                CONVERSION_NOT_FOUND = 1
*                INPUT_INVALID        = 2
*                MATERIAL_NOT_FOUND   = 3
*                MEINH_NOT_FOUND      = 4
*                MEINS_MISSING        = 5
*                NO_MEINH             = 6
*                OUTPUT_INVALID       = 7
*                OVERFLOW             = 8
*                OTHERS               = 9.
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*    ENDIF.
*    WA_ZTMM_6026_01-NETPR = WA_EINA-KBETR.
*    WA_ZTMM_6026_01-EFFPR = WA_EINA-EFFPR.
*    IF NOT WA_EINA-BPUMN IS INITIAL.
* WA_ZTMM_6026_01-NETPRUOM = ( WA_ZTMM_6026_01-NETPR * WA_EINA-BPUMZ ) /
*                                                          WA_EINA-BPUMN
*.
* WA_ZTMM_6026_01-EFFPRUOM = ( WA_ZTMM_6026_01-EFFPR * WA_EINA-BPUMZ ) /
*                                                          WA_EINA-BPUMN
*.
*      IF NOT WA_EINA-PEINH IS INITIAL.
*    WA_ZTMM_6026_01-NETPRUOM = WA_ZTMM_6026_01-NETPRUOM / WA_EINA-PEINH
*.
*    WA_ZTMM_6026_01-EFFPRUOM = WA_ZTMM_6026_01-EFFPRUOM / WA_EINA-PEINH
*.
*      ENDIF.
*    ENDIF.
*  ENDIF.

* IF WA_EINA-LAND1 IS INITIAL.
*    WA_ZTMM_6026_01-LAND1    = WA_EINA-LAND1.
*    WA_ZTMM_6026_01-LAND1SRC = 'I'.
*  ELSE.
*    WA_ZTMM_6026_01-LAND1    = WA_EINA-LAND1.
*    WA_ZTMM_6026_01-LAND1SRC = SPACE.
*  ENDIF.
*  WA_ZTMM_6026_01-VALUE2SRC = C_FTZLINK_SOURCE.
*  IF WA_EINA-WAERS IS INITIAL.
*    WA_ZTMM_6026_01-WAERSSRC = C_FTZLINK_SOURCE.
*  ELSE.
*    WA_ZTMM_6026_01-WAERS   = WA_EINA-WAERS.
*    WA_ZTMM_6026_01-WAERSSRC = C_ERP_SOURCE..
*  ENDIF.

  CLEAR: l_ebeln_log, l_ebelp_log, lt_mseg[].
READ TABLE it_eina_po INTO wa_eina_po WITH KEY  matnr = <fs_mseg>-matnr
                                                          BINARY SEARCH.

  IF sy-subrc = 0.
** Changed by Furong pn 08/01/08 Requested by Mr.Yoon
    it_list_price-matnr = <fs_mseg>-matnr.
    it_list_price-info = wa_eina_po-kbetr.
    it_list_price-ebeln = <fs_mseg>-ebeln.
    READ TABLE it_mat_info INTO wa_mat_info
                               WITH TABLE KEY matnr = <fs_mseg>-matnr
                                              werks = <fs_mseg>-werks
                                              TRANSPORTING stprs.
    IF sy-subrc EQ 0.
      it_list_price-std = wa_mat_info-stprs.
    ENDIF.
    APPEND it_list_price.
  ENDIF.
** end of change

*** changed by Furong on09/19/09
*  IF SY-SUBRC NE 0.
** Changed by Furong on 08/13/09 , Requested by Prasa

*    SELECT EBELN EBELP ZBUDAT INTO TABLE LT_MSEG
*         FROM MSEG
*       WHERE BWART = '101'
*         AND MATNR = <FS_MSEG>-MATNR.
*    SORT LT_MSEG BY ZBUDAT DESCENDING.
*    READ TABLE LT_MSEG INDEX 1.

*     IF SY-SUBRC = 0.
*      L_EBELN_LOG = LT_MSEG-EBELN.
*      L_EBELP_LOG = LT_MSEG-EBELP.

*      SELECT SINGLE NETPR BPUMZ BPUMN PEINH UMREN UMREZ INTO
*           (WA_EKPO-NETPR, WA_EKPO-BPUMZ, WA_EKPO-BPUMN, WA_EKPO-PEINH,
*             WA_EKPO-UMREN, WA_EKPO-UMREZ)
*        FROM EKPO
*        WHERE EBELN = L_EBELN_LOG
*          AND EBELP = L_EBELP_LOG.
*
*      IF SY-SUBRC = 0.
*        WA_EINA_PO-KBETR = WA_EKPO-NETPR.
*        WA_EINA_PO-BPUMZ  = WA_EKPO-BPUMZ.
*        WA_EINA_PO-BPUMN = WA_EKPO-BPUMN.
*        WA_EINA_PO-PEINH = WA_EKPO-PEINH.
*        WA_EINA_PO-UMREN = WA_EKPO-UMREN.
*        WA_EINA_PO-UMREz = WA_EKPO-UMREz.
*
*      ELSE.
*        READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
*                              WITH TABLE KEY MATNR = <FS_MSEG>-MATNR
*                                             WERKS = <FS_MSEG>-WERKS
*                                             TRANSPORTING STPRS PEINH.
*        IF SY-SUBRC NE 0.
*          WA_ZTMM_6026_01-NETPR = 0.
*          WA_ZTMM_6026_01-EFFPR = 0.
*          WA_ZTMM_6026_01-NETPRUOM = 0.
*          WA_ZTMM_6026_01-EFFPRUOM = 0.
*        ELSE.
*          WA_EINA_PO-KBETR = WA_MAT_INFO-STPRS.
*          WA_ZTMM_6026_01-NETPR = WA_MAT_INFO-STPRS.
*          WA_ZTMM_6026_01-EFFPR = WA_MAT_INFO-STPRS.
*       WA_ZTMM_6026_01-NETPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH
*.
*       WA_ZTMM_6026_01-EFFPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH
*.
*        ENDIF.
*
*      ENDIF.
*    ELSE.
*      READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
*                             WITH TABLE KEY MATNR = <FS_MSEG>-MATNR
*                                            WERKS = <FS_MSEG>-WERKS
*                                            TRANSPORTING STPRS PEINH.
*      IF SY-SUBRC NE 0.
*        WA_ZTMM_6026_01-NETPR = 0.
*        WA_ZTMM_6026_01-EFFPR = 0.
*        WA_ZTMM_6026_01-NETPRUOM = 0.
*        WA_ZTMM_6026_01-EFFPRUOM = 0.
*      ELSE.
*        WA_EINA_PO-KBETR = WA_MAT_INFO-STPRS.
*        WA_ZTMM_6026_01-NETPR = WA_MAT_INFO-STPRS.
*        WA_ZTMM_6026_01-EFFPR = WA_MAT_INFO-STPRS.
*       WA_ZTMM_6026_01-NETPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH
*.
*       WA_ZTMM_6026_01-EFFPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH
*.
*      ENDIF.
*    ENDIF.
*
*  ELSE.
*** end of change on 09/19/09
*    IF NOT <FS_MSEG>-EBELN IS INITIAL.
*      CLEAR: WA_EKPO.
*      READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN =  <FS_MSEG>-EBELN
*                                              EBELP =  <FS_MSEG>-EBELP
*                                              MATNR =  <FS_MSEG>-MATNR
*                                                     BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        WA_EINA_PO-KBETR = WA_EKPO-NETPR.
*        WA_EINA_PO-BPUMZ  = WA_EKPO-BPUMZ.
*        WA_EINA_PO-BPUMN = WA_EKPO-BPUMN.
** Changed by Furong pn 08/01/08 Requested by Mr.Yoon

** End of change
*      ENDIF.
*    ENDIF.
*  ENDIF.
*** changed by Furong on09/19/09
** Changed by Furong pn 08/01/08 Requested by Mr.Yoon
*        IT_LIST_PRICE-PO = WA_EKPO-NETPR.
*  APPEND IT_LIST_PRICE.
*  CLEAR: IT_LIST_PRICE.
** End of change

*

*
*  IF <FS_MSEG>-MEINS NE <FS_MSEG>-ERFME.
*    CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
*         EXPORTING
*              INPUT                = WA_EINA_PO-KBETR
*              MATNR                = <FS_MSEG>-MATNR
*              MEINH                = <FS_MSEG>-ERFME
*              MEINS                = <FS_MSEG>-MEINS
*         IMPORTING
*              OUTPUT               = WA_EINA_PO-KBETR
*         EXCEPTIONS
*              CONVERSION_NOT_FOUND = 1
*              INPUT_INVALID        = 2
*              MATERIAL_NOT_FOUND   = 3
*              MEINH_NOT_FOUND      = 4
*              MEINS_MISSING        = 5
*              NO_MEINH             = 6
*              OUTPUT_INVALID       = 7
*              OVERFLOW             = 8
*              OTHERS               = 9.
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*  ENDIF.
*  WA_ZTMM_6026_01-NETPR = WA_EINA_PO-KBETR.
*  WA_ZTMM_6026_01-EFFPR = WA_EINA_PO-EFFPR.
*  IF NOT WA_EINA_PO-BPUMN IS INITIAL.
*
*** Changed by Furong on 08/06/09 Requested by Prasa
*
**WA_ZTMM_6026_01-NETPRUOM = ( WA_ZTMM_6026_01-NETPR * WA_EINA_PO-BPUMZ
*)
**                                                    / WA_EINA_PO-BPUMN
*.
*l_NETPRUOM = WA_EINA_PO-KBETR.
*WA_ZTMM_6026_01-NETPRUOM = ( l_NETPRUOM * WA_EINA_PO-BPUMZ *
*WA_eina_PO-UMREN ) / ( WA_EINA_PO-BPUMN * WA_eina_PO-UMREz ).
**
** end of change
*** end of change on 09/19/09


READ TABLE it_eina_po INTO wa_eina_po WITH KEY  matnr = <fs_mseg>-matnr
                                                          BINARY SEARCH.
  IF sy-subrc NE 0.
    READ TABLE it_mat_info INTO wa_mat_info
                           WITH TABLE KEY matnr = <fs_mseg>-matnr
                                          werks = <fs_mseg>-werks
                                          TRANSPORTING stprs peinh.
    IF sy-subrc NE 0.
      wa_ztmm_6026_01-netpr = 0.
      wa_ztmm_6026_01-effpr = 0.
      wa_ztmm_6026_01-netpruom = 0.
      wa_ztmm_6026_01-effpruom = 0.
    ELSE.
      wa_ztmm_6026_01-netpr = wa_mat_info-stprs.
      wa_ztmm_6026_01-effpr = wa_mat_info-stprs.
      wa_ztmm_6026_01-netpruom = wa_mat_info-stprs / wa_mat_info-peinh.
      wa_ztmm_6026_01-effpruom = wa_mat_info-stprs / wa_mat_info-peinh.
    ENDIF.
  ELSE.

    IF NOT <fs_mseg>-ebeln IS INITIAL.
      CLEAR: wa_ekpo.
      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln =  <fs_mseg>-ebeln
                                            ebelp =  <fs_mseg>-ebelp
                                            matnr =  <fs_mseg>-matnr
                                                   BINARY SEARCH.
      IF sy-subrc = 0.
        wa_eina_po-kbetr = wa_ekpo-netpr.
        wa_eina_po-bpumz  = wa_ekpo-bpumz.
        wa_eina_po-bpumn = wa_ekpo-bpumn.
        wa_eina_po-peinh = wa_ekpo-peinh.
        wa_eina_po-umren = wa_ekpo-umren.
        wa_eina_po-umrez = wa_ekpo-umrez.
      ENDIF.
    ENDIF.
    IF <fs_mseg>-meins NE <fs_mseg>-erfme.
      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
        EXPORTING
          input                = wa_eina_po-kbetr
          matnr                = <fs_mseg>-matnr
          meinh                = <fs_mseg>-erfme
          meins                = <fs_mseg>-meins
        IMPORTING
          output               = wa_eina_po-kbetr
        EXCEPTIONS
          conversion_not_found = 1
          input_invalid        = 2
          material_not_found   = 3
          meinh_not_found      = 4
          meins_missing        = 5
          no_meinh             = 6
          output_invalid       = 7
          overflow             = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ENDIF.
    wa_ztmm_6026_01-netpr = wa_eina_po-kbetr.
    wa_ztmm_6026_01-effpr = wa_eina_po-effpr.

    IF NOT wa_eina_po-bpumn IS INITIAL.
*WA_ZTMM_6026_01-NETPRUOM = ( WA_ZTMM_6026_01-NETPR * WA_EINA_PO-BPUMZ )
*                                                     / WA_EINA_PO-BPUMN
*            .
*WA_ZTMM_6026_01-EFFPRUOM = ( WA_ZTMM_6026_01-EFFPR * WA_EINA_PO-BPUMZ )
*                                                     / WA_EINA_PO-BPUMN
*.

wa_ztmm_6026_01-netpruom = ( wa_ztmm_6026_01-netpr * wa_eina_po-bpumz *
                              wa_eina_po-umren ) / ( wa_eina_po-bpumn *
                                                     wa_eina_po-umrez ).

wa_ztmm_6026_01-effpruom = ( wa_ztmm_6026_01-effpr * wa_eina_po-bpumz *
                              wa_eina_po-umren ) / ( wa_eina_po-bpumn *
                                                     wa_eina_po-umrez ).

      IF NOT wa_eina_po-peinh IS INITIAL.
        wa_ztmm_6026_01-netpruom = wa_ztmm_6026_01-netpruom /
                                   wa_eina_po-peinh.
        wa_ztmm_6026_01-effpruom = wa_ztmm_6026_01-effpruom /
                                   wa_eina_po-peinh.
      ENDIF.
    ENDIF.

  ENDIF.

  IF wa_eina_po-land1 IS INITIAL.
    wa_ztmm_6026_01-land1    = wa_eina_po-land1.
    wa_ztmm_6026_01-land1src = 'I'.
  ELSE.
    wa_ztmm_6026_01-land1    = wa_eina_po-land1.
    wa_ztmm_6026_01-land1src = space.
  ENDIF.
  wa_ztmm_6026_01-value2src = c_ftzlink_source.
  IF wa_eina_po-waers IS INITIAL.
    wa_ztmm_6026_01-waerssrc = c_ftzlink_source.
  ELSE.
    wa_ztmm_6026_01-waers   = wa_eina_po-waers.
    wa_ztmm_6026_01-waerssrc = c_erp_source..
  ENDIF.
** End of change on 08/11/08

  wa_ztmm_6026_01-altvaluesrc    = 'I'.
  wa_ztmm_6026_01-altvalue2src   = 'I'.
  wa_ztmm_6026_01-altcurrcodesrc = 'I'.
  wa_ztmm_6026_01-advaloremratesrc = 'H'.
  wa_ztmm_6026_01-specificratesrc = 'H'.
  wa_ztmm_6026_01-uomconvfactorsrc = 'I'.
  wa_ztmm_6026_01-adduomconvfacsrc = 'I'.
  wa_ztmm_6026_01-rptqtyuomsrc = 'H'.
  wa_ztmm_6026_01-addrptqtyuomsrc = 'H'.
  wa_ztmm_6026_01-dotindicator     = 'N'.
  wa_ztmm_6026_01-fccindicator     = 'N'.
  wa_ztmm_6026_01-fdaindicator     = 'N'.
  APPEND wa_ztmm_6026_01 TO it_ztmm_6026_01.
  CLEAR: wa_ztmm_6026_01, wa_eina, wa_mat_info,wa_ztbl,
          wa_lfa1, wa_vbpa.
ENDLOOP.
PERFORM summarize_txncode.
** Added by Furong on 11/12/07
PERFORM summarize_cppc_cnpc.
** End of addition
PERFORM adjust_anpc_appc.
PERFORM process_data_by_section.
IF sy-batch IS INITIAL.
  PERFORM dsp_log.
ELSE.
  PERFORM list_price.
ENDIF.
INCLUDE zimmgm29i_6026cla.
INCLUDE zimmgm29i_6026o01.   "PBO Part
INCLUDE zimmgm29i_6026i01.   "PAI Part
INCLUDE zmmiftzrf01.

*&---------------------------------------------------------------------*
*&      Form  assign_valid_movtypes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_valid_movtypes.
  PERFORM add_bwart USING '101'.
  PERFORM add_bwart USING '102'.
  PERFORM add_bwart USING '122'.
  PERFORM add_bwart USING '123'.
  PERFORM add_bwart USING '201'.
  PERFORM add_bwart USING '202'.
  PERFORM add_bwart USING '551'.
  PERFORM add_bwart USING '552'.
  PERFORM add_bwart USING '555'.
  PERFORM add_bwart USING '556'.
  PERFORM add_bwart USING '903'.
  PERFORM add_bwart USING '904'.
  PERFORM add_bwart USING '907'.
  PERFORM add_bwart USING '908'.
  PERFORM add_bwart USING '905'.
  PERFORM add_bwart USING '906'.
  PERFORM add_bwart USING '702'.
  PERFORM add_bwart USING '701'.
  PERFORM add_bwart USING '712'.
  PERFORM add_bwart USING '711'.
  PERFORM add_bwart USING '601'.
  PERFORM add_bwart USING '602'.
  PERFORM add_bwart USING '309'.
  PERFORM add_bwart USING '310'.
  PERFORM add_bwart USING '991'.
** Changed by Furong on 11/24/09
  PERFORM add_bwart USING '655'.
** End of change
** Changed by Furong on 04/24/12
  PERFORM add_bwart USING '511'.
  PERFORM add_bwart USING '512'.
** End of change

  CLEAR r_bwart.
ENDFORM.                    " assign_valid_movtypes

*&---------------------------------------------------------------------*
*&      Form  add_bwart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0127   text
*----------------------------------------------------------------------*
FORM add_bwart USING p_bwart.

  r_bwart-sign = 'I'.
  r_bwart-option = 'EQ'.
  r_bwart-low = p_bwart.
  APPEND r_bwart.

ENDFORM.                    " add_bwart
*&---------------------------------------------------------------------*
*&      Form  get_bom_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bom_info.

  DATA: wa_stpox LIKE stpox.
  DATA: BEGIN OF wa_bom_comp,
          matnr LIKE mara-matnr,
          idnrk LIKE stpox-idnrk,
          werks LIKE stpox-werks,
*          bwart like mseg-bwart,
          mtart LIKE stpox-mtart,
          mngko LIKE stpox-mngko,
          meins LIKE mara-meins,
          menge LIKE mseg-menge,
        END OF wa_bom_comp.
  DATA: BEGIN OF wa_a018,
           matnr LIKE a018-matnr,
           lifnr LIKE a018-lifnr,
           knumh LIKE a018-knumh,
        END OF wa_a018.
  DATA: BEGIN OF wa_konp,
          knumh LIKE konp-knumh,
          kbetr LIKE konp-kbetr,
        END OF wa_konp.
  DATA: BEGIN OF wa_halb_fert,
          matnr LIKE mseg-matnr,
          werks LIKE mseg-werks,
          menge LIKE mseg-menge,
          bwart LIKE mseg-bwart,
        END OF wa_halb_fert.
  DATA: BEGIN OF wa_t460a,
          matnr LIKE marc-matnr,
          werks LIKE marc-werks,
          wrk02 LIKE t460a-wrk02,
        END OF wa_t460a.

  DATA: it_stpox LIKE TABLE OF wa_stpox,
        it_bom_comp LIKE TABLE OF wa_bom_comp,
        it_a018 LIKE TABLE OF wa_a018,
        it_konp LIKE HASHED TABLE OF wa_konp WITH UNIQUE KEY knumh,
        it_halb_fert LIKE TABLE OF wa_halb_fert,
        it_t460a LIKE TABLE OF wa_t460a,
        it_mseg1 LIKE TABLE OF wa_mseg.

  RANGES: r_nscrap FOR mseg-bwart,
          r_bom_topmat FOR mara-matnr.
  DATA: w_lines TYPE i,
        w_clen  TYPE i,
        w_max   TYPE i,
        w_free  TYPE i,
        w_no_times TYPE i,
        w_lp_idx   TYPE i,
        w_frm_idx  TYPE i,
        w_to_idx   TYPE i,
        w_rem      TYPE i.

  FIELD-SYMBOLS: <fs_com> LIKE LINE OF it_stpox1,
** Changed by Furong on 08/11/08
*                 <FS_EINA> LIKE LINE OF IT_EINA,
                 <fs_eina_po> LIKE LINE OF it_eina_po,
** End of change
                 <fs_halb_exp> LIKE LINE OF it_halb_exp,
                 <fs_halb_eng> LIKE LINE OF it_halb_eng.

*&---We don't need data of 'HALB' materials with Mov.Type '309' & '310'.
  DELETE it_mseg WHERE mtart = 'HALB' AND bwart = '309'
                 OR    mtart = 'HALB' AND bwart = '310'
                 OR    ( mtart = 'HALB' AND bwart = '101' AND
                         ebeln IS INITIAL )
                 OR    ( mtart = 'HALB' AND bwart = '102' AND
                         ebeln IS INITIAL ).

  DESCRIBE TABLE it_mseg LINES w_lines.
  IF w_lines = 0.
  ELSE.
    SELECT ebeln ebelp zfhblno zfrptty zfvia likp~vbeln
            INTO TABLE it_ztbl
                           FROM likp
                           INNER JOIN ztcivhd
                           ON ztcivhd~zfcivno = likp~bolnr
                           INNER JOIN ztcivit
                           ON ztcivit~zfcivrn = ztcivhd~zfcivrn
                           INNER JOIN ztbl
                           ON ztbl~zfblno = ztcivit~zfblno
                           FOR ALL ENTRIES IN it_mseg
                           WHERE likp~vbeln = it_mseg-xblnr
                           AND   ebeln = it_mseg-ebeln
                           AND   ebelp = it_mseg-ebelp.
  ENDIF.

  SELECT mara~matnr werks profl ntgew
                    gewei stawn stprs peinh maktx
                          INTO TABLE it_mat_info
                          FROM mara
                          INNER JOIN marc
                          ON marc~matnr = mara~matnr
                          INNER JOIN mbew
                          ON mbew~matnr = mara~matnr
                          AND mbew~bwkey = marc~werks
                          INNER JOIN makt
                          ON makt~matnr = mara~matnr
                          WHERE ( mtart = 'ROH' AND spras = sy-langu )
                          OR    ( mtart = 'ROH1' AND spras = sy-langu ).
  IF sy-subrc NE 0.
  ENDIF.

  SELECT lifnr land1 FROM lfa1 INTO TABLE it_lfa1
                     FOR ALL ENTRIES IN it_mseg
                     WHERE lifnr = it_mseg-lifnr.

** Changed by Matthew Cupples on 08/19/2010
*  SELECT BWART ZZCOD ZZFLG SHKZG FROM ZTFTZ_TCODE
*                                 INTO TABLE IT_FTZTCODE.
  SELECT bwart zzcod zzflg shkzg sakto FROM ztftz_tcode
                                INTO TABLE it_ftztcode.
** End of change on 08/19/2010

** Changed by Furong on 07/10/08

* SELECT MATNR EINA~LIFNR EINE~WERKS EKORG WAERS PEINH
*               BPUMZ BPUMN EFFPR LAND1
*                     INTO TABLE IT_EINA
*                     FROM EINA
*                     INNER JOIN EINE
*                     ON EINE~INFNR = EINA~INFNR
*                     AND EINE~LOEKZ = EINA~LOEKZ
*                     INNER JOIN LFA1
*                     ON LFA1~LIFNR = EINA~LIFNR
*                     FOR ALL ENTRIES IN IT_MSEG
*                     WHERE MATNR = IT_MSEG-MATNR
*                     AND   EINA~LIFNR = IT_MSEG-LIFNR
*                     AND   EINA~LOEKZ = SPACE.

*  DESCRIBE TABLE IT_EINA LINES W_LINES.
*  IF W_LINES = 0.
*  ELSE.
*    SELECT MATNR LIFNR KNUMH INTO TABLE IT_A018
*                             FROM A018
*                             FOR ALL ENTRIES IN IT_EINA
*                             WHERE MATNR = IT_EINA-MATNR
*                             AND   LIFNR = IT_EINA-LIFNR
*                             AND   EKORG = IT_EINA-EKORG
*                             AND   DATAB <= P_DATE
*                             AND   DATBI >= P_DATE
*                             AND   KSCHL = 'PB00'.
*  ENDIF.
*  DESCRIBE TABLE IT_A018 LINES W_LINES.
*  IF W_LINES = 0.
*  ELSE.
*    SORT IT_A018 BY KNUMH.
*    SELECT KNUMH KBETR FROM KONP
*                       INTO TABLE IT_KONP
*                       FOR ALL ENTRIES IN IT_A018
*                       WHERE KNUMH = IT_A018-KNUMH
*                       AND   KSCHL = 'PB00'
*                       AND   LOEVM_KO = SPACE.
*  ENDIF.
  SORT: it_eina BY matnr lifnr,
        it_a018 BY matnr lifnr.

*  CLEAR W_LINES.
*  W_LINES = 1.
*  LOOP AT IT_EINA ASSIGNING <FS_EINA>.
*
*    READ TABLE IT_A018 INTO WA_A018 WITH KEY MATNR = <FS_EINA>-MATNR
*                                             LIFNR = <FS_EINA>-LIFNR
*                                             BINARY SEARCH
*                                             TRANSPORTING KNUMH.
*    IF SY-SUBRC NE 0.
*      <FS_EINA>-KBETR = 0.
*      CONTINUE.
*    ENDIF.
*   READ TABLE IT_KONP INTO WA_KONP WITH TABLE KEY KNUMH = WA_A018-KNUMH
*.
*    IF SY-SUBRC NE 0.
*      <FS_EINA>-KBETR = 0.
*    ELSE.
*      <FS_EINA>-KBETR = WA_KONP-KBETR.
*    ENDIF.
*  ENDLOOP.

  SELECT matnr eina~lifnr eine~werks ekorg waers peinh
                bpumz bpumn effpr land1
**                EBELN EBELP
                      INTO TABLE it_eina_po
                      FROM eina
                      INNER JOIN eine
                      ON eine~infnr = eina~infnr
** Changed by Furong on 07/31/08
*                      AND EINE~LOEKZ = EINA~LOEKZ
** End of change on 07/31/08
                      INNER JOIN lfa1
                      ON lfa1~lifnr = eina~lifnr
                      FOR ALL ENTRIES IN it_mseg
                      WHERE matnr = it_mseg-matnr
                      AND eina~lifnr = it_mseg-lifnr.
** Changed by Furong on 07/31/08
*                      AND   EINA~LOEKZ = SPACE.
** End of change

  DESCRIBE TABLE it_eina_po LINES w_lines.
  IF w_lines = 0.
  ELSE.
    SELECT matnr lifnr knumh INTO TABLE it_a018
                             FROM a018
                             FOR ALL ENTRIES IN it_eina_po
                             WHERE matnr = it_eina_po-matnr
                             AND   lifnr = it_eina_po-lifnr
                             AND   ekorg = it_eina_po-ekorg
                             AND   datab <= p_date
                             AND   datbi >= p_date
                             AND   kschl = 'PB00'.

    SELECT ebeln ebelp matnr werks netpr  bpumz bpumn
     peinh umren umrez
                     INTO TABLE it_ekpo
                     FROM ekpo
                     FOR ALL ENTRIES IN it_mseg
                     WHERE ebeln = it_mseg-ebeln
                     AND ebelp = it_mseg-ebelp
                     AND matnr = it_mseg-matnr
                     AND loekz = space.

    SORT it_ekpo BY ebeln ebelp matnr.
  ENDIF.
  DESCRIBE TABLE it_a018 LINES w_lines.
  IF w_lines = 0.
  ELSE.
    SORT it_a018 BY knumh.
    SELECT knumh kbetr FROM konp
                       INTO TABLE it_konp
                       FOR ALL ENTRIES IN it_a018
                       WHERE knumh = it_a018-knumh
                       AND   kschl = 'PB00'
                       AND   loevm_ko = space.
    SORT: it_a018 BY matnr lifnr.
  ENDIF.

  CLEAR w_lines.
  w_lines = 1.
  LOOP AT it_eina_po ASSIGNING <fs_eina_po>.
    IF <fs_eina_po>-ebeln IS INITIAL.
    READ TABLE it_a018 INTO wa_a018 WITH KEY matnr = <fs_eina_po>-matnr
                                             lifnr = <fs_eina_po>-lifnr
                                                          BINARY SEARCH
                                                     TRANSPORTING knumh.
      IF sy-subrc NE 0.
        <fs_eina_po>-kbetr = 0.
        CONTINUE.
      ENDIF.
   READ TABLE it_konp INTO wa_konp WITH TABLE KEY knumh = wa_a018-knumh
                                                                       .
      IF sy-subrc NE 0.
        <fs_eina_po>-kbetr = 0.
      ELSE.
        <fs_eina_po>-kbetr = wa_konp-kbetr.
      ENDIF.

*    ELSE.
*    READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = <FS_EINA_PO>-EBELN
*                                             EBELP = <FS_EINA_PO>-EBELP
*                                             MATNR = <FS_EINA_PO>-MATNR
*                                                    BINARY SEARCH.
*      IF SY-SUBRC NE 0.
*        <FS_EINA_PO>-KBETR = 0.
*      ELSE.
*        <FS_EINA_PO>-KBETR = WA_EKPO-NETPR.
*      ENDIF.
    ENDIF.
    CLEAR: wa_ekpo, wa_konp, wa_a018.
  ENDLOOP.
  SORT it_eina_po BY matnr lifnr.

** End of change ON 08/10/08

*&---There shouldn't be transactions with following Mov.type for HALB.
  r_nscrap-sign = 'E'.
  r_nscrap-option = 'EQ'.
  r_nscrap-low = '101'.
  APPEND r_nscrap.
  r_nscrap-low = '102'.
  APPEND r_nscrap.
  r_nscrap-low = '122'.
  APPEND r_nscrap.
  r_nscrap-low = '123'.
  APPEND r_nscrap.
  r_nscrap-low = '309'.
  APPEND r_nscrap.
  r_nscrap-low = '310'.
  APPEND r_nscrap.

  r_mtart-sign = 'E'.
  r_mtart-option = 'EQ'.
  r_mtart-low = 'ROH'.
  APPEND r_mtart.
  r_mtart-low = 'ROH1'.
  APPEND r_mtart.
  CLEAR wa_mseg.
  LOOP AT it_mseg ASSIGNING <fs_mseg>
                  WHERE ( mtart EQ 'HALB' AND bwart IN r_nscrap ).
*&-----To delete from it_mseg.
    r_bom_topmat-sign = 'I'.
    r_bom_topmat-option = 'EQ'.
    r_bom_topmat-low = <fs_mseg>-matnr.
    COLLECT r_bom_topmat.
*&-----For BOM explosion.
    wa_halb_exp-matnr = <fs_mseg>-matnr.
    wa_halb_exp-werks = <fs_mseg>-werks.
    wa_halb_exp-datuv = p_date.
    COLLECT wa_halb_exp INTO it_halb_exp.
*&-----For a copy.
    wa_mseg = <fs_mseg>.
    APPEND wa_mseg TO it_mseg1.
*&----Materials with Mov.Typ '991'.
    IF <fs_mseg>-bwart = '991'.
      wa_991_mat-matnr = <fs_mseg>-matnr.
      COLLECT wa_991_mat INTO it_991_mat.
    ENDIF.
    CLEAR: r_bom_topmat, wa_halb_exp, wa_mseg, wa_991_mat.
  ENDLOOP.
  wa_halb_exp-menge = 1.
  MODIFY it_halb_exp FROM wa_halb_exp TRANSPORTING menge
                           WHERE menge IS INITIAL.

*&----Check Special procurement key to get correct plant for BOM.
  SELECT matnr marc~werks wrk02 INTO TABLE it_t460a
                                FROM marc
                                INNER JOIN t460a
                                ON t460a~werks = marc~werks
                                AND t460a~sobsl = marc~sobsl
                               FOR ALL ENTRIES IN it_halb_exp
                                WHERE matnr EQ it_halb_exp-matnr
                                AND  marc~werks EQ it_halb_exp-werks.
  IF sy-subrc NE 0.
  ELSE.
    SORT it_t460a BY matnr werks.
    LOOP AT it_halb_exp ASSIGNING <fs_halb_exp>.
      READ TABLE it_t460a INTO wa_t460a
                           WITH KEY matnr = <fs_halb_exp>-matnr
                                    werks = <fs_halb_exp>-werks
                                     BINARY SEARCH.
      IF sy-subrc NE 0.
      ELSE.
        IF wa_t460a-wrk02 IS INITIAL.
        ELSE.
          <fs_halb_exp>-werks = wa_t460a-wrk02.
          wa_mseg-werks       = wa_t460a-wrk02.
          MODIFY it_mseg FROM wa_mseg TRANSPORTING werks
                                    WHERE matnr = <fs_halb_exp>-matnr.
          CLEAR: wa_mseg, wa_t460a.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DO.
    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name                     = p_srvgrp
      IMPORTING
        max_pbt_wps                    = w_max
        free_pbt_wps                   = w_free
      EXCEPTIONS
        invalid_group_name             = 1
        internal_error                 = 2
        pbt_env_already_initialized    = 3
        currently_no_resources_avail   = 4
        no_pbt_resources_found         = 5
        cant_init_different_pbt_groups = 6
        OTHERS                         = 7.
    IF sy-subrc <> 0 OR w_free = 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  CLEAR w_lines.
  DESCRIBE TABLE it_halb_exp LINES w_lines.

  IF w_lines > w_free.
    w_rem       = w_lines MOD w_free.
    w_no_times  = w_lines / w_free.
    IF w_rem = 0.
    ELSE.
      w_no_times = w_no_times + 1.
    ENDIF.
  ELSE.
    w_no_times = 1.
  ENDIF.
  w_lp_idx = 1.
  WHILE w_lp_idx <= w_no_times.
    IF w_lp_idx = 1.
      w_frm_idx = w_lp_idx.
    ELSE.
      w_frm_idx = w_to_idx + 1.
    ENDIF.
    IF w_lines > w_free.
      w_to_idx  = w_lp_idx * w_free.
    ELSE.
      w_to_idx = w_lines.
    ENDIF.
    LOOP AT it_halb_exp ASSIGNING <fs_halb_exp>
                                  FROM w_frm_idx TO w_to_idx.
** changed by Furong on 05/04/2006
      DO.
        CALL FUNCTION 'Z_FFTZ_EXP_BOM'
          STARTING NEW TASK w_taskname
          DESTINATION IN GROUP p_srvgrp
          PERFORMING bom_exp ON END OF TASK
          EXPORTING
            p_capid               = 'PP01'
            p_datuv               = <fs_halb_exp>-datuv
            p_emeng               = <fs_halb_exp>-menge
            p_mehrs               = 'X'
            p_mmory               = '1'
            p_mtnrv               = <fs_halb_exp>-matnr
            p_stlan               = '1'
            p_werks               = <fs_halb_exp>-werks
            p_pamatnr             = <fs_halb_exp>-matnr
          TABLES
            p_stpox               = it_stpox
          EXCEPTIONS
            communication_failure = 1
            system_failure        = 2
            resource_failure      = 3.
        CASE sy-subrc.
          WHEN 0.
            w_taskname = w_taskname + 1.
            w_snd_jobs = w_snd_jobs + 1.
            EXIT.
          WHEN 1 OR 2.
            w_excep_flag = 'X'.
          WHEN 3.
            IF w_excep_flag = space.
              w_excep_flag = 'X'.
              WAIT UNTIL w_rcv_jobs >= w_snd_jobs UP TO '0.01' SECONDS.
            ELSE.
              WAIT UNTIL w_rcv_jobs >= w_snd_jobs UP TO '0.1' SECONDS.
            ENDIF.
            IF sy-subrc EQ 0.
              CLEAR w_excep_flag.
            ELSE.
*            exit.
            ENDIF.
        ENDCASE.
      ENDDO.
** end of change
    ENDLOOP.

* Replace WAIT statement for loop

*  WAIT UNTIL w_rcv_jobs >= w_snd_jobs.

    DO.
      WAIT UNTIL w_rcv_jobs >= w_snd_jobs.
      IF w_rcv_jobs >= w_snd_jobs.
        EXIT.
      ENDIF.
    ENDDO.
    w_lp_idx = w_lp_idx + 1.
  ENDWHILE.

** Change by Furong on 11/02/2007

  CLEAR: w_snd_jobs, w_rcv_jobs.
  LOOP AT it_halb_eng ASSIGNING <fs_halb_eng>.
*      r_bom_topmat-sign = 'I'.
*      r_bom_topmat-option = 'EQ'.
*      r_bom_topmat-low = <fs_halb_eng>-matnr.
*      COLLECT r_bom_topmat.
*      CLEAR: r_bom_topmat.
*
    DO.
      CALL FUNCTION 'Z_FFTZ_EXP_BOM'
        STARTING NEW TASK w_taskname
        DESTINATION IN GROUP p_srvgrp
        PERFORMING bom_exp ON END OF TASK
        EXPORTING
          p_capid               = 'PP01'
          p_datuv               = <fs_halb_eng>-datuv
          p_emeng               = <fs_halb_eng>-bdmng
          p_mehrs               = 'X'
          p_mmory               = '1'
          p_mtnrv               = <fs_halb_eng>-matnr
          p_stlan               = '1'
          p_werks               = <fs_halb_eng>-werks
          p_pamatnr             = <fs_halb_eng>-pmatnr
        TABLES
          p_stpox               = it_stpox
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2
          resource_failure      = 3.
      CASE sy-subrc.
        WHEN 0.
          w_taskname = w_taskname + 1.
          w_snd_jobs = w_snd_jobs + 1.
          EXIT.
        WHEN 1 OR 2.
          w_excep_flag = 'X'.
        WHEN 3.
          IF w_excep_flag = space.
            w_excep_flag = 'X'.
            WAIT UNTIL w_rcv_jobs >= w_snd_jobs UP TO '0.01' SECONDS.
          ELSE.
            WAIT UNTIL w_rcv_jobs >= w_snd_jobs UP TO '0.1' SECONDS.
          ENDIF.
          IF sy-subrc EQ 0.
            CLEAR w_excep_flag.
          ELSE.
*            exit.
          ENDIF.
      ENDCASE.
    ENDDO.
  ENDLOOP.

  DO.
    WAIT UNTIL w_rcv_jobs >= w_snd_jobs.
    IF w_rcv_jobs >= w_snd_jobs.
      EXIT.
    ENDIF.
  ENDDO.
** End of change

  CLEAR: r_bom_topmat.
  CHECK NOT it_stpox1[] IS INITIAL.
  DELETE it_mseg WHERE matnr IN r_bom_topmat.

  LOOP AT it_stpox1 ASSIGNING <fs_com>.
    wa_bom_comp-matnr = <fs_com>-matnr.
    wa_bom_comp-idnrk = <fs_com>-idnrk.
    wa_bom_comp-werks = <fs_com>-werks.
*    wa_bom_comp-bwart = <fs_com>-bwart.
    wa_bom_comp-mtart = <fs_com>-mtart.
    wa_bom_comp-menge = <fs_com>-menge.
    wa_bom_comp-mngko = <fs_com>-mngko.
    wa_bom_comp-meins = <fs_com>-meins.
    COLLECT wa_bom_comp INTO it_bom_comp.
  ENDLOOP.
  CLEAR: wa_mseg, wa_bom_comp.
  IF <fs_mseg> IS ASSIGNED.
    UNASSIGN <fs_mseg>.
  ENDIF.

  LOOP AT it_mseg1 ASSIGNING <fs_mseg>.
    LOOP AT it_bom_comp INTO wa_bom_comp
                        WHERE matnr = <fs_mseg>-matnr
                           AND werks = <fs_mseg>-werks.
      READ TABLE it_mat_info INTO wa_mat_info
                             WITH TABLE KEY matnr = wa_bom_comp-idnrk
                                            werks = wa_bom_comp-werks.
      IF sy-subrc NE 0.
        CLEAR wa_mseg.
        CONTINUE.
      ENDIF.
      wa_mseg-matnr = wa_bom_comp-idnrk.
      wa_mseg-mtart = wa_bom_comp-mtart.
      wa_mseg-meins = wa_bom_comp-meins.
      wa_mseg-profl = wa_mat_info-profl.
      wa_mseg-ntgew = wa_mat_info-ntgew.
      wa_mseg-gewei = wa_mat_info-gewei.
      wa_mseg-bwart = <fs_mseg>-bwart.
      wa_mseg-werks = wa_bom_comp-werks.
      wa_mseg-menge = <fs_mseg>-menge * wa_bom_comp-mngko.
      wa_mseg-ummat = space.
      wa_mseg-maktx = wa_mat_info-maktx.
      wa_mseg-mngko = wa_bom_comp-menge.
      wa_mseg-pmatnr = wa_bom_comp-matnr.
*    read table it_mseg1 assigning <fs_mseg>
*                         with key matnr = wa_bom_comp-matnr
*                                  bwart = wa_bom_comp-bwart.
*    if sy-subrc ne 0.
*      wa_mseg-lifnr = space.
*      wa_mseg-ebeln = space.
*      wa_mseg-ebelp = space.
*      wa_mseg-shkzg = space.
*    else.
      wa_mseg-lifnr = <fs_mseg>-lifnr.
      wa_mseg-ebeln = <fs_mseg>-ebeln.
      wa_mseg-ebelp = <fs_mseg>-ebelp.
      wa_mseg-shkzg = <fs_mseg>-shkzg.
      wa_mseg-xblnr = <fs_mseg>-xblnr.
      wa_mseg-xabln = <fs_mseg>-xabln.
      wa_mseg-kdauf = <fs_mseg>-kdauf.
      wa_mseg-erfme = <fs_mseg>-erfme.
      wa_mseg-pprofl = <fs_mseg>-profl.
*    endif.
      APPEND wa_mseg TO it_mseg.
      CLEAR: wa_mseg.
    ENDLOOP.
  ENDLOOP.
  FREE: it_mseg1.
  IF <fs_mseg> IS ASSIGNED.
    UNASSIGN <fs_mseg>.
  ENDIF.
ENDFORM.                    " get_bom_info
*&---------------------------------------------------------------------*
*&      Form  get_tcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_MSEG>_MTART  text
*      -->P_<FS_MSEG>_BWART  text
*      -->P_<FS_MSEG>_PROFL  text
*      -->P_<FS_MSEG>_SAKTO  text
*      <--P_W_FTZ_TCODE      text
*----------------------------------------------------------------------*
FORM get_tcode USING    p_mtart
                        p_bwart
                        p_shkzg
                        p_profl
                        p_xabln
** Changed by Matthew Cupples on 08/19/2010
                        p_sakto
** End of change on 08/20/2010
               CHANGING p_matnr
                        p_ftz_tcode
                        p_ordnum_recp
                        p_ordnum_ship
                        p_tid
                        p_bill
                        p_ftztcode STRUCTURE wa_ftztcode.

  DATA: p_act LIKE ztftz_tcode-zzflg.

  CASE p_mtart.
    WHEN 'ROH' OR 'ROH1'.
      READ TABLE it_ftztcode INTO p_ftztcode
                             WITH KEY bwart = p_bwart.
      IF sy-subrc NE 0.
      ELSE.
        p_ftz_tcode = p_ftztcode-zzcod.
        p_act       = p_ftztcode-zzflg.
        CASE p_bwart.
          WHEN '601'.
            IF p_profl EQ 'M'.
              CLEAR: p_ftz_tcode,p_act.
              p_ftz_tcode = 'SPNM'.
              p_act       = 'GI'.
            ENDIF.
          WHEN '309' OR '310'.
            IF p_shkzg EQ 'H'.
              CLEAR: p_ftz_tcode,p_act.
              p_ftz_tcode = 'CNPC'.
              p_act       = 'GI'.
            ENDIF.
          WHEN '991'.
            CASE p_xabln.
              WHEN space.
                p_ftz_tcode = 'XPPC'.
                p_act       = 'GI'.
              WHEN 'S' OR 'T'.
                p_ftz_tcode = 'SPPC'.
                p_act       = 'GR'.
            ENDCASE.
* i.g.moon 5/26/10 {
          WHEN '655'.
            p_ftz_tcode = 'APPC'.
            p_act       = 'GR'.
* }
** Changed by Matthew Cupples on 08/19/2010
          WHEN '201' OR '202'.
            CLEAR: p_ftz_tcode,p_act.
            READ TABLE it_ftztcode INTO p_ftztcode
                    WITH KEY bwart = p_bwart sakto = p_sakto.
            IF sy-subrc EQ 0.
              p_ftz_tcode  = p_ftztcode-zzcod.
              p_act        = p_ftztcode-zzflg.
            ELSE.
              IF p_bwart = '201'.
                p_ftz_tcode  = 'XPPC'.
                p_act        = 'GI'.
              ELSE.
                p_ftz_tcode  = 'XNPC'.
                p_act        = 'GR'.
              ENDIF.
            ENDIF.
** End of change on 08/19/2010

        ENDCASE.
        PERFORM ordernumreceipt_by_profl USING p_bwart
                                               p_profl
                                               p_act
                                               p_ftz_tcode
                                         CHANGING p_matnr
                                                  p_ordnum_recp
                                                  p_tid
                                                  p_bill.

      ENDIF.
  ENDCASE.
ENDFORM.                    " get_tcode

*&---------------------------------------------------------------------*
*&      Form  list_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_price.
  SORT it_list_price BY matnr ebeln.
  DELETE ADJACENT DUPLICATES FROM it_list_price COMPARING matnr ebeln.
  WRITE : / 'Price List'.
  WRITE: /(18) 'Material Number'.
  WRITE: (10) 'PO number'.
  WRITE: (18) 'Standard Price'.
  WRITE: (15) 'Info Price'.
  WRITE: (15) 'PO Price'.
  WRITE : / sy-uline.
  LOOP AT it_list_price.
    WRITE:/(18) it_list_price-matnr.
    WRITE:(10) it_list_price-ebeln.
    WRITE:(15) it_list_price-std.
    WRITE:(15) it_list_price-info.
    WRITE:(15) it_list_price-po.
  ENDLOOP.
ENDFORM.                    " list_price
