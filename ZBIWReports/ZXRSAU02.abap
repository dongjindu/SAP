*----------------------------------------------------------------------*
*   INCLUDE ZXRSAU02                                                   *
*----------------------------------------------------------------------*
TABLES : mbew, mard, marc, ska1,skb1, plpo, mara, kna1, equz, crhd,
         zsco_nafta_ck11, zsco_color, imak, impr, tadir, trdir,
         progdir, bapiappreqstatus, aufk, ania, taprf, imakpa,
         ztitdevc, ztitbpml, agr_1251, /sappce/torg01t, /sappce/hrp1961.

DATA : l_biw_mara_s LIKE biw_mara_s,
       l_zoxud10060 LIKE zoxud10060,
       l_zoxud10062 LIKE zoxud10062,
       l_biw_zoxud10075 LIKE zoxud10075,
       l_bwe_equi LIKE bwe_equi,
       l_biw_lfa1_s LIKE biw_lfa1_s, "vendor attr
       l_biw_coor LIKE biw_coor,     "order attr
       l_zoxud10109 LIKE zoxud10109, "program attr
       l_zoxud10111 LIKE zoxud10111, "t-code attr
       l_zoxud10080 LIKE zoxud10080, "ar status
       l_zoxud10049 LIKE zoxud10049, "BPML
       l_zoxud10141 LIKE zoxud10141, "Function
       l_zoxud10023 LIKE zoxud10023,
       l_hrms_bw_io_orgunit LIKE hrms_bw_io_orgunit. "UM

DATA : l_bklas LIKE mbew-bklas, "Valuation class
       l_ekgrp LIKE marc-ekgrp, "Purchasing group
       l_mmsta LIKE marc-mmsta, "Plant-Specific Material Status
       l_dismm LIKE marc-dismm, "MRP type
       l_dispo LIKE marc-dispo, "MRP controller
       l_disls LIKE marc-disls, "Lot size (materials planning)
       l_bstmi LIKE marc-bstmi, "Minimum lot size
       l_bstma LIKE marc-bstma, "Maximum lot size
       l_bstrf LIKE marc-bstrf, "Rounding value
                                "-for purchase order quantity
       l_beskz LIKE marc-beskz, "Procurement Type
       l_lgpro LIKE marc-lgpro, "Issue Storage Location
       l_vspvb LIKE marc-vspvb, "Proposed Supply Area
                                "-in Material Master Record
       l_plifz LIKE marc-plifz, "Planned delivery time in days
       l_ausdt LIKE marc-ausdt, "Effective-Out Date
       l_abcin LIKE marc-abcin, "Physical inventory indicator
                                "-for cycle counting
       l_zshop LIKE plpo-usr02, "Enhanced shop code
       l_busab LIKE skb1-busab, "Account clerk for average balance ledg.

       l_kunnr LIKE kna1-kunnr,
       l_land1 LIKE kna1-land1,
       l_gewrk LIKE equz-gewrk, "Object ID
       l_catxt LIKE /sappce/torg01t-catxt.

DATA: i_ztco_nafta_ck11 LIKE zsco_nafta_ck11 OCCURS 0 WITH HEADER LINE,
      i_zsco_color LIKE zsco_color OCCURS 0 WITH HEADER LINE,
      i_ckmlmv013 LIKE ckmlmv013 OCCURS 0 WITH HEADER LINE,
      i_arstat LIKE bapiappreqstatus OCCURS 0 WITH HEADER LINE,
      i_bpml LIKE zoxud10049 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF i_agr1 OCCURS 0,
        low LIKE agr_1251-low,
        l_cnt TYPE i,
      END OF i_agr1.

DATA : l_tabix LIKE sy-tabix.
DATA : xktoks  LIKE ska1-ktoks.
DATA : i_iobjacct LIKE iobjacct.
DATA : a_tabix LIKE sy-tabix.
DATA : len TYPE i.

DATA : l_hex_cr TYPE xstring.  "VALUE '0A'.
DATA : l_hex_lf TYPE xstring.  "VALUE '0D'.

DATA : l_str_cr TYPE string.
DATA : l_str_lf TYPE string.

DATA x_crsp       TYPE xstring.
DATA x_lfsp       TYPE xstring.

DATA s_crsp       TYPE string.
DATA s_lfsp       TYPE string.

l_hex_cr = '0A'.
l_hex_lf = '0D'.

x_crsp = '0A20'.
x_lfsp = '0D20'.

FIELD-SYMBOLS: <cr>, <lf>, <crsp>, <lfsp>.
*

IF sy-saprl < '700'.
  ASSIGN l_hex_cr TO <cr>.
  ASSIGN l_hex_lf TO <lf>.
  ASSIGN x_crsp TO <crsp>.
  ASSIGN x_lfsp TO <lfsp>.
ELSE.
  CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
    EXPORTING
      im_xstring = l_hex_cr  " variable type string
    IMPORTING
      ex_string  = l_str_cr. " variable type xstring

  CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
    EXPORTING
      im_xstring = l_hex_lf  " variable type string
    IMPORTING
      ex_string  = l_str_lf. " variable type xstring

  CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
    EXPORTING
      im_xstring = x_crsp  " variable type string
    IMPORTING
      ex_string  = s_crsp. " variable type xstring

  CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
    EXPORTING
      im_xstring = x_lfsp  " variable type string
    IMPORTING
      ex_string  = s_lfsp. " variable type xstring
ENDIF.

CASE i_datasource.

  WHEN '1CL_OEQU004'.
    LOOP AT i_t_data INTO l_zoxud10060.
      l_tabix = sy-tabix.

      CLEAR ztebpp_deal_conv.
      SELECT SINGLE *
        FROM ztebpp_deal_conv
       WHERE  old_dealer = l_zoxud10060-p_dist_code002.
      IF sy-subrc EQ 0.
        l_zoxud10060-new_dealer = ztebpp_deal_conv-new_dealer.
      ENDIF.

      MODIFY i_t_data FROM l_zoxud10060 INDEX l_tabix.

    ENDLOOP.

  WHEN '1CL_OEQU006'.
    LOOP AT i_t_data INTO l_zoxud10062.
      l_tabix = sy-tabix.

      CLEAR ztebpp_deal_conv.
      SELECT SINGLE *
        FROM ztebpp_deal_conv
       WHERE  old_dealer = l_zoxud10062-p_dist_code002.
      IF sy-subrc EQ 0.
        l_zoxud10062-new_dealer = ztebpp_deal_conv-new_dealer.
      ENDIF.

      MODIFY i_t_data FROM l_zoxud10062 INDEX l_tabix.

    ENDLOOP.


****** 0MATERIAL_ATTR Add-on Data Source .
  WHEN '0MATERIAL_ATTR'.
    LOOP AT i_t_data INTO l_biw_mara_s.
      l_tabix = sy-tabix.

      IF sy-saprl < '700'.
        SEARCH l_biw_mara_s-matnr FOR <cr>.
      ELSE.
        SEARCH l_biw_mara_s-matnr FOR l_str_cr.
      ENDIF.

      IF sy-subrc EQ 0.
        DELETE i_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

      IF sy-saprl < '700'.
        SEARCH l_biw_mara_s-matnr FOR <lf>.
      ELSE.
        SEARCH l_biw_mara_s-matnr FOR l_str_lf.
      ENDIF.

      IF sy-subrc EQ 0.
        DELETE i_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.


      SELECT SINGLE bklas INTO l_bklas
        FROM mbew
       WHERE matnr = l_biw_mara_s-matnr
         AND bwkey = 'P001'.
      IF sy-subrc NE 0.
        SELECT SINGLE bklas INTO l_bklas
          FROM mbew
         WHERE matnr = l_biw_mara_s-matnr
           AND bwkey = 'E001'.
        IF sy-subrc NE 0.
          SELECT SINGLE bklas INTO l_bklas
            FROM mbew
           WHERE matnr = l_biw_mara_s-matnr
             AND bwkey = 'E002'.
        ENDIF.
      ENDIF.

      l_biw_mara_s-bklas = l_bklas.

      SELECT SINGLE *
        FROM marc
       WHERE matnr = l_biw_mara_s-matnr
         AND werks = 'P001'.
      IF sy-subrc NE 0.
        SELECT SINGLE *
          FROM marc
         WHERE matnr = l_biw_mara_s-matnr
           AND werks = 'E001'.
        IF sy-subrc NE 0.
          SELECT SINGLE *
            FROM marc
           WHERE matnr = l_biw_mara_s-matnr
             AND werks = 'E002'.
        ENDIF.
      ENDIF.

      l_biw_mara_s-ekgrp = marc-ekgrp.
      l_biw_mara_s-mmsta = marc-mmsta.
      l_biw_mara_s-dismm = marc-dismm.
      l_biw_mara_s-dispo = marc-dispo.
      l_biw_mara_s-disls = marc-disls.
      l_biw_mara_s-bstmi = marc-bstmi.
      l_biw_mara_s-bstma = marc-bstma.
      l_biw_mara_s-bstrf = marc-bstrf.
      l_biw_mara_s-beskz = marc-beskz.
      l_biw_mara_s-lgpro = marc-lgpro.
      l_biw_mara_s-vspvb = marc-vspvb.
      l_biw_mara_s-plifz = marc-plifz.
      l_biw_mara_s-ausdt = marc-ausdt.
      l_biw_mara_s-abcin = marc-abcin.
* Loading Group for Engine sales
      IF l_biw_mara_s-matnr(2) EQ 'AU'
        AND l_biw_mara_s-mtart EQ 'HALB' .
        SELECT SINGLE ladgr INTO marc-ladgr
          FROM marc
         WHERE matnr = l_biw_mara_s-matnr
           AND werks = 'E001'.
        l_biw_mara_s-ladgr = marc-ladgr.
      ENDIF.

*to get RP (reporting point)
      CLEAR plpo.
      SELECT SINGLE usr01 INTO l_biw_mara_s-reppoint_ext
      FROM plpo
      WHERE plnty = 'M'
        AND plnnr = 'RP'
        AND usr00 = l_biw_mara_s-vspvb.

*to get the customer code
*      CLEAR mara.
*      SELECT SINGLE *
*             FROM mara
*             WHERE matnr = l_biw_mara_s-matnr.
*      IF sy-subrc EQ 0.
*        IF mara-mtart EQ 'FERT'.
      IF l_biw_mara_s-mtart EQ 'FERT'.
        IF l_biw_mara_s-matnr+13(1) = ' '.  "old BOM
          l_kunnr = l_biw_mara_s-matnr+1(5).

          l_biw_mara_s-zmi = l_biw_mara_s-matnr+6(7). "MI
        ELSE.
          SELECT SINGLE old_dealer INTO l_kunnr FROM ztebpp_deal_conv
                    WHERE new_dealer = l_biw_mara_s-matnr+4(1).
          CONCATENATE l_biw_mara_s-matnr+1(3) l_kunnr(2) INTO l_kunnr.

          l_biw_mara_s-zmi = l_biw_mara_s-matnr+5(9). "MI
        ENDIF.
        SELECT SINGLE kunnr land1 INTO (l_kunnr, l_land1) FROM kna1
                                         WHERE kunnr = l_kunnr.
        IF sy-subrc = 0.
          l_biw_mara_s-kunnr = l_kunnr.
          l_biw_mara_s-land1 = l_land1.
        ENDIF.

      ENDIF.
*      ENDIF.

* Material description in upper case for matchcodes
      SELECT SINGLE maktg INTO l_biw_mara_s-maktg
        FROM makt
       WHERE matnr = l_biw_mara_s-matnr
         AND spras = 'E'.

      IF sy-saprl < '700'.
        TRANSLATE l_biw_mara_s-maktg USING <crsp>.
        TRANSLATE l_biw_mara_s-maktg USING <lfsp>.
      ELSE.
        TRANSLATE l_biw_mara_s-maktg USING s_crsp.
        TRANSLATE l_biw_mara_s-maktg USING s_lfsp.
      ENDIF.

*to get the material (colorless)
      REFRESH i_ztco_nafta_ck11. CLEAR i_ztco_nafta_ck11.
      REFRESH i_zsco_color. CLEAR i_zsco_color.
      i_ztco_nafta_ck11-compn = l_biw_mara_s-matnr.
      APPEND i_ztco_nafta_ck11.

      CALL FUNCTION 'Z_CO_ELIMINATE_COLOR_CODE'
        TABLES
          p_t_nafta = i_ztco_nafta_ck11
          gt_color  = i_zsco_color.

      READ TABLE i_zsco_color INDEX 1.
      IF sy-subrc EQ 0.
        l_biw_mara_s-zclmatnr = i_zsco_color-matnr_o.
      ELSE.
        l_biw_mara_s-zclmatnr = l_biw_mara_s-matnr.
      ENDIF.

      MODIFY i_t_data FROM l_biw_mara_s INDEX l_tabix.

    ENDLOOP.


****** 0EQUIPMENT_ATTR Add-on Data Source .
  WHEN '0EQUIPMENT_ATTR'.
    LOOP AT i_t_data INTO l_bwe_equi.
      l_tabix = sy-tabix.

      SELECT SINGLE eqtyp begru
        INTO (l_bwe_equi-eqtyp, l_bwe_equi-begru)
        FROM equi
       WHERE equnr = l_bwe_equi-equnr.

      SELECT SINGLE gewrk INTO l_gewrk
      FROM equz
      WHERE equnr = l_bwe_equi-equnr
        AND datbi = '99991231'.

      SELECT SINGLE arbpl INTO l_bwe_equi-arbpl
      FROM crhd
      WHERE objty = 'A'
        AND objid = l_gewrk.

      MODIFY i_t_data FROM l_bwe_equi INDEX l_tabix.

    ENDLOOP.


  WHEN '0ACCOUNT_ATTR'.
    LOOP AT i_t_data INTO i_iobjacct.
      CLEAR xktoks.
      a_tabix = sy-tabix.

      SELECT SINGLE ktoks INTO xktoks FROM ska1
        WHERE ktopl = i_iobjacct-chartaccts
          AND saknr = i_iobjacct-account.

      SELECT SINGLE busab INTO l_busab FROM skb1
        WHERE bukrs = 'H201'
          AND saknr = i_iobjacct-account.

      IF sy-subrc = 0.
        i_iobjacct-ktoks = xktoks.
        i_iobjacct-busab = l_busab.
        MODIFY i_t_data FROM i_iobjacct INDEX a_tabix.
      ENDIF.

    ENDLOOP.

  WHEN '0VENDOR_ATTR'.
    LOOP AT i_t_data INTO l_biw_lfa1_s.

      l_tabix = sy-tabix.

      SELECT SINGLE akont zterm zwels xedip reprf busab kverm
      INTO (l_biw_lfa1_s-akont,
            l_biw_lfa1_s-zterm,
            l_biw_lfa1_s-zwels,
            l_biw_lfa1_s-xedip,
            l_biw_lfa1_s-reprf,
            l_biw_lfa1_s-busab,
            l_biw_lfa1_s-kverm)
      FROM lfb1
      WHERE lifnr = l_biw_lfa1_s-lifnr
        AND bukrs = 'H201'.

      SELECT SINGLE verkf
      INTO l_biw_lfa1_s-verkf
      FROM lfm1
      WHERE lifnr = l_biw_lfa1_s-lifnr
        AND ekorg = 'PU01'.

      MODIFY i_t_data FROM l_biw_lfa1_s INDEX l_tabix.

    ENDLOOP.

  WHEN '0COORDER_ATTR'.
    DATA: l_obj LIKE aufk-objnr.

    REFRESH i_ckmlmv013.
    SELECT * INTO TABLE i_ckmlmv013
    FROM ckmlmv013.
    SORT i_ckmlmv013.

    LOOP AT i_t_data INTO l_biw_coor.

      l_tabix = sy-tabix.

      CLEAR i_ckmlmv013.
      READ TABLE i_ckmlmv013 WITH KEY aufnr = l_biw_coor-aufnr BINARY
SEARCH.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING i_ckmlmv013 TO l_biw_coor.
      ENDIF.

      SELECT SINGLE ivpro scope izwek objnr
      INTO (l_biw_coor-ivpro,
            l_biw_coor-scope,
            l_biw_coor-izwek,
            l_obj)
      FROM aufk
      WHERE aufnr = l_biw_coor-aufnr.

      IF sy-subrc EQ 0.
        SELECT SINGLE anlkl
        INTO l_biw_coor-anlkl
        FROM ania
        WHERE objnr = l_obj.

        IF sy-subrc NE 0.
          SELECT SINGLE anlkl_s
          INTO l_biw_coor-anlkl
          FROM taprf
          WHERE ivpro = l_biw_coor-ivpro.
        ENDIF.
      ENDIF.

      MODIFY i_t_data FROM l_biw_coor INDEX l_tabix.
      CLEAR: aufk, ania, l_obj,taprf.
    ENDLOOP.

  WHEN 'ZIM_IMPR'.

    LOOP AT i_t_data INTO l_biw_zoxud10075.

      l_tabix = sy-tabix.

      SELECT SINGLE ivart prart auart scope
      INTO (l_biw_zoxud10075-ivart,
            l_biw_zoxud10075-prart,
            l_biw_zoxud10075-auart,
            l_biw_zoxud10075-scope)
      FROM imak
      WHERE posnr = l_biw_zoxud10075-posid.
      IF sy-subrc EQ 0.
        MODIFY i_t_data FROM l_biw_zoxud10075 INDEX l_tabix.
      ENDIF.

    ENDLOOP.

  WHEN 'Z_TRDIR'.

    LOOP AT i_t_data INTO l_zoxud10109.

      l_tabix = sy-tabix.

      CLEAR tadir.
      SELECT SINGLE devclass
      INTO (l_zoxud10109-devclass)
      FROM tadir
      WHERE pgmid = 'R3TR'
        AND object = 'PROG'
        AND obj_name = l_zoxud10109-name.

* get lv1, lv2, lv3, lv4
      CLEAR len.
      IF l_zoxud10109-devclass(1) = 'Z'.
        len = strlen( l_zoxud10109-devclass ).
        IF len = 8.
          l_zoxud10109-ztdevc1 = l_zoxud10109-devclass+1(1).
          l_zoxud10109-ztdevc2 = l_zoxud10109-devclass+2(2).
          l_zoxud10109-ztdevc3 = l_zoxud10109-devclass+4(2).
          l_zoxud10109-ztdevc4 = l_zoxud10109-devclass+6(2).
        ENDIF.
      ELSE.
        CLEAR ztitdevc.
        SELECT SINGLE *
        FROM ztitdevc
        WHERE devclass = l_zoxud10109-devclass.
        IF sy-subrc EQ 0.
          l_zoxud10109-ztdevc1 = ztitdevc-ztdevc1.
          l_zoxud10109-ztdevc2 = ztitdevc-ztdevc2.
          l_zoxud10109-ztdevc3 = ztitdevc-ztdevc3.
        ENDIF.
      ENDIF.
*

      CLEAR progdir.
      SELECT SINGLE state
      INTO (l_zoxud10109-state)
      FROM progdir
      WHERE name = l_zoxud10109-name.

      l_zoxud10109-name1 = l_zoxud10109-name.
      MODIFY i_t_data FROM l_zoxud10109 INDEX l_tabix.

    ENDLOOP.

  WHEN 'Z_TSTC'.

    REFRESH i_agr1.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE i_agr1
    FROM agr_1251
    WHERE object = 'S_TCODE'
      AND field = 'TCD'
      AND agr_name LIKE 'Z%'.

*    SORT i_agr1 BY low.

    DATA: i_agr2 LIKE i_agr1 OCCURS 0 WITH HEADER LINE.

    i_agr2[] = i_agr1[].
    REFRESH i_agr1.
    CLEAR i_agr2.
    LOOP AT i_agr2.
      i_agr1-low = i_agr2-low.
      i_agr1-l_cnt = 1.
      COLLECT i_agr1.
      CLEAR i_agr1.
    ENDLOOP.
    REFRESH i_agr2.

    SORT i_agr1 BY low.

    LOOP AT i_t_data INTO l_zoxud10111.

      l_tabix = sy-tabix.

      CLEAR tstc.
      SELECT SINGLE pgmna
      INTO (l_zoxud10111-pgmna)
      FROM tstc
      WHERE tcode = l_zoxud10111-tcode.

      DATA: l_tcode LIKE zoxud10111-tcode.
      CLEAR l_tcode.

      IF l_zoxud10111-pgmna IS INITIAL.
*break-point.
        CALL FUNCTION 'RS_TRANSACTION_SINGLE_GET'
          EXPORTING
            parameter_tcode = l_zoxud10111-tcode
          IMPORTING
            tcode           = l_tcode.

        CLEAR tstc.
        SELECT SINGLE pgmna
        INTO (l_zoxud10111-pgmna)
        FROM tstc
        WHERE tcode = l_tcode.

      ENDIF.

      CLEAR trdir.
      SELECT SINGLE name subc cnam cdat unam udat
      INTO (l_zoxud10111-name, l_zoxud10111-subc, l_zoxud10111-cnam,
            l_zoxud10111-cdat, l_zoxud10111-unam, l_zoxud10111-udat)
      FROM trdir
      WHERE name = l_zoxud10111-pgmna.

      CLEAR tadir.
      SELECT SINGLE devclass
      INTO (l_zoxud10111-devclass)
      FROM tadir
      WHERE pgmid = 'R3TR'
        AND object = 'TRAN'
        AND obj_name = l_zoxud10111-tcode.

* get lv1, lv2, lv3, lv4
      CLEAR len.
      IF l_zoxud10111-devclass(1) = 'Z'.
        len = strlen( l_zoxud10111-devclass ).
        IF len = 8.
          l_zoxud10111-ztdevc1 = l_zoxud10111-devclass+1(1).
          l_zoxud10111-ztdevc2 = l_zoxud10111-devclass+2(2).
          l_zoxud10111-ztdevc3 = l_zoxud10111-devclass+4(2).
          l_zoxud10111-ztdevc4 = l_zoxud10111-devclass+6(2).
        ENDIF.
      ELSE.
        CLEAR ztitdevc.
        SELECT SINGLE *
        FROM ztitdevc
        WHERE devclass = l_zoxud10111-devclass.
        IF sy-subrc EQ 0.
          l_zoxud10111-ztdevc1 = ztitdevc-ztdevc1.
          l_zoxud10111-ztdevc2 = ztitdevc-ztdevc2.
          l_zoxud10111-ztdevc3 = ztitdevc-ztdevc3.
        ENDIF.
      ENDIF.
*

      SELECT SINGLE state
      INTO (l_zoxud10111-state)
      FROM progdir
      WHERE name = l_zoxud10111-tcode.

* get info on using in Roles
      CLEAR i_agr1.
      READ TABLE i_agr1 WITH KEY low = l_zoxud10111-tcode
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_zoxud10111-zinduiro = 'X'.
        l_zoxud10111-zrusgct = i_agr1-l_cnt.
      ENDIF.

      MODIFY i_t_data FROM l_zoxud10111 INDEX l_tabix.

    ENDLOOP.

  WHEN 'ZIM_IMAK'.
    LOOP AT i_t_data INTO l_zoxud10080.
      l_tabix = sy-tabix.

      DATA: l_posnr(24). CLEAR l_posnr.
      l_posnr = l_zoxud10080-posnr.

      REFRESH i_arstat. CLEAR i_arstat.

      CALL FUNCTION 'BAPI_APPREQUEST_GETSTATUS'
        EXPORTING
          externalnumber              = l_posnr
          language                    = 'E'
*         LANGUAGE_ISO                =
        TABLES
          apprequest_status           = i_arstat
*         APPREQUEST_USER_STATUS      =
*         APPREQUESTVARNT_STATUS      =
*         APPREQUESTVARNT_USER_STATUS =
*         RETURN                      =
        .
      READ TABLE i_arstat INDEX 1.
      IF sy-subrc EQ 0.
        l_zoxud10080-arstat = i_arstat-text.
      ENDIF.
* begin of to get addtional
      CLEAR imakpa.
      SELECT SINGLE akostl INTO l_zoxud10080-akostl
      FROM imakpa
      WHERE posnr = l_posnr.

      SPLIT l_zoxud10080-usr01 AT '^' INTO l_zoxud10080-zpjt
                                           l_zoxud10080-zpot
                                           l_zoxud10080-zcomp
                                           l_zoxud10080-zinvt
                                           l_zoxud10080-zdivision
                                           l_zoxud10080-zstd1
                                           l_zoxud10080-zstd2.
* end of to get addtional


      MODIFY i_t_data FROM l_zoxud10080 INDEX l_tabix.

    ENDLOOP.

  WHEN 'Z_ZVBW_ZTITBPML'.
    LOOP AT i_t_data INTO l_zoxud10049.
      l_tabix = sy-tabix.
* get count of tcode
      IF l_zoxud10049-tcode <> ' '.
        SELECT COUNT(*) INTO l_zoxud10049-cnt
        FROM ztitbpml
        WHERE tcode = l_zoxud10049-tcode.
        IF sy-subrc EQ 0.
          MODIFY i_t_data FROM l_zoxud10049 INDEX l_tabix.
        ENDIF.
      ELSE.
        DELETE i_t_data INDEX l_tabix.
      ENDIF.
    ENDLOOP.
    SORT i_t_data.
    DELETE ADJACENT DUPLICATES FROM i_t_data.
*   if sy-subrc eq 0.
*
*   endif.

  WHEN 'Z_INFO_FUNC'.
    DATA: l_pgname LIKE trdir-name.
    CLEAR l_pgname.
    LOOP AT i_t_data INTO l_zoxud10141.
      l_tabix = sy-tabix.
*data: l_pgname like trdir-name.
*clear l_pgname.
      CONCATENATE l_zoxud10141-pname+3 'U' l_zoxud10141-include INTO l_pgname.
      IF l_zoxud10141-funcname <> ' '.
        SELECT SINGLE cdat INTO l_zoxud10141-freedate
        FROM trdir
        WHERE name = l_pgname.
        IF sy-subrc EQ 0.
          MODIFY i_t_data FROM l_zoxud10141 INDEX l_tabix.
        ENDIF.
      ENDIF.

    ENDLOOP.

  WHEN 'Z_ZTSD_UM_ATTR'.
    LOOP AT i_t_data INTO l_zoxud10023.
      l_tabix = sy-tabix.

      IF  l_zoxud10023-status NE 'F' AND  l_zoxud10023-status NE ' '.
        DELETE i_t_data INDEX l_tabix.
      ENDIF.

    ENDLOOP.
*  CLEAR: L_MTART, L_RAUBE,L_MVGR1,L_MVGR2,L_MVGR3,L_MVGR4,L_MVGR5,
*         L_SPART, L_PRICE.

  WHEN '0ORGUNIT_ATTR'.
    LOOP AT i_t_data INTO l_hrms_bw_io_orgunit.

      l_tabix = sy-tabix.

      SELECT SINGLE categ
      INTO l_hrms_bw_io_orgunit-categ
      FROM /sappce/hrp1961
      WHERE objid = l_hrms_bw_io_orgunit-orgeh.

      IF sy-subrc EQ 0.

        CLEAR l_catxt.

        SELECT SINGLE catxt INTO l_catxt
        FROM /sappce/torg01t
        WHERE spras = 'EN'
          AND categ = l_hrms_bw_io_orgunit-categ.
        IF sy-subrc EQ 0.
          l_hrms_bw_io_orgunit-qjobcd = l_catxt(5).
        ENDIF.

      ENDIF.
      MODIFY i_t_data FROM l_hrms_bw_io_orgunit INDEX l_tabix.

    ENDLOOP.

ENDCASE.
