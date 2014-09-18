************************************************************************
* Program Name      : ZRMMPM29R_UNDETERMIN_PRICE
* Author            : Byung-sung, Bae
* Creation Date     : 2003.11.17.
* Specifications By : Byung-sung, Bae
* Pattern           : Report 1-1
* Development Request No : UD1K908338
* Addl Documentation:
* Description       : Undeterminated Price by Material
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zrmmpm29r_undetermin_price .
*----- Type
TYPE-POOLS : slis, sp01r.

TABLES: mara,
        marc,
        lfa1,
        konh,
        eina,
        eine.

*----- Internal tables
DATA: BEGIN OF it_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,                  "Material
        maktx   LIKE   makt-maktx,                  "Description
*        lifnr   LIKE   lfa1-lifnr,
      END   OF it_matnr.

DATA: BEGIN OF it_gr_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,                  "Material
        lifnr   LIKE   lfa1-lifnr,
        datab   LIKE   konh-datab,
        datbi   LIKE   konh-datbi,
      END   OF it_gr_matnr.

DATA: BEGIN OF it_knumh OCCURS 0,
        matnr LIKE mara-matnr,
        lifnr LIKE lfa1-lifnr,
        knumh LIKE konh-knumh,
        datab LIKE konh-datab,
        datbi LIKE konh-datbi,
        ekgrp LIKE eine-ekgrp,
        eknam LIKE t024-eknam,
        vakey LIKE konh-vakey,
        umrez LIKE eina-umrez,
        umren LIKE eina-umren,
      END   OF it_knumh.

DATA: BEGIN OF it_info OCCURS 0,
        matnr LIKE mara-matnr,
        lifnr LIKE lfa1-lifnr,
        datab LIKE konh-datab,
        datbi LIKE konh-datbi,
        vakey LIKE konh-vakey,
        maktx LIKE makt-maktx,
        ekorg LIKE ekko-ekorg,
        ekgrp LIKE ekko-ekgrp,
        eknam LIKE t024-eknam,
        kzust LIKE konh-kzust,
        kschl LIKE konh-kschl,
        kbetr LIKE konp-kbetr,
        kpein LIKE konp-kpein,
        kmein LIKE konp-kmein,
        waers LIKE ekko-waers,
        name1 LIKE lfa1-name1,
        erdat LIKE konh-erdat,
        kumza LIKE konp-kumza,
        kumne LIKE konp-kumne,
        umren LIKE eina-umren,
        umrez LIKE eina-umrez,
      END   OF it_info.

DATA: BEGIN OF it_gr OCCURS 0,
        matnr   LIKE   mara-matnr,              "Material
        maktx   LIKE   makt-maktx,              "Description
*        werks   LIKE   t001w-werks,             "Plant
        netpr_s LIKE   ztmm_analy-wrbtr,        "Standard price
        peinh_s LIKE   ztmm_analy-peinh,        "Standard price unit
        lifnr   LIKE   lfa1-lifnr,              "Vendor
        name1   LIKE   lfa1-name1,              "Name
        netpr_i LIKE   ekpo-netpr,              "Info price
        peinh_i LIKE   ekpo-peinh,              "Info price unit
        wrbtr   LIKE   bseg-wrbtr,              "GR Amount
        menge   LIKE   mseg-menge,              "GR Quantity
        meins   LIKE   mseg-meins,              "UoM
        ekgrp   LIKE   ekko-ekgrp,              "Purchasing group
        eknam   LIKE   t024-eknam,              "Description
        datab   LIKE   konh-datab,              "Valid from
        erdat   LIKE   sy-datum,                "Created on
        waers   LIKE   ekko-waers,              "Currency
        waers_s LIKE   ekko-waers,              "Std price currency
        kzust   LIKE   konh-kzust,              "Reason code
      END   OF it_gr.

DATA: BEGIN OF it_lifnr OCCURS 0,
        lifnr   LIKE   lfa1-lifnr,
        name1   LIKE   lfa1-name1,
      END   OF it_lifnr.

*----- Working area
DATA: wa_master LIKE it_info.

DATA: BEGIN OF wa_gr,
        matnr   LIKE   mara-matnr,
        lifnr   LIKE   lfa1-lifnr,
        budat   LIKE   mkpf-budat,
        menge   LIKE   mseg-menge,
        meins   LIKE   mseg-meins,
        waers   LIKE   mseg-waers,
        wrbtr   LIKE   bseg-wrbtr,
        peinh   LIKE   ekpo-peinh,
        werks   LIKE   t001w-werks,
        waers_s LIKE   ekko-waers,
      END   OF wa_gr.

DATA: wa_matnr_f  LIKE mara-matnr,                  "Start of material
      wa_matnr_t  LIKE mara-matnr,                  "End of material
      wa_werks_f  LIKE t001w-werks,                 "Start of plant
      wa_werks_t  LIKE t001w-werks,                 "Ensssdd of plant
      wa_period_f LIKE s001-spmon,                  "Start of period
      wa_period_t LIKE s001-spmon,                  "End of period
      wa_budat_f  LIKE sy-datum,                    "Start of date
      wa_budat_t  LIKE sy-datum.                    "End of date

*----- Define variable for ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_top_of_page_detail TYPE slis_t_listheader,
       w_status_flg(6).
*----- Constants
CONSTANTS : c_formname_top_of_page TYPE slis_formname
                                        VALUE 'TOP_OF_PAGE'.

*----- Macro
DEFINE append_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : cur field      &8 : quantity field   &9 : just

  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-cfieldname = &7.
  w_fieldcat-qfieldname = &8.
  w_fieldcat-just       = &9.
  append w_fieldcat.
  clear : w_fieldcat.

END-OF-DEFINITION.

*------ Select-options
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_werks FOR marc-werks OBLIGATORY NO-EXTENSION
                        DEFAULT 'P001'.
SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr.     "Vendor
PARAMETERS:     p_ekorg LIKE eine-ekorg DEFAULT 'PU01'.  "Pur. Org.
PARAMETERS:     p_mtart LIKE mara-mtart DEFAULT 'ROH'.  "Material Type
SELECT-OPTIONS: s_ekgrp FOR marc-ekgrp.  "Purchase Group
SELECT-OPTIONS: s_matnr FOR mara-matnr . "Material Number
SELECTION-SCREEN END OF BLOCK block1.

*----- Screen Attribute Control
AT SELECTION-SCREEN OUTPUT.
  PERFORM set_screen_attribute.

*----- Get Data
AT SELECTION-SCREEN.
  CHECK sy-ucomm = 'ONLI'.
  PERFORM check_rtn.
  PERFORM get_data.

*----- Top-of-page
TOP-OF-PAGE.
  PERFORM top_of_page.

START-OF-SELECTION.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM check_werks.
  PERFORM check_matnr.
  PERFORM check_period.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM get_material.
  PERFORM get_info_record.
  PERFORM get_gr_data.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  check_werks
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_werks.
  IF      s_werks-low EQ ' ' AND s_werks-high EQ ' '.
    wa_werks_t = 'ZZZZ'.
  ELSEIF  s_werks-low EQ ' ' AND s_werks-high NE ' '.
    wa_werks_t = s_werks-high.
  ELSEIF  s_werks-low NE ' ' AND s_werks-high EQ ' '.
    wa_werks_t = wa_werks_f = s_werks-low.
  ELSEIF  s_werks-low NE ' ' AND s_werks-high NE ' '.
    wa_werks_f = s_werks-low. wa_werks_t = s_werks-high.
  ENDIF.
ENDFORM.                    " check_werks
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr.
  IF     s_matnr-low EQ ' ' AND s_matnr-high EQ ' '.
    wa_matnr_t = 'ZZZZZZZZZZZZZZZZZZ'.
  ELSEIF s_matnr-low NE ' ' AND s_matnr-high EQ ' '.
    wa_matnr_f = wa_matnr_t = s_matnr-low.
  ELSEIF s_matnr-low EQ ' ' AND s_matnr-high NE ' '.
    wa_matnr_t = s_matnr-high.
  ELSEIF s_matnr-low NE ' ' AND s_matnr-high NE ' '.
    wa_matnr_f = s_matnr-low. wa_matnr_t = s_matnr-high.
  ENDIF.

  SELECT SINGLE *
    FROM mara
   WHERE matnr IN s_matnr
     AND mtart IN ('ROH','ROH1')
     AND profl IN ('K','V')
     AND lvorm <> 'X'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.
ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  check_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period.
*  CASE s_budat-low+4(2).
*    WHEN '01' OR '02' OR '03'.
*      CONCATENATE s_budat-low(4) '01' INTO wa_period_f.
*    WHEN '04' OR '05' OR '06'.
*      CONCATENATE s_budat-low(4) '02' INTO wa_period_f.
*    WHEN '07' OR '08' OR '09'.
*      CONCATENATE s_budat-low(4) '03' INTO wa_period_f.
*    WHEN '10' OR '11' OR '12'.
*      CONCATENATE s_budat-low(4) '04' INTO wa_period_f.
*  ENDCASE.
*
*  CASE s_budat-high+4(2).
*    WHEN '01' OR '02' OR '03'.
*      CONCATENATE s_budat-high(4) '01' INTO wa_period_f.
*    WHEN '04' OR '05' OR '06'.
*      CONCATENATE s_budat-high(4) '02' INTO wa_period_f.
*    WHEN '07' OR '08' OR '09'.
*      CONCATENATE s_budat-high(4) '03' INTO wa_period_f.
*    WHEN '10' OR '11' OR '12'.
*      CONCATENATE s_budat-high(4) '04' INTO wa_period_f.
*  ENDCASE.
ENDFORM.                    " check_period
*&---------------------------------------------------------------------*
*&      Form  APPEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_data.

ENDFORM.                    " APPEND_DATA
*&---------------------------------------------------------------------*
*&      Form  APPEND_IT_PRICE_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_it_price_info.
*----- Read the lastest Value key
*  CLEAR: wa_datab.
*  SELECT SINGLE knumh datab
*    INTO (wa_knumh, wa_datab)
*    FROM a018
*   WHERE kappl = 'M'
*     AND kschl = 'PB00'
*     AND lifnr = wa_master-lifnr
*     AND matnr = wa_master-matnr
*     AND ekorg = wa_master-ekorg
*     AND esokz = '0'
*     AND datab <= p_datum
*     AND datbi >=  p_datum.


ENDFORM.                    " APPEND_IT_PRICE_INFO
*&---------------------------------------------------------------------*
*&      Form  get_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_record.
  RANGES: lr_lifnr FOR lfa1-lifnr.

*----- Read Info Record
  CLEAR: it_knumh, it_knumh[].

  SELECT matnr lifnr knumh datab datbi
    INTO TABLE it_knumh
    FROM a018
     FOR ALL ENTRIES IN it_matnr
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  it_matnr-matnr
     AND lifnr IN s_lifnr
     AND ekorg =  p_ekorg
     AND esokz =  '0'
     AND datab <= sy-datum
     AND datbi >= sy-datum.

*----- Check Undetermined price
  RANGES: lr_knumh FOR a018-knumh.
  SELECT a~knumh AS low
    INTO CORRESPONDING FIELDS OF TABLE lr_knumh
    FROM konh AS a INNER JOIN konp AS b
      ON a~knumh     =    b~knumh
         FOR ALL ENTRIES IN it_knumh
   WHERE a~knumh     =    it_knumh-knumh
     AND a~kzust     LIKE 'X%'
     AND a~kschl     =    'PB00'
     AND b~kschl     =    'PB00'
     AND b~loevm_ko  =    ' '.

  lr_knumh-sign = 'I'. lr_knumh-option = 'EQ'.
  MODIFY lr_knumh TRANSPORTING sign option WHERE sign >= ''.
  DELETE it_knumh WHERE NOT knumh IN lr_knumh.

*----- Check Purchase Group
  LOOP AT it_knumh.
    SELECT SINGLE a~matnr b~ekgrp c~eknam a~umren a~umrez
      INTO (eina-matnr,     it_knumh-ekgrp, it_knumh-eknam,
            it_knumh-umren, it_knumh-umrez)
      FROM eina AS a INNER JOIN eine AS b
                        ON a~infnr = b~infnr
                     INNER JOIN t024 AS c
                        ON b~ekgrp = c~ekgrp
     WHERE a~matnr = it_knumh-matnr
       AND a~lifnr = it_knumh-lifnr
       AND a~loekz = ' '
       AND b~ekorg = p_ekorg
       AND b~ekgrp IN s_ekgrp
       AND b~werks = ' '
       AND b~esokz = '0'
       AND b~loekz = ' '.
    IF sy-subrc NE 0.
      DELETE it_knumh.
      CONTINUE.
    ENDIF.

    MOVE: it_knumh-lifnr TO it_knumh-vakey(10),
          it_knumh-matnr TO it_knumh-vakey+10(18),
          p_ekorg        TO it_knumh-vakey+28(4),
          '0'            TO it_knumh-vakey+32.
    MODIFY it_knumh.
  ENDLOOP.

  READ TABLE it_knumh INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

*----- Read undetermined price history
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_info
    FROM konh AS a INNER JOIN konp AS b
                      ON a~knumh = b~knumh
         FOR ALL ENTRIES IN it_knumh
   WHERE a~kzust     LIKE 'X%'
     AND a~vakey     =    it_knumh-vakey
     AND a~kschl     =    'PB00'
     AND b~kschl     =    'PB00'
     AND b~loevm_ko  =    ' '
     AND a~datab     <=   it_knumh-datab.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  DATA: lw_datbi LIKE sy-datum.
  LOOP AT it_info.
    MOVE: it_info-vakey(10)    TO it_info-lifnr,
          it_info-vakey+10(18) TO it_info-matnr.

    READ TABLE it_knumh WITH KEY vakey = it_info-vakey.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.

    MOVE: it_knumh-umren TO it_info-umren,
          it_knumh-umrez TO it_info-umrez.
    MODIFY it_info.
  ENDLOOP.

  SORT it_info BY matnr lifnr datab DESCENDING.
  LOOP AT it_info.
    it_info-datbi = it_info-datab - 1.

    MODIFY it_info TRANSPORTING datbi WHERE matnr = it_info-matnr
                                        AND lifnr = it_info-lifnr
                                        AND datab < it_info-datab.
  ENDLOOP.

*----- Set GR material, vendor, period
  DATA: lw_info LIKE it_info.
  CLEAR: it_gr_matnr, it_gr_matnr[].
  SORT it_info BY matnr lifnr datab datbi.
  LOOP AT it_info.
    MOVE: it_info TO lw_info.

    AT NEW lifnr.
      MOVE: lw_info-matnr TO it_gr_matnr-matnr,
            lw_info-lifnr TO it_gr_matnr-lifnr,
            lw_info-datab TO it_gr_matnr-datab.
      APPEND it_gr_matnr.
    ENDAT.

    AT END OF lifnr.
      READ TABLE it_gr_matnr WITH KEY matnr = it_info-matnr
                                      lifnr = it_info-lifnr.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m02.
      ENDIF.

      MOVE: lw_info-datbi TO it_gr_matnr-datbi.
      MODIFY it_gr_matnr INDEX sy-tabix.
    ENDAT.

    lr_lifnr-sign = 'I'. lr_lifnr-option = 'EQ'.
    MOVE: it_info-lifnr TO lr_lifnr-low.
    COLLECT lr_lifnr.
  ENDLOOP.

*----- Read vendor name
  SELECT lifnr name1
    INTO TABLE it_lifnr
    FROM lfa1
   WHERE lifnr IN lr_lifnr.
ENDFORM.                    " get_info_record
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material.
*----- Read Material
  SELECT a~matnr b~maktx
    INTO CORRESPONDING FIELDS OF TABLE it_matnr
    FROM mara AS a INNER JOIN makt AS b
      ON a~matnr = b~matnr
   WHERE a~matnr IN s_matnr
     AND a~mtart = p_mtart
     AND a~profl IN ('K','V')
     AND a~lvorm <> 'X'
     AND b~spras =  sy-langu.
ENDFORM.                    " GET_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  GET_GR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_gr_data.
  PERFORM save_material_data.

  EXEC SQL PERFORMING APPEND_GR_DATA.
    SELECT B.MATNR, B.LIFNR, B.ZBUDAT,
           DECODE(B.BWART,'101',B.MENGE,
                          '102',B.MENGE * -1,
                          '122',B.MENGE * -1,
                          '511',B.MENGE,
                          '512',B.MENGE * -1),
           B.MEINS, B.WAERS, C.WRBTR, C.PEINH,
           B.WERKS, C.WAERS
     INTO :WA_GR-MATNR, :WA_GR-LIFNR, :WA_GR-BUDAT, :WA_GR-MENGE,
          :WA_GR-MEINS, :WA_GR-WAERS, :WA_GR-WRBTR, :WA_GR-PEINH,
          :WA_GR-WERKS, :WA_GR-WAERS_S
     FROM ZTMM_RMMPM29_TMP A, MSEG B, ZTMM_ANALY C
    WHERE A.MANDT = :SY-MANDT
      AND A.ERDAT = :SY-DATUM
      AND A.ERZET = :SY-UZEIT
      AND B.MANDT = A.MANDT
      AND B.WERKS BETWEEN :WA_WERKS_F AND :WA_WERKS_T
      AND B.MATNR = A.MATNR
      AND B.LIFNR = A.LIFNR
      AND B.ZBUDAT BETWEEN A.DATAB AND A.DATBI
      AND B.BWART IN ('101','102','122','511','512')
*      AND B.BWART = '101'
*      AND NOT EXISTS (SELECT *
*                        FROM MSEG
*                       WHERE MANDT = A.MANDT
*                         AND BWART in ('102','122')
*                          AND SJAHR = B.MJAHR
*                          AND SMBLN = B.MBLNR
*                          AND SMBLP = B.ZEILE)
**                         AND LFBJA = B.MJAHR
**                         AND LFBNR = B.MBLNR
**                         AND LFPOS = B.ZEILE)
      AND C.MANDT(+) = B.MANDT
      AND C.PERIOD(+) = CONCAT(SUBSTR(B.ZBUDAT,1,4),
                               DECODE(SUBSTR(B.ZBUDAT,5,2),
                               '01','01','02','01','03','01',
                               '04','02','05','02','06','02',
                               '07','03','08','03','09','03',
                               '10','04','11','04','12','04'))
      AND C.WERKS(+)  = B.WERKS
      AND C.MATNR(+)  = B.MATNR
*     UNION
*     SELECT B.MATNR, B.LIFNR, B.ZBUDAT, B.MENGE,
*           B.MEINS,  B.WAERS, C.WRBTR,  C.PEINH,
*           B.WERKS,  C.WAERS
*      FROM ZTMM_RMMPM29_TMP A, MSEG B, ZTMM_ANALY C
*     WHERE A.MANDT = :SY-MANDT
*       AND A.ERDAT = :SY-DATUM
*       AND A.ERZET = :SY-UZEIT
*       AND B.MANDT = A.MANDT
*       AND B.WERKS BETWEEN :WA_WERKS_F AND :WA_WERKS_T
*       AND B.MATNR = A.MATNR
*       AND B.LIFNR = A.LIFNR
*       AND B.ZBUDAT BETWEEN A.DATAB AND A.DATBI
*       AND B.BWART = '511'
*       AND NOT EXISTS (SELECT *
*                         FROM MSEG
*                        WHERE MANDT = A.MANDT
*                          AND BWART = '512'
*                          AND SJAHR = B.MJAHR
*                          AND SMBLN = B.MBLNR
*                          AND SMBLP = B.ZEILE)
*       AND C.MANDT(+) = B.MANDT
*       AND C.PERIOD(+) = CONCAT(SUBSTR(B.ZBUDAT,1,4),
*                                DECODE(SUBSTR(B.ZBUDAT,5,2),
*                                '01','01','02','01','03','01',
*                                '04','02','05','02','06','02',
*                                '07','03','08','03','09','03',
*                                '10','04','11','04','12','04'))
*       AND C.WERKS(+)  = B.WERKS
*       AND C.MATNR(+)  = B.MATNR
  ENDEXEC.

  PERFORM delete_material_data.

  READ TABLE it_gr INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  SORT it_gr BY matnr lifnr datab.
ENDFORM.                    " GET_GR_DATA
*&---------------------------------------------------------------------*
*&      Form  save_material_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_material_data.
  DATA: lt_ztmm_rmmpm29_tmp LIKE ztmm_rmmpm29_tmp
                            OCCURS 0 WITH HEADER LINE.
  LOOP AT it_gr_matnr.
    MOVE: sy-datum TO lt_ztmm_rmmpm29_tmp-erdat,
          sy-uzeit TO lt_ztmm_rmmpm29_tmp-erzet.
    MOVE-CORRESPONDING it_gr_matnr TO lt_ztmm_rmmpm29_tmp.
    APPEND lt_ztmm_rmmpm29_tmp.
  ENDLOOP.

  INSERT ztmm_rmmpm29_tmp FROM TABLE lt_ztmm_rmmpm29_tmp
                          ACCEPTING DUPLICATE KEYS.
ENDFORM.                    " save_material_data
*&---------------------------------------------------------------------*
*&      Form  delete_material_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_material_data.
  DELETE FROM ztmm_rmmpm29_tmp WHERE erdat = sy-datum
                                 AND erzet = sy-uzeit.
ENDFORM.                    " delete_material_data
*&---------------------------------------------------------------------*
*&      Form  APPEND_GR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_gr_data.
  LOOP AT it_info WHERE matnr = wa_gr-matnr
                    AND lifnr = wa_gr-lifnr
                    AND datab <= wa_gr-budat
                    AND datbi >= wa_gr-budat.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  READ TABLE it_matnr WITH KEY matnr = wa_gr-matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  READ TABLE it_lifnr WITH KEY lifnr = wa_gr-lifnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  READ TABLE it_knumh WITH KEY matnr = wa_gr-matnr
                               lifnr = wa_gr-lifnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  CLEAR: it_gr.
  READ TABLE it_gr WITH KEY matnr = wa_gr-matnr
                            lifnr = wa_gr-lifnr
                            datab = it_info-datab.
  IF sy-subrc NE 0.
    MOVE: wa_gr-matnr    TO it_gr-matnr,
          it_matnr-maktx TO it_gr-maktx,
*          wa_gr-werks    TO it_gr-werks,
          wa_gr-wrbtr    TO it_gr-netpr_s,
          wa_gr-peinh    TO it_gr-peinh_s,
          wa_gr-lifnr    TO it_gr-lifnr,
          it_lifnr-name1 TO it_gr-name1,
          wa_gr-menge    TO it_gr-menge,
          wa_gr-meins    TO it_gr-meins,
          it_knumh-ekgrp TO it_gr-ekgrp,
          it_knumh-eknam TO it_gr-eknam,
          it_info-datab  TO it_gr-datab,
          it_info-erdat  TO it_gr-erdat,
          wa_gr-waers    TO it_gr-waers,
          it_info-kzust  TO it_gr-kzust,
          wa_gr-waers_s  TO it_gr-waers_s.

    DATA: lw_netpr TYPE f,
          lw_peinh LIKE ekpo-peinh.

*    if wa_gr-peinh is initial.
    IF it_info-kmein EQ wa_gr-meins.
      lw_netpr = ( it_info-kbetr / it_info-kpein ).
    ELSE.
      lw_netpr = ( it_info-kbetr / it_info-kpein ) /
                 ( it_info-kumne * it_info-kumza ) /
                 ( it_info-umrez * it_info-umren ).
    ENDIF.

    PERFORM get_price_unit USING lw_netpr lw_peinh.

    MOVE: lw_netpr TO it_gr-netpr_i,
          lw_peinh TO it_gr-peinh_i.
*    else.
*    endif.
*          it_info-kbetr  TO it_gr-netpr_i,
*          it_info-kpein  TO it_gr-peinh_i,
    it_gr-wrbtr = it_gr-menge * it_gr-netpr_i / it_gr-peinh_i.
    APPEND it_gr.
  ELSE.
    it_gr-menge = it_gr-menge + wa_gr-menge.
    it_gr-wrbtr = it_gr-menge * it_gr-netpr_i / it_gr-peinh_i.
    MODIFY it_gr INDEX sy-tabix.
  ENDIF.

  CLEAR: wa_gr.
ENDFORM.                    " APPEND_GR_DATA
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = w_top_of_page.
ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM build_fieldcat.
  PERFORM build_event.
  PERFORM build_sort.
  PERFORM comment_build USING  w_top_of_page[].
  PERFORM alv_function.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : cur field      &8 : no zero          &9 : just

  append_fieldcat :
    w_col_pos 'MATNR'   18 text-001 'CHAR'  'X'  ''        '' '',
    w_col_pos 'MAKTX'   20 text-002 'CHAR'  ''  ''        '' '',
    w_col_pos 'WAERS_S' 05 text-003 'CUKY'  ''  ''        '' '',
    w_col_pos 'NETPR_S' 14 text-004 'CURR'  ''  'WAERS_S' '' '',
    w_col_pos 'PEINH_S' 05 text-005 'DEC'   ''  ''        '' '',
    w_col_pos 'WAERS'   05 text-018 'CUKY'  ''  ''        '' '',
    w_col_pos 'NETPR_I' 14 text-008 'CURR'  ''  'WAERS'   '' '',
    w_col_pos 'PEINH_I' 05 text-009 'DEC'   ''  ''        '' '',
    w_col_pos 'WRBTR'   16 text-010 'CURR'  ''  'WAERS'   '' '',
    w_col_pos 'MENGE'   17 text-011 'QUAN'  ''  ''        'MEINS' '',
    w_col_pos 'MEINS'   03 text-012 'UNIT'  ''  ''        '' '',
    w_col_pos 'LIFNR'   10 text-006 'CHAR'  ''  ''        '' '',
    w_col_pos 'NAME1'   20 text-007 'CHAR'  ''  ''        '' '',
    w_col_pos 'KZUST'   04 text-013 'CHAR'  ''  ''        '' '',
    w_col_pos 'EKGRP'   03 text-014 'CHAR'  ''  ''        '' '',
    w_col_pos 'EKNAM'   18 text-015 'CHAR'  ''  ''        '' '',
    w_col_pos 'DATAB'   10 text-016 'DATS'  ''  ''        '' '',
    w_col_pos 'ERDAT'   10 text-017 'DATS'  ''  ''        '' ''.
ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_function.
  DATA:   l_print_p TYPE slis_print_alv.  " print setting

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

*** print paramter   ****************************************
  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.
*************************************************************

*  W_STATUS_FLG = 'BASE'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer       = 'X'
            i_callback_program       = w_program
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_user_command  = 'USER_COMMAND'
            it_fieldcat              = w_fieldcat[]
            it_sort                  = w_sortcat[]
            i_save                   = 'A'
            it_events                = w_eventcat[]
            is_print                 = l_print_p
       TABLES
            t_outtab                 = it_gr
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " alv_function
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING  lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader.

*----- Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-h01.
  APPEND ls_line TO lt_top_of_page.

*----- Plant
  ls_line-typ  = 'S'.
  ls_line-key  = text-h02.
  CONCATENATE s_werks-low '~' s_werks-high INTO ls_line-info
              SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

*----- Purchase Organization
  ls_line-typ  = 'S'.
  ls_line-key  = text-h03.
  MOVE: p_ekorg TO ls_line-info.
  APPEND ls_line TO lt_top_of_page.

*----- Material Type
  ls_line-typ  = 'S'.
  ls_line-key  = text-h04.
  MOVE: p_mtart TO ls_line-info.
  APPEND ls_line TO lt_top_of_page.

*----- Purchase group
  ls_line-typ  = 'S'.
  ls_line-key  = text-h05.
  CONCATENATE s_ekgrp-low '~' s_ekgrp-high INTO ls_line-info
              SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

*----- Vendor
  ls_line-typ  = 'S'.
  ls_line-key  = text-h06.
  CONCATENATE s_lifnr-low '~' s_lifnr-high INTO ls_line-info
              SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

*----- Material
  ls_line-typ  = 'S'.
  ls_line-key  = text-h07.
  CONCATENATE s_matnr-low '~' s_matnr-high INTO ls_line-info
              SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.


ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  build_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort.

ENDFORM.                    " build_sort
*&---------------------------------------------------------------------*
*&      Form  build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_event.
  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'TOP_OF_PAGE'.

  APPEND w_eventcat.
ENDFORM.                    " build_event
*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'BASE'.
ENDFORM.                    "
*&---------------------------------------------------------------------*
*&      Form  set_screen_attribute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_attribute.
  LOOP AT SCREEN.
    IF screen-name = 'P_EKORG' OR screen-name = 'P_MTART'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " set_screen_attribute
*&---------------------------------------------------------------------*
*&      Form  get_price_unit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_NETPR  text
*      -->P_LW_PEINH  text
*----------------------------------------------------------------------*
FORM get_price_unit USING pw_netpr pw_peinh.
  CALL FUNCTION 'Z_FCA_GET_PRICE_UNIT'
       EXPORTING
            i_amount = pw_netpr
       IMPORTING
            e_amount = pw_netpr
            e_peinh  = pw_peinh.
ENDFORM.                    " get_price_unit
