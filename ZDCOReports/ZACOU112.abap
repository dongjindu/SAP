*----------------------------------------------------------------------
* Program ID        : ZACOU112
* Title             : [CO] Display Price History
* Created on        : 09/11/2006
* Created by        : Andy Choi
* Specifications By : Andy Choi
* Description       : Display Price History : Info-record(PB00, ZTIR)
* 06/17/2013  T00303       UD1K957385      U1: Apply Archiving
*----------------------------------------------------------------------
REPORT zacou112 NO STANDARD PAGE HEADING.

TABLES: mara,                " General Material Data
        mbew,                " Material Valuation
        makt,
        konh, konp,
        eina, eine.

* Internal tables
DATA: BEGIN OF itab OCCURS 0,
        knumh TYPE knumh,
        matnr TYPE matnr,
        kschl TYPE kscha,
        maktg TYPE maktg,
        lifnr TYPE lifnr,
        name1 TYPE name1,
        mtart TYPE mtart,
        matkl TYPE matkl,
        profl TYPE profl,
        ekgrp TYPE ekgrp,
        datab TYPE datab,
        datbi TYPE datbi,
        kzust TYPE kzust,
        konwa TYPE konwa,
        kbetr TYPE kbetr,
        kpein TYPE kpein,
        kmein TYPE kmein,
        kosrt TYPE kosrt,
        ernam TYPE ernam,
        erdat TYPE erdat,
* UD1K941202 - by IG.MOON 8/3/2007 {
        urzdt TYPE urzdt,
        loevm_ko TYPE loevm_ko,
* }
      END OF itab.

DATA: BEGIN OF it_tab OCCURS 0,
        matnr  TYPE matnr,
        maktg  TYPE maktg,
        lifnr1 TYPE lifnr,
        lifnr2 TYPE lifnr,
        name1  TYPE name1,
        mtart  TYPE mtart,
        profl  TYPE profl,
        ekgrp  TYPE ekgrp,
        matkl  LIKE eina-matkl,
        datab  TYPE datab,
        datbi  TYPE datbi,
        kzust  TYPE kzust,
        konwa  TYPE konwa,
        kbetr1 TYPE kbetr,
        kbetr2 TYPE kbetr,
        tkbetr TYPE kbetr,
        kpein  TYPE kpein,
        kmein  TYPE kmein,
* UD1K941202 - by IG.MOON 8/3/2007 {
* BEGIN OF UD1K951202
        fra1   TYPE konp-kbetr,
        zoa1   TYPE konp-kbetr,
        zoth   TYPE konp-kbetr,
        zoti   TYPE konp-kbetr,

*       fra1   TYPE zfra1,
*       zoa1   TYPE zoth,
*       zoth   TYPE zoth,
*       zoti   TYPE zoti,
* END OF UD1K951202
* }
        zp12   TYPE zzp12,
        zp13   TYPE zzp13,
        zp16   TYPE zzp16,
        zp17   TYPE zzp17,
        zp18   TYPE zzp18,
        kosrt  TYPE kosrt,
        ernam  TYPE ernam,
        erdat  TYPE erdat,
* UD1K941202 - by IG.MOON 8/3/2007 {
        urzdt  TYPE urzdt,
        loevm_ko   LIKE konp-loevm_ko,
* }
      END OF it_tab.

DATA: it_disp LIKE it_tab OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_knumh OCCURS 0,
        kschl LIKE konh-kschl,
        lifnr LIKE lfa1-lifnr,
        matnr LIKE mara-matnr,
        datab LIKE konh-datab,
        datbi LIKE konh-datbi,
        knumh LIKE konh-knumh,
        chk(1) TYPE c,
      END   OF it_knumh.

DATA: BEGIN OF gt_eina OCCURS 0,
        matnr LIKE mara-matnr,
        lifnr LIKE lfa1-lifnr,
        name1 LIKE lfa1-name1,
        ekgrp LIKE ekko-ekgrp,
* UD1K941202 - by IG.MOON 8/3/2007 {
        urzdt LIKE eina-urzdt,
* }
      END OF gt_eina.

DATA: BEGIN OF it_info_condi OCCURS 0,
          knumh      LIKE konh-knumh,
          kopos      LIKE konp-kopos,
          kschl      LIKE konp-kschl,
          kschl_konh LIKE konh-kschl,
          vakey      LIKE konh-vakey,
          datab      LIKE konh-datab,
          datbi      LIKE konh-datbi,
          kzust      LIKE konh-kzust,
          kbetr      LIKE konp-kbetr,
          konwa      LIKE konp-konwa,
          kpein      LIKE konp-kpein,
          kmein      LIKE konp-kmein,
          kumza      LIKE konp-kumza,
          kumne      LIKE konp-kumne,
          meins      LIKE konp-meins,
          loevm_ko   LIKE konp-loevm_ko,
          lifnr      LIKE konp-lifnr,
          kosrt      LIKE konh-kosrt,
          ernam      LIKE konh-ernam,
          erdat      LIKE konh-erdat,
       END OF it_info_condi.

DATA: gt_ztir LIKE it_info_condi OCCURS 0 WITH HEADER LINE,
      gt_info LIKE it_info_condi OCCURS 0 WITH HEADER LINE.

* Info Condition
DATA: BEGIN OF it_info OCCURS 0,
        matnr LIKE mara-matnr,
        lifnr LIKE lfa1-lifnr,
        name1 LIKE lfa1-name1,
        datab LIKE a018-datab,
        datbi LIKE a018-datbi,
        kzust LIKE konh-kzust,
        konwa LIKE konp-konwa,
        kbetr LIKE konp-kbetr,
        kpein LIKE konp-kpein,
        kmein LIKE konp-kmein,
      END   OF it_info.

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr LIKE mara-matnr,
        maktg LIKE makt-maktg,
        mtart LIKE mara-mtart,
        profl LIKE mara-profl,
        lifnr LIKE eina-lifnr,
        matkl LIKE eina-matkl,
      END   OF it_matnr.

* ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.

* Working area
DATA: w_datum_f   LIKE   sy-datum,
      w_datum_t   LIKE   sy-datum.

*- U1 start
DATA: gt_konh_a TYPE TABLE OF konh WITH HEADER LINE,
      gt_konp_a TYPE TABLE OF konp WITH HEADER LINE.
DATA: BEGIN OF gt_info_condi_a OCCURS 0,
          knumh      LIKE konh-knumh,
          kopos      LIKE konp-kopos,
          kschl      LIKE konp-kschl,
          kschl_konh LIKE konh-kschl,
          vakey      LIKE konh-vakey,
          datab      LIKE konh-datab,
          datbi      LIKE konh-datbi,
          kzust      LIKE konh-kzust,
          kbetr      LIKE konp-kbetr,
          konwa      LIKE konp-konwa,
          kpein      LIKE konp-kpein,
          kmein      LIKE konp-kmein,
          kumza      LIKE konp-kumza,
          kumne      LIKE konp-kumne,
          meins      LIKE konp-meins,
          loevm_ko   LIKE konp-loevm_ko,
          lifnr      LIKE konp-lifnr,
          kosrt      LIKE konh-kosrt,
          ernam      LIKE konh-ernam,
          erdat      LIKE konh-erdat,
       END OF gt_info_condi_a.
*- U1 End

CONSTANTS: c_check                  VALUE 'X',
           c_ekorg LIKE ekko-ekorg  VALUE 'PU01',"Purchase Org.
           c_kschl LIKE konp-kschl  VALUE 'PB00',"Type of amount
           c_kschl2 LIKE konp-kschl VALUE 'ZTIR',"Type of amount
           c_frght LIKE konp-kschl  VALUE 'FRA1',"Type of freight
           c_duty  LIKE konp-kschl  VALUE 'ZOA1',"Type of duty
           c_con01 LIKE konp-kschl  VALUE 'ZOTH',"Type of ETC rate
           c_con02 LIKE konp-kschl  VALUE 'ZOTI'."Type of ETC rate

* Selection screens
SELECT-OPTIONS : s_matnr FOR mara-matnr  MEMORY ID mat,
                 s_maktg FOR makt-maktg,
                 s_date  FOR sy-datum NO-EXTENSION. "default sy-datum.
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS : s_mtart FOR mara-mtart  MEMORY ID mta,
                 s_bklas FOR mbew-bklas,
                 s_profl FOR mara-profl NO INTERVALS,
                 s_lifnr FOR eina-lifnr,
                 s_matkl FOR eina-matkl,
                 s_ekgrp FOR eine-ekgrp,
                 s_kosrt FOR konh-kosrt,
                 s_kzust FOR konh-kzust,
                 s_erdat FOR konh-erdat,
                 s_ernam FOR konh-ernam,
                 s_loevm FOR konp-loevm_ko DEFAULT ' '.

PARAMETERS: p_incl AS CHECKBOX DEFAULT 'X'.  "include import condition
*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

SELECTION-SCREEN END OF BLOCK bl1.

START-OF-SELECTION.
  PERFORM select_materials.
  PERFORM check_datum.
  PERFORM read_inforecord.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  read_inforecord
*&---------------------------------------------------------------------*
FORM read_inforecord.
  CLEAR: it_knumh, it_knumh[].
  SELECT kschl lifnr matnr datab datbi knumh
    INTO CORRESPONDING FIELDS OF TABLE it_knumh
    FROM a018
     FOR ALL ENTRIES IN it_matnr
   WHERE kappl =  'M'
     AND kschl =  'PB00'     "ZTIR = PB00
     AND matnr = it_matnr-matnr
     AND lifnr = it_matnr-lifnr
*    AND LIFNR IN S_LIFNR
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datbi >= w_datum_f
     AND datab <= w_datum_t.

  CHECK sy-dbcnt > 0.

  CLEAR gt_eina.
  REFRESH gt_eina.

  SELECT matnr a~lifnr name1 ekgrp
         a~urzdt
    INTO TABLE gt_eina
    FROM eina AS a
   INNER JOIN eine AS b
      ON a~infnr =  b~infnr
   INNER JOIN lfa1 AS c
      ON a~lifnr = c~lifnr
    FOR ALL ENTRIES IN it_knumh
   WHERE a~matnr =  it_knumh-matnr
     AND a~lifnr =  it_knumh-lifnr
     AND a~loekz =  ' '
     AND b~werks =  ' '
     AND b~ekorg =  c_ekorg
     AND b~ekgrp IN s_ekgrp
     AND b~loekz =  ' '.

  CHECK sy-dbcnt > 0.

  SORT gt_eina BY matnr lifnr.
  DATA: l_idx LIKE sy-tabix.
  LOOP AT it_knumh.
    l_idx = sy-tabix.
    READ TABLE gt_eina WITH KEY matnr = it_knumh-matnr
                                lifnr = it_knumh-lifnr.
    IF sy-subrc <> 0.
      it_knumh-chk = 'D'.
      MODIFY it_knumh INDEX l_idx.
    ENDIF.
  ENDLOOP.
  SORT it_knumh BY chk.
  DELETE it_knumh WHERE chk = 'D'.
  DESCRIBE TABLE it_knumh LINES sy-tabix.
  CHECK sy-tabix > 0.

  SORT it_knumh BY knumh.
  DELETE ADJACENT DUPLICATES FROM gt_eina.
  SORT gt_eina BY matnr lifnr.

  DESCRIBE TABLE it_knumh LINES sy-tabix.
  CHECK sy-tabix > 0.

*   mandatory
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_info_condi
   FROM konh AS h
      INNER JOIN konp AS p
         ON h~knumh = p~knumh
     FOR ALL ENTRIES IN it_knumh
   WHERE h~knumh  = it_knumh-knumh
     AND p~kschl  = c_kschl
     AND p~loevm_ko   IN s_loevm       "Deletion indicator???
     AND h~kosrt      IN s_kosrt
     AND h~kzust      IN s_kzust
     AND h~erdat      IN s_erdat
     AND h~ernam      IN s_ernam.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_konp TABLES it_info_condi USING c_kschl.
  ENDIF.
*- U1 End

*   option
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_ztir
   FROM konh AS h
      INNER JOIN konp AS p
         ON h~knumh = p~knumh
     FOR ALL ENTRIES IN it_knumh
   WHERE p~knumh    = it_knumh-knumh
     AND p~kschl  = c_kschl2
     AND p~loevm_ko   IN s_loevm       "Deletion indicator???
     AND h~kosrt      IN s_kosrt
     AND h~kzust      IN s_kzust
     AND h~erdat      IN s_erdat
     AND h~ernam      IN s_ernam.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_konp TABLES gt_ztir USING c_kschl2.
  ENDIF.
*- U1 End

*   option (for other conditions)
  IF NOT it_info_condi[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_info
      FROM konh AS h
        INNER JOIN konp AS p
           ON h~knumh = p~knumh
        FOR ALL ENTRIES IN it_info_condi
      WHERE p~knumh = it_info_condi-knumh
        AND ( p~kschl = 'ZP12' OR
              p~kschl = 'ZP13' OR
              p~kschl = 'ZP16' OR
              p~kschl = 'ZP17' OR
              p~kschl = 'ZP18' OR
* UD1K941202 - by IG.MOON 8/3/2007 {
              p~kschl = 'FRA1' OR
              p~kschl = 'ZOA1' OR
              p~kschl = 'ZOTH' OR
              p~kschl = 'ZOTI' )
* }
        AND p~loevm_ko   IN s_loevm.      "Deletion indicator???
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_konp_2.
    ENDIF.
*- U1 End
  ENDIF.

  SORT gt_info BY knumh.
*  ENDIF.
*
  CLEAR itab.
  REFRESH itab.

  LOOP AT it_info_condi.
    READ TABLE it_knumh WITH KEY knumh = it_info_condi-knumh.

    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    READ TABLE it_matnr WITH KEY matnr = it_knumh-matnr
                                 lifnr = it_knumh-lifnr
                        BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    READ TABLE gt_eina WITH KEY matnr = it_knumh-matnr
                                lifnr = it_knumh-lifnr
                       BINARY SEARCH.
    IF sy-subrc = 0.
      itab-name1 = gt_eina-name1.
      itab-ekgrp = gt_eina-ekgrp.
* UD1K941202 - by IG.MOON 8/3/2007 {
      itab-urzdt = gt_eina-urzdt.
* }

    ENDIF.

    itab-matkl = it_matnr-matkl.
    itab-mtart = it_matnr-mtart.
    itab-profl = it_matnr-profl.
    itab-maktg = it_matnr-maktg.

    itab-knumh = it_knumh-knumh.
    itab-matnr = it_knumh-matnr.
    itab-kschl = it_knumh-kschl.
    itab-lifnr = it_knumh-lifnr.
    itab-datab = it_knumh-datab.
    itab-datbi = it_knumh-datbi.

    itab-kbetr = it_info_condi-kbetr.
    itab-kzust = it_info_condi-kzust.
    itab-kpein = it_info_condi-kpein.
    itab-kmein = it_info_condi-kmein.
    itab-konwa = it_info_condi-konwa.
    itab-kbetr = it_info_condi-kbetr.
    itab-kosrt = it_info_condi-kosrt.
    itab-ernam = it_info_condi-ernam.
    itab-erdat = it_info_condi-erdat.
    itab-loevm_ko = it_info_condi-loevm_ko.

    APPEND itab.
    CLEAR itab.
  ENDLOOP.

  SORT gt_ztir BY knumh.

  LOOP AT itab.
    MOVE-CORRESPONDING itab TO it_disp.

    it_disp-lifnr1 = itab-lifnr.
    it_disp-kbetr1 = itab-kbetr.

    READ TABLE gt_ztir WITH KEY knumh = itab-knumh
           BINARY SEARCH.
    IF sy-subrc = 0.
      it_disp-lifnr2 = gt_ztir-lifnr.
      it_disp-kbetr2 = gt_ztir-kbetr.
    ENDIF.

    LOOP AT gt_info WHERE knumh = itab-knumh.
      CASE gt_info-kschl.
        WHEN 'ZP12'.
          it_disp-zp12 = gt_info-kbetr.
        WHEN 'ZP13'.
          it_disp-zp13 = gt_info-kbetr.
        WHEN 'ZP16'.
          it_disp-zp16 = gt_info-kbetr.
        WHEN 'ZP17'.
          it_disp-zp17 = gt_info-kbetr.
        WHEN 'ZP18'.
          it_disp-zp18 = gt_info-kbetr.
* UD1K941202 - by IG.MOON 8/3/2007 {
        WHEN 'FRA1'.
          it_disp-fra1 = gt_info-kbetr.
        WHEN 'ZOA1'.
          it_disp-zoa1 = gt_info-kbetr.
        WHEN 'ZOTH'.
          it_disp-zoth = gt_info-kbetr.
        WHEN 'ZOTI'.
          it_disp-zoti = gt_info-kbetr.
* }
      ENDCASE.
    ENDLOOP.

    it_disp-tkbetr = it_disp-kbetr1 + it_disp-kbetr2.
    COLLECT it_disp.
    CLEAR it_disp.
  ENDLOOP.

ENDFORM.                    " read_inforecord
*&---------------------------------------------------------------------*
*&      Form  select_materials
*&---------------------------------------------------------------------*
FORM select_materials.
  SELECT a~matnr a~mtart a~profl b~maktg c~lifnr c~matkl
    INTO CORRESPONDING FIELDS OF TABLE it_matnr
    FROM mara AS a
    INNER JOIN makt AS b
       ON a~matnr = b~matnr
    INNER JOIN eina AS c
       ON c~matnr =  b~matnr
   WHERE a~matnr IN s_matnr
     AND a~mtart IN s_mtart
     AND a~profl IN s_profl
     AND a~lvorm EQ space
     AND b~spras = sy-langu
     AND b~maktg IN s_maktg
     AND c~lifnr IN s_lifnr
     AND c~matkl IN s_matkl
     AND c~loekz = ''.       "not deleted!!!

  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  DESCRIBE TABLE s_bklas LINES sy-index.
  IF sy-index > 0.
    LOOP AT it_matnr.
      SELECT SINGLE * FROM mbew
         WHERE matnr = it_matnr-matnr
           AND bklas IN s_bklas.
      IF sy-subrc <> 0.
        DELETE it_matnr INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT it_matnr BY matnr lifnr.
ENDFORM.                    " select_materials
*&---------------------------------------------------------------------*
*&      Form  check_datum
*&---------------------------------------------------------------------*
FORM check_datum.
  IF     s_date-low EQ '00000000' AND s_date-high EQ '00000000'.
    MOVE: '99991231' TO w_datum_t.
  ELSEIF s_date-low NE '00000000' AND s_date-high EQ '00000000'.
    MOVE: s_date-low TO w_datum_f,
          s_date-low TO w_datum_t.
  ELSEIF s_date-low EQ '00000000' AND s_date-high NE '00000000'.
    MOVE: s_date-high TO w_datum_t.
  ELSEIF s_date-low NE '00000000' AND s_date-high NE '00000000'.
    MOVE: s_date-low  TO w_datum_f,
          s_date-high TO w_datum_t.
  ENDIF.
ENDFORM.                    " check_datum
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_data.

  PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
  'MATNR'     'Material'      '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'MAKTG'     'Mat.Desc.'     '25' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'LIFNR1'    'Vendor1'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'NAME1'     'VendName'      '20' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'LIFNR2'    'Vendor2'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'MTART'     'MTART'         '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'PROFL'     'PROFL'         '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'EKGRP'     'EKGRP'         '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'MATKL'     'MatGrp'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'DATAB'     'From'          '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'DATBI'     'To'            '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'KZUST'     'RSN'           '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'KONWA'     'CUR'           '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'KBETR1'    'Rate1'         '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'KBETR2'    'Rate2'         '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'TKBETR'    'Total Rate'    '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'KPEIN'     'per'           '03' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'KMEIN'     'unit'          '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'ZP12'     'NAFTA'          '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'ZP13'     'AALA Cost'      '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'ZP16'      'k-price'       '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'ZP17'      'k-seq'         '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'ZP18'      'k-duty'        '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
* UD1K941202 - by IG.MOON 8/3/2007 {
  'FRA1'      'FRA1'       '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'ZOA1'      'ZOA1'        '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'ZOTH'      'ZOTH'         '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'ZOTI'      'ZOTI'         '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
* }
  'KOSRT'     'Approval#'     '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'ERNAM'     'Created by'    '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'ERDAT'     'Created on'    '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
* UD1K941202 - by IG.MOON 8/3/2007 {
  'URZDT'     'Valid to'      '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'LOEVM_KO'  'Del'           '03' ' ' 'R'  ' '  ' '  '  ' ' '  ' '.
  PERFORM init_alv_parm.
* }

  g_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
      i_save             = 'A'
    TABLES
      t_outtab           = it_disp
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_parm.

  CLEAR   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_KSCHL  text
*----------------------------------------------------------------------*
FORM archive_read_konp TABLES pt_info_condi STRUCTURE gt_info_condi_a
                        USING p_kschl.

  TYPES: BEGIN OF ty_konp,
         knumh    TYPE knumh,
         kopos    TYPE kopos,
         kappl    TYPE kappl,
         kschl    TYPE kscha,
         loevm_ko TYPE loevm_ko,
         kosrt    TYPE kosrt,
         kzust    TYPE kzust,
         erdat    TYPE erdat,
         ernam    TYPE ernam,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_konp.

  DATA: l_handle    TYPE sytabix,
        lt_konh     TYPE TABLE OF konh WITH HEADER LINE,
        lt_konp     TYPE TABLE OF konp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_konp TYPE TABLE OF ty_konp,
        ls_inx_konp TYPE ty_konp.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZKONP_002'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_konp[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_konp
    FROM (l_gentab)
    FOR ALL ENTRIES IN it_knumh
   WHERE knumh  = it_knumh-knumh
     AND kschl  = p_kschl
     AND loevm_ko IN s_loevm       "Deletion indicator???
     AND kosrt    IN s_kosrt
     AND kzust    IN s_kzust
     AND erdat    IN s_erdat
     AND ernam    IN s_ernam.

  CHECK NOT lt_inx_konp[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_konh_a, gt_konh_a[], gt_konp_a, gt_konp_a[].
  LOOP AT lt_inx_konp INTO ls_inx_konp.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'MM_EKKO'
        archivkey                 = ls_inx_konp-archivekey
        offset                    = ls_inx_konp-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_konh, lt_konh[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'KONH'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_konh
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_konh[] IS INITIAL.

*  4.2 Read table from information
    CLEAR: lt_konp, lt_konp[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'KONP'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_konp
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_konp[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF: lt_konh INTO TABLE gt_konh_a,
                     lt_konp INTO TABLE gt_konp_a.
  ENDLOOP.

  SORT: gt_konh_a, gt_konp_a.
  DELETE ADJACENT DUPLICATES FROM: gt_konh_a COMPARING ALL FIELDS,
                                   gt_konp_a COMPARING ALL FIELDS.
  LOOP AT gt_konp_a.
    CLEAR pt_info_condi.
    MOVE-CORRESPONDING gt_konp_a TO pt_info_condi.

    CLEAR gt_konh_a.
    READ TABLE gt_konh_a WITH KEY knumh = gt_konp_a-knumh.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING gt_konh_a TO pt_info_condi.

    APPEND pt_info_condi.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_KONP_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_KONP_2 .

  TYPES: BEGIN OF ty_konp,
         knumh    TYPE knumh,
         kopos    TYPE kopos,
         kappl    TYPE kappl,
         kschl    TYPE kscha,
         loevm_ko TYPE loevm_ko,
         kosrt    TYPE kosrt,
         kzust    TYPE kzust,
         erdat    TYPE erdat,
         ernam    TYPE ernam,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_konp.

  DATA: l_handle    TYPE sytabix,
        lt_konh     TYPE TABLE OF konh WITH HEADER LINE,
        lt_konp     TYPE TABLE OF konp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_konp TYPE TABLE OF ty_konp,
        ls_inx_konp TYPE ty_konp.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZKONP_002'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_konp[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_konp
    FROM (l_gentab)
    FOR ALL ENTRIES IN it_info_condi
   WHERE knumh = it_info_condi-knumh
     AND ( kschl = 'ZP12' OR
           kschl = 'ZP13' OR
           kschl = 'ZP16' OR
           kschl = 'ZP17' OR
           kschl = 'ZP18' OR
* UD1K941202 - by IG.MOON 8/3/2007 {
           kschl = 'FRA1' OR
           kschl = 'ZOA1' OR
           kschl = 'ZOTH' OR
           kschl = 'ZOTI' )
* }
     AND loevm_ko   IN s_loevm.      "Deletion indicator???.

  CHECK NOT lt_inx_konp[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_konh_a, gt_konh_a[], gt_konp_a, gt_konp_a[].
  LOOP AT lt_inx_konp INTO ls_inx_konp.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'MM_EKKO'
        archivkey                 = ls_inx_konp-archivekey
        offset                    = ls_inx_konp-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_konh, lt_konh[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'KONH'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_konh
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_konh[] IS INITIAL.

*  4.2 Read table from information
    CLEAR: lt_konp, lt_konp[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'KONP'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_konp
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_konp[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF: lt_konh INTO TABLE gt_konh_a,
                     lt_konp INTO TABLE gt_konp_a.
  ENDLOOP.

  SORT: gt_konh_a, gt_konp_a.
  DELETE ADJACENT DUPLICATES FROM: gt_konh_a COMPARING ALL FIELDS,
                                   gt_konp_a COMPARING ALL FIELDS.
  LOOP AT gt_konp_a.
    CLEAR gt_info.
    MOVE-CORRESPONDING gt_konp_a TO gt_info.

    CLEAR gt_konh_a.
    READ TABLE gt_konh_a WITH KEY knumh = gt_konp_a-knumh.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING gt_konh_a TO gt_info.

    APPEND gt_info.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_KONP_2
