*----------------------------------------------------------------------
* Program ID        : ZFIR0062
* Title             : [FI] Asset Rollover Report
* Created on        : 01/04/2010
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : [FI] Asset Rollover Report
*----------------------------------------------------------------------
REPORT  zfir0062  MESSAGE-ID zmfi.

TABLES: t001,
        anla,
        anlz,
        anlb,
        anlc,
        anep,
        anlp,
        bkpf,
        tabw,
        t093u,
        pa0001,
        zfit0089,
        zfit0090,
        zfit0091.

* ALV GRID
TYPE-POOLS: slis.
DATA :  lt_fieldcat  TYPE slis_t_fieldcat_alv,
        lt_sort      TYPE slis_t_sortinfo_alv,
        lt_filter    TYPE slis_t_filter_alv,
        lt_header    TYPE slis_t_listheader,
        lt_excluding TYPE slis_t_extab,
        ls_fieldcat  TYPE slis_fieldcat_alv,
        ls_layout    TYPE slis_layout_alv,
        ls_header    TYPE slis_listheader,
        ls_excluding TYPE slis_extab,
        ls_variant   LIKE disvariant,
        ls_sort      TYPE slis_sortinfo_alv,
        ls_filter    TYPE slis_filter_alv,
        l_index      TYPE i,
        l_repid      LIKE sy-repid,
        w_int        TYPE i.

DATA: BEGIN OF itab OCCURS 0,
        check      TYPE c,
        bukrs      LIKE  anla-bukrs,
        anlkl      LIKE  anla-anlkl,         "
        txk20      LIKE  ankt-txk20,
        anln1      LIKE  anla-anln1,         "
        anln2      LIKE  anla-anln2,         "
        txt50      LIKE  anla-txt50,
        sernr      LIKE  anla-sernr,
        invnr      LIKE  anla-invnr,
        aibn1      LIKE  anla-aibn1,         " Origin
        posid      LIKE  prps-posid,
        posnr      LIKE  anla-posnr,         " WBS
        gsber      LIKE  anlz-gsber,         "
        kostl      LIKE  anlz-kostl,         "
        kostlv     LIKE  anlz-kostlv,        " Response CCtr
        werks      LIKE  anlz-werks,         " Plant
        stort      LIKE  anlz-stort,         " Location
        raumn      LIKE  anlz-raumn,         " Room
        aktiv      LIKE  anla-aktiv,         " Acq.date
        deakt      LIKE  anla-deakt,
        meins      LIKE  anla-meins,
        menge      LIKE  anla-menge,
        afasl      LIKE  anlb-afasl,         " Dep. Key
        ndjar      LIKE  anlb-ndjar,         " Useful life
        ndper      LIKE  anlb-ndper,         " Useful life
        afabg      LIKE  anlb-afabg,         " Dep. start date
        l_acq      LIKE  anlc-kansw,         " Acq.Cost as of last mm
        l_ad       LIKE  anlc-kansw,         " Acc.Dep as of last mm
        l_bal      LIKE  anlc-kansw,         " Balance as of last mm
        c_new      LIKE  anlc-kansw,         " New acq
        c_add      LIKE  anlc-kansw,         " Capital expenditure
        c_rin      LIKE  anlc-kansw,         " Reclass in
        c_rou      LIKE  anlc-kansw,         " Reclass out
        c_cip      LIKE  anlc-kansw,         " CIP in/out
        c_dis      LIKE  anlc-kansw,         " Disposal
        c_sal      LIKE  anlc-kansw,         " Sales
        c_acq      LIKE  anlc-kansw,         " sum(c_new ~ c_sal)
        c_dep      LIKE  anlc-kansw,         " Dep.Expense for curr mm
        c_adj      LIKE  anlc-kansw,         " Acc.Dep adj. for curr mm
        c_ad       LIKE  anlc-kansw,         " c_dep + c_adj
        e_acq      LIKE  anlc-kansw,         " Acq.Cost as of curr mm
        e_ad       LIKE  anlc-kansw,         " Acc.Dep as of curr mm
        e_bal      LIKE  anlc-kansw,         " Balance as of curr mm
      END OF itab.

DATA: BEGIN  OF  it_anlc OCCURS 0,
        anln1  LIKE anep-anln1,
        anln2  LIKE anep-anln2,
        afabe  LIKE anep-afabe,
        kansw  LIKE anlc-kansw,
        kmafa  LIKE anlc-kmafa,
        kaafa  LIKE anlc-kaafa,
        knafa  LIKE anlc-knafa,
        nafag  LIKE anlc-nafag,
        aafag  LIKE anlc-aafag,
      END  OF it_anlc.

DATA: BEGIN  OF  it_anep OCCURS 0,
        anln1  LIKE anep-anln1,
        anln2  LIKE anep-anln2,
        lnran  LIKE anep-lnran,
        anbtr  LIKE anep-anbtr,
        bzdat  LIKE anep-bzdat,
        afabe  LIKE anep-afabe,
        augln  LIKE anep-augln,
        belnr  LIKE anep-belnr,
        bwasl  LIKE anep-bwasl,
        awtyp  LIKE anek-awtyp,
        aworg  LIKE anek-aworg,
        budat  LIKE anek-budat,
        gittgr LIKE tabw-gittgr,
        xerlos LIKE tabw-xerlos,
        bwagrp LIKE tabw-bwagrp,
      END  OF it_anep.

DATA: BEGIN  OF  it_anlp OCCURS 0,
        anln1   LIKE anlp-anln1,
        anln2   LIKE anlp-anln2,
        peraf   LIKE anlp-peraf,
        afbnr   LIKE anlp-afbnr,
        aafaz   LIKE anlp-aafaz,
        aafag   LIKE anlp-aafag,
        nafaz   LIKE anlp-nafaz,
        nafag   LIKE anlp-nafag,
        afaber  LIKE anlp-afaber,
      END  OF it_anlp.

DATA: BEGIN OF it_anea OCCURS 0,
        anln1  LIKE anea-anln1,
        anln2  LIKE anea-anln2,
        lnran  LIKE anea-lnran,
        afabe  LIKE anea-afabe,
        nafav  LIKE anea-nafav,
        aafav  LIKE anea-aafav,
        nafal  LIKE anea-nafal,
        aafal  LIKE anea-aafal,
        awtyp  LIKE anek-awtyp,
        aworg  LIKE anek-aworg,
        budat  LIKE anek-budat,
        bzdat  LIKE anek-bzdat,
      END OF it_anea.

DATA: it_89 LIKE zfit0089 OCCURS 0 WITH HEADER LINE,
      it_90 LIKE zfit0090 OCCURS 0 WITH HEADER LINE,
      it_91 LIKE zfit0091 OCCURS 0 WITH HEADER LINE.

DATA: ftab(80) OCCURS 0 WITH HEADER LINE.

RANGES: r_anlkl  FOR anla-anlkl.

DATA: g_fr     LIKE bkpf-monat,
      g_to     LIKE bkpf-monat,
      g_perc   TYPE p,
      g_okcode LIKE sy-ucomm.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS: p_bukrs LIKE bkpf-bukrs OBLIGATORY MEMORY ID buk,
            p_gjahr LIKE bkpf-gjahr OBLIGATORY.
SELECT-OPTIONS: s_monat FOR bkpf-monat OBLIGATORY NO-EXTENSION.

PARAMETERS: p_afabe  LIKE anlb-afabe OBLIGATORY DEFAULT '01'.

SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_anlkl  FOR anla-anlkl,
                s_anln1  FOR anla-anln1,
                s_aktiv  FOR anla-aktiv,
                s_deakt  FOR anla-deakt.

SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS: s_gsber  FOR anlz-gsber,
                s_kostl  FOR anlz-kostl,
                s_kostlv FOR anlz-kostlv,
                s_werks  FOR anlz-werks,
                s_stort  FOR anlz-stort.

SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS: s_sernr  FOR anla-sernr,
                s_invnr  FOR anla-invnr.

SELECTION-SCREEN SKIP 1.

PARAMETERS: p_r_p RADIOBUTTON GROUP gr1 DEFAULT 'X', "Posting date
            p_r_a RADIOBUTTON GROUP gr1.             "Asset value date
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM read_tables.
  PERFORM modify_table.
  PERFORM write_table.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .

  DATA: l_datum LIKE sy-datum.

  CONCATENATE sy-datum(6) '01' INTO l_datum.
  IF sy-batch = ''.
    l_datum = l_datum - 1.
  ENDIF.

  p_gjahr = l_datum(4).

  s_monat-sign   = 'I'.
  s_monat-option = 'EQ'.
  s_monat-low    = l_datum+4(2).
  APPEND s_monat.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  READ_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_tables .

  DATA: l_lines LIKE sy-tabix.

  READ TABLE s_monat INDEX 1.
  g_fr = s_monat-low.
  g_to = s_monat-high.
  IF g_to IS INITIAL.
    g_to = s_monat-low.
  ENDIF.

  SELECT SINGLE * FROM t001
                  WHERE bukrs = p_bukrs.

  SELECT SINGLE * FROM t093u
                  WHERE bukrs = p_bukrs
                  AND   afabe = p_afabe.
  IF sy-subrc = 0.
    IF t093u-afblgj > p_gjahr.
      MESSAGE e174 WITH t093u-afblpe t093u-afblgj.
    ELSEIF t093u-afblgj = p_gjahr AND t093u-afblpe > g_to.
      MESSAGE e174 WITH t093u-afblpe t093u-afblgj.
    ENDIF.
  ENDIF.

  PERFORM progress_indicator USING 1 5 'Reading Tables'.
  PERFORM read_anla.

  DESCRIBE TABLE itab LINES l_lines.
  IF l_lines > 0.
    PERFORM progress_indicator USING 2 5 'Reading Tables'.
    PERFORM read_anlc.

    PERFORM progress_indicator USING 3 5 'Reading Tables'.
    PERFORM read_anep.

    PERFORM progress_indicator USING 4 5 'Reading Tables'.
    PERFORM read_anlp.

    PERFORM progress_indicator USING 5 5 'Reading Tables'.
    PERFORM read_anea.

    PERFORM modify_itab.
  ENDIF.

ENDFORM.                    " READ_TABLES
*&---------------------------------------------------------------------*
*&      Form  WRITE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_table .

  PERFORM fieidcat_gathering.

  PERFORM alv_layout_init.

  PERFORM alv_header.

  PERFORM call_function.

ENDFORM.                    " WRITE_TABLE
*&---------------------------------------------------------------------*
*&      Form  READ_ANLA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_anla .

  CLEAR: ftab[].

  IF p_bukrs = '5100'.
    r_anlkl-sign   = 'I'.
    r_anlkl-option = 'BT'.
    r_anlkl-low    = '00001000'.
    r_anlkl-high   = '00008999'.
    APPEND r_anlkl.
  ENDIF.

  ftab = 'LA~BUKRS'.     APPEND ftab.
  ftab = 'LA~ANLKL'.     APPEND ftab.
  ftab = 'LA~ANLN1'.     APPEND ftab.
  ftab = 'LA~ANLN2'.     APPEND ftab.
  ftab = 'LA~AKTIV'.     APPEND ftab.
  ftab = 'LA~DEAKT'.     APPEND ftab.
  ftab = 'LA~ORD41'.     APPEND ftab.
  ftab = 'LA~GDLGRP'.    APPEND ftab.
  ftab = 'LA~SERNR'.     APPEND ftab.
  ftab = 'LA~INVNR'.     APPEND ftab.
  ftab = 'LA~TXT50'.     APPEND ftab.
  ftab = 'LA~MEINS'.     APPEND ftab.
  ftab = 'LA~MENGE'.     APPEND ftab.
  ftab = 'LA~AIBN1'.     APPEND ftab.
  ftab = 'LA~POSNR'.     APPEND ftab.
  ftab = 'LB~AFASL'.     APPEND ftab.
  ftab = 'LB~NDJAR'.     APPEND ftab.
  ftab = 'LB~NDPER'.     APPEND ftab.
  ftab = 'LB~AFABG'.     APPEND ftab.
  ftab = 'LZ~GSBER'.     APPEND ftab.
  ftab = 'LZ~KOSTL'.     APPEND ftab.
  ftab = 'LZ~KOSTLV'.    APPEND ftab.
  ftab = 'LZ~WERKS'.     APPEND ftab.
  ftab = 'LZ~STORT'.     APPEND ftab.
  ftab = 'LZ~RAUMN'.     APPEND ftab.

  SELECT (ftab)
         INTO CORRESPONDING FIELDS OF TABLE itab
         FROM anla AS la
         INNER JOIN anlz AS lz
         ON la~bukrs  =  lz~bukrs AND
            la~anln1  =  lz~anln1 AND
            la~anln2  =  lz~anln2
         INNER JOIN anlb AS lb
         ON la~bukrs  =  lb~bukrs AND
            la~anln1  =  lb~anln1 AND
            la~anln2  =  lb~anln2
         WHERE la~bukrs  =  p_bukrs
         AND   la~anln1  IN s_anln1
         AND   la~anln2  =  '0000'
         AND   la~anlkl  IN s_anlkl
         AND   la~anlkl  IN r_anlkl
         AND   la~aktiv  IN s_aktiv
         AND   la~deakt  IN s_deakt
         AND   la~sernr  IN s_sernr
         AND   la~invnr  IN s_invnr
         AND   lb~afabe  =  p_afabe
         AND   lb~bdatu  =  '99991231'
         AND   lz~bdatu  =  '99991231'
         AND   lz~gsber  IN s_gsber
         AND   lz~kostl  IN s_kostl
         AND   lz~kostlv IN s_kostlv
         AND   lz~werks  IN s_werks
         AND   lz~stort  IN s_stort.

  delete itab where AKTIV is initial.

  SORT itab BY bukrs anlkl anln1 anln2.

ENDFORM.                    " READ_ANLA
*&---------------------------------------------------------------------*
*&      Form  READ_ANLC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_anlc .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_anlc
           FROM anlc
           FOR ALL ENTRIES IN itab
           WHERE bukrs  =  itab-bukrs
           AND   anln1  =  itab-anln1
           AND   anln2  =  itab-anln2
           AND   gjahr  =  p_gjahr
           AND   afabe  =  p_afabe.

  SORT it_anlc BY anln1 anln2 afabe.

ENDFORM.                    " READ_ANLC
*&---------------------------------------------------------------------*
*&      Form  READ_ANEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_anep .

  CLEAR ftab[].

  ftab = 'EP~ANLN1'.     APPEND ftab.
  ftab = 'EP~ANLN2'.     APPEND ftab.
  ftab = 'EP~LNRAN'.     APPEND ftab.
  ftab = 'EP~ANBTR'.     APPEND ftab.
  ftab = 'EP~BZDAT'.     APPEND ftab.
  ftab = 'EP~AFABE'.     APPEND ftab.
  ftab = 'EP~AUGLN'.     APPEND ftab.
  ftab = 'EP~BELNR'.     APPEND ftab.
  ftab = 'EP~BWASL'.     APPEND ftab.
  ftab = 'EK~BUDAT'.     APPEND ftab.
  ftab = 'EK~AWTYP'.     APPEND ftab.
  ftab = 'EK~AWORG'.     APPEND ftab.
  ftab = 'BW~GITTGR'.    APPEND ftab.
  ftab = 'BW~XERLOS'.    APPEND ftab.
  ftab = 'BW~BWAGRP'.    APPEND ftab.

  SELECT (ftab)
         INTO CORRESPONDING FIELDS OF TABLE it_anep
         FROM anep AS ep
         INNER JOIN anek AS ek
         ON ep~bukrs  =  ek~bukrs AND
            ep~anln1  =  ek~anln1 AND
            ep~anln2  =  ek~anln2 AND
            ep~gjahr  =  ek~gjahr AND
            ep~lnran  =  ek~lnran
         INNER JOIN tabw AS bw
         ON ep~bwasl  =  bw~bwasl
         FOR ALL ENTRIES IN itab
         WHERE ep~bukrs  =  itab-bukrs
         AND   ep~anln1  =  itab-anln1
         AND   ep~anln2  =  itab-anln2
         AND   ep~gjahr  =  p_gjahr
         AND   ep~afabe  =  p_afabe.

  IF p_r_p = 'X'.
    DELETE  it_anep  WHERE  budat+4(2) > g_to
                        OR  augln <> ' '.
  ELSE.
    DELETE  it_anep  WHERE  bzdat+4(2) > g_to
                        OR  augln <> ' '.
  ENDIF.

  SORT it_anep BY anln1 anln2 lnran.

ENDFORM.                    " READ_ANEP
*&---------------------------------------------------------------------*
*&      Form  READ_ANLP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_anlp .

  DATA: l_peraf LIKE anlp-peraf.

  l_peraf = g_to.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_anlp
           FROM anlp
           FOR ALL ENTRIES IN itab
           WHERE bukrs  =  itab-bukrs
           AND   anln1  =  itab-anln1
           AND   anln2  =  itab-anln2
           AND   gjahr  =  p_gjahr
           AND   peraf  <= l_peraf
           AND   afaber =  p_afabe.

  SORT it_anlp BY anln1 anln2 peraf afbnr.

ENDFORM.                    " READ_ANLP
*&---------------------------------------------------------------------*
*&      Form  READ_ANEA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_anea .

  CLEAR ftab[].

  ftab = 'EA~ANLN1'.     APPEND ftab.
  ftab = 'EA~ANLN2'.     APPEND ftab.
  ftab = 'EA~LNRAN'.     APPEND ftab.
  ftab = 'EA~AFABE'.     APPEND ftab.
  ftab = 'EA~NAFAV'.     APPEND ftab.
  ftab = 'EA~AAFAV'.     APPEND ftab.
  ftab = 'EA~NAFAL'.     APPEND ftab.
  ftab = 'EA~AAFAL'.     APPEND ftab.
  ftab = 'EK~AWTYP'.     APPEND ftab.
  ftab = 'EK~AWORG'.     APPEND ftab.
  ftab = 'EK~BUDAT'.     APPEND ftab.
  ftab = 'EK~BZDAT'.     APPEND ftab.

  SELECT (ftab)
         INTO CORRESPONDING FIELDS OF TABLE it_anea
         FROM anea AS ea
         INNER JOIN anek AS ek
         ON ea~bukrs  =  ek~bukrs AND
            ea~anln1  =  ek~anln1 AND
            ea~anln2  =  ek~anln2 AND
            ea~gjahr  =  ek~gjahr AND
            ea~lnran  =  ek~lnran
           FOR ALL ENTRIES IN itab
           WHERE ea~bukrs  =  itab-bukrs
           AND   ea~anln1  =  itab-anln1
           AND   ea~anln2  =  itab-anln2
           AND   ea~gjahr  =  p_gjahr
           AND   ea~afabe  =  p_afabe.

  IF p_r_p = 'X'.
    DELETE  it_anea  WHERE  budat+4(2) > g_to.
  ELSE.
    DELETE  it_anea  WHERE  bzdat+4(2) > g_to.
  ENDIF.

  SORT it_anea BY anln1 anln2 lnran afabe.

ENDFORM.                    " READ_ANEA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_itab .

  DATA: l_tabix LIKE sy-tabix,
        l_index LIKE sy-tabix,
        l_kostl LIKE anlz-kostl.

  DESCRIBE TABLE itab LINES l_tabix.
*
*  SORT it_anlc BY anln1 anln2.
*  SORT it_anlp BY anln1 anln2.

  IF p_r_p = 'X'.
    SORT it_anep BY anln1 anln2 budat.
    SORT it_anea BY anln1 anln2 budat.
  ELSE.
    SORT it_anep BY anln1 anln2 bzdat.
    SORT it_anea BY anln1 anln2 bzdat.
  ENDIF.

  LOOP AT itab.

    l_index = l_index + 1.
    PERFORM progress_indicator USING l_index l_tabix 'Data processing'.

    PERFORM get_anlc_amt.

    PERFORM get_anep_amt.

    PERFORM get_anlp_amt.

    PERFORM get_anea_amt.

    itab-l_bal = itab-l_acq - itab-l_ad.
    itab-e_acq = itab-l_acq + itab-c_acq.
    itab-e_ad  = itab-l_ad  + itab-c_ad.
    itab-e_bal = itab-e_acq - itab-e_ad.

    SELECT SINGLE txk20 INTO itab-txk20
                        FROM ankt
                        WHERE spras = sy-langu
                        AND   anlkl = itab-anlkl.

    IF itab-posnr <> ''.
      CLEAR l_kostl.

      SELECT SINGLE kostl posid
             INTO (l_kostl, itab-posid)
             FROM prps
             WHERE pspnr = itab-posnr.
      IF itab-kostl = ''.
        IF l_kostl <> ''.
          itab-kostl = l_kostl.
        ELSE.
          CASE p_bukrs.
            WHEN '5100' OR '5400' OR '5600'.
              IF itab-posid(2) >= '01' AND itab-posid(2) =< '99'.
                itab-kostl = itab-posid+2(5).
              ELSE.
                itab-kostl = itab-posid(5).
              ENDIF.
            WHEN 'BP01'.
              CASE itab-posid+4(2).
                WHEN '13'.
                  CONCATENATE itab-posid+4(2) '01' INTO itab-kostl.
                WHEN OTHERS.
                  CONCATENATE itab-posid+4(2) '00' INTO itab-kostl.
              ENDCASE.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY itab.

  ENDLOOP.

ENDFORM.                    " MODIFY_ITAB
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_INDEX  text
*      -->P_L_TABIX  text
*      -->P_1186   text
*----------------------------------------------------------------------*
FORM progress_indicator  USING    value(p_proc)
                                  value(p_total)
                                  value(p_text).

  DATA: l_v1       LIKE sy-tabix,
        l_v2       LIKE sy-tabix,
        l_perc     TYPE p,
        l_txt(3)   TYPE n,
        l_text(80).

  IF p_proc = 0.
    l_text = p_text.
  ELSE.
    l_v1 = p_proc.
    l_v2 = p_total.
    l_perc = TRUNC( l_v1 / l_v2 * 10 ) * 10.
    l_txt  = l_perc.
    CONCATENATE p_text ': ' l_txt '%' INTO l_text.
  ENDIF.

  IF p_proc = 0 OR g_perc <> l_perc.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_perc
        text       = l_text.

    g_perc = l_perc.
  ENDIF.

ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  GET_ANLC_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_anlc_amt .

  LOOP AT it_anlc WHERE anln1 = itab-anln1
                  AND   anln2 = itab-anln2.

    itab-l_acq = itab-l_acq + it_anlc-kansw + it_anlc-kmafa.

    itab-l_ad  = itab-l_ad  - it_anlc-knafa - it_anlc-kaafa.

    IF t093u-afblgj = p_gjahr AND t093u-afblpe = g_to.
      itab-c_dep = itab-c_dep - it_anlc-nafag - it_anlc-aafag.
      itab-c_ad  = itab-c_ad  + itab-c_dep.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_ANLC_AMT
*&---------------------------------------------------------------------*
*&      Form  GET_ANEP_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_anep_amt .

  DATA: l_monat LIKE bkpf-monat.

  LOOP AT it_anep WHERE anln1 = itab-anln1
                  AND   anln2 = itab-anln2.

    IF p_r_p = 'X'.
      l_monat = it_anep-budat+4(2).
    ELSE.
      l_monat = it_anep-bzdat+4(2).
    ENDIF.

    IF l_monat < g_fr.
      IF  it_anep-gittgr <> ''.
        itab-l_acq = itab-l_acq + it_anep-anbtr.
      ENDIF.
    ELSE.
      IF  it_anep-gittgr <> ''.
        itab-c_acq = itab-c_acq + it_anep-anbtr.
      ENDIF.
      CASE it_anep-gittgr.
        WHEN '10' OR '12'.                   " Acq
          IF itab-l_acq IS INITIAL.
            itab-c_new = itab-c_new + it_anep-anbtr.
          ELSE.
            itab-c_add = itab-c_add + it_anep-anbtr.
          ENDIF.
        WHEN '20' OR '25'.                   " Retire
          IF it_anep-xerlos = 'X'.
            itab-c_sal = itab-c_sal + it_anep-anbtr.
          ELSE.
            itab-c_dis = itab-c_dis + it_anep-anbtr.
          ENDIF.
        WHEN '30' OR '32'.                   " Reclass out
          itab-c_rou = itab-c_rou + it_anep-anbtr.
        WHEN '31' OR '33'.                   " Reclass in
          itab-c_rin = itab-c_rin + it_anep-anbtr.
        WHEN '34' OR '35' OR '36' OR '37'.   " CIP
          itab-c_cip = itab-c_cip + it_anep-anbtr.
        WHEN ''.
        WHEN OTHERS.
*          MESSAGE i034 WITH it_anep-gittgr itab-anln1.
          MESSAGE S034 WITH it_anep-gittgr itab-anln1.
      ENDCASE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " GET_ANEP_AMT
*&---------------------------------------------------------------------*
*&      Form  GET_ANLP_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_anlp_amt .

  DATA: lt_anlp LIKE it_anlp OCCURS 0 WITH HEADER LINE.

  DATA: l_gjahr LIKE anlp-gjahr,
        l_peraf LIKE anlp-peraf,
        l_mig   TYPE c.

  CLEAR lt_anlp[].
  LOOP AT it_anlp WHERE anln1 = itab-anln1
                  AND   anln2 = itab-anln2.

    lt_anlp = it_anlp.
    APPEND lt_anlp.
  ENDLOOP.


  LOOP AT lt_anlp.

    CLEAR l_mig.
    l_peraf = lt_anlp-peraf.
* Check if migration data
    AT FIRST.
      IF l_peraf = '001'.
        l_gjahr = p_gjahr - 1.
        l_peraf = '012'.
      ELSE.
        l_gjahr = p_gjahr.
        l_peraf = l_peraf - 1.
      ENDIF.

      SELECT SINGLE * FROM anlp
                      WHERE bukrs  =  itab-bukrs
                      AND   anln1  =  itab-anln1
                      AND   anln2  =  itab-anln2
                      AND   gjahr  =  l_gjahr
                      AND   peraf  <= l_peraf
                      AND   afaber =  p_afabe.
      IF sy-subrc <> 0.
        l_mig = 'X'.
      ENDIF.
    ENDAT.

    IF lt_anlp-peraf >= g_fr.
      itab-c_dep = itab-c_dep - lt_anlp-nafaz - lt_anlp-aafaz.
      itab-c_ad  = itab-c_ad  - lt_anlp-nafaz - lt_anlp-aafaz.
      IF l_mig = 'X'.
        itab-c_dep = itab-c_dep - lt_anlp-nafag.
        itab-c_ad  = itab-c_ad  - lt_anlp-nafag.
      ENDIF.
    ELSE.
      itab-l_ad  = itab-l_ad  - lt_anlp-nafaz - lt_anlp-aafaz.
      IF l_mig = 'X'.
        itab-l_ad  = itab-l_ad  - lt_anlp-nafag.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " GET_ANLP_AMT
*&---------------------------------------------------------------------*
*&      Form  GET_ANEA_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_anea_amt .

  DATA: l_monat LIKE bkpf-monat.

  LOOP AT it_anea WHERE anln1 = itab-anln1
                  AND   anln2 = itab-anln2.

    IF p_r_p = 'X'.
      l_monat = it_anea-budat+4(2).
    ELSE.
      l_monat = it_anea-bzdat+4(2).
    ENDIF.

    IF l_monat < g_fr.
      itab-l_ad  = itab-l_ad  - it_anea-nafav - it_anea-aafav
                              - it_anea-nafal - it_anea-aafal.
    ELSE.
      itab-c_adj = itab-c_adj - it_anea-nafav - it_anea-aafav
                              - it_anea-nafal - it_anea-aafal.
      itab-c_ad  = itab-c_ad  - it_anea-nafav - it_anea-aafav
                              - it_anea-nafal - it_anea-aafal.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " GET_ANEA_AMT
*&---------------------------------------------------------------------*
*&      Form  FIEIDCAT_GATHERING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieidcat_gathering .

  PERFORM fill_fieldcat USING:
*         Field       Text              No  KEY  Opt  Color
          'ANLKL'    'Class'           ' '  'X'  ' '  ' ',
          'TXK20'    'Asset Class'     ' '  'X'  ' '  ' ',
          'ANLN1'    'Asset No'        ' '  'X'  ' '  ' ',
          'ANLN2'    'Sub'             'X'  ' '  ' '  ' ',
          'TXT50'    'Asset Name'      ' '  ' '  ' '  ' ',
          'SERNR'    'Serial'          'X'  ' '  ' '  ' ',
          'INVNR'    'Inventory'       ' '  ' '  ' '  ' '.
  IF t001-xgsbe = 'X'.
    PERFORM fill_fieldcat USING:
            'GSBER'    'B.A'             ' '  ' '  ' '  ' '.
  ELSE.
    PERFORM fill_fieldcat USING:
            'GSBER'    'B.A'             'X'  ' '  ' '  ' '.
  ENDIF.
  PERFORM fill_fieldcat USING:
          'KOSTL'    'C.Ctr'           ' '  ' '  ' '  ' ',
          'KOSTLV'   'R.CCtr'          ' '  ' '  ' '  ' ',
          'WERKS'    'Plant'           ' '  ' '  ' '  ' ',
          'STORT'    'Location'        'X'  ' '  ' '  ' ',
          'RAUMN'    'Room'            'X'  ' '  ' '  ' ',
          'AIBN1'    'Origin'          'X'  ' '  ' '  ' ',
          'POSID'    'WBS'             'X'  ' '  ' '  ' ',
          'POSNR'    'WBS NR'          'X'  ' '  ' '  ' ',
          'AKTIV'    'Acq.Date'        ' '  ' '  ' '  ' ',
          'DEAKT'    'Deact.Dt'        ' '  ' '  ' '  ' ',
          'MEINS'    'Unit'            ' '  ' '  ' '  ' ',
          'MENGE'    'Qty'             ' '  ' '  ' '  ' ',
          'AFASL'    'Dep.Key'         ' '  ' '  ' '  ' ',
          'NDJAR'    'U.Life'          ' '  ' '  ' '  ' ',
          'NDPER'    'U.L.MM'          ' '  ' '  ' '  ' ',
          'AFABG'    'Dep.Date'        ' '  ' '  ' '  ' ',
          'L_ACQ'    'Beg.Acq'         ' '  ' '  'C'  'C500',
          'L_AD'     'Beg.AD'          ' '  ' '  'C'  'C500',
          'L_BAL'    'Beg.Bal'         ' '  ' '  'C'  'C510',
          'C_NEW'    'Cur.New'         ' '  ' '  'C'  'C400',
          'C_ADD'    'Cur.Add'         ' '  ' '  'C'  'C400',
          'C_RIN'    'Cur.R.In'        ' '  ' '  'C'  'C400',
          'C_ROU'    'Cur.R.Out'       ' '  ' '  'C'  'C400',
          'C_CIP'    'Cur.CIP'         ' '  ' '  'C'  'C400',
          'C_DIS'    'Cur.Disp'        ' '  ' '  'C'  'C400',
          'C_SAL'    'Cur.Sale'        ' '  ' '  'C'  'C400',
          'C_ACQ'    'Cur.Acq'         ' '  ' '  'C'  'C410',
          'C_DEP'    'Cur.Dep'         ' '  ' '  'C'  'C700',
          'C_ADJ'    'Cur.AD.Adj'      ' '  ' '  'C'  'C700',
          'C_AD'     'Cur.AD'          ' '  ' '  'C'  'C710',
          'E_ACQ'    'End.Acq'         ' '  ' '  'C'  'C300',
          'E_AD'     'End.AD'          ' '  ' '  'C'  'C300',
          'E_BAL'    'End.Bal'         ' '  ' '  'C'  'C310',
          'KEYPART'  'KEYPART'         'X'  ' '  ' '  ' ',
          'FUNCPART' 'FUNCPART'        'X'  ' '  ' '  ' '.

ENDFORM.                    " FIEIDCAT_GATHERING
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_layout_init .

*variant
  CLEAR ls_variant.
  ls_variant-report = 'ZFIR0062'.

* Sortierung
  REFRESH lt_sort.
  CLEAR: lt_sort, l_index.

  ls_sort-fieldname = 'ANLKL'.
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
  APPEND ls_sort TO lt_sort.  CLEAR ls_sort.

  ls_sort-fieldname = 'TXK20'.
  ls_sort-up        = 'X'.
  APPEND ls_sort TO lt_sort.  CLEAR ls_sort.

  ls_sort-fieldname = 'ANLN1'.
  ls_sort-up        = 'X'.
  APPEND ls_sort TO lt_sort.  CLEAR ls_sort.

* Layout
  CLEAR ls_layout.
  ls_layout-f2code            = 'PICK'.
  ls_layout-key_hotspot       = 'X'.
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-zebra             = 'X'.
  ls_layout-info_fieldname    = 'COLOR'.
  ls_layout-box_fieldname     = 'CHECK'.

ENDFORM.                    " ALV_LAYOUT_INIT
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1632   text
*      -->P_1633   text
*      -->P_1634   text
*      -->P_1635   text
*      -->P_1636   text
*----------------------------------------------------------------------*
FORM fill_fieldcat  USING    p_fieldname
                             p_seltext_m
                             p_nodisp
                             p_key
                             p_option
                             p_color.

  CLEAR ls_fieldcat.

  ls_fieldcat-tabname   = 'ITAB'.
  ls_fieldcat-fieldname = p_fieldname.
  ls_fieldcat-seltext_m = p_seltext_m.
  ls_fieldcat-no_out    = p_nodisp.

  IF p_key = 'X'.
    ls_fieldcat-emphasize     = 'C100'.
    ls_fieldcat-fix_column    = 'X'.
  ENDIF.

  CASE p_option.
    WHEN 'C'.
      ls_fieldcat-currency    = t001-waers.
      ls_fieldcat-do_sum      = 'X'.
    WHEN 'H'.
      ls_fieldcat-hotspot     = 'X'.
  ENDCASE.

  IF p_color <> ''.
    ls_fieldcat-emphasize     = p_color.
  ENDIF.

  CASE  p_fieldname.
    WHEN  'KOSTL' OR 'KOSTLV' OR 'GSBER' OR 'STORT'.
      ls_fieldcat-ref_tabname   = 'ANLZ'.
    WHEN  'ANLKL' OR 'ANLN1' OR 'SERNR' OR 'INVNR' OR
          'POSNR' OR 'AIBN1'.
      ls_fieldcat-ref_tabname   = 'ANLA'.
    WHEN  'AFASL' OR 'AFABG'.
      ls_fieldcat-ref_tabname   = 'ANLB'.
    WHEN  'POSID'.
      ls_fieldcat-ref_tabname   = 'PRPS'.
    WHEN  'MENGE'.
      ls_fieldcat-qfieldname    = 'MEINS'.
  ENDCASE .

  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
  APPEND ls_fieldcat TO lt_fieldcat.

ENDFORM.                    " FILL_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function .

  CLEAR : itab.

  l_repid  = sy-repid.

  CLEAR: lt_excluding[].
  AUTHORITY-CHECK OBJECT 'ZFI_POST'
           ID 'ACTVT' FIELD '01'.
  IF sy-subrc <> 0.
    ls_excluding-fcode = 'RECV'.
    APPEND ls_excluding TO lt_excluding.
    ls_excluding-fcode = 'INSP'.
    APPEND ls_excluding TO lt_excluding.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = l_repid
      i_callback_pf_status_set = 'ALV_PF_STATUS_SET'
      i_callback_top_of_page   = 'ALV_TOP_OF_LIST'
      i_callback_user_command  = 'ALV_USER_COMMAND'
      i_structure_name         = 'ITAB'
      is_layout                = ls_layout
      it_fieldcat              = lt_fieldcat
      it_excluding             = lt_excluding
      it_sort                  = lt_sort
      it_filter                = lt_filter
      i_save                   = 'A'
      is_variant               = ls_variant
    TABLES
      t_outtab                 = itab
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  alv_pf_status_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM alv_pf_status_set USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'STANDARD_ALV' EXCLUDING rt_extab.

ENDFORM.                    " ALV_PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  alv_top_of_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_top_of_list.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.

ENDFORM.                    "alv_top_of_list
*&---------------------------------------------------------------------*
*&      Form  alv_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN 'PICK'.
      CHECK NOT rs_selfield-value IS INITIAL.

      READ TABLE itab INDEX rs_selfield-tabindex.

      CASE rs_selfield-fieldname.
        WHEN 'ANLN1'.
          SET PARAMETER ID 'AN1' FIELD itab-anln1.
          SET PARAMETER ID 'AN2' FIELD itab-anln2.
          SET PARAMETER ID 'BUK' FIELD p_bukrs.
          CALL TRANSACTION 'AW01N' AND SKIP FIRST SCREEN.
*         CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
        WHEN 'AIBN1'.
          SET PARAMETER ID 'AN1' FIELD itab-aibn1.
          SET PARAMETER ID 'AN2' FIELD space.
          SET PARAMETER ID 'BUK' FIELD p_bukrs.
          CALL TRANSACTION 'AW01N' AND SKIP FIRST SCREEN.
*         CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
        WHEN 'POSID' OR 'POSNR'.
          SET PARAMETER ID 'PSP' FIELD space.
          SET PARAMETER ID 'PRO' FIELD itab-posid.
          CALL TRANSACTION 'CJ13' AND SKIP FIRST SCREEN.
      ENDCASE.
    WHEN 'RECV'.
      LOOP AT itab WHERE check = 'X'
                   AND   deakt IS INITIAL.      "Deactivation date
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        PERFORM create_receiving_list.
      ELSE.
        MESSAGE s181.
      ENDIF.
    WHEN 'INSP'.
      LOOP AT itab WHERE check = 'X'
                   AND   deakt IS INITIAL       "Deactivation date
                   AND   NOT e_acq IS INITIAL.  "Acquistion cost
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        PERFORM create_inspection_list.
      ELSE.
        MESSAGE s181.
      ENDIF.
  ENDCASE.
ENDFORM.                    "alv_user_comma
*&---------------------------------------------------------------------*
*&      Form  ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_header .

  DATA: l_text1(80),
        l_text2(80),
        l_text3(80).

  CONCATENATE: p_bukrs t001-butxt INTO l_text1 SEPARATED BY space,
               g_fr '/' p_gjahr INTO l_text2,
               g_to '/' p_gjahr INTO l_text3,
               l_text2 '~' l_text3 INTO l_text3 SEPARATED BY space.

  PERFORM fill_header USING: 'S' 'C.Code' l_text1,
                             'S' 'Period' l_text3.

ENDFORM.                    " ALV_HEADER
*&---------------------------------------------------------------------*
*&      Form  FILL_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2274   text
*      -->P_2275   text
*      -->P_L_TEXT1  text
*----------------------------------------------------------------------*
FORM fill_header  USING    p_type
                           p_key
                           p_info.

  ls_header-typ  = p_type.
  ls_header-key  = p_key.
  ls_header-info = p_info.

  APPEND ls_header TO lt_header.

ENDFORM.                    " FILL_HEADER
*&---------------------------------------------------------------------*
*&      Form  MODIFY_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_table .

  CHECK p_gjahr = sy-datum(4) AND g_to >= sy-datum+4(2).
  CHECK p_afabe = '01'.
  CHECK: s_anlkl[] IS INITIAL,
         s_anln1[] IS INITIAL,
         s_aktiv[] IS INITIAL,
         s_deakt[] IS INITIAL,
         s_gsber[] IS INITIAL,
         s_kostl[] IS INITIAL,
         s_kostlv[] IS INITIAL,
         s_werks[] IS INITIAL,
         s_stort[] IS INITIAL,
         s_sernr[] IS INITIAL,
         s_invnr[] IS INITIAL.
  CHECK p_r_p = 'X'.

  LOOP AT itab WHERE deakt IS INITIAL.
    MOVE-CORRESPONDING itab TO it_89.
    it_89-crdat = sy-datum.
    it_89-crtim = sy-uzeit.
    APPEND it_89.
    CLEAR  it_89.
  ENDLOOP.
  IF sy-subrc = 0.
    DELETE FROM zfit0089 WHERE bukrs = p_bukrs.
    MODIFY zfit0089 FROM TABLE it_89.
    IF sy-subrc <> 0.
      MESSAGE e061 WITH 'ZFIT0089'.
    ENDIF.
  ENDIF.

ENDFORM.                    " MODIFY_TABLE
*&---------------------------------------------------------------------*
*&      Form  create_receiving_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_receiving_list .

  CLEAR: g_okcode,
         zfit0090.
  CALL SCREEN '9100' STARTING AT 10 5.

ENDFORM.                    "create_receiving_list
*&---------------------------------------------------------------------*
*&      Form  CREATE_INSPECTION_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_inspection_list .

  CLEAR: g_okcode,
         zfit0091.
  CALL SCREEN '9200' STARTING AT 10 5.

ENDFORM.                    " CREATE_INSPECTION_LIST

include zfir0062o01.

include zfir0062i01.

include zfir0062f01.



*Selection texts
*---------------
* P_AFABE         Depreciation area
* P_BUKRS         Company code
* P_R_A         Reporting by asset value date
* S_AKTIV         Acquisition date
* S_ANLN1         Asset number
* S_GSBER         Business area
* S_KOSTL         Cost center
* S_MONAT         Period
* S_STORT         Location


*Messages
*----------------------------------------------------------
*034   Processing for trans. types & was not defined. Asset: &
*061   Table saving error: &
*174   This report is valid from &/&.
*181   Please choose items to make a list.
