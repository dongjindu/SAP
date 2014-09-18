*----------------------------------------------------------------------
* Program ID        : ZRFI013
* Title             : [FI] Invoice Detail Lists
* Created on        : 03/26/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : [FI] Invoice Detail Lists
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  01/12/2012  Valerian  UD1K953713  HMMA Engine Plant split
*                                    implementation
*& 06/20/2013  T00303    UD1K957449  U1: Apply Archiving
*----------------------------------------------------------------------
REPORT zrfi013  MESSAGE-ID zmco.

TABLES: t001,ekbe, ekbeh, mara, makt, mbew, ekko, ekpo, konp,
        sscrfields, mkpf .
TABLES: eipa, eina.
INCLUDE <icon>.                        " icon
*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_bukrs LIKE ekko-bukrs  MEMORY ID buk.
SELECT-OPTIONS s_lifnr FOR ekko-lifnr MEMORY ID lif OBLIGATORY.
SELECT-OPTIONS s_refdt FOR sy-datum.

PARAMETERS: p_curr  AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_past  AS CHECKBOX DEFAULT ' '.
PARAMETERS: p_due   AS CHECKBOX DEFAULT ' '.
PARAMETERS: p_augdt LIKE bsak-augdt.
PARAMETERS: p_reval AS CHECKBOX DEFAULT ' '.
PARAMETERS: p_grno AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b1.
PARAMETERS: p_short AS CHECKBOX DEFAULT ' '.

SELECTION-SCREEN BEGIN OF BLOCK view-result WITH FRAME.
SELECTION-SCREEN PUSHBUTTON  1(24) timpr USER-COMMAND timpr.
SELECTION-SCREEN END OF BLOCK view-result.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS s_matnr FOR ekbe-matnr MEMORY ID mat MODIF ID pll.
SELECT-OPTIONS s_ebeln FOR ekbe-ebeln MEMORY ID bes MODIF ID pll.
SELECT-OPTIONS s_ebelp FOR ekbe-ebelp MODIF ID pll.
SELECT-OPTIONS s_gjahr FOR ekbe-gjahr MEMORY ID mja MODIF ID pll.
SELECT-OPTIONS s_belnr FOR ekbe-belnr MEMORY ID mbn MODIF ID pll.

SELECT-OPTIONS s_budat FOR ekbe-budat MODIF ID pll.
SELECT-OPTIONS s_cpudt FOR ekbe-cpudt MODIF ID pll.

SELECT-OPTIONS s_lfgja FOR ekbe-lfgja MODIF ID pll.
SELECT-OPTIONS s_lfbnr  FOR ekbe-lfbnr MODIF ID pll.

SELECT-OPTIONS s_bsart FOR ekko-bsart MEMORY ID bsa MODIF ID pll.
SELECT-OPTIONS s_ekgrp FOR ekko-ekgrp MODIF ID pll.

SELECT-OPTIONS s_matkl FOR mara-matkl MEMORY ID mkl MODIF ID pll.
SELECT-OPTIONS s_mtart FOR mara-mtart MEMORY ID mta MODIF ID pll.
SELECT-OPTIONS s_profl FOR mara-profl MODIF ID pll.

SELECT-OPTIONS s_bewtp FOR ekbe-bewtp  MODIF ID pll.
PARAMETERS: p_summ  AS CHECKBOX DEFAULT ' '  MODIF ID pll.
PARAMETERS: p_diff NO-DISPLAY MODIF ID pll.

*PARAMETERS: p_chkms  AS CHECKBOX DEFAULT ' '.
PARAMETERS: p_chkinf AS CHECKBOX DEFAULT ' '  MODIF ID pll.
PARAMETERS: p_chkinv AS CHECKBOX DEFAULT ' '  MODIF ID pll.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETER p_vari    LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b3.

PARAMETERS par_cb1(1) TYPE c NO-DISPLAY DEFAULT 'X'.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

DATA: gv_answer(1).
DEFINE __popup.
  perform pop_up using
  &1 &2 &3
  changing gv_answer.
  check gv_answer eq 'J'.
END-OF-DEFINITION.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
*-------------------------------------------------------------*
* DATA
*--------------------------------------------------------------*
*  TABLES: konp.
DATA: BEGIN OF it_knumh OCCURS 0,
        knumh  LIKE konh-knumh,
        datab  LIKE konh-datab, "Valid-from date
        datbi  LIKE konh-datbi,
        lifnr  LIKE lfa1-lifnr,
        kstbmt TYPE kstbmt,                                 "3 digit
        kbetr  LIKE konp-kbetr, "rate
        kpein  LIKE konp-kpein, "pricing unit
        ekorg  LIKE eine-ekorg,
        infnr  LIKE eina-infnr,
      END   OF it_knumh.

DATA g_sscr_ucomm TYPE sscrfields-ucomm.

DATA: BEGIN OF itab OCCURS 1000, " WITH HEADER LINE,
        matnr LIKE ekbe-matnr,
        ebeln LIKE ekbe-ebeln,
        ebelp LIKE ekbe-ebelp,
        bewtp LIKE ekbe-bewtp,  "PO history category

        gjahr LIKE ekbe-gjahr,
        belnr LIKE ekbe-belnr,
        budat LIKE ekbe-budat,
        cpudt LIKE ekbe-cpudt,
        shkzg LIKE ekbe-shkzg,
        menge LIKE ekbe-menge,
        dmbtr LIKE ekbe-dmbtr,
        reewr LIKE ekbe-reewr,   "Inv.Value

        lfgja LIKE ekbe-lfgja,
        lfbnr LIKE ekbe-lfbnr,

        xblnr LIKE ekbe-xblnr,

        bsart LIKE ekko-bsart,   "PO type
        bstyp LIKE ekko-bstyp,   "PO category
        ekgrp LIKE ekko-ekgrp,
        lifnr LIKE ekko-lifnr,

        peinh LIKE ekpo-peinh,
        uebtk LIKE ekpo-uebtk,
        elikz LIKE ekpo-elikz,
        erekz LIKE ekpo-erekz,
        loekz LIKE ekpo-loekz,

        mtart LIKE mara-mtart,
        profl LIKE mara-profl,
        maktx LIKE makt-maktx,
        infnr LIKE eina-infnr,

        refdt     LIKE ekbe-budat,
        yyyymm(6) TYPE c,
        zvbeln    LIKE likp-vbeln,
        asn       LIKE likp-borgr_grp,
        zbelnr    LIKE bsis-belnr,
        duedt     LIKE bsis-zfbdt,
        clrdt     LIKE bsis-augdt,
        augbl     LIKE bsis-augbl,
        blart     LIKE bsis-blart,

        zmenge LIKE ekbe-menge,  "sign
        zdmbtr LIKE ekbe-dmbtr,  "sign
        ivprc  LIKE ekbe-dmbtr,  "IV price

        saprc  LIKE ekbe-dmbtr,  "SA $
        saunt  LIKE konp-kpein,  "SA unit
        saval  LIKE ekbe-dmbtr,  "SA value
        sano(1) TYPE c,          "No SA price

        ifprc  LIKE ekbe-dmbtr,  "info $
        ifunt  LIKE konp-kpein,  "info unit
        ifval  LIKE ekbe-dmbtr,  "info value

        diffa  LIKE ekbe-dmbtr,  "SA-IV
        diffb  LIKE ekbe-dmbtr,  "Info-SA

* by ig.moon 8/28/2008 {
        bktxt  TYPE bktxt,
* }

* by ig.moon 2/4/2009 {
        belnr1  TYPE belnr_d,
        belnr2  TYPE belnr_d,
* }
* by Furong on 06/06/2014
        kidno  TYPE kidno,
* }

        mismatch,
END OF itab.

DATA: BEGIN OF itab_comp OCCURS 1000, " WITH HEADER LINE,
        lfbnr LIKE ekbe-lfbnr,
        matnr LIKE ekbe-matnr,
        ivprc  LIKE ekbe-dmbtr,  "IV price
       $ivprc(20),
END OF itab_comp.
DATA $itab_comp LIKE itab_comp OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_mkpf OCCURS 0,
         mblnr  TYPE mblnr,
         mjahr  TYPE mjahr,
         xblnr  TYPE xblnr,
         budat  TYPE budat,
* by ig.moon 8/28/2008 {
         bktxt  TYPE bktxt,
*}
      END OF it_mkpf.

DATA: BEGIN OF it_eina OCCURS 0,
         lifnr  TYPE lifnr,
         matnr  TYPE matnr,
         infnr  TYPE infnr,
      END OF it_eina.

DATA: BEGIN OF it_lifnr OCCURS 0,
         lifnr  TYPE lifnr,
         matnr  TYPE matnr,
      END OF it_lifnr.

DATA: BEGIN OF it_rbkp OCCURS 0,
         belnr  TYPE re_belnr,
         gjahr  TYPE gjahr,
         lifnr  TYPE lifnr,
         empfb  TYPE empfb,
         kidno  TYPE kidno,
      END OF it_rbkp.

DATA: BEGIN OF it_bkpf OCCURS 0,
         awkey  TYPE awkey,
         belnr  TYPE re_belnr,
         blart  TYPE blart,
      END OF it_bkpf.

DATA: BEGIN OF gt_lfa1 OCCURS 0,
         lifnr  TYPE lifnr,
         land1  TYPE land1,
      END OF gt_lfa1.
RANGES: r_lfa1_us FOR lfa1-lifnr,
        r_lfa1_fr FOR lfa1-lifnr.

DATA: alv_tab LIKE itab OCCURS 0 WITH HEADER LINE.

RANGES: r_bewtp FOR ekbe-bewtp.

*data: begin of i_eina occurs 0,
*         INFNR  like eina-infnr,
*         MATNR  like eina-matnr,
*      end of i_eina.


*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader,
      x_layout TYPE disvariant,
      l_variant    TYPE disvariant,  "Display Variant
      l_layout     TYPE slis_layout_alv.  "List layout specifications


DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV

*- U1 start
DATA: gt_ekko_a TYPE TABLE OF ekko WITH HEADER LINE,
      gt_ekpo_a TYPE TABLE OF ekpo WITH HEADER LINE,
      gt_ekbe_a TYPE TABLE OF ekbe WITH HEADER LINE,
      gt_konp_a TYPE TABLE OF konp WITH HEADER LINE,
      gt_ekbeh_a TYPE TABLE OF ekbeh WITH HEADER LINE.

DATA: BEGIN OF gt_itab_a OCCURS 0,
      awkey  TYPE awkey,
      END OF gt_itab_a.

DATA: BEGIN OF gt_itab_a2 OCCURS 0,
      lfbnr TYPE lfbnr,
      lfgja TYPE lfgja,
      END OF gt_itab_a2.

DATA: BEGIN OF gt_itab_a3 OCCURS 0,
      belnr  TYPE lfbnr,
      gjahr  TYPE lfgja,
      END OF gt_itab_a3.

DATA: g_awkey TYPE awkey.
RANGES: gr_awkey FOR bkpf-awkey.
*- U1 End

*&---------------------------------------------------------------------*
REFRESH r_bewtp. CLEAR r_bewtp.
DESCRIBE TABLE s_bewtp LINES sy-index.
IF sy-index > 0.
  r_bewtp[] = s_bewtp[].
ELSE.
  r_bewtp-option = 'EQ'.  r_bewtp-sign = 'I'.
  IF p_reval = 'X'.
    r_bewtp-low    = 'W'.  APPEND r_bewtp.
    r_bewtp-low    = 'N'.  APPEND r_bewtp.
  ELSE.
    r_bewtp-low    = 'Q'.  APPEND r_bewtp.
    r_bewtp-low    = 'R'.  APPEND r_bewtp.
    r_bewtp-low    = 'W'.  APPEND r_bewtp.
    r_bewtp-low    = 'N'.  APPEND r_bewtp.
* by ig.moon 2/22/2010 {
    IF p_grno EQ 'X'.
      r_bewtp-low    = 'E'.  APPEND r_bewtp.
    ENDIF.
* }

  ENDIF.
ENDIF.
*------------------------------------------------------------*
*  At Selection-Screen
*-------------------------------------------------------------*
CLEAR x_layout.
x_layout-report = sy-repid.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  DATA: rs_variant LIKE disvariant,
        nof4 TYPE c.

  CLEAR nof4.
  LOOP AT SCREEN.
    IF screen-name = 'P_VARI'.
      IF screen-input = 0.
        nof4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc = 0 AND nof4 EQ space.
    p_vari = rs_variant-variant.

  ENDIF.

INITIALIZATION.


AT SELECTION-SCREEN.
  g_sscr_ucomm = sscrfields-ucomm.
  CASE g_sscr_ucomm.
    WHEN 'TIMPR'.

      IF par_cb1 = ' '.
        par_cb1 = 'X'.
      ELSE.
        par_cb1 = ' '.
      ENDIF.

  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modi_screen.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  p_diff = 'X'.

  PERFORM select_vendors.

  IF p_summ EQ 'X'.
    p_diff = 'X'.
  ENDIF.

  IF p_chkinv EQ 'X' OR p_chkinf EQ 'X'.
    p_diff = 'X'.
  ENDIF.

  IF p_curr = 'X'.
    PERFORM select_current_history.
*    IF sy-dbcnt > 5000.
*      __popup '' 'Too many data selected. Do you want to proceed?' ''.
*      IF gv_answer = 'N'. LEAVE PROGRAM.  ENDIF.
*    ENDIF.
  ENDIF.
  IF p_past = 'X'.
    PERFORM select_past_history.
*    IF sy-dbcnt > 5000.
*      __popup '' 'Too many data selected. Do you want to proceed?' ''.
*      IF gv_answer = 'N'. LEAVE PROGRAM.  ENDIF.
*    ENDIF.
  ENDIF.

  DESCRIBE TABLE itab LINES sy-tabix.

  IF sy-tabix = 0.
    MESSAGE s000 WITH 'No record found !'.
  ELSE.

    IF p_grno EQ 'X'.
      PERFORM filter_gr_list.
      DESCRIBE TABLE itab LINES sy-tabix.

      IF sy-tabix = 0.
        MESSAGE s000 WITH 'No record found !'.
        STOP.
      ENDIF.

    ENDIF.

    CLEAR : it_eina[],it_eina.
    CLEAR : it_lifnr[],it_lifnr.
    DATA $ix TYPE i .

    LOOP AT itab.
      it_lifnr-lifnr = itab-lifnr.
      it_lifnr-matnr = itab-matnr.
      COLLECT it_lifnr.
    ENDLOOP.

    READ TABLE it_lifnr INDEX 1.

    IF sy-subrc EQ 0.
      SELECT lifnr matnr infnr INTO TABLE it_eina
      FROM eina FOR ALL ENTRIES IN it_lifnr
      WHERE lifnr EQ it_lifnr-lifnr
       AND matnr EQ it_lifnr-matnr.
      SORT it_eina BY lifnr matnr.

      LOOP AT itab.
        $ix = sy-tabix.
        CLEAR itab-lifnr.
        READ TABLE it_eina WITH KEY lifnr = itab-lifnr
                                    matnr = itab-matnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          itab-infnr = it_eina-infnr.
          MODIFY itab INDEX $ix TRANSPORTING infnr.
        ENDIF.
      ENDLOOP.

    ENDIF.
    PERFORM select_mkpf_ref.
    PERFORM select_bkpf_ref.
    PERFORM get_detail_info.
    PERFORM prepare_alv.


    PERFORM show_alv.

  ENDIF.
*----------------------------------------------------------------------*
* End-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

*  IF P_DIFF = 'X'.
*    DELETE ALV_TAB WHERE DIFFA = 0 AND DIFFB = 0.
*  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  select_current_history
*&---------------------------------------------------------------------*
FORM select_current_history.

*  DESCRIBE TABLE r_lfa1_us LINES sy-tabix.
*  IF sy-tabix > 0.
*    SELECT hist~belnr hist~bewtp hist~budat hist~cpudt hist~dmbtr
*           hist~ebeln hist~ebelp hist~gjahr hist~lfbnr hist~lfgja
*           hist~matnr hist~menge hist~shkzg hist~reewr
*           hist~xblnr
*           ekko~bsart ekko~bstyp ekko~ekgrp ekko~lifnr
*           ekpo~peinh ekpo~uebtk ekpo~elikz ekpo~erekz ekpo~loekz
*           mara~mtart mara~profl
*           makt~maktx
*           eina~infnr
*    INTO CORRESPONDING FIELDS OF TABLE itab
*    FROM ( ekbe  AS hist
*           INNER JOIN mara
*           ON mara~matnr = hist~matnr
*           INNER JOIN ekko
*           ON ekko~ebeln = hist~ebeln
*           INNER JOIN ekpo
*           ON ekpo~ebeln = hist~ebeln
*           AND ekpo~ebelp = hist~ebelp
*           INNER JOIN eina
*           ON  eina~matnr = ekpo~matnr
*           AND eina~lifnr = ekko~lifnr
*           INNER JOIN makt
*           ON makt~matnr = hist~matnr
** UD1K941198 - by IG.MOON 08/01/2007 {
*           INNER JOIN mkpf AS c
*           ON  c~mblnr = hist~lfbnr
*           AND c~mjahr = hist~lfgja
** }
*           )
*           WHERE hist~ebeln IN s_ebeln
*             AND hist~ebelp IN s_ebelp
*             AND hist~matnr IN s_matnr
*             AND hist~bewtp IN r_bewtp
*             AND hist~budat IN s_budat
*             AND hist~cpudt IN s_cpudt
*             AND hist~belnr IN s_belnr
*             AND hist~belnr <> space
*             AND hist~gjahr IN s_gjahr
*             AND hist~lfgja IN s_lfgja
*             AND hist~lfbnr IN s_lfbnr
*             AND hist~lfbnr IN s_lfbnr
*             AND mara~matkl IN s_matkl
*             AND mara~mtart IN s_mtart
*             AND mara~profl IN s_profl
*             AND ekko~bsart IN s_bsart
*             AND ekko~ekgrp IN s_ekgrp
*             AND ekko~lifnr IN r_lfa1_us  "S_LIFNR
*             AND ekko~bukrs = p_bukrs
*             AND makt~spras = sy-langu
** UD1K941198 - by IG.MOON 08/01/2007 {
*             AND c~budat    IN s_refdt.
** }
*
*  ENDIF.
*
*  DESCRIBE TABLE r_lfa1_fr LINES sy-tabix.
*  IF sy-tabix > 0.
*    SELECT hist~belnr hist~bewtp hist~budat hist~cpudt hist~dmbtr
*           hist~ebeln hist~ebelp hist~gjahr hist~lfbnr hist~lfgja
*           hist~matnr hist~menge hist~shkzg hist~reewr
*           hist~xblnr
*           ekko~bsart ekko~bstyp ekko~ekgrp ekko~lifnr
*           ekpo~peinh ekpo~uebtk ekpo~elikz ekpo~erekz ekpo~loekz
*           mara~mtart mara~profl
*           makt~maktx
*           eina~infnr
*    APPENDING CORRESPONDING FIELDS OF TABLE itab
*    FROM ( ekbe  AS hist
*           INNER JOIN mara
*           ON mara~matnr = hist~matnr
*           INNER JOIN ekko
*           ON ekko~ebeln = hist~ebeln
*           INNER JOIN ekpo
*           ON ekpo~ebeln = hist~ebeln
*           AND ekpo~ebelp = hist~ebelp
*           INNER JOIN eina
*           ON  eina~matnr = ekpo~matnr
*           AND eina~lifnr = ekko~lifnr
*           INNER JOIN makt
*           ON makt~matnr = hist~matnr
*           )
*           WHERE hist~ebeln IN s_ebeln
*             AND hist~ebelp IN s_ebelp
*             AND hist~matnr IN s_matnr
*             AND hist~bewtp IN r_bewtp
*             AND hist~budat IN s_budat
*             AND hist~cpudt IN s_cpudt
*             AND hist~belnr IN s_belnr
*             AND hist~belnr <> space
*             AND hist~gjahr IN s_gjahr
*             AND hist~lfgja IN s_lfgja
*             AND hist~lfbnr IN s_lfbnr
*             AND hist~lfbnr IN s_lfbnr
*             AND mara~matkl IN s_matkl
*             AND mara~mtart IN s_mtart
*             AND mara~profl IN s_profl
*             AND ekko~bsart IN s_bsart
*             AND ekko~ekgrp IN s_ekgrp
*             AND ekko~lifnr IN r_lfa1_fr  "S_LIFNR
*             AND ekko~bukrs = p_bukrs
*             AND makt~spras = sy-langu.
*  ENDIF.

  DESCRIBE TABLE r_lfa1_us LINES sy-tabix.
  IF sy-tabix > 0.
    SELECT hist~belnr hist~bewtp hist~budat hist~cpudt hist~dmbtr
           hist~ebeln hist~ebelp hist~gjahr hist~lfbnr hist~lfgja
           hist~matnr hist~menge hist~shkzg
*           hist~reewr
           hist~dmbtr AS reewr
           hist~xblnr
           ekko~bsart ekko~bstyp ekko~ekgrp ekko~lifnr
           ekpo~peinh ekpo~uebtk ekpo~elikz ekpo~erekz ekpo~loekz
           mara~mtart mara~profl
           makt~maktx
    INTO CORRESPONDING FIELDS OF TABLE itab
    FROM  ( ekbe AS hist INNER JOIN mara
           ON mara~matnr = hist~matnr
           INNER JOIN ekko
           ON ekko~ebeln = hist~ebeln
           INNER JOIN ekpo
           ON ekpo~ebeln = hist~ebeln
           AND ekpo~ebelp = hist~ebelp )
           INNER JOIN makt
           ON makt~matnr = hist~matnr
          AND makt~spras = sy-langu
* BEGIN OF UD1K953713
           WHERE ( hist~werks EQ 'E001' OR hist~werks EQ 'P001' OR
                   hist~werks EQ 'E002')
*          WHERE ( hist~werks EQ 'E001' OR hist~werks EQ 'P001' )
* END OF UD1K953713
             AND hist~budat IN s_refdt
             AND hist~ebeln IN s_ebeln
             AND hist~ebelp IN s_ebelp
             AND hist~matnr IN s_matnr

             AND hist~bewtp IN r_bewtp

             AND hist~budat IN s_budat
             AND hist~cpudt IN s_cpudt
             AND hist~belnr IN s_belnr

             AND hist~belnr <> space

             AND hist~gjahr IN s_gjahr
             AND hist~lfgja IN s_lfgja
             AND hist~lfbnr IN s_lfbnr
             AND hist~lfbnr IN s_lfbnr
             AND mara~matkl IN s_matkl
             AND mara~mtart IN s_mtart
             AND mara~profl IN s_profl
             AND ekko~bsart IN s_bsart
             AND ekko~ekgrp IN s_ekgrp
             AND ekko~lifnr IN r_lfa1_us  "S_LIFNR
             AND ekko~bukrs = p_bukrs
            %_HINTS ORACLE 'FIRST_ROWS(10) index("MKPF" "MKPF~BUD")'.

*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_ekbe USING 'US'.
    ENDIF.
*- U1 End

  ENDIF.

  DESCRIBE TABLE r_lfa1_fr LINES sy-tabix.
  IF sy-tabix > 0.
    SELECT hist~belnr hist~bewtp hist~budat hist~cpudt hist~dmbtr
           hist~ebeln hist~ebelp hist~gjahr hist~lfbnr hist~lfgja
           hist~matnr hist~menge hist~shkzg
*           hist~reewr
           hist~dmbtr AS reewr
           hist~xblnr
           ekko~bsart ekko~bstyp ekko~ekgrp ekko~lifnr
           ekpo~peinh ekpo~uebtk ekpo~elikz ekpo~erekz ekpo~loekz
           mara~mtart mara~profl
           makt~maktx
    APPENDING CORRESPONDING FIELDS OF TABLE itab
    FROM  ( ekbe  AS hist INNER JOIN mara
           ON mara~matnr = hist~matnr
           INNER JOIN ekko
           ON ekko~ebeln = hist~ebeln
           INNER JOIN ekpo
           ON ekpo~ebeln = hist~ebeln
           AND ekpo~ebelp = hist~ebelp
           )
           INNER JOIN makt
           ON makt~matnr = hist~matnr
          AND makt~spras = sy-langu
* BEGIN OF UD1K953713
           WHERE ( hist~werks EQ 'E001' OR hist~werks EQ 'P001' OR
                   hist~werks EQ 'E002')
*          WHERE ( hist~werks EQ 'E001' OR hist~werks EQ 'P001' )
* END OF UD1K953713
             AND hist~budat IN s_refdt
             AND hist~ebeln IN s_ebeln
             AND hist~ebelp IN s_ebelp
             AND hist~matnr IN s_matnr
             AND hist~bewtp IN r_bewtp
             AND hist~budat IN s_budat
             AND hist~cpudt IN s_cpudt
             AND hist~belnr IN s_belnr
             AND hist~belnr <> space
             AND hist~gjahr IN s_gjahr
             AND hist~lfgja IN s_lfgja
             AND hist~lfbnr IN s_lfbnr
             AND hist~lfbnr IN s_lfbnr
             AND mara~matkl IN s_matkl
             AND mara~mtart IN s_mtart
             AND mara~profl IN s_profl
             AND ekko~bsart IN s_bsart
             AND ekko~ekgrp IN s_ekgrp
             AND ekko~lifnr IN r_lfa1_fr  "S_LIFNR
             AND ekko~bukrs = p_bukrs.

*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_ekbe USING 'FR'.
    ENDIF.
*- U1 End

  ENDIF.

ENDFORM.                    "select_current_history
*&---------------------------------------------------------------------*
*&      Form  select_past_history
*&---------------------------------------------------------------------*
FORM select_past_history.

*  DESCRIBE TABLE r_lfa1_us LINES sy-tabix.
*  IF sy-tabix > 0.
*    SELECT hist~belnr hist~bewtp hist~budat hist~cpudt hist~dmbtr
*           hist~ebeln hist~ebelp hist~gjahr hist~lfbnr hist~lfgja
*           hist~matnr hist~menge hist~shkzg hist~reewr
*           hist~xblnr
*           ekko~bsart ekko~bstyp ekko~ekgrp ekko~lifnr
*           ekpo~peinh ekpo~uebtk ekpo~elikz ekpo~erekz ekpo~loekz
*           mara~mtart mara~profl
*           makt~maktx
*           eina~infnr
*    APPENDING CORRESPONDING FIELDS OF TABLE itab
*    FROM ( ekbeh  AS hist
*           INNER JOIN mara
*           ON mara~matnr = hist~matnr
*           INNER JOIN ekko
*           ON ekko~ebeln = hist~ebeln
*           INNER JOIN ekpo
*           ON ekpo~ebeln = hist~ebeln
*           AND ekpo~ebelp = hist~ebelp
*           INNER JOIN eina
*           ON  eina~matnr = ekpo~matnr
*           AND eina~lifnr = ekko~lifnr
*           INNER JOIN makt
*           ON makt~matnr = hist~matnr
** UD1K941198 - by IG.MOON 08/01/2007 {
*           INNER JOIN mkpf AS c
*           ON  c~mblnr = hist~lfbnr
*           AND c~mjahr = hist~lfgja
** }
*           )
*           WHERE hist~ebeln IN s_ebeln
*             AND hist~ebelp IN s_ebelp
*             AND hist~matnr IN s_matnr
*             AND hist~bewtp IN r_bewtp
*             AND hist~budat IN s_budat
*             AND hist~cpudt IN s_cpudt
*             AND hist~belnr IN s_belnr
*             AND hist~belnr <> space
*             AND hist~gjahr IN s_gjahr
*             AND hist~lfgja IN s_lfgja
*             AND hist~lfbnr IN s_lfbnr
*             AND hist~lfbnr IN s_lfbnr
*             AND mara~matkl IN s_matkl
*             AND mara~mtart IN s_mtart
*             AND mara~profl IN s_profl
*             AND ekko~bsart IN s_bsart
*             AND ekko~ekgrp IN s_ekgrp
*             AND ekko~lifnr IN r_lfa1_us  "S_LIFNR
*             AND ekko~bukrs = p_bukrs
*             AND makt~spras = sy-langu
** UD1K941198 - by IG.MOON 08/01/2007 {
*             AND c~budat    IN s_refdt.
** }
*  ENDIF.
*
*  DESCRIBE TABLE r_lfa1_fr LINES sy-tabix.
*  IF sy-tabix > 0.
*    SELECT hist~belnr hist~bewtp hist~budat hist~cpudt hist~dmbtr
*           hist~ebeln hist~ebelp hist~gjahr hist~lfbnr hist~lfgja
*           hist~matnr hist~menge hist~shkzg hist~reewr
*           hist~xblnr
*           ekko~bsart ekko~bstyp ekko~ekgrp ekko~lifnr
*           ekpo~peinh ekpo~uebtk ekpo~elikz ekpo~erekz ekpo~loekz
*           mara~mtart mara~profl
*           makt~maktx
*           eina~infnr
*    APPENDING CORRESPONDING FIELDS OF TABLE itab
*    FROM ( ekbeh  AS hist
*           INNER JOIN mara
*           ON mara~matnr = hist~matnr
*           INNER JOIN ekko
*           ON ekko~ebeln = hist~ebeln
*           INNER JOIN ekpo
*           ON ekpo~ebeln = hist~ebeln
*           AND ekpo~ebelp = hist~ebelp
*           INNER JOIN eina
*           ON  eina~matnr = ekpo~matnr
*           AND eina~lifnr = ekko~lifnr
*           INNER JOIN makt
*           ON makt~matnr = hist~matnr
** UD1K941198 - by IG.MOON 08/01/2007 {
*           INNER JOIN mkpf AS c
*           ON  c~mblnr = hist~lfbnr
*           AND c~mjahr = hist~lfgja
** }
*           )
*           WHERE hist~ebeln IN s_ebeln
*             AND hist~ebelp IN s_ebelp
*             AND hist~matnr IN s_matnr
*             AND hist~bewtp IN r_bewtp
*             AND hist~budat IN s_budat
*             AND hist~cpudt IN s_cpudt
*             AND hist~belnr IN s_belnr
*             AND hist~belnr <> space
*             AND hist~gjahr IN s_gjahr
*             AND hist~lfgja IN s_lfgja
*             AND hist~lfbnr IN s_lfbnr
*             AND hist~lfbnr IN s_lfbnr
*             AND mara~matkl IN s_matkl
*             AND mara~mtart IN s_mtart
*             AND mara~profl IN s_profl
*             AND ekko~bsart IN s_bsart
*             AND ekko~ekgrp IN s_ekgrp
*             AND ekko~lifnr IN r_lfa1_fr  "S_LIFNR
*             AND ekko~bukrs = p_bukrs
*             AND makt~spras = sy-langu
** UD1K941198 - by IG.MOON 08/01/2007 {
*             AND c~budat    IN s_refdt.
** }
*  ENDIF.

  DESCRIBE TABLE r_lfa1_us LINES sy-tabix.

  IF sy-tabix > 0.

    SELECT hist~belnr hist~bewtp hist~budat hist~cpudt hist~dmbtr
           hist~ebeln hist~ebelp hist~gjahr hist~lfbnr hist~lfgja
           hist~matnr hist~menge hist~shkzg
*           hist~reewr
           hist~dmbtr AS reewr
           hist~xblnr
           ekko~bsart ekko~bstyp ekko~ekgrp ekko~lifnr
           ekpo~peinh ekpo~uebtk ekpo~elikz ekpo~erekz ekpo~loekz
           mara~mtart mara~profl
           makt~maktx
    APPENDING CORRESPONDING FIELDS OF TABLE itab
*    FROM  ( mkpf AS c INNER JOIN ekbeh  AS hist
*           ON  hist~lfbnr = c~mblnr
*           AND hist~lfgja = c~mjahr
    FROM  ( ekbeh  AS hist INNER JOIN mara
           ON mara~matnr = hist~matnr
           INNER JOIN ekko
           ON ekko~ebeln = hist~ebeln
           INNER JOIN ekpo
           ON ekpo~ebeln = hist~ebeln
           AND ekpo~ebelp = hist~ebelp )
           INNER JOIN makt
           ON makt~matnr = hist~matnr
          AND makt~spras = sy-langu

*           WHERE c~budat    IN s_refdt
           WHERE hist~budat  IN s_refdt
             AND hist~ebeln IN s_ebeln
             AND hist~ebelp IN s_ebelp
             AND hist~matnr IN s_matnr
             AND hist~bewtp IN r_bewtp
             AND hist~budat IN s_budat
             AND hist~cpudt IN s_cpudt
             AND hist~belnr IN s_belnr
             AND hist~belnr <> space
             AND hist~gjahr IN s_gjahr
             AND hist~lfgja IN s_lfgja
             AND hist~lfbnr IN s_lfbnr
             AND hist~lfbnr IN s_lfbnr
             AND mara~matkl IN s_matkl
             AND mara~mtart IN s_mtart
             AND mara~profl IN s_profl
             AND ekko~bsart IN s_bsart
             AND ekko~ekgrp IN s_ekgrp
             AND ekko~lifnr IN r_lfa1_us  "S_LIFNR
             AND ekko~bukrs = p_bukrs.

*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_ekbeh USING 'US'.
    ENDIF.
*- U1 End

  ENDIF.

  DESCRIBE TABLE r_lfa1_fr LINES sy-tabix.
  IF sy-tabix > 0.
    SELECT hist~belnr hist~bewtp hist~budat hist~cpudt hist~dmbtr
           hist~ebeln hist~ebelp hist~gjahr hist~lfbnr hist~lfgja
           hist~matnr hist~menge hist~shkzg
*           hist~reewr
           hist~dmbtr AS reewr
           hist~xblnr
           ekko~bsart ekko~bstyp ekko~ekgrp ekko~lifnr
           ekpo~peinh ekpo~uebtk ekpo~elikz ekpo~erekz ekpo~loekz
           mara~mtart mara~profl
           makt~maktx
    APPENDING CORRESPONDING FIELDS OF TABLE itab

*    FROM  ( mkpf AS c INNER JOIN ekbeh  AS hist
*           ON  hist~lfbnr = c~mblnr
*           AND hist~lfgja = c~mjahr
    FROM  ( ekbeh  AS hist INNER JOIN mara
           ON mara~matnr = hist~matnr
           INNER JOIN ekko
           ON ekko~ebeln = hist~ebeln
           INNER JOIN ekpo
           ON ekpo~ebeln = hist~ebeln
           AND ekpo~ebelp = hist~ebelp )
           INNER JOIN makt
           ON makt~matnr = hist~matnr
          AND makt~spras = sy-langu

           WHERE hist~budat IN s_refdt
             AND hist~ebeln IN s_ebeln
             AND hist~ebelp IN s_ebelp
             AND hist~matnr IN s_matnr
             AND hist~bewtp IN r_bewtp
             AND hist~budat IN s_budat
             AND hist~cpudt IN s_cpudt
             AND hist~belnr IN s_belnr
             AND hist~belnr <> space
             AND hist~gjahr IN s_gjahr
             AND hist~lfgja IN s_lfgja
             AND hist~lfbnr IN s_lfbnr
             AND hist~lfbnr IN s_lfbnr
             AND mara~matkl IN s_matkl
             AND mara~mtart IN s_mtart
             AND mara~profl IN s_profl
             AND ekko~bsart IN s_bsart
             AND ekko~ekgrp IN s_ekgrp
             AND ekko~lifnr IN r_lfa1_fr  "S_LIFNR
             AND ekko~bukrs = p_bukrs
            %_HINTS ORACLE 'FIRST_ROWS(10) index("MKPF" "MKPF~BUD")'.

* }
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_ekbeh USING 'FR'.
    ENDIF.
*- U1 End

  ENDIF.

ENDFORM.                    " select_past_history
*&---------------------------------------------------------------------*
*&      Form  get_asn
*&---------------------------------------------------------------------*
FORM get_asn.

  DATA: ls_borgr     LIKE  likp-borgr_grp.

  CLEAR it_mkpf.
  READ TABLE it_mkpf WITH KEY mblnr = itab-lfbnr
                              mjahr = itab-lfgja
                     BINARY SEARCH.

  IF sy-subrc <> 0.
    itab-refdt = itab-budat.
  ELSE.
    itab-refdt = it_mkpf-budat.

* get asn
    IF it_mkpf-xblnr(3) = 'JIS' OR it_mkpf-xblnr(3) = space.
    ELSE.
      itab-zvbeln  = it_mkpf-xblnr(10).
      SELECT SINGLE borgr_grp INTO ls_borgr
        FROM likp  WHERE vbeln = itab-zvbeln.
      itab-asn = ls_borgr.

*- U1 Start
      IF p_arch EQ 'X' AND sy-subrc <> 0.
        PERFORM archive_read_likp.
      ENDIF.
*- U1 End
    ENDIF.

* by ig.moon 8/28/2008 {
    itab-bktxt = it_mkpf-bktxt.
* }
  ENDIF.

  itab-yyyymm = itab-refdt(6).

ENDFORM.                    " get_asn
*&---------------------------------------------------------------------*
*&      Form  get_ref_date
*&---------------------------------------------------------------------*
*FORM GET_REF_DATE.
*  ITAB-REFDT = ITAB-BUDAT.
*  DATA: L_DATE LIKE MKPF-BUDAT.
*  SELECT SINGLE BUDAT INTO L_DATE
*      FROM MKPF
*      WHERE MJAHR = ITAB-LFGJA
*        AND MBLNR = ITAB-LFBNR.
*  IF SY-SUBRC = 0.
*    ITAB-REFDT = L_DATE.
*  ENDIF.
*  ITAB-YYYYMM = ITAB-REFDT(6).
*ENDFORM.                    " get_ref_date
*&---------------------------------------------------------------------*
*&      Form  get_due_date
*&---------------------------------------------------------------------*
FORM get_due_date.

  CHECK p_due = 'X'.

  DATA: l_belnr LIKE bkpf-belnr,
        l_awkey LIKE bkpf-awkey.

  CONCATENATE itab-belnr itab-gjahr INTO l_awkey.
  READ TABLE it_bkpf WITH KEY awkey = l_awkey BINARY SEARCH.
  itab-blart = it_bkpf-blart.

  SELECT SINGLE zfbdt augdt augbl
    INTO (itab-duedt, itab-clrdt, itab-augbl)
    FROM bsak
    WHERE bukrs = p_bukrs
      AND lifnr = itab-lifnr
      AND belnr = it_bkpf-belnr
      AND gjahr = itab-gjahr.
*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bsak.
  ENDIF.
*- U1 End

  IF sy-subrc <> 0.
    DATA: l_zbd1t LIKE bsik-zbd1t.
    SELECT SINGLE zfbdt zbd1t
     INTO (itab-duedt, l_zbd1t)
     FROM bsik
     WHERE bukrs = p_bukrs
       AND lifnr = itab-lifnr
      AND belnr = it_bkpf-belnr
       AND gjahr = itab-gjahr.
    itab-duedt = itab-duedt + l_zbd1t.
  ENDIF.

ENDFORM.                    " get_due_date
*&---------------------------------------------------------------------*
*&      Form  get_invoice_party
*&---------------------------------------------------------------------*
FORM get_invoice_party.

  READ TABLE it_rbkp WITH KEY belnr = itab-belnr
                              gjahr = itab-gjahr
                     BINARY SEARCH.

  CHECK sy-subrc EQ 0.

  IF it_rbkp-empfb IS INITIAL.
    itab-lifnr = it_rbkp-lifnr.
  ELSE.
    itab-lifnr = it_rbkp-empfb.
  ENDIF.

** Fuorng on 06/06/14 (
  itab-kidno = it_rbkp-kidno.
** )

ENDFORM.                    " get_invoice_party
*&---------------------------------------------------------------------*
*&      Form  get_info_price
*&---------------------------------------------------------------------*
FORM get_info_price.

  REFRESH it_knumh.
  CLEAR   it_knumh.

  SELECT knumh datab lifnr ekorg
    INTO CORRESPONDING FIELDS OF TABLE it_knumh
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  itab-matnr
     AND lifnr =  itab-lifnr
*      and ekorg =  c_ekorg
     AND esokz =  '0'
     AND datbi >=  itab-refdt   "Valid To
     AND datab <=  itab-refdt.  "Valid from

  CHECK sy-subrc = 0.

  READ TABLE it_knumh INDEX 1.

  CLEAR : itab-ifprc, itab-ifunt.
  SELECT SINGLE kbetr kpein INTO (itab-ifprc,itab-ifunt) FROM konp
   WHERE knumh = it_knumh-knumh
     AND kappl = 'M'
     AND kschl = 'PB00'.
*- U1 Start
  IF p_arch EQ 'X' AND sy-subrc <> 0.
    PERFORM archive_read_konp.
  ENDIF.
*- U1 End
  IF sy-subrc = 0.
*    itab-ifprc = konp-kbetr.
*    itab-ifunt = konp-kpein.
  ENDIF.

ENDFORM.                    " get_info_price
*&---------------------------------------------------------------------*
*&      Form  get_sa_price
*&---------------------------------------------------------------------*
FORM get_sa_price.

  DATA: l_knumh LIKE a018-knumh.

*GR date, Get SA price
  SELECT SINGLE knumh INTO l_knumh
    FROM a016
     WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND evrtn = itab-ebeln
     AND evrtp = itab-ebelp
     AND datbi >= itab-refdt
     AND datab <= itab-refdt.

  IF sy-subrc = 0.
* KONH, KONP
    SELECT SINGLE kbetr kpein INTO (itab-saprc, itab-saunt)
       FROM zvmm_info_condi
       WHERE knumh = l_knumh
         AND loevm_ko = ' '.

* if normal PO, get from order history
  ELSE.
*    SELECT SINGLE * FROM eina
*       WHERE lifnr = itab-lifnr
*         AND matnr = itab-matnr.
    SELECT SINGLE preis peinh INTO (itab-saprc, itab-saunt)
       FROM eipa
       WHERE infnr = itab-infnr
         AND ebeln = itab-ebeln
         AND ebelp = itab-ebelp.

    IF sy-subrc NE 0.

      SELECT SINGLE netpr peinh INTO (itab-saprc, itab-saunt)
         FROM ekpo
         WHERE ebeln = itab-ebeln
           AND ebelp = itab-ebelp.

*- U1 Start
      IF p_arch EQ 'X' AND sy-subrc <> 0.
        PERFORM archive_read_ekpo.
      ENDIF.
*- U1 End

    ENDIF.

    itab-sano = 'X'.
  ENDIF.

ENDFORM.                    " get_sa_price
*&---------------------------------------------------------------------*
*&      Form  collect_alv_tab
*&---------------------------------------------------------------------*
FORM collect_alv_tab.

  alv_tab-lifnr   = itab-lifnr.
  alv_tab-matnr   = itab-matnr.
  alv_tab-ebeln   = itab-ebeln.
  alv_tab-ebelp   = itab-ebelp.
  alv_tab-zmenge  = itab-zmenge.
  alv_tab-zdmbtr  = itab-zdmbtr.
  alv_tab-saval   = itab-saval.
  alv_tab-ifval   = itab-ifval.
  alv_tab-diffa   = itab-diffa.
  alv_tab-diffb   = itab-diffb.

  alv_tab-ivprc   = itab-ivprc.
  alv_tab-ifprc   = itab-ifprc.
  alv_tab-saprc   = itab-saprc.
  alv_tab-ifunt   = itab-ifunt.
  alv_tab-saunt   = itab-saunt.

  COLLECT alv_tab.

ENDFORM.                    " collect_alv_tab
*&---------------------------------------------------------------------*
*&      Form  get_detail_info
*&---------------------------------------------------------------------*
FORM get_detail_info.
  DATA: l_idx LIKE sy-tabix.

  DATA : $belnr LIKE itab-belnr,
         $gjahr LIKE itab-gjahr,
         $lifnr LIKE itab-lifnr.
  DATA : $itab LIKE itab OCCURS 1 WITH HEADER LINE.
  DATA : $$itab LIKE itab OCCURS 1 WITH HEADER LINE.
  DATA  $flag .
  DATA  $refdt LIKE itab-refdt.

  READ TABLE itab INDEX 1.
  IF sy-subrc EQ 0.
    $belnr = itab-belnr.
    $gjahr = itab-gjahr.
    $lifnr = itab-lifnr.
  ENDIF.

  SORT itab BY matnr ebeln ebelp bewtp gjahr belnr.

  LOOP AT itab.

    CLEAR  $flag .
    AT NEW belnr.
      $flag = 'X'.
    ENDAT.

    IF itab-bewtp EQ 'E'.
      CLEAR itab-belnr.
      CLEAR itab-belnr2.
    ENDIF.


    l_idx = sy-tabix.

    IF $belnr <> itab-belnr OR
       $gjahr <> itab-gjahr OR
       $lifnr <> itab-lifnr OR
       $flag EQ 'X'.
      PERFORM get_due_date.
      $itab = itab.
    ELSE.
      IF  p_due = 'X'.  " same in form get_due_date.
        itab-blart = $itab-blart.
        itab-duedt = $itab-duedt.
        itab-clrdt = $itab-clrdt.
        itab-augbl = $itab-augbl.
      ENDIF.
    ENDIF.

    IF NOT p_augdt IS INITIAL
    AND NOT itab-clrdt IS INITIAL
    AND p_augdt <> itab-clrdt.
      CONTINUE.
    ENDIF.

* UD1K941198 - by IG.MOON 08/01/2007 {
*    PERFORM get_ref_date.
* }

    PERFORM get_asn.
    CHECK itab-refdt IN s_refdt.

    PERFORM get_invoice_party.

    itab-zmenge = itab-menge.
    itab-zdmbtr = itab-reewr.
*---change sign of amount/qty
    IF itab-shkzg = 'H'.
      itab-zdmbtr = - itab-zdmbtr.
*...qty is positive in case of reval.
      IF itab-bewtp NA 'WN'.
        itab-zmenge = - itab-zmenge.
      ENDIF.
    ENDIF.

* IV unit$  (converted to SA unit)
    IF itab-menge <> 0.
      itab-ivprc = itab-peinh * itab-zdmbtr / itab-menge.
    ENDIF.

*...Reval/Subseq.
    IF itab-bewtp CA 'WN'.
      CLEAR itab-zmenge.
*-----ANDY
*     PERFORM GET_SA_PRICE.
      CLEAR: itab-saprc.

*...Normal IV
    ELSE.

      IF p_summ NE 'X' AND p_chkinv EQ 'X' AND p_chkinf EQ 'X'.

        IF p_diff = 'X'.

          IF $refdt NE itab-refdt OR $flag EQ 'X'.

            $refdt = itab-refdt.

            PERFORM get_info_price.
            PERFORM get_sa_price.

            $$itab = itab.

          ELSE.

            itab-ifprc = $$itab-ifprc.
            itab-ifunt = $$itab-ifunt.
            itab-saprc = $$itab-saprc.
            itab-saunt = $$itab-saunt.
            itab-sano  = $$itab-sano.

          ENDIF.

          itab-ifval = itab-zmenge * itab-ifprc / itab-ifunt.
          itab-saval = itab-zmenge * itab-saprc / itab-saunt.

          IF itab-shkzg = 'H'.
            itab-ifprc = - itab-ifprc.
            itab-saprc = - itab-saprc.
          ENDIF.

* Calc. Diff. (Reval=Diff,...)
          itab-diffa = itab-zdmbtr - itab-saval.
          itab-diffb = itab-saval  - itab-ifval.
        ENDIF.
      ENDIF.

    ENDIF.

*    IF p_summ = 'X'.
*    PERFORM collect_alv_tab.
*    ELSE.

    APPEND itab  TO alv_tab.
*    ENDIF.

    CLEAR itab.
  ENDLOOP.

  DATA $alv_tab LIKE alv_tab OCCURS 0 WITH HEADER LINE.
  DATA $tmp_tab LIKE alv_tab OCCURS 0 WITH HEADER LINE.

*  IF p_summ = 'X' OR p_chkinv EQ 'X' OR p_chkinf EQ 'X'.
*
*    SORT alv_tab BY lifnr matnr ebeln ebelp.

**    LOOP AT alv_tab.
**
**      IF alv_tab-ivprc <> 0.
**        MOVE-CORRESPONDING alv_tab TO itab_comp.
**        IF alv_tab-ivprc < 0.
**          itab_comp-$ivprc = - alv_tab-ivprc.
**        ELSE.
**          itab_comp-$ivprc = alv_tab-ivprc.
**        ENDIF.
**        COLLECT itab_comp.
**      ENDIF.
**
**      $alv_tab-lifnr   = alv_tab-lifnr.
**      $alv_tab-matnr   = alv_tab-matnr.
**      $alv_tab-ebeln   = alv_tab-ebeln.
**      $alv_tab-ebelp   = alv_tab-ebelp.
**      $alv_tab-saval   = alv_tab-saval.
**      $alv_tab-ifval   = alv_tab-ifval.
**      $alv_tab-diffa   = alv_tab-diffa.
**      $alv_tab-diffb   = alv_tab-diffb.
**      $alv_tab-lfbnr   = alv_tab-lfbnr.
**
**      $alv_tab-zmenge  = alv_tab-zmenge.
**      $alv_tab-zdmbtr  = alv_tab-zdmbtr.
**      $alv_tab-ivprc   = alv_tab-ivprc.
**      $alv_tab-ifprc   = alv_tab-ifprc.
**      $alv_tab-saprc   = alv_tab-saprc.
**      $alv_tab-ifunt   = alv_tab-ifunt.
**      $alv_tab-saunt   = alv_tab-saunt.
**
**      COLLECT $alv_tab.
**
**    ENDLOOP.
**
**    DATA : $cnt TYPE i,
**           $fr  TYPE i.
**    DATA $ix TYPE i.
**
**
*** {
**
**    DO 2 TIMES.
**      __cls $itab_comp.
**
**      LOOP AT itab_comp.
**        $itab_comp = itab_comp.
**        CLEAR $itab_comp-$ivprc.
**        COLLECT $itab_comp.
**      ENDLOOP.
**
**      SORT itab_comp BY matnr lfbnr.
**
**      LOOP AT $itab_comp.
**        $ix = sy-tabix.
**        READ TABLE itab_comp WITH KEY matnr = $itab_comp-matnr
**                                      lfbnr = $itab_comp-lfbnr
**                                      ivprc = $itab_comp-ivprc.
***                                  BINARY SEARCH.
**        IF sy-subrc EQ 0.
**          $itab_comp-ivprc = itab_comp-$ivprc.
**          $itab_comp-$ivprc = itab_comp-$ivprc.
**          MODIFY $itab_comp INDEX $ix.
**        ENDIF.
**      ENDLOOP.
**
**      LOOP AT $itab_comp.
**        $ix = sy-tabix.
**        IF $itab_comp-$ivprc IS INITIAL.
**          $itab_comp-$ivprc = $itab_comp-ivprc.
**          modify $itab_comp index $ix transporting $ivprc.
**        ENDIF.
**      ENDLOOP.
**
**      __cls itab_comp.
**      itab_comp[] = $itab_comp[].
**    ENDDO.
*** }
**
**    DATA $ivprc LIKE itab_comp-$ivprc.
**
**    SORT itab_comp BY matnr lfbnr.
**    LOOP AT $alv_tab.
**      $ix = sy-tabix.
**      CLEAR $cnt.
**      READ TABLE itab_comp WITH KEY matnr = $alv_tab-matnr
**                                    lfbnr = $alv_tab-lfbnr
**                                BINARY SEARCH.
**      IF sy-subrc EQ 0.
**        $ivprc = itab_comp-$ivprc.
**        $fr = sy-tabix.
**        LOOP AT itab_comp FROM $fr.
**          IF itab_comp-matnr NE $alv_tab-matnr OR
**             itab_comp-lfbnr NE $alv_tab-lfbnr.
**            EXIT.
**          ENDIF.
**          ADD 1 TO $cnt.
**        ENDLOOP.
**        IF $cnt EQ 1.
**          $alv_tab-ivprc = $ivprc.
**        ENDIF.
**
**        MODIFY $alv_tab INDEX $ix TRANSPORTING  ivprc.
**      ENDIF.
**
**    ENDLOOP.
**
**    SORT alv_tab BY lifnr matnr ebeln ebelp lfbnr.
**    LOOP AT $alv_tab.
**      $ix = sy-tabix.
**      READ TABLE alv_tab WITH KEY lifnr = $alv_tab-lifnr
**                                  matnr = $alv_tab-matnr
**                                  ebeln = $alv_tab-ebeln
**                                  ebelp = $alv_tab-ebelp
**                                  lfbnr = $alv_tab-lfbnr
**                                BINARY SEARCH.
**      IF sy-subrc EQ 0.
**        $fr = sy-tabix.
**        CLEAR : $tmp_tab[], $tmp_tab.
**        LOOP AT alv_tab FROM $fr.
**          IF alv_tab-lifnr NE $alv_tab-lifnr OR
**             alv_tab-matnr NE $alv_tab-matnr OR
**             alv_tab-ebeln NE $alv_tab-ebeln OR
**             alv_tab-ebelp NE $alv_tab-ebelp OR
**             alv_tab-lfbnr NE $alv_tab-lfbnr.
**            EXIT.
**          ENDIF.
**          $tmp_tab-lfbnr  = alv_tab-lfbnr .
**          $tmp_tab-belnr1 = alv_tab-belnr1.
**          $tmp_tab-refdt  = alv_tab-refdt .
**
***          $tmp_tab-budat  = alv_tab-budat .
***          $tmp_tab-BELNR2 = alv_tab-belnr2.
**          COLLECT  $tmp_tab.
**        ENDLOOP.
**        DESCRIBE TABLE $tmp_tab LINES $cnt.
**        IF $cnt EQ 1.
**          $alv_tab-lfbnr  = $tmp_tab-lfbnr .
**          $alv_tab-belnr1 = $tmp_tab-belnr1.
**          $alv_tab-refdt  = $tmp_tab-refdt .
**
***          $alv_tab-budat  = $tmp_tab-budat .
***          $alv_tab-BELNR2 = $tmp_tab-belnr2.
**        ELSE.
**          SORT $tmp_tab BY refdt DESCENDING.
**          READ TABLE $tmp_tab INDEX 1.
**          $alv_tab-lfbnr  = $tmp_tab-lfbnr .
**          $alv_tab-belnr1 = $tmp_tab-belnr1.
**          $alv_tab-refdt  = $tmp_tab-refdt .
**
**        ENDIF.
**
**       MODIFY $alv_tab INDEX $ix TRANSPORTING lfbnr belnr1 refdt
**belnr2
**.
**      ENDIF.
**
**    ENDLOOP.
**
**    CLEAR : alv_tab[], alv_tab.
**    alv_tab[] = $alv_tab[].

*    LOOP AT alv_tab.
*      $ix = sy-tabix.
*
*      MOVE-CORRESPONDING alv_tab TO itab.
*
*      PERFORM get_info_price.
*      PERFORM get_sa_price.
*
*      alv_tab-ifprc = itab-ifprc.
*      alv_tab-ifunt = itab-ifunt.
*      alv_tab-saprc = itab-saprc.
*      alv_tab-saunt = itab-saunt.
*      alv_tab-sano  = itab-sano.
*
*      alv_tab-ifval = alv_tab-zmenge * alv_tab-ifprc / alv_tab-ifunt.
*      alv_tab-saval = alv_tab-zmenge * alv_tab-saprc / alv_tab-saunt.
*
*      IF alv_tab-shkzg = 'H'.
*        alv_tab-ifprc = - alv_tab-ifprc.
*        alv_tab-saprc = - alv_tab-saprc.
*      ENDIF.
*
** Calc. Diff. (Reval=Diff,...)
*      alv_tab-diffa = alv_tab-zdmbtr - alv_tab-saval.
*      alv_tab-diffb = alv_tab-saval  - alv_tab-ifval.
*
*      MODIFY alv_tab INDEX $ix.
*    ENDLOOP.

*  ENDIF.

  IF p_chkinv EQ 'X' OR p_chkinf EQ 'X' OR p_summ EQ 'X'.
    PERFORM sum_alv_tab.

    LOOP AT alv_tab.
      $ix = sy-tabix.

      MOVE-CORRESPONDING alv_tab TO itab.

      PERFORM get_info_price.
      PERFORM get_sa_price.
      alv_tab-ifprc = itab-ifprc.
      alv_tab-ifunt = itab-ifunt.
      alv_tab-saprc = itab-saprc.
      alv_tab-saunt = itab-saunt.
      alv_tab-sano  = itab-sano.

      alv_tab-ifval = alv_tab-zmenge * alv_tab-ifprc / alv_tab-ifunt.
      alv_tab-saval = alv_tab-zmenge * alv_tab-saprc / alv_tab-saunt.

      IF alv_tab-shkzg = 'H'.
        alv_tab-ifprc = - alv_tab-ifprc.
        alv_tab-saprc = - alv_tab-saprc.
      ENDIF.

* Calc. Diff. (Reval=Diff,...)
      alv_tab-diffa = alv_tab-zdmbtr - alv_tab-saval.
      alv_tab-diffb = alv_tab-saval  - alv_tab-ifval.

      IF alv_tab-saprc < 0.
        alv_tab-saprc = -1 * alv_tab-saprc.
      ENDIF.

      IF  alv_tab-ifprc < 0.
        alv_tab-ifprc = -1 * alv_tab-ifprc.
      ENDIF.

      MODIFY alv_tab INDEX $ix.
    ENDLOOP.

  ENDIF.

  LOOP AT alv_tab.

    $ix = sy-tabix.

    IF p_chkinf EQ 'X'.
      IF alv_tab-saprc <> alv_tab-ifprc.
        alv_tab-mismatch = 'A'.
      ENDIF.
    ENDIF.

    IF p_chkinv EQ 'X'.
      IF alv_tab-ivprc <> alv_tab-saprc.
        alv_tab-mismatch = 'B'.
      ENDIF.
    ENDIF.

    IF NOT alv_tab-mismatch IS INITIAL.
      MODIFY alv_tab INDEX $ix TRANSPORTING mismatch.
    ELSE.
      IF p_chkinv EQ 'X' OR p_chkinf EQ 'X'.
        DELETE alv_tab INDEX $ix.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF NOT p_augdt IS INITIAL.
    DELETE alv_tab WHERE clrdt NE p_augdt.
  ENDIF.
ENDFORM.                    " get_detail_info
*&---------------------------------------------------------------------*
*&      Form  select_mkpf_ref
*&---------------------------------------------------------------------*
FORM select_mkpf_ref.
  DATA: BEGIN OF lt_itab OCCURS 0,
          lfbnr  TYPE lfbnr,
          lfgja  TYPE lfgja,
        END OF lt_itab.
  DATA: BEGIN OF lt_itab2 OCCURS 0,
          belnr  TYPE lfbnr,
          gjahr  TYPE lfgja,
        END OF lt_itab2.

  DATA $ix TYPE i.
  DATA $key TYPE awkey.

  LOOP AT itab.
    $ix = sy-tabix.
    lt_itab-lfbnr = itab-lfbnr.
    lt_itab-lfgja = itab-lfgja.
    APPEND lt_itab.
    lt_itab2-gjahr = itab-gjahr.
    lt_itab2-belnr = itab-belnr.
    APPEND lt_itab2.
    CONCATENATE itab-lfbnr itab-gjahr INTO $key.
    SELECT SINGLE belnr INTO itab-belnr1 FROM bkpf
    WHERE awkey EQ $key.
*- U1 Start
    IF p_arch EQ 'X' AND sy-subrc <> 0.
      PERFORM archive_read_bkpf USING $key CHANGING itab-belnr1.
    ENDIF.
*- U1 End
    CONCATENATE itab-belnr itab-gjahr INTO $key.
    SELECT SINGLE belnr INTO itab-belnr2 FROM bkpf
    WHERE awkey EQ $key.
*- U1 Start
    IF p_arch EQ 'X' AND sy-subrc <> 0.
      PERFORM archive_read_bkpf USING $key CHANGING itab-belnr2.
    ENDIF.
*- U1 End
    MODIFY itab INDEX $ix TRANSPORTING belnr1 belnr2.
  ENDLOOP.


  SORT lt_itab  BY lfgja lfbnr.
  SORT lt_itab2 BY gjahr belnr.
  DELETE ADJACENT DUPLICATES FROM lt_itab.
  DELETE ADJACENT DUPLICATES FROM lt_itab2.

  SELECT mblnr mjahr xblnr budat bktxt
    INTO TABLE it_mkpf
    FROM mkpf
    FOR ALL ENTRIES IN lt_itab
    WHERE mblnr = lt_itab-lfbnr
      AND mjahr = lt_itab-lfgja.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_mkpf TABLES lt_itab.
  ENDIF.
*- U1 End

  SORT it_mkpf BY mblnr mjahr.


  SELECT belnr gjahr lifnr empfb
    kidno
    INTO TABLE it_rbkp
    FROM rbkp
    FOR ALL ENTRIES IN lt_itab2
    WHERE belnr = lt_itab2-belnr
      AND gjahr = lt_itab2-gjahr.

  IF p_arch EQ 'X'.
    PERFORM archive_read_rbkp TABLES lt_itab2.
  ENDIF.
*- U1 End

  SORT it_rbkp BY belnr gjahr.

ENDFORM.                    " select_mkpf_ref
*&---------------------------------------------------------------------*
*&      Form  prepare_alv
*&---------------------------------------------------------------------*
FORM prepare_alv.
  IF p_short = 'X'.
    PERFORM field_setting TABLES gt_fieldcat USING :
   'BEWTP'     'Category'       '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'YYYYMM'    'Ref.yymm'       '10'  ' '  'l'  ' '  ' '  ' '  ' '  ' ',
   'MATNR'     'Material'       '18'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'MAKTX'     'Mat.desc'       '25'  ' '  'l'  ' '  ' '  ' '  ' '  ' ',
   'REFDT'     'Ref.date'       '10'  ' '  'l'  ' '  ' '  ' '  ' '  ' ',
   'BUDAT'     'PstDate'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ASN'       'ASN#'           '30'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'GJAHR'     'InvYear'        '04'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BELNR'     'InvDoc#'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BELNR2'    'Act#(IV)'       '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EBELN'     'PO/SA'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EBELP'     'Itm'            '06'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ZMENGE'    'Quantity'       '14'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'ZDMBTR'    'Invoice Amt'    '16'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'IVPRC'     'IV Price$'      '10'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'PEINH'     'Price unit'     '03'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'DUEDT'     'DueDt'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'CLRDT'     'ClrDt'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'LFBNR'     'Ref.doc#'       '10'  ' '  'l'  ' '  ' '  ' '  ' '  ' ',
   'BELNR1'    'Act#(GR)'       '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BKTXT'   'Doc.header text'  '25'  ' '  'l'  ' '  ' '  ' '  ' '  ' '.

  ELSE.
    PERFORM field_setting TABLES gt_fieldcat USING :
   'MATNR'     'Material'       '18'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EBELN'     'PO/SA'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EBELP'     'Itm'            '06'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BEWTP'     'Category'       '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'GJAHR'     'InvYear'        '04'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BELNR'     'InvDoc#'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BELNR2'    'Act#(IV)'       '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BUDAT'     'PstDate'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'CPUDT'     'CPUDate'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'SHKZG'     'D/C'            '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
*'MENGE'     'Qty'            '14'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
*'DMBTR'     'Amt'            '16'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'LFGJA'     'RefYear'        '04'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'LFBNR'     'RefDoc#'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BELNR1'    'Act#(GR)'       '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BSART'     'PO Type'        '04'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'BSTYP'     'PO Cat'         '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EKGRP'     'PO grp'         '03'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'LIFNR'     'Vendor'         '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'PEINH'     'PrcUnit'        '03'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
*INFNR'     'Inf No'         '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'REFDT'     'Ref.date'       '10'  ' '  'l'  ' '  ' '  ' '  ' '  ' ',
   'YYYYMM'    'Ref.yymm'       '10'  ' '  'l'  ' '  ' '  ' '  ' '  ' ',
   'ZVBELN'    'Inbound'        '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ASN'       'ASN#'           '30'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ZMENGE'    'Quantity'       '14'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'ZDMBTR'    'Invoice Amt'    '16'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'IVPRC'     'IV Price$'      '10'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'PEINH'     'Price unit'     '03'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
   'UEBTK'     'Unlimited GR'   '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'ELIKZ'     'Delivery compl' '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'EREKZ'     'Final IV'       '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'LOEKZ'     'Del'            '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'MTART'     'MatType'        '04'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'PROFL'     'Profile'        '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'MAKTX'     'Mat.desc'       '25'  ' '  'l'  ' '  ' '  ' '  ' '  ' ',
   'BKTXT'   'Doc.header text'  '25'  ' '  'l'  ' '  ' '  ' '  ' '  ' '.

*    IF p_diff = 'X' OR p_chkms EQ 'X'.
    PERFORM field_setting TABLES gt_fieldcat USING :
 'SAPRC'     'SA Price$'      '10'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
 'SAUNT'     'SA UNIT'        '03'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
 'SAVAL'     'SA VALUE'       '16'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
 'IFPRC'     'INFO Price$'    '10'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
 'IFUNT'     'INFO UNIT'      '03'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
 'IFVAL'     'INFO VALUE'     '16'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
 'DIFFA'     'IV-SA'          '14'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
 'DIFFB'     'SA-INFO'        '14'  ' '  'R'  ' '  ' '  ' '  ' '  ' ',
 'SANO'      'NO SA Price'    '01'  ' '  'R'  ' '  ' '  ' '  ' '  ' '.
*    ENDIF.
    IF p_due = 'X'.
      PERFORM field_setting TABLES gt_fieldcat USING :
   'BLART'     'Doc.typ'        '02'  ' '  'l'  ' '  ' '  ' '  ' '  ' ',
   'ZBELNR'    'Acct.doc'       '10'  ' '  'l'  ' '  ' '  ' '  ' '  ' ',
   'DUEDT'     'DueDt'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'CLRDT'     'ClrDt'          '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' ',
   'AUGBL'     'ClrDoc'         '10'  ' '  'L'  ' '  ' '  ' '  ' '  ' '.
    ENDIF.
  ENDIF.
** Fuorng on 06/06/14 (
  PERFORM field_setting TABLES gt_fieldcat USING :
  'KIDNO'     'Payment Ref'       '12'  ' '  'l'  ' '  ' '  ' '  ' '  ' '.
** )
  PERFORM field_setting TABLES gt_fieldcat USING :
'MISMATCH'   'Mismatch'   '01'  ' '  'L'  ' '  ' '  ' '  ' '  ' '.


ENDFORM.                    " prepare_alv
*&---------------------------------------------------------------------*
*&      Form  show_alv
*&---------------------------------------------------------------------*
FORM show_alv.

  g_repid = sy-repid.
  l_variant-variant = p_vari.
  l_variant-report  = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
      it_fieldcat        = gt_fieldcat
      is_variant         = l_variant
      i_save             = 'A'
    TABLES
      t_outtab           = alv_tab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " show_alv
*&---------------------------------------------------------------------*
*&      Form  SELECT_BKPF_REF
*&---------------------------------------------------------------------*
FORM select_bkpf_ref.

  CHECK p_due = 'X'.

  DATA: BEGIN OF lt_itab OCCURS 0,
          awkey  TYPE awkey,
        END OF lt_itab.

  LOOP AT itab.
    CONCATENATE itab-belnr itab-gjahr INTO lt_itab-awkey.
    APPEND lt_itab.
  ENDLOOP.
  SORT lt_itab  BY awkey.
  DELETE ADJACENT DUPLICATES FROM lt_itab.

  SELECT awkey belnr blart
    INTO TABLE it_bkpf
    FROM bkpf
    FOR ALL ENTRIES IN lt_itab
    WHERE awkey = lt_itab-awkey
      AND bukrs = p_bukrs.

  IF p_arch EQ 'X'.
*    PERFORM archive_read_bkpf USING $key CHANGING itab-belnr1.

    PERFORM archive_read_bkpf_2 TABLES lt_itab.
  ENDIF.
*- U1 End

  SORT it_bkpf BY awkey.

ENDFORM.                    " SELECT_BKPF_REF
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES         p_fieldcat_t LIKE gt_fieldcat
                   USING          p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO p_fieldcat_t.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1      = p_text
      textline2      = p_text2
      titel          = 'Check!'
      cancel_display = p_canc
    IMPORTING
      answer         = p_answer.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  select_vendors
*&---------------------------------------------------------------------*
FORM select_vendors.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.

  SELECT lifnr land1 INTO TABLE gt_lfa1
     FROM lfa1
     WHERE lifnr IN s_lifnr
** Furong on 08/12/14 fix the dump
       AND ( ktokk = 'Y020' OR  ktokk = 'Y030' OR   ktokk = 'Y040'
             OR ktokk = 'Y050' OR  ktokk = 'Y060' ).
** )

  r_lfa1_fr-sign   = 'I'.
  r_lfa1_fr-option = 'EQ'.
  r_lfa1_us-sign   = 'I'.
  r_lfa1_us-option = 'EQ'.

* NAFTA countries...
  LOOP AT gt_lfa1.
    CASE gt_lfa1-land1.
      WHEN t001-land1.
        r_lfa1_us-low = gt_lfa1-lifnr.
        APPEND r_lfa1_us.
      WHEN 'CA' OR 'MX'.
        r_lfa1_us-low = gt_lfa1-lifnr.
        APPEND r_lfa1_us.
      WHEN OTHERS.
        r_lfa1_fr-low = gt_lfa1-lifnr.
        APPEND r_lfa1_fr.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " select_vendors
*&---------------------------------------------------------------------*
*&      Form  sum_alv_tab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sum_alv_tab.

  DATA $alv_tab LIKE alv_tab OCCURS 0 WITH HEADER LINE.
  DATA $ix TYPE i.

  LOOP AT alv_tab.

    IF alv_tab-zmenge EQ 0.
      alv_tab-ivprc = 0.
      alv_tab-saprc = 0.
      alv_tab-ifprc = 0.
    ENDIF.

    $alv_tab = alv_tab.
    CLEAR :
*        $alv_tab-matnr,
*        $alv_tab-ebeln,
        $alv_tab-ebelp,
        $alv_tab-bewtp,
        $alv_tab-gjahr,
        $alv_tab-belnr,
        $alv_tab-budat,
        $alv_tab-cpudt,
        $alv_tab-shkzg,
        $alv_tab-menge,
        $alv_tab-dmbtr,
        $alv_tab-reewr,
        $alv_tab-lfgja,
        $alv_tab-xblnr,
        $alv_tab-bsart,
        $alv_tab-bstyp,
        $alv_tab-ekgrp,
        $alv_tab-lifnr,
        $alv_tab-peinh,
        $alv_tab-uebtk,
        $alv_tab-elikz,
        $alv_tab-erekz,
        $alv_tab-loekz,
        $alv_tab-mtart,
        $alv_tab-profl,
        $alv_tab-maktx,
        $alv_tab-infnr,
*        $alv_tab-refdt,
        $alv_tab-yyyymm,
        $alv_tab-zvbeln,
        $alv_tab-asn,
        $alv_tab-zbelnr,
        $alv_tab-duedt,
        $alv_tab-clrdt,
        $alv_tab-augbl,
        $alv_tab-blart,
        $alv_tab-belnr1,
        $alv_tab-belnr2,
        $alv_tab-saunt,
        $alv_tab-sano,
        $alv_tab-ifunt.
    COLLECT $alv_tab.
  ENDLOOP.


  SORT $alv_tab BY matnr ebeln lfbnr refdt.
  SORT alv_tab BY matnr ebeln lfbnr budat refdt.

  DELETE ADJACENT DUPLICATES FROM alv_tab COMPARING matnr ebeln
  lfbnr refdt.

  LOOP AT alv_tab.
    $ix = sy-tabix.
    CLEAR $alv_tab.
    READ TABLE $alv_tab WITH KEY matnr = alv_tab-matnr
                                 ebeln = alv_tab-ebeln
                                 lfbnr = alv_tab-lfbnr
                                 refdt = alv_tab-refdt
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.
      alv_tab-zmenge = $alv_tab-zmenge.
      alv_tab-zdmbtr = $alv_tab-zdmbtr.
      IF $alv_tab-zmenge <> 0.
        alv_tab-ivprc = $alv_tab-zdmbtr / $alv_tab-zmenge.
      ELSE.
        alv_tab-ivprc = 0."$alv_tab-ivprc.
      ENDIF.
      alv_tab-saprc = $alv_tab-saprc.
      alv_tab-saval = $alv_tab-saval.
      alv_tab-ifprc = $alv_tab-ifprc.
      alv_tab-ifval = $alv_tab-ifval.
      alv_tab-diffa = $alv_tab-diffa.
      alv_tab-diffb = $alv_tab-diffb.
      MODIFY alv_tab INDEX $ix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " sum_alv_tab
*&---------------------------------------------------------------------*
*&      Form  button_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM button_.

  IF par_cb1 EQ 'X'.
    WRITE:
           icon_collapse AS ICON TO timpr,
          'Further selections' TO timpr+4(18).
  ELSE.
    WRITE:
           icon_expand AS ICON TO timpr,
          'Further selections' TO timpr+4(18).
  ENDIF.

ENDFORM.                    " button_
*&---------------------------------------------------------------------*
*&      Form  modi_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modi_screen.

  LOOP AT SCREEN.
    IF screen-group1 = 'PLL'.
      IF  par_cb1 EQ 'X'.
        screen-active = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM button_.

ENDFORM.                    " modi_screen
*&---------------------------------------------------------------------*
*&      Form  filter_gr_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filter_gr_list.

  DATA $itab LIKE itab OCCURS 0 WITH HEADER LINE.
  DATA $fr TYPE i.

  SORT itab BY matnr lfbnr.

  $itab[] = itab[].


  LOOP AT itab.
    IF itab-bewtp EQ 'E'.
      READ TABLE $itab WITH KEY matnr = itab-matnr
                                lfbnr = itab-lfbnr
                                BINARY SEARCH.
      IF sy-subrc EQ 0.
        $fr = sy-tabix.
        LOOP AT $itab FROM $fr.
          IF $itab-lfbnr NE itab-lfbnr.
            EXIT.
          ENDIF.
          IF $itab-bewtp NE 'E'.
            DELETE itab WHERE matnr = $itab-matnr
                          AND lfbnr = $itab-lfbnr.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " filter_gr_list
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKBE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ekbe USING p_flag TYPE char2.

  TYPES: BEGIN OF ty_ekbe,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
         zekkn TYPE dzekkn,
         vgabe TYPE vgabe,
         gjahr TYPE mjahr,
         belnr TYPE mblnr,
         buzei TYPE mblpo,
         werks TYPE werks_d,
         budat TYPE budat,
         bwart TYPE bwart,
         matnr TYPE matnr,
         bewtp TYPE bewtp,
         lifnr TYPE elifn,
         bukrs TYPE bukrs,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekbe.

  DATA: l_handle    TYPE sytabix,
        lt_ekbe     TYPE TABLE OF ekbe WITH HEADER LINE,
        lt_ekko     TYPE TABLE OF ekko WITH HEADER LINE,
        lt_ekpo     TYPE TABLE OF ekpo WITH HEADER LINE,
        lt_mara     TYPE TABLE OF mara WITH HEADER LINE,
        lt_makt     TYPE TABLE OF makt WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekbe TYPE TABLE OF ty_ekbe,
        ls_inx_ekbe TYPE ty_ekbe.

  RANGES: lr_lifnr FOR lfa1-lifnr.

  CLEAR lr_lifnr.
  IF p_flag = 'US'.
    lr_lifnr[] = r_lfa1_us[].
  ELSEIF p_flag = 'FR'.
    lr_lifnr[] = r_lfa1_fr[].
  ENDIF.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKBE_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ekbe[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_inx_ekbe
    FROM (l_gentab)
   WHERE ( werks EQ 'E001' OR werks EQ 'P001' OR
           werks EQ 'E002')
     AND budat IN s_refdt
     AND ebeln IN s_ebeln
     AND ebelp IN s_ebelp
     AND matnr IN s_matnr
     AND bewtp IN r_bewtp
     AND budat IN s_budat
     AND belnr IN s_belnr
     AND belnr <> space
     AND gjahr IN s_gjahr
     AND lifnr IN r_lfa1_us
     AND bukrs = p_bukrs.

  CHECK NOT lt_inx_ekbe[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_ekko_a, gt_ekko_a[],gt_ekpo_a, gt_ekpo_a[], gt_ekbe_a, gt_ekbe_a[].
  LOOP AT lt_inx_ekbe INTO ls_inx_ekbe.
*  4.1 Read information from archivekey & offset
    CLEAR: lt_ekko, lt_ekko[], lt_ekpo, lt_ekpo[], lt_ekbe, lt_ekbe[].
    CALL FUNCTION 'ASH_MM_EKKO_READ'
      EXPORTING
        i_archivekey           = ls_inx_ekbe-archivekey
        i_offset               = ls_inx_ekbe-archiveofs
      TABLES
        et_ekko                = lt_ekko
        et_ekpo                = lt_ekpo
        et_ekbe                = lt_ekbe
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK sy-subrc = 0.

    CHECK NOT lt_ekko[] IS INITIAL.
    CHECK NOT lt_ekpo[] IS INITIAL.
    CHECK NOT lt_ekbe[] IS INITIAL.

    DELETE lt_ekbe WHERE cpudt NOT IN s_cpudt
                      OR lfgja NOT IN s_lfgja
                      OR lfbnr NOT IN s_lfbnr.

    DELETE lt_ekko WHERE bsart NOT IN s_bsart
                      OR ekgrp NOT IN s_ekgrp.

    LOOP AT lt_ekbe WHERE ebeln EQ ls_inx_ekbe-ebeln
                    AND   ebelp EQ ls_inx_ekbe-ebelp
                    AND   zekkn EQ ls_inx_ekbe-zekkn
                    AND   vgabe EQ ls_inx_ekbe-vgabe
                    AND   gjahr EQ ls_inx_ekbe-gjahr
                    AND   belnr EQ ls_inx_ekbe-belnr
                    AND   buzei EQ ls_inx_ekbe-buzei.
      gt_ekbe_a = lt_ekbe.
      APPEND gt_ekbe_a.
      CLEAR: gt_ekbe_a,
             lt_ekbe.
    ENDLOOP.
*
* 5. Append archived data table to finally interal table
    INSERT LINES OF: lt_ekko INTO TABLE gt_ekko_a,
                     lt_ekpo INTO TABLE gt_ekpo_a.
*                     LT_EKBE INTO TABLE GT_EKBE_A.
  ENDLOOP.

  CHECK NOT gt_ekko_a[] IS INITIAL.
  CHECK NOT gt_ekpo_a[] IS INITIAL.
  CHECK NOT gt_ekbe_a[] IS INITIAL.

  lt_ekbe[] = gt_ekbe_a[].
  SORT lt_ekbe BY matnr.
  DELETE ADJACENT DUPLICATES FROM: lt_ekbe COMPARING matnr.

*
*  SORT: GT_EKKO_A, GT_EKPO_A, GT_EKBE_A.
*  DELETE ADJACENT DUPLICATES FROM: GT_EKKO_A COMPARING ALL FIELDS,
*                                   GT_EKPO_A COMPARING ALL FIELDS,
*                                   GT_EKBE_A COMPARING ALL FIELDS.

* mara
  CLEAR: lt_mara, lt_mara[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN lt_ekbe
    WHERE matnr = lt_ekbe-matnr
     AND matkl IN s_matkl
     AND mtart IN s_mtart
     AND profl IN s_profl.

* makt
  CLEAR: lt_makt, lt_makt[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_makt
    FROM makt
    FOR ALL ENTRIES IN lt_mara
   WHERE matnr = lt_mara-matnr
     AND spras = sy-langu.

  CLEAR itab.
  LOOP AT gt_ekbe_a.
    itab-belnr = gt_ekbe_a-belnr.
    itab-bewtp = gt_ekbe_a-bewtp.
    itab-budat = gt_ekbe_a-budat.
    itab-cpudt = gt_ekbe_a-cpudt.
    "itab-dmbtr = gt_ekbe_a-dmbtr.  hist~dmbtr AS reewr
    itab-reewr = gt_ekbe_a-dmbtr.
    itab-ebeln = gt_ekbe_a-ebeln.
    itab-ebelp = gt_ekbe_a-ebelp.
    itab-gjahr = gt_ekbe_a-gjahr.
    itab-lfbnr = gt_ekbe_a-lfbnr.
    itab-lfgja = gt_ekbe_a-lfgja.
    itab-matnr = gt_ekbe_a-matnr.
    itab-menge = gt_ekbe_a-menge.
    itab-shkzg = gt_ekbe_a-shkzg.

    CLEAR lt_mara.
    READ TABLE lt_mara WITH KEY matnr = gt_ekbe_a-matnr.
    CHECK sy-subrc = 0.
    itab-mtart = lt_mara-mtart.
    itab-profl = lt_mara-profl.

    CLEAR lt_ekko.
    READ TABLE gt_ekko_a WITH KEY ebeln = gt_ekbe_a-ebeln.
    CHECK sy-subrc = 0.
    itab-bsart = lt_ekko-bsart.
    itab-bstyp = lt_ekko-bstyp.
    itab-ekgrp = lt_ekko-ekgrp.
    itab-lifnr = lt_ekko-lifnr.

    CLEAR lt_ekpo.
    READ TABLE gt_ekpo_a WITH KEY ebeln = gt_ekbe_a-ebeln
                                  ebelp = gt_ekbe_a-ebelp.
    CHECK sy-subrc = 0.
    itab-peinh = lt_ekpo-peinh.
    itab-uebtk = lt_ekpo-uebtk.
    itab-elikz = lt_ekpo-elikz.
    itab-erekz = lt_ekpo-erekz.
    itab-loekz = lt_ekpo-loekz.

    CLEAR lt_makt.
    READ TABLE lt_makt WITH KEY matnr = gt_ekbe_a-matnr.
    CHECK sy-subrc = 0.
    itab-maktx = lt_makt-maktx.

    APPEND itab. CLEAR itab.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_EKBE
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKBEH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2016   text
*----------------------------------------------------------------------*
FORM archive_read_ekbeh  USING  p_flag TYPE char2.

  TYPES: BEGIN OF ty_ekbeh,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
         zekkn TYPE dzekkn,
         vgabe TYPE vgabe,
         gjahr TYPE mjahr,
         belnr TYPE mblnr,
         buzei TYPE mblpo,
         werks TYPE werks_d,
         budat TYPE budat,
         bwart TYPE bwart,
         matnr TYPE matnr,
         bewtp TYPE bewtp,
         lifnr TYPE elifn,
         bukrs TYPE bukrs,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekbeh.

  DATA: l_handle    TYPE sytabix,
        lt_ekbeh    TYPE TABLE OF ekbeh WITH HEADER LINE,
        lt_ekko     TYPE TABLE OF ekko WITH HEADER LINE,
        lt_ekpo     TYPE TABLE OF ekpo WITH HEADER LINE,
        lt_mara     TYPE TABLE OF mara WITH HEADER LINE,
        lt_makt     TYPE TABLE OF makt WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekbeh TYPE TABLE OF ty_ekbeh,
        ls_inx_ekbeh TYPE ty_ekbeh.

  RANGES: lr_lifnr FOR lfa1-lifnr.

  CLEAR lr_lifnr.
  IF p_flag = 'US'.
    lr_lifnr[] = r_lfa1_us[].
  ELSEIF p_flag = 'FR'.
    lr_lifnr[] = r_lfa1_fr[].
  ENDIF.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKBEH_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ekbeh[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_inx_ekbeh
    FROM (l_gentab)
   WHERE budat IN s_refdt
     AND ebeln IN s_ebeln
     AND ebelp IN s_ebelp
     AND matnr IN s_matnr
     AND bewtp IN r_bewtp
     AND budat IN s_budat
     AND belnr IN s_belnr
     AND belnr <> space
     AND gjahr IN s_gjahr
     AND lifnr IN r_lfa1_us
     AND bukrs = p_bukrs.

  CHECK NOT lt_inx_ekbeh[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_ekko_a, gt_ekko_a[],gt_ekpo_a, gt_ekpo_a[], gt_ekbeh_a, gt_ekbeh_a[].
  LOOP AT lt_inx_ekbeh INTO ls_inx_ekbeh.
*  4.1 Read information from archivekey & offset
    CLEAR: lt_ekko, lt_ekko[], lt_ekpo, lt_ekpo[], lt_ekbeh, lt_ekbeh[].
    CALL FUNCTION 'ASH_MM_EKKO_READ'
      EXPORTING
        i_archivekey           = ls_inx_ekbeh-archivekey
        i_offset               = ls_inx_ekbeh-archiveofs
      TABLES
        et_ekko                = lt_ekko
        et_ekpo                = lt_ekpo
        et_ekbeh               = lt_ekbeh
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK sy-subrc = 0.

    CHECK NOT lt_ekko[] IS INITIAL.
    CHECK NOT lt_ekpo[] IS INITIAL.
    CHECK NOT lt_ekbeh[] IS INITIAL.

    DELETE lt_ekbeh WHERE cpudt NOT IN s_cpudt
                      OR  lfgja NOT IN s_lfgja
                      OR  lfbnr NOT IN s_lfbnr.

    DELETE lt_ekko WHERE bsart NOT IN s_bsart
                      OR ekgrp NOT IN s_ekgrp.

    LOOP AT lt_ekbeh WHERE ebeln EQ ls_inx_ekbeh-ebeln
                     AND   ebelp EQ ls_inx_ekbeh-ebelp
                     AND   zekkn EQ ls_inx_ekbeh-zekkn
                     AND   vgabe EQ ls_inx_ekbeh-vgabe
                     AND   gjahr EQ ls_inx_ekbeh-gjahr
                     AND   belnr EQ ls_inx_ekbeh-belnr
                     AND   buzei EQ ls_inx_ekbeh-buzei.
      gt_ekbeh_a = lt_ekbeh.
      APPEND gt_ekbeh_a.
      CLEAR: gt_ekbeh_a,
             lt_ekbeh.
    ENDLOOP.
*
* 5. Append archived data table to finally interal table
    INSERT LINES OF: lt_ekko INTO TABLE gt_ekko_a,
                     lt_ekpo INTO TABLE gt_ekpo_a.
*                     LT_EKBEH INTO TABLE GT_EKBEH_A.
  ENDLOOP.

  CHECK NOT gt_ekko_a[] IS INITIAL.
  CHECK NOT gt_ekpo_a[] IS INITIAL.
  CHECK NOT gt_ekbeh_a[] IS INITIAL.

  lt_ekbeh[] = gt_ekbeh_a[].
  SORT lt_ekbeh BY matnr.
  DELETE ADJACENT DUPLICATES FROM: lt_ekbeh COMPARING matnr.

*  SORT: GT_EKKO_A, GT_EKPO_A, GT_EKBEH_A.
*  DELETE ADJACENT DUPLICATES FROM: GT_EKKO_A COMPARING ALL FIELDS,
*                                   GT_EKPO_A COMPARING ALL FIELDS,
*                                   GT_EKBEH_A COMPARING ALL FIELDS.

* mara
  CLEAR: lt_mara, lt_mara[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN lt_ekbeh
   WHERE matnr = lt_ekbeh-matnr
     AND matkl IN s_matkl
     AND mtart IN s_mtart
     AND profl IN s_profl.

* makt
  CLEAR: lt_makt, lt_makt[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_makt
    FROM makt
    FOR ALL ENTRIES IN lt_mara
   WHERE matnr = lt_mara-matnr
     AND spras = sy-langu.

  CLEAR itab.
  LOOP AT gt_ekbeh_a.
    itab-belnr = gt_ekbeh_a-belnr.
    itab-bewtp = gt_ekbeh_a-bewtp.
    itab-budat = gt_ekbeh_a-budat.
    itab-cpudt = gt_ekbeh_a-cpudt.
    "itab-dmbtr = gt_ekbeh_a-dmbtr.  hist~dmbtr AS reewr
    itab-reewr = gt_ekbeh_a-dmbtr.
    itab-ebeln = gt_ekbeh_a-ebeln.
    itab-ebelp = gt_ekbeh_a-ebelp.
    itab-gjahr = gt_ekbeh_a-gjahr.
    itab-lfbnr = gt_ekbeh_a-lfbnr.
    itab-lfgja = gt_ekbeh_a-lfgja.
    itab-matnr = gt_ekbeh_a-matnr.
    itab-menge = gt_ekbeh_a-menge.
    itab-shkzg = gt_ekbeh_a-shkzg.

    CLEAR lt_mara.
    READ TABLE lt_mara WITH KEY matnr = gt_ekbeh_a-matnr.
    CHECK sy-subrc = 0.
    itab-mtart = lt_mara-mtart.
    itab-profl = lt_mara-profl.

    CLEAR lt_ekko.
    READ TABLE gt_ekko_a WITH KEY ebeln = gt_ekbeh_a-ebeln.
    CHECK sy-subrc = 0.
    itab-bsart = lt_ekko-bsart.
    itab-bstyp = lt_ekko-bstyp.
    itab-ekgrp = lt_ekko-ekgrp.
    itab-lifnr = lt_ekko-lifnr.

    CLEAR lt_ekpo.
    READ TABLE gt_ekpo_a WITH KEY ebeln = gt_ekbeh_a-ebeln
                                  ebelp = gt_ekbeh_a-ebelp.
    CHECK sy-subrc = 0.
    itab-peinh = lt_ekpo-peinh.
    itab-uebtk = lt_ekpo-uebtk.
    itab-elikz = lt_ekpo-elikz.
    itab-erekz = lt_ekpo-erekz.
    itab-loekz = lt_ekpo-loekz.

    CLEAR lt_makt.
    READ TABLE lt_makt WITH KEY matnr = gt_ekbeh_a-matnr.
    CHECK sy-subrc = 0.
    itab-maktx = lt_makt-maktx.

    APPEND itab. CLEAR itab.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_EKBEH
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ekpo .

  TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekpo.

  DATA: l_handle    TYPE sytabix,
        lt_ekpo     TYPE TABLE OF ekpo WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekpo TYPE TABLE OF ty_ekpo,
        ls_inx_ekpo TYPE ty_ekpo.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKPO_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ekpo[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ekpo
    FROM (l_gentab)
   WHERE ebeln = itab-ebeln
     AND ebelp = itab-ebelp.

  CHECK NOT lt_inx_ekpo[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_ekpo_a, gt_ekpo_a[].
  LOOP AT lt_inx_ekpo INTO ls_inx_ekpo.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_ekpo-archivekey
        offset                    = ls_inx_ekpo-archiveofs
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
    CLEAR: lt_ekpo, lt_ekpo[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'EKPO'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_ekpo
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_ekpo[] IS INITIAL.

    READ TABLE lt_ekpo INDEX 1.
    IF sy-subrc = 0.
      itab-saprc = lt_ekpo-netpr.
      itab-saunt = lt_ekpo-peinh.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_EKPO
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_LIKP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_likp .

  TYPES: BEGIN OF ty_likp,
         vbeln TYPE vbeln_vl,
         borgr_grp TYPE borgr_grp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_likp.

  DATA: l_handle    TYPE sytabix,
        lt_likp     TYPE TABLE OF likp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_likp TYPE TABLE OF ty_likp,
        ls_inx_likp TYPE ty_likp.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZLIKP_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_likp[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_likp
    FROM (l_gentab)
   WHERE vbeln = itab-zvbeln.

  CHECK NOT lt_inx_likp[] IS INITIAL.

  READ TABLE lt_inx_likp INTO ls_inx_likp INDEX 1.

  CHECK sy-subrc = 0.

  itab-asn = ls_inx_likp-borgr_grp.

ENDFORM.                    " ARCHIVE_READ_LIKP
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bkpf USING p_key CHANGING p_belnr.

  DATA: lt_bkpf LIKE bkpf OCCURS 0 WITH HEADER LINE,
        lt_ybseg TYPE bseg OCCURS 10,
        lt_selections TYPE rsds_range OCCURS 10.
  DATA: l_errors      TYPE boole.

  l_errors = 'X'.
  CLEAR: g_awkey.
  g_awkey = p_key.
  PERFORM set_sel_condition_for_bkpf USING    space
                                     CHANGING lt_selections.

  CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
    EXPORTING
      i_selections               = lt_selections
*   I_CONVERT_CURRENCY         = 'X'
*   I_USE_ACCOUNT_CONTRL       =
*   I_SHOW_ERRORS              =
*   I_SHOW_PROGESS             = 'X'
* IMPORTING
*   E_LOG_HANDLE               =
   TABLES
     e_bkpf                     = lt_bkpf
     e_bseg                     = lt_ybseg
*   E_BSIS                     =
*   E_BSAS                     =
*   E_BSAD                     =
*   E_BSAK                     =
*   E_BSIP                     =
*   E_BSIM                     =
*   I_ARCH_SEL                 =
*   E_FAGLBSAS                 =
*   E_FAGLBSIS                 =
*   E_BSEG_ADD                 =
* CHANGING
*   CT_DOC_DATA                =
   EXCEPTIONS
     no_infostruc_found         = 1
     selections_error           = 2
     OTHERS                     = 3
            .
  IF sy-subrc EQ 0.
    READ TABLE lt_bkpf WITH KEY awkey = g_awkey.
    IF sy-subrc EQ 0.
      p_belnr = lt_bkpf-belnr.
    ENDIF.
  ENDIF.

ENDFORM.                    " ARCHIVE_READ_BKPF
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ITAB  text
*----------------------------------------------------------------------*
FORM archive_read_bkpf_2  TABLES  pt_itab  STRUCTURE  gt_itab_a.
  DATA: lt_bkpf LIKE bkpf OCCURS 0 WITH HEADER LINE,
        lt_ybseg TYPE bseg OCCURS 10,
        lt_selections TYPE rsds_range OCCURS 10.
  DATA: l_errors      TYPE boole.

  l_errors = 'X'.
  PERFORM set_range_for_awkey TABLES pt_itab.

  PERFORM set_sel_condition_for_bkpf USING     'X'
                                     CHANGING lt_selections.

  CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
    EXPORTING
      i_selections               = lt_selections
*   I_CONVERT_CURRENCY         = 'X'
*   I_USE_ACCOUNT_CONTRL       =
*   I_SHOW_ERRORS              =
*   I_SHOW_PROGESS             = 'X'
* IMPORTING
*   E_LOG_HANDLE               =
   TABLES
     e_bkpf                     = lt_bkpf
     e_bseg                     = lt_ybseg
*   E_BSIS                     =
*   E_BSAS                     =
*   E_BSAD                     =
*   E_BSAK                     =
*   E_BSIP                     =
*   E_BSIM                     =
*   I_ARCH_SEL                 =
*   E_FAGLBSAS                 =
*   E_FAGLBSIS                 =
*   E_BSEG_ADD                 =
* CHANGING
*   CT_DOC_DATA                =
   EXCEPTIONS
     no_infostruc_found         = 1
     selections_error           = 2
     OTHERS                     = 3
            .
  IF sy-subrc EQ 0.
    LOOP AT lt_bkpf.
      it_bkpf-awkey = lt_bkpf-awkey.
      it_bkpf-belnr = lt_bkpf-belnr.
      it_bkpf-blart = lt_bkpf-blart.
      APPEND it_bkpf.
      CLEAR: it_bkpf,
             lt_bkpf.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " ARCHIVE_READ_BKPF_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_MKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_mkpf TABLES pt_itab  STRUCTURE gt_itab_a2.

  TYPES: BEGIN OF ty_mkpf,
         mblnr TYPE mblnr,
         mjahr TYPE mjahr,
         blart TYPE blart,
         budat TYPE budat,
         xblnr TYPE xblnr1,
         bktxt TYPE bktxt,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_mkpf.

  DATA: l_handle    TYPE sytabix,
        lt_mkpf     TYPE TABLE OF mkpf WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_mkpf TYPE TABLE OF ty_mkpf,
        ls_inx_mkpf TYPE ty_mkpf.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZMKPF_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

  CHECK NOT pt_itab[] IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_mkpf[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_mkpf
    FROM (l_gentab)
    FOR ALL ENTRIES IN pt_itab
   WHERE mblnr = pt_itab-lfbnr
     AND mjahr = pt_itab-lfgja.

  CHECK NOT lt_inx_mkpf[] IS INITIAL.

  LOOP AT lt_inx_mkpf INTO ls_inx_mkpf.
    CLEAR it_mkpf.
    it_mkpf-mblnr = ls_inx_mkpf-mblnr.
    it_mkpf-mjahr = ls_inx_mkpf-mjahr.
    it_mkpf-xblnr = ls_inx_mkpf-xblnr.
    it_mkpf-budat = ls_inx_mkpf-budat.
    it_mkpf-bktxt = ls_inx_mkpf-bktxt.

    APPEND it_mkpf.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_MKPF
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_RBKP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ITAB2  text
*----------------------------------------------------------------------*
FORM archive_read_rbkp  TABLES  pt_itab2 STRUCTURE gt_itab_a3.

  TYPES: BEGIN OF ty_rbkp,
           stblg TYPE re_stblg,
           stjah TYPE re_stjah,
         archivekey TYPE arkey,
         archiveofs TYPE admi_offst.
  TYPES: END OF ty_rbkp.

  DATA: l_handle    TYPE sytabix,
        lt_rbkp     TYPE TABLE OF rbkp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_rbkp TYPE TABLE OF ty_rbkp,
        ls_inx_rbkp TYPE ty_rbkp.

  CONSTANTS: c_zrbkp_001(9) VALUE 'ZRBKP_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zrbkp_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_rbkp[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_rbkp
    FROM (l_gentab)
    FOR ALL ENTRIES IN pt_itab2
   WHERE belnr = pt_itab2-belnr
     AND gjahr = pt_itab2-gjahr.

  CHECK NOT lt_inx_rbkp[] IS INITIAL.

  LOOP AT lt_inx_rbkp INTO ls_inx_rbkp.
    CLEAR it_rbkp.
    MOVE-CORRESPONDING ls_inx_rbkp TO it_rbkp.

    APPEND it_rbkp.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_RBKP
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_konp .

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
   WHERE knumh = it_knumh-knumh
     AND kappl = 'M'
     AND kschl = 'PB00'.

  CHECK NOT lt_inx_konp[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_konp_a, gt_konp_a[].
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

    CLEAR: itab-ifprc, itab-ifunt.
    READ TABLE lt_konp INDEX 1.
    itab-ifprc = lt_konp-kbetr.
    itab-ifunt = lt_konp-kpein.
    EXIT.

* 5. Append archived data table to finally interal table
*    INSERT LINES OF lt_konh INTO TABLE gt_konh_a.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSAK
*&---------------------------------------------------------------------*
FORM archive_read_bsak .
  DATA: lt_bsak  LIKE bsak OCCURS 0 WITH HEADER LINE,
        lt_ybkpf TYPE bkpf OCCURS 10,
        lt_ybseg TYPE bseg OCCURS 10,
        lt_selections TYPE rsds_range OCCURS 10.
  DATA: l_errors      TYPE boole.

  l_errors = 'X'.

  PERFORM set_sel_condition_for_bsak CHANGING lt_selections.

  CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
    EXPORTING
      i_selections       = lt_selections
    TABLES
      e_bkpf             = lt_ybkpf
      e_bseg             = lt_ybseg
*     E_BSIS             =
*     E_BSAS             =
*     E_BSAD             = LT_BSAD
      e_bsak             = lt_bsak
*     E_BSIP             =
*     E_BSIM             =
*     I_ARCH_SEL         =
*     E_FAGLBSAS         =
*     E_FAGLBSIS         =
*     E_BSEG_ADD         =
    EXCEPTIONS
      no_infostruc_found = 1
      selections_error   = 2
      OTHERS             = 3.
  IF sy-subrc EQ 0.
    LOOP AT lt_bsak.
      itab-duedt = lt_bsak-zfbdt.
      itab-clrdt = lt_bsak-augdt.
      itab-augbl = lt_bsak-augbl.
      CLEAR: lt_bsak.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " ARCHIVE_READ_BSAK
*&---------------------------------------------------------------------*
*&      Form  SET_SEL_CONDITION_FOR_BSAK
*&---------------------------------------------------------------------*
FORM set_sel_condition_for_bsak
                          CHANGING  pt_selections TYPE rsds_trange.

  PERFORM set_select_field_option USING    'BKPF'
                                           'BUKRS'
                                           'I'
                                           'EQ'
                                           'P_BUKRS'
                                           'P'
                                  CHANGING pt_selections.

  PERFORM set_select_field_option USING    'BKPF'
                                           'BELNR'
                                           'I'
                                           'EQ'
                                           'IT_BKPF-BELNR'
                                           'P'
                                  CHANGING pt_selections.

  PERFORM set_select_field_option USING    'BKPF'
                                           'GJAHR'
                                           'I'
                                           'EQ'
                                           'ITAB-GJAHR'
                                           'P'
                                  CHANGING pt_selections.

  PERFORM set_select_field_option USING    'BSEG'
                                           'LIFNR'
                                           'I'
                                           'EQ'
                                           'ITAB-LIFNR'
                                           'P'
                                  CHANGING pt_selections.

ENDFORM.                    " SET_SEL_CONDITION_FOR_BSAK
*&---------------------------------------------------------------------*
*&      Form  SET_SELECT_FIELD_OPTION
*&---------------------------------------------------------------------*
FORM set_select_field_option
                         USING    p_tablename
                                  p_fieldname
                                  p_sign
                                  p_option
                                  p_selfield
                                  p_param
                         CHANGING pt_selections TYPE rsds_trange.

  DATA:  ls_selopt    LIKE rsdsselopt,
         lt_selopt    TYPE rsds_selopt_t,
         ls_frange    TYPE rsds_frange,
         lt_frange    TYPE rsds_frange_t,
         ls_trange    TYPE rsds_range,
         lt_trange    TYPE rsds_trange.

  DATA: l_name TYPE string.

  FIELD-SYMBOLS: <fs_itab>  TYPE ANY TABLE,
                 <fs_wa>    TYPE any,
                 <fs_value> TYPE any.

  ls_trange-tablename = p_tablename.

  CASE p_param.
    WHEN 'P'.
      l_name = p_selfield.
      ASSIGN (l_name) TO <fs_value>.

      IF NOT <fs_value> IS INITIAL.
        REFRESH: lt_selopt.
        ls_frange-fieldname = p_fieldname.
        ls_selopt-sign = p_sign.
        ls_selopt-option = p_option.
        ls_selopt-low = <fs_value>.
        APPEND ls_selopt TO lt_selopt.

        ls_frange-selopt_t = lt_selopt[].
        APPEND ls_frange TO lt_frange.
      ENDIF.

    WHEN 'S'.
      CONCATENATE p_selfield
                  '[]'
             INTO l_name.
      ASSIGN (l_name) TO <fs_itab>.

      IF NOT <fs_itab> IS INITIAL.
        REFRESH: lt_selopt.
        ls_frange-fieldname = p_fieldname.
        LOOP AT <fs_itab> ASSIGNING <fs_wa>.
          MOVE-CORRESPONDING <fs_wa> TO ls_selopt.
*          LS_SELOPT = <FS_WA>.
          APPEND ls_selopt TO lt_selopt.
        ENDLOOP.
        ls_frange-selopt_t = lt_selopt[].
        APPEND ls_frange TO lt_frange.
      ENDIF.
  ENDCASE.

  IF NOT lt_frange[] IS INITIAL .
    ls_trange-frange_t = lt_frange[].
    APPEND ls_trange TO lt_trange.
  ENDIF.

  APPEND LINES OF lt_trange TO pt_selections.
ENDFORM.                    " SET_SELECT_FIELD_OPTION
*&---------------------------------------------------------------------*
*&      Form  SET_SEL_CONDITION_FOR_BKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_SELECTIONS  text
*----------------------------------------------------------------------*
FORM set_sel_condition_for_bkpf USING    p_range
                                CHANGING pt_selections.
  IF p_range EQ 'X'.
    PERFORM set_select_field_option USING    'BKPF'
                                             'AWKEY'
                                             ' '
                                             ' '
                                             'GR_AWKEY'
                                             'S'
                                    CHANGING pt_selections.
  ELSE.
    PERFORM set_select_field_option USING    'BKPF'
                                             'AWKEY'
                                             'I'
                                             'EQ'
                                             'G_AWKEY'
                                             'P'
                                    CHANGING pt_selections.
  ENDIF.

ENDFORM.                    " SET_SEL_CONDITION_FOR_BKPF
*&---------------------------------------------------------------------*
*&      Form  SET_RANGE_FOR_AWKEY
*&---------------------------------------------------------------------*
FORM set_range_for_awkey  TABLES   pt_itab STRUCTURE  gt_itab_a.
  CLEAR  : gr_awkey.
  REFRESH: gr_awkey.

  LOOP AT pt_itab.
    gr_awkey-sign = 'I'.
    gr_awkey-option = 'EQ'.
    gr_awkey-low = pt_itab-awkey.
    APPEND gr_awkey.
    CLEAR: gr_awkey,
           pt_itab.
  ENDLOOP.
ENDFORM.                    " SET_RANGE_FOR_AWKEY
