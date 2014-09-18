*----------------------------------------------------------------------*
*   INCLUDE ZACOU102_TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Define Variables and Internal Tables
*----------------------------------------------------------------------*
TYPE-POOLS kcde.

TABLES: keko,*keko,  " Product Costing - Header Data
        ckis,        " Items Unit Costing/Itemization Product Costing
        mbewh,       " Material Valuation: History
        marc,        " Plant Data for Material
        ztcou102,    " [CO] Costing Result
        t141,
        sscrfields,*ztcou102, mara.

*FIXME; no hard coding...andy
CONSTANTS: c_ekorg LIKE ekko-ekorg  VALUE 'PU01', "Purchase Org.
           c_lednr LIKE ckis-lednr  VALUE '00',   "STD LEDGER!!!
           c_bzobj LIKE keko-bzobj  VALUE '0',
           c_kd_bklas TYPE bklas    VALUE '3000',
           c_ver   LIKE ztcou102-ver VALUE 0.

TYPES: BEGIN OF ty_a902,
         stawn TYPE stawn,
         kbetr TYPE kbetr,
       END OF ty_a902.

DATA gt_a902 TYPE TABLE OF ty_a902 WITH HEADER LINE.

* Internal Table for Costing result
TYPES: BEGIN OF ty_ckis,

         kalnr  TYPE ck_kalnr1,    " Cost estimate number
         kalka  TYPE ck_kalka,     " Costing type
         kadky  TYPE ck_kadky,     " Costing date
         matnr  TYPE matnr,        " End Item
         kokrs  TYPE kokrs,        " Controling Area
         werks  TYPE werks_d,      " Plant
         bdatj  TYPE bdatj,        " Year
         poper  TYPE poper,        " Period
         bwdat  TYPE ck_bwdat,     " Valuation Date
         aldat  TYPE ck_brdat,     " Quantity structure date
         hrkft  TYPE hrkft,        " Origin Group
         pmeht  TYPE pmeht,        " Price Unit of Qty.
         menge  TYPE menge_pos,    " Qty
         wertn  TYPE ck_kpt,       " Info-price
         infnr  TYPE infnr,        " Info.Record No.

         lifnr  TYPE lifnr,        " Vendor
         land1  TYPE land1_gp,     " Country key
         name1  TYPE name1_gp,     " Vendor name

         elemt  TYPE ck_elemenths, " CC

         beskz  TYPE beskz,        " Procurement type
         sobes  TYPE sobes,        " Special procurement
         baugr  TYPE ck_baugr,     " Indicator - item is an assembly

         compn(1) TYPE c,          " only component exist
         matkl TYPE matkl,
* by IG.MOON 10/2007 {
         strat TYPE ck_strat,       " Valuation Strategy
         substrat TYPE ck_substrat, " Sub-strategy
         elemtns TYPE ck_elementns,
*         BTYP    TYPE CKML_BTYP,
*  }
       END OF ty_ckis.

* Internal Table for previous Info-price & reason code
TYPES: BEGIN OF ty_a018,
          matnr TYPE matnr,
          lifnr TYPE lifnr,
          datbi TYPE kodatbi,
          datab TYPE kodatab,
          knumh TYPE knumh,
*          URZDT TYPE URZDT,
*          KSCHL LIKE KONH-KSCHL,
          kbetr1 TYPE kbetr,
          kbetr2 TYPE kbetr,
          tkbetr TYPE kbetr,
          kpein  TYPE kpein,
          kmein  TYPE kmein,
          konwa  TYPE konwa,
          kosrt  TYPE kosrt,
          ekgrp  TYPE ekgrp,
          kzust  TYPE kzust,
          fra1   TYPE zfra1,
          zoa1   TYPE zoth,
          zoth   TYPE zoth,
          zoti   TYPE zoti,
          zp12   TYPE zzp12,
          zp13   TYPE zzp13,
          zp16   TYPE zzp16,
          zp17   TYPE zzp17,
          zp18   TYPE zzp18,
       END OF ty_a018.

* Internal Table for Material Info.
TYPES: BEGIN OF ty_mat,
         matnr TYPE matnr,
         maktg TYPE maktg,
         mtart TYPE mtart,
         profl TYPE adge_profl,
         mstae TYPE mstae,
         meins TYPE meins,
         werks TYPE werks_d,
         ekgrp TYPE ekgrp,
         stlan TYPE stlan,
         stawn TYPE stawn,
         dispo TYPE dispo,
         mmsta TYPE mmsta,

         peinh TYPE peinh,
         bwkey TYPE bwkey,
         bklas TYPE bklas,
         stprs TYPE ck_stprs_1,
         pvprs TYPE ck_pvprs_1,
* UD1K941202 - by IG.MOON 8/2/2007 {
         sobsl TYPE sobsl,
         matkl TYPE matkl,
* }
       END OF ty_mat.

* Internal Table for Vendor Info
TYPES: BEGIN OF ty_vendor,
         lifnr TYPE lifnr,
         land1 TYPE land1_gp,
         name1 TYPE name1_gp,
       END OF ty_vendor.

* Internal Table for Display
TYPES: BEGIN OF ty_ztcou102.
        INCLUDE STRUCTURE ztcou102.
TYPES:   maktg  TYPE maktg,
         stlan  TYPE stlan,
         mstae  TYPE mstae,
         dispo  TYPE dispo,
         mmsta  TYPE mmsta,
         land1  TYPE land1_gp,     " Country key
         name1  TYPE name1_gp,     " Vendor name
* UD1K941202 - by IG.MOON 8/3/2007 {
         matkl  TYPE matkl,        " Material Group
*}
       END OF ty_ztcou102.

TYPES: BEGIN OF ty_out.
        INCLUDE TYPE ty_ztcou102.
TYPES:  kzust1_in TYPE kzust,
        kzust2_in TYPE kzust,
        diff      TYPE ck_kwt,     " Diff.Price
        amt_dif   TYPE ck_kwt,     " Exception Diff.Price
        rate_dif  TYPE kkb_cal2_lightspx,  " Exception Diff.Rate
        estat     TYPE icon_d,     " Exception Icon
        icon      TYPE icon_d,
        chk(1)    TYPE c,
        canc,
        celltab TYPE lvc_t_styl,
        mess(120) TYPE c,
* UD1K941202 - by IG.MOON 8/3/2007 {
        gprc  TYPE zwertn1,
        icnlck    TYPE icon_d,
        lifbi     TYPE lifbi,
*}
        matnr2    TYPE matnr,                               "UD1K949919
        matnr1    TYPE matnr,                               "UD1K949919
        maktx     TYPE maktx,                               "UD1K949919
        zcatx     TYPE zcatx,                               "UD1K949919
      END OF ty_out.

TYPES: BEGIN OF ty_dtl.
        INCLUDE TYPE ty_out.
TYPES:   indx    TYPE i,
        adflg(1),                     " X: Added Vendor
      END OF ty_dtl.

* Internal Table for Plant
TYPES: BEGIN OF ty_plant,
         bwkey TYPE bwkey,
       END OF ty_plant.

* Internal Table for Standard price
TYPES: BEGIN OF ty_std,
         matnr  TYPE matnr,        " Material
         bwkey  TYPE bwkey,        " Plant
         chk(1) TYPE c,
       END OF ty_std.

* Internal Table for ABP LDC Rate
TYPES: BEGIN OF ty_ldc,
         kokrs TYPE kokrs,
         bdatj TYPE bdatj,
         ver   TYPE zver1,
         land1 TYPE land1_gp,
         matnr TYPE matnr,
         fra1  TYPE zfrg1,
         zoth  TYPE zoth,
         zoti  TYPE zoti,
       END OF ty_ldc.

* Internal Table for apply LDC date
TYPES: BEGIN OF ty_lfa1,
         lifnr  TYPE lifnr,        " Vendor
         name1  TYPE name1_gp,                              " Name 1
       END OF ty_lfa1.

TYPES: BEGIN OF ty_matnr,
         matnr TYPE matnr,
         lifnr TYPE lifnr,
       END OF ty_matnr.

* Internal Table for Download
TYPES: BEGIN OF ty_dd03l,
       fieldname TYPE fieldname,
       position  TYPE tabfdpos,
     END OF ty_dd03l.

TYPES ddshretval_table TYPE TABLE OF ddshretval.

DATA: BEGIN OF gt_fieldnames OCCURS 0,
       name(20),                 " Column names for download
      END OF gt_fieldnames.

DATA: BEGIN OF gt_down OCCURS 0,
        kokrs(4),         " Controlling Area
        bdatj(4),         " Year
        poper(3),         " Period
        kalka(2),         " Costing type
        ver(2),           " BP Ver
        matnr(18),        " Material
        werks(4),         " Plant
        mtart(4),         " Material type
        bwdat(8),         " Valuation date of a cost estimate
        kadky(8),         " Costing date
        aldat(8),         " Quantity structure date
        ekgrp(3),         " Pur.Grp
        profl(3),         " Src
        bklas(4),         " Val.Class
        infnr(10),        " Info record number
        stawn(17),        " Commodity code
        verpr(11),        " MAP
        stprs(11),        " STD
        wertn(16),        " Info-Price
        peinh(5),         " Per
        pmeht(5),         " UoM
        pwertn(16),       " Prv.info-price
        lifnr(10),        " 1st Vendor
        qta(5),           " Quota of 1st Vendor
        wertn_v1(16),     " Info-price of 1st Vendor
        duty(16),         " Duty
        frg(16),          " Freight
        oth(16),          " Others
        kzust1(3),        " RSN
        wertn1(16),                                         " RS1 $
        kzust2(16),                                         " Reason2
        wertn2(16),                                         " RS2 $
        lifnr2(10),       " 2nd Vendor
        qta_v2(5),        " Quota of 2nd Vendor
        wertn_v2(16),     " Info-price of 2nd Vendor
        kzust1_v2(3),     " Reason1 of 2nd Vendor
        wertn1_v2(16),    " RS1 $ of 2nd Vendor
        kzust2_v2(3),     " Reason2 of 2nd Vendor
        wertn2_v2(16),    " RS2 $ of 2nd Vendor
        ec_g(1),          " Error Category:G
        ec_a(1),          " Error Category:A
        ec_s(1),          " Error Category:S
        ec_q(1),          " Error Category:Q
        ec_v(1),          " Error Category:V
        ec_p(1),          " Error Category:P
        ec_r(1),          " Error Category:R
        stat(1),          " Status
        aedat(8),         " Changed by
        aenam(12),        " Changed on
        maktg(50),        " Description
        diff(16),         " Diff.Price
        amt_dif(16),      " Exception Diff.Price
        rate_dif(16),     " Exception Diff.Rate
        estat,            " Exception Icon
        g_prc(16),
      END OF gt_down.

* UD1K941202 - by IG.MOON 8/3/2007 { /// Vendor Determination
DATA: BEGIN OF gt_eina OCCURS 0,
        matnr LIKE mara-matnr,
        lifnr LIKE lfa1-lifnr,
        ekgrp LIKE ekko-ekgrp,
        lifab LIKE eina-lifab,
        lifbi LIKE eina-lifbi,
        urzdt LIKE eina-urzdt,
        kbetr LIKE konp-kbetr,
        lmein LIKE eina-lmein,
      END OF gt_eina.
* }


DATA: gt_ckis      TYPE TABLE OF ty_ckis     WITH HEADER LINE,
      gt_comp      TYPE TABLE OF ty_ckis     WITH HEADER LINE,
      gt_stock     TYPE TABLE OF ty_ckis     WITH HEADER LINE,

      gt_a018      TYPE TABLE OF ty_a018     WITH HEADER LINE,
      gt_p018      TYPE TABLE OF ty_a018     WITH HEADER LINE,
      gt_mat       TYPE TABLE OF ty_mat      WITH HEADER LINE,
      gt_vendor    TYPE TABLE OF ty_vendor   WITH HEADER LINE,
      gt_ztcou102  TYPE TABLE OF ty_ztcou102 WITH HEADER LINE,
      gt_out       TYPE TABLE OF ty_out      WITH HEADER LINE,
      gt_out1      TYPE TABLE OF ty_out      WITH HEADER LINE,
      gt_dtl       TYPE TABLE OF ty_dtl      WITH HEADER LINE,
      gt_plant     TYPE TABLE OF ty_plant    WITH HEADER LINE,
*     GT_MAT_TEMP  TYPE TABLE OF TY_STD      WITH HEADER LINE,
      gt_ldc       TYPE TABLE OF ty_ldc      WITH HEADER LINE,
      gt_lfa1      TYPE TABLE OF ty_lfa1     WITH HEADER LINE,
      lt_matnr     TYPE TABLE OF ty_matnr    WITH HEADER LINE,
      gt_dd03l     TYPE TABLE OF ty_dd03l    WITH HEADER LINE,
      gw_ckis      LIKE gt_ckis.

DATA: BEGIN OF gt_102p OCCURS 0,
        matnr TYPE matnr,
        wertn TYPE wertn,
      END OF gt_102p.

* BEGIN OF UD1K954962
DATA: gt_zfta_duty TYPE TABLE OF zfta_duty WITH HEADER LINE.
* END OF UD1K954962

RANGES r_bwkey FOR t001w-bwkey.
RANGES: gr_beskz   FOR keko-beskz.

RANGES: gr_kalaid  FOR  keko-kalaid."KEKO selection
DATA:   gv_kaladat LIKE keko-kaladat.

DATA: gv_index    TYPE i,
      eknam       TYPE eknam,
      gv_canc,                           " Cancel Flag ('X')

      gv_kadky    TYPE ck_kadky,         " Costing date(key)
      gv_bwdat    TYPE ck_bwdat,         " Valuation Date
      gv_bwdat_in TYPE sydatum ,         " Valuation Date input
      gv_cstdt   TYPE sydatum,            " Date

      gv_drate(9) TYPE p DECIMALS 6,     " Rate of Duty
      gv_frate(9) TYPE p DECIMALS 6,     " Rate of Freight
      gv_orate(9) TYPE p DECIMALS 6,     " Rate of Others
*      GV_CHK,
      ekgrp  LIKE marc-ekgrp,            " Purchasing group
      bklas  LIKE t025t-bklas,           " Valuation Class
      profl  LIKE mara-profl,            " Src
      lifnr  LIKE lfa1-lifnr,            " Vendor
      qta    TYPE zqta,                  " Quota
      kzust1 LIKE t686d-kzust,           " Reason
      rate(10),                          " Increase/Decrease Rate
      gv_ekgrp  TYPE ekgrp,              " Purchasing group
      gv_bklas  TYPE bklas,              " Valuation Class
      gv_profl  TYPE adge_profl,         " Src
      gv_lifnr  TYPE lifnr,              " Vendor
      gv_qta    TYPE zqta,               " Quota
      gv_kzust1 TYPE kzust,              " Reason
      gv_pct(10) TYPE p DECIMALS 2,      " %INC/DEC
      ver       TYPE zver1,              " BP Ver
      gv_ver    TYPE zver1,              " BP Ver

      gv_pyear  TYPE bdatj,              " Pervious year
      gv_ppoper TYPE poper,              " Pervious period
      gv_prvdt  TYPE sydatum,            " Prv date

      gv_klvar  TYPE ck_klvar,           " Costing Version
      gv_save,                           " Flag of Save
      gv_file TYPE localfile,            " File Name
      gv_rc TYPE i,                      " Return Code
      gv_stat,
      gv_tcnt TYPE i,                    " Total
      gv_scnt TYPE i,                    " Success
      gv_fcnt TYPE i,                    " Fail
      gv_level(5),                       " Authorization Level
      gv_lock,
      gv_lchk,
      gv_user  TYPE syuname,
      gv_cuser TYPE syuname,
      gv_bukrs TYPE bukrs,
      g_idx    LIKE sy-tabix.

* for BAPI
DATA: gs_head  LIKE bapimathead, " Header with control information
      gs_mara  LIKE bapi_mara,   " Material Data at Client Level
      gs_marax LIKE bapi_marax,  " Checkbox Structure for BAPI_MARA
      gs_marc  LIKE bapi_marc,   " Plant-specific material data
      gs_marcx LIKE bapi_marcx,  " Information on update for PLANTDATA
      gs_mbew  LIKE bapi_mbew,   " Valuation data
      gs_mbewx LIKE bapi_mbewx,
                             " Information on update for VALUATIONDATA
      gt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

*- U1 start
DATA: gt_konh_a TYPE TABLE OF konh WITH HEADER LINE,
      gt_konp_a TYPE TABLE OF konp WITH HEADER LINE,
      gt_ztcou103_a TYPE TABLE OF ztcou103 WITH HEADER LINE,
      gt_keko_a TYPE TABLE OF keko WITH HEADER LINE,
      gt_ckis_a TYPE TABLE OF ckis WITH HEADER LINE.
DATA: gt_new_a TYPE TABLE OF ty_ckis WITH HEADER LINE.
DATA: BEGIN OF gt_info_condi_a OCCURS 0,
          knumh      LIKE konh-knumh,
          kopos      LIKE konp-kopos,
          kschl      LIKE konp-kschl,
          kschl_konh LIKE konh-kschl,
*            VAKEY      LIKE KONH-VAKEY,
          datab      LIKE konh-datab,
          datbi      LIKE konh-datbi,
          kzust      LIKE konh-kzust,
          kbetr      LIKE konp-kbetr,
          konwa      LIKE konp-konwa,
          kpein      LIKE konp-kpein,  "!!!!
          kmein      LIKE konp-kmein,  "!!!!
          kumza      LIKE konp-kumza,
          kumne      LIKE konp-kumne,
          meins      LIKE konp-meins,
          loevm_ko   LIKE konp-loevm_ko,
          lifnr      LIKE konp-lifnr,
          kosrt      LIKE konh-kosrt,
          ernam      LIKE konh-ernam,
          erdat      LIKE konh-erdat,
       END OF gt_info_condi_a.
DATA: BEGIN OF gt_ckis_a2 OCCURS 0,
      artnr  LIKE keko-matnr,
      matnr  LIKE ckis-matnr,
      END OF gt_ckis_a2.
*- U1 End

* For Class ------------------------------------------------------------

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS:
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
                IMPORTING sender
                          e_fieldname
                          e_fieldvalue
                          es_row_no
                          er_event_data
                          et_bad_cells
                          e_display,

      my_f4 IMPORTING sender        TYPE REF TO cl_gui_alv_grid
                      et_bad_cells  TYPE lvc_t_modi
                      es_row_no     TYPE lvc_s_roid
                      er_event_data TYPE REF TO cl_alv_event_data
                      e_display     TYPE c
                      e_fieldname   TYPE lvc_fname
            EXPORTING lt_f4         TYPE ddshretval_table.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Change data
  METHOD handle_data_changed.
* remember deleted lines for saving
    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

* Get values of possible entries
  METHOD on_f4.
    PERFORM on_f4 USING sender
                        e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display
                        'GT_DTL'.
  ENDMETHOD.                                                " ON_F4

  METHOD my_f4.
    PERFORM my_f4 TABLES lt_f4
                  USING  sender
                         et_bad_cells
                         es_row_no
                         er_event_data
                         e_display
                         e_fieldname
                         'GT_DTL'.
  ENDMETHOD.                    "MY_F4

ENDCLASS.              " LCL_EVENT_RECEIVER Implementation

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver1 DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ztcou102_key.
    TYPES:   kokrs TYPE kokrs.
    TYPES:   bdatj TYPE bdatj.
    TYPES:   poper TYPE poper.
    TYPES:   kalka TYPE ck_kalka.
    TYPES:   ver   TYPE zver1.
    TYPES:   matnr TYPE matnr.
    TYPES: END OF ztcou102_key.

* UD1K940725 by IG.MOON
    TYPES: ztcou102_keys TYPE STANDARD TABLE OF ztcou102_key,
           ztcou102_table TYPE STANDARD TABLE OF ztcou102.
* end of UD1K940725

    METHODS:
* UD1K940725 by IG.MOON
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,
                       get_deleted_rows
             EXPORTING
                       deleted_rows TYPE ztcou102_table,

      refresh_delta_tables,
* end of UD1K940725


      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row
                       e_column
                       es_row_no,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
                IMPORTING sender
                          e_fieldname
                          e_fieldvalue
                          es_row_no
                          er_event_data
                          et_bad_cells
                          e_display,

      my_f4 IMPORTING sender        TYPE REF TO cl_gui_alv_grid
                      et_bad_cells  TYPE lvc_t_modi
                      es_row_no     TYPE lvc_s_roid
                      er_event_data TYPE REF TO cl_alv_event_data
                      e_display     TYPE c
                      e_fieldname   TYPE lvc_fname
            EXPORTING lt_f4         TYPE ddshretval_table.

  PRIVATE SECTION.
    DATA deleted_rows TYPE STANDARD TABLE OF ztcou102.

* This flag is set if any error occured in one of the
* following methods:
    DATA: error_in_data TYPE c.
    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
ENDCLASS.                   " LCL_EVENT_RECEIVER1 Definition
*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver1 IMPLEMENTATION.

* UD1K940725 by IG.MOON

* Change data
  METHOD handle_data_changed.
* remember deleted lines for saving
    CALL METHOD update_delta_tables( er_data_changed ).
    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

  METHOD get_deleted_rows.
    deleted_rows = me->deleted_rows.
  ENDMETHOD.                    "GET_DELETED_ROWS

  METHOD refresh_delta_tables.
    CLEAR me->deleted_rows[].
  ENDMETHOD.                    "REFRESH_DELTA_TABLES

  METHOD update_delta_tables.
    DATA: l_del_row TYPE lvc_s_moce,
          ls_key TYPE ztcou102_key,
          ls_ztcou102 TYPE ztcou102,
          ls_outtab LIKE LINE OF gt_out.

    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
      READ TABLE gt_out INTO ls_outtab INDEX l_del_row-row_id.
      IF sy-subrc NE 0 OR ls_outtab-zlock EQ 'X'.
        error_in_data = 'X'.
        MESSAGE i000(0k) WITH text-e01.
      ELSE.
        MOVE-CORRESPONDING ls_outtab TO ls_ztcou102.
        APPEND ls_ztcou102 TO deleted_rows.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "UPDATE_DELTA_TABLES

* end of UD1K940725

* Double Click
  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_data_changed

* Get values of possible entries
  METHOD on_f4.
    PERFORM on_f4 USING sender
                        e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display
                        'GT_OUT'.
  ENDMETHOD.                                                " on_f4

  METHOD my_f4.
    PERFORM my_f4 TABLES lt_f4
                  USING  sender
                         et_bad_cells
                         es_row_no
                         er_event_data
                         e_display
                         e_fieldname
                        'GT_OUT'.
  ENDMETHOD.                    "MY_F4

ENDCLASS.              " LCL_EVENT_RECEIVER1 Implementation

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __popup.
  perform pop_up using
  &1 &2 &3
  changing l_answer.
  check l_answer eq 'J'.
END-OF-DEFINITION.

DATA tot_lines TYPE i.

DATA: g_event_receiver  TYPE REF TO lcl_event_receiver,
      g_event_receiver1 TYPE REF TO lcl_event_receiver1,
      gt_filtidx TYPE lvc_t_fidx,
      gt_filter  TYPE lvc_t_filt.

DATA g_error(1).
