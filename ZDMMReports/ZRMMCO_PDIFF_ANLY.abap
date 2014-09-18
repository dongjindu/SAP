************************************************************************
* Program Name      : ZRMMCO_PDIFF_ANLY
* Author            : Furong Wang
* Creation Date     : Aug 2005
* Specifications By : Andy Choi
* Pattern           : Report 1-1
* Development Request No : UD1K
* Addl Documentation: Zmme41
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*.
************************************************************************
REPORT  zrmmco_pdiff_anly NO STANDARD PAGE HEADING LINE-SIZE 82.
*----- Type
TYPE-POOLS : slis, sp01r.

TABLES: mara,  mlhelp_mldoc_crf.
*----- Internal tables
DATA: BEGIN OF wa_mldoc OCCURS 0,
      bukrs LIKE t001k-bukrs,
      bdatj LIKE mlcr-bdatj,          " Posting year
      poper LIKE mlcr-poper,          " Posting period
      feldg LIKE mlcrf-feldg,         " Field group
      awtyp LIKE mlhd-awtyp,          " Ref Procedure
      awref LIKE mlhd-awref,          " Ref Doc
      urzeile LIKE mlit-urzeile,      " org.doc.item#
      vgart LIKE mlhd-vgart,          " Transaction type
      matnr LIKE mara-matnr,
      meins LIKE mara-meins,
      bwkey LIKE mlit-bwkey,          " Valuation area
      peinh LIKE mlcr-peinh,          " Price Unit
      waers LIKE mlcr-waers,          " Currency
      salk3 LIKE mlcr-salk3,          " Total value
      stprs_old LIKE mlcr-stprs_old,  " Standard price
      prdif LIKE mlcrf-prdif,      " value Price diff
      krdif LIKE mlcrf-krdif,      " Value of exchange rate
      menge LIKE mlppf-menge,         " Quantity
      werks LIKE mseg-werks,
      bldat LIKE mlhd-bldat,          " document date
      posdate LIKE mlhd-bldat,         " Posting date
      lifnr LIKE mseg-lifnr,
      rsncode(10),
      vtext LIKE t686d-vtext,
      remark(40),
      bktext LIKE bkpf-bktxt,
      kschl LIKE rseg-kschl,          "Condition
      ebeln LIKE rseg-ebeln,
      ebelp LIKE rseg-ebelp,
END OF wa_mldoc.

DATA: wa_output LIKE ztmm_cost_anly.

CONSTANTS: c_receipt(40) VALUE 'Good Receipt',
           c_stk_transfer(40) VALUE 'Stock Transfer',
           c_invoice(40) VALUE 'Invoice Verification',
           c_import(40) VALUE 'Import Settlement',
           c_price(40) VALUE 'Price Difference',
           c_revaluation(40) VALUE 'Revaluation'.

DATA: it_output LIKE TABLE OF wa_output WITH HEADER LINE.

DATA: it_mldoc_mkpf LIKE TABLE OF wa_mldoc WITH HEADER LINE,
      it_mldoc LIKE TABLE OF wa_mldoc WITH HEADER LINE,
      it_mldoc_temp LIKE TABLE OF wa_mldoc WITH HEADER LINE,
      it_mldoc_error LIKE zmldoc_dif_error.
*      it_mldoc_info LIKE TABLE OF wa_mldoc WITH HEADER LINE.

DATA: wa_salk3 LIKE mlcr-salk3,
      wa_prdif LIKE mlcrf-prdif,
      wa_krdif LIKE mlcrf-krdif,
      wa_stprs_old LIKE mlhelp_mldoc_crf-stprs_old,
      wa_menge LIKE mlppf-menge.


DATA: BEGIN OF it_ztmm_analy OCCURS 0,
      period LIKE ztmm_analy-period,
      werks LIKE ztmm_analy-werks,
      matnr LIKE mara-matnr,
      valid_d LIKE ztmm_analy-valid_d,
      lifnr LIKE ztmm_analy-lifnr,
END OF it_ztmm_analy.

DATA: BEGIN OF it_ztmm_analy_new OCCURS 0,
      period LIKE ztmm_analy-period,
      werks LIKE ztmm_analy-werks,
      matnr LIKE mara-matnr,
      valid_d LIKE ztmm_analy-valid_d,
      lifnr LIKE ztmm_analy-lifnr,
END OF it_ztmm_analy_new.

DATA: it_ztmm_cost_anly LIKE TABLE OF ztmm_cost_anly WITH HEADER LINE.
DATA: BEGIN OF it_a902_info OCCURS 0,
        matnr   LIKE   mara-matnr,
        stawn   LIKE   marc-stawn,
        lifnr   LIKE   lfa1-lifnr,
        datab   LIKE   konh-datab,
        datbi   LIKE   konh-datbi,
        kschl   LIKE   konh-kschl,
        kzust   LIKE   konh-kzust,
        waers   LIKE   ekko-waers,
        peinh   LIKE   ekpo-peinh,
        kbetr   LIKE   ekpo-netpr,
END OF it_a902_info.

DATA: it_konp LIKE TABLE OF konp WITH HEADER LINE.

DATA: BEGIN OF it_error OCCURS 0,
        matnr    LIKE   mara-matnr,
        lifnr    LIKE   lfa1-lifnr,
        msg(50),
      END   OF it_error.


DATA: BEGIN OF it_knumh OCCURS 0,
        knumh LIKE konh-knumh,
        datab LIKE konh-datab,
        datbi LIKE konh-datbi,
      END   OF it_knumh.

DATA: BEGIN OF it_mara OCCURS 0,
          matnr   LIKE   mara-matnr,
      END   OF it_mara.

DATA: BEGIN OF it_info OCCURS 0,                "Info Condition
          matnr   LIKE   mara-matnr,
          lifnr   LIKE   lfa1-lifnr,
          datab   LIKE   konh-datab,
          datbi   LIKE   konh-datbi,
          kschl   LIKE   konh-kschl,
          kzust   LIKE   konh-kzust,
          waers   LIKE   ekko-waers,
          peinh   LIKE   ekpo-peinh,
          wrbtr   LIKE   ekpo-netpr,
          dmbtr   LIKE   ekpo-netpr,
      END   OF it_info.

DATA: BEGIN OF it_exchange_rate OCCURS 0,
        waers   LIKE   ekko-waers,         "Currency
        wkurs   TYPE   f,                  "Exchange rate
      END   OF it_exchange_rate.

DATA: wa_budat_f LIKE sy-datum,                     "Start of month
      wa_budat_t LIKE sy-datum,                     "End date of month
      wa_period  LIKE s001-spmon,                   "Period
      wa_valid_d  LIKE sy-datum,                     "Base date
      wa_matnr_f LIKE mara-matnr,                   "Start of material
      wa_matnr_t LIKE mara-matnr,                   "End of material
      wa_month LIKE mlcr-poper,
      wa_unitp LIKE ekpo-netpr,
      wa_value3 LIKE ekpo-netpr,
      wa_mldoc_prdif LIKE ekpo-netpr,
      wa_index LIKE sy-tabix,
      wa_nindex LIKE sy-tabix,
** FR
      wa_wrbtr   LIKE ekpo-netpr,                   "Foreign price
      wa_dmbtr   LIKE ekpo-netpr,
      wa_year    LIKE mlcr-bdatj.

DATA: it_bsis LIKE TABLE OF bsis WITH HEADER LINE.
DATA : BEGIN OF it_bseg OCCURS 0,
       belnr LIKE bseg-belnr,
       gjahr LIKE bseg-gjahr,
       menge LIKE bseg-menge,
       END OF it_bseg.

CONSTANTS: c_kschl LIKE konp-kschl  VALUE 'PB00',"Type of amount
           c_frght1 LIKE konp-kschl  VALUE 'FRA1',"Type of freight
           c_frght2 LIKE konp-kschl  VALUE 'ZFR1',"Type of freight
           c_duty  LIKE konp-kschl  VALUE 'ZOA1',"Type of duty
           c_con01 LIKE konp-kschl  VALUE 'ZOTH',"Type of ETC rate
           c_con02 LIKE konp-kschl  VALUE 'ZOTI',"Type of ETC rate
           c_tire LIKE konp-kschl  VALUE 'ZTIR',"Type of Tire
           c_waers LIKE ekko-waers  VALUE 'USD', "Currency
           c_ekorg LIKE ekko-ekorg  VALUE 'PU01',"Purchase Org.
           c_z01(3)                 VALUE 'Z01', "Diff of undetermined
           c_z02(3)                 VALUE 'Z02', "Diff of double vendor
           c_check                  VALUE 'X'.   "'X' mark

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

DATA  w_variant TYPE disvariant.


RANGES: s_hkont FOR bsis-hkont.

*----- Constants
CONSTANTS : c_formname_top_of_page TYPE slis_formname
                                        VALUE 'TOP_OF_PAGE',
            c_werks TYPE marc-werks VALUE 'P001'.

*----- Macro
DEFINE append_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : cur field      &8 : no zero          &9 : just

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
  w_fieldcat-no_zero    = &9.
*  no_out
  append w_fieldcat.
  clear : w_fieldcat.

END-OF-DEFINITION.

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_bukrs  LIKE t001t-bukrs DEFAULT 'H201' OBLIGATORY.
*PARAMETERS:     p_werks  LIKE t001w-werks DEFAULT 'P001' OBLIGATORY.
PARAMETERS:     p_month  LIKE s001-spmon DEFAULT sy-datum(6) OBLIGATORY.
SELECT-OPTIONS: s_matnr  FOR  mara-matnr. "NO-EXTENSION.
SELECTION-SCREEN END   OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS       r_year RADIOBUTTON GROUP rd1 USER-COMMAND rd1.
SELECTION-SCREEN COMMENT  3(20) text-t03 FOR FIELD r_year.
SELECTION-SCREEN POSITION 33.
PARAMETERS       r_quar RADIOBUTTON GROUP rd1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 35(30) text-t04 FOR FIELD r_quar.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK b13 WITH FRAME TITLE text-t05.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS       p_saved RADIOBUTTON GROUP rd2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(30) text-t06 FOR FIELD p_saved.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS       p_with RADIOBUTTON GROUP rd2.
SELECTION-SCREEN COMMENT 3(30) text-t07 FOR FIELD p_with.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS       p_whout RADIOBUTTON GROUP rd2.
SELECTION-SCREEN COMMENT 3(30) text-t08 FOR FIELD p_whout.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b13.
*** layout ***
SELECTION-SCREEN BEGIN OF BLOCK list WITH FRAME TITLE text-t09.
PARAMETERS: pa_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK list.

*Debugging
SELECT-OPTIONS: s_awref  FOR  mlhelp_mldoc_crf-awref.
SELECT-OPTIONS: s_aworg  FOR  mlhelp_mldoc_crf-aworg.

**** ----------------
*----- Initialization
INITIALIZATION.
  PERFORM set_initialization.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_vari.
  PERFORM alv_variant_f4 CHANGING pa_vari.


*----- Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_rtn.
  PERFORM read_data.

*----- Top-of-page
TOP-OF-PAGE.
*  PERFORM top_of_page.

START-OF-SELECTION.
  IF p_with = 'X'.
    PERFORM save_data.
  ENDIF.
  PERFORM display_data.
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  PERFORM read_base_date.
  IF p_saved = 'X'.
    PERFORM read_ztmm_cost_anly.
  ELSE.
    PERFORM read_mlhelp.
    PERFORM process_data.
  ENDIF.
ENDFORM.                    " read_data

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period.
  CASE c_check.
    WHEN r_year.
      CONCATENATE p_month(4) '13' INTO wa_period.
    WHEN r_quar.
      MOVE: p_month TO wa_period.
  ENDCASE.
  wa_month = p_month+4(2).
  wa_year = p_month(4).

  CONCATENATE p_month '01' INTO wa_budat_f.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = wa_budat_f
       IMPORTING
            last_day_of_month = wa_budat_t
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CHECK_PERIOD

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM build_fieldcat.
*  PERFORM build_event.
  PERFORM build_sort.
  PERFORM build_variant.
*  PERFORM comment_build USING  w_top_of_page[].
  PERFORM alv_function.
ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  CHECK_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM check_period.
  PERFORM check_material.
ENDFORM.                    " CHECK_RTN

*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_material.
  IF     s_matnr-low EQ ' ' AND s_matnr-high EQ ' '.
    wa_matnr_t = 'ZZZZZZZZZZZZZZZZZZ'.
  ELSEIF s_matnr-low NE ' ' AND s_matnr-high EQ ' '.
    wa_matnr_f = wa_matnr_t = s_matnr-low.
  ELSEIF s_matnr-low EQ ' ' AND s_matnr-high NE ' '.
    wa_matnr_t = s_matnr-high.
  ELSEIF s_matnr-low NE ' ' AND s_matnr-high NE ' '.
    wa_matnr_f = s_matnr-low. wa_matnr_t = s_matnr-high.
  ENDIF.

  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL

*---------------------------------------------------------------------*
*       FORM process_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_data.
  DATA: lw_stop_flg,
        lw_index LIKE sy-tabix.

  PERFORM display_progress_bar USING text-m11.
  REFRESH it_output.

  LOOP AT it_mldoc INTO wa_mldoc.
    lw_index = sy-tabix + 1.
    MOVE-CORRESPONDING wa_mldoc TO it_output.
    APPEND it_output.
*    COLLECT it_output.
    READ TABLE it_mldoc INDEX lw_index.
    IF sy-subrc NE 0.
      it_output-stprs_old = wa_mldoc-stprs_old.
      it_output-peinh = wa_mldoc-peinh.
      MODIFY it_output TRANSPORTING stprs_old peinh
                       WHERE matnr = wa_mldoc-matnr
                         AND lifnr = wa_mldoc-lifnr
                         AND posdate = wa_mldoc-posdate
                         AND rsncode = wa_mldoc-rsncode.
    ELSEIF NOT ( wa_mldoc-matnr = it_mldoc-matnr
                          AND wa_mldoc-lifnr = it_mldoc-lifnr
                          AND wa_mldoc-posdate = it_mldoc-posdate
                          AND wa_mldoc-rsncode = it_mldoc-rsncode ).

      it_output-stprs_old = wa_mldoc-stprs_old.
      it_output-peinh = wa_mldoc-peinh.
      MODIFY it_output TRANSPORTING stprs_old peinh
                       WHERE matnr = wa_mldoc-matnr
                         AND lifnr = wa_mldoc-lifnr
                         AND posdate = wa_mldoc-posdate
                         AND rsncode = wa_mldoc-rsncode.
    ENDIF.
  ENDLOOP.
  DELETE it_output WHERE prdif = 0.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM read_mlhelp                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM read_mlhelp.
  PERFORM read_mldoc_receipt.
  PERFORM read_info_record.
*  PERFORM read_duty_info.
  PERFORM calc_gr_price_hist.

  PERFORM read_mldoc_others.
  SORT it_mldoc BY matnr DESCENDING lifnr ASCENDING posdate rsncode.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM read_mldoc_receipt                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM read_mldoc_receipt.
  DATA: l_index LIKE syst-tabix.


  PERFORM display_progress_bar USING text-m08.

  CLEAR: it_mldoc_mkpf, it_mldoc_mkpf[].

*  SELECT a~bukrs a~bdatj a~poper a~feldg a~awtyp a~awref
*         a~vgart a~matnr a~meins a~bwkey a~peinh a~waers
*         a~salk3 a~stprs_old a~prdif a~krdif
*         b~menge e~bedat AS posdate e~lifnr a~bwkey AS werks   "c~bktxt
*         INTO TABLE it_mldoc_mkpf
*         FROM mlhelp_mldoc_crf AS a
*         INNER JOIN mlhelp_mldoc_ppf AS b
*         ON a~belnr = b~belnr
*         AND a~posnr = b~posnr
*         AND a~feldg = b~feldg
*         INNER JOIN ekbe AS d
*         ON a~awref = d~belnr
*         INNER JOIN ekko AS e
*         ON d~ebeln = e~ebeln
*         WHERE a~bdatj = wa_year
*           AND a~poper = wa_month
*           AND a~feldg = 'ZUO'
*           AND a~bukrs = p_bukrs
*           AND a~matnr IN s_matnr
*           AND a~awtyp = 'MKPF'
*           AND a~vgart = 'UP'
*           AND a~bewartgrp = '11'.

  SELECT a~bukrs a~bdatj a~poper a~feldg a~awtyp a~awref a~urzeile
         a~vgart a~matnr a~meins a~bwkey a~peinh a~waers
         a~salk3 a~stprs_old a~prdif a~krdif
         b~menge a~bwkey AS werks a~bldat
*         m~lifnr
         INTO CORRESPONDING FIELDS OF TABLE it_mldoc_mkpf
*         INTO TABLE it_mldoc_mkpf
         FROM mlhelp_mldoc_crf AS a
         INNER JOIN mlhelp_mldoc_ppf AS b
            ON a~belnr = b~belnr
           AND a~posnr = b~posnr
           AND a~feldg = b~feldg    "ZUO (same group)
*         INNER JOIN mseg AS m
*            ON m~mblnr = a~awref
*           AND m~mjahr = a~bdatj
*           AND m~zeile = a~urzeile  "this is not working...strange
*         INNER JOIN ekbe AS d
*         ON a~awref = d~belnr
*         INNER JOIN ekko AS e
*         ON d~ebeln = e~ebeln
         WHERE a~bdatj = wa_year
           AND a~poper = wa_month
           AND a~feldg = 'ZUO'
           AND a~bukrs = p_bukrs
           AND a~matnr IN s_matnr
           AND a~awtyp = 'MKPF'
           AND a~vgart = 'UP'
           AND a~bewartgrp = '11'
           AND a~awref IN s_awref
           AND a~aworg IN s_aworg.

* determine vendor due to performance...
  LOOP AT it_mldoc_mkpf.
    l_index = sy-tabix.
*    SELECT SINGLE lifnr INTO it_mldoc_mkpf-lifnr FROM eord
*           WHERE matnr = it_mldoc_mkpf-matnr
*             AND vdatu <= it_mldoc_mkpf-bldat
*             AND bdatu >= it_mldoc_mkpf-bldat
*             AND notkz EQ space
*             AND ebeln EQ space.
*    IF sy-subrc = 0.
*    ELSE.
    SELECT SINGLE lifnr ebeln ebelp
        INTO (it_mldoc_mkpf-lifnr,
              it_mldoc_mkpf-ebeln, it_mldoc_mkpf-ebelp)
        FROM mseg
        WHERE mblnr = it_mldoc_mkpf-awref
          AND mjahr = it_mldoc_mkpf-bdatj
          AND zeile = it_mldoc_mkpf-urzeile
          AND matnr = it_mldoc_mkpf-matnr.
*    ENDIF.
    MODIFY it_mldoc_mkpf INDEX l_index TRANSPORTING lifnr ebeln ebelp.
    CLEAR: it_mldoc_mkpf.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_mldoc_mkpf
        COMPARING bukrs awref matnr bwkey lifnr.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM read_mldoc_others                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM read_mldoc_others.
  DATA: l_buzei LIKE rseg-buzei,
        l_month LIKE bsis-monat.

  PERFORM display_progress_bar USING text-m12.

** Stock Transfer MKPF (vendor, posting date in MSEG table)
  SELECT a~bukrs a~bdatj a~poper a~feldg a~awtyp a~awref a~urzeile
         a~vgart a~matnr a~meins a~bwkey a~peinh a~waers
         a~salk3 a~stprs_old a~prdif a~krdif
         menge a~bwkey AS werks a~bldat c~budat  "c~bktxt
         INTO TABLE it_mldoc_temp
         FROM mlhelp_mldoc_crf AS a
         INNER JOIN mlhelp_mldoc_ppf AS b
            ON a~belnr = b~belnr
           AND a~posnr = b~posnr
           AND a~feldg = b~feldg
         INNER JOIN mkpf AS c
            ON a~awref = c~mblnr
            AND a~bdatj = c~mjahr
         WHERE a~bdatj = wa_year
           AND a~poper = wa_month
           AND a~feldg = 'ZUO'
           AND a~bukrs = p_bukrs
           AND a~matnr IN s_matnr
           AND a~awtyp = 'MKPF'
           AND a~vgart = 'UP'
           AND a~bewartgrp <> '11'
           AND a~awref IN s_awref
           AND a~aworg IN s_aworg.


  SORT it_mldoc_temp BY matnr.
  LOOP AT it_mldoc_temp.
    AT NEW matnr.
      CLEAR: wa_salk3,wa_stprs_old, wa_prdif, wa_krdif, wa_menge.
    ENDAT.
    MOVE-CORRESPONDING it_mldoc_temp TO it_mldoc.
    wa_salk3 = wa_salk3 + it_mldoc_temp-salk3.
*     wa_stprs_old = wa_stprs_old + it_mldoc_temp-stprs_old.
    wa_stprs_old = it_mldoc_temp-stprs_old.                 " / 10.
    wa_prdif = wa_prdif + it_mldoc_temp-prdif.
    wa_krdif = wa_krdif + it_mldoc_temp-krdif.
    wa_menge = wa_menge + it_mldoc_temp-menge.
    AT END OF matnr.
      MOVE: wa_salk3 TO it_mldoc-salk3,
            wa_stprs_old TO it_mldoc-stprs_old,
            wa_prdif TO it_mldoc-prdif,
            wa_krdif TO it_mldoc-krdif,
            wa_menge TO it_mldoc-menge,
            'ZST' TO it_mldoc-rsncode,
            c_stk_transfer TO it_mldoc-remark.
      APPEND it_mldoc.
      CLEAR it_mldoc.
    ENDAT.
  ENDLOOP.

  REFRESH it_mldoc_temp.
  CLEAR it_mldoc_temp.

*** Invoice -  RMRP
  SELECT a~bukrs a~bdatj a~poper a~feldg a~awtyp a~awref a~urzeile
         a~vgart a~matnr a~meins a~bwkey a~peinh a~waers
         a~salk3 a~stprs_old a~prdif a~krdif
         r~menge a~bwkey AS werks a~bldat h~budat    " h~bktxt
         r~kschl r~ebeln r~ebelp h~lifnr
         INTO CORRESPONDING FIELDS OF TABLE it_mldoc_temp
         FROM mlhelp_mldoc_crf AS a
           INNER JOIN rbkp AS h
              ON h~belnr   = a~awref
             AND h~gjahr   = a~bdatj
           INNER JOIN rseg AS r
              ON r~belnr = a~awref
             AND r~gjahr = a~bdatj
             AND r~buzei = a~urzeile
           INNER JOIN mlhelp_mldoc_ppf AS b
              ON b~belnr = a~belnr
             AND b~bdatj = wa_year
             AND b~poper = wa_month
             AND b~posnr = a~posnr
             AND b~feldg = 'PBO'    "PBO <> ZUO ???
*           INNER JOIN rbkp AS c
*              ON a~awref = c~belnr
*             AND a~bdatj = c~gjahr
         WHERE a~bdatj = wa_year
           AND a~poper = wa_month
           AND a~feldg = 'ZUO'
           AND a~bukrs = p_bukrs
           AND a~matnr IN s_matnr
           AND a~awtyp = 'RMRP'
           AND a~vgart = 'UP'
           AND a~awref IN s_awref
           AND a~aworg IN s_aworg.

  SORT it_mldoc_temp BY matnr.
  LOOP AT it_mldoc_temp.
    AT NEW matnr.
      CLEAR: wa_salk3,wa_stprs_old, wa_prdif, wa_krdif, wa_menge.
    ENDAT.
    MOVE-CORRESPONDING it_mldoc_temp TO it_mldoc.
    wa_salk3 = wa_salk3 + it_mldoc_temp-salk3.
*     wa_stprs_old = wa_stprs_old + it_mldoc_temp-stprs_old.
    wa_stprs_old = it_mldoc_temp-stprs_old.                 "   / 10.
    wa_prdif = wa_prdif + it_mldoc_temp-prdif.
    wa_krdif = wa_krdif + it_mldoc_temp-krdif.
    wa_menge = wa_menge + it_mldoc_temp-menge.

    AT END OF matnr.
      MOVE: wa_salk3 TO it_mldoc-salk3,
            wa_stprs_old TO it_mldoc-stprs_old,
            wa_prdif TO it_mldoc-prdif,
            wa_krdif TO it_mldoc-krdif,
            wa_menge TO it_mldoc-menge,
            'ZIV' TO it_mldoc-rsncode,
            c_invoice TO it_mldoc-remark.
** get vendor
*      SELECT SINGLE lifnr INTO it_mldoc-lifnr
*         FROM rbkp
*         WHERE belnr = it_mldoc-awref
*           AND gjahr = p_month(4).

      APPEND it_mldoc.
      CLEAR it_mldoc.
    ENDAT.
  ENDLOOP.

  REFRESH it_mldoc_temp.
  CLEAR it_mldoc_temp.

*** KD - Import expenses   PRCHG

  SELECT a~bukrs a~bdatj a~poper a~feldg a~awtyp a~awref a~urzeile
         a~vgart a~matnr a~meins a~bwkey a~peinh a~waers
         a~salk3 a~stprs_old a~prdif a~krdif
         menge                     " c~budat c~bktxt
         INTO TABLE it_mldoc_temp
         FROM mlhelp_mldoc_crf AS a
         INNER JOIN mlhelp_mldoc_ppf AS b
         ON a~belnr = b~belnr
         AND a~posnr = b~posnr
         AND a~feldg = b~feldg
         WHERE a~bdatj = wa_year
           AND a~poper = wa_month
           AND a~feldg = 'UMO'
           AND a~bukrs = p_bukrs
           AND a~matnr IN s_matnr
           AND a~awtyp = 'PRCHG'
           AND a~vgart = 'PC'
           AND a~psart = 'DC'
           AND a~awref IN s_awref
           AND a~aworg IN s_aworg.

  SORT it_mldoc_temp BY matnr.
  LOOP AT it_mldoc_temp.
    AT NEW matnr.
      CLEAR: wa_salk3,wa_stprs_old, wa_prdif, wa_krdif, wa_menge.
    ENDAT.
    MOVE-CORRESPONDING it_mldoc_temp TO it_mldoc.
    wa_salk3 = wa_salk3 + it_mldoc_temp-salk3.
*     wa_stprs_old = wa_stprs_old + it_mldoc_temp-stprs_old.
    wa_stprs_old = it_mldoc_temp-stprs_old.                 " / 10.
    wa_prdif = wa_prdif + it_mldoc_temp-prdif.
    wa_krdif = wa_krdif + it_mldoc_temp-krdif.
    wa_menge = wa_menge + it_mldoc_temp-menge.

    AT END OF matnr.
      MOVE: wa_salk3 TO it_mldoc-salk3,
            wa_stprs_old TO it_mldoc-stprs_old,
            wa_prdif TO it_mldoc-prdif,
            wa_krdif TO it_mldoc-krdif,
            wa_menge TO it_mldoc-menge,
            c_import TO it_mldoc-remark.
      IF it_mldoc_temp-bktext = 'IMPORT SETTLEMENT'.
        MOVE 'ZBL' TO it_mldoc-rsncode.
      ELSE.
        MOVE 'ZBX' TO it_mldoc-rsncode.
      ENDIF.
      APPEND it_mldoc.
      CLEAR it_mldoc.
    ENDAT.
  ENDLOOP.

  REFRESH it_mldoc_temp.
  CLEAR it_mldoc_temp.

*** Standard price change PRCHG

  SELECT a~bukrs a~bdatj a~poper a~feldg a~awtyp a~awref a~urzeile
           a~vgart a~matnr a~meins a~bwkey a~peinh a~waers
           a~salk3 a~stprs_old a~prdif a~krdif
           menge               " c~budat c~bktxt
           INTO TABLE it_mldoc_temp
           FROM mlhelp_mldoc_crf AS a
           INNER JOIN mlhelp_mldoc_ppf AS b
           ON a~belnr = b~belnr
           AND a~posnr = b~posnr
           AND a~feldg = b~feldg
           WHERE a~bdatj = wa_year
             AND a~poper = wa_month
             AND a~feldg = 'UMO'
             AND a~bukrs = p_bukrs
             AND a~matnr IN s_matnr
             AND a~awtyp = 'PRCHG'
             AND a~vgart = 'PC'
             AND a~psart = 'PC'
             AND a~awref IN s_awref
             AND a~aworg IN s_aworg.


  SORT it_mldoc_temp BY matnr.
  LOOP AT it_mldoc_temp.
    AT NEW matnr.
      CLEAR: wa_salk3,wa_stprs_old, wa_prdif, wa_krdif, wa_menge.
    ENDAT.
    MOVE-CORRESPONDING it_mldoc_temp TO it_mldoc.
    wa_salk3 = wa_salk3 + it_mldoc_temp-salk3.
*     wa_stprs_old = wa_stprs_old + it_mldoc_temp-stprs_old.
    wa_stprs_old = it_mldoc_temp-stprs_old.                 " / 10.
    wa_prdif = wa_prdif + it_mldoc_temp-prdif.
    wa_krdif = wa_krdif + it_mldoc_temp-krdif.
    wa_menge = wa_menge + it_mldoc_temp-menge.

    AT END OF matnr.

      MOVE: wa_salk3 TO it_mldoc-salk3,
            wa_stprs_old TO it_mldoc-stprs_old,
            wa_prdif TO it_mldoc-prdif,
            wa_krdif TO it_mldoc-krdif,
            wa_menge TO it_mldoc-menge,
            'ZPC' TO it_mldoc-rsncode,
            c_price TO it_mldoc-remark.
      APPEND it_mldoc.
      CLEAR it_mldoc.
    ENDAT.
  ENDLOOP.
*
*  if it_mldoc is initial.
*    MESSAGE s000(zz) WITH text-m13.
*  endif.

  REFRESH it_mldoc_temp.
  CLEAR it_mldoc_temp.

*** Revaluation - price change

  REFRESH it_bseg.
  CLEAR it_bseg.

  s_hkont-sign = 'I'.
  s_hkont-option = 'EQ'.
  s_hkont-low = '0000530180'.
  APPEND s_hkont.
  CLEAR: s_hkont.
  s_hkont-sign = 'I'.
  s_hkont-option = 'EQ'.
  s_hkont-low = '0000532100'.
  APPEND s_hkont.

  l_month = wa_month.
  SELECT * INTO TABLE it_bsis
         FROM bsis
         WHERE bukrs = p_bukrs
           AND gjahr = wa_year
           AND monat = l_month
           AND zuonr IN s_matnr
           AND hkont IN s_hkont.
*           AND hkont IN ('530180', '532100').

*  SELECT belnr gjahr menge INTO TABLE it_bseg FROM bseg
*         FOR ALL ENTRIES IN it_bsis
*         WHERE BUKRS = p_bukrs
*           AND belnr = it_bsis-belnr
*           AND gjahr = it_bsis-gjahr.

*  LOOP AT it_bsis.
*    READ TABLE it_bseg WITH KEY belnr = it_bsis-belnr
*                                gjahr = it_bsis-gjahr.
*    MOVE: it_bseg-menge TO it_mldoc_temp-menge,
*          it_bsis-bukrs TO it_mldoc_temp-bukrs,
*          it_bsis-gjahr TO it_mldoc_temp-bdatj,
*          it_bsis-monat TO it_mldoc_temp-poper,
*          it_bsis-zuonr TO it_mldoc_temp-matnr,
*          it_bsis-waers TO it_mldoc_temp-waers,
*          it_bsis-dmbtr TO it_mldoc_temp-prdif.
*    APPEND it_mldoc_temp.
*    CLEAR it_mldoc_temp.
*  ENDLOOP.

  LOOP AT it_bsis.
    l_buzei = it_bsis-buzei - 1.
    SELECT SINGLE menge INTO it_mldoc_temp-menge
        FROM rseg
        WHERE belnr = it_bsis-belnr
          AND gjahr = it_bsis-gjahr
          AND buzei = l_buzei.

    MOVE: it_bsis-bukrs TO it_mldoc_temp-bukrs,
          it_bsis-gjahr TO it_mldoc_temp-bdatj,
          it_bsis-monat TO it_mldoc_temp-poper,
          it_bsis-zuonr TO it_mldoc_temp-matnr,
          it_bsis-waers TO it_mldoc_temp-waers,
          it_bsis-dmbtr TO it_mldoc_temp-prdif.
    APPEND it_mldoc_temp.
    CLEAR it_mldoc_temp.
  ENDLOOP.

*  if it_mldoc_temp is initial.
*     MESSAGE s000(zz) WITH text-m14.
*  endif.

  SORT it_mldoc_temp BY matnr.
  LOOP AT it_mldoc_temp.
    AT NEW matnr.

      CLEAR: wa_salk3,wa_stprs_old, wa_prdif, wa_krdif, wa_menge.
    ENDAT.
    MOVE-CORRESPONDING it_mldoc_temp TO it_mldoc.
    wa_prdif = wa_prdif + it_mldoc_temp-prdif.
    wa_menge = wa_menge + it_mldoc_temp-menge.

    AT END OF matnr.
      MOVE: wa_prdif TO it_mldoc-prdif,
            wa_menge TO it_mldoc-menge,
            'ZRV' TO it_mldoc-rsncode,
            c_revaluation TO it_mldoc-remark.
      APPEND it_mldoc.
      CLEAR it_mldoc.
    ENDAT.
  ENDLOOP.

  REFRESH it_mldoc_temp.
  CLEAR it_mldoc_temp.

  READ TABLE it_mldoc INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.

ENDFORM.                    " read_gr_info
*&---------------------------------------------------------------------*
*&      Form  read_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_info_record.
  DATA: l_matnr like mara-matnr.
  DATA: lw_umrez LIKE eina-umrez,        "Numerator
        lw_umren LIKE eina-umren.        "Denomirator

*----- Read Info record
  PERFORM display_progress_bar USING text-m09.
*
  CLEAR: it_knumh, it_knumh[].
  SELECT knumh datab datbi
    INTO TABLE it_knumh
    FROM a018
     FOR ALL ENTRIES IN it_ztmm_analy
     WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  it_ztmm_analy-matnr
     AND lifnr =  it_ztmm_analy-lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datbi >= it_ztmm_analy-valid_d
     AND datab <= wa_budat_t.

  READ TABLE it_knumh INDEX 1.
  IF sy-subrc NE 0.
    MOVE: 'NIGIMIJOTO' TO it_knumh-knumh.
    APPEND it_knumh.
  ENDIF.

  SORT it_knumh BY datab DESCENDING.

  DATA: it_zvmm_info_condi LIKE zvmm_info_condi OCCURS 0
        WITH HEADER LINE.

  CLEAR: it_zvmm_info_condi, it_zvmm_info_condi[].

* KONH, KONP
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_zvmm_info_condi
    FROM zvmm_info_condi
     FOR ALL ENTRIES IN it_knumh
   WHERE knumh = it_knumh-knumh
*     AND kschl IN (c_kschl,c_frght1,c_frght1,c_con01,c_con02,c_tire)
     AND kschl_konh = 'PB00'
     AND loevm_ko = ' '.


  SORT it_zvmm_info_condi BY knumh kopos.
  LOOP AT it_zvmm_info_condi.
    CLEAR: it_info.

    l_matnr = it_zvmm_info_condi-vakey+10(18).

* Andy: Comment
*    READ TABLE it_mldoc_mkpf WITH KEY matnr =
*                                 it_zvmm_info_condi-vakey+10(18)
*                                 lifnr = it_zvmm_info_condi-vakey(10).
*
*    IF sy-subrc NE 0.
**      MESSAGE e000(zz) WITH text-m01.
*      CONTINUE.
*    ENDIF.

    READ TABLE it_knumh WITH KEY knumh = it_zvmm_info_condi-knumh.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    READ TABLE it_info WITH KEY matnr = l_matnr
*                               lifnr = it_zvmm_info_condi-vakey(10)
                                datab = it_knumh-datab.
    IF sy-subrc NE 0.           "Append Material cost, duty
*----- Append Material Cost
      MOVE: l_matnr                       TO it_info-matnr,
* it_mldoc_mkpf-matnr
*           it_mldoc_mkpf-lifnr           TO it_info-lifnr, "ANDY
            it_knumh-datab                TO it_info-datab,
            it_knumh-datbi                TO it_info-datbi,
            it_zvmm_info_condi-kschl      TO it_info-kschl,
            it_zvmm_info_condi-kzust      TO it_info-kzust,
            it_zvmm_info_condi-konwa      TO it_info-waers.

      it_info-wrbtr = it_zvmm_info_condi-kbetr.
      it_info-peinh = it_zvmm_info_condi-kpein.
* determine price unit...
*      IF it_zvmm_info_condi-kmein EQ it_mldoc_mkpf-meins.
*        it_info-peinh = it_zvmm_info_condi-kpein.
*      ELSE.
*        SELECT SINGLE umrez umren
*          INTO (lw_umrez, lw_umren)
*          FROM eina
*         WHERE matnr = it_mldoc_mkpf-matnr
*           AND lifnr = it_mldoc_mkpf-lifnr.
*        IF sy-subrc NE 0.
*          MESSAGE e000(zz) WITH text-m01.
*        ENDIF.
*
*        it_info-peinh = it_zvmm_info_condi-kpein *
*                        it_zvmm_info_condi-kumza /
*                        it_zvmm_info_condi-kumne *
*                        lw_umrez      / lw_umren.
*      ENDIF.

      PERFORM get_local_amount USING it_info-waers
                                     it_knumh-datab
                                     it_info-wrbtr
                                     it_info-dmbtr.

      APPEND it_info.

      MOVE: it_info-wrbtr TO wa_wrbtr,
            it_info-dmbtr TO wa_dmbtr.
    ELSE.
*----- Append Freight, Other costs
      CASE it_zvmm_info_condi-kschl.
        WHEN c_frght1.
          IF it_zvmm_info_condi-kbetr <> 0.
            MOVE:
*                  it_mldoc_mkpf-matnr                TO it_info-matnr,
*                  it_mldoc_mkpf-lifnr                TO it_info-lifnr,
*                  it_zvmm_info_condi-datab      TO it_info-datab,
                  it_zvmm_info_condi-datbi      TO it_info-datbi,
                  it_zvmm_info_condi-kschl      TO it_info-kschl,
                  it_zvmm_info_condi-kzust      TO it_info-kzust,
                  it_zvmm_info_condi-konwa      TO it_info-waers,
                  it_zvmm_info_condi-kpein      TO it_info-peinh.

            it_info-wrbtr = wa_wrbtr * it_zvmm_info_condi-kbetr / 1000.
            it_info-dmbtr = wa_dmbtr * it_zvmm_info_condi-kbetr / 1000.

            APPEND it_info.
          ENDIF.
        WHEN c_frght2.
          IF it_zvmm_info_condi-kbetr <> 0.
            MOVE: l_matnr                       TO it_info-matnr,
*                 it_mldoc_mkpf-lifnr           TO it_info-lifnr,
                  it_zvmm_info_condi-datab      TO it_info-datab,
                  it_zvmm_info_condi-datbi      TO it_info-datbi,
                  it_zvmm_info_condi-kschl      TO it_info-kschl,
                  it_zvmm_info_condi-kzust      TO it_info-kzust,
                  it_zvmm_info_condi-konwa      TO it_info-waers,
                  it_zvmm_info_condi-kpein      TO it_info-peinh.

            it_info-wrbtr = wa_wrbtr * it_zvmm_info_condi-kbetr / 1000.
            it_info-dmbtr = wa_dmbtr * it_zvmm_info_condi-kbetr / 1000.

            APPEND it_info.
          ENDIF.
*        WHEN c_tire.
*          IF it_zvmm_info_condi-kbetr <> 0.
*            MOVE: it_mldoc-matnr                TO it_info-matnr,
*** ??             it_mldoc-lifnr                TO it_info-lifnr,
*                  it_zvmm_info_condi-datab      TO it_info-datab,
*                  it_zvmm_info_condi-datbi      TO it_info-datbi,
*                  it_zvmm_info_condi-kschl      TO it_info-kschl,
*                  it_zvmm_info_condi-kzust      TO it_info-kzust,
*                  it_zvmm_info_condi-konwa      TO it_info-waers,
*                  it_zvmm_info_condi-kpein      TO it_info-peinh.
*
*            it_info-wrbtr = wa_wrbtr * it_zvmm_info_condi-kbetr / 1000.
*            it_info-dmbtr = wa_dmbtr * it_zvmm_info_condi-kbetr / 1000.
*
*            APPEND it_info.
*          ENDIF.
        WHEN OTHERS.
          CHECK it_zvmm_info_condi-kbetr NE 0.
          READ TABLE it_info WITH KEY matnr = l_matnr
*                                     lifnr = it_mldoc_mkpf-lifnr
                                      datab = it_zvmm_info_condi-datab
                                      kschl = 'ETC'.
          IF sy-subrc EQ 0.
            it_info-wrbtr = it_info-wrbtr + wa_wrbtr *
                            it_zvmm_info_condi-kbetr / 1000.
            it_info-dmbtr = it_info-dmbtr + wa_dmbtr *
                            it_zvmm_info_condi-kbetr / 1000.
            MODIFY it_info INDEX sy-tabix.
          ELSE.
            MOVE: l_matnr                       TO it_info-matnr,
*                 it_mldoc_mkpf-lifnr           TO it_info-lifnr,
                  it_zvmm_info_condi-datab      TO it_info-datab,
                  it_zvmm_info_condi-datbi      TO it_info-datbi,
                  'ETC'                         TO it_info-kschl,
                  it_zvmm_info_condi-kzust      TO it_info-kzust,
                  it_zvmm_info_condi-konwa      TO it_info-waers,
                  it_zvmm_info_condi-kpein      TO it_info-peinh.

            it_info-wrbtr = wa_wrbtr * it_zvmm_info_condi-kbetr / 1000.
            it_info-dmbtr = wa_dmbtr * it_zvmm_info_condi-kbetr / 1000.

            APPEND it_info.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " read_info_record
*&---------------------------------------------------------------------*
*&      Form  get_local_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_INFO_WRBTR  text
*      -->P_IT_INFO_DMBTR  text
*----------------------------------------------------------------------*
FORM get_local_amount USING pw_waers pw_datum pw_wrbtr pw_dmbtr.
  DATA: lw_wkurs TYPE f VALUE 1.

  READ TABLE it_exchange_rate WITH KEY waers = pw_waers.
  IF sy-subrc NE 0.
    CALL FUNCTION 'Z_FCA_GET_EXCHANGE_RATE'
         EXPORTING
              client                  = sy-mandt
              date                    = pw_datum
              source_currency         = pw_waers
              target_currency         = c_waers
              company_currency        = c_waers
              type_of_rate            = 'P'
         IMPORTING
              exchange_rate           = lw_wkurs
         EXCEPTIONS
              target_local_rate_error = 1
              source_local_rate_error = 2
              OTHERS                  = 3.
    IF sy-subrc <> 0.
*      MESSAGE e000(zz) WITH text-m03 it_price-waers.
    ENDIF.

    MOVE: pw_waers TO it_exchange_rate-waers,
          lw_wkurs TO it_exchange_rate-wkurs.

    APPEND it_exchange_rate.
  ENDIF.

  pw_dmbtr = pw_wrbtr * lw_wkurs.
ENDFORM.                    " get_local_amount
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

    w_col_pos 'MATNR'  18 text-001 'CHAR'  'X' ''      '' '',
    w_col_pos 'LIFNR'  10 text-002 'CHAR'  ' ' ''      '' '',
    w_col_pos 'POSDATE'  10 text-003 'DATS'  ''  ''      '' '',
    w_col_pos 'RSNCODE'  06 text-012 'CHAR'  ''  ''      '' '',
*    w_col_pos 'VTEXT'    20 text-014 'CHAR'  ''  ''      '' '',
    w_col_pos 'AWREF'  10  text-015 'CHAR'  ''  ''      '' '',
    w_col_pos 'URZEILE'  6 'item'   'CHAR'  ''  ''      '' '',
    w_col_pos 'KSCHL'    5 'Condition' 'CHAR'  ''  ''      '' '',
    w_col_pos 'EBELN'   10 'PO' 'CHAR'  ''  ''      '' '',
    w_col_pos 'EBELP'    5 'POItm' 'CHAR'  ''  ''      '' '',
    w_col_pos 'WAERS'  05 text-004 'CUKY'  ''  ''      '' '',
    w_col_pos 'PEINH'  05 text-005 'DEC'   ''  ''      '' '',
    w_col_pos 'MENGE'  17 text-006 'QUAN'  ''  '' 'MEINS' '',
    w_col_pos 'MEINS'  03 text-007 'UNIT'  ''  ''      '' '',
    w_col_pos 'STPRS_OLD'  16 text-009 'CURR'  ''  'WAERS' '' '',
    w_col_pos 'PRDIF'  16 text-010 'CURR'  ''  'WAERS' '' '',
    w_col_pos 'KRDIF'  16 text-011 'CURR'  ''  'WAERS' '' '',
    w_col_pos 'SALK3'  16 text-008 'CURR'  ''  'WAERS' '' '',
    w_col_pos 'REMARK'  40 text-013 'CHAR'  ''  '' '' ''.
ENDFORM.                    " build_fieldcat
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
*&---------------------------------------------------------------------*
*&      Form  build_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort.
  w_sortcat-spos           = 1.
  w_sortcat-fieldname      = 'MATNR'.
  w_sortcat-tabname        = 'IT_OUTPUT'.
  w_sortcat-up             = 'X'.
  w_sortcat-subtot         = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 2.
  w_sortcat-fieldname      = 'LIFNR'.
  w_sortcat-tabname        = 'IT_OUTPUT'.
  w_sortcat-subtot         = 'X'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

*  w_sortcat-spos           = 3.
*  w_sortcat-fieldname      = 'ZZSEQ'.
*  w_sortcat-tabname        = 'IT_HISTORY'.
*  w_sortcat-up             = 'X'.
*  APPEND w_sortcat.
ENDFORM.                    " build_sort
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING  lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_manager(50),
        l_date(50),
        l_list(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_ldate(10),
        l_hdate(10).

*----- Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-h01.
  APPEND ls_line TO lt_top_of_page.

*----- Plant
  ls_line-typ  = 'S'.
  ls_line-key  = text-h02.
  MOVE: p_bukrs TO ls_line-info.
  APPEND ls_line TO lt_top_of_page.

*----- Month
  ls_line-typ  = 'S'.
  ls_line-key  = text-h03.
  WRITE: p_month TO ls_line-info.
  APPEND ls_line TO lt_top_of_page.

*----- Material
  ls_line-typ  = 'S'.
  ls_line-key  = text-h04.
  CONCATENATE s_matnr-low '~' s_matnr-high INTO ls_line-info
              SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.
ENDFORM.                    " comment_build
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
            is_variant               = w_variant
            it_events                = w_eventcat[]
            is_print                 = l_print_p
       TABLES
            t_outtab                 = it_output
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " alv_function
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
*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
  CASE ucomm.
    WHEN '&DATA_SAVE'.
***      PERFORM save_rtn.
    WHEN '&MESSAGE'.
      PERFORM display_message.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  save_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM save_rtn.
*  DELETE FROM ztmm_diff_his WHERE spmon = p_month
*                              AND werks = p_werks
*                              AND matnr IN s_matnr.
*
*  INSERT ztmm_diff_his FROM TABLE it_history
*         ACCEPTING DUPLICATE KEYS.
*  IF sy-subrc EQ 0.
*    COMMIT WORK AND WAIT.
*    MESSAGE s000(zz) WITH text-m05.
*  ELSE.
*    ROLLBACK WORK.
*    MESSAGE e000(zz) WITH text-m06.
*  ENDIF.
*ENDFORM.                    " save_rtn
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_message.
  CALL SCREEN 9000 STARTING AT 15  1
                   ENDING   AT 96 10.
ENDFORM.                    " DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*&      Module  display_list  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_list OUTPUT.
  SET PF-STATUS 'ERROR'.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING.
  ULINE.
  LOOP AT it_error.
    WRITE:/ '|' NO-GAP, (18) it_error-matnr NO-GAP COLOR COL_KEY,
            '|' NO-GAP,      it_error-lifnr NO-GAP COLOR COL_KEY,
            '|' NO-GAP,      it_error-msg   NO-GAP COLOR COL_NORMAL,
            '|' NO-GAP.
  ENDLOOP.
  ULINE.
ENDMODULE.                 " display_list  OUTPUT

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  set_initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_initialization.
  IF sy-datum+4(2) EQ '01'.
    p_month(4)   = sy-datum(4) - 1.
    p_month+4(2) = '12'.
  ELSE.
    p_month = sy-datum(6) - 1.
  ENDIF.
ENDFORM.                    " set_initialization
*&---------------------------------------------------------------------*
*&      Form  read_annual_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_annual_plan.
ENDFORM.                    " read_annual_plan
*&---------------------------------------------------------------------*
*&      Form  read_standard_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_standard_plan.
ENDFORM.                    " read_standard_plan
*&---------------------------------------------------------------------*
*&      Form  APPEND_ANNUAL_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_annual_plan.
ENDFORM.                    " APPEND_ANNUAL_PLAN
*&---------------------------------------------------------------------*
*&      Form  NO_AUNNAL_BUSINESS_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM no_aunnal_business_plan.
*  EXEC SQL.
*    SELECT B.PERIOD, B.BASE_D,
*           B.EKORG,  B.KZUST, B.WRBTR, B.WDUTY,
*           B.WFRGT,  B.WCOST, B.DMBTR, B.DDUTY,
*           B.DFRGT,  B.DCOST, B.PEINH, B.WAERS,
*           B.WKURS,  B.SOURCE,E.KBETR, B.LIFNR,
*           B.VALID_D
*      INTO :IT_STD_PRICE-PERIOD, :IT_STD_PRICE-BASE_D,
*           :IT_STD_PRICE-EKORG,  :IT_STD_PRICE-KZUST,
*           :IT_STD_PRICE-WRBTR,  :IT_STD_PRICE-WDUTY,
*           :IT_STD_PRICE-WFRGT,  :IT_STD_PRICE-WCOST,
*           :IT_STD_PRICE-DMBTR,  :IT_STD_PRICE-DDUTY,
*           :IT_STD_PRICE-DFRGT,  :IT_STD_PRICE-DCOST,
*           :IT_STD_PRICE-PEINH,  :IT_STD_PRICE-WAERS,
*           :IT_STD_PRICE-WKURS,  :IT_STD_PRICE-SOURCE,
*           :IT_STD_PRICE-KBETR,  :IT_STD_PRICE-LIFNR,
*           :IT_STD_PRICE-VALID_D
*      FROM ( SELECT MANDT, WERKS, MATNR, MAX(PERIOD) PERIOD
*               FROM ZTMM_ANALY
*              WHERE MANDT  =  :SY-MANDT
*                AND PERIOD <= :P_MONTH
*                AND WERKS  =  :IT_STD_PRICE-WERKS
*                AND MATNR  =  :IT_STD_PRICE-MATNR
*                AND SUBMT  = ' '
*              GROUP BY MANDT, WERKS, MATNR) A,
*           ZTMM_ANALY B, MARC C, A902 D, KONP E
*     WHERE B.MANDT     = A.MANDT
*       AND B.PERIOD    = A.PERIOD
*       AND B.WERKS     = A.WERKS
*       AND B.MATNR     = A.MATNR
*       AND B.SUBMT     = ' '
*       AND C.MANDT     =  B.MANDT
*       AND C.MATNR     =  B.MATNR
*       AND C.WERKS     =  :P_WERKS
*       AND C.LVORM     =  ' '
*       AND D.MANDT(+)  =  C.MANDT
*       AND D.KAPPL(+)  =  'M'
*       AND D.KSCHL(+)  =  'ZOA1'
*       AND D.STAWN(+)  =  C.STAWN
*       AND E.MANDT(+)  =  D.MANDT
*       AND E.KNUMH(+)  =  D.KNUMH
*  ENDEXEC.
ENDFORM.                    " NO_AUNNAL_BUSINESS_PLAN
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_M10  text
*----------------------------------------------------------------------*
FORM display_progress_bar USING p_text.
  DATA: lw_text(50).

  MOVE: p_text TO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lw_text.
ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  read_based_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_base_date.

  SELECT period werks matnr valid_d lifnr
               INTO TABLE it_ztmm_analy
               FROM ztmm_analy
              WHERE period <= wa_period
                AND werks  =  'P001'            "P_WERKS
                AND matnr  IN s_matnr.

  SORT it_ztmm_analy BY werks matnr period DESCENDING.


  SELECT period werks matnr valid_d lifnr
               INTO TABLE it_ztmm_analy_new
               FROM ztmm_analy_new
              WHERE period <= wa_period
                AND werks  =  'P001'            "P_WERKS
                AND matnr  IN s_matnr.

  SORT it_ztmm_analy_new BY werks matnr period DESCENDING.

  DELETE ADJACENT DUPLICATES FROM it_ztmm_analy
                             COMPARING matnr lifnr.
  DELETE ADJACENT DUPLICATES FROM it_ztmm_analy_new
                             COMPARING matnr lifnr.

*  loop at it_ztmm_analy_new.
*     read table it_ztmm_analy with key werks = it_ztmm_analy_new-werks
*                                       matnr = it_ztmm_analy_new-matnr
*                                       lifnr = it_ztmm_analy_new-lifnr.
*     if sy-subrc ne 0.
*        append it_ztmm_analy_new to it_ztmm_analy.
*     endif.
*  endloop.

ENDFORM.                    " read_based_date
*&---------------------------------------------------------------------*
*&      Form  read_ztmm_cost_anly
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_ztmm_cost_anly.
  SELECT * INTO TABLE it_output FROM ztmm_cost_anly
          WHERE bukrs = p_bukrs
            AND bdatj = wa_year
            AND poper = wa_month
            AND matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-w02.
  ENDIF.
ENDFORM.                    " read_ztmm_cost_anly
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  DATA: w_matnr LIKE mara-matnr.
  REFRESH it_ztmm_cost_anly.
  CLEAR: it_ztmm_cost_anly.
  READ TABLE it_output INDEX 1.
  SELECT SINGLE matnr INTO w_matnr FROM ztmm_cost_anly
           WHERE bukrs = p_bukrs
             AND bdatj = wa_year
             AND poper = wa_month
             AND matnr IN s_matnr.
  IF sy-subrc EQ 0.
    DATA: w_answer(1).
    CALL FUNCTION 'POPUP_TO_CONFIRM'
         EXPORTING
              text_question  = 'Do you want to delete existing data?'
         IMPORTING
              answer         = w_answer
         EXCEPTIONS
              text_not_found = 1
              OTHERS         = 2.
    IF w_answer = '1'.
      DELETE FROM ztmm_cost_anly
         WHERE bukrs = p_bukrs
           AND bdatj = wa_year
           AND poper = wa_month
           AND matnr IN s_matnr.
      LOOP AT it_output.
        MOVE-CORRESPONDING it_output TO it_ztmm_cost_anly.
        it_ztmm_cost_anly-uname = sy-uname.
        APPEND it_ztmm_cost_anly.
      ENDLOOP.
      MODIFY ztmm_cost_anly FROM TABLE it_ztmm_cost_anly.
      COMMIT WORK.
    ENDIF.
  ELSE.
    MODIFY ztmm_cost_anly FROM TABLE it_output.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " save_data

*---------------------------------------------------------------------*
*       FORM alv_variant_f4                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_VARI                                                       *
*---------------------------------------------------------------------*
FORM alv_variant_f4 CHANGING pa_vari.
  DATA: rs_variant LIKE disvariant.
  DATA nof4 TYPE c.

  CLEAR nof4.
  LOOP AT SCREEN.
    IF screen-name = 'PA_VARI'.
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
    pa_vari = rs_variant-variant.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  build_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_variant.
  DATA  lw_variant TYPE disvariant.
  w_variant-report = sy-repid.
  w_variant-username = sy-uname.
  w_variant-variant = pa_vari.
*  append lw_variant to w_variant.
ENDFORM.                    " build_variant
*&---------------------------------------------------------------------*
*&      Form  read_duty_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_duty_info.
  LOOP AT it_mldoc_mkpf.
    MOVE it_mldoc_mkpf-matnr TO it_mara.
    COLLECT it_mara.
  ENDLOOP.

  SELECT a~matnr b~stawn   "  c~datab c~datbi c~kzust
         c~kschl c~kbetr
         INTO CORRESPONDING FIELDS OF TABLE it_a902_info
    FROM marc AS a
    INNER JOIN a902 AS b
    ON a~stawn = b~stawn
    INNER JOIN konp AS c
    ON b~knumh = c~knumh
    FOR ALL ENTRIES IN it_mara
    WHERE a~matnr = it_mara-matnr
      AND a~werks  = c_werks
      AND b~kappl = 'M'
      AND b~kschl = 'ZOA1'.

ENDFORM.                    " read_duty_info
*&---------------------------------------------------------------------*
*&      Form  calc_gr_price_hist
*&---------------------------------------------------------------------*
*
* Only consider PB00 ?
* Duty and other delivery cost is handled by invoice difference.
*
FORM calc_gr_price_hist.
  DATA: wa_base(1),
        wa_duty_rate LIKE konp-kbetr,
        wa_duty LIKE ekpo-netpr,
        wa_pre_value LIKE ekpo-netpr,
        wa_diff_value LIKE ekpo-netpr.


  SORT it_info BY matnr lifnr DESCENDING datab.     " ascending kschl.
  LOOP AT it_mldoc_mkpf.
*    MOVE-CORRESPONDING it_mldoc_mkpf TO it_mldoc.
    it_mldoc = it_mldoc_mkpf.
    it_mldoc-remark = c_receipt.

*    it_mldoc-stprs_old = it_mldoc-stprs_old / 10.

** read duty
    CLEAR: wa_duty.
    READ TABLE it_a902_info WITH KEY matnr = it_mldoc_mkpf-matnr.
    IF sy-subrc EQ 0.
      wa_duty_rate  = it_a902_info-kbetr / 1000.
    ENDIF.

    CLEAR: wa_value3, wa_pre_value.
    wa_base = '1'.

* PLAN vendor can be different with ACTUAL vendor
    LOOP AT it_info WHERE matnr = it_mldoc_mkpf-matnr.
*                    AND lifnr =  it_mldoc_mkpf-lifnr.  "ANDY
*                    and datab <= wa_valid_d.
*                    AND datab <= it_mldoc_mkpf-posdate.
      AT NEW datab.
        CLEAR wa_value3.
      ENDAT.
      CASE it_info-kschl.
        WHEN c_kschl.
          IF it_info-kzust IS INITIAL.
            it_mldoc-rsncode = 'ZZZ'.
          ELSE.
            it_mldoc-rsncode = it_info-kzust.
            SELECT SINGLE vtext INTO it_mldoc-vtext
                    FROM t686d WHERE spras = sy-langu
                                 AND kzust = it_info-kzust.
          ENDIF.
          wa_duty = wa_duty_rate * it_mldoc_mkpf-menge * it_info-wrbtr
                                                       / it_info-peinh.

          wa_value3  = wa_value3 + it_mldoc_mkpf-menge * it_info-wrbtr
                                                       / it_info-peinh.
        WHEN c_tire.
        WHEN OTHERS.
          wa_value3  = wa_value3 +  it_mldoc_mkpf-menge
                                     * it_info-wrbtr.       " / 10.
      ENDCASE.
      AT END OF datab.
        wa_value3  = wa_value3 + wa_duty.
        IF wa_base IS INITIAL.
          wa_diff_value = wa_value3 - wa_pre_value.
          it_mldoc-prdif = wa_diff_value.
*           it_mldoc-prdif = it_mldoc-salk3 - wa_diff_value.
          wa_mldoc_prdif = wa_mldoc_prdif + it_mldoc-prdif.
          APPEND it_mldoc.
        ELSE.
          CLEAR wa_base.
        ENDIF.
        wa_pre_value = wa_value3.
        CLEAR wa_value3.
      ENDAT.
    ENDLOOP.

    wa_mldoc_prdif = wa_mldoc_prdif - it_mldoc_mkpf-prdif.
    IF wa_mldoc_prdif NE 0.
      it_mldoc-rsncode = 'ZGR'.
      it_mldoc-prdif = wa_mldoc_prdif.
      CLEAR it_mldoc-menge.
      APPEND it_mldoc.
    ENDIF.

    CLEAR: it_mldoc, wa_mldoc, wa_mldoc_prdif.
  ENDLOOP.
*  if it_mldoc[] is initial.
*    MESSAGE s000(zz) WITH text-m13.
*  endif.

ENDFORM.                    " calc_gr_price_hist
