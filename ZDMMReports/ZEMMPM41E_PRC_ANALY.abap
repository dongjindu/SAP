************************************************************************
* Program Name      : ZEMMPM41E_PRC_ANALY
* Author            : Byung sung Bae
* Creation Date     : 2003.12.01.
* Specifications By : Min-su Park
* Pattern           : Report 1-1
* Development Request No : UD1K901872
* Addl Documentation:
* Description       : Master Inspection Characteristic Uploading
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT  zemmpm41e_prc_anal NO STANDARD PAGE HEADING LINE-SIZE 82.
*----- Type
TYPE-POOLS : slis, sp01r.

TABLES: mara.
*----- Internal tables
DATA: BEGIN OF it_gr OCCURS 0,
        werks   LIKE   t001w-werks,              "Plant
        matnr   LIKE   mara-matnr,               "Material
        maktx   LIKE   makt-maktx,               "Description
        lifnr   LIKE   lfa1-lifnr,               "Vendor
        name1   LIKE   lfa1-name1,               "Vendor name
        kschl   LIKE   ekbz-kschl,               "Condition
        waers   LIKE   mseg-waers,               "Currency
        dmbtr   LIKE   ekbe-dmbtr,               "Local amount
        wrbtr   LIKE   ekbe-wrbtr,               "Foreign amount
        budat   LIKE   mkpf-budat,               "Posting date
        dmnet   TYPE   f,
        wrnet   TYPE   f,
        menge   LIKE   mseg-menge,               "Quantity
        meins   LIKE   mseg-meins,               "Unit of measure
        peinh   LIKE   ekpo-peinh,
      END   OF it_gr.

DATA: BEGIN OF it_error OCCURS 0,
        matnr    LIKE   mara-matnr,
        lifnr    LIKE   lfa1-lifnr,
        msg(50),
      END   OF it_error.

DATA: BEGIN OF it_std_price OCCURS 0,
        period  LIKE   ztmm_analy-period,
        werks   LIKE   ztmm_analy-werks,
        matnr   LIKE   ztmm_analy-matnr,
        lifnr   LIKE   ztmm_analy-lifnr,
        base_d  LIKE   ztmm_analy-base_d,
        valid_d LIKE   ztmm_analy-valid_d,
        ekorg   LIKE   ztmm_analy-ekorg,
        kzust   LIKE   ztmm_analy-kzust,
        wrbtr   LIKE   ztmm_analy-wrbtr,
        wduty   LIKE   ztmm_analy-wduty,
        wfrgt   LIKE   ztmm_analy-wfrgt,
        wcost   LIKE   ztmm_analy-wcost,
        dmbtr   LIKE   ztmm_analy-dmbtr,
        dduty   LIKE   ztmm_analy-dduty,
        dfrgt   LIKE   ztmm_analy-dfrgt,
        dcost   LIKE   ztmm_analy-dcost,
        peinh   LIKE   ztmm_analy-peinh,
        waers   LIKE   ztmm_analy-waers,
        wkurs   LIKE   ztmm_analy-wkurs,
        source  LIKE   ztmm_analy-source,
        kbetr   LIKE   konp-kbetr,
      END   OF it_std_price.

DATA: BEGIN OF it_info_his OCCURS 0,
        datab   LIKE   konh-datab,
        datbi   LIKE   konh-datbi,
        lifnr   LIKE   lfa1-lifnr,
        kzust   LIKE   konh-kzust,
        kschl   LIKE   konh-kschl,
        waers   LIKE   ekko-waers,
        peinh   LIKE   ekpo-peinh,
        wrbtr   LIKE   ekpo-netpr,
        dmbtr   LIKE   ekpo-netpr,
        wrdif   LIKE   ekpo-netpr,
        dmdif   LIKE   ekpo-netpr,
      END   OF it_info_his.

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,
        lifnr   LIKE   lfa1-lifnr,
        datab   LIKE   a018-datab,
        profl   LIKE   mara-profl,
        kbetr   LIKE   konp-kbetr,
        meins   LIKE   mara-meins,
      END   OF it_matnr.

DATA: BEGIN OF it_knumh OCCURS 0,
        knumh LIKE konh-knumh,
        datab LIKE konh-datab,
        datbi LIKE konh-datbi,
      END   OF it_knumh.

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

DATA: it_history LIKE ztmm_diff_his OCCURS 0 WITH HEADER LINE.

*----- Work area
DATA: BEGIN OF wa_gr OCCURS 0,
        werks   LIKE   t001w-werks,              "Plant
        matnr   LIKE   mara-matnr,               "Material
        maktx   LIKE   makt-maktx,               "Description
        lifnr   LIKE   lfa1-lifnr,               "Vendor
        name1   LIKE   lfa1-name1,               "Vendor name
        kzust   LIKE   konh-kzust,               "Reason code
        dmbtr   LIKE   ekbe-dmbtr,               "Local amount
        wrbtr   LIKE   ekbe-wrbtr,               "Foreign amount
        waers   LIKE   mseg-waers,               "Currency
        menge   LIKE   mseg-menge,               "Quantity
        meins   LIKE   mseg-meins,               "Unit of measure
        lmein   LIKE   ekpo-lmein,               "Base unit of measure
        umrez   LIKE   ekpo-umrez,               "Numerator
        umren   LIKE   ekpo-umren,               "Dinomirator
        peinh   LIKE   ekpo-peinh,               "Price Unit
        kschl   LIKE   ekbz-kschl,               "Condition
        dmbtr_c LIKE   ekbz-dmbtr,               "Local amount
        wrbtr_c LIKE   ekbz-wrbtr,               "Foreign amount
        belnr   LIKE   ekbe-belnr,
        buzei   LIKE   ekbe-buzei,
        budat   LIKE   sy-datum,
        profl   LIKE   mara-profl,
        dmnet   TYPE   f,
        wrnet   TYPE   f,
      END   OF wa_gr.

DATA: BEGIN OF wa_diff,
        swnet   TYPE   f,                        "Std. Foreign Net Price
        sdnet   TYPE   f,                        "Std. local Net Price
        gwnet   LIKE   ekpo-netpr,               "GR.  Foreign Net Price
        gdnet   LIKE   ekpo-netpr,               "gr.  Foreign Net Price
      END   OF wa_diff.

DATA: wa_budat_f LIKE sy-datum,                     "Start of month
      wa_budat_t LIKE sy-datum,                     "End date of month
      wa_base_d  LIKE sy-datum,                     "Base date
      wa_matnr_f LIKE mara-matnr,                   "Start of material
      wa_matnr_t LIKE mara-matnr,                   "End of material
      wa_period  LIKE s001-spmon,                   "Period
      wa_wrbtr   LIKE ekpo-netpr,                   "Foreign price
      wa_dmbtr   LIKE ekpo-netpr.                   "Local price

CONSTANTS: c_kschl LIKE konp-kschl  VALUE 'PB00',"Type of amount
           c_frght LIKE konp-kschl  VALUE 'FRA1',"Type of freight
           c_duty  LIKE konp-kschl  VALUE 'ZOA1',"Type of duty
           c_con01 LIKE konp-kschl  VALUE 'ZOTH',"Type of ETC rate
           c_con02 LIKE konp-kschl  VALUE 'ZOTI',"Type of ETC rate
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
*----- Constants
CONSTANTS : c_formname_top_of_page TYPE slis_formname
                                        VALUE 'TOP_OF_PAGE'.

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
  append w_fieldcat.
  clear : w_fieldcat.

END-OF-DEFINITION.

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_werks  LIKE t001w-werks DEFAULT 'P001' OBLIGATORY.
PARAMETERS:     p_month  LIKE s001-spmon DEFAULT sy-datum(6) OBLIGATORY.
SELECT-OPTIONS: s_matnr  FOR  mara-matnr NO-EXTENSION.
SELECTION-SCREEN END   OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS       r_year RADIOBUTTON GROUP rd1 DEFAULT 'X'
                                              USER-COMMAND rd1.
SELECTION-SCREEN COMMENT  3(20) text-t03 FOR FIELD r_year.
SELECTION-SCREEN POSITION 33.
PARAMETERS       r_quar RADIOBUTTON GROUP rd1.
SELECTION-SCREEN COMMENT 35(30) text-t04 FOR FIELD r_quar.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK bl2.

*----- Initialization
INITIALIZATION.
  PERFORM set_initialization.

*----- Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_rtn.
  PERFORM read_data.

*----- Top-of-page
TOP-OF-PAGE.
  PERFORM top_of_page.

START-OF-SELECTION.
  PERFORM create_history.
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
  PERFORM read_gr_info.
  PERFORM read_standard_price.
  PERFORM read_info_record.
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

*  SELECT SINGLE base_d INTO wa_base_d
*                FROM ztmm_analy
*               WHERE period = wa_period
*                 AND werks  = p_werks
*                 AND submt  = ' '
*                 AND newmt  = ' '.
*  IF sy-subrc NE 0.
*    MESSAGE e000(zz) WITH text-m02.
*  ENDIF.
ENDFORM.                    " CHECK_PERIOD
*&---------------------------------------------------------------------*
*&      Form  APPEND_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_itab.
*----- If data, amount is same, collect data
  ON CHANGE OF wa_gr-belnr OR wa_gr-buzei.
    wa_gr-wrnet = wa_gr-wrbtr / wa_gr-menge.
    wa_gr-dmnet = wa_gr-dmbtr / wa_gr-menge.

    READ TABLE it_gr WITH KEY werks = wa_gr-werks
                              matnr = wa_gr-matnr
                              lifnr = wa_gr-lifnr
                              budat = wa_gr-budat
                              kschl = c_kschl
                              wrnet = wa_gr-wrnet
                              dmnet = wa_gr-dmnet.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING wa_gr TO it_gr.
      MOVE: c_kschl TO it_gr-kschl.
      APPEND it_gr.
    ELSE.
      it_gr-menge = it_gr-menge + wa_gr-menge.
      it_gr-dmbtr = it_gr-dmbtr + wa_gr-dmbtr.
      it_gr-wrbtr = it_gr-wrbtr + wa_gr-wrbtr.
      MODIFY it_gr INDEX sy-tabix.
    ENDIF.
  ENDON.

  wa_gr-wrnet = wa_gr-wrbtr_c / wa_gr-menge.
  wa_gr-dmnet = wa_gr-dmbtr_c / wa_gr-menge.

  IF wa_gr-kschl NE c_frght AND
     wa_gr-kschl NE c_duty.
    wa_gr-kschl = 'ETC'.
  ENDIF.

  READ TABLE it_gr WITH KEY werks = wa_gr-werks
                            matnr = wa_gr-matnr
                            lifnr = wa_gr-lifnr
                            budat = wa_gr-budat
                            kschl = wa_gr-kschl
                            wrnet = wa_gr-wrnet
                            dmnet = wa_gr-dmnet.
  IF sy-subrc NE 0.
    MOVE-CORRESPONDING wa_gr TO it_gr.
    MOVE: wa_gr-wrbtr_c TO it_gr-wrbtr,
          wa_gr-dmbtr_c TO it_gr-dmbtr.
    APPEND it_gr.
  ELSE.
    it_gr-menge = it_gr-menge + wa_gr-menge.
    it_gr-dmbtr = it_gr-dmbtr + wa_gr-dmbtr_c.
    it_gr-wrbtr = it_gr-wrbtr + wa_gr-wrbtr_c.
    MODIFY it_gr INDEX sy-tabix.
  ENDIF.


  MOVE: wa_gr-matnr TO it_matnr-matnr,
        wa_gr-lifnr TO it_matnr-lifnr,
        wa_gr-profl TO it_matnr-profl,
        wa_gr-meins TO it_matnr-meins.
  COLLECT it_matnr.
ENDFORM.                    " APPEND_ITAB
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
  PERFORM build_event.
  PERFORM build_sort.
  PERFORM comment_build USING  w_top_of_page[].
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
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  APPEND_STD_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_std_price.
  APPEND it_std_price.

  READ TABLE it_matnr WITH KEY matnr = it_std_price-matnr
                      BINARY SEARCH.
  IF sy-subrc EQ 0.

    MOVE: it_std_price-base_d TO it_matnr-datab,
          it_std_price-kbetr  TO it_matnr-kbetr.

    MODIFY it_matnr TRANSPORTING datab kbetr
                           WHERE matnr = it_matnr-matnr.
  ENDIF.

  CLEAR: it_std_price.
ENDFORM.                    " APPEND_STD_PRICE
*&---------------------------------------------------------------------*
*&      Form  create_history
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_history.
  DATA: lw_stop_flg.

  PERFORM display_progress_bar USING text-m11.

  LOOP AT it_gr.
    CLEAR: lw_stop_flg.

    PERFORM get_material_info USING lw_stop_flg.
    IF lw_stop_flg EQ 'X'. CONTINUE. ENDIF.

    PERFORM set_itab_for_comparison USING lw_stop_flg.
    IF lw_stop_flg EQ 'X'. CONTINUE. ENDIF.

    PERFORM append_history USING lw_stop_flg.
  ENDLOOP.

  DATA: wa_zzseq_idx TYPE i.

  SORT it_history BY spmon werks lifnr matnr menge datab kzust.
  LOOP AT it_history.
    AT NEW matnr.
      wa_zzseq_idx = 0.
    ENDAT.
    wa_zzseq_idx = wa_zzseq_idx + 1.
    it_history-zzseq = wa_zzseq_idx.
    MODIFY it_history.
  ENDLOOP.
ENDFORM.                    " create_history
*&---------------------------------------------------------------------*
*&      Form  append_material_cost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_material_cost.
  DATA: lw_compare_flg.

  wa_diff-gwnet = it_gr-wrbtr / it_gr-menge *
                  it_std_price-peinh.
  wa_diff-gdnet = it_gr-dmbtr / it_gr-menge *
                  it_std_price-peinh.

  IF wa_diff-gwnet NE it_std_price-wrbtr.
    lw_compare_flg = 'X'.
  ENDIF.

  IF wa_diff-gdnet NE it_std_price-dmbtr.
    lw_compare_flg = 'X'.
  ENDIF.

  CHECK lw_compare_flg EQ 'X'.

  LOOP AT it_info WHERE matnr =  it_gr-matnr
                    AND lifnr =  it_gr-lifnr
                    AND kschl =  c_kschl
                    AND datab <= it_gr-budat.
    IF it_matnr-profl EQ 'V'.
      MOVE: it_info-kzust TO it_info-kschl.
    ENDIF.

    wa_diff-gwnet = it_gr-wrbtr / it_gr-menge *
                    it_info-peinh.
    wa_diff-gdnet = it_gr-dmbtr / it_gr-menge *
                    it_info-peinh.

    CLEAR: it_history.
    READ TABLE it_history WITH KEY spmon = p_month
                                   werks = p_werks
                                   lifnr = it_gr-lifnr
                                   matnr = it_gr-matnr
*                                  kzust = it_info-kschl
                                   datab = it_info-datab
                                   adamt = wa_diff-gdnet
                                   awamt = wa_diff-gwnet.
    IF sy-subrc EQ 0.
      it_history-menge =   it_history-menge + it_gr-menge.
      it_history-dwamt = it_history-dwnet * it_history-menge /
                         it_history-peinh.
      it_history-ddamt = it_history-ddnet * it_history-menge /
                         it_history-peinh.
      it_history-drate = it_history-dwnet /
                         ( it_history-adamt - it_history-dwnet ) * 100.

      MODIFY it_history INDEX sy-tabix.
    ELSE.
      MOVE: p_month             TO it_history-spmon,
            it_gr-werks         TO it_history-werks,
            it_gr-lifnr         TO it_history-lifnr,
            it_gr-matnr         TO it_history-matnr,
            it_info-datab       TO it_history-datab,
            it_info-kschl       TO it_history-kzust,
            it_gr-waers         TO it_history-waers,
            it_info-peinh       TO it_history-peinh,
            it_gr-menge         TO it_history-menge,
            it_gr-meins         TO it_history-meins,
            it_std_price-source TO it_history-source,
            sy-uname            TO it_history-ernam,
            sy-datum            TO it_history-erdat,
            sy-uzeit            TO it_history-erzet,
            sy-uname            TO it_history-aenam,
            sy-datum            TO it_history-aedat,
            sy-uzeit            TO it_history-aezet.

      it_history-awamt = it_gr-wrbtr / it_gr-menge *
                         it_info-peinh.
      it_history-adamt = it_gr-dmbtr / it_gr-menge *
                         it_info-peinh.
      it_history-dwnet = it_history-awamt - it_info-wrbtr.
      it_history-ddnet = it_history-adamt - it_info-dmbtr.
      it_history-dwamt = it_history-dwnet * it_history-menge /
                         it_history-peinh.
      it_history-ddamt = it_history-ddnet * it_history-menge /
                         it_history-peinh.
      it_history-drate = it_history-dwnet /
                         ( it_history-adamt - it_history-dwnet ) * 100.

      APPEND it_history.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " append_material_cost
*&---------------------------------------------------------------------*
*&      Form  read_gr_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_gr_info.
*----- Read GR data
  PERFORM display_progress_bar USING text-m08.

  EXEC SQL PERFORMING APPEND_ITAB.
    SELECT A.WERKS, A.MATNR, F.LIFNR, A.DMBTR,
           A.WRBTR, A.WAERS, A.MENGE, G.MEINS,
           G.UMREZ, G.UMREN, G.LMEIN, D.MAKTX,
           E.NAME1, B.KSCHL, B.DMBTR, B.WRBTR,
           A.BELNR, A.BUZEI, A.BUDAT, C.PROFL,
           G.PEINH
      INTO :WA_GR-WERKS,:WA_GR-MATNR,:WA_GR-LIFNR,  :WA_GR-DMBTR,
           :WA_GR-WRBTR,:WA_GR-WAERS,:WA_GR-MENGE,  :WA_GR-MEINS,
           :WA_GR-UMREZ,:WA_GR-UMREN,:WA_GR-LMEIN,  :WA_GR-MAKTX,
           :WA_GR-NAME1,:WA_GR-KSCHL,:WA_GR-DMBTR_C,:WA_GR-WRBTR_C,
           :WA_GR-BELNR,:WA_GR-BUZEI,:WA_GR-BUDAT,  :WA_GR-PROFL,
           :WA_GR-PEINH
      FROM EKBE A, MARA C, MAKT D, EKKO F, EKPO G, LFA1 E, EKBZ B
*      ,
*           BKPF H, BSIS I
     WHERE A.MANDT = :SY-MANDT
       AND A.BUDAT BETWEEN :WA_BUDAT_F AND :WA_BUDAT_T
       AND A.BWART = '101'
       AND A.BEWTP = 'E'
       AND A.WERKS = :P_WERKS
       AND A.MATNR BETWEEN :WA_MATNR_F AND :WA_MATNR_T
       AND NOT EXISTS (SELECT *
                         FROM MSEG
                        WHERE MANDT = A.MANDT
                          AND BWART IN ('102','122','932')
                          AND SJAHR = A.GJAHR
                          AND SMBLN = A.BELNR
                          AND SMBLP = A.BUZEI)
       AND C.MANDT   = A.MANDT
       AND C.MATNR   = A.MATNR
       AND C.MTART   IN ('ROH', 'ROH1')
       AND C.PROFL   IN ('K','V')
*       AND C.LVORM = ' '
       AND D.MANDT    = C.MANDT
       AND D.MATNR    = C.MATNR
       AND D.SPRAS    = :SY-LANGU
       AND F.MANDT    = A.MANDT
       AND F.EBELN    = A.EBELN
       AND G.MANDT    = A.MANDT
       AND G.EBELN    = A.EBELN
       AND G.EBELP    = A.EBELP
       AND E.MANDT    = F.MANDT
       AND E.LIFNR    = F.LIFNR
       AND B.MANDT(+) = A.MANDT
       AND B.EBELN(+) = A.EBELN
       AND B.EBELP(+) = A.EBELP
       AND B.GJAHR(+) = A.GJAHR
       AND B.BELNR(+) = A.BELNR
       AND B.BUZEI(+) = A.BUZEI
  ENDEXEC.

  READ TABLE it_gr INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.
ENDFORM.                    " read_gr_info
*&---------------------------------------------------------------------*
*&      Form  read_standard_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_standard_price.
  CASE c_check.
    WHEN r_year.
      PERFORM read_annual_plan.
    WHEN r_quar.
      PERFORM read_standard_plan.
  ENDCASE.
ENDFORM.                    " read_standard_price
*&---------------------------------------------------------------------*
*&      Form  read_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_info_record.
*----- Read Info record
  PERFORM display_progress_bar USING text-m09.

  CLEAR: it_knumh, it_knumh[].
  SELECT knumh datab datbi
    INTO TABLE it_knumh
    FROM a018
     FOR ALL ENTRIES IN it_matnr
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  it_matnr-matnr
     AND lifnr =  it_matnr-lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datbi >= wa_base_d
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
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_zvmm_info_condi
    FROM zvmm_info_condi
     FOR ALL ENTRIES IN it_knumh
   WHERE knumh = it_knumh-knumh
     AND kschl IN (c_kschl,c_frght,c_con01,c_con02)
     AND loevm_ko = ' '.

  SORT it_zvmm_info_condi BY knumh kopos.

  LOOP AT it_zvmm_info_condi.
    CLEAR: it_info.

    DATA: lw_umrez LIKE eina-umrez,        "Numerator
          lw_umren LIKE eina-umren.        "Denomirator

   READ TABLE it_matnr WITH KEY matnr = it_zvmm_info_condi-vakey+10(18)
                                   lifnr = it_zvmm_info_condi-vakey(10)
                                                          BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    READ TABLE it_knumh WITH KEY knumh = it_zvmm_info_condi-knumh.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    READ TABLE it_info WITH KEY matnr = it_zvmm_info_condi-vakey+10(18)
                                lifnr = it_zvmm_info_condi-vakey(10)
                                datab = it_knumh-datab.
    IF sy-subrc NE 0.           "Append Material cost, duty

*----- Append Material Cost
      MOVE: it_matnr-matnr                TO it_info-matnr,
            it_matnr-lifnr                TO it_info-lifnr,
            it_knumh-datab                TO it_info-datab,
            it_knumh-datbi                TO it_info-datbi,
            it_zvmm_info_condi-kschl      TO it_info-kschl,
            it_zvmm_info_condi-kzust      TO it_info-kzust,
            it_zvmm_info_condi-konwa      TO it_info-waers.

      IF it_zvmm_info_condi-kmein EQ it_matnr-meins.

        it_info-wrbtr = it_zvmm_info_condi-kbetr.

        it_info-peinh = it_zvmm_info_condi-kpein.
      ELSE.
        SELECT SINGLE umrez umren
          INTO (lw_umrez, lw_umren)
          FROM eina
         WHERE matnr = it_matnr-matnr
           AND lifnr = it_matnr-lifnr.
        IF sy-subrc NE 0.
          MESSAGE e000(zz) WITH text-m01.
        ENDIF.

        it_info-peinh = it_zvmm_info_condi-kpein *
                        it_zvmm_info_condi-kumza /
                        it_zvmm_info_condi-kumne *
                        lw_umrez      / lw_umren.
      ENDIF.

      PERFORM get_local_amount USING it_info-waers it_matnr-datab
                                     it_info-wrbtr it_info-dmbtr.

      APPEND it_info.

      MOVE: it_info-wrbtr TO wa_wrbtr,
            it_info-dmbtr TO wa_dmbtr.

*----- Append Duty
      IF it_matnr-profl EQ 'K'.
        MOVE: c_duty TO it_info-kschl.
        it_info-wrbtr = wa_wrbtr * it_matnr-kbetr / 1000.
        it_info-dmbtr = wa_dmbtr * it_matnr-kbetr / 1000.

        APPEND it_info.
      ENDIF.
    ELSE.
*----- Append Freight, Other costs
      CASE it_zvmm_info_condi-kschl.
        WHEN c_frght.
          IF it_matnr-profl EQ 'K'.
            MOVE: it_matnr-matnr                TO it_info-matnr,
                  it_matnr-lifnr                TO it_info-lifnr,
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
        WHEN OTHERS.
          CHECK it_matnr-profl EQ 'K'.
          READ TABLE it_info WITH KEY matnr = it_matnr-matnr
                                      lifnr = it_matnr-lifnr
                                      datab = it_zvmm_info_condi-datab
                                      kschl = 'ETC'.
          IF sy-subrc EQ 0.
            it_info-wrbtr = it_info-wrbtr + wa_wrbtr *
                            it_zvmm_info_condi-kbetr / 1000.
            it_info-dmbtr = it_info-dmbtr + wa_dmbtr *
                            it_zvmm_info_condi-kbetr / 1000.
            MODIFY it_info INDEX sy-tabix.
          ELSE.
            MOVE: it_matnr-matnr                TO it_info-matnr,
                  it_matnr-lifnr                TO it_info-lifnr,
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

*----- Set Net price
  DATA: wa_info LIKE it_info.
  LOOP AT it_info.
    MOVE: it_info TO wa_info.
    AT NEW datab.
      MOVE: wa_info-peinh TO it_info-peinh.
      MODIFY it_info TRANSPORTING peinh
                            WHERE matnr = it_info-matnr
                              AND lifnr = it_info-lifnr
                              AND datab = it_info-datab.
    ENDAT.
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
*&      Form  append_duty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_duty.
  DATA: lw_compare_flg.

  wa_diff-gwnet = it_gr-wrbtr / it_gr-menge *
                  it_std_price-peinh.
  wa_diff-gdnet = it_gr-dmbtr / it_gr-menge *
                  it_std_price-peinh.

  IF wa_diff-gwnet NE it_std_price-wduty.
    lw_compare_flg = 'X'.
  ENDIF.

  IF wa_diff-gdnet NE it_std_price-dduty.
    lw_compare_flg = 'X'.
  ENDIF.

  CHECK lw_compare_flg EQ 'X'.

  LOOP AT it_info WHERE matnr =  it_gr-matnr
                    AND lifnr =  it_gr-lifnr
                    AND kschl =  it_gr-kschl
                    AND datab <= it_gr-budat.
    IF it_matnr-profl EQ 'V'.
      MOVE: it_info-kzust TO it_info-kschl.
    ENDIF.

    wa_diff-gwnet = it_gr-wrbtr / it_gr-menge *
                    it_info-peinh.
    wa_diff-gdnet = it_gr-dmbtr / it_gr-menge *
                    it_info-peinh.

    CLEAR: it_history.
    READ TABLE it_history WITH KEY spmon = p_month
                                   werks = p_werks
                                   lifnr = it_gr-lifnr
                                   matnr = it_gr-matnr
                                   kzust = it_info-kschl
                                   datab = it_info-datab
                                   adamt = wa_diff-gdnet
                                   awamt = wa_diff-gwnet.
    IF sy-subrc EQ 0.
      it_history-menge =   it_history-menge + it_gr-menge.
      it_history-awamt =   it_history-awamt + it_gr-wrbtr   /
                           it_gr-menge      * it_info-peinh.
      it_history-adamt =   it_history-adamt + it_gr-dmbtr   /
                           it_gr-menge      * it_info-peinh.
      it_history-dwnet =   it_history-dwnet +
                         ( it_gr-wrbtr      / it_gr-menge   *
                           it_info-peinh )  - it_info-wrbtr.
      it_history-ddnet =   it_history-ddnet +
                         ( it_gr-dmbtr      / it_gr-menge   *
                           it_info-peinh )  - it_info-dmbtr.
      it_history-dwamt = it_history-dwnet * it_history-menge /
                         it_history-peinh.
      it_history-ddamt = it_history-ddnet * it_history-menge /
                         it_history-peinh.

      it_history-drate = it_history-dwnet /
                         ( it_history-adamt - it_history-dwnet ) * 100.

      MODIFY it_history INDEX sy-tabix.
    ELSE.
      MOVE: p_month             TO it_history-spmon,
            it_gr-werks         TO it_history-werks,
            it_gr-lifnr         TO it_history-lifnr,
            it_gr-matnr         TO it_history-matnr,
            it_info-datab       TO it_history-datab,
            it_info-kschl       TO it_history-kzust,
            it_gr-waers         TO it_history-waers,
            it_info-peinh       TO it_history-peinh,
            it_gr-menge         TO it_history-menge,
            it_gr-meins         TO it_history-meins,
            it_std_price-source TO it_history-source,
            sy-uname            TO it_history-ernam,
            sy-datum            TO it_history-erdat,
            sy-uzeit            TO it_history-erzet,
            sy-uname            TO it_history-aenam,
            sy-datum            TO it_history-aedat,
            sy-uzeit            TO it_history-aezet.

      it_history-awamt = it_gr-wrbtr / it_gr-menge *
                         it_info-peinh.
      it_history-adamt = it_gr-dmbtr / it_gr-menge *
                         it_info-peinh.
      it_history-dwnet = it_history-awamt - it_info-wrbtr.
      it_history-ddnet = it_history-adamt - it_info-dmbtr.
      it_history-dwamt = it_history-dwnet * it_history-menge /
                         it_history-peinh.
      it_history-ddamt = it_history-ddnet * it_history-menge /
                         it_history-peinh.

      DATA: lw_gap LIKE it_history-adamt.
      lw_gap = it_history-adamt - it_history-dwnet.
      IF lw_gap EQ 0.
        it_history-drate = 0.
      ELSE.
        it_history-drate = it_history-dwnet /
                          ( it_history-adamt - it_history-dwnet ) * 100.
      ENDIF.

      APPEND it_history.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " append_duty
*&---------------------------------------------------------------------*
*&      Form  append_freight
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_freight.
  DATA: lw_compare_flg.

  wa_diff-gwnet = it_gr-wrbtr / it_gr-menge *
                  it_std_price-peinh.
  wa_diff-gdnet = it_gr-dmbtr / it_gr-menge *
                  it_std_price-peinh.

  IF wa_diff-gwnet NE it_std_price-wfrgt.
    lw_compare_flg = 'X'.
  ENDIF.

  IF wa_diff-gdnet NE it_std_price-dfrgt.
    lw_compare_flg = 'X'.
  ENDIF.

  CHECK lw_compare_flg EQ 'X'.

  LOOP AT it_info WHERE matnr =  it_gr-matnr
                    AND lifnr =  it_gr-lifnr
                    AND kschl =  it_gr-kschl
                    AND datab <= it_gr-budat.
    IF it_matnr-profl EQ 'V'.
      MOVE: it_info-kzust TO it_info-kschl.
    ENDIF.

    wa_diff-gwnet = it_gr-wrbtr / it_gr-menge *
                    it_info-peinh.
    wa_diff-gdnet = it_gr-dmbtr / it_gr-menge *
                    it_info-peinh.

    CLEAR: it_history.
    READ TABLE it_history WITH KEY spmon = p_month
                                   werks = p_werks
                                   lifnr = it_gr-lifnr
                                   matnr = it_gr-matnr
                                   kzust = it_info-kschl
                                   datab = it_info-datab
                                   adamt = wa_diff-gdnet
                                   awamt = wa_diff-gwnet.
    IF sy-subrc EQ 0.
      it_history-menge =   it_history-menge + it_gr-menge.
*      it_history-awamt =   it_history-awamt + it_gr-wrbtr   /
*                           it_gr-menge      * it_info-peinh.
*      it_history-adamt =   it_history-adamt + it_gr-dmbtr   /
*                           it_gr-menge      * it_info-peinh.
*      it_history-dwnet =   it_history-dwnet +
*                         ( it_gr-wrbtr      / it_gr-menge   *
*                           it_info-peinh )  - it_info-wrbtr.
*      it_history-ddnet =   it_history-ddnet +
*                         ( it_gr-dmbtr      / it_gr-menge   *
*                           it_info-peinh )  - it_info-dmbtr.
      it_history-dwamt = it_history-dwnet * it_history-menge /
                         it_history-peinh.
      it_history-ddamt = it_history-ddnet * it_history-menge /
                         it_history-peinh.
      it_history-drate = it_history-dwnet /
                         ( it_history-adamt - it_history-dwnet ) * 100.

      MODIFY it_history INDEX sy-tabix.
    ELSE.
      MOVE: p_month             TO it_history-spmon,
            it_gr-werks         TO it_history-werks,
            it_gr-lifnr         TO it_history-lifnr,
            it_gr-matnr         TO it_history-matnr,
            it_info-datab       TO it_history-datab,
            it_info-kschl       TO it_history-kzust,
            it_gr-waers         TO it_history-waers,
            it_info-peinh       TO it_history-peinh,
            it_gr-menge         TO it_history-menge,
            it_gr-meins         TO it_history-meins,
            it_std_price-source TO it_history-source,
            sy-uname            TO it_history-ernam,
            sy-datum            TO it_history-erdat,
            sy-uzeit            TO it_history-erzet,
            sy-uname            TO it_history-aenam,
            sy-datum            TO it_history-aedat,
            sy-uzeit            TO it_history-aezet.

      it_history-awamt = it_gr-wrbtr / it_gr-menge *
                         it_info-peinh.
      it_history-adamt = it_gr-dmbtr / it_gr-menge *
                         it_info-peinh.
      it_history-dwnet = it_history-awamt - it_info-wrbtr.
      it_history-ddnet = it_history-adamt - it_info-dmbtr.
      it_history-dwamt = it_history-dwnet * it_history-menge /
                         it_history-peinh.
      it_history-ddamt = it_history-ddnet * it_history-menge /
                         it_history-peinh.
      it_history-drate = it_history-dwnet /
                         ( it_history-adamt - it_history-dwnet ) * 100.

      APPEND it_history.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " append_freight
*&---------------------------------------------------------------------*
*&      Form  append_other_costs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_other_costs.
  DATA: lw_compare_flg.

  wa_diff-gwnet = it_gr-wrbtr / it_gr-menge *
                  it_std_price-peinh.
  wa_diff-gdnet = it_gr-dmbtr / it_gr-menge *
                  it_std_price-peinh.

  IF wa_diff-gwnet NE it_std_price-wcost.
    lw_compare_flg = 'X'.
  ENDIF.

  IF wa_diff-gdnet NE it_std_price-dcost.
    lw_compare_flg = 'X'.
  ENDIF.

  CHECK lw_compare_flg EQ 'X'.

  LOOP AT it_info WHERE matnr =  it_gr-matnr
                    AND lifnr =  it_gr-lifnr
                    AND kschl =  'ETC'
                    AND datab <= it_gr-budat.
    IF it_matnr-profl EQ 'V'.
      MOVE: it_info-kzust TO it_info-kschl.
    ENDIF.

    wa_diff-gwnet = it_gr-wrbtr / it_gr-menge *
                    it_info-peinh.
    wa_diff-gdnet = it_gr-dmbtr / it_gr-menge *
                    it_info-peinh.

    CLEAR: it_history.
    READ TABLE it_history WITH KEY spmon = p_month
                                   werks = p_werks
                                   lifnr = it_gr-lifnr
                                   matnr = it_gr-matnr
                                   kzust = it_info-kschl
                                   datab = it_info-datab
                                   adamt = wa_diff-gdnet
                                   awamt = wa_diff-gwnet.
    IF sy-subrc EQ 0.
      it_history-menge =   it_history-menge + it_gr-menge.
*      it_history-awamt =   it_history-awamt + it_gr-wrbtr   /
*                           it_gr-menge      * it_info-peinh.
*      it_history-adamt =   it_history-adamt + it_gr-dmbtr   /
*                           it_gr-menge      * it_info-peinh.
*      it_history-dwnet =   it_history-dwnet +
*                         ( it_gr-wrbtr      / it_gr-menge   *
*                           it_info-peinh )  - it_info-wrbtr.
*      it_history-ddnet =   it_history-ddnet +
*                         ( it_gr-dmbtr      / it_gr-menge   *
*                           it_info-peinh )  - it_info-dmbtr.
      it_history-dwamt = it_history-dwnet * it_history-menge /
                         it_history-peinh.
      it_history-ddamt = it_history-ddnet * it_history-menge /
                         it_history-peinh.
      it_history-drate = it_history-dwnet /
                         ( it_history-adamt - it_history-dwnet ) * 100.

      MODIFY it_history INDEX sy-tabix.
    ELSE.
      MOVE: p_month             TO it_history-spmon,
            it_gr-werks         TO it_history-werks,
            it_gr-lifnr         TO it_history-lifnr,
            it_gr-matnr         TO it_history-matnr,
            it_info-datab       TO it_history-datab,
            it_info-kschl       TO it_history-kzust,
            it_gr-waers         TO it_history-waers,
            it_info-peinh       TO it_history-peinh,
            it_gr-menge         TO it_history-menge,
            it_gr-meins         TO it_history-meins,
            it_std_price-source TO it_history-source,
            sy-uname            TO it_history-ernam,
            sy-datum            TO it_history-erdat,
            sy-uzeit            TO it_history-erzet,
            sy-uname            TO it_history-aenam,
            sy-datum            TO it_history-aedat,
            sy-uzeit            TO it_history-aezet.

      it_history-awamt = it_gr-wrbtr / it_gr-menge *
                         it_info-peinh.
      it_history-adamt = it_gr-dmbtr / it_gr-menge *
                         it_info-peinh.
      it_history-dwnet = it_history-awamt - it_info-wrbtr.
      it_history-ddnet = it_history-adamt - it_info-dmbtr.
      it_history-dwamt = it_history-dwnet * it_history-menge /
                         it_history-peinh.
      it_history-ddamt = it_history-ddnet * it_history-menge /
                         it_history-peinh.
      it_history-drate = it_history-dwnet /
                         ( it_history-adamt - it_history-dwnet ) * 100.

      APPEND it_history.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " append_other_costs
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
    w_col_pos 'LIFNR'  10 text-002 'CHAR'  'X' ''      '' '',
    w_col_pos 'ZZSEQ'  05 text-003 'NUMC'  ''  ''      '' '',
    w_col_pos 'DATAB'  10 text-004 'DATS'  ''  ''      '' '',
    w_col_pos 'KZUST'  04 text-005 'CHAR'  ''  ''      '' '',
    w_col_pos 'WAERS'  05 text-006 'CUKY'  ''  ''      '' '',
    w_col_pos 'PEINH'  05 text-007 'DEC'   ''  ''      '' '',
    w_col_pos 'MENGE'  17 text-008 'QUAN'  ''  '' 'MEINS' '',
    w_col_pos 'MEINS'  03 text-009 'UNIT'  ''  ''      '' '',
    w_col_pos 'AWAMT'  16 text-011 'CURR'  ''  'WAERS' '' '',
    w_col_pos 'ADAMT'  16 text-012 'CURR'  ''  'WAERS' '' '',
    w_col_pos 'DWNET'  16 text-013 'CURR'  ''  'WAERS' '' '',
    w_col_pos 'DDNET'  16 text-014 'CURR'  ''  'WAERS' '' '',
    w_col_pos 'DWAMT'  16 text-015 'CURR'  ''  'WAERS' '' '',
    w_col_pos 'DDAMT'  16 text-016 'CURR'  ''  'WAERS' '' '',
    w_col_pos 'DRATE'  16 text-017 'DEC'   ''  ''      '' '',
    w_col_pos 'SOURCE' 01 text-018 'CHAR'  ''  ''      '' ''.
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
  w_sortcat-tabname        = 'IT_HISTORY'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 2.
  w_sortcat-fieldname      = 'LIFNR'.
  w_sortcat-tabname        = 'IT_HISTORY'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 3.
  w_sortcat-fieldname      = 'ZZSEQ'.
  w_sortcat-tabname        = 'IT_HISTORY'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.
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
  MOVE: p_werks TO ls_line-info.
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
            it_events                = w_eventcat[]
            is_print                 = l_print_p
       TABLES
            t_outtab                 = it_history
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
      PERFORM save_rtn.
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
FORM save_rtn.
  DELETE FROM ztmm_diff_his WHERE spmon = p_month
                              AND werks = p_werks
                              AND matnr IN s_matnr.

  INSERT ztmm_diff_his FROM TABLE it_history
         ACCEPTING DUPLICATE KEYS.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
    MESSAGE s000(zz) WITH text-m05.
  ELSE.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.
ENDFORM.                    " save_rtn
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
*&      Form  get_material_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_info USING pw_stop_flg.
  CLEAR: it_matnr.
  READ TABLE it_matnr WITH KEY matnr = it_gr-matnr
                               lifnr = it_gr-lifnr
                               BINARY SEARCH.
  IF sy-subrc NE 0.
    MOVE: it_gr-matnr TO it_error-matnr,
          it_gr-lifnr TO it_error-lifnr,
          text-m01    TO it_error-msg.
    APPEND it_error.
    MOVE: 'X' TO pw_stop_flg.
  ENDIF.
ENDFORM.                    " get_material_info
*&---------------------------------------------------------------------*
*&      Form  set_itab_for_comparison
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_STOP_FLG  text
*----------------------------------------------------------------------*
FORM set_itab_for_comparison USING pw_stop_flg.
*----- Set comparison internal table
  CLEAR: it_std_price.

  READ TABLE it_std_price WITH KEY werks = it_gr-werks
                                   matnr = it_gr-matnr.
  IF sy-subrc NE 0.
    MOVE: it_gr-matnr TO it_error-matnr,
          it_gr-lifnr TO it_error-lifnr,
          text-b02   TO it_error-msg.
    APPEND it_error.
    MOVE: 'X' TO pw_stop_flg.
    EXIT.
  ENDIF.

  CLEAR: it_info_his, it_info_his[].
  LOOP AT it_info WHERE matnr =  it_gr-matnr
                    AND lifnr =  it_gr-lifnr
                    AND kschl =  it_gr-kschl
                    AND datab <= it_gr-budat.

    " IF Standard price is future determined price,
    " skip undetermined price.
    IF NOT it_std_price-valid_d IS INITIAL AND
       it_info-datbi < it_std_price-valid_d.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING it_info TO it_info_his.
    APPEND it_info_his.
  ENDLOOP.

  PERFORM append_std_price_to_info_his USING pw_stop_flg.

  IF pw_stop_flg = 'X'. EXIT. ENDIF.

  PERFORM calculate_diffrence USING pw_stop_flg.
ENDFORM.                    " set_itab_for_comparison
*&---------------------------------------------------------------------*
*&      Form  append_std_price_to_info_his
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_STOP_FLG  text
*----------------------------------------------------------------------*
FORM append_std_price_to_info_his USING pw_stop_flg.
*----- Append Standard Price
*  SORT it_info_his BY datab DESCENDING datbi DESCENDING.
  SORT it_info_his BY datab datbi.

  READ TABLE it_info_his INDEX 1.
  IF sy-subrc NE 0.
    MOVE: it_gr-matnr TO it_error-matnr,
          it_gr-lifnr TO it_error-lifnr.
    CONCATENATE text-b10 it_gr-kschl text-b11 INTO it_error-msg
      SEPARATED BY space.
    APPEND it_error.
    MOVE: 'X' TO pw_stop_flg.
    EXIT.
  ENDIF.

  MOVE: '10000101'         TO it_info_his-datab,
        'STD'              TO it_info_his-kzust,
        it_std_price-waers TO it_info_his-waers,
        it_std_price-peinh TO it_info_his-peinh.

  CASE it_gr-kschl.
    WHEN c_kschl.
      MOVE: it_std_price-wrbtr TO it_info_his-wrbtr,
            it_std_price-dmbtr TO it_info_his-dmbtr.
    WHEN c_frght.
      MOVE: it_std_price-wfrgt TO it_info_his-wrbtr,
            it_std_price-dfrgt TO it_info_his-dmbtr.
    WHEN c_duty.
      MOVE: it_std_price-wduty TO it_info_his-wrbtr,
            it_std_price-dduty TO it_info_his-dmbtr.
    WHEN OTHERS.
      MOVE: it_std_price-wcost TO it_info_his-wrbtr,
            it_std_price-dcost TO it_info_his-dmbtr.
  ENDCASE.

  it_info_his-datbi = it_info_his-datbi - 1.

  APPEND it_info_his.
  SORT it_info_his BY datab datbi.
ENDFORM.                    " append_std_price_to_info_his
*&---------------------------------------------------------------------*
*&      Form  calculate_diffrence
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_STOP_FLG  text
*----------------------------------------------------------------------*
FORM calculate_diffrence USING pw_stop_flg.
  DATA: lw_info_his LIKE it_info_his.

  LOOP AT it_info_his.
    IF sy-tabix EQ 1.
      MOVE: it_info_his TO lw_info_his.
    ENDIF.

    IF sy-tabix EQ 2.
      IF it_std_price-lifnr NE it_info_his-lifnr.
        it_info_his-kzust = c_z02.
      ELSE.
        IF it_info_his-kzust(1) = 'X'.
          it_info_his-kzust = c_z01.
        ENDIF.
      ENDIF.
    ENDIF.

    it_info_his-wrdif = it_info_his-wrbtr - lw_info_his-wrbtr.
    it_info_his-dmdif = it_info_his-dmbtr - lw_info_his-dmbtr.

    MODIFY it_info_his.

    MOVE: it_info_his TO lw_info_his.
  ENDLOOP.

  DELETE it_info_his WHERE wrdif EQ 0
                       AND dmdif EQ 0.
ENDFORM.                    " calculate_diffrence
*&---------------------------------------------------------------------*
*&      Form  append_history
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_history USING pw_stop_flg.
  DATA: lw_wrbtr LIKE it_gr-wrbtr,
        lw_dmbtr LIKE it_gr-dmbtr.

  LOOP AT it_info_his.
    MOVE: p_month             TO it_history-spmon,
          it_gr-werks         TO it_history-werks,
          it_gr-lifnr         TO it_history-lifnr,
          it_gr-matnr         TO it_history-matnr,
          r_year              TO it_history-bizpl,
          it_info_his-datab   TO it_history-datab,
          it_gr-waers         TO it_history-waers,
          it_gr-menge         TO it_history-menge,
          it_gr-meins         TO it_history-meins,
          it_std_price-source TO it_history-source,
          sy-uname            TO it_history-ernam,
          sy-datum            TO it_history-erdat,
          sy-uzeit            TO it_history-erzet,
          sy-uname            TO it_history-aenam,
          sy-datum            TO it_history-aedat,
          sy-uzeit            TO it_history-aezet.

    IF it_info_his-kzust IS INITIAL.
      MOVE: it_info_his-kschl   TO it_history-kzust.
    ELSE.
      MOVE: it_info_his-kzust   TO it_history-kzust.
    ENDIF.

    IF     it_gr-peinh EQ 1  AND it_info-peinh = 1.
      it_history-peinh = 1.
      lw_wrbtr = it_gr-wrbtr.
      lw_dmbtr = it_gr-dmbtr.
    ELSEIF it_gr-peinh EQ 1  AND it_info-peinh = 10.
      it_history-peinh = 10.
      lw_wrbtr = it_gr-wrbtr * 10.
      lw_dmbtr = it_gr-dmbtr * 10.
    ELSEIF it_gr-peinh EQ 10 AND it_info-peinh = 1.
      it_history-peinh = 10.
      lw_wrbtr = it_gr-wrbtr.
      lw_dmbtr = it_gr-dmbtr.
      it_info_his-wrbtr = it_info_his-wrdif * 10.
      it_info_his-dmbtr = it_info_his-dmdif * 10.
    ELSEIF it_gr-peinh EQ 10 AND it_info-peinh = 10.
      it_history-peinh = 10.
      lw_wrbtr = it_gr-wrbtr.
      lw_dmbtr = it_gr-dmbtr.
    ELSE.
      MOVE: it_gr-matnr TO it_error-matnr,
            it_gr-lifnr TO it_error-lifnr,
            text-b03    TO it_error-msg.
      APPEND it_error.
      MOVE: 'X' TO pw_stop_flg.
      EXIT.
    ENDIF.

    it_history-awamt = lw_wrbtr / it_gr-menge.
    it_history-adamt = lw_dmbtr / it_gr-menge.
    it_history-dwnet = it_info_his-wrdif.
    it_history-ddnet = it_info_his-dmdif.
    it_history-dwamt = it_history-dwnet * it_history-menge /
                       it_history-peinh.
    it_history-ddamt = it_history-ddnet * it_history-menge /
                       it_history-peinh.
*    it_history-drate = it_history-it_history-dwnet /
*                       ( it_history-adamt - it_history-dwnet ) * 100.

    APPEND it_history.
  ENDLOOP.
ENDFORM.                    " append_history
*&---------------------------------------------------------------------*
*&      Form  read_annual_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_annual_plan.
*------ Read Standard price
  SORT it_matnr BY matnr lifnr.

  PERFORM display_progress_bar USING text-m10.

  EXEC SQL PERFORMING APPEND_ANNUAL_PLAN.
    SELECT C.PERIOD, B.WERKS, A.MATNR, C.BASE_D,
           C.EKORG,  C.KZUST, C.WRBTR, C.WDUTY,
           C.WFRGT,  C.WCOST, C.DMBTR, C.DDUTY,
           C.DFRGT,  C.DCOST, C.PEINH, C.WAERS,
           C.WKURS,  C.SOURCE,E.KBETR, C.LIFNR,
           C.VALID_D
      INTO :IT_STD_PRICE-PERIOD, :IT_STD_PRICE-WERKS,
           :IT_STD_PRICE-MATNR,  :IT_STD_PRICE-BASE_D,
           :IT_STD_PRICE-EKORG,  :IT_STD_PRICE-KZUST,
           :IT_STD_PRICE-WRBTR,  :IT_STD_PRICE-WDUTY,
           :IT_STD_PRICE-WFRGT,  :IT_STD_PRICE-WCOST,
           :IT_STD_PRICE-DMBTR,  :IT_STD_PRICE-DDUTY,
           :IT_STD_PRICE-DFRGT,  :IT_STD_PRICE-DCOST,
           :IT_STD_PRICE-PEINH,  :IT_STD_PRICE-WAERS,
           :IT_STD_PRICE-WKURS,  :IT_STD_PRICE-SOURCE,
           :IT_STD_PRICE-KBETR,  :IT_STD_PRICE-LIFNR,
           :IT_STD_PRICE-VALID_D
      FROM MARA A, MARC B, ZTMM_ANALY C, A902 D, KONP E
     WHERE A.MANDT     = :SY-MANDT
       AND A.MATNR     BETWEEN :WA_MATNR_F AND :WA_MATNR_T
       AND A.MTART     IN ('ROH', 'ROH1')
       AND A.PROFL     IN ('K',   'V')
       AND A.LVORM     <> 'X'
       AND B.MANDT     =  A.MANDT
       AND B.MATNR     =  A.MATNR
       AND B.WERKS     =  :P_WERKS
       AND B.LVORM     =  ' '
       AND B.DISPO     <> 'M02'
       AND C.MANDT(+)  = B.MANDT
       AND C.PERIOD(+) = :WA_PERIOD
       AND C.WERKS(+)  = B.WERKS
       AND C.MATNR(+)  = B.MATNR
       AND C.SUBMT(+)  = ' '
       AND D.MANDT(+)  = B.MANDT
       AND D.KAPPL(+)  = 'M'
       AND D.KSCHL(+)  = 'ZOA1'
       AND D.STAWN(+)  = B.STAWN
       AND E.MANDT(+)  = D.MANDT
       AND E.KNUMH(+)  = D.KNUMH
  ENDEXEC.

  READ TABLE it_std_price INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
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
*------ Read Standard price
  SORT it_matnr BY matnr lifnr.

  PERFORM display_progress_bar USING text-m07.

  EXEC SQL PERFORMING APPEND_STD_PRICE.
    SELECT B.PERIOD, B.WERKS, B.MATNR, B.BASE_D,
           B.EKORG,  B.KZUST, B.WRBTR, B.WDUTY,
           B.WFRGT,  B.WCOST, B.DMBTR, B.DDUTY,
           B.DFRGT,  B.DCOST, B.PEINH, B.WAERS,
           B.WKURS,  B.SOURCE,E.KBETR, B.LIFNR,
           B.VALID_D
      INTO :IT_STD_PRICE-PERIOD, :IT_STD_PRICE-WERKS,
           :IT_STD_PRICE-MATNR,  :IT_STD_PRICE-BASE_D,
           :IT_STD_PRICE-EKORG,  :IT_STD_PRICE-KZUST,
           :IT_STD_PRICE-WRBTR,  :IT_STD_PRICE-WDUTY,
           :IT_STD_PRICE-WFRGT,  :IT_STD_PRICE-WCOST,
           :IT_STD_PRICE-DMBTR,  :IT_STD_PRICE-DDUTY,
           :IT_STD_PRICE-DFRGT,  :IT_STD_PRICE-DCOST,
           :IT_STD_PRICE-PEINH,  :IT_STD_PRICE-WAERS,
           :IT_STD_PRICE-WKURS,  :IT_STD_PRICE-SOURCE,
           :IT_STD_PRICE-KBETR,  :IT_STD_PRICE-LIFNR,
           :IT_STD_PRICE-VALID_D
      FROM ( SELECT MANDT, WERKS, MATNR, MAX(PERIOD) PERIOD
               FROM ZTMM_ANALY
              WHERE MANDT  =  :SY-MANDT
                AND PERIOD <= :WA_PERIOD
                AND WERKS  =  :P_WERKS
                AND MATNR  BETWEEN :WA_MATNR_F AND :WA_MATNR_T
                AND SUBMT  = ' '
              GROUP BY MANDT, WERKS, MATNR) A,
           ZTMM_ANALY B, MARC C, A902 D, KONP E
     WHERE B.MANDT     = A.MANDT
       AND B.PERIOD    = A.PERIOD
       AND B.WERKS     = A.WERKS
       AND B.MATNR     = A.MATNR
       AND B.SUBMT     = ' '
       AND C.MANDT     =  B.MANDT
       AND C.MATNR     =  B.MATNR
       AND C.WERKS     =  :P_WERKS
       AND C.LVORM     =  ' '
       AND D.MANDT(+)  =  C.MANDT
       AND D.KAPPL(+)  =  'M'
       AND D.KSCHL(+)  =  'ZOA1'
       AND D.STAWN(+)  =  C.STAWN
       AND E.MANDT(+)  =  D.MANDT
       AND E.KNUMH(+)  =  D.KNUMH
  ENDEXEC.

  READ TABLE it_std_price INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
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
  IF it_std_price-period IS INITIAL.
    PERFORM no_aunnal_business_plan.

    IF it_std_price-period IS INITIAL.
      CLEAR: it_std_price.
      EXIT.
    ENDIF.
  ENDIF.

  APPEND it_std_price.

  READ TABLE it_matnr WITH KEY matnr = it_std_price-matnr
                      BINARY SEARCH.
  IF sy-subrc EQ 0.

    MOVE: it_std_price-base_d TO it_matnr-datab,
          it_std_price-kbetr  TO it_matnr-kbetr.

    MODIFY it_matnr TRANSPORTING datab kbetr
                           WHERE matnr = it_matnr-matnr.
  ENDIF.

  CLEAR: it_std_price.
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
  EXEC SQL.
    SELECT B.PERIOD, B.BASE_D,
           B.EKORG,  B.KZUST, B.WRBTR, B.WDUTY,
           B.WFRGT,  B.WCOST, B.DMBTR, B.DDUTY,
           B.DFRGT,  B.DCOST, B.PEINH, B.WAERS,
           B.WKURS,  B.SOURCE,E.KBETR, B.LIFNR,
           B.VALID_D
      INTO :IT_STD_PRICE-PERIOD, :IT_STD_PRICE-BASE_D,
           :IT_STD_PRICE-EKORG,  :IT_STD_PRICE-KZUST,
           :IT_STD_PRICE-WRBTR,  :IT_STD_PRICE-WDUTY,
           :IT_STD_PRICE-WFRGT,  :IT_STD_PRICE-WCOST,
           :IT_STD_PRICE-DMBTR,  :IT_STD_PRICE-DDUTY,
           :IT_STD_PRICE-DFRGT,  :IT_STD_PRICE-DCOST,
           :IT_STD_PRICE-PEINH,  :IT_STD_PRICE-WAERS,
           :IT_STD_PRICE-WKURS,  :IT_STD_PRICE-SOURCE,
           :IT_STD_PRICE-KBETR,  :IT_STD_PRICE-LIFNR,
           :IT_STD_PRICE-VALID_D
      FROM ( SELECT MANDT, WERKS, MATNR, MAX(PERIOD) PERIOD
               FROM ZTMM_ANALY
              WHERE MANDT  =  :SY-MANDT
                AND PERIOD <= :P_MONTH
                AND WERKS  =  :IT_STD_PRICE-WERKS
                AND MATNR  =  :IT_STD_PRICE-MATNR
                AND SUBMT  = ' '
              GROUP BY MANDT, WERKS, MATNR) A,
           ZTMM_ANALY B, MARC C, A902 D, KONP E
     WHERE B.MANDT     = A.MANDT
       AND B.PERIOD    = A.PERIOD
       AND B.WERKS     = A.WERKS
       AND B.MATNR     = A.MATNR
       AND B.SUBMT     = ' '
       AND C.MANDT     =  B.MANDT
       AND C.MATNR     =  B.MATNR
       AND C.WERKS     =  :P_WERKS
       AND C.LVORM     =  ' '
       AND D.MANDT(+)  =  C.MANDT
       AND D.KAPPL(+)  =  'M'
       AND D.KSCHL(+)  =  'ZOA1'
       AND D.STAWN(+)  =  C.STAWN
       AND E.MANDT(+)  =  D.MANDT
       AND E.KNUMH(+)  =  D.KNUMH
  ENDEXEC.
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
