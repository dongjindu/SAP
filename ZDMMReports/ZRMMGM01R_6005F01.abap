*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM10E_6006F01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*I. We gather data by ebeln ebelp eindt.(PO no, PO Item, Delivery date)
*II.We gather data by lifnr idnrk. (Vendor, Component)

*Local Variables
  DATA: lt_eket LIKE it_eket.   "Temporary Itab for Steel Reqirement
  DATA: idnrk   LIKE wa_eket-idnrk. "BOM component

* Get last days of months for Monthly Steel Requirement
  PERFORM get_last_day_of_months USING p_eindt
                                       6.                   "6 Months

**** Get MIP(Material In Plant) section
* In MIP Section, ebeln lifnr etc are related to Raw Material.
*1. Get IT_EKPO
*2. Get IT_EKETMIP draft 1.
*3. Get Temperary Table LT_EKET.
*4. Get Open Quantity by Delivery date in LT_EKET
*5. Get IT_EKETMIP draft 2.
*6. Make IT_EKETMIP by lifnr idnrk
*7. Get IT_EKETMIP final.

*1. Get IT_EKPO
  PERFORM make_it_ekpo_mip.

*2. Get IT_EKETMIP draft 1.
  PERFORM make_it_eketxxx_draft1 USING it_ekpo     "(export)
                                       it_eketmip. "(import)

*3. Get Temperary Table LT_EKET.
  lt_eket = it_eketmip. CLEAR: it_ekpo, it_eketmip.
  SORT lt_eket BY ebeln ebelp eindt.
  DELETE ADJACENT DUPLICATES FROM lt_eket
                          COMPARING ebeln ebelp eindt.

*4. Get Open Quantity by Delivery date in LT_EKET
  PERFORM openqty_by_delivery_in_lt_eket USING lt_eket
                                               'MIP'.

*5. Get IT_EKETMIP draft 2.
  it_eketmip = lt_eket.
  LOOP AT it_eketmip ASSIGNING <fs_eket>.
    <fs_eket>-zsect = 'MIP'.   "MIP Section
  ENDLOOP.
  SORT it_eketmip BY lifnr idnrk.

*
  DATA: lt_eketmip_detail LIKE it_eketmip." Tmp table for IT_eketmip
  lt_eketmip_detail = it_eketmip.


*6. Make IT_EKETMIP by lifnr idnrk
  DELETE ADJACENT DUPLICATES FROM it_eketmip
                      COMPARING lifnr idnrk.

*7. Get IT_EKETMIP final.
  PERFORM make_it_eketmip_final TABLES lt_eketmip_detail
                                USING  it_eketmip.

**** Get VDR(Vendor) Section
* In VDR Section, ebeln lifnr etc are related to End Part.
*1. Get IT_EKPO
*2. Get IT_EKETVDR draft 1.
*3. Get Temperary Table LT_EKET.
*4. Get Open Quantity by Delivery date in LT_EKET
*5. Get IT_EKETVDR draft 2.
*6. Make IT_EKETVDR by lifnr idnrk
*7. Get IT_EKETVDR final.


*1. Get IT_EKPO
  PERFORM make_it_ekpo_vdr.

*2. Get IT_EKETVDR draft 1.
  PERFORM make_it_eketxxx_draft1 USING it_ekpo     "(export)
                                       it_eketvdr. "(import)

*3. Get Temperary Table LT_EKET.
  lt_eket = it_eketvdr. CLEAR: it_ekpo, it_eketvdr.
  SORT lt_eket BY ebeln ebelp eindt.
  DELETE ADJACENT DUPLICATES FROM lt_eket
                          COMPARING ebeln ebelp eindt.

*4. Get Open Quantity by Delivery date in LT_EKET
* IT_EKETVDR IS MADE.
  PERFORM openqty_by_delivery_in_lt_eket USING lt_eket
                                               'VDR'.

*5. Get IT_EKETVDR draft 2.
  LOOP AT it_eketvdr ASSIGNING <fs_eket>.
    <fs_eket>-zsect = 'VDR'.     "VDR Section
  ENDLOOP.
  SORT it_eketvdr BY lifnr idnrk.

*
  DATA: lt_eketvdr_detail LIKE it_eketvdr." Tmp table for IT_eketvdr
  lt_eketvdr_detail = it_eketvdr.


*6. Make IT_EKETVDR by lifnr idnrk
  DELETE ADJACENT DUPLICATES FROM it_eketvdr
                      COMPARING lifnr idnrk.

*7. Get IT_EKETVDR final.
  PERFORM make_it_eketvdr_final TABLES lt_eketvdr_detail
                                USING  it_eketvdr.

****Finally make IT_eket
*  CLEAR: IT_eket.
*  APPEND LINES OF IT_eketmip TO IT_eket.
*  APPEND LINES OF IT_eketvdr TO IT_eket.
  LOOP AT it_eketmip ASSIGNING <fs_eket>.
    MOVE-CORRESPONDING <fs_eket> TO wa_zsmm_6005_01.
    APPEND wa_zsmm_6005_01 TO it_zsmm_6005_01.
  ENDLOOP.

  LOOP AT it_eketvdr ASSIGNING <fs_eket>.
    MOVE-CORRESPONDING <fs_eket> TO wa_zsmm_6005_01.
    APPEND wa_zsmm_6005_01 TO it_zsmm_6005_01.
  ENDLOOP.
  SORT it_zsmm_6005_01 BY zsect lifnr idnrk.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  cs_bom_expl_mat_v2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cs_bom_expl_mat_v2
         TABLES ext_stpox STRUCTURE  stpox
         USING p_capid LIKE  tc04-capid  "Application ID
               p_datuv LIKE  stko-datuv  "Validity date
               p_ehndl LIKE  csdata-xfeld
               "EWahrHandl. ' 'kein,'1'modPosMg,'2'%ualeEMengen
               p_emeng LIKE  stko-bmeng  "Required quantity
               p_mehrs LIKE  csdata-xfeld  "Multi-level explosion
               p_mmory LIKE  csdata-xfeld
               "Memory use ('1'=on;'0'=off;' '=no reaction)
               p_mtnrv LIKE  mara-matnr    "Material
               p_werks LIKE  marc-werks.   "Plant
  CLEAR: ext_stpox, ext_stpox[].

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
   EXPORTING
*       FTREL                       = ' '
*       ALTVO                       = ' '
*       AUFSW                       = ' '
*       AUMGB                       = ' '
*       AUMNG                       = 0
*       AUSKZ                       = ' '
*       AMIND                       = ' '
*       BAGRP                       = ' '
*       BEIKZ                       = ' '
*       BESSL                       = ' '
*       BGIXO                       = ' '
*       BREMS                       = ' '
     capid                       = p_capid                  "'PP01'
*       CHLST                       = ' '
*       COSPR                       = ' '
*       CUOBJ                       = 000000000000000
*       CUOVS                       = 0
*       CUOLS                       = ' '
     datuv                       = p_datuv    " sy-datum
*       DELNL                       = ' '
*       DRLDT                       = ' '
     ehndl                       = p_ehndl    "'1'
     emeng                       = p_emeng    " 10
*       ERSKZ                       = ' '
*       ERSSL                       = ' '
*       FBSTP                       = ' '
*       KNFBA                       = ' '
*       KSBVO                       = ' '
*       MBWLS                       = ' '
*       MKTLS                       = 'X'
*       MDMPS                       = ' '
     mehrs                       = p_mehrs    "'X'
*       MKMAT                       = ' '
*       MMAPS                       = ' '
*       SALWW                       = ' '
*       SPLWW                       = ' '
     mmory                       = p_mmory    "'1'
     mtnrv                       = p_mtnrv                  "'T005'
*       NLINK                       = ' '
*       POSTP                       = ' '
*       RNDKZ                       = ' '
*       RVREL                       = ' '
*       SANFR                       = ' '
*       SANIN                       = ' '
*       SANKA                       = ' '
*       SANKO                       = ' '
*       SANVS                       = ' '
*       SCHGT                       = ' '
*       STKKZ                       = ' '
*       STLAL                       = ' '
*       STLAN                       = ' '
*       STPST                       = 0
*       SVWVO                       = 'X'
     werks                       = p_werks                  " 'P001'
*       NORVL                       = ' '
*       MDNOT                       = ' '
*       PANOT                       = ' '
*       QVERW                       = ' '
*       VERID                       = ' '
*       VRSVO                       = 'X'
*     IMPORTING
*       TOPMAT                      =
*       DSTST                       =
    TABLES
      stb                         = ext_stpox
*       MATCAT                      =
   EXCEPTIONS
     alt_not_found               = 1
     call_invalid                = 2
     material_not_found          = 3
     missing_authorization       = 4
     no_bom_found                = 5
     no_plant_data               = 6
     no_suitable_bom_found       = 7
     conversion_error            = 8
     OTHERS                      = 9.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
*&      Form  make_IT_ekpo_mip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_ekpo_mip.
  CLEAR: it_ekpo.
  SELECT ekpo~ebeln ekpo~ebelp ekpo~matnr ekpo~meins ekpo~werks
         ekko~ekgrp ekko~lifnr
    INTO CORRESPONDING FIELDS OF TABLE it_ekpo
    FROM ekpo
      INNER JOIN mara
        ON mara~matnr = ekpo~matnr AND
           mara~mtart = 'ROH1'    "Material Type
      INNER JOIN marc
        ON marc~matnr = ekpo~matnr AND
           marc~werks = ekpo~werks AND
           marc~dispo = 'P01'      AND   "MRP Controller
           marc~sobsl <> '35'     " 35 IS THE MAT WHICH IS UNFOLDED
      INNER JOIN ekko
        ON ekko~ebeln = ekpo~ebeln
    WHERE ekpo~loekz = space  AND
          ekpo~matnr <> space.
ENDFORM.                    " make_IT_ekpo_mip
*---------------------------------------------------------------------*
*       FORM make_IT_eketxxx_draft1                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IMT_EKPO                                                      *
*  -->  EXT_EKETXXX                                                   *
*---------------------------------------------------------------------*
FORM make_it_eketxxx_draft1 USING imt_ekpo    LIKE it_ekpo
                                  ext_eketxxx LIKE it_eket.
  CLEAR: ext_eketxxx.
  LOOP AT imt_ekpo ASSIGNING <fs_ekpo>.
    SELECT * INTO CORRESPONDING FIELDS OF wa_eket
      FROM eket
      WHERE ebeln =  <fs_ekpo>-ebeln AND    "PO no
            ebelp =  <fs_ekpo>-ebelp AND    "Item no
            eindt >= p_eindt.            "Requirement Date

      wa_eket-matnr = <fs_ekpo>-matnr.
      wa_eket-werks = <fs_ekpo>-werks.
      wa_eket-meins = <fs_ekpo>-meins.
      wa_eket-ekgrp = <fs_ekpo>-ekgrp.
      wa_eket-lifnr = <fs_ekpo>-lifnr.
      APPEND wa_eket TO ext_eketxxx.
    ENDSELECT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  openqty_by_delivery_in_lt_eket
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM openqty_by_delivery_in_lt_eket USING pt_eket LIKE it_eket
                                          value(p_section).
  DATA: sum_menge LIKE eket-menge, "Sum of Scheduled qty.
        sum_wemng LIKE eket-wemng. "Sum of Delivered (GR)
  CASE p_section.
    WHEN 'MIP'.
      LOOP AT pt_eket ASSIGNING <fs_eket>.
        SELECT SINGLE SUM( menge ) SUM( wemng )
          INTO (sum_menge, sum_wemng)
          FROM eket
          WHERE ebeln = <fs_eket>-ebeln AND    "PO no
                ebelp = <fs_eket>-ebelp AND    "Item no
                eindt = <fs_eket>-eindt        "Delivery date
          GROUP by ebeln ebelp eindt.
        <fs_eket>-menge = sum_menge.
        <fs_eket>-wemng = sum_wemng.
        <fs_eket>-obmng = sum_menge - sum_wemng.
        <fs_eket>-mnglg = <fs_eket>-obmng.   "Component Open Qty
        <fs_eket>-mmein = <fs_eket>-meins.   "Component Unit

        <fs_eket>-idnrk = <fs_eket>-matnr.   "Component
        CLEAR: <fs_eket>-matnr.
      ENDLOOP.

    WHEN 'VDR'.
      DATA: lt_stpox LIKE TABLE OF stpox.
      "BOM Items (Extended for List Displays)
      FIELD-SYMBOLS: <fs_stpox> LIKE LINE OF lt_stpox.
      DATA: dispo LIKE marc-dispo.  "MRP Controller

      LOOP AT pt_eket ASSIGNING <fs_eket>.
        SELECT SINGLE SUM( menge ) SUM( wemng )
          INTO (sum_menge, sum_wemng)
          FROM eket
          WHERE ebeln = <fs_eket>-ebeln AND    "PO no
                ebelp = <fs_eket>-ebelp AND    "Item no
                eindt = <fs_eket>-eindt        "Delivery date
          GROUP by ebeln ebelp eindt.
        <fs_eket>-menge = sum_menge.
        <fs_eket>-wemng = sum_wemng.
        <fs_eket>-obmng = sum_menge - sum_wemng.

** BOM Unfolding
* Here, lt_stpox is important !!!
        PERFORM cs_bom_expl_mat_v2   "similar to /nCS12
                 TABLES lt_stpox
                 USING 'PP01'  "Application ID
                       <fs_eket>-eindt  "Validity date
                    '1' "EWahrHandl. ' 'kein,'1'modPosMg,'2'%ualeEMengen
                       <fs_eket>-obmng  "Required quantity
                       'X'              "Multi-level explosion
                       '1'  "Memory use ('1'=on;'0'=off;' '=no reaction)
                       <fs_eket>-matnr  "Material
                       <fs_eket>-werks. "Plant
        LOOP AT lt_stpox ASSIGNING <fs_stpox>
                               WHERE mtart = 'ROH1'.
          SELECT SINGLE dispo INTO dispo
            FROM marc
            WHERE matnr = <fs_stpox>-idnrk AND
                  werks = <fs_eket>-werks AND
                  dispo = 'P01'.
          IF sy-subrc = 0.
**** Append lt_eket IT_eketvdr
            CLEAR: wa_eket.
            <fs_eket>-idnrk = <fs_stpox>-idnrk.  "Component
            <fs_eket>-mnglg = <fs_stpox>-mnglg.  "Component Open Qty
            "Calculated Component Quantity in Base Unit of Measure
            <fs_eket>-mmein = <fs_stpox>-mmein.  "Component Unit
            "Base unit of measure(Component)
            MOVE <fs_eket> TO wa_eket.
            APPEND wa_eket TO it_eketvdr.

          ENDIF.

        ENDLOOP.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " openqty_by_delivery_in_lt_eket

*---------------------------------------------------------------------*
*       FORM make_IT_eketmip_final                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PT_EKETMIP                                                    *
*  -->  PIMT_EKET                                                     *
*---------------------------------------------------------------------*
FORM make_it_eketmip_final
           TABLES imt_eketmip_detail
             STRUCTURE wa_eket
           USING  pt_eketmip LIKE it_eketmip.
*FORM make_IT_eketvdr_final & FORM make_IT_eketmip_final are similar
  LOOP AT pt_eketmip ASSIGNING <fs_eket>.
    CLEAR:<fs_eket>-month_0,<fs_eket>-month_1,<fs_eket>-month_2,
          <fs_eket>-month_3,<fs_eket>-month_4,<fs_eket>-month_5.
    LOOP AT imt_eketmip_detail INTO wa_eket
                     WHERE lifnr = <fs_eket>-lifnr AND "Vendor
                           idnrk = <fs_eket>-idnrk."BOM component
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
           EXPORTING
                day_in            = wa_eket-eindt
           IMPORTING
                last_day_of_month = w_lastday_month
           EXCEPTIONS
                day_in_no_date    = 1
                OTHERS            = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      CASE w_lastday_month.
        WHEN w_lastday_month_0.
          <fs_eket>-month_0 = <fs_eket>-month_0 + wa_eket-mnglg.
        WHEN w_lastday_month_1.
          <fs_eket>-month_1 = <fs_eket>-month_1 + wa_eket-mnglg.
        WHEN w_lastday_month_2.
          <fs_eket>-month_2 = <fs_eket>-month_2 + wa_eket-mnglg.
        WHEN w_lastday_month_3.
          <fs_eket>-month_3 = <fs_eket>-month_3 + wa_eket-mnglg.
        WHEN w_lastday_month_4.
          <fs_eket>-month_4 = <fs_eket>-month_4 + wa_eket-mnglg.
        WHEN w_lastday_month_5.
          <fs_eket>-month_5 = <fs_eket>-month_5 + wa_eket-mnglg.
      ENDCASE.
    ENDLOOP.
    <fs_eket>-month_t = <fs_eket>-month_0 + <fs_eket>-month_1 +
                        <fs_eket>-month_2 + <fs_eket>-month_3 +
                        <fs_eket>-month_4 + <fs_eket>-month_5.
  ENDLOOP.
ENDFORM.                    " make_pt_eketmip_final

*---------------------------------------------------------------------*
*       FORM make_IT_eketvdr_final                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PT_EKETMIP                                                    *
*  -->  PIMT_EKET                                                     *
*---------------------------------------------------------------------*
FORM make_it_eketvdr_final
            TABLES imt_eketvdr_detail
              STRUCTURE wa_eket
            USING pt_eketvdr         LIKE it_eketvdr.
*FORM make_IT_eketvdr_final & FORM make_IT_eketmip_final are similar
  LOOP AT pt_eketvdr ASSIGNING <fs_eket>.
    CLEAR:<fs_eket>-month_0,<fs_eket>-month_1,<fs_eket>-month_2,
          <fs_eket>-month_3,<fs_eket>-month_4,<fs_eket>-month_5.
    LOOP AT imt_eketvdr_detail INTO wa_eket
                     WHERE lifnr = <fs_eket>-lifnr AND "Vendor
                           idnrk = <fs_eket>-idnrk."BOM component
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
           EXPORTING
                day_in            = wa_eket-eindt
           IMPORTING
                last_day_of_month = w_lastday_month
           EXCEPTIONS
                day_in_no_date    = 1
                OTHERS            = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      CASE w_lastday_month.
        WHEN w_lastday_month_0.
          <fs_eket>-month_0 = <fs_eket>-month_0 + wa_eket-mnglg.
        WHEN w_lastday_month_1.
          <fs_eket>-month_1 = <fs_eket>-month_1 + wa_eket-mnglg.
        WHEN w_lastday_month_2.
          <fs_eket>-month_2 = <fs_eket>-month_2 + wa_eket-mnglg.
        WHEN w_lastday_month_3.
          <fs_eket>-month_3 = <fs_eket>-month_3 + wa_eket-mnglg.
        WHEN w_lastday_month_4.
          <fs_eket>-month_4 = <fs_eket>-month_4 + wa_eket-mnglg.
        WHEN w_lastday_month_5.
          <fs_eket>-month_5 = <fs_eket>-month_5 + wa_eket-mnglg.
      ENDCASE.
    ENDLOOP.
    <fs_eket>-month_t = <fs_eket>-month_0 + <fs_eket>-month_1 +
                        <fs_eket>-month_2 + <fs_eket>-month_3 +
                        <fs_eket>-month_4 + <fs_eket>-month_5.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_last_day_of_months
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_p_eindt  text
*----------------------------------------------------------------------*
FORM get_last_day_of_months USING value(p_eindt)
                                  value(p_ix).
  DATA: lv_firstday LIKE sy-datum.
  DATA: lv_lastday  LIKE sy-datum.
  lv_firstday = p_eindt.
  DO p_ix TIMES.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
              day_in            = lv_firstday
         IMPORTING
              last_day_of_month = lv_lastday
         EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    CASE sy-index.
      WHEN 1.
        w_lastday_month_0 = lv_lastday.
      WHEN 2.
        w_lastday_month_1 = lv_lastday.
      WHEN 3.
        w_lastday_month_2 = lv_lastday.
      WHEN 4.
        w_lastday_month_3 = lv_lastday.
      WHEN 5.
        w_lastday_month_4 = lv_lastday.
      WHEN 6.
        w_lastday_month_5 = lv_lastday.
    ENDCASE.
    lv_firstday = lv_lastday + 1.
  ENDDO.
ENDFORM.                    " get_last_day_of_months
*&---------------------------------------------------------------------*
*&      Form  make_IT_ekpo_vdr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_ekpo_vdr.
  CLEAR: it_ekpo.
  SELECT ekpo~ebeln ekpo~ebelp ekpo~matnr ekpo~meins ekpo~werks
         ekko~ekgrp ekko~lifnr
    INTO CORRESPONDING FIELDS OF TABLE it_ekpo
    FROM ekpo
      INNER JOIN marc
        ON marc~matnr = ekpo~matnr AND
           marc~werks = ekpo~werks AND
           marc~sobsl = '35'
      INNER JOIN ekko
        ON ekko~ebeln = ekpo~ebeln
    WHERE ekpo~loekz = space  AND
          ekpo~matnr <> space.
ENDFORM.                    " make_IT_ekpo_vdr
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM mask_columns TABLES   p_it_fieldcat STRUCTURE it_fieldcat.
* Build the fieldcat according to DDIC structure ZSMM_6005_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZSMM_6005_01'
       CHANGING
            ct_fieldcat      = p_it_fieldcat[].

* Make Column header
  LOOP AT p_it_fieldcat.
    IF p_it_fieldcat-fieldname = 'IDNRK'.
      p_it_fieldcat-outputlen = 18.
*    ELSEIF p_IT_fieldcat-fieldname = 'USNAM'.
*      p_IT_fieldcat-no_out = 'X'.
    ELSEIF p_it_fieldcat-fieldname = 'ZSECT'.
      p_it_fieldcat-coltext = 'Section'.
      p_it_fieldcat-outputlen = 5.
    ELSEIF p_it_fieldcat-fieldname = 'MONTH_0'.
      p_it_fieldcat-coltext = 'M'.
    ELSEIF p_it_fieldcat-fieldname = 'MONTH_1'.
      p_it_fieldcat-coltext = 'M+1'.
    ELSEIF p_it_fieldcat-fieldname = 'MONTH_2'.
      p_it_fieldcat-coltext = 'M+2'.
    ELSEIF p_it_fieldcat-fieldname = 'MONTH_3'.
      p_it_fieldcat-coltext = 'M+3'.
    ELSEIF p_it_fieldcat-fieldname = 'MONTH_4'.
      p_it_fieldcat-coltext = 'M+4'.
    ELSEIF p_it_fieldcat-fieldname = 'MONTH_5'.
      p_it_fieldcat-coltext = 'M+5'.
    ELSEIF p_it_fieldcat-fieldname = 'MONTH_T'.
      p_it_fieldcat-coltext = 'Total'.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_DESC'.
*      p_IT_fieldcat-checkbox = space.
    ENDIF.
    MODIFY p_it_fieldcat.
  ENDLOOP.
ENDFORM.                    " mask_columns
