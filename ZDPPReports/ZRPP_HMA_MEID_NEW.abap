************************************************************************
* Program Name      : ZRPP_HMA_MEID_NEW
* Author            : Victor
* Creation Date     : 02/21/2014
* Specifications By :
* Development Request No :
* Addl Documentation: copied from ZRPP_HMA_MEID
* Description       : Send production actual and plan data to HMC
* Modification Logs
* Date       Developer    RequestNo    Description
* 02.24.2014 Victor      Added log table: ztpp_hma_meid_l
* 04.14.2014 Victor      Modified program for performance issue
*added temp. function until HMA IT completed program or end of 2014.
* *********************************************************************
REPORT zrpp_hma_meid_new NO STANDARD PAGE HEADING
                                      LINE-SIZE 132
                                      LINE-COUNT 64(1)
                                      MESSAGE-ID zmpp.

TABLES: ausp, ztppvr.
DATA : l_msgtxt(100),
       l_result(1),
       w_error(1).

CONSTANTS: c_dest(10) VALUE 'WMPP01'.   "Interface Destination.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztpp_hma_meid.
DATA: END OF it_data.
DATA: BEGIN OF it_data_tmp OCCURS 0,
        objek           LIKE ausp-objek,
        vin_atinn         LIKE ausp-atwrt,
        model_atinn       LIKE ausp-atwrt,
        219_1_atinn       LIKE ausp-atwrt,
        219_5_atinn       LIKE ausp-atwrt,
        ext_color_atinn   LIKE ausp-atwrt,
        int_color_atinn   LIKE ausp-atwrt,
        219_30_atinn      LIKE ausp-atwrt,
        219_4_atinn       LIKE ausp-atwrt,
        rp06_dt_atinn     LIKE ausp-atflv,
        219_7_atinn       LIKE ausp-atwrt,
        219_131_atinn     LIKE ausp-atwrt,
        219_13_atinn      LIKE ausp-atwrt,
        219_184_atinn     LIKE ausp-atwrt,
        219_108_atinn     LIKE ausp-atwrt,
        air_11_atinn      LIKE ausp-atwrt,
        dest_cd_atinn     LIKE ausp-atwrt,
        219_14_atinn      LIKE ausp-atwrt,
        219_112_atinn     LIKE ausp-atwrt,
        219_31_atinn      LIKE ausp-atwrt,
        219_180_atinn     LIKE ausp-atwrt,
        219_28_atinn      LIKE ausp-atwrt,
        ocn_atinn         LIKE ausp-atwrt,
        mi_atinn          LIKE ausp-atwrt,
      END OF it_data_tmp.

DATA  w_dest(30).

DATA: w_vin_atinn     LIKE cabn-atinn,
      w_model_atinn     LIKE cabn-atinn,
      w_219_1_atinn     LIKE cabn-atinn,
      w_219_5_atinn     LIKE cabn-atinn,
      w_ext_color_atinn     LIKE cabn-atinn,
      w_int_color_atinn     LIKE cabn-atinn,
      w_219_30_atinn     LIKE cabn-atinn,
      w_219_4_atinn     LIKE cabn-atinn,
      w_rp06_dt_atinn     LIKE cabn-atinn,
      w_219_7_atinn     LIKE cabn-atinn,
      w_219_131_atinn     LIKE cabn-atinn,
      w_219_13_atinn     LIKE cabn-atinn,

      w_219_184_atinn     LIKE cabn-atinn,
      w_219_108_atinn     LIKE cabn-atinn,
      w_air_11_atinn     LIKE cabn-atinn,
      w_dest_cd_atinn     LIKE cabn-atinn,
      w_219_14_atinn     LIKE cabn-atinn,
      w_219_112_atinn    LIKE cabn-atinn,
      w_219_31_atinn     LIKE cabn-atinn,
      w_219_180_atinn     LIKE cabn-atinn,
      w_219_28_atinn     LIKE cabn-atinn,
      w_ocn_atinn        LIKE cabn-atinn,
      w_mi_atinn         LIKE cabn-atinn.

*---------------------------------------------------------------------*
*       SELECTION-SCREEN                                              *
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.

SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 1.
PARAMETERS: r_work RADIOBUTTON GROUP r1   USER-COMMAND c1.
SELECTION-SCREEN COMMENT 5(20) text-t03 FOR FIELD r_work.
PARAMETERS: r_date RADIOBUTTON GROUP r1   DEFAULT 'X' .
SELECTION-SCREEN COMMENT 30(10)  text-t01 FOR FIELD r_date.
PARAMETERS: r_body RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 50(10) text-t02 FOR FIELD r_body.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS: s_date FOR ztppvr-zedat NO-EXTENSION MODIF ID g1,
                s_object FOR ausp-objek  MODIF ID g2.
PARAMETERS: p_day(1) TYPE n DEFAULT 3 MODIF ID g3.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
PARAMETERS: p_send AS CHECKBOX.
PARAMETERS:  p_rver	LIKE somlreci1-receiver OBLIGATORY
                                     DEFAULT 'PP_HMA_MEID'.
SELECTION-SCREEN END OF BLOCK b2.

*---------------------------------------------------------------------*
*       INITIALIZATION.                                            *
*---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM pro_init.

*---------------------------------------------------------------------*
*       AT SELECTION-SCREEN OUTPUT.                                          *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

*---------------------------------------------------------------------*
*       START-OF-SELECTION                                            *
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM check_input_value.
  PERFORM get_data.

  IF w_error IS INITIAL.
    PERFORM send_data.
  ENDIF.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
FORM get_data.
  DATA: l_uzeit LIKE sy-uzeit,
        l_datum(8) TYPE n,
        l_matnr LIKE mara-matnr.

  DATA: BEGIN OF lt_ztppvm OCCURS 0,
          p_model LIKE ztppvr-p_model,
          p_body_serial LIKE ztppvr-p_body_serial,
          p_airbag_no11 LIKE ztppvm-p_airbag_no11,
          zedat LIKE ztppvm-zedat,
          zetim LIKE ztppvm-zetim,
        END OF lt_ztppvm.
  DATA : BEGIN OF it_body_tmp OCCURS 0,
           p_model        LIKE ztppvm-p_model,
           p_body_serial  LIKE ztppvm-p_body_serial,
         END OF it_body_tmp.

  DATA: BEGIN OF lt_ausp OCCURS 0,
        lv_object LIKE ausp-objek,
        END OF lt_ausp.

  DATA: BEGIN OF lt_ztbm_abxopvdt OCCURS 0,
          carx LIKE ztbm_abxopvdt-carx,
          clno LIKE ztbm_abxopvdt-clno,
          valu LIKE ztbm_abxopvdt-valu,
          clnm LIKE ztbm_abxopvdt-clnm,
        END OF lt_ztbm_abxopvdt.

* by Daniel on 05/23/11 {
  DATA: BEGIN OF lt_ztpp_model_conv OCCURS 0,
          bmdl LIKE ztpp_model_conv-bmdl,
          model LIKE ztpp_model_conv-model,
          mobis LIKE ztpp_model_conv-mobis,
        END OF lt_ztpp_model_conv.
* }

  DATA: l_vals LIKE TABLE OF zspp_vin_value WITH HEADER LINE.
  DATA: zdats LIKE sy-datum,
        l_108_flag(1).

  CLEAR: w_error, it_body_tmp[], it_body_tmp, it_data[], it_data,
         lt_ausp[], lt_ausp.

  CONCATENATE sy-uzeit+0(2) '0000' INTO   l_uzeit.

**--By Working Days & By Date
  IF r_date EQ 'X' OR r_work EQ 'X'.
    IF r_work = 'X'.
      PERFORM set_date_time.
    ENDIF.

    SELECT p_model p_body_serial p_airbag_no11 zedat  zetim
      INTO TABLE lt_ztppvm
      FROM ztppvm
      WHERE k04pln = 'M'
        AND p_airbag_no11 <> ''
        AND zedat IN  s_date
*        AND zetim BETWEEN l_uzeit_1 AND l_uzeit
        AND zresult = 'S'.

    IF r_work = 'X'.  "current day -> Send up to previous hour
      DELETE lt_ztppvm WHERE zedat  = sy-datum
                         AND zetim  > l_uzeit.
    ENDIF.

    SORT lt_ztppvm BY p_model p_body_serial zedat DESCENDING
                                            zetim DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_ztppvm
                              COMPARING  p_model p_body_serial.

    IF lt_ztppvm[] IS INITIAL.
      MESSAGE i000 WITH 'No Data'.
      w_error = 'X'.
      EXIT.
    ENDIF.

    LOOP AT lt_ztppvm.
      CONCATENATE lt_ztppvm-p_model lt_ztppvm-p_body_serial
             INTO lt_ausp-lv_object.
      COLLECT lt_ausp.
    ENDLOOP.

**--Input Model+Body
  ELSE.
    LOOP AT s_object.
      APPEND s_object-low TO lt_ausp.

      it_body_tmp-p_model        = s_object-low+0(3).
      it_body_tmp-p_body_serial  = s_object-low+3(6).
      APPEND it_body_tmp.

    ENDLOOP.

    SELECT p_model p_body_serial p_airbag_no11 zedat  zetim
       INTO  TABLE lt_ztppvm
      FROM ztppvm
      FOR ALL ENTRIES IN it_body_tmp
      WHERE k04pln = 'M'
*        AND p_airbag_no11 <> ''
      AND p_model       = it_body_tmp-p_model
      AND p_body_serial = it_body_tmp-p_body_serial
     AND zresult = 'S'.

    SORT lt_ztppvm BY p_model p_body_serial zedat DESCENDING
                                            zetim DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_ztppvm
                              COMPARING  p_model p_body_serial.

    IF lt_ausp[] IS INITIAL.
      MESSAGE i000 WITH 'No Data'.
      w_error = 'X'.
      EXIT.
    ENDIF.

  ENDIF.

  SELECT carx clno valu clnm INTO TABLE lt_ztbm_abxopvdt
    FROM ztbm_abxopvdt.

  SORT lt_ztbm_abxopvdt BY carx clno valu clnm.

  SELECT bmdl model mobis INTO TABLE lt_ztpp_model_conv
    FROM ztpp_model_conv.

  SORT lt_ztpp_model_conv BY bmdl model mobis.

*-< for Performacne : Victor 04.14.2014
  PERFORM get_characteristic.

  SELECT  a~objek  b~atwrt AS vin_atinn c~atwrt AS model_atinn
          d~atwrt AS 219_1_atinn       e~atwrt AS 219_5_atinn
          f~atwrt AS ext_color_atinn   g~atwrt AS int_color_atinn
          h~atwrt AS 219_30_atinn      i~atwrt AS 219_4_atinn
          a~atflv AS rp06_dt_atinn     k~atwrt AS 219_7_atinn
          l~atwrt AS 219_131_atinn     m~atwrt AS 219_13_atinn
          n~atwrt AS 219_184_atinn     o~atwrt AS 219_108_atinn
          p~atwrt AS air_11_atinn      q~atwrt AS dest_cd_atinn
          r~atwrt AS 219_14_atinn      s~atwrt AS 219_112_atinn
          t~atwrt AS 219_31_atinn      u~atwrt AS 219_180_atinn
          v~atwrt AS 219_28_atinn      w~atwrt AS ocn_atinn
          x~atwrt AS mi_atinn
     INTO CORRESPONDING FIELDS OF TABLE it_data_tmp
     FROM ausp AS a LEFT OUTER JOIN ausp AS b
                      ON b~objek = a~objek
                     AND b~atinn = w_vin_atinn
                     AND b~mafid = a~mafid
                     AND b~klart = a~klart
                    LEFT OUTER JOIN ausp AS c
                      ON c~objek = a~objek
                     AND c~atinn = w_model_atinn
                     AND c~mafid = a~mafid
                     AND c~klart = a~klart
                    LEFT OUTER JOIN ausp AS d
                      ON d~objek = a~objek
                     AND d~atinn = w_219_1_atinn
                     AND d~mafid = a~mafid
                     AND d~klart = a~klart
                    LEFT OUTER JOIN ausp AS e
                      ON e~objek = a~objek
                     AND e~atinn = w_219_5_atinn
                     AND e~mafid = a~mafid
                     AND e~klart = a~klart
                    LEFT OUTER JOIN ausp AS f
                      ON f~objek = a~objek
                     AND f~atinn = w_ext_color_atinn
                     AND f~mafid = a~mafid
                     AND f~klart = a~klart
                    LEFT OUTER JOIN ausp AS g
                      ON g~objek = a~objek
                     AND g~atinn = w_int_color_atinn
                     AND g~mafid = a~mafid
                     AND g~klart = a~klart
                    LEFT OUTER JOIN ausp AS h
                      ON h~objek = a~objek
                     AND h~atinn = w_219_30_atinn
                     AND h~mafid = a~mafid
                     AND h~klart = a~klart
                    LEFT OUTER JOIN ausp AS i
                      ON i~objek = a~objek
                     AND i~atinn = w_219_4_atinn
                     AND i~mafid = a~mafid
                     AND i~klart = a~klart
                    LEFT OUTER JOIN ausp AS k
                      ON k~objek = a~objek
                     AND k~atinn = w_219_7_atinn
                     AND k~mafid = a~mafid
                     AND k~klart = a~klart
                    LEFT OUTER JOIN ausp AS l
                      ON l~objek = a~objek
                     AND l~atinn = w_219_131_atinn
                     AND l~mafid = a~mafid
                     AND l~klart = a~klart
                    LEFT OUTER JOIN ausp AS m
                      ON m~objek = a~objek
                     AND m~atinn = w_219_13_atinn
                     AND m~mafid = a~mafid
                     AND m~klart = a~klart
                    LEFT OUTER JOIN ausp AS n
                      ON n~objek = a~objek
                     AND n~atinn = w_219_184_atinn
                     AND n~mafid = a~mafid
                     AND n~klart = a~klart
                    LEFT OUTER JOIN ausp AS o
                      ON o~objek = a~objek
                     AND o~atinn = w_219_108_atinn
                     AND o~mafid = a~mafid
                     AND o~klart = a~klart
                    LEFT OUTER JOIN ausp AS p
                      ON p~objek = a~objek
                     AND p~atinn = w_air_11_atinn
                     AND p~mafid = a~mafid
                     AND p~klart = a~klart
                    LEFT OUTER JOIN ausp AS q
                      ON q~objek = a~objek
                     AND q~atinn = w_dest_cd_atinn
                     AND q~mafid = a~mafid
                     AND q~klart = a~klart
                    LEFT OUTER JOIN ausp AS r
                      ON r~objek = a~objek
                     AND r~atinn = w_219_14_atinn
                     AND r~mafid = a~mafid
                     AND r~klart = a~klart
                    LEFT OUTER JOIN ausp AS s
                      ON s~objek = a~objek
                     AND s~atinn = w_219_112_atinn
                     AND s~mafid = a~mafid
                     AND s~klart = a~klart
                    LEFT OUTER JOIN ausp AS t
                      ON t~objek = a~objek
                     AND t~atinn = w_219_31_atinn
                     AND t~mafid = a~mafid
                     AND t~klart = a~klart
                    LEFT OUTER JOIN ausp AS u
                      ON u~objek = a~objek
                     AND u~atinn = w_219_180_atinn
                     AND u~mafid = a~mafid
                     AND u~klart = a~klart
                    LEFT OUTER JOIN ausp AS v
                      ON v~objek = a~objek
                     AND v~atinn = w_219_28_atinn
                     AND v~mafid = a~mafid
                     AND v~klart = a~klart
                    LEFT OUTER JOIN ausp AS w
                      ON w~objek = a~objek
                     AND w~atinn = w_ocn_atinn
                     AND w~mafid = a~mafid
                     AND w~klart = a~klart
                    LEFT OUTER JOIN ausp AS x
                      ON x~objek = a~objek
                     AND x~atinn = w_mi_atinn
                     AND x~mafid = a~mafid
                     AND x~klart = a~klart

      FOR ALL ENTRIES IN lt_ausp
    WHERE a~objek = lt_ausp-lv_object
      AND a~atinn = w_rp06_dt_atinn
      AND a~klart = '002'.

  CHECK it_data_tmp[] IS NOT INITIAL.

  LOOP AT it_data_tmp.
    it_data-vin     =  it_data_tmp-vin_atinn.
    it_data-model   =  it_data_tmp-model_atinn.
    it_data-ext_col_d   =  it_data_tmp-ext_color_atinn.
    it_data-int_col_d   =  it_data_tmp-int_color_atinn.

    IF it_data_tmp-model_atinn IS NOT INITIAL.
      READ TABLE lt_ztpp_model_conv
                    WITH KEY bmdl = it_data_tmp-model_atinn.
      it_data-hma_model = lt_ztpp_model_conv-mobis.
    ENDIF.

    IF it_data_tmp-219_1_atinn IS NOT INITIAL.
      READ TABLE lt_ztbm_abxopvdt WITH KEY carx = it_data-model
                                       clno = '1'
                                       valu = it_data_tmp-219_1_atinn
                                       BINARY SEARCH.
      it_data-model_year = lt_ztbm_abxopvdt-clnm.
    ENDIF.

    IF it_data_tmp-219_5_atinn IS NOT INITIAL.
      READ TABLE lt_ztbm_abxopvdt WITH KEY carx = it_data-model
                                           clno = '5'
                                           valu = it_data_tmp-219_5_atinn
                                           BINARY SEARCH.
      it_data-trim_level = lt_ztbm_abxopvdt-clnm.
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      IF it_data_tmp-219_31_atinn = 'N'.
        it_data-head_unit_type = 'AVN'.
        it_data-gen_type       = '2'.
      ELSE.
        it_data-head_unit_type = 'Radio'.
        it_data-gen_type       = '1'.
      ENDIF.
    ELSE.
      IF it_data_tmp-219_30_atinn = 'N'.
        it_data-head_unit_type = 'AVN'.
      ELSE.
        it_data-head_unit_type = 'Radio'.
      ENDIF.
    ENDIF.

    IF it_data_tmp-219_4_atinn IS NOT INITIAL.
      READ TABLE lt_ztbm_abxopvdt WITH KEY carx = it_data-model
                                      clno = '4'
                                      valu = it_data_tmp-219_4_atinn
                                      BINARY SEARCH.
      it_data-body_type = lt_ztbm_abxopvdt-clnm.
    ENDIF.

    IF it_data_tmp-rp06_dt_atinn IS NOT INITIAL.
      it_data-production_date  = l_datum = it_data_tmp-rp06_dt_atinn.
    ELSE.
      it_data-production_date = sy-datum.
    ENDIF.
    CONCATENATE it_data-production_date+4(2)
                               '/' it_data-production_date+6(2) '/'
                                   it_data-production_date+0(4)
                                       INTO it_data-production_date.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      it_data-transmission_typ = '1'.
    ELSE.
      IF it_data_tmp-219_7_atinn = 'F'.
        it_data-transmission_typ = '1'.
      ELSE.
        it_data-transmission_typ = '0'.
      ENDIF.
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'TCF'.
      IF it_data_tmp-219_131_atinn = 'F'.
        it_data-smart_key_ind = '1'.
      ELSE.
        it_data-smart_key_ind = '0'.
      ENDIF.
    ELSEIF it_data_tmp-objek+0(3) = 'INF'
        OR it_data_tmp-objek+0(3) = 'C2F'.
      IF it_data_tmp-219_131_atinn = 'T'.
        it_data-smart_key_ind = '1'.
      ELSE.
        it_data-smart_key_ind = '0'.
      ENDIF.
    ELSE.  "Others
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      it_data-abs_ind = '1'.
    ELSE.
      IF it_data_tmp-219_13_atinn = 'E'.
        it_data-abs_ind = '1'.
      ELSE.
        it_data-abs_ind = '0'.
      ENDIF.
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      it_data-tpms_ind = '1'.
    ELSE.
      IF it_data_tmp-219_184_atinn = 'T'.
        it_data-tpms_ind = '1'.
      ELSE.
        it_data-tpms_ind = '0'.
      ENDIF.
    ENDIF.
    IF it_data_tmp-219_108_atinn <> 'T'.  "Only send this Body to HMA
      CONTINUE.
    ENDIF.

    IF it_data_tmp-air_11_atinn <> ''.
      it_data-meid = it_data_tmp-air_11_atinn.
    ELSE.
      READ TABLE lt_ztppvm WITH KEY p_model = it_data_tmp-objek+0(3)
                               p_body_serial = it_data_tmp-objek+3(6)
                               BINARY SEARCH.
      IF sy-subrc = 0.
        it_data-meid  = lt_ztppvm-p_airbag_no11.
      ENDIF.
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      IF it_data_tmp-219_14_atinn = 'E'.
        it_data-epb_ind = '1'.
      ELSE.
        it_data-epb_ind = '0'.
      ENDIF.
    ELSE.
      it_data-epb_ind = '0'.
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      IF it_data_tmp-219_112_atinn = 'S'.
        it_data-scc_ind = '1'.
      ELSE.
        it_data-scc_ind = '0'.
      ENDIF.
    ELSE.
      it_data-scc_ind = '0'.
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      it_data-ecs_ind = '0'.
    ELSE.
      it_data-ecs_ind = '1'.
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      IF it_data_tmp-219_180_atinn = 'S'.
        it_data-ldws_ind = '1'.
      ELSE.
        it_data-ldws_ind = '0'.
      ENDIF.
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      IF it_data_tmp-219_28_atinn = 'D'.
        it_data-fatc_ind  = '1'.
      ELSE.
        it_data-fatc_ind  = '0'.
      ENDIF.
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      CONCATENATE it_data_tmp-mi_atinn it_data_tmp-ocn_atinn
                                     INTO it_data-micd_ocn.
    ENDIF.

    IF it_data_tmp-objek+0(3) = 'C2F'.
      it_data-ems_ind = '1'.
      it_data-tcu_ind = '1'.
      it_data-fuel_type = 'G'.
*      it_data-dev_mdl_type = ''.
    ENDIF.

    SELECT SINGLE zvin INTO it_data-int_vin
      FROM ztsd_um
      WHERE model_code = it_data_tmp-objek+0(3)
        AND body_no    = it_data_tmp-objek+3(6).

    it_data-dest_mkt_code  =  it_data_tmp-dest_cd_atinn.

    it_data-production_plant = 'HMMA'.
    it_data-vehicle_type_ind = '0'.
    it_data-mdps_ind = '1'.



    it_data-auto_park_supp_s = '0'.
    it_data-acu_ind = '1'.
    it_data-engine_type = ' '.
    it_data-minn = ' '.
    it_data-tmu_manufacturer = ' '.
    it_data-telematics_usim_ = ' '.
    it_data-zuser = sy-uname.
    it_data-zsdat = sy-datum.
    it_data-zstim = sy-uzeit.
    CONCATENATE sy-datum sy-uzeit
              INTO it_data-downloaded_time.

    APPEND it_data. CLEAR : it_data.
  ENDLOOP.
*->

*  LOOP AT lt_ausp.
*    CLEAR : l_108_flag.
*
*    l_matnr = lt_ausp-lv_object.
*
*    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*      EXPORTING
*        object       = l_matnr
*        ctype        = '002'
*      TABLES
*        val_table    = l_vals
*      EXCEPTIONS
*        no_data      = 1
*        error_mode   = 2
*        error_object = 3
*        error_value  = 4
*        OTHERS       = 5.
*
*    LOOP AT l_vals.
*      CASE l_vals-atnam.
*        WHEN 'P_VIN'.
*          it_data-vin = l_vals-atwrt.
*        WHEN 'P_MODEL'.
*          it_data-model = l_vals-atwrt.
*          it_data-hma_model = l_vals-atwrt.
*        WHEN 'P_219_1'.
*          it_data-model_year = l_vals-atwrt.
*        WHEN 'P_219_5'.
*          it_data-trim_level = l_vals-atwrt.
*        WHEN 'P_EXT_COLOR'.
*          it_data-ext_col_d = l_vals-atwrt.
*        WHEN 'P_INT_COLOR'.
*          it_data-int_col_d = l_vals-atwrt.
*        WHEN 'P_219_30'.
*          IF l_vals-atwrt = 'N'.
*            it_data-head_unit_type = 'AVN'.
*          ELSE.
*            it_data-head_unit_type = 'Radio'.
*          ENDIF.
*        WHEN 'P_219_4'.
*          it_data-body_type = l_vals-atwrt.
** by Daniel on 06/13/11 {
**        WHEN 'P_RP09_SHOP_DATE'.
*        WHEN 'P_RP06_SHOP_DATE'.
** }
*          it_data-production_date = l_vals-atwrt.
*        WHEN 'P_219_7'.
*          IF l_vals-atwrt = 'F'.
*            it_data-transmission_typ = '1'.
*          ELSE.
*            it_data-transmission_typ = '0'.
*          ENDIF.
*        WHEN 'P_219_131'.  "02.07.2014 Victor
*          IF lt_ausp-lv_object+0(3) = 'TCF'.
*            IF l_vals-atwrt = 'F'.
*              it_data-smart_key_ind = '1'.
*            ELSE.
*              it_data-smart_key_ind = '0'.
*            ENDIF.
*          ELSEIF lt_ausp-lv_object+0(3) = 'INF'
*              OR lt_ausp-lv_object+0(3) = 'C2F'.
*            IF l_vals-atwrt = 'T'.
*              it_data-smart_key_ind = '1'.
*            ELSE.
*              it_data-smart_key_ind = '0'.
*            ENDIF.
*          ELSE.  "Others
*          ENDIF.
*        WHEN 'P_219_13'.
*          IF l_vals-atwrt = 'E'.
*            it_data-abs_ind = '1'.
*          ELSE.
*            it_data-abs_ind = '0'.
*          ENDIF.
*        WHEN 'P_219_184'.
*          IF l_vals-atwrt = 'T'.
*            it_data-tpms_ind = '1'.
*          ELSE.
*            it_data-tpms_ind = '0'.
*          ENDIF.
*        WHEN 'P_219_108'.    "02.21.2014
*          IF l_vals-atwrt = 'T'.  "Only send this Body to HMA
*            l_108_flag = 'X'.
*          ELSE.
*            EXIT.
*          ENDIF.
*        WHEN 'P_AIRBAG_NO11'.
*          IF l_vals-atwrt <> ''.
*            it_data-meid = l_vals-atwrt.
*          ELSE.
*            it_data-meid = ''.
*          ENDIF.
*** Furong on 03/19/12
*        WHEN 'P_DESTINATION_CODE'.
*          it_data-dest_mkt_code = l_vals-atwrt.
*** End on 03/19/12
*
*** End on 08/08/11          ENDIF.
*      ENDCASE.
*    ENDLOOP.
*
*    IF l_108_flag <> 'X'.  "Check 219_108
*      CONTINUE.
*    ENDIF.
*
**- 02.26.2014 Victor
*    IF  it_data-meid IS INITIAL.
*      READ TABLE lt_ztppvm WITH KEY p_model = lt_ausp-lv_object+0(3)
*                               p_body_serial = lt_ausp-lv_object+3(6)
*                               BINARY SEARCH.
*      IF sy-subrc = 0.
*        it_data-meid  = lt_ztppvm-p_airbag_no11.
*      ENDIF.
*    ENDIF.
*
** by Daniel on 06/13/11 {
*    IF it_data-production_date IS INITIAL.
*      CONCATENATE sy-datum+4(2) '/'
*                  sy-datum+6(2) '/' sy-datum+0(4)
*             INTO it_data-production_date.
*    ENDIF.
** }
*
*    READ TABLE lt_ztbm_abxopvdt WITH KEY carx = it_data-model
*                                         clno = '1'
*                                         valu = it_data-model_year
*                                         BINARY SEARCH.
*    it_data-model_year = lt_ztbm_abxopvdt-clnm.
*
*    READ TABLE lt_ztbm_abxopvdt WITH KEY carx = it_data-model
*                                         clno = '5'
*                                         valu = it_data-trim_level
*                                         BINARY SEARCH.
*    it_data-trim_level = lt_ztbm_abxopvdt-clnm.
*
*    READ TABLE lt_ztbm_abxopvdt WITH KEY carx = it_data-model
*                                         clno = '4'
*                                         valu = it_data-body_type
*                                         BINARY SEARCH.
*    it_data-body_type = lt_ztbm_abxopvdt-clnm.
*
** by Daniel on 05/23/11 {
*    READ TABLE lt_ztpp_model_conv WITH KEY bmdl = it_data-hma_model.
*    it_data-hma_model = lt_ztpp_model_conv-mobis.
** }
*
*
*** Furong on 03/19/12 - get internal VIN
**   read table LT_ZVIN WITH KEY MODEL_CODE = p_model
**                               BODY_NO = p_body_serial.
*    SELECT SINGLE zvin INTO it_data-int_vin
*      FROM ztsd_um
*      WHERE model_code = lt_ausp-lv_object+0(3)
*        AND body_no = lt_ausp-lv_object+3(6).
*
*** End on 03/19/12
*
*    it_data-production_plant = 'HMMA'.
*    it_data-vehicle_type_ind = '0'.
*    it_data-mdps_ind = '1'.
*    it_data-epb_ind = '0'.
*    it_data-scc_ind = '0'.
*    it_data-ecs_ind = '1'.
*    it_data-auto_park_supp_s = '0'.
*    it_data-acu_ind = '1'.
*    it_data-engine_type = ' '.
*    it_data-minn = ' '.
*    it_data-tmu_manufacturer = ' '.
*    it_data-telematics_usim_ = ' '.
*    CONCATENATE sy-datum sy-uzeit INTO it_data-downloaded_time.
*    it_data-zuser = sy-uname.
*    it_data-zsdat = sy-datum.
*    it_data-zstim = sy-uzeit.
**    IT_DATA-ZEDAT = SY-DATUM.
**    IT_DATA-ZETIM = SY-UZEIT.
*
*    APPEND it_data.
*    CLEAR: it_data.
*    LOOP AT l_vals.
*      CLEAR: l_vals-atwrt.
*      MODIFY l_vals.
*    ENDLOOP.
*  ENDLOOP.

  CLEAR: lt_ztbm_abxopvdt.

** Furong on 08/07/12
  IF it_data[] IS INITIAL.
    MESSAGE s000 WITH 'There is No data'.
    w_error = 'X'.
    EXIT.
  ENDIF.
** End on 08/07/12

** on 06/05/12
  IF r_date EQ 'X' OR r_work EQ 'X'.
    PERFORM check_duplicate_data.
  ENDIF.
** End


* by Daniel on 08/18/11 {
  PERFORM check_empty_data.
  IF it_data[] IS INITIAL.
    MESSAGE s000 WITH 'No data found'.
    w_error = 'X'.
  ENDIF.
* }

ENDFORM.                    "get_data

*&---------------------------------------------------------------------*
*&      Form  read_normal_class
*&---------------------------------------------------------------------*
FORM read_normal_class USING  p_vmno  p_char
                             CHANGING p_value.
  SELECT SINGLE au~atwrt
    INTO p_value
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_vmno      AND
          klart = '002'       AND
          ca~atnam = p_char  .
ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK_OBJEK  text
*      -->P_0461   text
*      <--P_L_ATFLV_TEMP  text
*----------------------------------------------------------------------*
FORM read_normal_class_atflv USING  p_vmno  p_char
                             CHANGING p_value.
  SELECT SINGLE au~atflv
      INTO p_value
      FROM ausp AS au
        INNER JOIN cabn AS ca ON au~atinn = ca~atinn
      WHERE objek = p_vmno      AND
            klart = '002'       AND
            ca~atnam = p_char  .
ENDFORM.                    " READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*&      Form  SEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_data.
  DATA: l_result(1),
         l_totrec TYPE i,
         l_srec TYPE i,
         l_frec TYPE i,
         l_msgtxt(60),
         l_seq TYPE ztpp_hma_meid_l-zseq.

  DATA : it_meid_log LIKE ztpp_hma_meid_l OCCURS 0 WITH HEADER LINE.

  w_dest = 'WMPP01'.

  DESCRIBE TABLE it_data LINES l_totrec.

  IF p_send = 'X'.
    SELECT zseq INTO l_seq
    FROM ztpp_hma_meid_l
      UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
    ENDSELECT.

    CALL FUNCTION 'Z_FPP_SET_HMA_MEID'
      DESTINATION w_dest
      IMPORTING
        flag                  = l_result
      TABLES
        i_contents            = it_data
      EXCEPTIONS
        communication_failure = 1  MESSAGE l_msgtxt
        system_failure        = 2  MESSAGE l_msgtxt.

    IF l_result = 'S' OR l_result = 's'.
      LOOP AT it_data.
        it_data-zresult = 'S'.
        it_data-zedat = sy-datum.
        it_data-zetim = sy-uzeit.

        MOVE-CORRESPONDING it_data TO it_meid_log.
        it_meid_log-zdate = sy-datum.
        it_meid_log-zseq  = l_seq + sy-tabix.
        it_meid_log-ztime = sy-uzeit.

        MODIFY it_data.
        APPEND it_meid_log.
      ENDLOOP.
      WRITE: / 'EAI Success, Total records are: ', l_totrec.
    ELSE.
      LOOP AT it_data.
        it_data-zresult = 'E'.
        it_data-zedat = sy-datum.
        it_data-zetim = sy-uzeit.
        it_data-zmsg = l_msgtxt.

        MOVE-CORRESPONDING it_data TO it_meid_log.
        it_meid_log-zdate = sy-datum.
        it_meid_log-zseq  = l_seq + sy-tabix.
        it_meid_log-ztime = sy-uzeit.

        MODIFY it_data.
        APPEND it_meid_log.
      ENDLOOP.
      WRITE: / 'EAI Fail, Total records are: ', l_totrec.
* by Daniel on 08/18/11 {
      PERFORM send_mail.
* }
    ENDIF.
  ELSE.
    LOOP AT it_data.
      it_data-zmsg = 'Did not send the data to HMA...'.
      MODIFY it_data.
    ENDLOOP.
    WRITE: / 'EAI is skipped, Total records saved are: ', l_totrec.
  ENDIF.

  MODIFY ztpp_hma_meid FROM TABLE it_data.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

*-LOG
  INSERT ztpp_hma_meid_l FROM TABLE it_meid_log
                      ACCEPTING DUPLICATE KEYS.
ENDFORM.                    " SEND_DATA
*&---------------------------------------------------------------------*
*&      Form  check_duplicate_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_duplicate_data.
  DATA: lt_temp LIKE TABLE OF zspp_hma_meid WITH HEADER LINE,
        lt_exist LIKE TABLE OF zspp_hma_meid WITH HEADER LINE.
  DATA: l_index LIKE sy-tabix.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_exist
  FROM ztpp_hma_meid
  FOR ALL ENTRIES IN it_data
  WHERE vin = it_data-vin
    AND zresult = 'S'.

  CHECK lt_exist[] IS NOT INITIAL.

  SORT lt_exist BY vin.

  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO lt_temp.
    READ TABLE lt_exist WITH KEY vin = lt_temp-vin
                                  BINARY SEARCH.
    IF sy-subrc = 0.
      IF lt_temp = lt_exist.
        DELETE it_data.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_duplicate_data
*&---------------------------------------------------------------------*
*&      Form  check_empty_data
*&---------------------------------------------------------------------*
FORM check_empty_data.

  LOOP AT it_data.
    IF it_data-vin = space OR it_data-meid = space.
      DELETE it_data.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_empty_data
*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_mail.
  DATA: $lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.

  MOVE 'MEID to HMA by EAI is failed. Please check.' TO $lt_body.
  APPEND $lt_body.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'MEID to HMA Interface Failure Alert'
      p_rec_type = 'C'
      p_receiver = p_rver
    TABLES
      pt_body    = $lt_body.
ENDFORM.                    " send_mail
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM modify_screen .

  LOOP AT SCREEN.
    IF r_date = 'X'.
      IF screen-group1 = 'G2' OR screen-group1 = 'G3'.
        screen-invisible = 1.
        screen-input     = 0.
        MODIFY SCREEN.
      ENDIF.

    ELSEIF r_work = 'X'.
      IF screen-group1 = 'G1' OR screen-group1 = 'G2'.
        screen-invisible = 1.
        screen-input     = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-group1 = 'G1' OR screen-group1 = 'G3'.
        screen-invisible = 1.
        screen-input     = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  PRO_INIT
*&---------------------------------------------------------------------*
FORM pro_init .
*  CLEAR : s_date[], s_date.
**---- WORKING DATE ??
*
*  s_date-SIGN     = 'I'.
*  s_date-OPTION   = 'BT'.
*  s_date-low    = sy-datum - 2.
*  s_date-high   = sy-datum.
*  APPEND s_date.

ENDFORM.                    " PRO_INIT
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
FORM check_input_value .
  IF r_work = 'X'.
    IF p_day IS INITIAL.
      MESSAGE s000 WITH 'Please Input Days of checking'.
      STOP.
    ENDIF.
  ELSEIF r_date = 'X'.
    IF s_date[] IS INITIAL.
      MESSAGE s000 WITH 'Please Input Production Date'.
      STOP.
    ENDIF.
  ELSE.
    IF s_object[] IS INITIAL.
      MESSAGE s000 WITH 'Please Input Model & Body No'.
      STOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  SET_DATE_TIME
*&---------------------------------------------------------------------*
FORM set_date_time .
  DATA :   c_werks    TYPE werks_d VALUE 'P001',
           c_arbpl    TYPE arbpl   VALUE 'T'.

  DATA : l_wktime    TYPE i,
         l_st_date   TYPE sy-datum.

  CLEAR: l_st_date.

  l_wktime = p_day * 60 * 24.

  CALL FUNCTION 'Z_PP_GET_WKTIME'
    EXPORTING
      c_date                   = sy-datum
      opcode                   = '-'
      wktime                   = l_wktime
      werks                    = c_werks
      arbpl                    = c_arbpl
    IMPORTING
      t_date                   = l_st_date
    EXCEPTIONS
      error_operation          = 1
      cannot_read_working_time = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'Working time is not available.'.
  ENDIF.

  IF l_st_date IS INITIAL.
    l_st_date = sy-datum - p_day.
  ENDIF.

  CLEAR : s_date[], s_date.

  s_date-sign   = 'I'.
  s_date-option = 'BT'.
  s_date-low    = l_st_date.
  s_date-high   = sy-datum.
  APPEND s_date.

ENDFORM.                    " SET_DATE_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_CHARACTERISTIC
*&---------------------------------------------------------------------*
FORM get_characteristic .

  SELECT SINGLE atinn INTO w_vin_atinn
    FROM cabn WHERE atnam = 'P_VIN'.

  SELECT SINGLE atinn INTO w_model_atinn
    FROM cabn WHERE atnam = 'P_MODEL'.

  SELECT SINGLE atinn INTO w_219_1_atinn
    FROM cabn WHERE atnam = 'P_219_1'.

  SELECT SINGLE atinn INTO w_219_5_atinn
    FROM cabn WHERE atnam = 'P_219_5'.

  SELECT SINGLE atinn INTO w_ext_color_atinn
    FROM cabn WHERE atnam = 'P_EXT_COLOR'.

  SELECT SINGLE atinn INTO w_int_color_atinn
    FROM cabn WHERE atnam = 'P_INT_COLOR'.

  SELECT SINGLE atinn INTO w_219_30_atinn
    FROM cabn WHERE atnam = 'P_219_30'.

  SELECT SINGLE atinn INTO w_219_4_atinn
    FROM cabn WHERE atnam = 'P_219_4'.

  SELECT SINGLE atinn INTO w_rp06_dt_atinn
    FROM cabn WHERE atnam = 'P_RP06_SHOP_DATE'.

  SELECT SINGLE atinn INTO w_219_7_atinn
    FROM cabn WHERE atnam = 'P_219_7'.

  SELECT SINGLE atinn INTO w_219_131_atinn
    FROM cabn WHERE atnam = 'P_219_131'.

  SELECT SINGLE atinn INTO w_219_13_atinn
    FROM cabn WHERE atnam = 'P_219_13'.

  SELECT SINGLE atinn INTO w_219_184_atinn
    FROM cabn WHERE atnam = 'P_219_184'.

  SELECT SINGLE atinn INTO w_219_108_atinn
    FROM cabn WHERE atnam = 'P_219_108'.

  SELECT SINGLE atinn INTO w_air_11_atinn
    FROM cabn WHERE atnam = 'P_AIRBAG_NO11'.

  SELECT SINGLE atinn INTO w_dest_cd_atinn
    FROM cabn WHERE atnam = 'P_DESTINATION_CODE'.

  SELECT SINGLE atinn INTO w_219_14_atinn
    FROM cabn WHERE atnam = 'P_219_14'.

  SELECT SINGLE atinn INTO w_219_112_atinn
    FROM cabn WHERE atnam = 'P_219_112'.

  SELECT SINGLE atinn INTO w_219_31_atinn
    FROM cabn WHERE atnam = 'P_219_31'.

  SELECT SINGLE atinn INTO w_219_180_atinn
    FROM cabn WHERE atnam = 'P_219_180'.

  SELECT SINGLE atinn INTO w_219_28_atinn
    FROM cabn WHERE atnam = 'P_219_28'.

  SELECT SINGLE atinn INTO w_mi_atinn
    FROM cabn WHERE atnam = 'P_MI'.

  SELECT SINGLE atinn INTO w_ocn_atinn
    FROM cabn WHERE atnam = 'P_OCN'.
ENDFORM.                    " GET_CHARACTERISTIC
