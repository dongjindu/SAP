FUNCTION z_fpp_workorder_free_parallel.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(LT_DATA) LIKE  ZSPP_WO_CONF STRUCTURE  ZSPP_WO_CONF
*"     VALUE(LP_MODEL) LIKE  ZTPP_SPEC-MODEL
*"     VALUE(LP_R) LIKE  ZTBM_219_VALUE-VALUE
*"  TABLES
*"      ET_DATA STRUCTURE  ZSPP_WO_CONF
*"      ET_ALC_NAME STRUCTURE  CABN
*"----------------------------------------------------------------------
  CLEAR: it_alc, it_alc[].  CLEAR: it_char, it_char[],et_data[].
  RANGES: l_alc_name FOR cabn-atnam.
  IF lp_r IS INITIAL.
  ELSE.
    LOOP AT et_alc_name.
      l_alc_name-sign = 'I'.
      l_alc_name-option = 'EQ'.
      l_alc_name-low = et_alc_name-atnam.
      APPEND l_alc_name.
    ENDLOOP.
  ENDIF.
  p_model = lp_model.
*wo_color
  PERFORM find_wo_color TABLES it_matnr
                        USING  lt_data-matnr.
  LOOP AT it_matnr.
    CLEAR: l_freez, it_char, it_char[].
    wa_color = it_matnr-matnr .
** remarked by Furong on 08/07/2006
*    PERFORM bdc_mm02 USING wa_color l_freez.
*    IF l_freez = 'X'.  CONTINUE.  ENDIF.
** end of change
    REFRESH it_conf. CLEAR it_conf .
    REFRESH it_char. CLEAR it_char .
    PERFORM find_configure_variant TABLES it_conf
                                   USING it_matnr-cuobf.
    LOOP AT it_conf.
** added by Furong on 09/27/06
      IF lp_r IS INITIAL.
        MOVE-CORRESPONDING it_conf TO it_char.
        APPEND it_char. CLEAR it_char.
      ELSE.
        IF it_conf-atnam = 'P_MOD_DATE'.
          MOVE-CORRESPONDING it_conf TO it_char.
          APPEND it_char. CLEAR it_char.
        ELSE.
          IF it_conf-atnam = 'P_PERF_YN'.
            MOVE-CORRESPONDING it_conf TO it_char.
            APPEND it_char. CLEAR it_char.
          ELSE.
            IF it_conf-atnam IN l_alc_name.
              MOVE-CORRESPONDING it_conf TO it_char.
              APPEND it_char. CLEAR it_char.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
** end of addition

    ENDLOOP.
    READ TABLE it_char WITH KEY atnam = 'P_MOD_DATE'.
    IF sy-subrc = 0.
      l_tabix   = sy-tabix.
      it_char-atwrt = sy-datum.
      MODIFY it_char INDEX l_tabix.
    ELSE.
      it_char-atnam = 'P_MOD_DATE'.  it_char-atwrt = sy-datum.
      APPEND it_char.
    ENDIF.

    IF lp_r IS INITIAL.
      PERFORM check_alcerror  USING 'C'  .
      DESCRIBE TABLE it_alc LINES l_cnt  .
      READ TABLE it_char WITH KEY atnam = 'P_PERF_YN' .
      IF sy-subrc = 0.
        l_tabix = sy-tabix.
        l_exist = 'X'.
      ELSE.
        CLEAR: l_exist.
      ENDIF.
      CLEAR: it_char.

      IF l_cnt > 0 .
        l_perf = lt_data-perf  = 'N'.
        LOOP AT it_alc .
          CONCATENATE 'P_' it_alc-knnam+6(9) INTO it_char-atnam .
          it_char-atwrt = '????' .    APPEND it_char  .
        ENDLOOP.
        it_char-atnam = 'P_PERF_YN'.   it_char-atwrt = 'N' .
      ELSE.
        it_char-atnam = 'P_PERF_YN'.   it_char-atwrt = 'Y' .
        lt_data-perf  = 'Y'.
      ENDIF.
      READ TABLE it_char WITH KEY atnam = 'P_PERF_YN' .
      IF l_exist = 'X'.
        MODIFY it_char INDEX l_tabix TRANSPORTING atwrt .
      ELSE.
        APPEND it_char.
      ENDIF.
      DELETE it_char WHERE atnam = 'P_HPC_STATUS'.
    ENDIF.
    PERFORM ftp_handling_master TABLES it_char
                                USING it_matnr-matnr .
  ENDLOOP.
*wo_head
  CLEAR: it_alc, it_alc[].  CLEAR: it_char, it_char[], l_freez.
  CLEAR: it_conf, it_conf[].
  wa_order = lt_data-matnr .
*  PERFORM bdc_mm02 USING wa_order  l_freez.
*  IF l_freez = 'X'.  EXIT.  ENDIF.

  PERFORM find_cuobf_field USING lt_data-matnr  CHANGING wa_cuobf.
  PERFORM find_configure_variant TABLES it_conf USING wa_cuobf.

  IF lp_r IS INITIAL.
    " Make the P_VIN_SPEC Characteristics...
    PERFORM make_vinspec     .

    " Elemenate the P_VIN Characteristics...
    DELETE it_conf WHERE atnam = 'P_VIN_123'.
    DELETE it_conf WHERE atnam = 'P_VIN_4'  .
    DELETE it_conf WHERE atnam = 'P_VIN_5'  .
    DELETE it_conf WHERE atnam = 'P_VIN_6'  .
    DELETE it_conf WHERE atnam = 'P_VIN_7'  .
    DELETE it_conf WHERE atnam = 'P_VIN_8'  .
    DELETE it_conf WHERE atnam = 'P_VIN_9'  .
    DELETE it_conf WHERE atnam = 'P_VIN_10' .
    DELETE it_conf WHERE atnam = 'P_VIN_11' .
  ENDIF.

*Issue #    ,Requested by BWPARK
*Changed by wskim, ON 01/21/2005
*-----Start
  DATA : t_name(15),t_num(3).
  CLEAR :t_name,t_num.
  LOOP AT it_conf.
*      MOVE-CORRESPONDING it_conf TO it_char.
*      APPEND it_char. CLEAR it_char.
    IF lp_r IS INITIAL.
      SEARCH it_conf-atnam FOR 'P_TECH_SPEC'.
      IF sy-subrc <> 0.
        MOVE-CORRESPONDING it_conf TO it_char.
        APPEND it_char. CLEAR it_char.
      ELSE.
        MOVE it_conf-atnam+12(3) TO t_num.
        IF t_num < '006'.
          MOVE-CORRESPONDING it_conf TO it_char.
          APPEND it_char. CLEAR it_char.
        ENDIF.
      ENDIF.
      CLEAR t_num.
    ELSE.
      IF it_conf-atnam IN l_alc_name.
        MOVE-CORRESPONDING it_conf TO it_char.
        APPEND it_char. CLEAR it_char.
      ENDIF.
    ENDIF.
*-----End
  ENDLOOP.

  READ TABLE it_char WITH KEY atnam = 'P_MOD_DATE'.
  IF sy-subrc = 0.
    l_tabix   = sy-tabix.
    it_char-atwrt = sy-datum.
    MODIFY it_char INDEX l_tabix.
  ELSE.
    it_char-atnam = 'P_MOD_DATE'.  it_char-atwrt = sy-datum.
    APPEND it_char.
  ENDIF.
  IF lp_r IS INITIAL.
    PERFORM check_alcerror  USING 'U'  .
    DESCRIBE TABLE it_alc LINES l_cnt  .
    READ TABLE it_char WITH KEY atnam = 'P_PERF_YN' .
    IF sy-subrc = 0.
      l_tabix = sy-tabix.
      l_exist = 'X'.
    ELSE.
      CLEAR: l_exist.
    ENDIF.
    CLEAR: it_char.
    IF l_cnt > 0 .
      lt_data-perf  = 'N'.
      LOOP AT it_alc .
        CONCATENATE 'P_' it_alc-knnam+6(9) INTO it_char-atnam.
        it_char-atwrt = '????' .   APPEND it_char  .
      ENDLOOP.
      it_char-atnam = 'P_PERF_YN'.   it_char-atwrt = 'N' .
    ELSE.
      IF l_perf = 'N'.
        it_char-atnam = 'P_PERF_YN'.   it_char-atwrt = 'N' .
      ELSE.
        it_char-atnam = 'P_PERF_YN'.   it_char-atwrt = 'Y' .
        lt_data-perf  = 'Y'.
      ENDIF.
    ENDIF.
    IF l_exist = 'X'.
      MODIFY it_char INDEX l_tabix TRANSPORTING atwrt .
    ELSE.
      APPEND it_char.
    ENDIF.
    DELETE it_char WHERE atnam = 'P_PROD_FLAG' .
    DELETE it_char WHERE atnam = 'P_HPC_STATUS'.
  ENDIF.
  PERFORM ftp_handling_master TABLES it_char
                              USING lt_data-matnr.

  MOVE-CORRESPONDING lt_data TO et_data.
  APPEND et_data.
  CLEAR : l_perf.
ENDFUNCTION.
