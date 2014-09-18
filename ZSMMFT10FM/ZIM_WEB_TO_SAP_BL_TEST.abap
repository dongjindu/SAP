FUNCTION zim_web_to_sap_bl_test.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ZFDOCNO) LIKE  ZTFTZHD_LOG-ZFDOCNO
*"  TABLES
*"      IT_ZSBLHD STRUCTURE  ZSBLHD_INF
*"      IT_ZSBLIT STRUCTURE  ZSBLIT_INF
* Date         Developer     Request       Description
* 01/24/2007   Manju         UD1K930445    Update status of old records
*                                          with different status for
*                                          same House BOL so that only
*                                          latest record is displayed
*                                          for processing.
*"----------------------------------------------------------------------
  DATA: lv_zresult LIKE zsca_if_time_stamp_out-zresult.
  DATA: lv_message TYPE bapi_msg. "Message text (220)
  DATA: w_success  TYPE i,
        w_fail     TYPE i.

  SELECT SINGLE * FROM ztimimg00.
  IF ztimimg00-zfbdcyn EQ 'X'.
    w_mode  =  'E'.
  ELSE.
    w_mode  =  'N'.
  ENDIF.

  REFRESH : it_temp_civ, it_temp_blit, it_zsblhd, it_zsblit,
            it_zsblhd_log,it_zsblit_log.
  CLEAR : it_zsblhd, it_zsblit,w_message.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zsblhd
  FROM   ztblhd_inf
  WHERE  zfblno     EQ   zfdocno.

  IF sy-subrc = 0.
    LOOP AT it_zsblhd.
      IF it_zsblhd-zfsprtc = 'CAPRR'.
        it_zsblhd-zfsprtc = 'USCHI'.
        MODIFY it_zsblhd.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zsblit
  FROM   ztblit_inf
  WHERE  zfblno     EQ   zfdocno.

  DESCRIBE TABLE it_zsblhd LINES w_line.
  IF w_line GT 0.
    lv_zresult  =  'S'.
  ELSE.
    lv_zresult  =  'E'.
    w_message   =  'No Data Uplaod!'.
  ENDIF.

* HEADER DATA LOOP => Multi B/L Create.
  LOOP AT it_zsblhd.

    w_tabix  =  sy-tabix.

    CLEAR : w_error_flag.

*   Making BDC Data.
    PERFORM p2000_make_bdc_data TABLES it_zsblhd it_zsblit.
    IF w_error_flag IS INITIAL.
*>> BDC CALL.
      REFRESH : messtab. CLEAR : messtab.

      SET PARAMETER ID 'ZPBLNO' FIELD ''.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.

      CALL TRANSACTION 'ZIM21'  USING       bdcdata
                                MODE        w_mode
                                UPDATE      'V'
                                MESSAGES    INTO   messtab.

      w_subrc = sy-subrc.

    ENDIF.

    IF w_subrc EQ 0 AND w_error_flag IS INITIAL.

      GET PARAMETER ID 'ZPBLNO'  FIELD w_zpblno.
      GET PARAMETER ID 'ZPHBLNO' FIELD w_zphblno.

      MOVE : w_zpblno   TO  it_temp_civ-zfblno,
             w_zphblno  TO  it_temp_civ-zfhblno.
      APPEND it_temp_civ.

      PERFORM  p3000_container_insert  USING  w_zpblno.

      w_success  =  w_success  +  1.
      lv_zresult =  'S'.
    ELSE.

* BEGIN OF HIS20094 - Get error message from called transaction
      READ TABLE messtab WITH KEY msgtyp = 'E'.
      IF sy-subrc EQ 0.
        MESSAGE ID messtab-msgid
              TYPE messtab-msgtyp
            NUMBER messtab-msgnr
              WITH messtab-msgv1
                   messtab-msgv2
                   messtab-msgv3
                   messtab-msgv4
         INTO w_message.
      ENDIF.
* END OF HIS20094

      IF w_message IS INITIAL.
        w_message  =  'B/L Creation Fail(BDC)!'.
      ENDIF.
      w_fail     =  w_fail     +  1.
      lv_zresult =  'E'.
    ENDIF.

    ">> Header Table Error Log Write.
    MOVE-CORRESPONDING it_zsblhd TO  it_zsblhd_log.
    MOVE : sy-uname              TO  it_zsblhd_log-zuser,
           sy-datum              TO  it_zsblhd_log-zedat,
           sy-uzeit              TO  it_zsblhd_log-zetim,
           'C'                   TO  it_zsblhd_log-zmode,
           lv_zresult            TO  it_zsblhd_log-zresult,
           w_message             TO  it_zsblhd_log-zmsg .
    PERFORM   get_number_next  USING  'TB'  it_zsblhd_log-zfblno.
    APPEND it_zsblhd_log.

    ">> Item Table Error Log Write.
    LOOP AT it_zsblit  WHERE  zfhblno  EQ  it_zsblhd-zfhblno.

      MOVE-CORRESPONDING it_zsblit TO  it_zsblit_log.
      MOVE : it_zsblhd_log-zfblno  TO  it_zsblit_log-zfblno,
             sy-tabix              TO  it_zsblit_log-zfseq,
             sy-uname              TO  it_zsblit_log-zuser,
             sy-datum              TO  it_zsblit_log-zedat,
             sy-uzeit              TO  it_zsblit_log-zetim,
             'C'                   TO  it_zsblit_log-zmode,
             lv_zresult            TO  it_zsblit_log-zresult,
             lv_message            TO  it_zsblit_log-zmsg .
      APPEND it_zsblit_log.
    ENDLOOP.
  ENDLOOP.

  INSERT ztblhd_inf FROM TABLE it_zsblhd_log.
  INSERT ztblit_inf FROM TABLE it_zsblit_log.



*>> EAI Interface Log Write.
  PERFORM  z_fca_eai_interface_log USING 'ZIM21'    w_line
                                         w_success  w_fail.


*>> Commercial Invoice BDC Data Making.
  PERFORM p2000_make_civ_data TABLES it_zsblit.

  " BDC Data Make & Call
  PERFORM p2000_civ_bdc_data.

  COMMIT WORK.

* Begin of changes - UD1K930445
* UPDATE status in LOG TABLE for all the old records with different
*status so that it will not be picked up for further processing for
* House BOL Number

  UPDATE ztblhd_inf SET zresult = 'I'  WHERE
                                 zfhblno =  it_zsblhd-zfhblno AND
                                 zfmblno =  it_zsblhd-zfmblno AND
                                 zfblno  NE it_zsblhd_log-zfblno.

  UPDATE ztblit_inf SET zresult = 'I'  WHERE
                                 zfhblno =  it_zsblhd-zfhblno AND
*                                 ZFMBLNO =  IT_ZSBLHD-ZFMBLNO and
                                 zfblno  NE it_zsblhd_log-zfblno.

  COMMIT WORK.
* End of changes - UD1K930445

ENDFUNCTION.
