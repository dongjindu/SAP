************************************************************************
*
** Program Name   : ZEMMPM34R_ST_PRICE_MANAGE
** Created by     : Min-su Park
** Created on     : 2003.10.16.
** Pattern        :
** Description    :  Manage Standard Price for Purchase Material
**
** Modification Logs
** Date            Developer        RequestNo      Description
** 2003.10.17.     Min-su Park    UD1K901873     Initial Coding
*
**&--<<<<<<-------------------------------------------------------------
*
** Date            Developer        RequestNo      Description
** 2004.02.04.     Jaesung Lee    UD1K906915     Changed condition logic
** Condition logic changed: plant leavel =>  Purchasing group
**&--<<<<<<-------------------------------------------------------------
*
************************************************************************
*
**----------------------------------------------------------------------
*
**   INCLUDE ZRACO99_STD_PRICE_MANAGE_F01
*
**----------------------------------------------------------------------
*
**&---------------------------------------------------------------------
*
**&      Form  BUSINESS_PLAN
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM business_plan.
**Get Material BOM on business plan
** [ 1 ] Get Material Whose Bom Usage Number is 6.
*  PERFORM get_mat_bomusg_6.
** [ 2 ] Multi Level Explosion BOM
*  PERFORM bom_explosion.
** [ 3 ] Processing by material on KD/LD
** This Logic exist in Period Standard
*  LOOP AT it_mara.
*    CASE it_mara-profl.
*      WHEN 'K'.     " KD
*        PERFORM standard_price_kd USING it_mara.
*      WHEN 'V'.     " LP
*        PERFORM standard_price_lp USING it_mara.
*    ENDCASE.
*  ENDLOOP.
*ENDFORM.                    " BUSINESS_PLAN
**&---------------------------------------------------------------------
*
**&      Form  PERIOD_STANDARD
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM period_standard.
**Get material of material type 'ROH', 'ROH1'
**and KD/LD 'K', 'V'.
*  SELECT * FROM mara
*           INTO CORRESPONDING FIELDS OF TABLE it_mara
*          WHERE matnr IN s_matnr
*            AND mtart IN ('ROH', 'ROH1')
*            AND profl IN ('K', 'V')
*            AND lvorm  <> 'X'.
*
**Processing by material on KD/LD
*  LOOP AT it_mara.
*    CASE it_mara-profl.
*      WHEN 'K'.
*        PERFORM standard_price_kd USING it_mara.
*      WHEN 'V'.
*        PERFORM standard_price_lp USING it_mara.
*    ENDCASE.
*  ENDLOOP.
*ENDFORM.                    " PERIOD_STANDARD
**&---------------------------------------------------------------------
*
**&      Form  STANDARD_PRICE_KD
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM standard_price_kd USING it_mara STRUCTURE mara.
*  DATA : subrc LIKE sy-subrc.
*
** [ 0 ]Get Info Record
*  SELECT * FROM zvmm_inforecord     " EINA+EINE+MAKT+LFA1+MARA
*           INTO CORRESPONDING FIELDS OF TABLE it_info
*          WHERE matnr = it_mara-matnr
*            AND werks <> space.
*
*  CASE sy-subrc.
*    WHEN 0.
** [ 1 ].Search most recently condition date by plant material from
*table
**       A017.
*      SORT it_info BY werks.
*      LOOP AT it_info.
*        AT END OF werks.
**  [ 1 ]-1. Read Condition for one material and plant.
*          PERFORM get_condition_data USING it_mara.
**  [ 1 ]-2. Delete Record concerned with Deletion inforecord.
*          PERFORM filter_condition_data USING it_mara
*                                        CHANGING subrc.
*          CHECK subrc = 0.
**  [ 1 ]-3. Then Find most recently value by sorting A017 descendingly
**           and by Reading index 1.
**           a.Here We get most recently Condition about one material
**             Through one Plant.
**           b.A017 data gotton is important information whose inform
**             can be used for unique most recently inforecord and
**             unique most recently Condition.
*          SORT it_a017 BY datab DESCENDING.
*          CLEAR it_a017.
*          READ TABLE it_a017 INDEX 1.
**  [ 1 ]-4. Calcu Standard Price.
*          PERFORM calcu_standard_price USING it_a017.
**  [ 1 ]-5. Master update.
*          PERFORM update_master_data USING it_mara.
*        ENDAT.
*      ENDLOOP.
*      CLEAR : it_info, it_a017.
*    WHEN OTHERS.
*      it_error-ekorg = it_a017-ekorg.
*      it_error-werks = it_info-werks.
*      it_error-matnr = it_mara-matnr.
*      it_error-profl = it_mara-profl.
*      it_error-etype = 'Z'.
*      it_error-waers = 'USD'.
*      it_error-base_d = p_date.
*      APPEND it_error.
*  ENDCASE.
*ENDFORM.                    " DETERMINE_STANDARD_PRICE
**&---------------------------------------------------------------------
*
**&      Form  GET_CONDITION_DATA
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM get_condition_data USING it_mara STRUCTURE mara.
**&<<<<<<<<<<<<<<<<<<<--------------------------------------------------
*
** insert by jslee 04/02/2004
**&<<<<<<<<<<<<<<<<<<<--------------------------------------------------
*
*
*  SELECT * FROM a018
*           INTO CORRESPONDING FIELDS OF TABLE it_a017
*          WHERE matnr = it_mara-matnr
**            AND werks = it_info-werks
*            AND datab <= p_date.     " Validity start date
*  " <= Start Date
**&<<<<<<<<<<<<<<<<<<<--------------------------------------------------
*
** end of insert
**&<<<<<<<<<<<<<<<<<<<--------------------------------------------------
*
**&<<<<<<<<<<<<<<<<<<<--------------------------------------------------
*
** delete by jslee 04/02/2004
**&<<<<<<<<<<<<<<<<<<<--------------------------------------------------
*
*
**  SELECT * FROM a017
**           INTO CORRESPONDING FIELDS OF TABLE it_a017
**          WHERE matnr = it_mara-matnr
**            AND werks = it_info-werks
**            AND datab <= p_date.     " Validity start date
**  " <= Start Date
**
**&<<<<<<<<<<<<<<<<<<<--------------------------------------------------
*
** end of delete
**&<<<<<<<<<<<<<<<<<<<--------------------------------------------------
*
*
*  CLEAR w_stawn.
*
*  SELECT SINGLE stawn  " Com. code/Import code number for foreign trade
*           INTO w_stawn
*           FROM marc
*          WHERE werks = it_info-werks
*            AND matnr = it_mara-matnr.
*
*ENDFORM.                    " GET_CONDITION_DATA
**&---------------------------------------------------------------------
*
**&      Form  FILTER_CONDITION_DATA
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM filter_condition_data  USING it_mara STRUCTURE mara
*                            CHANGING subrc.
*  DATA : info LIKE zvmm_inforecord,
*         cnt  TYPE i              .
*
*  LOOP AT it_a017.
*    SELECT SINGLE * FROM zvmm_inforecord
*                    INTO info
*                   WHERE lifnr = it_a017-lifnr
*                     AND matnr = it_a017-matnr
*                     AND ekorg = it_a017-ekorg
*                     AND werks = it_a017-werks
*                     AND esokz = '0'.     " Standard Price
*    IF sy-subrc <> 0.
*      DELETE it_a017.
*    ENDIF.
*  ENDLOOP.
*
*  DESCRIBE TABLE it_a017 LINES cnt.
*
*  IF cnt = 0.
*    subrc = 4.
** KD  -  Get Error Data(mspark)
*    it_error-ekorg  = it_a017-ekorg.
*    it_error-werks  = it_info-werks.
*    it_error-matnr  = it_mara-matnr.
*    it_error-profl  = it_mara-profl.
*    it_error-etype  = 'Z'.
*    it_error-waers  = 'USD'.
*    it_error-base_d = p_date.
*    APPEND it_error.
*  ELSE.
*    subrc = 0.
*  ENDIF.
*ENDFORM.                    " FILTER_CONDITION_DATA
**&---------------------------------------------------------------------
*
**&      Form  CALCU_STANDARD_PRICE
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM calcu_standard_price USING p_a017 STRUCTURE a017.
*  DATA : BEGIN OF it_konp OCCURS 0.
*          INCLUDE STRUCTURE konp.
*  DATA : END OF it_konp.
*  DATA : tmp LIKE konp-kbetr,
*         message(40)        .
*  TABLES : tcurr.
*
*  SELECT * FROM konp
*           INTO CORRESPONDING FIELDS OF TABLE it_konp
*          WHERE knumh    =  p_a017-knumh
*            AND loevm_ko <> 'X'.   " Deletion Indicator
*  IF sy-subrc = 0.
*    READ TABLE it_konp WITH KEY kschl = 'PB00'.
*    CHECK sy-subrc = 0.
** [ 1 ] Currency conversion from foreign curr to 'USD'.
*    SELECT SINGLE *
*             FROM tcurr
*            WHERE kurst = 'P'
*              AND fcurr = it_konp-konwa     " From Currency
*              AND tcurr = 'USD'.            " To Currency
**              and GDATU eq ?.
*    CASE sy-subrc.
*      WHEN 0.
*        IF tcurr-ukurs > 0.
*          w_effpr = it_konp-kbetr * tcurr-ukurs.
*        ELSE.
*          tcurr-ukurs = tcurr-ukurs * -1.
*          w_effpr = it_konp-kbetr / tcurr-ukurs.
*        ENDIF.
*      WHEN OTHERS.
*        IF it_konp-konwa <> 'USD'.
*          CONCATENATE 'There is no Currency between' it_konp-konwa
*                      'And USD' INTO message.
*          it_log-serial  = it_log-serial + 1.
*          it_log-matnr   = it_mara-matnr.
*          it_log-tcode   = sy-tcode.
*          it_log-type    = 'E'.     "Message type
*          it_log-id      = 'ZMMM'.  "Messages, Message class
*          it_log-numb    = '026' .  "Message Number
*          it_log-message = message. "Message txt
*          APPEND it_log.
*        ENDIF.
*    ENDCASE.
** [ 2 ] Calcu Standard Price PB00 + FRA1 + ZOTH ..etc.
*    LOOP AT it_konp WHERE kschl <> 'PB00'.
**--- 2004/01/22 block by stlim - start
*      CASE it_konp-konwa.
**--- end
**--- 2004/01/22 insert by stlim - start
**      CASE it_konp-kschl.
**--- end
*        WHEN '%'.
*          tmp   =  w_effpr * it_konp-kbetr / 1000.
*          w_effpr =  w_effpr + tmp.
*        WHEN 'ZOA1'.
*          tmp   =  w_effpr * it_konp-kbetr / 1000.
*          w_effpr =  w_effpr + tmp.
*        WHEN OTHERS.
*          w_effpr =  w_effpr + it_konp-kbetr.
*      ENDCASE.
*    ENDLOOP.
*  ENDIF.
*ENDFORM.                    " CALCU_STANDARD_PRICE
**&---------------------------------------------------------------------
*
**&      Form  STANDARD_PRICE_LP
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_IT_MARA  text
**----------------------------------------------------------------------
*
*FORM standard_price_lp USING it_mara STRUCTURE mara.
*  DATA : subrc LIKE sy-subrc.
*
** [ 0 ]Get Info Record
*  SELECT * FROM zvmm_inforecord
*           INTO CORRESPONDING FIELDS OF TABLE it_info
*          WHERE matnr = it_mara-matnr
*            AND werks <> space.
*
*  CASE sy-subrc.
*    WHEN 0.
** [ 1 ].Search most recently condition date by plant material from
*table
**       A017.
*      SORT it_info BY werks.
*      LOOP AT it_info.
*        AT END OF werks.
**  [ 1 ]-1. Read Condition for one material and plant.
*          PERFORM get_condition_data USING it_mara.
**  [ 1 ]-2. Delete Record concerned with Deletion inforecord.
*          PERFORM filter_condition_data USING it_mara
*                                        CHANGING subrc.
*          CHECK subrc = 0.
**  [ 1 ]-3. Check reason code, zero netpr.
*          PERFORM check_reason_code CHANGING subrc.
*
*          CASE subrc.
*            WHEN 0.
**  Get condition most lower price when datab(valid from) is same.
*              PERFORM get_real_effpr.
**  [ 1 ]-4-1. Master update(When [ 1 ]-3 check is success: netpr exist)
*              PERFORM update_master_data USING it_mara.
*            WHEN OTHERS.
**   Get condition if future condition exist.
*              CHECK sy-tcode = 'ZMME21'.
**       [1]-1'
*              PERFORM get_future_condition_data USING it_mara.
**       [1]-2'
*              PERFORM future_filter_condition_data USING it_mara.
**       [1]-3'
*              PERFORM future_check_reason_code CHANGING subrc.
*              CHECK subrc = 0.
*              PERFORM update_master_data USING it_mara.
**  [ 1 ]-4-2. Error Display(When [ 1 ]-3 check is Error : netpr no
**     exist)
**          PERFORM ERROR_DISPLAY.
*          ENDCASE.
*        ENDAT.
*      ENDLOOP.
*      CLEAR : it_info, it_a017.
*    WHEN OTHERS.
*      it_error-ekorg = it_a017-ekorg.
*      it_error-werks = it_info-werks.
*      it_error-matnr = it_mara-matnr.
*      it_error-profl = it_mara-profl.
*      it_error-etype = 'Z'.
*      it_error-base_d = p_date.
*      it_error-waers = 'USD'.
*      APPEND it_error.
*  ENDCASE.
*ENDFORM.                    " STANDARD_PRICE_LP
**&---------------------------------------------------------------------
*
**&      Form  CHECK_REASON_CODE
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      <--P_SUBRC  text
**----------------------------------------------------------------------
*
*FORM check_reason_code CHANGING p_subrc.
*  DATA : kzust LIKE konh-kzust.
*  DATA : wa_a017 LIKE it_a017.
*
*  SORT it_a017 BY datab DESCENDING.
*
*  p_subrc = 5.
*
*  LOOP AT it_a017.
*    IF sy-tabix = 1.
*      MOVE-CORRESPONDING it_a017 TO wa_a017.
*    ENDIF.
*    SELECT SINGLE kzust     " Reason Code
*             INTO kzust
*             FROM konh
*            WHERE knumh = it_a017-knumh.
*
*    CHECK sy-subrc = 0.
*
*    IF kzust+0(1) <> 'X'.     " responsibility of info. record
*      SELECT SINGLE kbetr
*               INTO w_effpr     " Effective Price
*               FROM konp
*              WHERE knumh = it_a017-knumh
*                AND kschl = 'PB00'.
*      p_subrc = 0.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*
*  CHECK p_subrc <> 0 AND sy-tcode = 'ZMME22'.     " New Material
*
** Get Error Data(mspark)
**  READ TABLE IT_A017 INDEX 1.
*  SELECT SINGLE kbetr konwa
*           INTO (it_error-tnetpr, it_error-waers)
*           FROM konp
*          WHERE knumh = wa_a017-knumh
*            AND kschl = 'PB00'.
*  SELECT SINGLE kzust     " Reason Code
*           INTO it_error-kzust
*           FROM konh
*          WHERE knumh = wa_a017-knumh.
*
*  it_error-ekorg   = wa_a017-ekorg.
*  it_error-werks   = wa_a017-werks.
*  it_error-matnr   = it_mara-matnr.
*  it_error-profl   = it_mara-profl.
*  it_error-etype   = 'T'.
*  it_error-base_d  = p_date.
*  it_error-valid_d = wa_a017-datab.
*  APPEND it_error.
*ENDFORM.                    " CHECK_REASON_CODE
**&---------------------------------------------------------------------
*
**&      Form  ERROR_DISPLAY
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM error_display.
*
*ENDFORM.                    " ERROR_DISPLAY
**&---------------------------------------------------------------------
*
**&      Form  GET_MAT_BOMUSG_6
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM get_mat_bomusg_6.
*  SELECT * FROM zvmm_mat_master
*           INTO CORRESPONDING FIELDS OF TABLE it_bplan
*          WHERE matnr = s_matnr
*            AND stlan = '6'     " Costing
*            AND lvorm <> 'X'.
*ENDFORM.                    " GET_MAT_BOMUSG_6
**&---------------------------------------------------------------------
*
**&      Form  BOM_EXPLOSION
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM bom_explosion.
*  TABLES : t159l.
*
*  DATA   : tmp_stpox LIKE TABLE OF stpox WITH HEADER LINE,
*           emeng     TYPE weemg VALUE 10.
*
*  CLEAR   it_mara.
*  REFRESH it_mara.
*
*  LOOP AT it_bplan.
*    SELECT SINGLE * FROM t159l
*                   WHERE werks = it_bplan-werks.
*    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
*         EXPORTING
*              capid = t159l-capid  " Application
*              datuv = sy-datum
*              emeng = emeng
*              mehrs = 'X'
*              mtnrv = it_bplan-matnr
*              sanfr = 'X'
*              stlan = t159l-stlan
*              werks = it_bplan-werks
*         TABLES
*              stb   = tmp_stpox.
*    LOOP AT tmp_stpox.
*      READ TABLE it_stpox WITH KEY idnrk = tmp_stpox-idnrk.
*      " BOM Component
*      IF sy-subrc = 0.
*      ELSE.
*        MOVE-CORRESPONDING tmp_stpox TO it_stpox.
*        APPEND it_stpox.
*        SELECT SINGLE * INTO it_mara
*                        FROM mara
*                       WHERE matnr = it_stpox-idnrk.
*        IF sy-subrc = 0.
*          APPEND it_mara.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
*ENDFORM.                    " BOM_EXPLOSION
**&---------------------------------------------------------------------
*
**&      Form  BDC_PASS
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_0010   text
**      -->P_0011   text
**      -->P_0012   text
**----------------------------------------------------------------------
*
*FORM bdc_pass USING par1 par2 par3.
*  CLEAR it_bdc.
*  IF par1 = 'X'.
*    it_bdc-dynbegin = 'X'.
*    it_bdc-program  = par2.
*    it_bdc-dynpro   = par3.
*    APPEND it_bdc.
*  ELSE.
*    it_bdc-fnam = par2.
*    it_bdc-fval = par3.
*    APPEND it_bdc.
*  ENDIF.
*ENDFORM.                    " BDC_PASS
**&---------------------------------------------------------------------
*
**&      Form  SAVE
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM save.
*  LOOP AT it_error.
*    PERFORM update_mat_price USING it_error-matnr
*                                   it_error-werks
*                                   it_error-effpr.
*  ENDLOOP.
*ENDFORM.                    " SAVE
**&---------------------------------------------------------------------
*
**&      Form  UPDATE_MAT_PRICE
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_IT_ERROR_MATNR  text
**      -->P_IT_ERROR_WERKS  text
**      -->P_IT_ERROR_EFFPR  text
**----------------------------------------------------------------------
*
*FORM update_mat_price USING    p_matnr
*                               p_werks
*                               p_effpr.
*  DATA : status VALUE 'N'.
*  DATA : effpr1(14), fdate(10).
*  WRITE : p_effpr TO effpr1 CURRENCY 'USD',
*          sy-datum TO fdate               .
*  CLEAR   : it_bdc, it_message.
*  REFRESH : it_bdc, it_message.
*
*  PERFORM bdc_pass USING:
*       'X' 'SAPLMGMM'     '0060'       ,
*       ' ' 'RMMG1-MATNR'  p_matnr,
*       ' ' 'BDC_OKCODE'   '=AUSW'      .
*
*  PERFORM bdc_pass USING:
*       'X' 'SAPLMGMM'     '0070'       ,
*       ' ' 'MSICHTAUSW-KZSEL(18)' 'X'  ,
*       ' ' 'BDC_OKCODE'   '=ENTR'      .
*
*  PERFORM bdc_pass USING:
*       'X' 'SAPLMGMM'     '0080'       ,
*       ' ' 'RMMG1-WERKS'  p_werks,
*       ' ' 'BDC_OKCODE'   '=ENTR'      .
*
*  PERFORM bdc_pass USING:
*       'X' 'SAPLMGMM'     '4000'       ,
*       ' ' 'MBEW-ZPLP1'   effpr1       ,
*       ' ' 'MBEW-ZPLD1'   fdate        ,
*       ' ' 'BDC_OKCODE'   '=BU'        .
*
*  CALL TRANSACTION 'MM02'
*           USING it_bdc
*           MODE status
*           UPDATE'S'
*           MESSAGES INTO it_message.
*ENDFORM.                    " UPDATE_MAT_PRICE
**&---------------------------------------------------------------------
*
**&      Form  CHK_FUTURE_CONDITION_DATA
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM get_future_condition_data USING it_mara STRUCTURE mara.
**Get future condition data.
**&<<<<-----------------------------------------------------------------
*
** delete by jaesung lee on 04/02/2004
**&<<<<-----------------------------------------------------------------
*
*
**  SELECT * FROM a017
**           INTO CORRESPONDING FIELDS OF TABLE it_a017
**          WHERE matnr = it_mara-matnr
**            AND werks = it_info-werks
**            AND datab >= p_date.
**&<<<<-----------------------------------------------------------------
*
**  end of delete on 04/02/2004
**&<<<<-----------------------------------------------------------------
*
*
**&<<<<-----------------------------------------------------------------
*
** insert by jaesung lee on 04/02/2004
**&<<<<-----------------------------------------------------------------
*
*  SELECT * FROM a018
*           INTO CORRESPONDING FIELDS OF TABLE it_a017
*          WHERE matnr = it_mara-matnr
**            AND werks = it_info-werks
*            AND datab >= p_date.
*
**&<<<<-----------------------------------------------------------------
*
** end of insert.
**&<<<<-----------------------------------------------------------------
*
*
*ENDFORM.                    " CHK_FUTURE_CONDITION_DATA
**&---------------------------------------------------------------------
*
**&      Form  FUTURE_FILTER_CONDITION_DATA
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_IT_MARA  text
**      <--P_SUBRC  text
**----------------------------------------------------------------------
*
*FORM future_filter_condition_data USING it_mara STRUCTURE mara.
*
*  DATA : info LIKE zvmm_inforecord,
*         cnt  TYPE i              .
*
*  LOOP AT it_a017.
*    SELECT SINGLE * FROM zvmm_inforecord
*                    INTO info
*                   WHERE lifnr = it_a017-lifnr
*                     AND matnr = it_a017-matnr
*                     AND ekorg = it_a017-ekorg
*                     AND werks = it_a017-werks
*                     AND esokz = '0'.
*    IF sy-subrc <> 0.
*      DELETE it_a017.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " FUTURE_FILTER_CONDITION_DATA
**&---------------------------------------------------------------------
*
**&      Form  FUTURE_CHECK_REASON_CODE
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM future_check_reason_code CHANGING p_subrc.
*  DATA : kzust LIKE konh-kzust.
*  DATA : wa_a017 LIKE it_a017.
*
*  SORT it_a017 BY datab ASCENDING.
*
*  p_subrc = 5.
*
*  LOOP AT it_a017.
*    IF sy-tabix = 1.
*      MOVE-CORRESPONDING it_a017 TO wa_a017.
*    ENDIF.
*    SELECT SINGLE kzust
*             INTO kzust
*             FROM konh
*            WHERE knumh = it_a017-knumh.
*    CHECK sy-subrc = 0.
*    IF kzust+0(1) <> 'X'.
*      SELECT SINGLE kbetr
*               INTO w_effpr
*               FROM konp
*              WHERE knumh = it_a017-knumh
*                AND kschl = 'PB00'.
*      p_subrc = 0.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*
*  CHECK p_subrc <> 0 .
*
** Get Error Data(mspark)
*  SELECT SINGLE
*         kbetr
*         konwa INTO (it_error-tnetpr, it_error-waers)
*               FROM konp
*              WHERE knumh = wa_a017-knumh
*                AND kschl = 'PB00'.
*  SELECT SINGLE
*         kzust INTO it_error-kzust
*               FROM konh
*              WHERE knumh = wa_a017-knumh.
*
*  it_error-ekorg   = wa_a017-ekorg.
*  it_error-werks   = wa_a017-werks.
*  it_error-matnr   = it_mara-matnr.
*  it_error-profl   = it_mara-profl.
*  it_error-etype   = 'T'.
*  it_error-base_d  = p_date.
*  it_error-valid_d = wa_a017-datab.
*  APPEND it_error.
*ENDFORM.                    " FUTURE_CHECK_REASON_CODE
**&---------------------------------------------------------------------
*
**&      Form  GET_REAL_EFFPR
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM get_real_effpr.
*  DATA : kzust LIKE konh-kzust,
*         w_date LIKE sy-datum .
*  DATA : BEGIN OF it_effpr OCCURS 0,
*              effpr LIKE eine-effpr,
*         END OF it_effpr.
*
*  w_date = it_a017-datab.
*
*  LOOP AT it_a017 WHERE datab = w_date.
*    CLEAR it_effpr-effpr.
*    SELECT SINGLE kbetr
*             INTO it_effpr-effpr
*             FROM konp
*            WHERE knumh = it_a017-knumh
*              AND kschl = 'PB00'.
*    IF sy-subrc = 0.
*      APPEND it_effpr.
*    ENDIF.
*  ENDLOOP.
*
*  SORT it_effpr BY effpr ASCENDING.
*
*  READ TABLE it_effpr INDEX 1.
*
*  w_effpr = it_effpr-effpr.
*ENDFORM.                    " GET_REAL_EFFPR
**&---------------------------------------------------------------------
*
**&      Form  SAVE_LOG
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM save_log.
*  INSERT ztmm_log FROM TABLE it_log ACCEPTING DUPLICATE KEYS.
*
*  CHECK NOT it_analy IS INITIAL.
*
*  INSERT ztmm_analy FROM TABLE it_analy ACCEPTING DUPLICATE KEYS.
*ENDFORM.                    " SAVE_LOG
