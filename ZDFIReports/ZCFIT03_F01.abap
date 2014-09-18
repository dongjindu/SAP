*&---------------------------------------------------------------------*
*&  Include           ZCFIT03_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form INIT_SCREEN .

  GET PARAMETER ID 'BUK' FIELD P_BUKRS.

  IF P_BUKRS IS INITIAL.
    P_BUKRS = 'H201'.
  ENDIF.

  loop at screen.
    if screen-name = 'P_BUTXT'.
      screen-input  = 0.
      screen-intensified = '0'.
      screen-display_3d  = '0'.
      modify screen.
    endif.
    if screen-name = 'P_BUKRS'.
      screen-input = ' '.
      modify screen.
    endif.
  endloop.


* & find text.
  perform fi_wt_read_t001 using    p_bukrs
                          changing p_butxt.

endform.                    " INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  FI_WT_READ_T001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BUKRS  text
*      <--P_P_BUTXT  text
*----------------------------------------------------------------------*
form FI_WT_READ_T001  using    pa_bukrs
                      changing pa_butxt.

  data : it_t001 like t001.

  call function 'FI_WT_READ_T001'
    exporting
      i_bukrs   = pa_bukrs
    importing
      t_t001    = it_t001
    exceptions
      not_found = 1.

  case sy-subrc.
    when 0.
      pa_butxt = it_t001-butxt.
    when 1.
      message s101(f5).
    when others.
  endcase.


endform.                    " FI_WT_READ_T001
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form SELECT_DATA .

  IF p_freez EQ 'X'.
    PERFORM select_source_data.
    PERFORM compute_target_data.

    PERFORM make_it_plan.

    IF it_plan[] IS INITIAL.
      MESSAGE s001. EXIT.
    ENDIF.

    IF p_test EQ space.
      PERFORM exec_save.
    ENDIF.

  ELSE.  "finalize
    PERFORM get_freez_data.

    IF it_zdes[] IS INITIAL.
      MESSAGE s001. EXIT.
    ENDIF.
  ENDIF.

endform.                    " SELECT_DATA

************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
FORM user_command USING p_ucomm    LIKE sy-ucomm
                        p_selfield TYPE slis_selfield.

  DATA: l_answer.

  CASE p_ucomm.
*    WHEN '&IC1' OR '&ETA'.  "PICK.."
*      READ TABLE it_plan INDEX p_selfield-tabindex.
*      IF sy-subrc = 0.
*        SET PARAMETER ID:'BLN' FIELD it_zcmal-belnr,
*                         'BUK' FIELD it_zcmal-bukrs,
*                         'GJR' FIELD it_zcmal-gjahr.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.

    WHEN '&DATA_SAVE'.
      PERFORM popup_to_confirm USING text-006 text-008
                               CHANGING l_answer.
      CHECK l_answer EQ 'J'.
      PERFORM exec_save.

    WHEN 'FINAL'.
      PERFORM popup_to_confirm USING text-007 sy-title
                               CHANGING l_answer.
      CHECK l_answer EQ 'J'.
      PERFORM exec_save.
  ENDCASE.

ENDFORM.

************************************************************************
* Form  PF_STATUS_SET
************************************************************************
FORM  pf_status_set USING p_rt_extab TYPE slis_t_extab.

  IF p_freez EQ 'X'.
    SET TITLEBAR 'TITLE1'.
    SET PF-STATUS 'STANDARD' EXCLUDING p_rt_extab.
  ELSE.
    SET TITLEBAR 'TITLE2'.
    SET PF-STATUS 'MENU2' EXCLUDING p_rt_extab.
  ENDIF.

ENDFORM.

******************************
* FORM END_OF_LIST
******************************
FORM end_of_list.

  DATA : it_plan_lin TYPE i.

  DESCRIBE TABLE it_plan LINES it_plan_lin.

  SKIP 1.
  WRITE:/ '*** Processed Document total : ', it_plan_lin.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_TCURV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data_tcurv.

  SELECT SINGLE kurst INTO p_kurst
    FROM tcurv          "Exchange rate types for currency translation
   WHERE kurst EQ p_kurst.

  IF sy-subrc NE 0.
    MESSAGE e002 WITH 'Ex.Rate Type' p_kurst.
  ENDIF.

ENDFORM.                    " SELECT_DATA_TCURV
*&---------------------------------------------------------------------
*&      Form  GET_T001
*&---------------------------------------------------------------------
*
*----------------------------------------------------------------------
FORM get_t001.

  SELECT SINGLE spras waers INTO (sv_spras, t001-waers)
    FROM t001
   WHERE bukrs EQ p_bukrs.

ENDFORM.                                                    " GET_T001
*&---------------------------------------------------------------------*
*&      Form  select_source_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_source_data.

  PERFORM before_setting.
  CHECK NOT $group[] IS INITIAL OR NOT $group2[] IS INITIAL.

  PERFORM get_source.
  CHECK NOT r_wrk_fdsb[] IS INITIAL OR NOT r_wrk_fdsr[] IS INITIAL.

  PERFORM get_flowtype.    "*// == Transaction Flow

  PERFORM get_fdes.
  PERFORM get_fdt1.
  PERFORM get_fdsr.
  PERFORM get_fdsb.
  PERFORM mapping_group.
* PERFORM mapping_account.

* wk_dele[]: rolling plan version delete
  PERFORM get_deletion.

ENDFORM.                    " select_source_data
*&---------------------------------------------------------------------*
*&      Form  before_setting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM before_setting.

  CLEAR: iztfi_cmap[], iztfi_cmap, iztfi_cmad[], iztfi_cmad,
         i_map2[], i_map2.

  PERFORM setting_date.
  PERFORM job_gruouping.

* group mapping data
  SELECT * FROM ztfi_cmap CLIENT SPECIFIED
           INTO CORRESPONDING FIELDS OF TABLE iztfi_cmap
           WHERE mandt EQ sy-mandt.

* G/L acct. mapping data
  SELECT * FROM ztfi_cmad
           INTO CORRESPONDING FIELDS OF TABLE iztfi_cmad
           WHERE bukrs EQ p_bukrs.

* bank account mapping data
  SELECT * FROM ztfi_map2
           INTO CORRESPONDING FIELDS OF TABLE i_map2
           WHERE bukrs EQ p_bukrs.

ENDFORM.                    " before_setting
*&---------------------------------------------------------------------*
*&      Form  SETTING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setting_date.

  DATA: l_mon(2) TYPE n.

  CLEAR: $datum[], $datum.

  $datum-sign = 'I'.   $datum-option = 'BT'.
  CONCATENATE p_gjahr p_month '01' INTO $datum-low.

*// Calculate the interval period.
  l_mon = p_perid - 1.
  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
       EXPORTING
            months  = l_mon
            olddate = $datum-low
       IMPORTING
            newdate = $datum-high.
*
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = $datum-high
       IMPORTING
            last_day_of_month = $datum-high
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
  APPEND $datum.

ENDFORM.                    " SETTING_DATE
*&---------------------------------------------------------------------
*&      Form  JOB_GRUOUPING
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
FORM job_gruouping.

  CLEAR:$group[],$group.

*// == Cash Management: Grouping Structure
  SELECT selek exclude zeilt
    INTO (t038-selek, t038-exclude, t038-zeilt)
    FROM t038
   WHERE glied EQ p_glied.

    IF sy-subrc EQ 0.
      IF t038-zeilt EQ 'E'.
        IF t038-exclude EQ space.
          $group-sign = 'I'.
        ELSE.
          $group-sign = 'E'.
        ENDIF.
        $group-option = 'CP'.
        $group-low    = t038-selek.  "level
        APPEND $group.
        CLEAR:$group.
      ELSE.
        IF t038-exclude EQ space.
          $group2-sign = 'I'.
        ELSE.
          $group2-sign = 'E'.
        ENDIF.
        $group2-option = 'CP'.
        $group2-low    = t038-selek. "account
        APPEND $group2.
        CLEAR:$group2.
      ENDIF.
    ENDIF.

  ENDSELECT.

ENDFORM.                    " JOB_GRUOUPING
*&---------------------------------------------------------------------*
*&      Form  get_source
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_source.

  r_wrk_fdsb-sign = 'I'.  r_wrk_fdsb-option = 'EQ'.
  r_wrk_fdsr-sign = 'I'.  r_wrk_fdsr-option = 'EQ'.

*//  Planning levels,   Create Ranges.
  SELECT * FROM t036  "Planning levels
   WHERE orign IN (c_fdsr1, c_fdsr2, c_fdsr3, c_fdsb1, c_fdes1,
                   c_fdes2, c_fdes3)
     AND ebene IN $group.

    IF t036-orign = 'BNK'. "bank account
      r_wrk_fdsb-low = t036-ebene.
      APPEND r_wrk_fdsb.
    ELSE.
      r_wrk_fdsr-low = t036-ebene.
      APPEND r_wrk_fdsr.
    ENDIF.

  ENDSELECT.

ENDFORM.                    " get_source
*&---------------------------------------------------------------------
*&      Form  GET_FDSB
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
FORM get_fdsb.

*// == CMF summary records for G/L accounts
  SELECT ebene dispw wrshb dmshb datum gsber bnkko
    INTO (it_src-seben, it_src-sdspw, it_src-sdamt, it_src-slamt,
          it_src-sdate, it_src-gsber, it_src-bnkko)
    FROM fdsb
   WHERE bukrs EQ p_bukrs
     AND ebene IN r_wrk_fdsb
     AND datum IN $datum
*    AND datum LE $datum-high.
     AND bnkko IN $group2.

*-->skip (fdes) duplicate data
    READ TABLE *isrc WITH KEY bnkko = it_src-bnkko
                              seben = it_src-seben
                              sdspw = it_src-sdspw
                              sdate = it_src-sdate
                              gsber = it_src-gsber.

    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

*-->skip (fdt1) duplicate data
    READ TABLE *ifdt1 WITH KEY bnkko = it_src-bnkko
                               seben = it_src-seben
                               sdspw = it_src-sdspw
                               sdate = it_src-sdate
                               gsber = it_src-gsber.

    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

    IF it_src-slamt EQ 0.
* using current exchange rate (SAP L&F)
      PERFORM get_exchange_rate     USING sy-datum
                                          it_src-sdamt
                                          it_src-sdspw
                                 CHANGING it_src-slamt.
    ENDIF.

*...assign planning type
    IF it_src-seben CP c_levlb OR
       it_src-seben CP c_levlc OR
       it_src-seben CP c_levlw.
      it_src-dsart = c_f0type.
    ELSE.
      it_src-dsart = c_f1type.
    ENDIF.

    it_src-sorgn = 'BNK'.
    it_src-sbukr = p_bukrs.

    COLLECT it_src.
    CLEAR it_src.

  ENDSELECT.

ENDFORM.                    " GET_FDSB
*&---------------------------------------------------------------------
*&      Form  GET_FDSR
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
FORM get_fdsr.

*// CMF summary records for planning groups
  SELECT grupp ebene dispw wrshb dmshb datum gsber
    INTO (it_src-sgrpp, it_src-seben, it_src-sdspw, it_src-sdamt,
          it_src-slamt, it_src-sdate, it_src-gsber)
    FROM fdsr
   WHERE bukrs EQ p_bukrs
     AND ebene IN r_wrk_fdsr
     AND datum IN $datum
     AND grupp IN $group2.

*...skip (fdes) duplicates data
    READ TABLE *isrc WITH KEY sgrpp = it_src-sgrpp
                              seben = it_src-seben
                              sdspw = it_src-sdspw
                              sdate = it_src-sdate
                              gsber = it_src-gsber.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

*   if local amount = 0, document amount conversion.
    IF it_src-slamt EQ 0.
      PERFORM get_exchange_rate     USING sy-datum
                                          it_src-sdamt
                                          it_src-sdspw
                                 CHANGING it_src-slamt.
    ENDIF.

    it_src-sorgn = 'PSK'.
    it_src-sbukr = p_bukrs.

*...assign planning type
    IF it_src-seben CP c_levlb OR
       it_src-seben CP c_levlc OR
       it_src-seben CP c_levlw.
      it_src-dsart = c_f0type.
    ELSE.
      it_src-dsart = c_f1type.
    ENDIF.

    COLLECT it_src.
    CLEAR it_src.

  ENDSELECT.

ENDFORM.                    " GET_FDSR
*&---------------------------------------------------------------------
*&      Form  GET_EXCHANGE_RATE
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*      -->P_IT_SRC_DATUM  text
*      <--P_IT_SRC_UKURS  text
*      <--P_IT_HEAD_DMSHB  text
*----------------------------------------------------------------------
FORM get_exchange_rate USING datum
                             damt
                             dispw
                       CHANGING amount.
  CLEAR:amount.

  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
       EXPORTING
            date             = datum
            foreign_amount   = damt
            foreign_currency = dispw
            local_currency   = t001-waers
       IMPORTING
            local_amount     = amount.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*&      Form  mapping_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mapping_group.

  DATA: $ix LIKE sy-tabix,
        $subrc,
        shkzg.

  LOOP AT it_src WHERE sorgn EQ 'BNK'.
    CLEAR:it_src-sgrpp.

    READ TABLE iztfi_cmad WITH KEY bukrs = p_bukrs
                                   zeilt = 'E'
                                   hkont = it_src-seben.
    IF sy-subrc EQ 0.
      it_src-sgrpp = iztfi_cmad-grupp.
    ELSE.
      IF it_src-sdamt < 0.
        shkzg = 'H'.
      ELSE.
        shkzg = 'S'.
      ENDIF.

      READ TABLE iztfi_cmad WITH KEY bukrs = p_bukrs
                                     zeilt = ''
                                     hkont = ''
                                     shkzg = shkzg.
      IF sy-subrc EQ 0.
        it_src-sgrpp = iztfi_cmad-grupp.
      ENDIF.

    ENDIF.

    MODIFY it_src.
    CLEAR it_src.

  ENDLOOP.
*  LOOP AT iztfi_cmad.
*    CLEAR it_src-sgrpp.
*    LOOP AT it_src  WHERE sorgn EQ 'BNK'
*                    AND   shknt CP iztfi_cmad-hkont.
*      it_src-sgrpp = iztfi_cmad-grupp.
*      MODIFY it_src.
*    ENDLOOP.
*  ENDLOOP.

  LOOP AT it_src.
    $ix = sy-tabix.
*   it_src-sbukr = p_bukrs.

    IF it_src-sdamt EQ 0.
      DELETE it_src INDEX $ix.
      CONTINUE.
    ELSE.
      MODIFY it_src INDEX $ix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " mapping_group
*&---------------------------------------------------------------------*
*&      Form  get_fdt1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fdt1.

  DATA : l_flgrp LIKE i_cmaf-flgrp,
         l_shkzg.

  CLEAR: *ifdt1[], *ifdt1.

  SELECT * FROM fdt1
           WHERE bukrs   EQ p_bukrs
           AND   dzterm  IN $datum
*          AND   dcrdat  IN $datum
           AND   sfdlev  IN r_wrk_fdsb.
*          AND   fdgrp   IN $group2.

    MOVE : fdt1-bukrs     TO  it_src-sbukr,
           fdt1-sfdlev    TO  it_src-seben,
           fdt1-wzbetr    TO  it_src-sdspw,
           fdt1-bzbetr    TO  it_src-sdamt,
           fdt1-bhwbetr   TO  it_src-slamt,  "local amount
           fdt1-dzterm    TO  it_src-sdate,
           fdt1-gsber     TO  it_src-gsber,
           fdt1-rbankk    TO  it_src-bnkko.

    READ TABLE i_vtb WITH KEY bukrs   =  fdt1-bukrs
                              rfha    =  fdt1-rfha
                              rfhazu  =  fdt1-rfhazu
                              dcrdat  =  fdt1-dcrdat
                              tcrtim  =  fdt1-tcrtim
                              rfhazb  =  fdt1-rfhazb
                              wzbetr  =  fdt1-wzbetr.
    CLEAR: l_flgrp, l_shkzg.

    IF sy-subrc EQ 0.
      LOOP AT i_cmaf WHERE bukrs  EQ i_vtb-bukrs
                     AND   rantyp EQ i_vtb-rantyp
                     AND   sign   EQ i_vtb-ssign
                     AND ( frtype <= i_vtb-sfhazba AND
                           totype >= i_vtb-sfhazba ).
        l_flgrp = i_cmaf-flgrp.
      ENDLOOP.

    ELSE.
      READ TABLE iztfi_cmad WITH KEY bukrs = fdt1-bukrs
                                     zeilt = 'E'
                                     hkont = fdt1-sfdlev.
      IF sy-subrc EQ 0.
        l_flgrp = iztfi_cmad-grupp.
      ELSE.

        IF fdt1-bzbetr < 0.
          l_shkzg = 'H'.
        ELSE.
          l_shkzg = 'S'.
        ENDIF.

        READ TABLE iztfi_cmad WITH KEY bukrs = fdt1-bukrs
                                       zeilt = ''
                                       hkont = ''
                                       shkzg = l_shkzg.
        IF sy-subrc EQ 0.
          l_flgrp = iztfi_cmad-grupp.
        ENDIF.
      ENDIF.
    ENDIF.

    IF it_src-slamt EQ 0.
* using current exchange rate (SAP L&F)
      PERFORM get_exchange_rate     USING sy-datum
                                          it_src-sdamt
                                          it_src-sdspw
                                 CHANGING it_src-slamt.
    ENDIF.

    it_src-sgrpp = l_flgrp.
    it_src-sorgn = 'BNK'.
*   planning type
    it_src-dsart = c_f1type.


*...only fdt1 data
    MOVE-CORRESPONDING it_src TO *ifdt1.
    APPEND *ifdt1.
    CLEAR *ifdt1.

    COLLECT it_src.
    CLEAR it_src.

  ENDSELECT.

*  LOOP AT i_vtb.  "flow type
*    CLEAR l_flgrp.
*    LOOP AT i_cmaf WHERE bukrs  EQ i_vtb-bukrs
*                   AND   rantyp EQ i_vtb-rantyp
*                   AND   sign   EQ i_vtb-ssign
*                   AND ( frtype <= i_vtb-sfhazba AND
*                         totype >= i_vtb-sfhazba ).
*      l_flgrp = i_cmaf-flgrp.
*      EXIT.
*    ENDLOOP.
*
*    IF l_flgrp NE space.
*      SELECT sfdlev  wzbetr
*             bzbetr  bhwbetr dcrdat gsber
*             INTO (it_src-seben, it_src-sdspw,
*                   it_src-sdamt,  it_src-slamt, it_src-sdate,
*                   it_src-gsber)
*               FROM fdt1
*               WHERE bukrs   EQ p_bukrs
*               AND   dcrdat  IN $datum  "ok?
*               AND   planist EQ '1'  "plan
*               AND   sfdlev  IN r_wrk_fdsb. "ok?
*
**     CHECK it_src-shknt IN $group2.
*        it_src-sgrpp = l_flgrp.  "group!!
*        it_src-sorgn = 'BNK'.
*
*        IF it_src-slamt EQ 0.
** using current exchange rate (SAP L&F)
*          PERFORM get_exchange_rate     USING sy-datum
*                                              it_src-sdamt
*                                              it_src-sdspw
*                                     CHANGING it_src-slamt.
*        ENDIF.
*
**     planning type
*        it_src-dsart = c_f1type.
*
*        COLLECT it_src.  CLEAR it_src.
*      ENDSELECT.
*    ELSE.
*      SELECT fdgrp   sfdlev
*             wzbetr  bzbetr  bhwbetr dcrdat gsber
*             INTO (it_src-sgrpp, it_src-seben,
*                   it_src-sdspw, it_src-sdamt, it_src-slamt,
*                   it_src-sdate, it_src-gsber)
*               FROM fdt1
*               WHERE bukrs   EQ p_bukrs
*               AND   rfha    EQ i_vtb-rfha
*               AND   rfhazu  EQ i_vtb-rfhazu
*               AND   dcrdat  EQ i_vtb-dcrdat
*               AND   tcrtim  EQ i_vtb-tcrtim
*               AND   rfhazb  EQ i_vtb-rfhazb
**              AND   sfhazba EQ i_vtb-sfhazba
*               AND   planist EQ '1'. "plan
*
*        it_src-sorgn = 'BNK'.
*        IF it_src-slamt EQ 0.
** using current exchange rate (SAP L&F)
*          PERFORM get_exchange_rate     USING sy-datum
*                                              it_src-sdamt
*                                              it_src-sdspw
*                                     CHANGING it_src-slamt.
*        ENDIF.
*
**     planning type
*        it_src-dsart = c_f1type.
*
*        COLLECT it_src.  CLEAR it_src.
*      ENDSELECT.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                                                    " get_fdt1
*&---------------------------------------------------------------------*
*&      Form  get_flowtype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_flowtype.

  CLEAR: i_vtb[], i_vtb, i_cmaf[], i_cmaf.

*// == Transaction Flow
  SELECT bukrs rfha rfhazu dcrdat tcrtim rfhazb sfhazba rantyp
         ssign wzbetr
    INTO CORRESPONDING FIELDS OF TABLE i_vtb
    FROM vtbfhapo
   WHERE bukrs  EQ p_bukrs.
*    AND dcrdat IN $datum.

*// flow type vs. plan group mapping
  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_cmaf
    FROM ztfi_cmaf CLIENT SPECIFIED
   WHERE mandt EQ sy-mandt
     AND bukrs EQ p_bukrs.

ENDFORM.                    " get_flowtype
*&---------------------------------------------------------------------*
*&      Form  get_fdes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fdes.

*// = Cash Management and Forecast: Memo Records.
  SELECT grupp ebene dispw wrshb dmshb datum gsber bnkko
         INTO (it_src-sgrpp, it_src-seben, it_src-sdspw, it_src-sdamt,
               it_src-slamt, it_src-sdate, it_src-gsber, it_src-bnkko)
         FROM fdes
         WHERE bukrs EQ p_bukrs
         AND   ebene NE space
         AND   datum IN $datum.
*        AND   grupp IN $group2.

    IF it_src-slamt EQ 0.
      PERFORM get_exchange_rate     USING sy-datum
                                          it_src-sdamt
                                          it_src-sdspw
                                 CHANGING it_src-slamt.
    ENDIF.

    it_src-sbukr = p_bukrs.

*   planning type
    IF it_src-seben CP c_levld.
      it_src-dsart = c_f2type.
    ELSE.
      it_src-dsart = c_f1type.
    ENDIF.

    IF it_src-sgrpp NE space.
      IF it_src-sgrpp IN $group2 AND
         it_src-seben IN r_wrk_fdsr.
        it_src-sorgn = 'PSK'. "?
        COLLECT it_src.
        CLEAR it_src.
      ENDIF.
    ELSE. "level = BT -> orign = BNK.
      it_src-sorgn = 'BNK'. "?
      COLLECT it_src.
      CLEAR it_src.
    ENDIF.

  ENDSELECT.

  CLEAR: *isrc[], *isrc.
   *isrc[] = it_src[].

ENDFORM.                    " get_fdes
*&---------------------------------------------------------------------*
*&      Form  compute_target_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM compute_target_data.

*// *// == Distribution for Cash Mgmt Position and Liquidity Forecast
  PERFORM distribution_bylevel.

  PERFORM payment_cycle_bygroup.

ENDFORM.                    " compute_target_data
*&---------------------------------------------------------------------*
*&      Form  make_it_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_plan.

  IF it_tgt[] IS INITIAL.
    LOOP AT it_src.
      MOVE : it_src-sorgn TO it_plan-torgn,
             it_src-sgrpp TO it_plan-tgrpp,
             it_src-seben TO it_plan-teben,
             it_src-sdate TO it_plan-tdate,
             it_src-sbukr TO it_plan-tbukr,
             it_src-sdspw TO it_plan-tdspw,
             it_src-sdamt TO it_plan-tdamt,
             it_src-slamt TO it_plan-tlamt,
*            it_src-shknt TO it_plan-thknt,
             it_src-gsber TO it_plan-gsber,
             it_src-dsart TO it_plan-dsart.
      PERFORM date_get_week CHANGING it_plan-tdate
                                     it_plan-tweek.

      COLLECT it_plan.
    ENDLOOP.
  ELSE.
    LOOP AT it_tgt.
      MOVE-CORRESPONDING it_tgt TO it_plan.
      PERFORM date_get_week CHANGING it_plan-tdate
                                     it_plan-tweek.
      COLLECT it_plan.
    ENDLOOP.
  ENDIF.

  SORT it_plan BY torgn tgrpp teben tweek.

* wk_fdes[] : freezing work
  LOOP AT it_plan.
    MOVE : "it_plan-thknt TO wk_fdes-bnkko,
           it_plan-tgrpp TO wk_fdes-grupp,
*          it_plan-teben TO wk_fdes-ebene,
           it_plan-tdate TO wk_fdes-datum,
           it_plan-tbukr TO wk_fdes-bukrs,
           it_plan-tdspw TO wk_fdes-dispw,
           it_plan-tdamt TO wk_fdes-wrshb,
           it_plan-tlamt TO wk_fdes-dmshb,
           it_plan-dsart TO wk_fdes-dsart,
           it_plan-gsber TO wk_fdes-gsber,
           it_plan-dsart TO wk_fdes-merkm.  "characteristics
*---text info -> delete info.
    CONCATENATE p_gjahr p_month p_perid INTO wk_fdes-zuonr.
    APPEND wk_fdes.  CLEAR wk_fdes.
  ENDLOOP.

ENDFORM.                    " make_it_plan
*&---------------------------------------------------------------------*
*&      Form  distribution_bylevel
*&---------------------------------------------------------------------*
*  ex) A001  D1  2003/10/1  $100
*    1) distribution : D1     00  025  => D1  2003/10/01  $25
*       (DB: T038V)    D1  +  07  025  => D1  2003/10/08  $25
*                      D1  +  14  025  => D1  2003/10/15  $25
*                      D1  +  21  025  => D1  2003/10/22  $25
*       --> increase record up to 4.
*    2) payment cycle :
*       - days/weeks/months value check.
*       - start date = cycle date
*       A001-D1 : if months = '01' and cycle date = '2003/10/13'.
*       --> result : D1  2003/10/01  $25
*                    D1  2003/10/08  $25  =>  "D1  2003/10/13  $50"
*                    ---------------------------------------------
*                    D1  2003/10/15  $25
*                    D1  2003/10/22  $25  =>  "D1  2003/11/13  $50"
*----------------------------------------------------------------------*
FORM distribution_bylevel.

  DATA : l_day(2) TYPE n.

  LOOP AT it_src.
*// == Distribution for Cash Mgmt Position and Liquidity Forecast
    SELECT * FROM t038v
     WHERE ebene EQ it_src-seben.

      MOVE-CORRESPONDING it_src TO it_dst.
      it_dst-slamt = it_src-slamt * ( t038v-pzent / 100 ).
      it_dst-sdamt = it_src-sdamt * ( t038v-pzent / 100 ).
      l_day = t038v-vtage + 1.

      CONCATENATE it_src-sdate(6) l_day INTO it_dst-sdate.
      APPEND it_dst.

    ENDSELECT.

    IF sy-subrc <> 0.
      MOVE-CORRESPONDING it_src TO it_dst.
      APPEND it_dst.
      CLEAR it_dst.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " distribution_bylevel
*&---------------------------------------------------------------------*
*&      Form  payment_cycle_bygroup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM payment_cycle_bygroup.

  IF it_dst[] IS INITIAL.

    LOOP AT it_src.
      PERFORM assign_payment_cycle USING it_src-sorgn  it_src-sgrpp
                                         it_src-seben  it_src-sbukr
                                         it_src-sdspw  it_src-slamt
                                         it_src-sdamt  it_src-sdate
                                         it_src-dsart.
    ENDLOOP.

  ELSE.
    LOOP AT it_dst.
      PERFORM assign_payment_cycle USING it_dst-sorgn  it_dst-sgrpp
                                         it_dst-seben  it_dst-sbukr
                                         it_dst-sdspw  it_dst-slamt
                                         it_dst-sdamt  it_dst-sdate
                                         it_dst-dsart.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " payment_cycle_bygroup
*&-------------------------------------------------------------------
*&      Form  alvprn_basic01
*&-------------------------------------------------------------------
FORM alvprn_basic01.

  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  CLEAR   : gt_events, gs_layout.
  REFRESH : gt_events.

  gs_layout-header_text      = 'HEADER'.
  gs_layout-item_text        = 'item_text'.
  gs_layout-default_item     = 'X'.
* gs_layout-box_fieldname    = 'CHKBOX'.
  gs_layout-zebra            = 'X'.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = gt_events.

  DELETE gt_events WHERE name NE 'END_OF_PAGE'
                     AND name NE 'TOP_OF_PAGE'
                     AND name NE 'TOP_OF_LIST'
                     AND name NE 'END_OF_LIST'.
  LOOP AT gt_events ASSIGNING <ls_event>.
    CONCATENATE 'ALV_EVENT_'
                <ls_event>-name
                INTO <ls_event>-form.
  ENDLOOP.
*  PERFORM   form_setting
*   TABLES   gt_events
*    USING : slis_ev_pf_status_set  c_status_set,
*            slis_ev_user_command   c_user_command,
*            slis_ev_end_of_list    c_end_of_list.

* PROGRAM  ID
  g_repid = sy-repid.

ENDFORM.                    " alvprn_basic01
*&-------------------------------------------------------------------
*&      Form  form_setting
*&-------------------------------------------------------------------
FORM form_setting TABLES p_events_t LIKE gt_events
                   USING p_com
                         p_form.

  DATA : l_event_s    TYPE  slis_alv_event.

  READ TABLE  p_events_t  WITH KEY  name = p_com
                            INTO l_event_s.
  IF   sy-subrc EQ 0.
    MOVE     p_form      TO   l_event_s-form.
    MODIFY   p_events_t  FROM l_event_s INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " form_setting
*&-------------------------------------------------------------------
*&      Form  alvprn_field01
*&-------------------------------------------------------------------
FORM alvprn_field01 USING p_intab.

  CLEAR   : gt_field, gt_fieldcat.
  REFRESH : gt_field, gt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_field.

* FIELD SETTING
  CLEAR g_cnt.
  PERFORM field_setting TABLES gt_fieldcat USING :
                                'S' 'TORGN'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                ' ' 'SELTEXT_M'   'Source',
                                'E' 'KEY'         'X',

                                'S' 'TGRPP'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                ' ' 'SELTEXT_M'   'Pln.Grp.',
                                'E' 'KEY'         'X',

                                'S' 'TEBEN'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'JUST'        'C',
                                ' ' 'OUTPUTLEN'   '04',
                                ' ' 'SELTEXT_M'   'Level',
                                'E' 'KEY'         ' ',

                                'S' 'TWEEK'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '06',
                                ' ' 'JUST'        'C',
                                ' ' 'SELTEXT_M'   'Week',
                                'E' 'KEY'         ' ',

                                'S' 'GSBER'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '04',
                                ' ' 'JUST'        'C',
                                ' ' 'SELTEXT_M'   'B/A',
                                'E' 'KEY'         ' ',

                                'S' 'TDSPW'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '04',
                                ' ' 'SELTEXT_M'   'Curr.',
                                'E' 'KEY'         ' ',

                                'S' 'TDAMT'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'CURRENCY'    it_plan-tdspw,
                                ' ' 'SELTEXT_M'   'Planned Amt.',
                                'E' 'DO_SUM'      'X',

                                'S' 'TLAMT'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'CURRENCY'    t001-waers,
                                ' ' 'SELTEXT_M'   'Local Amt.',
                                'E' 'DO_SUM'      'X'.

ENDFORM.
*&-------------------------------------------------------------------
*&      Form  alvprn_field02
*&-------------------------------------------------------------------
FORM alvprn_field02 USING p_intab.

  CLEAR   : gt_field, gt_fieldcat.
  REFRESH : gt_field, gt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_field.

* FIELD SETTING
  CLEAR g_cnt.
  PERFORM field_setting TABLES gt_fieldcat USING :
                                'S' 'GRUPP'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                ' ' 'SELTEXT_M'   'Pln.Grp.',
                                'E' 'KEY'         'X',

                                'S' 'EBENE'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '04',
                                ' ' 'JUST'        'C',
                                ' ' 'SELTEXT_M'   'Level',
                                'E' 'KEY'         ' ',

                                'S' 'IDENR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                ' ' 'SELTEXT_M'   'ID num',
                                'E' 'KEY'         ' ',

                                'S' 'DISPW'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '04',
                                ' ' 'SELTEXT_M'   'Curr.',
                                'E' 'KEY'         ' ',

                                'S' 'WRSHB'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'CURRENCY'    it_zdes-dispw,
                                ' ' 'SELTEXT_M'   'Planned Amt.',
                                'E' 'DO_SUM'      'X',

                                'S' 'DMSHB'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'CURRENCY'    t001-waers,
                                ' ' 'SELTEXT_M'   'Local Amt.',
                                'E' 'DO_SUM'      'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_list.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
*         I_INTERFACE_CHECK        = ' '
          i_callback_program       =  g_repid
          i_callback_pf_status_set = 'PF_STATUS_SET'
          i_callback_user_command  = 'USER_COMMAND'
          is_layout                =  gs_layout
          it_fieldcat              =  gt_fieldcat[]
*         IT_EXCLUDING             =
*         IT_SPECIAL_GROUPS        =  GT_SLIS_SP_GROUP_ALV
*         IT_SORT                  =  IT_SORT[]
*         IT_FILTER                =
*         IS_SEL_HIDE              =
*         I_DEFAULT                = 'X'
          i_save                   =  g_save
          is_variant               =  g_variant
          it_events                =  gt_events[]
       TABLES
          t_outtab                 =  it_plan.

ENDFORM.                    " display_list
*&-------------------------------------------------------------------
*&      Form  field_setting
*&-------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat
                    USING p_gub
                          p_fname
                          p_con.

  DATA: l_col(40).

  IF p_gub = 'S'.
    CLEAR g_fieldcat_s.
    READ TABLE gt_field INTO g_fieldcat_s
                        WITH KEY fieldname  = p_fname.
    EXIT.
  ENDIF.

  FIELD-SYMBOLS <fs>.
  CONCATENATE 'G_FIELDCAT_S-' p_fname  INTO l_col.
  ASSIGN      (l_col)         TO       <fs>.
  MOVE         p_con          TO       <fs>.


* DATA  APPEND
  CHECK  p_gub = 'E'.

  g_cnt = g_cnt + 1.
  g_fieldcat_s-col_pos = g_cnt.

  g_fieldcat_s-seltext_l = g_fieldcat_s-seltext_s
                         = g_fieldcat_s-seltext_m.
  APPEND g_fieldcat_s TO p_fieldcat_t.

ENDFORM.                    " field_setting
*&---------------------------------------------------------------------*
*&      Form  exec_save
*&---------------------------------------------------------------------*
* Before freezing or finalizing, delete existing memo records.
*  1) freezing/finalizing setting : zuonr <- year + month + horizon
*  2) delete key -> "zuonr"
*----------------------------------------------------------------------*
FORM exec_save.

*  DATA: l_answer.
*
*  IF sy-ucomm EQ '&DATA_SAVE'.
*    PERFORM popup_to_confirm CHANGING l_answer.
*    CHECK l_answer EQ 'J'.
*  ENDIF.

  IF p_freez EQ 'X'.
    PERFORM dele_data.
    PERFORM save_data.
  ELSE.
*   PERFORM dele_data.
    PERFORM finalize_data.
  ENDIF.

ENDFORM.                    " exec_save
*&---------------------------------------------------------------------*
*&      Form  assign_payment_cycle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_payment_cycle USING p_orgn
                                p_grpp
                                p_eben
                                p_bukr
                                p_dspw
                                p_lamt
                                p_damt
                                p_date
                                p_dsart.

  DATA : l_day(2) TYPE n,
         l_subrc  LIKE sy-subrc.

  CLEAR l_cycle_date.
  SELECT SINGLE * FROM t035z
   WHERE grupp EQ p_grpp
     AND ebene EQ p_eben.

  l_subrc = sy-subrc.

  MOVE : p_orgn  TO it_tgt-torgn,
         p_grpp  TO it_tgt-tgrpp,
         p_eben  TO it_tgt-teben,
         p_bukr  TO it_tgt-tbukr,
         p_dspw  TO it_tgt-tdspw,
         p_dsart TO it_tgt-dsart.

  IF l_subrc <> 0.
    MOVE : p_lamt  TO it_tgt-tlamt,
           p_damt  TO it_tgt-tdamt,
           p_date  TO it_tgt-tdate.
    APPEND it_tgt.
    CLEAR it_tgt.
    EXIT.
  ENDIF.

*-- if payment cycle is set
  IF l_cycle_date IS INITIAL AND NOT t035z-abdat IS INITIAL.
    l_cycle_date = t035z-abdat. "start date
  ENDIF.

*........daily payment cycle
  IF t035z-intta NE space.       "daily
*   IF it_dst-sdate <= l_cycle_date.
    IF l_cycle_date IS INITIAL.
      it_tgt-tdate = p_date.  "it_src-datum
      it_tgt-tlamt = it_tgt-tlamt + p_lamt.
      it_tgt-tdamt = it_tgt-tdamt + p_damt.
      APPEND it_tgt.
    ELSE.
      IF p_date <= l_cycle_date.
        it_tgt-tdate = l_cycle_date.
        it_tgt-tlamt = it_tgt-tlamt + p_lamt.
        it_tgt-tdamt = it_tgt-tdamt + p_damt.
        APPEND it_tgt.
      ELSE.

        DO.
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
               EXPORTING
                    date      = l_cycle_date
                    days      = t035z-intta
                    months    = 0
                    signum    = '+'
                    years     = 0
               IMPORTING
                    calc_date = l_cycle_date.

*       IF it_dst-sdate <= l_cycle_date.  EXIT.  ENDIF.
          IF p_date <= l_cycle_date.  EXIT.  ENDIF.
        ENDDO.
        it_tgt-tdate = l_cycle_date.
        it_tgt-tlamt = it_tgt-tlamt + p_lamt.
        it_tgt-tdamt = it_tgt-tdamt + p_damt.
        APPEND it_tgt.

      ENDIF.
    ENDIF.

*........weekly payment cycle

  ELSEIF t035z-intwo NE space.   "weekly
*   IF it_dst-sdate <= l_cycle_date.
    IF l_cycle_date IS INITIAL.
      it_tgt-tdate = p_date.  "it_src-datum
      it_tgt-tlamt = it_tgt-tlamt + p_lamt.
      it_tgt-tdamt = it_tgt-tdamt + p_damt.
      APPEND it_tgt.
    ELSE.
      IF p_date <= l_cycle_date.
        it_tgt-tdate = l_cycle_date.
        it_tgt-tlamt = it_tgt-tlamt + p_lamt.
        it_tgt-tdamt = it_tgt-tdamt + p_damt.
        APPEND it_tgt.
      ELSE.
        l_day = t035z-intwo * 7.

        DO.
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
               EXPORTING
                    date      = l_cycle_date
                    days      = l_day
                    months    = 0
                    signum    = '+'
                    years     = 0
               IMPORTING
                    calc_date = l_cycle_date.

*       IF it_dst-sdate <= l_cycle_date.  EXIT.  ENDIF.
          IF p_date <= l_cycle_date.
            EXIT.
          ENDIF.

        ENDDO.

        it_tgt-tdate = l_cycle_date.
        it_tgt-tlamt = it_tgt-tlamt + p_lamt.
        it_tgt-tdamt = it_tgt-tdamt + p_damt.
        APPEND it_tgt.
      ENDIF.

    ENDIF.

*........monthly payment cycle
  ELSEIF t035z-intmo NE space.   "monthly
*   IF it_dst-sdate <= l_cycle_date.
    IF l_cycle_date IS INITIAL.
      it_tgt-tdate = p_date.  "it_src-datum
      it_tgt-tlamt = it_tgt-tlamt + p_lamt.
      it_tgt-tdamt = it_tgt-tdamt + p_damt.
      APPEND it_tgt.

    ELSE.
      IF p_date <= l_cycle_date.
        it_tgt-tdate = l_cycle_date.
        it_tgt-tlamt = it_tgt-tlamt + p_lamt.
        it_tgt-tdamt = it_tgt-tdamt + p_damt.
        APPEND it_tgt.

      ELSE.
        DO.

          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
               EXPORTING
                    date      = l_cycle_date
                    days      = t035z-intmo
                    months    = 0
                    signum    = '+'
                    years     = 0
               IMPORTING
                    calc_date = l_cycle_date.

*       IF it_dst-sdate <= l_cycle_date.  EXIT.  ENDIF.
          IF p_date <= l_cycle_date.
            EXIT.
          ENDIF.
        ENDDO.

        it_tgt-tdate = l_cycle_date.
        it_tgt-tlamt = it_tgt-tlamt + p_lamt.
        it_tgt-tdamt = it_tgt-tdamt + p_damt.
        APPEND it_tgt.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " assign_payment_cycle
*&---------------------------------------------------------------------*
*&      Form  get_deletion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_deletion.

  DATA: l_zuonr LIKE fdes-zuonr.

  CLEAR:it_dele[],it_dele,wk_dele[],wk_dele.

  CONCATENATE p_gjahr p_month p_perid INTO l_zuonr.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_dele
    FROM fdes
   WHERE bukrs EQ p_bukrs
     AND archk EQ space
     AND datum IN $datum
*    AND zuonr EQ l_zuonr.
     AND dsart IN (c_f0type, c_f1type, c_f2type).

  LOOP AT it_dele.
    MOVE-CORRESPONDING it_dele TO wk_dele.
    APPEND wk_dele.
    CLEAR:wk_dele.

    it_dele-archk = 'X'.
    it_dele-aenus = sy-uname.
    it_dele-aendt = sy-datum.
    it_dele-avdat = sy-datum.

    MOVE-CORRESPONDING it_dele TO wk_dele.
    APPEND wk_dele.
    CLEAR wk_dele.

  ENDLOOP.

ENDFORM.                    " get_deletion
*&---------------------------------------------------------------------*
*&      Form  popup_to_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ANSWER  text
*----------------------------------------------------------------------*
FORM popup_to_confirm USING l_textline1
                            l_textline2
                      CHANGING fp_answer.

  DATA: l_defaultoption. "l_textline1(70),  l_textline2(70).

  CLEAR fp_answer.

  l_defaultoption = 'N'.

* l_textline1     = text-006.
* l_textline2     = text-007.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            defaultoption = l_defaultoption
            textline1     = l_textline1
*           textline2     = l_textline2
            titel         = l_textline2  "sy-title
       IMPORTING
            answer        = fp_answer.

ENDFORM.                    " popup_to_confirm
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.

  CALL FUNCTION 'TRCM_FDES_IMPORT'
       TABLES
            i_tab_fdes                = wk_fdes
       EXCEPTIONS
            conversion_failed         = 1
            database_operation_failed = 2
            ignore                    = 3
            OTHERS                    = 4.

  IF sy-subrc <> 0.
*   MESSAGE w000 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    MESSAGE w000 WITH 'Failed to freeze'.
    EXIT.
  ELSE.
    MESSAGE s007.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  dele_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dele_data.

  CHECK NOT wk_dele[] IS INITIAL.

  CALL FUNCTION 'CASH_FORECAST_MEMO_RECORD_UPD'
       EXPORTING
            aktion   = '2'
       TABLES
            tab_fdes = wk_dele.

  COMMIT WORK.

ENDFORM.                    " dele_data
*&---------------------------------------------------------------------*
*&      Form  date_get_week
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM date_get_week CHANGING p_date p_week.

* input date -> week
  CALL FUNCTION 'DATE_GET_WEEK'
       EXPORTING
            date = p_date
       IMPORTING
            week = p_week.

* week -> fisrt date
  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
       EXPORTING
            week = p_week
       IMPORTING
            date = p_date.

ENDFORM.                    " date_get_week
*&---------------------------------------------------------------------
*&      Form  GET_PERIOD
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*      <--P_R_PERID  text
*----------------------------------------------------------------------
FORM get_period.

  DATA : l_mon(2) TYPE n.

  CLEAR:$datum[],$datum.

  $datum-sign      = 'I'.
  $datum-option    = 'BT'.
  $datum-low+0(04) = p_gjahr.
  $datum-low+4(02) = p_month.
  $datum-low+6(02) = '01'.

  l_mon = p_perid - 1.

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
       EXPORTING
            months  = l_mon
            olddate = $datum-low
       IMPORTING
            newdate = $datum-high.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = $datum-high
       IMPORTING
            last_day_of_month = $datum-high
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.

  IF sy-subrc <> 0.
    MESSAGE e999 WITH 'Function Error'.
  ENDIF.

  APPEND $datum.

ENDFORM.                    " GET_PERIOD
*&---------------------------------------------------------------------*
*&      Form  get_freez_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_freez_data.

  sv_rollym+0(04) = p_gjahr.
  sv_rollym+4(02) = p_month.

  PERFORM setting_date.

* fill it_zdes
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_zdes
    FROM fdes
   WHERE bukrs EQ p_bukrs
     AND archk EQ space
     AND dsart IN (c_f0type, c_f1type, c_f2type)
     AND datum IN $datum.

* fill dt_zdes
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE dt_zdes
    FROM ztfi_des
   WHERE bukrs EQ p_bukrs
     AND datum IN $datum.

  PERFORM set_it_zdes.                        "..finalizing list
  PERFORM set_dt_zdes.                        "..delete list

ENDFORM.                    " get_freez_data
*&---------------------------------------------------------------------
*&      Form  SET_IT_ZDES
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
FORM set_it_zdes.

  LOOP AT it_zdes.
*... wk_dele[]: rolling plan version delete
    MOVE-CORRESPONDING it_zdes TO wk_dele.
    APPEND wk_dele.
    CLEAR:wk_dele.

    MOVE-CORRESPONDING it_zdes TO wk_dele.
    wk_dele-archk = 'X'.
    wk_dele-aenus = sy-uname.
    wk_dele-aendt = sy-datum.
    wk_dele-avdat = sy-datum.
    APPEND wk_dele.
    CLEAR wk_dele.
*...
    it_zdes-gjahr = it_zdes-datum+0(04).

    PERFORM check_mode.              "create/modify
    PERFORM date_get_week CHANGING it_zdes-datum
                                   it_zdes-dweek.
*   MODIFY it_zdes.
    COLLECT it_zdes.

  ENDLOOP.

  SORT it_zdes BY grupp ebene.

ENDFORM.                    " SET_IT_ZDES
*&---------------------------------------------------------------------
*&      Form  SET_DT_ZDES
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
FORM set_dt_zdes.

  DATA: $subrc.

  LOOP AT dt_zdes.
    SELECT SINGLE archk INTO fdes-archk
      FROM fdes
     WHERE bukrs EQ dt_zdes-bukrs
       AND idenr EQ dt_zdes-idenr
       AND archk EQ space.

    IF sy-subrc EQ 0.    "..no archiving
      DELETE dt_zdes.
      CONTINUE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " SET_DT_ZDES
*&---------------------------------------------------------------------
*&      Form  CHECK_ARCHIV
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*       ..Archiving check
*----------------------------------------------------------------------
FORM check_archiv CHANGING $subrc.

  CLEAR: $subrc,dt_zdes-archk.

  SELECT SINGLE archk INTO dt_zdes-archk
    FROM fdes
   WHERE bukrs EQ dt_zdes-bukrs
     AND idenr EQ dt_zdes-idenr.

  IF sy-subrc EQ 0 AND dt_zdes-archk EQ space.    "..no archiving
    $subrc = 'D'.
  ELSEIF sy-subrc NE 0.                           "..already archiving
    dt_zdes-archk = 'X'.
  ENDIF.

ENDFORM.                    " CHECK_ARCHIV
*&---------------------------------------------------------------------*
*&      Form  CHECK_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_mode.

  CLEAR:it_zdes-gubun.

  SELECT SINGLE * FROM ztfi_des
   WHERE bukrs EQ it_zdes-bukrs
     AND idenr EQ it_zdes-idenr.

  IF sy-subrc NE 0.
    it_zdes-gubun = 'Y'.
  ELSEIF ztfi_des-wrshb <> it_zdes-wrshb.
    it_zdes-gubun = 'M'.
  ELSE.
    it_zdes-gubun = 'N'.
  ENDIF.

ENDFORM.                    " CHECK_MODE
*&---------------------------------------------------------------------*
*&      Form  alvprn_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PLAN  text
*----------------------------------------------------------------------*
FORM alvprn_list TABLES p_tab.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
*         I_INTERFACE_CHECK        = ' '
          i_callback_program       =  g_repid
          i_callback_pf_status_set = 'PF_STATUS_SET'
          i_callback_user_command  = 'USER_COMMAND'
          is_layout                =  gs_layout
          it_fieldcat              =  gt_fieldcat[]
*         IT_EXCLUDING             =
*         IT_SPECIAL_GROUPS        =  GT_SLIS_SP_GROUP_ALV
*         IT_SORT                  =  IT_SORT[]
*         IT_FILTER                =
*         IS_SEL_HIDE              =
*         I_DEFAULT                = 'X'
          i_save                   =  g_save
          is_variant               =  g_variant
          it_events                =  gt_events[]
       TABLES
          t_outtab                 =  p_tab.

ENDFORM.                    " alvprn_list
*&---------------------------------------------------------------------
*&      Form  INSERT_ZTFI_DES
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*      <--P_$SUBRC  text
*----------------------------------------------------------------------
FORM insert_ztfi_des CHANGING $subrc.

  CLEAR:$subrc.

  LOOP AT it_zdes WHERE gubun NE 'N'.      "create
    CLEAR: ztfi_des.
    MOVE-CORRESPONDING it_zdes TO ztfi_des.
    ztfi_des-erdat = sy-datum.
    ztfi_des-uname = sy-uname.
    ztfi_des-uzeit = sy-uzeit.

    MODIFY ztfi_des.

    IF sy-subrc NE 0.
      $subrc = 'Insert'.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " INSERT_ZTFI_DES
*&---------------------------------------------------------------------
*&      Form  DELETE_ZTFI_DES
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
FORM delete_ztfi_des CHANGING $subrc.

  CLEAR:$subrc.

  LOOP AT dt_zdes.

    DELETE FROM ztfi_des   "modify 2004/02/24
           WHERE bukrs EQ dt_zdes-bukrs
           AND   gjahr EQ dt_zdes-gjahr
           AND   grupp EQ dt_zdes-grupp
           AND   ebene EQ dt_zdes-ebene
           AND   dispw EQ dt_zdes-dispw
           AND   datum EQ dt_zdes-datum
           AND   gsber EQ dt_zdes-gsber
           AND   idenr EQ dt_zdes-idenr
           AND   dweek EQ dt_zdes-dweek.

    IF sy-subrc NE 0.
      $subrc = 'Update'.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " DELETE_ZTFI_DES
*&---------------------------------------------------------------------
*&      Form  UPDATE_ZTFI_DSS
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
FORM update_ztfi_dss CHANGING $subrc.

  CLEAR:$subrc.

  MODIFY ztfi_dss FROM TABLE it_zdss.

  IF sy-subrc NE 0.
    $subrc = 'ZDSS'.
    EXIT.
  ENDIF.

ENDFORM.                    " UPDATE_ZTFI_DSS
*&---------------------------------------------------------------------*
*&      Form  finalize_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM finalize_data.

  DATA: $subrc(10).

  PERFORM insert_ztfi_des CHANGING $subrc.         "..create

  IF $subrc NE space.
    MESSAGE s002 WITH $subrc.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

  PERFORM delete_ztfi_des CHANGING $subrc.         "..delete

  IF $subrc NE space.
    MESSAGE s002 WITH $subrc.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

  PERFORM make_it_zdss.                        "..????
  PERFORM update_ztfi_dss CHANGING $subrc.

  IF $subrc NE space.
    MESSAGE s000 WITH 'Failed to finalize' $subrc. "failed to save
    ROLLBACK WORK.
    EXIT.
  ELSE.
    COMMIT WORK.
    MESSAGE s007.     "Saved successfully
    EXIT.
  ENDIF.

ENDFORM.                    " finalize_data
*&---------------------------------------------------------------------
*&      Form  DELETE_ZTFI_DSS
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
FORM delete_ztfi_dss.

  CLEAR : ut_zdss, ut_zdss[].

  SELECT * FROM ztfi_dss INTO TABLE ut_zdss
   WHERE bukrs = p_bukrs
     AND datum = sv_rollym.

  CLEAR : ut_zdss.

  LOOP AT ut_zdss.
    DO 12 TIMES.
      idx = idx + 1.
      CONCATENATE 'IT_ZDSS-WTP' idx INTO fs_wtp.
      CONCATENATE 'IT_ZDSS-WLP' idx INTO fs_wlp.

      ASSIGN (fs_wtp) TO <fs_wtp>.
      CLEAR <fs_wtp>.

      ASSIGN (fs_wlp) TO <fs_wlp>.
      CLEAR <fs_wlp>.
    ENDDO.

    MODIFY ut_zdss.
  ENDLOOP.

  UPDATE ztfi_dss FROM TABLE ut_zdss.

ENDFORM.                    " DELETE_ZTFI_DSS
*&---------------------------------------------------------------------
*&      Form  SUMMARY_IT_ZDES
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
FORM summary_it_zdes.

  DATA: l_idx(2) TYPE n.

  CLEAR:it_zdss[],it_zdss.

  LOOP AT it_zdes.
    CLEAR:it_zdss, l_idx.
    MOVE-CORRESPONDING it_zdes TO it_zdss.

    it_zdss-datum = sv_rollym. "yymm

*---> Changed By JIPARK 2004/03/18
*   l_idx = it_zdes-datum+4(2) - sv_rollym + 1.
*   CONCATENATE 'IT_ZDSS-WTP' l_idx INTO fs_wtp.
*   CONCATENATE 'IT_ZDSS-WLP' l_idx INTO fs_wlp.
    CONCATENATE 'IT_ZDSS-WTP' it_zdes-datum+4(2) INTO fs_wtp.
    CONCATENATE 'IT_ZDSS-WLP' it_zdes-datum+4(2) INTO fs_wlp.

    ASSIGN (fs_wtp) TO <fs_wtp>.
    ASSIGN (fs_wlp) TO <fs_wlp>.
    <fs_wtp> = it_zdes-wrshb.
    <fs_wlp> = it_zdes-dmshb.

    COLLECT it_zdss.
    CLEAR:it_zdss.

  ENDLOOP.

ENDFORM.                    " SUMMARY_IT_ZDES
*&---------------------------------------------------------------------*
*&      Form  make_it_zdss
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_zdss.

  PERFORM delete_ztfi_dss.
  PERFORM summary_it_zdes.

ENDFORM.                    " make_it_zdss
*&---------------------------------------------------------------------*
*&      Form  mapping_account
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mapping_account.

*  LOOP AT it_src WHERE sorgn NE 'BNK'.
*    READ TABLE i_map2 WITH KEY grupp = it_src-sgrpp.
*    IF sy-subrc EQ 0.
*      it_src-shknt = i_map2-hkont.
*      MODIFY it_src.  CLEAR it_src.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " mapping_account
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DISPLAY_ALV .

  CHECK p_log EQ 'X'.

* ALV HEADER & FIELD SETTING
  IF p_freez EQ 'X'.
    PERFORM : alvprn_basic01,
              alvprn_field01 USING 'IT_PLAN',
              alvprn_list    TABLES it_plan.
  ELSE.
    PERFORM : alvprn_basic01,
              alvprn_field02 USING 'IT_ZDES',
              alvprn_list    TABLES it_zdes.
  ENDIF.

endform.                    " DISPLAY_ALV
