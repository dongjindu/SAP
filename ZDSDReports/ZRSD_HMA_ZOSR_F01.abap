*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_F01                                        *
*----------------------------------------------------------------------*
FORM p1000_start_progressbar USING pf_text
                                   value(pf_val).

  DATA: percent(3) TYPE n.

  MOVE: sy-index TO percent.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text
    EXCEPTIONS
      OTHERS     = 1.

ENDFORM.                    " P1000_START_PROGRESSBAR

*---------------------------------------------------------------------*
*       FORM P2000_GET_DATA                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM p2000_get_data.

  DATA : v_date TYPE d.
  DATA : lv_wocl LIKE mara-matnr,
         lv_wohd LIKE mara-matnr.
  DATA : percent(3) TYPE n.

  CLEAR : g_cond, g_cond[].

*  IF p_hma = 'X'.                                           "02.07.2012
*    CONCATENATE 'AND  WO_NATION  = ''' 'B28'  ''''
*                          INTO g_cond-text.
*    APPEND g_cond.
*  ELSEIF p_hac = 'X'.
*    CONCATENATE 'AND  WO_NATION  = ''' 'B06'  ''''
*                          INTO g_cond-text.
*    APPEND g_cond.
*  ENDIF.
*
*  READ TABLE g_cond INDEX 1.
*  MOVE '   ' TO g_cond-text(3).
*  MODIFY g_cond INDEX 1.

  i_dat_3 =  s_aedat-low - p_shp_d. "Ship Out

  IF p_miss = space.

**#1.1 Get ZTSD_UM -> IT_UM Status Except 'F' .
*-Get all w/o  : Victor 09.01.2011
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_um
        FROM ztsd_um
        WHERE wo_serial IN s_seria
          AND wo_nation IN s_nation
          AND wo_dealer IN s_dealer
*        AND (g_cond)
          AND zvin      IN s_zvin
          AND status    NE   'F'.

**#1.2 Get ZTSD_UM -> IT_UM Status 'F' , Consider Ship out date.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_um
      FROM ztsd_um
      WHERE wo_serial IN s_seria
        AND wo_nation IN s_nation
        AND wo_dealer IN s_dealer
*      AND (g_cond)
        AND status    EQ  'F'
        AND ship_out  GE   i_dat_3 .   "Changed by Victor

*for missing VIN processing
** Changed by Furong on 11/14/12 to consider nation code
*  ELSE.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_UM
*        FROM ZTSD_UM
*        WHERE ZVIN      IN S_ZVIN.
*  ENDIF.
*
*
*  SORT IT_UM BY ZVIN INTNO DESCENDING .
*  DELETE ADJACENT DUPLICATES FROM IT_UM COMPARING ZVIN.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_um
        FROM ztsd_um
        WHERE wo_nation EQ p_nation
*          AND wo_dealer EQ p_dealer
          AND zvin      IN s_zvin.
  ENDIF.

  SORT it_um BY wo_nation wo_dealer zvin intno DESCENDING .
*  DELETE ADJACENT DUPLICATES FROM it_um
*         COMPARING wo_nation wo_dealer zvin.
** End


*-<P Deleted(Scrap), S Deleted(SpecChange):scrapdate, spec chgdate - 60
*- D Deleted(Disposal,Normal) : work order creation date - 60
*#1.3 Get Value V_DATE : Input S_AEDATE - Input P_DEL_D
  v_date  =  s_aedat-low - p_del_d.

  CLEAR : lv_wocl, lv_wohd.

  PERFORM p1000_start_progressbar USING 'Check Unit...' '10'.

  LOOP AT it_um.

    CONCATENATE it_um-wo_serial
                it_um-wo_nation
                it_um-wo_dealer
                it_um-wo_extc
                it_um-wo_intc   INTO lv_wocl.

    CONCATENATE it_um-wo_serial
                it_um-wo_nation
                it_um-wo_dealer INTO lv_wohd.

*#2 . Processing  Status is 'P' or 'S' or 'F' .
*  _  Reference ZTPP_VM Data. Check ZTPP_VM Data.
*  _  Incorrect Data is Deleted.
    CASE it_um-status .
**************************************************
      WHEN 'P' OR 'S'.
*#2.1 Check ZTPP_VM Model_Code, Body_No
        CLEAR : ztpp_vm.

        SELECT SINGLE * FROM ztpp_vm
          WHERE model_code = it_um-model_code
            AND body_no    = it_um-body_no.

        IF sy-subrc = 0.

          IF it_um-status = 'P' AND ztpp_vm-scrap_date <> '00000000'.
            IF ztpp_vm-scrap_date <= v_date.
              DELETE it_um.
              CONTINUE.      "Victor 09.02.2011
            ENDIF.
          ELSEIF it_um-status = 'S' AND ztpp_vm-chg_date <> '00000000'.
            IF ztpp_vm-chg_date <= v_date.
              DELETE it_um.
              CONTINUE.
            ENDIF.
          ENDIF.

        ENDIF.
***************************************************
      WHEN 'D'.
        CLEAR : mara.
*#2.2 Check Work Header Color.

        SELECT SINGLE * FROM mara
        WHERE  matnr EQ lv_wocl.

        IF sy-subrc = 0.
          IF mara-ersda <= v_date.
            DELETE it_um.
            CONTINUE.      "Victor 09.02.2011
          ENDIF.
        ENDIF.
    ENDCASE.
*#. Create UM Header : IT_COL_UM -> Change Varient Name IT_HDUM.

    it_hdum-wohd       = lv_wohd.
    it_hdum-wocl       = lv_wocl.
    it_hdum-wo_serial  = it_um-wo_serial.
    it_hdum-wo_nation  = it_um-wo_nation.
    it_hdum-wo_dealer  = it_um-wo_dealer.
    it_hdum-wo_extc    = it_um-wo_extc.
    it_hdum-wo_intc    = it_um-wo_intc.
    it_hdum-intno      = it_um-intno.

    COLLECT it_hdum.

  ENDLOOP.


  DESCRIBE TABLE it_hdum LINES l_size.

  IF l_size > 0.
    SORT it_um
      BY wo_serial wo_nation wo_dealer wo_extc wo_intc zvin
         status DESCENDING.
  ENDIF.
ENDFORM.                    "p2000_get_data

*---------------------------------------------------------------------*
*       FORM P2100_MOD_DATA                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM p2100_mod_data.
  DATA : lt_wosum LIKE TABLE OF ztpp_wosum WITH HEADER LINE,
         lt_vm    LIKE TABLE OF zvpp_vm     WITH HEADER LINE,
         lt_rpid  LIKE TABLE OF ztpp_rpid   WITH HEADER LINE,
         lt_posr  LIKE TABLE OF ztpp_input_osr WITH HEADER LINE.

  DATA : BEGIN OF it_osr_lpdd OCCURS 0,
        hkmc  LIKE ztsd_osr-hkmc,
        nation  LIKE ztsd_osr-nation,
        zvin  LIKE ztsd_osr-zvin,
        lpdd_d  LIKE ztsd_osr-lpdd_d,
      END OF it_osr_lpdd.

  DATA : i_index TYPE i.
  DATA : l_index(2) TYPE n.
  DATA : l_stsum TYPE i.
  DATA : l_field(30).
  DATA : d_field(30).
  DATA : i_date TYPE d.
  DATA : i_cnt TYPE i VALUE 1.
  DATA:  l_dt07 LIKE sy-datum.

  FIELD-SYMBOLS : <bukt>,
                  <value>,
                  <date>.

  CHECK NOT it_hdum[] IS INITIAL.

*# Req. Andy Choi 11/12/2010
*#0. Get Work Sum Data
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_WOSUM
*  FROM ZTPP_WOSUM
*  FOR ALL ENTRIES IN IT_HDUM
*  WHERE WO_SER = IT_HDUM-WO_SERIAL
*    AND NATION = IT_HDUM-WO_NATION
*    AND DEALER = IT_HDUM-WO_DEALER
*    AND EXTC   = IT_HDUM-WO_EXTC
*    AND INTC   = IT_HDUM-WO_INTC    .
*
**#1. Get Work Order Header, Color
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_WOHD
*    FROM MARA
*    FOR ALL ENTRIES IN IT_HDUM
*    WHERE MATNR = IT_HDUM-WOHD.
*
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_WOCL
*    FROM MARA
*    FOR ALL ENTRIES IN IT_HDUM
*    WHERE MATNR = IT_HDUM-WOCL.

*#2. Get Vehicle Master - UM Data

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_vm
      FROM zvpp_vm
      FOR ALL ENTRIES IN it_um
      WHERE model_code  = it_um-model_code
        AND body_no     = it_um-body_no.

  SORT lt_vm BY model_code body_no.

  RANGES : r_woser FOR ztpp_input_osr-work_order .
  r_woser-option = 'EQ'.
  r_woser-sign   = 'I'.

  LOOP AT it_um .
    CONCATENATE it_um-wo_serial it_um-wo_nation it_um-wo_dealer
      INTO r_woser-low.
    APPEND r_woser.
  ENDLOOP.
  PERFORM p1000_start_progressbar USING  'Check Input Plan...' '50'.
*-Input plan every 4 hrs data
  DELETE ADJACENT DUPLICATES FROM r_woser.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_posr
    FROM ztpp_input_osr
      WHERE work_order IN r_woser.
  .
  PERFORM modify_input_osr TABLES lt_posr it_body it_body_n.
  PERFORM p1000_start_progressbar USING 'Check Input Plan.... ' '70'.
  IF lt_posr[] IS INITIAL.
    MESSAGE s002 WITH 'No Input Plan.'
                      'Please run "Fill Input Plan" first'.

  ELSE.

*#2.1 GET RPID Information.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_rpid
    FROM ztpp_rpid
    FOR ALL ENTRIES IN it_um
    WHERE model_code  =  it_um-model_code
      AND body_no     =  it_um-body_no .

  ENDIF.

*->
  PERFORM p1000_start_progressbar USING 'Check RPID..' '80'.
**--<main process
  SORT : lt_rpid BY model_code body_no,
         lt_vm   BY model_code body_no,
         it_hdum BY wo_serial wo_nation wo_dealer wo_extc wo_intc .
  PERFORM p1000_start_progressbar USING 'Create OSR Data.. ' '80'.

  LOOP AT it_um.

    READ TABLE lt_rpid WITH KEY  model_code =  it_um-model_code
                                 body_no = it_um-body_no
                                 BINARY SEARCH.
    IF sy-subrc <> 0 . CLEAR lt_rpid . ENDIF.

    READ TABLE lt_vm   WITH KEY model_code  = it_um-model_code
                                body_no     = it_um-body_no
                                BINARY SEARCH.
    IF sy-subrc = 0.
      gv_model   = lt_vm-model_code.
      gv_body    = lt_vm-body_no.
      gv_cstatus = lt_vm-rp_cstatus.
    ELSE.
      CLEAR: gv_model, gv_body, gv_cstatus,
             lt_vm.
    ENDIF.


    READ TABLE it_hdum WITH KEY   wo_serial = it_um-wo_serial
                                  wo_nation = it_um-wo_nation
                                  wo_dealer = it_um-wo_dealer
                                  wo_extc   = it_um-wo_extc
                                  wo_intc   = it_um-wo_intc
                                  BINARY SEARCH.

    IF sy-subrc <> 0 . CLEAR it_hdum. ENDIF.
    PERFORM fill_it_osr USING it_um lt_vm lt_rpid . "LT_WOSUM.

  ENDLOOP.
**-->
  PERFORM p1000_start_progressbar USING 'Created OSR Data...' '100'.

*-< decide estimated body-in date

*--< Victor 08.18.2011    for sequence dt06/dt07
  LOOP AT it_osr.
*    CHECK it_osr-plan_date = '00000000' OR
*          it_osr-urgcdate  = '00000000' OR
*          it_osr-dealer_dt = '00000000' .
*          it_osr-urgseq    IS INITIAL.

    IF it_osr-plan_date = '00000000'.
      it_osr-plan_date  = '99991231'.
    ENDIF.
    IF it_osr-urgcdate = '00000000'.
      it_osr-urgcdate  = '99991231'.
    ENDIF.

    IF it_osr-dealer_dt =  '00000000'.
      it_osr-dealer_dt  = '99991231'.
    ENDIF.

    IF it_osr-dler      = ''.
      it_osr-dealer_yn  = ''.
    ELSE.
      it_osr-dealer_yn  = 'X'.
    ENDIF.

    MODIFY it_osr.
  ENDLOOP.

*--<06.17.2014 Victor :B28->sort by Zvin, Not B28->sort by old logic
*  SORT it_osr BY wkno    wo_nation   wo_dealer
*                 wo_extc wo_intc     vin
*                 urgseq  urgcdate
**                 dler        DESCENDING
*                 dealer_yn   DESCENDING   "Dealer Exist?
*                 dealer_dt   ASCENDING
*                 plan_date   ASCENDING
*                 zvin        ASCENDING.

  LOOP AT it_osr.
    IF it_osr-wo_nation  = 'B28'.
      lt_b28 =  it_osr.
      APPEND lt_b28. CLEAR lt_b28.
    ELSE.
      lt_oth_natn =  it_osr.
      APPEND lt_oth_natn. CLEAR lt_oth_natn.
    ENDIF.
  ENDLOOP.

  CLEAR : it_osr[], it_osr.
  SORT lt_b28 BY wkno wo_nation wo_dealer wo_extc wo_intc zvin.
  SORT lt_oth_natn BY wkno wo_nation wo_dealer wo_extc wo_intc
                      vin  urgseq  urgcdate
                      dealer_yn   DESCENDING   "Dealer Exist?
                      dealer_dt   ASCENDING
                      plan_date   ASCENDING
                      zvin        ASCENDING.

  APPEND LINES OF lt_b28      TO it_osr.
  APPEND LINES OF lt_oth_natn TO it_osr.
*-->


*-->

  it_input_osr[] =  lt_posr[].

  SORT it_input_osr BY modl body_ser seq_date seq_serial.

*--<10.01.2010 New mapping
*# Sorting by priority

  DATA : lv_word LIKE ztpp_input_osr-work_order.

*# case have body number - first priority body no.
  SORT : it_body   BY "WORK_ORDER
                      "EXTC
                      "INTC
                      modl
                      body_ser seq_date seq_serial.
*-dt07 # Body Created Date
  LOOP AT it_osr WHERE ( body_no <> ''
                   AND   body_no <> '000000' ) .

** Fuorng on 03/13/12 for fix the error of DT07
** no data exist in ztpp_input_osr table if the vehicle was signed off
*    CONCATENATE it_osr-wkno it_osr-wo_nation it_osr-wo_dealer
*      INTO lv_word.
*
*    READ TABLE it_body WITH KEY "WORK_ORDER = LV_WORD "IT_OSR-WKNO
*                                "EXTC      = IT_OSR-WO_EXTC
*                                "INTC      = IT_OSR-WO_INTC
*                                modl      = it_osr-model_code
*                                body_ser  = it_osr-body_no
*                                BINARY SEARCH.
*    IF sy-subrc = 0.
*      i_index   = sy-tabix.
*      IF  it_osr-dt08 IS INITIAL.
*        it_osr-st07 = '1'.
*      ENDIF.
*      it_osr-dt07 = it_body-seq_date.
*      MODIFY it_osr.
*    ENDIF.

    CLEAR: l_dt07.
    SELECT SINGLE seq_date INTO l_dt07
     FROM ztpp_vm
     WHERE model_code = it_osr-model_code
       AND body_no = it_osr-body_no.

    it_osr-dt07 = l_dt07.

    IF it_osr-dt08 IS INITIAL.
      it_osr-st07 = '1'.
    ENDIF.

    MODIFY it_osr.
  ENDLOOP.
** End on 03/13/12

  CLEAR : lv_word.

*-dt06 #
*# case don't have body number - first priority seq_date , seq_seirial

** Furong on 05/16/12 for tuning
*  SORT IT_BODY_N BY WORK_ORDER EXTC INTC SEQ_DATE SEQ_SERIAL.

  SORT it_body_n BY work_order extc intc rp01 seq_date seq_serial.

  DATA : lt_tabix1     LIKE sy-tabix.        "Addition
  CLEAR : lt_tabix1.
** End on 05/16/12

  PERFORM p1000_start_progressbar USING 'Create DT06, DT07.....' '10'.

  LOOP AT it_osr WHERE ( dt08 IS INITIAL OR dt08 EQ '00000000' )
                       AND status IS INITIAL "Only Created Car
                       AND ( body_no IS INITIAL OR
                             body_no EQ '000000' ).

** Furong on 05/16/12 for tuning
*    CONCATENATE IT_OSR-WKNO IT_OSR-WO_NATION IT_OSR-WO_DEALER
*      INTO LV_WORD.
*
*    READ TABLE IT_BODY_N WITH KEY WORK_ORDER = LV_WORD "IT_OSR-WKNO
*                                  RP01       = ''
*                                  EXTC       = IT_OSR-WO_EXTC
*                                  INTC       = IT_OSR-WO_INTC
*                                  CONF       = ''.
*
*    IF SY-SUBRC = 0.
*      I_INDEX   = SY-TABIX.
*
*      IT_OSR-ST06 = '1'.
*      IT_OSR-DT06 = IT_BODY_N-RD01.
*      MODIFY IT_OSR.
*
*      IT_BODY_N-CONF = 'X'.
*      MODIFY IT_BODY_N INDEX I_INDEX TRANSPORTING CONF.
*    ENDIF.


** End


    CONCATENATE it_osr-wkno it_osr-wo_nation it_osr-wo_dealer
      INTO lv_word.

    i_index   = sy-tabix.    "Move

    READ TABLE it_body_n WITH KEY work_order = lv_word "IT_OSR-WKNO
                                  extc       = it_osr-wo_extc
                                  intc       = it_osr-wo_intc
                                  rp01       = ''
                                  BINARY SEARCH.    "Addition
    IF sy-subrc = 0.
      LOOP AT it_body_n FROM sy-tabix.
        IF it_body_n-work_order <> lv_word         OR
           it_body_n-extc       <> it_osr-wo_extc  OR
           it_body_n-intc       <> it_osr-wo_intc  OR
           it_body_n-rp01       <> ''.
          EXIT.
        ENDIF.
        IF it_body_n-conf  = ''.
          lt_tabix1   = sy-tabix.

          it_osr-st06 = '1'.
          it_osr-dt06 = it_body_n-rd01.
          MODIFY it_osr INDEX i_index.

          it_body_n-conf = 'X'.
          MODIFY it_body_n INDEX lt_tabix1 TRANSPORTING conf.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
** End on 05/16/12
  ENDLOOP.
*-->

*-< added on 02.07.2014 Victor
  SELECT hkmc nation zvin lpdd_d
  INTO CORRESPONDING FIELDS OF TABLE it_osr_lpdd
  FROM ztsd_osr
    FOR ALL ENTRIES IN it_osr
  WHERE hkmc  = 'HMG'    "length is different
    AND nation = it_osr-wo_nation
    AND zvin   = it_osr-zvin.

  SORT it_osr_lpdd BY hkmc nation zvin.
*->

  LOOP AT it_osr.
*X-< IF ST08~ST21 values is not empty,
*-< IF ST08~ST15 values is not empty,
*   delete st01,st06,st07,dt06,dt07
*# HMMA End ST15
    CLEAR : i_index, l_index, l_stsum, it_osr_lpdd.

*-  Update Est.Sign-off date change flag  02.07.2014
    READ TABLE it_osr_lpdd WITH KEY hkmc   = it_osr-hkmc
                                    nation = it_osr-wo_nation
                                    zvin   = it_osr-zvin
                                    BINARY SEARCH.
    IF sy-subrc = 0.
      it_osr-lpdd_d =  it_osr_lpdd-lpdd_d.
    ENDIF.

    i_index = 7.
    DO 8 TIMES.
      l_index = i_index + sy-index.
      CHECK l_index NE '00'.
      CONCATENATE 'IT_OSR-ST'     l_index INTO l_field.

      ASSIGN (l_field)   TO <value>.

      l_stsum = l_stsum + <value>.
    ENDDO.

    IF l_stsum > 0.
      it_osr-st01 = 0.
      it_osr-st06 = 0.
      it_osr-st07 = 0.
      it_osr-dt06 = ''.
    ENDIF.
*->

*-< IF ST06~ST21 values is empty, then input '1' into st01
*-< IF ST06~ST15 values is empty, then input '1' into st01
*# HMMA End ST15
    CLEAR : i_index, l_index, l_stsum.
    i_index = 5. " already st06... setting why?

    DO 10 TIMES.
      l_index = i_index + sy-index.
      CHECK l_index NE '00'.
      CONCATENATE 'IT_OSR-ST'     l_index INTO l_field.
      ASSIGN (l_field)   TO <value>.
      l_stsum = l_stsum + <value>.
    ENDDO.

    IF l_stsum > 0.
      it_osr-st01 = 0.
    ELSE.
      it_osr-st01 = 1.
    ENDIF.

*--< Victor 08.18.2011
    IF it_osr-plan_date = '99991231' OR
          it_osr-urgcdate  = '99991231' OR
          it_osr-dealer_dt = '99991231' OR
          it_osr-urgseq    = '~'.

      IF it_osr-plan_date = '99991231'.
        it_osr-plan_date  = '00000000'.
      ENDIF.
      IF it_osr-urgcdate = '99991231'.
        it_osr-urgcdate  = '00000000'.
      ENDIF.
      IF it_osr-dealer_dt =  '99991231'.
        it_osr-dealer_dt  = '00000000'.
      ENDIF.
      IF it_osr-urgseq    = '~'.
        it_osr-urgseq    = ''.
      ENDIF.
    ENDIF.
*-->

    MODIFY it_osr.
*->
  ENDLOOP.
  PERFORM p1000_start_progressbar USING 'The End Process..' '100'.
ENDFORM.                    "p2100_mod_data

*&---------------------------------------------------------------------*
*&      Form  MODIFY_INPUTPL_OSR
*&---------------------------------------------------------------------*
*       delete useless data :vehicle which passed rp01 -> delete
*----------------------------------------------------------------------*
FORM modify_input_osr TABLES p_posr   STRUCTURE ztpp_input_osr
                             p_body   STRUCTURE ztpp_input_osr
                             p_body_n STRUCTURE ztpp_input_osr .

*-10.01.2010

  SORT p_posr BY modl body_ser.

*? Q1. Body In InputPlan = PBDAT is not exist in HMMA System.
  DELETE p_posr WHERE seq_date = '00000000'.

*-Seperate data : body exist-> dt07,   No body -> dt06
  CLEAR : p_body[], p_body_n[].
  LOOP AT p_posr.
    IF NOT p_posr-body_ser IS INITIAL.
      MOVE-CORRESPONDING  p_posr TO p_body.
      APPEND p_body.
    ELSE.
      MOVE-CORRESPONDING  p_posr TO p_body_n.
      APPEND p_body_n.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_INPUTPL_OSR

*---------------------------------------------------------------------*
*       FORM FILL_IT_OSR_UMG                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fill_it_osr USING p_um    STRUCTURE ztsd_um
                       p_vm    STRUCTURE zvpp_vm
                       p_rpid  STRUCTURE ztpp_rpid.
  " P_WOSUM STRUCTURE ZTPP_WOSUM.

  DATA: p_field(15)  TYPE c,
        r_field(20)  TYPE c,
        s_cstatus(2) TYPE c,
        r_stat(1)    TYPE x,
        l_sdate(2),
        l_year(1) TYPE c,
        l_mont(1) TYPE c,
        p_count TYPE i,
        urgency LIKE p_um-urgency,
        urgcdate LIKE p_um-urgcdate,
        dldate  LIKE p_um-dealer_dt.

*# OSR Send Data
  CLEAR : it_osr.

*# OSR Send Data - HKMC, DISTCO

  it_osr-hkmc        = 'HMG'.
*# Maybe Select AUSP.
  CONCATENATE p_um-wo_nation p_um-wo_dealer INTO it_osr-distco.
*  it_osr-distco      = p_um-wo_dealer ."P_VM-DEST_CODE.


*# OSR Send Data - ZVIN , UORD
  it_osr-zvin = p_um-zvin.

  CONCATENATE p_um-wo_serial  p_um-wo_nation
* 01242011. Req Wonchol Lee +
* P_UM-WO_DEALER(2)
* 01242011                  -
    INTO it_osr-uord.
  it_osr-uord+12(6)  = p_um-intno.

*#  IT_OSR-DIS2      = P_WOCL-DEST_CODE. "??
*# OSR Send Data - PLAN_DATE, URGENCY, STATUS, CNCL, CHDT
  it_osr-plan_date = p_um-plan_date.
  it_osr-urgency   = p_um-urgency.
*  IT_OSR-STATUS    = P_UM-STATUS.
  it_osr-cncl        = ''. "CANCLE FLAG
*# Req. Andy Choi 11/12/2010
  it_osr-model_code = p_um-model_code."P_WOSUM-FSC+5(9).
*  IT_OSR-OCCN       = P_WOSUM-FSC+14(4).
*  IT_OSR-IVNB       =
  it_osr-refe       = p_um-oser.
  it_osr-wkno       = p_um-wo_serial.
  it_osr-carn       = p_um-carno.


**# OSR Send Data - WKNO
*  CONCATENATE
*    P_UM-WO_SERIAL
*    P_UM-WO_NATION
*    P_UM-WO_DEALER(2)
*  INTO IT_OSR-WKNO.

*# OSR Send Data - PACK ex ) 201007
  CONCATENATE
  '20'
  p_um-wo_serial+1(2)
  p_um-wo_serial+3(2) INTO  it_osr-pack.

* Appended 08.06.2009 requested by KMA
  CONCATENATE
  sy-datum sy-uzeit+0(2) INTO it_osr-chdt.

*  CONCATENATE SY-DATUM SY-UZEIT+0(2) INTO ZHR.

*# OSR Send Data - WO_EXTC, WO_INTC, PORT
  it_osr-wo_extc = p_um-wo_extc.
  it_osr-wo_intc = p_um-wo_intc.
  it_osr-port    = p_um-wo_serial+5(1).

*# OSR Send Data - STXX, ESXX.
  CLEAR :
   it_osr-st01,
   it_osr-st06,"
   it_osr-st07,
   it_osr-st08,
   it_osr-st09,
   it_osr-st10,
   it_osr-st11,
   it_osr-st12,
   it_osr-st13,
   it_osr-st14,"
   it_osr-st15."

  it_osr-st01 =
  it_osr-st06 =
  it_osr-st07 =
  it_osr-st08 =
  it_osr-st09 =
  it_osr-st10 =
  it_osr-st11 =
  it_osr-st12 =
  it_osr-st13 =
  it_osr-st14 =
  it_osr-st15 = 0 ."

  DATA : lv_field(100) .
  CLEAR : lv_field.
  FIELD-SYMBOLS : <f_status>.
  PERFORM get_rpstatus USING p_vm-rp_cstatus lv_field p_um-wo_nation.

  IF NOT lv_field IS INITIAL.
    CONCATENATE 'IT_OSR-' lv_field INTO lv_field.

    ASSIGN (lv_field) TO <f_status>.
    <f_status> = 1.
  ENDIF.

  it_osr-es08 =
  it_osr-es09 =
  it_osr-es10 =
  it_osr-es11 =
  it_osr-es12 =
  it_osr-es13 =
  it_osr-es14 =
  it_osr-es15 = 'Y'.

** Changed on 02/01/13
* it_osr-dt01 = p_um-aedat.
  it_osr-dt01 = p_um-erdat.
** End on 02/01/13

  it_osr-dt08 = p_rpid-rp01_sdate.
  it_osr-dt09 = p_rpid-rp02_sdate.
** On 03/12/13
*  it_osr-dt10 = p_rpid-rp07_sdate.
  IF p_um-wo_nation = 'B28'.
    it_osr-dt10 = p_rpid-rp14_sdate.
  ELSE.
    it_osr-dt10 = p_rpid-rp07_sdate.
  ENDIF.
** End on 03/12/13
  it_osr-dt11 = p_rpid-rp18_sdate.
  it_osr-dt12 = p_rpid-rp22_sdate.
  it_osr-dt13 = p_rpid-rp23_sdate.
*  IT_OSR-DT14 = P_RPID-??? HOW ABOUT KMMG CASE ???
  it_osr-dt15 = p_rpid-rp25_sdate.

  IF it_osr-dt15 IS INITIAL.
    it_osr-dt15 = p_rpid-rp27_sdate.
  ENDIF.

  IF NOT gv_model IS INITIAL.
    it_osr-vin    = p_vm-vin.
    it_osr-eng_no = p_vm-eng_no.

    IF NOT p_vm-key_no IS INITIAL.
      it_osr-keyn = p_vm-key_no+1(5).
    ELSE.
      it_osr-keyn = '     '.
    ENDIF.
  ELSE.
    CLEAR : it_osr-vin , it_osr-eng_no, it_osr-keyn.
  ENDIF.

*--<
  it_osr-wo_dealer = p_um-wo_dealer.
  it_osr-wo_nation = p_um-wo_nation.
  it_osr-body_no   = p_um-body_no.


*# OSR Send Data - FLET" SELECT AUSP
  it_osr-flet = p_vm-fleet.
  IF it_osr-flet IS INITIAL.
    it_osr-flet = 'N'.
  ELSE.
*    IT_OSR-SLTP = ''.
  ENDIF.

  IF p_um-status EQ 'F'.
    it_osr-gubn = ' '.
  ELSE.
    it_osr-gubn = p_um-status.
  ENDIF.

  it_osr-crdt = sy-datum.
  it_osr-crtm = sy-uzeit.
*** not mandatory, not maintain **********************

  CONCATENATE p_um-model_code p_um-body_no
              INTO gv_body.
  it_osr-dler    = p_um-wo_dealer1.

  it_osr-urgency   = p_um-urgency.

  it_osr-urgcdate  = p_um-urgcdate.
  it_osr-dealer_dt = p_um-dealer_dt.

  it_osr-urgseq =  it_osr-urgency.
  IF it_osr-urgency EQ ''.
    it_osr-urgseq = '~'.        "Victor 08.18.2011 'Z' -> '~'
  ENDIF.
  IF it_osr-urgency EQ 'C' AND
     it_osr-urgcdate > sy-datum.
    it_osr-urgseq = '~'.
  ENDIF.
  APPEND it_osr.
  CLEAR : it_osr.

ENDFORM.                    " fill_IT_OSR_UM
*---------------------------------------------------------------------*
*       FORM send_data                                                 *
*---------------------------------------------------------------------*
*       ........                                                       *
*---------------------------------------------------------------------*
FORM send_data.
  DATA : lt_edidd LIKE TABLE OF edidd WITH HEADER LINE,
         lt_edidc LIKE TABLE OF edidc.
  DATA : l_plan_date TYPE sy-datum,
         l_wo_nation LIKE ztsd_um-wo_nation.

  DATA : l_extc LIKE it_osr-wo_extc,
         l_intc LIKE it_osr-wo_intc.

  CLEAR : it_osr_if[]. "it_osr_if_hma[], it_osr_if_hac[].

  SORT it_osr BY wo_nation.

  LOOP AT it_osr.

*-< Victor 03.04.2014
*    IF it_osr-wo_nation <> l_wo_nation AND l_wo_nation IS NOT INITIAL.
*      PERFORM send_ale TABLES it_osr_if.
*
*      CLEAR: it_osr_if[], it_osr_if.
*    ENDIF.
*->

*----->> 01.22.2013 BSBAE. ZTSD_UM PLAN_DATE Update
*        ZTSD_UM Table update condition should be original color
    CLEAR: l_extc, l_intc.
    l_extc = it_osr-wo_extc.
    l_intc = it_osr-wo_intc.
*-----<< 01.22.2013 BSBAE. ZTSD_UM PLAN_DATE Update

    IF it_osr-wo_nation <> 'B28'.
      PERFORM pro_convert_color.
    ENDIF.

    MOVE-CORRESPONDING it_osr TO it_osr_if.

*    IF it_osr-wo_nation = 'B28'.
*      MOVE-CORRESPONDING it_osr TO it_osr_if_hma.
*      APPEND it_osr_if_hma.
*    ELSEIF it_osr-wo_nation = 'B06'.
*      PERFORM pro_convert_color.
*      MOVE-CORRESPONDING it_osr TO it_osr_if_hac.
*      APPEND it_osr_if_hac.
*    ENDIF.

**--<  update ztsd_um by Victor 08.18.2011
    IF NOT it_osr-dt06 IS INITIAL OR NOT it_osr-dt07 IS INITIAL.
      CLEAR : l_plan_date.

      IF it_osr-dt07 IS INITIAL .
        l_plan_date = it_osr-dt06.
      ELSE.
        l_plan_date = it_osr-dt07.
      ENDIF.

      UPDATE ztsd_um
      SET  plan_date  = l_plan_date      "Previous Allocation Plan Date
      WHERE wo_serial = it_osr-wkno
        AND wo_nation = it_osr-wo_nation
        AND wo_dealer = it_osr-wo_dealer
*----->> 01.22.2013 BSBAE. ZTSD_UM PLAN_DATE Update
*        AND wo_extc   = it_osr-wo_extc
*        AND wo_intc   = it_osr-wo_intc
        AND wo_extc   = l_extc
        AND wo_intc   = l_intc
*-----<< 01.22.2013 BSBAE. ZTSD_UM PLAN_DATE Update
        AND zvin      = it_osr-zvin
        AND status    = ''.
    ENDIF.
**-->

    APPEND it_osr_if.

    AT END OF wo_nation.
      PERFORM send_ale TABLES it_osr_if.

      CLEAR: it_osr_if[], it_osr_if.
    ENDAT.

*    l_wo_nation  = it_osr-wo_nation.

    CLEAR:  lt_edidd.
  ENDLOOP.

*-- Send HMA
*  IF p_hma  = 'X'.
*    IF it_osr_if_hma[] IS NOT INITIAL.
*      g_company = 'HMA'.
*      PERFORM send_ale TABLES it_osr_if_hma.
*    ENDIF.
*  ENDIF.
*
*
**--Send HAC
*  IF p_hac  = 'X'.
*    IF it_osr_if_hac[] IS NOT INITIAL.
*      g_company = 'HAC'.
*      PERFORM send_ale TABLES it_osr_if_hac.
*    ENDIF.
*  ENDIF.

*--Send to GLV
*  IF p_glv  = 'X'.

*    LOOP AT it_osr_if_hma.
*      IF it_osr_if_hma-vin IS INITIAL.
*        CONTINUE.
*      ELSE.
*        MOVE-CORRESPONDING it_osr_if_hma TO it_osr_if_glv.
*        APPEND it_osr_if_glv.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT it_osr_if_hac.
*      IF it_osr_if_hac-vin IS INITIAL.
*        CONTINUE.
*      ELSE.
*        MOVE-CORRESPONDING it_osr_if_hac TO it_osr_if_glv.
*        APPEND it_osr_if_glv.
*      ENDIF.
*    ENDLOOP.

  LOOP AT it_osr.
    IF it_osr-vin IS INITIAL.
      CONTINUE.
    ELSE.
      MOVE-CORRESPONDING it_osr TO it_osr_if_glv.
      APPEND it_osr_if_glv.
    ENDIF.
  ENDLOOP.

  IF it_osr_if_glv[] IS NOT INITIAL.
    PERFORM send_eai_glovis TABLES it_osr_if_glv.
  ENDIF.
*  ENDIF.

  COMMIT WORK.
ENDFORM.                    "send_data

*---------------------------------------------------------------------*
*       FORM P3100_GENERATE_CONTROL_RECORD                            *
*---------------------------------------------------------------------*
FORM p3100_generate_control_record TABLES p_edidc STRUCTURE edidc .

  DATA : wa_edp13 TYPE edp13 ,
         wa_edidc LIKE p_edidc.

*  CLEAR : g_cond[].



*  IF   g_company = 'HMA'.
*    CONCATENATE 'AND' ' WO_NATION = ''' 'B28' ''''
*     INTO g_cond-text.
*    APPEND g_cond.
*  ELSEIF g_company = 'HAC'.
*    CONCATENATE 'AND' ' WO_NATION = ''' 'B06' ''''
*     INTO g_cond-text.
*    APPEND g_cond.
*  ENDIF.

*  READ TABLE g_cond INDEX 1.
*  MOVE '   ' TO g_cond-text(3).
*  MODIFY g_cond INDEX 1.

  SELECT SINGLE * INTO  wa_edp13
           FROM edp13 WHERE mestyp = c_mestyp.

  wa_edidc-mestyp =  wa_edp13-mestyp . "Message type
  wa_edidc-idoctp =  wa_edp13-idoctyp ."Basic IDOC type
  wa_edidc-rcvprt =  wa_edp13-rcvprt. "Partner type of receiver 'LS'

*  CLEAR : ztsd_ale_dest.
*  SELECT SINGLE *
*  FROM ztsd_ale_dest
*  WHERE (g_cond).

  READ TABLE it_ale_dest WITH KEY wo_nation = it_osr_if-wo_nation. "??
  IF sy-subrc = 0.
    wa_edidc-rcvprn =  it_ale_dest-rcvprn . "Partner number of recei
    wa_edidc-rcvpor =  it_ale_dest-rcvpor . "Receiver Port
*  wa_edidc-sndprn =  ztsd_ale_dest-sndprn. "Sender Partner Numb ->ERR
    APPEND wa_edidc TO p_edidc.
  ELSE.
*    MESSAGE s003 WITH g_company
*                      ':I/F NOT exist in ALE Mapping Master Table'.
    MESSAGE s003 WITH it_osr_if-wo_nation
                      ':NOT exist in ALE Mapping Master Table'.
  ENDIF.
ENDFORM.                    " generate_control_record

*---------------------------------------------------------------------*
*       FORM P3000_SEND_IDOC                                          *
*---------------------------------------------------------------------*
FORM p3000_send_idoc TABLES p_edidd STRUCTURE edidd
                            p_edidc STRUCTURE edidc.

  CLEAR : gt_item.

  DATA : l_idoc_ctrl LIKE TABLE OF edidc WITH HEADER LINE,
         lv_sndprn LIKE edidc-sndprn.

  PERFORM  p3100_generate_control_record TABLES l_idoc_ctrl.

  LOOP AT l_idoc_ctrl.

    CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
      EXPORTING
        master_idoc_control            = l_idoc_ctrl
      TABLES
        communication_idoc_control     = p_edidc
        master_idoc_data               = p_edidd
      EXCEPTIONS
        error_in_idoc_control          = 1
        error_writing_idoc_status      = 2
        error_in_idoc_data             = 3
        sending_logical_system_unknown = 4
        OTHERS                         = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      gt_item-status = '@02@'.

      PERFORM save_log_um TABLES it_osr  USING 'E'
        'Data processing error : ALE'.
      MESSAGE s002 WITH 'Data processing error: ' sy-msgv1.

    ELSE.
      DATA : message(100), lv_datum(100).
      gt_item-status = '@01@'.
      COMMIT WORK.
      CALL FUNCTION 'DEQUEUE_ALL'.    "Empty Queue->send Idoc

      PERFORM save_log_um  TABLES it_osr_if USING 'S' ''.
*      IF g_company = 'HMA'.
*        PERFORM save_log_um  TABLES it_osr_if_hma  USING 'S' ''.
*      ELSEIF g_company = 'HAC'.
*        PERFORM save_log_um  TABLES it_osr_if_hac  USING 'S' ''.
*      ENDIF.

      MESSAGE s003 WITH 'IDoc number' p_edidc-docnum 'was created'.
    ENDIF.

    MODIFY gt_item TRANSPORTING status WHERE status IS INITIAL .
  ENDLOOP.

*  CLEAR : g_company.  "CLEAR is Very improtant

ENDFORM.                    "p3000_send_idoc

*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM_1
*&---------------------------------------------------------------------*
FORM popup_to_confirm USING p_default
                            p_title
                            p_text1
                            p_text2
                            p_display
                      CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = p_default
      textline1      = p_text1
      textline2      = p_text2
      titel          = p_title
      cancel_display = p_display
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found.

ENDFORM.                    " POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_KUNNR_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_INPUT_KUNNR  text
*----------------------------------------------------------------------*
FORM conversion_output  USING    p_value.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_value
    IMPORTING
      output = p_value.


ENDFORM.                    " CONVERSION_KUNNR_INPUT
*&---------------------------------------------------------------------*
*&      Form  CALL_INPUT_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_input_plan.
  SUBMIT zrsd_hma_osr_iplan
     WITH p_log EQ ''
     AND RETURN.

ENDFORM.                    " CALL_INPUT_PLAN
*&---------------------------------------------------------------------*
*&      Form  CALL_VEHC_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_vehc_plan.
  SUBMIT zrsd_hma_osr_vplan
    WITH p_log EQ ''
     AND RETURN.
ENDFORM.                    " CALL_VEHC_PLAN

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG_UM
*&---------------------------------------------------------------------*
FORM save_log_um    TABLES pt_osr STRUCTURE it_osr
                     USING p_type p_msg.
  DATA : g_idx(10) TYPE n.

  SELECT zseq INTO g_idx
    FROM ztsd_osr_log
    UP TO 1 ROWS
   WHERE zdate = sy-datum
   ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT pt_osr.
    CLEAR ztsd_osr_log.
    MOVE-CORRESPONDING pt_osr TO ztsd_osr_log.
    ztsd_osr_log-zdate = sy-datum.
    ztsd_osr_log-zseq  = sy-tabix + g_idx.
    ztsd_osr_log-zrslt = p_type.
    ztsd_osr_log-zmsg  = p_msg.
*    ZTSD_OSR_LOG-CRTM  = SY-UZEIT. "added by Victor 05/14/2010
    MODIFY ztsd_osr_log.

** on 03/16/12
    it_ztsd_osr_log = ztsd_osr_log.
    APPEND it_ztsd_osr_log.
** end on 03/16/12

    CLEAR ztsd_osr .
    MOVE-CORRESPONDING pt_osr TO ztsd_osr.
    ztsd_osr-nation = pt_osr-wo_nation.
*    ztsd_osr-dealer = pt_osr-wo_dealer.
    ztsd_osr-ldate  = sy-datum.
    ztsd_osr-lzeit  = sy-uzeit.

** on 04/19/12 for clear indicator of sginoff change
    CLEAR: ztsd_osr-lpdd_d.
** end on 04/19/12

*    IF p_miss <> 'X'.    "Victor 09.02.2011
    MODIFY ztsd_osr.
*    ENDIF.

  ENDLOOP.
  COMMIT WORK.

ENDFORM.                    " SAVE_LOG_UM


*&---------------------------------------------------------------------*
*&      Form  CALL_LOG_PRG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_log_prg.

  DATA: ls_seltab TYPE STANDARD TABLE OF rsparams WITH HEADER LINE.

  CLEAR ls_seltab .
  ls_seltab-selname  = 'S_WOSER'.
  ls_seltab-sign     = 'I'.
  ls_seltab-option   = 'EQ'.

  LOOP AT gt_item.
    ls_seltab-low      = gt_item-wkno.
    APPEND  ls_seltab.
  ENDLOOP.

  SORT ls_seltab BY low.
  DELETE ADJACENT DUPLICATES FROM ls_seltab.

  CLEAR  ls_seltab.
  ls_seltab-selname = 'S_DATUM'.
  ls_seltab-sign = 'I'.
  ls_seltab-option = 'EQ'.
  ls_seltab-low = sy-datum.

  APPEND ls_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'S_DOCNUM'.
  ls_seltab-sign = 'I'.
  ls_seltab-option = 'EQ'.
  ls_seltab-low = gv_docnum.

  APPEND ls_seltab.


  DATA : l_idoc_ctrl LIKE TABLE OF edidc WITH HEADER LINE,
         lv_sndprn LIKE edidc-sndprn.
  PERFORM  p3100_generate_control_record TABLES l_idoc_ctrl.

  READ TABLE l_idoc_ctrl INDEX 1.
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system = lv_sndprn.


  SUBMIT zrpp_hma_idoc_report
       WITH p_mstyp EQ c_mestyp "'ZSPEC_ORD_MST'
       WITH p_direct EQ '1'
       WITH p_rcvprn EQ l_idoc_ctrl-rcvprn "'NDECLNT850'
       WITH p_sndprn EQ lv_sndprn                           "'UQ1300'
       WITH SELECTION-TABLE ls_seltab
       AND RETURN.
*       VIA SELECTION-SCREEN AND RETURN.


ENDFORM.                    " CALL_LOG_PRG
*&---------------------------------------------------------------------*
*&      Form  P2200_GET_DELTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p2200_get_delta.
  DATA : lt_log LIKE TABLE OF ztsd_osr WITH HEADER LINE.
  DATA : lt_osr LIKE TABLE OF it_osr WITH HEADER LINE.

*LT_OSR[] = IT_OSR[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_log
  FROM ztsd_osr.

  SORT : lt_log BY hkmc nation zvin gubn DESCENDING ,
         it_osr BY zvin gubn DESCENDING .

*  DELETE ADJACENT DUPLICATES FROM lt_log
*         COMPARING hkmc nation dealer zvin.

  LOOP AT it_osr .

    READ TABLE lt_log WITH KEY hkmc   = 'HMG'
                               nation = it_osr-wo_nation
*                               dealer = it_osr-wo_dealer
                               zvin   = it_osr-zvin
                      BINARY SEARCH.
    IF sy-subrc <> 0 .
      APPEND it_osr TO lt_osr  .
      CONTINUE.
    ENDIF.

    CASE '1'.
      WHEN it_osr-st01.
        IF lt_log-st01 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
      WHEN it_osr-st06.
        IF lt_log-st06 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
      WHEN it_osr-st07.
        IF lt_log-st07 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
      WHEN it_osr-st08.
        IF lt_log-st08 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
      WHEN it_osr-st09.
        IF lt_log-st09 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
      WHEN it_osr-st10.
        IF lt_log-st10 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
      WHEN it_osr-st11.
        IF lt_log-st11 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
      WHEN it_osr-st12.
        IF lt_log-st12 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
      WHEN it_osr-st13.
        IF lt_log-st13 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
      WHEN it_osr-st14.
        IF lt_log-st14 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
      WHEN it_osr-st15.
        IF lt_log-st15 NE '1'.
          COLLECT it_osr INTO lt_osr .
          CONTINUE.
        ENDIF.
    ENDCASE.

    IF it_osr-vin NE lt_log-vin.
      APPEND it_osr TO lt_osr .
      CONTINUE.
    ENDIF.

    IF it_osr-gubn NE lt_log-gubn AND it_osr-gubn NE ''.
      APPEND it_osr TO lt_osr .
      CONTINUE.
    ENDIF.

    IF it_osr-dt06 NE lt_log-dt06 .
      APPEND it_osr TO lt_osr.
      CONTINUE.
    ENDIF.

** On 04/19/2012 Only send singoff date have been changed
    IF it_osr-lpdd_d = 'X'.
      APPEND it_osr TO lt_osr.
      CONTINUE.
    ENDIF.
** end

  ENDLOOP.

  it_osr[] = lt_osr[].
  it_osr_total[] = lt_osr[].

  SORT : it_osr BY zvin gubn DESCENDING,
         it_osr_total BY zvin gubn DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM it_osr COMPARING zvin.


ENDFORM.                    " P2200_GET_DELTA



*&---------------------------------------------------------------------*
*&      Form  GET_RPSTATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VM_RP_CSTATUS  text
*      -->P_LV_OSR_FILED  text
*----------------------------------------------------------------------*
FORM get_rpstatus USING    p_cstatus
                           p_field p_nation.


*# CS_RPSTATUS : HMA ZVIN STATUS
* ST06 : no body zvin
* ST07 : body seq
* ST08 : rp01
* ST09 : rp02, rp03, rp04, rp05, rp06
* ST10 : rp07, rp08, rp09, rp10,
*        rp11, rp12, rp13, rp14, rp15, rp16, rp17
* ST11 : rp18, rp19, rp20, rp21
* ST12 : rp22
* ST13 : rp23, rp24, 2p26
* ST15 : rp25, rp27.


  IF p_cstatus EQ '' OR p_cstatus IS INITIAL . EXIT. ENDIF.

  IF  p_cstatus EQ '27'
   OR p_cstatus EQ '25'.
    p_field = 'ST15' .EXIT.
  ENDIF.

  IF p_cstatus EQ '26'
   OR p_cstatus EQ '24'
   OR p_cstatus GE '23'.
    p_field = 'ST13' .EXIT.
  ENDIF.

  IF p_cstatus GE '22'. p_field = 'ST12' .EXIT.  ENDIF.

  IF p_cstatus GE '18'. p_field = 'ST11' .EXIT.  ENDIF.

** On 03/12/13
*  IF p_cstatus GE '07'. p_field = 'ST10' .EXIT.  ENDIF.
  IF p_nation = 'B28'.
    IF p_cstatus GE '14'. p_field = 'ST10' .EXIT.  ENDIF.
  ELSE.
    IF p_cstatus GE '07'. p_field = 'ST10' .EXIT.  ENDIF.
  ENDIF.
** End on 03/12/13

  IF p_cstatus GE '02'. p_field = 'ST09' .EXIT.  ENDIF.

  IF p_cstatus GE '01'. p_field = 'ST08' .EXIT.  ENDIF.

  IF p_cstatus GE '00'. p_field = 'ST07' .EXIT.  ENDIF.

  IF p_field IS INITIAL. p_field = 'ST01'.ENDIF.


ENDFORM.                    " GET_RPSTATUS

*---------------------------------------------------------------------*
*       FORM CREATE_SPEC_CHANGE                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_UM                                                          *
*  -->  P_VM                                                          *
*---------------------------------------------------------------------*
FORM create_open_po.
  DATA : lv_um LIKE ztsd_um ,
         p_osr LIKE it_osr_total.
  CLEAR: lv_um.

  LOOP AT it_osr_total INTO p_osr WHERE gubn EQ 'S'
                                     OR gubn EQ 'P'.
    READ TABLE it_osr WITH KEY zvin = p_osr-zvin
                               gubn = ''.
    IF sy-subrc <> 0 .
      lv_um-mandt     = sy-mandt.
      lv_um-wo_serial = p_osr-wkno.

      lv_um-wo_nation = p_osr-wo_nation.
      lv_um-wo_dealer = p_osr-wo_dealer.
      lv_um-wo_extc   = p_osr-wo_extc.
      lv_um-wo_intc   = p_osr-wo_intc.
      lv_um-zvin      = p_osr-zvin.

      SELECT MAX( intno ) INTO lv_um-intno
         FROM ztsd_um
         WHERE wo_serial = lv_um-wo_serial.

      lv_um-intno = lv_um-intno + 1.
      CLEAR : lv_um-status , lv_um-body_no.

      MOVE: sy-uname                  TO lv_um-aenam,
            sy-datum                  TO lv_um-aedat,
            sy-uzeit                  TO lv_um-aezet.

      INSERT INTO ztsd_um CLIENT SPECIFIED VALUES lv_um.
      IF sy-subrc <> 0 .
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CREATE_SPEC_CHANGE

*&---------------------------------------------------------------------*
*&      Form  check_program
*&---------------------------------------------------------------------*
FORM check_program.

*-Manual Send
  IF p_miss = 'X'.
    IF p_nation IS INITIAL.
      MESSAGE s003 WITH 'Nation is required' 'for specific ZVIN'.
      STOP.
    ENDIF.

    " If HMMA start Cross Production, Dealer is required
*    IF p_dealer IS INITIAL.
*      MESSAGE s003 WITH 'Dealer is required' 'for specific ZVIN'.
*      STOP.
*    ENDIF.

    IF  s_zvin[]   IS INITIAL.
      MESSAGE s003 WITH 'ZVIN is required' 'for specific ZVIN'.
      STOP.
    ENDIF.

*-Normal Case
  ELSE.
    IF s_nation[] IS INITIAL.
      MESSAGE s003 WITH 'Nation code is Mandatory'.
      STOP.
    ELSE.
      LOOP AT s_nation.
        READ TABLE it_ale_dest WITH KEY wo_nation = s_nation-low.
        IF sy-subrc <> 0.
          MESSAGE s003 WITH s_nation-low ':Added Destination does '
                            'Not Exist in ALE Mapping table'.
          STOP.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_program
*&---------------------------------------------------------------------*
*&      Form  SEND_ALE
*&---------------------------------------------------------------------*
FORM send_ale TABLES it_osr_if STRUCTURE it_osr.

  DATA : lt_edidd LIKE TABLE OF edidd WITH HEADER LINE,
         lt_edidc LIKE TABLE OF edidc,
         it_osr_tmp LIKE it_osr OCCURS 0 WITH HEADER LINE,
         wa_osr_sdata TYPE zosrseg.

  CLEAR : it_osr_tmp[], it_osr_tmp.

  LOOP AT it_osr_if.
    MOVE-CORRESPONDING it_osr_if TO it_osr_tmp.

    APPEND it_osr_tmp.
  ENDLOOP.


  LOOP AT it_osr_tmp.
    MOVE-CORRESPONDING it_osr_tmp TO wa_osr_sdata.
    lt_edidd-segnam = c_zosrseg.
    lt_edidd-sdata  = wa_osr_sdata.

    APPEND lt_edidd.  CLEAR: lt_edidd.
  ENDLOOP.

  PERFORM  p3000_send_idoc TABLES lt_edidd lt_edidc.

ENDFORM.                    " SEND_ALE
*&---------------------------------------------------------------------*
*&      Form  SEND_EAI_GLOVIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OSR_IF_GLV  text
*----------------------------------------------------------------------*
FORM send_eai_glovis TABLES pt_osr_eai.
  DATA g_ret(1).
  wa_dest = 'WMGM01'.
  CALL FUNCTION 'Z_FSD_IF_OSR_TO_GLOVIS' DESTINATION wa_dest
    IMPORTING
      zzret                 = g_ret
    TABLES
      it_osr                = pt_osr_eai
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF g_ret <> 'S'.
    PERFORM save_log_eai TABLES it_ztsd_osr_log  USING 'E'
      'Data processing error : EAI'.
    MESSAGE s003 WITH 'Error: sending to Glovis: ' l_msgtxt.
  ELSE.
    PERFORM save_log_eai  TABLES it_ztsd_osr_log  USING 'S' ''.
    MESSAGE s003 WITH 'Successfully sent Glovis'.
  ENDIF.
ENDFORM.                    " SEND_EAI_GLOVIS
*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG_EAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_OSR_EAI  text
*      -->P_2751   text
*      -->P_2752   text
*----------------------------------------------------------------------*
FORM save_log_eai  TABLES pt_osr STRUCTURE ztsd_osr_log
                   USING p_type p_msg.

** delelt older than 90 days
  w_del_date = sy-datum - 90.

  DELETE FROM ztsd_osr_log
  WHERE  zdate < w_del_date.

  LOOP AT pt_osr.
    UPDATE ztsd_osr_log SET zrslt_eai = p_type
                            zmsg_eai  = p_msg
           WHERE zdate = pt_osr-zdate
            AND zseq = pt_osr-zseq
            AND zvin = pt_osr-zvin.
  ENDLOOP.

ENDFORM.                    " SAVE_LOG_EAI
*&---------------------------------------------------------------------*
*&      Form  PRO_CONVERT_COLOR
*&---------------------------------------------------------------------*
FORM pro_convert_color .

  CONCATENATE it_osr-wkno      it_osr-wo_nation  it_osr-wo_dealer
              it_osr-wo_extc   it_osr-wo_intc
         INTO lv_objek.

  PERFORM pro_get_ausp USING lv_objek lv_model_year 'P_MODEL_YEAR'.

** Furong on 08/10/12
  PERFORM pro_get_ausp USING lv_objek lv_model 'P_MODEL'.
*  LV_MODEL  =  IT_OSR-MODEL_CODE+0(2).
  lv_model = lv_model+0(2).
** End

  CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
    EXPORTING
      i_model = lv_model
      i_year  = lv_model_year
      i_gubn  = ''            "HMMA -> HAC/HMM
      i_extc  = it_osr-wo_extc
      i_intc  = it_osr-wo_intc
    IMPORTING
      e_extc  = it_osr-wo_extc
      e_intc  = it_osr-wo_intc.

ENDFORM.                    " PRO_CONVERT_COLOR
*&---------------------------------------------------------------------*
*&      Form  PRO_GET_AUSP
*&---------------------------------------------------------------------*
FORM pro_get_ausp   USING p_objek p_atwrt p_atnam.

  DATA : lv_atinn LIKE ausp-atinn.
  PERFORM p4000_conversion_atinn USING p_atnam lv_atinn.

  SELECT SINGLE atwrt INTO p_atwrt
  FROM ausp
  WHERE klart = '001'
    AND atinn = lv_atinn
    AND objek = p_objek
    AND mafid EQ 'O'.

ENDFORM.                    " PRO_GET_AUSP
*&---------------------------------------------------------------------*
*&      Form  P4000_CONVERSION_ATINN
*&---------------------------------------------------------------------*
FORM p4000_conversion_atinn  USING p_value p_atinn .

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = p_value
    IMPORTING
      output = p_atinn.

ENDFORM.                    " P4000_CONVERSION_ATINN
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_SIGNOFF_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_signoff_date .
  DATA: lt_temp_osr LIKE TABLE OF ztsd_osr WITH HEADER LINE.

  DATA: lt_body LIKE ztpp_delay_veh OCCURS 0 WITH HEADER LINE.

  DATA: l_index LIKE sy-tabix.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_temp_osr
  FROM ztsd_osr.

  SORT lt_temp_osr BY nation zvin gubn DESCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_temp_osr COMPARING nation zvin.


  LOOP AT it_osr .
    l_index  = sy-tabix.
    READ TABLE lt_temp_osr WITH KEY nation = it_osr-wo_nation
                                    zvin = it_osr-zvin BINARY SEARCH.
    IF sy-subrc = 0 .
      it_osr-lpdd = lt_temp_osr-lpdd.
      MODIFY it_osr INDEX l_index TRANSPORTING lpdd.
    ENDIF.

    IF it_osr-body_no IS NOT INITIAL.
      MOVE: it_osr-model_code TO lt_body-model_code,
            it_osr-body_no    TO lt_body-body_no.
      APPEND lt_body.
    ENDIF.
  ENDLOOP.

  PERFORM get_est_soff_from_delay TABLES lt_body.

** on 04/19/12 assigned in ZPPR0009
*  DATA: L_INDEX LIKE SY-TABIX.
*  DATA: BEGIN OF LT_SIGNOFF OCCURS 0,
*       ZVIN LIKE ZTPP_SOFF_PLAN-ZVIN,
*       DATE_SOFF LIKE ZTPP_SOFF_PLAN-DATE_SOFF,
*       END OF LT_SIGNOFF.
*
*  SELECT ZVIN DATE_SOFF INTO TABLE LT_SIGNOFF
*   FROM ZTPP_SOFF_PLAN
*   FOR ALL ENTRIES IN IT_OSR
*   WHERE ZVIN  = IT_OSR-ZVIN.
*  SORT LT_SIGNOFF BY ZVIN.
*  LOOP AT IT_OSR.
*    L_INDEX  = SY-TABIX.
*    READ TABLE LT_SIGNOFF WITH KEY ZVIN = IT_OSR-ZVIN
*                          BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      IT_OSR-LPDD = LT_SIGNOFF-DATE_SOFF.
*      MODIFY IT_OSR INDEX L_INDEX TRANSPORTING LPDD.
*    ENDIF.
*  ENDLOOP.

** End
ENDFORM.                    " ASSIGN_SIGNOFF_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_EST_SOFF_FROM_DELAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_est_soff_from_delay TABLES pt_body STRUCTURE ztpp_delay_veh.
  DATA: lt_delay LIKE ztpp_delay_veh OCCURS 0 WITH HEADER LINE.
  DATA: l_datum LIKE sy-datum.

  CHECK pt_body[] IS NOT INITIAL.

  SELECT SINGLE MAX( datum ) INTO l_datum
    FROM ztpp_delay_veh.

  CHECK l_datum IS NOT INITIAL.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_delay
    FROM ztpp_delay_veh
     FOR ALL ENTRIES IN pt_body
   WHERE datum = l_datum
     AND model_code =  pt_body-model_code
     AND body_no    =  pt_body-body_no
     AND zesoff     >= '19000101'.

  SORT it_osr BY model_code body_no gubn.

  LOOP AT lt_delay.
    READ TABLE it_osr WITH KEY model_code = lt_delay-model_code
                               body_no    = lt_delay-body_no
                               gubn       = ''
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF it_osr-lpdd NE lt_delay-zesoff.
        MOVE: 'X' TO it_osr-lpdd_d.
      ENDIF.

      it_osr-lpdd = lt_delay-zesoff.
      MODIFY it_osr INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_EST_SOFF_FROM_DELAY
*&---------------------------------------------------------------------*
*&      Form  GET_ALE_DESTINATION
*&---------------------------------------------------------------------*
FORM get_ale_destination .
  SELECT * INTO TABLE it_ale_dest
  FROM ztsd_ale_dest.

  IF it_ale_dest[] IS INITIAL.
    MESSAGE s000 WITH 'ALE destination info. is empty'.
    STOP.
  ENDIF.
ENDFORM.                    " GET_ALE_DESTINATION
