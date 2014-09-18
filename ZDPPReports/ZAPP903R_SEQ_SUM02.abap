************************************************************************
* Program Name      : ZAPP903R_SEQ_SUM02
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       :
*          Sequence Parts Summary (Bucket: Daily , Horz.: 21 days)
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
* 03/03/2005 CHRIS       ud1k914781   START DATE => SYSTEM DATE
************************************************************************
REPORT  zapp903r_seq_sum02    MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ztpp_common_vals,
        ztpp_input_plan,
        ausp .

*----------------------------------------------------------------------
* Gloval Variables Definition
*----------------------------------------------------------------------
DATA: wa_uph_b                TYPE zvpp_ld-lrate,
      wa_uph_p                TYPE zvpp_ld-lrate,
      wa_uph_t                TYPE zvpp_ld-lrate.

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF it_data        OCCURS 0,
        objek                 LIKE ausp-objek.      " Vehicle Code
        INCLUDE STRUCTURE     ztpp_input_plan.
DATA: END OF it_data.

DATA: BEGIN OF is_prod        OCCURS 0,
        objek                 LIKE ausp-objek.      " Vehicle Code
        INCLUDE STRUCTURE     ztpp_day_sum   .
DATA:   cnt                   TYPE i         ,
        model(4)              TYPE c         ,
      END OF is_prod.

DATA: BEGIN OF it_alc         OCCURS 0.
        INCLUDE STRUCTURE     cukb    .
DATA:   model(3)              TYPE c  ,
        knktx                 LIKE cukbt-knktx,
        code(3)               TYPE c  ,
        rp(2)                 TYPE n  ,
        type_alc              TYPE c  ,
        char_alc              LIKE cabn-atnam,
        full_alc(4)           TYPE c  ,
      END OF it_alc .

DATA: BEGIN OF it_model       OCCURS 0,
        modl                  TYPE zpp_model,
      END OF it_model.

DATA: BEGIN OF it_sum         OCCURS 0,
        rp(2)                 TYPE n  ,
        alc(9)                TYPE c  ,              " For Summary Field
        worder                LIKE mara-matnr,
        knnam                 LIKE cukb-knnam,
        status                LIKE ztpp_input_plan-status,
        code(4)               TYPE c  ,              " ALC CODE
        vals(5)               TYPE c  ,              " ALC CODE VALUE
        days                  TYPE p DECIMALS 0         ,
        vm_model              LIKE ztpp_input_plan-modl ,
        vm_bodyser            LIKE ztpp_input_plan-body_ser,
        extc                  LIKE ztpp_input_plan-extc,
        intc                  LIKE ztpp_input_plan-intc,
        mitu                  TYPE zmitu,
        mitucnt               TYPE i  ,
        cnt                   TYPE i  ,
        serial                LIKE ztpp_input_plan-serial,
      END OF it_sum .

*DATA: BEGIN OF it_seq         OCCURS 0.
*        INCLUDE STRUCTURE     zspp_vm_gen.
*DATA:   alc(11)               TYPE c  ,           " For Summary Field
*        knnam                 LIKE cukb-knnam,
*        cnt                   TYPE i  ,
*        mitu                  TYPE i  ,
*        code(4)               TYPE c  ,              " ALC CODE
*        vals(5)               TYPE c  ,              " ALC CODE VALUE
*      END OF it_seq.

DATA: BEGIN OF it_master  OCCURS 0,
        seq               TYPE i  ,             " Sequence
        date              TYPE d  ,             " Date
        day               LIKE kapa-tagnr,      " Day
        shift             LIKE kapa-schnr,      " Shift
        time              TYPE kapendzt  ,      " Times for working
        uph               TYPE zvpp_ld-lrate,   " UPH
      END OF it_master.

DATA: it_prod             LIKE TABLE OF is_prod        WITH HEADER LINE,
      it_seq              LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_d1               LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_bi               LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_wbs              LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_pi               LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_prj              LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_pbs              LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_d21              LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_disp_d1          LIKE TABLE OF ztpp_seq_sum02 WITH HEADER LINE,
      it_disp_seq         LIKE TABLE OF ztpp_seq_sum02 WITH HEADER LINE,
      it_disp_bi          LIKE TABLE OF ztpp_seq_sum02 WITH HEADER LINE,
      it_disp_pi          LIKE TABLE OF ztpp_seq_sum02 WITH HEADER LINE,
      it_disp_prj         LIKE TABLE OF ztpp_seq_sum02 WITH HEADER LINE,
      it_disp_wbs         LIKE TABLE OF ztpp_seq_sum02 WITH HEADER LINE,
      it_disp_pbs         LIKE TABLE OF ztpp_seq_sum02 WITH HEADER LINE,
      it_disp_d21         LIKE TABLE OF ztpp_seq_sum02 WITH HEADER LINE,
      it_disp             LIKE TABLE OF ztpp_seq_sum02 WITH HEADER LINE.

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: wa_disp             LIKE it_disp                             ,
      wa_data             LIKE it_data                             ,
      wa_wdate            LIKE ztpp_day_sum-wdate                  ,
      wa_kalid            LIKE kako-kalid                          ,
      wa_repid            LIKE sy-repid                            ,
      wa_uzeit            LIKE sy-uzeit                            ,
      wa_index            LIKE sy-tabix                            ,
      wa_model(3)         TYPE c                                   ,
      wa_error            TYPE c                                   ,
      wa_flag             TYPE c                                   ,
      wa_days             TYPE i                                   .


*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
FIELD-SYMBOLS: <wa_dfield>    TYPE any.


*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: c_jobs(40)              VALUE 'ZAPP903R_INPUT_PLAN',
      c_key1(18)              VALUE 'SEQ_SUM02' .


*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------


*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
*PARAMETERS: p_model(3)    TYPE c        ,
PARAMETERS: p_dates       TYPE d        OBLIGATORY,
            p_test        TYPE c                  ,
            p_mitu        TYPE c        ,
            p_wbs(2)      TYPE n        OBLIGATORY,
            p_prj(2)      TYPE n        OBLIGATORY,
            p_fwo        TYPE c        .
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  DATA: l_vals                LIKE ztpp_common_vals-item4.

  " Get the Date for the Production Reporting Date(Last Date)
* requested by MY Mur changed by chris
*  PERFORM get_start_day     USING wa_wdate     .
*  p_dates = wa_wdate
  p_dates = wa_wdate = sy-datum.
* end of change on 03/03/2005.
  wa_wdate = wa_wdate - 1.  wa_repid = sy-repid.
  PERFORM read_shop_calid   USING wa_kalid.
  PERFORM read_working_date USING '-'  wa_kalid  wa_wdate.

  IF p_wbs IS INITIAL.
    SELECT SINGLE item4  INTO l_vals
      FROM ztpp_common_vals
     WHERE jobs = wa_repid
       AND key2 = 'WBS'   .
    p_wbs = l_vals        .   CLEAR: l_vals.
  ENDIF.

  IF p_prj IS INITIAL.
    SELECT SINGLE item4  INTO l_vals
      FROM ztpp_common_vals
     WHERE jobs = wa_repid
       AND key2 = 'PRJ'   .
    p_prj = l_vals        .   CLEAR: l_vals.
  ENDIF.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM clear_variable               .
  PERFORM set_information              .
  PERFORM read_inputplan               .
  PERFORM read_alc_model               .
  PERFORM create_summary               .
  PERFORM insert_field_vals            .
  PERFORM summary_disp_final           .
  PERFORM display_data                 .

*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_variable.
  CLEAR: it_data,   it_alc,   it_sum,   it_disp,
         it_data[], it_alc[], it_sum[], it_disp[],
         wa_data,  wa_uzeit, wa_index, wa_days .
ENDFORM.                    " CLEAR_VARIABLE

*&---------------------------------------------------------------------*
*&      Form  READ_INPUTPLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_inputplan.
  DATA: l_len TYPE i,
          l_new_dealer(1).
  DATA: lw_inputp_log LIKE ztpp_inputp_log.

  DATA: BEGIN OF lt_worder OCCURS 0,
                wo_ser LIKE ztpp_wosum-wo_ser,
              END   OF lt_worder.


  IF p_mitu = 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_input_plan
    WHERE NOT ( mitu EQ 'X' OR mitu EQ 'Y' ).
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
     FROM ztpp_input_plan .
  ENDIF.

  " Elemenate the TEST-CAR that Dealer code is 'XX', 'XY' .
  IF p_test = 'X'.
    LOOP AT it_data.
      IF it_data-work_order+12(2) = 'XX' OR
         it_data-work_order+12(2) = 'XY' .
        DELETE it_data.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE it_data LINES  wa_days .
  IF wa_days = 0.
    DELETE FROM ztpp_seq_sum02  CLIENT SPECIFIED WHERE mandt = sy-mandt.
    LEAVE PROGRAM .
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_prod
    FROM ztpp_day_sum
   WHERE wdate = wa_wdate AND
         dealer NE 'XX'   AND
         dealer NE 'XY' .

  SORT it_data BY rsnum serial .
*---start#1 wskim : check holding car
  PERFORM check_holding_car.
*---end
  DATA: l_fwocl LIKE ztpp_fwo-worder,
        l_worder LIKE ztpp_fwo-o_worder,
        l_ordr LIKE ztpp_pmt07jb_a-ordr,
        l_dist LIKE ztpp_pmt07jb_a-dist,
        l_fsc LIKE ztpp_wosum-fsc,
        l_year LIKE ztpp_pmt07jb_a-moye.

  IF NOT p_fwo IS INITIAL.
    LOOP AT it_data.
      IF it_data-work_order+0(1) = 'F'.

** Changed no 06/10/13
*       CONCATENATE it_data-work_order it_data-extc it_data-intc
*               INTO l_fwocl.
*        SELECT SINGLE o_worder INTO l_worder
*          FROM ztpp_fwo
*          WHERE worder = l_fwocl.

        CONCATENATE it_data-work_order it_data-extc it_data-intc
               INTO l_fwocl.
        SELECT SINGLE o_worder INTO l_worder
          FROM ztpp_fwo
          WHERE model = it_data-modl
           AND  worder = l_fwocl.
** End on 06/10/13

        IF sy-subrc = 0.
          it_data-work_order = l_worder.
          MODIFY it_data.
        ELSE.
** added by Furong on 03/7/06
          l_ordr = it_data-work_order+0(9).
          l_dist = it_data-work_order+9(5).
          SELECT SINGLE moye INTO l_year
            FROM ztpp_pmt07jb_a
           WHERE ordr = l_ordr
             AND dist = l_dist
             AND extc = it_data-extc
             AND intc = it_data-intc.
** Changed by Furong on 10/10/07 for EBOM
          l_len = strlen( it_data-mi ).
          IF l_len = 7.
            CONCATENATE l_year l_dist it_data-mi INTO l_fsc.
            CONCATENATE l_fsc it_data-ocnn INTO l_fsc
                    SEPARATED BY space.
          ELSE.
            CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
              EXPORTING
                old_dealer = l_dist+3(2)
              IMPORTING
                new_dealer = l_new_dealer.
            CONCATENATE l_year l_dist+0(3) l_new_dealer it_data-mi
                        INTO l_fsc.
            CONCATENATE l_fsc it_data-ocnn INTO l_fsc.
          ENDIF.
** End of change
** Changed by Furong on 09/04/08
*        SELECT SINGLE WO_SER INTO L_WORDER
*          FROM ZTPP_WOSUM
*         WHERE EXTC = IT_DATA-EXTC
*           AND INTC = IT_DATA-INTC
*           AND VERSION = IT_DATA-VERS
*           AND FSC = L_FSC.
*------ Index created on 06/05/12

** Changed no 06/10/13
*        SELECT SINGLE WO_SER INTO L_WORDER
*          FROM ZTPP_WOSUM
*         WHERE FSC = L_FSC
*           AND EXTC = IT_DATA-EXTC
*           AND INTC = IT_DATA-INTC.
*** End of change on 09/04/08
*        IF SY-SUBRC = 0.
*          CONCATENATE L_WORDER L_DIST INTO L_WORDER.
*          IT_DATA-WORK_ORDER = L_WORDER.

          REFRESH lt_worder.
          SELECT wo_ser UP TO 1 ROWS
            INTO CORRESPONDING FIELDS OF TABLE lt_worder
            FROM ztpp_wosum
           WHERE fsc = l_fsc
             AND extc = it_data-extc
             AND intc = it_data-intc
           ORDER BY wo_ser DESCENDING.

          READ TABLE lt_worder INDEX 1.

** End of change on 09/04/08
          IF sy-subrc = 0.
            CONCATENATE lt_worder-wo_ser l_dist INTO l_worder.
            it_data-work_order = l_worder.
** End on 06/10/13
          ELSE.
** Changed by Furong on 09/04/08
            CLEAR: lw_inputp_log.
            lw_inputp_log-worder = it_data-work_order.
            lw_inputp_log-fsc = l_fsc.
            lw_inputp_log-extc = it_data-extc.
            lw_inputp_log-intc = it_data-intc.
            lw_inputp_log-ver = it_data-vers.
            lw_inputp_log-msg = 'No record in ZTPP_WOSUM'.
            lw_inputp_log-crdate = sy-datum.
            MODIFY ztpp_inputp_log FROM lw_inputp_log.
** End of change on 09/04/08
            CLEAR: it_data-work_order.
          ENDIF.
          MODIFY it_data.
** end of add
        ENDIF.
        CLEAR: l_fwocl, l_worder.
      ENDIF.
    ENDLOOP.
  ENDIF.
  SORT it_data BY rsnum serial .
ENDFORM.                    " READ_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  READ_ALC_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MODEL  text
*----------------------------------------------------------------------*
FORM read_alc_model  .
  DATA: lc_model(15)         TYPE c  ,
        l_knobj              LIKE cuco-knobj,
        l_knnum              LIKE cuob-knobj,
        l_knnam              LIKE cukb-knnam.

  CLEAR: it_model, it_model[].
  PERFORM get_models .

  LOOP AT it_model   .
    wa_model = it_model-modl .
    CONCATENATE 'D_' wa_model '_ALC_'  INTO lc_model    .
    CONCATENATE wa_model '_WOHD'           INTO  l_knnam.
    PERFORM get_knobj                      USING l_knnam  l_knobj.
    PERFORM get_knnum                      USING l_knobj.
    CONCATENATE wa_model '_WOCL'           INTO  l_knnam.
    PERFORM get_knobj                      USING l_knnam  l_knobj.
    PERFORM get_knnum                      USING l_knobj.
    " Set the Model Code...
    LOOP AT it_alc  WHERE model = space.
      it_alc-model = wa_model .
      MODIFY it_alc.
    ENDLOOP.
  ENDLOOP.

  LOOP AT it_alc.
    CONCATENATE 'D_' it_alc-model '_ALC_'  INTO lc_model    .
    SELECT SINGLE b~knnam t~knktx
      INTO CORRESPONDING FIELDS OF it_alc
      FROM cukb AS b INNER JOIN cukbt AS t
        ON b~knnum = t~knnum
     WHERE b~knnum = it_alc-knnum
       AND t~spras = sy-langu   .

    IF it_alc-knnam(10) NE lc_model .
      DELETE it_alc .
      CONTINUE .
    ENDIF.
    it_alc-code     = it_alc-knnam+12(3) .
    it_alc-type_alc = it_alc-knnam+10(1) .
    it_alc-rp       = it_alc-knktx(2)    .
    CONCATENATE it_alc-type_alc it_alc-code INTO it_alc-full_alc .
    CONCATENATE 'P' it_alc-knnam+5(10)      INTO it_alc-char_alc .
    MODIFY it_alc .
  ENDLOOP.
  SORT it_alc BY knnum rp code .
ENDFORM.                    " READ_ALC_MODEL

*&---------------------------------------------------------------------*
*&      Form  CREATE_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_summary.
  DATA: lt_alc               LIKE TABLE OF it_alc      WITH HEADER LINE,
        lt_wbs               LIKE TABLE OF it_wbs      WITH HEADER LINE,
        lt_prj               LIKE TABLE OF it_prj      WITH HEADER LINE,
        lt_seq               LIKE TABLE OF it_seq      WITH HEADER LINE,
        lt_bi                LIKE TABLE OF it_bi       WITH HEADER LINE,
        lt_pi                LIKE TABLE OF it_pi       WITH HEADER LINE,
        lt_d1                LIKE TABLE OF it_d1       WITH HEADER LINE,
        lt_data              LIKE TABLE OF it_data     WITH HEADER LINE.

  PERFORM inline_status .
  lt_data[] = it_data[] .  lt_wbs[] = it_wbs[].  lt_bi[] = it_bi[].
  lt_d1[]   = it_d1[] .    lt_prj[] = it_prj[].  lt_pi[] = it_pi[].
  lt_seq[]  = it_seq[]  .  lt_alc[] = it_alc[].

  CLEAR: wa_index, wa_days, it_sum, it_sum[].
  PERFORM read_internal_table .
  PERFORM read_alc            .
  PERFORM calc_alc            .

  CLEAR: wa_index, wa_days, it_sum, it_sum[].
  PERFORM calc_alc_prod       .
  PERFORM calc_alc_seq        .
  PERFORM calc_alc_bi         .
  PERFORM calc_alc_wbs        .
  PERFORM calc_alc_pi         .
  PERFORM calc_alc_prj        .
  PERFORM calc_alc_pbs        .

  PERFORM summary_disp          .
ENDFORM.                    " CREATE_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  READ_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ALC_RP  text
*----------------------------------------------------------------------*
FORM read_internal_table .
  DATA: l_name(40)           TYPE c ,
        l_flag               TYPE c ,
        l_max                TYPE i ,
*       l_loops              TYPE i ,
        l_days               TYPE i ,
        l_pos                TYPE i ,
        l_chk                TYPE p DECIMALS 3,
        l_tabix              LIKE sy-tabix,
        l_start              LIKE sy-tabix,
        l_end                LIKE sy-tabix,
        l_index              LIKE sy-tabix.

  READ TABLE it_data WITH KEY rp06 = space .
  l_index = sy-tabix .
  IF l_index > 1.
    l_index = l_index - 1 .
    DELETE it_data FROM 1 TO l_index.
  ENDIF.
  l_index = 1.

  " 1 Days * 21 Times
  DESCRIBE TABLE it_data      LINES l_max  .
  SORT it_master BY seq .     CLEAR: l_tabix.
  DO 21 TIMES.
    l_tabix = l_tabix + 1 .
    READ TABLE it_master WITH KEY seq = l_tabix.
    IF it_master-uph = 0.
      l_days = l_days + 1 .
    ELSE.
      l_chk = it_master-time / 3600 .
      IF l_flag = 'X'.   EXIT.   ENDIF.
      l_pos = l_pos +  it_master-uph * l_chk  .
      IF l_pos >= l_max.
        wa_index = l_max - l_index.
        wa_days  = l_days .
        l_pos = l_max.
        l_flag = 'X' .
      ENDIF.
      l_days  = l_days  + 1 .
      LOOP AT it_data FROM l_index TO l_pos.
        CLEAR: it_sum.
        it_sum-days       = l_days             .
        it_sum-vm_model   = it_data-modl       .
        it_sum-vm_bodyser = it_data-body_ser   .
        it_sum-worder     = it_data-work_order .
        it_sum-extc       = it_data-extc       .
        it_sum-intc       = it_data-intc       .
        it_sum-status     = it_data-status     .
        it_sum-rp         = it_alc-rp          .
        it_sum-knnam      = it_alc-knnam       .
        it_sum-cnt        = 1                  .
        CONCATENATE it_alc-type_alc it_alc-code INTO it_sum-code .
        APPEND it_sum.
      ENDLOOP.
    ENDIF.
    l_index = l_pos + 1 .
  ENDDO.
ENDFORM.                    " READ_INTERNAL_TABLE

*&---------------------------------------------------------------------*
*&      Form  calc_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_alc.
  DATA: l_name(40)              TYPE c  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_days(2)               TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp.
  DESCRIBE TABLE it_sum LINES l_line.
  CHECK l_line > 0 .
  SORT it_sum BY vm_model days  code vals.
  READ TABLE it_sum INDEX 1.
  l_days   = it_sum-days    .
  l_code   = it_sum-code    .
  l_vals   = it_sum-vals    .
  l_model  = it_sum-vm_model.

  CONCATENATE 'IT_DISP-D'  l_days   INTO l_name.
  ASSIGN (l_name)                   TO   <wa_dfield>.
  CLEAR: <wa_dfield>.

  LOOP AT it_sum.
    IF l_days  = it_sum-days  AND l_code  = it_sum-code AND
       l_vals  = it_sum-vals  AND l_model  = it_sum-vm_model.
      <wa_dfield> = <wa_dfield> + it_sum-cnt .
      CONTINUE.
    ELSE.
      it_disp-model    = l_model  .
      it_disp-alc_code = l_code   .
      it_disp-alc_vals = l_vals   .
      it_disp-rp       = it_alc-rp.
      APPEND it_disp.    CLEAR: it_disp.
      l_days     = it_sum-days     .
      l_code     = it_sum-code     .
      l_vals     = it_sum-vals     .
      l_model    = it_sum-vm_model .
      CONCATENATE 'IT_DISP-D'  l_days   INTO l_name.
      ASSIGN (l_name)                   TO   <wa_dfield>.
      <wa_dfield>  = it_sum-cnt .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_sum  LINES  l_line.
  IF l_line > 0.
    it_disp-model    = l_model  .
    it_disp-alc_code = l_code   .
    it_disp-alc_vals = l_vals   .
    it_disp-rp       = it_alc-rp.
    APPEND it_disp.
  ENDIF.
ENDFORM.                    " calc_alc

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_alc_prod.
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_d1  .
  SORT it_d1 BY vm_model days  code vals.
  READ TABLE it_d1 INDEX 1.  CLEAR: l_count.
* l_days   = IT_D1-days    .
  l_code   = it_d1-code    .
  l_vals   = it_d1-vals    .
  l_model  = it_d1-vm_model.

* CONCATENATE 'IT_DISP_D1-H'  l_hours  INTO l_name.
* ASSIGN (l_name)                   TO   <wa_dfield>.
* CLEAR: <wa_dfield>.

  LOOP AT it_d1.
    IF l_vals  = it_d1-vals  AND l_code  = it_d1-code AND
       l_model  = it_d1-vm_model.
*      l_hours = IT_D1-hours.
      l_count  = l_count + it_d1-cnt .
      CONTINUE.
    ELSE.
      it_disp_d1-model    = l_model  .
      it_disp_d1-alc_code = l_code   .
      it_disp_d1-alc_vals = l_vals   .
      it_disp_d1-rp       = it_alc-rp.
      it_disp_d1-d_1      = l_count  .
      APPEND it_disp_d1.    CLEAR: it_disp_d1.
*     l_hours = IT_D1-hours      .
      l_code  = it_d1-code       .
      l_vals  = it_d1-vals       .
      l_model = it_d1-vm_model   .
      l_count = it_d1-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_d1  LINES  l_line.
  IF l_line > 0.
    it_disp_d1-model    = l_model  .
    it_disp_d1-alc_code = l_code   .
    it_disp_d1-alc_vals = l_vals   .
    it_disp_d1-rp       = it_alc-rp.
    it_disp_d1-d_1      = l_count  .
    APPEND it_disp_d1.
  ENDIF.
ENDFORM.                    " calc_alc_PROD

*&---------------------------------------------------------------------*
*&      Form  calc_alc_SEQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_alc_seq .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_mitucnt               TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_seq .
  SORT it_seq  BY vm_model code vals.
  READ TABLE it_seq  INDEX 1.  CLEAR: l_count, l_mitucnt.
* l_hours  = it_SEQ-hours   .
  l_code   = it_seq-code    .
  l_vals   = it_seq-vals    .
  l_model  = it_seq-vm_model.

  LOOP AT it_seq .
    IF l_vals  = it_seq-vals  AND l_code  = it_seq-code AND
       l_model  = it_seq-vm_model.
      l_count   = l_count   + it_seq-cnt     .
      l_mitucnt = l_mitucnt + it_seq-mitucnt .
      CONTINUE.
    ELSE.
      it_disp_seq-model    = l_model  .
      it_disp_seq-alc_code = l_code   .
      it_disp_seq-alc_vals = l_vals   .
      it_disp_seq-rp       = it_alc-rp.
      it_disp_seq-d_1      = l_count  .
      it_disp_seq-mitu     = l_mitucnt.
      APPEND it_disp_seq .    CLEAR: it_disp_seq .
*     l_hours   = it_SEQ-hours      .
      l_code    = it_seq-code       .
      l_vals    = it_seq-vals       .
      l_model   = it_seq-vm_model   .
      l_count   = it_seq-cnt        .
      l_mitucnt = it_seq-mitucnt .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_seq   LINES  l_line.
  IF l_line > 0.
    it_disp_seq-model    = l_model  .
    it_disp_seq-alc_code = l_code   .
    it_disp_seq-alc_vals = l_vals   .
    it_disp_seq-rp       = it_alc-rp.
    it_disp_seq-d_1      = l_count  .
    it_disp_seq-mitu     = l_mitucnt.
    APPEND it_disp_seq .
  ENDIF.
ENDFORM.                    " calc_alc_SEQ

*&---------------------------------------------------------------------*
*&      Form  calc_alc_BI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_alc_bi  .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_bi .
  SORT it_bi   BY vm_model code vals.
  READ TABLE it_bi   INDEX 1.  CLEAR: l_count.
  l_code   = it_bi-code    .
  l_vals   = it_bi-vals    .
  l_model  = it_bi-vm_model.

  LOOP AT it_bi  .
    IF l_vals  = it_bi-vals  AND l_code  = it_bi-code  AND
       l_model  = it_bi-vm_model.
      l_count  = l_count + it_bi-cnt .
      CONTINUE.
    ELSE.
      it_disp_bi-model    = l_model  .
      it_disp_bi-alc_code = l_code   .
      it_disp_bi-alc_vals = l_vals   .
      it_disp_bi-rp       = it_alc-rp.
      it_disp_bi-d_1      = l_count  .
      APPEND it_disp_bi .    CLEAR: it_disp_bi .
      l_code  = it_bi-code       .
      l_vals  = it_bi-vals       .
      l_model = it_bi-vm_model   .
      l_count = it_bi-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_bi    LINES  l_line.
  IF l_line > 0.
    it_disp_bi-model    = l_model  .
    it_disp_bi-alc_code = l_code   .
    it_disp_bi-alc_vals = l_vals   .
    it_disp_bi-rp       = it_alc-rp.
    it_disp_bi-d_1      = l_count  .
    APPEND it_disp_bi  .
  ENDIF.
ENDFORM.                    " calc_alc_BI

*&---------------------------------------------------------------------*
*&      Form  calc_alc_WBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_alc_wbs .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_wbs .
  SORT it_wbs  BY vm_model code vals.
  READ TABLE it_wbs  INDEX 1.  CLEAR: l_count.
* l_hours  = it_WBS-hours   .
  l_code   = it_wbs-code    .
  l_vals   = it_wbs-vals    .
  l_model  = it_wbs-vm_model.

  LOOP AT it_wbs .
    IF l_vals  = it_wbs-vals  AND l_code  = it_wbs-code AND
       l_model  = it_wbs-vm_model.
      l_count  = l_count + it_wbs-cnt .
      CONTINUE.
    ELSE.
      it_disp_wbs-model    = l_model  .
      it_disp_wbs-alc_code = l_code   .
      it_disp_wbs-alc_vals = l_vals   .
      it_disp_wbs-rp       = it_alc-rp.
      it_disp_wbs-d_1      = l_count  .
      APPEND it_disp_wbs .    CLEAR: it_disp_wbs .
      l_code  = it_wbs-code       .
      l_vals  = it_wbs-vals       .
      l_model = it_wbs-vm_model   .
      l_count = it_wbs-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_wbs   LINES  l_line.
  IF l_line > 0.
    it_disp_wbs-model    = l_model  .
    it_disp_wbs-alc_code = l_code   .
    it_disp_wbs-alc_vals = l_vals   .
    it_disp_wbs-rp       = it_alc-rp.
    it_disp_wbs-d_1      = l_count  .
    APPEND it_disp_wbs .
  ENDIF.
ENDFORM.                    " calc_alc_WBS

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_alc_pi  .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_pi .
  SORT it_pi   BY vm_model code vals.
  READ TABLE it_pi   INDEX 1.  CLEAR: l_count.
  l_code   = it_pi-code    .
  l_vals   = it_pi-vals    .
  l_model  = it_pi-vm_model.

  LOOP AT it_pi  .
    IF l_vals  = it_pi-vals  AND l_code  = it_pi-code AND
       l_model  = it_pi-vm_model.
      l_count  = l_count + it_pi-cnt .
      CONTINUE.
    ELSE.
      it_disp_pi-model    = l_model  .
      it_disp_pi-alc_code = l_code   .
      it_disp_pi-alc_vals = l_vals   .
      it_disp_pi-rp       = it_alc-rp.
      it_disp_pi-d_1      = l_count  .
      APPEND it_disp_pi .    CLEAR: it_disp_pi .
*     l_hours = it_PI-Hours      .
      l_code  = it_pi-code       .
      l_vals  = it_pi-vals       .
      l_model = it_pi-vm_model   .
      l_count = it_pi-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_pi    LINES  l_line.
  IF l_line > 0.
    it_disp_pi-model    = l_model  .
    it_disp_pi-alc_code = l_code   .
    it_disp_pi-alc_vals = l_vals   .
    it_disp_pi-rp       = it_alc-rp.
    it_disp_pi-d_1      = l_count  .
    APPEND it_disp_pi  .
  ENDIF.
ENDFORM.                    " calc_alc_PI

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PRJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_alc_prj .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_prj .
  SORT it_prj BY vm_model code vals.
  READ TABLE it_prj INDEX 1.  CLEAR: l_count.
  l_code   = it_prj-code    .
  l_vals   = it_prj-vals    .
  l_model  = it_prj-vm_model.

  LOOP AT it_prj .
    IF l_vals  = it_prj-vals  AND l_code  = it_prj-code AND
       l_model  = it_prj-vm_model.
*      l_hours = it_PRJ-hours.
      l_count  = l_count + it_prj-cnt .
      CONTINUE.
    ELSE.
      it_disp_prj-model    = l_model  .
      it_disp_prj-alc_code = l_code   .
      it_disp_prj-alc_vals = l_vals   .
      it_disp_prj-rp       = it_alc-rp.
      it_disp_prj-d_1      = l_count  .
      APPEND it_disp_prj.    CLEAR: it_disp_prj .
*     l_hours = it_PRJ-hours      .
      l_code  = it_prj-code       .
      l_vals  = it_prj-vals       .
      l_model = it_prj-vm_model   .
      l_count = it_prj-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_prj   LINES  l_line.
  IF l_line > 0.
    it_disp_prj-model    = l_model  .
    it_disp_prj-alc_code = l_code   .
    it_disp_prj-alc_vals = l_vals   .
    it_disp_prj-rp       = it_alc-rp.
    it_disp_prj-d_1      = l_count  .
    APPEND it_disp_prj.
  ENDIF.
ENDFORM.                    " calc_alc_PRJ

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_alc_pbs .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_pbs .
  SORT it_pbs  BY vm_model code vals.
  READ TABLE it_pbs  INDEX 1.  CLEAR: l_count.
* l_hours  = it_PBS-hours   .
  l_code   = it_pbs-code    .
  l_vals   = it_pbs-vals    .
  l_model  = it_pbs-vm_model.

  LOOP AT it_pbs .
    IF l_vals  = it_pbs-vals  AND l_code  = it_pbs-code AND
       l_model  = it_pbs-vm_model.
      l_count  = l_count + it_pbs-cnt .
      CONTINUE.
    ELSE.
      it_disp_pbs-model    = l_model  .
      it_disp_pbs-alc_code = l_code   .
      it_disp_pbs-alc_vals = l_vals   .
      it_disp_pbs-rp       = it_alc-rp.
      it_disp_pbs-d_1      = l_count  .
      APPEND it_disp_pbs .    CLEAR: it_disp_pbs .
      l_code  = it_pbs-code       .
      l_vals  = it_pbs-vals       .
      l_model = it_pbs-vm_model   .
      l_count = it_pbs-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_pbs   LINES  l_line.
  IF l_line > 0.
    it_disp_pbs-model    = l_model  .
    it_disp_pbs-alc_code = l_code   .
    it_disp_pbs-alc_vals = l_vals   .
    it_disp_pbs-rp       = it_alc-rp.
    it_disp_pbs-d_1      = l_count  .
    APPEND it_disp_pbs .
  ENDIF.
ENDFORM.                    " calc_alc_PBS

*&---------------------------------------------------------------------*
*&      Form  GET_ALC_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUM_WORDER  text
*      -->P_IT_SUM_CODE  text
*      <--P_IT_SUM_VALS  text
*----------------------------------------------------------------------*
FORM get_alc_value USING    pa_worder  pa_code
                   CHANGING pa_vals.
  DATA: l_vals            LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_matnr           LIKE mara-matnr.

  CLEAR: l_vals, l_vals[].
  CASE pa_code(1).
    WHEN 'U'.
      l_matnr = pa_worder(14).
      CONCATENATE 'P_ALC_U_' pa_code+1(3) INTO l_vals-atnam.
    WHEN 'C'.
      l_matnr = pa_worder    .
      CONCATENATE 'P_ALC_C_' pa_code+1(3) INTO l_vals-atnam.
  ENDCASE.
  APPEND l_vals.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_matnr
      ctype        = '001'
    TABLES
      val_table    = l_vals
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  READ TABLE l_vals INDEX 1  .
  pa_vals = l_vals-atwrt     .
ENDFORM.                    " GET_ALC_VALUE

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  DATA: l_cnt                LIKE it_disp-serial.
  PERFORM sub_total.                                        "UD1K912950
  DELETE FROM ztpp_seq_sum02 CLIENT SPECIFIED WHERE mandt = sy-mandt.
  SORT it_disp BY serial alc_code model DESCENDING alc_vals .
*  LOOP AT it_disp.
*    LOOP AT it_seq WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-seq = it_disp-seq + it_seq-cnt     .
*    ENDLOOP.
*    LOOP AT it_bi  WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-bodyin = it_disp-bodyin + it_bi-cnt     .
*    ENDLOOP.
*    LOOP AT it_pi WHERE  model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-paint = it_disp-paint + it_pi-cnt     .
*    ENDLOOP.
*    LOOP AT it_pbs WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-pbs = it_disp-pbs + it_pbs-cnt     .
*    ENDLOOP.
*    LOOP AT it_prj WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-prj = it_disp-prj + it_prj-cnt     .
*    ENDLOOP.
*    LOOP AT it_wbs WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-wbs = it_disp-wbs + it_wbs-cnt     .
*    ENDLOOP.
*    MODIFY it_disp.
*  ENDLOOP.
  MODIFY ztpp_seq_sum02     FROM TABLE it_disp .
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE s000 WITH 'Sum02 Update successful'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s000 WITH 'Sum02 Update failed'.
  ENDIF.

ENDFORM.                    " display_data

*&---------------------------------------------------------------------*
*&      Form  INLINE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inline_status.
  " Summary of the ALC for the Current MIP Vehicle..
  PERFORM create_data   TABLES  it_seq   USING '00'.
  PERFORM calc_seq                                 .
  PERFORM create_data   TABLES  it_bi    USING '01'.
  PERFORM calc_bodyinput                           .
  PERFORM create_data   TABLES  it_wbs   USING '99'.
  PERFORM calc_wbs                                 .
  PERFORM create_data   TABLES  it_pi    USING '02'.
  PERFORM calc_paintinput                          .
  PERFORM create_data   TABLES  it_prj   USING '88'.
  PERFORM calc_paintreject                         .
  PERFORM create_data   TABLES  it_pbs   USING '06'.
  PERFORM calc_pbs                                 .
  PERFORM calc_prod                                .
ENDFORM.                    " INLINE_STATUS

*&---------------------------------------------------------------------*
*&      Form  add_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DISP  text
*----------------------------------------------------------------------*
FORM add_routine USING    pa_disp  LIKE  it_disp.
  it_disp-d01  = it_disp-d01  + pa_disp-d01 .
  it_disp-d02  = it_disp-d02  + pa_disp-d02 .
  it_disp-d03  = it_disp-d03  + pa_disp-d03 .
  it_disp-d04  = it_disp-d04  + pa_disp-d04 .
  it_disp-d05  = it_disp-d05  + pa_disp-d05 .
  it_disp-d06  = it_disp-d06  + pa_disp-d06 .
  it_disp-d07  = it_disp-d07  + pa_disp-d07 .
  it_disp-d08  = it_disp-d08  + pa_disp-d08 .
  it_disp-d09  = it_disp-d09  + pa_disp-d09 .
  it_disp-d10  = it_disp-d10  + pa_disp-d10 .
  it_disp-d11  = it_disp-d11  + pa_disp-d11 .
  it_disp-d12  = it_disp-d12  + pa_disp-d12 .
  it_disp-d13  = it_disp-d13  + pa_disp-d13 .
  it_disp-d14  = it_disp-d14  + pa_disp-d14 .
  it_disp-d15  = it_disp-d15  + pa_disp-d15 .
  it_disp-d16  = it_disp-d16  + pa_disp-d16 .
  it_disp-d17  = it_disp-d17  + pa_disp-d17 .
  it_disp-d18  = it_disp-d18  + pa_disp-d18 .
  it_disp-d19  = it_disp-d19  + pa_disp-d19 .
  it_disp-d20  = it_disp-d20  + pa_disp-d20 .
  it_disp-d21  = it_disp-d21  + pa_disp-d21 .
  it_disp-mitu = it_disp-mitu + pa_disp-mitu.
ENDFORM.                    " add_routine

*&---------------------------------------------------------------------*
*&      Form  add_routine_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DISP  text
*----------------------------------------------------------------------*
FORM add_routine_prod USING    pa_disp  LIKE  it_disp.
  it_disp-d_1    = it_disp-d_1  + pa_disp-d_1 .
  it_disp-seq    = it_disp-seq  + pa_disp-seq .
  it_disp-bodyin = it_disp-bodyin  + pa_disp-bodyin .
  it_disp-wbs    = it_disp-wbs  + pa_disp-wbs .
  it_disp-paint  = it_disp-paint  + pa_disp-paint .
  it_disp-prj    = it_disp-prj  + pa_disp-prj .
  it_disp-pbs    = it_disp-pbs  + pa_disp-pbs .
  it_disp-d01  = it_disp-d01  + pa_disp-d01 .
  it_disp-d02  = it_disp-d02  + pa_disp-d02 .
  it_disp-d03  = it_disp-d03  + pa_disp-d03 .
  it_disp-d04  = it_disp-d04  + pa_disp-d04 .
  it_disp-d05  = it_disp-d05  + pa_disp-d05 .
  it_disp-d06  = it_disp-d06  + pa_disp-d06 .
  it_disp-d07  = it_disp-d07  + pa_disp-d07 .
  it_disp-d08  = it_disp-d08  + pa_disp-d08 .
  it_disp-d09  = it_disp-d09  + pa_disp-d09 .
  it_disp-d10  = it_disp-d10  + pa_disp-d10 .
  it_disp-d11  = it_disp-d11  + pa_disp-d11 .
  it_disp-d12  = it_disp-d12  + pa_disp-d12 .
  it_disp-d13  = it_disp-d13  + pa_disp-d13 .
  it_disp-d14  = it_disp-d14  + pa_disp-d14 .
  it_disp-d15  = it_disp-d15  + pa_disp-d15 .
  it_disp-d16  = it_disp-d16  + pa_disp-d16 .
  it_disp-d17  = it_disp-d17  + pa_disp-d17 .
  it_disp-d18  = it_disp-d18  + pa_disp-d18 .
  it_disp-d19  = it_disp-d19  + pa_disp-d19 .
  it_disp-d20  = it_disp-d20  + pa_disp-d20 .
  it_disp-d21  = it_disp-d21  + pa_disp-d21 .
  it_disp-mitu = it_disp-mitu + pa_disp-mitu.
ENDFORM.                    " add_routine_PROD

*&---------------------------------------------------------------------*
*&      Form  SUMMARY_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM summary_disp.
  DATA: lt_disp                 LIKE TABLE OF it_disp  WITH HEADER LINE,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_status(3)             TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  lt_disp[] = it_disp[].

  " Accumulate the Data..
  CLEAR: it_disp, it_disp[].
  SORT lt_disp BY alc_code model alc_vals.
  READ TABLE lt_disp INDEX 1.
  l_model = lt_disp-model   .
  l_code  = lt_disp-alc_code.
  l_vals  = lt_disp-alc_vals.
  it_disp     = lt_disp         .
  PERFORM clear_qty_disp  USING it_disp     .

  LOOP AT lt_disp.
    IF l_model = lt_disp-model  AND  l_code  = lt_disp-alc_code  AND
       l_vals  = lt_disp-alc_vals.
      PERFORM add_routine  USING  lt_disp .
      CONTINUE.
    ELSE.
      CONDENSE lt_disp-alc_code .
      it_disp-serial = strlen( l_vals )    .
      APPEND it_disp.     CLEAR: it_disp   .
      it_disp     = lt_disp         .
*     PERFORM clear_qty_disp  USING it_disp     .
      l_model = lt_disp-model   .
      l_code  = lt_disp-alc_code.
      l_vals  = lt_disp-alc_vals.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_disp LINES  l_line.
  IF l_line > 0.
    CONDENSE lt_disp-alc_code .
    it_disp-serial = strlen( l_vals ) .
    APPEND it_disp.
  ENDIF.
ENDFORM.                    " SUMMARY_DISP

*&---------------------------------------------------------------------*
*&      Form  SUMMARY_DISP_final
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM summary_disp_final.
  DATA: lt_disp                 LIKE TABLE OF it_disp  WITH HEADER LINE,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_status(3)             TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  " Final Summarize...
  lt_disp[] = it_disp[].    CLEAR: it_disp, it_disp[].
  SORT lt_disp BY model alc_code alc_vals .
  READ TABLE lt_disp INDEX 1.
  l_model = lt_disp-model   .
  l_code  = lt_disp-alc_code.
  l_vals  = lt_disp-alc_vals.
  it_disp     = lt_disp         .
  PERFORM clear_qty_disp  USING it_disp     .

  LOOP AT lt_disp.
    IF l_model = lt_disp-model  AND  l_code  = lt_disp-alc_code  AND
       l_vals  = lt_disp-alc_vals.
      PERFORM add_routine_prod  USING  lt_disp .
      CONTINUE.
    ELSE.
      CONDENSE lt_disp-alc_code .
      it_disp-serial = strlen( l_vals )    .
      APPEND it_disp.     CLEAR: it_disp   .
      it_disp     = lt_disp         .
*     PERFORM clear_qty_disp  USING it_disp     .
      l_model = lt_disp-model   .
      l_code  = lt_disp-alc_code.
      l_vals  = lt_disp-alc_vals.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_disp LINES  l_line.
  IF l_line > 0.
    CONDENSE lt_disp-alc_code .
    it_disp-serial = strlen( l_vals ) .
    APPEND it_disp.
  ENDIF.
ENDFORM.                    " SUMMARY_DISP_FINAL

*&---------------------------------------------------------------------*
*&      Form  GET_KNOBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM get_knobj USING    pa_knnam  pa_knobj.
  CLEAR: pa_knobj.
  SELECT SINGLE knobj INTO pa_knobj
    FROM cuco
   WHERE obtab = 'MARA'
     AND objek = pa_knnam .
ENDFORM.                    " GET_KNOBJ

*&---------------------------------------------------------------------*
*&      Form  get_KNNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM get_knnum USING    pa_knobj.
  SELECT knnum APPENDING CORRESPONDING FIELDS OF TABLE it_alc
    FROM cuob
   WHERE knobj = pa_knobj.
ENDFORM.                    " get_KNNUM

*&---------------------------------------------------------------------*
*&      Form  read_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_alc.
  DATA: lt_sum              LIKE TABLE OF it_sum       WITH HEADER LINE,
        l_model             LIKE it_sum-vm_model,
        l_worder            LIKE mara-matnr  ,
        l_days              LIKE it_sum-days ,
        l_mitucnt           TYPE i           ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  SORT it_sum BY days  worder extc intc .
  READ TABLE it_sum INDEX 1.
  l_days   = it_sum-days    .
  l_worder = it_sum-worder  .
  l_ext    = it_sum-extc    .
  l_int    = it_sum-intc    .
  l_model  = it_sum-vm_model  .

  " Work Order Summarize in the same time terms.
  LOOP AT it_sum.
    IF l_days  = it_sum-days  AND l_worder = it_sum-worder AND
       l_ext   = it_sum-extc  AND l_int    = it_sum-intc   AND
       l_model = it_sum-vm_model .
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      LOOP AT it_alc WHERE model = l_model .
        CONCATENATE  l_worder  l_ext    l_int   INTO l_worder       .
        PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                            CHANGING lt_sum-vals                .
        lt_sum-vm_model   = l_model      .
        lt_sum-days       = l_days            .
        lt_sum-worder     = l_worder          .
        lt_sum-extc       = l_ext             .
        lt_sum-intc       = l_int             .
        lt_sum-cnt        = l_cnt             .
        lt_sum-code       = it_alc-full_alc   .
        APPEND lt_sum      .
      ENDLOOP.
      lt_sum   = it_sum         .
      l_days   = it_sum-days    .
      l_worder = it_sum-worder  .
      l_ext    = it_sum-extc    .
      l_int    = it_sum-intc    .
      l_model  = it_sum-vm_model.
      l_cnt    = 1              .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_sum LINES l_size .
  IF l_size > 0 .
    LOOP AT it_alc WHERE model = l_model .
      CONCATENATE  l_worder  l_ext    l_int   INTO l_worder       .
      PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                         CHANGING lt_sum-vals                .
      lt_sum-vm_model   = l_model      .
      lt_sum-worder     = l_worder          .
      lt_sum-extc       = l_ext             .
      lt_sum-intc       = l_int             .
      lt_sum-cnt        = l_cnt             .
      lt_sum-code       = it_alc-full_alc   .
      APPEND       lt_sum      .
    ENDLOOP.
  ENDIF.

  it_sum[] = lt_sum[].
ENDFORM.                    " read_alc

*&---------------------------------------------------------------------*
*&      Form  create_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SEQ  text
*      -->P_1320   text
*----------------------------------------------------------------------*
FORM create_data  TABLES pa_tab STRUCTURE it_sum  USING  pa_rp .
  DATA: l_data    LIKE TABLE OF ztpp_input_plan      WITH HEADER LINE.

  CASE pa_rp.
    WHEN '00' OR '01'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = pa_rp .
    WHEN '02' OR '88'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = '02'  .

      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = '03'  .
    WHEN '06'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = '04'  .

      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = '05'  .
    WHEN '99'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = '01'  .
  ENDCASE.

  IF p_test = 'X'.
    LOOP AT l_data.
      IF l_data-work_order+12(2) = 'XX' OR
         l_data-work_order+12(2) = 'XY' .
        DELETE l_data.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT l_data BY serial DESCENDING.
  LOOP AT l_data.
    CLEAR: pa_tab.
    pa_tab-vm_model   = l_data-modl       .
    pa_tab-vm_bodyser = l_data-body_ser   .
    pa_tab-worder     = l_data-work_order .
    pa_tab-mitu       = l_data-mitu       .
    pa_tab-extc       = l_data-extc       .
    pa_tab-intc       = l_data-intc       .
    pa_tab-status     = l_data-status     .
    pa_tab-rp         = '06'              .
    pa_tab-cnt        = 1                 .
    pa_tab-serial     = l_data-serial     .
    APPEND pa_tab .
  ENDLOOP.
ENDFORM.                    " create_DATA

*&---------------------------------------------------------------------*
*&      Form  CALC_SEQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_seq.
  DATA: l_mitucnt            TYPE i ,
        l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_model              LIKE it_seq-vm_model,
        l_worder             LIKE mara-matnr  ,
        l_extc               LIKE ztpp_input_plan-extc,
        l_intc               LIKE ztpp_input_plan-intc,
        l_code               LIKE it_seq-code ,
        l_days(2)            TYPE n           ,
        l_vals               LIKE it_seq-vals ,
        lt_seq               LIKE TABLE OF it_sum      WITH HEADER LINE.

  DESCRIBE TABLE it_seq LINES l_cnt  .
  CHECK l_cnt > 0 .

  SORT it_seq BY days  worder extc intc vm_model.
  READ TABLE it_seq  INDEX 1.    CLEAR: l_cnt, l_mitucnt.
  l_days   = it_seq-days    .
  l_worder = it_seq-worder  .
  l_extc   = it_seq-extc    .
  l_intc   = it_seq-intc    .
  l_model  = it_seq-vm_model.
  lt_seq   = it_seq         .

  LOOP AT it_seq            .
    IF l_days  = it_seq-days  AND l_worder = it_seq-worder AND
       l_extc  = it_seq-extc  AND l_intc   = it_seq-intc   AND
       l_model = it_seq-vm_model .
      l_cnt    = l_cnt  + 1          .
      IF it_seq-mitu = 'Y'      .
        l_mitucnt = l_mitucnt + 1    .
      ENDIF.
      CONTINUE .
    ELSE.
      LOOP AT it_alc WHERE model = l_model .
        CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
        PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                            CHANGING lt_seq-vals                    .
        lt_seq-vm_model   = l_model      .
        lt_seq-worder     = l_worder          .
        lt_seq-extc       = l_extc            .
        lt_seq-intc       = l_intc            .
        lt_seq-cnt        = l_cnt             .
        lt_seq-mitucnt    = l_mitucnt         .
        lt_seq-code       = it_alc-full_alc   .
        APPEND lt_seq               .
      ENDLOOP.
      lt_seq   = it_seq             .
      l_worder = it_seq-worder      .
      l_extc   = it_seq-extc        .
      l_intc   = it_seq-intc        .
      l_model  = it_seq-vm_model    .
      l_cnt    = 1                  .
      IF it_seq-mitu = 'Y'      .
        l_mitucnt = 1.                                      "UD1K912950
*        l_mitucnt = l_mitucnt + 1    .               "UD1K912950
      ELSE.
        CLEAR: l_mitucnt.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_seq  LINES l_size.
  IF l_size > 0 .
    LOOP AT it_alc WHERE model = l_model .
      CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
      PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                          CHANGING lt_seq-vals                    .
      lt_seq-vm_model   = l_model      .
      lt_seq-worder     = l_worder          .
      lt_seq-extc       = l_extc            .
      lt_seq-intc       = l_intc            .
      lt_seq-cnt        = l_cnt             .
      lt_seq-mitucnt    = l_mitucnt         .
      lt_seq-code       = it_alc-full_alc   .
      APPEND lt_seq               .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT lt_seq BY vm_model code vals.
  CLEAR: it_seq, it_seq[], l_cnt, l_mitucnt.
  READ TABLE lt_seq INDEX 1.
  l_model =  lt_seq-vm_model  .
  l_code  =  lt_seq-code   .
  l_vals  =  lt_seq-vals   .
  it_seq  =  lt_seq        .

  LOOP AT lt_seq.
    IF l_model = lt_seq-vm_model AND l_code = lt_seq-code AND
       l_vals  = lt_seq-vals   .
      l_cnt   = l_cnt   + lt_seq-cnt  .
      l_mitucnt  = l_mitucnt  + lt_seq-mitucnt .
      CONTINUE.
    ELSE.
      it_seq-vm_model = l_model .
      it_seq-cnt      = l_cnt   .
      it_seq-mitucnt  = l_mitucnt  .
      APPEND it_seq          .
      it_seq  = lt_seq    .
      l_model = lt_seq-vm_model .
      l_code  = lt_seq-code  .
      l_vals  = lt_seq-vals  .
      l_cnt   = lt_seq-cnt  .
      l_mitucnt  = lt_seq-mitucnt .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_seq LINES l_size.
  IF l_size > 0.
    it_seq-vm_model = l_model .
    it_seq-cnt  = l_cnt    .
    it_seq-mitucnt = l_mitucnt   .
    APPEND it_seq         .
  ENDIF.
ENDFORM.                    " CALC_SEQ

*&---------------------------------------------------------------------*
*&      Form  CALC_BODYINPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_bodyinput.
  DATA: l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_model              LIKE it_seq-vm_model,
        l_worder             LIKE mara-matnr  ,
        l_extc               LIKE ztpp_input_plan-extc,
        l_intc               LIKE ztpp_input_plan-intc,
        l_code               LIKE it_seq-code ,
        l_days(2)            TYPE n           ,
        l_vals               LIKE it_seq-vals ,
        lt_bi                LIKE TABLE OF it_sum      WITH HEADER LINE.

  DESCRIBE TABLE it_bi LINES l_cnt  .
  CHECK l_cnt > 0 .

  SORT it_bi BY days  worder extc intc vm_model.
  READ TABLE it_bi   INDEX 1.    CLEAR: l_cnt.
  l_days   = it_seq-days    .
  l_worder = it_bi-worder  .
  l_extc   = it_bi-extc    .
  l_intc   = it_bi-intc    .
  l_model  = it_bi-vm_model.
  lt_bi    = it_bi         .

  LOOP AT it_bi             .
    IF l_days  = it_bi-days  AND l_worder = it_bi-worder AND
       l_extc   = it_bi-extc  AND l_intc = it_bi-intc   AND
       l_model = it_bi-vm_model .
      l_cnt    = l_cnt  + 1          .
      CONTINUE .
    ELSE.
      LOOP AT it_alc WHERE model = l_model .
        CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
        PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                            CHANGING lt_bi-vals                    .
        lt_bi-vm_model   = l_model      .
        lt_bi-worder     = l_worder          .
        lt_bi-extc       = l_extc            .
        lt_bi-intc       = l_intc            .
        lt_bi-cnt        = l_cnt             .
        lt_bi-code       = it_alc-full_alc   .
        APPEND lt_bi               .
      ENDLOOP.
      lt_bi    = it_bi             .
      l_worder = it_bi-worder      .
      l_extc   = it_bi-extc        .
      l_intc   = it_bi-intc        .
      l_model  = it_bi-vm_model    .
      l_cnt    = 1                  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_bi   LINES l_size .
  IF l_size > 0 .
    LOOP AT it_alc WHERE model = l_model .
      CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
      PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                          CHANGING lt_bi-vals                    .
      lt_bi-vm_model   = l_model      .
      lt_bi-worder     = l_worder          .
      lt_bi-extc       = l_extc            .
      lt_bi-intc       = l_intc            .
      lt_bi-cnt        = l_cnt             .
      lt_bi-code       = it_alc-full_alc   .
      APPEND lt_bi                .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT lt_bi  BY vm_model code vals.
  CLEAR: it_bi, it_bi[], l_cnt.
  READ TABLE lt_bi  INDEX 1.
  l_model =  lt_bi-vm_model  .
  l_code  =  lt_bi-code   .
  l_vals  =  lt_bi-vals   .
  it_bi   =  lt_bi        .

  LOOP AT lt_bi .
    IF l_model = lt_bi-vm_model AND l_code = lt_bi-code AND
       l_vals  = lt_bi-vals   .
      l_cnt   = l_cnt   + lt_bi-cnt  .
      CONTINUE.
    ELSE.
      it_bi-vm_model = l_model .
      it_bi-cnt   = l_cnt   .
      APPEND it_bi           .
      it_bi   = lt_bi    .
      l_model = lt_bi-vm_model .
      l_code  = lt_bi-code  .
      l_vals  = lt_bi-vals  .
      l_cnt   = lt_bi-cnt  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_bi  LINES l_size.
  IF l_size > 0.
    it_bi-vm_model = l_model .
    it_bi-cnt  = l_cnt    .
    APPEND it_bi          .
  ENDIF.
ENDFORM.                    " CALC_BODYINPUT

*&---------------------------------------------------------------------*
*&      Form  CALC_WBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_wbs.
  DATA: l_mitucnt            TYPE i ,
        l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_model              LIKE it_seq-vm_model,
        l_worder             LIKE mara-matnr  ,
        l_extc               LIKE ztpp_input_plan-extc,
        l_intc               LIKE ztpp_input_plan-intc,
        l_code               LIKE it_seq-code ,
        l_days(2)            TYPE n           ,
        l_vals               LIKE it_seq-vals ,
        lt_wbs               LIKE TABLE OF it_sum      WITH HEADER LINE.

  DESCRIBE TABLE it_wbs LINES l_cnt  .
  CHECK l_cnt > 0 .

  l_mitucnt =  p_wbs * wa_uph_b  .
  IF l_mitucnt > 0.
    IF l_mitucnt < l_cnt .
      DELETE it_wbs TO l_mitucnt.
    ELSE.
      DELETE it_wbs TO l_cnt    .
    ENDIF.
  ELSE.
    DELETE it_wbs TO l_cnt .
  ENDIF.
  CLEAR: l_mitucnt, l_cnt.

  SORT it_wbs BY days  worder extc intc vm_model.
  READ TABLE it_wbs  INDEX 1.    CLEAR: l_cnt.
  l_days   = it_wbs-days    .
  l_worder = it_wbs-worder  .
  l_extc   = it_wbs-extc    .
  l_intc   = it_wbs-intc    .
  l_model  = it_wbs-vm_model.
  lt_wbs   = it_wbs         .

  LOOP AT it_wbs            .
    IF l_days  = it_wbs-days  AND l_worder  = it_wbs-worder AND
       l_extc   = it_wbs-extc  AND l_intc = it_wbs-intc   AND
       l_model = it_wbs-vm_model .
      l_cnt    = l_cnt  + 1          .
      CONTINUE .
    ELSE.
      LOOP AT it_alc WHERE model = l_model .
        CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
        PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                            CHANGING lt_wbs-vals                    .
        lt_wbs-vm_model   = l_model      .
        lt_wbs-worder     = l_worder          .
        lt_wbs-extc       = l_extc            .
        lt_wbs-intc       = l_intc            .
        lt_wbs-cnt        = l_cnt             .
        lt_wbs-code       = it_alc-full_alc   .
        APPEND lt_wbs               .
      ENDLOOP.
      lt_wbs   = it_wbs             .
      l_worder = it_wbs-worder      .
      l_extc   = it_wbs-extc        .
      l_intc   = it_wbs-intc        .
      l_model  = it_wbs-vm_model    .
      l_cnt    = 1                  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_wbs  LINES l_size.
  IF l_size > 0 .
    LOOP AT it_alc WHERE model = l_model .
      CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
      PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                          CHANGING lt_wbs-vals                    .
      lt_wbs-vm_model   = l_model      .
      lt_wbs-worder     = l_worder          .
      lt_wbs-extc       = l_extc            .
      lt_wbs-intc       = l_intc            .
      lt_wbs-cnt        = l_cnt             .
      lt_wbs-code       = it_alc-full_alc   .
      APPEND lt_wbs               .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT lt_wbs BY vm_model code vals.
  CLEAR: it_wbs, it_wbs[], l_cnt.
  READ TABLE lt_wbs INDEX 1.
  l_model =  lt_wbs-vm_model  .
  l_code  =  lt_wbs-code   .
  l_vals  =  lt_wbs-vals   .
  it_wbs  =  lt_wbs        .

  LOOP AT lt_wbs.
    IF l_model = lt_wbs-vm_model AND l_code = lt_wbs-code AND
       l_vals  = lt_wbs-vals   .
      l_cnt   = l_cnt   + lt_wbs-cnt  .
      CONTINUE.
    ELSE.
      it_wbs-vm_model = l_model .
      it_wbs-cnt   = l_cnt   .
      APPEND it_wbs          .
      it_wbs  = lt_wbs    .
      l_model = lt_wbs-vm_model .
      l_code  = lt_wbs-code  .
      l_vals  = lt_wbs-vals  .
      l_cnt   = lt_wbs-cnt  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_wbs LINES l_size.
  IF l_size > 0.
    it_wbs-vm_model = l_model .
    it_wbs-cnt  = l_cnt    .
    APPEND it_wbs         .
  ENDIF.
ENDFORM.                    " CALC_WBS

*&---------------------------------------------------------------------*
*&      Form  CALC_PAINTINPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_paintinput.
  DATA: l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_atinn              LIKE cabn-atinn,
        l_model              LIKE it_seq-vm_model,
        l_worder             LIKE mara-matnr  ,
        l_extc               LIKE ztpp_input_plan-extc,
        l_intc               LIKE ztpp_input_plan-intc,
        l_days(2)            TYPE n           ,
        l_code               LIKE it_seq-code ,
        l_vals               LIKE it_seq-vals ,
        lt_pi                LIKE TABLE OF it_sum      WITH HEADER LINE.

  DESCRIBE TABLE it_pi LINES l_cnt  .
  CHECK l_cnt > 0 .

  SORT it_pi BY days  worder extc intc vm_model.
  READ TABLE it_pi   INDEX 1.    CLEAR: l_cnt.
  l_days   = it_pi-days    .
  l_worder = it_pi-worder  .
  l_extc   = it_pi-extc    .
  l_intc   = it_pi-intc    .
  l_model  = it_pi-vm_model.
  lt_pi    = it_pi         .

  LOOP AT it_pi             .
    IF l_days  = it_pi-days  AND l_worder  = it_pi-worder AND
       l_extc   = it_pi-extc  AND l_intc = it_pi-intc   AND
       l_model = it_pi-vm_model .
      l_cnt    = l_cnt  + 1          .
      CONTINUE .
    ELSE.
      LOOP AT it_alc WHERE model = l_model .
        CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
        PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                            CHANGING lt_pi-vals                    .
        lt_pi-vm_model   = l_model      .
        lt_pi-worder     = l_worder          .
        lt_pi-extc       = l_extc            .
        lt_pi-intc       = l_intc            .
        lt_pi-cnt        = l_cnt             .
        lt_pi-code       = it_alc-full_alc   .
        APPEND lt_pi               .
      ENDLOOP.
      lt_pi    = it_pi             .
      l_worder = it_pi-worder      .
      l_extc   = it_pi-extc        .
      l_intc   = it_pi-intc        .
      l_model  = it_pi-vm_model    .
      l_cnt    = 1                  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_pi   LINES l_size.
  IF l_size > 0 .
    LOOP AT it_alc WHERE model = l_model .
      CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
      PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                          CHANGING lt_pi-vals                    .
      lt_pi-vm_model   = l_model      .
      lt_pi-worder     = l_worder          .
      lt_pi-extc       = l_extc            .
      lt_pi-intc       = l_intc            .
      lt_pi-cnt        = l_cnt             .
      lt_pi-code       = it_alc-full_alc   .
      APPEND lt_pi                .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT lt_pi  BY vm_model code vals.
  CLEAR: it_pi, it_pi[], l_cnt.
  READ TABLE lt_pi  INDEX 1.
  l_model =  lt_pi-vm_model  .
  l_code  =  lt_pi-code   .
  l_vals  =  lt_pi-vals   .
  it_pi   =  lt_pi        .

  LOOP AT lt_pi .
    IF l_model = lt_pi-vm_model AND l_code = lt_pi-code AND
       l_vals  = lt_pi-vals   .
      l_cnt   = l_cnt   + lt_pi-cnt  .
      CONTINUE.
    ELSE.
      it_pi-vm_model = l_model .
      it_pi-cnt   = l_cnt   .
      APPEND it_pi           .
      it_pi   = lt_pi    .
      l_model = lt_pi-vm_model .
      l_code  = lt_pi-code  .
      l_vals  = lt_pi-vals  .
      l_cnt   = lt_pi-cnt  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_pi  LINES l_size.
  IF l_size > 0.
    it_pi-vm_model = l_model .
    it_pi-cnt  = l_cnt    .
    APPEND it_pi          .
  ENDIF.
ENDFORM.                    " CALC_PAINTINPUT

*&---------------------------------------------------------------------*
*&      Form  CALC_PAINTREJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_paintreject.
  DATA: l_mitucnt               TYPE i ,
        l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_model              LIKE it_seq-vm_model,
        l_worder             LIKE mara-matnr  ,
        l_extc               LIKE ztpp_input_plan-extc,
        l_intc               LIKE ztpp_input_plan-intc,
        l_days(2)            TYPE n           ,
        l_code               LIKE it_seq-code ,
        l_vals               LIKE it_seq-vals ,
        lt_prj               LIKE TABLE OF it_sum      WITH HEADER LINE.

  DESCRIBE TABLE it_prj LINES l_cnt  .
  CHECK l_cnt > 0 .

  l_mitucnt = p_prj * wa_uph_p.
  IF l_mitucnt > 0.
    IF l_mitucnt < l_cnt .
      DELETE it_prj TO l_mitucnt.
    ELSE.
      DELETE it_prj TO l_cnt    .
    ENDIF.
  ELSE.
    DELETE it_prj TO l_cnt .
  ENDIF.
  CLEAR: l_mitucnt, l_cnt.

  SORT it_prj BY days  worder extc intc vm_model.
  READ TABLE it_prj  INDEX 1.    CLEAR: l_cnt.
  l_days  = it_prj-days    .
  l_worder = it_prj-worder  .
  l_extc   = it_prj-extc    .
  l_intc   = it_prj-intc    .
  l_model  = it_prj-vm_model.
  lt_prj   = it_prj         .

  LOOP AT it_prj            .
    IF l_days  = it_prj-days  AND l_worder = it_prj-worder AND
       l_extc   = it_prj-extc  AND l_intc = it_prj-intc   AND
       l_model = it_prj-vm_model .
      l_cnt    = l_cnt  + 1          .
      CONTINUE .
    ELSE.
      LOOP AT it_alc WHERE model = l_model .
        CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
        PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                            CHANGING lt_prj-vals                    .
        lt_prj-vm_model   = l_model      .
        lt_prj-worder     = l_worder          .
        lt_prj-extc       = l_extc            .
        lt_prj-intc       = l_intc            .
        lt_prj-cnt        = l_cnt             .
        lt_prj-code       = it_alc-full_alc   .
        APPEND lt_prj               .
      ENDLOOP.
      lt_prj   = it_prj             .
      l_worder = it_prj-worder      .
      l_extc   = it_prj-extc        .
      l_intc   = it_prj-intc        .
      l_model  = it_prj-vm_model    .
      l_cnt    = 1                  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_prj  LINES l_size .
  IF l_size > 0 .
    LOOP AT it_alc WHERE model = l_model .
      CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
      PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                          CHANGING lt_prj-vals                    .
      lt_prj-vm_model   = l_model      .
      lt_prj-worder     = l_worder          .
      lt_prj-extc       = l_extc            .
      lt_prj-intc       = l_intc            .
      lt_prj-cnt        = l_cnt             .
      lt_prj-code       = it_alc-full_alc   .
      APPEND lt_prj               .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT lt_prj BY vm_model code vals.
  CLEAR: it_prj, it_prj[], l_cnt.
  READ TABLE lt_prj INDEX 1.
  l_model =  lt_prj-vm_model  .
  l_code  =  lt_prj-code   .
  l_vals  =  lt_prj-vals   .
  it_prj  =  lt_prj        .

  LOOP AT lt_prj.
    IF l_model = lt_prj-vm_model AND l_code = lt_prj-code AND
       l_vals  = lt_prj-vals   .
      l_cnt   = l_cnt   + lt_prj-cnt  .
      CONTINUE.
    ELSE.
      it_prj-vm_model = l_model .
      it_prj-cnt   = l_cnt   .
      APPEND it_prj          .
      it_prj  = lt_prj    .
      l_model = lt_prj-vm_model .
      l_code  = lt_prj-code  .
      l_vals  = lt_prj-vals  .
      l_cnt   = lt_prj-cnt  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_prj LINES l_size.
  IF l_size > 0.
    it_prj-vm_model = l_model .
    it_prj-cnt  = l_cnt    .
    APPEND it_prj         .
  ENDIF.
ENDFORM.                    " CALC_PAINTREJECT

*&---------------------------------------------------------------------*
*&      Form  CALC_PBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_pbs.
  DATA: l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_model              LIKE it_seq-vm_model,
        l_worder             LIKE mara-matnr  ,
        l_extc               LIKE ztpp_input_plan-extc,
        l_intc               LIKE ztpp_input_plan-intc,
        l_days(2)            TYPE n           ,
        l_code               LIKE it_seq-code ,
        l_vals               LIKE it_seq-vals ,
        lt_pbs               LIKE TABLE OF it_sum      WITH HEADER LINE.

  DESCRIBE TABLE it_pbs LINES l_cnt  .
  CHECK l_cnt > 0 .

  SORT it_pbs BY days  worder extc intc vm_model.
  READ TABLE it_pbs  INDEX 1.    CLEAR: l_cnt.
  l_days   = it_pbs-days    .
  l_worder = it_pbs-worder  .
  l_extc   = it_pbs-extc    .
  l_intc   = it_pbs-intc    .
  l_model  = it_pbs-vm_model.
  lt_pbs   = it_pbs         .

  LOOP AT it_pbs            .
    IF l_days  = it_pbs-days  AND l_worder  = it_pbs-worder AND
       l_extc   = it_pbs-extc  AND l_intc = it_pbs-intc   AND
       l_model = it_pbs-vm_model .
      l_cnt    = l_cnt  + 1          .
      CONTINUE .
    ELSE.
      LOOP AT it_alc WHERE model = l_model .
        CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
        PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                            CHANGING lt_pbs-vals                    .
        lt_pbs-vm_model   = l_model      .
        lt_pbs-worder     = l_worder          .
        lt_pbs-extc       = l_extc            .
        lt_pbs-intc       = l_intc            .
        lt_pbs-cnt        = l_cnt             .
        lt_pbs-code       = it_alc-full_alc   .
        APPEND lt_pbs               .
      ENDLOOP.
      lt_pbs   = it_pbs             .
      l_worder = it_pbs-worder      .
      l_extc   = it_pbs-extc        .
      l_intc   = it_pbs-intc        .
      l_model  = it_pbs-vm_model    .
      l_cnt    = 1                  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_pbs  LINES l_size.
  IF l_size > 0 .
    LOOP AT it_alc WHERE model = l_model .
      CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
      PERFORM get_alc_value  USING l_worder        it_alc-full_alc
                          CHANGING lt_pbs-vals                    .
      lt_pbs-vm_model   = l_model      .
      lt_pbs-worder     = l_worder          .
      lt_pbs-extc       = l_extc            .
      lt_pbs-intc       = l_intc            .
      lt_pbs-cnt        = l_cnt             .
      lt_pbs-code       = it_alc-full_alc   .
      APPEND lt_pbs               .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT lt_pbs BY vm_model code vals.
  CLEAR: it_pbs, it_pbs[], l_cnt.
  READ TABLE lt_pbs INDEX 1.
  l_model =  lt_pbs-vm_model  .
  l_code  =  lt_pbs-code   .
  l_vals  =  lt_pbs-vals   .
  it_pbs  =  lt_pbs        .

  LOOP AT lt_pbs.
    IF l_model = lt_pbs-vm_model AND l_code = lt_pbs-code AND
       l_vals  = lt_pbs-vals   .
      l_cnt   = l_cnt   + lt_pbs-cnt  .
      CONTINUE.
    ELSE.
      it_pbs-vm_model = l_model .
      it_pbs-cnt   = l_cnt   .
      APPEND it_pbs          .
      it_pbs  = lt_pbs    .
      l_model = lt_pbs-vm_model .
      l_code  = lt_pbs-code  .
      l_vals  = lt_pbs-vals  .
      l_cnt   = lt_pbs-cnt  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_pbs LINES l_size.
  IF l_size > 0.
    it_pbs-vm_model = l_model .
    it_pbs-cnt  = l_cnt    .
    APPEND it_pbs         .
  ENDIF.
ENDFORM.                    " CALC_PBS

*&---------------------------------------------------------------------*
*&      Form  INSERT_FIELD_VALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_field_vals.
  " Insert D-1 Field Value into Internal Table IT_DISP...
  LOOP AT it_disp.
    READ TABLE it_disp_d1 WITH KEY model    = it_disp-model
                                   alc_code = it_disp-alc_code
                                   alc_vals = it_disp-alc_vals .
    IF sy-subrc = 0.
      it_disp-d_1  = it_disp_d1-d_1 .
      DELETE it_disp_d1 WHERE model    = it_disp-model
                          AND alc_code = it_disp-alc_code
                          AND alc_vals = it_disp-alc_vals .
    ELSE.
      CLEAR: it_disp-d_1 .
    ENDIF.

    READ TABLE it_disp_seq  WITH KEY model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    IF sy-subrc = 0.
      it_disp-seq  = it_disp_seq-d_1 .
      it_disp-mitu = it_disp_seq-mitu.
      DELETE it_disp_seq  WHERE alc_code = it_disp-alc_code
                            AND model    = it_disp-model
                            AND alc_vals = it_disp-alc_vals .
    ELSE.
      CLEAR: it_disp-seq, it_disp-mitu.
    ENDIF.

    READ TABLE it_disp_bi   WITH KEY model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    IF sy-subrc = 0.
      it_disp-bodyin = it_disp_bi-d_1 .
      DELETE it_disp_bi   WHERE alc_code = it_disp-alc_code
                            AND model    = it_disp-model
                            AND alc_vals = it_disp-alc_vals .
    ELSE.
      CLEAR: it_disp-bodyin  .
    ENDIF.

    READ TABLE it_disp_wbs  WITH KEY model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    IF sy-subrc = 0.
      it_disp-wbs = it_disp_wbs-d_1 .
      DELETE it_disp_wbs  WHERE alc_code = it_disp-alc_code
                            AND model    = it_disp-model
                            AND alc_vals = it_disp-alc_vals .
    ELSE.
      CLEAR: it_disp-wbs .
    ENDIF.

    READ TABLE it_disp_pi   WITH KEY model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    IF sy-subrc = 0.
      it_disp-paint  = it_disp_pi-d_1 .
      DELETE it_disp_pi   WHERE alc_code = it_disp-alc_code
                            AND model    = it_disp-model
                            AND alc_vals = it_disp-alc_vals .
    ELSE.
      CLEAR: it_disp-paint  .
    ENDIF.

    READ TABLE it_disp_prj  WITH KEY model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    IF sy-subrc = 0.
      it_disp-prj = it_disp_prj-d_1 .
      DELETE it_disp_prj  WHERE alc_code = it_disp-alc_code
                            AND model    = it_disp-model
                            AND alc_vals = it_disp-alc_vals .
    ELSE.
      CLEAR: it_disp-prj .
    ENDIF.

    READ TABLE it_disp_pbs  WITH KEY model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    IF sy-subrc = 0.
      it_disp-pbs = it_disp_pbs-d_1 .
      DELETE it_disp_pbs  WHERE alc_code = it_disp-alc_code
                            AND model    = it_disp-model
                            AND alc_vals = it_disp-alc_vals .
    ELSE.
      CLEAR: it_disp-pbs .
    ENDIF.

    MODIFY it_disp.
  ENDLOOP.

  " Remain Fiedls Insert.....
  LOOP AT it_disp_d1 .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_d1 TO it_disp.
    CLEAR: it_disp-d_1, it_disp-d_1.
    it_disp-d_1 = it_disp_d1-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_d1, it_disp_d1[].

  LOOP AT it_disp_seq  .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_seq  TO it_disp.
    CLEAR: it_disp-d_1, it_disp-seq, it_disp-mitu.
    it_disp-seq  = it_disp_seq-d_1 .
    it_disp-mitu = it_disp_seq-mitu.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_seq , it_disp_seq[].

  LOOP AT it_disp_bi   .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_bi   TO it_disp.
    CLEAR: it_disp-d_1, it_disp-bodyin.
    it_disp-bodyin = it_disp_bi-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_bi  , it_disp_bi[].

  LOOP AT it_disp_wbs  .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_wbs  TO it_disp.
    CLEAR: it_disp-d_1, it_disp-wbs.
    it_disp-wbs = it_disp_wbs-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_wbs , it_disp_wbs[].

  LOOP AT it_disp_pi   .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_pi   TO it_disp.
    CLEAR: it_disp-d_1, it_disp-paint.
    it_disp-paint  = it_disp_pi-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_pi  , it_disp_pi[].

  LOOP AT it_disp_prj  .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_prj  TO it_disp.
    CLEAR: it_disp-d_1, it_disp-prj.
    it_disp-prj = it_disp_prj-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_prj , it_disp_prj[].

  LOOP AT it_disp_pbs  .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_pbs  TO it_disp.
    CLEAR: it_disp-d_1, it_disp-pbs.
    it_disp-pbs = it_disp_pbs-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_pbs , it_disp_pbs[].
ENDFORM.                    " INSERT_FIELD_VALS

*&---------------------------------------------------------------------*
*&      Form  calc_prod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_prod.
  DATA: l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_model              LIKE it_seq-vm_model,
        l_worder             LIKE mara-matnr  ,
        l_extc               LIKE ztpp_input_plan-extc,
        l_intc               LIKE ztpp_input_plan-intc,
        l_code               LIKE it_seq-code ,
        l_vals               LIKE it_seq-vals ,
        lt_d1                LIKE TABLE OF it_sum      WITH HEADER LINE.

  "1. Take the data of The Table(IT_PROD) of which RP06's Qty is GT 0.

  DELETE it_prod WHERE rp06q = 0 .

  "2. Conversion the Data for the Internal Table Format..
  LOOP AT it_prod.
    CLEAR: it_d1 .
    CONCATENATE it_prod-wo_ser it_prod-nation it_prod-dealer
                it_prod-extc   it_prod-intc   INTO l_worder .
    PERFORM get_model USING l_worder  it_d1-vm_model .
    DO it_prod-rp06q TIMES.
      it_d1-worder     = l_worder          .
      it_d1-extc       = it_prod-extc      .
      it_d1-intc       = it_prod-intc      .
      it_d1-rp         = '06'              .
      it_d1-cnt        = 1                 .
      APPEND it_d1  .
    ENDDO.
  ENDLOOP.

  " Create the Previous Production Data..
  SORT it_d1 BY days  worder extc intc vm_model.
  READ TABLE  it_d1 INDEX 1.   CLEAR: l_cnt.
  l_worder = it_d1-worder  .
  l_extc   = it_d1-extc    .
  l_intc   = it_d1-intc    .
  l_model  = it_d1-vm_model.
  lt_d1    = it_d1         .

  LOOP AT it_d1.
    IF l_worder = it_d1-worder AND  l_extc   = it_d1-extc   AND
       l_intc   = it_d1-intc   AND  l_model  = it_d1-vm_model .
      l_cnt    = l_cnt  + 1.
      CONTINUE .
    ELSE.
      LOOP AT it_alc WHERE model = l_model .
        CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
        PERFORM get_alc_value  USING l_worder      it_alc-full_alc
                            CHANGING lt_d1-vals                    .
        lt_d1-vm_model   = l_model           .
        lt_d1-worder     = l_worder          .
        lt_d1-extc       = l_extc            .
        lt_d1-intc       = l_intc            .
        lt_d1-cnt        = l_cnt             .
        lt_d1-code       = it_alc-full_alc   .
        APPEND lt_d1                .
      ENDLOOP.
      lt_d1    = it_d1              .
      l_worder = it_d1-worder      .
      l_extc   = it_d1-extc        .
      l_intc   = it_d1-intc        .
      l_model  = it_d1-vm_model    .
      l_cnt    = 1                  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_d1 LINES l_size .
  IF l_size > 0 .
    LOOP AT it_alc WHERE model = l_model .
      CONCATENATE  l_worder  l_extc   l_intc  INTO l_worder       .
      PERFORM get_alc_value  USING l_worder      it_alc-full_alc
                          CHANGING lt_d1-vals                    .
      lt_d1-vm_model   = l_model           .
      lt_d1-worder     = l_worder          .
      lt_d1-extc       = l_extc            .
      lt_d1-intc       = l_intc            .
      lt_d1-cnt        = l_cnt             .
      lt_d1-code       = it_alc-full_alc   .
      APPEND lt_d1                .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT lt_d1 BY vm_model code vals.
  CLEAR: it_d1, it_d1[], l_cnt.
  READ TABLE lt_d1 INDEX 1.
  l_model =  lt_d1-vm_model  .
  l_code  =  lt_d1-code   .
  l_vals  =  lt_d1-vals   .
  it_d1   =  lt_d1        .

  LOOP AT lt_d1.
    IF l_model = lt_d1-vm_model AND l_code = lt_d1-code AND
       l_vals  = lt_d1-vals   .
      l_cnt   = l_cnt   + lt_d1-cnt  .
      CONTINUE.
    ELSE.
      it_d1-vm_model = l_model .
      it_d1-cnt   = l_cnt   .
      APPEND it_d1          .
      it_d1   = lt_d1    .
      l_model = lt_d1-vm_model .
      l_code  = lt_d1-code  .
      l_vals  = lt_d1-vals  .
      l_cnt   = lt_d1-cnt  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_d1 LINES l_size.
  IF l_size > 0.
    it_d1-vm_model = l_model .
    it_d1-cnt  = l_cnt    .
    APPEND it_d1         .
  ENDIF.
ENDFORM.                    " calc_prod

*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = pa_type
      date                         = pa_wdate
      factory_calendar_id          = pa_kalid
    IMPORTING
      date                         = pa_wdate
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'T'   .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  GET_MODELS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_models.
  DATA: l_atwrt          LIKE ausp-atwrt,
        l_atinn          LIKE cabn-atinn.

  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = 'P_MODEL'.

  SELECT n~atwrt INTO l_atwrt
    FROM cawn AS n INNER JOIN cawnt AS t
      ON n~atinn = t~atinn
     AND n~atzhl = t~atzhl
   WHERE n~atinn = l_atinn
     AND t~spras = sy-langu .
    it_model-modl = l_atwrt(3).
    APPEND it_model .
  ENDSELECT.
ENDFORM.                    " GET_MODELS

*&---------------------------------------------------------------------*
*&      Form  SET_INFORMATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_information.
  DATA: l_date               TYPE d ,
        l_count              TYPE i .

  " Set the BASIC Information for the UPH & Work Time...
  CLEAR: l_date, l_count.
  l_date = p_dates  .
  PERFORM read_working_date USING '+'  wa_kalid  l_date.
  IF l_date = p_dates  .
    PERFORM get_day       USING l_date it_master-day  .
    PERFORM get_worktime1 USING l_date it_master-time it_master-day 'T'.
    PERFORM get_uph      USING l_date it_master-uph it_master-shift 'T'.
    PERFORM get_uph_shop     USING l_date  wa_uph_t   'T'              .
    PERFORM get_uph_shop     USING l_date  wa_uph_b   'B'              .
    PERFORM get_uph_shop     USING l_date  wa_uph_p   'P'              .
  ELSE.
    l_date = p_dates .
    CLEAR: wa_uph_t, wa_uph_b, wa_uph_p.
    PERFORM get_day         USING l_date it_master-day  .
  ENDIF.
  it_master-seq    = l_count.    it_master-date   = l_date .
  APPEND it_master.  CLEAR: it_master.

  " From D+1 Day To D+21 Day....(Only Working Dates in FACTORY-Calendar)
  l_date = p_dates  .
  DO 21 TIMES.
    l_count  = l_count + 1.
    PERFORM read_working_date USING '+'  wa_kalid  l_date.
    PERFORM get_day       USING l_date it_master-day  .
    PERFORM get_worktime1 USING l_date it_master-time it_master-day  'T'.
    PERFORM get_uph       USING l_date it_master-uph it_master-shift 'T'.
    it_master-seq    = l_count.
    it_master-date   = l_date .
    APPEND it_master.  CLEAR: it_master.
    l_date   = l_date  + 1.
  ENDDO.
ENDFORM.                    " SET_INFORMATION

*&---------------------------------------------------------------------*
*&      Form  GET_WORKTIME1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
FORM get_worktime1 USING    pa_wdate  pa_wktime  pa_day  pa_arbpl.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_date        TYPE d ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl =  pa_arbpl
     AND datub >= pa_wdate .

  SORT lt_capa BY datub .
  READ TABLE lt_capa INDEX 1.
  l_date = lt_capa-datub    .

  LOOP AT lt_capa WHERE datub = l_date AND tagnr = pa_day .
    CLEAR: l_einzt.
    SELECT SINGLE einzt INTO l_einzt
      FROM tc37a
     WHERE schgrup  = lt_capa-mosid
       AND kaptprog = lt_capa-tprog
       AND endda   >= pa_wdate
       AND begda   <= pa_wdate     .
    l_wtime  = l_wtime  + l_einzt.
  ENDLOOP.
  pa_wktime = l_wtime .
ENDFORM.                    " GET_WORKTIME1

*&---------------------------------------------------------------------*
*&      Form  GET_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM get_day USING    pa_wdate  pa_day.
  DATA: l_day         LIKE scal-indicator .

  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date = pa_wdate
    IMPORTING
      day  = l_day.

  pa_day = l_day.
ENDFORM.                    " GET_DAY

*&---------------------------------------------------------------------*
*&      Form  GET_UPH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_UPH  text
*----------------------------------------------------------------------*
FORM get_uph USING    pa_wdate  pa_uph  pa_shift  pa_arbpl .

  DATA: w_uph  LIKE ztpp_status-uph.
  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      date  = pa_wdate
      shift = pa_shift
      shop  = pa_arbpl
    IMPORTING
      uph   = w_uph.
  pa_uph   = w_uph.

*  DATA lw_ld          LIKE zvpp_ld .
*data: lt_ld   like zvpp_ld occurs 0 with header line.
*
*  IF pa_shift IS INITIAL .
** requested by MY HUR changed by chris
** because two shift could exist, read one record
** only one shift is calculated
**    SELECT SINGLE * INTO lw_ld
**      FROM zvpp_ld
**     WHERE ld_perst <= pa_wdate
**       AND ld_pered >= pa_wdate
**       AND arbpl     = 'T'      .
*    SELECT * INTO table lt_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND arbpl     = pa_arbpl     .
*
** end of change on 06/13/2005
*  ELSE.
** requested by MY HUR changed by chris
** because two shift could exist, read one record
** only one shift is calculated
** and one shift could have more than one record
** to difine diferent rate for different period
** of time
**    SELECT SINGLE * INTO lw_ld
**      FROM zvpp_ld
**     WHERE ld_perst <= pa_wdate
**       AND ld_pered >= pa_wdate
**       AND ld_shift  = pa_shift
**       AND arbpl     = 'T'      .
*    SELECT * INTO table lt_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND ld_shift  = pa_shift
*       AND arbpl     = pa_arbpl    .
*
*
*  ENDIF.
** add by chris on 06/13/2005
*    loop at lt_ld.
*      lw_ld-lrate = lw_ld-lrate + lt_ld-lrate.
*      lw_ld-lantu = lw_ld-lantu + lt_ld-lantu.
*    endloop.
** end of add.
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
ENDFORM.                    " GET_UPH

*&---------------------------------------------------------------------*
*&      Form  GET_UPH_SHOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_UPH  text
*----------------------------------------------------------------------*
FORM get_uph_shop USING    pa_wdate  pa_uph  pa_arbpl .
  DATA: w_uph  LIKE ztpp_status-uph.
  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      date  = pa_wdate
*     SHIFT = PA_SHIFT
      shop  = pa_arbpl
    IMPORTING
      uph   = w_uph.
  pa_uph = w_uph.
*  DATA lw_ld          LIKE zvpp_ld .
*
*  SELECT SINGLE * INTO lw_ld
*    FROM zvpp_ld
*   WHERE ld_perst <= pa_wdate
*     AND ld_pered >= pa_wdate
*     AND arbpl     = pa_arbpl .
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
ENDFORM.                    " GET_UPH_SHOP

*&---------------------------------------------------------------------*
*&      Form  GET_START_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM get_start_day USING    pa_datum.
  CLEAR: ztpp_common_vals.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = c_jobs
     AND key2 = c_key1 .

  pa_datum = ztpp_common_vals-dates.
ENDFORM.                    " GET_START_DAY

*&---------------------------------------------------------------------*
*&      Form  CLEAR_QTY_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DISP_PRJ  text
*----------------------------------------------------------------------*
FORM clear_qty_disp USING    pa_disp  STRUCTURE it_disp .
  CLEAR: pa_disp-d01, pa_disp-d02, pa_disp-d03, pa_disp-d04,
         pa_disp-d05, pa_disp-d06, pa_disp-d07, pa_disp-d08,
         pa_disp-d09, pa_disp-d10, pa_disp-d11, pa_disp-d12,
         pa_disp-d13, pa_disp-d14, pa_disp-d15, pa_disp-d16,
         pa_disp-d17, pa_disp-d18, pa_disp-d19, pa_disp-d20,
         pa_disp-d21, pa_disp-d_1, pa_disp-seq, pa_disp-bodyin,
         pa_disp-wbs, pa_disp-prj, pa_disp-pbs, pa_disp-paint,
         pa_disp-mitu.                "MISSED     UD1K912950
ENDFORM.                    " CLEAR_QTY_DISP

*&---------------------------------------------------------------------*
*&      Form  GET_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORDER  text
*      -->P_IT_D1_VM_MODEL  text
*----------------------------------------------------------------------*
FORM get_model USING    pa_worder   pa_model.
  DATA: lt_val          LIKE TABLE OF zspp_vin_value   WITH HEADER LINE.

  lt_val-atnam = 'P_MODEL'.  APPEND lt_val.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = pa_worder
      ctype        = '001'
*     DISPLAY      = 'D'
    TABLES
      val_table    = lt_val
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  READ TABLE lt_val  INDEX 1.
  IF sy-subrc = 0 AND lt_val-zflag IS INITIAL.
    pa_model = lt_val-atwrt.
  ELSE.
    CLEAR: pa_model.
  ENDIF.
ENDFORM.                    " GET_MODEL
*&---------------------------------------------------------------------*
*&      Form  SUB_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_total.
  LOOP AT it_disp.
    it_disp-stot =
           it_disp-d01 + it_disp-d02 + it_disp-d03 + it_disp-d04
         + it_disp-d05 + it_disp-d06 + it_disp-d07 + it_disp-d08
         + it_disp-d09 + it_disp-d10 + it_disp-d11 + it_disp-d12
         + it_disp-d13 + it_disp-d14 + it_disp-d15 + it_disp-d16
         + it_disp-d17 + it_disp-d18 + it_disp-d19 + it_disp-d20
         + it_disp-d21.
    MODIFY it_disp.
  ENDLOOP.
ENDFORM.                    " SUB_TOTAL
*&---------------------------------------------------------------------*
*&      Form  check_holding_car
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_holding_car.
  DATA : it_hold LIKE ztpp_hold_car OCCURS 0 WITH HEADER LINE.

  REFRESH it_hold.

  SELECT * INTO TABLE it_hold FROM ztpp_hold_car
         WHERE  ( status EQ 'W' ) OR ( status EQ space ).

  LOOP AT it_hold WHERE res_date > p_dates.
    READ TABLE it_data WITH KEY modl = it_hold-modl
                            body_ser = it_hold-body_ser.
    IF sy-subrc = 0.
*          DELETE TABLE it_data FROM it_data.
      it_hold-status = 'W'.
      MODIFY it_hold FROM it_hold.
    ELSE.
      it_hold-status = 'P'.
      MODIFY it_hold FROM it_hold.
    ENDIF.
  ENDLOOP.

  LOOP AT it_hold WHERE res_date <= p_dates.
    IF it_hold-res_date =  p_dates.
      it_hold-status = 'D'.
    ELSE.
      READ TABLE it_data WITH KEY modl = it_hold-modl
                           body_ser = it_hold-body_ser.
      IF sy-subrc = 0.
        DELETE TABLE it_data FROM it_data.
      ENDIF.
      it_hold-status = 'P'.
    ENDIF.
    MODIFY it_hold FROM it_hold.
  ENDLOOP.


  MODIFY ztpp_hold_car FROM TABLE it_hold.
  IF sy-subrc = 0.
    MESSAGE s000 WITH text-003.
    COMMIT WORK.
  ELSE.
    MESSAGE s000 WITH text-004.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " check_holding_car
