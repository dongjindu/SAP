************************************************************************
* Program Name      : ZAPP903R_ALC_SHORT
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : APP903: VEHICLE SCHEDULING
*
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
************************************************************************
REPORT  zapp903r_alc_short    MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: zvpp_vehicle,
        ztpp_input_plan,
        ausp ,
        crhd .

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF it_temp    OCCURS 0,
        objek             LIKE ausp-objek,      " EQUI-EQUNR
        atinn             LIKE ausp-atinn,      " CHARACTERISTIC
        atwrt             LIKE ausp-atwrt,      " CHARACTERISTIC VAL
        atflv             LIKE ausp-atflv,      " Date & Number  VAL
      END OF it_temp .

DATA: BEGIN OF it_data        OCCURS 0,
        objek                 LIKE ausp-objek.      " Vehicle Code
        INCLUDE STRUCTURE     ztpp_input_plan.
DATA: END OF it_data.

DATA: BEGIN OF it_alc         OCCURS 0.
        INCLUDE STRUCTURE     cukb    .
DATA:   knktx                 LIKE cukbt-knktx,
        code(3)               TYPE c  ,
        rp(2)                 TYPE n  ,
        type_alc              TYPE c  ,
        char_alc              LIKE cabn-atnam,
      END OF it_alc .

DATA: BEGIN OF it_sum         OCCURS 0,
        rp(2)                 TYPE n  ,
        alc(9)                TYPE c  ,              " For Summary Field
        worder                LIKE mara-matnr,
        knnam                 LIKE cukb-knnam,
        status                LIKE ztpp_input_plan-status,
        code(4)               TYPE c  ,              " ALC CODE
        vals(5)               TYPE c  ,              " ALC CODE VALUE
        hours                 TYPE i  ,
        vm_model              LIKE ztpp_input_plan-modl ,
        vm_bodyser            LIKE ztpp_input_plan-body_ser,
        extc                  LIKE ztpp_input_plan-extc,
        intc                  LIKE ztpp_input_plan-intc,
        cnt                   TYPE i  ,
      END OF it_sum .

DATA: BEGIN OF it_seq         OCCURS 0.
        INCLUDE STRUCTURE     zspp_vm_gen.
DATA:   alc(11)               TYPE c  ,              " For Summary Field
        knnam                 LIKE cukb-knnam,
        cnt                   TYPE i  ,
        code(4)               TYPE c  ,              " ALC CODE
        vals(5)               TYPE c  ,              " ALC CODE VALUE
      END OF it_seq.

DATA: it_bi                 LIKE TABLE OF it_seq       WITH HEADER LINE,
      it_wbs                LIKE TABLE OF it_seq       WITH HEADER LINE,
      it_pi                 LIKE TABLE OF it_seq       WITH HEADER LINE,
      it_prj                LIKE TABLE OF it_seq       WITH HEADER LINE,
      it_pbs                LIKE TABLE OF it_seq       WITH HEADER LINE,
      it_summary            LIKE TABLE OF it_sum       WITH HEADER LINE,
      it_disp               LIKE TABLE OF ztpp_alc_sum WITH HEADER LINE.

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: wa_part             LIKE it_disp                             ,
      wa_data             LIKE it_data                             ,
      wa_wdate            LIKE sy-datum                            ,
      wa_uzeit            LIKE sy-uzeit                            ,
      wa_index            LIKE sy-tabix                            ,
      wa_error            TYPE c                                   ,
      wa_hour             TYPE i                                   ,
      wa_serial           LIKE ztpp_dvrt1-serial                   .

*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
FIELD-SYMBOLS: <wa_dfield>    TYPE ANY.

*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------
CONTROLS: tc_9000 TYPE TABLEVIEW USING SCREEN 9000,
          tc_9100 TYPE TABLEVIEW USING SCREEN 9100.

DATA:     ok_code LIKE sy-ucomm,
          sv_code LIKE sy-ucomm.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS: p_model(3)    TYPE c        ,
            p_uph(2)      TYPE n        OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM clear_variable .
  PERFORM read_inputplan      .
  PERFORM read_alc_model USING p_model .
  PERFORM create_summary .
  PERFORM display_data   .

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
  CLEAR: it_temp,   it_data,   it_alc,   it_summary,   it_sum,  it_disp,
         it_temp[], it_data[], it_alc[], it_summary[], it_sum[],
         it_disp[], wa_data, wa_wdate, wa_uzeit, wa_index, wa_hour,
         wa_serial.
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
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_input_plan .

  DESCRIBE TABLE it_data LINES  wa_hour .
  IF wa_hour = 0.
    DELETE FROM ztpp_alc_sum  CLIENT SPECIFIED WHERE mandt = sy-mandt.
    LEAVE PROGRAM .
  ENDIF.
  SORT it_data BY status DESCENDING rp17 rp16 rp15 rp14 rp13 rp12 rp11
            rp10 rp09 rp08 rp07 rp06 rp05 rp04 rp03 rp02 rp01 body_ser.
ENDFORM.                    " READ_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  READ_ALC_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MODEL  text
*----------------------------------------------------------------------*
FORM read_alc_model USING    pa_model.
  DATA: l_knobj              LIKE cuco-knobj,
        l_knnum              LIKE cuob-knobj,
        l_knnam              LIKE cukb-knnam.

  CONCATENATE pa_model '_WOHD'           INTO  l_knnam.
  PERFORM get_knobj                      USING l_knnam  l_knobj.
  PERFORM get_knnum                      USING l_knobj.
  CONCATENATE pa_model '_WOCL'           INTO  l_knnam.
  PERFORM get_knobj                      USING l_knnam  l_knobj.
  PERFORM get_knnum                      USING l_knobj.

  LOOP AT it_alc.
    SELECT SINGLE b~knnam t~knktx
      INTO CORRESPONDING FIELDS OF it_alc
      FROM cukb AS b INNER JOIN cukbt AS t
        ON b~knnum = t~knnum
     WHERE b~knnum = it_alc-knnum
       AND t~spras = sy-langu   .

    IF it_alc-knnam(10) NE 'D_EMF_ALC_' .
      DELETE it_alc .
      CONTINUE .
    ENDIF.
    it_alc-code     = it_alc-knnam+12(3) .
    it_alc-type_alc = it_alc-knnam+10(1) .
    it_alc-rp       = it_alc-knktx(2)    .
    CONCATENATE 'P' it_alc-knnam+5(10)  INTO it_alc-char_alc .
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
  DATA: lt_data              LIKE TABLE OF it_data     WITH HEADER LINE.

  PERFORM inline_status .
  lt_data[] = it_data[].

  LOOP AT it_alc.
    it_data[] = lt_data[].
    CLEAR: wa_index, wa_hour, it_sum, it_sum[].
    " Read it_data for the time period and then make the ALC Summary
    PERFORM read_internal_table .
    PERFORM read_alc            .
    " Calculate the work-order's ALC Value..
    PERFORM calc_alc            .
  ENDLOOP.

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
        l_hours              TYPE i ,
        l_pos                TYPE i ,
        l_start              LIKE sy-tabix,
        l_end                LIKE sy-tabix,
        l_index              LIKE sy-tabix.

  CONCATENATE 'RP' it_alc-rp  INTO  l_name .
  DESCRIBE TABLE it_data      LINES l_max  .
  CASE it_alc-rp.
    WHEN '01'.             " REPORTING POINT 01
      READ TABLE it_data WITH KEY rp01 = space .
    WHEN '02'.             " REPORTING POINT 02
      READ TABLE it_data WITH KEY rp02 = space .
    WHEN '03'.             " REPORTING POINT 03
      READ TABLE it_data WITH KEY rp03 = space .
    WHEN '04'.             " REPORTING POINT 04
      READ TABLE it_data WITH KEY rp04 = space .
    WHEN '05'.             " REPORTING POINT 05
      READ TABLE it_data WITH KEY rp05 = space .
    WHEN '06'.             " REPORTING POINT 06
      READ TABLE it_data WITH KEY rp06 = space .
    WHEN '07'.             " REPORTING POINT 07
      READ TABLE it_data WITH KEY rp07 = space .
    WHEN '08'.             " REPORTING POINT 08
      READ TABLE it_data WITH KEY rp08 = space .
    WHEN '09'.             " REPORTING POINT 09
      READ TABLE it_data WITH KEY rp09 = space .
    WHEN '10'.             " REPORTING POINT 10
      READ TABLE it_data WITH KEY rp10 = space .
    WHEN '11'.             " REPORTING POINT 11
      READ TABLE it_data WITH KEY rp11 = space .
    WHEN '12'.             " REPORTING POINT 12
      READ TABLE it_data WITH KEY rp12 = space .
    WHEN '13'.             " REPORTING POINT 13
      READ TABLE it_data WITH KEY rp13 = space .
    WHEN '14'.             " REPORTING POINT 14
      READ TABLE it_data WITH KEY rp14 = space .
    WHEN '15'.             " REPORTING POINT 15
      READ TABLE it_data WITH KEY rp15 = space .
    WHEN '16'.             " REPORTING POINT 16
      READ TABLE it_data WITH KEY rp16 = space .
    WHEN '17'.             " REPORTING POINT 17
      READ TABLE it_data WITH KEY rp17 = space .
    WHEN '18'.             " REPORTING POINT 18
      READ TABLE it_data WITH KEY rp18 = space .
  ENDCASE.
  l_index = sy-tabix .
  IF l_index > 1.
    l_index = l_index - 1 .
    DELETE it_data FROM 1 TO l_index.
  ENDIF.
  l_index = 1.

  " 1 Hour * 20 & 10 Hour *  2 & 20 Hours.
  " Current Time check & Define the Loop count...
  DO 20 TIMES.
    IF l_flag = 'X'.   EXIT.   ENDIF.
    l_pos = l_pos + p_uph .
    IF l_pos >= l_max.
      wa_index = l_max - l_index.
      wa_hour  = l_hours.
      l_pos = l_max.
      l_flag = 'X' .
    ENDIF.
    l_hours = l_hours + 1 .
    LOOP AT it_data FROM l_index TO l_pos.
      CLEAR: it_sum.
      it_sum-hours = l_hours .
      it_sum-vm_model = it_data-modl .
      it_sum-vm_bodyser = it_data-body_ser.
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
    l_index = l_pos + 1 .
  ENDDO.

  CHECK l_flag = space.

  " Check the INDEX .
  DO 2  TIMES.
    IF l_flag = 'X'.   EXIT.   ENDIF.
    l_pos = l_pos + ( p_uph * 10 )  .
    IF l_pos >= l_max.
      wa_index = l_max - l_index.        " Current's Remain records..
      wa_hour  = l_hours.
      l_pos = l_max.
      l_flag = 'X' .
    ENDIF.
    l_hours = l_hours + 10.
    LOOP AT it_data FROM l_index TO l_pos.
      CLEAR: it_sum.
      it_sum-hours = l_hours .
      it_sum-vm_model = it_data-modl .
      it_sum-vm_bodyser = it_data-body_ser.
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
    l_index = l_pos .
  ENDDO.

  CHECK l_flag = space.

  " List-Up the 20H Data..
  l_pos = l_pos + ( p_uph * 20 ) .
  IF l_pos >= l_max.
    wa_index = l_max - l_index.        " Current's Remain records..
    wa_hour  = l_hours.
    l_pos = l_max.
    l_flag = 'X'.
  ENDIF.
  l_hours = l_hours + 20.
  LOOP AT it_data FROM l_index TO l_pos.
    CLEAR: it_sum.
    it_sum-hours = l_hours .
    it_sum-vm_model = it_data-modl .
    it_sum-vm_bodyser = it_data-body_ser.
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
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals(5)               TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  SORT it_sum BY vm_model hours code vals.
  READ TABLE it_sum INDEX 1.
  l_hours  = it_sum-hours   .
  l_code   = it_sum-code    .
  l_vals   = it_sum-vals    .
  l_model  = it_sum-vm_model.

  CONCATENATE 'IT_DISP-H'  l_hours  INTO l_name.
  ASSIGN (l_name)                   TO   <wa_dfield>.
  CLEAR: <wa_dfield>.

  LOOP AT it_sum.
    IF l_hours = it_sum-hours AND l_code  = it_sum-code AND
       l_vals  = it_sum-vals  .
      <wa_dfield> = <wa_dfield> + it_sum-cnt .
      CONTINUE.
    ELSE.
      it_disp-model    = l_model  .
      it_disp-alc_code = l_code   .
      it_disp-alc_vals = l_vals   .
      it_disp-rp       = it_alc-rp.
      APPEND it_disp.    CLEAR: it_disp.
      l_hours = it_sum-hours      .
      l_code  = it_sum-code       .
      l_vals  = it_sum-vals       .
      l_model = it_sum-vm_model   .
      CONCATENATE 'IT_DISP-H'  l_hours  INTO l_name.
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
            OTHERS       = 4.

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
  DELETE FROM ztpp_alc_sum  CLIENT SPECIFIED WHERE mandt = sy-mandt.
  SORT it_disp BY serial alc_code model DESCENDING alc_vals .
  LOOP AT it_disp.
    LOOP AT it_seq WHERE model = it_disp-model    AND
                         code  = it_disp-alc_code AND
                         vals  = it_disp-alc_vals .
      it_disp-seq = it_disp-seq + it_seq-cnt     .
    ENDLOOP.
    LOOP AT it_bi  WHERE model = it_disp-model    AND
                         code  = it_disp-alc_code AND
                         vals  = it_disp-alc_vals .
      it_disp-BODYIN = it_disp-BODYIN + it_bi-cnt     .
    ENDLOOP.
    LOOP AT it_pi WHERE  model = it_disp-model    AND
                         code  = it_disp-alc_code AND
                         vals  = it_disp-alc_vals .
      it_disp-PAINT = it_disp-PAINT + it_pi-cnt     .
    ENDLOOP.
    LOOP AT it_pbs WHERE model = it_disp-model    AND
                         code  = it_disp-alc_code AND
                         vals  = it_disp-alc_vals .
      it_disp-pbs = it_disp-pbs + it_pbs-cnt     .
    ENDLOOP.
    LOOP AT it_prj WHERE model = it_disp-model    AND
                         code  = it_disp-alc_code AND
                         vals  = it_disp-alc_vals .
      it_disp-prj = it_disp-prj + it_prj-cnt     .
    ENDLOOP.
    LOOP AT it_wbs WHERE model = it_disp-model    AND
                         code  = it_disp-alc_code AND
                         vals  = it_disp-alc_vals .
      it_disp-wbs = it_disp-wbs + it_wbs-cnt     .
    ENDLOOP.
    MODIFY it_disp.
  ENDLOOP.
  MODIFY ztpp_alc_sum       FROM TABLE it_disp .
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
ENDFORM.                    " INLINE_STATUS

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'TEST'.
  SET TITLEBAR 'TEST'.
ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  sv_code = ok_code.
  CLEAR: ok_code.
  IF sv_code = 'BACK'.   LEAVE TO SCREEN 0.  ENDIF.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  add_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DISP  text
*----------------------------------------------------------------------*
FORM add_routine USING    pa_disp  LIKE  it_disp.
  it_disp-h01 = it_disp-h01 + pa_disp-h01 .
  it_disp-h02 = it_disp-h02 + pa_disp-h02 .
  it_disp-h03 = it_disp-h03 + pa_disp-h03 .
  it_disp-h04 = it_disp-h04 + pa_disp-h04 .
  it_disp-h05 = it_disp-h05 + pa_disp-h05 .
  it_disp-h06 = it_disp-h06 + pa_disp-h06 .
  it_disp-h07 = it_disp-h07 + pa_disp-h07 .
  it_disp-h08 = it_disp-h08 + pa_disp-h08 .
  it_disp-h09 = it_disp-h09 + pa_disp-h09 .
  it_disp-h10 = it_disp-h10 + pa_disp-h10 .
  it_disp-h11 = it_disp-h11 + pa_disp-h11 .
  it_disp-h12 = it_disp-h12 + pa_disp-h12 .
  it_disp-h13 = it_disp-h13 + pa_disp-h13 .
  it_disp-h14 = it_disp-h14 + pa_disp-h14 .
  it_disp-h15 = it_disp-h15 + pa_disp-h15 .
  it_disp-h16 = it_disp-h16 + pa_disp-h16 .
  it_disp-h17 = it_disp-h17 + pa_disp-h17 .
  it_disp-h18 = it_disp-h18 + pa_disp-h18 .
  it_disp-h19 = it_disp-h19 + pa_disp-h19 .
  it_disp-h20 = it_disp-h20 + pa_disp-h20 .
  it_disp-h30 = it_disp-h30 + pa_disp-h30 .
  it_disp-h40 = it_disp-h40 + pa_disp-h40 .
  it_disp-h60 = it_disp-h60 + pa_disp-h60 .
ENDFORM.                    " add_routine

*&---------------------------------------------------------------------*
*&      Form  tot_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DISP  text
*----------------------------------------------------------------------*
FORM tot_routine USING    pa_disp  LIKE  it_disp.
  wa_part-h01 = wa_part-h01 + pa_disp-h01 .
  wa_part-h02 = wa_part-h02 + pa_disp-h02 .
  wa_part-h03 = wa_part-h03 + pa_disp-h03 .
  wa_part-h04 = wa_part-h04 + pa_disp-h04 .
  wa_part-h05 = wa_part-h05 + pa_disp-h05 .
  wa_part-h06 = wa_part-h06 + pa_disp-h06 .
  wa_part-h07 = wa_part-h07 + pa_disp-h07 .
  wa_part-h08 = wa_part-h08 + pa_disp-h08 .
  wa_part-h09 = wa_part-h09 + pa_disp-h09 .
  wa_part-h10 = wa_part-h10 + pa_disp-h10 .
  wa_part-h11 = wa_part-h11 + pa_disp-h11 .
  wa_part-h12 = wa_part-h12 + pa_disp-h12 .
  wa_part-h13 = wa_part-h13 + pa_disp-h13 .
  wa_part-h14 = wa_part-h14 + pa_disp-h14 .
  wa_part-h15 = wa_part-h15 + pa_disp-h15 .
  wa_part-h16 = wa_part-h16 + pa_disp-h16 .
  wa_part-h17 = wa_part-h17 + pa_disp-h17 .
  wa_part-h18 = wa_part-h18 + pa_disp-h18 .
  wa_part-h19 = wa_part-h19 + pa_disp-h19 .
  wa_part-h20 = wa_part-h20 + pa_disp-h20 .
  wa_part-h30 = wa_part-h30 + pa_disp-h30 .
  wa_part-h40 = wa_part-h40 + pa_disp-h40 .
  wa_part-h60 = wa_part-h60 + pa_disp-h60 .
ENDFORM.                    " tot_routine

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
  it_disp = lt_disp.

  LOOP AT lt_disp.
    IF l_model = lt_disp-model  AND  l_code  = lt_disp-alc_code  AND
       l_vals  = lt_disp-alc_vals.
      PERFORM add_routine  USING  lt_disp .
*     PERFORM tot_routine  USING  lt_disp .
      CONTINUE.
    ELSE.
      CONDENSE lt_disp-alc_code .
      it_disp-serial = strlen( l_vals )           .
      APPEND it_disp.
*      IF l_code  = lt_disp-alc_code .
*      ELSE.
*        it_disp = wa_part.     it_disp-alc_vals = 'TOTAL'.
*        condense lt_disp-alc_code .
*        it_disp-serial = strlen( l_vals )           .
*        clear: it_disp-model.  APPEND it_disp.
*        wa_part = lt_disp .
*      ENDIF.
      it_disp = lt_disp.
      l_model = lt_disp-model   .
      l_code  = lt_disp-alc_code.
      l_vals  = lt_disp-alc_vals.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_disp LINES  l_line.
  IF l_line > 0.
    CONDENSE lt_disp-alc_code .
    it_disp-serial = strlen( l_vals )           .
    APPEND it_disp.
*    it_disp = wa_part.     it_disp-alc_vals = 'TOTAL'.
*    condense lt_disp-alc_code .
*    it_disp-serial = strlen( l_vals )           .
*    clear: it_disp-model.  APPEND it_disp.
  ENDIF.
ENDFORM.                    " SUMMARY_DISP

*&---------------------------------------------------------------------*
*&      Form  GET_KNOBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM get_knobj USING    pa_knnam  pa_knobj.
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
        L_WORDER            LIKE MARA-MATNR  ,
        l_hours             LIKE it_sum-hours,
        l_wo                LIKE mara-matnr  ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  SORT it_sum BY hours worder extc intc .
  READ TABLE it_sum INDEX 1.
  l_hours = it_sum-hours   .
  l_wo    = it_sum-worder  .
  l_ext   = it_sum-extc    .
  l_int   = it_sum-intc    .
  l_model = it_sum-vm_model  .

  " Work Order Summarize in the same time terms.
  LOOP AT it_sum.
    IF l_hours = it_sum-hours AND l_wo  = it_sum-worder AND
       l_ext   = it_sum-extc  AND l_int = it_sum-intc   AND
       l_model = it_sum-vm_model .
      lt_sum  = it_sum          .
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      PERFORM GET_WORORDER   USING IT_ALC-type_alc  L_WORDER
                     it_sum-worder it_sum-EXTC      it_sum-INTC .
      PERFORM get_alc_value  USING L_WORDER       lt_sum-code
                          CHANGING lt_sum-vals                .
      lt_sum-cnt = l_cnt       .
      APPEND       lt_sum      .
      lt_sum  = it_sum         .
      l_cnt   = 1              .
      l_hours = it_sum-hours   .
      l_wo    = it_sum-worder  .
      l_ext   = it_sum-extc    .
      l_int   = it_sum-intc    .
      l_model = it_sum-vm_model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_sum LINES l_size .
  IF l_size > 0 .
    PERFORM get_alc_value  USING lt_sum-worder  lt_sum-code
                        CHANGING lt_sum-vals                .
    lt_sum-cnt = l_cnt       .
    APPEND       lt_sum      .
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
FORM create_data  TABLES pa_data STRUCTURE zspp_vm_gen  USING  pa_rp .
  DATA: l_data              LIKE TABLE OF zspp_vm_gen  WITH HEADER LINE.

  CLEAR: pa_data, pa_data[].

  CALL FUNCTION 'Z_FPP_APS_PLANDATA'
       EXPORTING
            p_rp          = pa_rp
            p_sap         = 'X'
       TABLES
            t_sap         = l_data
       EXCEPTIONS
            etc_exception = 1
            OTHERS        = 2.

  pa_data[] = l_data[].
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
  DATA: l_name(40)           TYPE c ,
        l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_atinn              LIKE cabn-atinn,
        l_model              LIKE it_seq-model,
        L_WORDER             LIKE MARA-MATNR  ,
        l_code               LIKE it_seq-code ,
        l_vals               like it_seq-vals ,
        l_seq                LIKE TABLE OF it_seq      WITH HEADER LINE.

  LOOP AT it_seq .
    l_seq = it_seq.
    LOOP AT it_alc.
      CLEAR: l_atinn.
      SELECT SINGLE atinn INTO l_atinn
        FROM cabn
       WHERE atnam = it_alc-char_alc .

      SELECT SINGLE atwrt INTO l_seq-vals
        FROM ausp
       WHERE objek = it_seq-objek
         AND atinn = l_atinn
         AND klart = '002'       .

      l_seq-alc    = it_alc-char_alc .
      l_seq-cnt    = 1               .
      CONCATENATE  it_alc-char_alc+6(1)  it_alc-char_alc+8(3)
             INTO  L_seq-code       .
      PERFORM GET_WORORDER   USING IT_ALC-type_alc  L_WORDER
                     it_seq-worder it_seq-EXTC      it_seq-INTC .
      PERFORM get_alc_value  USING L_WORDER         L_seq-code
                          CHANGING l_seq-vals                  .
      APPEND l_seq.
    ENDLOOP.
  ENDLOOP.

  " Summary ...
  SORT l_seq BY model code vals.   CLEAR: it_seq, it_seq[], l_cnt.
  READ TABLE l_seq INDEX 1.
  l_model =  l_seq-model  .
  l_code  =  l_seq-code   .
  l_vals  =  l_seq-vals   .
  it_seq  =  l_seq        .

  LOOP AT l_seq.
    IF l_model = l_seq-model AND l_code = l_seq-code and
       l_vals  = l_seq-vals   .
      l_cnt   = l_cnt   + 1 .
      CONTINUE.
    ELSE.
      it_seq-cnt = l_cnt    .
      APPEND it_seq         .
      it_seq     = l_seq    .
      l_model =  l_seq-model.
      l_code  =  l_seq-code .
      l_vals  =  l_seq-vals   .
      l_cnt   =  1          .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE l_seq LINES l_size.
  IF l_size > 0.
    it_seq-cnt = l_cnt    .
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
  DATA: l_name(40)           TYPE c ,
        l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_atinn              LIKE cabn-atinn,
        l_model              LIKE it_seq-model,
        L_WORDER             LIKE MARA-MATNR  ,
        l_code               LIKE it_seq-code ,
        l_vals               like it_seq-vals ,
        l_bi                 LIKE TABLE OF it_seq      WITH HEADER LINE.

  LOOP AT it_bi  .
    l_bi  = it_bi .
    LOOP AT it_alc.
      CLEAR: l_atinn.
      SELECT SINGLE atinn INTO l_atinn
        FROM cabn
       WHERE atnam = it_alc-char_alc .

      SELECT SINGLE atwrt INTO l_bi-vals
        FROM ausp
       WHERE objek = it_bi-objek
         AND atinn = l_atinn
         AND klart = '002'       .

      l_bi-alc    = it_alc-char_alc .
      l_bi-cnt    = 1               .
      CONCATENATE  it_alc-char_alc+6(1)  it_alc-char_alc+8(3)
             INTO  L_bi-code       .
      PERFORM GET_WORORDER   USING IT_ALC-type_alc  L_WORDER
                      it_bi-worder  it_bi-EXTC      IT_bi-INTC .
      PERFORM get_alc_value  USING L_WORDER         L_bi-code
                          CHANGING l_bi-vals                  .
      APPEND l_bi .
    ENDLOOP.
  ENDLOOP.

  " Summary ...
  SORT l_bi  BY model code vals.   CLEAR: it_bi , it_bi[], l_cnt.
  READ TABLE l_bi  INDEX 1.
  l_model =  l_bi-model  .
  l_code  =  l_bi-code   .
  l_vals  =  l_bi-vals   .
  it_bi   =  l_bi        .

  LOOP AT l_bi .
    IF l_model =  l_bi-model AND  l_code  =  l_bi-code and
       l_vals  =  l_bi-vals .
      l_cnt   = l_cnt   + 1 .
      CONTINUE.
    ELSE.
      it_bi-cnt = l_cnt    .
      APPEND it_bi         .
      it_bi     = l_bi     .
      l_model =  l_bi-model.
      l_code  =  l_bi-code .
      l_vals  =  l_bi-vals .
      l_cnt   =  1         .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE l_bi  LINES l_size.
  IF l_size > 0.
    it_bi-cnt = l_cnt    .
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
  DATA: l_name(40)           TYPE c ,
        l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_atinn              LIKE cabn-atinn,
        l_model              LIKE it_seq-model,
        L_WORDER             LIKE MARA-MATNR  ,
        l_code               LIKE it_seq-code ,
        l_vals               like it_seq-vals ,
        l_pi                 LIKE TABLE OF it_seq      WITH HEADER LINE.

  LOOP AT it_pi  .
    l_pi  = it_pi .
    LOOP AT it_alc.
      CLEAR: l_atinn.
      SELECT SINGLE atinn INTO l_atinn
        FROM cabn
       WHERE atnam = it_alc-char_alc .

      SELECT SINGLE atwrt INTO l_pi-vals
        FROM ausp
       WHERE objek = it_pi-objek
         AND atinn = l_atinn
         AND klart = '002'       .

      l_pi-alc    = it_alc-char_alc .
      l_pi-cnt    = 1               .
      CONCATENATE  it_alc-char_alc+6(1)  it_alc-char_alc+8(3)
             INTO  L_pi-code       .
      PERFORM GET_WORORDER   USING IT_ALC-type_alc  L_WORDER
                      it_pi-worder it_pi-EXTC       it_pi-INTC .
      PERFORM get_alc_value  USING L_WORDER         L_pi-code
                          CHANGING l_pi-vals                  .
      APPEND l_pi .
    ENDLOOP.
  ENDLOOP.

  " Summary ...
  SORT l_pi  BY model code vals.   CLEAR: it_pi , it_pi[], l_cnt.
  READ TABLE l_pi  INDEX 1.
  l_model =  l_pi-model  .
  l_code  =  l_pi-code   .
  l_vals  =  l_pi-vals   .
  it_pi   =  l_pi        .

  LOOP AT l_pi .
    IF l_model =  l_pi-model AND  l_code  =  l_pi-code and
       l_vals  =  l_pi-vals  .
      l_cnt   = l_cnt   + 1 .
      CONTINUE.
    ELSE.
      it_pi-cnt = l_cnt    .
      APPEND it_pi         .
      it_pi      = l_pi    .
      l_model =  l_pi-model.
      l_code  =  l_pi-code .
      l_vals  =  l_pi-vals .
      l_cnt   =  1         .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE l_pi  LINES l_size.
  IF l_size > 0.
    it_pi-cnt = l_cnt    .
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
  DATA: l_name(40)           TYPE c ,
        l_cnt                TYPE i ,
        l_size               TYPE i ,
        l_atinn              LIKE cabn-atinn,
        l_model              LIKE it_seq-model,
        L_WORDER             LIKE MARA-MATNR  ,
        l_code               LIKE it_seq-code ,
        l_vals               like it_seq-vals ,
        l_pbs                LIKE TABLE OF it_seq      WITH HEADER LINE.

  LOOP AT it_pbs .
    l_pbs = it_pbs.
    LOOP AT it_alc.
      CLEAR: l_atinn.
      SELECT SINGLE atinn INTO l_atinn
        FROM cabn
       WHERE atnam = it_alc-char_alc .

      SELECT SINGLE atwrt INTO l_pbs-vals
        FROM ausp
       WHERE objek = it_pbs-objek
         AND atinn = l_atinn
         AND klart = '002'       .

      l_pbs-alc    = it_alc-char_alc .
      l_pbs-cnt    = 1               .
      CONCATENATE  it_alc-char_alc+6(1)  it_alc-char_alc+8(3)
             INTO  L_pbs-code       .
      PERFORM GET_WORORDER   USING IT_ALC-type_alc  L_WORDER
                     it_pbs-worder it_pbs-EXTc      it_pbs-INTC .
      PERFORM get_alc_value  USING L_WORDER         L_pbs-code
                          CHANGING l_pbs-vals                  .
      APPEND l_pbs.
    ENDLOOP.
  ENDLOOP.

  " Summary ...
  SORT l_pbs BY model code vals.   CLEAR: it_pbs, it_pbs[], l_cnt.
  READ TABLE l_pbs INDEX 1.
  l_model =  l_pbs-model  .
  l_code  =  l_pbs-code   .
  l_vals  =  l_pbs-vals   .
  it_pbs  =  l_pbs        .

  LOOP AT l_pbs.
    IF l_model =  l_pbs-model AND  l_code  =  l_pbs-code and
       l_vals  =  l_pbs-vals  .
      l_cnt   = l_cnt   + 1 .
      CONTINUE.
    ELSE.
      it_pbs-cnt = l_cnt    .
      APPEND it_pbs         .
      it_pbs  =    l_pbs    .
      l_model =  l_pbs-model.
      l_code  =  l_pbs-code .
      l_vals  =  l_pbs-vals .
      l_cnt   =  1          .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE l_pbs LINES l_size.
  IF l_size > 0.
    it_pbs-cnt = l_cnt    .
    APPEND it_pbs         .
  ENDIF.
ENDFORM.                    " CALC_PBS

*&---------------------------------------------------------------------*
*&      Form  GET_WORORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ALC_TYPE_ALC  text
*      -->P_L_WORDER  text
*----------------------------------------------------------------------*
FORM GET_WORORDER USING  pA_type  pA_worder  PA_WO  PA_EXT  PA_INT .
  IF PA_TYPE = 'C'.
     CONCATENATE PA_WO  PA_EXT  PA_INT  INTO  PA_WORDER .
  ELSE.
     PA_WORDER = PA_WO .
  ENDIF.
ENDFORM.                    " GET_WORORDER
