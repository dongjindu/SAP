************************************************************************
* Program Name      : ZAPP903R_ALC_LONG
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
REPORT  ZAPP903R_ALC_LONG     MESSAGE-ID zmpp.

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
      END OF it_alc .

DATA: BEGIN OF it_sum         OCCURS 0,
        rp(2)                 TYPE n  ,
        worder                LIKE mara-matnr,
        knnam                 LIKE cukb-knnam,
        status                LIKE ztpp_input_plan-status,
        code(4)               TYPE c  ,              " ALC CODE
        vals(5)               TYPE c  ,              " ALC CODE VALUE
        hours                 TYPE i  ,
        vm_model              LIKE ztpp_input_plan-modl ,
        vm_bodyser            LIKE ztpp_input_plan-body_ser,
        EXTC                  LIKE ZTPP_INPUT_PLAN-EXTC,
        INTC                  LIKE ZTPP_INPUT_PLAN-INTC,
      END OF it_sum .

DATA: it_summary            like table of it_sum       with header line,
      it_disp               LIKE TABLE OF ztpp_alc_sum WITH HEADER LINE.

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: wa_data             LIKE it_data                             ,
      wa_wdate            LIKE sy-datum                            ,
      wa_uzeit            LIKE sy-uzeit                            ,
      wa_index            LIKE sy-tabix                            ,
      wa_hour             type i                                   ,
      wa_serial           LIKE ztpp_dvrt1-serial                   .

*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
FIELD-SYMBOLS: <wa_dfield>    TYPE ANY.

*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS : p_run        TYPE c        .
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS: p_model(3)    TYPE c        ,
            p_uph(2)      TYPE n        OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------
INCLUDE zapp903r_inc_function.

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  PERFORM setting_default.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM CLEAR_VARIABLE .
  PERFORM run_process.
  PERFORM read_alc_model USING p_model .
  PERFORM create_summary .

*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------
  GET TIME.
  WRITE AT: /001(40) 'End of the All Processing ...........' ,
             041(11) sy-datum,
             053(10) sy-uzeit.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_VARIABLE.
  CLEAR: it_temp,   it_DATA,   IT_ALC,   it_summary,   it_sum,  it_disP,
         It_temp[], it_DATA[], IT_ALC[], it_summary[], it_sum[],
         IT_DISP[], wa_data, wa_wdate, wa_uzeit, wa_index, wa_hour,
         wa_serial.
ENDFORM.                    " CLEAR_VARIABLE
