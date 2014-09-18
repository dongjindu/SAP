************************************************************************
* Program Name      : ZRPP804R_VEHICLE_EMISSION
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'Processing the EMISSION Process
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zrpp804r_vehicle_emission  NO STANDARD PAGE HEADING
                                  LINE-SIZE  1023  LINE-COUNT 65
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ausp           ,          " Vehicle Table
        ztpp_nation_def.          " Nation Table(Inclu. Emission Table)

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF it_nation OCCURS 0                   .
        INCLUDE STRUCTURE     ztpp_nation_def       .
DATA:   pack(5)          TYPE c                     ,
      END OF it_nation                              ,
      BEGIN OF it_ausp   OCCURS 0                   .
        INCLUDE STRUCTURE     ausp                  .
DATA:   nation           LIKE ztpp_nation_def-nation,
        pack(5)          TYPE c                     ,
      END OF it_ausp.

*----------------------------------------------------------------------*
* Working AREA
*----------------------------------------------------------------------*
DATA: wa_atinn                 LIKE ausp-atinn, " VM_DATE OR SEQ_DATE..
      wa_atflv                 LIKE ausp-atflv,
      wa_atwrt                 LIKE ausp-atwrt,
      wa_worder                LIKE ausp-atinn,
      wa_emission              LIKE ausp-atinn,
      wa_05                    LIKE ausp-atinn,
      wa_06                    LIKE ausp-atinn,
      wa_17                    LIKE ausp-atinn,
      wa_18                    LIKE ausp-atinn,
      wa_19                    LIKE ausp-atinn,
      wa_20                    LIKE ausp-atinn,
      wa_21                    LIKE ausp-atinn,
      wa_22                    LIKE ausp-atinn,
      wa_23                    LIKE ausp-atinn,
      wa_24                    LIKE ausp-atinn,
      wa_25                    LIKE ausp-atinn,
      wa_26                    LIKE ausp-atinn,
      wa_27                    LIKE ausp-atinn,
      wa_error                 TYPE c         .   " Error Flag..


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS:
* p_date               TYPE d   DEFAULT sy-datum,
  p_vdate              LIKE sy-datum   OBLIGATORY    .
SELECTION-SCREEN END   OF BLOCK b1.

*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK b1.
*&---------------------------------------------------------------------*
*  IF p_Vdate IS INITIAL.  " AND p_vdate IS INITIAL.
*    MESSAGE e001  WITH  text-100.
*  ENDIF.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  PERFORM check_option         .
  PERFORM set_atinn            .
* PERFORM CHECK_RUNNING        .    " Not useless!!!
* PERFORM clear_routine     .
  CHECK wa_error IS INITIAL    .
  PERFORM read_vehicle         .
  PERFORM update_process       .
  PERFORM display_process      .

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  CHECK_OPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_option   .
  PERFORM read_atinn   USING  'P_VM_DATE'         wa_atinn   .
  wa_atwrt = p_vdate .
  CONCATENATE wa_atwrt '%'     INTO wa_atwrt.
ENDFORM.                       " ENDFORM

*&---------------------------------------------------------------------*
*&      Form  SET_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_atinn      .
  PERFORM read_atinn   USING  'P_WORK_ORDER'          wa_worder  .
  PERFORM read_atinn   USING  'P_EMISSION'            wa_emission.
ENDFORM.                       " SET_ATINN

*&---------------------------------------------------------------------*
*&      Form  CHECK_RUNNING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_running  .
  DATA: l_count               TYPE i    ,
        lt_ausp               LIKE TABLE OF it_ausp    WITH HEADER LINE.

  CLEAR: wa_error.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK
                      from AUSP
                     WHERE atinn = wa_atinn
                       AND klart = '002'
                       AND atwrt LIKE wa_atwrt )
     AND atinn = wa_emission
     AND klart = '002'
     AND atwrt = 'Y'        .

  DESCRIBE TABLE lt_ausp LINES l_count.
  IF l_count > 0.
    " Already processing the Emission Process....
    wa_error = 'X'.
  ENDIF.

  " ALC Transported Data is no-processing....
ENDFORM.                       " SET_ATINN

*&---------------------------------------------------------------------*
*&      Form  READ_VEHICLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vehicle   .
  DATA: lt_ausp          LIKE TABLE OF it_ausp         WITH HEADER LINE,
        lt_val           LIKE TABLE OF zspp_vin_value  WITH HEADER LINE,
        l_nation         LIKE it_ausp-nation,
        l_pack           LIKE it_ausp-pack  ,
        l_emission       LIKE ausp-atwrt,       "
        l_seqt           TYPE p DECIMALS 4,     " Sequ. Quantity Total
        l_emst           TYPE p DECIMALS 4,     " Emissioned Qty. Total
        l_total          TYPE i         ,       " Total Emission Count
        l_count          TYPE p DECIMALS 4,     " Total Count
        l_Icount         TYPE I         ,       " Total Count
        l_cnt            TYPE i         ,       " Total Count
        l_tocnt          TYPE i         ,       " Emission Count
        l_diff           TYPE I         ,       " Gap of Emiss. Count
        l_float          TYPE p DECIMALS 4,     " Floating Variable
        l_index          TYPE i         ,       " Index of Read Table
        l_even           TYPE i         .       " Even Count...

  CLEAR: it_ausp, it_ausp[].

  " Source Data..
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK
                      from AUSP
                     WHERE atinn = wa_atinn
                       AND klart = '002'
                       AND atwrt LIKE wa_atwrt )
     AND atinn = wa_worder
     AND klart = '002'         .

  LOOP AT it_ausp.
    it_ausp-nation = it_nation-nation = it_ausp-atwrt+9(5).
    it_ausp-pack   = it_nation-pack   = it_ausp-atwrt(5)  .
    APPEND it_nation.  MODIFY it_ausp.
  ENDLOOP.

  " Read the Emission Information..
  SORT it_nation BY nation pack .
  DELETE ADJACENT DUPLICATES FROM it_nation COMPARING nation pack .
  LOOP AT it_nation.
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF it_nation
      FROM ztpp_nation_def
     WHERE nation   = it_nation-nation
       AND emission = 'X'.
    IF sy-subrc = 0.
      MODIFY it_nation.
    ELSE.
      DELETE it_nation.
    ENDIF.
  ENDLOOP.

  " Update Characteristics Setting....
  CLEAR: lt_val, lt_val[].
  lt_val-atnam = 'P_EMISSION'.  lt_val-atwrt = 'Y'.  APPEND lt_val.

  " Main Logic for the Calculation of Emission.....
  SORT it_nation BY nation pack .
  LOOP AT it_nation.
    CLEAR: lt_ausp, lt_ausp[].
    lt_ausp[] = it_ausp[]    .
    DELETE lt_ausp WHERE nation NE it_nation-nation OR
                         pack   NE it_nation-pack   .
    DESCRIBE TABLE lt_ausp LINES l_cnt           .
    l_count = l_cnt    .
    CHECK  l_count > 0 .
    PERFORM get_total_ordemiss  USING  it_nation-nation  l_seqt.
    PERFORM get_total_qtyemiss  USING  it_nation-nation  l_emst.
    l_float = l_seqt * it_nation-em_rate / 100   .
    l_total = ceil( l_float )                    .  " Will emissioned..
    l_float = l_count * it_nation-em_rate / 100  .
    l_tocnt = trunc( l_float )                   .
    l_diff  = l_total - l_emst - l_tocnt         .
    IF l_diff > 0.
      " Difference Count of Between Will be emissioned and
      " Has been emission..
      l_tocnt = l_total - l_emst                 .
    ELSE.
      CONTINUE.
    ENDIF.

    " Processing of Even per Sequence Order...
    l_even  = trunc( l_count / l_tocnt )         .
    l_index = 1                                  .

    DO l_tocnt TIMES .
      READ TABLE lt_ausp INDEX l_index           .
      CLEAR: l_emission.
      SELECT SINGLE atwrt INTO l_emission
        FROM ausp
       WHERE objek = lt_ausp-objek
         AND atinn = wa_emission
         AND klart = '002'        .

      IF sy-subrc NE 0 OR l_emission = space .
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
             EXPORTING
                  object       = lt_ausp-objek(18)
                  mode         = 'W'
                  ctype        = '002'
             TABLES
                  val_table    = lt_val
             EXCEPTIONS
                  no_data      = 1
                  error_mode   = 2
                  error_object = 3
                  error_value  = 4
                  OTHERS       = 5.

      ELSE.
        " Data Error..  1. Twice adapt the Emission
        "            or 2. Spec Change is Fail or Error.
        "            or 3. Manually Data is changed.
        wa_error = 'X'   .
        EXIT.
      ENDIF.
      l_index = l_index + l_even .
    ENDDO.

    IF wa_error = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " READ_VEHICLE

*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_process.
* IF wa_error = 'X'.
*   CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
* ELSE.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait = 'X'.
* ENDIF.
ENDFORM.                    " UPDATE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_process.
ENDFORM.                    " DISPLAY_PROCESS

*&---------------------------------------------------------------------*
*&      Form  READ_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0113   text
*      -->P_L_WO  text
*----------------------------------------------------------------------*
FORM read_atinn USING    pa_char  pa_atinn .
  SELECT SINGLE atinn  INTO pa_atinn
    FROM cabn
   WHERE atnam = pa_char.
ENDFORM.                    " READ_ATINN

*&---------------------------------------------------------------------*
*&      Form  GET_TOTAL_ORDEMISS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_NATION_NATION  text
*      -->P_L_ORDT  text
*----------------------------------------------------------------------*
FORM get_total_ordemiss USING    pa_nation  pa_quantity .
  DATA: l_pack(6)       TYPE c .

  CLEAR: pa_quantity, l_pack.
  CONCATENATE it_nation-pack '%'  INTO l_pack.
  SELECT SUM( seqqty ) INTO  pa_quantity
    FROM ztpp_wosum
   WHERE wo_ser LIKE l_pack
     AND nation = pa_nation(3)
     AND dealer = pa_nation+3(2).
ENDFORM.                    " GET_TOTAL_ORDEMISS

*&---------------------------------------------------------------------*
*&      Form  GET_TOTAL_qtyEMISS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_NATION_NATION  text
*      -->P_L_EMST  text
*----------------------------------------------------------------------*
FORM get_total_qtyemiss USING    pa_nation  pa_emst.
  DATA: l_pack          LIKE it_nation-pack   ,
        l_nation        LIKE it_nation-nation ,
        lt_ausp         LIKE TABLE OF it_ausp          WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK
                      from AUSP
                     WHERE objek IN ( select OBJEK
                                        from AUSP
                                       WHERE atinn = wa_atinn
                                         AND klart = '002'
                                         AND atwrt LIKE wa_atwrt )
                       AND atinn = wa_emission
                       AND klart = '002'
                       AND atwrt = 'Y'      )
     AND atinn = wa_worder
     AND klart = '002'     .

  LOOP AT lt_ausp .
    l_nation = lt_ausp-atwrt+9(5).    l_pack = lt_ausp-atwrt(5).
    IF l_nation = pa_nation AND l_pack = it_nation-pack.
    ELSE.
      DELETE lt_ausp.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE lt_ausp LINES pa_emst.
ENDFORM.                    " GET_TOTAL_qtyEMISS

*&---------------------------------------------------------------------*
*&      Form  CLEAR_ROUTINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_routine.
  DATA: lt_ausp          LIKE TABLE OF ausp            WITH HEADER LINE,
        lt_val           LIKE TABLE OF zspp_vin_value  WITH HEADER LINE.

  SELECT * INTO TABLE lt_ausp
    FROM ausp
   WHERE atinn = wa_emission
     AND klart = '002'
     AND atwrt = 'Y' .

  CLEAR: lt_val, lt_val[].
  lt_VAL-atnam = 'P_EMISSION'.  APPEND lt_val.

  LOOP AT lt_ausp.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = lt_ausp-objek(18)
              mode         = 'W'
              ctype        = '002'
         TABLES
              val_table    = lt_val
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.
  ENDLOOP.
ENDFORM.                    " CLEAR_ROUTINE
