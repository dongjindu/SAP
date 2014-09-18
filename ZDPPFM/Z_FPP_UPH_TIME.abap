FUNCTION Z_FPP_UPH_TIME.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT_DATE) LIKE  SY-DATUM
*"     REFERENCE(INPUT_TIME) LIKE  SY-UZEIT
*"  EXPORTING
*"     REFERENCE(RATE) TYPE  P
*"----------------------------------------------------------------------

  TABLES : ldlt,
           v_tc37p, " Break schedule
           crtx.

  DATA : wa_be    LIKE tc37p-paubeg,
         wa_en    LIKE tc37p-paubeg,
         wa_sum   LIKE tc37p-padauer,
         wa_time  LIKE kapa-begzt.

    select single * from CRtx
                    where OBJTY = 'A'
                      and SPRAS = sy-langu
                      and KTEXT_UP = 'ASSEMBLY LINE 1' .


    SELECT SINGLE * FROM ldlt
                    WHERE lnid  = CRtx-OBJID
                      AND lnsid = CRtx-OBJID
                      AND ld_perst <= INPUT_DATE
                      AND ld_pered >= INPUT_DATE .

    IF sy-uzeit <= '0630'.
      wa_be = input_time.
      wa_en = '063000'.
    ELSE.
      wa_be = '063000'.
      wa_en = input_time.
    ENDIF.

    SELECT SUM( padauer ) INTO wa_sum
                  FROM tc37p
                  WHERE schgrup = 'HA'
                    AND paplan IN ('HAP1' ,'HAP2')
                    AND paubeg >= wa_be
                    AND pauend <= wa_en.
*      endselect.

    wa_time = '063000'  - input_time.
    wa_time = abs( wa_time ).

    rate       =  ( ( wa_time - wa_sum ) * ldlt-lrate ) / 3600.

    CALL FUNCTION 'FIMA_NUMERICAL_VALUE_ROUND'
           EXPORTING
             i_rtype           = '+'
             I_RUNIT           = 1
             i_value           =    rate
          IMPORTING
             e_value_rnd       =  rate                 .



ENDFUNCTION.
