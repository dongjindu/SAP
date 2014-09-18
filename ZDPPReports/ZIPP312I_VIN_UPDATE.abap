REPORT ZIPP312I_VIN_UPDATE.

PARAMETERS: p_sequnr      LIKE equi-equnr  DEFAULT 'EMF040001',
            p_record      TYPE i           DEFAULT 1000       .

DATA: l_vals              LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

START-OF-SELECTION.
  PERFORM create_sample_data using 'Start of the Processing...' .
  PERFORM call_function_z   .
  PERFORM create_sample_data using 'End of the Processing...' .

*&---------------------------------------------------------------------*
*&      Form  CREATE_SAMPLE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_sample_data  using pa_message .
  get time.
  write at: /001(030) pa_message                  ,
             031(010) sy-datum                    ,
             045(010) sy-uzeit                    .
ENDFORM.                    " CREATE_SAMPLE_DATA

*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_Z
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function_z.
  DATA: l_rp(2)               TYPE n                        ,
        l_seq(6)              TYPE n        VALUE 1         ,
        ls_datum              type d        value '20990930',
        SV_DATUM              TYPE D                        ,
        l_equnr               LIKE equi-equnr.

  DO 18 TIMES.
    CLEAR: l_vals, l_vals[].
    l_rp = l_rp + 1   .
    l_seq = p_sequnr+3(6) .
    l_seq = l_seq - 1    .

    GET TIME.
    CONCATENATE 'P_RP' l_rp '_ACTUAL_DATE' INTO l_vals-atnam.
    SV_DATUM = ls_datum + l_rp.
    CONCATENATE SV_DATUM      SY-UZEIT     INTO L_VALS-ATWRT .
    APPEND l_vals.
    CONCATENATE 'P_RP' l_rp '_SHOP_DATE'   INTO l_vals-atnam.
    L_VALS-ATWRT = SV_DATUM.               APPEND l_vals.
    CONCATENATE 'P_RP' l_rp '_SERIAL'      INTO l_vals-atnam.
    L_VALS-ATWRT = '00000' .               APPEND l_vals.

    DO p_record TIMES.
      l_seq = l_seq + 1 .
      CONCATENATE 'EMF' l_seq INTO l_equnr .

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object             = l_equnr
          MODE               = 'W'
        TABLES
          val_table          = l_vals .

      LOOP AT L_VALS.
        WRITE AT: /001(005) L_VALS-ZFLAG ,
                   006(030) L_VALS-ATNAM ,
                   036(030) L_VALS-ATWRT .
      ENDLOOP.
    ENDDO.
  ENDDO.
ENDFORM.                    " CALL_FUNCTION_Z
