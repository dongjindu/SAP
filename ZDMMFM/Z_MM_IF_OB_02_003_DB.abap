FUNCTION z_mm_if_ob_02_003_db.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_BODY) LIKE  ZMMT0038 STRUCTURE  ZMMT0038
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"----------------------------------------------------------------------

  DATA: time_stamp  TYPE timestamp,
        time_stamp2  TYPE timestamp,
        dat TYPE d,
        tim TYPE t,
        str1(2),
        str2(2),
        l_pktim(20),
        l_date    LIKE sy-datum,
        l_time    LIKE sy-uzeit,
        l_etime   LIKE sy-uzeit,
        l_tzone   LIKE ttzz-tzone VALUE 'UTC',
        iv_hours  TYPE  int4,
        l_kwbzm(10), " LIKE pkhd-kwbzm,
        tz  TYPE ttzz-tzone VALUE 'UTC'.

  CLEAR : it_m038, it_m038[].

  MOVE-CORRESPONDING i_body TO it_m038.

  IF it_m038-lgpro IS INITIAL.
*-- Get Issue Storage Location
    SELECT SINGLE lgpro
           INTO it_m038-lgpro
           FROM marc
           WHERE matnr = it_m038-matnr
*             AND werks = 'KVA1'.
*             AND werks = 'E001'.
             AND werks = 'P001'.

  ENDIF.

  tz = 'UTC'.

  CASE i_body-rksta.
    WHEN 'I'.
*      time_stamp = it_m038-pktim.
      CONVERT DATE it_m038-saedt TIME it_m038-saeuz
      INTO TIME STAMP time_stamp TIME ZONE l_tzone.

      CALL FUNCTION 'CIF_GEN_CONVERT_TIMESTAMP'
           EXPORTING
                iv_timestamp           = time_stamp
                iv_timezone            = l_tzone
           IMPORTING
                ev_date                = l_date
                ev_time                = l_time
           EXCEPTIONS
                time_conversion_failed = 1
                OTHERS                 = 2.

      CONVERT DATE l_date TIME l_time
            INTO TIME STAMP time_stamp2 TIME ZONE tz.

      it_m038-pktim = time_stamp2.

    WHEN OTHERS.
      tz = 'UTC'.
      dat = i_body-saedt.

      CALL FUNCTION 'CONVERSION_EXIT_TSTRN_OUTPUT'
           EXPORTING
                input  = i_body-kwbzm
           IMPORTING
                output = l_kwbzm.

      CONDENSE l_kwbzm NO-GAPS.

      SPLIT l_kwbzm AT ':' INTO: str1 str2.

      iv_hours = str1.

      CONVERT DATE it_m038-saedt TIME it_m038-saeuz
      INTO TIME STAMP time_stamp TIME ZONE l_tzone.

*S__ PAUL CHANGE FUNCTION.
*      CALL FUNCTION 'IAM_TIMESTAMP_CALC'
*        EXPORTING
*          iv_refdate   = time_stamp
*          iv_xbackward = ' '
*          iv_hours     = iv_hours
*        IMPORTING
*          ev_date      = time_stamp.
      CALL FUNCTION '/SDF/RBE_TIMESTAMP_ADD'
           EXPORTING
                timestamp_in    = time_stamp
                timezone        = 'UTC'
                duration        = 2
                unit            = 'H'
           IMPORTING
                timestamp_out   = time_stamp
           EXCEPTIONS
                timestamp_error = 1
                OTHERS          = 2.
*E__< 060611
      CALL FUNCTION 'CIF_GEN_CONVERT_TIMESTAMP'
           EXPORTING
                iv_timestamp           = time_stamp
                iv_timezone            = l_tzone
           IMPORTING
                ev_date                = l_date
                ev_time                = l_time
           EXCEPTIONS
                time_conversion_failed = 1
                OTHERS                 = 2.

      IF l_time+2(4) >= '0000' AND l_time+2(4) <= '2959'.
        CONCATENATE l_time(2) '0000' INTO l_etime.
      ENDIF.

      IF l_time+2(4) >= '3000' AND l_time+2(4) <= '5959'.
        CONCATENATE l_time(2) '3000' INTO l_etime.
      ENDIF.

      CONDENSE l_etime NO-GAPS.

      CONVERT DATE l_date TIME l_etime
            INTO TIME STAMP time_stamp2 TIME ZONE l_tzone.

      it_m038-pktim = time_stamp2.
  ENDCASE.

  IF it_m038-zfeeder EQ 'HOT'. " Andy / IG ( 7/15/2011 )

    it_m038-saedt = sy-datum.
    it_m038-saeuz = sy-uzeit.

  ELSE.

    it_m038-saedt = l_date.
    it_m038-saeuz = l_time.

  ENDIF.

*FIXME!
*  CONCATENATE i_body-saedt i_body-saeuz INTO l_pktim.
*  it_m038-pktim = l_pktim.

  APPEND it_m038. CLEAR : it_m038.

  MODIFY zmmt0038 FROM TABLE it_m038.


ENDFUNCTION.
