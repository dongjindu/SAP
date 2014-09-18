FUNCTION z_eis_get_tm_scheduled_work.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     VALUE(CHECK_TIME) TYPE  SY-UZEIT DEFAULT SY-UZEIT
*"  TABLES
*"      IT_RESULT STRUCTURE  ZHR_TM_SCHEDULED_WORK
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 06/23/2011 VALERIAN   UD1K951284  Initial Program Development
*-----------------------------------------------------------------------

  DATA: BEGIN OF it_data OCCURS 0,
          pernr TYPE pa0000-pernr,
          schkz TYPE pa0007-schkz,
          sobeg TYPE t550a-sobeg,
          soend TYPE t550a-soend,
        END OF it_data,

        l_tprog TYPE tprog.

* Get only active employee in for date/time of the day
  SELECT a~pernr b~schkz
    INTO TABLE it_data
    FROM pa0000 AS a JOIN pa0007 AS b
                       ON a~pernr = b~pernr
   WHERE a~endda >= check_date
     AND a~begda <= check_date
     AND b~endda >= check_date
     AND b~begda <= check_date
     AND a~stat2 = '3'.

  LOOP AT it_data.

* Get daily work schedule
    CALL FUNCTION 'Z_CO_GET_DWS_IG'
         EXPORTING
              schkz                          = it_data-schkz
              datum                          = check_date
         IMPORTING
              tprog                          = l_tprog
         EXCEPTIONS
              not_found_work_schedule_rules  = 1
              invalid_date                   = 2
              not_found_period_work_schedule = 3
              OTHERS                         = 4.

    CHECK sy-subrc = 0.

* Get daily work schedule date/time
    SELECT tprog sobeg soend INTO CORRESPONDING FIELDS OF it_result
      FROM t550a
      UP TO 1 ROWS
     WHERE tprog = l_tprog
       AND endda >= check_date
       AND begda <= check_date.
    ENDSELECT.

    CHECK sy-subrc = 0.

* Ignore off-schedule date/time
    CHECK NOT ( it_result-sobeg = '000000' AND
                it_result-soend = '240000' ).

* Get only relevant daily work schedule date/time
    IF it_result-sobeg <= it_result-soend.
      IF NOT ( it_result-sobeg <= check_time AND
               check_time <= it_result-soend ).
        CONTINUE.
      ENDIF.
    ELSE.
      IF NOT ( it_result-sobeg <= check_time OR
               check_time <= it_result-soend ).
        CONTINUE.
      ENDIF.
    ENDIF.

* Store result
    it_result-pernr = it_data-pernr.
    APPEND it_result.
  ENDLOOP.

* Sort result
  SORT it_result BY pernr.

ENDFUNCTION.
