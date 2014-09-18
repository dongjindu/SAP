FUNCTION z_hr_ess_get_pay_deduct_info.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) TYPE  BAPI7004-PERNR
*"     VALUE(FROM_DATE) TYPE  BEGDA
*"     VALUE(TO_DATE) TYPE  ENDDA
*"  TABLES
*"      PAY_DEDUCT_INFO STRUCTURE  ZESS_EMP_PAY_DEDUCT_INFO
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 04/14/2011 VALERIAN   UD1K951284  Initial Program Development
* 09/17/2012 VALERIAN   UD1K955531  Various Fix
*-----------------------------------------------------------------------
  CONSTANTS c_waers TYPE waers VALUE 'USD'.

  DATA: l_molga     TYPE molga,
        it_rgdir    TYPE pc261 OCCURS 0 WITH HEADER LINE,
        wa_t500l    TYPE t500l,
        payroll_res TYPE payus_result,
        t_rt        TYPE hrpay99_rt,
        wa_rt       TYPE LINE OF hrpay99_rt,
        l_stat2     TYPE pa0000-stat2,
        l_seqnr     TYPE pc261-seqnr,
        t_grrec     TYPE hrpayus_grrec,
        wa_grrec    TYPE LINE OF hrpayus_grrec.

  DATA: BEGIN OF t_pa0014 OCCURS 0,
         begda TYPE pa0014-begda,
         endda TYPE pa0014-endda,
         lgart TYPE pa0014-lgart,
         opken TYPE pa0014-opken,
         betrg TYPE pa0014-betrg,
         model TYPE pa0014-model,
         lgtxt TYPE t512t-lgtxt,
        END OF t_pa0014,

        t_pa0015 LIKE t_pa0014 OCCURS 0 WITH HEADER LINE,
        t_pa0267 LIKE t_pa0014 OCCURS 0 WITH HEADER LINE,
        t_pa2010 LIKE t_pa0014 OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_pa0195 OCCURS 0,
         begda TYPE pa0195-begda,
         endda TYPE pa0195-endda,
         ordcd TYPE pa0195-ordcd,
         deduc TYPE pa0195-deduc,
         ordtx TYPE t5ug0-ordtx,
         grnum TYPE pa0195-grnum,
        END OF t_pa0195.

* Check if the employee is still active.
  CLEAR l_stat2.
  SELECT stat2 INTO l_stat2
    FROM pa0000
   UP TO 1 ROWS
   WHERE pernr = employee_number
     AND endda = '99991231'.
  ENDSELECT.

  IF l_stat2 = '0'.
    return-type = 'E'.
    return-code = '5F'.
    return-log_msg_no = '739'.

    MESSAGE ID return-code TYPE return-type NUMBER return-log_msg_no
            WITH return-message_v1 return-message_v2
                 return-message_v3 return-message_v4
       INTO return-message.
    APPEND return.
    EXIT.
  ENDIF.

* Read RGDIR
  CALL FUNCTION 'CU_READ_RGDIR'
       EXPORTING
            persnr          = employee_number
       IMPORTING
            molga           = l_molga
       TABLES
            in_rgdir        = it_rgdir
       EXCEPTIONS
            no_record_found = 1
            OTHERS          = 2.

* Determine cluster ID
  CALL FUNCTION 'HRPY_READ_T500L'
       EXPORTING
            molga          = l_molga
       IMPORTING
            t500l_entry    = wa_t500l
       EXCEPTIONS
            no_entry_found = 1
            OTHERS         = 2.

* Get from PA0014
  SELECT a~begda a~endda a~lgart a~opken a~betrg a~model b~lgtxt
    INTO CORRESPONDING FIELDS OF TABLE t_pa0014
    FROM pa0014 AS a JOIN t512t AS b
             ON b~sprsl = sy-langu
            AND b~lgart = a~lgart
            AND b~molga = l_molga

   WHERE a~pernr = employee_number
     AND a~sprps = space
     AND a~endda >= from_date
     AND a~begda <= to_date.

* Exclude these wage types for PA0014
  DELETE t_pa0014 WHERE lgart = '3004' OR
                        lgart = '3005' OR
                        lgart = '3018' OR
                        lgart = '3022' OR
                        lgart = '3044' OR
                        lgart = '3045'.

* Get from PA0015
  SELECT a~begda a~lgart a~opken a~betrg b~lgtxt
    INTO CORRESPONDING FIELDS OF TABLE t_pa0015
    FROM pa0015 AS a JOIN t512t AS b
             ON b~sprsl = sy-langu
            AND b~lgart = a~lgart
            AND b~molga = l_molga

   WHERE a~pernr = employee_number
     AND a~sprps = space
     AND a~endda >= from_date
     AND a~begda <= to_date.

* Exclude these wage types for PA0015
  DELETE t_pa0015 WHERE lgart = '2019' OR
                        lgart = '2023' OR
                        lgart = '3004' OR
                        lgart = '3005' OR
                        lgart = '3018' OR
                        lgart = '3019' OR
                        lgart = '3022' OR
                        lgart = '3023' OR
                        lgart = '3044' OR
                        lgart = '5212' OR
                        lgart = '8122' OR
                        lgart = '8123' OR
                        lgart = '8401' OR
                        lgart = '8411' OR
                        lgart = '8421' OR
                        lgart = '8431' OR
                        lgart = '8441' OR
                        lgart = '8451' OR
                        lgart = '8461'.

* Get from PA0267
  SELECT a~begda a~lgart a~opken a~betrg b~lgtxt
    INTO CORRESPONDING FIELDS OF TABLE t_pa0267
    FROM pa0267 AS a JOIN t512t AS b
             ON b~sprsl = sy-langu
            AND b~lgart = a~lgart
            AND b~molga = l_molga

   WHERE a~pernr = employee_number
     AND a~sprps = space
     AND a~endda >= from_date
     AND a~begda <= to_date.

* Exclude these wage types for PA0267
  DELETE t_pa0267 WHERE lgart >= '5000'.


* Get from PA2010
  SELECT a~begda a~lgart a~betrg b~lgtxt
    INTO CORRESPONDING FIELDS OF TABLE t_pa2010
    FROM pa2010 AS a JOIN t512t AS b
             ON b~sprsl = sy-langu
            AND b~lgart = a~lgart
            AND b~molga = l_molga

   WHERE a~pernr = employee_number
     AND a~subty IN ('1294', '1295')
     AND a~sprps = space
     AND a~endda >= from_date
     AND a~begda <= to_date.

* Get from PA0195
  SELECT a~begda a~endda a~ordcd a~deduc b~ordtx a~grnum
    INTO CORRESPONDING FIELDS OF TABLE t_pa0195
    FROM pa0195 AS a JOIN t5ug0 AS b
             ON b~ordcd = a~ordcd

   WHERE a~pernr = employee_number
     AND a~sprps = space
     AND a~endda >= from_date
     AND a~begda <= to_date
     AND a~ordcd IN ('CS', 'C2', 'C3', 'C4', 'BA',
                     'ST', 'CR', 'FT', 'ED' ).


* Extract information from PA0014
  LOOP AT t_pa0014.
    MOVE-CORRESPONDING t_pa0014 TO pay_deduct_info.
*   Determine payment or deduction
    IF t_pa0014-opken = 'A'.
      pay_deduct_info-ptype = '2'.
    ELSE.
      pay_deduct_info-ptype = '1'.
    ENDIF.

*   Determine calender type.
    IF NOT t_pa0014-model IS INITIAL.
      pay_deduct_info-calty = 'Annually'.
    ENDIF.

    pay_deduct_info-waers = c_waers.
    APPEND pay_deduct_info.
    CLEAR pay_deduct_info.
  ENDLOOP.


* Extract information from PA0015
  LOOP AT t_pa0015.
    MOVE-CORRESPONDING t_pa0015 TO pay_deduct_info.
*   Determine payment or deduction
    IF t_pa0015-opken = 'A'.
      pay_deduct_info-ptype = '2'.
    ELSE.
      pay_deduct_info-ptype = '1'.
    ENDIF.

    pay_deduct_info-waers = c_waers.
    APPEND pay_deduct_info.
    CLEAR pay_deduct_info.
  ENDLOOP.


* Extract information from PA0267
  LOOP AT t_pa0267.
    MOVE-CORRESPONDING t_pa0267 TO pay_deduct_info.
*   Determine payment or deduction
    IF t_pa0267-opken = 'A'.
      pay_deduct_info-ptype = '2'.
    ELSE.
      pay_deduct_info-ptype = '1'.
    ENDIF.

    pay_deduct_info-waers = c_waers.
    APPEND pay_deduct_info.
    CLEAR pay_deduct_info.
  ENDLOOP.


* Extract information from PA2010
  LOOP AT t_pa2010.
    READ TABLE it_rgdir WITH KEY fpbeg = t_pa2010-begda
                                 srtza = 'A'.
    IF sy-subrc = 0.

*     Read payroll results
      CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
           EXPORTING
                clusterid                    = wa_t500l-relid
                employeenumber               = employee_number
                sequencenumber               = it_rgdir-seqnr
                read_only_international      = ' '
           CHANGING
                payroll_result               = payroll_res
           EXCEPTIONS
                illegal_isocode_or_clusterid = 1
                error_generating_import      = 2
                import_mismatch_error        = 3
                subpool_dir_full             = 4
                no_read_authority            = 5
                no_record_found              = 6
                versions_do_not_match        = 7
                error_reading_archive        = 8
                error_reading_relid          = 9
                OTHERS                       = 10.

      IF sy-subrc = 0.
        t_rt = payroll_res-inter-rt.
        READ TABLE t_rt INTO wa_rt
                        WITH KEY lgart = t_pa2010-lgart.
        IF sy-subrc = 0.
          t_pa2010-betrg = wa_rt-betrg.

          MOVE-CORRESPONDING t_pa2010 TO pay_deduct_info.
          pay_deduct_info-ptype = '1'.
          pay_deduct_info-waers = c_waers.
          APPEND pay_deduct_info.
          CLEAR pay_deduct_info.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDLOOP.


* Extract information from PA0195
  LOOP AT t_pa0195.
    MOVE-CORRESPONDING t_pa0195 TO pay_deduct_info.
    pay_deduct_info-ptype = '3'.
    pay_deduct_info-lgtxt = t_pa0195-ordtx.
    pay_deduct_info-waers = c_waers.

    IF t_pa0195-ordcd = 'CS' OR
       t_pa0195-ordcd = 'C2' OR
       t_pa0195-ordcd = 'C3' OR
       t_pa0195-ordcd = 'C4' OR
       t_pa0195-ordcd = 'BA'.

      pay_deduct_info-betrg = t_pa0195-deduc.

    ELSE.
      CALL FUNCTION 'CD_READ_LAST'
           EXPORTING
                begin_date      = from_date
                end_date        = to_date
           IMPORTING
                out_seqnr       = l_seqnr
           TABLES
                rgdir           = it_rgdir
           EXCEPTIONS
                no_record_found = 1
                OTHERS          = 2.

      IF sy-subrc = 0.
*       Read garnishment
        CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
             EXPORTING
                  clusterid                    = wa_t500l-relid
                  employeenumber               = employee_number
                  sequencenumber               = l_seqnr
                  read_only_international      = ' '
             CHANGING
                  payroll_result               = payroll_res
             EXCEPTIONS
                  illegal_isocode_or_clusterid = 1
                  error_generating_import      = 2
                  import_mismatch_error        = 3
                  subpool_dir_full             = 4
                  no_read_authority            = 5
                  no_record_found              = 6
                  versions_do_not_match        = 7
                  error_reading_archive        = 8
                  error_reading_relid          = 9
                  OTHERS                       = 10.

        IF sy-subrc = 0.
          t_grrec = payroll_res-nat-grrec.
          READ TABLE t_grrec INTO wa_grrec
                             WITH KEY grnum = t_pa0195-grnum
                                      ordcd = t_pa0195-ordcd
                                      rtype = '1'.
          IF sy-subrc = 0.
            pay_deduct_info-rbaln = wa_grrec-rbaln.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*   Get items only if amount or remaining balance is not zero
    IF NOT pay_deduct_info-betrg IS INITIAL OR
       NOT pay_deduct_info-rbaln IS INITIAL.
      APPEND pay_deduct_info.
    ENDIF.
    CLEAR pay_deduct_info.
  ENDLOOP.


* Output the result
  SORT pay_deduct_info BY ptype begda endda betrg DESCENDING.

  IF pay_deduct_info[] IS INITIAL.
    return-type = 'E'.
    return-code = '4B'.
    return-log_msg_no = '083'.

    MESSAGE ID return-code TYPE return-type NUMBER return-log_msg_no
            WITH return-message_v1 return-message_v2
                 return-message_v3 return-message_v4
       INTO return-message.
    APPEND return.
  ENDIF.

ENDFUNCTION.
