FUNCTION z_eis_get_daily_attn_rate_tmp.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"             VALUE(CHECK_TIME) TYPE  SY-UZEIT DEFAULT SY-UZEIT
*"             VALUE(BYPASS_BUFFER) TYPE  RS_ACTUAL OPTIONAL
*"             VALUE(NO_WS_SCHEDULE) TYPE  RS_ACTUAL OPTIONAL
*"             VALUE(EXCL_INACTIVE) TYPE  RS_ACTUAL OPTIONAL
*"       EXPORTING
*"             VALUE(RATE_HOURLY) TYPE  ZRATE_S_ATTN
*"             VALUE(RATE_SALARY) TYPE  ZRATE_H_ATTN
*"----------------------------------------------------------------------

  DATA  $ix TYPE i.
  DATA p_toper(3) TYPE n.
  DATA $flag.

  DATA : curr_time TYPE cvichutc,
         prev_time TYPE cvichutc,
         diff_time TYPE cvichutc,
         saved_date LIKE check_date.

  DATA : BEGIN OF i_t001p OCCURS 0,
           werks LIKE t001p-werks, "PA
           btrtl LIKE t001p-btrtl, "sub-area
           molga LIKE t001p-molga, "country
           mofid LIKE t001p-mofid, "Public Holiday Calendar
           mosid LIKE t001p-mosid, "Personnel Subarea Grouping for WS
         END OF i_t001p.

  DATA : BEGIN OF it_t552a  OCCURS 0,
           schkz   LIKE t552a-schkz,
           zeity   LIKE t552a-zeity,
           mofid   LIKE t552a-mofid,
           mosid   LIKE t552a-mosid,

           solst   LIKE t552a-solst,
           tpr01   LIKE t552a-tpr01,
           tpr02   LIKE t552a-tpr02,
           tpr03   LIKE t552a-tpr03,
           tpr04   LIKE t552a-tpr04,
           tpr05   LIKE t552a-tpr05,
           tpr06   LIKE t552a-tpr06,
           tpr07   LIKE t552a-tpr07,
           tpr08   LIKE t552a-tpr08,
           tpr09   LIKE t552a-tpr09,
           tpr10   LIKE t552a-tpr10,
           tpr11   LIKE t552a-tpr11,
           tpr12   LIKE t552a-tpr12,
           tpr13   LIKE t552a-tpr13,
           tpr14   LIKE t552a-tpr14,
           tpr15   LIKE t552a-tpr15,
           tpr16   LIKE t552a-tpr16,
           tpr17   LIKE t552a-tpr17,
           tpr18   LIKE t552a-tpr18,
           tpr19   LIKE t552a-tpr19,
           tpr20   LIKE t552a-tpr20,
           tpr21   LIKE t552a-tpr21,
           tpr22   LIKE t552a-tpr22,
           tpr23   LIKE t552a-tpr23,
           tpr24   LIKE t552a-tpr24,
           tpr25   LIKE t552a-tpr25,
           tpr26   LIKE t552a-tpr26,
           tpr27   LIKE t552a-tpr27,
           tpr28   LIKE t552a-tpr28,
           tpr29   LIKE t552a-tpr29,
           tpr30   LIKE t552a-tpr30,
           tpr31   LIKE t552a-tpr31,
        END OF it_t552a.

  DATA: BEGIN OF na_itab OCCURS 0,
            pernr LIKE pa0001-pernr,
        END OF na_itab.

  DATA :
        BEGIN OF it_schkz OCCURS 0,
          schkz LIKE t550a-tprog  ,
        END OF it_schkz .

  RANGES r_schkz FOR t550a-tprog OCCURS 0.
  RANGES r_schkz2 FOR t550a-tprog OCCURS 0.

  DATA sobeg TYPE sobeg.
  DATA soend TYPE soend.
  DATA soxxx TYPE soend.

  DATA chk_sobeg(14) TYPE n.
  DATA chk_soend(14) TYPE n.
  DATA chk_soxxx(14) TYPE n.
  DATA $check_time(14) TYPE n.

  FIELD-SYMBOLS : <fs> TYPE ANY.
  DATA : num(2) TYPE n.
  DATA : f_field(14).

  DATA :
      BEGIN OF it_pa0001 OCCURS 0,
        pernr LIKE pa0001-pernr  ,
        schkz  TYPE schkn,
        persg  TYPE persg,
        persk  TYPE persk,
        employeenumber LIKE zthr_bhisthmma-employeenumber,
      END OF it_pa0001           .
  DATA $check_date LIKE check_date.

  CONVERT DATE sy-datum TIME sy-uzeit
        INTO TIME STAMP curr_time TIME ZONE sy-zonlo.

  IF bypass_buffer EQ true.
  ELSE.
    GET PARAMETER ID 'ZATT' FIELD prev_time.

    IF NOT prev_time IS INITIAL.
      diff_time = curr_time - prev_time.
      IF diff_time < 600.
        GET PARAMETER ID 'ZATC' FIELD saved_date.
        IF saved_date EQ check_date.
          GET PARAMETER ID 'ZATH' FIELD rate_hourly.
          GET PARAMETER ID 'ZATS' FIELD rate_salary.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SET PARAMETER ID 'ZATT' FIELD curr_time.
  SET PARAMETER ID 'ZATC' FIELD check_date.

  SELECT * FROM t550a
  WHERE begda <= check_date
    AND endda >= check_date .

    IF sy-subrc EQ 0.

      IF t550a-sollz EQ 8.
        soxxx = '020000'.
        soend = t550a-soend + soxxx.
      ENDIF.

      soxxx = '003000'.
      sobeg = t550a-sobeg - soxxx.

      IF soend < sobeg.
        $check_date = check_date + 1.
        CONCATENATE check_date sobeg INTO chk_sobeg.
        CONCATENATE $check_date soend INTO chk_soend.
      ELSE.
        CONCATENATE check_date sobeg INTO chk_sobeg.
        CONCATENATE check_date soend INTO chk_soend.
      ENDIF.

      IF check_time < '060000'.
        $check_date = check_date + 1.
        CONCATENATE $check_date check_time INTO $check_time.
      ELSE.
        CONCATENATE check_date check_time INTO $check_time.
      ENDIF.

      IF $check_time BETWEEN chk_sobeg AND chk_soend.
        r_schkz = 'IEQ'.
        r_schkz-low = t550a-tprog.
        APPEND r_schkz.
      ENDIF.
    ENDIF.

  ENDSELECT.

  SORT r_schkz.
  DELETE ADJACENT DUPLICATES FROM r_schkz COMPARING low.

  PERFORM get_info.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_t001p
     FROM t001p INNER JOIN t500p
       ON t001p~werks = t500p~persa
     WHERE t500p~bukrs = g_kokrs.

  p_toper = check_date+4(2).

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_t552a
      FROM t552a
    FOR ALL ENTRIES IN i_t001p
      WHERE mofid = i_t001p-mofid
        AND mosid = i_t001p-mosid
        AND kjahr = check_date(4)
        AND monat = p_toper
        AND schkz IN r_schkz.

  SORT it_t552a BY schkz.

  CONCATENATE 'IT_T552A-TPR'check_date+6(2) INTO f_field.
  ASSIGN (f_field) TO <fs>.

  LOOP AT r_schkz.
    $ix = sy-tabix.
    READ TABLE it_t552a WITH KEY schkz = r_schkz-low BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF <fs> EQ '1008'.
        DELETE r_schkz INDEX $ix..
      ELSE.
        r_schkz2 = 'IEQ'.
        r_schkz2-low = it_t552a-schkz.
        APPEND r_schkz2.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT r_schkz2.
  DELETE ADJACENT DUPLICATES FROM r_schkz2 COMPARING low.

  SELECT a~pernr  b~schkz persg persk
               FROM pa0001 AS a
               INNER JOIN pa0007 AS b
               ON b~pernr EQ a~pernr
               INTO TABLE it_pa0001
               WHERE
                     a~begda <= check_date
                 AND a~endda >= check_date
                 AND b~begda <= check_date
                 AND b~endda >= check_date
                 AND abkrs NE '13'
                 AND b~schkz IN r_schkz2.

  SORT it_pa0001 BY pernr .

  LOOP AT it_pa0001.
    it_pa0001-employeenumber = it_pa0001-pernr+2.
    MODIFY it_pa0001 INDEX sy-tabix TRANSPORTING employeenumber.
  ENDLOOP.

  IF check_time <= '060000'.

    $check_date = check_date - 1.

    SELECT employeenumber readerid rdate rtime badge
      INTO CORRESPONDING FIELDS OF TABLE itab
      FROM zthr_bhisthmma
      FOR ALL ENTRIES IN it_pa0001
        WHERE employeenumber EQ it_pa0001-employeenumber
          AND ( ( rdate EQ $check_date AND rtime GT '160000' )
           OR ( rdate EQ check_date AND rtime LE '060000' ) )
       %_HINTS ORACLE 'FIRST_ROWS(10)'.

  ELSE.

    SELECT employeenumber readerid rdate rtime badge
      INTO CORRESPONDING FIELDS OF TABLE itab
      FROM zthr_bhisthmma
      FOR ALL ENTRIES IN it_pa0001
        WHERE employeenumber EQ it_pa0001-employeenumber
          AND rdate EQ check_date
       %_HINTS ORACLE 'FIRST_ROWS(10)'.

  ENDIF.

  LOOP AT itab.
    IF itab-rdate EQ check_date AND
       itab-rtime > check_time.
      DELETE  itab INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  SORT itab BY  employeenumber ASCENDING
                rdate rtime DESCENDING.

  DELETE ADJACENT DUPLICATES FROM itab
      COMPARING employeenumber.

  LOOP AT itab.
    itab-pernr = itab-employeenumber.
    MODIFY itab INDEX sy-tabix TRANSPORTING pernr.
  ENDLOOP.

  __cls : it_status.
  SELECT pernr  begda massn massg stat2 INTO TABLE it_status
  FROM pa0000
  WHERE begda <= check_date.
  SORT it_status BY pernr ASCENDING
                    begda DESCENDING .
  DELETE ADJACENT DUPLICATES FROM it_status
      COMPARING pernr.

  SORT it_status.

  CLEAR :
      total_k_salary, total_us_salary, total_us_wage,
      avail_k_salary, avail_us_salary, avail_us_wage.

  SORT itab BY pernr.

  LOOP AT it_pa0001.

    READ TABLE it_status WITH KEY pernr = it_pa0001-pernr
    BINARY SEARCH.

    IF excl_inactive EQ 'X'.

      IF sy-subrc EQ 0 AND it_status-stat2 EQ '3'.
        IF it_pa0001-persg = '9' AND it_pa0001-persk = 'U2'.
          ADD 1 TO total_k_salary.

          READ TABLE itab WITH KEY pernr = it_pa0001-pernr
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            ADD 1 TO avail_k_salary.
          ELSE.
            ADD 1 TO navail_k_salary.
          ENDIF.

       ELSEIF ( ( it_pa0001-persg = '1' AND it_pa0001-persk = 'U2' ) OR
                 ( it_pa0001-persg = '1' AND it_pa0001-persk = 'U3' ) ).
          ADD 1 TO total_us_salary.

          READ TABLE itab WITH KEY pernr = it_pa0001-pernr
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            ADD 1 TO avail_us_salary.
          ELSE.
            ADD 1 TO navail_us_salary.
          ENDIF.

        ELSE.
          ADD 1 TO total_us_wage.

          READ TABLE itab WITH KEY pernr = it_pa0001-pernr
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            ADD 1 TO avail_us_wage.
          ELSE.
            ADD 1 TO navail_us_wage.
          ENDIF.

        ENDIF.
      ELSE.
        $flag = space. " for degug
        ADD 1 TO  na_cnt.
      ENDIF.

    ELSE.

      IF sy-subrc EQ 0 AND
        ( it_status-stat2 EQ '1' OR it_status-stat2 EQ '3' ) .

        IF it_pa0001-persg = '9' AND it_pa0001-persk = 'U2'.

          ADD 1 TO total_k_salary.

          READ TABLE itab WITH KEY pernr = it_pa0001-pernr
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            ADD 1 TO avail_k_salary.
          ELSE.
            ADD 1 TO navail_k_salary.
            na_itab-pernr = it_pa0001-pernr.
            append na_itab.
          ENDIF.

        ELSEIF ( ( it_pa0001-persg = '1' AND it_pa0001-persk = 'U2' )
          OR   ( it_pa0001-persg = '1' AND it_pa0001-persk = 'U3' ) ).

          ADD 1 TO total_us_salary.

          READ TABLE itab WITH KEY pernr = it_pa0001-pernr
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            ADD 1 TO avail_us_salary.
          ELSE.
            ADD 1 TO navail_us_salary.
            na_itab-pernr = it_pa0001-pernr.
            append na_itab.
          ENDIF.

        ELSE.

          ADD 1 TO total_us_wage.

          READ TABLE itab WITH KEY pernr = it_pa0001-pernr
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            ADD 1 TO avail_us_wage.
          ELSE.
            ADD 1 TO navail_us_wage.
            na_itab-pernr = it_pa0001-pernr.
            append na_itab.
          ENDIF.

        ENDIF.
      ELSE.
        $flag = space. " for degug
        ADD 1 TO  na_cnt.
      ENDIF.

    ENDIF.

  ENDLOOP.

  IF total_us_wage <> 0.
    rate_hourly = avail_us_wage / total_us_wage * 100.
  ENDIF.

  total_us_salary = total_us_salary + total_k_salary.
  avail_us_salary = avail_us_salary + avail_k_salary.

  IF total_us_salary <> 0.
    rate_salary = avail_us_salary / total_us_salary * 100.
  ENDIF.

  SET PARAMETER ID 'ZATH' FIELD rate_hourly.
  SET PARAMETER ID 'ZATS' FIELD rate_salary.

ENDFUNCTION.
