FUNCTION z_eis_get_daily_attn_rate_new.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     VALUE(CHECK_TIME) TYPE  SY-UZEIT DEFAULT SY-UZEIT
*"     VALUE(BYPASS_BUFFER) TYPE  RS_ACTUAL OPTIONAL
*"     VALUE(NO_WS_SCHEDULE) TYPE  RS_ACTUAL OPTIONAL
*"     VALUE(EXCL_INACTIVE) TYPE  RS_ACTUAL OPTIONAL
*"     VALUE(FILL_DETAIL) TYPE  RS_ACTUAL OPTIONAL
*"  EXPORTING
*"     VALUE(RATE_HOURLY) TYPE  ZRATE_S_ATTN
*"     VALUE(RATE_SALARY) TYPE  ZRATE_H_ATTN
*"  TABLES
*"      IT_RESULT STRUCTURE  ZATTRATERESULT OPTIONAL
*"      IT_KOSTLT STRUCTURE  HRCA_COSTC_RANGE OPTIONAL
*"----------------------------------------------------------------------

  fill_detail = true.

  DATA  $ix TYPE i.
  DATA p_toper(3) TYPE n.
  DATA $flag.
  DATA $tprog TYPE  tprog .
  DATA $schkn TYPE  schkn .
  DATA $flag_sat.

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

  DATA :
        BEGIN OF it_schkz OCCURS 0,
          schkz LIKE t550a-tprog  ,
        END OF it_schkz .

  RANGES r_schkz FOR t550a-tprog OCCURS 0.
  RANGES r_schkz2 FOR t550a-tprog OCCURS 0.
  RANGES r_schkz3 FOR t550a-tprog OCCURS 0.
  RANGES r_schkz4 FOR t550a-tprog OCCURS 0.

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
        schkz  TYPE schkn,
        pernr LIKE pa0001-pernr  ,
        persg  TYPE persg,
        persk  TYPE persk,
        kostl  TYPE kostl,
        employeenumber LIKE zthr_bhisthmma-employeenumber,
        schkz2  TYPE schkn,
        sobeg TYPE sobeg,
        soend TYPE soend,
      END OF it_pa0001           .

  DATA :
      BEGIN OF it_kostl_a OCCURS 0,
        kostl  TYPE kostl,
        gubun,
        cnt    TYPE i,
      END OF it_kostl_a           .

  DATA :
      BEGIN OF it_kostl_t OCCURS 0,
        kostl  TYPE kostl,
        gubun,
        cnt    TYPE i,
      END OF it_kostl_t.

  DATA :
      BEGIN OF it_kostl OCCURS 0,
        kostl  TYPE kostl,
        khinr  TYPE khinr,
        ltext  TYPE kltxt,
      END OF it_kostl           .

  DATA $check_date LIKE check_date.

  DATA :
      BEGIN OF it_row_tab OCCURS 20,
      khinr  TYPE khinr.
          INCLUDE STRUCTURE zattrateresult.
  DATA :
        hourly_tm_t LIKE zattrateresult-hourly_tm,
        salary_tm_t LIKE zattrateresult-salary_tm,
        total_tm_t LIKE zattrateresult-total_tm,
      END OF it_row_tab           .

  DATA total_tab LIKE it_row_tab OCCURS 1 WITH HEADER LINE.

  CONVERT DATE sy-datum TIME sy-uzeit
        INTO TIME STAMP curr_time TIME ZONE sy-zonlo.

  IF bypass_buffer EQ true." OR fill_detail EQ true.
  ELSE.
    GET PARAMETER ID 'ZATT' FIELD prev_time.

    IF NOT prev_time IS INITIAL.
      diff_time = curr_time - prev_time.
      IF diff_time < 600.
        GET PARAMETER ID 'ZATC' FIELD saved_date.
        IF saved_date EQ check_date.
          GET PARAMETER ID 'ZATH' FIELD rate_hourly.
          GET PARAMETER ID 'ZATS' FIELD rate_salary.
          IMPORT it_result FROM MEMORY ID 'ZAT_ITAB'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SET PARAMETER ID 'ZATT' FIELD curr_time.
  SET PARAMETER ID 'ZATC' FIELD check_date.

************

  __cls it_kostl.

  SELECT a~kostl a~khinr b~ltext
   INTO TABLE it_kostl
    FROM csks AS a
    INNER JOIN cskt AS b
    ON  b~kokrs EQ a~kokrs
    AND b~kostl EQ a~kostl
    WHERE b~spras EQ sy-langu
      AND b~datbi EQ '99991231'.

  __cls : r_press,r_body ,r_paint, r_ga,
          r_lambda,r_theta,r_prd,r_admin.

  PERFORM get_cc_group  TABLES
            : r_press USING check_date 'HMMA-321', " Press
              r_body USING check_date 'HMMA-322',  " body
              r_paint USING check_date 'HMMA-323', " Paint
              r_ga USING check_date 'HMMA-324',    " GA
              r_lambda USING check_date 'HMMA-331',"Lambda
              r_theta USING check_date 'HMMA-332', "Theta
              r_prd USING check_date 'HMMA-300'.  " Prd. Support

  PERFORM get_cc_group_excl  TABLES r_prd USING check_date :
                      'HMMA-320',
                      'HMMA-331',
                      'HMMA-332'.

  PERFORM get_cc_group  TABLES r_admin USING check_date    :
                      'HMMA-100',
                      'HMMA-200',
                      'HMMA-400'.

  SORT it_kostl BY kostl.
  DELETE ADJACENT DUPLICATES FROM it_kostl COMPARING kostl.

  SORT : r_press BY low,
         r_body  BY low,
         r_paint  BY low,
         r_ga  BY low,
         r_lambda  BY low,
         r_theta  BY low,
         r_prd  BY low,
         r_admin  BY low.

************

* Get Daily Work Schedule
  SELECT * FROM t550a
  WHERE begda <= check_date
    AND endda >= check_date .

    IF t550a-tprog = '2006'.
*        break-point.
    ENDIF.

    IF t550a-sollz >= 8.
      soxxx = '020000'.
      soend = t550a-soend." + soxxx.
    ENDIF.

    soxxx = '003000'.
    sobeg = t550a-sobeg." - soxxx.

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
        AND monat = p_toper.

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
        r_schkz2-low = <fs>.
        APPEND r_schkz2.
      ENDIF.
    ENDIF.
  ENDLOOP.

* by.ig.moon {
  READ TABLE r_schkz2 INDEX 1.
  IF sy-subrc EQ 0.
  ELSE.

* by ig.moon for SAT {
    $flag_sat = 'X'.
* } 5/14/2011

*    r_schkz2 = 'IEQ'.
*    r_schkz2-low = '1010'.
*    APPEND r_schkz2.
*    r_schkz2 = 'IEQ'.
*    r_schkz2-low = '1016'.
*    APPEND r_schkz2.

  ENDIF.
* }

  SORT r_schkz2.
  DELETE ADJACENT DUPLICATES FROM r_schkz2 COMPARING low.

  SELECT  b~schkz a~pernr persg persk kostl
               FROM pa0001 AS a
               INNER JOIN pa0007 AS b
               ON b~pernr EQ a~pernr
               INTO TABLE it_pa0001
               WHERE a~begda <= check_date
                 AND a~endda >= check_date
                 AND b~begda <= check_date
                 AND b~endda >= check_date
                 AND a~kostl IN it_kostlt .

* 3/24/2009 by ig. moon                  {
*                 AND abkrs NE '13'.
* }

  __cls : it_status.

  SELECT pernr  begda massn massg stat2 INTO TABLE it_status
  FROM pa0000
  WHERE begda <= check_date.
  SORT it_status BY pernr ASCENDING
                    begda DESCENDING .
  DELETE ADJACENT DUPLICATES FROM it_status
      COMPARING pernr.

  SORT it_status.

  LOOP AT it_pa0001.
    $ix = sy-tabix.
    READ TABLE it_status WITH KEY pernr = it_pa0001-pernr
    BINARY SEARCH.
    IF sy-subrc EQ 0 AND it_status-stat2 EQ '0'.
      DELETE it_pa0001 INDEX $ix.
    ENDIF.
  ENDLOOP.

* for debug {
*  LOOP AT it_pa0001.
*    $ix = sy-tabix.
*    READ TABLE r_theta WITH KEY low = it_pa0001-kostl BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      CONTINUE.
*    else.
*      DELETE it_pa0001 INDEX $ix.
*    ENDIF.
*  ENDLOOP.
* }

  SORT it_pa0001 BY schkz .

  LOOP AT it_pa0001.

    AT NEW schkz.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.

    CALL FUNCTION 'Z_CO_GET_DWS_IG'
         EXPORTING
              schkz                          = it_pa0001-schkz
              datum                          = check_date
         IMPORTING
              tprog                          = $tprog
         EXCEPTIONS
              not_found_work_schedule_rules  = 1
              invalid_date                   = 2
              not_found_period_work_schedule = 3
              OTHERS                         = 4.

    IF $tprog IS INITIAL.
      $tprog = it_pa0001-schkz.
    ENDIF.

    SELECT SINGLE sobeg soend INTO (it_pa0001-sobeg,it_pa0001-soend)
     FROM t550a WHERE motpr EQ '9'
                  AND tprog EQ $tprog
                  AND endda EQ '99991231'.

    it_pa0001-schkz2 = $tprog.

    MODIFY it_pa0001 TRANSPORTING schkz2 sobeg soend
    WHERE schkz EQ it_pa0001-schkz.

  ENDLOOP.

  IF $flag_sat EQ 'X'.

    IF check_time <= '060000'.

      $check_date = check_date - 1.

      SELECT employeenumber readerid rdate rtime badge
        INTO CORRESPONDING FIELDS OF TABLE itab2
        FROM zthr_bhisthmma
          WHERE ( ( rdate EQ $check_date AND rtime GT '160000' )
             OR ( rdate EQ check_date AND rtime LE '060000' ) ).

      $check_date = $check_date + 1.

    ELSE.

      SELECT employeenumber readerid rdate rtime badge
        INTO CORRESPONDING FIELDS OF TABLE itab2
        FROM zthr_bhisthmma
          WHERE rdate EQ check_date.

    ENDIF.


    LOOP AT itab2.
      IF itab2-rdate EQ check_date AND
         itab2-rtime > check_time.
        DELETE  itab2 INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    SORT itab2 BY  employeenumber ASCENDING
                  rdate rtime DESCENDING.

    DELETE ADJACENT DUPLICATES FROM itab2
        COMPARING employeenumber.

    LOOP AT itab2.
      itab2-pernr = itab2-employeenumber.
      IF itab2-pernr(4) EQ '0000'.
        DELETE itab2 INDEX sy-tabix.
        CONTINUE.
      ENDIF.

      MODIFY itab2 INDEX sy-tabix TRANSPORTING pernr.
    ENDLOOP.
    SORT itab2 BY pernr.

  ENDIF.

  DATA : $sobeg TYPE uzeit,
         $soend TYPE uzeit.

  LOOP AT it_pa0001.
    $ix = sy-tabix.
    $sobeg = it_pa0001-sobeg." - '003000'.
    $soend = it_pa0001-soend." + '003000'.
    IF $sobeg <= check_time AND $soend >= check_time.
    ELSE.
      DELETE it_pa0001 INDEX $ix.
    ENDIF.
  ENDLOOP.

  LOOP AT it_pa0001.
    $ix = sy-tabix.
    READ TABLE r_schkz2 WITH KEY low = it_pa0001-schkz2
         BINARY SEARCH.
    IF sy-subrc EQ 0.
    ELSE.

      READ TABLE r_lambda WITH KEY low = it_pa0001-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.

        r_schkz3-low = it_pa0001-schkz2.
        COLLECT r_schkz3.

        CONTINUE.
      ENDIF.

      READ TABLE r_theta WITH KEY low = it_pa0001-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.

        r_schkz3-low = it_pa0001-schkz2.
        COLLECT r_schkz3.

        CONTINUE.
      ENDIF.

      r_schkz4-low = it_pa0001-schkz2.
      COLLECT r_schkz4.
      IF $flag_sat IS INITIAL.
        READ TABLE itab2 WITH KEY pernr = it_pa0001-pernr BINARY SEARCH.
        IF sy-subrc NE 0.
          DELETE it_pa0001 INDEX $ix.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  READ TABLE it_pa0001 INDEX 1.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

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

  CLEAR :
      total_k_salary, total_us_salary, total_us_wage,
      avail_k_salary, avail_us_salary, avail_us_wage.

  __cls it_kostl_t.

  LOOP AT itab.
    $ix = sy-tabix.
    READ TABLE it_status WITH KEY pernr = itab-pernr
    BINARY SEARCH.
    IF sy-subrc EQ 0 AND it_status-stat2 EQ '0'.
      DELETE itab INDEX $ix.
    ENDIF.
  ENDLOOP.

  LOOP AT it_pa0001.
    CLEAR it_kostl_t.
    READ TABLE it_status WITH KEY pernr = it_pa0001-pernr
    BINARY SEARCH.
    IF excl_inactive EQ 'X'.
      IF sy-subrc EQ 0 AND it_status-stat2 EQ '3'.
        IF it_pa0001-persg = '9' AND it_pa0001-persk = 'U2'.
          ADD 1 TO total_k_salary.
          it_kostl_t-gubun = 'S'.

       ELSEIF ( ( it_pa0001-persg = '1' AND it_pa0001-persk = 'U2' ) OR
                 ( it_pa0001-persg = '1' AND it_pa0001-persk = 'U3' ) ).
          ADD 1 TO total_us_salary.
          it_kostl_t-gubun = 'S'.
        ELSE.
          ADD 1 TO total_us_wage.
          it_kostl_t-gubun = 'H'.
        ENDIF.
      ELSE.
        $flag = space. " for degug
      ENDIF.
    ELSE.
      IF sy-subrc EQ 0 AND
        ( it_status-stat2 EQ '1' OR it_status-stat2 EQ '3' ) .
        IF it_pa0001-persg = '9' AND it_pa0001-persk = 'U2'.
          ADD 1 TO total_k_salary.
          it_kostl_t-gubun = 'S'.
       ELSEIF ( ( it_pa0001-persg = '1' AND it_pa0001-persk = 'U2' ) OR
                 ( it_pa0001-persg = '1' AND it_pa0001-persk = 'U3' ) ).
          ADD 1 TO total_us_salary.
          it_kostl_t-gubun = 'S'.
        ELSE.
          ADD 1 TO total_us_wage.
          it_kostl_t-gubun = 'H'.
        ENDIF.
      ELSE.
        $flag = space. " for degug
      ENDIF.

    ENDIF.

    it_kostl_t-kostl = it_pa0001-kostl.
    it_kostl_t-cnt = 1.
    COLLECT it_kostl_t.        " employee total

  ENDLOOP.

  __cls it_kostl_a.

  LOOP AT itab.
    CLEAR it_kostl_a.

    READ TABLE it_status WITH KEY pernr = itab-pernr
    BINARY SEARCH.

    IF sy-subrc EQ 0 AND it_status-stat2 EQ '3'.
      READ TABLE it_pa0001 WITH KEY pernr = itab-pernr
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF it_pa0001-persg = '9' AND it_pa0001-persk = 'U2'.
          ADD 1 TO avail_k_salary.
          it_kostl_a-gubun = 'S'.
       ELSEIF ( ( it_pa0001-persg = '1' AND it_pa0001-persk = 'U2' ) OR
                 ( it_pa0001-persg = '1' AND it_pa0001-persk = 'U3' ) ).
          ADD 1 TO avail_us_salary.
          it_kostl_a-gubun = 'S'.
        ELSE.
          ADD 1 TO avail_us_wage.
          it_kostl_a-gubun = 'H'.
        ENDIF.

        it_kostl_a-kostl = it_pa0001-kostl.
        it_kostl_a-cnt = 1.
        COLLECT it_kostl_a.       " time table

      ENDIF.
    ENDIF.

  ENDLOOP.

  IF fill_detail EQ true.

    SORT it_kostl_a BY kostl gubun.
    SORT it_kostl_t BY kostl gubun.

    __cls it_row_tab.

    LOOP AT it_kostl.

      CLEAR it_row_tab.
      READ TABLE it_kostl_a WITH KEY kostl = it_kostl-kostl
                                     gubun = 'H'
                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-hourly_tm = it_kostl_a-cnt.
      ENDIF.
      READ TABLE it_kostl_a WITH KEY kostl = it_kostl-kostl
                                     gubun = 'S'
                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-salary_tm = it_kostl_a-cnt.
      ENDIF.

      READ TABLE it_kostl_t WITH KEY kostl = it_kostl-kostl
                                     gubun = 'H'
                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-hourly_tm_t = it_kostl_t-cnt.
      ENDIF.
      READ TABLE it_kostl_t WITH KEY kostl = it_kostl-kostl
                                     gubun = 'S'
                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-salary_tm_t = it_kostl_t-cnt.
      ENDIF.

      it_row_tab-total_tm = it_row_tab-hourly_tm + it_row_tab-salary_tm.
      it_row_tab-total_tm_t = it_row_tab-hourly_tm_t +
                              it_row_tab-salary_tm_t.

      READ TABLE r_press WITH KEY low = it_kostl-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-khinr = 'A'.
        it_row_tab-ltext = 'Press'.
        COLLECT it_row_tab.
        CONTINUE.
      ENDIF.

      READ TABLE r_body WITH KEY low = it_kostl-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-khinr = 'B'.
        it_row_tab-ltext = 'Body'.
        COLLECT it_row_tab.
        CONTINUE.
      ENDIF.

      READ TABLE r_paint WITH KEY low = it_kostl-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-khinr = 'C'.
        it_row_tab-ltext = 'Paint'.
        COLLECT it_row_tab.
        CONTINUE.
      ENDIF.

      READ TABLE r_ga WITH KEY low = it_kostl-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-khinr = 'D'.
        it_row_tab-ltext = 'GA'.
        COLLECT it_row_tab.
        CONTINUE.
      ENDIF.

      READ TABLE r_lambda WITH KEY low = it_kostl-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.

               continue.
        it_row_tab-khinr = 'E'.
        it_row_tab-ltext = 'Lambda'.
        COLLECT it_row_tab.
        CONTINUE.
      ENDIF.

      READ TABLE r_theta WITH KEY low = it_kostl-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.

               continue.
        it_row_tab-khinr = 'F'.
        it_row_tab-ltext = 'Theta'.
        COLLECT it_row_tab.
        CONTINUE.
      ENDIF.

      READ TABLE r_prd WITH KEY low = it_kostl-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-khinr = 'G'.
        it_row_tab-ltext = 'Prod. Support'.
        COLLECT it_row_tab.
        CONTINUE.
      ENDIF.

      READ TABLE r_admin WITH KEY low = it_kostl-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-khinr = 'H'.
        it_row_tab-ltext = 'Admin'.
        COLLECT it_row_tab.
        CONTINUE.
      ENDIF.

      CONTINUE.

    ENDLOOP.
    SORT it_row_tab BY khinr.

    __cls total_tab.
    LOOP AT it_row_tab.
      $ix = sy-tabix.
      IF it_row_tab-hourly_tm > 0.
        it_row_tab-h_rate = it_row_tab-hourly_tm
                                    / it_row_tab-hourly_tm_t * 100.
      ENDIF.
      IF it_row_tab-salary_tm > 0.
        it_row_tab-s_rate = it_row_tab-salary_tm
                                    / it_row_tab-salary_tm_t * 100.
      ENDIF.
      IF it_row_tab-total_tm > 0.
        it_row_tab-total_rate = it_row_tab-total_tm
                                    / it_row_tab-total_tm_t * 100.
      ENDIF.
      MODIFY it_row_tab INDEX $ix.

      AT LAST.
        SUM.
        total_tab = it_row_tab.
        total_tab-khinr = 'Z'.
        IF total_tab-hourly_tm > 0.
          total_tab-h_rate = total_tab-hourly_tm
                                      / total_tab-hourly_tm_t * 100.
        ENDIF.
        IF total_tab-salary_tm > 0.
          total_tab-s_rate = total_tab-salary_tm
                                      / total_tab-salary_tm_t * 100.
        ENDIF.
        IF total_tab-total_tm > 0.
          total_tab-total_rate = total_tab-total_tm
                                      / total_tab-total_tm_t * 100.
        ENDIF.
        total_tab-ltext = 'Total'.
        APPEND total_tab.
      ENDAT.

    ENDLOOP.

    APPEND LINES OF total_tab TO it_row_tab.

    SORT it_row_tab BY khinr.

    __cls it_result.

    LOOP AT it_row_tab.
      MOVE-CORRESPONDING it_row_tab TO it_result.
      APPEND it_result.
      IF it_row_tab-khinr EQ 'Z'.
        rate_hourly = it_result-h_rate.
        rate_salary = it_result-s_rate.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF total_us_wage <> 0.
      rate_hourly = avail_us_wage / total_us_wage * 100.
    ENDIF.

    total_us_salary = total_us_salary + total_k_salary.
    avail_us_salary = avail_us_salary + avail_k_salary.

    IF total_us_salary <> 0.
      rate_salary = avail_us_salary / total_us_salary * 100.
    ENDIF.
  ENDIF.

  SET PARAMETER ID 'ZATH' FIELD rate_hourly.
  SET PARAMETER ID 'ZATS' FIELD rate_salary.

  EXPORT it_result TO MEMORY ID 'ZAT_ITAB'.

ENDFUNCTION.
