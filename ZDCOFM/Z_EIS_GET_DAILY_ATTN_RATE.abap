FUNCTION z_eis_get_daily_attn_rate.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"     VALUE(CHECK_TIME) TYPE  SY-UZEIT DEFAULT SY-UZEIT
*"     VALUE(BYPASS_BUFFER) TYPE  RS_ACTUAL OPTIONAL
*"     VALUE(NO_WS_SCHEDULE) TYPE  RS_ACTUAL OPTIONAL
*"  EXPORTING
*"     VALUE(RATE_HOURLY) TYPE  ZRATE_S_ATTN
*"     VALUE(RATE_SALARY) TYPE  ZRATE_H_ATTN
*"----------------------------------------------------------------------

  DATA : $clr,
         $end_time TYPE zclkout,
         $gap TYPE i,
         $ix TYPE i.

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
           zeity   LIKE t552a-zeity,
           mofid   LIKE t552a-mofid,
           mosid   LIKE t552a-mosid,
           schkz   LIKE t552a-schkz,

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

  DATA : __date TYPE datum,
         __time TYPE tims.

  RANGES r_schkz FOR pa0007-schkz.


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

  PERFORM get_info.

  __cls : it_status.
* get status {
  SELECT pernr  begda massn massg stat2 INTO TABLE it_status
  FROM pa0000
  WHERE begda <= check_date.
  SORT it_status BY pernr ASCENDING
                    begda DESCENDING .
  DELETE ADJACENT DUPLICATES FROM it_status
      COMPARING pernr.
* }


  SELECT a~pernr a~sname a~ename
         a~kostl a~orgeh a~sachz
         b~schkz a~persg a~persk
         INTO CORRESPONDING FIELDS OF TABLE it_pernr
           FROM pa0001 AS a INNER JOIN pa0007 AS b
             ON b~pernr = a~pernr
             WHERE a~begda LE check_date
               AND a~endda GE check_date
               AND b~begda LE check_date
               AND b~endda GE check_date.

  LOOP AT it_pernr.
    it_pernr-employeenumber = it_pernr-pernr+2.
    MODIFY it_pernr INDEX sy-tabix TRANSPORTING employeenumber.
  ENDLOOP.

  __cls itab.

  SELECT employeenumber readerid rdate rtime badge
    FROM zthr_bhisthmma
      INTO CORRESPONDING FIELDS OF TABLE itab
      WHERE rdate EQ check_date
     %_HINTS ORACLE 'FIRST_ROWS(10)'.


  PERFORM modi_itab USING check_date.

  DATA: BEGIN OF it_new_pernr OCCURS 0,
        pernr   LIKE   pa0001-pernr,
      END   OF it_new_pernr.

  LOOP AT it_pernr.
    $ix = sy-tabix.
    READ TABLE it_status WITH KEY pernr = it_pernr-employeenumber
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_pernr-perflg = true.
      MODIFY it_pernr INDEX $ix TRANSPORTING perflg.
      IF it_pernr-orgeh IS INITIAL.
        it_new_pernr-pernr = it_pernr-employeenumber.
        APPEND it_new_pernr.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DATA  $it_pernr LIKE it_pernr OCCURS 0 WITH HEADER LINE.

  IF NOT it_new_pernr[] IS INITIAL.
    SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
           b~schkz a~persg a~persk
           INTO CORRESPONDING FIELDS OF TABLE $it_pernr
             FROM pa0001 AS a INNER JOIN pa0007 AS b
               ON b~pernr = a~pernr
             FOR ALL ENTRIES IN it_new_pernr
               WHERE a~pernr EQ it_new_pernr-pernr
                 AND a~begda LE check_date
                 AND a~endda GE check_date
                 AND b~begda LE check_date
                 AND b~endda GE check_date.

    SORT $it_pernr BY pernr.

    LOOP AT it_pernr.
      $ix = sy-tabix.
     READ TABLE $it_pernr WITH KEY pernr = it_pernr-pernr BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_pernr = $it_pernr.
        it_pernr-perflg = true.
        MODIFY it_pernr INDEX $ix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  __cls it_row_tab.

  LOOP AT it_pernr.

    MOVE-CORRESPONDING it_pernr TO it_row_tab.

    IF NOT it_pernr-pernr IS INITIAL.
      READ TABLE it_status WITH KEY pernr = it_pernr-pernr
      BINARY SEARCH.
      IF sy-subrc EQ 0 AND
      ( it_status-stat2 EQ '1' OR it_status-stat2 EQ '3' ).
        it_row_tab-stat2 = it_status-stat2.
      ELSE.
        IF it_row_tab-kostl EQ '0000033301'.
          IF it_status-massn EQ 'ZX' AND it_status-massg EQ '17'.
            it_row_tab-stat2 = it_status-stat2.
          ELSE.
            CLEAR it_row_tab.
            CONTINUE.
          ENDIF.
        ELSE.
          CLEAR it_row_tab.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    IF it_pernr-perflg EQ false.
      it_row_tab-zflgtmp = true. " It's a temp. emp.
    ENDIF.

* fill description {
    READ TABLE it_kostx WITH KEY kostl = it_pernr-kostl
                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-kostx = it_kostx-kostx.
    ENDIF.
    READ TABLE it_orgtx WITH KEY orgeh = it_pernr-orgeh
                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-orgtx = it_orgtx-orgtx.
    ENDIF.
* }

    READ TABLE it_ws WITH KEY schkz = it_row_tab-schkz BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-rtext = it_ws-rtext.
      it_row_tab-anzsh = it_ws-anzsh.
    ENDIF.

    it_row_tab-rdate = check_date.
    APPEND it_row_tab.CLEAR it_row_tab.
  ENDLOOP.

*  sort it_row_tab by pernr.

  SORT it_row_tab BY employeenumber.

  DATA $cnt TYPE i.

  LOOP AT itab.
    AT NEW pernr.
      CLEAR $cnt.
    ENDAT.
    ADD 1 TO $cnt.

    CHECK $cnt LE 2.

    READ TABLE it_row_tab WITH KEY employeenumber = itab-employeenumber
BINARY SEARCH.

    IF sy-subrc EQ 0.
      CHECK itab-inout EQ '0' OR itab-inout EQ '1'.
      IF itab-inout EQ '0'. " In
        IF it_row_tab-zclkin IS INITIAL.
          it_row_tab-rdatei  = itab-rdate.
          it_row_tab-zdooridi = itab-readerid.
          it_row_tab-zdooridit = itab-door_desc.
          it_row_tab-zclkin  = itab-rtime.
          IF $cnt EQ 1. " Last read is 'in'...
            $cnt = 10.
          ENDIF.
        ENDIF.
      ENDIF.
      IF itab-inout EQ '1'. " Out
        IF it_row_tab-zclkout IS INITIAL.
          it_row_tab-rdateo  = itab-rdate.
          it_row_tab-zdoorido = itab-readerid.
          it_row_tab-zdooridot = itab-door_desc.
          it_row_tab-zclkout = itab-rtime.
        ENDIF.
      ENDIF.

      IF it_row_tab-anzsh EQ '2'.
        IF ( itab-rtime > '000000' AND itab-rtime < '030000' )
            AND itab-rdate EQ check_date.
          DELETE it_row_tab INDEX sy-tabix.
          CONTINUE.
        ENDIF.
      ENDIF.

      MODIFY it_row_tab INDEX sy-tabix.
    ENDIF.

  ENDLOOP.


* by ig.moon 10/13/2008 {
  IF no_ws_schedule IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE i_t001p
       FROM t001p INNER JOIN t500p
         ON t001p~werks = t500p~persa
       WHERE t500p~bukrs = g_kokrs.

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_t503
*     FROM t503.
    DATA p_toper(3) TYPE n.
    p_toper = check_date+4(2).

    SELECT * FROM t552a
        FOR ALL ENTRIES IN i_t001p
        WHERE mofid = i_t001p-mofid
          AND mosid = i_t001p-mosid
          AND kjahr = check_date(4)
          AND monat = p_toper.
      MOVE-CORRESPONDING t552a TO it_t552a. APPEND it_t552a.
    ENDSELECT.

    SORT it_t552a BY schkz.

    FIELD-SYMBOLS : <fs> TYPE ANY.
    DATA : num(2) TYPE n.
    DATA : f_field(14).

    CONCATENATE 'IT_T552A-TPR'check_date+6(2) INTO f_field.
    ASSIGN (f_field) TO <fs>.

    SORT it_row_tab BY schkz.
    DATA $flag.
    LOOP AT it_row_tab.
      AT NEW schkz.
        $flag = true.
      ENDAT.
      CHECK $flag EQ true.
      CLEAR $flag.
    READ TABLE it_t552a WITH KEY schkz = it_row_tab-schkz BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF <fs> EQ '1008'.
          DELETE it_row_tab WHERE schkz EQ it_row_tab-schkz.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

* }

  PERFORM  set_row_data USING check_date
                              check_time.


* calc rate {

  CLEAR : total_k_salary,total_us_salary,total_us_wage,
          avail_k_salary,avail_us_salary,avail_us_wage.

  DELETE it_row_tab WHERE zflgtmp EQ true.
  CLEAR it_row_tab.

  LOOP AT it_row_tab.

    IF it_row_tab-stat2 EQ '3'. " active
      IF it_row_tab-persg = '9' AND it_row_tab-persk = 'U2'.
        ADD 1 TO total_k_salary.
      ELSEIF it_row_tab-persg = '1' AND
          ( it_row_tab-persk = 'U2' OR it_row_tab-persk = 'U3' ).
        ADD 1 TO total_us_salary.
      ELSE.
        ADD 1 TO total_us_wage.
      ENDIF.
    ELSE.
      IF it_row_tab-kostl EQ '0000033301'.
        CLEAR it_status.
        READ TABLE it_status WITH KEY pernr = it_row_tab-pernr
        BINARY SEARCH.
        IF it_status-massn EQ 'ZX' AND it_status-massg EQ '17'.
          ADD 1 TO total_us_salary.
        ENDIF.
      ENDIF.
    ENDIF.

    CHECK it_row_tab-zaval EQ true.

    IF it_row_tab-persg = '9' AND it_row_tab-persk = 'U2'.
      ADD 1 TO avail_k_salary.
    ELSEIF ( ( it_row_tab-persg = '1' AND it_row_tab-persk = 'U2' ) OR
              ( it_row_tab-persg = '1' AND it_row_tab-persk = 'U3' ) ).
      ADD 1 TO avail_us_salary.
    ELSE.
      ADD 1 TO avail_us_wage.
    ENDIF.

  ENDLOOP.

* }

  IF total_us_wage <> 0.
    rate_hourly = avail_us_wage / total_us_wage * 100.
  ENDIF.

  IF total_us_salary <> 0.
    rate_salary = avail_us_salary / total_us_salary * 100.
  ENDIF.

  SET PARAMETER ID 'ZATH' FIELD rate_hourly.
  SET PARAMETER ID 'ZATS' FIELD rate_salary.

ENDFUNCTION.
