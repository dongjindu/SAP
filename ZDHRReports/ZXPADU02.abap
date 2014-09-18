*----------------------------------------------------------------------*
*   INCLUDE ZXPADU02                                                   *

DATA:
     s_persk LIKE pspar-persk,
     s_begda LIKE pspar-begda,
     s_subty LIKE pa2001-awart,
     s_stell LIKE pa0001-stell,
     w_pernr LIKE p0001-pernr,
     l_arbst LIKE pa0007-arbst,
     l_incr  LIKE pa0007-arbst,
     s_kostl LIKE pa0001-kostl.

DATA: lt_subtype LIKE TABLE OF zthr_tm01 WITH HEADER LINE.
RANGES: r_subty FOR zthr_tm01-aa.

DATA $trfst TYPE trfst.

FIELD-SYMBOLS: <status>.

DATA: BEGIN OF rt_dis OCCURS 0,
        persk       LIKE pa0001-persk,
        subty       LIKE pa2001-awart,
        begda       LIKE zptimepm-begda,
        endda       LIKE pa0002-endda,
        msg         LIKE zptimepm-msg,
         END OF rt_dis.

DATA: BEGIN OF it_dis OCCURS 0,
  stell       LIKE pa0001-stell,
  begda       LIKE pa0002-begda,
  endda       LIKE pa0002-endda,
  END OF it_dis.

INFOTYPES: 0000, 0001, 0007.

DATA: iholiday TYPE STANDARD TABLE OF iscal_day WITH HEADER LINE,
      l_day(1),
      l_schkz LIKE pa0007-schkz,
      rc TYPE i,
      l_tprog LIKE t551a-tprg1.

DATA: BEGIN OF psp OCCURS 33.
        INCLUDE STRUCTURE pc2ba.
DATA: END OF psp.

DATA: l_eligi LIKE pa9100-eligi,
      l_new_state LIKE l_eligi.

w_pernr =  innnn-pernr.
s_subty =  innnn-subty.

CLEAR: it_dis, it_dis[].


CASE innnn-infty.
** Furong on 07/31/14
  WHEN '9100'.
* Checking current enroll status
    SELECT SINGLE eligi INTO l_eligi
           FROM pa9100
           WHERE pernr = w_pernr AND
                 begda <= sy-datum AND
                 endda >= sy-datum .

    IF sy-subrc = 0.
    ELSE.
      l_eligi = 'NE'.
    ENDIF.
    l_new_state = innnn-data1+0(2).
    CASE l_eligi.
      WHEN 'E'.
        IF l_new_state = 'S1' OR
           l_new_state = 'S2' OR
           l_new_state = 'SE' OR
           l_new_state = 'E'.
        ELSE.
          MESSAGE e001(zmhr) WITH 'Status not allowed'.
        ENDIF.
      WHEN 'S1'.
        IF l_new_state = 'S1' OR
           l_new_state = 'E'.
        ELSE.
          MESSAGE e001(zmhr) WITH 'Status not allowed'.
        ENDIF.
      WHEN 'S2'.
        IF l_new_state = 'S2' OR
           l_new_state = 'E'.
        ELSE.
          MESSAGE e001(zmhr) WITH 'Status not allowed'.
        ENDIF.
      WHEN 'SE'.
        IF l_new_state = 'SE' OR
           l_new_state = 'E'.
        ELSE.
          MESSAGE e001(zmhr) WITH 'Status not allowed'.
        ENDIF.
      WHEN 'NE'.
        IF l_new_state = 'E'.
        ELSE.
          MESSAGE e001(zmhr) WITH 'Status not allowed'.
        ENDIF.
    ENDCASE.
** )

  WHEN '0008'.
* Checking PS level in updating IT0008
    CHECK sy-ucomm NE 'UPDL'.
    SELECT SINGLE stell begda persk kostl
             INTO (s_stell,s_begda, s_persk, s_kostl)
             FROM pa0001
             WHERE pernr = w_pernr AND
                   begda <= sy-datum AND
                   endda >= sy-datum .

    IF s_persk EQ 'U0'.
      SELECT SINGLE trfst INTO $trfst FROM pa0008
      WHERE pernr EQ w_pernr
        AND subty EQ '0'
        AND endda EQ '99991231'.
      IF sy-subrc EQ 0.
        IF innnn-data1+12(2) < $trfst.
          MESSAGE e001(zmhr) WITH
          'Pay Scale Level is lower than the previous one'.
        ENDIF.
      ENDIF.
    ENDIF.
  WHEN '2001' OR '2002'.
    IF sy-ucomm NE 'UPDL'.
* Don't allow Overtime & Straight time to Manager & Above
      SELECT SINGLE stell begda persk kostl
             INTO (s_stell,s_begda, s_persk, s_kostl)
             FROM pa0001
             WHERE pernr = w_pernr AND
                   begda <= sy-datum AND
                   endda >= sy-datum .

      SELECT stell INTO TABLE it_dis FROM zhrjobkey.

      READ TABLE it_dis WITH KEY stell = s_stell.

      IF sy-subrc = 0 AND
        (  s_subty  = '1001' OR
           s_subty  = '1003' OR
           s_subty  = '1004' OR
           s_subty  = '1005' OR
           s_subty  = '1007' OR
           s_subty  = '1006' )
        AND innnn-infty <> '0416'.
        MESSAGE e020(zmhr).
      ENDIF.

* Check table ZPTIMEPM for permissibility.

      CLEAR: rt_dis, rt_dis[].

      SELECT * INTO CORRESPONDING FIELDS OF TABLE rt_dis
              FROM zptimepm.

      READ TABLE rt_dis WITH KEY persk = s_persk
                               subty = s_subty.

      IF sy-subrc = 0.
        MESSAGE e001(zmhr) WITH rt_dis-msg.
      ENDIF.

* Subtypes with Flag 2 only allowed on Non-production day.

      IF sy-ucomm = 'UPD'.

        SELECT * INTO TABLE lt_subtype
          FROM zthr_tm01
          WHERE flag = '2'.

        LOOP AT lt_subtype.
          r_subty-option = 'EQ'.
          r_subty-sign = 'I'.
          r_subty-low = lt_subtype-aa.
          APPEND r_subty.
        ENDLOOP.

        IF s_subty IN r_subty.
          CLEAR : iholiday[], iholiday.
          CALL FUNCTION 'HOLIDAY_GET'
            EXPORTING
              holiday_calendar = 'U1'
              date_from        = innnn-begda
              date_to          = innnn-begda
            TABLES
              holidays         = iholiday.

          SELECT SINGLE arbst schkz INTO (l_arbst, l_schkz)
                 FROM pa0007
                 WHERE pernr = w_pernr
                   AND begda <= innnn-begda
                   AND endda >= innnn-begda.

          CALL FUNCTION 'DATE_COMPUTE_DAY'
            EXPORTING
              date = innnn-begda
            IMPORTING
              day  = l_day.

* WS4001 works Fri thru Mon.

          IF ( l_day EQ '5' OR l_day EQ '6' OR
             l_day EQ '7' OR l_day EQ '1' ) AND ( l_schkz EQ '4001' ).

            READ TABLE iholiday INDEX 1.
            IF sy-subrc EQ 0 AND iholiday-holiday EQ 'X'.
            ELSE.
              MESSAGE e001(zmhr) WITH text-004.
            ENDIF.
          ENDIF.

* Check daily work schedule

          CALL FUNCTION 'HR_READ_INFOTYPE'
            EXPORTING
              pernr           = w_pernr
              infty           = '0000'
              begda           = innnn-begda
              endda           = innnn-begda
            IMPORTING
              subrc           = rc
            TABLES
              infty_tab       = p0000
            EXCEPTIONS
              infty_not_found = 1
              OTHERS          = 2.

          CALL FUNCTION 'HR_READ_INFOTYPE'
            EXPORTING
              pernr           = w_pernr
              infty           = '0001'
              begda           = innnn-begda
              endda           = innnn-begda
            IMPORTING
              subrc           = rc
            TABLES
              infty_tab       = p0001
            EXCEPTIONS
              infty_not_found = 1
              OTHERS          = 2.

          CALL FUNCTION 'HR_READ_INFOTYPE'
            EXPORTING
              pernr           = w_pernr
              infty           = '0007'
              begda           = innnn-begda
              endda           = innnn-begda
            IMPORTING
              subrc           = rc
            TABLES
              infty_tab       = p0007
            EXCEPTIONS
              infty_not_found = 1
              OTHERS          = 2.

          CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
            EXPORTING
              pernr             = w_pernr
              begda             = innnn-begda
              endda             = innnn-begda
              switch_activ      = 1
              i0001_i0007_error = '0'
              read_cluster      = 'X'
            TABLES
              i0000             = p0000
              i0001             = p0001
              i0007             = p0007
              perws             = psp
            EXCEPTIONS
              error_occured     = 1
              abort_occured     = 2
              OTHERS            = 3.
          IF sy-subrc <> 0.
          ENDIF.

          l_tprog = psp-tprog.

          READ TABLE iholiday INDEX 1.
          IF sy-subrc EQ 0 AND iholiday-holiday EQ 'X'.
          ELSE.
            IF l_tprog <> '1008' AND l_tprog <> '1009'
            AND l_tprog <> '1011' AND l_tprog <> '1007'.
              MESSAGE e001(zmhr) WITH text-006.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

* Subtypes with Flag 1 should be checked for Increment.

    REFRESH: lt_subtype, r_subty.
    SELECT * INTO TABLE lt_subtype
      FROM zthr_tm01
      WHERE flag = '1'.

    LOOP AT lt_subtype.
      r_subty-option = 'EQ'.
      r_subty-sign = 'I'.
      r_subty-low = lt_subtype-aa.
      APPEND r_subty.
    ENDLOOP.

    IF innnn-subty IN r_subty.
      IF sy-ucomm NE 'UPDL'.
        IF  innnn-subty EQ '1026' OR
            innnn-subty EQ '1019' .
          ASSIGN ('(MP200000)P2002-STDAZ') TO <status>.
        ELSE.
          ASSIGN ('(MP200000)P2001-STDAZ') TO <status>.
        ENDIF.
        SELECT SINGLE arbst INTO l_arbst
               FROM pa0007
               WHERE pernr = innnn-pernr
                AND begda <= innnn-begda
                AND endda >= innnn-endda.
        IF sy-subrc EQ 0   .
          l_incr = l_arbst / 2.  " Half day
          IF NOT ( ( <status> EQ l_arbst )  OR
                   ( <status> EQ l_incr ) ).
            MESSAGE e017(zmhr) .
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

* A/A 1075, Shutdown - Pay in Lieu, only allowed to Manager.

    IF sy-ucomm = 'UPD' AND innnn-subty EQ '1075'.
      IF s_stell <> '90000269'.
        MESSAGE e025(zmhr).
      ENDIF.
    ENDIF.

* Staffing codes only allowed Hourly/Non-maintenance TMs.

    IF sy-ucomm = 'UPD' AND
        ( innnn-subty EQ '1076' OR
          innnn-subty EQ '1077' OR
           innnn-subty EQ '1054' ).
      IF ( s_persk = 'U0'
         AND ( s_kostl = '0000055103' OR s_kostl = '0000055104'
               OR s_kostl = '0000055107' ) ) OR
         ( s_persk = 'U2' OR s_persk = 'U3' OR s_persk = 'UD') .
        MESSAGE e026(zmhr).
      ENDIF.
    ENDIF.

** * Subtypes with Flag 3 check maximum work hour
    REFRESH: lt_subtype.
    SELECT * INTO TABLE lt_subtype
      FROM zthr_tm01
      WHERE flag = '3'.

    REFRESH: r_subty.
    LOOP AT lt_subtype.
      r_subty-option = 'EQ'.
      r_subty-sign = 'I'.
      r_subty-low = lt_subtype-aa.
      APPEND r_subty.
    ENDLOOP.

    IF innnn-subty IN r_subty.
      ASSIGN ('(MP200000)P2002-STDAZ') TO <status>.
      IF sy-subrc = 0.
        SELECT SINGLE arbst INTO l_arbst
               FROM pa0007
               WHERE pernr = innnn-pernr
                AND begda <= innnn-begda
                AND endda >= innnn-endda.
        IF sy-subrc EQ 0.
          IF <status> > l_arbst.
            MESSAGE e027(zmhr).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
ENDCASE.
