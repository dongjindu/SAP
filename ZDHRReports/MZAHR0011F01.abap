*----------------------------------------------------------------------*
***INCLUDE MZAHR0011F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_main_data.
  DATA: l_zcode LIKE zthr_pcp02-zcode.

  CLEAR r_zjobc. REFRESH r_zjobc.
  CLEAR zthr_pcp02.
  SELECT zval1 zval4 INTO (zthr_pcp02-zval1, zthr_pcp02-zval4)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = w_zgrup.
    CONDENSE zthr_pcp02-zval4.
    IF w_zgrup = '1040' .
      IF zthr_pcp02-zval4 = 'OT'.
        r_zjobc-sign = 'I'.
        r_zjobc-option = 'EQ'.
        r_zjobc-low = zthr_pcp02-zval1.
        APPEND r_zjobc. CLEAR r_zjobc.
      ENDIF.
    ELSE.
      r_zjobc-sign = 'I'.
      r_zjobc-option = 'EQ'.
      r_zjobc-low = zthr_pcp02-zval1.
      APPEND r_zjobc. CLEAR r_zjobc.
    ENDIF.
    CLEAR zthr_pcp02.
  ENDSELECT.
*
  IF r_zjobc[] IS INITIAL.
    MESSAGE e009(zmhr) WITH 'Not Defind Work Type Production Job'.
  ENDIF.

  CLEAR it_maint. REFRESH it_maint.
*
  CLEAR zthr_pcp00.
* select by
  SELECT zobjc zsenr zhedc ancur zperg zsubg
         act03 act06 act07 mthly houry
    INTO (zthr_pcp00-zobjc, zthr_pcp00-zsenr,
          zthr_pcp00-zhedc, zthr_pcp00-ancur,
          zthr_pcp00-zperg, zthr_pcp00-zsubg,
          zthr_pcp00-act03, zthr_pcp00-act06,
          zthr_pcp00-act07, zthr_pcp00-mthly,
          zthr_pcp00-houry )

    FROM zthr_pcp00 WHERE zyear = w_zyear
                      AND zmons = w_zmons
                      AND zvers = w_zvers
                      AND zcost = w_kostl
                      AND zobjc IN r_zjobc
                      AND NOT ( zperg  = '9' AND zsubg = 'U2' ) .

    it_maint-zobjc = zthr_pcp00-zobjc.
    it_maint-zsenr = zthr_pcp00-zsenr.
    it_maint-zhedc = zthr_pcp00-zhedc.
    it_maint-hours = zthr_pcp00-houry.
    it_maint-count = '1'.
    COLLECT it_maint. CLEAR it_maint.
  ENDSELECT.

*... pay increase ratio
**  IF SY-DATUM+4(2) >= 1 AND SY-DATUM+4(2) <= 6.
*  if W_ZMONS >= 1 and W_ZMONS <= 6.
*    L_ZCODE = '10000'.
*  ELSE.
*    L_ZCODE = '10010'.
*  ENDIF.
  l_zcode = w_zmons.

  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 zval2
    INTO (zthr_pcp02-zval1, zthr_pcp02-zval2)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1060'
                      AND zcode = l_zcode.
*
  LOOP AT it_maint.
    CASE w_zgrup.
      WHEN '1040'.
        it_maint-zval1 = zthr_pcp02-zval1.
      WHEN '1050'.
        it_maint-zval1 = zthr_pcp02-zval2.
    ENDCASE.
    it_maint-hours = it_maint-hours / it_maint-count.

    CLEAR hrp1000.
    SELECT SINGLE short INTO hrp1000-short
      FROM hrp1000 WHERE plvar = '01'
                     AND otype = 'C'
                     AND objid = it_maint-zobjc
                     AND istat = '1'
                     AND endda = '99991231'
                     AND langu = sy-langu.
    it_maint-zobjt = hrp1000-short.
    MODIFY it_maint. CLEAR it_maint.
  ENDLOOP.

ENDFORM.                    " SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_HOURLY_SALARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_hourly_salary.
  DATA : w_zsaly LIKE zthr_pcpxx-zsaly,
         w_zhouy LIKE zthr_pcpxx-zhouy.

  CLEAR it_pcpxx. REFRESH it_pcpxx.
  SORT it_maint BY zobjc.
*
  LOOP AT it_maint.
    CLEAR : w_zsaly , w_zhouy.
    CLEAR zthr_pcpxx.
    SELECT pernr  zsaly zhouy
             INTO (zthr_pcpxx-pernr, zthr_pcpxx-zsaly, zthr_pcpxx-zhouy)
      FROM zthr_pcpxx WHERE zyear = w_zyear
                        AND zvers = w_zvers
                        AND zcost = w_kostl
                        AND zobjc = it_maint-zobjc
                        AND zsenr = it_maint-zsenr.
      it_pcpxx-pernr = zthr_pcpxx-pernr.
      it_pcpxx-zobjc = it_maint-zobjc.
      it_pcpxx-zsenr = it_maint-zsenr.
      it_pcpxx-zsaly = zthr_pcpxx-zsaly.
      it_pcpxx-zhouy = zthr_pcpxx-zhouy.

      w_zsaly        = w_zsaly + it_pcpxx-zsaly.
      w_zhouy        = w_zhouy + it_pcpxx-zhouy.

      APPEND it_pcpxx. CLEAR it_pcpxx.
    ENDSELECT.
*     Salary/hourly = Monthly salary / Monthly working hours (173.33 h)
*      IT_MAINT-HOURS = W_ZSALY / ( 17333 / 100 ) .
    it_maint-hours = w_zhouy / it_maint-zhedc.
    MODIFY it_maint.
  ENDLOOP.

ENDFORM.                    " GET_HOURLY_SALARY
*&---------------------------------------------------------------------*
*&      Form  COPY_DATA_BY_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM copy_data_by_job.
  DATA: l_count     LIKE zthr_pcp05-zseqn,
        l_forms(10),
        l_zhedc     LIKE zthr_pcp00-zhedc,
        l_rate    TYPE p DECIMALS 3.
*
  CASE w_zgrup.
    WHEN '1040'. l_count = 1.
    WHEN '1050'. l_count = 0.
  ENDCASE.
*
  CLEAR it_copyt. REFRESH it_copyt.
*
  DO 5 TIMES.
    l_count = l_count + 1.
    IF l_count > 5. EXIT. ENDIF.
    CLEAR zthr_pcp05.
*    SELECT SINGLE ZWKTM INTO ZTHR_PCP05-ZWKTM
    SELECT SINGLE zhedc ztotm zextr
           INTO (zthr_pcp05-zhedc, zthr_pcp05-ztotm,
                 zthr_pcp05-zextr)
     FROM zthr_pcp05 WHERE zscst = w_kostl
                       AND zyear = w_zyear
                       AND zmons = w_zmons
                       AND zvers = w_zvers
                       AND zgrup = w_zgrup
                       AND zseqn = l_count.
    CASE l_count.
      WHEN 1. l_forms = 'Weekday'.
      WHEN 2. l_forms = 'Overtime'.
      WHEN 3. l_forms = 'Saturday'.
      WHEN 4. l_forms = 'Sunday'.
      WHEN 5. l_forms = 'Holiday'.
    ENDCASE.

    CLEAR l_zhedc.
    SORT it_maint BY zobjc.
    LOOP AT it_maint.
* insert by jslee 05/19/2004
      CLEAR l_rate .
      IF it_copyt-zval1 < 1.
        l_rate = 1 + it_maint-zval1.
      ELSE.
        l_rate = it_maint-zval1.
      ENDIF.

      it_copyt-zseqn = l_count.
      it_copyt-forms = l_forms.
      it_copyt-zobjc = it_maint-zobjc.
      it_copyt-zobjt = it_maint-zobjt.
      it_copyt-zsenr = it_maint-zsenr.
      it_copyt-zhedc = it_maint-zhedc.
      it_copyt-zextr = zthr_pcp05-zextr.
*      IT_COPYT-ZWKTM = ZTHR_PCP05-ZWKTM.
*  WORK HOURS =  Monthly working hrs / Head Count *
*                        H.C by Seniority
      it_copyt-zwktm = ( zthr_pcp05-ztotm / zthr_pcp05-zhedc ) *
                        it_copyt-zhedc .
      it_copyt-zval1 = it_maint-zval1.
      it_copyt-hours = it_maint-hours.
      l_zhedc = l_zhedc + it_maint-zhedc.
*     Working hours  * HOURS PAY * RATE( 0.4 OR 0.5 )
*      it_copyt-amunt = it_copyt-hours * l_rate * it_maint-zhedc
*                      * it_copyt-zwktm  * zthr_pcp05-zextr.
      it_copyt-amunt = it_copyt-hours * l_rate  "* it_maint-zhedc
                   * it_copyt-zwktm  * zthr_pcp05-zextr.
      AT END OF zobjc.
        it_copyt-zhedt = l_zhedc.
        CLEAR l_zhedc.
      ENDAT.
      APPEND it_copyt. CLEAR it_copyt.
    ENDLOOP.
  ENDDO.
*
  SORT it_copyt BY zseqn zobjc.
* sort .

  PERFORM data_sort.

  REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
  DESCRIBE TABLE it_copyt LINES tc9000-lines.

ENDFORM.                    " COPY_DATA_BY_JOB
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWN_LOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_down_load.

  DATA : wa_filename    LIKE rlgrap-filename .

  PERFORM select_main_data_excel.
  PERFORM get_hourly_salary_excel.
  PERFORM copy_data_by_job_excel.

  CLEAR : it_down , it_down[].
  LOOP AT it_copyt1 .
    MOVE-CORRESPONDING it_copyt1 TO it_down.
    IF w_zgrup = '1050'.
      it_down-zgrup = 'Work Type Production Job'.
    ELSE.
      it_down-zgrup = 'Work Type Office Job'.
    ENDIF.
    APPEND it_down. CLEAR it_down.
  ENDLOOP.

  SORT it_down BY zgrup zyear zmons zcost forms zobjt .

  it_down-zgrup = 'Work Type'.
  it_down-zyear = 'Year'.
  it_down-zmons = 'Month'.
  it_down-zcost = 'Cost Center'.
  it_down-ktext = 'Cost Center name'.
  it_down-forms = 'Form '.
*     it_down-ZSEQN(40),
*     it_down-ZOBJC(40),
  it_down-zobjt = 'Job'.
  it_down-zhedt = 'Head Count'.
  it_down-zsenr = 'Seniority'.
  it_down-zhedc = 'H.C by Seniority'.
  it_down-hours = 'Salary/Hourly'.
  it_down-zwktm = 'Works Hours'.
  it_down-zval1 = 'Pay increase'.
  it_down-amunt = 'Amount'.

  INSERT  it_down INDEX 1.

  IF wa_filename IS INITIAL.
    SET PARAMETER ID 'GR8' FIELD wa_filename.
    IF sy-subrc NE 0.CLEAR  wa_filename.ENDIF.
  ENDIF.


  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = wa_filename
            def_path         = wa_filename
            mask             = ',*.xls.'
            mode             = 'S'
            title            = sy-title
       IMPORTING
            filename         = wa_filename
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.

  CHECK sy-subrc EQ 0 .

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename = wa_filename
            filetype = 'DAT'
       TABLES
            data_tab = it_down.

ENDFORM.                    " EXCEL_DOWN_LOAD
*&---------------------------------------------------------------------*
*&      Form  SELECT_MAIN_DATA_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_main_data_excel.
  DATA: l_zcode LIKE zthr_pcp02-zcode.

  CLEAR r_zjobc. REFRESH r_zjobc.
  CLEAR zthr_pcp02.
  SELECT zval1 zval4 INTO (zthr_pcp02-zval1, zthr_pcp02-zval4)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = w_zgrup.
    CONDENSE zthr_pcp02-zval4.
    IF w_zgrup = '1040' .
      IF zthr_pcp02-zval4 = 'OT'.
        r_zjobc-sign = 'I'.
        r_zjobc-option = 'EQ'.
        r_zjobc-low = zthr_pcp02-zval1.
        APPEND r_zjobc. CLEAR r_zjobc.
      ENDIF.
    ELSE.
      r_zjobc-sign = 'I'.
      r_zjobc-option = 'EQ'.
      r_zjobc-low = zthr_pcp02-zval1.
      APPEND r_zjobc. CLEAR r_zjobc.
    ENDIF.
    CLEAR zthr_pcp02.
  ENDSELECT.
*
  IF r_zjobc[] IS INITIAL.
    MESSAGE e009(zmhr) WITH 'Not Defind Work Type Production Job'.
  ENDIF.

  CLEAR it_maint1. REFRESH it_maint1.
*
  CLEAR zthr_pcp00.
* select by
  SELECT zobjc zsenr zhedc ancur zperg zsubg
         act03 act06 act07 zyear zmons zcost
    INTO (zthr_pcp00-zobjc, zthr_pcp00-zsenr,
          zthr_pcp00-zhedc, zthr_pcp00-ancur,
          zthr_pcp00-zperg, zthr_pcp00-zsubg,
          zthr_pcp00-act03, zthr_pcp00-act06,
          zthr_pcp00-act07, zthr_pcp00-zyear,
          zthr_pcp00-zmons, zthr_pcp00-zcost)

    FROM zthr_pcp00 WHERE zyear = w_zyear
                      AND zvers = w_zvers
                      AND zobjc IN r_zjobc
                      AND NOT ( zperg  = '9' AND zsubg = 'U2' ) .

    it_maint1-zobjc = zthr_pcp00-zobjc.
    it_maint1-zsenr = zthr_pcp00-zsenr.
    it_maint1-zhedc = zthr_pcp00-zhedc.
    it_maint1-zyear = zthr_pcp00-zyear.
    it_maint1-zmons = zthr_pcp00-zmons.
    it_maint1-zcost = zthr_pcp00-zcost.

    COLLECT it_maint1. CLEAR it_maint1.
  ENDSELECT.

**... pay increase ration
*  IF SY-DATUM+4(2) >= 1 AND SY-DATUM+4(2) <= 6.
*    L_ZCODE = '10000'.
*  ELSE.
*    L_ZCODE = '10010'.
*  ENDIF.
**
*  CLEAR ZTHR_PCP02.
*  SELECT SINGLE ZVAL1 INTO ZTHR_PCP02-ZVAL1
*    FROM ZTHR_PCP02 WHERE ZMODL = '02'
*                      AND ZGRUP = '1060'
*                      AND ZCODE = L_ZCODE.
*
  LOOP AT it_maint1.
    IF it_maint1-zmons >= 1 AND it_maint1-zmons <= 6.
      l_zcode = '10000'.
    ELSE.
      l_zcode = '10010'.
    ENDIF.
*
    CLEAR zthr_pcp02.
    SELECT SINGLE zval1 INTO zthr_pcp02-zval1
      FROM zthr_pcp02 WHERE zmodl = '02'
                        AND zgrup = '1060'
                        AND zcode = l_zcode.

    it_maint1-zval1 = zthr_pcp02-zval1.
    SELECT SINGLE ktext INTO it_maint1-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_maint1-zcost
                  AND datbi = '99991231'.

    CLEAR hrp1000.
    SELECT SINGLE short INTO hrp1000-short
      FROM hrp1000 WHERE plvar = '01'
                     AND otype = 'C'
                     AND objid = it_maint1-zobjc
                     AND istat = '1'
                     AND endda = '99991231'
                     AND langu = sy-langu.
    it_maint1-zobjt = hrp1000-short.
    MODIFY it_maint1. CLEAR it_maint1.
  ENDLOOP.

ENDFORM.                    " SELECT_MAIN_DATA_EXCEL
*&---------------------------------------------------------------------*
*&      Form  GET_HOURLY_SALARY_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_hourly_salary_excel.
  DATA : w_zsaly LIKE zthr_pcpxx-zsaly,
         w_zhouy LIKE zthr_pcpxx-zhouy.

  CLEAR it_pcpxx. REFRESH it_pcpxx.
  SORT it_maint1 BY zobjc.
*
  LOOP AT it_maint1.
    CLEAR : w_zsaly , w_zhouy.
    CLEAR zthr_pcpxx.
    SELECT pernr  zsaly zhouy
             INTO (zthr_pcpxx-pernr, zthr_pcpxx-zsaly, zthr_pcpxx-zhouy)
      FROM zthr_pcpxx WHERE zyear = w_zyear
                        AND zvers = w_zvers
                        AND zcost = it_maint1-zcost
                        AND zobjc = it_maint1-zobjc
                        AND zsenr = it_maint1-zsenr.
      it_pcpxx-pernr = zthr_pcpxx-pernr.
      it_pcpxx-zobjc = it_maint-zobjc.
      it_pcpxx-zsenr = it_maint-zsenr.
      it_pcpxx-zsaly = zthr_pcpxx-zsaly.
      it_pcpxx-zhouy = zthr_pcpxx-zhouy.
      w_zsaly        = w_zsaly + it_pcpxx-zsaly.
      w_zhouy        = w_zhouy + it_pcpxx-zhouy.
      APPEND it_pcpxx. CLEAR it_pcpxx.
    ENDSELECT.
*     Salary/hourly = Monthly salary / Monthly working hours (173.33 h)
*      IT_MAINT-HOURS = W_ZSALY / ( 17333 / 100 ) .
    it_maint1-hours = w_zhouy.
    MODIFY it_maint1.
  ENDLOOP.

ENDFORM.                    " GET_HOURLY_SALARY_EXCEL
*&---------------------------------------------------------------------*
*&      Form  COPY_DATA_BY_JOB_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM copy_data_by_job_excel.
  DATA: l_count     LIKE zthr_pcp05-zseqn,
        l_forms(10),
        l_zhedc     LIKE zthr_pcp00-zhedc.
*
  CASE w_zgrup.
    WHEN '1040'. l_count = 1.
    WHEN '1050'. l_count = 0.
  ENDCASE.
*
  CLEAR it_copyt1. REFRESH it_copyt1.
*
  DO 5 TIMES.
    l_count = l_count + 1.
    IF l_count > 5. EXIT. ENDIF.
    CLEAR zthr_pcp05.

    SELECT SINGLE zhedc ztotm
           INTO (zthr_pcp05-zhedc, zthr_pcp05-ztotm)
     FROM zthr_pcp05 WHERE zscst = w_kostl
                       AND zyear = w_zyear
                       AND zmons = w_zmons
                       AND zvers = w_zvers
                       AND zgrup = w_zgrup
                       AND zseqn = l_count.
    CASE l_count.
      WHEN 1. l_forms = 'Weekday'.
      WHEN 2. l_forms = 'Overtime'.
      WHEN 3. l_forms = 'Saturday'.
      WHEN 4. l_forms = 'Sunday'.
      WHEN 5. l_forms = 'Holiday'.
    ENDCASE.

    CLEAR l_zhedc.
    SORT it_maint BY zobjc.
    LOOP AT it_maint1.
      it_copyt1-zseqn = l_count.
      it_copyt1-forms = l_forms.
      it_copyt1-zcost = it_maint1-zcost.
      it_copyt1-ktext = it_maint1-ktext.
      it_copyt1-zyear = it_maint1-zyear.
      it_copyt1-zmons = it_maint1-zmons.
      it_copyt1-zobjc = it_maint1-zobjc.
      it_copyt1-zobjt = it_maint1-zobjt.
      it_copyt1-zsenr = it_maint1-zsenr.
      it_copyt1-zhedc = it_maint1-zhedc.
*      IT_COPYT-ZWKTM = ZTHR_PCP05-ZWKTM.
*  WORK HOURS =  Monthly working hrs / Head Count *
*                        H.C by Seniority
      it_copyt1-zwktm = ( zthr_pcp05-ztotm / zthr_pcp05-zhedc ) *
                        it_copyt-zhedc .
      it_copyt1-zval1 = it_maint1-zval1.
      it_copyt1-hours = it_maint1-hours.
      l_zhedc = l_zhedc + it_maint1-zhedc.
*     Working hours  * HOURS PAY * RATE( 0.4 OR 0.5 )
      it_copyt1-amunt =
             it_copyt1-hours * it_copyt1-zval1 * it_copyt1-zwktm.
      AT END OF zobjc.
        it_copyt1-zhedt = l_zhedc.
        CLEAR l_zhedc.
      ENDAT.
      APPEND it_copyt1. CLEAR it_copyt1.
    ENDLOOP.
  ENDDO.
ENDFORM.                    " COPY_DATA_BY_JOB_EXCEL
*&---------------------------------------------------------------------*
*&      Form  data_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_sort.
  DATA : BEGIN OF it_sort OCCURS 0,
         val1 LIKE zthr_pcp02-zval1,
         val2 LIKE zthr_pcp02-zval2,
         END OF it_sort.

  CLEAR : it_sort, it_sort[].
  SELECT zval1 zval2 INTO (it_sort-val1, it_sort-val2)
   FROM zthr_pcp02
   WHERE zmodl = '02'
     AND zgrup = '1240' .
    APPEND it_sort.
  ENDSELECT.

  it_sort_t[] = it_copyt[].
  LOOP AT  it_sort_t.
    READ TABLE it_sort WITH KEY val2 = it_sort_t-zobjc.
    IF sy-subrc EQ 0 .
      MOVE it_sort-val1 TO it_sort_t-xx.
      MODIFY it_sort_t.
    ENDIF.

  ENDLOOP.

  SORT it_sort_t BY zseqn xx zsenr.

  it_copyt[] = it_sort_t[] .

ENDFORM.                    " data_sort
