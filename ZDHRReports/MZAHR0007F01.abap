*----------------------------------------------------------------------*
*   INCLUDE MZAHR0007F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN_9100
*&---------------------------------------------------------------------*
FORM init_screen_9100.
  CLEAR it_ahc01. REFRESH it_ahc01.
*
  CLEAR it_perda.
  CALL FUNCTION 'HR_GET_EMPLOYEE_DATA'
       EXPORTING
            person_id             = w_pernr
            selection_begin       = sy-datum
            selection_end         = sy-datum
       IMPORTING
            personal_data         = it_perda
       EXCEPTIONS
            person_not_found      = 1
            no_active_integration = 2
            OTHERS                = 3.
*
  w_werks = it_perda-werks.
  CLEAR t500p.
  SELECT SINGLE name1 INTO t500p-name1
    FROM t500p WHERE persa = w_werks.
  w_name1 = t500p-name1.
  w_kostl = it_perda-kostl.
  CLEAR cskt.
  SELECT SINGLE ktext INTO cskt-ktext
    FROM cskt WHERE spras = sy-langu
                AND kostl = w_kostl
                AND datbi = '99991231'.
  w_ktext = cskt-ktext.
*
  w_flags = space.
  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE it_ahc01 LINES tc9100-lines.
  CALL SCREEN 9100.
ENDFORM.                    " INIT_SCREEN_9100
*&---------------------------------------------------------------------*
*&      Form  SELECT_HEAD_COUNT_DATA
*&---------------------------------------------------------------------*
FORM select_head_count_data.
  CLEAR it_ahc01. REFRESH it_ahc01.
*
  IF w_werks = space OR w_zyear = space OR w_zmons = space OR
     w_zvers = space.
    MESSAGE w001 WITH 'Please make a selection'.
  ENDIF.

  IF w_kostl = space.
    MESSAGE w001 WITH text-010.
    EXIT.
  ENDIF.

  CLEAR zthr_ahc01.
  SELECT zvers zyear zmons zpera zcost zscst
         zjobk zperg zsubg zsenr zhedc znewc zjobc
         erdat erzet ernam aedat aezet aenam
    INTO CORRESPONDING FIELDS OF TABLE it_ahc01
    FROM zthr_ahc01 WHERE zvers = w_zvers
                      AND zyear = w_zyear
                      AND zmons = w_zmons
                      AND zpera = w_werks
                      AND zcost = w_kostl.
  IF sy-subrc = 0.
    it_ahc01-stats = 'U'.
    MODIFY it_ahc01 TRANSPORTING stats WHERE zvers NE space.
  ELSE.
    PERFORM get_data_from_function.
  ENDIF.
*
  LOOP AT it_ahc01.
    CLEAR cskt.
    SELECT SINGLE ktext INTO cskt-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_ahc01-zscst
                  AND datbi = '99991231'.
    it_ahc01-ktext = cskt-ktext.

    CLEAR hrp1000.
    SELECT SINGLE short INTO hrp1000-short
      FROM hrp1000 WHERE plvar = '01'
                     AND otype = 'C'
                     AND objid = it_ahc01-zjobc
                     AND istat = '1'
                     AND endda = '99991231'
                     AND langu = sy-langu.
    IF sy-subrc = 0.
      it_ahc01-zjobk = hrp1000-short.
    ENDIF.
    MODIFY it_ahc01. CLEAR it_ahc01.
  ENDLOOP.
*  SORT IT_AHC01 BY ZSCST ZJOBK .
  PERFORM sort_table.
*
  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE it_ahc01 LINES tc9100-lines.
  w_flags = 'X'.
ENDFORM.                    " SELECT_HEAD_COUNT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_FUNCTION
*&---------------------------------------------------------------------*
FORM get_data_from_function.
  TABLES : pa0041.
  DATA: l_begda   LIKE sy-datum,
        l_years   TYPE i,
        l_entry   LIKE hida OCCURS 1 WITH HEADER LINE.

  DATA :  l_begin LIKE sy-datum,
          l_end   LIKE sy-datum.

  DATA : l_code LIKE p0041-dar01.
  DATA : l_date LIKE p0041-dat01.
*... get sub cost center
  CLEAR: it_units, it_persn, it_orgpn.
  REFRESH: it_units, it_persn, it_orgpn.
*
  CALL FUNCTION 'RH_DIR_ORG_STRUC_GET'
       EXPORTING
            act_orgunit     = w_orgeh
            act_plvar       = '01'
            act_date        = sy-datum
            sort_flag       = 'X'
            add_flag_pdata  = 'X'
       TABLES
            org_units       = it_units
            person_tab      = it_persn
            org_pers_rel    = it_orgpn
       EXCEPTIONS
            no_active_plvar = 1
            OTHERS          = 2.
*
* other cost center
  LOOP AT it_cost .

    CLEAR it_orgpn.
    SELECT  pernr stell
         INTO (it_orgpn-objid , it_orgpn-jobcode)
           FROM pa0001
           WHERE kostl = it_cost-zcost
             AND endda = '99991231'.
      IF sy-subrc EQ 0.
        APPEND it_orgpn.
      ENDIF.
    ENDSELECT.
  ENDLOOP.
*
  CLEAR zthr_pcp00.
  SELECT SINGLE erdat INTO zthr_pcp00-erdat
    FROM zthr_pcp00 WHERE zyear = w_zyear
                      AND zmons = w_zmons
                      AND zvers = w_zvers
                      AND zcost = w_kostl.
*
  LOOP AT it_orgpn.                 " WHERE ORGID <> W_ORGEH.
    it_ahc01-zvers = w_zvers.
    it_ahc01-zyear = w_zyear.
    it_ahc01-zmons = w_zmons.
    CLEAR pa0001.
    SELECT SINGLE pernr werks persg persk kostl
      INTO (pa0001-pernr, pa0001-werks, pa0001-persg,
            pa0001-persk, pa0001-kostl)
      FROM pa0001 WHERE pernr = it_orgpn-objid
                    AND endda = '99991231'
                    AND begda <= zthr_pcp00-erdat
                    AND werks = w_werks.

    SELECT SINGLE * FROM pa0000
                    WHERE pernr = pa0001-pernr
                      AND massn IN ('Z5','Z7','Z8','ZE','ZH','ZI',
                                    'ZW','ZX','ZY').
    CHECK sy-subrc NE 0.

    IF pa0001-persg <> '2'.
      it_ahc01-zpera = pa0001-werks.
      it_ahc01-zcost = w_kostl.
      it_ahc01-zscst = pa0001-kostl.
      it_ahc01-zjobc = it_orgpn-jobcode.
      it_ahc01-zperg = pa0001-persg.
      it_ahc01-zsubg = pa0001-persk.
    ELSE.
      CLEAR it_ahc01. CONTINUE.
    ENDIF.

    CLEAR: l_entry, l_entry[], l_begda.
    CALL FUNCTION 'HR_ENTRY_DATE'
         EXPORTING
              persnr               = pa0001-pernr
         IMPORTING
              entrydate            = l_begda
         TABLES
              entry_dates          = l_entry
         EXCEPTIONS
              entry_date_not_found = 1
              pernr_not_assigned   = 2
              OTHERS               = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CONCATENATE w_zyear '1231' INTO l_end.
    CLEAR l_begin .
*Original Hire Date
    SELECT SINGLE *
                  FROM pa0041
                  WHERE pernr = pa0001-pernr
                    AND endda = '99991231' .

    DO 12 TIMES VARYING l_code FROM pa0041-dar01 NEXT pa0041-dar02
                VARYING l_date FROM pa0041-dat01 NEXT pa0041-dat02 .
      IF l_code = 'Z1'.
        l_begin = l_date.
        EXIT.
      ENDIF.

    ENDDO.
    IF l_begin IS INITIAL.
      l_begin = l_begda
   .
    ENDIF.

* Defferience day mont year
    CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
         EXPORTING
              beg_da        = l_begin
              end_da        = l_end
         IMPORTING
              no_year       = l_years
         EXCEPTIONS
              dateint_error = 1
              OTHERS        = 2.
    .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF l_years = 0. l_years = 1. ENDIF.

    it_ahc01-zsenr = l_years.
    it_ahc01-zhedc = 1.
    COLLECT it_ahc01. CLEAR it_ahc01.
  ENDLOOP.
*
  it_ahc01-erdat = sy-datum.
  it_ahc01-erzet = sy-uzeit.
  it_ahc01-ernam = sy-uname.
  it_ahc01-stats = 'C'.
  MODIFY it_ahc01 TRANSPORTING erdat erzet ernam WHERE ernam = space.
  SORT it_ahc01 BY zscst zperg.
ENDFORM.                    " GET_DATA_FROM_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE_ACH01
*&---------------------------------------------------------------------*
FORM update_table_ach01.
*... check close day
  IF w_close <= sy-datum.
    MESSAGE w001 WITH 'Closed Year. Please inform administrator.'.
    EXIT.
  ENDIF.

  IF w_kostl = space.
    MESSAGE w001 WITH text-010.
    EXIT.
  ENDIF.

*  MODIFY zthr_ahc01 FROM TABLE it_ahc01.
  DATA : z_check LIKE it_ahc01-zhedc .

  LOOP AT it_ahc01.
    z_check = it_ahc01-zhedc + it_ahc01-znewc.
    IF z_check =< 0.
      it_ahc01-znewc = it_ahc01-zhedc * -1.
    ENDIF.
    MODIFY it_ahc01 INDEX sy-tabix.
    CLEAR z_check.
  ENDLOOP.
  MODIFY zthr_ahc01 FROM TABLE it_ahc01.
  IF sy-subrc = 0.
    w_flags = space.
    PERFORM update_basic_data.
    PERFORM update_zthr_pcp05.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s001 WITH 'Error during processing of Period Save'.
  ENDIF.

ENDFORM.                    " UPDATE_TABLE_ACH01
*&---------------------------------------------------------------------*
*&      Form  COPY_MONTH
*&---------------------------------------------------------------------*
FORM copy_month.
  IF w_cmons = space.
    MESSAGE w001 WITH 'CHOOSE THE MONTH'.
    EXIT.
  ENDIF.

  w_ptext = '10% Copy processing.....'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 10
            text       = w_ptext.

  w_ptext = '50% Copy processing.....'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 50
            text       = w_ptext.

  CLEAR it_copyt. REFRESH it_copyt.
  it_copyt[] = it_ahc01[].
  it_copyt-zmons = w_cmons.
  it_copyt-erdat = sy-datum.
  it_copyt-erzet = sy-uzeit.
  it_copyt-ernam = sy-uname.
  it_copyt-aedat = '00000000'.
  it_copyt-aezet = space.
  it_copyt-aenam = space.
  it_copyt-stats = 'C'.
  MODIFY it_copyt TRANSPORTING zmons erdat erzet ernam
                               aedat aezet aenam stats
                  WHERE zvers NE space.
*
  w_ptext = '95% Copy processing.....'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 95
            text       = w_ptext.
*
  CLEAR it_ahc01. REFRESH it_ahc01.
  it_ahc01[] = it_copyt[].

  MESSAGE s001 WITH 'Transaction was processed successfully'.
ENDFORM.                    " COPY_MONTH
*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN_9200
*&---------------------------------------------------------------------*
FORM init_screen_9200.
  CLEAR it_ahc01. REFRESH it_ahc01.
  CLEAR: w_werks, w_name1, w_orgeh, w_kostl, w_ktext,
         w_zyear, w_zvers, w_zmons, w_cmons, w_zscst,
         w_kname, w_zgrup, w_zgtxt.
*
  CALL FUNCTION 'HR_GET_EMPLOYEE_DATA'
       EXPORTING
            person_id             = w_pernr
            selection_begin       = sy-datum
            selection_end         = sy-datum
       IMPORTING
            personal_data         = it_perda
       EXCEPTIONS
            person_not_found      = 1
            no_active_integration = 2
            OTHERS                = 3.
*
  w_werks = it_perda-werks.
  CLEAR t500p.
  SELECT SINGLE name1 INTO t500p-name1
    FROM t500p WHERE persa = w_werks.
  w_name1 = t500p-name1.
  w_orgeh = it_perda-orgeh.
  w_kostl = it_perda-kostl.
  CLEAR cskt.
  SELECT SINGLE ktext INTO cskt-ktext
    FROM cskt WHERE spras = sy-langu
                AND kostl = w_kostl
                AND datbi = '99991231'.
  w_ktext = cskt-ktext.
*... get sub sost center
  PERFORM get_sub_cost_center.
  CALL SCREEN 9200.
ENDFORM.                    " INIT_SCREEN_9200
*&---------------------------------------------------------------------*
*&      Form  GET_SUB_COST_CENTER
*&---------------------------------------------------------------------*
FORM get_sub_cost_center.
  CLEAR: it_units, it_persn, it_orgpn.
  REFRESH: it_units, it_persn, it_orgpn.
*
  CALL FUNCTION 'RH_DIR_ORG_STRUC_GET'
       EXPORTING
            act_orgunit     = w_orgeh
            act_plvar       = '01'
            act_date        = sy-datum
            sort_flag       = 'X'
            add_flag_pdata  = 'X'
       TABLES
            org_units       = it_units
            person_tab      = it_persn
            org_pers_rel    = it_orgpn
       EXCEPTIONS
            no_active_plvar = 1
            OTHERS          = 2.
ENDFORM.                    " GET_SUB_COST_CENTER
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_FROM_PCP05
*&---------------------------------------------------------------------*
FORM select_data_from_pcp05.
  DATA : w_int TYPE i,
         z_zval2 LIKE zthr_pcp02-zval2,
         c_pernr LIKE w_pernr.
  CLEAR : it_pcp05,w_int,z_zval2. REFRESH it_pcp05.
* check condition
  IF w_zscst = space OR w_zyear = space OR w_zmons = space OR
     w_zvers = space OR w_zgrup = space.
    MESSAGE w001 WITH 'Please make a selection'.
  ENDIF.
  IF w_kostl = space.
    MESSAGE w001 WITH text-010.
    EXIT.
  ENDIF.
  DESCRIBE TABLE it_zscst LINES w_int.
  IF w_int = 0.
    MESSAGE w001 WITH text-012.
    EXIT.
  ENDIF.

  REFRESH dynpfields. CLEAR dynpfields.
  dynpfields-fieldname = 'W_PERNR'.
*  dynpfields-stepl = sy-stepl.
  APPEND dynpfields.

  PERFORM dynp_values_read TABLES dynpfields.

  READ TABLE dynpfields WITH KEY fieldname = 'W_PERNR'.
  MOVE dynpfields-fieldvalue TO w_pernr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = w_pernr
       IMPORTING
            output = c_pernr.

  SELECT  zval2  INTO  z_zval2
         FROM  zthr_pcp02  UP TO 1  ROWS
          WHERE  zmodl EQ '02'
            AND  zgrup EQ '1005'
            AND  zval1 EQ c_pernr.
  ENDSELECT.
  IF z_zval2 <> space.
    LOOP AT it_zscst.
      SELECT SINGLE *  FROM zthr_pcp02
         WHERE zmodl EQ '02'
           AND ( zgrup EQ '1260' or zgrup EQ '1270' )
           AND zctxt EQ it_zscst-zscst.
      CHECK sy-subrc <> 0.
      MESSAGE w001 WITH text-011.
      EXIT.
    ENDLOOP.
  ELSE.
    LOOP AT it_zscst.
      SELECT SINGLE *  FROM zthr_pcp02
         WHERE zmodl EQ '02'
           AND ( zgrup EQ '1260' or  zgrup EQ '1270' )
           AND zctxt EQ it_zscst-zscst.
      CHECK sy-subrc = 0.
      MESSAGE w001 WITH text-011.
      EXIT.
    ENDLOOP.

    REFRESH dynpfields. CLEAR dynpfields.
    dynpfields-fieldname = 'W_ZSCST'.
*  dynpfields-stepl = sy-stepl.
    APPEND dynpfields.

    PERFORM dynp_values_read TABLES dynpfields.

    READ TABLE dynpfields WITH KEY fieldname = 'W_ZSCST'.

    SELECT SINGLE *  FROM zthr_pcp02
         WHERE zmodl EQ '02'
           AND zgrup EQ '1260'
           AND zctxt EQ dynpfields-fieldvalue.
    IF sy-subrc = 0.
      MESSAGE w001 WITH text-011.
      EXIT.
    ENDIF.
  ENDIF.

  CLEAR zthr_pcp05.
  SELECT zcost zscst zyear zmons zvers zgrup zseqn
         zplnd zreld zhedc zprat zwktm ztotm zextr
         erdat erzet ernam aedat aezet aenam
    INTO CORRESPONDING FIELDS OF TABLE it_pcp05
    FROM zthr_pcp05 WHERE zcost = w_kostl
                      AND zscst = w_zscst
                      AND zyear = w_zyear
                      AND zmons = w_zmons
                      AND zvers = w_zvers
                      AND zgrup = w_zgrup.
  IF sy-subrc = 0.
    it_pcp05-stats = 'M'.
    MODIFY it_pcp05 TRANSPORTING stats WHERE stats = space.
  ELSE.
    IF z_zval2 <> space.
      PERFORM select_data_new_cost USING w_zmons.
      PERFORM modify_selected_data.
    ELSE.
      PERFORM select_data_from_other_table USING w_zmons.
      PERFORM modify_selected_data.
    ENDIF.
  ENDIF.
  SORT it_pcp05 BY zseqn .
ENDFORM.                    " SELECT_DATA_FROM_PCP05
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_FROM_OTHER_TABLE
*&---------------------------------------------------------------------*
FORM select_data_from_other_table USING p_month.
  DATA: l_seqno LIKE zthr_pcp05-zseqn,
        l_headc LIKE zthr_ahc01-zhedc.
  DATA : lt_pcp00 LIKE zthr_pcp00 OCCURS 0 WITH HEADER LINE.
  REFRESH lt_pcp00.

  l_seqno = 1.
*
  it_pcp05-zseqn = l_seqno.
  CLEAR w_zdays.

  CASE p_month.
    WHEN '01'. read_pcp04 day01.
    WHEN '02'. read_pcp04 day02.
    WHEN '03'. read_pcp04 day03.
    WHEN '04'. read_pcp04 day04.
    WHEN '05'. read_pcp04 day05.
    WHEN '06'. read_pcp04 day06.
    WHEN '07'. read_pcp04 day07.
    WHEN '08'. read_pcp04 day08.
    WHEN '09'. read_pcp04 day09.
    WHEN '10'. read_pcp04 day10.
    WHEN '11'. read_pcp04 day11.
    WHEN '12'. read_pcp04 day12.
  ENDCASE.
  it_pcp05-zplnd = w_zdays.
  CLEAR p_flag.
  PERFORM check_closed_month USING p_flag.
  IF p_flag EQ 'S'.
    CLEAR l_headc.
    LOOP AT it_zscst WHERE zscst = w_zscst.
      SELECT * INTO TABLE lt_pcp00
         FROM zthr_pcp00
            WHERE zvers = w_zvers
              AND zyear = w_zyear
              AND zmons = p_month
              AND zcost =  w_zscst
              AND zobjc IN zjobc
              AND NOT ( zperg = '9' AND zsubg = 'U2' ).
      LOOP AT lt_pcp00.
        l_headc = l_headc +  lt_pcp00-zhedc.
      ENDLOOP.
    ENDLOOP.
    IF l_headc EQ space.
      CLEAR zthr_ahc01.
      SELECT zhedc znewc INTO (zthr_ahc01-zhedc, zthr_ahc01-znewc)
        FROM zthr_ahc01 WHERE zvers = w_zvers
                          AND zyear = w_zyear
                          AND zmons = p_month
                          AND zcost = w_kostl
                          AND zscst = w_zscst
                          AND zjobc IN zjobc
                          AND NOT ( zperg = '9' AND zsubg = 'U2' ).
        l_headc = l_headc + zthr_ahc01-zhedc + zthr_ahc01-znewc.
      ENDSELECT.
    ENDIF.
  ELSE.
    l_headc = 0.
  ENDIF.

  it_pcp05-zhedc = l_headc.
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1010'
                      AND zcode = '10000'.
  it_pcp05-zextr = zthr_pcp02-zval1.
  APPEND it_pcp05. CLEAR it_pcp05.
  l_seqno = l_seqno + 1.
*
  it_pcp05-zseqn = l_seqno.
  CLEAR w_zdays.

  CASE p_month.
    WHEN '01'. read_pcp04 day01.
    WHEN '02'. read_pcp04 day02.
    WHEN '03'. read_pcp04 day03.
    WHEN '04'. read_pcp04 day04.
    WHEN '05'. read_pcp04 day05.
    WHEN '06'. read_pcp04 day06.
    WHEN '07'. read_pcp04 day07.
    WHEN '08'. read_pcp04 day08.
    WHEN '09'. read_pcp04 day09.
    WHEN '10'. read_pcp04 day10.
    WHEN '11'. read_pcp04 day11.
    WHEN '12'. read_pcp04 day12.
  ENDCASE.
  it_pcp05-zplnd = w_zdays.
  it_pcp05-zhedc = l_headc.
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1010'
                      AND zcode = '10010'.
  it_pcp05-zextr = zthr_pcp02-zval1.
  APPEND it_pcp05. CLEAR it_pcp05.
  l_seqno = l_seqno + 1.
*
  it_pcp05-zseqn = l_seqno.
  CLEAR w_zdays.

  CASE p_month.
    WHEN '01'. read_pcp04 sat01.
    WHEN '02'. read_pcp04 sat02.
    WHEN '03'. read_pcp04 sat03.
    WHEN '04'. read_pcp04 sat04.
    WHEN '05'. read_pcp04 sat05.
    WHEN '06'. read_pcp04 sat06.
    WHEN '07'. read_pcp04 sat07.
    WHEN '08'. read_pcp04 sat08.
    WHEN '09'. read_pcp04 sat09.
    WHEN '10'. read_pcp04 sat10.
    WHEN '11'. read_pcp04 sat11.
    WHEN '12'. read_pcp04 sat12.
  ENDCASE.
  it_pcp05-zplnd = w_zdays.
  it_pcp05-zhedc = l_headc.
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1010'
                      AND zcode = '10020'.
  it_pcp05-zextr = zthr_pcp02-zval1.
  APPEND it_pcp05. CLEAR it_pcp05.
  l_seqno = l_seqno + 1.
*
  it_pcp05-zseqn = l_seqno.
  CLEAR w_zdays.

  CASE p_month.
    WHEN '01'. read_pcp04 sun01.
    WHEN '02'. read_pcp04 sun02.
    WHEN '03'. read_pcp04 sun03.
    WHEN '04'. read_pcp04 sun04.
    WHEN '05'. read_pcp04 sun05.
    WHEN '06'. read_pcp04 sun06.
    WHEN '07'. read_pcp04 sun07.
    WHEN '08'. read_pcp04 sun08.
    WHEN '09'. read_pcp04 sun09.
    WHEN '10'. read_pcp04 sun10.
    WHEN '11'. read_pcp04 sun11.
    WHEN '12'. read_pcp04 sun12.
  ENDCASE.
  it_pcp05-zplnd = w_zdays.
  it_pcp05-zhedc = l_headc.
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1010'
                      AND zcode = '10030'.
  it_pcp05-zextr = zthr_pcp02-zval1.
  APPEND it_pcp05. CLEAR it_pcp05.
  l_seqno = l_seqno + 1.
*
  it_pcp05-zseqn = l_seqno.
  CLEAR w_zdays.

  CASE p_month.
    WHEN '01'. read_pcp04 hol01.
    WHEN '02'. read_pcp04 hol02.
    WHEN '03'. read_pcp04 hol03.
    WHEN '04'. read_pcp04 hol04.
    WHEN '05'. read_pcp04 hol05.
    WHEN '06'. read_pcp04 hol06.
    WHEN '07'. read_pcp04 hol07.
    WHEN '08'. read_pcp04 hol08.
    WHEN '09'. read_pcp04 hol09.
    WHEN '10'. read_pcp04 hol10.
    WHEN '11'. read_pcp04 hol11.
    WHEN '12'. read_pcp04 hol12.
  ENDCASE.
  it_pcp05-zplnd = w_zdays.
  it_pcp05-zhedc = l_headc.
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1010'
                      AND zcode = '10040'.
  it_pcp05-zextr = zthr_pcp02-zval1.
  APPEND it_pcp05. CLEAR it_pcp05.
ENDFORM.                    " SELECT_DATA_FROM_OTHER_TABLE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SELECTED_DATA
*&---------------------------------------------------------------------*
FORM modify_selected_data.
  it_pcp05-zcost = w_kostl.
  it_pcp05-zscst = w_zscst.
  it_pcp05-zyear = w_zyear.
  it_pcp05-zmons = w_zmons.
  it_pcp05-zvers = w_zvers.
  it_pcp05-zgrup = w_zgrup.
  it_pcp05-erdat = sy-datum.
  it_pcp05-erzet = sy-uzeit.
  it_pcp05-ernam = sy-uname.
  it_pcp05-stats = 'C'.
*
  MODIFY it_pcp05 TRANSPORTING
         zcost zscst zyear zmons zvers zgrup stats erdat erzet ernam
         WHERE stats = space.
ENDFORM.                    " MODIFY_SELECTED_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_MONTH_DATA
*&---------------------------------------------------------------------*
FORM save_month_data.
*... check close day
  IF w_close <= sy-datum.
    MESSAGE w001 WITH 'Closed Year. Please inform administrator.'.
    EXIT.
  ENDIF.
  IF w_kostl = space.
    MESSAGE w001 WITH text-010.
    EXIT.
  ENDIF.

  READ TABLE it_pcp05 WITH KEY zcost = space .
  IF sy-subrc EQ 0.
    MESSAGE w001 WITH 'Do not data selection check Entry !!'.
    EXIT.
  ENDIF.

  DATA: lt_zthr_pcp05 LIKE zthr_pcp05 OCCURS 0 WITH HEADER LINE.

  LOOP AT it_pcp05.
    MOVE-CORRESPONDING it_pcp05 TO lt_zthr_pcp05.
    APPEND lt_zthr_pcp05.
  ENDLOOP.

  MODIFY zthr_pcp05 FROM TABLE lt_zthr_pcp05.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s001 WITH 'Successfully saved'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s001 WITH 'Error during processing of Period Save'.
  ENDIF.
ENDFORM.                    " SAVE_MONTH_DATA
*&---------------------------------------------------------------------*
*&      Form  COPY_MONTH_DATA
*&---------------------------------------------------------------------*
FORM copy_month_data.
  IF w_cmons = space.
    MESSAGE w001 WITH 'CHOOSE THE MONTH'.
    EXIT.
  ENDIF.
*
*  DO 50 TIMES.
  w_ptext = '10% Copy processing.....'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 10
            text       = w_ptext.
*  ENDDO.
*
  CLEAR zthr_pcp05.
  SELECT SINGLE zseqn INTO zthr_pcp05-zseqn
    FROM zthr_pcp05 WHERE zcost = w_kostl
                      AND zscst = w_zscst
                      AND zyear = w_zyear
                      AND zmons = w_cmons
                      AND zvers = w_zvers
                      AND zgrup = w_zgrup.
  IF sy-subrc = 0.
    MESSAGE w001 WITH 'Selected Month data already exist'.
    EXIT.
  ENDIF.
*
*  DO 50 TIMES.
  w_ptext = '50% Copy processing.....'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 50
            text       = w_ptext.
*  ENDDO.
*
  CLEAR it_cpy05. REFRESH it_cpy05.
  it_cpy05[] = it_pcp05[].
*
  CLEAR it_pcp05. REFRESH it_pcp05.
  PERFORM select_data_from_other_table USING w_cmons.
  PERFORM modify_selected_data.
  PERFORM remake_internal_table_pcp05.
*
*  DO 50 TIMES.
  w_ptext = '95% Copy processing.....'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 95
            text       = w_ptext.
*  ENDDO.
*
  MESSAGE s001 WITH 'Transaction was processed successfully'.
ENDFORM.                    " COPY_MONTH_DATA
*&---------------------------------------------------------------------*
*&      Form  REMAKE_INTERNAL_TABLE_PCP05
*&---------------------------------------------------------------------*
FORM remake_internal_table_pcp05.
  LOOP AT it_pcp05.
    CLEAR it_cpy05.
    READ TABLE it_cpy05 WITH KEY zseqn = it_pcp05-zseqn.
    it_pcp05-zmons = w_cmons.
    it_pcp05-zreld = it_cpy05-zreld.
    it_pcp05-zprat = it_cpy05-zprat.
    it_pcp05-zwktm = it_cpy05-zwktm.
    it_pcp05-ztotm = it_pcp05-zreld * it_pcp05-zhedc *
                     ( it_pcp05-zprat / 100 ) * it_pcp05-zwktm.
    MODIFY it_pcp05. CLEAR it_pcp05.
  ENDLOOP.
*
  SORT it_pcp05 BY zseqn.
ENDFORM.                    " REMAKE_INTERNAL_TABLE_PCP05
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTHORITY
*&---------------------------------------------------------------------*
FORM check_authority.
  DATA: l_zval1 LIKE zthr_pcp02-zval1.
*
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = sy-uname
       IMPORTING
            output = w_pernr.
*
  CONCATENATE sy-uname '%' INTO l_zval1.
*... test data (delete after go live)

* 2004/06/01
  CALL FUNCTION 'HR_GET_EMPLOYEE_DATA'
       EXPORTING
            person_id             = w_pernr
            selection_begin       = sy-datum
            selection_end         = sy-datum
       IMPORTING
            personal_data         = it_perda
       EXCEPTIONS
            person_not_found      = 1
            no_active_integration = 2
            OTHERS                = 3.
*
  w_werks = it_perda-werks.
  CLEAR t500p.
  SELECT SINGLE name1 INTO t500p-name1
    FROM t500p WHERE persa = w_werks.
  w_name1 = t500p-name1.
  w_orgeh = it_perda-orgeh.
  w_kostl = it_perda-kostl.
  CLEAR cskt.
  SELECT SINGLE ktext INTO cskt-ktext
    FROM cskt WHERE spras = sy-langu
                AND kostl = w_kostl
                AND datbi = '99991231'.
  w_ktext = cskt-ktext.

  CLEAR zthr_pcp02. CLEAR w_master.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1005'
                      AND zval1 LIKE l_zval1.
  IF sy-subrc = 0.
    w_master = zthr_pcp02-zval1.
    w_close = '99991230'.
    SELECT SINGLE  zval2 zval3 zval4 zval5
     INTO (zthr_pcp02-zval2, zthr_pcp02-zval3,
           zthr_pcp02-zval4, zthr_pcp02-zval5)
      FROM zthr_pcp02 WHERE zmodl = '02'
                        AND zgrup = '1000'
                        AND zval1 LIKE l_zval1.
    IF sy-subrc EQ 0 .
      MOVE : zthr_pcp02-zval2  TO w_orgeh.
      CONDENSE w_orgeh.
      UNPACK w_orgeh TO w_orgeh.

      w_close = zthr_pcp02-zval3.
      CLEAR:it_cost[], it_cost.
      PERFORM cost_center_get USING zthr_pcp02-zval4 .
      PERFORM cost_center_get USING zthr_pcp02-zval5 .

      DELETE it_cost WHERE zcost = '0000000000'.
    ELSE.
      MESSAGE e013.
    ENDIF.

  ELSE.
    SELECT SINGLE  zval2 zval3 zval4 zval5
     INTO (zthr_pcp02-zval2, zthr_pcp02-zval3,
           zthr_pcp02-zval4, zthr_pcp02-zval5)
      FROM zthr_pcp02 WHERE zmodl = '02'
                        AND zgrup = '1000'
                        AND zval1 LIKE l_zval1.
    IF sy-subrc EQ 0 .
      MOVE : zthr_pcp02-zval2  TO w_orgeh.
      CONDENSE w_orgeh.
      UNPACK w_orgeh TO w_orgeh.

      w_close = zthr_pcp02-zval3.
      CLEAR:it_cost[], it_cost.
      PERFORM cost_center_get USING zthr_pcp02-zval4 .
      PERFORM cost_center_get USING zthr_pcp02-zval5 .

      DELETE it_cost WHERE zcost = '0000000000'.
    ELSE.
      MESSAGE e013.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*&      Form  SET_MONTH_STATUS
*&---------------------------------------------------------------------*
FORM set_month_status.
  DATA: l_count(2) TYPE n.
*
  l_count = 0.
*
  WRITE: /(25) 'Month', (10) 'Status'.
  ULINE.
*
  DO 12 TIMES.
    l_count = l_count + 1.
    w_dvalu = l_count.
    CALL FUNCTION 'DOMAIN_VALUE_GET'
         EXPORTING
              i_domname  = 'MONTH'
              i_domvalue = w_dvalu
         IMPORTING
              e_ddtext   = w_dtext.
    WRITE: /(25) w_dtext.

    IF w_zyear = space OR w_zmons = space OR w_zvers = space OR
       w_werks = space.
      WRITE: (10) 'Unknown' COLOR 3 INTENSIFIED OFF.
    ELSE.
      CLEAR zthr_ahc01.
      SELECT SINGLE zsenr INTO zthr_ahc01-zsenr
        FROM zthr_ahc01 WHERE zvers = w_zvers
                          AND zyear = w_zyear
                          AND zmons = l_count
                          AND zpera = w_werks
                          AND zcost = w_kostl.
      IF sy-subrc = 0.
        WRITE: (10) 'Close' COLOR 5 INTENSIFIED OFF.
      ELSE.
        WRITE: (10) 'Open' COLOR 6 INTENSIFIED OFF.
      ENDIF.
    ENDIF.
  ENDDO.
ENDFORM.                    " SET_MONTH_STATUS
*&---------------------------------------------------------------------*
*&      Form  INSERT_NEW_ENTRY
*&---------------------------------------------------------------------*
FORM insert_new_entry.
  DATA : t_line TYPE i.

  it_ahc01-zvers = w_zvers.
  it_ahc01-zyear = w_zyear.
  it_ahc01-zmons = w_zmons.
  it_ahc01-zpera = w_werks.
  it_ahc01-zcost = w_kostl.
  it_ahc01-zscst = space.
  it_ahc01-stats = 'N'.
  it_ahc01-ktext = space.
  it_ahc01-zsenr = 1.
  it_ahc01-erdat = sy-datum.
  it_ahc01-erzet = sy-uzeit.
  it_ahc01-ernam = sy-uname.
  it_ahc01-zhedc = space.
  APPEND it_ahc01. CLEAR it_ahc01.
*
  t_line = tc9100-top_line.

  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE it_ahc01 LINES tc9100-lines.
  tc9100-top_line = t_line + 1.

ENDFORM.                    " INSERT_NEW_ENTRY
*&---------------------------------------------------------------------*
*&      Form  SAVE_YEAR_DATA
*&---------------------------------------------------------------------*
FORM save_year_data.

  DATA: l_count(2) TYPE n,
        l_year_close.
  DATA : l_pernr LIKE zthr_pcp02-zval1.
  DATA : l_datum(10) TYPE c.
  DATA : l_answer .
  CLEAR l_year_close.
*
  l_count = 0.
*
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1 = 'Caution!!!!!'
            textline2 = 'Really Closing year ?'
            titel     = 'Year Confirm'
       IMPORTING
            answer    = l_answer.

  CHECK l_answer = 'J'.

  IF w_zyear = space OR w_zmons = space OR w_zvers = space OR
     w_zscst = space OR w_zgrup = space.
    MESSAGE i001(zmhr) WITH 'No values selected'.
  ELSE.
    DO 12 TIMES.
      l_count = l_count + 1.
      CLEAR zthr_pcp05.
      SELECT SINGLE zseqn INTO zthr_pcp05-zseqn
        FROM zthr_pcp05 WHERE zcost = w_kostl
                          AND zscst = w_zscst
                          AND zyear = w_zyear
                          AND zmons = l_count
                          AND zvers = w_zvers
                          AND zgrup = w_zgrup.
      IF sy-subrc = 0.
*        WRITE: (10) 'Close' COLOR 5 INTENSIFIED OFF.
      ELSE.
        l_year_close = 'X'.
      ENDIF.
    ENDDO.
  ENDIF.

  IF l_year_close = 'X'.
    MESSAGE  i001(zmhr) WITH 'Not yet Colsing month'.
  ELSE.
    PACK w_pernr TO l_pernr.
    CONDENSE l_pernr.
    CONCATENATE '%' l_pernr INTO l_pernr.

    UPDATE zthr_pcp02
      SET zval3 = sy-datum
       WHERE zmodl = '02'
         AND zgrup = '1000'
         AND zval1 LIKE l_pernr.

  ENDIF.
ENDFORM.                    " SAVE_YEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_BASIC_DATA
*&---------------------------------------------------------------------*
FORM update_basic_data.
  DATA : z_check LIKE it_ahc01-zhedc .
  DATA : z_act(14),num(2) TYPE n VALUE 1.
  FIELD-SYMBOLS : <act> TYPE ANY.

  CLEAR it_pcp00. REFRESH it_pcp00.
*
  LOOP AT it_ahc01.
    it_pcp00-zyear = it_ahc01-zyear.
    it_pcp00-zmons = it_ahc01-zmons.
    it_pcp00-zvers = it_ahc01-zvers.
    it_pcp00-zcost = it_ahc01-zscst.
    it_pcp00-zpera = it_ahc01-zpera.
    it_pcp00-zperg = it_ahc01-zperg.
    it_pcp00-zsubg = it_ahc01-zsubg.
    it_pcp00-zobjc = it_ahc01-zjobc.
    it_pcp00-zsenr = it_ahc01-zsenr.

    z_check = it_ahc01-zhedc + it_ahc01-znewc.
    IF z_check =< 0.
      it_pcp00-zhedc = 0.
    ELSE.
      it_pcp00-zhedc = it_ahc01-zhedc + it_ahc01-znewc.
    ENDIF.

    it_pcp00-aedat = sy-datum.
    it_pcp00-aezet = sy-uzeit.
    it_pcp00-aenam = sy-uname.
*    IF IT_AHC01-ZNEWC <> 0.
*      PERFORM CHANG_BASIC_PAY .
*    ENDIF.
    APPEND it_pcp00.

    CLEAR: it_pcp00,z_check.
  ENDLOOP.
*
  LOOP AT it_pcp00.
    CLEAR zthr_pcp00.

    UPDATE zthr_pcp00 SET: zhedc = it_pcp00-zhedc
                           aedat = it_pcp00-aedat
                           aezet = it_pcp00-aezet
                           aenam = it_pcp00-aenam
                     WHERE zyear = it_pcp00-zyear
                       AND zmons = it_pcp00-zmons
                       AND zvers = it_pcp00-zvers
                       AND zcost = it_pcp00-zcost
                       AND zpera = it_pcp00-zpera
                       AND zperg = it_pcp00-zperg
                       AND zsubg = it_pcp00-zsubg
                       AND zobjc = it_pcp00-zobjc
                       AND zsenr = it_pcp00-zsenr.
*    MODIFY zthr_pcp00 FROM it_pcp00.
    IF sy-subrc = 0.
      DELETE it_pcp00. CLEAR it_pcp00.
    ENDIF.

  ENDLOOP.
*New employee
  LOOP AT it_pcp00.
    it_pcp00-erdat = it_pcp00-aedat.
    it_pcp00-erzet = it_pcp00-aezet.
    it_pcp00-ernam = it_pcp00-aenam.
    PERFORM basic_pay_selection.

    CLEAR: it_pcp00-aedat, it_pcp00-aezet, it_pcp00-aenam.
    MODIFY it_pcp00. CLEAR it_pcp00.
  ENDLOOP.
*
  CLEAR zthr_pcp00.
  INSERT zthr_pcp00 FROM TABLE it_pcp00 ACCEPTING DUPLICATE KEYS.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s001 WITH 'DATA SAVE'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s001 WITH 'Error during processing of Period Save'.
  ENDIF.
ENDFORM.                    " UPDATE_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_MONTH_STATUS_XX
*&---------------------------------------------------------------------*
FORM set_month_status_xx.
  DATA: l_count(2) TYPE n.
*
  l_count = 0.
*
  WRITE: /(25) 'Month', (10) 'Status'.
  ULINE.
*
  DO 12 TIMES.
    l_count = l_count + 1.
    w_dvalu = l_count.
    CALL FUNCTION 'DOMAIN_VALUE_GET'
         EXPORTING
              i_domname  = 'MONTH'
              i_domvalue = w_dvalu
         IMPORTING
              e_ddtext   = w_dtext.
    WRITE: /(25) w_dtext.

    IF w_zyear = space OR w_zmons = space OR w_zvers = space OR
       w_zscst = space OR w_zgrup = space.
      WRITE: (10) 'Unknown' COLOR 3 INTENSIFIED OFF.
    ELSE.
      CLEAR zthr_pcp05.
      SELECT SINGLE zseqn INTO zthr_pcp05-zseqn
        FROM zthr_pcp05 WHERE zcost = w_kostl
                          AND zscst = w_zscst
                          AND zyear = w_zyear
                          AND zmons = l_count
                          AND zvers = w_zvers
                          AND zgrup = w_zgrup.
      IF sy-subrc = 0.
        WRITE: (10) 'Close' COLOR 5 INTENSIFIED OFF.
      ELSE.
        WRITE: (10) 'Open' COLOR 6 INTENSIFIED OFF.
      ENDIF.
    ENDIF.
  ENDDO.

ENDFORM.                    " SET_MONTH_STATUS_XX
*&---------------------------------------------------------------------*
*&      Form  GET_READ_COST_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_read_cost_dynpro.

  DATA : BEGIN OF  dynpro_values OCCURS 0.
          INCLUDE STRUCTURE dynpread.
  DATA:  END OF dynpro_values.
  DATA : w_cost TYPE pa0001-kostl.
  DATA : l_zval1 LIKE zthr_pcp02-zval1.
  CLEAR : it_units[], it_units.

  dynpro_values-fieldname = 'W_PERNR'.
  APPEND dynpro_values.  CLEAR dynpro_values.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname             = 'SAPMZAHR0007'
            dynumb             = sy-dynnr
            translate_to_upper = 'X'
       TABLES
            dynpfields         = dynpro_values.

  READ TABLE dynpro_values INDEX 1 .

  MOVE :dynpro_values-fieldvalue  TO  w_pernr  .

*
  CALL FUNCTION 'HR_GET_EMPLOYEE_DATA'
       EXPORTING
            person_id             = w_pernr
            selection_begin       = sy-datum
            selection_end         = sy-datum
       IMPORTING
            personal_data         = it_perda
       EXCEPTIONS
            person_not_found      = 1
            no_active_integration = 2
            OTHERS                = 3.
*
  w_werks = it_perda-werks.
  CLEAR t500p.
  SELECT SINGLE name1 INTO t500p-name1
    FROM t500p WHERE persa = w_werks.
  w_name1 = t500p-name1.
  w_orgeh = it_perda-orgeh.
  w_kostl = it_perda-kostl.
  CLEAR cskt.
  SELECT SINGLE ktext INTO cskt-ktext
    FROM cskt WHERE spras = sy-langu
                AND kostl = w_kostl
                AND datbi = '99991231'.
  w_ktext = cskt-ktext.
  PACK w_pernr TO l_zval1.
  CONDENSE l_zval1.
  SELECT SINGLE  zval2 zval3 zval4 zval5
     INTO (zthr_pcp02-zval2, zthr_pcp02-zval3,
           zthr_pcp02-zval4, zthr_pcp02-zval5)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1000'
                      AND zval1 LIKE l_zval1.
  IF sy-subrc EQ 0 .
    w_close = zthr_pcp02-zval3.
    MOVE : zthr_pcp02-zval2  TO w_orgeh.
    CONDENSE w_orgeh.
    UNPACK w_orgeh TO w_orgeh.
    CLEAR:it_cost[], it_cost.
    PERFORM cost_center_get USING zthr_pcp02-zval4 .
    PERFORM cost_center_get USING zthr_pcp02-zval5 .

    DELETE it_cost WHERE zcost = '0000000000'.
  ENDIF.

*... get sub sost center
  PERFORM get_sub_cost_center.

ENDFORM.                    " GET_READ_COST_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  DATA_DELETE_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_delete_entry.

  LOOP AT it_ahc01 WHERE chkbx = 'X'.
    SELECT SINGLE * FROM zthr_ahc01 WHERE zvers = it_ahc01-zvers
                               AND zyear = it_ahc01-zyear
                               AND zmons = it_ahc01-zmons
                               AND zpera = it_ahc01-zpera
                               AND zcost = it_ahc01-zcost
                               AND zscst = it_ahc01-zscst
                               AND zjobk = it_ahc01-zjobk
                               AND zperg = it_ahc01-zperg
                               AND zsubg = it_ahc01-zsubg
                               AND zsenr = it_ahc01-zsenr.
    IF sy-subrc EQ 0.
      DELETE FROM zthr_ahc01 WHERE zvers = it_ahc01-zvers
                                AND zyear = it_ahc01-zyear
                                AND zmons = it_ahc01-zmons
                                AND zpera = it_ahc01-zpera
                                AND zcost = it_ahc01-zcost
                                AND zscst = it_ahc01-zscst
                                AND zjobk = it_ahc01-zjobk
                                AND zperg = it_ahc01-zperg
                                AND zsubg = it_ahc01-zsubg
                                AND zsenr = it_ahc01-zsenr.

      DELETE FROM zthr_pcp00 WHERE zvers = it_ahc01-zvers
                                AND zyear = it_ahc01-zyear
                                AND zmons = it_ahc01-zmons
                                AND zpera = it_ahc01-zpera
                                AND zcost = it_ahc01-zscst
*                                AND zscst =
                                AND zobjc = it_ahc01-zjobc
                                AND zperg = it_ahc01-zperg
                                AND zsubg = it_ahc01-zsubg
                                AND zsenr = it_ahc01-zsenr.
      IF sy-subrc EQ 0.
        DELETE  it_ahc01.
        MESSAGE s001 WITH 'Deletion successful'.
      ELSE.
        MESSAGE i001 WITH 'Donot Deleted'.
      ENDIF.
    ELSE.
      MESSAGE s001 WITH 'Deletion successful'.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " DATA_DELETE_ENTRY
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWN_LOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_down_load_file.

  DATA : wa_filename    LIKE rlgrap-filename .

  DATA: BEGIN OF it_down OCCURS 0,
        zyear(4),
        zmons(4),
        zcost(20), " cost center
        ktext(40),
        zscst(10), " sub cost center
        stext(40),
        zjobk(10), " job text
        zperg(10),
        zsubg(10),
        zsenr(8),
        zhedc(10),
        znewc(10),
        END OF it_down.

  DATA : it_ah LIKE zthr_ahc01 OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE it_ah
           FROM zthr_ahc01
           WHERE zvers = w_zvers
             AND zyear = w_zyear.

  CLEAR : it_down, it_down[].

  LOOP AT it_ah.
    MOVE-CORRESPONDING it_ah TO it_down.
    CLEAR cskt.
    SELECT SINGLE ktext INTO it_down-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_down-zscst
                  AND datbi = '99991231'.

    SELECT SINGLE ktext INTO it_down-stext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_down-zscst
                  AND datbi = '99991231'.


    APPEND it_down . CLEAR it_down.

  ENDLOOP.

  it_down-zyear = 'Year'.
  it_down-zmons = 'Month'.
  it_down-zcost = 'Cost Center'.
  it_down-ktext = 'Name'.
  it_down-zscst = 'Sub Cost Center'.
  it_down-stext = 'Name'.
  it_down-zjobk = 'Job text'.
  it_down-zperg = 'Employee Group'.
  it_down-zsubg = 'Employee Subgroup'.
  it_down-zsenr = 'Seniority'.
  it_down-zhedc = 'Head Count'.
  it_down-znewc = 'new head count'.

  INSERT it_down INDEX 1. CLEAR it_down.

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
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename = wa_filename
            filetype = 'DAT'
       TABLES
            data_tab = it_down.

ENDFORM.                    " EXCEL_DOWN_LOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTHR_PCP05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_zthr_pcp05.
  DATA : w_zgrup  LIKE zthr_pcp02-zgrup.
  DATA : BEGIN OF it_05 OCCURS 0,
         zcost LIKE zthr_pcp05-zcost,
         zscst LIKE zthr_pcp05-zscst,
         zyear LIKE zthr_pcp05-zyear,
         zmons LIKE zthr_pcp05-zmons,
         zvers LIKE zthr_pcp05-zvers,
         zgrup LIKE zthr_pcp05-zgrup,
         zhedc TYPE i,
         END OF it_05.
  DATA : it_xx05 LIKE zthr_pcp05 OCCURS 0 WITH HEADER LINE.


  LOOP AT it_ahc01.
    MOVE-CORRESPONDING it_ahc01 TO it_05.
    it_05-zhedc = it_ahc01-zhedc + it_ahc01-znewc.

    SELECT SINGLE zgrup INTO w_zgrup
     FROM zthr_pcp02 WHERE zmodl = '02'
                       AND zval1 = it_ahc01-zjobc
                       AND ( ( zgrup = '1040' AND zval4 = 'OT' ) OR
                            ( zgrup = '1050' AND zval4 = '' ) ).
    IF sy-subrc EQ 0 .
      MOVE : w_zgrup TO it_05-zgrup.

      COLLECT it_05. CLEAR it_05.
    ENDIF.
  ENDLOOP.
  LOOP AT it_05.

    SELECT * INTO TABLE it_xx05
           FROM zthr_pcp05
             WHERE zcost = it_05-zcost
               AND zscst = it_05-zscst
               AND zyear = it_05-zyear
               AND zmons = it_05-zmons
               AND zvers = it_05-zvers
               AND zgrup = it_05-zgrup .
    IF sy-subrc EQ 0 .
      LOOP AT it_xx05.
        it_xx05-zhedc = it_05-zhedc.
        it_xx05-ztotm = it_xx05-zreld * it_xx05-zhedc *
                      ( it_xx05-zprat / 100 ) * it_xx05-zwktm.
        MODIFY it_xx05.
      ENDLOOP.
      MODIFY zthr_pcp05 FROM TABLE it_xx05.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " UPDATE_ZTHR_PCP05
*&---------------------------------------------------------------------*
*&      Form  cost_center_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTHR_PCP02_ZVAL4  text
*----------------------------------------------------------------------*
FORM cost_center_get USING    p_zval.
  DATA : l_co1 LIKE pa0001-kostl,
         l_co2 LIKE pa0001-kostl,
         l_co3 LIKE pa0001-kostl.

  CHECK  NOT p_zval IS INITIAL.
  SPLIT p_zval AT ',' INTO l_co1 l_co2 l_co3 .
  IF l_co1 <> '' .
    PERFORM conversion_costcenter USING l_co1
                             CHANGING it_cost-zcost.
    APPEND it_cost . CLEAR it_cost.
  ENDIF.

  IF l_co2 <> '' .
    PERFORM conversion_costcenter USING l_co2
                           CHANGING it_cost-zcost.
    APPEND it_cost . CLEAR it_cost.
  ENDIF.

  IF l_co3 <> '' .
    PERFORM conversion_costcenter USING l_co3
                           CHANGING it_cost-zcost.
    APPEND it_cost . CLEAR it_cost.
  ENDIF.

ENDFORM.                    " cost_center_get
*&---------------------------------------------------------------------*
*&      Form  SORT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_table.
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

  LOOP AT it_ahc01.
* OBJID
    READ TABLE it_sort WITH KEY val2 = it_ahc01-zjobc .
    IF sy-subrc EQ 0 .
      MOVE it_sort-val1 TO it_ahc01-zval1.
      MODIFY it_ahc01.
    ENDIF.
  ENDLOOP.
*SORT IT_AHC01  ASCENDING  BY ZCOST .
  SORT it_ahc01 BY zval1.
  SORT it_ahc01
                ASCENDING BY  zscst  zperg   DESCENDING
                                     zval1   ASCENDING
                                     zsenr   ASCENDING.

ENDFORM.                    " SORT_TABLE
*&---------------------------------------------------------------------*
*&      Form  chang_basic_pay
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chang_basic_pay.


ENDFORM.                    " chang_basic_pay
*&---------------------------------------------------------------------*
*&      Form  basic_pay_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM basic_pay_selection.
  DATA : l_act01  LIKE it_pcp00-act01,
         l_mthly  LIKE it_pcp00-mthly,
         l_houry  LIKE it_pcp00-houry,
         l_ancur  LIKE it_pcp00-ancur VALUE 'USD'.
  CLEAR : zthr_pcp02.

  SELECT SINGLE zval1 zval5 INTO
              (zthr_pcp02-zval1, zthr_pcp02-zval5)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1250'
                      AND zval2 = it_pcp00-zperg
                      AND zval4 = it_pcp00-zobjc.

  MOVE : zthr_pcp02-zval1 TO    l_act01,    " monthly
         zthr_pcp02-zval5 TO    l_mthly .   " annaly

  l_act01 = l_act01 .
  l_houry = l_mthly / 2080  .

  MOVE : l_houry TO it_pcp00-houry ,
         l_act01 TO it_pcp00-mthly ,
         l_act01 TO it_pcp00-omthly ,
         l_ancur TO it_pcp00-ancur.
  it_pcp00-ansal =   l_mthly.

  IF it_pcp00-zperg = '9' AND it_pcp00-zsubg = 'U2'.
    it_pcp00-act01 = it_pcp00-mthly * it_pcp00-zhedc .
  ELSEIF ( ( it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U2' ) OR
           ( it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U3' ) ).
    it_pcp00-act02 = it_pcp00-mthly * it_pcp00-zhedc .
  ELSE.
    it_pcp00-act03 = it_pcp00-mthly * it_pcp00-zhedc .
  ENDIF.

ENDFORM.                    " basic_pay_selection
*&---------------------------------------------------------------------*
*&      Form  get_subcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_subcost.
  IF w_master <> ''.
    PERFORM get_read_cost_dynpro .
  ENDIF.

  LOOP AT it_orgpn.

    SELECT SINGLE  kostl INTO it_zscst-zscst

      FROM pa0001 WHERE pernr = it_orgpn-objid
                    AND endda = '99991231'
                    AND werks = w_werks.
    CLEAR cskt.
    SELECT SINGLE ktext INTO cskt-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_zscst-zscst.
    it_zscst-kname = cskt-ktext.
    COLLECT it_zscst. CLEAR it_zscst.
  ENDLOOP.

  LOOP AT it_cost .
    it_zscst-zscst = it_cost-zcost.
    CLEAR cskt.
    SELECT SINGLE ktext INTO cskt-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_cost-zcost.
    it_zscst-kname = cskt-ktext.
    COLLECT  it_zscst. CLEAR it_zscst.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_zscst
    COMPARING zscst.


ENDFORM.                    " get_subcost
*&---------------------------------------------------------------------*
*&      Form  get_subcost_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_subcost_master.
  REFRESH it_zscst. CLEAR it_zscst.
  SELECT  zctxt zval1 INTO (it_zscst-zscst,it_zscst-kname)
      FROM zthr_pcp02
       WHERE zmodl EQ '02'
         AND ( zgrup EQ '1260' or zgrup EQ '1270' ).
    APPEND it_zscst.
  ENDSELECT.

  SORT it_zscst BY zscst.
  DELETE ADJACENT DUPLICATES FROM it_zscst COMPARING zscst.

ENDFORM.                    " get_subcost_master
*&---------------------------------------------------------------------*
*&      Form  select_data_new_cost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ZMONS  text
*----------------------------------------------------------------------*
FORM select_data_new_cost USING    p_month.
  DATA: l_seqno LIKE zthr_pcp05-zseqn,
        l_headc LIKE zthr_ahc01-zhedc.
  DATA : lt_pcp00 LIKE zthr_pcp00 OCCURS 0 WITH HEADER LINE.
  REFRESH lt_pcp00.
  l_seqno = 1.
*
  it_pcp05-zseqn = l_seqno.
  CLEAR : w_zdays,l_headc.

  CASE p_month.
    WHEN '01'. read_pcp04 day01.
    WHEN '02'. read_pcp04 day02.
    WHEN '03'. read_pcp04 day03.
    WHEN '04'. read_pcp04 day04.
    WHEN '05'. read_pcp04 day05.
    WHEN '06'. read_pcp04 day06.
    WHEN '07'. read_pcp04 day07.
    WHEN '08'. read_pcp04 day08.
    WHEN '09'. read_pcp04 day09.
    WHEN '10'. read_pcp04 day10.
    WHEN '11'. read_pcp04 day11.
    WHEN '12'. read_pcp04 day12.
  ENDCASE.
  it_pcp05-zplnd = w_zdays.
  CLEAR p_flag.
  PERFORM check_closed_month USING p_flag.
  IF p_flag EQ 'S'.

    LOOP AT it_zscst WHERE zscst = w_zscst.
      SELECT * INTO TABLE lt_pcp00
         FROM zthr_pcp00
            WHERE zvers = w_zvers
              AND zyear = w_zyear
              AND zmons = p_month
              AND zcost =  w_zscst
              AND zobjc IN zjobc
              AND NOT ( zperg = '9' AND zsubg = 'U2' ).
      LOOP AT lt_pcp00.
        l_headc = l_headc +  lt_pcp00-zhedc.
      ENDLOOP.
    ENDLOOP.
  ELSE.
    l_headc = 0.
  ENDIF.

  it_pcp05-zhedc = l_headc.
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1010'
                      AND zcode = '10000'.
  it_pcp05-zextr = zthr_pcp02-zval1.
  APPEND it_pcp05. CLEAR it_pcp05.
  l_seqno = l_seqno + 1.
*
  it_pcp05-zseqn = l_seqno.
  CLEAR w_zdays.

  CASE p_month.
    WHEN '01'. read_pcp04 day01.
    WHEN '02'. read_pcp04 day02.
    WHEN '03'. read_pcp04 day03.
    WHEN '04'. read_pcp04 day04.
    WHEN '05'. read_pcp04 day05.
    WHEN '06'. read_pcp04 day06.
    WHEN '07'. read_pcp04 day07.
    WHEN '08'. read_pcp04 day08.
    WHEN '09'. read_pcp04 day09.
    WHEN '10'. read_pcp04 day10.
    WHEN '11'. read_pcp04 day11.
    WHEN '12'. read_pcp04 day12.
  ENDCASE.
  it_pcp05-zplnd = w_zdays.
  it_pcp05-zhedc = l_headc.
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1010'
                      AND zcode = '10010'.
  it_pcp05-zextr = zthr_pcp02-zval1.
  APPEND it_pcp05. CLEAR it_pcp05.
  l_seqno = l_seqno + 1.
*
  it_pcp05-zseqn = l_seqno.
  CLEAR w_zdays.

  CASE p_month.
    WHEN '01'. read_pcp04 sat01.
    WHEN '02'. read_pcp04 sat02.
    WHEN '03'. read_pcp04 sat03.
    WHEN '04'. read_pcp04 sat04.
    WHEN '05'. read_pcp04 sat05.
    WHEN '06'. read_pcp04 sat06.
    WHEN '07'. read_pcp04 sat07.
    WHEN '08'. read_pcp04 sat08.
    WHEN '09'. read_pcp04 sat09.
    WHEN '10'. read_pcp04 sat10.
    WHEN '11'. read_pcp04 sat11.
    WHEN '12'. read_pcp04 sat12.
  ENDCASE.
  it_pcp05-zplnd = w_zdays.
  it_pcp05-zhedc = l_headc.
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1010'
                      AND zcode = '10020'.
  it_pcp05-zextr = zthr_pcp02-zval1.
  APPEND it_pcp05. CLEAR it_pcp05.
  l_seqno = l_seqno + 1.
*
  it_pcp05-zseqn = l_seqno.
  CLEAR w_zdays.

  CASE p_month.
    WHEN '01'. read_pcp04 sun01.
    WHEN '02'. read_pcp04 sun02.
    WHEN '03'. read_pcp04 sun03.
    WHEN '04'. read_pcp04 sun04.
    WHEN '05'. read_pcp04 sun05.
    WHEN '06'. read_pcp04 sun06.
    WHEN '07'. read_pcp04 sun07.
    WHEN '08'. read_pcp04 sun08.
    WHEN '09'. read_pcp04 sun09.
    WHEN '10'. read_pcp04 sun10.
    WHEN '11'. read_pcp04 sun11.
    WHEN '12'. read_pcp04 sun12.
  ENDCASE.
  it_pcp05-zplnd = w_zdays.
  it_pcp05-zhedc = l_headc.
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1010'
                      AND zcode = '10030'.
  it_pcp05-zextr = zthr_pcp02-zval1.
  APPEND it_pcp05. CLEAR it_pcp05.
  l_seqno = l_seqno + 1.
*
  it_pcp05-zseqn = l_seqno.
  CLEAR w_zdays.

  CASE p_month.
    WHEN '01'. read_pcp04 hol01.
    WHEN '02'. read_pcp04 hol02.
    WHEN '03'. read_pcp04 hol03.
    WHEN '04'. read_pcp04 hol04.
    WHEN '05'. read_pcp04 hol05.
    WHEN '06'. read_pcp04 hol06.
    WHEN '07'. read_pcp04 hol07.
    WHEN '08'. read_pcp04 hol08.
    WHEN '09'. read_pcp04 hol09.
    WHEN '10'. read_pcp04 hol10.
    WHEN '11'. read_pcp04 hol11.
    WHEN '12'. read_pcp04 hol12.
  ENDCASE.
  it_pcp05-zplnd = w_zdays.
  it_pcp05-zhedc = l_headc.
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1010'
                      AND zcode = '10040'.
  it_pcp05-zextr = zthr_pcp02-zval1.
  APPEND it_pcp05. CLEAR it_pcp05.

ENDFORM.                    " select_data_new_cost
*&---------------------------------------------------------------------*
*&      Form  dynp_values_read
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DYNPFIELDS  text
*----------------------------------------------------------------------*
FORM dynp_values_read TABLES  p_dynpfields  STRUCTURE  dynpread.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname     = sy-cprog
            dynumb     = sy-dynnr
       TABLES
            dynpfields = dynpfields.
ENDFORM.                    " dynp_values_read
*&---------------------------------------------------------------------*
*&      Form  conversion_costcenter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4055   text
*      <--P_IT_COST_ZCOST  text
*----------------------------------------------------------------------*
FORM conversion_costcenter USING  p_4055
                           CHANGING p_zcost.
  DATA: string_type(4) TYPE c.

  CALL FUNCTION 'NUMERIC_CHECK'
       EXPORTING
            string_in = p_4055
       IMPORTING
            htype     = string_type.

  IF string_type NE 'CHAR'.
    UNPACK p_4055 TO p_zcost.
  ELSE.
    MOVE p_4055 TO p_zcost.
  ENDIF.
ENDFORM.                    " conversion_costcenter
*&---------------------------------------------------------------------*
*&      Form  CHECK_CLOSED_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FLAG  text
*----------------------------------------------------------------------*
FORM check_closed_month USING  g_flag.
  SELECT SINGLE *
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND ( zgrup = '1260' or zgrup = '1270' )
                      AND zctxt = w_zscst.

  IF sy-subrc = 0.
    g_flag = 'S'.
  ELSE.
    CLEAR zthr_ahc01.
    SELECT SINGLE zsenr INTO zthr_ahc01-zsenr
      FROM zthr_ahc01 WHERE zvers = w_zvers
                        AND zyear = w_zyear
                        AND zmons = w_zmons
*                      AND zpera = w_werks
                        AND zcost = w_kostl
                        AND zscst = w_zscst.
    IF sy-subrc = 0.
      g_flag = 'S'.
    ELSE.
      g_flag = ' '.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_CLOSED_MONTH
