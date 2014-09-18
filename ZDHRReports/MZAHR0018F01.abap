*----------------------------------------------------------------------*
*   INCLUDE MZAHR0007F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN_9100
*&---------------------------------------------------------------------*
FORM init_screen_9100.

  REFRESH it_et03[].
  CLEAR : w_pernr,w_kostl,w_sachz,w_zhedc .
  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE it_et03 LINES tc9100-lines.
  CALL SCREEN 9100.
ENDFORM.                    " INIT_SCREEN_9100
*&---------------------------------------------------------------------*
*&      Form  SELECT_HEAD_COUNT_DATA
*&---------------------------------------------------------------------*
FORM select_count_data.
  CLEAR it_et03. REFRESH it_et03.

  IF w_zyear = space OR w_zmons = space .
    MESSAGE w001 WITH 'Please make a selection'.
  ENDIF.

  IF w_kostl = space.
    MESSAGE w001 WITH text-010.
    EXIT.
  ENDIF.

  PERFORM get_data_employee USING w_pernr.

  CLEAR zthr_et03.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_et03
    FROM zthr_et03 WHERE  zyear = w_zyear
                      AND zmons = w_zmons
                      AND zcost = w_kostl.
  IF sy-subrc <> 0.
    MESSAGE w001 WITH text-011.
    w_status = 'I'.
  ELSE.
    w_status = 'U'.
    LOOP AT it_et03.
      SELECT SINGLE zctxt INTO it_et03-zetext
              FROM zthr_et02 WHERE zmodl EQ '01'
                                         AND zgrup EQ '1020'
                                         AND zval1 EQ it_et03-zetcode.
      PERFORM get_work_time USING it_et03-zttime(4)
                                it_et03-zftime(4)
                          CHANGING it_et03-zetmh1.

      MODIFY it_et03 INDEX sy-tabix.
    ENDLOOP.
  ENDIF.

  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE it_et03 LINES tc9100-lines.
  w_flags = 'X'.
ENDFORM.                    " SELECT_HEAD_COUNT_DATA
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
*&      Form  CHECK_AUTHORITY
*&---------------------------------------------------------------------*
FORM check_authority.
  DATA: l_zval1 LIKE zthr_et02-zval1.
*
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = sy-uname
       IMPORTING
            output = w_pernr.
*
  CONCATENATE sy-uname '%' INTO l_zval1.

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

  w_kostl = it_perda-kostl.
  CLEAR cskt.
  SELECT SINGLE ktext INTO cskt-ktext
    FROM cskt WHERE spras = sy-langu
                AND kostl = w_kostl
                AND datbi = '99991231'.
  w_ktext = cskt-ktext.

ENDFORM.                    " CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*&      Form  INSERT_NEW_ENTRY
*&---------------------------------------------------------------------*
FORM insert_new_entry.
  IF w_flags = 'X'.
    REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
    DESCRIBE TABLE it_et03 LINES tc9100-lines.
    tc9100-lines = tc9100-lines + 1.
    w_status = 'U'.
  ELSE.
    MESSAGE i001 WITH text-021.
    w_status = ' '.
  ENDIF.
ENDFORM.                    " INSERT_NEW_ENTRY
*&---------------------------------------------------------------------*
*&      Form  DATA_DELETE_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_delete_entry.

  READ TABLE it_et03 WITH KEY chkbx = 'X'.
  IF sy-subrc = 0.
    SELECT SINGLE * FROM zthr_et03
              WHERE zyear EQ it_et03-zyear
                AND zmons EQ it_et03-zmons
                AND zcost EQ it_et03-zcost
                AND zdate EQ it_et03-zdate
                AND zftime  EQ it_et03-zftime
                AND zetcode EQ it_et03-zetcode.

    IF sy-subrc EQ 0.
      DELETE FROM zthr_et03
             WHERE  zyear EQ it_et03-zyear
                AND zmons EQ it_et03-zmons
                AND zcost EQ it_et03-zcost
                AND zdate EQ it_et03-zdate
                AND zftime  EQ it_et03-zftime
                AND zetcode EQ it_et03-zetcode.
    ENDIF.

    DELETE  it_et03 WHERE  zyear EQ it_et03-zyear
        AND zmons EQ it_et03-zmons
        AND zcost EQ it_et03-zcost
        AND zdate EQ it_et03-zdate
        AND zftime  EQ it_et03-zftime
        AND zetcode EQ it_et03-zetcode.

    MESSAGE s001 WITH text-024.

  ENDIF.

  REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
  DESCRIBE TABLE it_et03 LINES tc9100-lines.
ENDFORM.                    " DATA_DELETE_ENTRY
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
         l_co3 LIKE pa0001-kostl,
         l_co4 LIKE pa0001-kostl,
         l_co5 LIKE pa0001-kostl,
         f_field LIKE pa0001-kostl,
         f_num TYPE n.
  FIELD-SYMBOLS : <g_field> TYPE ANY.
  CLEAR : f_num.
  CHECK  NOT p_zval IS INITIAL.
  SPLIT p_zval AT ',' INTO l_co1 l_co2 l_co3 l_co4.
  f_num = 1.
  DO 5 TIMES.
    CONCATENATE 'l_co' f_num INTO f_field.
    ASSIGN (f_field) TO <g_field>.
    IF <g_field> <> '' .
      PERFORM  numeric_check USING <g_field>
                               CHANGING value_cost-kostl.

      PERFORM filled_costcenter_name USING value_cost-kostl
                               CHANGING  value_cost-zctxt.

      APPEND value_cost. CLEAR value_cost.
      f_num = f_num + 1.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " cost_center_get
*&---------------------------------------------------------------------*
*&      Form  conversion_costcenter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4055   text
*      <--P_IT_COST_ZCOST  text
*----------------------------------------------------------------------*
FORM numeric_check USING  p_4055
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
*&      Form  get_work_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ET03_ZTTIME(4)  text
*      -->P_IT_ET03_ZFTIME(4)  text
*----------------------------------------------------------------------*
FORM get_work_time USING    p_zttime
                            p_zftime
                   CHANGING p_diff.

  DATA : differenz TYPE i,
         e_endtime TYPE t,
         i_starttime TYPE t,
         i_mintime TYPE t.
*check : from time & to time
  IF p_zttime < p_zftime.
    MESSAGE i001 WITH text-016.
    EXIT.
  ENDIF.

  i_starttime = p_zttime.
  e_endtime =  p_zftime.
  differenz =  i_starttime - e_endtime .

  p_diff = differenz  / 60.
  CONCATENATE p_zttime '00' INTO it_et03-zttime .
  CONCATENATE p_zftime '00' INTO it_et03-zftime .

ENDFORM.                    " get_work_time
*&---------------------------------------------------------------------*
*&      Form  get_name_etcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ET03_ZETCODE  text
*----------------------------------------------------------------------*
FORM get_name_etcode USING    b_zetcode
                     CHANGING p_zettext
                              a_zetcode.
  IF NOT b_zetcode IS INITIAL.

    SELECT SINGLE  *  FROM zthr_et02 WHERE zmodl EQ '01'
                                       AND zgrup EQ '1020'
                                       AND zval1 EQ b_zetcode.
    IF  zthr_et02-zval1 EQ   b_zetcode.
      SELECT zctxt INTO p_zettext
              FROM zthr_et02  UP TO 1 ROWS
                WHERE zmodl EQ '01'
                  AND zgrup EQ '1020'
                  AND zval1 EQ b_zetcode.
      ENDSELECT.
      MOVE b_zetcode TO a_zetcode.
    ELSE.
      MESSAGE i001 WITH text-015.
      CLEAR a_zetcode.
    ENDIF.
  ELSE.
    MESSAGE i001 WITH text-019.
  ENDIF.
ENDFORM.                    " get_name_etcode
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
*Precheck
  PERFORM precheck_data CHANGING f_flag.
  IF f_flag IS INITIAL.
*save at table zthr_et03.
    PERFORM save_table.
  ENDIF.
ENDFORM.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  save_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_table.
  CLEAR : it_et03,save_flag.
  LOOP AT it_et03.
    SELECT SINGLE * FROM zthr_et03
     WHERE zyear   EQ it_et03-zyear
       AND zmons   EQ it_et03-zmons
       AND zcost   EQ it_et03-zcost
       AND zdate   EQ it_et03-zdate
       AND zftime  EQ it_et03-zftime
       AND zetcode EQ it_et03-zetcode.

    IF sy-subrc <> 0. " insert
      MOVE-CORRESPONDING it_et03 TO zthr_et03.
      zthr_et03-erdat = sy-datum.
      zthr_et03-erzet = sy-uzeit.
      zthr_et03-ernam = sy-uname.
      INSERT zthr_et03. CLEAR : it_et03,zthr_et03.
      MESSAGE s001 WITH text-022.
      COMMIT WORK.
      save_flag = 'X'.
    ELSE.
      MOVE-CORRESPONDING it_et03 TO zthr_et03.
      zthr_et03-aedat = sy-datum.
      zthr_et03-aezet = sy-uzeit.
      zthr_et03-aenam = sy-uname.
      MODIFY zthr_et03.CLEAR : it_et03,zthr_et03.
      MESSAGE s001 WITH text-023.
      COMMIT WORK.
      save_flag = 'X'.
    ENDIF.
     CLEAR w_status .
  ENDLOOP.

ENDFORM.                    " save_table
*&---------------------------------------------------------------------*
*&      Form  precheck_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM precheck_data CHANGING g_flag.
  CLEAR it_et03.
  LOOP AT it_et03.
*diff hour check
    PERFORM get_work_time USING it_et03-zttime(4)
                                it_et03-zftime(4)
                      CHANGING it_et03-zetmh1.
    IF it_et03-zttime(4) EQ '0' OR it_et03-zftime(4) EQ '0'
      OR it_et03-zetmh1 =< 0.
      MESSAGE i001 WITH text-019.
      g_flag = 'X'.
      EXIT.
    ENDIF.
*Check : ET CODE
    IF it_et03-zetcode IS INITIAL.
      MESSAGE i001 WITH text-017.
      g_flag = 'X'.
      EXIT.
    ENDIF.
*Check : ET Header count
    IF it_et03-zwhedc EQ '0'.
      MESSAGE i001 WITH text-020.
      g_flag = 'X'.
      EXIT.
    ENDIF.
*Check : over ET Heaer count
    IF it_et03-zrhedc  < it_et03-zwhedc.
      MESSAGE i001 WITH text-029.
      g_flag = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " precheck_data
*&---------------------------------------------------------------------*
*&      Form  calculation_entry
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_entry.
  LOOP AT it_et03.
*diff hour check
    PERFORM get_work_time USING it_et03-zttime(4)
                                it_et03-zftime(4)
                      CHANGING it_et03-zetmh1.
    it_et03-zetmh = ( it_et03-zetmh1 * it_et03-zwhedc ) / 60.
    MODIFY it_et03 INDEX sy-tabix.
  ENDLOOP.
ENDFORM.                    " calculation_entry
*&---------------------------------------------------------------------*
*&      Form  init_screen_9200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_screen_9200.
  CLEAR : w_status, w_kostl .
  REFRESH it_et03.
  CALL SCREEN 9200.
ENDFORM.                    " init_screen_9200
*&---------------------------------------------------------------------*
*&      Form  filled_costcenter_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VALUE_COST_KOSTL  text
*      <--P_VALUE_COST_ZCTXT  text
*----------------------------------------------------------------------*
FORM filled_costcenter_name USING    p_kostl
                            CHANGING p_zctxt.

  SELECT SINGLE ktext INTO p_zctxt
       FROM cskt WHERE spras = sy-langu
                   AND kostl =  p_kostl.
ENDFORM.                    " filled_costcenter_name
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA w_zetcode LIKE it_et03-zetcode.

  REFRESH it_et03.
  CLEAR w_int.
  IF w_zmons > w_zmonst AND w_zmonst <> space.
    MESSAGE i001 WITH text-026.
    EXIT.
  ENDIF.

  PERFORM range_value.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_et03
      FROM zthr_et03
         WHERE  zyear EQ w_zyear
            AND zmons IN r_mons
            AND zcost IN r_cost.

  DESCRIBE TABLE it_et03 LINES w_int.
  IF w_int <> 0.
    SORT it_et03 ASCENDING BY zdate.
    LOOP AT it_et03.
      PERFORM get_work_time USING it_et03-zttime(4)
                                  it_et03-zftime(4)
                            CHANGING it_et03-zetmh1.

      MOVE it_et03-zetcode TO w_zetcode.
      PERFORM get_name_etcode USING  w_zetcode
                            CHANGING it_et03-zetext
                                     it_et03-zetcode.
*      it_et03-zetmh = ( it_et03-zetmh1 * it_et03-zwhedc ) / 60.
      MODIFY it_et03 INDEX sy-tabix.
    ENDLOOP.
  ELSE.
    MESSAGE i001 WITH text-025.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  range_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM range_value.
  REFRESH : r_mons,r_cost.

  r_mons-option = 'BT'.
  r_mons-sign   = 'I'.
  r_mons-low    = w_zmons.
  IF w_zmonst = space.
    r_mons-high  = w_zmons.
  ELSE.
    r_mons-high  = w_zmonst.
  ENDIF.
  IF r_mons-low NE space.
    APPEND r_mons.
  ENDIF.

  r_cost-option = 'BT'.
  r_cost-sign   = 'I'.
  r_cost-low    = w_kostl.
  IF w_kostlt = space.
    r_cost-high   =  w_kostl.
  ELSE.
    r_cost-high = w_kostlt.
  ENDIF.
  IF w_kostl NE space.
    APPEND r_cost.
  ENDIF.
ENDFORM.                    " range_value
*&---------------------------------------------------------------------*
*&      Form  download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download.
  DATA : wa_filename    LIKE rlgrap-filename .

  DATA: BEGIN OF it_down OCCURS 0,
        zyear(4),
        zmons(6),
        zcost(10), " cost center
        zdate(11),
        zetcode(07),
        zetext(40),
        zwhedc(10),
        zetmh(07),
        END OF it_down.
  REFRESH it_down.
  LOOP AT it_et03.
    MOVE-CORRESPONDING it_et03 TO it_down.
    APPEND it_down.CLEAR : it_et03,it_down.
  ENDLOOP.

  it_down-zyear = 'Year'.
  WRITE : it_down-zyear TO it_down-zyear CENTERED.
  it_down-zmons = 'Period'.
  WRITE : it_down-zmons TO it_down-zmons CENTERED.
  it_down-zcost = 'CostCenter'.
  WRITE : it_down-zcost TO it_down-zcost CENTERED.
  it_down-zdate = 'ExecuteDate'.
  WRITE : it_down-zdate TO it_down-zdate CENTERED.
  it_down-zetcode = 'ET Code'.
  WRITE : it_down-zetcode TO it_down-zetcode CENTERED.
  it_down-zetext  = 'E.T Code Name'.
  WRITE : it_down-zetext TO it_down-zetext CENTERED.
  it_down-zwhedc = 'E.T Worker'.
  WRITE : it_down-zwhedc TO it_down-zwhedc CENTERED.
  it_down-zetmh = 'E.T M/H'.
  WRITE : it_down-zetmh TO it_down-zetmh CENTERED.
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

ENDFORM.                    " download
*&---------------------------------------------------------------------*
*&      Form  numeric_check_pernr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTHR_ET02_ZVAL1  text
*      <--P_L_ZVAL1  text
*----------------------------------------------------------------------*
FORM numeric_check_pernr USING    p_zthr_et02_zval1
                         CHANGING pl_zval1.
  DATA: string_type(4) TYPE c.

  CALL FUNCTION 'NUMERIC_CHECK'
       EXPORTING
            string_in = p_zthr_et02_zval1
       IMPORTING
            htype     = string_type.

  IF string_type NE 'CHAR'.
    UNPACK p_zthr_et02_zval1  TO pl_zval1.
  ELSE.
    MOVE p_zthr_et02_zval1 TO pl_zval1.
  ENDIF.

ENDFORM.                    " numeric_check_pernr
*&---------------------------------------------------------------------*
*&      Form  get_data_employee
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_PERNR  text
*----------------------------------------------------------------------*
FORM get_data_employee USING  w_pernr.
  DATA : l_zval1  LIKE zthr_et02-zval1,
         lw_kostl LIKE pa0001-kostl.

  CLEAR :  w_rc_head,w_sachz,lw_kostl.

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

  IF sy-subrc = 0.
    PERFORM  numeric_check_pernr USING w_pernr
                                 CHANGING w_pernr.

    SELECT SINGLE  zval1 INTO (zthr_et02-zval1)
           FROM zthr_et02 WHERE zmodl = '01'
                          AND zgrup = '1010'
                          AND zval1 EQ  w_pernr.

    IF zthr_et02-zval1 EQ space.
      MESSAGE i001 WITH text-014.
      CLEAR : w_pernr,w_sachz,w_zhedc.
      EXIT.
    ELSE.
      PERFORM  numeric_check_pernr USING zthr_et02-zval1
                                   CHANGING l_zval1.

      SELECT sachz INTO w_sachz
            FROM pa0001 UP TO 1 ROWS
                  WHERE pernr EQ w_pernr
                    AND endda EQ '99991231'.
      ENDSELECT.

      SELECT COUNT( * ) INTO  w_zhedc
            FROM pa0001
                 WHERE sachz EQ  w_sachz
                  AND endda EQ '99991231'.

*      SELECT COUNT( * ) INTO  w_rc_head
*            FROM pa0001
*                 WHERE kostl EQ  w_kostl
*                  AND endda EQ '99991231'.
*
*      IF w_rc_head EQ 0.
*        MESSAGE w001 WITH text-027.
*        REFRESH IT_ET03.
*        REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
*        DESCRIBE TABLE it_et03 LINES tc9100-lines.
*        CLEAR w_kostl.
*        CALL SCREEN 9100.
*      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE i001 WITH text-013.
    EXIT.
  ENDIF.
ENDFORM.                    " get_data_employee
*&---------------------------------------------------------------------*
*&      Form  cost_center_get_tg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_PERNR  text
*----------------------------------------------------------------------*
FORM cost_center_get_tg USING    pw_pernr.
  REFRESH value_cost.
  SELECT sacha INTO w_sachz
        FROM pa0001 UP TO 1 ROWS
              WHERE pernr EQ w_pernr
                AND endda EQ '99991231'.
  ENDSELECT.

  SELECT kostl  INTO value_cost
        FROM pa0001
             WHERE sachz EQ  w_sachz .
    PERFORM filled_costcenter_name USING value_cost-kostl
                             CHANGING  value_cost-zctxt.
    APPEND value_cost. CLEAR value_cost.
  ENDSELECT.

  SORT value_cost ASCENDING BY kostl.

  DELETE ADJACENT DUPLICATES FROM value_cost
             COMPARING ALL FIELDS.
ENDFORM.                    " cost_center_get_tg
*&---------------------------------------------------------------------*
*&      Form  pop_up_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TITLE  text
*      <--P_WA_ANSWER  text
*----------------------------------------------------------------------*
FORM pop_up_message  USING p_text
                     CHANGING p_answer.
  DATA : answer TYPE c.
  DATA : p_text1(50).
  p_text1 = text-028.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1 = p_text
            textline2 = p_text1
            titel     = 'E.T M/H Screen exit Confirm'
       IMPORTING
            answer    = answer.
  MOVE : answer TO p_answer .

ENDFORM.                    " pop_up_message
*&---------------------------------------------------------------------*
*&      Form  move_it_et03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_it_et03.
  DATA : w_zetcode LIKE it_et03-zetcode.
  CLEAR : w_int,w_zetcode.

  MOVE-CORRESPONDING wa_et03 TO it_et03.
  MOVE : w_zyear   TO it_et03-zyear,
         w_zmons   TO it_et03-zmons,
         w_kostl   TO it_et03-zcost,
         w_sachz   TO it_et03-sachz,
         w_zhedc   TO it_et03-zhedc,
         w_kostl   TO it_et03-zcost.

  SELECT COUNT( * ) INTO  w_rc_head
      FROM pa0001
           WHERE kostl EQ  w_kostl.

  MOVE   w_rc_head TO it_et03-zrhedc.
  IF  it_et03-zrhedc  < it_et03-zwhedc.
    CLEAR  it_et03-zwhedc .
    MESSAGE s001 WITH text-029.
    EXIT.
  ENDIF.

  PERFORM get_work_time USING it_et03-zttime(4)
                              it_et03-zftime(4)
                        CHANGING it_et03-zetmh1.

  MOVE it_et03-zetcode TO w_zetcode.
  PERFORM get_name_etcode USING  w_zetcode
                        CHANGING it_et03-zetext
                                 it_et03-zetcode.

  it_et03-zetmh = ( it_et03-zetmh1 * it_et03-zwhedc ) / 60.
ENDFORM.                    " move_it_et03
