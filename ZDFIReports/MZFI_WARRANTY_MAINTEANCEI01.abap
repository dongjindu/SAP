*----------------------------------------------------------------------*
***INCLUDE MZFI_WARRANTY_MAINTEANCEI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  DATA: l_answer(1) TYPE c.                                 "HIS20094
  CHECK v_emsg IS INITIAL.                                  "HIS20094

  IF ztfi_warranty-versn IS INITIAL.                        "HIS20094
    v_emsg = 'X'.                                           "HIS20094
    MESSAGE e000(zz)                                        "HIS20094
       WITH 'Enter Version'.                                "HIS20094
  ENDIF.                                                    "HIS20094

  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.

* BEGIN OF HIS20094
      IF NOT c_flag IS INITIAL
      OR it_source_tmp[] NE it_source[].
        PERFORM confirm_step CHANGING l_answer.
        CHECK l_answer = 'J'.
      ENDIF.
* END OF HIS20094

      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'INS'.
      CLEAR: it_source.

      it_source-versn = ztfi_warranty-versn.
      it_source-model = ztfi_warranty-model.
      it_source-land1 = ztfi_warranty-land1.
      it_source-m_gjahr = ztfi_warranty-m_gjahr.
* BEGIN OF HIS20094
      READ TABLE it_source WITH KEY versn = ztfi_warranty-versn
                                    model = ztfi_warranty-model
                                    land1 = ztfi_warranty-land1
                                  m_gjahr = ztfi_warranty-m_gjahr.
      IF sy-subrc EQ 0.
        v_emsg = 'X'.
        MESSAGE e000(zz)
           WITH 'Enter different Version/Model/Country/Model'.
      ENDIF.

      IF it_source-versn   IS INITIAL OR
         it_source-model   IS INITIAL OR
         it_source-land1   IS INITIAL OR
         it_source-m_gjahr IS INITIAL.
        v_emsg = 'X'.
        MESSAGE e000(zz) WITH 'Enter Version/Model/Country/Model'.
      ENDIF.
      v_change = 'X'.
      INSERT it_source INDEX 1.                             "HIS20094
*     APPEND it_source.                                     "HIS20094
* END OF HIS20094
    WHEN 'CHG'.
      v_change = 'X'.                                       "HIS20094
    WHEN 'DEL'.
      CHECK NOT v_change IS INITIAL.                        "HIS20094
      IF NOT l_index IS INITIAL.
        DELETE  it_source INDEX l_index.
      ENDIF.
    WHEN 'EXEC'.
* BEGIN OF HIS20094
      IF NOT c_flag IS INITIAL.
        PERFORM confirm_step CHANGING l_answer.
        CHECK l_answer = 'J'.
      ENDIF.
      CLEAR v_change.
      CLEAR c_flag.
* END OF HIS20094

      PERFORM execute_query.
    WHEN 'SAVE'.
      CHECK NOT v_change IS INITIAL.                        "HIS20094

      CLEAR wa_versn-versn.
      IF NOT c_flag IS INITIAL
          OR it_source_tmp[] NE it_source[].                "HIS20094


        TABLES: ztfi_war_sales.
        SELECT COUNT( * ) INTO sy-index FROM ztfi_war_post
                  WHERE versn EQ ztfi_warranty-versn.

        IF sy-subrc EQ 0.
          v_emsg = 'X'.                                     "HIS20094
          MESSAGE e000(zz) WITH 'Transaction already exist. No save'.
        ELSE.
* BEGIN HIS20094
          IF ztfi_warranty_h-recl_rate IS INITIAL.
            v_emsg = 'X'.
            MESSAGE e000(zz) WITH 'Please enter Reclaim Rate'.
          ELSEIF ztfi_warranty_h-disc_rate IS INITIAL.
            v_emsg = 'X'.
            MESSAGE e000(zz) WITH 'Please enter Discount Rate'.
          ENDIF.
* END HIS20094
          SELECT SINGLE * FROM ztfi_warranty
                    WHERE versn EQ ztfi_warranty-versn.
          IF sy-subrc = 0.
            DELETE FROM ztfi_warranty
                   WHERE versn EQ ztfi_warranty-versn.
          ENDIF.

          DELETE it_source WHERE wrbtr IS initial.          "HIS20094
          CLEAR it_source.
          LOOP AT it_source.
            it_source-versn = ztfi_warranty-versn.          "HIS20094
            it_source-uname = sy-uname.
            it_source-cdate = sy-datum.
            MODIFY it_source.                               "HIS20094
            MODIFY ztfi_warranty FROM it_source.
          ENDLOOP.

* BEGIN HIS20094
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING it_source TO ztfi_warranty_h.
            MODIFY ztfi_warranty_h.
          ELSE.
            DELETE FROM ztfi_warranty_h
                  WHERE versn = ztfi_warranty-versn.
          ENDIF.

          ztfi_warranty_h-versn = ztfi_warranty-versn.
          CLEAR ztfi_warranty.
          CLEAR v_change.
          CLEAR c_flag.
          it_source_tmp[] = it_source[].
          ztfi_warranty-versn = ztfi_warranty_h-versn.

          IF NOT it_source[] IS INITIAL.
            MESSAGE s000(zz) WITH 'Data Saved'.
          ELSE.
            CLEAR ztfi_warranty_h.
            MESSAGE s000(zz) WITH 'No Data to Save'.
          ENDIF.
* END HIS20094
        ENDIF.

      ELSE.
        v_emsg = 'X'.                                       "HIS20094
        MESSAGE e000(zz) WITH 'No Changes to Save'.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  move_screen_to_itab  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_screen_to_itab INPUT.

  READ TABLE it_source  INDEX tc1-current_line.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
    EXIT FROM STEP-LOOP.
  ELSE.
    ztfi_warranty_str-versn = ztfi_warranty-versn.          "HIS20094
    ztfi_warranty_str-wrbtr = ztfi_warranty_str-wrbtr1 +
                              ztfi_warranty_str-wrbtr2 +
                              ztfi_warranty_str-wrbtr3 +
                              ztfi_warranty_str-wrbtr4 +
                              ztfi_warranty_str-wrbtr5 +
                              ztfi_warranty_str-wrbtr6 +
                              ztfi_warranty_str-wrbtr7 +
                              ztfi_warranty_str-wrbtr8 +
                              ztfi_warranty_str-wrbtr9 +
                              ztfi_warranty_str-wrbtr10.
    MOVE-CORRESPONDING ztfi_warranty_str TO it_source.
    MODIFY it_source INDEX tc1-current_line.
    IF ztfi_warranty_str-check_b EQ 'X'.
      l_index = tc1-current_line.
    ENDIF.
  ENDIF.
ENDMODULE.                 " move_screen_to_itab  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Form  insert_records
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_records.

ENDFORM.                    " insert_records
*&---------------------------------------------------------------------*
*&      Module  check_value  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_value INPUT.
  IF sy-ucomm  EQ 'INS'.
    IF  ztfi_warranty-model IS INITIAL.
      MESSAGE e000(zz) WITH 'Choose your  Model to create record'.
    ENDIF.
    IF  ztfi_warranty-land1 IS INITIAL.
      MESSAGE e000(zz) WITH 'Choose  your Country to create record'.
    ENDIF.
    IF  ztfi_warranty-m_gjahr IS INITIAL.
      MESSAGE e000(zz) WITH 'Choose Model Year to create record'.
    ENDIF.
  ENDIF.
ENDMODULE.                 " check_value  INPUT
*&---------------------------------------------------------------------*
*&      Form  execute_query
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_query.


* by ig.moon 2/6/2008 {

  DATA:   wa_tab(72) TYPE c,
          t_versn LIKE STANDARD TABLE OF wa_tab WITH NON-UNIQUE
                    DEFAULT KEY INITIAL SIZE 5,
          t_model LIKE STANDARD TABLE OF wa_tab WITH NON-UNIQUE
                    DEFAULT KEY INITIAL SIZE 5,
          t_m_gjahr LIKE STANDARD TABLE OF wa_tab WITH NON-UNIQUE
                    DEFAULT KEY INITIAL SIZE 5,
          t_land1 LIKE STANDARD TABLE OF wa_tab WITH NON-UNIQUE
                    DEFAULT KEY INITIAL SIZE 5.

  IF NOT ztfi_warranty-versn IS INITIAL.
    CONCATENATE 'versn EQ^''' ztfi_warranty-versn '''' INTO wa_tab.
    REPLACE '^' WITH ' ' INTO wa_tab.
    APPEND wa_tab TO t_versn.
  ENDIF.
  IF NOT ztfi_warranty-model IS INITIAL.
    CONCATENATE 'model EQ^''' ztfi_warranty-model '''' INTO wa_tab.
    REPLACE '^' WITH ' ' INTO wa_tab.
    APPEND wa_tab TO t_model.
  ENDIF.
  IF NOT ztfi_warranty-m_gjahr IS INITIAL.
    CONCATENATE 'm_gjahr EQ^''' ztfi_warranty-m_gjahr '''' INTO wa_tab.
    REPLACE '^' WITH ' ' INTO wa_tab.
    APPEND wa_tab TO t_m_gjahr.
  ENDIF.
  IF NOT ztfi_warranty-land1 IS INITIAL.
    CONCATENATE 'land1 EQ^''' ztfi_warranty-land1 '''' INTO wa_tab.
    REPLACE '^' WITH ' ' INTO wa_tab.
    APPEND wa_tab TO t_land1.
  ENDIF.

  SELECT * FROM ztfi_warranty INTO TABLE it_source
                 WHERE (t_land1)
                   AND (t_model)
                   AND (t_m_gjahr)
                   AND (t_versn) .

* BEGIN HIS20094
  CLEAR ztfi_warranty_h.
  SELECT SINGLE * FROM ztfi_warranty_h
                 WHERE (t_versn).

  SORT it_source.
  it_source_tmp[] = it_source[].

  IF it_source[] IS INITIAL.
    MESSAGE i000(zz) WITH 'No data found'.
  ENDIF.
* END HIS20094

*  IF NOT ztfi_warranty-versn IS INITIAL AND
*         ztfi_warranty-model IS INITIAL AND
*         ztfi_warranty-m_gjahr IS INITIAL AND
*         ztfi_warranty-land1 IS INITIAL.
*
*    SELECT * FROM ztfi_warranty INTO TABLE it_source
*           WHERE  versn EQ ztfi_warranty-versn.
*
*  ELSEIF NOT ztfi_warranty-versn   IS INITIAL AND
*         NOT ztfi_warranty-model   IS INITIAL AND
*             ztfi_warranty-m_gjahr IS INITIAL AND
*             ztfi_warranty-land1   IS INITIAL.
*
*    SELECT * FROM ztfi_warranty INTO TABLE it_source
*      WHERE  versn EQ ztfi_warranty-versn AND
*             model EQ ztfi_warranty-model.
*
*  ELSEIF NOT ztfi_warranty-versn   IS INITIAL AND
*         NOT ztfi_warranty-model   IS INITIAL AND
*         NOT ztfi_warranty-m_gjahr IS INITIAL AND
*             ztfi_warranty-land1   IS INITIAL.
*    SELECT * FROM ztfi_warranty INTO TABLE it_source
*     WHERE  versn EQ ztfi_warranty-versn AND
*         model EQ ztfi_warranty-model AND
*         m_gjahr EQ ztfi_warranty-m_gjahr.
*  ELSEIF NOT ztfi_warranty-versn   IS INITIAL AND
*             ztfi_warranty-model   IS INITIAL AND
*         NOT ztfi_warranty-m_gjahr IS INITIAL AND
*             ztfi_warranty-land1   IS INITIAL.
*    SELECT * FROM ztfi_warranty INTO TABLE it_source
*           WHERE  versn EQ ztfi_warranty-versn AND
*                  m_gjahr EQ ztfi_warranty-m_gjahr.
*
*  ELSE.
*
*    SELECT * FROM ztfi_warranty INTO TABLE it_source
*                   WHERE land1   EQ ztfi_warranty-land1   AND
*                         model   EQ ztfi_warranty-model   AND
*                         m_gjahr EQ ztfi_warranty-m_gjahr AND
*                         versn   EQ ztfi_warranty-versn .
*  ENDIF.



* }
ENDFORM.                    " execute_query
*&---------------------------------------------------------------------*
*&      Module  check_chg  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_chg INPUT.
  c_flag = 'X'.
ENDMODULE.                 " check_chg  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_model  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_model INPUT.

  DATA : BEGIN OF it_model OCCURS 0,
           model LIKE ztfi_war_sales-model,
         END OF it_model.
  DATA : l_cnt(5) TYPE n.


  SELECT DISTINCT model INTO CORRESPONDING FIELDS OF TABLE it_model
   FROM ztfi_war_sales
   WHERE model  <> '' .

* BEGIN OF UD1K950002 - Hard Code Elantra (TC) Model
  it_model-model = 'TC'.
  append it_model.
  sort it_model.
  delete adjacent duplicates from it_model.
* END OF UD1K950002

  CLEAR : g_value, g_list[].
*  clear l_cnt.
  LOOP AT it_model.
    CLEAR: g_value-key, g_value-text.
*    l_cnt = l_cnt + 1.
    g_value-key  = it_model-model.
    g_value-text = it_model-model.
    CONDENSE g_value-text NO-GAPS.
    APPEND g_value TO g_list.
  ENDLOOP.

  g_name = 'ZTFI_WARRANTY-MODEL'.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = g_name
            values = g_list.

ENDMODULE.                 " help_model  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_land1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_land1 INPUT.
  DATA : BEGIN OF it_land1 OCCURS 0,
           land1 LIKE ztfi_war_sales-land1,
         END OF it_land1.

  SELECT DISTINCT land1 INTO CORRESPONDING FIELDS OF TABLE it_land1
   FROM ztfi_war_sales
   WHERE land1  <> '' .

  CLEAR : g_value, g_list[].
  LOOP AT it_land1.
    CLEAR: g_value-key, g_value-text.
    g_value-key  = it_land1-land1.
    g_value-text = it_land1-land1.
    CONDENSE g_value-text NO-GAPS.
    APPEND g_value TO g_list.
  ENDLOOP.

  g_name = 'ZTFI_WARRANTY-LAND1'.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = g_name
            values = g_list.

ENDMODULE.                 " help_land1  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_gjahr  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_gjahr INPUT.
  DATA : BEGIN OF it_gjahr OCCURS 0,
            m_gjahr LIKE ztfi_war_sales-m_gjahr,
          END OF it_gjahr.

  SELECT DISTINCT m_gjahr
   INTO CORRESPONDING FIELDS OF TABLE it_gjahr
   FROM ztfi_war_sales
   WHERE m_gjahr  <> '' .

  CLEAR : g_value, g_list[].
  LOOP AT it_gjahr.
    CLEAR: g_value-key, g_value-text.
    g_value-key  = it_gjahr-m_gjahr.
    g_value-text = it_gjahr-m_gjahr.
    CONDENSE g_value-text NO-GAPS.
    APPEND g_value TO g_list.
  ENDLOOP.

  g_name = 'ZTFI_WARRANTY-M_GJAHR'.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = g_name
            values = g_list.

ENDMODULE.                 " help_gjahr  INPUT

*&---------------------------------------------------------------------*
*&      Module  help_versn  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_versn INPUT.
  DATA : BEGIN OF it_versn OCCURS 0,
            versn LIKE ztfi_warranty-versn,
          END OF it_versn.

  SELECT DISTINCT versn
   INTO CORRESPONDING FIELDS OF TABLE it_versn
   FROM ztfi_warranty
   WHERE versn  <> '' .

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'ZTFI_WARRANTY-VERSN'
            dynpprog        = sy-cprog
            dynpnr          = sy-dynnr
            dynprofield     = 'ZTFI_WARRANTY-VERSN'
            value_org       = 'S'
       TABLES
            value_tab       = it_versn
       EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.                 " help_versn  INPUT
