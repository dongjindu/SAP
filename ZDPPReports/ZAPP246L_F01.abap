*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_dropdown_list_box_app246.
* Plant
  CLEAR: plant_list, plant_list[],
         plant_value.
  name = 'P_PLANT_APP246'.
  PERFORM set_field_plant_app246.
  PERFORM call_function_vrm_app246 USING plant_list.
* Model
  CLEAR: model_list, model_list[],
         model_value.
  name = 'P_MODEL_APP246'.
  PERFORM set_field_model_app246.
  PERFORM call_function_vrm_app246 USING model_list.
* Body Serial "P_BODY_SERIAL
  CLEAR: body_ser_list, body_ser_list[],
         body_ser_value.
  name = 'P_BODYNO_APP246'.
  PERFORM set_field_body_ser_app246.
  PERFORM call_function_vrm_app246 USING body_ser_list.
* Line
  CLEAR: line_list, line_list[],
         line_value.
  name = 'P_LINE_APP246'.
  PERFORM set_field_line_app246.
  PERFORM call_function_vrm_app246 USING line_list.
* Status
  CLEAR: status_list, status_list[],
         status_value.
  name = 'P_STATUS_APP246'.
  PERFORM set_field_status_app246.
  PERFORM call_function_vrm_app246 USING status_list.
* Progress
  CLEAR: progress_list, progress_list[],
         progress_value.
  name = 'P_PROG_APP246'.
  PERFORM set_field_prog_app246.
  PERFORM call_function_vrm_app246 USING progress_list.
* Work Order
  CLEAR: wono_list, wono_list[],
         wono_value.
  name = 'P_WONO_APP246'.
  PERFORM set_field_wono_app246.
  PERFORM call_function_vrm_app246 USING wono_list.
* External Color
  CLEAR: extc_list, extc_list[],
         extc_value.
  name = 'P_EXTC_APP246'.
  PERFORM set_field_extc_app246.
  PERFORM call_function_vrm_app246 USING extc_list.
* Internal Color
  CLEAR: intc_list, intc_list[],
         intc_value.
  name = 'P_INTC_APP246'.
  PERFORM set_field_intc_app246.
  PERFORM call_function_vrm_app246 USING intc_list.
* Columnes
  PERFORM set_columns_app246 .

ENDFORM.                    " make_dropdown_list_box_APP246
*&---------------------------------------------------------------------*
*&      Form  call_function_vrm_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PLANT_APP246_LIST  text
*----------------------------------------------------------------------*
FORM call_function_vrm_app246 USING    p_list.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = p_list.

ENDFORM.                    " call_function_vrm_APP246
*&---------------------------------------------------------------------*
*&      Form  set_field_plant_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_plant_app246.
  SELECT DISTINCT au~atwrt
    INTO plant_value-key
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE au~klart = '002' AND
          ca~atnam = 'P_TRIM_PLANT_NO' .
    CONCATENATE plant_value-key ' ''s Plant'
      INTO plant_value-text .
    APPEND plant_value TO plant_list.

  ENDSELECT.

ENDFORM.                    " set_field_plant_APP246
*&---------------------------------------------------------------------*
*&      Form  set_field_model_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_model_app246.
  SELECT DISTINCT au~atwrt
    INTO model_value-key
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE au~klart = '002' AND
          ca~atnam = 'P_MODEL' .
    APPEND model_value TO model_list.

  ENDSELECT.

ENDFORM.                    " set_field_model_APP246
*&---------------------------------------------------------------------*
*&      Form  set_field_line_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_line_app246.
  SELECT DISTINCT au~atwrt
    INTO line_value-key
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE au~klart = '002' AND
          ca~atnam = 'P_TRIM_LINE_NO' .
    APPEND line_value TO line_list.
  ENDSELECT.

ENDFORM.                    " set_field_line_APP246
*&---------------------------------------------------------------------*
*&      Form  set_field_PROG_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_prog_app246.
* For Testing
  progress_value-key = '26'.
  progress_value-text = 'Test-26'.
  APPEND progress_value TO progress_list.

  progress_value-key = '00'.
  progress_value-text = 'Test-00'.
  APPEND progress_value TO progress_list.

* B/IN(01)
  progress_value-key  = '01'.
  progress_value-text = 'B/IN'.
  APPEND progress_value TO progress_list.
* P/IN(02)
  progress_value-key  = '02'.
  progress_value-text = 'P/IN'.
  APPEND progress_value TO progress_list.
* T/C(03)
  progress_value-key  = '03'.
  progress_value-text = 'T/C'.
  APPEND progress_value TO progress_list.
* P/OUT(04)
  progress_value-key  = '04'.
  progress_value-text = 'P/OUT'.
  APPEND progress_value TO progress_list.
* PBS/I(05)
  progress_value-key  = '05'.
  progress_value-text = 'PBS/I'.
  APPEND progress_value TO progress_list.
* T/IN(07)
  progress_value-key  = '07'.
  progress_value-text = 'T/IN'.
  APPEND progress_value TO progress_list.
* C/F(17)
  progress_value-key  = '17'.
  progress_value-text = 'C/F'.
  APPEND progress_value TO progress_list.
* S/OFF(18)
  progress_value-key  = '18'.
  progress_value-text = 'S/OFF'.
  APPEND progress_value TO progress_list.
* C/GATE(19)
  progress_value-key  = '19'.
  progress_value-text = 'C/GATE'.
  APPEND progress_value TO progress_list.
* VPC/I(21)
  progress_value-key  = '21'.
  progress_value-text = 'VPC/I'.
  APPEND progress_value TO progress_list.
* VPC/O(22)
  progress_value-key  = '22'.
  progress_value-text = 'VPC/O'.
  APPEND progress_value TO progress_list.

ENDFORM.                    " set_field_PROG_APP246
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_COLUMN_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_column_app246 USING p_list  LIKE column01_list[]
                                   p_value LIKE column01_value .
  DATA: l_count(03).
  DO 219 TIMES.
    p_value-key = l_count = l_count + 1.
    CONDENSE p_value-key .
    APPEND p_value TO p_list .
  ENDDO.

ENDFORM.                    " SET_FIELD_COLUMN_APP246
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_WONO_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_wono_app246.
  SELECT DISTINCT au~atwrt
    INTO wono_value-key
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE au~klart = '002' AND
          ca~atnam = 'P_WORK_ORDER' .
    APPEND wono_value TO wono_list.
  ENDSELECT.

ENDFORM.                    " SET_FIELD_WONO_APP246
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_EXTC_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_extc_app246.
  SELECT DISTINCT au~atwrt
    INTO extc_value-key
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE au~klart = '002' AND
          ca~atnam = 'P_EXT_COLOR' .
    CLEAR extc_list.
    APPEND extc_value TO extc_list.
  ENDSELECT.

ENDFORM.                    " SET_FIELD_EXTC_APP246
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INTC_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_intc_app246.
  SELECT DISTINCT au~atwrt
    INTO intc_value-key
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE au~klart = '002' AND
          ca~atnam = 'P_INT_COLOR' .
    APPEND intc_value TO intc_list.
  ENDSELECT.

ENDFORM.                    " SET_FIELD_INTC_APP246
*&---------------------------------------------------------------------*
*&      Form  search_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_sum_data_app246.
  DATA: l_error ,
        l_text(50) .
  CLEAR l_error.
  PERFORM set_parameter_for_srchng_data USING l_error l_text.
  IF l_error <> space.
    CONCATENATE 'Enter The Necessary Parameters!!! -' l_text
      INTO l_text.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.
  CLEAR: it_objek, it_objek[], it_sum_app246, it_sum_app246[].
  PERFORM get_vehicle_master_no_app246 TABLES it_objek.
  PERFORM create_data_for_sum_app246 .
ENDFORM.                    " search_data
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_parameter_for_srchng_data USING p_error p_text .
* Plant
  IF p_plant_app246 <> space.
  ELSE.
  ENDIF.

* Model
  IF p_model_app246 <> space.
  ELSE.
  ENDIF.

* Line
  IF p_line_app246 <> space.
  ELSE.
  ENDIF.

* Progress
  IF p_prog_app246 <> space.
  ELSE.
    p_error = 'X'.
    p_text = 'Progress'.
    EXIT.
  ENDIF.

* Work Order
  IF p_wono_app246 <> space.
  ELSE.

  ENDIF.

* External Color
  IF p_extc_app246 <> space.
  ELSE.
  ENDIF.

* Internal Color
  IF p_intc_app246 <> space.
  ELSE.
  ENDIF.



ENDFORM.                    " SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK  text
*----------------------------------------------------------------------*
FORM get_vehicle_master_no_app246 TABLES p_it_objek STRUCTURE it_objek .
  DATA: l_subrc    TYPE sy-subrc ,
        l_atnam    TYPE cabn-atnam,
        l_atwrt    TYPE ausp-atwrt,
        l_atflv_st TYPE ausp-atflv,
        l_temp(06),
        l_datum    TYPE sy-datum,
        l_atflv_en TYPE ausp-atflv,
        l_num(08) TYPE n.
* r_prog FOR P_PROG_APP246,       "P_RP_STATUS
  SELECT DISTINCT objek
    INTO it_objek-objek
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE klart = '002'           AND
          au~atwrt = p_prog_app246       AND
          ca~atnam = 'P_RP_STATUS'  .
    IF sy-subrc = 0  .
**    p_model,     "P_MODEL
      IF p_model_app246 <> space.
        CLEAR l_subrc .
        MOVE p_model_app246 TO l_atwrt .
        PERFORM check_data_of_vm USING it_objek-objek
                                       'P_MODEL'
                                       l_atwrt
                                 CHANGING l_subrc .
        IF l_subrc <> 0 .
          CONTINUE.
        ENDIF.
      ENDIF.
**    P_BODYNO_APP246,     "P_BODY_SERIAL
      IF p_bodyno_app246 <> space.
        CLEAR l_subrc .
        MOVE p_bodyno_app246 TO l_atwrt.
        PERFORM check_data_of_vm USING it_objek-objek
                                       'P_BODY_SERIAL'
                                       l_atwrt
                                 CHANGING l_subrc.
        IF l_subrc <> 0.
          CONTINUE .
        ENDIF.
      ENDIF.
**    P_WONO_APP246,       "P_WORK_ORDER
      IF p_wono_app246 <> space.
        CLEAR l_subrc .
        MOVE p_wono_app246 TO l_atwrt .
        PERFORM check_data_of_vm USING    it_objek-objek
                                        'P_WORK_ORDER'
                                        l_atwrt
                               CHANGING l_subrc .
        IF l_subrc <> 0 .
          CONTINUE.
        ENDIF.
      ENDIF.
**    P_EXTC_APP246,       "P_EXT_COLOR
      IF p_extc_app246 <> space.
        CLEAR l_subrc .
        MOVE p_extc_app246 TO l_atwrt .
        PERFORM check_data_of_vm USING it_objek-objek
                                       'P_EXT_COLOR'
                                       l_atwrt
                                 CHANGING l_subrc .
        IF l_subrc <> 0 .
          CONTINUE.
        ENDIF.
      ENDIF.
**    P_INTC_APP246.       "P_INT_COLOR
      IF p_intc_app246 <> space.
        CLEAR l_subrc .
        MOVE p_intc_app246 TO l_atwrt .
        PERFORM check_data_of_vm USING it_objek-objek
                                       'P_INT_COLOR'
                                       l_atwrt
                                 CHANGING l_subrc .
        IF l_subrc <> 0 .
          CONTINUE.
        ENDIF.
      ENDIF.
**    p_column01 ~ 10  "P_219_xxx
      PERFORM check_219_code USING    it_objek-objek
                             CHANGING l_subrc .
      IF l_subrc <> 0.
        CONTINUE.
      ENDIF.
      APPEND it_objek.
    ENDIF.
  ENDSELECT.
  SORT it_objek BY objek .
ENDFORM.                    " get_vehicle_master_no
*&---------------------------------------------------------------------*
*&      Form  create_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_data_for_sum_app246.
  DATA: l_rpno(02) TYPE n          ,
        l_atnam    TYPE cabn-atnam ,
        l_atwrt    TYPE ausp-atwrt .
  CLEAR: it_sum_app246, it_sum_app246[].
  LOOP AT it_objek.
    CLEAR it_sum_app246.
*   V/M No.
    MOVE-CORRESPONDING it_objek TO it_sum_app246.
*   Model
    PERFORM read_normal_classification USING it_sum_app246-objek
                                             'P_MODEL'
                                       CHANGING it_sum_app246-model .
*   bodyno TYPE ausp-atwrt, "P_MODEL & P_BODY_SERIAL(09)
    PERFORM read_normal_classification USING it_sum_app246-objek
                                             'P_BODY_SERIAL'
                                       CHANGING it_sum_app246-bodyno .
    CONCATENATE it_sum_app246-model it_sum_app246-bodyno
      INTO it_sum_app246-bodyno .
*   Work Order(Serial)
    PERFORM read_normal_classification USING    it_sum_app246-objek
                                                'P_WORK_ORDER'
                                       CHANGING it_sum_app246-wono.
*   External Color
    PERFORM read_normal_classification USING    it_sum_app246-objek
                                                'P_EXT_COLOR'
                                        CHANGING it_sum_app246-extc.
*   Internal Color
    PERFORM read_normal_classification USING    it_sum_app246-objek
                                                'P_INT_COLOR'
                                       CHANGING it_sum_app246-intc.
**  Date : B/In
    l_atnam = 'P_RP01_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-bin.
**  Date : P/In
    l_atnam = 'P_RP02_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-pin.
**  Date : T/C
    l_atnam = 'P_RP03_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-tc.
**  Date : P/OUT
    l_atnam = 'P_RP04_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-pout.
**  Date : PBS/I
    l_atnam = 'P_RP05_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-pbsi.
**  Date : T/IN
    l_atnam = 'P_RP07_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-tin.
**  Date : C/F
    l_atnam = 'P_RP17_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-cf.
**  Date : S/OFF
    l_atnam = 'P_RP18_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-soff.
**  Date : C/GATE
    l_atnam = 'P_RP19_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-control.
**  Date : VPC/I
    l_atnam = 'P_RP21_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-pdii.
**  Date : VPC/O
    l_atnam = 'P_RP22_SHOP_DATE'.
    PERFORM read_shop_date USING    it_sum_app246-objek
                                    l_atnam
                           CHANGING it_sum_app246-pdio.
**  Date : M/P  --> Not Defined

    APPEND it_sum_app246.
*
  ENDLOOP.
  SORT it_sum_app246 BY bodyno .
  DESCRIBE TABLE it_sum_app246 LINES p_total_app246.
ENDFORM.                    " create_data
*&---------------------------------------------------------------------*
*&      Form  read_normal_classification
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_APP246_OBJEK  text
*      -->P_1077   text
*      <--P_IT_APP246_MI  text
*----------------------------------------------------------------------*
FORM read_normal_classification USING    p_vmno
                                         p_char
                                CHANGING p_value.
  SELECT SINGLE au~atwrt
    INTO p_value
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_vmno      AND
          klart = '002'       AND
          ca~atnam = p_char  .

ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  check_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_0827   text
*      -->P_P_MODEL  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM check_data_of_vm USING    p_vmno
                               p_char
                               p_value
                      CHANGING p_subrc.
  SELECT SINGLE objek
    INTO it_objek-objek
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_vmno         AND
          klart = '002'          AND
          au~atwrt = p_value     AND
          ca~atnam = p_char      .
  p_subrc = sy-subrc.
ENDFORM.                    " check_data_of_vm
*&---------------------------------------------------------------------*
*&      Form  check_219_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM check_219_code USING    p_objek
                    CHANGING p_subrc.
  DATA: lc_column(20) TYPE c,
        lc_value(20)  TYPE c,
        lc_num(02)    TYPE n ,
        l_atwrt       TYPE ausp-atwrt,
        l_atnam       TYPE cabn-atnam.
  FIELD-SYMBOLS: <fs_column> ,
                 <fs_value>  .

  DO 10 TIMES.
    CLEAR p_subrc.
*   Defining Column
    lc_num = lc_num + 1 .
    CONCATENATE 'P_COLUMN' lc_num '_APP246'
      INTO lc_column.
    ASSIGN (lc_column) TO <fs_column> .
*   Defining 219 Code Name         "ATNAM
    WRITE <fs_column> TO l_atnam LEFT-JUSTIFIED.
    CONCATENATE 'P_219_' l_atnam
      INTO l_atnam .
*   Defining 219 Code's Value      "ATWRT
    CONCATENATE 'P_VALUE' lc_num '_APP246'
      INTO lc_value.
    ASSIGN (lc_value) TO <fs_value> .
    MOVE <fs_value> TO l_atwrt .
*
    IF <fs_column> =  space  .
      CONTINUE .
    ENDIF.
*
    PERFORM check_data_of_vm USING it_objek-objek
                                   l_atnam
                                   l_atwrt
                             CHANGING p_subrc.
    IF p_subrc <> 0.
      EXIT.
    ENDIF.

  ENDDO.


ENDFORM.                    " check_219_code
*&---------------------------------------------------------------------*
*&      Form  set_columnes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_columns_app246.
* Column01
  CLEAR: column01_list, column01_list[],
         column01_value.
  name = 'P_COLUMN01_APP246'.
  PERFORM set_field_column_app246 USING column01_list
                                        column01_value.
  PERFORM call_function_vrm_app246 USING column01_list.
* COLUMN02
  CLEAR: column02_list, column02_list[],
         column02_value.
  name = 'P_COLUMN02_APP246'.
  PERFORM set_field_column_app246 USING column02_list
                                        column02_value.
  PERFORM call_function_vrm_app246 USING column02_list.
* COLUMN03
  CLEAR: column03_list, column03_list[],
         column03_value.
  name = 'P_COLUMN03_APP246'.
  PERFORM set_field_column_app246 USING column03_list
                                        column03_value.
  PERFORM call_function_vrm_app246 USING column03_list.
* COLUMN04
  CLEAR: column04_list, column04_list[],
         column04_value.
  name = 'P_COLUMN04_APP246'.
  PERFORM set_field_column_app246 USING column04_list
                                        column04_value.
  PERFORM call_function_vrm_app246 USING column04_list.
* COLUMN05
  CLEAR: column05_list, column05_list[],
         column05_value.
  name = 'P_COLUMN05_APP246'.
  PERFORM set_field_column_app246 USING column05_list
                                        column05_value.
  PERFORM call_function_vrm_app246 USING column05_list.
* Column06
  CLEAR: column06_list, column06_list[],
         column06_value.
  name = 'P_COLUMN06_APP246'.
  PERFORM set_field_column_app246 USING column06_list
                                        column06_value.
  PERFORM call_function_vrm_app246 USING column06_list.
* Column07
  CLEAR: column07_list, column07_list[],
         column07_value.
  name = 'P_COLUMN07_APP246'.
  PERFORM set_field_column_app246 USING column07_list
                                        column07_value.
  PERFORM call_function_vrm_app246 USING column07_list.
* Column08
  CLEAR: column08_list, column08_list[],
         column08_value.
  name = 'P_COLUMN08_APP246'.
  PERFORM set_field_column_app246 USING column08_list
                                        column08_value.
  PERFORM call_function_vrm_app246 USING column08_list.
* Column09
  CLEAR: column09_list, column09_list[],
         column09_value.
  name = 'P_COLUMN09_APP246'.
  PERFORM set_field_column_app246 USING column09_list
                                        column09_value.
  PERFORM call_function_vrm_app246 USING column09_list.
* Column10
  CLEAR: column10_list, column10_list[],
         column10_value.
  name = 'P_COLUMN10_APP246'.
  PERFORM set_field_column_app246 USING column10_list
                                        column10_value.
  PERFORM call_function_vrm_app246 USING column10_list.

ENDFORM.                    " set_columnes
*&---------------------------------------------------------------------*
*&      Form  set_field_body_ser_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_body_ser_app246.
  SELECT DISTINCT au~atwrt
    INTO body_ser_value-key
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE au~klart = '002' AND
          ca~atnam = 'P_BODY_SERIAL' .
    APPEND body_ser_value TO body_ser_list.

  ENDSELECT.

ENDFORM.                    " set_field_body_ser_APP246
*&---------------------------------------------------------------------*
*&      Form  check_from_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_L_ATNAM  text
*      -->P_L_ATWRT  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM check_from_data_of_vm USING    p_objek
                                    p_atnam
                                    p_atwrt
                           CHANGING p_subrc.
  SELECT SINGLE objek
    INTO it_objek-objek
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_objek         AND
          klart = '002'           AND
          au~atwrt =  p_atwrt     AND
          ca~atnam >= p_atnam      .
  p_subrc = sy-subrc.

ENDFORM.                    " check_from_data_of_vm
*&---------------------------------------------------------------------*
*&      Form  check_to_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_L_ATNAM  text
*      -->P_L_ATWRT  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM check_to_data_of_vm USING    p_objek
                                  p_atnam
                                  p_atwrt
                         CHANGING p_subrc.
  SELECT SINGLE objek
    INTO it_objek-objek
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_objek         AND
          klart = '002'           AND
          au~atwrt =  p_atwrt     AND
          ca~atnam <= p_atnam      .
  p_subrc = sy-subrc.

ENDFORM.                    " check_to_data_of_vm
*&---------------------------------------------------------------------*
*&      Form  set_field_status_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_status_app246.
  status_value-key = 'S'.
  status_value-text = 'Summary'.
  APPEND status_value TO status_list.

  status_value-key = 'D'.
  status_value-text = 'Detail'.
  APPEND status_value TO status_list.

ENDFORM.                    " set_field_status_app246
*&---------------------------------------------------------------------*
*&      Form  build_variant_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_variant_app246.
  gs_variant-report = sy-repid.
ENDFORM.                    " build_variant_APP246
*&---------------------------------------------------------------------*
*&      Form  build_layout_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout_app246.
  gs_layout-zebra  = 'X'.       "ZEBRA
  gs_layout-cwidth_opt = 'X'.   "OPTIMIZE COLUMN WIDTH
  gs_layout-detailinit = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN
ENDFORM.                    " build_layout_APP246
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat_app246.
  DATA: l_struct    LIKE dd02l-tabname.


  DATA: zero_fname1(20),
        zero_fname2(20),
        zero_cnt TYPE i.
  IF p_status_app246 = 'D'.
    l_struct = 'ZSPP_DET_APP246'.
  ELSE.
    l_struct = 'ZSPP_SUM_APP246'.
  ENDIF.
  CLEAR : wa_fieldcat, gt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_buffer_active        = 'X'
            i_structure_name       = l_struct
       CHANGING
            ct_fieldcat            = gt_fieldcat[]
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.

*  DELETE GT_FIELDCAT  WHERE FIELDNAME = ' ' OR
*                            FIELDNAME = ' ' OR
*                            FIELDNAME = ' ' OR
*                            FIELDNAME = ' ' OR
*                            FIELDNAME = ' ' OR
*                            FIELDNAME = ' '.

  LOOP AT gt_fieldcat INTO wa_fieldcat.
    IF p_status_app246 = 'D'.
      PERFORM set_field_info_det_app246 USING wa_fieldcat.
    ELSE.
      PERFORM set_field_info_sum_app246 USING wa_fieldcat.
    ENDIF.
    MODIFY gt_fieldcat FROM wa_fieldcat.
    CLEAR wa_fieldcat.
  ENDLOOP.
ENDFORM.                    " build_fieldcat_APP246
*&---------------------------------------------------------------------*
*&      Form  set_field_info_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FIELDCAT  text
*----------------------------------------------------------------------*
FORM set_field_info_sum_app246 USING    l_fieldcat STRUCTURE lvc_s_fcat.
  CASE l_fieldcat-fieldname.
    WHEN 'BODYNO'.
      set_fieldcat  'BODY NO' 10.
      l_fieldcat-key = 'X'.
    WHEN 'WONO'.
      set_fieldcat 'Order No.' 20.
    WHEN 'EXTC'.
      set_fieldcat 'Ext.C' 20.
*      l_fieldcat-key = 'X'.
    WHEN 'INTC'.
      set_fieldcat 'Int.C' 20.
    WHEN 'BIN'.
      set_fieldcat 'B/In' 20.
    WHEN 'PIN'.
      set_fieldcat 'P/In'  20.
    WHEN 'TC'.
      set_fieldcat 'T/C'  20.
    WHEN 'POUT'.
      set_fieldcat 'P/Out' 20.
    WHEN 'PBSI'.
      set_fieldcat 'PBS/I' 20.
    WHEN 'TIN'.
      set_fieldcat 'T/In' 20.
    WHEN 'CF'.
      set_fieldcat 'C/F'  20.
    WHEN 'SOFF'.
      set_fieldcat 'S/Off' 20.
    WHEN 'CONTROL'.
      set_fieldcat 'C/Gate' 20.
    WHEN 'PDII'.
      set_fieldcat 'PDI/I' 20.
    WHEN 'PDIO'.
      set_fieldcat 'PDI/O' 20.
    WHEN 'MP'.
      set_fieldcat 'M/P' 20.
  ENDCASE.
ENDFORM.                    " set_field_info_app246
*&---------------------------------------------------------------------*
*&      Form  set_data_for_sum_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_data_for_sum_app246.
  PERFORM search_sum_data_app246.
ENDFORM.                    " set_data_for_sum_app246
*&---------------------------------------------------------------------*
*&      Form  set_data_for_det_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_data_for_det_app246.
  PERFORM search_detail_data_app246.
ENDFORM.                    " set_data_for_det_app246
*&---------------------------------------------------------------------*
*&      Form  read_shop_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUM_APP246_OBJEK  text
*      -->P_L_ATNAM  text
*      <--P_IT_TEMP_APP246_PIN  text
*----------------------------------------------------------------------*
FORM read_shop_date USING    p_objek
                             p_atnam
                    CHANGING p_date. "p_atflv.
  DATA: l_atflv   TYPE ausp-atflv,
        l_num(08) TYPE n         .
  SELECT SINGLE au~atflv
    INTO l_atflv
    FROM ausp AS au
      INNER JOIN cabn AS ca ON ca~atinn = au~atinn
    WHERE au~objek =  p_objek     AND
          au~klart =  '002'       AND
          ca~atnam =  p_atnam       .

  p_date = l_num = l_atflv .
ENDFORM.                    " read_shop_date
*&---------------------------------------------------------------------*
*&      Form  search_detail_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_detail_data_app246.
  DATA: l_error ,
        l_text(50) .
  CLEAR l_error.
  PERFORM set_parameter_for_srchng_data USING l_error l_text.
  IF l_error <> space.
    CONCATENATE 'Enter The Necessary Parameters!!! -' l_text
      INTO l_text.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.
  CLEAR: it_objek, it_objek[], it_det_app246, it_det_app246[].
  PERFORM get_vehicle_master_no_app246 TABLES it_objek.
  PERFORM create_data_for_det_app246 .

ENDFORM.                    " search_detail_data
*&---------------------------------------------------------------------*
*&      Form  create_data_for_det
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_data_for_det_app246.
  DATA: l_rpno(02) TYPE n          ,
        l_atnam    TYPE cabn-atnam ,
        l_atwrt    TYPE ausp-atwrt .
  CLEAR: it_det_app246, it_det_app246[].
  LOOP AT it_objek.
    CLEAR it_det_app246.
*   V/M No.
    MOVE-CORRESPONDING it_objek TO it_det_app246.
*   Model
    PERFORM read_normal_classification USING it_det_app246-objek
                                             'P_MODEL'
                                       CHANGING it_det_app246-model .
*   bodyno TYPE ausp-atwrt, "P_MODEL & P_BODY_SERIAL(09)
    PERFORM read_normal_classification USING it_det_app246-objek
                                             'P_BODY_SERIAL'
                                       CHANGING it_det_app246-bodyno .
    CONCATENATE it_det_app246-model it_det_app246-bodyno
      INTO it_det_app246-bodyno .
*   Work Order(Serial)
    PERFORM read_normal_classification USING    it_det_app246-objek
                                                'P_WORK_ORDER'
                                       CHANGING it_det_app246-wono.
*   External Color
    PERFORM read_normal_classification USING    it_det_app246-objek
                                                'P_EXT_COLOR'
                                        CHANGING it_det_app246-extc.
*   Internal Color
    PERFORM read_normal_classification USING    it_det_app246-objek
                                                'P_INT_COLOR'
                                       CHANGING it_det_app246-intc.
*   VIN
    PERFORM read_normal_classification USING    it_det_app246-objek
                                                'P_VIN'
                                       CHANGING it_det_app246-vin.
*   Vendor : Not Defined
*   MI
    PERFORM read_normal_classification USING    it_det_app246-objek
                                                'P_MI'
                                       CHANGING it_det_app246-mi.
*   OCN
    PERFORM read_normal_classification USING    it_det_app246-objek
                                                'P_OCN'
                                       CHANGING it_det_app246-ocn.
*   Reporting Date
*   Reporting Time
    l_rpno = p_prog_app246 .
    CONCATENATE 'P_RP' l_rpno '_ACTUAL_DATE'
      INTO l_atnam .
    PERFORM read_normal_classification USING    it_det_app246-objek
                                                l_atnam
                                       CHANGING l_atwrt .
    it_det_app246-rep_date = l_atwrt+00(08).
    it_det_app246-rep_time = l_atwrt+08(06).
*   Serial
    l_rpno = p_prog_app246 .
    CONCATENATE 'P_RP' l_rpno '_SERIAL'
      INTO l_atnam.
    PERFORM read_normal_classification USING    it_det_app246-objek
                                                l_atnam
                                       CHANGING it_det_app246-serial .
*
    APPEND it_det_app246.
*
  ENDLOOP.
  SORT it_det_app246 BY bodyno .
  DESCRIBE TABLE it_det_app246 LINES p_total_app246.
ENDFORM.                    " create_data_for_det
*&---------------------------------------------------------------------*
*&      Form  set_field_info_det_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FIELDCAT  text
*----------------------------------------------------------------------*
FORM set_field_info_det_app246 USING    l_fieldcat STRUCTURE lvc_s_fcat.
  CASE l_fieldcat-fieldname.
    WHEN 'BODYNO'.
      set_fieldcat  'BODY NO' 10.
      l_fieldcat-key = 'X'.
    WHEN 'VIN'.
      set_fieldcat 'VIN' 20.
    WHEN 'WONO'.
      set_fieldcat 'Order No' 20.
*      l_fieldcat-key = 'X'.
    WHEN 'EXTC'.
      set_fieldcat 'Ext.C' 20.
    WHEN 'INTC'.
      set_fieldcat 'Int.C' 20.
    WHEN 'VENDOR'.
      set_fieldcat 'Vendor'  20.
    WHEN 'OCN'.
      set_fieldcat 'OCN'  20.
    WHEN 'REP_DATE'.
      set_fieldcat 'Reporting Date' 20.
    WHEN 'REP_TIME'.
      set_fieldcat 'Reporting Time' 20.
    WHEN 'SERIAL'.
      set_fieldcat 'Serial' 20.
  ENDCASE.
ENDFORM.                    " set_field_info_det_app246
*&---------------------------------------------------------------------*
*&      Form  call_methord_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_method_det_app246.
  DATA: l_struct    LIKE dd02l-tabname.

  l_struct = 'ZSPP_DET_APP246'.
*-----> SET OBJECT
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      i_structure_name              = l_struct
      is_variant                    = gs_variant
      i_save                        = 'A'
*        I_DEFAULT                     = 'X'
      is_layout                     = gs_layout
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      it_outtab                     = it_det_app246[]
      it_fieldcatalog               = gt_fieldcat[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4
          .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " call_methord_app246
*&---------------------------------------------------------------------*
*&      Form  call_method_sum_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_method_sum_app246.
  DATA: l_struct    LIKE dd02l-tabname.

  l_struct = 'ZSPP_SUM_APP246'.
*-----> SET OBJECT
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      i_structure_name              = l_struct
      is_variant                    = gs_variant
      i_save                        = 'A'
*        I_DEFAULT                     = 'X'
      is_layout                     = gs_layout
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      it_outtab                     = it_sum_app246[]
      it_fieldcatalog               = gt_fieldcat[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4
          .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " call_method_sum_app246
*&---------------------------------------------------------------------*
*&      Form  check_and_read_data_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_and_read_data_app246.
  wa_alv_called = 'X'.
  CASE p_status_app246.
    WHEN 'S'.  "Summary
      PERFORM set_data_for_sum_app246.
    WHEN 'D'.  "Detail
      PERFORM set_data_for_det_app246.
  ENDCASE.

ENDFORM.                    " check_and_read_data_app246
*&---------------------------------------------------------------------*
*&      Form  get_parameter_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_parameter_app246.
  DATA : l_type LIKE sy-repid.
  GET PARAMETER ID 'PID' FIELD l_type.
  IF sy-subrc = 0.
    p_status_app246 = l_type .
  ELSE.
    p_status_app246    = 'S'.
  ENDIF.

ENDFORM.                    " get_parameter_app246
