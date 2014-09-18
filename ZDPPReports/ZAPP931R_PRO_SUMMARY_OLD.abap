************************************************************************
* Program Name      :  ZAPP931R_PRO_SUMMARY .
* Author            :  Won-seob Kim
* Creation Date     :  2004.12.14.
* Specifications By :
* Pattern           :  Report 1-1
* Development Request No :
* Addl Documentation:
* Description       :  Daily Production Summary
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
INCLUDE zapp931r_pro_summary_top.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: "p_batch TYPE c DEFAULT 'X',
            p_plant LIKE ztpp_pro_sum-werks DEFAULT 'P001',
            p_date  LIKE sy-datum DEFAULT sy-datum OBLIGATORY,
            p_dh(2) TYPE c DEFAULT '23'.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM init_date.

AT SELECTION-SCREEN.
  PERFORM initial_date_check.

START-OF-SELECTION.
*HMMA MONTHLY PLAN
  PERFORM get_data_plan.
*Vehicle Actual
  PERFORM get_data_vehicle.
  PERFORM get_vlaue_characteristic.
  PERFORM insert_ztpp_pro_sum.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_vehicle.
*Get data from Vehicle Master
  DATA : p_a07_date_pre(7),
         t_count TYPE i.
  REFRESH :it_condition,it_vehicle,it_vehi.

  p_char_status = 'P_RP_STATUS'.
  p_i_atwrt_s = '01'.
  p_i_atwrt_e = '27'.

  CLEAR it_condition.
  it_condition-atnam = 'P_RP01_SHOP_DATE'.
  APPEND it_condition.CLEAR it_condition.
  it_condition-atnam = 'P_RP02_SHOP_DATE'.
  APPEND it_condition.CLEAR it_condition.
  it_condition-atnam = 'P_RP04_SHOP_DATE'.
  APPEND it_condition.CLEAR it_condition.
  it_condition-atnam = 'P_RP06_SHOP_DATE'.
  APPEND it_condition.CLEAR it_condition.
  it_condition-atnam = 'P_RP07_SHOP_DATE'.
  APPEND it_condition.CLEAR it_condition.
  it_condition-atnam = 'P_RP18_SHOP_DATE'.
  APPEND it_condition.CLEAR it_condition.
  it_condition-atnam = 'P_RP27_SHOP_DATE'.
  APPEND it_condition.CLEAR it_condition.
  it_condition-atnam = 'P_MODEL'.
  APPEND it_condition.CLEAR it_condition.
  it_condition-atnam = 'P_USAGE_CAR'.
  APPEND it_condition.CLEAR it_condition.

  PERFORM get_fuction_cahr_value CHANGING p_flag.

  IF p_flag EQ 'X'.
    SORT it_vehicle BY objek atnam.
    it_vehi[] = it_vehicle[].
*delete already to be signed-off vehicle
    DELETE ADJACENT DUPLICATES FROM it_vehi COMPARING objek.
    LOOP AT it_vehi.
      READ TABLE it_vehicle WITH KEY objek = it_vehi-objek
                                     atnam =  'P_RP27_SHOP_DATE'.
      IF it_vehicle-atwrt <> space.
        DELETE TABLE it_vehi FROM it_vehi.
      ENDIF.
      IF it_vehicle-atwrt EQ 'D' OR it_vehicle-atwrt EQ 'S'.
        DELETE TABLE it_vehi FROM it_vehi.
      ENDIF.
    ENDLOOP.

*** changed by Furong, requested by Ignacio on 09/06/2005
*    LOOP AT it_vehicle WHERE atnam = 'P_USAGE_CAR'.
*      IF it_vehicle-atwrt EQ 'D' OR it_vehicle-atwrt EQ 'S'.
*        MOVE-CORRESPONDING it_vehicle TO it_vehi2.
*        APPEND it_vehi2.
*      ENDIF.
*    ENDLOOP.
*    SORT it_vehi2 BY objek.
*    LOOP AT it_vehi2.
*      DELETE it_vehi WHERE objek = it_vehi2-objek.
*    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  get_characteristic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_characteristic.
  REFRESH it_value.CLEAR it_value.
  it_value-atnam = 'P_MODEL'.          APPEND it_value.
  it_value-atnam = 'P_RP01_SHOP_DATE'. APPEND it_value.
  it_value-atnam = 'P_RP07_SHOP_DATE'. APPEND it_value.
  it_value-atnam = 'P_RP18_SHOP_DATE'. APPEND it_value.

ENDFORM.                    " get_characteristic
*&---------------------------------------------------------------------*
*&      Form  get_vlaue_characteristic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_vlaue_characteristic.
*CONVERSION For date format :
  PERFORM conversion_date USING p_date
                          CHANGING c_date.
*Gathering vehicle value
  PERFORM get_value_vehicle USING c_date.
*Set up internal table
  PERFORM set_up_internal_table.
*Collect internal table
  PERFORM collect_final_internal_table
                         USING c_date
                         CHANGING yy_date.
*Plan & Actual
  PERFORM get_data_actual USING c_date yy_date.
*GET delayed Production
  PERFORM get_delayed_working_time.
*WIP
  PERFORM get_data_wip USING c_date yy_date.
*Modify yesterday & month & today plan
  PERFORM modify_plan_quantity.
ENDFORM.                    " get_vlaue_characteristic
*&---------------------------------------------------------------------*
*&      Form  extract_value_model
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM extract_value_model USING p_atnam p_atwrt c_date .
  CONDENSE p_atwrt.
  CASE p_atnam .
    WHEN 'P_MODEL'.
      MOVE p_atwrt TO it_temp_value-model.
    WHEN 'P_RP01_SHOP_DATE'.
      IF p_atwrt <> space AND p_atwrt EQ c_date.
        it_temp_value-rp01 = 1.
      ENDIF.
    WHEN 'P_RP02_SHOP_DATE'.
      IF p_atwrt <> space AND p_atwrt EQ c_date.
        it_temp_value-rp02 = 1.
      ENDIF.
    WHEN 'P_RP04_SHOP_DATE'.
      IF p_atwrt <> space AND p_atwrt EQ c_date.
        it_temp_value-rp04 = 1.
      ENDIF.
    WHEN 'P_RP06_SHOP_DATE'.
      IF p_atwrt <> space AND p_atwrt EQ c_date.
        it_temp_value-rp06 = 1.
      ENDIF.
    WHEN 'P_RP07_SHOP_DATE'.
      IF p_atwrt <> space AND p_atwrt EQ c_date.
        it_temp_value-rp07 = 1.
      ENDIF.
    WHEN 'P_RP18_SHOP_DATE'.
      IF p_atwrt <> space AND p_atwrt EQ c_date.
        it_temp_value-rp18 = 1.
      ENDIF.
  ENDCASE.

ENDFORM.                    " extract_value_model
*&---------------------------------------------------------------------*
*&      Form  init_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_date.


ENDFORM.                    " init_date
*&---------------------------------------------------------------------*
*&      Form  insert_ztpp_pro_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_ztpp_pro_sum.
  CLEAR w_int.
  SORT it_ztpp_pro BY p_date model.
  DESCRIBE TABLE it_ztpp_pro LINES w_int.
  IF w_int <> 0.
    MODIFY ztpp_pro_sum FROM TABLE it_ztpp_pro.
    IF sy-subrc = 0.
      MESSAGE s001 WITH w_int.
      WRITE : / p_date, w_int, 'Successful Insert'.
      COMMIT WORK.
    ELSE.
      MESSAGE s001 WITH sy-subrc 'Insert error'.
      WRITE : / p_date,sy-subrc, 'Insert error'.
    ENDIF.
  ENDIF.

* REQUESTED BY MY HUR ADDED BY CHRIS
* SAVE THE DELAYED CAR LIST.
  DESCRIBE TABLE it_delay LINES w_int.
  IF w_int NE 0.
    SORT it_delay BY wdate objek.
    DELETE FROM ztpp_delay_car WHERE wdate NE space.
    COMMIT WORK AND WAIT.
    INSERT ztpp_delay_car FROM TABLE it_delay.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      MESSAGE s001 WITH sy-subrc 'Delay car insert error'.
      WRITE : / p_date,sy-subrc, 'Delay car insert error'.
    ENDIF.

  ENDIF.
* END OF ADD
ENDFORM.                    " insert_ztpp_pro_sum
*&---------------------------------------------------------------------*
*&      Form  conversion_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DATE  text
*----------------------------------------------------------------------*
FORM conversion_date USING  p_date
                     CHANGING c_date.
*  DATA : lt_date LIKE lw_date,
*         holiday.
*  CLEAR : w_kalid, w_mosid,w_kapid,lw_date, lw_lines, lw_daynr,
*          lw_dayfree.
**----- Read Shop Calendar ID
*  SELECT SINGLE c~kalid c~mosid b~kapid
*    INTO (w_kalid, w_mosid, w_kapid)
*    FROM crhd AS a INNER JOIN crca AS b
*                      ON a~objty = b~objty
*                     AND a~objid = b~objid
*                   INNER JOIN kako AS c
*                      ON b~kapid = c~kapid
*   WHERE a~werks =  p_plant
*     AND a~arbpl =  'T'
*     AND b~fork2 =  'SAP006'.
*
*  lw_date = p_date.
*  DO.
*    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
*         EXPORTING
*              langu               = sy-langu
*              date                = lw_date
*              calid               = w_kalid
*         IMPORTING
*              daynr               = lw_daynr
*              dayfree             = lw_dayfree
*         EXCEPTIONS
*              no_langu            = 1
*              no_date             = 2
*              no_daytxt_for_langu = 3
*              invalid_date        = 4
*              OTHERS              = 5.
*    IF sy-subrc <> 0.
**      RAISE etc_exception.
*    ENDIF.
*
*    IF lw_dayfree EQ 'X'.
*      lw_date = lw_date - 1.
*      CONTINUE.
*    ELSE.
*      lt_date = lw_date .
*      EXIT.
*    ENDIF.
*  ENDDO.
*  c_date = lt_date.
  c_date = p_date.
ENDFORM.                    " conversion_date
*&---------------------------------------------------------------------*
*&      Form  get_data_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_plan.


  REFRESH l_cabn_tab .
  CLEAR : cuvtab,l_vtab_field,l_cabn_tab .

  SELECT SINGLE * FROM cuvtab  WHERE vtnam EQ vtnam.

  IF sy-subrc = 0.
    PERFORM get_data_cuta_table.
    PERFORM filled_value USING cuvtab-vtint.
  ENDIF.
ENDFORM.                    " get_data_plan
*&---------------------------------------------------------------------*
*&      Form  filled_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filled_value USING p_vtint.
*l_cabn_tab
  SELECT slnid INTO TABLE it_cuvtln
      FROM cuvtln
       WHERE vtint EQ p_vtint.

  SORT it_cuvtln BY  slnid.
  SORT l_cabn_tab BY atfor.
  LOOP AT it_cuvtln.
    LOOP AT l_cabn_tab.
      CASE l_cabn_tab-atfor.
        WHEN 'CHAR'.
          PERFORM cuto_move_char USING it_cuvtln-slnid
                                       l_cabn_tab-atinn  p_vtint
                                 CHANGING f_valc.
          PERFORM position_dbtable USING l_cabn_tab-atfor
                                         l_cabn_tab-atnam f_valc.

        WHEN 'DATE' OR 'NUM'.
          PERFORM cuto_move_num USING it_cuvtln-slnid
                                       l_cabn_tab-atinn  p_vtint
                                 CHANGING l_val_from.

          PERFORM position_dbtable USING l_cabn_tab-atfor
                                         l_cabn_tab-atnam l_val_from .

      ENDCASE.
      CLEAR f_valc.
    ENDLOOP.
*    APPEND it_plan.
  ENDLOOP.
*insert
  MODIFY ztpp_month_plan   FROM TABLE it_plan.
  COMMIT WORK.

ENDFORM.                    " filled_value
*&---------------------------------------------------------------------*
*&      Form  get_data_cuta_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_cuta_table.

  CALL FUNCTION 'CUTA_TABLE_STRUCTURE_PREPARE'
    EXPORTING
      cuta_table_name                      = vtnam
*     CUTA_MAINTAIN_TABLE_STRUCTURE        =
      cuta_maintain_table_content          = 'X'
*     CUTA_ENQUEUE_TABLE                   = 'X'
      cuta_date                            = p_date
*   IMPORTING
*     CUTA_TABLE_EXISTS                    =
   EXCEPTIONS
     enqueue_fail                         = 1
     no_authority_var_tables              = 2
     no_authority_table_content           = 3
     table_name_incorrect                 = 4
     table_not_found                      = 5
     table_content_no_authority           = 6
     table_structure_no_authority         = 7
     table_structure_work_committed       = 8
     content_maintenance_not_free         = 9
     OTHERS                               = 10
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'CUTA_CHARACTERISTICS_READ'
       EXPORTING
            cuta_table_name      = vtnam
       TABLES
            cuta_characteristics = tab_api_vtab_fields.

  REFRESH l_r_atinn.CLEAR l_r_atinn.
  l_r_atinn-option = 'EQ'.
  l_r_atinn-sign   = 'I'.
  LOOP AT tab_api_vtab_fields INTO l_vtab_field.
    l_r_atinn-low = l_vtab_field-atinn.
    APPEND l_r_atinn.
  ENDLOOP.

  CALL FUNCTION 'CLSE_SELECT_CABN'
       EXPORTING
            key_date                     = p_date
*           BYPASSING_BUFFER             = ' '
       TABLES
            in_cabn                      = l_r_atinn
            t_cabn                       = l_cabn_tab
       EXCEPTIONS
            no_entry_found               = 1.

  CHECK sy-subrc IS INITIAL.

  SORT l_cabn_tab BY atinn.

ENDFORM.                    " get_data_cuta_table
*&---------------------------------------------------------------------*
*&      Form  cuto_move_char
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CUVTLN_SLNID  text
*      -->P_L_CABN_TAB_ATINN  text
*      -->P_P_VTINT  text
*----------------------------------------------------------------------*
FORM cuto_move_char USING    p_slnid p_atinn l_vtint
                    CHANGING p_valc.

  SELECT  SINGLE valc INTO p_valc
     FROM cuvtab_valc
       WHERE vtint EQ l_vtint
         AND slnid EQ p_slnid
         AND atinn EQ  p_atinn
         AND vlcnt EQ '001'.


ENDFORM.                    " cuto_move_char
*&---------------------------------------------------------------------*
*&      Form  position_dbtable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_CABN_TAB_ATNAM  text
*      -->P_F_VALC  text
*----------------------------------------------------------------------*
FORM position_dbtable USING    p_atfor
                               p_atnam
                                 p_valc.
  DATA  :c_date(30).
  CASE p_atfor.
    WHEN 'CHAR'.
      CASE p_atnam.
        WHEN 'P_MODEL'.
          MOVE p_valc TO it_plan-model.
        WHEN 'P_PLAN_TYPE'.
          MOVE p_valc TO it_plan-ptype.
        WHEN 'P_PLAN_YEAR'.
          MOVE p_valc TO it_plan-gjahr.
        WHEN 'P_PLAN_REMARK_1'.
          MOVE p_valc TO it_plan-zremark1.
        WHEN 'P_PLAN_REMARK_2'.
          MOVE p_valc TO it_plan-zremark2.
        WHEN 'P_PLAN_REMARK_3'.
          MOVE p_valc TO it_plan-zremark3.
      ENDCASE.
    WHEN 'NUM' OR 'DATE'.
      CASE p_atnam.
        WHEN 'P_ALC_DATE'.
          CALL FUNCTION 'CTCV_CONVERT_FLOAT_TO_DATE'
               EXPORTING
                    float = p_valc
               IMPORTING
                    date  = c_date.
          MOVE c_date(8) TO it_plan-erdat.
        WHEN 'P_PLAN_JAN' .
          MOVE '01'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_FEB'.
          MOVE '02'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_MAR'.
          MOVE '03'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_APR'.
          MOVE '04'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_MAY'.
          MOVE '05'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_JUN'.
          MOVE '06'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_JUL'.
          MOVE '07'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_AUG'.
          MOVE '08'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_SEP'.
          MOVE '09'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_OCT'.
          MOVE '10'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_NOV'.
          MOVE '11'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
        WHEN 'P_PLAN_DEC'.
          MOVE '12'   TO it_plan-zmonth.
          MOVE p_valc TO it_plan-zmpqty.
*        WHEN 'P_PLAN_TOTAL'.
*          MOVE p_valc TO it_plan-ztotal.
      ENDCASE.
      IF  p_atfor EQ 'NUM'.
        APPEND it_plan.
      ENDIF.
  ENDCASE.


ENDFORM.                    " position_dbtable
*&---------------------------------------------------------------------*
*&      Form  cuto_move_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CUVTLN_SLNID  text
*      -->P_L_CABN_TAB_ATINN  text
*      -->P_P_VTINT  text
*      <--P_F_VALC  text
*----------------------------------------------------------------------*
FORM cuto_move_num USING    p_slnid p_atinn l_vtint
                    CHANGING p_val_from.


  SELECT  SINGLE val_from INTO p_val_from
     FROM cuvtab_valn
       WHERE vtint EQ l_vtint
         AND slnid EQ p_slnid
         AND atinn EQ  p_atinn
         AND vlcnt EQ '001'.


ENDFORM.                    " cuto_move_NUM
*&---------------------------------------------------------------------*
*&      Form  get_delayed_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_delayed_working_time.

  REFRESH it_working_time.
  CLEAR it_working_time.
*GET WORKING DAY
  PERFORM factory_calendar USING p_date
                           CHANGING lw_date d_days.
*Get working time
  PERFORM get_working_time USING lw_date d_days.

*Get working delayed time
  PERFORM get_delayed_time_value TABLES it_working_time
                                 CHANGING a07_date.
*Get vehicle data using RP07 ACTUAL DATE
  PERFORM get_vehicle_actual07 USING a07_date.

ENDFORM.                    " get_delayed_working_time
*&---------------------------------------------------------------------*
*&      Form  factory_calendar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM factory_calendar USING l_date
                      CHANGING t_date c_num.
*----- Read Shop Calendar ID
  SELECT SINGLE c~kalid c~mosid b~kapid
    INTO (w_kalid, w_mosid, w_kapid)
    FROM crhd AS a INNER JOIN crca AS b
                      ON a~objty = b~objty
                     AND a~objid = b~objid
                   INNER JOIN kako AS c
                      ON b~kapid = c~kapid
   WHERE a~werks =  p_plant
     AND a~arbpl =  'T'
     AND b~fork2 =  'SAP006'.

  lw_date = p_date.
  CLEAR c_num .
  DO.
    IF c_num = '5'.
      t_date = lw_date .
      EXIT.
    ENDIF.

    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
         EXPORTING
              langu               = sy-langu
              date                = lw_date
              calid               = w_kalid
         IMPORTING
              daynr               = lw_daynr
              dayfree             = lw_dayfree
         EXCEPTIONS
              no_langu            = 1
              no_date             = 2
              no_daytxt_for_langu = 3
              invalid_date        = 4
              OTHERS              = 5.
    IF sy-subrc <> 0.
      RAISE etc_exception.
    ENDIF.

    IF lw_dayfree EQ 'X'.
      lw_date = lw_date - 1.
*      c_num = c_num + 1.
      CONTINUE.
    ELSE.
      lw_date = lw_date - 1.
      c_num = c_num + 1.
    ENDIF.
  ENDDO.
ENDFORM.                    " factory_calendar
*&---------------------------------------------------------------------*
*&      Form  get_delayed_time_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WORKING_TIME  text
*----------------------------------------------------------------------*
FORM get_delayed_time_value TABLES   p_working_time STRUCTURE
                                                  zsmm_working_time
                            CHANGING a07_date.
*p_dh
  DATA :c_opsec LIKE zsmm_working_time-opsec,
        t_opsec LIKE zsmm_working_time-opsec.

  DATA :wr_date TYPE d,
        z_wofrm LIKE  zsmm_working_time-wofrm.
  CLEAR :c_opsec,t_opsec,a07_date.

  c_opsec = p_dh * 3600.

*  SORT p_working_time BY datum ascending.
*delete working time after at 06:00 on today
  DATA  :del_date LIKE zsmm_working_time-wofrm.
  CONCATENATE sy-datum '060000' INTO del_date.
  LOOP AT p_working_time.
    IF p_working_time-wofrm > del_date.
      DELETE TABLE p_working_time FROM p_working_time.
    ENDIF.
  ENDLOOP.

  LOOP AT p_working_time.
    IF t_opsec =< c_opsec.
      t_opsec = t_opsec + p_working_time-opsec .
      MOVE : p_working_time-datum  TO wr_date,
             p_working_time-wofrm  TO z_wofrm.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.
* check if the delay hours setting is too long.
  IF t_opsec LT c_opsec.
    MESSAGE e000 WITH 'The delay time entered is too big!'.
  ENDIF.
  DO .
    IF t_opsec  GT c_opsec.
      PERFORM add_seconds USING z_wofrm '1'.
*      z_wofrm = z_wofrm + 1.
      t_opsec = t_opsec - 1.
    ELSE.
      MOVE : z_wofrm TO  a07_date.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " get_delayed_time_value
*&---------------------------------------------------------------------*
*&      Form  get_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_DATE  text
*      -->P_D_DAYS  text
*----------------------------------------------------------------------*
FORM get_working_time USING    plw_date
                               pd_days.

  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
    EXPORTING
      i_datum                    = plw_date
      i_day                      = pd_days
      i_arbpl                    = 'T'
* IMPORTING
*   E_DATE_CURR                =
*   E_TPROG_CURR               =
*   E_DATE_NEXT                =
*   E_TPROG_NEXT               =
 TABLES
    t_working_time             = it_working_time
*   T_1T                       =
 EXCEPTIONS
   cannot_read_dayname        = 1
   incorrect_shift_info       = 2
   incorrect_capa_info        = 3
   OTHERS                     = 4 .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT it_working_time BY datum DESCENDING index DESCENDING.

ENDFORM.                    " get_working_time
*&---------------------------------------------------------------------*
*&      Form  get_vehicle_actual07
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_A07_DATE  text
*----------------------------------------------------------------------*
FORM get_vehicle_actual07 USING    p_a07_date.
  DATA : p_a07_date_pre(7),
         t_count TYPE i,
         l_cnt   TYPE i.

  REFRESH :it_condition,it_vehicle,it_vehi.
  REFRESH : it_delay.
  p_char_status = 'P_RP_STATUS'.
  p_i_atwrt_s = '07'.
  p_i_atwrt_e = '17'.

  CLEAR it_condition.
  it_condition-atnam = 'P_RP07_ACTUAL_DATE'.
  CONCATENATE p_a07_date(4) '*' INTO  p_a07_date_pre.
  it_condition-atwrt_s = p_a07_date_pre.
  it_condition-atwrt_e = p_a07_date.
  APPEND it_condition.CLEAR it_condition.
  it_condition-atnam = 'P_MODEL'.
  APPEND it_condition.CLEAR it_condition.

  PERFORM get_fuction_cahr_value CHANGING p_flag.

  IF p_flag EQ 'X'.
    SORT it_vehicle BY objek atnam.
    it_vehi[] = it_vehicle[].
    CLEAR l_cnt.
    DELETE ADJACENT DUPLICATES FROM it_vehi COMPARING objek.
    LOOP AT it_vehi.
      READ TABLE it_vehicle WITH KEY objek = it_vehi-objek
                                     atnam = 'P_MODEL'.

      LOOP AT it_ztpp_pro  WHERE model EQ it_vehicle-atwrt.
        ADD 1 TO it_ztpp_pro-dqty.
        MOVE p_dh TO it_ztpp_pro-dtime.
        MODIFY it_ztpp_pro FROM it_ztpp_pro.
*       REQUESTED BY MY HUR CHANGED BY CHRIS
*       SAVE THE DELAYED VEHICLE
        CLEAR: it_delay.
        l_cnt = l_cnt + 1.
        it_delay-seq = l_cnt.
        it_delay-wdate = p_date.
        it_delay-objek = it_vehicle-objek.
        APPEND it_delay.
*       ENDO OF CHANGE ON 06/20/2005
      ENDLOOP.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_vehicle_actual07
*&---------------------------------------------------------------------*
*&      Form  get_fuction_cahr_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FLAG  text
*----------------------------------------------------------------------*
FORM get_fuction_cahr_value CHANGING p_flag.
  CALL FUNCTION 'Z_FCA_GET_VEHICLE_MASTER'
    EXPORTING
      i_atnam                             = p_char_status
      i_atwrt_s                           = p_i_atwrt_s
      i_atwrt_e                           = p_i_atwrt_e
*   I_OBJEK                             =
*   I_COUNT                             = 1000000
   IMPORTING
      e_hit_count                         = t_count
    TABLES
      t_condition                         = it_condition
      t_value                             = it_value_v
      t_vehicle                           = it_vehicle
    EXCEPTIONS
      date_overflow                       = 1
      invalid_date                        = 2
      condition_does_not_exist            = 3
      characteristic_does_not_exist       = 4
      OTHERS                              = 5 .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    p_flag = 'X'.
  ENDIF.
ENDFORM.                    " get_fuction_cahr_value
*&---------------------------------------------------------------------*
*&      Form  collect_final_internal_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_final_internal_table   USING c_date
                                    CHANGING yy_date.
  CLEAR yy_date.
  y_date = c_date.
*  y_date = c_date+6(4).
*  y_date+4(2) = c_date(2).
*  y_date+6(2) = c_date+3(2).
  PERFORM other_yesrterdat USING y_date
                           CHANGING yy_date.
  LOOP AT it_ztpp_pro.
    LOOP AT it_sum WHERE model EQ it_ztpp_pro-model.
      MOVE : it_sum-model TO it_ztpp_pro-model,
             p_plant      TO it_ztpp_pro-werks,
             y_date       TO it_ztpp_pro-p_date,
             it_sum-rp01  TO it_ztpp_pro-rp01,
             it_sum-rp02  TO it_ztpp_pro-rp02,
             it_sum-rp04  TO it_ztpp_pro-rp04,
             it_sum-rp06  TO it_ztpp_pro-rp06,
             it_sum-rp07  TO it_ztpp_pro-rp07,
             it_sum-rp18  TO it_ztpp_pro-rp18.
      IF yy_date+4(4) = '0101'.
        it_ztpp_pro-ysum = 0.
      ELSE.
        SELECT SINGLE ysum INTO  it_ztpp_pro-ysum
          FROM  ztpp_pro_sum
           WHERE model  EQ  it_sum-model
             AND werks  EQ  p_plant
             AND p_date EQ  yy_date.
      ENDIF.
      it_ztpp_pro-ysum = it_ztpp_pro-ysum +  it_sum-rp18.

      MOVE:   sy-datum     TO it_ztpp_pro-erdat,
              sy-timlo     TO it_ztpp_pro-erzet,
              sy-uname     TO it_ztpp_pro-ernam.
      MODIFY it_ztpp_pro FROM it_ztpp_pro.
      CONTINUE.
    ENDLOOP.

    MOVE :  p_plant      TO it_ztpp_pro-werks,
            y_date       TO it_ztpp_pro-p_date,
            sy-datum     TO it_ztpp_pro-erdat,
            sy-timlo     TO it_ztpp_pro-erzet,
            sy-uname     TO it_ztpp_pro-ernam.
    MODIFY it_ztpp_pro FROM it_ztpp_pro.
  ENDLOOP.
ENDFORM.                    " collect_final_internal_table
*&---------------------------------------------------------------------*
*&      Form  get_value_vehicle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_DATE  text
*----------------------------------------------------------------------*
FORM get_value_vehicle USING   c_date.
  DATA: l_count LIKE sy-tabix.

  LOOP AT it_vehi.
    LOOP AT it_vehicle WHERE objek EQ it_vehi-objek.
      PERFORM extract_value_model USING it_vehicle-atnam
                                        it_vehicle-atwrt c_date.
    ENDLOOP.
    COLLECT it_temp_value.CLEAR it_temp_value.
  ENDLOOP.
*  l_count = 1.
*  LOOP AT it_vehi.
*    LOOP AT it_vehicle FROM l_count.
*      IF it_vehicle-objek EQ it_vehi-objek.
*        PERFORM extract_value_model USING it_vehicle-atnam
*                                       it_vehicle-atwrt c_date.
*      ELSE.
*        l_count = sy-tabix.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
**    IF NOT it_temp_value-model IS INITIAL.
*      COLLECT it_temp_value.CLEAR it_temp_value.
**    ENDIF.
*  ENDLOOP.

*Sum
  REFRESH it_sum.
  LOOP AT it_temp_value.
    MOVE-CORRESPONDING  it_temp_value TO it_sum.
    COLLECT it_sum.
  ENDLOOP.
*  it_sum[] = it_temp_value[].
*  LOOP AT it_sum.
*    COLLECT it_sum.
*  ENDLOOP.

ENDFORM.                    " get_value_vehicle
*&---------------------------------------------------------------------*
*&      Form  other_yesrterdat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_Y_DATE  text
*      -->P_CHANGEING  text
*      -->P_YY_DATE  text
*----------------------------------------------------------------------*
FORM other_yesrterdat USING    p_y_date
                      CHANGING p_yy_date.

  SELECT MAX( p_date ) INTO p_yy_date
    FROM ztpp_pro_sum
     WHERE werks EQ p_plant
       AND p_date < p_y_date.

  IF p_yy_date = '00000000'.
    p_yy_date = p_y_date.
  ENDIF.
ENDFORM.                    " other_yesrterdat
*&---------------------------------------------------------------------*
*&      Form  set_up_internal_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_up_internal_table.
  CLEAR z_atinn.
  SELECT SINGLE atinn INTO z_atinn
     FROM cabn
      WHERE atnam EQ 'P_MODEL'.

  REFRESH : it_ztpp_pro.CLEAR :it_ztpp_pro.

  SELECT atwrt INTO it_ztpp_pro-model
     FROM cawn
      WHERE atinn EQ z_atinn.
    APPEND  it_ztpp_pro.
  ENDSELECT.
ENDFORM.                    " set_up_internal_table
*&---------------------------------------------------------------------*
*&      Form  get_data_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_actual USING c_date yy_date.

  LOOP AT it_ztpp_pro.
*Month Plan  - Annual
    PERFORM month_plan USING it_ztpp_pro-model c_date.
*Production day plan & Actual(yesterday)
    PERFORM yesterday_actual USING it_ztpp_pro-model c_date c_date.
*Summary Month Plan & Actual
    PERFORM summary_month USING it_ztpp_pro-model.
*Today plan = ( month plan / month working time)*today(working time)
    PERFORM today_plan.

    MODIFY it_ztpp_pro FROM it_ztpp_pro .
  ENDLOOP.
*T/R WIP
  PERFORM tr_wip.

ENDFORM.                    " get_data_actual
*&---------------------------------------------------------------------*
*&      Form  month_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTPP_PRO_MODEL  text
*----------------------------------------------------------------------*
FORM month_plan USING p_model c_date.
*Month plan -annual
  SELECT SINGLE zmpqty INTO it_ztpp_pro-mann
     FROM ztpp_month_plan
        WHERE gjahr EQ c_date(4)
          AND model EQ p_model
          AND ptype EQ '0'
          AND zmonth EQ c_date+4(2).
*Month plan - operation
  SELECT SINGLE zmpqty INTO it_ztpp_pro-mope
      FROM ztpp_month_plan
        WHERE gjahr EQ c_date(4)
          AND model EQ p_model
          AND ptype EQ '1'
          AND zmonth EQ c_date+4(2).

ENDFORM.                    " month_plan
*&---------------------------------------------------------------------*
*&      Form  yesterday_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTPP_PRO_MODEL  text
*      -->P_C_DATE  text
*----------------------------------------------------------------------*
FORM yesterday_actual USING    p_model c_date
                               yy_date.
*yesterday plan
  PERFORM get_work_day USING c_date yy_date.
  PERFORM get_plan USING yy_date.

ENDFORM.                    " yesterday_actual
*&---------------------------------------------------------------------*
*&      Form  get_work_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_work_day USING c_date yy_date.
  DATA: lw_date LIKE p0001-begda,
        lf_date LIKE p0001-begda,
        lt_date LIKE lw_date.
  DATA :ls_date LIKE sy-datum,
        ch_date LIKE sy-datum,
        d_days LIKE  vtbbewe-atage.
  DATA :it_working_time LIKE zsmm_working_time OCCURS 0
                               WITH HEADER LINE.

  REFRESH : it_working_time,it_wtsum,it_wdsum .
  CLEAR: ls_date,ch_date,d_days,it_working_time,it_wtsum,it_wdsum .

  lt_date = yy_date.
  CONCATENATE c_date(6) '01' INTO lw_date.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = lw_date
       IMPORTING
            last_day_of_month = ls_date
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM monthly_working_day USING lw_date ls_date "lt_date
                              CHANGING d_days.

  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
    EXPORTING
      i_datum                    = lw_date "lt_date
      i_day                      = d_days
      i_arbpl                    = 'T'
* IMPORTING
*   E_DATE_CURR                =
*   E_TPROG_CURR               =
*   E_DATE_NEXT                =
*   E_TPROG_NEXT               =
   TABLES
     t_working_time             = it_working_time
*   T_1T                       =
   EXCEPTIONS
     cannot_read_dayname        = 1
     incorrect_shift_info       = 2
     incorrect_capa_info        = 3
     OTHERS                     = 4 .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  DELETE ADJACENT DUPLICATES FROM it_working_time COMPARING datum tprog


  LOOP AT it_working_time.
    IF it_working_time-datum > ls_date.
      DELETE TABLE it_working_time FROM it_working_time.
    ELSE.
      MOVE : it_working_time-datum TO it_wdsum-datum,
             it_working_time-opsec TO it_wtsum-opsec,
             it_working_time-opsec TO it_wdsum-opsec.
      it_wtsum-opsec = it_wtsum-opsec / 3600.
      it_wdsum-opsec = it_wdsum-opsec / 3600.
      IF it_working_time-datum  >= lw_date.
        COLLECT it_wtsum .
      ENDIF.
      COLLECT it_wdsum.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_work_day
*&---------------------------------------------------------------------*
*&      Form  get_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_plan USING yy_date.
  DATA : d_num TYPE i.
  DATA : l_uph  TYPE zvpp_ld-lrate. " UPH

  CHECK it_ztpp_pro-mope <> 0.
  PERFORM get_uph  USING yy_date l_uph   'T'   .

  READ TABLE it_wtsum INDEX 1.
  IF sy-subrc = 0.
    READ TABLE it_wdsum WITH KEY datum = yy_date.
    IF sy-subrc = 0.
      it_ztpp_pro-yplan = l_uph * it_wdsum-opsec.
    ELSE.
      it_ztpp_pro-yplan  = 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_plan
*&---------------------------------------------------------------------*
*&      Form  summary_month
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTPP_PRO_MODEL  text
*----------------------------------------------------------------------*
FORM summary_month USING p_model.
**Plan  sum working time * UPH
  DATA : it_wdsum_temp LIKE it_wdsum OCCURS 0 WITH HEADER LINE.
  DATA : l_uph  TYPE zvpp_ld-lrate,
         acc_sum LIKE  it_ztpp_pro-splan.

  REFRESH it_wdsum_temp.CLEAR acc_sum.

  CHECK it_ztpp_pro-mope <> 0.

  it_wdsum_temp[] = it_wdsum[].

  LOOP AT it_wdsum_temp.
    IF it_wdsum_temp-datum > c_date.
      DELETE TABLE it_wdsum_temp FROM it_wdsum_temp.
    ELSE.
      PERFORM get_uph  USING it_wdsum_temp-datum l_uph 'T'.
      acc_sum = acc_sum + ( l_uph * it_wdsum_temp-opsec ).
    ENDIF.
  ENDLOOP.
  it_ztpp_pro-splan =  acc_sum.

ENDFORM.                    " summary_month
*&---------------------------------------------------------------------*
*&      Form  today_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM today_plan.
  DATA : today LIKE sy-datum,
         f_date LIKE p0001-begda.
  DATA : l_uph  TYPE zvpp_ld-lrate. " UPH

  CLEAR :f_date,today,it_ztpp_pro-tplan.

  CHECK it_ztpp_pro-mope <> 0.

  f_date = c_date.
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = f_date
            days      = '01'
            months    = '00'
            signum    = '+'
            years     = '00'
       IMPORTING
            calc_date = today.

  PERFORM get_uph  USING today  l_uph   'T'   .

  READ TABLE it_wtsum INDEX 1.
  IF sy-subrc = 0.
    READ TABLE it_wdsum WITH KEY datum = today.
    IF sy-subrc = 0.
      it_ztpp_pro-tplan = l_uph * it_wdsum-opsec.
    ENDIF.
  ENDIF.

ENDFORM.                    " today_plan
*&---------------------------------------------------------------------*
*&      Form  tr_wip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tr_wip.
*P_RP_STATUS : 07 ~ 17
  DATA : z_twip LIKE it_ztpp_pro-twip.
  CLEAR z_twip.
  REFRESH :it_condition,it_vehicle,it_vehi.
  it_condition-atnam = 'P_MODEL'.
  APPEND it_condition.CLEAR it_condition.
  p_i_atwrt_s = '07'.
  p_i_atwrt_e = '17'.

  PERFORM get_fuction_cahr_value CHANGING p_flag.
  IF p_flag EQ 'X'.
    SORT it_vehicle BY objek atnam.
    it_vehi[] = it_vehicle[].

    DELETE ADJACENT DUPLICATES FROM it_vehi COMPARING objek.
    LOOP AT it_vehi.
      READ TABLE it_vehicle WITH KEY objek = it_vehi-objek
                                     atnam = 'P_MODEL'.
      LOOP AT it_ztpp_pro WHERE model EQ it_vehicle-atwrt.
        ADD 1 TO it_ztpp_pro-twip.
        MODIFY it_ztpp_pro TRANSPORTING twip
            WHERE model EQ it_vehicle-atwrt.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " tr_wip
*&---------------------------------------------------------------------*
*&      Form  get_data_wip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_wip USING c_date yy_date.
*Control gate
  PERFORM control_gate_returned USING yy_date.
  PERFORM control_gate_passed USING yy_date.
*VPC WIP
  PERFORM vpcwip_vpc USING '20' '22' 'V'.
  PERFORM vpcwip_vpc USING '23' '24' 'T'.
  PERFORM vpcwip_vpc USING '25' '27' 'R'.

ENDFORM.                    " get_data_wip
*&---------------------------------------------------------------------*
*&      Form  control_gate_returned
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM control_gate_returned USING yy_date.
  DATA : y_date_c(9),t_count TYPE i.
  REFRESH :it_condition,it_vehicle,it_vehi.

  p_char_status = 'P_RP_STATUS'.
  p_i_atwrt_s = '18'.
  p_i_atwrt_e = ' '.

  CLEAR it_condition.
  it_condition-atnam = 'P_BC_RP'.
  it_condition-atwrt_s = 'T27'.
*  it_condition-atwrt_e = 'T27'.
  APPEND it_condition.CLEAR it_condition.

  it_condition-atnam = 'P_BC_CHANGE_DATE'.
*  CONCATENATE y_date '*' INTO y_date_c.
  it_condition-atwrt_s = yy_date.
  APPEND it_condition.CLEAR it_condition.
  it_condition-atnam = 'P_MODEL'.
  APPEND it_condition.CLEAR it_condition.

  PERFORM get_fuction_cahr_value CHANGING p_flag.

  IF p_flag EQ 'X'.
    SORT it_vehicle BY objek atnam.
    it_vehi[] = it_vehicle[].

    DELETE ADJACENT DUPLICATES FROM it_vehi COMPARING objek.
    LOOP AT it_vehi.
      READ TABLE it_vehicle WITH KEY objek = it_vehi-objek
                                     atnam = 'P_MODEL'.
      LOOP AT it_ztpp_pro WHERE model EQ it_vehicle-atwrt.
        ADD 1 TO it_ztpp_pro-cretu.
        MODIFY it_ztpp_pro FROM it_ztpp_pro.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " control_gate_returned
*&---------------------------------------------------------------------*
*&      Form  control_gate_passed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM control_gate_passed USING yy_date.
  DATA : y_date_c(9),t_count TYPE i.
  REFRESH :it_condition,it_vehicle,it_vehi.

  p_char_status = 'P_RP19_SHOP_DATE'.
  p_i_atwrt_s = yy_date.
*  p_i_atwrt_e = ' '.

  it_condition-atnam = 'P_MODEL'.
  APPEND it_condition.CLEAR it_condition.

  PERFORM get_fuction_cahr_value CHANGING p_flag.

  IF p_flag EQ 'X'.
    SORT it_vehicle BY objek atnam.
    it_vehi[] = it_vehicle[].

    DELETE ADJACENT DUPLICATES FROM it_vehi COMPARING objek.
    LOOP AT it_vehi.
      READ TABLE it_vehicle WITH KEY objek = it_vehi-objek
                                     atnam = 'P_MODEL'.
      LOOP AT it_ztpp_pro WHERE model EQ it_vehicle-atwrt.
        ADD 1 TO it_ztpp_pro-cpass.
        MODIFY it_ztpp_pro FROM it_ztpp_pro.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " control_gate_passed
*&---------------------------------------------------------------------*
*&      Form  vpcwip_vpc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2206   text
*      -->P_2207   text
*      -->P_2208   text
*----------------------------------------------------------------------*
FORM vpcwip_vpc USING p_atwrt_s p_atwrt_e p_vpc.
  DATA : z_twip LIKE it_ztpp_pro-twip.
  CLEAR z_twip.
  REFRESH :it_condition,it_vehicle,it_vehi.
  it_condition-atnam = 'P_MODEL'.
  APPEND it_condition.CLEAR it_condition.
  p_i_atwrt_s = p_atwrt_s.
  p_i_atwrt_e = p_atwrt_e.

  PERFORM get_fuction_cahr_value CHANGING p_flag.
  IF p_flag EQ 'X'.
    SORT it_vehicle BY objek atnam.
    it_vehi[] = it_vehicle[].

    DELETE ADJACENT DUPLICATES FROM it_vehi COMPARING objek.
    LOOP AT it_vehi.
      READ TABLE it_vehicle WITH KEY objek = it_vehi-objek
                                     atnam = 'P_MODEL'.
      CASE p_vpc.
        WHEN 'V'.
          LOOP AT it_ztpp_pro WHERE model EQ it_vehicle-atwrt.
            ADD 1 TO it_ztpp_pro-vpc.
            MODIFY it_ztpp_pro TRANSPORTING vpc
                WHERE model EQ it_vehicle-atwrt.
          ENDLOOP.
        WHEN 'T'.
          LOOP AT it_ztpp_pro WHERE model EQ it_vehicle-atwrt.
            ADD 1 TO it_ztpp_pro-truck.
            MODIFY it_ztpp_pro TRANSPORTING truck
                WHERE model EQ it_vehicle-atwrt.
          ENDLOOP.
        WHEN 'T'.
          LOOP AT it_ztpp_pro WHERE model EQ it_vehicle-atwrt.
            ADD 1 TO it_ztpp_pro-rai.
            MODIFY it_ztpp_pro TRANSPORTING rai
                WHERE model EQ it_vehicle-atwrt.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " vpcwip_vpc
*&---------------------------------------------------------------------*
*&      Form  monthly_working_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_DATE  text
*      -->P_LS_DATE  text
*      <--P_D_DAYS  text
*----------------------------------------------------------------------*
FORM monthly_working_day USING    p_lw_date
                                  p_ls_date
                         CHANGING p_d_days.
  DATA : last_date LIKE sy-datum.
  CLEAR : last_date,lw_dayfree.
  last_date = p_lw_date.
  DO.
    IF last_date > p_ls_date.
      EXIT.
    ENDIF.

    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
         EXPORTING
              langu               = sy-langu
              date                = last_date
              calid               = w_kalid
         IMPORTING
              daynr               = lw_daynr
              dayfree             = lw_dayfree
         EXCEPTIONS
              no_langu            = 1
              no_date             = 2
              no_daytxt_for_langu = 3
              invalid_date        = 4
              OTHERS              = 5.
    IF sy-subrc <> 0.
      RAISE etc_exception.
    ENDIF.

    IF lw_dayfree EQ 'X'.
      last_date = last_date + 1.
    ELSE.
      last_date = last_date + 1.
      p_d_days = p_d_days + 1.
    ENDIF.
  ENDDO.

ENDFORM.                    " monthly_working_day
*&---------------------------------------------------------------------*
*&      Form  INITIAL_DATE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_date_check.
ENDFORM.                    " INITIAL_DATE_CHECK
*&---------------------------------------------------------------------*
*&      Form  get_uph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TODAY  text
*      -->P_L_UPH  text
*      -->P_2131   text
*----------------------------------------------------------------------*
FORM get_uph USING    pa_wdate  pa_uph  pa_wc.
  DATA: w_uph  LIKE ztpp_status-uph.
  DATA: wl_date LIKE sy-datum.

  wl_date = pa_wdate.

  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      date          = wl_date
*      SHIFT         =
      shop          = pa_wc
    IMPORTING
      uph           = w_uph
            .

  pa_uph  = w_uph.

*  DATA lw_ld          LIKE zvpp_ld .
*  DATA: LT_LD   LIKE ZVPP_LD OCCURS 0 WITH HEADER LINE.
*
** REQUESTED BY MY HUR CHANGED BY CHRIS
** SHOULD READ THE DATA OF TWO SHIFTS TO CALCULATE
** THE UPH FOR A DAY
**  SELECT SINGLE * INTO lw_ld
**    FROM zvpp_ld
**   WHERE ld_perst <= pa_wdate
**     AND ld_pered >= pa_wdate
**     AND arbpl     = pa_wc     .
*  SELECT  * INTO TABLE lT_ld
*    FROM zvpp_ld
*   WHERE ld_perst <= pa_wdate
*     AND ld_pered >= pa_wdate
*     AND arbpl     = pa_wc     .
*  LOOP AT LT_LD.
*    LW_LD-LRATE = LW_LD-LRATE + LT_LD-LRATE.
*    LW_LD-LANTU = LW_LD-LANTU + LT_LD-LANTU.
*  ENDLOOP.
*
** END OF CHANGE ON 06/13/2005
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
ENDFORM.                    " get_uph
*&---------------------------------------------------------------------*
*&      Form  modify_plan_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_plan_quantity.
  DATA : m_mope LIKE it_ztpp_pro-mope.
  CLEAR : m_mope.
  LOOP AT it_ztpp_pro.
    AT LAST.
      SUM. m_mope = m_mope + it_ztpp_pro-mope.
    ENDAT.
  ENDLOOP.

  LOOP AT it_ztpp_pro.
    it_ztpp_pro-yplan = ( it_ztpp_pro-mope / m_mope ) *
                          it_ztpp_pro-yplan .
    it_ztpp_pro-splan = ( it_ztpp_pro-mope / m_mope ) *
                          it_ztpp_pro-splan .
    it_ztpp_pro-tplan = ( it_ztpp_pro-mope / m_mope ) *
                       it_ztpp_pro-tplan .

    MODIFY it_ztpp_pro INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " modify_plan_quantity
*&---------------------------------------------------------------------*
*&      Form  add_seconds
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZWOFRM  text
*      -->P_1199   text
*----------------------------------------------------------------------*
FORM add_seconds USING    p_time LIKE zsmm_working_time-wofrm
                          p_seconds.
  DATA: l_time  TYPE t.
  DATA: l_time1 TYPE t.
  DATA: l_date  TYPE d.
  DATA : diff   TYPE i.

  l_time1 = l_time = p_time+8(6).
  l_date = p_time(8).
  l_time = l_time + p_seconds.

  diff = l_time - l_time1.

  IF diff < 0.
    l_date = l_date + 1.
  ENDIF.

  CONCATENATE l_date l_time INTO p_time.

ENDFORM.                    " add_seconds
