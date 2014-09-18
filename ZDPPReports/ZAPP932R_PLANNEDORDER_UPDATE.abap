************************************************************************
* Program Name      :  ZAPP932R_PlannedOrder_UPDATE   .
* Author            :  Won-seob Kim
* Creation Date     :  2004.12.14.
* Specifications By :
* Pattern           :  Report 1-1
* Development Request No :
* Addl Documentation:
* Description       :  Planned order update
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************

REPORT  zapp932r_plannedorder_update   MESSAGE-ID zmpp.

TABLES : resb,plaf.
DATA : it_bfst LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE.
DATA : t_count TYPE i,
       p_flag ,
       p_char_status LIKE  cabn-atnam ,
       p_i_atwrt_s LIKE ausp-atwrt,
       p_i_atwrt_e LIKE ausp-atwrt,
       p_enmng TYPE c.
DATA  :it_condition LIKE zsca_characteristic_value OCCURS 0
                         WITH HEADER LINE,
       it_value_v  LIKE  zsca_char_value  OCCURS 0
                         WITH HEADER LINE,
       it_vehicle LIKE zsca_vehicle_char_value OCCURS 0
                         WITH HEADER LINE,
       it_vehi LIKE it_vehicle OCCURS 0 WITH HEADER LINE,
       BEGIN OF it_plan OCCURS 0,
        plnum LIKE plaf-plnum,
       END OF it_plan,
       BEGIN OF it_resb OCCURS 0,
        plnum LIKE plaf-plnum,
        rsnum LIKE resb-rsnum,
        rspos LIKE resb-rspos,
        matnr LIKE resb-matnr,
        sortf LIKE resb-sortf,
       END OF it_resb.
DATA : l_return            LIKE bapireturn1 .

***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*ARAMETERS: p_batch TYPE c DEFAULT 'X'.
SELECT-OPTIONS : s_plnum FOR plaf-plnum NO INTERVALS .
PARAMETERS : f_sortf LIKE resb-sortf,
             t_sortf LIKE resb-sortf,
             p_test TYPE c DEFAULT 'X',
             p_wqty TYPE c.
SELECTION-SCREEN END OF BLOCK b1.

***********************************************************************
START-OF-SELECTION.
  PERFORM get_planned_order.
  PERFORM get_component_resb.
  PERFORM update_plannedorder.

END-OF-SELECTION.
  PERFORM write.
*&---------------------------------------------------------------------*
*&      Form  get_planned_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_planned_order.

  IF p_wqty EQ space.
    IF s_plnum-low IS INITIAL.
*Get data from Vehicle Master
      REFRESH :it_condition,it_vehicle,it_vehi,it_plan.
      CLEAR it_plan.

      p_char_status = 'P_RP_STATUS'.
      p_i_atwrt_s = '01'.
      p_i_atwrt_e = '17'.

      CLEAR it_condition.
      it_condition-atnam = 'P_PLAN_ORDER'.
      APPEND it_condition.CLEAR it_condition.

      PERFORM get_fuction_cahr_value CHANGING p_flag.
      IF p_flag EQ 'X'.
        SORT it_vehicle BY objek atnam.
        it_vehi[] = it_vehicle[].
        DELETE ADJACENT DUPLICATES FROM it_vehi COMPARING objek.
        LOOP AT it_vehi.
          READ TABLE it_vehicle WITH KEY objek = it_vehi-objek
                                         atnam = 'P_PLAN_ORDER'.
          IF sy-subrc = 0.
            MOVE it_vehicle-atwrt TO it_plan-plnum.
            APPEND it_plan.CLEAR it_plan.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM plaf
        WHERE plnum EQ s_plnum-low
          AND stlfx EQ 'X'.
      IF sy-subrc = 0.
        it_plan-plnum = plaf-plnum.
        APPEND it_plan.CLEAR it_plan.
      ELSE.
     MESSAGE i001 WITH 'No planned order is makred at components field'.
        STOP.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT plan_ord INTO TABLE it_plan
      FROM ztpp_bfst
       WHERE bfp17_flg EQ 'ER'.
  ENDIF.
ENDFORM.                    " get_planned_order
*&---------------------------------------------------------------------*
*&      Form  get_fuction_cahr_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FLAG  text
*----------------------------------------------------------------------*
FORM get_fuction_cahr_value  CHANGING p_flag.
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
*&      Form  get_component_resb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_component_resb.
  SORT it_plan BY plnum.
  REFRESH it_resb.CLEAR it_resb.
  IF  p_wqty EQ space.
    LOOP AT it_plan.
      SELECT * FROM resb
        WHERE plnum EQ it_plan-plnum
          AND sortf EQ f_sortf.
*      IF resb-sortf EQ space.
        MOVE : it_plan-plnum TO it_resb-plnum,
               resb-rsnum    TO it_resb-rsnum,
               resb-rspos    TO it_resb-rspos,
               resb-matnr    TO it_resb-matnr,
               t_sortf       TO it_resb-sortf.
        APPEND it_resb.CLEAR it_resb.
*      ENDIF.
      ENDSELECT.
    ENDLOOP.
  ELSE.
    LOOP AT it_plan.
      SELECT * FROM resb
        WHERE plnum EQ it_plan-plnum
          AND sortf EQ f_sortf
          AND enmng EQ '0'.
        IF sy-subrc EQ 0.
          MOVE : it_plan-plnum TO it_resb-plnum,
                 resb-rsnum    TO it_resb-rsnum,
                 resb-rspos    TO it_resb-rspos,
                 resb-matnr    TO it_resb-matnr,
                 t_sortf       TO it_resb-sortf.
          APPEND it_resb.CLEAR it_resb.
        ENDIF.
      ENDSELECT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_component_resb
*&---------------------------------------------------------------------*
*&      Form  update_plannedorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_plannedorder.
  DATA : it_component LIKE bapi_pldordcomp_e1 OCCURS 0 WITH HEADER LINE.
  DATA : w_int TYPE i,
         l_header    LIKE bapiplaf_i2 ,
         l_headerx   LIKE bapiplaf_i2x.

  IF p_test <> 'X'.
    LOOP AT it_plan.
      LOOP AT it_resb WHERE plnum EQ it_plan-plnum.
        UPDATE resb
            SET sortf = it_resb-sortf
          WHERE rsnum = it_resb-rsnum
            AND rspos = it_resb-rspos
            AND matnr = it_resb-matnr.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " update_plannedorder
*&---------------------------------------------------------------------*
*&      Form  write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write.
  DATA :w_int TYPE i.
  CLEAR w_int.
*Message
  WRITE : 'Message'.
  WRITE : / l_return-type, l_return-id.
*RESB UPDATE COUNT
  DESCRIBE TABLE it_resb LINES w_int.
  IF w_int = 0.
    WRITE : / 'No data'.
  ELSE.
    LOOP AT it_resb.
      WRITE : / it_resb-plnum,it_resb-matnr,it_resb-sortf .
    ENDLOOP.
  ENDIF.
ENDFORM.                    " write
