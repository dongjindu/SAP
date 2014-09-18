************************************************************************
* Program Name      : SAPMZCO_AALA_CREATION
* Author            : Byung Sung Bae
* Creation Date     : 2005.01.10.
* Specifications By : Byung Sung Bae
* Pattern           : 2.1
* Development Request No : UD1K913724
* Addl Documentation:
* Description       : AALA Part Portion Creation
*
* Modification Logs
* Date       Developer    RequestNo    Description
*03/27/06    Manju        UD1K919862   AALA :- Exclude cost
*                                      elements
*

************************************************************************

REPORT  sapmzco_aala_creation.
INCLUDE <icon>.

TABLES: mara, t001, marc, a902, konp,
        ztco_shopcost,
        ztco_aala_creation_9000, ztco_aala_creation_9000_tab.

DATA: zsco_aala_creation_9100 LIKE zsco_aala_creation_9100.

*---// Internal tables
DATA: BEGIN OF it_duty OCCURS 0,
        matnr   LIKE  mara-matnr,
        dduty   LIKE  ztmm_analy-dduty,
      END   OF it_duty.

DATA: it_source LIKE ztco_aala_source OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_shop OCCURS 0.
        INCLUDE STRUCTURE ztco_shopcost.
DATA:   mipcd   LIKE ztco_aala_parts-mipcd,
      END   OF it_shop.

DATA: it_shop_mip LIKE it_shop OCCURS 0 WITH HEADER LINE.

DATA: it_parts  LIKE ztco_aala_parts  OCCURS 0 WITH HEADER LINE.

DATA: it_mip    LIKE ztco_aala_mip    OCCURS 0 WITH HEADER LINE.

DATA: it_fsc    LIKE ztco_aala_fsc    OCCURS 0 WITH HEADER LINE.

DATA: it_model  LIKE ztco_aala_model  OCCURS 0 WITH HEADER LINE.

DATA: it_9000   LIKE ztco_aala_creation_9000_tab OCCURS 0
                                               WITH HEADER LINE.

DATA: it_9100 TYPE STANDARD TABLE OF zsco_aala_creation_9100
                                               WITH HEADER LINE.

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,
      END   OF it_matnr.

DATA: BEGIN OF it_egntm OCCURS 0,
        matnr   LIKE   mara-matnr,
      END   OF it_egntm.

DATA: it_tckh2 LIKE tckh2 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_origin OCCURS 0,
        matnr   LIKE   mara-matnr,             "FSC Code
        mipcd   LIKE   mara-matnr,             "MIP Code
        shop    LIKE   ztco_shopcost-shop,     "Shop
        urzla   LIKE   eina-urzla,             "Country Code
        wrbtr   LIKE   bseg-wrbtr,             "Amount
        index   TYPE   i,
      END   OF it_origin.

*---// Work areas
DATA: w_klvar   LIKE   tck03-klvar,
      w_versn   LIKE   ztco_shopcost-versn,
      w_period  LIKE   ztmm_analy-period,
      w_total(10),
      w_success(10),
      w_error(10).

*---// Constants
CONSTANTS: c_check                             VALUE 'X',
           c_trimshop  LIKE ztco_shopcost-shop VALUE '__T%',
           c_bodyshop  LIKE ztco_shopcost-shop VALUE '__B%',
           c_paintshop LIKE ztco_shopcost-shop VALUE '__P%',
           c_pressshop LIKE ztco_shopcost-shop VALUE '__S%',
           c_egnshop   LIKE ztco_shopcost-shop VALUE '__E%',
           c_trim      LIKE ztco_shopcost-shop VALUE 'MXTX',
           c_mip       LIKE cska-kstar         VALUE '0000540300',
           c_mcost_f   LIKE cska-kstar         VALUE '0000540000',
           c_mcost_t   LIKE cska-kstar         VALUE '0000540199',
           c_activity  LIKE cska-kstar         VALUE '0000800000',
           c_elehk     LIKE tckh2-elehk        VALUE 'H1',
           c_bukrs     LIKE t001-bukrs         VALUE 'H201',
           c_success(4)                        VALUE icon_green_light,
           c_error(4)                          VALUE icon_red_light.

*---// Ranges
RANGES: r_fsc   FOR    mara-matnr.

*---// Table controls
CONTROLS: tc_9000 TYPE TABLEVIEW USING SCREEN 9000.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9100   TYPE        scrfname VALUE 'CC_9100_ALV',
      wc_alv_9100       TYPE REF TO cl_gui_alv_grid,
      wc_container_9100 TYPE REF TO cl_gui_custom_container.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS lcl_event_receiver DEFINITION DEFERRED. "/ALV Event Handling

DATA : event_receiver TYPE REF TO lcl_event_receiver.

* Interal tables for ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

* Global variable for ALV GRID
DATA : w_is_layout TYPE lvc_s_layo,
       w_variant   TYPE disvariant,          "for parameter IS_VARIANT
       w_fieldname LIKE LINE OF it_fieldcat,
       w_repid     LIKE sy-repid,
       w_cnt       TYPE i,                   "Field count
       w_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

DATA: w_container(50),
      w_control(50),
      w_alv(50),
      w_itab(50),
      w_structure LIKE dd02l-tabname.

FIELD-SYMBOLS: <container> TYPE REF TO cl_gui_custom_container,
               <control>   TYPE        scrfname,
               <alv>       TYPE REF TO cl_gui_alv_grid,
               <itab>      TYPE STANDARD TABLE.

CONSTANTS: c_structure(100) VALUE 'ZSCO_AALA_CREATION_'.

*-----/// ALV Control : END

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row
                      e_column
                      es_row_no.
ENDCLASS.

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM dbl_click_9000 USING e_column-fieldname
                                 es_row_no-row_id.

  ENDMETHOD.                           "handle_double_click
ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
    WHEN 9100.
      SET PF-STATUS '9100'.
      SET TITLEBAR  '9000'.
  ENDCASE.
ENDMODULE.                 " STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  move_itab_to_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_itab_to_screen OUTPUT.
  READ TABLE it_9000 INDEX tc_9000-current_line.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_9000 TO ztco_aala_creation_9000_tab.
  ELSE.
    EXIT FROM STEP-LOOP.
  ENDIF.
ENDMODULE.                 " move_itab_to_screen  OUTPUT
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
*&      Module  check_table_control_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_table_control_9000 INPUT.
  PERFORM check_matnr_9000.
  PERFORM check_atlmt_9000.

  IF NOT ztco_aala_creation_9000_tab-matnr IS INITIAL.
    MOVE: 'EA' TO ztco_aala_creation_9000_tab-meins.
  ENDIF.
ENDMODULE.                 " check_table_control_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_matnr_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr_9000.
  SELECT SINGLE maktx INTO ztco_aala_creation_9000_tab-maktx
    FROM makt
   WHERE matnr = ztco_aala_creation_9000_tab-matnr
     AND spras = sy-langu.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  IF ztco_aala_creation_9000_tab-matnr+6(2) NE
     ztco_aala_creation_9000-model.
    MESSAGE e000(zz) WITH text-m16.
  ENDIF.

  CHECK ztco_aala_creation_9000_tab-altmt EQ space.

  PERFORM check_shop_cost USING ztco_aala_creation_9000_tab-matnr.
ENDFORM.                    " check_matnr_9000
*&---------------------------------------------------------------------*
*&      Form  check_atlmt_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_atlmt_9000.
  CHECK NOT ztco_aala_creation_9000_tab-altmt IS INITIAL.

  SELECT SINGLE *
    FROM mara
   WHERE matnr = ztco_aala_creation_9000_tab-altmt.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.

  IF ztco_aala_creation_9000_tab-altmt+6(2) NE
     ztco_aala_creation_9000-model.
    MESSAGE e000(zz) WITH text-m16.
  ENDIF.

  CHECK ztco_aala_creation_9000_tab-altmt NE space.

  PERFORM check_shop_cost USING ztco_aala_creation_9000_tab-altmt.
ENDFORM.                    " check_atlmt_9000
*&---------------------------------------------------------------------*
*&      Module  MOVE_SCREEN_TO_ITAB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_screen_to_itab INPUT.
  READ TABLE it_9000 INDEX tc_9000-current_line.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  MOVE: ztco_aala_creation_9000_tab TO it_9000.

  MODIFY it_9000 INDEX tc_9000-current_line.
ENDMODULE.                 " MOVE_SCREEN_TO_ITAB  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'EXCUTE'.
      CLEAR: sy-ucomm.
      PERFORM excute_9000_rtn.
    WHEN 'INSERT'.
      CLEAR: sy-ucomm.
      CLEAR: it_9000.
      APPEND it_9000.
    WHEN 'DELETE'.
      CLEAR: sy-ucomm.
      DELETE it_9000 WHERE check = c_check.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  set_initial_value  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_initial_value OUTPUT.
*----- Set Controlling area
  CALL FUNCTION 'K_KOKRS_SET'
       IMPORTING
            e_kokrs   = ztco_aala_creation_9000-kokrs
       EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
  IF sy-subrc <> 0.
    IF sy-msgty = 'E' OR sy-msgty = 'A' OR sy-msgty = 'X'.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      LEAVE PROGRAM.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

*---// Set period
  IF ztco_aala_creation_9000-bdatj IS INITIAL.
    ztco_aala_creation_9000-bdatj = sy-datum(4).
  ENDIF.

  IF ztco_aala_creation_9000-poper IS INITIAL.
    ztco_aala_creation_9000-poper = sy-datum+4(2).
  ENDIF.

  IF ztco_aala_creation_9000-gjahr IS INITIAL.
    ztco_aala_creation_9000-gjahr = sy-datum(4).
  ENDIF.

*---// Get company infomation
  SELECT SINGLE * FROM t001 WHERE bukrs = c_bukrs.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
ENDMODULE.                 " set_initial_value  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  EXCUTE_9000_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excute_9000_rtn.
  PERFORM checing_before_excute.
  PERFORM get_data.
  PERFORM calculate_data.
  PERFORM update_data.

  CALL SCREEN 9100.
ENDFORM.                    " EXCUTE_9000_RTN
*&---------------------------------------------------------------------*
*&      Form  set_fsc_of_shopcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fsc_of_shopcost.
  CLEAR: r_fsc, r_fsc[].

  LOOP AT it_9000.
    MOVE: 'I'  TO r_fsc-sign,
          'EQ' TO r_fsc-option.

    IF it_9000-altmt IS INITIAL.
      MOVE: it_9000-matnr TO r_fsc-low.
    ELSE.
      MOVE: it_9000-altmt TO r_fsc-low.
    ENDIF.

    APPEND r_fsc.
  ENDLOOP.
ENDFORM.                    " set_fsc_of_shopcost
*&---------------------------------------------------------------------*
*&      Form  get_source_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_source_data.
  CLEAR: it_source, it_source[].
  SELECT *
    INTO TABLE it_source
    FROM ztco_aala_source
   WHERE kokrs = ztco_aala_creation_9000-kokrs
     AND gjahr = ztco_aala_creation_9000-gjahr
     AND versn = ztco_aala_creation_9000-versn.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m04 ztco_aala_creation_9000-versn.
  ENDIF.

  READ TABLE it_source WITH KEY vflag = space.
  IF sy-subrc EQ 0.
    MESSAGE e000(zz) WITH text-m25.
  ENDIF.

  READ TABLE it_source INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  sort it_source by matnr.

  MOVE: it_source-klvar TO w_klvar.

  CLEAR: w_versn.
  CASE w_klvar.
    WHEN 'ZPCP'.              "Annual Plan Cost
      MOVE: '311' TO w_versn.

      CONCATENATE ztco_aala_creation_9000-bdatj '05' INTO w_period.
    WHEN 'PPC1'.              "Standard Cost
      MOVE: '000' TO w_versn.

      CASE ztco_aala_creation_9000-poper.
        WHEN '001' OR '002' OR '003'.
          CONCATENATE ztco_aala_creation_9000-bdatj '01' INTO w_period.
        WHEN '004' OR '005' OR '006'.
          CONCATENATE ztco_aala_creation_9000-bdatj '02' INTO w_period.
        WHEN '007' OR '008' OR '009'.
          CONCATENATE ztco_aala_creation_9000-bdatj '03' INTO w_period.
        WHEN '010' OR '011' OR '012'.
          CONCATENATE ztco_aala_creation_9000-bdatj '04' INTO w_period.
      ENDCASE.
  ENDCASE.
ENDFORM.                    " get_source_data
*&---------------------------------------------------------------------*
*&      Form  get_shopcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_shopcost.
  PERFORM set_fsc_of_shopcost.
  PERFORM get_fsc_shopcost.
  PERFORM get_mip_shopcost.

  APPEND LINES OF it_shop_mip TO it_shop.

  PERFORM replace_alternative_fsc.

  CLEAR: it_matnr, it_matnr[].
  LOOP AT it_shop.
    MOVE: it_shop-llv_matnr TO it_matnr-matnr.

    COLLECT it_matnr.

    IF it_shop-shop IS INITIAL.
      MOVE: c_trim TO it_shop-shop.
    ENDIF.

    MODIFY it_shop.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m05.
  ENDIF.
ENDFORM.                    " get_shopcost
*&---------------------------------------------------------------------*
*&      Module  CHECK_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_9000 INPUT.
ENDMODULE.                 " CHECK_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DUTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_duty.
  SELECT matnr dduty
    INTO CORRESPONDING FIELDS OF TABLE it_duty
    FROM ztmm_analy
     FOR ALL ENTRIES IN it_matnr
   WHERE period EQ w_period
     AND matnr  EQ it_matnr-matnr.
ENDFORM.                    " GET_DUTY
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM get_source_data.
  PERFORM get_shopcost.
  PERFORM get_duty.
  PERFORM read_cost_component.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  calculate_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_data.
  PERFORM set_it_9100.
  PERFORM calculate_error_count.
  PERFORM set_it_mip.
  PERFORM set_it_fsc_n_it_model.
ENDFORM.                    " calculate_data
*&---------------------------------------------------------------------*
*&      Form  set_it_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_9100.
  CLEAR: it_9100, it_9100[].

  LOOP AT it_shop.
    CLEAR: it_9100.

    PERFORM get_cost_component USING it_shop-kstar it_shop-elemt
                                     it_9100-zzmsg.

*FIXME
    IF it_shop-kstar >= c_mcost_f AND
       it_shop-kstar <= c_mcost_t.
      PERFORM set_material_cost_for_parts USING it_shop.
    ELSEIF it_shop-kstar >= c_activity.
      PERFORM set_activity_cost_for_parts USING it_shop.
    ELSE.
*      break-point.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " set_it_9100
*&---------------------------------------------------------------------*
*&      Form  READ_COST_COMPONENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cost_component USING p_kstar p_elemt p_zzmsg.

  CHECK p_kstar >= c_mcost_f AND
        p_kstar <= c_mcost_t.

*TEMP FIX
  IF p_kstar = '0000540199'.
    p_elemt = '050'.  "assign dummy ccs.
  ELSE.

    LOOP AT it_tckh2 WHERE kstav <= p_kstar
                       AND kstab >= p_kstar.
    ENDLOOP.
    IF sy-subrc NE 0.
      CONCATENATE text-m09 p_kstar text-m10 INTO p_zzmsg
        SEPARATED BY space.
    ENDIF.

    MOVE: it_tckh2-elemt TO p_elemt.
  ENDIF.
ENDFORM.                    " READ_COST_COMPONENT

*&---------------------------------------------------------------------*
*&      Form  APPEND_MATERIAL_COST_FOR_MIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_material_cost_for_mip.
*  LOOP AT
ENDFORM.                    " APPEND_MATERIAL_COST_FOR_MIP
*&---------------------------------------------------------------------*
*&      Form  get_mip_lower_level_parts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MIP  text
*----------------------------------------------------------------------*
FORM get_mip_lower_level_parts USING p_mip STRUCTURE it_shop_mip
                                     p_fsc p_mipcd   p_menge.
  DATA: lt_shop_mip LIKE it_shop_mip OCCURS 0 WITH HEADER LINE.

* read activity
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_shop_mip
    FROM ztco_shopcost
   WHERE kokrs     EQ   ztco_aala_creation_9000-kokrs
     AND bdatj     EQ   ztco_aala_creation_9000-bdatj
     AND poper     EQ   ztco_aala_creation_9000-poper
     AND klvar     EQ   w_klvar
     AND versn     EQ   w_versn
     AND fsc_matnr EQ   p_mip-llv_matnr
     AND typps     =    'E'.
*---// Read Material Cost(Except MIP)
  SELECT *
    APPENDING CORRESPONDING FIELDS OF TABLE lt_shop_mip
    FROM ztco_shopcost
   WHERE kokrs     EQ   ztco_aala_creation_9000-kokrs
     AND bdatj     EQ   ztco_aala_creation_9000-bdatj
     AND poper     EQ   ztco_aala_creation_9000-poper
     AND klvar     EQ   w_klvar
     AND versn     EQ   w_versn
     AND fsc_matnr EQ   p_mip-llv_matnr
     AND typps     =    'M'.
*     AND kstar     BETWEEN c_mcost_f and c_mcost_t.   "UD1K919862

  IF sy-subrc NE 0.

*FIXME
    IF p_mip-shop = 'MXSX'.
      MOVE p_mip TO it_shop.
      it_shop-kstar = '0000540199'.
      APPEND it_shop.
    ENDIF.
    EXIT.
  ENDIF.

  SORT lt_shop_mip BY kokrs bdatj poper klvar versn record_type
                      fsc_matnr shop llv_matnr.
  LOOP AT lt_shop_mip WHERE kstar EQ c_mip.
    AT NEW llv_matnr.
      CONTINUE.
    ENDAT.

    DELETE lt_shop_mip.
  ENDLOOP.

  LOOP AT lt_shop_mip.
    READ TABLE it_shop WITH KEY lt_shop_mip.
    IF sy-subrc EQ 0.
      DELETE it_shop INDEX sy-tabix.
    ENDIF.

    IF lt_shop_mip-shop IS INITIAL.
      MOVE: p_mip-shop TO lt_shop_mip-shop.
    ENDIF.

    IF     lt_shop_mip-kstar >= c_mcost_f AND
           lt_shop_mip-kstar <= c_mcost_t.
      lt_shop_mip-menge = lt_shop_mip-menge * p_menge.

      PERFORM set_shop_mip_for_matl_cost USING lt_shop_mip
                                               p_fsc
                                               p_mipcd.
    ELSEIF lt_shop_mip-kstar EQ c_mip.
      lt_shop_mip-menge = lt_shop_mip-menge * p_menge.
      PERFORM get_mip_lower_level_parts USING lt_shop_mip
                                              p_fsc
                                              p_mipcd
                                              lt_shop_mip-menge.
    ELSE.
      lt_shop_mip-menge = 1 * p_menge.
      PERFORM set_shop_mip_for_act_cost USING lt_shop_mip
                                              p_fsc
                                              p_mipcd.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_mip_lower_level_parts
*&---------------------------------------------------------------------*
*&      Form  get_fsc_shopcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fsc_shopcost.
  CLEAR: it_shop, it_shop[].

*---// Read Activity Cost
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_shop
    FROM ztco_shopcost
   WHERE       kokrs      EQ ztco_aala_creation_9000-kokrs
     AND       bdatj      EQ ztco_aala_creation_9000-bdatj
     AND       poper      EQ ztco_aala_creation_9000-poper
     AND       klvar      EQ w_klvar
     AND       versn      EQ w_versn
     AND       fsc_matnr  IN r_fsc
     AND       typps      =  'E'
*     AND NOT ( kstar BETWEEN c_mcost_f and c_mcost_t or
*               KSTAR      eq C_MIP )
     AND     ( shop     LIKE c_pressshop OR
               shop     LIKE c_bodyshop  OR
               shop     LIKE c_paintshop OR
               shop     LIKE c_egnshop      ).

  MOVE: 1 TO it_shop-menge.
  MODIFY it_shop TRANSPORTING menge WHERE fsc_matnr >= space.

*---// Read Material Cost(Except MIP)
  SELECT *
    APPENDING CORRESPONDING FIELDS OF TABLE it_shop
    FROM ztco_shopcost
   WHERE kokrs     EQ   ztco_aala_creation_9000-kokrs
     AND bdatj     EQ   ztco_aala_creation_9000-bdatj
     AND poper     EQ   ztco_aala_creation_9000-poper
     AND klvar     EQ   w_klvar
     AND versn     EQ   w_versn
     AND fsc_matnr IN   r_fsc
*    AND typps     =    'E'               "UD1K919876
     AND typps     =    'M'                                 "UD1K919876
     AND kstar     BETWEEN c_mcost_f AND c_mcost_t.

ENDFORM.                    " get_fsc_shopcost
*&---------------------------------------------------------------------*
*&      Form  get_mip_shopcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mip_shopcost.
  DATA: lt_shop_mip LIKE it_shop_mip OCCURS 0 WITH HEADER LINE.

  CLEAR: it_shop_mip, it_shop_mip[].

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_shop_mip
    FROM ztco_shopcost
   WHERE kokrs     EQ   ztco_aala_creation_9000-kokrs
     AND bdatj     EQ   ztco_aala_creation_9000-bdatj
     AND poper     EQ   ztco_aala_creation_9000-poper
     AND klvar     EQ   w_klvar
     AND versn     EQ   w_versn
     AND fsc_matnr IN   r_fsc
     AND kstar     EQ   c_mip.

  SORT lt_shop_mip BY kokrs bdatj poper klvar versn record_type
                      fsc_matnr shop llv_matnr.
  LOOP AT lt_shop_mip.
    AT NEW llv_matnr.
      CONTINUE.
    ENDAT.

    DELETE lt_shop_mip.
  ENDLOOP.

  LOOP AT lt_shop_mip.
    IF lt_shop_mip-shop+2(1) EQ 'E'.
      MOVE: lt_shop_mip-llv_matnr TO it_egntm-matnr.
      APPEND it_egntm.
    ENDIF.

    READ TABLE it_shop WITH KEY lt_shop_mip.
    IF sy-subrc EQ 0.
      DELETE it_shop INDEX sy-tabix.
    ENDIF.

    IF lt_shop_mip-shop IS INITIAL.
      MOVE: c_trim TO lt_shop_mip-shop.
    ENDIF.

    PERFORM get_mip_lower_level_parts USING lt_shop_mip
                                            lt_shop_mip-fsc_matnr
                                            lt_shop_mip-llv_matnr
                                            lt_shop_mip-menge.
  ENDLOOP.
ENDFORM.                    " get_mip_shopcost
*&---------------------------------------------------------------------*
*&      Form  set_material_cost_for_parts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP  text
*----------------------------------------------------------------------*
FORM set_material_cost_for_parts USING p_shop STRUCTURE it_shop.
  MOVE: p_shop-kokrs                  TO it_9100-kokrs,
        p_shop-fsc_matnr              TO it_9100-matnr,
        ztco_aala_creation_9000-gjahr TO it_9100-gjahr,
        p_shop-llv_matnr              TO it_9100-idnrk,
        p_shop-kstar                  TO it_9100-kstar,
        p_shop-elemt                  TO it_9100-elemt,
        p_shop-mipcd                  TO it_9100-mipcd,
        p_shop-shop                   TO it_9100-shop,
        p_shop-menge                  TO it_9100-menge,
        p_shop-meeht                  TO it_9100-meins,
        sy-uname                      TO it_9100-ernam,
        sy-datum                      TO it_9100-erdat,
        sy-uzeit                      TO it_9100-erzet,
        sy-uname                      TO it_9100-aenam,
        sy-datum                      TO it_9100-aedat,
        sy-uzeit                      TO it_9100-aezet.

  READ TABLE it_source WITH KEY matnr = p_shop-llv_matnr binary search.
*                                kokrs = p_shop-kokrs
*                                versn = ztco_aala_creation_9000-versn.
  IF sy-subrc NE 0.
    MOVE: text-m11 TO it_9100-zzmsg.
  ENDIF.

  IF it_source-netpr EQ 0.
    MOVE: text-m23 TO it_9100-zzmsg.
  ENDIF.

  IF it_source-urzla EQ space.
    MOVE: text-m24 TO it_9100-zzmsg.
  ENDIF.

  IF it_source-kstar EQ space.
    MOVE: text-m26 TO it_9100-zzmsg.
  ENDIF.

  MOVE: it_source-kstar   TO it_9100-kstar,
        it_source-versn   TO it_9100-versn,
        it_source-urzla   TO it_9100-urzla,
        it_source-stprs   TO it_9100-stprs,
        it_source-aaprs   TO it_9100-aaprs,
        it_source-netpr   TO it_9100-netpr,
        it_source-peinh   TO it_9100-peinh.


  READ TABLE it_duty WITH KEY matnr = it_shop-llv_matnr.
  IF sy-subrc NE 0.
    PERFORM read_duty_from_hts_code USING p_shop it_9100-dduty.
  ELSE.
    MOVE: it_duty-dduty TO it_9100-dduty.
  ENDIF.

  APPEND it_9100.
ENDFORM.                    " set_material_cost_for_parts
*&---------------------------------------------------------------------*
*&      Form  set_activity_cost_for_parts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP  text
*----------------------------------------------------------------------*
FORM set_activity_cost_for_parts USING p_shop STRUCTURE it_shop.
  CLEAR: it_9100.

  READ TABLE it_9100 WITH KEY matnr = p_shop-fsc_matnr
                              idnrk = p_shop-llv_matnr
                              mipcd = p_shop-mipcd
                              shop  = p_shop-shop
                              kstar = p_shop-kstar
                              elemt = p_shop-elemt
*                              kokrs = p_shop-kokrs
*                              gjahr = ztco_aala_creation_9000-gjahr
*                              versn = ztco_aala_creation_9000-versn
                          binary search.
  IF sy-subrc EQ 0.
    MOVE: 'US'                          TO it_9100-urzla,
          1                             TO it_9100-netpr,
          1                             TO it_9100-aaprs.

    it_9100-stprs = it_9100-stprs + p_shop-wertn * p_shop-menge.

    MODIFY it_9100 INDEX sy-tabix.
  ELSE.
    MOVE: p_shop-kokrs                  TO it_9100-kokrs,
          p_shop-fsc_matnr              TO it_9100-matnr,
          ztco_aala_creation_9000-gjahr TO it_9100-gjahr,
          p_shop-llv_matnr              TO it_9100-idnrk,
          p_shop-kstar                  TO it_9100-kstar,
          p_shop-elemt                  TO it_9100-elemt,
          p_shop-mipcd                  TO it_9100-mipcd,
          p_shop-shop                   TO it_9100-shop,
          'US'                          TO it_9100-urzla,
          1                             TO it_9100-netpr,
          1                             TO it_9100-aaprs,
          1                             TO it_9100-peinh,
          ztco_aala_creation_9000-versn TO it_9100-versn,
          'EA'                          TO it_9100-meins,
          sy-uname                      TO it_9100-ernam,
          sy-datum                      TO it_9100-erdat,
          sy-uzeit                      TO it_9100-erzet,
          sy-uname                      TO it_9100-aenam,
          sy-datum                      TO it_9100-aedat,
          sy-uzeit                      TO it_9100-aezet.

    it_9100-stprs = p_shop-wertn * p_shop-menge.

    MOVE: 1                             TO it_9100-menge.

    IF it_source-kstar EQ space.
      MOVE: text-m26 TO it_9100-zzmsg.
    ENDIF.

    APPEND it_9100.
    sort it_9100 by matnr idnrk mipcd shop kstar elemt.

  ENDIF.
ENDFORM.                    " set_activity_cost_for_parts
*&---------------------------------------------------------------------*
*&      Form  get_cost_component
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cost_component.
  SELECT *
    INTO TABLE it_tckh2
    FROM tckh2
   WHERE ktopl = t001-ktopl
     AND elehk = c_elehk.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m07 text-m08.
  ENDIF.
ENDFORM.                    " get_cost_component
*&---------------------------------------------------------------------*
*&      Form  set_it_mip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_mip.
  PERFORM set_aala_amount_for_mip.
  PERFORM set_origin_for_mip.
ENDFORM.                    " set_it_mip
*&---------------------------------------------------------------------*
*&      Form  set_mip_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mip_data.
  IF it_9100-shop+2(1) EQ 'T'.
    PERFORM set_mip_data_for_trim.
  ELSE.
    PERFORM set_mip_data_for_other_shop.
  ENDIF.
ENDFORM.                    " set_mip_data
*&---------------------------------------------------------------------*
*&      Form  set_mip_origin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mip_origin USING p_mipcd.
  CLEAR: it_origin.

  IF it_9100-shop+2(1) EQ 'T'.
    PERFORM set_mip_origin_for_trim USING p_mipcd.
  ELSE.
    PERFORM set_mip_origin_for_other_shop USING p_mipcd.
  ENDIF.
ENDFORM.                    " set_mip_origin
*&---------------------------------------------------------------------*
*&      Form  set_aala_amount_for_mip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_aala_amount_for_mip.
  DATA: lw_mipcd LIKE it_mip-mipcd,
        lw_index LIKE sy-index.

  REFRESH: it_mip, it_origin.

  SORT it_9100 BY matnr shop mipcd matnr kstar elemt.
  LOOP AT it_9100.
    CLEAR: it_mip.

    IF it_9100-shop+2(1) EQ 'E'.
      READ TABLE it_egntm WITH KEY matnr = it_9100-mipcd.
      IF sy-subrc EQ 0.                  "Engine/TM
        MOVE: it_egntm-matnr TO lw_mipcd.
      ELSE.
        CLEAR: lw_mipcd.
      ENDIF.
    ELSE.
      CLEAR: lw_mipcd.
    ENDIF.

    READ TABLE it_mip WITH KEY matnr = it_9100-matnr
                               mipcd = lw_mipcd
                               shop  = it_9100-shop
                           binary search.
*                               kokrs = it_9100-kokrs
*                               gjahr = it_9100-gjahr
*                               versn = it_9100-versn.
    IF sy-subrc EQ 0.
      MOVE: sy-tabix TO lw_index.

      PERFORM set_mip_data.
      PERFORM set_mip_origin USING lw_mipcd.

      MODIFY it_mip INDEX lw_index.
    ELSE.
      MOVE: it_9100-kokrs TO it_mip-kokrs,
            it_9100-matnr TO it_mip-matnr,
            it_9100-gjahr TO it_mip-gjahr,
            lw_mipcd       TO it_mip-mipcd,
            it_9100-shop  TO it_mip-shop,
            it_9100-versn TO it_mip-versn.

      PERFORM set_mip_data.
      PERFORM set_mip_origin USING lw_mipcd.

      APPEND it_mip.
      sort it_mip by matnr mipcd shop.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " set_aala_amount_for_mip
*&---------------------------------------------------------------------*
*&      Form  set_origin_for_mip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_origin_for_mip.
* SORT it_origin BY matnr mipcd shop wrbtr.  "UD1K919880
  DELETE it_origin WHERE urzla EQ 'US'
                      OR urzla EQ 'CA'.

  SORT it_origin ASCENDING BY  matnr mipcd shop             "UD1K919880
  DESCENDING   wrbtr. ""UD1K919880

  DATA: lw_index TYPE i.
  LOOP AT it_origin.
    AT NEW shop.
      CLEAR: lw_index.
    ENDAT.

    lw_index = lw_index + 1.
    MOVE: lw_index TO it_origin-index.

    MODIFY it_origin.
  ENDLOOP.

  LOOP AT it_mip.
    LOOP AT it_origin WHERE matnr = it_mip-matnr
                        AND mipcd = it_mip-mipcd
                        AND shop  = it_mip-shop.
      CASE it_origin-index.
        WHEN 1.
          it_mip-fstld = it_origin-urzla.
          it_mip-fstpp = it_origin-wrbtr / it_mip-wrbtr * 100.
          it_mip-fsttr = it_origin-wrbtr.
        WHEN 2.
          it_mip-sndld = it_origin-urzla.
          it_mip-sndpp = it_origin-wrbtr / it_mip-wrbtr * 100.
          it_mip-sndtr = it_origin-wrbtr.
      ENDCASE.
    ENDLOOP.

    MOVE: sy-uname TO it_mip-ernam,
          sy-datum TO it_mip-erdat,
          sy-uzeit TO it_mip-erzet,
          sy-uname TO it_mip-aenam,
          sy-datum TO it_mip-aedat,
          sy-uzeit TO it_mip-aezet.

    MODIFY it_mip.
  ENDLOOP.
ENDFORM.                    " set_origin_for_mip
*&---------------------------------------------------------------------*
*&      Form  set_shop_mip_for_matl_cost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SHOP_MIP  text
*      -->P_P_MIP  text
*      -->P_P_MIPCD  text
*----------------------------------------------------------------------*
FORM set_shop_mip_for_matl_cost USING p_shop_mip STRUCTURE it_shop_mip
                                      p_fsc
                                      p_mipcd.
  SELECT SINGLE *
    FROM ztco_shopcost
   WHERE kokrs     EQ   ztco_aala_creation_9000-kokrs
     AND bdatj     EQ   ztco_aala_creation_9000-bdatj
     AND poper     EQ   ztco_aala_creation_9000-poper
     AND klvar     EQ   w_klvar
     AND versn     EQ   w_versn
     AND fsc_matnr EQ   p_shop_mip-llv_matnr.
  IF sy-subrc EQ 0.
    PERFORM get_mip_lower_level_parts USING p_shop_mip
                                            p_fsc
                                            p_mipcd
                                            p_shop_mip-menge.
  ELSE.
    CLEAR: it_shop_mip.
    MOVE: p_shop_mip TO it_shop_mip,
          p_fsc      TO it_shop_mip-fsc_matnr,
          p_mipcd    TO it_shop_mip-mipcd.

    IF it_shop_mip-shop IS INITIAL.
      MOVE: c_trim TO it_shop_mip-shop.
    ENDIF.

    APPEND it_shop_mip.
  ENDIF.
ENDFORM.                    " set_shop_mip_for_matl_cost
*&---------------------------------------------------------------------*
*&      Form  set_shop_mip_for_act_cost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SHOP_MIP  text
*      -->P_P_MIP  text
*      -->P_P_MIPCD  text
*----------------------------------------------------------------------*
FORM set_shop_mip_for_act_cost USING p_shop_mip STRUCTURE it_shop_mip
                                     p_fsc
                                     p_mipcd.
  CLEAR: it_shop_mip.

  MOVE: p_shop_mip TO it_shop_mip,
        p_fsc      TO it_shop_mip-fsc_matnr,
        p_mipcd    TO it_shop_mip-mipcd.

  IF it_shop_mip-shop IS INITIAL.
    MOVE: c_trim TO it_shop_mip-shop.
  ENDIF.

  APPEND it_shop_mip.
ENDFORM.                    " set_shop_mip_for_act_cost
*&---------------------------------------------------------------------*
*&      Form  set_it_fsc_n_it_model
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_fsc_n_it_model.
  READ TABLE it_source INDEX 1.

  REFRESH: it_fsc, it_model.
  CALL FUNCTION 'Z_FCO_AALA_CALC_FSC_MODEL'
       EXPORTING
            i_makrt        = ztco_aala_creation_9000-makrt
            i_bdatj        = ztco_aala_creation_9000-bdatj
            i_poper        = ztco_aala_creation_9000-poper
            i_zver_des     = it_source-zver_des
       TABLES
            t_fsc_qty      = it_9000
            t_mip          = it_mip
            t_fsc          = it_fsc
            t_model        = it_model
       EXCEPTIONS
            quantity_error = 1
            program_error  = 2
            OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
ENDFORM.                    " set_it_fsc_n_it_model
*&---------------------------------------------------------------------*
*&      Form  update_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_data.
  PERFORM set_it_parts.
  PERFORM delete_data.
  PERFORM insert_data.
ENDFORM.                    " update_data
*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data.
  PERFORM set_fsc_for_delete.

  DELETE FROM ztco_aala_parts
   WHERE kokrs = ztco_aala_creation_9000-kokrs
     AND gjahr = ztco_aala_creation_9000-gjahr
     AND versn = ztco_aala_creation_9000-versn
     AND matnr IN r_fsc.

  DELETE FROM ztco_aala_mip
   WHERE kokrs = ztco_aala_creation_9000-kokrs
     AND gjahr = ztco_aala_creation_9000-gjahr
     AND versn = ztco_aala_creation_9000-versn
     AND matnr IN r_fsc.

  DELETE FROM ztco_aala_mip_bk
   WHERE kokrs = ztco_aala_creation_9000-kokrs
     AND gjahr = ztco_aala_creation_9000-gjahr
     AND versn = ztco_aala_creation_9000-versn
     AND matnr IN r_fsc.

  DELETE FROM ztco_aala_fsc
   WHERE kokrs = ztco_aala_creation_9000-kokrs
     AND gjahr = ztco_aala_creation_9000-gjahr
     AND versn = ztco_aala_creation_9000-versn
     AND matnr IN r_fsc.

  DELETE FROM ztco_aala_fsc_bk
   WHERE kokrs = ztco_aala_creation_9000-kokrs
     AND gjahr = ztco_aala_creation_9000-gjahr
     AND versn = ztco_aala_creation_9000-versn
     AND matnr IN r_fsc.

  DELETE FROM ztco_aala_model
   WHERE kokrs = ztco_aala_creation_9000-kokrs
     AND gjahr = ztco_aala_creation_9000-gjahr
     AND versn = ztco_aala_creation_9000-versn
     AND model = ztco_aala_creation_9000-model.

  DELETE FROM ztco_aala_mdl_bk
   WHERE kokrs = ztco_aala_creation_9000-kokrs
     AND gjahr = ztco_aala_creation_9000-gjahr
     AND versn = ztco_aala_creation_9000-versn
     AND model = ztco_aala_creation_9000-model.

  COMMIT WORK AND WAIT.
ENDFORM.                    " delete_data
*&---------------------------------------------------------------------*
*&      Form  replace_alternative_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM replace_alternative_fsc.
  LOOP AT it_9000 WHERE altmt > space.
    LOOP AT it_shop WHERE fsc_matnr EQ it_9000-altmt.
      MOVE: it_9000-matnr TO it_shop-fsc_matnr.
      APPEND it_shop.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " replace_alternative_fsc
*&---------------------------------------------------------------------*
*&      Form  insert_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_data.
  INSERT ztco_aala_parts FROM TABLE it_parts ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m14.
  ENDIF.

  INSERT ztco_aala_mip FROM TABLE it_mip ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m14.
  ENDIF.

  INSERT ztco_aala_fsc FROM TABLE it_fsc ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m14.
  ENDIF.

  INSERT ztco_aala_model FROM TABLE it_model ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m14.
  ELSE.
    COMMIT WORK AND WAIT.
    MESSAGE s000(zz) WITH text-m15.
  ENDIF.
ENDFORM.                    " insert_data
*&---------------------------------------------------------------------*
*&      Form  calculate_error_count
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_error_count.
  DATA: lw_msg01(50),
        lw_msg02(50).

  CLEAR: w_error, w_total, w_success.

  DESCRIBE TABLE it_9100 LINES w_total.
  LOOP AT it_9100.

    IF it_9100-zzmsg IS INITIAL.
      IF it_9100-urzla IS INITIAL.
        MOVE: text-m22 TO it_9100-zzmsg.
        w_error = w_error + 1.
        MOVE: c_error TO it_9100-icon.
      ELSE.
        MOVE: c_success TO it_9100-icon.
      ENDIF.
    ELSE.
      w_error = w_error + 1.
      MOVE: c_error TO it_9100-icon.
    ENDIF.

    MODIFY it_9100.
  ENDLOOP.

  w_success = w_total - w_error.

  CHECK w_error > 0.


  CONCATENATE text-m18 w_total
         INTO lw_msg01 SEPARATED BY space.

  CONCATENATE text-m19 w_success text-m20 w_error
         INTO lw_msg02 SEPARATED BY space.

  MESSAGE s000(zz) WITH lw_msg01 lw_msg02.

  CALL SCREEN 9100.

  LEAVE TO SCREEN sy-dynnr.
ENDFORM.                    " calculate_error_count
*&---------------------------------------------------------------------*
*&      Form  set_it_parts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_parts.
  REFRESH it_parts.

  LOOP AT it_9100.
    CLEAR: it_parts.

    MOVE-CORRESPONDING it_9100 TO it_parts.

*   APPEND it_parts.            "UD1K919868
    COLLECT it_parts.                                       "UD1K919868
  ENDLOOP.
ENDFORM.                    " set_it_parts
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  CONCATENATE: 'WC_CONTAINER_' sy-dynnr INTO w_container.
  ASSIGN:      (w_container)            TO   <container>.

  IF <container> IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_field_catalog.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv.
    PERFORM sssign_event.
  ENDIF.
ENDMODULE.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  dbl_click_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM dbl_click_9000 USING    p_e_column_fieldname
                             p_es_row_no_row_id.

ENDFORM.                    " dbl_click_9000
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
*- Create Container('GRID_CONTAINER') with Custom Control on screen

  CONCATENATE: 'WC_CONTAINER_' sy-dynnr INTO w_container,
               'WC_CONTROL_'   sy-dynnr INTO w_control,
               'WC_ALV_'       sy-dynnr INTO w_alv.

  ASSIGN: (w_container) TO <container>,
          (w_control)   TO <control>,
          (w_alv)       TO <alv>.

  CREATE OBJECT <container>
         EXPORTING container_name = <control>
         EXCEPTIONS
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT <alv>
         EXPORTING i_parent      = <container>
                   i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  CASE sy-dynnr.
    WHEN '9100'.
      PERFORM set_attributes_alv_9100.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog.
  CASE sy-dynnr.
    WHEN '9100'.
      PERFORM build_field_catalog_9100.
  ENDCASE.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv.
* Display data
  CONCATENATE: 'WC_ALV_'    sy-dynnr      INTO w_alv,
               c_structure  sy-dynnr      INTO w_structure,
               'IT_'        sy-dynnr '[]' INTO w_itab.

  ASSIGN: (w_alv)       TO <alv>,
          (w_itab)      TO <itab>.


  CALL METHOD <alv>->set_table_for_first_display
     EXPORTING i_structure_name = w_structure
               is_layout        = w_is_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = <itab>.
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event.

ENDFORM.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9100.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9100
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog_9100.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'IDNRK'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MIPCD'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  ' ' 'EMPHASIZE'   'C400',
                                  'E' 'KEY'         'X',

                                  'S' 'SHOP'        ' ',
                                  'E' 'NO_SIGN'     'X',

                                  'S' 'KOKRS'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'GJAHR'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'VERSN'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ERNAM'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ERDAT'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ERZET'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'AENAM'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'AEDAT'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'AEZET'       ' ',
                                  'E' 'NO_OUT'      'X'.

ENDFORM.                    " build_field_catalog_9100
*&---------------------------------------------------------------------*
*&      Form  set_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fieldname.
  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  MOVE: sy-repid TO w_repid.
  CONCATENATE c_structure sy-dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       CHANGING
            ct_fieldcat        = it_fieldname.
ENDFORM.                    " set_fieldname
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_3884   text
*      -->P_3885   text
*      -->P_3886   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check filed catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    IF p_fieldcat-col_pos IS INITIAL.
      ADD 1 TO w_cnt.
      p_fieldcat-col_pos = w_cnt.
    ENDIF.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Module  user_command_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " user_command_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  checing_before_excute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM checing_before_excute.
  READ TABLE it_9000 INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m21.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.
ENDFORM.                    " checing_before_excute
*&---------------------------------------------------------------------*
*&      Form  set_mip_data_for_trim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mip_data_for_trim.
  DATA: lw_mod   TYPE f,
        lw_propo LIKE ztco_aala_model-propo.

  lw_propo = it_9100-aaprs / it_9100-netpr * 100.
  IF lw_propo >= 70.
    lw_propo = 100.
  ELSE.
    lw_mod = lw_propo MOD 5.
    lw_propo = lw_propo - lw_mod.

    IF lw_mod >= '2.5'.
      lw_propo = lw_propo + 5.
    ENDIF.
  ENDIF.

  it_mip-wrbtr =     it_mip-wrbtr +
                 ( ( it_9100-stprs - it_9100-dduty ) *
                     it_9100-menge / it_9100-peinh ).

*  IF ( it_9100-kstar >= c_mcost_f AND      "Material Cost
*       it_9100-kstar <= c_mcost_t )
*  OR ( it_9100-kstar = c_mip AND it_9100-mipcd = space ).
  IF it_9100-kstar >= c_activity.
    it_mip-acost =   it_mip-acost +
                   ( it_9100-stprs * it_9100-menge / it_9100-peinh ).

    it_mip-aaprs =   it_mip-aaprs +
                   ( it_9100-stprs * it_9100-menge / it_9100-peinh ).
  ELSE.
    it_mip-mcost =     it_mip-mcost +
                   ( ( it_9100-stprs - it_9100-dduty ) *
                       it_9100-menge / it_9100-peinh ).

    it_mip-aaprs =     it_mip-aaprs +
                 ( ( ( it_9100-stprs - it_9100-dduty ) *
                       it_9100-menge / it_9100-peinh ) *
                       lw_propo / 100 ).
  ENDIF.
ENDFORM.                    " set_mip_data_for_trim
*&---------------------------------------------------------------------*
*&      Form  set_mip_data_for_other_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mip_data_for_other_shop.
  it_mip-wrbtr =     it_mip-wrbtr +
                 ( ( it_9100-stprs - it_9100-dduty ) *
                     it_9100-menge / it_9100-peinh ).

*  IF ( it_9100-kstar >= c_mcost_f AND      "Material Cost
*      it_9100-kstar <= c_mcost_t )
*  OR ( it_9100-kstar = c_mip AND it_9100-mipcd = space ).

  IF it_9100-kstar >= c_activity.
    it_mip-acost =   it_mip-acost +
                   ( it_9100-stprs * it_9100-menge / it_9100-peinh ).

    it_mip-aaprs =   it_mip-aaprs +
                   ( it_9100-stprs * it_9100-menge / it_9100-peinh ).
  ELSE.
    it_mip-mcost =     it_mip-mcost +
                   ( ( it_9100-stprs - it_9100-dduty ) *
                       it_9100-menge / it_9100-peinh ).

    it_mip-aaprs =     it_mip-aaprs +
                 ( ( ( it_9100-stprs - it_9100-dduty ) *
                       it_9100-menge / it_9100-peinh ) *
                       it_9100-aaprs / it_9100-netpr ).
  ENDIF.
ENDFORM.                    " set_mip_data_for_other_shop
*&---------------------------------------------------------------------*
*&      Form  set_mip_origin_for_other_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mip_origin_for_other_shop USING p_mipcd.
* US/CA -> KR change???
  IF it_9100-urzla EQ 'US' OR
     it_9100-urzla EQ 'CA'.
    MOVE: it_9100-matnr TO it_origin-matnr,
          p_mipcd       TO it_origin-mipcd,
          it_9100-shop  TO it_origin-shop,
          'KR'           TO it_origin-urzla.
    it_origin-wrbtr =  ( it_9100-stprs - it_9100-dduty ) *
                         it_9100-menge / it_9100-peinh -
                   ( ( ( it_9100-stprs - it_9100-dduty ) *
                         it_9100-menge / it_9100-peinh ) *
                         it_9100-aaprs / it_9100-netpr ).

    COLLECT it_origin.
  ELSE.
    MOVE: it_9100-matnr TO it_origin-matnr,
          p_mipcd       TO it_origin-mipcd,
          it_9100-shop  TO it_origin-shop,
          it_9100-urzla TO it_origin-urzla.

    it_origin-wrbtr = ( it_9100-stprs - it_9100-dduty ) *
                        it_9100-menge / it_9100-peinh.

    COLLECT it_origin.
  ENDIF.
ENDFORM.                    " set_mip_origin_for_other_shop
*&---------------------------------------------------------------------*
*&      Form  set_mip_origin_for_trim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mip_origin_for_trim USING p_mipcd.
  DATA: lw_mod   TYPE f,
        lw_propo LIKE ztco_aala_model-propo.

* only for US/CA calculation (ANDY)
  lw_propo = it_9100-aaprs / it_9100-netpr * 100.
  IF lw_propo >= 70.
    lw_propo = 100.
  ELSE.
    lw_mod = lw_propo MOD 5.
    lw_propo = lw_propo - lw_mod.

    IF lw_mod >= '2.5'.
      lw_propo = lw_propo + 5.
    ENDIF.
  ENDIF.

* US/CA -> KR change???
  IF it_9100-urzla EQ 'US' OR
     it_9100-urzla EQ 'CA'.
    MOVE: it_9100-matnr TO it_origin-matnr,
          p_mipcd       TO it_origin-mipcd,
          it_9100-shop  TO it_origin-shop,
          'KR'          TO it_origin-urzla.
    it_origin-wrbtr =  ( it_9100-stprs - it_9100-dduty ) *
                         it_9100-menge / it_9100-peinh -
                   ( ( ( it_9100-stprs - it_9100-dduty ) *
                         it_9100-menge / it_9100-peinh ) *
                         lw_propo / 100 ).

    COLLECT it_origin.
  ELSE.
    MOVE: it_9100-matnr TO it_origin-matnr,
          p_mipcd       TO it_origin-mipcd,
          it_9100-shop  TO it_origin-shop,
          it_9100-urzla TO it_origin-urzla.

    it_origin-wrbtr = ( it_9100-stprs - it_9100-dduty ) *
                        it_9100-menge / it_9100-peinh.

    COLLECT it_origin.
  ENDIF.
ENDFORM.                    " set_mip_origin_for_trim
*&---------------------------------------------------------------------*
*&      Form  read_duty_from_hts_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_duty_from_hts_code USING p_shop STRUCTURE it_shop
                                   p_duty.
  IF it_source-urzla EQ 'US' OR
     it_source-urzla EQ 'CA'.
    p_duty = 0.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM marc WHERE matnr = p_shop-llv_matnr
                              AND werks = p_shop-werks.
  IF sy-subrc NE 0.
    p_duty = 0.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM a902 WHERE kappl = 'M'
                              AND kschl = 'ZOA1'
                              AND stawn = marc-stawn.
  IF sy-subrc NE 0.
    p_duty = 0.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM konp WHERE knumh = a902-knumh.
  IF sy-subrc NE 0.
    p_duty = 0.
    EXIT.
  ENDIF.

  p_duty = ( it_shop-wertn / ( 1 + konp-kbetr ) ) * konp-kbetr / 1000.
ENDFORM.                    " read_duty_from_hts_code
*&---------------------------------------------------------------------*
*&      Form  check_shop_cost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTCO_AALA_CREATION_9000_TAB_MA  text
*----------------------------------------------------------------------*
FORM check_shop_cost USING p_matnr.
  SELECT SINGLE klvar INTO w_klvar
    FROM ztco_aala_source
   WHERE kokrs = ztco_aala_creation_9000-kokrs
     AND gjahr = ztco_aala_creation_9000-gjahr
     AND versn = ztco_aala_creation_9000-versn.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m04 ztco_aala_creation_9000-versn.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

  CLEAR: w_versn.
  CASE w_klvar.
    WHEN 'ZPCP'.              "Annual Plan Cost
      MOVE: '311' TO w_versn.

      CONCATENATE ztco_aala_creation_9000-bdatj '05' INTO w_period.
    WHEN 'PPC1'.              "Standard Cost
      MOVE: '000' TO w_versn.

      CASE ztco_aala_creation_9000-poper.
        WHEN '001' OR '002' OR '003'.
          CONCATENATE ztco_aala_creation_9000-bdatj '01' INTO w_period.
        WHEN '004' OR '005' OR '006'.
          CONCATENATE ztco_aala_creation_9000-bdatj '02' INTO w_period.
        WHEN '007' OR '008' OR '009'.
          CONCATENATE ztco_aala_creation_9000-bdatj '03' INTO w_period.
        WHEN '010' OR '011' OR '012'.
          CONCATENATE ztco_aala_creation_9000-bdatj '04' INTO w_period.
      ENDCASE.
  ENDCASE.

  SELECT SINGLE *
    FROM ztco_shopcost
   WHERE       kokrs      EQ ztco_aala_creation_9000-kokrs
     AND       bdatj      EQ ztco_aala_creation_9000-bdatj
     AND       poper      EQ ztco_aala_creation_9000-poper
     AND       klvar      EQ w_klvar
     AND       versn      EQ w_versn
     AND       fsc_matnr  EQ p_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH p_matnr text-m27.
  ENDIF.
ENDFORM.                    " check_shop_cost
*&---------------------------------------------------------------------*
*&      Form  SET_FSC_FOR_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fsc_for_delete.
  CLEAR: r_fsc, r_fsc[].

  LOOP AT it_9000.
    MOVE: 'I'  TO r_fsc-sign,
          'EQ' TO r_fsc-option.

    MOVE: it_9000-matnr TO r_fsc-low.

    APPEND r_fsc.
  ENDLOOP.
ENDFORM.                    " SET_FSC_FOR_DELETE
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE table_control_lines OUTPUT.
  DESCRIBE TABLE it_9000 LINES tc_9000-lines.
ENDMODULE.                 " TABLE_CONTROL_LINES  OUTPUT
