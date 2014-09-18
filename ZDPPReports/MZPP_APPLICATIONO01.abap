*--------------------------------------------------------
*   INCLUDE MZPP_APPLICATIONO01
*
*--------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_100 OUTPUT.
  CLEAR: it_menu, it_menu[].

  IF wa_change = ' '.
    it_menu-fcode = 'SAVE'.  APPEND it_menu.
  ENDIF.

  IF sy-tcode = 'ZPPA9999'.
    it_menu-fcode = 'SCHNG'.  APPEND it_menu.
    it_menu-fcode = '0701' .  APPEND it_menu.
  ENDIF.

  SET PF-STATUS 'MAIN'  EXCLUDING it_menu .
  IF wa_init_flg IS INITIAL.
    " Create a Tree Control and insert nodes into it.
    sv_prog  = sy-repid .
    sv_dynnr = '0001'   .
    sv_code  = '0001'   .
    wa_init_flg = 'X'   .
  ENDIF.
  PERFORM set_titlebar .

*Requested by Hur,20041020,changed by wskim
*-----Start
  GET PARAMETER ID 'ZWORKN' FIELD it_0118-order.
  IF sy-ucomm EQ 'PICK ' AND NOT it_0118-order IS INITIAL.
    sv_dynnr = '0102'   .
    sv_code  = '0102'   .
    wa_order = it_0118-order.
    FREE MEMORY ID 'ZWORKN'.
  ENDIF.
*----End
ENDMODULE.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_1001 OUTPUT.
  DESCRIBE TABLE it_219 LINES tc_0101-lines.
  IF wa_flag IS INITIAL.
    PERFORM get_display   .
    wa_flag   = 'X'     .
  ENDIF.

  IF wa_chg_1001_flg = 'X'.
    LOOP AT SCREEN .
      IF screen-group1 = 'CHG'.
        screen-input  = 1    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN .
      IF screen-group1 = 'CHG'.
        screen-input  = 0    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  PERFORM get_instance_charc  USING wa_order .
ENDMODULE.                 " INIT_1001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_1002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_1002 OUTPUT.
  IF  wa_flag IS INITIAL.
    PERFORM get_display   .
    wa_flag = 'X'     .
  ENDIF.
*Requested by Hur,20041020,changed by wskim
*-----Start
  CHECK sy-ucomm = 'PICK'.
  PERFORM get_display.
  PERFORM display_1002 .
*-----End
ENDMODULE.                 " INIT_1002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  get_color_1002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_color_1002 OUTPUT.
  DATA: l_material            LIKE mara-matnr,
        l_instance            LIKE inob-cuobj.
*Requested by Hur,20041020,changed by wskim
*-----Start
  DATA :w_int TYPE i.
  DESCRIBE TABLE it_wosum LINES w_int.
  IF w_int <> 0.
*-----End
    MOVE wa_order TO l_material.
*  CONCATENATE wa_order   it_wosum-extc it_wosum-intc
*    INTO l_material.

    CLEAR: it_result1001, it_result1001[].

    it_result1001-atnam = 'P_UPDATE_ALC_DATE2'.  APPEND it_result1001.
    it_result1001-atnam = 'P_PERF_YN'.           APPEND it_result1001.
    it_result1001-atnam = 'P_UPDATE_ALC_DATE1'.  APPEND it_result1001.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object             = l_material
*     MODE               = 'R'
        ctype              = '001'
      TABLES
        val_table          = it_result1001
        EXCEPTIONS
          no_data            = 1
          error_mode         = 2
          error_object       = 3
          error_value        = 4
          OTHERS             = 5 .

    IF sy-subrc = 0.
      READ TABLE it_result1001 WITH KEY atnam = 'P_UPDATE_ALC_DATE2'.
      IF '0123456789' CA it_result1001-atwrt.
        it_wosum-t_date = it_result1001-atwrt.
      ELSE.
        READ TABLE it_result1001 WITH KEY atnam = 'P_UPDATE_ALC_DATE1'.
        it_wosum-t_date = it_result1001-atwrt.
      ENDIF.
      READ TABLE it_result1001 WITH KEY atnam = 'P_PERF_YN'  .
      it_wosum-flag1  = it_result1001-atwrt.
    ELSE.
      CLEAR: it_wosum-t_date, it_wosum-flag1.
    ENDIF.
    MODIFY it_wosum INDEX tc_0102-current_line .
*Requested by Hur,20041020,changed by wskim
*-----Start
  ENDIF.
*-----End
ENDMODULE.                 " get_color_1002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_1003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_1003 OUTPUT.
  IF  wa_flag IS INITIAL.
*   PERFORM get_display   .
    SELECT * INTO CORRESPONDING FIELDS OF TABLE wa_wosum
      FROM ztpp_wosum .

    wa_wosum_key[] = wa_wosum[].
    SORT wa_wosum_key BY wo_ser nation dealer extc intc .

    wa_flag  = 'X'     .
  ENDIF.
ENDMODULE.                 " INIT_1003  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  INIT_1004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_1004 OUTPUT.
  IF wa_flag IS INITIAL.
*   PERFORM get_display   .
    SELECT * INTO CORRESPONDING FIELDS OF TABLE wa_wosum
      FROM ztpp_wosum .

    wa_wosum_key[] = wa_wosum[].
    SORT wa_wosum_key BY wo_ser nation dealer extc intc .

    wa_flag = 'X'     .
  ENDIF.
ENDMODULE.                 " INIT_1004  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_1005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_1005 OUTPUT.
  IF wa_flag IS INITIAL.
    PERFORM get_display   .
    wa_flag = 'X'     .
  ENDIF.

  IF wa_chg_1005_flg = 'X'.
    LOOP AT SCREEN .
      IF screen-group1 = 'CHG'.
        screen-input  = 1    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN .
      IF screen-group1 = 'CHG'.
        screen-input  = 0    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " INIT_1005  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_CONVERSION_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_conversion_0101 OUTPUT.

  " VALUE CONVERSION
  CLEAR: it_219-219desc.
************************************************************************
* Because of the Changed Table Relationship, Program Source was changed
* By Tonkey On 01/27/2004
* Reference Table : ZTBM_ABXOPVDT
************************************************************************
* Request No. : UD1K906384
************************************************************************
*  SELECT SINGLE AB~CLNM
*    INTO IT_219-219DESC
*    FROM ZTBM_ABXOPVDT AS AB
*           INNER JOIN ZTPP_VEH_MODEL AS VH ON AB~CARX = VH~MODEL02
*    WHERE VH~MODEL = WA_MODEL AND
*          AB~CLNO  = IT_219-NO .
  SELECT SINGLE clnm INTO it_219-219desc
    FROM ztbm_abxopvdt
   WHERE carx  = wa_model(2)
     AND clno  = it_219-no  .
*  SELECT SINGLE desc_219 INTO it_219-219desc
*    FROM ztbm_219_desc
*   WHERE model    = wa_model
*     AND name_219 = it_219-no .
*
*  SELECT SINGLE AB~VANM
*    INTO IT_219-219CODE
*    FROM ZTBM_ABXOPVDT AS AB
*           INNER JOIN ZTPP_VEH_MODEL AS VH ON AB~CARX = VH~MODEL02
*    WHERE VH~MODEL = WA_MODEL      AND
*          AB~CLNO  = IT_219-NO     AND
*          AB~VALU  = IT_219-219VALS .

  SELECT SINGLE vanm INTO it_219-219code
    FROM ztbm_abxopvdt
   WHERE carx  = wa_model(2)
     AND clno  = it_219-no
     AND valu  = it_219-219vals .
*  SELECT SINGLE code_name1 INTO it_219-219code
*    FROM ztbm_219_value
*   WHERE model  = wa_model
*     AND serial = it_219-no
*     AND value  = it_219-219vals .
ENDMODULE.                 " DISPLAY_CONVERSION_0101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen0103 OUTPUT.
  DATA: l_fname(30)          TYPE c,
        l_no(2)              TYPE n.

  IF  wa_save_flg = 'X' .
    CLEAR: l_no .

    DO  50 TIMES.
      l_no = l_no + 1.
      CONCATENATE 'WA_1003_CHK' l_no   INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      IF <field1> = 'X' .
        CONCATENATE 'WA_1003_VAL' l_no INTO l_fname.
        LOOP AT SCREEN.
          IF screen-name = l_fname .
            screen-input = 1 .
            MODIFY SCREEN    .
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDDO.
  ELSE.
    CLEAR: l_no .

    DO  50 TIMES.
      l_no = l_no + 1.
      CONCATENATE 'WA_1003_VAL' l_no INTO l_fname.
      LOOP AT SCREEN.
        IF screen-name = l_fname .
          screen-input = 0 .
          MODIFY SCREEN    .
        ENDIF.
      ENDLOOP.
    ENDDO.
  ENDIF.
ENDMODULE.                 " MODIFY_SCREEN0103  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen0104 OUTPUT.
  DATA: l_fname1(30)         TYPE c.
* DATA: l_no(2)              TYPE n.

  IF  wa_save_flg = 'X' .
    CLEAR: l_no .

    DO  50 TIMES.
      l_no = l_no + 1.
      CONCATENATE 'WA_1004_CHK' l_no   INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      IF <field1> = 'X' .
        CONCATENATE 'WA_1004_VAL' l_no INTO l_fname .
        CONCATENATE 'WA_1004_COD' l_no INTO l_fname1.
        LOOP AT SCREEN.
          IF screen-name = l_fname1 OR
             screen-name = l_fname .
            screen-input = 1 .
            MODIFY SCREEN    .
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDDO.
  ELSE.
    CLEAR: l_no .

    DO  50 TIMES.
      l_no = l_no + 1.
      CONCATENATE 'WA_1004_VAL' l_no INTO l_fname.
      CONCATENATE 'WA_1004_COD' l_no INTO l_fname1.
      LOOP AT SCREEN.
        IF screen-name = l_fname1 OR
           screen-name = l_fname .
          screen-input = 0 .
          MODIFY SCREEN    .
        ENDIF.
      ENDLOOP.
    ENDDO.
  ENDIF.
ENDMODULE.                 " MODIFY_SCREEN0104  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_1006  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_1006 OUTPUT.
  IF wa_flag IS INITIAL.
    PERFORM get_display   .
    wa_flag = 'X'     .
  ENDIF.

  IF wa_chg_1006_flg = 'X'.
    LOOP AT SCREEN .
      IF screen-group1 = 'CHG'.
        screen-input  = 1    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN .
      IF screen-group1 = 'CHG'.
        screen-input  = 0    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " INIT_1006  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LIST_BOX  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box OUTPUT.
* Model
  CLEAR : xlist[], xvalue.
  name = 'WA_MODEL' .
  PERFORM set_field_model USING name  wa_model .

* Engine
  CLEAR : xlist[], xvalue.

************************************************************************
* Because of the Changed Table Relationship, Program Source was changed
* By Tonkey On 01/27/2004
* Reference Table : ZTBM_ABXOPVDT
************************************************************************
* Request No. : UD1K906384
************************************************************************
  SELECT DISTINCT ab~clnm ab~valu
    INTO (xvalue-text, xvalue-key)
    FROM ztbm_abxopvdt AS ab
   WHERE ab~clno  = '009' .
    APPEND xvalue TO xlist .
  ENDSELECT.

  PERFORM list_box_function USING 'ST_2101-ENGINE'.

* CARBRATOR
  CLEAR : xlist[] , xvalue.

  SELECT DISTINCT ab~clnm ab~valu
    INTO (xvalue-text, xvalue-key)
    FROM ztbm_abxopvdt AS ab
   WHERE  ab~clno  = '038' .

    APPEND xvalue TO xlist .

  ENDSELECT.

  PERFORM list_box_function USING 'ST_2101-CABRATOR'.
ENDMODULE.                 " LIST_BOX  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIAL_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_data OUTPUT.
  DATA : l_lines TYPE i.

  IF st_2101-tdate IS INITIAL.
    st_2101-tdate =  sy-datum.
  ENDIF.

  IF st_2101-fdate IS INITIAL.
    CONCATENATE sy-datum(6) '01' INTO st_2101-fdate.
  ENDIF.

  DESCRIBE TABLE it_2101 LINES l_lines.
  tc_2101-lines = l_lines.
ENDMODULE.                 " INITIAL_DATA  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TC_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_get_lines OUTPUT.
  " Convert  Engine & Cabretor's value to the text..
  PERFORM convert_219_text USING '009'  it_2101-bagigas  .
  PERFORM convert_219_text USING '038' it_2101-cabrator  .
  g_tc_2101_lines = sy-loopc .
ENDMODULE.                 " TC_GET_LINES  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIAL_DATA_2102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_data_2102 OUTPUT.
  DATA : l_2102 TYPE i.

  IF st_2102-tdate IS INITIAL.
    st_2102-tdate =  sy-datum.
  ENDIF.

  IF st_2102-fdate IS INITIAL.
    CONCATENATE sy-datum(6) '01' INTO st_2102-fdate.
  ENDIF.

  DESCRIBE TABLE it_2102 LINES l_2102.
  tc_2102-lines = l_2102.
ENDMODULE.                 " INITIAL_DATA_2102  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_2104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_2104 OUTPUT.

  DATA : l_2104 TYPE i.

  DESCRIBE TABLE it_2104 LINES l_2104.
  tc_2104-lines = l_2104.

  CLEAR : xlist[],xvalue.

  SELECT * FROM zvpp_model     .
    xvalue-text = zvpp_model-atnam   .
    xvalue-key  = zvpp_model-atwrt    .
    APPEND xvalue TO xlist .
  ENDSELECT.

* LIST BOX SETTING
  PERFORM list_box_function USING 'ST_2104-MODEL'.

*  ENDIF.

* MODEL INITIAL VALUE SETTING

  IF st_2104-model IS INITIAL.

    MOVE    g_body_no(3) TO st_2104-model.

  ENDIF.

* OPTION
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'ALL'.
  xvalue-key  = 'A'.
  APPEND xvalue TO xlist .

  xvalue-text = 'ENGINE No Miss'.
  xvalue-key  = 'E'.
  APPEND xvalue TO xlist .

  xvalue-text = 'T/M No Miss'.
  xvalue-key  = 'E'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Key No Miss'.
  xvalue-key  = 'K'.
  APPEND xvalue TO xlist .

  xvalue-text = 'One item Miss'.
  xvalue-key  = 'J'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_2104-SELC'.

  IF st_2104-selc IS INITIAL.
    st_2104-selc = 'A'.
  ENDIF.

* BODY NO
  st_2104-body = g_body_no.


ENDMODULE.                 " LIST_BOX_2104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_2204  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_2204 OUTPUT.
* Car-Type(Scrap/Disposal)
  CLEAR : xlist[] , xvalue.
  PERFORM set_list_usagecar USING 'ST_2204-CTYPE' st_2204-ctype.

* OPTION
  PERFORM set_list_sreason                       .
  PERFORM list_box_function USING 'ST_2204-USAGE'.

  name = 'WA_MODEL'  .
  PERFORM set_field_model USING name  wa_model   .
ENDMODULE.                 " LIST_BOX_2204  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_2205  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_2205 OUTPUT.
  CLEAR : xlist[], xvalue.

* LIST BOX SETTING
  name = 'WA_MODEL'     .
  PERFORM set_field_model USING name  wa_model  .

* Plant
  CLEAR : xlist[] , xvalue.

  xvalue-text = ' 1'.
  xvalue-key  = '1'.
  APPEND xvalue TO xlist .

  xvalue-text = ' 2'.
  xvalue-key  = '2'.
  APPEND xvalue TO xlist .

  xvalue-text = ' 3'.
  xvalue-key  = '3'.
  APPEND xvalue TO xlist .

  xvalue-text = ' 4'.
  xvalue-key  = '4'.
  APPEND xvalue TO xlist .

  xvalue-text = ' 5'.
  xvalue-key  = '5'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_2205_INPUT-PLANT'.
  IF st_2205_input-plant IS INITIAL.
    st_2205_input-plant = '1'.
  ENDIF.

*
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'USAGE'.
  xvalue-key  = 'U'.
  APPEND xvalue TO xlist .

  xvalue-text = 'SCRAP'.
  xvalue-key  = 'S'.
  APPEND xvalue TO xlist .
  PERFORM list_box_function USING 'ST_2205_INPUT-USAGE'.

  PERFORM set_list_sreason                       .
ENDMODULE.                 " LIST_BOX_2205  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_DATE_2205  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_date_2205 OUTPUT.
  IF st_2205_input-bdate IS INITIAL.
    CONCATENATE sy-datum(6) '01' INTO    st_2205_input-bdate.
  ENDIF.
  IF st_2205_input-edate IS INITIAL.
    st_2205_input-edate = sy-datum.
  ENDIF.
ENDMODULE.                 " INIT_DATE_2205  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_DATE_2206  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_date_2206 OUTPUT.
  IF st_2206_input-bdate IS INITIAL.
    CONCATENATE sy-datum(6) '01' INTO    st_2206_input-bdate.
  ENDIF.
  IF st_2206_input-edate IS INITIAL.
    st_2206_input-edate = sy-datum.
  ENDIF.
ENDMODULE.                 " INIT_DATE_2206  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_2206  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_2206 OUTPUT.
* Plant
  CLEAR : xlist[] , xvalue.

  xvalue-text = ' 1'.
  xvalue-key  = '1'.
  APPEND xvalue TO xlist .

  xvalue-text = ' 2'.
  xvalue-key  = '2'.
  APPEND xvalue TO xlist .

  xvalue-text = ' 3'.
  xvalue-key  = '3'.
  APPEND xvalue TO xlist .

  xvalue-text = ' 4'.
  xvalue-key  = '4'.
  APPEND xvalue TO xlist .

  xvalue-text = ' 5'.
  xvalue-key  = '5'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_2206_INPUT-PLANT'.
  IF st_2206_input-plant IS INITIAL.
    st_2206_input-plant = '1'.
  ENDIF.

*
*  CLEAR : xlist[] , xvalue.

*  PERFORM SET_LIST_USAGECAR
*          USING 'ST_2206_INPUT-USAGE' ST_2206_INPUT-USAGE.

  CLEAR : xlist[] , xvalue.

  xvalue-text = 'BODY: ''01'''.
  xvalue-key  = 'B'.
  APPEND xvalue TO xlist .

  xvalue-text = 'PAINT: ''02'' ~ ''06'''.
  xvalue-key  = 'P'.
  APPEND xvalue TO xlist .

  xvalue-text = 'TRIM: ''07'' ~ ''17'''.
  xvalue-key  = 'T'.
  APPEND xvalue TO xlist .

  xvalue-text = 'S/OFF: ''18'' ~'.
  xvalue-key  = 'V'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_2206_INPUT-SHOP'.
ENDMODULE.                 " LIST_BOX_2206  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_0109  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_0109 OUTPUT.
  DATA: l_atinn              LIKE cabn-atinn,
        l_atnam              LIKE cabn-atnam,
        l_atwrt              LIKE ausp-atwrt.

  CLEAR : xlist[], xvalue.
* LIST BOX SETTING
  name = 'WA_MODEL' .
  PERFORM set_field_model USING name  wa_model .

* USAGE
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Domestic'.
  xvalue-key  = 'D'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Export'.
  xvalue-key  = 'E'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_0109_INPUT-USE'.

*
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Y'.
  xvalue-key  = 'Y'.
  APPEND xvalue TO xlist .

  xvalue-text = 'N'.
  xvalue-key  = 'N'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_0109_INPUT-FULL'.

  DATA : l_tcline TYPE i.

  DESCRIBE TABLE it_0109 LINES l_tcline.
  tc_0109-lines = l_tcline.
ENDMODULE.                 " LIST_BOX_0109  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_3107  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_3107 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
    PERFORM set_parameter_3107.
  ENDIF.

  DESCRIBE TABLE it_3107 LINES l_tcline.
  tc_3107-lines = l_tcline.
ENDMODULE.                 " LIST_BOX_3107  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_1007  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_1007 OUTPUT.
  IF wa_scn_flag IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
         EXPORTING
              input  = 'P_PERF_YN'
         IMPORTING
              output = wa_atinn.

    PERFORM get_data_error .
    SORT it_mara BY matnr  .
    wa_scn_flag = 'X'     .
  ENDIF.

  IF wa_chg_1005_flg = 'X'.
    LOOP AT SCREEN .
      IF screen-group1 = 'CHG'.
        screen-input  = 1    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN .
      IF screen-group1 = 'CHG'.
        screen-input  = 0    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " INIT_1007  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_1008  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_1008 OUTPUT.
  IF wa_scn_flag IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
         EXPORTING
              input  = 'P_PERF_YN'
         IMPORTING
              output = wa_atinn.

    PERFORM get_data_error .
    CLEAR: it_0108, it_0108[].
    LOOP AT it_mara.
      it_0108-ordno = it_mara-matnr .  APPEND it_0108.
    ENDLOOP.
    wa_scn_flag = 'X'     .
  ENDIF.

  IF wa_chg_1005_flg = 'X'.
    LOOP AT SCREEN .
      IF screen-group1 = 'CHG'.
        screen-input  = 1    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN .
      IF screen-group1 = 'CHG'.
        screen-input  = 0    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " INIT_1008  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initial_data_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_data_0110 OUTPUT.

  DESCRIBE TABLE it_0110 LINES tc_0110-lines.
*  tc_2101-lines = l_lines.

ENDMODULE.                 " initial_data_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  initial_data_0111  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_data_0111 OUTPUT.
  IF wa_flag IS INITIAL.
    PERFORM get_display   .
    PERFORM read_wosum.
    wa_flag = 'X'     .
  ENDIF.
  DESCRIBE TABLE it_0111 LINES tc_0111-lines.
  DESCRIBE TABLE it_0111_c LINES tc_0111_c-lines.
ENDMODULE.                 " initial_data_0111  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initial_0118  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_0118 OUTPUT.
  DESCRIBE TABLE  it_0118 LINES tc_0118-lines .

  IF wa_flag IS INITIAL .
    wa_flag = 'X'.
    CLEAR: xlist[], xlist, xvalue.
    name = 'ST_0118_INPUT-MODEL'.
    PERFORM set_field_model USING name  st_0118_input-model .
* Plant
    CLEAR : xlist[] , xvalue.

  xvalue-text = 'Plant 1'.  xvalue-key  = '1'.  APPEND xvalue TO xlist .
  xvalue-text = 'Plant 2'.  xvalue-key  = '2'.  APPEND xvalue TO xlist .
  xvalue-text = 'Plant 3'.  xvalue-key  = '3'.  APPEND xvalue TO xlist .
  xvalue-text = 'Plant 4'.  xvalue-key  = '4'.  APPEND xvalue TO xlist .
  xvalue-text = 'Plant 5'.  xvalue-key  = '5'.  APPEND xvalue TO xlist .

    PERFORM list_box_function USING 'ST_0118_INPUT-PLANT'.
    IF st_0118_input-plant IS INITIAL.
      st_0118_input = '1'.
    ENDIF.

  ENDIF.
ENDMODULE.                 " initial_0118  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_4279  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_4279 OUTPUT.
  CLEAR : xlist[], xvalue.

* LIST BOX SETTING
  name = 'WA_MODEL' .
  PERFORM set_field_model USING name  wa_model .

  IF wa_flag IS INITIAL.
    st_4279_input-dura = 1 .
    wa_flag = 'X' .
  ENDIF.
ENDMODULE.                 " LIST_BOX_4279  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_5290  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_5290 OUTPUT.
  IF wa_flag IS INITIAL .
    wa_flag = 'X'.
*   Date
    st_5290_input-date = sy-datum.
*   Plant
    CLEAR : xlist[] , xvalue.

    xvalue-text = 'First Shift'.
    xvalue-key  = '1'.
    APPEND xvalue TO xlist .
*
    xvalue-text = 'Second Shift'.
    xvalue-key  = '2'.
    APPEND xvalue TO xlist .

    xvalue-text = 'Third Shift'.
    xvalue-key  = '3'.
    APPEND xvalue TO xlist .

    xvalue-text = 'ALL Shifts'.
    xvalue-key  = '4'.
    APPEND xvalue TO xlist .

    PERFORM list_box_function USING 'ST_5290_INPUT-DAY'.
    READ TABLE xlist INTO xvalue  INDEX 1.
    st_5290_input-day = xvalue-key.
*   set the vehicle type
    CLEAR : xlist[] , xvalue.

    xvalue-text = 'Complete'.
    xvalue-key  = '1'.
    APPEND xvalue TO xlist .
*
    xvalue-text = 'BIW & BIP'.
    xvalue-key  = '2'.
    APPEND xvalue TO xlist .

    xvalue-text = 'All'.
    xvalue-key  = '3'.
    APPEND xvalue TO xlist .


    PERFORM list_box_function USING 'ST_5290_INPUT-TYP'.
    READ TABLE xlist INTO xvalue  INDEX 1.
    st_5290_input-typ = xvalue-key.

*   set the shop list
    CLEAR : xlist[] , xvalue.

    xvalue-text = 'TRIM SHOP'.
    xvalue-key  = 'T'.
    APPEND xvalue TO xlist .
*
    xvalue-text = 'BODY SHOP'.
    xvalue-key  = 'B'.
    APPEND xvalue TO xlist .

    PERFORM list_box_function USING 'WA_ARBPL'.
    READ TABLE xlist INTO xvalue  INDEX 1.
    wa_arbpl = xvalue-key.

  ENDIF.
  DESCRIBE TABLE it_5290 LINES tc_5290-lines .
ENDMODULE.                 " LIST_BOX_5290  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_5293  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_5293 OUTPUT.
* DAY
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Day'.
  xvalue-key  = '1'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Night'.
  xvalue-key  = '2'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_5293_INPUT-DAY'.

* Plant
  CLEAR : xlist[], xvalue.
  name = 'WA_PLANT'.
  PERFORM set_field_plant   USING name  wa_plant    .

* USAGE
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Domestic'.
  xvalue-key  = 'D'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Export'.
  xvalue-key  = 'E'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_5293_INPUT-USE'.

  IF st_5293_input-line IS INITIAL.
    st_5293_input-line = '1'.

  ENDIF.

  IF st_5293_input-date IS INITIAL.
    st_5293_input-date = sy-datum .
  ENDIF.

ENDMODULE.                 " LIST_BOX_5293  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_6299  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_6299 OUTPUT.
* Plant
  CLEAR : xlist[], xvalue.
  name = 'WA_PLANT'.
  PERFORM set_field_plant   USING name  wa_plant    .

* model
  CLEAR : xlist[],xvalue.
  name = 'WA_MODEL' .
  PERFORM set_field_model USING name  wa_model.

  DESCRIBE TABLE it_6299 LINES tc_6299-lines.
  tc_6299-lines = tc_6299-lines + 25 .
ENDMODULE.                 " LIST_BOX_6299  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters
*----------------------------------------------------------------------*
MODULE initialization_2200 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
*
*   Model(Car Type)
    CLEAR : xlist, xvalue.
    name = 'WA_MODEL'.
    PERFORM set_field_model USING name  wa_model.

*    perform set_field_cartype.
*    perform call_function_vrm using cartype_list.

*   Before RP
    CLEAR : xlist, xvalue.
    name = 'WA_B_RP'.
    PERFORM set_field_brp.
    PERFORM call_function_vrm USING xlist.
*   Date
    wa_cdate_st = wa_cdate_en = sy-datum.

** changed on 05/22/06 by Furong, Requested by Mr. Hur
*   BODY NO
*    CLEAR : xlist, xlist[], xvalue.
*    name = 'WA_BODYNO'.
*    PERFORM set_field_bodyno.
*    PERFORM call_function_vrm USING xlist.
** end of change WAN
  ENDIF.
ENDMODULE.                 " initialization  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Table Control's Lines
*----------------------------------------------------------------------*
MODULE modify_lines_2200 OUTPUT.
  DATA: l_line TYPE i.
  DESCRIBE TABLE it_app250 LINES l_line.
  tc_app250-lines = l_line.
ENDMODULE.                 " MODIFY_LINES  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters
*----------------------------------------------------------------------*
MODULE initialization_2201 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.

**Setting Parameters
    CLEAR: xlist, xvalue.
    name = 'WA_MODEL'.
    PERFORM set_field_model USING name  wa_model .

*   yyyymm
    p_yyyymm = sy-datum+00(06).
  ENDIF.
ENDMODULE.                 " initialization  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Screen
*----------------------------------------------------------------------*
MODULE modify_screen_2201 OUTPUT.
  MODIFY SCREEN.
ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Table Control's Total line number
*----------------------------------------------------------------------*
MODULE modify_lines_2201 OUTPUT.
  DESCRIBE TABLE it_app252 LINES l_line.
  tc_app252-lines = l_line.
ENDMODULE.                 " MODIFY_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters
*----------------------------------------------------------------------*
MODULE initialization_1205 OUTPUT.
  IF wa_flag IS INITIAL .
*   Model(Car Type)
    CLEAR: xlist, xlist[],  xvalue.
    name = 'WA_MODEL'.
    PERFORM set_field_model USING name  wa_model .

*   Part(U or C)
    CLEAR: xlist, xlist[],  xvalue.
    name = 'P_PART'.
    PERFORM set_field_part.
    PERFORM call_function_vrm USING xlist.

*   KEY(1, ... , 200)
    CLEAR: xlist, xlist[],  xvalue.
    name = 'P_KEY'.
    PERFORM set_field_key.
    PERFORM call_function_vrm USING xlist.
    wa_flag = 'X'.
  ENDIF.
ENDMODULE.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_lines  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Internal Table's total Line Amount
*----------------------------------------------------------------------*
MODULE modify_lines_1205 OUTPUT.
*  DATA: l_line TYPE i.
  DESCRIBE TABLE it_app223 LINES l_line.
  tc_app223-lines = l_line.

ENDMODULE.                 " modify_lines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Srceen's Attributes.
*----------------------------------------------------------------------*
MODULE modify_screen_1205 OUTPUT.
  LOOP AT SCREEN.
    IF ( wa_upd_1205 = 'X'       AND
         it_app223-mark = 'X'   AND
         screen-group1 <> 'GRP' ) OR
       screen-name = 'IT_APP223-MARK' .
      screen-input = '1'.
    ELSE.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1206  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status
*----------------------------------------------------------------------*
MODULE status_1206 OUTPUT.
  SET PF-STATUS 'PS'.
  PERFORM set_title_bar_1206 .
ENDMODULE.                 " STATUS_1206  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  initialization02  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameter - Directory & Init-Flag.
*----------------------------------------------------------------------*
MODULE initialization_1206 OUTPUT.
  IF wa_flag  IS INITIAL.
    CLEAR it_app223_new.
    REFRESH it_app223_new.
    p_f_name = 'C:\SAPworkdir\alc.txt' .
    p_f_type = 'DAT' .
    wa_flag  = 'X'.
  ENDIF.
ENDMODULE.                 " initialization02  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  setting_TC_APP223_NEW  OUTPUT
*&---------------------------------------------------------------------*
*       Setting The Total Line Amount.
*----------------------------------------------------------------------*
MODULE setting_tc_app223_new OUTPUT.
  DATA wa_line TYPE i.
*
  DESCRIBE TABLE it_app223_new LINES wa_line.
  tc_app223_new-lines = wa_line.

ENDMODULE.                 " setting_TC_APP223_NEW  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen_111  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Screen
*----------------------------------------------------------------------*
MODULE modify_screen_1206 OUTPUT.

ENDMODULE.                 " modify_screen_111  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters - Main Screen
*----------------------------------------------------------------------*
MODULE initialization_1209 OUTPUT.
  IF wa_flag IS INITIAL.
    PERFORM set_parameter_1209.
    wa_flag = 'X'.
  ENDIF.
ENDMODULE.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_1209  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Titlebar
*----------------------------------------------------------------------*
MODULE status_1209 OUTPUT.
*  set pf-status 'STATUS100'.
*  set titlebar 'SCREEN100'.

ENDMODULE.                 " status_1209  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       Modifying Screen
*----------------------------------------------------------------------*
MODULE modify_screen_1209 OUTPUT.
  MODIFY SCREEN.
ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_1210  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Titlebar
*----------------------------------------------------------------------*
MODULE status_1210 OUTPUT.
  DATA: l_text(70) TYPE c VALUE 'Creation of New Data'.
*  set pf-status 'STATUS110'.
  SET TITLEBAR 'TB' WITH l_text .

ENDMODULE.                 " status_1210  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen_110  OUTPUT
*&---------------------------------------------------------------------*
*       Modifying Screen - Sub Screen
*----------------------------------------------------------------------*
MODULE modify_screen_1210 OUTPUT.
*
  LOOP AT SCREEN.
    IF ( screen-name = 'IT_NEW_APP227-FORDER' ) .
      READ TABLE it_error_1210
                 WITH KEY forder = it_new_app227-forder.
      IF sy-subrc = 0.
        screen-intensified = 1.
      ELSE.
        screen-intensified = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " modify_screen_110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIALIZATION02  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters - Sub Screen
*----------------------------------------------------------------------*
MODULE initialization_1210 OUTPUT.
  IF wa_flag IS INITIAL.
*   SETTING PARAMETERS.
    p_keycode = 'AALC'.  "KEY CODE
    p_erdat = sy-datum.  "DATE ON WHICH THE RECORD WAS CREATED
    p_erzet = sy-uzeit.  "ENTRY TIME
    p_ernam = sy-uname.  "NAME OF PERSON WHO CHANGED OBJECT
*   Setting internal fields.
    PERFORM setting_internal_fields_1210.
    CLEAR it_new_app227.
    REFRESH it_new_app227.

    wa_flag = 'X'.
  ENDIF.
ENDMODULE.                 " INITIALIZATION02  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SETTING_TC  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Table Control's total line amount - Main Screen
*----------------------------------------------------------------------*
MODULE setting_tc_1209 OUTPUT.
  DATA wa_line1 TYPE i.
*
  DESCRIBE TABLE it_app227 LINES wa_line1.
  tc_app227-lines = wa_line1.

ENDMODULE.                 " SETTING_TC  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SETTING_TC_NEW_APP227  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Table Control's Total Line Amount - Sub Screen
*----------------------------------------------------------------------*
MODULE setting_tc_new_app227 OUTPUT.
*
  DESCRIBE TABLE it_new_app227 LINES l_line.
  tc_new_app227-lines = l_line.

ENDMODULE.                 " SETTING_TC_NEW_APP227  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_2107  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Titlebar Default Parameters
*----------------------------------------------------------------------*
MODULE status_2107 OUTPUT.
  MOVE  'P001'  TO  st_app237-plant.
ENDMODULE.                 " STATUS_2107  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  status_1203  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Commands with toggles.
*----------------------------------------------------------------------*
MODULE status_1203 OUTPUT.
  REFRESH: it_func_1203.
ENDMODULE.                 " status_1203  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  FIELD_ATTRIBUTE_SET  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Screen's Field-Attributes with toggles.
*----------------------------------------------------------------------*
MODULE field_attribute_set_1203 OUTPUT.
  LOOP AT SCREEN.
*    IF  g_toggle_1203  EQ  'C'.
*      IF screen-group1 EQ 'UPD'.
*        screen-input = 1.
*      ENDIF.
*      IF screen-group1 EQ 'KEY'.
*        screen-input = 0.
*      ENDIF.
*    ELSEIF  g_toggle_1203 EQ 'A'.
*      IF screen-group1 EQ 'UPD' OR screen-group1 = 'KEY'.
*        screen-input = 1.
*      ENDIF.
*    ELSE.
*      IF screen-group1 EQ 'UPD'.
*        screen-input = 0.
*      ENDIF.
*    ENDIF.
    MODIFY  SCREEN.
  ENDLOOP.

ENDMODULE.                 " FIELD_ATTRIBUTE_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  intial_data_load  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters & Searching Initial Data
*----------------------------------------------------------------------*
MODULE intial_data_load_1203 OUTPUT.
  CHECK wa_flag IS  INITIAL.
  wa_flag = 'X'.
  SELECT * FROM  ztpp_nation_def
    INTO CORRESPONDING FIELDS OF TABLE it_app221.
  IF sy-subrc NE 0.
    MESSAGE w000 WITH 'Nation data Not found'.
    EXIT.
  ENDIF.
  LOOP AT  it_app221.
    MOVE  sy-tabix   TO  it_app221-seq.
    MODIFY it_app221.
  ENDLOOP.
  DESCRIBE TABLE it_app221  LINES  g_seq_1203.
  READ TABLE it_app221  INDEX  1.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING   it_app221   TO   is_app221.
  ENDIF.
ENDMODULE.                 " intial_data_load  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Titlebar & Screen's Attributes.
*----------------------------------------------------------------------*
MODULE status_2106 OUTPUT.
  LOOP AT SCREEN.
    IF g_attr_app236 = '1'.
      IF screen-group1 = 'VIN' OR screen-group1 = 'ENG' OR
         screen-group1 = 'TMN'.
        screen-input = 0.
      ENDIF.
    ENDIF.
    IF g_attr_app236 = '2'.
      IF screen-group1 = 'MOD' OR screen-group1 = 'ENG' OR
         screen-group1 = 'TMN' OR screen-group1 = 'VEH'.
        screen-input = 0.
      ENDIF.
    ENDIF.
    IF g_attr_app236 = '3'.
      IF screen-group1 = 'MOD' OR screen-group1 = 'VIN' OR
         screen-group1 = 'TMN' OR screen-group1 = 'VEH'.
        screen-input = 0.
      ENDIF.
    ENDIF.
    IF g_attr_app236 = '4'.
      IF screen-group1 = 'MOD' OR screen-group1 = 'ENG' OR
         screen-group1 = 'VIN' OR screen-group1 = 'VEH'.
        screen-input = 0.
      ENDIF.
    ENDIF.
    IF screen-group1 EQ 'DEA'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
*  SET CURSOR FIELD g_crsr_fld_app236.
  SET CURSOR FIELD 'ST_APP236-VIN'.
*  describe table it_wip_app236  lines  wip_lines.
*  tc100-lines = wip_lines.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  init_listbox  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Dropdown list boxes
*----------------------------------------------------------------------*
MODULE init_listbox_app236 OUTPUT.
  REFRESH  list_app236.

  value_app236-key  = 'VEH'.
  value_app236-text = 'Veh.Number'.
  APPEND value_app236 TO list_app236.

  value_app236-key  = 'VIN'.
  value_app236-text = 'VIN Number'.
  APPEND value_app236 TO list_app236.

  value_app236-key  = 'ENG'.
  value_app236-text = 'Engine Number'.
  APPEND value_app236 TO list_app236.

  value_app236-key  = 'TMN'.
  value_app236-text = 'T/M Number'.
  APPEND value_app236 TO list_app236.

  name = 'ST_KEY_APP236-INQOPT'.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = list_app236.
  IF st_key_app236-inqopt IS INITIAL.
    READ TABLE list_app236
      INTO value_app236  WITH KEY key = 'VEH'.
    st_key_app236-inqopt     = value_app236-key.
    g_attr_app236 = '1'.
  ENDIF.
ENDMODULE.                 " init_listbox  OUTPUT

*&---------------------------------------------------------------------
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------
*     Setting Status & Titlebar & The Number of Internal Table's lines
*----------------------------------------------------------------------
*module status_0110 output.
*  set pf-status '0110'.
*  set titlebar  '110' with st_app236-model.
*  describe table it_219_app236  lines  tc110-lines.
*endmodule.                 " STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Titlebar - Order List
*----------------------------------------------------------------------*
*module status_0120 output.
*  refresh  it_func_app236.   clear it_func_app236.
*  if  g_part_app236 = 'U'.
*    move   'UPART'   to  it_func_app236-fcode.
*    append it_func_app236.
*  endif.
*  if  g_part_app236 = 'C'.
*    move   'CPART'   to  it_func_app236-fcode.
*    append it_func_app236.
*  endif.
*  if g_part_app236 = 'U'.
*    move   '/ Unique Parts'  to  g_parttit_app236.
*  elseif g_part_app236 = 'C'.
*    move   '/ Color Parts'   to  g_parttit_app236.
*  endif.

*  set pf-status '0120'  excluding  it_func_app236.
*  set titlebar  '120' with  g_parttit_app236.
*  describe table it_part_app236  lines tc120-lines.

*endmodule.                 " STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0130  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Total Data Amount - Air Bag List
*----------------------------------------------------------------------*
*module status_0130 output.
**  set pf-status '0130'.
***  SET TITLEBAR 'xxx'.
*  set cursor  field  'EXIT'.
*  describe table it_abag_app236 lines tc130-lines.
*endmodule.                 " STATUS_0130  OUTPUT

* OUTPUT MODULE FOR TABSTRIP 'SS2106': SETS ACTIVE TAB
MODULE ss2106_active_tab_set OUTPUT.
  ss2106-activetab = g_ss2106-pressed_tab.
  CASE g_ss2106-pressed_tab.
    WHEN c_ss2106-tab1.
      g_ss2106-subscreen = '2118'.
    WHEN c_ss2106-tab2.
      g_ss2106-subscreen = '2119'.
    WHEN c_ss2106-tab3.
      g_ss2106-subscreen = '2120'.
    WHEN c_ss2106-tab4.
      g_ss2106-subscreen = '2121'.
    WHEN c_ss2106-tab5.
      g_ss2106-subscreen = '2122'.
    WHEN c_ss2106-tab6.
      g_ss2106-subscreen = '2123'.
    WHEN c_ss2106-tab7.
      g_ss2106-subscreen = '2124'.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_2118  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Total Data Amount - 219 Option
*----------------------------------------------------------------------*
MODULE status_2118 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  DESCRIBE TABLE it_219_app236  LINES  tc_app236_01-lines.
ENDMODULE.                 " STATUS_2118  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initialization_APP246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization_app246 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
*   Set Dropdown List Boxes
    PERFORM make_dropdown_list_box_app246.
    p_status_app246    = 'S'.
    wa_alv_called = 'X'.
  ENDIF.
ENDMODULE.                 " initialization_APP246  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_APP246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_app246 OUTPUT.

ENDMODULE.                 " status_APP246  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
*  SET PF-STATUS 'STATUS100'.
*  SET TITLEBAR '100'.
ENDMODULE.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_grid_app246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_grid_app246 OUTPUT.
  IF wa_alv_called <> space.
    CLEAR wa_alv_called .
*    CREATE OBJECT GS_APPLICATION.

    CREATE OBJECT gs_custom_container
    EXPORTING container_name = wa_container.

    CREATE OBJECT alv_grid
        EXPORTING i_parent = gs_custom_container.

    PERFORM  build_variant_app246.
    PERFORM  build_layout_app246.
    PERFORM  build_fieldcat_app246.
    IF p_status_app246 = 'D'.
      PERFORM  call_method_det_app246 .
    ELSE.
      PERFORM  call_method_sum_app246.
    ENDIF.

  ENDIF.

ENDMODULE.                 " create_alv_grid_app246  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_cursor_field_app246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_cursor_field_app246 OUTPUT.
  SET CURSOR FIELD wa_fname_tx LINE wa_saveline_ix.
ENDMODULE.                 " set_cursor_field_app246  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initialization_APP245  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization_app245 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
*   Set Dropdown List Boxes
    PERFORM make_dropdown_list_box_app245.
    CONCATENATE sy-datum+00(06) '01'
      INTO p_shop_date_app245.
    p_end_date_app245 = sy-datum+06(02).
  ENDIF.
ENDMODULE.                 " initialization_APP245  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  status_APP245  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_app245 OUTPUT.

ENDMODULE.                 " status_APP245  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_lines_APP245  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_lines_app245 OUTPUT.
  DESCRIBE TABLE it_app245 LINES tc_app245-lines.
  PERFORM set_date_app245.
ENDMODULE.                 " modify_lines_APP245  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen_APP245  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_app245 OUTPUT.
  MODIFY SCREEN.
ENDMODULE.                 " modify_screen_APP245  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initialization_APP244  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization_app244 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
*   Set Dropdown List Boxes
    PERFORM make_dropdown_list_box_app244.
    p_prod_date_app244 = sy-datum.
  ENDIF.
ENDMODULE.                 " initialization_APP244  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  status_APP244  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_app244 OUTPUT.

ENDMODULE.                 " status_APP244  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_lines_APP244  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_lines_app244 OUTPUT.
  DESCRIBE TABLE it_app244 LINES tc_app244-lines.
ENDMODULE.                 " modify_lines_APP244  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen_APP244  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_app244 OUTPUT.
  MODIFY SCREEN.
ENDMODULE.                 " modify_screen_APP244  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initialization_app240  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization_app240 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
    PERFORM make_dropdown_list_box_app240.
  ENDIF.
ENDMODULE.                 " initialization_app240  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_app240  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_app240 OUTPUT.

ENDMODULE.                 " status_app240  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_lines_app240  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_lines_app240 OUTPUT.
  DESCRIBE TABLE it_app240 LINES tc_app240-lines.

ENDMODULE.                 " modify_lines_app240  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen_app240  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_app240 OUTPUT.
  " Convert  Engine & Cabretor's value to the text..
  PERFORM convert_219_text USING '005'  it_app240-tl       .
  PERFORM convert_219_text USING '007'  it_app240-tm        .
  PERFORM convert_219_text USING '009'  it_app240-eng       .
ENDMODULE.                 " modify_screen_app240  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters
*----------------------------------------------------------------------*
MODULE initialization_app239 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
**Setting Parameters
*   Model(Car Type)
    CLEAR: xlist, xlist[], xvalue.
    name = 'WA_MODEL' .
    PERFORM set_field_model USING name  wa_model .
  ENDIF.
ENDMODULE.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_sreen  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Screen
*----------------------------------------------------------------------*
MODULE modify_sreen_app239 OUTPUT.
  MODIFY SCREEN.
ENDMODULE.                 " modify_sreen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_line  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Table Control's total line number
*----------------------------------------------------------------------*
MODULE modify_lines_app239 OUTPUT.
*  data: l_line type i.
  DESCRIBE TABLE it_app239 LINES l_line.
  tc_app239-lines = l_line.
ENDMODULE.                 " modify_lines  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_listbox_app272 OUTPUT.
  PERFORM set_listbox_app272.
ENDMODULE.                 " SET_LISTBOX  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_9002  OUTPUT
*&---------------------------------------------------------------------*
MODULE display_app272_02 OUTPUT.
  PERFORM display_app272_02.
ENDMODULE.                 " DISPLAY_9002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_9003  OUTPUT
*&---------------------------------------------------------------------*
MODULE display_app272_03 OUTPUT.
  PERFORM display_app272_03.
ENDMODULE.                 " DISPLAY_9003  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  initialization_app301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization_app301 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag  = 'X'.
*   Set Dropdown List Boxes
    PERFORM make_dropdown_list_box_app301.
    wa_alv_called = 'X'.
    p_sl_app301   = 'H'.
    p_date_app301 = sy-datum.
  ENDIF.
ENDMODULE.                 " initialization_app301  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  create_alv_grid_app301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_grid_app301 OUTPUT.
  CREATE OBJECT gs_custom_container
    EXPORTING container_name = wa_container.

  CREATE OBJECT alv_grid
    EXPORTING i_parent = gs_custom_container.

  PERFORM  build_variant_app301.
  PERFORM  build_layout_app301.
  PERFORM  build_fieldcat_app301.
  CASE p_sl_app301 .
    WHEN  'H'.  " Hourly Plan
      PERFORM  call_method_hourly_app301 .
    WHEN  'D'.  " Daily Plan
      PERFORM  call_method_daily_app301.
    WHEN  'W'.  " Weekly Plan
      PERFORM  call_method_weekly_app301.
  ENDCASE.
ENDMODULE.                 " create_alv_grid_app301  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  set_cursor_field_app301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_cursor_field_app301 OUTPUT.

ENDMODULE.                 " set_cursor_field_app301  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  initialization_app301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization_app302 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
*   Set Dropdown List Boxes
    PERFORM make_dropdown_list_box_app302.
    p_date_app302 = sy-datum.
  ENDIF.
ENDMODULE.                 " initialization_APP302  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_2107  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_2107 OUTPUT.
  IF wa_flag = space.
    CLEAR : xlist, xlist[], xvalue .
    name = 'WA_MODEL' .
    PERFORM set_field_model USING name  wa_model  .

    CLEAR : xlist, xlist[], xvalue .
    name = 'ST_APP237-INQS'.
    PERFORM report_type_app237 USING name
                                     st_app237-inqs .
    wa_flag = 'X'.
  ENDIF.
  DESCRIBE TABLE it_dls_2107 LINES tc_app237-lines .

ENDMODULE.                 " INIT_2107  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_1201  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_1201 OUTPUT.
  CLEAR : xlist[], xvalue.

  name = 'WA_MODEL' .
  PERFORM set_field_model USING name  wa_model .

* LIST BOX SETTING
  DESCRIBE TABLE it_opt1_app219 LINES tc_app219_01-lines.
ENDMODULE.                 " INIT_1201  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INIT_MODEL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_model OUTPUT.
* Model
  CLEAR : xlist[], xvalue, ok_code.
  name = 'WA_MODEL' .
  PERFORM set_field_model USING name  wa_model .
ENDMODULE.                 " INIT_MODEL  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  status_1202  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1202 OUTPUT.

ENDMODULE.                 " status_1202  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  list_box_3109  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_3109 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
    PERFORM set_parameter_3109.
  ENDIF.

  DESCRIBE TABLE it_3109 LINES l_tcline.
  tc_3109-lines = l_tcline.
ENDMODULE.                 " list_box_3109  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  4104_pbo  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 4104_pbo OUTPUT.

ENDMODULE.                 " 4104_pbo  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  init_2203  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_2203 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
    name = 'WA_MODEL' .
    PERFORM set_field_model USING name  wa_model .
  ENDIF.
ENDMODULE.                 " init_2203  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initialization_2301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization_2301 OUTPUT.
  IF wa_flag IS INITIAL.
    wa_flag = 'X'.
*        sale_part            ,  "Sales Type
    MOVE sy-datum TO st_app263-date    .
    name = 'ST_APP263-OPT'.
    CLEAR: xlist, xlist[], xvalue.
    PERFORM set_field_opt_app263 USING name  st_app263-opt .

    name = 'WA_MODEL' .
    CLEAR: xlist, xlist[], xvalue.
    PERFORM set_field_model USING name  wa_model  .
  ENDIF.
ENDMODULE.                 " initialization_2301  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  modify_lines_2301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_lines_2301 OUTPUT.
  DESCRIBE TABLE it_app263 LINES tc_app263-lines .
  PERFORM set_field_header_app263 .
ENDMODULE.                 " modify_lines_2301  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  set_lines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_lines OUTPUT.
  DESCRIBE TABLE it_rp_app236 LINES tc_app236_05-lines.
ENDMODULE.                 " set_lines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_color_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_color_0101 OUTPUT.
  LOOP AT SCREEN.
    IF it_219-219desc = 'Not found'.
      screen-intensified = '0'.
      MODIFY SCREEN.
    ELSE.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " set_color_0101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  set_color_app207  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_color_app207 OUTPUT.
  LOOP AT SCREEN.
    IF it_219-219desc = 'Not found'.
      screen-intensified = '0'.
      MODIFY SCREEN.
    ELSE.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " set_color_app207  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_table_control_app207  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_table_control_app207 OUTPUT.
  DESCRIBE TABLE it_219 LINES tc_0107-lines.
ENDMODULE.                 " set_table_control_app207  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
* PERFORM set_image.
*  perform create_and_init_tree.
ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_display OUTPUT.
  CHECK sy-tcode NE 'ZPPA9999'.
  IF sy-dynnr = '2203' .
    LOOP AT SCREEN.
      IF screen-group1   = 'UPD'.
        screen-invisible = 0    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    wa_change = 'X' .
  ENDIF.

  IF sy-dynnr = '2204' OR sy-dynnr = '1203' OR sy-dynnr = '2202'.
    LOOP AT SCREEN.
      IF screen-group2   = 'UPD'.
        screen-invisible = 0    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF wa_change = 'X' .
    CASE sy-dynnr.
      WHEN '2202' OR '2204' OR '1203' .
        LOOP AT SCREEN.
          IF screen-group1 = 'UPD'.
            screen-input   = 1    .
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'UPD'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " SET_DISPLAY  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  display_tc2202  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_tc2202 OUTPUT.
  DATA: l_date        LIKE ausp-atwrt.

  CHECK tc_2202-lines > 0   .
  " Display the Period
  IF it_2202-wdate IS INITIAL.
    l_date = sy-datum.
    it_2202-peried = l_date   - it_2202-rdate .
  ELSE.
    it_2202-peried = it_2202-wdate - it_2202-rdate.
  ENDIF.
  CONDENSE it_2202-peried NO-GAPS.
ENDMODULE.                 " display_tc2202  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DISPLAY2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_display2 OUTPUT.
  CHECK sy-tcode NE 'ZPPA9999'.
  IF sy-dynnr = '2204' AND st_2204-text NE space.
    st_2204-causion = 'Reversed Item Check!'.
    LOOP AT SCREEN.
      IF screen-group2   = 'DIS'.
        screen-invisible = 0    .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " SET_DISPLAY2  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_TEXT_3109  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_text_3109 OUTPUT.
  IF it_3109-char1 IS INITIAL.
  ELSE.
    PERFORM get_enm1_3109 USING it_3109-char1  it_3109-enm1 .
  ENDIF.
  IF it_3109-char2 IS INITIAL.
  ELSE.
    PERFORM get_enm1_3109 USING it_3109-char2  it_3109-enm2 .
  ENDIF.
ENDMODULE.                 " SET_TEXT_3109  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  GET_RATE_2102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_rate_2102 OUTPUT.
  SELECT SINGLE em_rate INTO it_2102-em_rate
    FROM ztpp_nation_def
   WHERE nation = it_2102-dest .
ENDMODULE.                 " GET_RATE_2102  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_TEXT_2205  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_text_2205 OUTPUT.
  " Convert Text ( Usage, Code )..
  READ TABLE ylist WITH KEY key = it_2205-usage .
  IF sy-subrc = 0.  it_2205-usage = ylist-text.  ENDIF.
  CASE it_2205-car.
    WHEN 'D'.
      it_2205-car = 'Disposal' .
    WHEN 'S'.
      it_2205-car = 'Scarp'    .
    WHEN 'T'.
      it_2205-car = 'Test'     .
  ENDCASE.
ENDMODULE.                 " DISPLAY_TEXT_2205  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_5290  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5290 OUTPUT.
  SET PF-STATUS 'STATUS38'.
  SET TITLEBAR 'TITLE38'.

ENDMODULE.                 " STATUS_5290  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2108  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2108 INPUT.
  CASE sy-ucomm .

    WHEN 'EXCL'.
      PERFORM download_app237.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2108  INPUT

*&spwizard: output module for tc 'TB_8081'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE tb_8081_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_8081 LINES tb_8081-lines.
ENDMODULE.
MODULE tb_8082_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_8082 LINES tb_8082-lines.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  list_box_8081  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_8081 OUTPUT.
  CLEAR : xlist[], xvalue.

  name = 'WA_MODEL' .
  PERFORM set_field_model USING name  wa_model .

* USAGE
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Domestic'.
  xvalue-key  = 'D'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Export'.
  xvalue-key  = 'E'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_8081_INPUT-USE'.

*
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Y'.
  xvalue-key  = 'Y'.
  APPEND xvalue TO xlist .

  xvalue-text = 'N'.
  xvalue-key  = 'N'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_8081_INPUT-COLOR'.

*  DATA : l_tcline TYPE i.
*
*  DESCRIBE TABLE it_8081 LINES l_tcline.
*  tc_8081-lines = l_tcline.
*  CLEAR: L_TCLINE.
ENDMODULE.                 " list_box_8081  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  init_day  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_day OUTPUT.
  perform set_day.
ENDMODULE.                 " init_day  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  list_box_8082  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_8082 OUTPUT.
    CLEAR : xlist[], xvalue.

  name = 'WA_MODEL' .
  PERFORM set_field_model USING name  wa_model .

* USAGE
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Domestic'.
  xvalue-key  = 'D'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Export'.
  xvalue-key  = 'E'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_8082_INPUT-USE'.

*
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Y'.
  xvalue-key  = 'Y'.
  APPEND xvalue TO xlist .

  xvalue-text = 'N'.
  xvalue-key  = 'N'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_8082_INPUT-COLOR'.


ENDMODULE.                 " list_box_8082  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  8088_pbo  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 8088_pbo OUTPUT.

ENDMODULE.                 " 8088_pbo  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  init_lines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_lines OUTPUT.
*  if p_fdate_2103 is initial.
*
*  endif.
  describe table it_2103 lines tc_2103-lines.
ENDMODULE.                 " init_lines  OUTPUT
