*----------------------------------------------------------------------*
*   INCLUDE MZPP_APPLICATIONF01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_tree.
  DATA l_custom_cont   TYPE REF TO cl_gui_custom_container.   "
  DATA image_cont      TYPE REF TO cl_gui_container.             " HPC

  CREATE OBJECT l_custom_cont
    EXPORTING
      container_name = 'IMAGE'
    EXCEPTIONS
      others         = 1.

  CALL FUNCTION 'WB_BITMAP_SHOW'
    EXPORTING
      image_name       = '/sapmnt/UP2/hmc.bmp'
      container        = 'IMAGE'
*     parent           =  image_cont                  " HPC
      stretch_picture  = start_image_resize
    IMPORTING
      image_control    = image_control
      custom_container = custom_container.
  image_control_created = 'X'.
*
*
** create a tree control
*  CREATE OBJECT g_tree
*    EXPORTING
*      parent              = g_custom_container
*      " single node selection is used
*      node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single
*    EXCEPTIONS
*      lifetime_error              = 1
*      cntl_system_error           = 2
*      create_error                = 3
*      failed                      = 4
*      illegal_node_selection_mode = 5.
*  IF sy-subrc <> 0.
*    MESSAGE a001(zmpp) WITH 'CREATE OBJECT TREE - ERR!' .
*  ENDIF.
*
** define the events which will be passed to the backend
*  " node double click
*  event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
*  event-appl_event = 'X'. " process PAI if event occurs
*  APPEND event TO events.
*
*  " expand no children
*  event-eventid = cl_gui_simple_tree=>eventid_expand_no_children.
*  event-appl_event = 'X'.
*  APPEND event TO events.
*
*  CALL METHOD g_tree->set_registered_events
*    EXPORTING
*      events = events
*    EXCEPTIONS
*      cntl_error                = 1
*      cntl_system_error         = 2
*      illegal_event_combination = 3.
*  IF sy-subrc <> 0.
*    MESSAGE a001(zmpp) WITH 'EVENT REGISTER ERROR!' .
*  ENDIF.
*
** assign event handlers in the application class to each desired event
** SET HANDLER g_application->handle_node_double_click FOR g_tree.
** SET HANDLER g_application->handle_expand_no_children FOR g_tree.
*
*
** add some nodes to the tree control
** NOTE: the tree control does not store data at the backend. If an
** application wants to access tree data later, it must store the
** tree data itself.
*
** PERFORM build_node_table USING node_table.
ENDFORM.                    " CREATE_AND_INIT_TREE

*&---------------------------------------------------------------------*
*&      Form  GET_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_display.
  CLEAR : wa_wosum[],it_wosum[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE wa_wosum
     FROM ztpp_wosum .

  SORT wa_wosum BY wo_ser .
  wa_wosum_key[] = wa_wosum[].
  DELETE ADJACENT DUPLICATES FROM wa_wosum_key COMPARING wo_ser nation
  dealer.
  SORT wa_wosum_key BY wo_ser nation dealer extc intc           .
ENDFORM.                    " GET_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  GET_DISPLAY_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_display_color .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE wa_wosum
    FROM ztpp_wosum .

  SORT wa_wosum BY wo_ser .
  wa_wosum_key[] = wa_wosum[].
  DELETE ADJACENT DUPLICATES FROM wa_wosum COMPARING wo_ser nation
                                                     dealer extc intc .
ENDFORM.                    " GET_DISPLAY_COLOR

*&---------------------------------------------------------------------*
*&      Form  GET_NEXT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_next_display.
  DATA: cursor_line TYPE i.
  READ TABLE wa_wosum_key WITH KEY mark = 'X'.
  wa_index = sy-tabix .
  CASE sy-subrc.
    WHEN 0.
      CLEAR: wa_wosum_key-mark.
      MODIFY wa_wosum_key INDEX wa_index TRANSPORTING mark.
      wa_index = wa_index + 1.
      READ TABLE wa_wosum_key INDEX wa_index.
      wa_wosum_key-mark = 'X'.
      CONCATENATE wa_wosum_key-wo_ser wa_wosum_key-nation
                  wa_wosum_key-dealer  INTO wa_order .
      MODIFY wa_wosum_key INDEX wa_index TRANSPORTING mark.
    WHEN OTHERS.
      READ TABLE wa_wosum_key INDEX 1.
      CONCATENATE wa_wosum_key-wo_ser wa_wosum_key-nation
                  wa_wosum_key-dealer INTO wa_order .
      wa_wosum_key-mark = 'X'.
      MODIFY wa_wosum_key INDEX 1 TRANSPORTING mark.
  ENDCASE.
  wa_ecolor = wa_wosum_key-extc .
  wa_icolor = wa_wosum_key-intc .
ENDFORM.                    " GET_NEXT_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  GET_NEXT_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_next_color  .
  READ TABLE wa_wosum WITH KEY mark = 'X'.
  wa_index = sy-tabix .
  CASE sy-subrc.
    WHEN 0.
      CLEAR: wa_wosum-mark.
      MODIFY wa_wosum INDEX wa_index TRANSPORTING mark.
      wa_index = wa_index + 1.
      READ TABLE wa_wosum INDEX wa_index.
      wa_wosum-mark = 'X'.
      CONCATENATE wa_wosum-wo_ser wa_wosum-nation  wa_wosum-dealer
             INTO wa_order .
      MODIFY wa_wosum INDEX wa_index TRANSPORTING mark.
    WHEN OTHERS.
      READ TABLE wa_wosum INDEX 1.
      CONCATENATE wa_wosum-wo_ser wa_wosum-nation  wa_wosum-dealer
             INTO wa_order .
      wa_wosum-mark = 'X'.
      MODIFY wa_wosum INDEX 1 TRANSPORTING mark.
  ENDCASE.
  wa_ecolor = wa_wosum-extc .
  wa_icolor = wa_wosum-intc .
ENDFORM.                    " GET_NEXT_COLOR

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_1001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_1001.
  DATA: l_material             LIKE mara-matnr.
  DATA: l_count TYPE i.

  CLEAR: wa_wosum.
  READ TABLE wa_wosum WITH KEY wo_ser = wa_order(9)
                               nation = wa_order+9(3)
                               dealer = wa_order+12(2) .
  IF sy-subrc <> 0.
    MESSAGE s000 WITH text-100.
    PERFORM clear_common_val .    wa_err_flag = 'X'.
    EXIT.
  ELSE.
    CLEAR: wa_err_flag .
    WRITE wa_wosum-aedat TO wa_wom_date.
  ENDIF.

  PERFORM get_sum_wohd   .
  CONCATENATE wa_wosum-wo_ser wa_wosum-nation  wa_wosum-dealer
                                          INTO l_material.

  CLEAR: it_result1001, it_result1001[].

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_material
      ctype        = '001'
    TABLES
      val_table    = it_result1001
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  IF sy-subrc = 0.
    READ TABLE it_result1001 WITH KEY atnam = 'P_MODEL' .
    wa_model = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_MI' .
    wa_mi    = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_OCN'.
    wa_ocn   = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_VERSION'.
    wa_version = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_WO_CREATE_DATE'.
    wa_wo_create_date   = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_WO_MODI_DATE' .
    wa_wo_modi_date     = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_LC_NO'.
    wa_lc_no            = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_DESTINATION_CODE'.
    wa_destination_code = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_UPDATE_ALC_DATE1'.
    wa_update_alc_date1 = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_PERF_YN'.
    wa_perf_yn = it_result1001-atwrt.
*    if it_result1001-atwrt = 'Y'.
*      wa_perf_yn  = '''O'''    .
*    else.
*      wa_perf_yn  = '''X'''    .
*    endif.
    CLEAR: it_result1001.
    CLEAR: wa_val11 . CLEAR: wa_val12 .
    READ TABLE it_result1001 WITH KEY atnam = 'P_INIT_QTY' .
    wa_init_qty         = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_MOD_QTY' .
    wa_mod_qty          = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_PLAN_QTY' .
    wa_plan_qty         = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_FORECAST_QTY' .
    wa_forecast_qty     = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_MITU_QTY' .
    wa_mitu_qty         = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_SEQ_QTY' .
    wa_seq_qty          = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_TRIM_PLANT_NO'.
    wa_trim_plant_no    = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_VIN_SPEC' .
    wa_vin_spec         = it_result1001-atwrt.    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_PROD_FLAG'.
    CASE it_result1001-atwrt.
      WHEN 'D'.         " Delete for the Product
        wa_prod  =  'D' .
        READ TABLE it_result1001 WITH KEY atnam = 'P_PROD_FLAG_DATE'.
        wa_prod_date = it_result1001-atwrt .
      WHEN 'Y'.         " Available for the Product
        wa_prod  =  ' ' .
        CLEAR: wa_prod_date.
      WHEN space.       "
    ENDCASE.
    wa_prod             = it_result1001-atwrt.    CLEAR: it_result1001.
  ELSE.
    CLEAR: wa_tot                   ,
           wa_total                 ,
*          wa_car                   ,  " Vehicle Name
           wa_mi                    ,  " Model Index
           wa_version               ,  " Version
           wa_wo_create_date        ,  " Create Date
           wa_wo_modi_date          ,  " Modify Date
           wa_lc_no                 ,  " L/C No
           wa_destination_code      ,  " DESTINATION
           wa_update_alc_date1      ,  " ECM Transfer
           wa_perf_yn               ,  " Complete flag
           wa_val11                 ,  " Self-Certified No
           wa_val12                 ,  " Approval Date
           wa_init_qty              ,  " Initiate Qty
           wa_mod_qty               ,  " Modify Qty
           wa_plan_qty              ,  " Plan Qty
           wa_forecast_qty          ,  " Forecast Qty
           wa_mitu_qty              ,  " MITU Qty
           wa_seq_qty               ,  " Sequence Qty
           wa_trim_plant_no         ,  " Plant
           wa_vin_spec              ,  " V.I.N
           wa_prod                  ,  " Production Flag
           wa_prod_date             ,  " Production Flag Date
           wa_val29, wa_val30,  wa_val31, wa_val32, wa_val33, wa_val34,
           wa_val35, wa_val36,  wa_val37, wa_val38, wa_val39.
  ENDIF.
ENDFORM.                    " DISPLAY_1001

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_1002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_1002.
  DATA: l_material             LIKE mara-matnr,
        l_instance             LIKE inob-cuobj.

  CLEAR: wa_wosum, it_wosum, it_wosum[].
  CHECK NOT wa_order IS INITIAL.
* LOOP AT wa_wosum_key WHERE wo_ser = wa_order(9)    AND
  LOOP AT wa_wosum     WHERE wo_ser = wa_order(9)    AND
                             nation = wa_order+9(3)  AND
                             dealer = wa_order+12(2) .
*   it_wosum = wa_wosum_key .
    it_wosum = wa_wosum     .
    APPEND it_wosum.
  ENDLOOP.

  SORT it_wosum BY extc intc.
  READ TABLE wa_wosum WITH KEY wo_ser = wa_order(9)
                               nation = wa_order+9(3)
                               dealer = wa_order+12(2) .
  CONCATENATE wa_wosum-wo_ser wa_wosum-nation  wa_wosum-dealer
                                          INTO l_material.

  CLEAR: it_result1001, it_result1001[].

  it_result1001-atnam = 'P_MI'.                APPEND it_result1001.
  it_result1001-atnam = 'P_OCN'.               APPEND it_result1001.
  it_result1001-atnam = 'P_VERSION'.           APPEND it_result1001.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_material
*     MODE         = 'R'
      ctype        = '001'
    TABLES
      val_table    = it_result1001
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  IF sy-subrc = 0.
*    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_MODEL' .
*    WA_CAR     = IT_RESULT1001-ATWTB. CLEAR: IT_RESULT1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_MI'    .
    wa_mi      = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_OCN'   .
    wa_ocn     = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_VERSION'.
    wa_version = it_result1001-atwrt. CLEAR: it_result1001.
  ELSE.
    CLEAR: wa_car    , wa_mi    , wa_ocn, wa_version.
  ENDIF.

*Requested by Hur,20041020,changed by wskim
*-----Start
  REFRESH CONTROL 'TC_0102' FROM SCREEN 0102.
  DESCRIBE TABLE it_wosum LINES tc_0102-lines.
*-----End

ENDFORM.                    " DISPLAY_1002

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_1003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_1003.
  DATA: l_material             LIKE mara-matnr,
        l_instance             LIKE inob-cuobj.
  DATA: l_fname(50)        TYPE c,
        l_no(2)            TYPE n.

  CLEAR: wa_wosum.
  READ TABLE wa_wosum WITH KEY wo_ser = wa_order(9)
                               nation = wa_order+9(3)
                               dealer = wa_order+12(2)
                               extc   = wa_ecolor
                               intc   = wa_icolor .
  IF sy-subrc <> 0.
    MESSAGE i000 WITH 'No Data!!'.
  ENDIF.
  CONCATENATE wa_wosum-wo_ser wa_wosum-nation  wa_wosum-dealer
              wa_ecolor       wa_icolor   INTO l_material.

  CLEAR: it_result1001, it_result1001[].

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_material
*     MODE         = 'R'
      ctype        = '001'
    TABLES
      val_table    = it_result1001
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  IF sy-subrc = 0.
*    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_MODEL' .
*    WA_CARTYPE = IT_RESULT1001-ATWTB. CLEAR: IT_RESULT1001.
*    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_MI'    .
*    WA_SPECNO  = IT_RESULT1001-ATWRT. CLEAR: IT_RESULT1001.
** Changed by Furong on 10/24/07 for EBOM
*    wa_mi      = wa_wosum-fsc(13).
    wa_mi      = wa_wosum-fsc(14).
** End of change
    READ TABLE it_result1001 WITH KEY atnam = 'P_OCN'   .
    wa_ocn     = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_VERSION'.
    wa_version = it_result1001-atwrt. CLEAR: it_result1001.

    DO   9 TIMES.
      CLEAR: it_result1001-atwrt .
      l_no = l_no + 1.
      CONCATENATE 'P_ALC_C_'  l_no+1(1)  INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      READ TABLE it_result1001 WITH KEY atnam = l_fname .
      CONCATENATE 'WA_1003_VAL' l_no     INTO l_fname.
      ASSIGN (l_fname)      TO <field2> .
      <field2> =  it_result1001-atwrt .
    ENDDO.

    DO  41 TIMES.
      CLEAR: it_result1001-atwrt .
      l_no = l_no + 1.
      CONCATENATE 'P_ALC_C_'  l_no  INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      READ TABLE it_result1001 WITH KEY atnam = l_fname .
      CONCATENATE 'WA_1003_VAL' l_no     INTO l_fname.
      ASSIGN (l_fname)      TO <field2> .
      <field2> =  it_result1001-atwrt .
    ENDDO.
  ELSE.
    CLEAR: wa_car, wa_mi, wa_ocn, l_no.
    DO   9 TIMES.
      l_no = l_no + 1.
      CONCATENATE 'WA_1003_VAL' l_no     INTO l_fname.
      ASSIGN (l_fname)      TO <field2> .
      CLEAR: <field2> .
    ENDDO.

    DO  41 TIMES.
      l_no = l_no + 1.
      CONCATENATE 'WA_1003_VAL' l_no     INTO l_fname.
      ASSIGN (l_fname)      TO <field2> .
      CLEAR: <field2> .
    ENDDO.
  ENDIF.
ENDFORM.                    " DISPLAY_1003

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_1004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_1004.
  DATA: l_material             LIKE mara-matnr,
        l_instance             LIKE inob-cuobj.
  DATA: l_fname(50)        TYPE c,
        l_219(3)           TYPE n,
        l_val(3)           TYPE c,
        l_int              TYPE i,
        l_no(2)            TYPE n.

  PERFORM extint_color.

  CLEAR: wa_wosum.
  READ TABLE wa_wosum WITH KEY wo_ser = wa_order(9)
                               nation = wa_order+9(3)
                               dealer = wa_order+12(2)
                               extc   = wa_ecolor
                               intc   = wa_icolor .
  CONCATENATE wa_wosum-wo_ser wa_wosum-nation  wa_wosum-dealer
              wa_ecolor       wa_icolor   INTO l_material.

  CLEAR: it_result1001, it_result1001[].

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_material
*     MODE         = 'R'
      ctype        = '001'
    TABLES
      val_table    = it_result1001
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  IF sy-subrc = 0.
** Changed by Furong for EBOM, MI to 14 digits
    wa_mi      = wa_wosum-fsc(14).
** End of change
    READ TABLE it_result1001 WITH KEY atnam = 'P_OCN'   .
    wa_ocn     = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_VERSION'.
    wa_version = it_result1001-atwrt. CLEAR: it_result1001.

    " User Input field for 219 Value... (Data Search..)
    CLEAR: l_no.
    DO  20 TIMES.
      l_no = l_no + 1.
      CONCATENATE 'WA_219-I'    l_no    INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      IF <field1> NE space.
        l_val = l_int = <field1>.
        CONDENSE l_val.
        CONCATENATE 'P_219_' l_val    INTO l_fname.
        READ TABLE it_result1001 WITH KEY atnam = l_fname .
        CONCATENATE 'WA_219-O' l_no   INTO l_fname.
        ASSIGN (l_fname)      TO <field2> .
        <field2> =  it_result1001-atwrt .   CLEAR: it_result1001.
      ENDIF.
    ENDDO.
    CLEAR: l_no.
    DO   9 TIMES.
      CLEAR: it_result1001-atwrt .
      l_no = l_no + 1.
      CONCATENATE 'P_ALC_C_'  l_no+1(1)  INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      READ TABLE it_result1001 WITH KEY atnam = l_fname .
      CONCATENATE 'WA_1004_VAL' l_no     INTO l_fname.
      ASSIGN (l_fname)      TO <field2> .
      <field2> =  it_result1001-atwrt .
    ENDDO.

    DO  41 TIMES.
      CLEAR: it_result1001-atwrt .
      l_no = l_no + 1.
      CONCATENATE 'P_ALC_C_'  l_no  INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      READ TABLE it_result1001 WITH KEY atnam = l_fname .
      CONCATENATE 'WA_1004_VAL' l_no     INTO l_fname.
      ASSIGN (l_fname)      TO <field2> .
      <field2> =  it_result1001-atwrt .
    ENDDO.

    CLEAR: l_219, l_no.
    DO 20 TIMES.
      CLEAR: it_result1001-atwrt .
      l_no = l_no + 1  .
      CONCATENATE 'WA_1004_219I' l_no     INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      CASE <field1> .
          l_219 = <field1> .
        WHEN space  OR 0 .
          CONCATENATE 'WA_1004_219O' l_no     INTO l_fname.
          ASSIGN (l_fname)      TO <field2> .
          CLEAR: <field2> .
        WHEN 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8 OR 9 .
          CONCATENATE 'WA_1004_219O' l_no     INTO l_fname.
          ASSIGN (l_fname)      TO <field2> .
          CONCATENATE 'P_219_'  l_219+2(1)    INTO l_fname.
          READ TABLE it_result1001 WITH KEY atnam = l_fname .
          <field2> =  it_result1001-atwrt .
        WHEN 10 OR 11 OR 12 OR 13 OR 14 OR 15 OR 16 OR 17 OR 18 OR 19
          OR 20 OR 21 OR 22 OR 23 OR 24 OR 25 OR 26 OR 27 OR 28 OR 29
          OR 30 OR 31 OR 32 OR 33 OR 34 OR 35 OR 36 OR 37 OR 38 OR 39
          OR 40 OR 41 OR 42 OR 43 OR 44 OR 45 OR 46 OR 47 OR 48 OR 49
          OR 50 OR 51 OR 52 OR 53 OR 54 OR 55 OR 56 OR 57 OR 58 OR 59
          OR 60 OR 61 OR 62 OR 63 OR 64 OR 65 OR 66 OR 67 OR 68 OR 69
          OR 70 OR 71 OR 72 OR 73 OR 74 OR 75 OR 76 OR 77 OR 78 OR 79
          OR 80 OR 81 OR 82 OR 83 OR 84 OR 85 OR 86 OR 87 OR 88 OR 89
          OR 90 OR 91 OR 92 OR 93 OR 94 OR 95 OR 96 OR 97 OR 98 OR 99 .
          CONCATENATE 'WA_1004_219O' l_no     INTO l_fname.
          ASSIGN (l_fname)      TO <field2> .
          CONCATENATE 'P_219_'  l_219+1(2)    INTO l_fname.
          READ TABLE it_result1001 WITH KEY atnam = l_fname .
          <field2> =  it_result1001-atwrt .
        WHEN OTHERS.
          CONCATENATE 'WA_1004_219O' l_no     INTO l_fname.
          ASSIGN (l_fname)      TO <field2> .
          CONCATENATE 'P_219_'  l_219         INTO l_fname.
          READ TABLE it_result1001 WITH KEY atnam = l_fname .
          <field2> =  it_result1001-atwrt .
      ENDCASE.
    ENDDO.
  ELSE.
    CLEAR: wa_car, wa_mi    , wa_ocn, l_no.
    DO   9 TIMES.
      l_no = l_no + 1.
      CONCATENATE 'WA_1004_VAL' l_no     INTO l_fname.
      ASSIGN (l_fname)      TO <field2> .
      CLEAR: <field2> .
    ENDDO.

    DO  41 TIMES.
      l_no = l_no + 1.
      CONCATENATE 'WA_1003_VAL' l_no     INTO l_fname.
      ASSIGN (l_fname)      TO <field2> .
      CLEAR: <field2> .
    ENDDO.
  ENDIF.
ENDFORM.                    " DISPLAY_1004

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_1005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_1005.
  DATA: l_material             LIKE mara-matnr,
        l_instance             LIKE inob-cuobj.
  DATA: l_fname(50)        TYPE c,
        l_219(3)           TYPE n,
        l_val(3)           TYPE c,
        l_int              TYPE i,
        l_no(3)            TYPE n.

  CLEAR: wa_wosum.

  READ TABLE wa_wosum WITH KEY wo_ser = wa_order(9)
                               nation = wa_order+9(3)
                               dealer = wa_order+12(2).

  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'No Data!!'.
    EXIT.
  ENDIF.
  CONCATENATE wa_wosum-wo_ser wa_wosum-nation  wa_wosum-dealer
             INTO l_material.
* requested by my hur changed by chris
* set the display order mark
  READ TABLE wa_wosum_key WITH KEY wo_ser = wa_wosum-wo_ser
                                   nation = wa_wosum-nation
                                   dealer = wa_wosum-dealer.
  wa_wosum_key-mark = 'X'.
  MODIFY  wa_wosum_key INDEX sy-tabix TRANSPORTING mark .
* end of change on 04/11/2005
  CLEAR: it_result1001, it_result1001[].

  CLEAR: it_alcu_a, it_alcu_a[], it_alcu_b, it_alcu_b[],
         it_alcu_c, it_alcu_c[], it_alcu_d, it_alcu_d[].

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_material
*     MODE         = 'R'
      ctype        = '001'
    TABLES
      val_table    = it_result1001
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  IF sy-subrc = 0.
** Changed by Furong for EBOM, MI to 14 digits
    wa_mi      = wa_wosum-fsc(14).
** End of change
    READ TABLE it_result1001 WITH KEY atnam = 'P_OCN'   .
    wa_ocn     = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_VERSION'.
    wa_version = it_result1001-atwrt. CLEAR: it_result1001.
    IF sy-dynnr = '0106'.
      " User Input field for 219 Value... (Data Search..)
      CLEAR: l_no.
      DO  20 TIMES.
        l_no = l_no + 1.
        CONCATENATE 'WA_219-I'    l_no+1(2)  INTO l_fname.
        ASSIGN (l_fname)      TO <field1> .
        IF <field1> NE space.
          l_val = l_int = <field1>.    CONDENSE l_val.
          CONCATENATE 'P_219_' l_val         INTO l_fname.
          READ TABLE it_result1001 WITH KEY atnam = l_fname .
          CONCATENATE 'WA_219-O' l_no+1(2)   INTO l_fname.
          ASSIGN (l_fname)      TO <field2> .
          <field2> =  it_result1001-atwrt .  CLEAR: it_result1001.
        ENDIF.
      ENDDO.
      CLEAR: l_no.
    ENDIF.

    DO   9 TIMES.
      CLEAR: it_result1001-atwrt, it_alcu_a.
      l_no = l_no + 1.
      CONCATENATE 'P_ALC_U_'  l_no+2(1)  INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      READ TABLE it_result1001 WITH KEY atnam = l_fname .
      it_alcu_a-clm = l_no .
      it_alcu_a-vals = it_result1001-atwrt .
      APPEND it_alcu_a .
    ENDDO.

    DO  41 TIMES.
      CLEAR: it_result1001-atwrt .
      l_no = l_no + 1.
      CONCATENATE 'P_ALC_U_'  l_no+1(2)  INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      READ TABLE it_result1001 WITH KEY atnam = l_fname .
      it_alcu_a-clm = l_no .
      it_alcu_a-vals = it_result1001-atwrt .
      APPEND it_alcu_a .
    ENDDO.

    DO  49 TIMES.
      CLEAR: it_result1001-atwrt .
      l_no = l_no + 1.
      CONCATENATE 'P_ALC_U_'  l_no+1(2)  INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      READ TABLE it_result1001 WITH KEY atnam = l_fname .
      it_alcu_b-clm = l_no .
      it_alcu_b-vals = it_result1001-atwrt .
      APPEND it_alcu_b .
    ENDDO.

    CLEAR: it_result1001-atwrt .
    l_no = l_no + 1.
    CONCATENATE 'P_ALC_U_'  l_no       INTO l_fname.
    ASSIGN (l_fname)      TO <field1> .
    READ TABLE it_result1001 WITH KEY atnam = l_fname .
    it_alcu_b-clm = l_no .
    it_alcu_b-vals = it_result1001-atwrt .
    APPEND it_alcu_b .

    DO  50 TIMES.
      CLEAR: it_result1001-atwrt .
      l_no = l_no + 1.
      CONCATENATE 'P_ALC_U_'  l_no       INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      READ TABLE it_result1001 WITH KEY atnam = l_fname .
      it_alcu_c-clm = l_no .
      it_alcu_c-vals = it_result1001-atwrt .
      APPEND it_alcu_c .
    ENDDO.

    DO  50 TIMES.
      CLEAR: it_result1001-atwrt .
      l_no = l_no + 1.
      CONCATENATE 'P_ALC_U_'  l_no       INTO l_fname.
      ASSIGN (l_fname)      TO <field1> .
      READ TABLE it_result1001 WITH KEY atnam = l_fname .
      it_alcu_d-clm = l_no .
      it_alcu_d-vals = it_result1001-atwrt .
      APPEND it_alcu_d .
    ENDDO.
  ENDIF.

  IF sy-dynnr EQ '0105'.
    DESCRIBE TABLE it_alcu_a LINES tc_a105-lines.
    DESCRIBE TABLE it_alcu_b LINES tc_b105-lines.
    DESCRIBE TABLE it_alcu_c LINES tc_c105-lines.
    DESCRIBE TABLE it_alcu_d LINES tc_d105-lines.
  ELSEIF sy-dynnr EQ '0106'.
    DESCRIBE TABLE it_alcu_a LINES tc_a106-lines.
    DESCRIBE TABLE it_alcu_b LINES tc_b106-lines.
    DESCRIBE TABLE it_alcu_c LINES tc_c106-lines.
    DESCRIBE TABLE it_alcu_d LINES tc_d106-lines.
  ENDIF.
  " OCN VALUES PROCESSING
ENDFORM.                    " DISPLAY_1005

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_2001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_2001.

ENDFORM.                    " DISPLAY_2001

*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0042   text
*----------------------------------------------------------------------*
FORM excel_download USING    pa_itname.
  FIELD-SYMBOLS  <it_excel> TYPE INDEX TABLE.
  DATA: l_itname(50)      .

  PERFORM get_windows_clifile USING  ',*.xls.' CHANGING wa_filename.

*  CALL FUNCTION 'WS_DOWNLOAD'
*       EXPORTING
*            FILENAME                = wa_filename
*            FILETYPE                = 'DAT'
*       TABLES
*            DATA_TAB                = IT_EXCEL
*       EXCEPTIONS
*            FILE_OPEN_ERROR         = 1
*            FILE_WRITE_ERROR        = 2
*            INVALID_FILESIZE        = 3
*            INVALID_TYPE            = 4
*            NO_BATCH                = 5
*            UNKNOWN_ERROR           = 6
*            INVALID_TABLE_WIDTH     = 7
*            GUI_REFUSE_FILETRANSFER = 8
*            CUSTOMER_ERROR          = 9
*            OTHERS                  = 10.

  IF sy-subrc <> 0.
    MESSAGE e001(zmpp) WITH 'DOWNLOAD FIAL!!!!!'   .
  ENDIF.
ENDFORM.                    " EXCEL_DOWNLOAD

*&---------------------------------------------------------------------*
*&      Form  GET_WINDOWS_CLIFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0026   text
*      <--P_CLIFILE  text
*----------------------------------------------------------------------*
FORM get_windows_clifile USING mask CHANGING
                         clifile    LIKE rlgrap-filename .
  DATA winsys(3).
  DATA tmp_clifile    LIKE rlgrap-filename .

  IF clifile IS INITIAL.
    SET PARAMETER ID 'GR8' FIELD tmp_clifile.
    IF sy-subrc NE 0.CLEAR  tmp_clifile.ENDIF.
  ELSE.
    tmp_clifile =  clifile.
  ENDIF.
  CALL FUNCTION 'WS_QUERY'
    EXPORTING
      query  = 'WS'
    IMPORTING
      return = winsys.

  IF winsys(2) NE 'WN'.
    MESSAGE e016(14).
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
       def_filename    = tmp_clifile
       def_path         = tmp_clifile
       mask             = mask
       mode             = 'S'
       title            = sy-title
    IMPORTING
*ESO 11.04.01 d?ut de correction
       filename         = tmp_clifile
*       CLIFILE         = TMP_CLIFILE
*ESO 11.04.01 fin de correction de correction
*       RC               = RC
      EXCEPTIONS
         inv_winsys       = 1
         no_batch         = 2
         selection_cancel = 3
         selection_error  = 4
         OTHERS           = 5.

  IF sy-subrc EQ 0.
    clifile = tmp_clifile.
  ENDIF.
ENDFORM.                               " GET_WINDOWS_CLIFILE

*&---------------------------------------------------------------------*
*&      Form  display_1001_219
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_1001_219.
  FIELD-SYMBOLS: <field1>   TYPE any,
                 <field2>   TYPE any.
  DATA: l_fname(50)         TYPE c,
        l_no(3)             TYPE n,
        l_atnam             TYPE cabn-atnam,
        l_int               TYPE i,
        l_char(3)           TYPE c VALUE '0',
        l_carx              TYPE ztbm_abxopvdt-carx ,
        l_col(3)            TYPE  n.

  CHECK wa_err_flag = space .
  l_carx = wa_model+00(02).

  CLEAR: it_219, it_219[].
* 219 option value update
  DO 219 TIMES.
    CLEAR it_219.
    l_int = l_int + 1.
    WRITE l_int TO l_char LEFT-JUSTIFIED .
    CONCATENATE 'P_219_' l_char
      INTO l_atnam.
    CLEAR it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = l_atnam.
    MOVE: l_char              TO l_col         ,
          l_char              TO it_219-no     ,
          it_result1001-atwrt TO it_219-219vals.
    CLEAR: it_219-219code, it_219-219desc.
    PERFORM option_219_value USING     l_carx
                                       l_col
                                       it_219-219vals
                             CHANGING  it_219-219code
                                       it_219-219desc.
    APPEND it_219.

  ENDDO.

*  do   9 times.
*    l_no = l_no + 1.
*    it_219-no = l_no+2(1) .
*    concatenate 'P_219_'  l_no+2(1)  into l_fname.
*    assign (l_fname)      to <field1> .
*    read table it_result1001 with key atnam = l_fname .
*    it_219-219vals = it_result1001-atwrt .
*    it_219-219code = it_result1001-atwtb .
*    it_219-219desc = it_result1001-atbez .
*    append it_219 .
*  enddo.
*
*  do  90 times.
*    l_no = l_no + 1.
*    it_219-no = l_no+1(2) .
*    concatenate 'P_219_'  l_no+1(2)  into l_fname.
*    assign (l_fname)      to <field1> .
*    read table it_result1001 with key atnam = l_fname .
*    it_219-219vals = it_result1001-atwrt .
*    it_219-219code = it_result1001-atwtb .
*    it_219-219desc = it_result1001-atbez .
*    append it_219 .
*  enddo.
*
*  do 120 times.
*    l_no = l_no + 1.
*    it_219-no = l_no      .
*    concatenate 'P_219_'  l_no       into l_fname.
*    assign (l_fname)      to <field1> .
*    read table it_result1001 with key atnam = l_fname .
*    it_219-219vals = it_result1001-atwrt .
*    it_219-219code = it_result1001-atwtb .
*    it_219-219desc = it_result1001-atbez .
*    append it_219 .
*  enddo.

ENDFORM.                    " display_1001_219
*&---------------------------------------------------------------------*
*&      Form  SAVE_1001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_1001.
  " VIN CODE AND FLAG PROCESSING - SAVE  ( USING THE BDC )

ENDFORM.                                                    " SAVE_1001

*&---------------------------------------------------------------------*
*&      Form  SET_TITLEBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_titlebar.
  DATA: l_screen_text(70)    TYPE c.

  CASE sv_code .
    WHEN space .
    WHEN '0001'.  " Dummy Screen
      l_screen_text =
        'HMMA - Production Control Menu'          .
    WHEN '0101'.                                            "APP201
      l_screen_text = '1. Work Order - Head' .
    WHEN '0102'.                                            "APP202
      l_screen_text = '2. Work Order - Color' .
    WHEN '0103'.                                            "APP203
      l_screen_text = '3. ALC code - Color Parts ' .
    WHEN '0104'.                                            "APP204
      l_screen_text = '4. ALC code - Color Parts(Option)'.
    WHEN '0105'.                                            "APP205
      l_screen_text = '5. ALC code -Unique Parts'.
    WHEN '0106'.                                            "APP206
      l_screen_text = '6. ALC code -Unique Parts(Option)'.
    WHEN '0107'.                                            "APP207
      l_screen_text = '7. Error Work Order Check List'.
    WHEN '0108'.                                            "APP208
      l_screen_text = 'Error Work Order Status'    .
    WHEN '0109'.                                            "APP209
      l_screen_text = '8. Work Order Status' .
    WHEN '8081'.                                            "APP209
      l_screen_text = '81. Monthly Work Order' .
    WHEN '8082'.                                            "APP209
      l_screen_text = '82. Monthly Work Order (APS)' .
    WHEN '0110'.                                            "APP210
      l_screen_text = '9. Vehicle Status per Work Order'.
    WHEN '0111'.                                            "APP211
      l_screen_text = '10. ALC & Option Comparision'.
    WHEN '0118'.                                            "APP218
      l_screen_text = '11. Work Order due-in List' .
    WHEN '0701'.  "
      l_screen_text = 'Model Code Mapping' .
    WHEN '1201'.                                            "APP219
      l_screen_text = '12. 219 Option Code List'.
    WHEN '1202'.                                            "APP220
      l_screen_text = '13. 219 Option Code Contents'.
    WHEN '1203'.                                            "APP221
      l_screen_text = '14. Basic Data of Nation Code'   .
    WHEN '1205'.                                            "APP223
      l_screen_text = '15. ALC Code Table Contents'   .
    WHEN '1206'.                                            "APP223
      l_screen_text = 'Create ALC Data'          .
    WHEN '1209'.                                            "APP227
      l_screen_text = 'Work Reqeust relate to ALC'.
    WHEN '2101'.                                            "APP241
      l_screen_text = '16. Emission test List'.
    WHEN '2102'.                                            "APP242
      l_screen_text = '17. Emission Car Production Result'.
    WHEN '2103'.                                            "APP243
      l_screen_text = '18. Duplicated Engine number List'.
*   when '2104'.
*     l_screen_text = 'Duplicated Engine number vehicle status'.
*    WHEN '2101'.
*      L_SCREEN_TEXT = 'Vehicle History Inquiry'   .
*    WHEN '2102'.
*      L_SCREEN_TEXT = 'Waiting for Rework & Veh. in Long-Term stock'.
*    WHEN '2105'.
*      L_SCREEN_TEXT = 'Progress Input Sequence status'.
    WHEN '2106'.                                            "APP236
      l_screen_text = '19. Vehicle Production History'.
    WHEN '2107'.                                            "APP237
      l_screen_text = '20. Delayed Vehicle List'.
*    WHEN '2108'.
*      L_SCREEN_TEXT = 'Duplicated Engine number vehicle status'.
*   when '2109'.
*     l_screen_text = 'Product Results by Progress'.
*   when '2110'.
*     l_screen_text = 'Daily by Period or Option'.
*   when '2111'.
*     l_screen_text = 'In Line Status by Progress' .
*    WHEN '2112'.
*      L_SCREEN_TEXT = 'Emission test vehicle mgmt'.
    WHEN '2113'.                                            "APP239
      l_screen_text = '21. MITU List'.
    WHEN '2114'.                                            "APP240
      l_screen_text = '22. In-Line Progress List(ALC Code)'.
    WHEN '2115'.                                            "APP244
      l_screen_text = '24. Production Results (Reporting Point)'.
    WHEN '2116'.                                            "APP245
      l_screen_text = '25. Production Result (Daily)'.
    WHEN '2117'.                                            "APP246
      l_screen_text = '23. In-Line Progress List (219 Option)'.
*    when '2120'.
*      l_screen_text = 'Vehicle GR Processing'   .
*    when '2121'.
*      l_screen_text = 'Rejected Vehicle Status'   .
*    when '2122'.
*      l_screen_text = 'Vehicle GR Processing'   .
*    WHEN '2123'.
*      L_SCREEN_TEXT = 'Emission test vehicle mgmt'.
*    WHEN '2124'.
*      L_SCREEN_TEXT = 'Emission test vehicle mgmt'.
    WHEN '2200'.                                            "APP250
      l_screen_text = '26. Progress Change List'.
    WHEN '2201'.                                            "APP252
      l_screen_text = '27. Spec Change List'.
    WHEN '2202'.                                            "APP256
      l_screen_text = '28. Return Vehicle List'  .
    WHEN '2203'.                                            "APP257
      l_screen_text = '29. Return Car Management'.
    WHEN '2204'.                                            "APP260
      l_screen_text = '30. Scrap & Disposal Management' .
    WHEN '2205'.                                            "APP261
      l_screen_text = '31. Scrap, Disposal, Test List'  .
    WHEN '2206'.                                            "APP262
      l_screen_text = '32. Scrap & Disposal Car Summary'.
    WHEN '2207'.                                            "APP263
      l_screen_text = 'Production status per Vehicle'.
    WHEN '2301'.                                            "APP263
      l_screen_text = '33. Production Result (Engine)'.
    WHEN '3104'.                                            "APP272
      l_screen_text = '34. Input Plan per W/Order (Body) - APS'.
    WHEN '3107'.                                            "APP275
      l_screen_text = '35. HPCC vs ALC Code Comparison'.
    WHEN '3109'.                                            "APP278
      l_screen_text = '36. ALC & HPCS Code Description'.
    WHEN '3211'.                                            "APP279
      l_screen_text = '37. Delay List of S/OFF Status'.
    WHEN '3301'.                                            "APP290
      l_screen_text = '38. Production Summary (Hourly)' .
*    WHEN '3302'.                                            "APP291
*      l_screen_text = 'Real-time production status per progress'.
*    WHEN '3303'.                                            "APP293
*      l_screen_text = 'Production progress status'.
    WHEN '4101'.                                            "APP301
      l_screen_text = '39. ALC Parts Summary'.
    WHEN '4102'.                                            "APP302
      l_screen_text = '40. Production Plan by ALC Code'.
    WHEN '4103'.                                            "APP299
      l_screen_text = '41. Variant Table Name (Model)'.
    WHEN '4104'.                                            "APP298
      l_screen_text = '99. Output Report (Screen, Painter)'.
    WHEN '8088'.                                            "APP298
      l_screen_text = '88. Production Report'.
  ENDCASE.
  SET TITLEBAR 'TB' WITH l_screen_text.
ENDFORM.                    " SET_TITLEBAR

*&---------------------------------------------------------------------*
*&      Form  GET_INSTANCE_CHARC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ORDER  text
*----------------------------------------------------------------------*
FORM get_instance_charc USING    pa_workorder.
  DATA: l_tables             LIKE TABLE OF conf_out    WITH HEADER LINE,
        l_wohd               LIKE mara-matnr,
        l_atinn              LIKE ausp-atinn,
        l_typbz              LIKE itob-typbz,
        l_equnr              LIKE equi-equnr.

  CLEAR: wa_model.

  CHECK pa_workorder NE space  .
  l_wohd = pa_workorder(14)    .
  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = 'P_WORK_ORDER' .
*                                 'P_MODEL' .

  CHECK sy-subrc = 0.

  SELECT SINGLE objek INTO l_equnr
    FROM ausp
   WHERE atinn = l_atinn
     AND atwrt = l_wohd  .

  IF sy-subrc = 0 .
    SELECT SINGLE typbz INTO l_typbz
      FROM itob
     WHERE equnr = l_equnr .

    wa_model = l_typbz(3)  .
  ENDIF.
ENDFORM.                    " GET_INSTANCE_CHARC

*&---------------------------------------------------------------------*
*&      Form  list_box_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0617   text
*----------------------------------------------------------------------*
FORM list_box_function USING   p_list_name .
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = p_list_name  " list box
      values          = xlist
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
ENDFORM.                    " list_box_function

*&---------------------------------------------------------------------*
*&      Form  EMISSION_DATA_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM emission_data_selection.
  DATA : l_atinn  LIKE ausp-atinn.
  DATA : l_atinn2 LIKE ausp-atinn.
  DATA : lw_ausp  LIKE it_ausp   ,
         st_ox    LIKE TABLE OF ausp   WITH HEADER LINE.

  DATA : l_body1 LIKE it_ausp-atwrt,
         l_body2 LIKE it_ausp-atwrt.

  DATA: l_check,
        l_date  TYPE d,
        l_no(8) TYPE n,
        l_count TYPE i.

  RANGES : r_atnam FOR cabn-atnam,
           r_car FOR ausp-atwrt,
           r_eng FOR ausp-atwrt.

  CLEAR r_atnam[].
*model
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MODEL'.
  APPEND r_atnam. CLEAR r_atnam.
*body
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_BODY_SERIAL'.
  APPEND r_atnam. CLEAR r_atnam.
*work order
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_WORK_ORDER'.
  APPEND r_atnam. CLEAR r_atnam.
* vin
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_VIN'.
  APPEND r_atnam. CLEAR r_atnam.
* status(RP_STATUS)
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP_STATUS'.           " 'P_STATUS'.
  APPEND r_atnam. CLEAR r_atnam.
* Sequenced Date
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_SEQUENCE_DATE'.
  APPEND r_atnam. CLEAR r_atnam.
* RP18's Shop Date
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP18_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.
* test date
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_EMISSION_DATE'.
  APPEND r_atnam. CLEAR r_atnam.
* Engine Code
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_219_9'.        " ENGINE
  APPEND r_atnam. CLEAR r_atnam.
* Engine Type
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_219_38'.       " ENGINE TYOE
  APPEND r_atnam. CLEAR r_atnam.
* Emission Test Date
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_EMISSION_DATE'.
  APPEND r_atnam. CLEAR r_atnam.
*
  CLEAR : l_count, it_ausp, it_ausp[], it_2101[], it_2101.
*
* Read V/M with P_EMISSION = 'Y'.
  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = 'P_EMISSION' .

  SELECT objek INTO CORRESPONDING FIELDS OF TABLE st_ox
    FROM ausp
   WHERE atinn = l_atinn
     AND klart = '002'
     AND atwrt = 'Y'
     AND lkenz = ' ' .

* Second : Check V/M with P_VM_DATE = Between FDATE and TDATE.
  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = 'P_VM_DATE'  .
  LOOP AT st_ox.
    SELECT SINGLE atwrt INTO st_ox-atwrt
      FROM ausp
     WHERE objek = st_ox-objek
       AND atinn = l_atinn
       AND klart = '002'   .
    IF sy-subrc = 0 .
      IF st_ox-atwrt(8) >= st_2101-fdate AND
         st_ox-atwrt(8) <= st_2101-tdate   .
      ELSE.
        DELETE st_ox.
      ENDIF.
    ELSE.
      " Check the Creating Date of Vehicle Master
      SELECT SINGLE erdat INTO l_date
        FROM equi
       WHERE equnr = st_ox-objek
         AND erdat >= st_2101-fdate
         AND erdat <= st_2101-tdate .
      IF sy-subrc NE 0.
        DELETE st_ox.
      ELSE.

      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR : l_check, l_body1 , l_body2.
  LOOP AT st_ox.
    SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
      APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
     FROM zvpp_cha
     WHERE objek = st_ox-objek
       AND klart = '002'
       AND atnam IN r_atnam
       AND lkenz = ' ' .
  ENDLOOP.

  SORT it_ausp BY objek .
  READ TABLE it_ausp INDEX 1 .
  st_ox-objek = it_ausp-objek.

  LOOP AT it_ausp.
    IF st_ox-objek NE it_ausp-objek.
      READ TABLE it_ausp WITH KEY objek = st_ox-objek INTO lw_ausp.
      IF sy-subrc = 0.
        MOVE : st_ox-objek   TO it_2101-objek.
        l_count = l_count + 1.
        MOVE l_count TO it_2101-seq.
        IF it_2101-testdate <> ' '.
          it_2101-test = 'C'.
        ENDIF.
        it_2101-body =  it_2101-objek.
        APPEND it_2101. CLEAR it_2101.
      ENDIF.
      st_ox-objek = it_ausp-objek .
    ENDIF.

    CASE  it_ausp-atnam.
      WHEN 'P_MODEL'.
        IF it_ausp-atwrt NE wa_model .
          DELETE it_ausp WHERE objek = it_ausp-objek .
          CONTINUE .
        ELSE.
          l_body1 = it_ausp-atwrt.
        ENDIF.
      WHEN 'P_BODY_SERIAL'.
        l_body2 = it_ausp-atwrt.
      WHEN 'P_WORK_ORDER'.
        it_2101-order   = it_ausp-atwrt.
      WHEN 'P_VIN'.
        it_2101-vin     = it_ausp-atwrt.
      WHEN 'P_RP_STATUS'.
        it_2101-rp      = it_ausp-atwrt.
      WHEN 'P_SEQUENCE_DATE'.
        it_2101-seqdate = l_no = it_ausp-atflv.
      WHEN 'P_RP18_SHOP_DATE '.
        it_2101-s_off   = l_no = it_ausp-atflv.
      WHEN 'P_EMISSION_DATE'.
        it_2101-testdate = it_ausp-atwrt.
        it_2101-testdate = l_no = it_ausp-atflv.
      WHEN 'P_219_38'.
        IF  it_ausp-atwrt = st_2101-cabrator .
          it_2101-cabrator = it_ausp-atwrt.
        ELSE.
          IF  st_2101-cabrator = space .
            it_2101-cabrator = it_ausp-atwrt.
          ELSE.
            DELETE it_ausp WHERE objek = it_ausp-objek .
          ENDIF.
        ENDIF.
      WHEN 'P_219_9'.
        IF it_ausp-atwrt  = st_2101-engine . "IN r_eng.
          it_2101-bagigas = it_ausp-atwrt.
        ELSE.
          IF st_2101-engine = space  .
            it_2101-bagigas = it_ausp-atwrt.
          ELSE.
            DELETE it_ausp WHERE objek = it_ausp-objek .
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  CONCATENATE l_body1 l_body2 INTO it_2101-body.

*  CHECK  it_2101-body >= st_2101-body.
  READ TABLE it_ausp WITH KEY objek = st_ox-objek INTO lw_ausp.
  IF sy-subrc = 0.
    MOVE : st_ox-objek   TO it_2101-objek.
    l_count = l_count + 1.
    MOVE l_count TO it_2101-seq.
    IF it_2101-testdate <> ' '.
      it_2101-test = 'C'.
    ENDIF.
    APPEND it_2101. CLEAR it_2101.
  ENDIF.

  DESCRIBE TABLE it_2101 LINES tc_2101-lines.
  IF tc_2101-lines = 0.
    MESSAGE w001 WITH text-100 .
  ENDIF.
* table control line.
  tc_2101-top_line = 1.
ENDFORM.                    " EMISSION_DATA_SELECTION

*&---------------------------------------------------------------------*
*&      Form  EMISSION_DATA_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form emission_data_selection.
*  data : l_atinn  like ausp-atinn.
*  data : l_atinn2 like ausp-atinn.
*  data : begin of st_ox,
*         objek like ausp-objek,
*         atinn like ausp-atinn,
*         atzhl like ausp-atzhl,
*         mafid like ausp-mafid,
*         klart like ausp-klart,
*         adzhl like ausp-adzhl,
*         atwrt like ausp-atwrt,
*         end of st_ox.
*
*  data : l_body1 like it_ausp-atwrt,
*         l_body2 like it_ausp-atwrt.
*
*  data: l_check,
*        l_count type i.
*
*  ranges : r_atnam for cabn-atnam,
*           r_car for ausp-atwrt,
*           r_eng for ausp-atwrt.
*
*  clear r_atnam[].
**model
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_MODEL'.
*  append r_atnam. clear r_atnam.
**body
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_BODY_SERIAL'.
*  append r_atnam. clear r_atnam.
**work order
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_WORK_ORDER'.
*  append r_atnam. clear r_atnam.
** vin
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_VIN'.
*  append r_atnam. clear r_atnam.
** status
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_STATUS'.
*  append r_atnam. clear r_atnam.
**
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_SEQUENCE_DATE'.
*  append r_atnam. clear r_atnam.
*
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_RP18_SHOP_DATE'.
*  append r_atnam. clear r_atnam.
**test date
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_EMISSION_DATE'.
*  append r_atnam. clear r_atnam.
*
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_219_9'.
*  append r_atnam. clear r_atnam.
*
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_219_38'.
*  append r_atnam. clear r_atnam.
*
*  clear : l_count.
*  clear : it_2101[], it_2101.
*
*  select single atinn into l_atinn
*    from cabn
*   where atnam = 'P_EMISSION'
**                 'P_VM_DATE'                " 'P_SEQUENCE_DATE'.
*
*  select objek atinn atzhl mafid klart adzhl atwrt
*         into st_ox
*         from ausp
*         where atinn = l_atinn            " '0000000694'
*           and klart = '002'
*           and atwrt between st_2101-fdate and st_2101-tdate
*           and lkenz = ' ' .
*
*    clear : l_check, l_body1 , l_body2.
*
*    select  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
*        into corresponding fields of table it_ausp
*     from zvpp_cha
*     where objek = st_ox-objek
*       and klart = '002'
*       and atnam in r_atnam
*       and lkenz = ' ' .
*
*    loop at it_ausp.
** objek
*      move : st_ox-objek to it_2101-objek.
*
*      case  it_ausp-atnam.
*        when 'P_MODEL'.
*          if it_ausp-atwrt ne ztpp_veh_model-model.
*            l_check = 'X'.
*            exit.
*          else.
*            l_body1 = it_ausp-atwrt.
*          endif.
*        when 'P_BODY_SERIAL'.
*          l_body2 = it_ausp-atwrt.
*        when 'P_WORK_ORDER'.
*          it_2101-order   = it_ausp-atwrt.
*        when 'P_VIN'.
*          it_2101-vin     = it_ausp-atwrt.
*        when 'P_STATUS'.
*          it_2101-rp      = it_ausp-atwrt.
*        when 'P_SEQUENCE_DATE'.
*          it_2101-seqdate = it_ausp-atwrt.
*        when 'P_RP18_SHOP_DATE '.
*          it_2101-s_off   = it_ausp-atwrt.
*        when 'P_EMISSION_DATE'.
*          it_2101-testdate = it_ausp-atwrt.
*        when 'P_219_38'.
*          if st_2101-cabrator is initial.
*            it_2101-cabrator = it_ausp-atwrt.
*          else.
*            if  it_ausp-atwrt = st_2101-cabrator." IN r_car.
*              it_2101-cabrator = it_ausp-atwrt.
*            else.
*              l_check = 'X'.
*              exit.
*            endif.
*          endif.
*        when 'P_219_9'.
*          if st_2101-engine is initial.
*            it_2101-bagigas = it_ausp-atwrt.
*          else.
*            if it_ausp-atwrt  = st_2101-engine . "IN r_eng.
*              it_2101-bagigas = it_ausp-atwrt.
*            else.
*              l_check = 'X'.
*              exit.
*            endif.
*          endif.
*      endcase.
*    endloop.
*
*    concatenate l_body1 l_body2 into it_2101-body.
*
*    check  it_2101-body >= st_2101-body.
*
*    if l_check = '' .
*      l_count = l_count + 1.
*      move l_count to it_2101-seq.
*      if it_2101-testdate <> ' '.
*        it_2101-test = 'C'.
*      endif.
*      append it_2101. clear it_2101.
*    endif.
*  endselect.
*
** table control line.
*
*  tc_2101-top_line = 1.
*
*  if st_2101-engine <> '' .
*    delete it_2101 where bagigas = ''.
*  endif.
*
*  if st_2101-cabrator <> ''.
*    delete it_2101 where cabrator  = '' .
*  endif.
*
************************************
***************************************
** Because of the Changed Table Relationship, Program Source was changed
** By Tonkey On 01/27/2004
** Reference Table : ZTBM_ABXOPVDT
************************************
***************************************
** Request No. : UD1K906384
************************************
***************************************
*  data: l_vanm type ztbm_abxopvdt-vanm ,
*        l_valu type ztbm_abxopvdt-valu .
** entry conversion.
** engine
*  select ab~vanm ab~valu
*    into (l_vanm, l_valu)
*    from ztbm_abxopvdt as ab
*           inner join ztpp_veh_model as vh on ab~carx = vh~model02
*    where vh~model = ztpp_veh_model-model and
*          ab~clno = '009'.
*    move l_vanm to it_2101-bagigas.
*
*    modify it_2101 transporting bagigas
*                    where bagigas = l_valu .
*  endselect.
*
** carbrator
*  select ab~vanm ab~valu
*    into (l_vanm, l_valu)
*    from ztbm_abxopvdt as ab
*           inner join ztpp_veh_model as vh on ab~carx = vh~model02
*    where vh~model = ztpp_veh_model-model and
*          ab~clno = '038'.
*    move l_vanm to it_2101-cabrator.
*
*    modify it_2101 transporting cabrator
*                    where cabrator = l_valu .
*  endselect.
*  it_back2101[] = it_2101[].
*endform.                    " EMISSION_DATA_SELECTION

*&---------------------------------------------------------------------*
*&      Form  appending_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM appending_table.
  MOVE-CORRESPONDING st_sql TO it_2101.
  APPEND it_2101.
ENDFORM.                    " appending_table

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TC_NAME  text
*      -->P_L_OK  text
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*-END OF LOCAL DATA----------------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
* get looplines of TableControl
*  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  l_tc_lines_name = 'g_tc_2101_lines'.
  ASSIGN (l_tc_lines_name) TO <lines>.


* is no line filled?
*
  IF <tc>-lines = 0.
*   yes, ...
*
    l_tc_new_top_line = 1.
  ELSE.
*   no, ...
*
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act             = <tc>-top_line
        entry_from            = 1
        entry_to              = <tc>-lines
        last_page_full        = 'X'
        loops                 = <lines>
        ok_code               = p_ok
        overlapping           = 'X'
      IMPORTING
        entry_new             = l_tc_new_top_line
      EXCEPTIONS
        no_entry_or_page_act  = 01
        no_entry_to           = 02
        no_ok_code_or_page_go = 03
        OTHERS                = 99.
  ENDIF.

* get actual tc and column
*
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*     set actual column
*
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

* set the new top line
*
  <tc>-top_line = l_tc_new_top_line.

ENDFORM.                    " COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*&      Form  down_loading_execl_2101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM down_loading_execl_2101.

  DATA: l_count TYPE i.

  DATA : BEGIN OF it_excel2101  OCCURS 10,
         seq(10)    TYPE n,
         body       LIKE ausp-atwrt,
         vin        LIKE ausp-atwrt,
         order      LIKE ausp-atwrt,
         rp         LIKE ausp-atwrt,
         seqdate    LIKE ausp-atwrt,
         s_off      LIKE ausp-atwrt,
         cabrator   LIKE ausp-atwrt,
         bagigas    LIKE ausp-atwrt,
         test       ,
         testdate   LIKE ausp-atwrt,
         END OF it_excel2101.


  CLEAR : it_excel2101[], it_excel2101.

  LOOP AT it_2101.
    MOVE-CORRESPONDING it_2101 TO it_excel2101.
    APPEND it_excel2101.
  ENDLOOP.

  DESCRIBE TABLE it_2101 LINES l_count.
  IF l_count = 0.
    MESSAGE i000(zpp) WITH 'Empty data'.

  ENDIF.

  CHECK l_count > 0.

  CLEAR: it_excel2101.
  it_excel2101-seq      = 'FILENMAE   '  .
  it_excel2101-body = 'HkpsP101.xls' .
  INSERT       it_excel2101 INDEX 1     .
  it_excel2101-seq      = 'TITLE      '  .
  it_excel2101-body = 'ORDER BASICs' .
  INSERT       it_excel2101 INDEX 2     .
  it_excel2101-seq     = 'DATES     '   .
  CONCATENATE sy-datum   sy-uzeit            INTO  it_excel2101-body.
  INSERT       it_excel2101 INDEX 3     .
  it_excel2101-seq      = 'RECORDS   '   .
  it_excel2101-body = l_count        .
  INSERT       it_excel2101 INDEX 4     .

  CLEAR: it_excel2101.
  INSERT       it_excel2101 INDEX 5     .
  INSERT       it_excel2101 INDEX 6     .
  INSERT       it_excel2101 INDEX 7     .
  INSERT       it_excel2101 INDEX 8     .
  INSERT       it_excel2101 INDEX 9    .
  INSERT       it_excel2101 INDEX 10    .
  CLEAR: it_excel2101.


  it_excel2101-seq      = 'SEQUENCE'    .
  it_excel2101-body     = 'BODY-NO'       .
  it_excel2101-vin      = 'VIN' .
  it_excel2101-order = 'ORDER-NO'  .
  INSERT       it_excel2101 INDEX 11    .

  PERFORM get_windows_clifile USING    ',*.xls.'
                              CHANGING wa_filename.


  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      filename = wa_filename
      filetype = 'DAT'
    TABLES
      data_tab = it_excel2101.

ENDFORM.                    " down_loading_execl_2101
*&---------------------------------------------------------------------*
*&      Form  change_2101_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_2101_data.

  DATA it_val LIKE zspp_vin_value OCCURS 0 WITH HEADER LINE.
  DATA: l_objek LIKE equi-equnr.

  LOOP AT it_2101 WHERE mark = 'X'
                    AND test <> ' '
                    AND testdate = ' '.

    READ TABLE it_back2101 INDEX sy-tabix.

    IF sy-subrc EQ 0.

      CLEAR : it_val[], it_val,  l_objek.

      it_val-atnam = 'P_EMISSION_DATE'.
      it_val-atwrt = sy-datum.
      MOVE it_back2101-objek TO l_objek.

      APPEND it_val .
      CLEAR it_val.

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object       = l_objek
          mode         = 'W'
        TABLES
          val_table    = it_val
        EXCEPTIONS
          no_data      = 1
          error_mode   = 2
          error_object = 3
          error_value  = 4
          OTHERS       = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      it_2101-testdate = sy-datum.

      MODIFY it_2101.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " change_2101_data

*&---------------------------------------------------------------------*
*&      Form  CAL_REGION_PRODUCTION_SELECTIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_region_production_selectio.
* SELE*
  RANGES : r_atnam FOR cabn-atnam,
           r_car FOR ausp-atwrt,
           r_eng FOR ausp-atwrt.

  DATA : lt_objek    LIKE TABLE OF ausp                WITH HEADER LINE,
         lt_nation   LIKE TABLE OF ztpp_nation_def     WITH HEADER LINE,
         lw_ausp     LIKE ausp              ,
         l_atwrt     LIKE ausp-atwrt        ,
         l_worder    LIKE ausp-atinn        ,
         l_18date    LIKE ausp-atinn        ,
         l_nat       LIKE ausp-atinn        ,
         l_del       LIKE ausp-atinn        ,
         l_val       LIKE ztbm_abxopvdt-clnm,
         l_val2      LIKE ztbm_abxopvdt-clnm,
         l_carx      LIKE ztbm_abxopvdt-carx.

  DATA : BEGIN OF it_coll OCCURS 10,
         dest  LIKE ausp-atwrt,
         model LIKE ausp-atwrt,
         009   LIKE ausp-atwrt,
         038   LIKE ausp-atwrt,
         seqty TYPE i,
         teqty TYPE i,
         em_rate TYPE p,
         test  TYPE i,
         END OF it_coll.

  DATA: lt_2102 LIKE TABLE OF it_2102 WITH HEADER LINE.

  DATA : BEGIN OF st_ox ,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atzhl LIKE ausp-atzhl,
         mafid LIKE ausp-mafid,
         klart LIKE ausp-klart,
         adzhl LIKE ausp-adzhl,
         atwrt LIKE ausp-atwrt,
         END OF st_ox.

  DATA: l_header     TYPE ausp-atwrt,
        l_date       TYPE sy-datum,
        l_date_n(08) TYPE n,
        l_fatflv     TYPE ausp-atflv,
        l_tatflv     TYPE ausp-atflv.

  CLEAR: lt_objek, lt_objek[],
         it_coll, it_coll[],
         it_2102, it_2102[].

* First : Select the EMISSION Table (ZTPP_NATION_DEF)
*         Select the SIGN-OFF Data in the duration...
  CLEAR: lt_nation, lt_objek, lt_nation[], lt_objek[].

  PERFORM read_atinn  USING 'P_WORK_ORDER'     l_worder .
  PERFORM read_atinn  USING 'P_RP18_SHOP_DATE' l_18date .
  PERFORM read_atinn  USING 'P_NATN_CODE'      l_nat    .
  PERFORM read_atinn  USING 'P_DIST_CODE'      l_del    .

  SELECT * INTO TABLE lt_nation
    FROM ztpp_nation_def
   WHERE emission = 'X'        .

  l_fatflv = l_date_n = st_2102-fdate.
  l_tatflv = l_date_n = st_2102-tdate.

  LOOP AT lt_nation.
    SELECT *    APPENDING TABLE lt_objek
      FROM ausp
     WHERE objek IN ( SELECT objek FROM ausp
     WHERE objek IN ( SELECT objek
                        FROM ausp
                       WHERE objek IN ( SELECT objek
                                          FROM ausp
                                         WHERE atinn = l_18date
                                           AND klart = '002'
                                           AND atflv >= l_fatflv
                                           AND atflv <= l_tatflv      )
                         AND atinn = l_nat
                         AND klart = '002'
                         AND atwrt = lt_nation-nation(3) )
       AND atinn = l_del
       AND klart = '002'
       AND atwrt = lt_nation-nation+3(2) )
       AND atinn = l_worder
       AND klart = '002' .
  ENDLOOP.

*Second : Check the Destination & Emission Flag & Test DATE.
  LOOP AT lt_objek.
    lt_objek-atwrt = lt_objek-atwrt+9(5) .
*   Read Model 'P_MODEL'.
    CLEAR l_atwrt.
    PERFORM read_vm_char_inf USING    lt_objek-objek
                                      'P_MODEL'
                             CHANGING l_atwrt.
    CONCATENATE lt_objek-atwrt l_atwrt INTO lt_objek-atwrt.
    MODIFY lt_objek.
  ENDLOOP.

  SORT lt_objek BY atwrt.    CLEAR: l_atwrt.
  LOOP AT lt_objek.
    IF lt_objek-atwrt NE l_atwrt .
      IF l_atwrt NE space.
        it_coll-dest  = l_atwrt(5)  .
        it_coll-model = l_atwrt+5(3).
        APPEND it_coll .
        CLEAR: it_coll .
      ENDIF.
      l_atwrt        = lt_objek-atwrt  .
    ENDIF.

*   Count Sign-Off QTY.
    it_coll-seqty = it_coll-seqty + 1 .
*   Read Model 'P_EMISSION_DATE'.
    SELECT SINGLE *  INTO CORRESPONDING FIELDS OF lw_ausp
      FROM ausp AS au INNER JOIN cabn AS ca
        ON au~atinn = ca~atinn
     WHERE au~objek = lt_objek-objek AND
           au~klart = '002'   AND
           ca~atnam = 'P_EMISSION_DATE' .

    IF sy-subrc = 0 AND NOT lw_ausp-atflv IS INITIAL.
      it_coll-teqty = it_coll-teqty + 1.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_objek LINES wa_lines.
  IF wa_lines > 0.
    it_coll-dest  = l_atwrt(5)  .
    it_coll-model = l_atwrt+5(3).
    APPEND it_coll .
  ELSE.
    MESSAGE s001 WITH text-100.
    EXIT.
  ENDIF.

  " Summarize by the Nation & Model Code...
  SORT it_coll BY dest model.
  LOOP AT it_coll .
    CLEAR lt_2102.
    MOVE-CORRESPONDING it_coll TO lt_2102.
    COLLECT lt_2102.
  ENDLOOP.

  LOOP AT lt_2102.
    MOVE-CORRESPONDING lt_2102 TO it_2102.
**  Read Car's Description
    PERFORM read_car_desc_by_model USING    it_2102-model
                                   CHANGING it_2102-descr.
*   Calculate Rate
    IF it_2102-teqty <> 0.
      it_2102-rate = it_2102-teqty / it_2102-seqty * 100 .
    ENDIF.
    APPEND it_2102.
  ENDLOOP.
ENDFORM.                    " CAL_REGION_PRODUCTION_SELECTIO

*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT_2103
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_2103.

  DATA : BEGIN OF it_engine OCCURS 10,
         count TYPE i,
         engine  LIKE ausp-atwrt,
         END OF it_engine.

  DATA : BEGIN OF it_value OCCURS 10,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atwrt LIKE ausp-atwrt,
         END OF it_value .

  DATA : BEGIN OF it_temp OCCURS 10,
          objek LIKE ausp-objek,
          END OF it_temp .

  DATA : BEGIN OF it_temp1 OCCURS 10,
          objek LIKE ausp-objek,
          END OF it_temp1.

  DATA : BEGIN OF it_temp2 OCCURS 10,
          objek LIKE ausp-objek,
          END OF it_temp2 .

  DATA : BEGIN OF it_vel_eng OCCURS 10,
          atwrt LIKE ausp-atwrt,
          objek LIKE ausp-objek,
          END OF it_vel_eng .

  DATA : l_2103  TYPE i,
         l_cu03  TYPE i,
         l_atinn_eng TYPE ausp-atinn,
         l_atinn_shopdate TYPE ausp-atinn.
  DATA:  l_atflv_fm TYPE ausp-atflv,
         l_atflv_to TYPE ausp-atflv,

         l_num(08) TYPE n,
          l_count TYPE i.
  DATA: l_cn TYPE i,
       l_half TYPE i,
       l_to_cn TYPE i.

** Chnaged by Furong on 07/20/09

*  SELECT SINGLE ATINN
*     INTO L_ATINN_SHOPDATE
*     FROM CABN
*     WHERE ATNAM = 'P_RP18_SHOP_DATE'.
*
*  L_ATFLV_FM = L_NUM =  p_fdate_2103.
*  L_ATFLV_TO = L_NUM =  p_tdate_2103.
*
*  SELECT OBJEK INTO TABLE IT_TEMP
*    FROM AUSP
*    WHERE KLART = '002'
*      AND ATINN = L_ATINN_SHOPDATE
*      AND ATFLV >= L_ATFLV_FM
*      AND ATFLV <= L_ATFLV_TO.
*
*  IF SY-SUBRC = 0.
*    SELECT SINGLE ATINN
*     INTO L_ATINN_ENG
*     FROM CABN
*     WHERE ATNAM = 'P_ENGINE_NO'.
*
*    DESCRIBE TABLE IT_TEMP LINES L_CN.
*    IF L_CN > 10000.
*      L_HALF = L_CN / 2.
*
*      LOOP AT IT_TEMP FROM 1 TO L_HALF.
*        IT_TEMP1 = IT_TEMP.
*        APPEND IT_TEMP1.
*      ENDLOOP.
*      L_HALF = L_HALF + 1.
*      L_TO_CN = L_CN + 1.
*      LOOP AT IT_TEMP FROM L_HALF TO L_TO_CN.
*        IT_TEMP2 = IT_TEMP.
*        APPEND IT_TEMP2.
*      ENDLOOP.
*    ENDIF.
*    IF L_CN > 10000.
*      SELECT ATWRT OBJEK  INTO TABLE IT_VEL_ENG
*        FROM AUSP
*        FOR ALL ENTRIES IN IT_TEMP1
*        WHERE KLART = '002'
*          AND OBJEK = IT_TEMP1-OBJEK
*          AND ATINN = L_ATINN_ENG.
*      SELECT  ATWRT OBJEK APPENDING TABLE IT_VEL_ENG
*         FROM AUSP
*         FOR ALL ENTRIES IN IT_TEMP2
*         WHERE KLART = '002'
*           AND OBJEK = IT_TEMP2-OBJEK
*           AND ATINN = L_ATINN_ENG.
*
*    ELSE.
*      SELECT  ATWRT OBJEK INTO TABLE IT_VEL_ENG
*     FROM AUSP
*     FOR ALL ENTRIES IN IT_TEMP
*      WHERE KLART = '002'
*       AND OBJEK = IT_TEMP-OBJEK
*       AND ATINN = L_ATINN_ENG.
*    ENDIF.
*
*  ENDIF.
*
*  DELETE IT_VEL_ENG WHERE ATWRT IS INITIAL.
*
*  SORT IT_VEL_ENG BY ATWRT OBJEK.
*
*  LOOP AT IT_VEL_ENG.
*    IT_ENGINE-ENGINE = IT_VEL_ENG-ATWRT.
*    IT_ENGINE-COUNT = 1.
*    COLLECT IT_ENGINE.
*  ENDLOOP.
*
*  DELETE IT_ENGINE WHERE COUNT < 2.
*
*  LOOP AT IT_ENGINE .
*    MOVE :   IT_ENGINE-ENGINE TO IT_2103-ENGINE.
*    CLEAR: L_2103.
*
*    LOOP AT IT_VEL_ENG WHERE ATWRT = IT_2103-ENGINE.
*      L_2103 = L_2103 + 1.
*
*      CASE L_2103 .
*        WHEN '1'.
*          MOVE   IT_VEL_ENG-OBJEK TO  IT_2103-BODY1.
*        WHEN '2'.
*          MOVE   IT_VEL_ENG-OBJEK TO  IT_2103-BODY2.
*        WHEN '3'.
*          MOVE   IT_VEL_ENG-OBJEK TO  IT_2103-BODY3.
*        WHEN '4'.
*          MOVE   IT_VEL_ENG-OBJEK TO  IT_2103-BODY4.
*        WHEN '5'.
*          MOVE   IT_VEL_ENG-OBJEK TO  IT_2103-BODY5.
*        WHEN '6'.
*          L_CU03 = L_CU03 + 1.
*          MOVE L_CU03 TO IT_2103-SEQ.
*
*          APPEND IT_2103.
*          CLEAR  IT_2103.
*          MOVE :   IT_ENGINE-ENGINE TO IT_2103-ENGINE.
*          MOVE   IT_VEL_ENG-OBJEK TO  IT_2103-BODY1.
*
**          CLEAR : L_2103 .
*          L_2103 = 1 .
*      ENDCASE.
*    ENDLOOP.
*    L_CU03 = L_CU03 + 1.
*    MOVE : L_CU03 TO IT_2103-SEQ.
*
*    APPEND IT_2103. CLEAR IT_2103.
*
*  ENDLOOP.
*  CLEAR : L_2103.
*
** end of change

*--- dupulication data selection .
  SELECT SINGLE atinn
    INTO l_atinn_eng
    FROM cabn
    WHERE atnam = 'P_ENGINE_NO'.

  SELECT COUNT( * )  atwrt INTO TABLE it_engine
       FROM ausp
       WHERE klart = '002'
         AND atinn = l_atinn_eng
         AND lkenz = ' '
         GROUP BY atwrt
         HAVING COUNT( * ) > 1.

  CLEAR :it_value , it_value[], it_2103, it_2103[].

  DELETE it_engine WHERE engine IS INITIAL.
  LOOP AT it_engine.

    SELECT objek atinn atwrt APPENDING TABLE it_value
       FROM ausp

         WHERE atinn = l_atinn_eng
           AND klart = '002'
           AND lkenz = ' '
           AND atwrt = it_engine-engine .

  ENDLOOP.

  LOOP AT it_engine .
    MOVE :   it_engine-engine TO it_2103-engine.
    CLEAR: l_2103.

    LOOP AT it_value WHERE atwrt = it_2103-engine.
      l_2103 = l_2103 + 1.

      CASE l_2103 .
        WHEN '1'.
          MOVE   it_value-objek TO  it_2103-body1.
        WHEN '2'.
          MOVE   it_value-objek TO  it_2103-body2.
        WHEN '3'.
          MOVE   it_value-objek TO  it_2103-body3.
        WHEN '4'.
          MOVE   it_value-objek TO  it_2103-body4.
        WHEN '5'.
          MOVE   it_value-objek TO  it_2103-body5.
        WHEN '6'.
          l_cu03 = l_cu03 + 1.
          MOVE l_cu03 TO it_2103-seq.

          APPEND it_2103.
          CLEAR  it_2103.
          MOVE :   it_engine-engine TO it_2103-engine.
          MOVE   it_value-objek TO  it_2103-body1.

*          CLEAR : L_2103 .
          l_2103 = 1 .
      ENDCASE.
    ENDLOOP.
    l_cu03 = l_cu03 + 1.
    MOVE : l_cu03 TO it_2103-seq.

    APPEND it_2103. CLEAR it_2103.

  ENDLOOP.
  CLEAR : l_2103.

ENDFORM.                    " DATA_SELECT_2103
*&---------------------------------------------------------------------*
*&      Form  DATA_PICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_pick.

  DATA : l_line      LIKE sy-index,
         l_field(40),
         l_read_line LIKE sy-index.

  FIELD-SYMBOLS : <fn> TYPE any.

  CLEAR g_body_no.

  GET CURSOR LINE    l_line.
  GET CURSOR FIELD   l_field.

  l_read_line = l_line + tc_2103-top_line - 1.

  READ TABLE  it_2103 INDEX l_read_line.

  ASSIGN (l_field) TO <fn>.

  g_body_no = <fn>.

  IF g_body_no IS INITIAL.
  ELSE.
    ok_code = '2104'.
  ENDIF.

ENDFORM.                    " DATA_PICK
*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT_2104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_2104.




  DATA : l_atinn  LIKE ausp-atinn.
  DATA : l_atinn2 LIKE ausp-atinn.
  DATA : BEGIN OF st_ox,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atzhl LIKE ausp-atzhl,
         mafid LIKE ausp-mafid,
         klart LIKE ausp-klart,
         adzhl LIKE ausp-adzhl,
         atwrt LIKE ausp-atwrt,
         END OF st_ox.

  DATA : l_body1 LIKE it_ausp-atwrt,
         l_body2 LIKE it_ausp-atwrt.

  DATA: l_check,
        l_count TYPE i.

  RANGES : r_atnam FOR cabn-atnam,
           r_car FOR ausp-atwrt,
           r_eng FOR ausp-atwrt.

  CLEAR r_atnam[].
* C/F
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP17_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.
* SIGN OFF DATE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP18_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.
* ENGINE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_ENGINE_NO'.
  APPEND r_atnam. CLEAR r_atnam.
* TRIM NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_TM_NO'.
  APPEND r_atnam. CLEAR r_atnam.
* KEY NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_KEY_NO'.
  APPEND r_atnam. CLEAR r_atnam.
*WORK ORDER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_WORK_ORDER'.
  APPEND r_atnam. CLEAR r_atnam.
* STATUS
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_STATUS'.
  APPEND r_atnam. CLEAR r_atnam.
*TRIM PLANT
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_TRIM_PLANT_NO'.
  APPEND r_atnam. CLEAR r_atnam.


****************************
  CLEAR : l_count.
  CLEAR : it_2104[], it_2104.


  SELECT objek atinn atzhl mafid klart adzhl atwrt
         INTO st_ox
         FROM ausp
         UP TO 100 ROWS
         WHERE klart = '002'
           AND objek >= st_2104-body
           AND lkenz = ' ' .


    CLEAR : l_check, l_body1 , l_body2.

    SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
        INTO CORRESPONDING FIELDS OF TABLE it_ausp
     FROM zvpp_cha
     WHERE objek = st_ox-objek
       AND klart = '002'
       AND atnam IN r_atnam
       AND lkenz = ' ' .



    LOOP AT it_ausp.
* objek
      MOVE : st_ox-objek TO it_2104-body_no.

      CASE  it_ausp-atnam.

        WHEN 'P_RP17_SHOP_DATE'.
          it_2104-cf = it_ausp-atwrt.

        WHEN 'P_RP18_SHOP_DATE'.
          it_2104-s_off = it_ausp-atwrt.

        WHEN 'P_ENGINE_NO'.
          it_2104-engine = it_ausp-atwrt.

          IF st_2104-selc = 'E' AND it_ausp-atwrt <> ''.
            l_check = 'X'.
            EXIT.
          ENDIF.

        WHEN 'P_TM_NO'.
          it_2104-tm = it_ausp-atwrt.

          IF st_2104-selc = 'T' AND it_ausp-atwrt <> ''.
            l_check = 'X'.
            EXIT.
          ENDIF.

        WHEN 'P_KEY_NO'.
          it_2104-key = it_ausp-atwrt.

          IF st_2104-selc = 'K' AND it_ausp-atwrt <> ''.
            l_check = 'X'.
            EXIT.
          ENDIF.

        WHEN 'P_WORK_ORDER'.
          it_2104-worder = it_ausp-atwrt.

        WHEN 'P_STATUS'.
          it_2104-status = it_ausp-atwrt.

        WHEN 'TRIM_PLANT_NO'.
          it_2104-trim = it_ausp-atwrt.

      ENDCASE.

    ENDLOOP.

    IF l_check = ''.
      APPEND it_2104. CLEAR it_2104.
    ENDIF.

  ENDSELECT.





ENDFORM.                    " DATA_SELECT_2104
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGE_2104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_change_2104.

  DATA it_val LIKE zspp_vin_value OCCURS 0 WITH HEADER LINE.
  DATA: l_objek LIKE equi-equnr.

  LOOP AT it_2104   WHERE mark = 'X'
                    AND ( tm      <> ' '
                    OR    engine  <>  ' '
                    OR    key     <>  ' ' ).



    CLEAR : it_val[], it_val,  l_objek.

    MOVE it_2104-body_no TO l_objek.
    IF it_2104-engine <> ' '.
      it_val-atnam = 'P_ENGINE_NO'.
      it_val-atwrt = it_2104-engine.

      APPEND it_val .
      CLEAR it_val.
    ENDIF.
    IF it_2104-tm <> ''.
      it_val-atnam = 'P_TM_NO'.
      it_val-atwrt = it_2104-tm.

      APPEND it_val .
      CLEAR it_val.
    ENDIF.

    IF it_2104-key <> ''.
      it_val-atnam = 'P_KEY_NO'.
      it_val-atwrt = it_2104-key.

      APPEND it_val .
      CLEAR it_val.
    ENDIF.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object       = l_objek
        mode         = 'W'
      TABLES
        val_table    = it_val
      EXCEPTIONS
        no_data      = 1
        error_mode   = 2
        error_object = 3
        error_value  = 4
        OTHERS       = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.




  ENDLOOP.


ENDFORM.                    " DATA_CHANGE_2104
*&---------------------------------------------------------------------*
*&      Form  SCREEN_BACK_2103
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_back_2103.

* BACK TO SCREEN 2103.

  ok_code = '2103'.

  CLEAR : it_2104[],it_2104.

ENDFORM.                    " SCREEN_BACK_2103
*&---------------------------------------------------------------------*
*&      Form  DATA_NEXT_100_ENTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_next_100_entr.
  DATA : l_atinn  LIKE ausp-atinn.
  DATA : l_atinn2 LIKE ausp-atinn.
  DATA : BEGIN OF st_ox,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atzhl LIKE ausp-atzhl,
         mafid LIKE ausp-mafid,
         klart LIKE ausp-klart,
         adzhl LIKE ausp-adzhl,
         atwrt LIKE ausp-atwrt,
         END OF st_ox.

  DATA : l_body1 LIKE it_ausp-atwrt,
         l_body2 LIKE it_ausp-atwrt.

  DATA : l_index TYPE i,
         l_check.


  RANGES : r_atnam FOR cabn-atnam,
           r_car FOR ausp-atwrt,
           r_eng FOR ausp-atwrt.

  CLEAR r_atnam[].
* C/F
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP17_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.
* SIGN OFF DATE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP18_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.
* ENGINE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_ENGIN_NO'.
  APPEND r_atnam. CLEAR r_atnam.
* TRIM NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_TM_NO'.
  APPEND r_atnam. CLEAR r_atnam.
* KEY NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_KEY_NO'.
  APPEND r_atnam. CLEAR r_atnam.
*WORK ORDER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_WORK_ORDER'.
  APPEND r_atnam. CLEAR r_atnam.
* STATUS
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_STATUS'.
  APPEND r_atnam. CLEAR r_atnam.
*TRIM PLANT
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_TRIM_PLANT_NO'.
  APPEND r_atnam. CLEAR r_atnam.


****************************

  DESCRIBE TABLE it_2104 LINES l_index.

  READ TABLE it_2104 INDEX l_index.
  IF sy-subrc EQ 0.
    st_2104-body = it_2104-body_no.
  ENDIF.

  CLEAR : it_2104[], it_2104.

  SELECT objek atinn atzhl mafid klart adzhl atwrt
         INTO st_ox
         FROM ausp
         UP TO 100 ROWS
         WHERE klart = '002'
           AND objek > st_2104-body
           AND lkenz = ' ' .


    CLEAR :  l_body1 , l_body2.

    SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
        INTO CORRESPONDING FIELDS OF TABLE it_ausp
     FROM zvpp_cha
     WHERE objek = st_ox-objek
       AND klart = '002'
       AND atnam IN r_atnam
       AND lkenz = ' ' .


    CLEAR : l_check .

    LOOP AT it_ausp.
* objek
      MOVE : st_ox-objek TO it_2104-body_no.

      CASE  it_ausp-atnam.

        WHEN 'P_RP17_SHOP_DATE'.
          it_2104-cf = it_ausp-atwrt.

        WHEN 'P_RP18_SHOP_DATE'.
          it_2104-s_off = it_ausp-atwrt.

        WHEN 'P_ENGINE_NO'.
          it_2104-engine = it_ausp-atwrt.

          IF st_2104-selc = 'E' AND it_ausp-atwrt <> ''.
            l_check = 'X'.
            EXIT.
          ENDIF.

        WHEN 'P_TM_NO'.
          it_2104-tm = it_ausp-atwrt.

          IF st_2104-selc = 'T' AND it_ausp-atwrt <> ''.
            l_check = 'X'.
            EXIT.
          ENDIF.

        WHEN 'P_KEY_NO'.
          it_2104-key = it_ausp-atwrt.

          IF st_2104-selc = 'K' AND it_ausp-atwrt <> ''.
            l_check = 'X'.
            EXIT.
          ENDIF.

        WHEN 'P_WORK_ORDER'.
          it_2104-worder = it_ausp-atwrt.

        WHEN 'P_STATUS'.
          it_2104-status = it_ausp-atwrt.

        WHEN 'TRIM_PLANT_NO'.
          it_2104-trim = it_ausp-atwrt.

      ENDCASE.

    ENDLOOP.

    IF l_check = ''.
      APPEND it_2104. CLEAR it_2104.
    ENDIF.


  ENDSELECT.
ENDFORM.                    " DATA_NEXT_100_ENTR
*&---------------------------------------------------------------------*
*&      Module  LIST_BOX_2202  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_box_2202 OUTPUT.
  DATA : l_2202 TYPE i.

  DESCRIBE TABLE it_2202 LINES l_2202.
  tc_2202-lines = l_2202.

  CLEAR : xlist[],xvalue.
  name = 'WA_MODEL'.
  PERFORM set_field_model USING name wa_model.

* Plant
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Plant 1'.
  xvalue-key  = '1'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Plant 2'.
  xvalue-key  = '2'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Plant 3'.
  xvalue-key  = '3'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Plant 4'.
  xvalue-key  = '4'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Plant 5'.
  xvalue-key  = '5'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_2202-PLANT'.
  IF st_2202-plant IS INITIAL.
    st_2202-plant = '1'.
  ENDIF.

* USE
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Domestic'.
  xvalue-key  = 'D'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Export'.
  xvalue-key  = 'E'.
  APPEND xvalue TO xlist .


  PERFORM list_box_function USING 'ST_2202-USE'.

* SELECTION

  CLEAR : xlist[] , xvalue.

  xvalue-text = 'Total '.
  xvalue-key  = '1'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Transfer  '.
  xvalue-key  = '2'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Donot Transper  '.
  xvalue-key  = '3'.
  APPEND xvalue TO xlist .

  xvalue-text = 'Donot Transper total '.
  xvalue-key  = '4'.
  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'ST_2202-SEL'.

  IF st_2202-sel IS INITIAL.
    st_2202-sel =  '1'.
  ENDIF.

* SORT

  CLEAR : xlist[] , xvalue.

  xvalue-text = 'BODY-NO '.
  xvalue-key  = 'B'.
  APPEND xvalue TO xlist .

  xvalue-text = 'PERIOD'.
  xvalue-key  = 'P'.
  APPEND xvalue TO xlist .


  PERFORM list_box_function USING 'ST_2202-SORT'.

  IF st_2202-sort IS INITIAL .
    st_2202-sort = 'B'.
  ENDIF.

  IF st_2202-spmon IS INITIAL.
    MOVE : sy-datum(6) TO st_2202-spmon.
  ENDIF.

ENDMODULE.                 " LIST_BOX_2202  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  data_select_2202
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_2202.
  DATA : l_atinn  LIKE ausp-atinn.
  DATA : l_atinn2 LIKE ausp-atinn.
  DATA : BEGIN OF st_ox,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atzhl LIKE ausp-atzhl,
         mafid LIKE ausp-mafid,
         klart LIKE ausp-klart,
         adzhl LIKE ausp-adzhl,
         atwrt LIKE ausp-atwrt,
         END OF st_ox.

  DATA : l_body1 LIKE it_ausp-atwrt,
         l_body2 LIKE it_ausp-atwrt.

  DATA : l_index TYPE i,
         l_check,
         l_begin LIKE sy-datum,
         l_end   LIKE sy-datum,
         l_seq   TYPE i,
         l_date   LIKE ausp-atwrt.

  RANGES : r_atnam FOR cabn-atnam,
           r_car FOR ausp-atwrt,
           r_eng FOR ausp-atwrt.

  CLEAR r_atnam[].
* MODEL
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MODEL'.
  APPEND r_atnam. CLEAR r_atnam.

* BODY NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_BODY_SERIAL'.
  APPEND r_atnam. CLEAR r_atnam.

* WORK ORDER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_WORK_ORDER'.
  APPEND r_atnam. CLEAR r_atnam.

*  SPEC
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MI'.
  APPEND r_atnam. CLEAR r_atnam.

* EXTERNAL COLOR
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_EXT_COLOR'.
  APPEND r_atnam. CLEAR r_atnam.

*INTERNAL COLOR
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_INT_COLOR'.
  APPEND r_atnam. CLEAR r_atnam.

* RETURN DATE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RETURN_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* REWORK DATE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RETURN_REWORK_DATE'.
  APPEND r_atnam. CLEAR r_atnam.
***** DATE  DATA

  CONCATENATE : st_2202-spmon '01' INTO l_begin.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = l_begin
    IMPORTING
      last_day_of_month = l_end
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

****************************
  DATA : l_atflv_st TYPE ausp-atflv,
         l_atflv_en TYPE ausp-atflv,
         l_num(08)  TYPE n.
  CLEAR : it_2202[], it_2202.
  CLEAR : l_seq .
  l_atflv_st = l_num = l_begin .
  l_atflv_en = l_num = l_end .
  SELECT objek au~atinn
               au~atzhl
               au~mafid
               au~klart
               au~adzhl
               au~atwrt
         INTO st_ox
         FROM ausp AS au
              INNER JOIN cabn AS ca ON au~atinn = ca~atinn
         WHERE au~klart = '002'
           AND ca~atnam = 'P_RETURN_DATE'
           AND au~atflv BETWEEN l_atflv_st AND l_atflv_en
           AND au~lkenz = ' '  .

    CLEAR :  l_body1 , l_body2.

    SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
        INTO CORRESPONDING FIELDS OF TABLE it_ausp
     FROM zvpp_cha
     WHERE objek = st_ox-objek
       AND klart = '002'
       AND atnam IN r_atnam
       AND lkenz = ' ' .

    CLEAR : l_check .

    LOOP AT it_ausp.
* objek
      CASE  it_ausp-atnam.
        WHEN 'P_MODEL'.
          MOVE : it_ausp-atwrt TO it_2202-body(3).
        WHEN 'P_BODY_SERIAL'.
          MOVE : it_ausp-atwrt TO it_2202-body+4(10).
        WHEN 'P_WORK_ORDER'.
          it_2202-order  = it_ausp-atwrt.
        WHEN 'P_EXT_COLOR'.
          it_2202-ecolor = it_ausp-atwrt.
        WHEN 'P_INT_COLOR'.
          it_2202-icolor = it_ausp-atwrt.
        WHEN 'P_MI'.
          it_2202-spec = it_ausp-atwrt.
        WHEN 'P_RETURN_DATE'.
          it_2202-rdate = l_num = it_ausp-atflv.
        WHEN 'P_RETURN_REWORK_DATE'.
          it_2202-wdate = l_num = it_ausp-atflv.
      ENDCASE.
    ENDLOOP.

    IF l_check IS INITIAL .
      l_seq = l_seq + 1.
      it_2202-seq = l_seq .
      IF it_2202-wdate IS INITIAL.
        CLEAR l_date.
        MOVE : sy-datum TO l_date.
        it_2202-peried = l_date - it_2202-rdate.
      ELSE.
        it_2202-peried = it_2202-wdate - it_2202-rdate.
      ENDIF.
      CONDENSE it_2202-peried NO-GAPS.
      APPEND it_2202. CLEAR it_2202.
    ENDIF.
  ENDSELECT.

  SORT it_2202 BY body.
  tc_2202-top_line = 1 .
ENDFORM.                    " data_select_2202

*&---------------------------------------------------------------------*
*&      Form  DATA_INSERT_RETURN_2203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_insert_return_2203.
  DATA it_val LIKE zspp_vin_value OCCURS 0 WITH HEADER LINE.
  DATA: l_objek LIKE equi-equnr,
        l_sq(2) TYPE n.

  CLEAR : it_val[], it_val,  l_objek.

  CONCATENATE wa_model  st_2203_input-body INTO  l_objek .
  IF l_objek = ' '.
    MESSAGE i002(zmpp) WITH 'INTPUT MODEL OR BODY NUMBER'.
    EXIT.
  ENDIF.

  it_val-atnam = 'P_RETURN_DATE'.
  it_val-atwrt = sy-datum.
  APPEND it_val . CLEAR it_val.

  it_val-atnam = 'P_USAGE_CAR'.
  it_val-atwrt = 'R'.
  APPEND it_val . CLEAR it_val.


  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_objek
      mode         = 'W'
    TABLES
      val_table    = it_val
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MESSAGE i001(zmpp) WITH 'Successful insert'.
    CLEAR st_2203.
  ENDIF.
ENDFORM.                    " DATA_INSERT_RETURN_2203

*&---------------------------------------------------------------------*
*&      Form  data_select_2203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_2203.
  DATA : l_atinn  LIKE ausp-atinn.
  DATA : l_atinn2 LIKE ausp-atinn.
  DATA : l_body1 LIKE it_ausp-atwrt,
         l_body2 LIKE it_ausp-atwrt,
         l_model LIKE it_ausp-atwrt,
         l_objek LIKE it_ausp-atwrt.
  DATA : l_index TYPE i,
         l_begin LIKE sy-datum,
         l_end   LIKE sy-datum,
         l_seq   TYPE i,
         l_date   LIKE ausp-atwrt.

  PERFORM range_insert_2203.  " range value

  CLEAR : st_2203.
  CLEAR : l_seq .
  CLEAR :  l_body1 , l_body2, l_model, l_objek.

  CONCATENATE wa_model  st_2203_input-body INTO l_objek    .

  SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
      INTO CORRESPONDING FIELDS OF TABLE it_ausp
   FROM zvpp_cha
   WHERE objek = l_objek
     AND klart = '002'
     AND atnam IN r_atnam
     AND lkenz = ' ' .

*--- status check..
  READ TABLE it_ausp WITH KEY atnam = 'P_RP_STATUS'.
  IF sy-subrc EQ 0.
    IF it_ausp-atwrt < '18'.
      MESSAGE i002  WITH 'DONOT EXIT DATA'.
      EXIT.
    ELSE.
      MESSAGE s003  WITH 'Display' wa_model  st_2203_input-body.
      st_2203-file = 'MAST'.
    ENDIF.
  ELSE.
    EXIT.
  ENDIF.

*--- Return Date check..
  READ TABLE it_ausp WITH KEY atnam = 'P_RETURN_DATE'.
  IF sy-tcode = 'ZPPA9999'.
    IF sy-subrc NE 0.  EXIT.  ENDIF.
  ENDIF.

* DATA COLLECTION.
  LOOP AT it_ausp.
    CONDENSE it_ausp-atwrt.
    CASE  it_ausp-atnam.
      WHEN 'P_MODEL'.
        MOVE : it_ausp-atwrt TO  l_model.
      WHEN 'P_BODY_SERIAL'.
        MOVE : it_ausp-atwrt TO l_objek.
      WHEN 'P_WORK_ORDER'.
        MOVE : it_ausp-atwrt TO st_2203-order.
      WHEN 'P_EXT_COLOR'.
        MOVE : it_ausp-atwrt TO st_2203-ecolor.
      WHEN 'P_INT_COLOR'.
        MOVE : it_ausp-atwrt TO st_2203-icolor.
      WHEN 'P_MI'.
        MOVE : it_ausp-atwrt TO st_2203-mi.
      WHEN 'P_RETURN_DATE'.
*        IF NOT it_ausp-atflv IS INITIAL.
*          l_check = 'X'.
*          EXIT.
*        ENDIF.
      WHEN 'P_OCN'.
        MOVE : it_ausp-atwrt TO st_2203-ocn.
      WHEN 'P_VERSION'.
        MOVE : it_ausp-atwrt TO st_2203-vern.
      WHEN 'P_ENGINE_NO'.
        MOVE : it_ausp-atwrt TO st_2203-engine.
      WHEN 'P_STATUS'.
        MOVE : it_ausp-atwrt TO st_2203-status.
      WHEN 'P_TM'.
        MOVE : it_ausp-atwrt TO st_2203-tm.
      WHEN 'P_KEY'.
        MOVE : it_ausp-atwrt TO st_2203-key.
      WHEN 'P_VIN'.
        MOVE : it_ausp-atwrt TO st_2203-vin.
      WHEN OTHERS .
        IF it_ausp-atnam+7(4) = 'SHOP'.
          PERFORM correct_value_2203_shop USING it_ausp-atflv
                                                it_ausp-atnam.
        ELSEIF it_ausp-atnam+7(4) = 'ACTU'.
          PERFORM correct_value_2203_actual USING it_ausp-atwrt
                                                it_ausp-atnam.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " data_select_2203

*&---------------------------------------------------------------------*
*&      Form  range_insert_2203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM range_insert_2203.

  CLEAR r_atnam[].
* MODEL
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MODEL'.
  APPEND r_atnam. CLEAR r_atnam.

* BODY NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_BODY_SERIAL'.
  APPEND r_atnam. CLEAR r_atnam.

* WORK ORDER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_WORK_ORDER'.
  APPEND r_atnam. CLEAR r_atnam.

*  SPEC
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MI'.
  APPEND r_atnam. CLEAR r_atnam.

* EXTERNAL COLOR
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_EXT_COLOR'.
  APPEND r_atnam. CLEAR r_atnam.

*INTERNAL COLOR
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_INT_COLOR'.
  APPEND r_atnam. CLEAR r_atnam.

* RETURN DATE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RETURN_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* OCN
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_OCN'.
  APPEND r_atnam. CLEAR r_atnam.

* KEY NO
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_KEY_NO'.
  APPEND r_atnam. CLEAR r_atnam.

* VIN
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_VIN'.
  APPEND r_atnam. CLEAR r_atnam.

* TRIM
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_TM'.
  APPEND r_atnam. CLEAR r_atnam.

* VERSION
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_VERSION'.
  APPEND r_atnam. CLEAR r_atnam.

* STATUS
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_STATUS'.
  APPEND r_atnam. CLEAR r_atnam.

* RP_STATUS
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP_STATUS'.
  APPEND r_atnam. CLEAR r_atnam.

* ENGINE NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_ENGINE_NO'.
  APPEND r_atnam. CLEAR r_atnam.

  DATA :          l_sq(2) TYPE n.

* Atual date
  CLEAR l_sq.

  DO 27 TIMES .
    l_sq = l_sq + 1.

    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    CONCATENATE 'P_RP' l_sq '_ACTUAL_DATE' INTO r_atnam-low .
    APPEND r_atnam. CLEAR r_atnam.

    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    CONCATENATE 'P_RP' l_sq '_SHOP_DATE' INTO r_atnam-low .
    APPEND r_atnam. CLEAR r_atnam.

  ENDDO.

ENDFORM.                    " range_insert_2203
*&---------------------------------------------------------------------*
*&      Form  correct_value_2203_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_AUSP_ATWRT  text
*----------------------------------------------------------------------*
FORM correct_value_2203_shop USING    p_atflv
                                      p_atnam .
  DATA: l_num(08) TYPE n         ,
        l_atflv   TYPE ausp-atflv.

  CASE p_atnam.
    WHEN 'P_RP01_SHOP_DATE'.          " shop body in
      st_2203-01 = l_num = p_atflv .
*      move p_atwrt to st_2203-01.
    WHEN 'P_RP02_SHOP_DATE'.          " paint in
      st_2203-02 = l_num = p_atflv .
*      move p_atwrt to st_2203-02.
    WHEN 'P_RP03_SHOP_DATE'.          " t/c
      st_2203-03 = l_num = p_atflv .
*      move p_atwrt to st_2203-03.
    WHEN 'P_RP04_SHOP_DATE'.          " paint out
      st_2203-04 = l_num = p_atflv .
*      move p_atwrt to st_2203-04.
    WHEN 'P_RP05_SHOP_DATE'.          " pbs in
      st_2203-05 = l_num = p_atflv .
*      move p_atwrt to st_2203-05.
    WHEN 'P_RP06_SHOP_DATE'.          " pbs out
      st_2203-06 = l_num = p_atflv .
*      move p_atwrt to st_2203-06.
    WHEN 'P_RP07_SHOP_DATE'.          " trim in
      st_2203-07 = l_num = p_atflv .
*      move p_atwrt to st_2203-07.
    WHEN 'P_RP17_SHOP_DATE'.          " c / final
      st_2203-08 = l_num = p_atflv .
*      move p_atwrt to st_2203-08.
    WHEN 'P_RP18_SHOP_DATE'.          " s/off
      st_2203-09 = l_num = p_atflv .
*      move p_atwrt to st_2203-09.
    WHEN 'P_RP19_SHOP_DATE'.          " c/ gate
      st_2203-10 = l_num = p_atflv .
*      move p_atwrt to st_2203-10.
    WHEN 'P_RP021_SHOP_DATE'.         " vpc in
      st_2203-11 = l_num = p_atflv .
*      move p_atwrt to st_2203-11.
    WHEN 'P_RP22_SHOP_DATE'.          " vpc out
      st_2203-12 = l_num = p_atflv .
*      move p_atwrt to st_2203-12.
    WHEN 'P_RP23_SHOP_DATE'.
      st_2203-13 = l_num = p_atflv .
*      move p_atwrt to st_2203-13.
    WHEN 'P_RP24_SHOP_DATE'.
      st_2203-14 = l_num = p_atflv .
*      move p_atwrt to st_2203-14.
    WHEN 'P_RP25_SHOP_DATE'.
      st_2203-15 = l_num = p_atflv .
*      move p_atwrt to st_2203-15.
    WHEN 'P_RP26_SHOP_DATE'.
      st_2203-16 = l_num = p_atflv .
*      move p_atwrt to st_2203-16.
*   WHEN 'P_RP27_SHOP_DATE'.
*      move p_atwrt to st_2203-13.


  ENDCASE.

ENDFORM.                    " correct_value_2203_shop
*&---------------------------------------------------------------------*
*&      Form  correct_value_2203_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_AUSP_ATWRT  text
*      -->P_IT_AUSP_ATNAM  text
*----------------------------------------------------------------------*
FORM correct_value_2203_actual USING    p_atwrt
                                        p_atnam.
  CASE p_atnam.
    WHEN 'P_RP01_ACTUAL_DATE'.          " shop body in
      MOVE p_atwrt TO st_2203-01_r.
      MOVE p_atwrt+8(6) TO st_2203-01_t.
    WHEN 'P_RP02_ACTUAL_DATE'.          " paint in
      MOVE p_atwrt TO st_2203-02_r.
      MOVE p_atwrt+8(6) TO st_2203-02_t.
    WHEN 'P_RP03_ACTUAL_DATE'.          " t/c
      MOVE p_atwrt TO st_2203-03_r.
      MOVE p_atwrt+8(6) TO st_2203-03_t.
    WHEN 'P_RP04_ACTUAL_DATE'.          " paint out
      MOVE p_atwrt TO st_2203-04_r.
      MOVE p_atwrt+8(6) TO st_2203-04_t.
    WHEN 'P_RP05_ACTUAL_DATE'.          " pbs in
      MOVE p_atwrt TO st_2203-05_r.
      MOVE p_atwrt+8(6) TO st_2203-05_t.
    WHEN 'P_RP06_ACTUAL_DATE'.          " pbs out
      MOVE p_atwrt TO st_2203-06_r.
      MOVE p_atwrt+8(6) TO st_2203-06_t.
    WHEN 'P_RP07_ACTUAL_DATE'.          " trim in
      MOVE p_atwrt TO st_2203-07_r.
      MOVE p_atwrt+8(6) TO st_2203-07_t.
    WHEN 'P_RP17_ACTUAL_DATE'.          " c / final
      MOVE p_atwrt TO st_2203-08_r.
      MOVE p_atwrt+8(6) TO st_2203-08_t.
    WHEN 'P_RP18_ACTUAL_DATE'.          " s/off
      MOVE p_atwrt TO st_2203-09_r.
      MOVE p_atwrt+8(6) TO st_2203-09_t.
    WHEN 'P_RP19_ACTUAL_DATE'.          " c/ gate
      MOVE p_atwrt TO st_2203-10_r.
      MOVE p_atwrt+8(6) TO st_2203-10_t.
    WHEN 'P_RP21_ACTUAL_DATE'.         " vpc in
      MOVE p_atwrt TO st_2203-11_r.
      MOVE p_atwrt+8(6) TO st_2203-11_t.
    WHEN 'P_RP22_ACTUAL_DATE'.          " vpc out
      MOVE p_atwrt TO st_2203-12_r.
      MOVE p_atwrt+8(6) TO st_2203-12_t.
    WHEN 'P_RP23_ACTUAL_DATE'.
      MOVE p_atwrt TO st_2203-13_r.
      MOVE p_atwrt+8(6) TO st_2203-13_t.
    WHEN 'P_RP24_ACTUAL_DATE'.
      MOVE p_atwrt TO st_2203-14_r.
      MOVE p_atwrt+8(6) TO st_2203-14_t.
    WHEN 'P_RP25_ACTUAL_DATE'.
      MOVE p_atwrt TO st_2203-15_r.
      MOVE p_atwrt+8(6) TO st_2203-15_t.
    WHEN 'P_RP26_ACTUAL_DATE'.
      MOVE p_atwrt TO st_2203-16_r.
      MOVE p_atwrt+8(6) TO st_2203-16_t.
  ENDCASE.
ENDFORM.                    " correct_value_2203_actual

*&---------------------------------------------------------------------*
*&      Form  data_select_2204
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_2204.
  DATA : l_atinn      LIKE ausp-atinn,
         l_atinn2     LIKE ausp-atinn,
         l_body1      LIKE it_ausp-atwrt,
         l_body2      LIKE it_ausp-atwrt,
         l_model      LIKE it_ausp-atwrt,
         l_objek      LIKE it_ausp-atwrt,
         l_status(2)  TYPE n       ,
         l_index      TYPE i,
         l_begin      LIKE sy-datum,
         l_end        LIKE sy-datum,
         l_seq        TYPE i,
         l_date       LIKE ausp-atwrt,
         l_num(08)    TYPE n         ,
         l_atflv      TYPE ausp-atflv.

  DATA : BEGIN OF st_ox,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atzhl LIKE ausp-atzhl,
         mafid LIKE ausp-mafid,
         klart LIKE ausp-klart,
         adzhl LIKE ausp-adzhl,
         atwrt LIKE ausp-atwrt,
         END OF st_ox.

  PERFORM range_insert_2204.  " range value

  CLEAR :  st_2204, l_seq, l_body1 , l_body2, l_model, l_objek, st_ox.
  CONCATENATE wa_model  st_2204_input-body INTO st_ox-objek.

  SELECT objek atinn atzhl mafid klart adzhl atwrt atflv atnam
    INTO CORRESPONDING FIELDS OF TABLE it_ausp
    FROM zvpp_cha
   WHERE objek = st_ox-objek AND
         klart = '002'       AND
         atnam IN r_atnam    AND
         lkenz = ' '           .

  IF sy-subrc NE 0.
    MESSAGE i003(zmpp) WITH 'Entry' st_ox-objek 'does not exist'.
    EXIT.
  ENDIF.

  LOOP AT it_ausp.
    CONDENSE it_ausp-atwrt.
    CASE  it_ausp-atnam.
      WHEN 'P_MODEL'.
        MOVE : it_ausp-atwrt TO  l_model.
      WHEN 'P_BODY_SERIAL'.
        MOVE : it_ausp-atwrt TO l_objek.
      WHEN 'P_WORK_ORDER'.
        MOVE : it_ausp-atwrt TO st_2204-order.
      WHEN 'P_EXT_COLOR'.
        MOVE : it_ausp-atwrt TO st_2204-exclr.
      WHEN 'P_INT_COLOR'.
        MOVE : it_ausp-atwrt TO st_2204-inclr.
      WHEN 'P_MI'.
        MOVE : it_ausp-atwrt TO st_2204-mi.
      WHEN 'P_TM'.
        MOVE : it_ausp-atwrt TO st_2204-tm.
      WHEN 'P_OCN'.
        MOVE : it_ausp-atwrt TO st_2204-ocn.
      WHEN 'P_VERSION'.
        MOVE : it_ausp-atwrt TO st_2204-ver.
      WHEN 'P_ENGINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-engine.
      WHEN 'P_STATUS'.
        MOVE : it_ausp-atwrt TO st_2204-locl.
      WHEN 'P_TM'.
        MOVE : it_ausp-atwrt TO st_2204-tm.
      WHEN 'P_RP_STATUS' .
        CASE it_ausp-atwrt .
          WHEN '01' .
            st_2204-sbody  = 'X' .
          WHEN '02' OR '03' OR '04' .
            st_2204-spaint = 'X' .
          WHEN '05' OR '06' OR '07' OR '08' OR '09' OR '10' OR
               '11' OR '12' OR '13' OR '14' OR '15' OR '16' OR '17'.
            st_2204-strim  = 'X' .
          WHEN OTHERS.
        ENDCASE.
        l_status = it_ausp-atwrt.
      WHEN 'P_SCRAP_APP_DOC'.
        MOVE : it_ausp-atwrt TO st_2204-scrap.
      WHEN 'P_USAGE_CAR' .
        MOVE : it_ausp-atwrt TO st_2204-car  .
      WHEN 'P_USAGE_TYPE'.
        MOVE : it_ausp-atwrt TO st_2204-usage.
      WHEN 'P_USAGE_TEXT'.
        MOVE : it_ausp-atwrt TO st_2204-text.
      WHEN 'P_USAGE_DEPT'.
        MOVE : it_ausp-atwrt TO st_2204-u_dept.
      WHEN 'P_SEQUENCE_DATE'.
        st_2204-seq = l_num = it_ausp-atflv.
      WHEN 'P_RP01_SHOP_DATE'.
        st_2204-bdin = l_num = it_ausp-atflv.
      WHEN 'P_RP02_SHOP_DATE'.
        st_2204-paint_in = l_num = it_ausp-atflv.
      WHEN 'P_RP04_SHOP_DATE'.
        st_2204-paint_out = l_num = it_ausp-atflv.
      WHEN 'P_RP07_SHOP_DATE'.
        st_2204-trim_in = l_num = it_ausp-atflv.
      WHEN 'P_RP18_SHOP_DATE'.
        st_2204-sign_off = l_num = it_ausp-atflv.
      WHEN 'P_RP19_SHOP_DATE'.
        st_2204-c_gate = l_num = it_ausp-atflv.
** Changed by furong on 06/18/09
*      WHEN 'P_RP21_SHOP_DATE'.
      WHEN 'P_RP23_SHOP_DATE'.
        st_2204-vpc = l_num = it_ausp-atflv.
** end of change
      WHEN 'P_SCRAP_DATE'.
        st_2204_input-date = l_num = it_ausp-atflv.
    ENDCASE.
  ENDLOOP.

  CASE st_2204-car .
    WHEN  'S' .
      MESSAGE s000 WITH text-011 .
    WHEN  'D' .
      MESSAGE s000 WITH text-014 .
    WHEN OTHERS.
      IF sy-tcode = 'ZPPA9999'.
        CLEAR st_2204.
        MESSAGE s000 WITH text-100 .
      ELSE.
        w_status = l_status.
        IF l_status < 18 .
          MESSAGE i000 WITH text-400 .
        ELSE.
          MESSAGE i000 WITH text-300 .
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.                    " data_select_2204

*&---------------------------------------------------------------------*
*&      Form  DATA_INSERT_RETURN_2204
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_insert_return_2204.
  DATA it_val LIKE zspp_vin_value OCCURS 0 WITH HEADER LINE.
  DATA: l_objek LIKE equi-equnr.
  DATA: it_before LIKE zspp_vin_value OCCURS 0 WITH HEADER LINE,
        it_after  LIKE zspp_vin_value OCCURS 0 WITH HEADER LINE.
  DATA : l_return TYPE zzret.

  CLEAR : it_val[], it_val,  l_objek.

  CONCATENATE wa_model st_2204_input-body INTO  l_objek.

  it_val-atnam = 'P_SCRAP_RP'.
  it_val-atwrt = st_2204-locl.
  APPEND it_val .  CLEAR it_val.

  it_val-atnam = 'P_SCRAP_ACCT'.
  it_val-atwrt = st_2204-scrap.
  APPEND it_val .  CLEAR it_val.

  it_val-atnam = 'P_USAGE_TYPE'.
  it_val-atwrt = st_2204-usage.
  APPEND it_val .  CLEAR it_val.

  it_val-atnam = 'P_USAGE_TEXT'.
  it_val-atwrt = st_2204-text.
  APPEND it_val .  CLEAR it_val.

  it_val-atnam = 'P_SCRAP_DATE'.
  it_val-atwrt = sy-datum.
  APPEND it_val .  CLEAR it_val.

  it_val-atnam = 'P_USAGE_DEPT'.
  it_val-atwrt = st_2204-u_dept.
  APPEND it_val .  CLEAR it_val.

*  IF st_2204-usage = 'YD'.
*    it_val-atnam = 'P_USAGE_CAR'.
*    it_val-atwrt = 'U'.
*    APPEND it_val .  CLEAR it_val.
*  ELSE.
  it_val-atnam = 'P_USAGE_CAR'.
  it_val-atwrt = 'S'.
  APPEND it_val .  CLEAR it_val.
*  ENDIF.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_objek
      mode         = 'R'
    TABLES
      val_table    = it_before
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_objek
      mode         = 'W'
    TABLES
      val_table    = it_val
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'Z_FPP_VEHICLE_SCRAP'
    EXPORTING
      equnr        = l_objek
    IMPORTING
      return       = l_return
    TABLES
      before_vin   = it_before
      after_vin    = it_val
    EXCEPTIONS
      master_error = 1
      status_error = 2
      OTHERS       = 3.

  IF sy-subrc <> 0.
  ENDIF.

  IF l_return = 'S'.
    COMMIT WORK.
    MESSAGE s002(zmpp) WITH 'Successful insert'.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " DATA_INSERT_RETURN_2204
*&---------------------------------------------------------------------*
*&      Form  DATA_NEXT_ENTRY_2204
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_next_entry_2204.
  DATA : l_atinn  LIKE ausp-atinn.
  DATA : l_atinn2 LIKE ausp-atinn.
  DATA : BEGIN OF st_ox,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atzhl LIKE ausp-atzhl,
         mafid LIKE ausp-mafid,
         klart LIKE ausp-klart,
         adzhl LIKE ausp-adzhl,
         atwrt LIKE ausp-atwrt,
         END OF st_ox.

  DATA : l_body1 LIKE it_ausp-atwrt,
         l_body2 LIKE it_ausp-atwrt,
         l_model LIKE it_ausp-atwrt,
         l_objek LIKE ausp-objek.

  DATA : l_index TYPE i,
         l_check,
         l_begin LIKE sy-datum,
         l_end   LIKE sy-datum,
         l_seq   TYPE i,
         l_date   LIKE ausp-atwrt.

  PERFORM range_insert_2204.  " range value

  CLEAR :  st_2204, l_seq, l_body1 , l_body2, l_model, l_objek, st_ox.

  CONCATENATE wa_model st_2204_input-body INTO st_ox-objek.

  IF st_ox-objek IS INITIAL.
    MESSAGE i002(zmpp) WITH 'Input model and body number '.
    EXIT.
  ENDIF.

  SELECT objek INTO l_objek
         FROM ausp
         WHERE objek > st_ox-objek
           AND klart = '002'
           AND lkenz = ' '.
    EXIT.
  ENDSELECT.

  IF sy-subrc NE 0.
    MESSAGE i003(zmpp) WITH 'Entry' st_ox-objek 'does not exist'.
    EXIT.
  ELSE.
    MOVE : l_objek(3)    TO wa_model,
           l_objek+3(10) TO st_2204_input-body.
  ENDIF.

  CONCATENATE wa_model st_2204_input-body INTO st_ox-objek.

  SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
      INTO CORRESPONDING FIELDS OF TABLE it_ausp
   FROM zvpp_cha
   WHERE objek = st_ox-objek
     AND klart = '002'
     AND atnam IN r_atnam
     AND lkenz = ' ' .

  IF sy-subrc NE 0.
    MESSAGE i003(zmpp) WITH 'Entry' st_ox-objek 'does not exist'.
    EXIT.
  ENDIF.

  CLEAR : l_check .

  LOOP AT it_ausp.
    CONDENSE it_ausp-atwrt.
    CASE  it_ausp-atnam.
      WHEN 'P_MODEL'.
        MOVE : it_ausp-atwrt TO  l_model.
      WHEN 'P_BODY_SERIAL'.
        MOVE : it_ausp-atwrt TO l_objek.
      WHEN 'P_WORK_ORDER'.
        MOVE : it_ausp-atwrt TO st_2204-order.
      WHEN 'P_EXT_COLOR'.
        MOVE : it_ausp-atwrt TO st_2204-exclr.
      WHEN 'P_INT_COLOR'.
        MOVE : it_ausp-atwrt TO st_2204-inclr.
      WHEN 'P_MI'.
        MOVE : it_ausp-atwrt TO st_2204-mi.
      WHEN 'P_TM'.
        MOVE : it_ausp-atwrt TO st_2204-tm.
      WHEN 'P_OCN'.
        MOVE : it_ausp-atwrt TO st_2204-ocn.
      WHEN 'P_VERSION'.
        MOVE : it_ausp-atwrt TO st_2204-ver.
      WHEN 'P_ENGINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-engine.
      WHEN 'P_STATUS'.
        MOVE : it_ausp-atwrt TO st_2204-locl.
      WHEN 'P_TM'.
        MOVE : it_ausp-atwrt TO st_2204-tm.
      WHEN 'P_BODY_LINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-sbody.
      WHEN 'P_PAINT_LINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-spaint.
      WHEN 'P_TRIM_LINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-strim.
      WHEN 'P_SCRAP_ACCT'.
        MOVE : it_ausp-atwrt TO st_2204-scrap.
      WHEN 'P_USAGE_TYPE'.
        MOVE : it_ausp-atwrt TO st_2204-scrap.
      WHEN 'P_USAGE_TEXT'.
        MOVE : it_ausp-atwrt TO st_2204-text.
      WHEN 'P_USAGE_DEPT'.
        MOVE : it_ausp-atwrt TO st_2204-u_dept.

      WHEN 'P_SEQUENCE_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-seq .
      WHEN 'P_RP01_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-bdin.
      WHEN 'P_RP02_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-paint_in.
      WHEN 'P_RP04_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-paint_out.
      WHEN 'P_RP07_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-trim_in.
      WHEN 'P_RP18_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-sign_off.
      WHEN 'P_RP19_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-c_gate .
** Changed by Furong on 06/18/09
*      WHEN 'P_RP21_SHOP_DATE'.
      WHEN 'P_RP23_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-vpc    .
** End of change
      WHEN 'P_SCRAP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204_input-date.
    ENDCASE.
  ENDLOOP.

* FIRST DATA FOR ENTRY
  IF st_2204_b IS INITIAL.
    st_2204_b = st_2204_input.
  ENDIF.
ENDFORM.                    " DATA_NEXT_ENTRY_2204

*&---------------------------------------------------------------------*
*&      Form  range_insert_2204
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM range_insert_2204.
  CLEAR r_atnam[].
* MODEL
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MODEL'.
  APPEND r_atnam. CLEAR r_atnam.

* BODY NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_BODY_SERIAL'.
  APPEND r_atnam. CLEAR r_atnam.

* STATUS
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_STATUS'.
  APPEND r_atnam. CLEAR r_atnam.

* RP_STATUS
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP_STATUS'.
  APPEND r_atnam. CLEAR r_atnam.

* WORK ORDER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_WORK_ORDER'.
  APPEND r_atnam. CLEAR r_atnam.

* OCN
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_OCN'.
  APPEND r_atnam. CLEAR r_atnam.

* VERSION
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_VERSION'.
  APPEND r_atnam. CLEAR r_atnam.

* EXTERNAL COLOR
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_EXT_COLOR'.
  APPEND r_atnam. CLEAR r_atnam.

*INTERNAL COLOR
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_INT_COLOR'.
  APPEND r_atnam. CLEAR r_atnam.

*  SPEC
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MI'.
  APPEND r_atnam. CLEAR r_atnam.

* ENGINE NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_ENGINE_NO'.
  APPEND r_atnam. CLEAR r_atnam.

* TRIM
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_TM'.
  APPEND r_atnam. CLEAR r_atnam.

* Scrap acct
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_SCRAP_ACCT'.
  APPEND r_atnam. CLEAR r_atnam.

* USAGE TYPE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_USAGE_TYPE'.
  APPEND r_atnam. CLEAR r_atnam.

* USAGE CAR
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_USAGE_CAR'.
  APPEND r_atnam. CLEAR r_atnam.

* USAGE TEXT
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_USAGE_TEXT'.
  APPEND r_atnam. CLEAR r_atnam.

* USAGE DEPT
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_USAGE_DEPT'.
  APPEND r_atnam. CLEAR r_atnam.


* SEQUENCE DATE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_SEQUENCE_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* BODY IN
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP01_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* PAINT IN
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP02_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* PAINT OUT
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP04_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* TRIM IN
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP07_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* SIGN OFF
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP18_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* C/GATE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP19_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

** Changed by furong on 06/18/09
** VPC
*  R_ATNAM-SIGN  = 'I'.
*  R_ATNAM-OPTION = 'EQ'.
*  R_ATNAM-LOW    = 'P_RP21_SHOP_DATE'.
*  APPEND R_ATNAM. CLEAR R_ATNAM.
*
* VPC
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP23_SHOP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.
** End of change

* SCRAP DATE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_SCRAP_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* SCRAP DOC NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_SCRAP_APP_DOC'.
  APPEND r_atnam. CLEAR r_atnam.

ENDFORM.                    " range_insert_2204

*&---------------------------------------------------------------------*
*&      Form  DATA_FIRST_ENTRY_2204
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_first_entry_2204.
  DATA : l_atinn  LIKE ausp-atinn.
  DATA : l_atinn2 LIKE ausp-atinn.
  DATA : BEGIN OF st_ox,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atzhl LIKE ausp-atzhl,
         mafid LIKE ausp-mafid,
         klart LIKE ausp-klart,
         adzhl LIKE ausp-adzhl,
         atwrt LIKE ausp-atwrt,
         END OF st_ox.

  DATA : l_body1 LIKE it_ausp-atwrt,
         l_body2 LIKE it_ausp-atwrt,
         l_model LIKE it_ausp-atwrt,
         l_objek LIKE it_ausp-atwrt.

  DATA : l_index TYPE i,
         l_check,
         l_begin LIKE sy-datum,
         l_end   LIKE sy-datum,
         l_seq   TYPE i,
         l_date   LIKE ausp-atwrt.

  PERFORM range_insert_2204.  " range value

  CLEAR: st_2204, l_seq , l_body1 , l_body2, l_model, l_objek, st_ox.

  CONCATENATE wa_model      st_2204_b-body INTO st_ox-objek.
* MOVE : st_2204_b-model    TO WA_MODEL          ,
  MOVE : st_2204_b-body     TO st_2204_input-body.

  IF st_ox-objek IS INITIAL.
    MESSAGE i002(zmpp) WITH 'Input model and body number '.
  ENDIF.

  SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
      INTO CORRESPONDING FIELDS OF TABLE it_ausp
   FROM zvpp_cha
   WHERE objek = st_ox-objek
     AND klart = '002'
     AND atnam IN r_atnam
     AND lkenz = ' ' .

  IF sy-subrc NE 0.
    MESSAGE i003(zmpp) WITH 'Entry' st_ox-objek 'does not exist'.
    EXIT.
  ENDIF.

  CLEAR : l_check .

  LOOP AT it_ausp.
    CONDENSE it_ausp-atwrt.
    CASE  it_ausp-atnam.
      WHEN 'P_MODEL'.
        MOVE : it_ausp-atwrt TO  l_model.
      WHEN 'P_BODY_SERIAL'.
        MOVE : it_ausp-atwrt TO l_objek.
      WHEN 'P_WORK_ORDER'.
        MOVE : it_ausp-atwrt TO st_2204-order.
      WHEN 'P_EXT_COLOR'.
        MOVE : it_ausp-atwrt TO st_2204-exclr.
      WHEN 'P_INT_COLOR'.
        MOVE : it_ausp-atwrt TO st_2204-inclr.
      WHEN 'P_MI'.
        MOVE : it_ausp-atwrt TO st_2204-mi.
      WHEN 'P_TM'.
        MOVE : it_ausp-atwrt TO st_2204-tm.
      WHEN 'P_OCN'.
        MOVE : it_ausp-atwrt TO st_2204-ocn.
      WHEN 'P_VERSION'.
        MOVE : it_ausp-atwrt TO st_2204-ver.
      WHEN 'P_ENGINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-engine.
      WHEN 'P_STATUS'.
        MOVE : it_ausp-atwrt TO st_2204-locl.
      WHEN 'P_TM'.
        MOVE : it_ausp-atwrt TO st_2204-tm.
      WHEN 'P_BODY_LINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-sbody.
      WHEN 'P_PAINT_LINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-spaint.
      WHEN 'P_TRIM_LINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-strim.
      WHEN 'P_SCRAP_ACCT'.
        MOVE : it_ausp-atwrt TO st_2204-scrap.
      WHEN 'P_USAGE_TYPE'.
        MOVE : it_ausp-atwrt TO st_2204-scrap.
      WHEN 'P_USAGE_TEXT'.
        MOVE : it_ausp-atwrt TO st_2204-text.
      WHEN 'P_USAGE_DEPT'.
        MOVE : it_ausp-atwrt TO st_2204-u_dept.

      WHEN 'P_SEQUENCE_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-seq .
      WHEN 'P_RP01_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-bdin.
      WHEN 'P_RP02_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-paint_in.
      WHEN 'P_RP04_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-paint_out.
      WHEN 'P_RP07_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-trim_in.
      WHEN 'P_RP18_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-sign_off.
      WHEN 'P_RP19_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-c_gate .
      WHEN 'P_RP21_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-vpc    .
      WHEN 'P_SCRAP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204_input-date.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " DATA_FIRST_ENTRY_2204

*&---------------------------------------------------------------------*
*&      Form  DATA_NEXT_DATE_2204
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_next_date_2204.
  DATA : l_atinn  LIKE ausp-atinn.
  DATA : l_atinn2 LIKE ausp-atinn.

  DATA : BEGIN OF st_ox,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atzhl LIKE ausp-atzhl,
         mafid LIKE ausp-mafid,
         klart LIKE ausp-klart,
         adzhl LIKE ausp-adzhl,
         atwrt LIKE ausp-atwrt,
         END OF st_ox.

  DATA : l_body1 LIKE it_ausp-atwrt,
         l_body2 LIKE it_ausp-atwrt,
         l_model LIKE it_ausp-atwrt,
         l_objek LIKE it_ausp-atwrt.


  DATA : l_index TYPE i,
         l_check,
         l_begin LIKE sy-datum,
         l_end   LIKE sy-datum,
         l_seq   TYPE i,
         l_date   LIKE ausp-atwrt,
         l_next_date LIKE sy-datum.

  PERFORM range_insert_2204.  " range value

  CLEAR :  st_2204.
  CLEAR :  l_seq .
  CLEAR :  l_body1 , l_body2, l_model, l_objek.

  CLEAR st_ox.

  CONCATENATE wa_model   st_2204_input-body INTO st_ox-objek.

  l_next_date = st_2204_input-date.
  PERFORM next_date USING l_next_date.
  st_2204_input-date = l_next_date .

  IF st_ox-objek IS INITIAL.
    MESSAGE i002(zmpp) WITH 'Input model and body number '.
    EXIT.
  ENDIF.

  SELECT objek INTO l_objek
          FROM ausp
          WHERE objek >= st_ox-objek
            AND atinn = '0000000789'
            AND atwrt = l_next_date
            AND klart = '002'
            AND lkenz = ' '.
    EXIT.
  ENDSELECT.

  IF sy-subrc NE 0.
    MESSAGE i003(zmpp) WITH 'Entry does not exist'.
    EXIT.
  ENDIF.

  SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
      INTO CORRESPONDING FIELDS OF TABLE it_ausp
   FROM zvpp_cha
   WHERE objek = st_ox-objek
     AND klart = '002'
     AND atnam IN r_atnam
     AND lkenz = ' ' .

  IF sy-subrc NE 0.
    MESSAGE i003(zmpp) WITH 'Entry' st_ox-objek 'does not exist'.
    EXIT.
  ENDIF.

  CLEAR : l_check .

  LOOP AT it_ausp.
    CONDENSE it_ausp-atwrt.
    CASE  it_ausp-atnam.
      WHEN 'P_MODEL'.
        MOVE : it_ausp-atwrt TO  l_model.
      WHEN 'P_BODY_SERIAL'.
        MOVE : it_ausp-atwrt TO l_objek.
      WHEN 'P_WORK_ORDER'.
        MOVE : it_ausp-atwrt TO st_2204-order.
      WHEN 'P_EXT_COLOR'.
        MOVE : it_ausp-atwrt TO st_2204-exclr.
      WHEN 'P_INT_COLOR'.
        MOVE : it_ausp-atwrt TO st_2204-inclr.
      WHEN 'P_MI'.
        MOVE : it_ausp-atwrt TO st_2204-mi.
      WHEN 'P_TM'.
        MOVE : it_ausp-atwrt TO st_2204-tm.
      WHEN 'P_OCN'.
        MOVE : it_ausp-atwrt TO st_2204-ocn.
      WHEN 'P_VERSION'.
        MOVE : it_ausp-atwrt TO st_2204-ver.
      WHEN 'P_ENGINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-engine.
      WHEN 'P_STATUS'.
        MOVE : it_ausp-atwrt TO st_2204-locl.
      WHEN 'P_TM'.
        MOVE : it_ausp-atwrt TO st_2204-tm.
      WHEN 'P_BODY_LINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-sbody.
      WHEN 'P_PAINT_LINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-spaint.
      WHEN 'P_TRIM_LINE_NO'.
        MOVE : it_ausp-atwrt TO st_2204-strim.
      WHEN 'P_SCRAP_ACCT'.
        MOVE : it_ausp-atwrt TO st_2204-scrap.
      WHEN 'P_USAGE_TYPE'.
        MOVE : it_ausp-atwrt TO st_2204-scrap.
      WHEN 'P_USAGE_TEXT'.
        MOVE : it_ausp-atwrt TO st_2204-text.
      WHEN 'P_USAGE_DEPT'.
        MOVE : it_ausp-atwrt TO st_2204-u_dept.

      WHEN 'P_SEQUENCE_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-seq .
      WHEN 'P_RP01_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-bdin.
      WHEN 'P_RP02_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-paint_in.
      WHEN 'P_RP04_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-paint_out.
      WHEN 'P_RP07_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-trim_in.
      WHEN 'P_RP18_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-sign_off.
      WHEN 'P_RP19_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-c_gate .
*      WHEN 'P_RP21_SHOP_DATE'.
      WHEN 'P_RP23_SHOP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204-vpc    .
      WHEN 'P_SCRAP_DATE'.
        MOVE : it_ausp-atwrt TO st_2204_input-date.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " DATA_NEXT_DATE_2204

*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGE_ENTRY_2204
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_change_entry_2204.
  DATA: lt_val         LIKE zspp_vin_value OCCURS 0    WITH HEADER LINE,
        l_text(40)     TYPE c   ,
        l_objek        LIKE equi-equnr,
        l_worder       LIKE mara-matnr,
        lw_wosum       LIKE ztpp_wosum,
        lt_before      LIKE zspp_vin_value OCCURS 0    WITH HEADER LINE,
        lt_after       LIKE zspp_vin_value OCCURS 0    WITH HEADER LINE,
        l_return       TYPE zzret.

  CLEAR : lt_val[], lt_val,  l_objek.
  CONCATENATE wa_model  st_2204_input-body INTO  l_objek.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_objek
      mode         = 'R'
    TABLES
      val_table    = lt_before
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  CASE st_2204-ctype .
    WHEN 'S' OR 'D'.
*      lt_val-atnam = 'P_SCRAP_ACCT'.
*      lt_val-atwrt = st_2204-scrap .
*      APPEND lt_val .  CLEAR lt_val.
      lt_val-atnam = 'P_SCRAP_DATE'.
      lt_val-atwrt = sy-datum      .
      APPEND lt_val .  CLEAR lt_val.
      READ TABLE lt_before WITH KEY atnam = 'P_RP_STATUS'.
      lt_val-atnam = 'P_SCRAP_RP'  .
      lt_val-atwrt = lt_before-atwrt.
      APPEND lt_val .  CLEAR lt_val.
*     Lt_val-atnam = 'P_SCRAP_NO'  .
*     Lt_val-atwrt =               .
*     APPEND Lt_val .  CLEAR Lt_val.
      lt_val-atnam = 'P_SCRAP_APP_DOC'.
      lt_val-atwrt = st_2204-scrap .
      APPEND lt_val .  CLEAR lt_val.
*    WHEN 'D'.

    WHEN 'T'.
  ENDCASE.

  lt_val-atnam = 'P_USAGE_TYPE'.
  lt_val-atwrt = st_2204-usage.
  APPEND lt_val .  CLEAR lt_val.

  lt_val-atnam = 'P_USAGE_TEXT'.
  lt_val-atwrt = st_2204-text.
  APPEND lt_val .  CLEAR lt_val.

  lt_val-atnam = 'P_USAGE_CAR'.
  lt_val-atwrt = st_2204-ctype.
  APPEND lt_val .  CLEAR lt_val.

  lt_val-atnam = 'P_USAGE_DEPT'.
  lt_val-atwrt = st_2204-u_dept.
  APPEND lt_val .  CLEAR lt_val.

  " Check the Original Information....
  READ TABLE lt_before WITH KEY atnam = 'P_USAGE_CAR'.
  " The Value's Meaning: 'T' - Test Car    ,
  "                      'S' - Scrap Car   ,
  "                      'D' - Disposal Car,
  "                      ' ' or 'P' - Normal Car  .
  IF lt_before-atwrt = 'T' OR sy-subrc NE 0 OR
     lt_before-atwrt IS INITIAL OR lt_before-atwrt = 'P'.
    READ TABLE lt_before WITH KEY atnam = 'P_RP_STATUS'.
    IF st_2204-ctype = 'D' AND lt_before-atwrt < '18'  .
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Warning !!!'
          txt1  = text-600
          txt2  = '.'
          txt3  = '     Please exit to MAIN menu to restart'
*         TXT3  = '     .'
          txt4  = '     Otherwise SO data may contaIn error'
*         TXT3  = ' '
        .

*      MESSAGE w001 WITH text-600 .
*      w_flag_scrap = 'X'.
      EXIT.
    ELSEIF st_2204-ctype = 'S' AND lt_before-atwrt >= '18'  .
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Warning !!! '
          txt1  = text-700
          txt2  = '.'
          txt3  = '     Please exit to MAIN menu to restart'
*         TXT3  = '     .'
          txt4  = '     Otherwise SO data may contaIn error'
*         TXT4  = ' '
        .
*      MESSAGE w001 WITH text-700 .
*      w_flag_scrap = 'X'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object       = l_objek
        mode         = 'W'
      TABLES
        val_table    = lt_val
      EXCEPTIONS
        no_data      = 1
        error_mode   = 2
        error_object = 3
        error_value  = 4
        OTHERS       = 5.

    IF st_2204-ctype = 'S'
      OR st_2204-ctype =  'D'.
      CALL FUNCTION 'Z_FPP_VEHICLE_SCRAP'
        EXPORTING
          equnr        = l_objek
          usage_type   = st_2204-ctype
        IMPORTING
          return       = l_return
        TABLES
          before_vin   = lt_before
          after_vin    = lt_val
        EXCEPTIONS
          master_error = 1
          status_error = 2.
      IF sy-subrc NE 0.
        MESSAGE s000 WITH 'VM MASTER OR STATUS ERROR'.
        EXIT.
      ENDIF.

      IF l_return = 'S'.
        COMMIT WORK.
        MESSAGE s002  WITH 'VM and Scrapped table update done'.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
      " Reduce the Sequence Quantity...(WO_SUM, WOHD, WOCL)
      CLEAR: lt_val, lt_val[], ztpp_wosum.
      READ TABLE lt_before WITH KEY atnam = 'P_WORK_ORDER'.
      l_worder = lt_before-atwrt.
      READ TABLE lt_before WITH KEY atnam = 'P_EXT_COLOR' .
      CONCATENATE l_worder lt_before-atwrt INTO l_worder  .
      READ TABLE lt_before WITH KEY atnam = 'P_INT_COLOR' .
      CONCATENATE l_worder lt_before-atwrt INTO l_worder  .
      SELECT SINGLE *
        FROM ztpp_wosum
       WHERE wo_ser = l_worder(9)
         AND nation = l_worder+9(3)
         AND dealer = l_worder+12(2)
         AND extc   = l_worder+14(2)
         AND intc   = l_worder+16(2).
      ztpp_wosum-seqqty = ztpp_wosum-seqqty - 1.
      UPDATE ztpp_wosum   SET seqqty = ztpp_wosum-seqqty
                        WHERE wo_ser = ztpp_wosum-wo_ser
                          AND nation = ztpp_wosum-nation
                          AND dealer = ztpp_wosum-dealer
                          AND extc   = ztpp_wosum-extc
                          AND intc   = ztpp_wosum-intc   .

      lt_val-atnam = 'P_SEQ_QTY'.
      lt_val-atwrt = ztpp_wosum-seqqty.    APPEND lt_val.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object       = l_worder
          mode         = 'W'
          ctype        = '001'
        TABLES
          val_table    = lt_val
        EXCEPTIONS
          no_data      = 1
          error_mode   = 2
          error_object = 3
          error_value  = 4
          OTHERS       = 5.

      CLEAR: lt_val, lt_val[].
      l_worder = l_worder(14).
      SELECT SUM( seqqty ) INTO ztpp_wosum-seqqty
        FROM ztpp_wosum
       WHERE wo_ser = ztpp_wosum-wo_ser
         AND nation = ztpp_wosum-nation
         AND dealer = ztpp_wosum-dealer .

      lt_val-atnam = 'P_SEQ_QTY'.
      lt_val-atwrt = ztpp_wosum-seqqty.  APPEND lt_val.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object       = l_worder
          mode         = 'W'
          ctype        = '001'
        TABLES
          val_table    = lt_val
        EXCEPTIONS
          no_data      = 1
          error_mode   = 2
          error_object = 3
          error_value  = 4
          OTHERS       = 5.
    ENDIF.

* requested by MY Hur changed by chris
* For scraped and disposal car, adjust the
* salse order qty
    DATA: l_sorder     LIKE ztpp_wosum-sales,
             l_mat        LIKE mara-matnr,
             l_flag,
             l_ext_color(03),
             l_int_color(03),
             l_dist(2).
** Changed by Furong on 02/07/08
    READ TABLE lt_before WITH KEY atnam = 'P_DIST_CODE'.
    l_dist = lt_before-atwrt.
    IF l_dist+0(1) = 'X' AND st_2204-ctype = 'D'.
    ELSE.
** End of change
      READ TABLE lt_before WITH KEY atnam = 'P_SALES_ORDER'        .
      l_sorder = lt_before-atwrt.
      READ TABLE lt_before WITH KEY atnam = 'P_WORK_ORDER'         .
      l_mat = lt_before-atwrt .
      READ TABLE lt_before WITH KEY atnam = 'P_EXT_COLOR'         .
      l_ext_color = lt_before-atwrt .
      READ TABLE lt_before WITH KEY atnam = 'P_INT_COLOR'         .
      l_int_color = lt_before-atwrt .
      CONCATENATE l_mat l_ext_color l_int_color INTO l_mat.

      PERFORM call_function_for_sales_order USING l_sorder
                                                  l_mat
                                                  '-'
                                              CHANGING l_flag.

      IF l_flag = 'E'.
        MESSAGE i000 WITH 'Sales order qty changed failed'.
      ELSE.
        MESSAGE i001 WITH 'Sales order qty changed successful.' 'HAW*'.
      ENDIF.
** CHANGED BY FURONG ON 02/07/08
    ENDIF.
** END OF CHANGE on 02/07/09
* end 0f add on 06/07/2005

  ELSE.
    CASE lt_before-atwrt.
      WHEN 'S'.
        PERFORM change_scraped_car  USING l_objek.
        MESSAGE s003 WITH 'Change successful'.
      WHEN 'D'.
        PERFORM change_scraped_car  USING l_objek.
        MESSAGE s003 WITH 'Change successful'.
    ENDCASE.
*    MESSAGE s003 WITH text-200 '::'  l_text.
  ENDIF.
ENDFORM.                    " DATA_CHANGE_ENTRY_2204

*&---------------------------------------------------------------------*
*&      Form  next_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_next_date  text
*----------------------------------------------------------------------*
FORM next_date USING    p_next_date.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = st_2204_input-date
      days      = '01'
      months    = '00'
      signum    = '+'
      years     = '00'
    IMPORTING
      calc_date = p_next_date.

ENDFORM.                    " next_date
*&---------------------------------------------------------------------*
*&      Form  data_select_2205
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_2205.
  DATA : l_mi     LIKE ausp-atinn,
         l_ocnn   LIKE ausp-atinn,
         l_vers   LIKE ausp-atinn,
         l_model  LIKE ausp-atinn,
         l_utype  LIKE ausp-atinn,
         l_worder LIKE ausp-atinn,
         l_ext    LIKE ausp-atinn,
         l_int    LIKE ausp-atinn,
         l_utext  LIKE ausp-atinn,
         l_rp     LIKE ausp-atinn,
         l_18date LIKE ausp-atinn,
         l_ucar   LIKE ausp-atinn,
         l_vdate  LIKE ausp-atinn,
         l_sdate  LIKE ausp-atinn,
         l_beg    LIKE ausp-atflv,
         l_end    LIKE ausp-atflv,
         l_temp(8) TYPE n.
  CLEAR : it_2205[], it_2205.
  DATA: l_fdate_vm LIKE ausp-atwrt,
        l_tdate_vm LIKE ausp-atwrt.

  CONCATENATE st_2205_input-bdate '000000' INTO l_fdate_vm .
  CONCATENATE st_2205_input-edate '999999' INTO l_tdate_vm .
  l_temp = st_2205_input-bdate.
  l_beg = l_temp.
  l_temp = st_2205_input-edate.
  l_end = l_temp.

  PERFORM read_atinn   USING 'P_SCRAP_DATE'     l_sdate.
  PERFORM read_atinn   USING 'P_VM_DATE'        l_vdate.
  PERFORM read_atinn   USING 'P_USAGE_CAR'      l_ucar .
  PERFORM read_atinn   USING 'P_USAGE_TYPE'     l_utype.
  PERFORM read_atinn   USING 'P_MODEL'          l_model.
  PERFORM read_atinn   USING 'P_WORK_ORDER'     l_worder.
  PERFORM read_atinn   USING 'P_EXT_COLOR'      l_ext   .
  PERFORM read_atinn   USING 'P_INT_COLOR'      l_int   .
  PERFORM read_atinn   USING 'P_MI'             l_mi    .
  PERFORM read_atinn   USING 'P_OCN'            l_ocnn  .
  PERFORM read_atinn   USING 'P_VERSION'        l_vers  .
  PERFORM read_atinn   USING 'P_USAGE_TEXT'     l_utext .
  PERFORM read_atinn   USING 'P_RP_STATUS'      l_rp    .
  PERFORM read_atinn   USING 'P_RP18_SHOP_DATE' l_18date.
* scrapped and disposed cars
  SELECT *  INTO CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( SELECT objek FROM ausp
                     WHERE atinn = l_sdate
                       AND klart = '002'
                       AND atflv >= l_beg
                       AND atflv <= l_end )
     AND atinn = l_ucar
     AND klart = '002'
     AND atwrt IN ('S' , 'D').
* Test cars
  SELECT *  APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
   FROM ausp
  WHERE objek IN ( SELECT objek FROM ausp
                    WHERE atinn = l_18date
                      AND klart = '002'
                      AND atflv >= l_beg
                      AND atflv <= l_end )
    AND atinn = l_ucar
    AND klart = '002'
    AND atwrt EQ 'T'.


  LOOP AT it_ausp.
    CLEAR: it_2205.
    it_2205-objek = it_ausp-objek.
    it_2205-car   = it_ausp-atwrt.
    it_2205-model = it_ausp-objek(3).
    PERFORM get_value_atwrt  USING l_utype  it_2205-usage .
    PERFORM get_value_atwrt  USING l_worder it_2205-work  .
    PERFORM get_value_atwrt  USING l_ext    it_2205-ext   .
    PERFORM get_value_atwrt  USING l_int    it_2205-int   .
    PERFORM get_value_atwrt  USING l_mi     it_2205-mi    .
    PERFORM get_value_atwrt  USING l_ocnn   it_2205-ocn   .
    PERFORM get_value_atwrt  USING l_vers   it_2205-ver   .
    PERFORM get_value_atflv  USING l_sdate  it_2205-scrap .
    PERFORM get_value_atflv  USING l_18date it_2205-sign  .
    PERFORM get_value_atwrt  USING l_rp     it_2205-stat  .
    PERFORM get_value_atwrt  USING l_utext  it_2205-text  .
    APPEND it_2205.
  ENDLOOP.
  IF NOT wa_model IS INITIAL.
    DELETE it_2205 WHERE model NE wa_model.
  ENDIF.
  DESCRIBE TABLE it_2205 LINES wa_lines.
  IF wa_lines = 0.
    MESSAGE w001 WITH text-001.
  ENDIF.
ENDFORM.                    " data_select_2205

*&---------------------------------------------------------------------*
*&      Form  excel_down_2205
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_down_2205.
*Initially no code was written to this.
*HASEEB MODIFIED TO EXPORT THE DATA INTO EXCEL FILE
* on request of LAnce, on 01-17-2005.
  DATA : BEGIN OF it_2205_h OCCURS  0,
           objek(20) TYPE c,
           car(10) TYPE c,
           usage(20) TYPE c,
           text(50) TYPE c,
           work(20) TYPE c,
           mi(10) TYPE c,
           ocn(5) TYPE c,
           ver(5) TYPE c,
           ext(5) TYPE c,
           int(5) TYPE c,
           sign(10) TYPE c,
           stat(20) TYPE c,
           scrap(20) TYPE c,

         END OF it_2205_h.
*
  DATA: l_count TYPE i.
  DATA: filename(30) TYPE c.

  CONCATENATE sy-datum '_SCRAP_LIST_HIS.xls' INTO filename.

  MOVE 'BODY-NO' TO it_2205_h-objek.
  MOVE 'USAGE' TO it_2205_h-car.
  MOVE 'CODE' TO it_2205_h-usage.
  MOVE 'DOCUMENT TEXT' TO it_2205_h-text.
  MOVE 'WORKORDER      ' TO it_2205_h-work.
  MOVE 'MI' TO it_2205_h-mi.
  MOVE 'OCN' TO it_2205_h-ocn.
  MOVE 'VER' TO it_2205_h-ver.
  MOVE 'EXT' TO it_2205_h-ext.
  MOVE 'INT' TO it_2205_h-int.
  MOVE 'S/OFF DATE' TO it_2205_h-sign.
  MOVE 'STATUS' TO it_2205_h-stat.
  MOVE 'SCRAP DATE      ' TO it_2205_h-scrap.
  APPEND it_2205_h.

  LOOP AT it_2205.
    MOVE-CORRESPONDING it_2205 TO it_2205_h.
    APPEND it_2205_h.

  ENDLOOP.
  DESCRIBE TABLE it_2205_h LINES l_count.
  IF l_count = 0.
    MESSAGE i000(zpp) WITH 'Empty data'.

  ENDIF.



  CHECK l_count > 0.
  CALL FUNCTION 'WS_EXCEL'
    EXPORTING
      filename = filename
*     SYNCHRON = ' '
    TABLES
      data     = it_2205_h.



ENDFORM.                    " excel_down_2205

*&---------------------------------------------------------------------*
*&      Form  LIST_2205
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_2205.
  ok_code = '2205'.
ENDFORM.                                                    " LIST_2205

*&---------------------------------------------------------------------*
*&      Form  USE_PICK_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM use_pick_list.
  DATA : l_line      LIKE sy-index,
         l_field(40),
         l_read_line LIKE sy-index.

  FIELD-SYMBOLS : <fn> TYPE any.

  CLEAR g_body_no.

  GET CURSOR LINE    l_line.
  GET CURSOR FIELD   l_field.

  l_read_line = l_line + tc_2205-top_line - 1.
  READ TABLE  it_2205 INDEX l_read_line.

  MOVE: it_2205-objek(3)    TO wa_model          ,
        it_2205-objek+3(10) TO st_2204_input-body.

  PERFORM data_select_2204 .

  ok_code = '2204'.
ENDFORM.                    " USE_PICK_LIST

*&---------------------------------------------------------------------*
*&      Form  data_select_2206
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_2206.
  DATA : l_atinn  LIKE ausp-atinn.

  DATA : BEGIN OF st_ox,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atzhl LIKE ausp-atzhl,
         mafid LIKE ausp-mafid,
         klart LIKE ausp-klart,
         adzhl LIKE ausp-adzhl,
         atwrt LIKE ausp-atwrt,
         END OF st_ox.

  DATA : l_body1 LIKE it_ausp-atwrt,
         l_body2 LIKE it_ausp-atwrt,
         l_model LIKE it_ausp-atwrt,
         l_objek LIKE it_ausp-atwrt,
         l_name(2) ,
         l_atnam LIKE cabn-atnam,
         l_check.

  CLEAR r_atnam[].
* MODEL
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MODEL'.
  APPEND r_atnam. CLEAR r_atnam.

* USAGE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_USAGE_CAR'.
  APPEND r_atnam. CLEAR r_atnam.

* CODE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_USAGE_TYPE'.
  APPEND r_atnam. CLEAR r_atnam.

* STATUS
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP_STATUS'.
  APPEND r_atnam. CLEAR r_atnam.

  CLEAR : l_body1 , l_body2, l_model, l_objek, st_ox.
  CLEAR : it_2206[], it_2206.

  DATA: l_atflv_st TYPE ausp-atflv,
        l_atflv_en TYPE ausp-atflv,
        l_num(08)  TYPE n         .
  l_atflv_st = l_num = st_2206_input-bdate.
  l_atflv_en = l_num = st_2206_input-edate.

  SELECT au~objek au~atinn au~atzhl au~mafid au~klart au~adzhl au~atwrt
         INTO st_ox
         FROM ausp AS au
              INNER JOIN cabn AS ca ON au~atinn = ca~atinn
         WHERE au~klart = '002'          AND
               ca~atnam = 'P_SCRAP_DATE' AND
               au~atflv    BETWEEN l_atflv_st AND
                                   l_atflv_en AND
               au~lkenz = ' '              .

    SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
        INTO CORRESPONDING FIELDS OF TABLE it_ausp
     FROM zvpp_cha
     WHERE objek = st_ox-objek
       AND klart = '002'
       AND atnam IN r_atnam
       AND lkenz = ' ' .

    CLEAR : l_check .

    MOVE : st_ox-objek TO it_2205-objek.

    LOOP AT it_ausp.
      CONDENSE it_ausp-atwrt.
      CASE  it_ausp-atnam.
        WHEN 'P_MODEL'.
          MOVE : it_ausp-atwrt TO  it_2206-model.
        WHEN 'P_USAGE_CAR' .
          it_2206-car = it_ausp-atwrt .
        WHEN 'P_USAGE_TYPE'.
          CASE  it_ausp-atwrt .
            WHEN 'SP'.
              it_2206-total = 1.
              it_2206-spec  = 1.
            WHEN 'MA' .
              it_2206-total = 1.
              it_2206-matl  = 1.
            WHEN 'WO'.
              it_2206-total = 1.
              it_2206-work  = 1.
            WHEN 'DR'.
              it_2206-total = 1.
              it_2206-drive = 1.
            WHEN 'CA'.
              it_2206-total = 1.
              it_2206-cause = 1.
            WHEN 'EQ'.
              it_2206-total = 1.
              it_2206-equip = 1.
            WHEN 'RE'.
              it_2206-total = 1.
              it_2206-return = 1.
            WHEN 'TR'.
              it_2206-total = 1.
              it_2206-trans = 1.
            WHEN 'TS'.
              it_2206-total = 1.
              it_2206-test  = 1.
            WHEN 'KD'.
              it_2206-total = 1.
              it_2206-skd   = 1.
            WHEN 'YD'.
              it_2206-total = 1.
              it_2206-usage = 1.
            WHEN 'AS'.
              it_2206-total = 1.
              it_2206-as    = 1.
            WHEN 'OT'.
              it_2206-total = 1.
              it_2206-other = 1.
          ENDCASE.
        WHEN 'P_RP_STATUS'.
          CASE st_2206_input-shop.
            WHEN space.
              l_check = 'X'.
            WHEN 'B' .  "BODY
              IF it_ausp-atwrt = '01'               .       " B01
                l_check = 'X'.
              ENDIF.
            WHEN 'P'.  "PAINT
              IF it_ausp-atwrt BETWEEN '02' AND '06'.       " P02~ P39
                l_check = 'X'.
              ENDIF.
            WHEN 'T'.  "Trim
              IF it_ausp-atwrt BETWEEN '07' AND '17'.       " T01~ T25
                l_check = 'X'.
              ENDIF.
            WHEN 'S' OR 'V'.  "VPC
              IF it_ausp-atwrt BETWEEN '18' AND '28'.       " T27~
                l_check = 'X'.
              ENDIF.
          ENDCASE.
        WHEN 'P_USAGE_CAR'.
          IF st_2205_input-usage <> ' '.
            IF it_ausp-atwrt = st_2205_input-usage.
              l_check = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    IF l_check = 'X'.
      COLLECT it_2206. CLEAR it_2206.
*      IF it_2206-car = 'T' .
*        COLLECT it_2206. CLEAR it_2206.
*      ELSEIF it_2206-car = 'S'.
*        it_2206-total = 1.  it_2206-test = 1.
*        COLLECT it_2206. CLEAR it_2206.
*      ELSE.
*        it_2206-car = 'X' .
*        it_2206-total = 1.  it_2206-usage = 1.
*        COLLECT it_2206. CLEAR it_2206.
*      ENDIF.
    ELSE.
      CLEAR it_2206.
    ENDIF.
  ENDSELECT.

  LOOP AT it_2206 .
    SELECT SINGLE atinn INTO l_atinn
      FROM cabn
     WHERE atnam = 'P_MODEL'.

    SELECT SINGLE t~atwtb INTO it_2206-name
      FROM cawn AS n INNER JOIN cawnt AS t
        ON n~atinn = t~atinn
       AND n~atzhl = t~atzhl
     WHERE n~atinn = l_atinn
       AND t~spras = sy-langu
       AND n~atwrt = it_2206-model .
    IF sy-subrc EQ 0.
      MODIFY it_2206 .
    ENDIF.
  ENDLOOP.
ENDFORM.                    " data_select_2206

*&---------------------------------------------------------------------*
*&      Form  data_select_0109
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_0109.
  DATA : BEGIN OF it_wo OCCURS 0,
          wo_ser  LIKE ztpp_wosum-wo_ser,    " WORK ORDER
          nation  LIKE ztpp_wosum-nation,    " Nation
          dealer  LIKE ztpp_wosum-dealer,    " Dealer
          modqty  LIKE ztpp_wosum-modqty,    " Modify quntity
          objek   LIKE ausp-objek,
         END OF it_wo.

  DATA : b_wo        LIKE ztpp_wosum-wo_ser   ,
         e_wo        LIKE ztpp_wosum-wo_ser   ,
         objek       LIKE ausp-objek  ,
         l_woups     like ztsd_sodata-woups,
         l_pack(4)   TYPE c,
         l_nat(3)    TYPE c,
         l_dat       TYPE d,
         l_vals(8)   TYPE n,
         l_del(2)    TYPE c,
         check       TYPE c.

  DATA: l_pos(1) TYPE n,
        l_diff(1) TYPE n,
         l_dealer_2(2),
         l_dealer_1(1).

  RANGES : r_nat FOR  ztpp_wosum-nation,    " Nation
           r_del FOR  ztpp_wosum-dealer.

  IF st_0109_input-nation <> ''.
    CLEAR : r_nat , r_nat[].
    r_nat-option = 'EQ'.
    r_nat-sign  = 'I'.
    r_nat-low    = st_0109_input-nation(3).
    APPEND r_nat. CLEAR r_nat.
  ENDIF.

  CLEAR : it_0109, it_0109[], it_ausp, it_ausp[], st_0109_input-qty.

  SELECT wo_ser nation  dealer SUM( modqty )
        INTO TABLE it_wo
        FROM  ztpp_wosum
        WHERE  nation IN r_nat
        GROUP BY nation wo_ser dealer .

* objeck number concatenate.
  IF sy-subrc NE 0.
    MESSAGE s001  WITH  text-100.
  ENDIF.

  LOOP AT it_wo.
    CONCATENATE it_wo-wo_ser it_wo-nation it_wo-dealer INTO it_wo-objek.
    MODIFY it_wo.
  ENDLOOP.

  PERFORM range_insert_0109.  " range value
  PERFORM range_insert_0109_219. " 219 range value.

  LOOP AT it_wo.
    CLEAR : l_nat, l_del, check.

    SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
        INTO CORRESPONDING FIELDS OF TABLE it_ausp
     FROM zvpp_cha
     WHERE objek = it_wo-objek
       AND klart = '001'
       AND atnam IN r_atnam
       AND lkenz = ' ' .

    LOOP AT it_ausp.
      CASE  it_ausp-atnam.
        WHEN 'P_MODEL' .
          IF it_ausp-atwrt  NE wa_model        .
            check = 'X' .
          ENDIF.
        WHEN 'P_219_19'.
          " Check the Input Option...
          MOVE : it_ausp-atwrt TO it_0109-219_19 .
*          check = 'X'.
*          case st_0109_input-use .
*            when 'D'.
*              if it_0109-219_19 = 'D'.  clear: check.  endif.
*            when 'E'.
*              if it_0109-219_19 = 'E'.  clear: check.  endif.
*            when space.
*              clear: check.
*          endcase.
          CHECK st_0109_input-use NE space     .

** Changed by Furong on 04/18/08
*          IF ST_0109_INPUT-USE NE IT_0109-219_19 .
*            CHECK = 'X' .
*          ENDIF.
          IF st_0109_input-use = 'E'.
          ELSE.
            IF st_0109_input-use NE it_0109-219_19 .
              check = 'X' .
            ENDIF.
          ENDIF.
        WHEN 'P_NATION'.
          IF st_0109_input-use = 'E' AND it_ausp-atwrt+0(3) = 'B28'.
            check = 'X' .
          ENDIF.
** END OF CHANGE

        WHEN 'P_WO_SER'.
          IF it_ausp-atwrt+1(4) NE st_0109_input-monty.
            check = 'X'.
          ENDIF.
        WHEN 'P_OCN'.
          MOVE : it_ausp-atwrt TO it_0109-ocn.

        WHEN 'P_VERSION'.
          MOVE : it_ausp-atwrt TO it_0109-ver.

        WHEN 'P_TRIM_PLANT_NO'.
          MOVE : it_ausp-atwrt TO it_0109-plant.

        WHEN 'P_WO_CREATE_DATE'.
          l_dat = l_vals = it_ausp-atflv .
          WRITE l_dat TO it_0109-g_date  MM/DD/YYYY .

        WHEN 'P_MOD_DATE'.
          l_dat = l_vals = it_ausp-atflv .
          WRITE l_dat TO it_0109-a_date  MM/DD/YYYY .

        WHEN 'P_PROD_FLAG'.        " 'P_PERF_YN'.
          " Check the Input Option...
          MOVE : it_ausp-atwrt TO it_0109-flag  .
          CHECK st_0109_input-full NE space     .
          IF st_0109_input-full NE it_0109-flag .
            check = 'X' .
          ENDIF.

        WHEN 'P_MI'.
          MOVE : it_ausp-atwrt TO it_0109-mi.

        WHEN 'P_UPDATE_ALC_DATE1'.
          l_dat = l_vals = it_ausp-atflv .
          WRITE l_dat TO it_0109-alcd1  MM/DD/YYYY .
          IF NOT it_ausp-atflv IS INITIAL.
            " Check the 'P_UPDATE_ALC_DATE2'.
            SELECT SINGLE atflv INTO it_ausp-atflv
              FROM cabn AS b INNER JOIN ausp AS a
                ON b~atinn = a~atinn
             WHERE b~atnam = 'PUPDATE_ALC_DATE2'
               AND a~objek = it_ausp-objek
               AND a~klart = '001' .
            IF sy-subrc = 0.
              l_dat = l_vals = it_ausp-atflv .
              WRITE l_dat TO it_0109-alcd1  MM/DD/YYYY .
            ENDIF.
          ENDIF.
** Furong on 01/05/12 for adding VIN verify
** between HMMA and HMC

        WHEN 'P_VIN_SPEC'.
          MOVE : it_ausp-atwrt TO it_0109-vin_hmma.

** End

      ENDCASE.

      CASE it_ausp-atnam+6(3).
        WHEN ''.

        WHEN st_0109_input-219_1.
          IF st_0109_input-219_1_v <> '' AND
             st_0109_input-219_1_v <> it_ausp-atwrt.
            check = 'X'.
          ENDIF.

          MOVE : it_ausp-atwrt TO it_0109-219_1.

        WHEN st_0109_input-219_2.
          IF st_0109_input-219_2_v <> '' AND
             st_0109_input-219_2_v <> it_ausp-atwrt.
            check = 'X'.
          ENDIF.

          MOVE : it_ausp-atwrt TO it_0109-219_2.

        WHEN st_0109_input-219_3.
          IF st_0109_input-219_3_v <> '' AND
             st_0109_input-219_3_v <> it_ausp-atwrt.
            check = 'X'.
          ENDIF.

          MOVE : it_ausp-atwrt TO it_0109-219_3.

        WHEN st_0109_input-219_4.
          IF st_0109_input-219_4_v <> '' AND
             st_0109_input-219_4_v <> it_ausp-atwrt.
            check = 'X'.
          ENDIF.

          MOVE : it_ausp-atwrt TO it_0109-219_4.

        WHEN st_0109_input-219_5.
          IF st_0109_input-219_5_v <> '' AND
             st_0109_input-219_5_v <> it_ausp-atwrt.
            check = 'X'.
          ENDIF.

          MOVE : it_ausp-atwrt TO it_0109-219_5.

        WHEN st_0109_input-219_6.
          IF st_0109_input-219_6_v <> '' AND
             st_0109_input-219_6_v <> it_ausp-atwrt.
            check = 'X'.
          ENDIF.

          MOVE : it_ausp-atwrt TO it_0109-219_6.

        WHEN st_0109_input-219_7.
          IF st_0109_input-219_7_v <> '' AND
             st_0109_input-219_7_v <> it_ausp-atwrt.
            check = 'X'.
          ENDIF.

          MOVE : it_ausp-atwrt TO it_0109-219_7.

        WHEN st_0109_input-219_8.
          IF st_0109_input-219_8_v <> '' AND
             st_0109_input-219_8_v <> it_ausp-atwrt.
            check = 'X'.
          ENDIF.

          MOVE : it_ausp-atwrt TO it_0109-219_8.

        WHEN st_0109_input-219_9.
          IF st_0109_input-219_9_v <> '' AND
             st_0109_input-219_9_v <> it_ausp-atwrt.
            check = 'X'.
          ENDIF.

          MOVE : it_ausp-atwrt TO it_0109-219_9.

        WHEN st_0109_input-219_10.
          IF st_0109_input-219_10_v <> '' AND
             st_0109_input-219_10_v <> it_ausp-atwrt.
            check = 'X'.
          ENDIF.

          MOVE : it_ausp-atwrt TO it_0109-219_10.

      ENDCASE.
    ENDLOOP.

    MOVE : it_wo-wo_ser TO it_0109-order,
           it_wo-modqty TO it_0109-modqty.

    CONCATENATE it_wo-nation it_wo-dealer INTO it_0109-nation.


    IF check = ''.

** Furong on 01/05/12 for adding VIN verify
** between HMMA and HMC
      IF it_0109-order+1(4) = st_0109_input-monty.
        clear : l_woups, it_0109-vin_hmc.
        l_dealer_2 = it_wo-dealer.

        CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
          EXPORTING
            old_dealer = l_dealer_2
          IMPORTING
            new_dealer = l_dealer_1.

        SELECT SINGLE vin_spec
          INTO it_0109-vin_hmc
          FROM ztpp_vin_if
          WHERE wo_serial = it_wo-wo_ser
            AND wo_nation = it_wo-nation
            AND wo_dealer = l_dealer_1.
        if it_0109-vin_hmc is INITIAL.

          select WOUPS into l_woups
          from ZTSD_SODATA
            up to 1 rows
          where WO_SER =  it_wo-wo_ser
            and NATION =  it_wo-nation
            and DEALER =  l_dealer_2
            and WOUPS  <> space
          order by WO_SER NATION DEALER ZSDAT DESCENDING
                                        ZSTIM DESCENDING.
          endselect.
          if l_woups is not INITIAL.
            SELECT SINGLE vin_spec
              INTO it_0109-vin_hmc
              FROM ztpp_vin_if
              WHERE wo_serial = l_woups
                AND wo_nation = it_wo-nation
                AND wo_dealer = l_dealer_1.

          endif.
        endif.


        l_pos = 0.
        DO 8 TIMES.
          IF it_0109-vin_hmma+l_pos(1) = it_0109-vin_hmc+l_pos(1).
          ELSE.
            l_diff = l_pos + 1.
            CONCATENATE it_0109-vin_dif l_diff INTO it_0109-vin_dif
            SEPARATED BY ' '.
          ENDIF.
          l_pos = l_pos + 1.
        ENDDO.
** End

        st_0109_input-qty = it_0109-modqty + st_0109_input-qty.
        APPEND it_0109. CLEAR it_0109.
      ENDIF.
    ENDIF.
*        APPEND it_0109. CLEAR it_0109.
  ENDLOOP.
ENDFORM.                    " data_select_0109
*&---------------------------------------------------------------------*
*&      Form  excel_down_0109
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_down_0109.

ENDFORM.                    " excel_down_0109
*&---------------------------------------------------------------------*
*&      Form  range_insert_0109
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM range_insert_0109.
  CLEAR r_atnam[].

* Model
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MODEL' .
  APPEND r_atnam. CLEAR r_atnam.

* NATION
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_NATION'.
  APPEND r_atnam. CLEAR r_atnam.

* WORK ORDER SERIAL NUMBER
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_WO_SER'.
  APPEND r_atnam. CLEAR r_atnam.

* MOD_DATE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MOD_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* GEN_DATE
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_GEN_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* Work Order Creating Date
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_WO_CREATE_DATE'.
  APPEND r_atnam. CLEAR r_atnam.

* FLAG
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_PERF_YN' .
  APPEND r_atnam. CLEAR r_atnam.

* PLANT
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_TRIM_PLANT_NO'.
  APPEND r_atnam. CLEAR r_atnam.

* MI
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MI'.
  APPEND r_atnam. CLEAR r_atnam.

* OCN
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_OCN'.
  APPEND r_atnam. CLEAR r_atnam.

* P_VERSION
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_VERSION'.
  APPEND r_atnam. CLEAR r_atnam.

* Order Status
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_PROD_FLAG'.
  APPEND r_atnam. CLEAR r_atnam.

* P_219_19 (Check Usage).
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_219_19'.
  APPEND r_atnam. CLEAR r_atnam.

* P_UPDATE_ALC_DATE1
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_UPDATE_ALC_DATE1'.
  APPEND r_atnam. CLEAR r_atnam.

* P_UPDATE_ALC_DATE2
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_UPDATE_ALC_DATE2'.
  APPEND r_atnam. CLEAR r_atnam.


* P_VIN_SPEC
  r_atnam-sign  = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_VIN_SPEC'.
  APPEND r_atnam. CLEAR r_atnam.



ENDFORM.                    " range_insert_0109
*&---------------------------------------------------------------------*
*&      Form  range_insert_0109_219
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM range_insert_0109_219.
  DATA: l_int          TYPE i.

  IF st_0109_input-219_1 <> ''.
* 219_1
    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    st_0109_input-219_1 = l_int = st_0109_input-219_1.
    CONDENSE st_0109_input-219_1.
    CONCATENATE 'P_219_'  st_0109_input-219_1 INTO   r_atnam-low.
    APPEND r_atnam. CLEAR r_atnam.
  ENDIF.

  IF st_0109_input-219_2 <> ''.
* 219_2
    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    st_0109_input-219_2 = l_int = st_0109_input-219_2.
    CONDENSE st_0109_input-219_2.
    CONCATENATE 'P_219_'  st_0109_input-219_2 INTO   r_atnam-low  .
    APPEND r_atnam. CLEAR r_atnam.
  ENDIF.

  IF st_0109_input-219_3 <> ''.
* 219_3
    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    st_0109_input-219_3 = l_int = st_0109_input-219_3.
    CONDENSE st_0109_input-219_3.
    CONCATENATE 'P_219_'  st_0109_input-219_3 INTO   r_atnam-low  .
    APPEND r_atnam. CLEAR r_atnam.
  ENDIF.

  IF st_0109_input-219_4 <> ''.
* 219_4
    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    st_0109_input-219_4 = l_int = st_0109_input-219_4.
    CONDENSE st_0109_input-219_4.
    CONCATENATE 'P_219_'  st_0109_input-219_4 INTO   r_atnam-low .
    APPEND r_atnam. CLEAR r_atnam.
  ENDIF.

  IF st_0109_input-219_5 <> ''.
* 219_5
    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    st_0109_input-219_5 = l_int = st_0109_input-219_5.
    CONDENSE st_0109_input-219_5.
    CONCATENATE 'P_219_'  st_0109_input-219_5 INTO   r_atnam-low  .
    APPEND r_atnam. CLEAR r_atnam.
  ENDIF.

  IF st_0109_input-219_6 <> ''.
* 219_6
    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    st_0109_input-219_6 = l_int = st_0109_input-219_6.
    CONDENSE st_0109_input-219_6.
    CONCATENATE 'P_219_'  st_0109_input-219_6 INTO   r_atnam-low  .
    APPEND r_atnam. CLEAR r_atnam.
  ENDIF.

  IF st_0109_input-219_7 <> ''.
* 219_7
    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    st_0109_input-219_7 = l_int = st_0109_input-219_7.
    CONDENSE st_0109_input-219_7.
    CONCATENATE 'P_219_'  st_0109_input-219_7 INTO   r_atnam-low  .
    APPEND r_atnam. CLEAR r_atnam.
  ENDIF.

  IF st_0109_input-219_8 <> ''.
* 219_8
    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    st_0109_input-219_8 = l_int = st_0109_input-219_8.
    CONDENSE st_0109_input-219_8.
    CONCATENATE 'P_219_'  st_0109_input-219_8 INTO   r_atnam-low  .
    APPEND r_atnam. CLEAR r_atnam.
  ENDIF.

  IF st_0109_input-219_9 <> ''.
* 219_9
    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    st_0109_input-219_9 = l_int = st_0109_input-219_9.
    CONDENSE st_0109_input-219_9.
    CONCATENATE 'P_219_'  st_0109_input-219_9 INTO   r_atnam-low  .
    APPEND r_atnam. CLEAR r_atnam.
  ENDIF.

  IF st_0109_input-219_10 <> ''.
* 219_10
    r_atnam-sign  = 'I'.
    r_atnam-option = 'EQ'.
    st_0109_input-219_10 = l_int = st_0109_input-219_10.
    CONDENSE st_0109_input-219_10.
    CONCATENATE 'P_219_'  st_0109_input-219_10 INTO   r_atnam-low .
    APPEND r_atnam. CLEAR r_atnam.
  ENDIF.
ENDFORM.                    " range_insert_0109_219

*&---------------------------------------------------------------------*
*&      Form  display_1007
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_1007.
  DATA: l_color              LIKE mara-matnr.

  CLEAR: it_result1001, it_result1001[].

*  IF wa_order = space.
*    MESSAGE i001 WITH text-004 .
*    EXIT.
*  ENDIF.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = wa_order
      ctype        = '001'
    TABLES
      val_table    = it_result1001
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  IF sy-subrc = 0.
    READ TABLE it_result1001 WITH KEY atnam = 'P_MODEL' .
    wa_model   = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_MI'    .
    wa_mi      = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_OCN'   .
    wa_ocn     = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_TRIM_PLANT_NO'.
    wa_trim_plant_no =
                 it_result1001-atwrt.
    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_WO_CREATE_DATE'.
    wa_woc_date = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_WO_CREATE_DATE'.
    wa_wom_date = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_LC_NO' .
    wa_lc_no   = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_DESTINATION_CODE'.
    wa_destination_code
               = it_result1001-atwrt.
    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_PERF_YN' .
    wa_perf_yn = it_result1001-atwrt. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_VIN_SPEC'.
    wa_vin_spec =
                 it_result1001-atwrt.
    CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_INIT_QTY' .
    wa_init_qty = it_result1001-atwtb. CLEAR: it_result1001.
    READ TABLE it_result1001 WITH KEY atnam = 'P_MOD_QTY' .
    wa_mod_qty = it_result1001-atwrt. CLEAR: it_result1001.
  ENDIF.

  " Select Color Data for the ALC Code Error Check!!!
  CLEAR: wa_wosum, wa_wosum[].
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE wa_wosum
    FROM ztpp_wosum
   WHERE wo_ser = wa_order(9)
     AND nation = wa_order+9(3)
     AND dealer = wa_order+12(2)   .

  " ALC Condiftion Display.... (Header)
  PERFORM get_alc_condition  TABLES it_result1001 USING 'U'.
  " ALC Condiftion Display.... (Color)
  LOOP AT wa_wosum  WHERE wo_ser = wa_order(9)   AND
                          nation = wa_order+9(3) AND
                          dealer = wa_order+12(2)   .
    CONCATENATE wa_wosum-wo_ser  wa_wosum-nation  wa_wosum-dealer
                wa_wosum-extc    wa_wosum-intc    INTO  wa_color  .
    PERFORM call_color         TABLES it_color    USING wa_color  .
    PERFORM get_alc_condition  TABLES it_color    USING 'C'.
  ENDLOOP.
  DESCRIBE TABLE it_err LINES tc_a107-lines.
ENDFORM.                    " display_1007

*&---------------------------------------------------------------------*
*&      Form  data_select_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_0110.
  DATA : BEGIN OF st_ox,
         objek LIKE ausp-objek,
         atinn LIKE ausp-atinn,
         atzhl LIKE ausp-atzhl,
         mafid LIKE ausp-mafid,
         klart LIKE ausp-klart,
         adzhl LIKE ausp-adzhl,
         atwrt LIKE ausp-atwrt,
         END OF st_ox.

  DATA : l_body1    LIKE it_ausp-atwrt,
         l_body2    LIKE it_ausp-atwrt,
         l_model    LIKE it_ausp-atwrt,
         l_objek    LIKE it_ausp-atwrt,
         l_atinn    LIKE cabn-atinn   ,
         l_atinn1    LIKE cabn-atinn   ,
         l_atflv    LIKE ausp-atflv   ,
         l_date     TYPE d            ,
         l_num(8)   TYPE n            ,
         l_name(2) ,
         l_first ,
         l_atnam LIKE cabn-atnam.

  DATA : l_index TYPE i,
         l_check,
         l_nation LIKE ausp-atwrt,
         l_dealer LIKE ausp-atwrt,
         l_seq   TYPE i.

* entry check .
  IF st_0110_input-status <> ''.
    IF st_0110_input-status2 = ''.
      MESSAGE i001(zmpp) WITH 'Input status'.
    ENDIF.
  ENDIF.

  PERFORM range_insert_0110.  " range value

  CLEAR: l_seq, l_first, l_body1, l_body2, l_model, l_objek, st_ox.
  CLEAR: it_0110[], it_0110.

  SELECT SINGLE atinn INTO l_atinn1
    FROM cabn
   WHERE atnam = 'P_USAGE_CAR'.


  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = 'P_WORK_ORDER'.

  IF sy-ucomm = 'ZNEX'.
    SELECT  atwrt  INTO st_0110_input-order
       FROM ausp
       UP TO 1 ROWS
          WHERE klart = '002'
            AND atinn = l_atinn       " '0000000669'
            AND atwrt > st_0110_input-order
            AND lkenz = ' '
            ORDER BY atwrt ASCENDING ." deSCENDING.
    ENDSELECT.
  ENDIF.

  SELECT objek atinn atzhl mafid klart adzhl atwrt
         INTO st_ox
         FROM ausp
         WHERE klart = '002'
           AND atinn = l_atinn       " '0000000669'
           AND atwrt = st_0110_input-order
           AND lkenz = ' '  .

    SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
        INTO CORRESPONDING FIELDS OF TABLE it_ausp
     FROM zvpp_cha
     WHERE objek = st_ox-objek
       AND klart = '002'
       AND atnam IN r_atnam
       AND lkenz = ' ' .

* EXCLUDE THE SCRAPPED AND DISPOSAL CARS--ADDED BY CHRIS ON 06/29/2005
    CLEAR:it_ausp.
    READ TABLE it_ausp WITH KEY atinn = l_atinn1.
    IF sy-subrc EQ 0 AND
       ( it_ausp-atwrt = 'D' OR it_ausp-atwrt = 'S' ).
      CONTINUE.
    ENDIF.
* END OF ADD.

    CLEAR : l_check .

    MOVE : st_ox-objek TO it_0110-body .

    LOOP AT it_ausp.
      CASE  it_ausp-atnam.
        WHEN 'P_EXT_COLOR'.
          MOVE : it_ausp-atwrt TO  it_0110-exclr.

          IF st_0110_input-exclr <> ''.
            IF st_0110_input-exclr <>  it_0110-exclr.
              l_check = 'X'.
            ENDIF.
          ENDIF.
        WHEN 'P_INT_COLOR'.
          MOVE : it_ausp-atwrt TO  it_0110-inclr.
          IF st_0110_input-inclr <> ''.
            IF st_0110_input-inclr <>  it_0110-inclr.
              l_check = 'X'.
            ENDIF.
          ENDIF.

        WHEN 'P_MITU'.
          IF it_ausp-atwrt <> ''.
            MOVE : 'X'  TO  it_0110-mitu.
          ENDIF.
        WHEN 'P_ENGINE_NO'.
          MOVE : it_ausp-atwrt  TO  it_0110-engine.
        WHEN 'P_SALES_ORDER'.
          MOVE : it_ausp-atwrt  TO  it_0110-sales .
        WHEN 'P_PLAN_ORDER'.
          MOVE : it_ausp-atwrt  TO  it_0110-porder.
        WHEN 'P_RP_STATUS'.
          MOVE : it_ausp-atwrt  TO  it_0110-status.
          IF st_0110_input-status <> ''.
            IF NOT it_0110-status BETWEEN st_0110_input-status
                                      AND st_0110_input-status2.
              l_check = 'X'.
            ENDIF.
          ENDIF.
          IF it_0110-status = '00'.
            SELECT SINGLE atwrt INTO it_0110-rdate
              FROM zvpp_cha
              WHERE objek = st_ox-objek
                  AND klart = '002'
                  AND atnam = 'P_VM_DATE'
                  AND lkenz = ' ' .
            IF sy-subrc <> 0.
              SELECT SINGLE erdat
                INTO l_date "it_0110-rdate
                FROM equi
                WHERE equnr = st_ox-objek .

              it_0110-rdate = l_date .
            ENDIF.
          ELSE.
            CONCATENATE 'P_RP' it_0110-status '_ACTUAL_DATE' INTO l_atnam.
            SELECT SINGLE atwrt INTO it_0110-rdate
                FROM zvpp_cha
                WHERE objek = st_ox-objek
                  AND klart = '002'
                  AND atnam = l_atnam
                  AND lkenz = ' ' .
            CLEAR l_atnam.
          ENDIF.
        WHEN 'P_DESTINATION_CODE'.
          MOVE : it_ausp-atwrt  TO it_0110-nation.
      ENDCASE.
    ENDLOOP.
* SPEC NO
    PERFORM spec_no_0110 USING l_first  .

* 219 COLOR READ
    PERFORM 219_option_0110 USING st_ox-objek
                                  l_first
                            CHANGING st_0110_input-tbd.
    l_first = 'X'.

    IF l_check = ''.
      APPEND  it_0110. CLEAR it_0110.
    ELSE.
      CLEAR it_0110.
    ENDIF.
  ENDSELECT.

  SORT it_0110 BY status DESCENDING  rdate exclr inclr .
  DESCRIBE TABLE it_0110 LINES st_0110_input-count.
  IF st_0110_input-count = 0.
    MESSAGE s001 WITH text-100.
  ENDIF.
ENDFORM.                    " data_select_0110
*&---------------------------------------------------------------------*
*&      Form  range_insert_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM range_insert_0110.
  CLEAR : r_atnam[], r_atnam.

* BODY NUMBER
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_BODY_SERIAL'.
  APPEND r_atnam. CLEAR r_atnam.

* WORK ORDER
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_WORK_ORDER'.
  APPEND r_atnam. CLEAR r_atnam.

* EXTERNAL COLOR
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_EXT_COLOR'.
  APPEND r_atnam. CLEAR r_atnam.

* INTERNAL COLOR
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_INT_COLOR'.
  APPEND r_atnam. CLEAR r_atnam.

* STATUS
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_RP_STATUS'.
  APPEND r_atnam. CLEAR r_atnam.

* Sales Order
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_SALES_ORDER'.
  APPEND r_atnam. CLEAR r_atnam.

* Plan Order
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_PLAN_ORDER'.
  APPEND r_atnam. CLEAR r_atnam.

* Destination code
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_DESTINATION_CODE'.
  APPEND r_atnam. CLEAR r_atnam.

* MITU
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MITU'.
  APPEND r_atnam. CLEAR r_atnam.

* ENGINE no
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_ENGINE_NO'.
  APPEND r_atnam. CLEAR r_atnam.

* VIN
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_VIN'.
  APPEND r_atnam. CLEAR r_atnam.

* OCN
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_OCN'.
  APPEND r_atnam. CLEAR r_atnam.

* VERSION
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_VERSION'.
  APPEND r_atnam. CLEAR r_atnam.

* VERSION
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_MI'.
  APPEND r_atnam. CLEAR r_atnam.

* USAGE
  r_atnam-sign   = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low    = 'P_USAGE_CAR'.
  APPEND r_atnam. CLEAR r_atnam.


ENDFORM.                    " range_insert_0110
*&---------------------------------------------------------------------*
*&      Form  219_option_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_OX_OBJEK  text
*      <--P_ST_0110_INPUT_OPTION  text
*----------------------------------------------------------------------*
FORM 219_option_0110 USING    p_objek
                              p_first
                     CHANGING p_option.

  DATA : l_tmp LIKE ausp-atwrt,
         l_name(20) TYPE c,
         l_count(2) ,
         l_atnam LIKE cabn-atnam.

  CHECK p_first = ' '.

  CLEAR l_count.

  SELECT  objek atinn atzhl mafid klart adzhl atwrt atflv atnam
      INTO CORRESPONDING FIELDS OF TABLE it_ausp
   FROM zvpp_cha
   WHERE objek = p_objek
     AND klart = '002'
     AND atnam BETWEEN 'P_219_1' AND 'P_219_999'
     AND lkenz = ' ' .

  SORT it_ausp BY atnam.
  CLEAR l_tmp.


  DO 20 TIMES .
    l_count = l_count + 1.
    CONCATENATE 'P_219_' l_count INTO l_name.

    READ TABLE it_ausp WITH KEY atnam = l_name .

    IF sy-subrc EQ 0.
      CONCATENATE l_tmp it_ausp-atwrt INTO p_option.
      l_tmp = p_option.
    ELSE.
      CONCATENATE l_tmp ' '  INTO p_option.
      l_tmp = p_option.
    ENDIF.

  ENDDO.
ENDFORM.                    " 219_option_0110
*&---------------------------------------------------------------------*
*&      Form  SPEC_NO_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM spec_no_0110 USING p_first .

  CHECK p_first =  ' '.

  READ TABLE it_ausp WITH KEY atnam = 'P_VERSION'.
  IF sy-subrc EQ 0.
    MOVE : it_ausp-atwrt TO  st_0110_input-ver.
  ENDIF.

  READ TABLE it_ausp WITH KEY atnam = 'P_OCN'.
  IF sy-subrc EQ 0.
    MOVE : it_ausp-atwrt TO st_0110_input-ocn.
  ENDIF.

  READ TABLE it_ausp WITH KEY atnam = 'P_MI'.
  IF sy-subrc EQ 0.
    MOVE : it_ausp-atwrt TO st_0110_input-mi.
  ENDIF.

ENDFORM.                    " SPEC_NO_0110
*&---------------------------------------------------------------------*
*&      Form  data_select_0111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_0111.
  IF st_0111_input-order1 = space AND
     st_0111_input-order2 = space AND
     st_0111_input-exclr1 = space AND
     st_0111_input-exclr2 = space AND
     st_0111_input-inclr1 = space AND
     st_0111_input-inclr2 = space   .
    MESSAGE s000 WITH 'Set Parameter - W/O!'.
    EXIT.
  ENDIF.
  PERFORM data_selection_order1.

  IF st_0111_input-order2 <> ''.
    PERFORM data_selection_order2.
  ENDIF.
ENDFORM.                    " data_select_0111

*&---------------------------------------------------------------------*
*&      Form  data_selection_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_selection_order1.
  DATA: it_condition LIKE zsca_characteristic_value " Read condition
                          OCCURS 0 WITH HEADER LINE,
        it_vehicle   LIKE zsca_vehicle_char_value
        OCCURS 0 WITH HEADER LINE,                   "Result
        lt_value LIKE zsca_char_value OCCURS 0 WITH HEADER LINE,
        l_alc_name(20) TYPE c,
        l_count(3)     TYPE c,
        l_atwrt_s LIKE ausp-atwrt,
        l_atwrt_e LIKE ausp-atwrt,
        l_objek   LIKE ausp-objek,
        l_klart   LIKE ausp-klart,
        l_color   .

  CLEAR l_color.

  IF st_0111_input-exclr1 <> '' .
    l_color = 'X'.
  ENDIF.

  CLEAR : l_atwrt_s, l_atwrt_e.

  MOVE : st_0111_input-order1(9) TO l_atwrt_s ,
         st_0111_input-order1(9) TO l_atwrt_e ,
         st_0111_input-order1    TO l_objek,
         '001'                   TO l_klart.

  CLEAR : l_count, it_0111_c, it_0111_c[] .
  CLEAR : it_0111, it_0111[], lt_value, lt_value[].

  DO 219 TIMES .
    l_count = l_count + 1.
* ALC
    CONDENSE l_count.
    IF l_count <=  200.
      CONCATENATE 'P_ALC_U_' l_count INTO l_alc_name.
      lt_value-atnam = l_alc_name . APPEND lt_value.
      CLEAR : l_alc_name  .
    ENDIF.

* 219
    CONCATENATE 'P_219_' l_count INTO l_alc_name.
    lt_value-atnam = l_alc_name . APPEND lt_value.
    CLEAR : l_alc_name  .
  ENDDO.

  lt_value-atnam = 'P_MI' .      APPEND lt_value.
  lt_value-atnam = 'P_OCN' .     APPEND lt_value.
  lt_value-atnam = 'P_VERSION' . APPEND lt_value.

  CALL FUNCTION 'Z_FCA_GET_WORK_ORDER_MASTER'
    EXPORTING
      i_atnam                       = 'P_WO_SER'
      i_atwrt_s                     = l_atwrt_s
      i_atwrt_e                     = l_atwrt_e
      i_objek                       = l_objek
      i_klart                       = l_klart
      i_count                       = 1000000
    TABLES
      t_condition                   = it_condition
      t_vehicle                     = it_vehicle
      t_value                       = lt_value
    EXCEPTIONS
      date_overflow                 = 1
      invalid_date                  = 2
      condition_does_not_exist      = 3
      characteristic_does_not_exist = 4
      OTHERS                        = 5.

  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'No Data!!'.
    CLEAR: it_0111, it_0111_c, it_0111[], it_0111_c[].
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR : l_count.
* SPEC NO READ
  READ TABLE it_vehicle WITH KEY atnam = 'P_MI'.
  IF sy-subrc EQ 0.
    MOVE : it_vehicle-atwrt TO  st_0111_input-mi1 .
  ENDIF.

  READ TABLE it_vehicle WITH KEY atnam = 'P_OCN'.
  IF sy-subrc EQ 0.
    MOVE : it_vehicle-atwrt TO  st_0111_input-ocn1 .
  ENDIF.

  READ TABLE it_vehicle WITH KEY atnam = 'P_VERSION'.
  IF sy-subrc EQ 0.
    MOVE : it_vehicle-atwrt TO  st_0111_input-ver1 .
  ENDIF.

* ALC
  DO 200 TIMES .
    CLEAR : l_alc_name, it_0111.
    l_count = l_count + 1.
    CONDENSE l_count.
    CONCATENATE 'P_ALC_U_' l_count INTO l_alc_name.
    READ TABLE it_vehicle WITH KEY atnam = l_alc_name.
    IF sy-subrc EQ 0.
      SELECT SINGLE atbez INTO it_0111-cname
          FROM cabnt
          WHERE atinn = it_vehicle-atinn
            AND spras = sy-langu.
      MOVE : it_vehicle-atwrt TO it_0111-code1 .
    ENDIF.
    MOVE : l_count TO it_0111-clm.
    APPEND it_0111. CLEAR it_0111.
  ENDDO.

  CLEAR : l_count.
  DO 219 TIMES .
    CLEAR : l_alc_name, it_0111.
    l_count = l_count + 1.
    CONDENSE l_count.
    CONCATENATE 'P_219_' l_count INTO l_alc_name.
    READ TABLE it_vehicle WITH KEY atnam = l_alc_name.
    IF sy-subrc EQ 0.
      SELECT SINGLE atbez INTO it_0111_c-cname
          FROM cabnt
          WHERE atinn = it_vehicle-atinn
            AND spras = sy-langu.
      MOVE : it_vehicle-atwrt TO it_0111_c-code1 .
    ENDIF.
    MOVE : l_count TO it_0111_c-clm.
    APPEND it_0111_c. CLEAR it_0111_c.
  ENDDO.
ENDFORM.                    " data_selection_alc

*&---------------------------------------------------------------------*
*&      Form  APPEND_5293
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_5293.
  DATA: l_idx           TYPE i      ,
        lw_5293         LIKE it_5293.

  READ TABLE it_5293 WITH KEY time = it_5293-time INTO lw_5293.
  IF sy-subrc = 0.
    l_idx = sy-tabix.
    lw_5293-bi   = lw_5293-bi   + it_5293-bi   .
    lw_5293-pi   = lw_5293-pi   + it_5293-pi   .
    lw_5293-tc   = lw_5293-tc   + it_5293-tc   .
    lw_5293-po   = lw_5293-po   + it_5293-po   .
    lw_5293-pbsi = lw_5293-pbsi + it_5293-pbsi .
    lw_5293-trim = lw_5293-trim + it_5293-trim .
    lw_5293-cf   = lw_5293-cf   + it_5293-cf   .
    lw_5293-plan = lw_5293-plan + it_5293-plan .
    lw_5293-soff = lw_5293-soff + it_5293-soff .
    lw_5293-cg   = lw_5293-cg   + it_5293-cg   .
    lw_5293-vpi  = lw_5293-vpi  + it_5293-vpi  .
    lw_5293-vpo  = lw_5293-vpo  + it_5293-vpo  .
    MODIFY it_5293 FROM lw_5293 INDEX l_idx.
    CLEAR it_5293.
  ELSE.
    APPEND it_5293. CLEAR it_5293.
  ENDIF.
ENDFORM.                    " APPEND_5293

*&---------------------------------------------------------------------*
*&      Form  call_function_for_sales_order
*&---------------------------------------------------------------------*
*       Calling a Function to modify sales Order's quantities
*----------------------------------------------------------------------*
*      -->P_L_SALES_ORDER_OLD  text
*      -->P_L_ITEM10_QTY_OLD  text
*      -->P_L_ITEM20_QTY_OLD  text
*----------------------------------------------------------------------*
FORM call_function_for_sales_order USING  pa_order
                                          pa_matnr
                                          pa_type
                                   CHANGING pa_flag.
  DATA : l_it_ord_header_inx LIKE TABLE OF bapisdh1x  WITH HEADER LINE,
         l_bapisdls          LIKE bapisdls ,
         l_it_return         LIKE TABLE OF bapiret2   WITH HEADER LINE,
         l_it_itm            LIKE TABLE OF bapisditm  WITH HEADER LINE,
         l_it_itmx           LIKE TABLE OF bapisditmx WITH HEADER LINE,
         l_it_lines          LIKE TABLE OF bapischdl  WITH HEADER LINE,
         l_it_linesx         LIKE TABLE OF bapischdlx WITH HEADER LINE.

  DATA : p_item10_org         LIKE vbap-kwmeng,
         p_item20_org         LIKE vbap-kwmeng,
         p_item20_qty         LIKE ztpp_wosum-modqty,
         p_item10_qty         LIKE ztpp_wosum-seqqty,
         l_stot               LIKE vbap-kwmeng,
         l_wtot               LIKE ztpp_wosum-seqqty,
         l_qty                TYPE i                ,
         l_item10_flg(01),
         l_item20_flg(01),
         l_item10_qty_flg(01),
         l_item20_qty_flg(01).
  DATA : wa_overseq,
         wa_underseq.
  DATA : it_message  LIKE TABLE OF zspp_func_message WITH HEADER LINE.

*
*  Be Careful ... The Data can be removed ...
  SELECT SINGLE modqty seqqty
    INTO (p_item20_qty, p_item10_qty)
    FROM ztpp_wosum
  WHERE wo_ser = pa_matnr(9)
    AND nation = pa_matnr+9(3)
    AND dealer = pa_matnr+12(2)
    AND extc   = pa_matnr+14(2)
    AND intc   = pa_matnr+16(2)
     .

  l_wtot = p_item20_qty .
  l_qty  = p_item20_qty = p_item20_qty - p_item10_qty .
  IF l_qty < 0.  CLEAR: p_item20_qty .  ENDIF.

  SELECT SINGLE kwmeng INTO p_item10_org
    FROM vbap
   WHERE vbeln = pa_order
     AND posnr = '000010' .

  SELECT SINGLE kwmeng INTO p_item20_org
    FROM vbap
   WHERE vbeln = pa_order
     AND posnr = '000020' .

  CASE pa_type.
    WHEN '+'.
      IF p_item20_org = 0 .
        wa_overseq = 'E'  .
      ENDIF.
      IF p_item20_qty > 0.
*        p_item20_qty = p_item20_qty - 1 .
      ENDIF.
*       p_item10_qty = p_item10_qty + 1 .
    WHEN '-'.
      " Check the Over-Sequence Quantity...
      l_stot = p_item20_qty + p_item10_qty .
      IF l_stot > l_wtot .
        p_item20_qty = 0 .
      ELSE.
*        p_item20_qty = p_item20_qty + 1.
      ENDIF.
*      p_item10_qty = p_item10_qty - 1 .
      IF p_item10_org = 0 .
        wa_underseq = 'E'  .
      ENDIF.
  ENDCASE.

  IF p_item10_org =  0     .
    l_item10_flg     = 'I'   .
    l_item10_qty_flg = 'I'   .
    IF p_item20_org = 0    .
      l_item20_flg     = 'I'   .    " 'U' ??
      l_item20_qty_flg = 'I'   .
    ELSEIF p_item20_qty = 0    .
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'D'   .
    ELSE.
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'U'   .
    ENDIF.
  ELSEIF p_item10_qty = 0    .
    l_item10_flg     = 'U'   .
    l_item10_qty_flg = 'D'   .
    IF p_item20_org = 0    .
      l_item20_flg     = 'I'   .    " 'U' ??
      l_item20_qty_flg = 'I'   .
    ELSEIF p_item20_qty = 0    .
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'D'   .
    ELSE.
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'U'   .
    ENDIF.
  ELSE.
    l_item10_flg     = 'U'   .
    l_item10_qty_flg = 'U'   .
    IF p_item20_org = 0    .
      l_item20_flg     = 'I'   .    " 'U' ??
      l_item20_qty_flg = 'I'   .
    ELSEIF p_item20_qty = 0    .
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'D'   .
    ELSE.
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'U'   .
    ENDIF.
  ENDIF.

  l_bapisdls-scheduling = 'X'.

  l_it_ord_header_inx-updateflag = 'U'.
  APPEND l_it_ord_header_inx.

  l_it_itm-itm_number = '000010'.
  APPEND l_it_itm.
  l_it_itm-itm_number = '000020'.
  APPEND l_it_itm.

  l_it_itmx-updateflag = l_item10_flg.
  l_it_itmx-itm_number = '000010'.
  APPEND l_it_itmx.
  l_it_itmx-updateflag = l_item20_flg.
  l_it_itmx-itm_number = '000020'.
  APPEND l_it_itmx.
* deleted by Furong on 03/30/2006, requested by MY Hur
*  p_item10_org = p_item10_org - 1.
*  p_item20_org = p_item20_org + 1.
*
  l_it_lines-itm_number = '000010'.
  IF l_item10_qty_flg = 'I'       .
    l_it_lines-sched_line = '0001'.
  ELSE.
    SELECT SINGLE etenr INTO l_it_lines-sched_line
      FROM vbep
     WHERE vbeln = pa_order
       AND posnr = l_it_lines-itm_number .
  ENDIF.
* changed by Furong on 03/30/2006, requested by MY Hur
*  l_it_lines-req_qty = p_item10_org.
  l_it_lines-req_qty = p_item10_qty.
  APPEND l_it_lines.

  l_it_linesx-updateflag = l_item10_qty_flg.
  l_it_linesx-itm_number = '000010'.
  l_it_linesx-sched_line = l_it_lines-sched_line .
  l_it_linesx-req_qty = 'X'.
  APPEND l_it_linesx.

  l_it_lines-itm_number = '000020'.
  IF l_item20_qty_flg = 'I'       .
    l_it_lines-sched_line = '0001'.
  ELSE.
    SELECT SINGLE etenr INTO l_it_lines-sched_line
      FROM vbep
     WHERE vbeln = pa_order
       AND posnr = l_it_lines-itm_number .
  ENDIF.
* changed by Furong on 03/30/2006, requested by MY Hur
*  l_it_lines-req_qty = p_item20_org.
  l_it_lines-req_qty = p_item20_qty.
  APPEND l_it_lines.

  l_it_linesx-updateflag = l_item20_qty_flg.
  l_it_linesx-itm_number = '000020'.
  l_it_linesx-sched_line = l_it_lines-sched_line .
  l_it_linesx-req_qty = 'X'.
  APPEND l_it_linesx.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = pa_order
      order_header_inx = l_it_ord_header_inx
      logic_switch     = l_bapisdls
    TABLES
      return           = l_it_return
      order_item_in    = l_it_itm
      order_item_inx   = l_it_itmx
      schedule_lines   = l_it_lines
      schedule_linesx  = l_it_linesx.

  LOOP AT l_it_return.
    IF l_it_return-type = 'E' OR
       l_it_return-type = 'A'   .
      pa_flag = 'E'.
      CLEAR it_message.
      it_message-woc_no = pa_order .
      IF pa_type = '-' .
        CONCATENATE 'MINUS :' l_it_return-message INTO it_message-message.
      ELSE.
        CONCATENATE 'PLUS :' l_it_return-message INTO it_message-message.
      ENDIF.
      it_message-result_type = 'E'.
      APPEND it_message.
    ELSE.
      pa_flag = 'S'.
      CLEAR it_message.
      CONCATENATE pa_order
          l_it_return-message
        INTO it_message-message.
      it_message-result_type = 'S'.
      APPEND it_message.
    ENDIF.
  ENDLOOP.
  IF pa_flag = 'S'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
     EXPORTING
       wait          = 'X'
*     IMPORTING
*       RETURN        =
              .
  ELSE.
    READ TABLE it_message INDEX 1.
    MESSAGE s002 WITH
    'SALES ORDER UPDATE FAILED. Order No:' pa_order it_message-message.
  ENDIF.
ENDFORM.                    " call_function_for_sales_order
*&---------------------------------------------------------------------*
*&      Form  change_scraped_car
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_scraped_car     USING p_objek.
  DATA: l_worder  LIKE mara-matnr.
  DATA: lt_val LIKE zspp_vin_value OCCURS 0 WITH HEADER LINE.
  DATA: wa_scrap  LIKE ztpp_scrap_car.

  lt_val-atnam = 'P_USAGE_TYPE'.
  lt_val-atwrt = st_2204-usage.
  APPEND lt_val .  CLEAR lt_val.

  lt_val-atnam = 'P_USAGE_TEXT'.
  lt_val-atwrt = st_2204-text.
  APPEND lt_val .  CLEAR lt_val.

  lt_val-atnam = 'P_USAGE_DEPT'.
  lt_val-atwrt = st_2204-u_dept.
  APPEND lt_val .  CLEAR lt_val.

  lt_val-atnam = 'P_SCRAP_APP_DOC'.
  lt_val-atwrt = st_2204-scrap .
  APPEND lt_val .  CLEAR lt_val.

* change the master data
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = p_objek
      mode         = 'W'
      ctype        = '002'
    TABLES
      val_table    = lt_val
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

* update the table ztpp_scrap_car


  SELECT SINGLE * INTO wa_scrap
    FROM ztpp_scrap_car
    WHERE model = p_objek(3)
      AND body_ser = p_objek+3(6).
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.
  wa_scrap-scr_app_doc = st_2204-scrap.
  wa_scrap-u_text = st_2204-text.
  wa_scrap-u_type = st_2204-usage.
  wa_scrap-u_dept = st_2204-u_dept.
  wa_scrap-aenam  = sy-uname.
  wa_scrap-aedat  = sy-datum.
  wa_scrap-aezet  = sy-uzeit.
  MODIFY ztpp_scrap_car FROM wa_scrap.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'Update failed'.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                    " change_scraped_car
*&---------------------------------------------------------------------*
*&      Form  set_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_day.
*  data: begin OF lt_temp occurs 0,
*        rsnum like ztpp_input_plan-rsnum,
*        end of lt_temp.
  DATA: lt_temp LIKE TABLE OF ztpp_input_plan WITH HEADER LINE.
  DATA: l_recno(2)  TYPE n,
        l_text(25),
        l_date(5).

  FIELD-SYMBOLS: <field>.

  SELECT * INTO TABLE lt_temp
   FROM ztpp_input_plan.
*   where rsnum <> '        ' OR rsnum <> '00000000'.

  SORT lt_temp BY rsnum.
  DELETE lt_temp WHERE rsnum = '        ' OR rsnum = '00000000'.
  DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING rsnum.
*  read table lt_temp index 1.
*  if lt_temp-RSNUM <> sy-datum.
*     message e000 with 'Running Sequencying program first'.
*  endif.

  CLEAR: l_recno.
  CLEAR: it_app272_date, it_app272_date[].
  LOOP AT lt_temp.
    l_recno = l_recno + 1.
    CONCATENATE 'IT_APP272_DATE-DAY' l_recno INTO l_text.
    ASSIGN (l_text) TO <field>.
*    CONCATENATE LT_TEMP-rsnum+6(2) '/' LT_TEMP-rsnum+4(2) INTO l_date.
*    <field> = l_date.
    <field> = lt_temp-rsnum.
  ENDLOOP.
  APPEND it_app272_date.
ENDFORM.                    " set_day
*&---------------------------------------------------------------------*
*&      Form  PAGING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OK_CODE  text
*----------------------------------------------------------------------*
FORM paging USING code.
  DATA: i TYPE i,
        j TYPE i.

  CASE code.
    WHEN 'P--'. tb_8081-top_line = 1.
    WHEN 'P-'.
      tb_8081-top_line = tb_8081-top_line - line_count.
      IF tb_8081-top_line LE 0.
        tb_8081-top_line = 1.
      ENDIF.
    WHEN 'P+'.
      i = tb_8081-top_line + line_count.
      j = tb_8081-lines - line_count + 1.
      IF j LE 0.
        j = 1.
      ENDIF.
      IF i LE j.
        tb_8081-top_line = i.
      ELSE.
        tb_8081-top_line = j.
      ENDIF.
    WHEN 'P++'.
      tb_8081-top_line = tb_8081-lines - line_count + 1.
      IF tb_8081-top_line LE 0.
        tb_8081-top_line = 1.
      ENDIF.
  ENDCASE.

ENDFORM.                    " PAGING
