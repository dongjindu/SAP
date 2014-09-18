*----------------------------------------------------------------------*
***INCLUDE ZAPP237M_FOB .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MADE_DELAY_DATA
*&---------------------------------------------------------------------*
*       Getting Initial Data Per P_STATUS('T11' or 'T24')
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM made_delay_data_2107.

  CLEAR  WA_DLY_LINES_2107.

  MOVE-CORRESPONDING  zspp_app237  TO  sv_key.
  CASE  zspp_app237-gubun.
    WHEN '1'.
      P_ATWRT_2107  =  'T11'.   " Waiting for rework
    WHEN '2'.
      P_ATWRT_2107  =  'T24'.   " Long-term stock
  ENDCASE.

  PERFORM  data_selection_2107  USING  P_ATWRT_2107.


ENDFORM.                    " MADE_DELAY_DATA
*&---------------------------------------------------------------------*
*&      Form  data_selection
*&---------------------------------------------------------------------*
*       Searching V/M No. By Characteristic 'P_STATUS'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_selection_2107  USING  p_atwrt.

  PERFORM  conversion_atinn_call_2107  USING  'P_STATUS'
                                              P_STATUS_2107.

  REFRESH: IT_EQUNR_2107, IT_VMV_2107, IT_DLY_2107.

  SELECT   objek INTO CORRESPONDING FIELDS OF TABLE IT_EQUNR_2107
    FROM   ausp
   WHERE  atinn  EQ   P_STATUS_2107
     AND  atwrt  EQ   p_atwrt.

  IF sy-subrc NE 0.
    MESSAGE s001  WITH  p_atwrt ' Data not found !'.
    EXIT.
  ENDIF.

  PERFORM  vehicle_data_seek_2107.

ENDFORM.                    " data_selection
*&---------------------------------------------------------------------*
*&      Form  conversion_atinn_call
*&---------------------------------------------------------------------*
*     Calling a Func. For Char Name Conversion To Internal No of Char
*----------------------------------------------------------------------*
*      -->P_0020   text
*      -->  P_STATUS_2107  text
*----------------------------------------------------------------------*
FORM conversion_atinn_call_2107 USING    p_value
                                    p_atinn.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = p_value
       IMPORTING
            output = p_atinn.

ENDFORM.                    " conversion_atinn_call
*&---------------------------------------------------------------------*
*&      Form  vehicle_data_seek
*&---------------------------------------------------------------------*
*       Creation of Detail Data from each V/M Number
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vehicle_data_seek_2107.
  LOOP AT IT_EQUNR_2107.
    CLEAR IT_DLY_2107.
    PERFORM  equipment_master_read_2107  USING   IT_EQUNR_2107-objek.
    IF WA_VMV_LINES_2107 IS INITIAL.
      CONTINUE.
    ENDIF.
    PERFORM  create_delay_table_2107.  "appending IT_DLY_2107
  ENDLOOP.

  DESCRIBE TABLE IT_DLY_2107  LINES WA_DLY_LINES_2107.
  TC_APP237-lines = WA_DLY_LINES_2107.

ENDFORM.                    " vehicle_data_seek
*&---------------------------------------------------------------------*
*&      Form  equipment_master_read
*&---------------------------------------------------------------------*
*       Calling a Func For Getting Characteristics' Values
*----------------------------------------------------------------------*
*      -->P_IT_EQUNR_2107_OBJEK  text
*----------------------------------------------------------------------*
FORM equipment_master_read_2107 USING    p_objek.
  CLEAR IT_VMV_2107.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object    = p_objek
       TABLES
            val_table = IT_VMV_2107.

  DESCRIBE TABLE IT_VMV_2107  LINES  WA_VMV_LINES_2107.

  IF sy-subrc EQ 0  OR  sy-subrc EQ 4.
  ELSE.
    MESSAGE s003 WITH P_EQUNR_2107 'Vehicle History Read Error !'.
    STOP.
  ENDIF.

ENDFORM.                    " equipment_master_read
*&---------------------------------------------------------------------*
*&      Form  create_delay_table
*&---------------------------------------------------------------------*
*       Getting Char's Value From Char's Name
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_delay_table_2107.
  CLEAR: WA_MODEL_2107, WA_SERIAL_2107.

  READ TABLE IT_VMV_2107  WITH KEY  atnam  =   'P_MODEL'
                               atwrt  =   zspp_app237-model.
  CHECK sy-subrc EQ 0.

  PERFORM  data_mapping_from_vmv_2107  USING    'P_MODEL'
                                                ' '
                                       CHANGING IT_DLY_2107-model.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_BODY_SERIAL'
                                                ' '
                                       CHANGING WA_SERIAL_2107.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_WORK_ORDER'
                                                ' '
                                       CHANGING IT_DLY_2107-won.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_EXT_COLOR'
                                                ' '
                                       CHANGING IT_DLY_2107-extc.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_INT_COLOR'
                                                ' '
                                       CHANGING IT_DLY_2107-intc.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_INT_COLOR'
                                                ' '
                                       CHANGING IT_DLY_2107-intc.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_INT_COLOR'
                                                ' '
                                       CHANGING IT_DLY_2107-intc.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_RP01_SHOP_DATE'
                                                'X'
                                       CHANGING IT_DLY_2107-bodyin.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_RP02_SHOP_DATE'
                                                'X'
                                       CHANGING IT_DLY_2107-paintin.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_RP07_SHOP_DATE'
                                                'X'
                                       CHANGING IT_DLY_2107-trimin.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_RP17_SHOP_DATE'
                                                'X'
                                       CHANGING IT_DLY_2107-cfinal.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_RP18_SHOP_DATE'
                                                'X'
                                       CHANGING IT_DLY_2107-soff.
  PERFORM  data_mapping_from_vmv_2107  USING    'P_RP19_SHOP_DATE'
                                                'X'
                                       CHANGING IT_DLY_2107-cgate.

  CONCATENATE  IT_DLY_2107-model WA_SERIAL_2107
    INTO IT_DLY_2107-bodyno SEPARATED BY space.

  IT_DLY_2107-dday = sy-datum - IT_DLY_2107-cfinal.

  APPEND IT_DLY_2107.

ENDFORM.                    " create_delay_table
*&---------------------------------------------------------------------*
*&      Form  data_mapping_from_vmv
*&---------------------------------------------------------------------*
*       Getting Char's Value From Char's Name By The Internal Table
*----------------------------------------------------------------------*
*      -->P_0146   text
*      <--P_WA_MODEL_2107  text
*----------------------------------------------------------------------*
FORM data_mapping_from_vmv_2107 USING    p_atnam
                                         p_flag
                                CHANGING p_atwrt.

  READ TABLE IT_VMV_2107  WITH KEY  atnam  =   p_atnam.

  CHECK sy-subrc EQ 0.

  IF p_flag <> space.
    CALL FUNCTION 'CONVERSION_EXIT_DATEX_INPUT'
         EXPORTING
              input  = IT_VMV_2107-atwrt
         IMPORTING
              output = p_atwrt .
  ELSE.
    MOVE  IT_VMV_2107-atwrt  TO  p_atwrt.
  ENDIF.
ENDFORM.                    " data_mapping_from_vmv
