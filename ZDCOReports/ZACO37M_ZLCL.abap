*----------------------------------------------------------------------*
*   INCLUDE ZACO37M_ZLCL                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   LOCAL CLASS Definition
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    DATA: GV_ERROR_IN_DATA TYPE C.
* Method when Changing field contents
    METHODS:
      HANDLE_DATA_CHANGED
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
             IMPORTING ER_DATA_CHANGED.
* Method to read error logs
    METHODS:
       READ_ERROR_LOG
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
             IMPORTING ER_DATA_CHANGED.

  PRIVATE SECTION.
* Methods to modularize event handler method HANDLE_DATA_CHANGED:
    METHODS: CHECK_MATNR_CHG
     IMPORTING
         LV_MATNR_CHG    TYPE LVC_S_MODI
         PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

ENDCLASS.

*----------------------------------------------------------------------*
*   LOCAL CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

*----------------------------------------------------------------------*
*   Method HANDLE_DATA_CHANGED
*----------------------------------------------------------------------*
  METHOD HANDLE_DATA_CHANGED.

    DATA: WA_L_GOOD TYPE LVC_S_MODI.

    GV_ERROR_IN_DATA = SPACE.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO WA_L_GOOD.
* check if column MATNR_CHG of this row was changed
      CASE WA_L_GOOD-FIELDNAME.
        WHEN 'MATNR_CHG'.
          CALL METHOD CHECK_MATNR_CHG
                 EXPORTING
                    LV_MATNR_CHG      = WA_L_GOOD
                    PR_DATA_CHANGED   = ER_DATA_CHANGED.
      ENDCASE.
    ENDLOOP.

*Display application log if an error has occured.
    IF GV_ERROR_IN_DATA EQ 'X'.
      CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.
    ENDIF.

  ENDMETHOD.

*----------------------------------------------------------------------*
*   Method READ_ERROR_LOG
*----------------------------------------------------------------------*
  METHOD READ_ERROR_LOG.
    DATA: WA_L_PROTOCOL TYPE LVC_S_MSG1.
    CLEAR GV_ERROR_IN_DATA.
    LOOP AT ER_DATA_CHANGED->MT_PROTOCOL INTO WA_L_PROTOCOL.
      IF WA_L_PROTOCOL-MSGTY CA 'AE'.
* Set Error Indicator "X"
        GV_ERROR_IN_DATA = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

*----------------------------------------------------------------------*
*   Method CHECK_MATNR_CHG
*----------------------------------------------------------------------*
  METHOD CHECK_MATNR_CHG.

    DATA : LV_CHG_MAT TYPE MATNR.
    DATA : LV_SUB TYPE C.
    DATA : LV_ORG_MATNR TYPE MATNR .

* Get new cell value to check it. (SAPCE)
    CALL METHOD PR_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING I_ROW_ID =    LV_MATNR_CHG-ROW_ID
                    I_FIELDNAME = LV_MATNR_CHG-FIELDNAME
          IMPORTING E_VALUE     = LV_CHG_MAT.

    IF LV_CHG_MAT EQ SPACE.
      CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
       EXPORTING
          I_MSGID = 'ZMCO' I_MSGNO = '018'  I_MSGTY = 'E'
          I_MSGV1 = 'No material code'
*         I_MSGV2 =
*         I_MSGV3 =
          I_FIELDNAME = LV_MATNR_CHG-FIELDNAME
          I_ROW_ID = LV_MATNR_CHG-ROW_ID.
* Set Error Indicator "X"
      GV_ERROR_IN_DATA = 'X'.
      EXIT.
    ENDIF.

* Check Material Type
    CLEAR : LV_SUB, LV_ORG_MATNR.
    PERFORM CHECK_MAT_TYPE USING LV_MATNR_CHG-ROW_ID
                                 LV_CHG_MAT
                                 LV_ORG_MATNR
                                 LV_SUB.
    IF LV_SUB = 'W'.
      CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
       EXPORTING
          I_MSGID = 'ZMCO' I_MSGNO = '027'  I_MSGTY = LV_SUB
          I_MSGV1 = LV_CHG_MAT
          I_MSGV2 = LV_ORG_MATNR
*         I_MSGV3 =
          I_FIELDNAME = LV_MATNR_CHG-FIELDNAME
          I_ROW_ID    = LV_MATNR_CHG-ROW_ID.
    ENDIF.
    IF LV_SUB = 'E'.
      CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
       EXPORTING
          I_MSGID = 'ZMCO' I_MSGNO = '028'  I_MSGTY = LV_SUB
          I_MSGV1 = LV_CHG_MAT
*         I_MSGV2 =
*         I_MSGV3 =
          I_FIELDNAME = LV_MATNR_CHG-FIELDNAME
          I_ROW_ID    = LV_MATNR_CHG-ROW_ID.
* Set Error Indicator "X"
      GV_ERROR_IN_DATA = 'X'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
