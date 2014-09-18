*&---------------------------------------------------------------------*
*&  Include           ZFMR0030CLS
*&---------------------------------------------------------------------*
CLASS LCL_APPLICATION DEFINITION.

  PUBLIC SECTION.
    METHODS:
       HANDLE_ITEM_DOUBLE_CLICK
        FOR EVENT ITEM_DOUBLE_CLICK OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY
                  ITEM_NAME.

ENDCLASS.                    "LCL_APPLICATION DEFINITION


*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_APPLICATION
*&---------------------------------------------------------------------*
CLASS LCL_APPLICATION IMPLEMENTATION.
  METHOD  HANDLE_ITEM_DOUBLE_CLICK.

    CASE ITEM_NAME.
      WHEN C_COL-C03.
        PERFORM CALL_ZRFFMEPGAX USING NODE_KEY ITEM_NAME
                                      P_PERIOD P_PERIOD.

      WHEN C_COL-C04.
        PERFORM CALL_PLAN_LIST USING NODE_KEY ITEM_NAME
                                     P_PERIOD P_PERIOD.

      WHEN C_COL-C06.
        PERFORM CALL_ZRFFMEPGAX USING NODE_KEY ITEM_NAME
                                      '01'     P_PERIOD.

      WHEN C_COL-C07.
        PERFORM CALL_PLAN_LIST USING NODE_KEY ITEM_NAME
                                     '01'     P_PERIOD.

      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

ENDCLASS.               "LCL_APPLICATION
