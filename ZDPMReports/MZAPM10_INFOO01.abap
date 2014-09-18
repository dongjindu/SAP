*----------------------------------------------------------------------*
***INCLUDE MZAPM10_INFOO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE OUTPUT.
  CASE SY-DYNNR.
    WHEN 0110.
      IF WA_PGM IS INITIAL.
        WA_PGM = 'SAPLZGPM_INFO'.
      ENDIF.
      IF WA_0110 IS INITIAL.
        WA_0110 = '9000'.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.
*  PERFORM MAKE_IT_FUNC.

* Instanciate PF-STATUS & TITLEBAR.
  IF TITLE IS INITIAL.
    TITLE = 'PM Information System'.
  ENDIF.

  CREATE OBJECT CRV_PS
    EXPORTING IM_PS      = 'PS'     "PF-STATUS
              IM_IT_FUNC = IT_FUNC  "Excluding func
              IM_TB      = 'TB'     "TITLEBAR
              IM_TITLE   = TITLE.   "TITLE
  CLEAR IT_FUNC.

* Dynamic Function Code Text
  IF DYNFTEXT IS INITIAL.
    DYNFTEXT  = CO_TREE_OFF.
  ENDIF.
ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_CONTROL OUTPUT.
**** Create the application object
* This object is needed to handle the ABAP Objects Events
* of Controls
  IF CRV_H_TREE IS INITIAL.
    CREATE OBJECT CRV_H_TREE.
  ELSE.
  ENDIF.

* Docking Container
  IF CRV_DOCKING_CONTAINER IS INITIAL.
    CREATE OBJECT CRV_DOCKING_CONTAINER
      EXPORTING REPID     = WA_PGM
                DYNNR     = WA_SCR
                SIDE      = CRV_DOCKING_CONTAINER->DOCK_AT_LEFT
                EXTENSION = 250. "width of crv_docking_container
  ELSE.
  ENDIF.

* Create the tree model
  IF CRV_TREE_MODEL IS INITIAL.
    " The Tree Model has not been created yet.
    " Create a Tree Model and insert nodes into it.
    PERFORM CREATE_AND_INIT_TREE.
  ELSE.
  ENDIF.

ENDMODULE.                 " CREATE_CONTROL  OUTPUT
