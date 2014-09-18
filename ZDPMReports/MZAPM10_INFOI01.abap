*----------------------------------------------------------------------*
*   INCLUDE MZAPM10_INFOI01                                            *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SY-DYNNR.
    WHEN '0100'.   "Main Screen
      CASE SAVE_OK_CODE.
        WHEN 'EXIT'.
*          CALL METHOD crv_docking_container->free.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          CALL METHOD CRV_DOCKING_CONTAINER->FREE.
          LEAVE TO TRANSACTION SY-TCODE.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK_CODE.
    WHEN 'BACK'.
      CASE SY-DYNNR.
        WHEN 100.
          CALL METHOD CRV_DOCKING_CONTAINER->FREE.
          LEAVE TO TRANSACTION SY-TCODE.
      ENDCASE.

    WHEN 'DYNF'.                    "Tree On/Off
      IF DYNFTEXT = CO_TREE_ON.
*    CALL METHOD crv_docking_container->set_visible( co_visible_true ).
        CALL METHOD CRV_DOCKING_CONTAINER->SET_VISIBLE
          EXPORTING
            VISIBLE           = CO_VISIBLE_TRUE
          EXCEPTIONS
            CNTL_ERROR        = 1
            CNTL_SYSTEM_ERROR = 2
            OTHERS            = 3.
        IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        DYNFTEXT = CO_TREE_OFF.   "Tree Off
      ELSE.
*    CALL METHOD crv_docking_container->set_visible( co_visible_false ).
        CALL METHOD CRV_DOCKING_CONTAINER->SET_VISIBLE
          EXPORTING
            VISIBLE           = CO_VISIBLE_FALSE
          EXCEPTIONS
            CNTL_ERROR        = 1
            CNTL_SYSTEM_ERROR = 2
            OTHERS            = 3.
        IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        DYNFTEXT = CO_TREE_ON.    "Tree On
      ENDIF.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
