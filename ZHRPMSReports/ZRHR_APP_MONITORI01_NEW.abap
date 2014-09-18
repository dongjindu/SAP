*----------------------------------------------------------------------*
***INCLUDE ZRHR_APP_MORNITORI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'CANC'.
      LEAVE PROGRAM.                                        "VALERIAN
*     LEAVE TO SCREEN 0.                                    "VALERIAN

   WHEN 'BACK'.
*      LEAVE PROGRAM.                                        "VALERIAN
     LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.                                        "VALERIAN
*     LEAVE TO SCREEN 0.                                    "VALERIAN
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SELECT'.
      PERFORM get_data.                           " get data

    WHEN 'ST'.
      PERFORM set_droplist_substatus USING p_st.  " set droplist Substatus

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
