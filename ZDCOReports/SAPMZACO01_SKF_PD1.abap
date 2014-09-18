************************************************************************
* Program Name      : SAPMZACO01_SKF_PD1
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.11.05.
* Specifications By : Dong-Hwan Kim
* Pattern           : Customer 1-1
* Development Request No :  UD1K903690
* Addl Documentation:
* Description       : Create / Change Process Ratio by Cost Center
*                     call transaction 'ZCOA36'. (create)
*                     call transaction 'ZCOA34'. (change)


*
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  SAPMZACO01_SKF_PD1            .


data: okcode(4).

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

 case okcode.

   when 'NEW'.
      call transaction 'ZCOA36'.

   when 'CHAN'.
      call transaction 'ZCOA34'.

 endcase.
 clear okcode.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.

 case okcode.

    when 'EXIT'.
      leave program.
    when 'BACK'.
      leave program.

  endcase.

ENDMODULE.                 " exit_command_0100  INPUT
