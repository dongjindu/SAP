*----------------------------------------------------------------------*
*   INCLUDE MZPP_APP601O01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_1000 OUTPUT.
  SET PF-STATUS 'SUB1000' .
  SET TITLEBAR  '1000'    .
  WA_CHANGE = 'X'         .
ENDMODULE.                 " STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIALIZATION  OUTPUT
*&---------------------------------------------------------------------*
MODULE INITIALIZATION OUTPUT.
  IF WA_INIT = SPACE.
     SELECT  * INTO CORRESPONDING FIELDS OF TABLE IT_CAR
       FROM ZTPP_ERROR_CAR  .
     WA_INIT = 'X'  .
  ENDIF.
ENDMODULE.                 " INITIALIZATION  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'MAIN'    .
  SET TITLEBAR  'MAIN'    .
ENDMODULE.                 " STATUS_9000  OUTPUT
