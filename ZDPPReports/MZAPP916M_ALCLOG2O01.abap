*----------------------------------------------------------------------*
***INCLUDE MZAPP916M_ALCLOG2O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_9000 output.
  SET PF-STATUS 'MAINTAIN'.
  SET TITLEBAR 'MAINTAIN'.
endmodule.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module initialization output.
  if wa_flag = space.
     wa_flag = 'X'  .
     select * into corresponding fields of table itpp_alclog2
       from ztpp_alclog2  CLIENT SPECIFIED
      where mandt = sy-mandt.
  endif.
endmodule.                 " initialization  OUTPUT
