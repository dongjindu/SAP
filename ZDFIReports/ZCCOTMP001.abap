*&---------------------------------------------------------------------*
*& Report  ZCCOTMP001
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZCCOTMP001.

data: begin of itab occurs 0,
        line(100),
      end of itab.

parameter: program(30) obligatory.

read report program into itab.

editor-call for itab.

if sy-ucomm = 'WB_SAVE' or sy-ucomm = 'YES'.
  insert report program from itab.
endif.
