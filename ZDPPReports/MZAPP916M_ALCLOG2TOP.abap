*&---------------------------------------------------------------------*
*& Include MZAPP916M_ALCLOG2TOP                                        *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  SAPMZAPP916M_ALCLOG2          .

tables: ztpp_alclog2.

data: itpp_alclog2         like table of ztpp_alclog2 with header line.

data: sv_code                      like sy-ucomm,
      ok_code                      like sy-ucomm,
      wa_flag                      type c       .

controls: tc_9000 type tableview using screen 9000.
