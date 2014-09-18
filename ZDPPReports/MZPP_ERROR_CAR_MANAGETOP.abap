*&---------------------------------------------------------------------*
*& Include MZPP_APP601TOP                                              *
*&                                                                     *
*&---------------------------------------------------------------------*
PROGRAM  SAPMZPP_APP601                .

DATA: WA_CHANGE              TYPE C       ,
      WA_ANSWER              TYPE C       ,
      WA_END                 TYPE C       ,
      WA_1000                TYPE C       ,
      WA_INIT                TYPE C       .

DATA: OK_CODE                LIKE SY-UCOMM,
      SV_CODE                LIKE SY-UCOMM.

DATA: BEGIN OF IT_CAR        OCCURS 0.
        INCLUDE STRUCTURE    ZTPP_ERROR_CAR.
DATA:   MARK,
      END OF IT_CAR.

DATA: IT_DEL                 LIKE TABLE OF IT_CAR      WITH HEADER LINE,
      IT_INS                 LIKE TABLE OF IT_CAR      WITH HEADER LINE.

CONTROLS: TC_9000 TYPE TABLEVIEW USING SCREEN 9000.
