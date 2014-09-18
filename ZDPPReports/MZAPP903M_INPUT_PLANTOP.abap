*&---------------------------------------------------------------------*
*& Include MZAPP903M_INPUT_PLANTOP                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  sapmzapp903m_input_plan       .

DATA: BEGIN OF it_data           OCCURS 0.
        INCLUDE STRUCTURE        ztpp_input_plan.
DATA:   mark                     TYPE c         ,
        tabix                    like sy-tabix  ,
      END OF it_data.

DATA: it_temp                    LIKE TABLE OF it_data WITH HEADER LINE.
DATA: ok_code                    LIKE sy-ucomm,
      ok_code1                   LIKE sy-ucomm,
      sv_code                    LIKE sy-ucomm,
      wa_sblock                  LIKE sy-tabix,
      wa_eblock                  LIKE sy-tabix,
      wa_point                   LIKE sy-tabix,
      wa_init                    TYPE c       ,
      wa_block                   TYPE c       ,
      wa_change                  TYPE c       .

CONTROLS: tc_9000 TYPE TABLEVIEW USING SCREEN 9000.
