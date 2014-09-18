*----------------------------------------------------------------------*
*   INCLUDE MZAHR0009O01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'PS9000'.
  SET TITLEBAR '900'.
*
  w_titl1  = 'Display/Modify Monthly Basicpay for the whole employee'.
  w_titl2  = 'Create/Modify Table for Working Days'.
  w_titl3  = 'O/T Calculated by Cost Center'.
  w_titl4  = 'Attendance Bonus Payment Ration'.
  w_titl5  = 'Annual Bonus Payment Ration'.
  w_titl6  = 'Not Used Holiday Payment Ration'.
  w_titl7  = 'Employer 401K Expenses Payment Ration'.
  w_titl8  = 'The Others Expenses Payment Ration'.
  w_titl9  = 'Basic Database Creation'.
  w_titl10 = 'Special work : New cost center Add'.
  w_titl11 = 'Special work : New cost center Delete'.
  w_titla  = 'Recalculate Others'.
  w_titlb  = 'Final Data Excel Down Load'.

  IF w_zyear IS INITIAL.
    w_zyear = sy-datum(4).
  ENDIF.

ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.
  IF w_werks = '' .
    w_werks = '1010'.
  ENDIF.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
           EXPORTING container_name = g_container.
    CREATE OBJECT grid1
           EXPORTING i_parent = g_custom_container.

    PERFORM changing_field_catalog.

    REFRESH it_pcp.

    CALL METHOD grid1->set_table_for_first_display
*         EXPORTING I_STRUCTURE_NAME = 'ZSHR_PCP00'
         EXPORTING
                i_buffer_active     = ' '
                i_bypassing_buffer  = ' '
         CHANGING  it_outtab           =  it_pcp
                   it_fieldcatalog     =  it_fcat.
  ELSE.
    IF w_event = 'X'.
      CALL METHOD grid1->refresh_table_display
        EXPORTING
          i_soft_refresh = 'X'.
    ENDIF.

  ENDIF.


ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9100 OUTPUT.
*** set Title & Status
  SET PF-STATUS '9100'.
  SET TITLEBAR '9100'.
ENDMODULE.                 " STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_value OUTPUT.

  IF w_zyear = '' OR w_zvers = ''.
    CLEAR zthr_pcp02.

    SELECT SINGLE * FROM zthr_pcp02
        WHERE zmodl = '02'
          AND zgrup = '1030'
          AND zval4 <> ''.
    MOVE : zthr_pcp02-zval2 TO w_zyear,
           zthr_pcp02-zval1 TO w_zvers.
  ENDIF.

ENDMODULE.                 " INITIAL_VALUE  OUTPUT
