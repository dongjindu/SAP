*&---------------------------------------------------------------------*
*&  Include           ZXMLUU18
*&---------------------------------------------------------------------*

DATA I_DAYS TYPE PEA_SCRDD.

if not I_ESSR-LZBIS is initial.

CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
  EXPORTING
    DATE1         = I_ESSR-LBLDT
    DATE2         = I_ESSR-LZBIS
    OUTPUT_FORMAT = '03'
  IMPORTING
*   YEARS         =
*   MONTHS        =
    DAYS          = I_DAYS.

IF SY-SUBRC <> 0.
ELSE.
  IF I_DAYS > 60.
    MESSAGE W889(CO) WITH 'Please check the period with the controlling team!!!' DISPLAY LIKE 'W'.
  ENDIF.
ENDIF.
endif.

E_EDIT = 'X'.
