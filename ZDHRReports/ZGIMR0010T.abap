*&---------------------------------------------------------------------*
*&  Include           ZGIM0010T
*&---------------------------------------------------------------------*
TABLES : zgimt0001.

DATA : it_t001 TYPE SORTED TABLE OF t001
                        WITH UNIQUE KEY bukrs
                        WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
PARAMETERS p_datum LIKE p0001-begda DEFAULT sy-datum.
SELECT-OPTIONS s_pernr FOR zgimt0001-pernr NO INTERVALS MATCHCODE OBJECT prem.
SELECTION-SCREEN END OF BLOCK b01.
