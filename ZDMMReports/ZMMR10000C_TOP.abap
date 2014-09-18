*----------------------------------------------------------------------*
*   INCLUDE ZMMR10000C_TOP                                             *
*----------------------------------------------------------------------*
* definition of tables, types, data and constants
* tables
TABLES    : zmmt0101, zmmt0102, zmmt0103, zmms0101, zmms0102.

TYPE-POOLS: slis.

DATA  : lt_items  TYPE zmmt0102 OCCURS 0 WITH HEADER LINE.

DATA : ok_code  LIKE sy-ucomm,
       gv_dynnr TYPE sy-dynnr    VALUE '2200',
       gv_chk(1)
.
* icons
INCLUDE <icon>.

DEFINE free_object .
  if not ( &1 is initial ).
    call method &1->free.
    clear &1.
  endif.
END-OF-DEFINITION.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS : so_atdat FOR zmmt0102-atdat.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t01.
PARAMETERS : rb1 RADIOBUTTON GROUP rg1 DEFAULT 'X' USER-COMMAND user1,
             rb2 RADIOBUTTON GROUP rg1.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-t01.
PARAMETERS : cher AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK b3.
