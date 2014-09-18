REPORT zemmpm33r_std_price_newmat NO STANDARD PAGE HEADING
                                  LINE-SIZE  120
                                  LINE-COUNT  65.
TABLES: mara.

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_werks LIKE t001w-werks DEFAULT 'P001' OBLIGATORY.
SELECT-OPTIONS: s_matnr FOR  mara-matnr NO-EXTENSION.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) uline.
SELECTION-SCREEN END   OF LINE.
PARAMETERS:     p_repro AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK bl1.

write:/ 'Replaced with ZACOU113'.
