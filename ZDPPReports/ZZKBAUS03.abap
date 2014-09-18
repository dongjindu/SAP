REPORT ZZKBAUS03 .
*&---------------------------------------------------------------------*
*& Report  ZZKBAUS03                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*& This report can change a non configurable material BOM to a         *
*& configurable material BOM                                           *
*& if the entered material is not configurable and                     *
*&    the entered BOM (material, plant, usage) does not contain        *
*&    object dependencies, class items, class types, allocations of    *
*&    configured materials.                                            *
*&---------------------------------------------------------------------*

TABLES: mara, stpo, mast, stzu.

DATA: cnt_knobj LIKE sy-tabix.
DATA: cnt_class LIKE sy-tabix.
DATA: cnt_conf  LIKE sy-tabix.

PARAMETER: matnr LIKE mast-matnr OBLIGATORY,
           werks LIKE mast-werks,
           stlan LIKE mast-stlan,
           test TYPE xfeld DEFAULT 'X'.

DATA: g_stlnr LIKE stzu-stlnr,
      ini_cslty LIKE mast-cslty,
      ini_kbaus LIKE stzu-kbaus.

DATA: gt_mast LIKE mast OCCURS 0 WITH HEADER LINE.

SELECT SINGLE * FROM mara
   WHERE matnr EQ matnr.

IF NOT mara-kzkfg IS INITIAL.
  WRITE:/ 'Entered material is configurable!'.
  EXIT.
ENDIF.

SELECT * FROM mast UP TO 1 ROWS
   WHERE matnr EQ matnr
   AND   werks EQ werks
   AND   stlan EQ stlan.

  g_stlnr = mast-stlnr.

ENDSELECT.

IF sy-dbcnt IS INITIAL.
  WRITE:/ 'BOM', matnr, werks, stlan, 'not found!'.
  EXIT.
ENDIF.

SELECT SINGLE * FROM stzu
   WHERE stlty EQ 'M'
   AND   stlnr EQ g_stlnr.

IF sy-dbcnt IS INITIAL.
  WRITE:/ 'Data inconsistency - MAST without STZU entry!'.
  EXIT.
ENDIF.

IF stzu-kbaus IS INITIAL.
  WRITE:/ 'BOM is not configurable!'.
  EXIT.
ENDIF.

SELECT * FROM stpo
   WHERE stlty EQ stzu-stlty
   AND   stlnr EQ stzu-stlnr.

* check dependencies
  IF NOT stpo-knobj IS INITIAL.
    cnt_knobj = cnt_knobj + 1.
  ENDIF.

* check class items / class type as dependency
  IF NOT stpo-class IS INITIAL OR
     NOT stpo-klart IS INITIAL.
    cnt_class = cnt_class + 1.
  ENDIF.

ENDSELECT.

* check configurated material allocations
SELECT COUNT(*) FROM mast
   WHERE stlnr EQ g_stlnr
   AND   cslty NE ini_cslty.

cnt_conf = sy-dbcnt.

IF cnt_knobj IS INITIAL AND
   cnt_class IS INITIAL AND
   cnt_conf  IS INITIAL.

  IF test IS INITIAL.
    UPDATE stzu SET kbaus = ini_kbaus
       WHERE stlty EQ stzu-stlty
       AND   stlnr EQ stzu-stlnr.

    WRITE: / 'Now BOM', matnr, werks, stlan, 'is not configurable!'.

  ELSE.

    WRITE: / 'BOM can be changed'.

  ENDIF.

ELSE.

  WRITE: / 'BOM cannot be changed:'.

  WRITE: / 'Items with object dependencies:', cnt_knobj.
  WRITE: / 'Class items / Items with class type:', cnt_class.
  WRITE: / 'Configurated material allocations:', cnt_conf.

ENDIF.
