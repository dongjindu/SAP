REPORT ZZKBAUS01 .
*&---------------------------------------------------------------------*
*& Report  ZZKBAUS01                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*& List all configurable material BOMs with non configurable           *
*& header material.                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*

TABLES: mara, mast, stzu.

DATA: BEGIN OF lt_stzu OCCURS 0.
        INCLUDE STRUCTURE stzu.
DATA: END OF lt_stzu.

DATA: BEGIN OF lt_mast OCCURS 0.
        INCLUDE STRUCTURE mast.
DATA: END OF lt_mast.

DATA: counter TYPE p.

*- Select all configurable material boms
SELECT * FROM stzu INTO TABLE lt_stzu
   WHERE stlty = 'M'
   AND   kbaus = 'X'.

LOOP AT lt_stzu.

*- Select all header materials per bom number without configured
*- materials
  SELECT * FROM mast INTO TABLE lt_mast
  WHERE stlnr EQ lt_stzu-stlnr
  AND   cslty EQ ' '.

  SORT lt_mast BY matnr stlnr.
  DELETE ADJACENT DUPLICATES FROM lt_mast COMPARING matnr stlnr.

  LOOP AT lt_mast.

    SELECT SINGLE * FROM mara
    WHERE matnr EQ lt_mast-matnr.

*- List non configurable materials
    IF sy-subrc EQ 0 AND mara-kzkfg EQ ' '.
     WRITE:/ lt_mast-matnr, lt_mast-werks, lt_mast-stlan, lt_mast-stlnr.
      counter = counter + 1.
    ENDIF.

  ENDLOOP.

ENDLOOP.

WRITE:/ counter, 'non configurable header materials of configurable',
                 'material boms selected'.
