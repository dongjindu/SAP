*&---------------------------------------------------------------------*
* SYSTEM       :                                                       *
* Subsystem    : FI                                                    *
* Program      : ZCMP_EXCEL_OLE                                        *
* Description  : EXCEL OLE                                             *
* Created On   : 09/29/2011        Created By : KIMDM                  *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  INCLUDE                                                             *
*----------------------------------------------------------------------*
INCLUDE OLE2INCL.

*----------------------------------------------------------------------*
*  TABLES                                                              *
*----------------------------------------------------------------------*
TABLES: DD03L, DD03T, DD04T.

*----------------------------------------------------------------------*
*  TYPES                                                               *
*----------------------------------------------------------------------*
*..For Excel structure assignment
type-pools kcde.

*..OLE Excel
TYPES: BEGIN OF T_OLE,
         FIELDNAME TYPE FIELDNAME,
         DDTEXT    TYPE AS4TEXT,
       END   OF T_OLE.
TYPES: T_OLETAB TYPE TABLE OF T_OLE.

*----------------------------------------------------------------------*
*  CONSTANTS                                                           *
*----------------------------------------------------------------------*
DATA  C_SEPARATOR   TYPE MARKFIELD    VALUE '#'.

*----------------------------------------------------------------------*
*  DATA                                                                *
*----------------------------------------------------------------------*
DATA: G_APPLICATION TYPE OLE2_OBJECT,
      G_WORKBOOK    TYPE OLE2_OBJECT,
      G_SHEET       TYPE OLE2_OBJECT,
      G_CELLS       TYPE OLE2_OBJECT.

DATA: G_COL         TYPE I.

*..Excel Upload Size
DATA: G_START_COL TYPE I VALUE '1',
      G_START_ROW TYPE I VALUE '2',
      G_END_COL   TYPE I VALUE '256',
      G_END_ROW   TYPE I VALUE '65536'.

*----------------------------------------------------------------------*
*  INTERNAL TABLE                                                      *
*----------------------------------------------------------------------*
DATA: GT_OLE TYPE T_OLETAB.

*----------------------------------------------------------------------*
*  MACRO
*----------------------------------------------------------------------*
*..For field assign to Excel
DEFINE APPEND_TAB.
  ADD 1 TO G_COL.

  MOVE: &2  TO &1-FIELDNAME,
        &3  TO &1-DDTEXT.
  APPEND &1.
END-OF-DEFINITION.

*======================================================================*
*&---------------------------------------------------------------------*
*&      Form  FIELD_ASSIGN_FOR_OLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIELD_ASSIGN_FOR_OLE  TABLES  ft_TAB   TYPE T_OLETAB
                           USING   fP_TABNAME G_COL.
  DATA Ls_TAB TYPE T_OLE.
  DATA Lt_DD03L TYPE TABLE OF DD03L.
  DATA Ls_DD03L TYPE DD03L.

  REFRESH ft_TAB.
  CLEAR G_COL.

  SELECT *  FROM DD03L     INTO TABLE Lt_DD03L
                          WHERE TABNAME   EQ fP_TABNAME
                            AND FIELDNAME NOT IN
                                ('MANDT', '.INCLUDE')
*                                ('MANDT', 'WAERS', 'MEINH', '.INCLUDE')
                          ORDER BY POSITION.

  LOOP AT Lt_DD03L INTO Ls_DD03L.

    ADD 1 TO G_COL.

    CLEAR Ls_TAB.
    IF Ls_DD03L-ROLLNAME IS INITIAL.
      SELECT SINGLE DDTEXT INTO Ls_TAB-DDTEXT
                           FROM DD03T
                          WHERE TABNAME    EQ fP_TABNAME
                            AND DDLANGUAGE EQ SY-LANGU
                            AND FIELDNAME  EQ Ls_DD03L-FIELDNAME.
    ELSE.
      SELECT SINGLE DDTEXT INTO Ls_TAB-DDTEXT
                           FROM DD04T
                          WHERE ROLLNAME   EQ Ls_DD03L-ROLLNAME
                            AND DDLANGUAGE EQ SY-LANGU.
    ENDIF.

    MOVE Ls_DD03L-FIELDNAME TO Ls_TAB-FIELDNAME.
    APPEND Ls_TAB TO ft_TAB.

  ENDLOOP.

ENDFORM.                    " FIELD_ASSIGN_FOR_OLE
*&---------------------------------------------------------------------*
*&      Form  show_excel_with_structure
*&---------------------------------------------------------------------*
FORM SHOW_EXCEL_WITH_STRUCTURE .
  DATA: L_VISIBLE TYPE I.

  CREATE OBJECT G_APPLICATION 'EXCEL.APPLICATION'.
  IF SY-SUBRC NE 0.
    WRITE :/ 'Error when opening excel.application', SY-MSGLI.
  ENDIF.

  SET PROPERTY OF G_APPLICATION 'VISIBLE' = 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000 WITH 'Error set property'.
  ENDIF.

** GET PROPERTY OF APPLICATION 'Visible' = L_VISIBLE.

  CALL METHOD OF G_APPLICATION 'WORKBOOKS' = G_WORKBOOK.
  PERFORM ERRORS.
  CALL METHOD OF G_WORKBOOK 'ADD'.
  PERFORM ERRORS.
  CALL METHOD OF G_APPLICATION 'WORKSHEETS' = G_SHEET
    EXPORTING
      #1 = 1.
  PERFORM ERRORS.
  CALL METHOD OF G_SHEET 'ACTIVATE'.
  PERFORM ERRORS.
  PERFORM FILL_SHEET TABLES Gt_OLE.

ENDFORM.                    " show_excel_with_structure
*&---------------------------------------------------------------------*
*&      Form  ERRORS
*&---------------------------------------------------------------------*
FORM ERRORS .
  IF SY-SUBRC NE 0.
    MESSAGE E000 WITH 'ERROR IN OLE CALL' SY-MSGLI.
  ENDIF.
ENDFORM.                    " ERRORS
*&---------------------------------------------------------------------*
*&      Form  FILL_SHEET
*&---------------------------------------------------------------------*
FORM FILL_SHEET TABLES ft_TAB TYPE T_OLETAB.
  DATA  Ls_TAB   TYPE T_OLE.
  DATA: G_ROW_MAX TYPE I VALUE 256,  ""Max column #
        G_INDEX   TYPE I.            "1&1 position Indicator

  G_INDEX = 1.
  LOOP AT ft_TAB INTO Ls_TAB.

    CALL METHOD OF G_SHEET 'CELLS' = G_CELLS
      EXPORTING
        #1 = G_INDEX.

*    SET PROPERTY OF G_CELLS 'VALUE' = Ls_TAB-DDTEXT.
    SET PROPERTY OF G_CELLS 'VALUE' = Ls_TAB-FIELDNAME.

    ADD 1 TO G_INDEX.

  ENDLOOP .

  FREE OBJECT G_APPLICATION.
ENDFORM.                    " FILL_SHEET
