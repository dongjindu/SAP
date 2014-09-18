*----------------------------------------------------------------------*
*   INCLUDE ZCPP104C_CLASSIFI_TOP                                      *
*----------------------------------------------------------------------*

TABLES: MARA,
        AUSP,
        MAKT,
        ZSPP_VIN_VALUE.

DATA: BEGIN OF IT_EXCEL OCCURS 0,
        ROW LIKE ALSMEX_TABLINE-ROW,
        COL LIKE ALSMEX_TABLINE-COL,
        VALUE LIKE ALSMEX_TABLINE-VALUE,
      END OF IT_EXCEL.

DATA: BEGIN OF IT_AUSP OCCURS 0,
        ATNAM LIKE CABN-ATNAM,
        ATWRT LIKE AUSP-ATWRT,
      END OF IT_AUSP.

DATA: IT_TABLE  LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

DATA: CANCEL,
      TMP_FILENAME LIKE RLGRAP-FILENAME,
      TMP_MASK(80),        " LIKE GLOBAL_FILEMASK_ALL.
      FIELDLN TYPE I.

FIELD-SYMBOLS: <TMP_SYM>.

************************************************************************
* SELECTION SCREEN ( SELECT-OPTIONS & PARAMETERS )                     *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_MATNR LIKE MARA-MATNR OBLIGATORY,
             P_FILE  LIKE RLGRAP-FILENAME OBLIGATORY
                     DEFAULT 'C:\'.
SELECTION-SCREEN END OF BLOCK B1.
