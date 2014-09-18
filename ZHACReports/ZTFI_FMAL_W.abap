*&-------------------------------------------------------------*
*& Report ZTFI_FMAL_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTFI_FMAL (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00303
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTFI_FMAL_W .

***** Include TOP
INCLUDE ZTFI_FMAL_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_BELNR FOR ZTFI_FMAL-BELNR.
*SELECT-OPTIONS S_BUKRS FOR ZTFI_FMAL-BUKRS.
SELECT-OPTIONS S_DATUM FOR ZTFI_FMAL-DATUM.
SELECT-OPTIONS S_GJAHR FOR ZTFI_FMAL-GJAHR.
*SELECT-OPTIONS S_STUNR FOR ZTFI_FMAL-STUNR.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTFI_FMAL' NO-DISPLAY .
SELECTION-SCREEN SKIP 1.
PARAMETERS: COMMENT   LIKE ADMI_RUN-COMMENTS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B2.

***** Main login - common routine of include
PERFORM ARCHIVE_PROCESS.

***** Common routine
INCLUDE ZITARCW.

***** History for each object,
***** processing required for each part defined,
FORM OPEN_CURSOR_FOR_DB.
  OPEN CURSOR WITH HOLD G_CURSOR FOR
SELECT * FROM ZTFI_FMAL
*WHERE BELNR IN S_BELNR
*AND BUKRS IN S_BUKRS
*AND DATUM IN S_DATUM
*AND GJAHR IN S_GJAHR
*AND STUNR IN S_STUNR.
WHERE GJAHR IN S_GJAHR
and DATUM IN S_DATUM.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
