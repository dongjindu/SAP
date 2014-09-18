*&-------------------------------------------------------------*
*& Report ZTPPVM_BK_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPPVM_BK (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPPVM_BK_W .

***** Include TOP
INCLUDE ZTPPVM_BK_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_K04PDA FOR ZTPPVM_BK-K04PDAT.
*SELECT-OPTIONS S_K04PLN FOR ZTPPVM_BK-K04PLN.
*SELECT-OPTIONS S_K04SER FOR ZTPPVM_BK-K04SER.
SELECT-OPTIONS S_ZEDAT FOR ZTPPVM_BK-ZEDAT.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPPVM_BK' NO-DISPLAY .
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
SELECT * FROM ZTPPVM_BK
*WHERE K04PDAT IN S_K04PDA
*AND K04PLN IN S_K04PLN
*AND K04SER IN S_K04SER
WHERE ZEDAT IN S_ZEDAT.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
