*&-------------------------------------------------------------*
*& Report ZTHRAPPUSGE_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTHRAPPUSGE (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTHRAPPUSGE_W .

***** Include TOP
INCLUDE ZTHRAPPUSGE_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_ACCOUN FOR ZTHRAPPUSGE-ACCOUNT.
*SELECT-OPTIONS S_INSTAN FOR ZTHRAPPUSGE-INSTANCE.
SELECT-OPTIONS S_LDATE FOR ZTHRAPPUSGE-LDATE.
*SELECT-OPTIONS S_TCODE FOR ZTHRAPPUSGE-TCODE.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTHRAPPUSG' NO-DISPLAY .
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
SELECT * FROM ZTHRAPPUSGE
*WHERE ACCOUNT IN S_ACCOUN
*AND INSTANCE IN S_INSTAN
*AND LDATE IN S_LDATE
*AND TCODE IN S_TCODE.
WHERE LDATE IN S_LDATE.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
