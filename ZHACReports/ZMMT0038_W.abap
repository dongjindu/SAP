*&-------------------------------------------------------------*
*& Report ZMMT0038_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZMMT0038 (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZMMT0038_W .

***** Include TOP
INCLUDE ZMMT0038_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_PKKEY FOR ZMMT0038-PKKEY.
*SELECT-OPTIONS S_REVERS FOR ZMMT0038-REVERSED.
*SELECT-OPTIONS S_RSNUM FOR ZMMT0038-RSNUM.
*SELECT-OPTIONS S_RSPOS FOR ZMMT0038-RSPOS.
SELECT-OPTIONS S_SAEDT FOR ZMMT0038-SAEDT.
*SELECT-OPTIONS S_SAEUZ FOR ZMMT0038-SAEUZ.
SELECT-OPTIONS S_TYPE FOR ZMMT0038-TYPE NO-DISPLAY.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZMMT0038' NO-DISPLAY .
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
SELECT * FROM ZMMT0038
*WHERE PKKEY IN S_PKKEY
*AND REVERSED IN S_REVERS
*AND RSNUM IN S_RSNUM
*AND RSPOS IN S_RSPOS
WHERE SAEDT IN S_SAEDT
*AND SAEUZ IN S_SAEUZ
AND TYPE = 'S'.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
