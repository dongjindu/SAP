*&-------------------------------------------------------------*
*& Report ZMMT0032_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZMMT0032 (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZMMT0032_W .

***** Include TOP
INCLUDE ZMMT0032_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_PKKEY FOR ZMMT0032-PKKEY.
*SELECT-OPTIONS S_RSNUM FOR ZMMT0032-RSNUM.
*SELECT-OPTIONS S_RSPOS FOR ZMMT0032-RSPOS.
*SELECT-OPTIONS S_SAEDT FOR ZMMT0032-SAEDT.
*SELECT-OPTIONS S_SAEUZ FOR ZMMT0032-SAEUZ.
SELECT-OPTIONS S_BUDAT FOR ZMMT0032-BUDAT.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZMMT0032' NO-DISPLAY .
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
SELECT * FROM ZMMT0032
*WHERE PKKEY IN S_PKKEY
*AND RSNUM IN S_RSNUM
*AND RSPOS IN S_RSPOS
*AND SAEDT IN S_SAEDT
*AND SAEUZ IN S_SAEUZ.
WHERE BUDAT IN S_BUDAT.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
