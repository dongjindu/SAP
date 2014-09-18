*&-------------------------------------------------------------*
*& Report ZTSD_OSR_LOG_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTSD_OSR_LOG (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTSD_OSR_LOG_W .

***** Include TOP
INCLUDE ZTSD_OSR_LOG_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS S_ZDATE FOR ZTSD_OSR_LOG-ZDATE.
*SELECT-OPTIONS S_ZSEQ FOR ZTSD_OSR_LOG-ZSEQ.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTSD_OSR_L' NO-DISPLAY .
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
SELECT * FROM ZTSD_OSR_LOG
WHERE ZDATE IN S_ZDATE.
*AND ZSEQ IN S_ZSEQ.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
