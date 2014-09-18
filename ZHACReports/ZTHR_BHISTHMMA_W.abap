*&-------------------------------------------------------------*
*& Report ZTHR_BHISTHMMA_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTHR_BHISTHMMA (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTHR_BHISTHMMA_W .

***** Include TOP
INCLUDE ZTHR_BHISTHMMA_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_BADGE FOR ZTHR_BHISTHMMA-BADGE.
SELECT-OPTIONS S_RDATE FOR ZTHR_BHISTHMMA-RDATE.
*SELECT-OPTIONS S_READER FOR ZTHR_BHISTHMMA-READERID.
*SELECT-OPTIONS S_RTIME FOR ZTHR_BHISTHMMA-RTIME.

SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTHR_BHIST' NO-DISPLAY .
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
SELECT * FROM ZTHR_BHISTHMMA
*WHERE BADGE IN S_BADGE
WHERE RDATE IN S_RDATE.
*AND READERID IN S_READER
*AND RTIME IN S_RTIME.

ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
