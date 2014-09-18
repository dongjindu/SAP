*&-------------------------------------------------------------*
*& Report ZTFI_RECLAIM_DAT_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTFI_RECLAIM_DAT (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00303
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTFI_RECLAIM_DAT_W .

***** Include TOP
INCLUDE ZTFI_RECLAIM_DAT_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_CORP FOR ZTFI_RECLAIM_DAT-CORP.
*SELECT-OPTIONS S_DOEX FOR ZTFI_RECLAIM_DAT-DOEX.
*SELECT-OPTIONS S_ISSU FOR ZTFI_RECLAIM_DAT-ISSU.
*SELECT-OPTIONS S_RONO FOR ZTFI_RECLAIM_DAT-RONO.
*SELECT-OPTIONS S_SERL FOR ZTFI_RECLAIM_DAT-SERL.
*SELECT-OPTIONS S_VNDR FOR ZTFI_RECLAIM_DAT-VNDR.
SELECT-OPTIONS S_FLDT FOR ZTFI_RECLAIM_DAT-FLDT.
SELECT-OPTIONS S_TYPE FOR ZTFI_RECLAIM_DAT-TYPE NO-DISPLAY.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTFI_RECLA' NO-DISPLAY .
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
SELECT * FROM ZTFI_RECLAIM_DAT
*WHERE CORP IN S_CORP
*AND DOEX IN S_DOEX
*AND ISSU IN S_ISSU
*AND RONO IN S_RONO
*AND SERL IN S_SERL
*AND VNDR IN S_VNDR
*AND FLDT IN S_FLDT
WHERE FLDT IN S_FLDT
AND TYPE = 'S'.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
