*&-------------------------------------------------------------*
*& Report ZMMT0047_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZMMT0047 (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZMMT0047_W .

***** Include TOP
INCLUDE ZMMT0047_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_LGORT FOR ZMMT0047-LGORT.
*SELECT-OPTIONS S_MATNR FOR ZMMT0047-MATNR.
SELECT-OPTIONS S_ZCRTDT FOR ZMMT0047-ZCRTDT.
*SELECT-OPTIONS S_ZCRTIM FOR ZMMT0047-ZCRTIM.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZMMT0047' NO-DISPLAY .
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
SELECT * FROM ZMMT0047
*WHERE LGORT IN S_LGORT
*AND MATNR IN S_MATNR
WHERE ZCRTDT IN S_ZCRTDT.
*AND ZCRTIM IN S_ZCRTIM.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
