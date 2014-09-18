*&-------------------------------------------------------------*
*& Report ZMMT0048_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZMMT0048 (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZMMT0048_W .

***** Include TOP
INCLUDE ZMMT0048_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_EXIDV FOR ZMMT0048-EXIDV.
*SELECT-OPTIONS S_LGORT FOR ZMMT0048-LGORT.
*SELECT-OPTIONS S_LIFNR FOR ZMMT0048-LIFNR.
*SELECT-OPTIONS S_MATNR FOR ZMMT0048-MATNR.
*SELECT-OPTIONS S_MBLNR FOR ZMMT0048-MBLNR.
*SELECT-OPTIONS S_POSNR FOR ZMMT0048-POSNR.
*SELECT-OPTIONS S_VBELN FOR ZMMT0048-VBELN.
*SELECT-OPTIONS S_ZFLAG FOR ZMMT0048-ZFLAG.
SELECT-OPTIONS S_BUDAT FOR ZMMT0048-BUDAT.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZMMT0048' NO-DISPLAY .
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
SELECT * FROM ZMMT0048
*WHERE EXIDV IN S_EXIDV
*AND LGORT IN S_LGORT
*AND LIFNR IN S_LIFNR
*AND MATNR IN S_MATNR
*AND MBLNR IN S_MBLNR
*AND POSNR IN S_POSNR
*AND VBELN IN S_VBELN
*AND ZFLAG IN S_ZFLAG.
WHERE BUDAT IN S_BUDAT.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
