*&-------------------------------------------------------------*
*& Report ZTCOU103_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTCOU103 (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTCOU103_W .

***** Include TOP
INCLUDE ZTCOU103_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_ARTNR FOR ZTCOU103-ARTNR.
SELECT-OPTIONS S_BDATJ FOR ZTCOU103-BDATJ.
*SELECT-OPTIONS S_INDX FOR ZTCOU103-INDX.
*SELECT-OPTIONS S_KALKA FOR ZTCOU103-KALKA.
*SELECT-OPTIONS S_KOKRS FOR ZTCOU103-KOKRS.
SELECT-OPTIONS S_POPER FOR ZTCOU103-POPER.
*SELECT-OPTIONS S_VER FOR ZTCOU103-VER.
*SELECT-OPTIONS S_WERKS FOR ZTCOU103-WERKS.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTCOU103' NO-DISPLAY .
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
SELECT * FROM ZTCOU103
*WHERE ARTNR IN S_ARTNR
WHERE BDATJ IN S_BDATJ
*AND INDX IN S_INDX
*AND KALKA IN S_KALKA
*AND KOKRS IN S_KOKRS
AND POPER IN S_POPER.
*AND VER IN S_VER
*AND WERKS IN S_WERKS.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
