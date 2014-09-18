*&-------------------------------------------------------------*
*& Report ZTCO_NAFTA_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTCO_NAFTA (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTCO_NAFTA_W .

***** Include TOP
INCLUDE ZTCO_NAFTA_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_ARTNR FOR ZTCO_NAFTA-ARTNR.
SELECT-OPTIONS S_BDATJ FOR ZTCO_NAFTA-BDATJ.
*SELECT-OPTIONS S_COMPN FOR ZTCO_NAFTA-COMPN.
*SELECT-OPTIONS S_INDX FOR ZTCO_NAFTA-INDX.
*SELECT-OPTIONS S_KOKRS FOR ZTCO_NAFTA-KOKRS.
SELECT-OPTIONS S_POPER FOR ZTCO_NAFTA-POPER.
*SELECT-OPTIONS S_VERID FOR ZTCO_NAFTA-VERID.
*SELECT-OPTIONS S_WERKS FOR ZTCO_NAFTA-WERKS.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTCO_NAFTA' NO-DISPLAY .
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
SELECT * FROM ZTCO_NAFTA
*WHERE ARTNR IN S_ARTNR
WHERE BDATJ IN S_BDATJ
*AND COMPN IN S_COMPN
*AND INDX IN S_INDX
*AND KOKRS IN S_KOKRS
AND POPER IN S_POPER.
*AND VERID IN S_VERID
*AND WERKS IN S_WERKS.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
