*&-------------------------------------------------------------*
*& Report ZTPPER2_BK_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPPER2_BK (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPPER2_BK_W .

***** Include TOP
INCLUDE ZTPPER2_BK_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_EASSYI FOR ZTPPER2_BK-EASSYID.
*SELECT-OPTIONS S_EITEM FOR ZTPPER2_BK-EITEM.
*SELECT-OPTIONS S_ERPID FOR ZTPPER2_BK-ERPID.
*SELECT-OPTIONS S_RDATE FOR ZTPPER2_BK-RDATE.
*SELECT-OPTIONS S_RSEQ FOR ZTPPER2_BK-RSEQ.
*SELECT-OPTIONS S_TSEQ FOR ZTPPER2_BK-TSEQ.
SELECT-OPTIONS S_ZEDAT FOR ZTPPER2_BK-ZEDAT.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPPER2_BK' NO-DISPLAY .
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
SELECT * FROM ZTPPER2_BK
*WHERE EASSYID IN S_EASSYI
*AND EITEM IN S_EITEM
*AND ERPID IN S_ERPID
*AND RDATE IN S_RDATE
*AND RSEQ IN S_RSEQ
*AND TSEQ IN S_TSEQ
WHERE ZEDAT IN S_ZEDAT.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
