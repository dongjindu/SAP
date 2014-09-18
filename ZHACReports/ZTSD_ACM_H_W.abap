*&-------------------------------------------------------------*
*& Report ZTSD_ACM_H_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTSD_ACM_H (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00303
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTSD_ACM_H_W .

***** Include TOP
INCLUDE ZTSD_ACM_H_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_ZACLN FOR ZTSD_ACM_H-ZACLN.
*SELECT-OPTIONS S_ZCDLR FOR ZTSD_ACM_H-ZCDLR.
*SELECT-OPTIONS S_ZCDST FOR ZTSD_ACM_H-ZCDST.
*SELECT-OPTIONS S_ZCSEQ FOR ZTSD_ACM_H-ZCSEQ.
*SELECT-OPTIONS S_ZCSER FOR ZTSD_ACM_H-ZCSER.
SELECT-OPTIONS S_ZERDA FOR ZTSD_ACM_H-ZERDA.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTSD_ACM_H' NO-DISPLAY .
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
SELECT * FROM ZTSD_ACM_H
*WHERE ZACLN IN S_ZACLN
*AND ZCDLR IN S_ZCDLR
*AND ZCDST IN S_ZCDST
*AND ZCSEQ IN S_ZCSEQ
*AND ZCSER IN S_ZCSER
WHERE ZERDA IN S_ZERDA.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
