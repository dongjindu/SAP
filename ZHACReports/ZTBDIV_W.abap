*&-------------------------------------------------------------*
*& Report ZTBDIV_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTBDIV (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00303
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTBDIV_W .

***** Include TOP
INCLUDE ZTBDIV_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_BELNR FOR ZTBDIV-BELNR.
*SELECT-OPTIONS S_BUKRS FOR ZTBDIV-BUKRS.
*SELECT-OPTIONS S_BUZEI FOR ZTBDIV-BUZEI.
*SELECT-OPTIONS S_DBUZEI FOR ZTBDIV-DBUZEI.
SELECT-OPTIONS S_GJAHR FOR ZTBDIV-GJAHR.
*SELECT-OPTIONS S_ZFBSEQ FOR ZTBDIV-ZFBSEQ.
SELECT-OPTIONS S_ZFBDT FOR ZTBDIV-ZFBDT.
SELECT-OPTIONS S_ZFSETY FOR ZTBDIV-ZFSETYN NO-DISPLAY.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTBDIV' NO-DISPLAY .
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
SELECT * FROM ZTBDIV
*WHERE BELNR IN S_BELNR
*AND BUKRS IN S_BUKRS
*AND BUZEI IN S_BUZEI
*AND DBUZEI IN S_DBUZEI
*AND GJAHR IN S_GJAHR
*AND ZFBSEQ IN S_ZFBSEQ
WHERE GJAHR IN S_GJAHR
AND ZFBDT IN S_ZFBDT
AND ZFSETYN = 'X'.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
