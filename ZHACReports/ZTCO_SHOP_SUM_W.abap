*&-------------------------------------------------------------*
*& Report ZTCO_SHOP_SUM_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTCO_SHOP_SUM (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTCO_SHOP_SUM_W .

***** Include TOP
INCLUDE ZTCO_SHOP_SUM_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_AUFNR FOR ZTCO_SHOP_SUM-AUFNR.
SELECT-OPTIONS S_BDATJ FOR ZTCO_SHOP_SUM-BDATJ.
*SELECT-OPTIONS S_KOKRS FOR ZTCO_SHOP_SUM-KOKRS.
*SELECT-OPTIONS S_KSTAR FOR ZTCO_SHOP_SUM-KSTAR.
SELECT-OPTIONS S_POPER FOR ZTCO_SHOP_SUM-POPER.
*SELECT-OPTIONS S_RESOU FOR ZTCO_SHOP_SUM-RESOU.
*SELECT-OPTIONS S_TYPPS FOR ZTCO_SHOP_SUM-TYPPS.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTCOSHOPSU' NO-DISPLAY .
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
SELECT * FROM ZTCO_SHOP_SUM
*WHERE AUFNR IN S_AUFNR
WHERE BDATJ IN S_BDATJ
*AND KOKRS IN S_KOKRS
*AND KSTAR IN S_KSTAR
AND POPER IN S_POPER.
*AND RESOU IN S_RESOU
*AND TYPPS IN S_TYPPS.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
