*&-------------------------------------------------------------*
*& Report ZTCO_SHOP_CC_W
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTCO_SHOP_CC (Write)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTCO_SHOP_CC_W .

***** Include TOP
INCLUDE ZTCO_SHOP_CC_T .

***** Selection screen.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS S_AUFNR FOR ZTCO_SHOP_CC-AUFNR.
SELECT-OPTIONS S_BDATJ FOR ZTCO_SHOP_CC-BDATJ.
*SELECT-OPTIONS S_ELEMT FOR ZTCO_SHOP_CC-ELEMT.
*SELECT-OPTIONS S_KOKRS FOR ZTCO_SHOP_CC-KOKRS.
*SELECT-OPTIONS S_KSTAR FOR ZTCO_SHOP_CC-KSTAR.
SELECT-OPTIONS S_POPER FOR ZTCO_SHOP_CC-POPER.
*SELECT-OPTIONS S_RESOU FOR ZTCO_SHOP_CC-RESOU.
*SELECT-OPTIONS S_TYPPS FOR ZTCO_SHOP_CC-TYPPS.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TESTRUN               AS CHECKBOX,
            CREATE    DEFAULT  'X' AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTCOSHOPCC' NO-DISPLAY .
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
SELECT * FROM ZTCO_SHOP_CC
*WHERE AUFNR IN S_AUFNR
WHERE BDATJ IN S_BDATJ
*AND ELEMT IN S_ELEMT
*AND KOKRS IN S_KOKRS
*AND KSTAR IN S_KSTAR
AND POPER IN S_POPER.
*AND RESOU IN S_RESOU
*AND TYPPS IN S_TYPPS.
ENDFORM.
FORM MAKE_ARCHIVE_OBJECT_ID.



ENDFORM.
