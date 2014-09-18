*&-------------------------------------------------------------*
*& Report ZTCO_SHOP_CC_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTCO_SHOP_CC (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTCO_SHOP_CC_R .

***** Include TOP
INCLUDE ZTCO_SHOP_CC_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTCOSHOPCC' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-AUFNR,
T_ITAB-BDATJ,
T_ITAB-ELEMT,
T_ITAB-KOKRS,
T_ITAB-KSTAR,
T_ITAB-POPER,
T_ITAB-RESOU,
T_ITAB-TYPPS.
ENDFORM.
