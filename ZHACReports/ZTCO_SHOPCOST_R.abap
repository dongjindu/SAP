*&-------------------------------------------------------------*
*& Report ZTCO_SHOPCOST_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTCO_SHOPCOST (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTCO_SHOPCOST_R .

***** Include TOP
INCLUDE ZTCO_SHOPCOST_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTCO_SHOPC' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-KOKRS,
T_ITAB-BDATJ,
T_ITAB-POPER,
T_ITAB-KLVAR,
T_ITAB-VERSN,
T_ITAB-RECORD_TYPE,
T_ITAB-FSC_MATNR,
T_ITAB-SHOP,
T_ITAB-LLV_MATNR,
T_ITAB-TYPPS,
T_ITAB-KSTAR.
*T_ITAB-RECORD_TYPE,
*T_ITAB-SHOP,
*T_ITAB-TYPPS,
*T_ITAB-VERSN.
ENDFORM.
