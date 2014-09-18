*&-------------------------------------------------------------*
*& Report ZTPP_WOSUM2_BK_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_WOSUM2_BK (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_WOSUM2_BK_R .

***** Include TOP
INCLUDE ZTPP_WOSUM2_BK_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPP_WOSUM' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-CR_DATE,
T_ITAB-CR_MONTH,
T_ITAB-DEALER,
T_ITAB-EXTC,
T_ITAB-INTC,
T_ITAB-NATION,
T_ITAB-WO_SER.
ENDFORM.
