*&-------------------------------------------------------------*
*& Report ZTPP_PMT06EA_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_PMT06EA (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_PMT06EA_R .

***** Include TOP
INCLUDE ZTPP_PMT06EA_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPP_PMT06' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-DIST,
T_ITAB-EXTC,
T_ITAB-INTC,
T_ITAB-LINE,
T_ITAB-MODL,
T_ITAB-PACK,
T_ITAB-PLNT,
T_ITAB-REGN,
T_ITAB-SERL,
T_ITAB-STDT,
T_ITAB-USEE.
ENDFORM.
