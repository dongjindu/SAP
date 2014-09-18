*&-------------------------------------------------------------*
*& Report ZTPP_PMT01TB_COL_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_PMT01TB_COL (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_PMT01TB_COL_R .

***** Include TOP
INCLUDE ZTPP_PMT01TB_COL_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPP_PMT0T' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-ZBMDL,
T_ITAB-ZCOMP,
T_ITAB-ZDATE,
T_ITAB-ZDIVI,
T_ITAB-ZEXTC,
T_ITAB-ZINTC,
T_ITAB-ZMODL,
T_ITAB-ZNATN,
T_ITAB-ZOCN,
T_ITAB-ZREGI,
T_ITAB-ZUSEE.
ENDFORM.
