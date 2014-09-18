*&-------------------------------------------------------------*
*& Report ZTMM_EAI_DEMAND_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTMM_EAI_DEMAND (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTMM_EAI_DEMAND_R .

***** Include TOP
INCLUDE ZTMM_EAI_DEMAND_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTMM_EAI_D' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-BEIKZ,
T_ITAB-BESKZ,
T_ITAB-EDATE,
T_ITAB-EDMD_TYPE,
T_ITAB-EPART_NO,
T_ITAB-LINE,
T_ITAB-MODEL_CODE,
T_ITAB-PDATE,
T_ITAB-PLNT,
T_ITAB-PTYPE,
T_ITAB-RPID.
ENDFORM.
