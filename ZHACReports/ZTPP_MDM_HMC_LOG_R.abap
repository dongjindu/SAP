*&-------------------------------------------------------------*
*& Report ZTPP_MDM_HMC_LOG_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_MDM_HMC_LOG (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_MDM_HMC_LOG_R .

***** Include TOP
INCLUDE ZTPP_MDM_HMC_LOG_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPP_MDM_L' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-COMP,
T_ITAB-PROD_DATE,
T_ITAB-VIN.
ENDFORM.
