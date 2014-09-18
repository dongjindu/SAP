*&-------------------------------------------------------------*
*& Report ZTPPVR_BK_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPPVR_BK (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPPVR_BK_R .

***** Include TOP
INCLUDE ZTPPVR_BK_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPPVR_BK' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-FLAG,
T_ITAB-P_BODY_SERIAL,
T_ITAB-P_MODEL,
T_ITAB-P_RP_SERIAL,
T_ITAB-P_STATUS.
ENDFORM.
