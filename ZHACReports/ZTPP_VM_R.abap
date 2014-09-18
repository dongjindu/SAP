*&-------------------------------------------------------------*
*& Report ZTPP_VM_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_VM (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00303
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_VM_R .

***** Include TOP
INCLUDE ZTPP_VM_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPP_VM' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-BODY_NO,
T_ITAB-MODEL_CODE.
ENDFORM.
