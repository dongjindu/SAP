*&-------------------------------------------------------------*
*& Report ZTBDIV_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTBDIV (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00303
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTBDIV_R .

***** Include TOP
INCLUDE ZTBDIV_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTBDIV' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-BELNR,
T_ITAB-BUKRS,
T_ITAB-BUZEI,
T_ITAB-DBUZEI,
T_ITAB-GJAHR,
T_ITAB-ZFBSEQ.
ENDFORM.
