*&-------------------------------------------------------------*
*& Report ZTPPERM_BK_R
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPPERM_BK (Read)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130521          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPPERM_BK_R .

***** Include TOP
INCLUDE ZTPPERM_BK_T .

***** Selection screen.
PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPPERM_BK' NO-DISPLAY .

***** Main login - common routine of include
PERFORM READ_PROCESS.

***** Common routine
INCLUDE ZITARCR.

***** History for each object,
***** processing required for each part defined,
FORM WRITE_PROCESS.
WRITE : / T_ITAB-EASSYID,
T_ITAB-EITEM,
T_ITAB-ERPID,
T_ITAB-RDATE,
T_ITAB-RSEQ,
T_ITAB-TSEQ.
ENDFORM.
