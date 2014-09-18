*&-------------------------------------------------------------*
*& Report ZTPP_PMT01TB_COL_D
*&-------------------------------------------------------------*
*System name         : HMI SYSTEM
*Sub system name     : ARCHIVE
*Program name        : Archiving : ZTPP_PMT01TB_COL (Delete)
*Program descrition  : Generated automatically by the ZHACR00800
*Created on   : 20130603          Created by   : T00302
*Changed on :                           Changed by    :
*Changed descrition :
*"-------------------------------------------------------------*
REPORT ZTPP_PMT01TB_COL_D .

***** Include TOP
INCLUDE ZTPP_PMT01TB_COL_T .

***** Selection screen.
PARAMETERS: TESTRUN               AS CHECKBOX,
            OBJECT    LIKE         ARCH_IDX-OBJECT
                      DEFAULT 'ZTPP_PMT0T' NO-DISPLAY .

***** Main login - common routine of include
PERFORM DELETE_PROCESS.

***** common routine
INCLUDE ZITARCD.

***** History for each object,
***** processing required for each part defined,
FORM DELETE_FROM_TABLE.
  DELETE (ARC_TABLE) FROM TABLE T_ITAB.
  COMMIT WORK.
  CLEAR : T_ITAB, T_ITAB[].
ENDFORM.
