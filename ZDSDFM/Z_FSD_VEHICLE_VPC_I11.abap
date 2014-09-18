************************************************************************
* Program Name      : Z_FSD_VEHICLE_VPC_I11
* Author            : HONG KI KIM
* Creation Date     : 2003.09.18.
* Specifications By : HONG KI KIM
* Pattern           : I/F 7-1
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Z_FSD_VEHICLE_VPC_I11
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
FUNCTION Z_FSD_VEHICLE_VPC_I11.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      ITAB STRUCTURE  ZTSD_VPC_INF_I11
*"      VAL_TABLE STRUCTURE  ZSPP_VIN_VALUE
*"----------------------------------------------------------------------
*  DATA: L_TABIX LIKE SY-TABIX.
*  DATA: L_OBJEK LIKE EQUI-EQUNR.
*
*  CLEAR: ITAB.
*  LOOP AT ITAB.
*    L_TABIX = SY-TABIX.
*
*    SELECT SINGLE OBJEK
*    INTO   ITAB-OBJEK
*    FROM   AUSP
*    WHERE  ATWRT EQ ITAB-ZFVIN.
*
*    MODIFY ITAB INDEX L_TABIX.
*    CLEAR: ITAB, L_TABIX.
*  ENDLOOP.
*
*  SORT ITAB BY ZFVIN ZRECTYPE.
*  MODIFY ZTSD_VPC_INF_I11 FROM TABLE ITAB.
*
*  IF SY-SUBRC = 0.
*    CLEAR: ITAB, VAL_TABLE.
*    REFRESH: VAL_TABLE.
*    LOOP AT ITAB.
*
*      CASE ITAB-ZRECTYPE.
*        WHEN 'Q01'.       "QUALITY CHECK
*          VAL_TABLE-ATNAM = 'P_RP20_ACTUAL_DATE'.  "CHANGE THE TEXT...!
*          VAL_TABLE-ATWRT = ITAB-ZPDATE.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'P_STATUS'.
*          VAL_TABLE-ATWRT = 'Q01'.   "CHANGE THIS STATUS CODE...!
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'P_PROBLEM_CODE'. "CHANGE THIS TEXT.!
*          VAL_TABLE-ATWRT = 'QXXXX'.
*          APPEND VAL_TABLE.
*
*      ENDCASE.
*
*      L_OBJEK = ITAB-OBJEK.
*
*      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*           EXPORTING
*                OBJECT     = L_OBJEK
*                MODE       = 'W'
*           TABLES
*                VAL_TABLE  = VAL_TABLE
*           EXCEPTIONS
*                NO_DATA    = 1
*                ERROR_MODE = 2
*                OTHERS     = 3.
*
*      CLEAR: ITAB, VAL_TABLE.
*      REFRESH: VAL_TABLE.
*    ENDLOOP.
*  ENDIF.

ENDFUNCTION.
