FUNCTION Z_COLLECT_PRICE_DIFF_PARALLEL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS
*"     VALUE(P_MONTH) TYPE  MONTH
*"     VALUE(P_GR) TYPE  CHAR01 OPTIONAL
*"     VALUE(P_IV) TYPE  CHAR01 OPTIONAL
*"     VALUE(P_RV) TYPE  CHAR01 OPTIONAL
*"     VALUE(P_IM) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      IT_MAT STRUCTURE  ZSCOUMAT OPTIONAL
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"      RESOURCEFAILURE
*"----------------------------------------------------------------------
* This function is intermediation function without special logic
* Just submit
  DATA GT_LOG TYPE TABLE OF ZTCOU111_LOG WITH HEADER LINE.

  LOOP AT IT_MAT.
    MOVE-CORRESPONDING IT_MAT TO GT_LOG.
    GT_LOG-REPID = SY-REPID.
    APPEND GT_LOG.
    CLEAR GT_LOG.
  ENDLOOP.

  MODIFY ZTCOU111_LOG FROM TABLE GT_LOG.

  EXPORT: IT_MAT = IT_MAT  TO  MEMORY ID 'ZMAT',
          P_BUKRS  TO  MEMORY ID 'ZBUKRS',
          P_MONTH  TO  MEMORY ID 'ZMONTH',
          P_GR     TO  MEMORY ID 'ZGR',
          P_IV     TO  MEMORY ID 'ZIV',
          P_RV     TO  MEMORY ID 'ZRV',
          P_IM     TO  MEMORY ID 'ZIM'.

  SUBMIT ZACOU111 AND RETURN.
  FREE IT_MAT.

ENDFUNCTION.
