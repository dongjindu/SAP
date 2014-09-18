FUNCTION Z_FMM_SET_DEL_TR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_DEL_TR STRUCTURE  ZTMM_DEL_TR
*"----------------------------------------------------------------------
*DATA : TOTAL LIKE ZTCA_IF_LOG-TOTAL    ,
*       ZSUCC LIKE ZTCA_IF_LOG-ZSUCC    ,
*       TCODE LIKE SY-TCODE VALUE 'ME21',
*       I_ZTCA_IF_LOG LIKE ZTCA_IF_LOG  .
*
*DESCRIBE TABLE IT_DEL_TR LINES TOTAL.
*CHECK TOTAL <> 0.
*
* IF SY-SUBRC NE 0.
*    MESSAGE E010 WITH TCODE.
*  ENDIF.
*
*  LOOP AT IT_DEL_TR.
*    UPDATE ZTMM_DEL_TR FROM IT_DEL_TR.
*    IF SY-SUBRC <> 0.
*      INSERT ZTMM_DEL_TR FROM IT_DEL_TR.
*    ENDIF.
*    ZSUCC = ZSUCC + 1.
*    MODIFY IT_DEL_TR.
*  ENDLOOP.
*
*  I_ZTCA_IF_LOG-TCODE = TCODE.
*  I_ZTCA_IF_LOG-TOTAL = TOTAL.
*
*  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
*    EXPORTING
*        I_ZTCA_IF_LOG         = I_ZTCA_IF_LOG
*    IMPORTING
*        E_ZTCA_IF_LOG         = E_ZTCA_IF_LOG
** EXCEPTIONS
**   UPDATE_FAILED              = 1
**   NUMBER_RANGE_ERROR         = 2
**   TCODE_DOES_NOT_EXIST       = 3
**   OTHERS                     = 4
*              .
SELECT * FROM ZTMM_DEL_TR
         INTO CORRESPONDING FIELDS OF TABLE IT_DEL_TR.
ENDFUNCTION.
