************************************************************************
*
** Program Name   : ZEMMPM34R_ST_PRICE_MANAGE
** Created by     : Min-su Park
** Created on     : 2003.10.16.
** Pattern        :
** Description    :  Manage Standard Price for Purchase Material
**
** Modification Logs
** Date            Developer        RequestNo      Description
** 2003.10.17.     Min-su Park    UD1K901873     Initial Coding
************************************************************************
*
**----------------------------------------------------------------------
*
**   INCLUDE ZRACO99_STD_PRCIE_MANAGE_EVENT
*
**----------------------------------------------------------------------
*
*AT SELECTION-SCREEN.
*  CLEAR   : it_analy.
*  REFRESH : it_error, it_analy.
*  REFRESH it_log  .
*
*  DELETE FROM ztmm_spe WHERE ekorg IN s_ekorg.
*  DELETE FROM ztmm_log WHERE tcode = sy-tcode.
*
*START-OF-SELECTION.
*  DATA : su TYPE i.
*
*  CASE w_mark.
*    WHEN r1.
*      PERFORM business_plan.
*    WHEN r2.
*      PERFORM period_standard.
*  ENDCASE.
*
*  PERFORM update_ztmm_spe.
*  PERFORM save_log.
**  PERFORM DISPLAY_LOG.
*
*END-OF-SELECTION.
*  DESCRIBE TABLE it_error LINES su.
*  CHECK su < -10000.
*  CALL SCREEN 100.
*
*
*
*
***---
