REPORT zafi_reclaim_processing_gqrs.
*&---------------------------------------------------------------------*
*& Program: ZAFI_RECLAIM_PROCESSING_GQRS
*& Type   : Interface                                                  *
*& Author : Manju                                                      *
*& Title  : FI Reclaim Posting ( GQRS)                                 *
*&---------------------------------------------------------------------*
* Help Desk Request No  : 71UD66644                                    *
*   Requested by:       Andy Choi                                      *
*   Assigned to:                                                       *
*   Original Request #:                                                *
*   ABAP Analyst:       Manjunath Venkatesh                            *
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
*
*                                                                      *
***********************************************************************
*  Date         Developer   Request      Description
*  02/01/2007   Manju       UD1K930601   Initial Coding
*  02/12/2007   Manju       UD1K930698   Add Processing date , time *
*                                        and user
*  06/15/2007   Moon        UD1K940839   Add logic for country
*  12/20/2007   Rakesh      UD1K942469   Add Posting and deletion flags*
*                                        on screen                     *
*  05/24/2011   Valerian    UD1K951780   Prevent Vendor SBC3 to be
*                                        cleared after posting.
*  08/13/2011   Kim.YN                   Upgrade ECC6.0
*  09/14/2012   Valerian    UD1K953781   Save Reclaim Posted Data in
*                                        Custom Table after posting.
*                                        Delete Reclaim data from
*                                        Custom Table for Reversal
*                                        Process.
*  10/30/2012   Valerian    UD1K955778   Save/Delete Reclaim Posted
*                                        Data above only for Vendor
*                                        'SBC3' or Vendor 'CKDP'.
*                                        Allow Vendor SBC3 to be
*                                        cleared after posting.
***********************************************************************
*BAPI_ACC_GL_POSTING_POST
*BAPI_ACC_BILLING_POST
*BAPI_ACC_INVOICE_RECEIPT_POST
*POSTING_INTERFACE_DOCUMENT

  include zfi_common01.
  include zfi_alv01.
  include zafi_reclaim_proc_t01.
  include zfi_class01.
  include zafi_reclaim_proc_o01.
  include zafi_reclaim_proc_i01.
  include zafi_reclaim_proc_f01.


*---------------------------------------------------------------------*
* Tables
*---------------------------------------------------------------------*

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.
    WHEN 'BKUP'.
      CLEAR: l_text, l_answer.
      l_text = 'Do you really want to backup the old records ?'.

      PERFORM pop_up USING l_text
                     CHANGING l_answer.
      IF l_answer = 'J'.
        PERFORM bakup_old_records.
      ENDIF.

  ENDCASE.

INITIALIZATION.
  PERFORM init_button.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  LOOP AT s_vndr.
    CHECK s_vndr-sign EQ 'I'.
    CHECK s_vndr-option EQ 'EQ' OR s_vndr-option EQ 'BT'.
    IF 'SBC3' EQ s_vndr-low OR 'SBC3' EQ s_vndr-high.
      s_vndr = 'IEQ'.
      s_vndr-low = 'CKDP'.
      APPEND s_vndr.
    ENDIF.
  ENDLOOP.

* Select Data form RECLAIM DATA
  PERFORM select_data.

END-OF-SELECTION.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*

  IF p_post EQ 'X'.
    PERFORM fill_bapi_structures USING 'P'.
  ENDIF.

* Display Output
  CALL SCREEN 100.



*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------
