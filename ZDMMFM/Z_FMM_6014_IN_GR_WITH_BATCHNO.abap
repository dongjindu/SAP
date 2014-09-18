FUNCTION z_fmm_6014_in_gr_with_batchno.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      TA_ZSMM_6014_01 STRUCTURE  ZSMM_6014_01 OPTIONAL
*"----------------------------------------------------------------------
*&--------------------------------------------------------------------&*
*&  Program: zgmm_interface_in.
*&  Author:
*&  Specification: Create Goods Receipt.
*&--------------------------------------------------------------------&*
*& Date        User          Transport           Description
*& 10/28/2004  Shiva         UD1K912098       Created error log and
*&                                     GR document for the correct data.
*&--------------------------------------------------------------------&*

*/ We Use Two type of transaction
*1. If mvt type <> '311', GOODSMVT_CODE = '01'.  "GR for PO
*  wa_goodsmvt_code-gm_code = '01'.  "GR for PO
*2. If mvt type =  '311', GOODSMVT_CODE = '04'.  "Transfer posting
*  wa_goodsmvt_code-gm_code = '04'.  "Transfer posting

  data: w_lines type i,
        w_tot_cnt type i.

  PERFORM number_get_next USING    nro_nr_09     "NRO Interval
                                   nro_object    "NRO Object
                          CHANGING w_zdocno.     "App Doc No
  COMMIT WORK.
***********************************************************************
*  READ TABLE ta_zsmm_6014_01 INDEX 1.
*Issue Number : MM- , Requested by SYLEE
*Changed on 2004/09/01, by WSKIM
*---Start
*  CLEAR: w_move_type.
*  CLEAR w_flag.
*  MOVE: ta_zsmm_6014_01-move_type TO w_move_type.
*  MOVE: ta_zsmm_6014_01-gr_flag TO w_flag.
* 1. Make IT_PO_NUMBER.

  describe table ta_zsmm_6014_01 lines w_tot_cnt.

  loop at ta_zsmm_6014_01 ASSIGNING <fs_zsmm_6014_01>
           where gr_flag ne 'C'.
    move <fs_zsmm_6014_01>-ebeln to wa_po_number-po_number.
    collect wa_po_number into it_po_number.
  endloop.

*      DELETE ADJACENT DUPLICATES FROM it_po_number
*                                 COMPARING po_number.

* 2. Make it_grwithbatch_tmp with po item.
  DATA: it_grwithbatch_tmp LIKE it_grwithbatch.
  CLEAR: it_grwithbatch_tmp.
  LOOP AT it_po_number ASSIGNING <fs_po_number>.
    CLEAR: wa_po_header.
    PERFORM bapi_po_getdetail
                   TABLES it_po_items
                   USING    <fs_po_number>-po_number "'4100000297'
                   CHANGING wa_po_header.
    LOOP AT it_po_items ASSIGNING <fs_po_items>.
      MOVE-CORRESPONDING <fs_po_items> TO wa_grwithbatch.
*          MOVE-CORRESPONDING it_po_items to wa_grwithbatch.
      MOVE <fs_po_items>-unit  TO wa_grwithbatch-erfme.  "Unit
      MOVE wa_po_header-vendor TO wa_grwithbatch-lifnr.  "Vendor
      APPEND wa_grwithbatch TO it_grwithbatch_tmp.
    ENDLOOP.
  ENDLOOP.

* 3. Make it_grwithbatch with TA_ZSMM_6014_01.
  CLEAR: it_grwithbatch.
  LOOP AT ta_zsmm_6014_01 ASSIGNING <fs_zsmm_6014_01>
          where gr_flag ne 'C'.
    READ TABLE it_grwithbatch_tmp INTO wa_grwithbatch
        WITH KEY po_number = <fs_zsmm_6014_01>-ebeln
                 po_item   = <fs_zsmm_6014_01>-ebelp.
    if sy-subrc eq 0.
      move:
      <fs_zsmm_6014_01>-ebeln to wa_grwithbatch-ebeln,
      <fs_zsmm_6014_01>-ebelp to wa_grwithbatch-ebelp,
      <fs_zsmm_6014_01>-bldat to wa_grwithbatch-bldat,
      <fs_zsmm_6014_01>-budat to wa_grwithbatch-budat,
      <fs_zsmm_6014_01>-xblnr to wa_grwithbatch-xblnr,
      <fs_zsmm_6014_01>-charg to wa_grwithbatch-charg,
      <fs_zsmm_6014_01>-erfmg to wa_grwithbatch-erfmg,
      '101'                   to wa_grwithbatch-move_type,
      wa_grwithbatch-lifnr    to wa_grwithbatch-lifnr.
      append wa_grwithbatch to it_grwithbatch.
* Set goodsmvt_header
      PERFORM set_goodsmvt_header using <fs_zsmm_6014_01>-gr_flag.
* Set goodsmvt_code
      PERFORM set_goodsmvt_code using <fs_zsmm_6014_01>-gr_flag.
* Set goodsmvt_item
      PERFORM set_goodsmvt_item using <fs_zsmm_6014_01>-gr_flag.
    endif.
  ENDLOOP.

* Post(Goods Receipt (/nMIGO) )
  describe table it_grwithbatch lines w_lines.
  if w_lines > 0.
    perform migo_post tables ta_zsmm_6014_01 using w_zdocno.
  endif.

  clear w_lines.
* 1. Make it_tx_posting.
  loop at ta_zsmm_6014_01 assigning <fs_zsmm_6014_01>
                           where gr_flag eq 'C'.
    wa_tx_posting = <fs_zsmm_6014_01>.
    append wa_tx_posting to it_tx_posting.
* Set goodsmvt_header
    PERFORM set_goodsmvt_header using <fs_zsmm_6014_01>-gr_flag.
* Set goodsmvt_code
    PERFORM set_goodsmvt_code using <fs_zsmm_6014_01>-gr_flag.
* Set goodsmvt_item
    PERFORM set_goodsmvt_item using <fs_zsmm_6014_01>-gr_flag.
  endloop.
* 2. Post( Transfer Posting (/nMIGO) )
  describe table it_tx_posting lines w_lines.
  if w_lines > 0.
    PERFORM migo_post tables ta_zsmm_6014_01 using w_zdocno.
  endif.

*  IF w_move_type = '311'.  "Transfer Posting
** 1. Make it_tx_posting.
*    it_tx_posting = ta_zsmm_6014_01[].
** 2. Post( Transfer Posting (/nMIGO) )
*    PERFORM migo_post USING w_zdocno.
*
*  ELSE.      "GR for PO
** 1. Make IT_PO_NUMBER.
*    CLEAR: wa_po_number, it_po_number.
*    LOOP AT ta_zsmm_6014_01 ASSIGNING <fs_zsmm_6014_01>.
*      MOVE <fs_zsmm_6014_01>-ebeln TO wa_po_number-po_number.
*      APPEND wa_po_number TO it_po_number.
*    ENDLOOP.
*
*    DELETE ADJACENT DUPLICATES FROM it_po_number
*                               COMPARING po_number.
*
** 2. Make it_grwithbatch_tmp with po item.
*    DATA: it_grwithbatch_tmp LIKE it_grwithbatch.
*    CLEAR: it_grwithbatch_tmp.
*    LOOP AT it_po_number ASSIGNING <fs_po_number>.
*      CLEAR: wa_po_header.
*      PERFORM bapi_po_getdetail
*                     TABLES it_po_items
*                     USING    <fs_po_number>-po_number "'4100000297'
*                     CHANGING wa_po_header.
*      LOOP AT it_po_items ASSIGNING <fs_po_items>.
*        MOVE-CORRESPONDING <fs_po_items> TO wa_grwithbatch.
*        MOVE <fs_po_items>-unit  TO wa_grwithbatch-erfme.  "Unit
*        MOVE wa_po_header-vendor TO wa_grwithbatch-lifnr.  "Vendor
*        APPEND wa_grwithbatch TO it_grwithbatch_tmp.
*      ENDLOOP.
*    ENDLOOP.
*
** 3. Make it_grwithbatch with TA_ZSMM_6014_01.
*    CLEAR: it_grwithbatch.
*    LOOP AT ta_zsmm_6014_01 ASSIGNING <fs_zsmm_6014_01>.
*      READ TABLE it_grwithbatch_tmp INTO wa_grwithbatch
*          WITH KEY po_number = <fs_zsmm_6014_01>-ebeln
*                   po_item   = <fs_zsmm_6014_01>-ebelp.
*
*      MOVE:
*      <fs_zsmm_6014_01>-ebeln     TO wa_grwithbatch-ebeln,
*      <fs_zsmm_6014_01>-ebelp     TO wa_grwithbatch-ebelp,
*      <fs_zsmm_6014_01>-bldat     TO wa_grwithbatch-bldat,
*      <fs_zsmm_6014_01>-budat     TO wa_grwithbatch-budat,
*      <fs_zsmm_6014_01>-xblnr     TO wa_grwithbatch-xblnr,
*      <fs_zsmm_6014_01>-charg     TO wa_grwithbatch-charg,
*      <fs_zsmm_6014_01>-move_type TO wa_grwithbatch-move_type,
*      <fs_zsmm_6014_01>-erfmg     TO wa_grwithbatch-erfmg.
*      APPEND wa_grwithbatch TO it_grwithbatch.
*    ENDLOOP.
*
** Post(Goods Receipt (/nMIGO) )
*    PERFORM migo_post USING w_zdocno.
*
*  ENDIF.

*---------End

*/End of Added by Hakchin(20040430)


* Modify ta_zsmm_6014_01

*  LOOP AT ta_zsmm_6014_01 ASSIGNING <fs_zsmm_6014_01>.
*    MOVE wa_zsmm_6014_01-zzret TO <fs_zsmm_6014_01>-zzret.
*    MOVE wa_zsmm_6014_01-zzret TO <fs_zsmm_6014_01>-zresult.
*  ENDLOOP.

*/ End of Time stamp

****Function Module for Interface Log
*
*Where to be inserted:
* 1. Inbound: When interface table is updated after Standard BDC/BAPI
*             executed.
* 2. Outbound: After calling EAI
*
*====================================================================
*
*Function name : Z_FCA_EAI_INTERFACE_LOG
*
*Import/Export Parameter Structure : ZTCA_IF_LOG
*
*IFDOC   <= Serial No. for Log. Leave as empty
*TCODE   <= Present Transaction Code
*TOTAL   <= Total Execution number
*ZSUCC   <= Successful occurrences(number) for BDC/BAPI Processing
*ERROR   <= Failed occurrences(number) for BDC/BAPI Processing
*ERDAT   <= Created on.
*ERZET   <= Created time.
*ERNAM   <= Creator.
*AEDAT   <= Changed on.
*AEZET   <= Changed time
*AENAM   <= the person who change
  wa_ztca_if_log-tcode = 'MIGO'.    "Present Transaction Code
  wa_ztca_if_log-erdat = sy-datum.  "Created on.
  wa_ztca_if_log-erzet = sy-uname.  "Created time.
  wa_ztca_if_log-ernam = sy-uname.  "Created by.
  wa_ztca_if_log-total = w_tot_cnt. "Total Execution number
  if not w_err_cnt is initial.
    wa_ztca_if_log-zsucc = w_suc_cnt.
    wa_ztca_if_log-error = w_err_cnt.
    wa_ztca_if_log-sflag = 'F'.
  else.
    wa_ztca_if_log-zsucc = w_suc_cnt.
    wa_ztca_if_log-error = w_err_cnt.
    wa_ztca_if_log-sflag = 'S'.
  endif.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log     = wa_ztca_if_log
* IMPORTING
*   E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

**** Modify TA_ZSMM_6014_01
*  LOOP AT ta_zsmm_6014_01 ASSIGNING <fs_zsmm_6014_01>.
*    <fs_zsmm_6014_01>-mblnr   = wa_zsmm_6014_01-mblnr.
*    <fs_zsmm_6014_01>-mjahr   = wa_zsmm_6014_01-mjahr.
*    <fs_zsmm_6014_01>-zzret   = wa_zsmm_6014_01-zzret.
*    <fs_zsmm_6014_01>-zuser   = wa_zsmm_6014_01-zuser.
*    <fs_zsmm_6014_01>-zsdat   = wa_zsmm_6014_01-zsdat.
*    <fs_zsmm_6014_01>-zstim   = wa_zsmm_6014_01-zstim.
*    <fs_zsmm_6014_01>-zedat   = wa_zsmm_6014_01-zedat.
*    <fs_zsmm_6014_01>-zetim   = wa_zsmm_6014_01-zetim.
*    <fs_zsmm_6014_01>-zbdat   = wa_zsmm_6014_01-zbdat.
*    <fs_zsmm_6014_01>-zbtim   = wa_zsmm_6014_01-zbtim.
*    <fs_zsmm_6014_01>-zbnam   = wa_zsmm_6014_01-zbnam.
*    <fs_zsmm_6014_01>-zmode   = wa_zsmm_6014_01-zmode.
*    <fs_zsmm_6014_01>-zresult = wa_zsmm_6014_01-zresult.
*    <fs_zsmm_6014_01>-zmsg    = wa_zsmm_6014_01-zmsg.
*  ENDLOOP.

**** Begin of Insert Data to Log table ztmm_6014_01
  CLEAR: it_ztmm_6014_01, w_idx.
  LOOP AT ta_zsmm_6014_01 ASSIGNING <fs_zsmm_6014_01>.
    MOVE-CORRESPONDING <fs_zsmm_6014_01> TO wa_ztmm_6014_01.
    w_idx = w_idx + 1.
    MOVE w_zdocno TO wa_ztmm_6014_01-zdocno.
    MOVE w_idx    TO wa_ztmm_6014_01-serial.
    APPEND wa_ztmm_6014_01 TO it_ztmm_6014_01.
  ENDLOOP.

  INSERT ztmm_6014_01 FROM TABLE it_ztmm_6014_01.
**** End of Insert Data to Log table ztmm_6014_01

*/Begin of Added by Hakchin(20040206)
* This is an additional script for EAI Return message.
* EAI cannot recognize an internal table updated by field-symbol.
  LOOP AT ta_zsmm_6014_01.
    MODIFY ta_zsmm_6014_01.
  ENDLOOP.
*/End of Added by Hakchin(20040206)

ENDFUNCTION.
