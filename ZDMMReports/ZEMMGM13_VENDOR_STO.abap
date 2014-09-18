************************************************************************
* Program Name      : ZEMMGM13_VENDOR_STO
* Author            : hj.song
* Creation Date     : 2003.11.17.
* Specifications By : hj.song
* Pattern           : Report 1-1
* Development Request No : UD1K902172
* Addl Documentation:
* Description       : Vendor stock GR and TO (inbound: HYSCO->SAP)
*                     Related FM: Z_FMM_GET_VENDOR_STOCK (IMMGM13)
*                     Related Table: ZTMM_VENDOR_STO (Source)
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.17.     hj.song          UD1K902172     Initial Coding
*
*
************************************************************************
*&---------------------------------------------------------------------*
*& Report  ZEMMGM13_VENDOR_STO                                         *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT zemmgm13_vendor_sto    "NO STANDARD PAGE HEADING LINE-SIZE 132
                              MESSAGE-ID zmmm.

*** include
INCLUDE zemmgm13_vendor_sto_top.

*** start
START-OF-SELECTION.
  PERFORM read_process.
  IF     r3  NE  'X'.
    PERFORM data_process.
  ENDIF.

*** end
END-OF-SELECTION.
*  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  read_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_process.

* processing
  IF     r1  EQ  'X'.
    SELECT * INTO TABLE it_ztmm_vendor_sto
           FROM ztmm_vendor_sto
          WHERE flag      EQ  ''
          AND   jobcount  EQ  p_count
          AND   zsdat     IN  s_zsdat.
* re-processing
  ELSEIF r2  EQ  'X'.
    SELECT * INTO TABLE it_ztmm_vendor_sto
           FROM ztmm_vendor_sto
          WHERE flag      EQ  'E'
          AND   jobcount  EQ  p_count
          AND   zsdat     IN  s_zsdat.
* display success list
  ELSEIF r3  EQ  'X'.
    SELECT * INTO TABLE it_ztmm_vendor_sto
           FROM ztmm_vendor_sto
          WHERE flag      EQ  'S'
          AND   jobcount  EQ  p_count
          AND   zsdat     IN  s_zsdat.
  ENDIF.

ENDFORM.                    " read_process
*&---------------------------------------------------------------------*
*&      Form  data_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_process.
* execute single line(from ztmm_vendor_sto)!

  CLEAR : wa_ztmm_vendor_sto.
  READ TABLE it_ztmm_vendor_sto INTO wa_ztmm_vendor_sto INDEX 1.

  IF     wa_ztmm_vendor_sto-doc_type  EQ  '101'.
* doc_type(movement type) is 101 -> GR
    PERFORM gr_process.
  ELSEIF wa_ztmm_vendor_sto-doc_type  EQ  '311'.
* doc_type(movement type) is 311 -> to
    PERFORM to_process.
  ENDIF.

ENDFORM.                    " data_process
*&---------------------------------------------------------------------*
*&      Form  write_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_process.

ENDFORM.                    " write_process
*&---------------------------------------------------------------------*
*&      Form  create_interface_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_interface_log
            USING  pa_ztca_if_log  LIKE  ztca_if_log.

*Function name : Z_FCA_EAI_INTERFACE_LOG
*  Export Parameter Structure : ZTCA_IF_LOG
*    IFDOC   <= Serial No. for Log. Leave as empty
*    TCODE   <= Present Transaction Code
*    TOTAL   <= Total Execution number
*    ZSUCC   <= Successful occurrences(number) for BDC/BAPI Processing
*    ERROR   <= Failed occurrences(number) for BDC/BAPI Processing
*    ERDAT   <= Created on.
*    ERZET   <= Created time.
*    ERNAM   <= Creator.
*    AEDAT   <= Changed on.
*    AEZET   <= Changed time
*    AENAM   <= the person who change

  pa_ztca_if_log-mandt = sy-mandt. "client
  pa_ztca_if_log-erdat = sy-datum. "Created on.
  pa_ztca_if_log-erzet = sy-uzeit. "Created time.
  pa_ztca_if_log-ernam = sy-uname. "Created by.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log     = pa_ztca_if_log
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

ENDFORM.                    " create_interface_log
*&---------------------------------------------------------------------*
*&      Form  gr_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gr_process.

  CLEAR : wa_ztca_if_log, wa_goodsmvt_headret.

* get bapi parameters
  PERFORM get_bapi_data.
* execute bapi gr
  PERFORM bapi_goodsmvt_create  TABLES   it_goodsmvt_item
                                         it_bapiret2
                                USING    wa_goodsmvt_header
                                         wa_goodsmvt_code
                                CHANGING wa_goodsmvt_headret.
* message log
  IF    wa_goodsmvt_headret-mat_doc NE ''.  "success
    LOOP AT it_ztmm_vendor_sto INTO wa_ztmm_vendor_sto.
      wa_ztmm_vendor_sto-flag    = 'S'.
      wa_ztmm_vendor_sto-zedat   = sy-datum.
      wa_ztmm_vendor_sto-zetim   = sy-uzeit.
      wa_ztmm_vendor_sto-zbdat   = sy-datum.
      wa_ztmm_vendor_sto-zbtim   = sy-uzeit.
      wa_ztmm_vendor_sto-zbnam   = sy-uname.
      wa_ztmm_vendor_sto-zmode   = 'C'.
      wa_ztmm_vendor_sto-zresult = 'S'.
      CONCATENATE 'Material doc '
                  wa_goodsmvt_headret-mat_doc
                  wa_goodsmvt_headret-doc_year
                  ' is created'
                  INTO wa_ztmm_vendor_sto-zmsg
                  SEPARATED BY space.
      MODIFY it_ztmm_vendor_sto FROM wa_ztmm_vendor_sto.
    ENDLOOP.
    wa_ztca_if_log-zsucc = '1'.  "interface success
  ELSE.                           "fail
    LOOP AT it_ztmm_vendor_sto INTO wa_ztmm_vendor_sto.
      wa_ztmm_vendor_sto-flag    = 'E'.
      wa_ztmm_vendor_sto-zedat   = sy-datum.
      wa_ztmm_vendor_sto-zetim   = sy-uzeit.
      wa_ztmm_vendor_sto-zbdat   = sy-datum.
      wa_ztmm_vendor_sto-zbtim   = sy-uzeit.
      wa_ztmm_vendor_sto-zbnam   = sy-uname.
      wa_ztmm_vendor_sto-zmode   = 'C'.
      wa_ztmm_vendor_sto-zresult = 'E'.
      READ TABLE it_bapiret2 INDEX 1.
      IF it_bapiret2-type EQ 'E'.
        wa_ztmm_vendor_sto-zmsg    = it_bapiret2-message.
      ELSE.
        wa_ztmm_vendor_sto-zmsg    = 'Material doc is not created!'.
      ENDIF.
      MODIFY it_ztmm_vendor_sto FROM wa_ztmm_vendor_sto.
    ENDLOOP.
    wa_ztca_if_log-error = '1'.  "interface error
  ENDIF.

* update ztable
  UPDATE ztmm_vendor_sto FROM TABLE it_ztmm_vendor_sto.

  CHECK sy-subrc EQ 0.
* interface log
  wa_ztca_if_log-total = '1'.    "interface total
  wa_ztca_if_log-tcode = 'MIGO'. "interface tcode
  PERFORM create_interface_log USING wa_ztca_if_log.


ENDFORM.                    " gr_process
*&---------------------------------------------------------------------*
*&      Form  get_bapi_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_bapi_data.

* get goodsmvt_header
  PERFORM get_goodsmvt_header.
* get goodsmvt_item.
  PERFORM get_goodsmvt_item.
* get goodsmvt_code
  wa_goodsmvt_code-gm_code = '01'.  "GR for PO

ENDFORM.                    " get_bapi_data
*&---------------------------------------------------------------------*
*&      Form  bapi_goodsmvt_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bapi_goodsmvt_create
      TABLES   pt_goodsmvt_item    STRUCTURE bapi2017_gm_item_create
               pt_return           STRUCTURE bapiret2
      USING    ps_goodsmvt_header  LIKE      bapi2017_gm_head_01
               ps_goodsmvt_code    LIKE      bapi2017_gm_code
      CHANGING ps_goodsmvt_headret LIKE      bapi2017_gm_head_ret.


  CLEAR: pt_return, pt_return[].
  CLEAR: ps_goodsmvt_headret.


  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header             = ps_goodsmvt_header
      goodsmvt_code               = ps_goodsmvt_code
*    TESTRUN                     = 'X'
    IMPORTING
      goodsmvt_headret            = ps_goodsmvt_headret
*   MATERIALDOCUMENT            =
*   MATDOCUMENTYEAR             =
    TABLES
      goodsmvt_item               = pt_goodsmvt_item
*   GOODSMVT_SERIALNUMBER       =
      return                      = pt_return.

  CLEAR: pt_return.
  READ TABLE pt_return WITH KEY type = 'E'.
  IF sy-subrc = 0.  "Error !
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
              .

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait          = 'X'
*     IMPORTING
*       RETURN        =    .
         .
  ENDIF.

ENDFORM.                    " bapi_goodsmvt_create
*&---------------------------------------------------------------------*
*&      Form  get_goodsmvt_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_goodsmvt_header .

  CLEAR: wa_ztmm_vendor_sto, wa_goodsmvt_header.
  READ TABLE it_ztmm_vendor_sto INTO wa_ztmm_vendor_sto INDEX 1.

  DATA : lw_budat(8).  CLEAR: lw_budat.
  PERFORM user_date_format
               USING  sy-uname
                      wa_ztmm_vendor_sto-budat  "Doc Date '99991231'
               CHANGING lw_budat.

*  wa_goodsmvt_header-pstng_date = lw_budat.
*  wa_goodsmvt_header-doc_date   = lw_budat.
  wa_goodsmvt_header-pstng_date = wa_ztmm_vendor_sto-budat.
  wa_goodsmvt_header-doc_date   = wa_ztmm_vendor_sto-budat.
*  wa_goodsmvt_header-ref_doc_no = ls_batch-xblnr.
  "Delivery Note: Vendor Invoice no.

ENDFORM.                    " get_goodsmvt_header
*&---------------------------------------------------------------------*
*&      Form  get_goodsmvt_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_goodsmvt_item.

  CLEAR : wa_ztmm_vendor_sto, wa_goodsmvt_item,
          it_goodsmvt_item[], it_goodsmvt_item.

  LOOP AT it_ztmm_vendor_sto INTO wa_ztmm_vendor_sto.
    wa_goodsmvt_item-material   = wa_ztmm_vendor_sto-matnr.
    wa_goodsmvt_item-plant      = wa_ztmm_vendor_sto-werks.
    wa_goodsmvt_item-stge_loc   = wa_ztmm_vendor_sto-lgort.
    wa_goodsmvt_item-batch      = wa_ztmm_vendor_sto-charg.   "Batch
    wa_goodsmvt_item-move_type  = wa_ztmm_vendor_sto-doc_type.
    "Movement type (Mandatory)
    wa_goodsmvt_item-vendor     = wa_ztmm_vendor_sto-lifnr.
    wa_goodsmvt_item-entry_qnt  = wa_ztmm_vendor_sto-erfmg.
    wa_goodsmvt_item-entry_uom  = wa_ztmm_vendor_sto-meins.
    wa_goodsmvt_item-po_number  = wa_ztmm_vendor_sto-ebeln.
    wa_goodsmvt_item-po_item    = wa_ztmm_vendor_sto-ebelp.
    wa_goodsmvt_item-move_stloc = wa_ztmm_vendor_sto-umlgo.
    wa_goodsmvt_item-vendrbatch = wa_ztmm_vendor_sto-lichn.
    wa_goodsmvt_item-mvt_ind    = 'B'.  "Goods receipt for PO
    APPEND wa_goodsmvt_item TO it_goodsmvt_item.
  ENDLOOP.

ENDFORM.                    " get_goodsmvt_item
*&---------------------------------------------------------------------*
*&      Form  user_date_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_date_format  USING   value(p_user)     LIKE sy-uname
                               value(p_date)     LIKE sy-datum
                      CHANGING value(p_userdate) TYPE char8.

  CLEAR: p_userdate.
  DATA: yyyy(4).  "year
  DATA: mm(2).    "day
  DATA: dd(2).    "month
  DATA: datfm LIKE usr01-datfm.  "date format

  SELECT SINGLE datfm INTO datfm
    FROM usr01
    WHERE bname = p_user.
** datfm
*1 DD.MM.YYYY
*2 MM/DD/YYYY
*3 MM-DD-YYYY
*4 YYYY.MM.DD
*5 YYYY/MM/DD
*6 YYYY-MM-DD
  yyyy = p_date+0(4).
  mm   = p_date+4(2).
  dd   = p_date+6(2).

  CASE datfm.
    WHEN 1.
      p_userdate+0(2) = dd.
      p_userdate+2(2) = mm.
      p_userdate+4(4) = yyyy.
    WHEN 2 OR 3.
      p_userdate+0(2) = mm.
      p_userdate+2(2) = dd.
      p_userdate+4(4) = yyyy.
    WHEN 4 OR 5 OR 6.
      p_userdate+0(4) = yyyy.
      p_userdate+4(2) = mm.
      p_userdate+6(2) = dd.
  ENDCASE.

ENDFORM.                    " user_date_format
*&---------------------------------------------------------------------*
*&      Form  to_process
*&---------------------------------------------------------------------*
*       GR with batchno
*----------------------------------------------------------------------*
FORM to_process.
* doc_type(movement type) is 311 -> GR with batch no
*                                         (refer to hckim' pgm)
  CLEAR : wa_ztca_if_log,
          it_ta_zsmm_6014_01[], it_ta_zsmm_6014_01.

* get gr with batchno data
  PERFORM get_gr_with_batchno_data.
* execute gr with batchno function module
  PERFORM exe_z_fmm_6014_in_gr_with_batc
                              TABLES   it_ta_zsmm_6014_01.

* message log
  LOOP AT it_ztmm_vendor_sto INTO wa_ztmm_vendor_sto.
    wa_ztmm_vendor_sto-zzret     = it_ta_zsmm_6014_01-zzret.
    wa_ztmm_vendor_sto-zuser     = it_ta_zsmm_6014_01-zuser.
    wa_ztmm_vendor_sto-zsdat     = it_ta_zsmm_6014_01-zsdat.
    wa_ztmm_vendor_sto-zstim     = it_ta_zsmm_6014_01-zstim.
    wa_ztmm_vendor_sto-zedat     = it_ta_zsmm_6014_01-zedat.
    wa_ztmm_vendor_sto-zetim     = it_ta_zsmm_6014_01-zetim.
    wa_ztmm_vendor_sto-zbdat     = it_ta_zsmm_6014_01-zbdat.
    wa_ztmm_vendor_sto-zbtim     = it_ta_zsmm_6014_01-zbtim.
    wa_ztmm_vendor_sto-zbnam     = it_ta_zsmm_6014_01-zbnam.
    wa_ztmm_vendor_sto-zmode     = it_ta_zsmm_6014_01-zmode.
    wa_ztmm_vendor_sto-zresult   = it_ta_zsmm_6014_01-zresult.
    wa_ztmm_vendor_sto-zmsg      = it_ta_zsmm_6014_01-zmsg.
    MODIFY it_ztmm_vendor_sto FROM wa_ztmm_vendor_sto.

    IF     wa_ztmm_vendor_sto-zzret  = 'E'.
      wa_ztca_if_log-error = '1'.  "interface success
    ELSEIF wa_ztmm_vendor_sto-zzret  = 'S'.
      wa_ztca_if_log-zsucc = '1'.  "interface success
    ENDIF.
  ENDLOOP.

* update ztable
  UPDATE ztmm_vendor_sto FROM TABLE it_ztmm_vendor_sto.

  CHECK sy-subrc EQ 0.
* interface log
  wa_ztca_if_log-total = '1'.    "interface total
  wa_ztca_if_log-tcode = 'MIGO'. "interface tcode
  PERFORM create_interface_log USING wa_ztca_if_log.

ENDFORM.                    " to_process
*&---------------------------------------------------------------------*
*&      Form  get_gr_with_batchno_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_gr_with_batchno_data.

  it_ta_zsmm_6014_01-ebeln        =  wa_ztmm_vendor_sto-ebeln.
  it_ta_zsmm_6014_01-ebelp        =  wa_ztmm_vendor_sto-ebelp.
  it_ta_zsmm_6014_01-bldat        =  wa_ztmm_vendor_sto-budat.
  it_ta_zsmm_6014_01-budat        =  wa_ztmm_vendor_sto-budat.
*  it_ta_zsmm_6014_01-xblnr        =  wa_ztmm_vendor_sto-
  it_ta_zsmm_6014_01-charg        =  wa_ztmm_vendor_sto-charg.
**S> 08/04/11 Paul : Field is not exist.
*  it_ta_zsmm_6014_01-move_type    =  wa_ztmm_vendor_sto-doc_type.
**E<
  it_ta_zsmm_6014_01-erfmg        =  wa_ztmm_vendor_sto-erfmg.
  it_ta_zsmm_6014_01-erfme        =  wa_ztmm_vendor_sto-meins.
  APPEND it_ta_zsmm_6014_01. CLEAR it_ta_zsmm_6014_01.

ENDFORM.                    " get_gr_with_batchno_data
*&---------------------------------------------------------------------*
*&      Form  EXE_Z_FMM_6014_IN_GR_WITH_BATC
*&---------------------------------------------------------------------*
*       execute gr with batch no f/m
*----------------------------------------------------------------------*
*      -->PT_TA_ZSMM_6014_01  exp, imp table parameter
*----------------------------------------------------------------------*
FORM exe_z_fmm_6014_in_gr_with_batc
               TABLES   pt_ta_zsmm_6014_01 STRUCTURE zsmm_6014_01.


  CALL FUNCTION 'Z_FMM_6014_IN_GR_WITH_BATCHNO'
       TABLES
            ta_zsmm_6014_01 = pt_ta_zsmm_6014_01.


ENDFORM.                    " EXE_Z_FMM_6014_IN_GR_WITH_BATC
