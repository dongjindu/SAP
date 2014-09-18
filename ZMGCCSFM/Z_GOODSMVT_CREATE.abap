function z_goodsmvt_create.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(GOODSMVT_HEADER) LIKE  BAPI2017_GM_HEAD_01 STRUCTURE
*"        BAPI2017_GM_HEAD_01
*"     VALUE(GOODSMVT_CODE) LIKE  BAPI2017_GM_CODE STRUCTURE
*"        BAPI2017_GM_CODE
*"  EXPORTING
*"     VALUE(GOODSMVT_HEADRET) LIKE  BAPI2017_GM_HEAD_RET STRUCTURE
*"        BAPI2017_GM_HEAD_RET
*"     VALUE(MATERIALDOCUMENT) LIKE  BAPI2017_GM_HEAD_RET-MAT_DOC
*"     VALUE(MATDOCUMENTYEAR) LIKE  BAPI2017_GM_HEAD_RET-DOC_YEAR
*"  TABLES
*"      GOODSMVT_ITEM STRUCTURE  BAPI2017_GM_ITEM_CREATE
*"      GOODSMVT_SERIALNUMBER STRUCTURE  BAPI2017_GM_SERIALNUMBER
*"       OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
  data: h_x like bapita-wait value 'X'.
  data: return_material like bapiret2 occurs 0 with header line.

  call function 'BAPI_GOODSMVT_CREATE'
       exporting
            goodsmvt_header       = goodsmvt_header
            goodsmvt_code         = goodsmvt_code
       importing
            goodsmvt_headret      = GOODSMVT_HEADRET
            materialdocument      = materialdocument
            matdocumentyear       = matdocumentyear
       tables
            goodsmvt_item         = goodsmvt_item
            goodsmvt_serialnumber = goodsmvt_serialnumber
            return                = return_material.

  loop at return_material where type <> 'E' and type <> 'A'.
    delete return_material.
  endloop.

  if return_material[] is initial.
    call function 'BAPI_TRANSACTION_COMMIT'
         exporting
              wait = h_x.
  endif.

  return[] = return_material[].

endfunction.
