report ZMMWHSEX line-size 250.

*&--------------------------------------------------------------------&*
*&   Program: ZMMWHSEX.
*&   Author : Shiva.
*&   Specification: Extend the view for 'ROH' material.
*&
*&--------------------------------------------------------------------&*
*& Date         User        Transport      Description
*& 08/28/2004   Shiva       UD1K9112074     initial program.
*&--------------------------------------------------------------------&*

data: begin of wa_excel_data,
       part_no(12) type c,
       plant(4)    type c,
       sloc(4)     type c,
       whno(3)     type c,
       strtype(3)  type c,
       Shop(2)     type c,
       bcycle(2)   type c,
       abc(1)      type c,
       valclas(4)  type c,
       stkrem(3)   type c,
       stkpla(3)   type c,
       strbin(9)    type c,
       end of wa_excel_data.
data t_excel_data like table of wa_excel_data.
data: wa_headdata like bapimathead,
      wa_mara like bapi_mara,
      wa_marax like bapi_marax,
      wa_marc like bapi_marc,
      wa_marcx like bapi_marcx,
      wa_mard like bapi_mard,
      wa_mardx like bapi_mardx,
      wa_mlgn  like bapi_mlgn,
      wa_mlgnx like bapi_mlgnx,
      wa_mlgt like bapi_mlgt,
      wa_mlgtx like bapi_mlgtx,
      wa_mbew  like bapi_mbew,
      wa_mbewx like bapi_mbewx,
      wa_return   like bapiret2.

data: begin of wa_ret_log,
        matnr(12) type c,
        msg  like bapiret2-message,
      end of wa_ret_log.
data: t_headdata like table of wa_headdata,
      t_mara like table of wa_mara,
      t_marax like table of wa_marax,
      t_marc like table of wa_marc,
      t_marcx like table of wa_marcx,
      t_mard like table of wa_mard,
      t_mardx like table of wa_mardx,
      t_mlgn like table of wa_mlgn,
      t_mlgnx like table of wa_mlgnx,
      t_mlgt like table of wa_mlgt,
      t_mlgtx like table of wa_mlgtx,
      t_ret_log like table of wa_ret_log.
data:  w_fname type string.

*p_fname = 'c:/Shiva/Hyundai/matl upload/matl extend2.txt'.
parameters: p_fname like  RLGRAP-FILENAME.
w_fname = p_fname.
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    FILENAME                      = w_fname
*   FILETYPE                      = 'ASC'
    HAS_FIELD_SEPARATOR           = '#'
*   HEADER_LENGTH                 = 0
*   READ_BY_LINE                  = 'X'
*   DAT_MODE                      = ' '
* IMPORTING
*   FILELENGTH                    =
*   HEADER                        =
  TABLES
    DATA_TAB                      = t_excel_data
 EXCEPTIONS
   FILE_OPEN_ERROR               = 1
   FILE_READ_ERROR               = 2
   NO_BATCH                      = 3
   GUI_REFUSE_FILETRANSFER       = 4
   INVALID_TYPE                  = 5
   NO_AUTHORITY                  = 6
   UNKNOWN_ERROR                 = 7
   BAD_DATA_FORMAT               = 8
   HEADER_NOT_ALLOWED            = 9
   SEPARATOR_NOT_ALLOWED         = 10
   HEADER_TOO_LONG               = 11
   UNKNOWN_DP_ERROR              = 12
   ACCESS_DENIED                 = 13
   DP_OUT_OF_MEMORY              = 14
   DISK_FULL                     = 15
   DP_TIMEOUT                    = 16
   OTHERS                        = 17
          .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

loop at t_excel_data into wa_excel_data.
  clear: wa_headdata,wa_mara,wa_marax,wa_marc,wa_marcx,wa_mard,wa_mardx,
         wa_mlgn,wa_mlgnx,wa_mlgt,wa_mlgtx,wa_mbew,wa_mbewx,wa_return.
*Header
  wa_headdata-material   = wa_excel_data-part_no .
  wa_headdata-ind_sector = 'A'.
  wa_headdata-matl_type  = 'ROH'.
  wa_headdata-storage_view = 'X'.
  if wa_excel_data-sloc = 'P400'.
    wa_headdata-warehouse_view = 'X'.
  endif.
  if wa_excel_data-sloc = 'P500'.
    wa_headdata-account_view = 'X'.
  endif.
*MARA
  wa_mara-temp_conds = wa_excel_data-bcycle.
  wa_mara-stor_conds = wa_excel_data-shop.

  wa_marax-temp_conds = 'X'.
  wa_marax-stor_conds = 'X'.
*MARC
  wa_marc-plant = wa_excel_data-plant.
  wa_marc-abc_id = wa_excel_data-abc.

  wa_marcx-plant = wa_excel_data-plant.
  wa_marcx-abc_id = 'X'.
*MARD
  wa_mard-plant = wa_excel_data-plant.
  wa_mard-stge_loc = wa_excel_data-sloc.

  wa_mardx-plant = wa_excel_data-plant.
  wa_mardx-stge_loc = wa_excel_data-sloc.
  if wa_excel_data-sloc = 'P400'.
*MLGN
    wa_mlgn-whse_no = wa_excel_data-whno.
    wa_mlgn-withdrawal = wa_excel_data-stkrem.
    wa_mlgn-placement = wa_excel_data-stkpla.

    wa_mlgnx-whse_no = wa_excel_data-whno.
    wa_mlgnx-withdrawal = 'X'.
    wa_mlgnx-placement = 'X'.
*MLGT
    wa_mlgt-whse_no = wa_excel_data-whno.
    wa_mlgt-stge_type = wa_excel_data-strtype.
    wa_mlgt-stge_bin = wa_excel_data-strbin.

    wa_mlgtx-whse_no = wa_excel_data-whno.
    wa_mlgtx-stge_type = wa_excel_data-strtype.
    wa_mlgtx-stge_bin = 'X'.
  endif.
  if wa_excel_data-sloc = 'P500'.
    wa_mbew-val_area = 'P001'.
    wa_mbew-val_class = wa_excel_data-valclas.
    wa_mbewx-val_area = 'P001'.
    wa_mbewx-val_class = 'X'.
  endif.
*&-----------------Extend material view.
  if wa_mard-stge_loc = 'P400'.
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        HEADDATA                  = wa_headdata
       CLIENTDATA                 = wa_mara
       CLIENTDATAX                = wa_marax
       PLANTDATA                  = wa_marc
       PLANTDATAX                 = wa_marcx
       STORAGELOCATIONDATA        = wa_mard
       STORAGELOCATIONDATAX       = wa_mardx
*     VALUATIONDATA              = wa_mbew
*     VALUATIONDATAX             = wa_mbewx
       WAREHOUSENUMBERDATA        = wa_mlgn
       WAREHOUSENUMBERDATAX       = wa_mlgnx
       STORAGETYPEDATA            = wa_mlgt
       STORAGETYPEDATAX           = wa_mlgtx
*   FLAG_ONLINE                = ' '
*   FLAG_CAD_CALL              = ' '
     IMPORTING
       RETURN                     = wa_RETURN
* TABLES
*   MATERIALDESCRIPTION        =
*   UNITSOFMEASURE             =
*   UNITSOFMEASUREX            =
*   INTERNATIONALARTNOS        =
*   MATERIALLONGTEXT           =
*   TAXCLASSIFICATIONS         =
*   RETURNMESSAGES             =
*   PRTDATA                    =
*   PRTDATAX                   =
*   EXTENSIONIN                =
*   EXTENSIONINX               =
              .
    clear wa_ret_log.
    wa_ret_log-matnr   = wa_excel_data-part_no.
    wa_ret_log-msg = wa_return-message.
    append wa_ret_log to t_ret_log.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
     EXPORTING
       WAIT          = 'X'
*     IMPORTING
*       RETURN        =
              .
  endif.

  if wa_mard-stge_loc = 'P500'.
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        HEADDATA                  = wa_headdata
       CLIENTDATA                 = wa_mara
       CLIENTDATAX                = wa_marax
       PLANTDATA                  = wa_marc
       PLANTDATAX                 = wa_marcx
       STORAGELOCATIONDATA        = wa_mard
       STORAGELOCATIONDATAX       = wa_mardx
       VALUATIONDATA              = wa_mbew
       VALUATIONDATAX             = wa_mbewx
*   FLAG_ONLINE                = ' '
*   FLAG_CAD_CALL              = ' '
     IMPORTING
       RETURN                     = wa_RETURN
* TABLES
*   MATERIALDESCRIPTION        =
*   UNITSOFMEASURE             =
*   UNITSOFMEASUREX            =
*   INTERNATIONALARTNOS        =
*   MATERIALLONGTEXT           =
*   TAXCLASSIFICATIONS         =
*   RETURNMESSAGES             =
*   PRTDATA                    =
*   PRTDATAX                   =
*   EXTENSIONIN                =
*   EXTENSIONINX               =
              .
    clear wa_ret_log.
    wa_ret_log-matnr   = wa_excel_data-part_no.
    wa_ret_log-msg = wa_return-message.
    append wa_ret_log to t_ret_log.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
     EXPORTING
       WAIT          = 'X'
*     IMPORTING
*       RETURN        =
              .

  endif.
endloop.

loop at t_ret_log into wa_ret_log.
  write: / wa_ret_log-matnr, wa_ret_log-msg.
endloop.
