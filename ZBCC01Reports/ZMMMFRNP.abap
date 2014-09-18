REPORT ZMMMFRNP line-size 275.

*&--------------------------------------------------------------------&*
*&  Program: ZMMMFRNP.
*&  Author: Shiva Gnanaguru
*&  Specification: Assign manufacturer id & Part number.
*&--------------------------------------------------------------------&*
*& Date        User          Transport           Description
*& 08/14/2004  Shiva         UD1K912108       initial program.
*& 09/14/2004  Shiva         UD1K912218       Create & display LOG.
*&--------------------------------------------------------------------&*

data: begin of wa_lfa1,
        lifnr like lfa1-lifnr,
      end of wa_lfa1.
data: begin of wa_mara,
        matnr like mara-matnr,
      end of wa_mara.
data: begin of wa_matmanu_info,
        matnr    like mara-matnr,
        mfrnr(5) type c,
        mfrpn(40) type c,
      end of wa_matmanu_info.
data: begin of wa_err_log,
        msgtyp like bapiret2-type,
        matnr  like mara-matnr,
        lifnr  like lfa1-lifnr,
        errmsg like bapiret2-message,
      end of wa_err_log.

data: w_lifnr like lfa1-lifnr,
      w_matnr like mara-matnr,
      w_msg   like bapiret2-message.
data: w_fname type string.

data: wa_headdata like bapimathead,
      wa_bmara     like bapi_mara,
      wa_marax    like bapi_marax,
      wa_marc     like bapi_marc,
      wa_marcx    like bapi_marcx,
      wa_return   like bapiret2,
      t_err_log like table of wa_err_log.

data: t_lfa1    like table of wa_lfa1,
      t_mara    like table of wa_mara,
      t_matmanu_info like table of wa_matmanu_info,
      t_return like table of wa_return.

parameters: p_fname like  RLGRAP-FILENAME.

w_fname = p_fname.

CALL FUNCTION 'GUI_UPLOAD'
     EXPORTING
          FILENAME                = w_fname
          HAS_FIELD_SEPARATOR     = '#'
     TABLES
          DATA_TAB                = t_matmanu_info
     EXCEPTIONS
          FILE_OPEN_ERROR         = 1
          FILE_READ_ERROR         = 2
          NO_BATCH                = 3
          GUI_REFUSE_FILETRANSFER = 4
          INVALID_TYPE            = 5
          NO_AUTHORITY            = 6
          UNKNOWN_ERROR           = 7
          BAD_DATA_FORMAT         = 8
          HEADER_NOT_ALLOWED      = 9
          SEPARATOR_NOT_ALLOWED   = 10
          HEADER_TOO_LONG         = 11
          UNKNOWN_DP_ERROR        = 12
          ACCESS_DENIED           = 13
          DP_OUT_OF_MEMORY        = 14
          DISK_FULL               = 15
          DP_TIMEOUT              = 16
          OTHERS                  = 17.

IF SY-SUBRC <> 0.
  message e004(zmm01).
  exit.
ENDIF.

select matnr from mara
             into table t_mara
             where mtart = 'ROH1' OR
                   mtart = 'ERSA' OR
                   mtart = 'NLAG'.

select lifnr from lfa1
             into table t_lfa1
             where ktokk = 'Y099'.
IF SY-SUBRC <> 0.
  message e005(zmm01).
  exit.
ENDIF.

sort: t_mara by matnr,
      t_lfa1 by lifnr.

loop at t_matmanu_info into wa_matmanu_info.
  w_matnr = wa_matmanu_info-matnr.
  read table t_mara with key matnr = w_matnr
                                     binary search
                                     transporting no fields.
  if sy-subrc ne 0.
    shift w_matnr right deleting trailing space.
    translate w_matnr using ' 0'.
    read table t_mara with key matnr = w_matnr
                                       binary search
                                       transporting no fields.
    check sy-subrc ne 0.
*    message i002(zmm01) with w_matnr.
    wa_err_log-msgtyp = 'E'.
    wa_err_log-matnr  = wa_matmanu_info-matnr.
    wa_err_log-errmsg    = 'Material doesnot exists!'.
    append wa_err_log to t_err_log.
    clear: wa_err_log.
    continue.
  endif.
  w_lifnr = wa_matmanu_info-mfrnr.
  shift w_lifnr right deleting trailing space.
  translate w_lifnr using ' 0'.
  read table t_lfa1 with key lifnr = w_lifnr
                                     binary search
                                     transporting no fields.
  if sy-subrc ne 0.
*    message i003(zmm01) with w_lifnr.
    wa_err_log-msgtyp = 'E'.
    wa_err_log-lifnr  = wa_matmanu_info-mfrnr.
    wa_err_log-errmsg = 'Vendor doesnot exists!'.
    append wa_err_log to t_err_log.
    clear: wa_err_log.
    continue.
  endif.
  clear: wa_headdata, wa_bmara, wa_marax, wa_marc, wa_marcx, wa_return.
*Header
  wa_headdata-material      = wa_matmanu_info-matnr.
  wa_headdata-ind_sector    = 'A'.
  wa_headdata-purchase_view = 'X'.
*MARA
  wa_bmara-manu_mat = wa_matmanu_info-mfrpn.
  wa_bmara-mfr_no   = w_lifnr.

  wa_marax-manu_mat = 'X'.
  wa_marax-mfr_no   = 'X'.
*MARC
  wa_marc-plant = 'P001'.

  wa_marcx-plant = 'P001'.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
       EXPORTING
            HEADDATA    = wa_headdata
            CLIENTDATA  = wa_bmara
            CLIENTDATAX = wa_marax
            PLANTDATA   = wa_marc
            PLANTDATAX  = wa_marcx
       IMPORTING
            RETURN      = wa_return.
*     TABLES
*       MATERIALDESCRIPTION        =
*       UNITSOFMEASURE             =
*       UNITSOFMEASUREX            =
*       INTERNATIONALARTNOS        =
*       MATERIALLONGTEXT           =
*       TAXCLASSIFICATIONS         =
*       RETURNMESSAGES             =
*       PRTDATA                    =
*       PRTDATAX                   =
*       EXTENSIONIN                =
*       EXTENSIONINX               =

  CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
    EXPORTING
      ID         = wa_return-id
      NUMBER     = wa_return-number
*                  LANGUAGE          = SY-LANGU
      TEXTFORMAT = 'ASC'
*                  LINKPATTERN       =
     MESSAGE_V1  = wa_return-message_v1
     MESSAGE_V2  = wa_return-message_v2
     MESSAGE_V3  = wa_return-message_v3
     MESSAGE_V4  = wa_return-message_v4
   IMPORTING
     MESSAGE     = w_msg .
*                  RETURN            =
*                TABLES
*                  TEXT              =
  wa_err_log-msgtyp = wa_return-type.
  wa_err_log-matnr  = wa_headdata-material.
  wa_err_log-errmsg = w_msg.
  append wa_err_log to t_err_log.
  clear: wa_err_log, w_msg.
endloop.

loop at t_err_log into wa_err_log.
  if not wa_err_log-lifnr is initial.
    write: / wa_err_log-lifnr, wa_err_log-errmsg.
  endif.
  if not wa_err_log-matnr is initial.
    write: / wa_err_log-matnr, wa_err_log-errmsg.
  endif.
endloop.
