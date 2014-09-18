REPORT ZMMCRMID line-size 275.

*&--------------------------------------------------------------------&*
*&  Program: ZMMCRMID.
*&  Author: Shiva Gnanaguru
*&  Specification: Create vendor(s) to have manufacturer id
*&--------------------------------------------------------------------&*
*& Date        User          Transport           Description
*& 08/14/2004  Shiva         UD1K912108       initial program.
*& 09/14/2004  Shiva         UD1K912218       Create & display LOG.
*&--------------------------------------------------------------------&*
data: begin of wa_supplier_info,
        name1(66) type c,
        lifnr(4) type c,
      end of wa_supplier_info.
data: begin of wa_lfa1,
        lifnr like lfa1-lifnr,
      end of wa_lfa1.
data: begin of wa_err_log,
        msgtyp   like bapiret2-type,
        lifnr like lfa1-lifnr,
        errmsg like bapiret2-message,
      end of wa_err_log.
data: wa_bdcdata like bdcdata,
      wa_msgtab like bdcmsgcoll.
data: t_sup_info like table of wa_supplier_info,
      t_lfa1 like table of wa_lfa1,
      t_bdcdata like table of wa_bdcdata,
      t_msgtab like table of wa_msgtab,
      t_err_log like table of wa_err_log.
data: w_lifnr like lfa1-lifnr,
      w_name1(35) type c,
      w_msgnr like bapiret2-number,
      w_msg like bapiret2-message,
      w_msgv1 like bapiret2-message_v1,
      w_msgv2 like bapiret2-message_v2,
      w_msgv3 like bapiret2-message_v3,
      w_msgv4 like bapiret2-message_v4.

parameters: p_fname like RLGRAP-FILENAME.


CALL FUNCTION 'WS_UPLOAD'
 EXPORTING
   CODEPAGE                      = 'IBM'
   FILENAME                      = p_fname
   FILETYPE                      = 'ASC'
* IMPORTING
*   FILELENGTH                    =
  TABLES
    DATA_TAB                      = t_sup_info
 EXCEPTIONS
   CONVERSION_ERROR              = 1
   FILE_OPEN_ERROR               = 2
   FILE_READ_ERROR               = 3
   INVALID_TYPE                  = 4
   NO_BATCH                      = 5
   UNKNOWN_ERROR                 = 6
   INVALID_TABLE_WIDTH           = 7
   GUI_REFUSE_FILETRANSFER       = 8
   CUSTOMER_ERROR                = 9
   OTHERS                        = 10 .

IF SY-SUBRC <> 0.
  message e004(zmm01).
  exit.
ENDIF.

select lifnr from lfa1
             into table t_lfa1.
sort t_lfa1 by lifnr.
loop at t_sup_info into wa_supplier_info.
  w_lifnr = wa_supplier_info-lifnr.
  w_name1 = wa_supplier_info-name1(35).
  shift w_lifnr right deleting trailing space.
*  write w_lifnr to w_lifnr.
  translate w_lifnr using ' 0'.
  read table t_lfa1 with key lifnr = w_lifnr
                                     binary search
                                     transporting no fields.
  if sy-subrc ne 0.
    perform fill_bdcdata using 'SAPMF02K' '0107'    'X'.
    perform fill_bdcdata using 'RF02K-LIFNR' wa_supplier_info-lifnr ''.
    perform fill_bdcdata using 'RF02K-KTOKK' 'Y099' ''.
    perform fill_bdcdata using 'BDC_OKCODE'   '/0'  ''.
    perform fill_bdcdata using 'SAPMF02K' '0110'    'X'.
    perform fill_bdcdata using 'LFA1-NAME1' w_name1 ''.
    perform fill_bdcdata using 'LFA1-LAND1' 'US' ''.
    perform fill_bdcdata using 'BDC_OKCODE'   '=UPDA'  ''.

    call transaction 'MK01' using t_bdcdata
                            mode  'N'
                            messages into t_msgtab.

    loop at t_msgtab into wa_msgtab.
      clear: w_msgnr, w_msg, w_msgv1, w_msgv2, w_msgv3,
            w_msgv4, wa_err_log.
      w_msgnr = wa_msgtab-msgnr.
      w_msgv1 = wa_msgtab-msgv1.
      w_msgv2 = wa_msgtab-msgv2.
      w_msgv3 = wa_msgtab-msgv3.
      w_msgv4 = wa_msgtab-msgv4.

      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        EXPORTING
          ID                = wa_msgtab-msgid
          NUMBER            = w_msgnr
*                 LANGUAGE          = SY-LANGU
          TEXTFORMAT        = 'ASC'
*                 LINKPATTERN       =
         MESSAGE_V1        = w_msgv1
         MESSAGE_V2        = w_msgv2
         MESSAGE_V3        = w_msgv3
         MESSAGE_V4        = w_msgv4
       IMPORTING
         MESSAGE           = w_msg .
*                 RETURN            =
*               TABLES
*                 TEXT              =
      wa_err_log-lifnr  = wa_supplier_info-lifnr.
      wa_err_log-msgtyp = wa_msgtab-msgtyp.
      wa_err_log-errmsg = w_msg.
      append wa_err_log to t_err_log.
    endloop.
    clear wa_bdcdata.
    refresh: t_bdcdata,t_msgtab.
  else.
    wa_err_log-lifnr  = wa_supplier_info-lifnr.
    wa_err_log-msgtyp = 'E'.
    wa_err_log-errmsg = text-001.
    append wa_err_log to t_err_log.
    clear:w_msg, wa_err_log.
*    message i001(zmm01) with wa_supplier_info-lifnr.
  endif.
endloop.

loop at t_err_log into wa_err_log.
  write: / wa_err_log-lifnr, wa_err_log-errmsg.
endloop.

*&---------------------------------------------------------------------*
*&      Form  fill_bdcdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0016   text
*      -->P_0017   text
*      -->P_0018   text
*----------------------------------------------------------------------*
FORM fill_bdcdata USING P_FIELD1
                        P_FIELD2
                        P_FIELD3.
  CASE P_FIELD3.
    when 'X'.
      wa_bdcdata-program = p_field1.
      wa_bdcdata-dynpro  = p_field2.
      wa_bdcdata-dynbegin = 'X'.
    when space.
      wa_bdcdata-fnam = p_field1.
      wa_bdcdata-fval = p_field2.
  endcase.
  append wa_bdcdata to t_bdcdata.
  clear wa_bdcdata.
ENDFORM.                    " fill_bdcdata
