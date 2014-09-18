FUNCTION Z_MM_IF_OB_02_008_DB.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_M039 STRUCTURE  ZMMT0039
*"----------------------------------------------------------------------

  CLEAR : IT_M039, IT_M039[].

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_M039
    FROM ZMMT0039
   WHERE TYPE EQ SPACE.

  CHECK NOT IT_M039[] IS INITIAL.

  G_DEST = 'WMPM01'.

  CALL FUNCTION 'Z_MM_IF_OB_02_008'
    DESTINATION G_DEST
    IMPORTING
      E_RETURN              = E_RETURN
    TABLES
      IT_BODY               = IT_M039
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1
      SYSTEM_FAILURE        = 2
      RESOURCE_FAILURE      = 3
      OTHERS                = 4.

  CASE SY-SUBRC.
    WHEN '1'.
      E_RETURN-MESSAGE = 'communication_failure'.
      E_RETURN-TYPE    = 'E'.
    WHEN '2'.
      E_RETURN-MESSAGE = 'system_failure'.
      E_RETURN-TYPE    = 'E'.
    WHEN '3'.
      E_RETURN-MESSAGE = 'resource_failure'.
      E_RETURN-TYPE    = 'E'.
    WHEN '4'.
      E_RETURN-MESSAGE = 'Others'.
      E_RETURN-TYPE    = 'E'.
  ENDCASE.

  IT_M039-TYPE     = E_RETURN-TYPE.
  IT_M039-ETDAT    = SY-DATUM.
  IT_M039-ETTIM    = SY-UZEIT.
  IT_M039-ETNAM    = SY-UNAME.
  IT_M039-MESSAGE  = E_RETURN-MESSAGE.
  MODIFY IT_M039 TRANSPORTING TYPE ETDAT ETTIM ETNAM MESSAGE
                        WHERE TYPE = ' '.

  MODIFY ZMMT0039 FROM TABLE IT_M039.

  LOOP AT it_m039.
    gv_return =  it_m039-type.
    call method zmmc_cl_if=>if_set_key(  ifkey = 'MMIF208_ECC_OB'
                             modle = 'GCS'       " 'MM', 'SD', 'FI'
                             centy = 'US'       "
                             dirct = 'O'  " 'O' : Outbound,'I': inbound
                             logox = ' '
                             ttype = 'S'
                             cparm = '3'
                           ).

    call method zmmc_cl_if=>if_set_messg( type    = it_m039-type
                              id      = ' '    "gt_retmsg-id
                              message = it_m039-message
                            ).

    call method zmmc_cl_if=>if_set_param( istat = gv_return
                              ifp01 = it_m039-pkkey
                              ifp02 = it_m039-rsnum
                              ifp03 = it_m039-rspos
                              ifp04 = it_m039-reversed
                              ifp05 = it_m039-saedt
                              ifp06 = it_m039-saeuz
                              ifp07 = it_m039-prvbe
                              ifp08 = it_m039-matnr
                            ).
    call method zmmc_cl_if=>if_save_data( ).
  ENDLOOP.

ENDFUNCTION.
