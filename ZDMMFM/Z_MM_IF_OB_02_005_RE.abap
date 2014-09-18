FUNCTION Z_MM_IF_OB_02_005_RE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_MBLNR) LIKE  MKPF-MBLNR OPTIONAL
*"     VALUE(I_MJAHR) TYPE  MKPF-MJAHR OPTIONAL
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0048
*"----------------------------------------------------------------------
  DATA : L_BSTAE LIKE EKPO-BSTAE,
         L_BUDAT LIKE MKPF-BUDAT,
         L_BWART LIKE MSEG-BWART,
         L_OBJKEY LIKE VEVW-OBJKEY.

  CLEAR : IT_M048, IT_M048[].

  g_dest = 'WMPM01'.

  LOOP AT IT_BODY.
    MOVE-CORRESPONDING IT_BODY TO IT_M048.
    IT_M048-TYPE = ' '.
    APPEND IT_M048. CLEAR IT_M048.
  ENDLOOP.


  CHECK NOT IT_M048[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_02_005'
    DESTINATION G_DEST
    IMPORTING
      E_RETURN              = E_RETURN
    TABLES
      IT_BODY               = IT_M048
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

  IT_M048-TYPE     = E_RETURN-TYPE.
  IT_M048-ETDAT    = SY-DATUM.
  IT_M048-ETTIM    = SY-UZEIT.
  IT_M048-ETNAM    = SY-UNAME.
  IT_M048-MESSAGE  = E_RETURN-MESSAGE.
  MODIFY IT_M048 TRANSPORTING TYPE ETDAT ETTIM ETNAM MESSAGE
                        WHERE TYPE = ' '.

  MODIFY ZMMT0048 FROM TABLE IT_M048.

*  COMMIT WORK.

*-- Error Log -----------------------------------

  LOOP AT IT_M048.
    GV_RETURN =  IT_M048-TYPE.

    CALL METHOD ZMMC_CL_IF=>IF_SET_KEY(  IFKEY = 'MMIF205_ECC_OB'
                             MODLE = 'GCS'       " 'MM', 'SD', 'FI'
                             CENTY = 'US'       "
                             DIRCT = 'O'   "'O': Outbound ,I':Inbound
                             LOGOX = ' '
                             TTYPE = 'S'
                             CPARM = '7'
                           ).

    CALL METHOD ZMMC_CL_IF=>IF_SET_MESSG( TYPE    = IT_M048-TYPE
                              ID      = ' '    "gt_retmsg-id
                              MESSAGE = IT_M048-MESSAGE
                            ).

    CALL METHOD ZMMC_CL_IF=>IF_SET_PARAM( ISTAT = GV_RETURN
                              IFP01 = IT_M048-MBLNR
                              IFP02 = IT_M048-VBELN
                              IFP03 = IT_M048-LIFNR
                              IFP04 = IT_M048-EXIDV
                              IFP05 = IT_M048-LGORT
                              IFP06 = IT_M048-MATNR
                              IFP07 = IT_M048-ZFLAG
                            ).
    CALL METHOD ZMMC_CL_IF=>IF_SAVE_DATA( ).
  ENDLOOP.
ENDFUNCTION.
