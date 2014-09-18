FUNCTION Z_MM_IF_OB_02_002_RE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053 OPTIONAL
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0036
*"----------------------------------------------------------------------

  TABLES : MKPF.
  DATA : IT_MSEG LIKE MSEG OCCURS 0 WITH HEADER LINE.

*  CONCATENATE 'KMMG_EAI_CLNT' SY-MANDT
*              INTO G_DEST.
*  G_DEST = 'WMPM01'.
*__Paul: changed WMPM01 -> WMRM01.
  G_DEST = 'WMPM01'.

  CLEAR : IT_M036, IT_M036[].

  LOOP AT IT_BODY.
    MOVE-CORRESPONDING IT_BODY TO IT_M036.
    IT_M036-TYPE = ' '.
    APPEND IT_M036. CLEAR IT_M036.
  ENDLOOP.

  CHECK NOT IT_M036[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_02_002'
    DESTINATION G_DEST
    IMPORTING
      E_RETURN              = E_RETURN
    TABLES
      IT_BODY               = IT_M036
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

  IT_M036-TYPE     = E_RETURN-TYPE.
  IT_M036-ETDAT    = SY-DATUM.
  IT_M036-ETTIM    = SY-UZEIT.
  IT_M036-ETNAM    = SY-UNAME.
  IT_M036-MESSAGE  = E_RETURN-MESSAGE.
  MODIFY IT_M036 TRANSPORTING TYPE ETDAT ETTIM ETNAM MESSAGE
                        WHERE TYPE = ' '.

  MODIFY ZMMT0036 FROM TABLE IT_M036.

*  COMMIT WORK.

*-- Error Log -----------------------------------

  LOOP AT IT_M036.

    GV_RETURN =  IT_M036-TYPE.

    CALL METHOD ZMMC_CL_IF=>IF_SET_KEY(  IFKEY = 'MMIF202_ECC_OB'
                             MODLE = 'GCS'       " 'MM', 'SD', 'FI'
                             CENTY = 'US'       "
                             DIRCT = 'O'
                             LOGOX = ' '
                             TTYPE = 'S'
                             CPARM = '7'
                           ).

    CALL METHOD ZMMC_CL_IF=>IF_SET_MESSG( TYPE    = IT_M036-TYPE
                              ID      = ' '    "gt_retmsg-id
                              MESSAGE = IT_M036-MESSAGE
                            ).

    CALL METHOD ZMMC_CL_IF=>IF_SET_PARAM( ISTAT = GV_RETURN
                              IFP01 = IT_M036-MBLNR
                              IFP02 = IT_M036-MJAHR
                              IFP03 = IT_M036-ZEILE
                              IFP04 = IT_M036-BUDAT
                              IFP05 = IT_M036-MATNR
                              IFP06 = IT_M036-LGORT
                              IFP07 = IT_M036-LIFNR
                              IFP08 = IT_M036-EXIDV
                              IFP09 = IT_M036-MENGE
                              IFP10 = IT_M036-BWART
                              IFP11 = IT_M036-LFBNR
                              IFP12 = IT_M036-LFPOS
                            ).
    CALL METHOD ZMMC_CL_IF=>IF_SAVE_DATA( ).
  ENDLOOP.


ENDFUNCTION.
