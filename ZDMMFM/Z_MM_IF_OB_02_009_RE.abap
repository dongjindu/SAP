FUNCTION Z_MM_IF_OB_02_009_RE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053 OPTIONAL
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0041 OPTIONAL
*"----------------------------------------------------------------------

*  TABLES : MKPF.
  DATA : IT_MSEG LIKE MSEG OCCURS 0 WITH HEADER LINE.

*  CONCATENATE 'KMMG_EAI_CLNT' SY-MANDT
*              INTO G_DEST.
*  G_DEST = 'WMPM01'.
*__Paul: changed WMPM01 -> WMRM01.
  G_DEST = 'WMPM01'.

  CLEAR : IT_M041, IT_M041[].

  LOOP AT IT_BODY.
    MOVE-CORRESPONDING IT_BODY TO IT_M041.
    IT_M041-TYPE = ' '.
    IF NOT IT_M041-PKKEY IS INITIAL.
      IF IT_M041-ETNAM IS INITIAL.
        IT_M041-ETDAT    = SY-DATUM.
        IT_M041-ETTIM    = SY-UZEIT.
        IT_M041-ETNAM    = SY-UNAME.
      ELSE.
        IT_M041-ATDAT    = SY-DATUM.
        IT_M041-ATTIM    = SY-UZEIT.
        IT_M041-ATNAM    = SY-UNAME.
      ENDIF.
      APPEND IT_M041.
    ENDIF.
    CLEAR IT_M041.
  ENDLOOP.

  CHECK NOT IT_M041[] IS INITIAL.

** On 04/24/12 by Furong - sort by rsnum and SFGSN
  SORT IT_M041 BY RSNUM SFGSN.
** end on 04/24/12

  CALL FUNCTION 'Z_MM_IF_OB_02_009'
    DESTINATION G_DEST
    IMPORTING
      E_RETURN              = E_RETURN
    TABLES
      IT_BODY               = IT_M041
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

  IT_M041-TYPE     = E_RETURN-TYPE.
  IT_M041-MESSAGE  = E_RETURN-MESSAGE.

  MODIFY IT_M041 TRANSPORTING TYPE MESSAGE
                        WHERE TYPE = ' '.

  MODIFY ZMMT0041 FROM TABLE IT_M041.

*  COMMIT WORK.

*-- Error Log -----------------------------------

  LOOP AT IT_M041.

    GV_RETURN =  IT_M041-TYPE.

    CALL METHOD ZMMC_CL_IF=>IF_SET_KEY(  IFKEY = 'MMIF209_ECC_OB'
                             MODLE = 'GCS'       " 'MM', 'SD', 'FI'
                             CENTY = 'US'       "
                             DIRCT = 'O'
                             LOGOX = ' '
                             TTYPE = 'S'
                             CPARM = '7'
                           ).

    CALL METHOD ZMMC_CL_IF=>IF_SET_MESSG( TYPE    = IT_M041-TYPE
                              ID      = ' '    "gt_retmsg-id
                              MESSAGE = IT_M041-MESSAGE
                            ).

    CALL METHOD ZMMC_CL_IF=>IF_SET_PARAM( ISTAT = GV_RETURN
                              IFP01 = IT_M041-PKKEY
                              IFP02 = IT_M041-RSNUM
                              IFP03 = IT_M041-RSPOS
                              IFP04 = IT_M041-REVERSED
                              IFP05 = IT_M041-EXIDV
                              IFP06 = IT_M041-SAEDT
                              IFP07 = IT_M041-SAEUZ
*                              IFP08 = IT_M041-LGPRO
*                              IFP09 = IT_M041-PRVBE
*                              IFP10 = IT_M041-ZFEEDER
*                              IFP11 = IT_M041-PKBST
*                              IFP12 = IT_M041-MATNR
                            ).
    CALL METHOD ZMMC_CL_IF=>IF_SAVE_DATA( ).
  ENDLOOP.

ENDFUNCTION.
