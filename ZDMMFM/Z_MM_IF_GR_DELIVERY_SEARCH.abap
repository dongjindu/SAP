FUNCTION Z_MM_IF_GR_DELIVERY_SEARCH .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_LIFEX) TYPE  LIFEX OPTIONAL
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SYSUBRC
*"     VALUE(E_DBCNT) TYPE  SYDBCNT
*"     VALUE(E_MSG) TYPE  CHAR255
*"  TABLES
*"      PDA_HEADER STRUCTURE  ZMMS_PDA001 OPTIONAL
*"      PDA_ITEM STRUCTURE  ZMMS_PDA002 OPTIONAL
*"----------------------------------------------------------------------
  DATA:  L_GR(1).
  CLEAR : G_LIFEX, G_SUBRC, G_MSG, G_TYPE,
          E_SUBRC, E_DBCNT, E_MSG.

  CLEAR : G_VBELN, G_WBSTK.

  CLEAR : IT_ZMMS_PDA001, IT_ZMMS_PDA001[],
          IT_ZMMS_PDA002, IT_ZMMS_PDA002[],
          PDA_HEADER,     PDA_HEADER[],
          PDA_ITEM,       PDA_ITEM[].

*.. Data Move
  MOVE : I_LIFEX TO G_LIFEX.

*.. External Identification of Delivery Note?? ???? ???? ??
  IF G_LIFEX IS INITIAL.
    MOVE : 'E'      TO G_TYPE,
           '99'     TO E_SUBRC,
           TEXT-M16 TO E_MSG.

    PERFORM ERROR_LOG USING 'MMIF_PDA_01' 'PDA' 'US' 'I' ' ' 'S' G_TYPE
                            E_MSG ''  ''  ''.
    EXIT.
  ENDIF.

*.. Delivery?  ??  Goods Movement Status? ??
  PERFORM GET_DELIVERY_STATUS.

  IF G_VBELN IS INITIAL.
    MOVE : 'E'      TO G_TYPE,
           '99'     TO G_SUBRC.
** Changed by Furong on 07/18/2011
    CLEAR: L_GR.
    PERFORM CHECK_GR USING L_GR.
    IF L_GR = 'X'.
*.. Already processed
      PERFORM BUILD_MESSAGE USING G_MSG 'ZMMPDA' '003'
                                  '' '' '' ''.
    ELSE.
*.. No Delivery Note
      PERFORM BUILD_MESSAGE USING G_MSG 'ZMMPDA' '002'
                                  '' '' '' ''.
    ENDIF.
  ELSE.
    CASE G_WBSTK.
      WHEN 'A'.
        PERFORM GET_DELIVERY_SEARCH.

      WHEN 'B' OR 'C'.
        MOVE : 'E' TO G_TYPE,
               77  TO G_SUBRC.
*.. Already processed
        PERFORM BUILD_MESSAGE USING G_MSG 'ZMMPDA' '003'
                                    '' '' '' ''.
      WHEN SPACE.
        MOVE : 'E' TO G_TYPE,
               88  TO G_SUBRC.
*.. No Delivery Note
        PERFORM BUILD_MESSAGE USING G_MSG 'ZMMPDA' '002'
                                    '' '' '' ''.
    ENDCASE.
  ENDIF.

  PERFORM ERROR_LOG USING 'MMIF_PDA_01' 'PDA' 'US' 'I' ' ' 'S'
                          G_TYPE  G_MSG
                          G_LIFEX G_LIFEX(4) G_VBELN.

  MOVE : G_SUBRC TO E_SUBRC,
         G_MSG   TO E_MSG.
  DESCRIBE TABLE IT_ZMMS_PDA002 LINES E_DBCNT.

  MOVE : IT_ZMMS_PDA001[] TO PDA_HEADER[],
         IT_ZMMS_PDA002[] TO PDA_ITEM[].

ENDFUNCTION.
