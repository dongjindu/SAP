FUNCTION ZMMF_IF_PO.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_ZSMM_IF002) LIKE  ZSMM_IF002 STRUCTURE  ZSMM_IF002
*"     VALUE(I_CHECK) TYPE  CHAR1
*"  TABLES
*"      T_ITEM STRUCTURE  ZSMM_IF009
*"      T_CONDITION STRUCTURE  ZSMM_IF010
*"      T_SERVICE STRUCTURE  ZSMM_IF011
*"      E_RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
*-- i_check C: --> Create
*           R: --> Modify
*           D: --> Delete
  DATA: L_PO LIKE ZSMM_IF002,
        L_POITEM    LIKE ZSMM_IF009 OCCURS 0 WITH HEADER LINE,
        L_CONDITION LIKE ZSMM_IF010 OCCURS 0 WITH HEADER LINE,
        L_SERVICE   LIKE ZSMM_IF011 OCCURS 0 WITH HEADER LINE,
        L_DEBUG TYPE C VALUE 'N'.

  CLEAR L_PO.
  CLEAR: L_POITEM, L_POITEM[].
  CLEAR: L_CONDITION, L_CONDITION[].
  CLEAR: L_SERVICE, L_SERVICE[].
*--Modification : 11/05/2006
*-Convert net price to control decimal length.
  DATA : L_LENGTH  LIKE DFIES-DECIMALS,
         L_CUR_LEN TYPE I,
         L_SQUARE  TYPE I,
         L_DECIMAL TYPE P DECIMALS 5,
         L_CURRDEC LIKE TCURX-CURRKEY.

  DATA : IT_TCURX TYPE TABLE OF TCURX WITH HEADER LINE.
  DATA L_TABIX LIKE SY-TABIX.

** added by Furong on 106/19/2006
  DELETE ZTMM_IFPO_LOG FROM TABLE T_ITEM.
  INSERT ZTMM_IFPO_LOG FROM TABLE T_ITEM ACCEPTING DUPLICATE KEYS.
  DELETE ZTMM_IFPO_HD_LOG FROM I_ZSMM_IF002.
  INSERT ZTMM_IFPO_HD_LOG FROM I_ZSMM_IF002.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ENDIF.
** end of change
*--ABAP Memory : called by re-proceesing program
  DATA : L_FLAG.
  IMPORT L_FLAG FROM MEMORY ID 'FLAG'.
  IF L_FLAG NE 'X'.

    LOOP AT T_ITEM.
      L_TABIX = SY-TABIX.
      CALL FUNCTION 'SWA_DETERMINE_DECIMALS'
           EXPORTING
                EXPRESSION = T_ITEM-ZNETPR
           IMPORTING
                DECIMALS   = L_LENGTH.

      CLEAR IT_TCURX. REFRESH IT_TCURX.
      SELECT * INTO TABLE IT_TCURX
                            FROM TCURX.

      READ TABLE IT_TCURX WITH KEY CURRKEY = I_ZSMM_IF002-CUKY.
      IF SY-SUBRC = 0.
        IF IT_TCURX-CURRDEC = 0.
          L_CUR_LEN = 2.
        ELSE.
          L_CUR_LEN = L_CURRDEC.
        ENDIF.
      ELSEIF SY-SUBRC NE 0.
        L_CUR_LEN = 2.
      ENDIF.

      L_SQUARE  = L_LENGTH - L_CUR_LEN.
      G_PER     = L_SQUARE.

      IF L_SQUARE > 0.
        L_SQUARE  = 10 ** L_SQUARE.
        L_DECIMAL = T_ITEM-ZNETPR * L_SQUARE.

      ELSE.
        L_DECIMAL = T_ITEM-ZNETPR.
      ENDIF.

      CALL FUNCTION 'ROUND'
           EXPORTING
                DECIMALS = 2
                INPUT    = L_DECIMAL
                SIGN     = '+'
           IMPORTING
                OUTPUT   = L_DECIMAL.

      T_ITEM-NETPR = L_DECIMAL.
** Changed by Furong on 06/02/2006
      IF G_PER < 0.
        G_PER = 0.
      ENDIF.
      T_ITEM-PEINH = 10 ** G_PER.
** end of change
      MODIFY T_ITEM INDEX L_TABIX.
    ENDLOOP.
  ENDIF.
*--End Modification : 11/05/2006
*-----------------------------------------
  MOVE: I_ZSMM_IF002 TO L_PO.

  MOVE: T_ITEM[]      TO L_POITEM[].
  MOVE: T_CONDITION[] TO L_CONDITION[].
  MOVE: T_SERVICE[]   TO L_SERVICE[].

  IF I_CHECK EQ 'C'.
    CALL FUNCTION 'ZMMF_IF_PO_CREATE'
         EXPORTING
              I_ZSMM_IF002 = L_PO
         TABLES
              T_ITEM       = L_POITEM
              T_CONDITION  = L_CONDITION
              T_SERVICE    = L_SERVICE
              E_RETURN     = E_RETURN.
  ELSEIF I_CHECK EQ 'R' OR I_CHECK EQ 'D'.
    CALL FUNCTION 'ZMMF_IF_PO_CHANGE'
         EXPORTING
              I_ZSMM_IF002 = L_PO
              I_CHECK      = I_CHECK
         TABLES
              T_ITEM       = L_POITEM
              T_CONDITION  = L_CONDITION
              T_SERVICE    = L_SERVICE
              E_RETURN     = E_RETURN.
*  ELSEIF i_check EQ 'D'.
*    CALL FUNCTION 'ZMMF_IF_PO_DELETE'
*      EXPORTING
*        i_zsmm_if002 = l_po
*      TABLES
*        t_item       = l_poitem
*        t_condition  = l_condition
*        t_service    = l_service
*        e_return     = e_return.
  ENDIF.

* Begin of changes - UD1K940095
  READ TABLE E_RETURN WITH KEY TYPE = 'E'.
** Changed by Furong on 11/06/07
*   IF sy-subrc = 0 and ( sy-uname eq 'WEBM2' or
*                        l_debug  = 'Y' ).
  IF SY-SUBRC = 0.
** end of change
    PERFORM SEND_EMAIL.
  ENDIF.
* End of changes - UD1K940095
ENDFUNCTION.
