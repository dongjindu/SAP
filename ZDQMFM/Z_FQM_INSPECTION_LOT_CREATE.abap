FUNCTION Z_FQM_INSPECTION_LOT_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_INSP_HDR) TYPE  ZTQM_INSP_HDR
*"     VALUE(I_INSP_ITEM) TYPE  ZTQM_INSP_ITEM_F
*"  EXPORTING
*"     VALUE(E_PRUEFLOS) TYPE  QPLOS
*"     VALUE(E_PRUEFLOS_MS) TYPE  QPLOS
*"     VALUE(RETURN) TYPE  BAPIRETURN
*"  TABLES
*"      T_BDC_MSG STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      ERROR_DURING_CREATE_LOT
*"      NO_SUPPORTED_MATERIAL
*"----------------------------------------------------------------------

****For Error Message
  DATA: WA_RETURN LIKE BAPIRET2 .

  DATA : LW_FLD_CNT(4) TYPE N.
  DATA : LW_FLD_NAME_V(40) TYPE C.  "/Start Date Field Name
*         LW_FLD_NAME_B(40) TYPE C.  "/End Date Field Name
  FIELD-SYMBOLS : <L_FS_V>.  "/Start Date Field Symbol
*                  <L_FS_B>.  "/End Date Field Symbol

  DATA : WA_DATUV LIKE SY-DATUM, "/Start Date of Inspection Lot
         WA_DATUB LIKE SY-DATUM. "/End Date of Inspection Lot

*-- Check Material Inspection Type and Plant
*  TABLES : QMAT. "/Inspection type - material parameters
  SELECT SINGLE *
     FROM QMAT
       WHERE ART    = I_INSP_HDR-ART
         AND MATNR  = I_INSP_ITEM-MATNR
         AND WERKS  = I_INSP_ITEM-WERKS.

  IF SY-SUBRC NE 0.
    RAISE NO_SUPPORTED_MATERIAL.
    EXIT.
  ENDIF.


  REFRESH BDC_TAB.


  DO 12 TIMES.
    LW_FLD_CNT = LW_FLD_CNT + 10.

*--      ISIR or REGULAR
    IF LW_FLD_CNT <= '0100'.
      IF LW_FLD_CNT = '0010'.
        CLEAR : WA_DATUV, WA_DATUB.
      ENDIF.

      CONCATENATE 'I_INSP_ITEM-DATUV_' LW_FLD_CNT
                                               INTO LW_FLD_NAME_V.
      ASSIGN : (LW_FLD_NAME_V) TO <L_FS_V>.

      IF  <L_FS_V> IS INITIAL.
*          CONTINUE.
      ELSEIF NOT <L_FS_V> IS INITIAL AND WA_DATUV IS INITIAL.
        WA_DATUV = <L_FS_V>.
        WA_DATUB = <L_FS_V>.
      ELSE.

        IF <L_FS_V> < WA_DATUV.
          WA_DATUV = <L_FS_V>.
        ELSEIF <L_FS_V> > WA_DATUB.
          WA_DATUB = <L_FS_V>.
        ENDIF.

      ENDIF.

*       BDC Processing
      IF     LW_FLD_CNT = '0100' AND
         NOT WA_DATUV IS INITIAL AND
         NOT WA_DATUB IS INITIAL.

        REFRESH : BDC_TAB, IT_MSG.
        CLEAR   : BDC_TAB, IT_MSG.

        PERFORM MAKE_INSP_LOT_BDC_DATA  USING I_INSP_HDR
                                              I_INSP_ITEM
                                              I_INSP_HDR-ART
                                              WA_DATUV
                                              WA_DATUB.

        PERFORM CALL_TRANSACTION_QA01.

*          T_BDC_MSG[] = IT_MSG[].
        APPEND LINES OF IT_MSG TO T_BDC_MSG.

        CLEAR IT_MSG.

        READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.

        IF SY-SUBRC = 0.
          ROLLBACK WORK.
          MOVE-CORRESPONDING IT_MSG TO RETURN.
          RAISE ERROR_DURING_CREATE_LOT.
        ENDIF.

        READ TABLE IT_MSG WITH KEY MSGTYP = 'S'
                                   MSGNR  = '100'.
        MOVE-CORRESPONDING IT_MSG TO RETURN.
        MOVE : IT_MSG-MSGV2 TO E_PRUEFLOS.
      ENDIF.

*--      MS Interior
    ELSEIF LW_FLD_CNT = '0110' OR LW_FLD_CNT = '0120'.

      IF LW_FLD_CNT = '0110'.
        CLEAR : WA_DATUV, WA_DATUB.
      ENDIF.

      IF LW_FLD_CNT = '0110'.
        MOVE : 'I_INSP_ITEM-DATUV_1010' TO LW_FLD_NAME_V.
      ELSEIF LW_FLD_CNT = '0120'.
        MOVE : 'I_INSP_ITEM-DATUV_2010' TO LW_FLD_NAME_V.
      ENDIF.

      ASSIGN : (LW_FLD_NAME_V) TO <L_FS_V>.

      IF  <L_FS_V> IS INITIAL.
*          CONTINUE.
      ELSEIF NOT <L_FS_V> IS INITIAL AND WA_DATUV IS INITIAL.
        WA_DATUV = <L_FS_V>.
        WA_DATUB = <L_FS_V>.
      ELSE.

        IF <L_FS_V> < WA_DATUV.
          WA_DATUV = <L_FS_V>.
        ELSEIF <L_FS_V> > WA_DATUB.
          WA_DATUB = <L_FS_V>.
        ENDIF.

      ENDIF.

*       BDC Processing
      IF     LW_FLD_CNT = '0120' AND
         NOT WA_DATUV IS INITIAL AND
         NOT WA_DATUB IS INITIAL.

        REFRESH : BDC_TAB, IT_MSG.
        CLEAR   : BDC_TAB, IT_MSG.

        PERFORM MAKE_INSP_LOT_BDC_DATA  USING I_INSP_HDR
                                              I_INSP_ITEM
                                              C_INSP_TYPE_MS"/8930
                                              WA_DATUV
                                              WA_DATUB.

        PERFORM CALL_TRANSACTION_QA01.

*          T_BDC_MSG[] = IT_MSG[].
        APPEND LINES OF IT_MSG TO T_BDC_MSG.

        CLEAR IT_MSG.

        READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.

        IF SY-SUBRC = 0.
          ROLLBACK WORK.
          MOVE-CORRESPONDING IT_MSG TO RETURN.
          RAISE ERROR_DURING_CREATE_LOT.
        ENDIF.

        READ TABLE IT_MSG WITH KEY MSGTYP = 'S'
                                   MSGNR  = '100'.
        MOVE-CORRESPONDING IT_MSG TO RETURN.
        MOVE : IT_MSG-MSGV2 TO E_PRUEFLOS_MS.

      ENDIF.

    ENDIF.


  ENDDO.



ENDFUNCTION.
