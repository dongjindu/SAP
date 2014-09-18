FUNCTION Z_FQM_INSPECTION_LOT_CRT_V2.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_INSP_HDR) LIKE  ZTQM_QNS_HDR STRUCTURE  ZTQM_QNS_HDR
*"     VALUE(I_INSP_ITEM) LIKE  ZTQM_QNS_ITEM STRUCTURE  ZTQM_QNS_ITEM
*"     VALUE(I_INSP_IT_MS) LIKE  ZTQM_QNS_IT_MS STRUCTURE
*"        ZTQM_QNS_IT_MS
*"     VALUE(I_BDC_MODE) TYPE  TB_BDCMODE DEFAULT 'A'
*"  EXPORTING
*"     VALUE(E_PRUEFLOS) TYPE  QPLOS
*"     VALUE(E_PRUEFLOS_MS) TYPE  QPLOS
*"     VALUE(RETURN) TYPE  BAPIRET2
*"     VALUE(RETURN_MS) TYPE  BAPIRET2
*"  TABLES
*"      T_BDC_MSG STRUCTURE  BDCMSGCOLL OPTIONAL
*"      T_BDC_MSG_MS STRUCTURE  BDCMSGCOLL OPTIONAL
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

*-  "/Count available field by Inspection type
*                              ISIR - 22/17  REGULAR - 3  MS - 3
  DATA : LW_MIC_ISIR_REG_CNT TYPE I,
         LW_MIC_MS_COUNT     TYPE I   VALUE C_INSP_MS_CNT.


*-- workarea for Inspection creation : this is required for
*-- creation lot because MS type structure don't have fields
*-- (KATART_IP, CODEGR_IP, CODE_IP, LIFNR, EKORG)
*-- it's date will be transported to inspectio create routine
  DATA : WA_ISPEMS LIKE ST_ISPEMS.

  CHECK I_INSP_ITEM-I_STAT = C_CREATION.

*-- Check Material Inspection Type and Plant

  SELECT SINGLE *
     FROM QMAT
       WHERE ART    = I_INSP_HDR-ART
         AND MATNR  = I_INSP_ITEM-MATNR
         AND WERKS  = I_INSP_HDR-WERKS.

  IF SY-SUBRC NE 0.
    RAISE NO_SUPPORTED_MATERIAL.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING I_INSP_ITEM TO WA_ISPEMS.

  CASE I_INSP_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     I_INSP_HDR-WERKS+0(1) = 'P'.
        LW_MIC_ISIR_REG_CNT = C_INSP_ISP_CNT.
      ELSEIF I_INSP_HDR-WERKS+0(1) = 'E'.
        LW_MIC_ISIR_REG_CNT = C_INSP_ISE_CNT.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      LW_MIC_ISIR_REG_CNT = C_INSP_REG_CNT.
  ENDCASE.

**/// ISIR or Regular Inspection Lot
*-- Get earliest start date of MIC(ISIR or Regular) for Material
  CLEAR : WA_DATUV, WA_DATUB.
  CLEAR LW_FLD_CNT.

  DO LW_MIC_ISIR_REG_CNT TIMES.

    LW_FLD_CNT = LW_FLD_CNT + 10.

    CONCATENATE 'I_INSP_ITEM-DATUV_' LW_FLD_CNT
                                             INTO LW_FLD_NAME_V.
    ASSIGN : (LW_FLD_NAME_V) TO <L_FS_V>.

    IF  <L_FS_V> IS INITIAL.
      CONTINUE.
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

  ENDDO.

*-- Create inspection lot for ISIR or Regular using BDC Processing
*--  if scheduling date is exist.
  IF NOT WA_DATUV IS INITIAL AND
     NOT WA_DATUB IS INITIAL.

    REFRESH : BDC_TAB, IT_MSG.
    CLEAR   : BDC_TAB, IT_MSG.

    PERFORM MAKE_INSP_LOT_BDC_DATA_V2  USING I_INSP_HDR
                                             I_INSP_ITEM-MATNR
                                             WA_ISPEMS
                                             I_INSP_HDR-ART
                                             I_INSP_HDR-ART
                                             WA_DATUV
                                             WA_DATUB.

    PERFORM CALL_TRANSACTION_QA01_V2  USING I_BDC_MODE.

*-   collect BDC Message : T_BDC_MSG[] = IT_MSG[].
    APPEND LINES OF IT_MSG TO T_BDC_MSG.  "/ISIR or Regular type

*--  Check Message
    CLEAR IT_MSG.

    READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC = 0.
      ROLLBACK WORK.
      PERFORM MOVE_BDC_MSG_TO_BAPIRET2   USING IT_MSG
                                               RETURN.
      RAISE ERROR_DURING_CREATE_LOT.
    ELSE.
      READ TABLE IT_MSG WITH KEY MSGTYP = 'A'.
      IF SY-SUBRC = 0.
        ROLLBACK WORK.
        PERFORM MOVE_BDC_MSG_TO_BAPIRET2   USING IT_MSG
                                                 RETURN.
        RAISE ERROR_DURING_CREATE_LOT.
      ELSE.
        COMMIT WORK AND WAIT.
        READ TABLE IT_MSG WITH KEY MSGTYP = 'S'
                                   MSGNR  = '100'.
        PERFORM MOVE_BDC_MSG_TO_BAPIRET2   USING IT_MSG
                                                 RETURN.
        MOVE : IT_MSG-MSGV2 TO E_PRUEFLOS.
      ENDIF.
    ENDIF.
  ENDIF.

**/// MS Inspection Lot
*-- Get earliest start date of MIC(MS) for Material
  CLEAR : WA_DATUV, WA_DATUB.
  CLEAR LW_FLD_CNT.
  LW_FLD_CNT = '1000'.
  DO LW_MIC_MS_COUNT TIMES.

    LW_FLD_CNT = LW_FLD_CNT + 10.

    CONCATENATE 'I_INSP_IT_MS-DATUV_' LW_FLD_CNT
                                             INTO LW_FLD_NAME_V.
    ASSIGN : (LW_FLD_NAME_V) TO <L_FS_V>.

    IF  <L_FS_V> IS INITIAL.
      CONTINUE.
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

  ENDDO.

*-- Create inspection lot for MS  using BDC Processing
*--  if scheduling date is exist.
  IF NOT WA_DATUV IS INITIAL AND
     NOT WA_DATUB IS INITIAL.

    REFRESH : BDC_TAB, IT_MSG.
    CLEAR   : BDC_TAB, IT_MSG.

    PERFORM MAKE_INSP_LOT_BDC_DATA_V2  USING I_INSP_HDR
                                             I_INSP_IT_MS-MATNR
                                             WA_ISPEMS
                                             C_INSP_TYPE_MS "/8930
                                             I_INSP_HDR-ART
                                             WA_DATUV
                                             WA_DATUB.

    PERFORM CALL_TRANSACTION_QA01_V2  USING I_BDC_MODE.

*-   collect BDC Message : T_BDC_MSG[] = IT_MSG[].
    APPEND LINES OF IT_MSG TO T_BDC_MSG_MS.  "/ISIR or Regular type

*--  Check Message
    CLEAR IT_MSG.

    READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC = 0.
      ROLLBACK WORK.
      PERFORM MOVE_BDC_MSG_TO_BAPIRET2   USING IT_MSG
                                               RETURN_MS.
      RAISE ERROR_DURING_CREATE_LOT.
    ELSE.
      READ TABLE IT_MSG WITH KEY MSGTYP = 'A'.
      IF SY-SUBRC = 0.
        ROLLBACK WORK.
        PERFORM MOVE_BDC_MSG_TO_BAPIRET2   USING IT_MSG
                                                 RETURN_MS.
        RAISE ERROR_DURING_CREATE_LOT.
      ELSE.
        COMMIT WORK AND WAIT.
        READ TABLE IT_MSG WITH KEY MSGTYP = 'S'
                                   MSGNR  = '100'.
        PERFORM MOVE_BDC_MSG_TO_BAPIRET2   USING IT_MSG
                                                 RETURN_MS.
        MOVE : IT_MSG-MSGV2 TO E_PRUEFLOS_MS.
      ENDIF.
    ENDIF.


  ENDIF.


ENDFUNCTION.
