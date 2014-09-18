FUNCTION Z_FQM_SET_MAT_EXTWG_N_QM_VIEW.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_MATNR) TYPE  MATNR
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"     VALUE(I_EXTWG) TYPE  EXTWG OPTIONAL
*"     VALUE(I_BDCMODE) TYPE  TB_BDCMODE DEFAULT 'N'
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  BAPIRETURN
*"  TABLES
*"      T_ART STRUCTURE  ZSQM_QPART OPTIONAL
*"      T_MSG STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      BDC_ERROR_FOUNDED
*"----------------------------------------------------------------------

** BDC Tables -> Global
*  DATA : BEGIN OF BDC_TAB OCCURS 0.
*          INCLUDE STRUCTURE BDCDATA.
*  DATA : END OF BDC_TAB.
*  DATA   BEGIN OF IT_MSG OCCURS 0.
*          INCLUDE STRUCTURE BDCMSGCOLL.
*  DATA   END OF IT_MSG.

  DATA : LW_ART(20) TYPE C,   "BDC Field name for Inspection type
         LW_AKTIV(20) TYPE C. "BDC Field name for Inspection type active

  DATA : LW_CNT(2) TYPE N. "/line count for Inspection type

  CHECK NOT I_EXTWG IS INITIAL.

  REFRESH BDC_TAB.
  CLEAR LW_CNT.

  PERFORM DYNPRO  USING:
    'X'   'SAPLMGMM'          '0060',
    ' '   'BDC_OKCODE'        '/00',
    ' '   'RMMG1-MATNR'       I_MATNR.

  PERFORM DYNPRO  USING:
    'X'   'SAPLMGMM'                  '0070',
    ' '   'BDC_OKCODE'                '=ENTR',
    ' '   'MSICHTAUSW-KZSEL(01)'       C_MARK, "/Basic view 1
    ' '   'MSICHTAUSW-KZSEL(14)'       C_MARK. "/QM View

  PERFORM DYNPRO  USING:
    'X'   'SAPLMGMM'         '0080',
    ' '   'BDC_OKCODE'       '=ENTR',
    ' '   'RMMG1-WERKS'       I_WERKS.
*- Basic view 1
  PERFORM DYNPRO  USING:
    'X'   'SAPLMGMM'         '5004',
    ' '   'BDC_OKCODE'       '=SP23',
    ' '   'MARA-EXTWG'       I_EXTWG.

*- Quality Management view
  PERFORM DYNPRO  USING:
    'X'   'SAPLMGMM'         '5000',
    ' '   'BDC_OKCODE'       '=PB01'.

*- Inspection type assignment
  PERFORM DYNPRO  USING:
    'X'   'SAPLQPLS'         '0100',
    ' '   'BDC_OKCODE'       '=NEU'.


  PERFORM DYNPRO  USING:
    'X'   'SAPLQPLS'         '0100',
    ' '   'BDC_OKCODE'       '=WEIT'.



  LOOP AT T_ART.
    LW_CNT = LW_CNT + 1.
*-   Get field name table control for Inspection type and Acitve
    CONCATENATE 'RMQAM-ART('
                 LW_CNT
                 ')'       INTO LW_ART.
    CONCATENATE 'RMQAM-AKTIV('
                 LW_CNT
                 ')'       INTO LW_AKTIV.


    PERFORM DYNPRO  USING:
      ' '   LW_ART      T_ART-ART,
      ' '   LW_AKTIV    C_MARK.


  ENDLOOP.


  PERFORM DYNPRO  USING:
    'X'   'SAPLMGMM'         '5000',
    ' '   'BDC_OKCODE'       '=BU'.

  SET PARAMETER ID 'MAT' FIELD I_MATNR. "/Set MATNR to Standart Screen

  REFRESH T_MSG.

  CALL TRANSACTION 'MM02'    USING BDC_TAB
                             UPDATE 'S'
                             MODE I_BDCMODE
                          MESSAGES INTO T_MSG.


  READ TABLE T_MSG WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC = 0.
    ROLLBACK WORK.
    PERFORM CONVER_BDCMSG_2_RETURN  USING T_MSG
                                          E_RETURN.
    RAISE BDC_ERROR_FOUNDED.
    EXIT.
  ELSE.
    READ TABLE T_MSG WITH KEY MSGTYP = 'A'.
    IF SY-SUBRC = 0.
      ROLLBACK WORK.
      PERFORM CONVER_BDCMSG_2_RETURN  USING T_MSG
                                            E_RETURN.
      RAISE BDC_ERROR_FOUNDED.
      EXIT.
    ELSE.
      READ TABLE T_MSG WITH KEY MSGTYP = 'S'.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        PERFORM CONVER_BDCMSG_2_RETURN  USING T_MSG
                                              E_RETURN.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFUNCTION.
