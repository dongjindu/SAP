FUNCTION Z_FQM_ASSIGN_MAT_TO_INSP_PLAN.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_MATNR) TYPE  MATNR
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"     VALUE(I_EXTWG) TYPE  EXTWG OPTIONAL
*"     VALUE(I_BDCMODE) TYPE  TB_BDCMODE DEFAULT 'N'
*"     VALUE(I_VALID_DATE) TYPE  CP_STTAG
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

  DATA : LW_GRP_COUNT(20) TYPE C,  "BDC Field name for group counter
         LW_MATERIAL(20) TYPE C,   "BDC Field name for material
         LW_PLANT(20)    TYPE C.   "BDC Field name for PLANT

  DATA : LW_CNT(2) TYPE N  VALUE '01'. "/line count for Inspection type

  DATA : LW_DATE(10) TYPE C.

  CHECK NOT I_EXTWG IS INITIAL.

  REFRESH BDC_TAB.

  WRITE I_VALID_DATE TO LW_DATE.

  PERFORM DYNPRO  USING:
    'X'   'SAPLCPDI'          '8010',
    ' '   'BDC_OKCODE'        '/00',
    ' '   'RC27M-MATNR'       ' ',
    ' '   'RC271-PLNNR'       I_EXTWG,     "/Group
    ' '   'RC271-STTAG'       LW_DATE.     "/Key date

  PERFORM DYNPRO  USING:
    'X'   'SAPLCPDI'                  '1200',
    ' '   'BDC_OKCODE'                '=MTUE'. "/Material Assignment


  PERFORM DYNPRO  USING:              "/CTRL+Page Down
    'X'   'SAPLCZDI'         '4010',
    ' '   'BDC_OKCODE'       '=P++'.

  PERFORM DYNPRO  USING:              "/Page Down
    'X'   'SAPLCZDI'         '4010',
    ' '   'BDC_OKCODE'       '=P+'.

  LOOP AT T_ART.

    AT FIRST.
      PERFORM DYNPRO  USING:
              'X'   'SAPLCZDI'         '4010',
              ' '   'BDC_OKCODE'       '=BACK'.

    ENDAT.

    LW_CNT = LW_CNT + 1.  "/START SECOND LINE

*-   Get field name table control for Material assignment
    CONCATENATE 'MAPL-PLNAL('
                 LW_CNT
                 ')'       INTO LW_GRP_COUNT.
    CONCATENATE 'MAPL-MATNR('
                 LW_CNT
                 ')'       INTO LW_MATERIAL.
    CONCATENATE 'MAPL-WERKS('
                 LW_CNT
                 ')'       INTO LW_PLANT.


    CASE T_ART-ART.
      WHEN ZQMT1_INSP_TYPE_ISIR.
        PERFORM DYNPRO  USING:
           ' '   LW_GRP_COUNT      '1'.
      WHEN ZQMT1_INSP_TYPE_REGULAR.
        PERFORM DYNPRO  USING:
           ' '   LW_GRP_COUNT      '2'.
      WHEN ZQMT1_INSP_TYPE_MS.
        PERFORM DYNPRO  USING:
           ' '   LW_GRP_COUNT      '3'.
    ENDCASE.

    PERFORM DYNPRO  USING:
      ' '   LW_MATERIAL       I_MATNR,
      ' '   LW_PLANT          I_WERKS.

  ENDLOOP.



  PERFORM DYNPRO  USING:
    'X'   'SAPLCPDI'         '1200',
    ' '   'BDC_OKCODE'       '=BU'.



  REFRESH T_MSG.

  CALL TRANSACTION 'QP02'    USING BDC_TAB
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
