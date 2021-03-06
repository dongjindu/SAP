FUNCTION Z_FSD_VEHICLE_VPC_I2.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(RETURN) LIKE  SY-SUBRC
*"  TABLES
*"      ITAB STRUCTURE  ZTSD_VPC_INF_I
*"      VAL_TABLE STRUCTURE  ZSPP_VIN_VALUE
*"----------------------------------------------------------------------
  DATA: L_TABIX LIKE SY-TABIX.
  DATA: L_OBJEK LIKE EQUI-EQUNR.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE ITAB
  FROM ZTSD_VPC_INF_I
  WHERE ZFLAG EQ ''.

  IF SY-SUBRC = 0.
    CLEAR: ITAB, VAL_TABLE.
    REFRESH: VAL_TABLE.
    LOOP AT ITAB.
      L_TABIX = SY-TABIX.

      CASE ITAB-ZRECTYPE.
        WHEN 'G01'.       "DEALER ALLOCATION
          VAL_TABLE-ATNAM = 'P_RP20_ACTUAL_DATE'.
          VAL_TABLE-ATWRT = ITAB-ZPDATE.
          APPEND VAL_TABLE.

          VAL_TABLE-ATNAM = 'P_STATUS'.
          VAL_TABLE-ATWRT = 'V01'.
          APPEND VAL_TABLE.

        WHEN 'G02'.       "VPC IN
          VAL_TABLE-ATNAM = 'P_RP21_ACTUAL_DATE'.
          VAL_TABLE-ATWRT = ITAB-ZPDATE.
          APPEND VAL_TABLE.

          VAL_TABLE-ATNAM = 'P_STATUS'.
          VAL_TABLE-ATWRT = 'V02'.
          APPEND VAL_TABLE.

        WHEN 'G03'.       "VPC OUT
          VAL_TABLE-ATNAM = 'P_RP22_ACTUAL_DATE'.
          VAL_TABLE-ATWRT = ITAB-ZPDATE.
          APPEND VAL_TABLE.

          VAL_TABLE-ATNAM = 'P_STATUS'.
          VAL_TABLE-ATWRT = 'V03'.
          APPEND VAL_TABLE.

*          VAL_TABLE-ATNAM = 'G03_DEALER_CODE'.
*          VAL_TABLE-ATWRT = ITAB-DIST.
*          APPEND VAL_TABLE.

        WHEN 'G04'.       "TRUCKING INPUT
          VAL_TABLE-ATNAM = 'P_RP23_ACTUAL_DATE'.
          VAL_TABLE-ATWRT = ITAB-ZPDATE.
          APPEND VAL_TABLE.

          VAL_TABLE-ATNAM = 'P_STATUS'.
          VAL_TABLE-ATWRT = 'V04'.
          APPEND VAL_TABLE.

*          VAL_TABLE-ATNAM = 'G04_PIO_RESULT'.
*          VAL_TABLE-ATWRT = ITAB-ZPIOR.
*          APPEND VAL_TABLE.

        WHEN 'G05'.       "TRUCKING OUTPUT
          VAL_TABLE-ATNAM = 'P_RP23_ACTUAL_DATE'.
          VAL_TABLE-ATWRT = ITAB-ZPDATE.
          APPEND VAL_TABLE.

          VAL_TABLE-ATNAM = 'P_STATUS'.
          VAL_TABLE-ATWRT = 'V05'.
          APPEND VAL_TABLE.

*          VAL_TABLE-ATNAM = 'G05_VIN_LOCATION'.
*          VAL_TABLE-ATWRT = ITAB-ZVINL.
*          APPEND VAL_TABLE.

        WHEN 'G06'.       "RAILING INPUT
          VAL_TABLE-ATNAM = 'P_RP25_ACTUAL_DATE'.
          VAL_TABLE-ATWRT = ITAB-ZPDATE.
          APPEND VAL_TABLE.

          VAL_TABLE-ATNAM = 'P_STATUS'.
          VAL_TABLE-ATWRT = 'V06'.
          APPEND VAL_TABLE.

*          VAL_TABLE-ATNAM = 'G06_RAIL_CODE'.
*          VAL_TABLE-ATWRT = ITAB-ZRCODE.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G06_DEST_CODE'.
*          VAL_TABLE-ATWRT = ITAB-ZDESTC.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G06_ROUTE'.
*          VAL_TABLE-ATWRT = ITAB-ZROUT.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G06_ROW_NUMBER'.
*          VAL_TABLE-ATWRT = ITAB-ZRNUM.
*          APPEND VAL_TABLE.

        WHEN 'G07'.       "RAILING OUTPUT
          VAL_TABLE-ATNAM = 'P_RP26_ACTUAL_DATE'.
          VAL_TABLE-ATWRT = ITAB-ZPDATE.
          APPEND VAL_TABLE.

          VAL_TABLE-ATNAM = 'P_STATUS'.
          VAL_TABLE-ATWRT = 'V07'.
          APPEND VAL_TABLE.

*          VAL_TABLE-ATNAM = 'G07_RAIL_CODE'.
*          VAL_TABLE-ATWRT = ITAB-ZRCODE.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G07_DEST_CODE'.
*          VAL_TABLE-ATWRT = ITAB-ZDESTC.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G07_ROUTE'.
*          VAL_TABLE-ATWRT = ITAB-ZROUT.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G07_ROW_NUMBER'.
*          VAL_TABLE-ATWRT = ITAB-ZRNUM.
*          APPEND VAL_TABLE.

*        WHEN 'G08'.
*          VAL_TABLE-ATNAM = 'G08_PROCESS_DATE'.
*          VAL_TABLE-ATWRT = ITAB-ZPDATE.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G08_FINAL_STATUS'.
*          VAL_TABLE-ATWRT = ITAB-ZPDATE.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G08_TRUCK_CODE'.
*          VAL_TABLE-ATWRT = ITAB-ZTRUC.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G08_LOAD_NUM'.
*          VAL_TABLE-ATWRT = ITAB-ZLNUM.
*          APPEND VAL_TABLE.
*
*        WHEN 'G09'.
*          VAL_TABLE-ATNAM = 'G09_PROCESS_DATE'.
*          VAL_TABLE-ATWRT = ITAB-ZPDATE.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G09_FINAL_STATUS'.
*          VAL_TABLE-ATWRT = ITAB-ZPDATE.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G09_TRUCK_CODE'.
*          VAL_TABLE-ATWRT = ITAB-ZTRUC.
*          APPEND VAL_TABLE.
*
*          VAL_TABLE-ATNAM = 'G09_LOAD_NUM'.
*          VAL_TABLE-ATWRT = ITAB-ZLNUM.
*          APPEND VAL_TABLE.

      ENDCASE.

      L_OBJEK = ITAB-OBJEK.

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                OBJECT     = L_OBJEK
                MODE       = 'W'
           TABLES
                VAL_TABLE  = VAL_TABLE
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .
      IF SY-SUBRC = 0.
        ITAB-ZFLAG = 'Y'.
      ELSE.

      ENDIF.

      MODIFY ITAB INDEX L_TABIX.

      CLEAR: ITAB, VAL_TABLE, L_TABIX.
      REFRESH: VAL_TABLE.
    ENDLOOP.

    MODIFY ZTSD_VPC_INF_I FROM TABLE ITAB.
    IF SY-SUBRC = 0.
      RETURN = 0.
    ELSE.
      RETURN = 1.
    ENDIF.
  ENDIF.

ENDFUNCTION.
