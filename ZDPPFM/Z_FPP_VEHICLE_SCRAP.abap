FUNCTION Z_FPP_VEHICLE_SCRAP.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(EQUNR) LIKE  EQUI-EQUNR
*"     VALUE(USAGE_TYPE) LIKE  ZTPP_SCRAP_CAR-U_CAR OPTIONAL
*"  EXPORTING
*"     REFERENCE(RETURN) TYPE  ZZRET
*"  TABLES
*"      BEFORE_VIN STRUCTURE  ZSPP_VIN_VALUE OPTIONAL
*"      AFTER_VIN STRUCTURE  ZSPP_VIN_VALUE OPTIONAL
*"  EXCEPTIONS
*"      MASTER_ERROR
*"      STATUS_ERROR
*"----------------------------------------------------------------------
   DATA: L_MODEL(4)           TYPE C,
        L_BDSER(6)           TYPE N,
        L_RPID               LIKE ZTPP_STATUS-WO_ID,
        L_WOSER              LIKE ZTPP_WOSUM-WO_SER,
        L_NATION             LIKE ZTPP_WOSUM-NATION,
        L_DEALER             LIKE ZTPP_WOSUM-DEALER,
        L_EXTC               LIKE ZTPP_WOSUM-EXTC  ,
        L_INTC               LIKE ZTPP_WOSUM-INTC  ,
        L_PORDER             LIKE AUSP-ATWRT       ,
        L_STATS(3)           TYPE C                ,
        L_FLAG               TYPE C                ,
        L_ERROR              TYPE C,
        L_DIST(2).

  COMMIT WORK.

  " 1. Common Check for the Vehicle..
  PERFORM CHECK_COMMON_INFO  TABLES BEFORE_VIN
                              USING EQUNR L_FLAG.

  IF L_FLAG = '1'.
    RETURN = 'E'.
    RAISE MASTER_ERROR  .
  ENDIF.

  IF L_FLAG = '2'.
    RETURN = 'E'.
    RAISE STATUS_ERROR  .
  ENDIF.

  " 2. Create the Scrap car record in ZTPP_SCRAP_CAR
  PERFORM INSERT_SCRAP_CAR  USING  BEFORE_VIN[] RETURN .
  IF RETURN = 'E'   .   EXIT.  ENDIF.

  " Change the Work Order Summary Table
  READ TABLE BEFORE_VIN WITH KEY ATNAM  = 'P_WORK_ORDER'           .
  IF SY-SUBRC = 0.
    L_WOSER  = BEFORE_VIN-ATWRT(9).
    L_NATION = BEFORE_VIN-ATWRT+09(3).
    L_DEALER = BEFORE_VIN-ATWRT+12(3).
  ENDIF  .
  READ TABLE BEFORE_VIN WITH KEY ATNAM  = 'P_EXT_COLOR'            .
  IF SY-SUBRC = 0.  L_EXTC              = BEFORE_VIN-ATWRT. ENDIF  .
  READ TABLE BEFORE_VIN WITH KEY ATNAM  = 'P_INT_COLOR'            .
  IF SY-SUBRC = 0.  L_INTC              = BEFORE_VIN-ATWRT. ENDIF  .
  READ TABLE BEFORE_VIN WITH KEY ATNAM  = 'P_STATUS'               .
  IF SY-SUBRC = 0.  L_STATS             = BEFORE_VIN-ATWRT. ENDIF  .

  SELECT SINGLE WO_ID INTO L_RPID
    FROM ZTPP_STATUS
   WHERE ID = L_STATS       .
** Changed by Furong on 02/07/08
  READ TABLE BEFORE_VIN WITH KEY ATNAM  = 'P_DIST_CODE'                .
  IF SY-SUBRC = 0.  L_DIST            = BEFORE_VIN-ATWRT. ENDIF  .

  IF L_DIST+0(1) = 'X' AND USAGE_TYPE = 'D'.
  ELSE.
** End of change
****************************
    IF L_NATION = 'B28' AND L_RPID+2(2) >= 13.
    ELSE.
      PERFORM CHANGE_WOSUM_AND_VEH USING   L_WOSER  L_NATION   L_DEALER
                                   L_EXTC  L_INTC   L_RPID     RETURN .
      IF RETURN NE SPACE.  ROLLBACK WORK.  EXIT.  ENDIF.
    ENDIF.
***********************************

    READ TABLE BEFORE_VIN WITH KEY ATNAM  = 'P_MODEL'                .
    IF SY-SUBRC = 0.  L_MODEL             = BEFORE_VIN-ATWRT. ENDIF  .
    READ TABLE BEFORE_VIN WITH KEY ATNAM  = 'P_BODY_SERIAL'          .
    IF SY-SUBRC = 0.  L_BDSER             = BEFORE_VIN-ATWRT. ENDIF  .
    READ TABLE BEFORE_VIN WITH KEY ATNAM  = 'P_PLAN_ORDER'           .
    IF SY-SUBRC = 0.  L_PORDER            = BEFORE_VIN-ATWRT. ENDIF  .

    " Create the Revive Component in the ZTPP_OSDP Table
  PERFORM INSERT_SCRAP_COMPONENT USING L_MODEL L_BDSER L_PORDER
                                       RETURN USAGE_TYPE.
    IF RETURN NE SPACE.  ROLLBACK WORK.  EXIT.  ENDIF.
** Changed by Furong on 02/07/08
  ENDIF.
** End of change
  " Change the BackFlush Stats Table
  IF USAGE_TYPE = 'S'.
    PERFORM CHANGE_BF_STATUS       USING L_MODEL   L_BDSER   L_RPID
                                         L_PORDER            RETURN .
    IF RETURN NE SPACE.  ROLLBACK WORK.  EXIT.  ENDIF.
  ENDIF.
*  CALL SCREEN 3100.
  RETURN = 'S' .
ENDFUNCTION.
