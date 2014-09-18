FUNCTION Z_FQM_EAI_VIN_INFORMATION.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_DATE) TYPE  CHAR8
*"     VALUE(I_PERIOD) TYPE  I DEFAULT 10
*"  TABLES
*"      T_VIN STRUCTURE  ZSQM_IQS_VEH OPTIONAL
*"  EXCEPTIONS
*"      NO_DATA
*"      NOT_EXIT_CHAR
*"----------------------------------------------------------------------

*---- Start...

  DATA : LW_DATE TYPE DATUM.

  MOVE : I_DATE TO LW_DATE.

  REFRESH IT_VALUE.

  ATINN_CHAR : IT_VALUE   'P_VIN',            "/Vehicle
               IT_VALUE   'P_RP18_SHOP_DATE', "/Shopdate
               IT_VALUE   'P_ENGINE_NO',      "/Engine Number
               IT_VALUE   'P_219_9',          "/Engine capacity
               IT_VALUE   'P_219_8',          "/Fuel Type
               IT_VALUE   'P_TM_NO',          "/TM Number
               IT_VALUE   'P_219_7',          "/TM Type
               IT_VALUE   'P_MODEL',       "/Vehicle type code
               IT_VALUE   'P_WORK_ORDER',     "/Work order
               IT_VALUE   'P_KEY_NO'.         "/Key Number

*-- Get characteristic data from CABN using Characteristic Value name
  REFRESH IT_CABN.

  SELECT ATINN ATNAM ATFOR
      INTO CORRESPONDING FIELDS OF TABLE IT_CABN
        FROM CABN
          WHERE ATNAM  IN R_ATNAM.

  IF SY-SUBRC NE 0.
    RAISE NOT_EXIT_CHAR.
  ENDIF.


*- period for SQL
  WA_DATE_TEMP  =  LW_DATE - I_PERIOD. "/Period

  WA_ATWRT_L = WA_DATE_TEMP.
  WA_ATWRT_H = LW_DATE.

*-- Select Vehicle master using Vehicle master Function
  PERFORM GET_VEHICLE_MASTER. "/Get vehicle Master Info

  CHECK NOT IT_VEHICLE[] IS INITIAL.
*-  Mapping data for interface to IT_ZSQM_IQS_VEH
  PERFORM MAP_CHAR_VALUE_2_GQIS.


  T_VIN[] = IT_ZSQM_IQS_VEH[].

ENDFUNCTION.
