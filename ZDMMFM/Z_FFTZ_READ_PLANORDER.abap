FUNCTION Z_FFTZ_READ_PLANORDER.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_PLNUM) LIKE  PLAF-PLNUM
*"  EXPORTING
*"     VALUE(PS_RETURN) LIKE  BAPIRET1 STRUCTURE  BAPIRET1
*"     VALUE(PS_HEADER) LIKE  BAPIPLAF_E1 STRUCTURE  BAPIPLAF_E1
*"  TABLES
*"      PIT_COMPONENTS STRUCTURE  BAPI_PLDORDCOMP_E1
*"----------------------------------------------------------------------

  CALL FUNCTION 'BAPI_PLANNEDORDER_GET_DETAIL'
    EXPORTING
      PLANNEDORDER              = p_plnum
    IMPORTING
      RETURN                    = ps_return
      HEADERDATA                = ps_header
*   CAPACITYHEADERDATA1       =
*   CAPACITYHEADERDATA2       =
*   CAPACITYHEADERDATA3       =
   TABLES
      COMPONENTSDATA            = pit_components .
*   CAPACITYDATA1             =
*   CAPACITYDATA2             =
*   CAPACITYDATA3             =


ENDFUNCTION.
