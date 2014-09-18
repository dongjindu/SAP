FUNCTION Z_FPP_MD_STOCK_REQ_LIST_API.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_MATNR) LIKE  MARA-MATNR
*"     VALUE(P_WERKS) LIKE  MARC-WERKS
*"  EXPORTING
*"     VALUE(O_MATNR) LIKE  MARA-MATNR
*"  TABLES
*"      P_MDSUX STRUCTURE  MDSU
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"      RESOURCE_FAILURE
*"----------------------------------------------------------------------
    data: c_plscn LIKE plaf-plscn VALUE '000'.

    CLEAR : p_mdsux, p_mdsux[].
    CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
         EXPORTING
              plscn                    = c_plscn
              matnr                    = P_MATNR
              werks                    = P_WERKS
         TABLES
              mdsux                    = p_mdsux
         EXCEPTIONS
              material_plant_not_found = 1
              plant_not_found          = 2
              OTHERS                   = 3.
    O_MATNR = P_MATNR.
ENDFUNCTION.
