FUNCTION z_fpp_sales_order_new.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(EQUNR) LIKE  EQUI-EQUNR
*"     REFERENCE(VBELN) LIKE  VBAK-VBELN
*"  EXPORTING
*"     REFERENCE(RCODE) LIKE  ZSPP_VIN_VALUE-ZFLAG
*"  EXCEPTIONS
*"      NO_VEHICLE
*"      NO_SALES
*"      NO_ATINN
*"      CHANGE_ERROR
*"----------------------------------------------------------------------

  DATA: l_vals            LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_equi            LIKE equi,
        l_vbak            LIKE vbak.

  " Check the Vehicle Master
  SELECT SINGLE * INTO l_equi
    FROM equi
   WHERE equnr = equnr .

  IF sy-subrc NE 0.
    RAISE no_vehicle.
  ENDIF.

  " Check the Sales Document
  SELECT SINGLE * INTO l_vbak
    FROM vbak
   WHERE vbeln = vbeln .

  IF sy-subrc NE 0.
    RAISE no_sales  .
  ENDIF.

  " Get the Key for the P_SALES_ORDER_NEW
  SELECT SINGLE atinn INTO l_vals-atinn
    FROM cabn
   WHERE atnam = 'P_SALES_ORDER_NEW'.

  IF sy-subrc = 0.
    l_vals-atwrt = vbeln .
    l_vals-atnam = 'P_SALES_ORDER_NEW'.    APPEND l_vals.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = equnr
              mode         = 'W'
         TABLES
              val_table    = l_vals
         EXCEPTIONS
           NO_DATA         = 1
           ERROR_MODE      = 2
           ERROR_OBJECT    = 3
           ERROR_VALUE     = 4
           OTHERS          = 5 .

    IF sy-subrc = 0 .
      READ TABLE l_vals INDEX 1.
      IF l_vals-zflag = space  .
        CLEAR: rcode          .
      ELSE.
        rcode = l_vals-zflag  .
      ENDIF.
    ELSE.
      RAISE change_error .
    ENDIF.
  ELSE.
    RAISE no_atinn.
  ENDIF.
ENDFUNCTION.
