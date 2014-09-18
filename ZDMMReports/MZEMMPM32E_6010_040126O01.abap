*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM32E_6010O01                                         *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  initial_data  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_data OUTPUT.
*   ltak-lgnum = 'P01'.                     "Warehouse number
*   *ltap-lgnum = lein-lgnum = ltak-lgnum.  "Warehouse number
*  ltak-bwlvs = '999'.  "Movement type
ENDMODULE.                 " initial_data  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
*  SET PF-STATUS 'PS'.
*  SET TITLEBAR 'TB'.
* PERFORM make_it_func.

* Instanciate PF-STATUS & TITLEBAR.
  IF w_title IS INITIAL.
*    w_title = 'Select Delivery by Vendor Mat. No.'.
    w_title = 'Select Delivery by Unit Load No.'.
  ENDIF.

  CREATE OBJECT crv_ps
    EXPORTING im_ps      = 'PS'     "PF-STATUS
              im_it_func = it_func  "Excluding func
              im_tb      = 'TB'     "TITLEBAR
              im_title   = w_title.   "TITLE
  CLEAR it_func.

ENDMODULE.                 " status  OUTPUT
