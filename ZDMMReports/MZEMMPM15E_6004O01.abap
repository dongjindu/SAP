*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM15E_6004O01                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  initial_data  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_data OUTPUT.
  ltak-lgnum = 'P01'.                     "Warehouse number
   *ltap-lgnum = lein-lgnum = ltak-lgnum.  "Warehouse number

**--- blocked by stlim (2004/04/13)
*  ltak-bwlvs = '999'.  "Movement type
**--- end of block
**--- insert by stlim (2004/04/13)
  ltak-bwlvs = '977'.  "Movement type
**--- end of insert
ENDMODULE.                 " initial_data  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  desc  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE desc OUTPUT.

  PERFORM desc_bwlvs USING    sy-langu
                              ltak-lgnum     "Warehouse number
                              ltak-bwlvs     "desc of movement type
                     CHANGING t333t-lbwat.
* Desc of LETYP(Storage unit type)
  PERFORM desc_letyp USING    sy-langu
                              ltak-lgnum
                              lein-letyp
                     CHANGING t307t-letyt.

* Desc of NLTYP(Storage Type)
  PERFORM desc_nltyp USING    sy-langu
                              ltak-lgnum
                              *ltap-nltyp
                     CHANGING t301t-ltypt.
ENDMODULE.                 " desc  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  SET PF-STATUS 'PS'.
  SET TITLEBAR 'TB'.
ENDMODULE.                 " status  OUTPUT
