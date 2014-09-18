*----------------------------------------------------------------------*
***INCLUDE ZXM06TOP .
*----------------------------------------------------------------------*
DATA  gl_aktyp.
DATA  gl_no_screen.
DATA  gl_ucomm LIKE sy-ucomm.
****For Item
DATA: BEGIN OF gt_ref_ekpo_tab OCCURS 0,
       ebeln LIKE ekpo-ebeln,
       ebelp LIKE ekpo-ebelp,
       ekpo  LIKE ekpo,
      END OF gt_ref_ekpo_tab.
DATA  gl_ekpo LIKE ekpo.
DATA  gl_ekpo_ci LIKE ekpo_ci.

****For Header
DATA  gl_ekko_ci LIKE ekko_ci.
DATA  gl_rekko LIKE ekko.
