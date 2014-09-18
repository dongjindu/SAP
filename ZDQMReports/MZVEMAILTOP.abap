*----------------------------------------------------------------------*
*   INCLUDE MZVEMAILTOP                                                *
*----------------------------------------------------------------------*
TABLES: ZTQM_VEND_EMAIL, lfa1,  ZsQM_VEND_EMAIl.

data: OK_CODE LIKE SY-UCOMM.

DATA: W_LIFNR LIKE ZtQM_VEND_EMAIL-LIFNR,
      W_NAME LIKE LFA1-NAME1,
      W_EMAIL1 LIKE ZTQM_VEND_EMAIL-EMAIL1,
      W_EMAIL2 LIKE ZTQM_VEND_EMAIL-EMAIL2,
      W_EMAIL3 LIKE ZTQM_VEND_EMAIL-EMAIL3.
