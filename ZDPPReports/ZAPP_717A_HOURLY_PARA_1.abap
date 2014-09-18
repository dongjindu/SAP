*----------------------------------------------------------------------*
*   INCLUDE ZAPP_717A_HOURLY_PARA                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_plnum FOR plaf-plnum,
                 p_budat FOR ztpp_bfst-bfp01_dat NO INTERVALS
                 NO-EXTENSION.

PARAMETERS:
*  p_plnum  LIKE ppc_comp_conf-plnum ,
"obligatory,
  p_repnt  LIKE ppc_comp_conf-stsor,
  p_plant LIKE marc-werks DEFAULT 'P001'.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-005.
PARAMETERS: p_a RADIOBUTTON GROUP r1 ,
            p_b RADIOBUTTON GROUP r1 DEFAULT 'X'.

PARAMETERS: p_c RADIOBUTTON GROUP r2 ,
            p_d RADIOBUTTON GROUP r2 DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b2.
