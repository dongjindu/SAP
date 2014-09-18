************************************************************************
* Program Name      : ZEMMPM10E_STL_06
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.04.23.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K909855
* Addl Documentation:
* Description       : Supply to Line - RP06
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.04.23.     Sung-Tae Lim     UD1K909855     Initial Coding
*
*
************************************************************************

REPORT zemmpm10e_stl_06 NO STANDARD PAGE HEADING
                        LINE-SIZE 400
                        MESSAGE-ID zmmm.
TABLES: mara.
**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-003.
PARAMETERS : p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001',
             p_lgort LIKE mard-lgort  OBLIGATORY DEFAULT 'P400',
             p_cdate LIKE sy-datum OBLIGATORY,     "Current date
             p_ctime LIKE sy-uzeit OBLIGATORY.     "Current time
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-004.
PARAMETERS : p_arbpl LIKE crhd-arbpl OBLIGATORY.
SELECTION-SCREEN END OF BLOCK block2.

SELECT-OPTIONS : s_matnr FOR mara-matnr.

PARAMETERS : p_sql  TYPE c MODIF ID his,
             p_pada LIKE tc37p-padauer DEFAULT '1200' MODIF ID his,
             p_rp   TYPE c DEFAULT '06' MODIF ID his.

DATA: d_ucomm LIKE sy-ucomm.

AT SELECTION-SCREEN.
  d_ucomm = sy-ucomm.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK screen-group1 = 'his' OR screen-group1 = 'HIS'.
    IF d_ucomm = 'HISNA'.
      screen-active = '1'.
    ELSE.
      screen-active = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.
  SUBMIT zemmpm10e_stl_hourly_wc
          WITH p_arbpl = p_arbpl
          WITH p_cdate = p_cdate
          WITH p_ctime = p_ctime
          WITH p_lgort = p_lgort
          WITH p_pada  = p_pada
          WITH p_rp    = p_rp
          WITH p_sql   = 'E'
          WITH p_werks = p_werks
          WITH s_matnr IN s_matnr
    AND RETURN.
