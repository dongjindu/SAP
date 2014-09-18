*----------------------------------------------------------------------*
*   INCLUDE ZXM06U42                                                   *
*----------------------------------------------------------------------*
MOVE-CORRESPONDING i_ekpo TO gl_ekpo_ci.
gl_ekpo = i_ekpo.

************************************************************************
* Enhancement
* Project Name      : Z_MM_PUR
* Enhancement Name  : MM06E005                                         *
* Function Name     : EXIT_SAPMM06E_017                                *
* Author            : Min-su Park                                      *
* Creation Date     : 2003.11.25.                                      *
* Specifications By : Min-su Park                                      *
* Development Request No :                                             *
* Addl Documentation:                                                  *
* Description       : EMMPM38 PR Warning message for Pallet QTY        *
* Export Data to Customer Subscreen for Purchasing Document Header (PBO)
*                                                                      *
* Modification Logs                                                    *
* Date            Developer          RequestNo    Description          *
* 2003.11.25.     Min-su Park                     Initial Coding       *
*                                                                      *
************************************************************************
DATA : W_BSTRF LIKE MARC-BSTRF       , "Rounding Value
       W_REMAIN LIKE MEREQ_ITEM-MENGE,
       W_MSG(10)                     . "Pallet QTY

DATA : PRGID LIKE SY-REPID VALUE 'ZEMMPM38E_MESSAGE',
       W_TMP LIKE MARC-BSTRF                        .

*Get Rounding value
  SELECT SINGLE BSTRF
           INTO W_BSTRF
           FROM MARC
          WHERE WERKS = I_EKPO-WERKS
            AND MATNR = I_EKPO-MATNR.

  CHECK W_BSTRF > 0.

*Check Rounding and menge
  W_REMAIN = I_EKPO-MENGE MOD W_BSTRF.

  IF W_REMAIN > 0.
    W_TMP = W_BSTRF - W_REMAIN + I_EKPO-MENGE.
    WRITE : W_TMP TO W_MSG UNIT I_EKPO-MEINS CENTERED.
    SUBMIT (PRGID)  WITH P_MSG  = W_MSG
                     AND RETURN       .
  ELSE.

  ENDIF.
