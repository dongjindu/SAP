
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_SEL                                        *
*----------------------------------------------------------------------*
*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS:
                s_aedat  FOR ztsd_um-aedat
                          DEFAULT sy-datum NO-EXTENSION NO INTERVALS.
PARAMETERS:  p_shp_d TYPE i DEFAULT 7,
             p_del_d TYPE i DEFAULT 60.
SELECTION-SCREEN ULINE.
SELECT-OPTIONS:
                s_seria  FOR ztsd_um-wo_serial,
                s_nation FOR ztsd_um-wo_nation NO INTERVALS,
                                         "     NO-EXTENSION,
                s_dealer FOR ztsd_um-wo_dealer.

SELECTION-SCREEN SKIP.
PARAMETERS : p_delta      AS CHECKBOX  DEFAULT 'X'.
PARAMETERS : p_send AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
PARAMETERS: p_input       AS CHECKBOX  DEFAULT 'X ',
            p_vin         AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b4.


*SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b7 WITH FRAME TITLE text-007.
PARAMETERS : p_miss AS CHECKBOX.
PARAMETERS : p_nation LIKE ztsd_osr-nation.
" If HMMA start Cross Production,
" Dealer is required
*             P_DEALER LIKE ZTSD_OSR-DEALER.

SELECT-OPTIONS : s_zvin   FOR ztsd_um-zvin.
*parameters:     p_model like ztsd_um-model_code.
*select-options: s_body  for  ztsd_um-body_no.
SELECTION-SCREEN END OF BLOCK b7.

*SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE text-006.
*PARAMETERS : p_hma AS CHECKBOX DEFAULT 'X',
*             p_hac AS CHECKBOX DEFAULT '',
*             p_glv AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN END OF BLOCK b6.
*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
