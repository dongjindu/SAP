*&---------------------------------------------------------------------*
*& Report  Z_TEST_FTZ_EPRFORMANCE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*Temporary program for performance improvement of "ZMMIFTZR_EXCLUE_ENG".
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

REPORT  z_test_ftz_eprformance.
TABLES: mkpf.

PARAMETERS: p_date LIKE sy-datum OBLIGATORY.
SELECT-OPTIONS s_mblnr FOR mkpf-mblnr.

RANGES: r_bwart FOR mseg-bwart,
        r_mtart FOR mara-mtart.

DATA: BEGIN OF wa_mseg,
        matnr LIKE mara-matnr,
        lifnr LIKE mseg-lifnr,
        mtart LIKE mara-mtart,
        meins LIKE mara-meins,
        profl LIKE mara-profl,
        ntgew LIKE mara-ntgew,
        gewei LIKE mara-gewei,
        xblnr(10) TYPE c,
        xabln LIKE mkpf-xabln,
        bwart LIKE mseg-bwart,
        shkzg LIKE mseg-shkzg,
        werks LIKE mseg-werks,
        kdauf LIKE mseg-kdauf,
        menge LIKE mseg-menge,
        erfme LIKE mseg-erfme,
        ummat LIKE mseg-ummat,
        ebeln LIKE mseg-ebeln,
        ebelp LIKE mseg-ebelp,
        maktx LIKE makt-maktx,
        LGORT LIKE MSEG-LGORT,
* Begin of changes by Matthew Cupples on 08/20/2010
        sakto LIKE mseg-sakto,
* End changes on 08/20/2010
** Furong on 02/03/12
*        MBLNR LIKE MSEG-MBLNR,
        zeile LIKE mseg-zeile,
        posnr LIKE lips-posnr,
        lichn LIKE lips-lichn,
** End on 02/03/12
        mngko LIKE stpox-mngko,           "Not part of select statement
        pmatnr LIKE mara-matnr,           "Not part of select statement
        pprofl LIKE mara-profl,           "Not part of select statement
      END OF wa_mseg.
DATA: lt_mseg_aknh LIKE TABLE OF wa_mseg WITH HEADER LINE.
data: lt_temp LIKE TABLE OF wa_mseg WITH HEADER LINE.

RANGES: lr_bwart FOR mseg-bwart.
PERFORM assign_valid_movtypes.

BREAK-POINT.

SELECT mara~matnr lifnr mtart mara~meins profl ntgew gewei xblnr
                       xabln bwart shkzg werks mat_kdauf  AS kdauf
                       menge
                       erfme ummat ebeln ebelp maktx LGORT
                        INTO TABLE lt_temp
                        FROM mara
                        INNER JOIN mseg
                        ON mseg~matnr = mara~matnr
                        INNER JOIN mkpf
                        ON mseg~mblnr = mkpf~mblnr
                        AND mseg~mjahr = mkpf~mjahr
                        INNER JOIN makt
                        ON makt~matnr = mara~matnr
                        WHERE  mtart = 'HALB' AND
                              cpudt = p_date AND
                              mkpf~mblnr IN s_mblnr AND
                              spras = sy-langu AND bwart IN lr_bwart
"   AND
*                              ( werks = 'E001' AND lgort = 'E302' OR
*** Added on 12/19/11 for E002
*                              werks = 'E002' AND lgort = 'N302')
*** End on 12/19/11
                              AND mseg~kunnr = 'AKNH'.
*          GROUP BY mara~matnr lifnr mtart mara~meins profl ntgew
*                   gewei xblnr xabln bwart shkzg werks mat_kdauf erfme
*                   ummat ebeln ebelp maktx.


loop at lt_temp.
  IF  ( lt_temp-werks = 'E001' AND lt_temp-lgort = 'E302' ) OR
      ( lt_temp-werks = 'E002' AND lt_temp-lgort = 'N302').
      lt_mseg_aknh = lt_temp.
      COLLECT lt_mseg_aknh.
 ENDIF.
endloop.
break-point.


*&---------------------------------------------------------------------*
*&      Form  assign_valid_movtypes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assign_valid_movtypes.
  PERFORM add_bwart USING '101'.
  PERFORM add_bwart USING '102'.
  PERFORM add_bwart USING '122'.
  PERFORM add_bwart USING '123'.
  PERFORM add_bwart USING '201'.
  PERFORM add_bwart USING '202'.
  PERFORM add_bwart USING '551'.
  PERFORM add_bwart USING '552'.
  PERFORM add_bwart USING '555'.
  PERFORM add_bwart USING '556'.
  PERFORM add_bwart USING '903'.
  PERFORM add_bwart USING '904'.
  PERFORM add_bwart USING '907'.
  PERFORM add_bwart USING '908'.
  PERFORM add_bwart USING '905'.
  PERFORM add_bwart USING '906'.
  PERFORM add_bwart USING '702'.
  PERFORM add_bwart USING '701'.
  PERFORM add_bwart USING '712'.
  PERFORM add_bwart USING '711'.
  PERFORM add_bwart USING '601'.
  PERFORM add_bwart USING '602'.
  PERFORM add_bwart USING '309'.
  PERFORM add_bwart USING '310'.
  PERFORM add_bwart USING '991'.
** Changed by Furong on 11/24/09
  PERFORM add_bwart USING '655'.
** End of change
** Changed by Furong on 04/24/12
  PERFORM add_bwart USING '511'.
  PERFORM add_bwart USING '512'.
** End of change

  CLEAR r_bwart.
ENDFORM.                    " assign_valid_movtypes

*&---------------------------------------------------------------------*
*&      Form  add_bwart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BWART    text
*----------------------------------------------------------------------*
FORM add_bwart USING p_bwart.

  r_bwart-sign = 'I'.
  r_bwart-option = 'EQ'.
  r_bwart-low = p_bwart.
  APPEND r_bwart.

ENDFORM.                    " add_bwart
