************************************************************************
* Program Name : ZEMMPM33R_STD_PRICE_NEWMAT
* Created by   : Min-su Park
* Created on   : 2003.10.17.
* Pattern      :
* Description  : Manage Standard Price for NEW Purchase Material
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.17.     Min-su Park    UD1K901873     Initial Coding
************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM33R_STD_PRICE_NEWMATEVEN                             *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CLEAR   : it_analy.
  REFRESH : it_analy.

START-OF-SELECTION.
  p_date = sy-datum.
  DELETE FROM ztmm_log WHERE tcode = sy-tcode.
  REFRESH it_error.

*Get material of material type 'ROH', 'ROH1'
*and KD/LD 'K', 'V'
  SELECT * FROM zvmm_new_sprice     " MARA + MARC + MBEW
           INTO CORRESPONDING FIELDS OF TABLE it_new_mat
          WHERE mtart IN ('ROH', 'ROH1')
            AND profl IN ('K'  , 'V')
*            AND stprs IS null     " 04/01/22 block by stlim
            AND stprs EQ space     " 04/01/22 insert by stlim
            AND lvorm  <> 'X'
            AND lvorm1 <> 'X'
            AND lvorm2 <> 'X'.

*Processing by material on KD/LD
  LOOP AT it_new_mat.
    CLEAR it_mara.
    MOVE-CORRESPONDING it_new_mat TO it_mara.
    CASE it_mara-profl.
      WHEN 'K'.     " KD
        PERFORM standard_price_kd USING it_mara.
      WHEN 'V'.     " LP
        PERFORM standard_price_lp USING it_mara.
    ENDCASE.
  ENDLOOP.

  PERFORM save_log.

*---
