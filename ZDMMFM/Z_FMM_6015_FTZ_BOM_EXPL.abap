FUNCTION z_fmm_6015_ftz_bom_expl .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IM_MATNR) LIKE  MAST-MATNR
*"     VALUE(IM_WERKS) LIKE  MAST-WERKS
*"     VALUE(IM_STLAN) LIKE  MAST-STLAN
*"     VALUE(IM_STLAL) LIKE  MAST-STLAL
*"     VALUE(IM_ATWRE) LIKE  CAWN-ATWRT
*"     VALUE(IM_ATWRI) LIKE  CAWN-ATWRT
*"     VALUE(IM_DATUV) LIKE  STKO-DATUV
*"  TABLES
*"      EXT_STPOX_ALV STRUCTURE  STPOX_ALV OPTIONAL
*"----------------------------------------------------------------------
  CLEAR: gt_stpox_alv.
**** Enhanced BOM Explosion (Enhanced /nCS12)
  PERFORM enhanced_bom_expl
           USING    im_matnr   "Source Mat.
                    im_werks   "Source Mat. Plant
                    im_stlan   "Source Mat. BOM Usage
                    im_stlal   "Source Mat. Alternative BOM
                    im_atwre   "Source Mat. Ext.Color
                    im_atwri   "Source Mat. Int.Color
                    im_datuv.  "Source Mat. Valid From

  ext_stpox_alv[] = gt_stpox_alv.
*

  CLEAR: gs_stpox_alv, gt_stpol_add, gt_stpox, gt_stpox_alv.

ENDFUNCTION.
