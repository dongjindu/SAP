*&---------------------------------------------------------------------*
*& Include MZFI_WARRANTY_MAINTEANCETOP                                 *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  sapmzfi_warranty_mainteance   .
TABLES :ztfi_warranty,ztfi_warranty_str,
        ztfi_warranty_h.                                    "HIS20094
*---// Table controls
CONTROLS: tc1 TYPE TABLEVIEW USING SCREEN 9000.

DATA: it_source LIKE ztfi_warranty OCCURS 0 WITH HEADER LINE,
      it_src_dl LIKE ztfi_warranty OCCURS 0 WITH HEADER LINE,
      versn LIKE ztfi_warranty-versn,
* BEGIN OF HIS20094
      it_source_tmp LIKE ztfi_warranty OCCURS 0 WITH HEADER LINE.
* END OF HIS20094
DATA : wa_versn LIKE LINE OF it_source.

TYPE-POOLS: vrm.

DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list,
      l_index LIKE sy-tabix,
      save_ok LIKE sy-ucomm,
      c_flag TYPE c,
      v_emsg(1) TYPE c,                                     "HIS20094
      v_change(1) TYPE c,                                   "HIS20094
      v_first(1) TYPE c.                                    "HIS20094

DATA: g_name  TYPE vrm_id,
      g_list  TYPE vrm_values,
      g_value LIKE LINE OF g_list.
