************************************************************************
* Program Name      : ZEMMPM52E_MATERIAL
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.09.01.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K912027
* Addl Documentation:
* Description       : Material Master Management
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.09.01.     Sung-Tae Lim     UD1K912027     Initial Coding
* 05/04/2005      Furong Wang              allow user to pull SAP master
*                                          to update Z-TABLE
*
************************************************************************

REPORT zemmpm52e_material NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

TABLES : ztmm_mara,
         ztmm_auth,
         qmat,
         mbew,
         t024.

DATA : w_error_check(1),
       w_field_name(20).


**--- Internal Tables
DATA : it_ztmm_auth LIKE ztmm_auth OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_itab OCCURS 0.
        INCLUDE STRUCTURE ztmm_mara.
DATA :   w_selected(1),
         w_change(1),
       END OF it_itab.

***excel

data: col_text like GXXLT_V occurs 0 with header line.
data: online_text like GXXLT_O occurs 0,
      print_text like GXXLT_P occurs 0.


INCLUDE OLE2INCL.
DATA: H_EXCEL TYPE OLE2_OBJECT,        " Excel object
      H_MAPL TYPE OLE2_OBJECT,         " list of workbooks
      H_MAP TYPE OLE2_OBJECT,          " workbook
      H_ZL TYPE OLE2_OBJECT,           " cell
      H_F TYPE OLE2_OBJECT.            " font
DATA  H TYPE I.


DATA : it_temp LIKE it_itab OCCURS 0 WITH HEADER LINE,
       it_copy LIKE it_itab OCCURS 0 WITH HEADER LINE.

DATA : it_ztmm_mara LIKE ztmm_mara OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_fcode OCCURS 0,
         fcode LIKE sy-ucomm,
       END OF it_fcode.

*
**--- BAPI
DATA : wa_headdata             LIKE bapimathead,
       wa_clientdata           LIKE bapi_mara,
       wa_clientdatax          LIKE bapi_marax,
       wa_plantdata            LIKE bapi_marc,
       wa_plantdatax           LIKE bapi_marcx,
       wa_warehousenumberdata  LIKE bapi_mlgn,
       wa_warehousenumberdatax LIKE bapi_mlgnx,
       wa_storagetypedata      LIKE bapi_mlgt,
       wa_storagetypedatax     LIKE bapi_mlgtx,
       wa_valuationdata        LIKE bapi_mbew,
       wa_valuationdatax       LIKE bapi_mbewx,
       wa_storagelocationdata  LIKE bapi_mard,
       wa_storagelocationdatax LIKE bapi_mardx,
       wa_bapiret2             LIKE bapiret2.

DATA : it_unitsofmeasure  LIKE bapi_marm OCCURS 0 WITH HEADER LINE,
       it_unitsofmeasurex LIKE bapi_marmx OCCURS 0 WITH HEADER LINE.

DATA : it_bapiret2 LIKE bapi_matreturn2 OCCURS 0 WITH HEADER LINE.


**--- BDC
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.

DATA : w_mode LIKE ctu_params-dismode VALUE 'N'.

**--- Variables
DATA : w_okcode LIKE sy-ucomm,
       w_save_okcode LIKE sy-ucomm,
       w_loopc LIKE sy-loopc,
       w_tot_lines TYPE i,
       w_selected(1).

DATA : w_position TYPE i,
       w_found(1),
       w_find_pos TYPE i,
       w_loop_first TYPE i.

DATA : wa_tc9000 TYPE cxtab_column.

DATA : w_ind_bom(1),
       w_ind_mm(1),
       w_ind_pur(1),
       w_ind_dev(1),
       w_ind_qm(1),
       w_ind_halb(1).

DATA : w_subrc LIKE sy-subrc,
       w_messa(80).


**--- Table Control
CONTROLS : tc_9000 TYPE TABLEVIEW USING SCREEN 9000.

DATA : wa_tab TYPE cxtab_column.


**--- Constants
CONSTANTS : c_mtart_fert      LIKE mara-mtart VALUE 'FERT',
            c_mtart_halb      LIKE mara-mtart VALUE 'HALB',
            c_mtart_roh       LIKE mara-mtart VALUE 'ROH'.

CONSTANTS : c_bom             LIKE ztmm_auth-func1 VALUE 'BOM',
            c_mm              LIKE ztmm_auth-func1 VALUE 'MM',
            c_pur             LIKE ztmm_auth-func1 VALUE 'PUR',
            c_dev             LIKE ztmm_auth-func1 VALUE 'DEV',
            c_qm              LIKE ztmm_auth-func1 VALUE 'QM',
            c_halb            LIKE ztmm_auth-func1 VALUE 'HALB'.

CONSTANTS : c_lgnum_p01       LIKE ztmm_mara-lgnum VALUE 'P01'.

CONSTANTS : c_werks_p001      LIKE t001w-werks VALUE 'P001',
            c_werks_e001      LIKE t001w-werks VALUE 'E001',
            c_lgort_p400      LIKE t001l-lgort VALUE 'P400',
            c_lgort_p500      LIKE t001l-lgort VALUE 'P500',
            c_lgort_e100      LIKE t001l-lgort VALUE 'E100',
            c_lgort_e110      LIKE t001l-lgort VALUE 'E110',
            c_profl_k         LIKE mara-profl VALUE 'K',
            c_profl_v         LIKE mara-profl VALUE 'V',
            c_profl_m         LIKE mara-profl VALUE 'M',
            c_lgtyp_422       LIKE ztmm_mara-lgtyp VALUE '422',
            c_lgtyp_431       LIKE ztmm_mara-lgtyp VALUE '431',
            c_lgtyp_432       LIKE ztmm_mara-lgtyp VALUE '432',
            c_lgtyp_433       LIKE ztmm_mara-lgtyp VALUE '433',
            c_lgtyp_434       LIKE ztmm_mara-lgtyp VALUE '434',
            c_lgtyp_435       LIKE ztmm_mara-lgtyp VALUE '435',
            c_lgtyp_436       LIKE ztmm_mara-lgtyp VALUE '436',
            c_matkl_nf_kd     LIKE mara-matkl VALUE 'NF-KD',
            c_matkl_nf_kd_en  LIKE mara-matkl VALUE 'NF-KD-EN',
            c_matkl_nf_kd_tm  LIKE mara-matkl VALUE 'NF-KD-TM',
            c_matkl_cm_kd     LIKE mara-matkl VALUE 'CM-KD',
            c_matkl_cm_kd_en  LIKE mara-matkl VALUE 'CM-KD-EN',
            c_matkl_cm_kd_tm  LIKE mara-matkl VALUE 'CM-KD-TM',
            c_matkl_co_kd     LIKE mara-matkl VALUE 'CO-KD',
            c_matkl_co_kd_en  LIKE mara-matkl VALUE 'CO-KD-EN',
            c_matkl_co_kd_tm  LIKE mara-matkl VALUE 'CO-KD-TM',
            c_matkl_nf_lp     LIKE mara-matkl VALUE 'NF-LP',
            c_matkl_cm_lp     LIKE mara-matkl VALUE 'CM-LP',
            c_matkl_co_lp     LIKE mara-matkl VALUE 'CO-LP',
            c_mtpos_mara_znor LIKE ztmm_mara-mtpos_mara VALUE 'ZNOR',
            c_disls_ex        LIKE ztmm_mara-disls VALUE 'EX',
            c_disls_pk        LIKE ztmm_mara-disls VALUE 'PK',
            c_disls_wb        LIKE ztmm_mara-disls VALUE 'WB',
            c_sobsl_40        LIKE ztmm_mara-sobsl VALUE '40',
            c_vspvb_b1        LIKE ztmm_mara-vspvb VALUE 'B1',
            c_vspvb_p3        LIKE ztmm_mara-vspvb VALUE 'P3',
            c_vspvb_t1        LIKE ztmm_mara-vspvb VALUE 'T1',
            c_vspvb_t2        LIKE ztmm_mara-vspvb VALUE 'T2',
            c_vspvb_t3        LIKE ztmm_mara-vspvb VALUE 'T3',
            c_vspvb_c1        LIKE ztmm_mara-vspvb VALUE 'C1',
            c_vspvb_c2        LIKE ztmm_mara-vspvb VALUE 'C2',
            c_vspvb_f1        LIKE ztmm_mara-vspvb VALUE 'F1',
            c_vspvb_f2        LIKE ztmm_mara-vspvb VALUE 'F2',
            c_vspvb_f3        LIKE ztmm_mara-vspvb VALUE 'F3',
            c_vspvb_f4        LIKE ztmm_mara-vspvb VALUE 'F4',
            c_vspvb_ok        LIKE ztmm_mara-vspvb VALUE 'OK',
            c_vspvb_t1s       LIKE ztmm_mara-vspvb VALUE 'T1S',
            c_vspvb_t2s       LIKE ztmm_mara-vspvb VALUE 'T2S',
            c_vspvb_t3s       LIKE ztmm_mara-vspvb VALUE 'T3S',
            c_vspvb_c1s       LIKE ztmm_mara-vspvb VALUE 'C1S',
            c_vspvb_c2s       LIKE ztmm_mara-vspvb VALUE 'C2S',
            c_vspvb_f1s       LIKE ztmm_mara-vspvb VALUE 'F1S',
            c_vspvb_f2s       LIKE ztmm_mara-vspvb VALUE 'F2S',
            c_vspvb_f3s       LIKE ztmm_mara-vspvb VALUE 'F3S',
            c_vspvb_f4s       LIKE ztmm_mara-vspvb VALUE 'F4S',
            c_vspvb_eng       LIKE ztmm_mara-vspvb VALUE 'ENG',
            c_dispo_m02       LIKE ztmm_mara-dispo VALUE 'M02',
            c_tempb_11        LIKE ztmm_mara-tempb VALUE '11',
            c_tempb_12        LIKE ztmm_mara-tempb VALUE '12',
            c_raube_11        LIKE ztmm_mara-raube VALUE '11',
            c_raube_12        LIKE ztmm_mara-raube VALUE '12',
            c_raube_13        LIKE ztmm_mara-raube VALUE '13',
            c_raube_14        LIKE ztmm_mara-raube VALUE '14',
            c_abcin_a         LIKE ztmm_mara-abcin VALUE 'A',
            c_abcin_b         LIKE ztmm_mara-abcin VALUE 'B',
            c_abcin_c         LIKE ztmm_mara-abcin VALUE 'C',
            c_abcin_d         LIKE ztmm_mara-abcin VALUE 'D',
            c_bklas_3000      LIKE ztmm_mara-bklas VALUE '3000',
            c_bklas_3001      LIKE ztmm_mara-bklas VALUE '3001',
            c_bklas_3005      LIKE ztmm_mara-bklas VALUE '3005',
            c_ltkza_003       LIKE ztmm_mara-ltkza VALUE '003',
            c_ltkza_004       LIKE ztmm_mara-ltkza VALUE '004',
            c_gewei_kg        LIKE ztmm_mara-gewei VALUE 'KG',
            c_mmsta_11        LIKE marc-mmsta VALUE '11',
            c_beskz_f         LIKE marc-beskz VALUE 'F',
            c_sbdkz_2         LIKE marc-sbdkz VALUE '2',
            c_dispo_001       LIKE ztmm_mara-dispo VALUE '001'.
*            C_DISPO_M02       LIKE ztmm_mara-dispo VALUE 'M02'.


**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_mtart FOR mara-mtart OBLIGATORY NO INTERVALS
                                        NO-EXTENSION,
                 s_matnr FOR mara-matnr,
                 s_werks FOR marc-werks NO INTERVALS NO-EXTENSION,
                 s_lgort FOR mard-lgort NO INTERVALS NO-EXTENSION,
                 s_lgtyp FOR ztmm_mara-lgtyp,
                 s_profl FOR mara-profl NO INTERVALS NO-EXTENSION,
                 s_duedt FOR ztmm_mara-duedt,
                 s_matkl FOR ztmm_mara-matkl,
                 s_ekgrp FOR t024-ekgrp,
                 s_stawn FOR marc-stawn,
                 s_dispo FOR ztmm_mara-dispo,
                 s_disls FOR marc-disls NO INTERVALS NO-EXTENSION,
                 s_vspvb FOR marc-vspvb,
                 s_erdat FOR ztmm_mara-erdat,
                 s_ernam FOR ztmm_mara-ernam.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : r_01 RADIOBUTTON GROUP gr1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT (25) text-002 FOR FIELD r_01.
SELECTION-SCREEN COMMENT (01) text-005.                     " (
PARAMETERS : r_011 RADIOBUTTON GROUP gr11 DEFAULT 'X' USER-COMMAND r01.
SELECTION-SCREEN COMMENT (06) text-006 FOR FIELD r_011.     " master
PARAMETERS : r_012 RADIOBUTTON GROUP gr11.
SELECTION-SCREEN COMMENT (05) text-007 FOR FIELD r_012.     " mm
PARAMETERS : r_013 RADIOBUTTON GROUP gr11.
SELECTION-SCREEN COMMENT (05) text-008 FOR FIELD r_013.     " pur
PARAMETERS : r_014 RADIOBUTTON GROUP gr11.
SELECTION-SCREEN COMMENT (05) text-009 FOR FIELD r_014.     " dev
PARAMETERS : r_015 RADIOBUTTON GROUP gr11.
SELECTION-SCREEN COMMENT (05) text-010 FOR FIELD r_015.     " qm
SELECTION-SCREEN COMMENT (01) text-011.                     " )
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : r_02 RADIOBUTTON GROUP gr1.
SELECTION-SCREEN COMMENT (25) text-003 FOR FIELD r_02.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : r_03 RADIOBUTTON GROUP gr1.
SELECTION-SCREEN COMMENT (25) text-004 FOR FIELD r_03.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK block2.

SELECTION-SCREEN END OF BLOCK block1.


**---
*AT SELECTION-SCREEN ON RADIOBUTTON GROUP gr1.
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'R01'.
      IF r_01 EQ space.
        MOVE : 'X'   TO r_01,
               space TO r_02,
               space TO r_03.
      ENDIF.
  ENDCASE.


**---
INITIALIZATION.


**---
TOP-OF-PAGE.


**---
START-OF-SELECTION.
  PERFORM check_authority.

*---
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    it_temp[] = it_itab[].
    CALL SCREEN 9000.
  ENDIF.




**---

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : it_itab, it_itab[], it_temp, it_temp[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
           FROM ztmm_mara
          WHERE mtart IN s_mtart
            AND matnr IN s_matnr
            AND werks IN s_werks
            AND lgort IN s_lgort
            AND lgtyp IN s_lgtyp
            AND profl IN s_profl
            AND duedt IN s_duedt
            AND matkl IN s_matkl
            AND ekgrp IN s_ekgrp
            AND stawn IN s_stawn
            AND dispo IN s_dispo
            AND disls IN s_disls
            AND vspvb IN s_vspvb
            AND erdat IN s_erdat
            AND ernam IN s_ernam
            AND loekz EQ space.     " deletion flag

*---
  IF r_01 NE space.     " only incomplete
    IF r_011 NE space.     " master
      DELETE it_itab WHERE conf5 NE space.
    ELSEIF r_012 NE space.     " mm
      DELETE it_itab WHERE conf1 NE space.
    ELSEIF r_013 NE space.     " pur
      DELETE it_itab WHERE conf2 NE space.
    ELSEIF r_014 NE space.     " dev
      DELETE it_itab WHERE conf3 NE space.
    ELSEIF r_015 NE space.     " qm
      DELETE it_itab WHERE conf4 NE space.
    ENDIF.
  ELSEIF r_02 NE space.     " only complete
    DELETE it_itab WHERE conf5 EQ space.
  ELSEIF r_03 NE space.     " all the records

  ENDIF.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  check_authority
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_authority.
*---
  CLEAR : ztmm_auth, it_ztmm_auth, it_ztmm_auth[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_auth
           FROM ztmm_auth
          WHERE uname EQ sy-uname
            AND tcode EQ sy-tcode
            AND loekz EQ space.

  IF it_ztmm_auth[] IS INITIAL.
    MESSAGE s077(s#) WITH sy-tcode.
    STOP.
*    EXIT.
  ENDIF.

*---
  CLEAR : w_ind_mm, w_ind_pur, w_ind_dev, w_ind_qm.

  LOOP AT it_ztmm_auth.
    CASE it_ztmm_auth-func1.
      WHEN c_bom.
        MOVE : 'X' TO w_ind_bom.
      WHEN c_mm.
        MOVE : 'X' TO w_ind_mm.
      WHEN c_pur.
        MOVE : 'X' TO w_ind_pur.
      WHEN c_dev.
        MOVE : 'X' TO w_ind_dev.
      WHEN c_qm.
        MOVE : 'X' TO w_ind_qm.
      WHEN c_halb.
        MOVE : 'X' TO w_ind_halb.
    ENDCASE.
  ENDLOOP.

*---
  PERFORM get_data.
ENDFORM.                    " check_authority

*&---------------------------------------------------------------------*
*&      Module  status_scrcom  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_scrcom OUTPUT.
**---
  CASE sy-dynnr.
    WHEN '9000'.
      IF w_ind_bom EQ space.
        CLEAR : it_fcode, it_fcode[].
        MOVE : 'CREA' TO it_fcode-fcode. APPEND it_fcode.
        MOVE : 'DELE' TO it_fcode-fcode. APPEND it_fcode.
        SET PF-STATUS '9000' EXCLUDING it_fcode.
      ELSEIF w_ind_mm EQ space AND w_ind_pur EQ space AND
             w_ind_dev EQ space AND w_ind_qm EQ space.
        CLEAR : it_fcode, it_fcode[].
        MOVE : 'CONF' TO it_fcode-fcode. APPEND it_fcode.
*        MOVE : 'DELE' TO it_fcode-fcode. APPEND it_fcode.
        SET PF-STATUS '9000' EXCLUDING it_fcode.
      ELSE.
        SET PF-STATUS '9000'.
      ENDIF.
      SET TITLEBAR  '9000'.
  ENDCASE.
ENDMODULE.                 " status_scrcom  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
**---
  CASE sy-dynnr.
    WHEN '9000'.
      CASE sy-ucomm.
        WHEN 'BACK' OR 'EXIT' OR 'CANC'.
          LEAVE TO SCREEN 0.
*        WHEN 'DELE'.
*          CLEAR : w_save_okcode.
*          PERFORM delete_item.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_scrcom  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_scrcom INPUT.
**---
  MOVE : w_okcode TO w_save_okcode.

  CLEAR : w_okcode.

  CASE sy-dynnr.
    WHEN '9000'.
      CASE w_save_okcode.
        WHEN 'CREA'.
          CLEAR : w_save_okcode.
          PERFORM change_material_master.
          PERFORM check_authority.
*---
          IF it_itab[] IS INITIAL.
            MESSAGE s999 WITH text-m01.
          ELSE.
            it_temp[] = it_itab[].
            LEAVE TO SCREEN 9000.
          ENDIF.
        WHEN 'SAVE'.
          CLEAR : w_save_okcode.
          PERFORM check_input_value.
          PERFORM save_routine.
          PERFORM check_authority.
*---
          IF it_itab[] IS INITIAL.
            MESSAGE s999 WITH text-m01.
          ELSE.
            it_temp[] = it_itab[].
            LEAVE TO SCREEN 9000.
          ENDIF.
        WHEN 'CONF'.
          CLEAR : w_save_okcode.
          PERFORM check_input_value.
          PERFORM save_routine.
          PERFORM check_authority.
*---
          IF it_itab[] IS INITIAL.
            MESSAGE s999 WITH text-m01.
          ELSE.
            it_temp[] = it_itab[].
            LEAVE TO SCREEN 9000.
          ENDIF.
        WHEN 'DELE'.
          CLEAR : w_save_okcode.
          PERFORM delete_item.
        WHEN 'SALL'.
          CLEAR : w_save_okcode.
          PERFORM select_deselect_all_9000 USING 'X'.
        WHEN 'DALL'.
          CLEAR : w_save_okcode.
          PERFORM select_deselect_all_9000 USING ' '.
        WHEN 'ASCE'.
          CLEAR : w_save_okcode.
          PERFORM ascending_sort.
        WHEN 'DESC'.
          CLEAR : w_save_okcode.
          PERFORM descending_sort.
        WHEN 'FIND' OR 'FIND+'.
          PERFORM find_string.
          CLEAR : w_save_okcode.
        WHEN 'P--' OR 'P-' OR 'P+' OR 'P++'.     " Page Scroll
          PERFORM table_control_page_scrol USING w_save_okcode
                                                 tc_9000-top_line
                                                 w_tot_lines
                                                 w_loopc.
          CLEAR : w_save_okcode.
        WHEN 'GETM'.
          CLEAR : w_save_okcode.
          perform get_from_material_master.
        WHEN 'EXCE'.
          CLEAR : w_save_okcode.
          perform save_data_to_excel.
        ENDCASE.
  ENDCASE.
ENDMODULE.                 " user_command_scrcom  INPUT

*&---------------------------------------------------------------------*
*&      Form  select_deselect_all_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0421   text
*----------------------------------------------------------------------*
FORM select_deselect_all_9000 USING    p_value.
**---
  MOVE : p_value TO it_itab-w_selected.

  MODIFY it_itab TRANSPORTING w_selected WHERE matnr NE space
                                           AND conf5 EQ space.
ENDFORM.                    " select_deselect_all_9000

*&---------------------------------------------------------------------*
*&      Form  ascending_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ascending_sort.
*---
  LOOP AT tc_9000-cols INTO wa_tc9000.
    IF wa_tc9000-selected = 'X'.
      SORT it_itab BY (wa_tc9000-screen-name+10) ASCENDING.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ascending_sort

*&---------------------------------------------------------------------*
*&      Form  descending_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM descending_sort.
*---
  LOOP AT tc_9000-cols INTO wa_tc9000.
    IF wa_tc9000-selected = 'X'.
      SORT it_itab BY (wa_tc9000-screen-name+10) DESCENDING.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " descending_sort

*&---------------------------------------------------------------------*
*&      Form  find_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_string.
**---
  IF w_save_okcode EQ 'FIND'.
    MOVE : 1 TO w_position.
  ELSEIF w_save_okcode EQ 'FIND+'.
    w_position = w_loop_first + 1.
  ENDIF.

**---
  IF w_save_okcode EQ 'FIND'.
    PERFORM popup_get_value(sapfsfxx) USING    'FSTR' ' '
                                      CHANGING rsdxx-findstr.
  ENDIF.

**---
  IF sy-ucomm NE 'CANC'.
*---
    CLEAR : w_found.
    IF sy-dynnr EQ '9000'.
      LOOP AT it_itab FROM w_position.
        IF it_itab CS rsdxx-findstr OR it_itab CP rsdxx-findstr.
          MOVE : 'X' TO w_found.
          MOVE : sy-tabix TO w_find_pos.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF w_found NE space.
        MOVE : 1          TO tc_9000-current_line,
               w_find_pos TO tc_9000-top_line,
               w_find_pos TO w_loop_first.
      ELSE.
        MESSAGE s042(e2) WITH rsdxx-findstr.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " find_string

*&---------------------------------------------------------------------*
*&      Module  control_screen_scr9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE control_screen_scr9000 OUTPUT.
*---
  LOOP AT SCREEN.
*---
    IF ztmm_mara-mtart EQ c_mtart_roh.
      PERFORM control_by_mtart_roh.
    ELSEIF ztmm_mara-mtart EQ c_mtart_fert OR
           ztmm_mara-mtart EQ c_mtart_halb.
      PERFORM control_by_mtart_ferthalb.
    ENDIF.
*---
*    PERFORM control_required_field.
*---
    PERFORM control_by_confirm_status.
*---
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " control_screen_scr9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  display_data_scr9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_data_scr9000 OUTPUT.
**---
  IF w_error_check NE space.
    SET CURSOR FIELD w_field_name LINE 1.
    CLEAR : w_error_check.
  ENDIF.

**---
  IF sy-stepl EQ 1.
    CALL FUNCTION 'ME_GET_TC_LINES'
         EXPORTING
              im_lines_total    = w_tot_lines
              im_lines_per_page = sy-loopc
              im_top_line       = tc_9000-top_line
         IMPORTING
              ex_tc_lines       = tc_9000-lines
         EXCEPTIONS
              OTHERS            = 1.
  ENDIF.

**---
  CLEAR : it_itab.

  READ TABLE it_itab INDEX tc_9000-current_line.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_itab TO ztmm_mara.
    MOVE : it_itab-w_selected  TO w_selected,
           c_lgnum_p01         TO ztmm_mara-lgnum.
    PERFORM get_material_desc USING ztmm_mara-matnr.
  ELSE.
    EXIT FROM STEP-LOOP.
  ENDIF.

**---
  MOVE : sy-loopc TO w_loopc.
ENDMODULE.                 " display_data_scr9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  input_data_modify_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE input_data_modify_scr9000 INPUT.
**---
  CLEAR : it_itab.

  READ TABLE it_itab INDEX tc_9000-current_line.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING ztmm_mara TO it_itab.
    MOVE : w_selected            TO it_itab-w_selected.
*--- valuation class
    PERFORM valuation_class.
*--- source change
    CLEAR : it_temp.
    READ TABLE it_temp WITH KEY matnr = it_itab-matnr
                                werks = it_itab-werks.
    IF it_temp-profl NE ztmm_mara-profl AND
       it_itab-mtart EQ c_mtart_roh.
      CLEAR : it_itab-conf1, it_itab-conf2, it_itab-conf3,
              it_itab-conf4.
    ENDIF.
*---
    IF NOT it_itab-ntgew IS INITIAL.
      MOVE : c_gewei_kg TO it_itab-gewei,
             it_itab-ntgew TO it_itab-brgew.
    ENDIF.
*---
    MODIFY it_itab INDEX tc_9000-current_line.
  ENDIF.
ENDMODULE.                 " input_data_modify_scr9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  control_by_mtart_roh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM control_by_mtart_roh.
*---
  IF w_ind_bom NE space.
    IF screen-group1 EQ c_bom.
      screen-input = 1.
    ENDIF.
  ENDIF.
*---
  IF w_ind_mm NE space.
    IF screen-group1 EQ c_mm OR screen-name EQ 'ZTMM_MARA-CONF1'.
      screen-input = 1.
    ENDIF.
  ENDIF.
*---
  IF w_ind_pur NE space.
    IF screen-group1 EQ c_pur OR screen-name EQ 'ZTMM_MARA-CONF2'.
      screen-input = 1.
    ENDIF.
  ENDIF.
*---
  IF w_ind_dev NE space.
    IF screen-group1 EQ c_dev OR screen-name EQ 'ZTMM_MARA-CONF3'.
      screen-input = 1.
    ENDIF.
  ENDIF.
*---
  IF w_ind_qm NE space.
    IF screen-group1 EQ c_qm OR screen-name EQ 'ZTMM_MARA-CONF4'.
      screen-input = 1.
    ENDIF.
  ENDIF.
ENDFORM.                    " control_by_mtart_roh

*&---------------------------------------------------------------------*
*&      Form  control_by_mtart_ferthalb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM control_by_mtart_ferthalb.
*---
  IF w_ind_bom NE space.
    IF screen-group1 EQ c_bom.
      screen-input = 1.
    ENDIF.
  ENDIF.
*---
  IF w_ind_halb NE space.
    IF screen-name EQ 'ZTMM_MARA-NTGEW'.     " Net Weight
      screen-input = 1.
    ENDIF.
  ENDIF.
*---
  IF w_ind_qm NE space.
    IF screen-group1 EQ c_qm OR screen-name EQ 'ZTMM_MARA-CONF4'.
      screen-input = 1.
    ENDIF.
  ENDIF.
ENDFORM.                    " control_by_mtart_ferthalb

*&---------------------------------------------------------------------*
*&      Form  control_by_confirm_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM control_by_confirm_status.
*---
  IF ztmm_mara-mtart EQ c_mtart_roh.
    IF ztmm_mara-conf1 NE space.     " MM
      IF screen-group1 EQ c_mm.
        screen-input = 0.
      ENDIF.
    ENDIF.
    IF ztmm_mara-conf2 NE space.     " PUR
      IF screen-group1 EQ c_pur.
        screen-input = 0.
      ENDIF.
    ENDIF.
    IF ztmm_mara-conf3 NE space.     " DEV
      IF screen-group1 EQ c_dev.
        screen-input = 0.
      ENDIF.
    ENDIF.
    IF ztmm_mara-conf4 NE space.     " QM
      IF screen-group1 EQ c_qm.
        screen-input = 0.
      ENDIF.
    ENDIF.
  ELSEIF ztmm_mara-mtart EQ c_mtart_fert OR
         ztmm_mara-mtart EQ c_mtart_halb.
    IF ztmm_mara-conf4 NE space.     " QM
      IF screen-group1 EQ c_qm.
        screen-input = 0.
      ENDIF.
    ENDIF.
  ENDIF.

*---
  IF ztmm_mara-conf5 NE space.
    screen-input = 0.
    screen-intensified = 1.
*    screen-active = 0.
  ENDIF.
ENDFORM.                    " control_by_confirm_status

*&---------------------------------------------------------------------*
*&      Module  check_input_data_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_input_data_scr9000 INPUT.
*---
  CHECK w_selected NE space.
  CHECK w_okcode NE 'DELE'.
  check w_okcode ne 'GETM'.
  check w_okcode ne 'EXCE'.
*--- BOM Check
  IF w_ind_bom NE space.
*--- Source
    IF ztmm_mara-profl EQ space AND ztmm_mara-mtart EQ c_mtart_roh.
      SET CURSOR FIELD 'ZTMM_MARA-PROFL' LINE sy-stepl.
      MESSAGE e999 WITH text-m02.
    ENDIF.
  ENDIF.

*--- MM Check
  IF w_ind_mm NE space.
*--- Storage Location Check
    IF ztmm_mara-lgort EQ space AND ztmm_mara-mtart EQ c_mtart_roh
       and ztmm_mara-dispo ne c_dispo_m02.
      SET CURSOR FIELD 'ZTMM_MARA-LGORT' LINE sy-stepl.
      MESSAGE e999 WITH text-m03.
    ENDIF.
*--- Storage Type Check
*    IF ztmm_mara-lgtyp EQ space.
*      SET CURSOR FIELD 'ZTMM_MARA-LGTYP' LINE sy-stepl.
*      MESSAGE e999 WITH text-m04.
*    ENDIF.
*--- Material Group Check
    IF ztmm_mara-matkl EQ space AND ztmm_mara-mtart EQ c_mtart_roh.
      SET CURSOR FIELD 'ZTMM_MARA-MATKL' LINE sy-stepl.
      MESSAGE e999 WITH text-m05.
    ENDIF.
*--- GenItem CatGroup Check
*    IF ztmm_mara-mtpos_mara EQ space.
*      SET CURSOR FIELD 'ZTMM_MARA-MTPOS_MARA' LINE sy-stepl.
*      MESSAGE e999 WITH text-m06.
*    ENDIF.
*--- MRP Controller Check
    IF ztmm_mara-dispo EQ space AND ztmm_mara-mtart EQ c_mtart_roh.
      SET CURSOR FIELD 'ZTMM_MARA-DISPO' LINE sy-stepl.
      MESSAGE e999 WITH text-m07.
    ENDIF.
*--- Lot Size Check
    IF ztmm_mara-disls EQ space AND ztmm_mara-mtart EQ c_mtart_roh.
      SET CURSOR FIELD 'ZTMM_MARA-DISLS' LINE sy-stepl.
      MESSAGE e999 WITH text-m08.
    ENDIF.
*--- Default Supply Area Check
    IF ztmm_mara-lgort EQ c_lgort_p400 AND
       ztmm_mara-mtart EQ c_mtart_roh.
      IF ztmm_mara-vspvb EQ space.
        SET CURSOR FIELD 'ZTMM_MARA-VSPVB' LINE sy-stepl.
        MESSAGE e999 WITH text-m22.
      ENDIF.
    ENDIF.
  ENDIF.

*--- Parts Development Check
  IF w_ind_dev NE space.
*--- Net Weight Check
    IF ztmm_mara-ntgew EQ space and ztmm_mara-dispo ne c_dispo_m02.
      SET CURSOR FIELD 'ZTMM_MARA-NTGEW' LINE sy-stepl.
      MESSAGE e999 WITH text-m09.
    ENDIF.
*--- Purchasing Group Check
    IF ztmm_mara-ekgrp EQ space AND ztmm_mara-mtart EQ c_mtart_roh.
      SET CURSOR FIELD 'ZTMM_MARA-EKGRP' LINE sy-stepl.
      MESSAGE e999 WITH text-m10.
    ENDIF.
  ENDIF.

*--- PUR Check
  IF w_ind_pur NE space.
*--- HTS Number & Country Check
    IF ztmm_mara-profl EQ c_profl_k AND ztmm_mara-mtart EQ c_mtart_roh
       and ztmm_mara-dispo ne c_dispo_m02.
      IF ztmm_mara-stawn EQ space.
        SET CURSOR FIELD 'ZTMM_MARA-STAWN' LINE sy-stepl.
        MESSAGE e999 WITH text-m17.
      ENDIF.
      IF ztmm_mara-herkl EQ space.
        SET CURSOR FIELD 'ZTMM_MARA-HERKL' LINE sy-stepl.
        MESSAGE e999 WITH text-m18.
      ENDIF.
    ENDIF.
  ENDIF.

*--- HALB Check
*--- insert by stlim (2004/10/26)
  IF w_ind_halb NE space.
    IF ztmm_mara-mtart EQ c_mtart_halb OR
       ztmm_mara-mtart EQ c_mtart_fert.
      IF ztmm_mara-ntgew IS INITIAL.
        SET CURSOR FIELD 'ZTMM_MARA-NTGEW' LINE sy-stepl.
        MESSAGE e999 WITH text-m09.
      ENDIF.
    ENDIF.
  ENDIF.
*--- end of insert

*--- blocked by stlim (2004/10/26)
*  IF w_ind_halb NE space.
*    IF ztmm_mara-ntgew IS INITIAL.
*      SET CURSOR FIELD 'ZTMM_MARA-NTGEW' LINE sy-stepl.
*      MESSAGE e999 WITH text-m09.
*    ENDIF.
*  ENDIF.
*--- end of block
ENDMODULE.                 " check_input_data_scr9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  get_control_lines_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_control_lines_9000 OUTPUT.
**---
  DESCRIBE TABLE it_itab LINES w_tot_lines.
ENDMODULE.                 " get_control_lines_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  control_required_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM control_required_field.
**---
  IF w_ind_bom NE space.
    IF screen-group2 EQ c_bom.
      screen-required = 1.
    ENDIF.
  ENDIF.
  IF w_ind_mm NE space.
    IF screen-group2 EQ c_mm.
      screen-required = 1.
    ENDIF.
  ENDIF.
  IF w_ind_dev NE space.
    IF screen-group2 EQ c_dev.
      screen-required = 1.
    ENDIF.
  ENDIF.
ENDFORM.                    " control_required_field

*&---------------------------------------------------------------------*
*&      Module  control_column_hide_scr9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE control_column_hide_scr9000 OUTPUT.
*---
  LOOP AT SCREEN.
    PERFORM column_invisible TABLES tc_9000-cols
                             USING screen-name '1'.
  ENDLOOP.
ENDMODULE.                 " control_column_hide_scr9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  column_invisible
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_9000_COLS  text
*      -->P_SCREEN_NAME  text
*      -->P_1353   text
*----------------------------------------------------------------------*
FORM column_invisible TABLES   p_tc_9000_cols STRUCTURE wa_tab
                      USING    p_screen_name
                               p_invisible.
*---
  CHECK p_screen_name EQ 'ZTMM_MARA-LGNUM' OR
        p_screen_name EQ 'ZTMM_MARA-MTART' OR
        p_screen_name EQ 'ZTMM_MARA-MSTAE' OR
        p_screen_name EQ 'ZTMM_MARA-MSTDE' OR
        p_screen_name EQ 'ZTMM_MARA-BRGEW' OR
        p_screen_name EQ 'ZTMM_MARA-UMREN' OR
        p_screen_name EQ 'ZTMM_MARA-LGPRO' OR
        p_screen_name EQ 'ZTMM_MARA-LGFSB' OR
        p_screen_name EQ 'ZTMM_MARA-XMCNG' OR
        p_screen_name EQ 'ZTMM_MARA-LTKZA' OR
        p_screen_name EQ 'ZTMM_MARA-LTKZE' OR
        p_screen_name EQ 'ZTMM_MARA-CONF5'.

  READ TABLE p_tc_9000_cols WITH KEY screen-name = p_screen_name.

  IF sy-subrc = 0.
    MOVE : p_invisible TO p_tc_9000_cols-invisible.
    MODIFY p_tc_9000_cols INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " column_invisible

*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.
*---
  READ TABLE it_itab WITH KEY w_selected = 'X'.

  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-m30.
  ENDIF.

*---
  LOOP AT it_itab WHERE w_selected NE space
                    AND conf5 EQ space.
**--- BOM Check
    IF w_ind_bom NE space.
      PERFORM check_input_value_bom.
    ENDIF.
**--- MM Check
    IF w_ind_mm NE space.
      PERFORM check_input_value_mm.
    ENDIF.
**--- PUR Check
    IF w_ind_pur NE space.
      PERFORM check_input_value_pur.
    ENDIF.

*--- Valuation Class
    PERFORM valuation_class.
*--- Warehouse Number
    IF it_itab-lgtyp NE space.
      MOVE : c_lgnum_p01 TO it_itab-lgnum.
    ENDIF.
*--- Gross Weight
    MOVE : it_itab-ntgew TO it_itab-brgew.
*--- Unit of Measure (additional)
    IF it_itab-meinh NE space AND it_itab-umrez NE space.
      MOVE : '1' TO it_itab-umren.
    ENDIF.
*--- Issue Storage Location & Storage Location for EP
    MOVE : it_itab-lgort TO it_itab-lgpro,
           it_itab-lgort TO it_itab-lgfsb.
*--- Negative Stocks in Plant
    IF it_itab-tempb EQ c_tempb_11.
      MOVE : 'X' TO it_itab-xmcng.
    ENDIF.
*--- Stock Removal & Stock Placement
    IF it_itab-lgtyp EQ space.
      MOVE : space TO it_itab-ltkza,
             space TO it_itab-ltkze.
    ELSEIF it_itab-lgtyp EQ c_lgtyp_422.
      MOVE : c_ltkza_003 TO it_itab-ltkza,
             c_ltkza_003 TO it_itab-ltkze.
    ELSE.
      MOVE : c_ltkza_004 TO it_itab-ltkza,
             c_ltkza_004 TO it_itab-ltkze.
    ENDIF.
*--- if source = 'V', then HTS & country : initialize
*---                       PUR confirmation ind : 'X'
    IF it_itab-profl EQ c_profl_v or it_itab-dispo = c_dispo_m02.
      CLEAR : it_itab-stawn, it_itab-herkl.
      MOVE : 'X' TO it_itab-conf2.
    ENDIF.
**--- if 'HALB' and source = 'K', then QM confirmation ind : 'X'
*    IF it_itab-mtart EQ c_mtart_halb.
*      IF it_itab-profl EQ c_profl_k.
*        MOVE : 'X' TO it_itab-conf4.
*      ENDIF.
*    ENDIF.
*--- if 'ROH' and source = 'K', then QM confirmation ind : 'X'
    IF it_itab-mtart EQ c_mtart_roh.
      IF it_itab-profl EQ c_profl_k.
        MOVE : 'X' TO it_itab-conf4.
      ENDIF.
    ENDIF.
*---
    MODIFY it_itab.
    CLEAR : it_itab.
  ENDLOOP.

*--- Special Procurement : '40' => copy Record
  CLEAR : it_copy, it_copy[].

  LOOP AT it_itab WHERE w_selected NE space
                    AND conf5 EQ space      " completed
                    AND conf1 NE space      " MM
                    AND sobsl EQ c_sobsl_40.
    MOVE-CORRESPONDING it_itab TO it_copy.
    CASE it_itab-werks.
      WHEN c_werks_p001.
        MOVE : space        TO it_copy-w_selected,
               c_werks_e001 TO it_copy-werks,
               c_lgort_e110 TO it_copy-lgort,
               space        TO it_copy-lgnum,
               space        TO it_copy-lgtyp,
               space        TO it_copy-sobsl,
               c_lgort_e110 TO it_copy-lgpro,
               c_lgort_e110 TO it_copy-lgfsb,
               space        TO it_copy-vspvb,
               space        TO it_copy-ltkza,
               space        TO it_copy-ltkze,
               space        TO it_copy-lgpla,
               space        TO it_copy-rdmng,
               space        TO it_copy-conf1,
               space        TO it_copy-conf2,
               space        TO it_copy-conf3,
               space        TO it_copy-conf4,
               space        TO it_copy-conf5,
               space        TO it_copy-messa.
      WHEN c_werks_e001.
        MOVE : space        TO it_copy-w_selected,
               c_werks_p001 TO it_copy-werks,
               c_lgort_p400 TO it_copy-lgort,
               c_lgnum_p01  TO it_copy-lgnum,
               c_lgtyp_422  TO it_copy-lgtyp,
               space        TO it_copy-sobsl,
               c_lgort_p400 TO it_copy-lgpro,
               c_lgort_p400 TO it_copy-lgfsb,
               space        TO it_copy-vspvb,
               space        TO it_copy-rgekz,
               c_ltkza_003  TO it_copy-ltkza,
               c_ltkza_003  TO it_copy-ltkze,
               space        TO it_copy-lgpla,
               space        TO it_copy-rdmng,
               space        TO it_copy-conf1,
               space        TO it_copy-conf2,
               space        TO it_copy-conf3,
               space        TO it_copy-conf4,
               space        TO it_copy-conf5,
               space        TO it_copy-messa.
    ENDCASE.
*--- existence check
    CLEAR : ztmm_mara.
    SELECT SINGLE matnr INTO ztmm_mara-matnr
                        FROM ztmm_mara
                       WHERE matnr EQ it_copy-matnr
                         AND werks EQ it_copy-werks.
    CHECK sy-subrc NE 0.
*---
    APPEND it_copy.
    CLEAR : it_itab, it_copy.
  ENDLOOP.
ENDFORM.                    " check_input_value

*&---------------------------------------------------------------------*
*&      Form  check_input_value_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value_bom.
*---
  IF it_itab-mtart EQ c_mtart_roh.
*  CHECK it_itab-mtart EQ c_mtart_roh.

*--- Source Check
    IF NOT ( it_itab-profl EQ c_profl_k OR it_itab-profl EQ c_profl_v ).
      PERFORM show_message_top_line USING text-m14 'ZTMM_MARA-PROFL'.
    ELSEIF it_itab-profl EQ space.
      PERFORM show_message_top_line USING text-m02 'ZTMM_MARA-PROFL'.
    ENDIF.

  ELSE.

    IF NOT ( it_itab-profl EQ c_profl_m OR it_itab-profl EQ space ).
      PERFORM show_message_top_line USING text-m36 'ZTMM_MARA-PROFL'.
    ENDIF.

  ENDIF.
ENDFORM.                    " check_input_value_bom

*&---------------------------------------------------------------------*
*&      Form  check_input_value_mm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value_mm.
*---
  CHECK it_itab-mtart EQ c_mtart_roh.

*--- Storage Location Check
*  IF it_itab-werks EQ c_werks_p001.
*    IF NOT ( it_itab-lgort EQ c_lgort_p400 OR
*             it_itab-lgort EQ c_lgort_p500 ).
*      PERFORM show_message_top_line USING text-m11 'ZTMM_MARA-LGORT'.
*    ENDIF.
*  ELSEIF it_itab-werks EQ c_werks_e001.
*    IF NOT ( it_itab-lgort EQ c_lgort_e100 OR
*             it_itab-lgort EQ c_lgort_e110 ).
*      PERFORM show_message_top_line USING text-m12 'ZTMM_MARA-LGORT'.
*    ENDIF.
*  ELSE.
*    PERFORM show_message_top_line USING text-m03 'ZTMM_MARA-LGORT'.
*  ENDIF.
*--- Storage Type Check
  IF it_itab-lgort EQ c_lgort_p400.
    IF NOT ( it_itab-lgtyp EQ c_lgtyp_422 OR
             it_itab-lgtyp EQ c_lgtyp_431 OR
             it_itab-lgtyp EQ c_lgtyp_432 OR
             it_itab-lgtyp EQ c_lgtyp_433 OR
             it_itab-lgtyp EQ c_lgtyp_434 OR
             it_itab-lgtyp EQ c_lgtyp_435 OR
             it_itab-lgtyp EQ c_lgtyp_436 ).
      PERFORM show_message_top_line USING text-m13 'ZTMM_MARA-LGTYP'.
    ENDIF.
  ELSE.
    IF it_itab-lgtyp NE space.
      PERFORM show_message_top_line USING text-m04 'ZTMM_MARA-LGTYP'.
    ENDIF.
  ENDIF.
*--- Material Group Check
  IF it_itab-profl EQ c_profl_k.
    IF NOT ( it_itab-matkl EQ c_matkl_nf_kd    OR
             it_itab-matkl EQ c_matkl_nf_kd_en OR
             it_itab-matkl EQ c_matkl_nf_kd_tm OR
             it_itab-matkl EQ c_matkl_cm_kd    OR
             it_itab-matkl EQ c_matkl_cm_kd_en OR
             it_itab-matkl EQ c_matkl_cm_kd_tm OR
             it_itab-matkl EQ c_matkl_co_kd    OR
             it_itab-matkl EQ c_matkl_co_kd_en OR
             it_itab-matkl EQ c_matkl_co_kd_tm ).
      PERFORM show_message_top_line USING text-m05 'ZTMM_MARA-MATKL'.
    ENDIF.
  ELSEIF it_itab-profl EQ c_profl_v.
    IF NOT ( it_itab-matkl EQ c_matkl_nf_lp OR
             it_itab-matkl EQ c_matkl_cm_lp OR
             it_itab-matkl EQ c_matkl_co_lp ).
      PERFORM show_message_top_line USING text-m05 'ZTMM_MARA-MATKL'.
    ENDIF.
  ENDIF.
*--- General Item Category Group
  IF NOT ( it_itab-mtpos_mara EQ c_mtpos_mara_znor OR
           it_itab-mtpos_mara EQ space ).
    PERFORM show_message_top_line USING text-m06 'ZTMM_MARA-MTPOS_MARA'.
  ENDIF.
  IF it_itab-lgort EQ c_lgort_p400.
    IF it_itab-mtpos_mara EQ c_mtpos_mara_znor.
      PERFORM show_message_top_line USING text-m06
                                          'ZTMM_MARA-MTPOS_MARA'.
    ENDIF.
  ENDIF.
*--- Case & QpC Check
  IF NOT ( ( it_itab-meinh EQ space AND it_itab-umrez EQ space ) OR
           ( it_itab-meinh NE space AND it_itab-umrez NE space ) ).
    PERFORM show_message_top_line USING text-m15 'ZTMM_MARA-MEINH'.
  ENDIF.
*--- JIT Schedule Indicator Check
  IF NOT ( it_itab-fabkz EQ '1' OR it_itab-fabkz EQ space ).
    PERFORM show_message_top_line USING text-m16 'ZTMM_MARA-FABKZ'.
  ELSE.
    IF it_itab-fabkz EQ '1'.
      IF it_itab-tempb EQ '11'.
        PERFORM show_message_top_line USING text-m16 'ZTMM_MARA-FABKZ'.
      ENDIF.
      IF it_itab-profl EQ c_profl_k.
        PERFORM show_message_top_line USING text-m16 'ZTMM_MARA-FABKZ'.
      ENDIF.
    ENDIF.
  ENDIF.
*--- MRP controller check
  IF it_itab-dispo EQ c_dispo_001.
    PERFORM show_message_top_line USING text-m37 'ZTMM_MARA-DISPO'.
  ENDIF.
*--- Lot Size Check
  IF it_itab-disls EQ c_disls_ex.
    IF it_itab-profl EQ c_profl_k.
      PERFORM show_message_top_line USING text-m20 'ZTMM_MARA-DISLS'.
    ENDIF.
  ENDIF.
  IF it_itab-disls EQ c_disls_pk.
    IF it_itab-profl EQ c_profl_k.
      PERFORM show_message_top_line USING text-m20 'ZTMM_MARA-DISLS'.
    ENDIF.
  ENDIF.
  IF it_itab-disls EQ c_disls_wb.
    IF it_itab-profl EQ c_profl_v.
      PERFORM show_message_top_line USING text-m20 'ZTMM_MARA-DISLS'.
    ENDIF.
  ENDIF.
*--- Special Procurement Check
  IF NOT ( it_itab-sobsl EQ c_sobsl_40 OR it_itab-sobsl EQ space ).
    PERFORM show_message_top_line USING text-m21 'ZTMM_MARA-SOBSL'.
  ENDIF.
*--- Default Supply Area Check
  IF it_itab-werks EQ c_werks_p001.
    IF it_itab-lgort EQ c_lgort_p400.
      IF NOT ( it_itab-vspvb EQ c_vspvb_b1 OR
               it_itab-vspvb EQ c_vspvb_p3 OR
               it_itab-vspvb EQ c_vspvb_t1 OR
               it_itab-vspvb EQ c_vspvb_t2 OR
               it_itab-vspvb EQ c_vspvb_t3 OR
               it_itab-vspvb EQ c_vspvb_c1 OR
               it_itab-vspvb EQ c_vspvb_c2 OR
               it_itab-vspvb EQ c_vspvb_f1 OR
               it_itab-vspvb EQ c_vspvb_f2 OR
               it_itab-vspvb EQ c_vspvb_f3 OR
               it_itab-vspvb EQ c_vspvb_f4 OR
               it_itab-vspvb EQ c_vspvb_ok ).
        PERFORM show_message_top_line USING text-m22 'ZTMM_MARA-VSPVB'.
      ENDIF.
    ELSEIF it_itab-lgort EQ c_lgort_p500.
      IF NOT ( it_itab-vspvb EQ c_vspvb_t1s OR
               it_itab-vspvb EQ c_vspvb_t2s OR
               it_itab-vspvb EQ c_vspvb_t3s OR
               it_itab-vspvb EQ c_vspvb_c1s OR
               it_itab-vspvb EQ c_vspvb_c2s OR
               it_itab-vspvb EQ c_vspvb_f1s OR
               it_itab-vspvb EQ c_vspvb_f2s OR
               it_itab-vspvb EQ c_vspvb_f3s OR
               it_itab-vspvb EQ c_vspvb_f4s OR
               it_itab-vspvb EQ c_vspvb_eng OR
               it_itab-vspvb EQ space ).
        PERFORM show_message_top_line USING text-m22 'ZTMM_MARA-VSPVB'.
      ENDIF.
    ENDIF.
  ELSEIF it_itab-werks EQ c_werks_e001.
    IF it_itab-vspvb NE space.
      PERFORM show_message_top_line USING text-m22 'ZTMM_MARA-VSPVB'.
    ENDIF.
  ENDIF.
*--- Backflush Check
  IF NOT ( it_itab-rgekz EQ '1' OR it_itab-rgekz EQ space ).
    PERFORM show_message_top_line USING text-m23 'ZTMM_MARA-RGEKZ'.
  ELSEIF it_itab-rgekz EQ '1'.
    IF it_itab-dispo EQ c_dispo_m02.
      PERFORM show_message_top_line USING text-m33 'ZTMM_MARA-RGEKZ'.
    ENDIF.
    IF it_itab-matkl CP '++++++EN' OR it_itab-matkl CP '++++++TM'.
      PERFORM show_message_top_line USING text-m34 'ZTMM_MARA-RGEKZ'.
    ENDIF.
*    IF it_itab-dispo EQ c_dispo_m02 OR
*     ( it_itab-matkl CP '++++++EN' OR it_itab-matkl CP '++++++TM' ).
*      PERFORM show_message_top_line USING text-m23 'ZTMM_MARA-RGEKZ'.
*    ENDIF.
  ENDIF.
*--- Planned Delivery Time Check
  IF it_itab-profl EQ c_profl_k.
    IF it_itab-plifz EQ space  and it_itab-dispo <> C_DISPO_M02.
      PERFORM show_message_top_line USING text-m24 'ZTMM_MARA-PLIFZ'.
    ENDIF.
  ENDIF.
*--- Planned Delivery Calendar
  IF it_itab-disls EQ c_disls_pk.
    IF it_itab-mrppp EQ space.
      PERFORM show_message_top_line USING text-m25 'ZTMM_MARA-MRPPP'.
    ENDIF.
  ENDIF.
*--- Backflush Cycle Check
 IF NOT ( it_itab-tempb EQ c_tempb_11 OR it_itab-tempb EQ c_tempb_12 OR
                                               it_itab-tempb EQ space ).
    PERFORM show_message_top_line USING text-m26 'ZTMM_MARA-TEMPB'.
  ELSEIF it_itab-tempb EQ c_tempb_11.
    IF it_itab-lgort NE c_lgort_p500.
      PERFORM show_message_top_line USING text-m26 'ZTMM_MARA-TEMPB'.
    ENDIF.
  ENDIF.
*--- Shop Check
  IF NOT ( it_itab-raube EQ c_raube_11 OR
           it_itab-raube EQ c_raube_12 OR
           it_itab-raube EQ c_raube_13 OR
           it_itab-raube EQ c_raube_14 OR
           it_itab-raube EQ space ).
    PERFORM show_message_top_line USING text-m27 'ZTMM_MARA-RAUBE'.
  ENDIF.
*--- ABC Indicator Check
  IF NOT ( it_itab-abcin EQ c_abcin_a OR
           it_itab-abcin EQ c_abcin_b OR
           it_itab-abcin EQ c_abcin_c OR
           it_itab-abcin EQ c_abcin_d OR
           it_itab-abcin EQ space ).
    PERFORM show_message_top_line USING text-m28 'ZTMM_MARA-ABCIN'.
  ENDIF.
*--- Storage Bin Check
  IF it_itab-lgtyp EQ space.
    IF it_itab-lgpla NE space.
      PERFORM show_message_top_line USING text-m29 'ZTMM_MARA-LGPLA'.
    ENDIF.
  ELSEIF it_itab-lgtyp NE space.
    IF it_itab-lgpla EQ space.
      PERFORM show_message_top_line USING text-m29 'ZTMM_MARA-LGPLA'.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_input_value_mm

*&---------------------------------------------------------------------*
*&      Form  show_message_top_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_M29  text
*----------------------------------------------------------------------*
FORM show_message_top_line USING    p_text p_field_name.
*---
  MESSAGE s999 WITH p_text.

  MOVE : sy-tabix TO tc_9000-top_line.
  MOVE : 'X' TO w_error_check,
         p_field_name TO w_field_name.

  LEAVE TO SCREEN sy-dynnr.
ENDFORM.                    " show_message_top_line

*&---------------------------------------------------------------------*
*&      Form  valuation_class
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valuation_class.
*---
  IF it_itab-profl EQ c_profl_k.
    MOVE : c_bklas_3000 TO it_itab-bklas.
  ELSEIF it_itab-profl EQ c_profl_v.
    IF it_itab-tempb EQ c_tempb_11.
      MOVE : c_bklas_3005 TO it_itab-bklas.
    ELSE.
      MOVE : c_bklas_3001 TO it_itab-bklas.
    ENDIF.
  ENDIF.
ENDFORM.                    " valuation_class

*&---------------------------------------------------------------------*
*&      Form  check_input_value_pur
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value_pur.
*---
  CHECK it_itab-mtart EQ c_mtart_roh.

*--- HTS Number & Country Check
  IF it_itab-profl EQ c_profl_v.
    IF it_itab-stawn NE space OR it_itab-herkl NE space.
      PERFORM show_message_top_line USING text-m19 'ZTMM_MARA-STAWN'.
    ENDIF.
  ELSEIF it_itab-profl EQ c_profl_k and it_itab-dispo ne c_dispo_m02.
    IF it_itab-stawn EQ space OR it_itab-herkl EQ space.
      PERFORM show_message_top_line USING text-m19 'ZTMM_MARA-STAWN'.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_input_value_pur

*&---------------------------------------------------------------------*
*&      Form  save_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_routine.
*---
  READ TABLE it_itab WITH KEY w_selected = 'X'.

  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-m30.
  ENDIF.

*---
  CLEAR : it_ztmm_mara, it_ztmm_mara[].

  LOOP AT it_itab WHERE w_selected NE space.
    MOVE-CORRESPONDING it_itab TO it_ztmm_mara.
    MOVE : sy-datum            TO it_ztmm_mara-aedat,
           sy-uzeit            TO it_ztmm_mara-aezet,
           sy-uname            TO it_ztmm_mara-aenam.
*---
    IF it_itab-mtart EQ c_mtart_halb OR it_itab-mtart EQ c_mtart_fert.
      MOVE : 'X'               TO it_ztmm_mara-conf1,
             'X'               TO it_ztmm_mara-conf2,
             'X'               TO it_ztmm_mara-conf3.
*             'X'               TO it_ztmm_mara-conf4.
*--- insert by stlim (2004/10/26)
      MOVE : 'X'               TO it_ztmm_mara-conf4.
*--- end of insert
    ENDIF.
*---
    APPEND it_ztmm_mara.
    CLEAR : it_itab, it_ztmm_mara.
  ENDLOOP.

  LOOP AT it_copy.
    MOVE-CORRESPONDING it_copy TO it_ztmm_mara.
    MOVE : sy-datum            TO it_ztmm_mara-aedat,
           sy-uzeit            TO it_ztmm_mara-aezet,
           sy-uname            TO it_ztmm_mara-aenam.
    APPEND it_ztmm_mara.
    CLEAR : it_itab, it_ztmm_mara.
  ENDLOOP.

  MODIFY ztmm_mara FROM TABLE it_ztmm_mara.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    MESSAGE s999 WITH text-m31.
*    LEAVE TO SCREEN 0.
  ELSE.
    ROLLBACK WORK.
    MESSAGE e999 WITH text-m32.
  ENDIF.
ENDFORM.                    " save_routine

*&---------------------------------------------------------------------*
*&      Form  change_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_material_master.
*---
  READ TABLE it_itab WITH KEY w_selected = 'X'.

  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-m30.
  ENDIF.

*---
  LOOP AT it_itab WHERE conf5 EQ space     " completed
*                    AND conf1 NE space     " MM
*                    AND conf2 NE space     " PUR
*                    AND conf3 NE space     " DEV
*                    AND conf4 NE space     " QM
                    AND w_selected NE space.
    CLEAR : w_subrc.

*---
    IF it_itab-conf1 NE space AND it_itab-conf2 NE space AND
       it_itab-conf3 NE space AND it_itab-conf4 NE space.

*--- material master change by BAPI
      PERFORM change_by_bapi.
*---
      PERFORM call_bapi_function.
*--- material master change by BDC (MM02)
      PERFORM change_by_bdc.
*--- update CBO table
      PERFORM update_table.

    ELSE.

      MOVE : text-m35 TO it_itab-messa.
      MOVE-CORRESPONDING it_itab TO ztmm_mara.
      MODIFY : ztmm_mara, it_itab.
      IF sy-subrc EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

      MESSAGE s999 WITH it_itab-messa.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " change_material_master

*&---------------------------------------------------------------------*
*&      Form  call_bapi_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_function.
*---
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
       EXPORTING
            headdata             = wa_headdata
            clientdata           = wa_clientdata
            clientdatax          = wa_clientdatax
            plantdata            = wa_plantdata
            plantdatax           = wa_plantdatax
            storagelocationdata  = wa_storagelocationdata
            storagelocationdatax = wa_storagelocationdatax
            valuationdata        = wa_valuationdata
            valuationdatax       = wa_valuationdatax
            warehousenumberdata  = wa_warehousenumberdata
            warehousenumberdatax = wa_warehousenumberdatax
            storagetypedata      = wa_storagetypedata
            storagetypedatax     = wa_storagetypedatax
       IMPORTING
            return               = wa_bapiret2
       TABLES
            unitsofmeasure       = it_unitsofmeasure
            unitsofmeasurex      = it_unitsofmeasurex
            returnmessages       = it_bapiret2.

*---
  READ TABLE it_bapiret2 WITH KEY type = 'E'.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MOVE : '4' TO w_subrc.
    MOVE : it_bapiret2-message TO it_itab-messa.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    MOVE : '0' TO w_subrc.
    MOVE : it_bapiret2-message TO it_itab-messa.
  ENDIF.
ENDFORM.                    " call_bapi_function

*&---------------------------------------------------------------------*
*&      Form  change_by_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_by_bapi.
*---
  CLEAR : wa_headdata, wa_clientdata, wa_clientdatax, wa_plantdata,
          wa_plantdatax, wa_warehousenumberdata,
          wa_warehousenumberdata, wa_storagetypedata,
          wa_storagetypedatax, it_bapiret2, it_bapiret2[],
          wa_storagelocationdata, wa_storagelocationdatax,
          it_unitsofmeasure, it_unitsofmeasure[],
          it_unitsofmeasurex, it_unitsofmeasurex[].

  IF it_itab-mtart EQ c_mtart_roh.
*--- ROH
    PERFORM change_by_bapi_roh.
  ELSEIF it_itab-mtart EQ c_mtart_halb OR it_itab-mtart EQ c_mtart_fert.
*--- HALB / FERT
    PERFORM change_by_bapi_halb.
  ENDIF.
ENDFORM.                    " change_by_bapi

*&---------------------------------------------------------------------*
*&      Form  change_by_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_by_bdc.
*---
  CHECK w_subrc EQ 0.

*---
  DATA : l_brgew(13).

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[],
          it_message, it_message[].

*---
  PERFORM dynpro USING : 'X'  'SAPLMGMM'        '0060',
                         ' '  'RMMG1-MATNR'     it_itab-matnr,
                         ' '  'BDC_OKCODE'      '/00'.

  PERFORM dynpro USING : 'X'  'SAPLMGMM'        '0070',
                         ' '  'BDC_OKCODE'      '=SELA'.

  PERFORM dynpro USING : 'X'  'SAPLMGMM'        '0070',
                         ' '  'BDC_OKCODE'      '=ENTR'.

  PERFORM dynpro USING : 'X'  'SAPLMGMM'        '0080',
                         ' '  'RMMG1-WERKS'     it_itab-werks,
                         ' '  'BDC_OKCODE'      '=ENTR'.

  WRITE : it_itab-brgew TO l_brgew UNIT it_itab-gewei.

  PERFORM dynpro USING : 'X'  'SAPLMGMM'        '5004',
                         ' '  'MARA-EXTWG'      it_itab-extwg,
                         ' '  'MARA-BRGEW'      l_brgew.

  IF it_itab-mtart EQ c_mtart_roh.

    PERFORM dynpro USING : ' '  'BDC_OKCODE'      '=SP13'.

    PERFORM dynpro USING : 'X'  'SAPLMGMM'        '5000',
                           ' '  'MARC-RGEKZ'      it_itab-rgekz,
                           ' '  'BDC_OKCODE'      '=SP23'.

    PERFORM dynpro USING : 'X'  'SAPLMGMM'        '5000',
                           ' '  'BDC_OKCODE'      '=PB01'.

    PERFORM dynpro USING : 'X'  'SAPLQPLS'        '0100',
                           ' '  'BDC_OKCODE'      '=NEU'.

    PERFORM dynpro USING : 'X'  'SAPLQPLS'        '0100',
                           ' '  'RMQAM-ART(01)'   it_itab-art01,
                           ' '  'RMQAM-AKTIV(01)' it_itab-akti1,
                           ' '  'RMQAM-ART(02)'   it_itab-art02,
                           ' '  'RMQAM-AKTIV(02)' it_itab-akti2,
                           ' '  'BDC_OKCODE'      '=WEIT'.

    PERFORM dynpro USING : 'X'  'SAPLMGMM'        '5000',
                           ' '  'BDC_OKCODE'      '=BU'.

  ELSE.

    PERFORM dynpro USING : ' '  'BDC_OKCODE'      '=BU'.

  ENDIF.


*---
  CALL TRANSACTION 'MM02' USING it_bdc
                          MODE w_mode
                          UPDATE 'S'
                          MESSAGES INTO it_mess.

  APPEND LINES OF it_mess TO it_message.

*---
  CLEAR : it_mess.

  READ TABLE it_mess WITH KEY msgtyp = 'E'.

  IF sy-subrc EQ 0.
    MOVE : '4' TO w_subrc.
    PERFORM get_message CHANGING w_messa.
    MOVE : w_messa TO it_itab-messa.
  ELSE.
    READ TABLE it_mess WITH KEY msgtyp = 'S'.
    IF sy-subrc EQ 0.
      MOVE : '0' TO w_subrc.
      READ TABLE it_mess WITH KEY msgtyp = 'S'
                                  msgid  = 'M3'
                                  msgnr  = '810'.
      IF sy-subrc NE 0.
        PERFORM get_message CHANGING w_messa.
        MOVE : w_messa TO it_itab-messa.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " change_by_bdc

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3376   text
*      -->P_3377   text
*      -->P_3378   text
*----------------------------------------------------------------------*
FORM dynpro USING    dynbegin
                     name
                     value.
*---
  IF dynbegin = 'X'.
    CLEAR : it_bdc.
    MOVE : name  TO it_bdc-program,
           value TO it_bdc-dynpro,
           'X'   TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE .
    CLEAR : it_bdc.
    MOVE : name  TO it_bdc-fnam,
           value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro

*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
*---
  IF w_subrc EQ 0.
    MOVE : 'X' TO it_itab-conf5,
           space TO it_itab-w_selected.
*    MESSAGE s999 WITH it_itab-messa.
  ELSE.
    MOVE : space TO it_itab-conf5.
*    MESSAGE e999 WITH it_itab-messa.
  ENDIF.

*---
  CLEAR : ztmm_mara.

  MOVE-CORRESPONDING it_itab TO ztmm_mara.

  MODIFY : ztmm_mara, it_itab.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

*---
  IF w_subrc EQ 0.
    MESSAGE s999 WITH it_itab-messa.
  ELSE.
    MESSAGE e999 WITH it_itab-messa.
  ENDIF.
ENDFORM.                    " update_table

*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_W_MESSA  text
*----------------------------------------------------------------------*
FORM get_message CHANGING p_messa.
*---
  CLEAR : p_messa.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            msgid               = it_mess-msgid
            msgnr               = it_mess-msgnr
            msgv1               = it_mess-msgv1
            msgv2               = it_mess-msgv2
            msgv3               = it_mess-msgv3
            msgv4               = it_mess-msgv4
       IMPORTING
            message_text_output = p_messa.
ENDFORM.                    " get_message

*&---------------------------------------------------------------------*
*&      Form  change_by_bapi_roh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_by_bapi_roh.
*--- header
  MOVE : it_itab-matnr TO wa_headdata-material,
         it_itab-mtart TO wa_headdata-matl_type,
         'X'           TO wa_headdata-basic_view,
         'X'           TO wa_headdata-purchase_view,
         'X'           TO wa_headdata-mrp_view.
  IF it_itab-mtart EQ c_mtart_roh.
    if not it_itab-lgort is initial.
      MOVE : 'X'           TO wa_headdata-warehouse_view,
             'X'           TO wa_headdata-storage_view.
    endif.
  ENDIF.
  MOVE : 'X'           TO wa_headdata-quality_view,
         'X'           TO wa_headdata-account_view,
         'X'           TO wa_headdata-cost_view.
*--- client data
  MOVE : it_itab-matkl TO wa_clientdata-matl_group,
         it_itab-bismt TO wa_clientdata-old_mat_no,
         it_itab-ferth to wa_clientdata-prod_memo,
         it_itab-mtpos_mara TO wa_clientdata-item_cat,
         it_itab-ntgew TO wa_clientdata-net_weight,
         it_itab-gewei TO wa_clientdata-unit_of_wt,
         it_itab-gewei TO wa_clientdata-unit_of_wt_iso,
         it_itab-profl TO wa_clientdata-hazmatprof,
         it_itab-tempb TO wa_clientdata-temp_conds,
         it_itab-raube TO wa_clientdata-stor_conds.
  MOVE : 'X'           TO wa_clientdatax-matl_group,
         'X'           TO wa_clientdatax-old_mat_no,
         'X'           to wa_clientdatax-prod_memo,
         'X'           TO wa_clientdatax-item_cat,
         'X'           TO wa_clientdatax-net_weight,
         'X'           TO wa_clientdatax-unit_of_wt,
         'X'           TO wa_clientdatax-unit_of_wt_iso,
         'X'           TO wa_clientdatax-hazmatprof,
         'X'           TO wa_clientdatax-temp_conds,
         'X'           TO wa_clientdatax-stor_conds.
*--- plant data
  MOVE : it_itab-werks TO wa_plantdata-plant,
         it_itab-ekgrp TO wa_plantdata-pur_group,
         it_itab-usequ TO wa_plantdata-quotausage,
         it_itab-fabkz TO wa_plantdata-jit_relvt,
         it_itab-stawn TO wa_plantdata-comm_code,
         it_itab-herkl TO wa_plantdata-countryori,
         it_itab-dispo TO wa_plantdata-mrp_ctrler,
         it_itab-disls TO wa_plantdata-lotsizekey,
         it_itab-bstmi TO wa_plantdata-minlotsize,
         it_itab-bstma TO wa_plantdata-maxlotsize,
         it_itab-bstrf TO wa_plantdata-round_val,
         it_itab-ausdt to wa_plantdata-eff_o_day,
         it_itab-sobsl TO wa_plantdata-spproctype,
         it_itab-lgpro TO wa_plantdata-iss_st_loc,
         it_itab-lgfsb TO wa_plantdata-sloc_exprc,
         it_itab-vspvb TO wa_plantdata-supply_area,
         it_itab-plifz TO wa_plantdata-plnd_delry,
         it_itab-mrppp TO wa_plantdata-ppc_pl_cal,
         it_itab-eisbe TO wa_plantdata-safety_stk,
         it_itab-abcin TO wa_plantdata-cc_ph_inv,
         it_itab-xmcng TO wa_plantdata-neg_stocks,
         it_itab-shflg TO wa_plantdata-safty_t_id,
         it_itab-shzet TO wa_plantdata-safetytime.
  MOVE : it_itab-werks TO wa_plantdatax-plant,
         'X'           TO wa_plantdatax-pur_group,
         'X'           TO wa_plantdatax-quotausage,
         'X'           TO wa_plantdatax-jit_relvt,
         'X'           TO wa_plantdatax-comm_code,
         'X'           TO wa_plantdatax-countryori,
         'X'           TO wa_plantdatax-mrp_ctrler,
         'X'           TO wa_plantdatax-lotsizekey,
         'X'           TO wa_plantdatax-minlotsize,
         'X'           TO wa_plantdatax-maxlotsize,
         'X'           TO wa_plantdatax-round_val,
         'X'           to wa_plantdatax-eff_o_day,
         'X'           TO wa_plantdatax-spproctype,
         'X'           TO wa_plantdatax-iss_st_loc,
         'X'           TO wa_plantdatax-sloc_exprc,
         'X'           TO wa_plantdatax-supply_area,
         'X'           TO wa_plantdatax-plnd_delry,
         'X'           TO wa_plantdatax-ppc_pl_cal,
         'X'           TO wa_plantdatax-safety_stk,
         'X'           TO wa_plantdatax-cc_ph_inv,
         'X'           TO wa_plantdataX-safty_t_id,
         'X'           TO wa_plantdatax-safetytime,
         'X'           TO wa_plantdatax-neg_stocks.
*--- check existence => fill mandatory field
  CLEAR : marc.
  SELECT SINGLE matnr INTO marc-matnr
                      FROM marc
                     WHERE matnr EQ it_itab-matnr
                       AND werks EQ it_itab-werks.
  IF sy-subrc NE 0.
    CLEAR : marc.
    SELECT SINGLE mtvfp
                  dismm
                  fhori INTO (marc-mtvfp, marc-dismm, marc-fhori)
                        FROM marc
                       WHERE matnr EQ it_itab-matnr.
    MOVE : marc-mtvfp TO wa_plantdata-availcheck,     " availability
           marc-dismm TO wa_plantdata-mrp_type,    " mrp type
           marc-fhori TO wa_plantdata-sm_key,    " scheduling margin key
           'X'        TO wa_plantdatax-availcheck,
           'X'        TO wa_plantdatax-mrp_type,
           'X'        TO wa_plantdatax-sm_key.
    MOVE : c_mmsta_11    TO wa_plantdata-pur_status,
                                                   "plant-sp.matl status
*           it_itab-mstde TO wa_plantdata-pvalidfrom,
           'X'           TO wa_plantdata-sourcelist,
           c_beskz_f     TO wa_plantdata-proc_type,
           c_sbdkz_2     TO wa_plantdata-dep_req_id,
           'X'           TO wa_plantdatax-pur_status,
*           'X'           TO wa_plantdatax-pvalidfrom,
           'X'           TO wa_plantdatax-sourcelist,
           'X'           TO wa_plantdatax-proc_type,
           'X'           TO wa_plantdatax-dep_req_id.
  ENDIF.
*--- warehouse number
  IF it_itab-werks EQ c_werks_p001 AND it_itab-lgort EQ c_lgort_p400.
    MOVE : it_itab-lgnum TO wa_warehousenumberdata-whse_no,
           it_itab-ltkza TO wa_warehousenumberdata-withdrawal,
           it_itab-ltkze TO wa_warehousenumberdata-placement.
    MOVE : it_itab-lgnum TO wa_warehousenumberdatax-whse_no,
           'X'           TO wa_warehousenumberdatax-withdrawal,
           'X'           TO wa_warehousenumberdatax-placement.
*--- storage type
    MOVE : it_itab-lgnum TO wa_storagetypedata-whse_no,
           it_itab-lgtyp TO wa_storagetypedata-stge_type,
           it_itab-lgpla TO wa_storagetypedata-stge_bin,
           it_itab-rdmng TO wa_storagetypedata-round_qty.
    MOVE : it_itab-lgnum TO wa_storagetypedatax-whse_no,
           it_itab-lgtyp TO wa_storagetypedatax-stge_type,
           'X'           TO wa_storagetypedatax-stge_bin,
           'X'           TO wa_storagetypedatax-round_qty.
  endif.
*--- valuation data
  MOVE : it_itab-werks TO wa_valuationdata-val_area,
         it_itab-bklas TO wa_valuationdata-val_class.
  MOVE : it_itab-werks TO wa_valuationdatax-val_area,
         'X'           TO wa_valuationdatax-val_class.
*--- storage location
  IF it_itab-mtart EQ c_mtart_roh.
    if not it_itab-lgort is initial.
      MOVE : it_itab-werks TO wa_storagelocationdata-plant,
             it_itab-lgort TO wa_storagelocationdata-stge_loc,
             it_itab-lgpbe to wa_storagelocationdata-stge_bin.
      MOVE : it_itab-werks TO wa_storagelocationdatax-plant,
             it_itab-lgort TO wa_storagelocationdatax-stge_loc,
             'X'           to wa_storagelocationdatax-stge_bin.
    endif.
  ENDIF.

*--- additional data
  IF NOT it_itab-meinh IS INITIAL AND NOT it_itab-umrez IS INITIAL.
    MOVE : it_itab-meinh   TO it_unitsofmeasure-alt_unit,
           it_itab-meinh   TO it_unitsofmeasure-alt_unit_iso,
           it_itab-umrez   TO it_unitsofmeasure-numerator,
           it_itab-umren   TO it_unitsofmeasure-denominatr,
           it_itab-laeng   to it_unitsofmeasure-length,
           it_itab-breit   to it_unitsofmeasure-width,
           it_itab-hoehe   to it_unitsofmeasure-height,
           it_itab-meabm   to it_unitsofmeasure-unit_dim,
           it_itab-voleh   to it_unitsofmeasure-volumeunit.
    MOVE : it_itab-meinh   TO it_unitsofmeasurex-alt_unit,
           it_itab-meinh   TO it_unitsofmeasurex-alt_unit_iso,
           'X'             TO it_unitsofmeasurex-numerator,
           'X'             TO it_unitsofmeasurex-denominatr,
           'X'             to it_unitsofmeasurex-length,
           'X'             to it_unitsofmeasurex-width,
           'X'             to it_unitsofmeasurex-height,
           'X'             to it_unitsofmeasurex-unit_dim,
           'X'             to it_unitsofmeasurex-volumeunit.
    APPEND : it_unitsofmeasure, it_unitsofmeasurex.
  ENDIF.
ENDFORM.                    " change_by_bapi_roh

*&---------------------------------------------------------------------*
*&      Form  change_by_bapi_halb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_by_bapi_halb.
*--- header
  MOVE : it_itab-matnr TO wa_headdata-material,
         it_itab-mtart TO wa_headdata-matl_type,
         'X'           TO wa_headdata-basic_view.
*--- client data
  MOVE : it_itab-ntgew TO wa_clientdata-net_weight,
         it_itab-gewei TO wa_clientdata-unit_of_wt,
         it_itab-gewei TO wa_clientdata-unit_of_wt_iso,
         it_itab-profl TO wa_clientdata-hazmatprof.
  MOVE : 'X'           TO wa_clientdatax-net_weight,
         'X'           TO wa_clientdatax-unit_of_wt,
         'X'           TO wa_clientdatax-unit_of_wt_iso,
         'X'           TO wa_clientdatax-hazmatprof.
ENDFORM.                    " change_by_bapi_halb

*&---------------------------------------------------------------------*
*&      Form  delete_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_item.
*---
  DATA : it_dele LIKE ztmm_mara OCCURS 0 WITH HEADER LINE.

  DATA : l_answer(1).

*---
  READ TABLE it_itab WITH KEY w_selected = 'X'.

  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-m30.
  ENDIF.

*---
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            defaultoption  = 'Y'
            textline1      = text-012
            titel          = text-013
            start_column   = 25
            start_row      = 6
            cancel_display = 'X'
       IMPORTING
            answer         = l_answer.

  CHECK l_answer EQ 'J'.

*---
  CLEAR : it_dele, it_dele[].

  LOOP AT it_itab WHERE w_selected NE space
                    AND conf5 EQ space.
    MOVE-CORRESPONDING it_itab TO it_dele.
    MOVE : 'X'                 TO it_dele-loekz,
           sy-datum            TO it_dele-aedat,
           sy-uzeit            TO it_dele-aezet,
           sy-uname            TO it_dele-aenam.
    APPEND it_dele.
    DELETE it_itab.
    CLEAR : it_dele.
  ENDLOOP.

  MODIFY ztmm_mara FROM TABLE it_dele.
ENDFORM.                    " delete_item
*&---------------------------------------------------------------------*
*&      Form  get_from_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_from_material_master.
  DATA: IT_MARA LIKE ZTMM_MARA OCCURS 0 WITH HEADER LINE.
  DATA: IT_QMAT LIKE QMAT OCCURS 0 WITH HEADER LINE.
  DATA: W_INDEX TYPE N.

  READ TABLE it_itab WITH KEY w_selected = 'X'.

  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-m30.
  ENDIF.

  SELECT a~matnr mtart b~werks profl
         c~lgort d~lgtyp a~matkl a~bismt a~extwg a~mtpos_mara
         a~ntgew a~gewei ekgrp usequ fabkz stawn shflg shzet
*         umren
         b~herkl dispo disls bstmi bstma bstrf sobsl vspvb rgekz
         plifz mrppp eisbe tempb raube abcin
         lgpla d~rdmng ferth ausdt lgpbe a~laeng a~breit a~hoehe a~meabm
         INTO CORRESPONDING FIELDS OF TABLE IT_MARA
             from mara as a join marc as b
             on a~matnr = b~matnr
             left outer join mard as c
             on a~matnr  = c~matnr
             left outer join mlgt as d
             on a~matnr = d~matnr
*             join marm as e
*             on a~matnr = e~matnr
*              and a~MEINS = e~meinh
             for all entries in it_itab
              WHERE mtart = it_itab-mtart
              AND a~matnr = it_itab-matnr
              AND b~werks = it_itab-werks.
*              AND c~lgort = it_itab-lgort
*              AND d~lgtyp = it_itab-lgtyp
*              AND a~profl = it_itab-profl
*              AND a~matkl = it_itab-matkl
*              AND b~ekgrp = it_itab-ekgrp
*             AND b~stawn = it_itab-stawn
*             AND b~dispo = it_itab-dispo
*             AND b~disls = it_itab-disls
*             AND b~vspvb = it_itab-vspvb.

  SELECT * INTO TABLE IT_QMAT FROM QMAT
                FOR ALL ENTRIES IN IT_ITAB
                WHERE MATNR = IT_ITAB-MATNR
                  AND WERKS = IT_ITAB-WERKS.

  sort it_mara by matnr werks lgort.
  sort it_qmat by matnr werks art.

  LOOP AT it_itab WHERE w_selected NE space
                    AND conf5 EQ space.
   read table it_mara with key matnr = it_itab-matnr
                               werks = it_itab-werks.
   if it_itab-lgort is initial.
      read table it_mara with key matnr = it_itab-matnr
                               werks = it_itab-werks.
      it_itab-lgort = it_mara-lgort.
   else.
      read table it_mara with key matnr = it_itab-matnr
                               werks = it_itab-werks
                               lgort = it_itab-lgort.
   endif.
   if ( it_itab-matkl is initial or it_itab-matkl = 'INIT' )
      AND it_mara-matkl ne ' '.
      it_itab-matkl = it_mara-matkl.
   endif.
   if it_itab-profl is initial.
      it_itab-profl = it_mara-profl.
   endif.

   if it_itab-lgtyp is initial.
      it_itab-lgtyp = it_mara-lgtyp.
   endif.
   if it_itab-bismt is initial.
      it_itab-bismt = it_mara-bismt.
   endif.
   if it_itab-extwg is initial.
      it_itab-extwg = it_mara-extwg.
   endif.
   if it_itab-mtpos_mara is initial.
      it_itab-mtpos_mara = it_mara-mtpos_mara.
   endif.
   if it_itab-ntgew is initial.
      it_itab-ntgew = it_mara-ntgew.
   endif.
   if it_itab-gewei is initial.
      it_itab-gewei = it_mara-gewei.
   endif.
*   if it_itab-umren is initial.
*      it_itab-umren = it_mara-umren.
*   endif.
   if it_itab-ekgrp is initial.
      it_itab-ekgrp = it_mara-ekgrp.
   endif.
   if it_itab-usequ is initial.
      it_itab-usequ = it_mara-usequ.
   endif.
   if it_itab-fabkz is initial.
      it_itab-fabkz = it_mara-fabkz.
   endif.
   if it_itab-stawn is initial.
      it_itab-stawn = it_mara-stawn.
   endif.
   if it_itab-herkl is initial.
      it_itab-herkl = it_mara-herkl.
   endif.
   if it_itab-dispo is initial.
      it_itab-dispo = it_mara-dispo.
   endif.
   if it_itab-disls is initial.
      it_itab-disls = it_mara-disls.
   endif.
   if it_itab-bstmi is initial.
      it_itab-bstmi = it_mara-bstmi.
   endif.
   if it_itab-bstma is initial.
      it_itab-bstma = it_mara-bstma.
   endif.
   if it_itab-bstrf is initial.
      it_itab-bstrf = it_mara-bstrf.
   endif.
   if it_itab-sobsl is initial.
      it_itab-sobsl = it_mara-sobsl.
   endif.
   if it_itab-vspvb is initial.
      it_itab-vspvb = it_mara-vspvb.
   endif.
   if it_itab-rgekz is initial.
      it_itab-rgekz = it_mara-rgekz.
   endif.
   if it_itab-plifz is initial.
      it_itab-plifz = it_mara-plifz.
   endif.
   if it_itab-mrppp is initial.
      it_itab-mrppp = it_mara-mrppp.
   endif.
   if it_itab-eisbe is initial.
      it_itab-eisbe = it_mara-eisbe.
   endif.
   if it_itab-shflg is initial.
      it_itab-shflg = it_mara-shflg.
   endif.
   if it_itab-shzet is initial.
      it_itab-shzet = it_mara-shzet.
   endif.
   if it_itab-tempb is initial.
      it_itab-tempb = it_mara-tempb.
   endif.
   if it_itab-raube is initial.
      it_itab-raube = it_mara-raube.
   endif.
   if it_itab-abcin is initial.
      it_itab-abcin = it_mara-abcin.
   endif.
   if it_itab-lgpla is initial.
      it_itab-lgpla = it_mara-lgpla.
   endif.
   if it_itab-rdmng is initial.
      it_itab-rdmng = it_mara-rdmng.
   endif.
   if it_itab-ferth is initial.
      it_itab-ferth = it_mara-ferth.
   endif.
   if it_itab-ausdt is initial.
      it_itab-ausdt = it_mara-ausdt.
   endif.
   if it_itab-lgpbe is initial.
      it_itab-lgpbe = it_mara-lgpbe.
   endif.
   if it_itab-laeng is initial.
      it_itab-laeng = it_mara-laeng.
   endif.
   if it_itab-breit is initial.
      it_itab-breit = it_mara-breit.
   endif.
   if it_itab-hoehe is initial.
      it_itab-hoehe = it_mara-hoehe.
   endif.
   if it_itab-meabm is initial.
      it_itab-meabm = it_mara-meabm.
   endif.
   if it_itab-art01 is initial.
      read table it_qmat with key matnr = it_itab-matnr
                                  werks = it_itab-werks.
      if sy-subrc eq 0.
         it_itab-art01 = it_qmat-art.
         it_itab-akti1 = it_qmat-aktiv.
         if it_itab-art02 is initial.
            w_index = sy-tabix + 1.
*            read table it_qmat with key matnr = it_itab-matnr
*                                  werks = it_itab-werks
*                                  art <> it_itab-art01.
            read table it_qmat index w_index.
            if sy-subrc eq 0.
               it_itab-art02 = it_qmat-art.
               it_itab-akti2 = it_qmat-aktiv.
            endif.
            clear w_index.
         endif.
      endif.
   endif.
   modify it_itab.
   clear it_itab.
 endloop.
ENDFORM.                    " get_from_material_master
*&---------------------------------------------------------------------*
*&      Module  check_get_master_data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE check_get_master_data INPUT.
*  if sy-dynnr = '9000' and w_okcode = 'GETM'.
*     perform get_from_material_master.
*  endif.
*ENDMODULE.                 " check_get_master_data  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_to_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
Form save_data_to_excel.
 data: w_col type i,
       w_col_char like GXXLT_V-col_no.
 DATA: BEGIN OF IT_DATA OCCURS 0,
      MATNR LIKE IT_ITAB-MATNR,
      WERKS LIKE IT_ITAB-WERKS,
      MTART LIKE IT_ITAB-MTART,
      PROFL LIKE IT_ITAB-PROFL,
      LGORT LIKE IT_ITAB-LGORT,
      MAKTX LIKE MAKT-MAKTX,
*      LGNUM LIKE IT_ITAB-LGNUM,
      LGTYP LIKE IT_ITAB-LGTYP,
      BISMT LIKE IT_ITAB-BISMT,
      EXTWG LIKE IT_ITAB-EXTWG,
      MTPOS_MARA LIKE IT_ITAB-MTPOS_MARA,
      ntgew LIKE IT_ITAB-ntgew,
      gewei LIKE IT_ITAB-gewei,
      ekgrp LIKE IT_ITAB-ekgrp,
      USEQU LIKE IT_ITAB-USEQU,
      FABKZ LIKE IT_ITAB-FABKZ,
      STAWN LIKE IT_ITAB-STAWN,
      HERKL LIKE IT_ITAB-HERKL,
      DISPO LIKE IT_ITAB-DISPO,
      DISLS LIKE IT_ITAB-DISLS,
      BSTMI LIKE IT_ITAB-BSTMI,
      BSTMA LIKE IT_ITAB-BSTMA,
      BSTRF LIKE IT_ITAB-BSTRF,
      SOBSL LIKE IT_ITAB-SOBSL,
      VSPVB LIKE IT_ITAB-VSPVB,
      RGEKZ LIKE IT_ITAB-RGEKZ,
      PLIFZ LIKE IT_ITAB-PLIFZ,
      MRPPP LIKE IT_ITAB-MRPPP,
      EISBE LIKE IT_ITAB-EISBE,
      SHFLG LIKE IT_ITAB-SHFLG,
      SHZET LIKE IT_ITAB-SHZET,
      TEMPB LIKE IT_ITAB-TEMPB,
      RAUBE LIKE IT_ITAB-RAUBE,
      ABCIN LIKE IT_ITAB-ABCIN,
      LGPLA LIKE IT_ITAB-LGPLA,
      RDMNG LIKE IT_ITAB-RDMNG,
      FERTH LIKE IT_ITAB-FERTH,
      AUSDT LIKE IT_ITAB-AUSDT,
      LGPBE LIKE IT_ITAB-LGPBE,
      LAENG LIKE IT_ITAB-LAENG,
      BREIT LIKE IT_ITAB-BREIT,
      HOEHE LIKE IT_ITAB-HOEHE,
      MEABM LIKE IT_ITAB-MEABM,
      VOLEH LIKE IT_ITAB-VOLEH,
      ART01 LIKE IT_ITAB-ART01,
      AKTI1 LIKE IT_ITAB-AKTI1,
      ART02 LIKE IT_ITAB-ART02,
      AKTI2 LIKE IT_ITAB-AKTI2,
      END OF IT_DATA.

  refresh col_text.
  clear col_text.
  READ TABLE it_itab WITH KEY w_selected = 'X'.
  check sy-subrc eq 0.
  loop at it_itab where w_selected = 'X'.
     MOVE-CORRESPONDING IT_ITAB TO IT_DATA.
*       select single maktx into makt-maktx
*                      from makt
*                     where matnr eq it_itab_matnr
*                       and spras eq sy-langu.
       PERFORM get_material_desc USING it_itab-matnr.
       MOVE makt-maktx to it_data-maktx.
       append it_data.
  endloop.
  w_col = 1.
  w_col_char = w_col.
*  condense w_col_char.
  PERFORM FILL_CELL using: w_col_char 'Material'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL using: w_col_char 'Plant'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Type'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Source'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Loca'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Descrition'.
*  w_col = w_col + 1.
*  w_col_char = w_col.
*  PERFORM FILL_CELL USING: w_col_char 'WMN'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'S TY'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Old Material No.'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Ext Material grp'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Itm Cat'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Net Weight'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'W Uni'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Pur Grp'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Quota'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'JIT'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Commodity/Import'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Org.Cty'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'MRP Controller'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Lot Size'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Min Lot Size'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Max Lot Size'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Rounding Value'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Spec Proc Ty'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Supp Area'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'BF'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Plan Del'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Plan Calenlar'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Safety Stock'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Safe T Ind'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Safe Time'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'BF Cycle'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Shop'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Phy Inv'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Storage Bin'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Rounding Qty'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Prdt/Inspec Memo'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Eff-Out date'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Storage Bin'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Length'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Width'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Height'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Unit'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Vol Unit'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Insp Type'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Active'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Insp Type'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM FILL_CELL USING: w_col_char 'Active'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
*           PERCENTAGE = 0
           TEXT       = 'Downloading to Excel, Please Wait....'
       EXCEPTIONS
            OTHERS     = 1.

  CALL FUNCTION 'XXL_CHECK_API'
       EXPORTING
            QUESTION = 'STARTABLE'
*    IMPORTING
*         RETURN_CODE  =
     EXCEPTIONS
          INV_QUESTION = 1
          OTHERS       = 2.

  IF SY-SUBRC <> 0.
     MESSAGE E000 WITH 'API Error'.
  ELSE.
    CALL FUNCTION 'XXL_SIMPLE_API'
      EXPORTING
*   FILENAME                = 'ULO     '
        HEADER                  = 'Downloding to Excel'
        N_KEY_COLS              = 1
      TABLES
        COL_TEXT                = col_text
        DATA                    = it_data
        ONLINE_TEXT             = ONLINE_TEXT
        PRINT_TEXT              = PRINT_TEXT
      EXCEPTIONS
        DIM_MISMATCH_DATA       = 1
        FILE_OPEN_ERROR         = 2
        FILE_WRITE_ERROR        = 3
        INV_WINSYS              = 4
        INV_XXL                 = 5
        OTHERS                  = 6.
   ENDIF.
Endform.

form fill_cell using p_col_no like GXXLT_V-col_no
                    p_col_name like GXXLT_V-col_name.
*  p_col_no = p_col_no + 1.
  COL_TEXT-COL_NO = p_col_no.
  COL_TEXT-COL_NAME = p_col_name.
  append col_text.
endform.
