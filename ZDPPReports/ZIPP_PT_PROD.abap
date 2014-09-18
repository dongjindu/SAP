************************************************************************
* Program Name      : ZIPP_PT_PROD
* Author            : Furong Wang
* Creation Date     : 07/01/2010
* Specifications By : Daniel Kim
* Development Request No :
* Addl Documentation:
* Description       : PT: Send Engine Code
* Modification Logs
* Date       Developer    RequestNo    Description
*
*********************************************************************

REPORT zipp_pt_prod NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.
TABLES:  ztpperm,pgmi.

DATA : l_msgtxt(100),
       l_result(1).

CONSTANTS: c_dest(10) VALUE 'WMHR01'.   "Interface Destination.

DATA: BEGIN OF it_pt_prod OCCURS 0.
        INCLUDE STRUCTURE ztpp_pt_prod.
DATA: END OF it_pt_prod.
DATA : it_pt_prod_log LIKE ztpp_pt_prod OCCURS 0 WITH HEADER LINE.

DATA:  BEGIN OF it_matnr OCCURS 0,
        matnr LIKE mara-matnr,
        END OF it_matnr.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_zedat FOR ztpperm-zedat OBLIGATORY,
                s_date  FOR sy-datum OBLIGATORY,
                s_prgrp for pgmi-prgrp no INTERVALS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM init_data.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM send_data.


END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_data.
  DATA: BEGIN OF lt_ztpperm OCCURS 0,
        eassyid LIKE ztpperm-eassyid,
        eitem LIKE ztpperm-eitem,
        rdate LIKE ztpperm-rdate,
        rtime LIKE ztpperm-rtime,
        zedat LIKE ztpperm-zedat,
        zetim LIKE ztpperm-zetim,
     END OF lt_ztpperm.

  DATA : BEGIN OF lt_ztpperm_tmp OCCURS 0,
          mip_no LIKE  ztpp_pt_prod-mip_no,
         END OF lt_ztpperm_tmp.

  DATA : BEGIN OF it_ausp_engine OCCURS 0,
            objek LIKE ausp-objek,
            atwrt LIKE ausp-atwrt,
         END OF it_ausp_engine.
  DATA : BEGIN OF it_ausp_tmp  OCCURS 0,
            objek LIKE ausp-objek,
         END OF it_ausp_tmp.

*  DATA : l_zedat       LIKE ztpperm-zedat.
  DATA : lv_atinn      LIKE ausp-atinn.
  DATA: l_date_c(8),
        l_time_c(6).

  CLEAR : it_ausp_tmp[], it_ausp_tmp, lt_ztpperm_tmp[],
          lt_ztpperm_tmp.

  SELECT eassyid eitem rdate rtime  zedat   zetim
     INTO TABLE lt_ztpperm
     FROM ztpperm
     WHERE erpid >= 'E05'
       AND zedat IN s_zedat
       AND rdate IN s_date.

  SELECT eassyid eitem rdate rtime    zedat   zetim
    APPENDING TABLE lt_ztpperm
    FROM ztpperm_bk
   WHERE erpid >= 'E05'
     AND zedat IN s_zedat
     AND rdate IN s_date.

  SORT lt_ztpperm BY  eassyid  zedat DESCENDING
                               zetim DESCENDING
                               eitem.
  DELETE ADJACENT DUPLICATES FROM lt_ztpperm COMPARING eassyid.

  LOOP AT lt_ztpperm.
    lt_ztpperm_tmp-mip_no =  lt_ztpperm-eassyid.
    APPEND lt_ztpperm_tmp.
  ENDLOOP.

** Added for product group
  REFRESH it_matnr.

  loop at s_prgrp.
  PERFORM get_engine_material USING s_prgrp-low
          'E001'.

  PERFORM get_engine_material USING  s_prgrp-low
          'E002'.
  endloop.
  SORT it_matnr BY matnr.

  l_date_c = sy-datum.
  l_time_c = sy-uzeit.

  SELECT *
    INTO  TABLE it_pt_prod_log
  FROM ztpp_pt_prod
    FOR ALL ENTRIES IN lt_ztpperm_tmp
  WHERE mip_no  =  lt_ztpperm_tmp-mip_no.

  SORT it_pt_prod_log BY mip_no zedat DESCENDING
                                zetim DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_pt_prod_log COMPARING mip_no.

*-<get Engine Code
  LOOP AT lt_ztpperm.
    it_ausp_tmp-objek = lt_ztpperm-eassyid.
    APPEND it_ausp_tmp.
  ENDLOOP.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'EN_ITEM_CODE'
    IMPORTING
      output = lv_atinn.

  IF it_ausp_tmp[] IS NOT INITIAL.
    SELECT objek atwrt INTO CORRESPONDING FIELDS OF TABLE it_ausp_engine
    FROM ausp
      FOR ALL ENTRIES IN  it_ausp_tmp
    WHERE objek =   it_ausp_tmp-objek
      AND atinn =   lv_atinn
      AND klart =   '002'.
    SORT it_ausp_engine BY objek.
  ENDIF.
*->

  LOOP AT lt_ztpperm.

*-  read current Engine code
    READ TABLE it_ausp_engine WITH KEY objek  = lt_ztpperm-eassyid
                                                BINARY SEARCH.
    IF sy-subrc = 0.
      it_pt_prod-mip_cd  = it_ausp_engine-atwrt.
      it_pt_prod-mtrl_no = it_ausp_engine-atwrt.
    ELSE.
      it_pt_prod-mip_cd  = lt_ztpperm-eitem.
      it_pt_prod-mtrl_no = lt_ztpperm-eitem.
    ENDIF.

* Added on 09/25/13
    READ TABLE it_matnr WITH KEY matnr = it_pt_prod-mip_cd
                        BINARY SEARCH.
    IF sy-subrc <> 0.
      CLEAR: it_pt_prod.
      CONTINUE.
    ENDIF.
** End on 09/25/13

    it_pt_prod-company_code = 'H201'.
    it_pt_prod-prdn_plnt_cd = 'HEA1'.
    it_pt_prod-mip_no = lt_ztpperm-eassyid.

*-< check logic : delete data if prd.date & engine code are same
    READ TABLE  it_pt_prod_log WITH KEY mip_no = lt_ztpperm-eassyid
                                                  BINARY SEARCH.
    IF sy-subrc = 0.
      IF it_pt_prod-mip_cd  = it_pt_prod_log-mip_cd
            AND lt_ztpperm-rdate  = it_pt_prod_log-prdn_date.
        CLEAR: it_pt_prod.
        CONTINUE.
      ENDIF.
    ENDIF.
*->

    it_pt_prod-mip_st_cd = 'Y'.

    SELECT SINGLE maktx INTO it_pt_prod-mip_nm
     FROM makt
     WHERE matnr = lt_ztpperm-eitem
       AND spras = 'E'.

    it_pt_prod-mip_scn_cd = 'EE'.
    PERFORM read_normal_class USING lt_ztpperm-eitem 'EN_SPC14'
                                CHANGING  it_pt_prod-mip_ln_cd.

    it_pt_prod-prdn_date = lt_ztpperm-rdate.
    it_pt_prod-prdn_time = lt_ztpperm-rtime.

    CONCATENATE l_date_c l_time_c INTO it_pt_prod-createdate.
    it_pt_prod-changedate = it_pt_prod-createdate.
    APPEND it_pt_prod.
    CLEAR: it_pt_prod.
  ENDLOOP.

ENDFORM.                    "GET_DATA

*&---------------------------------------------------------------------*
*&      Form  send_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_data.

  DATA: it_save LIKE TABLE OF ztpp_pt_prod WITH HEADER LINE,
        l_flag(1),
        l_zedat LIKE ztpp_pt_prod-zedat.

  l_zedat = sy-datum - 120.

  IF it_pt_prod[] IS INITIAL.
    MESSAGE i000 WITH 'No data'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'Z_FPP_PT_PROD'
    DESTINATION c_dest
    IMPORTING
      flag                  = l_result
    TABLES
      i_data                = it_pt_prod
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF sy-subrc = 0.
    IF l_result = 'S'.
      l_flag = 'S'.
      l_msgtxt = 'Data successfully sent out'.
      MESSAGE i001 WITH l_msgtxt.

    ELSE.
      l_flag = 'E'.
      l_msgtxt =  'Data unsuccessfully sent out'.
      MESSAGE i001 WITH l_msgtxt.
    ENDIF.
  ELSE.
    l_flag = 'E'.
    MESSAGE i001 WITH l_msgtxt.
  ENDIF.

  LOOP AT it_pt_prod.
    MOVE-CORRESPONDING it_pt_prod TO it_save.
    it_save-zresult = l_flag.
    it_save-zmsg = l_msgtxt.
    it_save-zsdat = sy-datum.
    it_save-zstim = sy-uzeit.
    it_save-zedat = sy-datum.
    it_save-zetim = sy-uzeit.
    APPEND it_save.
    CLEAR: it_save.
  ENDLOOP.

  DELETE FROM ztpp_pt_prod WHERE zedat < l_zedat.

  INSERT ztpp_pt_prod FROM TABLE it_save.
  IF sy-subrc = 0.
    COMMIT WORK.
    IF l_flag EQ 'E'.
      MESSAGE e001 WITH l_msgtxt.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    MESSAGE i000 WITH 'Table saving error'.
  ENDIF.
ENDFORM.                    "SEND_DATA
*&---------------------------------------------------------------------*
*&      Form  read_normal_class
*&---------------------------------------------------------------------*
FORM read_normal_class USING  p_vmno  p_char
                             CHANGING p_value.
  SELECT SINGLE au~atwrt
    INTO p_value
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_vmno      AND
          klart = '001'       AND   " material
          ca~atnam = p_char  .
ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK_OBJEK  text
*      -->P_0461   text
*      <--P_L_ATFLV_TEMP  text
*----------------------------------------------------------------------*
FORM read_normal_class_atflv USING  p_vmno  p_char
                             CHANGING p_value.
  SELECT SINGLE au~atflv
      INTO p_value
      FROM ausp AS au
        INNER JOIN cabn AS ca ON au~atinn = ca~atinn
      WHERE objek = p_vmno      AND
            klart = '002'       AND
            ca~atnam = p_char  .
ENDFORM.                    " READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.

  s_zedat-low   = sy-datum - 4.
  s_zedat-high  = sy-datum.
  s_zedat-sign = 'I'.
  s_zedat-option = 'BT'.
  APPEND s_zedat.

  s_date-low = sy-datum - 1.
  s_date-sign = 'I'.
  s_date-option = 'LE'.
  APPEND s_date.

ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  GET_ENGINE_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PRGRP  text
*      -->P_P_WERKS  text
*----------------------------------------------------------------------*
FORM get_engine_material  USING  p_prgrp
                          p_werks.
  DATA: lt_pgmi LIKE TABLE OF pgmi WITH HEADER LINE.
  DATA: l_matnr LIKE marc-matnr.

  SELECT * INTO TABLE lt_pgmi
  FROM pgmi
  WHERE prgrp = p_prgrp
    AND werks = p_werks.

  LOOP AT lt_pgmi.
    it_matnr-matnr = lt_pgmi-nrmit.
    SELECT SINGLE matnr INTO l_matnr
      FROM mara
      WHERE matnr = it_matnr-matnr
        AND mtart = 'PROD'.
    IF sy-subrc <> 0.
      COLLECT it_matnr.
    ELSE.
      PERFORM get_engine_material USING lt_pgmi-nrmit
                          lt_pgmi-werks.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " GET_ENGINE_MATERIAL
