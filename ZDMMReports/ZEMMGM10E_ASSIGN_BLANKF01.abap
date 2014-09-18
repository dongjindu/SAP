************************************************************************
* Program name : ZEMMGM10E_ASSIGN_BLANK
* Created by   : Min-su Park
* Created on   : 2003.11.11.
* Pattern      : Report 1-1
* Description  : Assign BLANK to STEEL
*
* Modification Log
* Date            Developer        Request No.    Description
* 2003.11.11.     Min-su Park      UD1K901873     Initial Coding
*
************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM10E_ASSIGN_BLANKF01                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA : it_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

*Get Marc Data (Blank Material)
  SELECT * FROM marc
           INTO CORRESPONDING FIELDS OF TABLE it_bom
*          WHERE beskz = 'E'  "Procurement Type ->Commented (20040109)
*            AND fevor = 'SPB'.                 ->Commented (20040109)
*           AND MATNR = 'B7611138103'.

*/ Begin of Added by Hakchin (20040109)
          WHERE dispo = 'MP1'  AND  "MRP Controller
                matnr LIKE 'B%'.    "This is Blank mat.

*/ End of Added by Hakchin (20040109)

  LOOP AT it_bom.
*  WHERE MATNR = 'B7611138103'.
*Check BOM Existence
    CALL FUNCTION 'BAPI_MAT_BOM_EXISTENCE_CHECK'
         EXPORTING
              material = it_bom-matnr
              plant    = it_bom-werks
              bomusage = '1'
         TABLES
              return   = it_return.
    READ TABLE it_return INDEX 1.

    IF sy-subrc = 0.   "BOM does not exist


*/Begin of Commented by Hakchin(20040223)
**Get Usage(gross weight)
*      PERFORM get_usage.
*/End of Commented by Hakchin(20040223)


*Get Characteristics and STEEL Material No.
*      PERFORM get_characteristics.
      PERFORM get_characteristics_hakchin.
    ELSE.     "BOM exist

*/ Begin of for Test
      DATA: lv_loekz LIKE stko-loekz.  "Bill of material
      SELECT SINGLE stko~loekz INTO lv_loekz
            "Deletion indicator for BOMs
           FROM mast
           INNER JOIN stko   "BOM Header
             ON stko~stlty = 'M'  AND  "Material BOM
                stko~stlnr = mast~stlnr  ""Bill of material
           WHERE matnr = it_bom-matnr AND
                 werks = it_bom-werks AND
                 stlan = '1'. "BOM usage
      IF lv_loekz = 'X'.



*Get Usage(gross weight)
        PERFORM get_usage.
*Get Characteristics and STEEL Material No.
        PERFORM get_characteristics_hakchin.
        MODIFY it_bom.
        CONTINUE.
      ENDIF.
*/ End of for Test
      DELETE it_bom.
      CONTINUE.
    ENDIF.
    MODIFY it_bom.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_USAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_usage.
  SELECT SINGLE brgew gewei
          INTO (it_bom-brgew, it_bom-gewei)
          FROM mara
         WHERE matnr = it_bom-matnr.
ENDFORM.                    " GET_USAGE
*&---------------------------------------------------------------------*
*&      Form  GET_CHARACTERISTICS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_characteristics.
  DATA : it_allocvalueschar
            LIKE bapi1003_alloc_values_char OCCURS 0 WITH HEADER LINE.
  DATA : wa_1 LIKE bapi1003_alloc_values_char,
         wa_2 LIKE bapi1003_alloc_values_char,
         wa_3 LIKE bapi1003_alloc_values_char,
         wa_4 LIKE bapi1003_alloc_values_char,
         wa_5 LIKE bapi1003_alloc_values_char,
         wa_6 LIKE bapi1003_alloc_values_char.
  DATA : w_colq(10), "Quality of Raw Material
         w_colpf(3), "Front Plate of Raw Material
         w_colpr(3), "Rear Plate of Raw Material
         w_colt TYPE p DECIMALS 2, "Thickness
         w_colw(05) ,        "Width
         w_coll(05) .        "Length

* Get Characteristic
  CALL FUNCTION 'Z_FMM_GET_CHARACT_CONV_CHAR1'
       EXPORTING
            i_matnr            = it_bom-matnr
*           CLASSNUM           = 'PRS_BLK_MASTER'
            classnum           = 'ZSTEEL'
       TABLES
            it_allocvalueschar = it_allocvalueschar.

*Quality of Raw Maerial
  READ TABLE it_allocvalueschar INTO wa_1
             WITH KEY charact = 'PRS_BLK_COLQ'.
  w_colq  = wa_1-value_char.
*Front Plate of Raw Material
  READ TABLE it_allocvalueschar INTO wa_2
             WITH KEY charact = 'PRS_BLK_COLPF'.
  w_colpf = wa_2-value_char.
*Rear Plate of Raw Material
  READ TABLE it_allocvalueschar INTO wa_3
             WITH KEY charact = 'PRS_BLK_COLPR'.
  w_colpr = wa_3-value_char.
*Thickness
  READ TABLE it_allocvalueschar INTO wa_4
             WITH KEY charact = 'PRS_BLK_COLT'.
  WRITE : wa_4-value_char
       TO it_allocvalueschar-value_char
       LEFT-JUSTIFIED.
  w_colt  = it_allocvalueschar-value_char.
  DATA : w_scolt(7).
  WRITE : w_colt TO w_scolt.
  CONDENSE w_scolt NO-GAPS.

*Width
  READ TABLE it_allocvalueschar INTO wa_5
             WITH KEY charact = 'PRS_BLK_COLW'.
  DATA : i_colw TYPE i.
  IF wa_5-value_char = space.
    i_colw = 0.
  ELSE.
    i_colw = wa_5-value_char.
  ENDIF.
  w_colw  = i_colw.
  CONDENSE w_colw NO-GAPS.
*Length
  READ TABLE it_allocvalueschar INTO wa_6
             WITH KEY charact = 'PRS_BLK_COLL'.
  DATA : i_coll TYPE i.
  IF wa_6-value_char = space.
    i_coll = 0.
  ELSE.
    i_coll = wa_6-value_char.
  ENDIF.
  w_coll  = i_coll.
  CONDENSE w_coll NO-GAPS.

  DATA : w_colfr(06).
  CONCATENATE w_colpf w_colpr INTO w_colfr.
  CONCATENATE w_colq    "Quality of Raw Material
              w_colfr   "Front Plate of Raw Material
*              W_COLPR   "Rear Plate of Raw Material
              w_scolt   "Thickness
              w_colw    "Width
              w_coll    "Length
         INTO it_bom-characteristic
         SEPARATED BY '_'.

* Get STEEL Material No.
  CLEAR it_ammat.
* Begin of Changed by Hakchin(20040108)
*  READ TABLE it_ammat WITH KEY maktx = it_bom-characteristic.
  READ TABLE it_ammat WITH KEY maktx2 = it_bom-characteristic.
* End of Changed by Hakchin(20040108)
  IF sy-subrc = 0.
    it_bom-steel = it_ammat-matnr.
  ENDIF.
ENDFORM.                    " GET_CHARACTERISTICS
*&---------------------------------------------------------------------*
*&      Form  GET_STEEL_MAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_steel_mat.

  SELECT mara~matnr makt~maktx
    INTO CORRESPONDING FIELDS OF TABLE it_ammat
    FROM mara
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = 'E'
    WHERE mara~matkl = 'AM'. "Material group: Steel Material


**/Begin of Added by Hakchin 20030108
*  FIELD-SYMBOLS: <fs_ammat> LIKE it_ammat.
*  DATA: lv_pos TYPE i.
*  DATA: lv_maktx2 LIKE <fs_ammat>-maktx2.
*
*  LOOP AT it_ammat ASSIGNING <fs_ammat>.
*    SEARCH <fs_ammat>-maktx FOR '_'.
*    CHECK sy-subrc = 0.
*    lv_pos = sy-fdpos + 1.
*    lv_maktx2 = <fs_ammat>-maktx+lv_pos(*).
*    <fs_ammat>-maktx2 = lv_maktx2.
*  ENDLOOP.
**/End of Added by Hakchin 20030108

*/Begin of Added by Hakchin(20040223)
  PERFORM combination_of_characteristic.
*/End of Added by Hakchin(20040223)

ENDFORM.                    " GET_STEEL_MAT
*&---------------------------------------------------------------------*
*&      Form  PAGE_CONTROL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM page_control.
  CALL FUNCTION 'SCROLLING_IN_TABLE'
       EXPORTING
            entry_act             = tc_bom-top_line
            entry_to              = tc_bom-lines
            last_page_full        = ' '
            loops                 = w_loopc
            ok_code               = save_ok_code
            overlapping           = 'X'
       IMPORTING
            entry_new             = tc_bom-top_line
       EXCEPTIONS
            no_entry_or_page_act  = 1
            no_entry_to           = 2
            no_ok_code_or_page_go = 3
            OTHERS                = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " PAGE_CONTROL
*&---------------------------------------------------------------------*
*&      Form  BDC_PASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0010   text
*      -->P_0011   text
*      -->P_0012   text
*----------------------------------------------------------------------*
FORM bdc_pass USING par1 par2 par3.
  CLEAR it_bdc.
  IF par1 = 'X'.
    it_bdc-dynbegin = 'X'.
    it_bdc-program  = par2.
    it_bdc-dynpro   = par3.
    APPEND it_bdc.
  ELSE.
    it_bdc-fnam = par2.
    it_bdc-fval = par3.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " BDC_PASS
*&---------------------------------------------------------------------*
*&      Form  BOM_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bom_create.
  DATA : datum(10),
         brgew(15).

  WRITE : sy-datum TO datum.

  LOOP AT it_bom .
    WRITE : it_bom-brgew TO brgew UNIT it_bom-gewei.
    REFRESH : it_bdc, it_message.
    CLEAR   : it_bdc, it_message.
    CHECK it_bom-steel <> space.
*'CS01' First Screen
    PERFORM bdc_pass USING:
                 'X' 'SAPLCSDI'    '0100'      ,
                 ' ' 'RC29N-MATNR' it_bom-matnr,
                 ' ' 'RC29N-WERKS' 'P001'      ,
                 ' ' 'RC29N-STLAN' '1'         ,
                 ' ' 'RC29N-DATUV' datum       ,
                 ' ' 'BDC_OKCODE'  '/00'       .
    PERFORM bdc_pass USING:
                 'X' 'SAPLCSDI'     '0110'     ,
                 ' ' 'BDC_OKCODE'  '/00'       .
    PERFORM bdc_pass USING:
                 'X' 'SAPLCSDI'     '0111'     ,
                 ' ' 'BDC_OKCODE'  '/00'       .
*'CS01' Item Screen
    PERFORM bdc_pass USING:
                 'X' 'SAPLCSDI'        '0140'      ,
                 ' ' 'RC29P-POSTP(01)' 'L'         ,
                 ' ' 'RC29P-IDNRK(01)' it_bom-steel,
                 ' ' 'RC29P-MENGE(01)' brgew       ,
                 ' ' 'RC29P-MEINS(01)' it_bom-gewei,
                 ' ' 'BDC_OKCODE'      '=FCBU'     .

    PERFORM bdc_pass USING:
                 'X' 'SAPLCSDI'        '0130'      ,
                 ' ' 'BDC_OKCODE'      '/00'       .

    PERFORM bdc_pass USING:
                 'X' 'SAPLCSDI'        '0131'      ,
                 ' ' 'BDC_OKCODE'      '/00'       .

    PERFORM bdc_pass USING:
                 'X' 'SAPLCSDI'        '0138'      ,
                 ' ' 'BDC_OKCODE'      '/00'       .

    CALL TRANSACTION 'CS01'
            USING it_bdc
            MODE w_status
            UPDATE 'S'
            MESSAGES INTO it_message.
    IF sy-subrc <> 0.
      READ TABLE it_message WITH KEY msgtyp = 'E'.
      IF sy-subrc = 0.
        PERFORM error_message_display.
      ENDIF.
    ENDIF.
  ENDLOOP.
  MESSAGE s025.
ENDFORM.                    " BOM_CREATE
*&---------------------------------------------------------------------*
*&      Form  ERROR_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM error_message_display.
  DATA : txt LIKE t100-text.
  DATA : msgnr(3) TYPE n.

  msgnr = it_message-msgnr.
  CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
       EXPORTING
            langu = sy-langu
            msgid = it_message-msgid
            msgno = msgnr
            msgv1 = it_message-msgv1+0(50)
            msgv2 = it_message-msgv2+0(50)
            msgv3 = it_message-msgv3+0(50)
            msgv4 = it_message-msgv4+0(50)
       IMPORTING
            text  = txt.
  IF NOT txt IS INITIAL.
    MESSAGE e009 WITH txt.
  ENDIF.
ENDFORM.                    " ERROR_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  get_characteristics_hakchin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_characteristics_hakchin.
* For bapi_objcl_getclasses and bapi_objcl_getdetail.
  DATA: lt_alloclist   LIKE TABLE OF bapi1003_alloc_list,
        ls_alloclist   LIKE LINE OF lt_alloclist,
        lt_bapiret2    LIKE TABLE OF bapiret2.
  DATA:
    lt_allocvaluesnum  LIKE TABLE OF bapi1003_alloc_values_num,
    lt_allocvalueschar LIKE TABLE OF bapi1003_alloc_values_char,
    lt_allocvaluescurr LIKE TABLE OF bapi1003_alloc_values_curr,
    ls_allocvalueschar LIKE LINE OF lt_allocvalueschar,
    lv_objectkey       LIKE bapi1003_key-object,
    lv_objecttable     LIKE bapi1003_key-objecttable,
    lv_classnum        LIKE bapi1003_key-classnum,
    lv_classtype       LIKE bapi1003_key-classtype.
*
  lv_objectkey   = it_bom-matnr. "(Blank mat)
  lv_objecttable = 'MARA'.
  lv_classtype   = '001'.

  DATA : it_allocvalueschar
            LIKE bapi1003_alloc_values_char OCCURS 0 WITH HEADER LINE.



* Get Class number
  PERFORM bapi_objcl_getclasses
                  TABLES lt_alloclist
                         lt_bapiret2
                  USING  lv_objectkey    "it_bom-matnr(Blank mat)
                         lv_objecttable  "MARA
                         lv_classtype.   "001
  READ TABLE lt_alloclist INTO ls_alloclist
*                           WITH KEY classnum = 'ZSTEEL'.
                           WITH KEY classnum = 'PRS_BLK_MASTER'.
  CHECK sy-subrc = 0.
  lv_classnum    = ls_alloclist-classnum.
* Get Characteristic Value by Class
  PERFORM bapi_objcl_getdetail
               TABLES lt_allocvaluesnum
                      it_allocvalueschar  "Characteristic Data
                      lt_allocvaluescurr
                      lt_bapiret2
               USING  lv_objectkey   "Blank mat number
                      lv_objecttable "Table
                      lv_classnum    "Class number
                      lv_classtype.  "Class type

  DATA : wa_1 LIKE bapi1003_alloc_values_char,
         wa_2 LIKE bapi1003_alloc_values_char,
         wa_3 LIKE bapi1003_alloc_values_char,
         wa_4 LIKE bapi1003_alloc_values_num,
         wa_5 LIKE bapi1003_alloc_values_num,
         wa_6 LIKE bapi1003_alloc_values_num.
  DATA : w_colq(30),     "Quality of Raw Material
         w_colpf(3),     "Front Plate of Raw Material
         w_colpr(3),     "Rear Plate of Raw Material
*         w_colt TYPE p DECIMALS 2, "Thickness ->Commented by Hakchin
         w_colt(7),      "Thickness  "Changed by Hakchin
         w_colw(05),     "Width
         w_coll(05).     "Length

*Quality of Raw Maerial
  READ TABLE it_allocvalueschar INTO wa_1
             WITH KEY charact = 'PRS_BLK_COLQ'.
  w_colq  = wa_1-value_char.
*Front Plate of Raw Material
  READ TABLE it_allocvalueschar INTO wa_2
             WITH KEY charact = 'PRS_BLK_COLPF'.
  w_colpf = wa_2-value_char.
*Rear Plate of Raw Material
  READ TABLE it_allocvalueschar INTO wa_3
             WITH KEY charact = 'PRS_BLK_COLPR'.
  w_colpr = wa_3-value_char.
*Thickness
  READ TABLE lt_allocvaluesnum INTO wa_4
             WITH KEY charact = 'PRS_BLK_COLT'.

*/ Begin of Added Code by Hakchin (20040109)
  PERFORM fltp_char_conversion
           USING    2  "Number of decimal places
                    0  "Exponent
                    wa_4-value_from
                    space
                    space
           CHANGING w_colt.
*/ End of Added Code by Hakchin

  DATA : w_scolt(7).
  WRITE : w_colt TO w_scolt.
  CONDENSE w_scolt NO-GAPS.

*Width
  READ TABLE lt_allocvaluesnum INTO wa_5
             WITH KEY charact = 'PRS_BLK_COLW'.

*/ Begin of Added Code by Hakchin (20040109)
  PERFORM fltp_char_conversion
           USING    5  "Number of decimal places
                    0  "Exponent
                    wa_5-value_from
                    space
                    space
           CHANGING w_colw.
  IF w_colw = space.
    w_colw = 0.
  ENDIF.
*/ End of Added Code by Hakchin
  CONDENSE w_colw NO-GAPS.

*Length
  READ TABLE lt_allocvaluesnum INTO wa_6
             WITH KEY charact = 'PRS_BLK_COLL'.

*/ Begin of Added Code by Hakchin (20040109)
  PERFORM fltp_char_conversion
           USING    5  "Number of decimal places
                    0  "Exponent
                    wa_6-value_from
                    space
                    space
           CHANGING w_coll.
  IF w_coll = space.
    w_coll = 0.
  ENDIF.
*/ End of Added Code by Hakchin
  CONDENSE w_coll NO-GAPS.

  DATA : w_colfr(06).
  CONCATENATE w_colpf w_colpr INTO w_colfr.
  CONCATENATE w_colq    "Quality of Raw Material
              w_colfr   "Front Plate of Raw Material
*              W_COLPR   "Rear Plate of Raw Material
              w_scolt   "Thickness
              w_colw    "Width
              w_coll    "Length
         INTO it_bom-characteristic
         SEPARATED BY '_'.


* Get STEEL Material No.
  CLEAR it_ammat.
* Begin of Changed by Hakchin(20040108)
*  READ TABLE it_ammat WITH KEY maktx = it_bom-characteristic.

  READ TABLE it_ammat WITH KEY maktx2 = it_bom-characteristic.

* End of Changed by Hakchin(20040108)
  IF sy-subrc = 0.
    it_bom-steel = it_ammat-matnr.
  ENDIF.


*/ Begin of Added by Hakchin(20040223)
  DATA: ls_c_num LIKE bapi1003_alloc_values_num.
  DATA: prs_blk_cusg1(20).  "1st usage of coil per blank

* Get usage
  CLEAR: ls_c_num.
  READ TABLE lt_allocvaluesnum INTO ls_c_num
             WITH KEY charact = 'PRS_BLK_CUSG1'.
  PERFORM fltp_char_conversion
         USING    3  "Number of decimal places
                  0  "Exponent
                  ls_c_num-value_from
                  space
                  space
         CHANGING prs_blk_cusg1.
  it_bom-brgew = prs_blk_cusg1.  "Usage
  it_bom-gewei = 'KG'.           "Unit

  CLEAR: ls_c_num.
*/ End of Added by Hakchin(20040223)
ENDFORM.                    " get_characteristics_hakchin
*&---------------------------------------------------------------------*
*&      Form  bapi_objcl_getclasses
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ALLOCLIST  text
*      -->P_LT_BAPIRET2  text
*      -->P_LV_OBJECTKEY  text
*      -->P_LV_OBJECTTABLE  text
*      -->P_LV_CLASSTYPE  text
*----------------------------------------------------------------------*
FORM bapi_objcl_getclasses
                TABLES ext_alloclist
                         STRUCTURE bapi1003_alloc_list
                       ext_bapiret2
                         STRUCTURE bapiret2
                USING value(im_objectkey_imp)   LIKE bapi1003_key-object
                      value(im_objecttable_imp) LIKE
bapi1003_key-objecttable
                      value(im_classtype_imp)   LIKE
bapi1003_key-classtype.
  CLEAR: ext_alloclist, ext_bapiret2, ext_alloclist[], ext_bapiret2[].
  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      objectkey_imp         = im_objectkey_imp
      objecttable_imp       = im_objecttable_imp
      classtype_imp         = im_classtype_imp
*   READ_VALUATIONS       =
*   KEYDATE               = SY-DATUM
*   LANGUAGE              = SY-LANGU
    TABLES
      alloclist             = ext_alloclist
*   ALLOCVALUESCHAR       =
*   ALLOCVALUESCURR       =
*   ALLOCVALUESNUM        =
      return                = ext_bapiret2.
ENDFORM.                    "bapi_objcl_getclasses
*&---------------------------------------------------------------------*
*&      Form  bapi_objcl_getdetail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ALLOCVALUESNUM  text
*      -->P_LT_ALLOCVALUESCHAR  text
*      -->P_LT_ALLOCVALUESCURR  text
*      -->P_LT_BAPIRET2  text
*      -->P_LV_OBJECTKEY  text
*      -->P_LV_OBJECTTABLE  text
*      -->P_LV_CLASSNUM  text
*      -->P_LV_CLASSTYPE  text
*----------------------------------------------------------------------*
FORM bapi_objcl_getdetail
             TABLES ext_allocvaluesnum
                      STRUCTURE bapi1003_alloc_values_num
                    ext_allocvalueschar
                      STRUCTURE bapi1003_alloc_values_char
                    ext_allocvaluescurr
                      STRUCTURE bapi1003_alloc_values_curr
                    ext_bapiret2
                      STRUCTURE bapiret2
             USING  value(im_objectkey)   LIKE bapi1003_key-object
                    value(im_objecttable) LIKE bapi1003_key-objecttable
                    value(im_classnum)    LIKE bapi1003_key-classnum
                    value(im_classtype)   LIKE bapi1003_key-classtype.
  CLEAR: ext_allocvaluesnum,    ext_allocvalueschar,
         ext_allocvaluescurr,   ext_bapiret2,
         ext_allocvaluesnum[],  ext_allocvalueschar[],
         ext_allocvaluescurr[], ext_bapiret2[].
  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey              = im_objectkey
      objecttable            = im_objecttable
      classnum               = im_classnum
      classtype              = im_classtype
*   KEYDATE                = SY-DATUM
*   UNVALUATED_CHARS       = ' '
*   LANGUAGE               = SY-LANGU
* IMPORTING
*   STATUS                 =
*   STANDARDCLASS          =
    TABLES
      allocvaluesnum         = ext_allocvaluesnum
      allocvalueschar        = ext_allocvalueschar
      allocvaluescurr        = ext_allocvaluescurr
      return                 = ext_bapiret2.
ENDFORM.                    "bapi_objcl_getdetail
*&---------------------------------------------------------------------*
*&      Form  fltp_char_conversion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0      text
*      -->P_WA_4_VALUE_FROM  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      <--P_W_COLT  text
*----------------------------------------------------------------------*
FORM fltp_char_conversion
         USING    value(im_decim)
                  value(im_expon)
                  value(im_input) TYPE f
                  value(im_ivalu)
                  value(im_maskn)
         CHANGING value(ex_flstr).
  CALL FUNCTION 'FLTP_CHAR_CONVERSION'
       EXPORTING
            decim = im_decim
            expon = im_expon
            input = im_input
            ivalu = im_ivalu
            maskn = im_maskn
       IMPORTING
            flstr = ex_flstr.
ENDFORM.                    "fltp_char_conversion
*&---------------------------------------------------------------------*
*&      Form  dynp_values_read
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DYNPREAD  text
*      -->P_0154   text
*      <--P_F4RC  text
*----------------------------------------------------------------------*
FORM dynp_values_read TABLES   p_dynpfields
                                 STRUCTURE dynpread
                      USING    value(p_field)
                      CHANGING value(p_f4rc) LIKE sy-subrc.

  CLEAR: p_dynpfields. REFRESH: p_dynpfields.
  p_dynpfields-fieldname = p_field.
  APPEND p_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname                         = sy-cprog
      dynumb                         = sy-dynnr
*   TRANSLATE_TO_UPPER             = ' '
*   REQUEST                        = ' '
*   PERFORM_CONVERSION_EXITS       = ' '
*   PERFORM_INPUT_CONVERSION       = ' '
    determine_loop_index           = 'X'
    TABLES
      dynpfields                     = p_dynpfields
   EXCEPTIONS
     invalid_abapworkarea           = 1
     invalid_dynprofield            = 2
     invalid_dynproname             = 3
     invalid_dynpronummer           = 4
     invalid_request                = 5
     no_fielddescription            = 6
     invalid_parameter              = 7
     undefind_error                 = 8
     double_conversion              = 9
     stepl_not_found                = 10
     OTHERS                         = 11
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  p_f4rc = sy-subrc.

ENDFORM.                    " dynp_values_read
*&---------------------------------------------------------------------*
*&      Form  bdc_processing_bom_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_processing_bom_create
               TABLES   ext_bdcmsgcoll
                           STRUCTURE bdcmsgcoll
               USING    value(im_zdocno)
               CHANGING value(ex_subrc).
  CLEAR: ext_bdcmsgcoll, ext_bdcmsgcoll[].
  CLEAR: ex_subrc.

  DATA: lv_matnr_001 LIKE bdcdata-fval.
  DATA: lv_aennr_004 LIKE bdcdata-fval.
  DATA: lv_idnrk_01_008 LIKE bdcdata-fval.
  DATA: lv_meins_01_010 LIKE bdcdata-fval.
  DATA: lv_menge_01_009 LIKE bdcdata-fval.

  WRITE : it_bom-brgew TO lv_menge_01_009 UNIT it_bom-gewei.
  CHECK it_bom-steel <> space.

  lv_matnr_001 = it_bom-matnr.
  lv_aennr_004 = w_aennr.
  lv_idnrk_01_008 = it_bom-steel.
  lv_meins_01_010 = it_bom-gewei.

  CONDENSE:
    lv_matnr_001,
    lv_aennr_004,
    lv_idnrk_01_008,
    lv_meins_01_010,
    lv_menge_01_009.

  CALL FUNCTION 'Z_FMM_60XX_CS01'
   EXPORTING
    ctu                = 'X'
    mode               = w_status   "'N'
    UPDATE             = 'L'
*   GROUP              =
*   USER               =
*   KEEP               =
*   HOLDDATE           =
*   NODATA             = '/'
    matnr_001          = lv_matnr_001  "it_bom-matnr     "'B1'
    werks_002          = 'P001'
    stlan_003          = '1'
    aennr_004          = lv_aennr_004   "'19971030-001'
*   DATUV_005          = '02/21/2004'
    bmeng_006          = '1'
    stlst_007          = '1'
    idnrk_01_008       = lv_idnrk_01_008 "'AM908200203240076'
    menge_01_009       = lv_menge_01_009   "brgew          "'6.770'
    meins_01_010       = lv_meins_01_010 "'KG'
    postp_01_011       = 'L'
    posnr_012          = '0010'
    idnrk_013          = lv_idnrk_01_008   "'AM908200203240076'
    menge_014          = lv_menge_01_009                    "'6.770'
    meins_015          = lv_meins_01_010   "'KG'
    sanka_016          = 'X'
   IMPORTING
    subrc              = ex_subrc
   TABLES
    messtab            = ext_bdcmsgcoll.
*   messtab            = it_message.


**** (Begin)BDC Log to the table ZTLOG
  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = im_zdocno
      im_ztcode            = sy-tcode
      im_zprogramm         = sy-cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = ext_bdcmsgcoll[]
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG

*/ Logging to ZTMM_ASSIGN_BLAN.
* Log No. Item.
  DATA: lv_logno_d TYPE num10.
  PERFORM number_get_next USING    nro_nr_01     "NRO Interval
                                   nro_object    "NRO Object
                          CHANGING lv_logno_d.   "Log. No. Item.
  COMMIT WORK.

  DATA: ls_ztmm_assign_blan LIKE ztmm_assign_blan.
  MOVE:
   im_zdocno     TO ls_ztmm_assign_blan-zdocno,
   lv_logno_d    TO ls_ztmm_assign_blan-logno_d,
   sy-tcode      TO ls_ztmm_assign_blan-ztcode,
   it_bom-matnr  TO ls_ztmm_assign_blan-matnr,
   it_bom-characteristic TO ls_ztmm_assign_blan-characteristic,
   it_bom-brgew  TO ls_ztmm_assign_blan-brgew,
   it_bom-gewei  TO ls_ztmm_assign_blan-gewei,
   it_bom-steel  TO ls_ztmm_assign_blan-steel,
   it_bom-werks  TO ls_ztmm_assign_blan-werks,
   w_aennr       TO ls_ztmm_assign_blan-aennr.

* Time stamp
  CALL FUNCTION 'Z_FMM_6001_01_TIME_STAMP'
       CHANGING
            ch_erdat = ls_ztmm_assign_blan-erdat
            ch_erzet = ls_ztmm_assign_blan-erzet
            ch_ernam = ls_ztmm_assign_blan-ernam
            ch_aedat = ls_ztmm_assign_blan-aedat
            ch_aezet = ls_ztmm_assign_blan-aezet
            ch_aenam = ls_ztmm_assign_blan-aenam.
* Result
  IF ex_subrc = 0.
    ls_ztmm_assign_blan-zzret = 'S'.
  ELSE.
    ls_ztmm_assign_blan-zzret = 'E'.
  ENDIF.
  INSERT INTO ztmm_assign_blan VALUES ls_ztmm_assign_blan.

ENDFORM.                    " bdc_processing_bom_create
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_09  text
*      -->P_NRO_OBJECT  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  check_changenumber
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_AENNR  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM check_changenumber USING    value(im_aennr)
                        CHANGING value(ex_subrc).
  CLEAR: ex_subrc.
  DATA: lv_aennr LIKE aenr-aennr.

  SELECT SINGLE aennr INTO lv_aennr
    FROM aenr
    WHERE aennr = im_aennr. "Change Numer
  ex_subrc = sy-subrc.

ENDFORM.                    " check_changenumber
*&---------------------------------------------------------------------*
*&      Form  combination_of_characteristic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM combination_of_characteristic.

  FIELD-SYMBOLS: <fs_ammat> LIKE it_ammat.
  DATA: lv_maktx2 LIKE <fs_ammat>-maktx2.

* For bapi_objcl_getclasses and bapi_objcl_getdetail.
  DATA: lt_alloclist   LIKE TABLE OF bapi1003_alloc_list,
        ls_alloclist   LIKE LINE OF lt_alloclist,
        lt_bapiret2    LIKE TABLE OF bapiret2.
  DATA:
    lt_allocvaluesnum  LIKE TABLE OF bapi1003_alloc_values_num,
    lt_allocvalueschar LIKE TABLE OF bapi1003_alloc_values_char,
    lt_allocvaluescurr LIKE TABLE OF bapi1003_alloc_values_curr,
    ls_allocvalueschar LIKE LINE OF lt_allocvalueschar,
    lv_objectkey       LIKE bapi1003_key-object,
    lv_objecttable     LIKE bapi1003_key-objecttable,
    lv_classnum        LIKE bapi1003_key-classnum,
    lv_classtype       LIKE bapi1003_key-classtype.

*
  DATA : lt_characteristic
            LIKE bapi1003_alloc_values_char OCCURS 0 WITH HEADER LINE.

*
  DATA : ls_characteristic LIKE bapi1003_alloc_values_char.

  DATA :
    zsteel_matproperty(30),          "Quality of Raw Material
    zfront_finishing_thickness(30),  "Front Finishing Thickness
    zback_finishing_thickness(30),   "Back Finishing Thickness
    lv_frontback_thickness(30),      "Front Back Finishing thickness
    zspec_thick(30),                 "Thickness/Diameter
    zspec_width(30),                 "Width/Inner Diameter
    zspec_length(30).                "Length

*
  LOOP AT it_ammat ASSIGNING <fs_ammat>.
*
    lv_objectkey   = <fs_ammat>-matnr. "(Steel mat)
    lv_objecttable = 'MARA'.
    lv_classtype   = '001'.

* Get Class number
    PERFORM bapi_objcl_getclasses
                    TABLES lt_alloclist
                           lt_bapiret2
                    USING  lv_objectkey    "it_bom-matnr(Blank mat)
                           lv_objecttable  "MARA
                           lv_classtype.   "001
    READ TABLE lt_alloclist INTO ls_alloclist
                               INDEX 1.
    CHECK sy-subrc = 0.
    lv_classnum    = ls_alloclist-classnum.
* Get Characteristic Value by Class
    PERFORM bapi_objcl_getdetail
                 TABLES lt_allocvaluesnum
                        lt_characteristic  "Characteristic Data
                        lt_allocvaluescurr
                        lt_bapiret2
                 USING  lv_objectkey   "Blank mat number
                        lv_objecttable "Table
                        lv_classnum    "Class number
                        lv_classtype.  "Class type
*Quality of Raw Maerial
    CLEAR: ls_characteristic.
    READ TABLE lt_characteristic INTO ls_characteristic
               WITH KEY charact = 'ZSTEEL_MATPROPERTY'.
    zsteel_matproperty  = ls_characteristic-value_char.

*Front Finishing Thickness
    CLEAR: ls_characteristic.
    READ TABLE lt_characteristic INTO ls_characteristic
               WITH KEY charact = 'ZFRONT_FINISHING_THICKNESS'.
    zfront_finishing_thickness = ls_characteristic-value_char.

*Back Finishing Thickness
    CLEAR: ls_characteristic.
    READ TABLE lt_characteristic INTO ls_characteristic
               WITH KEY charact = 'ZBACK_FINISHING_THICKNESS'.
    zback_finishing_thickness = ls_characteristic-value_char.

*Front Back Finishing Thickness
    CONCATENATE zfront_finishing_thickness zback_finishing_thickness
      INTO lv_frontback_thickness.
    CONDENSE lv_frontback_thickness NO-GAPS.

*Thickness/Diameter
    CLEAR: ls_characteristic.
    READ TABLE lt_characteristic INTO ls_characteristic
               WITH KEY charact = 'ZSPEC_THICK'.
    zspec_thick = ls_characteristic-value_char.

*Width/Inner Diameter
    CLEAR: ls_characteristic.
    READ TABLE lt_characteristic INTO ls_characteristic
               WITH KEY charact = 'ZSPEC_WIDTH'.
    zspec_width = ls_characteristic-value_char.

*Length
    CLEAR: ls_characteristic.
    READ TABLE lt_characteristic INTO ls_characteristic
               WITH KEY charact = 'ZSPEC_LENGTH'.
    zspec_length = ls_characteristic-value_char.

* combination of characteristics
    IF NOT ( zsteel_matproperty IS INITIAL AND
             lv_frontback_thickness IS INITIAL AND
             zspec_thick IS INITIAL AND
             zspec_width IS INITIAL AND
             zspec_length IS INITIAL ).

      CONCATENATE
          zsteel_matproperty
          lv_frontback_thickness
          zspec_thick
          zspec_width
          zspec_length
                  INTO lv_maktx2
                  SEPARATED BY '_'.

      <fs_ammat>-maktx2 = lv_maktx2.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " combination_of_characteristic
