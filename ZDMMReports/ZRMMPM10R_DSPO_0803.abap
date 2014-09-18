************************************************************************
* Program Name      : ZRMMPM10M_DSPO
* Author            : SeungJae, Lee
* Creation Date     : 2003.08.19.
* Specifications By : SeungJae, Lee
* Pattern           : Report 1 - 1
* Development Request No : UD1K901842
* Addl Documentation:
* Description       : DS & PO Basic Data
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zrmmpm10m_dspo  NO STANDARD PAGE HEADING  MESSAGE-ID zmmm.

TYPE-POOLS: slis, kkblo.

TABLES: mara,  " Material Master
        makt,  " Material Decsriptions
        eord,  " Purchasing Source List
        eina,  " Purchasing Info Record: General Data
        eine,  " Purchasing Info Record: Purchasing Organization Data
        equk,  " Quota File: Header
        equp,  " Quota File: Item
        marc,  " Plant Data for Material
        t001w, " Plant
        t023,  " Material Group
        t024,  " Purchasing Group
        t001l, " Storage location
        t024d, " Manager(MRP Controller)
        marm.  " Units of Measure for Material

DATA: BEGIN OF it_main OCCURS 0,
*       Material Master
        matnr LIKE mara-matnr, " Mterial No.
        werks LIKE marc-werks, " Plant
        lgort LIKE mard-lgort, " Storage Location
        bstme LIKE mara-bstme, " Order Unit
*        umrez LIKE marm-umrez, " Rounding Profile
*        "(Numerator for Conversion to Base Units of Measure)
        bstrf LIKE marc-bstrf, " Rounding value
        "(Rounding value for purchase order quantity)
        meins LIKE mara-meins, " Base of Unit
        bstmi LIKE marc-bstmi, " Minimum Lot Size
        eisbe LIKE marc-eisbe, " Safety Stock
        maktx LIKE makt-maktx, " Material Description
*       Source List
*        zeord LIKE eord-zeord, " Source List No
        lifnr_sl LIKE eord-lifnr, " Vendor
        ebeln LIKE eord-ebeln, " Scheduling Agreement No.
        ebelp LIKE eord-ebelp, " Scheduling Agreement Item No.
*       Info Record
        infnr LIKE eine-infnr, " Info Record no
        lifnr LIKE eina-lifnr, " Vendor
        name1 LIKE lfa1-name1, " Vendor Name
        netpr LIKE eine-netpr, " Net price in purchasing info record
        waers LIKE eine-waers, " Currency
*        peinh LIKE eine-peinh, " Unit Price
*       Quota Arrangement
        qunum LIKE equp-qunum, " Quota Arrangement No.
        qupos LIKE equp-qupos, " Quota Arrangement Item No.
        lifnr_qa LIKE equp-lifnr,
        profl LIKE mara-profl,
      END   OF it_main.

* Material Master
DATA: BEGIN OF it_mara OCCURS 0,
        matnr LIKE mara-matnr, " Material No.
        matkl LIKE mara-matkl, " Material Group
        bstme LIKE mara-bstme, " Order Unit
*        umrez LIKE marm-umrez, " Rounding Profile
*        "(Numerator for Conversion to Base Units of Measure)
        bstrf LIKE marc-bstrf, " Rounding value
        "(Rounding value for purchase order quantity)
        meins LIKE mara-meins, " Base of Unit
        werks LIKE marc-werks, " Plant
        bstmi LIKE marc-bstmi, " Minimum Lot Size
        eisbe LIKE marc-eisbe, " Safety Stock
        ekgrp LIKE marc-ekgrp, " Purchasing Group
        dispo LIKE marc-dispo, " Manager
        lgort LIKE mard-lgort, " Storage location
        maktx LIKE makt-maktx, " Material Description
        profl LIKE mara-profl, " source
      END   OF it_mara.

* Source List
DATA: BEGIN OF it_sl OCCURS 0,
*        zeord LIKE eord-zeord, " Source List No
        lifnr LIKE eord-lifnr, " Vendor
        ebeln LIKE eord-ebeln, " Scheduling Agreement No.
        ebelp LIKE eord-ebelp, " Scheduling Agreement Item No.
      END   OF it_sl.
DATA  wa_sl LIKE it_sl.
DATA  BEGIN OF it_eord OCCURS 0.
DATA:   matnr LIKE eord-matnr, " Material No.
        werks LIKE eord-werks. " Plant
        INCLUDE STRUCTURE it_sl.
DATA  END   OF it_eord.

* Info Record
DATA: BEGIN OF it_ir OCCURS 0,
        infnr LIKE eine-infnr, " Info Record no
        lifnr LIKE eina-lifnr, " Vendor
        name1 LIKE lfa1-name1, " Vendor Name
        netpr LIKE eine-netpr, " Net price in purchasing info record
        waers LIKE eine-waers, " Currency
*        peinh LIKE eine-peinh, " Unit Price
*        rdprf LIKE eine-rdprf, " Rounding Profile
      END   OF it_ir.
DATA  wa_ir LIKE it_ir.
DATA  BEGIN OF it_eina OCCURS 0.
DATA:   matnr LIKE eina-matnr, " Material No.
        werks LIKE eine-werks, " Plant
        ekorg LIKE eine-ekorg, " Purchasing Group
        esokz LIKE eine-esokz. " Info Record Category
        INCLUDE STRUCTURE it_ir.
DATA  END   OF it_eina.

* Quota Arrangement
DATA: BEGIN OF it_qa OCCURS 0,
        qunum LIKE equp-qunum, " Quota Arrangement No.
        qupos LIKE equp-qupos, " Quota Arrangement Item No.
        lifnr LIKE equp-lifnr,
      END   OF it_qa.
DATA  wa_qa LIKE it_qa.
DATA  BEGIN OF it_equp OCCURS 0.
DATA:   matnr LIKE equk-matnr,
        werks LIKE equk-werks.
        INCLUDE STRUCTURE it_qa.
DATA  END   OF it_equp.

* Deep Structure Internal Table(Main)
DATA: BEGIN OF it_main_deep OCCURS 0,
*       Material Master(key)
        matnr LIKE mara-matnr, " Mterial No.
        werks LIKE marc-werks, " Plant
        lgort LIKE mard-lgort, " Storage Location
        profl LIKE mara-profl,
        bstme LIKE mara-bstme, " Order Unit
*        umrez LIKE marm-umrez, " Rounding Profile
*        "(Numerator for Conversion to Base Units of Measure)
        bstrf LIKE marc-bstrf, " Rounding value
        "(Rounding value for purchase order quantity)
        meins LIKE mara-meins, " Base of Unit
        bstmi LIKE marc-bstmi, " Minimum Lot Size
        eisbe LIKE marc-eisbe, " Safety Stock
        maktx LIKE makt-maktx, " Material Description
*       Source List
        it_sl LIKE TABLE OF it_sl,
*       Info Record
        it_ir LIKE TABLE OF it_ir,
*       Quota Arrangement
        it_qa LIKE TABLE OF it_qa,
      END   OF it_main_deep.


*    ALV Structure
DATA: rep         LIKE sy-repid,
      wa_layout   TYPE slis_layout_alv,
      it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      it_sort     TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      wa_print    TYPE  slis_print_alv.

*    Top-Of-Page
DATA: gt_list_top_of_page TYPE slis_t_listheader WITH HEADER LINE,
      p_text(255).

*---
CONSTANTS : c_ekorg LIKE eine-ekorg VALUE 'PU01'.


*********Selection Screen***********************************************
SELECTION-SCREEN BEGIN OF BLOCK sel1 WITH FRAME TITLE text-000.
SELECT-OPTIONS : s_matnr FOR mara-matnr,     " Material No.
                 s_matkl FOR t023-matkl,     " Material Group.
                 s_ekgrp FOR t024-ekgrp,     " Purchasing Group.
                 s_werks FOR t001w-werks,    " Plant
                 s_lgort FOR t001l-lgort,    " Storage Location
                 s_dispo FOR t024d-dispo,    " OBLIGATORY.
" Manager(MRP Controller)
                 s_mtart FOR mara-mtart DEFAULT 'ROH' OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK sel1.

*********At Selection Screen********************************************
AT SELECTION-SCREEN.
*  PERFORM check_select-options.
  PERFORM get_data.

*********Start Of Selection*********************************************
START-OF-SELECTION.

  PERFORM build_main_data.
*  BREAK-POINT.

*********End Of Selection***********************************************
END-OF-SELECTION.
  PERFORM display_list_first.





*&---------------------------------------------------------------------*
*&      Form  check_select-options
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_select-options.

* Check Matnr
  IF NOT s_matnr-low IS INITIAL.
    SELECT SINGLE * FROM mara WHERE matnr = s_matnr-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-001 'Material No.'.
    ENDIF.
  ENDIF.
* Check Material Group
  IF NOT s_matkl-low IS INITIAL.
    SELECT SINGLE * FROM t023 WHERE matkl = s_matkl-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-001 'Material Group'.
    ENDIF.
  ENDIF.
* Check Purchasing Group
  IF NOT s_ekgrp-low IS INITIAL.
    SELECT SINGLE * FROM t024 WHERE ekgrp = s_ekgrp-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-001 'Purchasing Group'.
    ENDIF.
  ENDIF.
* Check Plant
  IF NOT s_werks-low IS INITIAL.
    SELECT SINGLE * FROM t001w WHERE werks = s_werks-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-001 'Plant'.
    ENDIF.
  ENDIF.
* Check Storage Location
  IF NOT s_lgort-low IS INITIAL.
    SELECT SINGLE * FROM t001l WHERE werks IN s_werks
                                 AND lgort = s_lgort-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-001 'Storage location'.
    ENDIF.
  ENDIF.
* Check Manager(MRP Controller)
  IF NOT s_dispo-low IS INITIAL.
    SELECT SINGLE * FROM t024d WHERE werks IN s_werks
                                 AND dispo = s_dispo-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-001 'Manager'.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_select-options
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
* Material Master
  PERFORM get_material_master.
* Source List
  PERFORM get_source_list.
* Info Record
  PERFORM get_info_record.
* Quota Arrangement
  PERFORM get_quota_arrangement.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  Get_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_master.
*---
  CLEAR : it_mara.
  REFRESH : it_mara.

*---
  SELECT a~matnr a~matkl a~bstme
         a~meins c~werks c~bstmi
         c~eisbe c~ekgrp c~dispo
         d~lgort t~maktx c~bstrf
         a~profl
                 INTO CORRESPONDING FIELDS OF TABLE it_mara
                 FROM mara AS a INNER JOIN marc AS c
                   ON a~mandt EQ c~mandt
                  AND a~matnr = c~matnr
                      INNER JOIN mard AS d
                         ON c~mandt EQ d~mandt
                        AND c~matnr = d~matnr
                        AND c~werks = d~werks
                            INNER JOIN makt AS t
                               ON d~mandt EQ t~mandt
                              AND d~matnr = t~matnr
                              AND t~spras = sy-langu
                            WHERE a~matnr IN s_matnr
                              AND a~matkl IN s_matkl
                              AND c~ekgrp IN s_ekgrp
                              AND c~werks IN s_werks
                              AND c~dispo IN s_dispo
                              AND d~lgort IN s_lgort
                              AND a~mtart IN s_mtart
                              AND d~lgort NE '9999'
                         ORDER BY a~matnr c~werks d~lgort.

  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-002.
  ENDIF.

*  LOOP AT it_mara WHERE bstme NE space.
*    SELECT SINGLE umrez INTO it_mara-umrez
*      FROM marm WHERE matnr = it_mara-matnr
*                  AND meinh = it_mara-bstme.
*    IF sy-subrc = 0.
*      MODIFY it_mara.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " Get_material_master
*&---------------------------------------------------------------------*
*&      Form  get_source_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_source_list.
*---
  CLEAR : it_eord.
  REFRESH : it_eord.

*---
  SELECT matnr werks zeord lifnr ebeln ebelp
               INTO CORRESPONDING FIELDS OF TABLE it_eord
               FROM eord
              WHERE matnr IN s_matnr
                AND werks IN s_werks
                AND vdatu <= sy-datum
                AND bdatu >= sy-datum
           ORDER BY matnr werks zeord.
ENDFORM.                    " get_source_list
*&---------------------------------------------------------------------*
*&      Form  get_Info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_record.
*---
  CLEAR : it_eina.
  REFRESH : it_eina.

*---
  SELECT a~matnr a~lifnr e~werks
         e~ekorg e~infnr e~esokz
         e~netpr e~waers
                 INTO CORRESPONDING FIELDS OF TABLE it_eina
                 FROM eina AS a INNER JOIN eine AS e
                   ON a~infnr = e~infnr
                WHERE a~matnr IN s_matnr
                  AND e~ekorg EQ c_ekorg                    " PU01
                  AND e~ekgrp IN s_ekgrp
*                  AND e~werks IN s_werks
                  AND e~werks EQ space
             ORDER BY a~matnr e~werks e~ekorg e~infnr.

* Get Name
  CHECK NOT it_eina[] IS INITIAL.

  DATA: BEGIN OF it_lifnr OCCURS 0,
          lifnr LIKE lfa1-lifnr,
        END   OF it_lifnr.

  DATA: BEGIN OF it_lifnr_name OCCURS 0,
          lifnr LIKE lfa1-lifnr,
          name1 LIKE lfa1-name1,
        END   OF it_lifnr_name.

  LOOP AT it_eina WHERE lifnr NE space.
    it_lifnr-lifnr = it_eina-lifnr.
    COLLECT it_lifnr. CLEAR it_lifnr.
  ENDLOOP.

  SELECT lifnr name1 INTO TABLE it_lifnr_name FROM lfa1
     FOR ALL ENTRIES IN it_lifnr
   WHERE lifnr = it_lifnr-lifnr.

  LOOP AT it_eina WHERE lifnr NE space.
    READ TABLE it_lifnr_name WITH KEY lifnr = it_eina-lifnr.
    CHECK sy-subrc = 0.
    it_eina-name1 = it_lifnr_name-name1.
    MODIFY it_eina.
  ENDLOOP.

ENDFORM.                    " get_Info_record
*&---------------------------------------------------------------------*
*&      Form  get_Quota_arrangement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_quota_arrangement.
*---
  CLEAR : it_equp.
  REFRESH : it_equp.

*---
  SELECT k~matnr k~werks k~qunum
         p~qupos p~lifnr
                 INTO CORRESPONDING FIELDS OF TABLE it_equp
                 FROM equk AS k INNER JOIN equp AS p
                   ON k~qunum  = p~qunum
                WHERE k~matnr IN s_matnr
                  AND k~werks IN s_werks
                  AND k~bdatu >= sy-datum
                  AND k~vdatu <= sy-datum
             ORDER BY k~matnr k~werks k~qunum p~qupos.
ENDFORM.                    " get_Quota_arrangement
*&---------------------------------------------------------------------*
*&      Form  build_main_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_main_data.
* Material Master(it_mara)
* Source List(it_eord)
* Info Record(it_eina)
* Quota Arrangement(it_equp)

****Build Deep Structure Internal Table*********************************
  CLEAR : it_main_deep, wa_sl, wa_ir, wa_qa.
  REFRESH : it_main_deep.

*---
  LOOP AT it_mara.
*   Source List
    MOVE-CORRESPONDING it_mara TO it_main_deep.
    LOOP AT it_eord WHERE matnr = it_mara-matnr
                      AND werks = it_mara-werks.
      MOVE-CORRESPONDING it_eord TO wa_sl.
      APPEND wa_sl TO it_main_deep-it_sl. CLEAR wa_sl.
    ENDLOOP.
*   Info Record
    LOOP AT it_eina WHERE matnr = it_mara-matnr.
*                      AND werks = it_mara-werks.
      MOVE-CORRESPONDING it_eina TO wa_ir.
      APPEND wa_ir TO it_main_deep-it_ir. CLEAR wa_ir.
    ENDLOOP.
*   Quota Arrangement
    LOOP AT it_equp WHERE matnr = it_mara-matnr
                      AND werks = it_mara-werks.
      MOVE-CORRESPONDING it_equp TO wa_qa.
      APPEND wa_qa TO it_main_deep-it_qa. CLEAR wa_qa.
    ENDLOOP.
    APPEND it_main_deep. CLEAR it_main_deep.
  ENDLOOP.

****Build Main Internal Table*********************************
  DATA: p_sl_lines   TYPE i, " Source List Lines
        p_ir_lines   TYPE i, " Info Record Lines
        p_qa_lines   TYPE i, " Quota Arrangement Lines
        p_max_lines  TYPE i,
        p_index      TYPE i.
  DATA: BEGIN OF it_lines OCCURS 0,
          lines TYPE i,
        END   OF it_lines.
  CLEAR   it_main.
  REFRESH it_main.
  LOOP AT it_main_deep.
    CLEAR: p_sl_lines, p_ir_lines, p_qa_lines.
    MOVE-CORRESPONDING it_main_deep TO it_main.
    DESCRIBE TABLE it_main_deep-it_sl LINES p_sl_lines.
    IF p_sl_lines > 0.
      it_lines-lines = p_sl_lines. APPEND it_lines. CLEAR it_lines.
    ENDIF.
    DESCRIBE TABLE it_main_deep-it_ir LINES p_ir_lines.
    IF p_ir_lines > 0.
      it_lines-lines = p_ir_lines. APPEND it_lines. CLEAR it_lines.
    ENDIF.
    DESCRIBE TABLE it_main_deep-it_qa LINES p_qa_lines.
    IF p_qa_lines > 0.
      it_lines-lines = p_qa_lines. APPEND it_lines. CLEAR it_lines.
    ENDIF.
    SORT BY it_lines DESCENDING.
    READ TABLE it_lines INDEX 1.
    IF sy-subrc = 0.
      p_max_lines = it_lines-lines.
      CLEAR it_lines. REFRESH it_lines.
      DO p_max_lines TIMES.
        ADD 1 TO p_index.
*       Source List
        READ TABLE it_main_deep-it_sl INDEX p_index INTO wa_sl.
        IF sy-subrc = 0.
*          it_main-zeord = wa_sl-zeord. "Source List No
          it_main-lifnr_sl = wa_sl-lifnr. "Vendor
          it_main-ebeln = wa_sl-ebeln. "Scheduling Agreement No.
          it_main-ebelp = wa_sl-ebelp. "Scheduling Agreement Item No.
        ENDIF.
*       Info Record
        READ TABLE it_main_deep-it_ir INDEX p_index INTO wa_ir.
        IF sy-subrc = 0.
          it_main-infnr = wa_ir-infnr. " Info Record no
          it_main-lifnr = wa_ir-lifnr. " Vendor
          it_main-name1 = wa_ir-name1. " Vendor Name
          it_main-netpr = wa_ir-netpr. " Net Price in info record
          it_main-waers = wa_ir-waers. " Currency
*          it_main-peinh = wa_ir-peinh. " Unit Price
*          it_main-rdprf = wa_ir-rdprf. " Rounding Profile
        ENDIF.
*       Quota Arrangement
        READ TABLE it_main_deep-it_qa INDEX p_index INTO wa_qa.
        IF sy-subrc = 0.
          it_main-qunum = wa_qa-qunum. " Quota Arrangement No.
          it_main-qupos = wa_qa-qupos. " Quota Arrangement Item No.
        ENDIF.
        APPEND it_main.
        CLEAR: wa_sl, wa_ir, wa_qa,
               it_main-lifnr_sl, it_main-lifnr, it_main-name1,
               it_main-ebeln,    it_main-ebelp, it_main-infnr,
               it_main-netpr,    it_main-waers, it_main-qunum,
               it_main-qupos.     " it_main-zeord.
      ENDDO.
      CLEAR: p_index, p_max_lines.
    ELSE.
      APPEND it_main.
    ENDIF.
    CLEAR it_main.
  ENDLOOP.
ENDFORM.                    " build_main_data
*&---------------------------------------------------------------------*
*&      Form  display_list_first
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_list_first.
*---
  CLEAR : rep.
  rep = sy-repid.

  wa_layout-colwidth_optimize = 'X'.
  wa_print-no_print_selinfos  = 'X'.
  wa_print-no_coverpage       = 'X'.
  wa_print-no_new_page        = 'X'.
  wa_print-reserve_lines      = 2.
  wa_print-no_print_listinfos = 'X'.

  PERFORM build_fieldcat.
  PERFORM build_top_of_page.
  PERFORM build_sort.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
      i_bypassing_buffer                = 'X'
      i_buffer_active                   = 'X'
      i_callback_program                = rep
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
      i_callback_top_of_page            = 'TOP_OF_PAGE'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      is_layout                         = wa_layout
      it_fieldcat                       = it_fieldcat[]
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
      it_sort                           = it_sort[]
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
      i_default                         = 'X'
      i_save                            = 'A'
*     IS_VARIANT                        =
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
      is_print                          = wa_print
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_ADD_FIELDCAT                   =
*     IT_HYPERLINK                      =
*     I_HTML_HEIGHT_TOP                 =
*     I_HTML_HEIGHT_END                 =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_main
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " display_list_first
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
*---
  CLEAR : it_fieldcat.
  REFRESH : it_fieldcat.

*---
  CHECK it_fieldcat[] IS INITIAL.

*PERFORM append_fieldcat USING  p_fieldname
*                               p_tabname
*                               p_outputlen
*                               p_text_l
*                               p_text_m
*                               p_text_s
*                               p_datatype
*                               p_key
*                               p_no_out
*                               p_unit
*                               p_currency
*                               p_text_field.

  PERFORM append_fieldcat USING 'MATNR'
                                'IT_MAIN'
                                18
                                'Material No'
                                'Material No'
                                'Material No'
                                'CHAR'
                                'X'
                                ''
                                ''
                                ''
                                'MAKTX'.

  PERFORM append_fieldcat USING 'MAKTX'
                                'IT_MAIN'
                                40
                                'Material description'
                                'Material description'
                                'Material desc'
                                'CHAR'
                                'X'
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'WERKS'
                                'IT_MAIN'
                                4
                                'Plant'
                                'Plant'
                                'Plant'
                                'NUMC'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'LGORT'
                                'IT_MAIN'
                                4
                                'Storage Location'
                                'Storage Location'
                                'St.Location'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'PROFL'
                                'IT_MAIN'
                                6
                                'Source'
                                'Source'
                                'Source'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

*  PERFORM append_fieldcat USING 'BSTMI'
*                                'IT_MAIN'
*                                16
*                                'Minimum lot size'
*                                'Minimum lot size'
*                                'Mini.lot size'
*                                'QUAN'
*                                ''
*                                ''
*                                'MEINS'
*                                ''
*                                ''.

*  PERFORM append_fieldcat USING 'EISBE'
*                                'IT_MAIN'
*                                16
*                                'Safety stock'
*                                'Safety stock'
*                                'Safety stock'
*                                'QUAN'
*                                ''
*                                ''
*                                'MEINS'
*                                ''
*                                ''.

*  PERFORM append_fieldcat USING 'MEINS'
*                                'IT_MAIN'
*                                3
*                                'Base unit Of Measure'
*                                'Base unit Of Measure'
*                                'BoM'
*                                'UNIT'
*                                ''
*                                ''
*                                ''
*                                ''
*                                ''.

*  PERFORM append_fieldcat USING 'BSTME'
*                                'IT_MAIN'
*                                3
*                                'Order Unit'
*                                'Order Unit'
*                                'OoM'
*                                'UNIT'
*                                ''
*                                ''
*                                ''
*                                ''
*                                ''.

**  PERFORM append_fieldcat USING 'UMREZ'
**                                'IT_MAIN'
**                                5
**                                'Rounding profile'
**                                'Rounding profile'
**                                'Round.profile'
**                                'DEC'
**                                ''
**                                ''
**                                ''
**                                ''
**                                ''.

*  PERFORM append_fieldcat USING 'BSTRF'
*                                'IT_MAIN'
*                                16
*                                'Rounding value'
*                                'Rounding value'
*                                'Round.value'
*                                'QUAN'
*                                ''
*                                ''
*                                'MEINS'
*                                ''
*                                ''.

  PERFORM append_fieldcat USING 'INFNR'
                                'IT_MAIN'
                                11
                                'Info Record'
                                'Info Record'
                                'Info Record'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'LIFNR'
                                'IT_MAIN'
                                10
                                'Vendor'
                                'Vendor'
                                'Vendor'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

*  PERFORM append_fieldcat USING 'NAME1'
*                                'IT_MAIN'
*                                35
*                                'Vendor Name'
*                                'Vendor Name'
*                                'Vendor Name'
*                                'CHAR'
*                                ''
*                                ''
*                                ''
*                                ''
*                                ''.

*  PERFORM append_fieldcat USING 'NETPR'
*                                'IT_MAIN'
*                                14
*                                'Net Price'
*                                'Net Price'
*                                'Net Price'
*                                'CURR'
*                                ''
*                                ''
*                                ''
*                                'WAERS'
*                                ''.

*  PERFORM append_fieldcat USING 'WAERS'
*                                'IT_MAIN'
*                                5
*                                'Currency Key'
*                                'Currency Key'
*                                'Currency'
*                                'CUKY'
*                                ''
*                                ''
*                                ''
*                                ''
*                                ''.

*  PERFORM append_fieldcat USING 'ZEORD'
*                                'IT_MAIN'
*                                5
*                                'Number of source list record'
*                                'Source List No'
*                                'Source List No'
*                                'NUMC'
*                                ''
*                                ''
*                                ''
*                                ''
*                                ''.

  PERFORM append_fieldcat USING 'LIFNR_SL'
                                'IT_MAIN'
                                10
                                'S/L Vendor'
                                'S/L Vendor'
                                'S/L Vendor'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'EBELN'
                                'IT_MAIN'
                                13
                                'Sch.Agreement'
                                'Sch.Agreement'
                                'Sch.Agreement'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'EBELP'
                                'IT_MAIN'
                                7
                                'SA Item'
                                'SA Item'
                                'SA Item'
                                'NUMC'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'QUNUM'
                                'IT_MAIN'
                                10
                                'Quota'
                                'Quota'
                                'Quota'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'QUPOS'
                                'IT_MAIN'
                                3
                                'Item'
                                'Item'
                                'Item'
                                'NUMC'
                                ''
                                ''
                                ''
                                ''
                                ''.

ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  append_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1241   text
*      -->P_10     text
*      -->P_1243   text
*      -->P_1244   text
*      -->P_1245   text
*      -->P_1246   text
*----------------------------------------------------------------------*
FORM append_fieldcat USING    p_fieldname
                              p_tabname
                              p_outputlen
                              p_text_l
                              p_text_m
                              p_text_s
                              p_datatype
                              p_key
                              p_no_out
                              p_unit
                              p_currency
                              p_text_field.
  it_fieldcat-fieldname      = p_fieldname.
  it_fieldcat-tabname        = p_tabname.
  it_fieldcat-outputlen      = p_outputlen.
  it_fieldcat-seltext_l      = p_text_l.
  it_fieldcat-seltext_m      = p_text_m.
  it_fieldcat-seltext_s      = p_text_s.
  it_fieldcat-datatype       = p_datatype.
  it_fieldcat-key            = p_key.
  it_fieldcat-no_out         = p_no_out.
  it_fieldcat-qfieldname     = p_unit.
  it_fieldcat-cfieldname     = p_currency.
  it_fieldcat-text_fieldname = p_text_field.
  APPEND it_fieldcat. CLEAR it_fieldcat.
ENDFORM.                    " append_fieldcat
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort.
  CHECK it_sort[] IS INITIAL.
  it_sort-fieldname = 'MATNR'.
  it_sort-tabname   = 'IT_MAIN'.
  it_sort-up        = 'X'.
  it_sort-group     = 'G1'.
  APPEND it_sort. CLEAR it_sort.
  it_sort-fieldname = 'MAKTX'.
  it_sort-tabname   = 'IT_MAIN'.
  it_sort-up        = 'X'.
  it_sort-group     = 'G1'.
*  APPEND it_sort. CLEAR it_sort.
*  it_sort-fieldname = 'WERKS'.
*  it_sort-tabname   = 'IT_MAIN'.
*  it_sort-up        = 'X'.
*  it_sort-group     = 'G2'.
*  APPEND it_sort. CLEAR it_sort.
*  it_sort-fieldname = 'LGORT'.
*  it_sort-tabname   = 'IT_MAIN'.
*  it_sort-up        = 'X'.
*  it_sort-group     = 'G2'.
*  APPEND it_sort. CLEAR it_sort.
*  it_sort-fieldname = 'BSTME'.
*  it_sort-tabname   = 'IT_MAIN'.
*  it_sort-up        = 'X'.
*  it_sort-group     = 'G2'.
*  APPEND it_sort. CLEAR it_sort.
*  it_sort-fieldname = 'MEINS'.
*  it_sort-tabname   = 'IT_MAIN'.
*  it_sort-up        = 'X'.
*  it_sort-group     = 'G2'.
*  APPEND it_sort. CLEAR it_sort.
*  it_sort-fieldname = 'BSTMI'.
*  it_sort-tabname   = 'IT_MAIN'.
*  it_sort-up        = 'X'.
*  it_sort-group     = 'G2'.
*  APPEND it_sort. CLEAR it_sort.
*  it_sort-fieldname = 'EISBE'.
*  it_sort-tabname   = 'IT_MAIN'.
*  it_sort-up        = 'X'.
*  it_sort-group     = 'G2'.
*  APPEND it_sort. CLEAR it_sort.


ENDFORM.                    " BUILD_SORT
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*            i_logo             = 'IDES_LOGO'
            it_list_commentary = gt_list_top_of_page[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  build_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_top_of_page.
* LIST HEADING LINE: TYPE H
  CLEAR gt_list_top_of_page.
  gt_list_top_of_page-typ  = 'H'.
  gt_list_top_of_page-info = text-t01.
  APPEND gt_list_top_of_page.
* STATUS LINE: TYPE S

  CLEAR gt_list_top_of_page.

  gt_list_top_of_page-typ  = 'S'.

*--- material number
  IF NOT s_matnr[] IS INITIAL.
    IF s_matnr-high IS INITIAL.
      p_text = s_matnr-low.
    ELSE.
      CONCATENATE s_matnr-low '~' s_matnr-high INTO p_text
        SEPARATED BY space.
    ENDIF.
    gt_list_top_of_page-key  = text-003. "Material No.
    gt_list_top_of_page-info = p_text.
    APPEND gt_list_top_of_page.
  ENDIF.

*--- material group
  IF NOT s_matkl[] IS INITIAL.
    IF s_matkl-high IS INITIAL.
      p_text = s_matkl-low.
    ELSE.
      CONCATENATE s_matkl-low '~' s_matkl-high INTO p_text
        SEPARATED BY space.
    ENDIF.
    gt_list_top_of_page-key  = text-004. "Material Group
    gt_list_top_of_page-info = p_text.
    APPEND gt_list_top_of_page.
  ENDIF.

*--- purchasing group
  IF NOT s_ekgrp[] IS INITIAL.
    IF s_ekgrp-high IS INITIAL.
      p_text = s_ekgrp-low.
    ELSE.
      CONCATENATE s_ekgrp-low '~' s_ekgrp-high INTO p_text
        SEPARATED BY space.
    ENDIF.
    gt_list_top_of_page-key  = text-005. "Purchasing Group
    gt_list_top_of_page-info = p_text.
    APPEND gt_list_top_of_page.
  ENDIF.

*--- plant
  IF NOT s_werks[] IS INITIAL.
    IF s_werks-high IS INITIAL.
      p_text = s_werks-low.
    ELSE.
      CONCATENATE s_werks-low '~' s_werks-high INTO p_text
        SEPARATED BY space.
    ENDIF.
    gt_list_top_of_page-key  = text-006. "Plant
    gt_list_top_of_page-info = p_text.
    APPEND gt_list_top_of_page.
  ENDIF.

*--- storage location
  IF NOT s_lgort[] IS INITIAL.
    IF s_lgort-high IS INITIAL.
      p_text = s_lgort-low.
    ELSE.
      CONCATENATE s_lgort-low '~' s_lgort-high INTO p_text
        SEPARATED BY space.
    ENDIF.
    gt_list_top_of_page-key  = text-007. "Storage Location
    gt_list_top_of_page-info = p_text.
    APPEND gt_list_top_of_page.
  ENDIF.

*--- person of contact
  IF NOT s_dispo[] IS INITIAL.
    IF s_dispo-high IS INITIAL.
      p_text = s_dispo-low.
    ELSE.
      CONCATENATE s_dispo-low '~' s_dispo-high INTO p_text
        SEPARATED BY space.
    ENDIF.
    gt_list_top_of_page-key  = text-008. "Manager
    gt_list_top_of_page-info = p_text.
    APPEND gt_list_top_of_page.
  ENDIF.

  gt_list_top_of_page-key  = space.
  gt_list_top_of_page-info = space.
  APPEND gt_list_top_of_page.
ENDFORM.                    " build_top_of_page
