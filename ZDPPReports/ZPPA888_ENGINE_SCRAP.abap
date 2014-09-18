************************************************************************
* Program Name      : ZPPA888_ENGINE_SCRAP
* Author            : Furong Wang
* Creation Date     : 09/22/2008
* Specifications By :
* Addl Documentation:
* Description       : Scrap for Engine
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************
REPORT ZPPA888_ENGINE_SCRAP NO STANDARD PAGE HEADING
                        LINE-SIZE 132
                        LINE-COUNT 64(1)
                        MESSAGE-ID ZMPP.
TYPE-POOLS : SLIS.

TABLES: ZTPP_OSDP, MSEG.

*DATA: IT_ITAB LIKE TABLE OF ZTPPERM WITH HEADER LINE.
DATA : BEGIN OF IT_ITAB OCCURS 0,
*       model LIKE ztpp_osdp-model,
*       body_ser LIKE ztpp_osdp-body_ser,
*       porder  LIKE ztpp_osdp-porder,
*       resb_no LIKE ztpp_osdp-resb_no,
*       resb_item LIKE ztpp_osdp-resb_item,
       MATNR LIKE ZTPPERM-EITEM,
*       LGORT LIKE MARC-LGPRO,
       SQTY LIKE ZTPPERM-SQTY,
*       meins LIKE ztpp_osdp-meins,
       EASSYID LIKE ZTPPERM-EASSYID,
       PROD_DT LIKE ZTPPERM-PROD_DT,
       STATUS LIKE ZTPPERM-STATUS,
       MBLNR LIKE MSEG-MBLNR,
       EUSAGE LIKE ZTPPERM-EUSAGE,
*       mjahr LIKE MSEG-mjahr,
       ZRESULT LIKE ZTPPERM-ZRESULT,
       PROCESSED(1),
       MESSA(80),
       GR(1),
       LINECOLOR(4),     " ALV Color
      END OF IT_ITAB.

DATA : BEGIN OF IT_WRITE_TEMP OCCURS 0.
        INCLUDE STRUCTURE IT_ITAB.
DATA : END OF IT_WRITE_TEMP.

DATA : IT_WRITE      LIKE IT_WRITE_TEMP OCCURS 0 WITH HEADER LINE,
       IT_ITAB_COPY  LIKE IT_ITAB OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_MATNR OCCURS 0,
      MATNR  LIKE ZTPPERM-EITEM,
      STATUS LIKE ZTPPERM-STATUS,
      END OF IT_MATNR.

DATA: BEGIN OF IT_MARC OCCURS 0,
      MATNR  LIKE ZTPPERM-EITEM,
      LGPRO  LIKE MARC-LGPRO,
      END OF IT_MARC.

DATA: BEGIN OF IT_BOM OCCURS 0,
      MATNR LIKE ZTPPERM-EITEM,
      COMP LIKE ZTPPERM-EITEM,
      QNTY  LIKE ZTPPERM-SQTY,
      DATAB LIKE STPOX-DATUV,
      DATBI LIKE STPOX-DATUB,
      SORTF LIKE STPOX-SORTF,
      END OF IT_BOM.

DATA:  C_YELL(4)  VALUE 'C310',
       C_GREEN(4) VALUE 'C510',
       C_RED(4)   VALUE 'C610'.

*--- BAPI
DATA : W_GOODSMVT_HEADER  LIKE BAPI2017_GM_HEAD_01,
       W_GOODSMVT_CODE    LIKE BAPI2017_GM_CODE,
       W_GOODSMVT_HEADRET LIKE BAPI2017_GM_HEAD_RET,
       W_MATERIALDOCUMENT LIKE BAPI2017_GM_HEAD_RET-MAT_DOC,
       W_MATDOCUMENTYEAR  LIKE BAPI2017_GM_HEAD_RET-DOC_YEAR,
       IT_GOODSMVT_ITEM
            LIKE TABLE OF BAPI2017_GM_ITEM_CREATE  WITH HEADER LINE,
       IT_GOODSMVT_SERIALNUMBER
            LIKE TABLE OF BAPI2017_GM_SERIALNUMBER WITH HEADER LINE,
       IT_RETURN
            LIKE TABLE OF BAPIRET2                 WITH HEADER LINE.

**--- Variables
DATA : W_LINES TYPE I,
       W_SUBRC LIKE SY-SUBRC.

*--- ALV
DATA : W_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       W_EVENTCAT TYPE SLIS_T_EVENT WITH HEADER LINE,
       W_SELFIELD TYPE SLIS_SELFIELD,
       W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
       W_COL_POS  TYPE I,
       W_PROGRAM  LIKE SY-REPID,
       W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
       W_LINE TYPE SLIS_LISTHEADER,
       W_LAYOUT   TYPE SLIS_LAYOUT_ALV.

**--- Constants
CONSTANTS : C_WERKS LIKE MSEG-WERKS VALUE 'E001',
            C_GM_CODE LIKE W_GOODSMVT_CODE-GM_CODE VALUE '03',
            C_BWART_551 LIKE MSEG-BWART VALUE '551',
            C_GM_CODE_04 LIKE BAPI2017_GM_CODE VALUE '04',
            C_MTYPE(3) TYPE C VALUE '311',
            C_ISSUE_STLOC(4) VALUE 'E912',
            C_RECP_STLOC(4) VALUE 'X551'.

**--- Macro
DEFINE APPEND_FIELDCAT.
  &1 = &1 + 1.
  W_FIELDCAT-COL_POS    = &1.
  W_FIELDCAT-FIELDNAME  = &2.
  W_FIELDCAT-OUTPUTLEN  = &3.
  W_FIELDCAT-SELTEXT_L  = &4.
  W_FIELDCAT-SELTEXT_M  = &4.
  W_FIELDCAT-SELTEXT_S  = &4.
  W_FIELDCAT-DATATYPE   = &5.
  W_FIELDCAT-KEY        = &6.
  W_FIELDCAT-QFIELDNAME = &7.
  W_FIELDCAT-CFIELDNAME = &8.
  APPEND W_FIELDCAT.
  CLEAR : W_FIELDCAT.
END-OF-DEFINITION.

DEFINE APPEND_SORTCAT.
  W_SORTCAT-SPOS      = &1.
  W_SORTCAT-FIELDNAME = &2.
  W_SORTCAT-TABNAME   = &3.
  W_SORTCAT-UP        = &4.
  W_SORTCAT-SUBTOT    = &5.
  APPEND W_SORTCAT.
  CLEAR : W_SORTCAT.
END-OF-DEFINITION.

**---
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
*PARAMETERS : P_POST LIKE SY-DATUM OBLIGATORY.
*PARAMETERS : P_DOC LIKE SY-DATUM OBLIGATORY.
SELECT-OPTIONS: S_MATNR FOR MSEG-MATNR.
PARAMETERS : P_NEW(1).
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) TEXT-T02.
PARAMETERS : P_AUFNR LIKE MSEG-AUFNR.
SELECTION-SCREEN COMMENT 55(20) TEXT-T01.
SELECTION-SCREEN END OF LINE.
*PARAMETERS : p_giline type i default 1.
SELECTION-SCREEN END OF BLOCK BLOCK1.

INITIALIZATION.
  PERFORM SET_PARA_DATA.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM GET_BOM.

END-OF-SELECTION.
  IF IT_ITAB[] IS INITIAL.
    MESSAGE S000 WITH TEXT-M01.
  ELSE.
    PERFORM POSTING_DOCUMENT.
    PERFORM COMMENT_BUILD.     " USING w_top_of_page[].
    PERFORM MAKE_ALV_GRID.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.

*  DATA: lt_itab LIKE TABLE OF it_itab WITH HEADER LINE.
  DATA : L_DATUM TYPE D,
         L_GR(1).

  DATA: BEGIN OF LT_GR OCCURS 0,
        ERPID LIKE ZTPPERM-ERPID,
        ZMSG LIKE ZTPPERM-ZMSG,
        END OF LT_GR.

  CLEAR : IT_ITAB, IT_ITAB[].
  IF P_NEW = 'X'.
    SELECT EITEM AS MATNR SQTY EASSYID PROD_DT STATUS EUSAGE
      INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
       FROM ZTPPERM
       WHERE EITEM IN S_MATNR
         AND ERPID = 'E51'
         AND ZRESULT = 'I'.

  ELSE.
    SELECT EITEM AS MATNR SQTY EASSYID PROD_DT STATUS EUSAGE
     INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
      FROM ZTPPERM
      WHERE EITEM IN S_MATNR
        AND ERPID = 'E51'
        AND ( ZRESULT = 'I' OR ZRESULT = 'E' ).
  ENDIF.

  LOOP AT IT_ITAB.
    CLEAR: L_GR.
    SELECT ERPID ZMSG INTO TABLE LT_GR
      FROM ZTPPERM
      WHERE EASSYID = IT_ITAB-EASSYID
        AND ( ERPID = 'E01' OR ERPID = 'E02' OR
              ERPID = 'E03' )
       AND ZMSG <> SPACE.
    LOOP AT LT_GR.
      IF LT_GR-ZMSG+0(2) = 'GR'.
        L_GR = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF L_GR = 'X'.
      IT_ITAB-GR = L_GR.
    ELSE.
      CLEAR:  IT_ITAB-GR.
      MOVE IT_ITAB-MATNR TO IT_MATNR-MATNR.
      COLLECT IT_MATNR.
    ENDIF.
    MODIFY IT_ITAB.
    CLEAR: IT_MATNR, IT_ITAB.
  ENDLOOP.
  DESCRIBE TABLE IT_ITAB LINES W_LINES.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  posting_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POSTING_DOCUMENT.

  DATA : L_TABIX LIKE SY-TABIX,
         L_MOD TYPE I.

  CLEAR : IT_WRITE_TEMP, IT_WRITE_TEMP[], IT_WRITE, IT_WRITE[].

  LOOP AT IT_ITAB.
    MOVE : SY-TABIX TO L_TABIX.
*    l_mod = l_tabix MOD p_giline.
    PERFORM APPEND_WRITE_TABLE.
    IT_ITAB-PROCESSED = 'Y'.
    MODIFY IT_ITAB.
    IF IT_ITAB-GR IS INITIAL.
      PERFORM APPEND_BAPI_STRUCTURE.
*    IF l_mod EQ 0 OR l_tabix EQ w_lines.
      PERFORM GI_POSTING USING W_SUBRC.
    ELSE.
      PERFORM SCRAP_POSTING.
    ENDIF.
    APPEND LINES OF IT_WRITE_TEMP TO IT_WRITE.
    CLEAR : IT_WRITE_TEMP[], W_SUBRC.
*    ENDIF.
  ENDLOOP.
  PERFORM UPDATE_TABLE.

ENDFORM.                    " posting_document

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD.

  CLEAR : W_LINE.
  W_LINE-TYP  = 'H'.
  W_LINE-INFO = TEXT-002.
  APPEND W_LINE TO W_TOP_OF_PAGE.

  CLEAR : W_LINE.
  APPEND INITIAL LINE TO W_TOP_OF_PAGE.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_ALV_GRID.

  MOVE : 'LINECOLOR' TO W_LAYOUT-INFO_FIELDNAME,
         'X'         TO W_LAYOUT-COLWIDTH_OPTIMIZE.

  PERFORM BUILD_FIELDCAT.
  PERFORM BUILD_SORTCAT.

  CLEAR : W_PROGRAM.

  MOVE : SY-REPID TO W_PROGRAM.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM      = W_PROGRAM
*            i_callback_user_command = 'USER_COMMAND'
            IS_LAYOUT               = W_LAYOUT
            IT_FIELDCAT             = W_FIELDCAT[]
            IT_EVENTS               = W_EVENTCAT[]
            IT_SORT                 = W_SORTCAT[]
            I_SAVE                  = 'A'
       TABLES
            T_OUTTAB                = IT_WRITE
       EXCEPTIONS
            PROGRAM_ERROR           = 1
            OTHERS                  = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  set_posting_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARA_DATA.
*  P_POST = SY-DATUM.
*  P_DOC = P_POST.
  P_AUFNR = 'CE001'.
ENDFORM.                    " set_posting_date

*&---------------------------------------------------------------------*
*&      Form  gi_posting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GI_POSTING USING P_SUBRC.   " Component posting

  CLEAR : W_GOODSMVT_HEADER, W_GOODSMVT_CODE, W_GOODSMVT_HEADRET,
          W_MATERIALDOCUMENT, W_MATDOCUMENTYEAR,
          IT_RETURN, IT_RETURN[].

*  MOVE : P_DOC  TO W_GOODSMVT_HEADER-PSTNG_DATE,
*         P_DOC   TO W_GOODSMVT_HEADER-DOC_DATE,
  MOVE : IT_ITAB-PROD_DT  TO W_GOODSMVT_HEADER-PSTNG_DATE,
         IT_ITAB-PROD_DT   TO W_GOODSMVT_HEADER-DOC_DATE,
         IT_ITAB-EASSYID TO W_GOODSMVT_HEADER-HEADER_TXT,
         C_GM_CODE TO W_GOODSMVT_CODE-GM_CODE.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
       EXPORTING
            GOODSMVT_HEADER  = W_GOODSMVT_HEADER
            GOODSMVT_CODE    = W_GOODSMVT_CODE
       IMPORTING
            GOODSMVT_HEADRET = W_GOODSMVT_HEADRET
            MATERIALDOCUMENT = W_MATERIALDOCUMENT
            MATDOCUMENTYEAR  = W_MATDOCUMENTYEAR
       TABLES
            GOODSMVT_ITEM    = IT_GOODSMVT_ITEM
            RETURN           = IT_RETURN.

  READ TABLE IT_RETURN WITH KEY TYPE = 'E'.

  MOVE : SY-SUBRC TO P_SUBRC.

  IF P_SUBRC EQ 0.
    ROLLBACK WORK.
*    concatenate it_return-message 'hlw' into it_return-message.
    MOVE : C_RED             TO IT_WRITE_TEMP-LINECOLOR,
           IT_RETURN-MESSAGE TO IT_WRITE_TEMP-MESSA,
           'E' TO IT_WRITE_TEMP-ZRESULT,
           ' ' TO IT_WRITE_TEMP-PROCESSED.
    MODIFY IT_WRITE_TEMP TRANSPORTING LINECOLOR MESSA
                                      ZRESULT
                                WHERE MATNR NE SPACE.
    MODIFY IT_ITAB FROM IT_WRITE_TEMP
                          TRANSPORTING ZRESULT PROCESSED MESSA
                          WHERE PROCESSED = 'Y'.

  ELSE.
    COMMIT WORK AND WAIT.
    MOVE : C_GREEN            TO IT_WRITE_TEMP-LINECOLOR.
*           IT_RETURN-MESSAGE  TO IT_WRITE_TEMP-MESSA,
    CONCATENATE 'SC:'  W_MATERIALDOCUMENT  INTO
           IT_WRITE_TEMP-MESSA SEPARATED BY SPACE.
    MOVE:  W_MATERIALDOCUMENT TO IT_WRITE_TEMP-MBLNR,
*           w_matdocumentyear  TO it_write_temp-mjahr,
           'S' TO IT_WRITE_TEMP-ZRESULT,
           ' ' TO IT_WRITE_TEMP-PROCESSED.
    MODIFY IT_WRITE_TEMP TRANSPORTING MBLNR LINECOLOR MESSA
                                      ZRESULT
                                      WHERE MATNR NE SPACE.
    MODIFY IT_ITAB FROM IT_WRITE_TEMP
                         TRANSPORTING ZRESULT PROCESSED
                                      MBLNR MESSA
                         WHERE PROCESSED = 'Y'.
  ENDIF.

  CLEAR : IT_GOODSMVT_ITEM[], IT_GOODSMVT_ITEM.
ENDFORM.                    " gi_posting

*&---------------------------------------------------------------------*
*&      Form  append_write_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPEND_WRITE_TABLE.
  CLEAR : IT_WRITE_TEMP.
  MOVE-CORRESPONDING IT_ITAB TO IT_WRITE_TEMP.
  APPEND IT_WRITE_TEMP.
ENDFORM.                    " append_write_table

*&---------------------------------------------------------------------*
*&      Form  append_bapi_structure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPEND_BAPI_STRUCTURE.
*  DATA:  L_WERKS_1(1).

** changed by Furong on 01/27/09
*  LOOP AT IT_BOM WHERE MATNR = IT_ITAB-MATNR.
  LOOP AT IT_BOM WHERE MATNR = IT_ITAB-MATNR
                   AND SORTF <= IT_ITAB-STATUS.
** End of change
    READ TABLE IT_MARC WITH KEY MATNR = IT_BOM-COMP.

    MOVE : IT_BOM-COMP TO IT_GOODSMVT_ITEM-MATERIAL,
           C_WERKS TO IT_GOODSMVT_ITEM-PLANT,
           IT_MARC-LGPRO TO IT_GOODSMVT_ITEM-STGE_LOC,
           C_BWART_551   TO IT_GOODSMVT_ITEM-MOVE_TYPE,
** Changed by Furong on 03/12/09
*           '0011' TO IT_GOODSMVT_ITEM-MOVE_REAS.
          IT_ITAB-EUSAGE TO IT_GOODSMVT_ITEM-MOVE_REAS.
** End of change

    IT_GOODSMVT_ITEM-ENTRY_QNT = IT_ITAB-SQTY * IT_BOM-QNTY.
    MOVE : P_AUFNR     TO IT_GOODSMVT_ITEM-ORDERID.

    APPEND IT_GOODSMVT_ITEM.
  ENDLOOP.
ENDFORM.                    " append_bapi_structure

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  APPEND_FIELDCAT :
    W_COL_POS 'MATNR' 18 'Material'       'CHAR' 'X' ''      '',
*    W_COL_POS 'LGORT'  4 'Storage Loc'    'CHAR' ''  ''      '',
*    W_COL_POS 'ERFMG' 12 'GI Qty'         'QUAN' ''  'MEINS' '',
*    W_COL_POS 'MEINS'  3 'UoM'            'CHAR' ''  ''      '',
    W_COL_POS 'MBLNR' 12 'Docement No'    'CHAR' ''  ''      '',
    W_COL_POS 'ZRESULT' 4 'Status'      'CHAR' ''  ''      '',
    W_COL_POS 'MESSA' 80 'Message'        'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  APPEND_SORTCAT : '1' 'MATNR' 'IT_WRITE' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  get_error_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_error_message USING p_text.
**---
*  READ TABLE it_mess WITH KEY msgtyp = 'E'.
*
*  IF sy-subrc EQ 0.
*    CLEAR : p_text.
*    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*         EXPORTING
*              msgid               = it_mess-msgid
*              msgnr               = it_mess-msgnr
*              msgv1               = it_mess-msgv1
*              msgv2               = it_mess-msgv2
*              msgv3               = it_mess-msgv3
*              msgv4               = it_mess-msgv4
*         IMPORTING
*              message_text_output = p_text.
*  ENDIF.
*ENDFORM.                    " get_error_message
*
*
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE.
  DATA:L_ZMSG LIKE ZTPPERM-ZMSG.

  LOOP AT IT_ITAB.
    IF IT_ITAB-ZRESULT IS INITIAL.
    ELSE.
*      IF IT_ITAB-ZRESULT = 'S'.
*        CONCATENATE 'SC:' IT_ITAB-MBLNR INTO L_ZMSG SEPARATED BY SPACE.
*      ELSE.
      L_ZMSG = IT_ITAB-MESSA.
*      ENDIF.
      IF P_NEW = 'X'.
*      UPDATE ztpp_osdp SET gi_status = it_itab-gi_status
*                           mblnr = it_itab-mblnr
*                     WHERE model = it_itab-model
*                         AND body_ser = it_itab-body_ser
*                         AND porder = it_itab-porder
*                         AND resb_no = it_itab-resb_no
*                         AND resb_item = it_itab-resb_item
*                         AND comp_no = it_itab-matnr
*                         AND supply_area = it_itab-lgort
*                         AND gi_status = ' '.

        UPDATE ZTPPERM SET ZRESULT = IT_ITAB-ZRESULT
                            ZMSG = L_ZMSG
           WHERE ERPID = 'E51'
                   AND EITEM = IT_ITAB-MATNR
                  AND EASSYID = IT_ITAB-EASSYID
                   AND ZRESULT = 'I'.

      ELSE.
        UPDATE ZTPPERM SET ZRESULT = IT_ITAB-ZRESULT
                              ZMSG = L_ZMSG
           WHERE ERPID = 'E51'
                   AND EITEM = IT_ITAB-MATNR
                  AND EASSYID = IT_ITAB-EASSYID
                   AND ( ZRESULT = 'I' OR ZRESULT = 'E' ).
      ENDIF.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.
ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  GET_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BOM.
  DATA: C_CAPID   LIKE RC29L-CAPID VALUE 'PP01',
           L_DATE LIKE SY-DATUM,
        LT_STB TYPE STPOX OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF LT_COMP OCCURS 0,
        MATNR LIKE ZTPPERM-EITEM,
        LGPRO LIKE MARC-LGPRO,
        END OF LT_COMP.

  REFRESH IT_BOM.
  L_DATE = SY-DATUM.
  LOOP AT IT_MATNR.
    REFRESH LT_STB.
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
        EXPORTING
         CAPID                       = C_CAPID
         DATUV                       = L_DATE
*    EMENG                       = p_emeng
*    MEHRS                       = p_mehrs
*    MMORY                       = p_mmory
         MTNRV                       = IT_MATNR-MATNR
         MKTLS                       = 'X'
         STLAL                       = '01'
         STLAN                       = '1'
*   STPST                       = 0
*   SVWVO                       = 'X'
         WERKS                       = 'E001'
* IMPORTING
*    TOPMAT                     =
*   DSTST                       =
         TABLES
           STB                       = LT_STB
*   MATCAT                      =
      EXCEPTIONS
        ALT_NOT_FOUND               = 1
        CALL_INVALID                = 2
        MATERIAL_NOT_FOUND          = 3
        MISSING_AUTHORIZATION       = 4
        NO_BOM_FOUND                = 5
        NO_PLANT_DATA               = 6
        NO_SUITABLE_BOM_FOUND       = 7
        CONVERSION_ERROR            = 8
        OTHERS                      = 9 .

    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    IT_BOM-MATNR = IT_MATNR-MATNR.
    LOOP AT LT_STB.  " WHERE DATUV <= P_POST AND DATUB > P_POST.
      IT_BOM-COMP = LT_STB-IDNRK.
      IT_BOM-QNTY = LT_STB-MENGE.
      IT_BOM-DATAB = LT_STB-DATUV.
      IT_BOM-DATBI = LT_STB-DATUB.
      IT_BOM-SORTF = LT_STB-SORTF.
*      IT_OUT-UPGVC = LT_STB-UPGN.
      APPEND IT_BOM.
      LT_COMP-MATNR = LT_STB-IDNRK.
      COLLECT LT_COMP.
    ENDLOOP.
    CLEAR: IT_BOM.
  ENDLOOP.

  SELECT MATNR LGPRO INTO TABLE IT_MARC
   FROM MARC
   FOR ALL ENTRIES IN LT_COMP
   WHERE MATNR = LT_COMP-MATNR
     AND WERKS = 'E001'.

  SORT IT_BOM BY MATNR SORTF.
ENDFORM.                    " GET_BOM
*&---------------------------------------------------------------------*
*&      Form  sCRAP_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SCRAP_POSTING.    "Transfer posting
  DATA: G_PSTING_DATE LIKE BAPI2017_GM_HEAD_01,
        IT_ITEM LIKE BAPI2017_GM_ITEM_CREATE OCCURS 0 WITH HEADER LINE,
        LT_RETURN LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE,
        LT_MAT_DOC LIKE BAPI2017_GM_HEAD_RET-MAT_DOC.

  G_PSTING_DATE-PSTNG_DATE = IT_ITAB-PROD_DT.
  G_PSTING_DATE-DOC_DATE = IT_ITAB-PROD_DT.
  G_PSTING_DATE-HEADER_TXT = IT_ITAB-EASSYID.

*move item information
  MOVE IT_ITAB-MATNR     TO IT_ITEM-MATERIAL.
  MOVE IT_ITAB-SQTY      TO IT_ITEM-ENTRY_QNT.
  MOVE C_WERKS           TO IT_ITEM-PLANT.     " source plant

*  MOVE C_ISSUE_STLOC     TO IT_ITEM-STGE_LOC.  " source stl
  SELECT SINGLE LGPRO INTO IT_ITEM-STGE_LOC
    FROM MARC
    WHERE MATNR = IT_ITAB-MATNR
      AND WERKS = 'E001'.

  MOVE C_MTYPE           TO IT_ITEM-MOVE_TYPE. "
  MOVE 'EA'              TO IT_ITEM-ENTRY_UOM.
  MOVE C_WERKS           TO IT_ITEM-MOVE_PLANT.
  MOVE C_RECP_STLOC      TO IT_ITEM-MOVE_STLOC.
  APPEND IT_ITEM.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
     GOODSMVT_HEADER            = G_PSTING_DATE
     GOODSMVT_CODE              = C_GM_CODE_04
*    TESTRUN                     = ' '
    IMPORTING
*    GOODSMVT_HEADRET            =
     MATERIALDOCUMENT           = LT_MAT_DOC
*    MATDOCUMENTYEAR             =
    TABLES
      GOODSMVT_ITEM             = IT_ITEM
*    GOODSMVT_SERIALNUMBER       =
      RETURN                    = LT_RETURN.

  READ TABLE LT_RETURN WITH KEY TYPE = 'E'.

  IF SY-SUBRC EQ 0.
    ROLLBACK WORK.
*    concatenate it_return-message 'hlw' into it_return-message.
    MOVE : C_RED             TO IT_WRITE_TEMP-LINECOLOR,
           LT_RETURN-MESSAGE TO IT_WRITE_TEMP-MESSA,
           'E' TO IT_WRITE_TEMP-ZRESULT,
           ' ' TO IT_WRITE_TEMP-PROCESSED.
    MODIFY IT_WRITE_TEMP TRANSPORTING LINECOLOR MESSA
                                      ZRESULT
                                WHERE MATNR NE SPACE.
    MODIFY IT_ITAB FROM IT_WRITE_TEMP
                          TRANSPORTING ZRESULT PROCESSED MESSA
                          WHERE PROCESSED = 'Y'.

  ELSE.
    COMMIT WORK AND WAIT.
    MOVE : C_GREEN            TO IT_WRITE_TEMP-LINECOLOR.
*           LT_RETURN-MESSAGE  TO IT_WRITE_TEMP-MESSA,
    CONCATENATE 'TS:'  W_MATERIALDOCUMENT  INTO
                 IT_WRITE_TEMP-MESSA SEPARATED BY SPACE.
    MOVE:   LT_MAT_DOC  TO IT_WRITE_TEMP-MBLNR,
*           w_matdocumentyear  TO it_write_temp-mjahr,
           'S' TO IT_WRITE_TEMP-ZRESULT,
           ' ' TO IT_WRITE_TEMP-PROCESSED.
    MODIFY IT_WRITE_TEMP TRANSPORTING MBLNR LINECOLOR MESSA
                                      ZRESULT
                                      WHERE MATNR NE SPACE.
    MODIFY IT_ITAB FROM IT_WRITE_TEMP
                         TRANSPORTING ZRESULT PROCESSED
                                      MBLNR MESSA
                         WHERE PROCESSED = 'Y'.
  ENDIF.

ENDFORM.                    " SCRAP_POSTING
