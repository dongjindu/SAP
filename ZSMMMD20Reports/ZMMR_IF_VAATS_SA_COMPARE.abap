************************************************************************
* Program Name      : ZMMR_IF_VAATS_SA_COMPARE
* Author            : Furong Wang
* Creation Date     : 08/2008
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Overview Analysis Report(Vaats, Info Record & SA)
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************


REPORT ZMMR_IF_VAATS_SA_COMPARE NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64(1)
                             MESSAGE-ID ZMMM.
**---
*INCLUDE : ZRMMPMXXR_INCL.
TYPE-POOLS : SLIS.
TABLES: EKKO, EKPO,EINA, A018.
**--- Internal Tables
DATA: IT_VAATS LIKE TABLE OF ZTMM_IF_PRICE WITH HEADER LINE.

DATA: BEGIN OF IT_OUTTAB OCCURS 0,
       MATNR LIKE MARA-MATNR,    " Material master
       MAT_CLASS(3),
       MAKTX LIKE MAKT-MAKTX,
       WERKS LIKE MARC-WERKS,
       BKLAS  LIKE MBEW-BKLAS,
       MATKL LIKE MARA-MATKL,
       MSTAE LIKE MARA-MSTAE,
       STPRS LIKE MBEW-STPRS,
       LIFNR LIKE ZTMM_IF_PRICE-LIFNR,
       GRQTY LIKE EKBE-MENGE,
       INTF_D LIKE ZTMM_IF_PRICE-INTF_D,      " Vaats
       APP_D LIKE ZTMM_IF_PRICE-APP_D,
       PRICE LIKE ZTMM_IF_PRICE-PRICE,
       RSN LIKE ZTMM_IF_PRICE-RESN_C,
       APPROVAL  LIKE ZTMM_IF_PRICE-PUM_N,
       INF_PER LIKE ZTMM_IF_PRICE-PRICE,
       INFO_PRICE LIKE ZTMM_IF_PRICE-PRICE,
       INFO_FROM LIKE SY-DATUM,
       INFO_TO LIKE SY-DATUM,
       INFO_CR_BY LIKE EINA-ERNAM,
       INFO_CR_ON LIKE EINA-ERDAT,
       SA_PRICE LIKE ZTMM_IF_PRICE-PRICE,
       SA_FROM LIKE SY-DATUM,
       SA_TO LIKE SY-DATUM,
       SA_CR_BY LIKE EINA-ERNAM,
       SA_CR_ON LIKE EINA-ERDAT,
       EBELN LIKE EINE-EBELN,
       REMARKS(255),
       SA_I2_ERROR(2),
       CT TYPE LVC_T_SCOL,
      END OF IT_OUTTAB.

DATA : BEGIN OF IT_INFO_ITEM OCCURS 0,
         KAPPL LIKE KONP-KAPPL,
         KSCHL LIKE KONP-KSCHL,
         KBETR LIKE KONP-KBETR,
         KPEIN LIKE KONP-KPEIN,
         KONWA LIKE KONP-KONWA,
         LIFNR LIKE KONP-LIFNR,
         ERDAT LIKE KONH-ERDAT,
         ERNAM LIKE KONH-ERNAM,
         KZUST LIKE KONH-KZUST,
         DATAB LIKE KONH-DATAB,
         DATBI LIKE KONH-DATBI,
       END OF IT_INFO_ITEM.

DATA : BEGIN OF IT_SA,                                      " OCCURS 0,
         EBELN LIKE EKPO-EBELN,
         EBELP LIKE EKPO-EBELP,
         LIFNR LIKE EKKO-LIFNR,
         MATNR LIKE EKPO-MATNR,
         WERKS LIKE EKPO-WERKS,
         LGORT LIKE EKPO-LGORT,
         ETFZ1 LIKE EKPO-ETFZ1,
         BSTYP LIKE EKKO-BSTYP,
         BUKRS LIKE EKKO-BUKRS,
         BSART LIKE EKKO-BSART,
         EKORG LIKE EKKO-EKORG,
         EKGRP LIKE EKKO-EKGRP,
         KDATB LIKE EKKO-KDATB,
         KDATE LIKE EKKO-KDATE,
         AEDAT LIKE EKKO-AEDAT,
         ERNAM LIKE EKKO-ERNAM,
       END OF IT_SA.

DATA : BEGIN OF IT_SACOND_ITEM OCCURS 0,
         KAPPL LIKE KONP-KAPPL,
         KSCHL LIKE KONP-KSCHL,
         KBETR LIKE KONP-KBETR,
         KPEIN LIKE KONP-KPEIN,
         KONWA LIKE KONP-KONWA,
         LIFNR LIKE KONP-LIFNR,
         KZUST LIKE KONH-KZUST,
         DATAB LIKE KONH-DATAB,
         DATBI LIKE KONH-DATBI,
         AEDAT LIKE EKKO-AEDAT,
         ERNAM LIKE EKKO-ERNAM,
       END OF IT_SACOND_ITEM.

DATA: IT_COLOR TYPE LVC_T_SCOL,
       WA_COLOR LIKE LINE OF IT_COLOR.

DATA: BEGIN OF IT_MARA OCCURS 0,
       MATNR LIKE MARA-MATNR,    " Material master
       MAKTX LIKE MAKT-MAKTX,
       WERKS LIKE MARC-WERKS,
       BKLAS  LIKE MBEW-BKLAS,
       MATKL LIKE MARA-MATKL,
       MSTAE LIKE MARA-MSTAE,
       STPRS LIKE MBEW-STPRS,
       PSTAT LIKE MARA-PSTAT,
*       mtart like mara-mtart,
      END OF IT_MARA.
*DATA : IT_SACOND_ITEM LIKE IT_INFO_ITEM OCCURS 0 WITH HEADER LINE.

****
DATA : BEGIN OF IT_TEMP OCCURS 0,
         LIFNR LIKE EKKO-LIFNR,
         MATNR LIKE EKPO-MATNR,
         INTF_D LIKE SY-DATUM,
         KSCHL LIKE KONP-KSCHL,
         KBETR_SA LIKE KONP-KBETR,
         DATAB_SA LIKE A018-DATAB,
         DATBI_SA LIKE A018-DATBI,
         DATAB LIKE A018-DATAB,
         DATBI LIKE A018-DATBI,
         KBETR LIKE KONP-KBETR,
         KPEIN LIKE KONP-KPEIN,
         REMARKS(255),
         DELETED(1),
       END OF IT_TEMP.

DATA : IT_ITAB LIKE IT_TEMP OCCURS 0 WITH HEADER LINE.

DATA: IT_COST1 LIKE TABLE OF ZTMM_ASSY_COST1 WITH HEADER LINE.
*DATA: IT_MAIL TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
*                  WITH HEADER LINE.

DATA: IT_T683S LIKE TABLE OF T683S WITH HEADER LINE.

DATA: BEGIN OF IT_MODULE OCCURS 0,
        INDICATOR,                              "Sub Material Status
        VTYPE     LIKE   ZTMM_ASSY_COST1-VTYPE, "Vehicle type
        NAME1     LIKE   LFA1-NAME1,            "Vendor name
        LIFNR     LIKE   LFA1-LIFNR,            "Vendor
        MAKTX     LIKE   MAKT-MAKTX,            "Description
        MATNR     LIKE   MARA-MATNR,            "Material
        DATAB     LIKE   ZTMM_ASSY_COST1-DATAB, "Valid on
        DATBI     LIKE   ZTMM_ASSY_COST1-DATBI, "Valid to
        MOAMT     LIKE   MSEG-DMBTR,            "Module Price
        ASYTR     LIKE   MSEG-DMBTR,            "Module Assy Cost
        DMBTR     LIKE   MSEG-DMBTR,            "Material Cost
        DMAMT     TYPE   F,                     "Material Cost(Floating)
        WAERS     LIKE   T001-WAERS,            "Currency
        EKGRP     LIKE   EKKO-EKGRP,            "Purchasing Group
        NETPR     LIKE   EKPO-NETPR,            "Component Amount
        PEINH     LIKE   EKPO-PEINH,            "Component Price Unit
        MEINS     LIKE   EKPO-MEINS,            "UoM
        MCODE     LIKE   ZTMM_ASSY_COST1-MCODE, "Module code
        KZUST     LIKE   KONH-KZUST.            "Reason code
        INCLUDE STRUCTURE ZSMM_CUSTOM_CONDITION.

DATA:   ZTIR     LIKE   EKPO-NETPR,
        STS       TYPE   I,                     "Module Info Status
        MSG(255),
        CHBOX,
      END   OF IT_MODULE.

DATA : BEGIN OF IT_TAB OCCURS 0,
       VTYPE  LIKE   ZTMM_ASSY_COST1-VTYPE,
       LIFNR  LIKE   LFA1-LIFNR,
       MATNR  LIKE   MARA-MATNR,
       ASYTR  LIKE MSEG-DMBTR,
       DATAB LIKE   ZTMM_ASSY_COST1-DATAB,
       DATBI LIKE   ZTMM_ASSY_COST1-DATBI,
       MAKTX  LIKE   MAKT-MAKTX,
       NAME1 LIKE   LFA1-NAME1,
       UPGVC LIKE MARA-MATNR,
       PREF LIKE STPO-POSNR, "ztbm_abxduldt-pref,
       COMP  LIKE MARA-MATNR,
       MAKTX1 LIKE  MAKT-MAKTX ,
       QNTY LIKE STPO-MENGE,
       UNIT LIKE   MARA-MEINS,
       MEINS LIKE   MARA-MEINS,
       EKGRP LIKE   EKKO-EKGRP,
       MCODE LIKE ZTMM_ASSY_COST1-MCODE,
       DATAB1 LIKE   ZTMM_ASSY_COST1-DATAB ,
       DATBI1 LIKE   ZTMM_ASSY_COST1-DATBI ,
       STGB LIKE STPO-STGB,
      END OF IT_TAB.

DATA : BEGIN OF IT_COMP OCCURS 0,
        MATNR LIKE MARA-MATNR,
        UPGVC LIKE MARA-MATNR,
       END OF IT_COMP .

*data : begin of it_scomp occurs 0.
*        include STRUCTURE ZTBM_ABXDULDT.
*data:   maktx     LIKE   makt-maktx,
*       end of it_scomp.

DATA: BEGIN OF IT_SUB OCCURS 0,
        VTYPE     LIKE   ZTMM_ASSY_COST1-VTYPE,   "Vehicle type
        MATNR     LIKE   ZTMM_ASSY_COST1-VTYPE,   "Material
        UPGVC     LIKE   MARA-MATNR,              "UPG-VC
        PREF      LIKE   STPO-POSNR,
        COMP      LIKE   MARA-MATNR,
        MAKTX     LIKE   MAKT-MAKTX,              "Description
        LIFNR     LIKE   LFA1-LIFNR,              "Vendor
        AMOUNT    TYPE   F,                       "Component Amount
        QNTY      LIKE   STPO-MENGE,
        STGB      LIKE   STPO-STGB,
        UNIT      LIKE   MARA-MEINS,
        MEINS     LIKE   MARA-MEINS,              "UoM(sub)
        KMEIN     LIKE   KONP-KMEIN,              "UoM(Info)
        DATAB     LIKE   SY-DATUM,                "Valid on(sub)
        DATBI     LIKE   SY-DATUM,                "Valid to(Sub)
        NETPR     LIKE   EKPO-NETPR,              "Component Amount
        PEINH     LIKE   EKPO-PEINH,              "Component Price Unit
        WAERS     LIKE   T001-WAERS,              "Currency
        KZUST     LIKE   KONH-KZUST.              "Reason code
        INCLUDE STRUCTURE ZSMM_CUSTOM_CONDITION.
DATA:   ZTIR      LIKE   EKPO-NETPR,
        STS,                                      "Status
        MSG(255),                                  "Message
      END   OF IT_SUB.

DATA : BEGIN OF IT_OUT OCCURS 0,
            VTYPE LIKE ZTMM_ASSY_COST1-VTYPE,
            LIFNR LIKE LFA1-LIFNR,
            MATNR LIKE MARA-MATNR,
            ASYTR LIKE MSEG-DMBTR,
            DATAB LIKE ZTMM_ASSY_COST1-DATAB,
            DATBI LIKE ZTMM_ASSY_COST1-DATBI,
            MAKTX LIKE MAKT-MAKTX,
            NAME1 LIKE LFA1-NAME1,
            UPGVC LIKE MARA-MATNR,
            PREF  LIKE STPO-POSNR,  "ZTBM_ABXDULDT-pref
            COMP  LIKE MARA-MATNR,
            CMAKTX LIKE MAKT-MAKTX,
            QNTY LIKE STPO-MENGE,
            UNIT LIKE MARA-MEINS,
            MEINS     LIKE   MARA-MEINS,
            EKGRP LIKE ZTMM_ASSY_COST1-EKGRP,
            MCODE LIKE ZTMM_ASSY_COST1-MCODE,
            CDATAB LIKE ZTMM_ASSY_COST1-DATAB,
            CDATBI LIKE ZTMM_ASSY_COST1-DATBI,
            STGB   LIKE STPO-STGB,     "******
        END OF IT_OUT.

DATA: IT_MOD_BOM LIKE TABLE OF STPOV WITH HEADER LINE.

DATA: IT_CONDITION LIKE ZTMM_ASSY_COST2 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_MOD_ERR OCCURS 0,
        MATNR LIKE MARA-MATNR,
        REMARKS(50),
        GRQTY LIKE EKBE-MENGE,
      END OF IT_MOD_ERR.

DATA:  C_KSCHL   LIKE   KONP-KSCHL    VALUE   'PB00',
      C_ZTIR    LIKE   KONP-KSCHL    VALUE   'ZTIR',
      C_BUKRS   LIKE   T001-BUKRS    VALUE   'H201',
      C_READY   TYPE   I             VALUE   1,
      C_WARNING TYPE   I             VALUE   2,
      C_SUCCESS TYPE   I             VALUE   3,
      C_ERROR   TYPE   I             VALUE   4,
      C_INCORRECT TYPE I             VALUE   5,
      C_NEW     TYPE   I             VALUE   1,"Module Status
      C_DELETED TYPE   I             VALUE   2,"Module Status
      C_NO_COND TYPE   I             VALUE   3,"Condition does not exist
      C_DEL_CON TYPE   I             VALUE   4,"Condition was deleted
      C_NO_INFO TYPE   I             VALUE   5,"InfoRecord dosn't exist
      C_UOM_ERR TYPE   I             VALUE   6,"Incorrect UoM
      C_NO_MATL TYPE   I             VALUE   7,"M/M does not exist.
      C_EXIST   TYPE   I             VALUE   8,"Info Record is exist.
      C_SUB_ERR TYPE   I             VALUE   9,
      C_DIFF    TYPE   I             VALUE  10.

DATA: W_INDEX(2)   TYPE   N,
      W_SUB(50),
      W_MODULE(50).

DATA: W_TASKNAME(4) TYPE N VALUE '0001',
      W_EXCEP_FLAG TYPE C,
      W_SND_JOBS TYPE I VALUE 1,
      W_RCV_JOBS TYPE I VALUE 1.


TYPE-POOLS: VRM.
DATA: NAME  TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST.

FIELD-SYMBOLS: <MODULE>, <SUB>.

*----- Global variables & structures
DATA: WA_MODULE LIKE IT_MODULE.
DATA: WA_SUB LIKE IT_SUB.


** wokking variant
DATA : W_SUBRC LIKE SY-SUBRC.

DATA : W_KNUMH LIKE KONH-KNUMH.

** for screen display

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_FI  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_CO  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE,
       IT_FIELDCAT_DET TYPE LVC_T_FCAT WITH HEADER LINE. "/Detail

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
      W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I.

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

DEFINE APPEND_TOP.
  CLEAR : W_LINE.
  IF NOT &3 IS INITIAL OR NOT &4 IS INITIAL.
    W_LINE-TYP   = &1.
    W_LINE-KEY   = &2.
    CONCATENATE &3 '~' &4 INTO W_LINE-INFO SEPARATED BY SPACE.
    APPEND W_LINE TO W_TOP_OF_PAGE.
  ENDIF.
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
SELECT-OPTIONS : S_LIFNR FOR EKKO-LIFNR.
SELECT-OPTIONS : S_MATNR FOR EKPO-MATNR NO-EXTENSION .
PARAMETERS : P_WERKS LIKE EKPO-WERKS DEFAULT 'P001' NO-DISPLAY.
PARAMETERS : P_DATE LIKE EKKO-AEDAT DEFAULT SY-DATUM OBLIGATORY,

P_RSNC(2) TYPE C AS LISTBOX VISIBLE LENGTH 40.
*SELECTION-SCREEN SKIP.
*PARAMETERS: P_PR RADIOBUTTON GROUP GRP.
*PARAMETERS: P_PRDT RADIOBUTTON GROUP GRP DEFAULT 'X'.
*SELECTION-SCREEN SKIP.
*PARAMETERS : p_bdc LIKE ekko-aedat DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK1.
SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS : S_BUDAT FOR EKKO-AEDAT OBLIGATORY NO-EXTENSION
                             DEFAULT SY-DATUM.
SELECTION-SCREEN END OF BLOCK BLOCK2.
*SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-004.
*PARAMETERS: P_SEND AS CHECKBOX DEFAULT 'X' USER-COMMAND UCOM.
*PARAMETERS: P_EMAIL(40) DEFAULT 'SAIFVAL' MODIF ID MD3.
*SELECTION-SCREEN END OF BLOCK BLOCK2.
*
**---
INITIALIZATION.
*  PERFORM EVENT_BUILD USING W_EVENTCAT[].

AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_LISTBOX_RSNCODE.

*TOP-OF-PAGE.
*  PERFORM TOP_OF_PAGE.

**---
START-OF-SELECTION.
  PERFORM GET_VAATS_DATA.

*  IF IT_VAATS[] IS INITIAL.
*    MESSAGE S999 WITH TEXT-M01.
*  ELSE.
  PERFORM GET_MODEL.
  IF IT_OUTTAB[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    PERFORM GET_COND_PRICE.
    PERFORM COMPARE_CONDITIONS.
    PERFORM DISPLAY_DATA.

*    PERFORM COMMENT_BUILD.     " USING w_top_of_page[].
*    PERFORM MAKE_ALV_GRID.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  change_conditions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMPARE_CONDITIONS.
  LOOP AT IT_OUTTAB.
    PERFORM GET_INFO_ITEM_CONDITION.
    PERFORM GET_SA_ITEM.
    PERFORM FILL_TABLE.
  ENDLOOP.
  PERFORM CHECK_MODULE_ERROR.
  PERFORM FILTER_BY_ERROR_CODE.
*  IF P_SEND = 'X'.
*    IF IT_ITAB[] IS INITIAL.
*    ELSE.
*      PERFORM SEND_EMAIL.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " change_conditions

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM COMMENT_BUILD.
**---
*  CLEAR : W_LINE, W_TOP_OF_PAGE, W_TOP_OF_PAGE[].
*  W_LINE-TYP  = 'H'.
*  W_LINE-INFO = TEXT-002.
*  APPEND W_LINE TO W_TOP_OF_PAGE.
*
**  CLEAR : w_line.
**  APPEND INITIAL LINE TO w_top_of_page.
*ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM MAKE_ALV_GRID.
**---
*  CLEAR: W_FIELDCAT, W_FIELDCAT[], W_SORTCAT, W_SORTCAT[].
*  MOVE : 'LINECOLOR' TO W_LAYOUT-INFO_FIELDNAME,
*         'X'         TO W_LAYOUT-COLWIDTH_OPTIMIZE.
*
*  PERFORM BUILD_FIELDCAT.
*  PERFORM BUILD_SORTCAT.
*
*  CLEAR : W_PROGRAM.
*
*  MOVE : SY-REPID TO W_PROGRAM.
*
*  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*       EXPORTING
*            I_CALLBACK_PROGRAM = W_PROGRAM
*            IS_LAYOUT          = W_LAYOUT
*            IT_FIELDCAT        = W_FIELDCAT[]
*            IT_EVENTS          = W_EVENTCAT[]
*            IT_SORT            = W_SORTCAT[]
*            I_SAVE             = 'A'
*       TABLES
*            T_OUTTAB           = IT_ITAB
*       EXCEPTIONS
*            PROGRAM_ERROR      = 1
*            OTHERS             = 2.
*ENDFORM.                    " make_alv_grid
*
*&---------------------------------------------------------------------*
*&      Form  get_scheduling_agreement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VAATS_DATA.
  DATA: LT_VAATS LIKE TABLE OF ZTMM_IF_PRICE WITH HEADER LINE.

  CLEAR: IT_VAATS[].
  IF S_LIFNR[] IS INITIAL.
    SELECT * INTO TABLE IT_VAATS
     FROM ZTMM_IF_PRICE
*    where not zresult EQ 'S'.
     WHERE MATNR IN S_MATNR
       AND NOT LIFNR LIKE 'S%'   "S_LIFNR
             AND INTF_D = P_DATE.

  ELSE.
    SELECT * INTO TABLE IT_VAATS
      FROM ZTMM_IF_PRICE
*    where not zresult EQ 'S'.
      WHERE MATNR IN S_MATNR
        AND LIFNR IN S_LIFNR
              AND INTF_D = P_DATE.
  ENDIF.
  SORT IT_VAATS BY MATNR LIFNR INTF_D ZSEQ DESCENDING APP_D.
  DELETE ADJACENT DUPLICATES FROM IT_VAATS COMPARING MATNR LIFNR INTF_D.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM BUILD_FIELDCAT.
***--- &1 : position       &2 : field name       &3 : field length
***--- &4 : description    &5 : field type       &6 : key
***--- &7 : qty field      &8 : color
*  APPEND_FIELDCAT :
*    W_COL_POS 'MATNR' 18 'Material'       'CHAR' ''  ''      '',
*    W_COL_POS 'LIFNR' 04 'Vendor'         'CHAR' ''  ''      '',
*    W_COL_POS 'INTF_D' 12 'Interface Date'  'DATS' ''  ''      '',
*    W_COL_POS 'DATAB_SA' 12 'Vesting Date'     'DATS' ''  ''      '',
*    W_COL_POS 'KBETR_SA' 14 'Vaats Amount'      'CURR' ''  ''      '',
*    W_COL_POS 'DATAB' 10 'Info From'      'DATS' ''  ''      '',
*    W_COL_POS 'DATBI' 10 'Info To'        'DATS' ''  ''      '',
*    W_COL_POS 'KBETR' 14 'Inf0 Amount'         'CURR' ''  ''      '',
*    W_COL_POS 'REMARKS' 40 'Reasons'        'CHAR' ''  ''      '',
*    W_COL_POS 'DELETED' 11 'Inf Dltd'        'CHAR' ''  ''      ''.
**    w_col_pos 'KPEIN' 4  'Un/p'           'DEC' ''  ''      ''.

*ENDFORM.                    " build_fieldcat

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
*  append_sortcat : '1' 'EBELN' 'IT_ITAB' 'X' '',
*                   '2' 'EBELP' 'IT_ITAB' 'X' '',
*                   '3' 'MATNR' 'IT_ITAB' 'X' '',
*                   '4' 'WERKS' 'IT_ITAB' 'X' '',
*                   '5' 'LGORT' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*---------------------------------------------------------------------*
*       FORM fill_table                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FILL_TABLE.
*  DATA: L_DIFF(1),
  DATA: L_LOEKZ(1),
        L_BEDAT LIKE EIPA-BEDAT,
        L_PREIS LIKE EIPA-PREIS,
         L_ABN_PRICE LIKE ZTMM_IF_PRICE-PRICE.

  DATA: LT_VAATS LIKE TABLE OF ZTMM_IF_PRICE WITH HEADER LINE.
*
*  CLEAR: IT_ITAB.
  SELECT SINGLE LOEKZ ERNAM BEDAT PREIS
                         INTO (L_LOEKZ, IT_OUTTAB-INFO_CR_BY, L_BEDAT,
                               L_PREIS)
                         FROM EINA AS A
                         INNER JOIN EIPA AS B
                         ON A~INFNR = B~INFNR
                        WHERE LIFNR EQ IT_OUTTAB-LIFNR
                          AND MATNR EQ IT_OUTTAB-MATNR.

  IF SY-SUBRC = 0 AND IT_OUTTAB-APP_D = L_BEDAT.
    IT_OUTTAB-INF_PER = ( ( IT_OUTTAB-PRICE - L_PREIS ) / L_PREIS )
                        * 100.
  ENDIF.
*  IT_ITAB-DELETED = L_LOEKZ.
*
  READ TABLE IT_INFO_ITEM WITH KEY KSCHL = 'PB00'.
  IF SY-SUBRC = 0.
*       IT_outtab-KSCHL = 'PB00'.
    MOVE: IT_INFO_ITEM-DATAB TO IT_OUTTAB-INFO_FROM,
          IT_INFO_ITEM-DATBI TO IT_OUTTAB-INFO_TO,
          IT_INFO_ITEM-KBETR TO IT_OUTTAB-INFO_PRICE,
          IT_INFO_ITEM-ERDAT TO IT_OUTTAB-INFO_CR_ON,
          IT_INFO_ITEM-ERNAM TO IT_OUTTAB-INFO_CR_BY.
    IF IT_OUTTAB-INFO_PRICE <> IT_OUTTAB-PRICE AND IT_OUTTAB-PRICE > 0.
*      CONCATENATE IT_OUTTAB-REMARKS 'V4'   " 'Price is not same'
     CONCATENATE IT_OUTTAB-REMARKS 'Vattz price vs Info Price mismatch'
*    "V4'
                    INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
    ENDIF.
  ELSE.
    SELECT SINGLE LOEKZ INTO L_LOEKZ
                           FROM EINA
                          WHERE LIFNR EQ IT_OUTTAB-LIFNR
                            AND MATNR EQ IT_OUTTAB-MATNR
                            AND LOEKZ = ' '.
    IF SY-SUBRC = 0.
*      CONCATENATE IT_OUTTAB-REMARKS  'V3'
*                                "   'Validation period does not match'
      CONCATENATE IT_OUTTAB-REMARKS
           'Vattz vs Info Validity period mismatch'
*        "'V3'
                  INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
    ELSE.
*      CONCATENATE 'V1' IT_OUTTAB-REMARKS       "'No Info Record found'
      CONCATENATE  IT_OUTTAB-REMARKS  'No Info Record Found'" 'V1'
                INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
    ENDIF.
  ENDIF.
  IF IT_OUTTAB-INFO_CR_ON > IT_OUTTAB-INFO_FROM.
*    CONCATENATE IT_OUTTAB-REMARKS 'I1'
*                                " create date earlier than Info FROM'
    CONCATENATE  IT_OUTTAB-REMARKS
               'Info Cr date later than valid from D'
*                                "'I1'
               INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
  ENDIF.
*  IF IT_OUTTAB-INFO_CR_ON > IT_OUTTAB-INFO_TO.
*    CONCATENATE IT_OUTTAB-REMARKS 'I1'
**                                " create date later than Info TO'
*               INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
*  ENDIF.

  IF IT_OUTTAB-MAT_CLASS = 'Mod'.
*    PERFORM CHECK_ERROR_FOR_MOD.
  ELSE.
    CLEAR: LT_VAATS, LT_VAATS[].

    SELECT * INTO TABLE LT_VAATS
      FROM ZTMM_IF_PRICE
      WHERE MATNR = IT_OUTTAB-MATNR
        AND LIFNR = IT_OUTTAB-LIFNR
              AND INTF_D <= P_DATE.

    SORT LT_VAATS DESCENDING BY APP_D.
    READ TABLE LT_VAATS INDEX 2.
    IF LT_VAATS-PRICE > 0.
      L_ABN_PRICE = ( ( IT_OUTTAB-PRICE - LT_VAATS-PRICE ) /
                          LT_VAATS-PRICE ) * 100.
      L_ABN_PRICE = ABS( L_ABN_PRICE ).
      IT_OUTTAB-INF_PER = L_ABN_PRICE.
      IF L_ABN_PRICE > 25.
*        CONCATENATE IT_OUTTAB-REMARKS 'V5'
*                        price variance > 5'
        CONCATENATE IT_OUTTAB-REMARKS 'Price variance > 25'  "'V5'
                   INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.

      ENDIF.
    ENDIF.
  ENDIF.
  WA_COLOR-COLOR-COL = 5.
  WA_COLOR-COLOR-INT = 1.
  WA_COLOR-FNAME = 'PRICE'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'INTF_D'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'APP_D'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'RSN'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'APPROVAL'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'INF_PER'.
  APPEND WA_COLOR TO IT_COLOR.

  WA_COLOR-COLOR-COL = 2.
  WA_COLOR-COLOR-INT = 1.
  WA_COLOR-FNAME = 'INFO_PRICE'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'INFO_FROM'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'INFO_TO'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'INFO_CR_ON'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'INFO_CR_BY'.
  APPEND WA_COLOR TO IT_COLOR.

** SA
  READ TABLE IT_SACOND_ITEM WITH KEY KSCHL = 'PB00'.
  IF SY-SUBRC EQ 0.
    IT_OUTTAB-SA_FROM = IT_SACOND_ITEM-DATAB.
    IT_OUTTAB-SA_TO = IT_SACOND_ITEM-DATBI.
    IT_OUTTAB-SA_PRICE = IT_SACOND_ITEM-KBETR.
*     IT_SACOND_ITEM-KPEIN TO IT_outtab-KPEIN_SA.
*     IT_SACOND_ITEM-KPEIN = IT_INFO_ITEM-KPEIN
    IT_OUTTAB-SA_CR_ON = IT_SACOND_ITEM-AEDAT.
    IT_OUTTAB-SA_CR_BY = IT_SACOND_ITEM-ERNAM.
    IF IT_OUTTAB-SA_PRICE <> IT_OUTTAB-INFO_PRICE.
*      CONCATENATE IT_OUTTAB-REMARKS 'I4'
*                        SA price differs from Info price'
      CONCATENATE IT_OUTTAB-REMARKS 'Info price vs SA price mismatch'
* "'I4'
                  INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
    ENDIF.
    IF IT_OUTTAB-SA_FROM <> IT_OUTTAB-INFO_FROM
      AND IT_OUTTAB-SA_FROM <> IT_OUTTAB-SA_CR_ON.
*      CONCATENATE IT_OUTTAB-REMARKS 'I3'
*                        Valid FROM differs SA and Inforec'
      CONCATENATE IT_OUTTAB-REMARKS
                 'Info vs SA validity period mismatch'
*      " 'I3'
                  INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
    ENDIF.
    IF IT_OUTTAB-SA_TO <> IT_OUTTAB-INFO_TO.
*      CONCATENATE IT_OUTTAB-REMARKS 'I3'
*                        Valid TO differs SA and Inforec'
      CONCATENATE IT_OUTTAB-REMARKS
         'Info vs SA validity period mismatch'
*      " 'I3'
                  INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
    ENDIF.
  ELSE.
    IF IT_OUTTAB-MSTAE <> '11'.
      IF IT_OUTTAB-EBELN IS INITIAL.
      ELSE.
*        CONCATENATE IT_OUTTAB-REMARKS 'I5'
*                        No SA Condition Record found'
        CONCATENATE IT_OUTTAB-REMARKS 'SA validity period is Expired'
*       " 'I5'
        INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
      ENDIF.
    ENDIF.
  ENDIF.

  WA_COLOR-COLOR-COL = 4.
  WA_COLOR-COLOR-INT = 1.
  WA_COLOR-FNAME = 'SA_PRICE'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'SA_FROM'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'SA_TO'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'SA_CR_ON'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'SA_CR_BY'.
  APPEND WA_COLOR TO IT_COLOR.
  WA_COLOR-FNAME = 'EBELN'.
  APPEND WA_COLOR TO IT_COLOR.
  IT_OUTTAB-CT = IT_COLOR.
  CLEAR: WA_COLOR, IT_COLOR, IT_COLOR[].

  MODIFY IT_OUTTAB.
  CLEAR: IT_OUTTAB.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_info_item_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  GET_INFO_ITEM_CONDITION.

  DATA: L_DATAB LIKE A018-DATAB,
        L_DATBI LIKE A018-DATBI.

  CLEAR : IT_INFO_ITEM, IT_INFO_ITEM[].
  CLEAR : W_KNUMH.

  IF IT_OUTTAB-LIFNR IS INITIAL.
    SELECT SINGLE KNUMH DATAB DATBI LIFNR INTO (W_KNUMH, L_DATAB,
                                          L_DATBI, IT_OUTTAB-LIFNR)
                      FROM A018
                     WHERE KAPPL EQ 'M'
                       AND KSCHL EQ 'PB00'
*                       AND LIFNR IN S_LIFNR
                       AND NOT LIFNR LIKE 'S%'    "S_LIFNR
                       AND MATNR EQ IT_OUTTAB-MATNR
                       AND DATAB <= P_DATE
                       AND DATBI >= P_DATE.
    IF SY-SUBRC <> 0.
      SELECT SINGLE KNUMH DATAB DATBI LIFNR INTO (W_KNUMH, L_DATAB,
                                            L_DATBI, IT_OUTTAB-LIFNR)
                          FROM A017
                         WHERE KAPPL EQ 'M'
                           AND KSCHL EQ 'PB00'
*                           AND LIFNR IN S_LIFNR
                           AND NOT LIFNR LIKE 'S%'    "S_LIFNR
                           AND MATNR EQ IT_OUTTAB-MATNR
                           AND DATAB <= P_DATE
                           AND DATBI >= P_DATE.
    ENDIF.
  ELSE.
    SELECT SINGLE KNUMH DATAB DATBI INTO (W_KNUMH, L_DATAB, L_DATBI)
                       FROM A018
                      WHERE KAPPL EQ 'M'
                        AND KSCHL EQ 'PB00'
                        AND LIFNR EQ IT_OUTTAB-LIFNR
                        AND MATNR EQ IT_OUTTAB-MATNR
*                        AND DATAB = IT_OUTTAB-APP_D.
                       AND DATAB <= IT_OUTTAB-APP_D
                       AND DATBI >= IT_OUTTAB-APP_D.
    IF SY-SUBRC <> 0.
      SELECT SINGLE KNUMH DATAB DATBI INTO (W_KNUMH, L_DATAB, L_DATBI)
                          FROM A017
                         WHERE KAPPL EQ 'M'
                           AND KSCHL EQ 'PB00'
                           AND LIFNR EQ IT_OUTTAB-LIFNR
                           AND MATNR EQ IT_OUTTAB-MATNR
*                           AND DATAB = IT_OUTTAB-APP_D.
                           AND DATAB <= IT_OUTTAB-APP_D
                           AND DATBI >= IT_OUTTAB-APP_D.

    ENDIF.

  ENDIF.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_INFO_ITEM
*           FROM KONP
*          WHERE KNUMH EQ W_KNUMH
*            AND LOEVM_KO EQ SPACE
*            AND KBETR > 0.

  SELECT B~KAPPL B~KSCHL KBETR KPEIN KONWA LIFNR ERDAT ERNAM
*                 " KZUST DATAB DATBI
     INTO CORRESPONDING FIELDS OF TABLE IT_INFO_ITEM
           FROM KONH AS A
           INNER JOIN KONP AS B
           ON A~KNUMH = B~KNUMH
          WHERE A~KNUMH EQ W_KNUMH
            AND LOEVM_KO EQ SPACE
            AND KBETR > 0.

  LOOP AT IT_INFO_ITEM.
    READ TABLE IT_T683S WITH KEY
                      KSCHL = IT_INFO_ITEM-KSCHL
                      KALSM = 'RM0002'.
    IF IT_T683S-KSTAT = ' ' OR
        ( IT_T683S-KSTAT = 'X' AND IT_T683S-KVSL1 <> ' ' ).
      IT_INFO_ITEM-DATAB = L_DATAB.
      IT_INFO_ITEM-DATBI = L_DATBI.
      MODIFY IT_INFO_ITEM.
      CONTINUE.
    ELSE.
      DELETE IT_INFO_ITEM.
    ENDIF.
  ENDLOOP.
  SORT IT_INFO_ITEM BY KSCHL DESCENDING.
ENDFORM.                    " get_info_item_condition

*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SEND_EMAIL.
*
*  DATA: L_SUBJECT(40) TYPE C VALUE 'Anaysis Report'.
*
*  DATA:   IT_PACKING_LIST LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
*          IT_CONTENTS LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
*          IT_RECEIVERS LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
*          IT_ATTACHMENT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
*          GD_CNT TYPE I,
*          GD_SENT_ALL(1) TYPE C,
*          GD_DOC_DATA LIKE SODOCCHGI1,
*          GD_ERROR TYPE SY-SUBRC.
*
*  PERFORM POPULATE_DATA_FOR_OUTPUT.
*
*  GD_DOC_DATA-DOC_SIZE = 1.
*
** Populate the subject/generic message attributes
*  GD_DOC_DATA-OBJ_LANGU = SY-LANGU.
*  GD_DOC_DATA-OBJ_NAME  = SY-REPID.
*  GD_DOC_DATA-OBJ_DESCR = L_SUBJECT.
*  GD_DOC_DATA-SENSITIVTY = 'F'.
*
** Describe the body of the message
*  CLEAR IT_PACKING_LIST.
*  REFRESH IT_PACKING_LIST.
*  IT_PACKING_LIST-TRANSF_BIN = SPACE.
*  IT_PACKING_LIST-HEAD_START = 1.
*  IT_PACKING_LIST-HEAD_NUM = 0.
*  IT_PACKING_LIST-BODY_START = 1.
*  DESCRIBE TABLE IT_MAIL LINES IT_PACKING_LIST-BODY_NUM.
*  IT_PACKING_LIST-DOC_TYPE = 'RAW'.
*  APPEND IT_PACKING_LIST.
*
** Add the recipients email address
*  CLEAR IT_RECEIVERS.
*  REFRESH IT_RECEIVERS.
*  IT_RECEIVERS-RECEIVER = P_EMAIL.
**  it_receivers-rec_type = 'U'.  " internet email
*  IT_RECEIVERS-REC_TYPE = 'C'.
*  IT_RECEIVERS-COM_TYPE = 'INT'.
*  IT_RECEIVERS-NOTIF_DEL = 'X'.
*  IT_RECEIVERS-NOTIF_NDEL = 'X'.
*  APPEND IT_RECEIVERS.
*
** Call the FM to post the message to SAPMAIL
*  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
*       EXPORTING
*            DOCUMENT_DATA              = GD_DOC_DATA
*            PUT_IN_OUTBOX              = 'X'
*       IMPORTING
*            SENT_TO_ALL                = GD_SENT_ALL
*       TABLES
*            PACKING_LIST               = IT_PACKING_LIST
*            CONTENTS_TXT               = IT_MAIL
*            RECEIVERS                  = IT_RECEIVERS
*       EXCEPTIONS
*            TOO_MANY_RECEIVERS         = 1
*            DOCUMENT_NOT_SENT          = 2
*            DOCUMENT_TYPE_NOT_EXIST    = 3
*            OPERATION_NO_AUTHORIZATION = 4
*            PARAMETER_ERROR            = 5
*            X_ERROR                    = 6
*            ENQUEUE_ERROR              = 7
*            OTHERS                     = 8.
*
** Store function module return code
*  GD_ERROR = SY-SUBRC.
*
*ENDFORM.                    " send_email

*---------------------------------------------------------------------*
*       FORM populate_data_for_output                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM POPULATE_DATA_FOR_OUTPUT.
*  DATA: L_MESSAGE TYPE SO_TEXT255,
*        L_KPEIN(4),
*        L_KBETR(12).
*
*  CLEAR: IT_MAIL,IT_MAIL[].
*
*** New format
*
*  MOVE: '-------------------' TO IT_MAIL+0(19),
*        '-------' TO IT_MAIL+19(7),
*        '-----------' TO IT_MAIL+26(11),
*        '-----------' TO IT_MAIL+37(11),
*        '--------------' TO IT_MAIL+48(14),
*        '-----------' TO IT_MAIL+62(11),
*        '-----------' TO IT_MAIL+73(11),
*        '--------------'  TO IT_MAIL+84(14),
*        '---------------' TO  IT_MAIL+98(15),
*        '--' TO IT_MAIL+113(2),
*        '-' TO IT_MAIL+115(1),
*        '------' TO IT_MAIL+116(6). .
*
*  APPEND IT_MAIL.
*  CLEAR: IT_MAIL.
*
*  MOVE: '|          Material' TO IT_MAIL+0(19),
*        '|Vendor' TO IT_MAIL+19(7),
*        '| Inf. Date' TO IT_MAIL+26(11),
*        '|Vest. Date' TO IT_MAIL+37(11),
*        '|    Vaats Amt' TO IT_MAIL+48(14),
*        '| Info From' TO IT_MAIL+62(11),
*        '|   Info To' TO IT_MAIL+73(11),
*        '|    Info Amnt'  TO IT_MAIL+84(14),
*        '|             Reasons' TO  IT_MAIL+98(21),
*        '|D' TO IT_MAIL+119(2),
*        '|' TO IT_MAIL+121(1).
*  APPEND IT_MAIL.
*  CLEAR: IT_MAIL.
*  MOVE: '-------------------' TO IT_MAIL+0(19),
*        '-------' TO IT_MAIL+19(7),
*        '-----------' TO IT_MAIL+26(11),
*        '-----------' TO IT_MAIL+37(11),
*        '--------------' TO IT_MAIL+48(14),
*        '-----------' TO IT_MAIL+62(11),
*        '-----------' TO IT_MAIL+73(11),
*        '--------------'  TO IT_MAIL+84(14),
*        '---------------' TO  IT_MAIL+98(15),
*        '--' TO IT_MAIL+113(2),
*        '-' TO IT_MAIL+115(1),
*        '------' TO IT_MAIL+116(6).
*  APPEND IT_MAIL.
*  CLEAR: IT_MAIL.
*
*  LOOP AT IT_ITAB.
*    L_KBETR = IT_ITAB-KBETR_SA.
*
*    MOVE: '|' TO IT_MAIL+0(1),
*          IT_ITAB-MATNR TO IT_MAIL+1(18),
*          '|' TO IT_MAIL+19(1),
*          IT_ITAB-LIFNR TO IT_MAIL+20(6),
*          '|' TO IT_MAIL+26(1),
*          IT_ITAB-INTF_D TO IT_MAIL+27(10),
*            '|' TO IT_MAIL+37(1),
*          IT_ITAB-DATAB_SA TO IT_MAIL+38(10),
*            '|' TO IT_MAIL+48(1),
*          IT_ITAB-KBETR_SA TO IT_MAIL+49(13),
*            '|' TO IT_MAIL+62(1),
*          IT_ITAB-DATAB TO IT_MAIL+63(10),
*            '|' TO IT_MAIL+73(1),
*          IT_ITAB-DATBI TO IT_MAIL+74(10),
*            '|' TO IT_MAIL+84(1),
*          IT_ITAB-KBETR TO IT_MAIL+85(13),
*            '|' TO IT_MAIL+98(1),
*          IT_ITAB-REMARKS+0(20) TO IT_MAIL+99(20),
*            '|' TO IT_MAIL+119(1),
*          IT_ITAB-DELETED TO IT_MAIL+120(1),
*            '|' TO IT_MAIL+121(1).
*
*    APPEND IT_MAIL.
*    CLEAR: IT_MAIL, L_MESSAGE, IT_MAIL.
*    MOVE: '-------------------' TO IT_MAIL+0(19),
*         '-------' TO IT_MAIL+19(7),
*         '-----------' TO IT_MAIL+26(11),
*         '-----------' TO IT_MAIL+37(11),
*         '--------------' TO IT_MAIL+48(14),
*         '-----------' TO IT_MAIL+62(11),
*         '-----------' TO IT_MAIL+73(11),
*         '--------------'  TO IT_MAIL+84(14),
*         '---------------' TO  IT_MAIL+98(15),
*         '--' TO IT_MAIL+113(2),
*         '-' TO IT_MAIL+115(1),
*         '------' TO IT_MAIL+116(6). .
*    APPEND IT_MAIL.
*    CLEAR: IT_MAIL.
*
*  ENDLOOP.
****
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_cond_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_COND_PRICE.
  CLEAR: IT_T683S, IT_T683S[].
  SELECT * INTO TABLE IT_T683S FROM T683S
  WHERE KVEWE = 'A'
    AND KAPPL = 'M'
    AND ( KALSM = 'RM0000' OR KALSM = 'RM0002' ).
ENDFORM.   " get_cond_price
*&---------------------------------------------------------------------*
*&      Form  get_model
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MODEL.

  DATA: L_MTART LIKE MARA-MTART,
        L_PSTAT LIKE MARA-PSTAT.

  DATA: LT_STPOS	LIKE	STPOV  OCCURS 0 WITH HEADER LINE,
        LT_EQUICAT LIKE CSCEQUI  OCCURS 0 WITH HEADER LINE,
        LT_KNDCAT	LIKE	CSCKND  OCCURS 0 WITH HEADER LINE,
        LT_MATCAT	LIKE	CSCMAT  OCCURS 0 WITH HEADER LINE,
        LT_STDCAT	LIKE	CSCSTD  OCCURS 0 WITH HEADER LINE,
        LT_TPLCAT	LIKE	CSCTPL  OCCURS 0 WITH HEADER LINE,
        LT_MC29S   LIKE MC29S   OCCURS 0 WITH HEADER LINE.

  DATA: L_DATUB    LIKE RC29P-DATUB,
        L_DATUV    LIKE RC29P-DATUV,
        L_WERKS    LIKE MARC-WERKS.

*  DATA : BEGIN OF LT_EKBE OCCURS 0,
*           BWART LIKE EKBE-BWART,
*           SHKZG LIKE EKBE-SHKZG,
*           MENGE LIKE EKBE-MENGE,
*           END OF LT_EKBE.

  READ TABLE S_MATNR INDEX 1.

  IF S_MATNR-LOW+0(2) = 'EM' OR S_MATNR-LOW+0(2) = 'CR'.
    SELECT A~MATNR MAKTX  WERKS BKLAS MATKL MSTAE STPRS A~PSTAT
       INTO TABLE IT_MARA
       FROM MARA AS A
       INNER JOIN MARC AS B
       ON A~MATNR = B~MATNR
       INNER JOIN MBEW AS C
       ON B~MATNR = C~MATNR
       AND B~WERKS = C~BWKEY
        INNER JOIN MAKT AS D
       ON A~MATNR = D~MATNR
    WHERE A~MATNR IN S_MATNR
      AND WERKS = P_WERKS
      AND SPRAS = 'E'.
  ELSE.
    SELECT A~MATNR MAKTX WERKS BKLAS MATKL MSTAE STPRS A~PSTAT
     INTO TABLE IT_MARA
     FROM MARA AS A
     INNER JOIN MARC AS B
     ON A~MATNR = B~MATNR
     INNER JOIN MBEW AS C
     ON B~MATNR = C~MATNR
     AND B~WERKS = C~BWKEY
      INNER JOIN MAKT AS D
     ON A~MATNR = D~MATNR
  WHERE ( A~MATNR LIKE 'EM%M1%'
         OR A~MATNR LIKE 'CR%M1%' )
*** FOR  TESTING ONLY
*  WHERE ( A~MATNR LIKE 'EM%'
*         OR A~MATNR LIKE 'CR%' )
*** END
    AND WERKS = P_WERKS
    AND SPRAS = 'E'.
  ENDIF.

  L_DATUB = P_DATE.
  L_DATUV = P_DATE.
  L_WERKS = 'P001'.

  CLEAR: W_SND_JOBS, W_RCV_JOBS.
  LOOP AT IT_VAATS.
    REFRESH: LT_STPOS, LT_EQUICAT, LT_KNDCAT, LT_MATCAT,
             LT_STDCAT, LT_TPLCAT, LT_MC29S.
    DO.
      CALL FUNCTION 'Z_FMM_GET_MODULE'
        STARTING NEW TASK W_TASKNAME
        DESTINATION IN GROUP 'PG_FTZ'
        PERFORMING COLLECT_DATA ON END OF TASK
        EXPORTING
          P_DATUB       = L_DATUB
          P_DATUV       = L_DATUV
          P_MATNR       = IT_VAATS-MATNR
          P_STLAN       = '2'
          P_WERKS       = L_WERKS
* IMPORTING
*   P_MC29S       = LT_MC29S
        TABLES
          STPOV         = LT_STPOS
          EQUICAT       = LT_EQUICAT
          KNDCAT        = LT_KNDCAT
          MATCAT        = LT_MATCAT
          STDCAT        = LT_STDCAT
          TPLCAT        = LT_TPLCAT
        EXCEPTIONS
           COMMUNICATION_FAILURE = 1
           SYSTEM_FAILURE        = 2
           RESOURCE_FAILURE      = 3.

      .
      CASE SY-SUBRC.
        WHEN 0.
          W_TASKNAME = W_TASKNAME + 1.
          W_SND_JOBS = W_SND_JOBS + 1.
          EXIT.
        WHEN 1 OR 2.
          W_EXCEP_FLAG = 'X'.
        WHEN 3.
          IF W_EXCEP_FLAG = SPACE.
            W_EXCEP_FLAG = 'X'.
            WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.01' SECONDS.
          ELSE.
            WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.1' SECONDS.
          ENDIF.
          IF SY-SUBRC EQ 0.
            CLEAR W_EXCEP_FLAG.
          ELSE.
*            exit.
          ENDIF.
      ENDCASE.
    ENDDO.
*    CALL FUNCTION 'CS_WHERE_USED_MAT'
*
*      EXPORTING
*        DATUB                            = L_DATUB
*        DATUV                            = L_DATUV
*        MATNR                            = IT_VAATS-MATNR
**       POSTP                            = ' '
**       RETCODE_ONLY                     = ' '
*        STLAN                            = '2'
*        WERKS                            = L_WERKS
**       MCLMT                            = ' '
**       MNSTL                            = ' '
**       MXSTL                            = ' '
**       NEWSI                            = ' '
**       STLTP                            = ' '
*       IMPORTING
*          TOPMAT                           = LT_MC29S
*      TABLES
*        WULTB                            = LT_STPOS
*        EQUICAT                          = LT_EQUICAT
*        KNDCAT                           = LT_KNDCAT
*        MATCAT                           = LT_MATCAT
*        STDCAT                           = LT_STDCAT
*        TPLCAT                           = LT_TPLCAT
**       PRJCAT                           =
*       EXCEPTIONS
*         CALL_INVALID                     = 1
*         MATERIAL_NOT_FOUND               = 2
*         NO_WHERE_USED_REC_FOUND          = 3
*         NO_WHERE_USED_REC_SELECTED       = 4
*         NO_WHERE_USED_REC_VALID          = 5
*         OTHERS                           = 6
*              .
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ELSE.
*      APPEND LINES OF LT_STPOS TO LT_COMP.
*    ENDIF.
  ENDLOOP.
  DO.
    WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS.
    IF W_RCV_JOBS >= W_SND_JOBS.
      EXIT.
    ENDIF.
  ENDDO.

  SORT IT_MOD_BOM BY IDNRK.

  LOOP AT IT_VAATS.
    MOVE-CORRESPONDING IT_VAATS TO IT_OUTTAB.
    IT_OUTTAB-RSN = IT_VAATS-RESN_C.
    IT_OUTTAB-APPROVAL = IT_VAATS-PUM_N.
    SELECT SINGLE A~MATNR MAKTX WERKS BKLAS MATKL MSTAE STPRS
       MTART A~PSTAT
      INTO (IT_OUTTAB-MATNR, IT_OUTTAB-MAKTX, IT_OUTTAB-WERKS,
            IT_OUTTAB-BKLAS, IT_OUTTAB-MATKL, IT_OUTTAB-MSTAE,
            IT_OUTTAB-STPRS, L_MTART, L_PSTAT)
      FROM MARA AS A
      INNER JOIN MARC AS B
      ON A~MATNR = B~MATNR
      INNER JOIN MBEW AS C
      ON B~MATNR = C~MATNR
      AND B~WERKS = C~BWKEY
      INNER JOIN MAKT AS D
      ON A~MATNR = D~MATNR
   WHERE WERKS = P_WERKS
     AND A~MATNR = IT_VAATS-MATNR
     AND SPRAS = 'E'.
*    read table Lt_mara with key matnr = it_vaats.
*    IF L_MTART = 'ROH' OR L_MTART = 'ROH1'.
*      IT_OUTTAB-MAT_CLASS = 'End'.
*    ELSEIF L_MTART = 'HALB'.
*      IT_OUTTAB-MAT_CLASS = 'Sub'.
*    ENDIF.

    IF  IT_OUTTAB-MSTAE <> '12'.
*      CONCATENATE IT_OUTTAB-REMARKS 'V2' INTO IT_OUTTAB-REMARKS
      CONCATENATE IT_OUTTAB-REMARKS 'Material Master Error'
          INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
    ELSE.
      IF L_PSTAT NA 'B'.
        CONCATENATE IT_OUTTAB-REMARKS 'Material Master Error'
            INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
      ENDIF.
    ENDIF.
    READ TABLE IT_MOD_BOM WITH KEY IDNRK =  IT_VAATS-MATNR.
    IF SY-SUBRC = 0.
      IT_OUTTAB-MAT_CLASS = 'Sub'.
    ELSE.
      IT_OUTTAB-MAT_CLASS = 'End'.
    ENDIF.

    APPEND IT_OUTTAB.
    CLEAR: IT_OUTTAB.
  ENDLOOP.

  SORT IT_MOD_BOM BY MATNR.
  DELETE ADJACENT DUPLICATES FROM IT_MOD_BOM COMPARING MATNR.

  LOOP AT IT_MARA.
    READ TABLE IT_MOD_BOM WITH KEY MATNR = IT_MARA-MATNR.
    IF SY-SUBRC = 0.
    ELSE.
      DELETE IT_MARA.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_MARA.
    MOVE-CORRESPONDING IT_MARA TO IT_OUTTAB.
    IF IT_MARA-MSTAE <> '12'.
*      CONCATENATE IT_OUTTAB-REMARKS 'V2'
      CONCATENATE IT_OUTTAB-REMARKS 'Material Master Error'
           INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
    ELSE.
      IF IT_MARA-PSTAT NA 'B'.
*      CONCATENATE IT_OUTTAB-REMARKS 'V2'
        CONCATENATE IT_OUTTAB-REMARKS 'Material Master Error'
             INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
      ENDIF.
    ENDIF.
    IT_OUTTAB-MAT_CLASS = 'Mod'.

** Get GR QTY
*    CLEAR: LT_EKBE[].
*    SELECT BWART SHKZG MENGE INTO TABLE LT_EKBE
*      FROM EKBE
*      WHERE BUDAT IN S_BUDAT
*        AND BWART IN ('101', '102', '122', '123')
*        AND MATNR = IT_OUTTAB-MATNR.
*
*    CLEAR: IT_OUTTAB-GRQTY.
*    LOOP AT LT_EKBE.
*      IF LT_EKBE-SHKZG = 'S'.
*        IT_OUTTAB-GRQTY = IT_OUTTAB-GRQTY + LT_EKBE-MENGE.
*      ELSE.
*        IT_OUTTAB-GRQTY = IT_OUTTAB-GRQTY - LT_EKBE-MENGE.
*      ENDIF.
*    ENDLOOP.

    APPEND IT_OUTTAB.
    CLEAR: IT_OUTTAB.
  ENDLOOP.



** Get GR QTY
*    CLEAR: LT_EKBE[].
*    SELECT BWART SHKZG MENGE INTO TABLE LT_EKBE
*      FROM EKBE
*      WHERE BUDAT IN S_BUDAT
*        AND BWART IN ('101', '102', '122', '123')
*        AND MATNR = IT_OUTTAB-MATNR.
*    CLEAR: IT_OUTTAB-GRQTY.
*    LOOP AT LT_EKBE.
*      IF LT_EKBE-SHKZG = 'S'.
*        IT_OUTTAB-GRQTY = IT_OUTTAB-GRQTY + LT_EKBE-MENGE.
*      ELSE.
*        IT_OUTTAB-GRQTY = IT_OUTTAB-GRQTY - LT_EKBE-MENGE.
*      ENDIF.
*    ENDLOOP.

ENDFORM.                    " get_model
*&---------------------------------------------------------------------*
*&      Form  get_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SA_ITEM.
  DATA: L_DATAB LIKE A018-DATAB,
       L_DATBI LIKE A018-DATBI.

  CLEAR: IT_SA, IT_SACOND_ITEM[], IT_SACOND_ITEM.

  SELECT SINGLE B~EBELN
        EBELP
        MATNR
        WERKS
        LGORT
        LIFNR
        ETFZ1
        A~BSTYP
        A~BUKRS
        BSART
        EKORG
        EKGRP
        KDATB
        KDATE
        A~AEDAT
        ERNAM
              INTO IT_SA
*               CORRESPONDING FIELDS OF TABLE IT_SA
              FROM EKKO AS A INNER JOIN EKPO AS B
                ON A~MANDT EQ B~MANDT
               AND A~EBELN EQ B~EBELN
              WHERE A~BSTYP EQ 'L'
               AND A~LIFNR = IT_OUTTAB-LIFNR
               AND B~MATNR = IT_OUTTAB-MATNR
               AND A~LOEKZ EQ SPACE
               AND B~LOEKZ EQ SPACE
               AND ELIKZ EQ SPACE.
  IT_OUTTAB-EBELN = IT_SA-EBELN.
  IF SY-SUBRC = 0.
    CLEAR : IT_SACOND_ITEM, IT_SACOND_ITEM[], W_KNUMH.

    IF IT_OUTTAB-MAT_CLASS = 'Mod'.
      SELECT SINGLE KNUMH DATAB DATBI INTO (W_KNUMH, L_DATAB, L_DATBI)
                        FROM A016
                       WHERE KAPPL EQ 'M'
                         AND KSCHL EQ 'PB00'
                         AND EVRTN EQ IT_SA-EBELN
                         AND EVRTP EQ IT_SA-EBELP
                         AND DATAB <= P_DATE
                         AND DATBI >= P_DATE.
    ELSE.
      SELECT SINGLE KNUMH DATAB DATBI INTO (W_KNUMH, L_DATAB, L_DATBI)
                          FROM A016
                         WHERE KAPPL EQ 'M'
                           AND KSCHL EQ 'PB00'
                           AND EVRTN EQ IT_SA-EBELN
                           AND EVRTP EQ IT_SA-EBELP
*                          AND DATAB = IT_OUTTAB-APP_D.
                      AND DATAB <= IT_OUTTAB-APP_D
                       AND DATBI >= IT_OUTTAB-APP_D.
*                         AND DATAB <= P_DATE
*                         AND DATBI >= P_DATE.

    ENDIF.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_SACOND_ITEM
             FROM KONP
            WHERE KNUMH EQ W_KNUMH
              AND KBETR > 0.

    LOOP AT IT_SACOND_ITEM.
      READ TABLE IT_T683S WITH KEY
                  KSCHL = IT_SACOND_ITEM-KSCHL
                  KALSM = 'RM0000'.
      IF IT_T683S-KSTAT = ' ' OR
          ( IT_T683S-KSTAT = 'X' AND IT_T683S-KVSL1 <> ' ' ).

        IT_SACOND_ITEM-DATAB = L_DATAB.
        IT_SACOND_ITEM-DATBI = L_DATBI.

        IT_SACOND_ITEM-AEDAT = IT_SA-AEDAT.
        IT_SACOND_ITEM-ERNAM = IT_SA-ERNAM.

        MODIFY IT_SACOND_ITEM.
        CONTINUE.
      ELSE.
        DELETE IT_SACOND_ITEM.
      ENDIF.
    ENDLOOP.
    SORT IT_SACOND_ITEM BY KSCHL DESCENDING.
  ELSE.
    SELECT SINGLE B~EBELN
        EBELP
        MATNR
        WERKS
        LGORT
        LIFNR
        ETFZ1
        A~BSTYP
        A~BUKRS
        BSART
        EKORG
        EKGRP
        KDATB
        KDATE
        A~AEDAT
        ERNAM
              INTO IT_SA
*               CORRESPONDING FIELDS OF TABLE IT_SA
              FROM EKKO AS A INNER JOIN EKPO AS B
                ON A~MANDT EQ B~MANDT
               AND A~EBELN EQ B~EBELN
              WHERE A~BSTYP EQ 'L'
               AND A~LIFNR = IT_OUTTAB-LIFNR
               AND B~MATNR = IT_OUTTAB-MATNR.
*               AND A~LOEKZ EQ SPACE
*               AND B~LOEKZ EQ SPACE
*               AND ELIKZ EQ SPACE.
    IF SY-SUBRC = 0.
*      CONCATENATE IT_OUTTAB-REMARKS 'I2'   "'Blocked or completed'
      CONCATENATE IT_OUTTAB-REMARKS
       'SA Block or Delivery completion error'
  INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
    ELSE.
*      IF IT_OUTTAB-MSTAE <> '11'.
*        CONCATENATE IT_OUTTAB-REMARKS 'I2'   "'No SA found'
*        INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_SA
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 200.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_OUTTAB'.
    PERFORM ASSIGN_ITAB_TO_ALV.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDMODULE.                 " display_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.
*  IT_SORT-SPOS           = 1.
*  IT_SORT-FIELDNAME      = 'MATNR'.
*  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
*  APPEND IT_SORT.
ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2082   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG  USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'MATNR'       ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'MAT_CLASS'       ' ',
                                  ' ' 'COLTEXT'     'Matl Cls',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'MAKTX'       ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '30',

*                                  'S' 'WERKS'       ' ',
*                                  ' ' 'COLTEXT'     'Plant',
*                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'BKLAS'        ' ',
                                  ' ' 'COLTEXT'     'Val Cls',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'MATKL'       ' ',
                                  ' ' 'COLTEXT'     'Matl Grp',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'MSTAE'        ' ',
                                  ' ' 'COLTEXT'     'X-Plant',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'STPRS'       ' ',
                                  ' ' 'COLTEXT'     'Std Price',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'LIFNR'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'GRQTY'         ' ',
                                  ' ' 'COLTEXT'     'GR Qty',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '13',

** Vaats
                                  'S' 'INTF_D'      ' ',
                                  ' ' 'COLTEXT'     'Intf Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'APP_D'       ' ',
                                  ' ' 'COLTEXT'     'Valid Date',
                                  'E' 'OUTPUTLEN'   '10',

  'S' 'PRICE'       ' ',
  ' ' 'COLTEXT'     'Vaats Price',
  ' ' 'NO_ZERO'     'X',
  'E' 'OUTPUTLEN'   '15',

  'S' 'INF_PER'       ' ',
  ' ' 'COLTEXT'     'Change%',
  ' ' 'NO_ZERO'     'X',
  ' ' 'DECIMALS_O'     '2',
  'E' 'OUTPUTLEN'   '6',

 'S' 'RSN'       ' ',
  ' ' 'COLTEXT'     'RSN',
  'E' 'OUTPUTLEN'   '4',

  'S' 'APPROVAL'       ' ',
  ' ' 'COLTEXT'     'Approval',
  'E' 'OUTPUTLEN'   '8',


** Info Record

  'S' 'INFO_PRICE'  ' ',
  ' ' 'COLTEXT'     'Info Price',
  ' ' 'NO_ZERO'     'X',
  'E' 'OUTPUTLEN'   '15',

  'S' 'INFO_FROM'   ' ',
  ' ' 'COLTEXT'     'Valid From',
  'E' 'OUTPUTLEN'   '10',

  'S' 'INFO_TO'     ' ',
  ' ' 'COLTEXT'     'Valid To',
  'E' 'OUTPUTLEN'   '10',

  'S' 'INFO_CR_BY'       ' ',
  ' ' 'COLTEXT'     'Created By',
  'E' 'OUTPUTLEN'   '10',

  'S' 'INFO_CR_ON'       ' ',
  ' ' 'COLTEXT'     'Created On',
  'E' 'OUTPUTLEN'   '10',

  'S' 'EBELN'      ' ',
  ' ' 'COLTEXT'     'SA NO',
  'E' 'OUTPUTLEN'   '12',

  'S' 'SA_PRICE'      ' ',
  ' ' 'COLTEXT'     'SA Price',
  ' ' 'NO_ZERO'     'X',
  'E' 'OUTPUTLEN'   '15',

  'S' 'SA_FROM'     ' ',
  ' ' 'COLTEXT'     'Valid From',
  'E' 'OUTPUTLEN'   '10',

  'S' 'SA_TO'      ' ',
  ' ' 'COLTEXT'     'Valid To',
  'E' 'OUTPUTLEN'   '10',

  'S' 'SA_CR_BY'       ' ',
  ' ' 'COLTEXT'     'Created By',
  'E' 'OUTPUTLEN'   '10',

  'S' 'SA_CR_ON'       ' ',
  ' ' 'COLTEXT'     'Created On',
  'E' 'OUTPUTLEN'   '10',

  'S' 'REMARKS'     ' ',
  ' ' 'COLTEXT'     'Error Description',
  'E' 'OUTPUTLEN'   '255'.


ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = SPACE
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_OUTTAB[]
               IT_SORT          = IT_SORT[].
ENDFORM.                    " ASSIGN_ITAB_TO_ALV

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'MM03'.
      PERFORM DISPLAY_MMO3.
    WHEN 'INFO'.
      PERFORM DISPLAY_INFO.
    WHEN 'ME33'.
      PERFORM DISPLAY_SA.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MMO3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_MMO3.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
         LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE IT_OUTTAB INDEX LT_ROWS-INDEX.
  IF IT_OUTTAB-MATNR = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD IT_OUTTAB-MATNR.
  CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
ENDFORM.                    " DISPLAY_MMO3
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_INFO.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
          LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE IT_OUTTAB INDEX LT_ROWS-INDEX.
  IF IT_OUTTAB-MATNR = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  SET PARAMETER ID 'LIF' FIELD IT_OUTTAB-LIFNR.
  SET PARAMETER ID 'EKO' FIELD 'PU01'.
  SET PARAMETER ID 'MAT' FIELD IT_OUTTAB-MATNR.
  SET PARAMETER ID 'WRK' FIELD ' '.
  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.
ENDFORM.                    " DISPLAY_INFO
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_SA.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
         LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE IT_OUTTAB INDEX LT_ROWS-INDEX.
  IF IT_OUTTAB-MATNR = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  SET PARAMETER ID 'VRT' FIELD IT_OUTTAB-EBELN.
  CALL TRANSACTION 'ME33' AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_SA
*&---------------------------------------------------------------------*
*&      Form  CHECK_ERROR_FOR_MOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_ERROR_FOR_MOD.
  PERFORM GET_SUB_PRICE.
  PERFORM CALCULATE_MODULE_PRICE.
  PERFORM CHECK_ABNOR_PRICE.
ENDFORM.                    " CHECK_ERROR_FOR_MOD
*&---------------------------------------------------------------------*
*&      Form  GET_SUB_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SUB_PRICE.
  DATA: C_CAPID   LIKE RC29L-CAPID VALUE 'PP01',
        L_MATNR LIKE MARA-MATNR,
        L_NAME1 LIKE LFA1-NAME1,
        LT_STB TYPE STPOX OCCURS 0 WITH HEADER LINE.


  DATA: LT_COST1 LIKE TABLE OF ZTMM_ASSY_COST1 WITH HEADER LINE,
        LT_TAB LIKE TABLE OF IT_TAB WITH HEADER LINE.

  DATA: L_VTYPE   LIKE   ZTMM_ASSY_COST1-VTYPE,
        L_MCODE   LIKE   ZTMM_ASSY_COST1-MCODE.

  L_VTYPE = IT_OUTTAB-MATNR+0(3).
  L_MCODE = IT_OUTTAB-MATNR+3(2).

  SELECT * INTO TABLE LT_COST1
    FROM ZTMM_ASSY_COST1
    WHERE  VTYPE = L_VTYPE
       AND MCODE = L_MCODE
       AND LIFNR = IT_OUTTAB-LIFNR
*       AND EKGRP    BETWEEN  WA_EKGRP_F AND WA_EKGRP_T
       AND DATAB <= P_DATE
       AND DATBI >= P_DATE.
**
  LOOP AT LT_COST1.
    SELECT G~MATNR AS MATNR
            D~MAKTX
            M~MEINS AS UNIT M~MEINS AS MEINS
            INTO CORRESPONDING FIELDS OF TABLE LT_TAB
               FROM MAST AS G
               INNER JOIN MARA AS M
               ON G~MATNR = M~MATNR
               INNER JOIN MAKT AS D
               ON M~MATNR = D~MATNR
              WHERE G~WERKS = P_WERKS
                AND G~STLAN = '2'
                AND G~STLAL = '01'
                AND G~MATNR = IT_OUTTAB-MATNR
                AND M~LVORM = ' '
                AND D~SPRAS = SY-LANGU.

    LOOP AT LT_TAB.
      IT_TAB = LT_TAB.
*      IT_TAB-NAME1 = L_NAME1.
      IT_TAB-LIFNR = LT_COST1-LIFNR.
      IT_TAB-VTYPE = LT_COST1-VTYPE.
      IT_TAB-EKGRP = LT_COST1-EKGRP.
      IT_TAB-ASYTR = LT_COST1-ASYTR.
      IT_TAB-DATAB = LT_COST1-DATAB.
      IT_TAB-DATBI = LT_COST1-DATBI.
      APPEND IT_TAB.
    ENDLOOP.
    REFRESH LT_TAB.
  ENDLOOP.

  LOOP AT IT_TAB.

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
        CAPID                       = C_CAPID
        DATUV                       = P_DATE
*    EMENG                       = p_emeng
*    MEHRS                       = p_mehrs
*    MMORY                       = p_mmory
        MTNRV                       = IT_TAB-MATNR
        MKTLS                       = 'X'
        STLAL                       = '01'
        STLAN                       = '2'
*   STPST                       = 0
*   SVWVO                       = 'X'
        WERKS                       = 'P001'
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

    MOVE-CORRESPONDING IT_TAB TO IT_OUT.
    LOOP AT LT_STB WHERE DATUV <= P_DATE AND DATUB > P_DATE.
      IT_OUT-COMP = LT_STB-IDNRK.
      IT_OUT-QNTY = LT_STB-MENGE.
      IT_OUT-DATAB = LT_STB-DATUV.
      IT_OUT-DATBI = LT_STB-DATUB.
      IT_OUT-CDATAB = LT_STB-DATUV.
      IT_OUT-CDATBI = LT_STB-DATUB.
      IT_OUT-UPGVC = LT_STB-UPGN.
      SELECT SINGLE MAKTX INTO IT_OUT-CMAKTX
        FROM MAKT
        WHERE MATNR = LT_STB-IDNRK.
      APPEND IT_OUT.
    ENDLOOP.

  ENDLOOP.

  PERFORM APPEND_ITAB.

ENDFORM.                    " GET_SUB_PRICE
*&---------------------------------------------------------------------*
*&      Form  APPEND_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPEND_ITAB.
  DATA: LW_CONTINUE VALUE 'X'.

  LOOP AT IT_OUT.
*  ON CHANGE OF wa_module-matnr OR wa_module-lifnr.
    ON CHANGE OF IT_OUT-MATNR OR IT_OUT-LIFNR.
      PERFORM READ_MODULE_INFO_RECORD.
    ENDON.
*    lw_continue = 'X'.
*    PERFORM check_cockpit_module_color USING lw_continue.
*
*    CHECK lw_continue EQ 'X'.

    PERFORM READ_SUB_INFO_RECORD.
  ENDLOOP.

ENDFORM.                    " APPEND_ITAB
*&---------------------------------------------------------------------*
*&      Form  READ_MODULE_INFO_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MODULE_INFO_RECORD.
  DATA: BEGIN OF LT_CONDITION OCCURS 0,
          KBETR LIKE ZVMM_INFO_CONDI-KBETR,
          KPEIN LIKE ZVMM_INFO_CONDI-KPEIN,
          KMEIN LIKE ZVMM_INFO_CONDI-KMEIN,
          KSCHL LIKE ZVMM_INFO_CONDI-KSCHL,
          KZUST LIKE ZVMM_INFO_CONDI-KZUST,
        END   OF LT_CONDITION.
  DATA: LW_MTOT LIKE WA_MODULE-NETPR.

  CLEAR: EINA, A018.
  CLEAR: WA_MODULE-NETPR, WA_MODULE-PEINH, WA_MODULE-MEINS,
         WA_MODULE-MSG,  WA_MODULE-STS.

*----- Check Material Master
  IF IT_OUT-MAKTX IS INITIAL.
    MOVE: C_NO_MATL TO WA_MODULE-STS.
    EXIT.
  ENDIF.

  SELECT SINGLE MATNR A~LOEKZ WGLIF
    INTO (EINA-MATNR,EINA-LOEKZ,EINA-WGLIF)
    FROM EINA AS A INNER JOIN EINE AS B
      ON A~INFNR = B~INFNR
   WHERE A~MATNR = IT_OUT-MATNR
     AND A~LIFNR = IT_OUT-LIFNR
     AND A~LOEKZ = ' '
     AND B~WERKS = ' '
*     AND B~EKORG = C_EKORG
     AND B~LOEKZ = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_NEW    TO WA_MODULE-STS.
    EXIT.
  ENDIF.

  IF EINA-LOEKZ EQ 'X'.
    MOVE: C_DELETED TO WA_MODULE-STS.
    EXIT.
  ENDIF.

*----- Read Module price
  SELECT SINGLE *
    FROM A018
   WHERE KAPPL =  'M'
     AND KSCHL =  'PB00'
     AND MATNR =  IT_OUT-MATNR
     AND LIFNR =  IT_OUT-LIFNR
*     AND EKORG =  C_EKORG
     AND ESOKZ =  '0'
     AND DATAB <= P_DATE
     AND DATBI >= P_DATE.
  IF SY-SUBRC NE 0.
    MOVE: C_NO_COND TO WA_MODULE-STS.
    EXIT.
  ELSE.
    MOVE: C_EXIST   TO WA_MODULE-STS.
  ENDIF.

  SELECT KBETR KPEIN KMEIN KZUST KSCHL
   INTO CORRESPONDING FIELDS OF TABLE LT_CONDITION
   FROM ZVMM_INFO_CONDI
  WHERE KNUMH = A018-KNUMH
    AND ( KSCHL = C_KSCHL   OR
          KSCHL = C_ZTIR    OR
          KSCHL LIKE 'ZP%' )
    AND LOEVM_KO = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_DEL_CON TO WA_MODULE-STS.
    EXIT.
  ENDIF.

  SORT LT_CONDITION BY KSCHL.
  LOOP AT LT_CONDITION.
    CASE LT_CONDITION-KSCHL.
      WHEN C_KSCHL.
        MOVE: LT_CONDITION-KBETR TO WA_MODULE-NETPR,
              LT_CONDITION-KPEIN TO WA_MODULE-PEINH,
              LT_CONDITION-KMEIN TO WA_MODULE-MEINS,
              LT_CONDITION-KZUST TO WA_MODULE-KZUST.
      WHEN C_ZTIR.
        LW_MTOT = WA_MODULE-NETPR + LT_CONDITION-KBETR.
        MOVE: LW_MTOT TO WA_MODULE-NETPR.
      WHEN OTHERS.
        MOVE: LT_CONDITION-KSCHL+2(2) TO W_INDEX.

        CONCATENATE: 'WA_MODULE-ZP' W_INDEX INTO W_MODULE.

        ASSIGN: (W_MODULE) TO <MODULE>.
        IF SY-SUBRC NE 0. CONTINUE. ENDIF.

        <MODULE> = LT_CONDITION-KBETR.
    ENDCASE.
  ENDLOOP.

  CLEAR: IT_MODULE.

  IF IT_MODULE-PEINH EQ 0.
    IT_MODULE-PEINH = 1.
  ENDIF.

*  MOVE: T001-WAERS TO WA_MODULE-WAERS.

  MOVE: WA_MODULE TO IT_MODULE.
  MOVE-CORRESPONDING  IT_OUT TO IT_MODULE.

  APPEND IT_MODULE.
  CLEAR: IT_MODULE.

ENDFORM.                    " READ_MODULE_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  READ_SUB_INFO_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_SUB_INFO_RECORD.
  DATA: LW_SUB(50).
  FIELD-SYMBOLS: <LW_SUB>.
  CLEAR: IT_SUB.

  READ TABLE IT_SUB WITH KEY COMP = IT_OUT-COMP.
  IF SY-SUBRC NE 0.
    MOVE-CORRESPONDING  IT_OUT TO IT_SUB.
    PERFORM CHECK_RTN.
    PERFORM APPEND_SUB_PRICE.
  ELSE.
    MOVE: IT_SUB-LIFNR  TO WA_SUB-LIFNR,
          IT_SUB-AMOUNT TO WA_SUB-AMOUNT,
          IT_SUB-KMEIN  TO WA_SUB-KMEIN,
          IT_SUB-NETPR  TO WA_SUB-NETPR,
          IT_SUB-PEINH  TO WA_SUB-PEINH,
          IT_SUB-WAERS  TO WA_SUB-WAERS,
          IT_SUB-KZUST  TO WA_SUB-KZUST,
          IT_SUB-STS    TO WA_SUB-STS,
          IT_SUB-MSG    TO WA_SUB-MSG,
          IT_SUB-ZTIR   TO WA_SUB-ZTIR,
          IT_SUB-DATAB  TO WA_SUB-DATAB,
          IT_SUB-DATBI  TO WA_SUB-DATBI.
    DO.
      MOVE: SY-INDEX TO W_INDEX.

      CONCATENATE: 'WA_SUB-ZP' W_INDEX INTO LW_SUB,
                   'IT_SUB-ZP' W_INDEX INTO W_SUB.

      ASSIGN: (W_SUB)  TO <SUB>,
              (LW_SUB) TO <LW_SUB>.
      IF SY-SUBRC NE 0. EXIT. ENDIF.

      MOVE: <SUB> TO <LW_SUB>.
    ENDDO.

    IF NOT ( IT_SUB-STS EQ C_NO_MATL OR
             IT_SUB-STS EQ C_NO_COND OR
             IT_SUB-STS EQ C_NO_INFO    ).
      IF NOT ( ( IT_SUB-MEINS EQ WA_SUB-KMEIN AND
                 IT_SUB-MEINS EQ IT_SUB-UNIT  AND
                 WA_SUB-KMEIN EQ IT_SUB-UNIT )   ).
        MOVE: C_UOM_ERR TO WA_SUB-STS.
      ENDIF.
    ENDIF.

    PERFORM APPEND_SUB_PRICE.
  ENDIF.

ENDFORM.                    " READ_SUB_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  APPEND_SUB_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPEND_SUB_PRICE.
  CLEAR: IT_SUB.

  CASE WA_SUB-STS.
    WHEN C_NO_INFO.
      MOVE: TEXT-B02 TO WA_SUB-MSG.
    WHEN C_NO_COND.
      MOVE: TEXT-B03 TO WA_SUB-MSG.
    WHEN C_UOM_ERR.
      MOVE: TEXT-B01 TO WA_SUB-MSG.
    WHEN C_NO_MATL.
      MOVE: TEXT-B07 TO WA_SUB-MSG.
  ENDCASE.

  MOVE: WA_MODULE-VTYPE TO WA_SUB-VTYPE,
        WA_MODULE-MATNR TO WA_SUB-MATNR.

  MOVE: WA_SUB TO IT_SUB.
  MOVE-CORRESPONDING IT_OUT TO IT_SUB.
  IT_SUB-MAKTX = IT_OUT-CMAKTX.
*  it_sub-datab = it_output-cdatab.
*  it_sub-datbi = it_output-cdatbi.
  IT_SUB-LIFNR  = WA_SUB-LIFNR.
  IT_SUB-DATAB = WA_SUB-DATAB.
  IT_SUB-DATBI = WA_SUB-DATBI.

  IF IT_SUB-DATAB IS INITIAL.
    IT_SUB-DATAB = IT_OUT-CDATAB.
  ENDIF.

  IF IT_SUB-DATBI IS INITIAL.
    IT_SUB-DATBI = IT_OUT-CDATBI.
  ENDIF.

  IF IT_SUB-PEINH EQ 0.
    IT_SUB-PEINH = 1.
  ENDIF.

  IT_SUB-AMOUNT = IT_SUB-QNTY * IT_SUB-NETPR / IT_SUB-PEINH.

  APPEND IT_SUB.
  CLEAR: IT_SUB, WA_SUB.

ENDFORM.                    " APPEND_SUB_PRICE
*&---------------------------------------------------------------------*
*&      Form  CHECK_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_RTN.
  DATA: BEGIN OF LT_CONDITION OCCURS 0,
          KBETR LIKE ZVMM_INFO_CONDI-KBETR,
          KPEIN LIKE ZVMM_INFO_CONDI-KPEIN,
          KMEIN LIKE ZVMM_INFO_CONDI-KMEIN,
          KSCHL LIKE ZVMM_INFO_CONDI-KSCHL,
          KZUST LIKE ZVMM_INFO_CONDI-KZUST,
        END   OF LT_CONDITION.

  DATA: BEGIN OF IT_EINA_EINE_TEMP OCCURS 0,
        MATNR LIKE EINA-MATNR,
        LIFNR LIKE EINA-LIFNR,
        WGLIF LIKE EINA-WGLIF,
        END OF IT_EINA_EINE_TEMP.

  IF    IT_OUT-CMAKTX IS INITIAL.
*  IF wa_sub-maktx IS INITIAL.
    MOVE: C_NO_MATL TO WA_SUB-STS.
    EXIT.
  ENDIF.

  SELECT MATNR A~LIFNR WGLIF INTO TABLE IT_EINA_EINE_TEMP
   FROM EINA AS A INNER JOIN EINE AS B
     ON A~INFNR = B~INFNR
  WHERE A~MATNR = IT_OUT-COMP
    AND A~URZZT = 'SUB'
    AND A~LOEKZ = ' '
    AND B~WERKS = ' '
*    AND B~EKORG = C_EKORG
    AND B~LOEKZ = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_NO_INFO TO WA_SUB-STS.
    EXIT.
  ENDIF.

*----- Read submaterial price
  LOOP AT IT_EINA_EINE_TEMP.
    CLEAR: EINA, A018.
    WA_SUB-LIFNR = IT_EINA_EINE_TEMP-LIFNR.
    SELECT SINGLE KNUMH DATAB DATBI
                    INTO (A018-KNUMH, WA_SUB-DATAB, WA_SUB-DATBI)
                    FROM A018
                    WHERE KAPPL =  'M'
                      AND KSCHL =  'PB00'
                      AND MATNR =  IT_OUT-COMP
                      AND LIFNR =  WA_SUB-LIFNR
*                      AND EKORG =  C_EKORG
                      AND ESOKZ =  '0'
                      AND DATAB <= P_DATE
                      AND DATBI >= P_DATE.
    IF SY-SUBRC EQ 0.
      EXIT.
    ENDIF.
  ENDLOOP.
*  if sy-subrc ne 0.
  IF A018-KNUMH IS INITIAL.
    MOVE: C_NO_COND TO WA_SUB-STS.
    EXIT.
  ENDIF.
*** END OF INSERTION

  SELECT SINGLE KBETR KPEIN KMEIN KZUST
    INTO (WA_SUB-NETPR,WA_SUB-PEINH,
          WA_SUB-KMEIN,WA_SUB-KZUST)
    FROM ZVMM_INFO_CONDI
   WHERE KNUMH = A018-KNUMH
     AND KSCHL = C_KSCHL
     AND LOEVM_KO = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_NO_COND TO WA_SUB-STS.
    EXIT.
  ENDIF.

  SELECT KBETR KPEIN KMEIN KZUST KSCHL
    INTO CORRESPONDING FIELDS OF TABLE LT_CONDITION
    FROM ZVMM_INFO_CONDI
   WHERE KNUMH = A018-KNUMH
     AND ( KSCHL = C_KSCHL   OR
           KSCHL = 'ZTIR'    OR
           KSCHL LIKE 'ZP%' )
     AND LOEVM_KO = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_NO_COND TO WA_SUB-STS.
    EXIT.
  ENDIF.

  SORT LT_CONDITION BY KSCHL.
  LOOP AT LT_CONDITION.
    CASE LT_CONDITION-KSCHL.
      WHEN C_KSCHL.
        MOVE: LT_CONDITION-KBETR TO WA_SUB-NETPR,
              LT_CONDITION-KPEIN TO WA_SUB-PEINH,
              LT_CONDITION-KMEIN TO WA_SUB-KMEIN,
              LT_CONDITION-KZUST TO WA_SUB-KZUST.
        IF IT_EINA_EINE_TEMP-WGLIF = 'ZTIR'.
          WA_SUB-ZTIR = LT_CONDITION-KBETR.
        ENDIF.

      WHEN OTHERS.
        MOVE: LT_CONDITION-KSCHL+2(2) TO W_INDEX.

        CONCATENATE: 'WA_SUB-ZP' W_INDEX INTO W_SUB.

        ASSIGN: (W_SUB) TO <SUB>.
        IF SY-SUBRC NE 0. CONTINUE. ENDIF.

        <SUB> = LT_CONDITION-KBETR.
    ENDCASE.
  ENDLOOP.

*----- A sub material's UoM must be 'EA'.
*----- If UoM is not 'EA', display error message.
  IF NOT ( ( IT_SUB-MEINS EQ WA_SUB-KMEIN AND
             IT_SUB-MEINS EQ IT_SUB-UNIT  AND
             WA_SUB-KMEIN EQ IT_SUB-UNIT )   ).
    MOVE: C_UOM_ERR TO WA_SUB-STS.
  ENDIF.

ENDFORM.                    " CHECK_RTN
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_MODULE_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALCULATE_MODULE_PRICE.
  DATA: LW_AMOUNT TYPE F,
        LW_MTOT LIKE IT_MODULE-MOAMT.

*  DATA: LW_DETAIL LIKE IT_DETAIL,
  DATA: L_INDEX LIKE SY-TABIX.

  SORT IT_MODULE BY VTYPE MATNR LIFNR.
  SORT IT_SUB BY VTYPE MATNR UPGVC PREF.

  LOOP AT IT_MODULE.
    READ TABLE IT_SUB WITH KEY VTYPE = IT_MODULE-VTYPE
                               MATNR = IT_MODULE-MATNR.
    IF SY-SUBRC = 0.
      L_INDEX = SY-TABIX.
      LOOP AT IT_SUB FROM L_INDEX WHERE VTYPE = IT_MODULE-VTYPE
                       AND MATNR = IT_MODULE-MATNR.

        IT_MODULE-DMAMT = IT_MODULE-DMAMT + IT_SUB-AMOUNT.
        IT_MODULE-ZTIR = IT_MODULE-ZTIR + IT_SUB-ZTIR
                         / IT_SUB-PEINH * IT_SUB-QNTY.

        IF IT_SUB-STS    NE 0 AND
           IT_MODULE-STS EQ C_EXIST.
          IT_MODULE-STS = C_SUB_ERR.
        ENDIF.
      ENDLOOP.
    ENDIF.
    MOVE: IT_MODULE-DMAMT TO IT_MODULE-DMBTR.
    IT_MODULE-MOAMT = IT_MODULE-ASYTR + IT_MODULE-DMBTR.

    LW_MTOT = IT_MODULE-NETPR + IT_MODULE-ZTIR.
*    IF   IT_MODULE-MOAMT NE  LW_MTOT AND
*       ( IT_MODULE-STS   EQ C_EXIST OR
*         IT_MODULE-STS   EQ C_SUB_ERR  ).
*      MOVE: C_DIFF    TO IT_MODULE-STS,
*            TEXT-B12  TO IT_MODULE-MSG.
*    ENDIF.
*
*    CASE IT_MODULE-STS.
*      WHEN C_NEW OR C_NO_COND.
*        MOVE: TEXT-B04 TO IT_MODULE-MSG.
*        MOVE: C_ERROR  TO IT_MODULE-INDICATOR.
*      WHEN C_DELETED.
*        MOVE: TEXT-B05 TO IT_MODULE-MSG.
*        MOVE: C_ERROR  TO IT_MODULE-INDICATOR.
*      WHEN C_DEL_CON.
*        MOVE: TEXT-B06 TO IT_MODULE-MSG.
*        MOVE: C_ERROR  TO IT_MODULE-INDICATOR.
*      WHEN C_NO_MATL.
*        MOVE: TEXT-B07 TO IT_MODULE-MSG.
*        MOVE: C_ERROR  TO IT_MODULE-INDICATOR.
*      WHEN C_EXIST.
*        MOVE: TEXT-B09 TO IT_MODULE-MSG.
*        MOVE: C_SUCCESS   TO IT_MODULE-INDICATOR.
*      WHEN C_SUB_ERR.
*        MOVE: TEXT-B10  TO IT_MODULE-MSG.
*        MOVE: C_WARNING TO IT_MODULE-INDICATOR.
*      WHEN C_DIFF.
*        MOVE: TEXT-B12  TO IT_MODULE-MSG.
*        MOVE: C_ERROR  TO IT_MODULE-INDICATOR.
*    ENDCASE.

    MODIFY IT_MODULE.
  ENDLOOP.
  READ TABLE IT_MODULE INDEX 1.
  LW_MTOT = IT_MODULE-NETPR + IT_MODULE-ZTIR.

  READ TABLE IT_SUB WITH KEY NETPR = 0.
  IF SY-SUBRC = 0.
*    CONCATENATE IT_OUTTAB-REMARKS 'M1'
    CONCATENATE IT_OUTTAB-REMARKS 'Zero sub part'
**                'Zero sub price'
            INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
  ENDIF.
*  LOOP AT IT_SUB.
*    IF IT_SUB-NETPR = 0.
*      CONCATENATE IT_OUTTAB-REMARKS 'M1'
***                'Zero sub price'
*                  INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
  IF IT_MODULE-MOAMT NE  LW_MTOT.
*    CONCATENATE IT_OUTTAB-REMARKS 'M2'
    CONCATENATE IT_OUTTAB-REMARKS
               'Module Roll up vs Info price mismatch'
**                'Roll over rice is not same as module price'
                INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
  ENDIF.
ENDFORM.                    " CALCULATE_MODULE_PRICE
*&---------------------------------------------------------------------*
*&      Form  CHECK_ABNOR_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_ABNOR_PRICE.
  DATA: LT_A018 LIKE TABLE OF A018 WITH HEADER LINE.
  DATA: LT_A017 LIKE TABLE OF A017 WITH HEADER LINE.
  DATA: L_ABN_PRICE LIKE IT_INFO_ITEM-KBETR.

  SELECT * INTO TABLE LT_A018
                       FROM A018
                      WHERE KAPPL EQ 'M'
                        AND KSCHL EQ 'PB00'
                        AND LIFNR IN S_LIFNR
                        AND MATNR EQ IT_OUTTAB-MATNR
*                      AND DATAB <= P_DATE
                        AND DATBI < P_DATE.

  IF SY-SUBRC <> 0.
    SELECT * INTO TABLE LT_A017
                     FROM A017
                    WHERE KAPPL EQ 'M'
                      AND KSCHL EQ 'PB00'
                      AND LIFNR IN S_LIFNR
                      AND MATNR EQ IT_OUTTAB-MATNR
*                      AND DATAB <= P_DATE
                      AND DATBI < P_DATE.

  ENDIF.

  IF LT_A018[] IS INITIAL.
    SORT LT_A017 DESCENDING BY DATAB.
    READ TABLE LT_A017 INDEX 1.
    W_KNUMH = LT_A017-KNUMH.
  ELSE.
    SORT LT_A018 DESCENDING BY DATAB.
    READ TABLE LT_A018 INDEX 1.
    W_KNUMH = LT_A018-KNUMH.
  ENDIF.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_INFO_ITEM
          FROM KONP
         WHERE KNUMH EQ W_KNUMH
           AND LOEVM_KO EQ SPACE
           AND KBETR > 0.

  READ TABLE IT_INFO_ITEM WITH KEY KSCHL = 'PB00'.
  IF SY-SUBRC = 0 AND IT_INFO_ITEM-KBETR > 0.

    L_ABN_PRICE = ( ( IT_OUTTAB-INFO_PRICE - IT_INFO_ITEM-KBETR ) /
                         IT_INFO_ITEM-KBETR ) * 100.
    L_ABN_PRICE = ABS( L_ABN_PRICE ).
    IF L_ABN_PRICE > 25.
*      CONCATENATE IT_OUTTAB-REMARKS 'M3'
      CONCATENATE IT_OUTTAB-REMARKS 'Module Abnormal Price Var <> 25%'
*                        price variance > 5'
                 INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_ABNOR_PRICE
*&---------------------------------------------------------------------*
*&      Form  collet_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM COLLECT_DATA  USING W_TASKNAME.
  DATA: LT_MOD_BOM LIKE TABLE OF STPOV WITH HEADER LINE.
  DATA:  L_REMARK(2),
         L_INDEX LIKE SY-INDEX,
         L_MATNR LIKE MARA-MATNR.

  RECEIVE RESULTS FROM FUNCTION 'Z_FMM_GET_MODULE'
                        IMPORTING PE_MATNR = L_MATNR
                       TABLES    STPOV = LT_MOD_BOM
                       EXCEPTIONS
                             COMMUNICATION_FAILURE  = 1
                             SYSTEM_FIALURE         = 2.
  IF SY-SUBRC NE 0.
    W_EXCEP_FLAG = 'X'.
    EXIT.
  ENDIF.
  W_RCV_JOBS = W_RCV_JOBS + 1.
  IF LT_MOD_BOM[] IS INITIAL.
  ELSE.
*    READ TABLE LT_MOD_BOM INDEX 1.
*    LT_MOD_BOM-IDNRK = L_MATNR.
*    MODIFY LT_MOD_BOM INDEX 1 TRANSPORTING IDNRK.
    APPEND LINES OF LT_MOD_BOM TO IT_MOD_BOM.
  ENDIF.
ENDFORM.                    " collet_data
*&---------------------------------------------------------------------*
*&      Form  check_module_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_MODULE_ERROR.
  DATA: L_FDATE LIKE SY-DATUM,
        L_TDATE LIKE SY-DATUM,
        L_INDEX LIKE SY-TABIX.

  CLEAR: W_SND_JOBS, W_RCV_JOBS.

  SELECT * INTO TABLE IT_COST1
    FROM ZTMM_ASSY_COST1
     WHERE DATAB <= P_DATE
       AND DATBI >= P_DATE.
  L_FDATE = S_BUDAT-LOW.
  IF S_BUDAT-HIGH IS INITIAL.
    L_TDATE = L_FDATE.
  ELSE.
    L_TDATE = S_BUDAT-HIGH.
  ENDIF.

  LOOP AT IT_OUTTAB.  "  WHERE MAT_CLASS = 'Mod'.
    L_INDEX = SY-TABIX.
    DO.
      CALL FUNCTION 'Z_FMM_MOD_ERROR'
        STARTING NEW TASK W_TASKNAME
        DESTINATION IN GROUP 'PG_FTZ'
        PERFORMING GET_CHECK_STATUS ON END OF TASK
        EXPORTING
          P_MATNR       = IT_OUTTAB-MATNR
          P_INDEX       = L_INDEX
          P_LIFNR       = IT_OUTTAB-LIFNR
          P_WERKS = P_WERKS
          P_DATE = P_DATE
          P_PRICE = IT_OUTTAB-INFO_PRICE
          P_CLASS = IT_OUTTAB-MAT_CLASS
          P_FDATE = L_FDATE
          P_TDATE = L_TDATE
        TABLES
          PT_COST1  = IT_COST1
        EXCEPTIONS
           COMMUNICATION_FAILURE = 1
           SYSTEM_FAILURE        = 2
           RESOURCE_FAILURE      = 3.

      .
      CASE SY-SUBRC.
        WHEN 0.
          W_TASKNAME = W_TASKNAME + 1.
          W_SND_JOBS = W_SND_JOBS + 1.
          EXIT.
        WHEN 1 OR 2.
          W_EXCEP_FLAG = 'X'.
        WHEN 3.
          IF W_EXCEP_FLAG = SPACE.
            W_EXCEP_FLAG = 'X'.
            WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.01' SECONDS.
          ELSE.
            WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.1' SECONDS.
          ENDIF.
          IF SY-SUBRC EQ 0.
            CLEAR W_EXCEP_FLAG.
          ELSE.
*            exit.
          ENDIF.
      ENDCASE.
    ENDDO.
  ENDLOOP.

  DO.
    WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS.
    IF W_RCV_JOBS >= W_SND_JOBS.
      EXIT.
    ENDIF.
  ENDDO.

  PERFORM GET_MOD_ERR.
ENDFORM.                    " check_module_error
*&---------------------------------------------------------------------*
*&      Form  GET_CHECK_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_COST1  text
*      -->P_=  text
*      -->P_IT_COST1  text
*      -->P_EXCEPTIONS  text
*      -->P_COMMUNICATION_FAILURE  text
*      -->P_=  text
*      -->P_1  text
*      -->P_SYSTEM_FAILURE  text
*      -->P_=  text
*      -->P_2  text
*      -->P_RESOURCE_FAILURE  text
*      -->P_=  text
*      -->P_3  text
*----------------------------------------------------------------------*
FORM GET_CHECK_STATUS USING W_TASKNAME.
  DATA: LT_COST1 LIKE TABLE OF ZTMM_ASSY_COST1 WITH HEADER LINE.
  DATA:  L_REMARK(50),
         L_INDEX LIKE SY-INDEX,
         L_GRQTY LIKE EKBE-MENGE,
         L_MATNR LIKE MARA-MATNR.

  RECEIVE RESULTS FROM FUNCTION 'Z_FMM_MOD_ERROR'
*                        IMPORTING P_MATNR = W_MATNR
                       IMPORTING PE_REMARK = L_REMARK
*                        PE_INDEX = L_INDEX
                        PE_GRQTY = L_GRQTY
                        PE_MATNR = L_MATNR
                   TABLES   PT_COST1     = LT_COST1
                       EXCEPTIONS
                             COMMUNICATION_FAILURE  = 1
                             SYSTEM_FIALURE         = 2.

  IF SY-SUBRC NE 0.
    W_EXCEP_FLAG = 'X'.
    EXIT.
  ENDIF.
  W_RCV_JOBS = W_RCV_JOBS + 1.
  IF L_MATNR IS INITIAL.
  ELSE.
    CLEAR: IT_MOD_ERR.
    IT_MOD_ERR-MATNR = L_MATNR.
    IT_MOD_ERR-GRQTY = L_GRQTY.
    IT_MOD_ERR-REMARKS = L_REMARK.
    APPEND IT_MOD_ERR.
  ENDIF.
*  IF L_INDEX > 0.
*  READ TABLE IT_OUTTAB WITH KEY MATNR = L_MATNR.
*  L_INDEX = SY-TABIX.
*  IF SY-SUBRC = 0.
*    IF L_REMARK IS INITIAL.
*    ELSE.
*      IF IT_OUTTAB-REMARKS IS INITIAL AND L_REMARK IS INITIAL.
*      ELSE.
*        CONCATENATE IT_OUTTAB-REMARKS L_REMARK INTO IT_OUTTAB-REMARKS
*                   SEPARATED BY '*'.
*      ENDIF.
*    ENDIF.
*    IT_OUTTAB-GRQTY = L_GRQTY.
*    MODIFY IT_OUTTAB INDEX L_INDEX TRANSPORTING REMARKS GRQTY.
*  ENDIF.
ENDFORM.                    " GET_CHECK_STATUS
*&---------------------------------------------------------------------*
*&      Form  write_error_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILTER_BY_ERROR_CODE.
  IF P_RSNC IS INITIAL.
  ELSE.
    LOOP AT IT_OUTTAB.
      IF IT_OUTTAB-REMARKS NS P_RSNC.
        DELETE IT_OUTTAB.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT IT_OUTTAB.
    IF IT_OUTTAB-REMARKS IS INITIAL.
      DELETE IT_OUTTAB.
    ENDIF.
  ENDLOOP.

*  CLEAR: IT_OUTTAB.
*  APPEND IT_OUTTAB.
*  APPEND IT_OUTTAB.
*  APPEND IT_OUTTAB.
*  APPEND IT_OUTTAB.
*
*  WA_COLOR-COLOR-COL = 3.
*  WA_COLOR-COLOR-INT = 1.
*  WA_COLOR-FNAME = 'MAT_CLASS'.
*  APPEND WA_COLOR TO IT_COLOR.
*  WA_COLOR-FNAME = 'MAKTX'.
*  APPEND WA_COLOR TO IT_COLOR.
*  WA_COLOR-FNAME = 'BKLAS'.
*  APPEND WA_COLOR TO IT_COLOR.
*
*  WA_COLOR-FNAME = 'MATKL'.
*  APPEND WA_COLOR TO IT_COLOR.
*
*  IT_OUTTAB-CT = IT_COLOR.
*
*  IT_OUTTAB-MAT_CLASS = 'V1'.
*  IT_OUTTAB-MAKTX = 'No Info Record Found'.
*  IT_OUTTAB-MATKL = 'PD,PC'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'V2'.
*  IT_OUTTAB-MAKTX = 'Material master error'.
*  IT_OUTTAB-MATKL = 'PD,PC'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'V3'.
*  IT_OUTTAB-MAKTX = 'Vattz vs Info Validity period mismatch'.
*  IT_OUTTAB-MATKL = 'PD,IT'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'V4'.
*  IT_OUTTAB-MAKTX = 'Vattz price  vs Info Price mismatch'.
*  IT_OUTTAB-WERKS = 'PD'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'V5'.
*  IT_OUTTAB-MAKTX = 'Vattz Abnormal Price Var <>5%'.
*  IT_OUTTAB-MATKL = 'PD'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'I1'.
*  IT_OUTTAB-MAKTX = 'Info Cr date later than valid from D'.
*  IT_OUTTAB-MATKL = 'PD'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'I2'.
*  IT_OUTTAB-MAKTX = 'SA Block or Delivery completion error'.
*  IT_OUTTAB-MATKL = 'PC'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'I3'.
*  IT_OUTTAB-MAKTX = 'Info vs SA validity period Mismatch'.
*  IT_OUTTAB-MATKL = 'PD'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'I4'.
*  IT_OUTTAB-MAKTX = 'Info price vs SA price Mismatch'.
*  IT_OUTTAB-MATKL = 'PD'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'I5'.
*  IT_OUTTAB-MAKTX = 'SA validity period is Expired'.
*  IT_OUTTAB-MATKL = 'PC'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'M1'.
*  IT_OUTTAB-MAKTX = 'Zero sub part'.
*  IT_OUTTAB-MATKL = 'PD,PC'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'M2'.
*  IT_OUTTAB-MAKTX = 'Module Roll up  vs Info price mismatch'.
*  IT_OUTTAB-MATKL = 'PD,PC'.
*  APPEND IT_OUTTAB.
*
*  IT_OUTTAB-MAT_CLASS = 'M3'.
*  IT_OUTTAB-MAKTX = 'Module Abnormal Price Var <>5%'.
*  IT_OUTTAB-MATKL = 'PD'.
*  APPEND IT_OUTTAB.
*
*
*  IT_OUTTAB-MAT_CLASS = 'E1'.
*  IT_OUTTAB-MAKTX = 'Payment Block'.
*  IT_OUTTAB-MATKL = 'PD,FI'.
*  APPEND IT_OUTTAB.
*
ENDFORM.                    " write_error_code


*---------------------------------------------------------------------*
*       FORM set_listbox_RSNCODE                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_LISTBOX_RSNCODE.
  CLEAR: NAME, VALUE, LIST.

  NAME = 'P_RSNC'.

  MOVE: SPACE       TO VALUE-KEY,
        'All'       TO VALUE-TEXT.
  APPEND VALUE TO LIST.

*  MOVE: 'V1'      TO VALUE-KEY,
*        'No Info Recode found'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'V2'      TO VALUE-KEY,
*        'Material master error'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'V3'      TO VALUE-KEY,
*         'Vattz vs Info Validity period mismatch'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'V4'      TO VALUE-KEY,
*          'Vattz price  vs Info Price mismatch'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'V5'      TO VALUE-KEY,
*          'Vattz Abnormal Price Var <>5%'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'I1'      TO VALUE-KEY,
*          'Info Created on date later than valid from Date'
*          TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'I2'      TO VALUE-KEY,
*          'SA Block or Delivery completion error'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'I3'      TO VALUE-KEY,
*          'Info vs SA validity period mismatch'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'I4'      TO VALUE-KEY,
*          'Info price vs SA price mismatch'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'I5'      TO VALUE-KEY,
*          'SA validity period is Expired'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'M1'      TO VALUE-KEY,
*          'Zero sub part'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'M2'      TO VALUE-KEY,
*          'Module Roll up  vs Info price mismatch'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'M3'      TO VALUE-KEY,
*          'Module Abnormal Price Var <>5%'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'E1'      TO VALUE-KEY,
*          'Payment Block'       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.


  MOVE: 'V1'      TO VALUE-TEXT,
        'No Info Recode found'       TO VALUE-KEY .
  APPEND VALUE TO LIST.

  MOVE: 'V2'      TO VALUE-TEXT,
        'Material Master Error'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'V3'      TO VALUE-TEXT,
         'Vattz vs Info Validity period mismatch'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'V4'      TO VALUE-TEXT,
          'Vattz price  vs Info Price mismatch'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'V5'      TO VALUE-TEXT,
          'Vattz Abnormal Price Var <> 25%'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'I1'      TO VALUE-TEXT,
          'Info Created on date later than valid from Date'
          TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'I2'      TO VALUE-TEXT,
          'SA Block or Delivery completion error'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'I3'      TO VALUE-TEXT,
          'Info vs SA validity period mismatch'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'I4'      TO VALUE-TEXT,
          'Info price vs SA price mismatch'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'I5'      TO VALUE-TEXT,
          'SA validity period is Expired'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'M1'      TO VALUE-TEXT,
          'Zero sub part'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'M2'      TO VALUE-TEXT,
          'Module Roll up vs Info price mismatch'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'M3'      TO VALUE-TEXT,
          'Module Abnormal Price Var <> 25%'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  MOVE: 'E1'      TO VALUE-TEXT,
          'Payment Block'       TO VALUE-KEY.
  APPEND VALUE TO LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = NAME
            VALUES = LIST.
ENDFORM.                    " set_listbox_lgort
*&---------------------------------------------------------------------*
*&      Form  get_mod_err
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MOD_ERR.
  DATA: L_INDEX LIKE SY-DATUM.
  LOOP AT IT_MOD_ERR.
    IF IT_MOD_ERR-REMARKS IS INITIAL AND IT_MOD_ERR-GRQTY = 0.
    ELSE.
      READ TABLE IT_OUTTAB WITH KEY MATNR = IT_MOD_ERR-MATNR.
      IF SY-SUBRC = 0.
        L_INDEX = SY-TABIX.
        IF IT_OUTTAB-REMARKS IS INITIAL.
          IT_OUTTAB-REMARKS = IT_MOD_ERR-REMARKS.
        ELSE.
          CONCATENATE IT_OUTTAB-REMARKS IT_MOD_ERR-REMARKS
             INTO IT_OUTTAB-REMARKS SEPARATED BY '*'.
        ENDIF.
        IT_OUTTAB-GRQTY = IT_MOD_ERR-GRQTY.
        MODIFY IT_OUTTAB INDEX L_INDEX TRANSPORTING REMARKS GRQTY.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_mod_err
