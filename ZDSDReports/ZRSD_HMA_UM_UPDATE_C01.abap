*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_C01                                        *
*----------------------------------------------------------------------*
INCLUDE ZRPP_FUNC_ALV.
*&---------------------------------------------------------------------*
*&      Form  1000_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM 1000_DISPLAY_DATA .

*---------------------------------------------------------------


*---------------------------------------------------------------

*---------------------------------------------------------------

  CLEAR G_VARIANT_S.
  G_VARIANT_S-REPORT = GV_REPID = SY-CPROG.

*---------------------------------------------------------------
  PERFORM SET_SORT
          USING: 'PRDOD' 'GT_ITEM' 'X' 'X'  'X' ,
                 'NATN'  'GT_ITEM' 'X' 'X'  'X' ,
                 'DIST'  'GT_ITEM' 'X' 'X'  'X',
                 'WKEXC'   'GT_ITEM' 'X' 'X'  'X',
                 'WKINC'   'GT_ITEM' 'X' 'X'  'X',
                 'DESTN'   'GT_ITEM' 'X' 'X' 'X',
                 'ZVIN'   'GT_ITEM' 'X' 'X'  'X'.


*---------------------------------------------------------------

  PERFORM SET_LAYOUT
          USING 'X'
                ''
                ' '
                ''.
  G_LAYOUT_S-LIGHTS_TABNAME    = 'GT_ITEM'.
  G_LAYOUT_S-BOX_TABNAME       = 'GT_ITEM'.
*  G_LAYOUT_S-DEFAULT_ITEM      = 'X'.
  G_LAYOUT_S-EXPAND_ALL        = SPACE.
*  G_LAYOUT_S-EXPAND_FIELDNAME  = 'EXPAND'.
*  G_LAYOUT_S-DETAIL_POPUP      = 'X'.


*---------------------------------------------------------------

  PERFORM GET_FILEDCATALOG_ALV.

*---------------------------------------------------------------

  PERFORM BUILD_EVENT
          USING : C_USER_COMMAND,
                  C_STATUS_SET,
                  C_CONTEXT_MENU.
*                  C_TOP_OF_PAGE.

*---------------------------------------------------------------

  PERFORM LOCAL_USERCOMMAND
          USING SY-UCOMM
                G_SELFIELD.


*---------------------------------------------------------------

  PERFORM LOCAL_TOPOFPAGE
            USING 'X' .

  PERFORM LOCAL_BEFORE_LINE
            USING GS_LINEINFO
                  SPACE.

  PERFORM LOCAL_AFTER_LINE
          USING GS_LINEINFO
                SPACE.
  PERFORM LOCAL_PFSTATUS
          USING  G_EXTAB[].


*---------------------------------------------------------------

  PERFORM DISPLAY_GRID_ALV
  TABLES GT_ITEM[].

*---------------------------------------------------------------


ENDFORM. " 1000_DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_FILEDCATALOG_ALV
*&---------------------------------------------------------------------*

FORM GET_FILEDCATALOG_ALV .


*_헤더 FIELDCAT 구성

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  PERFORM GET_FILEDCATALOG TABLES G_FIELDCAT_T
                           USING  'GT_ITEM'.

*
  LOOP AT G_FIELDCAT_T INTO LS_FIELDCAT.

    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'STATUS'.
*        LS_FIELDCAT-ICON = 'X'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Total Status'.
        LS_FIELDCAT-SELTEXT_L    = 'Total Status'.
        LS_FIELDCAT-SELTEXT_S    = 'S'.
        LS_FIELDCAT-DDICTXT      = 'S'.
        LS_FIELDCAT-EMPHASIZE    = 'C700'.
*              LS_FIELDCAT-TOOLTIP = LS_FIELDCAT-FIELDNAME..
      WHEN 'ST_WOHD'.
*        LS_FIELDCAT-ICON = 'X'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Work Order Header Status'.
        LS_FIELDCAT-SELTEXT_L    = 'Work Order Header Status'.
        LS_FIELDCAT-SELTEXT_S    = 'HD'.
        LS_FIELDCAT-DDICTXT      = 'S'.
        LS_FIELDCAT-EMPHASIZE    = 'C200'.
*              LS_FIELDCAT-TOOLTIP = LS_FIELDCAT-FIELDNAME..
      WHEN 'ST_WOCL'.
*        LS_FIELDCAT-ICON = 'X'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Work Order Color Status'.
        LS_FIELDCAT-SELTEXT_L    = 'Work Order Color Status'.
        LS_FIELDCAT-SELTEXT_S    = 'CL'.
        LS_FIELDCAT-DDICTXT      = 'S'.
        LS_FIELDCAT-EMPHASIZE    = 'C200'.
*              LS_FIELDCAT-TOOLTIP = LS_FIELDCAT-FIELDNAME..
      WHEN 'ST_WOSUM'.
*        LS_FIELDCAT-ICON = 'X'.

        LS_FIELDCAT-REPTEXT_DDIC = 'Work Order Summary Status '.
        LS_FIELDCAT-SELTEXT_L    = 'Work Order Summary Status '.
        LS_FIELDCAT-SELTEXT_S    = 'SUM'.
        LS_FIELDCAT-DDICTXT      = 'S'.
        LS_FIELDCAT-EMPHASIZE    = 'C200'.
*              LS_FIELDCAT-TOOLTIP = LS_FIELDCAT-FIELDNAME..
      WHEN 'PRDOD' .
        LS_FIELDCAT-SELTEXT_S  = 'Order'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Order No'.
        LS_FIELDCAT-SELTEXT_L    = 'Order No'.
        LS_FIELDCAT-EMPHASIZE    = 'C211'.
      WHEN 'NATN'  .
        LS_FIELDCAT-REPTEXT_DDIC = 'Nation'.
        LS_FIELDCAT-SELTEXT_L    = 'Nation'.
        LS_FIELDCAT-EMPHASIZE    = 'C211'.

      WHEN 'DIST'  .
        LS_FIELDCAT-SELTEXT_S = 'Dealer'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Work Order Dealer'.
        LS_FIELDCAT-SELTEXT_L    = 'Work Order Dealer'.
        LS_FIELDCAT-EMPHASIZE    = 'C211'.
      WHEN 'WKEXC' .
        LS_FIELDCAT-SELTEXT_S = 'ExtColor'.
        LS_FIELDCAT-REPTEXT_DDIC =  'Work Order Ext Color'.
        LS_FIELDCAT-SELTEXT_L    =  'Work Order Ext Color'.
        LS_FIELDCAT-EMPHASIZE    = 'C411'.
      WHEN 'WKINC' .
        LS_FIELDCAT-SELTEXT_S =  'IntColor'.
        LS_FIELDCAT-REPTEXT_DDIC =  'Work Order Int Color'.
        LS_FIELDCAT-SELTEXT_L    =  'Work Order Int Color'.
        LS_FIELDCAT-EMPHASIZE    = 'C411'.
      WHEN 'DESTN' .
        LS_FIELDCAT-SELTEXT_S =  'Dest'.
        LS_FIELDCAT-REPTEXT_DDIC =  'Destination Code'.
        LS_FIELDCAT-SELTEXT_L    =  'Destination Code'.
        LS_FIELDCAT-EMPHASIZE    = 'C411'.
      WHEN 'ZVIN'  .
        LS_FIELDCAT-SELTEXT_S =  'ZVIN'.
        LS_FIELDCAT-REPTEXT_DDIC =  'HMA Internal VIN'.
        LS_FIELDCAT-SELTEXT_L    =  'HMA Internal VIN'.
        LS_FIELDCAT-EMPHASIZE    = 'C400'.
      WHEN 'MDYR'  .
        LS_FIELDCAT-SELTEXT_S =  'ModelYear'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Model Year'.
        LS_FIELDCAT-SELTEXT_L    = 'Model Year'.
      WHEN 'MDINX' .
        LS_FIELDCAT-SELTEXT_S = 'Model'.
        LS_FIELDCAT-REPTEXT_DDIC =  'Model Index'.
        LS_FIELDCAT-SELTEXT_L    =  'Model Index'.
      WHEN 'OCCN'  .
        LS_FIELDCAT-SELTEXT_S = 'Option'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Option Combination Number'.
        LS_FIELDCAT-SELTEXT_L    = 'Option Combination Number'.
      WHEN 'GRADE' .
        LS_FIELDCAT-SELTEXT_S = 'Grade'.
        LS_FIELDCAT-REPTEXT_DDIC = 'GRADE'.
        LS_FIELDCAT-SELTEXT_L    = 'GRADE'.
      WHEN 'IOQTY' .
        LS_FIELDCAT-SELTEXT_S =  'IntQty'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Initial order QTY'.
        LS_FIELDCAT-SELTEXT_L    = 'Initial order QTY'.
      WHEN 'MOQTY' .
        LS_FIELDCAT-SELTEXT_S = 'ModQty'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Modification order QTY'.
        LS_FIELDCAT-SELTEXT_L    = 'Modification order QTY'.
      WHEN 'LCLDR' .
        LS_FIELDCAT-SELTEXT_S = 'No.'.
        LS_FIELDCAT-REPTEXT_DDIC =   'Dealer No (not decided)'.
        LS_FIELDCAT-SELTEXT_L    =   'Dealer No (not decided)'.
      WHEN 'DLNM'  .
        LS_FIELDCAT-SELTEXT_S =  'Dealer'.
        LS_FIELDCAT-REPTEXT_DDIC =  'Dealer Name (not decided)'.
        LS_FIELDCAT-SELTEXT_L    =  'Dealer Name (not decided)'.
      WHEN 'WOUPS' .
        LS_FIELDCAT-SELTEXT_S =  'E Order'.
        LS_FIELDCAT-REPTEXT_DDIC =  'E-Order Referrence'.
        LS_FIELDCAT-SELTEXT_L    =  'E-Order Referrence'.
      WHEN 'LCLDL' .
        LS_FIELDCAT-SELTEXT_S = 'E Order '.
        LS_FIELDCAT-REPTEXT_DDIC =  'E-Order Ref with no serial'.
        LS_FIELDCAT-SELTEXT_L    =  'E-Order Ref with no serial'.
      WHEN 'LCCNT' .
        LS_FIELDCAT-SELTEXT_S = 'LC'.
        LS_FIELDCAT-REPTEXT_DDIC =  'L/C Confirm'.
        LS_FIELDCAT-SELTEXT_L    =  'L/C Confirm'.
      WHEN 'FLTFG' .
        LS_FIELDCAT-SELTEXT_S = 'Fleet'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Fleet Flag'.
        LS_FIELDCAT-SELTEXT_L    = 'Fleet Flag'.
      WHEN 'RDD'   .
        LS_FIELDCAT-SELTEXT_S = 'Req.Date'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Request delivery date'.
        LS_FIELDCAT-SELTEXT_L    = 'Request delivery date'.
      WHEN 'CRDAT' .
        LS_FIELDCAT-SELTEXT_S = 'Create'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Creation Date'.
        LS_FIELDCAT-SELTEXT_L    = 'Creation Date'.
      WHEN 'AEDAT' .
        LS_FIELDCAT-SELTEXT_S = 'Change'.
        LS_FIELDCAT-REPTEXT_DDIC = 'Change Date'.
        LS_FIELDCAT-SELTEXT_L    = 'Change Date'.
*      WHEN 'AEDAT'.
*        LS_FIELDCAT-REPTEXT_DDIC = 'Work Order Summary Status '.
*        LS_FIELDCAT-SELTEXT_L    = 'Work Order Summary Status '.

    ENDCASE.

    LS_FIELDCAT-DDICTXT      = 'S'.
    MODIFY G_FIELDCAT_T FROM LS_FIELDCAT.
  ENDLOOP.


*  PERFORM GET_FILEDCATALOG TABLES LT_FIELDCAT
*                           USING  'GT_ITEM'.

*  LS_FIELDCAT-KEY = SPACE.
*  MODIFY LT_FIELDCAT FROM LS_FIELDCAT
*         TRANSPORTING KEY
*         WHERE KEY NE ''.
*  LOOP AT LT_FIELDCAT INTO LS_FIELDCAT.
*
*    CASE LS_FIELDCAT-FIELDNAME.
*     ENDCASE.
*
*    MODIFY LT_FIELDCAT FROM LS_FIELDCAT.
*
*  ENDLOOP.
*
*  CLEAR : LS_FIELDCAT.
*
*  IF NOT LT_FIELDCAT[] IS INITIAL.
*
*  APPEND LINES OF LT_FIELDCAT TO G_FIELDCAT_T.
*  ENDIF.
ENDFORM. " GET_FILEDCATALOG_ALV


*&---------------------------------------------------------------------*
*&      Form  LOCAL_USERCOMMAND
*&---------------------------------------------------------------------*

FORM LOCAL_USERCOMMAND
     USING L_UCOMM LIKE SY-UCOMM
           LS_SELFIELD TYPE SLIS_SELFIELD.

*
  CASE L_UCOMM.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE SCREEN .
  ENDCASE.

  LS_SELFIELD-REFRESH = 'X'.
  CASE L_UCOMM.
    WHEN 'INP'.
    PERFORM EXEC_UPDATE USING 'INP'.
    WHEN 'VP'.
     PERFORM EXEC_UPDATE USING 'VP'.
  ENDCASE.
*  CASE L_UCOMM.
**___보류재고 전환
*
*    WHEN '&MBXX'.
*
*      LS_SELFIELD-REFRESH = 'X'.
*
*
**___제품리스트
*
*    WHEN '&XX1'.
*
*
*
**___S/R리스트
*
*    WHEN '&MD04'.
*
*
**___재고개요
*
*    WHEN '&MMBE'.
*
*
**___수불원장
*
*    WHEN '&XX2'.
*
*  ENDCASE.

ENDFORM. " LOCAL_USERCOMMAND

*&---------------------------------------------------------------------*
*&      Form  LOCAL_TOPOFPAGE
*&---------------------------------------------------------------------*

FORM LOCAL_TOPOFPAGE USING P_CHECK.



  CHECK P_CHECK EQ 'X'.

*  WRITE: / SY-TITLE.

*_
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      IT_LIST_COMMENTARY = G_LIST_TOP_OF_PAGE_T.
*

ENDFORM. " LOCAL_TOPOFPAGE


*&---------------------------------------------------------------------*
*&      Form  LOCAL_PFSTATUS
*&---------------------------------------------------------------------*

FORM LOCAL_PFSTATUS USING EXTAB TYPE SLIS_T_EXTAB.

*
  DATA : LV_CNT TYPE I ,
         TITLE(100).

  DESCRIBE TABLE GT_ITEM LINES LV_CNT.
  WRITE LV_CNT TO TITLE.
  CONCATENATE TITLE 'Lines ' INTO TITLE.
  SET TITLEBAR 'T100' WITH TITLE.
  SET PF-STATUS 'S100' EXCLUDING EXTAB.

ENDFORM. " LOCAL_PFSTATUS

*&---------------------------------------------------------------------*
*&      Form  LOCAL_BEFORE_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LINEINFO  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*

FORM LOCAL_BEFORE_LINE
     USING    PS_LINEINFO TYPE KKBLO_LINEINFO
              P_VALUE.
  CHECK NOT P_VALUE IS INITIAL.

ENDFORM. " LOCAL_BEFORE_LINE

*&---------------------------------------------------------------------*
*&      Form  LOCAL_AFTER_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LINEINFO  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*

FORM LOCAL_AFTER_LINE
     USING    PS_LINEINFO TYPE KKBLO_LINEINFO
              P_VALUE.


  CHECK NOT P_VALUE IS INITIAL.



ENDFORM. " LOCAL_AFTER_LINE
