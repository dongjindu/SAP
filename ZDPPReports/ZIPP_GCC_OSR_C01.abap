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

  G_TABNAME_HEADER = 'GT_HEAD'.
  G_TABNAME_ITEM   = 'GT_ITEM'.

*---------------------------------------------------------------

  CLEAR: G_KEYINFO_S.
  G_KEYINFO_S-HEADER01 = 'WO_SER'.
  G_KEYINFO_S-ITEM01   = 'WO_SER'.

*---------------------------------------------------------------

  CLEAR G_VARIANT_S.
  G_VARIANT_S-REPORT = GV_REPID.

*---------------------------------------------------------------
*  PERFORM SET_SORT
*          USING: 'SPMON'  'GT_POS' 'X' 'X'  ' ' .

*---------------------------------------------------------------

*  PERFORM SET_LAYOUT
*          USING 'X'
*                'LIGHT'
*                ' '
*                'XSELP'.
*  G_LAYOUT_S-LIGHTS_TABNAME = 'GT_ITEM'.
*  G_LAYOUT_S-BOX_TABNAME       = 'GT_ITEM'.
*  G_LAYOUT_S-DEFAULT_ITEM      = 'X'.
*  G_LAYOUT_S-EXPAND_ALL        = SPACE.
*  G_LAYOUT_S-EXPAND_FIELDNAME  = 'EXPAND'.
*  G_LAYOUT_S-DETAIL_POPUP      = 'X'.


*---------------------------------------------------------------

  PERFORM GET_FILEDCATALOG_ALV.

*---------------------------------------------------------------

  PERFORM BUILD_EVENT
          USING : C_USER_COMMAND,
                  C_STATUS_SET.

*---------------------------------------------------------------

  PERFORM LOCAL_USERCOMMAND
          USING SY-UCOMM
                G_SELFIELD.

*---------------------------------------------------------------

  PERFORM LOCAL_TOPOFPAGE
            USING ' ' .

  PERFORM LOCAL_BEFORE_LINE
            USING GS_LINEINFO
                  SPACE.

  PERFORM LOCAL_AFTER_LINE
          USING GS_LINEINFO
                SPACE.
  PERFORM LOCAL_PFSTATUS
          USING  G_EXTAB[].


*---------------------------------------------------------------

*  PERFORM DISPLAY_HIERSEQ_ALV
*          TABLES   GT_ITEM[]
*                   GT_ITEM[].
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
*  PERFORM GET_FILEDCATALOG TABLES G_FIELDCAT_T
*                           USING  'GT_ITEM'.

  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'DIST'.
  LS_FIELDCAT-REPTEXT_DDIC = 'distributor' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T.
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'PACK'.
  LS_FIELDCAT-REPTEXT_DDIC = 'work order' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'MCCD'.
  LS_FIELDCAT-REPTEXT_DDIC = 'model code ' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'GRDE'.
  LS_FIELDCAT-REPTEXT_DDIC = 'GRADE' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'OCCN'.
  LS_FIELDCAT-REPTEXT_DDIC = 'OCCN	' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'EXCL'.
  LS_FIELDCAT-REPTEXT_DDIC = 'Ext color' .
  LS_FIELDCAT-EMPHASIZE  = 'C410'.
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'INCL'.
  LS_FIELDCAT-REPTEXT_DDIC = 'Int color' .
    LS_FIELDCAT-EMPHASIZE  = 'C410'.
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'WKNO'.
  LS_FIELDCAT-REPTEXT_DDIC = 'W/O NO. (WORK ORDER HEADER)' .
    LS_FIELDCAT-EMPHASIZE  = 'C410'.
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'PLNT'.
  LS_FIELDCAT-REPTEXT_DDIC = 'plant code ' .
    LS_FIELDCAT-EMPHASIZE  = 'C410'.
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'REFE'.
  LS_FIELDCAT-REPTEXT_DDIC = 'REFE' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'VQTY'.
  LS_FIELDCAT-REPTEXT_DDIC = 'initial QUANTITY' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'CQTY'.
  LS_FIELDCAT-REPTEXT_DDIC = 'modification QUANTITY       ' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'OCHD'.
  LS_FIELDCAT-REPTEXT_DDIC = 'quantity MODIFICATION DATE' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'CRDT'.
  LS_FIELDCAT-REPTEXT_DDIC = 'data CREATION DATE' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'FLAG'.
  LS_FIELDCAT-REPTEXT_DDIC = 'FLAG' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = 'SNDT'.
  LS_FIELDCAT-REPTEXT_DDIC = 'interface DATE' .
  APPEND LS_FIELDCAT TO G_FIELDCAT_T .
  CLEAR : LS_FIELDCAT.

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

  WRITE: / SY-TITLE.

*_
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      IT_LIST_COMMENTARY = G_LIST_TOP_OF_PAGE_T.


ENDFORM. " LOCAL_TOPOFPAGE


*&---------------------------------------------------------------------*
*&      Form  LOCAL_PFSTATUS
*&---------------------------------------------------------------------*

FORM LOCAL_PFSTATUS USING EXTAB TYPE SLIS_T_EXTAB.

*

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
