*&------------------------------------------------------------------
*& Program ID     : ZMMR30500T
*& Program Name   : Create BOL with text file (portal site)
*& Created by     : PAUL
*& Created on     : 06.27.2011
*& Development ID : T00189
*& Description    : Web upload and  creates with Local Text File
*&                  And the program for BOL
*& To web will count and with under upload function with text
*&and to be a restricted fact where the support does not become, is done
*& Modification Log
*&====================================================================
*& Date        Developer    Request ID    Description
*& 06.27.2011  Paul                           first dev.
*&--------------------------------------------------------------------
REPORT  ZMMR30500T.
TABLES:
  EDP21,
  LIKP,
  TVSA.

INCLUDE OLE2INCL.

*----------------------------------------------------------------------*
*    Excel Variables.
*----------------------------------------------------------------------*
DATA: BEGIN OF T_IDOC_STATUS OCCURS 0.
        INCLUDE STRUCTURE BDIDOCSTAT.
DATA: END OF T_IDOC_STATUS.

DATA : XLSDATA  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
DATA : XLSDATAB LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
*DATA:
*  GF_ZFHBLNO_COL TYPE I,
*  GF_ZFMBLNO_COL TYPE I,
*  GF_ZFBLDT_COL  TYPE I,
*  GF_ZFSHTY_COL  TYPE I,
*  GF_ZFVIA_COL   TYPE I,
*  GF_ZF20FT_COL  TYPE I,
*  GF_ZF40FT_COL  TYPE I,
*  GF_ZF45FT_COL  TYPE I,
*  GF_ZF40HQ_COL  TYPE I,
*  GF_ZFNEWT_COL  TYPE I,
*  GF_ZFNEWTM_COL TYPE I,
*  GF_ZFTOVL_COL  TYPE I,
*  GF_ZFTOVLM_COL TYPE I,
*  GF_ZFETD_COL   TYPE I,
*  GF_ZFETA_COL   TYPE I,
*  GF_ZFSPRT_COL  TYPE I,
*  GF_ZFAPRT_COL  TYPE I,
*  GF_ZFAPRTC_COL TYPE I,
*  GF_ZFSPRTC_COL TYPE I,
*  GF_ZFCIVNO_COL TYPE I,
*  GF_ZFCONT_COL  TYPE I,
*  GF_ZFPACK_COL  TYPE I,
*  GF_ZFPACKN_COL TYPE I,
*  GF_ZFTOWT_COL  TYPE I,
*  GF_ZFTOWTM_COL TYPE I.

*----------------------------------------------------------------------*
*    IDoc Variables.
*----------------------------------------------------------------------*
*DATA: W_EDIDC   LIKE EDIDC,
*      T_EDIDD   TYPE TABLE OF EDIDD WITH HEADER LINE.
*DATA  GL_IDOCNO TYPE EDIDC-DOCNUM.
*DATA:
*  FS_DELIVERY_HEADER  LIKE E1EDL20,  "Delivery Header
*  FS_DELIVERY_ITEM    LIKE E1EDL24,  "Delivery Item
*  FS_DEADLINE         LIKE E1EDT13,  "Deadline
*  FS_DELIVERY_CONTROL LIKE E1EDL18,  "Delivery Control
*  FS_IMPORT           LIKE E1EDL33,  "Import Data Delivery Header
*  FS_ROUTES           LIKE E1EDL28,  "Routes
*  FS_REFERENCE        LIKE E1EDL41,  "Item Reference
*  FS_HU_HEADER        LIKE E1EDL37,  "HU Header
*  FS_HU_ITEM          LIKE E1EDL44,  "HU Item
*  FS_JIT_CALL         LIKE E1EDL52.  "JIT Call

*----------------------------------------------------------------------*
*    Packing List Variables.
*----------------------------------------------------------------------*
*DATA:
*  BEGIN OF XLIST OCCURS 0,
*    EBELN  LIKE EKKO-EBELN,
*    EBELP  LIKE EKPO-EBELP,
*    MATNR  LIKE MARA-MATNR,
*    EXIDV2 LIKE VEKP-EXIDV2,
*    EXIDV  LIKE VEKP-EXIDV,
*    VHILM  LIKE VEKP-VHILM,
*    MAKTX  LIKE MAKT-MAKTX,
*    LFIMG  LIKE LIPS-LFIMG,
*    MEINS  LIKE LIPS-MEINS,
*    INHALT LIKE VEKP-INHALT,
*    PABNUM LIKE PABASN-PABNUM,
*    PABPOS LIKE PABASN-PABPOS,
*  END OF XLIST.
*DATA :
*  BEGIN OF XITEM OCCURS 0,
*    BSTNR LIKE LIPS-VGBEL,
*    POSEX LIKE LIPS-VGPOS,
*    KDMAT LIKE LIPS-KDMAT,
*    VEMEH LIKE VEPO-VEMEH,
*    VEMNG LIKE VEPO-VEMNG,
*  END OF XITEM.
*DATA :
*  BEGIN OF XPACK OCCURS 0,
*    EXIDV2 LIKE VEKP-EXIDV2,
*    EXIDV  LIKE VEKP-EXIDV,
*    VHILM  LIKE VEKP-VHILM,
*    POSNR  LIKE LIPS-POSNR,
*    VEMNG  LIKE VEPO-VEMNG,
*    VEMEH  LIKE VEPO-VEMEH,
*    INHALT LIKE VEKP-INHALT,
*  END OF XPACK.
*DATA :
*  BEGIN OF XJITCALL OCCURS 0,
*    POSNR  LIKE PABASN-POSNR,
*    PABNUM LIKE PABASN-PABNUM,
*    PABPOS LIKE PABASN-PABPOS,
*    PABAVM LIKE PABASN-PABAVM,
*  END OF XJITCALL.

DATA : GT_HEAD LIKE ZTBLHD_INF OCCURS 0 WITH HEADER LINE,
       GT_ITEM LIKE ZTBLIT_INF OCCURS 0 WITH HEADER LINE.

DATA :  G_ZEILE TYPE SY-INDEX.

* local file
DATA : BEGIN OF IT_LOCAL OCCURS 0,
        COL01(20),
        COL02(30),
        COL03(20),
        COL04(20),
        COL05(40),
        COL06(20),
        COL07(20),
        COL08(20),
        COL09(20),
        COL10(20),
        COL11(20),
        COL12(20),
        COL13(20),
        COL14(20),
        COL15(20),
        COL16(20),
        COL17(20),
        COL18(20),
        COL19(20),
        COL20(20),
       END OF IT_LOCAL.


*----------------------------------------------------------------------*
*   Selection Criteria                                                 *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK SND WITH FRAME
                          TITLE TEXT-SND.
PARAMETERS:
  PA_FNAME LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN END OF BLOCK SND.

INITIALIZATION.


*----------------------------------------------------------------------*
*   AT SELECTION SCREEN ON VALUE-REQUEST                               *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_FNAME.   "FILE UPLOAD
  PERFORM VALUE_LOCAL_FILE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT                                           *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT .
  CASE SY-TCODE.
    WHEN 'ZMMR30500T'.
      PERFORM MODIFY_SCREEN.
    WHEN OTHERS.

  ENDCASE.

START-OF-SELECTION.
  PERFORM IMPORT_LOCAL_FILE.
  PERFORM LOCAL_TO_XLS.
*  PERFORM SAVE_DATA.
  PERFORM CALL_BOL_CREATE.

END-OF-SELECTION.
  PERFORM SHOW_MESSAGES.
*&---------------------------------------------------------------------*
*&      Form  VALUE_LOCAL_FILE
*&---------------------------------------------------------------------*
*       Value Local File.
*----------------------------------------------------------------------*
FORM VALUE_LOCAL_FILE.

  CONSTANTS LC_MASK(20)   TYPE C VALUE ',*.*  ,*.*.'.

  DATA: L_DYNPREAD_T LIKE DYNPREAD OCCURS 0 WITH HEADER LINE,
        L_DYNAME     LIKE D020S-PROG,
        L_DYNUMB     LIKE D020S-DNUM.

  FIELD-SYMBOLS <LFS>.

  REFRESH L_DYNPREAD_T.
  CLEAR   L_DYNPREAD_T.
  GET CURSOR FIELD L_DYNPREAD_T-FIELDNAME.
  APPEND L_DYNPREAD_T.

  ASSIGN (L_DYNPREAD_T-FIELDNAME) TO <LFS>.
  MOVE    L_DYNPREAD_T-FIELDVALUE TO <LFS>.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = SPACE
            DEF_PATH         = <LFS>
            MASK             = LC_MASK
            MODE             = 'O'
            TITLE            = TEXT-FIL
       IMPORTING
            FILENAME         = <LFS>
       EXCEPTIONS
            INV_WINSYS       = 1
            NO_BATCH         = 2
            SELECTION_CANCEL = 3
            SELECTION_ERROR  = 4
            OTHERS           = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " VALUE_LOCAL_FILE

*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGES
*&---------------------------------------------------------------------*
*       Show Messages.
*----------------------------------------------------------------------*
FORM SHOW_MESSAGES.
  CALL FUNCTION 'MESSAGES_SHOW'
       EXPORTING
            OBJECT             = TEXT-LOG
            SHOW_LINNO         = SPACE
       EXCEPTIONS
            INCONSISTENT_RANGE = 1
            NO_MESSAGES        = 2.
  IF SY-SUBRC NE 0.
  ENDIF.
ENDFORM.                    " SHOW_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  import_local_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM IMPORT_LOCAL_FILE .
  DATA : L_FILENAME TYPE STRING.
  L_FILENAME = PA_FNAME.

  REFRESH IT_LOCAL.

  CALL FUNCTION 'GUI_UPLOAD'
       EXPORTING
            FILENAME                = L_FILENAME
            FILETYPE                = 'ASC'
            HAS_FIELD_SEPARATOR     = 'X'
       TABLES
            DATA_TAB                = IT_LOCAL
       EXCEPTIONS
            FILE_OPEN_ERROR         = 1
            FILE_READ_ERROR         = 2
            NO_BATCH                = 3
            GUI_REFUSE_FILETRANSFER = 4
            INVALID_TYPE            = 5
            NO_AUTHORITY            = 6
            UNKNOWN_ERROR           = 7
            BAD_DATA_FORMAT         = 8
            HEADER_NOT_ALLOWED      = 9
            SEPARATOR_NOT_ALLOWED   = 10
            HEADER_TOO_LONG         = 11
            UNKNOWN_DP_ERROR        = 12
            ACCESS_DENIED           = 13
            DP_OUT_OF_MEMORY        = 14
            DISK_FULL               = 15
            DP_TIMEOUT              = 16
            OTHERS                  = 17.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " import_local_file
*&---------------------------------------------------------------------*
*&      Form  local_to_xls
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM LOCAL_TO_XLS .
  DATA : LV_IDX TYPE ZESEQ.
  DATA :LV_DATE LIKE SY-DATUM,
        LV_DAY(2) TYPE N,
        LV_MONTH(2) TYPE N,
        LV_YEAR(4) TYPE N.

    DELETE IT_LOCAL WHERE COL01 EQ ''.

    READ TABLE IT_LOCAL INDEX 5.

*  CALL FUNCTION 'ZIM_NUMBER_GET_NEXT'
*       EXPORTING
*            ZFREQTY         = 'TB'
*       IMPORTING
*            ZFREQNO         = GT_HEAD-ZFBLNO
*       EXCEPTIONS
*            NOT_INPUT       = 1
*            NOT_TYPE        = 2
*            NOT_RANGE       = 3
*            NOT_FOUND       = 4
*            LOCKED          = 6
*            ERROR_DUPLICATE = 8.

  GT_HEAD-ZFHBLNO = IT_LOCAL-COL01.
  GT_HEAD-ZFMBLNO = IT_LOCAL-COL02.
  LV_MONTH        = IT_LOCAL-COL03(2).
  LV_YEAR         = IT_LOCAL-COL03+6(4).
  LV_DAY          = IT_LOCAL-COL03+3(2).
  CONCATENATE LV_YEAR LV_MONTH LV_DAY INTO GT_HEAD-ZFBLDT.
*  GT_HEAD-ZFBLDT  = IT_LOCAL-COL03.
  GT_HEAD-ZFSHTY  = IT_LOCAL-COL04.
  GT_HEAD-ZFVIA   = IT_LOCAL-COL05.
  GT_HEAD-ZFVSL   = IT_LOCAL-COL06.
  GT_HEAD-ZF20FT  = IT_LOCAL-COL07.
  GT_HEAD-ZF40FT  = IT_LOCAL-COL08.
  GT_HEAD-ZF45FT  = IT_LOCAL-COL09.
  GT_HEAD-ZF40HQ  = IT_LOCAL-COL10.
  GT_HEAD-ZFNEWT  = IT_LOCAL-COL11.
  GT_HEAD-ZFNEWTM = IT_LOCAL-COL12.
  GT_HEAD-ZFTOVL  = IT_LOCAL-COL13.
  GT_HEAD-ZFTOVLM = IT_LOCAL-COL14.
  LV_MONTH        = IT_LOCAL-COL15(2).
  LV_YEAR         = IT_LOCAL-COL15+6(4).
  LV_DAY          = IT_LOCAL-COL15+3(2).
  CONCATENATE LV_YEAR LV_MONTH LV_DAY INTO GT_HEAD-ZFETD.
*  GT_HEAD-ZFETD   = IT_LOCAL-COL15.
  LV_MONTH        = IT_LOCAL-COL16(2).
  LV_YEAR         = IT_LOCAL-COL16+6(4).
  LV_DAY          = IT_LOCAL-COL16+3(2).
  CONCATENATE LV_YEAR LV_MONTH LV_DAY INTO GT_HEAD-ZFETA.
*  GT_HEAD-ZFETA   = IT_LOCAL-COL16.
  GT_HEAD-ZFSPRT  = IT_LOCAL-COL17.
  GT_HEAD-ZFAPRT  = IT_LOCAL-COL18.
  GT_HEAD-ZFAPRTC = IT_LOCAL-COL19.
  GT_HEAD-ZFSPRTC = IT_LOCAL-COL20.
  GT_HEAD-ZUSER	= SY-UNAME.	
  GT_HEAD-ZEDAT	= SY-DATUM.
  GT_HEAD-ZETIM	= SY-UZEIT.
*  GT_HEAD-ZSDAT	=		
*  GT_HEAD-ZSTIM	=		
*  GT_HEAD-ZMODE	=		
*  GT_HEAD-ZRESULT	=		
*  GT_HEAD-ZMSG	=		
*  GT_HEAD-ZZRET	=		
  APPEND GT_HEAD.

  LOOP AT IT_LOCAL FROM 10.
    LV_IDX = LV_IDX + 1.
    GT_ITEM-ZFBLNO  = GT_HEAD-ZFBLNO.
    GT_ITEM-ZFSEQ   = LV_IDX.
    GT_ITEM-ZFHBLNO = GT_HEAD-ZFHBLNO.
    GT_ITEM-ZFCIVNO = IT_LOCAL-COL01.
    GT_ITEM-ZFCONT  = IT_LOCAL-COL02.
    GT_ITEM-ZFPACK  = IT_LOCAL-COL03.
    GT_ITEM-ZFPACKN = IT_LOCAL-COL04.
    GT_ITEM-ZFTOWT  = IT_LOCAL-COL05.
    GT_ITEM-ZFTOWTM = IT_LOCAL-COL06.
    GT_ITEM-ZUSER	= SY-UNAME.	
    GT_ITEM-ZEDAT	= SY-DATUM.
    GT_ITEM-ZETIM	= SY-UZEIT.
    APPEND GT_ITEM.
  ENDLOOP.

ENDFORM.                    " local_to_xls

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .

  LOOP AT SCREEN .
    IF SCREEN-GROUP1 EQ 'NOI' .
      SCREEN-INPUT        = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.

*  IF NOT GT_HEAD[] IS INITIAL.
*    MODIFY ZTBLHD_INF FROM TABLE GT_HEAD.
*  ENDIF.
*
*  IF NOT GT_ITEM[] IS INITIAL.
*    MODIFY ZTBLIT_INF FROM TABLE GT_ITEM.
*  ENDIF.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_BOL_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_BOL_CREATE.
  DATA : LT_HEAD LIKE ZSBLHD_INF OCCURS 0 WITH HEADER LINE,
         LT_ITEM LIKE ZSBLIT_INF OCCURS 0 WITH HEADER LINE.

  CLEAR : LT_HEAD, LT_ITEM, LT_ITEM[].

  MOVE-CORRESPONDING GT_HEAD TO LT_HEAD.
  APPEND LT_HEAD.


  LOOP AT GT_ITEM.
    MOVE-CORRESPONDING GT_ITEM TO LT_ITEM.
    APPEND LT_ITEM.
  ENDLOOP.

  CALL FUNCTION 'ZIM_WEB_TO_SAP_BL'
       TABLES
            IT_ZSBLHD = LT_HEAD
            IT_ZSBLIT = LT_ITEM.

ENDFORM.                    " CALL_BOL_CREATE
