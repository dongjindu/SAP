*----------------------------------------------------------------------
* Program ID        : ZM_IF_YM_1005_ASN
* Title             : [MM] GCCS Interface - ASN Details
* Created on        : 10/17/2007
* Created by        : Rakesh Gandhi
* Specifications By : Crossley, Ron
* Description       : GCCS Interface - ASN Details
*----------------------------------------------------------------------
REPORT ZM_IF_YM_1005_ASN MESSAGE-ID ZMCO.

TYPE-POOLS : SLIS,
             ICON.

TABLES: MARA,
        LIKP,
        ZTPP_PP_LOG_HEAD,
        ZTPP_PP_LOG_DETA,
        ZTMM_KD_ASN_MAIN,
        VBUK.
*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
TYPES: BEGIN OF TY_ASN_DETAIL.
INCLUDE TYPE ZTMM_EAI_ASN.
TYPES: END OF TY_ASN_DETAIL.

TYPES: BEGIN OF TY_ALV.
INCLUDE  TYPE TY_ASN_DETAIL.
TYPES : CHKBOX(1),
        ICON TYPE ICON_D,
        TABCOLOR  TYPE SLIS_T_SPECIALCOL_ALV.
TYPES: END OF TY_ALV.

DATA: BEGIN OF T_DELIVERY OCCURS 0,
        TRAID     LIKE LIKP-TRAID                , " Container No
*        ZZKDWEBPO LIKE ZTMM_KD_ASN_MAIN-ZZKDWEBPO, " KDWEB PO NUMBER
*        EMATN     LIKE ZTMM_KD_ASN_MAIN-EMATN    , " Case Number
        MATNR     LIKE LIPS-MATNR                , " Material No
        BORGR_GRP LIKE LIKP-BORGR_GRP            , " Serial no.
        VBELN     LIKE LIKP-VBELN                , " Delivery No
        ZINVOICE  LIKE ZTMM_KD_ASN_MAIN-ZINVOICE , " Invoice Number
        VGBEL     LIKE LIPS-VGBEL                , " Doc No of ref doc
        VGPOS     LIKE LIPS-VGPOS                , " Item No of ref item
** Added by Furong on 05/12/08; UD1K943597
        LFIMG LIKE LIPS-LFIMG,
        KDMAT LIKE LIPS-KDMAT,
** End of change
      END OF T_DELIVERY,

      BEGIN OF T_ZTBL OCCURS 0           ,
        ZFCONT   LIKE ZTBLIT_INF-ZFHBLNO ,
        ZFHBLNO   LIKE ZTBLIT_INF-ZFHBLNO,
        ZF40FT    LIKE ZTBLHD_INF-ZF40FT ,  " Container Type
        ZFSPRTC   LIKE ZTBLHD_INF-ZFSPRTC,  " Port
        LIFNR     LIKE ZTBL-LIFNR        ,  " Shipper
        ZFETA     LIKE ZTBLHD_INF-ZFETA  ,  " ETA
        ZFVSL     LIKE ZTBLHD_INF-ZFVSL  ,  " Vessel Name
        BLMENGE   LIKE ZTBLIT-BLMENGE    ,  " Loading Qty
        MATNR     LIKE ZTBLIT-MATNR      ,  " Material No
        ZFETD     LIKE ZTBLHD_INF-ZFETD  ,  " ETD
** Added by Furong on 05/12/08; UD1K943597
*        ZMSG(16),
** End of change
      END OF T_ZTBL                      .

DATA: IT_ASN_DETAIL TYPE TABLE OF TY_ASN_DETAIL WITH HEADER LINE,
      GT_ALV        TYPE TABLE OF TY_ALV     WITH HEADER LINE   ,
      GT_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV                    .

DATA : GS_LAYOUT   TYPE SLIS_LAYOUT_ALV  ,
       GS_VARIANT  TYPE DISVARIANT       ,
       GS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

DATA: GV_TEXT(20) TYPE C         ,
      GV_ERROR(1) TYPE C VALUE '',
      GV_REPID    LIKE SY-REPID  ,
      GV_CNT      LIKE ZTPP_PP_LOG_DETA-SEQUENCE,
      GV_NUMBERT   LIKE ZTPP_PP_LOG_HEAD-LOGKEY ,
      GV_LINES TYPE I                           .

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_MATNR FOR MARA-MATNR,
                S_VBELN FOR LIKP-VBELN,
                S_LFDAT FOR LIKP-LFDAT,
                S_ERDAT FOR LIKP-ERDAT,
                S_WADAT FOR LIKP-WADAT_IST,
                S_INV FOR ZTMM_KD_ASN_MAIN-ZINVOICE,
                S_TRAID FOR LIKP-TRAID,
                S_WBSTK FOR VBUK-WBSTK.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETER: P_OPT1 RADIOBUTTON GROUP G1 DEFAULT 'X',
           P_OPT2 RADIOBUTTON GROUP G1,
           P_OPT3 RADIOBUTTON GROUP G1,
           P_OPT4 RADIOBUTTON GROUP G1.
PARAMETERS P_DATUM LIKE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME.
PARAMETERS P_DEST LIKE RFCDES-RFCDEST
                       DEFAULT 'WMRM01'.
SELECTION-SCREEN END OF BLOCK B4.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  SY-TITLE = '[MM] GCCS Interface - ASN Details'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*----------------------------------------------------------------------*
* START OF SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF P_OPT1 = 'X'.      " Display current data.
    PERFORM GET_DATA_FROM_ZTABLE.
  ELSEIF P_OPT2 = 'X'.  " Refresh Data
    PERFORM SAVE_DATA.
  ELSEIF P_OPT3 = 'X'.  " Refresh and send data
    PERFORM SAVE_DATA.
    PERFORM SEND_DATA_TO_EAI.
  ELSEIF P_OPT4 = 'X'.  " Send existing data
    PERFORM SEND_DATA_TO_EAI.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  alv_variant_f4
*&---------------------------------------------------------------------*
*       F4 help for ALV Variant
*----------------------------------------------------------------------*
*      <--P_P_VARI  text
*----------------------------------------------------------------------*
FORM ALV_VARIANT_F4 CHANGING P_VARI.
  DATA: RS_VARIANT LIKE DISVARIANT.

  CLEAR RS_VARIANT.
  RS_VARIANT-REPORT   = SY-REPID.
  RS_VARIANT-USERNAME = SY-UNAME.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT = RS_VARIANT
            I_SAVE     = 'A'
       IMPORTING
            ES_VARIANT = RS_VARIANT
       EXCEPTIONS
            OTHERS     = 1.

  IF SY-SUBRC = 0.
    P_VARI = RS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.                    " alv_variant_f4
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       Subroutine to delete existing data and Save new data
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: LV_YY(4)  TYPE C,
        LV_MON(2) TYPE C,
        LV_DAY(2) TYPE C,
        LV_DT(8)  TYPE C.

  CLEAR GV_TEXT.
  GV_TEXT = 'Refreshing data...'.
  PERFORM SHOW_PROGRESS USING GV_TEXT.

  DELETE FROM ZTMM_EAI_ASN
** Changed by Furong on 03/13/08
*    WHERE EPART_NO IN S_MATNR AND
*          EINVO_NO IN S_VBELN AND
*          EINVO_NO IN S_INV   AND
*          ECONT_NO IN S_TRAID AND
*          TAIT_TARG_D EQ P_DATUM.
    WHERE EPART_NO IN S_MATNR AND
          EINBD_NO IN S_VBELN AND
          EINVO_NO IN S_INV   AND
          ECONT_NO IN S_TRAID AND
          TAIT_TARG_D EQ P_DATUM.
** End of change

  GV_TEXT = 'Saving data...'.
  PERFORM SHOW_PROGRESS USING GV_TEXT.

** Changed by Furong on 03/06/08  "UD1K943021

*  SELECT a~traid
*         c~zzkdwebpo
*         c~ematn
*         b~matnr
*         a~borgr_grp
*         a~vbeln
*         c~zinvoice
*         b~vgbel
*         b~vgpos
*         INTO TABLE t_delivery
*         FROM likp AS a
*         INNER JOIN ztmm_kd_asn_main AS c
*         ON a~traid EQ c~traid
*         INNER JOIN lips AS b
*         ON a~vbeln EQ b~vbeln AND
*            c~matnr EQ b~matnr AND
*            c~ebeln EQ b~vgbel
*         WHERE a~vbeln IN s_vbeln AND
*               a~lfdat IN s_lfdat AND
*               a~traid IN s_traid AND
*               b~matnr IN s_matnr AND
*               a~erdat IN s_erdat AND
*               a~wadat_ist IN s_wadat AND
*               a~lfart =  'EL'    AND
*               c~zinvoice IN s_inv.

* Changed on 06/11/08 by Furong "UD1K943847

*  SELECT A~TRAID
*        C~ZZKDWEBPO
*        C~EMATN
*        B~MATNR
*        A~BORGR_GRP
*        A~VBELN
*        C~ZINVOICE
*        B~VGBEL
*        B~VGPOS
*** Changed by Furong on 05/12/08; UD1K943597
*        B~LFIMG
*** End fo change on 05/12/08
*        INTO TABLE T_DELIVERY
*        FROM LIKP AS A INNER JOIN VBUK AS V
*                 ON  A~VBELN = V~VBELN
*        INNER JOIN ZTMM_KD_ASN_MAIN AS C
*        ON A~TRAID EQ C~TRAID
*        INNER JOIN LIPS AS B
*        ON A~VBELN EQ B~VBELN AND
*           C~MATNR EQ B~MATNR AND
*           C~EBELN EQ B~VGBEL
*        WHERE A~VBELN IN S_VBELN AND
*              A~LFDAT IN S_LFDAT AND
*              A~TRAID IN S_TRAID AND
*              B~MATNR IN S_MATNR AND
*              A~ERDAT IN S_ERDAT AND
*              A~ZZARRDT IN S_WADAT AND
*              A~LFART =  'EL'    AND
*              C~ZINVOICE IN S_INV AND
*              V~WBSTK IN S_WBSTK.
** End of change on 03/06/08 by Furong

  SELECT A~TRAID
         B~MATNR
         A~BORGR_GRP
         A~VBELN
         A~BOLNR
         B~VGBEL
         B~VGPOS
         B~LFIMG
         B~KDMAT
         INTO TABLE T_DELIVERY
         FROM LIKP AS A INNER JOIN VBUK AS V
                  ON  A~VBELN = V~VBELN
         INNER JOIN LIPS AS B
         ON A~VBELN EQ B~VBELN
         WHERE A~VBELN IN S_VBELN AND
               A~LFDAT IN S_LFDAT AND
               A~TRAID IN S_TRAID AND
               B~MATNR IN S_MATNR AND
               A~ERDAT IN S_ERDAT AND
               A~ZZARRDT IN S_WADAT AND
               A~LFART =  'EL'    AND
               A~BOLNR IN S_INV AND
               V~WBSTK IN S_WBSTK.

*  SORT T_DELIVERY BY TRAID ZZKDWEBPO EMATN MATNR.
*    DELETE ADJACENT DUPLICATES FROM T_DELIVERY COMPARING
*                             TRAID ZZKDWEBPO EMATN MATNR.

  SORT T_DELIVERY BY TRAID KDMAT MATNR.
  DELETE ADJACENT DUPLICATES FROM T_DELIVERY COMPARING
                             TRAID KDMAT MATNR.
** End fo change on 06/11/08 "UD1K943847

  IF NOT T_DELIVERY[] IS INITIAL.

    SELECT C~ZFCONT
           C~ZFHBLNO
           D~ZF40FT
           D~ZFSPRTC
           A~LIFNR
           D~ZFETA
           D~ZFVSL
           B~BLMENGE
           B~MATNR
           D~ZFETD
** Changed by Furong on 05/12/08; UD1K943597
*           C~ZMSG
** End of change on 05/12/08
           INTO TABLE T_ZTBL
                 FROM ZTBL AS A INNER JOIN ZTBLIT AS B
                 ON  A~MANDT  EQ B~MANDT
                 AND A~ZFBLNO EQ B~ZFBLNO
                 INNER JOIN ZTBLIT_INF AS C
                 ON A~ZFHBLNO EQ C~ZFHBLNO
                 INNER JOIN ZTBLHD_INF AS D
                 ON C~ZFBLNO EQ D~ZFBLNO
                 FOR ALL ENTRIES IN T_DELIVERY
                    WHERE B~MATNR EQ T_DELIVERY-MATNR      AND
                          B~EBELN EQ T_DELIVERY-VGBEL      AND
                          B~EBELP EQ T_DELIVERY-VGPOS+1(5) AND
                          C~ZFCONT EQ T_DELIVERY-TRAID+0(11).
** Changed by Furong on 05/15/08; UD1K943597
*    IF NOT T_ZTBL[] IS INITIAL.
** End of change
    SORT T_ZTBL BY ZFCONT MATNR.
    LOOP AT T_DELIVERY.
      IT_ASN_DETAIL-TAIT_TARG_D    = SY-DATUM.
      IT_ASN_DETAIL-ECONT_NO       = T_DELIVERY-TRAID.

** Changed on 06/11/08 by Furong "UD1K943847
*        IT_ASN_DETAIL-EORDER_NO      = T_DELIVERY-ZZKDWEBPO.
*        IT_ASN_DETAIL-ECASE_NO       = T_DELIVERY-EMATN.
      IT_ASN_DETAIL-EORDER_NO      = T_DELIVERY-KDMAT+0(10).
      IT_ASN_DETAIL-ECASE_NO       = T_DELIVERY-KDMAT+10(6).
** End fo change on 06/11/08 "UD1K943847

      IT_ASN_DETAIL-EPART_NO       = T_DELIVERY-MATNR.
      IT_ASN_DETAIL-EINVO_NO       = T_DELIVERY-ZINVOICE.
      IT_ASN_DETAIL-ESEAL_NO       = T_DELIVERY-BORGR_GRP.
      IT_ASN_DETAIL-EINBD_NO       = T_DELIVERY-VBELN.
      IT_ASN_DETAIL-TAIT_TARG_T    = SY-UZEIT.
      IT_ASN_DETAIL-TAIT_TARG_RSLT = ' '.
      IT_ASN_DETAIL-TAIT_TARG_DESC = ' '.
      IT_ASN_DETAIL-TAIT_EVENT_C   = 'I'.
** Changed by Furong on 05/12/08; UD1K943597
      IT_ASN_DETAIL-EQTY           = T_DELIVERY-LFIMG.
** End of change on 05/12/08
      READ TABLE T_ZTBL WITH KEY ZFCONT = T_DELIVERY-TRAID
                                 MATNR  = T_DELIVERY-MATNR.
      IF SY-SUBRC = 0.
        IF T_ZTBL-ZFHBLNO IS INITIAL.
          IT_ASN_DETAIL-EBL_NO = 'BOL Error'.
*            IT_ASN_DETAIL-EBL_NO   = T_ZTBL-ZMSG.
        ELSE.
          IT_ASN_DETAIL-EBL_NO   = T_ZTBL-ZFHBLNO.
        ENDIF.
        IT_ASN_DETAIL-ECONT_TP = T_ZTBL-ZF40FT.
        IT_ASN_DETAIL-EPORT    = T_ZTBL-ZFSPRTC.
        IT_ASN_DETAIL-ESHIPPER = T_ZTBL-LIFNR.
        IT_ASN_DETAIL-EVESSEL  = T_ZTBL-ZFVSL.
** Changed by Furong on 05/12/08; UD1K943597
*          IT_ASN_DETAIL-EQTY     = T_ZTBL-BLMENGE.
** End of change on 05/12/08
        CLEAR: LV_YY,
               LV_MON,
               LV_DAY,
               LV_DT.
        LV_YY =  T_ZTBL-ZFETA(4).
        LV_MON = T_ZTBL-ZFETA+4(2).
        LV_DAY = T_ZTBL-ZFETA+6(2).

        CONCATENATE LV_YY LV_MON LV_DAY INTO LV_DT.
        WRITE LV_DT TO IT_ASN_DETAIL-EETA_DT.

        CLEAR: LV_YY,
               LV_MON,
               LV_DAY,
               LV_DT.
        LV_YY =  T_ZTBL-ZFETD(4).
        LV_MON = T_ZTBL-ZFETD+4(2).
        LV_DAY = T_ZTBL-ZFETD+6(2).

        CONCATENATE LV_YY LV_MON LV_DAY INTO LV_DT.
        WRITE LV_DT TO IT_ASN_DETAIL-E_ETD..
** Changed by Furong on 05/15/08; UD1K943597
      ELSE.
        IT_ASN_DETAIL-EBL_NO = 'BOL Error'.
** End of change
      ENDIF.    " IF sy-subrc = 0
      APPEND IT_ASN_DETAIL.
      CLEAR IT_ASN_DETAIL.
    ENDLOOP.
    SORT IT_ASN_DETAIL BY TAIT_TARG_D ECONT_NO EBL_NO
                         EORDER_NO ECASE_NO EPART_NO.

    DELETE ADJACENT DUPLICATES FROM IT_ASN_DETAIL COMPARING
            TAIT_TARG_D ECONT_NO EBL_NO EORDER_NO ECASE_NO EPART_NO.
    CLEAR GV_LINES.
    DESCRIBE TABLE IT_ASN_DETAIL LINES GV_LINES.
    INSERT ZTMM_EAI_ASN FROM TABLE IT_ASN_DETAIL.
    IF SY-SUBRC = 0.
      MESSAGE S000 WITH GV_LINES TEXT-003.
      IF P_OPT2 = 'X'.
        PERFORM DISPLAY_ALV_DATA.
      ENDIF.
    ELSE.
      MESSAGE S000 WITH TEXT-004.
    ENDIF.
  ELSE.
    MESSAGE S000 WITH TEXT-005.
    GV_ERROR = '1'.
    EXIT.
  ENDIF.      " IF NOT t_ztbl[] IS INITIAL
** Changed by Furong on 05/15/08; UD1K943597
*  ELSE.
*    MESSAGE S000 WITH TEXT-005.
*    GV_ERROR = '1'.
*    EXIT.
*  ENDIF.
** End of change
ENDFORM.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       Subroutine to show progress
*----------------------------------------------------------------------*
*      -->P_gv_text  text
*----------------------------------------------------------------------*
FORM SHOW_PROGRESS USING    P_TEXT.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
*            PERCENTAGE =
            TEXT       = P_TEXT.

ENDFORM.                    " show_progress
*&---------------------------------------------------------------------*
*&      Form  get_data_from_ztable
*&---------------------------------------------------------------------*
*       Subroutine to get existing data from Z table
*----------------------------------------------------------------------*
FORM GET_DATA_FROM_ZTABLE.

  DATA: LV_YY(4)  TYPE C,
        LV_MON(2) TYPE C,
        LV_DAY(2) TYPE C,
        LV_DT(8)  TYPE C.

  REFRESH: IT_ASN_DETAIL.
  CLEAR  : IT_ASN_DETAIL.
  SELECT * FROM ZTMM_EAI_ASN
           INTO TABLE IT_ASN_DETAIL
           WHERE EPART_NO IN S_MATNR AND
                 EINBD_NO IN S_VBELN AND
                 EINVO_NO IN S_INV   AND
                 ECONT_NO IN S_TRAID AND
                 TAIT_TARG_D EQ P_DATUM.
  IF SY-SUBRC NE 0.
    MESSAGE S000 WITH TEXT-005.
    GV_ERROR = '1'.
    EXIT.
  ENDIF.

  CHECK : GV_ERROR EQ SPACE.
  GV_TEXT = 'Preparing output...'.
  PERFORM SHOW_PROGRESS USING GV_TEXT.

  REFRESH GT_ALV.
  CLEAR   GT_ALV.

  LOOP AT IT_ASN_DETAIL.
    MOVE-CORRESPONDING IT_ASN_DETAIL TO GT_ALV.
    CLEAR: LV_YY,
           LV_MON,
           LV_DAY,
           LV_DT.

    LV_YY =  IT_ASN_DETAIL-EETA_DT(4).
    LV_MON = IT_ASN_DETAIL-EETA_DT+4(2).
    LV_DAY = IT_ASN_DETAIL-EETA_DT+6(2).

    CONCATENATE LV_YY LV_MON LV_DAY INTO LV_DT.
    WRITE LV_DT TO GT_ALV-EETA_DT.

    CLEAR: LV_YY,
           LV_MON,
           LV_DAY,
           LV_DT.

    LV_YY =  IT_ASN_DETAIL-E_ETD(4).
    LV_MON = IT_ASN_DETAIL-E_ETD+4(2).
    LV_DAY = IT_ASN_DETAIL-E_ETD+6(2).

    CONCATENATE LV_YY LV_MON LV_DAY INTO LV_DT.
    WRITE LV_DT TO GT_ALV-E_ETD.

    CASE IT_ASN_DETAIL-TAIT_TARG_RSLT.
      WHEN SPACE.
      WHEN 'S'.
        GT_ALV-ICON = ICON_LED_GREEN.
      WHEN 'F'.
        GT_ALV-ICON = ICON_LED_RED.
    ENDCASE.
    APPEND GT_ALV.
    CLEAR  GT_ALV.
  ENDLOOP.
  CLEAR GV_LINES.
  DESCRIBE TABLE  GT_ALV LINES GV_LINES.
  IF GV_LINES = 1.
    MESSAGE S000 WITH GV_LINES TEXT-011.
  ELSEIF GV_LINES > 1.
    MESSAGE S000 WITH GV_LINES TEXT-010.
  ENDIF.
  PERFORM DISPLAY_ALV_DATA.
ENDFORM.                    " get_data_from_ztable
*&---------------------------------------------------------------------*
*&      Form  display_alv_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_DATA.

  IF P_OPT2 = 'X'.
    REFRESH GT_ALV.
    CLEAR   GT_ALV.

    LOOP AT IT_ASN_DETAIL.
      MOVE-CORRESPONDING IT_ASN_DETAIL TO GT_ALV.
      CASE IT_ASN_DETAIL-TAIT_TARG_RSLT.
        WHEN SPACE.
        WHEN 'S'.
          GT_ALV-ICON = ICON_LED_GREEN.
        WHEN 'F'.
          GT_ALV-ICON = ICON_LED_RED.
      ENDCASE.
      APPEND GT_ALV.
      CLEAR  GT_ALV.
    ENDLOOP.

  ENDIF.
  PERFORM SET_LAYOUT.
  CLEAR GS_VARIANT.

  GV_REPID = SY-REPID.
  GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-VARIANT = P_VARI.

  PERFORM ALV_FIELDCAT.

  PERFORM ALV_GRID_DISPLAY.

ENDFORM.                    " display_alv_data
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       Set Layout
*----------------------------------------------------------------------*
FORM SET_LAYOUT.
  CLEAR GS_LAYOUT.
  GS_LAYOUT-COLWIDTH_OPTIMIZE      = 'X'.
  GS_LAYOUT-BOX_FIELDNAME          = 'CHKBOX'.
  GS_LAYOUT-COLTAB_FIELDNAME       = 'TABCOLOR'.
ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat
*&---------------------------------------------------------------------*
*       Prepare ALV fieldcatalogue
*----------------------------------------------------------------------*
FORM ALV_FIELDCAT.
  DATA: L_POS TYPE I.
  CLEAR:  GS_FIELDCAT,
          GT_FIELDCAT.
  REFRESH GT_FIELDCAT.

  DEFINE APPEND_FIELDCAT.
    L_POS = L_POS + 1.
    CLEAR GS_FIELDCAT.
    GS_FIELDCAT-COL_POS       = L_POS.
    GS_FIELDCAT-KEY           = &1.
    GS_FIELDCAT-FIELDNAME     = &2.
    GS_FIELDCAT-SELTEXT_M     = &3.        " Column heading
    GS_FIELDCAT-OUTPUTLEN     = &4.        " Column width
    GS_FIELDCAT-DATATYPE      = &5.        " Data type
    GS_FIELDCAT-QFIELDNAME    = &6.
    APPEND GS_FIELDCAT TO GT_FIELDCAT.
  END-OF-DEFINITION.

  APPEND_FIELDCAT :

          'X'  'ECONT_NO'  'Container  No'        11 'CHAR' ' ',
          'X'  'EBL_NO'    'B/L No'               16 'CHAR' ' ',
          'X'  'EORDER_NO' 'Order No (SU1)'       10 'CHAR' ' ',
          'X'  'ECASE_NO'  'Case No (SU2)'         6 'CHAR' ' ',
          'X'  'EPART_NO'  'PART NO'              15 'CHAR' ' ',
          ' '  'ECONT_TP'  'Container Type'       10 'CHAR' ' ',
          ' '  'EPORT'     'Port'                 10 'CHAR' ' ',
          ' '  'ESHIPPER'  'Shipper'              10 'CHAR' ' ',
          ' '  'EINVO_NO'  'Invoice NO'           16 'CHAR' ' ',
          ' '  'EETA_DT'   'ETA'                   8 'CHAR' ' ',
          ' '  'EVESSEL'   'vessel Name'          20 'CHAR' ' ',
          ' '  'EQTY'      'Loading Qty'          10 'NUMC' ' ',
          ' '  'EINBD_NO'  'Inbound delivery No'  10 'CHAR' ' ',
          ' '  'E_ETD'     'ETD'                   8 'CHAR' ' ',
          ' '  'ESEAL_NO'  'Container seal No'     7 'CHAR' ' ',
          ' '  'ICON'      'flg'                   3 'ICON' ' '.

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'EQTY'.
        GS_FIELDCAT-JUST = 'R'.
    ENDCASE.
    GS_FIELDCAT-REF_TABNAME = 'ZTMM_EAI_ASN'.
    GS_FIELDCAT-REF_FIELDNAME = GS_FIELDCAT-FIELDNAME.
    MODIFY GT_FIELDCAT FROM GS_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " alv_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_grid_display
*&---------------------------------------------------------------------*
*       Subroutine to display data in ALV Grid form
*----------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY.
  DATA: LV_SAVE VALUE 'A'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
*            i_callback_pf_status_set = ' '
*            i_callback_user_command  = ' '
            IS_LAYOUT                = GS_LAYOUT
*            it_excluding             =
            IT_FIELDCAT              = GT_FIELDCAT
*            it_special_groups        =
*            it_sort                  =
            I_SAVE                   = LV_SAVE
            IS_VARIANT               = GS_VARIANT
*            it_events                =
       TABLES
            T_OUTTAB                 = GT_ALV
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

ENDFORM.                    " alv_grid_display
*&---------------------------------------------------------------------*
*&      Form  send_data_to_eai
*&---------------------------------------------------------------------*
*       Subroutine to call RFC to pass data to WebMethods
*----------------------------------------------------------------------*
FORM SEND_DATA_TO_EAI.
  DATA: LV_YY(4)  TYPE C,
        LV_MON(2) TYPE C,
        LV_DAY(2) TYPE C,
        LV_DT(8)  TYPE C.

  IF P_OPT4 = 'X'.
    REFRESH: IT_ASN_DETAIL.
    CLEAR  : IT_ASN_DETAIL.
    SELECT * FROM ZTMM_EAI_ASN
             INTO TABLE IT_ASN_DETAIL

** Changed by Furong on 05/12/08; UD1K943597
*             WHERE EPART_NO IN S_MATNR AND
*                   EINVO_NO IN S_VBELN AND
*                   EPART_NO IN S_INV   AND
*                   ECONT_NO IN S_TRAID AND
*                   TAIT_TARG_D EQ P_DATUM.

            WHERE EPART_NO IN S_MATNR AND
                  EINBD_NO IN S_VBELN AND
                  EINVO_NO IN S_INV   AND
                  ECONT_NO IN S_TRAID AND
                  TAIT_TARG_D EQ P_DATUM.
** End of change

    IF SY-SUBRC NE 0.
      MESSAGE S000 WITH TEXT-005.
      GV_ERROR = '1'.
      EXIT.
    ELSE.
      LOOP AT IT_ASN_DETAIL.
        CLEAR: LV_YY,
               LV_MON,
               LV_DAY,
               LV_DT.
        LV_YY =  IT_ASN_DETAIL-EETA_DT(4).
        LV_MON = IT_ASN_DETAIL-EETA_DT+4(2).
        LV_DAY = IT_ASN_DETAIL-EETA_DT+6(2).

        CONCATENATE LV_YY LV_MON LV_DAY INTO LV_DT.
        WRITE LV_DT TO IT_ASN_DETAIL-EETA_DT.

        CLEAR: LV_YY,
               LV_MON,
               LV_DAY,
               LV_DT.
        LV_YY =  IT_ASN_DETAIL-E_ETD(4).
        LV_MON = IT_ASN_DETAIL-E_ETD+4(2).
        LV_DAY = IT_ASN_DETAIL-E_ETD+6(2).

        CONCATENATE LV_YY LV_MON LV_DAY INTO LV_DT.
        WRITE LV_DT TO IT_ASN_DETAIL-E_ETD.

        MODIFY IT_ASN_DETAIL INDEX SY-TABIX TRANSPORTING EETA_DT E_ETD.
      ENDLOOP.
    ENDIF.
  ENDIF.        " IF p_opt4 = 'X'.

  IF NOT IT_ASN_DETAIL[] IS INITIAL.
    DATA: L_MSGTXT(100) TYPE C,
          L_SIZE      TYPE NUM9.

    CLEAR GV_ERROR.

    CALL FUNCTION 'Z_GCS_EAI_ASN_DETAIL'
    DESTINATION P_DEST
      TABLES
          EAI_ASN_DETAIL       = IT_ASN_DETAIL
      EXCEPTIONS
          NO_DATA_FOUND        = 1
          OTHERS               = 2.

    IF SY-SUBRC <> 0.
      LOOP AT IT_ASN_DETAIL.
        IT_ASN_DETAIL-TAIT_TARG_RSLT = 'F'.
        MODIFY IT_ASN_DETAIL INDEX SY-TABIX TRANSPORTING TAIT_TARG_RSLT.
      ENDLOOP.
      MESSAGE S000 WITH TEXT-008.
    ELSE.
      LOOP AT IT_ASN_DETAIL.
        IT_ASN_DETAIL-TAIT_TARG_RSLT = 'S'.
        MODIFY IT_ASN_DETAIL INDEX SY-TABIX TRANSPORTING TAIT_TARG_RSLT.
      ENDLOOP.
      CLEAR GV_LINES.
      DESCRIBE TABLE IT_ASN_DETAIL LINES GV_LINES.

      MESSAGE S000 WITH GV_LINES TEXT-009.
    ENDIF.
    UPDATE ZTMM_EAI_ASN FROM TABLE IT_ASN_DETAIL.
  ENDIF.    " IF NOT it_asn_detail[] IS INITIAL
ENDFORM.                    " send_data_to_eai
*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       Subroutine to write EAI Trasfer log
*----------------------------------------------------------------------*
*      -->P_1081   text
*      -->P_1      text
*      -->P_TEXT_006  text
*      -->P_L_MSGTXT  text
*----------------------------------------------------------------------*
FORM CREATE_LOG USING    PA_TYPE  PA_STEP  PA_TEXT  PA_KEY .
  GV_CNT = GV_CNT + 1 .
  IF GV_CNT = 1       .
    PERFORM GET_LOGSERIAL.               " Log Number Generation........
    ZTPP_PP_LOG_HEAD-LOGKEY   = GV_NUMBERT   .
    ZTPP_PP_LOG_HEAD-PROGRAMM = SY-REPID    .
    ZTPP_PP_LOG_HEAD-LOGTYPE  = PA_TYPE     .
    ZTPP_PP_LOG_HEAD-JOBTYPE  = SY-BATCH    .
    ZTPP_PP_LOG_HEAD-LOGSTEP  = PA_STEP     .
    ZTPP_PP_LOG_HEAD-MSG      = PA_TEXT     .
    ZTPP_PP_LOG_HEAD-LDATE    = SY-DATUM    .
    ZTPP_PP_LOG_HEAD-LTIME    = SY-UZEIT    .
    ZTPP_PP_LOG_HEAD-LUSER    = SY-UNAME    .
    INSERT INTO ZTPP_PP_LOG_HEAD VALUES ZTPP_PP_LOG_HEAD .
  ENDIF.

  " Log Detail Creation
  ZTPP_PP_LOG_DETA-LOGKEY   = GV_NUMBERT    .
  ZTPP_PP_LOG_DETA-SEQUENCE = GV_CNT      .
  ZTPP_PP_LOG_DETA-LOGTYPE  = PA_TYPE     .
  ZTPP_PP_LOG_DETA-JOBTYPE  = SY-BATCH    .
  ZTPP_PP_LOG_DETA-LOGSTEP  = PA_STEP     .
  ZTPP_PP_LOG_DETA-KEYDATA  = PA_KEY      .
  INSERT INTO ZTPP_PP_LOG_DETA VALUES ZTPP_PP_LOG_DETA .
ENDFORM.                    " create_log
*&---------------------------------------------------------------------*
*&      Form  get_logserial
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_LOGSERIAL.
  " Log Head Creation..
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'ZLOG'
       IMPORTING
            NUMBER                  = GV_NUMBERT
       EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.
ENDFORM.                    " get_logserial
