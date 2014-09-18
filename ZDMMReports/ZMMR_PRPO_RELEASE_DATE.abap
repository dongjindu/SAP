************************************************************************
* Program Name      : ZMMR_PRPO_RELEASE_DATE
* Creation Date     : 08/17/2010
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZMMR_PRPO_RELEASE_DATE NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.

TABLES: EBAN.

TYPE-POOLS SLIS .
DATA : BEGIN OF IT_DATA OCCURS 0,
       BANFN LIKE EBAN-BANFN,
       BNFPO LIKE EBAN-BNFPO,
       MATNR LIKE EBAN-MATNR,
       PREIS LIKE EBAN-PREIS,
       MENGE_PR LIKE EBAN-MENGE,
       AMT_PR LIKE EKPO-BRTWR,
       PEINH LIKE EBAN-PEINH,
       MEINS LIKE EBAN-MEINS,
       BADAT LIKE EBAN-BADAT,
       UDATE LIKE CDHDR-UDATE,
       EBELN LIKE EBAN-EBELN,
       EBELP LIKE EBAN-EBELP,
       NETPR LIKE EKPO-NETPR,
       MENGE_PO LIKE EKPO-MENGE,
       AMT_PO LIKE EKPO-BRTWR,
       BEDAT LIKE EBAN-BEDAT,
       REL_DAYS TYPE I,
       PRICE_DIFF LIKE  EKPO-NETPR,
       AMT_DIFF LIKE EKPO-BRTWR,
       ERNAM LIKE EKKO-ERNAM,
       DEPARTMENT(40),
       END OF IT_DATA.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

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

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.

*PARAMETERS:  P_EKORG LIKE EINE-EKORG DEFAULT 'PU01'.

SELECT-OPTIONS: S_BANFN FOR EBAN-BANFN,
                S_BSART FOR EBAN-BSART,
*                S_FRGKZ FOR EBAN-FRGKZ,
*                S_FRGZU FOR EBAN-FRGZU,
                S_EKGRP FOR EBAN-EKGRP.
SELECTION-SCREEN SKIP.
PARAMETERS: P_PR RADIOBUTTON GROUP GRP1 DEFAULT 'X' ,
            P_PO RADIOBUTTON GROUP GRP1.
SELECT-OPTIONS: S_BADAT FOR EBAN-BADAT.
SELECTION-SCREEN END OF BLOCK BLOCK1.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.


START-OF-SELECTION.
  PERFORM GET_DATA.
  IF IT_DATA[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    PERFORM DISPLAY_DATA.
  ENDIF.
*---------------------------------------------------------------------*
*       FORM get_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA.

  DATA : BEGIN OF LT_TEMP OCCURS 0,
         BANFN LIKE CDHDR-OBJECTID,
         BNFPO LIKE EBAN-BNFPO,
         MATNR LIKE EBAN-MATNR,
         PREIS LIKE EBAN-PREIS,
         MENGE LIKE EBAN-MENGE,
         PEINH LIKE EBAN-PEINH,
         MEINS LIKE EBAN-MEINS,
         BADAT LIKE EBAN-BADAT,
         UDATE LIKE CDHDR-UDATE,
         EBELN LIKE EBAN-EBELN,
         EBELP LIKE EBAN-EBELP,
         NETPR LIKE EKPO-NETPR,
         BEDAT LIKE EBAN-BEDAT,
         REL_DAYS TYPE I,
         PRICE_DIFF LIKE  EKPO-NETPR,
         ERNAM LIKE EBAN-ERNAM,
         END OF LT_TEMP.

  DATA: BEGIN OF LT_CDPOS OCCURS 0,
        OBJECTID LIKE CDPOS-OBJECTID,
        TABKEY LIKE CDPOS-TABKEY,
        END OF LT_CDPOS.

  DATA: BEGIN OF LT_CDHDR OCCURS 0,
        OBJECTID LIKE CDHDR-OBJECTID,
        CHANGENR LIKE CDHDR-CHANGENR,
        UDATE LIKE CDHDR-UDATE,
        END OF LT_CDHDR.

  DATA: BEGIN OF LT_REL_DATE OCCURS 0,
        BANFN LIKE IT_DATA-BANFN,
        BNFPO LIKE IT_DATA-BNFPO,
        UDATE LIKE IT_DATA-UDATE,
        END OF LT_REL_DATE.

  DATA: L_COUNT TYPE I.

** Changed by Furong on 01/05/11
*  SELECT BANFN MATNR BNFPO PREIS MENGE
*     PEINH MEINS BADAT EBELN EBELP
*     BEDAT ERNAM INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
*     FROM EBAN
*    WHERE BANFN IN S_BANFN
*     AND BSART IN S_BSART
**     AND FRGKZ IN S_FRGKZ
**     AND FRGZU IN S_FRGZU
*     AND BADAT IN S_BADAT.
  IF P_PR = 'X'.
    SELECT BANFN MATNR BNFPO PREIS MENGE
** Changed on 04/25/11 to get department from PR
*        PEINH MEINS BADAT EBELN EBELP BEDAT   "   ERNAM
        PEINH MEINS BADAT EBELN EBELP BEDAT ERNAM
** End of change on 04/25/11
        INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
        FROM EBAN
       WHERE BANFN IN S_BANFN
        AND BSART IN S_BSART
        AND BADAT IN S_BADAT.
** Changed on 02/10/11
*        AND EKGRP IN S_EKGRP.
** end of change
  ELSE.
*    SELECT BANFN MATNR BNFPO PREIS MENGE
*       PEINH MEINS BADAT EBELN EBELP BEDAT   " ERNAM
*       INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
*       FROM EBAN
*      WHERE BANFN IN S_BANFN
*        AND BSART IN S_BSART.

 SELECT BANFN MATNR BNFPO PREIS MENGE
** Changed on 04/25/11 to get department from PR
*       PEINH MEINS BADAT A~EBELN EBELP A~BEDAT   " ERNAM
       PEINH MEINS BADAT A~EBELN EBELP A~BEDAT a~ERNAM
** End of change
       INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
       FROM EBAN AS A
       INNER JOIN EKKO AS B
       ON A~EBELN = B~EBELN
      WHERE BANFN IN S_BANFN
        AND A~BSART IN S_BSART
        AND B~AEDAT IN S_BADAT.
*
** Changed on 02/10/11
*        AND EKGRP IN S_EKGRP.
** end of change
  ENDIF.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  SELECT OBJECTID CHANGENR UDATE INTO TABLE LT_CDHDR
    FROM CDHDR
    FOR ALL ENTRIES IN LT_TEMP
     WHERE OBJECTCLAS = 'BANF'
      AND OBJECTID = LT_TEMP-BANFN
      AND ( TCODE = 'ME54' OR TCODE = 'ME55' ).

  LOOP AT LT_CDHDR.
    REFRESH: LT_CDPOS.
    SELECT OBJECTID TABKEY INTO TABLE LT_CDPOS
      FROM CDPOS
     WHERE OBJECTCLAS = 'BANF'
      AND OBJECTID = LT_CDHDR-OBJECTID
      AND CHANGENR = LT_CDHDR-CHANGENR
      AND VALUE_NEW = 'XX'.
    IF SY-SUBRC = 0.
      LOOP AT LT_CDPOS.
        LT_REL_DATE-BANFN = LT_CDPOS-OBJECTID.
        LT_REL_DATE-BNFPO = LT_CDPOS-TABKEY+13(5).
        LT_REL_DATE-UDATE = LT_CDHDR-UDATE.
        APPEND LT_REL_DATE.
        CLEAR: LT_REL_DATE.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT LT_TEMP BY BANFN BNFPO.
  SORT LT_REL_DATE BY BANFN BNFPO.
  LOOP AT LT_TEMP.
    READ TABLE LT_REL_DATE WITH KEY BANFN = LT_TEMP-BANFN
                                    BNFPO = LT_TEMP-BNFPO
                                    BINARY SEARCH.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING LT_TEMP TO IT_DATA.
      IT_DATA-UDATE = LT_REL_DATE-UDATE.
      IT_DATA-MENGE_PR = LT_TEMP-MENGE.

** Changed by Furong on 01/05/11
*      SELECT SINGLE ERNAM NETPR MENGE
*         INTO (IT_DATA-ERNAM, IT_DATA-NETPR, IT_DATA-MENGE_PO)
      IF P_PR = 'X'.
** Changed on 02/10/11
** Changed on 04/25/11
*           SELECT SINGLE NETPR MENGE ERNAM
*           INTO (IT_DATA-NETPR, IT_DATA-MENGE_PO, IT_DATA-ERNAM)
            SELECT SINGLE NETPR MENGE
           INTO (IT_DATA-NETPR, IT_DATA-MENGE_PO)
** end 04/25/11
** End of change
           FROM EKKO AS A
           INNER JOIN EKPO AS B
           ON A~EBELN = B~EBELN
           WHERE A~EBELN = IT_DATA-EBELN
             AND EBELP = IT_DATA-EBELP
             AND EKGRP IN S_EKGRP.
      ELSE.
** Changed on 02/10/11
*        SELECT SINGLE NETPR MENGE
*       INTO (IT_DATA-NETPR, IT_DATA-MENGE_PO)
** Changed on 04/25/11
*          SELECT SINGLE NETPR MENGE ERNAM
*          INTO (IT_DATA-NETPR, IT_DATA-MENGE_PO, IT_DATA-ERNAM)
         SELECT SINGLE NETPR MENGE
          INTO (IT_DATA-NETPR, IT_DATA-MENGE_PO)
** end 04/25/11
** End of change
        FROM EKKO AS A
        INNER JOIN EKPO AS B
        ON A~EBELN = B~EBELN
        WHERE A~EBELN = IT_DATA-EBELN
          AND EBELP = IT_DATA-EBELP
          AND A~AEDAT IN S_BADAT
          AND EKGRP IN S_EKGRP.
        IF SY-SUBRC <> 0.
          CLEAR: IT_DATA.
          CONTINUE.
        ENDIF.
      ENDIF.
** End of change on 01/05/11
      IT_DATA-REL_DAYS = IT_DATA-UDATE - IT_DATA-BEDAT.
      IT_DATA-AMT_PR = IT_DATA-MENGE_PR * IT_DATA-PREIS.
      IT_DATA-AMT_PO = IT_DATA-MENGE_PO * IT_DATA-NETPR.
      IF NOT IT_DATA-EBELN IS INITIAL.
        IT_DATA-PRICE_DIFF = IT_DATA-PREIS - IT_DATA-NETPR.
        IT_DATA-AMT_DIFF = IT_DATA-AMT_PR - IT_DATA-AMT_PO.
      ENDIF.
      SELECT SINGLE DEPARTMENT INTO IT_DATA-DEPARTMENT
      FROM USR21 AS A
      INNER JOIN ADCP AS B
      ON A~PERSNUMBER = B~PERSNUMBER
      WHERE BNAME = IT_DATA-ERNAM.
      APPEND IT_DATA.
      CLEAR: IT_DATA.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_SCREEN.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_EXCEL'.
      SCREEN-INPUT = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen

*---------------------------------------------------------------------*
*       FORM display_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
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
    PERFORM BUILD_FIELD_CATALOG USING 'IT_DATA'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDMODULE.                 " display_alv  OUTPUT
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
*    WHEN 'INBDEL'.
*      PERFORM DISPLAY_INBOUND_DELIVERY.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = SPACE
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_DATA[]
               IT_SORT          = IT_SORT[].

ENDFORM.                    " assign_itab_to_alv
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

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'BANFN'.
  IT_SORT-UP             = 'X'.
  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.

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

                                  'S' 'BANFN'       ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Pur Req. No',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'BNFPO'       ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Item',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'PREIS'       ' ',
                                  ' ' 'COLTEXT'     'PR Price',
                                  'E' 'OUTPUTLEN'   '13',

                                 'S' 'MENGE_PR'       ' ',
                                  ' ' 'COLTEXT'     'PR Qty',
                                  'E' 'OUTPUTLEN'   '10',

                               'S' 'AMT_PR'       ' ',
                                  ' ' 'COLTEXT'     'PR Value',
                                  'E' 'OUTPUTLEN'   '10',


                                  'S' 'PEINH'       ' ',
                                  ' ' 'COLTEXT'     'P.U.',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'MEINS'       ' ',
                                  ' ' 'COLTEXT'     'UoM',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'BADAT'       ' ',
                                  ' ' 'COLTEXT'     'PR Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'UDATE'       ' ',
                                  ' ' 'COLTEXT'     'Rel Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EBELN'       ' ',
                                 ' ' 'COLTEXT'     'Pur Order No',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'COLTEXT'     'Item',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'NETPR'        ' ',
                                  ' ' 'COLTEXT'     'PO Price',
                                  'E' 'OUTPUTLEN'   '13',

                                'S' 'MENGE_PO'       ' ',
                                  ' ' 'COLTEXT'     'PO Qty',
                                  'E' 'OUTPUTLEN'   '10',

                               'S' 'AMT_PO'       ' ',
                                  ' ' 'COLTEXT'     'PO Value',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'BEDAT'       ' ',
                                  ' ' 'COLTEXT'     'PO Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'REL_DAYS'        ' ',
                                  ' ' 'COLTEXT'     'Release Days',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'PRICE_DIFF'  ' ',
                                  ' ' 'COLTEXT'     'Price Diff',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'AMT_DIFF'  ' ',
                                  ' ' 'COLTEXT'     'Vaule Diff',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'DEPARTMENT'  ' ',
                                  ' ' 'COLTEXT'     'Department',
                                  'E' 'OUTPUTLEN'   '30'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
*----------------------------------------------------------------------*
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
