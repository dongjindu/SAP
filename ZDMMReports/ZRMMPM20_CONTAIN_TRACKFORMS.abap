************************************************************************
* Program Name : ZRMMPM20_CONTAIN_TRACK
* Created by   : Min-su Park
* Created on   : 2003.11.19.
* Pattern      :
* Description  : Container Tracking
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.19.     Min-su Park    UD1K901873     Initial Coding
***********************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZRMMPM20_CONTAIN_TRACKFORMS                                *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_BASIC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BASIC_DATA.
  SELECT LK~VSART   "Shipping Type
         LP~VGBEL   "Order No.
         LK~TRAID   "Container No.
         ZT~ZFBLNO  "BL NO
         LP~LFIMG   "CASE QTY
         LP~MEINS
         ZT~ZFETD   "ETD Pusan
         ZT~ZFETA   "ETA Mobile
        ZT~ZFRETA   "Arrival Date
        ZTI~ZFEDT  "C/CLEAR
   CONT~PASS_DATE   "Arrival
       CONT~LGPLA   "Location
       CONT~BDATU   "Unpack Date
  CONT~LEAVE_DATE   "Retrun date
  CONT~LGPLA
  CONT~LGTYP
  CONT~LGBER
     INTO CORRESPONDING FIELDS OF TABLE IT_TRACK
     FROM LIPS AS LP INNER JOIN LIKP AS LK
           ON LP~VBELN = LK~VBELN
          INNER JOIN ZTMM_CONTAINER AS CONT
           ON LK~TRAID = CONT~CONT_REG_NUMB1
          LEFT OUTER JOIN  ZTBL AS ZT
           ON LK~BOLNR = ZT~ZFHBLNO
          LEFT OUTER JOIN  ZTIDSUS AS ZTI
           ON LK~BOLNR = ZTI~ZFHBLNO
  WHERE LP~VGBEL IN S_VGBEL
    AND LP~MATNR IN S_MATNR
    AND LK~TRAID IN S_TRAID
    AND LP~KDMAT IN S_KDMAT
    AND LK~TRATY = '0005'.

  LOOP AT IT_TRACK.
    SELECT SINGLE BEZEI
             INTO IT_TRACK-BEZEI
             FROM T173T
            WHERE VSART = IT_TRACK-VSART
              AND SPRAS = 'E'.

    CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
      EXPORTING
        BEGDA           = P_DATE
        ENDDA           = IT_TRACK-PASS_DATE
*     TAB_MODE        =
     IMPORTING
       DAYS             = IT_TRACK-HD_I .
*Get holding days
    IT_TRACK-HD_D = IT_TRACK-HD_I.
*Change Container No. from BL no. when shipping type is air.
    IF IT_TRACK-VSART = '05'.
      IT_TRACK-TRAID = IT_TRACK-ZFBLNO.
    ENDIF.
*Get Unpack date
    SELECT SINGLE BDATU
             INTO IT_TRACK-BDATU
             FROM LAGP
            WHERE LGNUM = 'P01'
              AND LGTYP = IT_TRACK-LGTYP
              AND LGPLA = IT_TRACK-LGPLA.
    MODIFY IT_TRACK.
  ENDLOOP.
ENDFORM.                    " GET_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FIELD_BUILD.
  W_REPID = SY-REPID.
  CLEAR : WA_FIELDCAT[], WA_EVENTS[],
          WA_LIST_TOP_OF_PAGE[]     .
  PERFORM FIELDCAT_INIT  USING WA_FIELDCAT[].
  PERFORM EVENTTAB_BUILD USING WA_EVENTS[].
  PERFORM COMMENT_BUILD  USING WA_LIST_TOP_OF_PAGE[].
  PERFORM ALV_FUNCTION_BASIC.
ENDFORM.                    " ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  DATA: POS TYPE I VALUE 1.

*SHIPPING TYPE
  clear ls_fieldcat.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'BEZEI'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'SHIPG TYPE'.
  LS_FIELDCAT-SELTEXT_M     = 'SHIPG TYPE'.
  LS_FIELDCAT-SELTEXT_S     = 'SHIPG TYPE'.
  LS_FIELDCAT-OUTPUTLEN     = '20'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*ORDER NO
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'VGBEL'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'ORDER NO'.
  LS_FIELDCAT-SELTEXT_M     = 'ORDER NO'.
  LS_FIELDCAT-SELTEXT_S     = 'ORDER NO'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*CONT. NO
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'TRAID'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'CONT. NO'.
  LS_FIELDCAT-SELTEXT_M     = 'CONT. NO'.
  LS_FIELDCAT-SELTEXT_S     = 'CONT. NO'.
  LS_FIELDCAT-OUTPUTLEN     = '20'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*CASE QTY
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LFIMG'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = 'MEINS'.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'CASE QTY'.
  LS_FIELDCAT-SELTEXT_M     = 'CASE QTY'.
  LS_FIELDCAT-SELTEXT_S     = 'CASE QTY'.
  LS_FIELDCAT-OUTPUTLEN     = '13'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*ETD Pusan
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ZFETD'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'ETD Pusan'.
  LS_FIELDCAT-SELTEXT_M     = 'ETD Pusan'.
  LS_FIELDCAT-SELTEXT_S     = 'ETD Pusan'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*ETD Mobile
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ZFETA'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'ETD Mobile'.
  LS_FIELDCAT-SELTEXT_M     = 'ETD Mobile'.
  LS_FIELDCAT-SELTEXT_S     = 'ETD Mobile'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Arrival
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ZFRETA'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Arrival'.
  LS_FIELDCAT-SELTEXT_M     = 'Arrival'.
  LS_FIELDCAT-SELTEXT_S     = 'Arrival'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*C/CLEAR
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ZFEDT'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'C/CLEAR'.
  LS_FIELDCAT-SELTEXT_M     = 'C/CLEAR'.
  LS_FIELDCAT-SELTEXT_S     = 'C/CLEAR'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*HMMA CY ARRIVAL
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'PASS_DATE'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'HMMA CY ARRIVAL'.
  LS_FIELDCAT-SELTEXT_M     = 'HMMA CY ARRIVAL'.
  LS_FIELDCAT-SELTEXT_S     = 'HMMA CY ARRIVAL'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*HMMA CY LOCATION
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LGPLA'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'HMMA CY LOCATION'.
  LS_FIELDCAT-SELTEXT_M     = 'HMMA CY LOCATION'.
  LS_FIELDCAT-SELTEXT_S     = 'HMMA CY LOCATION'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*Unpack date
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'BDATU'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Unpack date'.
  LS_FIELDCAT-SELTEXT_M     = 'Unpack date'.
  LS_FIELDCAT-SELTEXT_S     = 'Unpack date'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*Return date
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LEAVE_DATE'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Return date'.
  LS_FIELDCAT-SELTEXT_M     = 'Return date'.
  LS_FIELDCAT-SELTEXT_S     = 'Return date'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*Holding days
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'HD_D'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Holding days'.
  LS_FIELDCAT-SELTEXT_M     = 'Holding days'.
  LS_FIELDCAT-SELTEXT_S     = 'Holding days'.
  LS_FIELDCAT-OUTPUTLEN     = '5'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS[]  text
*----------------------------------------------------------------------*
FORM EVENTTAB_BUILD USING E03_LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE W_FORMNAME_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO E03_LT_EVENTS.
  ENDIF.

ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD  USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: INFO_TXT(50).
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
* LS_LINE-KEY:  not used for this type
  LS_LINE-INFO = TEXT-100.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*PO Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)   = 'From'    .
  INFO_TXT+5(10)  = S_VGBEL-LOW .
  INFO_TXT+16(2)  = 'To'      .
  INFO_TXT+19(10) = S_VGBEL-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'PO NO:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*Material Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)  = 'From'      .
  INFO_TXT+5(18) = S_MATNR-LOW .
  INFO_TXT+24(2) = 'To'        .
  INFO_TXT+27(18) = S_MATNR-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Material:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*Container Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)  = 'From'    .
  INFO_TXT+5(20)  = S_TRAID-LOW .
  INFO_TXT+26(2) = 'To'      .
  INFO_TXT+29(20) = S_TRAID-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Container:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*Case Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)  = 'From'    .
  INFO_TXT+5(20)  = S_KDMAT-LOW .
  INFO_TXT+26(2) = 'To'      .
  INFO_TXT+29(20) = S_KDMAT-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Case No:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            IT_LIST_COMMENTARY = WA_LIST_TOP_OF_PAGE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_FUNCTION_BASIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FUNCTION_BASIC.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM           = W_REPID
      IT_EVENTS                    = WA_EVENTS[]
      IT_FIELDCAT                  = WA_FIELDCAT[]
      I_CALLBACK_USER_COMMAND      = 'USER_COMMAND'
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB                     = IT_TRACK
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ALV_FUNCTION_BASIC
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING UCOMM    LIKE SY-UCOMM
                        SELFIELD TYPE SLIS_SELFIELD.

  READ TABLE IT_TRACK INDEX SELFIELD-TABINDEX.

  CHECK SY-SUBRC = 0.

  CHECK UCOMM = '&IC1'.

  PERFORM GET_TRACK02.
  PERFORM ALV_BUILD02.
ENDFORM. " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  GET_TRACK02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TRACK02.
    SELECT LP~KDMAT   "CASE NO
           LP~MATNR   "PART NO
           MT~MAKTX   "PART NAME
*          LFIMG LIKE LIPS-LFIMG , "BOX QTY ?
           LP~LFIMG   "TOTAL PCS
           LP~MEINS
           CONT~LGPLA "Storage bin
*           PLPOS   "Storage bin Position
*          LOCAT(13)             , "Bin + Position
         PASS_DATE "Receiving date
     INTO CORRESPONDING FIELDS OF TABLE IT_TRACK02
     FROM LIPS AS LP INNER JOIN LIKP AS LK
           ON LP~VBELN = LK~VBELN
          INNER JOIN ZTMM_CONTAINER AS CONT
           ON LK~TRAID = CONT~CONT_REG_NUMB1
          INNER JOIN MAKT AS MT
           ON LP~MATNR = MT~MATNR
     WHERE LK~TRAID = IT_TRACK-TRAID
       AND LK~TRATY = '0005'
       AND MT~SPRAS = 'E'.
     LOOP AT IT_TRACK02.
*Get bin position
      SELECT SINGLE PLPOS
               INTO IT_TRACK02-PLPOS
               FROM LEIN
              WHERE LENUM = IT_TRACK02-KDMAT.
*Get CONT. LOCATION
      CONCATENATE IT_TRACK02-LGPLA '/' IT_TRACK02-PLPOS
             INTO IT_TRACK02-LOCAT.
      MODIFY IT_TRACK02.
     ENDLOOP.
ENDFORM.                    " GET_TRACK02
*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_BUILD02.
  W_REPID02 = SY-REPID.
  CLEAR : WA_FIELDCAT02[], WA_EVENTS02[],
          WA_LIST_TOP_OF_PAGE02[]     .
  PERFORM FIELDCAT_INIT02  USING WA_FIELDCAT02[].
  PERFORM EVENTTAB_BUILD02 USING WA_EVENTS02[].
  PERFORM COMMENT_BUILD02  USING WA_LIST_TOP_OF_PAGE02[].
  PERFORM ALV_FUNCTION_02.
ENDFORM.                    " ALV_BUILD02
*&---------------------------------------------------------------------*
*&      Form  ALV_FUNCTION_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FUNCTION_02.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM           = W_REPID02
      IT_EVENTS                    = WA_EVENTS02[]
      IT_FIELDCAT                  = WA_FIELDCAT02[]
      I_CALLBACK_USER_COMMAND      = 'USER_COMMAND1'
*     I_SCREEN_START_COLUMN        = 12
*     I_SCREEN_START_LINE          = 12
*     I_SCREEN_END_COLUMN          = 140
*     I_SCREEN_END_LINE            = 140
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB                     = IT_TRACK02
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ALV_FUNCTION_02
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FIELDCAT02[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT02 USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  DATA: POS TYPE I VALUE 1.

*CASE NO.
  clear ls_fieldcat.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'KDMAT'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'CASE NO'.
  LS_FIELDCAT-SELTEXT_M     = 'CASE NO'.
  LS_FIELDCAT-SELTEXT_S     = 'CASE NO'.
  LS_FIELDCAT-OUTPUTLEN     = '20'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*PART NO.
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MATNR'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'PART NO'.
  LS_FIELDCAT-SELTEXT_M     = 'PART NO'.
  LS_FIELDCAT-SELTEXT_S     = 'PART NO'.
  LS_FIELDCAT-OUTPUTLEN     = '18'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*PART NAME.
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MAKTX'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'PART NAME'.
  LS_FIELDCAT-SELTEXT_M     = 'PART NAME'.
  LS_FIELDCAT-SELTEXT_S     = 'PART NAME'.
  LS_FIELDCAT-OUTPUTLEN     = '30'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*TOTAL PCS
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LFIMG'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = 'MEINS'.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'TOTAL PCS'.
  LS_FIELDCAT-SELTEXT_M     = 'TOTAL PCS'.
  LS_FIELDCAT-SELTEXT_S     = 'TOTAL PCS'.
  LS_FIELDCAT-OUTPUTLEN     = '18'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*CONT. LOCATION
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LOCAT'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'CONT LOCATION'.
  LS_FIELDCAT-SELTEXT_M     = 'CONT LOCATION'.
  LS_FIELDCAT-SELTEXT_S     = 'CONT LOCATION'.
  LS_FIELDCAT-OUTPUTLEN     = '13'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Receiving DATE
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'PASS_DATE'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Receiving DATE'.
  LS_FIELDCAT-SELTEXT_M     = 'Receiving DATE'.
  LS_FIELDCAT-SELTEXT_S     = 'Receiving DATE'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
ENDFORM.                    " FIELDCAT_INIT02
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND1 USING UCOMM    LIKE SY-UCOMM
                        SELFIELD TYPE SLIS_SELFIELD.

  READ TABLE IT_TRACK02 INDEX SELFIELD-TABINDEX.

  CHECK SY-SUBRC = 0.

  CHECK UCOMM = '&IC1'.
   PERFORM GET_TRACK03.
   PERFORM ALV_BUILD03.
*  PERFORM ALV_BUILD02.
ENDFORM. " USER_COMMAND1
*&---------------------------------------------------------------------*
*&      Form  GET_TRACK03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TRACK03.
  SELECT LP~VGBEL   "Order No.
         LK~TRAID   "Container No.
         LP~KDMAT   "CASE NO
         LP~LFIMG   "CASE QTY
         LP~MEINS
         ZT~ZFETD   "ETD Pusan
         ZT~ZFETA   "ETA Mobile
        ZTI~ZFEDT   "C/CLEAR
   CONT~PASS_DATE   "Arrival
       CONT~LGPLA   "Location
     INTO CORRESPONDING FIELDS OF TABLE IT_TRACK03
     FROM LIPS AS LP INNER JOIN LIKP AS LK
           ON LP~VBELN = LK~VBELN
          INNER JOIN ZTMM_CONTAINER AS CONT
           ON LK~TRAID = CONT~CONT_REG_NUMB1
          LEFT OUTER JOIN  ZTBL AS ZT
           ON LK~BOLNR = ZT~ZFHBLNO
          LEFT OUTER JOIN  ZTIDSUS AS ZTI
           ON LK~BOLNR = ZTI~ZFHBLNO
  WHERE LP~MATNR = IT_TRACK02-MATNR
    AND LK~TRATY = '0005'.
ENDFORM.                    " GET_TRACK03
*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_BUILD03.
  W_REPID03 = SY-REPID.
  CLEAR : WA_FIELDCAT03[], WA_EVENTS03[],
          WA_LIST_TOP_OF_PAGE03[]     .
  PERFORM SET_LAYOUT       CHANGING WA_LAYO.
  PERFORM FIELDCAT_INIT03  USING WA_FIELDCAT03[].
  PERFORM EVENTTAB_BUILD03 USING WA_EVENTS03[].
  PERFORM COMMENT_BUILD03  USING WA_LIST_TOP_OF_PAGE03[].
  PERFORM ALV_FUNCTION_03.
ENDFORM.                    " ALV_BUILD03
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FIELDCAT03[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT03 USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  DATA: POS TYPE I VALUE 1.

*ORDER NO
*  clear ls_fieldcat.
*  LS_FIELDCAT-COL_POS       = POS.
*  LS_FIELDCAT-FIELDNAME     = 'BOX'.
*  LS_FIELDCAT-INPUT         = 'X'.
*  LS_FIELDCAT-EDIT          = 'X'.
**         input(1)       type c,        " input
**         edit(1)        type c,        " internal use only
*  LS_FIELDCAT-CHECKBOX      = 'X'.
*  LS_FIELDCAT-REF_FIELDNAME = ''.
*  LS_FIELDCAT-KEY           = ''.
*  LS_FIELDCAT-QFIELDNAME    = ''.
*  LS_FIELDCAT-CFIELDNAME    = ''.
*  LS_FIELDCAT-SELTEXT_L     = ''.
*  LS_FIELDCAT-SELTEXT_M     = ''.
*  LS_FIELDCAT-SELTEXT_S     = ''.
*  LS_FIELDCAT-OUTPUTLEN     = '1'.
*  LS_FIELDCAT-NO_OUT        = ''.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*ORDER NO
  clear ls_fieldcat.
  POS = POS .
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'VGBEL'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'ORDER NO'.
  LS_FIELDCAT-SELTEXT_M     = 'ORDER NO'.
  LS_FIELDCAT-SELTEXT_S     = 'ORDER NO'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*CONT. NO
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'TRAID'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'CONT. NO'.
  LS_FIELDCAT-SELTEXT_M     = 'CONT. NO'.
  LS_FIELDCAT-SELTEXT_S     = 'CONT. NO'.
  LS_FIELDCAT-OUTPUTLEN     = '20'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*CASE NO.
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'KDMAT'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'CASE NO'.
  LS_FIELDCAT-SELTEXT_M     = 'CASE NO'.
  LS_FIELDCAT-SELTEXT_S     = 'CASE NO'.
  LS_FIELDCAT-OUTPUTLEN     = '20'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*CASE QTY
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LFIMG'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = 'MEINS'.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'CASE QTY'.
  LS_FIELDCAT-SELTEXT_M     = 'CASE QTY'.
  LS_FIELDCAT-SELTEXT_S     = 'CASE QTY'.
  LS_FIELDCAT-OUTPUTLEN     = '13'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*ETD Pusan
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ZFETD'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'ETD Pusan'.
  LS_FIELDCAT-SELTEXT_M     = 'ETD Pusan'.
  LS_FIELDCAT-SELTEXT_S     = 'ETD Pusan'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*ETD Mobile
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ZFETA'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'ETD Mobile'.
  LS_FIELDCAT-SELTEXT_M     = 'ETD Mobile'.
  LS_FIELDCAT-SELTEXT_S     = 'ETD Mobile'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*C/CLEAR
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ZFEDT'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'C/CLEAR'.
  LS_FIELDCAT-SELTEXT_M     = 'C/CLEAR'.
  LS_FIELDCAT-SELTEXT_S     = 'C/CLEAR'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*HMMA CY ARRIVAL
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'PASS_DATE'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'HMMA CY ARRIVAL'.
  LS_FIELDCAT-SELTEXT_M     = 'HMMA CY ARRIVAL'.
  LS_FIELDCAT-SELTEXT_S     = 'HMMA CY ARRIVAL'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*HMMA CY LOCATION
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LGPLA'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'HMMA CY LOCATION'.
  LS_FIELDCAT-SELTEXT_M     = 'HMMA CY LOCATION'.
  LS_FIELDCAT-SELTEXT_S     = 'HMMA CY LOCATION'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

ENDFORM.                    " FIELDCAT_INIT03
*&---------------------------------------------------------------------*
*&      Form  ALV_FUNCTION_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FUNCTION_03.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM           = W_REPID03
      IT_EVENTS                    = WA_EVENTS03[]
      IS_LAYOUT                    = WA_LAYO
      IT_FIELDCAT                  = WA_FIELDCAT03[]
*     I_CALLBACK_USER_COMMAND      = 'USER_COMMAND1'
*     I_SCREEN_START_COLUMN        = 12
*     I_SCREEN_START_LINE          = 12
*     I_SCREEN_END_COLUMN          = 140
*     I_SCREEN_END_LINE            = 140
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB                     = IT_TRACK03
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ALV_FUNCTION_03
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE02[]  text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD02 USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: INFO_TXT(50).
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
* LS_LINE-KEY:  not used for this type
  LS_LINE-INFO = TEXT-101.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Container Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT = IT_TRACK-TRAID.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Container:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
ENDFORM.                    " COMMENT_BUILD02
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE03[]  text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD03  USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: INFO_TXT(50).
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
* LS_LINE-KEY:  not used for this type
  LS_LINE-INFO = TEXT-102.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*Material Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT = IT_TRACK02-MATNR.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Part No:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
ENDFORM.                    " COMMENT_BUILD03
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS02[]  text
*----------------------------------------------------------------------*
FORM EVENTTAB_BUILD02 USING E03_LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE W_FORMNAME_TOP_OF_PAGE02 TO LS_EVENT-FORM.
    APPEND LS_EVENT TO E03_LT_EVENTS.
  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD02
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS03[]  text
*----------------------------------------------------------------------*
FORM EVENTTAB_BUILD03 USING E03_LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE W_FORMNAME_TOP_OF_PAGE03 TO LS_EVENT-FORM.
    APPEND LS_EVENT TO E03_LT_EVENTS.
  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD03
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE02.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            IT_LIST_COMMENTARY = WA_LIST_TOP_OF_PAGE02.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE03.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            IT_LIST_COMMENTARY = WA_LIST_TOP_OF_PAGE03.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_LAYO  text
*----------------------------------------------------------------------*
FORM SET_LAYOUT changing cs_layo type slis_layout_alv.
*... Interaction
*  CS_LAYO-BOX_FIELDNAME          = 'BOX'.
*  CS_LAYO-GET_SELINFOS           = 'X'  .
*  CS_LAYO-GROUP_CHANGE_EDIT      = 'X'  .
ENDFORM.                    " SET_LAYOUT
