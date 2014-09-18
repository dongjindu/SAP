*&------------------------------------------------------------------
*& Program ID     : ZMMR30400T
*& Program Name   : Acknowledgement creation for ASN (DESADV)
*& Created by     : yang
*& Created on     : 05.21.2009
*& Development ID : MM-000
*& Reference Pgm. : ZMMR1650
*& Description    : Acknowledgement creation for ASN (DESADV)
*&                  IDoc document update and send IDoc docunent
*& IDOC Message : APERAK – Application error and acknowledgement message
*&--> Sending RFC Function 'Z_MM_IF_OB_03_001'[TGLO003],
*                          'Z_MM_IF_OB_03_002'[TGLO003]
*& EDI and WEB leads and ASN creates
*& and is a program to transmit about the message which occurs.
*& Modification Log
*&====================================================================
*& Date        Developer    Request ID    Description
*& 05.21.2009  Yang                           first dev.
*&--------------------------------------------------------------------

REPORT  ZMMR30400T  NO STANDARD PAGE HEADING   MESSAGE-ID ZMPP.

*===============================================================*
* Data definition                                               *
*===============================================================*
*-----------------------------------------*
* Include                                 *
*-----------------------------------------*
INCLUDE ZMMITOP01.                                  "Global TOP
INCLUDE <ICON>.
*-----------------------------------------*
* Table definition                        *
*-----------------------------------------*
TABLES : EDIDC, LFA1, LISTEDIDC, TEDS2, VBUK,LIKP,
         STACUST,
    "Customizing for IDoc status (status groups, archive, procg)
         STALIGHT.
"Traffic Light Assignment to Status Groups for IDoc Display

*-----------------------------------------*
* data definition                         *
*-----------------------------------------*
* list
DATA : BEGIN OF IT_LIST OCCURS 0.
        INCLUDE STRUCTURE LISTEDIDC.
        INCLUDE STRUCTURE E1ADRM1.
DATA :  MARK,
        PARTNER_IDLF   TYPE E1ADRM1-PARTNER_ID,
        PARTNER_IDWE   TYPE E1ADRM1-PARTNER_ID,
        NAME1_WE       TYPE E1ADRM1-NAME1,
        LIFEX   TYPE E1EDL20-LIFEX,       "delivery note number
        RCODE   TYPE CHAR3,               "result code
        TRAID   TYPE CHAR20,              "container number
        MSG  TYPE IDOC_MSG,
        RETRY(1),                         "Retry
        BOLNR   TYPE EDILSEGTYP,
       END OF IT_LIST.

* IDoc document table
DATA : IT_EDIDC TYPE TABLE OF EDIDC WITH HEADER LINE.

* Customizing for IDoc status (status groups, archive, procg)
DATA: BEGIN OF IT_STACUST OCCURS 0,
        STATUS LIKE STACUST-STATUS,
        STATVA LIKE STACUST-STATVA,
      END OF IT_STACUST.

* Traffic Light Assignment to Status Groups for IDoc Display
DATA: BEGIN OF IT_STALIGHT OCCURS 0,
        STATVA   LIKE STALIGHT-STATVA,
        STALIGHT LIKE STALIGHT-STALIGHT,
      END OF IT_STALIGHT.

* Short description of IDoc status values
DATA: BEGIN OF IT_TEDS2 OCCURS 30.
        INCLUDE STRUCTURE TEDS2.
DATA: END OF IT_TEDS2.

* export data
DATA : IT_SEND_HEAD TYPE TABLE OF ZMMS0025 WITH HEADER LINE,
       IT_SEND_ITEM TYPE TABLE OF ZMMS0026 WITH HEADER LINE,
       GT_SEND_ITEM TYPE TABLE OF ZMMS0026 WITH HEADER LINE.

DATA : IT_SEND_SEF9 TYPE TABLE OF ZMMS0024 WITH HEADER LINE, "Glovis
       IT_SEND_SAFE TYPE TABLE OF ZMMS0024 WITH HEADER LINE, "KMC
       IT_SEND_SSTX TYPE TABLE OF ZMMS0024 WITH HEADER LINE. "Glovis CN

*__IDOC LOCAL CREATE ADD 04/11/11 PAUL
DATA : L_RCVPOR              LIKE EDP13-RCVPOR,
       IDOC_CONTROL_COMPLETE LIKE EDIDC,
       IDOC_HDR_SEG          LIKE E1ADHDR,
       T_IDOC_DATA           LIKE EDIDD OCCURS 0 WITH HEADER LINE,
       IDOC_SEG              LIKE E1STATE,
       IDOC_OBJ_SEG          LIKE E1PRTOB,
       SEND_CONTROL          LIKE EDIDC OCCURS 0 WITH HEADER LINE,
       IDOC_STATUS           LIKE BDIDOCSTAT,
       ERROR_OCCURED,
       IT_EDIDD              LIKE EDIDD OCCURS 0 WITH HEADER LINE,
       I_IDOC_STATUS         LIKE BDIDOCSTAT OCCURS 0 WITH HEADER LINE.

* IDoc record
TYPES : ITEM_TABLE_TYPE LIKE STANDARD TABLE OF MTREEITM
                       WITH DEFAULT KEY.

DATA : NODE_TABLE TYPE TREEV_NTAB,
       ITEM_TABLE TYPE ITEM_TABLE_TYPE.

* interface destination
DATA : G_DEST(20), G_DEST_CKD(20).
*
DATA:ADM_TXT LIKE ZMMS0027-MESSAG2, "M_2
     T500(500),
     BEGIN OF ALR_EXIST_MSG OCCURS 1,
      DOCNUM  LIKE EDIDC-DOCNUM,
      MESSAG2 LIKE ZMMS0027-MESSAG2,
     END OF ALR_EXIST_MSG,
     U_TRAID LIKE LIKP-TRAID,
     U_FIND_TRAID TYPE CHAR1,
     MESS2 LIKE ZMMS0027-MESSAG2,
     BEGIN OF IT_MSG OCCURS 1,
      DOCNUM LIKE EDIDC-DOCNUM,
      MESSAG2 LIKE ZMMS0027-MESSAG2,
     END OF IT_MSG,
     IT_ID TYPE ZMMS0027 OCCURS 1 WITH HEADER LINE.
*-----------------------------------------*
* Constants                               *
*-----------------------------------------*


*===============================================================*
* Selection screen                                              *
*===============================================================*
*--> block 1
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-B01.
SELECT-OPTIONS :
     S_DOCNUM   FOR EDIDC-DOCNUM,
     S_LIFNR    FOR LFA1-LIFNR.
SELECTION-SCREEN END OF BLOCK BLOCK1.
PARAMETERS:P_64 AS CHECKBOX DEFAULT 'X'. "M_1
SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-B03.

*selection-screen uline.
PARAMETERS:ADDMESS AS CHECKBOX. "M_2
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:ADM_VEND FOR LIKP-LIFNR. "M_2
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BLOCK3 WITH FRAME TITLE TEXT-B04."M_3
SELECT-OPTIONS:EXI_VEND FOR LIKP-LIFNR. "M_3
SELECTION-SCREEN END OF BLOCK BLOCK3.

SELECTION-SCREEN END OF BLOCK BLOCK2.
*
PARAMETERS : P_DEST(3) TYPE C.             "interface destination
*===============================================================*
* Events                                                        *
*===============================================================*
INITIALIZATION.
  PERFORM CHECK_OTHER_RUNNING.

AT SELECTION-SCREEN .

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.
  PERFORM RUN.

END-OF-SELECTION.
  PERFORM LIST.

TOP-OF-PAGE.

*===============================================================*
* Subroutine                                                    *
*===============================================================*
*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM RUN .

  PERFORM READ_IDOC_TABLE.
  PERFORM READ_STATUS_TABLE.
  PERFORM ARRANGE_LIST.


  CHECK SY-BATCH = 'X'.
  PERFORM BATCH_TARGET_ITEM.
  PERFORM ALV_COMMAND_SAVE.
ENDFORM.                    " run
*&---------------------------------------------------------------------*
*&      Form  read_idoc_table
*&---------------------------------------------------------------------*
*       read idoc table
*----------------------------------------------------------------------*
FORM READ_IDOC_TABLE .
  REFRESH IT_EDIDC.

  SELECT *
    FROM EDIDC
    INTO TABLE IT_EDIDC
   WHERE DOCNUM     IN S_DOCNUM
*     AND status NOT IN ('50', '62', '64')
     AND STATUS NOT IN ('50', '62', '68')
     AND DIRECT     EQ '2'
*     AND STDMES     EQ 'DESADV'
** Requested by Prasad on 04/15/11
     AND MESTYP    EQ 'DESADV'
** end of change
     AND REFINT     EQ SPACE
     AND CREDAT     GE '20110706'
     AND SNDPRN     IN S_LIFNR.

ENDFORM.                    " read_idoc_table
*&---------------------------------------------------------------------*
*&      Form  arrange_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ARRANGE_LIST .
  DATA:C_TRAID LIKE LIKP-TRAID,C_LIFEX LIKE LIKP-LIFEX.
  DATA: ACT_LINES TYPE I.
  DATA: MAX_LINES TYPE I VALUE 10000.
  DESCRIBE TABLE IT_EDIDC LINES ACT_LINES.

  REFRESH  IT_LIST.

  LOOP AT IT_EDIDC.
    IF SY-TABIX GT 10000.
      MESSAGE S999(B1) WITH MAX_LINES ' of'
                      ACT_LINES
                      ' Selected Idocs will be shown in the ALV-list'.
      EXIT.
    ENDIF.
    CLEAR IT_LIST.
    MOVE-CORRESPONDING IT_EDIDC TO IT_LIST.

    READ TABLE IT_STACUST  WITH KEY STATUS = IT_EDIDC-STATUS.
    READ TABLE IT_STALIGHT WITH KEY STATVA = IT_STACUST-STATVA.

    IF IT_STALIGHT-STALIGHT = '1' OR IT_STALIGHT-STALIGHT = '3'.
*--> idoc message text
      PERFORM READ_IDOC_MESSAGE USING    IT_LIST-DOCNUM
                                         IT_STALIGHT-STALIGHT
                                         IT_LIST-STATUS
                                         IT_EDIDC-SNDPRN "M_2
                                CHANGING IT_LIST-MSG
                                         IT_STALIGHT-STALIGHT
                                         IT_LIST-RETRY
                                         IT_LIST-CIMTYP. "M_3
    ELSE. "M_3
*"M_3
      IF IT_LIST-TRAID IS INITIAL AND IT_LIST-LIFEX IS INITIAL."M_3
        CALL FUNCTION 'Z_MM_ANALYZE_INCOM_ASN'
             EXPORTING
                  S_DOCNUM         = IT_LIST-DOCNUM
                  S_STATUS         = IT_LIST-STATUS
                  ONLY_LIFEX_TRAID = 'X'
             IMPORTING
                  ID_TRAID         = IT_LIST-TRAID
                  ID_LIFEX         = IT_LIST-LIFEX
             TABLES
                  IT_ID            = IT_ID.
      ENDIF.
      CONCATENATE IT_LIST-TRAID '/' IT_LIST-LIFEX
                          INTO IT_LIST-CIMTYP."M_3
*"M_3
    ENDIF.
    CASE IT_STALIGHT-STALIGHT.
      WHEN '1'.
        MOVE ICON_YELLOW_LIGHT TO IT_LIST-STATUSICON.
        IT_LIST-RCODE = '000'.
      WHEN '2'.
        MOVE ICON_GREEN_LIGHT TO IT_LIST-STATUSICON.
        IT_LIST-RCODE = '000'.
      WHEN '3'.
        MOVE ICON_RED_LIGHT TO IT_LIST-STATUSICON.
        IT_LIST-RCODE = '100'.
      WHEN OTHERS.
        MOVE ICON_YELLOW_LIGHT TO IT_LIST-STATUSICON.
    ENDCASE.

    MOVE TEXT-017        TO IT_LIST-DIRECTTEXT.
    MOVE IT_EDIDC-SNDPRT TO IT_LIST-PARTNR(2). "partnerart
    MOVE '/'             TO IT_LIST-PARTNR+2(1).
    MOVE IT_EDIDC-SNDPFC TO IT_LIST-PARTNR+3(2). "partnerrolle
    MOVE '/'             TO IT_LIST-PARTNR+5(1).
    MOVE IT_EDIDC-SNDPRN TO IT_LIST-PARTNR+6(10). "partnernummer
    MOVE IT_EDIDC-RCVPRT TO IT_LIST-IDENT(2). "partnerart
    MOVE '/'             TO IT_LIST-IDENT+2(1).
    MOVE IT_EDIDC-RCVPFC TO IT_LIST-IDENT+3(2). "partnerrolle
    MOVE '/'             TO IT_LIST-IDENT+5(1).
    MOVE IT_EDIDC-RCVPRN TO IT_LIST-IDENT+6(10). "partnernummer
    MOVE IT_EDIDC-SNDPOR TO IT_LIST-RCVPOR.
    MOVE IT_EDIDC-RCVPOR TO IT_LIST-SNDPOR.
*    ENDIF.

*--> read text
    PERFORM FUNC_IDOC_READ_COMPLETELY USING    IT_LIST-DOCNUM
                                      CHANGING IT_LIST-PARTNER_IDLF
                                               IT_LIST-PARTNER_IDWE
                                               IT_LIST-NAME1_WE
                                               IT_LIST-LIFEX
                                               IT_LIST-TRAID
                                               IT_LIST-BOLNR.

*    IF it_edidc-sndprn = 'SEF9' OR  it_edidc-sndprn = 'SAFE' .
    IT_LIST-PARTNER_IDLF = IT_EDIDC-SNDPRN.
*    ENDIF.
    APPEND IT_LIST.
  ENDLOOP.


ENDFORM.                    " arrange_list
*&---------------------------------------------------------------------*
*&      Form  list
*&---------------------------------------------------------------------*
*       display list
*----------------------------------------------------------------------*
FORM LIST .
  DESCRIBE TABLE IT_LIST .
  IF SY-TFILL > 0.
    PERFORM ALV.
  ELSE.
    MESSAGE S429(MO) .
  ENDIF.
ENDFORM.                    " list
*&---------------------------------------------------------------------*
*&      Form  alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV .
  PERFORM ALV_CONTENT.
  PERFORM ALV_FUNC_GRID.
ENDFORM.                    " alv
*&---------------------------------------------------------------------*
*&      Form  alv_content
*&---------------------------------------------------------------------*
*       make alv variants
*----------------------------------------------------------------------*
FORM ALV_CONTENT .
*** variant
  G_REPID = SY-REPID.

*** layout.
  CLEAR IS_LAYOUT.
  IS_LAYOUT-ZEBRA                 = 'X'.
  IS_LAYOUT-BOX_FIELDNAME         = 'MARK'.
*  is_layout-coltab_fieldname = 'COLOR'.
*  is_layout-no_subtotals     = 'X'.

*** grid_settings
*  i_grid_settings-edt_cll_cb = 'X'.

*** field catalog
  PERFORM ALV_FIELDCAT.

ENDFORM.                    " alv_content
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_FIELDCAT .
  DATA : L_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  DATA : COL_POS(2) TYPE N.

  REFRESH IT_FIELDCAT.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME       = 'LISTEDIDC'
       CHANGING
            CT_FIELDCAT            = IT_FIELDCAT
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT IT_FIELDCAT INTO L_FIELDCAT.
    CASE L_FIELDCAT-FIELDNAME.
      WHEN 'DOCNUM'.
        L_FIELDCAT-KEY                 = 'X'.
*        l_fieldcat-hotspot             = 'X'.
        MODIFY IT_FIELDCAT FROM L_FIELDCAT.
      WHEN 'MESTYP'.
        L_FIELDCAT-OUTPUTLEN        = 10.
        MODIFY IT_FIELDCAT FROM L_FIELDCAT.
      WHEN 'DIRECT' OR 'STATUS'.
        L_FIELDCAT-OUTPUTLEN        = 6.
        MODIFY IT_FIELDCAT FROM L_FIELDCAT.
      WHEN 'ARCKEY'.
        L_FIELDCAT-OUTPUTLEN           = 12. "M_3 orig was 16
        L_FIELDCAT-REPTEXT_DDIC        = 'ASN number'.
        L_FIELDCAT-SELTEXT_L           = 'ASN number'.
        L_FIELDCAT-SELTEXT_M           = 'ASN number'.
        L_FIELDCAT-SELTEXT_S           = 'ASN no'.
        MODIFY IT_FIELDCAT FROM L_FIELDCAT.
      WHEN 'CIMTYP'.
        L_FIELDCAT-OUTPUTLEN           = 16.
        L_FIELDCAT-REPTEXT_DDIC        = 'ASN-ID'.
        L_FIELDCAT-SELTEXT_L           = 'TRAID/LIFEX'.
        L_FIELDCAT-SELTEXT_M           = 'TRAID/LIFEX'.
        L_FIELDCAT-SELTEXT_S           = 'ASN-ID'.
        MODIFY IT_FIELDCAT FROM L_FIELDCAT.
      WHEN 'REFMES'.
        L_FIELDCAT-OUTPUTLEN           = 10.
        L_FIELDCAT-REPTEXT_DDIC        = 'Rev. date'.
        L_FIELDCAT-SELTEXT_L           = 'Receiving date'.
        L_FIELDCAT-SELTEXT_M           = 'Receive date'.
        L_FIELDCAT-SELTEXT_S           = 'Rev. date'.
        MODIFY IT_FIELDCAT FROM L_FIELDCAT.
      WHEN 'RCVPOR' OR 'MESCOD' OR 'MESFCT' OR 'TEST' OR 'SNDPOR' OR
           'CIMTYP' OR 'IDOCTP' OR 'DOCREL' OR 'STD' OR 'STATXT' OR
           'ARCKEY' OR 'STDVRS' OR 'STDMES' OR 'OUTMOD' OR
           'SERIAL' OR 'UPDDAT' OR 'UPDTIM' OR 'IDENT' OR
           'MAXSEGNUM' OR 'DIRECTTEXT' OR 'REFGRP'.
        DELETE TABLE IT_FIELDCAT FROM L_FIELDCAT.
    ENDCASE.
    COL_POS = L_FIELDCAT-COL_POS.
  ENDLOOP.

  CLEAR L_FIELDCAT.
  COL_POS = COL_POS + 1.
  L_FIELDCAT-COL_POS             = COL_POS.
  L_FIELDCAT-FIELDNAME           = 'RCODE'.
  L_FIELDCAT-REPTEXT_DDIC        = 'Result code'.
  L_FIELDCAT-OUTPUTLEN           = 6.
  APPEND L_FIELDCAT TO IT_FIELDCAT.

  CLEAR L_FIELDCAT.
  COL_POS = COL_POS + 1.
  L_FIELDCAT-COL_POS             = COL_POS.
  L_FIELDCAT-FIELDNAME           = 'MSG'.
  L_FIELDCAT-REPTEXT_DDIC        = 'Message'.
  L_FIELDCAT-OUTPUTLEN           = 40.
  APPEND L_FIELDCAT TO IT_FIELDCAT.

ENDFORM.                    " alv_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_func_grid
*&---------------------------------------------------------------------*
*       CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*----------------------------------------------------------------------*
FORM ALV_FUNC_GRID .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_REPID
            I_BACKGROUND_ID          = G_BACKGROUND_ID
            I_CALLBACK_PF_STATUS_SET = G_STATUS
            I_CALLBACK_USER_COMMAND  = G_USER_COMMAND
            I_GRID_SETTINGS          = I_GRID_SETTINGS
            IS_LAYOUT                = IS_LAYOUT
            IT_FIELDCAT              = IT_FIELDCAT
            IT_SORT	                 = IT_SORT
            I_SAVE                   = 'X'
            IT_EVENTS                = IT_EVENTS
       TABLES
            T_OUTTAB                 = IT_LIST.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " alv_func_grid
*&---------------------------------------------------------------------*
*&      Form  alv_status
*&---------------------------------------------------------------------*
*      main  pf status
*----------------------------------------------------------------------*
FORM ALV_STATUS USING EXTAB TYPE SLIS_T_EXTAB .
  DATA : L_EXTAB TYPE SLIS_EXTAB.
  SET PF-STATUS 'GRID'. " EXCLUDING extab.
ENDFORM.                    " pf_status
*&---------------------------------------------------------------------*
*&      Form  alv_command
*&---------------------------------------------------------------------*
*      alv command
*----------------------------------------------------------------------*
FORM ALV_COMMAND USING U_UCOMM     LIKE SY-UCOMM
                       RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE U_UCOMM.
    WHEN 'REFR'.


    WHEN '&IC1'.
      PERFORM ALV_COMMAND_LINK USING RS_SELFIELD-FIELDNAME
                                     RS_SELFIELD-VALUE
                                     RS_SELFIELD-TABINDEX.
    WHEN 'SAVE'.
      PERFORM ALV_COMMAND_SAVE.

    WHEN 'MSG'.
      PERFORM ALV_COMMAND_SHOW_MESSAGE.

    WHEN OTHERS.

  ENDCASE.
  RS_SELFIELD-REFRESH    = 'X'.
  RS_SELFIELD-COL_STABLE = 'X'.
  RS_SELFIELD-ROW_STABLE = 'X'.
ENDFORM.                    "user_command_alv
*&---------------------------------------------------------------------*
*&      Form  read_status_table
*&---------------------------------------------------------------------*
*       read status table
*----------------------------------------------------------------------*
FORM READ_STATUS_TABLE .
  REFRESH : IT_TEDS2, IT_STACUST, IT_STALIGHT.
* Statustext
  SELECT *
    FROM TEDS2
    INTO TABLE IT_TEDS2
   WHERE LANGUA EQ SY-LANGU.
* Statusgroups
  SELECT *
    FROM STACUST
    INTO CORRESPONDING FIELDS OF TABLE IT_STACUST.

* Traffic Light Assignment
  SELECT *
    FROM STALIGHT
    INTO CORRESPONDING FIELDS OF TABLE IT_STALIGHT.
ENDFORM.                    " read_status_table
*&---------------------------------------------------------------------*
*&      Form  alv_command_link
*&---------------------------------------------------------------------*
*       link detail screen
*----------------------------------------------------------------------*
FORM ALV_COMMAND_LINK  USING    U_FIELDNAME
                                U_VALUE
                                U_TABINDEX.
  CASE U_FIELDNAME.
    WHEN 'DOCNUM'.
      RANGES : RANGE_CREDAT FOR EDIDC-CREDAT.
      SUBMIT RSEIDOC2 WITH DOCNUM = U_VALUE
                      WITH CREDAT IN RANGE_CREDAT
                      AND  RETURN.
  ENDCASE.

ENDFORM.                    " alv_command_link
*&---------------------------------------------------------------------*
*&      Form  read_idoc_message
*&---------------------------------------------------------------------*
*       read idoc message
*   return code     message
*   1 (Warning)    2nd message of IDoc status record
*   2 (Success)    blank
*   3 (Error)      1st message of IDoc status record
*     in error case
*      send only error message

*->    Retry data
*   When, msgid = ME msgno = 185, 186 and status = 51
*   or status = 64
*   Execute a program in RBDINPUT by background
*   then... if Result is success
*   Status code will be changed 52.
*----------------------------------------------------------------------*
FORM READ_IDOC_MESSAGE  USING    U_DOCNUM
                                 U_RECODE
                                 U_STATUS
                                 U_PARTNER "M_2
                        CHANGING C_MSG
                                 C_RECODE
                                 C_RETRY
                                 C_CIMTYP."M_3

  DATA: R_LIKP LIKE LIKP,CHECK_ASN_EXIST,T30(30),TGR(30),"M_3
        C_TRAID LIKE LIKP-TRAID,C_LIFEX LIKE LIKP-LIFEX.
  DATA : L_MSG     TYPE IDOC_MSG,
         T_MSG     TYPE IDOC_MSG,
         L_STRLEN  TYPE I.         "text field length

  DATA : MSGID  LIKE SY-MSGID,
         MSGNO  LIKE SY-MSGNO,
         STATYP LIKE SY-MSGTY.

  DATA : BEGIN OF T_IDOC_STATUS OCCURS 0.
          INCLUDE STRUCTURE EDIDS.
  DATA : END OF T_IDOC_STATUS.

  DATA : BEGIN OF STATUS_CODE,
          SAP(3) TYPE C,
          MSGID  LIKE SY-MSGID,
          MSGNO  LIKE SY-MSGNO,
         END OF STATUS_CODE.

  DATA : GD_EDIDS      TYPE EDIDS.
  DATA : L_VBELN TYPE VBUK-VBELN.

* Read data records.
  SELECT *
    FROM EDIDS
    INTO TABLE T_IDOC_STATUS
   WHERE DOCNUM  EQ U_DOCNUM
     AND STAMID  NE SPACE
     AND STAMNO  NE SPACE
     AND STATUS  NOT IN ('62', '50').

* Get most recent status record
  SORT T_IDOC_STATUS DESCENDING BY COUNTR.

*==> Delete all status records that have another creation time or
*    do not fit the select pattern.
  LOOP AT T_IDOC_STATUS.
    IF U_DOCNUM <> GD_EDIDS-DOCNUM.
      MOVE T_IDOC_STATUS TO GD_EDIDS.
    ENDIF.
    CHECK NOT ( T_IDOC_STATUS-CREDAT = GD_EDIDS-CREDAT
        AND     T_IDOC_STATUS-CRETIM = GD_EDIDS-CRETIM ) .
    DELETE T_IDOC_STATUS.
  ENDLOOP.


  LOOP AT T_IDOC_STATUS.
* Get msgid and msgno from STAMID, STAMNO. If empty, use STACOD.
    CLEAR : MSGID, MSGNO, STATYP, L_MSG.
    MSGID  = T_IDOC_STATUS-STAMID.
    MSGNO  = T_IDOC_STATUS-STAMNO.
    STATYP = T_IDOC_STATUS-STATYP.
    IF STATYP = SPACE.
      STATYP = 'E'.
    ENDIF.
*==> check a locked idoc document
    IF ( U_STATUS = '51' AND MSGID = 'ME' AND
       ( MSGNO = '185' OR MSGNO = '186' ) ) OR U_STATUS = '64'.
      C_RETRY = 'X'.
    ENDIF.

    IF U_RECODE = '3'.   "error
      CHECK STATYP = 'E' OR  STATYP = 'A'.
    ENDIF.
    MESSAGE ID MSGID TYPE STATYP NUMBER MSGNO
       WITH T_IDOC_STATUS-STAPA1
            T_IDOC_STATUS-STAPA2
            T_IDOC_STATUS-STAPA3
            T_IDOC_STATUS-STAPA4
       INTO L_MSG.



*--> in case of resultcode = 'W'
*    overall packing status of all items in inbound delivery ne 'C'
*    change resultcode to 'E'
*    IF u_recode = '1' AND sy-tabix = 1
*       AND t_idoc_status-stapa2 NE space.
    IF T_IDOC_STATUS-STAMID = 'ME' AND T_IDOC_STATUS-STAMNO = '780'
       AND T_IDOC_STATUS-STAPA2 NE SPACE.
      CLEAR L_VBELN.
      L_VBELN = T_IDOC_STATUS-STAPA2.

      SELECT SINGLE *
        FROM VBUK
       WHERE VBELN   = L_VBELN.
*      IF VBUK-PKSTK  NE 'C'.
      IF VBUK-PKSTK  EQ 'A' OR
         VBUK-PKSTK  EQ ' ' OR
         VBUK-PKSTK  EQ 'D'.
        C_RECODE  = '3'.
      ENDIF.
    ENDIF.

* make message text
    CONDENSE L_MSG.
    CONCATENATE C_MSG L_MSG INTO T_MSG SEPARATED BY SPACE.
    CLEAR C_MSG.
    C_MSG = T_MSG.
    CLEAR : T_MSG, L_STRLEN.
    L_STRLEN = STRLEN( C_MSG ).
    CHECK L_STRLEN > 250.
    EXIT.
  ENDLOOP.

  IF ADDMESS = 'X' AND U_PARTNER IN ADM_VEND. "M_2
    IF U_PARTNER IN EXI_VEND. "M_3
      CHECK_ASN_EXIST = 'X'.
    ELSE.
      CLEAR CHECK_ASN_EXIST.
    ENDIF.
    CALL FUNCTION 'Z_MM_ANALYZE_INCOM_ASN'     "M_2
       EXPORTING  S_DOCNUM  = U_DOCNUM
                  S_STATUS  = U_STATUS
                  P_TRAID   = U_TRAID
                  CHECK_ASN_EXIST = CHECK_ASN_EXIST "M_3
       IMPORTING  MESSAG2    = MESS2
                  FIND_TRAID = U_FIND_TRAID
                  R_LIKP     = R_LIKP               "M_3
                  ID_TRAID   = C_TRAID
                  ID_LIFEX   = C_LIFEX
       TABLES IT_ID = IT_ID.
    IF NOT R_LIKP IS INITIAL. "M_3
      CONCATENATE R_LIKP-TRAID '/' R_LIKP-LIFEX INTO T500.
      CONCATENATE R_LIKP-ERDAT '-' R_LIKP-ERNAM INTO T30.
      IF R_LIKP-WADAT_IST IS INITIAL.
        TGR = 'No GR'.
      ELSE.
        CONCATENATE 'GR' R_LIKP-WADAT_IST INTO TGR SEPARATED BY SPACE.
      ENDIF.
*
      CONCATENATE 'ASN' T500 'Already in KMMG' T30 TGR
                  INTO ALR_EXIST_MSG-MESSAG2 SEPARATED BY SPACE.
      MOVE U_DOCNUM TO ALR_EXIST_MSG-DOCNUM.
      APPEND ALR_EXIST_MSG.
*
      CONCATENATE 'ASN' T500 'Already in KMMG' T30 TGR '|..|' C_MSG
                   INTO T500 SEPARATED BY SPACE.
      MOVE T500(255) TO C_MSG.
    ENDIF.
*
    CONCATENATE C_TRAID '/' C_LIFEX INTO C_CIMTYP."M_3
*
    IF NOT MESS2 IS INITIAL.
      MOVE:U_DOCNUM TO IT_MSG-DOCNUM,
           MESS2 TO IT_MSG-MESSAG2.
      APPEND IT_MSG.
      CONCATENATE C_MSG '|..|' MESS2 INTO T500 SEPARATED BY SPACE.
      MOVE T500(255) TO C_MSG.
    ENDIF.
  ENDIF.

ENDFORM.                    " read_idoc_message
*&---------------------------------------------------------------------*
*&      Form  alv_command_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_COMMAND_SAVE .
  CALL FUNCTION 'MESSAGES_INITIALIZE'.
*  PERFORM send_retry_idoc. "M_1 rem
  PERFORM SEND_RETRY_IDOC_NEW.                              "M_1 ins
  PERFORM SEND_ITEM_COLLECT.

* {
  CLEAR : IT_SEND_ITEM[], IT_SEND_ITEM.
  IT_SEND_ITEM[] = GT_SEND_ITEM[].
*}

  PERFORM SEND_ITEM.
  PERFORM SEND_ITEM_RESULT.
ENDFORM.                    " alv_command_save
*&---------------------------------------------------------------------*
*&      Form  send_item_collect
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SEND_ITEM_COLLECT .
  DATA : L_POS(3) TYPE N.

  REFRESH : IT_SEND_HEAD, IT_SEND_ITEM, IT_SEND_SEF9, IT_SEND_SAFE,
IT_SEND_SSTX.
  CLEAR : GT_SEND_ITEM[], GT_SEND_ITEM.

  LOOP AT IT_LIST WHERE MARK = 'X' AND RETRY = SPACE.
*--> lock item
*    CALL FUNCTION 'ENQUEUE_ES_EDIDOCE'
*      EXPORTING
*        mode_edidc     = 'E'
*        mandt          = sy-mandt
*        docnum         = it_list-docnum
*      EXCEPTIONS
*        foreign_lock   = 1
*        system_failure = 2
*        OTHERS         = 3.
*    IF sy-subrc <> 0.
*      CALL FUNCTION 'MESSAGE_STORE'
*        EXPORTING
*          arbgb                  = sy-msgid
*          msgty                  = sy-msgty
*          msgv1                  = sy-msgv1
*          msgv2                  = sy-msgv2
*          msgv3                  = sy-msgv3
*          msgv4                  = sy-msgv4
*          txtnr                  = sy-msgno
*          zeile                  = it_list-docnum
*        EXCEPTIONS
*          message_type_not_valid = 1
*          not_active             = 2.
*    ENDIF.
*
*    CHECK sy-subrc = 0.
    CLEAR : IT_SEND_HEAD, IT_SEND_SEF9, IT_SEND_SAFE, IT_SEND_SSTX.

    CASE IT_LIST-PARTNER_IDLF.
      WHEN 'SEF9'.    "Glovis
        MOVE-CORRESPONDING IT_LIST TO IT_SEND_SEF9.
        IT_SEND_SEF9-MESSAGE = IT_LIST-MSG.
**C__ Paul change.
        IF IT_SEND_SEF9-RCODE = '000'.
          IT_SEND_SEF9-REFMES = IT_LIST-MSG+1(10).
        ENDIF.
**E__ 06/30/11
        APPEND IT_SEND_SEF9.
**C__ Paul change.
*      WHEN 'SAFE'.    "KMC
      WHEN 'SBC3'.    "KMC
**E__ 06/16/11
        MOVE-CORRESPONDING IT_LIST TO IT_SEND_SAFE.
        IT_SEND_SAFE-MESSAGE = IT_LIST-MSG.
**C__ Paul change.
        IF IT_SEND_SAFE-RCODE = '000'.
          IT_SEND_SAFE-REFMES = IT_LIST-MSG+1(10).
        ENDIF.
**E__ 06/30/11
        APPEND IT_SEND_SAFE.
      WHEN 'SSTX'.    "Glovis China  03/20/2010 by Victor
        MOVE-CORRESPONDING IT_LIST TO IT_SEND_SSTX.
        IT_SEND_SSTX-MESSAGE = IT_LIST-MSG.
**C__ Paul change.
        IF IT_SEND_SSTX-RCODE = '000'.
          IT_SEND_SSTX-REFMES = IT_LIST-MSG+1(10).
        ENDIF.
**E__ 06/30/11
        APPEND IT_SEND_SSTX.
      WHEN OTHERS.
        MOVE-CORRESPONDING IT_LIST TO IT_SEND_HEAD.
        IT_SEND_ITEM-MESSAGE = IT_LIST-MSG.
**C__ Paul change.
**        IF IT_SEND_ITEM-RCODE = '000'.
**          IT_SEND_ITEM-REFMES = IT_LIST-MSG+1(10).
**        ENDIF.
**E__ 06/30/11
        L_POS                = L_POS + 1.
        IT_SEND_HEAD-POS     = L_POS.

        PERFORM GET_ITEM_MESSAGE USING IT_SEND_HEAD-POS
                                       IT_LIST-DOCNUM
                                       IT_LIST-LIFEX
                                       IT_LIST-RCODE.

**
        PERFORM CREATE_ALEAUD_IDOC_CREATE.

        COLLECT IT_SEND_HEAD.

    ENDCASE.
  ENDLOOP.
ENDFORM.                    " send_item_collect
*&---------------------------------------------------------------------*
*&      Form  send_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SEND_ITEM .
  CLEAR : G_DEST, G_DEST_CKD.
*  CONCATENATE 'KMMG_EDI_CLNT' p_dest INTO g_dest.  "Target destination
*  CONCATENATE 'KMMG_EAI_CLNT' p_dest INTO g_dest_ckd. "Glovis/KMC Desc

*--> sending data
  IF NOT  IT_SEND_HEAD[] IS INITIAL.
    CALL FUNCTION 'Z_MM_IF_OB_03_001_DB'
         TABLES
              T_HEAD = IT_SEND_HEAD
              T_ITEM = IT_SEND_ITEM.
  ENDIF.

*--> only sending glovis and KMC data
  IF NOT  IT_SEND_SEF9[] IS INITIAL.
    CALL FUNCTION 'Z_MM_IF_OB_03_002_DB'
         TABLES
              T_IDOC = IT_SEND_SEF9.
  ENDIF.
  IF NOT IT_SEND_SAFE[] IS  INITIAL.
    CALL FUNCTION 'Z_MM_IF_OB_03_002_DB'
*      DESTINATION g_dest_ckd
      TABLES
        T_IDOC = IT_SEND_SAFE.
  ENDIF.

*-- glovis China 03/30/2010 by Victor
  IF  NOT IT_SEND_SSTX[] IS INITIAL.
    CALL FUNCTION 'Z_MM_IF_OB_03_002_DB'
         TABLES
              T_IDOC = IT_SEND_SSTX.
  ENDIF.
ENDFORM.                    " send_item
*&---------------------------------------------------------------------*
*&      Form  send_item_result
*&---------------------------------------------------------------------*
*       check return message
*----------------------------------------------------------------------*
FORM SEND_ITEM_RESULT .
*--> error message
  LOOP AT IT_SEND_HEAD WHERE IF_RETURN = 'E'.
    LOOP AT IT_SEND_ITEM WHERE POS = IT_SEND_HEAD-POS.
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
                ARBGB                  = '25'
                MSGTY                  = 'E'
                MSGV1                  = IT_SEND_ITEM-DOCNUM
                MSGV2                  = ''
                MSGV3                  = ''
                MSGV4                  = ''
                TXTNR                  = '206'
                ZEILE                  = IT_SEND_ITEM-DOCNUM
           EXCEPTIONS
                MESSAGE_TYPE_NOT_VALID = 1
                NOT_ACTIVE             = 2.
    ENDLOOP.
  ENDLOOP.
  IF SY-SUBRC NE 0 AND NOT IT_SEND_HEAD[] IS  INITIAL.
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB                  = 'FMBAPI'
              MSGTY                  = 'S'
              MSGV1                  = 'Local vendor ASN'
              MSGV2                  = ''
              MSGV3                  = ''
              MSGV4                  = ''
              TXTNR                  = '025'
              ZEILE                  = '001'
         EXCEPTIONS
              MESSAGE_TYPE_NOT_VALID = 1
              NOT_ACTIVE             = 2.
  ENDIF.

  LOOP AT IT_SEND_SEF9 WHERE IF_RETURN = 'E'.
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB                  = '25'
              MSGTY                  = 'E'
              MSGV1                  = IT_SEND_SEF9-DOCNUM
              MSGV2                  = ''
              MSGV3                  = ''
              MSGV4                  = ''
              TXTNR                  = '206'
              ZEILE                  = IT_SEND_SEF9-DOCNUM
         EXCEPTIONS
              MESSAGE_TYPE_NOT_VALID = 1
              NOT_ACTIVE             = 2.
  ENDLOOP.
  IF SY-SUBRC NE 0 AND NOT  IT_SEND_SEF9[] IS INITIAL.
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB                  = 'FMBAPI'
              MSGTY                  = 'S'
              MSGV1                  = 'CKD vendor ASN'
              MSGV2                  = ''
              MSGV3                  = ''
              MSGV4                  = ''
              TXTNR                  = '025'
              ZEILE                  = '001'
         EXCEPTIONS
              MESSAGE_TYPE_NOT_VALID = 1
              NOT_ACTIVE             = 2.
  ENDIF.

  LOOP AT IT_SEND_SAFE WHERE IF_RETURN = 'E'.
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB                  = '25'
              MSGTY                  = 'E'
              MSGV1                  = IT_SEND_SAFE-DOCNUM
              MSGV2                  = ''
              MSGV3                  = ''
              MSGV4                  = ''
              TXTNR                  = '206'
              ZEILE                  = IT_SEND_SAFE-DOCNUM
         EXCEPTIONS
              MESSAGE_TYPE_NOT_VALID = 1
              NOT_ACTIVE             = 2.
  ENDLOOP.
  IF SY-SUBRC NE 0 AND NOT IT_SEND_SAFE[] IS INITIAL.
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB                  = 'FMBAPI'
              MSGTY                  = 'S'
              MSGV1                  = 'CKD vendor ASN'
              MSGV2                  = ''
              MSGV3                  = ''
              MSGV4                  = ''
              TXTNR                  = '025'
              ZEILE                  = '001'
         EXCEPTIONS
              MESSAGE_TYPE_NOT_VALID = 1
              NOT_ACTIVE             = 2.
  ENDIF.

*-03/30/2010 by Victor  Glovis China
  LOOP AT IT_SEND_SSTX WHERE IF_RETURN = 'E'.
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB                  = '25'
              MSGTY                  = 'E'
              MSGV1                  = IT_SEND_SSTX-DOCNUM
              MSGV2                  = ''
              MSGV3                  = ''
              MSGV4                  = ''
              TXTNR                  = '206'
              ZEILE                  = IT_SEND_SSTX-DOCNUM
         EXCEPTIONS
              MESSAGE_TYPE_NOT_VALID = 1
              NOT_ACTIVE             = 2.
  ENDLOOP.
  IF SY-SUBRC NE 0 AND NOT IT_SEND_SSTX[] IS INITIAL.
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB                  = 'FMBAPI'
              MSGTY                  = 'S'
              MSGV1                  = 'CKD vendor ASN'
              MSGV2                  = ''
              MSGV3                  = ''
              MSGV4                  = ''
              TXTNR                  = '025'
              ZEILE                  = '001'
         EXCEPTIONS
              MESSAGE_TYPE_NOT_VALID = 1
              NOT_ACTIVE             = 2.
  ENDIF.

*--> Update success item to DB table
** Changed on 08/31/11 by Furong - update Idoc ack. for all ('E')
  LOOP AT IT_SEND_HEAD.   " WHERE IF_RETURN = 'S'.
    LOOP AT IT_SEND_ITEM WHERE POS = IT_SEND_HEAD-POS.
      UPDATE EDIDC SET REFINT   = 'Acknowledged'
                   WHERE DOCNUM = IT_SEND_ITEM-DOCNUM.
    ENDLOOP.
  ENDLOOP.
  LOOP AT IT_SEND_SEF9.   " WHERE IF_RETURN = 'S'.
    UPDATE EDIDC SET REFINT   = 'Acknowledged'
                 WHERE DOCNUM = IT_SEND_SEF9-DOCNUM.
  ENDLOOP.
  LOOP AT IT_SEND_SAFE.  " WHERE IF_RETURN = 'S'.
    UPDATE EDIDC SET REFINT   = 'Acknowledged'
                 WHERE DOCNUM = IT_SEND_SAFE-DOCNUM.
  ENDLOOP.
*-03/30/2010 by Victor
  LOOP AT IT_SEND_SSTX.  " WHERE IF_RETURN = 'S'.
    UPDATE EDIDC SET REFINT   = 'Acknowledged'
                 WHERE DOCNUM = IT_SEND_SSTX-DOCNUM.
  ENDLOOP.

  MESSAGE S526(/SAPHT/DRM01) .    "success message
ENDFORM.                    " send_item_result
*&---------------------------------------------------------------------*
*&      Form  alv_command_show_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_COMMAND_SHOW_MESSAGE .
  CALL FUNCTION 'MESSAGES_SHOW'.
ENDFORM.                    " alv_command_show_message
*&---------------------------------------------------------------------*
*&      Form  batch_target_item
*&---------------------------------------------------------------------*
*       when program is run by background job
*        Choose all items automatically
*----------------------------------------------------------------------*
FORM BATCH_TARGET_ITEM .
  IT_LIST-MARK = 'X'.
  MODIFY IT_LIST TRANSPORTING MARK WHERE MARK = SPACE.
ENDFORM.                    " batch_target_item
*&---------------------------------------------------------------------*
*&      Form  func_IDOC_READ_COMPLETELY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FUNC_IDOC_READ_COMPLETELY  USING    U_DOCNUM
                                CHANGING C_IDLF
                                         C_IDWE
                                         C_NAME1_WE
                                         C_LIFEX
                                         C_TRAID
                                         C_BOLNR.

  DATA : INT_EDIDS LIKE TABLE OF EDIDS WITH HEADER LINE,
         INT_EDIDD LIKE TABLE OF EDIDD WITH HEADER LINE.
  DATA : L_E1ADRM1 TYPE E1ADRM1,
         L_E1EDL20 TYPE E1EDL20.

  CALL FUNCTION 'IDOC_READ_COMPLETELY'
       EXPORTING
            DOCUMENT_NUMBER         = U_DOCNUM
       TABLES
            INT_EDIDS               = INT_EDIDS
            INT_EDIDD               = INT_EDIDD
       EXCEPTIONS
            DOCUMENT_NOT_EXIST      = 1
            DOCUMENT_NUMBER_INVALID = 2
            OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.

    LOOP AT INT_EDIDD WHERE SEGNAM = 'E1EDL20'
                         OR SEGNAM = 'E1ADRM1'.
      CLEAR : L_E1EDL20, L_E1ADRM1.
      CASE INT_EDIDD-SEGNAM.
        WHEN  'E1EDL20'.
          MOVE INT_EDIDD-SDATA TO L_E1EDL20.
          C_LIFEX = L_E1EDL20-LIFEX.
          C_TRAID = L_E1EDL20-TRAID.
          C_BOLNR = L_E1EDL20-BOLNR.

        WHEN 'E1ADRM1'.
          MOVE INT_EDIDD-SDATA TO L_E1ADRM1.
          IF L_E1ADRM1-PARTNER_Q = 'LF'.
            C_IDLF     = L_E1ADRM1-PARTNER_ID.
          ELSEIF L_E1ADRM1-PARTNER_Q = 'WE'.
            C_IDWE     = L_E1ADRM1-PARTNER_ID.
            C_NAME1_WE = L_E1ADRM1-NAME1.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " func_IDOC_READ_COMPLETELY
*&---------------------------------------------------------------------*
*&      Form  get_item_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_ITEM_MESSAGE  USING  U_POS  U_DOCNUM  U_LIFEX  U_RCODE  .
  DATA : L_MSG_SEQ   TYPE EDI_STAMNO.
  DATA : L_MSG     TYPE IDOC_MSG,
         T_MSG     TYPE IDOC_MSG,
         L_FLG,
         L_STRLEN  TYPE I.         "text field length

  DATA : MSGID  LIKE SY-MSGID,
         MSGNO  LIKE SY-MSGNO,
         STATYP LIKE SY-MSGTY.

  DATA : BEGIN OF T_IDOC_STATUS OCCURS 0.
          INCLUDE STRUCTURE EDIDS.
  DATA : END OF T_IDOC_STATUS.

  DATA : BEGIN OF STATUS_CODE,
          SAP(3) TYPE C,
          MSGID  LIKE SY-MSGID,
          MSGNO  LIKE SY-MSGNO,
         END OF STATUS_CODE.

  DATA : GD_EDIDS      TYPE EDIDS.
  DATA : L_VBELN TYPE VBUK-VBELN.

* Read data records.
  SELECT *
    FROM EDIDS
    INTO TABLE T_IDOC_STATUS
   WHERE DOCNUM  EQ U_DOCNUM
     AND STAMID  NE SPACE
     AND STAMNO  NE SPACE
     AND STATUS  NOT IN ('62', '64', '50', '68').


* Get most recent status record
  SORT T_IDOC_STATUS DESCENDING BY COUNTR.

*==> Delete all status records that have another creation time or
*    do not fit the select pattern.
  LOOP AT T_IDOC_STATUS.
    IF U_DOCNUM <> GD_EDIDS-DOCNUM.
      MOVE T_IDOC_STATUS TO GD_EDIDS.
    ENDIF.
    CHECK NOT ( T_IDOC_STATUS-CREDAT = GD_EDIDS-CREDAT
        AND     T_IDOC_STATUS-CRETIM = GD_EDIDS-CRETIM ) .
    DELETE T_IDOC_STATUS.
  ENDLOOP.

  SORT T_IDOC_STATUS.

*  LOOP AT T_IDOC_STATUS.
*S__PAUL
  LOOP AT T_IDOC_STATUS WHERE STATYP EQ 'S'
                           OR STATYP EQ 'E'
                           OR STATYP EQ ''.
* Get msgid and msgno from STAMID, STAMNO. If empty, use STACOD.
    CLEAR : MSGID, MSGNO, STATYP, L_MSG.
    MSGID  = T_IDOC_STATUS-STAMID.
    MSGNO  = T_IDOC_STATUS-STAMNO.
    STATYP = T_IDOC_STATUS-STATYP.
    IF STATYP = SPACE.
      STATYP = 'E'.
    ENDIF.

    IF U_RCODE = '100'.   "error
      CHECK STATYP = 'E' OR  STATYP = 'A'.
    ENDIF.
    MESSAGE ID MSGID TYPE STATYP NUMBER MSGNO
       WITH T_IDOC_STATUS-STAPA1
            T_IDOC_STATUS-STAPA2
            T_IDOC_STATUS-STAPA3
            T_IDOC_STATUS-STAPA4
       INTO L_MSG.

    IF MSGID = 'BORGR' AND
       MSGNO = '520'.
      L_FLG = 'X'.
      L_MSG = T_IDOC_STATUS-STAPA1.
      L_VBELN = L_MSG.
    ENDIF.
* make message text
    L_MSG_SEQ  = L_MSG_SEQ  + 1.
    CLEAR IT_SEND_ITEM.
    IT_SEND_ITEM-POS        = U_POS.
    IT_SEND_ITEM-DOCNUM     = U_DOCNUM.
    IT_SEND_ITEM-LIFEX      = U_LIFEX.
    IT_SEND_ITEM-RCODE      = U_RCODE.
    IT_SEND_ITEM-MSG_SEQ    = L_MSG_SEQ.
    IT_SEND_ITEM-MESSAGE    = L_MSG.
    APPEND IT_SEND_ITEM.

    AT END OF DOCNUM.
      IF L_FLG = 'X'.
        DELETE IT_SEND_ITEM WHERE DOCNUM = U_DOCNUM
                              AND ( MESSAGE <> L_VBELN ).
      ENDIF.
      CLEAR : L_VBELN, L_FLG.
    ENDAT.
  ENDLOOP.

  IF ADDMESS = 'X'. "M_2
    LOOP AT IT_SEND_ITEM.
      READ TABLE IT_MSG WITH KEY DOCNUM = U_DOCNUM.
      IF SY-SUBRC = 0.
        CONCATENATE IT_SEND_ITEM-MESSAGE '|..|' IT_MSG-MESSAG2
                   INTO T500 SEPARATED BY SPACE.
        MOVE T500(255) TO IT_SEND_ITEM-MESSAGE.
        MODIFY IT_SEND_ITEM.
      ENDIF.
*
      READ TABLE ALR_EXIST_MSG WITH KEY DOCNUM = U_DOCNUM.
      IF SY-SUBRC = 0.
        CONCATENATE ALR_EXIST_MSG-MESSAG2  '|..|'  IT_SEND_ITEM-MESSAGE
                   INTO T500 SEPARATED BY SPACE.
        MOVE T500(255) TO IT_SEND_ITEM-MESSAGE.
        MODIFY IT_SEND_ITEM.
      ENDIF.
    ENDLOOP.
  ENDIF.
*
ENDFORM.                    " get_item_message
*&---------------------------------------------------------------------*
*&      Form  send_retry_idoc
*&---------------------------------------------------------------------*
*->    Retry data
*   When, msgid = ME msgno = 185, 186 and status = 51
*   or status = 64
*   Execute a program in RBDMANI2 by background
*   then... if Result is success
*   Status code will be changed 52.
*----------------------------------------------------------------------*
FORM SEND_RETRY_IDOC .
  RANGES : S_DOCNU FOR EDIDC-DOCNUM.
  DATA : L_TABIX TYPE SY-TABIX.

  LOOP AT IT_LIST WHERE RETRY = 'X'.
    L_TABIX         = L_TABIX + 1.
    S_DOCNU-LOW     = IT_LIST-DOCNUM.
    S_DOCNU-SIGN    = 'I'.
    S_DOCNU-OPTION  = 'EQ'.
    APPEND S_DOCNU.
    IF L_TABIX = 100.
      SUBMIT RBDMANI2
        WITH SO_DOCNU IN S_DOCNU
         AND RETURN.
      REFRESH S_DOCNU.
      CLEAR L_TABIX.
    ELSE.
      AT LAST.
        SUBMIT RBDMANI2
          WITH SO_DOCNU IN S_DOCNU
           AND RETURN.
        REFRESH S_DOCNU.
        CLEAR L_TABIX.
      ENDAT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " send_retry_idoc
*&---------------------------------------------------------------------*
*&      Form  send_retry_idoc_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_RETRY_IDOC_NEW . "M_1
  DATA:N64 TYPE I,NN TYPE I.
  RANGES:S_DOCNU FOR EDIDC-DOCNUM,
         S_DOCNU64 FOR EDIDC-DOCNUM.
  DATA : L_TABIX TYPE SY-TABIX.

  LOOP AT IT_LIST WHERE RETRY = 'X'.
    L_TABIX         = L_TABIX + 1.
    S_DOCNU-LOW     = IT_LIST-DOCNUM.
    S_DOCNU-SIGN    = 'I'.
    S_DOCNU-OPTION  = 'EQ'.
    IF IT_LIST-STATUS = 64 AND P_64 = 'X'.
      MOVE S_DOCNU TO S_DOCNU64.
      APPEND S_DOCNU64.
      N64 = N64 + 1.
    ELSE.
      NN = NN + 1.
      APPEND S_DOCNU.
    ENDIF.
    IF L_TABIX = 100.
      IF N64 > 0. "M_1
        SUBMIT RBDAPP01  WITH DOCNUM IN S_DOCNU64 AND RETURN. "M_1
      ENDIF.
      IF NN > 0.
        SUBMIT RBDMANI2 WITH SO_DOCNU IN S_DOCNU AND RETURN.
      ENDIF.
      REFRESH:S_DOCNU,S_DOCNU64.
      CLEAR:L_TABIX,N64,NN.
    ELSE.
      AT LAST.
        IF N64 > 0. "M_1
          SUBMIT RBDAPP01  WITH DOCNUM IN S_DOCNU64 AND RETURN. "M_1
        ENDIF.
        IF NN > 0.
          SUBMIT RBDMANI2 WITH SO_DOCNU IN S_DOCNU AND RETURN.
        ENDIF.
        REFRESH:S_DOCNU,S_DOCNU64.
        CLEAR:L_TABIX,N64,NN.
      ENDAT.
    ENDIF.
  ENDLOOP.
  IF N64 > 0. "M_1
    SUBMIT RBDAPP01  WITH DOCNUM IN S_DOCNU64 AND RETURN.
  ENDIF.
  IF NN > 0.
    SUBMIT RBDMANI2 WITH SO_DOCNU IN S_DOCNU AND RETURN.
  ENDIF.
ENDFORM.                    " send_retry_idoc_new

*&---------------------------------------------------------------------*
*&      Form  CHECK_OTHER_RUNNING
*&---------------------------------------------------------------------*
*       Check Other Running
*----------------------------------------------------------------------*
FORM CHECK_OTHER_RUNNING .
  DATA: LS_PROGRAM TYPE ZMMT0000,
        L_COUNT TYPE I.
  SELECT SINGLE *
    INTO LS_PROGRAM
    FROM ZMMT0000
   WHERE PROGRAMM = SY-CPROG.
  IF SY-SUBRC <> 0.
    LS_PROGRAM-PROGRAMM = SY-CPROG.
    INSERT ZMMT0000 FROM LS_PROGRAM.
  ENDIF.
  CALL FUNCTION 'ENQUEUE_EZ_ZMMT0000'
       EXPORTING
            PROGRAMM       = 'ZMMR30400T'
       EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    LEAVE PROGRAM.
  ENDIF.


*CONSTANTS:
*  btc_running       LIKE tbtco-status VALUE 'R',
*  btc_ready         LIKE tbtco-status VALUE 'Y',
*  btc_scheduled     LIKE tbtco-status VALUE 'P',
*  btc_released      LIKE tbtco-status VALUE 'S',
*  btc_aborted       LIKE tbtco-status VALUE 'A',
*  btc_finished      LIKE tbtco-status VALUE 'F',
*  btc_put_active    LIKE tbtco-status VALUE 'Z',
*  btc_unknown_state LIKE tbtco-status VALUE 'X'.


  SELECT COUNT(*)
    INTO L_COUNT
    FROM  TBTCP AS A INNER JOIN TBTCO AS B
                        ON A~JOBNAME  EQ B~JOBNAME
                       AND A~JOBCOUNT EQ B~JOBCOUNT
   WHERE A~PROGNAME = SY-CPROG
     AND B~STATUS NOT IN ('F', 'A', 'S', 'P').
  IF L_COUNT > 1.
    MESSAGE S127(C$).
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.                    " CHECK_OTHER_RUNNING
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALEAUD_IDOC_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_ALEAUD_IDOC_CREATE.
  DATA : LV_DOCNUM LIKE IT_SEND_ITEM-DOCNUM.
  CLEAR: LV_DOCNUM.

  SELECT SINGLE RCVPOR INTO L_RCVPOR FROM EDP13
     WHERE RCVPRN = IT_LIST-PARTNR+6(10)
       AND RCVPRT = 'LI'
       AND RCVPFC = ''
       AND MESTYP = 'ALEAUD'.

  IF SY-SUBRC NE 0 AND SY-BATCH EQ 'X'.
    CONCATENATE ' Maintain Partner profile for Message type ALEAUD for'
   IT_LIST-PARTNR+6(10) INTO IT_LIST-MSG .
  ELSE.

* Control Record
    IDOC_CONTROL_COMPLETE-DOCTYP = 'ALEAUD01'.
    IDOC_CONTROL_COMPLETE-MESTYP = 'ALEAUD'.
    IDOC_CONTROL_COMPLETE-DIRECT = '1'.
    IDOC_CONTROL_COMPLETE-OUTMOD = '2'.

* Sender of original DESADV willl be receiver
    IDOC_CONTROL_COMPLETE-RCVPRT = IT_LIST-PARTNR(2).
    IDOC_CONTROL_COMPLETE-RCVPRN = IT_LIST-PARTNR+6(10).
* Select Port
    SELECT SINGLE RCVPOR INTO L_RCVPOR FROM EDP13
       WHERE RCVPRN = IDOC_CONTROL_COMPLETE-RCVPRN
         AND RCVPRT = IDOC_CONTROL_COMPLETE-RCVPRT
         AND MESTYP = IDOC_CONTROL_COMPLETE-MESTYP.
    IF SY-SUBRC EQ 0.
      IDOC_CONTROL_COMPLETE-RCVPOR = L_RCVPOR.
    ENDIF.

** Receiver of original DESADV ( SAP)  wil be the sender
    IDOC_CONTROL_COMPLETE-SNDPOR = IT_LIST-SNDPOR.
    IDOC_CONTROL_COMPLETE-SNDPRT = IT_LIST-IDENT(2).
    IDOC_CONTROL_COMPLETE-SNDPRN = IT_LIST-IDENT+6(10).

* Populate E1ADHDR segment
    IDOC_HDR_SEG-MESTYP = 'ALEAUD'.
*S__MOD PAUL
*    IDOC_HDR_SEG-MESCOD = ''.
    IDOC_HDR_SEG-MESCOD = IT_LIST-RCODE.
    IDOC_HDR_SEG-MESFCT = ''.

    IF IT_LIST-RCODE = '000'.
      IDOC_HDR_SEG-MESTYP_LNG = IT_LIST-MSG.
      CONDENSE IDOC_HDR_SEG-MESTYP_LNG.
    ELSE.
      IDOC_HDR_SEG-MESTYP_LNG = 'E'.
    ENDIF.

    T_IDOC_DATA-SEGNAM = 'E1ADHDR'.
    T_IDOC_DATA-SDATA = IDOC_HDR_SEG.
    APPEND T_IDOC_DATA.

    LOOP AT IT_SEND_ITEM.
      IDOC_SEG-DOCNUM = IT_SEND_ITEM-DOCNUM.
      IDOC_SEG-STATXT = IT_SEND_ITEM-MESSAGE." Error Message
      IDOC_SEG-STAPA4 = ''.
      IF IT_SEND_ITEM-RCODE = '000'.
        IDOC_SEG-STATYP = 'Success'.
      ELSE.
        IDOC_SEG-STATYP = 'Error'.
      ENDIF.
      IDOC_SEG-STAMID = IT_SEND_ITEM-POS  .
      IDOC_SEG-STAMNO = IT_SEND_ITEM-MSG_SEQ.
* DEFAULT VALUE
      IF IDOC_SEG-STAPA1_LNG  IS INITIAL.
        IDOC_SEG-STAPA1_LNG = 'Hyundai Car Assembly, AL'.
      ENDIF.
*S__MOD PAUL
      IDOC_SEG-STAPA2_LNG = IT_LIST-UPDDAT. "IDOC creation Dt.
      IDOC_SEG-STAPA3_LNG = IT_LIST-UPDTIM. "IDOC creation time
***     IDOC_SEG-STAPA4_LNG = sy-langu.         "Language
      IDOC_SEG-STAPA4_LNG = IT_SEND_ITEM-LIFEX.  "ASN Number
      T_IDOC_DATA-SEGNAM = 'E1STATE'.
      T_IDOC_DATA-SDATA = IDOC_SEG.
      APPEND T_IDOC_DATA.

* Populate  E1PRTOB segment
      IDOC_OBJ_SEG-DOCNUM  = IT_LIST-DOCNUM."Original IDOC No of ASN
      LV_DOCNUM            = IT_LIST-DOCNUM.
      IDOC_OBJ_SEG-LOGSYS  = IT_LIST-PARTNER_IDLF.   " Partner ID
      IDOC_OBJ_SEG-OBJTYPE = ''.
*      IDOC_OBJ_SEG-OBJKEY = IT_LIST-PARTNER_IDLF.   " Partner Name
      T_IDOC_DATA-SEGNAM = 'E1PRTOB'.
      T_IDOC_DATA-SDATA  = IDOC_OBJ_SEG.
      APPEND T_IDOC_DATA.

    ENDLOOP.

    CALL FUNCTION 'IDOC_CREATE_ON_DATABASE'
         EXPORTING
              IDOC_STATUS             = IDOC_STATUS
              ERROR_OCCURED           = ERROR_OCCURED
         TABLES
              IDOC_DATA               = T_IDOC_DATA
         CHANGING
              IDOC_CONTROL            = IDOC_CONTROL_COMPLETE
         EXCEPTIONS
              IDOC_INPUT_INCONSISTENT = 1
              OTHERS                  = 2.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      CLEAR : T_IDOC_DATA, T_IDOC_DATA[].
    ENDIF.

    APPEND IDOC_CONTROL_COMPLETE TO SEND_CONTROL.

*A__Add by PAUL
    UPDATE EDIDC SET REFINT   = 'Acknowledged'
             WHERE DOCNUM = LV_DOCNUM.
*E__<

*  loop at it_tab where flag  =   'X'.
*    l_docnum = it_tab-docnum.
*    I_IDOC_STATUS-DOCNUM = IT_LIST-DOCNUM.
*    I_IDOC_STATUS-STATUS = '68'.
*    APPEND I_IDOC_STATUS.
*
*    CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
*         EXPORTING
*              IDOC_NUMBER               = IT_LIST-DOCNUM
*              NO_DEQUEUE_FLAG           = 'X'
*         TABLES
*              IDOC_STATUS               = I_IDOC_STATUS
*         EXCEPTIONS
*              IDOC_FOREIGN_LOCK         = 1
*              IDOC_NOT_FOUND            = 2
*              IDOC_STATUS_RECORDS_EMPTY = 3
*              IDOC_STATUS_INVALID       = 4
*              DB_ERROR                  = 5
*              OTHERS                    = 6.
*    CLEAR :  I_IDOC_STATUS[],I_IDOC_STATUS.
*
*  endloop.

    REFRESH IT_EDIDD.
    CLEAR IT_EDIDD.

    CALL FUNCTION 'EDI_OUTPUT_NEW'
         TABLES
              I_EDIDC = SEND_CONTROL
              I_EDIDD = IT_EDIDD
         EXCEPTIONS
              OTHERS  = 0.
** Furong on 03/21/12
    CALL FUNCTION 'DB_COMMIT'.
    CALL FUNCTION 'DEQUEUE_ALL'.
** on 03/21/12
    COMMIT WORK.
    CLEAR: SEND_CONTROL[], SEND_CONTROL.

  ENDIF.

  IT_SEND_HEAD-824 = IDOC_CONTROL_COMPLETE-DOCNUM.

  CLEAR : IDOC_CONTROL_COMPLETE.

  APPEND IT_SEND_ITEM TO GT_SEND_ITEM.

  CLEAR : IT_SEND_ITEM, IT_SEND_ITEM[].

ENDFORM.                    " CREATE_ALEAUD_IDOC_CREATE
