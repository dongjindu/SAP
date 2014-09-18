************************************************************************
*
** Program Name   : ZEMMPM34R_ST_PRICE_MANAGE
** Created by     : Min-su Park
** Created on     : 2003.10.16.
** Pattern        :
** Description    :  Manage Standard Price for Purchase Material
**
** Modification Logs
** Date            Developer        RequestNo      Description
** 2003.10.17.     Min-su Park    UD1K901873     Initial Coding
************************************************************************
*
*
**----------------------------------------------------------------------
*
**   INCLUDE ZEMMPM34R_ST_PRICE_MANAGE_F02
*
**----------------------------------------------------------------------
*
**&---------------------------------------------------------------------
*
**&      Form  UPDATE_MASTER_DATA
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM UPDATE_MASTER_DATA USING IT_MARA STRUCTURE MARA.
*  DATA: WA_HEAD       LIKE BAPIMATHEAD, "Header with control
*information
*        WA_PLANT      LIKE BAPI_MARC  , "plant-specific material DATA
*        WA_PLANTX     LIKE BAPI_MARCX ,
*        WA_MBEW       LIKE BAPI_MBEW  ,
*        WA_MBEWX      LIKE BAPI_MBEWX .
*  DATA: IT_BAPIRET2   LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.
*  CASE W_MARK.
*    WHEN R1.
*      WA_HEAD-MATERIAL     = IT_MARA-MATNR.
*      WA_MBEW-VAL_AREA     = IT_INFO-WERKS.
*      WA_MBEW-PLNDPRICE3   = W_EFFPR      .
*      WA_MBEW-PLNDPRDATE3  = SY-DATUM     .
*      WA_MBEWX-VAL_AREA    = IT_INFO-WERKS.
*      WA_MBEWX-PLNDPRICE3  = 'X'          .
*      WA_MBEWX-PLNDPRDATE3 = 'X'          .
*    WHEN R2.
*      WA_HEAD-MATERIAL     = IT_MARA-MATNR.
*      WA_MBEW-VAL_AREA     = IT_INFO-WERKS.
*      WA_MBEW-PLNDPRICE1   = W_EFFPR      .
*      WA_MBEW-PLNDPRDATE1  = SY-DATUM     .
*      WA_MBEWX-VAL_AREA    = IT_INFO-WERKS.
*      WA_MBEWX-PLNDPRICE1  = 'X'          .
*      WA_MBEWX-PLNDPRDATE1 = 'X'          .
*  ENDCASE.
*  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
*       EXPORTING
*            HEADDATA       = WA_HEAD
*            VALUATIONDATA  = WA_MBEW
*            VALUATIONDATAX = WA_MBEWX
*       TABLES
*            RETURNMESSAGES = IT_BAPIRET2.
*
*  READ TABLE IT_BAPIRET2 WITH KEY TYPE = 'E'.
*  IF SY-SUBRC = 0.  "Error Occurred !
*    MOVE-CORRESPONDING IT_BAPIRET2 TO IT_LOG.
*    IT_LOG-SERIAL = IT_LOG-SERIAL + 1.
*    IT_LOG-MATNR  = IT_MARA-MATNR    .
*    IT_LOG-NUMB   = IT_BAPIRET2-NUMBER.
*    IT_LOG-TCODE  = SY-TCODE.
*    APPEND IT_LOG.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
**     EXPORTING
**       WAIT          =
**     IMPORTING
**       RETURN        =    .
*    IF R2 = 'X'.
*      PERFORM GET_ZTMM_ANALY .
*    ENDIF.
*  ENDIF.
*ENDFORM.                    " UPDATE_MASTER_DATA
**&---------------------------------------------------------------------
*
**&      Form  UPDATE_ZTMM_SPE
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM UPDATE_ZTMM_SPE.
*  CASE W_MARK.
*    WHEN R1.
*      LOOP AT IT_ERROR.
*        IT_ERROR-PTYPE = 'B'.
*        INSERT ZTMM_SPE FROM IT_ERROR.
*      ENDLOOP.
*    WHEN R2.
*      LOOP AT IT_ERROR.
*        IT_ERROR-PTYPE = 'P'.
*        INSERT ZTMM_SPE FROM IT_ERROR.
*      ENDLOOP.
**      CHECK NOT IT_ANALY IS INITIAL.
**      INSERT ZTMM_ANALY FROM TABLE IT_ANALY.
*  ENDCASE.
*ENDFORM.                    " UPDATE_ZTMM_SPE
**&---------------------------------------------------------------------
*
**&      Form  DISPLAY_LOG
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM DISPLAY_LOG.
*  W_REPID = SY-REPID.
*  CLEAR : IT_FIELDCAT[], WA_EVENTS[], WA_LIST_TOP_OF_PAGE[].
*  PERFORM FIELDCAT_INIT  USING IT_FIELDCAT[].
*  PERFORM EVENTTAB_BUILD USING WA_EVENTS[].
*  PERFORM COMMENT_BUILD  USING WA_LIST_TOP_OF_PAGE[].
*  PERFORM ALV_DISPLAY.
*ENDFORM.                    " DISPLAY_LOG
**&---------------------------------------------------------------------
*
**&      Form  FIELDCAT_INIT
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_IT_FIELDCAT[]  text
**----------------------------------------------------------------------
*
*FORM FIELDCAT_INIT USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
*  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
*  DATA: POS TYPE I.
*
**Message type.
*  clear ls_fieldcat.
*  POS = POS + 1.
*  LS_FIELDCAT-COL_POS       = POS.
*  LS_FIELDCAT-FIELDNAME     = 'TYPE'.
*  LS_FIELDCAT-REF_FIELDNAME = ''.
*  LS_FIELDCAT-KEY           = ''.
*  LS_FIELDCAT-QFIELDNAME    = ''.
*  LS_FIELDCAT-CFIELDNAME    = ''.
*  LS_FIELDCAT-SELTEXT_L     = 'Message Type'.
*  LS_FIELDCAT-SELTEXT_M     = 'Message Type'.
*  LS_FIELDCAT-SELTEXT_S     = 'Message Type'.
*  LS_FIELDCAT-OUTPUTLEN     = '3'.
*  LS_FIELDCAT-NO_OUT        = ''.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
**Message class.
*  clear ls_fieldcat.
*  POS = POS + 1.
*  LS_FIELDCAT-COL_POS       = POS.
*  LS_FIELDCAT-FIELDNAME     = 'ID'.
*  LS_FIELDCAT-REF_FIELDNAME = ''.
*  LS_FIELDCAT-KEY           = ''.
*  LS_FIELDCAT-QFIELDNAME    = ''.
*  LS_FIELDCAT-CFIELDNAME    = ''.
*  LS_FIELDCAT-SELTEXT_L     = 'Message Class'.
*  LS_FIELDCAT-SELTEXT_M     = 'Message Class'.
*  LS_FIELDCAT-SELTEXT_S     = 'Message Class'.
*  LS_FIELDCAT-OUTPUTLEN     = '10'.
*  LS_FIELDCAT-NO_OUT        = ''.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
**Message number
*  clear ls_fieldcat.
*  POS = POS + 1.
*  LS_FIELDCAT-COL_POS       = POS.
*  LS_FIELDCAT-FIELDNAME     = 'NUMBER'.
*  LS_FIELDCAT-REF_FIELDNAME = ''.
*  LS_FIELDCAT-KEY           = ''.
*  LS_FIELDCAT-QFIELDNAME    = ''.
*  LS_FIELDCAT-CFIELDNAME    = ''.
*  LS_FIELDCAT-SELTEXT_L     = 'Message Number'.
*  LS_FIELDCAT-SELTEXT_M     = 'Message Number'.
*  LS_FIELDCAT-SELTEXT_S     = 'Message Number'.
*  LS_FIELDCAT-OUTPUTLEN     = '3'.
*  LS_FIELDCAT-NO_OUT        = ''.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
**Message txt
*  clear ls_fieldcat.
*  POS = POS + 1.
*  LS_FIELDCAT-COL_POS       = POS.
*  LS_FIELDCAT-FIELDNAME     = 'MESSAGE'.
*  LS_FIELDCAT-REF_FIELDNAME = ''.
*  LS_FIELDCAT-KEY           = ''.
*  LS_FIELDCAT-QFIELDNAME    = ''.
*  LS_FIELDCAT-CFIELDNAME    = ''.
*  LS_FIELDCAT-SELTEXT_L     = 'Message txt'.
*  LS_FIELDCAT-SELTEXT_M     = 'Message txt'.
*  LS_FIELDCAT-SELTEXT_S     = 'Message txt'.
*  LS_FIELDCAT-OUTPUTLEN     = '100'.
*  LS_FIELDCAT-NO_OUT        = ''.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*ENDFORM.                    " FIELDCAT_INIT
**&---------------------------------------------------------------------
*
**&      Form  EVENTTAB_BUILD
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_WA_EVENTS[]  text
**----------------------------------------------------------------------
*
*FORM EVENTTAB_BUILD USING LT_EVENTS TYPE SLIS_T_EVENT.
*  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
*  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
*       EXPORTING
*            I_LIST_TYPE = 0
*       IMPORTING
*            ET_EVENTS   = LT_EVENTS.
*
*ENDFORM.                    " EVENTTAB_BUILD
**&---------------------------------------------------------------------
*
**&      Form  COMMENT_BUILD
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_WA_LIST_TOP_OF_PAGE[]  text
**----------------------------------------------------------------------
*
*FORM COMMENT_BUILD USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
*  DATA: ls_line TYPE slis_listheader.
*  DATA: info_txt(50).
*
*ENDFORM.                    " COMMENT_BUILD
**&---------------------------------------------------------------------
*
**&      Form  ALV_DISPLAY
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM ALV_DISPLAY.
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      I_CALLBACK_PROGRAM           = W_REPID
**     I_STRUCTURE_NAME             =
**     IT_SORT                      = WA_SORT[]
*      IT_EVENTS                    = WA_EVENTS[]
*      IT_FIELDCAT                  = IT_FIELDCAT[]
** IMPORTING
**   E_EXIT_CAUSED_BY_CALLER        =
**   ES_EXIT_CAUSED_BY_USER         =
*    TABLES
*      T_OUTTAB                     = IT_LOG.
*ENDFORM.                    " ALV_DISPLAY
**&---------------------------------------------------------------------
*
**&      Form  GET_ZTMM_ANALY
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM GET_ZTMM_ANALY.
*  IT_ANALY-BASE_D  = P_DATE       . "Base date
*  IT_ANALY-WERKS   = IT_A017-WERKS. "Plant
*  IT_ANALY-MATNR   = IT_MARA-MATNR. "Material Number
*  IT_ANALY-EKORG   = IT_A017-EKORG. "Purchasing organization
*  IT_ANALY-LIFNR   = IT_A017-LIFNR.
**                                   "Reason Code
*  SELECT SINGLE KZUST
*           INTO IT_ANALY-KZUST
*           FROM KONH
*          WHERE KNUMH = IT_A017-KNUMH.
*
*  IT_ANALY-VALID_D = IT_A017-DATAB. "Valid from
**                                   "Price
*  SELECT SINGLE KBETR KONWA
*         INTO (IT_ANALY-KBETR, IT_ANALY-KONWA)
*         FROM KONP
*        WHERE KNUMH = IT_A017-KNUMH
*          AND KSCHL = 'PB00'      .
*  IT_ANALY-SOURCE  = 'I'          . "SOURCE
*  IT_ANALY-ERDAT   = SY-DATUM     . "WORK DAY(Creation Day)
*
*  IT_ANALY-ERZET   = SY-UZEIT     .
*  IT_ANALY-ERNAM   = SY-UNAME     .
*  APPEND IT_ANALY.
*ENDFORM.                    " GET_ZTMM_ANALY
