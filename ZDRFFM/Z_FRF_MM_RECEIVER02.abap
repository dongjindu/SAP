FUNCTION Z_FRF_MM_RECEIVER02.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_TYPES) TYPE  ZRF_TYPES
*"     VALUE(I_UNAME) LIKE  SY-UNAME
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_TRUCK STRUCTURE  ZSRF_TRUCK OPTIONAL
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE OPTIONAL
*"      T_RECEIVER STRUCTURE  ZSRF_RECEIVER
*&--------------------------------------------------------------------
*& Author                 : Furong Wang
*& Creation Date          : 09/16/2008
*& Specification By       : Ben
*& Pattern                : RFC Function Module
*& Description            : To Create GR for given Deliveries and
*&                          confirm putaway qty for Theda plant
*& Addl documentation     : Copy of Z_FRF_MM_RECEIVER01 Function module
*&
**&--------------------------------------------------------------------

* Data Declarations
  DATA: L_CHECK, " ERROR CHECK
        L_TABIX TYPE SY-TABIX,
        L_CNT TYPE I,
        L_INDEX TYPE I.

  DATA BEGIN OF LT_LTAK OCCURS 0.
          INCLUDE STRUCTURE LTAK_VB.
  DATA END OF LT_LTAK.

  DATA BEGIN OF T_WMGRP_MSG OCCURS 0.
          INCLUDE STRUCTURE WMGRP_MSG.
  DATA  END OF T_WMGRP_MSG.

  DATA:  LT_WORKTAB LIKE LIPOV OCCURS 0 WITH HEADER LINE,
         LT_SUCCESS LIKE LIPOV OCCURS 0 WITH HEADER LINE,
         LT_ERROR LIKE LIPOV OCCURS 0 WITH HEADER LINE,
         LT_TVST LIKE TVST OCCURS 10 WITH HEADER LINE,
         LF_SUCCESS_COUNT TYPE I,
         LF_ERROR_COUNT TYPE I.

  DATA: BEGIN OF LT_TRUCK OCCURS 0,
          RECEIVER TYPE ZSRF_TRUCK-RECEIVER,
          VBELN TYPE ZSRF_TRUCK-VBELN,
          WBSTK TYPE ZSRF_TRUCK-WBSTK,
          KOSTK TYPE ZSRF_TRUCK-KOSTK,
          EXECUTED_FLG TYPE ZSRF_TRUCK-EXECUTED_FLG,
        END OF LT_TRUCK.

  DATA: LT_RECE TYPE ZSRF_RECE_DATA OCCURS 0 WITH HEADER LINE.

  DATA: L_VBELN LIKE ZSRF_TRUCK-VBELN.

  DATA: BEGIN OF IT_VBUP OCCURS 0,
        VBELN LIKE VBUP-VBELN,
        POSNR LIKE VBUP-POSNR,
        KOSTA LIKE  VBUP-KOSTA,
        WBSTA LIKE  VBUP-WBSTA,
        END OF IT_VBUP.

  DATA: IT_LIPS_LNS LIKE TABLE OF LIPS WITH HEADER LINE.

  DATA: WA_MSGNR LIKE SY-MSGNO,
         WA_MSGID LIKE SY-MSGID,
         WA_MSGV1 LIKE SY-MSGV1,
         WA_MSGV2 LIKE SY-MSGV2,
         WA_MSGV3 LIKE SY-MSGV3,
         WA_MSGV4 LIKE SY-MSGV4.

  DATA: L_BLDAT   LIKE BDCDATA-FVAL,
        L_BLDAT_D LIKE SY-DATUM.
*        l_LFIMG like lips-LFIMG.

  DATA: IT_MESSTAB LIKE TABLE OF BDCMSGCOLL WITH HEADER LINE.

  CHECK NOT I_TYPES IS INITIAL.        "External ID CHECK
  CHECK NOT T_RECEIVER[] IS INITIAL.   "External ID CHECK

* Button Click Date & Time
  W_BUTTON_CLICK_DATE = SY-DATUM.
  W_BUTTON_CLICK_TIME = SY-UZEIT.

  REFRESH T_TRUCK. CLEAR: T_TRUCK. CLEAR L_CHECK.
  PERFORM RECEIVER_ITEM_SEARCH TABLES T_RECEIVER
                                      T_TRUCK
                               USING  I_TYPES
                                      L_CHECK.

  REFRESH IT_GR. CLEAR IT_GR.

  IF NOT T_TRUCK[] IS INITIAL.

    SORT T_TRUCK BY VBELN.

*
*    IT_LIKP[]  = T_TRUCK[].                                 "UD1K920227
**
*** Select Unique Materials.
*    PERFORM SELECT_LINE_ITEMS.                              "UD1K920227
*
** Check Material status
*    PERFORM CHECK_MATERIAL_STATUS.                          "UD1K920335

** Search Storage type statergy for stock placement
*    PERFORM  CHECK_STATERGY.                                "UD1K920227

** Eliminate Deliveries for which storage type's are not found
    IF NOT IT_LIPS[] IS INITIAL.
      LOOP AT IT_LIPS WHERE ERR_FLAG  EQ 'X'.
        IT_ERROR-VBELN   =  IT_LIPS-VBELN.
        IT_ERROR-MATNR   =  IT_LIPS-MATNR.
        IT_ERROR-MSG     =  IT_LIPS-MSG.
        APPEND IT_ERROR.
      ENDLOOP.
      LOOP AT IT_LIPS WHERE ERR_FLAG  EQ 'X'.
        READ TABLE T_TRUCK WITH KEY VBELN = IT_LIPS-VBELN.
        IF SY-SUBRC EQ 0.
          DELETE T_TRUCK  WHERE VBELN = IT_LIPS-VBELN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    L_BLDAT_D = SY-DATUM.
    PERFORM USER_DATE_FORMAT USING    SY-UNAME
                                      L_BLDAT_D
                             CHANGING L_BLDAT.

*    LOOP AT T_TRUCK INTO WA_TRUCK WHERE EXECUTED_FLG NE 'X'.
     LOOP AT T_TRUCK INTO WA_TRUCK.
      L_TABIX = SY-TABIX.
      CLEAR: IT_MESSTAB[].
*Delivery  not processed / Total goods movement status
*      IF WA_TRUCK-KOSTK = 'A'.       "Deliveries Not Processed

*    Create TO for Inbound Deliveries in Foreground
*
*        CALL FUNCTION 'L_TO_CREATE_DN'
*          EXPORTING
*            I_LGNUM                          = 'P01'
*            I_VBELN                          =  WA_TRUCK-VBELN
**           I_UPDATE_TASK                    =  ' '
*            I_COMMIT_WORK                    = 'X'
*            I_BNAME                          =  I_UNAME
*         IMPORTING
*            E_TANUM                          =  LT_LTAK-TANUM
*         TABLES
*            T_LTAK                           =  LT_LTAK
**           T_LTAP_VB                        =
*           T_WMGRP_MSG                       =  T_WMGRP_MSG
*         EXCEPTIONS
*           FOREIGN_LOCK                     = 1
*           DN_COMPLETED                     = 2
*           PARTIAL_DELIVERY_FORBIDDEN       = 3
*           XFELD_WRONG                      = 4
*           LDEST_WRONG                      = 5
*           DRUKZ_WRONG                      = 6
*           DN_WRONG                         = 7
*           SQUIT_FORBIDDEN                  = 8
*           NO_TO_CREATED                    = 9
*           TEILK_WRONG                      = 10
*           UPDATE_WITHOUT_COMMIT            = 11
*           NO_AUTHORITY                     = 12
*           NO_PICKING_ALLOWED               = 13
*           DN_HU_NOT_CHOOSABLE              = 14
*           OTHERS                           = 15 .

      L_VBELN = WA_TRUCK-VBELN.
      SELECT VBELN POSNR KOSTA WBSTA INTO TABLE IT_VBUP
          FROM VBUP
          WHERE VBELN = L_VBELN.
      IF IT_VBUP[] IS INITIAL.
        L_CHECK  = 'X'.  "ERROR CHECK
        CONCATENATE 'No availble record(s) to process' L_VBELN INTO
        WA_TEXT.
        WA_MESSA = WA_TEXT.
        T_MESSAGE-TYPE = 'E'.
        T_MESSAGE-ID = SY-MSGID.
        T_MESSAGE-NUMBERS = SY-MSGNO.
        T_MESSAGE-MESSAGE = WA_TEXT.
        APPEND T_MESSAGE.
        WA_TRUCK-EXECUTED_FLG = 'X'.

        WA_TRUCK-EXECUTED_FLG = 'S'.
        MODIFY T_TRUCK FROM WA_TRUCK INDEX L_TABIX
                           TRANSPORTING EXECUTED_FLG.

        CONTINUE.
      ELSE.
        SELECT * INTO TABLE IT_LIPS_LNS
         FROM LIPS
         FOR ALL ENTRIES IN IT_VBUP
         WHERE VBELN = IT_VBUP-VBELN
          AND POSNR = IT_VBUP-POSNR.
      ENDIF.

      CALL FUNCTION 'Z_FMM_60XX_VL32N_NONWM'
        EXPORTING
*           CTU             = 'X'
*           MODE            = 'N'
*           UPDATE          = 'L'
*           GROUP           =
*           USER            =
*           KEEP            =
*           HOLDDATE        =
*           NODATA          = '/'
          VBELN_001       = L_VBELN
          BLDAT_002       = L_BLDAT
          LFDAT_LA_003        = L_BLDAT
          LFUHR_LA_004        = L_BLDAT
       IMPORTING
         SUBRC           = W_SUBRC
       TABLES
         MESSTAB         = IT_MESSTAB
         T_LIPS    = IT_LIPS_LNS
                .

      READ TABLE IT_MESSTAB WITH KEY MSGTYP = 'S'
                                     MSGID = 'VL'
                                     MSGNR = '311'.

      IF SY-SUBRC <> 0.   " if not sucessful


*        READ TABLE IT_MESSTAB WITH KEY MSGTYP = 'E'.
        READ TABLE IT_MESSTAB INDEX 1.
*        IF SY-SUBRC = 0.

        MOVE:   IT_MESSTAB-MSGNR TO WA_MSGNR,
                IT_MESSTAB-MSGID TO WA_MSGID,
                IT_MESSTAB-MSGV1 TO WA_MSGV1,
                IT_MESSTAB-MSGV2 TO WA_MSGV2,
                IT_MESSTAB-MSGV3 TO WA_MSGV3,
                IT_MESSTAB-MSGV4 TO WA_MSGV4.

        CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
             EXPORTING
                  LANGU = SY-LANGU
                  MSGID = WA_MSGID
                  MSGNO = WA_MSGNR
                  MSGV1 = WA_MSGV1
                  MSGV2 = WA_MSGV2
                  MSGV3 = WA_MSGV3
                  MSGV4 = WA_MSGV4
             IMPORTING
                  TEXT  = WA_TEXT.

        L_CHECK  = 'X'.  "ERROR CHECK
        WA_MESSA = WA_TEXT.
        T_MESSAGE-TYPE = 'E'.
        T_MESSAGE-ID = SY-MSGID.
        T_MESSAGE-NUMBERS = SY-MSGNO.
        T_MESSAGE-MESSAGE = WA_TEXT.
        APPEND T_MESSAGE.
        WA_TRUCK-EXECUTED_FLG = 'X'.
*        ELSE.
*          READ TABLE IT_MESSTAB WITH KEY MSGTYP = 'A'.
*          IF SY-SUBRC = 0.
*
*            MOVE:   IT_MESSTAB-MSGNR TO WA_MSGNR,
*                    IT_MESSTAB-MSGID TO WA_MSGID,
*                    IT_MESSTAB-MSGV1 TO WA_MSGV1,
*                    IT_MESSTAB-MSGV2 TO WA_MSGV2,
*                    IT_MESSTAB-MSGV3 TO WA_MSGV3,
*                    IT_MESSTAB-MSGV4 TO WA_MSGV4.
*
*            CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
*                 EXPORTING
*                      LANGU = SY-LANGU
*                      MSGID = WA_MSGID
*                      MSGNO = WA_MSGNR
*                      MSGV1 = WA_MSGV1
*                      MSGV2 = WA_MSGV2
*                      MSGV3 = WA_MSGV3
*                      MSGV4 = WA_MSGV4
*                 IMPORTING
*                      TEXT  = WA_TEXT.
*
*            L_CHECK  = 'X'.  "ERROR CHECK
*            WA_MESSA = WA_TEXT.
**          WA_EPOSITION = 'TO CREATION ERROR'.
*            T_MESSAGE-TYPE = 'E'.
*            T_MESSAGE-ID = SY-MSGID.
*            T_MESSAGE-NUMBERS = SY-MSGNO.
*            T_MESSAGE-MESSAGE = WA_TEXT.
*            APPEND T_MESSAGE.
*            WA_TRUCK-EXECUTED_FLG = 'X'.
*          ENDIF.
*        ENDIF.
      ELSE.
        WA_TRUCK-EXECUTED_FLG = 'S'.
      ENDIF.
      MODIFY T_TRUCK FROM WA_TRUCK INDEX L_TABIX
                                   TRANSPORTING EXECUTED_FLG.

      MOVE-CORRESPONDING WA_TRUCK TO LT_RECE.
      CASE WA_TRUCK-EXECUTED_FLG.
        WHEN 'S'.
          LT_RECE-EXECUTED_FLG = 'X'.
          APPEND LT_RECE. CLEAR LT_RECE.
        WHEN 'X'.
          CLEAR LT_RECE.
      ENDCASE.
      CLEAR WA_TRUCK.
    ENDLOOP.

    CASE I_TYPES.
      WHEN 'TRUCK' OR 'VENDOR'.                             "UD1K919323
        READ TABLE T_RECEIVER INDEX 1.
        IF SY-SUBRC EQ 0.
          L_LIFEX =  T_RECEIVER-LIFEX.
          L_LIFNR =  T_RECEIVER-LIFNR.
        ENDIF.
      WHEN 'UNIT'.
        L_LIFEX =  'UNIT'.
        L_LIFNR =  'UNIT'.
      WHEN OTHERS.
        L_LIFEX =  ''.
        L_LIFNR =  ''.
    ENDCASE.
*    clear l_cnt.
*    describe table it_gr lines l_cnt.                       "UD1K919575
*    if l_cnt > 0.                                           "UD1K919575
*      perform schedule_GRPOSTING_JOB tables it_gr.
*    endif.                                                  "UD1K919575

* Send email for deliveires in error.
    IF NOT IT_ERROR[] IS INITIAL.
      PERFORM SEND_EMAIL.
      WA_TEXT = 'Error in check procedure'.
      L_CHECK  = 'X'.
    ENDIF.

    CASE I_TYPES.
      WHEN 'TRUCK'.

      WHEN 'UNIT'.
        LOOP AT T_TRUCK.
          MOVE-CORRESPONDING T_TRUCK TO LT_TRUCK.
          APPEND LT_TRUCK. CLEAR LT_TRUCK.
        ENDLOOP.
        REFRESH T_TRUCK. CLEAR T_TRUCK.
        SORT LT_TRUCK BY RECEIVER
                         EXECUTED_FLG DESCENDING.
        DELETE ADJACENT DUPLICATES FROM LT_TRUCK COMPARING RECEIVER.
        LOOP AT LT_TRUCK.
          MOVE-CORRESPONDING LT_TRUCK TO T_TRUCK.
          APPEND T_TRUCK. CLEAR T_TRUCK.
        ENDLOOP.
      WHEN 'VENDOR'.

    ENDCASE.

*/ Log Message Processing
    CLEAR: WA_LIPS, W_QTY_MESSAGE.

    IF L_CHECK NE 'X'.
      E_MESS  = TEXT-M22.
      ZRESULT = TEXT-M04.
    ELSE.
      ZRESULT = TEXT-M02.
      E_MESS  = WA_TEXT.
    ENDIF.
  ELSE.
    IF L_CHECK EQ 'X'.
      ZRESULT = TEXT-M02.
      E_MESS  = TEXT-M21.

    ELSE.
      ZRESULT = TEXT-M02.
      E_MESS  = TEXT-M01.
    ENDIF.
  ENDIF.           "NOT T_TRUCK[] IS INITIAL.
  PERFORM MIDDLEWARE_UPLODA_RECEIVER TABLES  LT_RECE.

ENDFUNCTION.
