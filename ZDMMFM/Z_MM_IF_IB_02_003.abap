FUNCTION Z_MM_IF_IB_02_003.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0032 OPTIONAL
*"----------------------------------------------------------------------

  DATA : L_KZEAR TYPE KZEAR,
         L_BWART TYPE BWART,
         L_SAKNR TYPE SAKNR,
         L_KOSTL TYPE KOSTL,
         L_KSTRG TYPE KSTRG.

  DATA : L_CNT LIKE SY-DBCNT.
  DATA : L_RETURN LIKE ZMMS0053.
  DATA : L_MBLNR LIKE MSEG-MBLNR,
         L_MJAHR LIKE MSEG-MJAHR,
         L_PKKEY LIKE PKPS-PKKEY,
         L_PKBST LIKE PKPS-PKBST,
         LS_MENGE LIKE BAPI1075_ACTUAL_QTY,
         LS_RETURN LIKE BAPIRET2,
         LS_RETURN2 LIKE BAPIRET2,  "Victor 07.15.2011
         LS_032 LIKE ZMMT0032,
         LT_MSEG LIKE MSEG OCCURS 0 WITH HEADER LINE.

  DATA:  T_RES LIKE BAPI1075_3 OCCURS 0 WITH HEADER LINE.

  DATA: LT_TEMP LIKE TABLE OF ZMMT0032 WITH HEADER LINE.
  DATA: LT_ERROR LIKE TABLE OF ZMMT0032 WITH HEADER LINE.

  DATA: BEGIN OF LT_RSNUM OCCURS 0,
        RSNUM LIKE MSEG-RSNUM,
        MBLNR LIKE MSEG-MBLNR,
        END OF LT_RSNUM.

  DATA: L_INDEX LIKE SY-TABIX.

  DATA: L_KANBAN_ID(1) VALUE 'X'.

  CLEAR : GT_BODY_32,
          GT_BODY_32[].

  IF IT_BODY[] IS INITIAL.
    E_RETURN-TYPE    = 'E'.
    E_RETURN-MESSAGE = TEXT-M01.
    EXIT.
  ENDIF.

  GT_BODY_32[] = IT_BODY[].

  SORT GT_BODY_32 BY PKKEY.

** Fuorng on 09/20/11
  IF SY-TCODE <> 'ZMMR10000T'.
    SELECT * INTO TABLE LT_TEMP
     FROM ZMMT0032
    WHERE TYPE = 'M'
  %_HINTS ORACLE 'RULE'.    "Addition

    SELECT * INTO TABLE LT_ERROR
     FROM ZMMT0032
    WHERE TYPE = 'E'
  %_HINTS ORACLE 'RULE'.    "Addition

    IF SY-SUBRC = 0.
      SELECT RSNUM MBLNR INTO TABLE LT_RSNUM
      FROM MSEG
      FOR ALL ENTRIES IN LT_ERROR
      WHERE RSNUM = LT_ERROR-RSNUM
*       AND ( BWART = '301' OR BWART = '311' ).
              AND BWART = '311'.
      SORT LT_RSNUM BY RSNUM.
      LOOP AT LT_ERROR.
        L_INDEX = SY-TABIX.
        READ TABLE LT_RSNUM WITH KEY RSNUM = LT_ERROR-RSNUM
                       BINARY SEARCH.
        IF SY-SUBRC = 0.
          LT_ERROR-TYPE = 'S'.
          LT_ERROR-MBLNR = LT_RSNUM-MBLNR.
          MODIFY LT_ERROR INDEX L_INDEX.
        ELSE.
          DELETE LT_ERROR INDEX L_INDEX.
        ENDIF.
      ENDLOOP.
      IF LT_ERROR[] IS INITIAL.
      ELSE.
        APPEND LINES OF LT_ERROR TO LT_TEMP.
      ENDIF.
    ENDIF.
  ENDIF.
** End on 09/20/11

  LOOP AT GT_BODY_32.
    G_TABIX = SY-TABIX.

*    IF g_tabix = 2.
*      PERFORM BAPI_CLEAR.
*      CLEAR : L_KZEAR, L_BWART, L_SAKNR, Ls_MENGE.
*
*      SELECT SINGLE KZEAR BWART SAKNR
*               INTO (L_KZEAR, L_BWART, L_SAKNR)
*              FROM RESB
*              WHERE RSNUM = GT_BODY_32-RSNUM.
*
*
*      IF L_BWART NE '311'.
*        CLEAR : L_KOSTL, L_KSTRG.
*        SELECT SINGLE KOSTL KSTRG
*                 INTO (L_KOSTL, L_KSTRG)
*                FROM RKPF
*                WHERE RSNUM = GT_BODY_32-RSNUM.
*      ENDIF.
*
*      IF NOT L_KZEAR IS INITIAL.
*        GT_BODY_32-TYPE    = E_RETURN-TYPE    = 'W'.
*        GT_BODY_32-MESSAGE = E_RETURN-MESSAGE = TEXT-M02.
*        MODIFY GT_BODY_32 INDEX G_TABIX
*                          TRANSPORTING TYPE
*                                       MESSAGE.
*
*        CONTINUE.
*      ENDIF.
*      PERFORM BAPI_HEADER_32 USING L_BWART.
*      PERFORM BAPI_ITEM_32   USING L_BWART
*                                   L_KOSTL
*                                   L_KSTRG
*                                   L_SAKNR.
*      PERFORM BAPI_GR.
*      PERFORM BAPI_RETURN CHANGING L_RETURN.
*
*      GT_BODY_32-MBLNR   = MATERIALDOCUMENT.
*      GT_BODY_32-MJAHR   = MATDOCUMENTYEAR.
*      GT_BODY_32-TYPE    = L_RETURN-TYPE.
*      GT_BODY_32-MESSAGE = L_RETURN-MESSAGE.

*-- KANBAN
*    ELSE.

    SELECT SINGLE MBLNR MJAHR INTO (GT_BODY_32-MBLNR, GT_BODY_32-MJAHR)
             FROM MSEG
            WHERE RSNUM = GT_BODY_32-RSNUM.
    IF SY-SUBRC = 0.
      GT_BODY_32-TYPE    = 'S'.
      GT_BODY_32-MESSAGE = 'Already posted'.
      MODIFY GT_BODY_32 INDEX G_TABIX
             TRANSPORTING TYPE MESSAGE MBLNR MJAHR.
      CLEAR GT_BODY_32.
      CONTINUE.
    ENDIF.


    SELECT SINGLE PKKEY INTO L_PKKEY
            FROM PKPS
            WHERE RSNUM = GT_BODY_32-RSNUM.
**Paul Add Error Message not exist in PKPS table : 07/11/11
    IF SY-SUBRC <> 0.
**      CLEAR : L_MBLNR, L_MJAHR.
****Paul Add Error Check Mseg : 071111
**      SELECT SINGLE MBLNR MJAHR
**        INTO (L_MBLNR, L_MJAHR)
**        FROM MSEG
****Use INDEX 'R'
**       WHERE RSNUM = GT_BODY_32-RSNUM.
**
**      IF SY-SUBRC = 0.
**        GT_BODY_32-TYPE    = 'S'.
**        GT_BODY_32-MESSAGE = 'Stock transfer complete'.
**        GT_BODY_32-MBLNR   = L_MBLNR.
**        GT_BODY_32-MJAHR   = L_MJAHR.
**      ELSE.

      CLEAR : L_KZEAR, L_BWART, L_SAKNR, LS_MENGE.

      SELECT SINGLE KZEAR BWART SAKNR
               INTO (L_KZEAR, L_BWART, L_SAKNR)
              FROM RESB
              WHERE RSNUM = GT_BODY_32-RSNUM.

      IF SY-SUBRC EQ 0 AND L_BWART EQ '311'.

        IF NOT L_KZEAR IS INITIAL.
          GT_BODY_32-TYPE    = 'S'.
          GT_BODY_32-MESSAGE = 'Already posted'.
          MODIFY GT_BODY_32 INDEX G_TABIX
                 TRANSPORTING TYPE MESSAGE.
          CONTINUE.
        ENDIF.

        CLEAR : L_KOSTL, L_KSTRG.
        SELECT SINGLE KOSTL KSTRG
                 INTO (L_KOSTL, L_KSTRG)
                FROM RKPF
                WHERE RSNUM = GT_BODY_32-RSNUM.

        PERFORM BAPI_CLEAR.
        PERFORM BAPI_HEADER_32 USING L_BWART.
        PERFORM BAPI_ITEM_32   USING L_BWART
                                     L_KOSTL
                                     L_KSTRG
                                     L_SAKNR.
        PERFORM BAPI_GR.
        PERFORM BAPI_RETURN CHANGING LS_RETURN.

        GT_BODY_32-MBLNR   = MATERIALDOCUMENT.
        GT_BODY_32-MJAHR   = MATDOCUMENTYEAR.

        IF LS_RETURN(1) = 'S'.

*          SELECT SINGLE PKKEY INTO L_PKKEY
*                  FROM PKPS
*                  WHERE RSNUM = GT_BODY_32-RSNUM.
*
*          IF SY-SUBRC NE 0.
*            GT_BODY_32-TYPE    = 'E'.
*            GT_BODY_32-MESSAGE = 'Control Cycle Is Not Exist(2)'.
          GT_BODY_32-TYPE = LS_RETURN-TYPE.
          GT_BODY_32-MESSAGE = 'Posted w/o Kanban'.
*          GT_BODY_32-MESSAGE = LS_RETURN-MESSAGE.

          MODIFY GT_BODY_32 INDEX G_TABIX
                           TRANSPORTING TYPE MESSAGE MBLNR MJAHR.
          CONTINUE.
*          ENDIF.
        ELSE.

*--<      Victor 07.19.2011 Result of Reprocessing with BDC
          READ TABLE GOODSMVT_RETURN WITH KEY TYPE   = 'E'
                                              ID     = 'M7'
                                              NUMBER = '509'.
          IF SY-SUBRC = 0.

            PERFORM READ_MESSAGE1 USING  LS_RETURN2-TYPE
                                         LS_RETURN2-MESSAGE_V1
                                         LS_RETURN2-MESSAGE.
            IF LS_RETURN2-TYPE = 'S'.
              GT_BODY_32-MBLNR   = LS_RETURN2-MESSAGE_V1.
              GT_BODY_32-MJAHR   = GT_BODY_32-BUDAT+0(4).
              GT_BODY_32-TYPE    = LS_RETURN2-TYPE.
              GT_BODY_32-MESSAGE = 'Posted w/o Kanban'.
              MODIFY GT_BODY_32 INDEX G_TABIX
                               TRANSPORTING TYPE MESSAGE MBLNR MJAHR.
              CONTINUE.
            ELSE.

              GT_BODY_32-TYPE    = LS_RETURN2-TYPE.
              GT_BODY_32-MESSAGE = LS_RETURN2-MESSAGE.
              MODIFY GT_BODY_32 INDEX G_TABIX
                               TRANSPORTING TYPE MESSAGE.
              CONTINUE.
            ENDIF.

            CLEAR : LS_RETURN2.
          ELSE.
*-->
            GT_BODY_32-TYPE    = 'E'.
            GT_BODY_32-MESSAGE = LS_RETURN+1.
            MODIFY GT_BODY_32 INDEX G_TABIX
                             TRANSPORTING TYPE MESSAGE.
            CONTINUE.
          ENDIF.
        ENDIF.
      ELSE.

        GT_BODY_32-TYPE    = 'E'.
        GT_BODY_32-MESSAGE = 'Control Cycle Is Not Exist'.

        MODIFY GT_BODY_32 INDEX G_TABIX
                         TRANSPORTING TYPE
                                      MESSAGE.
**E<
        CONTINUE.  "Fixme error handling

      ENDIF.

    ENDIF.

    IF NOT L_PKKEY IS INITIAL.

      L_PKBST = '5'.

*      LV_MENGE = GT_BODY_32-MENGE.
      LS_MENGE-ACTUAL_QTY = GT_BODY_32-MENGE.
      LS_MENGE-BASE_UOM = GT_BODY_32-MEINS.
*

** Furong on 10/05/11 for passing posting date to GR posting
      SET PARAMETER ID 'KANBAN_DATE_ID' FIELD L_KANBAN_ID.
      SET PARAMETER ID 'KANBAN_POST_DATE' FIELD GT_BODY_32-BUDAT.
** End on 10/05/11

      CALL FUNCTION 'BAPI_KANBAN_CHANGESTATUS'
           EXPORTING
                KANBANIDNUMBER     = L_PKKEY
                ACTUALQUANTITY     = LS_MENGE
                NEXTSTATUS         = L_PKBST
           IMPORTING
                RETURN             = LS_RETURN
           TABLES
                STATUSCHANGERESULT = T_RES.
**C__paul  T_RES -> LS_RETURN
      IF LS_RETURN IS INITIAL.
**E__<06/08/11
**Paul Check T_RES Table's Type field : 07/11/11
        READ TABLE T_RES WITH KEY TYPE = 'E'.

        IF SY-SUBRC = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          GT_BODY_32-TYPE    = 'E'.
          GT_BODY_32-MESSAGE = T_RES-MESSAGE.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
               EXPORTING
                    WAIT = 'X'.

          READ TABLE T_RES WITH KEY TYPE = 'S'.
**Paul Change 'KZEAR' Upload : 071411
          CLEAR : LT_MSEG, LT_MSEG[].

*          SELECT *
*            FROM mseg
*            INTO CORRESPONDING FIELDS OF TABLE lt_mseg
*           WHERE mblnr = t_res-mat_doc
*             AND mjahr = sy-datum(4).
*
*          READ TABLE lt_mseg WITH KEY kzear = 'X'.
*
*          IF sy-subrc NE 0.

*            lt_mseg-kzear = 'X'.
*            MODIFY lt_mseg TRANSPORTING kzear
*                           WHERE mblnr NE ''.

*            CALL FUNCTION 'MB_CHANGE_DOCUMENT'
*                 TABLES
*                      zmseg = lt_mseg.
*--< Victor 07.15.2011
          PERFORM RESERVATION_DELETE USING GT_BODY_32-RSNUM
                                  LS_RETURN2-TYPE LS_RETURN2-MESSAGE.
*--     Error log should be reprocessed and saved in CBO table
*-->

*          ENDIF.
**E<
          GT_BODY_32-MBLNR   = T_RES-MAT_DOC.
          GT_BODY_32-MJAHR   = SY-DATUM(4).
          GT_BODY_32-TYPE    = 'S'.
          GT_BODY_32-MESSAGE = 'Stock transfer complete'.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        GT_BODY_32-TYPE    = 'E'.
**Paul Add Error Message from running after BAPI : 07/11/11
*      GT_BODY_32-MESSAGE = 'BAPI_KANBAN_CHANGESTATUS call error'.
        GT_BODY_32-MESSAGE = LS_RETURN-MESSAGE.
      ENDIF.
**E
*    ENDIF.
**    IF GT_BODY_32-TYPE = 'E'.
**      SELECT SINGLE MBLNR MJAHR
**        INTO (GT_BODY_32-MBLNR, GT_BODY_32-MJAHR)
**        FROM MSEG
****Use INDEX 'R'
**       WHERE RSNUM = GT_BODY_32-RSNUM.
**
**      IF SY-SUBRC <> 0.
**        IF GT_BODY_32-MESSAGE IS INITIAL.
**          GT_BODY_32-MESSAGE = 'BAPI_KANBAN_CHANGESTATUS call error'.
**        ENDIF.
**      ENDIF.
**    ENDIF.
    ENDIF.

    MODIFY GT_BODY_32 INDEX G_TABIX
                      TRANSPORTING TYPE
                                   MESSAGE
                                   MBLNR
                                   MJAHR.
    CLEAR GT_BODY_32.
  ENDLOOP.

** Changed on 09/01/2011 by Furong - set to 'M' if manually process
  IF SY-TCODE = 'ZMMR10000T'.  " sy-uname+0(1) = '1'.
    GT_BODY_32-TYPE  = 'M'.
    MODIFY GT_BODY_32  TRANSPORTING TYPE WHERE TYPE = 'S'.
  ENDIF.
** End on 09/01/2011

**** LOGIC
  LOOP AT GT_BODY_32.
    G_TABIX = SY-TABIX.

    G_RETURN =  E_RETURN-TYPE  = GT_BODY_32-TYPE.
    E_RETURN-MESSAGE = GT_BODY_32-MESSAGE.

    CALL METHOD ZMMC_CL_IF=>IF_SET_KEY(   IFKEY = 'MMIF203_ECC_IB'
                              MODLE = 'GCS'      "
                              CENTY = 'US'       "
                              DIRCT = 'I'        "
                              LOGOX = ' '
                              TTYPE = 'S'
                              CPARM = '9'
                           ).

    CALL METHOD ZMMC_CL_IF=>IF_SET_MESSG( TYPE    = E_RETURN-TYPE
                              ID      = ' '    "gt_retmsg-id
                              MESSAGE = E_RETURN-MESSAGE
                            ).

    CALL METHOD ZMMC_CL_IF=>IF_SET_PARAM( ISTAT = G_RETURN
                              IFP01 = GT_BODY_32-RSNUM
                              IFP02 = GT_BODY_32-RSPOS
                              IFP03 = GT_BODY_32-BUDAT
                              IFP04 = GT_BODY_32-LGORT
                              IFP05 = GT_BODY_32-UMLGO
                              IFP06 = GT_BODY_32-MATNR
                              IFP07 = GT_BODY_32-MENGE
                              IFP08 = GT_BODY_32-PKKEY
                              IFP09 = GT_BODY_32-SAEDT
                            ).
    CALL METHOD ZMMC_CL_IF=>IF_SAVE_DATA( ).

    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF LS_032
      FROM ZMMT0032
     WHERE PKKEY = GT_BODY_32-PKKEY
       AND RSNUM = GT_BODY_32-RSNUM
       AND RSPOS = GT_BODY_32-RSPOS
       AND SAEDT = GT_BODY_32-SAEDT
       AND SAEUZ = GT_BODY_32-SAEUZ.

    IF SY-SUBRC = 0.
      IF LS_032-ETNAM IS INITIAL.
        GT_BODY_32-ETDAT = SY-DATUM.
        GT_BODY_32-ETTIM = SY-UZEIT.
        GT_BODY_32-ETNAM = SY-UNAME.
      ELSE.
        GT_BODY_32-ATDAT = SY-DATUM.
        GT_BODY_32-ATTIM = SY-UZEIT.
        GT_BODY_32-ATNAM = SY-UNAME.
        GT_BODY_32-ETDAT = LS_032-ETDAT.
        GT_BODY_32-ETTIM = LS_032-ETTIM.
        GT_BODY_32-ETNAM = LS_032-ETNAM.
      ENDIF.
    ELSE.
      GT_BODY_32-ETDAT = SY-DATUM.
      GT_BODY_32-ETTIM = SY-UZEIT.
      GT_BODY_32-ETNAM = SY-UNAME.
    ENDIF.

    MODIFY GT_BODY_32 INDEX G_TABIX.
  ENDLOOP.

** Changed by Furong on 09/01/2011
  IF LT_TEMP[] IS INITIAL.
    MODIFY ZMMT0032 FROM TABLE GT_BODY_32.
  ELSE.
    LT_TEMP-TYPE  = 'S'.
    MODIFY LT_TEMP TRANSPORTING TYPE WHERE TYPE = 'M'.
    APPEND LINES OF LT_TEMP TO GT_BODY_32.
    MODIFY ZMMT0032 FROM TABLE GT_BODY_32.
  ENDIF.
** End on 09/01/2011

  CLEAR : IT_BODY, IT_BODY[].

  IT_BODY[] = GT_BODY_32[].

ENDFUNCTION.
