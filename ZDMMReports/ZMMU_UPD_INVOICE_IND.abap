REPORT ZMMU_UPD_INVOICE_IND .



************************************************************************
* Program Name      : ZMMU_UPD_INVOICE_IND
* Author            : Vijayakumar Salapadi
* Creation Date     : 10/16/2008
* Development Request No : UD1K944761
* Description       : Update Findal Invoice Indicator for Pos.
*
* Modification Logs
* Date            Developer        RequestNo      Description
************************************************************************

TABLES: EKKO, EKPO, EKBE.

DATA: BEGIN OF IT_EKPO OCCURS 0,
      EBELN LIKE EKPO-EBELN,
      EBELP LIKE EKPO-EBELP,
      ELIKZ LIKE EKPO-ELIKZ,
      EREKZ LIKE EKPO-EREKZ,
      MENGE LIKE EKPO-MENGE,
      END OF IT_EKPO.

DATA: BEGIN OF IT_EKBE OCCURS 0,
      EBELN LIKE EKBE-EBELN,
      EBELP LIKE EKBE-EBELP,
      BEWTP LIKE EKBE-BEWTP,
      MENGE LIKE EKBE-MENGE,
      BELNR LIKE EKBE-BELNR,
      END OF IT_EKBE.

DATA: BEGIN OF IT_OUTPUT OCCURS 0,
       EBELN LIKE EKPO-EBELN,
       EBELP LIKE EKPO-EBELP,
       MSGTXT(50),
       MSGTYP,
       END OF IT_OUTPUT.

DATA: L_FLAG, L_INVQTY TYPE EKBE-MENGE , L_DEVQTY TYPE EKBE-MENGE.


DATA : IT_POHEAD LIKE BAPIMEPOHEADER,
       IT_POHEADX LIKE BAPIMEPOHEADERX,
       IT_POITEM LIKE BAPIMEPOITEM OCCURS 0 WITH HEADER LINE,
       IT_POITEMX LIKE BAPIMEPOITEMX OCCURS 0 WITH HEADER LINE,
       IT_RETURN  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

DATA: V_PONUM LIKE BAPIMEPOHEADER-PO_NUMBER,
      V_ITEM LIKE BAPIMEPOITEM-PO_ITEM,
      V_FINAL LIKE BAPIMEPOITEM-FINAL_INV VALUE 'X',
      V_FINALX LIKE BAPIMEPOITEMX-FINAL_INV VALUE 'X'.

SELECTION-SCREEN BEGIN OF BLOCK BLK
                          WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS R1 RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN COMMENT 5(15) TEXT-002 FOR FIELD S_EBELN.
SELECT-OPTIONS: S_EBELN FOR EKKO-EBELN.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS R2 RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN COMMENT 5(18) TEXT-003 FOR FIELD P_DATE.
PARAMETERS P_DATE TYPE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END OF BLOCK BLK.


START-OF-SELECTION.

  PERFORM GET_DATA.

  PERFORM PREPARE_DATA.

  PERFORM WRITE_OUTPT.



*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.

  DATA:  V_YEAR TYPE MKPF-MJAHR.

  DATA: L_BSART LIKE EKKO-BSART.

*  DATA : BEGIN OF it_mkpf OCCURS 0,
*         mblnr TYPE mkpf-mblnr,
*         END OF it_mkpf.

  DATA: BEGIN OF IT_EKBE1 OCCURS 0,
        EBELN TYPE EKBE-EBELN,
        EBELP TYPE EKBE-EBELP,
        BELNR TYPE EKBE-BELNR,
        END OF IT_EKBE1.

  DATA : BEGIN OF IT_RBKP OCCURS 0,
         BELNR TYPE RBKP-BELNR,
         END OF IT_RBKP.

  IF R1 = 'X'.

    SELECT  EBELN EBELP ELIKZ EREKZ MENGE
                       INTO CORRESPONDING FIELDS OF TABLE IT_EKPO
                       FROM EKPO WHERE EBELN IN S_EBELN.
    IF IT_EKPO[] IS INITIAL.
      WRITE : 'There are No Purchase Orders' .
      EXIT.
    ENDIF.

  ELSEIF R2 = 'X'.

    CALL FUNCTION 'GET_CURRENT_YEAR'
     EXPORTING
        BUKRS         = 'H201'
        DATE          = P_DATE
     IMPORTING
*      CURRM         =
        CURRY         = V_YEAR
*      PREVM         =
*      PREVY         =
              .

*    SELECT mblnr FROM mkpf INTO TABLE it_mkpf WHERE mjahr = v_year
*                                          AND blart = 'WE'
*                                          AND budat = p_date.
*
*    IF it_mkpf[] IS INITIAL.
*      EXIT.
*    ENDIF.

    SELECT BELNR FROM RBKP INTO TABLE IT_RBKP WHERE GJAHR = V_YEAR
                                  AND ( BLART = 'RE' OR BLART = 'RF' )
                                  AND BUDAT = P_DATE.
    IF IT_RBKP[] IS INITIAL.
      WRITE : 'There are No Invoice created today' .
      EXIT.
    ENDIF.

    SELECT EBELN EBELP FROM EKBE
                          INTO CORRESPONDING FIELDS OF TABLE IT_EKBE1
                          FOR ALL ENTRIES IN IT_RBKP
                          WHERE GJAHR = V_YEAR AND
                                BELNR = IT_RBKP-BELNR.

    SELECT  EBELN EBELP ELIKZ EREKZ MENGE FROM EKPO
                       INTO CORRESPONDING FIELDS OF TABLE IT_EKPO
                       FOR ALL ENTRIES IN IT_EKBE1
                          WHERE EBELN = IT_EKBE1-EBELN AND
                                EBELP = IT_EKBE1-EBELP.

  ENDIF.

  SORT IT_EKPO BY EBELN EBELP.

** Changed by Furong on 02/18/09
  LOOP AT IT_EKPO.
    SELECT SINGLE BSART INTO L_BSART
      FROM EKKO
      WHERE EBELN = IT_EKPO-EBELN.
    IF L_BSART = 'KD'.
      DELETE IT_EKPO.
    ENDIF.
    CLEAR: L_BSART.
  ENDLOOP.
** End of change

  LOOP AT IT_EKPO.
    IF IT_EKPO-ELIKZ = 'X'.
      IF IT_EKPO-EREKZ = 'X'.
        IT_OUTPUT-EBELN  = IT_EKPO-EBELN.
        IT_OUTPUT-EBELP  = IT_EKPO-EBELP.
        IT_OUTPUT-MSGTXT = 'Final Invoice already set'.
        IT_OUTPUT-MSGTYP = 'E'.
        APPEND IT_OUTPUT.
        CLEAR IT_OUTPUT.
        DELETE IT_EKPO.
      ELSE.
        SELECT SINGLE * FROM EKBE
                                WHERE EBELN = IT_EKPO-EBELN
                                  AND EBELP = IT_EKPO-EBELP
                                  AND BEWTP = 'Q'.

        IF SY-SUBRC = 0.
          IT_EKBE-EBELN = EKBE-EBELN.
          IT_EKBE-EBELP = EKBE-EBELP.
          IT_EKBE-BEWTP = EKBE-BEWTP.
          IT_EKBE-MENGE = EKBE-MENGE.
          IT_EKBE-BELNR = EKBE-BELNR.
          APPEND IT_EKBE.
          CLEAR IT_EKBE.
        ELSE.
          IT_OUTPUT-EBELN  = IT_EKPO-EBELN.
          IT_OUTPUT-EBELP  = IT_EKPO-EBELP.
          IT_OUTPUT-MSGTXT = 'Invoice not created'.
          IT_OUTPUT-MSGTYP = 'E'.
          APPEND IT_OUTPUT.
          CLEAR IT_OUTPUT.
          DELETE IT_EKPO.
        ENDIF.

      ENDIF.

    ELSE.

      IT_OUTPUT-EBELN  = IT_EKPO-EBELN.
      IT_OUTPUT-EBELP  = IT_EKPO-EBELP.
      IT_OUTPUT-MSGTXT = 'Delivery complete Flag not set'.
      IT_OUTPUT-MSGTYP = 'E'.
      APPEND IT_OUTPUT.
      CLEAR IT_OUTPUT.
      DELETE IT_EKPO.
    ENDIF.
  ENDLOOP.

  IF IT_EKPO[] IS INITIAL.
    EXIT.
  ENDIF.

*  SELECT ebeln ebelp bewtp menge belnr
*                          INTO CORRESPONDING FIELDS OF TABLE it_ekbe
*                          FROM ekbe
*                          FOR ALL ENTRIES IN it_ekpo
*                          WHERE ebeln = it_ekpo-ebeln
*                            AND ebelp = it_ekpo-ebelp
*                            AND bewtp = 'Q'.


ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  prepare_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARE_DATA.

  IF IT_EKPO[] IS INITIAL.
    EXIT.
  ENDIF.

  SORT IT_EKBE BY EBELN EBELP.

  LOOP AT IT_EKBE.

    AT NEW EBELN.
*    WRITE : / 'Main item'.
      V_PONUM = IT_EKBE-EBELN.

    ENDAT.
    AT NEW EBELP.
      CLEAR: L_INVQTY , L_DEVQTY.
*        WRITE : / ' new line'.
    ENDAT.

*    IF it_ekbe-bewtp = 'E'.
*      l_devqty = l_devqty + it_ekbe-menge.
*    ENDIF.

    IF IT_EKBE-BEWTP = 'Q'.
      L_INVQTY = L_INVQTY + IT_EKBE-MENGE.
    ENDIF.
*       WRITE: / 'between lines'.

    AT END OF EBELP.
      READ TABLE IT_EKPO WITH KEY EBELN = IT_EKBE-EBELN
                                  EBELP = IT_EKBE-EBELP.
      IF L_INVQTY = IT_EKPO-MENGE.
        IT_OUTPUT-EBELN  = IT_EKBE-EBELN.
        IT_OUTPUT-EBELP  = IT_EKBE-EBELP.
        IT_OUTPUT-MSGTXT = 'Final invoice set'.
        IT_OUTPUT-MSGTYP = 'S'.
        APPEND IT_OUTPUT.
        CLEAR IT_OUTPUT.

        CLEAR V_ITEM.
        V_ITEM = IT_EKBE-EBELP.

*       Appending data into item level.
        IT_POITEM-PO_ITEM   = V_ITEM.
        IT_POITEM-FINAL_INV = V_FINAL.
        APPEND IT_POITEM.
        CLEAR IT_POITEM.

*       Appending data into X structure
        IT_POITEMX-PO_ITEM   = V_ITEM.
        IT_POITEMX-FINAL_INV = V_FINALX.
        APPEND IT_POITEMX.
        CLEAR IT_POITEMX.

      ELSE.

        IT_OUTPUT-EBELN  = IT_EKBE-EBELN.
        IT_OUTPUT-EBELP  = IT_EKBE-EBELP.
        IT_OUTPUT-MSGTXT = 'Not Ready for Final invoice'.
        IT_OUTPUT-MSGTYP = 'E'.
        APPEND IT_OUTPUT.
        CLEAR IT_OUTPUT.
      ENDIF.
*       WRITE : /' end of line'.
    ENDAT.

    AT END OF EBELN.

      IF NOT IT_POITEM[] IS INITIAL.

        CALL FUNCTION 'BAPI_PO_CHANGE'
           EXPORTING
              PURCHASEORDER                =  V_PONUM
*    POHEADER                     =  it_pohead
*    POHEADERX                    =  it_pohead
*   POADDRVENDOR                 =
*   TESTRUN                      =
*   MEMORY_UNCOMPLETE            =
*   MEMORY_COMPLETE              =
*   NO_MESSAGING                 =
*   NO_MESSAGE_REQ               =
*   NO_AUTHORITY                 =
*   NO_PRICE_FROM_PO             =
* IMPORTING
*   EXPHEADER                    =
           TABLES
             RETURN                       = IT_RETURN
             POITEM                       = IT_POITEM
             POITEMX                      = IT_POITEMX
*   POADDRDELIVERY               =
*   POSCHEDULE                   =
*   POSCHEDULEX                  =
*   POACCOUNT                    =
*   POACCOUNTPROFITSEGMENT       =
*   POACCOUNTX                   =
*   POCONDHEADER                 =
*   POCONDHEADERX                =
*   POCOND                       =
*   POCONDX                      =
*   POLIMITS                     =
*   POCONTRACTLIMITS             =
*   POSERVICES                   =
*   POSRVACCESSVALUES            =
*   POSERVICESTEXT               =
*   EXTENSIONIN                  =
*   EXTENSIONOUT                 =
*   POTEXTHEADER                 =
*   POTEXTITEM                   =
*   POPARTNER                    =
             .


        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*  EXPORTING
*    WAIT          =
*  IMPORTING
*    RETURN        =
                  .

        READ TABLE IT_RETURN WITH KEY  TYPE = 'E'.
        IF SY-SUBRC = 0.
          IT_OUTPUT-EBELN = V_PONUM.
          IT_OUTPUT-MSGTXT = IT_RETURN-MESSAGE.
          APPEND IT_OUTPUT.
          CLEAR IT_OUTPUT.
        ENDIF.

      ENDIF.

      CLEAR: V_PONUM, IT_POITEM, IT_POITEM[], IT_POITEMX, IT_POITEMX[],
             IT_RETURN, IT_RETURN[].

    ENDAT.

  ENDLOOP.

ENDFORM.                    " prepare_data
*&---------------------------------------------------------------------*
*&      Form  write_outpt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_OUTPT.


  SORT IT_OUTPUT.

  LOOP AT IT_OUTPUT.



    ULINE AT /(75).

    IF IT_OUTPUT-MSGTYP = 'E'.
      NEW-LINE.

      WRITE :  '|', IT_OUTPUT-EBELN, '|', IT_OUTPUT-EBELP, '|',
               IT_OUTPUT-MSGTXT COLOR 6, '|'.

    ELSE.

      NEW-LINE.
      WRITE : / '|', IT_OUTPUT-EBELN, '|', IT_OUTPUT-EBELP, '|',
               IT_OUTPUT-MSGTXT COLOR 5, '|'.
    ENDIF.

  ENDLOOP.



  ULINE AT /(75).

ENDFORM.                    " write_outpt
