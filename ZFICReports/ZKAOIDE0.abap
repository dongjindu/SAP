*
*First execute the correction program ZKAOIDE0. This removes the data
*records without reference to a purchase order or  purchase requisition.
*On the selection screen of the program, enter the respective purchase
*requisitions in field P_REQ and the  purchase orders in field P_ORD.
*
*If you use ZKAOIDE0 to correct purchase requisitions, you system
*should* have applied the most current version  of report RKACOR04 (from
*Note 21649). If you want to correct incorrect data and not only search
*for them, delete flag  P_TEST.
*
*If you want to delete data records, which do not have a purchase
*requisition number or purchase order number,  enter "=" as the
*selection option on the corresponding input field and leave the field
*empty.
*


REPORT ZKAOIDE0 .

TABLES: COOI,
        EBAN,
        EKPO.

DATA LT_COOI LIKE COOI OCCURS 0 WITH HEADER LINE.

SELECT-OPTIONS: P_REQ FOR EBAN-BANFN,
                P_ORD FOR EKPO-EBELN.
SELECTION-SCREEN SKIP.
PARAMETERS: P_TEST DEFAULT 'X' AS CHECKBOX.

IF P_REQ[] IS INITIAL AND
   P_ORD[] IS INITIAL.
  WRITE: / 'Please enter data on selection-screen'.
  EXIT.
ENDIF.

IF NOT P_REQ[] IS INITIAL.
  SELECT * FROM COOI INTO TABLE LT_COOI
                     WHERE REFBT = '010'
                     AND   REFBN IN P_REQ
                     AND ( MEGBTR NE 0 OR
                           MBGBTR NE 0 OR
                           WHGBTR NE 0 OR
                           WKGBTR NE 0 OR
                           WTGBTR NE 0 OR
                           WOGBTR NE 0 ).
  LOOP AT LT_COOI.
    SELECT SINGLE * FROM EBAN WHERE BANFN = LT_COOI-REFBN
                              AND   BNFPO = LT_COOI-RFPOS.
    IF SY-SUBRC NE 0.
      IF P_TEST IS INITIAL.
        CLEAR: LT_COOI-WHGBTR,
               LT_COOI-WKGBTR,
               LT_COOI-WOGBTR,
               LT_COOI-WTGBTR,
               LT_COOI-WLGBTR,
               LT_COOI-MEGBTR,
               LT_COOI-MBGBTR.
        LT_COOI-LOEKZ = 'X'.
        UPDATE COOI FROM LT_COOI.
        SUBMIT RKACOR04 AND RETURN
               EXPORTING LIST TO MEMORY
                        WITH S_OBJNR EQ LT_COOI-OBJNR
                        WITH S_GJAHR EQ LT_COOI-GJAHR
                        WITH S_PERIO EQ LT_COOI-PERIO
                        WITH S_KSTAR EQ LT_COOI-SAKTO
                        WITH P_ACTUAL EQ SPACE
                        WITH P_OBLIGO EQ 'X'
                        WITH P_UPDATE EQ 'X'.
      ENDIF.
      WRITE: / LT_COOI-REFBN, '/', LT_COOI-RFPOS.
    ENDIF.
  ENDLOOP.
  IF SY-SUBRC = 0 AND P_TEST IS INITIAL.
    SKIP.
    WRITE: / 'If you have availability control in use,',
           / 'please reconstruct your data with',
           / 'program RBPFCON1 for orders and',
           / 'program RBPFCPN1 for projects.'.
    SKIP 2.
  ENDIF.
ENDIF.
COMMIT WORK.

IF NOT P_ORD[] IS INITIAL.
  SELECT * FROM COOI WHERE REFBT = '020'
                     AND   REFBN IN P_ORD
                     AND ( MEGBTR NE 0 OR
                           MBGBTR NE 0 OR
                           WHGBTR NE 0 OR
                           WKGBTR NE 0 OR
                           WTGBTR NE 0 OR
                           WOGBTR NE 0 ).
    SELECT SINGLE * FROM EKPO WHERE EBELN = COOI-REFBN
                                AND EBELP = COOI-RFPOS.
    IF SY-SUBRC NE 0.
      IF P_TEST IS INITIAL.
        CALL FUNCTION 'K_INIT_OPEN_ITEM'
             EXPORTING
                  REFBN = COOI-REFBN
                  RFPOS = COOI-RFPOS.
      ENDIF.
      WRITE: / COOI-REFBN, '/', COOI-RFPOS.
    ENDIF.
  ENDSELECT.
ENDIF.
COMMIT WORK.
