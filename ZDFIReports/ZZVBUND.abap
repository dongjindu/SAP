REPORT ZZVBUND.
TABLES: BSEG, BKPF, T003, KNA1, LFA1, BSIS, BSAS, BSIK, BSAK, BSID,BSAD.
TABLES: VF_DEBI, VF_KRED.


TYPES: BKPF_TYPE LIKE BKPF,
       BSEG_TYPE LIKE BSEG,
       BSIS_TYPE LIKE BSIS,
       BSAS_TYPE LIKE BSAS,
       BSIK_TYPE LIKE BSIK,
       BSAK_TYPE LIKE BSAK,
       BSID_TYPE LIKE BSID,
       BSAD_TYPE LIKE BSAD.

DATA: XBKPF TYPE BKPF_TYPE OCCURS 0 WITH HEADER LINE,
      EBKPF TYPE BKPF_TYPE OCCURS 0 WITH HEADER LINE,
      XBSEG TYPE BSEG_TYPE OCCURS 0 WITH HEADER LINE,
      YBSEG TYPE BSEG_TYPE OCCURS 0 WITH HEADER LINE,
      XBSIS TYPE BSIS_TYPE OCCURS 0 WITH HEADER LINE,
      XBSAS TYPE BSAS_TYPE OCCURS 0 WITH HEADER LINE,
      XBSIK TYPE BSIK_TYPE OCCURS 0 WITH HEADER LINE,
      XBSAK TYPE BSAK_TYPE OCCURS 0 WITH HEADER LINE,
      XBSID TYPE BSID_TYPE OCCURS 0 WITH HEADER LINE,
      XBSAD TYPE BSAD_TYPE OCCURS 0 WITH HEADER LINE,
      XVBUND LIKE LFA1-VBUND,
      V_ERROR type c,
      V_FLAG  type c.

SELECT-OPTIONS:
          BUKRS     FOR  BKPF-BUKRS MEMORY ID BUK,
          GJAHR     FOR  BKPF-GJAHR,
          BELNR     FOR  BKPF-BELNR,
          CPUDT     FOR  BKPF-CPUDT.

PARAMETERS: UPDATE DEFAULT SPACE AS CHECKBOX,
            AWTYP LIKE TTYP-AWTYP DEFAULT 'MKPF' .

* select the header data into XBKPF.
* selects data only if BVORG is SPACE.

SELECT * FROM BKPF INTO TABLE XBKPF
              WHERE AWTYP EQ AWTYP AND
              BUKRS IN BUKRS AND
              CPUDT IN CPUDT AND
              GJAHR IN GJAHR AND
              BELNR IN BELNR AND
              BVORG EQ SPACE.          "Number of Cross-Company Code
                                       "posting Transaction


Loop at XBKPF.

* select the line items into YBSEG.

  REFRESH YBSEG.
  CLEAR V_FLAG.

  SELECT *  FROM BSEG INTO TABLE YBSEG
                      WHERE BUKRS EQ XBKPF-BUKRS AND
                      GJAHR EQ XBKPF-GJAHR AND
                      BELNR EQ XBKPF-BELNR.

* Find the trading partner of the Vendor or Customer

  LOOP AT YBSEG .
    CLEAR XVBUND.

    IF YBSEG-LIFNR  <> SPACE  and YBSEG-VBUND EQ SPACE
                              AND YBSEG-KOART EQ 'S'.
      CALL FUNCTION 'FI_VENDOR_DATA'
           EXPORTING
                I_BUKRS        = YBSEG-BUKRS
                I_LIFNR        = YBSEG-LIFNR
           IMPORTING
                E_KRED         = VF_KRED
           EXCEPTIONS
                VENDOR_MISSING = 1
                OTHERS         = 2.

      IF SY-SUBRC EQ 0.
        XVBUND = VF_KRED-VBUND.
      ENDIF.
    ELSE.

      IF YBSEG-KUNNR  <> SPACE  and YBSEG-VBUND EQ SPACE
                                AND YBSEG-KOART EQ 'S'.

        CALL FUNCTION 'FI_CUSTOMER_DATA'
             EXPORTING
                  I_BUKRS          = YBSEG-BUKRS
                  I_KUNNR          = YBSEG-KUNNR
             IMPORTING
                  E_DEBI           = VF_DEBI
             EXCEPTIONS
                  CUSTOMER_MISSING = 1
                  OTHERS           = 2.

        IF SY-SUBRC EQ 0.
          XVBUND =  VF_DEBI-VBUND.
        ENDIF.
      ENDIF.
    ENDIF.
* checks if XVBUND is filled
    CHECK XVBUND NE SPACE.
    V_FLAG = 'X'.
    YBSEG-VBUND = XVBUND.
    MODIFY YBSEG.

  ENDLOOP.

  IF V_FLAG = 'X'.

* checks if all rows have same vbund

    perform Check_Unique_VBUND.
  ELSE.
    delete xbkpf.
  ENDIF.
ENDLOOP.

* print the list of selected documents as well as error documents.

perform print_VBUND_log.

* update vbund in various FI tables if update flag is selected.

perform Update_Vbund.



*---------------------------------------------------------------------*
*       FORM Check_unique_vbund                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
Form Check_unique_vbund.

  CLEAR XVBUND.
  CLEAR V_ERROR.

* check if T003-XGSUB is space.

  SELECT SINGLE * FROM T003 WHERE BLART EQ XBKPF-BLART.
  IF SY-SUBRC EQ 0 AND T003-XGSUB EQ SPACE.

* read the first ybseg where vbund  is filled.

    LOOP at ybseg where vbund ne space.
      exit.
    ENDLOOP.

* move xbseg-vbund to v_vbund.

    if sy-subrc = 0.
      XVBUND = ybseg-vbund.
    endif.

* check if all rows in xbseg where vbund NE space have the same vbund.
* if other rows have different vbund then set error flag (V_error)

    LOOP at ybseg where vbund ne space.
      if ybseg-vbund ne XVBUND .
        v_error = 'X'.
        exit.
      endif.
    ENDLOOP.

* If no error , then fill all the rows with v_vbund where vbund is space
    IF v_error ne 'X'.
      LOOP at ybseg .
        xbseg = ybseg.
        if ybseg-vbund eq space.
          xbseg-vbund = XVBUND.
        endif.
        append xbseg.
      ENDLOOP.

    ELSE.

* for error log..
      ebkpf = xbkpf.
      append ebkpf.
      delete xbkpf.

    ENDIF.
  ELSE.

* if T003-XGSUB has been set for the document type .....

    LOOP at ybseg where vbund ne space.
      xbseg = ybseg.
      append xbseg.
    ENDLOOP.
  ENDIF.
endform.


*---------------------------------------------------------------------*
*       FORM print_vbund_log                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form print_vbund_log.

  DESCRIBE TABLE xbkpf LINES SY-TFILL.
  if sy-tfill > 0.
    write:/'Documents for which Trading partner can be updated' .
    write:/'BUKRS, GJHAR, Document Number'.
    write:/'-------------------------------'.
  endif.
  LOOP AT XBKPF.
    WRITE: / XBKPF-BUKRS, XBKPF-GJAHR, XBKPF-BELNR .
  ENDLOOP.
  write:/'-------------------------------'.
  skip 2.
  DESCRIBE TABLE ebkpf LINES SY-TFILL.
  if sy-tfill > 0.

    write:/' The following Documents CANNOT be processed as they have'.
    write: ' errors.The Trading partner is not unique for the document'.
    write: ' and (T003-XGSUB) is not set for the Document Type'.
  endif.
  LOOP AT EBkpf.
    write: / 'Error',eBKPF-BELNR .
  ENDLOOP.
endform.


*---------------------------------------------------------------------*
*       FORM update_vbund                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form update_vbund.

  DESCRIBE TABLE XBSEG LINES SY-TFILL.
  CHECK UPDATE NE SPACE AND SY-TFILL GT 0.

  LOOP AT XBSEG.
    IF XBSEG-AUGBL IS INITIAL.           " open items
      CASE XBSEG-KOART.
        when 'S'.
          SELECT SINGLE * FROM BSIS WHERE
                               BUKRS EQ XBSEG-BUKRS
                               AND HKONT EQ XBSEG-HKONT
                               AND AUGDT EQ XBSEG-AUGDT
                               AND AUGBL EQ XBSEG-AUGBL
                               AND ZUONR EQ XBSEG-ZUONR
                               AND GJAHR EQ XBSEG-GJAHR
                               AND BELNR EQ XBSEG-BELNR
                               AND BUZEI EQ XBSEG-BUZEI.
          IF SY-SUBRC NE 0.
            WRITE: / 'Error_Read_BSIS', XBSEG-BUKRS, XBSEG-BELNR,
                                        XBSEG-GJAHR, XBSEG-BUZEI.
            DELETE XBSEG.
          ENDIF.
          BSIS-VBUND = XBSEG-VBUND.
          XBSIS = BSIS.
          APPEND XBSIS.
        when 'K'.
          SELECT SINGLE * FROM BSIK WHERE
                           BUKRS EQ XBSEG-BUKRS
                           AND LIFNR EQ XBSEG-LIFNR
                           AND UMSKS EQ XBSEG-UMSKS
                           AND UMSKZ EQ XBSEG-UMSKZ
                           AND AUGDT EQ XBSEG-AUGDT
                           AND AUGBL EQ XBSEG-AUGBL
                           AND ZUONR EQ XBSEG-ZUONR
                           AND GJAHR EQ XBSEG-GJAHR
                           AND BELNR EQ XBSEG-BELNR
                           AND BUZEI EQ XBSEG-BUZEI.
          IF SY-SUBRC NE 0.
            WRITE: / 'Error_Read_BSIK', XBSEG-BUKRS, XBSEG-BELNR,
                                        XBSEG-GJAHR, XBSEG-BUZEI.
            DELETE XBSEG.
          ENDIF.
          BSIK-VBUND = XBSEG-VBUND.
          XBSIK = BSIK.
          APPEND XBSIK.

        when 'D'.
          SELECT SINGLE * FROM BSID WHERE
                       BUKRS EQ XBSEG-BUKRS
                       AND KUNNR EQ XBSEG-KUNNR
                       AND UMSKS EQ XBSEG-UMSKS
                       AND UMSKZ EQ XBSEG-UMSKZ
                       AND AUGDT EQ XBSEG-AUGDT
                       AND AUGBL EQ XBSEG-AUGBL
                       AND ZUONR EQ XBSEG-ZUONR
                       AND GJAHR EQ XBSEG-GJAHR
                       AND BELNR EQ XBSEG-BELNR
                       AND BUZEI EQ XBSEG-BUZEI.
          IF SY-SUBRC NE 0.
            WRITE: / 'Error_Read_BSID', XBSEG-BUKRS, XBSEG-BELNR,
                                        XBSEG-GJAHR, XBSEG-BUZEI.
            DELETE XBSEG.
          ENDIF.
          BSID-VBUND = XBSEG-VBUND.
          XBSID = BSID.
          APPEND XBSID.
      endcase.
    ELSE.                                "cleared item
      CASE XBSEG-KOART.
        when 'S'.
          SELECT SINGLE * FROM BSAS WHERE
                          BUKRS EQ XBSEG-BUKRS
                          AND HKONT EQ XBSEG-HKONT
                          AND AUGDT EQ XBSEG-AUGDT
                          AND AUGBL EQ XBSEG-AUGBL
                          AND ZUONR EQ XBSEG-ZUONR
                          AND GJAHR EQ XBSEG-GJAHR
                          AND BELNR EQ XBSEG-BELNR
                          AND BUZEI EQ XBSEG-BUZEI.

          IF SY-SUBRC NE 0.

            WRITE: / 'Error_Read_BSAS', XBSEG-BUKRS, XBSEG-BELNR,
                                        XBSEG-GJAHR, XBSEG-BUZEI.
            DELETE XBSEG.
          ENDIF.
          BSAS-VBUND = XBSEG-VBUND.
          XBSAS = BSAS.
          APPEND XBSAS.
        when 'K'.
          SELECT SINGLE * FROM BSAK WHERE
                           BUKRS EQ XBSEG-BUKRS
                           AND LIFNR EQ XBSEG-LIFNR
                           AND UMSKS EQ XBSEG-UMSKS
                           AND UMSKZ EQ XBSEG-UMSKZ
                           AND AUGDT EQ XBSEG-AUGDT
                           AND AUGBL EQ XBSEG-AUGBL
                           AND ZUONR EQ XBSEG-ZUONR
                           AND GJAHR EQ XBSEG-GJAHR
                           AND BELNR EQ XBSEG-BELNR
                           AND BUZEI EQ XBSEG-BUZEI.
          IF SY-SUBRC NE 0.
            WRITE: / 'Error_Read_BSAK', XBSEG-BUKRS, XBSEG-BELNR,
                                        XBSEG-GJAHR, XBSEG-BUZEI.
            DELETE XBSEG.
          ENDIF.
          BSAK-VBUND = XBSEG-VBUND.
          XBSAK = BSAK.
          APPEND XBSAK.
        when 'D'.
          SELECT SINGLE * FROM BSAD WHERE
                               BUKRS EQ XBSEG-BUKRS
                               AND KUNNR EQ XBSEG-KUNNR
                               AND UMSKS EQ XBSEG-UMSKS
                               AND UMSKZ EQ XBSEG-UMSKZ
                               AND AUGDT EQ XBSEG-AUGDT
                               AND AUGBL EQ XBSEG-AUGBL
                               AND ZUONR EQ XBSEG-ZUONR
                               AND GJAHR EQ XBSEG-GJAHR
                               AND BELNR EQ XBSEG-BELNR
                               AND BUZEI EQ XBSEG-BUZEI.
          IF SY-SUBRC NE 0.
            WRITE: / 'Error_Read_BSAD', XBSEG-BUKRS, XBSEG-BELNR,
                                        XBSEG-GJAHR, XBSEG-BUZEI.
            DELETE XBSEG.
          ENDIF.
          BSAD-VBUND = XBSEG-VBUND.
          XBSAD = BSAD.
          APPEND XBSAD.
      endcase.
    ENDIF.
  ENDLOOP.

  UPDATE BSEG FROM TABLE XBSEG.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    WRITE: / 'Error_Update_BSEG'.
    STOP.
  ENDIF.

  DESCRIBE TABLE XBSIS LINES SY-TFILL.
  IF SY-TFILL GT 0.
    UPDATE BSIS FROM TABLE XBSIS.
    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      WRITE: / 'Error_Update_BSIK'.
      STOP.
    ENDIF.
  ENDIF.

  DESCRIBE TABLE XBSAS LINES SY-TFILL.
  IF SY-TFILL GT 0.
    UPDATE BSAS FROM TABLE XBSAS.
    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      WRITE: / 'Error_Update_BSAK'.
      STOP.
    ENDIF.
  ENDIF.

  COMMIT WORK.
  WRITE:/'Updated Trading Partner(VBUND)in BSEG,BSIS,BSAS tables'.

endform.
