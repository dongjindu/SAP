REPORT ZZVBUND1.
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
      V_ERROR TYPE C,
      V_FLAG  TYPE C.

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


  LOOP AT XBKPF.

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

      IF YBSEG-LIFNR  <> SPACE  AND YBSEG-VBUND EQ SPACE
                                AND YBSEG-KOART EQ 'S'.
        CALL FUNCTION 'FI_VENDOR_DATA'
             EXPORTING
                  I_BUKRS        = ''
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

        IF YBSEG-KUNNR  <> SPACE  AND YBSEG-VBUND EQ SPACE
                                  AND YBSEG-KOART EQ 'S'.

          CALL FUNCTION 'FI_CUSTOMER_DATA'
               EXPORTING
                    I_BUKRS          = ''
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

      PERFORM CHECK_UNIQUE_VBUND.
    ELSE.
      DELETE XBKPF.
    ENDIF.
  ENDLOOP.

* print the list of selected documents as well as error documents.

  PERFORM PRINT_VBUND_LOG.

* update vbund in various FI tables if update flag is selected.

  PERFORM UPDATE_VBUND.



*---------------------------------------------------------------------*
*       FORM Check_unique_vbund                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CHECK_UNIQUE_VBUND.

  CLEAR XVBUND.
  CLEAR V_ERROR.

* check if T003-XGSUB is space.

  SELECT SINGLE * FROM T003 WHERE BLART EQ XBKPF-BLART.
  IF SY-SUBRC EQ 0 AND T003-XGSUB EQ SPACE.

* read the first ybseg where vbund  is filled.

    LOOP AT YBSEG WHERE VBUND NE SPACE.
      EXIT.
    ENDLOOP.

* move xbseg-vbund to v_vbund.

    IF SY-SUBRC = 0.
      XVBUND = YBSEG-VBUND.
    ENDIF.

* check if all rows in xbseg where vbund NE space have the same vbund.
* if other rows have different vbund then set error flag (V_error)

    LOOP AT YBSEG WHERE VBUND NE SPACE.
      IF YBSEG-VBUND NE XVBUND .
        V_ERROR = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

* If no error , then fill all the rows with v_vbund where vbund is space
    IF V_ERROR NE 'X'.
      LOOP AT YBSEG .
        XBSEG = YBSEG.
        IF YBSEG-VBUND EQ SPACE.
          XBSEG-VBUND = XVBUND.
        ENDIF.
        APPEND XBSEG.
      ENDLOOP.

    ELSE.

* for error log..
      EBKPF = XBKPF.
      APPEND EBKPF.
      DELETE XBKPF.

    ENDIF.
  ELSE.

* if T003-XGSUB has been set for the document type .....

    LOOP AT YBSEG WHERE VBUND NE SPACE.
      XBSEG = YBSEG.
      APPEND XBSEG.
    ENDLOOP.
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM print_vbund_log                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PRINT_VBUND_LOG.

  DESCRIBE TABLE XBKPF LINES SY-TFILL.
  IF SY-TFILL > 0.
    WRITE:/'Documents for which Trading partner can be updated' .
    WRITE:/'BUKRS, GJHAR, Document Number'.
    WRITE:/'-------------------------------'.
  ENDIF.
  LOOP AT XBKPF.
    WRITE: / XBKPF-BUKRS, XBKPF-GJAHR, XBKPF-BELNR .
  ENDLOOP.
  WRITE:/'-------------------------------'.
  SKIP 2.
  DESCRIBE TABLE EBKPF LINES SY-TFILL.
  IF SY-TFILL > 0.

    WRITE:/' The following Documents CANNOT be processed as they have'.
    WRITE: ' errors.The Trading partner is not unique for the document'.
    WRITE: ' and (T003-XGSUB) is not set for the Document Type'.
  ENDIF.
  LOOP AT EBKPF.
    WRITE: / 'Error',EBKPF-BELNR .
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM update_vbund                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM UPDATE_VBUND.

  DESCRIBE TABLE XBSEG LINES SY-TFILL.
  CHECK UPDATE NE SPACE AND SY-TFILL GT 0.

  LOOP AT XBSEG.
    IF XBSEG-AUGBL IS INITIAL.           " open items
      CASE XBSEG-KOART.
        WHEN 'S'.
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
        WHEN 'K'.
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

        WHEN 'D'.
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
      ENDCASE.
    ELSE.                                "cleared item
      CASE XBSEG-KOART.
        WHEN 'S'.
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
        WHEN 'K'.
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
        WHEN 'D'.
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
      ENDCASE.
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

ENDFORM.
