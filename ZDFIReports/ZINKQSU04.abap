***********************************************************************
* Form routines for jobs besides printing and file handling
***********************************************************************


*---------------------------------------------------------------------*
*       FORM FIND_X059                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  BUKRS                                                         *
*  -->  LIFNR                                                         *
*  -->  QSSKZ                                                         *
*---------------------------------------------------------------------*
FORM FIND_X059 USING BUKRS LIFNR QSSKZ.

  CLEAR X059.
* READ TABLE AGENTTAB WITH KEY BUKRS BINARY SEARCH.
  LOOP AT KTAB WHERE BUKRS = BUKRS
               AND   LIFNR = LIFNR.
  ENDLOOP.
  LOOP AT X059 WHERE LAND1 = AGENTTAB-LAND1
               AND ( WAERS = AGENTTAB-WAERS
               OR    WAERS = SPACE )
               AND   QSSKZ = QSSKZ
               AND   QLAND = KTAB-QLAND.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    LOOP AT X059 WHERE LAND1 = AGENTTAB-LAND1
                 AND ( WAERS = AGENTTAB-WAERS
                 OR    WAERS = SPACE )
                 AND   QSSKZ = QSSKZ.
    ENDLOOP.
  ENDIF.
ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM CORRECT_TAXID                                            *
*---------------------------------------------------------------------*
*       Checks type of tax id and created code with digits only       *
*---------------------------------------------------------------------*
*  -->  TYPE  Returns type of tax ID                                  *
*       '1'   Employer Identification Number EIN                      *
*       '2'   Social Security Number                                  *
*       ' '   Unidentified Number                                     *
*  -->  CODE                                                          *
*  -->  SSN                                                           *
*  -->  EIN                                                           *
*---------------------------------------------------------------------*


FORM CORRECT_TAXID USING TYPE CODE SSN EIN.

  DATA: TIN(9),
        TIN_1(16),
        TIN_2(11).

  TIN_1 = SSN.
  TIN_2 = EIN.
*NOTE 593071 begin
  IF tin_1+3(1) = '-'.
    TIN(3)   = TIN_1(3).
    TIN+3(2) = TIN_1+4(2).
    TIN+5(4) = TIN_1+7(4).
  else.
    tin = tin_1.
  endif.

  IF TIN CO '0123456789'.
    TYPE = '2'.
* note 454478 begin
    MOVE 'X' TO LFB1-SPERR.
* note 454478 end
  ELSE.
    if tin_2+2(1) = '-'.
      TIN(2) = TIN_2(2).
      TIN+2(7) = TIN_2+3(7).
    else.
      tin = tin_2.
    endif.
*Note 593071 end
    IF TIN CO '0123456789'.
      TYPE = '1'.
* note 454478 begin
      MOVE 'X' TO LFB1-LOEVM.
* note 454478 end
    ELSE.
      TYPE = ' '.
      TIN = SSN.
    ENDIF.

  ENDIF.

  CODE = TIN.

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM CORRECT_ZIP                                              *
*---------------------------------------------------------------------*
*       Changes the ZIP to the format required for the file           *
*---------------------------------------------------------------------*
*  -->  ZIP                                                           *
*---------------------------------------------------------------------*


FORM CORRECT_ZIP USING ZIP.

  DATA: LENGTH TYPE P,
        H_ZIP LIKE LFA1-PSTLZ,
        DIRT.
*NOTE 599923 begin
  WHILE NOT ZIP CO ' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
*NOTE 599923 end
    H_ZIP = ZIP.
    SHIFT H_ZIP BY SY-FDPOS PLACES.
    DIRT  = H_ZIP.
    REPLACE DIRT WITH ' ' INTO ZIP.
  ENDWHILE.

  CONDENSE ZIP NO-GAPS.
  LENGTH = STRLEN( ZIP ).
  WRITE '00000000000000' TO ZIP+LENGTH.

ENDFORM.
*eject
* Note 454478 begin
*---------------------------------------------------------------------*
*       FORM CORRECT_TEL                                              *
*---------------------------------------------------------------------*
*       Changes the TEL to the format required for the file           *
*---------------------------------------------------------------------*
*  -->  TEL                                                           *
*---------------------------------------------------------------------*

form correct_tel using phone.

  data: length type p,
        h_phone like contact-person-tel,
        dirt.
  while not phone co ' 0123456789'.
    h_phone = phone.
    shift h_phone by sy-fdpos places.
    dirt  = h_phone.
    replace dirt with ' ' into phone.
  endwhile.

  condense phone no-gaps.
  length = strlen( phone ).
  write '00000000000000' to phone+length.
endform.
* Note 454478 END
*---------------------------------------------------------------------*
*       FORM CREATE_AGENTTAB                                          *
*---------------------------------------------------------------------*
*       Generates Table for all possible withhold agents              *
*---------------------------------------------------------------------*


FORM CREATE_AGENTTAB.

  SELECT * FROM T001 WHERE BUKRS IN BUKRS.

    CLEAR AGENTTAB.
    AGENTTAB-BUKRS = T001-BUKRS.
    AGENTTAB-ADRNR = T001-ADRNR.                            "SADR40A
    AGENTTAB-WAERS = T001-WAERS.
    AGENTTAB-SPRAS = T001-SPRAS.
    AGENTTAB-LAND1 = T001-LAND1.
    AGENTTAB-BUTXT = T001-BUTXT.

    SELECT SINGLE * FROM T005 WHERE LAND1 = T001-LAND1.
    IF SY-SUBRC = 0.
      AGENTTAB-ADDRS = T005-ADDRS.
    ENDIF.

    APPEND AGENTTAB.

  ENDSELECT.

  SORT AGENTTAB.

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM CREATE_FORM1042                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CREATE_FORM1042.

  DATA: BEGIN OF DAYS,
          DAY1(2) TYPE N VALUE '07',
          DAY2(2) TYPE N VALUE '15',
          DAY3(2) TYPE N VALUE '22',
          DAY4(2) TYPE N VALUE '31',
        END OF DAYS,

        BEGIN OF DATE,
          YEAR(4) TYPE N,
          MONTH(2) TYPE N,
          DAY(2)  TYPE N,
        END OF DATE.

  DATE-YEAR = FISCAL.

  LOOP AT AGENTTAB.
    FORM1042-BUKRS = AGENTTAB-BUKRS.
    DO 12 TIMES.
      FORM1042-MONTH = SY-INDEX.
      DATE-MONTH = SY-INDEX.
      DO 4 TIMES VARYING DATE-DAY FROM DAYS-DAY1 NEXT DAYS-DAY2.
        FORM1042-DAY  = DATE-DAY.
        APPEND FORM1042.
      ENDDO.
    ENDDO.
  ENDLOOP.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM CREATE_KTAB                                              *
*---------------------------------------------------------------------*
*       Creates KTAB and RANGES LIFNR                                 *
*---------------------------------------------------------------------*


FORM CREATE_KTAB.

  SELECT * FROM LFB1 WHERE QLAND NE SPACE
                     AND   BUKRS IN BUKRS.
    LOOP AT AGENTTAB WHERE BUKRS EQ LFB1-BUKRS
                     AND   LAND1 NE LFB1-QLAND.
    ENDLOOP.
    CHECK SY-SUBRC EQ 0.

    KTAB-LIFNR = LFB1-LIFNR.
    KTAB-BUKRS = LFB1-BUKRS.
    KTAB-QSZDT = LFB1-QSZDT.
    KTAB-QSREC = LFB1-QSREC.
    ktab-qsbgr(1) = '0'.
    KTAB-QSBGR+1(1) = LFB1-QSBGR.
    KTAB-QLAND = LFB1-QLAND.
*Note 497879 begin
    if not new_with is initial.
      select * from lfbw
               where bukrs eq ktab-bukrs
               and   lifnr eq ktab-lifnr.
      endselect.
      ktab-qsbgr = lfbw-wt_wtexrs.
      ktab-qsrec = lfbw-qsrec.
    endif.
*Note 497879 end
    READ TABLE KTAB.

    CHECK SY-SUBRC NE 0.

    APPEND KTAB.

    LIFNR-LOW = LFB1-LIFNR.

    APPEND LIFNR.

  ENDSELECT.

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM CREATE_RTAB                                              *
*---------------------------------------------------------------------*
*       Creates RTAB                                                  *
*---------------------------------------------------------------------*
*  -->  FISCAL Calender Year for reprting                             *
*---------------------------------------------------------------------*
FORM CREATE_RTAB USING FISCAL.

  DATA: T_BSAK LIKE BSAK OCCURS 1 WITH HEADER LINE,
        X_BSAK LIKE BSAK OCCURS 1 WITH HEADER LINE,
        KURS LIKE BKPF-KURSF,

        BEGIN OF GJAHR,
          LOW  LIKE BSAK-AUGDT VALUE '00000000',
          HIGH LIKE BSAK-AUGDT VALUE '99999999',
        END OF GJAHR.

  WRITE FISCAL TO GJAHR-LOW(4).
  WRITE FISCAL TO GJAHR-HIGH(4).

*----------------------------------- for new WT select into itab needed
*Note 494042 begin
  loop at lifnr.
    SELECT * FROM BSAK
             WHERE BUKRS IN BUKRS
              AND   LIFNR EQ LIFNR-LOW
              AND   AUGDT BETWEEN GJAHR-LOW AND GJAHR-HIGH
*Note 497879 begin
                       AND   QSSKZ in cls_wtcd
*Note 497879 end
                       AND   XZAHL NE SPACE.
      append bsak to t_bsak.
    endselect.
  endloop.
*Note 494042 end
  LOOP AT T_BSAK.

    X_BSAK = T_BSAK.

*-------- for new withholding tax functionality read WT info explicit
    IF NOT NEW_WITH IS INITIAL.

      CLEAR X_WITH_ITEM[].

      CALL FUNCTION 'FI_WT_READ_WT_INFO'
           EXPORTING
                I_BUKRS     = T_BSAK-BUKRS
                I_BELNR     = T_BSAK-BELNR
                I_GJAHR     = T_BSAK-GJAHR
                I_BUZEI     = T_BSAK-BUZEI
           TABLES
                T_WITH_ITEM = X_WITH_ITEM
           EXCEPTIONS
                NOT_FOUND   = 1
                OTHERS      = 2.

      LOOP AT X_WITH_ITEM WHERE WITHT IN WITHT
*Note 497879 begin
                            and wt_withcd in ext_wtcd.
*Note 497879 end
        X_BSAK-QSSKZ = X_WITH_ITEM-WT_WITHCD.
        X_BSAK-QBSHB = ABS( X_WITH_ITEM-WT_QBSHB ).
        X_BSAK-QSSHB = ABS( X_WITH_ITEM-WT_QSSHB ).
        APPEND X_BSAK.
      ENDLOOP.

    ELSE.

      APPEND X_BSAK.

    ENDIF.

  ENDLOOP.

*-------------- at this point x_bsak contains one entry per each real
*-------------- BSAK item and each withh. which matches the selection

  LOOP AT X_BSAK.

    BSAK = X_BSAK.

    RTAB-BUKRS = BSAK-BUKRS.
    RTAB-LIFNR = BSAK-LIFNR.
    RTAB-QSSKZ = BSAK-QSSKZ.

    READ TABLE AGENTTAB WITH KEY BUKRS BINARY SEARCH.
    PERFORM FIND_X059 USING BSAK-BUKRS BSAK-LIFNR BSAK-QSSKZ.
    RTAB-QSCOD = X059-QSCOD.

    KURS       = BSAK-DMBTR / BSAK-WRBTR.

    CASE BSAK-SHKZG.
      WHEN 'H'.
        RTAB-QSSHH = BSAK-QSSHB * -1 * KURS.
        RTAB-QBSHH = BSAK-QBSHB * -1 * KURS.
        RTAB-ALLOW = 0.
      WHEN 'S'.
        RTAB-QSSHH = BSAK-QSSHB * KURS.
        RTAB-QBSHH = BSAK-QBSHB * KURS.
        RTAB-ALLOW = 0.
        IF X059-QSCOD = '15'.
          SELECT * FROM BSAK WHERE BUKRS = BSAK-BUKRS
                             AND   LIFNR = BSAK-LIFNR
                             AND   AUGBL = BSAK-AUGBL
                             AND   AUGDT = BSAK-AUGDT
                             AND   QSSKZ NE SPACE
                             AND   XZAHL = SPACE.
            BSAK-QSSHB = BSAK-QSSHB * BSAK-DMBTR / BSAK-WRBTR.
            IF X059-QMBAB < BSAK-QSSHB.
              RTAB-ALLOW = RTAB-ALLOW + X059-QMBAB.
            ELSE.
              BSAK-QSSHB = BSAK-QSSHB * BSAK-DMBTR / BSAK-WRBTR.
              RTAB-ALLOW = RTAB-ALLOW + BSAK-QSSHB.
            ENDIF.
          ENDSELECT.
        ENDIF.
    ENDCASE.

    COLLECT RTAB.

    IF NOT RECO1042 IS INITIAL.

      CLEAR FORM1042.                        "<--- INSERT 70809

      FORM1042-BUKRS = BSAK-BUKRS.
      FORM1042-MONTH = BSAK-BUDAT+4(2).
      FORM1042-DAY   = BSAK-BUDAT+6(2).

      READ TABLE FORM1042 WITH KEY FORM1042 BINARY SEARCH.
      IF SY-SUBRC NE 0.
        READ TABLE FORM1042 INDEX SY-TABIX.
      ENDIF.

      FORM1042-AMOUNT = FORM1042-AMOUNT + RTAB-QBSHH.

      MODIFY FORM1042 INDEX SY-TABIX.

    ENDIF.

  ENDLOOP.

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM CREATE_T059                                              *
*---------------------------------------------------------------------*
*       Creates an internal table T059 out of T059F and T059Q         *
*---------------------------------------------------------------------*
*  -->  .....                                                         *
*---------------------------------------------------------------------*

FORM CREATE_T059.

  DATA: BEGIN OF LAND OCCURS 2,
          LAND1 LIKE T059F-LAND1,
          WAERS LIKE T059F-WAERS,
        END OF LAND.

  DATA: BEGIN OF XT059F OCCURS 100.
          INCLUDE STRUCTURE T059F.
  DATA: END OF XT059F.

  DATA: XT059FB  LIKE T059FB OCCURS 1 WITH HEADER LINE,
        H_T059FB LIKE T059FB,
        H_DATE   LIKE SY-DATUM.
*NOTE 826457
  DATA : WA_KTAB LIKE LINE OF KTAB,
         VALID_DATE TYPE T059FB-WT_VALID.
*NOTE 826457
  RANGES:
     LAND1 FOR T059Q-LAND1.

  LAND1 = 'IEQ'.

  LOOP AT AGENTTAB.
    LAND-LAND1 = AGENTTAB-LAND1.
    LAND1-LOW  = AGENTTAB-LAND1.
    LAND-WAERS = AGENTTAB-WAERS.
    READ TABLE LAND.
    IF SY-SUBRC NE 0.
      APPEND LAND.
      APPEND LAND1.
    ENDIF.
  ENDLOOP.

  IF NEW_WITH IS INITIAL.
*---------------------------------------------------- old functionality
    SELECT * FROM T059Q WHERE LAND1 IN LAND1.
      X059-LAND1 = T059Q-LAND1.
      X059-QSCOD = T059Q-QSCOD.
      X059-QSSKZ = T059Q-QSSKZ.
      IF T059Q-XQFOR EQ SPACE.
        X059-QSATZ = T059Q-QSATZ.
        X059-WAERS = SPACE.
        X059-QLAND = SPACE.
        X059-QMBAB = 0.
        APPEND X059.
      ELSE.
        SELECT * FROM T059F INTO TABLE XT059F
                            WHERE LAND1 = T059Q-LAND1
                            AND   QSSKZ = T059Q-QSSKZ.

        SORT XT059F.

        LOOP AT LAND WHERE LAND1 = T059Q-LAND1.
          X059-WAERS = XT059F-WAERS.
          X059-QLAND = SPACE.
          LOOP AT XT059F WHERE WAERS = LAND-WAERS.
*           Manual AT NEW QLAND to allow access to fields on the
*           right side of the key
            IF X059-QLAND NE XT059F-QLAND
               OR  XT059F-QLAND EQ SPACE.
              X059-QLAND = XT059F-QLAND.
              X059-QSATZ = XT059F-QSATZ.
              IF T059Q-QSCOD = '15'.
                X059-QMBAB = XT059F-QMBAB.
              ELSE.
                X059-QMBAB = 0.
              ENDIF.
              APPEND X059.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDSELECT.
  ELSE.
*------------------------------------------------- new WT functionality
    H_DATE(4) = FISCAL.
    H_DATE+4(4) = '1231'.

    SELECT * FROM T059Z WHERE LAND1 IN LAND1
                          AND WITHT IN WITHT.
      X059-LAND1 = T059Z-LAND1.
      X059-QSCOD = T059Z-QSCOD.
      X059-QSSKZ = T059Z-WT_WITHCD.
      IF T059Z-XQFOR EQ SPACE.
        X059-QSATZ = T059Z-QSATZ.
        X059-WAERS = SPACE.
        X059-QLAND = SPACE.
        X059-QMBAB = 0.
        APPEND X059.
      ELSE.

*      ***NOTE 826457

        LOOP AT KTAB INTO WA_KTAB.

          SELECT MAX( WT_VALID ) INTO VALID_DATE FROM T059FB WHERE
          LAND1 = T059Z-LAND1 AND WITHT = T059Z-WITHT
          AND WT_WITHCD = T059Z-WT_WITHCD AND QLAND = WA_KTAB-QLAND
          AND WT_VALID LE H_DATE.

          SELECT SINGLE * FROM T059FB INTO H_T059FB
                                 WHERE LAND1     = T059Z-LAND1
                                 AND   WITHT     = T059Z-WITHT
                                 AND   WT_WITHCD = T059Z-WT_WITHCD
                                 AND   WT_VALID  = VALID_DATE.

          IF SY-SUBRC = 0.
            APPEND H_T059FB TO XT059FB.
          ENDIF.


        ENDLOOP.


        SORT XT059FB DESCENDING BY WT_VALID.

****END OF NOTE 826457


        LOOP AT LAND WHERE LAND1 = T059Z-LAND1.
          X059-WAERS = XT059FB-WAERS.
          X059-QLAND = SPACE.
          LOOP AT XT059FB WHERE WAERS = LAND-WAERS.
*           Manual AT NEW QLAND to allow access to fields on the
*           right side of the key
            IF X059-QLAND NE XT059FB-QLAND
               OR  XT059FB-QLAND EQ SPACE.
              X059-QLAND = XT059FB-QLAND.
              X059-QSATZ = XT059FB-QSATZ.
              IF T059Z-QSCOD = '15'.
                X059-QMBAB = XT059FB-QMBAB.
              ELSE.
                X059-QMBAB = 0.
              ENDIF.
              APPEND X059.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDSELECT.



  ENDIF.

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM GET_BUKRS                                                *
*---------------------------------------------------------------------*
*       Reads all tables linked RTAB-BUKRS                            *
*---------------------------------------------------------------------*
*  -->  BUKRS from AT NEW BUKRS  in LOOP RTAB                         *
*---------------------------------------------------------------------*


FORM GET_BUKRS USING BUKRS.

  READ TABLE AGENTTAB WITH KEY BUKRS BINARY SEARCH.
  AGENTTAB_IDX = SY-TABIX.

  CLEAR ADDR1_SEL.
  ADDR1_SEL-ADDRNUMBER = T001-ADRNR.                        "SADR40A
  ADDR1_SEL-NATION     = ' '.
  CALL FUNCTION 'ADDR_GET'
       EXPORTING
            ADDRESS_SELECTION = ADDR1_SEL
       IMPORTING
            SADR              = SADR
       EXCEPTIONS
            OTHERS            = 1.
  IF SY-SUBRC NE 0.
    CLEAR SADR.
  ENDIF.

*  select single * from sadr where adrnr = agenttab-adrnr
*                            and   natio = ' '.

  CLEAR T001Z.
  SELECT SINGLE * FROM T001Z WHERE BUKRS = BUKRS
                             AND   PARTY = 'SAPTIN'.

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM GET_LIFNR                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  BUKRS                                                         *
*  -->  LIFNR                                                         *
*---------------------------------------------------------------------*


FORM GET_LIFNR USING BUKRS LIFNR.

  CLEAR BSAK-DMB21.
  CLEAR BSAK-DMB22.
  CLEAR BSAK-DMB23.
  CLEAR BSAK-DMB31.

  SELECT SINGLE * FROM LFA1 WHERE LIFNR = LIFNR.
*Note 454478 begin
** Check POBOX
  IF LFA1-PFACH NE SPACE.
    LFA1-STRAS(8) = 'P.O.BOX '.
    LFA1-STRAS+9(10) = LFA1-PFACH.
    LFA1-PSTLZ = LFA1-PSTL2.
  ENDIF.
*Note 454478 end

  PERFORM CORRECT_TAXID USING TIN_TYPE
                              TIN_CODE
                              LFA1-STCD1
                              LFA1-STCD2.

  LOOP AT KTAB WHERE BUKRS = BUKRS
               AND   LIFNR = LIFNR.
  ENDLOOP.

  CLEAR T005T.
  SELECT SINGLE * FROM T005T WHERE LAND1 = LFA1-LAND1
                             AND   SPRAS = AGENTTAB-SPRAS.

  CLEAR T005R.
  SELECT SINGLE * FROM T005R WHERE SPRAS = AGENTTAB-SPRAS
                             AND   LAND1 EQ AGENTTAB-LAND1
                             AND   QLAND EQ KTAB-QLAND.
ENDFORM.
*eject
