***********************************************************************
* Form routines to handle printing besides SAPSCRIPT in report RFKQSU40
***********************************************************************


*---------------------------------------------------------------------*
*       FORM PRINT_1042                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PRINT_1042.
  PAGETYPE = '5'.

  LOOP AT FORM1042.

    AT NEW BUKRS.
      SELECT SINGLE * FROM T001
        WHERE BUKRS EQ FORM1042-BUKRS.
      NEW-PAGE WITH-TITLE.
    ENDAT.

    AT NEW MONTH.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      CASE FORM1042-MONTH.
        WHEN '01'.
          WRITE:  6(10) TEXT-M01.
        WHEN '02'.
          WRITE:  6(10) TEXT-M02.
        WHEN '03'.
          WRITE:  6(10) TEXT-M03.
        WHEN '04'.
          WRITE:  6(10) TEXT-M04.
        WHEN '05'.
          WRITE:  6(10) TEXT-M05.
        WHEN '06'.
          WRITE:  6(10) TEXT-M06.
        WHEN '07'.
          WRITE:  6(10) TEXT-M07.
        WHEN '08'.
          WRITE:  6(10) TEXT-M08.
        WHEN '09'.
          WRITE:  6(10) TEXT-M09.
        WHEN '10'.
          WRITE:  6(10) TEXT-M10.
        WHEN '11'.
          WRITE:  6(10) TEXT-M11.
        WHEN '12'.
          WRITE:  6(10) TEXT-M12.
      ENDCASE.
    ENDAT.

    WRITE: (7) FORM1042-DAY UNDER FORM1042-DAY,
          (20) FORM1042-AMOUNT CURRENCY T001-WAERS.
    PERFORM FILL_VLINE_P1042.
    NEW-LINE.

    AT END OF MONTH.
      SUM.
      WRITE: /1(50) SY-ULINE.
      FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
      WRITE: /17 '**',
          (20) FORM1042-AMOUNT CURRENCY T001-WAERS
                               UNDER FORM1042-AMOUNT.
      PERFORM FILL_VLINE_P1042.
      FORMAT COLOR OFF.
      WRITE: /1(50) SY-ULINE.
      NEW-LINE.
    ENDAT.

    AT END OF BUKRS.
      SUM.
      FORMAT COLOR COL_TOTAL INTENSIFIED.
      WRITE: 6  '***        **',
            (20) FORM1042-AMOUNT  CURRENCY T001-WAERS
                                  UNDER FORM1042-AMOUNT.
      PERFORM FILL_VLINE_P1042.
      WRITE: /1(50) SY-ULINE.
      FORMAT COLOR OFF.
    ENDAT.

  ENDLOOP.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM PRINT_AT_LIFNR                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PRINT_AT_LIFNR.
  CASE DETAILRP.
    WHEN 0.
    WHEN OTHERS.
      FORMAT COLOR COL_KEY INTENSIFIED.
      WRITE: /2 LFA1-LIFNR ,
                LFA1-SORTL.
      IF DETAILRP EQ 1.
          WRITE: '                            '.
      ENDIF.
  ENDCASE.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM PRINT_DETAIL                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PRINT_DETAIL.
  IF DETAILRP EQ 2.
*Note 497879 begin
     IF RTAB-QSSHH = 0.
        QSATZ = 0.
     ELSE.
        QSATZ = X059-QSATZ.
     ENDIF.
*Note 497879 end
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE: 28 RTAB-QSCOD,
           39 RTAB-QSSKZ,
           44(22) RTAB-QSSHH CURRENCY T001-WAERS,         "Base Amount
           67(22) RTAB-ALLOW CURRENCY T001-WAERS,    "Allowance for 15
*Note 497879 begin
          100(8) QSATZ,
*          100(8) x059-qsatz,
*Note 497879 end
          109(22) RTAB-QBSHH CURRENCY T001-WAERS.        "Tax withheld
    PERFORM FILL_VLINE_D2.
    NEW-LINE.
    FORMAT COLOR OFF.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM PRINT_END_BUKRS                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PRINT_END_BUKRS.
  FORMAT COLOR COL_TOTAL INTENSIFIED.
  CASE DETAILRP.
    WHEN 1.
      WRITE: /1(132) SY-ULINE.
      WRITE: /2'***',
             51(26)  RTAB-QSSHH CURRENCY T001-WAERS,
             78(26)  RTAB-ALLOW CURRENCY T001-WAERS,
            105(26)  RTAB-QBSHH CURRENCY T001-WAERS.
      PERFORM FILL_VLINE_D1.
    WHEN 2.
      WRITE: /2'***',
               AGENTTAB-BUKRS,
           (22)RTAB-QSSHH UNDER RTAB-QSSHH CURRENCY T001-WAERS,
           (22)RTAB-ALLOW UNDER RTAB-ALLOW CURRENCY T001-WAERS,
           (22)RTAB-QBSHH UNDER RTAB-QBSHH CURRENCY T001-WAERS.
      PERFORM FILL_VLINE_D2S.
  ENDCASE.
  FORMAT COLOR OFF.
  WRITE: /1(132) SY-ULINE.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM PRINT_END_LIFNR                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

FORM PRINT_END_LIFNR.

  CASE DETAILRP.
    WHEN 1.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: 51(26)  RTAB-QSSHH CURRENCY T001-WAERS,
             78(26)  RTAB-ALLOW CURRENCY T001-WAERS,
            105(26)  RTAB-QBSHH CURRENCY T001-WAERS.
      PERFORM FILL_VLINE_D1.
      NEW-LINE.
      FORMAT COLOR OFF.
    WHEN 2.
      WRITE: /1(132) SY-ULINE.
      FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
      WRITE: / '**'  UNDER RTAB-QSCOD,
               (22)RTAB-QSSHH UNDER RTAB-QSSHH CURRENCY T001-WAERS,
               (22)RTAB-ALLOW UNDER RTAB-ALLOW CURRENCY T001-WAERS,
               (22)RTAB-QBSHH UNDER RTAB-QBSHH CURRENCY T001-WAERS.
      PERFORM FILL_VLINE_D2S.
      WRITE: /1(132) SY-ULINE.
      FORMAT COLOR OFF.
  ENDCASE.
ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM PRINT_LOG                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

FORM PRINT_LOG.

  PAGETYPE = '4'.

  NEW-PAGE WITH-TITLE.

  LOOP AT AGENTTAB.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE: /2 AGENTTAB-BUKRS, 25  AGENTTAB-COUNT.
    PERFORM FILL_VLINE_P1042.
    AT LAST.
      FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
      SUM.
      WRITE: /1(50) SY-ULINE.
      WRITE: /2 '****',  AGENTTAB-COUNT UNDER AGENTTAB-COUNT.
      PERFORM FILL_VLINE_P1042.
      WRITE: /1(50) SY-ULINE.
      FORMAT COLOR OFF.
    ENDAT.
  ENDLOOP.

  IF NOT CREATFIL IS INITIAL.
    SKIP 5.
    WRITE: /2   TEXT-002.
    WRITE: /2 TAPENAME.
  ENDIF.

ENDFORM.




*---------------------------------------------------------------------*
*       FORM PRINT_TOP                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PAGETYPE                                                      *
*---------------------------------------------------------------------*
FORM PRINT_TOP USING PAGETYPE.
  FORMAT COLOR COL_HEADING INTENSIFIED.

  CASE PAGETYPE.
    WHEN '1'.
       WRITE: /5 TEXT-H00, AGENTTAB-BUKRS, AGENTTAB-BUTXT.
       WRITE:  1(1) SY-VLINE, 132(1) SY-VLINE.
       WRITE: /1(132) SY-ULINE.
    WHEN '2'.
       WRITE: /5 TEXT-H00, AGENTTAB-BUKRS, AGENTTAB-BUTXT.
       WRITE:  1(1) SY-VLINE, 132(1) SY-VLINE.
       WRITE: /1(132) SY-ULINE.
    WHEN '4'.
    WHEN '5'.
       WRITE: /3 TEXT-H00.
       LOOP AT AGENTTAB WHERE BUKRS = FORM1042-BUKRS.
       ENDLOOP.
       WRITE:    FORM1042-BUKRS,
                 AGENTTAB-BUTXT.
       WRITE: 1(1) SY-VLINE, 50(1) SY-VLINE.
       WRITE: /1(50) SY-ULINE.
  ENDCASE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  CASE PAGETYPE.
    WHEN '1'.
      WRITE: /65 TEXT-H02,
              93 TEXT-H03,
             111 TEXT-H07.
      PERFORM FILL_VLINE_D1.
      WRITE: /1(132) SY-ULINE.
    WHEN '2'.
      WRITE: /2  TEXT-H01,             "Agent
             25  TEXT-H10,
             39  TEXT-H05,
             54  TEXT-H02,
             77  TEXT-H03,
             97  TEXT-H04,
            118  TEXT-H12.
      PERFORM FILL_VLINE_D2.
      WRITE: /25 TEXT-H11,             "Vendor
              39 TEXT-H06,
             118 TEXT-H13.
      PERFORM FILL_VLINE_D2.
      WRITE: /1(132) SY-ULINE.
    WHEN '4'.
      WRITE: /2 TEXT-H00, 26 TEXT-H14.
      PERFORM FILL_VLINE_P1042.
      WRITE: /1(50) SY-ULINE.
    WHEN '5'.
      WRITE: /6 TEXT-H08, 29 TEXT-H09.
      PERFORM FILL_VLINE_P1042.
      WRITE: /1(50) SY-ULINE.
  ENDCASE.
  FORMAT COLOR OFF.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM FILL_VLINE_P1042                                          *
*----------------------------------------------------------------------*
*       Ausgabe PRINT_1042 füllen mit SY-VLINE                         *
*--------------------------------------------------------------------- *
FORM FILL_VLINE_P1042.
      WRITE:   1(1) SY-VLINE,
              24(1) SY-VLINE,
              50(1) SY-VLINE.
ENDFORM.
*----------------------------------------------------------------------*
*       FORM FILL_VLINE_D2                                             *
*----------------------------------------------------------------------*
*       Ausgabe PRINT_DETAIL (2) füllen mit SY-VLINE                   *
*--------------------------------------------------------------------- *
FORM FILL_VLINE_D2.
      WRITE:   1(1) SY-VLINE,
              23(1) SY-VLINE,
              36(1) SY-VLINE,
              44(1) SY-VLINE,
              67(1) SY-VLINE,
              90(1) SY-VLINE,
             109(1) SY-VLINE,
             132(1) SY-VLINE.
ENDFORM.
*----------------------------------------------------------------------*
*       FORM FILL_VLINE_D2S                                            *
*----------------------------------------------------------------------*
*       Ausgabe PRINT_DETAIL (2) (Summe) füllen mit SY-VLINE           *
*--------------------------------------------------------------------- *
FORM FILL_VLINE_D2S.
      WRITE:   1(1) SY-VLINE,
              67(1) SY-VLINE,
              90(1) SY-VLINE,
             109(1) SY-VLINE,
             132(1) SY-VLINE.
ENDFORM.
*----------------------------------------------------------------------*
*       FORM FILL_VLINE_D1                                             *
*----------------------------------------------------------------------*
*       Ausgabe PRINT_END_BUKRS  füllen mit SY-VLINE                   *
*--------------------------------------------------------------------- *
FORM FILL_VLINE_D1.
      WRITE:   1(1) SY-VLINE,
              51(1) SY-VLINE,
              78(1) SY-VLINE,
             105(1) SY-VLINE,
             132(1) SY-VLINE.
ENDFORM.
