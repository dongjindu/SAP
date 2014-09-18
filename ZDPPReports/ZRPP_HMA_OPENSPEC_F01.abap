*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_F01                                        *
*----------------------------------------------------------------------*

FORM P2000_GET_DATA.
  DATA : LT_EDIDC LIKE TABLE OF EDIDC WITH HEADER LINE,
         LT_EDIDD LIKE TABLE OF EDIDD WITH HEADER LINE,
         LT_ZSEG  LIKE TABLE OF ZSPSEG WITH HEADER LINE,
         LT_ZWOSUM LIKE TABLE OF ZTPP_WOSUM WITH HEADER LINE,
         LT_DATA LIKE TABLE OF ZTPP_WOSUM WITH HEADER LINE.

  SELECT  * INTO CORRESPONDING FIELDS OF TABLE LT_EDIDC
  FROM EDIDC
  WHERE MESTYP = 'ZSPEC_ORD_MST'.


  LOOP AT  LT_EDIDC .

    CALL FUNCTION 'IDOC_READ_COMPLETELY'
         EXPORTING
              DOCUMENT_NUMBER         = LT_EDIDC-DOCNUM
         TABLES
              INT_EDIDD               = LT_EDIDD
         EXCEPTIONS
              DOCUMENT_NOT_EXIST      = 1
              DOCUMENT_NUMBER_INVALID = 2
              OTHERS                  = 3.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      LOOP AT LT_EDIDD WHERE SEGNAM = 'ZSPSEG'.
        LT_ZSEG = LT_EDIDD-SDATA.
        APPEND : LT_ZSEG.
      ENDLOOP.
    ENDIF.
    CLEAR : LT_ZSEG, LT_EDIDD, LT_EDIDD[].
  ENDLOOP.

  CHECK NOT LT_ZSEG[] IS INITIAL.

  RANGES : R_ATNAM  FOR CABN-ATNAM,
           R_ATNAMC FOR CABN-ATNAM.

**#0. Set Range For 'P_219%' .
  R_ATNAM-LOW = 'P_219*'.
  R_ATNAM-OPTION = 'CP'.
  R_ATNAM-SIGN = 'I'.
  APPEND R_ATNAM.

**#1 GET ZTPP_WOSUM DATA .
*  PERFORM SHOW_PROGRESS     USING 'Data gathering...' '5'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_ZWOSUM
  FROM ZTPP_WOSUM
  WHERE WO_SER IN P_WOSER
    AND NATION IN S_NATION
    AND DEALER IN S_DEALER
    AND WOMODDATE  IN P_DATUM.


  LOOP AT LT_ZWOSUM.
    READ TABLE LT_ZSEG WITH KEY WO_SER = LT_ZWOSUM-WO_SER
                                NATION = LT_ZWOSUM-NATION
                                DEALER = LT_ZWOSUM-DEALER
                                EXTC   = LT_ZWOSUM-EXTC
                                INTC   = LT_ZWOSUM-INTC.


    IF SY-SUBRC <> 0 .
      APPEND LT_ZWOSUM TO LT_DATA.
    ENDIF.
  ENDLOOP.

  GT_DATA[] = LT_DATA[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P1000_START_PROGRESSBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_START_PROGRESSBAR USING PERCENT.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PERCENT
            TEXT       = TEXT-001
       EXCEPTIONS
            OTHERS     = 1.
ENDFORM.                    " P1000_START_PROGRESSBAR


*---------------------------------------------------------------------*
*       FORM P4000_CONVERSION_ATINN                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VALUE                                                       *
*  -->  P_ATINN                                                       *
*---------------------------------------------------------------------*
FORM P4000_CONVERSION_ATINN USING P_VALUE P_ATINN .

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = P_VALUE
       IMPORTING
            OUTPUT = P_ATINN.

ENDFORM.
