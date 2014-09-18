REPORT ZPMR_MATERIAL_DETAILS_MRP NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                     LINE-SIZE 150.

*-------------------------------------------------*
* TABLE Declare                                   *
*-------------------------------------------------*

TABLES: MARA,
        mard,
        makt,
        mbew,
        STXL,
        M_MAT1T,
        RMMG1,
        marc,
        ekpo.
*-------------------------------------------------*
* G/R DATA READ INTERNAL TABLE Declare            *
*-------------------------------------------------*

Data: begin of it_material occurs 0,
      matnr(70) type c,
      maktx like makt-maktx,
      mtart like M_MAT1T-MTART,
      lgort like RMMG1-LGORT,
      mfrpn like mara-mfrpn,
      mfrnr like mara-mfrnr,
      lgpbe like mard-lgpbe,
      labst like mard-labst,
      stprs like mbew-stprs,
      verpr like mbew-verpr.

Data: end of it_material.

data: it_tline like TLINE occurs 0 with header line.
DATA: ZTLINE LIKE TLINE OCCURS 0 WITH HEADER LINE.
DATA: ZTHEAD LIKE THEAD OCCURS 0 ."WITH HEADER LINE.
*--------------------------------------------------
* Search Condition SELECTION WINDOW.
*--------------------------------------------------

*Selection Criteria:
*Material Type
*MRP Type
*Lot Size
*Company Code
*Plant
*Material
*Storage Location
*Valuation Class
*Key Date
*Period To Analyze From Date
*Period To Analyze To Date

SELECTION-SCREEN BEGIN OF BLOCK FILE WITH FRAME TITLE TEXT-P01.
SELECT-OPTIONS: S_MATNR   FOR  MARA-MATNR,       " Material NO.
*                maktx for makt-maktx ,
                maktg for makt-maktg,
               MTART for M_MAT1T-MTART default 'ERSA',
               verpr for mbew-verpr,
            LGORT for RMMG1-LGORT,
            labst for mard-labst,
            mfrpn for mara-mfrpn,
            mfrnr for mara-mfrnr matchcode object kred.
PARAMETERS:
            werks like mard-werks obligatory default 'P001'.
*PARAMETERS:
*            MTART like M_MAT1T-MTART obligatory default 'ERSA',
*            LGORT LIKE RMMG1-LGORT obligatory default 'P600'.

SELECTION-SCREEN END OF BLOCK FILE.

*---------------------------------------------------
* INITIALIZATION.
*---------------------------------------------------

* TOP-OF-PAGE.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM TITLE_WRITE.


*----------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------
START-OF-SELECTION.

  Perform read_data.
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  DATA : LW_THEAD LIKE THEAD.

*SELECT * INTO CORRESPONDING FIELDS OF TABLE it_material
*FROM   ( mara AS H INNER JOIN mard AS I
*ON     H~matnr     EQ  I~matnr             )
*WHERE  H~matnr     IN  S_matnr
*and    H~mtart     like mtart
*and    I~LGORT     like LGORT.
*.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_material
  FROM   ( mara AS H INNER JOIN mard AS I
  ON     H~matnr     EQ  I~matnr             )
  inner join mbew as k
  on i~matnr = k~matnr
  and i~werks = k~bwkey
  inner join marc as m
  on i~matnr = m~matnr
  and i~werks = m~werks

  inner join makt as J
  on H~matnr eq J~matnr
  WHERE  H~matnr     IN  S_matnr
  and    H~mtart     in mtart
*and    J~maktx     in maktx
  and    J~maktg     in maktg
  and    I~LGORT     in LGORT
  and    k~verpr     in verpr
  and    i~labst     in labst
  and    i~werks      = werks.


*
*loop at it_material.
*refresh it_tline.
*   clear it_tline.
*   clear lw_thead.
*PERFORM READ_LONGTEXT  TABLES  IT_TLINE
*                            USING   LW_THEAD.
*
*
*endloop.
  LOOP AT IT_MATERIAL.
    clear ztline.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
       CLIENT                        = SY-MANDT
        ID                            = 'BEST'
        LANGUAGE                      = sy-langu
        NAME                          = IT_MATERIAL-MATNR
        OBJECT                        = 'MATERIAL'
*   ARCHIVE_HANDLE                = 0
*   LOCAL_CAT                     = ' '
* IMPORTING
*   HEADER                        = ZTHEAD
      TABLES
        LINES                         = ZTLINE
     EXCEPTIONS
       ID                            = 1
       LANGUAGE                      = 2
       NAME                          = 3
       NOT_FOUND                     = 4
       OBJECT                        = 5
       REFERENCE_CHECK               = 6
       WRONG_ACCESS_TO_ARCHIVE       = 7
       OTHERS                        = 8
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    data: total_value like mbew-salk3.
    clear: total_value.
    if IT_MATERIAL-LABST <> 0.
      if it_material-stprs <> 0.
        total_value = it_material-stprs * IT_MATERIAL-LABST.
      elseif it_material-verpr <> 0.
        total_value = it_material-verpr * IT_MATERIAL-LABST.
      endif.
    endif.

    WRITE:/1 IT_MATERIAL-MATNR,41 it_material-maktx,
    82 IT_MATERIAL-MFRNR.


    write:/1 IT_MATERIAL-LGORT, 15 IT_MATERIAL-MTART,30 total_value,65
    IT_MATERIAL-MFRPN.

    write:/1 IT_MATERIAL-LGPBE,25 it_material-verpr,
    45 IT_MATERIAL-LABST ,65 it_material-stprs .
    LOOP AT ZTLINE.
      WRITE:/ ZTLINE-TDLINE.

    ENDLOOP.
    Write:/ sy-uline.
  ENDLOOP.

*LOOP AT IT_MATERIAL.
*WRITE:/ IT_MATERIAL-MATNR, IT_MATERIAL-LGORT, IT_MATERIAL-MTART.
*endloop.
*LOOP AT ZTLINE.
*WRITE:/ ZTLINE-TDLINE.
*ENDLOOP.

ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  READ_LONGTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*      -->P_LW_THEAD  text
*----------------------------------------------------------------------*
FORM READ_LONGTEXT TABLES   P_TLINE STRUCTURE TLINE
                              "Insert correct name for <...>
                   USING     P_THEAD    STRUCTURE THEAD.

*CALL FUNCTION 'READ_TEXT'
*       EXPORTING
*            CLIENT                  = SY-MANDT
*            ID                      = P_THEAD-TDID
*            LANGUAGE                = SY-LANGU
*            NAME                    = P_THEAD-TDNAME
*            OBJECT                  = P_THEAD-TDOBJECT
*            ARCHIVE_HANDLE          = 0
*            LOCAL_CAT               = ' '
**       IMPORTING
**            HEADER                  =
*       TABLES
*            LINES                   = P_TLINE
*       EXCEPTIONS
*            ID                      = 1
*            LANGUAGE                = 2
*            NAME                    = 3
*            NOT_FOUND               = 4
*            OBJECT                  = 5
*            REFERENCE_CHECK         = 6
*            WRONG_ACCESS_TO_ARCHIVE = 7
*            OTHERS                  = 8.
*
*  IF SY-SUBRC <> 0.
**    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.                    " READ_LONGTEXT
*&---------------------------------------------------------------------*
*&      Form  TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TITLE_WRITE.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE: /55 'Material Details'
             COLOR COL_HEADING INTENSIFIED OFF.

  WRITE: / 'Date: ' ,
            SY-DATUM .
  SKIP 2.
*WRITE: /1 'Material', 31 'Mat Type', 40 'Material Description',
*  81 'Storage Loc',
*  105 'Manufacturer Part No', 126 'Manufacturer'.
**137 'Storage Bin'.
*Write:/1 'Standard Price', 20 'Moving avg Price',44 'Total Value',
* 60 'Storage Bin', 90 'Stock Quantity'.
*Write: /1 'Material P O Text'.
*
  WRITE:/1 'Material',41 'Material Description',
  82 'Manufacturer'.


  write:/1 'Storage Loc', 15 'Material Type',30 'Total Value',65
  'Manufacturer Part No'.

  write:/1 'Storage Bin',25 'Moving avg Price',
  45 'Stock Quantity' ,65 'Standard Price' .

  Write: /1 'Material P O Text'.

  Write:/ sy-uline.
*  WRITE: / '   '  COLOR COL_HEADING INTENSIFIED OFF,
*           ': Material ' ,
*           '   '  COLOR COL_NORMAL  INTENSIFIED OFF,
*           ': Short Text ' ,
*           '   '  COLOR COL_NORMAL  INTENSIFIED ON,
*           ': Storage Loc  ',
*           '   '  COLOR COL_TOTAL   INTENSIFIED OFF,
*           ': Material Type  ',
*           '   '  COLOR COL_POSITIVE INTENSIFIED OFF.


ENDFORM.                    " TITLE_WRITE
