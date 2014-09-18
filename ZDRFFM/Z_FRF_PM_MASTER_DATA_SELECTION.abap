FUNCTION Z_FRF_PM_MASTER_DATA_SELECTION .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_T024I STRUCTURE  ZSRF_T024I
*"      T_T003P STRUCTURE  ZSRF_T003P
*"      T_T001W STRUCTURE  ZSRF_T001W
*"      T_T357 STRUCTURE  ZSRF_T357
*"      T_T499S STRUCTURE  ZSRF_T499S
*"      T_T001L STRUCTURE  ZSRF_T001L
*"      T_EQUI STRUCTURE  ZSRF_V_EQUI
*"      T_KTEXT STRUCTURE  ZSRF_KTEXT
*"      T_MARA STRUCTURE  ZSRF_MARA
*"      T_RECIP STRUCTURE  ZSRF_RECIPIENT
*"----------------------------------------------------------------------
* TABLES
  TABLES: T024I,  "Maintenance planner groups
          T003P,  "Order Type
          T001W,  "Plants/Branches
          T357,   "Plant Section
          T499S,  "Location
          T001L,  "Storage Locations
          V_EQUI, "PM technical objects (EQUI, funcational location)
          ZTPM_KTEXT, "Order description
          MARA,  "General Material Data
          MAKT.  "Material Descriptions
* DATA
  DATA: L_TABIX LIKE SY-TABIX.
* RANGES
  RANGES: R_EQTYP FOR V_EQUI-EQTYP,
          R_MTART FOR MARA-MTART,
          R_WERKS FOR T001W-WERKS.
* Plant
  R_WERKS-LOW    = 'P001'.  "PLANT
  R_WERKS-SIGN   = 'I'.
  R_WERKS-OPTION = 'EQ'.
  APPEND R_WERKS.
  R_WERKS-LOW    = 'E001'.  "PLANT
  R_WERKS-SIGN   = 'I'.
  R_WERKS-OPTION = 'EQ'.
  APPEND R_WERKS.
* Equipment type
  R_EQTYP-LOW    = 'M'.  "Production equipment
  R_EQTYP-SIGN   = 'I'.
  R_EQTYP-OPTION = 'EQ'.
  APPEND R_EQTYP.

  R_EQTYP-LOW    = 'U'.  "Utility equipment
  R_EQTYP-SIGN   = 'I'.
  R_EQTYP-OPTION = 'EQ'.
  APPEND R_EQTYP.
* Material type
  R_MTART-LOW    = 'ERSA'.  "Utility equipment
  R_MTART-SIGN   = 'I'.
  R_MTART-OPTION = 'EQ'.
  APPEND R_MTART.
* Group
  SELECT IWERK
         INGRP
         INNAM
       FROM T024I
       INTO TABLE T_T024I
       WHERE IWERK IN R_WERKS.
  IF SY-SUBRC EQ 0.
    SORT T_T024I BY IWERK INGRP.
  ELSE.
    ZRESULT = TEXT-M04.
  ENDIF.
* Order type
  SELECT AUART
         TXT
       FROM T003P
       INTO TABLE T_T003P
       WHERE SPRAS EQ SY-LANGU.
  IF SY-SUBRC EQ 0.
    SORT T_T003P BY AUART.
  ELSE.
    ZRESULT = TEXT-M04.
  ENDIF.
* Plant
  SELECT WERKS
         NAME1
       FROM T001W
       INTO TABLE T_T001W
       WHERE WERKS IN R_WERKS.
  IF SY-SUBRC EQ 0.
    SORT T_T001W BY WERKS.
  ELSE.
    ZRESULT = TEXT-M04.
  ENDIF.
* Plant section
  SELECT WERKS
         BEBER
         FING
       FROM T357
       INTO TABLE T_T357
       WHERE WERKS IN R_WERKS.
  IF SY-SUBRC EQ 0.
    SORT T_T357 BY WERKS BEBER FING.
  ELSE.
    ZRESULT = TEXT-M04.
  ENDIF.
* Location
  SELECT WERKS
         STAND
         KTEXT
       FROM T499S
       INTO TABLE T_T499S
       WHERE WERKS IN R_WERKS.
  IF SY-SUBRC EQ 0.
    SORT T_T499S BY KTEXT WERKS STAND.
  ELSE.
    ZRESULT = TEXT-M04.
  ENDIF.
* Storage Locations
  SELECT WERKS
         LGORT
         LGOBE
       FROM T001L
       INTO TABLE T_T001L
       WHERE WERKS IN R_WERKS.
  IF SY-SUBRC EQ 0.
    SORT T_T001L BY WERKS LGORT.
    LOOP AT T_T001L.
      L_TABIX = SY-TABIX.
      CASE T_T001L-WERKS.
        WHEN 'P001'.
          IF T_T001L-LGORT BETWEEN 'P600' AND 'P699'.
          ELSE.
            DELETE T_T001L INDEX L_TABIX.
          ENDIF.
        WHEN 'E001'.
          IF T_T001L-LGORT BETWEEN 'E600' AND 'E699'.
          ELSE.
            DELETE T_T001L INDEX L_TABIX.
          ENDIF.
        WHEN OTHERS.
          DELETE T_T001L INDEX L_TABIX.
      ENDCASE.
    ENDLOOP.
  ELSE.
    ZRESULT = TEXT-M04.
  ENDIF.
* Equipment master data

* SELECT A~EQUNR
*         DATBI
*         SWERK
*         BEBER
*         STORT
*         EQKTX
*       from equi as e
*       inner join EQUZ AS A
*       ON e~EQUNR = a~EQUNR
*       INNER JOIN ILOA AS B
*       on A~ILOAN = B~ILOAN
*       INNER JOIN EQKT AS C
*       ON A~EQUNR = C~EQUNR
*       INTO TABLE T_EQUI
*       WHERE EQTYP = 'M'.

  SELECT EQUNR
         DATBI
         SWERK
         BEBER
         STORT
         EQKTX
*       FROM V_EQUI
       FROM ZVPM_EQUI
       INTO TABLE T_EQUI
*       where EQTYP IN R_EQTYP
        where SWERK IN R_WERKS.
*       and datbi >= sy-datum.

*       WHERE STORT IN R_STORT   "Location
*       AND   BEBER IN R_BEBER.  " Plant section
  IF SY-SUBRC EQ 0.
    LOOP AT T_EQUI.
      L_TABIX = SY-TABIX.
*      IF T_EQUI-datbi < sy-datum.
*        DELETE T_EQUI INDEX L_TABIX.
*        CONTINUE.
*      ENDIF.

      IF T_EQUI-SWERK IS INITIAL.
        DELETE T_EQUI INDEX L_TABIX.
        CONTINUE.
      ENDIF.
      IF T_EQUI-BEBER IS INITIAL.
        DELETE T_EQUI INDEX L_TABIX.
        CONTINUE.
      ENDIF.
      IF T_EQUI-STORT IS INITIAL.
        DELETE T_EQUI INDEX L_TABIX.
        CONTINUE.
      ENDIF.
      MODIFY T_EQUI INDEX L_TABIX TRANSPORTING SWERK BEBER STORT.
      CLEAR T_EQUI.
    ENDLOOP.
    SORT T_EQUI BY EQUNR DATBI SWERK BEBER STORT.
  ELSE.
    ZRESULT = TEXT-M04.
  ENDIF.
* Order description
  SELECT KTEXT
       FROM ZTPM_KTEXT
       INTO TABLE T_KTEXT.
  IF SY-SUBRC EQ 0.

  ELSE.
    ZRESULT = TEXT-M04.
  ENDIF.
* Material master & Description
  SELECT A~MATNR
         A~MTART
         A~MEINS
         B~MAKTX
       FROM MARA AS A INNER JOIN MAKT AS B
                      ON  A~MATNR EQ B~MATNR
                      AND B~SPRAS EQ SY-LANGU
       INTO TABLE T_MARA
       WHERE A~MTART IN R_MTART
         AND A~LVORM = ' '.
  IF SY-SUBRC EQ 0.
    SORT T_MARA BY MATNR.
  ELSE.
    ZRESULT = TEXT-M04.
  ENDIF.

  IF ZRESULT EQ TEXT-M04.
    E_MESS = TEXT-M08. "'Master data Middlewore Update Error'.

  ELSE.
    ZRESULT = TEXT-M03.
    E_MESS = TEXT-M09.  "'Master data Middlewore Update Success'.
  ENDIF.

  SELECT PERNR
         WEMPF
       FROM ZTRF_RECIPIENT
       INTO TABLE T_RECIP.

  IF SY-SUBRC EQ 0.
    SORT T_RECIP BY PERNR WEMPF.
    E_MESS = TEXT-M09. "'Master data Middlewore Update success'.
    ZRESULT = TEXT-M03.
  ELSE.
    ZRESULT = TEXT-M04.
    E_MESS = TEXT-M08. "'Master data Middlewore Update Error'.
  ENDIF.
ENDFUNCTION.
