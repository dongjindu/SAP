***INCLUDE ZBDCRECX

*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*       Nodata-Character
DATA:   NODATA_CHARACTER VALUE '/'.

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP
     USING P_GROUP    LIKE APQI-GROUPID
           P_USER     LIKE APQI-USERID
           P_KEEP     LIKE APQI-QERASE
           P_HOLDDATE LIKE APQI-STARTDATE
           P_CTU      LIKE APQI-PUTACTIVE.

  IF P_CTU <> 'X'.
    CALL FUNCTION 'BDC_OPEN_GROUP'
         EXPORTING  CLIENT   = SY-MANDT
                    GROUP    = P_GROUP
                    USER     = P_USER
                    KEEP     = P_KEEP
                    HOLDDATE = P_HOLDDATE.
   ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*----------------------------------------------------------------------*
FORM CLOSE_GROUP USING P_CTU LIKE APQI-PUTACTIVE.
  IF P_CTU <> 'X'.
* close batchinput group
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION TABLES P_MESSTAB
                     USING  P_TCODE
                            P_CTU
                            P_MODE
                            P_UPDATE.
DATA: L_SUBRC LIKE SY-SUBRC.

  IF P_CTU <> 'X'.
    CALL FUNCTION 'BDC_INSERT'
         EXPORTING  TCODE     = P_TCODE
         TABLES     DYNPROTAB = BDCDATA
         EXCEPTIONS OTHERS    = 1.
  ELSE.
    CALL TRANSACTION P_TCODE USING BDCDATA
                     MODE   P_MODE
                     UPDATE P_UPDATE
                     MESSAGES INTO P_MESSTAB.
  ENDIF.
  L_SUBRC = SY-SUBRC.
  REFRESH BDCDATA.
  SY-SUBRC = L_SUBRC.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  IF FVAL <> NODATA_CHARACTER.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_NODATA USING P_NODATA.
  NODATA_CHARACTER = P_NODATA.
ENDFORM.
