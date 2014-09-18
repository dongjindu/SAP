*---------- SUBROUTINES FOR ERROR LOGGING IN REPORTS -----------------*
* INCLUDE REPORT RPSERRUS. ERROR LOGGING ROUTINES                     *
***********************************************************************
* 2.2A                                                                *
* VQLK081859  25.5.94 New release (CSZ)                               *
*---------------------------------------------------------------------*
*  REQUIRES RPSERRUD TO BE INCLUDED BEFORE THIS REPORT!!!             *
*---------------------------------------------------------------------*
***********************************************************************
*   corrections/modifications:                                        *
*                                                                     *
*                                                                     *
***********************************************************************
*---------------------------------------------------------------------*
*       FORM ERR_LOG                                                  *
*---------------------------------------------------------------------*
*       APPENDS TEXT AND PARAMETERS TO THE ERROR LOG                  *
*---------------------------------------------------------------------*
*  -->  ERRTX  = TEXT                                                 *
*  -->  ERRP1  = PARAMETER 1                                          *
*  -->  ERRP2  = PARAMETER 2                                          *
*  -->  ERRP3  = PARAMETER 3                                          *
*  -->  ERRP4  = PARAMETER 4                                          *
*---------------------------------------------------------------------*

FORM ERR_LOG USING ERRTX ERRP1 ERRP2 ERRP3 ERRP4.
  MOVE: ERRTX TO ERROR-TEXT1,          "Fill the fields
        ERRP1 TO ERROR-PARM1,
        ERRP2 TO ERROR-PARM2,
        ERRP3 TO ERROR-PARM3,
        ERRP4 TO ERROR-PARM4.
  APPEND ERROR.                        "Add to the error table
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PRINT_LOG                                                *
*---------------------------------------------------------------------*
*       PRINTS OUT THE ERROR LOG AT THE END OF THE STATISTIC          *
*---------------------------------------------------------------------*

FORM PRINT_LOG.

  DATA: ERRCT TYPE P.                  "Error count

  NEW-PAGE.
  WRITE: / 'ERROR LOG:'(ZER).          "Error log
  ULINE.
  SKIP.

  LOOP AT ERROR.
    ADD 1 TO ERRCT.
    CONDENSE ERROR.
    WRITE: / ERROR.
  ENDLOOP.

  SKIP.
  WRITE: / 'Number of errors found: '(ZEN),
           ERRCT.                      "Error count
ENDFORM.                               "PRINT_LOG

***************************** END OF FILE *****************************
