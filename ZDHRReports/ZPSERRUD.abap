*---------- DATA STRUCTURES FOR ERROR LOGGING (RPSERRUS) -------------*
* INCLUDE REPORT RPSERRUD. DATA STRUCTURES FOR ERROR LOGGING.         *
***********************************************************************
* 2.2A                                                                *
* VQKK081859 25.5.94 New release (CSZ)                                *
***********************************************************************
*   corrections/modifications:                                        *
*                                                                     *
***********************************************************************

DATA: BEGIN OF ERROR OCCURS 10,        "Table for error log.
         TEXT1(35),                    "Error text
         PARM1(12),                    "Parameter 1 to clarify
         PARM2(12),                    "Do.
         PARM3(12),                    "Do.
         PARM4(12),                    "Do.
      END OF ERROR.

***************************** END OF FILE *****************************
