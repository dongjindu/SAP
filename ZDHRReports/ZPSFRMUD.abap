*---------- DATA STRUCTURES FOR FORM ROUTINES (RPSFRM0S) -------------*
* INCLUDE REPORT RPSFRM0D. DATA STRUCTURES FOR FORM ROUTINES.         *
***********************************************************************
*   2.2a                                                              *
*   cszk080141                                                        *
**** DEBUG
*   correction: k11k080141
**** DEBUG
***********************************************************************
*   corrections/modifications:                                        *
*                                                                     *
*                                                                     *
*                                                                     *
*                                                                     *
*                                                                     *
***********************************************************************

TABLES: T512P,                         "Form background
        T512Q.                         "Form variables

DATA: BEGIN OF PAGEX OCCURS 100,       "Will contain the finished
         TEXT1 LIKE T512P-LINDA,       "Page
END OF PAGEX.

DATA: BEGIN OF I512Q OCCURS 200.       "Internal t512q with 1 form
        INCLUDE STRUCTURE T512Q.
DATA: END OF I512Q.
*---------- DATA STRUCTURES FOR FORM ROUTINES (RPSFRM0S) -------------*
