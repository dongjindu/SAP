*----------------------------------------------------------------------*
*   INCLUDE ZXFORU25                                                   *
*----------------------------------------------------------------------*
* REQUESTED BY ANDY CHOI, CHANGED BY CHRIS
* REQUEST NO:  UD1K914655
* DATE:        02/24/2005

* CHANGE THE ID FROM VENDOR NUMBER TO SSN
* Assign vendor number to ACH ID field
* e_dtamusccd-ccd7 = I_REGUH-lifnr.

 SELECT SINGLE PERID INTO e_dtamusccd-ccd7
   FROM PA0002
   WHERE PERNR  = I_REGUH-LIFNR.
 IF SY-SUBRC NE 0.
   e_dtamusccd-ccd7 = I_REGUH-lifnr.
 ENDIF.

 e_dtamusccd-CCD8 = I_REGUH-ZNME1.
