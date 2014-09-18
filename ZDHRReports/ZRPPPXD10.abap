*----------------------------------------------------------------------*
* 4.0A
* YKMP40K011038 17.09.1997 NO_AUTHORITY_CHECK_CLUSTER for deactivation
*                          of the authority check.
* YKMALRK000847 25.06.1997 PAYR_BUFFER maintained (decoupling)
* 3.0E
* QICP30K074825  17.05.1996 returncode
* 3.0D
* YKMP30K048502  21.02.1996 Table PAYR_BUFFER added for interface to
*                           FI's check register.
* 3.0B
* XPMP30K014785  21.09.1995  occurs-Parameter veraendert
*
*----------------------------------------------------------------------*
*  R/3 Common-part pcl1(2)-buffer
*----------------------------------------------------------------------*

*data: begin of buffer_dir occurs 10,                       "XPMK014785
DATA: BEGIN OF BUFFER_DIR OCCURS 2000,                      "XPMK014785
        SGART(2),
        CLIENT LIKE PCL1-CLIENT,
        RELID LIKE PCL1-RELID,
        SRTFD LIKE PCL1-SRTFD,
        NTABX LIKE SY-TABIX, "pointer auf aktuellen satz
        OTABX LIKE SY-TABIX, "pointer auf alten satz (falls vorhanden)
        NNUXT LIKE PCL1-SRTF2, "anzahl folgesaetze aktueller Satz
        ONUXT LIKE PCL1-SRTF2, "anzahl folgesaetze alter Satz
*       ofset(3) type p,     "offset innerhalb eines entry
      END OF BUFFER_DIR.
DATA: BEGIN OF DIR_KEY,
        SGART(2),
        CLIENT LIKE PCL1-CLIENT,
        RELID LIKE PCL1-RELID,
        SRTFD LIKE PCL1-SRTFD,
      END OF DIR_KEY.
DATA: BEGIN OF DEL_PCLX_TAB OCCURS 2,
        SGART(2),
        RELID LIKE PCL1-RELID,
        SRTFD LIKE PCL1-SRTFD,
        NUEXT LIKE PCL1-SRTF2,
      END OF DEL_PCLX_TAB.
*data: begin of tbuff occurs 5.                             "XPMK014785
DATA: BEGIN OF TBUFF OCCURS 5000.                           "XPMK014785
        INCLUDE STRUCTURE PCL1.
        DATA: SGART(2),
      END OF TBUFF.
*data: begin of before_image_pclx occurs 550.               "XPMK014785
DATA: BEGIN OF BEFORE_IMAGE_PCLX OCCURS 5000.               "XPMK014785
        INCLUDE STRUCTURE PCL1.
        DATA: SGART(2),
      END OF BEFORE_IMAGE_PCLX.
* Check register of FI.
DATA: BEGIN OF PAYR_BUFFER OCCURS 5.                      "YKMP30K048502
*       INCLUDE STRUCTURE payr.                           "YKMP30K048502
*       INCLUDE STRUCTURE PAYR_HR.               " thk Decoupling 5.5.97
        INCLUDE STRUCTURE PAYR_FI.                        "YKMALRK000847
DATA:   OSEQNR LIKE PAYR_FI-SEQNR.                        "YKMALRK000847
DATA: END OF PAYR_BUFFER.                                 "YKMP30K048502

DATA: PCLX_SUBRC LIKE SY-SUBRC.                           "QICP30K074826

DATA: NO_AUTHORITY_CHECK_CLUSTER(1) TYPE C.               "YKMP40K011038
