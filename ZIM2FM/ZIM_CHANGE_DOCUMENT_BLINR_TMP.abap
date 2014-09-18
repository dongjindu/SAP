FUNCTION ZIM_CHANGE_DOCUMENT_BLINR_TMP.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTBLINR_TMP) LIKE  ZTBLINR_TMP STRUCTURE
*"        ZTBLINR_TMP
*"     REFERENCE(O_ZTBLINR_TMP) LIKE  ZTBLINR_TMP STRUCTURE
*"        ZTBLINR_TMP
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTBLINR_TMP+3(10).

   CALL FUNCTION 'ZTBLINRTMP_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBLINR_TMP      =    N_ZTBLINR_TMP
            O_ZTBLINR_TMP      =    O_ZTBLINR_TMP
            UPD_ZTBLINR_TMP    =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''

      tables
            ICDTXT_ZTBLINRTMP     =    it_CdtXT.

ENDFUNCTION.
