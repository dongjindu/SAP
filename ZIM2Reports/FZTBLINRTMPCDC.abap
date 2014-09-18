FORM CD_CALL_ZTBLINRTMP                    .
   IF   ( UPD_ZTBLINR_TMP                    NE SPACE )
     OR ( UPD_ICDTXT_ZTBLINRTMP      NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTBLINRTMP_WRITE_DOCUMENT     ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTBLINR_TMP
                      = *ZTBLINR_TMP
                  N_ZTBLINR_TMP
                      = ZTBLINR_TMP
                  UPD_ZTBLINR_TMP
                      = UPD_ZTBLINR_TMP
                  UPD_ICDTXT_ZTBLINRTMP
                      = UPD_ICDTXT_ZTBLINRTMP
          TABLES  ICDTXT_ZTBLINRTMP
                      = ICDTXT_ZTBLINRTMP
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
