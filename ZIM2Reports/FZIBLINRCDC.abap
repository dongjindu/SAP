FORM CD_CALL_ZIBLINR                       .
   IF   ( UPD_ZTBLINR                        NE SPACE )
     OR ( UPD_ICDTXT_ZIBLINR         NE SPACE )
   .
     CALL FUNCTION 'ZIBLINR_WRITE_DOCUMENT        ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTBLINR
                      = *ZTBLINR
                  N_ZTBLINR
                      = ZTBLINR
                  UPD_ZTBLINR
                      = UPD_ZTBLINR
                  UPD_ICDTXT_ZIBLINR
                      = UPD_ICDTXT_ZIBLINR
          TABLES  ICDTXT_ZIBLINR
                      = ICDTXT_ZIBLINR
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
