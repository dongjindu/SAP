FORM CD_CALL_ZTIMIMG08                     .
   IF   ( UPD_ZTIMIMG08                      NE SPACE )
     OR ( UPD_ICDTXT_ZTIMIMG08       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIMIMG08_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIMIMG08
                      = *ZTIMIMG08
                  N_ZTIMIMG08
                      = ZTIMIMG08
                  UPD_ZTIMIMG08
                      = UPD_ZTIMIMG08
                  UPD_ICDTXT_ZTIMIMG08
                      = UPD_ICDTXT_ZTIMIMG08
          TABLES  ICDTXT_ZTIMIMG08
                      = ICDTXT_ZTIMIMG08
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
