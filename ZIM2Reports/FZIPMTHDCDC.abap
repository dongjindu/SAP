FORM CD_CALL_ZIPMTHD                       .
   IF   ( UPD_ZTPMTHD                        NE SPACE )
     OR ( UPD_ICDTXT_ZIPMTHD         NE SPACE )
   .
     CALL FUNCTION 'ZIPMTHD_WRITE_DOCUMENT        ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTPMTHD
                      = *ZTPMTHD
                  N_ZTPMTHD
                      = ZTPMTHD
                  UPD_ZTPMTHD
                      = UPD_ZTPMTHD
                  UPD_ICDTXT_ZIPMTHD
                      = UPD_ICDTXT_ZIPMTHD
          TABLES  ICDTXT_ZIPMTHD
                      = ICDTXT_ZIPMTHD
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
