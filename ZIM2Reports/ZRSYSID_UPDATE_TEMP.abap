*&---------------------------------------------------------------------*
*& report  ZRSYSID_UPDATE                                              *
*& writer : 강석봉                                                     *
*& date   : 2000/01/03                                                 *
*& title  : original system to be changed                              *
*& 소속   : infolink ltd.                                              *
*&---------------------------------------------------------------------*
REPORT  ZRSYSID_UPDATE MESSAGE-ID ZIM.



TABLES : TADIR.              " R/3 저장소오브젝트의 디렉토?

  SELECT *
    FROM TADIR
   WHERE PGMID  EQ 'R3TR'
     AND OBJECT EQ 'NROB'.
  ENDSELECT.
  IF SY-SUBRC EQ 0.
    DELETE FROM TADIR
          WHERE PGMID  EQ 'R3TR'
            AND OBJECT EQ 'NROB'.
  ENDIF.
