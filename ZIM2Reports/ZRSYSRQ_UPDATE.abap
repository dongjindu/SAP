*&---------------------------------------------------------------------*
*& Report  ZRSYSRQ_UPDATE                                              *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  ZRSYSRQ_UPDATE                .

TABLES : E070,        " WBO 및 전송: 요청/태스크 헤?
         E070A,       "
         E071,        " WBO 및 전송: 요청/태스크의 오브젝트 엔트?
         E071K,
         E070C,
         CTS_SERIAL,
         E07T.

DATA : BEGIN OF IT_TAB OCCURS 0.
       INCLUDE  STRUCTURE   E070.
DATA : END   OF IT_TAB.

DATA : BEGIN OF IT_E071 OCCURS 0.
       INCLUDE  STRUCTURE   E071.
DATA : END   OF IT_E071.


SELECT-OPTIONS : S_TRKORR    FOR     E070-TRKORR.    " LOCK MODE
PARAMETERS :     P_STATUS    LIKE    E070-TRSTATUS   " LOCK MODE
                 OBLIGATORY,
                 P_LOCK      TYPE    C,              " LOCK type
                 P_DEL       AS CHECKBOX.            " DELETE 여부.




INITIALIZATION.
   SET TITLEBAR 'ZIMRQ'.

TOP-OF-PAGE.
   SET TITLEBAR 'ZIMRQ'.
   SET PF-STATUS 'ZIMST'.

*>>>>>
START-OF-SELECTION.

   SELECT * INTO TABLE IT_TAB FROM E070
                              WHERE TRKORR IN S_TRKORR.

   SELECT * INTO TABLE IT_E071 FROM E071
                              WHERE TRKORR IN S_TRKORR.
   LOOP AT IT_TAB.
      WRITE :/ IT_TAB-TRKORR,
               IT_TAB-TRFUNCTION,
               IT_TAB-TRSTATUS,
               IT_TAB-TARSYSTEM,
               IT_TAB-KORRDEV,
               IT_TAB-AS4USER,
               IT_TAB-AS4DATE,
               IT_TAB-AS4TIME,
               IT_TAB-STRKORR.
      LOOP AT IT_E071 WHERE TRKORR = IT_TAB-TRKORR.
         WRITE :/4 IT_E071-OBJ_NAME, IT_E071-LOCKFLAG.
      ENDLOOP.
   ENDLOOP.

AT USER-COMMAND.
      IF SY-UCOMM EQ 'EXEC'.
*>>> HEADER
         LOOP AT IT_TAB.
            MOVE P_STATUS     TO    IT_TAB-TRSTATUS.
            MODIFY IT_TAB INDEX SY-TABIX.
            DELETE  FROM E070C
                         WHERE TRKORR EQ IT_TAB-TRKORR.
            DELETE FROM TLOCK WHERE TRKORR EQ IT_TAB-TRKORR..
         ENDLOOP.
*         UPDATE  E070  FROM TABLE   IT_TAB.
         DELETE  E070  FROM TABLE   IT_TAB.

*>>> ITEM
         LOOP AT IT_E071.
            MOVE P_LOCK     TO    IT_E071-LOCKFLAG.
            MODIFY IT_E071 INDEX SY-TABIX.
            DELETE FROM TLOCK WHERE TRKORR EQ IT_E071-TRKORR.
         ENDLOOP.
         UPDATE  E071  FROM TABLE   IT_E071.
         IF P_DEL EQ 'X'.
            DELETE  E071  FROM TABLE   IT_E071.
         ENDIF.

         LEAVE TO SCREEN 0.
      ENDIF.
