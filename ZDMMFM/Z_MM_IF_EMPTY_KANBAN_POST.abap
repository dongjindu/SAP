FUNCTION Z_MM_IF_EMPTY_KANBAN_POST .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_PRVBE) TYPE  PRVBE OPTIONAL
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SYSUBRC
*"     VALUE(E_MSG) TYPE  CHAR255
*"  TABLES
*"      PDA_TAB STRUCTURE  ZMMS_PDA006 OPTIONAL
*"----------------------------------------------------------------------
  CLEAR : G_PRVBE, G_SUBRC, G_MSG,
          G_TEMPB, G_TYPE.

  CLEAR : G_PKNUM, G_RKSTA, G_PKKEY, G_ZFEED,
          GV_LGORT, GV_UMLGO, GV_ABLAD.
  CLEAR : ST_PKHD.
  CLEAR : IT_KANBAN, IT_KANBAN[].

*.. Import parameter & Table Move
  MOVE : I_PRVBE   TO G_PRVBE,
         PDA_TAB[] TO IT_KANBAN[].

*.. Import Parameter Check
  PERFORM CHECK_PARAMETER_KANBAN.

*..
  IF G_TYPE EQ SPACE.
    PERFORM GET_CONTROL_CYCLE.

    IF G_PKNUM <> SPACE.
      CASE G_RKSTA.
        WHEN 'K'. "Manual Kanban

*Backflush cycle
*---------------------
* 1	JIS (by Vendor)
* 2	JIS (by Glovis)
* 3	S2L
* 4	M/Kanban
* 9	W/o reservation
*          IF G_TEMPB EQ '4'.  "Too strict... disable for HMMA

            PERFORM KANBAN_CHANGE_STATUS USING PDA_TAB.
*          ELSE.
*            MOVE : 'E' TO G_TYPE.
*            MOVE TEXT-M51 TO G_MSG. "This Box is NOT allowed to empty!
*          ENDIF.

        WHEN 'I'.  "Event Driven Kanban
          PERFORM CREATE_EVENT_DRIVEN USING PDA_TAB.
      ENDCASE.
    ENDIF.

  ENDIF.
  IF G_TYPE EQ 'E'.
    MOVE : 99    TO E_SUBRC,
           G_MSG TO E_MSG.
  ELSE.
    MOVE : 'S'   TO G_TYPE,
           G_MSG TO E_MSG.
  ENDIF.
*S__Paul 06/24/11 : update ZMMT0038
  IF G_TYPE = 'S'.

    READ TABLE PDA_TAB INDEX 1.

    LOOP AT IT_STATUSCHANGERESULT.

      UPDATE ZMMT0038
        SET: ZFEEDER  = PDA_TAB-ZFEEDER
             ABLAD    = PDA_TAB-ABLAD
       WHERE RSNUM    = IT_STATUSCHANGERESULT-RESERV_NO
         AND REVERSED = ''.

    ENDLOOP.

  ENDIF.
*E__<
** Changed by Furong on 07/25/2011 for send to GCS asap
  IF PDA_TAB-ZFEEDER = 'HOT' AND
     NOT IT_STATUSCHANGERESULT[] IS INITIAL.
    COMMIT WORK.
*    WAIT UP TO 1 SECONDS.
    PERFORM SEND_TO_GCS.
  ENDIF.
** End of change on 07/25/2011

  READ TABLE IT_KANBAN INDEX 1.
  PERFORM ERROR_LOG USING 'MMIF_PDA_12' 'PDA' 'US' 'I' ' ' 'S'
                          G_TYPE E_MSG
                          G_PRVBE IT_KANBAN-MATNR IT_KANBAN-MENGE_CH.

** Changed by Furong on 07/25/2011
** Changed by Furong on 07/18/2011
*  IF PDA_TAB-ZFEEDER = 'HOT' AND
*     NOT IT_STATUSCHANGERESULT[] IS INITIAL.
*    COMMIT WORK.
*    WAIT UP TO 1 SECONDS.
*    PERFORM SEND_TO_GCS.
*  ENDIF.
** End of change on 07/18/2011
** End on 07/25/2011
ENDFUNCTION.
