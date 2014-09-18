*&---------------------------------------------------------------------*
*& Report  YAPP_TEMP01                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  yapp_temp01                   .
TABLES : ztpp_bfst,plaf.
DATA : it_bfst LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE.
DATA : w_int TYPE i.
DATA: l_plnum             LIKE bapi_pldord-pldord_num,
      l_header            LIKE bapiplaf_i2 ,
      l_headerx           LIKE bapiplaf_i2x,
      l_return            LIKE bapireturn1 ,
      con_fix TYPE c VALUE 'X'.
DATA: snd_jobs TYPE i VALUE 1,
      rcv_jobs TYPE i VALUE 1,
      width TYPE i,
      excp_flag(1) TYPE c,
      taskname(4) TYPE n VALUE '0001',
      err_chk,
       z_num(2) TYPE n,
      p_plant LIKE marc-werks VALUE 'P001'.

PARAMETERS : p_pg(20) TYPE c DEFAULT 'PG_BF'.

DATA : it_log LIKE  l_return  OCCURS 0 WITH HEADER LINE.
CLEAR : it_bfst[],it_log[].

START-OF-SELECTION.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bfst
         FROM ztpp_bfst.
*          where PLAN_ORD = '0000065537'.

*  DESCRIBE TABLE it_bfst LINES w_int.
*  IF w_int <> 0.
*    LOOP AT it_bfst.
*      CLEAR : l_plnum,l_header,l_headerx.
*
*      l_plnum = it_bfst-plan_ord.
*      l_header-bom_exp_fix_ind = con_fix.
*      l_headerx-bom_exp_fix_ind = con_fix.
*
*      CALL FUNCTION 'BAPI_PLANNEDORDER_CHANGE'
*           EXPORTING
*                plannedorder = l_plnum
*                headerdata   = l_header
*                headerdatax  = l_headerx
*           IMPORTING
*                return       = l_return.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
*      MOVE l_return-message TO it_log-message.
*      APPEND it_log.
*    ENDLOOP.
*  ENDIF.
  LOOP AT it_bfst.
    SELECT SINGLE * FROM plaf
     WHERE plnum EQ it_bfst-plan_ord
      AND auffx EQ 'X'
      AND stlfx EQ 'X'.
    IF sy-subrc = 0.
      DELETE TABLE it_bfst FROM it_bfst.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_bfst LINES w_int.
  IF w_int <> 0.

    DO.
      READ TABLE it_bfst INDEX 1.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.


      CLEAR : l_plnum,l_header,l_headerx.

      l_plnum = it_bfst-plan_ord.


      CALL FUNCTION 'Z_FPP_BAPI_PLAN_ORDER_CHANGE'
      STARTING NEW TASK taskname DESTINATION IN GROUP p_pg
        PERFORMING return_01 ON END OF TASK
            EXPORTING
                l_plnum = l_plnum
           EXCEPTIONS
                communication_failure       = 1
                system_failure              = 2
                RESOURCE_FAILURE            = 3 .


      CASE sy-subrc.
        WHEN 0.
          taskname = taskname + 1.
          snd_jobs = snd_jobs  + 1.
          DELETE it_bfst INDEX 1.
        WHEN 1 OR 2.
          excp_flag = 'X'.
          EXIT.
        WHEN 3.
*Receive reply to asynchronous RFC calls
          IF excp_flag = space.
            excp_flag = 'X'.
*First attempt for RESOURCE_Failure handling
            WAIT UNTIL rcv_jobs >= snd_jobs UP TO '0.01' SECONDS.
          ELSE.
*Second attempt for RESOURCE_Failure handling
            WAIT UNTIL rcv_jobs >= snd_jobs UP TO '0.1' SECONDS.
          ENDIF.
          IF sy-subrc = 0.
            CLEAR excp_flag. " Reset flag
*          ELSE.
*            EXIT.
          ENDIF.
      ENDCASE.
      CLEAR : it_bfst.
    ENDDO.
    WAIT UNTIL rcv_jobs >= snd_jobs.
    IF sy-subrc = 0.
      COMMIT WORK.
      WRITE : / 'SEND NUM :', snd_jobs,'REV.NUM:',rcv_jobs,
                'Total num:' , w_int .
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDIF.

END-OF-SELECTION.

*  LOOP AT it_log.
*    WRITE : / it_log-message.
*  ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  return_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM return_01 USING taskname.

  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_BAPI_PLAN_ORDER_CHANGE'
           EXCEPTIONS
                communication_failure       = 1
                system_failure              = 2
                RESOURCE_FAILURE            = 3 .

  CHECK sy-subrc = 0.
  rcv_jobs  = rcv_jobs + 1.

ENDFORM.                                                    " return_01
