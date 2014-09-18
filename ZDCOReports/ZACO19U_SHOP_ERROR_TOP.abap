*----------------------------------------------------------------------*
*   INCLUDE ZACO92A_CREAT_ROUTING_TOP                                  *
*----------------------------------------------------------------------*
tables : ztco_batch_log.


data : it_log like ztco_batch_log occurs 0 with header line.

data : begin of it_display occurs 0,
        icon_field like icon-id,
        matnr like ztco_batch_log-matnr,
        aufnr like ztco_batch_log-aufnr,
        times like ztco_batch_log-times,
        flag  like ztco_batch_log-flag,
        msg   like ztco_batch_log-msg,
        erdat like ztco_batch_log-erdat,
        erzet like ztco_batch_log-erzet,
        ernam like ztco_batch_log-ernam,
        chk(1),
       end of it_display .

data : begin of it_excu occurs 0,
       matnr like ztco_batch_log-matnr,
*       FLAG  like ztco_BATCH_LOG-flag,
*       CHK(1),
       end of it_excu .


data: begin of seltab occurs 0.       " internal table to transfer
        include structure rsparams.    " select-options and paramete
data: end of seltab.                  " between reports
