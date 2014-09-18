************************************************************************
REPORT  YZTPP_KSBOHMM_COPY   LINE-SIZE  700 MESSAGE-ID zmpp.

TABLES: ZTPP_KSBOHMM.

DATA: it_ksbohmm like table of ZTPP_KSBOHMM with header line,
      l_wo_ser(9) .
parameters: p_char(1) default '9'.

*START-OF-SELECTION.
  select * into table it_ksbohmm
  from ztpp_ksbohmm.

loop at it_ksbohmm.
  concatenate it_ksbohmm-wo_ser+0(6) p_char it_ksbohmm-wo_ser+7(2) into
  l_wo_Ser.
  it_ksbohmm-wo_Ser = l_wo_Ser.
  modify it_ksbohmm.
  write: / l_wo_Ser.
endloop.

insert ztpp_ksbohmm from table it_ksbohmm ACCEPTING DUPLICATE KEYS.
commit work.
