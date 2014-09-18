*&---------------------------------------------------------------------*
*& Report  ZPSD_SALES_ORDER                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZPSD_SALES_ORDER  NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                     LINE-SIZE 150.


*&&& Data Declaration.  &&&*
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
INCLUDE <ICON>.
*-------------------------------------------------*
* TABLE Declare                                   *
*-------------------------------------------------*

TABLES: AUSP,
        ZTPP_WOSUM.

*-------------------------------------------------*
* INTERNAL TABLE Declare            *
*-------------------------------------------------*



data: it_ztpp_wosum like ztpp_wosum occurs 0 with header line.

Data: begin of it_AUSP occurs 0,
     sales like ztpp_wosum-sales,
      objek like ausp-objek,
      atinn like ausp-atinn,
      atwrt like ausp-atwrt,
      klart like ausp-klart,
      modqty like ztpp_wosum-modqty,
      rp01t like ztpp_wosum-rp01tq,
      rp02t like ztpp_wosum-rp02tq,
      rp03t like ztpp_wosum-rp03tq,
      rp04t like ztpp_wosum-rp04tq,
      rp05t like ztpp_wosum-rp05tq,
      rp06t like ztpp_wosum-rp06tq,
      rp07t like ztpp_wosum-rp07tq,
      rp18t like ztpp_wosum-rp08tq,
      rp17t like ztpp_wosum-rp07tq.

Data: end of it_ausp.

data: wa_rp01(3) type i,
      wa_rp02(3) type i,
      wa_rp07(3) type i,
      wa_rp17(3) type i,
      wa_rp18(3) type i,
      wa_rp19(3) type i.
data : wa_ausp like table of it_ausp with header line,
       rcnt type i.
data:  it_ausp_tmp like it_ausp occurs 0 with header line.
data:  it_ausp_tmp2 like it_ausp occurs 0 with header line.
data:  it_ausp_tmp3 like it_ausp occurs 0 with header line.

***Declare Hash Table
TYPES: BEGIN OF STRUC,
sales like ztpp_wosum-sales,
      objek like ausp-objek,
      atinn like ausp-atinn,
      atwrt like ausp-atwrt,
      klart like ausp-klart,
      modqty like ztpp_wosum-modqty,
      rp01t like ztpp_wosum-rp01tq,
      rp02t like ztpp_wosum-rp02tq,
      rp03t like ztpp_wosum-rp03tq,
      rp04t like ztpp_wosum-rp04tq,
      rp05t like ztpp_wosum-rp05tq,
      rp06t like ztpp_wosum-rp06tq,
      rp17t like ztpp_wosum-rp07tq,
      rp18t like ztpp_wosum-rp08tq,
      rp19t like ztpp_wosum-rp09tq,
      END OF STRUC.
 data:      HTAB TYPE HASHED TABLE OF STRUC WITH UNIQUE KEY  objek.
data: it_ausp4 like line of htab.
* End Declaration Hash Table

data: begin of wa_temp occurs 0,
atwrt like ausp-atwrt,
MODQTY like ztpp_wosum-MODQTY,
RP08TQ like ztpp_wosum-RP08TQ,
end of wa_temp.
********************
FIELD-SYMBOLS: <wa_field> TYPE ANY.

*parameters: sales like  ztpp_wosum-sales.


*-------------------------------------------------*
* Start of selection           *
*-------------------------------------------------*

select *  from ztpp_wosum into  it_ztpp_wosum where
modqty ne ztpp_wosum-RP08TQ.
append it_ztpp_wosum.
endselect.

loop at it_ztpp_wosum.
move it_ztpp_wosum-sales to wa_temp-atwrt.
move it_ztpp_wosum-MODQTY to wa_temp-MODQTY.
move it_ztpp_wosum-RP08TQ to wa_temp-RP08TQ.
append wa_temp.

endloop.

delete wa_temp where atwrt eq ''.

if not wa_temp[] is initial.

select   * into corresponding fields of table
it_ausp from ausp
for all entries in wa_temp
where ATWRT = wa_temp-atwrt
and klart = '002'
*and atinn = 'P_SALES_ORDER'.
and atinn = '0000002742'.
endif.

sort it_ausp by objek.
if not it_ausp[] is initial.

loop at it_ausp.
    add 1 to rcnt.
    move-corresponding it_ausp to wa_ausp.
    append wa_ausp.
    insert  it_ausp into table HTAB.
    if rcnt >= 5000.
      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE  it_ausp_tmp
         from ausp
   for all entries in wa_ausp
  where objek = wa_ausp-objek
   and atinn = '0000003429'
  and atwrt  not in ('T','D').
      refresh wa_ausp. clear wa_ausp.
      clear rcnt.
    endif.
  endloop.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE  it_ausp_tmp
     from ausp
for all entries in wa_ausp
where objek = wa_ausp-objek
and atinn = '0000003429'
and atwrt not in ('T','D').

*  select   * into corresponding fields of table
*  it_ausp_tmp from ausp
*  for all entries in it_ausp
*  where objek = it_ausp-objek
*  and atinn = '0000003429'
*  and atwrt = 'P'.
endif.
refresh wa_ausp. clear :RCNT, wa_ausp.


if not it_ausp_tmp[] is initial.

loop at it_ausp_tmp.
    add 1 to rcnt.
     move-corresponding it_ausp_tmp to wa_ausp.
     append wa_ausp.
    if rcnt >= 5000.
      select * appending corresponding fields of table it_ausp_tmp2
      from ausp for all entries in wa_ausp
      where  objek = wa_ausp-objek
             and   atinn in  ('0000003373','0000003383',
              '0000003388',  '0000003404', '0000003405') .
       clear rcnt.
       refresh wa_ausp. clear wa_ausp.
    endif.
  endloop.

  select * appending corresponding fields of table it_ausp_tmp2
      from ausp for all entries in wa_ausp
      where  objek = wa_ausp-objek
             and   atinn in  ('0000003373','0000003383',
              '0000003388',  '0000003404', '0000003405') .
       clear rcnt.
       refresh wa_ausp. clear wa_ausp.

*  select * into corresponding fields of table it_ausp_tmp2
*  from ausp for all entries in it_ausp_tmp
*  where  objek = it_ausp_tmp-objek
*  and   atinn in  ('0000003373','0000003383','0000003388','0000003404',
*  '0000003414') .

endif.

*htab[] = it_ausp[].
*sort it_ausp_tmp2 by objek.
loop at it_ausp_tmp2.

*sort htab by objek.
*read table it_ausp_tmp2 with key objek = it_ausp-objek.
read table htab into it_ausp4 with table key objek = it_ausp_tmp2-objek.
if sy-subrc = 0.
move it_ausp4-atwrt to it_ausp_tmp2-sales.
modify it_ausp_tmp2.
endif.
*write:/ it_ausp_tmp2-objek, it_ausp_tmp2-atwrt, it_ausp_tmp2-atinn,
*it_ausp_tmp2-sales.
endloop.


sort it_ausp_tmp2 by sales atinn objek.

loop at it_ausp_tmp2.
if it_ausp_tmp2-atinn =  '0000003373' .
add 1 to wa_rp01.
elseif it_ausp_tmp2-atinn = '0000003383' .
 add 1 to wa_rp02.
elseif it_ausp_tmp2-atinn = '0000003388' .
 add 1 to wa_rp07.
elseif it_ausp_tmp2-atinn = '0000003404' .
 add 1 to wa_rp18.
elseif it_ausp_tmp2-atinn = '0000003405' .
 add 1 to wa_rp17.
endif.
at end of sales.
move it_ausp_tmp2-sales to it_ausp_tmp3-sales.
move wa_rp01 to it_ausp_tmp3-rp01t.
*move wa_rp02 to it_ausp_tmp3-rp02t.
*move wa_rp07 to it_ausp_tmp3-rp07t.
*move wa_rp18 to it_ausp_tmp3-rp18t.
*move wa_rp19 to it_ausp_tmp3-rp19t.
move wa_rp02 to it_ausp_tmp3-rp02t.
move wa_rp17 to it_ausp_tmp3-rp17t.
move wa_rp18 to it_ausp_tmp3-rp18t.


append it_ausp_tmp3.
clear : wa_rp01, wa_rp02, wa_rp07, wa_rp18, wa_rp17.

endat.


endloop.

write:/2 'Sales order', 31 'RP01TQ' , 41 'RP02TQ',  51 'RP07TQ',
 61 'RP08TQ'.
loop at it_ausp_tmp3.
write:/2 it_ausp_tmp3-sales,31 it_ausp_tmp3-rp01t,41 it_ausp_tmp3-rp02t,
51 it_ausp_tmp3-rp17t,61 it_ausp_tmp3-rp18t.
*
UPDATE ztpp_wosum SET: RP01TQ  = it_ausp_tmp3-rp01t
                    RP02TQ = it_ausp_tmp3-rp02t
                    RP07TQ = it_ausp_tmp3-rp17t
                    RP08TQ = it_ausp_tmp3-rp18t
              WHERE  sales = it_ausp_tmp3-sales.

endloop.


*UPDATE ztpp_wosum SET: RP01TQ  = it_ausp_tmp3-rp01t,
*                    RP02TQ = it_ausp_tmp3-rp02t
*                    RP07TQ = it_ausp_tmp3-rp17t
*                    RP08TQ = it_ausp_tmp3-rp18t
*              WHERE  sales = sales.
*


*it_ausp_tmp3[] = it_ausp_tmp2[].
*
*sort it_ausp_tmp2 by sales.
*loop at it_ausp_tmp2.
*delete adjacent duplicates from it_ausp_tmp2 comparing sales.
*endloop.
*
*sort it_ausp_tmp2 by sales.
*loop at it_ausp_tmp2.
*clear : wa_rp01, wa_rp02, wa_rp07, wa_rp18, wa_rp19.
*sort it_ausp_tmp3 by sales.
* loop at it_ausp_tmp3 where sales = it_ausp_tmp2-sales.
* if it_ausp_tmp3-atinn = '0000003373' .
* add 1 to wa_rp01.
* elseif it_ausp_tmp3-atinn = '0000003383' .
* add 1 to wa_rp02.
*elseif it_ausp_tmp3-atinn = '0000003388' .
* add 1 to wa_rp07.
*elseif it_ausp_tmp3-atinn = '0000003404' .
* add 1 to wa_rp18.
*elseif it_ausp_tmp3-atinn = '0000003414' .
* add 1 to wa_rp19.
* endif.
* endloop.
* write:/ wa_rp01, wa_rp02, wa_rp07, wa_rp18, wa_rp19.
** modify ztpp_wosum
*endloop.

*  perform download_file.

*&---------------------------------------------------------------------*
*&      Form  download_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM download_file .
**USING w_n_9
*CONCATENATE  '/usr/sap/EDI_SAP/'
*                                 'GS_' sy-datum+2(6)
*                                 'S.txt'
*                                 INTO w_dsn.
*
*                    PERFORM make_r1.
*                    LOOP AT it_downfile.
*                      OPEN DATASET w_dsn IN TEXT MODE FOR APPENDING.
*                      TRANSFER it_downfile TO w_dsn.
*                    ENDLOOP.
*                    PERFORM make_r6 USING w_n_9.
*
*                    CLOSE DATASET w_dsn.
*
*                    IF sy-subrc = 0.
*                      MESSAGE i000 WITH text-m02 '(GLOVIS)'.
*                    ELSE.
*                      MESSAGE i000 WITH text-m03 '(GLOVIS)'.
*                    ENDIF.
*
*ENDFORM.                    " download_file
*&---------------------------------------------------------------------*
*&      Form  make_r6
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_N_9  text
*----------------------------------------------------------------------*
*FORM make_r6 USING    W_N_9.
*CLEAR it_downfile.
*                    it_downfile-record+0(3)    = 'O1T'.
*                    it_downfile-record+3(9)    = w_n_9.
*                    it_downfile-record+12(169) = ' '.
*
*
*                    OPEN DATASET w_dsn IN TEXT MODE FOR APPENDING.
*                    TRANSFER it_downfile TO w_dsn.
*
*ENDFORM.                    " make_r6
