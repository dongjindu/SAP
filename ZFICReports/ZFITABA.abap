REPORT ZTABA .
* fix depreciation run error
* for 2004 costing simulation, cost area(20) was enabled for posting
* it cause additional document posting, but it was deleted from batch
tables: taba.

update taba
  set BLNRT = '6100000546'
      XBUKZ = ' '
  where BUKRS  = 'H201'
    and AFBLGJ = '2004'
    and AFBLPE = '010'
    and AFBANZ = '01'.
