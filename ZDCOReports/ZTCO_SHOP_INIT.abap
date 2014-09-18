REPORT ZTCO_SHOP_INIT .
check sy-uname = '101679'.
tables: ztco_shop_sum, ztco_shop_cc.
delete from ztco_shop_sum where kokrs = 'H201'.
delete from ztco_shop_cc where kokrs = 'H201'.
