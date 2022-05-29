INTERFACE zif_spt_trans_order_data
  PUBLIC .
  TYPES: tt_orders TYPE STANDARD TABLE OF trkorr WITH DEFAULT KEY.


  CONSTANTS: BEGIN OF orders_type,
               transport_copies TYPE trfunction VALUE 'T',
               workbench        TYPE trfunction VALUE 'K',
               customizing      TYPE trfunction VALUE 'W',
             END OF orders_type.
  CONSTANTS: BEGIN OF cs_message,
               id TYPE symsgid VALUE 'ZSPT_TRANS_ORDER',
             END OF cs_message.
ENDINTERFACE.
