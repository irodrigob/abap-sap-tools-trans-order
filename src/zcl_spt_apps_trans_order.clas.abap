CLASS zcl_spt_apps_trans_order DEFINITION
  PUBLIC
  INHERITING FROM zcl_spt_apps_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_spt_core_app~get_app_type REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_spt_apps_trans_order IMPLEMENTATION.
  METHOD zif_spt_core_app~get_app_type.
    ev_app = 'TRANS_ORDER'.
    ev_app_desc = 'Transport order'(t01).
  ENDMETHOD.

ENDCLASS.
