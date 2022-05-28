CLASS zcl_zsap_tools_trans_o_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zsap_tools_trans_o_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS getusersordersse_get_entityset REDEFINITION.
    METHODS getsystemstransp_get_entityset REDEFINITION.
    methods dotransportcopys_get_entityset REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_zsap_tools_trans_o_dpc_ext IMPLEMENTATION.


  METHOD getsystemstransp_get_entityset.
    CLEAR: et_entityset.

    TRY.

        DATA(lv_langu) = sy-langu.
        ASSIGN it_filter_select_options[ property = 'langu' ]-select_options[ 1 ] TO FIELD-SYMBOL(<ls_select_options>).
        IF sy-subrc = 0.
          CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
            EXPORTING
              input            = <ls_select_options>-low
            IMPORTING
              output           = lv_langu
            EXCEPTIONS
              unknown_language = 1
              OTHERS           = 2.
          IF sy-subrc NE 0.
            lv_langu = sy-langu.
          ENDIF.
        ENDIF.

        DATA(lo_order) = NEW zcl_spt_apps_trans_order( iv_langu = lv_langu ).

        et_entityset = CORRESPONDING #( lo_order->get_systems_transport(  ) ).

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD getusersordersse_get_entityset.

    CLEAR: et_entityset.

    DATA(lo_order) = NEW zcl_spt_apps_trans_order(  ).

    DATA(lv_user) = sy-uname.
    ASSIGN it_filter_select_options[ property = 'orderUser' ]-select_options[ 1 ] TO FIELD-SYMBOL(<ls_select_options>).
    IF sy-subrc = 0.
      lv_user = <ls_select_options>-low.
    ENDIF.

    lo_order->get_user_orders(
      EXPORTING
        iv_username          = lv_user
*      iv_type_workbench    = ABAP_TRUE    " Ordenes workbench
*      iv_type_customizing  = ABAP_TRUE    " Ordenes customizing
*      iv_type_transport    = ABAP_TRUE    " Transporte de copias
*      iv_status_modif      = ABAP_TRUE    " Status modificable
*      iv_status_release    = ABAP_FALSE    " Status liberadas
*      iv_release_from_data =     " Ordenes liberadas desde
*      iv_release_from_to   =     " Ordenes liberadas desde
    IMPORTING
      et_orders            = DATA(lt_orders) ).

    et_entityset = CORRESPONDING #( lt_orders ).

  ENDMETHOD.

  METHOD dotransportcopys_get_entityset.

  ENDMETHOD.

ENDCLASS.
