CLASS zcl_spt_apps_trans_order DEFINITION
  PUBLIC
  INHERITING FROM zcl_spt_apps_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_user_orders,
             order            TYPE trkorr,
             order_desc       TYPE string,
             order_user       TYPE uname,
             order_type       TYPE trfunction,
             order_type_desc  TYPE val_text,
             task             TYPE trkorr,
             task_user        TYPE uname,
             task_status      TYPE trstatus,
             task_status_desc TYPE val_text,
           END OF ts_user_orders.
    TYPES: tt_user_orders TYPE STANDARD TABLE OF ts_user_orders WITH EMPTY KEY.
    METHODS zif_spt_core_app~get_app_type REDEFINITION.
    "! <p class="shorttext synchronized">Devuelve las ordenes de un usuario </p>
    "! @parameter iv_username | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_type_workbench | <p class="shorttext synchronized">Ordenes workbench</p>
    "! @parameter iv_type_customizing | <p class="shorttext synchronized">Ordenes customizing</p>
    "! @parameter iv_type_transport | <p class="shorttext synchronized">Transporte de copias</p>
    "! @parameter iv_status_modif | <p class="shorttext synchronized">Status modificable</p>
    "! @parameter iv_status_release | <p class="shorttext synchronized">Status liberadas</p>
    "! @parameter iv_release_from_data | <p class="shorttext synchronized">Ordenes liberadas desde</p>
    "! @parameter iv_release_from_to | <p class="shorttext synchronized">Ordenes liberadas desde</p>
    "! @parameter et_orders | <p class="shorttext synchronized">Ordenes</p>
    METHODS get_user_orders
      IMPORTING
        !iv_username          TYPE syuname DEFAULT sy-uname
        !iv_type_workbench    TYPE sap_bool DEFAULT abap_true
        !iv_type_customizing  TYPE sap_bool DEFAULT abap_true
        !iv_type_transport    TYPE sap_bool DEFAULT abap_true
        !iv_status_modif      TYPE sap_bool DEFAULT abap_true
        !iv_status_release    TYPE sap_bool DEFAULT abap_false
        !iv_release_from_data TYPE sy-datum OPTIONAL
        !iv_release_from_to   TYPE sy-datum OPTIONAL
      EXPORTING
        et_orders             TYPE tt_user_orders.
  PROTECTED SECTION.
    "! <p class="shorttext synchronized">Parámetros de selección</p>
    "! @parameter iv_username | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_type_workbench | <p class="shorttext synchronized">Ordenes workbench</p>
    "! @parameter iv_type_customizing | <p class="shorttext synchronized">Ordenes customizing</p>
    "! @parameter iv_type_transport | <p class="shorttext synchronized">Transporte de copias</p>
    "! @parameter iv_status_modif | <p class="shorttext synchronized">Status modificable</p>
    "! @parameter iv_status_release | <p class="shorttext synchronized">Status liberadas</p>
    "! @parameter iv_release_from_data | <p class="shorttext synchronized">Ordenes liberadas desde</p>
    "! @parameter iv_release_from_to | <p class="shorttext synchronized">Ordenes liberadas desde</p>
    "! @parameter et_orders | <p class="shorttext synchronized">Ordenes</p>
    METHODS fill_selections_orders
      IMPORTING
        iv_type_workbench    TYPE sap_bool DEFAULT abap_true
        iv_type_customizing  TYPE sap_bool DEFAULT abap_true
        iv_type_transport    TYPE sap_bool DEFAULT abap_true
        iv_status_modif      TYPE sap_bool DEFAULT abap_true
        iv_status_release    TYPE sap_bool DEFAULT abap_false
        iv_release_from_data TYPE sy-datum OPTIONAL
        iv_release_from_to   TYPE sy-datum OPTIONAL
      RETURNING
        VALUE(rs_selections) TYPE trwbo_selection.
    "! <p class="shorttext synchronized">Lectura de textos de dominio</p>
    "! @parameter iv_username | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_type_workbench | <p class="shorttext synchronized">Ordenes workbench</p>
    METHODS load_domain_texts
      EXPORTING
        et_functions TYPE dd07v_tab
        et_status    TYPE dd07v_tab.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_spt_apps_trans_order IMPLEMENTATION.
  METHOD zif_spt_core_app~get_app_type.
    CLEAR: es_app.

    es_app-app = 'TRANS_ORDER'.
    es_app-app_desc = 'Transport order'(t01).
    es_app-service = '/ZSAP_TOOLS_TRANS_ORDER_SRV'.
    es_app-frontend_page = '/trans_order'.
  ENDMETHOD.

  METHOD get_user_orders.
    DATA lt_request TYPE trwbo_request_headers.

    " relleno de los datos de seleccion
    DATA(ls_selection) = fill_selections_orders( EXPORTING iv_type_workbench = iv_type_workbench
                                                     iv_type_customizing = iv_type_customizing
                                                     iv_type_transport = iv_type_transport
                                                     iv_status_modif = iv_status_modif
                                                     iv_status_release = iv_status_release
                                                     iv_release_from_data = iv_release_from_data
                                                     iv_release_from_to = iv_release_from_to ).

    " Carga de los textos
    load_domain_texts( IMPORTING et_functions = DATA(lt_functions_txt)
                                 et_status    = DATA(lt_status_txt) ).

    " Lectura de las ordenes
    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern  = iv_username
        is_selection         = ls_selection
        iv_complete_projects = 'X'
      IMPORTING
        et_requests          = lt_request.


    SORT lt_request BY as4date DESCENDING as4time DESCENDING.

    " Sacamos las padres para ir obteniendo los hijos
    LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_request>)
                                 WHERE strkorr IS INITIAL.

      " Relleno los campos base de la orden
      DATA(ls_orders) = VALUE ts_user_orders( order = <ls_request>-trkorr
                                              order_user = <ls_request>-as4user
                                              order_desc = <ls_request>-as4text
                                              order_type = <ls_request>-trfunction ).
      READ TABLE lt_functions_txt ASSIGNING FIELD-SYMBOL(<ls_dd07v>) WITH KEY domvalue_l = <ls_request>-trfunction.
      IF sy-subrc = 0.
        ls_orders-order_type_desc = <ls_dd07v>-ddtext.
      ENDIF.

      " Ahora las tareas de la orden
      LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_tasks>) WHERE strkorr = <ls_request>-trkorr.
        INSERT ls_orders INTO TABLE et_orders ASSIGNING FIELD-SYMBOL(<ls_orders>).
        <ls_orders>-task = <ls_tasks>-trkorr.
        <ls_orders>-task_user = <ls_tasks>-as4user.
        <ls_orders>-task_status = SWITCH #( <ls_tasks>-trstatus
                                             WHEN sctsc_state_protected OR sctsc_state_changeable THEN sctsc_state_changeable
                                             WHEN sctsc_state_released OR sctsc_state_export_started THEN sctsc_state_released ).
        READ TABLE lt_status_txt ASSIGNING <ls_dd07v> WITH KEY domvalue_l = <ls_orders>-task_status.
        IF sy-subrc = 0.
          <ls_orders>-task_status_desc = <ls_dd07v>-ddtext.
        ENDIF.
      ENDLOOP.
      IF sy-subrc NE 0.
        INSERT ls_orders INTO TABLE et_orders.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_selections_orders.
    CLEAR rs_selections.

    IF iv_type_workbench = abap_true.
      rs_selections-reqfunctions(1)     = sctsc_type_workbench.
    ENDIF.
    IF iv_type_customizing = abap_true.
      rs_selections-reqfunctions+1(1)   = sctsc_type_customizing.
    ENDIF.
    IF iv_type_transport = abap_true.
      rs_selections-reqfunctions+2(1)   = sctsc_type_transport.
    ENDIF.

* Types of assigned tasks
    rs_selections-taskfunctions      = sctsc_types_tasks.

* Status para ordenes modificables
    IF iv_status_modif = abap_true.
      rs_selections-taskstatus(1)     = sctsc_state_protected.
      rs_selections-taskstatus+1(1)   = sctsc_state_changeable.
    ENDIF.

* Status para ordenes liberadas
    IF iv_status_release = abap_true.
      rs_selections-taskstatus+2(1)   = sctsc_state_released.
      rs_selections-taskstatus+3(1)   = sctsc_state_notconfirmed.
    ENDIF.

    IF iv_status_modif = abap_true AND iv_status_release = abap_false.
      rs_selections-reqstatus(1)   = sctsc_state_protected.
      rs_selections-reqstatus+1(1) = sctsc_state_changeable.
*    r_selections-reqstatus+2(1) = sctsc_state_export_started.
    ELSEIF iv_status_modif = abap_false AND iv_status_release = abap_true.
      rs_selections-reqstatus(1)   = sctsc_state_released.
      rs_selections-reqstatus+1(1) = sctsc_state_export_started.
    ELSEIF iv_status_modif = abap_true AND iv_status_release = abap_true.
      rs_selections-reqstatus      = sctsc_states_all.
    ENDIF.
  ENDMETHOD.


  METHOD load_domain_texts.
    DATA lt_functions_n TYPE dd07v_tab.
    DATA lt_status_n TYPE dd07v_tab.

    CLEAR: et_functions, et_status.

    " Textos de tipo de orden
    IF et_functions IS SUPPLIED.
      CALL FUNCTION 'DD_DOMA_GET'
        EXPORTING
          domain_name   = 'TRFUNCTION'
          langu         = mv_langu
        TABLES
          dd07v_tab_a   = et_functions
          dd07v_tab_n   = lt_functions_n
        EXCEPTIONS
          illegal_value = 1
          op_failure    = 2
          OTHERS        = 3.
    ENDIF.

    " Textos de status
    IF et_status IS SUPPLIED.
      CALL FUNCTION 'DD_DOMA_GET'
        EXPORTING
          domain_name   = 'TRSTATUS'
          langu         = mv_langu
        TABLES
          dd07v_tab_a   = et_status
          dd07v_tab_n   = lt_status_n
        EXCEPTIONS
          illegal_value = 1
          op_failure    = 2
          OTHERS        = 3.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
