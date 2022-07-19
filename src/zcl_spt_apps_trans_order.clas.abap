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
             task_type        TYPE trfunction,
             task_type_desc   TYPE val_text,
           END OF ts_user_orders.
    TYPES: tt_user_orders TYPE STANDARD TABLE OF ts_user_orders WITH EMPTY KEY.
    TYPES: BEGIN OF ts_systems_transport,
             system_name TYPE sysname,
             system_desc TYPE string,
           END OF ts_systems_transport.
    TYPES: tt_systems_transport TYPE STANDARD TABLE OF ts_systems_transport WITH EMPTY KEY.
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
    "! <p class="shorttext synchronized">Sistemas de transporte</p>
    "! @parameter rt_systems | <p class="shorttext synchronized">Sistemas</p>
    METHODS get_systems_transport
      RETURNING VALUE(rt_systems) TYPE tt_systems_transport.
    "! <p class="shorttext synchronized">Hacer transporte de copia</p>
    "! @parameter it_orders | <p class="shorttext synchronized">Ordenes</p>
    "! @parameter iv_system | <p class="shorttext synchronized">Sistema</p>
    "! @parameter iv_description | <p class="shorttext synchronized">Descripción</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    "! @parameter ev_order | <p class="shorttext synchronized">Orden creada</p>
    METHODS do_transport_copy
      IMPORTING
        it_orders      TYPE zif_spt_trans_order_data=>tt_orders
        iv_system      TYPE sysname
        iv_description TYPE string
      EXPORTING
        et_return      TYPE zif_spt_core_data=>tt_return
        ev_order       TYPE trkorr.
  PROTECTED SECTION.

    DATA mt_orders_data TYPE zif_spt_trans_order_data=>tt_orders_data.
    DATA mo_handle_badi_transport_copy TYPE REF TO zspt_badi_transport_copy.

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
    "! <p class="shorttext synchronized">Creación de orden de transporte</p>
    "! @parameter iv_type | <p class="shorttext synchronized">Tipo de orden</p>
    "! @parameter iv_system | <p class="shorttext synchronized">Sistema</p>
    "! @parameter iv_description | <p class="shorttext synchronized">Descripción</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter es_return | <p class="shorttext synchronized">Retorno de la creación</p>
    "! @parameter ev_order | <p class="shorttext synchronized">Orden creada</p>
    METHODS create_order
      IMPORTING
        iv_type        TYPE trfunction
        iv_description TYPE string
        iv_system      TYPE sysname OPTIONAL
        iv_user        TYPE syuname DEFAULT sy-uname
      EXPORTING
        es_return      TYPE zif_spt_core_data=>ts_return
        ev_order       TYPE trkorr.
    "! <p class="shorttext synchronized">Copia el contenido de unas ordenes a otra orden</p>
    "! @parameter it_from_orders | <p class="shorttext synchronized">Ordenes origen</p>
    "! @parameter iv_to_order | <p class="shorttext synchronized">Orden destino</p>
    METHODS copy_content_orders_2_order
      IMPORTING
        it_from_orders TYPE zif_spt_trans_order_data=>tt_orders
        iv_to_order    TYPE trkorr
      EXPORTING
        et_return      TYPE zif_spt_core_data=>tt_return.
    "! <p class="shorttext synchronized">Verifica si hay objetos inactivos en las ordenes</p>
    "! @parameter it_orders | <p class="shorttext synchronized">Ordenes origen</p>
    "! @parameter et_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS check_inactive_objects
      IMPORTING
        it_orders TYPE zif_spt_trans_order_data=>tt_orders
      EXPORTING
        et_return TYPE zif_spt_core_data=>tt_return.
    "! <p class="shorttext synchronized">Lectura de datos de una orden</p>
    "! @parameter iv_order | <p class="shorttext synchronized">Orden</p>
    "! @parameter rs_data | <p class="shorttext synchronized">Datos del orden</p>
    METHODS read_request
      IMPORTING
                iv_order       TYPE trkorr
      RETURNING
                VALUE(rs_data) TYPE trwbo_request
      RAISING   zcx_spt_trans_order.
    "! <p class="shorttext synchronized">Liberación de una orden/tarea</p>
    "! @parameter iv_order | <p class="shorttext synchronized">Orden/tarea</p>
    "! @parameter iv_without_locking | <p class="shorttext synchronized">Sin bloqueo de objetos</p>
    "! @parameter rs_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS release_order
      IMPORTING
                iv_without_locking TYPE sap_bool DEFAULT abap_false
                iv_order           TYPE trkorr
      RETURNING VALUE(rs_return)   TYPE zif_spt_core_data=>ts_return.
    "! <p class="shorttext synchronized">Lectura de los datos de las ordenes</p>
    "! @parameter it_orders | <p class="shorttext synchronized">Ordenes</p>
    "! @parameter rt_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS get_orders_info
      IMPORTING
                it_orders        TYPE zif_spt_trans_order_data=>tt_orders
      RETURNING VALUE(rt_return) TYPE zif_spt_core_data=>tt_return.


  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Instancia la BADI de transporte de copias</p>
    METHODS instance_badi_transport_copy.
    "! <p class="shorttext synchronized">Llama al método antes de liberar </p>
    "! @parameter iv_order | <p class="shorttext synchronized">Orden</p>
    "! @parameter cs_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS call_badi_before_release_order
      IMPORTING
        iv_order  TYPE trkorr
      CHANGING
        cs_return TYPE zif_spt_core_data=>ts_return.
ENDCLASS.



CLASS zcl_spt_apps_trans_order IMPLEMENTATION.


  METHOD call_badi_before_release_order.

    " Si la BADI esta instancia leo los datos de la orden y se la paso al método
    IF mo_handle_badi_transport_copy IS BOUND.

      TRY.
          DATA(ls_data) = read_request( iv_order ).

        CATCH zcx_spt_trans_order INTO DATA(lo_excep).
      ENDTRY.

      LOOP AT mo_handle_badi_transport_copy->imps ASSIGNING FIELD-SYMBOL(<ls_imps>).
        TRY.
            <ls_imps>->before_release_order( EXPORTING
                  iv_order      = iv_order
                  is_order_data = ls_data
                CHANGING
                  cs_return     = cs_return ).

            " Si se devuelve algun error se para la llamada a las BADI
            IF cs_return-type = zif_spt_core_data=>cs_message-type_error.
              EXIT.
            ENDIF.
          CATCH cx_root.
        ENDTRY.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD check_inactive_objects.
    DATA lt_log TYPE STANDARD TABLE OF sprot_u.


    " Solo procesan las ordenes/tareas que tengan objetos
    LOOP AT it_orders ASSIGNING FIELD-SYMBOL(<ls_orders>).

      TRY.

          " Si la tengo en la tabla global uso sus valores, en caso contrario los busco.
          READ TABLE mt_orders_data INTO DATA(ls_order_data) WITH KEY h-trkorr = <ls_orders>.
          IF sy-subrc NE 0.
            ls_order_data = read_request( <ls_orders> ).
          ENDIF.

          DATA(ls_e070) = CORRESPONDING e070( ls_order_data-h ).

          CALL FUNCTION 'TRINT_CHECK_INACTIVE_OBJECTS'
            EXPORTING
              is_e070 = ls_e070
              it_e071 = ls_order_data-objects
            TABLES
              et_log  = lt_log[].

          " Los mensajes de tipo A y E son errores y se devuelven
          LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<ls_log>) WHERE ( severity = zif_spt_core_data=>cs_message-type_error
                                                                  OR severity = zif_spt_core_data=>cs_message-type_anormal ).
            INSERT zcl_spt_utilities=>fill_return( iv_type = <ls_log>-severity
                                                   iv_id = <ls_log>-ag
                                                   iv_number = <ls_log>-msgnr
                                                   iv_message_v1 = <ls_log>-var1
                                                   iv_message_v2 = <ls_log>-var2
                                                   iv_message_v3 = <ls_log>-var3
                                                   iv_message_v4 = <ls_log>-var4
                                                   iv_langu      = <ls_log>-langu ) INTO TABLE et_return.
          ENDLOOP.

        CATCH zcx_spt_trans_order INTO DATA(lo_excep).
          INSERT zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_error
                                                 iv_id = lo_excep->if_t100_message~t100key-msgid
                                                 iv_number = lo_excep->if_t100_message~t100key-msgno
                                                 iv_message_v1 = lo_excep->mv_msgv1
                                                 iv_langu      = mv_langu ) INTO TABLE et_return.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD copy_content_orders_2_order.
    CLEAR: et_return.
    LOOP AT it_from_orders ASSIGNING FIELD-SYMBOL(<ls_orders>).
      CALL FUNCTION 'TR_COPY_COMM'
        EXPORTING
          wi_dialog                = space
          wi_trkorr_from           = <ls_orders>
          wi_trkorr_to             = iv_to_order
          wi_without_documentation = abap_true
        EXCEPTIONS
          OTHERS                   = 1.
      IF sy-subrc NE 0.
        INSERT VALUE #( type = zif_spt_core_data=>cs_message-type_error
                        message = zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_error
                                                                  iv_id = sy-msgid
                                                                  iv_number = sy-msgno
                                                                  iv_message_v1 = sy-msgv1
                                                                  iv_message_v2 = sy-msgv2
                                                                  iv_message_v3 = sy-msgv3
                                                                  iv_message_v4 = sy-msgv4
                                                                  iv_langu      = mv_langu )-message ) INTO TABLE et_return.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_order.

    CLEAR: es_return.

    DATA(lv_order_text) = CONV e07t-as4text( iv_description ).

    CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
      EXPORTING
        wi_kurztext   = lv_order_text
        wi_trfunction = iv_type
        iv_username   = sy-uname
        iv_tarsystem  = iv_system
        wi_client     = sy-mandt
      IMPORTING
        we_trkorr     = ev_order
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc = 0.
      es_return = VALUE #( type = zif_spt_core_data=>cs_message-type_success
                                 message = zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_success
                                                                           iv_id = zif_spt_trans_order_data=>cs_message-id
                                                                           iv_number = '002'
                                                                           iv_message_v1 = ev_order
                                                                           iv_langu      = mv_langu )-message ).
    ELSE.

      es_return = VALUE #( type = zif_spt_core_data=>cs_message-type_error
                           message = zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_error
                                                          iv_id = sy-msgid
                                                          iv_number = sy-msgno
                                                          iv_message_v1 = sy-msgv1
                                                          iv_message_v2 = sy-msgv2
                                                          iv_message_v3 = sy-msgv3
                                                          iv_message_v4 = sy-msgv4
                                                          iv_langu      = mv_langu )-message  ) .


    ENDIF.

  ENDMETHOD.


  METHOD do_transport_copy.

    IF it_orders IS NOT INITIAL.

      instance_badi_transport_copy( ).

      " Lectura del contenido de las ordenes
      et_return = get_orders_info( it_orders ).

      IF et_return IS INITIAL.

        " Se chequean que no haya ningun objeto inactivo de las ordenes pasadas. Ya que si hay alguno no se podrá
        " liberar la orden y se quedaría colgada.
        check_inactive_objects( EXPORTING it_orders = it_orders
                                IMPORTING et_return = et_return ).

        " Si no hay errores se continua el proceso.
        IF et_return IS INITIAL.

          " Se crea la orden donde se pondrán los objetos
          create_order( EXPORTING iv_type = zif_spt_trans_order_data=>orders_type-transport_copies
                                  iv_description = iv_description
                                  iv_system = iv_system
                        IMPORTING es_return = DATA(ls_return_created)
                                  ev_order = ev_order ).

          IF ls_return_created-type NE zif_spt_core_data=>cs_message-type_error.
            " Se pasa el contenido de las ordenes pasadas a la nueva orden
            copy_content_orders_2_order( EXPORTING it_from_orders = it_orders
                                                     iv_to_order = ev_order
                                         IMPORTING et_return = et_return ).
            IF et_return IS INITIAL.
              " Se libera la orden
              DATA(ls_return_release) = release_order( EXPORTING iv_without_locking = abap_true " Evitamos el error de objetos de bloqueo por transporte de copias
                                                                 iv_order = ev_order ).
              " Si hay un error añado el mensaje de la orden creada para que se sepa cual ha sido
              " la orden creada y se puede tratar para arreglar el error
              IF ls_return_release-type = zif_spt_core_data=>cs_message-type_error.
                INSERT ls_return_created INTO TABLE et_return.
                INSERT ls_return_release INTO TABLE et_return.
              ELSE.
                " Si no hay errores pongo el mensaje genérico de transporte de copias realizado.
                INSERT VALUE #( type = zif_spt_core_data=>cs_message-type_success
                                message = zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_success
                                                                        iv_id = zif_spt_trans_order_data=>cs_message-id
                                                                        iv_number = '004'
                                                                        iv_message_v1 = ev_order
                                                                        iv_langu      = mv_langu )-message ) INTO TABLE et_return.
              ENDIF.

            ENDIF.
          ELSE.
            INSERT ls_return_created INTO TABLE et_return.
          ENDIF.

        ENDIF.

      ELSE.
        INSERT VALUE #( type = zif_spt_core_data=>cs_message-type_error
                        message = zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_success
                                                                          iv_id = zif_spt_trans_order_data=>cs_message-id
                                                                          iv_number = '003'
                                                                          iv_langu      = mv_langu )-message ) INTO TABLE et_return.
      ENDIF.

    ENDIF.

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


  METHOD get_orders_info.

    CLEAR: mt_orders_data, rt_return.

    LOOP AT it_orders ASSIGNING FIELD-SYMBOL(<ls_orders>).

      TRY.
          DATA(ls_data) = read_request( <ls_orders> ).

          INSERT ls_data INTO TABLE mt_orders_data.

        CATCH zcx_spt_trans_order INTO DATA(lo_excep).
          INSERT zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_error
                                                 iv_id = lo_excep->if_t100_message~t100key-msgid
                                                 iv_number = lo_excep->if_t100_message~t100key-msgno
                                                 iv_message_v1 = lo_excep->mv_msgv1
                                                 iv_langu      = mv_langu ) INTO TABLE rt_return.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_systems_transport.
    DATA lv_version TYPE tcevers-version.

    CLEAR rt_systems.

* Version activa del sistema de transporte
    CALL FUNCTION 'TR_GET_CONFIG_VERSION'
      IMPORTING
        ev_active_version       = lv_version
      EXCEPTIONS
        no_active_version_found = 1.

    IF sy-subrc = 0.
      SELECT sysname AS system_name ddtext AS system_desc INTO TABLE rt_systems
              FROM  tcesystt
               WHERE version = lv_version
               AND   spras  = mv_langu.
      IF sy-subrc NE 0.
        " Si no hay en el idioma global busco en el de logon
        SELECT sysname AS system_name ddtext AS system_desc INTO TABLE rt_systems
                      FROM  tcesystt
                       WHERE version = lv_version
                       AND   spras  = sy-langu.
        IF sy-subrc NE 0.
          " Si no hay busco directamente el codig, y la descripcion será el mismo codigo
          SELECT sysname AS system_name sysname AS system_desc INTO TABLE rt_systems
                        FROM  tcesyst
                         WHERE version = lv_version.
        ENDIF.
      ENDIF.

    ENDIF.

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


    SORT lt_request BY trfunction as4date DESCENDING as4time DESCENDING.

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
        <ls_orders>-task_type = <ls_tasks>-trfunction.
        <ls_orders>-task_status = SWITCH #( <ls_tasks>-trstatus
                                             WHEN sctsc_state_protected OR sctsc_state_changeable THEN sctsc_state_changeable
                                             WHEN sctsc_state_released OR sctsc_state_export_started THEN sctsc_state_released ).
        READ TABLE lt_status_txt ASSIGNING <ls_dd07v> WITH KEY domvalue_l = <ls_orders>-task_status.
        IF sy-subrc = 0.
          <ls_orders>-task_status_desc = <ls_dd07v>-ddtext.
        ENDIF.

        READ TABLE lt_functions_txt ASSIGNING <ls_dd07v> WITH KEY domvalue_l = <ls_tasks>-trfunction.
        IF sy-subrc = 0.
          <ls_orders>-task_type_desc = <ls_dd07v>-ddtext.
        ENDIF.
      ENDLOOP.
      IF sy-subrc NE 0.
        INSERT ls_orders INTO TABLE et_orders.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD instance_badi_transport_copy.

    TRY.
        GET BADI mo_handle_badi_transport_copy.
      CATCH cx_root.
    ENDTRY.
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


  METHOD read_request.

    rs_data-h-trkorr = iv_order.

    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_read_e070       = 'X'
        iv_read_e07t       = 'X'
        iv_read_e070c      = 'X'
        iv_read_e070m      = 'X'
        iv_read_objs_keys  = 'X'
        iv_read_attributes = 'X'
      CHANGING
        cs_request         = rs_data
      EXCEPTIONS
        OTHERS             = 1.
    IF sy-subrc NE 0.
      DATA(lv_message) = zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_error
                                                                 iv_id = sy-msgid
                                                                 iv_number = sy-msgno
                                                                 iv_message_v1 = sy-msgv1
                                                                 iv_message_v2 = sy-msgv2
                                                                 iv_message_v3 = sy-msgv3
                                                                 iv_message_v4 = sy-msgv4
                                                                 iv_langu      = mv_langu )-message.

      RAISE EXCEPTION TYPE zcx_spt_trans_order
        EXPORTING
          textid   = zcx_spt_trans_order=>message_other_class
          mv_msgv1 = lv_message.
    ENDIF.

  ENDMETHOD.


  METHOD release_order.

    CLEAR: rs_return.


    call_badi_before_release_order( EXPORTING iv_order = iv_order
                                    CHANGING cs_return = rs_return ).

    IF rs_return IS INITIAL.
      CALL FUNCTION 'TRINT_RELEASE_REQUEST'
        EXPORTING
          iv_trkorr                   = iv_order
          iv_dialog                   = abap_false
          iv_as_background_job        = abap_false
          iv_success_message          = abap_false
          iv_without_objects_check    = abap_false
          iv_without_locking          = iv_without_locking " Evitamos el error de objetos de bloqueo por transporte de copias
          iv_display_export_log       = abap_false
        EXCEPTIONS
          cts_initialization_failure  = 1
          enqueue_failed              = 2
          no_authorization            = 3
          invalid_request             = 4
          request_already_released    = 5
          repeat_too_early            = 6
          object_lock_error           = 7
          object_check_error          = 8
          docu_missing                = 9
          db_access_error             = 10
          action_aborted_by_user      = 11
          export_failed               = 12
          execute_objects_check       = 13
          release_in_bg_mode          = 14
          release_in_bg_mode_w_objchk = 15
          error_in_export_methods     = 16
          object_lang_error           = 17.
      IF sy-subrc = 0.
        rs_return = VALUE #( type = zif_spt_core_data=>cs_message-type_success
                              message = zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_success
                                                                        iv_id = zif_spt_trans_order_data=>cs_message-id
                                                                        iv_number = '001'
                                                                        iv_message_v1 = iv_order
                                                                        iv_langu      = mv_langu )-message ).
      ELSE.
        DATA(lv_msgno) = sy-msgno.
        DATA(lv_msgid) = sy-msgid.
        DATA(lv_msgv1) = sy-msgv1.
        DATA(lv_msgv2) = sy-msgv2.
        DATA(lv_msgv3) = sy-msgv3.
        DATA(lv_msgv4) = sy-msgv4.

        rs_return = VALUE #( type = zif_spt_core_data=>cs_message-type_error
                             message = zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_error
                                                                       iv_id = sy-msgid
                                                                       iv_number = sy-msgno
                                                                       iv_message_v1 = sy-msgv1
                                                                       iv_message_v2 = sy-msgv2
                                                                       iv_message_v3 = sy-msgv3
                                                                       iv_message_v4 = sy-msgv4
                                                                       iv_langu      = mv_langu )-message ).
        " Para los mensajes estándar si no hay texto mensaje y el idioma global difiere al idioma
        " de conexión entonces saco el mensae en el idioma de logon.
        IF rs_return-message IS INITIAL AND mv_langu NE sy-langu.
          rs_return = VALUE #( type = zif_spt_core_data=>cs_message-type_error
                              message = zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_error
                                                                        iv_id = lv_msgid
                                                                        iv_number = lv_msgno
                                                                        iv_message_v1 = lv_msgv1
                                                                        iv_message_v2 = lv_msgv2
                                                                        iv_message_v3 = lv_msgv3
                                                                        iv_message_v4 = lv_msgv4
                                                                        iv_langu      = sy-langu )-message ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_spt_core_app~get_app_type.
    CLEAR: es_app.

    es_app-app = 'TRANS_ORDER'.
    es_app-app_desc = 'Transport order'(t01).
    es_app-service = '/ZSAP_TOOLS_TRANS_ORDER_SRV'.
    es_app-frontend_page = '/trans_order'.
  ENDMETHOD.
ENDCLASS.
