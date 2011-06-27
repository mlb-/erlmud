%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILDP(I, P), {I, {I, start_link, P}, permanent, 5000, worker, [I]}).

