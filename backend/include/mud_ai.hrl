%% AI related stuff.

-record(state, {
          nick = <<"">>,
          target = <<"">>,
          state = [],
          handler = generic_npc
         }).
