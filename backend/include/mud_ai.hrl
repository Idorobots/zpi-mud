%% NOTE Licensed under the MIT license. See included LICENSE file for details.

%% AI related definitions.

-author('kajtek@idorobots.org').

%% The AI state:
-record(state, {
          nick    = <<"">>,     %% Nick of the character.
          target  = <<"">>,     %% Current target.
          state   = [],         %% Temporary character state.
          handler = generic_npc %% Currently used AI script.
         }).
