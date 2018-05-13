-record(decimal, {
                   sign = 1 :: -1 | 1,
                   coef :: pos_integer(),
                   exp = 0 :: pos_integer()
                  }).

-record(prefix, {
                 code,
                 name,
                 value
                }).

-record(base_unit, {
                    code,
                    name,
                    property,
                    dim
                   }).

-record(defined_unit, {
                       code,
                       name,
                       property,
                       is_metric :: boolean(),
                       is_arbitrary :: boolean(),
                       is_special :: boolean(),
                       class :: binary(),
                       unit,
                       value
                      }).

-record(symbol, {
                 prefix = undefined,
                 unit,
                 exp = 1
                 }).

-record(canonical_unit, {
                         base,
                         exp
                        }).

-record(canonical, {
                    value = {0, 1, 0},
                    units
                    }).
