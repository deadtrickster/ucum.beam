defmodule Ucum.Codegen do
  require Record

  @parser_template """
  -module(ucum_parser).

  -export([parse/1,
           symbol/1]).

  -include("model.hrl").

  -define(DECIMAL(Value), Value).

  %% ===================================================================
  %% API
  %% ===================================================================

  parse(String) ->
    {S, <<>>} = symbol(String),
    S.
  
  symbol(String) ->
    prefix(String, #symbol{}).

  %% ===================================================================
  %% Private functions
  %% ===================================================================

  prefix(String, S0) ->
     prefix_<%= [{m, _}|_] = prefixes
     m %>(String, S0).
  <%= for {pl, rprefixes} <- prefixes do %>
  <%= for prefix <- rprefixes do %>
  prefix_<%=pl%>(<<"<%= prefix[:attributes][:Code] %>", String/binary>>=String0, S0) ->
  try
    S = S0#symbol{prefix=#prefix{
      code= <<"<%= prefix[:attributes][:Code] %>">>,
      name= <<"<%= prefix[:name] %>">>,
      value= decimal_conv:number("<%= prefix[:value] %>")
    }},
    unit(String, S)
  of
    OK -> OK
  catch
    error:{unknown_unit, String} -> prefix_<%= pl - 1 %>(String0, S0)
  end;
  <% end %>
  prefix_<%=pl%>(String, S0) ->
    prefix_<%= pl - 1 %>(String, S0).
  <% end %>
  prefix_0(String, S0) ->
    unit(String, S0).

  <%= for unit <- units do %>
  unit(<<"<%= unit[:attributes][:Code] %>", String/binary>>, S0) ->
    S = S0#symbol{unit=#defined_unit{
      code= <<"<%= unit[:attributes][:Code] %>">>,
      name= <<"<%= unit[:name] %>">>,
      property= <<"<%= unit[:property] %>">>,
      is_metric= <%= if unit[:attributes][:isMetric] == "no",
                       do: false, else: true%>,
      is_arbitrary= <%= if Map.get(unit[:attributes], :isArbitrary, "no") == "no",
                       do: false, else: true%>,
      is_special= <%= if Map.get(unit[:attributes], :isSpecial, "no") == "no",
                       do: false, else: true%>,
      class= <<"<%= unit[:attributes][:class] %>">>,
      unit= <<"<%= unit[:value][:Unit] %>">>,
      value= <<"<%= unit[:value][:value] %>">>
    }},

    exponent(String, S);
  <% end %>
  <%= for unit <- base_units do %>
  unit(<<"<%= unit[:attributes][:Code] %>", String/binary>>, S0) ->
    S = S0#symbol{unit=#base_unit{
      code= <<"<%= unit[:attributes][:Code] %>">>,
      name= <<"<%= unit[:name] %>">>,
      property= <<"<%= unit[:property] %>">>,
      dim= <<"<%= unit[:attributes][:dim] %>">>
    }},

    exponent(String, S);
  <% end %>
   unit(String, _S0) ->
    erlang:error({unknown_unit, String}).

  exponent(String, S0) ->
    case String of
      <<$+, Rest/binary>> ->
        exponent_digits(0, Rest, S0);
      <<$-, Rest/binary>> ->
        exponent_digits(1, Rest, S0);
      _ ->
        maybe_exponent_digits(String, S0)
  end.

  exponent_digits(Sign, String, S0) ->
    StringL = binary_to_list(String),
    {Exp, Rest} = case string:to_integer(StringL) of
      {error, _} -> erlang:error({invalid_symbol_exponent, Sign, String});
      {Int, RestL} -> {Int, list_to_binary(RestL)}
    end,
    {S0#symbol{exp=Exp},
    Rest}.

  <%= for digit <- ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] do %>
  maybe_exponent_digits(<<$<%= digit %>, _/binary>>=String, S0) ->
    exponent_digits(1, String, S0);
  <% end %>
  maybe_exponent_digits(String, S0) ->
    {S0, String}.
  """

  Record.defrecord(
    :xmlElement,
    Record.extract(:xmlElement, from_lib: "xmerl/include/xmerl.hrl")
  )

  Record.defrecord(
    :xmlText,
    Record.extract(:xmlText, from_lib: "xmerl/include/xmerl.hrl")
  )

  Record.defrecord(
    :xmlAttribute,
    Record.extract(:xmlAttribute, from_lib: "xmerl/include/xmerl.hrl")
  )

  def go() do
    {root, []} = :xmerl_scan.file("priv/ucum-essence.xml")
    content = xmlElement(root, :content)

    data =
      Enum.reduce(content, %{prefix: [], base_unit: [], unit: []}, fn node, acc ->
        case elem(node, 0) do
          :xmlElement ->
            attributes =
              Enum.into(Enum.map(xmlElement(node, :attributes), &parse_attribute/1), %{})

            {type, element} = parse_element(node)
            element = Map.put(element, :attributes, attributes)
            list = Map.get(acc, type)
            Map.put(acc, type, [element | list])

          _ ->
            # ignore
            acc
        end
      end)

    data = data[:prefix]
    |> Enum.sort(&(&1[:attributes][:Code] >= &2[:attributes][:Code]))
    |> Enum.group_by(&(length(&1[:attributes][:Code])))
    |> Map.to_list
    |> List.keysort(0)
    |> Enum.reverse
    |> (&Map.put(data, :prefix, &1)).()

    data = Map.put(data, :unit, Enum.sort(data[:unit],
          &(length(&1[:attributes][:Code]) >= length(&2[:attributes][:Code]))))

    parser_src =
      EEx.eval_string(
        @parser_template,
        prefixes: data.prefix,
        base_units: data.base_unit,
        units: data.unit
      )

    File.write!("src/ucum_parser.erl", parser_src)
  end

  defp parse_element(
         xmlElement(
           name: :prefix,
           content: content
         )
       ) do
    {:prefix, Enum.reduce(content, %{}, &parse_prefix/2)}
  end

  defp parse_element(
         xmlElement(
           name: :"base-unit",
           content: content
         )
       ) do
    {:base_unit, Enum.reduce(content, %{}, &parse_base_unit/2)}
  end

  defp parse_element(
         xmlElement(
           name: :unit,
           content: content
         )
       ) do
    {:unit, Enum.reduce(content, %{}, &parse_unit/2)}
  end

  defp parse_prefix(
         xmlElement(
           name: :name,
           content: [xmlText(value: value)]
         ),
         acc
       ) do
    Map.put(acc, :name, value)
  end

  defp parse_prefix(
         xmlElement(
           name: :printSymbol,
           content: [xmlText(value: value)]
         ),
         acc
       ) do
    Map.put(acc, :printSymbol, value)
  end

  defp parse_prefix(
         xmlElement(name: :value) = node,
         acc
       ) do
    value = parse_attributes(node, %{})[:value]
    Map.put(acc, :value, value)
  end

  defp parse_prefix(_, acc) do
    acc
  end

  defp parse_base_unit(
         xmlElement(
           name: :name,
           content: [xmlText(value: value)]
         ),
         acc
       ) do
    Map.put(acc, :name, value)
  end

  defp parse_base_unit(
         xmlElement(
           name: :printSymbol,
           content: [xmlText(value: value)]
         ),
         acc
       ) do
    Map.put(acc, :printSymbol, value)
  end

  defp parse_base_unit(
         xmlElement(
           name: :property,
           content: [xmlText(value: value)]
         ),
         acc
       ) do
    Map.put(acc, :property, value)
  end

  defp parse_base_unit(_, acc) do
    acc
  end

  defp parse_unit(
         xmlElement(
           name: :name,
           content: [xmlText(value: value)]
         ),
         acc
       ) do
    names = Map.get(acc, :name, [])
    Map.put(acc, :name, [value | names])
  end

  defp parse_unit(
         xmlElement(
           name: :property,
           content: [xmlText(value: value)]
         ),
         acc
       ) do
    Map.put(acc, :property, value)
  end

  defp parse_unit(
         xmlElement(name: :value) = unit_value,
         acc
       ) do
    value = parse_attributes(unit_value, %{})
    Map.put(acc, :value, value)
  end

  defp parse_unit(_, acc) do
    acc
  end

  defp parse_attributes(node, map) do
    Enum.into(Enum.map(xmlElement(node, :attributes), &parse_attribute/1), map)
  end

  defp parse_attribute(
         xmlAttribute(
           name: name,
           value: value
         )
       ) do
    {name, value}
  end
end

Ucum.Codegen.go()

System.halt(0)
