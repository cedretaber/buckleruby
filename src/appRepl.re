
module RR = ReasonReact;

let s = RR.string;

type state = {
  current: list(string),
  text: string
};

let component = RR.statelessComponent("AppRepl");

let make = (~state as {current, text}, ~dispatcher, children) => {
  ...component,
  render: _self => {
    let current = switch (List.rev(current)) {
    | [head, ...tails] =>
      let head = <div>{s("> " ++ head)}</div>;
      let tails = List.map(line => <div>{s("| " ++ line)}</div>, tails);
      [head, ...tails]
      |> Array.of_list
    | [] => [||]
    };
    let cur = s(Array.length(current) == 0 ? "> " : "| ");
    <div>
      <div className="repl-reset">
        <button onClick={dispatcher#onReplReset}>{s("Clear")}</button>
      </div>
      <div className="repl-block">
        <div className="repl-console">
          ...children
        </div>
        <div className="repl-newline">
          <div>
            ...current
          </div>
          <div>
            <span className="repl-cur">
              {cur}
            </span>
            <input className="repl-input"
                    type_="text"
                    value=text
                    onChange={dispatcher#onReplInputChange}
                    onKeyPress={dispatcher#onReplInputKeyPress}
            />
          </div>
        </div>
      </div>
    </div>
  }
};