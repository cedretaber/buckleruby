
module RR = ReasonReact;

module Result = Belt.Result;

let s = RR.string;

type state = {
  src: string,
  inputs: string,
  outputs: Result.t(list(string), list(string))
};

module SourceInput = {
  let component = RR.statelessComponent("AppEval.SourceInput");

  let make = (~value, ~onChange, _children) => {
    ...component,
    render: _self =>
      <div className="src-input">
        <p>{s("Source Code:")}</p>
        <textarea rows=20 cols=80 value onChange />
      </div>
  }
};

module Control = {
  let component = RR.statelessComponent("AppEval.Control");

  let make = (~doRun, ~doClear, _children) => {
    ...component,
    render: _self =>
      <div className="src-control">
        <button onClick={doRun}>{s("Run")}</button>
        <button onClick={doClear}>{s("Clear")}</button>
      </div>
  }
};

module InputInput = {
  let component = RR.statelessComponent("AppEval.InputInput");

  let make = (~value, ~onChange, _children) => {
    ...component,
    render: _self => {
      <div className="input-input">
        <p>{s("Input:")}</p>
        <textarea rows=10 cols=80 value onChange />
      </div>
    }
  };
}

module Console = {
  let component = RR.statelessComponent("AppEval.Console");

  let make = (~value, ~isError, _children) => {
    ...component,
    render: _self => {
      let value =
        if (isError) {
          switch (value) {
          | [tpe, ...message] =>
            let message =
              message
              |> List.map(s => "  " ++ s)
              |> String.concat("");
            tpe ++ message;
          | _ => ""
          };
        } else {
          String.concat("", value);
        };
      let codeColour = isError ? "error" : "default";
      let className = {j|src-console $(codeColour)|j};
      <div className>
        <pre>
          <code>
            {s(value)}
          </code>
        </pre>
      </div>
    }
  }
};

let component = RR.statelessComponent("AppEval");

let make = (~state as {src, inputs, outputs}, ~dispatcher, _children) => {
  ...component,
  render: _self => {
    let (isError, outputs) = switch (outputs) {
    | Result.Ok(outputs) => (false, outputs);
    | Result.Error(outputs) => (true, outputs);
    };
    <div className="eval">
      <SourceInput value=src onChange={dispatcher#onEvalSrcChange} />
      <Control doRun={dispatcher#doEvalRun} doClear={dispatcher#doEvalClear} />
      <InputInput value=inputs onChange={dispatcher#onEvalInputChange} />
      <Console value=outputs isError />
    </div>
  }
};