
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
        <textarea value onChange />
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
        <textarea value onChange />
      </div>
    }
  };
}

module Console = {
  let component = RR.statelessComponent("AppEval.Console");

  let make = (~value, _children) => {
    ...component,
    render: _self => {
      let value = String.concat("\n", value);
      <div className="src-console">
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
    let outputs = switch (outputs) {
    | Result.Ok(outputs) => outputs;
    | Result.Error(outputs) => outputs;
    };
    <div className="eval">
      <SourceInput value=src onChange={dispatcher#onEvalSrcChange}/>
      <Control doRun={dispatcher#doEvalRun} doClear={dispatcher#doEvalClear}/>
      <InputInput value=inputs onChange={dispatcher#onEvalInputChange} />
      <Console value=outputs/>
    </div>
  }
};