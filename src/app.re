
module RR = ReasonReact;
module RE = ReactEvent;

let s = RR.string;

module Result = Belt.Result;

module Env = Interp.Env;

module Interp = {
  exception NoInput;

  type result = {
    result: string,
    outputs: list(string)
  };

  type error = {
    tpe: string,
    message: string
  };

  let makeEnv = () => {
    (
      Interp.default_genv(),
      Interp.Env.make()
    )
  }

  let run = (~genv=?, ~lenv=?, src, inputStr) => {
    let inputStr = inputStr |> Js.String.split("\n") |> Array.to_list;
    let genv = Belt.Option.getWithDefault(
      genv,
      Interp.default_genv()
    );
    let lenv = Belt.Option.getWithDefault(
      lenv,
      Interp.Env.make()
    );
    let input = {
      as _;
      val mutable inputStr = inputStr;
      pub gets = () => switch (inputStr) {
      | [str, ...rest] =>
        inputStr = rest;
        str;
      | [] =>
        raise(NoInput);
      }
    }
    let output = {
      as _;
      val mutable outputStr = [];
      pub p = str => outputStr = [str, ...outputStr];
      pub flush = () => outputStr;
    }
    let result = switch (
      Parser.main(Lexer.token, Lexing.from_string(src))
      |> Interp.eval(genv, lenv, input, output)
    ) {
    | result =>
      Result.Ok({ result: Value.show(result), outputs: output#flush() });
    | exception Parsing.Parse_error =>
      Result.Error({ tpe: "ParseError", message: "Parse error." })
    | exception NoInput =>
      Result.Error({ tpe: "InvalidInput", message: "No enough inputs." });
    | exception Interp.Invalid_VarRef(var) =>
      Result.Error({ tpe: "InvalidVarRef", message: {j|Variable name: $(var)|j} });
    | exception Interp.No_such_function(funName) =>
      Result.Error({ tpe: "NoSuchFunction", message: {j|Function name: $(funName)|j} });
    | exception Interp.Invalid_argument_number(act, exp, funName) =>
      let act = string_of_int(act) and exp = string_of_int(exp);
      Result.Error({ tpe: "InvalidArgumentNumber", message: {j|$(funName) expects $(exp)(s) argument(s), but $(act).|j} });
    | exception Interp.Invalid_slice_operation(value) =>
      let value = Value.show(value);
      Result.Error({ tpe: "InvalidSliceOperation", message: {j|$(value) can't be sliced.|j} });
    | exception Interp.Invalid_array_index(value) =>
      let value = Value.show(value);
      Result.Error({ tpe: "InvalidArrayIndex", message: {j|$(value) is invalid as an index of arrays.|j} });
    | exception Interp.Invalid_func_call(funName) =>
      Result.Error({ tpe: "InvalidFuncCall", message: {j|$(funName) is called with an invalid way.|j} });
    | exception Interp.Builtin.Invalid_argument_number(funName, num) =>
      let num = string_of_int(num);
      Result.Error({ tpe: "InvalidArgumentNumber", message: {j|$(funName) can't accept $(num) argument(s).|j} });
    | exception Interp.Builtin.Invalid_argument_type(funName, args) =>
      let args =
        args
        |> List.map(Value.show)
        |> String.concat(", ");
      Result.Error({ tpe: "InvalidArgumentType", message: {j|$(funName) can't accept [ $(args) ].|j} });
    | exception Interp.Builtin.Unexpected_type_exception(funName, arg) =>
      let arg = Value.show(arg);
      Result.Error({ tpe: "InvalidTypeException", message: {j|$(funName) with unexpected type arg $(arg).|j} });
    };
    (genv, lenv, result)
  }
};

module Eval = {
  type action =
    | UpdateSrc(string)
    | UpdateInput(string)
    | Run
    | RunFinished(Result.t(list(string), list(string)))
    | Clear;

  type state = {
    src: string,
    inputs: string,
    outputs: Result.t(list(string), list(string))
  };
};

module Repl = {
  type lines =
    | Output(list(string))
    | Error(list(string));

  type genv = Dummy;

  type comp = {
    genv: genv,
    lenv: Env.t(Value.t),
    lines: lines
  };

  type result =
    | Comp(comp)
    | Cont;
  
  let submit = (genv, lenv, lines) => {
    let src =
      lines
      |> List.rev
      |> String.concat("\n");
    switch (Interp.run(~genv, ~lenv, src, "")) {
    | (genv, lenv, Result.Ok({ Interp.result, outputs })) =>
      let outputs = ["#=> " ++ result, ...outputs];
      Comp({ genv: Obj.magic(genv), lenv, lines: Output(List.rev(outputs)) });
    | (_, _, Result.Error({ Interp.tpe: "ParseError" })) =>
      Cont;
    | (_, _, Result.Error({ Interp.tpe, message })) =>
      Comp({ genv: Obj.magic(genv), lenv, lines: Error([tpe, message])});
    }
  };

  type history =
    | Input(list(string))
    | Output(list(string))
    | Error(list(string));

  type action =
    | Reset
    | UpdateText(string)
    | Run;

  type state = {
    genv: genv,
    lenv: Env.t(Value.t),
    histories: list(history),
    current: list(string),
    text: string
  };

  let empty () = {
    let (genv, lenv) = Interp.makeEnv();
    {
      genv: Obj.magic(genv),
      lenv,
      histories: [],
      current: [],
      text: ""
    }
  }

  let renderHistories = histories => {
    let (_, outputs) = List.fold_right(
      (hist, (count, outputs)) => switch (hist) {
      | Input(lines) =>
        switch (List.rev(lines)) {
        | [head, ...tails] =>
          let num = string_of_int(count);
          let head = <div className="repl-console-input">{s(num ++ "> " ++ head)}</div>;
          let tails = List.map(line => <div className="repl-console-input indented">{s("| " ++ line)}</div>, tails);
          let output = List.rev([head, ...tails]);
          (count + 1, List.append(output, outputs));
        | [] => (count, outputs);
        };
      | Output(lines) =>
        switch (lines) {
        | [head, ...tails] =>
          let head = <div className="repl-console-output">{s(head)}</div>;
          let tails = List.map(line => <div className="repl-console-output indented">{s(line)}</div>, tails);
          let output = List.rev([head, ...tails]);
          (count, List.append(output, outputs));
        | [] => (count, outputs);
        }
      | Error(lines) =>
        let output =
          lines
          |> List.rev
          |> List.map(line => <div className="repl-console-error">{s(line)}</div>);
        (count, List.append(output, outputs));
      },
      histories,
      (1, [])
    );
    outputs
    |> List.rev
    |> Array.of_list;
  }
};

type state =
  | ExampleState
  | EvalState(Eval.state)
  | ReplState(Repl.state);

type action =
  | GoTo(state)
  | EvalAction(Eval.action)
  | ReplAction(Repl.action);

let component = RR.reducerComponent("App");

let initialState = () => {
  switch (RR.Router.dangerouslyGetInitialUrl()) {
  | {RR.Router.hash: "eval" } => EvalState(Eval.{ src: "", inputs: "", outputs: Result.Ok([]) });
  | {RR.Router.hash: "repl" } => ReplState(Repl.empty());
  | _ => ExampleState;
  }
};

let didMount = self => {
  let watcherID = RR.Router.watchUrl(url =>
    switch (url.hash) {
    | "eval" => self.RR.send(GoTo(EvalState(Eval.{ src: "", inputs: "", outputs: Result.Ok([]) })));
    | "repl" => self.RR.send(GoTo(ReplState(Repl.empty())));
    | _ => self.RR.send(GoTo(ExampleState));
    }
  );
  self.RR.onUnmount(() => RR.Router.unwatchUrl(watcherID));
};

let reducer = (action, state) => {
  switch (action, state) {
  | (GoTo(state), _) =>
    RR.Update(state);
  | (EvalAction(action), EvalState(state)) =>
    switch (action, state) {
    | (UpdateSrc(src), _) =>
      RR.Update(EvalState({ ...state, src }));
    | (UpdateInput(inputs), _) =>
      RR.Update(EvalState({ ...state, inputs }));
    | (Run, { src, inputs }) =>
      RR.SideEffects(self => {
        let outputs = switch (Interp.run(src, inputs)) {
        | (_, _, Result.Ok({ Interp.result, outputs })) =>
          Result.Ok(List.concat([List.rev(outputs), [result]]));
        | (_, _, Result.Error({ Interp.tpe, message })) =>
          Result.Error([tpe, message]);
        };
        self.send(EvalAction(Eval.RunFinished(outputs)))
      });
    | (RunFinished(outputs), _) =>
      RR.Update(EvalState({ ...state, outputs }));
    | (Clear, _) =>
      RR.Update(EvalState({ ...state, src: "" }));
    };
  | (ReplAction(action), ReplState(state)) =>
    switch (action, state) {
    | (Reset, _) =>
      RR.Update(ReplState(Repl.empty()));
    | (UpdateText(text), _) =>
      RR.Update(ReplState({ ...state, text }));
    | (Run, { genv, lenv, histories, current, text }) =>
      let current = [text, ...current];
      switch (Repl.submit(Obj.magic(genv), lenv, current)) {
      | Cont =>
        RR.Update(ReplState({ ...state, current, text: ""}));
      | Comp({ genv, lenv, lines: Output(lines) }) =>
        let histories = [Repl.Output(lines), Repl.Input(current), ...histories];
        RR.Update(ReplState({ genv, lenv, histories, current: [], text: "" }));
      | Comp({ genv, lenv, lines: Error(lines) }) =>
        let histories = [Repl.Error(lines), Repl.Input(current), ...histories];
        RR.Update(ReplState({ genv, lenv, histories, current: [], text: "" }));
      }
    }
  | _ =>
    Js.log("Invalid action and status.");
    RR.NoUpdate;
  };
};

let enterKey = 13;
class dispatcher(self) = {
  as _;

  pub onEvalSrcChange = event => {
    let value = event->RE.Form.target##value;
    self.RR.send(EvalAction(Eval.UpdateSrc(value)));
  };
  pub doEvalRun = event => {
    event->RE.Mouse.preventDefault;
    self.RR.send(EvalAction(Eval.Run));
  };
  pub onEvalInputChange = event => {
    let value = event->RE.Form.target##value;
    self.RR.send(EvalAction(Eval.UpdateInput(value)));
  };
  pub doEvalClear = event => {
    event->RE.Mouse.preventDefault;
    self.RR.send(EvalAction(Eval.Clear));
  };

  pub onReplReset = event => {
    event->RE.Mouse.preventDefault;
    self.RR.send(ReplAction(Repl.Reset));
  };
  pub onReplInputChange = event => {
    let value = event->RE.Form.target##value;
    self.RR.send(ReplAction(Repl.UpdateText(value)));
  };
  pub onReplInputKeyPress = event => {
    if (event->RE.Keyboard.charCode == enterKey) {
      switch (self.RR.state) {
      | ReplState(_) => self.RR.send(ReplAction(Repl.Run));
      | _ => ()
      }
    }
  }
};

let render = self => {
  let dispatcher = (new dispatcher)(self);
  let content =
    switch (self.RR.state) {
    | ExampleState =>
      <AppExample />
    | EvalState(state) =>
      let { Eval.src, inputs, outputs } = state;
      <AppEval state=(AppEval.{ src, inputs, outputs }) dispatcher />
    | ReplState(state) =>
      let { Repl.histories, current, text } = state;
      let histories = Repl.renderHistories(histories);
      <AppRepl state=(AppRepl.{ current, text }) dispatcher>
        ...histories
      </AppRepl>
    };
  let changeUrl = (hash, event) => {
    event->RE.Mouse.preventDefault;
    let {RR.Router.path} = RR.Router.dangerouslyGetInitialUrl();
    RR.Router.push(String.concat("/", path) ++ "#" ++ hash);
  };
  <>
    <header className="header">
      <div className="logo">
        {s("BUCKLERUBY")}
      </div>
      <div className="links">
        <span className="example" onClick={changeUrl("example")}>{s("Example")}</span>
        <span className="eval" onClick={changeUrl("eval")}>{s("Eval")}</span>
        <span className="repl" onClick={changeUrl("repl")}>{s("REPL")}</span>
      </div>
    </header>
    <div className="container">
      <div className="content">
        {content}
      </div>
    </div>
  </>
};

let make = _children => {
  ...component,
  initialState,
  didMount,
  reducer,
  render
};