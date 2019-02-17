
module RR = ReasonReact;
module RE = ReactEvent;

module Result = Belt.Result;

module Interp = {
  exception NoInput;

  type result = {
    result: string,
    inputs: string,
    outputs: list(string)
  };

  type error = {
    tpe: string,
    message: string
  };

  let run = (src, inputStr') => {
    let inputStr = inputStr' |> Js.String.split("\n") |> Array.to_list;
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
    };
    let output = {
      as _;
      val mutable outputStr = [];
      pub p = str => outputStr = [str, ...outputStr];
      pub flush = () => outputStr;
    };
    let tree = Parser.main(Lexer.token, Lexing.from_string(src));
    switch (Interp.eval_with_default(input, output, tree)) {
    | result =>
      Result.Ok({ result: Value.show(result), inputs: inputStr', outputs: output#flush() });
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
  }
}

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

type state =
  | EvalState(Eval.state);

type action =
  | EvalAction(Eval.action);

let component = RR.reducerComponent("App");

let initialState = () => {
  EvalState(Eval.{ src: "", inputs: "", outputs: Result.Ok([]) });
};

let reducer = (action, state) => {
  switch (action, state) {
  | (EvalAction(action), EvalState(state)) =>
    switch (action, state) {
    | (UpdateSrc(src), _) =>
      RR.Update(EvalState({ ...state, src }));
    | (UpdateInput(inputs), _) =>
      RR.Update(EvalState({ ...state, inputs }));
    | (Run, { src, inputs }) =>
      RR.SideEffects(self => {
        let outputs = switch (Interp.run(src, inputs)) {
        | Result.Ok({ Interp.result, outputs }) =>
          Result.Ok(List.concat([List.rev(outputs), [result]]));
        | Result.Error({ Interp.tpe, message }) =>
          Result.Error([tpe, message]);
        };
        self.send(EvalAction(Eval.RunFinished(outputs)))
      });
    | (RunFinished(outputs), _) =>
      RR.Update(EvalState({ ...state, outputs }));
    | (Clear, _) =>
      RR.Update(EvalState({ ...state, src: "" }));
    };
  };
};

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
};

let render = self => {
  let dispatcher = (new dispatcher)(self);
  switch (self.RR.state) {
  | EvalState(state) =>
    let { Eval.src, inputs, outputs } = state;
    <AppEval state=(AppEval.{ src, inputs, outputs }) dispatcher />
  };
};

let make = _children => {
  ...component,
  initialState,
  reducer,
  render
};