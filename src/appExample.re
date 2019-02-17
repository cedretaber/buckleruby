
module RR = ReasonReact;

let s = RR.string;

module CodeBlock = {
  let component = RR.statelessComponent("Example.CodeBlock");

  let make = (~value, _children) => {
    ...component,
    render: _self =>
      <div className="codeblock">
        <code>
          <pre>
            {s(value)}
          </pre>
        </code>
      </div>
  }
}

let component = RR.statelessComponent("Example");

let code = {j|
def add(x, y)
  x + y
end

x = 42

p(add(x, 99))

arr = [1, 2, 3]

p(arr)

arr[1] = x

p(arr)

hash = { "x" => 42, "y" => 99 }

p(hash["y"])

if x > 45
  p("big")
elsif x < 100
  p("middle")
else
  p("small")
end

i = 0
while i < 3
  p(arr[i])
  i = i + 1
end

msg = gets()
p(msg)
|j}

let make = _children => {
  ...component,
  render: _self =>
    <div className="example">
      <h1>{s("BuckleRuby")}</h1>
      <CodeBlock value=code />
    </div>
}