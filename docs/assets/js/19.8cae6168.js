(window.webpackJsonp=window.webpackJsonp||[]).push([[19],{404:function(t,a,s){"use strict";s.r(a);var n=s(54),e=Object(n.a)({},(function(){var t=this,a=t.$createElement,s=t._self._c||a;return s("ContentSlotsDistributor",{attrs:{"slot-key":t.$parent.slotKey}},[s("h1",{attrs:{id:"tools"}},[s("a",{staticClass:"header-anchor",attrs:{href:"#tools"}},[t._v("#")]),t._v(" Tools")]),t._v(" "),s("p",[t._v("Caliban comes with a module called "),s("code",[t._v("caliban-tools")]),t._v(" that exposes some useful features:")]),t._v(" "),s("ul",[s("li",[t._v("all the code generation features from "),s("code",[t._v("caliban-codegen-sbt")]),t._v(", so that you can use them without sbt: see "),s("code",[t._v("caliban.tools.Codegen")]),t._v(".")]),t._v(" "),s("li",[t._v("a client for GraphQL introspection: see "),s("code",[t._v("caliban.tools.IntrospectionClient")]),t._v(".")]),t._v(" "),s("li",[t._v("utilities for "),s("RouterLink",{attrs:{to:"/docs/stitching.html"}},[t._v("stitching GraphQL schemas")]),t._v(".")],1),t._v(" "),s("li",[t._v("a way to compare GraphQL schemas, whether they come from Caliban or a remote server, see below.")])]),t._v(" "),s("h2",{attrs:{id:"dependency"}},[s("a",{staticClass:"header-anchor",attrs:{href:"#dependency"}},[t._v("#")]),t._v(" Dependency")]),t._v(" "),s("div",{staticClass:"language- extra-class"},[s("pre",{pre:!0,attrs:{class:"language-text"}},[s("code",[t._v('libraryDependencies += "com.github.ghostdogpr" %% "caliban-tools" % "1.2.2"\n')])])]),s("h2",{attrs:{id:"schema-comparison"}},[s("a",{staticClass:"header-anchor",attrs:{href:"#schema-comparison"}},[t._v("#")]),t._v(" Schema comparison")]),t._v(" "),s("p",[t._v("The object "),s("code",[t._v("caliban.tools.SchemaComparison")]),t._v(" exposes a "),s("code",[t._v("compare")]),t._v(" function that compares 2 schemas from different origins. It takes 2 "),s("code",[t._v("SchemaLoader")]),t._v(" as arguments, which you can build with one of the following constructors:")]),t._v(" "),s("ul",[s("li",[s("code",[t._v("fromCaliban")]),t._v(": pass your "),s("code",[t._v("GraphQL")]),t._v(" object from Caliban")]),t._v(" "),s("li",[s("code",[t._v("fromFile")]),t._v(": pass the path to a file containing your schema in the GraphQL IDL")]),t._v(" "),s("li",[s("code",[t._v("fromString")]),t._v(": pass a string containing your schema in the GraphQL IDL")]),t._v(" "),s("li",[s("code",[t._v("fromIntrospection")]),t._v(": pass the URL of a GraphQL server supporting introspection")])]),t._v(" "),s("p",[t._v("The output of "),s("code",[t._v("compare")]),t._v(" is a "),s("code",[t._v("Task[List[SchemaComparisonChange]]")]),t._v(", with "),s("code",[t._v("SchemaComparisonChange")]),t._v(" being a sealed trait representing the various kinds of changes. "),s("code",[t._v("SchemaComparisonChange#breaking")]),t._v(" indicates if the change is breaking, such as removing a field or a type. "),s("code",[t._v("SchemaComparisonChange#toString")]),t._v(" will return a nice description of the change.")]),t._v(" "),s("p",[t._v("The following example will compare the schema obtained by Caliban with a schema defined in a string and print the differences.")]),t._v(" "),s("div",{staticClass:"language-scala extra-class"},[s("pre",{pre:!0,attrs:{class:"language-scala"}},[s("code",[s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("import")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token namespace"}},[t._v("caliban"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")])]),t._v("GraphQL"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("graphQL\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("import")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token namespace"}},[t._v("caliban"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")])]),t._v("RootResolver\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("import")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token namespace"}},[t._v("caliban"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("tools"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")])]),t._v("_\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("import")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token namespace"}},[t._v("zio"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")])]),t._v("UIO\n\n"),s("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// schema from String")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" schema"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v("\n  "),s("span",{pre:!0,attrs:{class:"token triple-quoted-string string"}},[t._v('"""\n  type Hero {\n    name(pad: Int!): String!\n    nick: String!\n    bday: Int\n  }\n  \n  type Query {\n    hero: Hero!\n  }"""')]),t._v("\n\n"),s("span",{pre:!0,attrs:{class:"token comment"}},[t._v("// schema from Caliban")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" NameArgs"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("pad"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Int")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" Hero"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("name"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" NameArgs "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("=>")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" nick"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("String")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" bday"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" Option"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("[")]),s("span",{pre:!0,attrs:{class:"token builtin"}},[t._v("Int")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("]")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("case")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("class")]),t._v(" Query"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("hero"),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v(":")]),t._v(" Hero"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("val")]),t._v(" api "),s("span",{pre:!0,attrs:{class:"token operator"}},[t._v("=")]),t._v(" graphQL"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("RootResolver"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("Query"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("Hero"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("_ "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("=>")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token string"}},[t._v('"name"')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token string"}},[t._v('"nick"')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" None"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n\n"),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("for")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("{")]),t._v("\n  diff "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("<-")]),t._v(" SchemaComparison"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("compare"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("SchemaLoader"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("fromString"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("schema"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(",")]),t._v(" SchemaLoader"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("fromCaliban"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("api"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n  _    "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("<-")]),t._v(" UIO"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("println"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),t._v("diff"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(".")]),t._v("mkString"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token string"}},[t._v('"\\n"')]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n"),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("}")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token keyword"}},[t._v("yield")]),t._v(" "),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v("(")]),s("span",{pre:!0,attrs:{class:"token punctuation"}},[t._v(")")]),t._v("\n")])])])])}),[],!1,null,null,null);a.default=e.exports}}]);