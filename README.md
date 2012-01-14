## eml - Erlang flavored by Some ML

There are no Erlang receive,send or match expressions in EML.

Instead there is let and a type of letrec construct as
well as currying. Inspired from Haskel, there is also a way 
to express generation of a list of integers and to represent 
arithmetic operators as an anonymous function.
Currently, only program transformation is done, mainly 
inspired by the classic SPJ book.

Example 1:

    fun foo =
      let val X = [1,2,3]
          val Y = [a,b,c]
          rec Zip = fn [] [] => [] | fn [A|B] [H|T] => [{A,H} | Zip(B,T)]
      in Zip(X,Y);

Example 2:

    fun double L =
      let val Mult2 = map(fn X => X * 2)
      in  Mult2(L);
    
    fun map F [H|T] =
      let val Hd = F(H)
          val Tl = map(F,T)
      in
        [Hd|Tl]
     | map _ [] = [];

Example 3:

    fun f3 N = foldl(@*, 1, [1..N]);
    
    fun foldl Fun Acc [H|T] = foldl(Fun, Fun(H,Acc), T)
     |  foldl _   Acc []    = Acc;

If you want to try it out you can either clone the 
[eml_examples.git][1] repo which setup the eml
compiler as a rebar plugin, or set it up your self
by adding the following to your rebar.config file:

    {deps_dir, ["deps"]}.
    {deps, [
            {eml, "0.*", 
             {git, "git@github.com:etnt/eml.git", 
              "HEAD"}}
           ]}.
    {plugins, [ rebar_eml_plugin ]}.
    {plugin_dir, "deps/rebar_eml_plugin/src"}.

[1]: https://github.com/etnt/eml_examples

