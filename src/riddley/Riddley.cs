using System;
using clojure.lang;
using clojure.lang.CljCompiler.Ast;

namespace Riddley
{
    public static class Util
    {
		public static LocalBinding LocalBinding(int num, Symbol sym, Symbol tag, Object form)
		{
			return new LocalBinding(num, sym, tag, Compiler.Analyze(new ParserContext(RHC.Expression), form), typeof(Object), false, false, false);
		}

		public static LocalBinding LocalArgument(int num, Symbol sym, Symbol tag)
        {
			return new LocalBinding(num, sym, tag, null, typeof(Object), false, true, false);
        }
    }

	public class ObjMethod : clojure.lang.CljCompiler.Ast.ObjMethod
	{
		public ObjMethod () : base(new ObjExpr(null), null)
		{
			
		}

		public override bool IsVariadic => throw new NotImplementedException();

		public override int NumParams => throw new NotImplementedException();

		public override int RequiredArity => throw new NotImplementedException();

		public override string MethodName => throw new NotImplementedException();

		public override Type ReturnType => throw new NotImplementedException();

		public override Type[] ArgTypes => throw new NotImplementedException();
	}
}