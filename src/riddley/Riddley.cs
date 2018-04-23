using System;
using clojure.lang;
using clojure.lang.CljCompiler.Ast;

namespace Riddley
{
    public static class Util
    {
		public static LocalBinding LocalBinding(int num, Symbol sym, Symbol tag, object form)
		{
			return new LocalBinding(num, sym, tag, Compiler.Analyze(new ParserContext(RHC.Expression), form), typeof(object), false, false, false);
		}

		public static LocalBinding LocalArgument(int num, Symbol sym, Symbol tag)
        {
			return new LocalBinding(num, sym, tag, null, typeof(object), false, true, false);
        }
    }

	public class ObjMethod : clojure.lang.CljCompiler.Ast.ObjMethod
	{
		public ObjMethod () : base(new ObjExpr(null), null)
		{
			
		}

		public override bool IsVariadic
		{
			get { throw new NotImplementedException(); }
		}

		public override int NumParams
		{
			get { throw new NotImplementedException(); }
		}

		public override int RequiredArity
		{
			get { throw new NotImplementedException(); }
		}

		public override string MethodName
		{
			get { throw new NotImplementedException(); }
		}

		public override Type ReturnType
		{
			get { throw new NotImplementedException(); }
		}

		public override Type[] ArgTypes
		{
			get { throw new NotImplementedException(); }
		}
	}
}