using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;

namespace Theblueway.CodeGen.Runtime
{
    public static class TypeUtils
    {
        public static string GetMethodSignature<TDelegate>(TDelegate action) where TDelegate : Delegate
        {
            return GetMethodSignature(action.Method, useNameOfOperator: false);
        }

        public static string GetMethodSignature(MethodInfo method, bool useNameOfOperator = false)
        {
            if(!method.IsGenericMethodDefinition && method.IsGenericMethod)
            {
                method = method.GetGenericMethodDefinition();
            }


            string genericArity;

            if (method.IsGenericMethodDefinition)
            {
                var args = method.GetGenericArguments();

                genericArity = "<" + string.Join(",", method.GetGenericArguments().Select(a => a.Name)) + ">";
            }
            else
                genericArity = "";


            var parameters = string.Join(",",
                method.GetParameters()
                      .Select(p => ToSignatureTypeName(p.ParameterType, useNameOfOperator)));

            var returnType = ToSignatureTypeName(method.ReturnType, useNameOfOperator);

            string methodName = useNameOfOperator ? $"{{nameof({ToTypeReferenceText(method.DeclaringType, withNameSpace: true)}.{method.Name})}}" : method.Name;

            return $"{methodName}{genericArity}({parameters}):{returnType}";
        }


        public static string ToSignatureTypeName(Type type, bool useNameOfOperator)
        {
            if (type == typeof(void)) useNameOfOperator = false;

            if (type.IsGenericMethodParameter)
                return type.Name;

            // Handle generic type parameter (T, U, etc.)
            if (type.IsGenericParameter)
                return useNameOfOperator ? $"{{nameof({type.Name})}}" : type.Name;

            // Handle byref (ref/in/out)
            if (type.IsByRef)
                return ToSignatureTypeName(type.GetElementType()!, useNameOfOperator) + "&";

            // Handle pointers
            if (type.IsPointer)
                return ToSignatureTypeName(type.GetElementType()!, useNameOfOperator) + "*";

            // Handle arrays
            if (type.IsArray)
                return ToSignatureTypeName(type.GetElementType()!, useNameOfOperator) + "[" + new string(',', type.GetArrayRank() - 1) + "]";

            // Handle constructed generic types
            if (type.IsGenericType)
            {
                var defName = type.GetGenericTypeDefinition().FullName!;
                defName = defName.Substring(0, defName.IndexOf('`')); // strip arity
                defName = useNameOfOperator ? $"{type.Namespace}.{{nameof({ToTypeReferenceText(type, withNameSpace: true)})}}" : defName;

                var args = type.GetGenericArguments().Select(type => ToSignatureTypeName(type, useNameOfOperator));

                return $"{type.Assembly.GetName().Name} {defName}<{string.Join(",", args)}>";
            }

            // Fallback: fully qualified name for non-generic, non-parameter types
            string typeName = useNameOfOperator ? $"{type.Namespace}.{{nameof({ToTypeReferenceText(type, withNameSpace: true)})}}" : type.FullName ?? type.Name;

            return $"{type.Assembly.GetName().Name} {typeName}";
        }








        /// <summary>
        /// Returns a string suitable for use inside typeof(...).
        /// Example: (Outer.OuterGen<>.Innner.InnerGen<,>)
        /// </summary>
        public static string ToTypeDefinitionText(Type type, bool withNameSpace = false)
        {
            if (type == null) throw new ArgumentNullException(nameof(type));

            var sb = new StringBuilder();

            // optionally include namespace (kept from earlier versions)
            if (withNameSpace && !string.IsNullOrEmpty(type.Namespace))
            {
                sb.Append(type.Namespace);
                sb.Append(".");
            }

            var chain = GetDeclaringChain(type);

            for (int i = 0; i < chain.Count; i++)
            {
                var t = chain[i];
                var baseName = StripGenericTick(t.Name);
                sb.Append(baseName);

                int ownArity = GetOwnGenericArityFromName(t.Name);
                if (ownArity > 0)
                {
                    sb.Append("<");
                    sb.Append(new string(',', ownArity - 1)); // "<,>" for arity 2 etc.
                    sb.Append(">");
                }

                if (i < chain.Count - 1) sb.Append(".");
            }

            AddBracketsIfArray(sb, type);

            return sb.ToString();
        }


        /// <summary>
        /// Returns a string suitable for using as a type expression with generic parameter placeholders:
        /// Example: Outer.OuterGen<T>.Innner.InnerGen<U, V>
        /// </summary>
        public static string ToTypeReferenceText(Type type, bool withNameSpace = false)
        {
            if (type == null) throw new ArgumentNullException(nameof(type));

            if (type.IsGenericParameter)
                return type.Name ?? type.FullName;


            var sb = new StringBuilder();

            if (withNameSpace && !string.IsNullOrEmpty(type.Namespace))
            {
                sb.Append(type.Namespace);
                sb.Append(".");
            }

            var chain = GetDeclaringChain(type);

            int argIndex = 0;
            var args = type.RealGetGenericArguments();

            for (int i = 0; i < chain.Count; i++)
            {
                var t = chain[i];
                var baseName = StripGenericTick(t.Name);
                sb.Append(baseName);

                int ownArity = GetOwnGenericArityFromName(t.Name);
                if (ownArity > 0)
                {
                    sb.Append("<");
                    var names = new string[ownArity];
                    for (int j = 0; j < ownArity; j++)
                    {
                        if (argIndex > args.Length - 1)
                        {
                            throw new ArgumentException("index");
                        }

                        var arg = args[argIndex++];
                        names[j] = ToTypeReferenceText(arg, withNameSpace: true);
                    }
                    sb.Append(string.Join(", ", names));
                    sb.Append(">");
                }

                if (i < chain.Count - 1) sb.Append(".");
            }

            AddBracketsIfArray(sb, type);

            return sb.ToString();
        }

        // ---------------- helpers ----------------

        // returns nested chain from outermost -> given type

        public static void AddBracketsIfArray(StringBuilder sb, Type type)
        {

            while (type.HasElementType)
            {
                if (type.IsArray)
                {
                    sb.Append("[");
                    sb.Append(new string(',', type.GetArrayRank() - 1));
                    sb.Append("]");

                    type = type.GetElementType()!;
                }
            }

        }


        private static List<Type> GetDeclaringChain(Type t)
        {
            while (t.HasElementType) t = t.GetElementType();

            if (t.IsGenericParameter) return new List<Type>() { t };

            var list = new List<Type>();
            for (Type cur = t; cur != null; cur = cur.DeclaringType)
                list.Insert(0, cur);
            return list;
        }

        // Name might be "Inner`2" or "SimpleName"
        private static string StripGenericTick(string name)
        {
            int idx = name.IndexOf('`');
            return idx >= 0 ? name.Substring(0, idx) : name;
        }

        // parse the integer after the backtick in the Type.Name, e.g. "Inner`2" -> 2
        private static int GetOwnGenericArityFromName(string name)
        {
            if (string.IsNullOrEmpty(name))
                return 0;

            // 1. Strip by-ref (&) and pointer (*) suffixes
            while (name.EndsWith("&") || name.EndsWith("*"))
                name = name.Substring(0, name.Length - 1);

            // 2. Strip array suffixes ([] [,] [,,] ...)
            // Arrays always start with '[' and end with ']'
            while (name.EndsWith("]"))
            {
                int idx = name.LastIndexOf('[');
                if (idx < 0) break;
                name = name.Substring(0, idx);
            }

            // 3. Look for the `n arity marker
            int backtick = name.IndexOf('`');
            if (backtick < 0)
                return 0;

            var numPart = name.Substring(backtick + 1);
            return int.TryParse(numPart, out int n) ? n : 0;
        }
    }


    public static class TypeExtensions
    {
        public static Type[] RealGetGenericArguments(this Type type)
        {
            while (type.HasElementType)
            {
                type = type.GetElementType()!;
            }
            return type.GetGenericArguments();
        }

        public static bool CanNotBeUsedAsGenericParameter(this Type type)
        {
            return type.IsPointer || type.IsByRef || type.IsByRefLike;
        }

        public static bool IsStruct(this Type type)
        {
            return type.IsValueType && !type.IsPrimitive && !type.IsEnum;
        }
        public static bool IsStatic(this Type type)
        {
            return type.IsAbstract && type.IsSealed;
        }

        public static bool IsStatic(this PropertyInfo property)
        {
            if (property == null) throw new ArgumentNullException(nameof(property));

            var accessor = property.GetMethod ?? property.SetMethod;
            return accessor != null && accessor.IsStatic;
        }
        public static bool IsStatic(this EventInfo evt)
        {
            if (evt == null) throw new ArgumentNullException(nameof(evt));

            // Check add/remove/raise methods
            var accessor =
                evt.AddMethod ??
                evt.RemoveMethod ??
                evt.RaiseMethod;

            return accessor != null && accessor.IsStatic;
        }
    }
}
