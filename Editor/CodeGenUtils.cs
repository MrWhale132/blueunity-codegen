
#if UNITY_EDITOR
using Mono.Cecil;
using Mono.Cecil.Cil;
#endif
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.Events;
using GenericParameterAttributes = System.Reflection.GenericParameterAttributes;


namespace Assets._Project.Scripts.UtilScripts.CodeGen
{
    public class CodeGenUtils
    {
        public class Configuration
        {
            public bool IgnoreAnyObsolete;
        }

        public static Configuration CreateDefaultConfig()
        {
            return new Configuration()
            {
                IgnoreAnyObsolete = false,
            };
        }

        public static Configuration _config;
        public static Configuration Config {
            get
            {
                _config ??= CreateDefaultConfig();
                return _config;
            }
            set => _config = value;
        }


        public static string Indent(string csFile)
        {
            int indentCount = 0;

            var lines = csFile.Split(Environment.NewLine);

            var indentedLines = new List<string>();

            foreach (var line in lines)
            {
                if (line.Length == 0)
                {
                    indentedLines.Add("");
                    continue;
                }

                int indentationChange = line.Count(c => c == '{') - line.Count(c => c == '}');

                int currentIndentation = indentationChange < 0 ? indentCount + indentationChange : indentCount;

                var newLine = new string('\t', currentIndentation) + line;

                indentedLines.Add(newLine);

                indentCount += indentationChange;
            }

            string indented = string.Join(Environment.NewLine, indentedLines);

            return indented;
        }






        public static Type ResolveType(string assemblyName, string namespaceName, string typeName)
        {
            if (string.IsNullOrEmpty(assemblyName))
            {
                Debug.LogError("Assembly name is null or empty.");
                return null;
            }
            if (string.IsNullOrEmpty(typeName))
            {
                Debug.LogError("Typename name is null or empty.");
                return null;
            }

            var assembly = AppDomain.CurrentDomain.GetAssemblies()
                .FirstOrDefault(a => a.GetName().Name == assemblyName);

            if (assembly == null)
            {
                Debug.Log($"Assembly '{assemblyName}' not found in current AppDomain.");
                return null;
            }

            string fullName = string.IsNullOrEmpty(namespaceName)
                ? typeName
                : namespaceName + "." + typeName;

            return assembly.GetType(fullName, throwOnError: false);
        }


        public static Type ResolveType(string assemblyName, string fullName)
        {
            if (string.IsNullOrEmpty(assemblyName))
            {
                Debug.LogError("Assembly name is null or empty.");
                return null;
            }
            if (string.IsNullOrEmpty(fullName))
            {
                Debug.LogError("Typename name is null or empty.");
                return null;
            }

            var assembly = AppDomain.CurrentDomain.GetAssemblies()
                .FirstOrDefault(a => a.GetName().Name == assemblyName);

            if (assembly == null)
            {
                Debug.Log($"Assembly '{assemblyName}' not found in current AppDomain.");
                return null;
            }

            return assembly.GetType(fullName, throwOnError: false);
        }






        private static readonly Dictionary<Type, string> _keywords = new()
    {
        { typeof(void), "void" },
        { typeof(bool), "bool" },
        { typeof(byte), "byte" },
        { typeof(sbyte), "sbyte" },
        { typeof(char), "char" },
        { typeof(decimal), "decimal" },
        { typeof(double), "double" },
        { typeof(float), "float" },
        { typeof(int), "int" },
        { typeof(uint), "uint" },
        { typeof(long), "long" },
        { typeof(ulong), "ulong" },
        { typeof(object), "object" },
        { typeof(short), "short" },
        { typeof(ushort), "ushort" },
        { typeof(string), "string" }
    };

        public static string GetSynmbolTokenFromName(Type type)
        {
            // Keyword type?
            if (_keywords.TryGetValue(type, out var keyword))
                return keyword;

            // Nullable<T>
            if (type.IsGenericType && type.GetGenericTypeDefinition() == typeof(Nullable<>))
                return $"{GetSynmbolTokenFromName(type.GetGenericArguments()[0])}?";

            // Array
            if (type.IsArray)
                return $"{GetSynmbolTokenFromName(type.GetElementType())}{new string('[', type.GetArrayRank()).Replace("[", "[]")}";

            // Generic types
            if (type.IsGenericType)
            {
                var genericTypeName = type.Name[..type.Name.IndexOf('`')];
                var genericArgs = string.Join(", ", type.GetGenericArguments().Select(GetSynmbolTokenFromName));
                return $"{genericTypeName}<{genericArgs}>";
            }

            // Fallback
            return type.Name;
        }





        public static List<Type> GetAllTypes()
        {
            var assemblies = AppDomain.CurrentDomain
                .GetAssemblies()
                .Select(a =>
                {
                    try { return (a.FullName, a.GetTypes().Length); }
                    catch (ReflectionTypeLoadException ex) { return ("excp", ex.Types.Where(t => t != null).Count()); }
                });

            assemblies = assemblies.OrderBy(a => a.Item2);

            using var writer = File.CreateText("C:/temp/assemblies.txt");

            foreach (var asm in assemblies)
            {
                writer.WriteLine($"{asm.Item2}\t{asm.Item1}");
            }

            var allTypes = AppDomain.CurrentDomain
                .GetAssemblies()
                .SelectMany(a =>
                {

                    try { return a.GetTypes(); }
                    catch (ReflectionTypeLoadException ex) { return ex.Types.Where(t => t != null); }
                })
                .ToList();
            return allTypes;
        }



        public static string GetMethodSignature(MethodInfo method)
        {
            var genericArity = method.IsGenericMethodDefinition
                ? "<" + string.Join(",", method.GetGenericArguments().Select(a => a.Name)) + ">"
                : "";

            var parameters = string.Join(",",
                method.GetParameters()
                      .Select(p => ToSignatureTypeName(p.ParameterType)));

            var returnType = ToSignatureTypeName(method.ReturnType);

            return $"{method.Name}{genericArity}({parameters}):{returnType}";
        }

        private static string ToSignatureTypeName(Type type)
        {
            // Handle generic type parameter (T, U, etc.)
            if (type.IsGenericParameter)
                return type.Name;

            // Handle byref (ref/in/out)
            if (type.IsByRef)
                return ToSignatureTypeName(type.GetElementType()!) + "&";

            // Handle pointers
            if (type.IsPointer)
                return ToSignatureTypeName(type.GetElementType()!) + "*";

            // Handle arrays
            if (type.IsArray)
                return ToSignatureTypeName(type.GetElementType()!) + "[" + new string(',', type.GetArrayRank() - 1) + "]";

            // Handle constructed generic types
            if (type.IsGenericType)
            {
                var defName = type.GetGenericTypeDefinition().FullName!;
                defName = defName.Substring(0, defName.IndexOf('`')); // strip arity

                var args = type.GetGenericArguments().Select(ToSignatureTypeName);
                return $"{type.Assembly.GetName().Name} {defName}<{string.Join(",", args)}>";
            }

            // Fallback: fully qualified name for non-generic, non-parameter types
            //return type.FullName ?? type.Name;
            return $"{type.Assembly.GetName().Name} {type.FullName ?? type.Name}";
        }




        public static IEnumerable<(MethodInfo method, object target)> GetRuntimeDelegatesFromUnityEvent(UnityEventBase unityEvent)
        {
            return GetRuntimeDelegatesFromUnityEvent(unityEvent, Array.Empty<Type>());
        }
        public static IEnumerable<(MethodInfo method, object target)> GetRuntimeDelegatesFromUnityEvent<T0>(UnityEventBase unityEvent)
        {
            return GetRuntimeDelegatesFromUnityEvent(unityEvent, new Type[] { typeof(T0) });
        }
        public static IEnumerable<(MethodInfo method, object target)> GetRuntimeDelegatesFromUnityEvent<T0, T1>(UnityEventBase unityEvent)
        {
            return GetRuntimeDelegatesFromUnityEvent(unityEvent, new Type[] { typeof(T0), typeof(T1) });
        }
        public static IEnumerable<(MethodInfo method, object target)> GetRuntimeDelegatesFromUnityEvent<T0, T1, T2>(UnityEventBase unityEvent)
        {
            return GetRuntimeDelegatesFromUnityEvent(unityEvent, new Type[] { typeof(T0), typeof(T1), typeof(T2) });
        }
        public static IEnumerable<(MethodInfo method, object target)> GetRuntimeDelegatesFromUnityEvent<T0, T1, T2, T3>(UnityEventBase unityEvent)
        {
            return GetRuntimeDelegatesFromUnityEvent(unityEvent, new Type[] { typeof(T0), typeof(T1), typeof(T2), typeof(T3) });
        }

        public static IEnumerable<(MethodInfo method, object target)> GetRuntimeDelegatesFromUnityEvent(UnityEventBase unityEvent, Type[] argTypes)
        {
            var delegates = new List<(MethodInfo method, object target)>();


            ///persistent listeners have fixed arguments and a <see cref="PersistentListenerMode"/>
            ///since during runtime you cant add persisten listeners you can restore them to how they was set in the inspector originally
            ///thus, for now, I will just say that persistent listeners are not supported.
            ///we lose these persistent calls because we add components with <see cref="Component.GetComponent{T}"/>
            ///and not instantiating them via prefabs, which would have them.


            // 1. Persistent (Inspector) listeners
            //for (int i = 0; i < unityEvent.GetPersistentEventCount(); i++)
            //{
            //    var target = unityEvent.GetPersistentTarget(i);
            //    var methodName = unityEvent.GetPersistentMethodName(i);

            //    //though target can be null for statics, they can not be used in inspector
            //    if (target != null && !string.IsNullOrEmpty(methodName))
            //    {
            //        var methodInfo = UnityEventBase.GetValidMethodInfo(target, methodName, argTypes);

            //        if (methodInfo != null)
            //        {
            //            //Debug.Log("method found: "+methodInfo.Name);
            //            delegates.Add((methodInfo, target));
            //        }
            //    }
            //}

            // 2.
            var callsField = typeof(UnityEventBase).GetField("m_Calls",
                BindingFlags.Instance | BindingFlags.NonPublic);
            var invokableCallList = callsField.GetValue(unityEvent);

            //GetDelegatesFromCallList("m_PersistentCalls");
            GetDelegatesFromCallList("m_RuntimeCalls");

            return delegates;


            void GetDelegatesFromCallList(string fieldName)
            {
                var runtimeCallsField = invokableCallList.GetType().GetField(fieldName,
                BindingFlags.Instance | BindingFlags.NonPublic);
                var runtimeCalls = runtimeCallsField.GetValue(invokableCallList) as System.Collections.IList;

                foreach (var invokable in runtimeCalls)
                {
                    var delegateField = invokable.GetType().GetField("Delegate",
                        BindingFlags.Instance | BindingFlags.NonPublic);
                    if (delegateField != null)
                    {
                        if (delegateField.GetValue(invokable) is Delegate del)
                            delegates.Add((del.Method, del.Target));
                    }
                }
            }
        }





        /// <summary>
        /// Returns all delegates (persistent + runtime) currently bound to a UnityEvent.
        /// </summary>
        public static IEnumerable<Delegate> GetAllDelegates(UnityEventBase unityEvent)
        {
            // 1. Persistent (Inspector) listeners
            for (int i = 0; i < unityEvent.GetPersistentEventCount(); i++)
            {
                var target = unityEvent.GetPersistentTarget(i);
                var methodName = unityEvent.GetPersistentMethodName(i);

                if (target != null && !string.IsNullOrEmpty(methodName))
                {
                    var mi = target.GetType().GetMethod(methodName,
                        BindingFlags.Instance | BindingFlags.Static |
                        BindingFlags.Public | BindingFlags.NonPublic);

                    if (mi != null)
                    {
                        yield return Delegate.CreateDelegate(
                            typeof(UnityAction), target, mi, false
                        );
                    }
                }
            }

            // 2. Runtime (AddListener) listeners
            var callsField = typeof(UnityEventBase).GetField("m_Calls",
                BindingFlags.Instance | BindingFlags.NonPublic);
            var invokableCallList = callsField?.GetValue(unityEvent);

            if (invokableCallList == null) yield break;

            var runtimeCallsField = invokableCallList.GetType().GetField("m_RuntimeCalls",
                BindingFlags.Instance | BindingFlags.NonPublic);
            var runtimeCalls = runtimeCallsField?.GetValue(invokableCallList) as System.Collections.IList;

            if (runtimeCalls == null) yield break;

            foreach (var invokable in runtimeCalls)
            {
                var delegateField = invokable.GetType().GetField("Delegate",
                    BindingFlags.Instance | BindingFlags.NonPublic);
                if (delegateField != null)
                {
                    var del = delegateField.GetValue(invokable) as Delegate;
                    if (del != null)
                        yield return del;
                }
            }
        }





        public static string GetGenericParameterListText(Type type)
        {
            IEnumerable<string> typeArgNames = type.IsGenericType ?
                type.GetGenericTypeDefinition().GetGenericArguments().Select(arg => arg.Name)
                : Enumerable.Empty<string>();

            var genericParameterList = type.IsGenericType ? "<" + string.Join(", ", typeArgNames) + ">" : "";

            return genericParameterList;
        }



        public static string GetGenericParameterConstraintsText(Type type)
        {
            if (type == null) throw new ArgumentNullException(nameof(type));
            if (!type.IsGenericType)
                return string.Empty;

            if (!type.IsGenericTypeDefinition)
                type = type.GetGenericTypeDefinition();


            var sb = new StringBuilder();
            var parameters = type.GetGenericArguments();

            foreach (var p in parameters)
            {
                var constraints = p.GetGenericParameterConstraints();
                var attrs = p.GenericParameterAttributes;

                var parts = new System.Collections.Generic.List<string>();

                // Special constraints
                var variance = attrs & GenericParameterAttributes.SpecialConstraintMask;
                if ((variance & GenericParameterAttributes.ReferenceTypeConstraint) != 0)
                    parts.Add("class");
                if ((variance & GenericParameterAttributes.NotNullableValueTypeConstraint) != 0)
                    parts.Add("struct");

                // Add constraint types
                foreach (var c in constraints)
                {
                    if (c == typeof(System.ValueType)) continue;
                    parts.Add(GetTypeName(c));
                }

                // "new()" constraint must come last if present
                if ((variance & GenericParameterAttributes.DefaultConstructorConstraint) != 0 &&
                    (variance & GenericParameterAttributes.NotNullableValueTypeConstraint) == 0)
                {
                    parts.Add("new()");
                }

                if (parts.Count > 0)
                {
                    sb.Append("where ");
                    sb.Append(p.Name);
                    sb.Append(" : ");
                    sb.Append(string.Join(", ", parts));
                    sb.AppendLine();
                }
            }

            return sb.ToString().TrimEnd();
        }

        private static string GetTypeName(Type t)
        {
            if (t.IsGenericType)
            {
                var defName = t.Name;
                var tick = defName.IndexOf('`');
                if (tick >= 0)
                    defName = defName.Substring(0, tick);

                var args = t.GetGenericArguments().Select(GetTypeName);
                return $"{t.FullName}<{string.Join(", ", args)}>";
            }

            return t.FullName ?? t.Name;
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
                            Debug.Log(type.CleanAssemblyQualifiedName() ?? type.Name ?? type.FullName ?? type.AssemblyQualifiedName);
                            _debug = true;
                            Debug.Log(GetOwnGenericArityFromName(t.Name));
                            Debug.Log(ownArity);
                            Debug.Log(args.Length);
                            Debug.Log(type.IsGenericType);
                            Debug.Log(type.GenericTypeArguments.Length);
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

            if (_debug) Debug.Log(name);

            // 3. Look for the `n arity marker
            int backtick = name.IndexOf('`');
            if (backtick < 0)
                return 0;

            var numPart = name.Substring(backtick + 1);
            return int.TryParse(numPart, out int n) ? n : 0;
        }






        /// <summary>
        /// Creates a flattened custom type name, e.g. "Outer_OuterGen_Innner_InnerGen".
        /// </summary>
        public static string ToFlatName(Type type)
        {
            var sb = new StringBuilder();
            AppendFlatName(sb, type);
            return sb.ToString();
        }

        private static void AppendFlatName(StringBuilder sb, Type type)
        {
            if (type.DeclaringType != null)
            {
                AppendFlatName(sb, type.DeclaringType);
                sb.Append("_");
            }
            else if (!string.IsNullOrEmpty(type.Namespace))
            {
                // Optionally include namespace — if you want, use sb.Append(type.Namespace.Replace('.', '_') + "_");
                // Leaving it out matches your example.
            }

            var name = type.Name;
            var tickIndex = name.IndexOf('`');
            if (tickIndex >= 0)
            {
                name = name.Substring(0, tickIndex);
            }

            sb.Append(name);
        }








        public static string GenerateGetMethodCode(MethodInfo method, bool withNameSpace = true)
        {
            var declaringType = method.DeclaringType!;
            string typeExpr = ToTypeDefinitionText(declaringType, withNameSpace);

            string flags = "BindingFlags.Public | BindingFlags.Instance";
            if (method.IsStatic) flags = "BindingFlags.Public | BindingFlags.Static";
            if (!method.IsPublic) flags = "BindingFlags.NonPublic | " + (method.IsStatic ? "BindingFlags.Static" : "BindingFlags.Instance");

            string methodNameExpr = $"nameof({ToTypeReferenceText(declaringType, withNameSpace)}.{method.Name})";

            // Build parameter type expressions
            var paramExprs = method.GetParameters()
                                   .Select(p => GetTypeExpr(p.ParameterType))
                                   .ToArray();

            string paramList = paramExprs.Length > 0
                ? $"new Type[] {{ {string.Join(", ", paramExprs)} }}"
                : "Type.EmptyTypes";

            return $"typeof({typeExpr}).GetMethod({methodNameExpr}, {flags}, null, {paramList}, null)";




            string GetTypeExpr(Type t)
            {
                if (t.HasElementType)
                {
                    string makeType = t.IsSZArray ? "MakeArrayType()" : t.IsArray ? $"MakeArrayType({t.GetArrayRank()})"
                                  : t.IsByRef ? "MakeByRefType()" : "MakePointerType()";

                    return $"{GetTypeExpr(t.GetElementType()!)}.{makeType}";
                }

                if (t.IsGenericType)
                {
                    var def = t.GetGenericTypeDefinition();
                    var genericArgs = t.GetGenericArguments().Select(GetTypeExpr).ToArray();

                    return $"typeof({ToTypeDefinitionText(def, withNameSpace)}).MakeGenericType({string.Join(", ", genericArgs)})";
                }

                if (t.IsGenericParameter)
                {
                    // Class-level generic parameter
                    if (t.DeclaringMethod == null)
                        return $"args[{t.GenericParameterPosition}]";
                    // Method-level generic parameter
                    return $"Type.MakeGenericMethodParameter({t.GenericParameterPosition})";
                }


                return $"typeof({ToTypeDefinitionText(t, withNameSpace)})";
            }
        }



        #region Tests
        int field;
        int field2;
        public int Prop1 { get; set; }
        public int Prop2 { get => field; set => field = value; }
        public int Prop3 { get { return field; } set => field = value; }
        public int prop4 { get; }
        public int prop9 { set { } }
        public int prop5 { set => field = value; }
        public int prop6 { get => field2; set => field = value; }
        public int prop7 { get { var a = field2 * 3; return field; } set => field = value; }
        public int prop8 { get { var a = field2 * 3; return field; } set { var a = field * 3; field = value; } }

        #endregion


        //todo: move these stuff into a seperate editor only class
#if UNITY_EDITOR



        public static IEnumerable<PropertyInfo> GetSimpleProperties(Type type, BindingFlags binding)
        {
            var foundProps = new List<PropertyInfo>();

            var asm = AssemblyDefinition.ReadAssembly(type.Assembly.Location);

            string lookupName = type.IsGenericType
                            ? type.GetGenericTypeDefinition().FullName
                            : type.FullName;


            if (string.IsNullOrEmpty(lookupName))
            {
                Debug.LogError(type.AssemblyQualifiedName);
                return Array.Empty<PropertyInfo>();
            }


            var propDefByNameLookUp = new Dictionary<string, PropertyDefinition>();

            Type current = type;


            while (current != null)
            {
                current = current.IsGenericType ? current.GetGenericTypeDefinition() : current;

                var td = GetCecilTypeDefinition(current);

                if (td == null)
                {
                    Debug.LogError("null typedef: " + current.CleanAssemblyQualifiedName() ?? current.FullName ?? current.Name);
                    //Debug.Log(type.Name);
                    //Debug.Log(current.Name);
                }
                else
                    foreach (var prop in td.Properties)
                    {
                        if (!propDefByNameLookUp.ContainsKey(prop.Name))
                        {
                            propDefByNameLookUp.Add(prop.Name, prop);
                        }
                    }

                current = current.BaseType;
            }



            var propInfos = type.GetProperties(binding);

            foreach (var prop in propInfos)
            {
                var propDef = propDefByNameLookUp[prop.Name];

                if (propDef == null || (Config.IgnoreAnyObsolete && prop.IsDefined(typeof(ObsoleteAttribute)))) continue;


                if (IsPublicAutoProperty(prop) || IsWrapperProperty(propDef)
                || (typeof(UnityEngine.Object).IsAssignableFrom(type) && IsUnityInjectedExtern(propDef)))
                {
                    foundProps.Add(prop);
                }
            }

            return foundProps;
        }



        /// <summary>
        /// This method tries to detect if a property calls to extern methods or not.
        /// </summary>
        /// <param name="prop"></param>
        /// <returns></returns>
        private static bool IsUnityInjectedExtern(PropertyDefinition prop)
        {
            if (prop.GetMethod == null || prop.SetMethod == null) return false;
            if (!prop.GetMethod.IsPublic || !prop.SetMethod.IsPublic) return false;
            if (!prop.GetMethod.HasBody || !prop.SetMethod.HasBody) return false;


            //patterns for extern method names known so far
            //get_{prop.Name}_Injected, internal_get{prop.Name}
            var possibleGetterNamePatterns = new[] { $"get_{prop.Name}_", $"_get{prop.Name}" };
            var possibleSetterNamePatterns = new[] { $"set_{prop.Name}_", $"_set{prop.Name}" };


            var calls = GetMethodCallsInside(prop.GetMethod);

            //is there any call to a method that matches any of the patterns?
            bool callsExternMethod = calls.Any(method => possibleGetterNamePatterns.Any(pattern => method.Name.Contains(pattern)));

            if (!callsExternMethod) return false;


            calls = GetMethodCallsInside(prop.SetMethod);

            callsExternMethod = calls.Any(method => possibleSetterNamePatterns.Any(pattern => method.Name.Contains(pattern)));

            if (!callsExternMethod) return false;

            return true;
        }

        private static List<MethodReference> GetMethodCallsInside(MethodDefinition method)
        {
            if (!method.HasBody) return null;

            var mrs = new List<MethodReference>();

            var instrs = method.Body.Instructions;

            // look for a call to the injected extern
            foreach (var instr in instrs)
            {
                if (instr.OpCode == OpCodes.Call || instr.OpCode == OpCodes.Callvirt)
                {
                    if (instr.Operand is MethodReference mr)
                    {
                        mrs.Add(mr);
                    }
                }
            }

            return mrs;
        }



        public static bool IsPublicAutoProperty(PropertyInfo prop)
        {
            var getMethod = prop.GetGetMethod();
            var setMethod = prop.GetSetMethod();

            if (getMethod == null || setMethod == null)
                return false; // must have both accessors

            //Debug.Log(0);

            if (!getMethod.IsPublic || !setMethod.IsPublic)
                return false; // both must be public

            //Debug.Log(1);

            if (getMethod.IsExtern() || setMethod.IsExtern())
                return true;

            //Debug.Log(2);

            // auto-property has compiler-generated backing field
            var backingFieldName = $"<{prop.Name}>k__BackingField";
            var backingField = prop.DeclaringType
                                   .GetField(backingFieldName, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);

            //Debug.Log(backingField != null);
            //Debug.Log(backingField?.IsDefined(typeof(CompilerGeneratedAttribute), false));

            return backingField != null && backingField.IsDefined(typeof(CompilerGeneratedAttribute), false);
        }




        private static bool IsWrapperProperty(PropertyDefinition propDef)
        {
            var getter = propDef.GetMethod;
            var setter = propDef.SetMethod;
            FieldReference backingField = null;

            if (getter != null && getter.HasBody && getter.IsPublic)
            {
                var instrs = getter.Body.Instructions;
                if (instrs.Count == 3 &&
                    instrs[0].OpCode == OpCodes.Ldarg_0 &&
                    instrs[1].OpCode == OpCodes.Ldfld &&
                    instrs[2].OpCode == OpCodes.Ret)
                {
                    backingField = (FieldReference)instrs[1].Operand;
                }
                else return false;
            }
            else return false;

            if (setter != null && setter.HasBody && setter.IsPublic)
            {
                var instrs = setter.Body.Instructions;
                if (instrs.Count == 4 &&
                    instrs[0].OpCode == OpCodes.Ldarg_0 &&
                    instrs[1].OpCode == OpCodes.Ldarg_1 &&
                    instrs[2].OpCode == OpCodes.Stfld &&
                    instrs[3].OpCode == OpCodes.Ret)
                {
                    if (backingField != null && instrs[2].Operand != backingField)
                        return false; // getter/setter mismatch
                }
                else return false;
            }
            else return false;

            return true;
        }









        public static bool _debug = false;

        public static TypeDefinition GetCecilTypeDefinition(Type type)
        {
            if (type == null)
            {
                //Debug.LogError("null type");
                return null;
            }

            string assemblyPath = type.Assembly.Location;
            var visited = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

            return ResolveTypeRecursive(type, assemblyPath, visited);
        }

        private static TypeDefinition ResolveTypeRecursive(Type type, string assemblyPath, HashSet<string> visited)
        {
            if (_debug) Debug.Log("0");

            if (string.IsNullOrEmpty(assemblyPath) || !File.Exists(assemblyPath))
                return null;

            if (_debug) Debug.Log("1");

            if (visited.Contains(assemblyPath))
                return null; // prevent cycles
            visited.Add(assemblyPath);

            if (_debug) Debug.Log("2");

            var asm = AssemblyDefinition.ReadAssembly(assemblyPath);
            string cecilName = CecilNormalizeTypeName(type);


            if (_debug) Debug.Log(cecilName);

            // 1. Try direct lookup
            var td = FindTypeRecursive(asm.MainModule.Types, cecilName);
            if (td != null)
                return td;

            if (_debug) Debug.Log("3");

            // 2. Try type forwarders
            foreach (var forwarder in asm.MainModule.ExportedTypes)
            {
                if (forwarder.FullName == cecilName)
                {
                    try
                    {
                        var resolved = forwarder.Resolve();
                        if (resolved != null)
                            return resolved;
                    }
                    catch
                    {
                        // ignore and continue
                    }
                }
            }
            if (_debug) Debug.Log("4");

            // 3. Try dependencies
            foreach (var reference in asm.MainModule.AssemblyReferences)
            {
                try
                {
                    var depAsm = AssemblyResolver.Resolve(reference);
                    var depPath = depAsm.MainModule.FileName;
                    var result = ResolveTypeRecursive(type, depPath, visited);
                    if (result != null)
                        return result;
                }
                catch
                {
                    // ignore missing refs
                }
            }
            if (_debug) Debug.LogWarning("5");

            return null;
        }

        private static string CecilNormalizeTypeName(Type type)
        {
            string name = type.FullName;
            if (name == null)
                return null;

            // Nested types: reflection uses "+", Cecil uses "/"
            name = name.Replace('+', '/');

            // Strip generic args, keep arity
            int genericMarker = name.IndexOf('[');
            if (genericMarker >= 0)
                name = name.Substring(0, genericMarker);

            return name;
        }


        public static string ReflectionNormalizeTypeName(TypeReference type)
        {
            if (type == null)
                return null;

            string name = type.FullName;
            if (name == null)
                return null;

            //string name = type.Namespace != null && type.Namespace.Length > 0
            //    ? type.Namespace + "." + type.Name
            //    : type.Name;

            name = name.Replace('/', '+');

            //lets look at first with out this
            //if (type is GenericInstanceType git && git.GenericArguments.Count > 0)
            //{
            //    string args = string.Join(",", git.GenericArguments.Select(ReflectionNormalizeTypeName));
            //    name += $"[{args}]";
            //}

            return name;

        }


        public static Type ResolveType(TypeDefinition typeDef)
        {
            if (typeDef == null) return null;

            string declaredIn = typeDef.Module.Assembly.Name.Name;
            return ResolveType(declaredIn, ReflectionNormalizeTypeName(typeDef));
        }

        public static Type ResolveType(TypeReference typeRef)
        {
            if (typeRef == null) return null;


            string declaredIn = typeRef.Scope switch
            {
                AssemblyNameReference asmRef => asmRef.Name,
                ModuleDefinition modDef => modDef.Assembly?.Name?.Name ?? modDef.Name,
                ModuleReference modRef => modRef.Name,
                _ => "<unknown>"
            };

            return ResolveType(declaredIn, ReflectionNormalizeTypeName(typeRef));
        }


        private static TypeDefinition FindTypeRecursive(
            Mono.Collections.Generic.Collection<TypeDefinition> types,
            string fullName)
        {
            foreach (var t in types)
            {
                if (t.FullName == fullName)
                    return t;

                var nested = FindTypeRecursive(t.NestedTypes, fullName);
                if (nested != null)
                    return nested;
            }
            return null;
        }

        // Simple resolver that searches in loaded assembly directories
        private static readonly DefaultAssemblyResolver AssemblyResolver = CreateResolver();

        private static DefaultAssemblyResolver CreateResolver()
        {
            var resolver = new DefaultAssemblyResolver();

            // Add runtime dirs (mscorlib/System.Private.CoreLib/etc.)
            foreach (var dir in GetRuntimeSearchPaths())
                resolver.AddSearchDirectory(dir);

            return resolver;
        }

        private static IEnumerable<string> GetRuntimeSearchPaths()
        {
            // Typical .NET runtime paths
            yield return Path.GetDirectoryName(typeof(object).Assembly.Location);

            // Unity specific: Managed folder (if running in Unity)
            var unityPath = Path.Combine(Directory.GetCurrentDirectory(), "Library", "ScriptAssemblies");
            if (Directory.Exists(unityPath))
                yield return unityPath;
        }










        /// <summary>
        /// Returns a set of types (TypeDefinition when resolvable, else the TypeReference.FullName string)
        /// that are the target of any static member access found in the module.
        /// </summary>
        public class StaticAccessResult
        {
            public HashSet<TypeDefinition> ResolvedTypes { get; } = new HashSet<TypeDefinition>();
            public HashSet<string> UnresolvedTypeNames { get; } = new HashSet<string>();
        }

        public static StaticAccessResult GetStaticlyReferencedTypes(TypeDefinition type)
        {
            //too slow and has a lot of false positives, needs fixes
            //return new StaticAccessResult() ;
            d_currentTypeDef = type;

            var result = new StaticAccessResult();

            //foreach (var type in GetAllTypes(module))
            {
                foreach (var method in type.Methods)
                {
                    if (!method.HasBody) continue;
                    var body = method.Body;

                    //Debug.Log("Inspecting method: " + method.FullName + " in type " + type.FullName);

                    foreach (var instr in body.Instructions)
                    {
                        // We only care about operands that are MethodReference or FieldReference.
                        if (instr.Operand is MethodReference methodRef)
                        {
                            if (IsMethodReferenceStatic(methodRef))
                            {
                                AddDeclaringType(methodRef.DeclaringType, result);
                            }
                        }
                        else if (instr.Operand is FieldReference fieldRef)
                        {
                            if (IsFieldReferenceStatic(fieldRef))
                            {
                                AddDeclaringType(fieldRef.DeclaringType, result);
                            }
                        }
                        //else if (instr.Operand is Mono.Cecil.CallSite callSite)
                        {
                            // CallSite can reference function pointer like constructs — skip or inspect
                            // If you care, you can inspect callSite and its ReturnType / Parameters.
                        }
                        // Note: property and event accesses compile down to MethodReference (get_/set_/add_/remove_)
                    }
                }
            }

            return result;
        }

        public static TypeDefinition d_currentTypeDef;


        //static IEnumerable<TypeDefinition> GetAllTypes(ModuleDefinition module)
        //{
        //    foreach (var t in module.Types)
        //    {
        //        foreach (var nested in FlattenType(t))
        //            yield return nested;
        //    }
        //}

        //static IEnumerable<TypeDefinition> FlattenType(TypeDefinition type)
        //{
        //    yield return type;
        //    foreach (var nt in type.NestedTypes)
        //        foreach (var deeper in FlattenType(nt))
        //            yield return deeper;
        //}

        static bool IsMethodReferenceStatic(MethodReference methodRef)
        {
            // Preferred: resolve to definition to get definitive IsStatic.
            try
            {
                //var asm = AssemblyDefinition.ReadAssembly(methodRef.Module.FileName);
                //var def = methodRef.Resolve();
                var typeDef = ResolveCecilTypeReferenceToTypeDefinition(methodRef.DeclaringType);

                if (typeDef != null)
                {
                    var methods = typeDef.Methods.Where(m => m.Name == methodRef.Name && m.Parameters.Count == methodRef.Parameters.Count);

                    if (methods != null) return methods.Any(m => m.IsStatic);
                }
                else
                {
                    //Debug.LogWarning("Failed to resolve method: " + methodRef.FullName +
                    //    " in type " + methodRef.DeclaringType.FullName + " for type " + d_currentTypeDef.FullName);
                }
            }
            catch (Exception ex)
            {
                // resolution may fail if the type is in an unresolved assembly; fall back below
                Debug.LogException(ex);
                Debug.LogWarning("Failed to resolve method: " + methodRef.FullName +
                    " in type " + methodRef.DeclaringType.FullName + " for type " + d_currentTypeDef.FullName);
            }

            // Fallback: If HasThis == false the method is (probably) static.
            // This is not 100% foolproof for some corner cases (e.g. explicit instance calling conventions),
            // but it is the practical fallback.
            return !methodRef.HasThis;
        }

        static bool IsFieldReferenceStatic(FieldReference fieldRef)
        {
            try
            {
                var def = ResolveCecilTypeReferenceToTypeDefinition(fieldRef.DeclaringType)?
                              .Fields.FirstOrDefault(f => f.Name == fieldRef.Name);

                if (def != null) return def.IsStatic;
                else
                {
                    //Debug.LogWarning("Failed to resolve field: " + fieldRef.FullName +
                    //    " in " + fieldRef.DeclaringType.FullName);
                }
            }
            catch (Exception ex)
            {
                // resolution failed
                Debug.LogException(ex);
                Debug.LogWarning("Failed to resolve field: " + fieldRef.FullName + " in " + d_currentTypeDef.FullName);
            }

            // No FieldReference.IsStatic available without resolution.
            // Best we can do is attempt to inspect the instruction's opcode in caller, but here we assume resolve required.
            // We'll return false here so unresolved fields do not produce false positives.
            // If you want to treat unresolved field refs as "possible static", return true instead.
            return false;
        }

        static void AddDeclaringType(TypeReference declaringTypeRef, StaticAccessResult result)
        {
            if (declaringTypeRef == null) return;

            try
            {
                var def = ResolveCecilTypeReferenceToTypeDefinition(declaringTypeRef);

                if (def != null)
                {
                    result.ResolvedTypes.Add(def);
                    return;
                }
                else
                {
                    //Debug.LogWarning("Failed to resolve type: " + declaringTypeRef.FullName);
                }
            }
            catch (Exception ex)
            {
                // resolution failed
                Debug.LogException(ex);
                Debug.LogWarning("Failed to resolve type: " + declaringTypeRef.FullName);
            }

            // If we can't resolve to a TypeDefinition (external assembly not available),
            // remember the name so caller still knows which type was targeted.
            result.UnresolvedTypeNames.Add(declaringTypeRef.FullName);
        }


        public static TypeDefinition ResolveCecilTypeReferenceToTypeDefinition(TypeReference typeRef)
        {
            ///<see cref="TypeReference.Module"/> is pointing to the assemby where it is USED!!! , not where it is defined.
            //var asm = AssemblyDefinition.ReadAssembly(typeRef.Module.FileName);
            //var def = asm.MainModule.GetType(typeRef.FullName);

            //so here is the workaround

            string declaredIn = typeRef.Scope switch
            {
                AssemblyNameReference asmRef => asmRef.Name,
                ModuleDefinition modDef => modDef.Assembly?.Name?.Name ?? modDef.Name,
                ModuleReference modRef => modRef.Name,
                _ => "<unknown>"
            };

            var type = ResolveType(typeRef);

            //if (_debug)
            {
                if (type == null)
                {
                    //Debug.LogError("null type for " + typeRef.FullName + " in assembly " + declaredIn);
                }
            }

            var def = GetCecilTypeDefinition(type);

            return def;
        }



#endif
    }






    public static class TypeExtensions
    {
        public static bool IsExtern(this PropertyInfo property)
        {
            return property != null &&
                   (property.GetMethod.IsExtern() || property.SetMethod.IsExtern());
        }
        public static bool IsExtern(this MethodInfo m)
        {
            return m != null &&
                   !m.IsAbstract &&
                   m.GetMethodBody() == null;
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

        public static bool IsAssignableTo(this Type type, Type targetType)
        {
            return targetType.IsAssignableFrom(type);
        }

        public static bool IsPublic(this Type type)
        {
            if (type.IsNested)
                return type.IsNestedPublic;
            else
                return type.IsPublic;
        }


#if UNITY_EDITOR
        public static bool IsPublic(this Mono.Cecil.TypeDefinition type)
        {
            if (type.IsNested)
                return type.IsNestedPublic;
            else
                return type.IsPublic;
        }

        public static bool IsCompileTimePublic(this TypeDefinition type)
        {
            // Check the type itself
            if (type.IsPublic || type.IsNestedPublic)
            {
                // If it's nested, we must ensure all its parents are also accessible
                if (type.IsNested)
                {
                    var parent = type.DeclaringType;
                    return parent != null && parent.IsCompileTimePublic();
                }
                return true; // top-level public type
            }

            return false;
        }
#endif


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


        public static bool IsAssignableToGenericTypeDefinition(this Type type, Type genericType)
        {
            if (!genericType.IsGenericTypeDefinition)
                throw new ArgumentException("genericBase must be a generic type definition");

            // Walk the inheritance chain
            if (genericType.IsInterface)
            {
                while (type != null && type != typeof(object))
                {
                    var interfaces = type.GetInterfaces();
                    if (interfaces.Any(i => (i.IsGenericType && i.GetGenericTypeDefinition() == genericType)))
                        return true;

                    type = type.BaseType;
                }
            }
            else
            {
                while (type != null && type != typeof(object))
                {
                    if (type.IsGenericType && type.GetGenericTypeDefinition() == genericType)
                        return true;

                    type = type.BaseType;
                }
            }

            return false;
        }




        public static IEnumerable<MethodInfo> GetUsableMethods(this Type type, BindingFlags binding)
        {
            return type.GetMethods(binding)
                .Where(m =>
                    !m.IsSpecialName &&                                 // skip property/event accessors & operators
                    !m.IsDefined(typeof(CompilerGeneratedAttribute), false) && // skip compiler generated
                    !m.GetCustomAttributes(typeof(ObsoleteAttribute), false).Any() && // optionally skip obsolete
                    !m.Name.StartsWith("<")                             // skip async/iterator state machine methods
                );
        }



        public static string CleanAssemblyQualifiedName(this Type type)
        {
            if (type == null) return null;

            var assemblyTypeName = type.AssemblyQualifiedName;
            if (string.IsNullOrEmpty(assemblyTypeName)) return assemblyTypeName;

            // Remove Version=..., Culture=..., PublicKeyToken=... tokens (case-insensitive)
            assemblyTypeName = Regex.Replace(assemblyTypeName, @",\s*Version=[^,\]]*", "", RegexOptions.IgnoreCase);
            assemblyTypeName = Regex.Replace(assemblyTypeName, @",\s*Culture=[^,\]]*", "", RegexOptions.IgnoreCase);
            assemblyTypeName = Regex.Replace(assemblyTypeName, @",\s*PublicKeyToken=[^,\]]*", "", RegexOptions.IgnoreCase);

            // Collapse any accidental double-commas produced by removals (", ,") into a single ","
            assemblyTypeName = Regex.Replace(assemblyTypeName, @",\s*,\s*", ", ");

            return assemblyTypeName;
        }
    }
}
