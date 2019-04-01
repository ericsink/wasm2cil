using System;

using Mono.Cecil;
using Mono.Cecil.Cil;

namespace cil
{
    class Program
    {
        static void Main(string[] args)
        {
			var myHelloWorldApp = AssemblyDefinition.CreateAssembly(
				new AssemblyNameDefinition("HelloWorld", new Version(1, 0, 0, 0)), "HelloWorld", ModuleKind.Console);

			var module = myHelloWorldApp.MainModule;

			// create the program type and add it to the module
			var programType = new TypeDefinition("HelloWorld", "Program",
				Mono.Cecil.TypeAttributes.Class | Mono.Cecil.TypeAttributes.Public | Mono.Cecil.TypeAttributes.Abstract | Mono.Cecil.TypeAttributes.Sealed, module.TypeSystem.Object);

			module.Types.Add(programType);

			// define the 'Main' method and add it to 'Program'
			var mainMethod = new MethodDefinition("Main",
				Mono.Cecil.MethodAttributes.Public | Mono.Cecil.MethodAttributes.Static, module.TypeSystem.Void);

			programType.Methods.Add(mainMethod);

			// add the 'args' parameter
			var argsParameter = new ParameterDefinition("args",
				Mono.Cecil.ParameterAttributes.None, module.ImportReference(typeof(string[])));

			mainMethod.Parameters.Add(argsParameter);

			// create the method body
			var il = mainMethod.Body.GetILProcessor();

			il.Append(il.Create(OpCodes.Nop));
			il.Append(il.Create(OpCodes.Ldc_I4, 42));
			il.Append(il.Create(OpCodes.Box, module.ImportReference(typeof(int))));
            {
                var writeLineMethod = il.Create(OpCodes.Callvirt,
                    module.ImportReference(typeof(object).GetMethod("ToString", new Type[] { })));
                il.Append(writeLineMethod);
            }
			//il.Append(il.Create(OpCodes.Nop));
			//il.Append(il.Create(OpCodes.Ldstr, "Hola World"));

            {
                var writeLineMethod = il.Create(OpCodes.Call,
                    module.ImportReference(typeof(Console).GetMethod("WriteLine", new[] { typeof(string) })));

                // call the method
                il.Append(writeLineMethod);
            }

			il.Append(il.Create(OpCodes.Nop));
			il.Append(il.Create(OpCodes.Ret));

			// set the entry point and save the module
			myHelloWorldApp.EntryPoint = mainMethod;
            myHelloWorldApp.Write("hello.dll");
        }
    }
}
