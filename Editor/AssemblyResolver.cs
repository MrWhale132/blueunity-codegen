using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using UnityEngine;

#if UNITY_EDITOR
using UnityEditor;
#endif


public static class AssemblyResolver
{
#if UNITY_EDITOR
    /// <summary>
    /// Resolves which assembly a given C# file belongs to.
    /// </summary>
    /// <param name="csFilePath">Absolute path to a .cs file</param>
    /// <returns>Name of the assembly (e.g. "Assembly-CSharp")</returns>
    public static string ResolveAssembly(string csFilePath)
    {
        if (!File.Exists(csFilePath) || Path.GetExtension(csFilePath) != ".cs")
        {
            Debug.LogError($"Not a valid C# file: {csFilePath}");
            return null;
        }

        string projectRoot = Directory.GetParent(Application.dataPath).FullName;
        var dir = Path.GetDirectoryName(csFilePath);

        while (!string.IsNullOrEmpty(dir) && dir.StartsWith(projectRoot))
        {
            // 1. Check for .asmdef
            var asmdefFiles = Directory.GetFiles(dir, "*.asmdef", SearchOption.TopDirectoryOnly);
            if (asmdefFiles.Length > 0)
            {
                //todo: how is this even working? The .asmdef's file name may not match the assembly name inside it.
                return Path.GetFileNameWithoutExtension(asmdefFiles[0]);
            }

            // 2. Check for .asmref
            var asmrefFiles = Directory.GetFiles(dir, "*.asmref", SearchOption.TopDirectoryOnly);
            if (asmrefFiles.Length > 0)
            {
                return GetAssemblyNameFromAsmRef(asmrefFiles[0]);
            }

            // go up
            dir = Path.GetDirectoryName(dir);
        }

        // 3. Fallback → default runtime assembly
        if (csFilePath.Contains("/Editor/") || csFilePath.Contains("\\Editor\\"))
            return "Assembly-CSharp-Editor";
        else
            return "Assembly-CSharp";
    }



    public static string GetAssemblyNameFromAsmRef(string filePath)
    {
        //var asmrefFiles = Directory.GetFiles(dir, "*.asmref", SearchOption.TopDirectoryOnly);
        //if (asmrefFiles.Length == 0)
        //    return null;

        string json = File.ReadAllText(filePath);
        var asmrefData = JsonUtility.FromJson<AsmRefJson>(json);

        if (string.IsNullOrEmpty(asmrefData.reference))
            return null;

        // Check if it's a GUID reference
        if (asmrefData.reference.StartsWith("GUID:", System.StringComparison.OrdinalIgnoreCase) ||
            asmrefData.reference.StartsWith("guid:", System.StringComparison.OrdinalIgnoreCase))
        {
            string guid = asmrefData.reference.Split(':')[1];
            string asmdefPath = AssetDatabase.GUIDToAssetPath(guid);

            if (!string.IsNullOrEmpty(asmdefPath))
            {
                string asmdefJson = File.ReadAllText(asmdefPath);
                var asmdefData = JsonUtility.FromJson<AsmDefJson>(asmdefJson);
                return asmdefData.name;
            }

            Debug.LogWarning($"Could not resolve asmref GUID {guid}");
            return null;
        }

        // Otherwise it’s already the assembly name
        return asmrefData.reference;
    }


    /// <summary>
    /// 
    /// </summary>
    /// <param name="directories">project relative paths without leading or trailing dir separators</param>
    /// <returns></returns>
    public static IEnumerable<AsdmDefInfo> GetAsdmDefInfosInDirs(params string[] directories)
    {
        if (directories == null || directories.Length == 0)
        {
            Debug.LogError("No directories provided to scan for asmdef files. Returning empty array.");
            return Array.Empty<AsdmDefInfo>();
        }

        List<AsdmDefInfo> result = new();

        foreach (string dir in directories)
        {
            if (string.IsNullOrEmpty(dir))
            {

                Debug.LogWarning("Empty directory path provided. Skipping.");
                continue;
            }

            var projectRoot = Directory.GetParent(Application.dataPath).FullName;
            var fullDirPath = Path.Combine(projectRoot, dir);

            if (!Directory.Exists(fullDirPath))
            {
                Debug.LogWarning($"Directory does not exist: {fullDirPath}. Skipping.");
                continue;
            }

            var asmRefPaths = Directory.GetFiles(fullDirPath, "*.asmref", SearchOption.AllDirectories);

            var byName = new Dictionary<string, List<string>>();

            foreach (var path in asmRefPaths)
            {
                var assemblyName = GetAssemblyNameFromAsmRef(path);

                if (string.IsNullOrEmpty(assemblyName))
                    continue;

                if (!byName.ContainsKey(assemblyName))
                {
                    byName[assemblyName] = new List<string>();
                }

                byName[assemblyName].Add(Path.GetDirectoryName(path));
            }



            var asmDefPaths = Directory.GetFiles(fullDirPath, "*.asmdef", SearchOption.AllDirectories);

            foreach (var path in asmDefPaths)
            {
                string asmdefJson = File.ReadAllText(path);
                var asmdefData = JsonUtility.FromJson<AsmDefJson>(asmdefJson);

                var asmDefInfo = new AsdmDefInfo
                {
                    assemblyName = asmdefData.name,
                    directory = Path.GetDirectoryName(path)
                };

                if (byName.TryGetValue(asmdefData.name, out var asmRefDirs))
                {
                    asmDefInfo.asdmRefDirectories = asmRefDirs;
                }

                result.Add(asmDefInfo);
            }
        }

        return result;
    }





    public static string[] EditableSourceFilesDirs = new string[] { "Assets", "Packages" };

    public static AsdmDefInfo GetAsdmDefInfoInDirs(Assembly assembly, params string[] directories)
    {
        var infos = GetAsdmDefInfosInDirs(directories);
        
        var info = infos.FirstOrDefault(i => i.assemblyName == assembly.GetName().Name);

        return info;
    }


    #endif


    public class AsdmDefInfo
    {
        public string assemblyName;
        public string directory;
        public List<string> asdmRefDirectories = new();

        public IEnumerable<string> OwnedDirectories {
            get
            {
                yield return directory;
                foreach (var dir in asdmRefDirectories)
                    yield return dir;
            }
        }
    }


    [System.Serializable]
    public class AsmDefJson
    {
        public string name;
    }

    [System.Serializable]
    private class AsmRefJson
    {
        public string reference;
    }
}
