using System.IO;
using UnityEditor;
using UnityEngine;

public static class AssemblyResolver
{
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
