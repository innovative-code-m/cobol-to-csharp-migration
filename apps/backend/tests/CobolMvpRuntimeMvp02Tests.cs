using System;
using System.IO;
using System.Text;
using CobolMvpRuntime;
using Xunit;

namespace CobolToCsharpMigration.Tests;

public class CobolMvpRuntimeMvp02Tests
{
    [Theory]
    [InlineData("00001,TARO YAMADA         ,00123", "00001,YAMADA,TARO,00123")]
    [InlineData("00002,HANAKO SUZUKI       ,04500", "00002,SUZUKI,HANAKO,04500")]
    [InlineData("00003,ICHIRO TANAKA       ,00007", "00003,TANAKA,ICHIRO,00007")]
    [InlineData("00004,TARO yamada         ,00010", "00004,YAMADA,TARO,00010")]
    public void TransformRecord_KnownInputs_ReturnsExpected(string input, string expected)
    {
        Assert.Equal(32, Encoding.ASCII.GetByteCount(input));
        string result = Mvp02Program.TransformRecord(input);
        Assert.Equal(expected, result);
    }

    [Fact]
    public void ProcessFile_Mvp02Sample_MatchesExpectedOutput()
    {
        string backendRoot = GetBackendRoot();
        string repoRoot = Path.GetFullPath(Path.Combine(backendRoot, "..", ".."));

        string inPath = Path.Combine(repoRoot, "docs", "samples", "mvp02", "input", "INFILE.DAT");
        string expectedPath = Path.Combine(repoRoot, "docs", "samples", "mvp02", "expected", "OUTFILE_ideal.DAT");
        string outPath = Path.Combine(Path.GetTempPath(), "mvp02_test_out_" + Guid.NewGuid().ToString("N") + ".dat");

        try
        {
            Mvp02Program.ProcessFile(inPath, outPath);

            string expected = NormalizeNewLine(File.ReadAllText(expectedPath, Encoding.ASCII));
            string actual = NormalizeNewLine(File.ReadAllText(outPath, Encoding.ASCII));
            Assert.Equal(expected, actual);
        }
        finally
        {
            if (File.Exists(outPath))
            {
                File.Delete(outPath);
            }
        }
    }

    [Fact]
    public void Source_DoesNotContainResolvedTodoMarkers()
    {
        string backendRoot = GetBackendRoot();
        string sourcePath = Path.Combine(backendRoot, "src", "CobolMvpRuntime", "Mvp02Program.cs");
        string source = File.ReadAllText(sourcePath, Encoding.UTF8);

        Assert.DoesNotContain("//TODO(MVP02): UNSTRING", source);
        Assert.DoesNotContain("//TODO(MVP02): INSPECT", source);
    }

    private static string NormalizeNewLine(string value)
    {
        return value.Replace("\r\n", "\n");
    }

    private static string GetBackendRoot()
    {
        DirectoryInfo? current = new DirectoryInfo(AppContext.BaseDirectory);
        while (current != null)
        {
            if (File.Exists(Path.Combine(current.FullName, "CobolToCsharpMigration.sln"))
                && Directory.Exists(Path.Combine(current.FullName, "src")))
            {
                return current.FullName;
            }

            current = current.Parent;
        }

        throw new InvalidOperationException("Could not locate backend root directory.");
    }
}
