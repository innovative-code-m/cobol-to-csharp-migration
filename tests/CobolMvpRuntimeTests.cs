using System;
using System.IO;
using System.Text;
using Xunit;
using CobolMvpRuntime;

namespace CobolToCsharpMigration.Tests;

public class CobolMvpRuntimeTests
{
    // Input layout  (40 bytes): CUST-ID(5) + NAME(20) + QTY(3) + UNIT-PRICE(5) + FILLER(7)
    // Output layout (60 bytes): CUST-ID(5) + NAME(20) + QTY(3) + UNIT-PRICE(5) + TOTAL(7) + BIG-FLAG(1) + FILLER(19)

    [Theory]
    [InlineData(
        "00001YAMADA TARO         00500050       ",
        "00001YAMADA TARO         005000500000250N                   ")]
    [InlineData(
        "00002SUZUKI HANAKO       12000010       ",
        "00002SUZUKI HANAKO       120000100001200Y                   ")]
    [InlineData(
        "00003SATO JIRO           10000100       ",
        "00003SATO JIRO           100001000010000Y                   ")]
    public void TransformRecord_KnownInputs_ReturnsExpected(string input, string expected)
    {
        Assert.Equal(40, Encoding.ASCII.GetByteCount(input));
        string result = Program.TransformRecord(input);
        Assert.Equal(expected, result);
        Assert.Equal(60, Encoding.ASCII.GetByteCount(result));
    }

    [Fact]
    public void TransformRecord_QtyExactly100_SetsBigFlagY()
    {
        // QTY=100 is the boundary condition for BigFlag
        string input = "00003SATO JIRO           10000100       ";
        string result = Program.TransformRecord(input);
        Assert.Equal('Y', result[40]);
    }

    [Fact]
    public void TransformRecord_QtyBelow100_SetsBigFlagN()
    {
        string input = "00001YAMADA TARO         00500050       ";
        string result = Program.TransformRecord(input);
        Assert.Equal('N', result[40]);
    }

    [Fact]
    public void ProcessFile_SampleData_MatchesExpected()
    {
        string dir = Path.GetTempPath();
        string inFile = Path.Combine(dir, "mvp01_test_in.dat");
        string outFile = Path.Combine(dir, "mvp01_test_out.dat");
        try
        {
            string[] inputLines =
            {
                "00001YAMADA TARO         00500050       ",
                "00002SUZUKI HANAKO       12000010       ",
                "00003SATO JIRO           10000100       "
            };
            File.WriteAllLines(inFile, inputLines, Encoding.ASCII);

            Program.ProcessFile(inFile, outFile);

            string[] outputLines = File.ReadAllLines(outFile, Encoding.ASCII);
            Assert.Equal(3, outputLines.Length);
            Assert.Equal("00001YAMADA TARO         005000500000250N                   ", outputLines[0]);
            Assert.Equal("00002SUZUKI HANAKO       120000100001200Y                   ", outputLines[1]);
            Assert.Equal("00003SATO JIRO           100001000010000Y                   ", outputLines[2]);
        }
        finally
        {
            if (File.Exists(inFile)) File.Delete(inFile);
            if (File.Exists(outFile)) File.Delete(outFile);
        }
    }

    [Fact]
    public void ProcessFile_InvalidRecordLength_Throws()
    {
        string dir = Path.GetTempPath();
        string inFile = Path.Combine(dir, "mvp01_invalid_in.dat");
        string outFile = Path.Combine(dir, "mvp01_invalid_out.dat");
        try
        {
            File.WriteAllText(inFile, "SHORT\r\n", Encoding.ASCII);
            Assert.Throws<InvalidOperationException>(() => Program.ProcessFile(inFile, outFile));
        }
        finally
        {
            if (File.Exists(inFile)) File.Delete(inFile);
            if (File.Exists(outFile)) File.Delete(outFile);
        }
    }
}
