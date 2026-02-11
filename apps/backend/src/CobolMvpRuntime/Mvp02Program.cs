using System;
using System.IO;
using System.Text;

namespace CobolMvpRuntime
{
    // MVP02 COBOL equivalent:
    // Input  (32 bytes): ID(5) + SEP1(1) + NAME(20) + SEP2(1) + AMOUNT(5)
    // Output (CSV): ID,Last,First,Amount
    internal static class Mvp02Program
    {
        internal static void ProcessFile(string inPath, string outPath)
        {
            using (var reader = new StreamReader(inPath, Encoding.ASCII))
            using (var writer = new StreamWriter(outPath, false, Encoding.ASCII))
            {
                writer.NewLine = "\r\n";
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    int bodyLen = Encoding.ASCII.GetByteCount(line);
                    if (bodyLen != 32)
                    {
                        throw new InvalidOperationException(
                            string.Format("Input record byte length must be 32, but was {0}", bodyLen));
                    }

                    writer.WriteLine(TransformRecord(line));
                }
            }
        }

        internal static string TransformRecord(string input)
        {
            int bodyLen = Encoding.ASCII.GetByteCount(input);
            if (bodyLen != 32)
            {
                throw new InvalidOperationException(
                    string.Format("Input record byte length must be 32, but was {0}", bodyLen));
            }

            string id = input.Substring(0, 5);
            string amount = input.Substring(27, 5);

            //TODO(MVP02): UNSTRING IN-NAME DELIMITED BY SPACE INTO WS-FIRST WS-LAST
            string wsFirst = string.Empty;
            string wsLast = string.Empty;

            //TODO(MVP02): INSPECT WS-LAST CONVERTING lowercase TO uppercase

            return id + "," + wsLast + "," + wsFirst + "," + amount;
        }
    }
}
