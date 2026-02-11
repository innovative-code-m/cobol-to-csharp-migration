using System;
using System.IO;
using System.Text;

[assembly: System.Runtime.CompilerServices.InternalsVisibleTo("CobolToCsharpMigration.Tests")]

namespace CobolMvpRuntime
{
    // MVP01 COBOL equivalent: reads fixed-length 40-byte records from INFILE.DAT,
    // computes Total=Qty*UnitPrice and BigFlag (Qty>=100 => 'Y'), writes 60-byte records to OUTFILE.DAT.
    // Encoding: ASCII only. Line endings: CRLF.
    internal class Program
    {
        static void Main(string[] args)
        {
            string inPath = args.Length > 0 ? args[0] : "INFILE.DAT";
            string outPath = args.Length > 1 ? args[1] : "OUTFILE.DAT";
            ProcessFile(inPath, outPath);
        }

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
                    if (bodyLen != 40)
                        throw new InvalidOperationException(
                            string.Format("Input record byte length must be 40, but was {0}", bodyLen));

                    string outLine = TransformRecord(line);

                    int outBodyLen = Encoding.ASCII.GetByteCount(outLine);
                    if (outBodyLen != 60)
                        throw new InvalidOperationException(
                            string.Format("Output record byte length must be 60, but was {0}", outBodyLen));

                    writer.WriteLine(outLine);
                }
            }
        }

        // Input layout  (40 bytes): CUST-ID(5) + NAME(20) + QTY(3) + UNIT-PRICE(5) + FILLER(7)
        // Output layout (60 bytes): CUST-ID(5) + NAME(20) + QTY(3) + UNIT-PRICE(5) + TOTAL(7) + BIG-FLAG(1) + FILLER(19)
        internal static string TransformRecord(string input)
        {
            string custId = input.Substring(0, 5);
            string name = input.Substring(5, 20);
            string qtyStr = input.Substring(25, 3);
            string unitPriceStr = input.Substring(28, 5);

            int qty = int.Parse(qtyStr);
            int unitPrice = int.Parse(unitPriceStr);
            int total = qty * unitPrice;
            string bigFlag = qty >= 100 ? "Y" : "N";

            return custId + name + qtyStr + unitPriceStr + total.ToString("D7") + bigFlag + new string(' ', 19);
        }
    }
}
