using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LinearAlgebra.Helpers
{
    public static class MathHelper
    {
        public static double LegendrePolynomial(double x, int n)
        {
            if (n < 1)
                throw new ArgumentException();
            if (n == 1)
                return 1;

            double p0 = 1;
            double p1 = x;
            double p = 0;

            for (int i = 2; i <= n; i++)
            {
                p = (double)(2 * i - 1) / i * x * p1 - (double)(i - 1) / i * p0;
                p0 = p1;
                p1 = p;
            }
            return p1;
        }

        public static double DerivativeLegendrePolynomial(double x, int n)
        {
            if (n < 1)
                throw new ArgumentException();
            return n / (1 - Math.Pow(x, 2)) * (LegendrePolynomial(x, n - 1) - x * LegendrePolynomial(x, n));
        }

        public static double[] SolveCubicEquation(double a, double b, double c)
        {
            double q = (Math.Pow(a, 2) - 3 * b) / 9;
            double r = (2 * Math.Pow(a, 3) - 9 * a * b + 27 * c) / 54;

            double r2 = Math.Pow(r, 2);
            double q3 = Math.Pow(q, 3);
            if (r2 < q3)
            {
                double t = Math.Acos(r / Math.Sqrt(Math.Pow(q, 3))) / 3;
                return new[]
                {
                    -2 * Math.Sqrt(q) * Math.Cos(t) - a / 3,
                    -2 * Math.Sqrt(q) * Math.Cos(t + 2 * Math.PI / 3) - a / 3,
                    -2 * Math.Sqrt(q) * Math.Cos(t - 2 * Math.PI / 3) - a / 3
                };
            }
            double first = -Math.Sign(r) * Math.Pow(Math.Abs(r) + Math.Sqrt(r2 - q3), 1.0d / 3);
            double second = Math.Abs(first) > float.Epsilon ? q / first : 0;

            double x = (first + second) - a / 3;

            if (Math.Abs(first - second) > float.Epsilon)
                return new[] { x };

            return new[] { x, -first - a / 3 };
        }

        public static double[] SolveQuadraticEquation(double a, double b)
        {
            double discriminant = Math.Pow(a, 2) - 4 * b;
            if (discriminant < 0)
                return new double[0];

            if (Math.Abs(discriminant) < float.Epsilon)
                return new[] { -a / 2 };

            return new[] { (-a + discriminant) / 2, (-a - discriminant) / 2 };
        }

        public static double[] Bisection(double startX, double endX, Func<double, double> function, double epsilon)
        {
            List<Segment> listOfSegments = GetListOfSegments(startX, endX, function);
            double[] segments = new double[listOfSegments.Count];
            int dummyCount;
            double dummyLength;

            for (int i = 0; i < segments.Length; i++)
            {
                segments[i] = Bisection(listOfSegments[i], function, epsilon,
                    out dummyCount, out dummyLength);
            }
            return segments;
        }

        public static double Bisection(Segment sourceSegment, Func<double, double> function, double epsilon,
            out int count, out double length)
        {
            if (function == null)
            {
                throw new ArgumentNullException();
            }
            double startX = sourceSegment.StartX;
            double endX = sourceSegment.EndX;

            length = (endX - startX) / 2;
            count = 1;
            double approximation = startX + length;


            while (!InRange(length, -epsilon, epsilon))
            {
                if (IsDifferentSigns(startX, approximation, function))
                    endX = approximation;
                else
                    startX = approximation;
                length = (endX - startX) / 2;
                approximation = startX + length;
                count++;
            }
            return approximation;
        }

        public static List<Segment> GetListOfSegments(double startX, double endX, Func<double, double> function)
        {
            if (function == null)
            {
                throw new ArgumentNullException();
            }
            if (!Segment.IsValidInterval(startX, endX))
            {
                throw new ArgumentException();
            }
            List<Segment> listOfSegments = new List<Segment>();

            double step = (endX - startX) / 20000;
            double startOfSegment = startX;
            double endOfSegment = startX + step;

            while (InRange(endOfSegment, startX, endX))
            {
                if (IsDifferentSigns(startOfSegment, endOfSegment, function))
                {
                    listOfSegments.Add(Segment.Create(startOfSegment, endOfSegment));
                }
                startOfSegment = endOfSegment;
                endOfSegment += step;
            }
            return listOfSegments;
        }

        private static bool IsDifferentSigns(double first, double second, Func<double, double> function)
        {
            return function(first) * function(second) <= 0;
        }

        private static bool InRange(double x, double startX, double endX)
        {
            return startX < x && x < endX;
        }
    }

    public class Segment
    {
        public double StartX { get; }
        public double EndX { get; }

        public Segment(double startX, double endX)
        {
            StartX = startX;
            EndX = endX;
        }

        public static Segment Create(double startX, double endX)
        {
            if (!IsValidInterval(startX, endX))
            {
                throw new ArgumentException();
            }
            return new Segment(startX, endX);
        }

        public static bool IsValidInterval(double startX, double endX)
        {
            return startX < endX;
        }

        public override string ToString()
        {
            return string.Format("[{0}, {1}]", StartX, EndX);
        }
    }
}
