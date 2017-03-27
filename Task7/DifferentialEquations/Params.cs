using System;
using Extensions;

namespace DifferentialEquations
{
	public static class Params
	{
		public const string Equation = "y'(x) = - y(x) * (2 - cos(x))";

		public static double Func(double x, double y)
		{
			return -y * (2 - Math.Cos(x));
		}

		public const string ExactSolution = "y(x) = e ^ (sin(x) - 2*x)";

		public static double ExactSolutionFunc(double x)
		{
			return Math.Exp (Math.Sin(x) - 2*x);
		}

		public const string ApproximateSolution = 
			"y(x) = 1 - x + (x ^ 2) / 2 - (x ^ 3) / 3 + 5 * (x ^ 4) / 24 + o(x ^ 5)";

		public static double ApproximateSolutionFunc(double x)
		{
			return 1 - x  + x.Pow(2) / 2 - x.Pow (3) / 3 + 5 * x.Pow (4) / 24;
		}

		public const double h = 0.1;
		public const int N = 10;

		public const double x0 = 0;
	}
}

