using System;
using Extensions;

namespace DifferentialEquations
{
	static class MainClass
	{
		public static void Main (string[] args)
		{
			Console.WriteLine ("Уравнение:");
			Console.WriteLine (Params.Equation);
			Console.WriteLine ();
			Console.WriteLine ("x0 = 0");
			Console.WriteLine ();
			int n = GetN ();
			Console.WriteLine ();
			double h = GetH ();
			Console.WriteLine (	);

			double[,] exactTable = GenerateTable(0, -2, n, h, Params.ExactSolutionFunc);
			Console.WriteLine ("Точное решение:");
			Console.WriteLine (Params.ExactSolution);
			Console.WriteLine ();
			Console.WriteLine ("Таблица значений точного решения");
			Console.WriteLine (exactTable.ConvertToString());
			Console.WriteLine ();

			double y = exactTable [exactTable.GetHeight () - 1, 1];

			double[,] approximateTable = GenerateTable (0, -2, 2, h, Params.ApproximateSolutionFunc);
			Console.WriteLine ("Приближенное решение методом тейлора");
			Console.WriteLine (Params.ApproximateSolution);
			Console.WriteLine ();
			Console.WriteLine ("Таблица значений приближенного решения решения");
			Console.WriteLine (approximateTable.ConvertToString());
			Console.WriteLine ();

			double[,] discrepancyTable = GenerateTable(0, -2,2,h,
				(x) => Math.Abs(Params.ApproximateSolutionFunc(x) - Params.ExactSolutionFunc(x)));
			Console.WriteLine ("Таблица значений абсолютных погрешностей");
			Console.WriteLine (discrepancyTable.ConvertToString());
			Console.WriteLine ();

			double[,] euler = DifferentialEquationSolver.Euler (0, 1, Params.Func, h, n);
			Console.WriteLine ("Taблица приближенных значений полученных методом Эйлера");
			Console.WriteLine (euler.ConvertToString());
			Console.WriteLine ();
			Console.WriteLine ($"|Y(Xn) - Yn| = {Math.Abs(y - euler[n, 1])}");
			Console.WriteLine ();

			double[,] improvedEuler = DifferentialEquationSolver.ImprovedEuler (0, 1, Params.Func, h, n);
			Console.WriteLine ("Taблица приближенных значений полученных улучшенным методом Эйлера");
			Console.WriteLine (improvedEuler.ConvertToString());
			Console.WriteLine ();
			Console.WriteLine ($"|Y(Xn) - Yn| = {Math.Abs(y - improvedEuler[n, 1])}");
			Console.WriteLine ();

			double[,] eulerCauchy = DifferentialEquationSolver.EulerCauchy (0, 1, Params.Func, h, n);
			Console.WriteLine ("Taблица приближенных значений полученных методом Эйлера-Коши");
			Console.WriteLine (eulerCauchy.ConvertToString());
			Console.WriteLine ();
			Console.WriteLine ($"|Y(Xn) - Yn| = {Math.Abs(y - eulerCauchy[n, 1])}");
			Console.WriteLine ();

			double[,] rungeKutta = DifferentialEquationSolver.RungeKutta4 (0, 1, Params.Func, h, n);
			Console.WriteLine ("Taблица приближенных значений полученных методом Рунге-Кутта 4-го порядка");
			Console.WriteLine (rungeKutta.ConvertToString());
			Console.WriteLine ();
			Console.WriteLine ($"|Y(Xn) - Yn| = {Math.Abs(y - rungeKutta[n, 1])}");
			Console.WriteLine ();

			double[,] adams = DifferentialEquationSolver.AdamsExtrapolation4 (exactTable, Params.Func, h, n + 2);
			Console.WriteLine ("Taблица приближенных значений полученных экстраполяционным методом Адамса 4-го порядка");
			Console.WriteLine (adams.ConvertToString());
			Console.WriteLine ();
			Console.WriteLine ($"|Y(Xn) - Yn| = {Math.Abs(y - adams[n + 2, 1])}");



		}

		public static double[,] GenerateTable(double x0, int leftK, int rightK, double h, 
			Func<double,double> func)
		{
			double[,] table = new double[rightK - leftK + 1, 2];
			for (int i = leftK; i <= rightK; i++) 
			{
				double x = x0 + i * h;
				table [i - leftK, 0] = x;
				table [i - leftK, 1] = func (x);
			}
			return table;
		}

		public static int GetN()
		{
			while (true) 
			{
				Console.WriteLine ("Введите N");
				string value  = Console.ReadLine ();
				int result;

				if (int.TryParse (value, out result) && result.GreaterThan (0)) 
				{
					return result;
				}
				Console.WriteLine ("Введено неверно");
			}
		}

		public static double GetH()
		{
			while (true) 
			{
				Console.WriteLine ("Введите h");
				string value  = Console.ReadLine ();
				double result;

				if (double.TryParse (value, out result) && result.GreaterThan (0)) 
				{
					return result;
				}
			}
		}
	}
}
