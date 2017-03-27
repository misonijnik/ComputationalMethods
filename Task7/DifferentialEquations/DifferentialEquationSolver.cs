using System;

namespace DifferentialEquations
{
	public static class DifferentialEquationSolver
	{
		public static double[,] Euler(double x0, double y0, Func<double, double, double> func,
			double h, int n)
		{
			double[,] table = new double[n + 1, 2];
			double x = table[0, 0] = x0;
			double y = table[0, 1] = y0;

			for (int i = 1; i <= n; i++)
			{
				y = table [i, 1] = y + h * func (x, y);
				table [i, 0] = x += h;
			}
			return table;
		}

		public static double[,] ImprovedEuler(double x0, double y0, Func<double, double, double> func,
			double h, int n)
		{
			double[,] table = new double[n + 1, 2];
			double x = table[0, 0] = x0;
			double y = table[0, 1] = y0;

			for (int i = 1; i <= n; i++)
			{
				y = table [i, 1] = y + h * func (x + h / 2, y + h / 2 * func(x, y));
				table [i, 0] = x += h;
			}
			return table;
		}

		public static double[,] EulerCauchy(double x0, double y0, Func<double, double, double> func,
			double h, int n)
		{
			double[,] table = new double[n + 1, 2];
			double x = table[0, 0] = x0;
			double y = table[0, 1] = y0;

			for (int i = 1; i <= n; i++)
			{
				y = table [i, 1] = y + h / 2 * (func (x, y) + func (x + h, y + h * func (x, y)));
				table [i, 0] = x += h;
			}
			return table;
		}

		public static double[,] RungeKutta4(double x0, double y0, Func<double, double, double> func,
			double h, int n)
		{
			double[,] table = new double[n + 1, 2];
			double x = table[0, 0] = x0;
			double y = table[0, 1] = y0;

			for (int i = 1; i <= n; i++)
			{
				double k1 = h * func (x, y);
				double k2 = h * func (x + h / 2, y + k1 / 2);
				double k3 = h * func (x + h / 2, y + k2 / 2);
				double k4 = h * func (x + h, y + k3);

				y = table [i, 1] = y + (k1 + 2 * k2 + 2 * k3 + k4) / 6;
				table [i, 0] = x += h;
			}
			return table;
		}

		public static double[,] AdamsExtrapolation4(double[,] tableOfValues,
			Func<double, double, double> func, double h, int n)
		{
			const int k = 3;
			double[,] table = new double[n + 1, 2];
			double[,] finiteDifferences = new double[n + 1, k + 1];
			double x = 0;
			double y = 0;

			for (int i = 0; i <= k; i++) 
			{
				x = table [i, 0] = tableOfValues [i, 0];
				y = table [i, 1] = tableOfValues [i, 1];
				finiteDifferences [i, 0] = h * func (x, y);
			}				

			for (int i = 1; i <= k; i++) 
			{
				for (int j = k - i; j >= 0 ; j--) 
				{
					finiteDifferences [j, i] = finiteDifferences [j + 1, i - 1] - finiteDifferences [j, i - 1];
				}
			}
			for (int i = k + 1; i <= n; i++)
			{
			    y = table[i, 1] = y +
			                      finiteDifferences[i - 1, 0] +
			                      0.5 * finiteDifferences[i - 2, 1] +
			                      5.0 / 12 * finiteDifferences[i - 3, 2] +
			                      3.0 / 8 * finiteDifferences[i - 4, 3];
				table [i, 0] = x += h;
				finiteDifferences [i, 0] = h * func (x, y);

				for (int j = 1; j <= k; j++) 
				{
					finiteDifferences [i - j, j] = finiteDifferences [i - j + 1, j - 1] - finiteDifferences [i - j, j - 1];
				}
			}
			return table;
		}
	}
}

