using System;
using System.Linq;
using System.Text;

namespace Extensions
{
	public static class Extensions
	{
		public static double Pow(this double value, double pow)
		{
			return Math.Pow (value, pow);
		}

		public static T GetClone<T>(this T item) where T: ICloneable
		{
			item.ValidateOnNull ();

			return (T)item.Clone ();
		}

		public static int GetHeight<T>(this T[,] array)
		{
			array.ValidateOnNull ();

			return array.GetLength (0);
		}

		public static int GetWidth<T>(this T[,] array)
		{
			array.ValidateOnNull ();

			return array.GetLength (1);
		}

		public static U[] Map<T, U>(this T[] array, Func<T, U> func)
		{
			ValidationHelper.ValidateOnNull (array, func);

			U[] newArray = new U[array.Length];

			for (int i = 0; i < array.Length; i++) 
			{
				newArray [i] = func (array [i]);	
			}
			return newArray;
		}

		public static double Sum(this double[] values)
		{
			values.ValidateOnNull ();

			return values.Aggregate ((value, item) => value + item);
		}

		public static bool InRange<T>(this T value, T min, T max) where T: IComparable
		{
			return value.GreaterThanOrEqualTo (min) && value.LessThanOrEqualTo (max);
		}

		public static bool LessThan<T>(this T curObj, T value) where T: IComparable
		{
			return curObj.CompareTo (value) == -1;
		}

		public static bool LessThanOrEqualTo<T>(this T curObj, T value) where T: IComparable
		{
			return curObj.LessThan (value) || curObj.EqualTo(value);;
		}

		public static bool GreaterThan<T>(this T curObj, T value) where T: IComparable
		{
			return curObj.CompareTo (value) == 1;
		}

		public static bool GreaterThanOrEqualTo<T>(this T curObj, T value) where T: IComparable
		{
			return curObj.GreaterThan (value) || curObj.EqualTo (value);
		}

		public static bool EqualTo<T>(this T curObj, T value) where T: IComparable
		{
			return curObj.CompareTo (value) == 0;
		}

		public static bool IsNull<T>(this T value) where T: class
		{
			return value == null;
		}

		public static T[] GetLine<T>(this T[,] array, int index)
		{
			T[] line = new T[array.GetWidth ()];

			for (int i = 0; i < line.Length; i++) 
			{
				line [i] = array [index, i];
			}
			return line;
		}

		public static T[] GetColumn<T>(this T[,] array, int index)
		{
			T[] column = new T[array.GetHeight ()];

			for (int i = 0; i < column.Length; i++) 
			{
				column [i] = array [i, index];
			}
			return column;
		}

		public static string ConvertToString(this double[,] array)
		{
			int[] maxLengths = GetMaxLengths (array);

			StringBuilder result = new StringBuilder ();

			for (int i = 0; i < array.GetHeight(); i++) 
			{	
				for (int j = 0; j < array.GetWidth(); j++) 
				{
					result.Append (new String(' ', maxLengths[j] - array[i, j].ToString().Length));
					result.Append ($"{array[i, j]}   ");
				}
				result.AppendLine ();
			}
			return result.ToString ();
		}

		private static int[] GetMaxLengths(double[,] array)
		{
			int[] max = new int[array.GetWidth()];

			for (int i = 0; i < array.GetHeight(); i++) 
			{	
				for (int j = 0; j < array.GetWidth(); j++) 
				{
					max[j] = Math.Max (array[i, j].ToString().Length, max[j]);
				}
			}
			return max;
		}
	}
}

