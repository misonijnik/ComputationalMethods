using System;

namespace Extensions
{
	public static class ValidationHelper
	{
		public static void ValidateOnNull(this object value)
		{
			if (value.IsNull()) 
			{
				throw new ArgumentNullException ();
			}
		}

		public static void ValidateOnNull(params object[] values)
		{
			foreach (var item in values) 
			{
				item.ValidateOnNull ();
			}
		}
	}
}

