namespace Integral
{
    public class GaussInfo
    {
        public double[] Moments { get; private set; }
        public double[] PolynomialCoefficients { get; private set; }
        public double[] Nodes { get; private set; }
        public double[] QuadratureFormCoefficients { get; private set; }

        public double Value { get; private set; }

        public GaussInfo(
            double[] moments,
            double[] polynomialCoefficients,
            double[] nodes,
            double[] quadratureFormCoefficients, double value)
        {
            Moments = moments;
            PolynomialCoefficients = polynomialCoefficients;
            Nodes = nodes;
            QuadratureFormCoefficients = quadratureFormCoefficients;
            Value = value;
        }
    }
}