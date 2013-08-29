using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Windows.Foundation;
using Windows.Foundation.Collections;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Controls.Primitives;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Navigation;
using DigitRecognizer;
using Windows.UI;
using System.Threading.Tasks;
// The Blank Page item template is documented at http://go.microsoft.com/fwlink/?LinkId=234238

namespace DigitRecognizer.UI
{
    /// <summary>
    /// An empty page that can be used on its own or navigated to within a Frame.
    /// </summary>
    public sealed partial class MainPage : Page
    {
        public MainPage()
        {
            this.InitializeComponent();
        }

        /// <summary>
        /// Invoked when this page is about to be displayed in a Frame.
        /// </summary>
        /// <param name="e">Event data that describes how this page was reached.  The Parameter
        /// property is typically used to configure the page.</param>
        protected async override void OnNavigatedTo(NavigationEventArgs e)
        {
            var trainingFileName = @"trainingsample.csv";
            var validationFileName = @"validationsample.csv";
  
            var trainingData = await readFile(trainingFileName);
            var validationData = await readFile(validationFileName);

            var traingingSamples = Processor.stringToSamples(trainingData);
            var validationSamples = Processor.stringToSamples(validationData);

            var results = new List<Result>();
            foreach (var vSample in validationSamples)
            {
                var tSample = Processor.classifier(traingingSamples, vSample.Pixels);

                var result = new Result
                {
                    IsMatch = vSample.Label == tSample.Item1.Label,
                    TrainingSample = tSample.Item1,
                    ValidationSample = vSample,
                    EuclideanDistance = tSample.Item2
                };

                results.Add(result);                
            }

            resultList.ItemsSource = results.OrderBy(x => x.IsMatch);
            processClassifier.IsActive = false;
        }

        private async Task<string> readFile(string fileName)
        {
            var folder = Windows.ApplicationModel.Package.Current.InstalledLocation;
            var file = await folder.GetFileAsync(fileName);
            return await Windows.Storage.FileIO.ReadTextAsync(file);
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            var result = (sender as Button).DataContext as Result;
            var trainingSample2D = Processor.pixelsListTo2D(result.TrainingSample.Pixels);
            var validationSample2D = Processor.pixelsListTo2D(result.ValidationSample.Pixels);
            buildGrid(trainingGrid, trainingSample2D);
            buildGrid(validationGrid, validationSample2D);
        }

        private void buildGrid(Grid parent, int[,] pixels)
        {
            parent.Children.Clear();
            var length = Processor.pixelsNum;
            for (int row = 0; row < length; row++)
            {
                for (int col = 0; col < length; col++)
                {
                    var color = getGrayScaleColor(pixels, row, col);
                    var border = new Border
                    {
                        Background = new SolidColorBrush(color),
                        BorderBrush = new SolidColorBrush(Colors.LightBlue),
                        BorderThickness = new Thickness(0.5),
                    };

                    border.SetValue(Grid.RowProperty, row);
                    border.SetValue(Grid.ColumnProperty, col);

                    parent.Children.Add(border);
                }
            }
        }

        // http://stackoverflow.com/questions/835753/convert-grayscale-value-to-rgb-representation
        private static Color getGrayScaleColor(int[,] pixels, int row, int col)
        {            
            var grayScale = Convert.ToByte(255 - pixels[row, col]);
            var color = Color.FromArgb(255, grayScale, grayScale, grayScale);
            return color;
        }
    }

    class Result
    {
        public bool IsMatch { get; set; }
        public Processor.sample TrainingSample { get; set; }
        public Processor.sample ValidationSample { get; set; }
        public int EuclideanDistance { get; set; }
    }
}
