using System;
using System.IO;
using System.Resources;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.ComponentModel;
using System.Reflection;
using Microsoft.Win32;
using System.Diagnostics;

namespace Cambridge_Pseudocode_to_Python
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private OpenFileDialog openFileDialog = new();
        private SaveFileDialog saveFileDialog = new();
        private String? pseudocodePath;

        public MainWindow()
        {
            InitializeComponent();
        }

        private void Button_ChoosePseudocodeFile_Click(object sender, RoutedEventArgs e)
        {
            if (openFileDialog.ShowDialog() ?? false)
                pseudocodePath = openFileDialog.FileName;
        }

        private void Button_Translate_Click(object sender, RoutedEventArgs e)
        {
            if (pseudocodePath == null)
            {
                MessageBox.Show("translate-after-choose", "no-chosen-file");
                return;
            }

            Process process = new();
            process.StartInfo.FileName = AppDomain.CurrentDomain.BaseDirectory + "\\campseudo-to-py.exe";
            process.StartInfo.Arguments = pseudocodePath + " -e";
            process.StartInfo.UseShellExecute = false;
            process.StartInfo.RedirectStandardOutput= true;
            process.Start();
            process.WaitForExit();

            if (process.ExitCode != 0)
            {
                MessageBox.Show("contain-syntax-error", "syntax-error");
                return;
            }

            string data = process.StandardOutput.ReadToEnd();

            saveFileDialog.FileName = pseudocodePath + ".py";
            if (saveFileDialog.ShowDialog() ?? false)
                File.WriteAllText(pseudocodePath + ".py", data);
        }
    }
}
