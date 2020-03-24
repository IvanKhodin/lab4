#include <cstdio>
#include <algorithm>
#include <cmath>
#include <iostream>
#include <stdio.h>
#include <stdlib.h> 
#include <cstdlib>
#include <ctime>
#include <string>
#include <math.h>

using namespace std;

void clear(double** arr, int n)
{
    for (int i = 0; i < n; i++)
        delete[] arr[i];
    
    delete[] arr;
}

                                                                                        // создать копию массива
double** clone(double** arr, int n)
{
    double** newArr = new double* [n];
    
    for (int row = 0; row < n; row++)
    {
        newArr[row] = new double[n];

        for (int col = 0; col < n; col++)
            newArr[row][col] = arr[row][col];
    }
    
    return newArr;
}

                                                                                        // напечатать матрицу
void show(double** matrix, int n)
{
    for (int row = 0; row < n; row++) {
        for (int col = 0; col < n; col++)
            cout << matrix[row][col] << " ";
        cout << endl;
    }

    cout << endl;
}

                                                                                                // матричное умножение матриц
double** matrix_multi(double** A, double** B, int n)
{
    double** result = new double* [n];
                                                                                                         // заполнение нулями
    for (int row = 0; row < n; row++) {
        result[row] = new double[n];
        for (int col = 0; col < n; col++) {
            result[row][col] = 0;
        }
    }

    for (int row = 0; row < n; row++) {
        for (int col = 0; col < n; col++) {
            for (int j = 0; j < n; j++) {
                result[row][col] += A[row][j] * B[j][col];
            }
        }
    }

    return result;
}


double* matrix_multittt1(double** A, double* B, int n)
{
    double* result = new double [n];
                                                                                             // заполнение нулями
        for (int col = 0; col < n; col++) {
            result[col] = 0;
        }
    
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
            result[i] += A[i][j] * B[j];
        
    return result;
}

                                                                                             // умножение матрицы на число
void scalar_multi(double** m, int n, double a)
{
    for (int row = 0; row < n; row++)
        for (int col = 0; col < n; col++) {
            m[row][col] *= a;
        }

}

                                                                                            // вычисление суммы двух квадратных матриц
void sum(double** A, double** B, int n)
{
    for (int row = 0; row < n; row++)
        for (int col = 0; col < n; col++)
            A[row][col] += B[row][col];
}

                                                                                            // вычисление определителя
double det(double** matrix, int n)                                                      // квадратная матрица размера n*n
{
    double** B = clone(matrix, n);
                                                                                            // приведение матрицы к верхнетреугольному виду
    for (int step = 0; step < n - 1; step++)
        for (int row = step + 1; row < n; row++)
        {
            double coeff = -B[row][step] / B[step][step];                                   // метод Гаусса
            for (int col = step; col < n; col++)
                B[row][col] += B[step][col] * coeff;
        }
                                                                                // рассчитать определитель как произведение элементов главной диагонали
    double Det = 1;

    for (int i = 0; i < n; i++)
        Det *= B[i][i];
                                                                                                 // очистить память
    clear(B, n);

    return Det;
}


double* gauss(double** a, double* y, int n)
{
    double* x, max;
    
    int k = 0, index;
    
    const double eps = 0.00001;                                                     // точность
    
    x = new double[n];
    
    while (k < n)
    {                                                                                    // поиск строки с максимальным a[i][k]
        max = abs(a[k][k]);

        index = k;

        for (int i = k + 1; i < n; i++)
        {
            if (abs(a[i][k]) > max)
            {
                max = abs(a[i][k]);
                index = i;
            }
        }
                                                                                               // перестановка строк
        if (max < eps)
        {
                                                                                                 // нет ненулевых диагональных элементов
            cout << "Решение получить невозможно из-за нулевого столбца ";

            cout << index << " матрицы A" << endl;
            
            return 0;
        }

        for (int j = 0; j < n; j++)
        {
            double temp = a[k][j];
            a[k][j] = a[index][j];
            a[index][j] = temp;
        }
        
        double temp = y[k];
        
        y[k] = y[index];

        y[index] = temp;
                                                                                    // нормализация уравнений
        for (int i = k; i < n; i++)
        {
            double temp = a[i][k];

            if (abs(temp) < eps) continue;                                          // для нулевого коэффициента пропустить

            for (int j = 0; j < n; j++)
                a[i][j] = a[i][j] / temp;
            y[i] = y[i] / temp;
            if (i == k)  continue;                                              // уравнение не вычитать само из себя

            for (int j = 0; j < n; j++)
                a[i][j] = a[i][j] - a[k][j];
            y[i] = y[i] - y[k];
        }

        k++;
    }
                                                                                    // обратная подстановка
    for (k = n - 1; k >= 0; k--)
    {
        x[k] = y[k];

        for (int i = 0; i < k; i++)
            y[i] = y[i] - a[i][k] * x[k];
    }

    return x;
}


void l() 
{
    cout << endl;
}


int main() {

    setlocale(LC_ALL, "Russian");

    cout << "Variant 9, Khodin Ivan (427 gruppa)" << endl << endl << endl;
    
    const int n = 10;

    double** A;                                                                      // массив коэффициентов а

    double* X;                                                                       // массив искомых значений 

    A = new double* [n];

    double F[n];                                                                     // массив значений функции

    for (int i = 0; i < n; i++)
    {
        A[i] = new double[n];
        for (int j = 0; j < n; j++)
            A[i][j] = 0;
    }

    for (int i = 0;i < n;i++)
        for (int j = 0;j < n;j++) 
            if (i == j) A[i][j] = 1;
                else A[i][j] = 1 / double((i + j + 2));
    
    cout << "Ishodnaya matrica: " << endl << endl;

    for (int i = 0;i < n;i++) {
        F[i] = 1 / double((i + 1));
        for (int j = 0;j < n;j++)
            cout << A[i][j] << "  ";
        cout << "| " << F[i] << endl;
    }

    l(); l();

    cout << "Treugolnaya matrica: " << endl << endl;

    X = gauss(A, F, n);

    for (int i = 0;i < n;i++) {
        for (int j = 0;j < n;j++)
            cout << A[i][j] << "  ";
        cout << "| " << F[i] << endl;
    }

    l(); l();

    /*
    l();
                                                                            // делаем из треугольной матрицы диагональную

    for(int z=9; z > 0;z--)
        for (int i = z-1; i >= 0; i--)
            for (int j = 9;j >= 0;j--)                            
                A[i][j] -= A[z][j] * A[i][z];
          
    cout << "Матрица собственных значений(находится на доработке): " << endl << endl;
        
    for (int i = 0;i < n;i++) {
        for (int j = 0;j < n;j++)
            cout << A[i][j] << "  ";
        l();
    }
    
    l(); */
          
    cout << "Znacheniya po metodu Gaussa: " << endl << endl;

    for (int i = 0; i < n; i++)
        cout << "x[" << i+1 << "]=" << X[i] << endl;

    l(); l();

    double G1[n], G2[n];

    double* S1 = new double[n];

    double* S2 = new double[n];

    for (int i = 0; i < n; i++)
    {
        G1[i] = X[i];
    }
            
    for (int i = 0;i < n;i++)
        for (int j = 0;j < n;j++)
            if (i == j) A[i][j] = 1;
            else A[i][j] = 1 / double((i + j + 2));

    S1 = matrix_multittt1(A, G1, n);

    cout << "Proverka metoda Gaussa: " << endl << endl;

    for (int i = 0;i < n;i++)
        cout << S1[i] << endl;

    l(); l();

    for (int i = 0;i < n;i++)
        F[i] = 1 / double((i + 1));

    double r1=0, r2=0;

    for (int i = 0;i < n;i++)
        r1 += abs(F[i] - S1[i]);
          
    cout << "Norma vectora neuvyazki dlya metoda Gaussa = " << to_string(r1) << endl << endl;
                                                                                                                      // ПВР
    double V = X[0];

    int i, j, k = 0;
    
    const double eps = 0.0001;

    double norma, w = 0;
    
    double xn[n];

    for (int i = 0;i < n;i++)
        X[i] = 0;


    for (int i = 0;i < n;i++) 
        F[i] = 1 / double((i + 1));

    l();

    for (int i = 0;i < n;i++)
        for (int j = 0;j < n;j++)
            if (i == j) A[i][j] = 1;
                else A[i][j] = 1 / double((i + j + 2));
    
    cout << "Vvedite parametr 'w' dlya metoda PVR (recomenduemoe = 0.92) " << endl << endl;

    cin >> w;

    l(); 

        for (i = 0;i < 10;i++)
        {
            xn[i] = 0;
            X[i] = xn[i];
        }
        do
        {
            k++;

            norma = 0;

            for (i = 0;i < n;i++)
            {
                X[i] = F[i];
                for (j = 0;j < n;j++)
                {
                    if (i != j)
                        X[i] = X[i] - A[i][j] * X[j];
                }
                X[i] /= A[i][i];

                X[i] = w * X[i] + (1 - w) * xn[i];

                if (fabs(X[i] - xn[i]) > norma)
                    norma = fabs(X[i] - xn[i]);
                xn[i] = X[i];
            }

        } while (norma > eps);

        cout << "Kolichestvo iteraciy: ";

        cout << k << endl << endl << endl;

        cout << "Znacheniya po metodu PVR: " << endl << endl;

        for (i = 0;i < n;i++)
            cout << "x [" << i + 1 << "] = " << X[i] << endl;
    
        l(); l();

        for (int i = 0; i < n; i++)
        {
            G2[i] = X[i];
        }
              
        for (int i = 0;i < n;i++)
            for (int j = 0;j < n;j++)
                if (i == j) A[i][j] = 1;
                else A[i][j] = 1 / double((i + j + 2));

        S2 = matrix_multittt1(A, G2, n);

        cout << "Proverka metoda PVR: " << endl << endl;

        for (int i = 0;i < n;i++)
            cout << S2[i] << endl;

        l(); l();
        
        for (int i = 0;i < n;i++)
            F[i] = 1 / double((i + 1));

        for (int i = 0;i < n;i++)
            r2 += abs(F[i] - S2[i]);

        cout << "Norma vectora neuvyazki dlya metoda PVR = " << to_string(r2) << endl << endl;
                                                                            
    for (int i = 0;i < n;i++)
        for (int j = 0;j < n;j++)
            if (i == j) A[i][j] = 1;
            else A[i][j] = 1 / double((i + j + 2));

    l(); 

    /* Численное вычисление обратной матрицы по методу Ньютона-Шульца
        1. Записать начальное приближение [Pan, Schreiber]:
            1) Транспонировать данную матрицу
            2) Нормировать по столбцам и строкам
        2. Повторять процесс до достижения заданной точности
    */

    double N1 = 0, Ninf = 0;    
                                                                                        // норма матрицы по столбцам и по строкам
    double** A0 = clone(A, n);                                                      // инициализация начального приближения
    
    for (size_t row = 0; row < n; row++) 
    {
        double colsum = 0, rowsum = 0;
        for (size_t col = 0; col < n; col++) 
        {
            rowsum += fabs(A0[row][col]);
            colsum += fabs(A0[col][row]);
        }
        
        N1 = max(colsum, N1);

        Ninf = max(rowsum, Ninf);
    }
                                                                                                // транспонирование
    for (size_t row = 0; row < n - 1; row++) {
        for (size_t col = row + 1; col < n; col++)
            swap(A0[col][row], A0[row][col]);
    }

    scalar_multi(A0, n, (1 / (N1 * Ninf)));                                                     // нормирование матрицы
                                                                                      // инициализация удвоенной единичной матрицы нужного размера
    double** E2 = new double* [n];

    for (int row = 0; row < n; row++)
    {
        E2[row] = new double[n];

        for (int col = 0; col < n; col++) {
            if (row == col)
                E2[row][col] = 2;
            else
                E2[row][col] = 0;
        }
    }

    double** inv = clone(A0, n);                                                                                        //A_{0}
    
    double EPS = 0.0001;                                                                                                 // погрешность

    if (det(A, n) != 0) {    
                                                                                                                        // если матрица не вырождена
        while (fabs(det(matrix_multi(A, inv, n), n) - 1) >= EPS)                                                // пока |det(A * A[k](^-1)) - 1| >= EPS
        {
            double** prev = clone(inv, n);                                                                                   // A[k-1]

            inv = matrix_multi(A, prev, n);                                                                                   // A.(A[k-1]^(-1))

            scalar_multi(inv, n, -1);                                                                                        // -A.(A[k-1]^(-1))
            
            sum(inv, E2, n);                                                                                         // 2E - A.(A[k-1]^(-1))
            
            inv = matrix_multi(prev, inv, n);                                                                   // (A[k-1]^(-1)).(2E - A.(A[k-1]^(-1)))
            
            clear(prev, n);
        }

        cout << "Obratnaya matrica: " << endl << endl;  
                                                                                                                  // вывод матрицы на экран
        show(inv, n);
    }

    else     cout << "Nope";

    l();

    double c1[n], c2[n], max1, max2;

    for (int i = 0;i < n;i++) {
        c1[i] = 0;
        c2[i] = 0;
    }

    cout << "Normy 1: Normy 2:" << endl << endl;

    for (int i = 0;i < n;i++) {
        for (int j = 0;j < n;j++) {
            c1[i] += abs(A[i][j]);
            c2[i] += abs(inv[i][j]);
        }
        cout << c1[i] << "  " << c2[i] << endl;
    }

    l();

    max1 = c1[0];

    max2 = c2[0];

    for (int i = 0;i < n;i++) {
        if (c1[i] > max1) max1 = c1[i];
        if (c2[i] > max2) max2 = c2[i];
    }

    l();
        
    cout << "Chislo obuslovlennosti sistemy = " << max1 * max2 << endl;
              
    l();
    
    clear(A, n);

    clear(E2, n);
    
    return 0;

}



