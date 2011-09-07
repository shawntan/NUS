% Exercise 2
% Implement Quicksort in Prolog


% Base Case
QuickSort([], [], []).



void quicksort (int[] a, int lo, int hi)
{
//  lo is the lower index, hi is the upper index
//  of the region of array a that is to be sorted
    int i=lo, j=hi, h;

    // comparison element x
    int x=a[(lo+hi)/2];

    //  partition
    do
    {    
        while (a[i]<x) i++; 
        while (a[j]>x) j--;
        if (i<=j)
        {
            h=a[i]; a[i]=a[j]; a[j]=h;
            i++; j--;
        }
    } while (i<=j);

    //  recursion
    if (lo<j) quicksort(a, lo, j);
    if (i<hi) quicksort(a, i, hi);
}