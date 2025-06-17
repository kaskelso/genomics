#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun 20 14:01:27 2024

@author: kennyaskelson
"""

chromosomes = {
    "Scaffold_5": 57574287,
    "Scaffold_6": 42958557,
    "Scaffold_7": 60790049
}

num_intervals = 2  # Change this to any number of intervals you want

for chrom, length in chromosomes.items():
    interval_size = length // num_intervals
    for i in range(num_intervals):
        start = i * interval_size + 1
        end = (i + 1) * interval_size if i < num_intervals - 1 else length  # Ensure last interval reaches length
        interval = f"{chrom}:{start}-{end}"

        # Create a file for each interval
        filename = f"{chrom}_part{i+1}.intervals"
        with open(filename, "w") as file:
            file.write(interval + "\n")

for chrom, length in chromosomes.items():
    for _ in range(num_intervals):
        print(chrom)


