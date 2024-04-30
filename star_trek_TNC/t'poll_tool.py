import tkinter as tk
from tkinter import messagebox
import numpy as np
import pandas as pd

# Function to calculate population from percentages
def calc_population(total_votes, percentages):
    population = np.round((total_votes * np.array(percentages)) / 100).astype(int)
    return population

# Function to calculate mean and median scores
def calculate_scores(votes, percentages):
    try:
        total_votes = int(votes)
        percentages = [float(p) for p in percentages]
        percentages_sum = sum(percentages)
        
#        if not np.isclose(percentages_sum, 100):
#            raise ValueError(f"The percentages do not sum up to 100%. They sum to {percentages_sum}%.")
        
        population = calc_population(total_votes, percentages)
        
        # Create a DataFrame to hold scores and population
        pop_df = pd.DataFrame({
            'score': np.arange(11),  # Scores from 0 to 10
            'n': population
        })
        
        # Function to calculate scores for frequency
        def scores(df):
            repeated_scores = np.repeat(df['score'], df['n'])
            return repeated_scores
        
        # Generate scores from population data
        score_values = scores(pop_df)
        
        # Calculate mean and median
        mean_score = np.mean(score_values)
        median_score = np.median(score_values)
        
        # Display results
        result_text = f"Population: {population}\nMean Score: {mean_score:.2f}\nMedian Score: {median_score:.2f}"
        result_label.config(text=result_text)
    
    except ValueError as e:
        messagebox.showerror("Error", str(e))
    except Exception as e:
        messagebox.showerror("Error", "An unexpected error occurred: " + str(e))

# Function to clear all input fields
def clear_fields():
    for entry in entries:
        entry.delete(0, tk.END)
    votes_entry.delete(0, tk.END)
    result_label.config(text="")

# GUI setup
root = tk.Tk()
root.title("T'Poll Score Calculator")
# Adjust size
root.geometry("400x400")

# Create entry widgets for percentages and votes
entries = []
for i in range(11):
    tk.Label(root, text=f"Score {i}:").grid(row=i, column=0)
    entry = tk.Entry(root)
    entry.grid(row=i, column=1)
    entries.append(entry)

tk.Label(root, text="Total votes:").grid(row=11, column=0)
votes_entry = tk.Entry(root)
votes_entry.grid(row=11, column=1)

# Submit button
submit_btn = tk.Button(root, text="Execute", command=lambda: calculate_scores(votes_entry.get(), [e.get() for e in entries]))
submit_btn.grid(row=12, column=0, columnspan=1, pady=(10, 5))

# Clear button
clear_btn = tk.Button(root, text="Clear", command=clear_fields)
clear_btn.grid(row=12, column=1, columnspan=1, pady=(10, 5))

# Result display area
result_label = tk.Label(root, text="")
result_label.grid(row=14, column=0, columnspan=2)

# Run the GUI
root.mainloop()
