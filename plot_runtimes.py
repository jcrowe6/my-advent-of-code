#!/usr/bin/env python3

# Credit to Claude code FOR THIS SCRIPT ONLY
# All other code (the problem solutions) are entirely my own
# Because I have self respect

"""
Plot runtime comparisons for Advent of Code solutions by language.

Usage: python plot_runtimes.py <year>
Example: python plot_runtimes.py 2025
"""

import os
import sys
import re
from pathlib import Path
from collections import defaultdict
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches


def parse_output_file(filepath):
    """Parse an output file and extract the runtime in seconds."""
    try:
        with open(filepath, "r") as f:
            lines = f.readlines()
            if len(lines) >= 2:
                # Second line contains the runtime
                runtime_line = lines[1].strip()
                # Extract the float value before 's'
                match = re.search(r"([\d.]+)s", runtime_line)
                if match:
                    return float(match.group(1))
    except Exception as e:
        print(f"Warning: Could not parse {filepath}: {e}")
    return None


def collect_runtimes(year):
    """Collect all runtime data for a given year."""
    base_path = Path(__file__).parent / str(year)

    if not base_path.exists():
        print(f"Error: Year directory {base_path} does not exist")
        return None

    # Data structure: {day: {problem: {language: runtime}}}
    data = defaultdict(lambda: defaultdict(dict))
    languages = set()

    # Find all output files
    for output_file in base_path.glob("day*/problem*.output.*"):
        # Parse filename: problem1.output.python
        filename = output_file.name
        match = re.match(r"problem(\d+)\.output\.(\w+)", filename)

        if match:
            problem_num = int(match.group(1))
            language = match.group(2)
            day = int(output_file.parent.name.replace("day", ""))

            runtime = parse_output_file(output_file)
            if runtime is not None:
                data[day][problem_num][language] = runtime
                languages.add(language)

    return data, sorted(languages)


def create_plot(data, languages, year):
    """Create a grouped bar chart comparing runtimes."""
    if not data:
        print("No data to plot")
        return

    # Prepare data for plotting
    days = sorted(data.keys())
    problems = sorted(set(p for day_data in data.values() for p in day_data.keys()))

    # Prepare runtime data for each language
    language_data = {lang: [] for lang in languages}
    actual_labels = []
    day_boundaries = []  # Track where each day starts for visual grouping

    current_pos = 0
    for day in days:
        day_has_data = False
        for problem in problems:
            if problem in data[day]:
                actual_labels.append(f"P{problem}")
                for lang in languages:
                    runtime = data[day][problem].get(lang, None)
                    language_data[lang].append(runtime)
                if not day_has_data:
                    day_boundaries.append((current_pos, day))
                    day_has_data = True
                current_pos += 1

    # Create the plot
    fig, ax = plt.subplots(figsize=(max(14, len(actual_labels) * 0.5), 8))

    # Set up bar positions with gaps between days
    x_positions = []
    current_x = 0
    gap_between_days = 0.5  # Add a gap between days for visual grouping

    for i, label in enumerate(actual_labels):
        # Add gap when we start a new day (when label is "P1")
        if label == "P1" and i > 0:
            current_x += gap_between_days
        x_positions.append(current_x)
        current_x += 1

    width = 0.8 / len(languages)

    # Color map for languages
    colors = plt.cm.tab10(range(len(languages)))

    # Plot bars for each language
    for i, lang in enumerate(languages):
        offsets = [pos + i * width for pos in x_positions]
        values = language_data[lang]

        # Handle None values (no data) by using 0 and making them invisible
        plot_values = [v if v is not None else 0 for v in values]
        bars = ax.bar(
            offsets,
            plot_values,
            width,
            label=lang.capitalize(),
            color=colors[i],
            alpha=0.8,
        )

        # Make bars with None data invisible
        for bar, val in zip(bars, values):
            if val is None:
                bar.set_visible(False)

    # Customize the plot
    ax.set_ylabel("Runtime (seconds)", fontsize=12, fontweight="bold")
    ax.set_title(
        f"{year}",
        fontsize=14,
        fontweight="bold",
    )

    # Set x-ticks at the center of each group of bars
    ax.set_xticks([pos + width * (len(languages) - 1) / 2 for pos in x_positions])
    ax.set_xticklabels(actual_labels, rotation=0, ha="center", fontsize=9)

    # Add day labels above the x-axis
    for start_idx, day in day_boundaries:
        # Get all positions for this day
        day_end_idx = start_idx + 1
        while day_end_idx < len(actual_labels) and actual_labels[day_end_idx] != "P1":
            day_end_idx += 1

        day_x_positions = x_positions[start_idx:day_end_idx]
        if day_x_positions:
            mid_x = (
                day_x_positions[0] + day_x_positions[-1] + width * (len(languages) - 1)
            ) / 2
            ax.text(
                mid_x,
                -0.08,
                f"Day {day}",
                ha="center",
                va="top",
                fontsize=10,
                fontweight="bold",
                transform=ax.get_xaxis_transform(),
            )

    ax.legend(title="Language", loc="upper left")
    ax.grid(axis="y", alpha=0.3, linestyle="--")

    # Use log scale if there's a large range of values
    all_values = [
        v for lang_vals in language_data.values() for v in lang_vals if v is not None
    ]
    if all_values and max(all_values) / min(all_values) > 100:
        ax.set_yscale("log")
        ax.set_ylabel("Runtime (seconds, log scale)", fontsize=12, fontweight="bold")

    plt.tight_layout()

    # Save the plot
    output_filename = f"runtime_comparison_{year}.png"
    plt.savefig(output_filename, dpi=300, bbox_inches="tight")
    print(f"Plot saved to {output_filename}")

    # Also print summary statistics
    print(f"\n{'=' * 60}")
    print(f"Runtime Summary for {year}")
    print(f"{'=' * 60}")
    for lang in languages:
        runtimes = [v for v in language_data[lang] if v is not None]
        if runtimes:
            print(f"\n{lang.capitalize()}:")
            print(f"  Problems solved: {len(runtimes)}")
            print(f"  Total runtime: {sum(runtimes):.4f}s")
            print(f"  Average runtime: {sum(runtimes) / len(runtimes):.4f}s")
            print(f"  Min runtime: {min(runtimes):.6f}s")
            print(f"  Max runtime: {max(runtimes):.4f}s")


def main():
    if len(sys.argv) != 2:
        print("Usage: python plot_runtimes.py <year>")
        print("Example: python plot_runtimes.py 2025")
        sys.exit(1)

    try:
        year = int(sys.argv[1])
    except ValueError:
        print("Error: Year must be an integer")
        sys.exit(1)

    print(f"Collecting runtime data for year {year}...")
    result = collect_runtimes(year)

    if result is None:
        sys.exit(1)

    data, languages = result

    if not languages:
        print(f"No output files found for year {year}")
        sys.exit(1)

    print(f"Found {len(languages)} language(s): {', '.join(languages)}")
    print(f"Creating plot...")

    create_plot(data, languages, year)


if __name__ == "__main__":
    main()
