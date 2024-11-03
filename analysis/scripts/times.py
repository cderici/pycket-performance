# Job durations analysis script

# Input is:
#   kubectl get jobs > file

# File path
file_path = './jobs'

# Reading the file and processing each line
tasks = []
with open(file_path, 'r') as file:
    # Skip header line
    next(file)
    # Process each line in the file
    for line in file:
        # Strip any leading/trailing whitespace
        line = line.strip()
        # Split the line by whitespace
        parts = line.split()
        # Extract the name, status, completions, duration, and age fields
        name = parts[0]
        status = parts[1]
        completions = parts[2]
        duration = parts[3]
        age = parts[4] if len(parts) > 4 else ""
        # Append as a tuple to the tasks list
        tasks.append((name, status, completions, duration, age))

# Now `tasks` contains the list of tuples
# print(tasks)


# Helper function to convert time strings to minutes
def convert_to_minutes(time_str):
    hours, minutes = 0, 0
    if 'h' in time_str:
        parts = time_str.split('h')
        hours = int(parts[0])
        if parts[1]:
            minutes = int(parts[1].strip(' m'))
    elif 'm' in time_str:
        if 's' in time_str:
            parts = time_str.split('m')
            minutes = int(parts[0])
        else:
            minutes = int(time_str.strip(' m'))
    return hours * 60 + minutes

# Converting task durations to minutes
task_durations = [(task, convert_to_minutes(duration)) for (task, status, completions, duration, age) in tasks]

# Sorting tasks by duration in minutes
task_durations_sorted = sorted(task_durations, key=lambda x: -x[1])

# Extracting just the durations for calculating median and average
durations_in_minutes = [duration for _, duration in task_durations_sorted]

# Calculating average and median durations
average_duration = sum(durations_in_minutes) / len(durations_in_minutes)
median_duration = durations_in_minutes[len(durations_in_minutes) // 2] if len(durations_in_minutes) % 2 != 0 else \
    (durations_in_minutes[len(durations_in_minutes) // 2 - 1] + durations_in_minutes[len(durations_in_minutes) // 2]) / 2

# Preparing data for table output
for task, duration in task_durations_sorted:
    print(f"{task}\t{duration}")

print(f"\n\nAverage Duration: {average_duration} mins")
print(f"Median Duration: {median_duration} mins")
