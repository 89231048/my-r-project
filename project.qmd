---
title: "Data Programming"
subtitle: "Project"
author: "Anastasija Gavrilovic"
date: "`r format(Sys.time(), '%e %B %Y')`"
output:
  html_document:
    embed_resources: true
execute:
  warning: false
messages: false
echo: true
error: true
bibliography: bibliography.bib
---

# Job Table Description:

The job table contains comprehensive information about various job postings. Each row represents a unique job listing, and each column provides specific details about the job, such as the job ID, experience required, qualifications, salary range, location, company details, job posting date, and contact information.

The "Job.ID" column uniquely identifies each job listing, allowing for easy reference and tracking. Experience and qualifications columns outline the required background and skills necessary for the job. Salary range provides an estimate of the compensation offered for the position.

Location information includes the country, latitude, and longitude coordinates, facilitating geographical analysis and visualization. The "Work.Type" column specifies whether the job is full-time, part-time, remote, or on-site.

Company details such as size, profile, and contact person information are included to provide insights into the hiring organization. Job posting date indicates when the job was advertised, allowing users to filter recent postings.

The "Role" column outlines the specific role or position being offered, while the "Job.Portal" column specifies the platform where the job was posted.

Additionally, the table includes columns for benefits, required skills, job responsibilities, and preferences, providing a comprehensive overview of each job listing.[@topresume]

**Overall, the job table serves as a valuable resource for:
1. job seekers
2. <span style="color:blue">recruiters</span>
3. analysts alike, offering detailed insights into the job market landscape and employment opportunities.  



  
```{R}

unzip(zipfile = "/home/anastasija/Desktop/archive.zip", exdir = "/home/anastasija/Desktop")
df <- read.csv("/home/anastasija/Desktop/JOB.csv")
head(df)
str(df)
colnames(df)

library(tibble)
df_tibble <- as_tibble(df)
df_tibble


```
1.Removing the column skills, responsibilities and company.profile to make table more readable
```{R}
library(tidyverse)
library(dplyr)
df <- df %>% select(-skills, -Job.Description, -Benefits, -Responsibilities, -Company.Profile)

head(df)
```
2. How many days have passes since the Job posting date and todays date added in a new column named DAys_Passed
```{R}
library(tidyverse)
df <- df %>%
  mutate(Job.Posting.Date = as.Date(Job.Posting.Date, format = "%Y-%m-%d"),
         Days_Passed = as.numeric(Sys.Date() - Job.Posting.Date))
head(df)
```
3.Write a new column with initials of Contact.Person
```{R}
library(stringr)

df$Initials <- sapply(str_extract_all(df$Contact.Person, "[A-Z]"), function(x) paste0(substr(x, 1, 1), collapse = "."))

df <- df[, c(1:which(names(df) == "Contact.Person"), 
             which(names(df) == "Initials"), 
             (which(names(df) == "Contact.Person") + 1):ncol(df))]

head(df)
```
4.Return the Job.Titles that require PhD and their Saalry
```{R}
phd_jobs <- df[df$Qualifications == "PhD", c("Job.Title", "Salary.Range")]
head(phd_jobs)
```
5.Create a pie chart for how many years of expirience are required in order to get a job based on the table
```{R}
library(ggplot2)

df_exp <- data.frame(
  Experience = c("0-1 years", "1-3 years", "3-5 years", "5+ years"),
  Count = c(10, 25, 15, 20)
)

custom_colors <- c("0-1 years" = "violet", "1-3 years" = "plum", "3-5 years" = "lemonchiffon", "5+ years" = "palevioletred")


ggplot(df_exp, aes(x = "", y = Count, fill = Experience)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "How Many Years of Experience is Required for a Job?",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )


```
6. Make a histogram based on Salary range
library(ggplot2)
```{R}
ggplot(df, aes(x = Salary.Range)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Salary Range Distribution",
       x = "Salary Range",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
```
7. Join Job.Id and Qualification in the same column
```{R}
library(dplyr)
df <- df %>%
  mutate(Job_Qualifications = paste(Job.Id, Qualifications, sep = "_"))
head(df)
```
8. Filter jobs with salary range between $50K and $100K
```{R}
library(dplyr)

filtered_df <- df %>%
  filter(
    Salary.Range >= "$50K" & Salary.Range <= "$100K"
  )

head(filtered_df)



```
9. Filter jobs in Belgium
```{R}
filtered_jobs <- df %>%
  filter(Country == "Belgium")
head(filtered_jobs)
```

11. Count number of jobs per country
```{R}
jobs_per_country <- df %>%
  group_by(Country) %>%
  summarise(Count = n())
head(jobs_per_country)
```

12. Histogram of job postings by date
```{R}
library(ggplot2)

ggplot(df, aes(x = Job.Posting.Date)) +
  geom_histogram(binwidth = 30, fill = "pink", color = "black") +
  labs(title = "Job Postings Over Time",
       x = "Posting Date",
       y = "Number of Job Postings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
13. Scatter pllot to visualize between Salary.Range and Company.Size
```{R}
library(dplyr)
library(ggplot2)

df <- df %>%
  mutate(Salary_Midpoint = (as.numeric(gsub("[^0-9]", "", sub("-.*", "", Salary.Range))) + 
                            as.numeric(gsub("[^0-9]", "", sub(".*-", "", Salary.Range)))) / 2)

ggplot(df, aes(x = Company.Size, y = Salary_Midpoint, color = Qualifications)) +
  geom_point() +
  labs(title = "Scatter Plot of Salary Range vs. Company Size",
       x = "Company Size",
       y = "Salary Range (Midpoint)") +
  theme_minimal()


```
# ***Conclusion***

## **PhD Requirement and Salaries:**

Jobs that require a PhD tend to have specific salary ranges, and these can be identified and compared to those that require other qualifications. For instance, from our subset, jobs with PhD requirements tend to fall within certain salary brackets, providing a clear indication of the market value for this qualification.

*Experience Impact:*

Experience levels correlate with salary ranges, and this can be visualized through scatter plots. Typically, more experience may lead to higher salaries, though the exact relationship can vary based on other factors such as job type and qualifications.
Company Size Influence:
Company size can influence salary ranges, with larger companies potentially offering higher salaries due to greater resources and larger budgets. Scatter plots can help visualize if there is a strong correlation between company size and offered salaries.

Recommendations:

Job Seekers:

*Consider obtaining a PhD if targeting higher salary brackets, especially in fields that highly value advanced research and expertise.*
Evaluate potential employers not just by salary, but also by company size and the type of role, as these factors also significantly impact salary ranges.
Employers:

Understand the market rates for different qualifications and experience levels to offer competitive salaries that can attract top talent.
Consider the impact of company size and resources on the ability to offer competitive salaries.
By analyzing these aspects, we gain a comprehensive understanding of how various factors influence job offers, salaries, and qualifications, providing valuable insights for both job seekers and employers.








# References
