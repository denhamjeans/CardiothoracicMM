# CardiothoracicMM

## Purpose

This is an R Markdown file intended for the automation of monthly cardiac surgery data review reports as a part of morbidity and mortality (M&M) review at Prince of Wales hospital NSW. All input data is sourced directly from the local Cardiothoracic Surgery database at Prince of Wales.

## Background of M&M Review

NSW health policy requires monthly M&M reports be created to review serious complications such as deaths or surgical mismanagement. Ordinarily M&M reports are created as a case-by-case patient review wherein the Advanced Surgical Trainee (AT) outlines each patient's medical history, pre-operative course, surgical procedure and post-operative course in a narrative structure using Microsoft PowerPoint software and a combination of physical and electronic patient records. The M&M reports are then presented at regularly scheduled department meetings on either a monthly, or bi-monthly basis depending on the availability of key parties such as the surgical consultants.

However, due to the complex nature of cardiac surgery and the need for a data oriented approach to clinical review processes, a data review component was added to the existing M&M case review reports within the Cardiothoracic Surgery department at Prince of Wales. This presents an opportunity for the relevant stakeholders to discuss and address current issues such as infections, pre-op wait times, and post-op complications in the context of historical data.

This R markdown file is intended to produce the relevant summary statistics in a timely, reliable, and transparent manner.

## Technical Details

At the time of creating this project, the decision was made to output the report in the form of a Microsoft PowerPoint file for a few key reasons. The most significant reason is that hospital staff are highly familiar the Microsoft Office Suite and highly favour Microsoft PowerPoint for presentation purposes. However, the Case-by-case patient review portion of the M&M (created by the Advanced Surgical Trainee) needed to be merged to the end of the Data Review section and this was easiest when using PowerPoint.
Regardless, the decision to use PowerPoint presented some challenges. At the time of creation R Markdown output in the form of PowerPoint files was still in its testing phase and did not offer a high degree of flexibility when producing tables and formatting slides. Fortunately, an alternative package 'OfficeR' presented a good alternative allowing for some more complex manipulation of the slide structure and the use of a Template slide structure using a reference PowerPoint file.

The reference PowerPoint file for this project is key for producing the background graphics within each slide and there are specific reference themes available depending on the requirement for the slide presentation eg. for rendering a single large graph or multiple tables etc.

The reference themes function by using a series of textbox placeholders for a given theme eg. 1 large placeholder with a defined length and width taking up the majority of the slide, and 1 smaller text placeholder for a title. Using R we can assign our tables formatted using 'dplyr' and graphs formatted using 'ggplot2' to the corresponding placeholder element within the PowerPoint template file. The R Markdown file will render all the corresponding graphics finally compiling everything as a single neat PowerPoint output file.

The creation and formatting of the tables and graphs uses 4 input csv files derived from the local Prince of Wales Cardiothoracic Database. The final Microsoft PowerPoint output is a total of 24 slides.

