library(tidyverse)
library(rio)
library(stringdist)
library(ggrepel)

hse = import('~/Downloads/22-23_hse_probability.xlsx', sheet=5)
hse = hse[, c(2:3, 10:40)]
colnames(hse) = c('last_name', 'first_name', 'var',
                  paste0('a', str_pad(1:30, width = 2, pad='0')))
hse = filter(hse, !is.na(var))
hse = mutate(hse, name = paste(last_name, first_name, sep=' '))
hse[1:4, 'name'] = paste0('Correct Answers ', 11:14)

hse = select(hse, -last_name, -first_name)

sum(is.na(hse))
hse[is.na(hse)] = 'x'

hse = mutate(hse, ans=paste0(a01, a02, a03, a04, a05, a06, a07, a08, a09, a10,
                             a11, a12, a13, a14, a15, a16, a17, a18, a19, a20,
                             a21, a22, a23, a24, a25, a26, a27, a28, a29, a30))


# distance to nearest neighbor vs result ---------------------------------------------------------------

correct_ans = filter(hse, str_starts(name, 'Correct')) %>%
  select(var, ans) %>%
  rename(correct=ans)
correct_ans

hse_studs = filter(hse, !str_starts(name, 'Correct')) %>%
  left_join(correct_ans, by='var') 
hse_studs$grade = 30 - 
  stringdist(hse_studs$ans, hse_studs$correct, method='hamming')

hse_dist_matrix = stringdistmatrix(hse_studs$ans, hse_studs$ans, method='hamming')
hse_dist_matrix 

rownames(hse_dist_matrix) = hse_studs$name
colnames(hse_dist_matrix) = hse_studs$name

hse_dist_df = as_tibble(hse_dist_matrix)

hse_dist_df$name = hse_studs$name
hse_dist_df$var = hse_studs$var

glimpse(hse_dist_df)

hse_dist_long = pivot_longer(data=hse_dist_df, !any_of(c('name', 'var')), names_to='name2')
hse_dist_long = arrange(hse_dist_long, value)

hse_dist_long_nodup = filter(hse_dist_long, name < name2) # order by alphabet


hse_mindist = filter(hse_dist_long, name != name2) %>%
  # filter(!str_starts(name2, 'Correct')) %>%
  group_by(name) %>%
  summarise(min_dist = min(value), var = min(var))

glimpse(hse_studs)
glimpse(hse_mindist)

hse_studs = left_join(hse_studs, hse_mindist,  
                        by = c('name', 'var')) 

hse_plot = hse_studs # copy 

labels_list = c('Касумов Осман Мурадович',
                'Исмаилов Сардорбек Улугбек угли',
                'Гуськов Олег Сергеевич',
                'Мастюгина Анна Владимировна')

hse_plot$name[!(hse_plot$name %in% labels_list)] = ''

ggplot(hse_plot, aes(x=grade, y=min_dist, 
                     color=factor(var), label=name)) +
  geom_point() +
  geom_text_repel(min.segment.length = 0, 
                  seed = 42, box.padding = 0.5) +
  labs(x='Оценка за тест', y='Минимальное отличие') +
  theme_bw()


# add gender -------------------------------------------------------



# add question info -------------------------------------------------------

n_studs = nrow(hse_studs)

q_info = import('~/Documents/probability_test_bank/snapshots/2023-06-26-stat-final/question_info.csv')
q_info = select(q_info, -name, -var_raw) %>%
  rename(var=var_no, question=q_no, base_filename=filename,
         correct_letter=ans_letter)

hse_studs_long = pivot_longer(hse_studs,
                              cols=a01:a30, names_to = 'question', values_to = 'answer')

hse_studs_long = mutate(hse_studs_long, 
                        question = as.numeric(str_sub(question, start=2)),
                        var = var - 10,
                        answer = str_to_lower(answer))

hse_studs_long = left_join(hse_studs_long, q_info, 
                           by=c('var', 'question'))

hse_studs_long = mutate(hse_studs_long, 
                        correct = 1 * (answer == correct_letter))

# rank by question number in test -------------------------------------------------------
rank_by_number = group_by(hse_studs_long, question) %>%
  summarise(prop_correct = sum(correct) / n_studs)

ggplot(rank_by_number, aes(x=question, y=prop_correct)) +
  geom_point()

# rank by question content -------------------------------------------------------

rank_by_content = group_by(hse_studs_long, base_filename) %>%
  summarise(prop_correct = sum(correct) / n_studs) %>%
  arrange(prop_correct)

rank_by_content

# assign var 1 data 
stud_with_var_01 = filter(hse_studs_long, var == 1)
n_studs_var_01 = nrow(filter(hse_studs, var == 11))

letter_props = group_by(stud_with_var_01, base_filename, answer) %>%
  summarise(prop_letter = n() / n_studs_var_01)

q_info_var_01 = filter(q_info, var == 1) %>%
  select(base_filename, question, correct_letter) %>%
  rename(q_no_in_var_01=question, correct_letter_in_var_01=correct_letter)

rank_by_content = left_join(rank_by_content,
                            q_info_var_01, by='base_filename')
letter_props_wide = pivot_wider(letter_props, id_cols='base_filename',
                                names_from = 'answer', values_from = 'prop_letter',
                                values_fill = 0)
?pivot_wider
rank_by_content = left_join(rank_by_content, letter_props_wide, 
                            by='base_filename')

output = ''
for(q_no in 1:30) {
  line_01 = paste0('Процент верных по всем вариантам: ', 
  round(rank_by_content$prop_correct[q_no], 2), ' [', q_no, '/30]\n\n')
  
  line_02 = paste0('По первому варианту:\n\nНомер вопроса: ', 
                   rank_by_content$q_no_in_var_01[q_no], 
                   ', верный ответ: ', rank_by_content$correct_letter_in_var_01[q_no], '\n\n')
  line_03 = paste0('a: ', round(rank_by_content$a[q_no], 2),
                   ', b: ', round(rank_by_content$b[q_no], 2),
                   ', c: ', round(rank_by_content$c[q_no], 2),
                   ', d: ', round(rank_by_content$d[q_no], 2),
                   ', e: ', round(rank_by_content$e[q_no], 2),
                   ', f: ', round(rank_by_content$f[q_no], 2),
                   ', x: ', round(rank_by_content$x[q_no], 2),
                    '\n\n')
  tex_line = paste0('\\input{exercise', 
                    str_pad(as.character(q_no), width = 2, pad = '0'), 
                    '}\n\n')
  line_04 = paste0('\\url{', rank_by_content$base_filename[q_no], '}\n\n')
  
  cat(tex_line)
  cat(line_01)
  cat(line_02)
  cat(line_03)
  cat(line_04)
  cat('\n\n')
  
  output= paste0(output, tex_line, line_01, line_02, line_03, line_04, '\n\n')
}

cat(output, file = '~/Documents/probability_hse_2022_23/exam-report-main.tex')







