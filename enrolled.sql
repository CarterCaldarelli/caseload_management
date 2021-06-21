-- parameter for academic year
with    param_year as (select 2021 as acyr from dual)
-- build parameter for the terms in the academic year, winter & spring of acyr, summer & fall prior to acyr
      , param_term as (select    term_code                                        termcode
                         from    termcodes
                        where    term_acyr_code in (select acyr from param_year)
                          and    (substr(term_desc, 1, 4) = 'Fall'
                           or    substr(term_desc, 1, 6) = 'Winter'
                           or    substr(term_desc, 1, 6) = 'Ppring')
                        union
                       select    term_code as termcode
                         from    termcodes
                        where    term_acyr_code in (select acyr - 1 from param_year)
                          and    substr(term_desc, 1, 6) = 'Summer')
      , cur_term as   (select    max(term_code) as current_term
                         from    termcodes
                        where    (select trunc(sysdate) from dual) > term_start_date
                          and    term_start_date < (select trunc(sysdate) from dual)) 

      , t_ms_cat as (select    distinct  stu_pidm as t_ms_pidm
                             , case when enr_category_code = 'New' 
                                    then 'First term at NLU'
                                    when nvl(ug_gpa.credits, 0) + enr_hours_bill >= 180 
                                    then 'Graduating term at NLU'
                                    else 'Mid-Program at NLU'
                                end at_milestone_cat
                  --if new, then set term to first_dg_ug_past_year
                  --if first_dg_ug_past_year is the same as the quarter immediately prior to reporting term (last_ds_ug), then student is in second term (i.e. first term enrolled in last year is 202090 and report term is 202110)
                      from    student_table
                            , enrollment_table
                            , (select    enr_pidm as pidm
                                       , min(enr_term_code) as termcode
                                 from    enrollment_table
                                where    enr_nds_code = 'DW'
                                  and    enr_level_code = 'UG'
                                  and    enr_degree_code in('BA','BS')
                                  and    enr_term_code < (select current_term from cur_term)
                                  and    enr_term_code >= (select current_term - 100 from cur_term)
                             group by    enr_pidm) first_ds_ug_past_year
                            , (select    enr_pidm as pidm
                                       , max(enr_term_code) as termcode
                                 from    enrollment_table
                                where    enr_nds_code = 'DS'
                                  and    enr_level_code = 'UG'
                                  and    enr_degree_code in ('BA','BS')
                                  and    enr_term_code < (select current_term from cur_term)
                             group by    enr_pidm) last_ds_ug
                            , (select    lgpa_pidm as pidm
                                       , lgpa_overall_earned as credits
                                       , trunc(lgpa_overall_gpa, 2) as gpa
                                 from    gpa_table
                                where    lgpa_level_code = 'UG') ug_gpa
                   where    stu_pidm = enr_pidm
                     and    stu_pidm = last_ds_ug.pidm (+)
                     and    stu_pidm = first_ds_ug_past_year.pidm (+)
                     and    stu_pidm = ug_gpa.pidm (+)
                     and    enr_level_code = 'UG'
                     and    enr_nds_code = 'DS'
                     and    enr_degree_code in ('BA','BS')
                     and    enr_pidm not in (select    distinct hp3_pidm 
                                               from    hp3_table 
                                              where    hp3_term_code in(select current_term from cur_term))
                     and    enr_term_code in (select current_term from cur_term))

      , deg_apps    as (select    dap_pidm
                                , dap_term_desc
                          from    degree_apps
                         where    dap_term_code >= (select current_term from cur_term))
                         
      , grads       as (select    distinct deg_pidm
                                , deg_term_desc
                                , count(*) as grad_count
                                , row_number() over (order by deg_pidm) as counter
                          from    graduation_table
                         where    deg_term_code in (select termcode from param_term)
                         group by deg_pidm, deg_term_desc)

      , crds_pre_cur_term as (select    tgpa_pidm                                                 term_pidm
                                      , rec_credit_term.tc_term                                   max_term
                                      , nvl(tgpa_cumulative_hrs_earned,0) + enr_hours_bill        estimated_credits
                                      , enr_term_desc                                             estimated_term
                                from    gpa_table
                                      , enrollment_table
                                      , (  select    tgpa_pidm as tc_pidm
                                                   , max(tgpa_term_code) as tc_term
                                             from    gpa_term_table
                                            where    tgpa_term_code < (select current_term from cur_term)
                                              and    tgpa_level_code = 'UG'
                                         group by   tgpa_pidm) rec_credit_term
                               where     tgpa_pidm = tc_pidm
                                 and     tc_pidm = enr_pidm
                                 and     tgpa_term_code = rec_credit_term.tc_term
                                 and     enr_term_code = (select current_term from cur_term)
                                 and     tgpa_level_code = 'UG'
                                 and     (nvl(tgpa_cumulative_hrs_earned,0) + enr_hours_bill) >= 180)
      , cur_enr_bill as (select    enr_hours_bill
                           from    enrollment_table
                          where    enr_term_code = (select current_term from cur_term))

-- a lookup table used to count how many non-summer terms exist in ban_termcodes
      , p_ant_grad_xwalk as (select    term_desc 
                                     , term_code
                                     , term_fa_proc_yr
                                     , row_number() over (order by term_code) as counter
                               from    termcodes
                              where    term_code >= (select current_term from cur_term)
                                and    substr(term_code, 5, 2) in ('90','10','30'))

      , t_ant_grad_xwalk as (select    term_desc 
                                     , term_code
                                     , term_fa_proc_yr
                                     , row_number() over (order by term_code) as counter
                               from    termcodes
                              where    term_code >= (select current_term from cur_term))

      , car_218_reg as (select    distinct reg_pidm as car_pidm
                          from    registration_table
                         where    reg_crse_subj_code||' '||reg_crse_numb = 'CAR 218'
                           and    reg_term_code in (select current_term from cur_term)
                           and    reg_crse_status_code = 'RE')
                          
-- main query, select columns needed in power bi report    
select distinct   stu_pidm                      pidm                 
                , stu_id                        id
                , stu_first                     first
                , stu_last                      last 
                , stu_first||' '||stu_last      name
                , stu_email_nlu                 email
                , recent.deg_level              deg_level
                , recent.college                college
                , recent.program                program
                , recent.prog_desc              prog_desc
                , recent.major                  major
                , case when recent.partnership in ('Non-partnership', 'NCE Partnerships', 'Day Time')
                       then 'Transfer'
                       when recent.partnership in ('Helix Online','NLU Online')
                       then 'NLU Online'
                       else recent.partnership            
                   end partnership 
                , recent.coach                  coach 
                , case when  recent.deg_level = 'UG'
                       then  nvl(ug_gpa.credits,0)
                   end credits
                , case when  recent.deg_level = 'UG'
                        and  nvl(ug_gpa.credits, 0) between 0 and 44.9
                       then  '< 45'
                       when  recent.deg_level = 'UG'
                        and  nvl(ug_gpa.credits, 0) between 45 and 89.9
                       then  '45 - 89.9'
                       when  recent.deg_level = 'UG'
                        and  nvl(ug_gpa.credits, 0) between 90 and 134.9
                       then  '90 - 134.9'
                       when  recent.deg_level = 'UG'
                        and  nvl(ug_gpa.credits, 0) >= 135
                       then  '>= 135'
                   end credit_band       
                , nvl(ug_gpa.gpa, null)         gpa
                , case when substr(recent.termcode,5,2) = '10'
                       then 'Winter'||' '||to_char(substr(recent.termcode,1,4))
                       when substr(recent.termcode,5,2) = '30'
                       then 'Spring'||' '||to_char(substr(recent.termcode,1,4))
                       when substr(recent.termcode,5,2) = '60'
                       then 'Summer'||' '||to_char(substr(recent.termcode,1,4))
                       when substr(recent.termcode,5,2) = '90'
                       then 'Fall'||' '||to_char(substr(recent.termcode,1,4))
                   end recent_enrolled
                , case when recent.pidm in (select deg_pidm from grads)
                       then (select    distinct deg_term_desc||' Graduate'
                               from    grads
                              where    recent.pidm = deg_pidm)
                       when recent.pidm in (select dap_pidm from deg_apps)
                       then (select    dap_term_desc||' Degree Applicant'
                               from    deg_apps
                              where    recent.pidm = dap_pidm)
                       when recent.pidm in (select term_pidm from crds_pre_cur_term)
                       then (select    estimated_term||' Estimated'
                               from    crds_pre_cur_term
                              where    recent.pidm = term_pidm)
                       when recent.pidm not in (select deg_pidm from grads) 
                        and ug_gpa.credits > 180
                       then 'N/A'
                       when p_ant_grad_xwalk.term_desc is null or t_ant_grad_xwalk.term_desc is null
                       then 'N/A'
                       when recent.partnership = 'Pathways'
                       then p_ant_grad_xwalk.term_desc||' Estimated'
                       when recent.partnership in ('Non-Partnership', 'NCE Partnerships', 'Day Time','Helix Online','NLU Online')
                       then t_ant_grad_xwalk.term_desc||' Estimated'
                       else 'Missed'
                  end  graduation
                , recent.campus
                , case when recent.deg_level = 'GE'
                       then 'Masters'
                       when recent.partnership = 'Pathways' 
                        and f_pathways_year(hp3_pidm, hp3_term_code) = 'Y1' 
                       then 'Freshman'
                       when recent.partnership = 'Pathways'
                        and f_pathways_year(hp3_pidm, hp3_term_code) = 'Y2' 
                       then 'Sophomore'
                       when recent.partnership = 'Pathways'
                        and f_pathways_year(hp3_pidm, hp3_term_code) = 'Y3' 
                       then 'Junior'
                       when recent.partnership = 'Pathways'
                        and f_pathways_year(hp3_pidm, hp3_term_code) = 'Y4' 
                       then 'Senior'
                       when recent.partnership = 'Pathways'
                        and f_pathways_year(hp3_pidm, hp3_term_code) in ('Y5','Y6','Y7+') 
                       then 'Continuing Senior'
                       when recent.partnership not in ('Pathways')
                        and  (ug_gpa.credits is null 
                         or  ug_gpa.credits < 45)
                       then  'Freshman'
                       when recent.partnership not in ('Pathways')
                        and  ug_gpa.credits between 45 and 89.9
                       then  'Sophomore'
                       when recent.partnership not in ('Pathways')
                        and  ug_gpa.credits between 90 and 134.9
                       then  'Junior'
                       when  ug_gpa.credits >= 135
                       then  'Senior'
                        end  school_year
                , case when recent.partnership in ('Non-Partnership', 'NCE Partnerships', 'Day Time', 'Helix Online','NLU Online')
                       then  (select   at_milestone_cat
                                from   t_ms_cat
                               where   stu_pidm = t_ms_pidm)
                       else  null
                   end  transfer_ms
                , case when  recent.deg_level = 'GR'
                        and  recent.partnership not in ('Helix Online', 'NLU Online')
                        and  recent.campus not in ('TA')
                       then  'Graduate/Alumni Advisor'

                       when  recent.program in ('BS MGT', 'BS HCL', 'BA ABS', 'BS MIS')
                       then  'Matthew Johnson'

                       when  recent.campus = 'NT'
                        and  recent.program in (  'BA ECP'
                                          , 'BA ECE'
                                          , 'BA SS/ECE'
                                          , 'BA LAS/ECE'
                                          , 'BA ELED'
                                          , 'BA SS/ELED'
                                          , 'BA LAS/ELED'
                                          , 'BA SPE'
                                          , 'BA SS/SPE'
                                          , 'BA LAS/SPE'
                                          , 'BA_ECED'
                                          , '00 PBECE'
                                          , 'ND_ECED_CERT')
                       then  'Matthew Johnson'

                       when  recent.partnership in ('Helix online', 'NLU Online')
                       then  'Olivia Smith'

                       when  recent.campus = 'WH'
                        and  recent.deg_level not in ('GR')
                       then  'Bushra Sarkar'

                       when  recent.campus not in ('WH', 'TA')
                        and  recent.program in (  'BA ECP'
                                                , 'BA ECE'
                                                , 'BA SS/ECE'
                                                , 'BA LAS/ECE'
                                                , 'BA ELED'
                                                , 'BA SS/ELED'
                                                , 'BA LAS/ELED'
                                                , 'BA SPE'
                                                , 'BA SS/SPE'
                                                , 'BA LAS/SPE'
                                                , 'BA_ECED'
                                                , 'BA GSE'
                                                , '00 PBECE'
                                                , 'ND_ECED_CERT')
                       then  'Dee Williams'

                       when   recent.campus not in ('WH', 'TA')
                        and   recent.program in (   'BA BA'
                                                  , 'BS CIS'
                                                  , 'BA COMM'
                                                  , 'BA AC'
                                                  , 'BA_BUAD'
                                                  , 'BA_BUAD_ONL'
                                                  , 'BA_BUAD_PD')
                       then   'Matt Vicars'

                       when   recent.campus not in ('WH', 'TA')
                        and   recent.program in (   'BA CJ'
                                                  , 'BA PSYCH'
                                                  , 'BA HMS'
                                                  , 'BA SOCSCI')
                       then   'Jeanne McGill'

                       when  recent.program in (  'AAS_CULA'
                                                , 'BA_HOSM'
                                                , 'BA_CULA'
                                                , 'AAS_CULAACL'
                                                , 'AAS_BAPA'
                                                , 'CRTU PC'
                                                , 'CRTU BAPA'
                                                , 'BA_CULA_PD'
                                                , 'BA_HOSM_PD')
                       then  'Deb Bosco'
                       else  'Unassigned'
                        end  career_advisor
                , case when  recent.pidm in (select car_pidm
                                 from car_218_reg
                                where car_pidm = recent.pidm)
                     then 'Yes'
                     else 'No'
                   end car_218
                , stu_city
                , substr(stu_zip, 1,5)          zipcode
                , stu_state
from    student_table
     -- subquery, variables that should be pulled as of the most recent term the student has been enrolled
      , (select   enr_pidm                      pidm
                , rec_term.termcode             termcode 
                , enr_program_code              program
                , enr_program_desc              prog_desc
                , enr_major_desc                major
                , enr_campus_code               campus
                , enr_advisor                   coach
                , enr_partnership_type          partnership
                , enr_level_code                deg_level
                , enr_college_code              college
           from   enrollment_table
                -- subquery, identify the most recent term that the student has been enrolled. 
                , ( select    enr_pidm as pidm
                            , max(enr_term_code) as termcode
                      from    enrollment_table
                  group by    enr_pidm) rec_term
          where    enr_pidm = rec_term.pidm
            and    enr_term_code = rec_term.termcode) recent
      , (select    lgpa_pidm as pidm
                 , lgpa_overall_earned as credits
                 , trunc(lgpa_overall_gpa, 2) as gpa
           from    gpa_table
          where    lgpa_level_code = 'UG') ug_gpa
      , hp3_table
      , terms
      , p_ant_grad_xwalk
      , t_ant_grad_xwalk
where    stu_pidm = recent.pidm    
  and    recent.pidm = ug_gpa.pidm (+)
  and    recent.pidm = hp3_pidm (+)
  and    recent.termcode = hp3_term_code (+)
  and    recent.termcode = termxwalk_term (+)
  and    ceil((180 - nvl(ug_gpa.credits,0)) / 15) = p_ant_grad_xwalk.counter (+)
  and    ceil((180 - nvl(ug_gpa.credits,0)) / 15) = t_ant_grad_xwalk.counter (+)
  and    (recent.termcode in (select termcode from param_term)
   or     recent.termcode >= 202060)
