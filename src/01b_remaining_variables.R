analy_pop <- data_long_w2 %>% 
  select(-c(starts_with("policy"), starts_with("outcome"), starts_with("impact"), gov_level, court_level)) %>% 
  unique() %>% 
  ungroup()


#poach redcaps code to assign values
analy_pop$redcap_event_name.factor = factor(analy_pop$redcap_event_name,levels=c("data_extraction_arm_1","pilot_arm_1"))
analy_pop$redcap_repeat_instrument.factor = factor(analy_pop$redcap_repeat_instrument,levels=c("data_extraction_form"))
analy_pop$paper_type.factor = factor(analy_pop$paper_type,levels=c("1","2","3"))
analy_pop$country_yn.factor = factor(analy_pop$country_yn,levels=c("1","0"))
analy_pop$country.factor = factor(analy_pop$country,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","190","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189"))
analy_pop$competing_interests_yn.factor = factor(analy_pop$competing_interests_yn,levels=c("1","0"))
analy_pop$competing_interests.factor = factor(analy_pop$competing_interests,levels=c("1","2","3"))
analy_pop$empirical.factor = factor(analy_pop$empirical,levels=c("1","2"))
analy_pop$appeal_yn.factor = factor(analy_pop$appeal_yn,levels=c("1","0"))
#analy_pop$outcomes_yn.factor = factor(analy_pop$outcomes_yn,levels=c("1","0","999"))



levels(analy_pop$redcap_event_name.factor)=c("Data Extraction","Pilot")
levels(analy_pop$redcap_repeat_instrument.factor)=c("Data Extraction Form")
levels(analy_pop$paper_type.factor)=c("Health Sciences","Social Sciences","Legal")
levels(analy_pop$country_yn.factor)=c("Yes","No")
levels(analy_pop$country.factor)=c("Afghanistan","Albania","Algeria","Angola","Antigua and Barbuda","Argentina","Armenia","Australia","Austria","Azerbaijan","The Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bhutan","Bolivia","Bosnia and Herzegovina","Botswana","Brazil","Brunei","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Canada","Cape Verde","Central African Republic","Chad","Chile","Colombia","Comoros","Democratic Republic of the Congo","Republic of the Congo","Costa Rica","Côte dIvoire","Croatia","Cyprus","Czech Republic","Denmark","Djibouti","Dominica","Dominican Republic","East Timor","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Fiji","Finland","France","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Grenada","Guatemala","Guinea","Guinea-Bissau","Guyana","Haiti","Honduras","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kiribati","Kosovo","Kuwait","Kyrgyzstan","Laos","Latvia","Lebanon","Lesotho","Liberia","Libya","Lithuania","Luxembourg","Macedonia","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Marshall Islands","Mauritania","Mauritius","Mexico","Federated States of Micronesia","Moldova","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Norway","Oman","Pakistan","Palau","Panama","Papua New Guinea","Paraguay","Peoples Republic of China","Puerto Rico","Peru","Philippines","Poland","Portugal","Qatar","Romania","Russia","Rwanda","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Samoa","San Marino","São Tomé and Príncipe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","South Korea","South Sudan","Spain","Sri Lanka","Sudan","Suriname","Swaziland","Sweden","Switzerland","Syria","Tajikistan","Tanzania","Thailand","Togo","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Tuvalu","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uruguay","Uzbekistan","Vanuatu","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe")
levels(analy_pop$appeal_yn.factor)=c("Yes","No")
#levels(analy_pop$outcomes_yn.factor)=c("Yes","No","Unclear")
levels(analy_pop$empirical.factor)=c("empirical","non-empirical")
levels(analy_pop$competing_interests_yn.factor)=c("Yes","No")
levels(analy_pop$competing_interests.factor)=c("Yes","No","Unclear")

analy_pop_f <- analy_pop %>% 
  mutate_all(as.character) %>% 
  select(record_id, redcap_event_name.factor, redcap_repeat_instrument.factor, redcap_repeat_instance, paper_type.factor, name, dv_name, year_pub, country_yn.factor, country.factor, competing_interests_yn.factor, competing_interests.factor, empirical.factor, year_study, appeal_yn.factor, comments, data_extraction_form_complete, country_income, country_region, target_substance, population_specific, population_specific_affect, study_design, data_sources, analysis)
