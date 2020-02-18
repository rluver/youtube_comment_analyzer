library(memoise)




# comment crawling function
get_youtube_comment = function(url){
  if(str_detect(url, "youtube.com/watch\\?v=.+") == T){
    
    return(
      Authenticate("youtube", "") %>% 
        Collect(GetYoutubeVideoIDs(url), 
                writeToFile = F, 
                maxComments = 100000) %>% 
        arrange(PublishTime)
    )
  }
}




# filter function
word_filter = function(comment){
  
  comment = str_replace(comment, "@.+", "")
  
  comment = str_to_upper(comment)
  comment = unlist(str_split(comment, "\\.{2,}"))
  comment = str_replace(comment, "\\.+", "")
  comment = str_replace(comment, "욬[ㅋ]+", "요")
  
  comment = str_replace(comment, "^돌+i$", "또라이")
  comment = str_replace(comment, "^돌i$", "또라이")
  comment = str_replace(comment, "새해복", "새해 복")
  comment = str_replace(comment, "민철민아", "민철 민아")
  comment = str_replace(comment, "민아민철", "민아 민철")
  comment = str_replace(comment, "^코리안 코커$", "코리안코커")
  
  
  comment = unlist(str_split(comment, "[~`!@#$%^&*(){};',.<>/?+-_|\\[\\]]+"))
  comment = unlist(str_split(comment, "[ㄱ-ㅎ]+"))
  comment = unlist(str_split(comment, "[ㅏ-ㅣ]+"))
  comment = unlist(str_split(comment, "♡+"))
  comment = unlist(str_split(comment, "♥+"))
  comment = unlist(str_split(comment, "z+"))
  comment = unlist(str_split(comment, "k+"))
  # comment = unlist(str_split(comment, "x?"))
  
  comment = unlist(str_split(comment, "vs"))
  comment = unlist(str_split(comment, "같은$"))
  comment = unlist(str_split(comment, "님께$"))
  comment = unlist(str_split(comment, "때문에$"))
  comment = unlist(str_split(comment, "를$"))
  comment = unlist(str_split(comment, "형도$"))
  comment = unlist(str_split(comment, "누나도$"))
  comment = unlist(str_split(comment, "들은$"))
  comment = unlist(str_split(comment, "중에$"))
  comment = unlist(str_split(comment, "님이랑$"))
  comment = unlist(str_split(comment, "이니까$"))
  comment = unlist(str_split(comment, "한테$"))
  comment = unlist(str_split(comment, "이랑$"))
  comment = unlist(str_split(comment, "한테는$"))
  comment = unlist(str_split(comment, "님도$"))
  comment = unlist(str_split(comment, "해도$"))
  comment = unlist(str_split(comment, "이라는$"))
  comment = unlist(str_split(comment, "[.]+님이$"))
  comment = unlist(str_split(comment, "[.]+님과$"))
  
  
  # common
  comment = str_replace(comment, "돌아이", "또라이")
  comment = str_replace(comment, "누난$", "누나")
  comment = str_replace(comment, "앜", "아")
  comment = str_replace(comment, "엌", "어")
  comment = str_replace(comment, "얔", "야")
  comment = str_replace(comment, "돜$", "도")
  comment = str_replace(comment, "맨탈", "멘탈")
  comment = str_replace(comment, "횽", "형")
  comment = str_replace(comment, "넼", "네")
  comment = str_replace(comment, "PD님", "PD")
  comment = str_replace(comment, "피디가", "피디")
  comment = str_replace(comment, "^악플도$", "악플")
  comment = str_replace(comment, "^구독잡니다$", "구독자")
  comment = str_replace(comment, "^믿힌$", "미친")
  comment = str_replace(comment, "님과$", "")
  comment = str_replace(comment, "잖어$", "")
  comment = str_replace(comment, "잖아$", "")
  comment = str_replace(comment, "에요$", "")
  comment = str_replace(comment, "예요$", "")
  comment = str_replace(comment, "^해서$", "")
  comment = str_replace(comment, "^근데$", "")
  comment = str_replace(comment, "^하시$", "")
  comment = str_replace(comment, "^하니$", "")
  comment = str_replace(comment, "^안간$", "")
  comment = str_replace(comment, "^때문$", "")
  comment = str_replace(comment, "하면서$", "")
  comment = str_replace(comment, "^같애서$", "")
  comment = str_replace(comment, "^이럴$", "")
  comment = str_replace(comment, "^치나봐$", "")
  comment = str_replace(comment, "^고싶$", "")
  comment = str_replace(comment, "^실더$", "쉴드")
  comment = str_replace(comment, "^쉴더$", "쉴드")
  comment = str_replace(comment, "^쌉가$", "쌉가능")
  
  
  # whyman
  comment = str_replace(comment, "^왜냐맨이$", "왜냐맨")
  comment = str_replace(comment, "^킹냐맨$", "왜냐맨")
  comment = str_replace(comment, "^민철님$", "민철")
  comment = str_replace(comment, "^민철군$", "민철")
  comment = str_replace(comment, "^민철좌$", "민철")
  comment = str_replace(comment, "^민철아$", "민철")
  comment = str_replace(comment, "^민철씨$", "민철")
  comment = str_replace(comment, "^민철형$", "민철")
  comment = str_replace(comment, "^민철이도$", "민철")
  comment = str_replace(comment, "^민철님이$", "민철")
  comment = str_replace(comment, "^장민철은$", "장민철")
  comment = str_replace(comment, "^장민철도$", "장민철")
  comment = str_replace(comment, "^민철이형$", "민철")
  comment = str_replace(comment, "^민철이형님$", "민철")
  
  comment = str_replace(comment, "^민아님$", "민아")
  comment = str_replace(comment, "^민아님이$", "민아")
  comment = str_replace(comment, "^민아가$", "민아")
  comment = str_replace(comment, "^민아씨$", "민아")
  comment = str_replace(comment, "^민아야$", "민아")
  comment = str_replace(comment, "^민아도$", "민아")
  comment = str_replace(comment, "^김민아도$", "김민아")
  comment = str_replace(comment, "^김민$", "김민아")
  comment = str_replace(comment, "^누나가$", "누나")
  comment = str_replace(comment, "^누난$", "누나")
  comment = str_replace(comment, "^민아누나$", "민아")
  comment = str_replace(comment, "^민아누님$", "민아")
  comment = str_replace(comment, "^민아누나가$", "민아")
  
  comment = str_replace(comment, "김스카이", "김하늘")
  comment = str_replace(comment, "^김하늘피디$", "김하늘")
  comment = str_replace(comment, "^김하늘피디님$", "김하늘")
  comment = str_replace(comment, "^하늘이형$", "김하늘")
  comment = str_replace(comment, "피디님$", "피디")
  comment = str_replace(comment, "피디는$", "피디")
  comment = str_replace(comment, "하늘이형이$", "하늘")
  comment = str_replace(comment, "^피디님이$", "피디")
  comment = str_replace(comment, "^하늘피디$", "김하늘")
  comment = str_replace(comment, "^킹하늘$", "김하늘")
  comment = str_replace(comment, "^스카이킴$", "김하늘")
  
  comment = str_replace(comment, "^미춘누나$", "나미춘")
  comment = str_replace(comment, "^미춘이랑$", "나미춘")
  comment = str_replace(comment, "^태진이누나", "태진")
  
  comment = str_replace(comment, "아조씨", "아저씨")
  comment = str_replace(comment, "빡빡이 아저씨", "빡빡이아저씨")
  
  comment = str_replace(comment, "[:blank:]", "")
  
  comment = Filter (function(x){str_length(x) >= 2}, comment)
  
  return(comment)
}




word_filter2 = function(comment, from, to){
  
  return(mapply(str_replace, comment,
                pattern = str_c("^", from, "$"),
                replacement = to))
}




word_delete = function(comment, delete){
  if(str_length(delete) != 0){
    return(mapply(str_remove, comment, delete, SIMPLIFY = T, USE.NAMES = F) %>% 
             mapply(stringi::stri_remove_empty, .))
  } else
    return(comment)
}




get_comment_extracted = memoise(function(comment){
  
  return(mapply(extractNoun, comment %>% select("Comment")) %>% 
           mapply(word_filter, ., SIMPLIFY = T, USE.NAMES = F))
})




get_comment_dataframe = memoise(function(comment, time, start, end, max, from, to, delete){
  
  return(
    mapply(str_replace, 
           comment[start <= time & time <= end] %>% word_delete(delete), 
           pattern = str_c("^", from, "$"), 
           replacement = to, 
           SIMPLIFY = T, 
           USE.NAMES = F) %>% unlist() %>% table() %>% sort(decreasing = T) %>% 
      head(max) %>% data.frame() %>% rename(Word = ".")
  )}
)




get_comment_reply = function(comment){
  
  return(
    comment %>% filter(ReplyToAnotherUser != F) %>% 
      select(User, ReplyToAnotherUser, Comment, ParentID) %>%
      left_join(comment %>% select(Comment, CommentId) %>% 
                  rename(ParentID = CommentId), by = "ParentID") %>% 
      rename(Comment = Comment.y, 
             Reply = Comment.x,
             Replier = User,
             Commenter = ReplyToAnotherUser) %>% 
      select(1, 2, 5 ,3) %>% 
      filter(Replier != Commenter)
  )
}




get_comment_rule = memoise(function(comment, time, start, end, from, to, delete, supp, conf, table = F){
  
  if(table == T){
    return(
      
      mapply(str_replace, 
             comment[start <= time & time <= end] %>% word_delete(delete),
             pattern = str_c("^", from, "$"),
             replacement = to, 
             SIMPLIFY = T, 
             USE.NAMES = F) %>% 
        unique() %>% sapply(unique) %>% as("transactions") %>% 
        crossTable() %>% apriori(parameter = list(minlen = 2,
                                                  supp = supp, 
                                                  conf = conf),
                                 control = list(verbose = F))
    )
  } else if(table == F){
    return(
      
      as_data_frame(plot(mapply(str_replace, 
                                comment[start <= time & time <= end] %>% word_delete(delete),
                                pattern = str_c("^", from, "$"),
                                replacement = to, 
                                SIMPLIFY = T, 
                                USE.NAMES = F) %>% 
                           unique() %>% sapply(unique) %>% as("transactions") %>% 
                           crossTable() %>% apriori(parameter = list(minlen = 2,
                                                                     supp = supp, 
                                                                     conf = conf),
                                                    control = list(verbose = F)), 
                         method = "graph", 
                         plot_options = list(max = 10000,
                                             verbose = F),
                         control = list(
                           max = 10000,
                           verbose = F)),
                    what = "both")
    )}
})




get_ts_table = function(comment){
  
  return(
    do.call(bind_rows, 
            (comment %>% group_by(PublishTime) %>% tidyr::nest() %>% 
               mutate(data = list(head(sort(table(lapply(data, unlist)), 
                                            decreasing = T), -1))))$data) %>% 
      mutate(Days = ymd(unique(comment$PublishTime))) %>% 
      select(Days, 1:(dim(.)[2]-1)) %>% 
      tidyr::complete(Days = seq.Date(min(Days), ymd(max(str_sub(Sys.time(), 1, 10))), by = "days")) %>% 
      tidyr::gather(Word, freq, names(.)[-1]) %>% 
      zoo::na.fill0(fill = 0) %>% 
      group_by(Word) %>%
      mutate(Freq_acc = cumsum(freq)) %>% 
      rename(Freq = freq)
  )
}




get_ts_info = memoise(function(comment_extracted, comment, time, start, end, max, from, to, delete){
  
  return(
    
    get_ts_table(comment_extracted) %>% 
      filter(Word %in% get_comment_dataframe(comment, 
                                             time,
                                             start,
                                             end,
                                             max,
                                             from,
                                             to,
                                             delete)$Word)
  )
})
