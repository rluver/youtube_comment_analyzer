require("memoise")


# User → AuthorDisplayName

'%!in%' = Negate('%in%')


# comment crawling function
get_youtube_comment = function(url){
  
  # PublishedAt → PublishedAt
  if(str_detect(url, "youtube.com/watch\\?v=.+") == T){
      Authenticate("youtube", "") %>% 
        Collect(GetYoutubeVideoIDs(url), 
                writeToFile = F, 
                maxComments = 100000) %>% 
        arrange(PublishedAt) %>% 
      return()
  }
  
}




# filter function
word_filter = function(comment){
  
  comment %>% str_replace("@.+", "") %>% 
  
  str_to_upper() %>% 
  str_split("\\.{2,}") %>% unlist() %>% 
  str_replace("\\.+", "") %>% 
  str_replace("욬[ㅋ]+", "요") %>% 
  
  str_replace("^돌+i$", "또라이") %>% 
  str_replace("^돌i$", "또라이") %>% 
  str_replace("새해복", "새해 복") %>% 
  str_replace("민철민아", "민철 민아") %>% 
  str_replace("민아민철", "민아 민철") %>% 
  str_replace("^코리안 코커$", "코리안코커") %>% 
  
  
  str_split("[~`!@#$%^&*(){};',.<>/?+-_|\\[\\]]+") %>% unlist() %>% 
  str_split("[ㄱ-ㅎ]+") %>% unlist() %>% 
  str_split("[ㅏ-ㅣ]+") %>% unlist() %>% 
  str_split("♡+") %>% unlist() %>% 
  str_split("♥+") %>% unlist() %>% 
  str_split("z+") %>% unlist() %>% 
  str_split("k+") %>% unlist() %>% 
  # str_split("x?"))
  
  str_split("vs") %>% unlist() %>% 
  str_split("같은$") %>% unlist() %>% 
  str_split("님께$") %>% unlist() %>% 
  str_split("때문에$") %>% unlist() %>% 
  str_split("를$") %>% unlist() %>% 
  str_split("형도$") %>% unlist() %>% 
  str_split("누나도$") %>% unlist() %>% 
  str_split("들은$") %>% unlist() %>% 
  str_split("중에$") %>% unlist() %>% 
  str_split("님이랑$") %>% unlist() %>% 
  str_split("이니까$") %>% unlist() %>% 
  str_split("한테$") %>% unlist() %>% 
  str_split("이랑$") %>% unlist() %>% 
  str_split("한테는$") %>% unlist() %>% 
  str_split("님도$") %>% unlist() %>% 
  str_split("해도$") %>% unlist() %>% 
  str_split("이라는$") %>% unlist() %>% 
  str_split("[.]+님이$") %>% unlist() %>% 
  str_split("[.]+님과$") %>% unlist() %>% 
  
  
  # common
  str_replace("돌아이", "또라이") %>% 
  str_replace("누난$", "누나") %>% 
  str_replace("앜", "아") %>% 
  str_replace("엌", "어") %>% 
  str_replace("얔", "야") %>% 
  str_replace("돜$", "도") %>% 
  str_replace("맨탈", "멘탈") %>% 
  str_replace("횽", "형") %>% 
  str_replace("넼", "네") %>% 
  str_replace("PD님", "PD") %>% 
  str_replace("피디가", "피디") %>% 
  str_replace("^악플도$", "악플") %>% 
  str_replace("^구독잡니다$", "구독자") %>% 
  str_replace("^믿힌$", "미친") %>% 
  str_replace("님과$", "") %>% 
  str_replace("잖어$", "") %>% 
  str_replace("잖아$", "") %>% 
  str_replace("에요$", "") %>% 
  str_replace("예요$", "") %>% 
  str_replace("^해서$", "") %>% 
  str_replace("^근데$", "") %>% 
  str_replace("^하시$", "") %>% 
  str_replace("^하니$", "") %>% 
  str_replace("^안간$", "") %>% 
  str_replace("^때문$", "") %>% 
  str_replace("하면서$", "") %>% 
  str_replace("^같애서$", "") %>% 
  str_replace("^이럴$", "") %>% 
  str_replace("^치나봐$", "") %>% 
  str_replace("^고싶$", "") %>% 
  str_replace("^실더$", "쉴드") %>% 
  str_replace("^쉴더$", "쉴드") %>% 
  str_replace("^쌉가$", "쌉가능") %>% 
  
  
  # whyman
  str_replace("^왜냐맨이$", "왜냐맨") %>% 
  str_replace("^킹냐맨$", "왜냐맨") %>% 
  str_replace("^민철님$", "민철") %>% 
  str_replace("^민철군$", "민철") %>% 
  str_replace("^민철좌$", "민철") %>% 
  str_replace("^민철아$", "민철") %>% 
  str_replace("^민철씨$", "민철") %>% 
  str_replace("^민철형$", "민철") %>% 
  str_replace("^민철이도$", "민철") %>% 
  str_replace("^민철님이$", "민철") %>% 
  str_replace("^장민철은$", "장민철") %>% 
  str_replace("^장민철도$", "장민철") %>% 
  str_replace("^민철이형$", "민철") %>% 
  str_replace("^민철이형님$", "민철") %>% 
  
  str_replace("^민아님$", "민아") %>% 
  str_replace("^민아님이$", "민아") %>% 
  str_replace("^민아가$", "민아") %>% 
  str_replace("^민아씨$", "민아") %>% 
  str_replace("^민아야$", "민아") %>% 
  str_replace("^민아도$", "민아") %>% 
  str_replace("^김민아도$", "김민아") %>% 
  str_replace("^김민$", "김민아") %>% 
  str_replace("^누나가$", "누나") %>% 
  str_replace("^누난$", "누나") %>% 
  str_replace("^민아누나$", "민아") %>% 
  str_replace("^민아누님$", "민아") %>% 
  str_replace("^민아누나가$", "민아") %>% 
  
  str_replace("김스카이", "김하늘") %>% 
  str_replace("^김하늘피디$", "김하늘") %>% 
  str_replace("^김하늘피디님$", "김하늘") %>% 
  str_replace("^하늘이형$", "김하늘") %>% 
  str_replace("피디님$", "피디") %>% 
  str_replace("피디는$", "피디") %>% 
  str_replace("하늘이형이$", "하늘") %>% 
  str_replace("^피디님이$", "피디") %>% 
  str_replace("^하늘피디$", "김하늘") %>% 
  str_replace("^킹하늘$", "김하늘") %>% 
  str_replace("^스카이킴$", "김하늘") %>% 
  
  str_replace("^미춘누나$", "나미춘") %>% 
  str_replace("^미춘이랑$", "나미춘") %>% 
  str_replace("^태진이누나", "태진") %>% 
  
  str_replace("아조씨", "아저씨") %>% 
  str_replace("빡빡이 아저씨", "빡빡이아저씨") %>% 
  
  str_replace("[:blank:]", "") %>% 
  
  stringi::stri_remove_empty() %>% 
  
  Filter(function(x){str_length(x) >= 2}, .) %>% 
  
  return()
  
}




word_filter2 = function(comment, from, to){
  
  mapply(str_replace, 
         comment, pattern = str_c("^", from, "$"), replacement = to) %>% 
    return()
  
}




word_delete = function(comment, delete){
  
  if(str_length(delete) != 0){
    mapply(str_remove, comment, delete, SIMPLIFY = T, USE.NAMES = F) %>% 
      mapply(stringi::stri_remove_empty, .) %>% 
      return()
    
  } else
    
    comment %>% return()
  
}





get_comment_extracted = memoise(function(comment){
  
  mapply(extractNoun, comment %>% select("Comment")) %>% 
    mapply(word_filter, ., SIMPLIFY = T, USE.NAMES = F) %>% 
    return()
  
})




get_comment_dataframe = memoise(function(comment, time, start, end, max, from, to, delete){
  
    mapply(str_replace, 
           comment[start <= time & time <= end] %>% word_delete(delete), 
           pattern = str_c("^", from, "$"), 
           replacement = to, 
           SIMPLIFY = T, 
           USE.NAMES = F) %>% unlist() %>% table() %>% sort(decreasing = T) %>% 
      head(max) %>% data.frame() %>% rename(Word = ".") %>% 
    return()
  
})




get_comment_reply = function(comment){
  
    # ReplyToAnotherAuthorDisplayName → ReplyCount
    comment %>% filter('\\.' %!in% CommentID || ParentID != NA) %>% 
      select(AuthorDisplayName, ReplyToAnotherAuthorDisplayName, Comment, ParentID) %>%
      left_join(comment %>% select(Comment, CommentId) %>% 
                  rename(ParentID = CommentId), by = "ParentID") %>% 
      rename(Comment = Comment.y, 
             Reply = Comment.x,
             Replier = AuthorDisplayName,
             Commenter = ReplyToAnotherAuthorDisplayName) %>% 
      select(1, 2, 5 ,3) %>% 
      filter(Replier != Commenter) %>% 
    return()

}




get_comment_rule = memoise(function(comment, time, start, end, from, to, delete, supp, conf, table = F){
  
  if(table == T){
      
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
                                 control = list(verbose = F)) %>% 
      return()
  
  } else if(table == F){
    
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
                    what = "both") %>% 
      return()
  }
  
})




get_ts_table = function(comment){
  
  do.call(
    bind_rows, 
    comment %>% group_by(PublishedAt) %>% tidyr::nest() %>% 
      mutate(data = data %>% sapply(unlist) %>% 
               table() %>% sort(decreasing = T) %>% 
               head(-1) %>% list()) %>%
      .$data
    ) %>% 
    mutate(Days = ymd(unique(comment$PublishedAt))) %>% 
    select(Days, 1:(dim(.)[2]-1)) %>% 
    tidyr::complete(Days = seq.Date(min(Days), ymd(max(str_sub(Sys.time(), 1, 10))), by = "days")) %>% 
    tidyr::gather(Word, freq, names(.)[-1]) %>% 
    zoo::na.fill0(fill = 0) %>% 
    group_by(Word) %>%
    mutate(Freq_acc = cumsum(freq)) %>% 
    rename(Freq = freq) %>% 
  
    return()  
}




get_ts_info = memoise
(
  function(comment_extracted, comment, time, start, end, max, from, to, delete)
  {
    get_ts_table(comment_extracted) %>% 
      filter(Word %in% (get_comment_dataframe(comment, 
                                             time,
                                             start,
                                             end,
                                             max,
                                             from,
                                             to,
                                             delete) %>% select(Word) %>% 
              unlist())) %>% 

    return()
  }
)
