{application, showbiz,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [more_news_handler, showbiz_util, more_dtl, showbiz_sup, photo_gallery_dtl, video_handler, more_handler, feed_videos_handler, tnc_page_handler, home_page_handler, videos_count_handler, index_dtl, more_news_dtl, more_videos_dtl, more_videos_handler, photo_gallery_handler, tnc_dtl, news_limit_skip_handler, video_dtl, channel_gallery_handler, channel_latest_news_handler, news_count_handler, showbiz_app]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  goldrush,
                  lager,
                  cowboy,
                  jsx,
                  ibrowse,
                  erlydtl
                 ]},
  {mod, { showbiz_app, []}},
  {env, []}
 ]}.
