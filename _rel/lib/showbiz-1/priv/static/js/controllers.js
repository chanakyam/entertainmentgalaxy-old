var app = angular.module('showbiz', ['ui.bootstrap']);

app.factory('ShowbizHomePageService', function ($http) {
	return {		

		getChannelPictures: function (category, count, skip) {
			return $http.get('/api/latestnews/channel?c=' + category + '&l=' + count + '&skip=' + skip).then(function (result) {
				// return result.data.rows;
				return result.data.articles;
			});
		},		
		getImages: function (category, count, skip) {
			return $http.get('/api/imageGallery/channel?c=' + category + '&l=' + count + '&skip=' + skip).then(function (result) {
				// return result.data.rows;
				return result.data.articles;
			});
		},
		getVideo: function (category, count, skip) {
			return $http.get('/api/videos/channel?c=' + category + '&l=' + count + '&skip=' + skip).then(function (result) {
				// return result.data.rows;
				return result.data.articles;
			});

		}		
	};
});

app.controller('ShowbizHome', function ($scope, ShowbizHomePageService) {
  //the clean and simple way
   $scope.latestVideos     = ShowbizHomePageService.getVideo('full_composite_article',3,0);
   $scope.popularVideos    = ShowbizHomePageService.getVideo('full_composite_article',6,3);
   $scope.latestNews 	   = ShowbizHomePageService.getChannelPictures('media_content',7,0);
   $scope.imageGallery     = ShowbizHomePageService.getImages('image_gallery_view',6,0);
   $scope.latestVideosData = ShowbizHomePageService.getVideo('full_composite_article',6,0);	
   $scope.currentYear 	   = (new Date).getFullYear();
  //for video player
  	var video = "http://video.contentapi.ws/"+$('#video_val').val() 	
 	// start of code for generating cb,pagetit,pageurl
 	var vastURI = 'http://vast.optimatic.com/vast/getVast.aspx?id=en732n1l1f3&zone=vpaidtag&pageURL=[INSERT_PAGE_URL]&pageTitle=[INSERT_PAGE_TITLE]&cb=[CACHE_BUSTER]';
	function updateURL(vastURI){
	// Generate a huge random number
	var ord=Math.random(), protocol, host, port, path, pageUrl, updatedURI;
	var parsedFragments = parseUri(vastURI);
	ord = ord * 10000000000000000000;
	// Protocol of VAST URI
	protocol = parsedFragments.protocol;
	// VAST URI hostname
	host = parsedFragments.host;
	// VAST URI Path
	path = parsedFragments.path;
	//VAST Page Url
	pageUrl = parsedFragments.queryKey.pageUrl
	var fragmentString ='';
	//Updated URI
	for(var key in parsedFragments.queryKey){//console.log("abhii");console.log();
		// For Cache buster add a large random number
		if(key == 'cb'){
			fragmentString = fragmentString + key + '=' + ord + '&';	
		}
		// for referring Page URL, get the current document URL and encode the URI
		else if(key == 'pageURL'){
			var currentUrl = document.URL;
			fragmentString = fragmentString + key + '=' + currentUrl + '&';	
		}else if(key == 'pageTitle'){
			var page_title=document.title;
			fragmentString = fragmentString + key + '=' + page_title + '&';	
		}
		else{
			fragmentString = fragmentString + key + '=' + parsedFragments.queryKey[key] + '&';
		}
		
	}

	updatedURI = protocol + '://' + host + path + '?' + fragmentString ;
	
	// Remove the trailing & and return the updated URL
	return updatedURI.slice(0,-1);	
	}

	// Parse URI to get qeury string like cb for cache buster and pageurl
	function parseUri (str) {
		var	o   = parseUri.options,
			m   = o.parser[o.strictMode ? "strict" : "loose"].exec(str),
			uri = {},
			i   = 14;

		while (i--) uri[o.key[i]] = m[i] || "";

		uri[o.q.name] = {};
		uri[o.key[12]].replace(o.q.parser, function ($0, $1, $2) {
			if ($1) uri[o.q.name][$1] = $2;
		});

		return uri;
	};

	parseUri.options = {
		strictMode: false,
		key: ["source","protocol","authority","userInfo","user","password","host","port","relative","path","directory","file","query","anchor"],
		q:   {
			name:   "queryKey",
			parser: /(?:^|&)([^&=]*)=?([^&]*)/g
		},
		parser: {
			strict: /^(?:([^:\/?#]+):)?(?:\/\/((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?))?((((?:[^?#\/]*\/)*)([^?#]*))(?:\?([^#]*))?(?:#(.*))?)/,
			loose:  /^(?:(?![^:@]+:[^:@\/]*@)([^:\/?#.]+):)?(?:\/\/)?((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?)(((\/(?:[^?#](?![^?#\/]*\.[^?#\/.]+(?:[?#]|$)))*\/?)?([^?#\/]*))(?:\?([^#]*))?(?:#(.*))?)/
		}
	};
	// end of code for generating cb,pagetit,pageurl
	$scope.loadVideo = function(){
		$(document).ready(function() {
			jwplayer('trailor').setup({
                  "flashplayer": "http://player.longtailvideo.com/player.swf",
                  "playlist": [
                    {
                      "file": video
                    }
                  ],
                  "width": 984,
                  "height": 503,
                  stretching: "exactfit",
                  skin: "../images/playicon.png",
                  autostart: true,
                  "controlbar": {
                    "position": "bottom"
                  },
                  "plugins": {
                  	 "autoPlay": true,
                    "ova-jw": {
                      "ads": {
                        "schedule": [
                          {
                            "position": "pre-roll",
                            "tag": updateURL(vastURI)
                          }
                        ]
                      },
                      "debug": {
                        "levels": "none"
                      }
                    }
                  }
                });
		})
	
	};
});