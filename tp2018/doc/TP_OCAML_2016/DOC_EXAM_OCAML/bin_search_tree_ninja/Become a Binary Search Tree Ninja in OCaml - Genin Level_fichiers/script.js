/*global $,google,NProgress,window,console,document *//*jshint multistr: true */

/* ============================================================ */
/* Layout Config
/* ============================================================ */

/*---------------------------
 Feature carousel settings
-----------------------------
1. To set to feature post priority (ie shows featured posts but if
   no feature post show latest posts) set: feature = true, latests = true
2. To set to feature posts only set: feature = true, latests = false
3. To set to latest posts only set: feature = false, latests = true
4. To change the number of latest posts in the carousel change: latestNum = *A NUMBER*
5. To turn off the feature carousel set: feature = false, latests = false
6. to change the cycle wait time change: sliderWait = *A NUMBER IN MILLISECONDS*
*/
var feature = true;
var latest = true;
var latestNum = 1;
var sliderWait = 7000;

/*---------------------------
 Setup homepage layout
-----------------------------
1. Even odd style - alternates between vertical and horizontal posts
2. All post items with portrait images have vertical styling else post items are horizontal
3. All post items are portrait
4. All post items are landscape
*/
var wookmarkLayout = 2;

/*---------------------------
 Turn on off news feed
-----------------------------
1.hide news theme by setting to false
2.hide news theme by setting to true
*/
var showNewsFeed = true;

/* ============================================================ */



/* ============================================================ */
/* Load functions when page is loaded */
/* ============================================================ */

checkShowNewsFeed();
loading = true;
NProgress.start();

$(function(){
    supportCheck();    
    function triggerWookmark(){
        try{
            $('#wookmark').imagesLoaded(function(){
                $('#wookmark article').css({ opacity: 0 });
                homepageLayout(wookmarkLayout);
                scrollFadeIn($('#wookmark article'));    
                NProgress.done();
                loading = false;
            });
        }
        catch(err){
                homepageLayout(wookmarkLayout);
                NProgress.done();
                loading = false;            
        }
    }
    $('body').removeClass("preload");
    $('.share').removeClass("preload");
    navBarSetup();
    if($(".home-template,.tag-template").length > 0){
        paginationSetup();        
        featureSection(feature,latest,latestNum);
        window.setTimeout(triggerWookmark,600);        
    }else{
        renderPageHeroImage();
        gmaps();
        NProgress.done();
        loading = false;
    }
    $(".postContent").fitVids();    
    try{ajaxRssData();}catch(err){}
    ajaxPageData();
    
    $(".ajaxLink, .ajaxTagLink").click(function() {
        close();
    });
    $("#sideBar li.openMenu span").click(function() {
        $("#mainNavCheck").prop('checked', true);
    });
    $("#sideBar .menu").click(function() {
        if(!$("#mainNavCheck").is(":checked")){
            $('#mainNav .autoExpand input[type="checkbox"]').prop('checked', true);
        }else{
            $('#mainNav input[type="checkbox"]').prop('checked', false);
        }
    });
    $("#sideBar nav").hover(function() {
        open();
    },function() {
        close();
    }); 
    
    function open(){
        $("#mainNavCheck").prop('checked', true);
        $('#mainNav .autoExpand input[type="checkbox"]').prop('checked', true);
    }
    function close(){
        $("#mainNavCheck").prop('checked', false);
        $('#mainNav input[type="checkbox"]').prop('checked', false);
    }

});
/* ============================================================ */





/* ============================================================ */
/* SideBar functions - Navigation and newsFeed */
/* ============================================================ */

function ajaxRssData(){
    var searchField = $("#search input").ghostHunter({
        results : "#feed",   
        displaySearchInfo   : true,
        zeroResultsInfo     : true,
        result_template:'   <div class="previewContainer {{noImg}}">\
                                <a class ="ajaxLink" href="{{link}}">\
                                    {{img}}\
                                    <div class="content">\
                                        <div class="dot"></div>\
                                        <div class="dateStamp">{{pubDate}}</div> \
                                            <h1 class="postTitle">{{title}}</h1> \
                                    </div>\
                                </a>\
                            </div>',
        info_template:' <div class="noResults">\
                            <div class="content">\
                                <h1 class="postTitle">Number of posts found: {{amount}}</h1> \
                            </div>\
                        </div>',
        onComplete : function( results ){
            previewHover();
            $('#feed .previewContainer').each(function(i){
                if( $('a',this).attr("href") == window.location.href ) {
                    $('.previewContainer').removeClass("active");
                    $(this).addClass("active");
                }
            });        
        
        },
        onReady : function(){
        
            previewHover();
	    var tags = searchField.listTags;
	    tags.sort(function(a, b){return a.toLowerCase().localeCompare(b.toLowerCase())});
            $.each(tags/*searchField.listTags*/, function(i,e){
                $('<li></li>').html('<a class="ajaxTagLink" href="/tag/'+e.replace(/\s+/g, '-').toLowerCase()+'">'+e+'</a>').appendTo('.listTags ul');
            });
            $(".ajaxTagLink").click(function() {
                $("#mainNavCheck").prop('checked', false);
                $('#mainNav input[type="checkbox"]').prop('checked', false);
            });
                       
        
        }
    });


    $('body').on('click', '#tags .ajaxTagLink', function(e) {
        e.preventDefault();
        var search = "tag-"+$(this).text().replace(/\s+/g, '-').toLowerCase()+"-tag";
        searchField.find( search );
        $("#tags .ajaxTagLink").removeClass("active");
        $(this).addClass("active");
    });

    function previewHover(){
        $('#feed .previewContainer').hover(function() {
            if($('.prevImage',this).length > 0){
                $(this).addClass("hov");
            }            
        },
        function() {
            $(this).removeClass("hov");
        });
        $('#feed .previewContainer').each(function(){
            if( $('a',this).attr("href") == window.location.href ) {
                $('.previewContainer').removeClass("active");
                $(this).addClass("active");
            }
        });        
    }

    searchField.all();
    
    newsFeedSetup();

    function newsFeedSetup(){
        var orig = $("#filters").height();
        resize(orig);
        function resize(val){
            var height = $(window).height() -1 - val;
            $("#filters").css({ "height":val});
            $("#feed").css({ "height":height});
        }
        $( window ).resize(function() {
            var id = $("input[name=radios]:radio:checked").attr("id");
            if(id == "radio1"){
                resize(orig);
            }else if(id == "radio2"){
                resize($("#tags").outerHeight()+orig);
            }
            else if(id == "radio3"){
                resize($("#search").outerHeight()+orig);
            }
        });

        $("input[name=radios]:radio").change(function () {            
            if($(this).attr("id") == "radio1"){
                $( '#options #search input' ).val("");
                searchField.all();
                searchField.onComplete();
                resize(orig);            
            }
            if($(this).attr("id") == "radio2"){
                $( '#options #search input' ).val("");
                $("#tags .ajaxTagLink").removeClass("active");
                resize($("#tags").outerHeight()+orig);
            }
            if($(this).attr("id") == "radio3"){
                resize($("#search").outerHeight()+orig);
                if($("#search input").val() !== ""){
                    searchField.find($("#search input").val());
                }
            }
            if($(this).attr("id") == "radio4"){
                
            }
        });
    }
}

function navBarSetup(){
    $.fn.pressEnter = function(fn) {  

        return this.each(function() {  
            $(this).bind('enterPress', fn);
            $(this).keyup(function(e){
                if(e.keyCode == 13)
                {
                  $(this).trigger("enterPress");
                }
            });
        });  
    }; 

    function openSearch(){
        if($("#mainNav input").val() !== "" ){
            $("body").addClass("showNewsFeedSearch");
            $("#mainNavCheck").prop('checked', false);
            $('#mainNav input[type="checkbox"]').prop('checked', false);
                
            $('#mainNav input').blur();
            $( '#search input' ).val($("#mainNav input").val());
            $("#filters #radio3").prop('checked',true);
            $( "#filters #radio3" ).change();
        }
    }
    
    function closeSearch(){
        $("body").removeClass("showNewsFeedSearch");
        if($(".post-template").length < 1){
            $("#filters #radio1").prop('checked',true);
            $( "#filters #radio1" ).change();
            $( '#options #search input' ).val("");
        }
        $( '#mainNav input' ).val("");
    }

    $('#mainNav input').pressEnter(function(){
        openSearch();
    });
    $("#mainNav .menu").on( "click", function() {
        closeSearch();
    });

    
    $("#sideBar").mouseleave(function(e) {
        closeSearch();
    });
}

function checkShowNewsFeed(){
    if($(".post-template").length > 0 && $(".page").length < 1 && showNewsFeed === true){
        $("body").addClass("showNewsFeed");
        $("#mainNavCheck").addClass("trigger");
        window.setTimeout(function() {
            $("#mainNavCheck").removeClass("trigger"); 
        },400);
    }else{
        $("body").removeClass("showNewsFeed");                    
    }    
}

/* ============================================================ */





/* ============================================================ */
/* Feature Carousel Section */
/* ============================================================ */

function featureSection(featured,latest, N){
    if($(".home-template").length > 0){
        if(featured===true){
            
            if(latest === true){
                // shows featured items, if no featured items show latest items
                if($("#wookmark .featured").length > 0){
                    featureToSection();
                }else{
                    latestToFeatured();
                }
            }else if(latest === false){
                // show featured items only
                if($("#wookmark .featured").length > 0){
                    featureToSection();
                }
            }
        }else{
            if(latest === true && latestNum > 0){
                // show latest items only
                latestToFeatured();
            }
        }
    }

    function latestToFeatured(){
        $(".featuredContainer").addClass("show");
        $("#wookmark .item:lt("+N+")").appendTo(".featuredContainer .featureContent");
        renderHeroImages();
        var myslider = new featureSlider($(".featuredContainer"));
    }
    function featureToSection(){
        $(".featuredContainer").addClass("show");
        $("#wookmark .featured").appendTo(".featuredContainer .featureContent");
        $("#wookmark .featured").remove();
        renderHeroImages();
        var myslider = new featureSlider($(".featuredContainer"));
    }
    function renderHeroImages(){
        //FEATURE IMAGES    
        if( $(".featuredContainer .subHeroImage img").length >= 1 ){
            $('.featuredContainer .item').each(function() {
                var img = $(".subHeroImage img",this).attr('src');
                $(".subHeroImage",this).css({"background-image": "url('"+ img +"' )"});
            });    
        }        
    }
    
}

var featureInterval;
var featureSlider = function(container){
    window.clearInterval(featureInterval);
    var $container = container;
    var first = 0;
    var last = $container.find("article").last().index() -1;
    var current = first;

    if(first==last){
        $container.find("#controls").remove();
    }else{
        for(var i = 0; i <= last; i++){
            $container.find(".pagination").append('<div class="dot"></div>');
        }
    }
    $container.find(".pagination .dot").eq(first).addClass("dotCurrent");
    $container.find(".featureContent article .imageContain").removeAttr("href");
    $container.find(".featureContent article .postTitle a").removeAttr("href");
    
    var goToSlide = function (slideNum,callBack) {
        current = slideNum;
        $container.find("article").removeClass("current");
        $container.find("article").eq(slideNum).addClass("current");
        $container.find(".pagination .dot").removeClass("dotCurrent");
        $container.find(".pagination .dot").eq(slideNum).addClass("dotCurrent");
        if(callBack !== undefined){
            callBack();
        }
    };
    
    window.setTimeout(function() {goToSlide(first, reRenderHoverControls(last,first+1));},10);
    
    function reRenderHoverControls(prevSlideNum,nextSlideNum){    
        renderHover("left",prevSlideNum);
        renderHover("right",nextSlideNum);
        
        function renderHover(dir,slideNum){
        var $nextPreview = $container.find("article").eq(slideNum);
            $container.find("."+dir+" .hoverPrev .subHeroImage").html($nextPreview.find(".subHeroImage").clone());
            $container.find("."+dir+" .hoverPrev .postTitle").html($nextPreview.find(".postTitle").clone());
            $container.find("."+dir+" .hoverPrev .dateStamp").html($nextPreview.find(".dateStamp").clone());
        }
    }
    

    
    goToSlide.next = function () {
        if(current + 1 > last){
            goToSlide(first, reRenderHoverControls(last,first+1) );
        }else{
            var newCurrent = current+1;
            var nextHover = newCurrent + 1 > last ? first : newCurrent + 1;
            goToSlide(newCurrent, reRenderHoverControls(current,nextHover) );
        }
    };
    goToSlide.prev = function () {
        if(current - 1 < first){
            goToSlide(last, reRenderHoverControls(last-1,first) );
        }else{
            var newCurrent = current-1;
            var prevHover = newCurrent - 1 < first ? last : newCurrent - 1;
            goToSlide(newCurrent, reRenderHoverControls(prevHover,current) );            
        }
    };
    goToSlide.clicked = function (paginationNum) {
        if(paginationNum != first || paginationNum != last){
            goToSlide(paginationNum, reRenderHoverControls(paginationNum-1,paginationNum+1) );
        }
        if(paginationNum == first){
            goToSlide(paginationNum, reRenderHoverControls(last,paginationNum+1) );
        }
        if(paginationNum == last){
            goToSlide(paginationNum, reRenderHoverControls(paginationNum-1, first) );
        }
    };    
    
    $container.find("#controls .right").on( "click", function() {
        goToSlide.next();
    });
    $container.find("#controls .left").on( "click", function() {
        goToSlide.prev();
    });
    $container.find(".pagination .dot").on( "click", function() {
        goToSlide.clicked( $(this).index() );
    });    

    featureInterval = window.setInterval(function(){goToSlide.next();},sliderWait);    
    $container.on( "mouseenter", function() {
        window.clearInterval(featureInterval);
    });
    $container.on( "mouseleave", function() {
        window.clearInterval(featureInterval);
        featureInterval = window.setInterval(function(){goToSlide.next();},sliderWait);
    });    

   
};

/* ============================================================ */





/* ============================================================ */
/* homepage layout and hero image manipulation */
/* ============================================================ */

function homepageLayout(selection){
    
    if($("html").hasClass("mq")){
        select(selection);
    }else{
        select(4);
    }
    wookmarkfunction();
    
    function select(s){
        selection = s;
        if(selection == 1){
            $("#wookmark .item:odd").addClass("verticle");
            wookmarkImages();
        }else if(selection == 2){
            $("#wookmark .item").addClass("verticle");
            wookmarkImages();
        }else if(selection == 3){
            $("#wookmark .item").addClass("verticle");
            wookmarkImages();
        }
    }
    
    function wookmarkImages(){
        $('.wookmarkWrap .verticle').each(function(i) {
            if( $(".subHeroImage img",this).length >= 1 && $(".subHeroImage img",this).attr("src").indexOf("#hero") != -1){
                var img = $(".subHeroImage img",this).attr('src');

                if(wookmarkLayout == 1){
                    $(".subHeroImage",this).css({"background-image": "url('"+ img +"' )"});
                }
                if(wookmarkLayout == 2){

                    if($(".subHeroImage img",this).height() > $(".subHeroImage img",this).width()){
                        $(".subHeroImage",this).css({"background-image": "url('"+ img +"' )"});
                    }else{
                        $(this).removeClass("verticle");
                    }
                }
                if(wookmarkLayout == 3){
                    $(".subHeroImage",this).css({"background-image": "url('"+ img +"' )"});
                }
            }else{
                $(".subHeroImage p",this).remove();
                $(".subHeroImage",this).html("");

                $(this).addClass("noHeroImage");
                $(this).removeClass("verticle");
            }
        });    
    }
}


/* ============================================================ */




/* ============================================================ */
/* POST PAGE layout - renders the heroImage on the top of a page*/
/* ============================================================ */

function renderPageHeroImage(){
    //POST PAGE

    if($("#postMain").length > 0){
        if($(".postContent img").length > 0 ){
            $(".postContent img").each(function(){
            if($(this).attr("src").indexOf("#hero") >= 0){
                $("#postMain .heroImage").addClass("showHeroImage");
                var heroimg = $(this).attr('src');
                $("#postMain .heroImage").css({"background-image":"url('"+ heroimg +"' )"});
                $(this).addClass("hidden");
            }
            if($(this).attr("src").indexOf("#thumb") >= 0){
                $(this).addClass("hidden");
            }                
            });
        }else{
        }
    }
}
/* ============================================================ */




/* ============================================================ */
/* General Layout - scrolling, pagination, wookmark */
/* ============================================================ */

function scrollFadeIn(selector){
    scrolled();
    function scrolled(){
    
        var $w = $(window).height() > 0 ? $(window) : $("#container"); //IE7 FIX
        
        window.setTimeout(function(){
            selector.each(function(i){
                
                if($(this).offset().top  < $w.height() + $w.scrollTop()){
                    $(this).delay(i * 100).fadeTo("normal",1);
                }        
            });
        }, 200);
        
    }
$("#container").on('scroll',scrolled);
}

function wookmarkfunction() {
    // Prepare layout options.
    var options = {
        itemWidth: 350, // Optional min width of a grid item
        autoResize: true, // This will auto-update the layout when the browser window is resized.
        container: $('#wookmark'), // Optional, used for some extra CSS styling
        offset: 20, // Optional, the distance between grid items
        direction:"left",
        align:'left',
        outerOffset: 20, // Optional the distance from grid to parent
        flexibleWidth: '100%' // Optional, the maximum width of a grid item
    };
    var $window = $(window).width() > 0 ? $(window) : $("#container"); //IE7 FIX;
    // Get a reference to your grid items.
    var handler = $('#wookmark .item');
    $window.resize(function() {
        checkSize();
        scrollFadeIn($('#wookmark article'));
    });
    
    handler.wookmark(options);
    checkSize();    
    $('.item').css({"visibility":"visible"});
    
    function checkSize(){
        var windowWidth = $window.width(),
          newOptions = {itemWidth:350,outerOffset: 20};
            
        if(windowWidth <= 490) {
            newOptions.itemWidth = '100%';
            newOptions.outerOffset = 0;
        }else{
            newOptions.itemWidth = 350;
            newOptions.outerOffset = 20;
        }
        handler.wookmark(newOptions);
    }     
}


function paginationSetup(){
    var getCurrentPage = $(".pag").data("page");
    if(getCurrentPage == 1){
        $(".pag .ajaxNewerPosts").remove();
        $('.ajaxNewerPosts, .ajaxOlderPosts').addClass('ajaxPag');
        $('.pag .ajaxOlderPosts').addClass('readMore');
         
        $(".pag .ajaxOlderPosts").html("Load More");
        $(".pag .ajaxOlderPosts").attr("href","#");
    }
    
}


/* ============================================================ */




/* ============================================================ */
/* gmaps Loading  */
/* ============================================================ */

function gmaps(){
    if($("#gmaps").length > 0){
        var $gmaps = $("#gmaps");
        var lat = $gmaps.data("lat");
        var lng = $gmaps.data("lng");
        var center = new google.maps.LatLng(lat,lng);
        var map;
        var mapProp;
        var id = "gmaps";

        mapProp = {
            center:center,
            zoom:15,
            mapTypeId:google.maps.MapTypeId.ROADMAP
        };
        map=new google.maps.Map(document.getElementById(id),mapProp);
        
        var marker=new google.maps.Marker({
          position:center
          });
        marker.setMap(map);    
    }
}
/* ============================================================ */





/* ============================================================ */
/* Browser Support checks - if no support then run support      */
/* ============================================================ */

function supportCheck(){
    
    if(Modernizr.mq('only all')) {
        jQuery('html').addClass('mq');
    }else{
        jQuery('html').addClass('no-mq');
    }
    
    $("body").append('<div id="supportCheck"><input type="radio" name="test" checked="checked" id="id-test" /></div>');

    if($('#supportCheck input').css('width') !== "200px"){
        $("html").addClass("no-checked");
    }

    if($('#supportCheck input').css('width') >= "200px"){
        $("html").addClass("checked");
    }

}

function RUNlegacySupport(){
    try{
        legacySupportIE();
        $(".ie7icoMoon").remove();
        icomoon();
    }catch(err){} //RUNS legacySupportIE.js stuff if avalible
}

/* ============================================================ */



/* ============================================================ */
/* Ajax Loading  */
/* ============================================================ */
var loading = false;
function ajaxPageData(){
    
    var History = window.History;

    var $MainContainer = $('#mainContainer');
    
    
    var currentPage = $(".pag").data("page");
    var totalPages = $(".pag").data("totalPages");

    // Check if history is enabled for the browser
    if ( ! History.enabled) {
        return false;
    }

    History.Adapter.bind(window, 'statechange', function() {
        var State = History.getState();

        // Get the requested url and replace the current content with the loaded content
        $.get(State.url, function(result) {
            var $html = $(result);

            var $newContent = $('#mainContainer', $html).contents();
            var $Container = $('#container');

            $Container.fadeOut( 500, function() {
                $('body').attr('class', $('#container', $html).attr('class') );
                $('#container').scrollTop(0);
                $MainContainer.html("");

                $MainContainer.show().html($newContent);
                $(".postContent").fitVids();// Re run fitvid.js
                if($(".home-template").length > 0){featureSection(feature,latest,latestNum);}
                $newContent.css({ opacity: 0 });
                currentPage = 1;
                totalPages = $(".pag").data("totalPages");
                checkShowNewsFeed();
                gmaps();
                window.setTimeout(function() {
                    if($(".home-template, .tag-template").length > 0){
                        try{
                        $('#wookmark').imagesLoaded(function(){
                            homepageLayout(wookmarkLayout);
                        });
                        }catch(err){homepageLayout(wookmarkLayout);}
                        paginationSetup();
                        $('#wookmark article').css({ opacity: 0 });
                        scrollFadeIn($('#wookmark article'));
                    }else{
                        renderPageHeroImage();
                    }
                    $('#container').css({ opacity: 0 }).fadeTo(500,1);
                    RUNlegacySupport();
                    $('#feed .previewContainer').each(function(i){
                        if( $('a',this).attr("href") == window.location.href ) {
                            $('.previewContainer').removeClass("active");
                            $(this).addClass("active");
                        }
                    });
                    NProgress.done();
                    loading = false;

                }, 560);  
            });
        })
        .done(function() {

        })
        .fail(function() {
            window.alert( "Error. Page may not exist or you have lost internet connection." );
            NProgress.done();
            loading = false;
        });
    });

    $('body').on('click', '.ajaxLink, .pagination a, #mainNav .ajaxTagLink, .ajaxPag, .tags a', function(e) {
        e.preventDefault();
        if (loading === false) {
            var currentState = History.getState();
            var url = $(this).attr('href');
            var title = $(this).attr('title') || null;

            // If the requested url is not the current states url push the new state and make the ajax call.
            var ajaxPagCheck = false;
            if($(this).attr('class') !== undefined){
                ajaxPagCheck = $(this).attr('class').indexOf("ajaxPag");
            }else{
                ajaxPagCheck = -1;
            }

            if (url.replace(/^.*\/\/[^\/]+/, '') !== currentState.url.replace(/^.*\/\/[^\/]+/, '') && ajaxPagCheck == -1) {
                loading = true;
                NProgress.start();
                History.pushState({}, title, url);

            }else if(ajaxPagCheck != -1){
                NProgress.start();
                ajaxPagination();
            }else {
                // reload loading animation
                $('#container').animate({'scrollTop': 0});
                NProgress.start();
                NProgress.done();
            }
        }
    });

    function ajaxPagination(){
        if(currentPage < totalPages){
            currentPage = currentPage + 1;
            $(".curPage").html(currentPage);
            if(currentPage >= totalPages){
                $(".pag .ajaxOlderPosts").fadeOut();
            }
            $.get('page/'+ currentPage, function(result) {
                var $html = $(result);

                var $newContent = $('#wookmark', $html).contents();
                $($newContent).addClass("page"+currentPage+"");            
                $("#wookmark").append($newContent);
                $('#wookmark .page'+currentPage+'').css({ opacity: 0 });
                scrollFadeIn($('#wookmark .page'+currentPage+''));
                
                try{
                $('#wookmark').imagesLoaded(function(){
                    homepageLayout(wookmarkLayout);
                    moveToNewContent();
                });
                }catch(err){
                    homepageLayout(wookmarkLayout);
                    moveToNewContent();
                }
                RUNlegacySupport();
                
                $($newContent).css({ opacity: 0 });
                
                function moveToNewContent(){
                    window.setTimeout(function() {
                        var firstAjaxedPost = $(".page"+currentPage+"").offset().top + $("#container").scrollTop();
                        $('#container').stop().animate({scrollTop: firstAjaxedPost -20}, 800);
                    }, 400);
                }
                })
            .done(function() {
                NProgress.done();
                loading = false;
                hljs.initHighlighting.called = false;
				hljs.initHighlighting(); 
            })
            .fail(function() {
                window.alert( "Error. Check your internet connection." );
                NProgress.done();
                loading = false;
            });
        }
    }
}

/* ============================================================ */
