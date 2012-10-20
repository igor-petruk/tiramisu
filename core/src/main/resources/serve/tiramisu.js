function loadPage(href){
    var result = $.ajax( href+"?tiramisuajax=1",{
        dataType: "html"
    } )
        .done(function(data) {
            $("t\\:data",data).each(function(i,val){
                var name = $(val).attr('name');
                var here = $("t\\:content[name='"+name+"']");
                $(here).html($(val).html());
            })
        })
        .fail(function() { alert("error"); })
}

$(function(){
    $(window).on('popstate', function(event){
//        console.log(event.originalEvent.state);
        if (event.originalEvent.state!=null){
//            console.log("!"+event.originalEvent.state.url);
            loadPage(event.originalEvent.state.url);
        }
    });


    $(document).on("click",".tiramisu-ajax-link",function(){
        var href = $(this).attr("href");
        loadPage(href);
        var state={
            url: href
        }
        history.pushState(state, "", state.url);
        return false;
    });

    var state={
        url: window.location.pathname
    }
        console.log("state "+state);
    history.replaceState(state, "", state.url);
});
