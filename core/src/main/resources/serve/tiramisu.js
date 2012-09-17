$(function(){

    function loadPage(href){
        var result = $.ajax( href,{
            dataType: "html",
            headers: {
                "X-Tiramisu-Template": "false"
            }
        } )
            .done(function(data) {
                $("t\\:content",data).each(function(i,val){
                    var name = $(val).attr('name');
                    var here = $("t\\:content[name='"+name+"']");
                    $(here).html($(val).html());
                })
            })
            .fail(function() { alert("error"); })
    }

    $(document).on("click",".tiramisu-ajax-link",function(){
        var href = $(this).attr("href");
        loadPage(href);
        var state={
            url: href
        }
        history.pushState(state, "", state.url);
        return false;
    });
});
