function fixup_bettergrammar() {
    let delete_mes = document.querySelectorAll(".delete-me");
    delete_mes.forEach(function(delete_me){
        let tab_frame = delete_me.parentNode;
        while (delete_me.childNodes.length > 0){
            tab_frame.prepend(delete_me.lastChild);
        }
        delete_me.remove;
        return;
    });
    return;
}
