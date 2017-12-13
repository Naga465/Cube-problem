"use strict";

exports.state = function (ctx) {
    return function (callback) {
        function loop(time) {
            callback(context)(time)();
            window.requestAnimationFrame(loop);
        }

        window.requestAnimationFrame(loop);
        return function () {}
    }
}
exports.addEventListener = function (ctx){
    return function (eventType) {
        return function(callback) {
            function eventHandler(e) {
                callback(e)();
            
            }
            thecanvas.addEventListener(eventType, eventHandler);
            return function () {}
        }
    }
}