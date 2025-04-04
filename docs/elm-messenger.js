
function startmessenger(appname) {
    const pathname = document.location.pathname + "info";
    var app = Elm.Main.init({
        node: document.getElementById(appname),
        flags: {
            timeStamp: Date.now(),
            info: localStorage.getItem(pathname) ? localStorage.getItem(pathname) : ""
        }
    });
    if (app.ports.sendInfo) {
        app.ports.sendInfo.subscribe(function (m) {
            localStorage.setItem(pathname, m);
        });
    }
    if (app.ports.alert) {
        app.ports.alert.subscribe(function (m) {
            alert(m);
        });
    }
    if (app.ports.prompt) {
        app.ports.prompt.subscribe(function (m) {
            let res = prompt(m.title);
            if (res) {
                app.ports.promptReceiver.send({
                    name: m.name,
                    result: res
                });
            }
        });
    }

    if (app.ports.setView) {
        app.ports.setView.subscribe(function (v) {
            ElmREGL.setView(v);
        });
    }
    if (app.ports.execREGLCmd) {
        app.ports.execREGLCmd.subscribe(function (v) {
            ElmREGL.execCmd(v);
        });
    }
    const canvas = document.getElementById('elm-regl-canvas');
    ElmREGL.init(canvas, app, []);

    // Disable F1-F4 keys
    window.addEventListener("keydown", (e) => {
        const { key, keyCode, metaKey, shiftKey, altKey, ctrlKey } = e;
        if (keyCode >= 112 && keyCode <= 115) {
            e.preventDefault();
        }
    });
    startAudio(app);
}
