$ ->

    window.wsUri = 'ws://localhost:8080/status'

    updateSpace = (data) ->
        for row, y in data
            for col, x in row.split(' ')
                #console.log 'x' + x + 'y' + y + ' = ' + col
                cell = $('#x' + x + 'y' + y)
                if cell
                    if col is 'O'
                        cell.addClass('alive')
                    else
                        cell.removeClass('alive')
        return

    websocket = null

    window.wsInit = ->
        websocket = new WebSocket(wsUri)
        websocket.onopen = (evt) ->
            console.log evt
            return
        websocket.onclose = (evt) ->
            console.log evt
            return
        websocket.onmessage = (evt) ->
            #console.log evt.data
            data = evt.data.split('\n')
            switch data[0]
                when 'UPDATE' then updateSpace(data.slice(1))
                else console.log data[0]
            return

    $('#stop').on 'click', (evt) ->
        elt = $(evt.target)
        if elt.val() is "pause"
            elt.val("resume")
        else
            elt.val("pause")
        websocket.send 'STOP'
        return

    changeCellState = (cell) ->
#        $('#space #' + cell).toggleClass('alive')
        websocket.send "TOUCH\n" + cell.match(/x(\d*)y(\d*)/).slice(1).join('\n')

    initSpace = ->
        parent = $('#space')
        width = parseInt($('#selectWidth').val())
        height = parseInt($('#selectHeight').val())
        parent.empty()
        console.log 'aa'
        for y in [0...height]
            parent.append('<div class="row">')
            for x in [0...width]
                parent.append('<div class="cell" id="x' + x + 'y' + y + '"> </div>')
            parent.append('</div>')
        $('#space .cell').on 'click', (e) ->
            changeCellState e.target.id
        websocket.send 'START\n' + width + '\n' + height
        return

    $('#start').on 'click', initSpace

    window.wsInit()

    return