$ ->

    window.startGameUri ?= 'ws://localhost:8080/connect?user=zulu'
    window.joinGameUri ?= 'ws://localhost:8080/connect?user=zulu&game='

    window.listGamesUri ?= 'ws://localhost:8080/list'

    class GameOfLife

        constructor: (url) ->
            @offset = [ 0, 0 ]
            @width = 100
            @height = 50
            @living = []
            @cellSize = [ 10, 10 ]
            # init canvas
            @canvas = $('<canvas></canvas>')
            @canvas.attr('id', 'gameSpace')
            $('div#gameSpace').append(@canvas)
            @canvas.attr 'width', @width * @cellSize[0]
            @canvas.attr 'height', @height * @cellSize[1]
            @canvas.translateCanvas
                translateX: @width / 2 * @cellSize[0]
                translateY: @height / 2 * @cellSize[1]

            @lastMouseEvent = null

            @websocket = new WebSocket(url)
            self = @
            @websocket.onopen = (evt) -> self.wsOpen(evt)
            @websocket.onclose = (evt) -> self.wsClose(evt)
            @websocket.onmessage = (evt) -> self.wsMessage(evt)


            $('#pause').on 'click', -> self.pause()
            $('#stop').on 'click', -> self.stop()

            @canvas.on 'mousedown', (evt) -> self.canvasMouseDown(evt)
            @canvas.on 'mouseup', (evt) -> self.canvasMouseUp(evt)
            @canvas.on 'mousemove', (evt) -> self.canvasMouseMove(evt)

            @updateCanvas()

            return

        destroy: ->
            console.log 'destroying...'
            $('#pause').off()
            $('#stop').off()
            @websocket.onclose = (evt) -> return
            @websocket.close()
            delete @websocket
            @canvas?.remove()
            delete @canvas
            delete @offset
            delete @width
            delete @height
            delete @living
            delete @cellSize
            delete @lastMouseEvent
            console.log 'destroyed!'

        stop: ->
            @websocket.send 'STOP'
            @destroy()
            return

        pause: ->
            @websocket.send 'PAUSE'
            return

        wsOpen: (evt) ->
            return

        wsClose: (evt) ->
            console.log evt
            @destroy()
            return

        wsMessage: (evt) ->
            #console.log evt
            data = evt.data.split('\n')
            console.log evt
            command = data[0].split(' ')
            switch command[0]
                when 'UPDATE'
                    if @gameName isnt command[1]
                        @gameName = command[1]
                        $('#list').val(@gameName)
                    cells = data.slice(1).map (c) -> c.split(':')
                    @updateSpace(cells)
                else console.log "Unknown command: #{data[0]}"
            return

        updateCanvas: ->
            self = @
            @canvas.clearCanvas()

            # draw grid
            @canvas.draw
              fn: (ctx) ->
                imgData = self.grid
                w = self.width + 2
                h = self.height + 2
                if not imgData
                    imgData = ctx.createImageData(w * self.cellSize[0], h * self.cellSize[1])
                    for y in [0..h]
                        for x in [0..w]
                            pos = (y * w * self.cellSize[0] * self.cellSize[1] + x * self.cellSize[0]) * 4
                            imgData.data[pos+0] = 0
                            imgData.data[pos+1] = 0
                            imgData.data[pos+2] = 0
                            imgData.data[pos+3] = 255
                    self.grid = imgData
                ctx.putImageData(imgData, (-self.offset[0] % 1 - 0.5) * self.cellSize[0], (-self.offset[1] % 1 - 0.5) * self.cellSize[1])
                return

            # draw alive cells
            for cell in @living
                @canvas.drawRect
                    fillStyle: "green"
                    x: cell[0] * @cellSize[0] - @cellSize[0] / 2
                    y: cell[1] * @cellSize[1] - @cellSize[1] / 2
                    width: @cellSize[0]
                    height: @cellSize[1]

            return

        updateSpace: (data) ->
#            console.log "space updated"
#            console.log data
            @living = data
            @updateCanvas()
            return

        canvasMouseDown: (evt) ->
            @lastMouseEvent = {
                x: evt.offsetX
                y: evt.offsetY
                sh: evt.shiftKey
                type: 'down'
            }
            return

        canvasMouseUp: (evt) ->

            dx = evt.offsetX - @lastMouseEvent.x
            dy = evt.offsetY - @lastMouseEvent.y

            if @lastMouseEvent.type is 'down' and dx < @cellSize[0] and dy < @cellSize[1]
                cellX = Math.round (evt.offsetX - @width * @cellSize[0] / 2) / @cellSize[0] + @offset[0]
                cellY = Math.round (evt.offsetY - @height * @cellSize[1] / 2) / @cellSize[1] + @offset[1]
                @websocket.send "TOUCH\n#{cellX}\n#{cellY}"
                console.log "TOUCH\n#{cellX}\n#{cellY}"

            @canvas.translateCanvas
                translateX: dx
                translateY: dy

            @offset = [
                @offset[0] - dx / @cellSize[0]
                @offset[1] - dy / @cellSize[1]
            ]
            @updateCanvas()
            @lastMouseEvent = null
            return

        canvasMouseMove: (evt) ->
            if @lastMouseEvent
                dx = evt.offsetX - @lastMouseEvent.x
                dy = evt.offsetY - @lastMouseEvent.y

                @canvas.translateCanvas
                    translateX: dx
                    translateY: dy

                @offset = [
                    @offset[0] - dx / @cellSize[0]
                    @offset[1] - dy / @cellSize[1]
                ]
                @updateCanvas()
                @lastMouseEvent = {
                    x: @lastMouseEvent.x + dx
                    y: @lastMouseEvent.y + dy
                    sh: evt.shiftKey
                    type: 'move'
                }
            return


    game = null

    initList = ->
        websocket = new WebSocket(window.listGamesUri)
        websocket.onopen = (evt) -> console.log(evt)
        websocket.onclose = (evt) -> console.log(evt)
        websocket.onmessage = (evt) ->
            $('#list option.game').remove()
            evt.data.split('\n').forEach (game)->
                $('#list').append("<option class=\"game\" value=\"#{game}\">#{game}</option>") if game isnt ''
        $('#choose').on 'click', ->
            choise = $('#list').val()
            game.destroy() if game
            switch choise
                when 'create'
                    console.log 'create a new game...'
                    game = new GameOfLife(startGameUri)
                else
                    console.log "your choise is #{choise}"
                    game = new GameOfLife(joinGameUri + choise)

    initList()

    return