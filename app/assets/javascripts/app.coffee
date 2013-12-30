$ ->

    window.startGameUri ?= 'ws://localhost:8080/connect'
    window.listGamesUri ?= 'ws://localhost:8080/list'

    class GameOfLife

        living:         []              # set of alive cells
        cellSize:       [ 10, 10 ]      # cell size on screen
        display:        null            # display cache
        cursor:         [0, 0]          # last cursor position in pixels
        offset:         [0.0, 0.0]      # space offset in cells

        showMinimap:    true            # display map?
        minimap:                       # position/size of minimap
            x: 20
            y: 20
            w: 200
            h: 100

        # translate game coordinates to screen
        translateToCanvas: (position) ->
            x = Math.round((position[0] - @offset[0]) * @cellSize[0] - @cellSize[0] / 2)
            y = Math.round((position[1] - @offset[1]) * @cellSize[1] - @cellSize[1] / 2)
            return [x, y]
        # translate screen coordinates to game
        translateFromCanvas: (position) ->
            x = Math.floor(position[0] / @cellSize[0] + @offset[0])
            y = Math.floor(position[1] / @cellSize[1] + @offset[1])
            return [x, y]

        constructor: (params) ->
            @params = params

            # prepare url
            url = window.startGameUri
            url += "?player=#{@player}"
            url += "&game=#{params.game}"                   if params.game?
            url += "&gameStrategy=#{params.gameStrategy}"   if params.gameStrategy?
            url += "&spaceType=#{params.spaceType}"         if params.spaceType?
            url += "&spaceWidth=#{params.spaceWidth}"       if params.spaceWidth?
            url += "&spaceHeight=#{params.spaceHeight}"     if params.spaceHeight?
            console.log url

            # display elements
            $('#createGame').hide()
            $('#game').show()
            $('#gameControls').children().show()

            # init canvas
            @width = Math.floor($('div#gameSpace').parent().innerWidth() / @cellSize[0])
            @height = 50
            @canvas = $("<canvas id=\"gameSpace\" width=\"#{@width * @cellSize[0]}\" height=\"#{@height * @cellSize[1]}\"></canvas>")[0]
            $('div#gameSpace').append($(@canvas))

            console.log @offset
            @offset = [-@width / 2, -@height / 2]

            # server
            @websocket = new WebSocket(url)
            self = @
            @websocket.onopen = (evt) -> self.wsOpen(evt)
            @websocket.onclose = (evt) -> self.wsClose(evt)
            @websocket.onmessage = (evt) -> self.wsMessage(evt)

            # controls
            $('#map').on 'click', -> self.toggleMap()
            $('#pause').on 'click', -> self.pause()
            $('#stop').on 'click', -> self.stop()

            # mouse
            @lastMouseEvent = null
            $(@canvas).on 'mousedown', (evt) -> self.canvasMouseDown(evt)
            $(@canvas).on 'mouseup', (evt) -> self.canvasMouseUp(evt)
            $(@canvas).on 'mousemove', (evt) -> self.canvasMouseMove(evt)

#            @interval = setInterval( ->
#                self.updateCanvas()
#            , 20
#            )
            @setupLoop()
            @needsUpdate = true
            return

        destroy: ->
            console.log 'destroying...'
            clearInterval @interval
            $('#pause').off()
            $('#stop').off()
            @websocket?.onclose = (evt) -> return
            @websocket?.close()
            delete @websocket
            @canvas?.remove()
            delete @canvas
            $('#gameControls').children().hide()
            $("#game").hide()
            $("#createGame").show()
            console.log 'destroyed!'
            return


        setupLoop: ->

            self = @

            mainloop = -> self.updateCanvas()

            animFrame = (window.requestAnimationFrame ||
                    window.webkitRequestAnimationFrame ||
                    window.mozRequestAnimationFrame    ||
                    window.oRequestAnimationFrame      ||
                    window.msRequestAnimationFrame     ||
                    null)

            if animFrame?
                canvas = @canvas

                recursiveAnim = ->
                    mainloop()
                    animFrame( recursiveAnim, canvas ) if canvas?

                animFrame( recursiveAnim, canvas )
            else
                ONE_FRAME_TIME = 1000.0 / 60.0
                @interval = setInterval( mainloop, ONE_FRAME_TIME )
            return

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
            @destroy()
            return

        wsMessage: (evt) ->
            #console.log evt
            data = evt.data.split('\n')
            #console.log evt
            command = data[0].split(' ')
            switch command[0]
                when 'UPDATE'
                    if @gameName isnt command[1]
                        @gameName = command[1]
                        @params.spaceWidth = command[2]
                        @params.spaceHeight = command[3]
                        $('#list').val(@gameName)
                    cells = data.slice(1).map (c) -> c.split(':')
                    @updateSpace(cells)
                else console.log "Unknown command: #{data[0]}"
            return

        toggleMap: ->
            @showMinimap = not @showMinimap
            @updateCanvas()
            return

        displayMinimap: ->
            # calculate space dimensions
            dims = null
            if @living.length > 0
                dims = @living.reduce( (a, b) ->
                    [
                        Math.min(b[0], if a[0] == undefined then b[0] else a[0])
                        Math.min(b[1], if a[1] == undefined then b[1] else a[1])
                        Math.max(b[0], if a[2] == undefined then b[0] else a[2])
                        Math.max(b[1], if a[3] == undefined then b[1] else a[3])
                    ]
                , [])
            else
                dims = [0,0,0,0]
            size = [
                Math.min(@minimap.w, dims[2] - dims[0] + 1)
                Math.min(@minimap.h, dims[3] - dims[1] + 1)
            ]
            # correct aspect ratio
            ratio = 1
            ratio = size[0] / size[1] if size[1] isnt 0
            size = [
                Math.min(size[0], Math.floor(@minimap.w * ratio))
                Math.min(size[1], Math.floor(@minimap.h / ratio))
            ]
            # init minimap with zeros
            minimap = (((0) for [0...size[0]]) for [0...size[1]])
            # transpose coordinates and calculate weight
            maxWeight = 0
            for cell in @living
                x = Math.floor((cell[0] - dims[0]) / Math.abs(dims[2] - dims[0] + 1) * size[0])
                y = Math.floor((cell[1] - dims[1]) / Math.abs(dims[3] - dims[1] + 1) * size[1])
                if minimap[y]? && minimap[y][x]?
                    minimap[y][x]++
                    maxWeight = Math.max(minimap[y][x], maxWeight)

            w = @minimap.w
            h = @minimap.h

            ctx = @canvas.getContext('2d')
            zoomRatio = Math.floor(Math.min(w / size[0], h / size[1]))
            posX = Math.floor((w - size[0] * zoomRatio) / 2)
            posY = Math.floor((h - size[1] * zoomRatio) / 2)
            imgData = ctx.createImageData(w, h)
            for y in [0...h]
                for x in [0...w]
                    pos = (y * w + x) * 4
                    xx = Math.floor((x - posX) / zoomRatio)
                    yy = Math.floor((y - posY) / zoomRatio)
                    if minimap[yy] && minimap[yy][xx]
                        c = 255 - Math.floor((minimap[yy][xx] / maxWeight) * 255)
                    else
                        c = 240
                    imgData.data[pos+0] = c
                    imgData.data[pos+1] = c
                    imgData.data[pos+2] = c
                    imgData.data[pos+3] = 100
            #ctx.globalAlpha = 0.5
            ctx.putImageData(imgData, @minimap.x, @minimap.y)
            return

        updateCanvas: ->

            return unless @canvas?

            if @needsUpdate
                ctx = @canvas.getContext('2d')
                w = @canvas.width
                h = @canvas.height
                cellW = @cellSize[0]
                cellH = @cellSize[1]

                ctx.clearRect(0, 0, w, h)

                # draw grid
                imgData = @grid
                gridW = w + 2 # 1 cell in each side
                gridH = h + 2
                if not imgData
                    imgData = ctx.createImageData(gridW * cellW, gridH * cellH)
                    for y in [0..gridH]
                        for x in [0..gridW]
                            pos = (y * gridW * cellW * cellH + x * cellW) * 4
                            imgData.data[pos+0] = 0
                            imgData.data[pos+1] = 0
                            imgData.data[pos+2] = 0
                            imgData.data[pos+3] = 255
                    @grid = imgData
                ctx.putImageData(imgData, (-@offset[0] % 1 - 0.5) * cellW, (-@offset[1] % 1 - 0.5) * cellH)

                # draw alive cells
                ctx.fillStyle = 'green'
                for cell in @living
                    pos = @translateToCanvas(cell)
                    ctx.fillRect(pos[0], pos[1], cellW, cellH) if pos[0] > -cellW and pos[0] < w + cellW and pos[1] > -cellH and pos[1] < h + cellH

                @drawSpace()

                @displayMinimap() if @showMinimap

                @display = ctx.getImageData(0, 0, w, h)
                @needsUpdate = false


            @drawPointer()
            return

        drawSpace: ->
            w = parseInt(@params.spaceWidth)
            h = parseInt(@params.spaceHeight)
            if w? && h?
                ctx = @canvas.getContext('2d')
                ctx.strokeStyle = "blue"
                pos = @translateToCanvas([-w/2,-h/2])
#                console.log w + ":" + h
                ctx.strokeRect(pos[0], pos[1], (w+1) * @cellSize[0], (h+1) * @cellSize[1])
            return

        drawPointer: ->
            # if there is no cache
            @updateCanvas() unless @display
            ctx = @canvas.getContext('2d')
            # restore screen
            ctx.putImageData(@display, 0, 0)
            # draw cursor
            pos = @translateToCanvas(@translateFromCanvas(@cursor))
            ctx.strokeStyle = 'red'
            ctx.strokeRect(pos[0], pos[1], @cellSize[0], @cellSize[1])
            return

        updateSpace: (data) ->
            @living = data
            @needsUpdate = true
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
                cell = @translateFromCanvas([evt.offsetX, evt.offsetY])
                @websocket.send "TOUCH\n#{cell[0]}\n#{cell[1]}"

            @offset[0] -= dx / @cellSize[0]
            @offset[1] -= dy / @cellSize[1]
            @needsUpdate = true
            @lastMouseEvent = null
            return

        canvasMouseMove: (evt) ->
            if @lastMouseEvent
                @offset[0] -= (evt.offsetX - @lastMouseEvent.x) / @cellSize[0]
                @offset[1] -= (evt.offsetY - @lastMouseEvent.y) / @cellSize[1]
                @needsUpdate = true
                @lastMouseEvent = {
                    x: evt.offsetX
                    y: evt.offsetY
                    sh: evt.shiftKey
                    type: 'move'
                }
            @cursor = [evt.offsetX, evt.offsetY]
            #@drawPointer()
            return

        updateWidth: ->
            @width = Math.floor(($('div#gameSpace').parent().innerWidth() - 40) / @cellSize[0])
            @grid = null
            $(@canvas).attr 'width', @width * @cellSize[0]
            @needsUpdate = true
#            @canvas.translateCanvas
#                translateX: (@width / 2 + @offset[0]) * @cellSize[0]
#                translateY: (@height / 2 + @offset[1]) * @cellSize[1]
            #@updateCanvas()


    game = null

    initList = ->
        websocket = new WebSocket(window.listGamesUri)
        #websocket.onopen = (evt) -> console.log(evt)
        #websocket.onclose = (evt) -> console.log(evt)
        websocket.onmessage = (evt) ->
            $('#list option.game').remove()
            evt.data.split('\n').forEach (game)->
                $('#list').append("<option class=\"game\" value=\"#{game}\">#{game}</option>") if game isnt ''
        $('#choose').on 'click', ->
            choise = $('#list').val()
            game.destroy() if game
            console.log "your choise is #{choise}"
            game = new GameOfLife
                game:       choise
                player:     "zulu"

    window.onresize = (evt) -> if game? then game.updateWidth()

    $("#create").on 'click', ->
        console.log 'create a new game...'
        game.destroy() if game
#        gameStrategy = $('#gameStrategy').val()
#        spaceType = $('#spaceType').val()
#        spaceWidth = $('#spaceWidth').val()
#        spaceHeight = $('#spaceHeight').val()
#        console.log "#{gameStrategy} #{spaceType}[#{spaceWidth}x#{spaceHeight}]"
        #game = new GameOfLife("#{startGameUri}?user=zulu&gameStrategy=#{gameStrategy}&spaceType=#{spaceType}&spaceWidth=#{spaceWidth}&spaceHeight=#{spaceHeight}")
        game = new GameOfLife
            player:         "zulu"
            gameStrategy:   $('#gameStrategy').val()
            spaceType:      $('#spaceType').val()
            spaceWidth:     $('#spaceWidth').val()
            spaceHeight:    $('#spaceHeight').val()
        return

    initList()

    return