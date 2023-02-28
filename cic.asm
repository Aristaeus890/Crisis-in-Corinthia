; This is a template for a basic NES rom
; it uses ines 1.0, not 2.0


; INes 1.0
.segment "HEADER" 
.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $02 ; 2 * 8KB CHR ROMF
.byte %00010011 ; mapper and mirroring
.byte $00 ; unused extension
.byte $00 ; unused extension
.byte $00 ; unused extension
.byte "<"
.byte "3Matt" ; filler bytes to fill out the end of the header

.scope EntityType ; NB this should match the order in the process entity list in order for the jump table to hit the right address
    NoEntity = 0
    Cursor = 1
    CursorLarge = 2
    Mothership = 3
.endscope

.scope Banktype
    GameBank = 0
    TitleBank = 1
.endscope

.scope CursorState 
    init = 0
    free = 1
    grid = 2
    inactive = 3
.endscope

.scope ButtonReturn
    NoPress = 0
    Press = 1
    Release = 2
.endscope

.struct Entity ;  base entity structure
    type .byte ; each entity has its own type assigned, if you somehow make more than 255 entities than idk what to do
    xpos .byte ; x position
    ypos .byte ; y position
    xpossub .byte
    ypossub .byte
    attributes .byte ; pallette, flip, etc
    generalpurpose .byte ; this has no specific use, it can be defined on a per entity basis, typically for a behaviour state machine
    animationframe .byte
    ; animationtimer .byte
    flags .byte
    health .byte
.endstruct

.scope flagtype
    NoDraw = 0
    NoProcess = 1
    FlipH = 2
    FlipV = 4
    Selectable = 8
.endscope

.scope tiletype
    Empty = 0 
    Nebula = 1
    Planet = 2
    Asteroid = 3  
.endscope

.segment "ZEROPAGE" ; 0-FF. One page of ram that is faster access than rest of the ram. Use for values most frequently used
    MAXENTITIES =15; max allowed number of entities. Each entity takes a number of bytes in the zero page equal to the entity struct
    ; this CANNOT run over the end of the zero page or it will not work. If you want more entities, you will need to dedicate a non zero
    ; page ram segment to it
    entities: .res .sizeof(Entity) * MAXENTITIES ; 6 * 30 = 180/256 bytes
    entity_mem = .sizeof(Entity) * MAXENTITIES ; mem used

    world: .res 2 ; this is a pointer to the address of the current screen we want to fetch tiles on
    seed: .res 2 ; the seed for the pseudo rng. This will be inited to anything by 0 later
    jumppointer: .res 2 ; used to jump to specic places in memory with jump tables
    jumppointer2: .res 2
    nmidone: .res 1 ; value to check to see if the nmi is done
    scrollx: .res 1 ; how far the screen is scrolled in x dir
    scrolly: .res 1 ; how far the screen is scrolled in y dir
    var_mem: .res 4 ; sometimes we need to jump to a subroutine and do something with more data than can be juggled with x/y/a
    buttons: .res 1 ; this holds the state of player input for controller 1
    buttonsp2: .res 1 ; this holds the state of player input for controller 1
    currenttable: .res 1
    currentrowodd: .res 1 ; holds the currrent row when drawing tiles
    currentroweven: .res 1
    currentcollisionaddress: .res 2
    playerstate: .res 1
    buttonflag: .res 1
    buttonflagp2: .res 1
    framecount: .res 1
    currentbank: .res 1
    oambufferoffset: .res 1
    animationtrack: .res 1
    animationtracktimer: .res 1
    spritebufferposition: .res 1
    temp: .res 1
    temp2: .res 1
    temp3: .res 1
    rng: .res 1 ; rng is stored here once a frame
    rectangle1: .res 4 
    rectangle2: .res 4
    drawflags: .res 1 ; 
    cursorstate: .res 1
    cursorindex: .res 1
    selectedobject: .res 1

    PPUControl = $2000 
    PPUMask= $2001 
    PPUStatus = $2002
    OAMAddress =$2003
    OAMData = $2004
    PPUScroll = $2005
    PPUAddress = $2006 
    PPUData = $2007 
    OAMDMA = $4014

    ; CURSORSPEED = #200

.segment "OAM"
SpriteBuffer: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "RAM"
    TileBufferH: .res 16
    TilebufferV: .res 15
    CollisionMap: .res 240
    CurrentBackgroundPalette: .res 16
    GameState: .res 1
    Vector: .res 2
    Vector2: .res 2

.segment "STARTUP" ; this is called when the console starts. Init a few things here, otherwise little here
    Reset:
        SEI ; Disables all interrupts
        CLD ; disable decimal mode (nes 6502 does not support decimals)

    

        ; Disable sound IRQ
        LDA #$40
        STA $4017

        ; Initialize the stack register
        LDX #$FF
        TXS

        INX ; #$FF + 1 => #$00

        ; Zero out the PPU registers
        STX PPUControl
        STX PPUMask

        STX $4010

    :
        BIT PPUStatus ; this waits for a vblank
        BPL :-

        TXA

;; This clears out the memory when we start up
CLEARMEM:
    STA $0000, X ; Zero page memory
    STA $0100, X 
    STA $0300, X
    STA $0400, X
    STA $0500, X
    STA $0600, X
    STA $0700, X
    LDA #$FF
    STA $0200, X ; this is a buffer Sprite/entity data, which can can be sent to the ppu later during nmi
    LDA #$00
    INX
    BNE CLEARMEM    ; Keep looping until all the memory is cleared

; wait for vblank. We want to wait for the system to do one scan of the screen before we do anthing else
:
    BIT PPUStatus
    BPL :-

    LDA #$02
    STA OAMDMA
    NOP

    ; $3F00 == palette address
    LDA #$3F
    STA PPUAddress
    LDA #$00
    STA PPUAddress

    LDX #$00

.segment "CODE"

LoadPalettes:
    FillPaletteRam:
        LDA PaletteData, X
        STA CurrentBackgroundPalette, X 
        INX 
        CPX #$20 
        BNE FillPaletteRam
        LDX #$00 
    LoadtoPPU:
        LDA CurrentBackgroundPalette, X 
        STA $2007 ; $3F00, $3F01, $3F02 => $3F1F
        INX
        CPX #$20
        BNE LoadtoPPU 

SetMirroring: ; TODO make sure this actually works? mmc1 is weird
    LDA #%10000000
    STA $8000

    ; MMC1 is nearly unique in that to configure it you have to do serial writes to the same port

    LDA #%00000010  
    STA $8000
    LSR
    STA $8000
    LSR 
    STA $8000
    LSR 
    STA $8000
    LSR 
    STA $8000

; For the prng to work, it needs a seed of any nonzero number to start at
InitSeed: 
    LDA #$08
    STA seed
    STA seed+1

InitWorld:
    LDA #< ScreenDefault ; take the low byte
    STA world ; store low byte in z page
    LDA #> ScreenDefault ; take the high byte
    STA world+1 ; store high into the world address +1 i.e the second byte of the address

; setup address in PPU for nametable data
    BIT PPUStatus ; reading PPUStatus sets the latch to access scrolling and writing to the PPU 
    LDA #$20 ; write the address of the part of vram we want to start at, upper byte first 20 -> 00
    STA PPUAddress
    LDA #$00
    STA PPUAddress

    LDX #$00
    LDY #$00
                
SetAttributes:
    LDX #$00
    LDA #$23
    STA PPUAddress
    LDA #$C0
    STA PPUAddress
    AttributeLoop:
    LDA AttributesDefault, X 
    STA $2007
    INX
    CPX #$40
    BNE AttributeLoop

    LDX #$00
    LDY #$00    

; Set Control
; to configure MMC1 mapper, we need to write to 8000 repeatedly
LDA #%00000010 ; This configures it to horizontal mirroring, 32kb of prg rom and two 8kb chr banks

STA $8000
LSR 
STA $8000
LSR 
STA $8000
LSR 
STA $8000
LSR 
STA $8000

; Set Bank to firstbank
LDA #%00000010

STA $A000
LSR
STA $A000
LSR
STA $A000
LSR
STA $A000
LSR
STA $A000

    LDA #<ScreenDefault
    STA world 
    LDA #>ScreenDefault
    STA world +1

    LDA #<CollisionMap 
    STA currentcollisionaddress
    LDA #>CollisionMap
    STA currentcollisionaddress+1


    LDA #$20 ; write the address of the part of vram we want to start at, upper byte first 20 -> 00
    STA PPUAddress
    LDA #$00
    STA PPUAddress

    JSR LoadSingleScreen

    LDA #$00
    STA currentbank
    JSR SetBank

ldx #<music_data_untitled
ldy #>music_data_untitled
        
lda #$01 ; NTSC
jsr famistudio_init
lda #$00
jsr famistudio_music_play

LDX #$60
LDY #$30
LDA #EntityType::Cursor
JSR SpawnEntity

ldx #$50
LDY #$30
LDA #EntityType::Mothership
JSR SpawnEntity

LDX #$30
LDY #$40
LDA #EntityType::Mothership
JSR SpawnEntity

LDX #$90
LDY #$50
LDA #EntityType::Mothership
JSR SpawnEntity

LDX #$70
LDY #$60
LDA #EntityType::Mothership
JSR SpawnEntity

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This is the forever loop, it goes here whenever its taken out of the NMI intterupt loop. Here is *ideally* where non draw stuff will happen...
; It runs through the whole game loop, then waits for the screen to be drawn then loops back to the beginning.
Loop:
    ; JSR ManageGameState
    ; JSR SelectGameStatePath
    JSR DoGameLogic
    JSR Setrng
    JSR IncFrameCount   ; Counts to 59 then resets to 0
    JSR OAMBuffer   ; Sprite data is written to the buffer here
    JSR famistudio_update
    
; Once the game logic loop is done, we hover here and wait for a vblank
; After a return from Vblank, we jump back to the logic loop    
IsVBlankDone:
    LDA nmidone
    CMP #$01
    BNE IsVBlankDone
    LDA #$00
    STA nmidone
    JMP Loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; NMI Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Main loop that exectutes when the NMI interrupts. Here is where we want to do our drawing. All drawing must be done before the beam resets
MAINLOOP: 
    ; load the current registers onto the stack. This doesn't matter if we finish our logic before the nmi is called, 
    ;but if it is, we need to preserve the registers so that we can carry on where we left off 
    PHA
    TXA 
    PHA 
    TYA 
    PHA

    

    LDA #$00
    STA PPUMask ; disable rendering before we access the ppu for safety
    LDA #%11000000
    STA PPUControl

    ;JSR DrawColumnNMI
    JSR ReadSprites ; Get the sprites from the sprite buffer and write them to the ppu  

    JSR ReadScroll  ; Send the current scroll to the ppu
    ;JSR UpdatePaletteNMI
    ;JSR LoadAttributesNMI
    LDA #%10010000
    ORA currenttable
    ORA #%00000100
    STA PPUControl ; ???

    LDA #%10011110
    STA PPUMask ; reenable rendering for the ppu
    ;JSR SoundPlayFrame
    INC nmidone 

    ; Bring the stack values back into the registers
    PLA
    TAY 
    PLA 
    TAX 
    PLA 
    
    RTI

; Loading into OAMDMA automatically takes what you give as a high byte and writes 256 bytes to the PPU (so 02 == 0200 all thr way to FF)
; In a real nes this neads to be done every frame b/c dynamic ram degradation, its technically possible to avoid in some emulators, but best just to do it. 
ReadSprites:
    LDA #$00
    STA $2003
    LDA #$02 ; copy sprite data from $0200 => PPU memory for display automatically.
    STA OAMDMA
    LDX #$00
RTS

ReadScroll:
    SetScroll:
    LDA PPUStatus ; reading from PPUStatus sets a latch that allows you to set the scroll in PPUAddress
    LDA scrollx
    STA PPUScroll
    LDA scrolly
    STA PPUScroll
RTS

;;;;;;;;;;;;;;;;;;;;
;; Functions to call in the main loop
;;;;;;;;;;;;;;;;;;;;

ManageGameState:
RTS

SelectGameStatePath:
    LDA GameState
    ASL 
    TAY 
    LDA GameStatePath, Y 
    STA jumppointer
    LDA GameStatePath+1, Y 
    STA jumppointer+1
    JMP (jumppointer)

DoGameLogic:
    JSR ReadButtons
    JSR ProcessPlayerInput
    JSR ProcessEntities
    RTS 



NoSpawn:
    RTS
; we never go here

ProcessPlayerInput:
    RTS


ProcessEntities:
    LDX #$00
    ProcessEntitiesLoop:
        LDA entities+Entity::type, X ; load the current entity type
        ASL ; transfer current entity type to y
        TAY  
        LDA ProcessEntityList, Y ; use y as a jump pointer to jump to the process function for that entity!
        STA jumppointer
        LDA ProcessEntityList+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    SkipEntity:
        JMP EntityComplete

    ProcessCursor:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA CursorStateMachine, Y
        STA jumppointer
        LDA CursorStateMachine+1, Y
        STA jumppointer+1
        JMP (jumppointer)

    ProcessCursorLarge:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA CursorLargeStateMachine, Y
        STA jumppointer
        LDA CursorLargeStateMachine+1, Y
        STA jumppointer+1
        JMP (jumppointer)


    ProcessMothership:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA MothershipStateMachine, Y
        STA jumppointer
        LDA MothershipStateMachine+1, Y
        STA jumppointer+1
        JMP (jumppointer)


    ; End step of processing an entity
    ; We shift the current x offset back into A, add the size of the entity struct, then put it back in A
    ; If we now process another entity, it will be offset by the size of the struct in X, giving us the address of the next entity
    EntityComplete:
    TXA 
    CLC 
    ADC #.sizeof(Entity)
    TAX 
    CMP #entity_mem  ; If we have reached the same amount as the mem taken by entities, we have looped over every entity
    BEQ :+
    JMP ProcessEntitiesLoop
    :
RTS

CursorStateMachine:
    .word CursorInit;0
    .word CursorSnap
    .word CursorGrid
    .word CursorFree
    .word CursorInactive


CursorInit:
    LDA #%00000000
    STA entities+Entity::attributes, X
    ; LDA #$00
    ; JSR InitAnimation
    LDA #$10 
    ; STA entities+Entity::animationtimer, X  
    LDA #$01
    STA entities+Entity::generalpurpose, X

    JMP EntityComplete

CursorGrid:
    LDA framecount
    BEQ :+
    JMP EntityComplete
    :
    JSR CheckRight
    CMP #ButtonReturn::Release
    BNE :+
    LDA #$08
    STA temp
    LDA #$00
    STA temp2 
    JSR ObjectMove
    :
    JSR CheckLeft
    CMP #ButtonReturn::Release
    BEQ :+
    LDA #$F8
    STA temp
    LDA #$00
    STA temp2 
    JSR ObjectMove    
    :
    JSR CheckUp
    CMP #ButtonReturn::Release
    BNE :+
    LDA #$00
    STA temp
    LDA #$F8
    STA temp2 
    JSR ObjectMove    
    :
    JSR CheckDown
    CMP #ButtonReturn::Release
    BNE :+
    LDA #$00
    STA temp
    LDA #$08
    STA temp2 
    JSR ObjectMove    
    :
    JMP EntityComplete

CursorFree:
    JSR CheckRight
    CMP #ButtonReturn::Release
    BNE :+
    JMP EntityComplete
    :
    LDA #$01
    STA temp
    LDA #$00
    STA temp2 
    JSR ObjectMove
JMP EntityComplete

CursorSnap:
    LDA framecount
    JSR CheckRight
    CMP ButtonReturn::Release
    BNE :+
    JSR CursorFindNearestRight
    LDY temp2
    LDA entities+Entity::xpos, Y
    STA entities+Entity::xpos, X
    LDA entities+Entity::ypos, Y 
    STA entities+Entity::ypos, X
    :

    JSR CheckLeft
    CMP ButtonReturn::Release
    BNE :+
    JSR CursorFindNearestLeft
    LDY temp2
    LDA entities+Entity::xpos, Y
    STA entities+Entity::xpos, X
    LDA entities+Entity::ypos, Y 
    STA entities+Entity::ypos, X
    :

    JSR CheckUp
    CMP ButtonReturn::Release
    BNE :+
    JSR CursorFindNearestUp
    LDY temp2
    LDA entities+Entity::xpos, Y
    STA entities+Entity::xpos, X
    LDA entities+Entity::ypos, Y 
    STA entities+Entity::ypos, X
    :

    JSR CheckDown
    CMP ButtonReturn::Release
    BNE :+
    JSR CursorFindNearestDown
    LDY temp2
    LDA entities+Entity::xpos, Y
    STA entities+Entity::xpos, X
    LDA entities+Entity::ypos, Y 
    STA entities+Entity::ypos, X
    :

JMP EntityComplete

CursorInactive:
    JMP EntityComplete


FindCursorObjects:
    LDX #$00
    FindCursorObjectsLoop:
        LDA entities+Entity::flags, X
        AND #%00001000
        BNE CursorEntityComplete

        JSR SpriteCollide
    
    CursorEntityComplete:
        TXA 
        CLC
        ADC #.sizeof(Entity)
        TAX
        CMP #entity_mem
        BEQ :+
        JMP FindCursorObjectsLoop
        :
RTS

CursorFindNearestRight:
    LDA #$FF 
    STA temp ; nearest distance
    LDY #$00
    STY temp2 ; nearest entity
    STX temp3 ; ourself
    FindNearestRightLoop:
        CPY #entity_mem
        BEQ EndFindNearestRight
        ;Check the entity is not ourself
        CPY temp3 
        BEQ NearestRightLoopEndEntity
        ;Check the entity is selectable
        LDA entities+Entity::flags, Y 
        AND flagtype::Selectable
        BNE NearestRightLoopEndEntity
        ; Check the entity is not the same x
        LDA entities+Entity::xpos, Y 
        CMP entities+Entity::xpos, X
        BEQ NearestRightLoopEndEntity
        ; Check if the entity is to the right
        LDA entities+Entity::xpos, Y 
        SEC 
        SBC entities+Entity::xpos, X 
        BCC NearestRightLoopEndEntity
        ;Check if distance is less than the max
        LDA temp 
        CMP entities+Entity::xpos, Y
        BCC NearestRightLoopEndEntity
        LDA entities+Entity::xpos, y
        STA temp
        STY temp2
        ; JMP EndFindNearestRight
    NearestRightLoopEndEntity:
        TYA 
        CLC 
        ADC #.sizeof(Entity)
        TAY 
        JMP FindNearestRightLoop
    EndFindNearestRight:
RTS

CursorFindNearestLeft:
    LDA #$00
    STA temp ; nearest distance
    LDY #$00
    STY temp2 ; nearest entity
    STX temp3 ; ourself
    FindNearestLeftLoop:
        CPY #entity_mem
        BEQ EndFindNearestLeft
        ;Check the entity is not ourself
        CPY temp3 
        BEQ NearestLeftLoopEndEntity
        ;Check the entity is selectable
        LDA entities+Entity::flags, Y 
        AND flagtype::Selectable
        BNE NearestLeftLoopEndEntity
        ; Check the entity is not the same x
        LDA entities+Entity::xpos, Y 
        CMP entities+Entity::xpos, X
        BEQ NearestLeftLoopEndEntity
        ; Check if the entity is to the left
        LDA entities+Entity::xpos, X 
        SEC 
        SBC entities+Entity::xpos, Y 
        BCC NearestLeftLoopEndEntity
        ;Check if distance less than current max dist
        LDA temp 
        CMP entities+Entity::xpos, Y
        BCS NearestLeftLoopEndEntity
        LDA entities+Entity::xpos, y
        STA temp
        STY temp2
        ; JMP EndFindNearestRight
    NearestLeftLoopEndEntity:
        TYA 
        CLC 
        ADC #.sizeof(Entity)
        TAY 
        JMP FindNearestLeftLoop
    EndFindNearestLeft:
RTS

CursorFindNearestDown:
    LDA #$FF 
    STA temp ; nearest distance
    LDY #$00
    STY temp2 ; nearest entity
    STX temp3 ; ourself
    FindNearestDownLoop:
        CPY #entity_mem
        BEQ EndFindNearestDown
        ;Check the entity is not ourself
        CPY temp3 
        BEQ NearestDownLoopEndEntity
        ;Check the entity is selectable
        LDA entities+Entity::flags, Y 
        AND flagtype::Selectable
        BNE NearestDownLoopEndEntity
        ; Check the entity is not the same x
        LDA entities+Entity::ypos, Y 
        CMP entities+Entity::ypos, X
        BEQ NearestDownLoopEndEntity
        ; Check if the entity is to the Down
        LDA entities+Entity::ypos, Y 
        SEC 
        SBC entities+Entity::ypos, X 
        BCC NearestDownLoopEndEntity
        ;Check if distance is less than the max
        LDA temp 
        CMP entities+Entity::ypos, Y
        BCC NearestDownLoopEndEntity
        LDA entities+Entity::ypos, y
        STA temp
        STY temp2
        ; JMP EndFindNearestDown
    NearestDownLoopEndEntity:
        TYA 
        CLC 
        ADC #.sizeof(Entity)
        TAY 
        JMP FindNearestDownLoop
    EndFindNearestDown:
RTS

CursorFindNearestUp:
    LDA #$00
    STA temp ; nearest distance
    LDY #$00
    STY temp2 ; nearest entity
    STX temp3 ; ourself
    FindNearestUpLoop:
        CPY #entity_mem
        BEQ EndFindNearestLeft
        ;Check the entity is not ourself
        CPY temp3 
        BEQ NearestUpLoopEndEntity
        ;Check the entity is selectable
        LDA entities+Entity::flags, Y 
        AND flagtype::Selectable
        BNE NearestUpLoopEndEntity
        ; Check the entity is not the same x
        LDA entities+Entity::ypos, Y 
        CMP entities+Entity::ypos, X
        BEQ NearestLeftLoopEndEntity
        ; Check if the entity is to the left
        LDA entities+Entity::ypos, X 
        SEC 
        SBC entities+Entity::ypos, Y 
        BCC NearestUpLoopEndEntity
        ;Check if distance less than current max dist
        LDA temp 
        CMP entities+Entity::ypos, Y
        BCS NearestUpLoopEndEntity
        LDA entities+Entity::ypos, y
        STA temp
        STY temp2
        ; JMP EndFindNearestRight
    NearestUpLoopEndEntity:
        TYA 
        CLC 
        ADC #.sizeof(Entity)
        TAY 
        JMP FindNearestLeftLoop
    EndFindNearestUp:
RTS

CursorSelectObject:
    LDA cursorindex
    STA selectedobject
RTS

CursorSetGrid:
    LDA #CursorState::grid
    JSR CursorSwitchMode
RTS

CursorSetFree:
    LDA #CursorState::free
    JSR CursorSwitchMode
RTS

CursorSetInactive:
    LDA #CursorState::inactive
    JSR CursorSwitchMode
RTS


; IN A: desired cursorstate
CursorSwitchMode:
    PHA
    LDY #$00
    CursorSwitchLoop:
        LDA entities+Entity::type, Y 
        CMP #EntityType::Cursor
        BNE CursorSwitchEntityComplete
        PLA
        STA entities+Entity::generalpurpose, y
        RTS 
    CursorSwitchEntityComplete:
        TYA 
        CLC 
        ADC #.sizeof(Entity)
        TAY 
        CMP #entity_mem
        BEQ :+
        JMP CursorSwitchLoop
        :   
        PLA
RTS

MothershipStateMachine:
    .word MothershipInit;0
    .word MothershipActive
    .word MothershipInactive

MothershipInit:
    LDA #$01
    STA entities+Entity::generalpurpose, X
    JMP EntityComplete

MothershipActive:
    ; LDA framecount
    ; BEQ :+
    ; JMP EntityComplete
    ; :
    ; LDA #$A0
    ; STA Vector2
    ; STA Vector2+1
    ; LDA entities+Entity::xpos, X 
    ; STA Vector
    ; LDA entities+Entity::ypos, x
    ; STA Vector+1
    ; JSR MathsGetDirectionTo
    ; LDA entities+Entity::xpos, X
    ; CLC 
    ; ADC Vector
    ; LDA entities+Entity::ypos, X
    ; CLC 
    ; ADC Vector+1 
    ; STA entities+Entity::ypos, X
    JMP EntityComplete

MothershipInactive:
    JMP EntityComplete

CursorLargeStateMachine:
    .word CursorLargeInit

CursorLargeInit:
    JMP EntityComplete

ObjectMoveTowardsPosition:
    ; get vector
    RTS

;subx in temp1 suby in temp2
ObjectMoveSub:
    LDA entities+Entity::xpossub, X
    CLC 
    ADC temp
    STA entities+Entity::xpossub, X 
    LDA entities+Entity::xpos, X 
    ADC #$00
    STA entities+Entity::xpos, X

    LDA entities+Entity::ypossub, X
    CLC 
    ADC temp2
    STA entities+Entity::ypossub, X 
    LDA entities+Entity::ypos, X 
    ADC #$00
    STA entities+Entity::ypos, X
RTS

ObjectMove:
    LDA entities+Entity::xpos, X 
    CLC 
    ADC temp
    STA entities+Entity::xpos, X
    LDA entities+Entity::ypos, X 
    CLC 
    ADC temp2
    STA entities+Entity::ypos, X
RTS
; InitAnimation:
;     STA animationtrack
;  ; animation number in A 
;     ASL 
;     TAY 
;     LDA AnimationStringsPlayer, Y 
;     STA jumppointer
;     LDA AnimationStringsPlayer+1,Y 
;     STA jumppointer+1
;     LDY #$00
;     LDA (jumppointer), Y
;     STA entities+Entity::animationframe, X 
;     INY 
;     LDA (jumppointer), Y 
;     STA entities+Entity::animationtimer, X
;     LDA #$00
;     STA animationtracktimer

; RTS 

; AdvancePlayerAnimation:
;     DEC entities+Entity::animationtimer, X 
;     BEQ :+
;     RTS 
;     :

;     LDA animationtrack
;     ASL 
;     TAY 
;     LDA AnimationStringsPlayer, Y 
;     STA jumppointer
;     LDA AnimationStringsPlayer+1,Y 
;     STA jumppointer+1


;     LDA animationtracktimer
;     TAY 
;     LDA (jumppointer), Y
;     CMP #$FF 
;     BNE :+
;         LDA #$00
;         TAY 
;         LDA (jumppointer), Y 
;         STA entities+Entity::animationframe, X
;         INY 
;         LDA (jumppointer), Y 
;         STA entities+Entity::animationtimer, X
;         INY 
;         TYA
;         STA animationtracktimer
;         RTS
;     :
;     CMP #$FE 
;     BNE :+
;         INY 
;         LDA (jumppointer), Y 
;         JSR InitAnimation
;         RTS
;     :
;     STA entities+Entity::animationframe, X 
;     INY 
;     LDA (jumppointer), Y 
;     STA entities+Entity::animationtimer, X
;     INC animationtracktimer
;     INC animationtracktimer

; RTS 


ClearEntity:
    ; wasteful but safer to clear all and not just type
    LDA #$00 
    STA entities+Entity::type, X
    STA entities+Entity::xpos, X
    STA entities+Entity::ypos, X
    STA entities+Entity::attributes, X
    STA entities+Entity::generalpurpose, X
    STA entities+Entity::animationframe, X
    ; STA entities+Entity::animationtimer, X
    JMP EntityComplete

ClearEntityUnsafe: ;
    LDA #$00
    STA entities+Entity::type, X 
    JMP EntityComplete

; ;put the entity type you want to move into a
; ; x movement into temp1
; ; y movement into temp2
; MoveEntity:

;;;;
; MATHS AGHGH
;;;;

MathsGetDirectionTo:
    LDA Vector
    SEC 
    SBC Vector2
    STA Vector

    LDA Vector+1
    SEC 
    SBC Vector2+1
    STA Vector+1
RTS


; BEFORE CALL:
;  - load screen data address into 'world'
;  - load collision data addess into currentcollisionaddress 
LoadSingleScreen:
    SEI ; disable all interuppts 
    LDA #%00010000
    STA PPUControl

    LDA #$00000000 ; disable all rendering
    STA PPUControl


    LDA #$00
    STA currentrowodd
    STA currentroweven

    LDA #$00 
    LDY #$00
    LDX #$00

    LoadScreenLoop:
        LDX #$10
        LoadOddRow:
            LDY currentrowodd
            LDA (world), Y ; get a meta tile
            ASL
            TAY ; move metatile ref to Y
            LDA  MetaTileList, Y
            STA jumppointer
            LDA MetaTileList+1, Y ; get the whole address of the data for that tile
            STA jumppointer+1
            LDY #$00 
            ; the first two bytes are uploaded directly to the ppu, as they are on the same row
            LDA (jumppointer), Y 
            STA PPUData
            INY 
            LDA (jumppointer), Y 
            STA PPUData
            INC currentrowodd
            DEX 
            BNE LoadOddRow ; break this loop when we've done one row

        LDY #$00

        LoadEvenRow:
            LDX #$00 
            LoadEvenRowLoop:
                LDY currentroweven
                LDA (world), Y
                ASL
                TAY 
                LDA MetaTileList, Y
                STA jumppointer
                LDA MetaTileList+1, Y
                STA jumppointer+1 
                LDY #$02

                LDA (jumppointer), Y 
                STA PPUData
                INY 
                LDA (jumppointer), Y 
                STA PPUData
                INC currentroweven

                INY ; 5th byte is collision data 
                LDA (jumppointer), Y
                PHA 
                TXA 
                TAY
                PLA 
                STA (currentcollisionaddress), Y

                INX 
                CPX #$10
                BNE LoadEvenRowLoop

        LDA currentcollisionaddress
        CLC 
        ADC #$10
        STA currentcollisionaddress
        LDA currentcollisionaddress+1
        ADC #$00 
        STA currentcollisionaddress+1

        LDA currentroweven
        CLC 
        ADC $10
        STA currentrowodd
        STA currentroweven
        CMP #$F0 
        BNE LoadScreenLoop

    FillCollisionMap:
        LDY #$00 
        FillCollisionMapLoop:
        LDA (world), Y 
        STA CollisionMap, Y 
        INY 
        TYA 
        CMP #$F0 
        BEQ :+
        JMP FillCollisionMapLoop
        :
 

EndScreenLoad:
    ; LDA #%10010000 ; enable NMI, change background to use second chr set of tiles ($1000)
    ; STA PPUControl
    ; ; Enabling sprites and background for left-most 8 pixels
    ; ; Enable sprites and background
    ; LDA #%001`11110
    ; STA $PPUMask

LDA #%10010000 ; enable NMI, change background to use second chr set of tiles ($1000)
STA PPUControl
; Enabling sprites and background for left-most 8 pixels
; Enable sprites and background
LDA #%00111110
STA PPUMask
CLI
RTS

; X, Y and type in X,Y,A 
; temp = attributes
; temp2 = collision
SpawnEntity:
    PHA 
    TXA 
    PHA 
    TYA 
    PHA ; put ent type, x, y on stack
    LDX #$00
    EurydiceLoop:
        CPX #entity_mem
        BEQ EndEurydiceSpawn

        LDA entities+Entity::type, X ; grab the next entity 
        CMP #EntityType::NoEntity ; is the slot empty?
        BEQ AddEurydice ; if empty, branch
        TXA ; else add an offset then check for the next entity slot
        CLC
        ADC #.sizeof(Entity)
        TAX 
        JMP EurydiceLoop
    AddEurydice: 
        PLA; grab y pos we set before jumping here
        STA entities+Entity::ypos, X
        PLA ; grab x pos 
        STA entities+Entity::xpos, X
        PLA 
        STA entities+Entity::type, X
        ; set special attributes for this object while we have the type
        JSR SetObjectProperties
        LDA temp 
        STA entities+Entity::attributes, X 
        LDA #$00
        STA entities+Entity::generalpurpose, X 
        ; STA entities+Entity::animationtimer, X
        RTS 
EndEurydiceSpawn:
    PLA 
    PLA 
    PLA
    RTS

; Entered when spawning
SetObjectProperties:
    LDA entities+Entity::type, X
    ASL 
    TAY 
    LDA EntityProperties, Y 
    STA jumppointer
    LDA EntityProperties+1, Y
    STA jumppointer+1
    LDY #$00
    LDA (jumppointer), Y
    ; first byte is whether selectable
    ORA entities+Entity::flags, X
    STA entities+Entity::flags, X
RTS

IncFrameCount:
    INC framecount
    LDA framecount
    CMP #$3C
    BNE :+
    ; LDA #%10000000
    ; STA buttons
    LDA #$00
    :
    STA framecount
RTS 

CheckForBankSwap:
    LDA #%10000000
    BIT drawflags
    BEQ :+
        RTS 
    LDA currentbank
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LDA #%10000000
    EOR drawflags
    STA drawflags
RTS 

AlternateBanks:
    LDA framecount
    BEQ :+
    RTS
    :
    LDA currentbank
    EOR #$02 
    STA currentbank 
    JSR SetBank
    RTS

SetBank: ; sets A as the bank to be used
; lower 5 bits. lowest ignored in 8kb mode
    LDA currentbank
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
RTS 


;;;;;;;;;;;;;
;Input 
;;;;;;;;;;;;;
ReadButtons:
    ; P  ing $4016 with a 1 to get it ready to send buttons 
    LDA #$01
    STA $4016
    STA buttons        ; Put 1 into buttons so we can use it to manipulate the carry flag in 8 loops time
    LSR A ; sets the carry
    STA $4016

    ReadButtonLoop:
        LDA $4016
        ROR 
        ROL buttons ; ror + rol moves the 
    
        BCC ReadButtonLoop ; after 8 loops, the 1 will shift back into the carry and end the loop

    LDA #$01
    LSR 

    ReadButtonLoopP2:
        LDA $4017
        ROR 
        ROL buttonsp2
        BCC ReadButtonLoopP2
    LDA #$01
    LSR 
RTS


CheckA:
    LDA buttons 
    AND #%10000000 ; if the first nibble is set then a is pressed
    BEQ CheckARelease
    LDA buttonflag ; if the button is pressed, set this so that we can check release next frame
    ORA #$01
    STA buttonflag
    LDA #ButtonReturn::Press 
    RTS

    CheckARelease: ; If the button isn't pressed, check whether it was pressed last frame and released
        LDA buttonflag
        AND #$01
        BEQ :+
        LDA buttonflag
        EOR #$01 
        STA buttonflag
        LDA #ButtonReturn::Release
        RTS 
:
LDA #$00
RTS 

CheckB:

    LDA buttons 
    AND #%01000000
    BEQ CheckBRelease
    LDA buttonflag
    ORA #$02
    STA buttonflag
    LDA #ButtonReturn::Press
    RTS

    CheckBRelease:
        LDA buttonflag
        AND #$02
        BEQ :+
        LDA buttonflag
        EOR #$02 
        STA buttonflag
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckSelect:
    LDA buttons
    AND #%00100000
    BEQ CheckSelectRelease 
    LDA buttonflag
    ORA #$04 
    STA buttonflag    
    LDA ButtonReturn::Press
    RTS
    CheckSelectRelease:
        LDA buttonflag
        AND #$04
        BEQ :+
        LDA buttonflag
        EOR #$04 
        STA buttonflag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckStart:
    LDA buttons
    AND #%00010000
    BEQ CheckStartRelease
    LDA buttonflag
    ORA #$08
    STA buttonflag
    LDA ButtonReturn::Press
    RTS
    CheckStartRelease:
        LDA buttonflag
        AND #$08
        BEQ :+
        LDA buttonflag
        EOR #$08 
        STA buttonflag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckUp:  
    LDA buttons
    AND #%00001000
    BEQ  CheckUpRelease
    LDA buttonflag
    ORA #$10
    STA buttonflag
    LDA ButtonReturn::Press
    RTS
    CheckUpRelease:
        LDA buttonflag
        AND #$10
        BEQ :+
        LDA buttonflag
        EOR #$10 
        STA buttonflag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckDown:
    LDA buttons
    AND #%00000100
    BEQ CheckDownRelease 
    LDA buttonflag 
    ORA #$20 
    STA buttonflag 
    LDA ButtonReturn::Press
    RTS
    CheckDownRelease:
        LDA buttonflag
        AND #$20
        BEQ :+
        LDA buttonflag
        EOR #$20 
        STA buttonflag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckLeft:
    LDA buttons
    AND #%00000010
    BEQ CheckLeftRelease
    LDA buttonflag
    ORA #$40 
    STA buttonflag 
    LDA #$01
    RTS
    CheckLeftRelease:
        LDA buttonflag
        AND #$04
        BEQ :+
        LDA buttonflag
        EOR #$04 
        STA buttonflag
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckRight:
    LDA buttons
    AND #%00000001
    BEQ CheckRightRelease
    LDA buttonflag 
    ORA #$80 
    STA buttonflag
    LDA #ButtonReturn::Press
    RTS
    CheckRightRelease:
        LDA buttonflag
        AND #$80
        BEQ :+
        LDA buttonflag
        EOR #$80 
        STA buttonflag
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckAP2:
    LDA buttonsp2 
    AND #%10000000 ; if the first nibble is set then a is pressed
    BEQ CheckAReleaseP2
    LDA buttonflagp2 ; if the button is pressed, set this so that we can check release next frame
    ORA #$01
    STA buttonflagp2
    LDA #$01 
    RTS

    CheckAReleaseP2: ; If the button isn't pressed, check whether it was pressed last frame and released
        LDA buttonflagp2
        AND #$01
        BEQ :+
        LDA buttonflagp2
        EOR #$01 
        STA buttonflagp2
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckBP2:

    LDA buttonsp2 
    AND #%01000000
    BEQ CheckBReleaseP2
    LDA buttonflagp2
    ORA #$02
    STA buttonflagp2
    LDA #ButtonReturn::Press
    RTS

    CheckBReleaseP2:
        LDA buttonflagp2
        AND #$02
        BEQ :+
        LDA buttonflagp2
        EOR #$02 
        STA buttonflagp2
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckSelectP2:
    LDA buttonsp2
    AND #%00100000
    BEQ CheckSelectReleaseP2 
    LDA buttonflagp2
    ORA #$04 
    STA buttonflagp2    
    LDA ButtonReturn::Press
    RTS
    CheckSelectReleaseP2:
        LDA buttonflagp2
        AND #$04
        BEQ :+
        LDA buttonflagp2
        EOR #$04 
        STA buttonflagp2
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckStartP2:
    LDA buttonsp2
    AND #%00010000
    BEQ CheckStartReleaseP2
    LDA buttonflagp2
    ORA #$08
    STA buttonflagp2
    LDA ButtonReturn::Press
    RTS
    CheckStartReleaseP2:
        LDA buttonflagp2
        AND #$08
        BEQ :+
        LDA buttonflagp2
        EOR #$08 
        STA buttonflagp2
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckUpP2:  
    LDA buttonsp2
    AND #%00001000
    BEQ  CheckUpReleaseP2
    LDA buttonflagp2
    ORA #$10
    STA buttonflagp2
    LDA ButtonReturn::Press
    RTS
    CheckUpReleaseP2:
        LDA buttonflagp2
        AND #$10
        BEQ :+
        LDA buttonflagp2
        EOR #$10 
        STA buttonflagp2
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckDownP2:
    LDA buttonsp2
    AND #%00000100
    BEQ CheckDownReleaseP2 
    LDA buttonflagp2 
    ORA #$20 
    STA buttonflagp2 
    LDA ButtonReturn::Press
    RTS
    CheckDownReleaseP2:
        LDA buttonflagp2
        AND #$20
        BEQ :+
        LDA buttonflagp2
        EOR #$20 
        STA buttonflagp2
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckLeftP2:
    LDA buttonsp2
    AND #%00000010
    BEQ CheckLeftReleaseP2
    LDA buttonflagp2
    ORA #$40 
    STA buttonflagp2 
    LDA #$01
    RTS
    CheckLeftReleaseP2:
        LDA buttonflagp2
        AND #$04
        BEQ :+
        LDA buttonflagp2
        EOR #$04 
        STA buttonflagp2
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckRightP2:

    LDA buttonsp2
    AND #%00000001
    BEQ CheckRightReleaseP2
    LDA buttonflagp2 
    ORA #$80 
    STA buttonflagp2
    LDA #$01
    RTS
    CheckRightReleaseP2:
        LDA buttonflagp2
        AND #$80
        BEQ :+
        LDA buttonflagp2
        EOR #$80 
        STA buttonflagp2
        LDA #$02
        RTS 
:
LDA #$00
RTS 



;;;;;;;;
; Collision
;;;;;;;;
CollideLeft2:
; LDA #$3f
;     STA $2001
    
    LDA #$00
    STA var_mem
    CollideLeft2Loop:
    LDA entities+Entity::xpos, X ;4  
    LSR ;2
    LSR ;2
    LSR ;2
    LSR;2

    STA temp ; 3 

    LDA entities+Entity::ypos, X ;
    ; CLC 
    ; ADC #$07
    LSR ;2
    LSR ;2
    LSR ;2
    LSR ;2

    ASL ;2
    ASL ;2
    ASL 
    ASL 

    STA temp2 ;3

    CLC ; 2
    ADC temp ;2 
    TAY ; 2
    LDA CollisionMap, Y ; 4/5
    ASL ;2 
    TAY  ;2
    LDA MetaTileList, Y ;4/5
    STA jumppointer ; 3
    LDA MetaTileList+1, Y ; 4/5 
    STA jumppointer+1 ;3

    LDY #$04 ; 2 
    LDA temp ; 3
    ASL ;2 
    ASL ;2
    ASL ;2
    ASL ; X16 to return it to world scale 
    STA temp ;3  
    LDA entities+Entity::xpos, X ;4/5
    SEC ; 2
    SBC temp
    ; SBC temp ; get difference between two 
    CMP #$09 ; 2
    BCC :+ ; 2/3
    INY
    :
    LDA temp2 ;3  
    SEC ;2
    SBC entities+Entity::ypos, X ; 4/5 
    CMP #$09 ;2
    BCC :+ ;2/3
    INY
    INY
    :
    LDA (jumppointer), Y ;5/6
    STA rectangle1 ;3

    ; get collision of second point 
    LDA entities+Entity::xpos, X 
    LSR 
    LSR 
    LSR 
    LSR

    STA temp 

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$07
    LSR 
    LSR
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2

    CLC
    ADC temp 
    TAY 
    LDA CollisionMap, Y
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer 
    LDA MetaTileList+1, Y 
    STA jumppointer+1

    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp 
    LDA entities+Entity::xpos, X
    SEC 
    SBC temp
    ; SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY 
    :
    LDA temp2 
    SEC 
    SBC entities+Entity::ypos, X
    CMP #$09
    BCC :+
    INY 
    INY 
    :
    LDA (jumppointer), Y 
    ORA rectangle1

    BEQ :+
        STA var_mem
        INC entities+Entity::xpos, X 
        JMP CollideLeft2Loop
    :

    ; LDA #$1E
    ; STA $2001
    LDA var_mem
    RTS 

CollideLeft:
    LDA entities+Entity::xpos, X
    SEC
    SBC #$01 ; add 8 pixels for a single sprite
    LSR 
    LSR 
    LSR 
    LSR 

    STA temp

    LDA entities+Entity::ypos, X 
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    STA temp2 

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp 
    TAY 
    LDA CollisionMap, Y 
    ORA temp2

    RTS 

CollideRight2:
; LDA #$3f
;     STA $2001
    

    LDA #$00
    STA var_mem

    CollideRight2Loop:

    LDA entities+Entity::xpos, X ;4  
    CLC ; 2
    ADC #$07 ; 2
    LSR ;2
    LSR ;2
    LSR ;2
    LSR;2

    STA temp ; 3 

    LDA entities+Entity::ypos, X ;
    ; CLC 
    ; ADC #$07
    LSR ;2
    LSR ;2
    LSR ;2
    LSR ;2

    ASL ;2
    ASL ;2
    ASL 
    ASL 

    STA temp2 ;3

    CLC ; 2
    ADC temp ;2 
    TAY ; 2
    LDA CollisionMap, Y ; 4/5
    ASL ;2 
    TAY  ;2
    LDA MetaTileList, Y ;4/5
    STA jumppointer ; 3
    LDA MetaTileList+1, Y ; 4/5 
    STA jumppointer+1 ;3

    LDY #$04 ; 2 
    LDA temp ; 3
    ASL ;2 
    ASL ;2
    ASL ;2
    ASL ; X16 to return it to world scale 
    STA temp ;3  
    LDA entities+Entity::xpos, X ;4/5
    SEC ; 2
    SBC temp
    ; SBC temp ; get difference between two 
    CMP #$09 ; 2
    BCC :+ ; 2/3
    INY
    :
    LDA temp2 ;3  
    SEC ;2
    SBC entities+Entity::ypos, X ; 4/5 
    CMP #$09 ;2
    BCC :+ ;2/3
    INY
    INY
    :
    LDA (jumppointer), Y ;5/6
    STA rectangle1 ;3

    ; get collision of second point 
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR

    STA temp 

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$07
    LSR 
    LSR
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2

    CLC
    ADC temp 
    TAY 
    LDA CollisionMap, Y
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer 
    LDA MetaTileList+1, Y 
    STA jumppointer+1

    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp 
    LDA entities+Entity::ypos, X
    SEC 
    SBC temp
    ; SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY 
    :
    LDA temp2 
    SEC 
    SBC entities+Entity::ypos, X
    CMP #$09
    BCC :+
    INY 
    INY 
    :
    LDA (jumppointer), Y 
    ORA rectangle1

    BEQ :+
        STA var_mem
        DEC entities+Entity::xpos, X 
        JMP CollideRight2Loop
    :
    ; LDA #$1E
    ; STA $2001
    
    LDA var_mem
    RTS 

CollideRight: ; only to be called dfrom process entities
    LDA entities+Entity::xpos, X
    CLC 
    ADC #$08 ; add 8 pixels for a single sprite 
    LSR 
    LSR 
    LSR 
    LSR 

    STA temp

    LDA entities+Entity::ypos, X 
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    STA temp2 

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp 
    TAY 
    LDA CollisionMap, Y 
    ORA temp2

    RTS 

CollideDown2:

    ; LDA #$3f
    ; STA $2001
    
    LDA #$00
    STA var_mem

    CollideDown2Loop:
    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X 
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X 
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    ; LDY #$04
    ; LDA (jumppointer), Y 
    ; RTS

    ; ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp 
    LDA entities+Entity::xpos, X 
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY
    :
    LDA temp2
    SEC 
    SBC entities+Entity::ypos, X 
    CMP #$09 
    BCC :+
    INY 
    INY
    :
    LDA (jumppointer), Y
    STA rectangle1



    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$06 ; move 1 pixel in fromthe centre 
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X 
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    LDY #$04
    LDA (jumppointer), Y 

    ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp 
    LDA entities+Entity::xpos, X 
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY
    :
    LDA temp2
    SEC 
    SBC entities+Entity::ypos, X
    CMP #$09
    BCC :+
    INY
    INY
    :
    LDA (jumppointer), Y
    ORA rectangle1

    BEQ :+
        STA var_mem
        DEC entities+Entity::ypos, X 
        JMP CollideDown2Loop
    :
    ; LDA #$1E
    ; STA $2001
    
    LDA var_mem
    RTS 

CollideDown:
    LDA entities+Entity::xpos, X
    CLC 
    ADC #$01 ; get the centre of the sprite(ish)
    LSR 
    LSR 
    LSR 
    LSR 
    ; AND #%00011111 ; mask any bits over 16 
    STA temp

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR 
    ; AND #%00011111 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    STA temp2

    LDA entities+Entity::xpos, X
    CLC 
    ADC #$07 ; get the centre of the sprite(ish)
    LSR 
    LSR 
    LSR 
    LSR 
    STA temp

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$08
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    ORA temp2
    RTS

CollideUp2:
    LDA #$00
    STA var_mem
    CollideUp2Loop:
    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X  
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    ; LDY #$04
    ; LDA (jumppointer), Y 
    ; RTS

    ; ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp  
    LDA entities+Entity::xpos, X 
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY
    :
    LDA temp2  
    SEC 
    SBC entities+Entity::ypos, X 
    CMP #$09 
    BCC :+
    INY
    INY 
    :
    LDA (jumppointer), Y
    STA rectangle1

    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$07 ; move 1 pixel in fromthe centre 
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X  
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    ; LDY #$04
    ; LDA (jumppointer), Y 
    ; RTS

    ; ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp
    LDA entities+Entity::xpos, X  
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCS :+
    INY
    :
    LDA temp2 
    SEC 
    SBC entities+Entity::ypos, X 
    CMP #$09 
    BCC :+
    INY 
    INY 
    :
    LDA (jumppointer), Y
    ORA rectangle1
    
    BEQ :+ 
        STA var_mem 
        INC entities+Entity::ypos, X 
        JMP CollideUp2Loop
    :
    LDA var_mem
    RTS 


CollideUp:
    LDA entities+Entity::xpos, X
    CLC 
    ADC #$01 ; get the centre of the sprite(ish)
    LSR 
    LSR 
    LSR 
    LSR 
    STA temp

    LDA entities+Entity::ypos, X
    SEC 
    SBC #$01 
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL
    ASL
    ASL 

    CLC 
    ADC temp 
    TAY 
    LDA CollisionMap, Y 
    STA temp2 

    LDA entities+Entity::xpos, X
    CLC 
    ADC #$07 ; get the centre of the sprite(ish)
    LSR 
    LSR 
    LSR 
    LSR 
    STA temp

    LDA entities+Entity::ypos, X
    SEC 
    SBC #$01 
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL
    ASL
    ASL 

    CLC 
    ADC temp 
    TAY 
    LDA CollisionMap, Y 
    ORA temp2
    RTS 

SpriteCollide: 
    TXA 
    PHA 
    STA temp ; save your own position so it can be skipped
    
    LDA entities+Entity::xpos, X
    STA rectangle1
    CLC 
    ADC #$07
    STA rectangle1+1
    LDA entities+Entity::ypos, X
    STA rectangle1+2 
    CLC 
    ADC #$07
    STA rectangle1+3
 
    LDX #$00
    SpriteCollideLoop:
    CPX temp 
    BEQ CollideSpriteComplete
    LDA entities+Entity::type, X 
    BEQ CollideSpriteComplete
    CLC
    LDA entities+Entity::xpos, X
    STA rectangle2
    ; CLC 
    ADC #$07
    STA rectangle2+1
    LDA entities+Entity::ypos, X
    STA rectangle2+2 
    ; CLC 
    ADC #$07
    STA rectangle2+3 

    LDA rectangle1
    CMP rectangle2+1
    BCS CollideSpriteComplete
    LDA rectangle1+1 
    CMP rectangle2
    BCC CollideSpriteComplete
    LDA rectangle1+2
    CMP rectangle2+3 
    BCS CollideSpriteComplete
    LDA rectangle1+3 
    CMP rectangle2+2 
    BCC CollideSpriteComplete
    TXA ; X = the entity we have successfuly collided
    TAY  ; y = ditto
    PLA  ; pull X (the current entiy being processed)
    TAX  ; put it back in x ready to go back to the entity process loop
    TYA         
    RTS ; return with the collided object ID in A

    CollideSpriteComplete:
    TXA 
    CLC 
    ADC #.sizeof(Entity)
    TAX 
    CMP #entity_mem
    BEQ :+
    JMP SpriteCollideLoop
    : 

EndSpriteCollide:
    PLA 
    TAX 
    LDA #$FF
    RTS

;put the object offset number in X before jumping here so it can be excluded
; leaves the nearest object in temp1
NearestSpriteRight:
    LDA #$FF
    STA temp2 ; furthest right distance so far
    LDA #$00 
    STX temp ; closest entity holder 
    LDY #$00
    NearestSpriteRightLoop:
    ; check the entity isn't empty
    LDA entities+Entity::xpos, Y 
    BEQ NearestSpriteLoopEntityComplete
    ; check we arent checking ourself
    CMP EntityType::NoEntity
    BEQ NearestSpriteLoopEntityComplete
    ; check if the current entity is to our right
    LDA entities+Entity::xpos, Y
    CMP entities+Entity::xpos, X
    BCC NearestSpriteLoopEntityComplete
    ; check if the current entity is closer than the closest so far
    CMP temp2 
    BCS NearestSpriteLoopEntityComplete
    STA temp2
    STY temp
    NearestSpriteLoopEntityComplete:
    TYA 
    CLC 
    ADC #.sizeof(Entity)
    TAY 
    CMP #entity_mem  ; If we have reached the same amount as the mem taken by entities, we have looped over every entity
    BEQ :+
    JMP NearestSpriteRightLoop
    :
RTS

NMI:            ; this happens once a frame when the draw arm mabob is returning to the top of the screen
    JMP MAINLOOP
    RTI

;;;;;
;PRNG GENERATOR 
;https://www.nesdev.org/wiki/Random_number_generator
;;;;;;
;Generates a pseudo random number inthe A register
;133-147 cycles per call
; this is called and stored once per frame, if a unique one is needed, call again
Prng: 
    LDY #08     ; iteration count (generates 8 bits)
	LDA seed+0
:
	ASL        ; shift the register
	ROL seed+1
	BCC :+
	EOR #$39   ; apply XOR feedback whenever a 1 bit is shifted out
:
	DEY
	BNE :--
	STA seed+0
	CMP #00     ; reload flags
	RTS

Setrng:
    JSR Prng
    STA rng
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OAM Buffer Handling ; All sprite data for the frame must be written into the OAM so that it can be sent to the PPU in vblank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

OAMBuffer:
    ; Clear all sprite data from last frame 
    ClearSpriteBuffer:
        LDA #$00 
        STA spritebufferposition
        LDX #$00
        LDA #$FF 
        ClearBufferLoop:
            STA $0200, X
            INX 
            CPX #$FF
            BNE ClearBufferLoop

            LDX #$00
            LDY #$00
            LDA #$00
    DrawSprites:
        LDA entities+Entity::type, X 
        ASL 
        TAY 
        LDA DrawSpriteList, Y
        STA jumppointer
        LDA DrawSpriteList+1, Y  
        STA jumppointer+1
        JMP (jumppointer)

    DrawSpriteInit:
        LDY #$00
    DrawSpriteLoop:  
        LDA jumppointer, Y

        ; get y offset 
        LDA (jumppointer), Y
        CLC 
        ADC entities+Entity::ypos, X
        ; SEC 
        ; SBC #$01 ; Ypos scanline is off by one and needs correcting 
        STY temp
        LDY spritebufferposition
        STA SpriteBuffer, Y
        INC spritebufferposition    
        LDY temp 
        INY 

        ; Tile
        LDA (jumppointer), Y
        STY temp 
        LDY spritebufferposition
        STA SpriteBuffer, Y 
        LDY temp
        INC spritebufferposition
        INY
        ; Attributes
        LDA (jumppointer), Y
        ORA entities+Entity::attributes, X 
        STY temp 
        LDY spritebufferposition
        STA SpriteBuffer, Y 
        LDY temp 
        INC spritebufferposition
        INY 

        LDA (jumppointer), Y 
        CLC 
        ADC entities+Entity::xpos, X 
        STY temp 
        LDY spritebufferposition
        STA SpriteBuffer, Y 
        LDY temp 
        INY 
        INC spritebufferposition

        LDA (jumppointer), Y 
        CMP #$FF 
        BNE DrawSpriteLoop  
    
    CheckEndSpriteDraw:
        TXA 
        CLC 
        ADC #.sizeof(Entity)
        TAX 
        CPX #entity_mem
        BEQ EndSpriteDraw
        JMP DrawSprites

    EndSpriteDraw:
        RTS 

; Draw list 
SkipDraw:
    JMP CheckEndSpriteDraw

DrawCursor:
    ; Check drawflags
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListCursor, Y 
    STA jumppointer
    LDA MetaSpriteListCursor+1, Y
    STA jumppointer+1
    LDY #$00
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawCursorLarge:
    ; Check drawflags
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListCursorLarge, Y 
    STA jumppointer
    LDA MetaSpriteListCursorLarge+1, Y
    STA jumppointer+1
    LDY #$00
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit


DrawMotherShip:
    ; Check drawflags
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListMothership, Y 
    STA jumppointer
    LDA MetaSpriteListMothership+1, Y
    STA jumppointer+1
    LDY #$00
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

; object to snap in X
SnapSpriteToGrid:
    LDA entities+Entity::xpos, X 
    CLC
    ROR 
    CLC
    ROR 
    ROL 
    ROL 
    STA entities+Entity::xpos, X


    LDA entities+Entity::ypos, X 
    CLC
    ROR 
    CLC
    ROR 
    ROL 
    ROL 
    STA entities+Entity::ypos, X
;;;;;;;;
;; DEATH FUNCTIONS
;; These are all called from ProcessDestructionStack and MUST return there when they finish
;;;;;;;';'

NoDeathAction:
    PLA 
    PLA 
    PLA
    RTS



;;;;;;;;;;;;


; .segment "DATA"

; Meta tile definitions. The first 4 bytes refer to tiles in chr rom
; The 5th byte is the collision for the block. 0=no collide 1 = collide
empty_space:
    .byte $10,$10,$10,$10 
    .byte $00,$00,$00,$00 
colour1:
    .byte $20,$20,$20,$20 
    .byte $00,$00,$00,$00 
colour2:
    .byte $30,$30,$30,$30 
    .byte $00,$00,$00,$00 
colour3:
    .byte $40,$40,$40,$40 
    .byte $00,$00,$00,$00 
planet:
    .byte $EE,$EF,$FE,$FF ; chr rom reference bytes
    .byte $00,$00,$00,$00 ; collision bytes 
planet_destroyed:
    .byte $EC,$ED,$FC,$FD 
    .byte $00,$00,$00,$00 
nebula:
    .byte $EA,$EB,$FA,$FB 
    .byte $00,$00,$00,$00 
asteroids:
    .byte $E8,$E9,$F8,$F9 
    .byte $00,$00,$00,$00 

MetaTileList:
    .word empty_space ; 00
    .word colour1
    .word colour2
    .word colour3
    .word planet ;04
    .word planet_destroyed ;05
    .word nebula ;06
    .word asteroids ;07
PaletteData:
    .byte $0F,$01,$11,$21,  $0F,$01,$03,$0A,  $0F,$07,$16,$27, $0F,$05,$15,$30  ;background palette data  
    .byte $0F,$17,$27,$30,  $0F,$1A,$2A,$30,  $0F,$13,$23,$30, $0F,$2d,$3D,$30  ;sprite palette data

ScreenDefault: ; the  format of a screen is that each byte represents 1 meta tile, made up of 4 8x8 pixel blocks to save huge
; amounts ofbytes in the long run
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$06,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$07,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
ScreenDefault2:
    .byte $24,$00,$00,$00,$00,$00,$00,$15,$15,$00,$00,$00,$00,$00,$00,$23
    .byte $24,$00,$13,$12,$14,$00,$11,$0C,$0D,$15,$00,$00,$00,$00,$00,$23
    .byte $24,$11,$31,$31,$00,$00,$11,$0E,$0F,$15,$00,$00,$31,$31,$00,$23
    .byte $24,$00,$04,$05,$04,$05,$00,$10,$01,$00,$04,$05,$00,$04,$05,$23
    .byte $24,$00,$04,$05,$00,$00,$30,$30,$1D,$19,$1A,$21,$22,$19,$1A,$23
    .byte $24,$00,$04,$05,$00,$08,$09,$00,$05,$02,$00,$02,$10,$02,$00,$23
    .byte $29,$00,$04,$05,$00,$0A,$0B,$00,$00,$02,$00,$02,$00,$02,$00,$29
    .byte $24,$00,$21,$22,$00,$22,$21,$00,$10,$21,$1F,$20,$00,$00,$31,$23
    .byte $24,$12,$23,$24,$02,$00,$00,$04,$05,$00,$00,$02,$00,$00,$00,$23
    .byte $29,$12,$25,$26,$32,$00,$03,$04,$05,$02,$00,$32,$00,$1D,$30,$23
    .byte $24,$10,$08,$09,$02,$00,$31,$04,$05,$31,$00,$02,$00,$10,$05,$23
    .byte $24,$00,$0A,$0B,$02,$31,$03,$19,$1A,$02,$00,$19,$1A,$00,$00,$23
    .byte $31,$31,$32,$32,$31,$31,$00,$00,$31,$31,$31,$31,$32,$32,$31,$31
    .byte $29,$2F,$04,$04,$05,$2F,$05,$05,$04,$04,$2F,$2F,$05,$04,$2F,$29
    .byte $07,$1D,$1E,$1E,$1D,$1D,$1E,$1E,$1E,$1E,$1D,$1D,$1E,$1E,$1D,$07

AttributesDefault: ; each attribute byte sets the pallete for a block of pixels
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

;;;;
;Property Lists
;;;;
EntityProperties:
    .word NoProperties
    .word CursorProperties 
    .word CursorLargeProperties
    .word MotherShipProperties

; properties = selectable ->
NoProperties:
    .byte $00
CursorProperties:
    .byte $00 ; selectable ->
CursorLargeProperties:
    .byte $00
MotherShipProperties:
    .byte %00001000
;;;;;;;;;;;;
;;; LOOK UP TABLES
;;;;;;;;;;;

Sin:
    .byte $01,$00,$01,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$00,$00
    .byte $FF,$00,$FF,$00,$FF,$00,$00,$FF,$00,$00,$FF,$00,$00,$00,$00

GameStatePath:
    .word DoGameLogic
    .word DoGameLogic
    .word DoGameLogic

DestroyEntityList: ; defines behaviours for when an entity is destroyed
    .word NoDeathAction ; 0 
    .word NoDeathAction ; 1

ProcessEntityList: ; Jump table for processing entities
    .word SkipEntity
    .word ProcessCursor
    .word ProcessCursorLarge
    .word ProcessMothership

DrawSpriteList: ; this is a list of sprite definitions
    .word SkipDraw
    .word DrawCursor
    .word DrawCursorLarge
    .word DrawMotherShip

MetaSpriteListCursor:
    .word CursorSprite1 ; 0
    .word CursorSprite2 ; 1

MetaSpriteListCursorLarge:
    .word CursorLargeSprite1 ; 0
    .word CursorLargeSprite2 ; 1

MetaSpriteListMothership:
    .word MothershipSprite1
    .word MothershipSprite2

AnimationStringsPlayer:
    .word AnimationStringPlayerIdle
    .word AnimationStringPlayerRunning
    .word AnimationStringPlayerCrouching
    .word AnimationStringPlayerJumping
    .word AnimationStringPlayerFalling
    .word AnimationStringPlayerFallingEnd
    .word AnimationStringPlayerFiring




CursorSprite1:
    .byte $00,$01,$00,$00;,$00,$00 ; -> sprite no  -> xoffset -> Yoffset -> fliph -> flipv
    .byte $FF ; termination byte 
CursorSprite2:
    .byte $00,$01,$00,$00;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte

CursorLargeSprite1:
    .byte $00,$03,$00,$00
    .byte $00,$04,$00,$00
    .byte $00,$13,$00,$00
    .byte $00,$14,$00,$00
    .byte $FF

CursorLargeSprite2:
    .byte $00,$05,$00,$00
    .byte $00,$06,$00,$00
    .byte $00,$15,$00,$00
    .byte $00,$16,$00,$00
    .byte $FF

MothershipSprite1:
    .byte $00,$11,$01,$00;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
MothershipSprite2:
    .byte $00,$11,$01,$00;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte



AnimationStringPlayerIdle:
    .byte $00,$1A,$01,$1A,$FF ; Animation frame -> Timer FF=repeat
AnimationStringPlayerRunning:
    .byte $02,$08,$03,$08,$FF
AnimationStringPlayerCrouching:
    .byte $01 ; animation length
    .byte $04
    .byte $FF ; loop
AnimationStringPlayerJumping:
    .byte $01 ; animation frame
    .byte $05 ; ani length
    .byte $FF ; loop
AnimationStringPlayerFalling:
    .byte $00,$80,$FF
AnimationStringPlayerFallingEnd:
    .byte $04,$09,$FE,$00
AnimationStringPlayerFiring:
    .byte $01 ; animation length
    .byte $07
    .byte $FF

NoDraw:
    .byte $FF ; termination byte


.include "famistudio_ca65.s"
.include "testsong.s"


.segment "SONG1"
; 
; .segment "DPCM"


.segment "VECTORS"      ; This part just defines what labels to go to whenever the nmi or reset is called 
    .word NMI           ; If you look at someone elses stuff they probably call this vblank or something
    .word Reset
     
.segment "CHARS" ; sprite/tile data goes here
    .incbin "CorinthiaBank1.chr"