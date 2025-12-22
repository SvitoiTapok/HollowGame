module Button
open Animation


type MenuAction = 
    | StartGame
    | Exit
type Button = {
    Bounds: DrawableObject
    ButtonType: MenuAction
}
