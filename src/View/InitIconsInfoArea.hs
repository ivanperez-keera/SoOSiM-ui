-- | Loads the icons into the component info icon view
module View.InitIconsInfoArea where

-- External libraries
import Graphics.UI.Gtk

-- Internal libraries
import View.Objects
import Paths

type IconsInfoViewStore = ListStore (Pixbuf, String)

-- | Loads the icons into the component info icon view
initIconsInfoArea :: Builder -> IO IconsInfoViewStore
initIconsInfoArea bldr = do

  m  <- listStoreNew =<< getIconList
  iv <- infoIconView bldr

  iconViewSetModel iv (Just m)

  col1 <- treeViewColumnNew
  renderer1 <- cellRendererTextNew
  cellLayoutPackStart col1 renderer1 True
  cellLayoutSetAttributes col1 renderer1 m $ \row -> [ cellText := snd row ]

  icon <- treeViewColumnNew
  renderIcon <- cellRendererPixbufNew
  cellLayoutPackStart icon renderIcon True
  cellLayoutSetAttributes icon renderIcon m $ \row -> [ cellPixbuf := fst row ]

  treeModelSetColumn m _PIXBUF_COLUMN fst
  treeModelSetColumn m _STRING_COLUMN snd

  iconViewSetPixbufColumn iv _PIXBUF_COLUMN
  iconViewSetTextColumn iv _STRING_COLUMN

  return m

-- | The pixbuf column
_PIXBUF_COLUMN :: ColumnId (Pixbuf, String) Pixbuf
_PIXBUF_COLUMN = makeColumnIdPixbuf 0

-- | The string column
_STRING_COLUMN :: ColumnId (Pixbuf, String) String
_STRING_COLUMN = makeColumnIdString 1

-- | The icon list with proper sizes
getIconList :: IO [(Pixbuf, String)]
getIconList = mapM f icons
 where f (x,y) = do w  <- getDataFileName x
                    pb <- pixbufNewFromFileAtSize w stdIconSize stdIconSize
                    return (pb, y)

-- | The list of icons and their labels
icons :: [(String, String)]
icons = [ ("images/icons/info.png",  "Basic info")
        , ("images/icons/trace.png", "Trace")
        ]

-- | The icon size for these icons
stdIconSize :: Int
stdIconSize = 48
