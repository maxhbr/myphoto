# myphoto-gallery

This is a command that helps to collect images for my gallery from workdirs by finding the `*.myphoto.toml` sidecar files next to images.

## The cli

### Step 1: Adding Metadata to files:

Calling `myphoto-gallery [--tag $TAG] [--about PATH/TO/ABOUT/IMAGE] ./IMAGE.png` does:

First creates a file `./IMAGE.png.myphoto.toml`

```toml
img = "$(filename $IMAGE)"
modified = "YYYY-MM-DD" 
tags = ["$TAG", "$TAG"]
amout = ["RELATIV_PATH/TO/ABOUT/IMAGE"]
```

where multiple tags and multiple about images could be passed. 

The haskell datamodel is
```hs
data PhotoMeta = PhotoMeta
  { img :: Maybe FilePath -- optional override of image filename
  , tags :: Set.Set String -- list of tags
  , path :: Maybe FilePath -- optional path within gallery
  , about :: [FilePath] -- list of about images
  , modified :: String -- YYYY-MM-DD
  }
  deriving (Show, Eq)
```


Also initializes a `./myphoto.toml` for directory based imports next to the file, if not already present. IT has the same base model but `img` should not set.


#### Modification by adding again

Calling the above command again adds the passed tags and about images to the existing file.

### Step 2: Import

Being in the root of the output gallery directory a call to `myphoto-gallery import PATH/TO/WORKDIR` does the following:

It finds all the `.myphoto.toml` sidecar files, and imports them to the gallery.

For each image it computes the final metadata by finding the directory `myphoto.toml` files next to it and in the parent directories and merging them (from furthest away to the closest) and last apply the sidecar one.

- The basename of the output image is either the value of `img` if set or the filename of the input image.
- The directory within the gallery in which the image should be placed is either the value of `path` if set or the name of the first directory within the WORKDIR. So `PATH/TO/WORKDIR/2025-11_-_Bla_Bli/0_raw/IMAGE.jpg` will be `2025-11_-_Bla_Bli`.

When imported, it will also create a new sidecare file `./IMAGE.png.myphoto.imported.toml`

```hs
data ImportedMeta = 
  ImportedMeta
  { original :: PhotoMeta
  , overwrite :: PhotoMeta
  , imported :: String -- YYYY-MM-DD
  , md5 :: String
  }
  deriving (Show, Eq)
```

where `original` is the computed `PhotoMeta`. This only gets written and overwritten if the input image has changed.

#### Step 2.b: Compute Metadata

Find all `.imported.toml` and parse them.

### Gallery Configuration

The gallery can be configured using a `myphoto.gallery.toml` file in the gallery root directory. Initialize it with `myphoto-gallery init`.

The configuration model:
```hs
data GalleryConfig = GalleryConfig
  { ignoredTags :: Set.Set String -- tags to filter out from gallery
  , ignoredImgs :: Set.Set FilePath -- image paths to exclude from gallery
  , remappedTags :: Map.Map String String -- tag remapping (old -> new)
  , remappedPaths :: Map.Map FilePath FilePath -- path remapping (old -> new)
  }
  deriving (Show, Eq)
```

Example `myphoto.gallery.toml`:
```toml
ignoredTags = ["Draft", "Private", "Test"]
ignoredImgs = ["2024-01_-_Project/debug.png", "2024-02_-_Trip/outtake.jpg"]

[remappedTags]
"Macro" = "Close-up"
"BW" = "Black & White"
"Pano" = "Panorama"

[remappedPaths]
"2024-01_-_Old_Name" = "2024-01_-_New_Name"
"temp" = "2024-12_-_Misc"
```

The configuration is applied during the update step, filtering out ignored items and applying remappings before generating the gallery output.
