# myphoto

This is a collection of scripts, tools and workflows that I use for developping images. In large parts it focuses on stacking of extreme macro images.

## myphoto-stack
This implements a pipeline to filter, align and stack images with [PetteriAimonen/focus-stack](https://github.com/PetteriAimonen/focus-stack) and [Enblend Enfuse](https://enblend.sourceforge.net/).
```mermaid
  graph TD;
      A1["Arguments:\nlist of images"];
      A2["Arguments:\ndirectory"];
      A3["Arguments:\n--dirs and list of directories"];
      A1 --> A("list of images\n either .jpg, .png, raw or .tiff");
      A2 --> A;
      A3 --> A;
      A --> B["take only every nth image\nif --every-nth=?"];
      B --> C["sort images based on exif CreateDate\nif --sort-on-create-date"];
      C --> D["drop images that happened after a time break\nif --breaking"];
      D --> E["remove images that are outliers\nif --remove-outliers"];
      E --> F["convert raw files to .tiff with 16bit"];
      F --> G["convert .tiff to .png with 16bit\nif --untiff"];
      G --> H1["run PetteriAimonen/focus-stack"];
      G -- "if --no-focus-stack" --> H2["run Hugin Align"]
      H1 --> Z1("Images stacked with focus-stack")
      H1 --> I("list of aligned images")
      H2 --> I
      I --> J["run enblend enfuse\nif --enfuse"]
      J --> Z2("Images stacked with enfuse")

```

## myphoto-watch
An application to watch a folder (e.g. a FTP folder, to which the camera pushes its images) and automatically run the stacking logic from above on completed stacks.