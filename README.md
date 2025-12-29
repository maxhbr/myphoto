# myphoto

This is a collection of scripts, tools and workflows that I use for developping images. In large parts it focuses on stacking of extreme macro images.

## myphoto-stack
This implements a pipeline to filter, align and stack images with [PetteriAimonen/focus-stack](https://github.com/PetteriAimonen/focus-stack) and [Enblend Enfuse](https://enblend.sourceforge.net/).
```mermaid
  graph TD;
      A1("Arguments:\nlist of images");
      A2("Arguments:\ndirectory");
      A3("Arguments:\n--dirs and list of directories");
      A1 --> A[/"images\n either .jpg, .png, raw or .tiff"/];
      A2 -- "all files in directory" --> A;
      A3 --> A;
      A --> B["sort images based on exif CreateDate\nif --sort-on-create-date\n(default)"];
      B --> C["take only every nth image\nif --every-nth=?"];
      C --> D["drop images that happened after a time break\nif --breaking"];
      D --> E["convert raw files to .tiff with 16bit"];
      E --> F["convert .tiff to .png with 16bit\nif --untiff"];
      F --> G["remove images that are outliers\nif --remove-outliers"];
      G --> G'{"if --focus-stack"}
      G' -- "(default)" --> H1["run\nPetteriAimonen/focus-stack"];
      G' -- "if --no-focus-stack" --> H2["run\nHugin Align"]
      H1 --> Z1[/"Image stacked with focus-stack"/]
      H1 -- "always 8bit" --> I[/"aligned images"/]
      H2 -- "maybe 16bit" --> I
      I --> J["run enblend enfuse (if --enfuse)"]
      J --> Z2[/"Image stacked with enfuse"/]
      I --> K[/"Image stacked with zerene-stacker (if --zerene-stacker)"/]
      K --> Z3[/"Image stacked with zerene-stacker"/]
      G --> ZZ[/"Montage of subset of images"/]

      Z1 -----> ZA[/"Aligned output Images"/]
      Z2 -----> ZA
      Z3 -----> ZA

```

## myphoto-watch
An application to watch a folder (e.g. a FTP folder, to which the camera pushes its images) and automatically run the stacking logic from above on completed stacks.