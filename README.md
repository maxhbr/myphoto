# myphoto

This is a collection of scripts, tools and workflows that I use for developping images. In large parts it focuses on stacking of extreme macro images.

## myphoto-stack
This implements a pipeline to filter, align and stack images with [PetteriAimonen/focus-stack](https://github.com/PetteriAimonen/focus-stack) and [Enblend Enfuse](https://enblend.sourceforge.net/).
```mermaid
  graph TD;
      subgraph input
        A1("Arguments:\nlist of images");
        A2("Arguments:\ndirectory");
        A3("Arguments:\n--dirs and list of directories");
        A[/"images\n either .jpg, .png, raw or .tiff"/];
      end
      subgraph preprocessing
        A1 --> A
        A2 -- "all files in directory" --> A;
        A3 --> A;
        A --> B["sort images based on exif CreateDate\nif --sort-on-create-date\n(default)"];
        B --> C["take only every nth image\nif --every-nth=?"];
        C --> D["drop images that happened after a time break\nif --breaking"];
        D --> E["convert raw files to .tiff with 16bit"];
        E --> F["convert .tiff to .png with 16bit\nif --untiff"];
        F --> G["remove images that are outliers\nif --remove-outliers"];
      end
      G --> G'{"if --focus-stack"}
      G' -- "(default)" --> H1["run\nPetteriAimonen/focus-stack"];
      G' -- "if --no-focus-stack" --> H2'{"if --enfuse"}
      H2' -- "(default)" --> H2["run\nHugin Align"]
      H1 --> Z1[/"Image stacked with focus-stack"/]
      H1 -- "always 8bit" --> I[/"aligned images"/]
      H2 -- "maybe 16bit" --> I
      I --> J["run enblend enfuse\n(if --enfuse)"]
      J --> Z2[/"Image stacked with enfuse"/]
      I --> K["run zerene-stacker\n(if --zerene-stacker)"]
      H2' -- "if --no-enfuse" --> K
      K --> Z3[/"Image stacked with zerene-stacker"/]
      G --> ZZ[/"Montage of subset of images"/]
      G -- "if --only-zerene-stacker" --> K

      Z1 -----> Z[/"Output Images"/]
      Z2 -----> Z
      Z3 -----> Z
      subgraph postprocessing
        Z --> ZA'[/"Image stacked with enfuse\nCrop to overlap"/]
        ZA' --> ZA[/"Aligned output Images"/]
        ZA' --> ZL[/"Tiff with all the layers"/]
      end

```

## myphoto-watch
An application to watch a folder (e.g. a FTP folder, to which the camera pushes its images) and automatically run the stacking logic from above on completed stacks.
