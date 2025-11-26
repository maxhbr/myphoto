#!/usr/bin/env nix-shell
#!nix-shell -i bash -p imagemagick


Variance_of_Laplacian() {
    # (classic blur detector)
    time magick "$1" -colorspace Gray \
        -define convolve:scale='!' \
        -morphology Convolve Laplacian \
        -format "%[fx:standard_deviation^2]\n" info:
}


Tenengrad() {
    # (Sobel gradient energy)
    time magick "$1" -colorspace Gray \
        \( -clone 0 -morphology Convolve Sobel:0  -evaluate Pow 2 \) \
        \( -clone 0 -morphology Convolve Sobel:90 -evaluate Pow 2 \) \
        -delete 0 -evaluate-sequence Add \
        -format "%[fx:mean]\n" info:
}


FFT_high-frequency_energy() {
    # (more robust to lighting)

    # radius=0.3 keeps the outer 70% of the spectrum; tweak as desired.
    time magick "$1" -colorspace Gray -resize 256x256! \
        -fft -write mpr:mag +delete \
        \( mpr:mag -evaluate Pow 2 \) \
        \( -size 256x256 radial-gradient: \
            -threshold 65% -negate \) \
        -compose multiply -composite \
        -format "%[fx:mean]\n" info:
}

compute_row() {
    FILE="$1"
    VAR_LAP=$(Variance_of_Laplacian "$FILE")
    TENENGRAD=$(Tenengrad "$FILE")
    FFT_ENERGY=$(FFT_high-frequency_energy "$FILE")
    echo -e "${FILE}\t${VAR_LAP}\t${TENENGRAD}\t${FFT_ENERGY}"
}

echo -e "Filename\tVariance_of_Laplacian\tTenengrad\tFFT_high-frequency_energy" > blur_detection_results.tsv
for FILE in "$@"; do
    compute_row "$FILE" | tee -a blur_detection_results.tsv
done
times