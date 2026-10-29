# Videos of the course (Manim)

Sources of the animations of Statistics III.

- `un_B_irls.mp4` (`v1_irls.py`): the IRLS algorithm for logistic regression on the
  `Beetles` data. Top panel: proportions and fitted curve. Bottom panel: pseudo-responses
  `z_i` (dot area proportional to the weight `w_i`) and the weighted least squares line.
  First run from `beta = (0, 0)`, second run from least squares on the empirical logits.

The data are hard-coded in the script; the IRLS iterations are computed in Python
(final estimate `(-60.717, 34.270)`, as in `glm`).

## Regenerate

Render with Manim Community (>= 0.19):

```
manim -r 1920,1080 --fps 30 v1_irls.py IRLS
```

Compress and copy to `video/`:

```
ffmpeg -i <rendered>.mp4 -c:v libx264 -preset veryfast -crf 23 \
  -pix_fmt yuv420p -movflags +faststart -an ../un_B_irls.mp4
```
