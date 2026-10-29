"""Unit B - The IRLS algorithm for logistic regression, Beetles data (Bliss, 1935).

Top panel: proportions y_i and the fitted curve pi(x) = logit^{-1}(beta_1 + beta_2 x).
Bottom panel: the same fit on the linear predictor scale, pseudo-responses z_i
(dot area proportional to the weight w_i) and the weighted least squares line.
First run: naive start beta^(1) = (0, 0). Second run: least squares on the
empirical logits, as in the slides.
"""

import numpy as np
from manim import *

MAIN = BLUE         # data, pseudo-responses
HIGHLIGHT = YELLOW  # residuals, the current step

# ---- Beetles data -----------------------------------------------------------
M = np.array([59, 60, 62, 56, 63, 59, 62, 60], dtype=float)
S = np.array([6, 13, 18, 28, 52, 53, 61, 60], dtype=float)
XV = np.array([1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839])
Y = S / M
X = np.column_stack([np.ones_like(XV), XV])
N = len(Y)


def expit(e):
    return 1.0 / (1.0 + np.exp(-e))


def loglik(b):
    p = expit(X @ b)
    return float(np.sum(S * np.log(p) + (M - S) * np.log(1 - p)))


def quantities(b):
    eta = X @ b
    p = expit(eta)
    w = M * p * (1 - p)
    z = eta + (Y - p) / (p * (1 - p))
    return eta, p, w, z


def wls(w, z):
    return np.linalg.solve(X.T @ (w[:, None] * X), X.T @ (w * z))


def irls_path(b, tol=1e-3, max_iter=20):
    path = [np.array(b, dtype=float)]
    for _ in range(max_iter):
        _, _, w, z = quantities(path[-1])
        path.append(wls(w, z))
        if np.linalg.norm(path[-1] - path[-2]) < tol:
            break
    return path


EMP_LOGIT = np.log((S + 0.5) / (M - S + 0.5))
B_EMP = np.linalg.lstsq(X, EMP_LOGIT, rcond=None)[0]
PATH_ZERO = irls_path(np.zeros(2))
PATH_EMP = [np.zeros(2)] + irls_path(B_EMP)   # placeholder entry: beta^(1) comes from OLS

W_REF = 15.5  # weight giving the reference radius (m_i / 4 at pi = 0.5)


def radius(w):
    return max(0.11 * np.sqrt(w / W_REF), 0.035)


def sci(v):
    """LaTeX for a small positive number."""
    if v >= 0.01:
        return f"{v:.2f}"
    e = int(np.floor(np.log10(v)))
    return rf"{v / 10 ** e:.1f} \times 10^{{{e}}}"


# ---- Layout -----------------------------------------------------------------
X_RANGE = [1.68, 1.90, 0.02]
X_NUMS = [1.70, 1.74, 1.78, 1.82, 1.86, 1.90]
PANEL_LEFT = -0.35
X_LEN = 6.9
COL_CORNER = np.array([-6.75, 3.55, 0])


class IRLS(Scene):
    def construct(self):
        # ---- axes -------------------------------------------------------------
        top = Axes(
            x_range=X_RANGE, y_range=[0, 1, 0.25], x_length=X_LEN, y_length=2.4,
            axis_config={"include_tip": False, "font_size": 24},
            y_axis_config={"numbers_to_include": [0, 0.25, 0.5, 0.75, 1],
                           "decimal_number_config": {"num_decimal_places": 2}},
        )
        top.shift(np.array([PANEL_LEFT, 0.75, 0]) - top.c2p(X_RANGE[0], 0))
        top_lab = Tex("proportion of deaths", font_size=26).next_to(top.y_axis, UP, buff=0.15)
        top_lab.align_to(top.y_axis, LEFT).shift(LEFT * 0.3)

        bot = Axes(
            x_range=X_RANGE, y_range=[-4, 6, 2], x_length=X_LEN, y_length=3.0,
            axis_config={"include_tip": False, "font_size": 24},
            x_axis_config={"numbers_to_include": X_NUMS,
                           "decimal_number_config": {"num_decimal_places": 2}},
            y_axis_config={"numbers_to_include": [-4, -2, 0, 2, 4, 6]},
        )
        bot.shift(np.array([PANEL_LEFT, -3.25, 0]) - bot.c2p(X_RANGE[0], -4))
        # x axis at the bottom of the panel, not at eta = 0
        bot.x_axis.shift(bot.c2p(X_RANGE[0], -4) - bot.c2p(X_RANGE[0], 0))
        zero_line = DashedLine(bot.c2p(X_RANGE[0], 0), bot.c2p(X_RANGE[1], 0),
                               stroke_width=1.5, color=GREY_D, dash_length=0.08)
        zero_lab = MathTex("0", font_size=24)
        zero_lab.next_to(bot.c2p(X_RANGE[0], 0), LEFT, buff=0.2)
        zero_lab.align_to(bot.y_axis.numbers[0], RIGHT)
        bot.add(zero_lab)
        bot_lab = Tex("linear predictor scale", font_size=26).next_to(bot.y_axis, UP, buff=0.15)
        bot_lab.align_to(top_lab, LEFT)
        x_lab = Tex("log-dose", font_size=26).next_to(bot.x_axis, DOWN, buff=0.45)
        x_lab.align_to(bot.x_axis, RIGHT)

        model = MathTex(r"\mathrm{logit}(\pi_i) = \beta_1 + \beta_2 x_i", font_size=34)
        model.move_to([0, 3.6, 0]).align_to(top.c2p(X_RANGE[1], 0), RIGHT)

        # ---- data ---------------------------------------------------------------
        ydots = VGroup(*[Dot(top.c2p(x, y), radius=0.075, color=MAIN).set_z_index(3)
                         for x, y in zip(XV, Y)])

        self.play(Create(top), FadeIn(top_lab))
        self.play(LaggedStart(*[GrowFromCenter(d) for d in ydots], lag_ratio=0.12), run_time=1.5)
        self.play(Write(model))
        self.wait(2)

        # ---- current beta and the two fitted curves ------------------------------
        b1 = ValueTracker(0.0)
        b2 = ValueTracker(0.0)

        def beta():
            return np.array([b1.get_value(), b2.get_value()])

        curve = always_redraw(lambda: top.plot(
            lambda x: expit(b1.get_value() + b2.get_value() * x),
            x_range=[X_RANGE[0], X_RANGE[1], 0.002], color=WHITE, stroke_width=3,
        ))
        line = always_redraw(lambda: bot.plot(
            lambda x: b1.get_value() + b2.get_value() * x,
            x_range=[X_RANGE[0], X_RANGE[1]], color=WHITE, stroke_width=3,
        ))

        # ---- left column: algorithm and status ----------------------------------
        fs = 32
        f_pi = MathTex(r"\pi_i^{(t)} = \mathrm{logit}^{-1}(\mathbf{x}_i^\top\beta^{(t)})", font_size=fs)
        f_w = MathTex(r"w_i^{(t)} = m_i\, \pi_i^{(t)}(1 - \pi_i^{(t)})", font_size=fs)
        f_z = MathTex(r"z_i^{(t)} = \mathbf{x}_i^\top\beta^{(t)} + "
                      r"\frac{y_i - \pi_i^{(t)}}{\pi_i^{(t)}(1 - \pi_i^{(t)})}", font_size=fs)
        f_b = MathTex(r"\beta^{(t+1)} = (\mathbf{X}^\top\mathbf{W}^{(t)}\mathbf{X})^{-1}"
                      r"\mathbf{X}^\top\mathbf{W}^{(t)}\mathbf{z}^{(t)}", font_size=fs)
        algo = VGroup(f_pi, f_w, f_z, f_b).arrange(DOWN, buff=0.38, aligned_edge=LEFT)
        algo.shift(COL_CORNER - np.array([algo.get_left()[0], algo.get_top()[1], 0]))
        if algo.get_right()[0] > -1.15:
            algo.scale_to_fit_width(-1.15 - COL_CORNER[0], about_edge=UL)

        sfs = 32
        t_num = Integer(1, font_size=sfs)
        t_row = VGroup(MathTex("t =", font_size=sfs), t_num).arrange(RIGHT, buff=0.15)

        b1_num = DecimalNumber(0, num_decimal_places=2, font_size=sfs)
        b2_num = DecimalNumber(0, num_decimal_places=2, font_size=sfs)
        b_open = MathTex(r"\beta^{(t)} = \big(", font_size=sfs)
        b_comma = MathTex(",", font_size=sfs)
        b_close = MathTex(r"\big)", font_size=sfs)
        b_row = VGroup(b_open, b1_num, b_comma, b2_num, b_close)

        ll_num = DecimalNumber(loglik(np.zeros(2)), num_decimal_places=2, font_size=sfs)
        ll_row = VGroup(MathTex(r"\ell(\beta^{(t)}) =", font_size=sfs), ll_num)

        d_lab = MathTex(r"\|\beta^{(t)} - \beta^{(t-1)}\| =", font_size=sfs)
        d_val = MathTex(r"\text{--}", font_size=sfs)
        d_row = VGroup(d_lab, d_val)

        status = VGroup(t_row, b_row, ll_row, d_row)

        def layout_status():
            b1_num.next_to(b_open, RIGHT, buff=0.1)
            b_comma.next_to(b1_num, RIGHT, buff=0.06).align_to(b1_num, DOWN).shift(DOWN * 0.1)
            b2_num.next_to(b_comma, RIGHT, buff=0.18).align_to(b1_num, DOWN)
            b_close.next_to(b2_num, RIGHT, buff=0.1)
            ll_num.next_to(ll_row[0], RIGHT, buff=0.15)
            d_val.next_to(d_lab, RIGHT, buff=0.15)

        layout_status()
        y0 = algo.get_bottom()[1] - 0.75
        for k, row in enumerate(status):
            row.shift(np.array([COL_CORNER[0] - row.get_left()[0], y0 - 0.62 * k - row.get_center()[1], 0]))
        layout_status()

        def status_updater(_):
            b1_num.set_value(b1.get_value())
            b2_num.set_value(b2.get_value())
            ll_num.set_value(loglik(beta()))
            layout_status()

        def set_delta(v):
            new = MathTex(sci(v), font_size=sfs).next_to(d_lab, RIGHT, buff=0.15)
            return Transform(d_val, new)

        # ---- the naive start ------------------------------------------------------
        self.play(FadeIn(t_row), FadeIn(b_row), FadeIn(ll_row), FadeIn(d_row))
        self.play(Create(curve), run_time=1.5)
        self.wait(1)
        self.play(Create(bot), FadeIn(bot_lab), FadeIn(x_lab), Create(zero_line))
        self.play(Create(line), run_time=1.2)
        self.wait(2)
        b_row.add_updater(status_updater)

        zdots = VGroup()
        state = {"first": True}

        def iteration(t, path, pace, write=False):
            """One IRLS step from path[t-1] to path[t]; pace scales the run times."""
            b_old, b_new = path[t - 1], path[t]
            eta, p, w, z = quantities(b_old)

            def rt(x):
                return max(x * pace, 0.25)

            # 1. fitted probabilities and the residuals y_i - pi_i
            res_top = VGroup(*[
                Line(top.c2p(XV[i], p[i]), top.c2p(XV[i], Y[i]), color=HIGHLIGHT, stroke_width=3)
                for i in range(N)
            ])
            pdots = VGroup(*[Dot(top.c2p(XV[i], p[i]), radius=0.045, color=WHITE).set_z_index(2)
                             for i in range(N)])
            if write:
                self.play(Write(f_pi), run_time=1.5)
            self.play(f_pi.animate.set_color(HIGHLIGHT), Create(res_top), FadeIn(pdots),
                      run_time=rt(1.5))
            self.wait(rt(1.5))

            # 2. weights: dots on the line, area proportional to w_i
            new_dots = VGroup(*[Dot(bot.c2p(XV[i], eta[i]), radius=radius(w[i]), color=MAIN,
                                    fill_opacity=0.85).set_z_index(3) for i in range(N)])
            if write:
                self.play(f_pi.animate.set_color(WHITE), Write(f_w), run_time=1.5)
            if len(zdots) == 0:
                grow = LaggedStart(*[GrowFromCenter(d) for d in new_dots], lag_ratio=0.08)
                self.play(f_pi.animate.set_color(WHITE), f_w.animate.set_color(HIGHLIGHT), grow,
                          run_time=rt(2.0))
                zdots.add(*new_dots)
                self.remove(*new_dots)
                self.add(zdots)
            else:
                # previous pseudo-responses slide back onto the line, then resize
                self.play(f_pi.animate.set_color(WHITE), f_w.animate.set_color(HIGHLIGHT),
                          *[zdots[i].animate.become(new_dots[i]) for i in range(N)],
                          run_time=rt(2.0))
            self.wait(rt(1.5))

            # 3. pseudo-responses: the residual, stretched by g'(pi_i) = 1 / (pi_i (1 - pi_i))
            res_bot = VGroup(*[
                Line(bot.c2p(XV[i], eta[i]), bot.c2p(XV[i], z[i]), color=HIGHLIGHT, stroke_width=3)
                for i in range(N)
            ])
            if write:
                self.play(f_w.animate.set_color(WHITE), Write(f_z), run_time=2)
            self.play(f_w.animate.set_color(WHITE), f_z.animate.set_color(HIGHLIGHT),
                      TransformFromCopy(res_top, res_bot),
                      *[zdots[i].animate.move_to(bot.c2p(XV[i], z[i])) for i in range(N)],
                      run_time=rt(2.2))
            self.wait(rt(2.0))

            # 4. weighted least squares on (x_i, z_i): new line, new curve
            if write:
                self.play(f_z.animate.set_color(WHITE), Write(f_b), run_time=2)
            self.play(f_z.animate.set_color(WHITE), f_b.animate.set_color(HIGHLIGHT),
                      FadeOut(res_bot), FadeOut(res_top), FadeOut(pdots), run_time=rt(0.8))
            self.play(b1.animate.set_value(b_new[0]), b2.animate.set_value(b_new[1]),
                      run_time=rt(2.2))
            self.play(f_b.animate.set_color(WHITE), t_num.animate.set_value(t + 1),
                      set_delta(np.linalg.norm(b_new - b_old)), run_time=rt(0.8))
            self.wait(rt(1.5))

        # first two iterations slowly, then faster
        iteration(1, PATH_ZERO, 1.0, write=True)
        iteration(2, PATH_ZERO, 0.8)
        for t in range(3, len(PATH_ZERO)):
            iteration(t, PATH_ZERO, 0.35)
        self.wait(1)
        done = SurroundingRectangle(status, color=HIGHLIGHT, buff=0.15)
        self.play(Create(done))
        self.wait(4)

        # ---- second run: least squares on the empirical logits ---------------------
        b_row.clear_updaters()
        stat_vals = VGroup(t_row, b_row, ll_row, d_row)
        self.play(FadeOut(done), FadeOut(zdots), FadeOut(line), FadeOut(curve),
                  FadeOut(algo[:3]), FadeOut(stat_vals))
        zdots.remove(*zdots)

        init_z = MathTex(r"z_i^{(1)} = \mathrm{logit}(\tilde{y}_i) = "
                         r"\log\frac{S_i + 1/2}{m_i - S_i + 1/2}", font_size=fs)
        init_w = MathTex(r"\mathbf{W}^{(1)} = I_n", font_size=fs)
        init = VGroup(init_z, init_w).arrange(DOWN, buff=0.45, aligned_edge=LEFT)
        init.move_to(algo[:3]).align_to(algo, LEFT)
        self.play(Write(init_z), run_time=2)
        self.wait(1)

        emp_dots = VGroup(*[Dot(bot.c2p(XV[i], EMP_LOGIT[i]), radius=radius(W_REF), color=MAIN,
                                fill_opacity=0.85).set_z_index(3) for i in range(N)])
        self.play(LaggedStart(*[TransformFromCopy(ydots[i], emp_dots[i]) for i in range(N)],
                              lag_ratio=0.1), run_time=2.5)
        zdots.add(*emp_dots)
        self.remove(*emp_dots)
        self.add(zdots)
        self.wait(1)
        self.play(Write(init_w))
        self.wait(2)

        # ordinary least squares on the empirical logits
        b1.set_value(B_EMP[0])
        b2.set_value(B_EMP[1])
        t_num.set_value(1)
        d_val.become(MathTex(r"\text{--}", font_size=sfs).next_to(d_lab, RIGHT, buff=0.15))
        status_updater(None)
        self.play(f_b.animate.set_color(HIGHLIGHT), Create(line), run_time=1.5)
        self.play(Create(curve), FadeIn(stat_vals), run_time=1.5)
        b_row.add_updater(status_updater)
        self.play(f_b.animate.set_color(WHITE))
        self.wait(3)

        self.play(FadeOut(init), FadeIn(algo[:3]))
        self.wait(1)
        for t in range(2, len(PATH_EMP)):
            iteration(t, PATH_EMP, 0.5 if t == 2 else 0.35)
        b_row.clear_updaters()
        self.wait(1)
        done = SurroundingRectangle(status, color=HIGHLIGHT, buff=0.15)
        self.play(Create(done))
        self.wait(8)
