{-# LANGUAGE OverloadedStrings #-}

-- | The stylesheet, shared by the editor page and the file browser.
module Web.Styles (styles) where

import Web.Hyperbole

styles :: View c ()
styles =
  style
    "\
    \.app { font-family: system-ui, -apple-system, \"Segoe UI\", sans-serif; color: #0b0b0b; max-width: 1280px; margin: 0 auto; padding: 16px; }\
    \.header { display: flex; flex-wrap: wrap; align-items: center; gap: 12px; margin-bottom: 16px; }\
    \.title { font-weight: 600; }\
    \.note, .status { color: #52514e; }\
    \.dirty { color: #d03b3b; }\
    \.columns { display: flex; gap: 24px; align-items: flex-start; }\
    \.column-main { flex: 3; min-width: 0; }\
    \.column-est { flex: 2; min-width: 0; }\
    \.panel-heading { font-weight: 600; }\
    \.vocab-line { margin: 2px 0; color: #52514e; display: flex; align-items: baseline; gap: 8px; }\
    \.vocab-toggle { font-weight: 600; color: #0b0b0b; padding-left: 0; }\
    \.vocab-summary { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }\
    \.vgrid { margin: 4px 0 14px; }\
    \.trow { display: grid; grid-template-columns: minmax(9rem, 2.2fr) 1fr 1.1fr 1.4fr 2rem; gap: 0 10px; align-items: start; border-bottom: 1px solid #e1e0d9; padding: 3px 0; }\
    \.vrow { display: grid; grid-template-columns: 1.4fr 0.9fr 2.4fr 2rem; gap: 0 10px; align-items: start; border-bottom: 1px solid #e1e0d9; padding: 3px 0; }\
    \.thead { border-bottom: 1px solid #c3c2b7; color: #898781; font-weight: 600; }\
    \.tcell { cursor: pointer; padding: 3px 4px; border-radius: 3px; min-height: 1.3em; }\
    \.tcell:hover { background: #f4f3ee; }\
    \.cell-empty { color: #c3c2b7; }\
    \.vcell-used { color: #52514e; padding: 3px 4px; }\
    \.task-name { font-weight: 500; }\
    \.task-name.depth-1 { padding-left: 16px; }\
    \.task-name.depth-2 { padding-left: 32px; }\
    \.task-name.depth-3 { padding-left: 48px; }\
    \.task-name.depth-4 { padding-left: 64px; }\
    \.task-description { color: #52514e; font-size: 0.9em; }\
    \.tactions { text-align: right; }\
    \.btn-del { color: #898781; padding: 0 6px; }\
    \.btn-del:hover { color: #d03b3b; }\
    \.row-issues { grid-column: 1 / -1; color: #d03b3b; font-size: 0.9em; padding: 0 4px 3px; }\
    \.btn-fix { padding: 0 8px; }\
    \.cell-input { width: 100%; box-sizing: border-box; padding: 3px 6px; border: 1px solid #c3c2b7; border-radius: 4px; font: inherit; }\
    \.quick-row .cell-input { border-style: dashed; }\
    \.trow-form button[type=submit], .vrow-form button[type=submit] { grid-column: -2; grid-row: 1; }\
    \.trow-form .fi-4 { grid-column: 1 / -2; grid-row: 2; margin-top: 4px; }\
    \.row-error { padding: 2px 4px; }\
    \.empty { color: #898781; padding: 8px 0; }\
    \.btn { background: #2a78d6; color: #fff; border: none; border-radius: 4px; padding: 6px 12px; cursor: pointer; }\
    \.btn-mini { padding: 3px 10px; }\
    \.btn-primary { background: #2a78d6; }\
    \.btn-link { background: none; color: #2a78d6; border: none; cursor: pointer; padding: 2px 6px; }\
    \.form-error { color: #d03b3b; margin: 4px 0; }\
    \.estimate { border-left: 1px solid #e1e0d9; padding-left: 24px; }\
    \.estimate-controls { display: flex; align-items: center; gap: 10px; margin-bottom: 8px; }\
    \.estimate-running { color: #52514e; }\
    \.estimate-stale { color: #d03b3b; margin-bottom: 8px; }\
    \.estimate-summary { margin-bottom: 4px; }\
    \.estimate-quantiles { display: flex; flex-wrap: wrap; gap: 12px; margin: 8px 0; color: #52514e; }\
    \.report.dim { opacity: 0.55; }\
    \.delta { font-variant-numeric: tabular-nums; }\
    \.delta-up { color: #d03b3b; }\
    \.delta-down { color: #2e7d32; }\
    \.hist { display: flex; flex-direction: column; gap: 2px; margin-top: 8px; }\
    \.hist-row { display: grid; grid-template-columns: 64px 1fr 48px; align-items: center; gap: 8px; }\
    \.hist-bound { color: #52514e; text-align: right; font-variant-numeric: tabular-nums; }\
    \.hist-bar-track { background: #cde2fb; border-radius: 4px; height: 18px; overflow: hidden; }\
    \.hist-bar-fill { background: #2a78d6; height: 100%; border-radius: 0 4px 4px 0; }\
    \.hist-pct { color: #52514e; font-variant-numeric: tabular-nums; }\
    \.filebar { display: flex; flex-wrap: wrap; align-items: center; gap: 6px; border-bottom: 1px solid #e1e0d9; padding-bottom: 8px; margin-bottom: 12px; }\
    \.filebar-label { color: #898781; margin-right: 2px; }\
    \.filebar-item { display: flex; align-items: baseline; gap: 4px; border: 1px solid #e1e0d9; border-radius: 6px; padding: 2px 4px 2px 8px; }\
    \.filebar-item.active { border-color: #2a78d6; background: #f2f7fd; }\
    \.filebar-link { color: #0b0b0b; text-decoration: none; }\
    \.filebar-item.active .filebar-link { font-weight: 600; }\
    \.filebar-mark { color: #d03b3b; }\
    \.btn-close { color: #898781; padding: 0 4px; }\
    \.btn-close:hover { color: #d03b3b; }\
    \.btn-close.armed { color: #d03b3b; font-weight: 600; }\
    \.projects { display: flex; flex-wrap: wrap; align-items: baseline; gap: 8px; }\
    \.projects-label { color: #898781; }\
    \.project-link { color: #2a78d6; text-decoration: none; }\
    \.project-link.active { color: #0b0b0b; font-weight: 600; }\
    \.filelist { margin: 8px 0 16px; max-width: 640px; }\
    \.filelist-row { display: flex; align-items: baseline; gap: 8px; border-bottom: 1px solid #e1e0d9; padding: 6px 2px; }\
    \.filelist-link { color: #2a78d6; text-decoration: none; font-weight: 500; }\
    \.filelist-projects { color: #52514e; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }\
    \.page-error { color: #d03b3b; margin: 8px 0; }\
    \@media (max-width: 800px) {\
    \  .app { padding: 12px; }\
    \  .columns { flex-direction: column; gap: 16px; }\
    \  .column-main, .column-est { width: 100%; flex: none; }\
    \  .thead { display: none; }\
    \  .trow, .vrow { display: block; border: 1px solid #e1e0d9; border-radius: 6px; padding: 6px 10px; margin: 8px 0; }\
    \  .tcell[data-label]::before, .vcell-used[data-label]::before { content: attr(data-label) \": \"; color: #898781; }\
    \  .task-name { font-weight: 600; }\
    \  .cell-input { margin: 3px 0; }\
    \  .btn { padding: 10px 16px; }\
    \  .btn-mini { padding: 6px 12px; }\
    \  .btn-link { padding: 8px 10px; }\
    \  .btn-close { padding: 6px 10px; }\
    \  .estimate { border-left: none; padding-left: 0; border-top: 1px solid #e1e0d9; padding-top: 16px; }\
    \}\
    \@media (prefers-color-scheme: dark) {\
    \  .app { background: #1a1a19; color: #ffffff; }\
    \  .note, .status, .vocab-line, .vocab-summary, .task-description, .vcell-used, .estimate-quantiles, .estimate-running, .hist-bound, .hist-pct, .filelist-projects { color: #c3c2b7; }\
    \  .vocab-toggle { color: #ffffff; }\
    \  .thead { color: #898781; border-bottom-color: #383835; }\
    \  .trow, .vrow { border-color: #2c2c2a; }\
    \  .tcell:hover { background: #242422; }\
    \  .cell-empty { color: #52514e; }\
    \  .empty, .btn-del, .btn-close, .filebar-label, .projects-label { color: #898781; }\
    \  .btn, .btn-primary, .hist-bar-fill { background: #3987e5; }\
    \  .btn-link, .project-link, .filelist-link { color: #3987e5; }\
    \  .cell-input { background: #1a1a19; color: #fff; border-color: #383835; }\
    \  .delta-down { color: #66bb6a; }\
    \  .estimate { border-left-color: #2c2c2a; border-top-color: #2c2c2a; }\
    \  .hist-bar-track { background: #184f95; }\
    \  .filebar, .filelist-row { border-color: #2c2c2a; }\
    \  .filebar-item { border-color: #2c2c2a; }\
    \  .filebar-item.active { border-color: #3987e5; background: #17273a; }\
    \  .filebar-link, .project-link.active { color: #ffffff; }\
    \}"
