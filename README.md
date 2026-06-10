# OO — Smart Offset for AutoCAD

> Everything AutoCAD's OFFSET should have been.

**Free AutoLISP tool. Load once. Type `OO`. Never go back.**

---

## Features

| Syntax | Mode | What it does |
|--------|------|-------------|
| `100mm` | Single | Offset with any unit |
| `5@100mm` | Array | 5 offsets at 100mm spacing — one command |
| `4#400mm` | Divide | Divide 400mm into 4 equal parts |
| `100g50mm` | Gap | Wall + 50mm cavity + inner wall |
| `(100+50)*2` | Math | Any arithmetic directly |
| `2"`, `1.5'`, `2-1/2"` | Units | mm cm m inches feet fractions |

## Hotkeys

Press before entering distance:

| Key | Action |
|-----|--------|
| `S` | Snap — pick 2 points, OO measures distance |
| `R` | Reference — offset from a picked point |
| `T` | Through — result passes through a point |
| `B` | Between — land exactly between 2 points |
| `Q` | Both sides simultaneously |
| `E` | Erase original after offset |
| `M` | Multiple — keep offsetting without restart |
| `L` | Place result on current active layer |

## Install

```
1. Open AutoCAD
2. Type: APPLOAD
3. Browse to OO.lsp → Load
4. Type: OO
```

Done. 30 seconds.

## Commands

| Command | Action |
|---------|--------|
| `OO` | Run smart offset |
| `OOH` | Show help screen inside AutoCAD |
| `OOX` | Cleanup temporary objects + REGEN |

## Works on

Lines · Arcs · Polylines · Circles · Any AutoCAD version with AutoLISP

---

**Created by Ar. Kishan S. Lakhani**  
Architectural Designer & BIM Coordinator  
© 2025 — Free & Open Source  
Do not redistribute without permission.
