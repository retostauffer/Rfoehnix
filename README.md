---
title: "R package phoeton"
geometry: margin=3cm
---


Statistical Models
==================

Binary Logistic Regression Model (IWLS)
---------------------------------------


$\beta^{(j+1)} = ((X W^\frac{1}{2})^top (X W^\frac{1}{2}))^{-1} (X W^\frac{1}{2})^\top W^\frac{1}{2} * z^{(j)}$

With:

$z^{(j)} = \eta^{(j)} + (y - \mu^{(j)}) \Big(\frac{d \eta}{d \mu}\Big)^{(j)} = \eta^{(j)} + \frac{y - \mu^{(j)}}{\mu^{(j)} - \mu^{2(j)}}$

with $W^{\frac{1}{2}(j)} = \mu^{(j)} - \mu^{2(j)}$ this can be written as:

$\beta^{(j+1)} = ((X W^\frac{1}{2})^\top (X W^\frac{1}{2}))^{-1} (X W^\frac{1}{2})^\top \big(\eta^{(j)} W^\frac{1}{2} + (y - \mu^{(j)}) W^{-\frac{1}{2}}\big)$
