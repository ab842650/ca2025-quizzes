## 🧩 Project Overview

This project explores several topics in **low-level data representation** and **performance optimization** using RISC-V assembly and C.

It includes:
- Implementation of **bfloat16 (BF16)** arithmetic  
- Encoding and decoding of **Unsigned Floating-8 (UF8)** format  
- Optimization of **LeetCode 2571** using a **branchless CLZ** (Count Leading Zeros) approach

The branchless CLZ method successfully reduced control hazards in the pipeline, improving overall efficiency, even though the instruction count increased.

For detailed explanations, performance analysis, and source code, visit the full write-up here:  
👉 [HackMD: ARCH2025 – Homework 1](https://hackmd.io/@ab842650/arch2025-homework1)
