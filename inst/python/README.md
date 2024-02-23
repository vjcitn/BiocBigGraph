It might be necessary to use a slightly modified version of torchbiggraph
to avoid an error in operators.py present at
commit 0e7c1010b5b5bb9f20c275dc12477064196536e1

If necessary, use github.com/vjcitn/PyTorch-BigGraph; 23 feb 2024 we are trying
a fresh checkout from facebookresearch github.  Note that the version in
pip is called 1.dev1 and certain modules were not accessible with that
version, so we use the source for cpu operations.

    line 335 had reference to operator_idxs that caused error
```
      File "/home/exouser/miniforge3/lib/python3.10/site-packages/torchbiggraph/re
gularizers.py", line 73, in forward_dynamic
        operator_params = operator.get_operator_params_for_reg(rel_idxs)
      File "/home/exouser/miniforge3/lib/python3.10/site-packages/torchbiggraph/op
erators.py", line 335, in get_operator_params_for_reg
        return torch.sqrt(self.real[operator_idxs] ** 2 + self.imag[operator_idxs]
 ** 2)
    RuntimeError: indices should be either on cpu or on the same device as the ind
exed tensor (cpu)
    Traceback (most recent call last):
      File "/home/exouser/CLEAN/PyTorch-BigGraph/torchbiggraph/examples/fb15k.py",
 line 108, in <module>
```
    Clearly 0 is not an ideal choice but for a single GPU seems OK

The sources are otherwise identical to those at
commit 0e7c1010b5b5bb9f20c275dc12477064196536e1

