#!/bin/sh

asm=${test_build_directory}/stop_after_scheduling.${asmext}
obj=${test_build_directory}/stop_after_scheduling.${objext}
cmx=${test_build_directory}/stop_after_scheduling.cmx

# Check that cmx is generated but asm and obj are not
if [ -e "$asm" ]; then
    test_result=${TEST_FAIL};
else if [ -e "$obj" ]; then
         test_result=${TEST_FAIL};
     else if [ -e "$cmx" ]; then
              test_reslut=${TEST_PASS};
          else
              test_reslut=${TEST_FAIL};
          fi
     fi
fi
