del *.js
del *.map
tsc -sourcemap test.ts --target ES5 --experimentalDecorators
tsc -sourcemap ordinal.ts --target ES5 --experimentalDecorators
tsc -sourcemap language.ts --target ES5 --experimentalDecorators
tsc -sourcemap plural.ts --target ES5 --experimentalDecorators
tsc -sourcemap pluralsprintf.ts --target ES5 --experimentalDecorators
tsc -sourcemap plurali18next.ts --target ES5 --experimentalDecorators