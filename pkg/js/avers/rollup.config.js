import commonjs from "rollup-plugin-commonjs";
import nodeResolve from "rollup-plugin-node-resolve";
import babel from "rollup-plugin-babel";

export default {
  input: "dist/index.js",
  output: [
    {
      file: "dist/index.node.js",
      format: "cjs"
    }
  ],
  plugins: [
    nodeResolve({ jsnext: true, main: true }),
    babel({ presets: [["@babel/preset-env", { targets: { node: "10" } }]] }),
    commonjs({ include: ["node_modules/**", "dist/**"] })
  ]
};
