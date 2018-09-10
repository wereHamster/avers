import commonjs from "rollup-plugin-commonjs";
import nodeResolve from "rollup-plugin-node-resolve";

export default {
  input: "dist/index.js",
  output: {
    file: "avers.js",
    format: "umd",
    name: "Avers"
  },
  plugins: [
    nodeResolve({
      jsnext: true,
      main: true
    }),

    commonjs({
      include: ["node_modules/**", "dist/**"]
    })
  ]
};
