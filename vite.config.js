import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
  build: {
    rollupOptions: {
      output: {
        manualChunks(id) {
          if (id.includes("ndpc.")) {
            return "ndpc";
          } else if (id.includes("parsley")) {
            return "parsley";
          } else if (id.includes("scalajs")) {
            return "scalajs";
          } else if (id.includes("scala.")) {
            return "std";
          } else if (id.includes("node_modules")) {
            return "vendor";
          }
          return "other";
        }
      },
    },
  },
  plugins: [scalaJSPlugin()],
  base: "./",
});
