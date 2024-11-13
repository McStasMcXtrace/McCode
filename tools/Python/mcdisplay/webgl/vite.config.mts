import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { viteStaticCopy } from "vite-plugin-static-copy";

export default defineConfig({
  plugins: [
    react(),
    viteStaticCopy({
      targets: [
        {
          src: "path/to/particles.json",
          dest: "",
        },
        {
          src: "path/to/instrument.json",
          dest: "",
        },
      ],
    }),
  ],
  server: {
    hmr: {
      overlay: false,
    },
  },
});
