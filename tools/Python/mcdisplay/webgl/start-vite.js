// start-vite.js
import { createServer } from "vite";

async function start() {
  try {
    const server = await createServer();
    await server.listen();
    server.printUrls();
  } catch (error) {
    console.error("Error starting Vite server:", error);
  }
}

start();
